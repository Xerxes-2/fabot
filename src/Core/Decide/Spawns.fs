/// Reading a row off a living body, and the spawn plan that fills the gap
/// between the quota and the roster (ADR 0006): which pattern a creep was cast
/// from, what a row leads by, and what to cast next.
[<AutoOpen>]
module Fabot.Core.Decide.Spawns

open Fabot.Core
open Fabot.Core.Types

/// Whether a living body carries a CLAIM part — the reserver row's own cut
/// (ADR 0042), the one part no other row buys, and so the row's living census.
/// `patternOfParts`' reserver arm is this same test; it is written here as a
/// census over a list, where the cascade wants an arm. The cascade asks it
/// before the comparative tests for a reason this census does not have to care
/// about but must not contradict: `[Claim; Carry; Move]` has no Work beside a
/// Carry, so a cascade that reached the hauler arm first would read a reserver
/// as a hauler.
let private isReserverBody (creep: CreepInfo) = partCount creep.Body BodyPart.Claim > 0

/// Whether a living body carries Carry parts and no Work — the hauler row's
/// own cut (ADR 0012), and `patternOfParts`' hauler arm read as a census.
let private isHaulerBody (creep: CreepInfo) =
    partCount creep.Body Work = 0 && partCount creep.Body Carry > 0

/// Whether a body still in the oven is Work-heavy — the anchor row's `Work >
/// Move`, the ratio fatigue parity forbids a worker body (ADR 0006). The
/// reading a living creep gets from `Atlas.workHeavy`, which a body with no
/// name yet cannot be looked up in; it is the one input `patternOfParts` and
/// `canRefillParts` take rather than derive, and this is the cast's answer to
/// it.
let private castIsHeavy parts =
    partCount parts Work > partCount parts Move

/// The pattern row a living body was cast from (`patternOfParts` over its part
/// map), with the Atlas's `workHeavy` set answering the heavy question.
let private patternOf (tuning: Tuning) atlas (creep: CreepInfo) =
    patternOfParts tuning (Atlas.workHeavy atlas creep.Name) creep.Body

/// The row a body **still in the oven** was bought for: the same rule over the
/// same counts, which is the whole point of there being one — the six arms
/// used to be written out twice, in two representations, and only prose kept
/// them in the same order.
let private patternOfCast (tuning: Tuning) (body: BodyPart list) =
    let parts = partsOf body
    patternOfParts tuning (castIsHeavy parts) parts

/// Whether a living body can put energy into an extension (ADR 0050).
let private canRefill (tuning: Tuning) atlas (creep: CreepInfo) =
    canRefillParts tuning (Atlas.workHeavy atlas creep.Name) creep.Body

/// Whether a body in the oven will be able to put energy into an extension
/// once it stands — `canRefillParts` over a cast rather than over a living
/// creep, for the supply floor's one question (ADR 0050): is there anything,
/// alive or bought, that can break the deadlock?
let private castCanRefill (tuning: Tuning) (body: BodyPart list) =
    let parts = partsOf body
    canRefillParts tuning (castIsHeavy parts) parts

/// The largest ceiling the row's Posts ask for, and the held one where it has
/// no Post at all: the anchor row's answer wherever a reader wants a body but
/// names no Post (ADR 0053).
let private richestAnchorCap (caps: Map<RoomPos, int>) =
    caps
    |> Map.fold (fun richest _ cap -> max richest cap) 0
    |> function
        | 0 -> heldWorkCap
        | cap -> cap

/// The ceiling of the Post at a tile, and the richest one for a tile that
/// is no Post (ADR 0053): the two readings a body sized for a *place*
/// needs — the Post a garrison stands on, and the nothing-in-particular a
/// body still walking to one stands on.
let private anchorCapAt (caps: Map<RoomPos, int>) (tile: RoomPos) =
    match Map.tryFind tile caps with
    | Some cap -> cap
    | None -> richestAnchorCap caps

/// **The body a row casts to put a creep on one tile**, at this colony's bank
/// and under this tick's second fact where the row has one (ADR 0052 decision
/// 4): the anchor row under the ceiling of the [[post]] that tile is (ADR
/// 0053), the reserver row at its largest outstanding demand, and every other
/// row at `bodyFor`'s answer, which for them *is* the whole rule. A tile and
/// not a row alone, because since ADR 0053 the anchor row casts no single body:
/// its ceiling is the rock the Post seats, and the one caller here is the
/// [[lead]], which asks what will stand *where this creep stands*. So an
/// incumbent on the home room's Post is led by a six-Work successor while one
/// on an outpost's lapsed Post is led by a three-Work one, off one rule. A tile
/// that is no Post takes the richest ceiling the row has. The entry point every
/// reader that means "what will this colony buy" asks, where `bodyFor` answers
/// the narrower question a caller holding only a capacity can ask.
let private castBodyOf
    (view: ColonyView)
    (sizing: RowSizing)
    (pattern: BodyPattern)
    (tile: RoomPos)
    =
    sizedBodyFor
        {
            AnchorCap = anchorCapAt sizing.AnchorPostCaps tile
            ReserverClaims = sizing.ReserverClaims
        }
        pattern
        view.Bank.Capacity

/// A creep's lead (ADR 0026): the ticks its replacement needs to stand where it
/// stands — the successor body's cast time plus that body's walk out of the
/// spawn, priced for the successor's own fatigue factor and not the
/// incumbent's. The body is **the one this colony's row would cast to stand on
/// the incumbent's own tile** (`castBodyOf`, ADR 0053) and not the largest
/// that row could cast, so an Anchor on a lapsed outpost's [[post]]
/// earns a shorter lead than the Anchor beside it at home. The walk starts
/// beside the spawner rather than on it, where the engine actually places the
/// finished creep. Several spawns resolve at the cheapest. Geometry that prices
/// nothing leads nobody (ADR 0004), and a lead of 0 leaves every living creep
/// counted.
let private leadOf (view: ColonyView) atlas (sizing: RowSizing) (creep: CreepInfo) : int =
    let pattern = patternOf view.Tuning atlas creep

    match Atlas.creepTile atlas creep.Name with
    | None -> 0
    | Some tile ->
        // The colony's one bank, whatever room the spawn is filed under: every
        // spawn a colony casts from stands in its home room (ADR 0052 decision
        // 1), so the capacity a replacement would be cast at is the same for
        // all of them — and so is the tile it is cast to stand on (ADR 0053).
        let body = castBodyOf view sizing pattern tile

        view.Spawns
        |> List.choose (fun s ->
            match Atlas.positionOf atlas s.Id with
            | None -> None
            | Some spawnPos ->
                Atlas.castWalkTicks atlas body (RoomPos.pos spawnPos) tile
                |> Option.map (fun walk -> Engine.spawnTicksPerPart * List.length body + walk))
        |> function
            | [] -> 0
            | leads -> List.min leads

/// Whether a creep is expiring (ADR 0026): its remaining life is at or under
/// its lead, so it will be dead before a replacement cast now could stand where
/// it stands. It leaves the workforce's living count and its row's gap, which
/// is what casts the successor while it still works.
let internal expiring (view: ColonyView) atlas (sizing: RowSizing) (creep: CreepInfo) =
    creep.TicksToLive <= leadOf view atlas sizing creep

/// One specialist row of the spawn cascade, stated once: the name the `quotas`
/// view files it under (ADR 0009), the pattern it casts, how many bodies it
/// wants this tick, which living bodies answer to it, and how a seat of it is
/// sized against the bank. The three readings a row is put to — the gap it
/// casts for, the row it reports and the seats it takes — are each one pass
/// over the list of these, so the casting order, the reported order and the
/// generalist's subtraction are one order and cannot drift apart.
///
/// `Size` is a function of the bank and not a lookup off the pattern, for the
/// reason `castFromBank` states where it takes one.
type private SpecialistRow =
    {
        Name: string
        Pattern: BodyPattern
        Quota: int
        Census: CreepInfo -> bool
    }


/// The spawn Intents the Workforce target's rows are owed. The target is the
/// quota the *generalist* row is hired against; every other row is hired against
/// its own unfilled quota and can carry the fleet past the target. Spawning is a
/// colony-level need, not a Task creeps get matched to, so it sits beside the
/// Planner/Matcher pipeline rather than inside it. It reads the tick's Task pool
/// for one number: the worker row's floor is "two while anything stands in the
/// Build or Repair pool" (ADR 0046), and the pool is the only honest reading of
/// that. The step is derived from the pool and never feeds it.
let internal planSpawns
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (threats: Threats)
    (tasks: Task list)
    (haulerQuota: int)
    : Intent list * Quotas =
    // The spawn holds while its doorstep is hot (ADR 0033): a creep born into a
    // Reach is a kill delivered, so no spawn casts anything while any tile
    // beside any spawn lies in one — the disaster fallback included, an empty
    // colony's first creep least affording to be born under fire.
    let doorstepInReach (s: SpawnInfo) =
        match Atlas.targetRoom atlas s.Id, Atlas.positionOf atlas s.Id with
        | Some room, Some pos ->
            let reach = Threats.reachIn threats room

            RoomPos.pos pos
            |> tilesWithin 1
            |> List.exists (fun tile -> Set.contains tile reach)
        | _ -> false

    // Asked before anything is priced, the way the reflexes ask their
    // hostiles first: a held tick derives no Workforce target and floods
    // no lead.
    if view.Spawns |> List.exists doorstepInReach then
        [], Quotas.silent
    else

        // Every specialist row's quota rule in one value (`quotaRowsOf`, ADR
        // 0006, ADR 0012, ADR 0042, ADR 0046, ADR 0056): one Anchor per Post,
        // haulers per the throughput arithmetic — the hauler quota arriving
        // memoised on the census signature (ADR 0017), which signs the *union*
        // of what the Layout and the quota read, neither input set containing
        // the other — the reserver row's demand list, the guard row's zero on
        // every ordinary tick, the income those leave and the standing upgrade
        // row that income buys. Read once here because every row is an addend of
        // the target below *and* a gap of its own in the cascade, and a body
        // hired against one reading and counted against another is an oversell
        // every tick.
        let rows = quotaRowsOf view atlas sizing haulerQuota

        let target = workforceTarget view atlas tasks rows

        // The deficit and every row gap count the creeps that will still be
        // alive when a replacement could arrive: an expiring creep is already
        // outside the count (ADR 0026), so its successor is cast while it still
        // works.
        let living =
            view.Creeps |> List.filter (fun creep -> not (expiring view atlas sizing creep))

        // The bodies already bought and not yet standing (#156). A creep in an
        // oven is in no `Creeps` list — it cannot act, cannot be matched and
        // holds no tile — so every row's living count read straight past it,
        // and a colony with **two** idle spawns bought the same seat twice:
        // spawn one casts an Anchor for the empty Post at tick T, and at T+1
        // the gap is still one and spawn two casts a second for the same Post.
        // ADR 0026 rejected counting a gestating body on a reason true of one
        // spawn and of no other number of them.
        let casting = view.Casting

        let castOf pattern =
            casting
            |> List.filter (fun body -> patternOfCast view.Tuning body = pattern)
            |> List.length

        let deficit = target - (List.length living + List.length casting)

        // A body is sized to the bank's capacity and cast the tick the bank
        // holds its cost (ADR 0021) — a full bank for rows priced at capacity,
        // sooner for the capped Anchor row. Disaster fallback: an empty colony
        // can never refill extensions, so a capacity-sized body would wait
        // forever — spawn a minimal worker unit from whatever is banked right
        // now, time-to-first-creep outranking specialisation (ADR 0006). The
        // row's sizing rule arrives as a function of the bank rather than being
        // looked up from the pattern, which is the choice ADR 0042's reserver
        // row forces: two rows are the bank's answer alone, but the reserver's
        // body is `min(reservation deficit, bank)` and the anchor's is capped
        // by the ceiling of the **Post** the cast is filling (ADR 0053) — a
        // fact about the room being reserved and a fact about one rock, neither
        // of them about the row.
        let castFromBank pattern (sizing: RoomEnergy -> BodyPart list) (bank: RoomEnergy) =
            if List.isEmpty view.Creeps then
                if bank.Available >= bodyCost workerPattern.Block then
                    Some(workerPattern, workerPattern.Block)
                else
                    None
            else
                let body = sizing bank

                if bank.Available >= bodyCost body then
                    Some(pattern, body)
                else
                    None

        // Every body the anchor row counts, as a list and not only as a
        // number: `emptyPostCaps` below reads the tiles they stand on where the
        // row reads their count, so the census is asked once and the two halves
        // cannot disagree about who is manning a Post.
        let isAnchorBody (creep: CreepInfo) = Atlas.workHeavy atlas creep.Name

        let anchorGarrison = living |> List.filter isAnchorBody

        // **The vacancies this row is casting into, richest ceiling first**
        // (ADR 0053): every Post with nobody standing on it who will still be
        // there when a replacement could arrive. This is what pairs a body to a
        // rock in an architecture where no caster knows which Post a finished
        // Anchor will man (ADR 0021, ADR 0006) — the row is sizing for the hole
        // it is filling, not for a posting. Judged at **arrival** and never by
        // who is standing there now (ADR 0026): an expiring incumbent is
        // already out of `living`, so the Post it is still standing on reads
        // empty and its own successor is sized off its own rock. Read off who
        // is standing instead, an ordinary home succession would find its Post
        // occupied, fall through to whatever outpost Post happened to be free,
        // and buy the home room's replacement off a neutral rock.
        let emptyPostCaps =
            let manned =
                anchorGarrison
                |> List.choose (fun creep -> Atlas.creepTile atlas creep.Name)
                |> Set.ofList

            sizing.AnchorPostCaps
            |> Map.toList
            |> List.filter (fun (tile, _) -> not (Set.contains tile manned))
            |> List.map snd
            |> List.sortDescending

        // The ceiling every Anchor cast this tick is sized under: the **dearest
        // vacancy's** (ADR 0053), with the richest Post the row hires for
        // standing in to keep the expression total. One ceiling for the tick's
        // casts and not one per vacancy at its own rock, because the caster
        // cannot steer the finished body: the Matcher pairs it to a Post by
        // travel cost and knows nothing of which vacancy it was bought for (ADR
        // 0021's rejection). A tick that bought `6W` for a held vacancy and
        // `3W/1C/1M` for a neutral one beside it has bought a body that can
        // land on the held rock and dig six where the rock gives ten — the
        // "cheapest vacancy first" ADR 0053 rejects by name.
        let anchorCap =
            emptyPostCaps
            |> List.tryHead
            |> Option.defaultValue (richestAnchorCap sizing.AnchorPostCaps)

        // Reserver gaps are filled before Anchor gaps, Anchor gaps before
        // hauler gaps, hauler gaps before upgrader gaps and those before
        // generalist gaps (ADR 0046) — and the worker row's quota is whatever
        // the target has left. The reserver goes in front of all four (ADR
        // 0042): the other rows spend income, and this one decides whether the
        // income is five a tick or ten across every source of an outpost at
        // once. The guard goes in front of *it* (ADR 0056), behind the supply
        // floor alone: it is zero for the whole of a colony's ordinary life,
        // and on the ticks it is not, every tick of delay is an [[anchor]] and
        // a [[hauler unit]] dying in a room a reserver would walk into next.
        // Being first it is asked first, and it does not *hold* the cascade the
        // tick the bank cannot pay for it: a row the bank cannot afford yields
        // the tick to the rows below it (ADR 0050), which at a 300 bank is
        // exactly what the guard row does. Each specialist gap is that row's
        // own unfilled quota, answered on its own terms rather than out of the
        // deficit: an empty Post is a fact about the ground, and the row that
        // hires for it does not stop hiring because the headcount overshot some
        // other row's arithmetic.
        //
        // That order is this list's and nowhere else's: the seats are cast in
        // it, the `quotas` view reports in it, and the worker row subtracts the
        // sum of it. Written out three times it was three orderings kept in
        // step by hand, and a seventh pattern was four edits the compiler could
        // not check.
        // What the two rows whose sizing reads a second fact read this tick
        // (`Bodies.sizedBodyFor`): the dearest vacancy's Work ceiling, and the
        // outstanding reserver demands. A positive gap on the reserver row is a
        // non-empty demand list, so that arm's `List.max` is total wherever it
        // is reached.
        let rowSizing =
            {
                AnchorCap = anchorCap
                ReserverClaims = rows.Reserver
            }

        let rows: SpecialistRow list =
            [
                // The guard row's own `Living` is where ADR 0056's "no decay"
                // lands: a guard that outlived its raid is still an ATTACK body
                // in the fleet, so the next raid inside its life reads a filled
                // row and waits no thirty ticks of oven for a body it already
                // owns. Counted over the **fleet** against a quota derived per
                // room, which is the reserver row's own shape one entry down
                // (ADR 0042): the row counts bodies, and which room each
                // finished body works is the Matcher's, priced by travel cost.
                {
                    Name = "guard"
                    Pattern = guardPattern
                    Quota = rows.Guard
                    // Priced at capacity like every row but the floor (ADR
                    // 0021), so a bank that cannot hold 750 casts nothing here
                    // and yields to the reserver behind it (ADR 0050): a colony
                    // that small has ADR 0043's [[stand-down]] and nothing else.
                    Census = isGuardBody
                }
                {
                    Name = "reserver"
                    Pattern = reserverPattern
                    Quota = List.length rows.Reserver
                    // Sized at the largest outstanding demand, which is
                    // `sizedBodyFor`'s own reserver arm over `rowSizing` below.
                    Census = isReserverBody
                }
                {
                    Name = "anchor"
                    Pattern = anchorPattern
                    Quota = rows.Anchor
                    // Sized under the dearest **vacancy**'s ceiling and never a
                    // colony-wide constant (ADR 0053): which Post the finished
                    // body lands on is the Matcher's, so the cast carries the
                    // saturation of the richest rock this row has a hole on —
                    // `rowSizing`'s `AnchorCap` below.
                    Census = isAnchorBody
                }
                {
                    Name = "hauler"
                    Pattern = haulerPattern
                    Quota = rows.Hauler
                    Census = isHaulerBody
                }
                // Behind the three rows hired off the ground and ahead of the
                // generalist (ADR 0046): the upgrader spends the surplus those
                // three produce, so it is cast once they stand, and it spends it
                // at eleven Work against the generalist's nine.
                //
                // Bodies and not names (ADR 0006): the row's living count is
                // what `patternOf` reads back off the parts, so a `11W/1C/11M`
                // the colony inherited or was handed fills this quota exactly
                // as one it cast does. Asking `patternOf` rather than
                // `isStandingBody` alone is what keeps the Anchor row out of
                // it: `6W/1C/1M` answers to both descriptions and it is the
                // anchor arm that claims it, so an Anchor at its Post never
                // pays off an upgrader's gap.
                {
                    Name = "upgrader"
                    Pattern = upgraderPattern
                    Quota = rows.Upgrader
                    Census = fun creep -> patternOf view.Tuning atlas creep = upgraderPattern
                }
            ]

        // Each row's living count, what it already has in an oven (#156), and
        // the gap those two leave against its quota — read once here, because
        // all three readings below want all three numbers.
        let filled =
            rows
            |> List.map (fun row ->
                let alive = living |> List.filter row.Census |> List.length
                let inOven = castOf row.Pattern
                row, alive, inOven, row.Quota - alive - inOven |> max 0)

        // The tick's arithmetic, written down for the `quotas` view (ADR
        // 0009: a record returned, never logged).
        let quotas: Quotas =
            let specialists = filled |> List.sumBy (fun (row, _, _, _) -> row.Quota)

            {
                Target = target
                Living = List.length living
                Casting = List.length casting
                HaulerLoad = 0
                HaulerDemand = []
                Rows =
                    (filled
                     |> List.map (fun (row, alive, inOven, _) ->
                         ({
                             Row = row.Name
                             Quota = row.Quota
                             Living = alive
                             Casting = inOven
                         }
                         : RowQuota)))
                    @ [
                        // The generalist takes what the target has left after
                        // the five specialist rows, and its living count is the
                        // fleet less theirs — a partition only because each of
                        // the five censuses above claims a body no other does.
                        ({
                            Row = "worker"
                            Quota = target - specialists |> max 0
                            Living =
                                List.length living
                                - (filled |> List.sumBy (fun (_, alive, _, _) -> alive))
                            Casting = castOf workerPattern
                        }
                        : RowQuota)
                    ]
            }

        // The supply floor (ADR 0050), and the one row that is not a quota: a
        // colony holding no body that can put energy into an extension hires
        // one hauler in front of every row, sized from what is banked **right
        // now**. It is the disaster fallback's own argument (ADR 0006) carried
        // to the state that fallback cannot see. Every row below it prices its
        // body at `bank.Capacity`, so none is buyable until the extensions are
        // full — and the extensions are filled by creeps.
        let supplyFloor =
            if
                view.Creeps |> List.exists (canRefill view.Tuning atlas)
                // Or one already bought (#156): the floor asks whether the bank
                // can ever be filled again, and a hauler nine ticks from
                // standing answers yes — buying a second one out of the same
                // stranded bank is the oversell this row exists to make exactly
                // once.
                || casting |> List.exists (castCanRefill view.Tuning)
            then
                0
            else
                1

        // The rows expanded into the seats they are owed, in casting order: the
        // supply floor, then the table's own order, and last the generalist,
        // whose seats are whatever the whole-fleet deficit has left once every
        // row above is counted. The deficit gates the *worker* row alone and
        // stands in for that row's own gap: ADR 0012 hires it against whatever
        // the target has left once the specialist rows are counted, and the
        // whole-fleet gap less the rows above is exactly that remainder while
        // every specialist row is at or under quota.
        let specialistSeats = filled |> List.sumBy (fun (_, _, _, gap) -> gap)

        let seats =
            List.replicate
                supplyFloor
                // The one row sized from `Available` (with the disaster
                // fallback inside `castFromBank`, for the same reason).
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Available))
            @ (filled
               |> List.collect (fun (row, _, _, gap) ->
                   List.replicate
                       gap
                       (castFromBank row.Pattern (fun bank ->
                           sizedBodyFor rowSizing row.Pattern bank.Capacity))))
            @ List.replicate
                (deficit - (supplyFloor + specialistSeats) |> max 0)
                (castFromBank workerPattern (fun bank -> bodyFor workerPattern bank.Capacity))


        // Idle spawns draw from the colony's one bank in list order — each body
        // debits the budget the next spawn sees, so the same energy is never
        // committed twice. One bank and never a map keyed by the spawn's room
        // (ADR 0052 decision 1): every spawn a colony casts from stands in its
        // home room.
        let intents, _, _ =
            view.Spawns
            |> List.filter (fun s -> not s.IsSpawning)
            |> List.fold
                (fun
                    (intents,
                     bank: RoomEnergy,
                     unfilled: (RoomEnergy -> (BodyPattern * BodyPart list) option) list)
                    s ->
                    // The first seat this bank can pay for, and the rest of
                    // the list with exactly that seat taken out of it.
                    let rec take passed remaining =
                        match remaining with
                        | [] -> None
                        | cast :: rest ->
                            match cast bank with
                            | Some filled -> Some(filled, List.rev passed @ rest)
                            | None -> take (cast :: passed) rest

                    match take [] unfilled with
                    | Some((pattern, body), left) ->
                        SpawnCreep(s.Name, body, $"{pattern.Name}-{view.Time}-{s.Name}") :: intents,
                        { bank with
                            Available = bank.Available - bodyCost body
                        },
                        left
                    | None -> intents, bank, unfilled)
                ([], view.Bank, seats)

        List.rev intents, quotas
