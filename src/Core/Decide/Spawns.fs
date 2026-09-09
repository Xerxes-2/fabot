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

/// The two facts the two rows whose sizing is not the bank's answer alone read,
/// derived once for the tick (ADR 0042): the anchor row's Work ceilings and the
/// reserver row's outstanding CLAIM demands. Together with the bank they say
/// what **this colony's rows will cast this tick** (ADR 0052 decision 4), which
/// is the number three readers have to agree on: the casting cascade that buys
/// the body, the amortization that charges for it, and the lead that prices its
/// succession. A record and not two arguments, and derived in
/// `decideUnarbitrated` rather than per reader, because both folds walk the
/// projection and a lead is priced once per living creep in two different steps
/// of the tick. Neither field may be derived from a creep's remaining life (ADR
/// 0053): a [[lead]] is priced off this record, so which Posts stand *empty* —
/// an arrival-time judgement (ADR 0026) — cannot be a field of it without
/// closing a circle.
type RowSizing =
    {
        /// `postWorkCapsOf`'s answer this tick — one ceiling per [[post]],
        /// keyed by the Post's own tile.
        AnchorPostCaps: Map<RoomPos, int>
        /// `reserverClaimsOf`'s answer this tick — one entry per room the
        /// row hires for, each that room's CLAIM demand.
        ReserverClaims: int list
    }

let internal rowSizingOf (view: ColonyView) atlas : RowSizing =
    {
        AnchorPostCaps = postWorkCapsOf view atlas
        ReserverClaims = reserverClaimsOf view
    }

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
    let capacity = view.Bank.Capacity

    if pattern.Name = anchorPattern.Name then
        anchorBodyFor (anchorCapAt sizing.AnchorPostCaps tile) capacity
    elif pattern.Name = reserverPattern.Name then
        match sizing.ReserverClaims with
        | [] -> bodyFor reserverPattern capacity
        | claims -> reserverBodyWithin (List.max claims) capacity
    else
        bodyFor pattern capacity

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

        // The specialist rows' quota rules (ADR 0006, ADR 0012): one Anchor per
        // Post, haulers per the throughput arithmetic — the hauler quota
        // arriving memoised on the census signature (ADR 0017), which signs the
        // *union* of what the Layout and the quota read, neither input set
        // containing the other. Both quotas are addends of the target itself.
        // One Anchor per Post of *every* projected room (ADR 0042): an
        // outpost's Post is the same garrison tile a home Post is, so it hires
        // from the same row and travel cost pins each Anchor on the Post
        // nearest it.
        let anchorQuota = Atlas.postCount atlas

        // The reserver row's quota and its body in one value (ADR 0042): one
        // entry per declared outpost, each entry that outpost's CLAIM demand,
        // and the largest of them is what every cast this tick carries.
        let reserverClaims = sizing.ReserverClaims

        // The guard row's quota (ADR 0056): zero on every ordinary tick, and on
        // the ticks a raid stands in a declared outpost, one or two per raided
        // room. Read here beside the others because it is an addend of the
        // target below and a gap of its own in the cascade.
        let guardQuota = guardQuota view

        // The anchor row's ceilings this tick, one per Post, read once beside
        // the quotas for the reason the reserver's demand list is (ADR 0042,
        // ADR 0053): the row's bodies are what the amortization is charged and
        // what the casts below buy, and each Post's two readings must be one
        // body.
        let anchorPostCaps = sizing.AnchorPostCaps

        // The income the two upgrade rows are hired out of, once (ADR
        // 0046): the standing row's quota is derived from it and the
        // commuting row's is derived from what that quota leaves, so the
        // two must read one number and not two spellings of it.
        let surplus =
            surplusOverLifetime view atlas reserverClaims anchorPostCaps haulerQuota

        // The upgrader row's quota (ADR 0046), read here beside the other
        // rows' for the same reason: it is an addend of the target below
        // and a gap of its own in the cascade, and a body hired for one
        // and not counted in the other would be an oversell every tick.
        let upgraderQuota = upgraderQuota view atlas surplus

        let target =
            workforceTarget
                view
                atlas
                tasks
                reserverClaims
                guardQuota
                anchorQuota
                haulerQuota
                upgraderQuota
                surplus

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
        // The guard row's own `Living` is where ADR 0056's "no decay" lands: a
        // guard that outlived its raid is still an ATTACK body in the fleet, so
        // the next raid inside its life reads a filled row and waits no thirty
        // ticks of oven for a body it already owns. Counted over the **fleet**
        // against a quota derived per room, which is the reserver row's own
        // shape one line down (ADR 0042): the row counts bodies and which room
        // each finished body works is the Matcher's, priced by travel cost.
        let guardLiving = living |> List.filter isGuardBody |> List.length

        let guardGap = guardQuota - guardLiving - castOf guardPattern |> max 0

        let reserverLiving = living |> List.filter isReserverBody |> List.length

        let reserverGap =
            List.length reserverClaims - reserverLiving - castOf reserverPattern |> max 0

        let anchorGarrison =
            living |> List.filter (fun creep -> Atlas.workHeavy atlas creep.Name)

        let anchorLiving = List.length anchorGarrison

        let anchorGap = anchorQuota - anchorLiving - castOf anchorPattern |> max 0

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

            anchorPostCaps
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
            |> Option.defaultValue (richestAnchorCap anchorPostCaps)

        let haulerLiving = living |> List.filter isHaulerBody |> List.length

        let haulerGap = haulerQuota - haulerLiving - castOf haulerPattern |> max 0

        // Bodies and not names (ADR 0006): the row's living count is what
        // `patternOf` reads back off the parts, so a `11W/1C/11M` the colony
        // inherited or was handed fills this quota exactly as one it cast does.
        // Asking `patternOf` rather than `isStandingBody` alone is what keeps
        // the Anchor row out of it: `6W/1C/1M` answers to both descriptions and
        // it is the anchor arm that claims it, so an Anchor at its Post never
        // pays off an upgrader's gap.
        let upgraderLiving =
            living
            |> List.filter (fun creep -> patternOf view.Tuning atlas creep = upgraderPattern)
            |> List.length

        let upgraderGap = upgraderQuota - upgraderLiving - castOf upgraderPattern |> max 0

        // The tick's arithmetic, written down for the `quotas` view (ADR
        // 0009: a record returned, never logged). The worker row is what
        // the target leaves after the five specialist rows, which is how
        // the cascade below hires it.
        let quotas: Quotas =
            let row name quota living casting =
                {
                    Row = name
                    Quota = quota
                    Living = living
                    Casting = casting
                }

            let specialists =
                List.length reserverClaims
                + guardQuota
                + anchorQuota
                + haulerQuota
                + upgraderQuota

            {
                Target = target
                Living = List.length living
                Casting = List.length casting
                HaulerLoad = 0
                HaulerDemand = []
                Rows =
                    [
                        row "guard" guardQuota guardLiving (castOf guardPattern)
                        row
                            "reserver"
                            (List.length reserverClaims)
                            reserverLiving
                            (castOf reserverPattern)
                        row "anchor" anchorQuota anchorLiving (castOf anchorPattern)
                        row "hauler" haulerQuota haulerLiving (castOf haulerPattern)
                        row "upgrader" upgraderQuota upgraderLiving (castOf upgraderPattern)
                        row
                            "worker"
                            (target - specialists |> max 0)
                            (List.length living
                             - guardLiving
                             - reserverLiving
                             - anchorLiving
                             - haulerLiving
                             - upgraderLiving)
                            (castOf workerPattern)
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
        // supply floor, then guard, reserver, Anchor, hauler, upgrader (ADR
        // 0042, ADR 0046, ADR 0056) and last the generalist, whose seats are
        // whatever the whole-fleet deficit has left once every row above is
        // counted. The deficit gates the *worker* row alone and stands in for
        // that row's own gap: ADR 0012 hires it against whatever the target has
        // left once the specialist rows are counted, and the whole-fleet gap
        // less the rows above is exactly that remainder while every specialist
        // row is at or under quota.
        let seats =
            List.replicate
                supplyFloor
                // The one row sized from `Available` (with the disaster
                // fallback inside `castFromBank`, for the same reason).
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Available))
            @ List.replicate
                guardGap
                // Priced at capacity like every row but the floor (ADR 0021),
                // so a bank that cannot hold 750 casts nothing here and yields
                // to the reserver behind it (ADR 0050): a colony that small has
                // ADR 0043's [[stand-down]] and nothing else.
                (castFromBank guardPattern (fun bank -> bodyFor guardPattern bank.Capacity))
            @ List.replicate
                reserverGap
                // Every cast at the largest outstanding demand and never at the
                // one standing beside it in the list: the Matcher pairs a
                // finished body to a controller by travel cost, so a body sized
                // for the room that has slipped furthest can land on the room
                // that has not. A positive gap is a non-empty demand list, so
                // the `List.max` is total inside this sizing — and it is inside
                // it, because `List.replicate 0` still evaluates the element.
                (castFromBank reserverPattern (fun bank ->
                    reserverBodyWithin (List.max reserverClaims) bank.Capacity))
            @ List.replicate
                anchorGap
                // Sized under the dearest **vacancy**'s ceiling and never a
                // colony-wide constant (ADR 0053): which Post the finished
                // body lands on is the Matcher's, so the cast carries the
                // saturation of the richest rock this row has a hole on.
                (castFromBank anchorPattern (fun bank -> anchorBodyFor anchorCap bank.Capacity))
            @ List.replicate
                haulerGap
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Capacity))
            @ List.replicate
                upgraderGap
                // Ahead of the generalist and behind the three rows hired off
                // the ground (ADR 0046): the upgrader spends the surplus those
                // three produce, so it is cast once they stand, and it spends
                // it at eleven Work against the generalist's nine.
                (castFromBank upgraderPattern (fun bank -> bodyFor upgraderPattern bank.Capacity))
            @ List.replicate
                (deficit
                 - (supplyFloor + guardGap + reserverGap + anchorGap + haulerGap + upgraderGap)
                 |> max 0)
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
