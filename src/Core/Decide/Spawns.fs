/// Reading a row off a living body, and the spawn plan that fills the gap
/// between the quota and the roster: which pattern a creep was cast from, what
/// a row leads by, and what to cast next.
[<AutoOpen>]
module Fabot.Core.Decide.Spawns

open Fabot.Core
open Fabot.Core.Types

/// The reserver row's living census: a CLAIM part, the one part no other row
/// buys. `patternOfParts`' reserver arm is this same test, asked before the
/// comparative tests: `[Claim; Carry; Move]` has no Work beside a Carry, so a
/// cascade that reached the hauler arm first would read a reserver as a hauler.
let private isReserverBody (creep: CreepInfo) = partCount creep.Body BodyPart.Claim > 0

/// The hauler row's census: Carry parts and no Work.
let private isHaulerBody (creep: CreepInfo) =
    not (isNamedFor courierPattern creep.Name)
    && partCount creep.Body Work = 0
    && partCount creep.Body Carry > 0

/// Whether a body still in the oven is Work-heavy — `Work > Move`, the reading
/// a living creep gets from `Atlas.workHeavy`, which a body with no name yet
/// cannot be looked up in.
let private castIsHeavy parts =
    partCount parts Work > partCount parts Move

/// The pattern row a living body was cast from: its name's, for the rows read
/// back that way (`patternByName`), and its parts' for every other.
let private patternOf (tuning: Tuning) atlas (creep: CreepInfo) =
    patternByName creep.Name
    |> Option.defaultWith (fun () ->
        patternOfParts tuning (Atlas.workHeavy atlas creep.Name) creep.Body)

/// The row a body **still in the oven** was bought for: the same rule over the
/// same name and counts. Preserving the name also prevents a 1,500-capacity
/// hauler in one oven from filling the courier row's gap in another.
let private patternOfCast (tuning: Tuning) (cast: CastingInfo) =
    patternByName cast.Name
    |> Option.defaultWith (fun () ->
        let parts = partsOf cast.Body
        patternOfParts tuning (castIsHeavy parts) parts)

/// Whether a living body can put energy into an extension.
let private canRefill (tuning: Tuning) atlas (creep: CreepInfo) =
    canRefillParts tuning (Atlas.workHeavy atlas creep.Name) creep.Body

/// `canRefillParts` over a cast rather than a living creep, for the supply
/// floor's one question: is there anything, alive or bought, that can break the
/// deadlock?
let private castCanRefill (tuning: Tuning) (body: BodyPart list) =
    let parts = partsOf body
    canRefillParts tuning (castIsHeavy parts) parts

/// The largest ceiling the row's Posts ask for, and the held one where it has
/// no Post at all.
let private richestAnchorCap (caps: Map<RoomPos, int>) =
    caps
    |> Map.fold (fun richest _ cap -> max richest cap) 0
    |> function
        | 0 -> heldWorkCap
        | cap -> cap

/// The ceiling of the Post at a tile, and the richest one for a tile that is no
/// Post — the nothing-in-particular a body still walking to one stands on.
let private anchorCapAt (caps: Map<RoomPos, int>) (tile: RoomPos) =
    match Map.tryFind tile caps with
    | Some cap -> cap
    | None -> richestAnchorCap caps

/// Legs for an Anchor bought while these Posts stand vacant: an outpost's walk
/// if the finished body may take any of them that is an outpost's (#515),
/// since travel cost and not the caster picks its Post.
let private anchorLegsFor (view: ColonyView) (vacant: RoomPos seq) =
    let home = SpatialInfo.homeName view.Spatial

    if vacant |> Seq.exists (fun tile -> anchorLegsIn home tile.Room = OutpostLegs) then
        OutpostLegs
    else
        HomeLegs

/// The legs an incumbent's successor is cast with: its own Post's, or an
/// outpost's while an outpost Post stands with no heavy body on it — the cast
/// that replaces it will be bought beside that vacancy. Read off who stands
/// where now, not off arrival: a lead cannot read the vacancies leads decide.
let private successorLegs (view: ColonyView) atlas (sizing: RowSizing) (tile: RoomPos) =
    let manned () =
        view.Creeps
        |> List.filter (fun creep -> Atlas.workHeavy atlas creep.Name)
        |> List.choose (fun creep -> Atlas.creepTile atlas creep.Name)
        |> Set.ofList

    match anchorLegsFor view [ tile ] with
    | OutpostLegs -> OutpostLegs
    | HomeLegs ->
        let manned = manned ()

        sizing.AnchorPostCaps
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.filter (fun post -> not (Set.contains post manned))
        |> anchorLegsFor view

/// The body a row casts to put a creep on one tile, at this colony's bank and
/// under this tick's second fact where the row has one. A tile and not a row,
/// because the anchor row casts no single body: its ceiling is the rock the
/// Post seats, so an incumbent on the home Post is led by a six-Work successor
/// while one on an outpost's lapsed Post is led by a three-Work one, off one
/// rule.
let private castBodyOf
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (pattern: BodyPattern)
    (tile: RoomPos)
    =
    sizedBodyFor
        {
            AnchorCap = anchorCapAt sizing.AnchorPostCaps tile
            AnchorLegs =
                if pattern = anchorPattern then
                    successorLegs view atlas sizing tile
                else
                    HomeLegs
            ReserverClaims = sizing.ReserverClaims
            MinerWorkPerMove = sizing.MinerWorkPerMove
            GuardBlocks = sizing.GuardBlocks
            RangerBlocks = sizing.RangerBlocks
            UpgraderWork = upgraderWorkCeiling sizing
            DefenderBlocks = view.Tuning.DefenderBlocks
        }
        pattern
        view.Bank.Capacity

/// ADR-0026
/// A creep's lead: the ticks its replacement needs to stand where it stands —
/// the successor body's cast time plus its walk out of the spawn, priced for
/// the successor's own fatigue factor. The body is the one this colony's row
/// would cast for the incumbent's own tile, not the largest it could cast. The
/// walk starts beside the spawner, where the engine places the finished creep.
/// Geometry that prices nothing leads nobody, and a lead of 0 leaves every
/// living creep counted.
let private leadOf (view: ColonyView) atlas (sizing: RowSizing) (creep: CreepInfo) : int =
    let pattern = patternOf view.Tuning atlas creep

    match Atlas.creepTile atlas creep.Name with
    | None -> 0
    | Some tile ->
        // The colony's one bank, whatever room the spawn is filed under:
        // every spawn stands in the home room.
        let body = castBodyOf view atlas sizing pattern tile

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

/// ADR-0069
/// The extra life an incumbent standing in a declared errand room must retain
/// before it still counts in its row. A property of the remote seat, not of a
/// CLAIM body: a reserver or claimer at home keeps the ordinary lead.
let private errandOverlap (view: ColonyView) atlas (creep: CreepInfo) =
    match Atlas.creepRoom atlas creep.Name with
    | Some room when view.Errands |> List.exists (fun errand -> errand.RoomName = room) ->
        view.Tuning.ReclaimerOverlap
    | _ -> 0

/// Whether a creep is expiring: its remaining life is at or under its lead, so
/// it leaves the living count and its row's gap. An incumbent in an errand room
/// leaves one handover window early, which with `Reclaim`'s matching capacity
/// lets both bodies stand at the remote seat for that window.
let internal expiring (view: ColonyView) atlas (sizing: RowSizing) (creep: CreepInfo) =
    if patternOf view.Tuning atlas creep = courierPattern then
        // A courier is economically spent after one delivery slot even though
        // the shorter W15S28 route leaves physical life behind (#319): counting
        // the remainder would make the next load depend on a second Source
        // Keeper crossing by the same body.
        creep.TicksToLive <= Engine.creepLifetime - view.Tuning.DeliveryInterval
    else
        creep.TicksToLive
        <= leadOf view atlas sizing creep + errandOverlap view atlas creep

/// One specialist row of the spawn cascade, stated once: the name the `quotas`
/// view files it under, the pattern it casts, how many bodies it wants and
/// which living bodies answer to it. The gap, the report and the seats are each
/// one pass over the list of these, so the three orders cannot drift apart.
type private SpecialistRow =
    {
        Name: string
        Pattern: BodyPattern
        Quota: int
        Census: CreepInfo -> bool
    }


/// Whether a row waits while a [[defender]] is owed (#504): an economic row,
/// and never a fighting one — the guard, the ranger, the squad's — nor the
/// defender's own.
let heldForDefender (pattern: BodyPattern) =
    not (isOwnRowPattern pattern)
    && pattern.Name <> guardPattern.Name
    && pattern.Name <> rangerPattern.Name

/// The Anchors stranded off the projection (#506), out of the row's count:
/// one on no Post that does not live to walk to a vacant one — the Pool's
/// in-time sum, `childStay` included — and one standing in a room the colony
/// does not project (`Unprojected`), which the mover never steps whatever its
/// life, or in an [[evacuating room]], which it is walking out of. A relief walking to its expiring incumbent's Post reaches a vacancy
/// and counts. With no vacancy the garrison fills the row without them, so
/// nobody is stranded; with some, at most one placed walker per vacancy
/// counts, since two bodies walking to one Post fill one seat.
let internal strandedAnchors
    (view: ColonyView)
    atlas
    (posts: Map<RoomPos, int>)
    (vacant: Map<RoomPos, int>)
    (garrison: CreepInfo list)
    : Set<string> =
    if Map.isEmpty vacant then
        Set.empty
    else
        let unprojected, offPost =
            garrison
            |> List.filter (fun creep ->
                match Atlas.creepTile atlas creep.Name with
                | Some tile -> not (Map.containsKey tile posts)
                | None -> Set.contains creep.Name view.Unprojected)
            // One in an evacuating room (#512) is leaving, not reaching a Post.
            |> List.partition (fun creep ->
                match Atlas.creepTile atlas creep.Name with
                | Some tile -> Set.contains tile.Room view.Evacuating
                | None -> true)

        let rocks =
            vacant
            |> Map.toList
            |> List.choose (fun (tile, _) -> Atlas.sourceOfPost atlas tile.Room (RoomPos.pos tile))

        let reaches (creep: CreepInfo) =
            rocks
            |> List.exists (fun rock ->
                let task = Harvest rock
                let stay = childStay view atlas creep task |> Option.defaultValue 0
                outlivesWalk creep stay (Atlas.walkTicks atlas creep.Name task))

        let reaching, short = offPost |> List.partition reaches

        unprojected
        @ short
        @ List.skip (min (Map.count vacant) (List.length reaching)) reaching
        |> List.map (fun creep -> creep.Name)
        |> Set.ofList

/// The spawn Intents the Workforce target's rows are owed. The target is the
/// quota the *generalist* row is hired against; every other row is hired
/// against its own unfilled quota and can carry the fleet past the target.
/// Reads the tick's Task pool for one number, the worker row's floor, and never
/// feeds it.
let internal planSpawns
    (view: ColonyView)
    atlas
    (outposts: OutpostFacts)
    (sizing: RowSizing)
    (threats: Threats)
    (tasks: Task list)
    (busy: BusyReason list)
    (haulerQuota: int)
    : Intent list * Quotas =
    // The spawn holds while its doorstep is hot: a creep born into a Reach is
    // a kill delivered, the disaster fallback included.
    let doorstepInReach (s: SpawnInfo) =
        match Atlas.targetRoom atlas s.Id, Atlas.positionOf atlas s.Id with
        | Some room, Some pos ->
            let reach = Threats.reachIn threats room

            RoomPos.pos pos
            |> tilesWithin 1
            |> List.exists (fun tile -> Set.contains tile reach)
        | _ -> false

    // Asked before anything is priced: a held tick derives no Workforce
    // target and floods no lead.
    if view.Spawns |> List.exists doorstepInReach then
        [], Quotas.silent
    else

        // Every specialist row's quota in one value, read once because every
        // row is an addend of the target *and* a gap of its own in the
        // cascade.
        let rows = quotaRowsOf view atlas threats outposts sizing busy haulerQuota

        let target = workforceTarget view atlas tasks rows

        // The miner row's cut: Work-heavy and no Carry, subtracted from the
        // Anchor row's census because the Carry part is the whole of the
        // difference between the two.
        let isMinerBody (creep: CreepInfo) =
            Atlas.workHeavy atlas creep.Name && partCount creep.Body Carry = 0

        let isAnchorBody (creep: CreepInfo) =
            Atlas.workHeavy atlas creep.Name && not (isMinerBody creep)

        // An expiring creep is already outside the count, so its successor is
        // cast while it still works.
        let unexpired =
            view.Creeps |> List.filter (fun creep -> not (expiring view atlas sizing creep))

        let unexpiredGarrison = unexpired |> List.filter isAnchorBody

        // ADR-0053
        // The vacancies this row is casting into: every Post with nobody
        // standing on it who will still be there when a replacement could
        // arrive. Judged at arrival and never by who is standing there now:
        // an expiring incumbent is already out of the count, so its Post
        // reads empty and its successor is sized off its own rock. Read off
        // who is standing instead, a home succession would find its Post
        // occupied and buy the replacement off a neutral rock.
        let vacantPosts =
            let manned =
                unexpiredGarrison
                |> List.choose (fun creep -> Atlas.creepTile atlas creep.Name)
                |> Set.ofList

            sizing.AnchorPostCaps
            |> Map.filter (fun tile _ -> not (Set.contains tile manned))

        // A stranded Anchor is outside the count as an expiring one is (#506).
        let stranded =
            strandedAnchors view atlas sizing.AnchorPostCaps vacantPosts unexpiredGarrison

        let living =
            unexpired |> List.filter (fun creep -> not (Set.contains creep.Name stranded))

        // The bodies already bought and not yet standing (#156): a creep in an
        // oven is in no `Creeps` list, so without this a colony with two idle
        // spawns bought the same seat twice — one Anchor per spawn for one
        // empty Post, a tick apart.
        let casting = view.Casting

        let castOf pattern =
            casting
            |> List.filter (fun cast -> patternOfCast view.Tuning cast = pattern)
            |> List.length

        // The fleet the target is a number about: every body but the squad's
        // casts and the defenders, which answer to their own rows alone
        // (`workforceTarget`).
        let inFleet name = not (ownRowOnly name)

        let deficit =
            target
            - (living |> List.filter (fun creep -> inFleet creep.Name) |> List.length)
            - (casting |> List.filter (fun cast -> inFleet cast.Name) |> List.length)

        // ADR-0006
        // A body is sized to the bank's capacity and cast the tick the bank
        // holds its cost. Disaster fallback: an empty colony can never refill
        // extensions, so a capacity-sized body would wait forever — spawn a
        // minimal worker from whatever is banked right now. The sizing rule
        // arrives as a function of the bank rather than a lookup off the
        // pattern because the reserver's body is `min(deficit, bank)` and the
        // anchor's is capped by the Post's ceiling.
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

        // The vacancies' ceilings, richest first. A stranded Anchor stands on
        // no Post, so leaving it out of the count leaves these unmoved.
        let emptyPostCaps = vacantPosts |> Map.toList |> List.map snd |> List.sortDescending

        // The ceiling every Anchor cast this tick is sized under: the dearest
        // vacancy's, the richest Post standing in to keep the expression total.
        // One ceiling for the tick's casts, because the caster cannot steer
        // the finished body: a tick that bought `6W` for a held vacancy and
        // `3W/1C/1M` for a neutral one has bought a body that can land on the
        // held rock and dig six where the rock gives ten.
        let anchorCap =
            emptyPostCaps
            |> List.tryHead
            |> Option.defaultValue (richestAnchorCap sizing.AnchorPostCaps)

        let anchorLegs = anchorLegsFor view (vacantPosts |> Map.toSeq |> Seq.map fst)

        // The casting order is the row list's below and nowhere else: the
        // seats are cast in it, the `quotas` view reports in it, and the
        // worker row subtracts the sum of it. Each specialist gap is that
        // row's own unfilled quota, never taken out of the deficit: an empty
        // Post is a fact about the ground, and the row does not stop hiring
        // because the headcount overshot some other row's arithmetic.
        //
        // A positive gap on the reserver row is a non-empty demand list, so
        // `sizedBodyFor`'s `List.max` is total wherever it is reached.
        let rowSizing =
            {
                AnchorCap = anchorCap
                AnchorLegs = anchorLegs
                ReserverClaims = rows.Reserver
                MinerWorkPerMove = sizing.MinerWorkPerMove
                GuardBlocks = sizing.GuardBlocks
                RangerBlocks = sizing.RangerBlocks
                UpgraderWork = upgraderWorkCeiling sizing
                DefenderBlocks = view.Tuning.DefenderBlocks
            }

        let squadRows: SpecialistRow list =
            SquadRole.all
            |> List.map (fun role ->
                let pattern = squadPatternOf role

                {
                    Name = pattern.Name
                    Pattern = pattern
                    Quota = Map.find role rows.Squad
                    Census = fun creep -> squadRoleByName creep.Name = Some role
                })

        let rows: SpecialistRow list =
            [
                // The home's own rampart defender (#504), ahead of every row:
                // the line it stands on is what every other row lives behind.
                {
                    Name = "defender"
                    Pattern = defenderPattern
                    Quota = rows.Defender
                    Census = fun creep -> isNamedFor defenderPattern creep.Name
                }
                // Counted over the fleet against a quota derived per room,
                // like the reserver row: which room each finished body works
                // is the Matcher's. A guard that outlived its raid is still an
                // ATTACK body, so the next raid inside its life reads a filled
                // row.
                {
                    Name = "guard"
                    Pattern = guardPattern
                    Quota = rows.Guard
                    // Priced at capacity like every row but the floor, so a
                    // bank that cannot hold 750 casts nothing here and yields
                    // to the reserver behind it.
                    Census = isGuardRowBody
                }
                // The errand room's ranged guard (#411), behind the melee one
                // and for the same reason: a fight is cast before the seats it
                // protects.
                {
                    Name = "ranger"
                    Pattern = rangerPattern
                    Quota = rows.Ranger
                    Census = isRangerRowBody
                }
                // A pooled Fight's squad (#453), its roles in the order they
                // cast: the brawler the medic walks behind, then the medic,
                // then the kiters. Each row's census is its casts, by name.
                yield! squadRows
                {
                    Name = "reserver"
                    Pattern = reserverPattern
                    Quota = List.length rows.Reserver
                    // Sized at the largest outstanding demand, `rowSizing`.
                    Census = isReserverBody
                }
                {
                    Name = "anchor"
                    Pattern = anchorPattern
                    Quota = rows.Anchor
                    // Sized under the dearest vacancy's ceiling,
                    // `rowSizing`'s `AnchorCap`.
                    Census = isAnchorBody
                }
                {
                    Name = "hauler"
                    Pattern = haulerPattern
                    Quota = rows.Hauler
                    Census = isHaulerBody
                }
                // Behind the rows that make and carry the colony's energy and
                // ahead of the two that spend it: a miner bought before the
                // anchor that feeds it spends 2,200 on a resource nothing in
                // the colony eats, and the season's score rides on this row
                // where an upgrade mouth is the surplus by construction.
                {
                    Name = "miner"
                    Pattern = minerPattern
                    Quota = rows.Miner
                    Census = isMinerBody
                }
                // The courier follows the rows that make and carry income and
                // precedes the two surplus mouths (#319); `expiring` turns its
                // cadence into the same living-count seam every row uses.
                {
                    Name = "courier"
                    Pattern = courierPattern
                    Quota = rows.Courier
                    Census = fun creep -> patternOf view.Tuning atlas creep = courierPattern
                }
                // One body bought once, ahead of the surplus mouths: what it
                // saves is CPU every tick the salvage room still stands.
                {
                    Name = "dismantler"
                    Pattern = dismantlerPattern
                    Quota = rows.Dismantler
                    Census = fun creep -> patternOf view.Tuning atlas creep = dismantlerPattern
                }
                // Bodies and not names: `patternOf` rather than
                // `isStandingBody` alone is what keeps the Anchor row out of
                // it, since `6W/1C/1M` answers to both descriptions and the
                // anchor arm claims it.
                {
                    Name = "upgrader"
                    Pattern = upgraderPattern
                    Quota = rows.Upgrader
                    Census = fun creep -> patternOf view.Tuning atlas creep = upgraderPattern
                }
            ]

        // Each row's living count, what it has in an oven, and the gap those
        // leave against its quota — read once, because all three readings
        // below want all three numbers.
        let filled =
            rows
            |> List.map (fun row ->
                let alive = living |> List.filter row.Census |> List.length
                let inOven = castOf row.Pattern
                row, alive, inOven, row.Quota - alive - inOven |> max 0)

        // The rows the target and the deficit count: every row but the
        // squad's and the defender's.
        let fleetRows =
            filled |> List.filter (fun (row, _, _, _) -> not (isOwnRowPattern row.Pattern))

        // The tick's arithmetic, written down for the `quotas` view.
        let quotas: Quotas =
            let specialists = fleetRows |> List.sumBy (fun (row, _, _, _) -> row.Quota)

            {
                Target = target
                Living = List.length living
                Casting = List.length casting
                HaulerLoad = 0
                HaulerDemand = []
                Overflow = HaulOverflow.none
                Fights = []
                Busy = []
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
                        // The generalist takes what the target has left, and
                        // its living count is the fleet less the specialists —
                        // a partition only because each census above claims a
                        // body no other does.
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

        // ADR-0050
        // The supply floor, the one row that is not a quota: a colony holding
        // no body that can put energy into an extension hires one hauler in
        // front of every row, sized from what is banked right now. Every row
        // below prices at `bank.Capacity`, so none is buyable until the
        // extensions are full — and the extensions are filled by creeps.
        let supplyFloor =
            if
                view.Creeps |> List.exists (canRefill view.Tuning atlas)
                // Or one already bought (#156): a hauler nine ticks from
                // standing answers yes, and a second out of the same stranded
                // bank is the oversell this row exists to make exactly once.
                || casting |> List.exists (fun cast -> castCanRefill view.Tuning cast.Body)
            then
                0
            else
                1

        // The supply floor's rule for income instead of refill (#460): a
        // colony with a Post and no body standing or casting that harvests
        // it casts its first Anchor from what is banked, while the extensions
        // are short. A worker that can refill reads as covered above and
        // still has nothing to draw.
        let harvestFloor =
            if
                filled
                |> List.forall (fun (row, _, _, _) -> row.Pattern <> anchorPattern || row.Quota = 0)
                || view.Bank.Available >= view.Bank.Capacity
                // A stranded Anchor still answers: the floor is for a colony
                // with no harvester at all, and one whose outpost anchor is
                // merely stuck has income enough to wait for a whole body
                // (W17S25 bought a 3W one off a short bank, 2026-10-05).
                || view.Creeps
                   |> List.exists (fun creep -> isAnchorBody creep || isMinerBody creep)
                // Exactly once, as above: a harvester in the oven answers.
                || casting
                   |> List.exists (fun cast ->
                       let pattern = patternOfCast view.Tuning cast
                       pattern = anchorPattern || pattern = minerPattern)
            then
                0
            else
                1

        // The seats in casting order: the two floors, the table's own
        // order, and last the generalist, whose seats are whatever the
        // whole-fleet deficit has left. The deficit gates the worker row
        // alone: the whole-fleet gap less the rows above is exactly that
        // row's remainder while every specialist row is at or under quota.
        let specialistSeats = fleetRows |> List.sumBy (fun (_, _, _, gap) -> gap)

        // A defender owed holds the economic rows (#504): a seat the bank
        // cannot pay yet is otherwise passed over for a cheaper one, and a
        // worker bought while the line falls is the bank the defender waited
        // on. The fighting rows are never held.
        let defenderOwed =
            filled
            |> List.exists (fun (row, _, _, gap) -> row.Pattern = defenderPattern && gap > 0)

        let owed (row: SpecialistRow) =
            not defenderOwed || not (heldForDefender row.Pattern)

        let rowSeats (rows: (SpecialistRow * int * int * int) list) =
            rows
            |> List.filter (fun (row, _, _, _) -> owed row)
            |> List.collect (fun (row, _, _, gap) ->
                List.replicate
                    gap
                    (castFromBank row.Pattern (fun bank ->
                        sizedBodyFor rowSizing row.Pattern bank.Capacity)))

        // The upgrader row casts after the workers (user, 2026-10-05): W17S25
        // held its one remaining worker's seat behind an upgrader the bank
        // could not yet pay while its west line lay broken, and the idle
        // upgraders had nothing to drink. The surplus mouth waits on the
        // hands that build and repair.
        let upgraderRows, otherRows =
            filled |> List.partition (fun (row, _, _, _) -> row.Pattern = upgraderPattern)

        let seats =
            List.replicate
                harvestFloor
                (castFromBank anchorPattern (fun bank ->
                    sizedBodyFor rowSizing anchorPattern bank.Available))
            @ List.replicate
                supplyFloor
                // Sized from `Available` like the harvest floor (with the disaster
                // fallback inside `castFromBank`, for the same reason).
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Available))
            @ rowSeats otherRows
            @ List.replicate
                (if defenderOwed then
                     0
                 else
                     deficit - (harvestFloor + supplyFloor + specialistSeats) |> max 0)
                (castFromBank workerPattern (fun bank -> bodyFor workerPattern bank.Capacity))
            @ rowSeats upgraderRows


        // Idle spawns draw from the colony's one bank in list order — each
        // body debits the budget the next spawn sees, so the same energy is
        // never committed twice.
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
