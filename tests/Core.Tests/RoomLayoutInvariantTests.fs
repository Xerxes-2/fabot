/// The Layout's invariants on real terrain, the losses the sweep found, and
/// the derived clustered horizon.
module Fabot.Core.Tests.RoomLayoutInvariantTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

[<Tests>]
let invariantTests =
    testList
        "layout invariants on real terrain"
        [
            test "the sweep covers every room from many spawns" {
                // The guard on the rest of this list: an invariant asserted
                // over an empty sweep is green and says nothing.
                let cases = sweep.Value

                Expect.isGreaterThan (List.length cases) 100 "a sweep worth the name"

                Expect.equal
                    (cases |> List.map (fun case -> case.Room.Name) |> List.distinct |> List.sort)
                    (rooms |> List.map (fun room -> room.Name) |> List.sort)
                    "every captured room is planned"

                Expect.isTrue
                    (cases
                     |> List.exists (fun case ->
                         case.Room.Name = "W12S28" && case.Spawn = { X = 12; Y = 40 }))
                    "W12S28's own spawn tile is swept"
            }

            test "only paved-buffer fallbacks leave their controller Link footing unserved" {
                // An unserved target is a guarantee the colony no longer
                // has. #331 accepts exactly one kind: a controller buffer
                // kept in a fully paved pocket at the cost of its Link.
                let cases = sweep.Value

                let accepted (case: Case) =
                    usesPavedBuffer case.Room.PavedBuffer case.Spawn

                let isControllerShortfall (case: Case) =
                    match case.Unserved with
                    | [ footing ] -> footing.Kind = FootingKind.ControllerContainer
                    | _ -> false

                Expect.isEmpty
                    (cases
                     |> List.filter (fun case ->
                         not (List.isEmpty case.Unserved) && not (accepted case))
                     |> List.map describe)
                    "no other room or spawn trades a footing for its buffer"

                Expect.equal
                    (cases |> List.filter accepted |> List.forall isControllerShortfall)
                    true
                    "every accepted case records exactly its controller-container footing"
            }

            test "the footing target count is the rule's, never a constant" {
                // The targets are the container picks plus the Storage, so
                // sources + 2 is arithmetic the room does rather than a
                // number written down.
                let miscounts (case: Case) =
                    let containers = case.SourceCount + 1

                    List.length case.Containers <> containers
                    || List.length (tilesOfKind Storage case.Placed) <> 1

                Expect.isEmpty
                    (violations miscounts)
                    "a room whose footing targets its sources do not explain"
            }

            test "every reserved footing is off the trunks, the targets and the others" {
                // The search rule in full: range 1 of its target, off every
                // tile the Layout paves, off the footings' own targets, off
                // every other footing. Real terrain is what makes this worth
                // asserting: hand-built fixtures can only pose the collisions
                // their author imagined.
                let breaksTheRule (case: Case) =
                    // A swept colony starts with no road standing and none
                    // pending, so the first tick's road sites are the whole
                    // plan.
                    let paved = tilesOfKind Road case.Placed |> Set.ofList

                    let targets =
                        (case.Served |> List.map (fun footing -> RoomPos.pos footing.Target))
                        @ (case.Unserved |> List.map (fun footing -> RoomPos.pos footing.Target))
                        |> Set.ofList

                    let tiles = case.Served |> List.map (fun footing -> RoomPos.pos footing.Tile)

                    List.length (List.distinct tiles) <> List.length tiles
                    || case.Served
                       |> List.exists (fun footing ->
                           RoomPos.range footing.Tile footing.Target <> Some 1
                           || Set.contains (RoomPos.pos footing.Tile) paved
                           || Set.contains (RoomPos.pos footing.Tile) targets)

                Expect.isEmpty
                    (violations breaksTheRule)
                    "a room reserving a footing on a road, on a target or on another footing"
            }

            test "served and unserved footings partition the room's targets" {
                // The two records are one record read two ways, so their
                // counts sum to the targets the room constructs and no
                // target is in both. Counted off the room's own arithmetic
                // rather than off sources + 2, which #104's swamp pocket is
                // a counterexample to. The containers are counted a tick on,
                // where the road plan no longer defers them, and the Storage
                // off this tick's sites; no container pick is ever a Storage
                // pick, so nothing collapses between the counts.
                let miscounts (case: Case) =
                    let targets =
                        (case.Served |> List.map (fun footing -> footing.Target))
                        @ (case.Unserved |> List.map (fun footing -> footing.Target))

                    List.length targets
                    <> List.length case.Containers + List.length (tilesOfKind Storage case.Placed)
                    || List.length (List.distinct targets) <> List.length targets

                Expect.isEmpty
                    (violations miscounts)
                    "a room whose two footing records do not partition its targets"
            }

            test "the clustered ordering never takes working ground" {
                let onWorkingGround (case: Case) =
                    clusteredTiles case.Placed
                    |> Set.exists (fun tile -> Set.contains tile case.WorkingGround)

                Expect.isEmpty
                    (violations onWorkingGround)
                    "a room clustering onto ground the colony works from"
            }

            test "a maxed room's whole cluster is inside the reservation its trunks dodged" {
                // A reservation narrower than the placement plants a tower or
                // an extension on a tile the same plan paves. Read at RCL8,
                // where the placement is widest and the slack thinnest: the
                // reservation's spare tile per Link footing hides a narrowing
                // of up to three tower slots on every case, and these two
                // spawns are the counterexamples a probe over all 171 found
                // for four. Two named cases rather than a sixth level swept
                // across all 171 for two answers.
                let collisionsAt roomName spawn =
                    let room = rooms |> List.find (fun room -> room.Name = roomName)
                    let loaded = project (load roomName) spawn room.FallbackController

                    let placed =
                        decide (colonyOf loaded 8) Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    Set.intersect (tilesOfKind Road placed |> Set.ofList) (clusteredTiles placed)

                Expect.isEmpty
                    (collisionsAt "W13S28" { X = 18; Y = 12 })
                    "W13S28 beside its live spawn: no clustered tile on a tile the same plan paves"

                Expect.isEmpty
                    (collisionsAt "W15S25" { X = 24; Y = 6 })
                    "and the other tile a narrowed reservation plants a structure on the road at"
            }

            test "the trunks carry every source to the spawn and the controller" {
                Expect.isEmpty
                    (violations (fun case ->
                        not (List.contains case.Spawn case.Room.SealedDoorsteps)
                        && not (trunksCarryEverySource case)))
                    "a source with no paved line home"
            }

            test "every tile the Layout paves is one a creep can stand on" {
                // A trunk is a paved line, so every tile of it has to be
                // walkable ground: a road on a wall is not a road.
                let pavesTheImpassable (case: Case) =
                    tilesOfKind Road case.Placed
                    |> List.exists (fun tile -> not (Set.contains tile case.Walkable))

                Expect.isEmpty
                    (violations pavesTheImpassable)
                    "a room paving ground nothing can walk"
            }

            test "no tile is asked for two structures in one tick" {
                // Ramparts are excluded: sharing a tile is what a rampart is
                // for.
                let doubleBooked (case: Case) =
                    let footprints =
                        case.Placed
                        |> List.filter (fun (_, kind) -> kind <> Rampart)
                        |> List.map fst

                    List.length (List.distinct footprints) <> List.length footprints

                Expect.isEmpty
                    (violations doubleBooked)
                    "a room asking two structures onto one tile"
            }

            test "a level-up asks for exactly what the level unlocks, less what stands" {
                // #341's invariant over every spawn of every capture: a room
                // built out at the sweep's level and then levelled asks, per
                // kind, for the next level's whole allowance minus what
                // stands. Read against the engine's table and not a second
                // plan of the same room, since a horizon wrong in the same
                // direction at both levels satisfies a plan-against-plan
                // comparison perfectly; the numbers are `allowanceOf`'s RCL4
                // and RCL5 rows, so no tile off real terrain is named.
                let entitled = [ Tower, 1, 2 - 1; StructureKind.Extension, 20, 30 - 20 ]

                let shortchanged (case: Case) = case.LevelUpAsks <> entitled

                Expect.isEmpty
                    (violations shortchanged)
                    "a room that levels and asks for less than the level unlocked it"
            }

            test "the dropped trunks the Layout records are the ones its roads show" {
                // The record pinned against an independent derivation: the
                // road plan says which sources its paved tiles do not carry
                // to which goal, and the two must name the same pairs.
                // Compared as sets, so the fold's order is not the claim.
                let disagrees (case: Case) =
                    Set.ofList (unroutedByRoads case) <> Set.ofList case.Unrouted

                Expect.isEmpty
                    (violations disagrees)
                    "a room whose dropped trunks and whose road plan tell different stories"
            }

            test "a plan recalled from its memo is the plan that was computed" {
                // Under an unchanged census the memo hands back the same
                // Intents and the same shortfall, tile for tile.
                Expect.isEmpty
                    (violations (fun case -> not case.RecallsIdentically))
                    "a room whose recalled plan differs from the computed one"
            }
        ]

[<Tests>]
let knownLossTests =
    testList
        "layout losses the sweep found"
        [
            test "an all-swamp pocket keeps its buffer and records the footing it trades (#331)" {
                // W12S27's controller sits in a pocket whose 7x7 Upgrade
                // Work Area holds no plain tile at all. The paved fallback
                // keeps the controller buffer from every spawn; its Link
                // footing is the recorded remainder of #104's loss.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W12S27")

                Expect.isNonEmpty cases "W12S27 is swept"

                Expect.all
                    cases
                    (fun case -> List.length case.Containers = case.SourceCount + 1)
                    "every spawn in W12S27 plans its source and controller containers"

                Expect.all
                    cases
                    (fun case ->
                        match case.Unserved with
                        | [ footing ] -> footing.Kind = FootingKind.ControllerContainer
                        | _ -> false)
                    "and records exactly the controller Link footing it cannot reserve"
            }

            test "a sealed spawn doorstep drops a source's trunk whole (#105)" {
                // W12S27 from 6,18: the spawn has exactly two walkable
                // neighbours and the clustered reservation takes one of
                // them, so the source-to-spawn trunk cannot be routed. The
                // working-ground exclusion guards Seats and the Upgrade area;
                // nothing guards the spawn's own doorstep. 32,2 is the same
                // mechanism from the other side, held level by level in the
                // test below. Accepted behaviour, not a pending fix: #105
                // closed without taking the doorstep exclusion it measured.
                let sealed' =
                    sweep.Value
                    |> List.filter (fun case -> List.contains case.Spawn case.Room.SealedDoorsteps)

                Expect.isNonEmpty sealed' "the sealed-doorstep case is still in the sweep"

                Expect.all
                    sealed'
                    (trunksCarryEverySource >> not)
                    "the trunk is still dropped — the exclusion records that, and comes out with the rule that fixes it"

                // The loss is per (source, goal), which this room is the live
                // counterexample for: the source→spawn trunk is dropped and
                // the source→controller trunk is routed and paved.
                Expect.all
                    sealed'
                    (fun case -> not (List.isEmpty case.Unrouted))
                    "the room says so on the layout record rather than only in its road plan"

                Expect.all
                    sealed'
                    (fun case ->
                        case.Unrouted
                        |> List.forall (fun trunk -> trunk.Goal = TrunkGoal.Spawn case.SpawnId))
                    "and names the spawn alone: the controller's trunk is routed and paved"
            }

            test "the doorstep 32,2 is sealed at every level, and no level-up pays for it (#105)" {
                // The same loss as above, reached from the other side, and a
                // function of the terrain alone: a level-blind reservation
                // closes this spawn's corridor at RCL3 exactly as at RCL8, so
                // the 95-tile corridor a level-dependent one paved at RCL4 and
                // orphaned at RCL5 is never laid. No colony stands on `32,2`.
                let loaded = project (load "W12S27") { X = 32; Y = 2 } None

                let unroutedAt level =
                    decide (colonyOf loaded level) Map.empty Set.empty None
                    |> fun decision -> decision.Memo.UnroutedTrunks

                for level in 3..8 do
                    Expect.all
                        (unroutedAt level)
                        (fun trunk -> trunk.Goal = TrunkGoal.Spawn "spawn-1")
                        $"at RCL{level} the loss is the spawn trunk's alone, as it is from 6,18"

                    Expect.isNonEmpty
                        (unroutedAt level)
                        $"at RCL{level} the reservation seals the doorstep, and the room routes out of it nowhere"

                // And what it costs in pavement: the plan is the sealed one
                // from the start.
                let pavedAt level =
                    decide (colonyOf loaded level) Map.empty Set.empty None
                    |> fun decision ->
                        placementsOf decision.Intents |> tilesOfKind Road |> Set.ofList

                for level in 3..8 do
                    Expect.equal
                        (Set.count (pavedAt level))
                        35
                        $"RCL{level} plans the sealed room's 35 tiles: the 95 is never laid"

                Expect.isEmpty
                    (Set.difference (pavedAt 4) (pavedAt 5))
                    "and the RCL4 → RCL5 level-up that orphaned 60 tiles orphans none"
            }

            test
                "W15S28 falls back to a paved buffer, and records the Link footing it trades (#331)" {
                // #104's mechanism, reached by the live spawn in W15S28: the
                // strict set is empty at both levels, since plain ground lies
                // beside paving but no trunk, while the usable swamp is paved.
                let capture = load "W15S28"
                let loaded = project capture { X = 18; Y = 30 } None
                let controllerId = Option.get loaded.ControllerId

                let planAt level =
                    let colony = colonyOf loaded level
                    let decision = decide colony Map.empty Set.empty None
                    ofView colony, placementsOf decision.Intents, decision.Memo

                let atLevel = [ 5; 6 ] |> List.map (fun level -> level, planAt level)

                for level, (atlas, placed, memo) in atLevel do
                    let area = workArea atlas (Upgrade controllerId) |> RoomPos.inRoom "W15S28"

                    // A swept colony starts with no road standing and none
                    // pending, so this tick's road sites are the whole plan;
                    // the trunks are what is left once the Work Area's own
                    // swamps are taken out.
                    let roadPlan = tilesOfKind Road placed |> Set.ofList
                    let swamps = area |> Set.filter (isSwampIn atlas "W15S28")
                    let trunks = Set.difference roadPlan swamps

                    let afterRoads =
                        colonyOf loaded level
                        |> withRoadsStanding "W15S28" roadPlan
                        |> fun colony -> decide colony Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    let besideATrunk tile =
                        trunks |> Set.exists (fun t -> range tile t = 1)

                    // The premise, not the expectation: this is a pocket, and
                    // the capture is what says so.
                    Expect.isTrue
                        (Set.count swamps * 2 > Set.count area)
                        $"RCL{level}: the Upgrade Work Area is mostly swamp"

                    // The road site wins the tile on the first tick; once that
                    // road stands the container plan below asks for the buffer.
                    Expect.isEmpty
                        (tilesOfKind Container placed
                         |> List.filter (fun tile -> Set.contains tile area))
                        $"RCL{level}: the fallback container defers to this tick's road site"

                    Expect.equal
                        (tilesOfKind Container afterRoads
                         |> List.filter (fun tile -> Set.contains tile area))
                        [ { X = 23; Y = 29 } ]
                        $"RCL{level}: once the road stands the fallback buffer is requested"

                    Expect.equal
                        (memo.UnservedFootings
                         |> List.filter (fun footing ->
                             footing.Kind = FootingKind.ControllerContainer)
                         |> List.map (fun footing -> RoomPos.pos footing.Target))
                        [ { X = 23; Y = 29 } ]
                        $"RCL{level}: the fallback target records the Link footing it cannot reserve"

                    // The strict set is empty: an area tile off the whole
                    // road plan and beside a trunk.
                    Expect.isEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile roadPlan) && besideATrunk tile))
                        $"RCL{level}: the rule's candidate set is empty"

                    // The fallback set differs only by allowing paved swamp.
                    Expect.isNonEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile trunks) && besideATrunk tile))
                        $"RCL{level}: paved swamps beside the trunk, refused for being paved"

                    Expect.isNonEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile roadPlan)
                             && roadPlan |> Set.exists (fun t -> range tile t = 1)))
                        $"RCL{level}: plain tiles beside the paving, refused for being off the trunk"

                // The decision is independent of RCL: the footings and road
                // reservation are identical at the standing and target levels.
                let footings (_, (_, _, memo)) = memo.ServedFootings

                Expect.equal
                    (atLevel |> List.map footings |> List.distinct |> List.length)
                    1
                    "the footings are the same at RCL5 and at RCL6"

                let paved (_, (_, placed, _)) = tilesOfKind Road placed |> Set.ofList

                Expect.equal
                    (atLevel |> List.map (paved >> Set.count) |> List.distinct |> List.length)
                    1
                    "and the wider reservation costs the trunks a detour, not a longer road"

                // The general fallback gives every swept spawn a buffer.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W15S28")

                Expect.isNonEmpty cases "W15S28 is swept"

                Expect.isEmpty
                    (cases
                     |> List.filter (fun case -> List.length case.Containers = case.SourceCount)
                     |> List.map (fun case -> case.Spawn))
                    "the paved fallback gives every swept spawn its controller buffer"
            }

            test "a level-up abandons no paved road, and moves no container's pick (#344)" {
                // Stated as a rule and not as a ratchet: a bound of "no more
                // than 152 tiles" is green on a change that churns 151.
                // Measured on real terrain at the levels the live colonies
                // stand at, because the openRoom ladder in
                // `LayoutPlacementTests` runs on featureless ground where a
                // trunk barely exists.
                let plannedFrom roomName spawn level =
                    let room = rooms |> List.find (fun room -> room.Name = roomName)
                    let loaded = project (load roomName) spawn room.FallbackController
                    loaded, decide (colonyOf loaded level) Map.empty Set.empty None

                let paved placed = tilesOfKind Road placed |> Set.ofList

                // On the first tick a source container defers to the road
                // site it shares ground with, so the picks are read off each
                // plan with its own roads already standing.
                let picksOf roomName colony placed =
                    decide
                        (colony |> withRoadsStanding roomName (paved placed))
                        Map.empty
                        Set.empty
                        None
                    |> fun decision ->
                        placementsOf decision.Intents |> tilesOfKind Container |> Set.ofList

                let levelUp roomName spawn level =
                    let loaded, before = plannedFrom roomName spawn level
                    let placed = placementsOf before.Intents
                    let levelled = colonyOf loaded (level + 1) |> withBuilt (standingCluster placed)
                    let after = decide levelled Map.empty Set.empty None
                    let placedAfter = placementsOf after.Intents

                    (placed, picksOf roomName (colonyOf loaded level) placed),
                    (placedAfter, picksOf roomName levelled placedAfter)

                // W13S28 from `36,42`, RCL6 to RCL7: the largest churn a
                // level-dependent reservation produced in a room the colony
                // owns. `36,42` is a stride tile of the sweep and not the
                // spawn W13S28 stands on, so no claim about the live colony
                // rests on this number (#345).
                let (before, picksBefore), (after, picksAfter) =
                    levelUp "W13S28" { X = 36; Y = 42 } 6

                Expect.equal
                    (Set.count (paved before))
                    92
                    "RCL6 paves from 36,42 the 92 tiles RCL7 wants, not the 111 it used to"

                Expect.equal
                    (paved before)
                    (paved after)
                    "and the levelled room plans the same set: not a superset, the same tiles"

                Expect.equal
                    picksBefore
                    picksAfter
                    "no source container's pick moves out from under the container that stands on it"

                // W15S28 from `18,30`, the live spawn: the colony's own next
                // level-up.
                let (before, _), (after, _) = levelUp "W15S28" { X = 18; Y = 30 } 5

                Expect.equal
                    (paved before)
                    (paved after)
                    "W15S28's own next level-up abandons nothing and lays nothing"

                // Two more transitions the sweep cannot reach: it plans
                // RCL4 → RCL5 alone, and a tower reservation re-coupled to the
                // level churns at RCL6 → 7, where the tower allowance moves.
                // These are the two worst cases that mutation produces —
                // twenty tiles and six, the six carrying a source container's
                // pick off `16,44` with them.
                let (before, picksBefore), (after, picksAfter) =
                    levelUp "W15S28" { X = 18; Y = 12 } 6

                Expect.equal
                    (paved before)
                    (paved after)
                    "an RCL6 → 7 level-up abandons nothing either: the tower allowance moves there, the reservation does not"

                Expect.equal picksBefore picksAfter "and no pick moves across it"

                let (before, picksBefore), (after, picksAfter) =
                    levelUp "W12S27" { X = 18; Y = 30 } 6

                Expect.equal
                    (paved before)
                    (paved after)
                    "nor in the room whose corridors are narrow enough to re-route on one reserved tile"

                Expect.equal picksBefore picksAfter "and `16,44` keeps the container standing on it"

                // And the sweep's half, built out at RCL4 and levelled to
                // RCL5: zero, stated as zero.
                Expect.isEmpty
                    (sweep.Value |> List.collect (fun case -> case.LevelUpAbandons))
                    "no spawn of any capture abandons a paved tile when its room levels"

                Expect.isEmpty
                    (violations (fun case -> not (List.isEmpty case.LevelUpAbandons)))
                    "and the claim is the whole sweep's, named case by case when it breaks"
            }
        ]

// ---- the horizon, re-derived on the room that is about to reach it ------

/// ADR-0063, re-derived on W12S28 at RCL7: the live room #341 was found on
/// and the widest placement window the derivation opens anywhere, since
/// `allowanceOf` answers anything above 7 with sixty extensions and six
/// towers. Planned from `12,40`, the tile the live spawn occupies, which is
/// why this list sits outside the sweep: the sweep is the general rule over
/// every spawn, and this is the one room's arithmetic.
[<Tests>]
let horizonTests =
    testList
        "the derived clustered horizon"
        [
            test "W12S28 at RCL7 asks for the ten extensions and the third tower the level unlocks" {
                let loaded = project (load "W12S28") { X = 12; Y = 40 } None

                let planOf (view: ColonyView) =
                    decide view Map.empty Set.empty None
                    |> fun decision -> placementsOf decision.Intents

                // RCL6 under a lookahead of none: forty extensions and two
                // towers, the census #341 measured standing in W12S28 live at
                // t427,931.
                let shipped = planOf (colonyOf loaded 6 |> atLookahead 0)
                let forty = tilesOfKind Extension shipped
                let two = tilesOfKind Tower shipped

                Expect.hasLength forty 40 "RCL6 under no lookahead is the forty the live room built"
                Expect.hasLength two 2 "and the two towers standing beside them"

                // That room, levelled: nothing moves but the controller's
                // level.
                let standing =
                    (forty |> List.map (fun tile -> tile, BuiltKind.Extension))
                    @ (two |> List.map (fun tile -> tile, BuiltKind.Tower))

                let levelled = colonyOf loaded 7 |> withBuilt standing
                let placed = planOf levelled

                Expect.hasLength
                    (tilesOfKind Extension placed)
                    10
                    "the ten RCL7 unlocks, asked for on the tick the level lands"

                Expect.hasLength (tilesOfKind Tower placed) 1 "and the third tower RCL7 unlocks"

                // #341 itself, reproduced by the only setting that can still
                // produce it: a lookahead of −1 is a stale horizon of 6 met by
                // an RCL7 room.
                let stale = planOf (levelled |> atLookahead -1)

                Expect.isEmpty
                    (tilesOfKind Extension stale)
                    "sized a level behind, the room asks for none of the ten — #341, on the room it was found on"

                Expect.isEmpty (tilesOfKind Tower stale) "and none of the third tower either"

                // And nothing standing moves, measured against every kind the
                // Layout places and not the clustered ones alone. A rampart is
                // the one kind that may share a standing structure's tile.
                let standingTiles = standing |> List.map fst |> Set.ofList

                Expect.isEmpty
                    (placed
                     |> List.filter (fun (tile, kind) ->
                         kind <> Rampart && Set.contains tile standingTiles))
                    "no tile a structure already stands on is planned for anything else"
            }

            test "the ten new picks take no working ground and move no trunk" {
                // The widest window the derivation opens: does the overflow
                // tread on a Seat or on the Upgrade Work Area, and does the
                // router pave differently? It grows a ring further out — row
                // 33 and column 18 — and the trunks do not move at all.
                let loaded = project (load "W12S28") { X = 12; Y = 40 } None
                let colony = colonyOf loaded 7
                let atlas = ofView colony

                let planOf (view: ColonyView) =
                    decide view Map.empty Set.empty None
                    |> fun decision -> placementsOf decision.Intents, decision.Memo

                let stalePlaced, staleMemo = planOf (colony |> atLookahead -1)
                let placed, memo = planOf colony

                let added =
                    Set.difference
                        (tilesOfKind Extension placed |> Set.ofList)
                        (tilesOfKind Extension stalePlaced |> Set.ofList)

                Expect.isEmpty
                    (Set.intersect added (workingGroundIn atlas "W12S28"))
                    "no new pick on a Seat or in the Upgrade Work Area"

                Expect.equal
                    (tilesOfKind Road placed |> Set.ofList)
                    (tilesOfKind Road stalePlaced |> Set.ofList)
                    "the trunks pave the same tiles under both horizons — the same set, not the same length"

                Expect.equal
                    memo.ServedFootings
                    staleMemo.ServedFootings
                    "and the Link footings sit on the tiles the narrower horizon gave them"

                Expect.isEmpty
                    memo.UnservedFootings
                    "no footing target goes unserved at the wider horizon"
            }

            // No captured room asks for less than its own level allows, at
            // any level it can stand at: #341 as a property, which an
            // absolute constant could never satisfy.
            for room, spawn in
                [
                    "W12S28", { X = 12; Y = 40 }
                    "W13S28", { X = 16; Y = 12 }
                    "W15S28", { X = 18; Y = 30 }
                ] do
                test $"{room} asks for its whole allowance at every level it can stand at" {
                    let loaded = project (load room) spawn None

                    let placedAt level =
                        decide (colonyOf loaded level) Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    // The engine's own table, written out rather than read
                    // off `allowanceOf`, which is the function under test.
                    for level, extensions, towers in
                        [ 2, 5, 0; 3, 10, 1; 4, 20, 1; 5, 30, 2; 6, 40, 2; 7, 50, 3; 8, 60, 6 ] do
                        let placed = placedAt level

                        Expect.hasLength
                            (tilesOfKind Extension placed)
                            extensions
                            $"RCL{level} allows {extensions} extensions and the room asks for all of them"

                        Expect.hasLength
                            (tilesOfKind Tower placed)
                            towers
                            $"RCL{level} allows {towers} towers and the room asks for all of them"
                }
        ]

// ---- the extractor and its container, on the rooms that hold a deposit ---

/// The extractor on the three rooms that hold a deposit, outside the sweep
/// for the reason the horizon list is. The deposit's tile is the capture's
/// own; the spawn tiles are in no committed artifact — a capture records a
/// room's fixed furniture and never a base — so they are read off the live
/// rooms and are as good as the tick they were read at.
[<Tests>]
let extractorTests =
    testList
        "the extractor at RCL6"
        [
            // Each room beside the tile its live spawn stands on and the
            // deposit's coordinates as the capture records them.
            for room, spawn, deposit in
                [
                    "W12S28", { X = 12; Y = 40 }, { X = 26; Y = 5 }
                    "W13S28", { X = 16; Y = 12 }, { X = 42; Y = 30 }
                    "W15S28", { X = 18; Y = 30 }, { X = 29; Y = 12 }
                ] do
                test $"{room} at RCL6 plans the extractor on its deposit, and at RCL5 plans neither" {
                    let capture = load room
                    let loaded = project capture spawn None

                    let planOf level =
                        decide (colonyOf loaded level) Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    // The capture is the premise, not the expectation: the
                    // room holds exactly one Thorium deposit and it is where
                    // the season mod put it.
                    Expect.equal
                        (capture.Minerals |> List.map snd)
                        [ deposit ]
                        "the capture holds one Thorium deposit, on the tile the live room has it on"

                    let atSix = planOf 6

                    Expect.equal
                        (tilesOfKind Extractor atSix)
                        [ deposit ]
                        "one extractor, on the mineral's own tile"

                    // The container is judged on what a whole room can say
                    // about it: exactly one, on a Seat of the deposit, on a
                    // tile nothing else wants. *Which* Seat is pinned in
                    // `LayoutPlanTests` on a fixture where "nearest the
                    // Storage's trunk" and "nearest any trunk" disagree;
                    // re-deriving it here would pass under either.
                    let atlas = ofView (colonyOf loaded 6)
                    let seats = seatTilesOf atlas (List.exactlyOne loaded.MineralIds)

                    let containers =
                        tilesOfKind Container atSix
                        |> List.filter (fun tile -> Set.contains (RoomPos.at room tile) seats)

                    Expect.hasLength containers 1 "one container for the deposit"

                    Expect.isFalse
                        (List.contains (List.exactlyOne containers) (tilesOfKind Road atSix))
                        "and the plan does not pave the tile it seats it on"

                    // The whole-room invariants at a level the sweep does not
                    // reach: without this the extractor and the mineral
                    // container are in no double-booking check at all.
                    let footprints =
                        placementsOf (decide (colonyOf loaded 6) Map.empty Set.empty None).Intents
                        |> List.filter (fun (_, kind) -> kind <> Rampart)
                        |> List.map fst

                    Expect.equal
                        (List.length (List.distinct footprints))
                        (List.length footprints)
                        "no tile is asked for two structures in one tick, the extractor among them"

                    let unserved =
                        (decide (colonyOf loaded 6) Map.empty Set.empty None).Memo.UnservedFootings

                    let fixture = rooms |> List.find (fun fixture -> fixture.Name = room)

                    if usesPavedBuffer fixture.PavedBuffer spawn then
                        Expect.all
                            unserved
                            (fun footing -> footing.Kind = FootingKind.ControllerContainer)
                            "the live paved buffer trades only its own Link footing"

                        Expect.hasLength
                            unserved
                            1
                            "the accepted paved-buffer shortfall is recorded once"
                    else
                        Expect.isEmpty unserved "every Link footing finds a tile"

                    // The level below, where `CONTROLLER_STRUCTURES.extractor`
                    // is still 0. Not the horizon's business: the deposit sits
                    // on a wall tile off the clustered checkerboard, so there
                    // is nothing to hold open a level early.
                    let atFive = planOf 5

                    Expect.isEmpty
                        (tilesOfKind Extractor atFive)
                        "RCL5 unlocks no extractor, so none is asked for"

                    Expect.isEmpty
                        (tilesOfKind Container atFive
                         |> List.filter (fun tile -> Set.contains (RoomPos.at room tile) seats))
                        "and no container for a deposit no body can dig"
                }

            test "the deposit's ground is off the clustered ordering at every level" {
                // Not gated on the level the extractor is: an extension
                // landing on the one accessible tile at a wall mouth would
                // cost the room its whole deposit, long before RCL6.
                let capture = load "W12S28"
                let loaded = project capture { X = 12; Y = 40 } None

                for level in [ 4; 6 ] do
                    let colony = colonyOf loaded level
                    let atlas = ofView colony

                    let placed =
                        decide colony Map.empty Set.empty None |> fun d -> placementsOf d.Intents

                    let mineralId = List.exactlyOne loaded.MineralIds

                    let ground =
                        seatTilesOf atlas mineralId
                        |> Set.add (
                            RoomPos.at "W12S28" (List.exactlyOne (List.map snd capture.Minerals))
                        )
                        |> Set.map RoomPos.pos

                    Expect.isTrue
                        (Set.isSubset ground (workingGroundIn atlas "W12S28"))
                        $"the deposit and its Seats are working ground at RCL{level}"

                    Expect.isEmpty
                        (Set.intersect ground (clusteredTiles placed))
                        $"and no clustered pick takes one of them at RCL{level}"
            }
        ]

// ---- the Seam bands the captured rooms hold -----------------------------
