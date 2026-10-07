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

                // #345: a colony whose own spawn was never swept is a colony
                // whose real plan nobody checked, as W13S28's was until then.
                // A declared colony with no spawn yet has no tile to ask about.
                Expect.equal
                    (colonySpawns
                     |> List.filter (fun (home, tile) ->
                         not (
                             cases
                             |> List.exists (fun case ->
                                 case.Room.Name = home && case.Spawn = tile)
                         ))
                     |> List.map fst)
                    []
                    "every declared colony with a spawn is swept from the tile it stands on"
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
                    (violations (fun case -> not (cutOff case) && miscounts case))
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
                        && not (cutOff case)
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

            test
                "a sealed corridor cuts a source off whole: no container, no trunk to either goal (#345)" {
                // W12S26 from its west channel: the cluster closes the one way
                // out to the room's only source, so the pick that rides the
                // trunk is never placed and both trunks are recorded dropped.
                // Found the first time the room was swept; the colony's own
                // spawn is clean.
                let cutOffCases = sweep.Value |> List.filter cutOff

                Expect.equal
                    (cutOffCases |> List.map describe |> List.sort)
                    (rooms
                     |> List.collect (fun room ->
                         room.SealedCorridors
                         |> List.map (fun tile -> $"%s{room.Name} from %d{tile.X},%d{tile.Y}"))
                     |> List.sort)
                    "every recorded corridor is still in the sweep"

                Expect.all
                    cutOffCases
                    (fun case -> List.length case.Containers < case.SourceCount + 1)
                    "the cut-off source is still planned no container"

                Expect.all
                    cutOffCases
                    (fun case ->
                        case.Sources
                        |> List.exists (fun (source, _) ->
                            [ TrunkGoal.Spawn case.SpawnId; TrunkGoal.UpgradeArea ]
                            |> List.forall (fun goal ->
                                case.Unrouted |> List.contains { Source = source; Goal = goal })))
                    "and the layout record names both of its trunks"
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

            // The lab cluster's home on its real terrain, built out at RCL8
            // as its own plan lays it, and its Storage over the floor: the
            // stamp must fit, one lab at a time, into what the plan leaves.
            test "W13S28 built out at RCL8 finds its three labs a stamp, one site at a time" {
                let loaded = project (load "W13S28") { X = 16; Y = 12 } None
                let bare = colonyOf loaded 8
                let placed = placementsOf (decide bare Map.empty Set.empty None).Intents

                let cluster =
                    (tilesOfKind Terminal placed |> List.map (fun tile -> tile, BuiltKind.Terminal))
                    @ standingCluster placed

                let storage = cluster |> List.findIndex (fun (_, kind) -> kind = BuiltKind.Storage)

                let funded (built: (Pos * BuiltKind) list) =
                    let colony = withBuilt built bare

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.ofList [ $"built-{storage}", 150_000 ]
                            }
                        Labs =
                            Some
                                {
                                    Cluster = Colony.labCluster
                                    Stock = Map.empty
                                }
                    }

                let labSites built =
                    placementsOf (decide (funded built) Map.empty Set.empty None).Intents
                    |> tilesOfKind StructureKind.Lab

                let labs =
                    (([], cluster), [ 1..3 ])
                    ||> List.fold (fun (labs, built) _ ->
                        match labSites built with
                        | [ site ] -> labs @ [ site ], built @ [ site, BuiltKind.Lab ]
                        | sites -> failwith $"one lab site at a time, got %A{sites}")
                    |> fst

                Expect.isSome
                    (Labs.rolesOf (labs |> List.mapi (fun i tile -> $"lab-{i}", tile)))
                    "the three react: an output within reach of both reagent labs"
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

// ---- the outpost roads, on the rooms the haul crosses (#548) ------------

/// One room's layer, changed.
let private inRoom (room: string) (change: RoomLayer -> RoomLayer) (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        (change (SpatialInfo.layerOf colony.Spatial room))
                        colony.Spatial.Rooms
            }
    }

/// A structure of ours standing on a tile of one room.
let private standing
    (room: string)
    (id: string)
    (tile: Pos)
    (kind: BuiltKind)
    (colony: ColonyView)
    =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id (Structure kind) colony.Spatial.TargetKinds
            }
    }
    |> inRoom room (fun layer ->
        { layer with
            TargetPositions = Map.add id tile layer.TargetPositions
        })

/// Roads standing on tiles of one room at `hits` of 5,000, as a human laid
/// them: in the census, on the layer's road set, and repairable.
let private roadsStanding (room: string) (hits: int) (tiles: Pos list) (colony: ColonyView) =
    let ids = tiles |> List.map (fun tile -> $"road-{room}-{tile.X}-{tile.Y}", tile)

    (colony, ids)
    ||> List.fold (fun colony (id, tile) -> standing room id tile BuiltKind.Road colony)
    |> inRoom room (fun layer ->
        { layer with
            Roads = Set.union layer.Roads (Set.ofList tiles)
        })
    |> fun colony ->
        { colony with
            Spatial =
                { colony.Spatial with
                    Hits =
                        (colony.Spatial.Hits, ids)
                        ||> List.fold (fun hitsOf (id, _) ->
                            Map.add id { Hits = hits; HitsMax = 5000 } hitsOf)
                }
        }

/// Every extension, Storage and tower the home's plan asks for, standing: a
/// home past its build phase (#526), where an outpost road's sites go down.
let private homeBuilt (home: string) (colony: ColonyView) =
    decide colony Map.empty Set.empty None
    |> fun decision -> Fixtures.placementIntents decision.Intents
    |> List.filter (fun (room, _, kind) ->
        room = home && List.contains kind [ Extension; Storage; Tower ])
    |> List.indexed
    |> List.fold
        (fun colony (index, (room, tile, kind)) ->
            let built =
                match kind with
                | Extension -> BuiltKind.Extension
                | Storage -> BuiltKind.Storage
                | _ -> BuiltKind.Tower

            standing room $"home-{index}" tile built colony)
        colony

/// A colony at `home` from `spawn` mining `outposts`, each seen and reserved
/// by us, with a container standing on the Seat `planOutpostContainers`
/// picks, and `crossed` rooms carried for their ground alone, as
/// `transiting` leaves a room the colony only crosses, its home past its
/// build phase (`homeBuilt`). The declarations are laid in by
/// `Outpost.place`, the production rule.
let private miningColony
    (home: string)
    (spawn: Pos)
    (outposts: Outpost list)
    (crossed: string list)
    =
    let loaded = project (load home) spawn None

    let spatial =
        (loaded.Spatial, (outposts |> List.map (fun o -> o.RoomName)) @ crossed)
        ||> List.fold (fun spatial room ->
            let capture = load room

            { spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain = capture.Terrain
                        }
                        spatial.Rooms
                Borders = Map.add room capture.Border spatial.Borders
            })
        |> Outpost.place outposts

    let reserved =
        {
            Owner = Ownership.Unowned
            Reservation =
                Some
                    {
                        Holder = ReservationHolder.Ours
                        TicksToEnd = 4000
                        Username = "Xerxes_2"
                    }
            SafeMode = false
            SafeModeCooldownUntil = 0
            Sign = None
        }

    let colony = colonyOf loaded 5

    let colony =
        { colony with
            Spatial = spatial
            RoomControl =
                (colony.RoomControl, outposts)
                ||> List.fold (fun control outpost -> Map.add outpost.RoomName reserved control)
            Sources =
                colony.Sources
                |> Outpost.pooledSources (Outpost.roomsProjected outposts home) outposts
            Crossed = Set.ofList crossed
        }

    // The containers, on the tiles the production rule asks for; a rock two
    // crossings out, which that rule seats against a Seam toward home it
    // does not border, on its lowest Seat.
    let asked =
        decide colony Map.empty Set.empty None
        |> fun decision -> Fixtures.placementIntents decision.Intents
        |> List.filter (fun (room, _, kind) -> room <> home && kind = Container)
        |> List.map (fun (room, tile, _) -> RoomPos.at room tile)

    let atlas = ofView colony

    outposts
    |> List.collect (fun outpost -> outpost.Sources)
    |> List.map (fun (id, rock) ->
        let seats = seatTilesOf atlas id

        asked
        |> List.tryFind (fun tile -> Set.contains tile seats)
        |> Option.defaultWith (fun () -> Set.minElement seats)
        |> fun tile -> rock.Room, RoomPos.pos tile)
    |> List.indexed
    |> List.fold
        (fun colony (index, (room, tile)) ->
            standing room $"container-{index}" tile BuiltKind.Container colony)
        colony
    |> homeBuilt home

/// W12S28 from its first spawn, mining W12S27 one crossing north and W12S26
/// two crossings out through it: the seventh colony's room, worked as an
/// outpost before its claim.
let private twoHops () =
    let w12s27 = Outpost.adr0042 |> List.find (fun o -> o.RoomName = "W12S27")
    miningColony "W12S28" { X = 12; Y = 40 } [ w12s27; Outpost.w12s26 ] []

/// A tick's decision beside the outpost roads' planned tiles, which the memo
/// does not keep: `planLayout` run again over the same view.
type private Planned =
    {
        Decision: Decision
        Tiles: Set<RoomPos>
    }

/// `Entry.decide`, with the planned tiles beside it.
let private decide colony assignments verbose memo =
    let _, _, _, _, _, _, tiles = planLayout colony (ofView colony)

    {
        Decision = Entry.decide colony assignments verbose memo
        Tiles = tiles
    }

let private roadsOf (planned: Planned) = planned.Decision.Memo.OutpostRoads

let private roadIn (room: string) (planned: Planned) =
    roadsOf planned |> List.find (fun road -> road.Room = room)

/// The outpost roads' sites this tick: the road sites on a planned tile, the
/// home's own trunks being the home rule's.
let private roadSites (planned: Planned) =
    Fixtures.placementIntents planned.Decision.Intents
    |> List.filter (fun (room, tile, kind) ->
        kind = Road && Set.contains (RoomPos.at room tile) planned.Tiles)

/// `twoHops`' outpost road sites alone: its road sites less the trunk sites
/// W12S28 places with no outpost at all, which the line shares.
let private twoHopSites (planned: Planned) =
    let trunks =
        miningColony "W12S28" { X = 12; Y = 40 } [] []
        |> fun home -> Entry.decide home Map.empty Set.empty None
        |> fun d -> Fixtures.placementIntents d.Intents

    roadSites planned |> List.filter (fun site -> not (List.contains site trunks))

/// Road sites of ours pending on tiles of any rooms: in the census, on each
/// room's layer, and in `ConstructionSites`.
let private roadSitesPending (tiles: (string * Pos) list) (colony: ColonyView) =
    let ids =
        tiles
        |> List.map (fun (room, tile) -> $"site-{room}-{tile.X}-{tile.Y}", room, tile)

    (colony, ids)
    ||> List.fold (fun colony (id, room, tile) ->
        { colony with
            ConstructionSites =
                colony.ConstructionSites @ [ { Id = id; Left = 300; Begun = false } ]
            Spatial =
                { colony.Spatial with
                    TargetKinds = Map.add id (Site BuiltKind.Road) colony.Spatial.TargetKinds
                }
        }
        |> inRoom room (fun layer ->
            { layer with
                TargetPositions = Map.add id tile layer.TargetPositions
            }))

let private plannedIn (room: string) (planned: Planned) =
    planned.Tiles
    |> Set.filter (fun tile -> tile.Room = room)
    |> Set.map RoomPos.pos

/// Whether `tiles` hold one line from a tile beside `from` to a tile on the
/// room's edge rows: a road a hauler can walk to the Seam.
let private joinsToEdge (from: Pos) (tiles: Set<Pos>) =
    let edge (tile: Pos) =
        tile.X = 1 || tile.X = 48 || tile.Y = 1 || tile.Y = 48

    let rec spread (reached: Set<Pos>) =
        let next =
            tiles
            |> Set.filter (fun tile ->
                not (Set.contains tile reached)
                && reached |> Set.exists (fun r -> range r tile = 1))

        if Set.isEmpty next then
            reached
        else
            spread (Set.union reached next)

    let start = tiles |> Set.filter (fun tile -> range tile from = 1)
    spread start |> Set.exists edge

let private containerIn (room: string) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    layer.TargetPositions
    |> Map.toList
    |> List.pick (fun (id, tile) ->
        match Map.tryFind id colony.Spatial.TargetKinds with
        | Some(Structure BuiltKind.Container) -> Some tile
        | _ -> None)

[<Tests>]
let outpostRoadTests =
    testList
        "outpost roads on real terrain"
        [
            test "an outpost two rooms off gets a road along the haul route" {
                let colony = twoHops ()
                let decision = decide colony Map.empty Set.empty None

                Expect.equal
                    (roadsOf decision |> List.map (fun road -> road.Room, road.State))
                    [ "W12S26", OutpostRoadState.Planned; "W12S27", OutpostRoadState.Planned ]
                    "both reserved outposts' roads pay, in source-id order"

                let far = plannedIn "W12S26" decision
                let through = plannedIn "W12S27" decision

                Expect.isTrue
                    (joinsToEdge (containerIn "W12S26" colony) far)
                    $"W12S26's road is one line from its container to the Seam: {far}"

                Expect.isNonEmpty
                    through
                    "and it crosses W12S27, the room between, which this colony works"

                Expect.isTrue
                    (decision.Tiles
                     |> Set.forall (fun tile ->
                         List.contains tile.Room [ "W12S28"; "W12S27"; "W12S26" ]))
                    "every planned tile lies on the haul's chain"

                // Less the home's own trunk sites, which the line shares.
                let trunks =
                    miningColony "W12S28" { X = 12; Y = 40 } [] []
                    |> fun home -> decide home Map.empty Set.empty None
                    |> fun d -> Fixtures.placementIntents d.Decision.Intents

                let sites =
                    roadSites decision |> List.filter (fun site -> not (List.contains site trunks))

                Expect.equal
                    (List.length sites)
                    Tuning.defaults.OutpostRoadSites
                    "the cap of the outpost roads' sites goes down, and no more"

                // From home outward: the first site is the line's nearest to
                // the drop point.
                Expect.equal
                    (sites |> List.head |> (fun (room, _, _) -> room))
                    "W12S28"
                    "the first site is at home, where the road leaves"
            }

            test
                "W12S26's and W17S25's declared outposts each get one road from home to their container" {
                for home, outposts in
                    [
                        "W12S26", [ Outpost.w13s26; Outpost.w12s25 ]
                        "W17S25", [ Outpost.w17s26; Outpost.w18s25 ]
                    ] do
                    let spawn =
                        colonySpawnIn home
                        |> Option.defaultWith (fun () ->
                            failwith $"{home} has a live spawn on record")

                    let colony = miningColony home spawn outposts []
                    let decision = decide colony Map.empty Set.empty None

                    for outpost in outposts do
                        let room = outpost.RoomName

                        Expect.equal
                            (roadIn room decision).State
                            OutpostRoadState.Planned
                            $"{home}'s road to {room} pays"

                        Expect.isTrue
                            (joinsToEdge (containerIn room colony) (plannedIn room decision))
                            $"{home}'s road in {room} is one line from the container to the Seam"

                        Expect.isNonEmpty
                            (plannedIn home decision)
                            $"and it leaves {home} on a planned line"
            }

            test "a hand road along the line is followed, and paved over by no site" {
                let colony = twoHops ()
                let before = decide colony Map.empty Set.empty None
                let line = plannedIn "W12S26" before

                // A road a human laid beside ours: the line the router would
                // take were our tiles in W12S26 walled off. Real, contiguous,
                // and sharing no tile with the plan's.
                let atlas = ofView colony
                let container = containerIn "W12S26" colony

                let hand =
                    roadAlong
                        atlas
                        (line |> Set.map (RoomPos.at "W12S26"))
                        Set.empty
                        [ RoomPos.at "W12S27" (Set.minElement (plannedIn "W12S27" before)) ]
                        [ "W12S27"; "W12S26" ]
                        (adjacentWalkableIn atlas "W12S26" container
                         |> List.map (RoomPos.at "W12S26")
                         |> Set.ofList)
                    |> Option.map (
                        snd
                        >> List.filter (fun tile -> tile.Room = "W12S26")
                        >> List.map RoomPos.pos
                    )
                    |> Option.defaultValue []

                Expect.isNonEmpty hand "the capture has a second line to the container"

                let paved = colony |> roadsStanding "W12S26" 5000 hand
                let after = decide paved Map.empty Set.empty None
                let followed = plannedIn "W12S26" after
                let off = Set.difference followed (Set.ofList hand)

                Expect.isLessThanOrEqual
                    (Set.count off)
                    (Set.count followed / 5)
                    $"the road follows the hand road: {Set.count off} of {Set.count followed} tiles off it"

                Expect.isEmpty
                    (roadSites after
                     |> List.filter (fun (room, tile, _) ->
                         room = "W12S26" && List.contains tile hand))
                    "no site goes down on a tile a road already stands on"

                Expect.equal
                    (roadIn "W12S26" after).Standing
                    (Set.count (Set.intersect followed (Set.ofList hand)))
                    "the record counts the hand road's tiles the line takes as standing"
            }

            test "a room the colony only crosses is planned, counted and never paved" {
                let colony =
                    miningColony "W12S28" { X = 12; Y = 40 } [ Outpost.w12s26 ] [ "W12S27" ]

                let decision = decide colony Map.empty Set.empty None
                let road = roadIn "W12S26" decision

                Expect.equal road.State OutpostRoadState.Planned "W12S26's road still pays"

                Expect.isGreaterThan
                    road.Skipped
                    0
                    "its stretch through W12S27 is counted as skipped"

                Expect.isEmpty
                    (roadSites decision |> List.filter (fun (room, _, _) -> room = "W12S27"))
                    "and no site goes down in a room whose sites the colony cannot see"
            }

            test
                "an outpost whose road no longer pays gets no new site, and its standing roads keep their Repair" {
                let colony = twoHops ()
                let line = plannedIn "W12S26" (decide colony Map.empty Set.empty None)

                let unpaid =
                    { (colony |> roadsStanding "W12S26" 1000 (Set.toList line |> List.truncate 5)) with
                        Tuning = { Tuning.defaults with RoadPayback = 1 }
                    }

                let decision = decide unpaid Map.empty Set.empty None

                Expect.equal
                    (roadIn "W12S26" decision).State
                    OutpostRoadState.Unpaid
                    "a road that does not repay its build within a tick is not planned"

                Expect.isEmpty
                    (Fixtures.placementIntents decision.Decision.Intents
                     |> List.filter (fun (room, _, kind) -> room = "W12S26" && kind = Road))
                    "nothing new is placed in its room"

                let ids =
                    line
                    |> Set.toList
                    |> List.truncate 5
                    |> List.map (fun tile -> Repair $"road-W12S26-{tile.X}-{tile.Y}")

                Expect.containsAll
                    (Fixtures.planTasksOn unpaid (noThreats ()))
                    ids
                    "the roads already standing are still repaired: payback gates new sites only"
            }

            test "a hand road off the planned line in a judged outpost keeps its Repair" {
                let colony = twoHops ()
                let line = plannedIn "W12S26" (decide colony Map.empty Set.empty None)

                // A tile of ground nowhere near the line: a road the user laid
                // to somewhere the plan does not go.
                let hand =
                    walkableTilesIn (ofView colony) "W12S26"
                    |> Set.filter (fun tile -> line |> Set.forall (fun t -> range t tile > 3))
                    |> Set.minElement

                let paved = colony |> roadsStanding "W12S26" 1000 [ hand ]
                let decision = decide paved Map.empty Set.empty None

                Expect.equal
                    (roadIn "W12S26" decision).State
                    OutpostRoadState.Planned
                    "the outpost's road was judged and planned"

                Expect.contains
                    (Fixtures.planTasksOn paved (noThreats ()))
                    (Repair $"road-W12S26-{hand.X}-{hand.Y}")
                    "the hand road off the line is the user's, and stays repaired"
            }

            test
                "a colony holds three outpost road sites, and places the next only as one completes" {
                let colony = twoHops ()
                let first = twoHopSites (decide colony Map.empty Set.empty None)

                Expect.equal (List.length first) 3 "the premise: three sites, nearest home first"

                let held = first |> List.map (fun (room, tile, _) -> room, tile)
                let pending = colony |> roadSitesPending held

                Expect.isEmpty
                    (twoHopSites (decide pending Map.empty Set.empty None)
                     |> List.filter (fun site -> not (List.contains site first)))
                    "three held, no fourth goes down"

                // The first one built: a road stands where its site was.
                let room, tile = List.head held

                let completed =
                    colony |> roadSitesPending (List.tail held) |> roadsStanding room 5000 [ tile ]

                Expect.equal
                    (twoHopSites (decide completed Map.empty Set.empty None)
                     |> List.filter (fun site -> not (List.contains site first))
                     |> List.length)
                    1
                    "one built, and one more goes down in its place"
            }

            test "no outpost road site goes down while the player's sites stand at the ceiling" {
                // Every colony's sites share the engine's 100, and a level-up asks
                // for a dozen at once.
                let colony = twoHops ()
                let ceiling = Tuning.defaults.OutpostRoadSiteCeiling

                Expect.isEmpty
                    (twoHopSites (
                        decide { colony with PlayerSites = ceiling } Map.empty Set.empty None
                    ))
                    "at the ceiling, none"

                Expect.equal
                    (twoHopSites (
                        decide
                            { colony with
                                PlayerSites = ceiling - 2
                            }
                            Map.empty
                            Set.empty
                            None
                     )
                     |> List.length)
                    2
                    "two below it, two"
            }

            test "while the home builds, the outpost road is planned and no site of it goes down" {
                // #526's build phase: an extension site standing at home holds
                // every new outpost road site back; the plan and its record go
                // on.
                let building =
                    { twoHops () with
                        ConstructionSites =
                            [
                                {
                                    Id = "ext-site"
                                    Left = 3000
                                    Begun = false
                                }
                            ]
                    }
                    |> standing "W12S28" "ext-site" { X = 20; Y = 20 } BuiltKind.Extension
                    |> fun colony ->
                        { colony with
                            Spatial =
                                { colony.Spatial with
                                    TargetKinds =
                                        Map.add
                                            "ext-site"
                                            (Site BuiltKind.Extension)
                                            colony.Spatial.TargetKinds
                                }
                        }

                let decision = decide building Map.empty Set.empty None

                Expect.equal
                    (roadIn "W12S26" decision).State
                    OutpostRoadState.Planned
                    "the road is still planned"

                Expect.isEmpty
                    (roadSites decision |> List.filter (fun (room, _, _) -> room <> "W12S28"))
                    "and no outpost room gets a site"
            }

            test "a room another player reserves is planned through and never paved" {
                // The engine refuses a site under another's reservation (#334).
                let colony = twoHops ()

                let held =
                    { colony with
                        RoomControl =
                            Map.add
                                "W12S27"
                                { (Map.find "W12S27" colony.RoomControl) with
                                    Reservation =
                                        Some
                                            {
                                                Holder = ReservationHolder.Rival
                                                TicksToEnd = 3000
                                                Username = "rival"
                                            }
                                }
                                colony.RoomControl
                    }

                let decision = decide held Map.empty Set.empty None

                Expect.isGreaterThan
                    (roadIn "W12S26" decision).Skipped
                    0
                    "W12S26's stretch through the held room is counted as skipped"

                Expect.isEmpty
                    (roadSites decision |> List.filter (fun (room, _, _) -> room = "W12S27"))
                    "and no site goes down under the rival's reservation"
            }

            test "a lapsed reservation is priced at the rate the reserver row holds" {
                // #515's reading: the reserver restores the rate long before
                // a road repays its build, so a lapse this tick changes no
                // verdict. At 38,000 ticks every unbuilt tile pays at ten
                // (plain repays in 35,700, swamp in 18,300) and none at five
                // (81,100 and 39,000), whatever the line's ground.
                let colony = twoHops ()

                let lapsed =
                    { colony with
                        Tuning =
                            { colony.Tuning with
                                RoadPayback = 38_000
                            }
                        Bank = { colony.Bank with Capacity = 1300 }
                        RoomControl =
                            Map.add
                                "W12S27"
                                { (Map.find "W12S27" colony.RoomControl) with
                                    Reservation = None
                                }
                                colony.RoomControl
                    }

                Expect.equal
                    (roadIn "W12S27" (decide lapsed Map.empty Set.empty None)).State
                    OutpostRoadState.Planned
                    "a lapse the reserver row can restore keeps the road planned"

                let noReserver =
                    { lapsed with
                        Bank = { lapsed.Bank with Capacity = 300 }
                    }

                Expect.equal
                    (roadIn "W12S27" (decide noReserver Map.empty Set.empty None)).State
                    OutpostRoadState.Unpaid
                    "and a bank that cannot buy a reserver prices it at five, where it does not pay"
            }

            test "an undeclared or shut outpost's road is placed no more" {
                // Undeclared: the room leaves the projection with the
                // declaration. Shut: a stood-down room stays in the scan set
                // as a room the colony crosses, its rock cut out. What stops
                // is the plan and its sites; a road standing in a room the
                // colony still works keeps the ordinary repair rules (the
                // unpaid case above), and a room it only crosses carries no
                // hits to repair (`transiting`).
                let w12s27 = Outpost.adr0042 |> List.find (fun o -> o.RoomName = "W12S27")

                for colony in
                    [
                        miningColony "W12S28" { X = 12; Y = 40 } [ w12s27 ] []
                        miningColony "W12S28" { X = 12; Y = 40 } [ w12s27 ] [ "W12S26" ]
                    ] do
                    let decision = decide colony Map.empty Set.empty None

                    Expect.equal
                        (roadsOf decision |> List.map (fun road -> road.Room))
                        [ "W12S27" ]
                        "W12S26 has no road record"

                    Expect.isEmpty (plannedIn "W12S26" decision) "and no planned tile"

                    Expect.isEmpty
                        (roadSites decision |> List.filter (fun (room, _, _) -> room = "W12S26"))
                        "and no site is placed there"
            }
        ]

// ---- the Seam bands the captured rooms hold -----------------------------
