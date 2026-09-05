/// The Layout's whole-room invariants, checked against real room terrain
/// (ADR 0036). Real terrain is a counterexample generator here, never a
/// source of expected values: no test below names a tile the Layout is
/// supposed to pick. The spawn sweeps, because the failure this suite
/// exists for — #77's — is a function of terrain *and* of where the
/// cluster grew from.
module Fabot.Core.Tests.RoomInvariantTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures

/// These pin the *capture* — that the loader reads the committed file as
/// the room the server actually answered with. They name tiles, and the
/// invariants below deliberately do not: a golden tile is unreviewable
/// when it is the Layout's pick and is the whole point when it is the
/// room's own furniture.
[<Tests>]
let loaderTests =
    testList
        "room fixture loader"
        [
            test "the header names the room the capture came from" {
                let room = load "W12S28"

                Expect.equal room.RoomName "W12S28" "the header's room name"
                Expect.equal room.Shard "shardSeason" "the shard it was captured from"
                Expect.isGreaterThan room.Tick 0 "a tick, so the capture can be placed in time"
            }

            test "terrain is trimmed to the window the shell projects" {
                // `Snapshot.terrainOf` projects x,y in 1..48 and leaves the
                // exit rows out — an absent tile is impassable, so no path
                // or Work Area ever uses one. The capture holds all 50 rows
                // verbatim; the trim is the loader's, and it is the one
                // line here that has to agree with the shell.
                let room = load "W12S28"

                Expect.hasLength room.Terrain (48 * 48) "the 48x48 interior, exits excluded"

                Expect.isTrue
                    (room.Terrain
                     |> Map.forall (fun tile _ ->
                         tile.X >= 1 && tile.X <= 48 && tile.Y >= 1 && tile.Y <= 48))
                    "no tile outside the projected window"
            }

            test "the border ring is loaded beside that window, holding the room's exits" {
                // The trim above and this are one split, not two truths: the
                // ring is delivered beside the ground and never inside it
                // (ADR 0041), so the Seam has the engine's own exit terrain
                // while nothing that stands a creep can reach it. The exits
                // are named here, with the furniture, because they are what
                // the server said this room's edges are — the invariants
                // below name no tile.
                let home = load "W12S28"

                Expect.hasLength home.Border (50 * 50 - 48 * 48) "the ring, and only the ring"

                Expect.isTrue
                    (home.Border
                     |> Map.forall (fun tile _ ->
                         tile.X = 0 || tile.X = 49 || tile.Y = 0 || tile.Y = 49))
                    "no tile off the border"

                let exitsAlong on across =
                    home.Border
                    |> Map.toList
                    |> List.filter (fun (tile, terrain) -> on tile && terrain <> Wall)
                    |> List.map (fst >> across)

                Expect.equal
                    (exitsAlong (fun tile -> tile.Y = 0) (fun tile -> tile.X))
                    [ 4..39 ]
                    "the north edge W12S27 is reached across: 36 exits, x 4..39"

                Expect.equal
                    (exitsAlong (fun tile -> tile.X = 0) (fun tile -> tile.Y))
                    [ 22..40 ]
                    "the west edge W13S28 is reached across: 19 exits, y 22..40"
            }

            test "the terrain reads row-major, as the live room's own record shows" {
                // Orientation is not a matter of taste: the encoded string
                // is not symmetric, so reading it transposed would give a
                // different room. #77 recorded that the 16,39 container's
                // footing slipped to "the swamp tile 15,40" while 15,39
                // took an extension — which is what this room is read
                // row-major, and is not what it is read any other way.
                let room = load "W12S28"

                Expect.equal
                    (Map.tryFind { X = 15; Y = 40 } room.Terrain)
                    (Some Swamp)
                    "15,40 is the swamp tile #77 names"

                Expect.equal
                    (Map.tryFind { X = 15; Y = 39 } room.Terrain)
                    (Some Plain)
                    "15,39 is the tile the RCL4 burst's extension took"
            }

            test "the furniture is the room's own, under readable ids" {
                let room = load "W12S28"

                Expect.equal
                    room.Sources
                    [ "src-0", { X = 9; Y = 44 }; "src-1", { X = 17; Y = 40 } ]
                    "both sources, in the capture's order, keyed for a person to read"

                Expect.equal
                    room.Controller
                    (Some("ctrl", { X = 5; Y = 41 }))
                    "the controller the room has"
            }

            test "the engine's own ids ride beside the readable ones" {
                // The decision #124 had to make and this pins: an outpost
                // is declared in the *engine's* ids, because a live
                // projection keys every target by the id the server hands
                // back — `TargetKinds`, `Hits`, `Stores` and
                // `Snapshot.Sources` all do. A declaration written in the
                // readable names above would match nothing online, and
                // would do it in silence: an id the projection does not
                // place is unpriceable geometry, so the outpost would never
                // enter a Task rather than fail (ADR 0004). These are what
                // ADR 0042's declaration of this very room is written from,
                // and what a Snapshot built to meet it has to carry — so
                // they are pinned here, with the furniture, against the
                // committed capture.
                let room = load "W12S27"

                Expect.equal
                    room.RealSources
                    [ "6a8caabadd4872bccd3194a6", { X = 16; Y = 45 } ]
                    "the one source, under the id the server gave it"

                Expect.equal
                    room.RealController
                    (Some("6a8caabadd4872bccd3194a5", { X = 37; Y = 43 }))
                    "and the controller a reserver would hold (ADR 0042)"

                // Read on the multi-source rooms by literal, in capture
                // order, because that is the only reading of order that can
                // fail: `Sources` and `RealSources` are two `List.map`s of
                // one list, so comparing them to each other is `x = x` and
                // a loader that sorted the objects would reorder both
                // together and stay green. The order is a claim — ADR
                // 0042's declaration of W13S28 pairs `…362` with (16,7) and
                // `…361` with (18,4), the reverse of the order its prose
                // reads in — and a reorder here would have a declaration
                // and the Snapshot built beside it name two different
                // rocks.
                Expect.equal
                    (load "W13S28").RealSources
                    [
                        "6a8caaaddd4872bccd319362", { X = 16; Y = 7 }
                        "6a8caaaddd4872bccd319361", { X = 18; Y = 4 }
                    ]
                    "the west outpost's two sources, paired as ADR 0042 declares them"

                let centre = load "W15S25"

                Expect.equal
                    centre.RealSources
                    [
                        "6a8caa95dd4872bccd319003", { X = 16; Y = 14 }
                        "6a8caa95dd4872bccd319002", { X = 32; Y = 10 }
                        "6a8caa95dd4872bccd319004", { X = 45; Y = 42 }
                    ]
                    "and the three-source room's, where the ids do not run in tile order either"

                Expect.equal
                    centre.Sources
                    [
                        "src-0", { X = 16; Y = 14 }
                        "src-1", { X = 32; Y = 10 }
                        "src-2", { X = 45; Y = 42 }
                    ]
                    "the rename renames and does not reorder — src-0 is the first row"

                Expect.isNone
                    centre.RealController
                    "and a room with no controller has no id for one"
            }

            test "a three-source room loads three sources and no controller" {
                // Measured, not assumed: claimable rooms carry one or two
                // sources, and the rooms that carry three — sector centres
                // and Source Keeper rooms — carry no controller at all.
                let room = load "W15S25"

                Expect.hasLength room.Sources 3 "the count ADR 0022's rule is stated over"
                Expect.isNone room.Controller "a sector centre has no controller to own"
            }
        ]

// ---- the rooms the sweep runs on ----------------------------------------

/// One captured room and everything the sweep knows about it beyond the
/// file: the controller a room without one borrows, the spawn tiles worth
/// sweeping that the stride misses, and the losses already found there,
/// each beside the issue that owns it rather than in a table of its own.
type private Room =
    {
        Name: string
        /// A controller for a room the capture found none in. It is a
        /// premise of the fixture, not part of what is tested — the rules
        /// must hold wherever the controller sits — which is why it lives
        /// here and not in the capture, whose whole value is that it says
        /// only what the server said. Without a projected controller
        /// *position* the Layout degenerates: the Upgrade Work Area is
        /// empty, the trunks route to the spawn alone, and the footing
        /// count comes out wrong for reasons that are not the rule's.
        FallbackController: Pos option
        /// Spawn tiles swept on top of the stride's.
        AlsoSweep: Pos list
        /// #104: this room's controller sits in a pocket whose whole
        /// Upgrade Work Area is swamp. Every candidate for the controller
        /// container is paved, so the room plans no buffer and holds one
        /// fewer footing target than sources + 2 — and records neither,
        /// because a target that is never constructed is never unserved.
        PlansControllerContainer: bool
        /// #105: spawn tiles whose doorstep the clustered reservation
        /// seals, so a source's trunk cannot be routed and is dropped
        /// whole. Excluded from the trunk invariant and asserted to be
        /// *still* broken below, so the pin cannot outlive its cause.
        SealedDoorsteps: Pos list
    }

let private noLosses =
    {
        Name = ""
        FallbackController = None
        AlsoSweep = []
        PlansControllerContainer = true
        SealedDoorsteps = []
    }

/// Ours, two ordinary claimable neighbours (#83's remote targets), and a
/// three-source sector centre.
let private rooms =
    [
        // The one room whose plan can be compared against a live colony,
        // so it is swept over the tile that colony actually stands on.
        { noLosses with
            Name = "W12S28"
            AlsoSweep = [ { X = 12; Y = 40 } ]
        }
        { noLosses with
            Name = "W12S27"
            PlansControllerContainer = false
            SealedDoorsteps = [ { X = 6; Y = 18 } ]
        }
        { noLosses with Name = "W13S28" }
        // The plain tile nearest the centroid of its three sources.
        { noLosses with
            Name = "W15S25"
            FallbackController = Some { X = 31; Y = 22 }
        }
    ]

/// One tile in six, on plain ground, clear of the room's furniture. A
/// stride rather than every tile because the suite is a gate and not a
/// batch job; deterministic rather than random because a counterexample
/// nobody can reproduce is a rumour.
let private stride = 6

let private spawnTiles (room: Room) (capture: RoomCapture) =
    let furniture =
        (capture.Sources |> List.map snd)
        @ (capture.Controller |> Option.toList |> List.map snd)

    [
        for KeyValue(tile, terrain) in capture.Terrain do
            if
                terrain = Plain
                && tile.X % stride = 0
                && tile.Y % stride = 0
                && furniture |> List.forall (fun target -> range tile target >= 3)
            then
                yield tile
    ]
    @ room.AlsoSweep
    |> List.distinct
    |> List.sortBy (fun tile -> tile.X, tile.Y)

let private colonyOf (room: LoadedRoom) level =
    let name = room.Spatial.RoomName |> Option.defaultValue ""

    {
        Time = 42
        Spawns =
            [
                {
                    Name = "Spawn1"
                    Id = "spawn-1"
                    RoomName = name
                    IsSpawning = false
                }
            ]
        RoomEnergy = Map.ofList [ name, { Available = 300; Capacity = 300 } ]
        Refillables =
            [
                {
                    Id = "spawn-1"
                    FreeCapacity = 0
                    Kind = BuiltKind.Spawn
                }
            ]
        Sources = room.SourceIds |> List.map (fun id -> { Id = id; TicksToRestock = 0 })
        Controller =
            room.ControllerId
            |> Option.map (fun id ->
                {
                    Id = id
                    Level = level
                    TicksToDowngrade = 20000
                    SafeModeAvailable = 1
                    SafeModeActive = false
                })
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        Spatial = room.Spatial
    }

let private placementsOf intents =
    intents
    |> List.choose (function
        | PlaceConstructionSite(_, pos, kind) -> Some(pos, kind)
        | _ -> None)

let private tilesOfKind kind placed =
    placed |> List.choose (fun (pos, k) -> if k = kind then Some pos else None)

/// The clustered structures of a plan: the Storage, the tower and every
/// extension — the tiles one ordering rule picks (ADR 0011, ADR 0022).
let private clusteredTiles placed =
    tilesOfKind Storage placed
    @ tilesOfKind Tower placed
    @ tilesOfKind Extension placed
    |> Set.ofList

// ---- the sweep ----------------------------------------------------------

/// One room planned from one spawn tile, at the level the whole Layout
/// exists at.
type private Case =
    {
        Room: Room
        Spawn: Pos
        SourceCount: int
        /// The room's sources, each under the id the Layout records a
        /// dropped trunk against beside the tile the roads are checked
        /// from: the two halves of the cross-check below have to be about
        /// the same source, and only the id says so.
        Sources: (string * Pos) list
        /// The id of the spawn the case is planned from, for the same
        /// reason: a trunk goal names its spawn (#107).
        SpawnId: string
        ControllerId: string option
        Atlas: Atlas
        /// The sites the Layout asks for this tick.
        Placed: (Pos * StructureKind) list
        Unserved: UnservedFooting list
        /// The footings the Layout placed, each naming its target, that
        /// target's kind and the tile reserved for it (#106). Read off the
        /// plan this case already computed, like `Unserved`: plans per
        /// case is the expensive axis of this sweep, and an invariant that
        /// re-derived one to see the tiles would double it.
        Served: ServedFooting list
        /// The trunks the Layout could not route (#107), one entry per
        /// (source, goal) the router found no path for. Read off the same
        /// plan for the same reason as the two footing records.
        Unrouted: UnroutedTrunk list
        /// The container sites a tick on, with the road plan standing. A
        /// source container is planned onto the Seat nearest its trunk,
        /// which in practice is the tile the trunk leaves the source by,
        /// and the engine takes one construction site per tile — so on the
        /// first tick a container defers to the road it shares ground
        /// with. The count the rule promises is the one that drops once
        /// the roads are up.
        Containers: Pos list
        /// Whether recalling the plan from its own memo gives back the
        /// plan that was computed (ADR 0017). The memo path is the one
        /// worth testing: that a pure function is pure is not news.
        RecallsIdentically: bool
        /// The clustered tiles the same room plans at RCL2. The
        /// reservation is level-blind by construction — the tower's and
        /// the extensions' slots are both sized at the horizon, not at
        /// today's level — so what this pins is that the *placement*
        /// filter only ever adds.
        ClusterAtRcl2: Set<Pos>
    }

/// Every (room, spawn) the suite sweeps, planned once. The invariants read
/// the same plan: re-deriving it per invariant would pay the tick's
/// dearest step many times over for one answer.
let private sweep =
    lazy
        [
            for room in rooms do
                let capture = load room.Name

                for spawn in spawnTiles room capture do
                    let loaded = project capture spawn room.FallbackController
                    let colony = colonyOf loaded 4
                    let atlas = ofSnapshot colony
                    let first = decide colony Map.empty Set.empty None
                    let placed = placementsOf first.Intents
                    let recalled = decide colony Map.empty Set.empty (Some first.Memo)
                    let early = decide (colonyOf loaded 2) Map.empty Set.empty None

                    let withRoads =
                        { colony with
                            Spatial =
                                { colony.Spatial with
                                    // The captured room's own layer, roads
                                    // and all: tiles live under a room name
                                    // and nowhere else (ADR 0041).
                                    Rooms =
                                        Map.add
                                            room.Name
                                            { SpatialInfo.layerOf colony.Spatial room.Name with
                                                Roads = tilesOfKind Road placed |> Set.ofList
                                            }
                                            colony.Spatial.Rooms
                                }
                        }

                    yield
                        {
                            Room = room
                            Spawn = spawn
                            SourceCount = List.length loaded.SourceIds
                            Sources =
                                loaded.SourceIds
                                |> List.choose (fun id ->
                                    positionOf atlas id |> Option.map (fun pos -> id, pos))
                            SpawnId = (List.head colony.Spawns).Id
                            ControllerId = loaded.ControllerId
                            Atlas = atlas
                            Placed = placed
                            Unserved = first.Memo.UnservedFootings
                            Served = first.Memo.ServedFootings
                            Unrouted = first.Memo.UnroutedTrunks
                            Containers =
                                decide withRoads Map.empty Set.empty None
                                |> fun decision ->
                                    placementsOf decision.Intents |> tilesOfKind Container
                            RecallsIdentically =
                                recalled.Intents = first.Intents
                                && recalled.Memo.UnservedFootings = first.Memo.UnservedFootings
                                && recalled.Memo.ServedFootings = first.Memo.ServedFootings
                                && recalled.Memo.UnroutedTrunks = first.Memo.UnroutedTrunks
                            ClusterAtRcl2 = clusteredTiles (placementsOf early.Intents)
                        }
        ]

/// How a case reads in a failure message: the room and the spawn it was
/// planned from are the whole reproduction.
let private describe (case: Case) =
    $"%s{case.Room.Name} from %d{case.Spawn.X},%d{case.Spawn.Y}"

let private violations pick =
    sweep.Value |> List.filter pick |> List.map describe

/// The (source, goal) pairs a case's road plan does *not* carry, derived
/// from the paved tiles alone (ADR 0011): the road tiles beside the source
/// must reach the goal over roads alone, both ways a trunk goes — to the
/// spawn, and to the controller's Upgrade Work Area. A second derivation
/// of the same fact the Layout records for itself (#107), which is the
/// only kind of check worth making against a record: one that agreed with
/// itself would pin nothing (ADR 0035, ADR 0036).
///
/// Independent of the record and *not* of the plan: the roads are read off
/// this tick's sites, which are the road gap and not the road plan (ADR
/// 0010). A swept colony starts with no road standing and no road pending,
/// so the two coincide — the same premise the footing-rule invariant above
/// rests on. A sweep case that started with roads already built would seed
/// this BFS off a hole and call every trunk lost.
///
/// A room the Layout cannot orient itself in plans nothing and loses
/// nothing, so it carries nothing to check — the same gate `planLayout`
/// opens on, stated once more here rather than assumed.
let private unroutedByRoads (case: Case) : UnroutedTrunk list =
    match case.ControllerId with
    | None -> []
    | Some controllerId ->
        let roads = tilesOfKind Road case.Placed |> Set.ofList

        let goals =
            [
                TrunkGoal.UpgradeArea, workArea case.Atlas (Upgrade controllerId)
                TrunkGoal.Spawn case.SpawnId, Set.singleton case.Spawn
            ]

        let reached (source: Pos) =
            let seen = System.Collections.Generic.HashSet<Pos>()
            let queue = System.Collections.Generic.Queue<Pos>()

            for tile in roads |> Set.filter (fun tile -> range tile source = 1) do
                if seen.Add tile then
                    queue.Enqueue tile

            while queue.Count > 0 do
                let tile = queue.Dequeue()

                for dx in -1 .. 1 do
                    for dy in -1 .. 1 do
                        let step = { X = tile.X + dx; Y = tile.Y + dy }

                        if Set.contains step roads && seen.Add step then
                            queue.Enqueue step

            seen

        case.Sources
        |> List.collect (fun (id, source) ->
            let seen = reached source

            goals
            |> List.choose (fun (goal, area) ->
                if
                    seen
                    |> Seq.exists (fun tile -> area |> Set.exists (fun g -> range tile g <= 1))
                then
                    None
                else
                    Some { Source = id; Goal = goal }))

/// Whether a case's road plan carries every source both ways a trunk goes
/// (ADR 0011): to the spawn, and to the controller's Upgrade Work Area.
/// The shape ADR 0036's trunk invariant is stated in, kept beside the
/// pair-wise answer it now reads off rather than flooding a second time.
let private trunksCarryEverySource (case: Case) = List.isEmpty (unroutedByRoads case)

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

            test "every Link footing target the Layout names is served" {
                // The #77 detector, and the invariant ADR 0035 made cheap:
                // an unserved target is a guarantee the colony no longer
                // has. Across every room and every spawn there is not one,
                // so this is the strong form with no ceiling to weaken it.
                Expect.isEmpty
                    (violations (fun case -> not (List.isEmpty case.Unserved)))
                    "a room whose plan is short a footing"
            }

            test "the footing target count is the rule's, never a constant" {
                // The targets are the container picks plus the Storage
                // (ADR 0022, ADR 0027), so sources + 2 is arithmetic the
                // room does rather than a number written down — one
                // container per source, one for the controller, one
                // Storage. A three-source room holds five.
                let miscounts (case: Case) =
                    let containers =
                        if case.Room.PlansControllerContainer then
                            case.SourceCount + 1
                        else
                            case.SourceCount

                    List.length case.Containers <> containers
                    || List.length (tilesOfKind Storage case.Placed) <> 1

                Expect.isEmpty
                    (violations miscounts)
                    "a room whose footing targets its sources do not explain"
            }

            test "every reserved footing is off the trunks, the targets and the others" {
                // ADR 0036's fourth invariant, unassertable when that ADR
                // was written and assertable now that the Layout records
                // the tiles it reserved (#106). The search rule in full:
                // range 1 of its target, off every tile the Layout paves,
                // off the footings' own targets, off every other footing
                // (ADR 0022, ADR 0027). Real terrain is what makes this
                // worth asserting — a footing is chosen from whatever
                // handful of tiles a target's ring leaves, and hand-built
                // fixtures can only pose the collisions their author
                // imagined.
                let breaksTheRule (case: Case) =
                    // The road plan the fold filtered on, read off the
                    // sites: a swept colony starts with no road standing
                    // and no road pending, so the gap the first tick asks
                    // for is the whole plan rather than the remainder of
                    // one.
                    let paved = tilesOfKind Road case.Placed |> Set.ofList

                    let targets =
                        (case.Served |> List.map (fun footing -> footing.Target))
                        @ (case.Unserved |> List.map (fun footing -> footing.Target))
                        |> Set.ofList

                    let tiles = case.Served |> List.map (fun footing -> footing.Tile)

                    List.length (List.distinct tiles) <> List.length tiles
                    || case.Served
                       |> List.exists (fun footing ->
                           range footing.Tile footing.Target <> 1
                           || Set.contains footing.Tile paved
                           || Set.contains footing.Tile targets)

                Expect.isEmpty
                    (violations breaksTheRule)
                    "a room reserving a footing on a road, on a target or on another footing"
            }

            test "served and unserved footings partition the room's targets" {
                // The two records are one record read two ways, so their
                // counts sum to the targets the room actually constructs —
                // one per planned container plus the Storage (ADR 0022,
                // ADR 0027) — and no target is in both. Counted off the
                // room's own arithmetic rather than off sources + 2, which
                // #104's swamp pocket is a standing counterexample to. The
                // containers are counted a tick on, where the road plan no
                // longer defers them, and the Storage off this tick's
                // sites: the two plans name the same tiles, and no
                // container pick is ever a Storage pick — one is working
                // ground and the ordering never offers the other (ADR
                // 0022) — so nothing collapses between the counts.
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
                // ADR 0022: a tower, extension or Storage on a Seat or an
                // Upgrade tile eats a tile an Anchor or an upgrader stands
                // on, and nothing they do is worth that.
                let onWorkingGround (case: Case) =
                    let working = workingGround case.Atlas
                    clusteredTiles case.Placed |> Set.exists (fun tile -> Set.contains tile working)

                Expect.isEmpty
                    (violations onWorkingGround)
                    "a room clustering onto ground the colony works from"
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
                    let walkable = walkableTiles case.Atlas

                    tilesOfKind Road case.Placed
                    |> List.exists (fun tile -> not (Set.contains tile walkable))

                Expect.isEmpty
                    (violations pavesTheImpassable)
                    "a room paving ground nothing can walk"
            }

            test "no tile is asked for two structures in one tick" {
                // Ramparts are excluded by construction: one goes over
                // every Keep structure and every Post a container stands
                // on, so sharing a tile is what a rampart is for (ADR
                // 0034).
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

            test "a level never takes a clustered tile back" {
                // What climbing the ladder changes is only which reserved
                // tiles the placement filter lets through, so nothing a
                // colony was already building should move because it
                // levelled up. The whole ladder — 1 to 2 to 3 to 4 to 5 to
                // 8, 680 level pairs across this sweep — was checked once
                // by hand and holds everywhere; the 2-to-4 pair is the one
                // pinned here, because it is the one that costs a plan.
                let shrinks (case: Case) =
                    not (Set.isSubset case.ClusterAtRcl2 (clusteredTiles case.Placed))

                Expect.isEmpty (violations shrinks) "a room dropping a clustered tile as it levels"
            }

            test "the dropped trunks the Layout records are the ones its roads show" {
                // #107's record, pinned against an independent derivation
                // rather than against itself. The Layout says which
                // (source, goal) pairs it could not route; the road plan
                // says which sources its paved tiles do not carry to which
                // goal, and the two must name the same pairs in every case
                // — the sealed doorstep included, which is the whole point
                // of recording the loss instead of dropping it (#105).
                // Compared as sets, so the invariant is about the pairs and
                // not about the order the fold happens to accumulate them.
                let disagrees (case: Case) =
                    Set.ofList (unroutedByRoads case) <> Set.ofList case.Unrouted

                Expect.isEmpty
                    (violations disagrees)
                    "a room whose dropped trunks and whose road plan tell different stories"
            }

            test "a plan recalled from its memo is the plan that was computed" {
                // ADR 0017's guarantee, stated over rooms big enough for it
                // to be worth something: under an unchanged census the memo
                // hands back the same Intents and the same shortfall, tile
                // for tile, rather than a plan that merely resembles them.
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
            test "a controller in an all-swamp pocket gets no container (#104)" {
                // W12S27's controller sits in a pocket whose 7x7 Upgrade
                // Work Area holds no plain tile at all. The controller
                // container must be an Upgrade tile off the road plan, and
                // every swamp in that area is paved, so there is no
                // candidate: the room plans no buffer, holds one fewer
                // footing target than sources + 2, and records neither.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W12S27")

                Expect.isNonEmpty cases "W12S27 is swept"

                Expect.all
                    cases
                    (fun case -> List.length case.Containers = case.SourceCount)
                    "every spawn in W12S27 plans source containers and no controller container"

                Expect.all
                    cases
                    (fun case -> List.isEmpty case.Unserved)
                    "and the loss is invisible: a target that never existed is never unserved"
            }

            test "a sealed spawn doorstep drops a source's trunk whole (#105)" {
                // W12S27 from 6,18: the spawn has exactly two walkable
                // neighbours and the clustered reservation takes one of
                // them, so the source-to-spawn trunk cannot be routed and
                // is dropped in silence. The working-ground exclusion
                // guards Seats and the Upgrade area; nothing guards the
                // spawn's own doorstep.
                let sealed' =
                    sweep.Value
                    |> List.filter (fun case -> List.contains case.Spawn case.Room.SealedDoorsteps)

                Expect.isNonEmpty sealed' "the sealed-doorstep case is still in the sweep"

                Expect.all
                    sealed'
                    (trunksCarryEverySource >> not)
                    "the trunk is still dropped; delete the pin and the exclusion when #105 lands"

                // And the drop is no longer silent (#107). The loss is per
                // (source, goal), which this room is the live counterexample
                // for: the source→spawn trunk is dropped and the
                // source→controller trunk is routed and paved, so the record
                // names the spawn alone. A record keyed on the source would
                // be false here, and one that named both goals would claim a
                // haul the colony does in fact have.
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
        ]

// ---- the Seam bands the captured rooms hold -----------------------------

/// One border two captured rooms share, spelled the way a person reads a
/// map. The pairing is stated here rather than derived, because deriving
/// it is precisely what `seams` does: a test that recomputed the room-name
/// arithmetic would agree with the query however wrong both were.
type private Border =
    {
        /// The room the band is asked from, and the neighbour across it.
        From: string
        To: string
        /// Where each half of a pair has to sit: this room's exit row or
        /// column, and the neighbour's opposite one.
        Near: Pos -> bool
        Far: Pos -> bool
        /// The coordinate the border leaves free — the one an exit and its
        /// landing tile share.
        Along: Pos -> int
        /// What to add to a tile of the neighbour to read it in this
        /// room's own coordinates — a whole room's width or height, in the
        /// direction the neighbour lies. The two rooms' grids are fifty
        /// apart on the world map, which is what makes a Chebyshev
        /// distance across a border a thing that can be measured at all.
        Offset: Pos
        /// How wide the band is. The server's own answer, read off the two
        /// committed captures, and the number ADR 0041 sizes the cross-room
        /// walk on — "a minimum over 36 additions, not 36 floods". Named
        /// here, with the furniture the loader tests name, because the
        /// derivation the test below runs is symmetric: recompute the band
        /// from the same two rings and a neighbour recaptured with a
        /// narrower exit row shrinks both sides at once, silently.
        Exits: int
    }

/// W12S27 is the room across W12S28's north edge and W13S28 the room
/// across its west edge (#83's two remote targets). W15S25 is a sector
/// centre four rooms off and borders neither, which is what makes it the
/// negative case below.
let private borders =
    [
        {
            From = "W12S28"
            To = "W12S27"
            Near = fun tile -> tile.Y = 0
            Far = fun tile -> tile.Y = 49
            Along = fun tile -> tile.X
            Offset = { X = 0; Y = -50 }
            Exits = 36
        }
        {
            From = "W12S28"
            To = "W13S28"
            Near = fun tile -> tile.X = 0
            Far = fun tile -> tile.X = 49
            Along = fun tile -> tile.Y
            Offset = { X = -50; Y = 0 }
            Exits = 19
        }
    ]

/// The Atlas over two captures' border rings and nothing else: the Seam is
/// answered from the border layer alone, so this is its whole input, and
/// every tile in it is the server's own. `Terrain` stays empty — a room
/// with no ground at all still has its exits, which is the separation
/// under test.
let private acrossFrom (near: RoomCapture) (far: RoomCapture) =
    { SpatialInfo.empty with
        Borders = Map.ofList [ near.RoomName, near.Border; far.RoomName, far.Border ]
    }
    |> AtlasTests.snapshotWith []
    |> ofSnapshot

[<Tests>]
let seamTests =
    testList
        "seams on real terrain"
        [
            test "every captured pair of neighbours has a band, on their shared border" {
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let band = seams (acrossFrom near far) near.RoomName far.RoomName
                    let edge = $"{border.From} -> {border.To}"

                    let ground (ring: Map<Pos, Terrain>) tile =
                        match Map.tryFind tile ring with
                        | Some Plain
                        | Some Swamp -> true
                        | Some Wall
                        | None -> false

                    Expect.isNonEmpty band $"{edge}: rooms the engine joins have exits"

                    Expect.all
                        band
                        (fun (here, _) -> border.Near here)
                        $"{edge}: every near tile on this room's own exit row"

                    Expect.all
                        band
                        (fun (_, there) -> border.Far there)
                        $"{edge}: every far tile on the neighbour's opposite row"

                    Expect.all
                        band
                        (fun (here, there) -> border.Along here = border.Along there)
                        $"{edge}: each pair joins one coordinate to itself"

                    Expect.all
                        band
                        (fun (here, there) -> ground near.Border here && ground far.Border there)
                        $"{edge}: no wall on either side of a crossing"

                    Expect.equal
                        band
                        (band |> List.distinct |> List.sortBy (fun (here, _) -> here.X, here.Y))
                        $"{edge}: each pair once, in (X, Y) order"
            }

            test "the bands are as wide as ADR 0041 sizes the cross-room walk on" {
                // 36 north and 19 west are the numbers the cross-room walk
                // is costed against — "a minimum over 36 additions, not 36
                // floods" — so they are asserted of the query, not only of
                // one room's ring. The test below recomputes the band from
                // the same two rings and would follow a narrower recapture
                // of either room down without a word; this is the line that
                // would go red instead.
                for border in borders do
                    let atlas = acrossFrom (load border.From) (load border.To)

                    Expect.hasLength
                        (seams atlas border.From border.To)
                        border.Exits
                        $"{border.From} -> {border.To}: the band the two captures hold"
            }

            test "the band is every exit the two captures agree on, and no other" {
                // The width is pinned above; what is pinned here is which
                // tiles, recomputed off the committed captures rather than
                // written down — the query drops nothing a creep could
                // cross and admits nothing it could not.
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let edge = $"{border.From} -> {border.To}"

                    // A corner is on two borders at once and is a crossing
                    // on neither, so it is not one of the exits the two
                    // rooms agree on. Every capture walls its corners, so
                    // this line changes nothing today; it is here so a
                    // capture that did not would fail the width test above
                    // rather than this one.
                    let corner tile =
                        border.Along tile = 0 || border.Along tile = 49

                    let crossable =
                        near.Border
                        |> Map.toList
                        |> List.filter (fun (tile, terrain) ->
                            border.Near tile
                            && not (corner tile)
                            && terrain <> Wall
                            && far.Border
                               |> Map.exists (fun landing landingTerrain ->
                                   border.Far landing
                                   && border.Along landing = border.Along tile
                                   && landingTerrain <> Wall))
                        |> List.map (fst >> border.Along)

                    Expect.equal
                        (seams (acrossFrom near far) near.RoomName far.RoomName
                         |> List.map (fst >> border.Along))
                        crossable
                        $"{edge}: the band is the exits both rooms hold"
            }

            test "the band reads the same from the neighbour's side, every pair swapped" {
                // Adjacency has no preferred end: the same crossing, asked
                // from the other room, is the same tiles the other way round.
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let atlas = acrossFrom near far

                    Expect.equal
                        (seams atlas far.RoomName near.RoomName)
                        (seams atlas near.RoomName far.RoomName
                         |> List.map (fun (here, there) -> there, here)
                         |> List.sortBy (fun (here, _) -> here.X, here.Y))
                        $"{border.To} -> {border.From}: the same band, swapped"
            }

            test "a room that borders none of them has no band with any of them" {
                // W15S25 is four rooms away, so nothing joins it — and that
                // is an empty answer, never a failure or a block (ADR 0004).
                let stranger = load "W15S25"

                for name in [ "W12S28"; "W12S27"; "W13S28" ] do
                    let other = load name
                    let atlas = acrossFrom stranger other

                    Expect.isEmpty
                        (seams atlas stranger.RoomName other.RoomName)
                        $"W15S25 -> {name}: no shared border, no band"

                    Expect.isEmpty
                        (seams atlas other.RoomName stranger.RoomName)
                        $"{name} -> W15S25: and none the other way"
            }
        ]

/// The Atlas over two captures' ground *and* their rings, with a creep
/// standing in the near room and the far room's own sources placed under
/// the loader's ids: the whole input a cross-room walk reads (ADR 0041).
/// Everything geometric is the server's; the creep and the ids are the
/// test's, and no expected value comes from either room.
let private walkingAcross (near: RoomCapture) (far: RoomCapture) (stand: Pos) =
    { SpatialInfo.empty with
        RoomName = Some near.RoomName
        Rooms =
            Map.ofList
                [
                    near.RoomName,
                    { RoomLayer.empty with
                        Terrain = near.Terrain
                        CreepPositions = Map.ofList [ "w", stand ]
                    }
                    far.RoomName,
                    { RoomLayer.empty with
                        Terrain = far.Terrain
                        TargetPositions = Map.ofList far.Sources
                    }
                ]
        Borders = Map.ofList [ near.RoomName, near.Border; far.RoomName, far.Border ]
        TargetKinds = far.Sources |> List.map (fun (id, _) -> id, Source) |> Map.ofList
    }
    |> AtlasTests.snapshotWith [ AtlasTests.worker "w" ]
    |> ofSnapshot

/// How far apart the cross-room sweep's stands are, and the file's second
/// sampling knob beside `stride` above. Deliberately not that number and
/// deliberately not that mechanism: `stride` picks tiles by coordinate and
/// costs a lookup apiece, while a stand here costs a whole Atlas and the
/// floods a cross-room price runs on it, so this one has to leave a
/// handful of stands per capture rather than a hundred. It strides the
/// room's *passable* tiles in `Pos` order — a position in that list, not a
/// coordinate on the grid — so which tiles come back depends on how much
/// wall precedes them; that is fine for a sweep whose whole point is that
/// no tile was chosen for what it proves, and it is written down here so
/// nobody widens the sweep by editing `stride` and wonders why this one
/// did not move. Deterministic for `stride`'s own reason: a counterexample
/// nobody can reproduce is a rumour.
let private crossRoomStride = 397

/// Standing tiles spread over a capture's own passable ground by
/// `crossRoomStride`, so no tile is chosen for what it proves — the
/// sweep's habit, at the smaller scale a cross-room price wants (one Atlas
/// and its floods per tile).
let private standingSample (capture: RoomCapture) =
    capture.Terrain
    |> Map.toList
    |> List.filter (fun (_, terrain) -> terrain <> Wall)
    |> List.mapi (fun index (tile, _) -> index, tile)
    |> List.filter (fun (index, _) -> index % crossRoomStride = 0)
    |> List.map snd

[<Tests>]
let crossRoomWalkTests =
    testList
        "cross-room walks on real terrain"
        [
            test
                "no cross-room walk undercuts the rooms' own distance, but for the border's free tile" {
                // The lower bound ADR 0041's join has to respect, on real
                // terrain and naming no tile: a creep crosses at most one
                // tile of Chebyshev distance per tick, so a walk cannot come
                // in under the distance between where it starts and the
                // nearest tile it may work from — measured on the world
                // grid, with the neighbour's coordinates shifted a room's
                // width into this room's frame.
                //
                // Less exactly one tile, and the one is the crossing itself:
                // the creep pays for stepping onto the exit tile, and the
                // engine then relocates it onto the landing tile in the
                // neighbouring room at the end of that tick, for no tick at
                // all. That free tile is the whole of the slack, it is the
                // engine's rule and not this join's, and every other tile of
                // the journey still costs a tick at least (ADR 0029).
                let mutable priced = 0

                for border in borders do
                    for from, into, offset in
                        [
                            border.From, border.To, border.Offset
                            border.To,
                            border.From,
                            {
                                X = -border.Offset.X
                                Y = -border.Offset.Y
                            }
                        ] do
                        let near = load from
                        let far = load into

                        let shift (tile: Pos) =
                            {
                                X = tile.X + offset.X
                                Y = tile.Y + offset.Y
                            }

                        for stand in standingSample near do
                            let atlas = walkingAcross near far stand

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId

                                Expect.equal
                                    (Option.isSome (travelCost atlas "w" task))
                                    (Option.isSome (walkTicks atlas "w" task))
                                    $"{from} -> {into} from {stand.X},{stand.Y}: one join answers both prices"

                                match walkTicks atlas "w" task with
                                | None -> ()
                                | Some walk ->
                                    priced <- priced + 1

                                    let apart =
                                        workArea atlas task
                                        |> Set.toList
                                        |> List.map (shift >> range stand)
                                        |> List.min

                                    Expect.isTrue
                                        (walk + 1 >= apart)
                                        $"{from} -> {into}: a walk of {walk} from {stand.X},{stand.Y} to {sourceId}, {apart} tiles off"

                Expect.isGreaterThan
                    priced
                    0
                    "and the rooms the engine joins really do price across: an empty sweep proves nothing"
            }

            test "a crossing that has a price has a step, and the step stays in this room" {
                // #142's invariant, on real terrain and naming no tile. A
                // Task the Matcher can price is a Task the Matcher will
                // hand out, so a price without a step is a creep parked on
                // an assignment for life — which is the defect this test
                // exists to keep out, stated as an equivalence rather than
                // as a route anybody checked by hand.
                //
                // And the step is always one of this room's own tiles: its
                // ground, or an exit of the band toward the target's room.
                // Never the neighbour's, which a bare `Pos` could not tell
                // apart, and never further than one tile away, because a
                // step is a step (ADR 0001, ADR 0041).
                let mutable stepped = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        // The band is read off the two rings and nothing
                        // else (ADR 0041), so the tile this Atlas stands its
                        // creep on is never looked at.
                        let crossings =
                            seams (walkingAcross near far { X = 0; Y = 0 }) from into
                            |> List.map fst
                            |> Set.ofList

                        for stand in standingSample near do
                            let atlas = walkingAcross near far stand

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId
                                let step = firstStep atlas "w" task (workAreaFor atlas "w" task)

                                Expect.equal
                                    (Option.isSome step)
                                    (Option.isSome (travelCost atlas "w" task))
                                    $"{from} -> {into} from {stand.X},{stand.Y}: priced and walkable answer alike for {sourceId}"

                                match step with
                                | None -> ()
                                | Some tile ->
                                    stepped <- stepped + 1

                                    Expect.equal
                                        (range stand tile)
                                        1
                                        $"{from} -> {into}: the step from {stand.X},{stand.Y} to {tile.X},{tile.Y} is one tile"

                                    let onGround =
                                        match Map.tryFind tile near.Terrain with
                                        | Some terrain -> terrain <> Wall
                                        | None -> false

                                    Expect.isTrue
                                        (onGround || Set.contains tile crossings)
                                        $"{from} -> {into}: {tile.X},{tile.Y} is this room's ground or its exit, nothing else"

                Expect.isGreaterThan
                    stepped
                    0
                    "and the sweep really does walk somebody across: an empty one proves nothing"
            }
        ]

/// The Atlas over the projection of one declared outpost, built out of
/// the declaration itself over that room's committed terrain: the room's
/// ground, and its declared sources and controller placed at the declared
/// tiles under the declared ids. That is the shape the shell hands Core
/// for a room it can see (`Snapshot.buildSpatial`), and the Atlas is what
/// prices it (CONTEXT.md keeps the two apart: the projection is the data,
/// the Atlas the query interface over it) — so a declaration that named a
/// tile the room walls, or an id nothing places, is priced here the way
/// the live colony would price it.
///
/// The home room's name is the one the declaration is written relative to
/// — W12S28 — and it carries no geometry, because what is under test is
/// the outpost's own: every query below is asked of the outpost's layer
/// and would answer the empty set for a room the projection did not carry
/// (ADR 0004, ADR 0041).
let private declaredAtlas (outpost: Outpost) =
    let capture = load outpost.RoomName

    let targets =
        [ for id, pos in outpost.Sources -> id, pos, Source ]
        @ [ fst outpost.Controller, snd outpost.Controller, Controller ]

    { SpatialInfo.empty with
        RoomName = Some "W12S28"
        Rooms =
            Map.ofList
                [
                    outpost.RoomName,
                    { RoomLayer.empty with
                        Terrain = capture.Terrain
                        TargetPositions =
                            targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                        Obstacles = Set.singleton (snd outpost.Controller)
                    }
                ]
        Borders = Map.ofList [ outpost.RoomName, capture.Border ]
        TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
    }
    |> AtlasTests.snapshotWith []
    |> ofSnapshot

[<Tests>]
let outpostDeclarationTests =
    testList
        "the declared outposts against their captures"
        [
            test "each declaration names its own capture's furniture, id and tile alike" {
                // ADR 0042 declares W12S27 and W13S28 in the engine's own
                // ids (ADR 0041's decision, pinned in the loader tests
                // above), and this is where the constant and the committed
                // capture are made to agree. Compared against the capture
                // rather than against a literal: two literals of the same
                // ids agree with each other and with nothing the server
                // ever said, and a re-capture that moved a rock would leave
                // both of them green.
                //
                // Order included, and it carries a claim: W13S28's sources
                // are `16,7` then `18,4`, the reverse of ADR 0042's prose,
                // so a declaration written from the prose would pair each
                // id with the other rock. Nothing downstream may read a
                // source by its index — the tile is the identity — and this
                // is the line that says which tile each id is.
                Expect.isNonEmpty Outpost.declared "a declaration nobody made is nothing to check"

                for outpost in Outpost.declared do
                    let capture = load outpost.RoomName

                    Expect.equal
                        outpost.RoomName
                        capture.RoomName
                        "the capture read is the room the declaration names"

                    Expect.equal
                        outpost.Sources
                        capture.RealSources
                        $"{outpost.RoomName}: every source the server answered with, in its order"

                    Expect.equal
                        (Some outpost.Controller)
                        capture.RealController
                        $"{outpost.RoomName}: the controller a reserver would hold (ADR 0042)"
            }

            test "every declared source and controller is geometry the projection can price" {
                // ADR 0042's first acceptance: the three outpost sources
                // and the two outpost controllers are *in* the projection
                // and answerable by the geometry queries — Seats for a
                // source, an Upgrade Work Area for a controller. Both are
                // read in the target's own room off its id (ADR 0041), so
                // an empty answer here would be a declaration the colony
                // can see and never work.
                //
                // Named as properties, never as tiles: a Seat is a
                // walkable neighbour of its source, and a Work Area tile is
                // walkable within the Upgrade range of its controller. The
                // capture supplies the terrain and the test supplies no
                // expected value (ADR 0036).
                for outpost in Outpost.declared do
                    let capture = load outpost.RoomName
                    let atlas = declaredAtlas outpost

                    let walkable tile =
                        match Map.tryFind tile capture.Terrain with
                        | Some terrain -> terrain <> Wall
                        | None -> false

                    for id, pos in outpost.Sources do
                        let seatTiles = seatTilesOf atlas id
                        let where = $"{outpost.RoomName} source {id}"

                        Expect.equal
                            (targetRoom atlas id)
                            (Some outpost.RoomName)
                            $"{where}: filed under its own room, so its Seats are that room's ground"

                        Expect.isNonEmpty
                            seatTiles
                            $"{where}: a source nobody can stand beside is no outpost"

                        Expect.equal
                            (seats atlas id)
                            (Some(Set.count seatTiles))
                            $"{where}: the Seat count is the Seat tiles'"

                        Expect.all
                            seatTiles
                            (fun tile -> range tile pos = 1 && walkable tile)
                            $"{where}: every Seat a walkable neighbour of the rock"

                        // The container is the switch that admits an
                        // outpost into the economy (ADR 0042), and no
                        // container stands in either room yet — so every
                        // one of these sources is unposted, which is
                        // exactly what makes it worth nothing to the
                        // workforce target this ticket narrowed.
                        Expect.isEmpty
                            (postsOf atlas id)
                            $"{where}: no container stands, so the source has no Post"

                    let controllerId, controllerPos = outpost.Controller
                    let area = workArea atlas (Upgrade controllerId)
                    let where = $"{outpost.RoomName} controller {controllerId}"

                    Expect.equal
                        (targetRoom atlas controllerId)
                        (Some outpost.RoomName)
                        $"{where}: filed under its own room"

                    Expect.isNonEmpty
                        area
                        $"{where}: a controller with no ground around it is unreservable"

                    Expect.all
                        area
                        (fun tile -> range tile controllerPos <= 3 && walkable tile)
                        $"{where}: every Work Area tile walkable within the Upgrade range"
            }
        ]
