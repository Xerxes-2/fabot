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
        SourceTiles: Pos list
        ControllerId: string option
        Atlas: Atlas
        /// The sites the Layout asks for this tick.
        Placed: (Pos * StructureKind) list
        Unserved: UnservedFooting list
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
                                    Roads = tilesOfKind Road placed |> Set.ofList
                                }
                        }

                    yield
                        {
                            Room = room
                            Spawn = spawn
                            SourceCount = List.length loaded.SourceIds
                            SourceTiles = loaded.SourceIds |> List.choose (positionOf atlas)
                            ControllerId = loaded.ControllerId
                            Atlas = atlas
                            Placed = placed
                            Unserved = first.Memo.UnservedFootings
                            Containers =
                                decide withRoads Map.empty Set.empty None
                                |> fun decision ->
                                    placementsOf decision.Intents |> tilesOfKind Container
                            RecallsIdentically =
                                recalled.Intents = first.Intents
                                && recalled.Memo.UnservedFootings = first.Memo.UnservedFootings
                            ClusterAtRcl2 = clusteredTiles (placementsOf early.Intents)
                        }
        ]

/// How a case reads in a failure message: the room and the spawn it was
/// planned from are the whole reproduction.
let private describe (case: Case) =
    $"%s{case.Room.Name} from %d{case.Spawn.X},%d{case.Spawn.Y}"

let private violations pick =
    sweep.Value |> List.filter pick |> List.map describe

/// Whether a case's road plan carries every source both ways a trunk goes
/// (ADR 0011): to the spawn, and to the controller's Upgrade Work Area.
/// The road tiles beside the source must reach each over roads alone.
let private trunksCarryEverySource (case: Case) =
    let roads = tilesOfKind Road case.Placed |> Set.ofList

    let goals =
        case.ControllerId
        |> Option.map (fun id -> workArea case.Atlas (Upgrade id))
        |> Option.toList
        |> List.append [ Set.singleton case.Spawn ]

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

    case.SourceTiles
    |> List.forall (fun source ->
        let seen = reached source

        goals
        |> List.forall (fun goal ->
            seen |> Seq.exists (fun tile -> goal |> Set.exists (fun g -> range tile g <= 1))))

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
            }
        ]
