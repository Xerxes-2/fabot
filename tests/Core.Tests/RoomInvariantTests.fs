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
            Exits = 36
        }
        {
            From = "W12S28"
            To = "W13S28"
            Near = fun tile -> tile.X = 0
            Far = fun tile -> tile.X = 49
            Along = fun tile -> tile.Y
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
