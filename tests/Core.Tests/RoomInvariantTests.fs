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
        // The captured room is this colony's own, so it is owned and its
        // sources are priced at the full rate (ADR 0042) — the sweep is
        // over one room and every one of them is a room with a spawn in it.
        RoomControl =
            Map.ofList
                [
                    name,
                    {
                        Owner = Ownership.Ours
                        Reservation = None
                    }
                ]
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        // A captured room holds no invader core: the four fixtures were
        // taken off a sector whose cores stand four rooms away (ADR 0043).
        InvaderCores = []
        Spatial = room.Spatial
        // The sweep is over one owned room, so it declares none: a
        // candidate colony is a declared home nobody owns yet (ADR 0047),
        // and this room has a spawn standing in it.
        ColonyHomes = []
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

/// Wall tiles of a capture, strided exactly as the standing sample is:
/// unreachable goals nobody chose for what they prove.
let private wallSample (capture: RoomCapture) =
    capture.Terrain
    |> Map.toList
    |> List.filter (fun (_, terrain) -> terrain = Wall)
    |> List.mapi (fun index (tile, _) -> index, tile)
    |> List.filter (fun (index, _) -> index % crossRoomStride = 0)
    |> List.map snd

/// The eight tiles around one, unclipped — what a spawner places a
/// finished body on, before the room says which of them is ground.
let private neighbourhood (tile: Pos) =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if dx <> 0 || dy <> 0 then
                    { X = tile.X + dx; Y = tile.Y + dy }
    ]

/// The body every lead below is priced for: one fatigue-generating part
/// over one Move, carrying nothing. `AtlasTests.worker`'s own parts with
/// its own empty store, so the creep standing on the birth tile and the
/// replacement being priced for it have one fatigue factor between them
/// (`emptyFactorOf` of this list is `fatigueFactorOf` of that creep) —
/// which is what lets the two clocks below be compared at all.
let private leadBody = [ Work; Carry; Move ]

/// The Atlas a cross-Seam lead is priced over: two captures' ground and
/// their rings, a spawn structure standing in the near room with every
/// neighbour but one obstructed, and the creep it would replace standing
/// on that one. Everything geometric is the server's; the spawner, the
/// obstacles that fence it and the creep are the test's, and no expected
/// value comes from any of them.
///
/// The fencing is what makes the comparison exact rather than approximate.
/// A lead's near leg floods out of *all* the tiles beside the spawner
/// (ADR 0026), and the Matcher's walk floods out of the one tile its creep
/// stands on; leave the spawner a single free neighbour and the two floods
/// are the same flood, so the lead's join and the Matcher's may be read
/// against each other tile by tile (ADR 0030's one join, two readers).
///
/// The far room takes whatever extra sources and obstacles the caller
/// hands it, which is how the same fencing is played on the other side of
/// the border: a probe source laid beside one goal tile with its every
/// other neighbour closed has that tile for its whole Work Area, so the
/// Matcher's minimum is taken over one named tile and the two clocks can
/// be compared *at* it rather than across a cluster (`probeBeside`).
let private leadingAcross
    (near: RoomCapture)
    (far: RoomCapture)
    (spawn: Pos)
    (birth: Pos)
    (probes: (string * Pos) list)
    (fence: Set<Pos>)
    =
    { SpatialInfo.empty with
        RoomName = Some near.RoomName
        Rooms =
            Map.ofList
                [
                    near.RoomName,
                    { RoomLayer.empty with
                        Terrain = near.Terrain
                        TargetPositions = Map.ofList [ "spawn-1", spawn ]
                        CreepPositions = Map.ofList [ "w", birth ]
                        Obstacles =
                            neighbourhood spawn
                            |> List.filter (fun tile -> tile <> birth)
                            |> Set.ofList
                            |> Set.add spawn
                    }
                    far.RoomName,
                    { RoomLayer.empty with
                        Terrain = far.Terrain
                        TargetPositions = Map.ofList (far.Sources @ probes)
                        Obstacles = fence
                    }
                ]
        Borders = Map.ofList [ near.RoomName, near.Border; far.RoomName, far.Border ]
        TargetKinds =
            ("spawn-1", Structure BuiltKind.Spawn)
            :: ((far.Sources @ probes) |> List.map (fun (id, _) -> id, Source))
            |> Map.ofList
    }
    |> AtlasTests.snapshotWith [ AtlasTests.worker "w" ]
    |> ofSnapshot

/// A source laid beside one goal tile, and the obstacles that leave it no
/// other Seat: a Harvest of it has a Work Area of exactly that tile, so
/// `walkTicks` — which minimises over a Work Area — becomes an oracle for
/// one goal rather than for a cluster's cheapest member. The neighbour the
/// source stands on is the first in range in `neighbourhood`'s own order,
/// chosen by nothing about what it proves; the fence is that neighbour's
/// whole neighbourhood but the goal, plus its own tile the way the
/// spawner's is fenced above — a Seat is any ground beside the source and
/// the source's tile is beside itself for nothing, but it is ground and
/// would be a second Seat. A goal the fence happens to seal off answers
/// absent on both clocks and is compared all the same.
let private probeBeside (goal: Pos) : Pos * Set<Pos> =
    let inGround (tile: Pos) =
        tile.X >= 1 && tile.X <= 48 && tile.Y >= 1 && tile.Y <= 48

    let source = neighbourhood goal |> List.filter inGround |> List.head

    source,
    neighbourhood source
    |> List.filter (fun tile -> tile <> goal)
    |> Set.ofList
    |> Set.add source

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

            test "a cross-Seam lead prices every goal tile exactly as the Matcher's walk does" {
                // The pin #169's far-leg memo goes in under: by ADR 0030 the
                // lead's cross-room clock and the Matcher's are two readers
                // of one join, so on real terrain and with one near flood
                // between them they agree tile by tile — whatever the lead
                // computes the answer from. A lead priced per goal tile and
                // a lead read out of a table filled once per census are the
                // same number here, or this test is the alarm.
                //
                // Two readings, because the Matcher only ever answers a
                // minimum over a Work Area. Over a real source's Seats that
                // is a cluster, so the lead's per-tile answers are minimised
                // to meet it; over a probe source fenced down to one Seat
                // (`probeBeside`) it is one named tile, and that is where a
                // seeding that is right at a room's cheapest tile and wrong
                // at a dearer one would show — `leadOf` reads the tile its
                // creep happens to stand on, never the room's minimum.
                // Absence has to line up on both: an area whose every tile
                // is unreachable leads nobody, exactly as it walks nobody
                // (ADR 0004).
                let mutable led = 0
                let mutable pinned = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        for birth in standingSample near do
                            // A neighbour of the birth tile, inside the
                            // grid: which one is nobody's decision worth
                            // making, since the spawner's own tile is fenced
                            // off and never walked.
                            let spawn =
                                if birth.Y < 48 then
                                    { birth with Y = birth.Y + 1 }
                                else
                                    { birth with Y = birth.Y - 1 }

                            let atlas = leadingAcross near far spawn birth [] Set.empty

                            Expect.equal
                                (adjacentWalkable atlas spawn)
                                [ birth ]
                                $"the premise: at {spawn.X},{spawn.Y} a body is born on {birth.X},{birth.Y} and nowhere else"

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId

                                // The body-blind area, because that is the
                                // set the far leg of a cross-room price
                                // floods into: `workAreaFor` hands a creep
                                // only its *own* room's tiles (ADR 0041),
                                // and this light body narrows nothing
                                // anyway (ADR 0020).
                                let perTile =
                                    workArea atlas task
                                    |> Set.toList
                                    |> List.choose (castWalkTicks atlas leadBody spawn into)

                                let leadWalk =
                                    match perTile with
                                    | [] -> None
                                    | ticks ->
                                        led <- led + 1
                                        Some(List.min ticks)

                                Expect.equal
                                    leadWalk
                                    (walkTicks atlas "w" task)
                                    $"{from} -> {into}: the lead out of {spawn.X},{spawn.Y} and the walk from {birth.X},{birth.Y} price {sourceId} alike"

                            // Unreachable, tile by tile, and on the same
                            // Atlas the reachable ones were read off: the far
                            // room's own wall is no ground, and its ring is
                            // no ground either — both absent, never a zero
                            // that would leave a creep counted living for
                            // ever.
                            for wall in wallSample far do
                                Expect.equal
                                    (castWalkTicks atlas leadBody spawn into wall)
                                    None
                                    $"{from} -> {into}: the wall at {wall.X},{wall.Y} leads nobody"

                            for exit in seams atlas into from |> List.map fst do
                                Expect.equal
                                    (castWalkTicks atlas leadBody spawn into exit)
                                    None
                                    $"{from} -> {into}: the exit at {exit.X},{exit.Y} is the ring, and no room's ground"

                        // The per-tile half, one stand per direction because
                        // each probe is an Atlas and the floods on it. The
                        // goals are the far room's own strided sample, so
                        // they are scattered over the whole room rather than
                        // gathered where a source happens to sit.
                        for birth in standingSample near |> List.truncate 1 do
                            let spawn =
                                if birth.Y < 48 then
                                    { birth with Y = birth.Y + 1 }
                                else
                                    { birth with Y = birth.Y - 1 }

                            for goal in standingSample far do
                                let source, fence = probeBeside goal

                                let atlas =
                                    leadingAcross near far spawn birth [ "probe", source ] fence

                                Expect.equal
                                    (workArea atlas (Harvest "probe") |> Set.toList)
                                    [ goal ]
                                    $"the premise: the probe beside {goal.X},{goal.Y} is worked from that tile and no other"

                                let led = castWalkTicks atlas leadBody spawn into goal

                                if Option.isSome led then
                                    pinned <- pinned + 1

                                Expect.equal
                                    led
                                    (walkTicks atlas "w" (Harvest "probe"))
                                    $"{from} -> {into}: the lead out of {spawn.X},{spawn.Y} and the walk from {birth.X},{birth.Y} price the tile {goal.X},{goal.Y} alike"

                Expect.isGreaterThan
                    led
                    0
                    "and the sweep really does lead somebody across: an empty one proves nothing"

                Expect.isGreaterThan
                    pinned
                    0
                    "and some far tile was pinned at a number rather than at absence, or the per-tile half proves nothing either"
            }
        ]

/// The outposts the captures below are read with: ADR 0042's two rooms,
/// laid in beside W12S28 the way they were when the captures were taken.
/// Read off `Outpost.adr0042` and not off the live declaration, which
/// moved when W13S28 stood its own spawn (ADR 0047) — the geometry these
/// tests pin did not. This is the one place the tests join the home to
/// its outposts; the non-emptiness guard beside each loop is what keeps a
/// renamed pair from being checked as an empty list forever.
let private declaredOutposts = Outpost.adr0042

/// The Atlas over the projection of one declared outpost on the tick the
/// colony cannot see it: that room's committed terrain and border ring —
/// the whole of what `Game.map.getRoomTerrain` answers without vision —
/// with the declaration laid in by the production rule that lays it in
/// (`Outpost.place`, ADR 0041). That is the shape the shell hands Core for
/// a room it has never had a creep in (`Snapshot.projectRoom`, then the
/// one splice in `Snapshot.build`), and the Atlas is what prices it
/// (CONTEXT.md keeps the two apart: the projection is the data, the Atlas
/// the query interface over it) — so a declaration that named a tile the
/// room walls, or an id nothing places, is priced here the way the live
/// colony would price it.
///
/// Built through `place` and never by hand (#148): a helper that typed the
/// placement out itself would prove that a source *placed* in the
/// projection can be priced, which nothing ever doubted, and would stay
/// green through the whole of the defect this ticket fixes — that the
/// blind room's furniture never reached the projection at all.
///
/// The home room's name is the one the declaration is written relative to
/// — W12S28 — and it carries no geometry, because what is under test is
/// the outpost's own: every query below is asked of the outpost's layer
/// and would answer the empty set for a room the projection did not carry
/// (ADR 0004, ADR 0041).
let private declaredAtlas (outpost: Outpost) =
    let capture = load outpost.RoomName

    { SpatialInfo.empty with
        RoomName = Some "W12S28"
        Rooms =
            Map.ofList
                [
                    outpost.RoomName,
                    { RoomLayer.empty with
                        Terrain = capture.Terrain
                    }
                ]
        Borders = Map.ofList [ outpost.RoomName, capture.Border ]
    }
    |> Outpost.place [ outpost ]
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
                Expect.isNonEmpty declaredOutposts "a declaration nobody made is nothing to check"

                for outpost in declaredOutposts do
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
                for outpost in declaredOutposts do
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

                    // The area the reserver actually stands on (ADR 0042):
                    // reserveController acts at range 1 and a controller's
                    // own tile is an obstacle, so this is its walkable
                    // neighbours and nothing else — a much narrower set
                    // than the Upgrade area above, and W12S27's is two
                    // tiles of swamp. Named as a property and never as
                    // those tiles (ADR 0036): what must hold is that the
                    // set is non-empty, because an empty one is silent —
                    // the Task stays pooled, `threatened` reads an empty
                    // area as unthreatened, and the reserver matched to it
                    // is rejected as unreachable for its whole life.
                    let reserveArea = workArea atlas (Reserve controllerId)

                    Expect.isNonEmpty
                        reserveArea
                        $"{where}: a controller nobody can stand beside can never be reserved"

                    Expect.all
                        reserveArea
                        (fun tile -> range tile controllerPos = 1 && walkable tile)
                        $"{where}: every Reserve Work Area tile a walkable neighbour of the controller"
            }
        ]

/// The colony ADR 0042 declares, over the committed captures: W12S28
/// projected as the colony's own room with a spawn on the tile the live
/// colony stands on, every declared outpost's ground and border ring laid
/// in beside it, and the declaration's own furniture laid over that the
/// way the shell lays it (`Outpost.place`) — under the engine's ids,
/// because that is the vocabulary the constant is written in. The outpost
/// rocks are pooled the way the shell pools them (`Outpost.pooledSources`),
/// so nothing here hand-writes a source list.
///
/// Both rings, because a Seam joins two rooms and a band is empty unless
/// the projection carries both sides (ADR 0041). A `RoomControl` entry per
/// outpost, held by nobody: that map is one entry per *seen* room, so this
/// is the fixture saying the colony is looking into those rooms this tick
/// — the state a creep sent to an outpost rock puts them in, and the only
/// state the container can be planned in, since the census that defers the
/// plan and the `Game.rooms` lookup that executes it are both paid for by
/// vision (ADR 0004). Held by nobody rather than reserved because the
/// container comes before the reserver: what admits the room to the
/// economy is the container, so nothing here may depend on the room
/// already being in it.
let private declaredColony level =
    let loaded = project (load "W12S28") { X = 12; Y = 40 } None

    let spatial =
        (loaded.Spatial, declaredOutposts)
        ||> List.fold (fun spatial outpost ->
            let capture = load outpost.RoomName

            { spatial with
                Rooms =
                    Map.add
                        outpost.RoomName
                        { RoomLayer.empty with
                            Terrain = capture.Terrain
                        }
                        spatial.Rooms
                Borders = Map.add outpost.RoomName capture.Border spatial.Borders
            })
        |> Outpost.place declaredOutposts

    let colony = colonyOf loaded level

    { colony with
        Spatial = spatial
        RoomControl =
            (colony.RoomControl, declaredOutposts)
            ||> List.fold (fun control outpost ->
                Map.add
                    outpost.RoomName
                    {
                        Owner = Ownership.Unowned
                        Reservation = None
                    }
                    control)
        Sources =
            colony.Sources
            |> Outpost.pooledSources
                (Outpost.roomsProjected declaredOutposts (SpatialInfo.homeName spatial))
                declaredOutposts
    }

[<Tests>]
let outpostContainerTests =
    testList
        "the outpost container on real terrain"
        [
            test "each declared source is planned one container, on the Seat nearest the Seam" {
                // ADR 0042's placement rule, stated as a property and never
                // as a tile: the pick is on that rock's *own* Seats, and no
                // other Seat of that rock walks out to the Seam in fewer
                // ticks. Both halves matter and neither implies the other —
                // the first would hold for a rule that read another room's
                // geometry into this one, the second for a rule that picked
                // any tile at all.
                //
                // Real terrain is the counterexample generator here (ADR
                // 0036): the two captures hold a single-Seat rock (`16,7`),
                // a two-Seat rock split between plain and swamp (`18,4`)
                // and a three-Seat rock of nothing but swamp (`16,45`), and
                // no expected value below comes from any of them.
                let colony = declaredColony 5
                let atlas = ofSnapshot colony
                let home = SpatialInfo.homeName colony.Spatial
                let { Intents = intents } = decide colony Map.empty Set.empty None

                let sites =
                    intents
                    |> List.choose (function
                        | PlaceConstructionSite(room, pos, Container) -> Some(room, pos)
                        | _ -> None)

                let declaredSources =
                    [
                        for outpost in declaredOutposts do
                            for id, pos in outpost.Sources -> outpost.RoomName, id, pos
                    ]

                // Everything below is derived from the declaration, and an
                // empty one would leave this case green having checked
                // nothing — the guard the sweep above this file uses, for
                // the same reason.
                Expect.isNonEmpty declaredSources "a declaration nobody made is nothing to check"

                Expect.hasLength
                    (sites |> List.filter (fun (room, _) -> room <> home))
                    (List.length declaredSources)
                    "one container planned per declared outpost rock, and not one more"

                for room, id, pos in declaredSources do
                    let where = $"{room} source {id}"
                    let seats = seatTilesOf atlas id

                    // Attributed by the geometry a source container *is* —
                    // range 1 of the rock (ADR 0012) — so that standing on
                    // a Seat is something this asserts rather than
                    // something it assumed to find the site.
                    let mine =
                        sites
                        |> List.filter (fun (siteRoom, tile) ->
                            siteRoom = room && range tile pos <= 1)

                    Expect.hasLength mine 1 $"{where}: exactly one container planned for this rock"

                    let _, pick = List.head mine

                    Expect.isTrue
                        (Set.contains pick seats)
                        $"{where}: the pick is one of this rock's own Seats, in its own room"

                    match seamWalkTicks atlas room home pick with
                    | None -> failtest $"{where}: the pick is a tile no walk reaches the Seam from"
                    | Some picked ->
                        for seat in seats do
                            match seamWalkTicks atlas room home seat with
                            | None -> ()
                            | Some other ->
                                Expect.isLessThanOrEqual
                                    picked
                                    other
                                    $"{where}: no Seat of this rock walks out to the Seam in fewer ticks"
            }

            test "the candidate colony's controller is the pool's one Claim, on the real rooms" {
                // ADR 0047's arrangement on the ground the colony actually
                // stands on: W13S28 declared a colony of its own while it
                // is still one of W12S28's outposts. The room is projected
                // because the mother declares it, its controller is in the
                // kind census under the engine's own id, and the tick a
                // human writes the second entry that controller stops being
                // a Reserve and becomes a Claim — while the *other*
                // outpost, W12S27, is untouched beside it.
                //
                // Read off the declaration rather than typed out: the ids
                // are the engine's and are pinned against the captures
                // above, so naming one here would be a second literal
                // agreeing with the first and with nothing the server said.
                let candidate = "W13S28"

                let controllerOf room =
                    declaredOutposts
                    |> List.tryFind (fun outpost -> outpost.RoomName = room)
                    |> Option.map (fun outpost -> fst outpost.Controller)

                let colony = declaredColony 5

                // Sorted, because what is asserted is which Task stands on
                // which controller and never the order a Map's keys came
                // out in.
                let pooled homes =
                    planTasks { colony with ColonyHomes = homes } noThreats
                    |> List.filter (function
                        | Reserve _
                        | Claim _ -> true
                        | _ -> false)
                    |> List.sort

                match controllerOf candidate, controllerOf "W12S27" with
                | Some west, Some north ->
                    Expect.equal
                        (pooled [])
                        (List.sort [ Reserve north; Reserve west ])
                        "undeclared, both outpost controllers are Reserves and neither is claimed"

                    Expect.equal
                        (pooled [ "W12S28"; candidate ])
                        (List.sort [ Reserve north; Claim west ])
                        "declared, the candidate colony's controller is a Claim and the other outpost is unmoved"
                | _ -> failtest "the declaration names a controller for each of its outposts"
            }
        ]

// ---- the flood settled on demand (#174) ---------------------------------

/// The captured room with one creep standing in it: the shape the tick's
/// per-creep flood memo is laid for (ADR 0029), over terrain no hand-built
/// fixture poses — walls, swamp lanes, and pockets nothing reaches. Every
/// case below builds one of these per read order rather than sharing one,
/// because the memo is the thing under test: what a resumable flood
/// answers must not depend on what was asked of it before.
let private standingIn (capture: RoomCapture) (spawn: Pos) (creep: CreepInfo) (stand: Pos) =
    (project capture spawn None).Spatial
    |> AtlasTests.withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ creep.Name, stand ]
        })
    |> AtlasTests.snapshotWith [ creep ]
    |> ofSnapshot

/// The room's own ground, nearest the creep first. This is the order a
/// tick actually reads a flood in — a Task is usually a dozen tiles off —
/// so it is the order that leaves the most of the room unsettled between
/// reads, and the one a resumed flood has the most chance to get wrong.
/// Its reverse settles almost the whole room on the first read, which is
/// the other end of the same promise.
let private nearestFirst (stand: Pos) (capture: RoomCapture) =
    capture.Terrain
    |> Map.toList
    |> List.map fst
    |> List.sortBy (fun tile -> range stand tile, tile.X, tile.Y)

/// A tile no flood can reach, wherever the creep stands: the border ring
/// is not the projection's ground (ADR 0041), so nothing settles it and
/// the read has to answer absent rather than "not yet".
let private offTheGround = { X = 0; Y = 0 }

/// The bodies the cases sweep: one at ADR 0003's fatigue parity, which
/// walks plain ground at a tick a tile, and one below it, which pays two
/// units a plain step and prices swamp far dearer — different step
/// tables, so different heaps, different tie-breaks and a different
/// settle order over the same terrain (ADR 0029).
let private floodBodies =
    [
        "parity", AtlasTests.creepWith "w" 0 [ Carry; Carry; Move; Move ]
        "slow", AtlasTests.worker "w"
    ]

/// The rooms the cases sweep, each with the spawn tile the projection
/// stands its furniture on and the tile the creep stands on: the colony's
/// own room and one of #83's remote targets.
let private floodRooms =
    [
        "W12S28", { X = 12; Y = 40 }, { X = 24; Y = 24 }
        "W13S28", { X = 24; Y = 24 }, { X = 12; Y = 30 }
    ]

/// One tile in thirteen gets a flood of its very own — a read no earlier
/// read can have contaminated. A stride rather than every tile because
/// this is the case that pays for an Atlas per tile, and the passes it
/// sits beside already compare all 2,304 against each other.
let private coldStride = 13

[<Tests>]
let onDemandFloodTests =
    testList
        "atlas flood settled on demand"
        [
            test "a walk read off the resumable memo is the whole flood's, tile for tile" {
                // The oracle, and the only case here that does not compare
                // the memo against itself. `haulRoundTripTicks` runs its own
                // floods and settles them whole (ADR 0012, and #174 left it
                // that way deliberately), while `walkTicks` reads the tick's
                // per-creep memo, which #174 made resumable. Point them at
                // one room, one origin and one goal set and they must
                // answer the same number: same weights, same Walk pricing
                // (ADR 0029), both traffic-blind.
                //
                // The two are made to line up by the fixture and not by a
                // special case: a Carry-less body is priced identically
                // loaded and empty, so the round trip is exactly twice the
                // one-way walk; and a spawn structure is an obstacle, so
                // the Refill Work Area at range 1 is exactly the sink's
                // adjacent walkable tiles. Real terrain is what makes it
                // worth asserting — the walk detours around walls and pays
                // swamp, and a flood stopped a pop too early would answer a
                // route it had not finished finding (ADR 0036).
                let body = [ Work; Move ]

                for roomName, spawn, _ in floodRooms do
                    let capture = load roomName
                    let task = Refill "spawn-1"

                    let stands =
                        capture.Terrain
                        |> Map.toList
                        |> List.choose (fun (tile, terrain) ->
                            if terrain <> Wall && (tile.X + tile.Y) % 7 = 0 then
                                Some tile
                            else
                                None)

                    Expect.isGreaterThan
                        (List.length stands)
                        100
                        $"{roomName}: tiles worth sweeping"

                    let mutable walked = 0

                    for stand in stands do
                        let creep = AtlasTests.creepWith "w" 0 body
                        let atlas = standingIn capture spawn creep stand

                        let onDemand = walkTicks atlas creep.Name task
                        let whole = haulRoundTripTicks atlas body roomName stand roomName spawn

                        match onDemand, whole with
                        | Some one, Some round ->
                            walked <- walked + 1

                            Expect.equal
                                (one * 2)
                                round
                                $"{roomName} from {stand}: the memo's walk is the whole flood's"
                        | None, None -> () // no route: absent from both, as ADR 0004 has it
                        | one, round ->
                            failtest
                                $"{roomName} from {stand}: memo {one} and whole flood {round} disagree on whether there is a walk"

                    Expect.isGreaterThan walked 50 $"{roomName}: walks actually compared"
            }

            test "a tile prices the same whichever order the room is read in" {
                // The memo is shared: every query pricing a creep this tick
                // reads one flood, so what it answers must not depend on
                // what was asked of it before (`Floods`). `travelCostWithin`
                // takes the tiles the caller names, so one call is one
                // tile's distance out of that flood, and a flood pushed out
                // tile by tile, a flood asked from the far end in, and a
                // flood asked for one tile and nothing else must agree
                // everywhere. Dijkstra settles a tile for good when it
                // leaves the heap, so this is a property and not a
                // coincidence.
                for roomName, spawn, stand in floodRooms do
                    let capture = load roomName

                    for bodyName, creep in floodBodies do
                        let where = $"{roomName}/{bodyName}"
                        let tiles = nearestFirst stand capture
                        let atlasOf () = standingIn capture spawn creep stand

                        let priced atlas tile =
                            travelCostWithin atlas creep.Name (Set.singleton tile)

                        Expect.isGreaterThan
                            (List.length tiles)
                            2000
                            $"{where}: a room worth sweeping"

                        let onDemand =
                            let atlas = atlasOf ()
                            tiles |> List.map (priced atlas)

                        let farEndFirst =
                            let atlas = atlasOf ()
                            tiles |> List.rev |> List.map (priced atlas) |> List.rev

                        let cold =
                            tiles
                            |> List.indexed
                            |> List.filter (fun (index, _) -> index % coldStride = 0)
                            |> List.map (fun (_, tile) -> priced (atlasOf ()) tile)

                        // The fourth order, and the one that makes this a
                        // comparison against the whole flood rather than
                        // against another resumable one: a tile nothing
                        // reaches settles the heap to exhaustion, so every
                        // read after it comes off exactly the flood the
                        // pre-#174 code laid in one go. Asking it first is
                        // the ticket's "whole flood, then on demand".
                        let wholeFirst =
                            let atlas = atlasOf ()

                            Expect.isNone
                                (priced atlas offTheGround)
                                $"{where}: the border ring is reached by nothing, so asking drains the flood"

                            tiles |> List.map (priced atlas)

                        Expect.isGreaterThan
                            (onDemand |> List.choose id |> List.length)
                            1500
                            $"{where}: a room the creep can price at all"

                        Expect.equal
                            onDemand
                            wholeFirst
                            $"{where}: a flood pushed out tile by tile prices what one settled whole does"

                        Expect.equal
                            onDemand
                            farEndFirst
                            $"{where}: the read order does not move a single tile's price"

                        Expect.equal
                            cold
                            (onDemand
                             |> List.indexed
                             |> List.filter (fun (index, _) -> index % coldStride = 0)
                             |> List.map snd)
                            $"{where}: a flood asked one tile answers what a flood asked every tile does"
            }

            test "the step toward a tile is the one the whole flood leaves, read early or late" {
                // The other half of a flood is its predecessor chain, and it
                // is read *after* the goal settles: every tile of a cheapest
                // path is strictly cheaper than the goal, so all of them
                // left the heap first. `firstStep` is the reader, and a
                // stale parent would move its answer without moving a single
                // price — a creep walking one way while ranked another
                // (ADR 0008, #142). Both routing pricings are swept: the
                // Resolver compares them and blames the difference on
                // traffic (ADR 0018, ADR 0030).
                for roomName, spawn, stand in floodRooms do
                    let capture = load roomName
                    let loaded = project capture spawn None
                    let task = Harvest(List.head loaded.SourceIds)
                    let creep = AtlasTests.worker "w"

                    let goals =
                        nearestFirst stand capture
                        |> List.indexed
                        |> List.filter (fun (index, _) -> index % 5 = 0)
                        |> List.map snd

                    let atlasOf () = standingIn capture spawn creep stand

                    let steps step =
                        let onDemand =
                            let atlas = atlasOf ()
                            goals |> List.map (step atlas)

                        let farEndFirst =
                            let atlas = atlasOf ()
                            goals |> List.rev |> List.map (step atlas) |> List.rev

                        let cold = goals |> List.map (fun goal -> step (atlasOf ()) goal)

                        // The chain against a flood settled whole, which is
                        // the only order here that is not the memo compared
                        // with itself: a goal nothing reaches drains the
                        // heap through this very reader, so the steps that
                        // follow are walked back down the predecessor grid
                        // the pre-#174 flood left. The drain has to answer
                        // absent, or it has not drained.
                        let wholeFirst =
                            let atlas = atlasOf ()

                            Expect.isNone
                                (step atlas offTheGround)
                                $"{roomName}: no step toward a tile the flood never reaches"

                            goals |> List.map (step atlas)

                        onDemand, farEndFirst, cold, wholeFirst

                    let routes =
                        [
                            "priced",
                            steps (fun atlas goal ->
                                firstStep atlas creep.Name task (Set.singleton goal))
                            "traffic-blind",
                            steps (fun atlas goal ->
                                firstStepIgnoringTraffic atlas creep.Name task (Set.singleton goal))
                        ]

                    for pricing, (onDemand, farEndFirst, cold, wholeFirst) in routes do
                        let where = $"{roomName}/{pricing}"

                        Expect.isGreaterThan
                            (onDemand |> List.choose id |> List.length)
                            100
                            $"{where}: a room the creep can step into at all"

                        Expect.equal
                            onDemand
                            wholeFirst
                            $"{where}: a chain read off a resumable flood is the chain a whole one leaves"

                        Expect.equal
                            onDemand
                            farEndFirst
                            $"{where}: the read order does not move a single step"

                        Expect.equal
                            onDemand
                            cold
                            $"{where}: a chain read off a resumed flood is the chain a fresh one leaves"
            }

            test "the walk resumes across the Tasks that share one flood" {
                // The third pricing (ADR 0029): the clock, whose reader takes
                // a Task rather than a tile, so it is the Tasks of a room
                // that ask one flood for one creep in one tick. Asked in
                // either order, or each on a flood of its own, every Task
                // must answer the same walk — the memo's own promise
                // (`Floods`), which #174 made depend on where the flood
                // stopped.
                for roomName, spawn, stand in floodRooms do
                    let capture = load roomName
                    let loaded = project capture spawn None
                    let creep = AtlasTests.worker "w"

                    let tasks =
                        [
                            for id in loaded.SourceIds -> Harvest id
                            for id in Option.toList loaded.ControllerId -> Upgrade id
                            yield Refill "spawn-1"
                        ]

                    let atlasOf () = standingIn capture spawn creep stand
                    let walked atlas task = walkTicks atlas creep.Name task

                    Expect.isGreaterThan (List.length tasks) 2 $"{roomName}: Tasks worth sweeping"

                    let apiece = tasks |> List.map (fun task -> walked (atlasOf ()) task)

                    // Three lists of absences would agree with each other
                    // too, so the walks have to be walks: a flood that
                    // answered nothing at all would satisfy every equality
                    // below and pin nothing.
                    Expect.isGreaterThan
                        (apiece |> List.choose id |> List.length)
                        2
                        $"{roomName}: Tasks the creep can actually walk to"

                    let inOrder =
                        let atlas = atlasOf ()
                        tasks |> List.map (walked atlas)

                    let reversed =
                        let atlas = atlasOf ()
                        tasks |> List.rev |> List.map (walked atlas) |> List.rev

                    Expect.equal
                        inOrder
                        apiece
                        $"{roomName}: a Task asked second walks what it walks asked alone"

                    Expect.equal
                        reversed
                        apiece
                        $"{roomName}: nor does asking them the other way round"
            }

            test "a tile nothing reaches is unreachable and not merely unsettled" {
                // The trap #174 introduces and no type can catch: `unreached`
                // means "nothing gets here" in a whole flood and "nobody has
                // asked yet" in a resumed one, so a reader that answered off
                // the grid before settling would call a reachable tile
                // unreachable — and an unpriceable Task is a creep that never
                // works (ADR 0004). Every wall of a real room is a case, and
                // so is the ring around it.
                let capture = load "W12S28"
                let creep = AtlasTests.worker "w"
                let stand = { X = 24; Y = 24 }
                let atlas = standingIn capture { X = 12; Y = 40 } creep stand

                let walls =
                    capture.Terrain
                    |> Map.toList
                    |> List.choose (fun (tile, terrain) ->
                        if terrain = Wall then Some tile else None)

                Expect.isNonEmpty walls "a captured room has walls"

                Expect.isNone
                    (travelCostWithin atlas creep.Name (Set.singleton offTheGround))
                    "the border ring is no tile of the projection's ground"

                Expect.isNone
                    (travelCostWithin atlas creep.Name (Set.singleton { X = 60; Y = 3 }))
                    "a tile off the fifty-by-fifty grid is unpriceable, not an exception (ADR 0004)"

                for wall in walls do
                    Expect.isNone
                        (travelCostWithin atlas creep.Name (Set.singleton wall))
                        $"a wall at {wall} is reachable by nothing"

                // After all of that: an absent answer drains the flood, and
                // the reads that follow one must still answer — the failure
                // a shortcut that gave up on absence would hide. The tile
                // the creep stands on is left out of both, because
                // `pricedPathTo` answers that one before it asks the flood
                // anything (`Set.contains pos area`), so a set holding it
                // would be green with no flood at all.
                let elsewhere =
                    nearestFirst stand capture |> List.filter (fun tile -> tile <> stand)

                Expect.equal
                    (travelCostWithin atlas creep.Name (Set.singleton stand))
                    (Some 0)
                    "the creep's own tile prices at nothing, asked before any flood is"

                Expect.isSome
                    (travelCostWithin atlas creep.Name (Set.ofList elsewhere))
                    "and the room around it is still reachable"

                Expect.isGreaterThan
                    (elsewhere
                     |> List.filter (fun tile ->
                         travelCostWithin atlas creep.Name (Set.singleton tile) |> Option.isSome)
                     |> List.length)
                    1500
                    "and so is each of its tiles, asked one at a time after the drain"
            }
        ]

// ---- the band read bounded by the best sum (#176) -----------------------

/// Where a cross-room sweep stands its creep: the near room's own ground,
/// strided as every other cross-room case strides it, plus the crossings
/// themselves. A creep parked on a crossing is where the engine leaves one
/// the tick it steps over (#142, #145), and it is the stand the bound has
/// least room on — the flood is seeded on the ring tile whatever it
/// weighs, so the first approach it settles costs nothing and the frontier
/// it prunes by stays at the bottom of the band.
let private standsAcross (near: RoomCapture) (far: RoomCapture) (from: string) (into: string) =
    let onTheRing =
        seams (acrossFrom near far) from into
        |> List.map fst
        |> List.indexed
        |> List.filter (fun (index, _) -> index % 7 = 0)
        |> List.map snd

    standingSample near @ onTheRing

/// A tile of a capture nothing ever reaches — its first wall, in the
/// loader's own order, chosen for nothing but being a wall. Asking a
/// resumable flood about it drains the flood to exhaustion (#174), which
/// is how the cases below get the *whole* band's reading to compare the
/// bounded one against: a drained flood has an empty heap, so its frontier
/// bounds nothing, every grid read is already final, and the bound admits
/// every crossing in the band exactly as the pre-#176 minimum over all of
/// them did.
let private drainTile (capture: RoomCapture) =
    capture.Terrain
    |> Map.toList
    |> List.find (fun (_, terrain) -> terrain = Wall)
    |> fst

/// The body both readings of the walk are taken over: one
/// fatigue-generating part to one Move (ADR 0003) and no Carry at all.
/// Carry-less is what makes the round trip twice the one-way walk — an
/// empty Carry generates no fatigue, so a body holding one prices its two
/// legs under two factors — and one Work against one Move is under the
/// Work-heavy line, so a Harvest keeps the Work Area a source's own
/// surroundings rather than a Post it has no container for (ADR 0020).
let private haulBody = [ Work; Move ]

/// The two captures again, with the far room's sources standing as
/// obstacles and the creep carrying nothing: the one fixture on which the
/// hauler quota's round trip and the Matcher's walk are the same journey.
/// A source tile nothing may stand on makes the Harvest Work Area exactly
/// the sink's adjacent walkable tiles, and a Carry-less body prices its
/// loaded and its empty leg under one fatigue factor (ADR 0003), so the
/// round trip is twice the one-way walk and nothing else. Everything
/// geometric is still the server's.
let private haulingAcross (near: RoomCapture) (far: RoomCapture) (stand: Pos) =
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
                        Obstacles = far.Sources |> List.map snd |> Set.ofList
                    }
                ]
        Borders = Map.ofList [ near.RoomName, near.Border; far.RoomName, far.Border ]
        TargetKinds = far.Sources |> List.map (fun (id, _) -> id, Source) |> Map.ofList
    }
    |> AtlasTests.snapshotWith [ AtlasTests.creepWith "w" 0 haulBody ]
    |> ofSnapshot

[<Tests>]
let boundedBandTests =
    testList
        "atlas band read bounded by the best sum"
        [
            test "a cross-room price is the one the whole band answers, crossing for crossing" {
                // #176's whole promise. The near leg is now pushed out only
                // as far as the crossing that wins, so the crossings behind
                // the bound are never settled for at all — and the answer
                // has to be the one the band gave when every crossing was.
                //
                // The comparison is against the same memo read after it has
                // been drained, which is the pre-#176 reading exactly: with
                // the heap empty the frontier bounds nothing, every tile
                // holds its final distance, and no crossing is skipped. So
                // one side prunes and the other cannot, over one band, one
                // creep, one body and one room's terrain (ADR 0036).
                //
                // Both routes ride along, because a price and a step that
                // disagree are a creep walked to one crossing and ranked at
                // another (#142): the winning exit comes out of this very
                // minimum, so a wrongly pruned crossing moves the step
                // whether or not it moves the number.
                let mutable priced = 0
                let mutable stepped = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into
                        let drain = Set.singleton (drainTile near)

                        for stand in standsAcross near far from into do
                            let bounded = walkingAcross near far stand
                            let whole = walkingAcross near far stand

                            Expect.isNone
                                (travelCostWithin whole "w" drain)
                                $"{from}: a wall is reached by nothing, so asking drains the flood"

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId
                                let cost = travelCost bounded "w" task

                                Expect.equal
                                    cost
                                    (travelCost whole "w" task)
                                    $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} prices what the whole band prices"

                                if Option.isSome cost then
                                    priced <- priced + 1

                                // The drained side's goals are the drain
                                // itself: the tiles a cross-room Task hands
                                // its mover are empty either way (`workAreaFor`
                                // answers a creep a room away with nothing),
                                // so both sides fall through to the Seam, and
                                // passing the unreachable tile is what settles
                                // the traffic-blind flood before it does.
                                let step = firstStep bounded "w" task (workAreaFor bounded "w" task)

                                Expect.equal
                                    step
                                    (firstStep whole "w" task drain)
                                    $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} steps where the whole band steps"

                                if Option.isSome step then
                                    stepped <- stepped + 1

                                Expect.equal
                                    (firstStepIgnoringTraffic
                                        bounded
                                        "w"
                                        task
                                        (workAreaFor bounded "w" task))
                                    (firstStepIgnoringTraffic whole "w" task drain)
                                    $"{from} -> {into} from {stand.X},{stand.Y}: and so does the traffic-blind route"

                Expect.isGreaterThan priced 20 "the sweep really did price across"
                Expect.isGreaterThan stepped 20 "and really did step across"
            }

            test "the walk across is the one a flood settled whole answers" {
                // The oracle from outside the memo, and the only case here
                // that does not compare a flood against itself. The hauler
                // quota's round trip runs its own floods and settles them
                // whole (ADR 0012), joins the same band by the same rule
                // (`joinedAcross`), and prices in the same whole ticks
                // (ADR 0029) — so over a Carry-less body its two legs are
                // one journey twice and it must answer exactly twice the
                // Matcher's walk, which reads the tick's resumable memo and
                // prunes the band against its best sum.
                //
                // Real terrain is what makes it worth asserting: the walk
                // detours around walls and pays swamp on both sides of the
                // border, and the two rooms' cheapest crossings are not the
                // same tile from every stand (ADR 0036).
                let mutable walked = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        for stand in standsAcross near far from into do
                            let atlas = haulingAcross near far stand

                            for sourceId, source in far.Sources do
                                let one = walkTicks atlas "w" (Harvest sourceId)
                                let round = haulRoundTripTicks atlas haulBody from stand into source

                                match one, round with
                                | Some out, Some trip ->
                                    walked <- walked + 1

                                    Expect.equal
                                        (out * 2)
                                        trip
                                        $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId}'s bounded walk is the whole flood's"
                                | None, None -> () // unreachable from both, as ADR 0004 has it
                                | out, trip ->
                                    failtest
                                        $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} walks {out} on the memo and {trip} on the whole flood"

                Expect.isGreaterThan walked 20 "the sweep really did walk across"
            }
        ]

/// The road level gate on real terrain (#209 amending ADR 0011). Stated
/// pairwise on one fixture and one spawn, a level at a time: what the gate
/// changes is *when* the plan reaches the ground, so nothing but the level
/// may differ between the two runs it is read off. The child colony the
/// gate was written for is W13S28 — the room that placed 64 road sites at
/// RCL1 with 8 energy a tick coming in — and the mother is W12S28, which
/// is above the line and must not move.
///
/// No tile is named: the room says which tiles the trunks want and this
/// says only that the level decides whether they are asked for.
[<Tests>]
let roadLevelTests =
    /// One captured room planned from one fixed spawn at a level, as the
    /// sweep plans it — the same `colonyOf` and the same projection, so the
    /// only thing that varies across a pair below is the controller level.
    ///
    /// The spawn is the tile the room's colony actually stands on where
    /// there is one: `AlsoSweep` carries W12S28's live spawn for exactly
    /// that reason (its own doc above), and the stride's first tile is
    /// (6,6), a corner of the room no colony has ever stood in. A pair
    /// read off `List.head` would be a real pair about an imaginary
    /// mother. A room the capture cannot answer for is planned from the
    /// stride's first tile, which is a premise and not an answer: what the
    /// gate is read off is the level, and the spawn only has to be the
    /// same in both runs.
    let placedAt roomName level =
        let room = rooms |> List.find (fun room -> room.Name = roomName)
        let capture = load roomName

        let spawn =
            match room.AlsoSweep with
            | live :: _ -> live
            | [] -> spawnTiles room capture |> List.head

        let loaded = project capture spawn room.FallbackController

        decide (colonyOf loaded level) Map.empty Set.empty None
        |> fun result -> placementsOf result.Intents

    testList
        "the road level gate on real terrain"
        [
            test "W13S28 places no road site below RCL3 and its whole trunk set at RCL3" {
                let rcl1 = placedAt "W13S28" 1
                let rcl2 = placedAt "W13S28" 2
                let rcl3 = placedAt "W13S28" 3

                Expect.isEmpty
                    (tilesOfKind Road rcl1)
                    "RCL1: the bootstrapping colony pours no income into pavement"

                Expect.isEmpty (tilesOfKind Road rcl2) "RCL2: still under the road level"

                Expect.isNonEmpty
                    (tilesOfKind Road rcl3)
                    "RCL3: `roadLevel` is reached and the whole gap drops at once"
            }

            test "the road gap is all the level withheld, and it drops whole" {
                // The gate is a filter on the placement and never on the
                // plan (ADR 0011's "computed whole"), so the set that
                // arrives at RCL3 is the set the room wanted all along —
                // pinned as level-invariance above the line rather than as
                // a tile list, since the plan is the room's answer and not
                // the test's.
                Expect.equal
                    (tilesOfKind Road (placedAt "W13S28" 4) |> Set.ofList)
                    (tilesOfKind Road (placedAt "W13S28" 3) |> Set.ofList)
                    "nothing above the line moves: RCL4 places what RCL3 places"
            }

            test "a bootstrapping room still gets its containers — they wait on no road" {
                // The tile clause (ADR 0040) defers a container to a road
                // *site* on its tile, and below the gate there is none. A
                // source container is the [[post]] that hires the [[anchor]]
                // whose income the gate exists to protect, so holding it
                // back until RCL3 would spend the gate's own saving.
                let rcl1 = placedAt "W13S28" 1

                Expect.isNonEmpty
                    (tilesOfKind Container rcl1)
                    "the containers are planned at RCL1, with no road owed under them"
            }

            test "W13S28's trunks cross the swamp field instead of looping the west edge" {
                // #211: priced at the walk's swamp 10 the router paved a
                // ~28-tile loop along the room's north and west edges to
                // reach the spawn from the north source; priced as a road
                // (swamp 3) it crosses the swamp field between them. Pinned
                // as "some placed road site stands on swamp, and the whole
                // set is well under the loop's size" rather than as a tile
                // list (`RoomFixtures`: real terrain is a counterexample
                // generator, not a source of expected values). The loop's
                // set was 68 sites; the crossing's is 30.
                let capture = load "W13S28"
                let roads = tilesOfKind Road (placedAt "W13S28" 3)

                Expect.isTrue
                    (roads
                     |> List.exists (fun tile -> Map.tryFind tile capture.Terrain = Some Swamp))
                    "at least one trunk tile is paved over swamp"

                Expect.isLessThan
                    (List.length roads)
                    45
                    "and the trunk set is the crossing's, not the loop's"
            }

            test "the mother is above the line and does not move" {
                // W12S28 from the tile its colony stands on, at the live
                // RCL5: the gate is inert from `roadLevel` up, so the road
                // sites there are the same set RCL3 places. What this pins
                // is the gate's upper edge and not byte-identity with the
                // revision before it — one revision cannot compare itself
                // to another, and a stored tile list is the expected value
                // this suite refuses to hold (`RoomFixtures`: real terrain
                // is a counterexample generator, not a source of expected
                // values). What judges the set itself is the RCL4 sweep's
                // own road invariants, which run over every swept spawn of
                // this room, this tile among them.
                let rcl5 = tilesOfKind Road (placedAt "W12S28" 5) |> Set.ofList

                Expect.isNonEmpty rcl5 "the mother still paves"

                Expect.equal
                    rcl5
                    (tilesOfKind Road (placedAt "W12S28" 3) |> Set.ofList)
                    "and paves exactly what it pays for at every level the gate lets through"
            }

            test "the gate withholds one kind and does not stop the Layout" {
                // What a level gate on roads is not: a Layout that waits for
                // RCL3. Every other kind is judged at RCL2 by its own rule
                // and still reaches the ground — the extensions the level
                // does unlock, the containers no level gates at all, the
                // ramparts from `rampartLevel` up — so the room goes on
                // growing into exactly the spend the gate is protecting.
                let rcl2 = placedAt "W13S28" 2

                Expect.isEmpty (tilesOfKind Road rcl2) "the premise: RCL2 places no road"

                Expect.isNonEmpty
                    (tilesOfKind Extension rcl2)
                    "the extensions the level unlocks still drop"

                Expect.isNonEmpty (tilesOfKind Container rcl2) "and the containers with them"
            }
        ]

/// The creep a Verdict is about (ADR 0009): every arm names one, and the
/// smoke tests below read the whole tick's Verdicts back through this to
/// ask whether a creep was accounted for at all. `Observe` keeps its own
/// copy `private`, which is where the fold reads it; this is the same
/// total function and not a second rule.
let private verdictCreep =
    function
    | Verdict.Matched(creep, _, _)
    | Verdict.Kept(creep, _)
    | Verdict.Released(creep, _, _)
    | Verdict.Unassigned(creep, _)
    | Verdict.Scoring(creep, _)
    | Verdict.Grounded creep
    | Verdict.Yielded(creep, _)
    | Verdict.Rerouted creep -> creep

/// The three rungs of a colony's life the fixture is built at: the child
/// as it was claimed, the child at the level the bootstrap window closes
/// on, and the mother this bot grew up on (ADR 0052). Named once, because
/// both lists below are read against the same three colonies.
let private colonyTiers = [ "W13S28", 1, 300; "W13S28", 3, 800; "W12S28", 5, 1800 ]

/// One tick of `decide` over a whole colony, at three rungs of a colony's
/// life (ADR 0052): the child as it was claimed, the child at the level
/// the bootstrap window closes on, and the mother this bot grew up on.
/// Smoke tests and deliberately not assertions about the decisions: what
/// they pin is that a colony built from real terrain at that level and
/// bank goes through the whole pipeline without throwing, and that no
/// creep of its fleet comes out of it unaccounted for — the property
/// every rule R1 and after rewrites has to keep (ADR 0009: a creep that
/// gets no Verdict is a creep nobody can explain).
[<Tests>]
let colonyTierTests =
    testList
        "a whole colony, one tick of decide"
        [
            for room, level, bank in colonyTiers ->
                test $"{room} at RCL{level} on a {bank} bank" {
                    let snapshot = colonyAt (load room) level bank

                    Expect.isNonEmpty
                        snapshot.Creeps
                        "the premise: a colony with no fleet would make the Verdict check vacuous"

                    let decision = decide snapshot Map.empty Set.empty None
                    let judged = decision.Verdicts |> List.map verdictCreep |> Set.ofList

                    for creep in snapshot.Creeps do
                        Expect.isTrue
                            (Set.contains creep.Name judged)
                            $"{creep.Name} came out of the tick with no Verdict"
                }
        ]

/// Where the fixture's Anchor row stands, which is the one placement in it
/// a later pairwise test cannot check for itself. A work-heavy body
/// harvests from its Post and from nothing else (ADR 0020, ADR 0048), and
/// the Post is the Seat its source container stands on (ADR 0012, ADR
/// 0051) — so an Anchor stationed *beside* its container is a body on a
/// walk, and every quota, cap and Seat rule R1 to R5 reads off this
/// fixture would be read against a fleet that never digs. W13S28's `16,7`
/// is the counterexample that makes this a test rather than a comment: its
/// one Seat is the container's, so "the nearest free tile" is range 2 from
/// the rock and out of Harvest range altogether.
[<Tests>]
let colonyAnchorPostTests =
    testList
        "a whole colony, its Anchors on their Posts"
        [
            for room, level, bank in colonyTiers ->
                test $"{room} at RCL{level}: every Anchor stands on a Post" {
                    let capture = load room
                    let snapshot = colonyAt capture level bank
                    let layer = snapshot.Spatial.Rooms[room]

                    let containerTiles =
                        snapshot.Spatial.TargetKinds
                        |> Map.toList
                        |> List.choose (fun (id, kind) ->
                            match kind with
                            | Structure BuiltKind.Container -> Map.tryFind id layer.TargetPositions
                            | _ -> None)
                        |> Set.ofList

                    let sources = capture.Sources |> List.map snd

                    let anchors =
                        snapshot.Creeps
                        |> List.filter (fun creep -> creep.Name.StartsWith "anchor-")
                        |> List.map (fun creep -> creep.Name, layer.CreepPositions[creep.Name])

                    Expect.hasLength
                        anchors
                        sources.Length
                        "one Anchor per Post, which is one per source here"

                    for name, pos in anchors do
                        Expect.isTrue
                            (Set.contains pos containerTiles)
                            $"{name} at {pos.X},{pos.Y} stands on no container, so it garrisons no Post"

                        Expect.isTrue
                            (sources
                             |> List.exists (fun source ->
                                 max (abs (source.X - pos.X)) (abs (source.Y - pos.Y)) <= 1))
                            $"{name} at {pos.X},{pos.Y} is out of Harvest range of every source"
                }
        ]
