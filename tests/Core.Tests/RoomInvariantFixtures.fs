/// The sweep itself and the fixtures it runs: the committed room captures,
/// the colonies laid on them, and the readers each invariant is checked
/// through. Real terrain is a counterexample generator, never a source of
/// expected values; the spawn sweeps because #77's failure is a function of
/// terrain *and* of where the cluster grew from.
module Fabot.Core.Tests.RoomInvariantFixtures

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide

/// Which spawns need the paved-swamp fallback rather than the strict
/// controller-buffer pick: a fact the sweep discovered, kept beside the other
/// per-room losses instead of repeated as room names in each invariant that
/// accepts the resulting Link-footing shortfall.
type internal PavedBuffer =
    | Never
    | EverySpawn
    | From of Pos list

let internal usesPavedBuffer fallback spawn =
    match fallback with
    | Never -> false
    | EverySpawn -> true
    | From spawns -> List.contains spawn spawns

/// One captured room and everything the sweep knows about it beyond the
/// file: the controller a room without one borrows, the spawn tiles worth
/// sweeping that the stride misses, and the losses already found there,
/// each beside the issue that owns it rather than in a table of its own.
type internal Room =
    {
        Name: string
        /// A controller for a room the capture found none in: a premise of
        /// the fixture, kept out of the capture, whose value is that it says
        /// only what the server said. Without a projected controller the
        /// Layout degenerates: no Upgrade Work Area, trunks to the spawn
        /// alone, and a footing count wrong for reasons that are not the
        /// rule's.
        FallbackController: Pos option
        /// Spawn tiles swept on top of the stride's.
        AlsoSweep: Pos list
        /// Spawns where only the paved controller-buffer fallback preserves
        /// the room's growth buffer.
        PavedBuffer: PavedBuffer
        /// Spawn tiles whose doorstep the clustered reservation seals, so a
        /// source's trunk is dropped whole (#105). Excluded from the trunk
        /// invariant and asserted to be *still* broken, so the pin cannot
        /// outlive its cause.
        SealedDoorsteps: Pos list
    }

let internal noLosses =
    {
        Name = ""
        FallbackController = None
        AlsoSweep = []
        PavedBuffer = Never
        SealedDoorsteps = []
    }

/// Our three rooms, two ordinary claimable neighbours (#83's remote targets),
/// and a three-source sector centre. The third home joined the sweep with
/// #331, which is how that ticket's loss turned out to be one spawn's and not
/// the room's.
let internal rooms =
    [
        // The one room whose plan can be compared against a live colony,
        // so it is swept over the tile that colony actually stands on.
        { noLosses with
            Name = "W12S28"
            AlsoSweep = [ { X = 12; Y = 40 } ]
        }
        // 32,2 is swept on top of the stride because the reservation seals
        // it: one of three tiles in this room — 31,1 and 33,1 are the others
        // — whose corridor out the clustered window closes. A loss the suite
        // cannot see is a loss nobody reproduces.
        { noLosses with
            Name = "W12S27"
            AlsoSweep = [ { X = 32; Y = 2 } ]
            PavedBuffer = EverySpawn
            SealedDoorsteps = [ { X = 6; Y = 18 }; { X = 32; Y = 2 } ]
        }
        { noLosses with Name = "W13S28" }
        // From the live spawn tile alone its strict buffer set is empty, so
        // the paved-swamp fallback applies and its controller Link footing
        // is the one accepted unserved target in the sweep (#331).
        { noLosses with
            Name = "W15S28"
            AlsoSweep = [ { X = 18; Y = 30 } ]
            PavedBuffer = From [ { X = 18; Y = 30 } ]
        }
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
let internal stride = 6

let internal spawnTiles (room: Room) (capture: RoomCapture) =
    let furniture =
        (capture.Sources |> List.map snd)
        @ (capture.Controller |> Option.toList |> List.map snd)

    [
        for tile, terrain in TerrainGrid.toList capture.Terrain do
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

let internal colonyOf (room: LoadedRoom) level =
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
        Bank = { Available = 300; Capacity = 300 }
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
        // sources are priced at the full rate.
        HeldOutposts = Set.empty
        ThreatenedOutposts = Set.empty
        RoomControl =
            Map.ofList
                [
                    name,
                    {
                        Owner = Ownership.Ours
                        Reservation = None
                        SafeMode = false
                        Sign = None
                    }
                ]
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        // A captured room holds no invader core: the captures were taken
        // off a sector whose cores stand four rooms away.
        InvaderCores = []
        Spatial = room.Spatial
        // One owned room with a spawn in it declares no candidate colony.
        Declared = []
        // The stage is derived from the same level the controller above
        // carries rather than written down beside it, so the two cannot
        // disagree. A room the capture gives no controller has no stage.
        Stages =
            match
                room.ControllerId
                |> Option.bind (fun _ -> Colony.stageOf Tuning.defaults true true (Some level))
            with
            | Some stage -> Map.ofList [ name, stage ]
            | None -> Map.empty
        // One colony over one room: every body is its own, it raises no
        // child, declares no errand, and every tick has vision.
        Foreign = Set.empty
        Borrowed = { Rooms = [] }
        Refused = []
        Errands = []
        Consignee = None
        Crossed = Set.empty
        Reactors = []
        Sightings = Map.empty
        // The tests that are *about* a tunable move the one field they are
        // about.
        Tuning = Tuning.defaults
        Casting = []
    }

let internal placementsOf intents =
    intents
    |> List.choose (function
        | PlaceConstructionSite(tile, kind) -> Some(RoomPos.pos tile, kind)
        | _ -> None)

let internal tilesOfKind kind placed =
    placed |> List.choose (fun (pos, k) -> if k = kind then Some pos else None)

/// The clustered structures of a plan: the Storage, the tower and every
/// extension — the tiles one ordering rule picks.
let internal clusteredTiles placed =
    tilesOfKind Storage placed
    @ tilesOfKind Tower placed
    @ tilesOfKind Extension placed
    |> Set.ofList

/// The same room with a list of clustered structures standing on it, made
/// obstacles the way the shell makes a standing structure one. The tiles are
/// the Layout's own picks rather than a person's, so the fixture stays a
/// counterexample generator. The kind travels with the tile: a model that
/// stood only the extensions would hand the third tower a tile one of the
/// first two is on.
let internal withBuilt (built: (Pos * BuiltKind) list) (colony: ColonyView) =
    { colony with
        Spatial =
            colony.Spatial
            |> Fixtures.withTargets (
                built
                |> List.mapi (fun index (tile, kind) -> $"built-{index}", tile, Structure kind)
            )
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.union layer.Obstacles (built |> List.map fst |> Set.ofList)
                })
    }

/// The same room with a set of roads already paved on its own layer. What
/// reads them is the container rule, which defers a container to a road
/// *site* sharing its tile and so answers differently once the road stands.
let internal withRoadsStanding (name: string) (roads: Set<Pos>) (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        name
                        { SpatialInfo.layerOf colony.Spatial name with
                            Roads = roads
                        }
                        colony.Spatial.Rooms
            }
    }

/// The clustered kinds of a plan, paired with the `BuiltKind` that stands
/// on them, ready for `withBuilt`. The kind travels with the tile because
/// the census `gapAt` subtracts is per kind: extensions standing where the
/// plan expects towers would be a different room, not this one a level on.
let internal standingCluster placed =
    placed
    |> List.choose (fun (tile, kind) ->
        match kind with
        | Storage -> Some(tile, BuiltKind.Storage)
        | Tower -> Some(tile, BuiltKind.Tower)
        | StructureKind.Extension -> Some(tile, BuiltKind.Extension)
        | _ -> None)

// ---- the sweep ----------------------------------------------------------

/// One room planned from one spawn tile, at the level the whole Layout
/// exists at.
type internal Case =
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
        // The three fields below are Atlas censuses taken while the case is
        // built, here instead of the Atlas itself because a case is a value
        // every list shares and a shared Atlas is Expecto's parallel tests
        // writing one Dictionary (#310).

        /// The room's ground an Anchor or an upgrader works from
        /// (`workingGroundIn`), which the clustered ordering may not take.
        WorkingGround: Set<Pos>
        /// The room's tiles a creep can stand on (`walkableTilesIn`), which
        /// every paved tile has to be one of.
        Walkable: Set<Pos>
        /// The controller's Upgrade Work Area in this room, empty for a room
        /// the projection places no controller in: one of the two goals a
        /// trunk is drawn to (#107).
        UpgradeArea: Set<Pos>
        /// The sites the Layout asks for this tick.
        Placed: (Pos * StructureKind) list
        Unserved: UnservedFooting list
        /// The footings the Layout placed, each naming its target, that
        /// target's kind and the tile reserved for it. Read off the plan
        /// this case already computed, like `Unserved`: plans per case is
        /// the expensive axis of this sweep.
        Served: ServedFooting list
        /// The trunks the Layout could not route, one entry per (source,
        /// goal) the router found no path for. Read off the same plan.
        Unrouted: UnroutedTrunk list
        /// The container sites a tick on, with the road plan standing: on
        /// the first tick a container defers to the road it shares ground
        /// with, and the count the rule promises is the one that drops
        /// once the roads are up.
        Containers: Pos list
        /// Whether recalling the plan from its own memo gives back the
        /// plan that was computed. The memo path is the one worth testing:
        /// that a pure function is pure is not news.
        RecallsIdentically: bool
        /// The clustered counts this room stood up at the sweep's level,
        /// and what it asks for after levelling once with them standing.
        /// Per kind, because `gapAt` is per kind, and read against
        /// `allowanceOf`'s own table rather than a second plan: a horizon
        /// wrong the same way at both levels satisfies a plan-against-plan
        /// comparison and fails this one. Over a room that built out rather
        /// than two bare rooms, which do not nest (the extra tower pick
        /// shifts a bare room's extension picks by one).
        LevelUpAsks: (StructureKind * int * int) list
        /// The road tiles this room paves at the sweep's level that the
        /// same room, one level up with its cluster standing, no longer
        /// plans. Zero by construction while the reservation is level-blind,
        /// so the field records an invariant rather than a loss.
        LevelUpAbandons: Pos list
    }

/// Every (room, spawn) the suite sweeps, planned once. The invariants read
/// the same plan: re-deriving it per invariant would pay the tick's
/// dearest step many times over for one answer.
///
/// The sweep's level is 4, so the widest placement this bot ever makes (an
/// RCL7 room's horizon of 8) is exercised only by the per-room ladders in
/// `RoomLayoutInvariantTests` and `LayoutPlacementTests`, one spawn each.
/// Recorded rather than fixed: a second level would double the sweep's cost
/// for a placement window three of the five rooms will never reach.
let internal sweep =
    lazy
        [
            for room in rooms do
                let capture = load room.Name

                for spawn in spawnTiles room capture do
                    let loaded = project capture spawn room.FallbackController
                    let colony = colonyOf loaded 4
                    let atlas = ofView colony
                    let first = decide colony Map.empty Set.empty None
                    let placed = placementsOf first.Intents
                    let recalled = decide colony Map.empty Set.empty (Some first.Memo)

                    // This room built out at the sweep's own level and then
                    // levelled once. The cluster that stands is this case's
                    // own plan, so the whole ladder costs one plan.
                    let standing = standingCluster placed

                    let levelled =
                        decide (colonyOf loaded 5 |> withBuilt standing) Map.empty Set.empty None

                    let withRoads =
                        colony
                        |> withRoadsStanding room.Name (tilesOfKind Road placed |> Set.ofList)

                    yield
                        {
                            Room = room
                            Spawn = spawn
                            SourceCount = List.length loaded.SourceIds
                            Sources =
                                loaded.SourceIds
                                |> List.choose (fun id ->
                                    positionOf atlas id
                                    |> Option.map (fun tile -> id, RoomPos.pos tile))
                            SpawnId = (List.head colony.Spawns).Id
                            ControllerId = loaded.ControllerId
                            WorkingGround = workingGroundIn atlas room.Name
                            Walkable = walkableTilesIn atlas room.Name
                            UpgradeArea =
                                loaded.ControllerId
                                |> Option.map (fun controllerId ->
                                    workArea atlas (Upgrade controllerId)
                                    |> RoomPos.inRoom room.Name)
                                |> Option.defaultValue Set.empty
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
                            LevelUpAsks =
                                [
                                    for kind in [ Tower; StructureKind.Extension ] do
                                        yield
                                            kind,
                                            List.length (tilesOfKind kind placed),
                                            List.length (
                                                tilesOfKind kind (placementsOf levelled.Intents)
                                            )
                                ]
                            LevelUpAbandons =
                                Set.difference
                                    (tilesOfKind Road placed |> Set.ofList)
                                    (tilesOfKind Road (placementsOf levelled.Intents) |> Set.ofList)
                                |> Set.toList
                        }
        ]

/// How a case reads in a failure message: the room and the spawn it was
/// planned from are the whole reproduction.
let internal describe (case: Case) =
    $"%s{case.Room.Name} from %d{case.Spawn.X},%d{case.Spawn.Y}"

let internal violations pick =
    sweep.Value |> List.filter pick |> List.map describe

/// The (source, goal) pairs a case's road plan does *not* carry, derived
/// from the paved tiles alone: the road tiles beside the source must reach
/// the goal over roads alone, to the spawn and to the controller's Upgrade
/// Work Area. A second derivation of the fact the Layout records for itself;
/// one that agreed with itself would pin nothing.
///
/// Independent of the record and *not* of the plan: the roads are read off
/// this tick's sites, which are the road gap and not the road plan. A swept
/// colony starts with no road standing or pending, so the two coincide; a
/// case that started with roads built would seed this BFS off a hole and
/// call every trunk lost. A room the Layout cannot orient itself in plans
/// nothing and carries nothing to check.
let internal unroutedByRoads (case: Case) : UnroutedTrunk list =
    match case.ControllerId with
    | None -> []
    | Some _ ->
        let roads = tilesOfKind Road case.Placed |> Set.ofList

        let goals =
            [
                TrunkGoal.UpgradeArea, case.UpgradeArea
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

/// Whether a case's road plan carries every source both ways a trunk goes,
/// read off the pair-wise answer rather than flooding a second time.
let internal trunksCarryEverySource (case: Case) = List.isEmpty (unroutedByRoads case)

/// The colony with its lookahead moved, which is how one horizon is compared
/// against another: a test that wants an absolute horizon asks for it by
/// arithmetic on the colony's own level, since no constant is left to set.
let internal atLookahead lookahead (colony: ColonyView) =
    { colony with
        Tuning =
            { colony.Tuning with
                HorizonLookahead = lookahead
            }
    }

/// One border two captured rooms share, spelled the way a person reads a
/// map. The pairing is stated here rather than derived, because deriving
/// it is precisely what `seams` does: a test that recomputed the room-name
/// arithmetic would agree with the query however wrong both were.
type internal Border =
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
        /// direction the neighbour lies.
        Offset: Pos
        /// How wide the band is: the server's own answer, read off the two
        /// committed captures. Named here because the derivation the test
        /// runs is symmetric: a neighbour recaptured with a narrower exit
        /// row shrinks both sides at once, silently.
        Exits: int
    }

/// W12S27 is the room across W12S28's north edge and W13S28 the room
/// across its west edge (#83's two remote targets). W15S25 is a sector
/// centre four rooms off and borders neither, which is what makes it the
/// negative case below.
let internal borders =
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

/// The Atlas over two captures' border rings and their ground: a Seam is
/// answered from the border layer and from the ground behind the landing
/// tile, so this is its whole input, and every tile in it is the server's
/// own. The ground is not left out: a room with no ground is one every
/// crossing into it strands a body in, and one the shell cannot build.
let internal acrossFrom (near: RoomCapture) (far: RoomCapture) =
    { SpatialInfo.empty with
        Borders = Map.ofList [ near.RoomName, near.Border; far.RoomName, far.Border ]
        Rooms =
            Map.ofList
                [
                    near.RoomName,
                    { RoomLayer.empty with
                        Terrain = near.Terrain
                    }
                    far.RoomName,
                    { RoomLayer.empty with
                        Terrain = far.Terrain
                    }
                ]
    }
    |> AtlasFixtures.snapshotWith []
    |> ofView

/// The Atlas over two captures' ground *and* their rings, with one body
/// standing in the near room and the far room's own sources placed under the
/// loader's ids: the whole input a cross-room walk reads. Everything
/// geometric is the server's; the body and the ids are the test's.
///
/// Two things are the caller's, because they are the two things the fixtures
/// that take this differ in: whether the far room's source tiles are obstacles,
/// and which body is standing.
let private twoCaptureAtlas
    (near: RoomCapture)
    (far: RoomCapture)
    (stand: Pos)
    (farObstacles: Set<Pos>)
    body
    =
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
                        Obstacles = farObstacles
                    }
                ]
        Borders = Map.ofList [ near.RoomName, near.Border; far.RoomName, far.Border ]
        TargetKinds = far.Sources |> List.map (fun (id, _) -> id, Source) |> Map.ofList
    }
    |> AtlasFixtures.snapshotWith [ body ]
    |> ofView

/// The walk's own reading of that pair: nothing in the far room is an obstacle
/// and an ordinary worker is standing.
let internal walkingAcross (near: RoomCapture) (far: RoomCapture) (stand: Pos) =
    twoCaptureAtlas near far stand Set.empty (AtlasFixtures.worker "w")

/// How far apart the cross-room sweep's stands are: a second knob beside
/// `stride`, because a stand here costs a whole Atlas and its floods, so it
/// leaves a handful of stands per capture rather than a hundred. It strides
/// the room's *passable* tiles in `Pos` order — a position in that list,
/// not a coordinate — so editing `stride` does not move it. Deterministic
/// because a counterexample nobody can reproduce is a rumour.
let internal crossRoomStride = 397

/// Standing tiles spread over a capture's own passable ground by
/// `crossRoomStride`, so no tile is chosen for what it proves — the
/// sweep's habit, at the smaller scale a cross-room price wants (one Atlas
/// and its floods per tile).
let internal standingSample (capture: RoomCapture) =
    capture.Terrain
    |> TerrainGrid.toList
    |> List.filter (fun (_, terrain) -> terrain <> Wall)
    |> List.mapi (fun index (tile, _) -> index, tile)
    |> List.filter (fun (index, _) -> index % crossRoomStride = 0)
    |> List.map snd

/// Wall tiles of a capture, strided exactly as the standing sample is:
/// unreachable goals nobody chose for what they prove.
let internal wallSample (capture: RoomCapture) =
    capture.Terrain
    |> TerrainGrid.toList
    |> List.filter (fun (_, terrain) -> terrain = Wall)
    |> List.mapi (fun index (tile, _) -> index, tile)
    |> List.filter (fun (index, _) -> index % crossRoomStride = 0)
    |> List.map snd

/// The eight tiles around one, unclipped — what a spawner places a
/// finished body on, before the room says which of them is ground.
let internal neighbourhood (tile: Pos) =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if dx <> 0 || dy <> 0 then
                    { X = tile.X + dx; Y = tile.Y + dy }
    ]

/// The body every lead below is priced for: one fatigue-generating part
/// over one Move, carrying nothing. `AtlasFixtures.worker`'s own parts with
/// its own empty store, so the creep standing on the birth tile and the
/// replacement being priced for it have one fatigue factor between them
/// (`emptyFactorOf` of this list is `fatigueFactorOf` of that creep) —
/// which is what lets the two clocks below be compared at all.
let internal leadBody = [ Work; Carry; Move ]

/// The Atlas a cross-Seam lead is priced over: two captures' ground and
/// their rings, a spawn structure standing in the near room with every
/// neighbour but one obstructed, and the creep it would replace standing
/// on that one. Everything geometric is the server's; the spawner, the
/// obstacles that fence it and the creep are the test's, and no expected
/// value comes from any of them.
///
/// The fencing is what makes the comparison exact rather than approximate.
/// A lead's near leg floods out of *all* the tiles beside the spawner, and
/// the Matcher's walk floods out of the one tile its creep stands on; leave
/// the spawner a single free neighbour and the two floods are the same
/// flood, so the two joins may be read against each other tile by tile.
///
/// The far room takes whatever extra sources and obstacles the caller
/// hands it, which is how the same fencing is played on the other side of
/// the border: a probe source laid beside one goal tile with its every
/// other neighbour closed has that tile for its whole Work Area, so the
/// Matcher's minimum is taken over one named tile and the two clocks can
/// be compared *at* it rather than across a cluster (`probeBeside`).
let internal leadingAcross
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
    |> AtlasFixtures.snapshotWith [ AtlasFixtures.worker "w" ]
    |> ofView

/// A source laid beside one goal tile, and the obstacles that leave it no
/// other Seat: a Harvest of it has a Work Area of exactly that tile, so
/// `walkTicks` becomes an oracle for one goal rather than a cluster's
/// cheapest member. The source stands on the first in-range neighbour in
/// `neighbourhood`'s own order; the fence is that neighbour's whole
/// neighbourhood but the goal, plus the source's own tile, which is ground
/// and would be a second Seat. A goal the fence seals off answers absent on
/// both clocks and is compared all the same.
let internal probeBeside (goal: Pos) : Pos * Set<Pos> =
    let inGround (tile: Pos) =
        tile.X >= 1 && tile.X <= 48 && tile.Y >= 1 && tile.Y <= 48

    let source = neighbourhood goal |> List.filter inGround |> List.head

    source,
    neighbourhood source
    |> List.filter (fun tile -> tile <> goal)
    |> Set.ofList
    |> Set.add source

/// The outposts the captures below are read with: the two rooms laid in
/// beside W12S28 the way they were when the captures were taken. Read off
/// `Outpost.adr0042` and not off the live declaration, which moved when
/// W13S28 stood its own spawn; the geometry these tests pin did not. The
/// non-emptiness guard beside each loop keeps a renamed pair from being
/// checked as an empty list forever.
let internal declaredOutposts = Outpost.adr0042

/// The Atlas over the projection of one declared outpost on the tick the
/// colony cannot see it: that room's committed terrain and border ring —
/// the whole of what `Game.map.getRoomTerrain` answers without vision —
/// with the declaration laid in by `Outpost.place`, the production rule.
/// Built through `place` and never by hand: a helper that typed the
/// placement out would prove a source *placed* in the projection can be
/// priced, and stay green through the blind room's furniture never reaching
/// the projection at all (#148).
///
/// The home room, W12S28, carries no geometry, because what is under test
/// is the outpost's own: every query is asked of the outpost's layer and
/// would answer the empty set for a room the projection did not carry.
let internal declaredAtlas (outpost: Outpost) =
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
    |> AtlasFixtures.snapshotWith []
    |> ofView

/// The declared colony over the committed captures: W12S28 projected as the
/// colony's own room with a spawn on the live tile, every declared outpost's
/// ground and border ring laid in beside it, and the declaration's furniture
/// laid over that by `Outpost.place` and pooled by `Outpost.pooledSources`,
/// so nothing here hand-writes a source list.
///
/// Both rings, because a band is empty unless the projection carries both
/// sides. A `RoomControl` entry per outpost, held by nobody: that map is one
/// entry per *seen* room, and vision is the only state the container can be
/// planned in. Held by nobody rather than reserved because the container
/// comes before the reserver.
let internal declaredColony level =
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
                        SafeMode = false
                        Sign = None
                    }
                    control)
        Sources =
            colony.Sources
            |> Outpost.pooledSources
                (Outpost.roomsProjected declaredOutposts (SpatialInfo.homeName spatial))
                declaredOutposts
    }

/// The captured room with one creep standing in it: the shape the tick's
/// per-creep flood memo is laid for, over terrain no hand-built fixture
/// poses. Every case builds one of these per read order rather than sharing
/// one, because what a resumable flood answers must not depend on what was
/// asked of it before.
let internal standingIn (capture: RoomCapture) (spawn: Pos) (creep: CreepInfo) (stand: Pos) =
    (project capture spawn None).Spatial
    |> withCreepsAt [ creep.Name, stand ]
    |> AtlasFixtures.snapshotWith [ creep ]
    |> ofView

/// The room's own ground, nearest the creep first. This is the order a
/// tick actually reads a flood in — a Task is usually a dozen tiles off —
/// so it is the order that leaves the most of the room unsettled between
/// reads, and the one a resumed flood has the most chance to get wrong.
/// Its reverse settles almost the whole room on the first read, which is
/// the other end of the same promise.
let internal nearestFirst (stand: Pos) (capture: RoomCapture) =
    capture.Terrain
    |> TerrainGrid.toList
    |> List.map fst
    |> List.sortBy (fun tile -> range stand tile, tile.X, tile.Y)

/// A tile no flood can reach, wherever the creep stands: the border ring
/// is not the projection's ground, so nothing settles it and the read has
/// to answer absent rather than "not yet".
let internal offTheGround = { X = 0; Y = 0 }

/// The bodies the cases sweep: one at fatigue parity, which walks plain
/// ground at a tick a tile, and one below it, which pays two units a plain
/// step and prices swamp far dearer — different step tables, so different
/// heaps, tie-breaks and settle order over the same terrain.
let internal floodBodies =
    [
        "parity", AtlasFixtures.creepWith "w" 0 [ Carry; Carry; Move; Move ]
        "slow", AtlasFixtures.worker "w"
    ]

/// The rooms the cases sweep, each with the spawn tile the projection
/// stands its furniture on and the tile the creep stands on: the colony's
/// own room and one of #83's remote targets.
let internal floodRooms =
    [
        "W12S28", { X = 12; Y = 40 }, { X = 24; Y = 24 }
        "W13S28", { X = 24; Y = 24 }, { X = 12; Y = 30 }
    ]

/// One tile in thirteen gets a flood of its very own — a read no earlier
/// read can have contaminated. A stride rather than every tile because
/// this is the case that pays for an Atlas per tile, and the passes it
/// sits beside already compare all 2,304 against each other.
let internal coldStride = 13

/// Where a cross-room sweep stands its creep: the near room's own ground,
/// strided as every other cross-room case strides it, plus the crossings
/// themselves. A creep parked on a crossing is where the engine leaves one
/// the tick it steps over (#142, #145), and it is the stand the bound has
/// least room on — the flood is seeded on the ring tile whatever it
/// weighs, so the first approach it settles costs nothing and the frontier
/// it prunes by stays at the bottom of the band.
let internal standsAcross (near: RoomCapture) (far: RoomCapture) (from: string) (into: string) =
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
let internal drainTile (capture: RoomCapture) =
    capture.Terrain
    |> TerrainGrid.toList
    |> List.find (fun (_, terrain) -> terrain = Wall)
    |> fst

/// The body both readings of the walk are taken over: one fatigue-generating
/// part to one Move and no Carry at all. Carry-less is what makes the round
/// trip twice the one-way walk — an empty Carry generates no fatigue, so a
/// body holding one prices its two legs under two factors — and one Work
/// against one Move is under the Work-heavy line, so a Harvest keeps the
/// Work Area a source's own surroundings rather than a Post.
let internal haulBody = [ Work; Move ]

/// The two captures again, with the far room's sources standing as
/// obstacles and the creep carrying nothing: the one fixture on which the
/// hauler quota's round trip and the Matcher's walk are the same journey.
/// A source tile nothing may stand on makes the Harvest Work Area exactly
/// the sink's adjacent walkable tiles, and a Carry-less body prices both
/// legs under one fatigue factor.
let internal haulingAcross (near: RoomCapture) (far: RoomCapture) (stand: Pos) =
    twoCaptureAtlas
        near
        far
        stand
        (far.Sources |> List.map snd |> Set.ofList)
        (AtlasFixtures.creepWith "w" 0 haulBody)

/// The creep a Verdict is about: every arm names one. `Observe` keeps its
/// own copy `private`; this is the same total function and not a second
/// rule.
let internal verdictCreep =
    function
    | Verdict.Matched(creep, _, _)
    | Verdict.Kept(creep, _)
    | Verdict.Released(creep, _, _)
    | Verdict.Unassigned(creep, _)
    | Verdict.Scoring(creep, _)
    | Verdict.Grounded creep
    | Verdict.Yielded(creep, _)
    | Verdict.Rerouted creep
    | Verdict.Stalled creep -> creep

/// The three rungs of a colony's life the fixture is built at: the child
/// as it was claimed, the child at the level the bootstrap window closes
/// on, and the mother this bot grew up on.
let internal colonyTiers = [ "W13S28", 1, 300; "W13S28", 3, 800; "W12S28", 5, 1800 ]
