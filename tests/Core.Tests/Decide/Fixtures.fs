/// The colony view builders every `Decide` suite shares — the rooms, the
/// colonies, the bodies — beside the small readers that pull one shape out of a
/// Decision. What earns a place here is a caller in more than one sibling
/// suite: a fixture only one suite reads stays private in that suite, beside
/// the tests it serves, so this module stays the shared surface rather than a
/// second home for everything. Which suite a new test belongs in is written
/// down in `docs/agents/orchestration.md` — by domain, never by ticket.
module Fabot.Core.Tests.Decide.Fixtures

open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests

/// A single idle spawn standing in the default room.
let spawn =
    {
        Name = "Spawn1"
        Id = "spawn-1"
        RoomName = "W1N1"
        IsSpawning = false
    }

/// The colony's bank holding the given energy against the given capacity:
/// one account, its home room's.
let bank energy capacity : RoomEnergy =
    {
        Available = energy
        Capacity = capacity
    }

/// A controller far from its downgrade deadline, stock intact.
let controllerAt level =
    {
        Id = "ctrl-1"
        Level = level
        TicksToDowngrade = 20000
        SafeModeAvailable = 1
        SafeModeActive = false
    }

/// A room this colony owns: the spawn room's control entry, and the rate
/// every source in it is priced at.
let ownedRoom: RoomControlInfo =
    {
        Owner = Ownership.Ours
        Reservation = None
        SafeMode = false
        Sign = None
    }

/// A room another player has taken: seen, owned, and owned by somebody
/// else. The one control entry that latches a stand-down, where a rival's
/// reservation runs a clock.
let rivalRoom: RoomControlInfo =
    {
        Owner = Ownership.Rival
        Reservation = None
        SafeMode = false
        Sign = None
    }

/// A neutral room nobody holds: seen, and worth half. Not the same fact as
/// a room with no entry at all, which is one the colony cannot see.
let neutralRoom: RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation = None
        SafeMode = false
        Sign = None
    }

/// A neutral room under a reservation of the given holder, with the given
/// ticks left on it: ours is what doubles the room, so `false` is the
/// rival's reservation, which reads as none at all for pricing.
let reservedRoom ours ticksToEnd : RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation =
            Some
                {
                    Holder =
                        if ours then
                            ReservationHolder.Ours
                        else
                            ReservationHolder.Rival
                    TicksToEnd = ticksToEnd
                }
        SafeMode = false
        Sign = None
    }

/// A neutral room whose reservation the NPC Invader holds — what a
/// level-0 invader core leaves behind when it `attackController`s a room
/// it expanded into. A fixture of its own because it prices exactly as a
/// rival's does and withdraws on the opposite rule.
let coreReservedRoom ticksToEnd : RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation =
            Some
                {
                    Holder = ReservationHolder.Invader
                    TicksToEnd = ticksToEnd
                }
        SafeMode = false
        Sign = None
    }

/// The stage map of a colony that declares itself and nothing else, at the
/// given controller level. Keyed by the home name alone, unlike
/// `homeControl` below: a stage entry says a colony of ours lives there, so
/// a second name would be a second colony hiring pioneers. Derived through
/// `Colony.stageOf` and never written down: a fixture that spelled a stage
/// out could spell a colony the shell can never build.
let homeStages (spatial: SpatialInfo) level =
    match Colony.stageOf Tuning.defaults true true (Some level) with
    | Some stage -> Map.ofList [ SpatialInfo.homeName spatial, stage ]
    | None -> Map.empty

/// The control map for a colony holding its own room and nothing else, under
/// both names the fixtures file a home layer under: `SpatialInfo.empty` and
/// the `spatial` funnel file under the empty name, `openRoom` under "W1N1".
/// An entry for a room the fixture has no layer for is read by nothing.
let homeControl = Map.ofList [ "", ownedRoom; "W1N1", ownedRoom ]

/// A stocked source: a restock of zero, ready to dig now.
let source id : SourceInfo = { Id = id; TicksToRestock = 0 }

/// A drained source, the given number of ticks from its restock.
let drained id ticks : SourceInfo = { Id = id; TicksToRestock = ticks }

/// An energy-hungry structure of the given kind with the given free capacity.
let refillable id freeCapacity kind =
    {
        Id = id
        FreeCapacity = freeCapacity
        Kind = kind
    }

let bareRespawn =
    {
        Time = 42
        Spawns = [ spawn ]
        Bank = bank 300 300
        Refillables = [ refillable "spawn-1" 0 BuiltKind.Spawn ]
        Sources = [ source "src-a"; source "src-b" ]
        Controller = Some(controllerAt 1)
        RoomControl = homeControl
        HeldOutposts = Set.empty
        ThreatenedOutposts = Set.empty
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        InvaderCores = []
        Spatial = SpatialInfo.empty
        // No colony declared but this one, which claims nothing.
        Declared = []
        // No stage either, deliberately: this fixture's home is
        // `SpatialInfo.empty`'s unnamed room, and a stage filed under
        // `openRoom`'s "W1N1" would read as a second colony of ours
        // standing beside it, hiring pioneers. A stage arrives with the
        // room: `atLevel` and `withLevel` file one under the home the
        // fixture actually has.
        Stages = Map.empty
        // One colony in the world: every body is its own, it raises no
        // child, declares no errand, and every tick has vision. "W1N2"
        // borders "W1N1", so nothing is refused.
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
        // Nothing in the oven: the casting cascade's own tests put a body
        // here.
        Casting = []
    }

/// A creep with the given body's part counts, freshly cast: a full
/// Screeps CREEP_LIFE_TIME to live, so no fixture creep is expiring and no
/// lead has to be priced to read a test.
let creepWith name energy freeCapacity body =
    {
        Name = name
        TicksToLive = 1500
        Hits =
            {
                Hits = Engine.partHits * List.length body
                HitsMax = Engine.partHits * List.length body
            }
        Fatigue = 0
        Energy = energy
        // Energy unless a case says otherwise: `carrying` below is the one
        // builder that puts the season's ore in a body.
        Thorium = 0
        FreeCapacity = freeCapacity
        Moved = false
        Body = body |> List.countBy id |> Map.ofList
    }

/// The same creep with the given ticks left to live — what puts it inside
/// its row's lead and makes it expiring.
let withLife ticks (creep: CreepInfo) = { creep with TicksToLive = ticks }

/// A generalist worker-unit creep: one Work, one Carry, one Move.
let worker name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Carry; Move ]

let spawnIntents intents =
    intents
    |> List.choose (function
        | SpawnCreep(s, b, c) -> Some(s, b, c)
        | _ -> None)

/// Synthetic open room: every tile within `radius` of (25,25) is Plain,
/// with the spawn structure "spawn-1" standing at the centre.
let openRoom radius =
    let spawnPos = { X = 25; Y = 25 }

    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                TerrainGrid.ofList
                    [
                        for x in 25 - radius .. 25 + radius do
                            for y in 25 - radius .. 25 + radius do
                                { X = x; Y = y }, Plain
                    ]
            TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
            Obstacles = Set.singleton spawnPos
        })

/// The room with extra targets standing (or being built) on given tiles.
let withTargets targets (room: SpatialInfo) =
    { room with
        TargetKinds =
            (room.TargetKinds, targets)
            ||> List.fold (fun acc (id, _, kind) -> Map.add id kind acc)
    }
    |> withHome (fun layer ->
        { layer with
            TargetPositions =
                (layer.TargetPositions, targets)
                ||> List.fold (fun acc (id, pos, _) -> Map.add id pos acc)
        })

let placementIntents intents =
    intents
    |> List.choose (function
        | PlaceConstructionSite(tile, kind) -> Some(tile.Room, RoomPos.pos tile, kind)
        | _ -> None)

let placedTiles intents =
    placementIntents intents |> List.map (fun (_, pos, _) -> pos)

/// The tiles a plan places one kind of site on, in plan order.
let sitesOfKind kind intents =
    placementIntents intents
    |> List.choose (fun (_, pos, k) -> if k = kind then Some pos else None)

/// The colony at one controller level: the level its controller carries
/// and the stage that level puts it at, moved together, because a fixture
/// that moved one alone would be a colony the shell cannot build.
let atLevel level room =
    { bareRespawn with
        Controller = Some(controllerAt level)
        Stages = homeStages room level
        Spatial = room
    }

/// The same rule for a colony a fixture has already assembled: move the
/// level and the stage together, never one alone.
let withLevel level (colony: ColonyView) =
    { colony with
        Controller = Some(controllerAt level)
        Stages = homeStages colony.Spatial level
    }

/// The trunk fixture: a broad plain field with the spawn at
/// (25,25), the controller at (35,25), one source embedded in wall terrain
/// at (15,25), two swamps inside the controller's Upgrade Work Area, one
/// far swamp off every trunk line, and two extensions already built on the
/// cluster's nearest tiles.
let trunkRoom =
    let sourcePos = { X = 15; Y = 25 }
    let spawnPos = { X = 25; Y = 25 }
    let controllerPos = { X = 35; Y = 25 }
    let builtExtensions = [ { X = 24; Y = 26 }; { X = 26; Y = 24 } ]
    let areaSwamps = [ { X = 33; Y = 27 }; { X = 34; Y = 24 } ]

    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds =
            Map.ofList
                [
                    "spawn-1", Structure BuiltKind.Spawn
                    "ctrl-1", Controller
                    "src-a", Source
                    "ext-1", Structure BuiltKind.Extension
                    "ext-2", Structure BuiltKind.Extension
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                TerrainGrid.ofList
                    [
                        for x in 10..40 do
                            for y in 15..35 do
                                let tile = { X = x; Y = y }

                                tile,
                                (if tile = sourcePos then
                                     Wall
                                 elif
                                     List.contains tile areaSwamps || tile = { X = 20; Y = 20 }
                                 then
                                     Swamp
                                 else
                                     Plain)
                    ]
            TargetPositions =
                Map.ofList
                    [
                        "spawn-1", spawnPos
                        "ctrl-1", controllerPos
                        "src-a", sourcePos
                        "ext-1", builtExtensions.[0]
                        "ext-2", builtExtensions.[1]
                    ]
            Obstacles = Set.ofList (spawnPos :: controllerPos :: builtExtensions)
        })

/// The trunk fixture's colony at a controller level.
let trunkColony level =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt level)
        Stages = homeStages trunkRoom level
        Spatial = trunkRoom
    }

/// The clustered structures of a plan: the Storage, the tower and every
/// extension, the tiles one ordering rule picks.
let clusterTiles intents =
    sitesOfKind Storage intents
    @ sitesOfKind Tower intents
    @ sitesOfKind Extension intents
    |> Set.ofList

/// The trunk colony with one extra target standing (or pending) anywhere.
let withTarget id pos kind colony =
    { colony with
        Spatial = colony.Spatial |> withTargets [ id, pos, kind ]
    }

/// The same colony with a second room's geometry beside its own. The kind
/// census stays unlayered and world-unique, as the projection keeps it,
/// which is what lets these fixtures ask whether a reader joins a kind to
/// the right room's tile. Adds the outpost's entry and never replaces the
/// map, so the helper is order-blind.
let withOutpost room targets tiles (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain = TerrainGrid.ofList tiles
                            TargetPositions =
                                targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                        }
                        colony.Spatial.Rooms
                TargetKinds =
                    (colony.Spatial.TargetKinds, targets)
                    ||> List.fold (fun acc (id, _, kind) -> Map.add id kind acc)
            }
    }

/// The 8 tiles around a position, all Plain: an open-ground source site.
let openSeats pos =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if (dx, dy) <> (0, 0) then
                    { X = pos.X + dx; Y = pos.Y + dy }, Plain
    ]

let harvesters assignments sourceId =
    assignments
    |> Map.toList
    |> List.filter (fun (_, tid) -> tid = taskId (Harvest sourceId))
    |> List.map fst

let moveIntents intents =
    intents
    |> List.choose (function
        | MoveCreep(name, direction) -> Some(name, direction)
        | _ -> None)

/// Creep action Intents only — spawn and placement Intents filtered out.
let actionIntents intents =
    intents
    |> List.filter (function
        | HarvestSource _
        | TransferEnergyToStructure _
        | BuildSite _
        | UpgradeController _ -> true
        | _ -> false)

/// The tile one step in `direction` from `pos` — mirrors the engine's move.
let stepFrom (pos: Pos) direction =
    match direction with
    | Top -> { pos with Y = pos.Y - 1 }
    | TopRight -> { X = pos.X + 1; Y = pos.Y - 1 }
    | Right -> { pos with X = pos.X + 1 }
    | BottomRight -> { X = pos.X + 1; Y = pos.Y + 1 }
    | Bottom -> { pos with Y = pos.Y + 1 }
    | BottomLeft -> { X = pos.X - 1; Y = pos.Y + 1 }
    | Left -> { pos with X = pos.X - 1 }
    | TopLeft -> { X = pos.X - 1; Y = pos.Y - 1 }

/// One tick of `decide` over a colony that remembers nothing: no prior
/// assignments, nobody owed verbose scoring, no plan memo. That is the whole
/// of what all but a handful of this suite's `decide` calls pass, and it is
/// three arguments of noise at each of them — beside `poolOn` and `resolveOn`,
/// which run the two seams below it the same way.
let decideOn colony = decide colony Map.empty Set.empty None

/// The same tick with remembered assignments — the one argument a keep-path
/// test varies.
let decideFrom assigned colony = decide colony assigned Set.empty None

/// `planTasksOn` over a colony whose assignment table holds nothing. Named
/// `…On` like `decideOn` rather than shadowing `Planner.planTasks`: a shadow
/// resolves by open order, and the day the Planner gains a fourth fact the
/// tempting repair is to default it here and leave every call site compiling
/// with two facts it never named. A test *about* the two lines calls
/// `planTasksHolding` below.
let planTasksOn view threats =
    Planner.planTasks
        view
        (Atlas.ofView view)
        threats
        HeldTaskFacts.empty
        (Planner.outpostFactsOf view)

/// `planTasksOn` over a colony whose living creeps hold these Tasks, spelled
/// forward through `taskId` so a test names the Task and never a string.
let planTasksHolding (holding: Task list) view =
    Planner.planTasks
        view
        (Atlas.ofView view)
        noThreats
        { HeldTaskFacts.empty with
            All = holding |> List.map taskId |> Set.ofList
        }
        (Planner.outpostFactsOf view)

/// The same narrow held-task fact for a carrier that still has Thorium aboard.
let planTasksHoldingThorium (holding: Task list) view =
    let ids = holding |> List.map taskId |> Set.ofList

    Planner.planTasks
        view
        (Atlas.ofView view)
        noThreats
        { All = ids; WithThorium = ids }
        (Planner.outpostFactsOf view)

/// This tick's pool with its priorities and capacities, over the
/// snapshot's own Atlas — what the Matcher and the mover are both handed.
let poolOn snapshot =
    planPool snapshot (Atlas.ofView snapshot) (planTasksOn snapshot noThreats)

/// Run the Resolver at its own seam: assigned Tasks as data over the
/// snapshot's Atlas; a creep absent from the list is idle. Move Intents
/// only; the movement Verdicts riding beside them are resolveVerdictsOn.
let resolveOn snapshot assigned =
    resolve
        snapshot
        (Atlas.ofView snapshot)
        noThreats
        (poolOn snapshot)
        (Map.ofList assigned)
        Map.empty
        Set.empty
    |> fst

/// A one-wide east-west lane, a rock walled in at either end, and — when
/// `pocket` — one tile of ground beside it at (11,11). That tile is the
/// only one there is that is beside both (11,12) and the step east onto
/// (12,12) and beside nothing further east at all: somewhere to stand out
/// of the way, and never a way around. Which is what makes it the
/// counterexample #219 needs — a detour the flood's own occupancy
/// surcharge can never choose, so what takes it is the Move Intent's tail
/// and nothing else.
let lane pocket =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                TerrainGrid.ofList (
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ { X = 7; Y = 12 }, Wall; { X = 16; Y = 12 }, Wall ]
                    @ (if pocket then [ { X = 11; Y = 11 }, Plain ] else [])
                )
            TargetPositions =
                Map.ofList [ "src-w", { X = 7; Y = 12 }; "src-e", { X = 16; Y = 12 } ]
        })

let sayIntents intents =
    intents
    |> List.choose (function
        | SayCreep(name, message) -> Some(name, message)
        | _ -> None)

/// Project one structure of the given built kind carrying the given hits
/// onto a snapshot — position-less, so the pool and matching are exercised
/// without terrain.
let withHits id kind hits hitsMax (snapshot: ColonyView) =
    { snapshot with
        Spatial =
            { snapshot.Spatial with
                TargetKinds = Map.add id (Structure kind) snapshot.Spatial.TargetKinds
                Hits = Map.add id { Hits = hits; HitsMax = hitsMax } snapshot.Spatial.Hits
            }
    }

let repairTasks tasks =
    tasks
    |> List.choose (function
        | Repair structureId -> Some structureId
        | _ -> None)

/// The tier fixture: a two-row plain corridor, y = 10..11, x = 9..21,
/// carrying one Refill target per tier — the spawn at (11,10), a tower at
/// (14,10) and the controller container at (18,10), inside the Work Area
/// of the controller standing at (20,10). Spawn, tower and controller
/// stand as obstacles; the second row keeps the corridor open past them.
let tierRoom =
    let corridor =
        [
            for x in 9..21 do
                for y in 10..11 -> { X = x; Y = y }, Plain
        ]

    { spatial [] corridor with
        Stores = Map.ofList [ "can-ctrl", 800 ]
    }
    |> withObstacles [ { X = 11; Y = 10 }; { X = 14; Y = 10 }; { X = 20; Y = 10 } ]
    |> withTargets
        [
            "spawn-1", { X = 11; Y = 10 }, Structure BuiltKind.Spawn
            "tower-1", { X = 14; Y = 10 }, Structure BuiltKind.Tower
            "can-ctrl", { X = 18; Y = 10 }, Structure BuiltKind.Container
            "ctrl-1", { X = 20; Y = 10 }, Controller
        ]

let activations intents =
    intents
    |> List.choose (function
        | ActivateSafeMode id -> Some id
        | _ -> None)

/// A hostile creep of the given body standing on the given tile. Its owner
/// and its room are immaterial to the reflexes; the room is the empty
/// string, the name a projection that names none files under, which is
/// what the colonies assigning `Hostiles` directly here are built on. A
/// colony that names its room gets that name stamped on by `facing`.
let hostileAt id pos body : HostileInfo =
    {
        Id = id
        Owner = "raider"
        Pos = RoomPos.at "" pos
        Body = body
        // A full Invader life: a fixture that said otherwise would be
        // making a claim about the stand-down rather than about the raid.
        TicksToLive = Engine.creepLifetime
    }

/// The same colony with the given hostiles standing in its own room, the
/// name stamped on here. A hostile carrying the empty name would be filed
/// in a room an `openRoom` projection holds no layer for, so every reader
/// that joins it to the geometry around it would measure it against
/// nothing.
let facing hostiles (snapshot: ColonyView) =
    { snapshot with
        Hostiles =
            hostiles
            |> List.map (fun (h: HostileInfo) ->
                { h with
                    Pos = RoomPos.at (SpatialInfo.homeName snapshot.Spatial) (RoomPos.pos h.Pos)
                })
    }

/// One source at (10,10) and the controller at (13,10). The Seat (11,10)
/// sits at range 2 of the controller — inside its Upgrade Work Area — while
/// (9,10) sits at range 4. No container, so no Post (#405).
let unpostedRoom =
    { spatial
          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 13; Y = 10 } ]
          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
    }

/// An Anchor-bodied creep: four Work, one Carry, one Move.
let anchor name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Work; Work; Work; Carry; Move ]

/// The unposted room, one source, controller in place — the base Anchor scenario.
let unpostedColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt 2)
        Spatial = unpostedRoom
    }

/// The unposted room with a built container on the Seat (11,10): its one Post.
let postedRoom =
    { unpostedRoom with
        TargetKinds = unpostedRoom.TargetKinds |> Map.add "cont-1" (Structure BuiltKind.Container)
    }
    |> withHome (fun layer ->
        { layer with
            TargetPositions = layer.TargetPositions |> Map.add "cont-1" { X = 11; Y = 10 }
        })

/// The base Anchor colony standing on `postedRoom`.
let postedColony =
    { unpostedColony with
        Spatial = postedRoom
    }

let moveIntentsFor name intents =
    intents
    |> List.filter (function
        | MoveCreep(creep, _) -> creep = name
        | _ -> false)

/// The dig Intents a creep is issued this tick — what tells a body the
/// Matcher kept on Harvest from one the Emitter actually lets dig.
let digIntentsFor name intents =
    intents
    |> List.filter (function
        | HarvestSource(creep, _) -> creep = name
        | _ -> false)

/// The tile the mine fixture's Thorium deposit stands on, and the mine post
/// beside it: the deposit is embedded in wall where the mod puts one, and
/// the container on its east Seat is the tile the miner stands on.
let minePos = { X = 10; Y = 10 }
let minePost = { X = 11; Y = 10 }

/// The mine fixture: a plain corridor y = 10, x = 8..20
/// with the deposit "min-a" embedded in wall at (10,10) and the spawn standing
/// at (15,10). The extractor "ext-a" stands on the deposit's **own tile** —
/// which is the placement rule, the extractor's tile being its target's — and
/// the mineral container "can-min" on the Seat (11,10). The deposit holds a
/// whole d3 reading of 22,000 Thorium and the extractor's clock reads zero, so
/// the fixture as it stands is a colony that may dig this tick; every case
/// below moves exactly one of those facts.
///
/// No sources at all, deliberately: this fixture is about the one rock the
/// colony harvests that has no regeneration, no store to overflow into and no
/// [[anchor]] over it, and a source beside it would put a second Harvest in the
/// pool for every case to have to exclude.
let mineColony =
    { bareRespawn with
        Sources = []
        Refillables = []
        Controller = None
        Spatial =
            { spatial [] [ for x in 8..20 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
                Thorium = Map.ofList [ "min-a", 22_000 ]
                Cooldowns = Map.ofList [ "ext-a", 0 ]
            }
            |> withObstacles [ { X = 15; Y = 10 } ]
            |> withTargets
                [
                    "min-a", minePos, Mineral
                    "ext-a", minePos, Structure BuiltKind.Extractor
                    "can-min", minePost, Structure BuiltKind.Container
                    "spawn-1", { X = 15; Y = 10 }, Structure BuiltKind.Spawn
                ]
    }

/// The same colony with the extractor **not yet standing**: the kind the
/// projection files it under moves from a structure to a construction site, and
/// nothing else does. The pairwise premise of "a site is 0".
let withExtractorSite (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ext-a" (Site BuiltKind.Extractor) colony.Spatial.TargetKinds
            }
    }

/// The same colony with the deposit **gone**, which is what the mod does to an
/// exhausted one: the target leaves the projection outright, taking its kind,
/// its tile and its remaining amount with it.
let withDepositGone (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.remove "min-a" colony.Spatial.TargetKinds
                Thorium = Map.remove "min-a" colony.Spatial.Thorium
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.remove "min-a" layer.TargetPositions
                })
    }

/// The same colony with the mineral container **not yet standing**, so the
/// deposit has no mine [[post]] and nowhere for a miner to dig from.
let withoutMineContainer (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.remove "can-min" colony.Spatial.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.remove "can-min" layer.TargetPositions
                })
    }

/// The same colony with the extractor's clock reading `ticks` rather than zero
/// — the five ticks in six on which `harvest.js` refuses the act.
let onCooldown ticks (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Cooldowns = Map.add "ext-a" ticks colony.Spatial.Cooldowns
            }
    }

/// A [[miner]]-bodied creep: the row's own block, Work-heavy with **no Carry at
/// all**, which is the one shape no other row of this colony casts. Store-less,
/// so it has neither energy nor free capacity to report.
let miner name = creepWith name 0 0 [ Work; Work; Move ]

/// The same body with `units` of Thorium aboard, out of the free room it was
/// built with: a creep's store is general, so what the ore takes it takes off
/// the whole store's free capacity. The one builder that puts the season's
/// ore in a body; a case that wants a mixed load says both fields by hand.
let carrying units (creep: CreepInfo) =
    { creep with
        Thorium = units
        FreeCapacity = max 0 (creep.FreeCapacity - units)
    }

/// The mine fixture with the colony's Storage standing at (14,10) — an
/// obstacle, as the projection carries a built one — and 600 Thorium in the
/// mineral container: the whole of the mine-to-Storage leg in one colony, the
/// Storage being the one store the contact penalty never reaches because
/// nothing can stand on it.
///
/// The corridor is one tile wide, so the stand is forced and legible: the
/// container's only Seat is (12,10) and the Storage's is (13,10), one step
/// apart. No controller and no Refillables, inherited from `mineColony`, so the
/// only energy sink in the pool is the Storage's own — which is what makes the
/// Thorium pair's ranking readable beside it.
let mineHaulColony =
    { mineColony with
        Spatial =
            { mineColony.Spatial with
                Thorium = Map.add "can-min" 600 mineColony.Spatial.Thorium
            }
            |> withObstacles [ { X = 14; Y = 10 } ]
            |> withTargets [ "sto-1", { X = 14; Y = 10 }, Structure BuiltKind.Storage ]
    }

/// The same colony under a named home room, geometry and all. Every fixture
/// built on `SpatialFixtures.spatial` carries its layer under the empty name,
/// which is invisible until a rule reads the name, and `planConsignment`
/// does. This moves the layer rather than setting the field: naming the room
/// and leaving the geometry where it was leaves every room-keyed query
/// falling back to `RoomLayer.empty`.
let named room (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial (SpatialInfo.homeName colony.Spatial)

    { colony with
        Spatial =
            { colony.Spatial with
                RoomName = Some room
                Rooms = Map.add room layer colony.Spatial.Rooms
            }
    }

/// The ground both colony reflexes are tested on (#381): one target of the
/// given kind at (10,10), open plain five tiles either way, and the given
/// creeps standing on the given tiles. The pickup reflex and the signature
/// reflex ask the same question of it — who is standing within range 1 of one
/// thing — so they ask it of one fixture rather than of two that agree by
/// coincidence.
let reflexColony targetId kind creeps positions =
    { bareRespawn with
        Sources = []
        Controller = None
        Creeps = creeps
        Spatial =
            { spatial
                  [ targetId, { X = 10; Y = 10 } ]
                  [
                      for x in 8..12 do
                          for y in 8..12 -> { X = x; Y = y }, Plain
                  ] with
                TargetKinds = Map.ofList [ targetId, kind ]
            }
            |> withCreepsAt positions
    }

/// The shipping colony (#349): `mineHaulColony`'s mine and Storage with a
/// **terminal** standing beside them at (15,10) and a consignee declared. This
/// is W12S28's live shape — a bank of ore, a terminal, and no errand anywhere
/// within `Tuning.MaxHops` — and the fixture keeps the mine so that the
/// outbound haul and the mine haul are pooled against each other rather than in
/// isolation.
///
/// `thorium` and `energy` are the terminal's own two stores, which are the only
/// facts `planConsignment` reads besides the declaration: what there is to ship
/// and what there is to pay the fee with.
let consigningColony thorium energy =
    let home = named "W1N1" mineHaulColony

    { home with
        Consignee = Some "W1N4"
        Spatial =
            { home.Spatial with
                Thorium = home.Spatial.Thorium |> Map.add "sto-1" 20_000 |> Map.add "term-1" thorium
                Stores = Map.add "term-1" energy home.Spatial.Stores
            }
            |> withTargets [ "term-1", { X = 15; Y = 10 }, Structure BuiltKind.Terminal ]
    }

/// The receiving end of the same consignment: the terminal holds ore that
/// arrived by `send`, and this colony declares no consignee of its own — which
/// is what makes the ore walk **out** of the terminal here and into it there.
let receivingColony arrived =
    let home = named "W1N1" mineHaulColony

    { home with
        Consignee = None
        Crossed = Set.empty
        Spatial =
            { home.Spatial with
                Thorium = Map.add "term-1" arrived home.Spatial.Thorium
            }
            |> withTargets [ "term-1", { X = 15; Y = 10 }, Structure BuiltKind.Terminal ]
    }

let sends intents =
    intents
    |> List.choose (function
        | SendFromTerminal(terminal, resource, amount, destination) ->
            Some(terminal, resource, amount, destination)
        | _ -> None)

/// The sends a whole tick emits (#349). Taken off `decide` and not off
/// `Layout.planConsignment`, which is `internal`: the intent has to survive
/// `IntentPlan.create`'s channel check to be worth asserting, or a structure
/// verb that collided with a creep's would be dropped with the rule that wrote
/// it still green.

let sendsOn view = (decideOn view).Intents |> sends

/// The same colony with the mineral container holding `units` rather than 600 —
/// the one fact a pairwise case about the Thorium leg moves.
let withMineStock units (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Thorium = Map.add "can-min" units colony.Spatial.Thorium
            }
    }

/// The same colony with `units` of the season's ore lying **on the mine post**
/// (#311) — the pile "pile-min" on (11,10), which is the mineral container's own
/// tile, because that is where it lands: the [[miner]] stands on the container
/// and the engine drops its dig on the floor of that tile the moment the
/// container is at its 2,000 cap. The kind carries the resource and the amount
/// rides in the Thorium column beside the container's, a pile holding its
/// amount in `object[resourceType]` and never in a `store`.
let withMinePile units (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Thorium = Map.add "pile-min" units colony.Spatial.Thorium
            }
            |> withTargets [ "pile-min", minePost, Dropped Thorium ]
    }

/// The haul fixture: a plain corridor y = 10, x = 9..21; the
/// source embedded in wall at (10,10) with Seats (9,10) and (11,10), the
/// controller standing at (20,10); the source container "can-src" on the
/// Seat (11,10) and the controller container "can-ctrl" at (18,10),
/// inside the Upgrade Work Area. The buffer starts stocked, the source
/// container empty.
let haulRoom =
    { spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
        Stores = Map.ofList [ "can-src", 0; "can-ctrl", 800 ]
    }
    |> withObstacles [ { X = 20; Y = 10 } ]
    |> withTargets
        [
            "src-a", { X = 10; Y = 10 }, Source
            "ctrl-1", { X = 20; Y = 10 }, Controller
            "can-src", { X = 11; Y = 10 }, Structure BuiltKind.Container
            "can-ctrl", { X = 18; Y = 10 }, Structure BuiltKind.Container
        ]

let haulColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = haulRoom
    }

/// The W11S27 shape (#405): the source in wall at (10,10) on a plain field
/// x = 8..14, y = 8..12, the controller in wall at (12,11). Two containers
/// stand on the rock's Seats: "can-far" at (9,9), range 3 of the controller,
/// and "can-near" at (11,10), range 1. The rock's Post is the far one, and the
/// near one is the controller's buffer. Both start empty.
let seatBufferRoom =
    { spatial
          []
          [
              for x in 8..14 do
                  for y in 8..12 ->
                      { X = x; Y = y },
                      (if (x, y) = (10, 10) || (x, y) = (12, 11) then
                           Wall
                       else
                           Plain)
          ] with
        Stores = Map.ofList [ "can-far", 0; "can-near", 0 ]
    }
    |> withTargets
        [
            "src-a", { X = 10; Y = 10 }, Source
            "ctrl-1", { X = 12; Y = 11 }, Controller
            "can-far", { X = 9; Y = 9 }, Structure BuiltKind.Container
            "can-near", { X = 11; Y = 10 }, Structure BuiltKind.Container
        ]

let seatBufferColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt 3)
        Spatial = seatBufferRoom
    }

/// A hauler-unit creep: two Carry, one Move — the hauler row's block.
let hauler name energy freeCapacity =
    creepWith name energy freeCapacity [ Carry; Carry; Move ]

/// The crowding fixture (#161): a three-row plain field y = 9..11,
/// x = 5..35, with two containers standing on the middle row — "can-near"
/// at (10,10) and "can-far" at (30,10). Every creep the tests below stand
/// on it sits one step from the near store's Work Area and seventeen or
/// more from the far one, so travel cost points the whole crowd at one
/// container and only a capacity can send any of it to the other. Three
/// rows and not one, so a waiting hauler is never in another's path: a
/// crowd that thins itself by standing in its own way would prove the cap
/// without the cap. Two containers and not a container and a Storage,
/// because the two must sit on the same tier.
let crowdField =
    [
        for x in 5..35 do
            for y in 9..11 -> { X = x; Y = y }, Plain
    ]

/// The pile fixture (#167): the crowding field again, one dropped pile on
/// the middle row at (10,10) holding the given amount, and empty hauler
/// bodies on the given tiles.
///
/// The bank is 150 — one whole hauler block, `[2 Carry; 1 Move]` — so a
/// trip is exactly 100 energy and every capacity below is written against
/// that load. No source, no placed controller and a full spawn, so a
/// Carry-only body is applicable to the pile and to nothing else: what
/// these tests read is the Pickup rule and never a tie against some other
/// Task.
let pileTaskColony amount (creeps: (string * Pos) list) =
    { bareRespawn with
        Bank = bank 150 150
        Sources = []
        Creeps = [ for name, _ in creeps -> hauler name 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "pile-a", amount ]
            }
            |> withTargets [ "pile-a", { X = 10; Y = 10 }, (Dropped Energy) ]
            |> withCreepsAt creeps
    }

/// The workforce target this ColonyView decides — the sum every row is an
/// addend of (`Quota.workforceTarget`).
let targetOf (colony: ColonyView) = (decideOn colony).Quotas.Target

/// The same colony with `units` of energy standing in a Storage of its own
/// (#364): the **stock**, as against `Bank`'s spawn account, which is what the
/// worker row's backlog term is paid out of.
let stocking units (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "sto-stock" (Structure BuiltKind.Storage) colony.Spatial.TargetKinds
                Stores = Map.add "sto-stock" units colony.Spatial.Stores
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "sto-stock" { X = 26; Y = 26 } layer.TargetPositions
                })
    }

/// The same colony with a construction site standing for each of these
/// outstanding costs (#364). The cost is the fact the backlog term reads: a
/// road owes 300 and a terminal 100,000, and a row that cannot tell them apart
/// hires the same crowd at either.
let owing (costs: int list) (colony: ColonyView) =
    { colony with
        ConstructionSites =
            costs
            |> List.mapi (fun index cost ->
                ({
                    Id = $"site-owing-{index}"
                    Left = cost
                }
                : ConstructionSiteInfo))
    }

/// The hauler quota this ColonyView decides, read off the plan memo `decide`
/// returns — the quota's only seam, since the rule itself is private to
/// that pipeline.
let quotaOf snapshot =
    let { Memo = memo } = decideOn snapshot
    memo.HaulerQuota

/// The haul this ColonyView prices, summed over its source and mineral
/// containers — the numbers the quota above divides, the mine's apart. A rock's own rate lives here,
/// where the shaping rules the quota carries (#279's floor for a haul that
/// crosses a Seam) cannot reach it, so a case about the rate reads this and a
/// case about the crowd reads the quota.
let haulDemandOf snapshot =
    let { Quotas = quotas } = decideOn snapshot
    quotas.HaulerDemand |> List.sumBy (fun row -> row.Demand)

/// The W12S28 shape: a 3-wide plain field y = 9..11 from x = 8
/// to 32, two sources embedded in wall at (10,10) and (30,10) with their
/// built containers on the Seats (11,10) and (29,10) — two Posts — and the
/// spawn structure at (20,10), eight steps from either
/// container.
let incomeRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "src-b", { X = 30; Y = 10 }
              "can-a", { X = 11; Y = 10 }
              "can-b", { X = 29; Y = 10 }
              "spawn-1", { X = 20; Y = 10 }
          ]
          [
              for x in 8..32 do
                  for y in 9..11 ->
                      { X = x; Y = y }, (if (x = 10 || x = 30) && y = 10 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withObstacles [ { X = 20; Y = 10 } ]

/// The W12S28 colony: four idle spawns on the one 300-capacity bank with
/// energy to spare — restraint must come from the target, never from the
/// bank running dry.
let incomeColony =
    { bareRespawn with
        Spawns =
            [
                for i in 1..4 ->
                    { spawn with
                        Name = $"Spawn{i}"
                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                    }
            ]
        Bank = bank 1200 300
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = incomeRoom
    }

/// The income-based fleet the W12S28 shape pins at its 300 bank, and so at
/// the Anchor body that bank buys: `2W/1C/1M` digs 4 a tick, so each Post is
/// worth four and not the room's ten (#208). One Anchor per Post (2), the
/// throughput quota (1 hauler: each round trip is 16 ticks out loaded and 8
/// back empty, and ceil((24 + 24) × 4 / 200) = 1, rounded once for the
/// colony), and the income workers: 2 × 4 e/tick × 1500 = 12,000, minus the
/// anchor and hauler rows' amortization (900), over one worker's Work drain
/// × lifetime (1500) → ceil(7.4) = 8. Read at the room's rate the same
/// colony hired 19 workers and 3 haulers and left most of them idle.
let incomeFleet =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100 ]
    @ [ for i in 1..8 -> worker $"w{i}" 0 50 ]

/// The same geometry at the 1,800 bank of an RCL5 room, with four bodies'
/// worth of energy standing in it. The bank makes every row's body the
/// largest the rule gives: Anchor `6W/1C/1M` = 700 digging twelve, hauler
/// 24C/12M = 1,800 carrying 1,200, worker `9W/9C/9M` = 1,800 at a Work drain
/// of nine. The Anchor's twelve is over the ten an owned rock pays, so the
/// cap on a Post's worth is not binding: the rule is a cap and not a
/// discount.
let richestIncomeColony =
    { incomeColony with
        Bank = bank 7200 1800
    }

/// That bank's fleet at a given worker count: 2 Anchors, the one hauler
/// 0.4 of a body rounds up to, and the workers the case is pinning.
let richestIncomeFleet workers =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100 ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

/// A rock in the middle of a three-tile field, placed wherever a test puts
/// it: three Seats, all Plain, and nothing else within reach of it. The
/// field is written relative to the rock so the same shape can be dropped
/// into the home room and into an outpost, and the only difference between
/// the two fixtures is which room's layer it lands in.
let threeSeatField (rock: Pos) =
    [
        { rock with X = rock.X - 1 }, Plain
        { rock with X = rock.X + 1 }, Plain
        { rock with Y = rock.Y - 1 }, Plain
    ]

/// The W12S28 colony at its whole target with one more source somewhere:
/// the fleet already matches, so any Seat the target counts on top shows
/// up as a spawn Intent and nothing else can.
let incomeColonyPlus (place: ColonyView -> ColonyView) =
    { incomeColony with
        Creeps = incomeFleet
        Sources = incomeColony.Sources @ [ source "src-out" ]
    }
    |> place

/// The W12S28 shape at the bank the cases below read it at: 600 rather
/// than the 300 `incomeColony` banks (#208). What a Post is worth is what
/// the Anchor row's cast digs, capped at its room's rate, and at 300 the
/// row casts `2W/1C/1M` and digs four — under the neutral five as well as
/// the held ten, so every rate this list is about would price alike and
/// each pairwise case below would be comparing a number with itself. At
/// 600 the row casts five Work against a held rock and three against a
/// neutral one (`sourceOutputOf`): the cap binds on neither and the rate
/// is the answer, which is what these cases exist to read.
let midIncomeColony =
    { incomeColony with
        Bank = bank 2400 600
    }

/// That colony's fleet at a given hauler and worker count: one Anchor per
/// home Post — the one row no case below moves — beside the two rows that
/// do. The hauler row is a parameter because it falls with the home
/// room's output — two bodies held against one at the neutral rate — so
/// a case that neutralises the spawn room and leaves the held count
/// standing is reading a fleet a body above the quota it says it is sized
/// to.
let incomeFleetRows haulers workers =
    [ anchor "a1" 0 50; anchor "a2" 0 50 ]
    @ [ for i in 1..haulers -> hauler $"h{i}" 0 100 ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

/// The same fleet at the held home room's hauler quota of two, which is
/// every case whose home room stays this colony's own — the 600 bank's
/// hauler carries 400 and the pair of containers asks 1.2 of a body.
let incomeFleetOf workers = incomeFleetRows 2 workers

/// The W12S28 colony with a posted outpost source beside it: the same rock
/// in the same three-Seat field, with a container standing on one of its
/// Seats. The fleet is the caller's, and so is who holds W1N2; everything
/// else is `incomeColony`, unmoved.
///
/// Its hauler quota is the home room's either way: W1N2 arrives with no
/// border ring, so the haul has no price and the container hires nobody
/// (`outpostHaulTests` lays the rings). Its Anchor row is three, one per
/// Post across every projected room. Which leaves the income base as the
/// one addend a reservation moves, and a worker count as the whole reading
/// of it.
let postedOutpostColony workers (control: (string * RoomControlInfo) list) =
    let rock = { X = 40; Y = 40 }

    let colony =
        incomeColonyPlus (
            withOutpost
                "W1N2"
                [
                    "src-out", rock, Source
                    "can-out", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
                ]
                (threeSeatField rock)
        )

    { colony with
        Creeps = incomeFleetOf workers @ [ anchor "a-out" 0 50 ]
        Bank = midIncomeColony.Bank
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

/// The Anchor bodies the tick casts, in casting order, off `decide`'s own
/// Intents. The row is read off the creep name the casting step stamps, so
/// a tick that cast some other row shows as an empty list here rather than
/// quietly asserting about a worker.
let anchorCastsBy colony =
    spawnIntents (decideOn colony).Intents
    |> List.filter (fun (_, _, name) -> name.StartsWith "anchor-")
    |> List.map (fun (_, body, _) -> body)

/// The anchor row's body under either of the two ceilings a reservation
/// decides: five Work saturate a held rock and two a rock nobody holds, so
/// the ceilings are six and three. Written once because the cast, the
/// charge and the lead are all read against them.
let sixWork = [ Work; Work; Work; Work; Work; Work; Carry; Move ]

let threeWork = [ Work; Work; Work; Carry; Move ]

/// The Reach of one tick, read at the seam its three readers share: this is
/// that derivation, not a second one. The home room's share, since these
/// colonies stand their hostiles at home.
let reachIn snapshot =
    Threats.reachIn
        (threatsOf snapshot (Atlas.ofView snapshot))
        (SpatialInfo.homeName snapshot.Spatial)

/// The open colony facing one hostile of the given body on the given tile.
let facingBody pos body =
    atLevel 2 (openRoom 8) |> facing [ hostileAt "h-1" pos body ]

/// A plain corridor down one column, the shape a two-room fixture needs
/// twice: geometry a reader can count steps along, in a room the flood
/// must not leave.
let corridor x y0 y1 =
    [ for y in y0..y1 -> { X = x; Y = y }, Plain ]

/// A plain border ring. A projection without one answers an empty band and
/// prices no crossing at all, which is what the two fixtures above rest on
/// and what the ones below must not. Plain the whole way round, so no
/// crossing is picked out by its terrain.
let plainRing =
    Map.ofList
        [
            for x in 0..49 do
                for y in 0..49 do
                    if x = 0 || x = 49 || y = 0 || y = 49 then
                        { X = x; Y = y }, Plain
        ]

/// The home half of the fixtures below: the corridor running down from the
/// north border, one worker at (10,2) a step inside it, and one source
/// wherever the test puts it. No controller, no refillable with room and
/// no store, so the whole Task pool is the sources — which is what lets a
/// Matched Verdict's factor name the one comparison that separated them
/// rather than report on some third candidate.
let northBorderColony (homeSource: Pos) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-home" ]
        Creeps = [ worker "w" 0 50 ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing ]
                TargetKinds = Map.ofList [ "src-home", Source ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (corridor 10 1 40)
                    TargetPositions = Map.ofList [ "src-home", homeSource ]
                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 2 } ]
                })
    }

/// The same colony with its outpost beside it, one room north: W1N2's
/// y = 49 row lands on W1N1's y = 0 row, and `Atlas.seams` reads that join
/// out of the two room names alone, so no fixture here declares an edge.
/// The outpost's corridor runs to its own y = 48, so the tile a crossing
/// lands a creep on opens onto ground.
///
/// `None` is the room before anything is laid into it: the whole of what
/// `World.factsOf` builds for a room with no vision, terrain and border
/// ring, and not one entry more. `Outpost.place` lays the declared sources
/// and controller in afterwards, so this is the baseline the declaration is
/// added to and never a blind outpost as the colony really projects one.
let withNorthOutpost (outpostSource: Pos option) (colony: ColonyView) =
    { colony with
        Sources = colony.Sources @ [ for _ in Option.toList outpostSource -> source "src-out" ]
        Spatial =
            { colony.Spatial with
                Borders = Map.add "W1N2" plainRing colony.Spatial.Borders
                TargetKinds =
                    match outpostSource with
                    | Some _ -> Map.add "src-out" Source colony.Spatial.TargetKinds
                    | None -> colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                    TargetPositions =
                        outpostSource
                        |> Option.map (fun pos -> Map.ofList [ "src-out", pos ])
                        |> Option.defaultValue Map.empty
                }
    }

/// The sites standing in the outpost, as many as the caller names, each under
/// its own id: the container rule is the only thing this colony ever places
/// out there, so every site of another kind laid here is a human's hand.
/// Each arrives in the three pieces the shell hands Core a site in: the
/// id-keyed kind census, the outpost layer's own tile, and the
/// `ConstructionSites` entry vision pays for. Merges into whatever layer
/// `withNorthOutpost` already laid, so the two compose in either order.
let withOutpostTrunk (sites: (string * BuiltKind * Pos) list) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        ConstructionSites =
            colony.ConstructionSites
            @ [ for id, _, _ in sites -> { Id = id; Left = siteOwes } ]
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    (colony.Spatial.TargetKinds, sites)
                    ||> List.fold (fun kinds (id, kind, _) -> Map.add id (Site kind) kinds)
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions =
                        (outpost.TargetPositions, sites)
                        ||> List.fold (fun tiles (id, _, pos) -> Map.add id pos tiles)
                }
    }

/// One site out there under the frozen id every case that wants a single one
/// names: `withOutpostTrunk`'s one-site case. The kind is a parameter because
/// a Seat's *container* site is a Post and reopens Build to the body standing
/// on it, where a site of any other kind is ordinary surplus work.
let withOutpostSiteOf (kind: BuiltKind) (site: Pos) (colony: ColonyView) =
    withOutpostTrunk [ "site-out", kind, site ] colony

/// The container site the outpost rule really places — the kind every case
/// but #205's pairwise ones wants.
let withOutpostSite (site: Pos) (colony: ColonyView) =
    withOutpostSiteOf BuiltKind.Container site colony

/// The same worker, carrying a full load: Harvest asks for free capacity
/// and Build asks for carried energy (`applicable`), so a full worker
/// leaves the home source's Task inapplicable and is matched over a pool
/// whose one candidate is the Build — pairwise by construction, with no
/// third rival standing in for either side of a comparison.
let loaded (colony: ColonyView) =
    { colony with
        Creeps = [ worker "w" 50 0 ]
    }

/// Three loaded workers standing in the home corridor, a step apart: the row
/// the outpost builders' budget rations, and one more body than the shipped
/// budget of two, so what the budget refuses is read off the one left over.
let threeLoadedAtHome (colony: ColonyView) =
    { colony with
        Creeps = [ for name in [ "w1"; "w2"; "w3" ] -> worker name 50 0 ]
        Spatial =
            colony.Spatial
            |> withCreepsAt [ for i in 1..3 -> $"w{i}", { X = 10; Y = i + 1 } ]
    }

/// What each Task in the colony holds this tick, by Task — the whole tally, so
/// a budget that admitted one body too many or one too few fails either way.
let heldBy (colony: ColonyView) =
    let { Assignments = assignments } = decideOn colony

    assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort

/// What the tick decided, less the plan memo: the memo carries a mutable
/// walk table whose identity is not a decision, and these three are the
/// whole of what leaves the colony.
let outcomeOf (colony: ColonyView) =
    let decision = decideOn colony
    decision.Intents, decision.Assignments, decision.Verdicts

/// Which Task won the one worker, and what separated it from its closest
/// rival, off the Matched Verdict.
let matchOf (colony: ColonyView) =
    let { Verdicts = verdicts } = decideOn colony

    verdicts
    |> List.tryPick (function
        | Verdict.Matched("w", task, factor) -> Some(task, factor)
        | _ -> None)

/// The same colony with its own controller standing where the caller puts
/// it: the rival every loaded worker in the real colony always has, and
/// the one the fixtures above leave out so that their Matched factor can
/// name a single comparison. Level 2 and far from its downgrade deadline,
/// so nothing here is the deadline rank in disguise.
let withHomeController (pos: Pos) (colony: ColonyView) =
    { colony with
        Controller = Some(controllerAt 2)
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctrl-1" TargetKind.Controller colony.Spatial.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "ctrl-1" pos layer.TargetPositions
                })
    }

/// The same colony with one hungry extension of ours standing where the
/// caller puts it: the Feeding-tier rival a home site's Build is measured
/// against. The controller `withHomeController` adds cannot do that work
/// for a home site: such a site outranks the Upgrade beside it by a rung
/// (`isHomeSite`), so the factor stops naming the tier. A site lifted onto
/// the flow *ties* this Refill and the nearer wins, while a surplus one is
/// outranked by it however near it stands; place it farther than the site
/// and the two readings differ in the winner. A site past the Seam needs
/// none of this: the rung stops at the home room.
let withHungryExtension (pos: Pos) (colony: ColonyView) =
    { colony with
        Refillables = colony.Refillables @ [ refillable "ext-1" 50 BuiltKind.Extension ]
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "ext-1" (Structure BuiltKind.Extension) colony.Spatial.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "ext-1" pos layer.TargetPositions
                })
    }

/// The names of the bodies a tick casts, in the order the colony emits
/// them — the row a spawn Intent came from, which is the whole of what the
/// cases below read.
let castNames (colony: ColonyView) =
    spawnIntents (decideOn colony).Intents |> List.map (fun (_, _, name) -> name)

/// The switch's home half: the same corridor down column 25 of W1N1 with
/// the spawn at (25,10), and one home source in the rock beside it at
/// (24,20) with its container standing on the Seat (25,20). A home Post,
/// a home hauler term and a home income share, so this colony is already
/// running an economy well clear of `Tuning.MinWorkforce` before the outpost
/// is asked to add anything to it — which is what lets the pair below read a
/// *difference* rather than a floor.
let switchHome =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-home" ]
        Bank = bank 300 300
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-home", Source
                            "can-home", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (({ X = 24; Y = 20 }, Wall) :: corridor 25 1 48)
                    TargetPositions =
                        Map.ofList
                            [
                                "spawn-1", { X = 25; Y = 10 }
                                "src-home", { X = 24; Y = 20 }
                                "can-home", { X = 25; Y = 20 }
                            ]
                    Obstacles = Set.singleton { X = 25; Y = 10 }
                })
    }

/// The same colony with the outpost's rock declared one room north and
/// reserved — projected, priced, and with nothing built on it. This is the
/// tick before the switch: the Harvest is pooled, the room is held, and
/// every quota still reads the home room alone.
let switchUnposted =
    { switchHome with
        Sources = switchHome.Sources @ [ source "src-out" ]
        RoomControl = Map.add "W1N2" (reservedRoom true 4000) switchHome.RoomControl
        Spatial =
            { switchHome.Spatial with
                TargetKinds = switchHome.Spatial.TargetKinds |> Map.add "src-out" Source
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 25 41 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
                }
    }

/// And the tick the switch closes: the container standing on the outpost
/// rock's one Seat, and nothing else in the world different.
let switchPosted =
    let outpost = SpatialInfo.layerOf switchUnposted.Spatial "W1N2"

    { switchUnposted with
        Spatial =
            { switchUnposted.Spatial with
                TargetKinds =
                    switchUnposted.Spatial.TargetKinds
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions =
                        outpost.TargetPositions |> Map.add "can-out" { X = 25; Y = 41 }
                }
    }

/// The home room's whole target, one row at a time: its one Post's Anchor,
/// the one hauler its container's round trip hires, and the four workers
/// the Post's output feeds once those are amortized —
/// ceil((4 × 1500 − 2 × 300) / 1500) = 4. Four a tick and not ten because
/// this fixture banks 300: the Anchor row casts `2W/1C/1M` there, and a
/// Post is worth what its garrison digs under the rock's own rate.
let switchHomeFleet =
    [ anchor "a-home" 0 50; hauler "h-home1" 0 100 ]
    @ [ for i in 1..4 -> worker $"w{i}" 0 50 ]

/// What the outpost's container adds, and nothing else: one Anchor for the
/// Post it makes, the one hauler its own round trip across the Seam adds,
/// and its income share — the worker row goes from four to eight, since
/// ceil((8 × 1500 − 4 × 300) / 1500) = 8. One hauler and not two because
/// the pool is rounded once: the home container's 0.54 of a body and the
/// outpost's 1.02 come to 1.56 and hire two, where a ceiling apiece hired
/// one and two.
let switchOutpostRows =
    [ anchor "a-out" 0 50; hauler "h-out1" 0 100 ]
    @ [ for i in 5..8 -> worker $"w{i}" 0 50 ]

/// A hostile filed under the room it stands in — the field `facing`
/// stamps with the home name, set by hand here because these fixtures
/// put a hostile in either room.
let hostileIn room pos body =
    { hostileAt "h-1" pos body with
        Pos = RoomPos.at room pos
    }

/// The engine's own `smallMelee`: two TOUGH, five MOVE, a RANGED_ATTACK, a
/// WORK and an ATTACK — 1,000 hits and 40 damage at range 1, and the body
/// nine remote raids in ten arrive as
/// (`docs/research/remote-invader-defence.md`). Written part for part
/// because the parts are what every rule reads: the ATTACK makes it a
/// threat, the RANGED_ATTACK sets its reach at 3 plus the margin, and it
/// carries no HEAL, which keeps the guard row's count at one against it.
let smallMelee =
    [ Tough; Tough; Move; Move; Move; Move; RangedAttack; Work; Attack; Move ]

/// The engine's own `smallHealer`: five MOVE and five HEAL, 60 hits a tick at
/// range 1 unboosted. No ATTACK and no RANGED_ATTACK, so it is a hostile the
/// raid log records and no threat at all, and it is exactly what the guard
/// row's count rule prices: one 750-energy guard's 90 damage stands against
/// one of these and loses to two.
let smallHealer = [ Move; Move; Move; Move; Move; Heal; Heal; Heal; Heal; Heal ]

/// The ranged half of a two-creep raid, as the engine casts it: two TOUGH,
/// five MOVE and three RANGED_ATTACK — thirty a tick at range 1..3, and no
/// ATTACK part, so it is the one that kites rather than closing. The body
/// W13S29 took beside a `smallMelee` on 2026-09-08, which is the raid #280's
/// count rule is written against.
let smallRanged =
    [
        Tough
        Tough
        Move
        Move
        Move
        Move
        Move
        RangedAttack
        RangedAttack
        RangedAttack
    ]

/// One guard as the row would really cast it at an 800 bank: `[T; A×3; M×5; H]`,
/// 90 damage and 12 self-heal a tick. Sized through `bodyFor` rather than
/// written out, so the damage the count rule reads off it is the damage the row
/// bought and never a second spelling of it — and so that a row which stopped
/// casting an ATTACK part would stop standing a `Fighter` up here too.
let guard name =
    creepWith name 0 0 (bodyFor guardPattern 800)

/// The outpost the Reserve tests hold: one room across the north border,
/// its controller declared under the engine's own id and laid into the
/// projection the way the shell lays one (`Outpost.place`) — so it is in
/// the pool with no vision at all, and its tile is an obstacle, which puts
/// the reserver beside the controller and never on it.
///
/// No rock declared. The pool these fixtures want is the one Reserve and
/// nothing else, because the Matcher scores a winner against its cheapest
/// rival alone: a second candidate would leave every comparison below
/// reporting on some third Task.
let reserveDeclaration =
    {
        RoomName = "W1N2"
        Sources = []
        // Off the corridor on purpose: a controller stands in `Obstacles`
        // whether vision found it or a declaration laid it, and one on the
        // corridor would seal the room rather than leave the tiles beside
        // it to stand on.
        Controller = "ctrl-out", { Room = "W1N2"; X = 11; Y = 44 }
    }

/// The colony the Reserve tests run in: the home corridor with no Task in
/// it at all, the declared outpost across the border, and the creeps the
/// test names standing in that outpost, filed under its own layer.
let reserveColony (creeps: (CreepInfo * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None
    let spatial = Outpost.place [ reserveDeclaration ] colony.Spatial
    let outpost = SpatialInfo.layerOf spatial "W1N2"

    { colony with
        Sources = []
        Creeps = creeps |> List.map fst
        Spatial =
            spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions =
                        creeps |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// The reserver: two CLAIM parts and two Move, 1,300 energy.
/// Carrying nothing and with nowhere to put anything — a CLAIM body has no
/// Carry part — so no gate below can be passing on an energy state.
let reserver name =
    creepWith name 0 0 [ BodyPart.Claim; BodyPart.Claim; Move; Move ]

let reserveTasks tasks =
    tasks
    |> List.choose (function
        | Reserve controllerId -> Some controllerId
        | _ -> None)

/// And the tick after: the same declaration, the same room, ours now. One
/// fact apart from `asCandidate` — who holds the controller — which is the
/// whole of what turns a candidate into a nursery, there being no spawn of
/// ours in that room until `withNorthSpawn` puts one there.
let asNursery (colony: ColonyView) =
    { colony with
        RoomControl = Map.add "W1N2" ownedRoom colony.RoomControl
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
        // The shell borrows a nursery for the mother the tick it is
        // claimed; the pool's budget reads that field.
        Borrowed = { Rooms = [ "W1N2" ] }
        // Declared and owned with no spawn of ours standing in it is the
        // whole of `Nursery`, so the shell would derive exactly this entry.
        Stages = Map.add "W1N2" Nursery colony.Stages
    }

/// One outpost's furniture in a layer of its own: the rock in a
/// three-Seat field, a controller — what a CLAIM body walks to, and what
/// pools the Reserve Task (#130) — and, when `posted`, a built container
/// on one of those Seats: the switch that makes the rock a Post and
/// admits the room to the economy (#129). Ids carry the room's name, so
/// two outposts stand in one projection without colliding.
let withOutpostRoom room (rock: Pos) posted (colony: ColonyView) =
    let container =
        if posted then
            [ $"can-{room}", { rock with X = rock.X - 1 }, Structure BuiltKind.Container ]
        else
            []

    { colony with
        Sources = colony.Sources @ [ source $"src-{room}" ]
    }
    |> withOutpost
        room
        ([
            $"src-{room}", rock, Source
            $"ctrl-{room}", { rock with Y = rock.Y + 2 }, Controller
         ]
         @ container)
        (threeSeatField rock)

/// The reserver row's colony: the W12S28 shape at the live RCL5 bank of
/// 1,800 — the level `[2Claim;2Move]` is priced against — with the named
/// outposts standing beside it and the named holder on each room.
/// Everything else is `incomeColony`, unmoved. The bank holds 8,000 against
/// that capacity: restraint in these cases must come from the rows, never
/// from the bank running dry.
let reserverColony outposts creeps control =
    let colony =
        ({ incomeColony with
            Bank = bank 8000 1800
            Creeps = creeps
         },
         outposts)
        ||> List.fold (fun acc (room, rock, posted) -> withOutpostRoom room rock posted acc)

    { colony with
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

/// The north outpost and the west one, diagonal to each other as W12S27
/// and W13S28 are — two rooms, so "one reserver per declared outpost" can
/// be told apart from "one reserver".
let northOutpost posted = "W1N2", { X = 40; Y = 40 }, posted
let westOutpost posted = "W2N2", { X = 20; Y = 40 }, posted

/// A fleet standing over every row's quota but the reserver's: one Anchor
/// per Post — the home room's two plus one for each posted outpost — four
/// haulers where the row wants two, and forty workers where the income
/// base hires a handful. Every other gap is therefore zero and the
/// whole-fleet deficit is negative, so a `SpawnCreep` in these cases is a
/// reserver or it is a defect.
let surplusFleet posts =
    [ for i in 1..posts -> anchor $"a{i}" 0 50 ]
    @ [ for i in 1..4 -> hauler $"h{i}" 0 100 ]
    @ [ for i in 1..40 -> worker $"w{i}" 0 50 ]

/// The bodies of this tick's reserver casts, in casting order.
let reserverCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "reserver-")
    |> List.map (fun (_, body, _) -> body)

/// The one block the row never casts below, and the body a reservation
/// standing at its 5,000 cap asks for: the deficit is zero and the floor
/// is one.
let oneBlock = [ BodyPart.Claim; Move ]

/// The errand the suites in this directory run: W1N2, and the sector Reactor
/// standing at (25,44) in it under the engine id a declaration names. One
/// crossing rather than the live three, because no case in `Decide` is
/// about the walk — `RoomSeamTests` prices that over the committed captures.
/// One spelling for the two domains that read it: `ErrandTests` asks what
/// the declaration *buys* and `QuotaReserverTests` what it *costs*.
let reactorErrand: Errand =
    {
        RoomName = "W1N2"
        Target = "reactor-1", RoomPos.at "W1N2" { X = 25; Y = 44 }
        Held = false
    }

/// The declared target's id and tile, and the ring tile due south of it that a
/// re-claimer makes its act from — pulled out because a case that names one
/// names the others.
let reactorId = fst reactorErrand.Target
let reactorTile = { X = 25; Y = 44 }
let reactorRing = { X = 25; Y = 43 }

/// A plain floor around the reactor, wide enough that its range-1 ring is
/// walkable and that a body three tiles off has somewhere to stand and a walk
/// to price.
let private errandFloor =
    [
        for x in 22..28 do
            for y in 40..47 do
                { X = x; Y = y }, Plain
    ]

/// The colony with that errand declared and its room's floor laid, the target
/// placed kind-less the way `Errand.place` places it, so no pool that sweeps
/// a kind enumerates it. Nothing else of the programme stands: the claim is
/// the first act of the delivery and not its last. Merges into whatever
/// layer that room already carries, so bodies may be stood in it before or
/// after.
///
/// On the floor it was first written with, which cannot price its crossing
/// (#379): asked for through `bareDeliveryColony` below by the one case
/// whose subject is what a rule does with no price at all.
let internal withBareReactorErrand (colony: ColonyView) =
    let existing = SpatialInfo.layerOf colony.Spatial reactorErrand.RoomName

    { colony with
        Errands = [ reactorErrand ]
        Consignee = None
        Crossed = Set.empty
        Spatial =
            colony.Spatial
            |> withNeighbour
                reactorErrand.RoomName
                { existing with
                    Terrain = TerrainGrid.ofList errandFloor
                    TargetPositions = Map.add reactorId reactorTile existing.TargetPositions
                }
    }

/// A whole room of plain ground, which is what a crossing needs on **both**
/// sides of it (#379).
let private wholeFloor =
    [
        for x in 1..48 do
            for y in 1..48 -> { X = x; Y = y }, Plain
    ]

/// The same declaration with its one crossing priceable (#379). The bare
/// fixture cannot price a cross-room walk for two reasons at once: its
/// errand floor stops at y 47 and its home floor is a corridor, so neither
/// side of the crossing has ground behind the landing tile and
/// `Atlas.routes` answers `[]`; and the `spatial` funnel files home under
/// the empty name, which has no sector coordinates to be adjacent by. Every
/// cross-room price out of it is therefore `None` and every rule that reads
/// one takes its permissive branch, so three suites were exercising the
/// delivery on the branch where the walk has no price.
///
/// Lays its ground over whatever was there: the home layer's terrain and
/// both rooms' borders are replaced rather than merged, so a case that wants
/// walls at home lays them after this.
let private priced (colony: ColonyView) =
    let home = SpatialInfo.homeName colony.Spatial
    let errandLayer = SpatialInfo.layerOf colony.Spatial reactorErrand.RoomName

    // A home that already has a name keeps it: a suite that named its own
    // said something by naming it — `ObserveTests` puts the home three rooms
    // out so the alarm's lead is the live route's — and renaming it here would
    // answer a question nobody asked. What this supplies is the name the
    // `spatial` funnel leaves **empty**, which is the half of the unpriceable
    // fixture that no amount of ground can fix.
    let named = if home = "" then "W1N1" else home

    { colony with
        Spatial =
            { colony.Spatial with
                RoomName = Some named
                Rooms =
                    colony.Spatial.Rooms
                    |> Map.remove home
                    |> Map.add named (SpatialInfo.layerOf colony.Spatial home)
                Borders =
                    colony.Spatial.Borders
                    |> Map.add named plainRing
                    |> Map.add reactorErrand.RoomName plainRing
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList wholeFloor
                })
            |> withNeighbour
                reactorErrand.RoomName
                { errandLayer with
                    Terrain = TerrainGrid.ofList wholeFloor
                }
    }

/// The declaration as the suites read it: priceable, because a delivery whose
/// leg has no price is a delivery none of its rules are really being asked
/// about (#379).
let withReactorErrand (colony: ColonyView) =
    colony |> withBareReactorErrand |> priced

/// Our own bodies standing in the errand room, filed into its layer: a
/// re-claimer the projection places nowhere holds no Task and makes no act.
let standingInErrand (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial reactorErrand.RoomName

    { colony with
        Creeps = colony.Creeps @ (ours |> List.map fst)
        Spatial =
            colony.Spatial
            |> withNeighbour
                reactorErrand.RoomName
                { layer with
                    CreepPositions =
                        ours |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// And whose the declared target is this tick, which is the fact the
/// reclaim's act is gated on. `None` leaves the entry out altogether, which
/// is what a gapped relay reads and what the act treats as *not ours*.
let withReactorOwner owner (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Owners =
                    match owner with
                    | Some who -> Map.add reactorId who colony.Spatial.Owners
                    | None -> colony.Spatial.Owners
            }
        // The Reactor's own row travels with its ownership, because in the
        // shell both come off one `FIND_REACTORS` sweep of one object
        // (`World.reactorFacts`): vision gives ownership *and* store together,
        // or gives neither. A fixture that could carry one without the other
        // would let a rule read a store in a room this colony cannot see.
        Reactors =
            match owner with
            | Some who ->
                [
                    {
                        Id = reactorId
                        Owner =
                            if who = Ownership.Ours then
                                ReactorOwner.Ours
                            else
                                ReactorOwner.Rival "somebody"
                        Thorium = 0
                        ContinuousWork = 0
                    }
                ]
            | None -> []
    }

/// The delivering colony: `mineHaulColony`'s mine and Storage with the sector
/// Reactor declared as an errand, a re-claimer standing on its ring and a
/// bank that can buy a courier. Shared because `ErrandTests` and the
/// projection sweep both read it.
///
/// The Reactor's own store is not written into `Spatial.Thorium`: the sweep
/// files it in `RoomFacts.Reactors` and gives the object neither a tile nor a
/// kind, so an entry here would be the shape that cost #354 its 915 T (the
/// gate read this map and the projection answered 0 for a store holding
/// 999). `withReactorOwner` stands the row it really rides.
let private deliveryColonyWith declare owner =
    let resident = creepWith "relay" 0 0 [ BodyPart.Claim; Move ]

    let colony =
        { mineHaulColony with
            Bank = bank 2300 2300
            Spatial =
                { mineHaulColony.Spatial with
                    Thorium = mineHaulColony.Spatial.Thorium |> Map.add "sto-1" 2997
                }
        }

    colony
    |> declare
    |> withReactorOwner owner
    |> standingInErrand [ resident, reactorRing ]

/// The delivering colony with its crossing priceable, which is what the cases
/// about the delivery want (#379).
let deliveryColony owner =
    deliveryColonyWith withReactorErrand owner

/// And the same colony on the unpriceable floor, for the cases whose subject
/// is what a rule does when the walk has no price at all.
let bareDeliveryColony owner =
    deliveryColonyWith withBareReactorErrand owner

/// What the declared Reactor's store holds, set on the Reactor's row and not
/// in `SpatialInfo.Thorium` (see `deliveryColonyWith`). Beside
/// `withReactorOwner` and never instead of it: no vision, no row.
let withReactorStore (held: int) (colony: ColonyView) =
    { colony with
        Reactors =
            colony.Reactors
            |> List.map (fun reactor ->
                if reactor.Id = reactorId then
                    { reactor with Thorium = held }
                else
                    reactor)
    }

/// Ore banked in a home Terminal, so a declared errand is fuelled (#420)
/// whoever holds the Reactor. A Terminal and not a Storage: the delivery
/// draws from a Storage alone, so this opens no courier programme.
let withBankedOre (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "term-ore" (Structure BuiltKind.Terminal) colony.Spatial.TargetKinds
                Thorium = Map.add "term-ore" 1_000 colony.Spatial.Thorium
            }
    }

/// The same ore taken away again.
let withoutBankedOre (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.remove "term-ore" colony.Spatial.TargetKinds
                Thorium = Map.remove "term-ore" colony.Spatial.Thorium
            }
    }

/// The declared Reactor ours and burning, so the errand is fuelled (#420)
/// without a Storage whose ore would open the courier programme.
let withBurningReactor (colony: ColonyView) =
    colony |> withReactorOwner (Some Ownership.Ours) |> withReactorStore 100

/// The buffer lane: a plain corridor three rows deep, the controller
/// standing at (10,10) as an obstacle, and its upgrade buffer "can-buf" at
/// (13,10), the outermost tile of the controller's Upgrade Work Area.
///
/// A creep beside the buffer at (14,10) stands one step outside that Work
/// Area and inside the Work Area of anything at (15,10), which is where
/// each case puts its delivery. So Upgrade costs a step and the delivery
/// costs nothing, the two share the Surplus tier, and what separates them
/// is the gate and nothing else.
let bufferLaneField =
    [
        for x in 5..20 do
            for y in 9..11 -> { X = x; Y = y }, Plain
    ]

let bufferLane =
    { spatial [] bufferLaneField with
        Stores = Map.ofList [ "can-buf", 400 ]
    }
    |> withTargets
        [
            "ctrl-1", { X = 10; Y = 10 }, Controller
            "can-buf", { X = 13; Y = 10 }, Structure BuiltKind.Container
        ]
    |> withObstacles [ { X = 10; Y = 10 } ]

/// The lane with one creep on the buffer's doorstep and the given furniture
/// wherever the case wants it — (15,10), a step out, for the gate's own
/// cases. No source, no hungry spawn and no Storage, so the pool is exactly
/// the controller's Upgrade, the buffer's own Withdraw and Refill, and
/// whatever the case stands beside the creep. The bank is the live RCL5
/// 1,800, which is the capacity both bodies below are cast at.
let bufferLaneColony furniture sites creep =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Refillables = []
        Controller = Some(controllerAt 2)
        ConstructionSites = sites
        Creeps = [ creep ]
        Spatial =
            bufferLane
            |> withTargets furniture
            |> withCreepsAt [ (creep: CreepInfo).Name, { X = 14; Y = 10 } ]
    }

/// The same lane with the flow standing in it: a spawn of the colony's own
/// at (19,9), hungry, five steps from the creep and so the **farther** of
/// the two deliveries. It is the Feeding-tier rival a site in this lane is
/// measured against since #234 — `withHungryExtension`'s reason, in the one
/// fixture that has a controller of its own and so no room for an extension
/// beside it. A site read onto the flow ties this Refill and wins on price;
/// a surplus one is outranked by it.
let bufferLaneFlow furniture sites creep =
    let colony =
        bufferLaneColony
            (("spawn-1", { X = 19; Y = 9 }, Structure BuiltKind.Spawn) :: furniture)
            sites
            creep

    { colony with
        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
    }

/// A mother one room north of a child of hers, at whichever [[stage]] the
/// case wants. Her Storage stands on the lane down to the border and the
/// child's controller a few tiles into W1N2, so the [[ferry]]'s round trip
/// is priceable — and nothing else in this colony hauls anything: no
/// spawn, no rock and no container of her own, so the quota this fixture
/// answers with is the ferry term alone.
let ferryMother stage =
    { bareRespawn with
        Spawns = []
        Controller = Some(controllerAt 5)
        Refillables = []
        Sources = []
        Bank = bank 1800 1800
        RoomControl = Map.ofList [ "W1N1", ownedRoom; "W1N2", ownedRoom ]
        Declared = [ "W1N1"; "W1N2" ]
        Stages = Map.ofList [ "W1N1", Independent; "W1N2", stage ]
        Borrowed = { Rooms = [ "W1N2" ] }
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "storage-1", Structure BuiltKind.Storage
                            "ctrl-child", Controller
                            // The child's upgrade buffer, beside its
                            // controller: the tile the [[ferry]] is priced
                            // to and the store its Refill is pooled for
                            // (#222) — one store for both halves of the
                            // lend, so a body hired here has a Task.
                            "can-child", Structure BuiltKind.Container
                        ]
                Stores = Map.ofList [ "can-child", 500 ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 9..11 do
                                    for y in 1..10 -> { X = x; Y = y }, Plain
                            ]
                    TargetPositions = Map.ofList [ "storage-1", { X = 10; Y = 6 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 9..11 do
                                    for y in 44..48 -> { X = x; Y = y }, Plain
                            ]
                    TargetPositions =
                        Map.ofList
                            [ "ctrl-child", { X = 10; Y = 46 }; "can-child", { X = 10; Y = 45 } ]
                }
    }

/// The row each of this tick's casts was bought for, in casting order —
/// the row name the caster writes into the creep's name.
let castRows intents =
    spawnIntents intents
    |> List.map (fun (_, _, name: string) -> (name: string).Split('-') |> Array.head)

/// Three loaded workers standing in the home corridor with one container site
/// waiting across the north border, and the home room holding nothing to build.
/// What the three of them are given is therefore a question about the outpost's
/// site alone, which is what the concurrent-builder budget, the declaration and
/// the tuning cases each ask of this shape from their own side.
let crowdAtOutpostSite (colony: ColonyView) =
    let colony =
        colony
        |> withNorthOutpost None
        |> withOutpostSite { X = 10; Y = 43 }
        |> withHomeController { X = 10; Y = 5 }

    { colony with
        Creeps = [ for name in [ "w1"; "w2"; "w3" ] -> worker name 50 0 ]
        Spatial =
            colony.Spatial
            |> withCreepsAt
                [ "w1", { X = 10; Y = 2 }; "w2", { X = 10; Y = 3 }; "w3", { X = 10; Y = 4 } ]
    }

/// A pile of ore on the floor of `room`, wherever that room is in this
/// colony's projection (#360). Named `-In` because the room is the whole point:
/// the same pile is a leak this colony answers for or a stranger's floor
/// depending only on which room it lies in and what the projection says about
/// that room.
let withPileIn room id amount (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id (Dropped Thorium) colony.Spatial.TargetKinds
                Thorium = Map.add id amount colony.Spatial.Thorium
            }
            |> withNeighbour
                room
                { layer with
                    TargetPositions = Map.add id { X = 25; Y = 25 } layer.TargetPositions
                }
    }

/// The same ore one object over: a tombstone holding it, which is what a
/// courier that dies on the loaded leg leaves behind (#359, #360).
let withTombstoneIn room id amount (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id Tombstone colony.Spatial.TargetKinds
                Thorium = Map.add id amount colony.Spatial.Thorium
            }
            |> withNeighbour
                room
                { layer with
                    TargetPositions = Map.add id { X = 26; Y = 25 } layer.TargetPositions
                }
    }
