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

/// The colony's bank holding the given energy against the given capacity
/// (ADR 0052 decision 1): one account, its home room's, and no longer a
/// map keyed by room — every spawn a colony casts from stands in its home.
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
/// every source in it is priced at (ADR 0042). Owned and not reserved —
/// the two halves the engine gives the same 3,000 a cycle — because that
/// is what the colony's own room is.
let ownedRoom: RoomControlInfo =
    {
        Owner = Ownership.Ours
        Reservation = None
        SafeMode = false
    }

/// A room another player has taken: seen, owned, and owned by somebody
/// else (ADR 0043). The third answer to one question, which is why it is a
/// fixture of its own beside the two around it rather than a flag on
/// either — and since #165 the one control entry that **latches** a
/// [[stand-down]], where a rival's reservation runs a clock. Shared since
/// #165, when the gate's own suite came to want it beside the quota rows.
let rivalRoom: RoomControlInfo =
    {
        Owner = Ownership.Rival
        Reservation = None
        SafeMode = false
    }

/// A neutral room nobody holds: seen, and worth half. Not the same fact as
/// a room with no entry at all, which is one the colony cannot see and so
/// cannot price (ADR 0004).
let neutralRoom: RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation = None
        SafeMode = false
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
    }

/// A neutral room whose reservation the NPC Invader holds — what a
/// level-0 invader core leaves behind it when it `attackController`s a
/// room it expanded into (ADR 0043). The third holder, and a fixture of
/// its own because it prices exactly as a rival's does and, under ADR
/// 0043, withdraws on the opposite rule.
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
    }

/// The [[stage]] map of a colony that declares itself and nothing else
/// (ADR 0052 decision 3), standing at the given controller level. Keyed by
/// the room its projection is filed under, which is the name every reader
/// looks a home up by — and by that name **alone**, unlike `homeControl`
/// below: a control entry prices a room, where a stage entry says a colony
/// of ours lives there, so a second name here would be a second colony,
/// and the fixtures' home room would have a [[nursery]] beside it hiring
/// [[pioneer]]s.
///
/// Owned, with a spawn standing, because that is what `Main.loop` runs
/// `decide` for (ADR 0047 decision 1). Derived through `Colony.stageOf`
/// from the level the fixture's own controller carries, and never written
/// down beside it: a fixture that spelled a stage out could spell a colony
/// the shell can never build — an RCL5 room the roads are gated out of, an
/// RCL1 one that keeps ramparts — and every rule that used to read the
/// level reads this.
let homeStages (spatial: SpatialInfo) level =
    match Colony.stageOf Tuning.defaults true true (Some level) with
    | Some stage -> Map.ofList [ SpatialInfo.homeName spatial, stage ]
    | None -> Map.empty

/// The control map for a colony holding its own room and nothing else.
/// Both names the fixtures below file a home layer under: `SpatialInfo.empty`
/// and the `spatial` funnel leave `RoomName` unset and file under the empty
/// name, `openRoom` names the room "W1N1", and the control map is keyed by
/// room like every other room-keyed fact (ADR 0041). Holding both keeps one
/// default honest for either funnel; an entry for a room the fixture has no
/// layer for is read by nothing.
let homeControl = Map.ofList [ "", ownedRoom; "W1N1", ownedRoom ]

/// A stocked source: a restock of zero, ready to dig now (ADR 0025).
let source id : SourceInfo = { Id = id; TicksToRestock = 0 }

/// A drained source, the given number of ticks from its restock (ADR 0025).
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
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        InvaderCores = []
        Spatial = SpatialInfo.empty
        // No colony declared but the one this ColonyView is: a **candidate
        // colony** is a declared home the colony does not own yet (ADR
        // 0047), so an empty list is every fixture that claims nothing —
        // which is every fixture but the claim tests' own.
        Declared = []
        // And no [[stage]] for it either (ADR 0052 decision 3), which is
        // deliberate and is the same sentence: this fixture's home is
        // `SpatialInfo.empty`'s unnamed room, and a fixture that named a
        // room of its own — `openRoom`'s "W1N1" — would inherit a stage
        // filed under the wrong name and read as a colony of ours standing
        // beside it, hiring [[pioneer]]s for it. So a stage arrives with
        // the room: `atLevel` and `withLevel` file one under the home the
        // fixture actually has, and a colony with none answers every stage
        // rule the way one whose controller cannot be placed does.
        Stages = Map.empty
        // One colony in the world, so every body standing in its rooms is
        // its own: another colony's creeps are the ones this one cannot
        // move and does not price (ADR 0052 decision 1), and the fixtures
        // that need one put it here by name.
        Foreign = Set.empty
        // And nothing borrowed: this colony raises no child, so no room's
        // Upgrade and Build are hers to take (ADR 0047 decision 4).
        Borrowed = { Rooms = [] }
        // And nothing refused: "W1N2" borders "W1N1", so the declaration
        // this colony is cut from is one a Seam reaches (#243).
        Refused = []
        // And nothing remembered of a room it cannot see (#151): a fixture
        // is a tick with vision wherever it lays a fact, so an empty
        // sighting map is what every case here decides under, and the
        // vision grace is inert until a test puts a room in the dark on
        // purpose (`goneDark`).
        Sightings = Map.empty
        // The numbers this bot ships with (ADR 0052 decision 5): a
        // fixture starts from them and the tests that are *about* a
        // tunable move the one field they are about.
        Tuning = Tuning.defaults
        // Nothing in the oven: a fixture's rows count what is alive, and
        // the casting cascade's own tests are the ones that put a body
        // here (#156).
        Casting = []
    }

/// A creep with the given body's part counts, freshly cast: a full
/// Screeps CREEP_LIFE_TIME to live, so no fixture creep is expiring and no
/// lead has to be priced to read a test (ADR 0026).
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
        FreeCapacity = freeCapacity
        Moved = false
        Body = body |> List.countBy id |> Map.ofList
    }

/// The same creep with the given ticks left to live — what puts it inside
/// its row's lead and makes it expiring (ADR 0026).
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
                Map.ofList
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
/// and the [[stage]] that level puts it at, moved together (ADR 0052
/// decision 3), because they are one fact of the world read twice and a
/// fixture that moved one alone would be a colony the shell cannot build.
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

/// The trunk fixture (ADR 0011): a broad plain field with the spawn at
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
                Map.ofList
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
/// extension, the tiles one ordering rule picks (ADR 0011, ADR 0022).
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

/// The same colony with a second room's geometry beside its own: one more
/// entry in `Rooms`, under that room's name (ADR 0041). The kind census
/// stays unlayered and world-unique, exactly as the projection keeps it —
/// which is what makes these fixtures able to ask whether a reader joins a
/// kind to the right room's tile. It adds the outpost's entry and never
/// replaces the map, so the colony's own layer survives it and the helper
/// is order-blind: a `withTarget` composed either side of it is still
/// read.
let withOutpost room targets tiles (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain = Map.ofList tiles
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

/// This tick's pool with its priorities and capacities, over the
/// snapshot's own Atlas — what the Matcher and the mover are both handed
/// (ADR 0052 decision 6).
let poolOn snapshot =
    planPool snapshot (Atlas.ofView snapshot) (planTasks snapshot noThreats)

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
                Map.ofList (
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
/// onto a snapshot — position-less: unpriceable geometry never counts
/// against a Task (ADR 0004), so the pool and matching are exercised
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

/// A hostile creep of the given body standing on the given tile. Its
/// owner and its room are both immaterial to the reflexes — the Raid log
/// is the only reader of either (ADR 0028, ADR 0041) — and the room is
/// spelled the empty string, the name `SpatialInfo.homeName` gives a
/// projection that names none, which is what the colonies assigning
/// `Hostiles` directly here are built on (`bareRespawn`, `spatial`,
/// `towerColony`). A colony that *does* name its room gets that name
/// stamped on by `facing`, so no fixture files a hostile in a room its
/// own projection has no layer for.
let hostileAt id pos body : HostileInfo =
    {
        Id = id
        Owner = "raider"
        Pos = RoomPos.at "" pos
        Body = body
        // A full Invader life. The one reader is ADR 0043's clock for a raid
        // with no core in it (#257), and a fixture that said otherwise would
        // be making a claim about the stand-down rather than about the raid.
        TicksToLive = Engine.creepLifetime
    }

/// The same colony with the given hostiles standing in its room — in
/// **its** room, which is why the name is stamped on here rather than left
/// to `hostileAt` (ADR 0041). The colonies this composes onto are built on
/// `openRoom`, which names its projection "W1N1"; a hostile carrying the
/// empty name would be filed in a room those projections hold no layer
/// for, so every reader that joins a hostile to the geometry around it —
/// the Raid log's closest approach today, the reflexes' Reach at #117 —
/// would measure it against nothing.
let facing hostiles (snapshot: ColonyView) =
    { snapshot with
        Hostiles =
            hostiles
            |> List.map (fun (h: HostileInfo) ->
                { h with
                    Pos = RoomPos.at (SpatialInfo.homeName snapshot.Spatial) (RoomPos.pos h.Pos)
                })
    }

/// A room with one Dual Seat: source at (10,10), controller at (13,10).
/// The Seat (11,10) sits at range 2 of the controller — inside its Upgrade
/// Work Area — while (9,10) sits at range 4, an ordinary Seat.
let dualSeatRoom =
    { spatial
          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 13; Y = 10 } ]
          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
    }

/// An Anchor-bodied creep: four Work, one Carry, one Move.
let anchor name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Work; Work; Work; Carry; Move ]

/// The Dual Seat room, one source, controller in place — the base Anchor scenario.
let dualSeatColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt 2)
        Spatial = dualSeatRoom
    }

let moveIntentsFor name intents =
    intents
    |> List.filter (function
        | MoveCreep(creep, _) -> creep = name
        | _ -> false)

/// The dig Intents a creep is issued this tick — what tells a body the
/// Matcher kept on Harvest from one the Emitter actually lets dig (ADR
/// 0020, ADR 0024).
let digIntentsFor name intents =
    intents
    |> List.filter (function
        | HarvestSource(creep, _) -> creep = name
        | _ -> false)

/// The haul fixture (ADR 0012): a plain corridor y = 10, x = 9..21; the
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

/// A hauler-unit creep: two Carry, one Move — the hauler row's block.
let hauler name energy freeCapacity =
    creepWith name energy freeCapacity [ Carry; Carry; Move ]

/// The crowding fixture (#161): a three-row plain field y = 9..11,
/// x = 5..35, with two containers standing on the middle row — "can-near"
/// at (10,10) and "can-far" at (30,10). Every creep the tests below stand
/// on it sits one step from the near store's Work Area and seventeen or
/// more from the far one, so travel cost points the whole crowd at one
/// container and only a capacity can send any of it to the other. Three
/// rows and not one, so a waiting hauler is never in another's path: the
/// occupancy surcharge (ADR 0008) prices a queue on a one-tile lane at a
/// swamp step apiece, and a crowd that thins itself by standing in its own
/// way would prove the cap without the cap.
///
/// Two containers and not a container and a Storage, because the two must
/// sit on the *same* tier: a rank between them (ADR 0023) would decide the
/// split before capacity was ever asked.
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
            |> withTargets [ "pile-a", { X = 10; Y = 10 }, Dropped ]
            |> withCreepsAt creeps
    }

/// The hauler quota this ColonyView decides, read off the plan memo `decide`
/// returns — the quota's only seam, since the rule itself is private to
/// that pipeline.
let quotaOf snapshot =
    let { Memo = memo } = decideOn snapshot
    memo.HaulerQuota

/// The haul this ColonyView prices, summed over its source containers — the
/// number the quota above is the division of. A rock's own rate lives here,
/// where the shaping rules the quota carries (#279's floor for a haul that
/// crosses a Seam) cannot reach it, so a case about the rate reads this and a
/// case about the crowd reads the quota.
let haulDemandOf snapshot =
    let { Quotas = quotas } = decideOn snapshot
    quotas.HaulerDemand |> List.sumBy (fun row -> row.Demand)

/// The W12S28 shape (ADR 0012): a 3-wide plain field y = 9..11 from x = 8
/// to 32, two sources embedded in wall at (10,10) and (30,10) with their
/// built containers on the Seats (11,10) and (29,10) — two Posts, no Dual
/// Seat — and the spawn structure at (20,10), eight steps from either
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

/// The income-based fleet the W12S28 shape pins (ADR 0012), at the 300
/// bank this fixture banks and therefore at the Anchor body that bank buys
/// (#208): the row casts `2W/1C/1M`, which digs 4 a tick, so each of the
/// two owned Posts is worth **four** and not the ten the room would pay a
/// body big enough to take it. One Anchor per Post (2), the throughput
/// quota (1 hauler — each container's round trip is 16 ticks out loaded
/// and 8 back empty since ADR 0029 priced each leg as a walk, and the
/// colony's demand is ceil((24 + 24) × 4 / 200) = 1, rounded once for the
/// colony and not once per container, ADR 0049), and the income workers —
/// 2 posted sources × 4 e/tick × the 1500-tick lifetime = 12,000, minus
/// the anchor and hauler rows' replacement amortization (2 × 300 + 1 × 300
/// = 900), over one worker body's Work drain × lifetime (1 × 1500) →
/// ceil(7.4) = 8 (ADR 0037).
///
/// This is the live defect #208 was filed on, at the fixture that always
/// held it: read at the room's rate the same colony counted 20 a tick,
/// hired 19 workers and 3 haulers off it, and left most of them idle
/// beside a spawn already full. `richIncomeColony` below is the other half
/// of the pair — the same geometry at a bank whose cast outruns the rock,
/// where the rate is the answer again and nothing moves.
let incomeFleet =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100 ]
    @ [ for i in 1..8 -> worker $"w{i}" 0 50 ]

/// The same geometry again at the 1,800 bank of an RCL5 room, with four
/// bodies' worth of energy standing in it so a target that wanted two
/// casts could take them. Two cases read it and both are about a number
/// that is only visible here.
///
/// The bank makes every row's body the largest the rule gives: Anchor
/// `6W/1C/1M` = 700 and digging twelve, hauler 24C/12M = 1,800 carrying
/// 1,200, worker `9W/9C/9M` = 1,800 at a Work drain of nine. The Anchor's
/// twelve is over the ten an owned rock pays, so the cap #208 put on a
/// Post's worth is not binding and each of the two Posts is worth the
/// room's rate — which is the *upper* half of that ticket's pair, and the
/// half that says the rule is a cap and not a discount.
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
/// the two fixtures is which room's layer it lands in — which is the whole
/// of what ADR 0042's narrowing turns on.
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

/// The W12S28 colony with a **posted** outpost source beside it: the same
/// rock in the same three-Seat field the unposted case above leaves out of
/// every quota, with a container standing on one of its Seats — the switch
/// that admits an outpost into the economy (ADR 0042). The fleet is the
/// caller's, and so is who holds W1N2: everything else is `incomeColony`,
/// unmoved, so a difference between two calls is the reservation and
/// nothing else.
///
/// Its hauler quota is the home room's either way: the quota does fold
/// this container since #149, but W1N2 arrives here with no border ring,
/// so the two rooms share no Seam band, the haul has no price and the
/// container hires nobody (ADR 0004) — the quota's own outpost case is
/// `outpostHaulTests`, on a fixture that lays the rings. Its Anchor row
/// is *three* since #129: the container standing on that Seat makes the
/// rock a Post, and one Anchor per Post counts every projected room's
/// Posts (ADR 0042), so the fleet below carries the outpost's Anchor
/// beside the home room's two. Which leaves the income base as the one
/// addend a reservation moves, and a worker count as the whole reading of
/// it.
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
///
/// A list since ADR 0053, because the row no longer casts one body: a
/// colony with two vacancies of different ceilings and two idle spawns
/// buys the dearer first and the cheaper out of what is left.
let anchorCastsBy colony =
    spawnIntents (decideOn colony).Intents
    |> List.filter (fun (_, _, name) -> name.StartsWith "anchor-")
    |> List.map (fun (_, body, _) -> body)

/// The anchor row's body under either of the two ceilings a [[reservation]]
/// decides: five Work saturate a held rock and two a rock nobody holds, so
/// the ceilings are six and three (ADR 0021, ADR 0042). Written once
/// because the cast, the charge and the [[lead]] are all read against them.
let sixWork = [ Work; Work; Work; Work; Work; Work; Carry; Move ]

let threeWork = [ Work; Work; Work; Carry; Move ]

/// The Reach of one tick, read at the seam its three readers share (ADR
/// 0033) — the Threats are derived once from the ColonyView and the Atlas,
/// and this is that derivation, not a second one. The home room's share,
/// since the Reach is filed by room (#138) and these colonies stand their
/// hostiles at home.
let reachIn snapshot =
    Threats.reachIn
        (threatsOf snapshot (Atlas.ofView snapshot))
        (SpatialInfo.homeName snapshot.Spatial)

/// The open colony facing one hostile of the given body on the given tile.
let facingBody pos body =
    atLevel 2 (openRoom 8) |> facing [ hostileAt "h-1" pos body ]

/// A plain corridor down one column, the shape a two-room fixture needs
/// twice: geometry a reader can count steps along, in a room the flood
/// must not leave (ADR 0041).
let corridor x y0 y1 =
    [ for y in y0..y1 -> { X = x; Y = y }, Plain ]

/// A plain border ring. The Seam query reads the border layer and nothing
/// else (ADR 0041), so a projection without one answers an empty band and
/// prices no crossing at all — which is what the two fixtures above rest
/// on and what the ones below must not. Plain the whole way round, so no
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
                    Terrain = Map.ofList (corridor 10 1 40)
                    TargetPositions = Map.ofList [ "src-home", homeSource ]
                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 2 } ]
                })
    }

/// The same colony with its outpost beside it, one room north: W1N2's
/// y = 49 row lands on W1N1's y = 0 row, and `Atlas.seams` reads that join
/// out of the two room names alone (ADR 0041) — no fixture here declares
/// an edge, because a declared edge is a second fact that can disagree
/// with the first. The outpost's corridor runs to its own y = 48, so the
/// tile a crossing lands a creep on opens onto ground.
///
/// `None` is the room before anything is laid into it: the whole of what
/// `World.factsOf` builds for a room with no vision — its terrain
/// and its border ring, because `Game.map.getRoomTerrain` needs neither —
/// and not one entry more, because everything vision pays for is absent
/// entry by entry until vision returns (ADR 0004).
///
/// That is not the whole of what the shell hands Core for a *declared*
/// room it cannot see: `Outpost.place` lays the declared sources and
/// controller in afterwards, with no vision at all (ADR 0041, #148). So
/// this is the baseline the declaration is added to and never a blind
/// outpost as the colony really projects one — the tests below that want
/// one build it by calling `place`, as the shell does.
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
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions =
                        outpostSource
                        |> Option.map (fun pos -> Map.ofList [ "src-out", pos ])
                        |> Option.defaultValue Map.empty
                }
    }

/// The sites standing in the outpost, as many as the caller names, each under
/// its own id: the container rule is the only thing *this colony* ever places
/// out there (ADR 0042), so every site of another kind laid here is a human's
/// hand — the **trunk** #244 recorded and #266's budget queues (`withNorthSpawnSite`
/// below is the other way one gets there, which the nursery cases are built on).
/// Each arrives in the three pieces the shell hands Core a site in: the id-keyed
/// kind census, the outpost layer's own tile, and the `ConstructionSites` entry
/// vision pays for (#150). Merges into whatever layer `withNorthOutpost` already
/// laid, so the two compose in either order.
let withOutpostTrunk (sites: (string * BuiltKind * Pos) list) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        ConstructionSites = colony.ConstructionSites @ [ for id, _, _ in sites -> { Id = id } ]
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
/// names — `withOutpostTrunk`'s one-site case and never a second spelling of
/// it, the three pieces above being the shell's contract and this file the one
/// place it is written.
///
/// The kind is a parameter because #205's gates read it: a Seat's
/// *container* site is a Post and reopens Build to the body standing on
/// it, and a site of any other kind on the same tile is the ordinary
/// surplus work it always was. Pairwise cases below swap the kind and
/// nothing else.
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
/// rival (ADR 0009's Matched Verdict).
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
/// so nothing here is ADR 0007's deadline rank in disguise.
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
/// caller puts it: the **Feeding**-tier rival a *home* site's Build is
/// measured against since #234.
///
/// The controller `withHomeController` adds cannot do that work for a home
/// site any more. Such a site outranks the Upgrade beside it by a rung now
/// (`isHomeSite`), so it wins on rank whichever tier it is on and the
/// factor stops naming the tier. The flow still names it, because the rung
/// never leaves the surplus tier: a site lifted onto the flow (ADR 0042,
/// ADR 0047) *ties* this Refill and the nearer of the two wins, while a
/// surplus one is outranked by it outright however near it stands. Place
/// it **farther** than the site and the two readings differ in the winner
/// and not merely in the factor, which is what the cases below do.
///
/// A site past the Seam needs none of this: the rung stops at the home
/// room, so the controller is still the instrument there and the cases
/// about an outpost's site go on reading it.
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
                    Terrain = Map.ofList (({ X = 24; Y = 20 }, Wall) :: corridor 25 1 48)
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
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
                }
    }

/// And the tick the switch closes: the container standing on the outpost
/// rock's one Seat, and nothing else in the world different (ADR 0042).
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
/// the one hauler its container's round trip to the spawn hires, and the
/// four workers the Post's output feeds once the two rows above are
/// amortized — ceil((4 × 1500 − 2 × 300) / 1500) = 4 (ADR 0012, ADR 0037).
/// Six bodies, and every case below reads against it.
///
/// Four a tick and not ten because this fixture banks 300 (#208): the
/// Anchor row's cast there is `2W/1C/1M`, and a Post is worth what its
/// garrison digs under the rock's own rate. The switch these cases are
/// about is unmoved by that — what a container standing does is add a row
/// of each kind, whatever the bank prices the rock at.
let switchHomeFleet =
    [ anchor "a-home" 0 50; hauler "h-home1" 0 100 ]
    @ [ for i in 1..4 -> worker $"w{i}" 0 50 ]

/// What the outpost's container adds, and nothing else: one Anchor for the
/// Post it makes, the one hauler its own round trip across the Seam adds
/// to the colony's pool at its own source's output, and its income
/// share — the worker row goes from four to eight, because eight a tick
/// less the four rows' amortization over one worker's Work drain is
/// ceil((8 × 1500 − 4 × 300) / 1500) = 8. Six more bodies, which is the
/// whole of ADR 0042's switch stated as a fleet.
///
/// One hauler and not two because the pool is rounded once (ADR 0049): the
/// home container's 0.54 of a body and the outpost's 1.02 come to 1.56 and
/// hire two, where a ceiling apiece hired one and two.
let switchOutpostRows =
    [ anchor "a-out" 0 50; hauler "h-out1" 0 100 ]
    @ [ for i in 5..8 -> worker $"w{i}" 0 50 ]

/// A hostile filed under the room it stands in — the field `facing`
/// stamps with the home name, set by hand here because these fixtures
/// put a hostile in either room (ADR 0041).
let hostileIn room pos body =
    { hostileAt "h-1" pos body with
        Pos = RoomPos.at room pos
    }

/// The engine's own `smallMelee`: two TOUGH, five MOVE, a RANGED_ATTACK, a
/// WORK and an ATTACK — 1,000 hits and 40 damage at range 1, and the body
/// nine remote raids in ten arrive as (ADR 0056,
/// `docs/research/remote-invader-defence.md`). Written part for part rather
/// than reduced to "something armed", because the parts are what every rule
/// reads: the ATTACK is what makes it a [[threat]] at all (ADR 0033), the
/// RANGED_ATTACK beside it is what sets its [[reach]] at 3 plus the margin
/// rather than 1 plus it — which is the whole of how much ground a raid takes
/// — and it carries **no HEAL**, which is what keeps the guard row's count at
/// one against it.
let smallMelee =
    [ Tough; Tough; Move; Move; Move; Move; RangedAttack; Work; Attack; Move ]

/// The engine's own `smallHealer`: five MOVE and five HEAL, 60 hits a tick at
/// range 1 unboosted. No ATTACK and no RANGED_ATTACK, so it is a [[hostile]]
/// the [[raid log]] records and no [[threat]] at all — no [[reach]], no ring,
/// and no reason of its own to buy a body — and it is exactly what the guard
/// row's count rule prices, one 750-energy guard's 90 damage standing against
/// one of these and losing to two (ADR 0056).
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
/// projection the way the shell lays one (`Outpost.place`, ADR 0041) — so
/// it is in the pool with no vision at all, and its tile is an obstacle,
/// which is what puts the reserver beside the controller and never on it.
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
/// test names standing in that outpost — filed under its own layer,
/// because a creep is placed in the room it stands in and nowhere else
/// (ADR 0041).
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

/// ADR 0042's own reserver: two CLAIM parts and two Move, 1,300 energy.
/// Carrying nothing and with nowhere to put anything — a CLAIM body has no
/// Carry part — so no gate below can be passing on an energy state.
let reserver name =
    creepWith name 0 0 [ BodyPart.Claim; BodyPart.Claim; Move; Move ]

let reserveTasks tasks =
    tasks
    |> List.choose (function
        | Reserve controllerId -> Some controllerId
        | _ -> None)

/// And the tick after: the same declaration, the same room, ours now
/// (ADR 0047 decision 4). One fact apart from `asCandidate` — who holds
/// the controller — which is the whole of what turns a candidate into a
/// **nursery**, there being no spawn of ours in that room in any fixture
/// here until `withNorthSpawn` puts one there.
let asNursery (colony: ColonyView) =
    { colony with
        RoomControl = Map.add "W1N2" ownedRoom colony.RoomControl
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
        // The shell's `ColonyView.ofWorld` borrows a nursery for the
        // mother the tick it is claimed; the pool's budget reads that
        // field (#210), so the fixture carries it as the shell would.
        Borrowed = { Rooms = [ "W1N2" ] }
        // Declared and owned with no spawn of ours standing in it is the
        // whole of the [[stage]] `Nursery` (ADR 0052 decision 3), so the
        // shell would derive exactly this entry for the room; the
        // ownership above is what makes the room *this* colony's to raise
        // and stays beside it.
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
/// 1,800 — the level ADR 0042's `[2Claim;2Move]` is priced against — with
/// the named outposts standing beside it and the named holder on each
/// room. Everything else is `incomeColony`, unmoved, so a difference
/// between two calls is the outposts, the fleet or the reservation and
/// nothing else. The bank holds 8,000 against that capacity: restraint in
/// these cases must come from the rows, never from the bank running dry.
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
/// and W13S28 are (ADR 0042) — two rooms, so "one reserver per declared
/// outpost" can be told apart from "one reserver".
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

/// The buffer lane (ADR 0046): a plain corridor three rows deep, the
/// controller standing at (10,10) — an obstacle, as a projected one is —
/// and its upgrade buffer "can-buf" at (13,10), the outermost tile of the
/// controller's Upgrade Work Area and so a container the Planner pools as
/// the buffer.
///
/// A creep beside the buffer at (14,10) stands one step *outside* that
/// Work Area and *inside* the Work Area of anything at (15,10), which is
/// where each case below puts its delivery. So Upgrade costs a step and
/// the delivery costs nothing, the two share the Surplus tier, and travel
/// cost alone would take every body to the delivery: what separates them
/// is the gate and nothing else (ADR 0046). Which is the whole reason the
/// gate is a prohibition rather than a price — the work a standing body
/// must not walk to is the work standing closest to it.
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

/// The lane with one creep on the buffer's doorstep and the given
/// furniture wherever the case wants it — (15,10), a step out, for ADR
/// 0046's own cases. No source, no hungry spawn and no Storage, so the
/// pool is exactly the controller's Upgrade, the buffer's own Withdraw and
/// Refill, and whatever the case stands beside the creep — the smallest
/// pool that can hold ADR 0046's question. The bank is the live RCL5
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
                        Map.ofList
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
                        Map.ofList
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
/// the row name the caster writes into the creep's name (ADR 0006:
/// observability only, and this is the observation).
let castRows intents =
    spawnIntents intents
    |> List.map (fun (_, _, name: string) -> (name: string).Split('-') |> Array.head)
