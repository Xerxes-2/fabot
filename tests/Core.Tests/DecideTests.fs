module Fabot.Core.Tests.DecideTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide

/// A single idle spawn standing in the default room.
let spawn =
    {
        Name = "Spawn1"
        Id = "spawn-1"
        RoomName = "W1N1"
        IsSpawning = false
    }

/// The default room's bank holding the given energy against the given capacity.
let bank energy capacity =
    Map.ofList
        [
            "W1N1",
            {
                Available = energy
                Capacity = capacity
            }
        ]

/// A controller far from its downgrade deadline, stock intact.
let controllerAt level =
    {
        Id = "ctrl-1"
        Level = level
        TicksToDowngrade = 20000
        SafeModeAvailable = 1
        SafeModeActive = false
    }

/// A stocked source — the kind the Planner pools a Harvest for (ADR 0013).
let source id : SourceInfo = { Id = id; Stocked = true }

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
        RoomEnergy = bank 300 300
        Refillables = [ refillable "spawn-1" 0 BuiltKind.Spawn ]
        Sources = [ source "src-a"; source "src-b" ]
        Controller = Some(controllerAt 1)
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        Spatial = SpatialInfo.empty
    }

/// A creep with the given body's part counts.
let creepWith name energy freeCapacity body =
    {
        Name = name
        Fatigue = 0
        Energy = energy
        FreeCapacity = freeCapacity
        Body = body |> List.countBy id |> Map.ofList
    }

/// A generalist worker-unit creep: one Work, one Carry, one Move.
let worker name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Carry; Move ]

let spawnIntents intents =
    intents
    |> List.choose (function
        | SpawnCreep(s, b, c) -> Some(s, b, c)
        | _ -> None)

[<Tests>]
let directionCodeTests =
    testList
        "direction codes"
        [
            test "matches the engine's TOP = 1, then clockwise" {
                // These constants leave the program as Creep.move arguments; the
                // table here is the engine's spec, restated so a swapped case fails.
                Expect.equal
                    ([ Top; TopRight; Right; BottomRight; Bottom; BottomLeft; Left; TopLeft ]
                     |> List.map directionCode)
                    [ 1; 2; 3; 4; 5; 6; 7; 8 ]
                    "each Direction maps to its Screeps constant"
            }
        ]

[<Tests>]
let partNameTests =
    testList
        "part names"
        [
            test "matches the engine's spelling, one name per part" {
                // These strings leave the program in spawnCreep bodies and
                // come back in hostile body arrays; the table is the engine's
                // spec, restated so a swapped or misspelt case fails.
                Expect.equal
                    (allBodyParts |> List.map partName)
                    [ "work"; "carry"; "move"; "attack"; "ranged_attack"; "heal"; "claim"; "tough" ]
                    "each BodyPart maps to its Screeps string"
            }
        ]

[<Tests>]
let bodyTests =
    testList
        "worker body"
        [
            test "a 150 remainder buys two Carry and a Move" {
                // 550 = 2 units + 150: the old whole-unit body stranded 150.
                Expect.equal
                    (workerBodyFor 550)
                    [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                    "remainder is spent at parity: max Carry without moving slower than the pure-unit body"
            }

            test "a 50 remainder buys a Move, not a Carry" {
                // A lone Carry would tip loaded fatigue past the pure-unit body's.
                Expect.equal
                    (workerBodyFor 250)
                    [ Work; Carry; Move; Move ]
                    "the trailing 50 goes to Move"
            }

            test "a 100 remainder buys a Carry/Move pair" {
                Expect.equal
                    (workerBodyFor 500)
                    [ Work; Work; Carry; Carry; Carry; Move; Move; Move ]
                    "a pair keeps parity and adds haul"
            }

            test "an exact multiple stays pure units" {
                Expect.equal
                    (workerBodyFor 800)
                    (List.replicate 4 Work @ List.replicate 4 Carry @ List.replicate 4 Move)
                    "no remainder, no pad"
            }

            test "below one unit cost the floor is one unit" {
                Expect.equal (workerBodyFor 150) [ Work; Carry; Move ] "never below one unit"
            }

            test "every capacity is spent to within a part price, at fatigue parity" {
                for capacity in 200..50..1300 do
                    let body = workerBodyFor capacity

                    let count part =
                        body |> List.filter ((=) part) |> List.length

                    let work, carry, move = count Work, count Carry, count Move

                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        capacity
                        $"affordable at capacity {capacity}"

                    Expect.isLessThan
                        (capacity - bodyCost body)
                        50
                        $"nothing a part could buy is stranded at capacity {capacity}"

                    Expect.isLessThanOrEqual
                        (work + carry)
                        (2 * move)
                        $"loaded parity with the pure-unit body at capacity {capacity}"

                    Expect.isLessThanOrEqual
                        work
                        move
                        $"empty parity with the pure-unit body at capacity {capacity}"
            }

            test "the body never exceeds the 50-part engine cap" {
                // RCL8 capacity: unbounded replication would emit 192 parts,
                // which the engine rejects outright.
                Expect.equal
                    (workerBodyFor 12900)
                    (List.replicate 16 Work @ List.replicate 17 Carry @ List.replicate 17 Move)
                    "16 units plus a Carry/Move pair fill exactly 50 parts"
            }
        ]

[<Tests>]
let patternTableTests =
    testList
        "pattern table"
        [
            test "the worker unit, the Anchor and the hauler are the table's rows" {
                Expect.equal
                    patternTable
                    [
                        {
                            Name = "worker"
                            Block = [ Work; Carry; Move ]
                        }
                        {
                            Name = "anchor"
                            Block = [ Work; Work; Carry; Move ]
                        }
                        {
                            Name = "hauler"
                            Block = [ Carry; Carry; Move ]
                        }
                    ]
                    "every body the colony casts comes from these rows"
            }

            test "the anchor row spends everything on Work beside one Carry and one Move" {
                // 550 = the RCL2 full bank: 100 buys the Carry/Move pair,
                // the rest is Work — no parity padding (ADR 0006 exempts
                // the Anchor from fatigue parity).
                Expect.equal
                    (bodyFor anchorPattern 550)
                    [ Work; Work; Work; Work; Carry; Move ]
                    "all remaining energy buys Work"
            }

            test "the anchor row never casts below its block" {
                Expect.equal
                    (bodyFor anchorPattern 300)
                    [ Work; Work; Carry; Move ]
                    "two Work keep the Anchor readable off its body (Work > Move)"
            }

            test "the anchor row stops at source saturation plus one spare Work" {
                // ADR 0021: a source regenerates 3,000 energy per 300 ticks
                // and a Work digs 2 a tick, so five Work saturate it; the
                // sixth is slack for an unmanned Post's gap. RCL4's 1,300
                // bank would otherwise buy twelve.
                Expect.equal
                    (bodyFor anchorPattern 1300)
                    [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                    "six Work beside the Carry/Move pair, the rest stays banked"
            }

            test "the anchor cap holds at the richest bank" {
                Expect.equal
                    (bodyFor anchorPattern 12900)
                    [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                    "an RCL8 bank casts the same six-Work Anchor"
            }

            test "the hauler row builds whole blocks and nothing else" {
                // 500 buys three whole blocks (450) and strands the rest:
                // the row's own declaration is road parity, which a padded
                // lone Carry would break — three Move pay six fatigue a
                // tick, seven loaded Carry on a road would generate seven.
                Expect.equal
                    (bodyFor haulerPattern 500)
                    (List.replicate 6 Carry @ List.replicate 3 Move)
                    "capacity buys whole [Carry;Carry;Move] blocks; the remainder stays banked"
            }

            test "the hauler row never casts below its block" {
                Expect.equal
                    (bodyFor haulerPattern 100)
                    [ Carry; Carry; Move ]
                    "the block is the row's minimal cast"
            }

            test "the hauler body never exceeds the 50-part engine cap" {
                Expect.equal
                    (bodyFor haulerPattern 9000)
                    (List.replicate 32 Carry @ List.replicate 16 Move)
                    "sixteen blocks fill 48 parts"
            }

            test "spawn planning casts from the pattern table's row" {
                // An established colony at full capacity: the spawned body
                // is the table row sized to capacity, and the creep name
                // carries the row's name — not a hard-coded worker shape.
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 550 550
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                let row = List.head patternTable

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body (bodyFor row 550) "body is the row repeated by capacity"
                    Expect.stringStarts creepName $"{row.Name}-" "creep name carries the row's name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

[<Tests>]
let plannerTests =
    testList
        "planner"
        [
            test "one Harvest task per source" {
                let tasks = planTasks bareRespawn

                let harvests =
                    tasks
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal
                    harvests
                    [ "src-a"; "src-b" ]
                    "each source gets exactly one Harvest task"
            }

            test "a drained source pools no Harvest task" {
                // ADR 0013: the task exists while the source holds energy
                // and is gone otherwise — the Repair shape. Holders are
                // released through the existing TaskGone path.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a"; { source "src-b" with Stocked = false } ]
                    }

                let harvests =
                    planTasks snapshot
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal harvests [ "src-a" ] "only the stocked source is worth digging"
            }

            test "a controller yields an Upgrade task" {
                let upgrades =
                    planTasks bareRespawn
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.equal upgrades [ "ctrl-1" ] "the controller gets exactly one Upgrade task"
            }

            test "no Upgrade task without a controller" {
                let tasks = planTasks { bareRespawn with Controller = None }

                let upgrades =
                    tasks
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.isEmpty upgrades "nothing to upgrade"
            }

            test "each construction site yields a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" }; { Id = "site-2" } ]
                    }

                let builds =
                    planTasks snapshot
                    |> List.choose (function
                        | Build siteId -> Some siteId
                        | _ -> None)

                Expect.equal builds [ "site-1"; "site-2" ] "one Build task per construction site"
            }

            test "a structure missing energy gets a Refill task; a full structure gets none" {
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" 50 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "ext-2" 50 BuiltKind.Extension
                            ]
                    }

                let refills =
                    planTasks snapshot
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "spawn-1"; "ext-2" ]
                    "only structures with free capacity need a Refill"
            }

            test "a tower missing energy gets a Refill task; a full tower gets none" {
                // Same generalized Task, same free-capacity filter (ADR 0010) —
                // a tower is just one more energy-hungry structure to the Planner.
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "tower-2" 0 BuiltKind.Tower
                            ]
                    }

                let refills =
                    planTasks snapshot
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "tower-1" ]
                    "only the tower with free capacity needs a Refill"
            }
        ]

/// Synthetic open room: every tile within `radius` of (25,25) is Plain,
/// with the spawn structure "spawn-1" standing at the centre.
let openRoom radius =
    let spawnPos = { X = 25; Y = 25 }

    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Terrain =
            Map.ofList
                [
                    for x in 25 - radius .. 25 + radius do
                        for y in 25 - radius .. 25 + radius do
                            { X = x; Y = y }, Plain
                ]
        TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
        TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
        Obstacles = Set.singleton spawnPos
    }

/// The room with extra targets standing (or being built) on given tiles.
let withTargets targets room =
    { room with
        TargetPositions =
            (room.TargetPositions, targets)
            ||> List.fold (fun acc (id, pos, _) -> Map.add id pos acc)
        TargetKinds =
            (room.TargetKinds, targets)
            ||> List.fold (fun acc (id, _, kind) -> Map.add id kind acc)
    }

let placementIntents intents =
    intents
    |> List.choose (function
        | PlaceConstructionSite(room, pos, kind) -> Some(room, pos, kind)
        | _ -> None)

let placedTiles intents =
    placementIntents intents |> List.map (fun (_, pos, _) -> pos)

let atLevel level room =
    { bareRespawn with
        Controller = Some(controllerAt level)
        Spatial = room
    }

[<Tests>]
let placementTests =
    testList
        "placement"
        [
            test "RCL2 on open terrain places 5 extensions checkerboard, nearest first" {
                let { Intents = intents } = decide (atLevel 2 (openRoom 3)) Map.empty Set.empty None

                // The nearest checkerboard tile (24,24) is the Storage's pick
                // and (24,26) the tower's in the RCL4-horizon Layout (ADR
                // 0022), so the extensions start two tiles in.
                Expect.equal
                    (placedTiles intents)
                    [
                        { X = 26; Y = 24 }
                        { X = 26; Y = 26 }
                        { X = 23; Y = 23 }
                        { X = 23; Y = 25 }
                        { X = 23; Y = 27 }
                    ]
                    "the last diagonal neighbours, then rank-2 checkerboard tiles"

                for (room, _, kind) in placementIntents intents do
                    Expect.equal room "W1N1" "sites go in the spawn's room"
                    Expect.equal kind Extension "only extensions are placed at RCL2"
            }

            test "below RCL2 no placement Intents are emitted" {
                let { Intents = intents } = decide (atLevel 1 (openRoom 3)) Map.empty Set.empty None
                Expect.isEmpty (placementIntents intents) "no extensions allowed at RCL1"
            }

            test "unwalkable tiles are skipped" {
                let room = openRoom 3

                let holed =
                    { room with
                        Terrain = Map.add { X = 24; Y = 24 } Wall room.Terrain
                    }

                let { Intents = intents } = decide (atLevel 2 holed) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "wall tile is never chosen"

                Expect.hasLength (placementIntents intents) 5 "the cap is still reached elsewhere"
            }

            test "occupied tiles are skipped" {
                let blocked =
                    openRoom 3
                    |> withTargets [ "rock-1", { X = 24; Y = 24 }, Structure BuiltKind.Other ]

                let { Intents = intents } = decide (atLevel 2 blocked) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "occupied tile is never chosen"

                Expect.hasLength (placementIntents intents) 5 "the cap is still reached elsewhere"
            }

            test "built extensions and pending sites count against the cap" {
                let room =
                    openRoom 3
                    |> withTargets
                        [
                            "ext-1", { X = 24; Y = 24 }, Structure BuiltKind.Extension
                            "ext-2", { X = 24; Y = 26 }, Structure BuiltKind.Extension
                            "site-1", { X = 26; Y = 24 }, Site BuiltKind.Extension
                            "site-2", { X = 26; Y = 26 }, Site BuiltKind.Extension
                        ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None
                Expect.hasLength (placementIntents intents) 1 "only the shortfall is placed"
            }

            test "no placement Intents once the allowance is exhausted" {
                let room =
                    openRoom 3
                    |> withTargets
                        [
                            for i in 1..5 ->
                                $"ext-{i}", { X = 22 + i; Y = 22 }, Structure BuiltKind.Extension
                        ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None
                Expect.isEmpty (placementIntents intents) "allowance already used up"
            }

            test "the controller's tile is never chosen" {
                // The controller stands on a free same-colour tile the old
                // Placement projection would have offered to a site.
                let room = openRoom 3 |> withTargets [ "ctrl-1", { X = 24; Y = 24 }, Controller ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a target's tile is never chosen"

                Expect.hasLength (placementIntents intents) 5 "the cap is still reached elsewhere"
            }

            test "no placement Intents without a projected room" {
                let snapshot =
                    { bareRespawn with
                        Controller = Some(controllerAt 2)
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (placementIntents intents) "nothing to plan around"
            }
        ]

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
        Terrain =
            Map.ofList
                [
                    for x in 10..40 do
                        for y in 15..35 do
                            let tile = { X = x; Y = y }

                            tile,
                            (if tile = sourcePos then
                                 Wall
                             elif List.contains tile areaSwamps || tile = { X = 20; Y = 20 } then
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
        TargetKinds =
            Map.ofList
                [
                    "spawn-1", Structure BuiltKind.Spawn
                    "ctrl-1", Controller
                    "src-a", Source
                    "ext-1", Structure BuiltKind.Extension
                    "ext-2", Structure BuiltKind.Extension
                ]
        Obstacles = Set.ofList (spawnPos :: controllerPos :: builtExtensions)
    }

/// The trunk fixture's colony at a controller level.
let trunkColony level =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt level)
        Spatial = trunkRoom
    }

/// The trunk fixture without its source: nothing to pave a trunk from.
let noSourceColony level =
    { trunkColony level with
        Sources = []
        Spatial =
            { trunkRoom with
                TargetPositions = Map.remove "src-a" trunkRoom.TargetPositions
                TargetKinds = Map.remove "src-a" trunkRoom.TargetKinds
            }
    }

/// The trunk fixture with a second source walled into a pocket: every
/// neighbour of (20,30) is wall terrain except the single Seat east of
/// it — the W12S28-source-B shape (ADR 0012).
let pocketColony level =
    let srcB = { X = 20; Y = 30 }
    let seat = { X = 21; Y = 30 }

    let walled =
        [
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    { X = srcB.X + dx; Y = srcB.Y + dy }
        ]
        |> List.filter (fun tile -> tile <> seat)

    let room =
        { trunkRoom with
            Terrain = (trunkRoom.Terrain, walled) ||> List.fold (fun acc t -> Map.add t Wall acc)
        }
        |> withTargets [ "src-b", srcB, Source ]

    { trunkColony level with
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = room
    }

let sitesOfKind kind intents =
    placementIntents intents
    |> List.choose (fun (_, pos, k) -> if k = kind then Some pos else None)

/// The colony with its own road plan already standing: the state the
/// source containers drop in — a container defers to a road site on its
/// tile (one construction site per tile) and coexists with the built road.
let withRoadsBuilt colony =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    { colony with
        Spatial =
            { colony.Spatial with
                Roads = sitesOfKind Road intents |> Set.ofList
            }
    }

let chebyshev a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// The clustered structures of a plan: the Storage, the tower and every
/// extension, the tiles one ordering rule picks (ADR 0011, ADR 0022).
let clusterTiles intents =
    sitesOfKind Storage intents
    @ sitesOfKind Tower intents
    @ sitesOfKind Extension intents
    |> Set.ofList

/// The clustered ordering's sort key for a fixture whose spawn stands at
/// (25,25): nearest-to-spawn first, ties by x then y (ADR 0011).
let orderKey tile =
    chebyshev tile { X = 25; Y = 25 }, tile.X, tile.Y

[<Tests>]
let layoutTests =
    testList
        "layout"
        [
            test "RCL2 places the extension gap and every trunk road, no tower" {
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None

                Expect.isEmpty (sitesOfKind Tower intents) "no tower below RCL3"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    3
                    "only the gap against the two built extensions is placed"

                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 15; Y = 25 } = 1))
                    "a trunk starts beside the source"

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 25; Y = 25 } = 1))
                    "a trunk ends beside the spawn"

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 35; Y = 25 } <= 3))
                    "a trunk reaches the controller's Work Area"

                Expect.contains roads { X = 33; Y = 27 } "a Work Area swamp is paved"
                Expect.contains roads { X = 34; Y = 24 } "the other Work Area swamp is paved"

                Expect.isFalse
                    (Set.contains { X = 20; Y = 20 } roads)
                    "a swamp off every trunk line is not paved"
            }

            test "the same fixture at RCL3 adds the tower and extensions 6-10 at once" {
                let { Intents = intents } = decide (trunkColony 3) Map.empty Set.empty None

                // (24,24) is the ordering's first free tile and the Storage's
                // reservation (ADR 0022); the tower takes the one after it,
                // and the fixture's two built extensions hold (24,26)/(26,24).
                Expect.equal
                    (sitesOfKind Tower intents)
                    [ { X = 26; Y = 26 } ]
                    "the tower takes the ordering's first free tile after the Storage's"

                let extensions = sitesOfKind Extension intents
                Expect.hasLength extensions 8 "the RCL3 allowance fills against the two built"

                for tile in extensions do
                    Expect.isLessThan
                        (orderKey { X = 26; Y = 26 })
                        (orderKey tile)
                        "the tower's pick comes before every extension in the one ordering"
            }

            test "the same Snapshot recomputes to the identical site set" {
                let first = decide (trunkColony 2) Map.empty Set.empty None
                let second = decide (trunkColony 2) Map.empty Set.empty None

                Expect.equal
                    (placementIntents first.Intents)
                    (placementIntents second.Intents)
                    "the Layout is deterministic — sites never jitter between computations"
            }

            test "trunks route around every RCL4-horizon reservation" {
                let rcl2 = decide (trunkColony 2) Map.empty Set.empty None
                let rcl4 = decide (trunkColony 4) Map.empty Set.empty None
                let roads = sitesOfKind Road rcl2.Intents |> Set.ofList

                let cluster = clusterTiles rcl4.Intents

                Expect.equal
                    (sitesOfKind Road rcl4.Intents |> Set.ofList)
                    roads
                    "the road plan is the same at every level — the horizon never moves"

                Expect.isEmpty
                    (Set.intersect roads cluster)
                    "no trunk tile coincides with a reserved structure tile"
            }

            test "a Seat beside the spawn is working ground: no tower, no extension" {
                // The source stands two tiles north of the spawn, so four of
                // its Seats are the cluster's own nearest same-colour tiles.
                let sourcePos = { X = 25; Y = 23 }

                let colony = atLevel 3 (openRoom 6 |> withTargets [ "src-a", sourcePos, Source ])

                let { Intents = intents } = decide colony Map.empty Set.empty None

                let seats =
                    Set.ofList
                        [
                            for x in sourcePos.X - 1 .. sourcePos.X + 1 do
                                for y in sourcePos.Y - 1 .. sourcePos.Y + 1 do
                                    if { X = x; Y = y } <> sourcePos then
                                        { X = x; Y = y }
                        ]

                let cluster = clusterTiles intents

                Expect.isNonEmpty cluster "the cluster still fills, one ring out"

                Expect.isEmpty
                    (Set.intersect cluster seats)
                    "no clustered structure eats a Seat the Anchors stand on"
            }

            test "the Upgrade Work Area is working ground: no tower, no extension" {
                // The controller stands four tiles north of the spawn, so its
                // Upgrade Work Area covers the cluster's nearest same-colour
                // tiles without covering the spawn itself.
                let controllerPos = { X = 25; Y = 21 }

                let colony =
                    atLevel 3 (openRoom 6 |> withTargets [ "ctrl-1", controllerPos, Controller ])

                let { Intents = intents } = decide colony Map.empty Set.empty None

                let upgradeArea =
                    Set.ofList
                        [
                            for x in controllerPos.X - 3 .. controllerPos.X + 3 do
                                for y in controllerPos.Y - 3 .. controllerPos.Y + 3 do
                                    { X = x; Y = y }
                        ]

                let cluster = clusterTiles intents

                Expect.isNonEmpty cluster "the cluster still fills, one ring out"

                Expect.isEmpty
                    (Set.intersect cluster upgradeArea)
                    "no clustered structure eats a tile an upgrader stands on"
            }

            test "without a source only the Work Area swamps are paved, never plain" {
                let { Intents = intents } = decide (noSourceColony 2) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Road intents |> Set.ofList)
                    (Set.ofList [ { X = 33; Y = 27 }; { X = 34; Y = 24 } ])
                    "exactly the Work Area's swamp tiles get roads"
            }

            test "built roads and pending road sites are never placed again" {
                let colony = noSourceColony 2

                let snapshot =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Roads = Set.singleton { X = 33; Y = 27 }
                            }
                            |> withTargets
                                [ "road-site-1", { X = 34; Y = 24 }, Site BuiltKind.Road ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Road intents)
                    "the gap reads the projection's road census: both tiles are claimed"
            }

            test "each source gets one container on the Seat where its trunk starts" {
                let colony = withRoadsBuilt (trunkColony 2)
                let { Intents = intents } = decide colony Map.empty Set.empty None

                let sourceContainers =
                    sitesOfKind Container intents
                    |> List.filter (fun tile -> chebyshev tile { X = 15; Y = 25 } = 1)

                Expect.hasLength sourceContainers 1 "one container per source"

                Expect.contains
                    colony.Spatial.Roads
                    sourceContainers.Head
                    "the Seat nearest the trunk is the trunk's own first tile"
            }

            test "a container never shares a tile with a planned road site" {
                // One construction site per tile (engine rule): on a fresh
                // plan the source container defers to the trunk road site
                // under it and drops only once that road stands.
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None
                let roads = sitesOfKind Road intents |> Set.ofList

                for tile in sitesOfKind Container intents do
                    Expect.isFalse
                        (Set.contains tile roads)
                        "the container waits for the road on its tile"
            }

            test "the controller container lands in the Work Area beside a trunk" {
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None
                let controllerPos = { X = 35; Y = 25 }

                let controllerContainers =
                    sitesOfKind Container intents
                    |> List.filter (fun tile -> chebyshev tile controllerPos <= 3)

                Expect.hasLength controllerContainers 1 "exactly one controller container"

                let tile = controllerContainers.Head
                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isTrue
                    (roads |> Set.exists (fun road -> chebyshev road tile = 1))
                    "the container sits adjacent to a trunk tile"

                Expect.isFalse (Set.contains tile roads) "the container stays off the road itself"
            }

            test "containers have no RCL gate — level 1 already places both kinds" {
                let { Intents = intents } =
                    decide (withRoadsBuilt (trunkColony 1)) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Container intents)
                    2
                    "one source container and one controller container"
            }

            test "a one-Seat source gets its container on that Seat" {
                let { Intents = intents } =
                    decide (withRoadsBuilt (pocketColony 2)) Map.empty Set.empty None

                Expect.contains
                    (sitesOfKind Container intents)
                    { X = 21; Y = 30 }
                    "the single Seat is the nearest Seat to the pocket source's trunk"
            }

            test "built containers and pending container sites are never placed again" {
                let colony = withRoadsBuilt (trunkColony 2)
                let planned = decide colony Map.empty Set.empty None

                let standing =
                    match sitesOfKind Container planned.Intents with
                    | [ a; b ] ->
                        [
                            "can-1", a, Structure BuiltKind.Container
                            "can-site-1", b, Site BuiltKind.Container
                        ]
                    | other -> failtest $"expected two planned container sites, got %A{other}"

                let snapshot =
                    { colony with
                        Spatial = colony.Spatial |> withTargets standing
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Container intents)
                    "the census claims both tiles: nothing re-drops"

                Expect.equal
                    (sitesOfKind Road intents)
                    (sitesOfKind Road planned.Intents)
                    "standing containers never perturb the road plan"
            }
        ]

[<Tests>]
let storageTests =
    testList
        "storage"
        [
            test "RCL4 places one Storage on the ordering's first pick, the tower next" {
                // The cluster's nearest same-colour tile is the Storage's at
                // every level (ADR 0022) — the tower and the extensions take
                // the picks after it.
                let { Intents = intents } = decide (atLevel 4 (openRoom 3)) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Storage intents)
                    [ { X = 24; Y = 24 } ]
                    "one Storage, on the ordering's first still-open pick"

                Expect.equal
                    (sitesOfKind Tower intents)
                    [ { X = 24; Y = 26 } ]
                    "the tower's pick is the one after the Storage's"

                for tile in sitesOfKind Extension intents do
                    Expect.isLessThan
                        (orderKey { X = 24; Y = 24 })
                        (orderKey tile)
                        "the Storage's pick comes before every extension in the one ordering"
            }

            test "RCL3 places no Storage yet still holds its tile against the cluster" {
                // The reservation is level-blind (ADR 0022): once an extension
                // takes that tile it never comes back, so it is held from the
                // first tick, levels before the engine allows the Storage.
                let { Intents = intents } = decide (atLevel 3 (openRoom 3)) Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the engine allows no Storage below RCL4"

                Expect.isNonEmpty (sitesOfKind Extension intents) "the cluster still fills"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "nothing at all is placed on the reserved first pick"
            }

            test "a standing Storage places none, and its tile leaves the ordering" {
                let standing =
                    openRoom 3
                    |> withTargets [ "sto-1", { X = 24; Y = 24 }, Structure BuiltKind.Storage ]

                let { Intents = intents } = decide (atLevel 4 standing) Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the standing census fills the allowance"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a standing structure's tile is not buildable: nothing is planned onto it"
            }

            test "a pending Storage site places none" {
                let pending =
                    openRoom 3
                    |> withTargets [ "sto-site", { X = 24; Y = 24 }, Site BuiltKind.Storage ]

                let { Intents = intents } = decide (atLevel 4 pending) Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the pending census fills the allowance too: nothing re-drops"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a site is a target too: nothing else is planned onto its tile"
            }

            test "the trunks and both container picks keep off the Storage tile" {
                // (24,24) is this fixture's cheapest last step from the
                // source into the spawn: unreserved, the trunk takes it. The
                // reservation is impassable before the trunks are priced, so
                // the lane ends on (24,25) instead. The container picks miss
                // it by construction (ADR 0022): the Storage comes from the
                // clustered ordering, which excludes the working ground,
                // while both container picks draw only from working ground —
                // the Seats and the Upgrade Work Area.
                let colony = withRoadsBuilt (trunkColony 4)
                let { Intents = intents } = decide colony Map.empty Set.empty None

                let storage = sitesOfKind Storage intents |> Set.ofList
                let containers = sitesOfKind Container intents |> Set.ofList

                Expect.isNonEmpty storage "the Storage is planned at RCL4"
                Expect.isNonEmpty containers "the containers drop once their roads stand"

                Expect.isEmpty
                    (Set.intersect storage colony.Spatial.Roads)
                    "no trunk road crosses the Storage's tile"

                Expect.isEmpty
                    (Set.intersect storage containers)
                    "the Storage's tile is never a container's"
            }

            test "the cluster keeps its tiles as the Storage goes from reserved to standing" {
                // The recomputation that can move something: a standing
                // Storage leaves the ordering and frees its slot at once, so
                // the tower and every extension keep the picks they had
                // while the tile was only reserved (ADR 0022).
                let planned = decide (atLevel 4 (openRoom 3)) Map.empty Set.empty None

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let built =
                    decide
                        (atLevel
                            4
                            (openRoom 3
                             |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (sitesOfKind Tower built.Intents)
                    (sitesOfKind Tower planned.Intents)
                    "the tower's pick is the same tile either way"

                Expect.equal
                    (sitesOfKind Extension built.Intents)
                    (sitesOfKind Extension planned.Intents)
                    "and every extension's, in the same order"
            }

            test "a standing Storage changes neither the hauler quota nor the trunk plan" {
                // The spawn stays the trunk hub (ADR 0022): the Storage sits
                // beside it by construction and hires no haul capacity of its
                // own — the quota counts source containers (ADR 0012). One
                // stands on the source's trunk Seat, so the quota is a real
                // number on both sides of the comparison.
                let colony =
                    { trunkColony 4 with
                        Spatial =
                            trunkRoom
                            |> withTargets
                                [ "can-a", { X = 16; Y = 24 }, Structure BuiltKind.Container ]
                    }

                let planned = decide colony Map.empty Set.empty None

                Expect.isGreaterThan
                    planned.Memo.HaulerQuota
                    0
                    "the standing source container hires the haulers the Storage must not"

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let standing =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Obstacles = Set.add storageTile colony.Spatial.Obstacles
                            }
                            |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ]
                    }

                let built = decide standing Map.empty Set.empty None

                Expect.equal
                    built.Memo.HaulerQuota
                    planned.Memo.HaulerQuota
                    "a Storage hires nothing: the quota reads source containers alone"

                Expect.equal
                    (sitesOfKind Road built.Intents)
                    (sitesOfKind Road planned.Intents)
                    "the trunks keep their endpoints — the spawn stays the hub"
            }
        ]

/// Synthetic footing fixture: the source stands three tiles north of the
/// spawn, so its trunk leaves by (24,23) and the source container is
/// planned there, while the Storage takes the ordering's first pick at
/// (24,24). The tile the container's Link footing wants, (23,23), is one
/// of the cluster's own same-colour tiles — the collision the reservation
/// exists to settle (ADR 0022).
let footingRoom = openRoom 6 |> withTargets [ "src-a", { X = 25; Y = 22 }, Source ]

/// The two tiles `footingRoom` holds as Link footings: one beside the
/// planned source container at (24,23), one beside the Storage at (24,24).
/// Two, not four — the count is one per planned source container plus the
/// controller container and the Storage, and this room projects no
/// controller position.
let footingTiles = Set.ofList [ { X = 23; Y = 23 }; { X = 24; Y = 25 } ]

/// The room with a target standing on a tile and blocking it, the way the
/// projection carries a built Storage or link: a target and an obstacle.
let withStanding id pos kind room =
    { withTargets [ id, pos, kind ] room with
        Obstacles = Set.add pos room.Obstacles
    }

/// A footing fixture whose trunks run through the clustered ring: the
/// source stands against the room's east edge and the controller against
/// its west edge, so the source→controller trunk crosses the whole
/// cluster and the tiles a footing pushes the cluster onto are the ones
/// the trunk would otherwise want (ADR 0022, ADR 0027).
let crossedRoom =
    openRoom 6
    |> withTargets [ "src-a", { X = 30; Y = 26 }, Source ]
    |> withStanding "ctrl-1" { X = 19; Y = 25 } Controller

/// The colony one tick on: every site the Layout just asked for now
/// standing in the projection as a construction site, the obstacle kinds
/// blocking their tile exactly as the engine's own sites do — the state
/// the next tick's plan is computed against.
let withPlanPending colony =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    let builtKindOf =
        function
        | Extension -> BuiltKind.Extension
        | Tower -> BuiltKind.Tower
        | Road -> BuiltKind.Road
        | Container -> BuiltKind.Container
        | Storage -> BuiltKind.Storage

    let sites =
        placementIntents intents
        |> List.mapi (fun i (_, pos, kind) -> $"site-{i}", pos, builtKindOf kind)

    let walkable = [ BuiltKind.Road; BuiltKind.Container ]

    { colony with
        Spatial =
            { withTargets [ for id, pos, kind in sites -> id, pos, Site kind ] colony.Spatial with
                Obstacles =
                    (colony.Spatial.Obstacles, sites)
                    ||> List.fold (fun acc (_, pos, kind) ->
                        if List.contains kind walkable then acc else Set.add pos acc)
            }
    }

[<Tests>]
let linkFootingTests =
    testList
        "link footing"
        [
            test "no site lands on a Link footing, at any level" {
                // The footings are held from level 0, levels before the
                // engine unlocks links, because the tile never comes back
                // once an extension takes it — and past RCL4, where links
                // would be allowed, nothing is placed on them either: Link
                // is a built kind with no placeable counterpart, so the
                // Layout emits no site for one at any level (ADR 0022).
                for level in 1..8 do
                    let { Intents = intents } =
                        decide (atLevel level footingRoom) Map.empty Set.empty None

                    Expect.isEmpty
                        (placedTiles intents
                         |> List.filter (fun tile -> Set.contains tile footingTiles))
                        $"RCL{level}: no extension, tower, road or container site sits on a footing"
            }

            test "a footing outranks the extensions: the cluster fills one tile further out" {
                // (23,23) is the ordering's third free pick and the footing
                // beside the source container. The footing wins it, so the
                // cluster takes the next tile instead — the allowance still
                // fills, it just reaches one ring wider.
                let { Intents = intents } = decide (atLevel 3 footingRoom) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    10
                    "the RCL3 allowance still fills whole"

                Expect.isFalse
                    (Set.contains { X = 23; Y = 23 } (clusterTiles intents))
                    "the footing's tile is out of the clustered picks"

                Expect.contains
                    (clusterTiles intents)
                    { X = 22; Y = 24 }
                    "the pick behind the footing is drawn in: nothing is lost, the cluster moves out"
            }

            test
                "a footing may sit on a Seat: the working ground is off-limits to the cluster alone" {
                // This source's trunk leaves by the Seat at (24,22), so that
                // Seat is the container pick and the footing beside it wants
                // (24,23) — another Seat. A footing is the one structure
                // footing allowed on working ground (ADR 0022); were it to
                // dodge Seats the way the ordering does, it would fall
                // through to (25,23) and cost the cluster that tile.
                let colony =
                    atLevel
                        4
                        (openRoom 6
                         |> withTargets
                             [
                                 "src-a", { X = 23; Y = 23 }, Source
                                 "ctrl-1", { X = 30; Y = 25 }, Controller
                             ])

                let { Intents = intents } = decide colony Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 23 } (placedTiles intents))
                    "the Seat beside the container is held, not built on"

                Expect.contains
                    (clusterTiles intents)
                    { X = 25; Y = 23 }
                    "the cluster keeps the tile a working-ground dodge would have cost it"
            }

            test "the footings hold their tiles as the container and the Storage are built" {
                // The reservation becomes a structure: the source container
                // on (24,23), the Storage on (24,24). Both picks are judged
                // from geometry that does not move when they are built, so
                // the footings beside them do not move either — (23,23) is
                // buildable again on this tick and still nothing takes it.
                let built =
                    footingRoom
                    |> withTargets [ "can-a", { X = 24; Y = 23 }, Structure BuiltKind.Container ]
                    |> withStanding "sto-1" { X = 24; Y = 24 } (Structure BuiltKind.Storage)

                let { Intents = intents } = decide (atLevel 4 built) Map.empty Set.empty None

                Expect.isNonEmpty (sitesOfKind Extension intents) "the cluster still fills"

                Expect.isEmpty
                    (placedTiles intents |> List.filter (fun tile -> Set.contains tile footingTiles))
                    "both footings are still held, target built or reserved"
            }

            test "a standing Storage keeps the footing beside it, and a pending one too" {
                // (23,23) is the footing beside the Storage's pick at
                // (24,24). The tick the Storage is placed its reservation
                // leaves the clustered ordering, so the footing reads the
                // site's — and then the structure's — own tile instead, or
                // the tile it was holding falls to the next extension.
                let room =
                    openRoom 6
                    |> withTargets
                        [
                            "src-a", { X = 22; Y = 26 }, Source
                            "ctrl-1", { X = 30; Y = 25 }, Controller
                        ]

                let planOf colony =
                    let { Intents = intents } = decide (atLevel 4 colony) Map.empty Set.empty None
                    intents

                let planned = planOf room

                let pending =
                    planOf (
                        room |> withTargets [ "sto-1", { X = 24; Y = 24 }, Site BuiltKind.Storage ]
                    )

                let built =
                    planOf (
                        room
                        |> withStanding "sto-1" { X = 24; Y = 24 } (Structure BuiltKind.Storage)
                    )

                Expect.equal
                    (sitesOfKind Storage planned)
                    [ { X = 24; Y = 24 } ]
                    "the Storage is planned on the ordering's first pick"

                for (label, intents) in
                    [ "reserved", planned; "pending", pending; "standing", built ] do
                    Expect.isFalse
                        (List.contains { X = 23; Y = 23 } (placedTiles intents))
                        $"{label}: the footing beside the Storage keeps its tile"
            }

            test "a standing link keeps its own footing: the plan is the tick-before plan" {
                // A link is a target, so the tick it goes up its tile stops
                // being buildable — the footing reads standing links back
                // into its candidates rather than jumping to the next tile
                // and costing the cluster a second one. This room's Storage
                // already stands off in the corner, so the source
                // container's footing is the only one bidding near the
                // cluster and the jump would be visible.
                let room =
                    openRoom 6
                    |> withTargets
                        [
                            "src-a", { X = 25; Y = 21 }, Source
                            "ctrl-1", { X = 26; Y = 28 }, Controller
                        ]
                    |> withStanding "sto-1" { X = 29; Y = 29 } (Structure BuiltKind.Storage)

                let linked =
                    room |> withStanding "link-1" { X = 23; Y = 23 } (Structure BuiltKind.Link)

                let before = decide (atLevel 4 room) Map.empty Set.empty None
                let after = decide (atLevel 4 linked) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 23; Y = 23 } (placedTiles after.Intents))
                    "the link's tile leaves the ordering: nothing is planned onto it"

                Expect.equal
                    (placementIntents after.Intents)
                    (placementIntents before.Intents)
                    "the link standing on its footing moves nothing else in the plan"
            }

            test "no clustered structure and no trunk want the same tile" {
                // A footing takes one of the cluster's own picks, so the
                // cluster draws one more tile in behind it — and the
                // reservation the trunk flood was routed around is widened
                // by the footing count for exactly that (ADR 0027), so the
                // drawn-in tile is still ground no trunk was allowed to
                // cross. ADR 0011's precedence survives the push: a road
                // never sits where a structure will.
                let { Intents = intents } = decide (atLevel 4 crossedRoom) Map.empty Set.empty None

                Expect.isNonEmpty (sitesOfKind Road intents) "the trunks are paved"

                Expect.isEmpty
                    (Set.intersect (clusterTiles intents) (sitesOfKind Road intents |> Set.ofList))
                    "the cluster the footings pushed out is still inside the reservation"
            }

            test "the footings survive the placement burst: the next tick asks for nothing" {
                // RCL4 places the whole horizon at once, so the tick after
                // it every gap is zero and the reservation is carried by
                // the sites themselves — except the footings, which no site
                // ever stands on. The widened window is what still holds
                // them (ADR 0027); without it the trunk flood is free to
                // take a footing the moment the cluster is placed, and the
                // Layout emits a road on the tile it had been reserving
                // since level 0, orphaning the roads it just moved off.
                let { Intents = intents } =
                    decide (withPlanPending (atLevel 4 crossedRoom)) Map.empty Set.empty None

                Expect.isEmpty
                    (placementIntents intents)
                    "the whole plan stands where it was asked for: nothing moved, nothing is re-sited"
            }
        ]

/// The trunk colony with one extra target standing (or pending) anywhere.
let withTarget id pos kind colony =
    { colony with
        Spatial = colony.Spatial |> withTargets [ id, pos, kind ]
    }

[<Tests>]
let censusSignatureTests =
    testList
        "census signature"
        [
            // Every census input, perturbed alone, moves the signature —
            // the test surface ADR 0017 demands: a missed input would stall
            // the Layout until a reset instead of failing here.
            test "a structure appearing moves the signature" {
                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the standing census is a signature input"
            }

            test "a standing Storage is its own kind in the signature" {
                let standing kind =
                    trunkColony 2 |> withTarget "sto-1" { X = 24; Y = 24 } (Structure kind)

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (trunkColony 2))
                    "a Storage is a Structure: the standing census carries it (ADR 0022)"

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (standing BuiltKind.Other))
                    "and it is a kind of its own, not the unmodelled kind it used to project as"
            }

            test "a structure moving moves the signature" {
                let colony = trunkColony 2

                let moved =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetPositions =
                                    Map.add
                                        "ext-1"
                                        { X = 24; Y = 27 }
                                        colony.Spatial.TargetPositions
                            }
                    }

                Expect.notEqual
                    (censusSignature moved)
                    (censusSignature colony)
                    "the census is (kind, position), not a count"
            }

            test "a pending site appearing moves the signature" {
                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the pending census is a signature input"
            }

            test "a structure and a site of the same kind on the same tile differ" {
                let standing =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Structure BuiltKind.Container)

                let pending =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Site BuiltKind.Container)

                Expect.notEqual
                    (censusSignature standing)
                    (censusSignature pending)
                    "a site becoming a structure is a census change"
            }

            test "the controller level moves the signature" {
                Expect.notEqual
                    (censusSignature (trunkColony 3))
                    (censusSignature (trunkColony 2))
                    "the level gates allowances, so it is a signature input"
            }

            test "the room name moves the signature" {
                let colony = trunkColony 2

                let renamed =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RoomName = Some "W2N2"
                            }
                    }

                Expect.notEqual
                    (censusSignature renamed)
                    (censusSignature colony)
                    "terrain is keyed by the room, so the name is a signature input"
            }

            test "everything outside the census leaves the signature alone" {
                let colony = trunkColony 2

                let perturbed =
                    { colony with
                        Time = colony.Time + 100
                        RoomEnergy = bank 0 300
                        Sources = [ { source "src-a" with Stocked = false } ]
                        Creeps = [ worker "w1" 25 25 ]
                        Hostiles =
                            [
                                {
                                    Id = "h1"
                                    Pos = { X = 30; Y = 25 }
                                    Body = [ Attack; Move ]
                                }
                            ]
                        ConstructionSites = [ { Id = "site-9" } ]
                        Spatial =
                            ({ colony.Spatial with
                                CreepPositions = Map.ofList [ "w1", { X = 20; Y = 25 } ]
                                Hits = Map.ofList [ "ext-1", { Hits = 1; HitsMax = 3000 } ]
                                Stores = Map.ofList [ "ext-1", 50 ]
                             }
                             |> withTargets [ "pile-1", { X = 22; Y = 25 }, Dropped ])
                    }

                Expect.equal
                    (censusSignature perturbed)
                    (censusSignature colony)
                    "creeps, stores, hits, drops, hostiles, bank and tick are not census"
            }
        ]

/// A memo whose site Intents are a sentinel no computation would produce:
/// reuse is then observable verbatim at the decide seam.
let sentinelMemo snapshot =
    {
        Signature = censusSignature snapshot
        SiteIntents = [ PlaceConstructionSite("W1N1", { X = 1; Y = 1 }, Tower) ]
        HaulerQuota = 0
    }

[<Tests>]
let planMemoTests =
    testList
        "plan memo"
        [
            test "a memo with the matching signature is reused verbatim" {
                let snapshot = trunkColony 2
                let memo = sentinelMemo snapshot
                let decision = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (placementIntents decision.Intents)
                    [ "W1N1", { X = 1; Y = 1 }, Tower ]
                    "the memo's site Intents pass through, nothing recomputes"

                Expect.equal decision.Memo memo "the memo rides out unchanged for next tick"
            }

            test "an added structure invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "an added site invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "a level-up invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)
                let decision = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "the memo's hauler quota feeds spawn planning; a stale one is discarded" {
                // The trunk colony has no source containers, so a fresh
                // quota is 0 and the sentinel's 3 is observable: with a
                // matching memo the hauler gap wins the casting order,
                // with a stale one the fresh quota casts a worker again.
                let snapshot =
                    { trunkColony 2 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let memo =
                    { sentinelMemo snapshot with
                        HaulerQuota = 3
                    }

                let castNames decision =
                    spawnIntents decision.Intents |> List.map (fun (_, _, name) -> name)

                let reused = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (castNames reused)
                    [ "hauler-42-Spawn1" ]
                    "the memo's quota opens a hauler gap the casting order fills first"

                let stale = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (castNames stale)
                    (castNames fresh)
                    "a stale memo's quota is recomputed, never reused"
            }

            test "decide without a memo emits one keyed to this census" {
                let snapshot = trunkColony 2
                let decision = decide snapshot Map.empty Set.empty None

                Expect.equal
                    decision.Memo.Signature
                    (censusSignature snapshot)
                    "the memo carries the census it was computed from"

                let next = decide snapshot Map.empty Set.empty (Some decision.Memo)

                Expect.equal
                    next.Intents
                    decision.Intents
                    "feeding the memo back reproduces the tick verbatim"
            }
        ]

/// Spatial projection holding exactly the given terrain tiles and target
/// positions; absent tiles are outside the projection (impassable). No
/// creep positions and no obstacles — movement tests add those on top.
let spatial targets tiles =
    { SpatialInfo.empty with
        Terrain = Map.ofList tiles
        TargetPositions = Map.ofList targets
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

[<Tests>]
let seatTests =
    testList
        "seat capacity"
        [
            test "a single-Seat source gets exactly one of three empty creeps" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    1
                    "one Seat supports exactly one harvester"

                let harvestIntents =
                    intents
                    |> List.filter (function
                        | HarvestSource _ -> true
                        | _ -> false)

                Expect.hasLength harvestIntents 1 "surplus creeps emit no Harvest intent"
            }

            test "creeps overflowing a single-Seat source are matched elsewhere" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 20; Y = 20 } ]
                                ([ { X = 9; Y = 10 }, Plain ] @ openSeats { X = 20; Y = 20 })

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.hasLength
                    (harvesters assignments "src-b")
                    2
                    "overflow lands on the source with free Seats"
            }

            test "a creep denied a Seat falls through to a lower-rank task" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 25 25 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.contains
                    (assignments |> Map.toList |> List.map snd)
                    (taskId (Upgrade "ctrl-1"))
                    "the denied creep sinks its energy into the controller instead"
            }

            test "Seats are counted from terrain: swamp is a Seat, wall and absent are not" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 } ]
                                [
                                    { X = 9; Y = 10 }, Plain
                                    { X = 11; Y = 10 }, Swamp
                                    { X = 10; Y = 9 }, Wall
                                ]

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    2
                    "plain and swamp neighbours are Seats; wall and off-map are not"
            }

            test "oversold remembered assignments are trimmed back to the Seat count" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let stale =
                    Map.ofList
                        [ "w1", (taskId (Harvest "src-a")); "w2", (taskId (Harvest "src-a")) ]

                let { Assignments = assignments } = decide snapshot stale Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the cap holds even against remembered oversell"
            }

            test "without a spatial projection Harvest stays uncapped" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    3
                    "no terrain data means no cap — today's room behaviour"
            }
        ]

[<Tests>]
let partApplicabilityTests =
    testList
        "part-based applicability"
        [
            test "a Work-less body is never matched to Harvest, Build, or Upgrade" {
                // Energy on board and capacity free: only the missing Work
                // part can make these tasks inapplicable.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Controller = Some(controllerAt 2)
                        Creeps = [ creepWith "hauler" 25 25 [ Carry; Move ] ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Work part can do none of the Work-part tasks"
            }

            test "a Carry-less body is never matched to Refill" {
                // Energy crafted non-zero so only the missing Carry part
                // can make Refill inapplicable.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ creepWith "digger" 25 25 [ Work; Move ] ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Carry part cannot deliver energy"
            }

            test "a remembered assignment to a task the body cannot do is released" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = []
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let remembered = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "applicability release covers parts the body lacks"
            }
        ]

/// Spatial projection of a plain corridor x = 10, y = 9..21 with a source
/// at each end (source tiles are walls): "src-far" at (10, 10), "src-near"
/// at (10, 20).
let nearFarCorridor creepPositions =
    { spatial
          [ "src-far", { X = 10; Y = 10 }; "src-near", { X = 10; Y = 20 } ]
          [
              for y in 9..21 -> { X = 10; Y = y }, (if y = 10 || y = 20 then Wall else Plain)
          ] with
        CreepPositions = Map.ofList creepPositions
    }

[<Tests>]
let travelCostTests =
    testList
        "travel-cost matching"
        [
            test
                "live-bug regression: a fresh creep takes the near source regardless of Snapshot order" {
                // The creep stands three steps from the near source, seven
                // from the far one.
                let snapshotWith (sources: SourceInfo list) =
                    { bareRespawn with
                        Sources = sources
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let far: SourceInfo = source "src-far"
                let near: SourceInfo = source "src-near"

                for sources in [ [ far; near ]; [ near; far ] ] do
                    let { Assignments = assignments } =
                        decide (snapshotWith sources) Map.empty Set.empty None

                    Expect.equal
                        (Map.tryFind "w1" assignments)
                        (Some(taskId (Harvest "src-near")))
                        "the cheaper-to-reach source wins the rank tie"
            }

            test "swamp prices the route: a range-nearer target loses to a longer plain path" {
                // One corridor, a source at each end. src-swamp is 3 tiles
                // away by range but behind two swamp tiles (cost 20);
                // src-plain is 5 tiles away over plain ground (cost 8).
                let corridor =
                    [
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Swamp
                        { X = 10; Y = 14 }, Swamp
                        { X = 10; Y = 15 }, Plain
                        { X = 10; Y = 16 }, Plain
                        { X = 10; Y = 17 }, Plain
                        { X = 10; Y = 18 }, Plain
                        { X = 10; Y = 19 }, Plain
                        { X = 10; Y = 20 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-swamp"; source "src-plain" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial
                                  [
                                      "src-swamp", { X = 10; Y = 12 }
                                      "src-plain", { X = 10; Y = 20 }
                                  ]
                                  corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-plain")))
                    "true path cost decides, not Chebyshev range"
            }

            test "rank dominates: an adjacent Build never outbids a four-tiles-away Refill" {
                // The hungry spawn sits at the top of the corridor, four
                // steps from the creep; the construction site is close
                // enough to build without moving at all.
                let corridor = [ for y in 10..16 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "spawn-1", { X = 10; Y = 10 }; "site-1", { X = 10; Y = 16 } ]
                                  corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                Obstacles = Set.singleton { X = 10; Y = 10 }
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "travel cost breaks ties within a rank, never across ranks"
            }

            test "a sticky assignment is kept even when a cheaper task exists this tick" {
                // Same corridor as the live-bug regression, but the creep
                // already holds the far source from an earlier tick.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-far")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "sticky assignments are never re-evaluated for a closer target"
            }

            test "an unplaced creep is matched as today: Snapshot order decides the tie" {
                // The projection places both sources but not the creep, so
                // no flood can run — the pick falls back to (rank, load).
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor []
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "without a creep position, behaviour is unchanged"
            }

            test "an unreachable Work Area makes the Task inapplicable: the creep sinks lower" {
                // The source's one Seat is walled off from the creep; the
                // controller is reachable. The half-full creep could do
                // either, but Harvest is off the table entirely — no
                // range-based fallback march at a wall.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 10; Y = 16 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                  terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the unreachable Harvest is not applicable to this creep at all"
            }

            test "body-aware cost: the slow heavy body is matched near, the generalist far" {
                // The near source hides behind two swamp tiles (terrain 20);
                // the far one lies nine plain steps away (terrain 18). By
                // bare terrain weight both creeps would march far. Priced
                // by body, the heavy one (5 fatigue parts on 3 Moves) wades
                // the swamps for 17 + 17 = 34 rather than walk nine plains
                // at ceil(10/3) = 4 apiece for 36 — while the generalist's
                // cost equals terrain, so it still takes the far source.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall // src-near
                        { X = 10; Y = 11 }, Swamp // its only Seat
                        { X = 10; Y = 12 }, Swamp
                        { X = 10; Y = 13 }, Plain // the heavy body stands here
                        { X = 11; Y = 13 }, Plain // the generalist beside it
                        yield! [ for y in 14..22 -> { X = 10; Y = y }, Plain ]
                        { X = 10; Y = 23 }, Wall // src-far
                    ]

                let heavy =
                    creepWith "mule" 0 50 [ Work; Work; Work; Work; Work; Carry; Move; Move; Move ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-near"; source "src-far" ]
                        Creeps = [ heavy; worker "runner" 0 50 ]
                        Spatial =

                            { spatial
                                  [ "src-near", { X = 10; Y = 10 }; "src-far", { X = 10; Y = 23 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "mule", { X = 10; Y = 13 }; "runner", { X = 11; Y = 13 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "mule" assignments)
                    (Some(taskId (Harvest "src-near")))
                    "the slow body's real travel time keeps it near"

                Expect.equal
                    (Map.tryFind "runner" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "the generalist stays the cheaper traveller to the far source"
            }
        ]

/// A rectangle of Plain tiles, bounds inclusive.
let plainRect x0 x1 y0 y1 =
    [
        for x in x0..x1 do
            for y in y0..y1 -> { X = x; Y = y }, Plain
    ]

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

[<Tests>]
let movementTests =
    testList
        "movement"
        [
            test "a creep outside its Work Area steps toward the source, acting not yet" {
                // A one-tile-wide plain corridor: x = 10, y = 9..15, with the
                // source tile itself a wall (sources always sit on walls).
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "one single-step move Intent up the corridor"

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
            }

            test "a creep inside its Work Area acts and does not move" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] (openSeats { X = 10; Y = 10 }) with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains intents (HarvestSource("w1", "src-a")) "seated creep harvests"

                Expect.isEmpty (moveIntents intents) "nowhere to go: no move Intent"
            }

            test "the approach detours around swamp when a plain lane is cheaper" {
                // Straight lane x = 10 is swamp (cost 10 each); the lane at
                // x = 11 is plain and reaches a Seat in as many steps.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Swamp
                        { X = 10; Y = 13 }, Swamp
                        { X = 10; Y = 14 }, Plain
                        { X = 11; Y = 11 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", TopRight ]
                    "the first step leaves the swamp lane for the plain one"
            }

            test
                "a loaded worker's first step lands on the road: the paved detour beats the terrain line" {
                // The terrain line runs straight up the plain lane x = 10,
                // three steps to the Seat at (10,11). A paved arc swings
                // out through x = 11..12 — four steps, one more than the
                // line — to the road Seat at (11,11); the unprojected gap
                // at (11,12)/(11,13) keeps the arc from being cut short.
                // The half-loaded worker prices a road step at 2 and a
                // plain step at 4, so the longer paved detour (8) beats the
                // straight terrain line (12): the road sets the first step.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 11; Y = 14 }, Plain
                        { X = 12; Y = 13 }, Plain
                        { X = 12; Y = 12 }, Plain
                        { X = 11; Y = 11 }, Plain
                    ]

                let paved =
                    Set.ofList
                        [
                            { X = 11; Y = 14 }
                            { X = 12; Y = 13 }
                            { X = 12; Y = 12 }
                            { X = 11; Y = 11 }
                        ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                Roads = paved
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Right ]
                    "the first step leaves the terrain line for the paved detour"
            }

            test "a creep in range on a tile it may not keep acts and moves in one tick" {
                // An obstacle structure now sits under the creep (built beneath
                // it), so its tile is no longer Work Area — but the engine
                // judges actions by the tick-start position, so upgrading
                // this tick is still legal while stepping off.
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "ctrl-1", { X = 10; Y = 10 } ]
                                  [
                                      { X = 10; Y = 10 }, Plain
                                      { X = 10; Y = 11 }, Plain
                                      { X = 10; Y = 12 }, Plain
                                  ] with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                Obstacles = Set.ofList [ { X = 10; Y = 10 }; { X = 10; Y = 12 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (UpgradeController("w1", "ctrl-1"))
                    "in range at tick start: the action stays legal"

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "and the creep steps onto the one legal standing tile"
            }

            test "an unreachable Work Area yields no move Intent at all" {
                // The source's Seat exists but the tiles between creep and
                // Seat are outside the projection: no path, so the creep
                // waits instead of thrashing.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 } ]
                                  [ { X = 10; Y = 11 }, Plain; { X = 10; Y = 14 }, Plain ] with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (moveIntents intents) "no path: standing still beats oscillating"
                Expect.isEmpty (actionIntents intents) "and the target is out of range"
            }

            test "a builder works from range 3 without closing in" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "site-1", { X = 10; Y = 10 } ]
                                  [ for y in 10..13 -> { X = 10; Y = y }, Plain ] with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 13 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains intents (BuildSite("w1", "site-1")) "range 3 is close enough"
                Expect.isEmpty (moveIntents intents) "no reason to walk closer"
            }

            test "a refiller two tiles out still has to walk to the structure" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "spawn-1", { X = 10; Y = 10 } ]
                                  [ for y in 10..12 -> { X = 10; Y = y }, Plain ] with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                Obstacles = Set.singleton { X = 10; Y = 10 }
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "transfer needs range 1, so the creep closes in"

                Expect.isEmpty (actionIntents intents) "no transfer from range 2"
            }
        ]

[<Tests>]
let unreachableTests =
    testList
        "unreachable targets"
        [
            test
                "a remembered assignment to an unreachable source is released and its Seat refilled" {
                // src-a's one Seat connects only to w2; w1 sits on a walkable
                // island with no path anywhere, remembering the source from
                // before the wall closed in.
                let terrain =
                    [
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 20; Y = 20 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "w1", { X = 20; Y = 20 }; "w2", { X = 10; Y = 12 } ]
                            }
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w2" ]
                    "the freed Seat goes to the creep that can reach it"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the walled-off creep falls through to the next applicable task"
            }

            test "a creep with no reachable applicable task is left unassigned and emits nothing" {
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                            }
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "the dead-end assignment is released"

                Expect.isEmpty (actionIntents intents) "no action fires at an unreachable target"
                Expect.isEmpty (moveIntents intents) "and no move Intent marches at the wall"
            }

            test "an empty Work Area releases a remembered assignment" {
                // The controller is placed but every tile within upgrade
                // range lies outside the projection: nowhere to stand at all.
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            { spatial [ "ctrl-1", { X = 10; Y = 10 } ] [ { X = 20; Y = 20 }, Plain ] with
                                CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                            }
                    }

                let sticky = Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal (Map.tryFind "w1" assignments) None "no Work Area means no assignment"
            }

            test
                "an unplaced creep keeps its assignment: no reachability filtering without geometry" {
                // Same walled-off source, but the projection does not place
                // the creep — nothing can be proven, so nothing is released.
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "geometry the projection cannot price never releases an assignment"
            }
        ]

/// The tile one step in `direction` from `pos` — mirrors the engine's move.
let stepFrom pos direction =
    match direction with
    | Top -> { pos with Y = pos.Y - 1 }
    | TopRight -> { X = pos.X + 1; Y = pos.Y - 1 }
    | Right -> { pos with X = pos.X + 1 }
    | BottomRight -> { X = pos.X + 1; Y = pos.Y + 1 }
    | Bottom -> { pos with Y = pos.Y + 1 }
    | BottomLeft -> { X = pos.X - 1; Y = pos.Y + 1 }
    | Left -> { pos with X = pos.X - 1 }
    | TopLeft -> { X = pos.X - 1; Y = pos.Y - 1 }

/// Run the Resolver at its own seam: assigned Tasks as data over the
/// snapshot's Atlas; a creep absent from the list is idle. Move Intents
/// only; the movement Verdicts riding beside them are resolveVerdictsOn.
let resolveOn snapshot assigned =
    resolve snapshot (Atlas.ofSnapshot snapshot) (Map.ofList assigned) Set.empty
    |> fst

/// The Resolver's movement Verdicts at the same seam, with the named
/// creeps on the verbose list (ADR 0018).
let resolveVerdictsVerboseOn snapshot assigned verbose =
    resolve snapshot (Atlas.ofSnapshot snapshot) (Map.ofList assigned) (Set.ofList verbose)
    |> snd

/// The same for a quiet colony: nobody on the verbose list.
let resolveVerdictsOn snapshot assigned =
    resolveVerdictsVerboseOn snapshot assigned []

/// Run the Emitter at its own seam, over the same tick-start Atlas.
let emitOn snapshot assigned =
    emit snapshot (Atlas.ofSnapshot snapshot) (Map.ofList assigned)

/// Two single-Seat sources at the ends of a two-tile corridor; each creep
/// stands on the other's Seat.
let headOnSwap =
    let terrain =
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 12 }, Plain
            { X = 10; Y = 13 }, Wall
        ]

    { bareRespawn with
        Sources = [ source "src-a"; source "src-b" ]
        Creeps = [ worker "wa" 0 50; worker "wb" 0 50 ]
        Spatial =

            { spatial [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 10; Y = 13 } ] terrain with
                CreepPositions = Map.ofList [ "wa", { X = 10; Y = 12 }; "wb", { X = 10; Y = 11 } ]
            }
    }

[<Tests>]
let arbitrationTests =
    testList
        "yield arbitration"
        [
            test
                "squatting regression: the upgrader on the sole Seat yields to the inbound harvester" {
                // Source at (10,10) with (10,11) as its only Seat; controller
                // at (10,14), so the Seat is also at upgrade range 3. The
                // upgrader squats the Seat; the harvester stands one tile out.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 14 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "upg", { X = 10; Y = 11 } ]
                            }
                    }

                let assigned = [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ]
                let moves = resolveOn snapshot assigned |> moveIntents

                Expect.contains moves ("har", Top) "the harvester steps onto the Seat"

                Expect.contains
                    (emitOn snapshot assigned)
                    (UpgradeController("upg", "ctrl-1"))
                    "the displaced upgrader still upgrades this tick"

                match moves |> List.filter (fun (name, _) -> name = "upg") with
                | [ (_, direction) ] ->
                    let dest = stepFrom { X = 10; Y = 11 } direction

                    Expect.isLessThanOrEqual
                        (max (abs (dest.X - 10)) (abs (dest.Y - 14)))
                        3
                        "the upgrader is displaced to a tile still inside its Work Area"
                | other -> failtest $"expected exactly one move for the upgrader, got %A{other}"
            }

            test "head-on swap: two creeps blocking each other exchange tiles" {
                let moves =
                    resolveOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ]
                    |> moveIntents

                Expect.equal
                    (moves |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "both creeps move: they swap instead of deadlocking"
            }

            test "pipeline wiring: remembered assignments flow through match, emit, and resolve" {
                // The one arbitration test that still runs the whole decide
                // seam: sticky Assignments survive the Matcher, the Emitter
                // says their glyphs, and the Resolver settles the swap.
                let sticky =
                    Map.ofList
                        [ "wa", (taskId (Harvest "src-a")); "wb", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = next
                    } =
                    decide headOnSwap sticky Set.empty None

                Expect.equal next sticky "the Matcher keeps both remembered assignments"

                Expect.contains
                    intents
                    (SayCreep("wa", "⛏"))
                    "the Emitter's bubbles reach decide's output"

                Expect.equal
                    (moveIntents intents |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "the Resolver's swap reaches decide's output"
            }

            test "an idle creep is displaced by a working creep passing through" {
                // w2 carries no assignment and idles astride the harvester's
                // path.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 50 0 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "w1", { X = 10; Y = 13 }; "w2", { X = 10; Y = 12 } ]
                            }
                    }

                let moves = resolveOn snapshot [ "w1", Harvest "src-a" ] |> moveIntents

                Expect.contains moves ("w1", Top) "the working creep claims the idler's tile"

                Expect.isTrue
                    (moves |> List.exists (fun (name, _) -> name = "w2"))
                    "the idler is displaced out of the way"
            }

            test "a contested tile goes to the higher task rank" {
                // One gap at (10,12): the harvester's and the upgrader's
                // cheapest paths both step onto it. Harvest outranks Upgrade,
                // so the upgrader waits in place.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 8 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                            }
                    }

                let moves =
                    resolveOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ]
                    |> moveIntents

                Expect.equal
                    moves
                    [ "h", Top ]
                    "the harvester takes the gap; the outranked upgrader waits"
            }

            test "within a rank the most-constrained creep places first" {
                // Two Seats; h1 sits on the one h2's cheapest path targets.
                // h2 (one candidate tile) outranks h1 (two) inside the same
                // priority, so h1 shuffles along to the free Seat.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 11; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h1" 0 50; worker "h2" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions =
                                    Map.ofList [ "h1", { X = 10; Y = 11 }; "h2", { X = 9; Y = 12 } ]
                            }
                    }

                let assigned = [ "h1", Harvest "src-a"; "h2", Harvest "src-a" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents |> List.sort)
                    [ "h1", Right; "h2", TopRight ]
                    "h2 claims the occupied Seat; h1 is displaced to the free one"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("h1", "src-a"))
                    "the displaced harvester still harvests this tick"
            }

            test "a builder blocked by a seated harvester still makes progress" {
                // Corridor y=12, x 8..15. Source at (10,11) seats the
                // harvester mid-corridor; the site sits at the far end. The
                // builder's only path runs through the seated harvester's
                // tile — it must not stand idle while a swap (or an in-area
                // shuffle by the harvester) would let it pass.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                            }
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]
                let moves = resolveOn snapshot assigned |> moveIntents

                Expect.isTrue
                    (moves |> List.exists (fun (name, _) -> name = "bob"))
                    "the travelling builder moves instead of stalling behind the seat"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("har", "src-a"))
                    "the harvester still harvests this tick"
            }

            test "a fatigued creep is never asked to move, nor displaced through" {
                // The same one-lane corridor, but the seated harvester is
                // still paying off fatigue: the engine would answer any move
                // with ERR_TIRED, so the Resolver issues none — neither to
                // the harvester nor to the builder whose only path runs
                // through its blocked tile.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                            }
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveOn snapshot assigned |> moveIntents)
                    "no move Intent the engine would refuse with ERR_TIRED"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("har", "src-a"))
                    "the tired harvester still harvests this tick"
            }

            test "a fatigued traveller stands down for the tick instead of failing a move" {
                // The live -11 spam came from loaded travellers: a creep
                // mid-journey with fatigue outstanding used to be issued its
                // next step anyway, which the engine refused every tick.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                Expect.isEmpty
                    (resolveOn snapshot [ "w1", Harvest "src-a" ] |> moveIntents)
                    "a rested copy of this creep would step Top; the tired one is issued nothing"
            }

            test "a travelling builder detours around a seated harvester when a lane is open" {
                // The corridor grows a parallel lane at y = 13. The straight
                // path runs through the seated harvester's tile; the flood
                // prices that tile dearer for the standing creep, so the
                // builder sidesteps into the lane instead of displacing the
                // Seat.
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                            }
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents)
                    [ "bob", BottomRight ]
                    "the builder takes the lane; the seated harvester is left alone"
            }

            test "an occupant with no in-area alternative swaps with its displacer" {
                // The upgrader's only in-area standing tile is the Seat
                // itself: every adjacent walkable tile is outside upgrade
                // range. Displaced, it swaps into the harvester's tile.
                let terrain =
                    [
                        { X = 11; Y = 12 }, Wall
                        { X = 13; Y = 12 }, Wall
                        { X = 10; Y = 12 }, Plain
                        { X = 9; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 9; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 11; Y = 12 }; "ctrl-1", { X = 13; Y = 12 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 9; Y = 12 }; "upg", { X = 10; Y = 12 } ]
                            }
                    }

                let assigned = [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents |> List.sort)
                    [ "har", Right; "upg", Left ]
                    "displacer and occupant exchange tiles"

                Expect.contains
                    (emitOn snapshot assigned)
                    (UpgradeController("upg", "ctrl-1"))
                    "the swapped-out upgrader still upgrades from its tick-start tile"
            }
        ]

[<Tests>]
let resolverVerdictTests =
    testList
        "resolver verdicts"
        [
            test "a grounded creep gets a grounded Verdict; the creep behind it yields to it" {
                // The one-lane corridor with a fatigued seated harvester: har
                // sits arbitration out with its tile blocked, and bob — whose
                // only path runs through that tile — stands down for the tick.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                            }
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "bob", Build "site-1" ])
                    [ Verdict.Grounded "har"; Verdict.Yielded("bob", "har") ]
                    "har is grounded; bob's blocked step names the tired creep holding the tile"
            }

            test "a lone fatigued traveller is grounded, nothing more" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    [ Verdict.Grounded "w1" ]
                    "grounding is the whole story: no move was asked, none was denied"
            }

            test "a displaced squatter's Verdict names its displacer" {
                // The squatting regression's geometry: the upgrader on the
                // sole Seat is displaced by the inbound harvester.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 14 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "upg", { X = 10; Y = 11 } ]
                            }
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("upg", "har") ]
                    "the displaced upgrader yields to the harvester; the harvester says nothing"
            }

            test "losing a contested tile to a higher rank is a yield naming the winner" {
                // The contested-gap geometry: Harvest outranks Upgrade, so
                // the upgrader waits in place while the harvester takes the
                // gap it also wanted.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 8 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                            }
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("u", "h") ]
                    "the outranked upgrader's wait is attributed to the harvester"
            }

            test "the reroute Verdict is manufactured only for a creep on the verbose list" {
                // The two-lane corridor: the builder's straight path runs
                // through the seated harvester's tile, and the surcharge
                // sends it into the parallel lane instead. Nobody yields —
                // the detour is a pricing event, not an arbitration one.
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                  terrain with
                                CreepPositions =
                                    Map.ofList
                                        [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                            }
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveVerdictsOn snapshot assigned)
                    "a quiet colony pays for no second flood, so it records no reroute"

                Expect.isEmpty
                    (resolveVerdictsVerboseOn snapshot assigned [ "har" ])
                    "the list is read per creep: the detourer is not the one being watched"

                Expect.equal
                    (resolveVerdictsVerboseOn snapshot assigned [ "bob" ])
                    [ Verdict.Rerouted "bob" ]
                    "the lane sidestep is attributed to traffic; the seated harvester says nothing"
            }

            test "a creep simply stepping toward its Work Area produces no movement noise" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                Expect.isEmpty
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    "conclusion level means events, not every step"
            }

            test "a clean head-on swap is silent: both creeps settle where they asked" {
                Expect.isEmpty
                    (resolveVerdictsOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ])
                    "each traveller got exactly its preferred tile; nothing became of either move"
            }

            test "movement Verdicts ride behind the Matcher's in decide's output" {
                // A fatigued lone traveller at the decide seam: the Matcher
                // speaks first (the fresh match), the Resolver after (the
                // grounding) — one additive list, interleaved downstream.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Grounded "w1"
                    ]
                    "matcher verdicts first, then the Resolver's, in one list"
            }
        ]

[<Tests>]
let workforceTests =
    testList
        "workforce target"
        [
            // Two sources spaced apart: src-a with three Seats, src-b with
            // two — a Seat total of five.
            let fiveSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 30; Y = 30 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                        { X = 29; Y = 30 }, Plain
                        { X = 31; Y = 30 }, Plain
                    ]

            test "the Seat total raises the target above the floor" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = fiveSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "five Seats support five creeps; two living is a deficit"
            }

            test "no spawn Intent once the workforce matches the Seat total" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ for i in 1..5 -> worker $"w{i}" 0 50 ]
                        Spatial = fiveSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "workforce already at target"
            }

            test "a Seat total below the floor leaves the floor in charge" {
                let oneSeat = spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = oneSeat
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "one Seat cannot lower the target below the floor of two"
            }

            test "an unplaced source contributes no Seats" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = spatial [] []
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "only the floor applies"
            }
        ]

let sayIntents intents =
    intents
    |> List.choose (function
        | SayCreep(name, message) -> Some(name, message)
        | _ -> None)

[<Tests>]
let sayTests =
    testList
        "chat bubbles"
        [
            test "an assigned harvester says the Harvest glyph" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (SayCreep("w1", "⛏"))
                    "the bubble shows the creep's current Task"
            }

            test "each Task has its own glyph: Refill, Build, Upgrade" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0; worker "w2" 50 0; worker "w3" 50 0 ]
                    }

                let sticky =
                    Map.ofList
                        [
                            "w1", (taskId (Refill "spawn-1"))
                            "w2", (taskId (Build "site-1"))
                            "w3", (taskId (Upgrade "ctrl-1"))
                        ]

                let { Intents = intents } = decide snapshot sticky Set.empty None

                Expect.equal
                    (sayIntents intents)
                    [ "w1", "🔋"; "w2", "🔨"; "w3", "⚡" ]
                    "one bubble per assigned creep, glyph matched to its Task"
            }

            test "an unassigned creep says nothing" {
                // Nothing applicable for a full creep: no refill need, no
                // sites, no controller.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (sayIntents intents) "no Task, no bubble"
            }

            test "a creep still walking toward its target says its glyph anyway" {
                // Out of action range: no action Intent this tick, but the
                // assignment holds — the bubble reports it every tick.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
                Expect.equal (sayIntents intents) [ "w1", "⛏" ] "the bubble still shows the Task"
            }
        ]

/// Project one structure of the given built kind carrying the given hits
/// onto a snapshot — position-less: unpriceable geometry never counts
/// against a Task (ADR 0004), so the pool and matching are exercised
/// without terrain.
let withHits id kind hits hitsMax (snapshot: Snapshot) =
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

[<Tests>]
let repairTests =
    testList
        "repair"
        [
            test "a road below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "road-1" BuiltKind.Road 2499 5000
                let half = bareRespawn |> withHits "road-1" BuiltKind.Road 2500 5000

                Expect.equal
                    (repairTasks (planTasks low))
                    [ "road-1" ]
                    "below the trigger: one Repair per ailing road"

                Expect.isEmpty (repairTasks (planTasks half)) "at half hits the road is left alone"
            }

            test "a repaired-whole road leaves the pool" {
                let whole = bareRespawn |> withHits "road-1" BuiltKind.Road 5000 5000
                Expect.isEmpty (repairTasks (planTasks whole)) "a whole road needs nothing"
            }

            test "non-repairable kinds never enter the pool on low hits" {
                // The Snapshot projects hits on repairable kinds only, but the
                // kind gate holds in the Planner regardless of what arrives.
                let snapshot =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 1 5000
                    |> withHits "ext-1" BuiltKind.Extension 1 5000
                    |> withHits "tower-1" BuiltKind.Tower 1 5000

                Expect.isEmpty
                    (repairTasks (planTasks snapshot))
                    "low hits on spawn, extension or tower are a tower's business, not Repair's"
            }

            test "a Storage never enters the Repair pool: it does not decay" {
                // Not a repairable kind (ADR 0023): the Planner's kind gate
                // refuses a Storage's hits even when, as here, the projection
                // carries them.
                let snapshot = bareRespawn |> withHits "sto-1" BuiltKind.Storage 1 5000

                Expect.isEmpty
                    (repairTasks (planTasks snapshot))
                    "the colony's stock is never patched up"
            }

            test "a surplus creep is sent to repair: assignment, intent and bubble" {
                // Feeding satisfied — the spawn is full, the creep can carry no
                // more — so the surplus tier is all that is left, and the
                // half-hit road is its only member.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Repair "road-1")))
                    "the surplus creep is assigned to the Repair"

                Expect.contains
                    intents
                    (RepairStructure("w1", "road-1"))
                    "the assignment emits the repair intent"

                Expect.equal (sayIntents intents) [ "w1", "🔧" ] "a repairing creep says 🔧"
            }

            test "Repair never poaches from the feeding tier" {
                // A hungry spawn and an ailing road bid for the same loaded
                // creep: the feeding tier wins on rank, not pool order.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds itself before it patches roads: rank decided"
            }

            test "Repair never poaches from Harvest either" {
                // A half-loaded creep fits both tiers — room to harvest,
                // energy to spend — and the feeding tier wins on rank.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 25 25 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.Rank) ]
                    "the economy is fed before roads are patched: rank decided"
            }

            test "a container below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "cont-1" BuiltKind.Container 124999 250000
                let half = bareRespawn |> withHits "cont-1" BuiltKind.Container 125000 250000

                Expect.equal
                    (repairTasks (planTasks low))
                    [ "cont-1" ]
                    "below the trigger: one Repair per ailing container"

                Expect.isEmpty
                    (repairTasks (planTasks half))
                    "at half hits the container is left alone"
            }

            test "a whole container produces no Repair" {
                let whole = bareRespawn |> withHits "cont-1" BuiltKind.Container 250000 250000
                Expect.isEmpty (repairTasks (planTasks whole)) "a whole container needs nothing"
            }

            test "container Repair is surplus-tier: feeding still wins the creep" {
                // The same duel the road fights: a hungry spawn and an ailing
                // container bid for one loaded creep, and feeding wins on rank.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 100 250000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds itself before it mends containers: rank decided"
            }

            test "a surplus creep mends the container: assignment, intent and bubble" {
                // Feeding satisfied — spawn full, creep full — so the ailing
                // container is the only work left, exactly like a road.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 100 250000

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Repair "cont-1")))
                    "the surplus creep is assigned to the container Repair"

                Expect.contains
                    intents
                    (RepairStructure("w1", "cont-1"))
                    "the assignment emits the repair intent"

                Expect.equal (sayIntents intents) [ "w1", "🔧" ] "a repairing creep says 🔧"
            }

            test "an empty creep is inapplicable to Repair" {
                // Nothing to spend: no energy makes Repair unworkable, and the
                // remembered assignment is released rather than kept.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let remembered = Map.ofList [ "w1", taskId (Repair "road-1") ]

                let {
                        Verdicts = verdicts
                        Assignments = assignments
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Repair "road-1"), ReleaseReason.Inapplicable))
                    "the empty creep's remembered Repair is released"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "nothing else fits an empty creep here"
            }
        ]

[<Tests>]
let verdictTests =
    testList
        "matcher verdicts"
        [
            test "a lone applicable Task wins as the only candidate" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate) ]
                    "one creep, one candidate: the Verdict names the Task and the walkover"
            }

            test "rank decides: Refill outbids Upgrade for a loaded creep" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the feeding tier beat the surplus tier: rank decided"
            }

            test "rank layers by target: feeding the spawn outbids feeding the tower" {
                // The tower sits first in the pool, so only the target-layered
                // rank (ADR 0010) — not pool order — can hand the spawn the win.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "spawn-1" 50 BuiltKind.Spawn
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction before its guns: rank decided"
            }

            test "travel cost decides: the near source wins the rank tie" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-near"), MatchFactor.TravelCost) ]
                    "same rank, cheaper path: travel cost decided"
            }

            test "load decides: the second creep spreads to the emptier source" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.PoolOrder)
                        Verdict.Matched("w2", taskId (Harvest "src-b"), MatchFactor.Load)
                    ]
                    "w1's tie fell to pool order; w2 avoided the loaded source"
            }

            test "a remembered assignment kept is distinguishable from a fresh match" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-far") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Kept("w1", taskId (Harvest "src-far")) ]
                    "anti-thrash speaks as Kept, never as a fresh Matched"
            }

            test "a Task that left the pool releases with TaskGone" {
                // The remembered Refill target has no free capacity this
                // tick, so the Planner never generates the Task.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Refill "spawn-1") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Refill "spawn-1"), ReleaseReason.TaskGone))
                    "the release names the vanished Task"
            }

            test "a drained source releases its harvester with TaskGone" {
                // Issue #48: anti-thrash must not pin a creep to a dry
                // rock. The Harvest task is simply not pooled (ADR 0013),
                // so the kept assignment dies through the same TaskGone
                // path as any vanished Task.
                let snapshot =
                    { bareRespawn with
                        Sources = [ { source "src-a" with Stocked = false } ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.TaskGone))
                    "the empty source's Harvest no longer exists to be kept"
            }

            test "a creep that fills up releases Harvest as Inapplicable and matches fresh" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable)
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "the handover carries both halves: why released, what won next"
            }

            test "a body that cannot do its remembered Task releases as Inapplicable" {
                // Part-based, not energy-state: the hauler has room to
                // harvest into but no Work part to harvest with.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let sticky = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "hauler",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Inapplicable
                    ))
                    "the missing Work part releases the assignment as Inapplicable"
            }

            test "a walled-off Work Area releases with Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 10; Y = 16 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            { spatial
                                  [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                  terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
                    "no Seat can be reached: the release says so"
            }

            test "a remembered oversell releases with OverCapacity, the loser idles as NoneFree" {
                // One Seat at the source, two creeps remembered on it — an
                // oversell memory can carry across a redeploy. The
                // alphabetically first keeps; nothing else fits the loser.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions =
                                    Map.ofList
                                        [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                            }
                    }

                let sticky =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity)
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap releases the oversell and explains the loser's idleness"
            }

            test "an empty pool idles a creep with NoTasks" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoTasks) ]
                    "the Planner generated nothing at all"
            }

            test "a full creep with only Harvest on offer idles as NoneApplicable" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneApplicable) ]
                    "no Task fit the creep's body or energy state"
            }

            test "an applicable Task with an unreachable Work Area idles as NoneReachable" {
                // The source's one Seat is walled off; nothing else exists.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneReachable) ]
                    "the Task fit and had room, but no path reaches its Work Area"
            }

            test "a dead creep's dropped assignment speaks no Verdict" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = []
                    }

                let sticky = Map.ofList [ "ghost", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot sticky Set.empty None

                Expect.isEmpty (Map.toList assignments) "the dead creep's assignment is dropped"
                Expect.isEmpty verdicts "Verdicts attribute to living creeps only"
            }
        ]

[<Tests>]
let verboseScoringTests =
    testList
        "verbose scoring"
        [
            test "a verbose creep's Scoring covers the whole pool, scores and rejections both" {
                // Loaded and full: Harvest cannot fit the energy state, while
                // Refill and Upgrade score on the full key — no projection, so
                // every travel cost prices at 0.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Inapplicable
                                )
                                Candidate.Scored(taskId (Refill "spawn-1"), 0, 0, 0)
                                Candidate.Scored(taskId (Upgrade "ctrl-1"), 1, 0, 0)
                            ]
                        )
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "every pool Task appears once: scored on the key or rejected at its gate"
            }

            test "a full Task rejects as CapacityFull; only the listed creep gets a Scoring" {
                // One Seat at the source, claimed by w1's match before w2's
                // turn: w2's scoring shows the cap, and its upgrade row shows
                // the empty carry. w1 is off the list and speaks no Scoring.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions =
                                    Map.ofList
                                        [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                            }
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w2" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Scoring(
                            "w2",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.CapacityFull
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap that idled w2 is named per Task; the unlisted creep stays terse"
            }

            test "a kept creep's own single-Seat Task scores as held, never capacity-full" {
                // The creep's own claim is set aside for its scoring: the
                // Task it holds must read as the winning row, not as
                // rejected against its holder's own seat.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                            }
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Scored(taskId (Harvest "src-a"), 0, 0, 0)
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                    ]
                    "the held Task is the scoring's winning row"
            }

            test "a walled-off Work Area rejects as Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                            }
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Unreachable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneReachable)
                    ]
                    "the scoring pinpoints the gate the idle reason summarises"
            }
        ]

[<Tests>]
let tests =
    testList
        "decide"
        [
            test "an empty creep is matched to a Harvest task and remembered" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.contains intents (HarvestSource("w1", "src-a")) "empty creep goes harvesting"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "assignment is remembered"
            }

            test "bare respawn yields exactly one spawn Intent" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None

                match spawnIntents intents with
                | [ (spawnName, body, creepName) ] ->
                    Expect.equal spawnName "Spawn1" "spawns from the only spawn"
                    Expect.isNonEmpty body "body must not be empty"
                    Expect.isNotEmpty creepName "creep needs a name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawn Intent body is affordable at bare-respawn energy" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None

                for (_, body, _) in spawnIntents intents do
                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        300
                        "body cost within bare-respawn energy"
            }

            test "no spawn Intent when energy is below a worker body cost" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 100 300
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
            }

            test "no spawn Intent while the spawn is already spawning" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ { spawn with IsSpawning = true } ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "spawn is busy"
            }

            // Three Seats around src-a: a target of three, so one worker
            // leaves a deficit of two — enough demand for both spawns.
            let threeSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                    ]

            test "two idle spawns in one room spend the shared bank once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (spawnName, _, _) ] ->
                    Expect.equal spawnName "Spawn1" "the first spawn in list order takes the budget"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawns in different rooms each draw their own bank" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                    RoomName = "W2N2"
                                }
                            ]
                        RoomEnergy =
                            bank 300 300 |> Map.add "W2N2" { Available = 300; Capacity = 300 }
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, _, _) -> name))
                    [ "Spawn1"; "Spawn2" ]
                    "full banks in separate rooms fund one body each"
            }

            test "with zero creeps one bank funds two minimal bodies at once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        RoomEnergy = bank 550 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, body, _) -> name, body))
                    [ "Spawn1", [ Work; Carry; Move ]; "Spawn2", [ Work; Carry; Move ] ]
                    "the fallback debits the bank per body instead of waiting on the engine"
            }

            test "at 550 capacity the whole capacity is spent" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 550 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                        "two units plus the 150 remainder as Carry/Carry/Move"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at 300 capacity the remainder pads the single unit" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Carry; Move; Move ]
                        "one unit plus the 100 remainder as a Carry/Move pair"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "below minimum workforce, spawning waits for full capacity" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 400 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (spawnIntents intents)
                    "a living workforce can bank up to a bigger body"
            }

            test "with zero creeps a minimal body is spawned from available energy" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 250 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "an empty colony cannot wait for extensions it cannot refill"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "with zero creeps and unaffordable minimal body, no spawn Intent" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 150 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "even the fallback needs its unit cost"
            }

            test "one worker is below minimum: a second is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.hasLength (spawnIntents intents) 1 "a lone worker cannot keep the loop going"
            }

            test "no spawn Intent when workforce is at minimum" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50; worker "worker-2" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
            }

            test "empty creeps spread across sources instead of piling on one" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None
                let assigned = assignments |> Map.toList |> List.map snd |> List.sort

                Expect.equal
                    assigned
                    [ (taskId (Harvest "src-a")); (taskId (Harvest "src-b")) ]
                    "greedy matching balances load per task"
            }

            test "greedy matching counts kept assignments as load" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "w1 keeps its source"

                Expect.equal
                    (Map.tryFind "w2" assignments)
                    (Some(taskId (Harvest "src-b")))
                    "w2 avoids the occupied source"
            }

            test "assignments pass through unchanged when no creeps died" {
                let assignments = Map.ofList [ "worker-1", (taskId (Harvest "src-a")) ]

                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Assignments = kept } = decide snapshot assignments Set.empty None
                Expect.equal kept assignments "assignments survive the tick"
            }

            test "an assignment sticks across ticks even when greedy would rebalance" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30 ]
                    }

                let assignments = Map.ofList [ "w1", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot assignments Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Harvest "src-b")))
                    "no thrash: creep stays on its source"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-b"))
                    "intent follows the sticky assignment"
            }

            test "a creep that fills up is reassigned from Harvest to Refill" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1"))
                    "delivery intent emitted"
            }

            test "a loaded creep feeds a hungry tower once spawn and extensions are full" {
                // Full feeders leave the pool, so the tower Refill is the one
                // delivery on offer — the same transfer to the creep (ADR 0010).
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "spawn-1" 0 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "tower-1" 500 BuiltKind.Tower
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "tower-1")))
                    "the tower is the delivery that remains"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "tower-1"))
                    "the same transfer intent feeds a tower"
            }

            test "a creep that empties is reassigned from Refill back to Harvest" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Refill "spawn-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes back to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "surplus: a full creep with a full spawn switches to upgrading" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "nothing to refill, so surplus goes to the controller"

                Expect.contains intents (UpgradeController("w1", "ctrl-1")) "upgrade intent emitted"
            }

            test "a hungry structure beats the controller for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "refill outranks upgrade while a structure is missing energy"
            }

            test "an upgrading creep that empties goes back to harvest" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "spent creep returns to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test
                "a full creep with a full spawn and no controller is left unassigned with no intent" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.isEmpty (Map.toList kept) "no applicable task"

                let creepIntents =
                    intents
                    |> List.filter (function
                        | SpawnCreep _ -> false
                        | _ -> true)

                Expect.isEmpty creepIntents "idle creep emits nothing"
            }

            test "a full creep with a construction site and a full spawn goes building" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Build "site-1")))
                    "surplus energy goes into construction"

                Expect.contains intents (BuildSite("w1", "site-1")) "build intent emitted"
            }

            test "an empty creep is never matched to a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Build "site-1")) ]) Set.empty None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes harvesting instead"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "a hungry structure beats a construction site for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "refill outranks build while a structure is missing energy"
            }

            test "assignments of dead creeps are dropped" {
                let assignments = Map.ofList [ "ghost", "task-a" ]
                let { Assignments = kept } = decide bareRespawn assignments Set.empty None
                Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
            }
        ]

let activations intents =
    intents
    |> List.choose (function
        | ActivateSafeMode id -> Some id
        | _ -> None)

/// A hostile creep of the given body standing on the given tile.
let hostileAt id pos body : HostileInfo = { Id = id; Pos = pos; Body = body }

/// A hostile creep of the given body, position immaterial.
let hostile body = hostileAt "h-1" { X = 25; Y = 25 } body

[<Tests>]
let safeModeTests =
    testList
        "safe-mode reflex"
        [
            test "an unplaced controller falls back to firing on sight" {
                // No controller tile in the projection means no deadline to
                // measure — the reflex keeps the old conservative answer.
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostile [ Claim; Claim; Move; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "safe mode fires immediately"
            }

            test "a claimer beyond reach holds the stock — the towers get their window" {
                // attackController is range 1 and judged from tick-start
                // position, so a claimer 4 tiles out cannot tap for at
                // least 3 more ticks; holding costs nothing (ADR 0015).
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "range 4: the tap cannot land yet"
            }

            test "a claimer at the reach deadline fires safe mode" {
                // Range 3 = the precise deadline (2) plus one tile of margin
                // for a skipped tick (ADR 0015).
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "the deadline is now"
            }

            test "a hostile without CLAIM parts does not spend the activation" {
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostile [ Tough; Attack; RangedAttack; Heal; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (activations intents)
                    "fighters cannot touch the controller; the stock is kept"
            }

            test "an empty stock fires nothing" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeAvailable = 0
                                }
                        Hostiles = [ hostile [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "nothing to activate with"
            }

            test "safe mode already running is not re-fired" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeActive = true
                                }
                        Hostiles = [ hostile [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "the room is already protected"
            }

            test "a quiet room fires nothing" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None
                Expect.isEmpty (activations intents) "no hostiles, no reflex"
            }
        ]

let shots intents =
    intents
    |> List.choose (function
        | FireTower(tower, target) -> Some(tower, target)
        | _ -> None)

/// A colony whose towers stand on the given tiles, facing the given hostiles.
let towerColony towers hostiles =
    { bareRespawn with
        Hostiles = hostiles
        Spatial =
            { spatial towers [] with
                TargetKinds =
                    towers |> List.map (fun (id, _) -> id, Structure BuiltKind.Tower) |> Map.ofList
            }
    }

[<Tests>]
let fireReflexTests =
    testList
        "fire reflex"
        [
            test "a tower shoots the hostile in the room" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-1" ] "any hostile is fired on"
            }

            test "the nearest hostile is shot — damage decays with range" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "h-far" { X = 40; Y = 10 } [ Attack; Move ]
                            hostileAt "h-near" { X = 12; Y = 38 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-near" ] "never waste a decayed shot"
            }

            test "equidistant hostiles tie-break by id — the pick is deterministic" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "h-b" { X = 15; Y = 40 } [ Attack; Move ]
                            hostileAt "h-a" { X = 10; Y = 45 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-a" ] "same range: lowest id wins"
            }

            test "each tower picks its own nearest — no focus fire" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "tower-2", { X = 40; Y = 10 } ]
                        [
                            hostileAt "h-a" { X = 12; Y = 38 } [ Attack; Move ]
                            hostileAt "h-b" { X = 38; Y = 12 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (shots intents |> List.sort)
                    [ "tower-1", "h-a"; "tower-2", "h-b" ]
                    "the rule is per-tower, stated once"
            }

            test "a quiet room fires no shot" {
                let snapshot = towerColony [ "tower-1", { X = 10; Y = 40 } ] []
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (shots intents) "no hostile, no reflex"
            }

            test "a towerless room shoots nothing and keeps the safe-mode rule intact" {
                // A clawless hostile before RCL3: no tower to answer with,
                // and the safe-mode stock stays banked (ADR 0007).
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (shots intents) "no tower, no shot"
                Expect.isEmpty (activations intents) "fighters never spend the stock"
            }
        ]

let pickups intents =
    intents
    |> List.choose (function
        | PickupEnergy(creep, pile) -> Some(creep, pile)
        | _ -> None)

/// A colony around a dropped energy pile at (10, 10) on open ground, with
/// the given creeps standing on the given tiles.
let pileColony creeps positions =
    { bareRespawn with
        Sources = []
        Creeps = creeps
        Spatial =
            { spatial
                  [ "pile-1", { X = 10; Y = 10 } ]
                  [
                      for x in 8..12 do
                          for y in 8..12 -> { X = x; Y = y }, Plain
                  ] with
                TargetKinds = Map.ofList [ "pile-1", Dropped ]
                CreepPositions = Map.ofList positions
            }
    }

[<Tests>]
let pickupReflexTests =
    testList
        "pickup reflex"
        [
            test "an adjacent creep with free capacity picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "in reach and hungry: pick up"
            }

            test "a creep standing on the pile picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 10 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "range 0 is within reach"
            }

            test "a full creep leaves the pile alone" {
                let snapshot = pileColony [ worker "w1" 50 0 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "no free capacity, nothing to gain"
            }

            test "a pile out of reach draws nobody — the reflex never moves a creep" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 13 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "range 3: recapture only what is in reach"
            }

            test "every adjacent creep picks — the engine settles duplicates" {
                let snapshot =
                    pileColony
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 11 }; "w2", { X = 9; Y = 10 } ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "w1", "pile-1"; "w2", "pile-1" ]
                    "zero coordination: both reach, both ask"
            }

            test "the pickup rides beside the task's own action" {
                // The creep sits on a Seat of src-a with the pile also in
                // reach: pickup conflicts with no other action, so both
                // Intents are emitted for the same tick.
                let snapshot =
                    { pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ] with
                        Sources = [ source "src-a" ]
                    }

                let withSource =
                    { snapshot with
                        Spatial =
                            { snapshot.Spatial with
                                Terrain = Map.add { X = 11; Y = 11 } Wall snapshot.Spatial.Terrain
                                TargetPositions =
                                    Map.add
                                        "src-a"
                                        { X = 11; Y = 11 }
                                        snapshot.Spatial.TargetPositions
                                TargetKinds = Map.add "src-a" Source snapshot.Spatial.TargetKinds
                            }
                    }

                let { Intents = intents } = decide withSource Map.empty Set.empty None

                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the reflex fires"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-a"))
                    "the assigned task's action still goes out"
            }

            test "a pile keeps no construction site off its tile" {
                // Layout determinism (ADR 0011): a transient pile must not
                // perturb the ordering, so placement with and without the
                // pile is identical.
                let bare = atLevel 2 (openRoom 3)

                let strewn =
                    atLevel 2 (openRoom 3 |> withTargets [ "pile-1", { X = 24; Y = 24 }, Dropped ])

                let placedWith = decide strewn Map.empty Set.empty None
                let placedWithout = decide bare Map.empty Set.empty None

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see piles"
            }
        ]

[<Tests>]
let downgradeDeadlineTests =
    testList
        "downgrade deadline"
        [
            test "a controller near downgrade outranks refill for a loaded creep" {
                // A downgrade zeroes the safe-mode stock, so the timer is a
                // hard deadline, not surplus-rank work.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = 4000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the deadline escalates Upgrade above the feeding tier"
            }

            test "the deadline scales with level: RCL4 at 15,000 is already urgent" {
                // The engine refuses activateSafeMode below half the level's
                // full timer minus 5,000 — at RCL4 that is 15,000. Escalating
                // at half (20,000) keeps the reflex's activation legal with
                // the whole 5,000-tick grace intact.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 15000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a flat deadline would sleep through RCL4's refusal threshold"
            }

            test "RCL4 above half its timer is not urgent" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 25000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "above half the timer, upgrade stays surplus work"
            }

            test "far from the deadline upgrade stays surplus work" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "a fresh timer changes nothing"
            }
        ]

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

[<Tests>]
let anchorTests =
    testList
        "anchor"
        [
            test "an Anchor on a plain Seat walks to the Post instead of digging there" {
                // The W12S28 bug (ADR 0020): controller far from every Seat,
                // a built container on the Seat at (9,10) — the source's one
                // Post — and the Anchor standing on the plain Seat (10,11).
                // Harvesting there would fill its single Carry in four ticks
                // and hand it to Upgrade, five tiles away; instead it holds
                // its dig and walks.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { spatial
                                  [
                                      "src-a", { X = 10; Y = 10 }
                                      "ctrl-1", { X = 40; Y = 40 }
                                      "cont-1", { X = 9; Y = 10 }
                                  ]
                                  (openSeats { X = 10; Y = 10 }) with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "src-a", Source
                                            "ctrl-1", Controller
                                            "cont-1", Structure BuiltKind.Container
                                        ]
                                CreepPositions = Map.ofList [ "a1", { X = 10; Y = 11 } ]
                            }
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (actionIntents intents
                     |> List.filter (function
                         | HarvestSource _ -> true
                         | _ -> false))
                    "off the Post a heavy body does not dig, so it never fills"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopLeft) ]
                    "it steps toward the container Seat"

                Expect.contains
                    verdicts
                    (Verdict.Matched("a1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "and stays matched to Harvest the whole walk"
            }

            test "an Anchor that is already full off-Post commutes once, then settles" {
                // The deployment path ADR 0020 names: a full store off the
                // Post still catches no overflow, so Harvest is inapplicable
                // and the body spends its load at the controller one last
                // time. Nothing pins it until it is empty again — pinned
                // here so the one-time heal stays a decision, not a
                // surprise.
                let room =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 14; Y = 11 }
                              "cont-1", { X = 9; Y = 10 }
                          ]
                          (openSeats { X = 10; Y = 10 }
                           @ [ for x in 11..14 -> { X = x; Y = 11 }, Plain ]) with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "cont-1", Structure BuiltKind.Container
                                ]
                        CreepPositions = Map.ofList [ "a1", { X = 10; Y = 11 } ]
                    }

                let full =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = room
                    }

                let { Verdicts = verdicts } = decide full Map.empty Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Matched("a1", taskId (Upgrade "ctrl-1"), MatchFactor.OnlyCandidate))
                    "a full store off the Post catches no overflow: only Upgrade is left"

                // Emptied at the controller, the same body is pulled home.
                let emptied =
                    { full with
                        Creeps = [ anchor "a1" 0 50 ]
                    }

                let { Intents = intents } = decide emptied Map.empty Set.empty None

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopLeft) ]
                    "empty again, it walks to the Post and stays there"
            }

            test "a Dual Seat and banked capacity plan an Anchor body" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "the Anchor row sized to the 300 bank"

                    Expect.stringStarts creepName "anchor-" "the name carries the anchor row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "without a Dual Seat only generalists are planned" {
                // Same Seats, controller placed far away: no Seat falls in
                // its Upgrade Work Area, so there is no Dual Seat to cast for.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetPositions =
                                    Map.ofList
                                        [
                                            "src-a", { X = 10; Y = 10 }
                                            "ctrl-1", { X = 40; Y = 40 }
                                        ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body (workerBodyFor 300) "the worker row sized to the bank"
                    Expect.stringStarts creepName "worker-" "the name carries the worker row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a source-container Post with no Dual Seat casts an Anchor at full bank" {
                // The W12S28 shape: controller far from every Seat, but a
                // built container stands on the Seat at (9,10) — a Post,
                // so the Anchor row comes alive without any Dual Seat.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetPositions =
                                    Map.ofList
                                        [
                                            "src-a", { X = 10; Y = 10 }
                                            "ctrl-1", { X = 40; Y = 40 }
                                            "cont-1", { X = 9; Y = 10 }
                                        ]
                                TargetKinds =
                                    dualSeatRoom.TargetKinds
                                    |> Map.add "cont-1" (Structure BuiltKind.Container)
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "the Anchor row sized to the 300 bank"

                    Expect.stringStarts creepName "anchor-" "the container Seat is a Post"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a capped Anchor is cast the tick its bank holds the body's cost, not a full bank" {
                // ADR 0021: at RCL4 the bank caps at 1,300 but the Anchor
                // row prices at 700 (6W1C1M); waiting for a full bank would
                // hold every Anchor replacement past RCL3 for nothing.
                let snapshot =
                    { dualSeatColony with
                        RoomEnergy = bank 700 1300
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "the capped Anchor row, priced at exactly the bank's holding"

                    Expect.stringStarts creepName "anchor-" "the name carries the anchor row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a bank short of the capped Anchor's cost still waits" {
                let snapshot =
                    { dualSeatColony with
                        RoomEnergy = bank 650 1300
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (spawnIntents intents) "650 does not buy 6W1C1M"
            }

            test "the Anchor quota counts Posts: a Dual Seat plus a container Seat want two" {
                // One living Anchor covers the Dual Seat; the built
                // container on the other Seat is a second Post, so the
                // remaining gap is cast from the anchor row, not generalist.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetPositions =
                                    dualSeatRoom.TargetPositions
                                    |> Map.add "cont-1" { X = 9; Y = 10 }
                                TargetKinds =
                                    dualSeatRoom.TargetKinds
                                    |> Map.add "cont-1" (Structure BuiltKind.Container)
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "anchor-" "the second Post's gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a living Anchor fills the quota: the remaining gap goes generalist" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the one Dual Seat is already worked"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            // Three Seats — (11,10) the Dual Seat, (9,10) and (9,9) ordinary —
            // and a second idle spawn drawing from the same bank.
            let threeSeatRoom =
                { dualSeatRoom with
                    Terrain =
                        Map.ofList
                            [
                                { X = 9; Y = 10 }, Plain
                                { X = 11; Y = 10 }, Plain
                                { X = 9; Y = 9 }, Plain
                            ]
                }

            let secondSpawn =
                { spawn with
                    Name = "Spawn2"
                    Id = "spawn-2"
                }

            test "the Anchor gap is filled before generalist gaps" {
                let snapshot =
                    { dualSeatColony with
                        Spawns = [ spawn; secondSpawn ]
                        RoomEnergy = bank 600 300
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeatRoom
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, firstBody, firstName); (_, _, secondName) ] ->
                    Expect.stringStarts firstName "anchor-" "the Anchor gap is filled first"
                    Expect.equal firstBody [ Work; Work; Carry; Move ] "the Anchor row's body"
                    Expect.stringStarts secondName "worker-" "the generalist fills the remainder"
                | other -> failtest $"expected exactly two SpawnCreep intents, got %A{other}"
            }

            test "planned creeps never exceed the workforce target" {
                // One Post and nine income workers make a target of ten
                // (ADR 0012); nine living leave one gap — the second idle
                // spawn must stay quiet even with energy banked for it.
                let snapshot =
                    { dualSeatColony with
                        Spawns = [ spawn; secondSpawn ]
                        RoomEnergy = bank 600 300
                        Creeps = anchor "a1" 0 50 :: [ for i in 1..8 -> worker $"w{i}" 0 50 ]
                        Spatial = threeSeatRoom
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "the Anchor quota lives inside the target, never on top of it"
            }

            test "an empty Anchor on its Dual Seat is assigned Harvest without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "an empty store calls for Harvest"

                Expect.contains intents (HarvestSource("a1", "src-a")) "the action fires in place"
                Expect.isEmpty (moveIntentsFor "a1" intents) "no movement step is emitted"
            }

            test "a full Anchor on its Dual Seat is assigned Upgrade without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { dualSeatRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a full store calls for Upgrade"

                Expect.contains
                    intents
                    (UpgradeController("a1", "ctrl-1"))
                    "the action fires in place"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no movement step is emitted"
            }

            test
                "alternation is emergent: a filled-up Anchor's Harvest releases and rematches to Upgrade" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { dualSeatRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "ordinary applicability release + rematch flips the assignment"
            }

            // The Dual Seat room extended east: a plain corridor from
            // (12,10) to (30,10) carrying distant mobile work at its end.
            let corridorEast extraTargets =
                { dualSeatRoom with
                    TargetPositions =
                        (Map.toList dualSeatRoom.TargetPositions @ extraTargets) |> Map.ofList
                    Terrain =
                        (Map.toList dualSeatRoom.Terrain
                         @ [ for x in 12..30 -> { X = x; Y = 10 }, Plain ])
                        |> Map.ofList
                }

            test "a distant Build flows to the generalist; the Anchor upgrades in place" {
                let snapshot =
                    { dualSeatColony with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ anchor "a1" 50 0; worker "g1" 50 0 ]
                        Spatial =
                            { corridorEast [ "site-1", { X = 31; Y = 10 } ] with
                                CreepPositions =
                                    Map.ofList
                                        [ "a1", { X = 11; Y = 10 }; "g1", { X = 29; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Build "site-1")))
                    "the mobile body takes the distant site"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the slow heavy body stays where it is valuable"
            }

            test "a distant Refill flows to the generalist; the empty Anchor harvests" {
                let snapshot =
                    { dualSeatColony with
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ anchor "a1" 0 50; worker "g1" 50 0 ]
                        Spatial =
                            { corridorEast [ "spawn-1", { X = 31; Y = 10 } ] with
                                CreepPositions =
                                    Map.ofList
                                        [ "a1", { X = 11; Y = 10 }; "g1", { X = 30; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the loaded mobile body delivers"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the empty Anchor works its Seat instead"
            }

            test "the disaster fallback still spawns bare worker units beside a Dual Seat" {
                let snapshot = { dualSeatColony with Creeps = [] }
                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | (_, body, creepName) :: _ ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "time-to-first-creep outranks specialisation"

                    Expect.stringStarts creepName "worker-" "the fallback casts the worker row"
                | [] -> failtest "expected the fallback to spawn"
            }
        ]

/// The haul fixture (ADR 0012): a plain corridor y = 10, x = 9..21; the
/// source embedded in wall at (10,10) with Seats (9,10) and (11,10), the
/// controller standing at (20,10); the source container "can-src" on the
/// Seat (11,10) and the controller container "can-ctrl" at (18,10),
/// inside the Upgrade Work Area. The buffer starts stocked, the source
/// container empty.
let haulRoom =
    { spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
        Obstacles = Set.singleton { X = 20; Y = 10 }
        Stores = Map.ofList [ "can-src", 0; "can-ctrl", 800 ]
    }
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

let withdrawTasks tasks =
    tasks
    |> List.choose (function
        | Withdraw containerId -> Some containerId
        | _ -> None)

let refillTasks tasks =
    tasks
    |> List.choose (function
        | Refill structureId -> Some structureId
        | _ -> None)

[<Tests>]
let logisticsTests =
    testList
        "logistics"
        [
            test "a stocked container yields a Withdraw Task; an empty one yields none" {
                Expect.equal
                    (withdrawTasks (planTasks haulColony))
                    [ "can-ctrl" ]
                    "the stocked buffer enters the pool; the empty source container does not"
            }

            test
                "the controller container with room is a Refill target; source containers never are" {
                let snapshot =
                    { haulColony with
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                    }

                let tasks = planTasks snapshot

                Expect.equal
                    (refillTasks tasks)
                    [ "can-ctrl" ]
                    "only the buffer is a Refill target, however stocked the source container"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "can-src" ]
                    "both stocked containers stay Withdraw Tasks"
            }

            test "a full controller container is no Refill target, but stays a Withdraw" {
                let snapshot =
                    { haulColony with
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-ctrl", 2000 ]
                            }
                    }

                let tasks = planTasks snapshot
                Expect.isEmpty (refillTasks tasks) "no room left to refill"
                Expect.equal (withdrawTasks tasks) [ "can-ctrl" ] "still stocked to draw from"
            }

            test "an empty creep between source and stocked container is matched by travel cost" {
                // At (15,10) the buffer's Work Area is two steps away, the
                // nearest Seat four: collect beats dig. At (12,10) the Seat
                // is one step away: dig beats collect. Same rule both ways.
                let colonyAt pos =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "w1", pos ]
                            }
                    }

                let near = decide (colonyAt { X = 15; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" near.Assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the cheaper-to-reach buffer wins the feeding-tier tie"

                Expect.contains
                    near.Verdicts
                    (Verdict.Matched("w1", taskId (Withdraw "can-ctrl"), MatchFactor.TravelCost))
                    "the match speaks its Verdict: travel cost decided"

                let far = decide (colonyAt { X = 12; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" far.Assignments)
                    (Some(taskId (Harvest "src-a")))
                    "nearer the source, digging wins the same tie"
            }

            test "a heavy-Work body never collects: the far Post's Harvest beats the near buffer" {
                // Same geometry where the worker above picks Withdraw — at
                // (15,10) the buffer is two steps, the nearest Seat four.
                // Work > Move makes Withdraw inapplicable (ADR 0016), so
                // the anchor's only feeding-tier candidate is Harvest and
                // the unmanned Post wins regardless of distance.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 15; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the stocked buffer never outbids the Post for a Work-heavy body"
            }

            test "a kept Withdraw on a heavy-Work body releases as inapplicable and digs" {
                // Deployment heals the live colony without a death: the
                // remembered orbit breaks the first tick the gate lands.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 15; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Withdraw "can-ctrl"),
                        ReleaseReason.Inapplicable
                    ))
                    "the gate releases the remembered collection"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the rematch walks the anchor home"
            }

            test "alternation is emergent: a filled-up creep's Withdraw releases and rematches" {
                // The creep filled up inside the buffer's Work Area — which
                // is also the controller's. Withdraw loses applicability;
                // the rematch sinks the load into Upgrade, never back into
                // the container it just drew from.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "w1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Withdraw "can-ctrl"),
                        ReleaseReason.Inapplicable
                    ))
                    "the full store releases Withdraw"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the rematch flips to Upgrade, like the Anchor's harvest↔upgrade"
            }

            test "the alternation's other half: an emptied creep's Upgrade releases into Withdraw" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "w1", taskId (Upgrade "ctrl-1") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the empty store tops up from the buffer one tile away"
            }

            test "spawn-feeding Refill still outranks the buffer Refill" {
                // The spawn stands mid-corridor, two steps from the loaded
                // creep; the buffer's Work Area costs nothing at all. Rank
                // dominates: reproduction is fed before the buffer.
                let snapshot =
                    { haulColony with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                Obstacles = Set.ofList [ { X = 14; Y = 10 }; { X = 20; Y = 10 } ]
                                CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                            }
                            |> withTargets
                                [ "spawn-1", { X = 14; Y = 10 }, Structure BuiltKind.Spawn ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the buffer never outbids feeding the spawn"
            }

            test "a loaded Carry-only body is the buffer's Refill worker" {
                // The buffer's tier sits below every surplus Task, so
                // Work-bodied creeps pass it by — but a full hauler-shaped
                // body has no surplus work of its own, and the outflow
                // lands on it.
                let snapshot =
                    { haulColony with
                        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "h1", { X = 15; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the buffer Refill is live work for a body that can do nothing better"
            }

            test "a seated Withdraw emits the engine withdraw call and speaks 📥" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("w1", "can-ctrl"))
                    "in range at tick start: the Executor-bound Intent fires"

                Expect.contains intents (SayCreep("w1", "📥")) "the Task's own chat bubble"
            }
        ]

[<Tests>]
let containerPostTests =
    testList
        "container post garrison"
        [
            // The garrison rule (#47, ADR 0012): a full creep standing on a
            // built source container keeps Harvest — the engine drops the
            // overflow into the container underfoot, so the creep
            // effectively has capacity. Everywhere else the ordinary
            // full-store rule stands.
            test "a full Anchor on a built source container keeps its Harvest across ticks" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the overflow falls into the container: the Post stays garrisoned"

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "kept, never released as Inapplicable"

                Expect.contains
                    intents
                    (HarvestSource("a1", "src-a"))
                    "the dig keeps firing past a full store"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no drift off the Post"
            }

            test "a full Anchor on the container matches Harvest fresh, not just from memory" {
                // Both gates — the remembered-assignment release and the
                // fresh judge — must read the same widened rule, or the
                // Anchor would be matched and released in alternate ticks.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the feeding-tier dig at cost 0 wins the fresh match too"
            }

            test "a full worker on the container releases Harvest: the garrison is body-aware" {
                // The squat of #67 (ADR 0024): body-blind, this widening let
                // a light body that filled up on the Post keep Harvest for
                // the rest of its life — never Inapplicable, so anti-thrash
                // never let the tile go — while the Anchor cast for that
                // Post read `none-free`. Only a garrisoning body's overflow
                // keeps the dig past a full store.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "w1", { X = 11; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "a light body's full store ends its dig, container or no container"
            }

            test "a full Anchor on a bare Seat still releases Harvest" {
                // (9,10) is a Seat of src-a with no container: harvesting
                // past a full store there spills onto the ground.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 9; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "no container underfoot: the ordinary full-store rule stands"
            }

            test "a full creep beside the built container still releases Harvest" {
                // (12,10) touches the container at (11,10) but stands off
                // it: adjacency catches nothing — only the tile itself.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 12; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "the widening reads the creep's own tile, never a neighbour"
            }

            test "a container construction site catches no overflow: Harvest still releases" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds =
                                    haulRoom.TargetKinds
                                    |> Map.add "can-src" (Site BuiltKind.Container)
                                CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "a pending container is not yet a container"
            }

            test "a built container off the Seats widens nothing: Harvest still releases" {
                // The controller container's tile is no Seat of src-a — a
                // full creep standing on it is nowhere the overflow rule
                // helps, however built the container underfoot.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "a1", { X = 18; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "only that source's own container Seat catches its overflow"
            }
        ]

[<Tests>]
let postCapacityTests =
    testList
        "post capacity"
        [
            // The over-admission half of #67 (ADR 0024): a Work-heavy body's
            // Harvest Work Area is that source's Posts (ADR 0020), so the
            // Seat count admits garrisons to standing room that does not
            // exist. `haulRoom`'s src-a has two Seats and one Post.
            test "a source's Posts cap its heavy harvesters, however many Seats it has" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0; anchor "a2" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions =
                                    Map.ofList
                                        [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                            }
                    }

                let remembered =
                    Map.ofList [ "a1", taskId (Harvest "src-a"); "a2", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity))
                    "one Post seats one garrison: the second Anchor is released, not left to crowd it"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the Post's holder keeps the dig"
            }

            test "a fresh heavy body is not matched to a source whose Post is taken" {
                // Both gates read the same cap, or the second Anchor would be
                // released and rematched in alternate ticks.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions =
                                    Map.ofList
                                        [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the fresh match stops at the Post count too"
            }

            test "a light body still fills every Seat: the Post cap governs heavy bodies alone" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions =
                                    Map.ofList [ "w1", { X = 11; Y = 10 }; "w2", { X = 9; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1"; "w2" ]
                    "a light body stands on any Seat, so the Seat count is its only cap"
            }

            test "a source with no Post caps heavy harvesters at its Seats" {
                // The pre-container fallback (ADR 0020): with nothing built,
                // a heavy body harvests from any Seat, so a Post cap of zero
                // would strand the colony instead of ordering it.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds =
                                    haulRoom.TargetKinds
                                    |> Map.add "can-src" (Site BuiltKind.Container)
                                CreepPositions =
                                    Map.ofList [ "a1", { X = 11; Y = 10 }; "a2", { X = 9; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "a2" ]
                    "no Post derives no Post cap: both Seats are open"
            }
        ]

/// A hauler-unit creep: two Carry, one Move — the hauler row's block.
let hauler name energy freeCapacity =
    creepWith name energy freeCapacity [ Carry; Carry; Move ]

/// The hauler quota fixture: a 3-wide field y = 9..11 from x = 8 to one
/// tile past the spawn, the source embedded in wall at (10,10) with its
/// eight Seats open — a seat-based target roomy enough to leave the
/// hauler row slots — the built source container "can-src" on the Seat
/// (11,10) (a Post), and the spawn structure standing at (spawnX,10).
let quotaRoom spawnX =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "can-src", { X = 11; Y = 10 }
              "spawn-1", { X = spawnX; Y = 10 }
          ]
          [
              for x in 8 .. spawnX + 1 do
                  for y in 9..11 -> { X = x; Y = y }, (if x = 10 && y = 10 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "can-src", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
        Obstacles = Set.singleton { X = spawnX; Y = 10 }
    }

/// The quota fixture's colony: `spawnCount` idle spawns drawing on the one
/// 300-capacity bank holding `available` energy.
let quotaColony spawnX spawnCount available =
    { bareRespawn with
        Spawns =
            [
                for i in 1..spawnCount ->
                    { spawn with
                        Name = $"Spawn{i}"
                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                    }
            ]
        RoomEnergy = bank available 300
        Sources = [ source "src-a" ]
        Spatial = quotaRoom spawnX
    }

let haulerCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "hauler-")
    |> List.length

[<Tests>]
let haulerTests =
    testList
        "hauler"
        [
            test "a farther source container hires a larger hauler quota" {
                // Near spawn: 8 steps from the container, [4 Carry; 2 Move]
                // at the 300-capacity bank — 20 round-trip ticks, quota
                // ceil(20 x 10 / 200) = 1. Far spawn: 27 steps — 68 ticks,
                // quota 4. The living Anchor fills the Post, so every
                // remaining specialist gap is a hauler cast.
                let decideAt spawnX =
                    decide
                        { quotaColony spawnX 4 1200 with
                            Creeps = [ anchor "a1" 0 50 ]
                        }
                        Map.empty
                        Set.empty
                        None

                let near = decideAt 20
                let far = decideAt 39

                Expect.equal (haulerCasts near.Intents) 1 "8 steps ship in one body"
                Expect.equal (haulerCasts far.Intents) 4 "27 steps hire four"
            }

            test "no source containers hires no haulers" {
                let room = quotaRoom 39

                let snapshot =
                    { quotaColony 39 4 1200 with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { room with
                                TargetPositions = Map.remove "can-src" room.TargetPositions
                                TargetKinds = Map.remove "can-src" room.TargetKinds
                            }
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal (haulerCasts intents) 0 "no container, nothing to ship"

                Expect.all
                    (spawnIntents intents)
                    (fun (_, _, name) -> name.StartsWith "worker-")
                    "every cast is the generalist row"
            }

            test "casting order runs Anchor, then hauler, then worker from the one debited bank" {
                let snapshot =
                    { quotaColony 20 3 900 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, firstBody, firstName)
                    (_, secondBody, secondName)
                    (_, thirdBody, thirdName) ] ->
                    Expect.stringStarts firstName "anchor-" "the Post's gap is filled first"
                    Expect.equal firstBody [ Work; Work; Carry; Move ] "the Anchor row's body"
                    Expect.stringStarts secondName "hauler-" "the hauler quota comes second"

                    Expect.equal
                        secondBody
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        "two whole blocks at the 300 bank"

                    Expect.stringStarts thirdName "worker-" "the generalist fills the remainder"
                    Expect.equal thirdBody (workerBodyFor 300) "the worker row sized to the bank"
                | other -> failtest $"expected exactly three SpawnCreep intents, got %A{other}"
            }

            test "the disaster fallback still casts a bare worker unit" {
                let snapshot =
                    { quotaColony 20 1 300 with
                        Creeps = []
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body [ Work; Carry; Move ] "time-to-first-creep outranks the rows"
                    Expect.stringStarts creepName "worker-" "the fallback casts the worker row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an empty hauler body is matched into Withdraw" {
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                                CreepPositions = Map.ofList [ "h1", { X = 12; Y = 10 } ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-src")))
                    "the intake half of the haul cycle: free capacity beside a stocked container"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "can-src"))
                    "in range at tick start: the withdraw fires"
            }

            test "a hauler's intake is a source container, never the upgrade buffer" {
                // The buffer stands one step away and the source container
                // six, yet a body with no Work part can spend nothing at the
                // controller: drawing from the buffer only sends energy back
                // the way it came.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                                CreepPositions = Map.ofList [ "h1", { X = 17; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-src")))
                    "the buffer is inapplicable by body; the far source container is the intake"
            }

            test "the buffer stays intake for a Work body: the worker row still draws" {
                // The gate reads the target's kind beside the body, so the
                // colony's own worker row — four Work, four Carry, four
                // Move — keeps the buffer it upgrades from.
                let snapshot =
                    { haulColony with
                        Creeps =
                            [
                                creepWith
                                    "w1"
                                    0
                                    100
                                    [
                                        Work
                                        Work
                                        Work
                                        Work
                                        Carry
                                        Carry
                                        Carry
                                        Carry
                                        Move
                                        Move
                                        Move
                                        Move
                                    ]
                            ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                                CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                            }
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "a body that can spend at the controller draws from the buffer beside it"
            }

            test "with only the buffer stocked, a hauler idles instead of cycling it" {
                // The loop this gate closes: every other sink full, the
                // hauler's only Refill target is the buffer it just drew
                // from, so it emptied and refilled the same container tick
                // after tick without ever delivering.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                CreepPositions = Map.ofList [ "h1", { X = 17; Y = 10 } ]
                            }
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal (Map.tryFind "h1" assignments) None "no intake a hauler may draw from"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("h1", IdleReason.NoneApplicable))
                    "the idle Verdict names the body gate"
            }

            test "filled, the hauler's Withdraw releases and rematches to Refill" {
                // No Work part: Harvest, Build, Upgrade and Repair are
                // inapplicable by body, so the outflow is the only work
                // left — the same emergent alternation as every Task pair.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 100 0 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                                CreepPositions = Map.ofList [ "h1", { X = 12; Y = 10 } ]
                            }
                    }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "can-src") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("h1", taskId (Withdraw "can-src"), ReleaseReason.Inapplicable))
                    "the full store releases Withdraw"

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the rematch flips to the outflow"
            }
        ]

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
        Obstacles = Set.singleton { X = 20; Y = 10 }
    }

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
        RoomEnergy = bank 1200 300
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = incomeRoom
    }

/// The income-based fleet the W12S28 shape pins (ADR 0012): one Anchor
/// per Post (2), the throughput quota (1 hauler per container at 8 steps),
/// and the income workers — 2 posted sources × 10 e/tick × the 1500-tick
/// lifetime = 30,000, minus the anchor and hauler rows' replacement
/// amortization (2 × 300 + 2 × 300 = 1,200), over one worker body's Work
/// drain × lifetime (1 × 1500) → floor(19.2) = 19.
let incomeFleet =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100; hauler "h2" 0 100 ]
    @ [ for i in 1..19 -> worker $"w{i}" 0 50 ]

[<Tests>]
let incomeWorkforceTests =
    testList
        "income-based workforce"
        [
            test "the W12S28 fleet is the whole target: 2 Anchors + 2 haulers + 19 workers" {
                // Each posted source retires its 8 Seats: a seat base would
                // add 16 on top and the idle spawns would cast into it.
                let snapshot =
                    { incomeColony with
                        Creeps = incomeFleet
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "the fleet already matches the target"
            }

            test "amortization is deducted: one worker short casts exactly one worker" {
                // Without the anchor/hauler replacement deduction income
                // would feed 20 workers and this gap would draw two casts.
                let snapshot =
                    { incomeColony with
                        Creeps = List.truncate (List.length incomeFleet - 1) incomeFleet
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the gap is a worker gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]
