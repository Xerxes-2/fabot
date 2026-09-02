module Fabot.Core.Tests.DecideTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

/// A single idle spawn with the given banked energy and room capacity.
let spawn energy capacity =
    {
        Name = "Spawn1"
        EnergyAvailable = energy
        EnergyCapacity = capacity
        IsSpawning = false
    }

let bareRespawn =
    {
        Time = 42
        Spawns = [ spawn 300 300 ]
        Refillables = [ { Id = "spawn-1"; FreeCapacity = 0 } ]
        Sources = [ { Id = "src-a" }; { Id = "src-b" } ]
        Controller = Some { Id = "ctrl-1"; Level = 1 }
        ConstructionSites = []
        Creeps = []
        Placement = None
        Spatial = None
    }

let worker name energy freeCapacity =
    {
        Name = name
        Energy = energy
        FreeCapacity = freeCapacity
    }

let spawnIntents intents =
    intents
    |> List.choose (function
        | SpawnCreep(s, b, c) -> Some(s, b, c)
        | _ -> None)

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
                                { Id = "spawn-1"; FreeCapacity = 50 }
                                { Id = "ext-1"; FreeCapacity = 0 }
                                { Id = "ext-2"; FreeCapacity = 50 }
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
        ]

/// Synthetic open terrain: every tile within `radius` of the spawn is
/// walkable; only the spawn tile itself is occupied.
let openTerrain radius =
    let spawnPos = { X = 25; Y = 25 }

    let walkable =
        Set.ofList
            [
                for x in 25 - radius .. 25 + radius do
                    for y in 25 - radius .. 25 + radius do
                        { X = x; Y = y }
            ]

    {
        RoomName = "W1N1"
        SpawnPos = spawnPos
        Walkable = walkable
        Occupied = Set.singleton spawnPos
        BuiltExtensions = 0
        PendingExtensions = 0
    }

let placementIntents intents =
    intents
    |> List.choose (function
        | PlaceConstructionSite(room, pos, kind) -> Some(room, pos, kind)
        | _ -> None)

let placedTiles intents =
    placementIntents intents |> List.map (fun (_, pos, _) -> pos)

let atLevel level placement =
    { bareRespawn with
        Controller = Some { Id = "ctrl-1"; Level = level }
        Placement = Some placement
    }

[<Tests>]
let placementTests =
    testList
        "placement"
        [
            test "RCL2 on open terrain places 5 extensions checkerboard, nearest first" {
                let intents, _ = decide (atLevel 2 (openTerrain 3)) Map.empty

                Expect.equal
                    (placedTiles intents)
                    [
                        { X = 24; Y = 24 }
                        { X = 24; Y = 26 }
                        { X = 26; Y = 24 }
                        { X = 26; Y = 26 }
                        { X = 23; Y = 23 }
                    ]
                    "diagonal neighbours first, then the nearest rank-2 checkerboard tile"

                for (room, _, kind) in placementIntents intents do
                    Expect.equal room "W1N1" "sites go in the spawn's room"
                    Expect.equal kind Extension "only extensions are placed"
            }

            test "below RCL2 no placement Intents are emitted" {
                let intents, _ = decide (atLevel 1 (openTerrain 3)) Map.empty
                Expect.isEmpty (placementIntents intents) "no extensions allowed at RCL1"
            }

            test "unwalkable tiles are skipped" {
                let terrain = openTerrain 3

                let holed =
                    { terrain with
                        Walkable = Set.remove { X = 24; Y = 24 } terrain.Walkable
                    }

                let intents, _ = decide (atLevel 2 holed) Map.empty

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "wall tile is never chosen"

                Expect.hasLength (placementIntents intents) 5 "the cap is still reached elsewhere"
            }

            test "occupied tiles are skipped" {
                let terrain = openTerrain 3

                let blocked =
                    { terrain with
                        Occupied = Set.add { X = 24; Y = 24 } terrain.Occupied
                    }

                let intents, _ = decide (atLevel 2 blocked) Map.empty

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "occupied tile is never chosen"

                Expect.hasLength (placementIntents intents) 5 "the cap is still reached elsewhere"
            }

            test "built extensions and pending sites count against the cap" {
                let terrain =
                    { openTerrain 3 with
                        BuiltExtensions = 2
                        PendingExtensions = 2
                    }

                let intents, _ = decide (atLevel 2 terrain) Map.empty
                Expect.hasLength (placementIntents intents) 1 "only the shortfall is placed"
            }

            test "no placement Intents once the allowance is exhausted" {
                let terrain =
                    { openTerrain 3 with
                        BuiltExtensions = 5
                    }

                let intents, _ = decide (atLevel 2 terrain) Map.empty
                Expect.isEmpty (placementIntents intents) "allowance already used up"
            }

            test "no placement Intents without placement info" {
                let snapshot =
                    { bareRespawn with
                        Controller = Some { Id = "ctrl-1"; Level = 2 }
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (placementIntents intents) "nothing to plan around"
            }
        ]

/// Spatial projection holding exactly the given terrain tiles and target
/// positions; absent tiles are outside the projection (impassable). No
/// creep positions and no obstacles — movement tests add those on top.
let spatial targets tiles =
    {
        Terrain = Map.ofList tiles
        TargetPositions = Map.ofList targets
        CreepPositions = Map.empty
        Obstacles = Set.empty
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
    |> List.filter (fun (_, tid) -> tid = $"harvest:{sourceId}")
    |> List.map fst

[<Tests>]
let seatTests =
    testList
        "seat capacity"
        [
            test "a single-Seat source gets exactly one of three empty creeps" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =
                            Some(
                                spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]
                            )
                    }

                let intents, assignments = decide snapshot Map.empty

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
                            Some(
                                spatial
                                    [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 20; Y = 20 } ]
                                    ([ { X = 9; Y = 10 }, Plain ] @ openSeats { X = 20; Y = 20 })
                            )
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.hasLength
                    (harvesters assignments "src-b")
                    2
                    "overflow lands on the source with free Seats"
            }

            test "a creep denied a Seat falls through to a lower-rank task" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 25 25 ]
                        Spatial =
                            Some(
                                spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]
                            )
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.contains
                    (assignments |> Map.toList |> List.map snd)
                    "upgrade:ctrl-1"
                    "the denied creep sinks its energy into the controller instead"
            }

            test "Seats are counted from terrain: swamp is a Seat, wall and absent are not" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =
                            Some(
                                spatial
                                    [ "src-a", { X = 10; Y = 10 } ]
                                    [
                                        { X = 9; Y = 10 }, Plain
                                        { X = 11; Y = 10 }, Swamp
                                        { X = 10; Y = 9 }, Wall
                                    ]
                            )
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.hasLength
                    (harvesters assignments "src-a")
                    2
                    "plain and swamp neighbours are Seats; wall and off-map are not"
            }

            test "oversold remembered assignments are trimmed back to the Seat count" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =
                            Some(
                                spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]
                            )
                    }

                let stale = Map.ofList [ "w1", "harvest:src-a"; "w2", "harvest:src-a" ]
                let _, assignments = decide snapshot stale

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the cap holds even against remembered oversell"
            }

            test "without a spatial projection Harvest stays uncapped" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.hasLength
                    (harvesters assignments "src-a")
                    3
                    "no terrain data means no cap — today's room behaviour"
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            Some
                                { spatial [ "src-a", { X = 10; Y = 10 } ] corridor with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "one single-step move Intent up the corridor"

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
            }

            test "a creep inside its Work Area acts and does not move" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 10; Y = 10 } ]
                                      (openSeats { X = 10; Y = 10 }) with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.contains intents (HarvestSource("w1", "src-a")) "seated creep harvests"

                Expect.isEmpty (moveIntents intents) "nowhere to go: no move Intent"
            }

            test "the approach detours around swamp when a plain lane is cheaper" {
                // Straight lane x = 10 is swamp (cost 5 each); the lane at
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            Some
                                { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.equal
                    (moveIntents intents)
                    [ "w1", TopRight ]
                    "the first step leaves the swamp lane for the plain one"
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
                            Some
                                { spatial
                                      [ "ctrl-1", { X = 10; Y = 10 } ]
                                      [
                                          { X = 10; Y = 10 }, Plain
                                          { X = 10; Y = 11 }, Plain
                                          { X = 10; Y = 12 }, Plain
                                      ] with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                    Obstacles =
                                        Set.ofList [ { X = 10; Y = 10 }; { X = 10; Y = 12 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 10; Y = 10 } ]
                                      [ { X = 10; Y = 11 }, Plain; { X = 10; Y = 14 }, Plain ] with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.isEmpty (moveIntents intents) "no path: standing still beats oscillating"
                Expect.isEmpty (actionIntents intents) "and the target is out of range"
            }

            test "a builder works from range 3 without closing in" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "site-1", { X = 10; Y = 10 } ]
                                      [ for y in 10..13 -> { X = 10; Y = y }, Plain ] with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 13 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.contains intents (BuildSite("w1", "site-1")) "range 3 is close enough"
                Expect.isEmpty (moveIntents intents) "no reason to walk closer"
            }

            test "a refiller two tiles out still has to walk to the structure" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "spawn-1", { X = 10; Y = 10 } ]
                                      [ for y in 10..12 -> { X = 10; Y = y }, Plain ] with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                    Obstacles = Set.singleton { X = 10; Y = 10 }
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "transfer needs range 1, so the creep closes in"

                Expect.isEmpty (actionIntents intents) "no transfer from range 2"
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

                let intents, assignments = decide snapshot Map.empty
                Expect.contains intents (HarvestSource("w1", "src-a")) "empty creep goes harvesting"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-a")
                    "assignment is remembered"
            }

            test "bare respawn yields exactly one spawn Intent" {
                let intents, _ = decide bareRespawn Map.empty

                match spawnIntents intents with
                | [ (spawnName, body, creepName) ] ->
                    Expect.equal spawnName "Spawn1" "spawns from the only spawn"
                    Expect.isNonEmpty body "body must not be empty"
                    Expect.isNotEmpty creepName "creep needs a name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawn Intent body is affordable at bare-respawn energy" {
                let intents, _ = decide bareRespawn Map.empty

                for (_, body, _) in spawnIntents intents do
                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        300
                        "body cost within bare-respawn energy"
            }

            test "no spawn Intent when energy is below a worker body cost" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ spawn 100 300 ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
            }

            test "no spawn Intent while the spawn is already spawning" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ { spawn 300 300 with IsSpawning = true } ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "spawn is busy"
            }

            test "at 550 capacity a 2x-unit body is spawned" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ spawn 550 550 ]
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Move; Work; Carry; Move ]
                        "body doubles at double capacity"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at 300 capacity the 1x-unit body is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal body [ Work; Carry; Move ] "300 capacity affords one unit"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "below minimum workforce, spawning waits for full capacity" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ spawn 400 550 ]
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.isEmpty
                    (spawnIntents intents)
                    "a living workforce can bank up to a bigger body"
            }

            test "with zero creeps a minimal body is spawned from available energy" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ spawn 250 550 ]
                    }

                let intents, _ = decide snapshot Map.empty

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
                        Spawns = [ spawn 150 550 ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "even the fallback needs its unit cost"
            }

            test "one worker is below minimum: a second is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.hasLength (spawnIntents intents) 1 "a lone worker cannot keep the loop going"
            }

            test "no spawn Intent when workforce is at minimum" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50; worker "worker-2" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
            }

            test "empty creeps spread across sources instead of piling on one" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let _, assignments = decide snapshot Map.empty
                let assigned = assignments |> Map.toList |> List.map snd |> List.sort

                Expect.equal
                    assigned
                    [ "harvest:src-a"; "harvest:src-b" ]
                    "greedy matching balances load per task"
            }

            test "greedy matching counts kept assignments as load" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30; worker "w2" 0 50 ]
                    }

                let _, assignments = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-a")
                    "w1 keeps its source"

                Expect.equal
                    (Map.tryFind "w2" assignments)
                    (Some "harvest:src-b")
                    "w2 avoids the occupied source"
            }

            test "assignments pass through unchanged when no creeps died" {
                let assignments = Map.ofList [ "worker-1", "harvest:src-a" ]

                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let _, kept = decide snapshot assignments
                Expect.equal kept assignments "assignments survive the tick"
            }

            test "an assignment sticks across ticks even when greedy would rebalance" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30 ]
                    }

                let assignments = Map.ofList [ "w1", "harvest:src-b" ]
                let intents, kept = decide snapshot assignments

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "harvest:src-b")
                    "no thrash: creep stays on its source"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-b"))
                    "intent follows the sticky assignment"
            }

            test "a creep that fills up is reassigned from Harvest to Refill" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "refill:spawn-1")
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1"))
                    "delivery intent emitted"
            }

            test "a creep that empties is reassigned from Refill back to Harvest" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let _, kept = decide snapshot (Map.ofList [ "w1", "refill:spawn-1" ])

                match Map.tryFind "w1" kept with
                | Some tid -> Expect.stringStarts tid "harvest:" "empty creep goes back to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "surplus: a full creep with a full spawn switches to upgrading" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "upgrade:ctrl-1")
                    "nothing to refill, so surplus goes to the controller"

                Expect.contains intents (UpgradeController("w1", "ctrl-1")) "upgrade intent emitted"
            }

            test "a hungry structure beats the controller for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let _, kept = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "refill:spawn-1")
                    "refill outranks upgrade while a structure is missing energy"
            }

            test "an upgrading creep that empties goes back to harvest" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let _, kept = decide snapshot (Map.ofList [ "w1", "upgrade:ctrl-1" ])

                match Map.tryFind "w1" kept with
                | Some tid -> Expect.stringStarts tid "harvest:" "spent creep returns to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test
                "a full creep with a full spawn and no controller is left unassigned with no intent" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])
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

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "build:site-1")
                    "surplus energy goes into construction"

                Expect.contains intents (BuildSite("w1", "site-1")) "build intent emitted"
            }

            test "an empty creep is never matched to a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let _, kept = decide snapshot (Map.ofList [ "w1", "build:site-1" ])

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.stringStarts tid "harvest:" "empty creep goes harvesting instead"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "a hungry structure beats a construction site for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let _, kept = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "refill:spawn-1")
                    "refill outranks build while a structure is missing energy"
            }

            test "assignments of dead creeps are dropped" {
                let assignments = Map.ofList [ "ghost", "task-a" ]
                let _, kept = decide bareRespawn assignments
                Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
            }
        ]
