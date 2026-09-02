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
                        Spatial = Some(nearFarCorridor [ "w1", { X = 10; Y = 17 } ])
                    }

                let far: SourceInfo = { Id = "src-far" }
                let near: SourceInfo = { Id = "src-near" }

                for sources in [ [ far; near ]; [ near; far ] ] do
                    let _, assignments = decide (snapshotWith sources) Map.empty

                    Expect.equal
                        (Map.tryFind "w1" assignments)
                        (Some "harvest:src-near")
                        "the cheaper-to-reach source wins the rank tie"
            }

            test "swamp prices the route: a range-nearer target loses to a longer plain path" {
                // One corridor, a source at each end. src-swamp is 3 tiles
                // away by range but behind two swamp tiles (cost 10);
                // src-plain is 5 tiles away over plain ground (cost 4).
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
                        Sources = [ { Id = "src-swamp" }; { Id = "src-plain" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            Some
                                { spatial
                                      [
                                          "src-swamp", { X = 10; Y = 12 }
                                          "src-plain", { X = 10; Y = 20 }
                                      ]
                                      corridor with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                }
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-plain")
                    "true path cost decides, not Chebyshev range"
            }

            test "rank dominates: an adjacent Build never outbids a four-tiles-away Refill" {
                // The hungry spawn sits at the top of the corridor, four
                // steps from the creep; the construction site is close
                // enough to build without moving at all.
                let corridor = [ for y in 10..16 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            Some
                                { spatial
                                      [
                                          "spawn-1", { X = 10; Y = 10 }
                                          "site-1", { X = 10; Y = 16 }
                                      ]
                                      corridor with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                    Obstacles = Set.singleton { X = 10; Y = 10 }
                                }
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "refill:spawn-1")
                    "travel cost breaks ties within a rank, never across ranks"
            }

            test "a sticky assignment is kept even when a cheaper task exists this tick" {
                // Same corridor as the live-bug regression, but the creep
                // already holds the far source from an earlier tick.
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-far" }; { Id = "src-near" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = Some(nearFarCorridor [ "w1", { X = 10; Y = 17 } ])
                    }

                let sticky = Map.ofList [ "w1", "harvest:src-far" ]
                let _, assignments = decide snapshot sticky

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-far")
                    "sticky assignments are never re-evaluated for a closer target"
            }

            test "an unplaced creep is matched as today: Snapshot order decides the tie" {
                // The projection places both sources but not the creep, so
                // no flood can run — the pick falls back to (rank, load).
                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-far" }; { Id = "src-near" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = Some(nearFarCorridor [])
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-far")
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                      terrain with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                }
                    }

                let _, assignments = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "upgrade:ctrl-1")
                    "the unreachable Harvest is not applicable to this creep at all"
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 0 50 ]
                        Spatial =
                            Some
                                { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 20; Y = 20 }; "w2", { X = 10; Y = 12 } ]
                                }
                    }

                let sticky = Map.ofList [ "w1", "harvest:src-a" ]
                let _, assignments = decide snapshot sticky

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w2" ]
                    "the freed Seat goes to the creep that can reach it"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "upgrade:ctrl-1")
                    "the walled-off creep falls through to the next applicable task"
            }

            test "a creep with no reachable applicable task is left unassigned and emits nothing" {
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            Some
                                { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                                }
                    }

                let sticky = Map.ofList [ "w1", "harvest:src-a" ]
                let intents, assignments = decide snapshot sticky

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
                            Some
                                { spatial
                                      [ "ctrl-1", { X = 10; Y = 10 } ]
                                      [ { X = 20; Y = 20 }, Plain ] with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                                }
                    }

                let sticky = Map.ofList [ "w1", "upgrade:ctrl-1" ]
                let _, assignments = decide snapshot sticky

                Expect.equal (Map.tryFind "w1" assignments) None "no Work Area means no assignment"
            }

            test
                "an unplaced creep keeps its assignment: no reachability filtering without geometry" {
                // Same walled-off source, but the projection does not place
                // the creep — nothing can be proven, so nothing is released.
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = Some(spatial [ "src-a", { X = 10; Y = 10 } ] terrain)
                    }

                let sticky = Map.ofList [ "w1", "harvest:src-a" ]
                let _, assignments = decide snapshot sticky

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-a")
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                      terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "upg", { X = 10; Y = 11 } ]
                                }
                    }

                let sticky = Map.ofList [ "har", "harvest:src-a"; "upg", "upgrade:ctrl-1" ]
                let intents, _ = decide snapshot sticky

                Expect.contains
                    (moveIntents intents)
                    ("har", Top)
                    "the harvester steps onto the Seat"

                Expect.contains
                    intents
                    (UpgradeController("upg", "ctrl-1"))
                    "the displaced upgrader still upgrades this tick"

                match moveIntents intents |> List.filter (fun (name, _) -> name = "upg") with
                | [ (_, direction) ] ->
                    let dest = stepFrom { X = 10; Y = 11 } direction

                    Expect.isLessThanOrEqual
                        (max (abs (dest.X - 10)) (abs (dest.Y - 14)))
                        3
                        "the upgrader is displaced to a tile still inside its Work Area"
                | other -> failtest $"expected exactly one move for the upgrader, got %A{other}"
            }

            test "head-on swap: two creeps blocking each other exchange tiles" {
                // Two single-Seat sources at the ends of a two-tile corridor;
                // each creep stands on the other's Seat.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" }; { Id = "src-b" } ]
                        Creeps = [ worker "wa" 0 50; worker "wb" 0 50 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 10; Y = 13 } ]
                                      terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "wa", { X = 10; Y = 12 }; "wb", { X = 10; Y = 11 } ]
                                }
                    }

                let sticky = Map.ofList [ "wa", "harvest:src-a"; "wb", "harvest:src-b" ]
                let intents, _ = decide snapshot sticky

                Expect.equal
                    (moveIntents intents |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "both creeps move: they swap instead of deadlocking"
            }

            test "an idle creep is displaced by a working creep passing through" {
                // No controller and nothing to refill: the full creep w2 has
                // no applicable task and idles astride the harvester's path.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50; worker "w2" 50 0 ]
                        Spatial =
                            Some
                                { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 13 }; "w2", { X = 10; Y = 12 } ]
                                }
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.contains
                    (moveIntents intents)
                    ("w1", Top)
                    "the working creep claims the idler's tile"

                Expect.isTrue
                    (moveIntents intents |> List.exists (fun (name, _) -> name = "w2"))
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                      terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                                }
                    }

                let sticky = Map.ofList [ "h", "harvest:src-a"; "u", "upgrade:ctrl-1" ]
                let intents, _ = decide snapshot sticky

                Expect.equal
                    (moveIntents intents)
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "h1" 0 50; worker "h2" 0 50 ]
                        Spatial =
                            Some
                                { spatial [ "src-a", { X = 10; Y = 10 } ] terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h1", { X = 10; Y = 11 }; "h2", { X = 9; Y = 12 } ]
                                }
                    }

                let sticky = Map.ofList [ "h1", "harvest:src-a"; "h2", "harvest:src-a" ]
                let intents, _ = decide snapshot sticky

                Expect.equal
                    (moveIntents intents |> List.sort)
                    [ "h1", Right; "h2", TopRight ]
                    "h2 claims the occupied Seat; h1 is displaced to the free one"

                Expect.contains
                    intents
                    (HarvestSource("h1", "src-a"))
                    "the displaced harvester still harvests this tick"
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
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =
                            Some
                                { spatial
                                      [ "src-a", { X = 11; Y = 12 }; "ctrl-1", { X = 13; Y = 12 } ]
                                      terrain with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 9; Y = 12 }; "upg", { X = 10; Y = 12 } ]
                                }
                    }

                let sticky = Map.ofList [ "har", "harvest:src-a"; "upg", "upgrade:ctrl-1" ]
                let intents, _ = decide snapshot sticky

                Expect.equal
                    (moveIntents intents |> List.sort)
                    [ "har", Right; "upg", Left ]
                    "displacer and occupant exchange tiles"

                Expect.contains
                    intents
                    (UpgradeController("upg", "ctrl-1"))
                    "the swapped-out upgrader still upgrades from its tick-start tile"
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
                        Spatial = Some fiveSeats
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "five Seats support five creeps; two living is a deficit"
            }

            test "no spawn Intent once the workforce matches the Seat total" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ for i in 1..5 -> worker $"w{i}" 0 50 ]
                        Spatial = Some fiveSeats
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "workforce already at target"
            }

            test "a Seat total below the floor leaves the floor in charge" {
                let oneSeat = spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ { Id = "src-a" } ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = Some oneSeat
                    }

                let intents, _ = decide snapshot Map.empty

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "one Seat cannot lower the target below the floor of two"
            }

            test "an unplaced source contributes no Seats" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = Some(spatial [] [])
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "only the floor applies"
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

            test "at 550 capacity the whole capacity is spent" {
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
                        [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                        "two units plus the 150 remainder as Carry/Carry/Move"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at 300 capacity the remainder pads the single unit" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty

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
