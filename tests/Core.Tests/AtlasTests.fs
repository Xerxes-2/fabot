module Fabot.Core.Tests.AtlasTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas

/// Minimal Snapshot around a spatial projection: the Atlas reads only the
/// projection and the creep list's order.
let snapshotWith creeps spatial =
    {
        Time = 1
        Spawns = []
        RoomEnergy = Map.empty
        Refillables = []
        Sources = []
        Controller = None
        ConstructionSites = []
        Creeps = creeps
        Hostiles = []
        Spatial = spatial
    }

let worker name =
    {
        Name = name
        Energy = 0
        FreeCapacity = 50
    }

/// Projection with the given target positions and terrain tiles; no creeps,
/// no obstacles — tests layer those on top.
let spatial targets tiles =
    { SpatialInfo.empty with
        Terrain = Map.ofList tiles
        TargetPositions = Map.ofList targets
    }

[<Tests>]
let workAreaTests =
    testList
        "atlas workArea"
        [
            test "a placed Harvest target's Work Area is its passable range-1 ring" {
                // Source at (10,10); three neighbours are projected: two
                // passable (one swamp), one wall. Everything else lies
                // outside the projection, hence impassable.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 9; Y = 10 }, Plain
                            { X = 11; Y = 10 }, Swamp
                            { X = 10; Y = 9 }, Wall
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (workArea atlas (Harvest "src-a"))
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 } ])
                    "plain and swamp tiles in range are standing tiles; wall and absent are not"
            }

            test "an unplaced target has an empty Work Area" {
                let atlas = spatial [] [ { X = 9; Y = 10 }, Plain ] |> snapshotWith [] |> ofSnapshot

                Expect.equal (workArea atlas (Harvest "ghost")) Set.empty "nowhere to stand"
            }

            test "Build and Upgrade Work Areas reach range 3" {
                let tiles =
                    [
                        for x in 7..13 do
                            for y in 7..13 -> { X = x; Y = y }, Plain
                    ]

                let atlas =
                    spatial [ "ctrl-1", { X = 10; Y = 10 } ] tiles |> snapshotWith [] |> ofSnapshot

                let area = workArea atlas (Upgrade "ctrl-1")
                Expect.hasLength area 49 "the full 7x7 square is passable"
                Expect.isTrue (Set.contains { X = 7; Y = 7 } area) "the corner at range 3 is in"
            }
        ]

[<Tests>]
let seatTests =
    testList
        "atlas seats"
        [
            test "a placed source's Seats count passable neighbours by terrain alone" {
                // Two walkable neighbours (one swamp), one wall, the rest
                // absent; an obstacle sits on a walkable neighbour but does
                // not consume the Seat (ADR 0001: terrain only).
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          [
                              { X = 9; Y = 10 }, Plain
                              { X = 11; Y = 10 }, Swamp
                              { X = 10; Y = 9 }, Wall
                          ] with
                        Obstacles = Set.singleton { X = 9; Y = 10 }
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seats atlas "src-a")
                    (Some 2)
                    "plain and swamp are Seats; wall and absent are not"
            }

            test "an unplaced source has no Seat count at all" {
                let atlas = spatial [] [ { X = 9; Y = 10 }, Plain ] |> snapshotWith [] |> ofSnapshot

                Expect.equal (seats atlas "ghost") None "no position, no derivable capacity"
            }
        ]

[<Tests>]
let standingTests =
    testList
        "atlas standing"
        [
            test "adjacentWalkable excludes walls, obstacles and absent tiles, in (X, Y) order" {
                let atlas =
                    { spatial
                          []
                          [
                              { X = 9; Y = 10 }, Plain
                              { X = 10; Y = 9 }, Wall
                              { X = 10; Y = 11 }, Swamp
                              { X = 11; Y = 10 }, Plain
                          ] with
                        Obstacles = Set.singleton { X = 11; Y = 10 }
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (adjacentWalkable atlas { X = 10; Y = 10 })
                    [ { X = 9; Y = 10 }; { X = 10; Y = 11 } ]
                    "unlike Seats, standing respects obstacles"
            }

            test "placedCreeps keeps Snapshot creep order and skips the unplaced" {
                let atlas =
                    { spatial [] [ { X = 5; Y = 5 }, Plain ] with
                        CreepPositions =
                            Map.ofList [ "zed", { X = 5; Y = 5 }; "amy", { X = 6; Y = 6 } ]
                    }
                    |> snapshotWith [ worker "zed"; worker "ghost"; worker "amy" ]
                    |> ofSnapshot

                Expect.equal
                    (placedCreeps atlas)
                    [ "zed", { X = 5; Y = 5 }; "amy", { X = 6; Y = 6 } ]
                    "Snapshot order, not Map order; the unplaced creep is absent"
            }
        ]

[<Tests>]
let placementQueryTests =
    testList
        "atlas placement queries"
        [
            test "roomName passes the projection's room through, absent when empty" {
                let named =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (roomName named) (Some "W1N1") "the projection names its room"

                let bare = SpatialInfo.empty |> snapshotWith [] |> ofSnapshot
                Expect.equal (roomName bare) None "an empty projection covers no room"
            }

            test "positionOf finds a projected target and misses an absent one" {
                let atlas =
                    spatial [ "spawn-1", { X = 25; Y = 25 } ] [] |> snapshotWith [] |> ofSnapshot

                Expect.equal
                    (positionOf atlas "spawn-1")
                    (Some { X = 25; Y = 25 })
                    "a projected target has a tile"

                Expect.equal (positionOf atlas "ghost") None "an unprojected target has none"
            }

            test "buildableTiles excludes walls and every target's tile, in (X, Y) order" {
                // Plain and swamp qualify; the wall, the structure's tile and
                // the site's tile do not; a creep does not block placement.
                let atlas =
                    { spatial
                          [ "ext-1", { X = 10; Y = 11 }; "site-1", { X = 11; Y = 10 } ]
                          [
                              { X = 10; Y = 10 }, Plain
                              { X = 10; Y = 11 }, Plain
                              { X = 11; Y = 10 }, Plain
                              { X = 11; Y = 11 }, Swamp
                              { X = 12; Y = 10 }, Wall
                          ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "ext-1", Structure BuiltKind.Extension
                                    "site-1", Site BuiltKind.Extension
                                ]
                        CreepPositions = Map.ofList [ "w", { X = 10; Y = 10 } ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (buildableTiles atlas)
                    [ { X = 10; Y = 10 }; { X = 11; Y = 11 } ]
                    "free plain and swamp tiles only, sorted by (X, Y)"
            }

            test "extension censuses count exactly the built and pending extensions" {
                let atlas =
                    { SpatialInfo.empty with
                        TargetKinds =
                            Map.ofList
                                [
                                    "spawn-1", Structure BuiltKind.Spawn
                                    "ext-1", Structure BuiltKind.Extension
                                    "ext-2", Structure BuiltKind.Extension
                                    "road-1", Structure BuiltKind.Other
                                    "site-1", Site BuiltKind.Extension
                                    "site-2", Site BuiltKind.Other
                                    "src-a", Source
                                    "ctrl-1", Controller
                                ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (builtExtensions atlas) 2 "only standing extensions are built"

                Expect.equal
                    (pendingExtensions atlas)
                    1
                    "only sites that will become extensions are pending"
            }
        ]

/// Source at (10,12) on a wall; its Work Area is a swamp Seat at (10,13)
/// and a plain Seat at (11,13). A plain lane runs down to (10,15).
let corridor creeps =
    { spatial
          [ "src-a", { X = 10; Y = 12 } ]
          [
              { X = 10; Y = 12 }, Wall
              { X = 10; Y = 13 }, Swamp
              { X = 11; Y = 13 }, Plain
              { X = 10; Y = 14 }, Plain
              { X = 11; Y = 14 }, Plain
              { X = 10; Y = 15 }, Plain
          ] with
        CreepPositions = Map.ofList creeps
    }

[<Tests>]
let travelCostTests =
    testList
        "atlas travelCost"
        [
            test "the cost is the cheapest path to any Work Area tile, swamp priced in" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 2)
                    "the plain Seat at cost 2 beats stepping into the swamp Seat at 6"
            }

            test "a creep already inside the Work Area costs 0" {
                let atlas =
                    corridor [ "w", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal (travelCost atlas "w" (Harvest "src-a")) (Some 0) "already there"
            }

            test "an unreachable Work Area is None: the Task is inapplicable" {
                // The creep sits on a walkable island the corridor cannot reach.
                let projection = corridor [ "w", { X = 20; Y = 20 } ]

                let atlas =
                    { projection with
                        Terrain = Map.add { X = 20; Y = 20 } Plain projection.Terrain
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    None
                    "no path means never matched, never marched"
            }

            test "an unplaced creep prices everything at 0" {
                let atlas = corridor [] |> snapshotWith [ worker "w" ] |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 0)
                    "geometry that cannot be priced never counts against a Task"
            }

            test "an unplaced target prices at 0, not None" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "ghost"))
                    (Some 0)
                    "an unplaced target is unpriceable, not unreachable"
            }
        ]

[<Tests>]
let firstStepTests =
    testList
        "atlas firstStep"
        [
            test "the first step follows the cheapest path, detouring around swamp" {
                // Straight lane x = 10 is swamp; the lane at x = 11 is plain
                // and reaches a Seat in as many steps.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          [
                              { X = 10; Y = 10 }, Wall
                              { X = 10; Y = 11 }, Plain
                              { X = 10; Y = 12 }, Swamp
                              { X = 10; Y = 13 }, Swamp
                              { X = 10; Y = 14 }, Plain
                              { X = 11; Y = 11 }, Plain
                              { X = 11; Y = 12 }, Plain
                              { X = 11; Y = 13 }, Plain
                          ] with
                        CreepPositions = Map.ofList [ "w", { X = 10; Y = 14 } ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the swamp lane for the plain one"
            }

            test "a creep already inside the Work Area has no step to take" {
                let atlas =
                    corridor [ "w", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal (firstStep atlas "w" (Harvest "src-a")) None "already there"
            }

            test "an unreachable Work Area yields no step: waiting beats marching at a wall" {
                let projection = corridor [ "w", { X = 20; Y = 20 } ]

                let atlas =
                    { projection with
                        Terrain = Map.add { X = 20; Y = 20 } Plain projection.Terrain
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal (firstStep atlas "w" (Harvest "src-a")) None "no path, no step"
            }

            test "an unplaced creep has no step: no movement without geometry" {
                let atlas = corridor [] |> snapshotWith [ worker "w" ] |> ofSnapshot

                Expect.equal (firstStep atlas "w" (Harvest "src-a")) None "nothing derivable"
            }
        ]

[<Tests>]
let mayActTests =
    testList
        "atlas mayAct"
        [
            test "acting is judged by the action's range from the tick-start position" {
                let atlasAt creepPos =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 20; Y = 20 } ]
                          [ for y in 11..15 -> { X = 10; Y = y }, Plain ] with
                        CreepPositions = Map.ofList [ "w", creepPos ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue
                    (mayAct (atlasAt { X = 10; Y = 11 }) "w" (Harvest "src-a"))
                    "harvest reaches at range 1"

                Expect.isFalse
                    (mayAct (atlasAt { X = 10; Y = 12 }) "w" (Harvest "src-a"))
                    "harvest does not reach at range 2"

                Expect.isTrue
                    (mayAct (atlasAt { X = 18; Y = 17 }) "w" (Upgrade "ctrl-1"))
                    "upgrade reaches at range 3"
            }

            test "a creep or target the projection cannot place never blocks the action" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue (mayAct atlas "ghost" (Harvest "src-a")) "unplaced creep acts"
                Expect.isTrue (mayAct atlas "w" (Harvest "ghost")) "unplaced target is acted on"
            }
        ]

[<Tests>]
let dualSeatTests =
    testList
        "atlas dualSeats"
        [
            test "a Dual Seat is a Seat inside the controller's Upgrade Work Area, over all sources" {
                // Sources at (10,10) and (16,10) flank the controller at
                // (13,10). Each source has a Seat at range 2 of the
                // controller (inside the Upgrade Work Area) and one at
                // range 4 (outside); src-a's swamp Seat at (11,11) is in.
                let atlas =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 13; Y = 10 }
                              "src-b", { X = 16; Y = 10 }
                          ]
                          [
                              { X = 9; Y = 10 }, Plain
                              { X = 11; Y = 10 }, Plain
                              { X = 11; Y = 11 }, Swamp
                              { X = 15; Y = 10 }, Plain
                              { X = 17; Y = 10 }, Plain
                          ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "ctrl-1", Controller; "src-b", Source ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (dualSeats atlas)
                    (Set.ofList [ { X = 11; Y = 10 }; { X = 11; Y = 11 }; { X = 15; Y = 10 } ])
                    "exactly the Seats within upgrade range; the range-4 Seats are not"
            }

            test "an obstacle keeps a Seat out of the Dual Seats: a creep must stand there" {
                // The lone Seat within upgrade range carries an obstacle
                // structure — it stays a Seat (ADR 0001) but no creep can
                // stand on it, so the Upgrade Work Area excludes it.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 13; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
                        Obstacles = Set.singleton { X = 11; Y = 10 }
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (dualSeats atlas) Set.empty "an unstandable Seat is no Dual Seat"
            }

            test "a room without a controller has no Dual Seats" {
                let atlas =
                    { spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 11; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "src-a", Source ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (dualSeats atlas) Set.empty "no Upgrade Work Area to intersect"
            }

            test "a room without sources has no Dual Seats" {
                let atlas =
                    { spatial [ "ctrl-1", { X = 13; Y = 10 } ] [ { X = 12; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "ctrl-1", Controller ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (dualSeats atlas) Set.empty "no Seats to intersect"
            }

            test "a source out of upgrade range yields an empty, harmless answer" {
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 40; Y = 40 } ]
                          [ { X = 11; Y = 10 }, Plain; { X = 39; Y = 40 }, Plain ] with
                        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (dualSeats atlas) Set.empty "a disjoint intersection is just empty"
            }
        ]

[<Tests>]
let consistencyTests =
    testList
        "atlas consistency"
        [
            test "travelCost, firstStep, workArea and mayAct agree from every standing tile" {
                // Mixed ground around a source: seats on plain and swamp, a
                // dead lane, an obstacle, and an unreachable island — the
                // sweep stands one creep on every standing tile in turn.
                let projection =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          [
                              { X = 10; Y = 10 }, Wall
                              { X = 9; Y = 10 }, Plain
                              { X = 10; Y = 11 }, Swamp
                              { X = 11; Y = 11 }, Plain
                              { X = 9; Y = 11 }, Plain
                              { X = 9; Y = 12 }, Plain
                              { X = 10; Y = 12 }, Plain
                              { X = 11; Y = 12 }, Plain
                              { X = 8; Y = 10 }, Plain
                              { X = 8; Y = 9 }, Swamp
                              { X = 20; Y = 20 }, Plain
                              { X = 21; Y = 20 }, Plain
                          ] with
                        Obstacles = Set.singleton { X = 11; Y = 12 }
                    }

                let standing =
                    projection.Terrain
                    |> Map.toList
                    |> List.choose (fun (tile, kind) ->
                        if kind <> Wall && not (Set.contains tile projection.Obstacles) then
                            Some tile
                        else
                            None)

                let task = Harvest "src-a"

                for pos in standing do
                    let atlas =
                        { projection with
                            CreepPositions = Map.ofList [ "w", pos ]
                        }
                        |> snapshotWith [ worker "w" ]
                        |> ofSnapshot

                    let area = workArea atlas task
                    let cost = travelCost atlas "w" task
                    let step = firstStep atlas "w" task

                    if Set.contains pos area then
                        Expect.equal cost (Some 0) $"inside the Work Area costs 0 at {pos}"
                        Expect.equal step None $"no step inside the Work Area at {pos}"
                        Expect.isTrue (mayAct atlas "w" task) $"in-area implies in range at {pos}"
                    else
                        match cost, step with
                        | Some c, Some s ->
                            Expect.isGreaterThan c 0 $"reachable from outside costs > 0 at {pos}"

                            Expect.contains
                                (adjacentWalkable atlas pos)
                                s
                                $"the step is an adjacent standing tile at {pos}"
                        | None, None -> () // unreachable: inapplicable, stationary
                        | c, s -> failtest $"cost {c} and step {s} disagree at {pos}"
            }

            test "a Harvest Work Area is the source's Seats minus obstacle-blocked tiles" {
                // Same ground as the seats test: two terrain Seats, one
                // under an obstacle — standing loses it, the Seat count
                // keeps it (ADR 0001), so standing never exceeds Seats.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          [
                              { X = 9; Y = 10 }, Plain
                              { X = 11; Y = 10 }, Swamp
                              { X = 10; Y = 9 }, Wall
                          ] with
                        Obstacles = Set.singleton { X = 9; Y = 10 }
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                let area = workArea atlas (Harvest "src-a")

                Expect.equal
                    area
                    (Set.singleton { X = 11; Y = 10 })
                    "the obstacle removes a standing tile but not the Seat"

                Expect.isLessThanOrEqual
                    (Set.count area)
                    (seats atlas "src-a" |> Option.defaultValue 0)
                    "standing tiles never exceed Seats"
            }
        ]
