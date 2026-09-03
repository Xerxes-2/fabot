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
        Fatigue = 0
        Energy = 0
        FreeCapacity = 50
        Body = Map.ofList [ Work, 1; Carry, 1; Move, 1 ]
    }

/// A creep with the given carried energy and body's part counts.
let creepWith name energy body =
    {
        Name = name
        Fatigue = 0
        Energy = energy
        FreeCapacity = 50
        Body = body |> List.countBy id |> Map.ofList
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

            test "droppedEnergy lists placed piles in id order; buildableTiles ignores them" {
                // A pile is a target the reflex reads, not a thing standing
                // on the tile: it never keeps a construction site off it.
                let atlas =
                    { spatial
                          [ "pile-b", { X = 10; Y = 11 }; "pile-a", { X = 10; Y = 10 } ]
                          [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds = Map.ofList [ "pile-a", Dropped; "pile-b", Dropped ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (droppedEnergy atlas)
                    [ "pile-a", { X = 10; Y = 10 }; "pile-b", { X = 10; Y = 11 } ]
                    "both piles placed, id order"

                Expect.equal
                    (buildableTiles atlas)
                    [ { X = 10; Y = 10 }; { X = 10; Y = 11 } ]
                    "pile tiles stay buildable"
            }

            test
                "a placed container is a target, not an obstacle: repairable in place, unbuildable under" {
                // Container at (10,10) on a fully projected 7x7 plain square,
                // carrying hits and store as the projection now does.
                let tiles =
                    [
                        for x in 7..13 do
                            for y in 7..13 -> { X = x; Y = y }, Plain
                    ]

                let atlas =
                    { spatial [ "cont-1", { X = 10; Y = 10 } ] tiles with
                        TargetKinds = Map.ofList [ "cont-1", Structure BuiltKind.Container ]
                        Hits = Map.ofList [ "cont-1", { Hits = 100; HitsMax = 250000 } ]
                        Stores = Map.ofList [ "cont-1", 800 ]
                        CreepPositions = Map.ofList [ "w", { X = 7; Y = 7 } ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                let area = workArea atlas (Repair "cont-1")

                Expect.hasLength
                    area
                    49
                    "the full range-3 square stands: a container blocks no tile, its own included"

                Expect.equal
                    (travelCost atlas "w" (Repair "cont-1"))
                    (Some 0)
                    "the corner creep already stands inside the Work Area"

                Expect.isFalse
                    (List.contains { X = 10; Y = 10 } (buildableTiles atlas))
                    "the container's tile takes no construction site"
            }

            test "an unplaced container gets the documented answers: empty area, free pricing" {
                // Hits arrive without a position — unpriceable geometry never
                // counts against a Task (ADR 0004).
                let atlas =
                    { spatial [] [ { X = 10; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "cont-1", Structure BuiltKind.Container ]
                        Hits = Map.ofList [ "cont-1", { Hits = 100; HitsMax = 250000 } ]
                        CreepPositions = Map.ofList [ "w", { X = 10; Y = 10 } ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal (workArea atlas (Repair "cont-1")) Set.empty "nowhere to stand"

                Expect.equal
                    (travelCost atlas "w" (Repair "cont-1"))
                    (Some 0)
                    "an unplaced target prices at 0, never against the Task"

                Expect.isTrue
                    (mayAct atlas "w" (Repair "cont-1"))
                    "an unplaced target never blocks the action"
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
                    (Some 4)
                    "the plain Seat at cost 4 beats stepping into the swamp Seat at 12"
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

            test "a standing creep prices its tile dearer: the free swamp Seat wins" {
                // Another creep parks on the plain Seat at (11,13). Its
                // occupancy surcharge makes that route cost 14, so the
                // untouched swamp Seat at 12 is now the cheapest way in —
                // dearer, but never inapplicable, unlike an obstacle.
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 }; "b", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 12)
                    "standing traffic re-prices the route without ever closing it"
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

/// Source at (10,10) whose only Seat is at (10,11) with the given terrain
/// and roads; the creep "w" stands one step below on plain — the cost is
/// exactly one step onto that Seat.
let seatPriced terrain roads =
    { spatial
          [ "src-a", { X = 10; Y = 10 } ]
          [
              { X = 10; Y = 10 }, Wall
              { X = 10; Y = 11 }, terrain
              { X = 10; Y = 12 }, Plain
          ] with
        CreepPositions = Map.ofList [ "w", { X = 10; Y = 12 } ]
        Roads = roads
    }

[<Tests>]
let roadPricingTests =
    testList
        "atlas road pricing"
        [
            test "a built road prices a step at 1: half a plain step, a tenth of a swamp step" {
                let costOn terrain roads =
                    let atlas =
                        seatPriced terrain roads |> snapshotWith [ worker "w" ] |> ofSnapshot

                    travelCost atlas "w" (Harvest "src-a")

                let road = Set.singleton { X = 10; Y = 11 }

                Expect.equal (costOn Plain Set.empty) (Some 2) "a plain step costs 2"
                Expect.equal (costOn Swamp Set.empty) (Some 10) "a swamp step costs 10"
                Expect.equal (costOn Plain road) (Some 1) "a road on plain costs 1: half"

                Expect.equal
                    (costOn Swamp road)
                    (Some 1)
                    "a road on swamp costs 1: the road overrides the terrain under it"
            }

            test "the occupancy surcharge is worth exactly one swamp step" {
                // Another creep parks on the plain Seat: the step onto it
                // costs its plain weight plus the surcharge — 2 + 10, the
                // 10 being the same price as stepping into swamp (ADR 0010).
                let atlas =
                    { seatPriced Plain Set.empty with
                        CreepPositions =
                            Map.ofList [ "w", { X = 10; Y = 12 }; "b", { X = 10; Y = 11 } ]
                    }
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 12)
                    "plain weight 2 plus the one-swamp-step surcharge 10"
            }

            test "a road construction site is not yet a road: only Roads tiles price at 1" {
                // A road site is projected as a target of Site kind, never
                // into Roads — the tile keeps pricing by its terrain.
                let atlas =
                    { seatPriced Plain Set.empty with
                        TargetPositions =
                            Map.ofList [ "src-a", { X = 10; Y = 10 }; "site-1", { X = 10; Y = 11 } ]
                        TargetKinds = Map.ofList [ "src-a", Source; "site-1", Site BuiltKind.Other ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 2)
                    "the unbuilt road's tile still prices as plain"
            }
        ]

[<Tests>]
let travelUnitTests =
    testList
        "atlas travel units"
        [
            test "the same path costs more units for a body with fewer Move parts per part" {
                // The corridor's cheapest path is two plain steps. The
                // worker unit (1 fatigue part per Move) walks it in 4 units;
                // a heavy body (5 fatigue parts per Move) needs
                // ceil(2 × 5 / 1) = 10 units a step, 20 in all. The empty
                // Carry rides free in both bodies (engine fatigue rules).
                let costFor creep =
                    let atlas =
                        corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ creep ] |> ofSnapshot

                    travelCost atlas "w" (Harvest "src-a")

                Expect.equal (costFor (worker "w")) (Some 4) "the worker unit's cost equals terrain"

                Expect.equal
                    (costFor (creepWith "w" 0 [ Work; Work; Work; Work; Work; Carry; Move ]))
                    (Some 20)
                    "five fatigue parts on one Move price each plain step at 10 units"
            }

            test "a Move surplus divides the weight, ceiled, never below one unit a step" {
                // One step onto the only Seat: on swamp (weight 10) the
                // worker pays 10 units, three Moves under one Work pay
                // ceil(10 × 1 / 3) = 4 — the ceil is visible — and on plain
                // (weight 2) the same surplus-Move body pays ceil(2 / 3) =
                // 1: the one-unit floor, never a fraction of a unit.
                let costOn terrain creep =
                    let atlas = seatPriced terrain Set.empty |> snapshotWith [ creep ] |> ofSnapshot
                    travelCost atlas "w" (Harvest "src-a")

                Expect.equal
                    (costOn Swamp (worker "w"))
                    (Some 10)
                    "the worker unit's cost equals terrain"

                let surplus = creepWith "w" 0 [ Work; Move; Move; Move ]

                Expect.equal
                    (costOn Swamp surplus)
                    (Some 4)
                    "ceil(10/3) = 4: surplus Moves cannot divide a step below whole units"

                Expect.equal
                    (costOn Plain surplus)
                    (Some 1)
                    "ceil(2/3) = 1: the floor is one unit, never zero"
            }

            test "carried energy loads Carry parts into the fatigue count" {
                // Deliberate choice, documented here: travel is priced from
                // the load the creep carries right now — the engine loads
                // Carry parts 50 energy apiece, and an empty Carry generates
                // no fatigue. The same worker walks the two-plain-step path
                // in 4 units empty and 8 units with its Carry full.
                let costFor energy =
                    let atlas =
                        corridor [ "w", { X = 10; Y = 15 } ]
                        |> snapshotWith [ creepWith "w" energy [ Work; Carry; Move ] ]
                        |> ofSnapshot

                    travelCost atlas "w" (Harvest "src-a")

                Expect.equal (costFor 0) (Some 4) "empty: only the Work part generates fatigue"
                Expect.equal (costFor 50) (Some 8) "loaded: the full Carry part joins in"
            }

            test "a body without Move parts reaches nothing beyond where it stands" {
                let atlasAt pos =
                    corridor [ "w", pos ]
                    |> snapshotWith [ creepWith "w" 0 [ Work; Carry ] ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost (atlasAt { X = 10; Y = 15 }) "w" (Harvest "src-a"))
                    None
                    "outside the Work Area every path is unwalkable: the Task is inapplicable"

                Expect.equal
                    (travelCost (atlasAt { X = 11; Y = 13 }) "w" (Harvest "src-a"))
                    (Some 0)
                    "already inside, the body works without a step"
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

            test "the first step detours around a standing creep when a lane is open" {
                // Same shape as the swamp detour, but on all-plain ground
                // with a creep parked mid-lane: the occupancy surcharge
                // sends the first step into the free lane at x = 11.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          [
                              { X = 10; Y = 10 }, Wall
                              { X = 10; Y = 11 }, Plain
                              { X = 10; Y = 12 }, Plain
                              { X = 10; Y = 13 }, Plain
                              { X = 10; Y = 14 }, Plain
                              { X = 11; Y = 11 }, Plain
                              { X = 11; Y = 12 }, Plain
                              { X = 11; Y = 13 }, Plain
                          ] with
                        CreepPositions =
                            Map.ofList [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                    }
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the parked creep's lane for the free one"
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

            test "repair reaches at range 3, like build and upgrade" {
                let atlasAt creepPos =
                    { spatial
                          [ "road-1", { X = 10; Y = 10 } ]
                          [ for y in 10..15 -> { X = 10; Y = y }, Plain ] with
                        CreepPositions = Map.ofList [ "w", creepPos ]
                    }
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue
                    (mayAct (atlasAt { X = 10; Y = 13 }) "w" (Repair "road-1"))
                    "repair reaches at range 3"

                Expect.isFalse
                    (mayAct (atlasAt { X = 10; Y = 14 }) "w" (Repair "road-1"))
                    "repair does not reach at range 4"
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
let postTests =
    testList
        "atlas posts"
        [
            test "posts are the Dual Seats plus Seats under built source containers" {
                // Source at (10,10), controller at (13,10): (11,10) is a
                // Dual Seat, (9,10) an ordinary Seat carrying a built
                // container — both are Posts.
                let atlas =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 13; Y = 10 }
                              "cont-1", { X = 9; Y = 10 }
                          ]
                          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "cont-1", Structure BuiltKind.Container
                                ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (posts atlas)
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 } ])
                    "the Dual Seat and the container Seat are both Posts"

                Expect.equal
                    (dualSeats atlas)
                    (Set.singleton { X = 11; Y = 10 })
                    "dualSeats is untouched by the container"
            }

            test "a container construction site is no Post" {
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Site BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (posts atlas) Set.empty "a pending container garrisons nothing"
            }

            test "a built container off any Seat adds no Post" {
                // The controller container's shape: built, but not on a
                // Seat — range 2 of the source.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 12; Y = 10 } ]
                          [ { X = 11; Y = 10 }, Plain; { X = 12; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (posts atlas) Set.empty "only a Seat under a container is a Post"
            }

            test "a room with no controller still derives container Posts" {
                // The W12S28 shape: no Dual Seat can exist, yet the Seat
                // under the built source container is a Post.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (posts atlas)
                    (Set.singleton { X = 9; Y = 10 })
                    "no Dual Seats, one container Post"
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

[<Tests>]
let trunkPathTests =
    testList
        "atlas trunkPath"
        [
            // A 5-wide corridor of plain ground along y = 10, three rows tall,
            // anchored at a wall tile the way a source is embedded in one.
            let corridor =
                spatial
                    []
                    [
                        yield { X = 10; Y = 10 }, Wall
                        for x in 11..14 do
                            for y in 9..11 do
                                yield { X = x; Y = y }, Plain
                    ]

            test "paves the straight line from beside the anchor to the goal" {
                // A single-row corridor: diagonal steps cost the same as
                // straight ones, so a wider room may legally drift the line.
                let atlas =
                    spatial
                        []
                        [
                            yield { X = 10; Y = 10 }, Wall
                            for x in 11..14 do
                                yield { X = x; Y = 10 }, Plain
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (trunkPath atlas Set.empty { X = 10; Y = 10 } (Set.singleton { X = 14; Y = 10 }))
                    [
                        { X = 11; Y = 10 }
                        { X = 12; Y = 10 }
                        { X = 13; Y = 10 }
                        { X = 14; Y = 10 }
                    ]
                    "the path starts beside the impassable anchor and ends on the goal"
            }

            test "routes around avoided tiles" {
                let atlas = corridor |> snapshotWith [] |> ofSnapshot

                let path =
                    trunkPath
                        atlas
                        (Set.singleton { X = 12; Y = 10 })
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 })

                Expect.isFalse
                    (List.contains { X = 12; Y = 10 } path)
                    "an avoided tile is never paved through"

                Expect.equal (List.last path) { X = 14; Y = 10 } "the goal is still reached"
                Expect.hasLength path 4 "the detour is a same-length diagonal"
            }

            test "prices raw terrain: a built road on a swamp does not attract the line" {
                // The straight line crosses a swamp that already carries a
                // road; normal pricing would make it the cheap lane, but
                // trunk pricing reads the ground under it (ADR 0011).
                let atlas =
                    { corridor with
                        Terrain = Map.add { X = 12; Y = 10 } Swamp corridor.Terrain
                        Roads = Set.singleton { X = 12; Y = 10 }
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                let path =
                    trunkPath atlas Set.empty { X = 10; Y = 10 } (Set.singleton { X = 14; Y = 10 })

                Expect.isFalse
                    (List.contains { X = 12; Y = 10 } path)
                    "the swamp is dodged though its road would be cheap to walk"

                Expect.equal (List.last path) { X = 14; Y = 10 } "the goal is still reached"
            }

            test "unreachable goals pave nothing" {
                let atlas = corridor |> snapshotWith [] |> ofSnapshot

                Expect.isEmpty
                    (trunkPath atlas Set.empty { X = 10; Y = 10 } (Set.singleton { X = 30; Y = 30 }))
                    "a goal outside the projection is unreachable"
            }

            test "of equally cheap goals the lowest (cost, tile) wins" {
                let atlas = corridor |> snapshotWith [] |> ofSnapshot

                let path =
                    trunkPath
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.ofList [ { X = 13; Y = 9 }; { X = 13; Y = 11 } ])

                Expect.equal (List.last path) { X = 13; Y = 9 } "ties break on the tile ordering"
            }
        ]

[<Tests>]
let haulRoundTripTests =
    testList
        "atlas haulRoundTripTicks"
        [
            // Corridor y = 10, x = 10..20: the container tile at (10,10),
            // the spawn structure standing at (20,10) — nine steps to the
            // one spawn-adjacent goal, (19,10).
            let corridorWith roads creeps =
                { spatial
                      [ "spawn-1", { X = 20; Y = 10 } ]
                      [ for x in 10..20 -> { X = x; Y = 10 }, Plain ] with
                    Obstacles = Set.singleton { X = 20; Y = 10 }
                    Roads = roads
                    CreepPositions = Map.ofList creeps
                }
                |> snapshotWith []
                |> ofSnapshot

            test "the loaded leg out and the empty leg back sum to whole ticks" {
                // [Carry;Carry;Move] loaded on plain: two full Carry x
                // weight 2 over one Move is 4 units a step; empty Carry
                // rides free, so the leg back costs the 1-unit floor. Nine
                // steps x 5 units = 45 half-ticks, rounded up to 23.
                Expect.equal
                    (haulRoundTripTicks
                        (corridorWith Set.empty [])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 23)
                    "both legs are priced by the body's own fatigue factor"
            }

            test "a road under the trunk discounts the loaded leg" {
                // Road weight 1 halves the loaded step to 2 units; the
                // empty leg already rides the floor. Nine steps x 3 units
                // = 27 half-ticks, rounded up to 14.
                Expect.equal
                    (haulRoundTripTicks
                        (corridorWith (Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]) [])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 14)
                    "road parity is worth hiring for"
            }

            test "the pricing is traffic-blind: a standing creep never resizes the fleet" {
                // The quota is capacity planning, not routing: the same
                // corridor with a creep parked mid-lane prices identically
                // — no occupancy surcharge.
                Expect.equal
                    (haulRoundTripTicks
                        (corridorWith Set.empty [ "w", { X = 15; Y = 10 } ])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 23)
                    "today's traffic is not tomorrow's throughput"
            }

            test "an unreachable sink prices no round trip" {
                let gapped =
                    { spatial
                          [ "spawn-1", { X = 20; Y = 10 } ]
                          [
                              for x in 10..20 do
                                  if x <> 15 then
                                      { X = x; Y = 10 }, Plain
                          ] with
                        Obstacles = Set.singleton { X = 20; Y = 10 }
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (haulRoundTripTicks
                        gapped
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    None
                    "unpriceable geometry hires nobody"
            }
        ]
