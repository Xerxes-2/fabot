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
        TicksToLive = 1500
        Fatigue = 0
        Energy = 0
        FreeCapacity = 50
        Body = Map.ofList [ Work, 1; Carry, 1; Move, 1 ]
    }

/// A creep with the given carried energy and body's part counts.
let creepWith name energy body =
    {
        Name = name
        TicksToLive = 1500
        Fatigue = 0
        Energy = energy
        FreeCapacity = 50
        Body = body |> List.countBy id |> Map.ofList
    }

/// The home room's geometry, read back off a projection: the room
/// `RoomName` names, and the one the empty name files when it names none
/// (`SpatialInfo.homeName`). Absent geometry reads as an empty layer, never
/// as a lookup that throws (ADR 0004).
let homeLayer (spatial: SpatialInfo) : RoomLayer =
    SpatialInfo.layerOf spatial (SpatialInfo.homeName spatial)

/// The same projection with the home room's layer changed. Since ADR 0041's
/// contract step the tile-shaped containers live under a room name and
/// nowhere else, so a test that used to copy-update the projection itself
/// — `{ spatial … with CreepPositions = … }` — reaches through this
/// instead. It merges into whatever layer is already there, so composing it
/// with the target funnels below is order-blind. Apply it after `RoomName`
/// is final: the home name is resolved when it runs, and a projection
/// layered then renamed leaves its geometry filed under the old name.
let withHome (change: RoomLayer -> RoomLayer) (spatial: SpatialInfo) : SpatialInfo =
    { spatial with
        Rooms = Map.add (SpatialInfo.homeName spatial) (change (homeLayer spatial)) spatial.Rooms
    }

/// Projection with the given target positions and terrain tiles; no creeps,
/// no obstacles — tests layer those on top. It files them through
/// `withHome` and inherits its ordering rule, which bites hardest here
/// because this funnel starts from `SpatialInfo.empty`: the home name it
/// resolves is the empty one, so a projection built by this and *then*
/// given a `RoomName` carries its geometry under the empty name while
/// `RoomName` says another, and every reader that asks by *room* — the
/// weight grid, the census signature, the hauler quota — answers off
/// `RoomLayer.empty`. The target-keyed queries still find it, because
/// `SpatialInfo.placementOf` scans every layer, which is what makes the
/// mistake quiet. Name the room first, then build.
let spatial targets tiles =
    SpatialInfo.empty
    |> withHome (fun layer ->
        { layer with
            Terrain = Map.ofList tiles
            TargetPositions = Map.ofList targets
        })

/// `mayAct` over a Task's own Work Area — the tiles the decision layer
/// hands it on a tick with nothing taken out of one (ADR 0033).
let mayActFor atlas creep task =
    mayAct atlas creep task (workAreaFor atlas creep task)

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
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 9; Y = 10 }, Plain
                            { X = 11; Y = 10 }, Swamp
                            { X = 10; Y = 9 }, Wall
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 9; Y = 10 }
                        })
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
                    spatial
                        []
                        [
                            { X = 9; Y = 10 }, Plain
                            { X = 10; Y = 9 }, Wall
                            { X = 10; Y = 11 }, Swamp
                            { X = 11; Y = 10 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 11; Y = 10 }
                        })
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (adjacentWalkable atlas { X = 10; Y = 10 })
                    [ { X = 9; Y = 10 }; { X = 10; Y = 11 } ]
                    "unlike Seats, standing respects obstacles"
            }

            test "walkableTiles is the whole room's standing ground, on adjacentWalkable's rules" {
                // The same three exclusions over the projection at large —
                // wall terrain, an obstacle, and everything outside it — and
                // the road that discounts a tile is standing ground like any
                // other (ADR 0033's safe set is built out of this).
                let atlas =
                    spatial
                        []
                        [
                            { X = 9; Y = 10 }, Plain
                            { X = 10; Y = 9 }, Wall
                            { X = 10; Y = 10 }, Swamp
                            { X = 10; Y = 11 }, Plain
                            { X = 11; Y = 10 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 11; Y = 10 }
                            Roads = Set.singleton { X = 10; Y = 11 }
                        })
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (walkableTiles atlas)
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 10; Y = 10 }; { X = 10; Y = 11 } ])
                    "every tile the floods price and no other"
            }

            test "mayAct judges the tiles it is handed, not the Task's whole area" {
                // The area is the caller's (ADR 0033): a tile the decision
                // layer has taken out of it is no tile to act from, however
                // well the action's range reaches the target from there.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [ { X = 10; Y = 10 }, Wall; { X = 10; Y = 11 }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue
                    (mayActFor atlas "w" (Harvest "src-a"))
                    "the Seat it stands on is in the Task's own area"

                Expect.isFalse
                    (mayAct atlas "w" (Harvest "src-a") Set.empty)
                    "and out of a narrowed one, it acts from nowhere"
            }

            test "creepTile places a projected creep, and answers nothing for the rest" {
                let atlas =
                    spatial [] [ { X = 5; Y = 5 }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "amy", { X = 5; Y = 5 } ]
                        })
                    |> snapshotWith [ worker "amy"; worker "ghost" ]
                    |> ofSnapshot

                Expect.equal (creepTile atlas "amy") (Some { X = 5; Y = 5 }) "the tile it stands on"
                Expect.isNone (creepTile atlas "ghost") "a creep the projection does not place"
            }

            test "placedCreeps keeps Snapshot creep order and skips the unplaced" {
                let atlas =
                    spatial [] [ { X = 5; Y = 5 }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions =
                                Map.ofList [ "zed", { X = 5; Y = 5 }; "amy", { X = 6; Y = 6 } ]
                        })
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
            test "homeRoom passes the projection's room through, absent when empty" {
                let named =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal (homeRoom named) (Some "W1N1") "the projection names its room"

                let bare = SpatialInfo.empty |> snapshotWith [] |> ofSnapshot
                Expect.equal (homeRoom bare) None "an empty projection covers no room"
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
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", { X = 10; Y = 10 } ]
                        })
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

            test "a standing link is a built kind: its tile is censused and no longer buildable" {
                // Link is a projection kind with no placeable counterpart
                // (ADR 0022): the Layout never asks for one, it only needs
                // to see the ones that stand, so a link on a footing does
                // not send the footing looking for another tile.
                let atlas =
                    { spatial
                          [ "link-1", { X = 10; Y = 10 }; "sto-1", { X = 10; Y = 11 } ]
                          [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "link-1", Structure BuiltKind.Link
                                    "sto-1", Structure BuiltKind.Storage
                                ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (linkTiles atlas)
                    (Set.singleton { X = 10; Y = 10 })
                    "the link's tile, and no other kind's"

                Expect.equal
                    (storageTiles atlas)
                    (Set.singleton { X = 10; Y = 11 })
                    "the Storage's tile, the anchor its own footing is read from"

                Expect.isEmpty
                    (buildableTiles atlas)
                    "both stand on their tiles: neither takes a site"
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
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", { X = 7; Y = 7 } ]
                        })
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
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", { X = 10; Y = 10 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal (workArea atlas (Repair "cont-1")) Set.empty "nowhere to stand"

                Expect.equal
                    (travelCost atlas "w" (Repair "cont-1"))
                    (Some 0)
                    "an unplaced target prices at 0, never against the Task"

                Expect.isTrue
                    (mayActFor atlas "w" (Repair "cont-1"))
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
    spatial
        [ "src-a", { X = 10; Y = 12 } ]
        [
            { X = 10; Y = 12 }, Wall
            { X = 10; Y = 13 }, Swamp
            { X = 11; Y = 13 }, Plain
            { X = 10; Y = 14 }, Plain
            { X = 11; Y = 14 }, Plain
            { X = 10; Y = 15 }, Plain
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creeps
        })

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
                    projection
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 20; Y = 20 } Plain layer.Terrain
                        })
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
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, terrain
            { X = 10; Y = 12 }, Plain
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ "w", { X = 10; Y = 12 } ]
            Roads = roads
        })

/// Source at (10,10) behind a Seat at (10,11); the creep "w" stands two
/// steps below it at (10,13), so its only route runs through (10,12) —
/// the tile each corridor test dresses. Nothing else is projected, so the
/// corridor is one tile wide and no diagonal skirts the dressed tile.
let corridorThrough middle roads obstacles =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        ([
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 13 }, Plain
         ]
         @ middle)
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ "w", { X = 10; Y = 13 } ]
            Roads = roads
            Obstacles = obstacles
        })

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
                    seatPriced Plain Set.empty
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions =
                                Map.ofList [ "w", { X = 10; Y = 12 }; "b", { X = 10; Y = 11 } ]
                        })
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
                        TargetKinds = Map.ofList [ "src-a", Source; "site-1", Site BuiltKind.Other ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            TargetPositions =
                                Map.ofList
                                    [ "src-a", { X = 10; Y = 10 }; "site-1", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 2)
                    "the unbuilt road's tile still prices as plain"
            }

            test "a road discounts passable ground only, and an obstacle overrides it" {
                // The flood prices off a weight table laid once per tick,
                // not off a per-tile query, so the three tiles where a road
                // does not win are pinned here: on a wall (a tunnel, which
                // the projection does not model), off the terrain
                // projection, and under an obstacle.
                let costThrough middle roads obstacles =
                    let atlas =
                        corridorThrough middle roads obstacles
                        |> snapshotWith [ worker "w" ]
                        |> ofSnapshot

                    travelCost atlas "w" (Harvest "src-a")

                let road = Set.singleton { X = 10; Y = 12 }
                let blocked = Set.singleton { X = 10; Y = 12 }
                let plain = [ { X = 10; Y = 12 }, Plain ]
                let wall = [ { X = 10; Y = 12 }, Wall ]

                Expect.equal
                    (costThrough plain road Set.empty)
                    (Some 3)
                    "a road on plain carries the corridor: one road step, then a plain Seat"

                Expect.equal
                    (costThrough wall road Set.empty)
                    None
                    "a road on a wall is a tunnel the projection does not model: impassable"

                Expect.equal
                    (costThrough [] road Set.empty)
                    None
                    "a road on a tile outside the terrain projection stays impassable"

                Expect.equal
                    (costThrough plain road blocked)
                    None
                    "an obstacle over a road blocks the tile: the obstacle wins"
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

/// A paved lane running east from the source's only Seat: source at
/// (10,10) in wall, Seat at (11,10), road tiles from there out to
/// (19,10). A creep at (19,10) is eight road steps from the Seat —
/// #79's corridor.
let roadCorridor creeps =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [ for x in 10..19 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ]
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]
            CreepPositions = Map.ofList creeps
        })

/// The same lane unpaved: six plain steps from (17,10) to the Seat.
let plainCorridor creeps =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [ for x in 10..17 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creeps
        })

/// A varied room for the walk's floor property: 15 × 15 of mixed terrain
/// with scattered single walls — spaced four apart, so no two touch and
/// the passable tiles stay one connected component, which makes every
/// Work Area tile reachable from every creep. Sources sit at four corners
/// of the interior.
let mixedRoom creeps =
    let tiles =
        [
            for x in 0..14 do
                for y in 0..14 do
                    if x % 4 = 1 && y % 4 = 1 then { X = x; Y = y }, Wall
                    elif (x + y) % 3 = 0 then { X = x; Y = y }, Swamp
                    else { X = x; Y = y }, Plain
        ]

    spatial
        [
            for i, tile in List.indexed [ { X = 3; Y = 3 }; { X = 11; Y = 4 }; { X = 6; Y = 12 } ] ->
                $"src-%d{i}", tile
        ]
        tiles
    |> withHome (fun layer ->
        { layer with
            Roads =
                Set.ofList
                    [
                        for x in 0..14 do
                            for y in 0..14 do
                                if (2 * x + y) % 5 = 0 then
                                    { X = x; Y = y }
                    ]
            CreepPositions = Map.ofList creeps
        })

[<Tests>]
let walkTests =
    testList
        "atlas walk"
        [
            test "the walk is at least the Chebyshev distance to the tile it reaches" {
                // ADR 0029's floor, checked over a fixture rather than an
                // example: every body against every source in a room of
                // mixed terrain, roads and scattered walls. No body crosses
                // a tile in less than a tick, so no walk may price below
                // the tiles it must cross — the defect #79 reported, stated
                // as a property the pricing cannot break.
                let bodies =
                    [
                        "worker", 0, [ Work; Carry; Move ]
                        "loaded", 50, [ Work; Carry; Move ]
                        "hauler", 0, [ Carry; Carry; Move ]
                        "anchor", 0, [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "surplus", 0, [ Work; Move; Move; Move ]
                    ]

                let starts =
                    [ { X = 0; Y = 0 }; { X = 14; Y = 0 }; { X = 7; Y = 8 }; { X = 12; Y = 13 } ]

                let sources = [ "src-0"; "src-1"; "src-2" ]

                let violations =
                    [
                        for name, energy, body in bodies do
                            for start in starts do
                                let atlas =
                                    mixedRoom [ name, start ]
                                    |> snapshotWith [ creepWith name energy body ]
                                    |> ofSnapshot

                                for source in sources do
                                    let task = Harvest source

                                    let floor =
                                        workArea atlas task
                                        |> Set.toList
                                        |> List.map (range start)
                                        |> function
                                            | [] -> 0
                                            | tiles -> List.min tiles

                                    match walkTicks atlas name task with
                                    | Some walk when walk < floor ->
                                        yield
                                            $"%s{name} from %A{start} to %s{source}: walk %d{walk} under floor %d{floor}"
                                    | _ -> ()
                    ]

                Expect.isEmpty violations "no walk prices below the tiles it must cross"
            }

            test "eight road tiles are eight ticks for an empty road-parity body, not four" {
                // #79's worked example. The empty hauler unit generates no
                // fatigue at all, so travel cost floors its road step at
                // one unit — half a tick — and eight steps read as four
                // ticks once halved. The walk floors the same step at a
                // whole tick: eight tiles, eight ticks.
                let atlas =
                    roadCorridor [ "h", { X = 19; Y = 10 } ]
                    |> snapshotWith [ creepWith "h" 0 [ Carry; Carry; Move ] ]
                    |> ofSnapshot

                Expect.equal
                    (walkTicks atlas "h" (Harvest "src-a"))
                    (Some 8)
                    "eight tiles cannot be crossed in fewer than eight ticks"

                Expect.equal
                    (travelCost atlas "h" (Harvest "src-a"))
                    (Some 8)
                    "travel cost is untouched: eight units, the ranking price the old rule halved"
            }

            test "the floor lifts a cheap step without capping a dear one" {
                // The floor is a floor, not a rounding: a worker unit's
                // road step is one tick where travel cost priced it half a
                // one, and its swamp step stays the five ticks the engine
                // charges — ceil(10 / 2) — rather than flattening to the
                // floor beside it.
                let walkOn terrain roads =
                    let atlas =
                        seatPriced terrain roads |> snapshotWith [ worker "w" ] |> ofSnapshot

                    walkTicks atlas "w" (Harvest "src-a")

                let road = Set.singleton { X = 10; Y = 11 }

                Expect.equal (walkOn Plain road) (Some 1) "a road step is one tick"
                Expect.equal (walkOn Plain Set.empty) (Some 1) "so is a plain step"
                Expect.equal (walkOn Swamp Set.empty) (Some 5) "a swamp step is five, not one"
            }

            test "a Move surplus buys travel cost half a tick a tile and the walk nothing" {
                // The two numbers deliberately disagree. Three Moves under
                // one Work price a plain step at ceil(2 / 3) = 1 unit — the
                // per-unit floor, which is half of what a plain step costs
                // the worker unit beside it — so six tiles cost six units,
                // and the old rule read those as three ticks. The walk
                // floors each step at a whole tick: six tiles, six ticks.
                // Travel cost keeps ranking the fast body ahead; the clock
                // refuses to believe it.
                let atlas =
                    plainCorridor [ "s", { X = 17; Y = 10 } ]
                    |> snapshotWith [ creepWith "s" 0 [ Work; Move; Move; Move ] ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "s" (Harvest "src-a"))
                    (Some 6)
                    "one unit a tile, half what the worker unit pays for the same ground"

                Expect.equal
                    (walkTicks atlas "s" (Harvest "src-a"))
                    (Some 6)
                    "one tick a tile: no body crosses a tile faster than that"
            }

            test "the walk is blind to standing traffic" {
                // #78 inverted at the Atlas seam: the occupancy surcharge
                // re-prices travel cost around a bystander and the walk does
                // not move, because a creep standing in the lane this tick
                // is not part of the path's physical length.
                let clear =
                    corridor [ "w", { X = 10; Y = 15 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                let crowded =
                    corridor [ "w", { X = 10; Y = 15 }; "b", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost clear "w" (Harvest "src-a"),
                     travelCost crowded "w" (Harvest "src-a"))
                    (Some 4, Some 12)
                    "travel cost sees the crowd and detours into the swamp Seat"

                Expect.equal
                    (walkTicks clear "w" (Harvest "src-a"), walkTicks crowded "w" (Harvest "src-a"))
                    (Some 2, Some 2)
                    "the walk prices the same two tiles either way"
            }

            test "totality follows travel cost's contract" {
                // ADR 0004, verbatim from travel cost (ADR 0029 changes the
                // pricing, never the contract): unplaceable geometry prices
                // 0, an unreachable Work Area has no walk at all, and a
                // creep already inside has none left to walk.
                let unplaced = corridor [] |> snapshotWith [ worker "w" ] |> ofSnapshot

                Expect.equal
                    (walkTicks unplaced "w" (Harvest "src-a"))
                    (Some 0)
                    "an unplaced creep prices at 0"

                let placed =
                    corridor [ "w", { X = 10; Y = 15 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (walkTicks placed "w" (Harvest "ghost"))
                    (Some 0)
                    "an unplaced target prices at 0, not None"

                let inside =
                    corridor [ "w", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (walkTicks inside "w" (Harvest "src-a"))
                    (Some 0)
                    "already there: no walk left to cover anything"

                let island = corridor [ "w", { X = 20; Y = 20 } ]

                let stranded =
                    island
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 20; Y = 20 } Plain layer.Terrain
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (walkTicks stranded "w" (Harvest "src-a"))
                    None
                    "an unreachable Work Area has no walk: readers count from now"
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
                    spatial
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
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", { X = 10; Y = 14 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the swamp lane for the plain one"
            }

            test "the first step detours around a standing creep when a lane is open" {
                // Same shape as the swamp detour, but on all-plain ground
                // with a creep parked mid-lane: the occupancy surcharge
                // sends the first step into the free lane at x = 11.
                let atlas =
                    spatial
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
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions =
                                Map.ofList [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                        })
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the parked creep's lane for the free one"
            }

            test "a creep already inside the Work Area has no step to take" {
                let atlas =
                    corridor [ "w", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    None
                    "already there"
            }

            test "an unreachable Work Area yields no step: waiting beats marching at a wall" {
                let projection = corridor [ "w", { X = 20; Y = 20 } ]

                let atlas =
                    projection
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 20; Y = 20 } Plain layer.Terrain
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    None
                    "no path, no step"
            }

            test "an unplaced creep has no step: no movement without geometry" {
                let atlas = corridor [] |> snapshotWith [ worker "w" ] |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    None
                    "nothing derivable"
            }
        ]

[<Tests>]
let firstStepIgnoringTrafficTests =
    testList
        "atlas firstStepIgnoringTraffic"
        [
            test "the traffic-blind step keeps the lane the surcharge steers the priced step out of" {
                // The reroute attribution's whole comparison (ADR 0008, ADR
                // 0009): the same body over the same ground, once with
                // today's crowd priced in and once without. A creep parked
                // mid-lane bends the priced step into the parallel lane;
                // the blind step walks straight at it.
                let atlas =
                    spatial
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
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions =
                                Map.ofList [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                        })
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofSnapshot

                Expect.equal
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    (Some { X = 11; Y = 13 })
                    "the priced step leaves the parked creep's lane"

                Expect.equal
                    (firstStepIgnoringTraffic atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    (Some { X = 10; Y = 13 })
                    "the blind step holds the lane: the detour is the surcharge's doing"
            }

            test "the blind route is priced in travel cost's units, never the walk's ticks" {
                // Two lanes to two Seats, and the two prices choose
                // differently. The paved lane is three road steps (3 units,
                // 3 ticks); the bare lane is two plain steps (4 units, 2
                // ticks). Travel cost's units buy the trunk — which is what
                // the trunk is for — and the walk's whole ticks flatten
                // road and plain for this body and take the short lane
                // instead. The attribution compares against firstStep's
                // route, so it must read the units: the shared memo's
                // traffic-blind entries are two, and this is the other one.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 10; Y = 10 }, Wall
                            { X = 11; Y = 9 }, Plain
                            { X = 11; Y = 10 }, Wall
                            { X = 11; Y = 11 }, Plain
                            { X = 12; Y = 8 }, Plain
                            { X = 12; Y = 9 }, Wall
                            { X = 12; Y = 10 }, Wall
                            { X = 12; Y = 11 }, Plain
                            { X = 13; Y = 9 }, Plain
                            { X = 13; Y = 10 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Roads =
                                Set.ofList
                                    [ { X = 13; Y = 9 }; { X = 12; Y = 8 }; { X = 11; Y = 9 } ]
                            CreepPositions = Map.ofList [ "w", { X = 13; Y = 10 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 3)
                    "three road steps beat two plain ones in units"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-a"))
                    (Some 2)
                    "two plain steps beat three road ones in whole ticks"

                Expect.equal
                    (firstStepIgnoringTraffic atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    (firstStep atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    "empty ground: the blind route is the priced one, down the paved lane"

                Expect.equal
                    (firstStepIgnoringTraffic atlas "w" (workAreaFor atlas "w" (Harvest "src-a")))
                    (Some { X = 13; Y = 9 })
                    "the road lane, not the whole-tick lane at (12,11)"
            }
        ]

[<Tests>]
let workAreaForTests =
    testList
        "atlas workAreaFor"
        [
            // Source at (10,10) with three Seats: (9,10) carries a built
            // container, (11,10) lies inside the controller's Upgrade area,
            // (10,11) is an ordinary Seat. Both Posts, one plain Seat.
            let posted creeps =
                { spatial
                      [
                          "src-a", { X = 10; Y = 10 }
                          "ctrl-1", { X = 14; Y = 10 }
                          "cont-1", { X = 9; Y = 10 }
                      ]
                      [
                          { X = 9; Y = 10 }, Plain
                          { X = 11; Y = 10 }, Plain
                          { X = 10; Y = 11 }, Plain
                      ] with
                    TargetKinds =
                        Map.ofList
                            [
                                "src-a", Source
                                "ctrl-1", Controller
                                "cont-1", Structure BuiltKind.Container
                            ]
                }
                |> withHome (fun layer ->
                    { layer with
                        CreepPositions = Map.ofList creeps
                    })

            let anchor name =
                creepWith name 0 [ Work; Work; Carry; Move ]

            test "a Work-heavy body harvests a posted source from its Posts alone" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofSnapshot

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a"))
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 } ])
                    "the container Seat and the Dual Seat, not the plain Seat"

                Expect.equal
                    (workArea atlas (Harvest "src-a"))
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 }; { X = 10; Y = 11 } ])
                    "the body-blind area keeps every Seat"
            }

            test "a light body keeps every Seat of the same source" {
                let atlas =
                    posted [ "w", { X = 10; Y = 11 } ] |> snapshotWith [ worker "w" ] |> ofSnapshot

                Expect.equal
                    (workAreaFor atlas "w" (Harvest "src-a"))
                    (workArea atlas (Harvest "src-a"))
                    "Work <= Move narrows nothing"
            }

            test "a source with no Post narrows nothing, heavy body or not" {
                // Same geometry with the container gone and the controller
                // out of range: the source has neither kind of Post.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          [
                              { X = 9; Y = 10 }, Plain
                              { X = 11; Y = 10 }, Plain
                              { X = 10; Y = 11 }, Plain
                          ] with
                        TargetKinds = Map.ofList [ "src-a", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "a", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ anchor "a" ]
                    |> ofSnapshot

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a"))
                    (workArea atlas (Harvest "src-a"))
                    "the full Seat set is the fallback before the first container"
            }

            test "only Harvest narrows: the heavy body's Upgrade area is untouched" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofSnapshot

                Expect.equal
                    (workAreaFor atlas "a" (Upgrade "ctrl-1"))
                    (workArea atlas (Upgrade "ctrl-1"))
                    "Upgrade is body-blind (ADR 0020)"
            }

            test "an unplaceable source stays empty for a heavy body" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofSnapshot

                Expect.equal (workAreaFor atlas "a" (Harvest "ghost")) Set.empty "nowhere to stand"
            }

            test "a blocked Post empties the area rather than widening back to the Seats" {
                // An obstacle stands on the container Seat: it is still a
                // Post by census, so the area narrows to it and stays
                // empty — Harvest goes inapplicable, as an unreachable Work
                // Area does for every Task (ADR 0020), instead of silently
                // handing the heavy body every Seat back.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 9; Y = 10 }
                            CreepPositions = Map.ofList [ "a", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ creepWith "a" 0 [ Work; Work; Carry; Move ] ]
                    |> ofSnapshot

                Expect.equal
                    (postsOf atlas "src-a")
                    (Set.singleton { X = 9; Y = 10 })
                    "the Seat under the container is a Post by census"

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a"))
                    Set.empty
                    "a Post nothing may stand on leaves nowhere to stand"

                Expect.equal
                    (travelCost atlas "a" (Harvest "src-a"))
                    None
                    "so the Task is inapplicable — no retry with the full Seat set"
            }

            test "travel cost and first step price the Post, not the nearer Seat" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofSnapshot

                Expect.isSome
                    (travelCost atlas "a" (Harvest "src-a"))
                    "the Post is reachable, so Harvest stays applicable"

                Expect.notEqual
                    (travelCost atlas "a" (Harvest "src-a"))
                    (Some 0)
                    "standing on a plain Seat is no longer standing in the area"

                Expect.equal
                    (firstStep atlas "a" (workAreaFor atlas "a" (Harvest "src-a")))
                    (Some { X = 9; Y = 10 })
                    "the step goes to a Post — equally cheap, lowest tile wins"
            }
        ]

[<Tests>]
let mayActTests =
    testList
        "atlas mayAct"
        [
            test "acting is judged by the action's range from the tick-start position" {
                let atlasAt creepPos =
                    spatial
                        [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 20; Y = 20 } ]
                        [ for y in 11..15 -> { X = 10; Y = y }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", creepPos ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue
                    (mayActFor (atlasAt { X = 10; Y = 11 }) "w" (Harvest "src-a"))
                    "harvest reaches at range 1"

                Expect.isFalse
                    (mayActFor (atlasAt { X = 10; Y = 12 }) "w" (Harvest "src-a"))
                    "harvest does not reach at range 2"

                Expect.isTrue
                    (mayActFor (atlasAt { X = 18; Y = 17 }) "w" (Upgrade "ctrl-1"))
                    "upgrade reaches at range 3"
            }

            test "repair reaches at range 3, like build and upgrade" {
                let atlasAt creepPos =
                    spatial
                        [ "road-1", { X = 10; Y = 10 } ]
                        [ for y in 10..15 -> { X = 10; Y = y }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "w", creepPos ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue
                    (mayActFor (atlasAt { X = 10; Y = 13 }) "w" (Repair "road-1"))
                    "repair reaches at range 3"

                Expect.isFalse
                    (mayActFor (atlasAt { X = 10; Y = 14 }) "w" (Repair "road-1"))
                    "repair does not reach at range 4"
            }

            test "a creep or target the projection cannot place never blocks the action" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.isTrue (mayActFor atlas "ghost" (Harvest "src-a")) "unplaced creep acts"
                Expect.isTrue (mayActFor atlas "w" (Harvest "ghost")) "unplaced target is acted on"
            }

            test "a Work-heavy body digs from its Post and nowhere else in range" {
                // Source at (10,10) with a built container on the Seat at
                // (9,10) — the source's only Post. The plain Seat (10,11) is
                // in harvest range all the same.
                let atlasAt creepPos =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "a", creepPos ]
                        })
                    |> snapshotWith [ creepWith "a" 0 [ Work; Work; Carry; Move ] ]
                    |> ofSnapshot

                Expect.isTrue
                    (mayActFor (atlasAt { X = 9; Y = 10 }) "a" (Harvest "src-a"))
                    "on the Post it digs"

                Expect.isFalse
                    (mayActFor (atlasAt { X = 10; Y = 11 }) "a" (Harvest "src-a"))
                    "in range but off the Post it does not — so it never fills en route"
            }

            test "a creep on a tile the projection calls impassable is judged by range" {
                // An obstacle-type site dropped under a standing creep: the
                // engine lets it stay, and it keeps working (ADR 0004).
                let atlas =
                    spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 10; Y = 11 }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 10; Y = 11 }
                            CreepPositions = Map.ofList [ "w", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal
                    (workArea atlas (Harvest "src-a"))
                    Set.empty
                    "the creep's own tile is not a standing tile"

                Expect.isTrue
                    (mayActFor atlas "w" (Harvest "src-a"))
                    "unpriceable footing never blocks the action"
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
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 11; Y = 10 }
                        })
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
let workingGroundTests =
    testList
        "atlas workingGround"
        [
            test "the working ground is every source's Seats plus the Upgrade Work Area" {
                // Source at (10,10) with three projected neighbours — the
                // wall is no Seat — and a controller at (13,10) whose
                // Upgrade Work Area reaches (12,10), (11,10) and (10,11) —
                // the two Seats in both halves count once.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 13; Y = 10 } ]
                          [
                              { X = 9; Y = 10 }, Plain
                              { X = 10; Y = 11 }, Swamp
                              { X = 11; Y = 10 }, Plain
                              { X = 11; Y = 9 }, Wall
                              { X = 12; Y = 10 }, Plain
                          ] with
                        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (workingGround atlas)
                    (Set.ofList
                        [
                            { X = 9; Y = 10 }
                            { X = 10; Y = 11 }
                            { X = 11; Y = 10 }
                            { X = 12; Y = 10 }
                        ])
                    "the Seats the Anchors stand on and the tiles the upgraders stand on, together"
            }

            test "a room with neither sources nor a controller works no ground" {
                let atlas =
                    { spatial [ "spawn-1", { X = 10; Y = 10 } ] [ { X = 10; Y = 11 }, Plain ] with
                        TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (workingGround atlas)
                    Set.empty
                    "geometry the projection cannot place answers empty, and nothing is off-limits"
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
                    spatial
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
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 11; Y = 12 }
                        })

                let standing =
                    (homeLayer projection).Terrain
                    |> Map.toList
                    |> List.choose (fun (tile, kind) ->
                        if
                            kind <> Wall
                            && not (Set.contains tile (homeLayer projection).Obstacles)
                        then
                            Some tile
                        else
                            None)

                let task = Harvest "src-a"

                for pos in standing do
                    let atlas =
                        projection
                        |> withHome (fun layer ->
                            { layer with
                                CreepPositions = Map.ofList [ "w", pos ]
                            })
                        |> snapshotWith [ worker "w" ]
                        |> ofSnapshot

                    let area = workArea atlas task
                    let cost = travelCost atlas "w" task
                    let step = firstStep atlas "w" area

                    if Set.contains pos area then
                        Expect.equal cost (Some 0) $"inside the Work Area costs 0 at {pos}"
                        Expect.equal step None $"no step inside the Work Area at {pos}"

                        Expect.isTrue
                            (mayActFor atlas "w" task)
                            $"in-area implies in range at {pos}"
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
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 9; Y = 10 }, Plain
                            { X = 11; Y = 10 }, Swamp
                            { X = 10; Y = 9 }, Wall
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 9; Y = 10 }
                        })
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
                    corridor
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 12; Y = 10 } Swamp layer.Terrain
                            Roads = Set.singleton { X = 12; Y = 10 }
                        })
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
                spatial
                    [ "spawn-1", { X = 20; Y = 10 } ]
                    [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
                |> withHome (fun layer ->
                    { layer with
                        Obstacles = Set.singleton { X = 20; Y = 10 }
                        Roads = roads
                        CreepPositions = Map.ofList creeps
                    })
                |> snapshotWith []
                |> ofSnapshot

            test "the loaded leg out and the empty leg back sum to whole ticks" {
                // Each leg is a walk (ADR 0029). [Carry;Carry;Move]
                // loaded on plain: two full Carry x weight 2 over one Move
                // is 4 units a step, ceil(4 / 2) = 2 ticks. Empty Carry
                // rides free, so the leg back sits on the one-tick floor.
                // Nine steps out at 2 and nine back at 1 = 27 ticks, with
                // nothing halved on the total.
                Expect.equal
                    (haulRoundTripTicks
                        (corridorWith Set.empty [])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 27)
                    "both legs are priced by the body's own fatigue factor"
            }

            test "a road under the trunk discounts the loaded leg" {
                // Road weight 1 halves the loaded step to 2 units, one
                // tick; the empty leg already rides the floor. Nine steps
                // out and nine back at a tick apiece = 18.
                Expect.equal
                    (haulRoundTripTicks
                        (corridorWith (Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]) [])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 18)
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
                    (Some 27)
                    "today's traffic is not tomorrow's throughput"
            }

            test "neither leg prices below the tiles it crosses" {
                // ADR 0029's floor, on this reader too: the round trip is
                // two walks, so it can never price below twice the
                // Chebyshev distance to the sink's nearest goal. The guard
                // that makes reintroducing the trailing halve-and-round-up
                // go red — under it the Move-surplus body below crossed
                // eighteen tiles in nine ticks.
                let floor = 2 * range { X = 10; Y = 10 } { X = 19; Y = 10 }

                for body in
                    [
                        [ Carry; Carry; Move ]
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        [ Carry; Move; Move; Move ]
                    ] do
                    match
                        haulRoundTripTicks
                            (corridorWith Set.empty [])
                            body
                            { X = 10; Y = 10 }
                            { X = 20; Y = 10 }
                    with
                    | Some ticks ->
                        Expect.isGreaterThanOrEqual
                            ticks
                            floor
                            $"%A{body} rounds a nine-tile leg below nine ticks"
                    | None -> failtest $"%A{body} should reach the sink"
            }

            test "an unreachable sink prices no round trip" {
                let gapped =
                    spatial
                        [ "spawn-1", { X = 20; Y = 10 } ]
                        [
                            for x in 10..20 do
                                if x <> 15 then
                                    { X = x; Y = 10 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 20; Y = 10 }
                        })
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

[<Tests>]
let castWalkTicksTests =
    testList
        "atlas castWalkTicks"
        [
            // Corridor y = 10, x = 10..20: the spawn structure standing at
            // (20,10) — an obstacle, so the one tile a replacement can be
            // born on is (19,10) — and the tile it must reach nine steps
            // further on at (10,10).
            let corridorWith roads creeps =
                spatial
                    [ "spawn-1", { X = 20; Y = 10 } ]
                    [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
                |> withHome (fun layer ->
                    { layer with
                        Obstacles = Set.singleton { X = 20; Y = 10 }
                        Roads = roads
                        CreepPositions = Map.ofList creeps
                    })
                |> snapshotWith []
                |> ofSnapshot

            /// The Anchor row's shape empty: four Work and a Carry over one
            /// Move, so a plain step costs 8 units.
            let anchorBody = [ Work; Work; Work; Work; Carry; Move ]

            test "the walk is priced for the body given, not for any creep standing there" {
                // The lead's whole point (ADR 0026): an empty Anchor body
                // pays 8 units a plain step, ceil(8 / 2) = 4 ticks, so the
                // nine steps out of the spawner cost 36. A hauler unit over
                // the same ground carries no fatigue empty and rides the
                // walk's one-tick floor: 9.
                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        anchorBody
                        { X = 20; Y = 10 }
                        { X = 10; Y = 10 })
                    (Some 36)
                    "a slow body earns a long lead"

                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        [ Carry; Carry; Move ]
                        { X = 20; Y = 10 }
                        { X = 10; Y = 10 })
                    (Some 9)
                    "a hauler on the same ground earns a short one"
            }

            test "the walk starts beside the spawner, on the tile the engine places the body on" {
                // The step out of the spawner's own tile is one the
                // replacement never walks: the engine puts a finished creep
                // on a free neighbour. Charging it would buy the lead a
                // whole plain step — 4 ticks for this body — and cast the
                // successor that much too early to be admitted to the tile
                // it is walking to.
                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        anchorBody
                        { X = 20; Y = 10 }
                        { X = 19; Y = 10 })
                    (Some 0)
                    "a replacement is born beside the spawner, not moved there"
            }

            test "a road under the walk discounts it, as it discounts travel cost" {
                // Road weight 1 quarters the Anchor's step to 4 units —
                // 2 ticks — over the eight paved tiles it steps onto; the
                // last step onto the unpaved (10,10) still costs 8 units,
                // 4 ticks. 16 + 4 = 20.
                Expect.equal
                    (castWalkTicks
                        (corridorWith (Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]) [])
                        anchorBody
                        { X = 20; Y = 10 }
                        { X = 10; Y = 10 })
                    (Some 20)
                    "the trunk shortens a succession"
            }

            test "the pricing is traffic-blind: today's crowd never moves a succession" {
                // A lead is planning, not routing — the same corridor with
                // a creep parked mid-lane prices identically, with no
                // occupancy surcharge.
                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [ "w", { X = 15; Y = 10 } ])
                        anchorBody
                        { X = 20; Y = 10 }
                        { X = 10; Y = 10 })
                    (Some 36)
                    "a standing creep is not a detour a replacement will still face"
            }

            test "the cast walk prices no tile below a tick" {
                // ADR 0029's floor, on the lead's reader too: the walk out
                // of the spawner starts beside it, so it can never price
                // below the Chebyshev distance from the birth tile (19,10)
                // to the goal. The guard that makes reintroducing the
                // trailing halving go red — under it a Move-surplus body
                // walked nine tiles in five ticks.
                let floor = range { X = 19; Y = 10 } { X = 10; Y = 10 }

                for body in [ anchorBody; [ Carry; Carry; Move ]; [ Work; Move; Move; Move ] ] do
                    match
                        castWalkTicks
                            (corridorWith Set.empty [])
                            body
                            { X = 20; Y = 10 }
                            { X = 10; Y = 10 }
                    with
                    | Some ticks ->
                        Expect.isGreaterThanOrEqual
                            ticks
                            floor
                            $"%A{body} leads on a walk shorter than its tiles"
                    | None -> failtest $"%A{body} should reach the goal"
            }

            test "an unreachable tile prices no walk" {
                let gapped =
                    spatial
                        [ "spawn-1", { X = 20; Y = 10 } ]
                        [
                            for x in 10..20 do
                                if x <> 15 then
                                    { X = x; Y = 10 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 20; Y = 10 }
                        })
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (castWalkTicks
                        gapped
                        [ Work; Carry; Move ]
                        { X = 20; Y = 10 }
                        { X = 10; Y = 10 })
                    None
                    "unpriceable geometry leads nobody"
            }

            test "a spawner with no free neighbour prices no walk" {
                // The other half of ADR 0004's totality: there is nowhere
                // for the replacement to be born, so the walk is
                // unpriceable and the row leads nobody.
                let walled =
                    spatial
                        [ "spawn-1", { X = 20; Y = 10 } ]
                        [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.ofList [ { X = 20; Y = 10 }; { X = 19; Y = 10 } ]
                        })
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (castWalkTicks
                        walled
                        [ Work; Carry; Move ]
                        { X = 20; Y = 10 }
                        { X = 10; Y = 10 })
                    None
                    "a spawner that can place nothing leads nobody"
            }
        ]

[<Tests>]
let walkRecallTests =
    testList
        "atlas walk table recall"
        [
            // The castWalkTicks corridor: the spawn structure stands at
            // (20,10), so (19,10) is the one tile a body is born on, and
            // (10,10) lies nine steps further west.
            let corridorSnapshot () =
                spatial
                    [ "spawn-1", { X = 20; Y = 10 } ]
                    [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
                |> withHome (fun layer ->
                    { layer with
                        Obstacles = Set.singleton { X = 20; Y = 10 }
                    })
                |> snapshotWith []

            let hauler = [ Carry; Carry; Move ]
            let spawnTile = { X = 20; Y = 10 }
            let goal = { X = 10; Y = 10 }

            /// The one flood a corridor's lead pricing lays, read off the
            /// table by value: the array itself, so a table that was read
            /// rather than refilled is visible as the very array the first
            /// Atlas allocated.
            let flood (walks: WalkTable) =
                walks |> Seq.map (fun entry -> entry.Value) |> Seq.exactlyOne

            test "a table handed in is filled once and recalled by the next Atlas over it" {
                // ADR 0032: every input of this flood is in the census, so
                // an Atlas handed a filled table reads the entry instead of
                // running the Dijkstra a second time.
                let walks = WalkTable()
                let first = corridorSnapshot () |> ofSnapshotRecalling walks

                Expect.equal
                    (castWalkTicks first hauler spawnTile goal)
                    (Some 9)
                    "the first Atlas floods to price the lead"

                Expect.equal walks.Count 1 "and leaves the flood in the table it was handed"
                let flooded = flood walks

                let second = corridorSnapshot () |> ofSnapshotRecalling walks

                Expect.equal
                    (castWalkTicks second hauler spawnTile goal)
                    (Some 9)
                    "the recalled walk prices the same lead"

                Expect.equal walks.Count 1 "no second entry under the same key"

                Expect.isTrue
                    (obj.ReferenceEquals(flood walks, flooded))
                    "the second Atlas read the first's flood rather than running its own"
            }

            test "an Atlas with nothing to recall floods for itself" {
                // The other half of the seam: a table with nothing in it is
                // flooded into, so a caller that dropped its memo prices the
                // same lead off its own Dijkstra.
                let fresh = WalkTable()
                let atlas = corridorSnapshot () |> ofSnapshotRecalling fresh

                Expect.equal
                    (castWalkTicks atlas hauler spawnTile goal)
                    (Some 9)
                    "an empty table is flooded into, and prices the lead identically"

                Expect.equal fresh.Count 1 "the flood it ran is left in it"

                Expect.equal
                    (castWalkTicks (corridorSnapshot () |> ofSnapshot) hauler spawnTile goal)
                    (Some 9)
                    "and the plain entry point, which lays its own table, agrees"
            }
        ]

[<Tests>]
let controllerContainerTests =
    testList
        "atlas controllerContainers"
        [
            test "the buffer is a built container in the Upgrade area off every Seat" {
                // Source at (10,10), controller at (14,10): "can-src" sits on
                // the Seat (11,10), "can-ctrl" at (13,10) inside the Upgrade
                // Work Area and on no Seat.
                let atlas =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 14; Y = 10 }
                              "can-src", { X = 11; Y = 10 }
                              "can-ctrl", { X = 13; Y = 10 }
                          ]
                          [ for x in 11..13 -> { X = x; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "can-src", Structure BuiltKind.Container
                                    "can-ctrl", Structure BuiltKind.Container
                                ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (controllerContainers atlas)
                    (Set.singleton "can-ctrl")
                    "the source container is intake, not buffer, however near the controller"
            }

            test "a container site, a far container and a controllerless room are no buffer" {
                let atlasWith kinds ctrlPos =
                    { spatial
                          ([ "can-ctrl", { X = 13; Y = 10 } ]
                           @ (ctrlPos |> Option.toList |> List.map (fun p -> "ctrl-1", p)))
                          [ for x in 11..20 -> { X = x; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList kinds
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (controllerContainers (
                        atlasWith
                            [ "ctrl-1", Controller; "can-ctrl", Site BuiltKind.Container ]
                            (Some { X = 14; Y = 10 })
                    ))
                    Set.empty
                    "a pending container buffers nothing"

                Expect.equal
                    (controllerContainers (
                        atlasWith
                            [ "ctrl-1", Controller; "can-ctrl", Structure BuiltKind.Container ]
                            (Some { X = 20; Y = 10 })
                    ))
                    Set.empty
                    "out of the Upgrade Work Area, a container is nobody's buffer"

                Expect.equal
                    (controllerContainers (
                        atlasWith [ "can-ctrl", Structure BuiltKind.Container ] None
                    ))
                    Set.empty
                    "no controller, no buffer — the empty answer opens the gate (ADR 0004)"
            }
        ]

/// A projection carrying border rings under room names — the Seam query's
/// whole input, and nothing else, so a test that names three exit tiles
/// documents the rule the way `spatial`'s three ground tiles do. A tile a
/// ring leaves out is impassable, exactly as a tile missing from the
/// ground is.
let bordered rings =
    { SpatialInfo.empty with
        Borders = rings |> List.map (fun (room, tiles) -> room, Map.ofList tiles) |> Map.ofList
    }

[<Tests>]
let seamTests =
    testList
        "atlas seams"
        [
            test "a north neighbour's band joins this room's y=0 to the neighbour's y=49" {
                // W12S28 sits at world (-13,28) and W12S27 at (-13,27), so
                // W12S27 is the room across the top border: the pairing the
                // engine makes is x for x, y=0 onto y=49. A swamp exit is in
                // the band, dearly, exactly as swamp ground is; the wall is
                // not.
                let atlas =
                    bordered
                        [
                            "W12S28",
                            [
                                { X = 10; Y = 0 }, Plain
                                { X = 11; Y = 0 }, Swamp
                                { X = 12; Y = 0 }, Wall
                            ]
                            "W12S27",
                            [
                                { X = 10; Y = 49 }, Plain
                                { X = 11; Y = 49 }, Plain
                                { X = 12; Y = 49 }, Plain
                            ]
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seams atlas "W12S28" "W12S27")
                    [ { X = 10; Y = 0 }, { X = 10; Y = 49 }; { X = 11; Y = 0 }, { X = 11; Y = 49 } ]
                    "the passable exits, each beside the tile it lands on, in (X, Y) order"
            }

            test "a wall on the far side takes the pair out, as one on this side does" {
                // The band is what a creep can cross, so both halves have to
                // be ground: an exit onto a wall lands nowhere.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 10; Y = 0 }, Plain; { X = 11; Y = 0 }, Plain ]
                            "W12S27", [ { X = 10; Y = 49 }, Wall; { X = 11; Y = 49 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seams atlas "W12S28" "W12S27")
                    [ { X = 11; Y = 0 }, { X = 11; Y = 49 } ]
                    "only the pair that is ground on both sides"
            }

            test "a west neighbour's band joins x=0 to x=49" {
                // W13S28 is world (-14,28): one room further west, so the
                // shared border is a column, and the pairing runs y for y.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 0; Y = 30 }, Plain; { X = 0; Y = 31 }, Plain ]
                            "W13S28", [ { X = 49; Y = 30 }, Plain; { X = 49; Y = 31 }, Swamp ]
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seams atlas "W12S28" "W13S28")
                    [ { X = 0; Y = 30 }, { X = 49; Y = 30 }; { X = 0; Y = 31 }, { X = 49; Y = 31 } ]
                    "the west column, in (X, Y) order"
            }

            test "the band reads the same from the far side, every pair swapped" {
                // The south and east borders are the north's and the west's
                // read the other way round, which is the whole of what
                // "adjacent" means here: one band, asked from either end.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 10; Y = 0 }, Plain; { X = 0; Y = 30 }, Plain ]
                            "W12S27", [ { X = 10; Y = 49 }, Plain ]
                            "W13S28", [ { X = 49; Y = 30 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seams atlas "W12S27" "W12S28")
                    [ { X = 10; Y = 49 }, { X = 10; Y = 0 } ]
                    "the south border is the north border swapped"

                Expect.equal
                    (seams atlas "W13S28" "W12S28")
                    [ { X = 49; Y = 30 }, { X = 0; Y = 30 } ]
                    "the east border is the west border swapped"
            }

            test "rooms that share no border share no band" {
                // Diagonal neighbours touch at a corner the engine joins
                // nothing across, and two rooms apart touch not at all. Both
                // answer empty rather than failing: an unpriceable Seam is
                // no Seam, never a blocked one (ADR 0004).
                let ring room y =
                    room, [ { X = 10; Y = y }, Plain; { X = 0; Y = 30 }, Plain ]

                let atlas =
                    bordered [ ring "W12S28" 0; ring "W13S27" 49; ring "W12S26" 49 ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.isEmpty (seams atlas "W12S28" "W13S27") "a diagonal pair joins nowhere"
                Expect.isEmpty (seams atlas "W12S28" "W12S26") "two rooms apart share no border"
                Expect.isEmpty (seams atlas "W12S28" "W12S28") "and a room borders no self"
            }

            test "a corner tile is on two borders at once, so it is a Seam on neither" {
                // (0,0) is the north row and the west column both. Offered
                // as a crossing it would hand the same tile two different
                // landings — (0,49) north and (49,0) west — and the engine
                // makes at most one of them, so pricing a route through it
                // would put the creep in the wrong room. Every room the
                // engine generates walls its four corners (all four
                // captures do), so no band on real terrain loses a tile:
                // what is pinned is that a passable corner invents none.
                let atlas =
                    bordered
                        [
                            "W12S28",
                            [
                                { X = 0; Y = 0 }, Plain
                                { X = 1; Y = 0 }, Plain
                                { X = 0; Y = 1 }, Plain
                            ]
                            "W12S27", [ { X = 0; Y = 49 }, Plain; { X = 1; Y = 49 }, Plain ]
                            "W13S28", [ { X = 49; Y = 0 }, Plain; { X = 49; Y = 1 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seams atlas "W12S28" "W12S27")
                    [ { X = 1; Y = 0 }, { X = 1; Y = 49 } ]
                    "the north band keeps the row and drops the corner"

                Expect.equal
                    (seams atlas "W12S28" "W13S28")
                    [ { X = 0; Y = 1 }, { X = 49; Y = 1 } ]
                    "and the west band, which would otherwise claim the same tile"
            }

            test "a room the projection has no border for answers the empty band" {
                // The outpost the colony cannot see, entry by entry (ADR
                // 0004) — and a name the engine's grammar does not spell is
                // the same absence, not an error. The ungrammatical name
                // carries a ring of its own here, so the band it answers is
                // empty for the one reason under test: the name places no
                // room. Without the ring the missing layer would empty it
                // first and the assertion would hold however the grammar
                // was read.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 10; Y = 0 }, Plain ]
                            "the outpost", [ { X = 10; Y = 49 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.isEmpty
                    (seams atlas "W12S28" "W12S27")
                    "the neighbour is unprojected, so there is nothing to join to"

                Expect.isEmpty
                    (seams atlas "W12S28" "the outpost")
                    "and a name outside the grammar places no room at all"
            }

            test "an exit tile is in nothing the projection offers to stand or build on" {
                // The prohibition ADR 0041 keeps by not admitting the border
                // rows as ground: a source in the room's corner has its
                // exits passable and in the Seam band, and not one of them
                // is a Seat, a Work Area tile, a buildable tile, a walkable
                // tile or a passable entry in the flood's weight table. The
                // engine moves a creep that ends its tick on an exit into
                // the next room, so a Matcher that could pick one would lose
                // the creep out from under its Task.
                let corner =
                    { spatial
                          [ "src-a", { X = 1; Y = 1 } ]
                          [
                              { X = 1; Y = 2 }, Plain
                              { X = 2; Y = 1 }, Plain
                              { X = 2; Y = 2 }, Plain
                          ] with
                        Borders =
                            Map.ofList
                                [
                                    "W12S28",
                                    Map.ofList
                                        [
                                            for x in 0..2 do
                                                { X = x; Y = 0 }, Plain

                                            for y in 0..2 do
                                                { X = 0; Y = y }, Plain
                                        ]
                                    "W12S27",
                                    Map.ofList [ for x in 0..2 -> { X = x; Y = 49 }, Plain ]
                                ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.hasLength
                    (seams corner "W12S28" "W12S27")
                    2
                    "the exits are real Seam tiles, not merely absent ones — x 1 and 2, the corner never"

                let onTheBorder (tile: Pos) =
                    tile.X = 0 || tile.X = 49 || tile.Y = 0 || tile.Y = 49

                Expect.equal
                    (seatTilesOf corner "src-a")
                    (Set.ofList [ { X = 1; Y = 2 }; { X = 2; Y = 1 }; { X = 2; Y = 2 } ])
                    "the corner source seats three interior tiles and no exit"

                Expect.isEmpty
                    (workArea corner (Harvest "src-a") |> Set.filter onTheBorder)
                    "no exit in a Work Area"

                Expect.isEmpty
                    (buildableTiles corner |> List.filter onTheBorder)
                    "no exit is buildable"

                Expect.isEmpty
                    (walkableTiles corner |> Set.filter onTheBorder)
                    "no exit is walkable"

                // The projection names no room, so its ground is filed
                // under the empty name — the room the census signature
                // already spells that way (ADR 0041).
                let weights = stepWeights corner ""

                Expect.isTrue
                    (List.forall
                        (fun (tile: Pos) -> weights.[tile.X * 50 + tile.Y] < 0)
                        [
                            for x in 0..49 do
                                for y in 0..49 do
                                    if onTheBorder { X = x; Y = y } then
                                        { X = x; Y = y }
                        ])
                    "and the flood's weight table marks every exit impassable"
            }
        ]

/// A projection carrying two rooms: the colony's own, filed by `withHome`
/// under the name the projection gives it, and the outpost added beside it
/// under its own (ADR 0041). It adds an entry and never replaces the map,
/// so the home room's geometry survives an outpost joining after it — the
/// two rooms arrive exactly as the shell's projection and an outpost's
/// will.
let private withOutpost (room: string) (layer: RoomLayer) (spatial: SpatialInfo) =
    { spatial with
        Rooms = Map.add room layer spatial.Rooms
    }

/// A straight line of Plain ground, for geometry that has to differ
/// between two rooms in a way a reader can count.
let private plainLine tiles =
    tiles |> List.map (fun tile -> tile, Plain)

[<Tests>]
let roomTests =
    testList
        "atlas rooms"
        [
            test "two rooms' floods do not meet on one tile" {
                // ADR 0041's reason for a flood table per room while the
                // memo key keeps the three fields ADR 0029 gave it: two
                // rooms hold the same coordinates, so two creeps of one
                // fatigue factor standing on the same tile of different
                // rooms key alike. One table would hand one of them the
                // other room's distances. The two rooms' ground is shaped
                // differently on purpose — a corridor south at home, a
                // corridor west in the outpost — so a flood run over the
                // wrong grid cannot reach the Work Area at all and answers
                // None rather than a number that happens to agree.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source; "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for x in 5..10 -> { X = x; Y = 10 } ])
                        TargetPositions = Map.ofList [ "src-out", { X = 4; Y = 10 } ]
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 10 } ]
                    }

                let atlas =
                    home
                    |> withOutpost "W2N1" outpost
                    |> snapshotWith [ worker "w-home"; worker "w-out" ]
                    |> ofSnapshot

                Expect.equal
                    (creepTile atlas "w-home", creepTile atlas "w-out")
                    (Some { X = 10; Y = 10 }, Some { X = 10; Y = 10 })
                    "the premise: one coordinate, two rooms, one body between them"

                Expect.equal
                    (travelCost atlas "w-home" (Harvest "src-home"))
                    (Some 14)
                    "seven plain steps down its own corridor"

                Expect.equal
                    (travelCost atlas "w-out" (Harvest "src-out"))
                    (Some 10)
                    "five plain steps down the other room's, priced off the other room's ground"

                Expect.equal
                    (firstStep atlas "w-home" (workArea atlas (Harvest "src-home")))
                    (Some { X = 10; Y = 11 })
                    "and the route each one walks is its own room's, predecessors and all"

                Expect.equal
                    (firstStep atlas "w-out" (workArea atlas (Harvest "src-out")))
                    (Some { X = 9; Y = 10 })
                    "the other way entirely, out of the same coordinate"
            }

            test "a Task in the neighbouring room is inapplicable, not mispriced" {
                // Every flood stops at its room's border (ADR 0041), so a
                // creep here and a target there have no priced path between
                // them — and the honest answer is the one an unreachable
                // Work Area in the creep's own room gets: the Task does not
                // apply to this creep. What must never happen is a number,
                // which is what reading the neighbour's tiles out of this
                // room's flood would produce. Since #123 a border can be
                // crossed for a price, but only where there is a Seam to
                // cross at: this projection carries no border ring at all,
                // so the band is empty, the minimum is over nothing, and
                // the answers below are the ones they always were.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                        TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 18 } ]
                    }

                let atlas =
                    home
                    |> withOutpost "W2N1" outpost
                    |> snapshotWith [ worker "w-home" ]
                    |> ofSnapshot

                Expect.isNonEmpty
                    (workArea atlas (Harvest "src-out"))
                    "the target is placed, and its Work Area is the room it stands in"

                Expect.equal
                    (travelCost atlas "w-home" (Harvest "src-out"))
                    None
                    "no ranking price across a border"

                Expect.equal
                    (walkTicks atlas "w-home" (Harvest "src-out"))
                    None
                    "and no clock either — the walk and the price agree (ADR 0030)"

                Expect.isFalse
                    (mayActFor atlas "w-home" (Harvest "src-out"))
                    "and no action reaches across one: the engine's ranges are room-local"

                // The seam `decide` actually prices through. `travelCost`
                // and `walkTicks` are the Task-shaped wrappers; the
                // Matcher, the Emitter and the mover reach the flood with
                // a bare tile set, taken from `workAreaFor`. Were that set
                // the neighbour's ground, this room's flood would answer
                // it a number and a first step off *home* terrain — a
                // creep priced on ground it is not standing on and walked
                // seven tiles inside its own room. So the creep-aware Work
                // Area is empty across a border while the body-blind one
                // above is not, and #123 left it that way: the price
                // crosses the border, the standing tiles do not.
                let area = workAreaFor atlas "w-home" (Harvest "src-out")

                Expect.isEmpty
                    area
                    "the creep has nowhere it may stand: the Work Area it is handed is the empty one"

                Expect.equal
                    (travelCostWithin atlas "w-home" area)
                    None
                    "so the tile-shaped price refuses too, not only the Task-shaped one"

                Expect.equal
                    (firstStep atlas "w-home" area)
                    None
                    "and the mover is given no step toward it"

                Expect.equal
                    (firstStepIgnoringTraffic atlas "w-home" area)
                    None
                    "nor the traffic-blind route the reroute attribution compares against"
            }

            test "one room's traffic never surcharges another room's flood" {
                // The occupancy half of the per-room split (ADR 0008's
                // surcharge inside ADR 0041's layering). Both rooms hold
                // the same corridor, and the outpost parks a creep partway
                // down the coordinate the home creep must cross. One
                // shared occupancy grid would price that step ten dearer —
                // a one-wide corridor has no detour — and reprice a home
                // creep off a creep it can never meet.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 13 } ]
                    }

                let atlas =
                    home
                    |> withOutpost "W2N1" outpost
                    |> snapshotWith [ worker "w-home"; worker "w-out" ]
                    |> ofSnapshot

                Expect.equal
                    (creepTile atlas "w-out")
                    (Some { X = 10; Y = 13 })
                    "the premise: the other room's creep stands on a coordinate this path crosses"

                Expect.equal
                    (travelCost atlas "w-home" (Harvest "src-home"))
                    (Some 14)
                    "seven plain steps, and not one of them surcharged"
            }

            test "a target in a room the projection does not carry is absent, entry by entry" {
                // ADR 0004 read a room at a time: a room with no layer and a
                // room whose every container is empty are one answer, and it
                // is the answer an unplaced target has always had — not
                // priceable, counted against no Task, blocking no action.
                // Both shapes are asserted because the layer admits both and
                // nothing may tell them apart.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-far", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                for label, spatial in
                    [
                        "a room with no layer at all", home
                        "a room named and empty", home |> withOutpost "W3N1" RoomLayer.empty
                    ] do
                    let atlas = spatial |> snapshotWith [ worker "w-home" ] |> ofSnapshot

                    Expect.equal (positionOf atlas "src-far") None $"{label}: nowhere to place it"
                    Expect.equal (seats atlas "src-far") None $"{label}: no Seats to derive"

                    Expect.isEmpty
                        (workArea atlas (Harvest "src-far"))
                        $"{label}: and no ground to work it from"

                    Expect.equal
                        (travelCost atlas "w-home" (Harvest "src-far"))
                        (Some 0)
                        $"{label}: unpriceable geometry never counts against a Task"

                    Expect.isTrue
                        (mayActFor atlas "w-home" (Harvest "src-far"))
                        $"{label}: and never blocks an action"
            }

            test "one coordinate standing in two rooms is no Post and no Dual Seat" {
                // The bleed a `Set<Pos>` invites, refused where the sets are
                // built (ADR 0041): the outpost's controller puts (10,10)
                // inside an Upgrade area and its container stands on that
                // tile, while at home (10,10) is one of a source's Seats.
                // Unioned across rooms that coordinate would read as a Dual
                // Seat and as a container Post — a Post nothing stands on,
                // an Anchor place nothing can fill, and a source reading as
                // posted with no container of its own.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-home", Source
                                    "ctrl-out", Controller
                                    "cont-out", Structure BuiltKind.Container
                                ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList
                                    [
                                        for x in 9..11 do
                                            for y in 9..11 do
                                                { X = x; Y = y }, Plain
                                    ]
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 9 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList
                                [
                                    for x in 9..11 do
                                        for y in 9..12 do
                                            { X = x; Y = y }, Plain
                                ]
                        TargetPositions =
                            Map.ofList
                                [ "ctrl-out", { X = 10; Y = 12 }; "cont-out", { X = 10; Y = 10 } ]
                    }

                let atlas =
                    home
                    |> withOutpost "W2N1" outpost
                    |> snapshotWith [ worker "w-home" ]
                    |> ofSnapshot

                Expect.isTrue
                    (Set.contains { X = 10; Y = 10 } (seatTilesOf atlas "src-home"))
                    "the premise: the home source seats that coordinate"

                Expect.isTrue
                    (Set.contains { X = 10; Y = 10 } (workArea atlas (Upgrade "ctrl-out")))
                    "and the outpost controller's Upgrade area holds it too"

                Expect.isEmpty (dualSeats atlas) "no Dual Seat is made out of two rooms"
                Expect.isEmpty (posts atlas) "and no Post"
                Expect.isEmpty (postsOf atlas "src-home") "the home source has none of its own"

                Expect.isFalse
                    (catchesOverflow atlas "w-home" "src-home")
                    "and the other room's container catches nothing this creep digs"
            }

            test "placedCreeps answers the room the mover moves in" {
                // ADR 0041's Consequences: arbitrated movement (ADR 0001,
                // ADR 0008) and the occupancy surcharge stay single-room,
                // unchanged. The Resolver unions these tiles into a
                // `Set<Pos>` and a `Map<Pos, string>`, the pickup reflex
                // measures range against home piles, and the lead prices
                // the tile off the home room's flood — all three of which
                // read a second room's creep as a creep of this one when
                // the coordinates agree. The floods still get every room's
                // creep; this query is what the mover sees.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 11 } ]
                    }

                let atlas =
                    home
                    |> withOutpost "W2N1" outpost
                    |> snapshotWith [ worker "w-home"; worker "w-out" ]
                    |> ofSnapshot

                Expect.equal
                    (placedCreeps atlas)
                    [ "w-home", { X = 10; Y = 10 } ]
                    "the home room's creeps and no other"

                Expect.equal
                    (creepTile atlas "w-out")
                    (Some { X = 10; Y = 11 })
                    "the other room's creep is still placed — it is the bare list that is home's"
            }
        ]

/// A projection carrying the colony's own room and one outpost across its
/// north border, rings and all. W1N1 is world (-2,-2) and W1N2 (-2,-3), so
/// stepping onto y=0 at home lands the creep on y=49 there — the pairing
/// `seams` answers and the join `pricedAcross` sums over. The rings ride
/// beside the ground and never inside it (ADR 0041), which is what makes a
/// crossing priceable without any exit tile becoming a tile to stand on.
let private northOf (home: RoomLayer) homeRing (outpost: RoomLayer) outpostRing kinds creeps =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Borders = Map.ofList [ "W1N1", Map.ofList homeRing; "W1N2", Map.ofList outpostRing ]
        TargetKinds = Map.ofList kinds
    }
    |> withHome (fun _ -> home)
    |> withOutpost "W1N2" outpost
    |> snapshotWith creeps
    |> ofSnapshot

/// The colony's own room as every cross-room case below shapes it: one
/// plain corridor down column 25 to the exit row, with the creeps standing
/// in it.
let private corridorHome creeps =
    { RoomLayer.empty with
        Terrain = Map.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
        CreepPositions = Map.ofList creeps
    }

/// The outpost as the worked example shapes it: the same corridor, with a
/// source standing on ground the projection does not carry, so its Work
/// Area is the one tile below it and the arithmetic has one route to count.
let private corridorOutpost =
    { RoomLayer.empty with
        Terrain =
            Map.ofList (
                plainLine
                    [
                        for y in 1..48 do
                            if y <> 40 then
                                { X = 25; Y = y }
                    ]
            )
        TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
    }

[<Tests>]
let crossRoomTests =
    testList
        "atlas cross-room walk"
        [
            test "a walk across the border is the near leg, the exit's own price and the far leg" {
                // The worked example, countable a tile at a time. The creep
                // stands at (25,10) of a one-wide plain corridor: nine steps
                // up to (25,1), one onto the exit at (25,0), then the engine
                // moves it to (25,49) of the outpost for nothing at the end
                // of that tick, one step off the landing onto (25,48), and
                // seven more down to (25,41) — the Work Area of a source at
                // (25,40) whose own tile the projection carries no ground
                // for. Eighteen tiles stepped onto, each one tick for a body
                // at fatigue parity: the crossing charges the exit tile and
                // the far room's first tile, and never the landing tile,
                // which the creep arrives on without moving.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 }; "w-back", { X = 25; Y = 14 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w"; worker "w-back" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "nine near, the exit, and eight in the outpost"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 36)
                    "and the same join in the ranking price's own units — two a plain step"

                // The far leg is the target's, not the creep's: a second
                // creep four tiles further back pays four more and not a
                // tile besides, which is what one flood out of the target
                // serving the whole colony looks like from outside.
                Expect.equal
                    (walkTicks atlas "w-back" (Harvest "src-out"))
                    (Some 22)
                    "four tiles further back is four ticks dearer, the far leg unchanged"
            }

            test "a swamp exit is priced as a swamp step, not counted as one tile" {
                // ADR 0041 writes the join as `walk_here + 1 + walk_there`,
                // and #123 narrows that `+1` to the price ADR 0029 gives the
                // exit tile itself: `max(1, ceil(units / 2))`, the same rule
                // every other step is priced by. On plain ground under a
                // body at fatigue parity the two agree, which is why this is
                // a narrowing and not an overturning; on a swamp exit they
                // do not, and the engine charges the swamp.
                let across ring =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, ring ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks (across Plain) "w" (Harvest "src-out"))
                    (Some 18)
                    "a plain exit costs the one tick the ADR's +1 spells"

                Expect.equal
                    (walkTicks (across Swamp) "w" (Harvest "src-out"))
                    (Some 22)
                    "a swamp exit costs five, and the same walk is four ticks dearer"

                Expect.equal
                    (travelCost (across Swamp) "w" (Harvest "src-out"))
                    (Some 44)
                    "the ranking price charges the swamp exit its own ten units"
            }

            test "the walk takes the cheapest crossing in the band, not the nearest" {
                // Two exits, and the near one is the wrong one: the creep
                // reaches (25,0) in nine steps and (27,0) in ten, but the
                // outpost's column below (25,49) is swamp all the way down
                // while the one below (27,49) is plain. The minimum is over
                // the whole band — 10 + 1 + 8 against 9 + 1 + 36 — which is
                // the arithmetic ADR 0041 pays a Seam band for.
                let home =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
                                plainLine
                                    [
                                        for y in 1..10 -> { X = 25; Y = y }
                                        for x in 26..27 -> { X = x; Y = 10 }
                                        for y in 1..9 -> { X = 27; Y = y }
                                    ]
                            )
                        CreepPositions = Map.ofList [ "w", { X = 25; Y = 10 } ]
                    }

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList
                                [
                                    for x in 25..27 -> { X = x; Y = 41 }, Plain
                                    for y in 42..48 -> { X = 25; Y = y }, Swamp
                                    for y in 42..48 -> { X = 27; Y = y }, Plain
                                ]
                        TargetPositions = Map.ofList [ "src-out", { X = 26; Y = 40 } ]
                    }

                let across farRing =
                    northOf
                        home
                        [
                            { X = 25; Y = 0 }, Plain
                            { X = 26; Y = 0 }, Wall
                            { X = 27; Y = 0 }, Plain
                        ]
                        outpost
                        farRing
                        [ "src-out", Source ]
                        [ worker "w" ]

                let bothOpen =
                    across
                        [
                            { X = 25; Y = 49 }, Plain
                            { X = 26; Y = 49 }, Wall
                            { X = 27; Y = 49 }, Plain
                        ]

                Expect.hasLength (seams bothOpen "W1N1" "W1N2") 2 "the premise: two crossings"

                Expect.equal
                    (walkTicks bothOpen "w" (Harvest "src-out"))
                    (Some 19)
                    "the farther exit, because the ground behind it is cheaper"

                Expect.equal
                    (travelCost bothOpen "w" (Harvest "src-out"))
                    (Some 38)
                    "and the ranking price joins at that same crossing, in its own units"

                // Take the cheap crossing out and the walk does not vanish:
                // it falls back to the dear one, which is the band being a
                // minimum rather than a choice made once.
                let swampOnly =
                    across
                        [
                            { X = 25; Y = 49 }, Plain
                            { X = 26; Y = 49 }, Wall
                            { X = 27; Y = 49 }, Wall
                        ]

                Expect.equal
                    (walkTicks swampOnly "w" (Harvest "src-out"))
                    (Some 46)
                    "nine near, the exit, and thirty-six down the swamp column"
            }

            test "a border with no crossing has no price, and the target is still placed" {
                // A walled ring: the band is empty, so the minimum is over
                // nothing. That is the answer an unreachable Work Area in
                // the creep's own room gets — the Task is inapplicable to
                // this creep — and not the zero an unplaced target gets,
                // which would count it as free. The same projection with
                // that one tile of ring opened closes the case at the
                // bottom: the None is the wall's answer and not something
                // the two rooms would have said anyway.
                let across ring =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, ring ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let atlas = across Wall

                Expect.isEmpty (seams atlas "W1N1" "W1N2") "the premise: the exit is walled"

                Expect.isNonEmpty
                    (workArea atlas (Harvest "src-out"))
                    "the target is placed and its ground is real"

                Expect.equal (walkTicks atlas "w" (Harvest "src-out")) None "no crossing, no walk"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    None
                    "and no ranking price either — one join answers both"

                Expect.equal
                    (walkTicks (across Plain) "w" (Harvest "src-out"))
                    (Some 18)
                    "and it is the wall that answers None: open that tile and the same rooms price"
            }

            test "a target in a room the projection does not carry has no walk to price" {
                // ADR 0004's totality, at the seam #123 widens: a room that
                // is not in the projection at all leaves its targets
                // unplaced, and unplaceable geometry prices at zero, counts
                // against no Task and blocks no action. The walk answers it
                // the way travel cost always has.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source; "src-far", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-far"))
                    (Some 0)
                    "a target no room places is free, never unreachable"

                Expect.isTrue
                    (mayActFor atlas "w" (Harvest "src-far"))
                    "and it blocks nothing (ADR 0004)"
            }

            test "the price crosses the border and the standing tiles do not" {
                // ADR 0041's Consequences drawn on one Snapshot: geometry
                // crosses, arbitration does not. The creep has an honest
                // number for a Task in the outpost — that is what puts the
                // outpost's Harvest in the same pool as home's — and no tile
                // of the outpost is ever handed to it as somewhere to stand,
                // step to or act from, because a `Set<Pos>` carries no room
                // and the mover reads it as this room's.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "the premise: the Task is priceable across the border"

                let handed = workAreaFor atlas "w" (Harvest "src-out")

                Expect.isEmpty handed "and the creep is handed no tile of the other room"

                Expect.isFalse
                    (mayAct atlas "w" (Harvest "src-out") handed)
                    "it may not act on a target a room away"

                Expect.equal
                    (firstStep atlas "w" handed)
                    None
                    "and the mover is given no step toward one (ADR 0001, ADR 0008)"
            }

            test "a heavy body's far leg is its Post's, not the source's nearest Seat" {
                // The far leg floods out of the Work Area *for this body*
                // (ADR 0020), so the narrowing has to cross the border with
                // the price: a Work-heavy creep is walked to the Seat under
                // the outpost's container even when a nearer Seat is on the
                // way. The source at (25,40) seats (25,41), one step off the
                // corridor, and (24,39), reachable only the long way round
                // through column 23 — eighteen tiles to the near Seat and
                // twenty to the Post. Three Work over two Move is heavy by
                // ADR 0016's predicate; every plain step costs it two ticks,
                // so the light body's numbers are half of its own.
                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
                                plainLine
                                    [
                                        for y in 41..48 -> { X = 25; Y = y }
                                        for y in 39..48 -> { X = 23; Y = y }
                                        for x in 23..25 -> { X = x; Y = 48 }
                                        yield { X = 24; Y = 39 }
                                    ]
                            )
                        TargetPositions =
                            Map.ofList
                                [ "src-out", { X = 25; Y = 40 }; "cont-out", { X = 24; Y = 39 } ]
                    }

                let across kinds =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 }; "heavy", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        outpost
                        [ { X = 25; Y = 49 }, Plain ]
                        kinds
                        [ worker "w"; creepWith "heavy" 0 [ Work; Work; Work; Move; Move ] ]

                let bare = across [ "src-out", Source ]

                let posted = across [ "src-out", Source; "cont-out", Structure BuiltKind.Container ]

                Expect.isEmpty (postsOf bare "src-out") "the premise: no container, no Post"

                Expect.equal
                    (postsOf posted "src-out")
                    (Set.ofList [ { X = 24; Y = 39 } ])
                    "and with one standing, the far Seat is the source's Post"

                Expect.equal
                    (walkTicks bare "heavy" (Harvest "src-out"))
                    (Some 36)
                    "unposted, the heavy body walks to the nearest Seat: eighteen tiles at two ticks"

                Expect.equal
                    (walkTicks posted "heavy" (Harvest "src-out"))
                    (Some 40)
                    "posted, it walks the long way to the Post — two tiles further, four ticks"

                Expect.equal
                    (walkTicks posted "w" (Harvest "src-out"))
                    (Some 18)
                    "and the light body ignores the Post, over the same border on the same tick"
            }
        ]
