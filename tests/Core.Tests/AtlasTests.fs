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
        // Who holds a room prices its sources (ADR 0042) and the Atlas
        // prices nothing: geometry is all it reads.
        RoomControl = Map.empty
        ConstructionSites = []
        Creeps = creeps
        Hostiles = []
        // A threat prices nothing and blocks nothing either: the Atlas
        // reads geometry, and an invader core reaches it as an ordinary
        // structure's obstacle or not at all (ADR 0043).
        InvaderCores = []
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

/// `firstStep` over a Task's own Work Area, the same tick the mover sees:
/// the Task rides beside the tiles since #142, because a target in the
/// neighbouring room leaves the creep-aware area empty and the step is then
/// the Seam's near side.
let firstStepFor atlas creep task =
    firstStep atlas creep task (workAreaFor atlas creep task)

/// The traffic-blind route over the same area — the half the reroute
/// attribution compares against (ADR 0008, ADR 0018).
let firstStepBlindFor atlas creep task =
    firstStepIgnoringTraffic atlas creep task (workAreaFor atlas creep task)

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

            test "a source on the room's edge Seats only tiles of the grid" {
                // The Seat query reads the room's terrain grid a tile at a
                // time (#173), and an index off the grid is no index at
                // all: under Fable an unchecked read of one answers
                // `undefined`, which the weight test would call walkable
                // ground the engine has never heard of, while .NET throws.
                // Both corners and a mid-edge tile, because `neighbours`
                // produces a -1 at one end and a 50 at the other. The ring
                // itself is not ground (ADR 0036), so a source standing on
                // it Seats none of its own row — the fourth case, and the
                // one a real capture actually holds.
                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList
                                    [
                                        for x in 1..48 do
                                            for y in 1..48 -> { X = x; Y = y }, Plain
                                    ]
                            TargetPositions =
                                Map.ofList
                                    [
                                        "src-low", { X = 0; Y = 0 }
                                        "src-high", { X = 49; Y = 49 }
                                        "src-side", { X = 0; Y = 25 }
                                        "src-in", { X = 1; Y = 1 }
                                    ]
                        })
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (seatTilesOf atlas "src-low")
                    (Set.singleton { X = 1; Y = 1 })
                    "the low corner Seats its one ground neighbour, and no negative coordinate"

                Expect.equal
                    (seatTilesOf atlas "src-high")
                    (Set.singleton { X = 48; Y = 48 })
                    "the high corner Seats its one ground neighbour, and nothing at 50"

                Expect.equal
                    (seatTilesOf atlas "src-side")
                    (Set.ofList [ { X = 1; Y = 24 }; { X = 1; Y = 25 }; { X = 1; Y = 26 } ])
                    "a mid-edge tile Seats the three ground tiles inside it"

                Expect.equal
                    (seatTilesOf atlas "src-in")
                    (Set.ofList [ { X = 1; Y = 2 }; { X = 2; Y = 1 }; { X = 2; Y = 2 } ])
                    "and a source one tile in Seats no exit tile: the ring is not ground"
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

            test "standing tiles at the room's edge stop at the grid" {
                // `adjacentWalkableIn` reads the room's weight grid a tile
                // at a time (#173) over the eight `neighbours` produces,
                // which at an edge are a -1 or a 50 away from being an
                // index at all — unchecked under Fable, where the read
                // answers `undefined` and would price as walkable, and a
                // throw on .NET. Both corners and a mid-edge tile, and the
                // answers are the room's own ground: an exit tile is not
                // ground (ADR 0036) and is no tile to stand on.
                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList
                                    [
                                        for x in 1..48 do
                                            for y in 1..48 -> { X = x; Y = y }, Plain
                                    ]
                        })
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (adjacentWalkableIn atlas "W1N1" { X = 0; Y = 0 })
                    [ { X = 1; Y = 1 } ]
                    "the low corner stands on its one ground neighbour, and no negative coordinate"

                Expect.equal
                    (adjacentWalkableIn atlas "W1N1" { X = 49; Y = 49 })
                    [ { X = 48; Y = 48 } ]
                    "the high corner stands on its one ground neighbour, and nothing at 50"

                Expect.equal
                    (adjacentWalkableIn atlas "W1N1" { X = 0; Y = 25 })
                    [ { X = 1; Y = 24 }; { X = 1; Y = 25 }; { X = 1; Y = 26 } ]
                    "a mid-edge tile stands on the three ground tiles inside it, in (X, Y) order"
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

            test
                "droppedEnergyIn lists a room's placed piles in id order; buildableTiles ignores them" {
                // A pile is a target the reflex reads, not a thing standing
                // on the tile: it never keeps a construction site off it.
                /// The room this funnel files its geometry under: the
                /// projection names none, so it is filed under the empty
                /// name (`SpatialInfo.homeName`).
                let home = SpatialInfo.homeName SpatialInfo.empty

                let atlas =
                    { spatial
                          [ "pile-b", { X = 10; Y = 11 }; "pile-a", { X = 10; Y = 10 } ]
                          [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds = Map.ofList [ "pile-a", Dropped; "pile-b", Dropped ]
                    }
                    |> snapshotWith []
                    |> ofSnapshot

                Expect.equal
                    (droppedEnergyIn atlas home)
                    [ "pile-a", { X = 10; Y = 10 }; "pile-b", { X = 10; Y = 11 } ]
                    "both piles placed, id order"

                Expect.isEmpty
                    (droppedEnergyIn atlas "W9N9")
                    "a room the projection does not carry places no pile (ADR 0004)"

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

/// Two lanes to one Seat, so a route can be read off the weights alone.
/// The source at (9,9) sits in wall with (10,10) its only Seat; the creep
/// "w" stands at (10,12). One swamp tile at (10,11) joins the two in two
/// steps; a five-tile paved ring — (11,13), (12,12), (12,11), (12,10),
/// (11,9) — joins them in six. Nothing else is projected, so no diagonal
/// links the lanes and the only choice is which one to take.
let forkedLanes creeps =
    let ring =
        [
            { X = 11; Y = 13 }
            { X = 12; Y = 12 }
            { X = 12; Y = 11 }
            { X = 12; Y = 10 }
            { X = 11; Y = 9 }
        ]

    spatial
        [ "src-a", { X = 9; Y = 9 } ]
        ([
            { X = 9; Y = 9 }, Wall
            { X = 10; Y = 10 }, Plain
            { X = 10; Y = 11 }, Swamp
            { X = 10; Y = 12 }, Plain
         ]
         @ [ for tile in ring -> tile, Plain ])
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList ring
            CreepPositions = Map.ofList creeps
        })

[<Tests>]
let stepPriceTableTests =
    testList
        "atlas step price table"
        [
            test "travel cost prices every weight the ground carries as the body's fatigue" {
                // The flood reads a step's price off a table laid once per
                // flood rather than by asking per relaxation (#168), so the
                // table's whole domain is pinned here against the fatigue
                // arithmetic it stands for: ceil(weight × fatigue parts /
                // Move parts), never below one unit (ADR 0010, ADR 0029).
                // Three weights — road 1, plain 2, swamp 10 — against four
                // bodies, read off one step onto the Seat.
                let unitsOn terrain roads body =
                    let atlas =
                        seatPriced terrain roads
                        |> snapshotWith [ creepWith "w" 0 body ]
                        |> ofSnapshot

                    travelCost atlas "w" (Harvest "src-a")

                let seat = Set.singleton { X = 10; Y = 11 }

                let priceOn body =
                    unitsOn Plain seat body,
                    unitsOn Plain Set.empty body,
                    unitsOn Swamp Set.empty body

                Expect.equal
                    (priceOn [ Work; Carry; Move ])
                    (Some 1, Some 2, Some 10)
                    "one fatigue part on one Move pays the weight itself: 1, 2, 10"

                Expect.equal
                    (priceOn [ Work; Work; Work; Work; Work; Carry; Move ])
                    (Some 5, Some 10, Some 50)
                    "five fatigue parts on one Move pay five times it: 5, 10, 50"

                Expect.equal
                    (priceOn [ Work; Move; Move; Move ])
                    (Some 1, Some 1, Some 4)
                    "three Moves under one part: ceil(1/3) and ceil(2/3) hit the one-unit floor, ceil(10/3) = 4"

                Expect.equal
                    (priceOn [ Work; Work; Move; Move ])
                    (Some 1, Some 2, Some 10)
                    "fatigue parity pays the weight again: the ratio is what prices a step, not the part count"
            }

            test "the walk prices the same weights in whole ticks" {
                // The `Walk` row of the same table: two units make a tick,
                // a part of one still costs a whole tick, and no step
                // crosses a tile in less than one (ADR 0029). Same three
                // weights, same four bodies, so the two rows are pinned
                // over one domain and can be read side by side.
                let ticksOn terrain roads body =
                    let atlas =
                        seatPriced terrain roads
                        |> snapshotWith [ creepWith "w" 0 body ]
                        |> ofSnapshot

                    walkTicks atlas "w" (Harvest "src-a")

                let seat = Set.singleton { X = 10; Y = 11 }

                let ticksFor body =
                    ticksOn Plain seat body,
                    ticksOn Plain Set.empty body,
                    ticksOn Swamp Set.empty body

                Expect.equal
                    (ticksFor [ Work; Carry; Move ])
                    (Some 1, Some 1, Some 5)
                    "ceil(1/2) and ceil(2/2) are one tick, ceil(10/2) is five"

                Expect.equal
                    (ticksFor [ Work; Work; Work; Work; Work; Carry; Move ])
                    (Some 3, Some 5, Some 25)
                    "ceil(5/2) = 3, ceil(10/2) = 5, ceil(50/2) = 25"

                Expect.equal
                    (ticksFor [ Work; Move; Move; Move ])
                    (Some 1, Some 1, Some 2)
                    "a Move surplus buys the walk nothing below a tick: 1, 1, ceil(4/2) = 2"

                Expect.equal
                    (ticksFor [ Work; Work; Move; Move ])
                    (Some 1, Some 1, Some 5)
                    "fatigue parity walks the worker unit's ticks"
            }

            test "the traffic-blind route prices the same weights, and a Move surplus moves it" {
                // The `Baseline` row (ADR 0030), whose only reader is the
                // reroute attribution's route, so it is read as a choice
                // rather than as a number. Two lanes to one Seat: two steps
                // over swamp, or six over road. The worker unit pays
                // 10 + 2 = 12 for the swamp lane and 5 × 1 + 2 = 7 for the
                // paved one, and takes the long way round; three Moves
                // under one part floor every road step at one unit, so the
                // paved lane costs 5 × 1 + 1 = 6 against the swamp lane's
                // ceil(10/3) + 1 = 5, and the same geometry sends that body
                // the short way. The flip is the table's whole weight
                // domain and the one-unit floor in one assertion.
                let blindStepFor body =
                    let atlas =
                        forkedLanes [ "w", { X = 10; Y = 12 } ]
                        |> snapshotWith [ creepWith "w" 0 body ]
                        |> ofSnapshot

                    firstStepBlindFor atlas "w" (Harvest "src-a")

                Expect.equal
                    (blindStepFor [ Work; Carry; Move ])
                    (Some { X = 11; Y = 13 })
                    "the worker unit rounds the paved ring: five road steps beat one swamp step"

                Expect.equal
                    (blindStepFor [ Work; Move; Move; Move ])
                    (Some { X = 10; Y = 11 })
                    "the surplus body cuts across the swamp: its road steps cannot price below one unit"
            }

            test "a body with no Move parts prices no weight at all, under every pricing" {
                // The table's impassable row: `stepUnits` refuses a body
                // the engine's move refuses, and it is written as the same
                // -1 the weight grid marks a wall with, so one test in the
                // flood settles both. Every weight, and all three pricings
                // — travel cost, the walk, and the traffic-blind route.
                let atlasOn terrain roads =
                    seatPriced terrain roads
                    |> snapshotWith [ creepWith "w" 0 [ Work; Carry ] ]
                    |> ofSnapshot

                let seat = Set.singleton { X = 10; Y = 11 }

                let answers =
                    [
                        for terrain, roads in [ Plain, seat; Plain, Set.empty; Swamp, Set.empty ] do
                            let atlas = atlasOn terrain roads
                            yield travelCost atlas "w" (Harvest "src-a") |> Option.isSome
                            yield walkTicks atlas "w" (Harvest "src-a") |> Option.isSome
                            yield firstStepBlindFor atlas "w" (Harvest "src-a") |> Option.isSome
                    ]

                Expect.allEqual answers false "no weight is steppable by a body that cannot move"
            }

            test "the weight grid carries no weight the price table has no slot for" {
                // The table spans 0..`swampWeight`, and the flood reads it
                // unchecked, so it is in range only while swamp stays the
                // dearest ground a grid can hold (ADR 0010). A terrain
                // priced above swamp would index past the end, which under
                // Fable reads as a *free* step where .NET throws — the two
                // halves of one table disagreeing. So the grid's whole
                // weight domain is pinned here, off `stepWeights`: every
                // terrain the projection knows, a road over one and an
                // obstacle over another.
                let weights =
                    spatial
                        []
                        [
                            { X = 1; Y = 1 }, Plain
                            { X = 1; Y = 2 }, Swamp
                            { X = 1; Y = 3 }, Wall
                            { X = 2; Y = 1 }, Plain
                            { X = 2; Y = 2 }, Swamp
                            { X = 2; Y = 3 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Roads = Set.ofList [ { X = 2; Y = 1 }; { X = 2; Y = 2 } ]
                            Obstacles = Set.singleton { X = 2; Y = 3 }
                        })
                    |> snapshotWith []
                    |> ofSnapshot
                    // The projection names no room, so its ground is filed
                    // under the empty name (ADR 0041).
                    |> fun atlas -> stepWeights atlas ""

                let swamp = weights.[1 * 50 + 2]

                Expect.equal
                    (Set.ofArray weights)
                    (Set.ofList [ -1; 1; 2; swamp ])
                    "four weights and no more: impassable -1, road 1, plain 2, swamp"

                Expect.isTrue
                    (weights |> Array.forall (fun weight -> weight <= swamp))
                    "and swamp is the dearest of them — the table's last slot is swamp's own"
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
                    (firstStepFor atlas "w" (Harvest "src-a"))
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
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the parked creep's lane for the free one"
            }

            test "a creep already inside the Work Area has no step to take" {
                let atlas =
                    corridor [ "w", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofSnapshot

                Expect.equal (firstStepFor atlas "w" (Harvest "src-a")) None "already there"
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

                Expect.equal (firstStepFor atlas "w" (Harvest "src-a")) None "no path, no step"
            }

            test "an unplaced creep has no step: no movement without geometry" {
                let atlas = corridor [] |> snapshotWith [ worker "w" ] |> ofSnapshot

                Expect.equal (firstStepFor atlas "w" (Harvest "src-a")) None "nothing derivable"
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
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the priced step leaves the parked creep's lane"

                Expect.equal
                    (firstStepBlindFor atlas "w" (Harvest "src-a"))
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
                    (firstStepBlindFor atlas "w" (Harvest "src-a"))
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    "empty ground: the blind route is the priced one, down the paved lane"

                Expect.equal
                    (firstStepBlindFor atlas "w" (Harvest "src-a"))
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
                    (firstStepFor atlas "a" (Harvest "src-a"))
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
                    let step = firstStep atlas "w" task area

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

            // Both tiles are the colony's own room's, and these fixtures
            // file it under the empty name (`SpatialInfo.homeName`): since
            // #149 the two rooms ride on the API, because a `Pos` names
            // none (ADR 0041). Same room in and out, this is the flood the
            // rule always ran.
            let atHome atlas body from sink =
                haulRoundTripTicks atlas body "" from "" sink

            test "the loaded leg out and the empty leg back sum to whole ticks" {
                // Each leg is a walk (ADR 0029). [Carry;Carry;Move]
                // loaded on plain: two full Carry x weight 2 over one Move
                // is 4 units a step, ceil(4 / 2) = 2 ticks. Empty Carry
                // rides free, so the leg back sits on the one-tick floor.
                // Nine steps out at 2 and nine back at 1 = 27 ticks, with
                // nothing halved on the total.
                Expect.equal
                    (atHome
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
                    (atHome
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
                    (atHome
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
                        atHome
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
                    (atHome gapped [ Carry; Carry; Move ] { X = 10; Y = 10 } { X = 20; Y = 10 })
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

            /// The room every goal below stands in. The goal's room is the
            /// caller's since #153 — a creep is led wherever it stands, and
            /// these cases lead one at home — and `spatial` builds from
            /// `SpatialInfo.empty`, which names no room, so the corridor is
            /// filed under the empty name (`SpatialInfo.homeName`).
            let home = SpatialInfo.homeName SpatialInfo.empty

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
                        home
                        { X = 10; Y = 10 })
                    (Some 36)
                    "a slow body earns a long lead"

                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        [ Carry; Carry; Move ]
                        { X = 20; Y = 10 }
                        home
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
                        home
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
                        home
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
                        home
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
                            home
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
                        home
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
                        home
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

            /// The corridor's own room, which is where this goal stands: the
            /// name `spatial` files an unnamed projection's layer under
            /// (`SpatialInfo.homeName`). The cross-border half of the same
            /// table is `crossRoomLeadTests`.
            let home = SpatialInfo.homeName SpatialInfo.empty
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
                    (castWalkTicks first hauler spawnTile home goal)
                    (Some 9)
                    "the first Atlas floods to price the lead"

                Expect.equal walks.Count 1 "and leaves the flood in the table it was handed"
                let flooded = flood walks

                let second = corridorSnapshot () |> ofSnapshotRecalling walks

                Expect.equal
                    (castWalkTicks second hauler spawnTile home goal)
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
                    (castWalkTicks atlas hauler spawnTile home goal)
                    (Some 9)
                    "an empty table is flooded into, and prices the lead identically"

                Expect.equal fresh.Count 1 "the flood it ran is left in it"

                Expect.equal
                    (castWalkTicks (corridorSnapshot () |> ofSnapshot) hauler spawnTile home goal)
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

/// A projection carrying one room's ground and any number of rooms' border
/// rings — the whole input a walk out to a Seam reads. The ground is
/// W12S28's, because the walk runs inside one room and stops at its
/// border; the far room needs a ring and nothing else, exactly as `seams`
/// needs of it.
let private seamGround ground rings =
    { SpatialInfo.empty with
        RoomName = Some "W12S28"
        Rooms =
            Map.ofList
                [
                    "W12S28",
                    { RoomLayer.empty with
                        Terrain = Map.ofList ground
                    }
                ]
        Borders = rings |> List.map (fun (room, tiles) -> room, Map.ofList tiles) |> Map.ofList
    }
    |> snapshotWith []
    |> ofSnapshot

/// A plain three-tile column running up to the room's north exit, and the
/// exit's plain landing across it — the smallest room that has a walk out
/// to a Seam at all.
let private toNorthExit =
    [ { X = 10; Y = 1 }, Plain; { X = 10; Y = 2 }, Plain; { X = 10; Y = 3 }, Plain ]

let private northExit terrain =
    [
        "W12S28", [ { X = 10; Y = 0 }, terrain ]
        "W12S27", [ { X = 10; Y = 49 }, Plain ]
    ]

[<Tests>]
let seamWalkTests =
    testList
        "atlas seam walk"
        [
            test "the walk is the ground to a tile beside the exit, plus the step onto it" {
                // The near half of a cross-room price with the far leg left
                // off (ADR 0041, #123), which is what a plan anchored on the
                // Seam is measured with (ADR 0042). Charged the way every
                // walk in the colony is: one tick a plain step, the tile the
                // creep steps onto and never the one it starts on.
                let atlas = seamGround toNorthExit (northExit Plain)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 1 })
                    (Some 1)
                    "from the tile beside the exit, the crossing itself is the whole walk"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 3 })
                    (Some 3)
                    "two tiles further back, two more plain steps and the same crossing"
            }

            test "a swamp exit is not free, which is #123's narrowing of the ADR's +1" {
                // ADR 0041 writes the crossing as `+1`; that is the price of
                // stepping onto a *plain* exit under a body at fatigue
                // parity, and a swamp exit costs five like any other swamp.
                let atlas = seamGround toNorthExit (northExit Swamp)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 1 })
                    (Some 5)
                    "the swamp crossing, and nothing else, from the tile beside it"
            }

            test "the tile asked at is charged nothing, whatever it costs to stand on" {
                // The convention spelled out where it bites: a swamp tile
                // beside a plain exit is one tick from the Seam, not six.
                // Whoever walks *in* to that tile pays for it; the walk out
                // of it does not, and two Seats of one source are therefore
                // compared on the ground between them (ADR 0042's pick).
                let atlas =
                    seamGround
                        [ { X = 10; Y = 1 }, Swamp; { X = 10; Y = 2 }, Plain ]
                        (northExit Plain)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 1 })
                    (Some 1)
                    "the swamp tile's own step belongs to the walk that arrives on it"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 2 })
                    (Some 6)
                    "and the tile behind it does pay for it: five onto the swamp, one onto the exit"
            }

            test "no band, no ground and no path each answer with no walk at all" {
                // Total (ADR 0004), one absence at a time. An unpriceable
                // Seam is no Seam and never a blocked one, so each of these
                // costs nothing and stops nothing.
                let atlas =
                    seamGround (({ X = 30; Y = 30 }, Plain) :: toNorthExit) (northExit Plain)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W15S25" { X = 10; Y = 1 })
                    None
                    "a room four sectors away shares no border, so there is nothing to walk to"

                Expect.equal
                    (seamWalkTicks atlas "W12S27" "W12S28" { X = 10; Y = 48 })
                    None
                    "and a room the projection carries no ground for reaches no exit of its own"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 30; Y = 30 })
                    None
                    "a tile walled off from every crossing is unpriceable, not far away"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 40 })
                    None
                    "and so is a tile the projection carries no ground for"
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
                    (firstStepFor atlas "w-home" (Harvest "src-home"))
                    (Some { X = 10; Y = 11 })
                    "and the route each one walks is its own room's, predecessors and all"

                Expect.equal
                    (firstStepFor atlas "w-out" (Harvest "src-out"))
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
                    (firstStep atlas "w-home" (Harvest "src-out") area)
                    None
                    "and the mover is given no step toward it"

                Expect.equal
                    (firstStepIgnoringTraffic atlas "w-home" (Harvest "src-out") area)
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

            test "two rooms' Posts on one coordinate are two Posts, not one" {
                // The Anchor row's quota crosses the border since ADR 0042
                // — an outpost's Post hires an Anchor exactly as a home
                // Post does — and this is the shape that decides whether
                // it may be counted by unioning the rooms' tiles. It may
                // not: a `Pos` carries no room, so these two garrison
                // tiles are a room apart at one coordinate, and a union
                // would hire one Anchor to stand on both. Counted room by
                // room they are two.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-home", Source
                                    "can-home", Structure BuiltKind.Container
                                    "src-out", Source
                                    "can-out", Structure BuiltKind.Container
                                ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (plainLine [ { X = 10; Y = 10 } ])
                            TargetPositions =
                                Map.ofList
                                    [
                                        "src-home", { X = 10; Y = 9 }
                                        "can-home", { X = 10; Y = 10 }
                                    ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ { X = 10; Y = 10 } ])
                        TargetPositions =
                            Map.ofList
                                [ "src-out", { X = 10; Y = 11 }; "can-out", { X = 10; Y = 10 } ]
                    }

                let atlas = home |> withOutpost "W2N1" outpost |> snapshotWith [] |> ofSnapshot

                Expect.equal
                    (posts atlas)
                    (Set.singleton { X = 10; Y = 10 })
                    "the premise: the home room's own Post is that one tile"

                Expect.equal
                    (postsOf atlas "src-out")
                    (Set.singleton { X = 10; Y = 10 })
                    "and the outpost rock's Post stands on the same coordinate"

                Expect.equal (postCount atlas) 2 "so the Anchor row is two, never the union's one"
            }

            test "an outpost's Dual Seat is no Post: the colony upgrades one controller" {
                // The Dual Seat half of a Post presumes a controller the
                // colony upgrades, and it upgrades its own room's alone —
                // an outpost's controller it reserves (ADR 0042). Taken
                // across the border the intersection would name a tile
                // nobody ever upgrades from, and that tile would be a Post:
                // an Anchor place and an income share for an outpost source
                // with no container standing under it, which is exactly the
                // switch the container is supposed to be.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source; "ctrl-out", Controller ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (plainLine [ { X = 20; Y = 20 } ])
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ { X = 10; Y = 10 }; { X = 10; Y = 11 } ])
                        TargetPositions =
                            Map.ofList
                                [ "src-out", { X = 10; Y = 10 }; "ctrl-out", { X = 10; Y = 12 } ]
                    }

                let atlas = home |> withOutpost "W2N1" outpost |> snapshotWith [] |> ofSnapshot

                Expect.isTrue
                    (Set.contains { X = 10; Y = 11 } (seatTilesOf atlas "src-out"))
                    "the premise: (10,11) is a Seat of the outpost rock"

                Expect.isTrue
                    (Set.contains { X = 10; Y = 11 } (workArea atlas (Upgrade "ctrl-out")))
                    "and the outpost controller's Upgrade area covers it"

                Expect.isEmpty
                    (postsOf atlas "src-out")
                    "yet the rock has no Post: nothing is built on that Seat"

                Expect.equal (postCount atlas) 0 "so the Anchor row hires nobody for it"
            }

            test "droppedEnergyIn answers each room's own piles on one coordinate" {
                // The pickup reflex's geometry since #166: it measures a
                // bare pile `Pos` against a bare creep `Pos`, so the two
                // have to come out of one layer or a pile at home and a
                // creep in the outpost on the same coordinate read as range
                // 0 (ADR 0041). The kind census stays flat and world-unique
                // — both ids are Dropped here — and it is the join to a
                // *named* room's positions that separates them.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "pile-home", Dropped; "pile-out", Dropped ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            TargetPositions = Map.ofList [ "pile-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                        TargetPositions = Map.ofList [ "pile-out", { X = 10; Y = 10 } ]
                    }

                let atlas = home |> withOutpost "W2N1" outpost |> snapshotWith [] |> ofSnapshot

                Expect.equal
                    (droppedEnergyIn atlas "W1N1")
                    [ "pile-home", { X = 10; Y = 10 } ]
                    "the home room's pile and no other, though both share the tile"

                Expect.equal
                    (droppedEnergyIn atlas "W2N1")
                    [ "pile-out", { X = 10; Y = 10 } ]
                    "and the outpost's own, off the layer it is filed in"
            }

            test "placedCreepsByRoom files each room's creeps under its own name, in Snapshot order" {
                // The Resolver's list since #145: arbitration runs once per
                // room, each over that room's creeps and tiles alone (ADR
                // 0041's Consequences), so the grouping is the seam that
                // keeps two rooms' coordinates from ever meeting in one
                // `Map<Pos, string>`. Within a group the order is the
                // Snapshot's, as every per-creep derivation's is; a creep
                // the projection places nowhere is in no group (ADR 0004).
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                            CreepPositions =
                                Map.ofList
                                    [ "b-home", { X = 10; Y = 12 }; "a-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 10 } ]
                    }

                let atlas =
                    home
                    |> withOutpost "W2N1" outpost
                    |> snapshotWith
                        [ worker "b-home"; worker "w-out"; worker "a-home"; worker "ghost" ]
                    |> ofSnapshot

                // The order the rooms come in is no promise — no reader
                // depends on it — so the groups are compared as a map.
                Expect.equal
                    (placedCreepsByRoom atlas |> Map.ofList)
                    (Map.ofList
                        [
                            "W1N1", [ "b-home", { X = 10; Y = 12 }; "a-home", { X = 10; Y = 10 } ]
                            "W2N1", [ "w-out", { X = 10; Y = 10 } ]
                        ])
                    "each room's creeps under its name, Snapshot order inside, the unplaced in none"

                Expect.equal
                    (adjacentWalkableIn atlas "W2N1" { X = 10; Y = 10 })
                    [ { X = 10; Y = 11 } ]
                    "and the standing tiles beside an outpost creep are read off its own room's ground"

                Expect.isEmpty
                    (adjacentWalkableIn atlas "W3N1" { X = 10; Y = 10 })
                    "a room the projection does not carry has no ground beside anything"
            }
        ]

/// A projection carrying the colony's own room and one outpost across its
/// north border, rings and all. W1N1 is world (-2,-2) and W1N2 (-2,-3), so
/// stepping onto y=0 at home lands the creep on y=49 there — the pairing
/// `seams` answers and the join `pricedAcross` sums over. The rings ride
/// beside the ground and never inside it (ADR 0041), which is what makes a
/// crossing priceable without any exit tile becoming a tile to stand on.
///
/// The Snapshot is handed out beside the Atlas because one case below
/// prices a lead over a walk table it supplies itself
/// (`ofSnapshotRecalling`, ADR 0032); every other case wants the Atlas and
/// takes the shorthand.
let private northOfSnapshot
    (home: RoomLayer)
    homeRing
    (outpost: RoomLayer)
    outpostRing
    kinds
    creeps
    =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Borders = Map.ofList [ "W1N1", Map.ofList homeRing; "W1N2", Map.ofList outpostRing ]
        TargetKinds = Map.ofList kinds
    }
    |> withHome (fun _ -> home)
    |> withOutpost "W1N2" outpost
    |> snapshotWith creeps

let private northOf (home: RoomLayer) homeRing (outpost: RoomLayer) outpostRing kinds creeps =
    northOfSnapshot home homeRing outpost outpostRing kinds creeps |> ofSnapshot

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

                // Nor does it move anybody: there is no room to cross to,
                // so there is no Seam to aim at and no near side of one
                // (#142). An unplaced target is free, unblocking and
                // unwalked, all three off the same absence.
                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-far"))
                    None
                    "and the mover is given no step toward a room nobody projected"
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

                // #142's correction, and the one line of this case that
                // moved: a step is not a standing tile. The mover is given
                // the near side of the crossing the price was paid at —
                // a tile of the creep's *own* room — because a Task that is
                // priced and unwalkable is a Task the Matcher gives away and
                // anti-thrash never takes back. What stays refused is
                // everything the creep would do on the far side: nowhere to
                // stand, and no action reaching over.
                Expect.equal
                    (firstStep atlas "w" (Harvest "src-out") handed)
                    (Some { X = 25; Y = 9 })
                    "but it is walked up its own corridor toward the Seam it was priced at"
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
                //
                // Unposted, this case read `Some 36` until #159: the heavy
                // body took ADR 0020's bare-Seat fallback across the border
                // and walked to a rock with no container under it. That
                // fallback is home's bootstrap and an outpost has another
                // one (ADR 0042), so the far leg now floods out of nothing
                // and there is no walk at all — the same answer a blocked
                // Post gives, one room over. The light body's own walk to
                // that Seat is untouched, which is what says the narrowing
                // is still the body's and not the room's.
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
                    None
                    "unposted and a room away, the heavy body has nowhere to walk to at all"

                Expect.equal
                    (walkTicks bare "w" (Harvest "src-out"))
                    (Some 18)
                    "while the light body still walks the eighteen tiles to that same Seat"

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

/// The two rooms the narrowing's own room rule is read in: the cross-room
/// corridor, with a rock at (25,20) of the colony's own room and a rock at
/// (25,40) of the outpost — neither with a container on it — and a
/// Work-heavy body and a light one standing in each room.
///
/// Both rooms, because the room is the whole of what the rule turns on and
/// one room cannot show it. Creeps on both sides, because the two halves
/// are read at different queries: `workAreaFor` answers a creep only in
/// its target's own room (ADR 0041), so the tiles are read from inside
/// each room, and the price is read from the home pair, which is where a
/// fresh Anchor really stands when the Matcher asks.
///
/// The caller places what stands in the outpost, so the same geometry
/// serves the unposted case and the posted one and nothing but the
/// container moves between them.
let private twoRockRooms placed kinds =
    northOf
        { corridorHome [ "a", { X = 25; Y = 10 }; "w", { X = 25; Y = 11 } ] with
            TargetPositions = Map.ofList [ "src-home", { X = 25; Y = 20 } ]
        }
        [ { X = 25; Y = 0 }, Plain ]
        { corridorOutpost with
            TargetPositions =
                (corridorOutpost.TargetPositions, placed)
                ||> List.fold (fun targets (id, pos) -> Map.add id pos targets)
            CreepPositions = Map.ofList [ "a-out", { X = 25; Y = 44 }; "w-out", { X = 25; Y = 45 } ]
        }
        [ { X = 25; Y = 49 }, Plain ]
        (("src-home", Source) :: ("src-out", Source) :: kinds)
        [
            creepWith "a" 0 [ Work; Work; Carry; Move ]
            worker "w"
            creepWith "a-out" 0 [ Work; Work; Carry; Move ]
            worker "w-out"
        ]

/// The Seats of the outpost's rock, which sits on ground the projection
/// does not carry: the corridor tile above it and the one below.
let private outpostSeats = Set.ofList [ { X = 25; Y = 39 }; { X = 25; Y = 41 } ]

[<Tests>]
let outpostHeavyAreaTests =
    testList
        "atlas outpost heavy work area"
        [
            test "an unposted rock a room away is no ground at all for a Work-heavy body" {
                // ADR 0020's fallback to the bare Seats is home's bootstrap:
                // before the first container stands, an Anchor there still
                // has to dig, and it does it a few tiles from the spawn that
                // replaces it and the haulers already working the room. An
                // outpost bootstraps through a reserver and a light builder
                // instead (ADR 0042), so the fallback would put a heavy body
                // on a rock with nothing under it to catch twelve a tick and
                // no hauler quota to collect it — the strand ADR 0042 names
                // when it makes the container the switch.
                let bare = twoRockRooms [] []

                Expect.isEmpty (postsOf bare "src-out") "the premise: no container, no Post"

                Expect.equal
                    (workArea bare (Harvest "src-out"))
                    outpostSeats
                    "the body-blind area is still the rock's two Seats"

                Expect.isEmpty
                    (workAreaFor bare "a-out" (Harvest "src-out"))
                    "and a heavy body standing in that room is handed none of them"

                Expect.equal
                    (workAreaFor bare "w-out" (Harvest "src-out"))
                    outpostSeats
                    "while a light body beside it keeps every Seat: the narrowing is the body's"

                Expect.equal
                    (travelCost bare "a" (Harvest "src-out"))
                    None
                    "so the Task is inapplicable to the Anchor that would have crossed for it"

                Expect.isSome
                    (travelCost bare "w" (Harvest "src-out"))
                    "and applicable to the light body, over the same Seam on the same tick"
            }

            test "the same body, the same tick, keeps the bare Seats of the rock at home" {
                // The discriminator, in one projection: two rocks with no
                // container on either, one heavy body's answer for each, and
                // the room the only difference between them. Read on the
                // home creep rather than the outpost one because that is
                // where ADR 0020's fallback has to survive — a colony whose
                // own first container is not built yet.
                let bare = twoRockRooms [] []

                Expect.isEmpty (postsOf bare "src-home") "the home rock has no container either"

                Expect.equal
                    (workAreaFor bare "a" (Harvest "src-home"))
                    (workArea bare (Harvest "src-home"))
                    "at home the bare Seats are still the fallback (ADR 0020)"

                Expect.isSome
                    (travelCost bare "a" (Harvest "src-home"))
                    "and the Task the outpost's rock lost stays applicable here"
            }

            test "a container standing on the outpost Seat gives the heavy body its ground back" {
                // The switch, at the geometry (ADR 0042). Nothing here is a
                // rule about outposts and heavy bodies: it is the same
                // narrowing to the Posts the home room has always had, and
                // the outpost's answer moves the tick a container stands.
                let posted =
                    twoRockRooms
                        [ "cont-out", { X = 25; Y = 41 } ]
                        [ "cont-out", Structure BuiltKind.Container ]

                Expect.equal
                    (postsOf posted "src-out")
                    (Set.singleton { X = 25; Y = 41 })
                    "the Seat under the container is the rock's Post"

                Expect.equal
                    (workAreaFor posted "a-out" (Harvest "src-out"))
                    (Set.singleton { X = 25; Y = 41 })
                    "and it is the one tile the heavy body may work from"

                Expect.isSome
                    (travelCost posted "a" (Harvest "src-out"))
                    "so the Anchor at home is priced across the border again"
            }
        ]

/// The spawn structure of every lead below, standing at (25,10) of the home
/// corridor. An obstacle, as a spawn is, so a finished body is born on
/// (25,9) or (25,11) and the walk it is led by starts there.
let private leadSpawn = { X = 25; Y = 10 }

/// The tile in the outpost a lead below is priced to: the Seat under that
/// room's rock, which is where an outpost's Anchor garrisons (ADR 0042).
let private outpostSeat = { X = 25; Y = 41 }

/// The two rooms every lead below is priced over: the cross-room fixture's
/// own corridors, with the spawn structure standing in the home one and the
/// rings the caller shapes. The Snapshot, because one case hands its own
/// walk table in (ADR 0032).
let private leadAcrossSnapshot homeRing outpostRing placed creeps =
    northOfSnapshot
        { corridorHome placed with
            TargetPositions = Map.ofList [ "spawn-1", leadSpawn ]
            Obstacles = Set.singleton leadSpawn
        }
        homeRing
        corridorOutpost
        outpostRing
        [ "spawn-1", Structure BuiltKind.Spawn; "src-out", Source ]
        creeps

let private leadAcross homeRing outpostRing placed creeps =
    leadAcrossSnapshot homeRing outpostRing placed creeps |> ofSnapshot

[<Tests>]
let crossRoomLeadTests =
    testList
        "atlas cross-room castWalkTicks"
        [
            /// The hauler unit empty: no fatigue-generating part at all, so
            /// it rides the walk's one-tick floor over every tile.
            let hauler = [ Carry; Carry; Move ]

            /// The Anchor row's minimal cast, empty: two Work over one Move,
            /// so 4 units and 2 ticks a plain step and 20 units and 10 ticks
            /// a swamp one. The body an outpost's garrison is actually
            /// replaced by at a 300 bank (`anchorBodyFor`).
            let anchorUnit = [ Work; Work; Carry; Move ]

            let openRings = [ { X = 25; Y = 0 }, Plain ], [ { X = 25; Y = 49 }, Plain ]

            test "a creep in the outpost is led across the Seam, on the join everything else uses" {
                // ADR 0026's succession over ADR 0041's border (#153). The
                // replacement is born on (25,9), walks eight tiles up to
                // (25,1), steps onto the exit at (25,0), is moved to (25,49)
                // for nothing at the end of that tick, steps off onto
                // (25,48) and walks seven more down to the Seat at (25,41):
                // seventeen tiles stepped onto, one tick each for a body
                // that generates no fatigue.
                let homeRing, outpostRing = openRings

                let atlas =
                    leadAcross
                        homeRing
                        outpostRing
                        [ "w", { X = 25; Y = 9 } ]
                        [ creepWith "w" 0 hauler ]

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N2" outpostSeat)
                    (Some 17)
                    "eight near, the exit, and eight in the outpost"

                // The same ground, the same body and the same border, read
                // by the other clock in the colony: a creep standing on the
                // birth tile is walked to that Seat in exactly the ticks the
                // lead charges for reaching it. One join, two readers (ADR
                // 0030) — a second cross-room arithmetic of the lead's own
                // would agree here and drift everywhere else.
                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (castWalkTicks atlas hauler leadSpawn "W1N2" outpostSeat)
                    "the Matcher's walk and the lead's walk price one crossing"
            }

            test "the exit tile is charged what the body pays to step onto it" {
                // #123's narrowing of ADR 0041's literal `+1`, on the lead's
                // reader too: the Anchor unit pays 2 ticks a plain tile, so
                // sixteen near, the exit, and sixteen in the outpost. A
                // swamp crossing costs it ten rather than two, and the whole
                // lead is eight ticks longer — the eight ticks a colony
                // whose Seam is swamp has to cast its replacement earlier.
                let homeRing, outpostRing = openRings

                let over ring =
                    leadAcross [ { X = 25; Y = 0 }, ring ] outpostRing [] []

                Expect.equal
                    (castWalkTicks
                        (leadAcross homeRing outpostRing [] [])
                        anchorUnit
                        leadSpawn
                        "W1N2"
                        outpostSeat)
                    (Some 34)
                    "sixteen near, two onto a plain exit, sixteen in the outpost"

                Expect.equal
                    (castWalkTicks (over Swamp) anchorUnit leadSpawn "W1N2" outpostSeat)
                    (Some 42)
                    "and a swamp exit is charged its own ten"
            }

            test "a goal in the colony's own room is led off the home flood, unchanged" {
                // The regression ADR 0026's existing cases are the rest of:
                // the room the goal stands in is the caller's since #153,
                // and naming home is the walk this rule always ran — the
                // same flood out of the birth tiles, the same lookup, no
                // band consulted. Nine tiles down the corridor from (25,11).
                let homeRing, outpostRing = openRings
                let atlas = leadAcross homeRing outpostRing [] []

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N1" { X = 25; Y = 20 })
                    (Some 9)
                    "the home leg alone, priced as it was before the border was crossable"
            }

            test "no crossing and no ring each lead nobody" {
                // Total (ADR 0004), one absence at a time: an unpriceable
                // Seam is no Seam, so the lead is absent exactly as an
                // unreachable tile inside one room makes it absent — never a
                // zero, which would leave the creep counted as living for
                // ever. The open ring at the bottom is what makes the None
                // above the wall's answer rather than the fixture's.
                //
                // Two of `seams`'s absences, and the third is not here: the
                // walled ring reaches the band and finds nothing passable in
                // it, the last room reaches no band at all because the
                // projection carries no ring under its name — while a room
                // that *is* projected and simply is not an orthogonal
                // neighbour is `RoomInvariantTests.seamTests`' case, on the
                // real captures. One answer, three reasons.
                let homeRing, outpostRing = openRings
                let open' = leadAcross homeRing outpostRing [] []
                let walled = leadAcross [ { X = 25; Y = 0 }, Wall ] outpostRing [] []

                Expect.isEmpty (seams walled "W1N1" "W1N2") "the premise: the exit is walled"

                Expect.equal
                    (castWalkTicks walled hauler leadSpawn "W1N2" outpostSeat)
                    None
                    "no crossing, no lead"

                Expect.equal
                    (castWalkTicks open' hauler leadSpawn "W1N2" outpostSeat)
                    (Some 17)
                    "open that one tile and the same two rooms lead again"

                Expect.equal
                    (castWalkTicks open' hauler leadSpawn "W5N5" outpostSeat)
                    None
                    "and a room the projection carries no ring for has no band to be led over"
            }

            test "a creep on the border ring itself leads nobody, on either side of it" {
                // The tick a crossing lands: the engine parks the creep on
                // the far room's ring tile and `Snapshot` files it there, so
                // a lead asked for that tick is asked about a tile that is
                // in no room's ground (ADR 0041 keeps the rings beside the
                // projection, never inside it). Unpriceable, therefore, and
                // absent rather than zero-with-a-guess — for one tick the
                // creep is simply counted living, and the next tick it
                // stands on ground and is led again.
                let homeRing, outpostRing = openRings
                let atlas = leadAcross homeRing outpostRing [] []

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N2" { X = 25; Y = 49 })
                    None
                    "the landing tile is the outpost's ring, and no ground of it"

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N1" { X = 25; Y = 0 })
                    None
                    "and the exit tile is the home room's ring, which the flood never enters"
            }

            test "the far leg joins the walk table, under the room the goal stands in" {
                // #169: the far leg rides ADR 0032's table exactly as the
                // near leg does, because every input it reads is in the
                // census — the goal room's weight grid is signed per
                // projected room, and the Seam band is terrain, which never
                // moves. So the key grew the room that keeps two rooms'
                // coordinates apart, and the table holds one entry per goal
                // *room* rather than one per goal tile: a second tile of the
                // same outpost is a lookup, not a flood.
                let homeRing, outpostRing = openRings
                let walks = WalkTable()

                let byRoom () =
                    walks
                    |> Seq.map (fun entry ->
                        let _, _, room = entry.Key
                        room, entry.Value)
                    |> Map.ofSeq

                let atlas =
                    leadAcrossSnapshot homeRing outpostRing [] [] |> ofSnapshotRecalling walks

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N2" outpostSeat)
                    (Some 17)
                    "the premise: the lead is priced across the border"

                Expect.equal
                    (byRoom () |> Map.toList |> List.map fst)
                    [ "W1N1"; "W1N2" ]
                    "the near leg is filed under home and the joined far leg under the outpost"

                let flooded = byRoom ()

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N2" { X = 25; Y = 42 })
                    (Some 16)
                    "a second tile of the same outpost — one back toward the border, one tick nearer — is read off that same entry"

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn "W1N1" { X = 25; Y = 20 })
                    (Some 9)
                    "and the home lead reads the entry filed under home"

                Expect.equal
                    walks.Count
                    2
                    "neither adds an entry: the key names the room, never the goal"

                // The recall half, both rooms at once: an Atlas handed a
                // filled table reads the very arrays the first one flooded,
                // so a census that has not moved pays for no second
                // Dijkstra on either side of the border (ADR 0032).
                let second =
                    leadAcrossSnapshot homeRing outpostRing [] [] |> ofSnapshotRecalling walks

                Expect.equal
                    (castWalkTicks second hauler leadSpawn "W1N2" outpostSeat)
                    (Some 17)
                    "the recalled far leg prices the same lead"

                Expect.equal
                    (castWalkTicks second hauler leadSpawn "W1N1" { X = 25; Y = 20 })
                    (Some 9)
                    "and the recalled near leg the same home one"

                Expect.equal walks.Count 2 "no second entry under either key"

                for room in [ "W1N1"; "W1N2" ] do
                    Expect.isTrue
                        (obj.ReferenceEquals(Map.find room (byRoom ()), Map.find room flooded))
                        $"the second Atlas read {room}'s flood rather than running its own"

                Expect.equal
                    (castWalkTicks second hauler leadSpawn "W5N5" outpostSeat)
                    None
                    "a room the projection carries no ring for is still led over by nobody"

                Expect.equal
                    walks.Count
                    2
                    "and leaves no entry behind: a band that answers empty is asked again, never remembered"
            }
        ]

/// The hauler the haul below is priced for, and the body the two creeps
/// standing on its container are cast from: `fatigueFactorOf` reads a
/// living creep's load, so a full one carries the round trip's loaded
/// factor and an empty one its empty factor, exactly — which is what lets
/// the quota's legs be pinned against the Matcher's own walk.
let private haulerBody = [ Carry; Carry; Move ]

/// The haul the outpost's container makes: the container standing at
/// (25,41) of the outpost's corridor, the spawn structure eleven tiles
/// down the home room's at (25,10). The spawn is an obstacle, so the only
/// tile a transfer reaches it from on the side the haul arrives on is
/// (25,9); the tile behind it is ground the corridor never opens onto from
/// the north. The two haulers stand on the container, one full and one
/// empty; standing creeps price nothing here, the round trip and the walk
/// alike being traffic-blind (ADR 0029). The rings are the caller's, so a
/// case can wall a crossing off or lay swamp on it.
let private haulAcross homeRing outpostRing =
    northOf
        { RoomLayer.empty with
            Terrain = Map.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
            TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
            Obstacles = Set.singleton { X = 25; Y = 10 }
        }
        homeRing
        { RoomLayer.empty with
            Terrain = Map.ofList (plainLine [ for y in 41..48 -> { X = 25; Y = y } ])
            TargetPositions = Map.ofList [ "can-out", { X = 25; Y = 41 } ]
            CreepPositions =
                Map.ofList [ "loaded", { X = 25; Y = 41 }; "empty", { X = 25; Y = 41 } ]
        }
        outpostRing
        [
            "spawn-1", Structure BuiltKind.Spawn
            "can-out", Structure BuiltKind.Container
        ]
        [ creepWith "loaded" 100 haulerBody; creepWith "empty" 0 haulerBody ]

/// The one haul every case below prices: the outpost's container to the
/// home room's spawn, rooms named on the way in because a `Pos` names none
/// (ADR 0041).
let private roundTripOf atlas =
    haulRoundTripTicks atlas haulerBody "W1N2" { X = 25; Y = 41 } "W1N1" { X = 25; Y = 10 }

[<Tests>]
let crossRoomHaulTests =
    testList
        "atlas cross-room haulRoundTripTicks"
        [
            test "the round trip across a border is two Seam joins, one per leg" {
                // ADR 0042's outpost haul, countable a tile at a time.
                // Loaded, a plain step costs two ticks for this body and
                // the exit tile costs the same: seven steps up the
                // outpost's corridor to (25,48) is 14, the crossing at
                // (25,49) is 2, and the far leg — the step onto (25,1)
                // plus eight more down to (25,9) — is 18. Thirty-four out.
                // Empty, every one of those tiles sits on ADR 0029's
                // one-tick floor: 7 + 1 + 9 = 17 back. Fifty-one for the
                // round trip, which is the order ADR 0042 costs an unpaved
                // outpost haul at and not a bug.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Plain ]

                Expect.equal
                    (roundTripOf atlas)
                    (Some 51)
                    "fourteen, two and eighteen out; seven, one and nine back"
            }

            test "each leg is the walk's own join, read off the same Seam band" {
                // ADR 0030's law, at the reader that used to be exempt from
                // it: the quota's round trip must be the same arithmetic
                // the Matcher ranks on and the mover walks, and not a
                // second cross-room pricing of its own. So the two legs are
                // pinned against `walkTicks` — one creep carrying the load
                // the leg out is priced for, one empty as the leg back is —
                // walking to the very tiles a transfer reaches the spawn
                // from.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Plain ]

                let out = walkTicks atlas "loaded" (Refill "spawn-1")
                let back = walkTicks atlas "empty" (Refill "spawn-1")

                Expect.equal out (Some 34) "the premise: the loaded leg the Matcher would price"
                Expect.equal back (Some 17) "and the empty one"

                Expect.equal
                    (roundTripOf atlas)
                    (Option.map2 (+) out back)
                    "the round trip is those two walks and nothing else"
            }

            test "a swamp crossing is charged to each leg at its own factor" {
                // The trap the two legs exist for. Swamp weighs five
                // times plain, so this body pays ten ticks to step onto
                // the crossing loaded and one empty — the empty leg's step
                // was already on ADR 0029's floor and cannot get dearer.
                // Against the plain-exit case above the round trip gains
                // exactly the loaded leg's eight, which a single crossing
                // priced once for both legs could not produce at any
                // factor.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Swamp ]

                Expect.equal
                    (roundTripOf atlas)
                    (Some 59)
                    "the loaded leg pays the swamp its ten, the empty leg its one"
            }

            test "a border with no crossing prices no round trip" {
                // ADR 0004, and the shape the hauler quota reads it in: an
                // unpriceable Seam is no Seam, so the haul has no price and
                // the container hires nobody — never a zero, which would
                // hire a fleet for free.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Wall ] [ { X = 25; Y = 49 }, Plain ]

                Expect.isEmpty (seams atlas "W1N2" "W1N1") "the premise: the exit is walled"

                Expect.equal (roundTripOf atlas) None "unpriceable geometry hires nobody"
            }
        ]

[<Tests>]
let crossRoomStepTests =
    testList
        "atlas cross-room step"
        [
            test "the mover aims at the crossing the price was paid at, not the nearest one" {
                // #142's trap, on the fixture the price is already pinned
                // on: (25,0) is nine steps away and (27,0) ten, but the
                // outpost's column under (25,49) is swamp and the one under
                // (27,49) is plain, so the band's minimum is the *farther*
                // exit. A mover that minimised the near leg again — its own
                // second minimisation — would walk the creep up column 25
                // to a crossing it was never priced at, and the two answers
                // would agree on every number and split on this one.
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

                Expect.equal
                    (walkTicks bothOpen "w" (Harvest "src-out"))
                    (Some 19)
                    "the premise: the price crosses at (27,0), the farther of the two"

                Expect.equal
                    (firstStepFor bothOpen "w" (Harvest "src-out"))
                    (Some { X = 26; Y = 10 })
                    "so the creep leaves its column sideways, toward that crossing"

                // Wall the cheap crossing and the price falls back to the
                // near one; the step falls back with it, which is the pair
                // moving together rather than one of them being a constant.
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
                    "with only the dear crossing left, the price is paid at (25,0)"

                Expect.equal
                    (firstStepFor swampOnly "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 9 })
                    "and the same creep now walks straight up its own column instead"
            }

            test "a creep parked on the ring prices and crosses from the tile it stands on" {
                // The one tile off a room's ground a near leg is honestly
                // read at (#175). The near side of a crossing is the room's
                // own ground and nothing else — the ring is not ground (ADR
                // 0036) and a flood never relaxes onto it — but a flood
                // *seeds* its origin whatever that tile weighs, so a creep
                // the engine parked on the ring the tick it crossed (#142,
                // #145) is already standing beside every crossing next to
                // it, at no cost at all.
                //
                // What is pinned here is the price and the step #175 found
                // in the tree, not a ruling on #146: whether a creep on the
                // ring should be aimed sideways along it at the crossing
                // next door is that ticket's open question, and this one
                // only had to leave the answer where it was.
                //
                // Countable: the creep stands on the home room's ring at
                // (24,0), with two crossings open. Priced at (25,0) it pays
                // nothing to approach — it is standing beside it — one tick
                // onto the exit and eight in the outpost, which is nine.
                // Priced at (24,0) it would first step to (25,1), the only
                // ground beside that exit, and pay ten. So the band's
                // minimum is (25,0), the crossing the creep can reach
                // without leaving the ring, and the step is that exit
                // itself. Read the ring off the near side instead and both
                // crossings cost ten, the tie falls to (24,0), and the
                // creep is walked inland to (25,1) to cross where it was
                // never priced.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 24; Y = 0 } ])
                        [ { X = 24; Y = 0 }, Plain; { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 24; Y = 49 }, Plain; { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.hasLength (seams atlas "W1N1" "W1N2") 2 "the premise: two crossings"

                Expect.isTrue (standsOnSeam atlas "w") "and the creep stands on the ring"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 9)
                    "no approach to pay, the exit's own tick, and eight in the outpost"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "and the same join in the ranking price's own units"

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 0 })
                    "so the step is the crossing the price was paid at, taken off the ring"
            }

            test "the last step onto an exit tile is a step nothing offers to stand on" {
                // ADR 0036 and ADR 0041 keep the exit out of the projection's
                // ground, and #142 does not put it back: the mover may push a
                // creep *onto* one, and every query that offers somewhere to
                // stand still refuses to name it. So the tile is reachable as
                // a destination and unreachable as a Seat, a Work Area member
                // or a walkable tile — which is what stops the Matcher ever
                // seating a creep the engine will empty out from under it.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 1 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 0 })
                    "standing beside the exit, the step is the exit itself"

                Expect.isFalse
                    (Set.contains { X = 25; Y = 0 } (walkableTiles atlas))
                    "and that tile is no walkable ground of this room"

                Expect.isFalse
                    (List.contains { X = 25; Y = 0 } (adjacentWalkable atlas { X = 25; Y = 1 }))
                    "no neighbour a parked creep may be displaced onto"

                Expect.isFalse
                    (Set.contains { X = 25; Y = 0 } (workArea atlas (Harvest "src-out")))
                    "and no tile of any Work Area"
            }

            test "the action gate stays shut at the Seam and opens where the creep may stand" {
                // The boundary #142 pushes a creep up to and never over.
                // `mayAct` is `false` across a border because the engine's
                // ranges are measured inside one room, and the mover asking
                // for a step toward the Seam does not make it `true`: the
                // Work Area a creep is handed is still empty, and the tile
                // it is walked to is still no tile of one. The gate opens
                // when the creep is standing in the target's own room and on
                // a tile of the Work Area there, which is a fact about where
                // the projection files it and needs no rule of its own —
                // and the tile the engine lands it on is not yet one.
                let across creepAt outpostCreeps =
                    northOf
                        (corridorHome creepAt)
                        [ { X = 25; Y = 0 }, Plain ]
                        { corridorOutpost with
                            CreepPositions = Map.ofList outpostCreeps
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let here = across [ "w", { X = 25; Y = 1 } ] []
                let landed = across [] [ "w", { X = 25; Y = 49 } ]
                let there = across [] [ "w", { X = 25; Y = 41 } ]

                Expect.isFalse
                    (mayActFor here "w" (Harvest "src-out"))
                    "a step from the Seam is still a room away from digging"

                Expect.equal
                    (firstStepFor here "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 0 })
                    "and it is walked onto the exit on the very tick it may not act"

                // The tile the crossing above actually delivers to, and not
                // an interior one hand-placed past it: stepping onto (25,0)
                // lands the creep on the outpost's own ring at (25,49). The
                // gate is still shut there — the landing tile is no Work
                // Area tile, and the raw-range escape a ringed creep takes
                // measures nine, not one — and the Atlas answers the step
                // that opens it. What walks that step is the Resolver,
                // which arbitrates each projected room by itself (ADR
                // 0041, #145): the outpost's pass hands the landed creep
                // that step, and `DecideTests` drives it from the landing
                // tile to the dig. This test's subject is the gate, which
                // the Atlas keeps shut until the creep may stand.
                Expect.isFalse
                    (mayActFor landed "w" (Harvest "src-out"))
                    "the tile the engine puts it down on is no tile of the Work Area"

                Expect.equal
                    (firstStepFor landed "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 48 })
                    "and the geometry has the step off the ring that the ordinary rules ask for"

                Expect.isTrue
                    (mayActFor there "w" (Harvest "src-out"))
                    "standing in the target's own room, the gate opens off `sharesRoom` alone"

                Expect.equal
                    (firstStepFor there "w" (Harvest "src-out"))
                    None
                    "standing in the Work Area it has arrived at, it has no step left to take"
            }

            test
                "a creep on the border ring stands on a Seam, and one on ground or nowhere does not" {
                // The fact the far-side mover reads (#145): the tile the
                // engine lands a crossing creep on is a Seam, never ground
                // (ADR 0036), and a creep left standing on it is moved out
                // of the room again at the end of the tick. Read off the
                // coordinate, in whichever room the projection files the
                // creep under; a creep it places nowhere stands on no Seam
                // (ADR 0004).
                let at homeCreeps outpostCreeps =
                    northOf
                        (corridorHome homeCreeps)
                        [ { X = 25; Y = 0 }, Plain ]
                        { corridorOutpost with
                            CreepPositions = Map.ofList outpostCreeps
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.isTrue
                    (standsOnSeam (at [] [ "w", { X = 25; Y = 49 } ]) "w")
                    "landed on the outpost's ring"

                Expect.isTrue
                    (standsOnSeam (at [ "w", { X = 25; Y = 0 } ] []) "w")
                    "and on the home room's exit row alike"

                Expect.isFalse
                    (standsOnSeam (at [] [ "w", { X = 25; Y = 48 } ]) "w")
                    "one step inside, the creep is on ground"

                Expect.isFalse
                    (standsOnSeam (at [] []) "w")
                    "and a creep the projection cannot place stands on no Seam"
            }

            test "a tied band is committed to, not shuttled between" {
                // The tie is where two minimisations diverge, so the fixture
                // holds the tie open for the whole approach: the creep walks
                // a column that stays equidistant from both crossings, and
                // the two are mirror images down to the far room's ground.
                // Priced separately they cost the same to the tick, which is
                // the premise; priced together the band's minimum picks one,
                // and the mover has to pick that same one every tick or the
                // creep walks a diagonal back and forth below the border and
                // never crosses at all.
                let home pos =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
                                plainLine
                                    [
                                        for y in 1..10 -> { X = 25; Y = y }
                                        for x in 23..27 -> { X = x; Y = 1 }
                                    ]
                            )
                        CreepPositions = Map.ofList [ "w", pos ]
                    }

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
                                plainLine
                                    [
                                        for y in 41..48 -> { X = 25; Y = y }
                                        for x in 23..27 -> { X = x; Y = 48 }
                                    ]
                            )
                        TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
                    }

                let across ring pos =
                    northOf
                        (home pos)
                        [ for x in ring -> { X = x; Y = 0 }, Plain ]
                        outpost
                        [ for x in ring -> { X = x; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let start = { X = 25; Y = 10 }

                Expect.equal
                    (walkTicks (across [ 23 ] start) "w" (Harvest "src-out"))
                    (walkTicks (across [ 27 ] start) "w" (Harvest "src-out"))
                    "the premise: either crossing alone costs this creep the same walk"

                Expect.hasLength
                    (seams (across [ 23; 27 ] start) "W1N1" "W1N2")
                    2
                    "and with both open the band holds two of them"

                // Driven a tick at a time, the way the Resolver drives it:
                // the creep stands where the last step put it and is asked
                // again. The drive stops the tick the step leaves this
                // room's ground, which is the tick it crosses.
                let ground = (home start).Terrain

                let rec drive pos taken =
                    if List.length taken > 20 then
                        failtest "the creep never reached a crossing"
                    else
                        match firstStepFor (across [ 23; 27 ] pos) "w" (Harvest "src-out") with
                        | Some step when Map.containsKey step ground -> drive step (step :: taken)
                        | Some step -> List.rev (step :: taken)
                        | None -> List.rev taken

                Expect.equal
                    (drive start [])
                    [
                        { X = 25; Y = 9 }
                        { X = 25; Y = 8 }
                        { X = 25; Y = 7 }
                        { X = 25; Y = 6 }
                        { X = 25; Y = 5 }
                        { X = 25; Y = 4 }
                        { X = 25; Y = 3 }
                        { X = 25; Y = 2 }
                        { X = 24; Y = 1 }
                        { X = 23; Y = 0 }
                    ]
                    "one crossing, one route, and the exit tile is the last step of it"

                // The price falls by one plain step's units every tick of
                // that drive — which is the two answers being read off one
                // minimisation and not two that happen to agree today.
                let priced =
                    start :: List.take 9 (drive start [])
                    |> List.map (fun pos ->
                        travelCost (across [ 23; 27 ] pos) "w" (Harvest "src-out"))

                Expect.equal
                    (priced |> List.pairwise |> List.map (fun (a, b) -> Option.map2 (-) a b))
                    (List.replicate 9 (Some 2))
                    "every step of the drive buys exactly the two units a plain step costs"
            }
        ]
