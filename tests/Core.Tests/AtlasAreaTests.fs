/// Work Areas, the refill cluster, Seats, and where a body may stand.
module Fabot.Core.Tests.AtlasAreaTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

/// Every tile of a room but its exit ring, plain — the widest ground a room
/// can have. What an edge case answers here is the checked index and never a
/// hole in the terrain, which is the whole point of standing the cases on it.
let private wholeRoomPlain =
    Map.ofList
        [
            for x in 1..48 do
                for y in 1..48 -> { X = x; Y = y }, Plain
        ]

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
                    |> ofView

                Expect.equal
                    (workArea atlas (Harvest "src-a") |> tilesHome atlas)
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 } ])
                    "plain and swamp tiles in range are standing tiles; wall and absent are not"
            }

            test "an unplaced target has an empty Work Area" {
                let atlas = spatial [] [ { X = 9; Y = 10 }, Plain ] |> snapshotWith [] |> ofView

                Expect.equal
                    (workArea atlas (Harvest "ghost") |> tilesHome atlas)
                    Set.empty
                    "nowhere to stand"
            }

            test "Build and Upgrade Work Areas reach range 3" {
                let tiles =
                    [
                        for x in 7..13 do
                            for y in 7..13 -> { X = x; Y = y }, Plain
                    ]

                let atlas =
                    spatial [ "ctrl-1", { X = 10; Y = 10 } ] tiles |> snapshotWith [] |> ofView

                let area = workArea atlas (Upgrade "ctrl-1") |> tilesHome atlas
                Expect.hasLength area 49 "the full 7x7 square is passable"
                Expect.isTrue (Set.contains { X = 7; Y = 7 } area) "the corner at range 3 is in"
            }
        ]

[<Tests>]
let refillClusterTests =
    testList
        "atlas refill cluster"
        [
            test "the cluster's Work Area is the union of its hungry members' rings" {
                // ADR 0054: the Refill's target id is the anchor, and the
                // tiles it may be worked from are every hungry member's ring
                // — which is what makes one Task out of a ring of ten.
                let atlas = clusterAtlas [] (50, 50, 0)

                let area = workArea atlas (Refill("spawn-1", Energy)) |> tilesHome atlas

                Expect.isTrue
                    (Set.contains { X = 9; Y = 10 } area)
                    "the anchor's own ring is in the area"

                Expect.isTrue
                    (Set.contains { X = 15; Y = 10 } area)
                    "so is a hungry extension's, four tiles away from the anchor"

                Expect.isFalse
                    (Set.contains { X = 14; Y = 10 } area)
                    "a hungry member's own tile is an obstacle and never a standing tile"

                Expect.isFalse
                    (Set.contains { X = 12; Y = 10 } area)
                    "and a full member's tile is outside the area on both counts"
            }

            test "a full member contributes no tile: a body is never sent where it cannot pour" {
                // The pairing that says the area is the *hungry* members'
                // and not every member's. (11,10) touches ext-2 alone.
                let hungry = clusterAtlas [] (0, 0, 50)
                let full = clusterAtlas [] (0, 50, 0)

                Expect.isTrue
                    (Set.contains
                        { X = 11; Y = 10 }
                        (workArea hungry (Refill("spawn-1", Energy)) |> tilesHome hungry))
                    "ext-2 has room, so the tile beside it is a tile to work from"

                Expect.isFalse
                    (Set.contains
                        { X = 11; Y = 10 }
                        (workArea full (Refill("spawn-1", Energy)) |> tilesHome full))
                    "ext-2 is full, so its ring is nobody's standing room this tick"
            }

            test "refillTarget names a hungry member the body stands beside, not a full one" {
                // w1 at (13,10) touches ext-1 (14,10) and ext-2 (12,10)
                // alike, so the pair below moves only which of them has
                // room — the whole of what the Emitter's pick is for.
                let eastHungry = clusterAtlas [ "w1", { X = 13; Y = 10 } ] (0, 50, 0)
                let westHungry = clusterAtlas [ "w1", { X = 13; Y = 10 } ] (0, 0, 50)

                Expect.equal
                    (refillTarget eastHungry "w1" "spawn-1" Energy)
                    (Some "ext-1")
                    "ext-2 is full, so the transfer names the extension that is not"

                Expect.equal
                    (refillTarget westHungry "w1" "spawn-1" Energy)
                    (Some "ext-2")
                    "and the other way round, so it is room and not id order deciding"
            }

            test "a Refill that anchors no cluster names its own target" {
                // Every Refill but the cluster's — a tower's, the buffer's,
                // the Storage's, a ferry sink's — resolves to the structure
                // the Task already names, through the same call (ADR 0054).
                let atlas = clusterAtlas [ "w1", { X = 13; Y = 10 } ] (50, 50, 50)

                Expect.equal
                    (refillTarget atlas "w1" "tower-1" Energy)
                    (Some "tower-1")
                    "an unclustered Refill is the single structure it always was"
            }

            test "a cluster with nothing left to pour into names nothing" {
                let atlas = clusterAtlas [ "w1", { X = 13; Y = 10 } ] (0, 0, 0)

                Expect.isNone
                    (refillTarget atlas "w1" "spawn-1" Energy)
                    "the whole ring full is the tick the Emitter issues no transfer"
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
                    |> withObstacles [ { X = 9; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

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
                            Terrain = wholeRoomPlain
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
                    |> ofView

                Expect.equal
                    (seatTilesOf atlas "src-low" |> tilesHome atlas)
                    (Set.singleton { X = 1; Y = 1 })
                    "the low corner Seats its one ground neighbour, and no negative coordinate"

                Expect.equal
                    (seatTilesOf atlas "src-high" |> tilesHome atlas)
                    (Set.singleton { X = 48; Y = 48 })
                    "the high corner Seats its one ground neighbour, and nothing at 50"

                Expect.equal
                    (seatTilesOf atlas "src-side" |> tilesHome atlas)
                    (Set.ofList [ { X = 1; Y = 24 }; { X = 1; Y = 25 }; { X = 1; Y = 26 } ])
                    "a mid-edge tile Seats the three ground tiles inside it"

                Expect.equal
                    (seatTilesOf atlas "src-in" |> tilesHome atlas)
                    (Set.ofList [ { X = 1; Y = 2 }; { X = 2; Y = 1 }; { X = 2; Y = 2 } ])
                    "and a source one tile in Seats no exit tile: the ring is not ground"
            }

            test "an unplaced source has no Seat count at all" {
                let atlas = spatial [] [ { X = 9; Y = 10 }, Plain ] |> snapshotWith [] |> ofView

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
                    |> withObstacles [ { X = 11; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (adjacentWalkableIn atlas (atlasHome atlas) { X = 10; Y = 10 })
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
                    |> withHome (fun layer -> { layer with Terrain = wholeRoomPlain })
                    |> snapshotWith []
                    |> ofView

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
                    |> ofView

                Expect.equal
                    (walkableTilesIn atlas (atlasHome atlas))
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
                    |> withCreepsAt [ "w", { X = 10; Y = 11 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

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
                    |> withCreepsAt [ "amy", { X = 5; Y = 5 } ]
                    |> snapshotWith [ worker "amy"; worker "ghost" ]
                    |> ofView

                Expect.equal
                    (creepTile atlas "amy")
                    (Some(at "" { X = 5; Y = 5 }))
                    "the tile it stands on, joined to the room it stands in"

                Expect.isNone (creepTile atlas "ghost") "a creep the projection does not place"
            }
        ]
