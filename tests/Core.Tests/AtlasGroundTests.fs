/// The ground a body works and idles on: Work Areas narrowed to a creep,
/// Posts, and the consistency between them.
module Fabot.Core.Tests.AtlasGroundTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

[<Tests>]
let workAreaForTests =
    testList
        "atlas workAreaFor"
        [
            // Source at (10,10) with three Seats: (9,10) carries a built
            // container, (11,10) lies inside the controller's Upgrade area,
            // (10,11) is an ordinary Seat. One Post, two plain Seats (#405).
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
                |> withCreepsAt creeps

            let anchor name =
                creepWith name 0 [ Work; Work; Carry; Move ]

            test "a Work-heavy body harvests a posted source from its Posts alone" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofView

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a") |> tilesHome atlas)
                    (Set.singleton { X = 9; Y = 10 })
                    "the container Seat, not the plain Seats"

                Expect.equal
                    (workArea atlas (Harvest "src-a") |> tilesHome atlas)
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 }; { X = 10; Y = 11 } ])
                    "the body-blind area keeps every Seat"
            }

            test "a light body keeps the Seats beyond the Posts of the same source" {
                let atlas =
                    posted [ "w", { X = 10; Y = 11 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (workAreaFor atlas "w" (Harvest "src-a") |> tilesHome atlas)
                    (Set.difference
                        (workArea atlas (Harvest "src-a") |> tilesHome atlas)
                        (postsOf atlas "src-a" |> tilesHome atlas))
                    "Work <= Move keeps the Seats less the Posts"

                Expect.isFalse
                    (workAreaFor atlas "w" (Harvest "src-a") |> Set.isEmpty)
                    "and this source has a bare Seat to keep"
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
                    |> withCreepsAt [ "a", { X = 10; Y = 11 } ]
                    |> snapshotWith [ anchor "a" ]
                    |> ofView

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a") |> tilesHome atlas)
                    (workArea atlas (Harvest "src-a") |> tilesHome atlas)
                    "the full Seat set is the fallback before the first container"
            }

            // The deposit's mirror: "min-a" embedded in wall at (20,20) with
            // three open neighbours, the container on one of them.
            let mined kinds creeps =
                { spatial
                      [
                          "min-a", { X = 20; Y = 20 }
                          "ext-a", { X = 20; Y = 20 }
                          "can-min", { X = 19; Y = 20 }
                      ]
                      [
                          { X = 19; Y = 20 }, Plain
                          { X = 21; Y = 20 }, Plain
                          { X = 20; Y = 21 }, Plain
                      ] with
                    TargetKinds = Map.ofList kinds
                }
                |> withCreepsAt creeps

            let minerBody name = creepWith name 0 [ Work; Work; Move ]

            let minedKinds =
                [
                    "min-a", Mineral
                    "ext-a", Structure BuiltKind.Extractor
                    "can-min", Structure BuiltKind.Container
                ]

            test "a miner digs a deposit from the tile its container stands on" {
                // A harvest with no room for the yield drops it on the
                // creep's own tile, and a drop onto a container tile lands
                // in the container.
                let atlas =
                    mined minedKinds [ "m", { X = 21; Y = 20 } ]
                    |> snapshotWith [ minerBody "m" ]
                    |> ofView

                Expect.equal
                    (workAreaFor atlas "m" (Harvest "min-a") |> tilesHome atlas)
                    (Set.singleton { X = 19; Y = 20 })
                    "the container's Seat alone"

                Expect.equal
                    (workArea atlas (Harvest "min-a") |> tilesHome atlas)
                    (Set.ofList [ { X = 19; Y = 20 }; { X = 21; Y = 20 }; { X = 20; Y = 21 } ])
                    "the body-blind area keeps every Seat, as a source's does"
            }

            test "a deposit with no container keeps no fallback, even at home" {
                // Pairwise against the source case, one target kind apart.
                let atlas =
                    mined [ "min-a", Mineral ] [ "m", { X = 21; Y = 20 } ]
                    |> snapshotWith [ minerBody "m" ]
                    |> ofView

                Expect.isNonEmpty
                    (workArea atlas (Harvest "min-a") |> tilesHome atlas |> Set.toList)
                    "the premise: the deposit's Seats are there to fall back to"

                Expect.equal
                    (workAreaFor atlas "m" (Harvest "min-a") |> tilesHome atlas)
                    Set.empty
                    "and the miner is offered none of them"
            }

            test "only Harvest narrows: the heavy body's Upgrade area is untouched" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofView

                Expect.equal
                    (workAreaFor atlas "a" (Upgrade "ctrl-1") |> tilesHome atlas)
                    (workArea atlas (Upgrade "ctrl-1") |> tilesHome atlas)
                    "Upgrade is body-blind (ADR 0020)"
            }

            test "an unplaceable source stays empty for a heavy body" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofView

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "ghost") |> tilesHome atlas)
                    Set.empty
                    "nowhere to stand"
            }

            test "a blocked Post empties the area rather than widening back to the Seats" {
                // An obstacle stands on the container Seat, still a Post by census.
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
                    |> ofView

                Expect.equal
                    (postsOf atlas "src-a" |> tilesHome atlas)
                    (Set.singleton { X = 9; Y = 10 })
                    "the Seat under the container is a Post by census"

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a") |> tilesHome atlas)
                    Set.empty
                    "a Post nothing may stand on leaves nowhere to stand"

                Expect.equal
                    (travelCost atlas "a" (Harvest "src-a"))
                    None
                    "so the Task is inapplicable — no retry with the full Seat set"
            }

            test "travel cost and first step price the Post, not the nearer Seat" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofView

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
                    |> withCreepsAt [ "w", creepPos ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

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
                    |> withCreepsAt [ "w", creepPos ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.isTrue
                    (mayActFor (atlasAt { X = 10; Y = 13 }) "w" (Repair "road-1"))
                    "repair reaches at range 3"

                Expect.isFalse
                    (mayActFor (atlasAt { X = 10; Y = 14 }) "w" (Repair "road-1"))
                    "repair does not reach at range 4"
            }

            test "a creep or target the projection cannot place never blocks the action" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.isTrue (mayActFor atlas "ghost" (Harvest "src-a")) "unplaced creep acts"
                Expect.isTrue (mayActFor atlas "w" (Harvest "ghost")) "unplaced target is acted on"
            }

            test "a Work-heavy body digs from its Post and nowhere else in range" {
                // The container Seat (9,10) is the only Post; the plain
                // Seat (10,11) is in harvest range all the same.
                let atlasAt creepPos =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> withCreepsAt [ "a", creepPos ]
                    |> snapshotWith [ creepWith "a" 0 [ Work; Work; Carry; Move ] ]
                    |> ofView

                Expect.isTrue
                    (mayActFor (atlasAt { X = 9; Y = 10 }) "a" (Harvest "src-a"))
                    "on the Post it digs"

                Expect.isFalse
                    (mayActFor (atlasAt { X = 10; Y = 11 }) "a" (Harvest "src-a"))
                    "in range but off the Post it does not — so it never fills en route"
            }

            test "a creep on a tile the projection calls impassable is judged by range" {
                // An obstacle-type site dropped under a standing creep: the
                // engine lets it stay.
                let atlas =
                    spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 10; Y = 11 }, Plain ]
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 10; Y = 11 }
                            CreepPositions = Map.ofList [ "w", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (workArea atlas (Harvest "src-a") |> tilesHome atlas)
                    Set.empty
                    "the creep's own tile is not a standing tile"

                Expect.isTrue
                    (mayActFor atlas "w" (Harvest "src-a"))
                    "unpriceable footing never blocks the action"
            }
        ]

[<Tests>]
let postTests =
    testList
        "atlas posts"
        [
            test "a Seat inside the controller's Upgrade area is no Post without a container" {
                // (11,10) sits at range 2 of the controller, (9,10) under a
                // built container: only the container makes a Post (#405).
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
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    (Set.singleton { X = 9; Y = 10 })
                    "the container Seat alone; the Seat in upgrade range is a plain Seat"
            }

            test
                "one source seats one Post: of two containers on its Seats, the one farther from the controller" {
                // The W11S27 shape (#405): the controller container landed
                // on a Seat of the rock beside it. (9,9) is at range 3 of the
                // controller, (11,10) at range 1: the rock's Post is (9,9),
                // and (11,10) is the controller's buffer.
                let atlas =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 12; Y = 11 }
                              "can-far", { X = 9; Y = 9 }
                              "can-near", { X = 11; Y = 10 }
                          ]
                          [ { X = 9; Y = 9 }, Plain; { X = 11; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "can-far", Structure BuiltKind.Container
                                    "can-near", Structure BuiltKind.Container
                                ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    (Set.singleton { X = 9; Y = 9 })
                    "the farther container is the rock's one Post"

                Expect.equal (postCount atlas) 1 "so the Anchor row is hired once"

                Expect.equal
                    (controllerContainers atlas)
                    (Set.singleton "can-near")
                    "and the nearer one is the controller's buffer"
            }

            test "two rocks sharing a Seat each name their own Post, never the other's" {
                // (11,10) seats both rocks. src-b's only container is there;
                // src-a's first pick is (9,10), so (11,10) is src-b's alone.
                let atlas =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "src-b", { X = 12; Y = 10 }
                              "can-a", { X = 9; Y = 10 }
                              "can-b", { X = 11; Y = 10 }
                          ]
                          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "src-b", Source
                                    "can-a", Structure BuiltKind.Container
                                    "can-b", Structure BuiltKind.Container
                                ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (postsOf atlas "src-a" |> tilesHome atlas)
                    (Set.singleton { X = 9; Y = 10 })
                    "src-a's one Post, though src-b's stands on its Seat too"

                Expect.equal
                    (postsOf atlas "src-b" |> tilesHome atlas)
                    (Set.singleton { X = 11; Y = 10 })
                    "and src-b's"

                Expect.equal (postCount atlas) 2 "two rocks, two Posts"
            }

            test "a container construction site is a Post, and no standing one" {
                // A pending container catches no overflow (`standingPostsOf`),
                // but the body on its Seat digs the rock beside it and
                // spends the yield into the site under its feet (#205).
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Site BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    (Set.singleton { X = 9; Y = 10 })
                    "the Seat carrying the site is a garrison place"

                Expect.equal
                    (postsOf atlas "src-a" |> tilesHome atlas)
                    (Set.singleton { X = 9; Y = 10 })
                    "and it is that source's own, by the Seat it stands on"

                Expect.isEmpty
                    (standingPostsOf atlas "src-a" |> tilesHome atlas)
                    "yet nothing stands there: the source is in no quota until it does"

                Expect.equal (postCount atlas) 1 "so the Anchor row is hired for it"
            }

            test "a container site off any Seat is no Post" {
                // A container going up two tiles from the rock (a buffer,
                // or a neighbouring source's Post) is nobody's garrison.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 12; Y = 10 } ]
                          [ { X = 11; Y = 10 }, Plain; { X = 12; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Site BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.isTrue
                    (Set.contains { X = 11; Y = 10 } (seatTilesOf atlas "src-a" |> tilesHome atlas))
                    "the premise: the rock's one Seat is the tile between the two"

                Expect.isEmpty
                    (postsIn atlas (atlasHome atlas))
                    "and the site a step past it garrisons nothing"
            }

            test "a built container off any Seat adds no Post" {
                // The controller container's shape: built, at range 2 of the source.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 12; Y = 10 } ]
                          [ { X = 11; Y = 10 }, Plain; { X = 12; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    Set.empty
                    "only a Seat under a container is a Post"
            }

            test "a room with no controller still derives container Posts" {
                // The W12S28 shape.
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "cont-1", { X = 9; Y = 10 } ]
                          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList [ "src-a", Source; "cont-1", Structure BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    (Set.singleton { X = 9; Y = 10 })
                    "one container Post"
            }
        ]

[<Tests>]
let workingGroundTests =
    testList
        "atlas workingGround"
        [
            test "the working ground is every source's Seats plus the Upgrade Work Area" {
                // The Upgrade Work Area reaches (12,10), (11,10) and
                // (10,11); the two Seats in both halves count once.
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
                    |> ofView

                Expect.equal
                    (workingGroundIn atlas (atlasHome atlas))
                    (Set.ofList
                        [
                            { X = 9; Y = 10 }
                            { X = 10; Y = 11 }
                            { X = 11; Y = 10 }
                            { X = 12; Y = 10 }
                        ])
                    "the Seats the Anchors stand on and the tiles the upgraders stand on, together"
            }

            test "a Thorium deposit's tile and Seats are working ground, in any room" {
                // The deposit stands on a wall, where the mod puts one, so
                // its tile is in the set through the mineral and never
                // through the terrain.
                let atlas =
                    { spatial
                          [ "min-a", { X = 20; Y = 20 } ]
                          [
                              { X = 19; Y = 20 }, Plain
                              { X = 20; Y = 20 }, Wall
                              { X = 21; Y = 20 }, Swamp
                              { X = 20; Y = 19 }, Wall
                          ] with
                        TargetKinds = Map.ofList [ "min-a", Mineral ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (workingGroundIn atlas (atlasHome atlas))
                    (Set.ofList [ { X = 19; Y = 20 }; { X = 20; Y = 20 }; { X = 21; Y = 20 } ])
                    "the deposit and the two Seats it has; the walled neighbour is no Seat"
            }

            test "a deposit's Seat under a container is no Post, and hires no Anchor" {
                let atlas =
                    { spatial
                          [ "min-a", { X = 20; Y = 20 }; "can-min", { X = 19; Y = 20 } ]
                          [ { X = 19; Y = 20 }, Plain; { X = 20; Y = 20 }, Wall ] with
                        TargetKinds =
                            Map.ofList
                                [ "min-a", Mineral; "can-min", Structure BuiltKind.Container ]
                    }
                    |> snapshotWith []
                    |> ofView

                let home = atlasHome atlas

                Expect.isTrue
                    (Set.contains { X = 19; Y = 20 } (workingGroundIn atlas home))
                    "the Seat is ground the Layout may not cluster on"

                Expect.equal (postsIn atlas home) Set.empty "and it is no Post"

                Expect.equal (postCount atlas) 0 "so no Anchor is hired to garrison it"

                // The deposit's Post and the room's Posts are two answers on purpose.
                Expect.equal
                    (tilesHome atlas (postsOf atlas "min-a"))
                    (Set.singleton { X = 19; Y = 20 })
                    "the deposit's own Post is the Seat its container stands on"
            }

            test "a deposit with no container standing has no Post at all" {
                // The pairwise premise is one projection entry.
                let atlasWith kind =
                    { spatial
                          [ "min-a", { X = 20; Y = 20 }; "can-min", { X = 19; Y = 20 } ]
                          [ { X = 19; Y = 20 }, Plain; { X = 20; Y = 20 }, Wall ] with
                        TargetKinds = Map.ofList [ "min-a", Mineral; "can-min", kind ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (postsOf (atlasWith (Structure BuiltKind.Container)) "min-a" |> Set.count)
                    1
                    "the premise: a standing container makes the Post"

                Expect.equal
                    (postsOf (atlasWith (Site BuiltKind.Container)) "min-a")
                    Set.empty
                    "and a site makes none"
            }

            test "a room with neither sources nor a controller works no ground" {
                let atlas =
                    { spatial [ "spawn-1", { X = 10; Y = 10 } ] [ { X = 10; Y = 11 }, Plain ] with
                        TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (workingGroundIn atlas (atlasHome atlas))
                    Set.empty
                    "geometry the projection cannot place answers empty, and nothing is off-limits"
            }
        ]

[<Tests>]
let idleGroundTests =
    testList
        "atlas idleGround"
        [
            test "the idle ground is the working ground plus every store's ring" {
                let atlas = ofView storeRingView
                let home = atlasHome atlas

                let seats =
                    Set.ofList
                        [
                            for dx in -1 .. 1 do
                                for dy in -1 .. 1 do
                                    if (dx, dy) <> (0, 0) then
                                        { X = 10 + dx; Y = 10 + dy }
                        ]

                Expect.equal
                    (workingGroundIn atlas home)
                    seats
                    "the premise: with no controller the Layout's set is the source's Seats"

                Expect.equal
                    (idleGroundIn atlas home)
                    (Set.union
                        seats
                        (Set.ofList
                            [
                                // The source container's ring past the Seats:
                                // the corridor mouth it is drawn from.
                                { X = 12; Y = 10 }
                                // The stock's two standing tiles.
                                { X = 19; Y = 10 }
                                { X = 21; Y = 10 }
                                // And the cluster spawn's.
                                { X = 29; Y = 10 }
                                { X = 31; Y = 10 }
                            ]))
                    "the Seats, and the walkable ring of the container, the Storage and the spawn"
            }

            test "an obstacle store is no tile of its own, and one store's ring is not another's" {
                let atlas = ofView storeRingView
                let ground = idleGroundIn atlas (atlasHome atlas)

                Expect.isFalse
                    (Set.contains { X = 20; Y = 10 } ground)
                    "the Storage's own tile is an obstacle: nothing idles there to be moved off"

                Expect.isFalse
                    (Set.contains { X = 15; Y = 10 } ground)
                    "and a corridor tile between two stores rings neither of them"
            }

            test "widening the mover's set leaves the Layout's where it was" {
                // A Storage's ring inside `workingGroundIn` would move
                // every extension the Layout places.
                let atlas = ofView storeRingView
                let home = atlasHome atlas

                Expect.isFalse
                    (Set.contains { X = 19; Y = 10 } (workingGroundIn atlas home))
                    "the stock's standing tile is not working ground"

                Expect.isTrue
                    (Set.isProperSubset (workingGroundIn atlas home) (idleGroundIn atlas home))
                    "and the mover's set strictly contains the Layout's"
            }

            // A tower tucked against a wall (#277): two walkable
            // neighbours, the shape that jams. No source and no controller,
            // so the idle ground is the stores' rings alone.
            let towerRoom =
                { spatial
                      [ "tow-1", { X = 10; Y = 10 } ]
                      [
                          { X = 10; Y = 10 }, Plain
                          { X = 10; Y = 11 }, Plain
                          { X = 11; Y = 10 }, Plain
                      ] with
                    TargetKinds = Map.ofList [ "tow-1", Structure BuiltKind.Tower ]
                }
                |> withObstacles [ { X = 10; Y = 10 } ]

            test "a wall-tucked tower rings the idle ground like every other store" {
                // #268's enumeration named the cluster and held the tower
                // out, leaving its ring as ground for an idle body to park on.
                let view =
                    { snapshotWith [] towerRoom with
                        Refillables =
                            [
                                {
                                    Id = "tow-1"
                                    FreeCapacity = 300
                                    Kind = BuiltKind.Tower
                                }
                            ]
                    }

                let atlas = ofView view

                Expect.equal
                    (idleGroundIn atlas (atlasHome atlas))
                    (Set.ofList [ { X = 10; Y = 11 }; { X = 11; Y = 10 } ])
                    "the tower's two walkable neighbours, and its own obstacle tile is not one"
            }

            test "a structure of somebody else's rings nothing of ours" {
                // `FIND_STRUCTURES` carries every owner's, so an abandoned
                // room's tower is in the kind census with no Refill of ours.
                let atlas = ofView (snapshotWith [] towerRoom)

                Expect.equal
                    (idleGroundIn atlas (atlasHome atlas))
                    Set.empty
                    "a tower the colony does not fill is no store of this colony's"
            }

            test "a room with no working ground and no store idles anywhere" {
                let atlas =
                    spatial [] [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (idleGroundIn atlas (atlasHome atlas))
                    Set.empty
                    "no geometry, no stores, nothing an idle body has to step off (ADR 0004)"
            }
        ]

[<Tests>]
let consistencyTests =
    testList
        "atlas consistency"
        [
            test "travelCost, firstStep, workArea and mayAct agree from every standing tile" {
                // Seats on plain and swamp, a dead lane, an obstacle, an
                // unreachable island; one creep on every standing tile in turn.
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
                    |> withObstacles [ { X = 11; Y = 12 } ]

                let standing =
                    (homeLayer projection).Terrain
                    |> TerrainGrid.toList
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
                        |> withCreepsAt [ "w", pos ]
                        |> snapshotWith [ worker "w" ]
                        |> ofView

                    let area = workArea atlas task
                    let cost = travelCost atlas "w" task
                    let step = firstStep atlas "w" task area |> tileHome atlas
                    let tiles = area |> tilesHome atlas

                    if Set.contains pos tiles then
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
                                (adjacentWalkableIn atlas (atlasHome atlas) pos)
                                s
                                $"the step is an adjacent standing tile at {pos}"
                        | None, None -> () // unreachable: inapplicable, stationary
                        | c, s -> failtest $"cost {c} and step {s} disagree at {pos}"
            }

            test "a Harvest Work Area is the source's Seats minus obstacle-blocked tiles" {
                // Two terrain Seats, one under an obstacle.
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

                let area = workArea atlas (Harvest "src-a") |> tilesHome atlas

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
let keeperMaskTests =
    testList
        "atlas keeper mask"
        [
            let centres = Keepers.centresIn "W15S26"
            let margin = Tuning.keeperMargin Tuning.defaults
            // One Atlas per case: Expecto runs these in parallel (#310).
            let masked () = keeperRoom |> snapshotWith [] |> ofView

            test "every tile within the margin of a declared rock is off the walkable ground" {
                // The ground under this room is plain everywhere, so every
                // missing tile is the mask's doing.
                let walkable = walkableTilesIn (masked ()) "W15S26"

                Expect.isNonEmpty centres "W15S26's rocks are declared"

                let inside =
                    walkable
                    |> Set.filter (fun tile ->
                        centres |> List.exists (fun centre -> range centre tile <= margin))

                Expect.isEmpty
                    inside
                    "no walkable tile of a keeper room lies within the margin of one of its rocks"

                Expect.isTrue
                    (walkable
                     |> Set.exists (fun tile ->
                         centres |> List.exists (fun centre -> range centre tile = margin + 1)))
                    "and the tile one step past the margin is ground, so the mask is a margin and not the room"
            }

            test "a masked tile is impassable to the grid, the ring and the ground alike" {
                // The mask goes on the raw ground before the walking grid
                // is copied from it, so every query answers alike.
                let lair = { X = 35; Y = 11 }
                let ringTile = { X = 0; Y = 20 }

                Expect.isTrue
                    (List.contains lair centres)
                    "the north-east lair is a declared centre"

                Expect.equal
                    (stepWeights (masked ()) "W15S26").[lair.X * 50 + lair.Y]
                    -1
                    "the rock itself is not a tile a body steps onto"

                Expect.isEmpty
                    (adjacentWalkableIn (masked ()) "W15S26" lair)
                    "and neither is anything beside it"

                Expect.equal
                    (seats (masked ()) "sk-src-0")
                    (Some 0)
                    "and a declared rock inside the mask counts no Seat, a Seat being counted off terrain alone (ADR 0001)"

                Expect.isTrue
                    (Keepers.masked margin "W15S26" ringTile)
                    "(0,20) is within six of the west lair at (6,17)"

                Expect.isEmpty
                    (seams (masked ()) "W15S26" "W16S26"
                     |> List.filter (fun (here, _) -> here = ringTile))
                    "so it is no crossing either, though the ring carries plain terrain for it"
            }

            test "a room the declaration names none of keeps every tile of its ground" {
                // W15S27 carries the identical invented ground and loses nothing.
                Expect.isEmpty (Keepers.centresIn "W15S27") "W15S27 declares no keeper rock"

                Expect.equal
                    (walkableTilesIn (masked ()) "W15S27" |> Set.count)
                    (48 * 48)
                    "every tile of the plain window is ground"

                Expect.isLessThan
                    (walkableTilesIn (masked ()) "W15S26" |> Set.count)
                    (48 * 48)
                    "and the keeper room's is short by the mask"
            }

            test "the bulk mask and the per-tile rule are the same rule" {
                // The grids are laid from `maskedTilesIn`; the route search
                // asks `masked` per ring tile.
                let bulk = Keepers.maskedTilesIn margin "W15S26" |> Set.ofList

                let pointwise =
                    Set.ofList
                        [
                            for x in 0..49 do
                                for y in 0..49 do
                                    if Keepers.masked margin "W15S26" { X = x; Y = y } then
                                        { X = x; Y = y }
                        ]

                Expect.equal bulk pointwise "one rule, two shapes"

                Expect.isEmpty
                    (bulk
                     |> Set.filter (fun tile ->
                         tile.X < 0 || tile.X > 49 || tile.Y < 0 || tile.Y > 49))
                    "and the bulk form is clamped to the grid it indexes"
            }
        ]
