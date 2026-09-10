/// The ground a body works and idles on: Work Areas narrowed to a creep,
/// Dual Seats, Posts, and the consistency between them.
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
                |> withCreepsAt creeps

            let anchor name =
                creepWith name 0 [ Work; Work; Carry; Move ]

            test "a Work-heavy body harvests a posted source from its Posts alone" {
                let atlas =
                    posted [ "a", { X = 10; Y = 11 } ] |> snapshotWith [ anchor "a" ] |> ofView

                Expect.equal
                    (workAreaFor atlas "a" (Harvest "src-a") |> tilesHome atlas)
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 } ])
                    "the container Seat and the Dual Seat, not the plain Seat"

                Expect.equal
                    (workArea atlas (Harvest "src-a") |> tilesHome atlas)
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 }; { X = 10; Y = 11 } ])
                    "the body-blind area keeps every Seat"
            }

            test "a light body keeps the Seats beyond the Posts of the same source" {
                // ADR 0051: the complement of the heavy narrowing. The Post
                // is the garrison's tile, so a light body's area is every
                // other Seat.
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
                // engine lets it stay, and it keeps working (ADR 0004).
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
                    |> ofView

                Expect.equal
                    (dualSeatsIn atlas (atlasHome atlas))
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
                    |> withObstacles [ { X = 11; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (dualSeatsIn atlas (atlasHome atlas))
                    Set.empty
                    "an unstandable Seat is no Dual Seat"
            }

            test "a room without a controller has no Dual Seats" {
                let atlas =
                    { spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 11; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "src-a", Source ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (dualSeatsIn atlas (atlasHome atlas))
                    Set.empty
                    "no Upgrade Work Area to intersect"
            }

            test "a room without sources has no Dual Seats" {
                let atlas =
                    { spatial [ "ctrl-1", { X = 13; Y = 10 } ] [ { X = 12; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "ctrl-1", Controller ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal (dualSeatsIn atlas (atlasHome atlas)) Set.empty "no Seats to intersect"
            }

            test "a source out of upgrade range yields an empty, harmless answer" {
                let atlas =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 40; Y = 40 } ]
                          [ { X = 11; Y = 10 }, Plain; { X = 39; Y = 40 }, Plain ] with
                        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (dualSeatsIn atlas (atlasHome atlas))
                    Set.empty
                    "a disjoint intersection is just empty"
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
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    (Set.ofList [ { X = 9; Y = 10 }; { X = 11; Y = 10 } ])
                    "the Dual Seat and the container Seat are both Posts"

                Expect.equal
                    (dualSeatsIn atlas (atlasHome atlas))
                    (Set.singleton { X = 11; Y = 10 })
                    "dualSeats is untouched by the container"
            }

            test "a container construction site is a Post, and no standing one" {
                // #205 inverts the rule this test used to pin. A pending
                // container catches no overflow and pays no haul term, and
                // that is what `standingPostsOf` still answers; but the
                // Seat under it is a tile worth garrisoning all the same,
                // because the body that stands there digs the rock beside
                // it and spends what it digs into the site under its feet.
                // The two questions the census used to answer at once —
                // where a heavy body stands, and what a source is worth to
                // the quotas — are what the split keeps apart.
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
                // The trap #205 names, and the reason the site half joins
                // through the Seats rather than through the site's range:
                // a container going up two tiles from the rock — the
                // controller's own buffer, or a neighbouring source's Post
                // — is nobody's garrison, and counting it would hire an
                // Anchor for a tile it cannot dig from.
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
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    Set.empty
                    "only a Seat under a container is a Post"
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
                    |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
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
                // ADR 0022's exclusion is what pushes every clustered pick a
                // ring out, so #268's wider set is a second function and never
                // this one: a Storage's ring inside `workingGroundIn` would
                // move every extension the Layout places.
                let atlas = ofView storeRingView
                let home = atlasHome atlas

                Expect.isFalse
                    (Set.contains { X = 19; Y = 10 } (workingGroundIn atlas home))
                    "the stock's standing tile is not working ground"

                Expect.isTrue
                    (Set.isProperSubset (workingGroundIn atlas home) (idleGroundIn atlas home))
                    "and the mover's set strictly contains the Layout's"
            }

            // A [[tower]] tucked against a wall (#277): its own tile an
            // obstacle as the engine has it, and two walkable neighbours,
            // which is the shape that jams — both taken, and the hauler
            // holding its Refill never reaches range 1. No source and no
            // controller, so the working ground is empty and whatever the
            // idle ground holds is the stores' rings alone.
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
                // #277: ADR 0010 pools a Refill on a tower, so a hauler
                // holding one queues on its range-1 ring exactly as the
                // Storage's does — and #268's enumeration named the cluster
                // and held the tower out, which left that ring reading as
                // ordinary ground for an idle body to park on.
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
                // The census this set is read off is the view's Refillables —
                // ours (#277). `FIND_STRUCTURES` carries every owner's, so an
                // abandoned room's tower stands in the kind census with no
                // Refill of ours ever pooled on it, and no body of ours ever
                // queues at it.
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
                    |> withObstacles [ { X = 11; Y = 12 } ]

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
