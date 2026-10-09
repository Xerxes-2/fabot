/// Bodies on the move under fire in the arena: one shot on its way home, a
/// melee chase over swamp, a hauler walking off its store, and a harassment
/// room's pair under its stand-down.
module Fabot.Core.Tests.ArenaChaseTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

// ---- arena capture: W17S26 t1,012,444 (#608) ---------------------------------
// `npm run capture -- W17S26 1012444 --rooms W17S25`, read at live t1,012,655.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1012400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1012400.json
// W17S26's reservation, 8 ruins and 2 tombstones are not placed. W17S25's
// ramparts are laid without their decay clocks. The Transition log no longer
// held the reserver's, the anchor's or the guard's Task.

/// The live tiles of Trepidimous' Shadow2363 (3 RANGED_ATTACK 4 HEAL 7 MOVE)
/// from t1,012,445, in runs that never revisit a tile (`Waypoints` takes the
/// latest match): in from the west, round the swamp at (10,22), after the
/// reserver to (18,15), then after the anchor up the road and west along
/// y = 1.
let private shadowLive: Behaviour =
    let run tiles =
        Waypoints [ for x, y in tiles -> RoomPos.at "W17S26" { X = x; Y = y } ]

    Phases
        [
            30,
            run
                [
                    1, 32
                    2, 33
                    3, 33
                    4, 33
                    5, 32
                    6, 31
                    7, 30
                    8, 30
                    9, 30
                    10, 29
                    11, 28
                    12, 27
                    11, 26
                    11, 25
                    11, 24
                    11, 23
                    10, 22
                ]
            46,
            run
                [
                    11, 23
                    12, 23
                    13, 23
                    14, 22
                    15, 21
                    16, 20
                    17, 20
                    18, 19
                    18, 18
                    18, 17
                    19, 16
                    19, 15
                    18, 15
                ]
            88,
            run
                [
                    19, 16
                    20, 16
                    21, 15
                    22, 14
                    23, 13
                    24, 12
                    24, 11
                    25, 10
                    26, 9
                    27, 8
                    26, 7
                    25, 6
                    24, 5
                    23, 4
                    22, 3
                    21, 2
                    20, 1
                    19, 1
                    18, 1
                    17, 1
                    16, 1
                    15, 1
                    14, 1
                ]
            120, run [ 15, 1; 14, 0 ]
        ]

/// W17S26 at t1,012,444 (#608): Shadow2363 walks in from the west. Live,
/// reserver-1012111 (2 CLAIM 2 MOVE) reserved from the swamp at (9,22) until
/// the shooter stood five off, stepped onto the swamp at (10,22) (t1,012,465)
/// and was shot there four ticks; it ran east and north and stepped from
/// (18,15) onto the swamp at (19,14) with the shooter three off (t1,012,484),
/// where it died held (t1,012,490). anchor-1012148 (6 WORK 1 CARRY 3 MOVE)
/// left its container at t1,012,495, walked the road to the exit road's
/// (20,1), then west along y = 1 beside the exits home, the shooter on its
/// heels, and died at (13,1) (t1,012,532) without crossing.
let private w17s26ShotHome (shadow: Behaviour) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_700 250_000 { X = 27; Y = 9 } with
                    Energy = 648
                }
            ]
        |> withRoads
            [
                20, 1, 2_700
                21, 2, 4_500
                22, 3, 2_200
                23, 4, 2_700
                24, 5, 4_400
                25, 6, 2_900
                26, 7, 4_600
                27, 8, 2_700
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 41_999
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        9, 23
                        9, 25
                        9, 27
                        9, 29
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 23
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 23
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        16, 32
                        17, 29
                        17, 31
                        18, 30
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                yield
                    { structureOf "container" None 160_800 250_000 { X = 16; Y = 34 } with
                        Energy = 1_493
                    }
                yield
                    { structureOf "container" None 165_000 250_000 { X = 17; Y = 20 } with
                        Energy = 36
                    }
                for x, y in
                    [
                        2, 15
                        2, 16
                        2, 17
                        2, 18
                        2, 19
                        2, 20
                        2, 23
                        2, 24
                        2, 25
                        2, 26
                        2, 27
                        11, 29
                        13, 27
                        13, 29
                        14, 28
                        16, 44
                        17, 20
                        17, 44
                        18, 44
                        32, 31
                        32, 32
                    ] -> structureOf "rampart" (Some Side.Ours) 99_000 10_000_000 { X = x; Y = y }
                yield
                    { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                        Energy = 10_170
                    }
                yield towerOf Side.Ours { X = 11; Y = 29 } 1_000
                yield towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 4_100
                2, 17, 4_100
                3, 18, 4_100
                4, 19, 2_500
                5, 20, 4_100
                6, 21, 2_900
                7, 22, 15_900
                8, 23, 15_300
                9, 24, 14_600
                9, 26, 20_500
                9, 28, 21_000
                10, 23, 20_000
                10, 25, 15_100
                10, 27, 17_800
                10, 29, 19_200
                11, 24, 20_100
                11, 26, 14_400
                11, 28, 16_600
                11, 30, 2_700
                12, 23, 18_800
                12, 25, 13_900
                12, 27, 16_700
                12, 29, 4_100
                12, 31, 4_000
                13, 24, 3_300
                13, 26, 4_000
                13, 28, 3_600
                13, 30, 4_000
                14, 23, 3_800
                14, 25, 4_400
                14, 27, 4_300
                14, 29, 4_100
                14, 31, 3_900
                15, 22, 3_500
                15, 28, 4_000
                15, 30, 2_900
                15, 32, 3_900
                16, 21, 4_200
                16, 29, 3_200
                16, 31, 4_100
                16, 33, 2_800
                16, 35, 3_700
                16, 37, 3_200
                16, 38, 19_000
                16, 39, 21_800
                16, 41, 4_100
                16, 43, 3_800
                16, 44, 3_000
                17, 20, 3_100
                17, 30, 4_300
                17, 32, 3_600
                17, 34, 4_300
                17, 36, 2_500
                17, 38, 2_300
                17, 39, 20_000
                17, 40, 15_600
                17, 42, 4_300
                17, 45, 4_300
                18, 29, 4_100
                18, 31, 2_700
                18, 35, 3_200
                18, 37, 2_400
                18, 39, 3_200
                18, 41, 4_800
                18, 46, 3_000
                19, 30, 3_800
                19, 34, 4_200
                19, 36, 3_700
                19, 38, 3_200
                19, 40, 3_100
                19, 47, 2_900
                20, 31, 3_800
                20, 33, 4_200
                20, 35, 3_100
                20, 37, 2_600
                20, 39, 3_200
                20, 48, 2_400
                21, 32, 4_100
                21, 34, 3_800
                21, 36, 3_200
                21, 38, 3_800
                22, 33, 2_600
                22, 35, 3_300
                22, 37, 2_700
                23, 34, 2_500
                23, 36, 3_100
                24, 35, 3_100
                25, 34, 1_500
                26, 33, 1_900
                27, 32, 1_600
                28, 31, 18_400
                28, 33, 8_600
                29, 32, 12_900
                29, 34, 20_100
                30, 31, 19_400
                30, 33, 19_400
                31, 32, 19_200
                32, 31, 18_300
                33, 30, 12_640
                33, 32, 19_280
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body
                  "reserver-1012111-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 277
            }
            { body
                  "anchor-1012148-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 1_232
                Energy = 50
            }
            { body
                  "Shadow2363"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W17S26" 1 31)
                  (Some shadow) with
                TicksToLive = 1_459
            }
            { body
                  "guard-1011187-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 17 47)
                  None with
                TicksToLive = 301
            }
            { body
                  "hauler-1011295-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 457
                Energy = 840
            }
            { body
                  "anchor-1011403-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 487
                Energy = 50
            }
            { body
                  "worker-1011790-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 925
                Energy = 306
            }
            { body
                  "upgrader-1012042-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_165
                Energy = 39
            }
            { body
                  "scout-W17S22-1011026"
                  odiodin
                  (parts [ Move, 1 ])
                  (at "W17S25" 13 14)
                  (Some Hold) with
                TicksToLive = 83
            }
        ]

    let a = arena 1_012_445 [ w17s26; w17s25 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "hauler-1011295-Spawn11", "refill:container-16-34"
                            "anchor-1011403-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "worker-1011790-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "upgrader-1012042-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                        ]
                Towered = Set.ofList [ "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_004_791
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_012_444
                                            Expiry = 1_014_298
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

[<Tests>]
let shotHomeTests =
    testList
        "arena a body shot on its way home"
        [
            // #608. Before the fix the capture reproduced live tile for tile:
            // the reserver left (9,22) by the swamp (10,22) at t20 (live
            // t1,012,465), ran east and north into the pocket under the wall
            // at (17,15), its walk home planned through the swamps (19,14)
            // and (20,13) though only its first step was weighed, and from
            // (18,15) stepped onto (19,14) with the shooter three off, struck
            // a tick later there than where it stood but held four ticks:
            // dead at t45 (live t1,012,490). The anchor walked the road to
            // (20,1) by t64 and west along y = 1 beside the exits, the
            // landings ahead of it in the Reach the shooter at its heels
            // carried across, and died at (13,1) at t86 (live t1,012,532).
            // Now the reserver's walk home keeps to the tiles it is on first,
            // east of the wall on the plain, and it crosses home; the anchor
            // crosses at (20,0) the tick it reaches the exit road. Against a
            // shooter that chases the nearest body from t30 the reserver
            // crosses home as well, and the anchor, cut off from the exits,
            // runs south on the plain, alive past its live death.
            test
                "#608 capture t1,012,444: the reserver's walk home keeps off the swamp it is struck on and the anchor crosses home under fire" {
                let home id (trace: TickTrace list) =
                    pathOf id trace
                    |> List.tryFind (fun (_, s) -> s.At.Room = "W17S25")
                    |> Option.map fst

                let reserver = "reserver-1012111-Spawn11"
                let anchor = "anchor-1012148-Spawn11"

                let _, trace = w17s26ShotHome shadowLive |> run 80
                let failure = describe trace

                Expect.isTrue
                    (home reserver trace |> Option.exists (fun t -> t <= 70))
                    $"live tiles: the reserver crosses home by t70\n{failure}"

                Expect.isTrue
                    (home anchor trace |> Option.exists (fun t -> t <= 70))
                    $"live tiles: so does the anchor\n{failure}"

                Expect.isTrue (survived reserver trace) $"live tiles: the reserver lives\n{failure}"
                Expect.isTrue (survived anchor trace) $"live tiles: so does the anchor\n{failure}"

                let chasing =
                    match shadowLive with
                    | Phases(first :: _) -> Phases [ first; 999, Kite(Nearest, Engine.rangedRange) ]
                    | other -> other

                let _, chased = w17s26ShotHome chasing |> run 90
                let failure = describe chased

                Expect.isTrue
                    (home reserver chased |> Option.exists (fun t -> t <= 75))
                    $"chasing: the reserver crosses home by t75\n{failure}"

                Expect.isTrue (survived reserver chased) $"chasing: the reserver lives\n{failure}"

                // Its fallback once ranked the tiles beside it by when they
                // are struck alone: the swamp (31,4), struck later than the
                // plain beside it, held it a dozen ticks under fire.
                Expect.isFalse
                    (pathOf anchor chased
                     |> List.exists (fun (_, s) -> s.At = RoomPos.at "W17S26" { X = 31; Y = 4 }))
                    $"chasing: the anchor never steps onto the swamp (31,4) under fire\n{failure}"

                Expect.isTrue
                    (survived anchor chased)
                    $"chasing: the anchor outlives t87\n{failure}"
            }
        ]

// ---- arena capture guard-swamp-chase -----------------------------------------
// `npm run capture -- W17S26 1015553 --rooms W17S25 --name guard-swamp-chase`, read at live t1,015,755.
// The room-history replay at t1,015,553, the state after that tick ran: arena
// tick 0 runs as t1,015,554 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1015500.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1015500.json
// W17S26 is reserved by us to t1016089: the arena has no reservations.
// not placed: 9 ruin, 3 tombstone (the arena has no drops, tombstones or ruins).
/// W17S25's melee guard (10 MOVE, 10 other parts) chasing Trepidimous's
/// 5W3M miner Viral5422 off W17S26's source (#614).
let private guardSwampChase () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 502
                }
            ]
        |> withRoads
            [
                20, 1, 4_000
                21, 2, 4_100
                22, 3, 4_600
                23, 4, 2_300
                24, 5, 4_000
                25, 6, 2_400
                26, 7, 4_100
                27, 8, 2_300
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 61_681
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_864
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            // Viral5422: live, a step every two ticks south-west off the
            // source, its live tiles to t1,015,567.
            { body
                  "Viral5422"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 25 10)
                  (Some(
                      Waypoints
                          [
                              for x, y in [ 24, 11; 24, 12; 24, 13; 23, 14; 22, 15; 22, 16; 23, 17 ] ->
                                  at "W17S26" x y
                          ]
                  )) with
                TicksToLive = 1_391
                Fatigue = 4
            }
            { body
                  "anchor-1014956-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 931
                Energy = 50
            }
            { body
                  "guard-1014093-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S26" 24 6)
                  None with
                TicksToLive = 98
            }
        ]

    let a = arena 1_015_554 [ w17s26; w17s25 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The guard's from its timeline (matched t1,015,490); the
                // anchor's from the Transition log.
                Assignments =
                    Map.ofList
                        [
                            "anchor-1014956-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "guard-1014093-Spawn11", "guard:W17S26"
                        ]
                ArmedSeen = Map.ofList [ "W17S26", 1_014_874 ]
                Towered = Set.ofList [ "W17S25" ]
            }
    }

[<Tests>]
let guardSwampChaseTests =
    testList
        "arena a melee chase over swamp"
        [
            // #614. Before the fix the capture reproduced live tile for tile:
            // the guard, three off the miner at (26,8), stepped onto the swamp
            // at (25,9) (t2, live t1,015,556) — the chase weighed a step only
            // within one of striking — and stood there held while the miner
            // walked two tiles off; it caught it at t13 and killed it at t17
            // (live t1,015,567 and t1,015,571). Now it steps onto the plain
            // at (26,9), keeps pace, swings from t7 and kills it at t11.
            test
                "#614 capture t1,015,553: the melee guard keeps pace with the fleeing miner on the plain" {
                let guard = "guard-1014093-Spawn11"
                let _, trace = guardSwampChase () |> run 20
                let failure = describe trace

                Expect.isFalse
                    (pathOf guard trace
                     |> List.exists (fun (_, s) -> s.At = RoomPos.at "W17S26" { X = 25; Y = 9 }))
                    $"the guard never steps onto the swamp (25,9)\n{failure}"

                Expect.isEmpty
                    (pathOf "Viral5422" trace
                     |> List.choose (fun (tick, miner) ->
                         pathOf guard trace
                         |> List.tryFind (fun (at, s) ->
                             at = tick
                             && s.Fatigue > 0
                             && RoomPos.range s.At miner.At |> Option.exists (fun r -> r > 1))
                         |> Option.map fst))
                    $"nor is held while the miner is out of its reach\n{failure}"

                Expect.isLessThanOrEqual
                    (diedOn "Viral5422" trace |> Option.defaultValue System.Int32.MaxValue)
                    12
                    $"and kills it by t12 (live t1,015,571, t17)\n{failure}"
            }
        ]

// ---- arena capture w17s26-t1017284 -------------------------------------------
// `npm run capture -- W17S26 1017284 --rooms W17S25`, read at live t1,017,586.
// The room-history replay at t1,017,284, the state after that tick ran: arena
// tick 0 runs as t1,017,285 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1017200.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1017200.json
// W17S26 is reserved by us to t1018610: the arena has no reservations.
// not placed: 9 ruin, 1 tombstone (the arena has no drops, tombstones or ruins).

/// Trepidimous' Wolf6836, an empty 23C23M hauler shot to 2,480, walks past our
/// container (27,9) in W17S26 and out along the road to the west exit (0,6) at
/// t1,017,319, on its live tiles; W17S25's two rangers hold the trespass.
let private w17s26FleeingHauler () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 123_000 250_000 { X = 27; Y = 9 } with
                    Energy = 862
                }
            ]
        |> withRoads
            [
                20, 1, 3_700
                21, 2, 3_700
                22, 3, 4_300
                23, 4, 3_800
                24, 5, 3_600
                25, 6, 3_900
                26, 7, 3_800
                27, 8, 3_800
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 83_424
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        9, 23
                        9, 25
                        9, 27
                        9, 29
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 23
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 23
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        16, 32
                        17, 29
                        18, 30
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 190_900 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 480
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 17; Y = 31 }
                { structureOf "rampart" (Some Side.Ours) 101_021 10_000_000 { X = 2; Y = 15 } with
                    NextDecay = 1_017_378
                }
                { structureOf "rampart" (Some Side.Ours) 100_961 10_000_000 { X = 2; Y = 16 } with
                    NextDecay = 1_017_359
                }
                { structureOf "rampart" (Some Side.Ours) 101_031 10_000_000 { X = 2; Y = 17 } with
                    NextDecay = 1_017_362
                }
                { structureOf "rampart" (Some Side.Ours) 99_731 10_000_000 { X = 2; Y = 18 } with
                    NextDecay = 1_017_367
                }
                { structureOf "rampart" (Some Side.Ours) 100_911 10_000_000 { X = 2; Y = 19 } with
                    NextDecay = 1_017_339
                }
                { structureOf "rampart" (Some Side.Ours) 100_831 10_000_000 { X = 2; Y = 20 } with
                    NextDecay = 1_017_341
                }
                { structureOf "rampart" (Some Side.Ours) 101_021 10_000_000 { X = 2; Y = 23 } with
                    NextDecay = 1_017_366
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 10_000_000 { X = 2; Y = 24 } with
                    NextDecay = 1_017_328
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 10_000_000 { X = 2; Y = 25 } with
                    NextDecay = 1_017_345
                }
                { structureOf "rampart" (Some Side.Ours) 100_561 10_000_000 { X = 2; Y = 26 } with
                    NextDecay = 1_017_360
                }
                { structureOf "rampart" (Some Side.Ours) 100_261 10_000_000 { X = 2; Y = 27 } with
                    NextDecay = 1_017_309
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 10_000_000 { X = 11; Y = 29 } with
                    NextDecay = 1_017_342
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 10_000_000 { X = 13; Y = 27 } with
                    NextDecay = 1_017_359
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 10_000_000 { X = 13; Y = 29 } with
                    NextDecay = 1_017_344
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 10_000_000 { X = 14; Y = 28 } with
                    NextDecay = 1_017_320
                }
                { structureOf "rampart" (Some Side.Ours) 97_621 10_000_000 { X = 16; Y = 44 } with
                    NextDecay = 1_017_308
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 10_000_000 { X = 17; Y = 20 } with
                    NextDecay = 1_017_365
                }
                { structureOf "rampart" (Some Side.Ours) 97_601 10_000_000 { X = 17; Y = 44 } with
                    NextDecay = 1_017_310
                }
                { structureOf "rampart" (Some Side.Ours) 97_401 10_000_000 { X = 18; Y = 44 } with
                    NextDecay = 1_017_306
                }
                { structureOf "rampart" (Some Side.Ours) 91_811 10_000_000 { X = 32; Y = 31 } with
                    NextDecay = 1_017_319
                }
                { structureOf "rampart" (Some Side.Ours) 90_501 10_000_000 { X = 32; Y = 32 } with
                    NextDecay = 1_017_337
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_401
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 3_300
                2, 17, 3_300
                3, 18, 3_300
                4, 19, 3_400
                5, 20, 3_200
                6, 21, 3_800
                7, 22, 20_400
                8, 23, 19_300
                9, 24, 18_600
                9, 26, 17_000
                9, 28, 18_000
                10, 23, 16_500
                10, 25, 19_100
                10, 27, 13_800
                10, 29, 16_200
                11, 24, 16_100
                11, 26, 17_900
                11, 28, 12_600
                11, 30, 3_900
                12, 23, 15_800
                12, 25, 16_900
                12, 27, 20_700
                12, 29, 3_100
                12, 31, 3_400
                13, 24, 4_000
                13, 26, 3_100
                13, 28, 2_700
                13, 30, 3_000
                14, 23, 2_700
                14, 25, 3_900
                14, 27, 3_500
                14, 29, 3_100
                14, 31, 3_000
                15, 22, 4_200
                15, 28, 3_200
                15, 30, 3_900
                15, 32, 2_900
                16, 21, 3_400
                16, 29, 4_200
                16, 31, 3_200
                16, 33, 3_800
                16, 35, 2_900
                16, 37, 2_400
                16, 38, 15_000
                16, 39, 17_800
                16, 41, 3_300
                16, 43, 3_000
                16, 44, 4_000
                17, 20, 2_600
                17, 30, 3_500
                17, 32, 2_800
                17, 34, 3_600
                17, 36, 3_600
                17, 38, 3_600
                17, 39, 17_500
                17, 40, 12_100
                17, 42, 3_700
                17, 45, 3_600
                18, 29, 3_500
                18, 31, 3_900
                18, 35, 2_500
                18, 37, 3_700
                18, 39, 2_700
                18, 41, 4_300
                18, 46, 2_400
                19, 30, 3_200
                19, 34, 3_600
                19, 36, 3_200
                19, 38, 2_700
                19, 40, 2_600
                19, 47, 4_100
                20, 31, 3_300
                20, 33, 3_700
                20, 35, 2_600
                20, 37, 2_100
                20, 39, 2_700
                20, 48, 3_600
                21, 32, 3_500
                21, 34, 3_400
                21, 36, 2_700
                21, 38, 3_300
                22, 33, 3_900
                22, 35, 2_800
                22, 37, 2_200
                23, 34, 3_800
                23, 36, 2_600
                24, 35, 2_600
                25, 34, 4_600
                26, 33, 4_600
                27, 32, 4_600
                28, 31, 15_900
                28, 33, 18_700
                29, 32, 19_400
                29, 34, 17_600
                30, 31, 16_900
                30, 33, 16_900
                31, 32, 16_700
                32, 31, 15_800
                33, 30, 10_140
                33, 32, 16_780
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    // Wolf6836's live tiles t1,017,285-318 (`observe history W17S26`), the
    // last a step off the west exit it crossed at t1,017,319.
    let wolfWalk =
        [
            33, 8
            32, 7
            31, 7
            30, 7
            29, 7
            28, 8
            27, 8
            26, 7
            25, 6
            24, 5
            23, 4
            22, 3
            21, 2
            20, 1
            19, 1
            18, 1
            17, 2
            16, 2
            15, 1
            14, 1
            13, 1
            12, 1
            11, 1
            10, 1
            9, 1
            8, 1
            7, 1
            6, 1
            5, 1
            4, 2
            3, 2
            3, 3
            2, 4
            1, 5
        ]
        |> List.map (fun (x, y) -> at "W17S26" x y)

    let bodies: Body list =
        [
            { body
                  "anchor-1016455-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 699
                Energy = 20
            }
            { body
                  "reserver-1017160-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 486
            }
            // Wolf6836: live 13 CARRY, 12 MOVE; its store empty throughout. It
            // walks round a ranger on its path, as live.
            { body
                  "Wolf6836"
                  trepidimous
                  (List.concat (List.replicate 23 [ Move; Carry ]))
                  (at "W17S26" 33 9)
                  (Some(WalkRound wolfWalk)) with
                Hits = 2_480
                TicksToLive = 1_188
            }
            { body
                  "ranger-1017078-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 32 9)
                  None with
                TicksToLive = 1_328
            }
            { body
                  "ranger-1016532-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 32 8)
                  None with
                TicksToLive = 782
            }
            { body
                  "hauler-1015828-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S26" 17 17)
                  None with
                TicksToLive = 150
                Energy = 46
            }
            { body
                  "worker-1015724-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 19
                Energy = 9
            }
            { body
                  "anchor-1015936-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 174
                Energy = 50
            }
            { body
                  "worker-1016374-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 669
                Energy = 450
            }
            { body
                  "upgrader-1016966-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 19 35)
                  None with
                TicksToLive = 1_249
            }
            { body
                  "hauler-1015964-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 16 29)
                  None with
                TicksToLive = 286
                Energy = 172
            }
        ]

    let a = arena 1_017_285 [ w17s26; w17s25 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t1,017,284,
                // read from Memory at live t1,017,586; the economy bodies it
                // holds none for that early start unassigned.
                Assignments =
                    Map.ofList
                        [
                            "anchor-1016455-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "reserver-1017160-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "ranger-1017078-Spawn11", "guard:W17S26"
                            "ranger-1016532-Spawn11", "guard:W17S26"
                            "worker-1016374-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                        ]
            }
    }

[<Tests>]
let w17s26FleeingHaulerTests =
    testList
        "arena a hauler walking off its store (#619)"
        [
            // Live, the rangers stood on the container's ring t1,017,292-295
            // while Wolf6836 walked past it and on (the ground was waited for at
            // the store within 5 of it, whichever way the hauler walked), then
            // trailed it four to five behind and out of the room; on the light
            // ticks (cadence 2) they replayed a stand as well. The arena decides
            // every tick and does not model light ticks, so it parts from live
            // from t1,017,287 on (live stood, the arena steps); the light tick's
            // half is `LightTickTests`' (`LightForce.Chase`). With the 5-tile
            // store ground the arena still kept live's outcome: both rangers
            // stood on 28,7/29,7 t7-11 and Wolf6836 left at 1,640 (live 1,720).
            test
                "#619 capture t1,017,284: the rangers chase the hauler walking off the container and keep it in reach until it dies or exits" {
                let _, trace = w17s26FleeingHauler () |> run 36
                let failure = describe trace
                let wolf = pathOf "Wolf6836" trace |> Map.ofList
                let rangers = [ "ranger-1017078-Spawn11"; "ranger-1016532-Spawn11" ]

                let inReach tick (quarry: Snapshot) =
                    rangers
                    |> List.exists (fun ranger ->
                        pathOf ranger trace
                        |> List.exists (fun (t, s) ->
                            t = tick
                            && RoomPos.range s.At quarry.At
                               |> Option.exists (fun r -> r <= Engine.rangedRange)))

                // It leaves the container's side at t7 (26,7) and stands a step
                // off the exit from t33.
                for tick in 7..33 do
                    match Map.tryFind tick wolf with
                    | Some quarry ->
                        Expect.isTrue
                            (inReach tick quarry)
                            $"t{tick}: a ranger keeps Wolf6836 in reach\n{failure}"
                    | None -> ()
            }

            // #650: on main one ranger stepped into the swamp at 24,3 at t12
            // and stood five ticks out of reach: its partner took 24,4, the
            // tile its chase stepped round the swamp by, and the arbitration
            // rehoused it onto the swamp, a step no chase weighed. A chaser is
            // shoved only onto the tiles beside it its chase keeps. Wolf6836
            // now falls to 80 hits a step off the exit (560 on main). Along
            // the north edge from x 14 the two walk single file, the second
            // four behind: the terrain's, not the traffic's.
            test
                "#650 capture t1,017,284: neither ranger is shoved onto a tile that holds it out of Wolf6836's reach" {
                let _, trace = w17s26FleeingHauler () |> run 36
                let failure = describe trace
                let wolf = pathOf "Wolf6836" trace |> Map.ofList

                for ranger in [ "ranger-1017078-Spawn11"; "ranger-1016532-Spawn11" ] do
                    let path = pathOf ranger trace |> Map.ofList

                    let held =
                        [ 7..33 ]
                        |> List.filter (fun tick ->
                            match
                                Map.tryFind tick wolf,
                                Map.tryFind (tick - 1) path,
                                Map.tryFind tick path
                            with
                            | Some quarry, Some before, Some body ->
                                body.At = before.At
                                && RoomPos.range body.At quarry.At
                                   |> Option.forall (fun r -> r > Engine.rangedRange)
                            | _ -> false)

                    Expect.isEmpty held $"{ranger} stands out of reach\n{failure}"
            }
        ]

// ---- arena capture w19s25-t1019120 -------------------------------------------
// `npm run capture -- W19S25 1019120 --rooms W17S29`, read at live t1,019,411.
//   W19S25  https://screeps.com/room-history/shardSeason/W19S25/1019100.json
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/1019100.json
// Spawn9 is spawning worker-1019086-Spawn9 until t1019191: not placed.
// not placed: 2 tombstone (the arena has no drops, tombstones or ruins).

/// W17S29's ranger on Flee in W19S25 through the harassment room's stand-down
/// (t1,018,999-1,019,160), Trepidimous' Ballistic8515 beside it and Bang8545
/// two off (#623). Both harassers chase it, as live they did down the west
/// edge.
let private w19s25StandDownPair () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w19s25 =
        room "W19S25"
        |> withSite trepidimous "road" None 0 { X = 9; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 10; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 11; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 12; Y = 37 }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 2_149_412
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        20, 44
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        22, 36
                        22, 38
                        22, 40
                        22, 42
                        23, 37
                        24, 38
                        24, 42
                        26, 44
                        27, 43
                        28, 44
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        21, 45
                        22, 44
                        23, 35
                        23, 43
                        23, 45
                        24, 36
                        24, 44
                        25, 35
                        25, 37
                        25, 39
                        25, 41
                        25, 43
                        25, 45
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                        27, 45
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 195_600 250_000 { X = 13; Y = 29 } with
                    Energy = 552
                }
                structureOf "container" None 143_600 250_000 { X = 15; Y = 43 }
                structureOf "container" None 141_000 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 125_000 250_000 { X = 44; Y = 38 } with
                    Energy = 274
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_601 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 1_019_137
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 1_019_148
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 1_019_183
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 1_019_127
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 1_019_126
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 1_019_132
                }
                { structureOf "rampart" (Some Side.Ours) 100_591 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 1_019_180
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 20_059
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 1_000
                towerOf Side.Ours { X = 23; Y = 41 } 1_000
            ]
        |> withRoads
            [
                13, 29, 4_300
                14, 29, 4_100
                15, 30, 3_000
                16, 31, 3_000
                16, 42, 4_400
                17, 32, 3_300
                17, 42, 21_000
                18, 33, 4_000
                18, 41, 18_000
                19, 34, 12_800
                19, 40, 2_500
                19, 42, 3_800
                20, 35, 17_400
                20, 37, 12_800
                20, 39, 4_300
                20, 41, 4_600
                20, 43, 3_200
                21, 36, 2_700
                21, 38, 3_100
                21, 40, 2_500
                21, 42, 3_400
                21, 44, 3_800
                22, 35, 2_700
                22, 37, 2_800
                22, 39, 3_300
                22, 41, 2_400
                22, 43, 2_870
                22, 45, 4_160
                23, 36, 3_100
                23, 38, 3_100
                23, 42, 4_100
                23, 44, 3_070
                24, 35, 13_300
                24, 37, 3_300
                24, 39, 3_200
                24, 41, 2_700
                24, 43, 3_900
                24, 45, 4_460
                25, 36, 20_400
                25, 38, 3_100
                25, 40, 3_900
                25, 42, 3_100
                25, 44, 4_100
                26, 35, 16_800
                26, 37, 20_800
                26, 39, 15_100
                26, 41, 4_500
                26, 43, 4_200
                26, 45, 3_400
                27, 36, 15_900
                27, 37, 18_400
                27, 38, 21_100
                27, 39, 19_900
                27, 40, 18_700
                27, 41, 3_000
                27, 42, 4_400
                27, 44, 3_600
                28, 36, 16_500
                28, 37, 17_500
                28, 38, 14_300
                28, 39, 19_700
                28, 40, 20_500
                28, 41, 3_000
                28, 43, 4_300
                29, 35, 3_100
                29, 36, 18_000
                29, 37, 20_200
                29, 38, 14_400
                29, 39, 20_700
                29, 40, 18_300
                29, 41, 20_300
                29, 42, 2_600
                30, 34, 3_100
                30, 36, 17_000
                30, 37, 13_500
                30, 40, 22_400
                30, 41, 13_300
                30, 42, 19_900
                31, 33, 2_900
                31, 36, 17_700
                31, 41, 17_700
                31, 42, 18_600
                32, 32, 4_200
                32, 36, 13_700
                32, 41, 4_100
                33, 31, 3_600
                33, 36, 21_000
                33, 37, 16_100
                33, 40, 3_800
                34, 30, 3_800
                34, 39, 3_800
                35, 29, 4_600
                35, 38, 4_300
                36, 28, 3_100
                36, 37, 2_700
                37, 27, 4_400
                37, 38, 4_500
                38, 26, 4_500
                38, 39, 4_300
                39, 25, 4_600
                39, 40, 3_500
                40, 24, 3_000
                40, 41, 4_200
                41, 23, 4_400
                41, 40, 4_400
                42, 24, 2_900
                42, 39, 2_500
                43, 25, 4_560
                43, 38, 2_600
                44, 25, 3_180
                44, 37, 3_600
                44, 38, 4_300
                45, 26, 4_460
                45, 36, 3_200
                46, 26, 3_870
                46, 35, 3_600
                47, 25, 4_270
                47, 36, 3_200
                48, 20, 3_970
                48, 21, 2_970
                48, 22, 4_270
                48, 23, 4_410
                48, 24, 3_070
                48, 37, 2_500
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S29" ])

    let bodies: Body list =
        [
            // Ballistic8515: live: attacked→8,16 rangedAttack→8,16 heal→9,16 healed→10,16
            { body
                  "Ballistic8515"
                  trepidimous
                  (parts [ RangedAttack, 2; Move, 4; Heal, 3; Move, 1 ])
                  (at "W19S25" 8 16)
                  (Some(Chase(Nearest, None))) with
                Hits = 988
                TicksToLive = 923
            }
            // Bang8545: live: heal→9,16 rangedAttack→8,16
            { body
                  "Bang8545"
                  trepidimous
                  (parts [ RangedAttack, 2; Move, 4; Heal, 3; Move, 1 ])
                  (at "W19S25" 9 16)
                  (Some(Follow "Ballistic8515")) with
                TicksToLive = 953
            }
            // ranger-1018280-Spawn9: live: 9 MOVE, 6 RANGED_ATTACK, 3 HEAL
            { body
                  "ranger-1018280-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W19S25" 7 17)
                  None with
                Hits = 1_756
                TicksToLive = 712
            }
            { body
                  "upgrader-1017622-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 32 43)
                  None with
                TicksToLive = 93
            }
            { body
                  "hauler-1017985-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 23 42)
                  None with
                TicksToLive = 498
                Energy = 550
            }
            { body
                  "anchor-1018120-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 522
                Energy = 50
            }
            { body
                  "anchor-1018256-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 658
                Energy = 50
            }
            { body
                  "worker-1018334-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 28 42)
                  None with
                TicksToLive = 817
            }
            { body
                  "upgrader-1018554-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 22 47)
                  None with
                TicksToLive = 1_025
            }
            { body
                  "worker-1018816-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 44)
                  None with
                TicksToLive = 1_299
                Energy = 450
            }
        ]

    let a = arena 1_019_121 [ w19s25; w17s29 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t1,019,120,
                // read from Memory at live t1,019,411; upgrader-1017622-Spawn9
                // has none that early.
                Assignments =
                    Map.ofList
                        [
                            "ranger-1018280-Spawn9", "flee"
                            "hauler-1017985-Spawn9", "refill:spawn-Spawn9"
                            "anchor-1018120-Spawn9", "harvest:6a8caa7ddd4872bccd318c83"
                            "anchor-1018256-Spawn9", "harvest:6a8caa7ddd4872bccd318c81"
                            "worker-1018334-Spawn9", "upgrade:6a8caa7ddd4872bccd318c82"
                            "worker-1018816-Spawn9", "refill:spawn-Spawn9"
                        ]
                // From live Memory at t1,019,411, less what is stamped after
                // the tick.
                Towered = Set.ofList [ "W17S29" ]
                Raids =
                    Map.ofList
                        [
                            "W17S29",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_014_864
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_019_120
                                            Expiry = 1_021_368
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S25"
                                            Opened = 1_018_999
                                            LastSeen = 1_019_110
                                            Expiry = 1_019_160
                                            Basis = StandDownBasis.HarassSighting
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 1_016_566
                                            LastSeen = 1_019_120
                                            Expiry = 1_021_258
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                                HomeRaided = Some 1_016_852
                            }
                        ]
            }
    }

[<Tests>]
let w19s25StandDownPairTests =
    testList
        "arena a harassment room under its stand-down (#623)"
        [
            // Live t1,019,121 the ranger shot Ballistic8515 beside it (60)
            // and never massed (60 + 24) while the pair chased it down the
            // west edge, until the stand-down ended at t1,019,160.
            test
                "#623 capture t1,019,120: the ranger beside one harasser and two off the other mass-attacks, and never shoots one while both stand within two" {
                let _, trace = w19s25StandDownPair () |> run 20
                let failure = describe trace
                let ranger = "ranger-1018280-Spawn9"

                let struck tick =
                    trace
                    |> List.tryFind (fun t -> t.Tick = tick)
                    |> Option.map (fun t ->
                        t.Ours
                        |> List.choose (function
                            | RangedAttackCreep(who, target) when who = ranger -> Some(Some target)
                            | RangedMassAttack who when who = ranger -> Some None
                            | _ -> None))
                    |> Option.defaultValue []

                Expect.equal (struck 0) [ None ] $"t0: the mass attack\n{failure}"

                // Both within two of the ranger after a tick: the next tick's
                // mass deals at least 60 + 24 against a shot's 60.
                for t in trace do
                    let at id =
                        t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map _.At

                    let near id =
                        match at id, at ranger with
                        | Some them, Some us ->
                            RoomPos.range them us |> Option.exists (fun r -> r <= 2)
                        | _ -> false

                    if near "Ballistic8515" && near "Bang8545" then
                        Expect.isFalse
                            (struck (t.Tick + 1) |> List.exists Option.isSome)
                            $"t{t.Tick + 1}: no single shot beside the pair\n{failure}"
            }
        ]

// ---- a dancer on the sweep's border (#658) --------------------------------
// Live after c8a356c7 (W19S26): Trepidimous' 23C23M hauler stood on the
// room's exit, carried across and back every other tick; our ranger walked
// toward it and away, over and over, and never killed it. Here: W12S28
// sweeping W12S27, the hauler on W12S27's north exit (10,0), carried into
// W12S26 and back, nothing of ours seeing W12S26; an eight-block ranger
// standing in W12S27. Nothing casts.

let private dancerAcrossNorth () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let home =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 30_000
                }
            ]

    let ranger = body "ranger-1-Spawn1" Side.Ours (ourRanger 8) (at "W12S27" 14 10) None

    let dancer =
        body
            "dancer"
            (Side.Player "Trepidimous")
            (List.concat (List.replicate 23 [ Move; Carry ]))
            (at "W12S27" 10 0)
            (Some Hold)

    arena 900_000 [ home; room "W12S27"; room "W12S26" ] [ colony "W12S28" ] [ ranger; dancer ]
    |> withHarass
        [
            {
                RoomName = "W12S27"
                Enemy = "Trepidimous"
                Via = []
            }
        ]

/// The sweep's phase after each of `ticks` arena ticks (1-based), an
/// observer of ours looking at W12S27 on the ticks `looks` names and on no
/// other.
let private phasesUnder (looks: int -> bool) (ticks: int) (a: Arena) : (int * OpPhase option) list =
    ((a, []), [ 1..ticks ])
    ||> List.fold (fun (state, phases) k ->
        let next, _ =
            { state with
                Lookout = if looks k then set [ "W12S27" ] else Set.empty
            }
            |> run 1

        next, (k, next.Carried.Operations |> List.tryHead |> Option.map _.Phase) :: phases)
    |> snd
    |> List.rev

[<Tests>]
let borderCampTests =
    testList
        "arena a dancer on the sweep's border is camped"
        [
            // A healer of its owner across the border, beside the tile the
            // dancer lands on, heals it whole each tick it is out (20 HEAL,
            // 240 a tick, against what lands while it is in): no camp pays.
            test
                "with a healer across the border that out-heals what lands, the dancer is not camped" {
                let a = dancerAcrossNorth ()

                let healer =
                    body
                        "healer"
                        (Side.Player "Trepidimous")
                        (parts [ Move, 20; Heal, 20 ])
                        (RoomPos.at "W12S26" { X = 10; Y = 48 })
                        (Some Hold)

                let healed =
                    { a with
                        Bodies = a.Bodies @ [ healer ]
                        Lookout = set [ "W12S26" ]
                    }

                let _, trace = healed |> run 60
                let failure = describe trace

                Expect.isTrue (survived "dancer" trace) $"the dancer stands\n{failure}"

                Expect.isFalse
                    (pathOf "ranger-1-Spawn1" trace
                     |> List.exists (fun (_, s) ->
                         s.At.Room = "W12S27"
                         && max (abs (s.At.X - 10)) s.At.Y <= Engine.rangedRange))
                    $"and the ranger never camps its entry\n{failure}"
            }

            test
                "the ranger stands on one tile covering the dancer's entry and kills it, never walking to and fro" {
                let _, trace = dancerAcrossNorth () |> run 200
                let failure = describe trace

                let killed = diedOn "dancer" trace

                Expect.isSome killed $"the dancer dies\n{failure}"

                // From its first tick on a tile within three of the entry,
                // until the kill: one tile.
                let camped =
                    pathOf "ranger-1-Spawn1" trace
                    |> List.filter (fun (tick, _) -> tick < Option.get killed)
                    |> List.skipWhile (fun (_, s) ->
                        s.At.Room <> "W12S27"
                        || max (abs (s.At.X - 10)) s.At.Y > Engine.rangedRange)
                    |> List.map (fun (_, s) -> s.At)
                    |> List.distinct

                Expect.equal (List.length camped) 1 $"one camp tile, held: {camped}\n{failure}"

                Expect.isTrue
                    (camped |> List.forall (fun tile -> tile.Y > 0))
                    $"off the exit\n{failure}"
            }

            // The camp is the sweep's operation's job (#663 S7c): its ranger
            // holds the operation's Task, committed to the dancer's room.
            test
                "the sweep's camp is its operation's: its ranger holds the operation's Task, committed to the dancer's room" {
                let operating (a: Arena) =
                    a.Carried.Assignments |> Map.exists (fun _ tid -> tid = "op:sweep")

                let working, trace = dancerAcrossNorth () |> runUntil operating 200

                Expect.isTrue (operating working) $"a body holds the sweep's Task\n{describe trace}"

                Expect.equal
                    (working.Carried.Operations |> List.choose _.Room)
                    [ "W12S27" ]
                    "committed to the dancer's room"
            }

            // Live W19S26, t1,061,361-541 (S7c): the sweep flapped execute ->
            // hold -> execute, `Exhausted` and the re-entering rate reading
            // its quarry apart. Two ways, both red here before the fix: the
            // tick the dancer is seen on its exit it read out-healed by no
            // heal at all (nothing against a share of nothing, t3), and once
            // out of sight its odds ran out on the clock and the room read
            // empty with no look finding it so (t103), re-entered on the next
            // look. Here nothing of ours stands in W12S27 and no group holds
            // the sweep: an observer looks on the first ticks and at 251
            // alone, and the dancer is carried across the north exit and back
            // all along. With no look coming its odds are all the room has
            // (the fixup's review): the sweep holds once they run out, and
            // only the look at 251 moves it back.
            test "a dancer bouncing across the border out of sight never flaps the sweep's phase" {
                let a = dancerAcrossNorth ()

                let phases =
                    { a with
                        Bodies = a.Bodies |> List.filter (fun b -> b.Side <> Side.Ours)
                    }
                    |> phasesUnder (fun k -> k <= 3 || k = 251) 300

                let changes =
                    phases
                    |> List.pairwise
                    |> List.filter (fun ((_, before), (_, after)) -> before <> after)
                    |> List.map (fun (_, (k, phase)) -> k, phase)

                Expect.isTrue
                    (phases
                     |> List.forall (fun (k, phase) -> k >= 100 || phase = Some OpPhase.Execute))
                    $"executing while its odds last, no hold on the tick it is seen on its exit: {phases}"

                Expect.isLessThanOrEqual
                    (changes |> List.filter (fun (k, _) -> k < 251) |> List.length)
                    1
                    $"one hold at most before the look, never back on the next tick: {changes}"
            }

            // The other side of the rule, with no group: no look is coming,
            // so the sweep holds as the miner's odds run out, never before,
            // and a look that finds the room empty keeps it held.
            test
                "with no group and no look coming, the sweep holds as its quarry's odds run out, never before, and stays held" {
                let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

                let miner =
                    body
                        "miner"
                        (Side.Player "Trepidimous")
                        (parts [ Move, 5; Work, 5 ])
                        (at "W12S27" 14 10)
                        (Some(GoTo(at "W12S26" 10 47)))

                let a = dancerAcrossNorth ()

                let phases =
                    { a with Bodies = [ miner ] } |> phasesUnder (fun k -> k <= 3 || k = 250) 260

                let holding =
                    phases
                    |> List.filter (fun (_, phase) -> phase = Some OpPhase.Hold)
                    |> List.map fst

                Expect.isNonEmpty holding $"it holds: {phases}"

                Expect.isGreaterThanOrEqual
                    (List.min holding)
                    100
                    $"not before its odds run out, the miner gone on the quiet: {phases}"

                Expect.isTrue
                    (phases
                     |> List.forall (fun (k, phase) ->
                         k < List.min holding || phase = Some OpPhase.Hold))
                    $"and held from then on, the look at 250 finding the room empty: {phases}"
            }

            // The same review: the dancer with a healer across the border
            // that out-heals the camp, played under the operation. The camp
            // never pays, so the sweep holds, and never flaps back on a price
            // its guard refused.
            test "a dancer its healer keeps from the camp: the sweep holds, and stays held" {
                let a = dancerAcrossNorth ()

                let healer =
                    body
                        "healer"
                        (Side.Player "Trepidimous")
                        (parts [ Move, 20; Heal, 20 ])
                        (RoomPos.at "W12S26" { X = 10; Y = 48 })
                        (Some Hold)

                // The healer in sight all along, as the camp test above.
                let healed =
                    { a with
                        Bodies = a.Bodies @ [ healer ]
                        Lookout = set [ "W12S26" ]
                    }

                let phases =
                    ((healed, []), [ 1..60 ])
                    ||> List.fold (fun (state, phases) k ->
                        let next, _ = run 1 state

                        next,
                        (k, next.Carried.Operations |> List.tryHead |> Option.map _.Phase)
                        :: phases)
                    |> snd
                    |> List.rev

                let held = phases |> List.skipWhile (fun (_, phase) -> phase <> Some OpPhase.Hold)

                Expect.isNonEmpty held $"it holds: {phases}"

                Expect.isTrue
                    (held |> List.forall (fun (_, phase) -> phase = Some OpPhase.Hold))
                    $"and never executes again: {phases}"
            }

            // #615, W17S26 replay t1,052,656-685 (our rangers held d4-7 off
            // Trepidimous' empty haulers Midnight and Onyx): rebuilt off the
            // replay, each hauler walking its live tiles a tick at a time.
            // Not a pinned capture: the arena runs no light ticks and the
            // capture's Transition log was stale, so it does not keep the
            // live tiles; it pins the mechanism live showed. Midnight walks
            // in to the container at (27,9) and stands at (28,8); the kill's
            // ground there is the store ring (#619), and ranger-1051197 stood
            // on it three off Midnight. The exchange seats it beside.
            test
                "W17S26 replay t1,052,656: a ranger after an empty hauler standing at the store stands beside it" {
                let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
                let trepidimous = Side.Player "Trepidimous"

                let live (tiles: (int * int * int) list) =
                    Phases [ for k, x, y in tiles -> k, GoTo(at "W17S26" x y) ]

                let midnight =
                    live
                        [
                            1, 40, 3
                            2, 40, 4
                            3, 41, 5
                            4, 40, 6
                            5, 40, 7
                            6, 39, 8
                            7, 40, 7
                            8, 39, 6
                            9, 38, 5
                            10, 37, 4
                            11, 36, 3
                            12, 35, 2
                            13, 34, 2
                            14, 33, 2
                            15, 32, 2
                            16, 31, 2
                            17, 30, 3
                            18, 29, 4
                            19, 28, 5
                            20, 27, 6
                            21, 26, 7
                            22, 27, 8
                            28, 28, 8
                            29, 29, 7
                            30, 30, 7
                            31, 29, 7
                            32, 30, 6
                            33, 31, 7
                            34, 32, 8
                            35, 33, 9
                            36, 33, 10
                            37, 33, 11
                            38, 33, 10
                            39, 33, 9
                            40, 32, 8
                        ]

                let onyx =
                    live
                        [
                            1, 26, 7
                            2, 27, 8
                            3, 26, 7
                            4, 25, 6
                            5, 24, 5
                            6, 23, 4
                            7, 22, 3
                            8, 21, 2
                            9, 20, 1
                            10, 19, 1
                            12, 18, 1
                            14, 17, 1
                            16, 16, 1
                            23, 19, 1
                            24, 20, 1
                            25, 21, 2
                            26, 22, 3
                            27, 23, 4
                            28, 24, 5
                            29, 25, 6
                            30, 26, 7
                            31, 27, 8
                            32, 28, 7
                            33, 29, 7
                            34, 30, 7
                            35, 31, 8
                            36, 32, 9
                            37, 32, 10
                            38, 32, 9
                            39, 31, 8
                            40, 30, 7
                        ]

                let w17s26 =
                    room "W17S26"
                    |> withStructures
                        [
                            { structureOf "container" None 145_000 250_000 { X = 27; Y = 9 } with
                                Energy = 1_041
                            }
                        ]
                    |> withRoads
                        [
                            20, 1, 4_100
                            21, 2, 4_100
                            22, 3, 2_400
                            23, 4, 4_200
                            24, 5, 2_400
                            25, 6, 2_500
                            26, 7, 2_400
                            27, 8, 4_000
                        ]

                let w17s25 =
                    room "W17S25"
                    |> withController Ownership.Ours None 5 3
                    |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
                    |> withStructures
                        [
                            { structureOf
                                  "storage"
                                  (Some Side.Ours)
                                  10_000
                                  10_000
                                  { X = 13; Y = 27 } with
                                Energy = 10_597
                            }
                            towerOf Side.Ours { X = 11; Y = 29 } 1_000
                            towerOf Side.Ours { X = 13; Y = 29 } 1_000
                        ]

                let colonies =
                    Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

                let hauler parts =
                    List.concat (List.replicate parts [ Move; Carry ])

                let bodies: Body list =
                    [
                        { body
                              "ranger-1051197-Spawn11"
                              Side.Ours
                              (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                              (at "W17S26" 35 1)
                              None with
                            TicksToLive = 76
                        }
                        { body
                              "ranger-1052323-Spawn11"
                              Side.Ours
                              (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                              (at "W17S26" 33 1)
                              None with
                            TicksToLive = 1_184
                        }
                        { body
                              "Midnight2077"
                              trepidimous
                              (hauler 23)
                              (at "W17S26" 40 2)
                              (Some midnight) with
                            Hits = 3_628
                        }
                        { body "Onyx2239" trepidimous (hauler 13) (at "W17S26" 25 6) (Some onyx) with
                            Hits = 2_420
                        }
                        { body
                              "anchor-1052591-Spawn11"
                              Side.Ours
                              (parts [ Work, 6; Carry, 1; Move, 3 ])
                              (at "W17S26" 27 9)
                              None with
                            Energy = 12
                        }
                    ]

                let a = arena 1_052_656 [ w17s26; w17s25 ] colonies bodies

                let a =
                    { a with
                        Carried =
                            { a.Carried with
                                Assignments =
                                    Map.ofList
                                        [
                                            "ranger-1051197-Spawn11", "guard:W17S26"
                                            "ranger-1052323-Spawn11", "guard:W17S26"
                                            "anchor-1052591-Spawn11",
                                            "harvest:6a8caa7ddd4872bccd318c78"
                                        ]
                            }
                    }

                let _, trace = run 30 a
                let failure = describe trace
                let ranger = pathOf "ranger-1051197-Spawn11" trace |> Map.ofList
                let midnight = pathOf "Midnight2077" trace

                // Midnight's ticks standing at the container's (28,8) after
                // its first there.
                let standing =
                    midnight
                    |> List.pairwise
                    |> List.filter (fun ((_, before), (_, now)) ->
                        before.At = now.At && now.At = at "W17S26" 28 8)
                    |> List.map (fun (_, (tick, now)) -> tick, now.At)

                Expect.isNonEmpty
                    standing
                    $"the premise: Midnight stands at the container\n{failure}"

                // Its first tick there the ranger is still walking in (two
                // off); from the next, beside it (main: three off).
                for tick, quarry in List.tail standing do
                    Expect.equal
                        (Map.tryFind tick ranger |> Option.bind (fun s -> RoomPos.range s.At quarry))
                        (Some 1)
                        $"t{tick}: the ranger beside Midnight\n{failure}"
            }
        ]

// ---- arena capture: the sweep's room over W19S25/W19S26, t1,068,240 (#663) --
// `npm run capture -- W19S26 1068292 --rooms W19S25,W17S29,W18S27,W17S25`,
// read at live t1,069,178, started 52 ticks earlier off `observe history`
// (t1,068,240-342) so the quarry memory holds Bolt7991's dance first.
//   W19S26  https://screeps.com/room-history/shardSeason/W19S26/1068200.json
//   W19S25  https://screeps.com/room-history/shardSeason/W19S25/1068200.json
// Laid by hand from the skeleton: the homes W17S29 (Spawn9, which cast the
// group) and W17S25 bare of all but controller, spawn, storage and towers,
// their economy bodies left out; the rooms between them and the candidates
// laid empty, so the Caster finds its hops; W18S26 left out, as live it is
// rival-held. Talon8086 (2CLAIM, leaving west) and Nova8122 (1W1C1M) left
// out. Every body walks its live tiles a tick at a time, ours too: the
// pipeline decides the sweep's room off the live walk. On main the latch
// moved sixteen times between t1,068,284 and t1,068,330, at t1,068,316,
// 317, 322 and 328 on the live ticks (Memory.fabot.ops.events), the rest a
// tick to three off.

/// A body's live tiles, one an arena tick from tick 0 on: a room's name
/// stands before the tiles in it.
let private livePath (path: string) : Behaviour =
    path.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold
        (fun (room, tiles) token ->
            match token.Split ',' with
            | [| x; y |] -> room, RoomPos.at room { X = int x; Y = int y } :: tiles
            | _ -> token, tiles)
        ("", [])
    |> snd
    |> List.rev
    |> List.mapi (fun k tile -> k + 1, GoTo tile)
    |> Phases

let private w19s25SweepLatch () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let hauler = List.concat (List.replicate 23 [ Move; Carry ])

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 3_617_636
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 25_294
                }
                towerOf Side.Ours { X = 21; Y = 39 } 1_000
                towerOf Side.Ours { X = 23; Y = 41 } 1_000
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 676_665
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_106
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let empty =
        [
            "W19S26"
            "W19S25"
            "W18S27"
            "W18S25"
            "W17S26"
            "W17S27"
            "W17S28"
            "W18S28"
            "W18S29"
            "W19S27"
            "W19S28"
            "W19S29"
        ]
        |> List.map room

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W17S25" ])

    // Oblivion7691 on its three tiles beside the source, two ticks each.
    let oblivion =
        Phases
            [
                for k in 1..102 ->
                    k,
                    GoTo(
                        List.item
                            (((k - 1) / 2) % 3)
                            [ at "W19S25" 26 21; at "W19S25" 26 22; at "W19S25" 27 22 ]
                    )
            ]

    let bodies =
        [
            { body
                  "guard-1067926-Spawn9"
                  Side.Ours
                  (parts [ Tough, 1; Move, 4; Attack, 3; Heal, 1; Move, 1 ])
                  (at "W19S26" 28 36)
                  (Some(
                      livePath (
                          "W19S26 29,35 30,34 31,33 32,32 33,31 34,30 33,29 32,28 31,27 30,26 29,25 29,24 "
                          + "30,23 30,22 31,21 32,20 33,19 34,18 33,18 32,18 31,17 31,17 31,17 31,17 31,17 "
                          + "30,16 30,16 29,15 28,14 27,13 26,12 25,11 24,10 23,11 24,12 24,13 24,12 23,11 "
                          + "23,10 23,9 23,8 23,7 22,6 23,5 24,4 25,3 26,2 27,1 26,1 25,1 24,1 23,1 22,1 21,1 "
                          + "22,1 23,1 24,1 25,1 26,1 27,1 26,1 25,1 24,1 23,1 22,1 21,1 22,1 23,1 24,1 25,1 "
                          + "26,1 27,1 26,1 25,1 24,1 23,1 22,1 23,1 24,1 25,1 26,1 25,1 24,1 23,1 24,1 25,1 "
                          + "26,1 25,1 24,1 23,1 22,1 21,1 20,1 19,1 18,1 17,1 16,1 15,1 14,1 15,0 W19S25 "
                          + "15,48 15,48"
                      )
                  )) with
                TicksToLive = 1_214
            }
            { body
                  "ranger-1067770-Spawn9"
                  Side.Ours
                  (parts [ Move, 8; RangedAttack, 6; Heal, 3; Move, 1 ])
                  (at "W19S25" 29 48)
                  (Some(
                      livePath (
                          "W19S25 30,47 31,47 32,48 33,47 34,47 35,48 34,47 33,47 32,46 31,46 30,47 29,48 "
                          + "30,47 31,47 32,48 33,47 34,47 35,48 34,47 33,47 32,46 31,46 30,47 29,48 30,47 "
                          + "31,46 32,46 33,47 34,47 35,48 34,47 33,47 32,46 32,47 33,48 33,48 34,49 W19S26 "
                          + "34,1 34,1 35,1 36,2 37,3 37,3 37,3 37,3 38,2 39,1 40,2 39,1 38,1 37,1 36,1 35,1 "
                          + "34,1 35,1 36,1 37,2 36,3 37,4 38,5 37,4 36,3 35,2 34,1 33,1 32,1 33,1 34,1 35,1 "
                          + "36,2 37,3 38,4 37,3 36,2 35,1 34,1 33,1 34,1 35,1 36,1 37,1 36,1 35,1 34,1 35,1 "
                          + "36,1 37,1 36,1 35,1 34,1 33,1 32,1 31,1 30,1 29,1 28,1 27,1 26,1 25,1 26,0 "
                          + "W19S25 26,48 26,48"
                      )
                  )) with
                TicksToLive = 1_082
            }
            // An empty 23C23M hauler shot down to 16M 17C, at the group's
            // pace: dancing on W19S25's south exit while the ranger camped
            // it there, then into W19S26 after the ranger crossed, kiting
            // the group (t1,068,278-338), and back across.
            { body
                  "Bolt7991"
                  trepidimous
                  hauler
                  (at "W19S25" 32 48)
                  (Some(
                      livePath (
                          "W19S25 33,48 34,48 35,48 36,48 37,49 W19S26 37,1 36,1 35,0 W19S25 35,49 W19S26 "
                          + "35,1 34,1 33,0 W19S25 34,48 35,48 36,48 37,48 38,49 W19S26 38,1 37,1 36,1 35,1 "
                          + "34,1 33,0 W19S25 34,48 35,48 36,48 37,48 38,48 39,49 W19S26 39,1 38,1 37,1 36,0 "
                          + "W19S25 37,48 38,48 39,49 W19S26 39,1 40,2 41,3 40,2 41,3 42,4 43,5 44,6 43,5 "
                          + "44,6 45,7 44,8 43,9 42,8 41,7 41,8 40,7 41,8 40,7 41,8 40,7 41,8 42,7 43,6 44,7 "
                          + "43,6 42,5 41,4 40,3 39,2 38,1 39,2 40,3 41,4 42,5 43,6 44,7 43,6 42,5 41,4 40,3 "
                          + "39,2 40,3 41,4 42,5 43,6 42,5 41,4 40,3 41,4 42,5 43,6 42,5 41,4 40,3 39,2 38,1 "
                          + "37,1 36,1 35,1 34,1 33,0 W19S25 32,48 31,48 32,47 33,48"
                      )
                  )) with
                Hits = 3_700
                TicksToLive = 1_387
            }
            // W19S25's seated miners and the haulers at their source.
            body
                "Morgana7524"
                trepidimous
                (parts [ Move, 3; Work, 5 ])
                (at "W19S25" 27 20)
                (Some Hold)
            body "Quirk7553" trepidimous hauler (at "W19S25" 27 21) (Some Hold)
            body "Oblivion7691" trepidimous hauler (at "W19S25" 27 22) (Some oblivion)
            body "Bite7829" trepidimous (parts [ Move, 3; Work, 5 ]) (at "W19S25" 5 19) (Some Hold)
            body "Phantom7853" trepidimous hauler (at "W19S25" 4 20) (Some Hold)
            // W18S27, the sweep's third room.
            { body
                  "Hammer7389"
                  trepidimous
                  (parts [ Move, 7; Carry, 7 ])
                  (at "W18S27" 25 7)
                  (Some Hold) with
                Hits = 1_400
            }
            body
                "Eternity8186"
                trepidimous
                (parts [ Move, 3; Work, 5 ])
                (at "W18S27" 26 8)
                (Some Hold)
            body
                "Torpedo8171"
                trepidimous
                (parts [ Move, 1; RangedAttack, 1; Move, 1; Heal, 1; Move, 1 ])
                (at "W18S27" 3 11)
                (Some Hold)
        ]

    let a =
        arena 1_068_241 (w17s29 :: w17s25 :: empty) colonies bodies
        |> withOperations (Colony.operations |> List.filter (fun wire -> wire.Id = Colony.sweepId))

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [ "guard-1067926-Spawn9", "op:sweep"; "ranger-1067770-Spawn9", "op:sweep" ]
                // The record's room before the ring's first event (t1,068,293
                // moved off W19S25); its tick is not in the ring.
                Operations =
                    [
                        {
                            Id = Colony.sweepId
                            Started = 1_061_277
                            Phase = OpPhase.Execute
                            Since = 1_068_200
                            Room = Some "W19S25"
                            RoomSince = 1_068_200
                            Spent = 0
                            Lost = 0
                            NextDue = 0
                            Ended = None
                        }
                    ]
            }
    }

[<Tests>]
let w19s25SweepLatchTests =
    testList
        "arena capture: the sweep's room over W19S25/W19S26"
        [
            // Live W19S25/W19S26, bundle a3801bee, t1,068,293-340: eighteen
            // `Outpaid` moves between the two rooms. Bolt7991 kited the group
            // in W19S26, read caught on each tick it turned back (W19S26 69
            // a tick against W19S25's 49) and camped at a dancer's odds each
            // time it came within four of the north edge (133-160), though
            // it had crossed last at t1,068,277 and walked inland since; a
            // step on, as walking (14). Its price is its trail's: once it
            // kites inland the room is W19S25, held to the end.
            test
                "t1,068,284-340: once Bolt7991 kites inland, the room is W19S25 on every tick, whichever way it steps" {
                let rooms =
                    ((w19s25SweepLatch (), []), [ 1..100 ])
                    ||> List.fold (fun (state, rooms) _ ->
                        let next, _ = run 1 state

                        next,
                        (state.Time, next.Carried.Operations |> List.tryHead |> Option.bind _.Room)
                        :: rooms)
                    |> snd
                    |> List.rev

                let kited =
                    rooms |> List.filter (fun (tick, _) -> tick >= 1_068_284 && tick <= 1_068_340)

                Expect.isTrue
                    (kited |> List.forall (fun (_, room) -> room = Some "W19S25"))
                    $"W19S25 from t1,068,284 to t1,068,340: {rooms}"
            }
        ]
