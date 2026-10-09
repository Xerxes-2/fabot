/// The world lane in the arena: which fighter answers which raid, how far a
/// lane reaches, a fight lost where it is struck, and the observer's vision
/// the lane decides on.
module Fabot.Core.Tests.ArenaLaneTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures
open Fabot.Core.Tests.ArenaOutpostCaptures
open Fabot.Core.Tests.ArenaMovementCaptures

/// W13S28 at RCL8 with its observer, and W14S27 declared a colony of ours
/// nobody has claimed: diagonal to the home, so in no scan set and blind.
let private observing () =
    let home =
        room "W13S28"
        |> withController Ownership.Ours None 8 0
        |> withSpawn "Spawn3" { X = 36; Y = 42 } 12_900
        |> withStructures [ structureOf "observer" (Some Side.Ours) 500 500 { X = 30; Y = 30 } ]

    arena 922_600 [ home; room "W14S27" ] [ colony "W13S28"; colony "W14S27" ] []

[<Tests>]
let arenaVisionTests =
    testList
        "arena vision"
        [
            test
                "an observed room's sighting reaches the next tick's World, and is kept after (#484)" {
                let observer = "observer-30-30"

                let sightingOf (a: Arena) =
                    Map.tryFind "W14S27" (worldOf a).Sightings

                let start = observing ()

                Expect.isNone (sightingOf start) "blind before the look"

                let looked, trace = step start

                Expect.contains
                    trace.Ours
                    (ObserveRoom(observer, "W14S27"))
                    "the observer looks at the declared room nobody of ours sees"

                Expect.equal
                    (sightingOf looked |> Option.map (fun s -> s.Tick))
                    (Some looked.Time)
                    "seen the next tick, through the World's own path"

                let after, _ = step looked

                Expect.isFalse (Set.contains "W14S27" after.Observed) "seen, so not looked at again"

                Expect.equal
                    (sightingOf after |> Option.map (fun s -> s.Tick))
                    (Some looked.Time)
                    "and the sighting is remembered once the vision lapses"
            }
        ]

// ---- arena capture intercept850 (#569) -------------------------------------
// `npm run capture -- W18S25 990400 --rooms W17S25,W15S28,W17S26 --name intercept850`,
// read at live t990,553. The room-history replay at t990,400, the state after
// that tick ran: arena tick 0 runs as t990,401.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/990400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/990400.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/990400.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/990400.json
// Healers, ExitWatches, Sightings and the raid memory start empty (heap only);
// the Raid log, ArmedSeen and Towered are live Memory's at t990,553, less what
// postdates the tick (ArmedSeen W18S25 t990,548, Threatened W18S25 t990,548).
// W18S25's and W17S26's reservations, 1 energy, 3 tombstones and 2 ruins are
// not placed.

/// W18S25 at t990,400 (#569): Intercept850, Trepidimous's 17 MOVE 17 ATTACK
/// 1 MOVE crippled to 14 ATTACK and 1 MOVE, plods down the road from (42,13)
/// toward (22,10), one tile every few ticks, while W17S25 stands W18S25 down
/// on its raid (PlayerRaidHold, to t990,545). Nothing of ours stands in the
/// room: it is seen only on W13S28's observer's looks. Live it lived to
/// t990,631, killed once the stand-down ran out and the rooms' work resumed.
let private intercept850 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 31; Y = 5 } with
                    Energy = 410
                }
            ]
        |> withRoads
            [
                28, 9, 4_000
                29, 8, 3_000
                29, 10, 4_400
                30, 7, 3_000
                30, 11, 2_400
                31, 6, 3_800
                31, 12, 2_600
                32, 13, 2_500
                33, 14, 2_700
                34, 15, 3_600
                35, 16, 3_300
                36, 17, 3_300
                37, 16, 2_600
                38, 15, 3_200
                39, 14, 3_100
                40, 13, 4_600
                41, 12, 4_200
                42, 13, 2_900
                43, 13, 4_000
                44, 13, 4_500
                45, 13, 2_600
                46, 14, 2_500
                47, 15, 4_100
                48, 16, 4_400
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 263_454
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 172_000 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 262
                }
                { structureOf "rampart" (Some Side.Ours) 100_021 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 990_450
                }
                { structureOf "rampart" (Some Side.Ours) 100_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 990_431
                }
                { structureOf "rampart" (Some Side.Ours) 100_191 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 990_434
                }
                { structureOf "rampart" (Some Side.Ours) 100_531 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 990_439
                }
                { structureOf "rampart" (Some Side.Ours) 100_111 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 990_411
                }
                { structureOf "rampart" (Some Side.Ours) 100_031 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 990_413
                }
                { structureOf "rampart" (Some Side.Ours) 100_421 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 990_438
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 990_499
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 990_417
                }
                { structureOf "rampart" (Some Side.Ours) 100_761 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 990_432
                }
                { structureOf "rampart" (Some Side.Ours) 100_361 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 990_480
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 990_431
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 990_416
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 990_491
                }
                { structureOf "rampart" (Some Side.Ours) 100_521 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 990_479
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 990_437
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 990_481
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 990_477
                }
                { structureOf "rampart" (Some Side.Ours) 90_101 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 990_490
                }
                { structureOf "rampart" (Some Side.Ours) 95_891 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 990_409
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_804
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 2_600
                2, 17, 2_700
                3, 18, 2_600
                4, 19, 3_900
                5, 20, 3_900
                6, 21, 4_500
                7, 22, 19_500
                8, 23, 17_800
                9, 24, 16_900
                10, 25, 13_800
                10, 27, 18_400
                10, 29, 14_500
                11, 24, 13_300
                11, 26, 18_000
                11, 28, 15_400
                11, 30, 2_900
                12, 23, 15_400
                12, 25, 13_400
                12, 27, 16_900
                12, 29, 2_900
                13, 24, 4_800
                13, 26, 2_600
                13, 28, 3_600
                13, 30, 3_100
                14, 23, 4_600
                14, 25, 4_100
                14, 27, 2_900
                14, 29, 2_800
                14, 31, 2_800
                15, 22, 3_300
                15, 28, 3_600
                15, 30, 4_400
                15, 32, 2_800
                16, 21, 3_000
                16, 29, 4_400
                16, 31, 4_600
                16, 33, 4_600
                16, 35, 2_500
                16, 37, 4_300
                16, 38, 13_000
                16, 39, 13_300
                16, 41, 4_600
                16, 43, 4_400
                16, 44, 4_400
                17, 20, 2_900
                17, 30, 4_400
                17, 32, 2_600
                17, 34, 4_500
                17, 36, 3_600
                17, 38, 4_700
                17, 39, 12_900
                17, 40, 18_100
                17, 42, 4_700
                17, 45, 3_100
                18, 29, 2_600
                18, 31, 3_000
                18, 35, 4_100
                18, 37, 2_400
                18, 39, 3_000
                18, 41, 3_600
                18, 46, 3_600
                19, 30, 4_800
                19, 34, 3_100
                19, 36, 4_200
                19, 38, 3_000
                19, 40, 3_000
                19, 47, 3_300
                20, 31, 3_900
                20, 33, 3_300
                20, 35, 2_900
                20, 37, 4_800
                20, 39, 3_100
                20, 48, 4_000
                21, 32, 2_500
                21, 34, 4_300
                21, 36, 3_000
                21, 38, 4_200
                22, 33, 2_500
                22, 35, 3_100
                22, 37, 2_500
                23, 34, 2_500
                23, 36, 2_900
                24, 35, 3_100
                25, 34, 3_900
                26, 33, 4_300
                27, 32, 4_100
                28, 31, 18_000
                28, 33, 20_100
                29, 32, 18_000
                29, 34, 13_000
                30, 31, 18_500
                30, 33, 13_500
                31, 32, 14_000
                32, 31, 19_000
                33, 30, 20_000
                33, 32, 16_310
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_019_484
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 170_700 250_000 { X = 6; Y = 29 }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 726
                }
                { structureOf "container" None 197_500 250_000 { X = 23; Y = 29 } with
                    Energy = 1_950
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 990_461
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 990_421
                }
                { structureOf "rampart" (Some Side.Ours) 102_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 990_479
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 990_429
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 990_422
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 990_475
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 990_496
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 990_445
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 990_438
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 30_493
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 2_500
                5, 4, 3_800
                5, 6, 5_000
                5, 44, 4_200
                6, 3, 2_900
                6, 7, 4_300
                6, 29, 3_500
                6, 43, 3_800
                6, 45, 4_700
                7, 2, 4_500
                7, 8, 2_500
                7, 28, 4_700
                7, 42, 3_600
                7, 46, 4_000
                8, 1, 2_800
                8, 9, 3_300
                8, 28, 3_500
                8, 41, 3_900
                8, 47, 3_300
                9, 10, 4_300
                9, 27, 4_300
                9, 28, 4_900
                9, 40, 3_600
                9, 48, 4_000
                10, 11, 3_500
                10, 26, 4_700
                10, 29, 4_400
                10, 39, 3_900
                11, 12, 3_000
                11, 18, 3_100
                11, 27, 3_300
                11, 30, 4_600
                11, 38, 4_400
                12, 13, 3_900
                12, 17, 4_300
                12, 25, 20_100
                12, 27, 13_400
                12, 28, 4_800
                12, 29, 4_500
                12, 31, 3_400
                12, 33, 17_700
                12, 35, 21_500
                12, 37, 3_800
                13, 14, 4_000
                13, 16, 3_900
                13, 26, 18_300
                13, 28, 3_300
                13, 30, 4_900
                13, 32, 2_800
                13, 34, 3_900
                13, 36, 4_000
                14, 15, 4_900
                14, 25, 21_200
                14, 27, 2_900
                14, 29, 3_900
                14, 31, 4_200
                14, 33, 5_000
                14, 35, 4_300
                15, 15, 4_500
                15, 26, 2_700
                15, 28, 3_800
                15, 30, 2_800
                15, 32, 2_500
                15, 34, 4_000
                16, 16, 4_500
                16, 25, 15_000
                16, 27, 2_800
                16, 29, 2_900
                16, 31, 4_000
                16, 33, 3_500
                16, 35, 3_400
                17, 17, 2_900
                17, 26, 14_300
                17, 28, 2_800
                17, 30, 5_000
                17, 32, 4_700
                17, 34, 2_600
                18, 18, 4_000
                18, 25, 15_100
                18, 27, 12_900
                18, 29, 4_200
                18, 31, 3_800
                18, 33, 4_300
                19, 19, 19_200
                19, 22, 13_500
                19, 24, 19_500
                19, 26, 16_500
                19, 28, 3_000
                19, 30, 4_000
                19, 32, 5_000
                20, 20, 20_000
                20, 21, 14_500
                20, 23, 14_700
                20, 25, 13_800
                20, 27, 19_700
                20, 29, 2_700
                20, 31, 3_300
                20, 33, 13_600
                21, 21, 4_400
                21, 22, 2_700
                21, 23, 2_700
                21, 24, 13_000
                21, 25, 18_100
                21, 26, 15_800
                21, 28, 14_000
                21, 30, 4_700
                21, 32, 18_000
                21, 34, 14_700
                22, 24, 4_700
                22, 25, 19_300
                22, 27, 13_800
                22, 28, 4_500
                22, 29, 3_900
                22, 30, 14_500
                22, 31, 18_800
                22, 32, 20_100
                22, 33, 18_200
                22, 35, 4_300
                23, 26, 21_300
                23, 28, 18_000
                23, 29, 19_700
                23, 30, 19_400
                23, 31, 12_500
                23, 32, 19_900
                23, 33, 13_700
                23, 34, 4_900
                24, 25, 18_600
                24, 28, 20_500
                24, 29, 21_600
                24, 30, 18_900
                24, 33, 15_500
                25, 24, 4_700
                25, 28, 21_200
                25, 29, 20_800
                25, 34, 21_900
                26, 23, 21_500
                27, 22, 18_700
                28, 21, 18_400
                28, 34, 11_600
                29, 20, 19_700
                30, 19, 18_100
                31, 18, 4_300
                32, 17, 4_300
                33, 16, 3_000
                34, 15, 3_600
                35, 14, 4_400
                36, 13, 13_000
                37, 12, 4_600
                38, 11, 3_400
                39, 12, 4_300
                40, 13, 19_900
                41, 14, 18_200
                42, 15, 2_900
                43, 16, 2_900
                44, 17, 4_400
                45, 18, 4_400
                46, 19, 4_400
                47, 20, 3_100
                48, 21, 3_900
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 143_600 250_000 { X = 27; Y = 9 } with
                    Energy = 1_128
                }
            ]
        |> withRoads
            [
                20, 1, 3_900
                21, 2, 3_500
                22, 3, 3_500
                23, 4, 4_000
                24, 5, 4_400
                25, 6, 4_000
                26, 7, 2_500
                27, 8, 3_000
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    // Intercept850's live tiles, t990,400-990,620 (`observe history`).
    let plod =
        [
            42, 13
            41, 12
            40, 13
            39, 14
            38, 15
            37, 16
            36, 17
            35, 16
            34, 15
            33, 14
            32, 13
            31, 12
            30, 11
            29, 10
            28, 9
            27, 10
            26, 11
            25, 11
            24, 10
            23, 10
            22, 10
        ]
        |> List.map (fun (x, y) -> at "W18S25" x y)

    let bodies: Body list =
        [
            // Live: 14 ATTACK and 1 MOVE acting; it walks its live tiles.
            { body
                  "Intercept850"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 42 13)
                  (Some(Waypoints plod)) with
                Hits = 1_474
                TicksToLive = 1_053
                Fatigue = 11
            }
            { body
                  "worker-988870-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 28
                Energy = 188
            }
            { body
                  "anchor-989072-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 194
                Energy = 50
            }
            { body
                  "worker-989418-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 4 19)
                  None with
                TicksToLive = 576
                Energy = 344
            }
            { body
                  "hauler-989600-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 770
            }
            { body
                  "ranger-989686-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 910
            }
            { body
                  "hauler-989738-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 18 46)
                  None with
                TicksToLive = 908
                Energy = 300
            }
            { body
                  "upgrader-989914-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_063
            }
            { body
                  "upgrader-990062-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_211
            }
            { body
                  "defender-990336-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 9 24)
                  None with
                TicksToLive = 1_494
            }
            { body
                  "worker-989078-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 31)
                  None with
                TicksToLive = 326
                Energy = 258
            }
            { body
                  "worker-989272-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 520
                Energy = 136
            }
            { body
                  "guard-989780-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 47 20)
                  None with
                TicksToLive = 908
            }
            { body
                  "hauler-989812-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 13 36)
                  None with
                TicksToLive = 1_054
                Energy = 922
            }
            { body
                  "hauler-989962-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 37 12)
                  None with
                TicksToLive = 1_204
                Energy = 1_114
            }
            { body
                  "hauler-990022-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 33)
                  None with
                TicksToLive = 1_264
                Energy = 1_300
            }
            { body
                  "anchor-990126-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 1_248
                Energy = 50
            }
            { body
                  "anchor-990150-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_272
                Energy = 50
            }
            { body
                  "upgrader-990248-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_493
                Energy = 26
            }
            { body
                  "reserver-990388-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S28" 22 27)
                  None with
                TicksToLive = 592
            }
            // Live: 13 MOVE and 13 CARRY, a hauler of Trepidimous's, standing.
            { body
                  "Enigma370"
                  trepidimous
                  (parts (List.replicate 23 [ Move, 1; Carry, 1 ] |> List.concat))
                  (at "W17S26" 19 27)
                  (Some Hold) with
                Hits = 2_590
                TicksToLive = 606
            }
            { body
                  "ranger-989396-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 17 23)
                  None with
                TicksToLive = 620
            }
            { body
                  "hauler-989528-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S26" 21 3)
                  None with
                TicksToLive = 698
                Energy = 50
            }
            { body
                  "reserver-989858-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 68
            }
            { body
                  "anchor-990128-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 1_256
                Energy = 50
            }
            { body
                  "guard-990286-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 19 17)
                  None with
                TicksToLive = 1_414
            }
        ]

    let a = arena 990_401 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    // A home's stand-down rows as live Memory held them, LastSeen clamped to
    // the tick: W16S26's stronghold, W18S25's raid, and the two rival
    // reservations.
    let row room opened expiry basis stronghold : Fabot.Core.Observe.OutpostEpisode =
        {
            RoomName = room
            Opened = opened
            LastSeen = 990_400
            Expiry = expiry
            Basis = basis
            Stronghold = stronghold
            Raiders = Set.empty
            PlayerSince = None
            Chased = false
            Bounced = false
            HolderUnread = false
        }

    let rows =
        [
            row "W16S26" 972_732 1_045_681 StandDownBasis.CollapseTimer true
            row "W18S25" 990_345 990_545 StandDownBasis.PlayerRaidHold false
            row "W18S29" 986_839 991_717 StandDownBasis.RivalReservation false
            row "W19S28" 986_841 991_510 StandDownBasis.RivalReservation false
        ]

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t990,400,
                // read from Memory at live t990,553; it held none that early
                // for worker-988870, defender-990336 and reserver-989858.
                Assignments =
                    Map.ofList
                        [
                            "anchor-989072-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "worker-989418-Spawn11", "repair:rampart-2-16"
                            "ranger-989686-Spawn8", "guard:W17S25"
                            "hauler-989738-Spawn11", "withdraw:container-27-9"
                            "worker-989078-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "worker-989272-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-989812-Spawn8", "refill:storage-17-29"
                            "hauler-989962-Spawn8", "refill:storage-17-29"
                            "hauler-990022-Spawn3", "refill:spawn-Spawn3"
                            "anchor-990126-Spawn8", "harvest:6a8caa95dd4872bccd319013"
                            "anchor-990150-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "upgrader-990248-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "reserver-990388-Spawn8", "reserve:6a8caaa1dd4872bccd3191fa"
                            "ranger-989396-Spawn8", "guard:W17S26"
                            "hauler-989528-Spawn11", "withdraw:container-27-9"
                            "anchor-990128-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "guard-990286-Spawn11", "guard:W17S26"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 990_343; "W17S26", 990_124 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts = rows
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts = rows
                                HomeRaided = Some 990_343
                            }
                        ]
            }
    }

/// The same tick with W18S25's stand-down spent, as it was from t990,545:
/// the room worked again, the raid seen only on the observer's looks.
let private intercept850Spent () : Arena =
    let a = intercept850 ()

    { a with
        Carried =
            { a.Carried with
                Raids =
                    a.Carried.Raids
                    |> Map.map (fun _ log ->
                        { log with
                            Outposts =
                                log.Outposts |> List.filter (fun row -> row.RoomName <> "W18S25")
                        })
            }
    }

/// Run `ticks` ticks with W18S25 in sight only on W13S28's observer's looks,
/// which the arena does not hold: every eighth tick from tick 4, as live
/// (`dangerStride`). Each tick's trace, and the assignments it ended on.
let private underTheObserver (ticks: int) (start: Arena) : (TickTrace * Assignments) list =
    ((start, []), List.replicate ticks ())
    ||> List.fold (fun (a, acc) () ->
        let looked =
            { a with
                Lookout = (if (a.Tick + 4) % 8 = 0 then set [ "W18S25" ] else Set.empty)
            }

        let next, trace = step looked
        next, (trace, next.Carried.Assignments) :: acc)
    |> snd
    |> List.rev

/// The ticks from `first` until Intercept850 dies on which the ranger did not
/// end holding W18S25's Guard, and the tick it died on.
let private guardGaps (first: int) (run: (TickTrace * Assignments) list) =
    let trace = run |> List.map fst
    let died = diedOn "Intercept850" trace

    let gaps =
        run
        |> List.filter (fun (t, assignments) ->
            t.Tick >= first
            && died |> Option.forall (fun d -> t.Tick < d)
            && Map.tryFind "ranger-989686-Spawn8" assignments <> Some "guard:W18S25")
        |> List.map (fun (t, _) -> t.Tick)

    gaps, died

[<Tests>]
let intercept850Tests =
    testList
        "arena capture intercept850"
        [
            // Not pinned, a finding about the arena: with the stand-down
            // seated the arena chases and kills the cripple inside it on main
            // too (its verdict at the first look is Kite, which sets the row's
            // `Chased`), while live the room went unanswered to t990,545. The
            // Win half of that gap is pinned in `OutpostRaidTests` and
            // `ObserveOutpostTests` (#569).
            //
            // Live after t990,545 the ranger matched the Guard on each look and
            // dropped it task-gone the next tick (#569); main dropped it on
            // ticks 5 and 13 and the guard killed it at t67.
            test
                "#569 capture t990,400, its stand-down spent: the ranger holds W18S25's Guard through the ticks between the looks and kills the cripple" {
                let run = intercept850Spent () |> underTheObserver 60
                let gaps, died = guardGaps 4 run

                Expect.isEmpty gaps "never task-gone between two looks"

                let died =
                    match died with
                    | Some tick -> tick
                    | None -> failtest "the cripple dies inside the run (t42 at writing)"

                // The guard is in the capture too: the ranger's shot lands on
                // the death tick or the one before it.
                Expect.isTrue
                    (run
                     |> List.exists (fun (t, _) ->
                         (t.Tick = died || t.Tick = died - 1)
                         && t.Ours
                            |> List.contains (
                                RangedAttackCreep("ranger-989686-Spawn8", "Intercept850")
                            )))
                    "and the ranger's shots kill it"
            }
        ]

// ---- the world lane ---------------------------------------------------------
// The captures the world lane's one match over the fleet first moved or
// reproduced (ADR 0091 W5), each pinned on its outcome and why.

[<Tests>]
let worldLaneCaptureTests =
    testList
        "arena captures on the world lane"
        [
            // At t10 W17S26's chase turns Kite. Before #596 it seated one body
            // world-wide, the one at the fight soonest: the child's 2-RANGED
            // ranger-985441, the mother's 14-RANGED ranger-984683 let go, and
            // the melee died at t114. Now the kite seats the
            // body that ends the raid soonest (the mother's, walk and kill)
            // and beside it every ranger already at the fight (the child's).
            // Moved on purpose (#615 U3): the child's 2-RANGED ranger ties the
            // melee and falls to it before the mother's land, so it no longer
            // fits the kite (it held on `rangers > 1`); the mother's second
            // ranger, free and fitting beside the first, takes the seat, and
            // the melee dies two ticks sooner. The shots and the t32 kill are
            // pinned in ArenaBorderTests' movement S3 kite test. Moved again
            // by #625: HalfLife847 (18M 17A, MOVE first) is at the child's
            // ranger's pace, and its shots lame it inside the ground it has,
            // so it keeps it off untouched and fits the kite once more; the
            // mother's second ranger is not wanted beside it.
            test
                "movement S3 capture t986,005, the world lane: at t20 the mother's ranger and the child's hold the kite" {
                let first, _ = rangerIntoMelee () |> run 20

                Expect.equal
                    (first.Carried.Assignments
                     |> Map.filter (fun _ tid -> tid = "guard:W17S26")
                     |> Map.keys
                     |> List.ofSeq)
                    [ "ranger-984683-Spawn3"; "ranger-985441-Spawn11" ]
                    "at t20 the mother's ranger and the child's hold the kite"
            }
        ]

// ---- a fight lost where it is struck ----------------------------------------
// `npm run capture -- W19S26 1007219 --rooms W19S25,W18S26,W17S25,W18S25,W17S26 --name w19s26-losing-trade`, read at live t1,008,800.
//   W19S26  https://screeps.com/room-history/shardSeason/W19S26/1007219.json
// W17S25's ranger-1006821 (6M 4RA 2H) harassing Trepidimous' remote W19S26
// as Wave7157 (7M 3RA 4H) crosses in from his home: its 40 a tick under the
// other's 48 heal, the other's 30 over its own 24. Live it closed to range 1
// by t1,007,230 and traded there, its MOVE shot off first, until it died at
// t1,007,417. Wave7157, crossing on the tick, stands on its landing and
// chases the nearest of ours (live it kept range 1 throughout); every other
// body not ours holds but the hauler Ion6927, which walks to 43,10 as live.
// Trepidimous' home W18S26 is its controller, towers, spawn and stores only
// (the fight never enters it); the reservations, the drops and the replay's
// ruins and tombstones are not placed. The Task log no longer held either
// ranger's Task.
//
// Before #602 the capture reproduced the live trade tick for tick: range 1
// at 40,17 from t10, 1,122 hits at t20 and 822 at t70 (live t1,007,240 and
// t1,007,290). It parts from live at t75, where the other ranger,
// ranger-1006965, arrives: live it stood at W19S25 27,20 until past
// t1,007,350, whatever held it there gone from the log, while here W19S26's
// Guard takes it at t0. The pair then killed Wave7157 at t109, the first
// ranger down to 818. `stock` is W17S25's Storage energy: live 6,991.
let private w19s26LosingTrade (stock: int) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w19s26 =
        room "W19S26"
        |> withSite trepidimous "container" None 2_710 { X = 5; Y = 47 }
        |> withSite trepidimous "container" None 0 { X = 22; Y = 35 }

    let w19s25 = room "W19S25"

    let w18s26 =
        room "W18S26"
        |> withController Ownership.Rival (Some "Trepidimous") 6 4
        |> withControllerClocks 0 0 120_000 1_987_328
        |> withStructures
            [
                { structureOf "spawn" (Some trepidimous) 5_000 5_000 { X = 20; Y = 12 } with
                    Energy = 300
                }
                { structureOf "storage" (Some trepidimous) 10_000 10_000 { X = 20; Y = 14 } with
                    Energy = 20_989
                }
                { structureOf "terminal" (Some trepidimous) 3_000 3_000 { X = 22; Y = 16 } with
                    Energy = 40_674
                }
                towerOf trepidimous { X = 18; Y = 8 } 1_000
                towerOf trepidimous { X = 22; Y = 8 } 990
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 79_954 8_043
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
        |> withRamparts
            Side.Ours
            100_000
            [
                for y in [ 15..20 ] @ [ 23..27 ] -> { X = 2; Y = y }
                for x, y in
                    [
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
                    ] -> { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 137_000 250_000 { X = 16; Y = 34 } with
                    Energy = 2
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 198
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = stock
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 4_900
                2, 17, 5_000
                3, 18, 4_900
                4, 19, 3_400
                5, 20, 3_200
                6, 21, 3_800
                7, 22, 12_900
                8, 23, 19_800
                9, 24, 20_100
                9, 26, 24_000
                9, 28, 24_000
                10, 23, 23_500
                10, 25, 20_100
                10, 27, 13_300
                10, 29, 14_100
                11, 24, 15_100
                11, 26, 19_900
                11, 28, 21_100
                11, 30, 3_300
                12, 23, 13_300
                12, 25, 20_400
                12, 27, 21_200
                12, 29, 1_800
                12, 31, 4_600
                13, 24, 4_600
                13, 26, 5_000
                13, 28, 2_700
                13, 30, 1_800
                14, 23, 5_000
                14, 25, 5_000
                14, 27, 5_000
                14, 29, 2_100
                14, 31, 4_800
                15, 22, 2_800
                15, 28, 3_000
                15, 30, 3_800
                15, 32, 4_700
                16, 21, 3_100
                16, 29, 3_900
                16, 31, 4_900
                16, 33, 3_600
                16, 35, 2_500
                16, 37, 3_900
                16, 38, 8_100
                16, 39, 9_100
                16, 41, 4_700
                16, 43, 4_500
                16, 44, 3_700
                17, 20, 3_600
                17, 30, 1_800
                17, 32, 2_600
                17, 34, 4_900
                17, 36, 3_200
                17, 38, 2_800
                17, 39, 13_500
                17, 40, 18_600
                17, 42, 4_900
                17, 45, 4_900
                18, 29, 2_900
                18, 31, 3_300
                18, 35, 3_800
                18, 37, 2_900
                18, 39, 3_700
                18, 41, 1_800
                18, 46, 3_600
                19, 30, 2_600
                19, 34, 3_100
                19, 36, 2_400
                19, 38, 3_700
                19, 40, 3_600
                19, 47, 3_600
                20, 31, 4_300
                20, 33, 3_000
                20, 35, 3_600
                20, 37, 3_100
                20, 39, 3_700
                20, 48, 3_000
                21, 32, 2_800
                21, 34, 2_600
                21, 36, 3_700
                21, 38, 2_500
                22, 33, 3_100
                22, 35, 3_800
                22, 37, 3_200
                23, 34, 3_000
                23, 36, 3_600
                24, 35, 3_700
                25, 34, 2_000
                26, 33, 2_500
                27, 32, 2_200
                28, 31, 6_500
                28, 33, 11_100
                29, 32, 15_400
                29, 34, 13_600
                30, 31, 7_500
                30, 33, 12_900
                31, 32, 10_900
                32, 31, 20_800
                33, 30, 15_140
                33, 32, 7_380
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 146_100 250_000 { X = 31; Y = 5 } with
                    Energy = 424
                }
            ]
        |> withRoads
            [
                28, 9, 3_400
                29, 8, 2_850
                29, 10, 4_580
                30, 7, 3_000
                30, 11, 4_200
                31, 6, 3_500
                31, 12, 5_000
                32, 13, 4_000
                33, 14, 2_500
                34, 15, 2_800
                35, 16, 2_500
                36, 17, 3_100
                37, 16, 4_000
                38, 15, 3_000
                39, 14, 2_900
                40, 13, 4_900
                41, 12, 4_800
                42, 13, 2_700
                43, 13, 3_870
                44, 13, 3_800
                45, 13, 2_400
                46, 14, 5_000
                47, 15, 3_900
                48, 16, 4_900
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 154_900 250_000 { X = 27; Y = 9 } with
                    Energy = 420
                }
            ]
        |> withRoads
            [
                20, 1, 3_600
                21, 2, 2_580
                22, 3, 3_100
                23, 4, 3_700
                24, 5, 1_700
                25, 6, 3_700
                26, 7, 2_300
                27, 8, 3_400
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body
                  "Wave7157"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W19S26" 49 7)
                  (Some(Chase(Nearest, None))) with
                TicksToLive = 1_478
            }
            { body
                  "Ion6927"
                  trepidimous
                  (parts
                      [
                          for _ in 1..22 do
                              yield! [ Move, 1; Carry, 1 ]
                      ])
                  (at "W19S26" 39 18)
                  (Some(GoTo(at "W19S26" 43 10))) with
                Hits = 4_360
                TicksToLive = 1_338
            }
            { body
                  "Ocean6422"
                  trepidimous
                  (parts
                      [
                          for _ in 1..23 do
                              yield! [ Move, 1; Carry, 1 ]
                      ])
                  (at "W17S26" 28 8)
                  (Some Hold) with
                TicksToLive = 839
            }
            { body
                  "Coefficient6755"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 12 24)
                  (Some Hold) with
                TicksToLive = 1_139
            }
            { body
                  "Constant7133"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 17 1)
                  (Some Hold) with
                TicksToLive = 1_436
                Fatigue = 4
            }
            { body
                  "dismantler-1006415-Spawn11"
                  Side.Ours
                  (parts [ Work, 12; Move, 12 ])
                  (at "W19S26" 21 35)
                  None with
                TicksToLive = 766
            }
            { body
                  "ranger-1006821-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S26" 32 20)
                  None with
                TicksToLive = 1_136
            }
            { body
                  "ranger-1006965-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 27 20)
                  None with
                TicksToLive = 1_280
            }
            { body
                  "worker-1005881-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 9; Move, 9 ])
                  (at "W17S25" 12 23)
                  None with
                TicksToLive = 238
                Energy = 378
            }
            { body
                  "worker-1006267-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 10; Move, 9 ])
                  (at "W18S25" 32 13)
                  None with
                TicksToLive = 627
                Energy = 500
            }
            { body
                  "hauler-1006605-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 7 22)
                  None with
                TicksToLive = 992
            }
            { body
                  "hauler-1006713-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 21)
                  None with
                TicksToLive = 1_100
            }
            { body
                  "anchor-1006887-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_196
                Energy = 50
            }
            { body
                  "guard-1007097-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 19 35)
                  None with
                TicksToLive = 1_436
            }
            { body
                  "anchor-1006857-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 1_166
                Energy = 50
            }
            { body
                  "reserver-1007073-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 458
            }
        ]

    // W19S26 and W19S25 are harassment rooms W17S25 casts (#609): without
    // them the capture never sat ranger-1006965 on W19S25's Stand as live.
    arena 1_007_220 [ w19s26; w19s25; w18s26; w17s25; w18s25; w17s26 ] colonies bodies
    |> withHarass Colony.harassDeclared

[<Tests>]
let foughtWhereStruckTests =
    testList
        "arena a fight lost where it is struck"
        [
            // #602: a Guard is priced again on every tick its body is struck.
            // #608 had ranger-1006821, losing alone, run for the exit and cross
            // home at t28; home, nothing of ours saw W19S26, its Guard went,
            // and Wave7157 was left at the border.
            // #609: with the harassment rooms cast as live, ranger-1006965
            // held W19S25's quiet Guard on its Stand (live: 27,20 past
            // t1,007,350) while W19S26's harassment Guard seated one ranger.
            // Now the raided room seats the pair that wins it, two rungs up,
            // so ranger-1006965 is sent at t1; and ranger-1006821, which
            // outlasts Wave7157 by two hundred ticks (6 a tick net), holds
            // the fight until it lands. The pair kills Wave7157 at t109, the
            // first ranger down to 824, neither leaving W19S26 once in it.
            // W17S25's stock is raised from the live 6,991 to
            // `Tuning.CasterReserve`, so it casts both harassment rooms as it
            // did when the capture was taken (W6 S3 added the reserve).
            test
                "#602 capture t1,007,219: a ranger losing alone holds while its partner walks in, and the pair kills Wave7157" {
                let _, trace = w19s26LosingTrade Tuning.defaults.CasterReserve |> run 120
                let failure = describe trace

                let least ranger =
                    trace
                    |> List.collect (fun t -> t.Bodies |> List.filter (fun b -> b.Id = ranger))
                    |> List.map _.Hits
                    |> List.min

                Expect.isGreaterThanOrEqual
                    (least "ranger-1006821-Spawn11")
                    800
                    $"it holds at the cost of the wait, never to its death\n{failure}"

                Expect.isGreaterThanOrEqual
                    (least "ranger-1006965-Spawn11")
                    1_150
                    $"the other walks in to a won fight\n{failure}"

                Expect.isFalse
                    (pathOf "ranger-1006821-Spawn11" trace
                     |> List.exists (fun (_, s) -> s.At.Room <> "W19S26"))
                    $"it never leaves W19S26 for the exit row\n{failure}"

                Expect.isLessThanOrEqual
                    (diedOn "Wave7157" trace |> Option.defaultValue System.Int32.MaxValue)
                    115
                    $"the pair kills Wave7157 by t115\n{failure}"
            }

            // W19S26 under Wave7157's raid is the sweep's room, fought as a
            // contested room's raid, its ring laid; guard-1007097 stands in
            // W17S25, a declared candidate that is a home of ours: the code
            // spec drops it (`Operation.ofCode`), and the guard is no body
            // of the group.
            test
                "#663 S7b: the sweep as an operation, run in shadow, answers the raided room's ring, group and seats every tick" {
                let final, trace = w19s26LosingTrade Tuning.defaults.CasterReserve |> run 120
                let _, log = final.Carried.Shadow

                Expect.isGreaterThan log.Working 0 $"the sweep worked its room\n%A{log}"
                Expect.isEmpty log.Counts $"no divergence\n%A{log}\n{describe trace}"

                Expect.equal
                    log.Refused
                    [ { Id = "sweep"; Why = "ours: W17S25" } ]
                    "our home dropped from the spec"
            }

            // At the live 6,991, under `Tuning.CasterReserveHold`, no colony
            // casts either harassment room; each is held for its rangers
            // alone, and a Guard there is no fight they keep
            // (`Planner.planFightTasks`). Moved by #625: ranger-1006965, home
            // in W17S25 by t76, walks back out to W17S26 (t112) for its
            // Guard, whose Coefficient6755 (18M 17A, MOVE first, a ranger's
            // pace) it now kites; it walks home first all the same.
            test
                "#602 capture t1,007,219 at the live stock: with W19S26 and W19S25 cast by nobody, both rangers yield their Guards and walk home unhurt" {
                let final, trace = w19s26LosingTrade 6_991 |> run 120
                let failure = describe trace

                let least ranger =
                    trace
                    |> List.collect (fun t -> t.Bodies |> List.filter (fun b -> b.Id = ranger))
                    |> List.map _.Hits
                    |> List.min

                for ranger in [ "ranger-1006821-Spawn11"; "ranger-1006965-Spawn11" ] do
                    Expect.equal (least ranger) 1_200 $"{ranger} is never struck\n{failure}"

                    Expect.isTrue
                        (pathOf ranger trace
                         |> List.exists (fun (_, s) ->
                             List.contains s.At.Room [ "W18S25"; "W17S25" ]))
                        $"{ranger} walks home to its caster\n{failure}"

                Expect.equal
                    (Map.tryFind "ranger-1006965-Spawn11" final.Carried.Assignments)
                    (Some "guard:W17S26")
                    $"and one, home, takes W17S26's Guard\n{failure}"

                Expect.isTrue (survived "Wave7157" trace) $"and Wave7157 stands\n{failure}"
            }
        ]

// ---- arena capture t1,015,956 (#616, #617) -----------------------------------
// `npm run capture -- W17S26 1015956 --rooms W17S25,W15S28,W19S25,W18S25,W15S27,W16S27 --name w17s26-mother-guards`, read at live t1,016,090.
// The room-history replay at t1,015,956, the state after that tick ran: arena
// tick 0 runs as t1,015,957 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1015900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1015900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/1015900.json
//   W19S25  https://screeps.com/room-history/shardSeason/W19S25/1015900.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/1015900.json
//   W15S27  https://screeps.com/room-history/shardSeason/W15S27/1015900.json
//   W16S27  https://screeps.com/room-history/shardSeason/W16S27/1015900.json
// Healers, ExitWatches and Sightings start empty (heap only); the Raid log,
// ArmedSeen and Towered are live Memory's at t1,016,090, less what postdates
// the tick. The arena has no reservations (W17S26, W18S25, W15S27 ours,
// W16S27 Invader's), and places no ruin or tombstone. Spawn11 was spawning
// anchor-1015936-Spawn11 until t1,015,960: not placed. Trimmed from the
// skeleton: the ramparts, roads and road sites — the pin reads who is
// handed the trespass, not how fast a walk goes.

/// Three unarmed Trepidimous bodies in W17S25's outpost W17S26 while
/// W15S28's two melee guards stand idle at home, its blind W16S28 and
/// W17S27 between them (#617).
let private w17s26FarGuards () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let mirroar = Side.Player "Mirroar"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 334
                }
            ]
        |> withRoads
            [
                20, 1, 4_000
                21, 2, 4_000
                22, 3, 4_500
                23, 4, 2_200
                24, 5, 3_900
                25, 6, 2_400
                26, 7, 4_100
                27, 8, 2_200
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 67_807
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
                { structureOf "container" None 125_800 250_000 { X = 16; Y = 34 } with
                    Energy = 54
                }
                { structureOf "container" None 130_000 250_000 { X = 17; Y = 20 } with
                    Energy = 552
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_307
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_818_774
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                        16, 32
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 132_700 250_000 { X = 6; Y = 29 } with
                    Energy = 768
                }
                structureOf "container" None 125_100 250_000 { X = 11; Y = 18 }
                { structureOf "container" None 182_300 250_000 { X = 23; Y = 29 } with
                    Energy = 52
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 32_900
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    let w19s25 = room "W19S25"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 31; Y = 5 } with
                    Energy = 894
                }
            ]

    let w15s27 =
        room "W15S27"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 13; Y = 29 } with
                    Energy = 514
                }
            ]

    let w16s27 =
        room "W16S27"
        |> withStructures
            [
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    100_000
                    100_000
                    { X = 26; Y = 34 }
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "Genetic5584"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 27 8)
                  (Some Hold) with
                TicksToLive = 1_150
            }
            { body
                  "Forest5611"
                  trepidimous
                  (parts [ Move, 24; Carry, 24 ])
                  (at "W17S26" 26 8)
                  (Some Hold) with
                TicksToLive = 1_291
            }
            { body
                  "Dragon5749"
                  trepidimous
                  (parts [ Move, 3; BodyPart.Claim, 3 ])
                  (at "W17S26" 10 23)
                  (Some Hold) with
                TicksToLive = 409
            }
            { body
                  "anchor-1014956-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 528
                Energy = 50
            }
            { body
                  "reserver-1015541-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 195
            }
            { body
                  "anchor-1014449-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 15
                Energy = 50
            }
            { body
                  "upgrader-1015472-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_083
                Energy = 39
            }
            { body
                  "worker-1015724-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 1_347
                Energy = 9
            }
            { body
                  "hauler-1015828-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 15 30)
                  None with
                TicksToLive = 1_478
                Energy = 500
            }
            { body
                  "worker-1014855-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 0 15)
                  None with
                TicksToLive = 478
                Energy = 216
            }
            { body
                  "upgrader-1014645-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 334
                Energy = 26
            }
            { body
                  "anchor-1014845-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 411
                Energy = 50
            }
            { body
                  "guard-1014878-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 46 14)
                  None with
                TicksToLive = 450
            }
            { body
                  "guard-1015184-Spawn3"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W15S28" 7 47)
                  None with
                TicksToLive = 786
            }
            { body
                  "anchor-1015660-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_232
                Energy = 50
            }
            { body
                  "worker-1015760-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 30)
                  None with
                TicksToLive = 1_452
                Energy = 200
            }
            { body
                  "worker-1015800-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 10 29)
                  None with
                TicksToLive = 1_492
            }
            { body
                  "hauler-1014619-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 13 16)
                  None with
                TicksToLive = 305
                Energy = 1_044
            }
            { body
                  "worker-1015272-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 11 27)
                  None with
                TicksToLive = 964
                Energy = 548
            }
            { body
                  "hauler-1015510-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 29)
                  None with
                TicksToLive = 1_196
                Energy = 1_600
            }
            { body
                  "Launch5733"
                  trepidimous
                  (parts [ Move, 9; Work, 9; Carry, 9 ])
                  (at "W19S25" 14 17)
                  (Some Hold) with
                TicksToLive = 1_356
            }
            { body
                  "ranger-1015096-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 16 28)
                  None with
                TicksToLive = 674
            }
            { body
                  "anchor-1015340-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 912
                Energy = 50
            }
            { body
                  "reserver-1015570-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 224
            }
            { body
                  "hauler-1014493-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W18S25" 41 11)
                  None with
                TicksToLive = 143
            }
            { body
                  "anchor-1014763-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S27" 13 29)
                  None with
                TicksToLive = 335
                Energy = 50
            }
            { body
                  "reserver-1015518-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S27" 5 9)
                  None with
                TicksToLive = 166
            }
            { body "S_te" mirroar (parts [ Move, 1 ]) (at "W15S27" 49 25) (Some Hold) with
                TicksToLive = 142
            }
        ]

    let a =
        arena 1_015_957 [ w17s26; w17s25; w15s28; w19s25; w18s25; w15s27; w16s27 ] colonies bodies

    let shut
        (room: string)
        (opened: int)
        (lastSeen: int)
        (expiry: int)
        (basis: StandDownBasis)
        : Fabot.Core.Observe.OutpostEpisode =
        {
            RoomName = room
            Opened = opened
            LastSeen = lastSeen
            Expiry = expiry
            Basis = basis
            Stronghold = (basis = StandDownBasis.CollapseTimer)
            Bounced = false
            Raiders = Set.empty
            PlayerSince = None
            Chased = false
            HolderUnread = false
        }

    let outposts =
        [
            shut "W16S26" 972_732 1_014_864 1_045_681 StandDownBasis.CollapseTimer
            shut "W18S29" 986_839 1_015_956 1_017_784 StandDownBasis.RivalReservation
            shut "W19S26" 1_015_886 1_015_956 1_016_076 StandDownBasis.HarassSighting
        ]

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t1,015,956. anchor-1014449-Spawn11 has none that early,
                // worker-1014855-Spawn11's names a tombstone and
                // worker-1015272-Spawn3's a rampart the arena does not place.
                Assignments =
                    Map.ofList
                        [
                            "anchor-1014956-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "reserver-1015541-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "upgrader-1015472-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "worker-1015724-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-1015828-Spawn11", "refill:spawn-Spawn11"
                            "upgrader-1014645-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-1014845-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "anchor-1015660-Spawn3", "harvest:6a8caa95dd4872bccd319014"
                            "worker-1015760-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "worker-1015800-Spawn8", "withdraw:container-6-29"
                            "hauler-1014619-Spawn3", "refill:container-23-29"
                            "hauler-1015510-Spawn3", "withdraw:storage-17-29"
                            "ranger-1015096-Spawn11", "guard:W17S26"
                            "anchor-1015340-Spawn11", "harvest:6a8caa71dd4872bccd318b28"
                            "reserver-1015570-Spawn11", "reserve:6a8caa71dd4872bccd318b29"
                            "hauler-1014493-Spawn11", "withdraw:container-31-5"
                            "anchor-1014763-Spawn3", "harvest:6a8caa95dd4872bccd319011"
                            "reserver-1015518-Spawn8", "reserve:6a8caa95dd4872bccd319010"
                        ]
                ArmedSeen = Map.ofList [ "W15S27", 1_014_946; "W17S26", 1_014_874 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts = outposts
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts = outposts
                                HomeRaided = Some 1_013_957
                            }
                        ]
            }
    }

[<Tests>]
let w17s26FarGuardsTests =
    testList
        "arena lane reach (#617)"
        [
            // Live, W15S28's guards never saw the trespass: the crossings to
            // W17S26 were counted over the rooms in sight, and W16S28 and
            // W17S27 were not. Over the ground they are four, W15S28's west
            // wall sending even W16S28 three round: past the lane's two. The
            // nearest fighter in reach holds the kill — W17S25's ranger,
            // three out in W19S25 — and the far guards hold home, as live.
            test
                "#617 capture t1,015,956: the trespass is the nearest fighter's in reach, counted over the ground and not over vision" {
                let start = w17s26FarGuards ()

                let reach =
                    World.reachHops StandDown.none (JoinTable()) Tuning.defaults (worldOf start)

                Expect.isNone
                    (World.hopsUnder
                        StandDown.none
                        (JoinTable())
                        Tuning.defaults
                        (worldOf start)
                        "W15S28"
                        "W17S26")
                    "the premise: no chain over the rooms the world holds"

                Expect.equal
                    (reach Tuning.defaults.MaxHops "W15S28" "W17S26")
                    (Some 4)
                    "four over the ground"

                Expect.equal
                    (reach Tuning.defaults.MaxHops "W15S28" "W16S28")
                    (Some 3)
                    "round the west wall"

                Expect.equal
                    (reach Tuning.defaults.MaxHops "W19S25" "W17S26")
                    (Some 3)
                    "the ranger's three"

                let after, trace = start |> run 40
                let failure = describe trace

                Expect.equal
                    (Map.tryFind "ranger-1015096-Spawn11" after.Carried.Assignments)
                    (Some "guard:W17S26")
                    $"W17S25's ranger holds the kill\n{failure}"

                for guard in [ "guard-1014878-Spawn3"; "guard-1015184-Spawn3" ] do
                    Expect.equal (roomsOf guard trace) [ "W15S28" ] $"{guard} holds home\n{failure}"

                    Expect.notEqual
                        (Map.tryFind guard after.Carried.Assignments)
                        (Some "guard:W17S26")
                        $"{guard} is not handed it\n{failure}"
            }

            // #621: the three bodies priced by one kill score for the ranger
            // that takes the kill (6M4R2H, 40 a tick): the 3M3C claimer
            // (1,950 body, the room's reservation, a 15-tick kill) before
            // the 5W3M miner on its seat (650 body, its dig, 20 ticks)
            // before the empty 24C24M hauler (2,400 body, 120 ticks). Before
            // it the ranger stood on the hauler's store ring and shot it for
            // a hundred ticks, the claimer standing. The bodies hold where
            // live stood them (the skeleton's script): the pin reads the
            // order on the live bodies and ground, not their live walks.
            //
            // #663: a kill is worth the life left it. The ranger lands at
            // t142, when the claimer has 267 of its 600 ticks left (its body
            // worth 868, not 1,950) and the miner 1,008 of 1,500: the miner
            // on its seat leads by its kill score, the claimer after it.
            test
                "#621 capture t1,015,956: the ranger kills the miner, then the claimer its life has spent, before the hauler" {
                let _, trace = w17s26FarGuards () |> run 250

                let diedAt who =
                    trace
                    |> List.tryPick (fun t ->
                        if
                            t.Events
                            |> List.exists (function
                                | Died id -> id = who
                                | _ -> false)
                        then
                            Some t.Tick
                        else
                            None)

                let failure = describe trace

                match diedAt "Dragon5749", diedAt "Genetic5584" with
                | Some claimer, Some miner ->
                    Expect.isLessThan miner claimer $"the miner first\n{failure}"
                | claimer, miner ->
                    failtest $"both die: the claimer at {claimer}, the miner at {miner}\n{failure}"

                Expect.isNone (diedAt "Forest5611") $"the hauler's long kill comes last\n{failure}"
            }
        ]

/// The world epic's W7: a colony that decides nothing this tick is projected over
/// the lane alone (`ColonyView.lane`), and every deciding colony decides the
/// same off that cut as off its whole view, on every tick of every capture
/// with more than one colony.
[<Tests>]
let arenaLaneCutTests =
    testList
        "arena lane cut"
        [
            // Each capture with whether a lane cut there leaves a room out:
            // W15S28's far outposts and their walks, or W17S25's W17S26.
            for label, start, narrows in
                [
                    "two raids beside W17S25", twoRaidsBesideW17S25, true
                    "the shut outpost's cripple (#573)", shutOutpostCripple, true
                    "the digger beside a held exit", diggerBesideHeldExit, true
                    "the swaying trespassers (#558)", swayingTrespassers, true
                    "the ranger into the melee", rangerIntoMelee, true
                    "the slow melee's haulers", slowMeleeHaulers, true
                    "Trepidimous entering W17S26",
                    (fun () -> trepEntersW17S26 (Chase(Nearest, None))),
                    false
                ] do
                test $"a deciding colony decides the same off the idle colonies' lane cuts: {label}" {
                    let mutable a = start ()
                    let mutable left = Set.empty

                    for _ in 1..30 do
                        Expect.isEmpty (laneBreaks a) "the lane cut moves no decision"

                        left <-
                            laneLeaves a
                            |> List.fold
                                (fun acc (_, idle, rooms) ->
                                    Set.union acc (Set.map (fun r -> idle, r) rooms))
                                left

                        a <- fst (step a)

                    Expect.equal
                        (not (Set.isEmpty left))
                        narrows
                        $"the lane cut leaves out %A{left}"
                }
        ]
