/// The arena's border play on captured terrain: bodies crossing between a room
/// and its neighbour, an outpost's stand-down, and the traffic at an exit.
module Fabot.Core.Tests.ArenaBorderTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures
open Fabot.Core.Tests.ArenaOutpostCaptures
open Fabot.Core.Tests.ArenaMovementCaptures

/// W17S25 as W17S26's declared outpost, with W18S25 beyond its west exits:
/// two runs of five tiles at x 0, y 15–19 and y 24–28 (#450).
let private westBorder (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let outpost = room "W17S25"

    let declared =
        { colony "W17S26" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    arena 881_000 [ mother; outpost; room "W18S25" ] [ declared ] bodies

/// The siege world's nursery with W18S25 beside it, for a resident ranger.
let private nurseryWest (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child = room "W17S25" |> withController Ownership.Ours None 1 0

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
            }
        ]

    arena 881_000 [ mother; child; room "W18S25" ] colonies bodies

/// The ticks a body crossed into a room on.
let private crossingsInto (id: string) (roomName: string) (trace: TickTrace list) =
    trace
    |> List.filter (fun t ->
        t.Events
        |> List.exists (function
            | Crossed(who, _, landing) -> who = id && landing.Room = roomName
            | _ -> false))
    |> List.map (fun t -> t.Tick)

// ---- arena capture doorway-jam (movement S0) -------------------------------
// `npm run capture -- W17S25 972061 --rooms W18S25,W15S28 --name doorwayJam`, read at live t986,357.
// The room-history replay at t972,061, the state after that tick ran: arena
// tick 0 runs as t972,062.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/972000.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/972000.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/972000.json
// Read too late for live Memory: the Raid log, ArmedSeen, Healers, ExitWatches
// and Sightings start empty, and every body of ours starts idle. The 272
// roads are laid (#560); W18S25's reservation and 5 tombstones are not
// placed, nor the anchor Spawn11 was casting.

/// W17S25 at t972,061 (the movement ADR's doorway jam): no hostile in sight;
/// guard-970892-Spawn11 stands idle on (1,15), the tile inside the west exit
/// and outside the rampart line at x = 2; hauler-971381-Spawn11, carrying 600
/// home from W18S25, lands on the exit tile (0,15) every other tick, cannot
/// step onto the guard's tile and is carried back to W18S25 (49,15) (live
/// t972,055-972,085).
let private doorwayJam () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_517 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_150
        |> withStructures
            [
                for x, y in [ 10, 24; 12, 24; 13, 25; 13, 31; 15, 31; 17, 29; 17, 31 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        11, 25
                        11, 27
                        12, 28
                        12, 30
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        16, 28
                        16, 30
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 81_200 250_000 { X = 16; Y = 34 } with
                    Energy = 386
                }
                structureOf "container" None 123_600 250_000 { X = 17; Y = 20 }
                { structureOf "rampart" (Some Side.Ours) 94_321 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 972_135
                }
                { structureOf "rampart" (Some Side.Ours) 94_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 972_116
                }
                { structureOf "rampart" (Some Side.Ours) 90_021 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 972_119
                }
                { structureOf "rampart" (Some Side.Ours) 96_931 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 972_124
                }
                { structureOf "rampart" (Some Side.Ours) 96_871 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 972_096
                }
                { structureOf "rampart" (Some Side.Ours) 91_631 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 972_098
                }
                { structureOf "rampart" (Some Side.Ours) 92_921 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 972_123
                }
                { structureOf "rampart" (Some Side.Ours) 97_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 972_085
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 972_102
                }
                { structureOf "rampart" (Some Side.Ours) 94_461 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 972_117
                }
                { structureOf "rampart" (Some Side.Ours) 94_961 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 972_066
                }
                { structureOf "rampart" (Some Side.Ours) 12_401 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 972_101
                }
                { structureOf "rampart" (Some Side.Ours) 2_701 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 972_077
                }
                { structureOf "rampart" (Some Side.Ours) 96_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 972_065
                }
                { structureOf "rampart" (Some Side.Ours) 36_201 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 972_122
                }
                { structureOf "rampart" (Some Side.Ours) 96_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 972_067
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 972_063
                }
                { structureOf "rampart" (Some Side.Ours) 95_071 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 972_076
                }
                { structureOf "rampart" (Some Side.Ours) 93_681 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 972_094
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 2_841 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 200 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 1_080 { X = 10; Y = 30 }
        |> withRoads
            [
                2, 17, 3_300
                3, 18, 3_300
                4, 19, 2_900
                5, 20, 2_900
                6, 21, 2_600
                7, 22, 13_000
                8, 23, 13_000
                9, 24, 12_000
                10, 25, 11_500
                10, 27, 25_000
                10, 29, 25_000
                11, 24, 17_500
                11, 26, 9_800
                11, 28, 18_000
                11, 30, 5_000
                12, 23, 18_000
                12, 25, 11_600
                12, 27, 12_000
                12, 29, 3_200
                13, 24, 2_300
                13, 26, 3_000
                13, 28, 3_700
                13, 30, 1_900
                14, 23, 2_400
                14, 25, 1_300
                14, 27, 3_600
                14, 29, 3_900
                14, 31, 1_700
                15, 22, 2_000
                15, 28, 4_400
                15, 30, 3_600
                15, 32, 1_700
                16, 21, 3_200
                16, 29, 1_500
                16, 31, 2_900
                16, 33, 1_300
                16, 35, 3_300
                16, 37, 4_700
                16, 38, 19_000
                16, 39, 8_000
                16, 41, 3_800
                16, 43, 2_500
                16, 44, 2_500
                17, 20, 1_800
                17, 30, 4_900
                17, 32, 3_200
                17, 34, 2_900
                17, 36, 4_800
                17, 38, 4_800
                17, 39, 8_000
                17, 40, 23_500
                17, 42, 2_800
                17, 45, 2_900
                18, 29, 4_900
                18, 31, 3_500
                18, 35, 2_600
                18, 37, 4_800
                18, 39, 4_900
                18, 41, 2_500
                18, 46, 3_000
                19, 30, 4_800
                19, 34, 3_300
                19, 36, 3_800
                19, 38, 4_900
                19, 40, 2_500
                19, 47, 3_000
                20, 31, 3_800
                20, 33, 3_300
                20, 35, 4_900
                20, 37, 4_300
                20, 39, 2_000
                20, 48, 2_700
                21, 32, 5_000
                21, 34, 3_800
                21, 36, 4_900
                21, 38, 3_700
                22, 33, 5_000
                22, 35, 4_900
                22, 37, 1_400
                23, 34, 4_900
                23, 36, 1_300
                24, 35, 3_200
                25, 34, 3_900
                26, 33, 4_300
                27, 32, 4_000
                28, 33, 7_000
                29, 34, 8_000
                30, 33, 8_500
                31, 32, 9_000
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 31; Y = 5 } with
                    Energy = 369
                }
            ]
        |> withRoads
            [
                28, 9, 2_900
                29, 8, 3_200
                29, 10, 2_800
                30, 7, 3_200
                30, 11, 3_100
                31, 6, 3_400
                31, 12, 2_700
                32, 13, 2_900
                33, 14, 2_500
                34, 15, 3_300
                35, 16, 2_500
                36, 17, 2_500
                37, 16, 2_400
                38, 15, 2_400
                39, 14, 2_300
                40, 13, 2_100
                41, 12, 2_300
                42, 13, 2_100
                43, 13, 1_900
                44, 13, 1_800
                45, 13, 2_000
                46, 14, 2_000
                47, 15, 1_900
                48, 16, 2_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_727_681
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
                        18, 34
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
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 240
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 732
                }
                { structureOf "container" None 158_100 250_000 { X = 23; Y = 29 } with
                    Energy = 824
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 972_146
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 972_106
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 972_065
                }
                { structureOf "rampart" (Some Side.Ours) 98_901 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 972_120
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 972_107
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 972_160
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 972_082
                }
                { structureOf "rampart" (Some Side.Ours) 103_101 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 972_130
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 972_123
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 33_074
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 3_700
                5, 4, 4_100
                5, 6, 3_700
                5, 44, 4_500
                6, 3, 3_200
                6, 7, 4_600
                6, 29, 2_800
                6, 43, 4_100
                6, 45, 4_900
                7, 2, 3_100
                7, 8, 3_700
                7, 28, 5_000
                7, 42, 3_700
                7, 46, 2_600
                8, 1, 3_700
                8, 9, 3_600
                8, 28, 4_300
                8, 41, 4_200
                8, 47, 3_500
                9, 10, 4_600
                9, 27, 4_700
                9, 28, 4_600
                9, 40, 3_700
                9, 48, 4_600
                10, 11, 3_700
                10, 26, 2_500
                10, 29, 2_600
                10, 39, 4_200
                11, 12, 3_300
                11, 18, 3_300
                11, 27, 3_700
                11, 30, 4_500
                11, 38, 4_800
                12, 13, 2_700
                12, 17, 4_500
                12, 25, 20_500
                12, 27, 13_200
                12, 28, 2_600
                12, 29, 4_400
                12, 31, 3_300
                12, 33, 20_100
                12, 35, 17_400
                12, 37, 4_200
                13, 14, 2_700
                13, 16, 5_000
                13, 26, 13_200
                13, 28, 3_200
                13, 30, 5_000
                13, 32, 3_000
                13, 34, 2_900
                13, 36, 4_500
                14, 15, 4_200
                14, 25, 16_500
                14, 27, 4_100
                14, 29, 3_900
                14, 31, 4_600
                14, 33, 2_900
                14, 35, 4_400
                15, 15, 4_400
                15, 26, 3_700
                15, 28, 3_700
                15, 30, 4_200
                15, 32, 3_600
                15, 34, 2_900
                16, 16, 4_400
                16, 25, 13_400
                16, 27, 4_200
                16, 29, 4_200
                16, 31, 4_900
                16, 33, 4_700
                16, 35, 3_300
                17, 17, 3_800
                17, 26, 17_700
                17, 28, 3_100
                17, 30, 3_400
                17, 32, 2_800
                17, 34, 3_100
                18, 18, 3_500
                18, 25, 17_500
                18, 27, 15_200
                18, 29, 2_500
                18, 31, 4_800
                18, 33, 4_600
                19, 19, 16_900
                19, 22, 13_800
                19, 24, 19_400
                19, 26, 14_300
                19, 28, 2_700
                19, 30, 4_700
                19, 32, 4_000
                20, 20, 17_700
                20, 21, 16_800
                20, 23, 15_500
                20, 25, 16_100
                20, 27, 15_500
                20, 29, 4_400
                20, 31, 4_600
                20, 33, 17_000
                21, 21, 3_700
                21, 22, 2_400
                21, 23, 4_900
                21, 24, 19_400
                21, 25, 17_900
                21, 26, 20_300
                21, 28, 18_400
                21, 30, 4_900
                21, 32, 18_300
                21, 34, 16_600
                22, 24, 4_300
                22, 25, 13_100
                22, 27, 21_600
                22, 28, 4_800
                22, 29, 4_000
                22, 30, 15_900
                22, 31, 19_200
                22, 32, 19_500
                22, 33, 20_200
                22, 35, 3_700
                23, 26, 16_600
                23, 28, 17_700
                23, 29, 20_100
                23, 30, 20_900
                23, 31, 13_500
                23, 32, 19_300
                23, 33, 15_200
                23, 34, 4_200
                24, 25, 14_400
                24, 28, 20_900
                24, 29, 21_500
                24, 30, 19_900
                24, 33, 15_400
                25, 24, 5_000
                25, 28, 21_100
                25, 29, 20_700
                25, 34, 21_800
                26, 23, 16_300
                27, 22, 13_500
                28, 21, 13_200
                28, 34, 20_600
                29, 20, 15_000
                30, 19, 13_400
                31, 18, 4_300
                32, 17, 4_800
                33, 16, 3_400
                34, 15, 2_500
                35, 14, 3_300
                36, 13, 18_400
                37, 12, 2_300
                38, 11, 3_600
                39, 12, 4_300
                40, 13, 13_100
                41, 14, 13_000
                42, 15, 3_000
                43, 16, 3_000
                44, 17, 2_400
                45, 18, 2_400
                46, 19, 4_000
                47, 20, 4_300
                48, 21, 3_300
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-970795-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 5 ])
                  (at "W17S25" 15 32)
                  None with
                TicksToLive = 271
            }
            { body
                  "defender-970852-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; Attack, 6 ])
                  (at "W17S25" 32 31)
                  None with
                TicksToLive = 325
            }
            { body
                  "guard-970892-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 1 15)
                  None with
                TicksToLive = 359
            }
            { body
                  "anchor-970943-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 410
                Energy = 14
            }
            { body
                  "ranger-971410-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 973
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 40 27)
                  None with
                TicksToLive = 1_255
                Energy = 46
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 13 28)
                  None with
                TicksToLive = 1_437
            }
            { body
                  "ranger-971130-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 693
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 16 21)
                  None with
                TicksToLive = 1_491
                Energy = 289
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 20 30)
                  None with
                TicksToLive = 1_318
            }
            { body
                  "hauler-971381-Spawn11"
                  Side.Ours
                  (parts [ Carry, 12; Move, 6 ])
                  (at "W17S25" 0 15)
                  None with
                TicksToLive = 872
                Energy = 600
            }
            { body
                  "anchor-970998-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 465
                Energy = 50
            }
            { body
                  "reserver-971527-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 70
            }
            { body
                  "worker-971252-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 6; Move, 5 ])
                  (at "W18S25" 31 6)
                  None with
                TicksToLive = 734
                Energy = 300
            }
            { body
                  "guard-970714-Spawn3"
                  Side.Ours
                  (parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ])
                  (at "W15S28" 7 1)
                  None with
                TicksToLive = 301
            }
            { body
                  "upgrader-970821-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 405
                Energy = 26
            }
            { body
                  "anchor-971063-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 524
                Energy = 50
            }
            { body
                  "worker-971258-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 845
                Energy = 658
            }
            { body
                  "hauler-971479-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 22 30)
                  None with
                TicksToLive = 1_060
                Energy = 1_600
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_365
                Energy = 50
            }
            { body
                  "hauler-970917-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 27 18)
                  None with
                TicksToLive = 498
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 47)
                  None with
                TicksToLive = 1_345
                Energy = 1_412
            }
        ]

    arena 972_062 [ w17s25; w18s25; w15s28 ] colonies bodies

// ---- arena capture idle-ranger-in-reach (#533, movement S0) ----------------
// `npm run capture -- W18S25 973267 --rooms W17S25,W15S28,W17S26 --name idleRangerInReach`, read at live t986,392.
// The room-history replay at t973,267, the state after that tick ran: arena
// tick 0 runs as t973,268.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/973200.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/973200.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/973200.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/973200.json
// Read too late for live Memory: W17S25's stand-down of W18S25 is seated as
// `shutOutpostCripple` seats it (#534's "Why": opened t973,265 on a Lose while
// Shade461 healed Idol768); the rest of the Raid log, ArmedSeen, Healers,
// ExitWatches and Sightings start empty, and every body of ours starts idle.
// The reservations, roads and tombstones are not placed, nor the ranger and
// the hauler W15S28 was casting.

/// W18S25 at t973,267 (#533's "Why"): the stand-down took
/// ranger-972464-Spawn8's Guard (`task-gone`) on (46,14), two tiles from
/// Idol768 (7 live ATTACK, 1 live MOVE) whom Shade461 heals from the exit
/// tile; the ranger itself is down to 2 live MOVE. Live it stood idle there,
/// the melee stepped beside it at t973,273 and struck it to death by
/// t973,294.
let private idleRangerInReach () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 796
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_311 70_919
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
                        14, 30
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 14, 26; 15, 27; 17, 31 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 66_200 250_000 { X = 16; Y = 34 } with
                    Energy = 100
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 216
                }
                { structureOf "rampart" (Some Side.Ours) 90_721 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 973_323
                }
                { structureOf "rampart" (Some Side.Ours) 90_961 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 973_304
                }
                { structureOf "rampart" (Some Side.Ours) 89_411 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 973_307
                }
                { structureOf "rampart" (Some Side.Ours) 93_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 973_312
                }
                { structureOf "rampart" (Some Side.Ours) 93_271 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 973_284
                }
                { structureOf "rampart" (Some Side.Ours) 98_531 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 973_286
                }
                { structureOf "rampart" (Some Side.Ours) 99_521 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 973_311
                }
                { structureOf "rampart" (Some Side.Ours) 93_501 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 973_273
                }
                { structureOf "rampart" (Some Side.Ours) 90_101 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 973_290
                }
                { structureOf "rampart" (Some Side.Ours) 90_861 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 973_305
                }
                { structureOf "rampart" (Some Side.Ours) 91_061 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 973_353
                }
                { structureOf "rampart" (Some Side.Ours) 8_801 3_000_000 { X = 13; Y = 29 } with
                    IsPublic = true
                    NextDecay = 973_289
                }
                { structureOf "rampart" (Some Side.Ours) 1_201 3_000_000 { X = 14; Y = 28 } with
                    IsPublic = true
                    NextDecay = 973_364
                }
                { structureOf "rampart" (Some Side.Ours) 92_421 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 32_601 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 973_310
                }
                { structureOf "rampart" (Some Side.Ours) 92_401 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 973_354
                }
                { structureOf "rampart" (Some Side.Ours) 96_401 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 973_350
                }
                { structureOf "rampart" (Some Side.Ours) 91_171 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 973_363
                }
                { structureOf "rampart" (Some Side.Ours) 90_081 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 973_282
                }
                towerOf Side.Ours { X = 13; Y = 29 } 890
            ]
        |> withSite Side.Ours "storage" None 13_863 { X = 13; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_753_712
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
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
                        18, 28
                        18, 32
                        18, 34
                        19, 31
                        19, 33
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
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 13, 25; 13, 31; 14, 26; 14, 30; 15, 27; 15, 29; 15, 31; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 13, 29; 18, 26; 19, 27; 20, 26 ] ->
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
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 156
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 120
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 24 } with
                    Energy = 50
                    Id = "extension-12-24-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 } with
                    Energy = 100
                    Id = "extension-12-30-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 973_334
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 973_294
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 973_281
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 973_295
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 973_348
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 973_270
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 973_318
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 973_311
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 31_183
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 535
                }
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 38 15)
                  None with
                TicksToLive = 749
                Energy = 50
            }
            { body
                  "reserver-972681-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W18S25" 24 5)
                  None with
                TicksToLive = 24
            }
            // ranger-972464-Spawn8: live 2 MOVE, 14 RANGED_ATTACK, 7 HEAL.
            { body
                  "ranger-972464-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W18S25" 46 14)
                  None with
                Hits = 2_286
                TicksToLive = 821
                Fatigue = 11
            }
            // Idol768: live 7 ATTACK, 1 MOVE; it stepped on the ranger as
            // soon as its fatigue ran out.
            { body
                  "Idol768"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 48 16)
                  (Some(Chase(Nearest, None))) with
                Hits = 734
                TicksToLive = 1_104
                Fatigue = 9
            }
            // Shade461: live 6 MOVE, 7 HEAL; it healed Idol768 from the exit
            // tile, crossing to W17S25 and back.
            { body
                  "Shade461"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S25" 49 15)
                  (Some(Do [ Act.Heal "Idol768" ])) with
                Hits = 1_278
                TicksToLive = 746
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 16 44)
                  None with
                TicksToLive = 49
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 12 29)
                  None with
                TicksToLive = 285
                Energy = 350
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 719
                Energy = 50
            }
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 789
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 1_020
            }
            { body "scout-W17S22-972936" odiodin (parts [ Move, 1 ]) (at "W17S25" 13 26) (Some Hold) with
                TicksToLive = 1_170
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 10 25)
                  None with
                TicksToLive = 112
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 14 31)
                  None with
                TicksToLive = 231
                Energy = 350
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_488
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 1_164
                Energy = 611
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 13 30)
                  None with
                TicksToLive = 663
                Energy = 350
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 20 48)
                  None with
                TicksToLive = 315
                Energy = 50
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 159
                Energy = 50
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 11 28)
                  None with
                TicksToLive = 631
                Fatigue = 12
                Energy = 350
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 695
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 19 26)
                  None with
                TicksToLive = 775
                Energy = 96
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 799
                Energy = 50
            }
            { body
                  "worker-972857-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_238
                Energy = 72
            }
            { body
                  "hauler-972938-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 28)
                  None with
                TicksToLive = 1_313
                Energy = 100
            }
            { body
                  "reserver-973249-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S28" 6 45)
                  None with
                TicksToLive = 586
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 1)
                  None with
                TicksToLive = 139
                Energy = 1_280
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 819
                Energy = 50
            }
            { body
                  "reserver-973148-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 491
            }
        ]

    let a = arena 973_268 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            RoomName = "W18S25"
                                            Opened = 973_265
                                            LastSeen = 973_265
                                            Expiry = 973_465
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Raiders = set [ "Idol768"; "Shade461" ]
                                            PlayerSince = Some 973_265
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture w17s26-t1014100 -------------------------------------------
// `npm run capture -- W17S26 1014100 --rooms W17S25`, read at live t1,014,280.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1014100.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1014100.json
// W17S26 is reserved by 2 to t1014110: the arena has no reservations.
// Spawn11 is spawning guard-1014093-Spawn11 until t1014153: not placed.
// Zero3956 walks its live tiles t1,014,101-149 (`observe history`), slowed to a
// step every second tick from t1,014,117, when the guard's swings took its
// three head MOVE, and swings at the core beside (13,23) from t1,014,149.

/// #613: Trepidimous' Zero3956 (18M 17A, no heal) against the lone 6M 4RA 2H
/// ranger W17S26's stand-down left idle at t1,014,093.
let private zeroAtTheCore (zero: Behaviour) () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 133_700 250_000 { X = 27; Y = 9 } with
                    Energy = 163
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    92_880
                    100_000
                    { X = 12; Y = 24 }
            ]
        |> withRoads
            [
                20, 1, 4_200
                21, 2, 4_300
                22, 3, 4_800
                23, 4, 2_500
                24, 5, 4_200
                25, 6, 2_600
                26, 7, 4_300
                27, 8, 2_500
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 79_938 52_681
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in [ 9, 23; 9, 25; 9, 27; 9, 29; 11, 23; 13, 23 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
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
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                structureOf "container" None 145_800 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 145_000 250_000 { X = 17; Y = 20 } with
                    Energy = 108
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 6_069
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withStructures
            [
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
                    ] -> structureOf "rampart" (Some Side.Ours) 100_000 10_000_000 { X = x; Y = y }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body
                  "Zero3956"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 9 1)
                  (Some zero) with
                TicksToLive = 1_459
            }
            { body
                  "ranger-1013698-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 15 19)
                  None with
                TicksToLive = 1_132
                Fatigue = 36
            }
            { body
                  "ranger-1013734-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 22 14)
                  None with
                TicksToLive = 1_168
            }
            { body
                  "reserver-1013856-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 10 22)
                  None with
                TicksToLive = 366
            }
            { body
                  "anchor-1013881-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 22 3)
                  None with
                TicksToLive = 1_309
            }
            { body
                  "guard-1013931-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S26" 17 20)
                  None with
                TicksToLive = 1_389
            }
            { body
                  "worker-1012533-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 2 17)
                  None with
                TicksToLive = 12
                Energy = 450
            }
            { body
                  "hauler-1012841-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 12 26)
                  None with
                TicksToLive = 347
            }
            { body
                  "anchor-1012949-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 371
                Energy = 50
            }
            { body
                  "hauler-1012989-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 13 26)
                  None with
                TicksToLive = 495
                Energy = 1_200
            }
            { body
                  "upgrader-1013787-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_254
            }
            { body
                  "scout-W17S22-1012929"
                  odiodin
                  (parts [ Move, 1 ])
                  (at "W17S25" 14 14)
                  (Some Hold) with
                TicksToLive = 330
            }
        ]

    let a = arena 1_014_101 [ w17s26; w17s25 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-1013734-Spawn11", "guard:W19S25"
                            "hauler-1012841-Spawn11", "withdraw:storage-13-27"
                            "anchor-1012949-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-1012989-Spawn11", "withdraw:storage-13-27"
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
                                            RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_014_100
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
                                            RoomName = "W17S26"
                                            Opened = 1_014_093
                                            LastSeen = 1_014_100
                                            Expiry = 1_014_293
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.ofList [ "Zero3956" ]
                                            PlayerSince = Some 1_014_093
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_014_100
                                            Expiry = 1_016_242
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                                Threatened =
                                    Map.ofList
                                        [
                                            "W17S26",
                                            ({ Until = 1_015_593 }: Fabot.Core.Observe.ThreatLatch)
                                        ]
                                HomeRaided = Some 1_013_957
                            }
                        ]
            }
    }

/// Zero3956's live tiles, t1,014,101-149, to the core it swings at.
let private zeroLiveTiles =
    [
        for x in 10..20 -> RoomPos.at "W17S26" { X = x; Y = 1 }
        for x, y in
            [
                21, 2
                22, 3
                23, 4
                24, 5
                25, 6
                26, 7
                27, 8
                26, 9
                25, 10
                24, 11
                23, 12
                22, 13
                22, 14
                21, 15
                20, 16
                19, 17
                18, 18
                18, 19
                17, 20
                16, 21
                15, 21
                14, 22
                13, 23
            ] -> RoomPos.at "W17S26" { X = x; Y = y }
    ]

/// An arena run a tick at a time: the trace, and one creep's Task at the end
/// of every tick ("" for none).
let private taskEachTick (creep: string) (ticks: int) (start: Arena) =
    let rec go n (a: Arena) traces tasks =
        if n = 0 then
            List.rev traces |> List.concat, List.rev tasks
        else
            let next, trace = a |> run 1
            let task = Map.tryFind creep next.Carried.Assignments |> Option.defaultValue ""
            go (n - 1) next (trace :: traces) (task :: tasks)

    go ticks start [] []

/// #618 on the t1,037,961 capture: Synapse7330 alone walks off our W17S26
/// container out over the west border into W18S26 (seen where `lookout`
/// names it) and back, twice. The ranger holds W17S26's Guard from taking
/// it until the hauler dies or the run ends, and shoots it on its returns.
let private hunterDancesWith (lookout: Set<string>) (adjust: Arena -> Arena) =
    let ranger = "ranger-1037139-Spawn11"
    let at roomName x y = RoomPos.at roomName { X = x; Y = y }

    let start =
        let a = haulersAtTheContainer () |> adjust

        { a with
            Rooms = a.Rooms |> Map.add "W18S26" (room "W18S26")
            Lookout = Set.union a.Lookout lookout
            Bodies =
                a.Bodies
                |> List.filter (fun b -> b.Id <> "Nova7626")
                |> List.map (fun b ->
                    if b.Id = "Synapse7330" then
                        { b with
                            Script =
                                Some(
                                    Phases
                                        [
                                            35, GoTo(at "W18S26" 46 10)
                                            60, GoTo(at "W17S26" 3 13)
                                            90, GoTo(at "W18S26" 46 10)
                                            0, GoTo(at "W17S26" 3 13)
                                        ]
                                )
                        }
                    else
                        b)
        }

    let trace, tasks = start |> taskEachTick ranger 140
    let failure = describe trace

    let out =
        trace
        |> List.filter (fun t ->
            t.Events
            |> List.exists (function
                | Crossed("Synapse7330", from, _) -> from.Room = "W17S26"
                | _ -> false))

    Expect.isGreaterThanOrEqual
        (List.length out)
        2
        $"the premise: the hauler steps out of W17S26 twice\n{failure}"

    let taken = tasks |> List.findIndex ((=) "guard:W17S26")
    let held = tasks |> List.skip taken

    let ends =
        match diedOn "Synapse7330" trace with
        | Some died -> died - taken
        | None -> List.length held

    Expect.isTrue
        (held |> List.take ends |> List.forall ((=) "guard:W17S26"))
        $"""the ranger holds W17S26's Guard from t{taken}: {String.concat "," tasks}{"\n"}{failure}"""

    // Camped where it stepped out, the ranger shoots it each time it comes
    // back.
    let hitsAt (t: TickTrace) =
        t.Bodies |> List.tryFind (fun b -> b.Id = "Synapse7330") |> Option.map _.Hits

    Expect.isLessThan
        (trace |> List.last |> hitsAt |> Option.defaultValue 0)
        (trace |> List.item (List.head out).Tick |> hitsAt |> Option.defaultValue 0)
        $"and it is shot on its returns\n{failure}"

let private hunterKeepsTheDancer (lookout: Set<string>) = hunterDancesWith lookout id

[<Tests>]
let arenaBorderTests =
    testList
        "arena border play"
        [
            // #613 (live t1,014,093-291): the idle ranger walked in toward
            // Zero3956 and off again for 200 ticks, and the one fit ranger
            // went elsewhere: `Gait.fallsBehind` asked the melee to fall
            // behind on road too, where 15 MOVE under 17 ATTACK ties a
            // ranger, so no ranger kited the body that steps every second
            // tick on plain, and #602's release kept no kiter it once caught.
            // The live Zero3956 stood at the core it broke; here it walks its
            // live tiles and then chases, as our pricing must assume it does.
            // The capture parts from live at t16: the live guard stalled a
            // tile short and traded 360 with the melee, slowing it from
            // t1,014,117; here the guard flees untouched, the melee keeps its
            // pace, and the idle ranger-1013698 is caught at t23 (live never).
            // Moved by #625: Zero3956 is at the ranger's pace with its MOVE
            // first, so ranger-1013734 takes the Guard at t0 (was t22, once
            // the melee's second MOVE fell) and kites it from there; the kill
            // is t81, where it was t132. The pocket at (20..21, 11..12) that
            // caught the idle ranger still catches a ranger there, now the
            // holder, struck once at t24 (486, as the idle one was) by the
            // lamed melee: it flees for a tick, stands two, and takes the
            // Guard back. Moved by #615 (the kite walk reads the run-off it is
            // priced by): the kill is t82 (t81), the strike at t24 as before.
            test
                "a lone ranger takes the Guard of a melee at its pace it lames, keeps it but for one strike, and kites it dead" {
                let trace, tasks =
                    zeroAtTheCore
                        (Phases [ 50, Waypoints zeroLiveTiles; 0, Chase(Nearest, None) ])
                        ()
                    |> taskEachTick "ranger-1013734-Spawn11" 160

                let guard = "guard:W17S26"
                let died = diedOn "Zero3956" trace

                Expect.isSome died $"Zero3956 dies to the kite\n{describe trace}"
                Expect.isLessThanOrEqual died.Value 82 $"by t82\n{describe trace}"

                let taken = tasks |> List.findIndex ((=) guard)
                Expect.equal taken 0 "ranger-1013734 takes the Guard at once"

                Expect.isLessThanOrEqual
                    (tasks.[taken .. died.Value] |> List.filter ((<>) guard) |> List.length)
                    3
                    $"ranger-1013734 keeps the Guard until the melee dies but for one strike's flee: {String.concat (string ',') tasks}\n{describe trace}"

                Expect.isTrue (survived "ranger-1013734-Spawn11" trace) "the kiter lives"
                Expect.isTrue (survived "ranger-1013698-Spawn11" trace) "and so does the idle one"
            }

            // #615 U3's hysteresis: live, ranger-1013698 took W17S26's Guard,
            // walked in, was let go and walked off, again and again (#613).
            // One answer for taking and keeping, the taker priced once struck
            // to its next decision: neither ranger turns its Guard over more
            // than once each way in 500 ticks, longer than a ranger heals out
            // its wounds in.
            test "neither ranger takes and drops the Guard more than once over 500 ticks" {
                let flips creep =
                    let _, tasks =
                        zeroAtTheCore
                            (Phases [ 50, Waypoints zeroLiveTiles; 0, Chase(Nearest, None) ])
                            ()
                        |> taskEachTick creep 500

                    tasks
                    |> List.map ((=) "guard:W17S26")
                    |> List.pairwise
                    |> List.filter (fun (before, now) -> before <> now)
                    |> List.length

                for creep in [ "ranger-1013698-Spawn11"; "ranger-1013734-Spawn11" ] do
                    Expect.isLessThanOrEqual
                        (flips creep)
                        2
                        $"{creep} flips its Guard at most twice"
            }
            // Live (t985,560-985,681) both rangers stood beside the core for
            // 100+ ticks shooting nothing: the core claimed the room ahead of
            // the hauler's kill. On main this capture reproduces that (both on
            // the core's ring, no shot, the core untouched over 40 ticks).
            // Characterisation (movement S0): #558's capture for this stage; the
            // range-3 core ring is a stand tile S4's picker takes over, keeping
            // the outcome.
            test
                "#558 capture t985,660: both rangers shoot Obsidian522 down, then one shoots the core from three, never beside it" {
                let _, trace = haulerBesideCore () |> run 80
                let failure = describe trace
                let rangers = [ "ranger-984393-Spawn3"; "ranger-985441-Spawn11" ]
                let core = RoomPos.at "W17S26" { X = 12; Y = 25 }

                let shotAt target (t: TickTrace) ranger =
                    t.Ours |> List.contains (RangedAttackCreep(ranger, target))

                let dead =
                    match diedOn "Obsidian522" trace with
                    | Some tick -> tick
                    | None -> failtest $"Obsidian522 dies\n{failure}"

                // The child's 600-hit ranger-985441 keeps its seat on W17S26's
                // trespass (#601); before, it stepped between this Guard and its
                // home's and never shot, and the hauler died to the other alone
                // at t35.
                Expect.isLessThanOrEqual dead 31 $"at 160 a tick, by t31\n{failure}"

                // The one beside the core shoots it at t0, on its way to the
                // creep, which stands out of its reach.
                Expect.isTrue
                    (trace
                     |> List.filter (fun t -> t.Tick >= 5 && t.Tick < dead)
                     |> List.forall (fun t -> rangers |> List.forall (shotAt "Obsidian522" t)))
                    $"both shoot it every tick from t5 until it dies\n{failure}"

                let coreShots =
                    trace
                    |> List.collect (fun t ->
                        t.Ours
                        |> List.choose (function
                            | RangedAttackStructure(ranger, "invaderCore-12-25") ->
                                Some(t.Tick, ranger)
                            | _ -> None))

                let afterward = coreShots |> List.filter (fun (tick, _) -> tick > dead)
                Expect.isNonEmpty afterward $"a ranger then shoots the core\n{failure}"

                for tick, ranger in afterward do
                    let at =
                        pathOf ranger trace
                        |> List.tryFind (fun (t, _) -> t = tick - 1)
                        |> Option.bind (fun (_, s) -> RoomPos.range s.At core)

                    Expect.equal
                        at
                        (Some Engine.rangedRange)
                        $"{ranger} shoots from three at t{tick}\n{failure}"

                // Moved by #649: ranger-984393, its seat given up at t57 once
                // the guard comes to break the core, walks north past the
                // core's ring (13,24 at t59) where it went round east: the
                // other colony's guard walking in is no longer priced as
                // standing on its way. One tick on the ring in passing is no
                // seat there; two running would be.
                for ranger in rangers do
                    Expect.isFalse
                        (pathOf ranger trace
                         |> List.filter (fun (tick, _) -> tick > dead)
                         |> List.pairwise
                         |> List.exists (fun ((_, s), (_, next)) ->
                             [ s; next ]
                             |> List.forall (fun b ->
                                 RoomPos.range b.At core |> Option.exists (fun r -> r <= 1))))
                        $"{ranger} never stands beside the core two ticks running once the creep is dead\n{failure}"

                Expect.isLessThan
                    (trace |> List.last |> (fun t -> Map.find "invaderCore-12-25" t.Structures))
                    66_670
                    $"the core is struck\n{failure}"
            }
            test "a guard that crosses in steps off the border ring rather than being carried back" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

                let _, trace = outpostRaid invader |> run 30
                let guard = "guard-1-Spawn1"
                let path = pathOf guard trace |> Map.ofList

                let crossings =
                    trace
                    |> List.choose (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | Crossed(who, _, _) when who = guard -> Some t.Tick
                            | _ -> None))

                Expect.isNonEmpty crossings "the guard crossed at least once"

                for tick in crossings do
                    match Map.tryFind (tick + 1) path with
                    | Some next ->
                        Expect.isFalse
                            (Seam.onRing (RoomPos.pos next.At))
                            $"t{tick + 1}: off the ring after crossing\n{describe trace}"
                    | None -> ()
            }

            test "#450: a guard holds the exit a kiter left by and hits it on re-entry" {
                // Seven HEAL out-heal the kiter's 80 a tick: a guard the kiter
                // out-damages, and cannot strike back at range, flees (#533,
                // #535) instead of holding.
                let guard =
                    body
                        "guard-1-Spawn8"
                        Side.Ours
                        (parts [ Tough, 3; Move, 25; Attack, 15; Heal, 7 ])
                        (RoomPos.at "W17S25" { X = 14; Y = 17 })
                        None

                let kiter =
                    body
                        "Kiter1"
                        trep
                        (parts [ Move, 10; RangedAttack, 8; Heal, 2 ])
                        (RoomPos.at "W17S25" { X = 5; Y = 17 })
                        (Some(
                            Phases
                                [
                                    6, Kite(Nearest, 3)
                                    30, GoTo(w18s25 44 17)
                                    200, GoTo(RoomPos.at "W17S25" { X = 3; Y = 17 })
                                ]
                        ))

                // The guard already holds the outpost's Guard (#535): a kiter as
                // quick as it is the ranger row's to answer, but the fight the
                // guard holds is its own while it can strike the kiter.
                let start =
                    let a = westBorder [ guard; kiter ]

                    { a with
                        Carried =
                            { a.Carried with
                                Assignments = Map.ofList [ "guard-1-Spawn8", "guard:W17S25" ]
                            }
                    }

                let _, trace = start |> run 45
                let back = crossingsInto "Kiter1" "W17S25" trace
                Expect.isNonEmpty back $"the kiter came back\n{describe trace}"

                // The guard holds the run (#450): it stands within one of the
                // exit tiles, off the ring, while the kiter is away.
                let away = crossingsInto "Kiter1" "W18S25" trace |> List.head
                let landed = List.head back

                let held =
                    pathOf "guard-1-Spawn8" trace
                    |> List.filter (fun (tick, _) -> tick > away + 3 && tick <= landed)

                for tick, s in held do
                    Expect.equal
                        s.At.X
                        1
                        $"t{tick}: holding beside the west exits\n{describe trace}"

                // Five exit tiles are more than one melee covers: the kiter
                // lands on the run's end, two from the guard, and is hit the
                // tick after it steps in.
                let struck =
                    trace
                    |> List.filter (fun t -> t.Tick > landed && t.Tick <= landed + 2)
                    |> List.exists (fun t ->
                        t.Ours |> List.contains (AttackCreep("guard-1-Spawn8", "Kiter1")))

                Expect.isTrue
                    struck
                    $"the guard hit it within two ticks of t{landed}\n{describe trace}"

                Expect.equal
                    (roomsOf "guard-1-Spawn8" trace)
                    [ "W17S25" ]
                    "and never crossed after it"
            }

            test
                "a hurt melee retreats toward the border shot down to its legs, and our ranger does not follow" {
                let ranger =
                    body
                        "ranger-880900-Spawn8"
                        Side.Ours
                        (parts [ Move, 24; RangedAttack, 16; Heal, 8 ])
                        (w17s25 16 17)
                        None

                let anchor = w17s25 6 17

                // Legs at the tail, so a hurt body still walks out; each falls
                // back at its first wound and comes back healed.
                let refuge = w18s25 44 17

                let melee =
                    body
                        "Brute1"
                        trep
                        (parts [ Tough, 4; Attack, 6; Move, 10 ])
                        (w17s25 3 16)
                        (Some(Retreat(1900, refuge, 2000, Chase(Nearest, Some(anchor, 49)))))

                let healer =
                    body
                        "Medic1"
                        trep
                        (parts [ Heal, 4; Move, 4 ])
                        (w17s25 2 17)
                        (Some(Retreat(760, refuge, 800, Follow "Brute1")))

                let _, trace = nurseryWest [ ranger; melee; healer ] |> run 40

                // Moved by #625: the brute is at the ranger's pace, its legs
                // last, and the ranger's 160 a tick take its ten head parts
                // inside the ground it has, so the ranger keeps it off and
                // shoots it from range three where it closed and traded
                // before (#598: the medic beside the brute died short of the
                // exit and the brute left at 1,776). The brute is shot to its
                // legs short of the exit; the medic, behind it and out of the
                // kiter's reach, now lives. Moved by #648: shot disarmed, the
                // brute is no raider, but its kill still pays (#621's score:
                // its body, lamed behind the ranger, nothing healing it), so
                // the garrison's ground follows it off the controller's ring
                // and the ranger shoots it dead where, before, it walked back
                // to the ring and the medic healed the brute back over the
                // border. Moved by #615 (the kite walk reads the run-off it
                // is priced by): the ranger waits on its tile while the raid
                // walks in, shoots from range three from t12 untouched, kills
                // the medic at t21 and the brute at t31, past the 30 ticks
                // this ran (now 40), where the medic lived.
                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Events
                         |> List.exists (function
                             | Died who -> who = "Brute1"
                             | _ -> false)))
                    $"the disarmed brute is killed\n{describe trace}"

                let brute =
                    trace
                    |> List.collect (fun t -> t.Bodies |> List.filter (fun b -> b.Id = "Brute1"))
                    |> List.map _.Hits
                    |> List.min

                Expect.isLessThanOrEqual
                    brute
                    (6 * Engine.partHits + 4 * Engine.partHits)
                    $"its TOUGH and ATTACK are shot off\n{describe trace}"

                Expect.isEmpty (crossingsInto "Brute1" "W18S25" trace) "before it reaches the exit"

                Expect.isEmpty
                    (crossingsInto "ranger-880900-Spawn8" "W18S25" trace)
                    $"the ranger did not follow\n{describe trace}"

                // #598: half the raid going ends nothing for the half
                // still in our room; the ranger fires every tick a raider
                // stands in its reach there.
                let shots =
                    trace
                    |> List.map (fun t ->
                        t.Tick,
                        t.Ours
                        |> List.exists (function
                            | RangedAttackCreep(who, _)
                            | RangedMassAttack who -> who = ranger.Id
                            | _ -> false))
                    |> Map.ofList

                let inReach =
                    trace
                    |> List.choose (fun t ->
                        let at id =
                            t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map _.At

                        let reached raider =
                            match at raider, at ranger.Id with
                            | Some them, Some us ->
                                them.Room = "W17S25"
                                && RoomPos.range them us |> Option.exists (fun r -> r <= 3)
                            | _ -> false

                        if reached "Brute1" || reached "Medic1" then
                            Some(t.Tick + 1)
                        else
                            None)

                for tick in inReach do
                    if Map.containsKey tick shots then
                        Expect.isTrue
                            shots[tick]
                            $"t{tick}: the ranger fires while a raider stands in reach in W17S25\n{describe trace}"
            }

            // As live (t972,617-972,661): the guard kills Wraith403 on its post
            // by t972,620; while Wraith403 stands in W17S26 no resident is lent
            // to Crunchy047 next door (HomeHold, `Facts.homeHoldReleases`);
            // once it is dead one is, and
            // kills Crunchy047 by t972,661. Wraith403 has no weapon left and
            // Crunchy047 steps once in ~17 ticks, so neither reaches the home
            // inside the lend's window (#531).
            test
                "#532 capture t972,617: while Wraith403 stands in W17S26 the residents hold home, then one kills Crunchy047, as live" {
                let _, trace = twoRaidsBesideW17S25 () |> run 80
                let failure = describe trace

                let wraithDead =
                    match diedOn "Wraith403" trace with
                    | Some tick -> tick
                    | None -> failtest $"the guard kills Wraith403\n{failure}"

                Expect.isLessThanOrEqual wraithDead 3 $"on its post, by t972,620\n{failure}"

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours |> List.contains (AttackCreep("guard-972528-Spawn11", "Wraith403"))))
                    "the guard swings at it"

                for ranger in [ "ranger-971130-Spawn3"; "ranger-971410-Spawn3" ] do
                    Expect.equal
                        (pathOf ranger trace
                         |> List.filter (fun (tick, _) -> tick <= wraithDead)
                         |> List.map (fun (_, s) -> s.At.Room)
                         |> List.distinct)
                        [ "W17S25" ]
                        $"{ranger} holds home while Wraith403 lives\n{failure}"

                // Moved 2026-10-06 (#550's ranger row, live t981,500): the
                // other resident is called to W17S26's 23C23M hauler the guard
                // there cannot catch, where it used to stay home because the
                // guard of W17S25's held the kill.
                Expect.equal
                    (roomsOf "ranger-971130-Spawn3" trace)
                    [ "W17S25"; "W17S26" ]
                    $"the other resident is called to W17S26's hauler\n{failure}"

                Expect.isNonEmpty
                    (crossingsInto "ranger-971410-Spawn3" "W18S25" trace)
                    $"the other is lent to W18S25\n{failure}"

                Expect.isSome (diedOn "Crunchy047" trace) $"and kills Crunchy047\n{failure}"
            }

            // The world layer's S1 (#534) flipped the S0 pin here, which read
            // as live (t973,321-973,405): nobody answered the cripple, and the
            // log's last sighting stayed the one that shut it. Now the
            // observer's look reaches the mother's watch of the shut room and
            // W17S25's log: the resident next door is called while the room is
            // still shut, and kills it.
            test
                "#534 capture t973,320: a cripple alone in the child's shut outpost is answered by the mother's resident while the room is shut" {
                let resident = "ranger-972663-Spawn3"

                let rowOf (a: Arena) =
                    Map.tryFind "W17S25" a.Carried.Raids
                    |> Option.bind (fun raids ->
                        raids.Outposts |> List.tryFind (fun row -> row.RoomName = "W18S25"))

                let inside (a: Arena) =
                    a.Bodies |> List.exists (fun b -> b.Id = resident && b.At.Room = "W18S25")

                let entered, toEntry = shutOutpostCripple () |> runUntil inside 85
                let failure = describe toEntry

                Expect.isTrue (inside entered) $"the resident enters W18S25\n{failure}"

                Expect.isTrue
                    (rowOf entered
                     |> Option.exists (fun row -> Fabot.Core.Observe.standingDown entered.Time row))
                    $"while W17S25's stand-down still shuts it\n{failure}"

                Expect.isTrue
                    (rowOf entered |> Option.exists (fun row -> row.LastSeen > 973_265))
                    $"and the log has read the observer's look\n{failure}"

                let _, trace = shutOutpostCripple () |> run 85

                Expect.isSome (diedOn "Idol768" trace) $"Idol768 dies\n{describe trace}"
            }

            // Live (t976,867-976,972) Specter836 dug W17S26 for ~100 ticks while
            // both W17S25 guards stood idle: the exit held for Paragon731, which
            // nobody held, claimed the room ahead of the trespass and pooled no
            // Guard until it ran out at t976,972 (#542). A claim that pools
            // nothing shadows nothing: the kill is pooled at t0.
            test
                "#542 capture t976,955: a held exit nobody holds leaves Specter836 to the trespass's kill at once" {
                let first, opening = diggerBesideHeldExit () |> step

                let holders =
                    first.Carried.Assignments
                    |> Map.filter (fun _ task -> task = "guard:W17S26")
                    |> Map.keys
                    |> List.ofSeq

                Expect.isNonEmpty
                    holders
                    $"a body takes W17S26's Guard at t0\n{describe [ opening ]}"

                let _, trace = diggerBesideHeldExit () |> run 60

                Expect.isSome (diedOn "Specter836" trace) $"and Specter836 dies\n{describe trace}"
            }

            // #656: a held exit gives way to the room's prey only where its
            // holder kills a trespasser and is back before the raid that left
            // by it is due (`Facts.preyBeforeReturn`). Live (t1,020,780-830)
            // ranger-1019663 stood on (1,42)-(1,45) while Virtual9490, which
            // left by (0,40) ~35 ticks before and bounced just across, out of
            // our sight, came back in at t1,020,800: reproduced, the ranger
            // keeps the exit, the prey (the nearest, Dragon9899, ~40 tiles off,
            // 2,060 hits to its 40 a tick) out of reach of a kill and a walk
            // back inside that. Once the hold has lapsed, the prey is its kill.
            test
                "#656 capture t1,020,780: the ranger keeps the exit a kiter just left by, and goes for the prey once the hold lapses" {
                let ranger = "ranger-1019663-Spawn11"

                let tilesOf (trace: TickTrace list) =
                    trace
                    |> List.choose (fun t ->
                        t.Bodies |> List.tryFind (fun b -> b.Id = ranger) |> Option.map _.At)

                // The room's Guard as W17S25 reads it, its holder the ranger.
                let briefOf (a: Arena) =
                    let view = viewOf a "W17S25"
                    let atlas = Fabot.Core.Atlas.ofView view

                    let threats =
                        Fabot.Core.Decide.Threat.threatsOfHeld
                            view
                            atlas
                            (Fabot.Core.Decide.Facts.heldTaskFacts view a.Carried.Assignments)

                    let brief = Fabot.Core.Decide.Threat.Threats.briefOf threats "W17S26"
                    brief.Duty, Fabot.Core.Decide.Threat.Brief.isKill brief

                let start = kiterAcrossHeldExit ()

                Expect.equal
                    (briefOf start)
                    (Fabot.Core.Decide.Threat.Duty.Guard Fabot.Core.Decide.Threat.BriefKind.ExitHold,
                     false)
                    "the kiter gone ~35 ticks: the held exit's Guard"

                let held, trace = start |> run 30

                Expect.equal
                    (Map.tryFind ranger held.Carried.Assignments)
                    (Some "guard:W17S26")
                    $"it keeps W17S26's Guard\n{describe trace}"

                Expect.isTrue
                    (tilesOf trace
                     |> List.forall (fun at -> at.Room = "W17S26" && at.X <= 3 && at.Y >= 38))
                    $"and stands by the west exit's run, as live: {tilesOf trace}"

                let lapsed =
                    { start with
                        Carried =
                            { start.Carried with
                                ExitWatches = Map.empty
                            }
                    }

                Expect.equal
                    (briefOf lapsed)
                    (Fabot.Core.Decide.Threat.Duty.Guard
                        Fabot.Core.Decide.Threat.BriefKind.Contested,
                     true)
                    "the hold lapsed: the contested room's prey, a kill"

                let _, trace = lapsed |> run 30

                // Off the run, along the west edge north to Dragon9899 at (8,2),
                // the prey nearest it.
                Expect.isTrue
                    (tilesOf trace |> List.last |> (fun at -> at.Y < 35))
                    $"the hold lapsed, it walks off the exit's run to the prey: {tilesOf trace}"
            }

            // Live (t979,593-980,050) guard-978780 stood on Gambit229's store
            // ring at (26,7), counted itself arrived, and struck nothing for
            // ~450 ticks (#550). Beside Onyx632, slower than the guard, the
            // kill's ground is beside each trespasser. No ranger holds the
            // kill, so W17S25's guards answer all four, two at most; the mother's
            // resident is lent to her own kill there, its ranger row asked for
            // the haulers, but this arena holds no room between W15S28 and
            // W17S26 for it to walk.
            test
                "#550 capture t979,950: the guard walks off the hauler's store ring and kills the trespassers" {
                // #551's sway kept Onyx632's step every tick with two MOVE more,
                // on a road the arena does not lay; the guard's catch reads
                // plain, where its live 3M5W falls behind.
                let start =
                    let a = swayingTrespassers ()

                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = "Onyx632" then
                                    { b with
                                        Parts = parts [ Move, 3; Work, 5 ]
                                    }
                                else
                                    b)
                    }

                let guard = "guard-978780-Spawn11"

                let briefIn home =
                    let view = viewOf start home
                    let atlas = Fabot.Core.Atlas.ofView view
                    let threats = Fabot.Core.Decide.Threat.threatsOf view atlas
                    view, threats, Fabot.Core.Decide.Threat.Threats.briefOf threats "W17S26"

                let _, threats, brief = briefIn "W17S25"

                // Moved by #654: the split asked two guards and two rangers;
                // the one cast that kills the most of the four, cheapest, is
                // two rangers, which reach the haulers a swing never catches.
                // The guard standing there keeps its seat (`trespassSplit`).
                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted threats))
                    [ CastRow.Fighter BodyTemplate.Ranged, Engine.guardCap ]
                    "its one cast is the rangers that reach all four"

                let mother, threats, brief = briefIn "W15S28"

                // The W6 S3 Caster floor moved this: W15S28's stock in the capture is
                // under `Tuning.CasterReserve` and no ranger of hers stands in
                // W17S25, so the Caster hands her no garrison there (live she
                // kept one by kinship), her ranger is no resident to lend and
                // her ranger row is asked for nothing.
                Expect.isEmpty
                    mother.Borrowed.Garrisoned
                    "the mother garrisons no home under the reserve"

                Expect.isFalse
                    (mother.Fighters
                     |> List.filter brief.Admits
                     |> List.exists (fun c -> c.Name = "ranger-979811-Spawn8"))
                    "so her ranger is no resident lent to the kill"

                Expect.isFalse
                    (brief.Asks
                     |> List.exists (fun ask ->
                         ask.Row = CastRow.Fighter BodyTemplate.Ranged && ask.Wanted threats > 0))
                    "and her ranger row is asked for nothing there"

                let _, trace = start |> run 90
                let failure = describe trace

                Expect.isTrue
                    (trace
                     |> List.take 10
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | AttackCreep(name, _) -> name = guard
                             | _ -> false)))
                    $"the guard walks to the trespassers and strikes within ten ticks\n{failure}"

                for id in [ "Onyx632"; "Morgana682" ] do
                    Expect.isSome (diedOn id trace) $"{id} dies\n{failure}"

                // Left with Gambit229 at its pace and Infinity425 crossing, the
                // guard waited on the store ring until #619: Gambit229, four off
                // the container, is beside itself, and the guards kill it there.
                Expect.isSome (diedOn "Gambit229" trace) $"and then Gambit229 dies\n{failure}"
            }

            // Live (t974,722-974,950) the anchor stepped onto the invader's
            // tile every tick and only walked once it died. A body we cannot
            // move is ground nobody of ours can take, an ally's as much as an
            // enemy's: Odiodin's creep on the same tile is walked round too.
            test
                "#536 capture t974,800: the anchor walks round a body rooted on its step to its seat, invader or ally" {
                let anchor = "anchor-974661-Spawn3"
                let seat = RoomPos.at "W15S27" { X = 13; Y = 29 }

                let asAlly (a: Arena) =
                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = "invader_W15S27_250" then
                                    { b with
                                        Side = Side.Player "Odiodin"
                                        Script = Some Hold
                                    }
                                else
                                    b)
                    }

                for label, edit in [ "invader", id; "ally", asAlly ] do
                    let _, trace = rootedInvaderOnAnchorStep () |> edit |> run 30
                    let failure = describe trace

                    Expect.isTrue
                        (pathOf anchor trace |> List.exists (fun (_, s) -> s.At = seat))
                        $"beside the {label}, the anchor reaches its seat\n{failure}"
            }

            // Live (t979,951-979,970) the hauler stepped every tick onto the
            // tile a swaying trespasser was entering, and stood. A hostile
            // that stepped since the last tick also stands on the tile it
            // left, its likeliest next one. Characterisation (movement S0):
            // the capture for #551's row (moving blockers); S5 swaps the
            // mechanism (`Left` plus an `A <= 1` surcharge, Decided 4) and
            // must keep this outcome.
            test
                "#551 capture t979,950: the hauler walks round two swaying trespassers to its container" {
                let hauler = "hauler-979756-Spawn11"
                let container = RoomPos.at "W17S26" { X = 27; Y = 9 }
                let _, trace = swayingTrespassers () |> run 20

                Expect.isTrue
                    (pathOf hauler trace
                     |> List.exists (fun (_, s) -> RoomPos.range s.At container = Some 1))
                    $"the hauler reaches its container\n{describe trace}"
            }

            // Movement S3 (`docs/adr/0088`) flipped the S0 pin here. Live
            // (t986,006-986,012) the ranger stepped to range 2 of HalfLife847
            // to shoot, and was struck four times, t3-t6 (4,200 to 2,496);
            // S0 pinned one strike at t6, S2 (whole ticks) at t4. The melee
            // walks its live road tiles (#560). Now the ranger's walk keeps off
            // every tile the melee's map strikes before the ranger, by its own
            // timing, could be on it and gone (`Threats.kiteUnsafeFor`): it
            // holds at (25,7) while the melee is far, then steps and shoots
            // each tick a step ahead of it, never struck, until its life runs
            // out at t11 (as live). The melee dies to ranger-984683 at t36
            // (#595 below).
            test
                "movement S3 capture t986,005: a ranger holding the Guard kites the melee down untouched" {
                let ranger = "ranger-984393-Spawn3"
                let start = rangerIntoMelee ()

                // Moved by #625: on t0 the child's 1-block ranger-985441,
                // which now keeps the melee at its pace off, takes the seat,
                // and this one W17S25's Guard Obsidian522 crosses into; it
                // holds W17S26's from t1.
                let second, _ = start |> run 2

                Expect.equal
                    (Map.tryFind ranger second.Carried.Assignments)
                    (Some "guard:W17S26")
                    "the ranger holds W17S26's Guard"

                let gone (a: Arena) =
                    a.Bodies |> List.forall (fun b -> b.Id <> "HalfLife847")

                let _, trace = start |> runUntil gone 200
                let failure = describe trace
                let lived = pathOf ranger trace

                Expect.isTrue
                    (lived |> List.forall (fun (_, s) -> s.Hits = 4_200))
                    $"the melee never strikes the ranger\n{failure}"

                let melee = pathOf "HalfLife847" trace |> Map.ofList

                Expect.isTrue
                    (lived
                     |> List.filter (fun (tick, _) -> tick >= 3)
                     |> List.forall (fun (tick, _) ->
                         match Map.tryFind (tick - 1) melee, Map.tryFind tick melee with
                         | Some before, Some now -> now.Hits < before.Hits
                         | _ -> false))
                    $"it shoots the melee every tick from t3 while it lives\n{failure}"

                // #595: the mother's ranger-984683 (14 RANGED_ATTACK) enters at
                // t13 by (18,0). Every tile of its kite ground is one the
                // melee strikes before the ranger, twelve tiles off, could be
                // there and gone, so S3's walk found none and it parked on
                // (17,1) until t67 while the melee stood killable at 2,030
                // hits (main: first shot t68, kill t74). Now it closes on the
                // ground by the safe tiles nearest it, waits a step off it for
                // the melee's fatigue, steps in at t23 and shoots from t24.
                let mother = pathOf "ranger-984683-Spawn3" trace

                Expect.isTrue
                    (mother |> List.forall (fun (_, s) -> s.Hits = 4_200))
                    $"the melee never strikes the mother's ranger\n{failure}"

                let firstShot =
                    trace
                    |> List.tryFind (fun t ->
                        t.Ours
                        |> List.contains (RangedAttackCreep("ranger-984683-Spawn3", "HalfLife847")))
                    |> Option.map (fun t -> t.Tick)

                // #615 U1 moved t24 to t22 and t36 to t34: the melee ties the
                // mother's ranger on plain, so it stands off nothing, and the
                // ranger, winning beside the child's, closes to its reach
                // rather than waiting a step off the ground. #615 U3 moved them
                // to t21 and t32: the mother's two rangers are each other's
                // side (the child's 2-RANGED one no longer fits beside them),
                // so the first closes sooner, the second beside it. #625 moved
                // them to t24 and t36: the child's ranger keeps the melee at
                // its pace off and fits the kite again, so the kite seats it
                // beside the first of the mother's (`Facts.kiteSeats`: the
                // fewest that kill before the raid ages out, and every body
                // already at the fight), and the second, 140 a tick more, is
                // not sent. Slower to the kill by four ticks, nobody struck.
                // The kite exchange (#615) moved the first shot to t22: the
                // ground is the band's tiles within three a ranger trades best
                // on (`kiteExchangeOn`, the melee's swing priced off its body,
                // a tile nearer where it can step first), where it was a fixed
                // band of two, else three, its escape room read over that band
                // alone; every holder seated on it in one priced pass. Two
                // ticks sooner to the first shot, three to the kill (t33),
                // nobody struck.
                Expect.equal
                    firstShot
                    (Some 22)
                    $"it shoots the melee from t22 (main t68)\n{failure}"

                Expect.equal
                    (diedOn "HalfLife847" trace)
                    (Some 33)
                    $"the melee dies at t33 (S3 t74)\n{failure}"

                // Characterisation (for S4's Flee band, #583). S3 kept
                // anchor-985849 and lost reserver-985957 at t10: the 1-block
                // ranger held (25,10), out of reach of its ground, and the
                // reserver's walk priced round it took the swamp at (25,9).
                // Since #595 that ranger closes on its ground by (26,8), the
                // reserver walks (26,9) on plain and lives (unchanged by #600,
                // which prices a loose body's tile at its step: the ranger
                // swaps onto (26,8) as the reserver steps); the anchor steps to
                // (26,8) at t6 as the melee steps beside it, and dies at t8
                // (as before S3).
                let reserver = pathOf "reserver-985957-Spawn11" trace

                Expect.equal
                    (reserver
                     |> List.tryFind (fun (tick, _) -> tick = 1)
                     |> Option.map (fun (_, s) -> s.At, s.Fatigue))
                    (Some(RoomPos.at "W17S26" { X = 26; Y = 9 }, 0))
                    $"the reserver steps onto the plain round the ranger\n{failure}"

                Expect.isTrue (survived "reserver-985957-Spawn11" trace) $"and lives\n{failure}"

                Expect.equal
                    (diedOn "anchor-985849-Spawn11" trace)
                    (Some 8)
                    $"the anchor dies at t8\n{failure}"
            }

            // Characterisation (movement S4, #583). The capture does not
            // reproduce live: the Transition log no longer held either body's
            // Task, and live ran an older bundle. Here the reserver holds its
            // Reserve, walks toward the controller, turns at (23,12) the tick
            // the melee's map reaches it and flees south-west, untouched,
            // against the melee on its live tiles and against one chasing the
            // nearest body, on main as under S4. The anchor (6 WORK 1 CARRY 3
            // MOVE: two ticks a road tile, three a plain one) walks to its
            // Post down the road the melee takes, flees ahead of it from t5
            // and is struck at t10, dead by t12: a melee at a tile a tick reaches every
            // tile before it, the exit home (20,0) included, so it has no
            // escape to be given. Moved by #625 (was dead by t12 in both): the
            // ranger now keeps Coefficient6755 (18M 17A, MOVE first, its
            // pace) off and shoots it from t1. On its live tiles its second
            // MOVE is off by t6, a step every second tick from then, and the
            // anchor dies at t14; chasing the nearest body, it chases the
            // ranger over the exit home, where W17S25 kills it by t11, and the
            // anchor lives.
            test
                "S4 capture t1,006,904: the reserver flees the melee untouched; the anchor it outpaces has none" {
                let live =
                    Waypoints
                        [
                            for x, y in
                                [
                                    15, 1
                                    16, 1
                                    17, 1
                                    18, 1
                                    19, 1
                                    20, 1
                                    21, 2
                                    22, 3
                                    23, 4
                                    24, 5
                                    25, 6
                                    26, 7
                                    27, 8
                                    26, 9
                                    25, 10
                                    24, 11
                                    23, 12
                                    22, 13
                                    22, 14
                                    21, 15
                                    20, 16
                                ] -> RoomPos.at "W17S26" { X = x; Y = y }
                        ]

                for label, melee in [ "live tiles", live; "chasing", Chase(Nearest, None) ] do
                    let _, trace = trepEntersW17S26 melee |> run 40
                    let failure = describe trace

                    Expect.isTrue
                        (pathOf "reserver-1006558-Spawn11" trace
                         |> List.forall (fun (_, s) -> s.Hits = 400))
                        $"{label}: the melee never strikes the reserver\n{failure}"

                    let died = diedOn "anchor-1005773-Spawn11" trace

                    if label = "live tiles" then
                        Expect.equal died (Some 14) $"{label}: the anchor dies at t14\n{failure}"
                    else
                        Expect.isNone died $"{label}: the anchor lives\n{failure}"

                        Expect.isLessThanOrEqual
                            (diedOn "Coefficient6755" trace
                             |> Option.defaultValue System.Int32.MaxValue)
                            11
                            $"{label}: the melee chases the ranger into W17S25 and dies there by t11\n{failure}"
            }

            // Movement S2 (`docs/adr/0088`) flipped the S0 pin here, which read
            // #527's loop live (t971,046-971,081): hauler-970335-Spawn11 fled a
            // tile back each time the slow melee stepped and rematched the same
            // Refill, standing still between; hauler-969730-Spawn11 fled once
            // and crossed home at t25. Now Virtual458's map waits out its
            // fatigue, so its Reach is its swing alone between steps, and a
            // walk to work across a border goes round what the melee strikes
            // before the walker is gone (`Threats.unsafeFor`): both haulers
            // walk north round it with the Refill held, never flee, and cross
            // home (the first standing still no more: its Work Area across the
            // border was empty, and the walk round asked it for its way).
            test
                "#527 capture t971,045: both haulers walk round the slow melee to the tower and keep its Refill" {
                let hauler = "hauler-970335-Spawn11"
                let walker = "hauler-969730-Spawn11"

                let rows, trace =
                    (slowMeleeHaulers (), [ 0..35 ])
                    ||> List.mapFold (fun a tick ->
                        let next, t = step a
                        let taskOf id = Map.tryFind id next.Carried.Assignments
                        ((tick, taskOf hauler, taskOf walker), t), next)
                    |> fst
                    |> List.unzip

                let failure = describe trace

                for id, held in
                    [
                        hauler, rows |> List.map (fun (_, task, _) -> task)
                        walker, rows |> List.map (fun (_, _, task) -> task)
                    ] do
                    Expect.equal
                        (List.distinct held)
                        [ Some "refill:tower-13-29" ]
                        $"{id} holds the tower's Refill every tick and never flees"

                    Expect.isTrue
                        (pathOf id trace
                         |> List.pairwise
                         |> List.forall (fun ((_, a), (_, b)) -> b.Hits >= a.Hits))
                        $"{id} is never struck\n{failure}"

                let home id =
                    pathOf id trace
                    |> List.tryFind (fun (_, s) -> s.At.Room = "W17S25")
                    |> Option.map fst

                // A tick later than t23 since #622: the tower's Refill is
                // priced by its twenty of room (rank 13, the surplus's 20
                // before), so the hauler pushes a weight heavier in the
                // arbitration by the exit and steps back once at t20.
                Expect.equal
                    (home hauler)
                    (Some 24)
                    $"the first crosses home round the melee\n{failure}"

                Expect.equal (home walker) (Some 20) $"and so does the other\n{failure}"
            }

            // Movement S0's doorway pin, flipped early (2026-10-07, live
            // t987,159-987,529: guard-986309 parked on the same (1,15)): the
            // tiles beside an exit are idle ground, so an idle body steps off
            // them. Live (t972,062-972,082) the guard stood idle on (1,15) and
            // the hauler behind it was carried back to W18S25 every other
            // tick. S4's stand-tile picker keeps the outcome. The hauler's
            // bounce does not reproduce, and not for want of roads: with them
            // laid (#560) and the guard back on (1,15), it steps in by (1,16)
            // at t0.
            test
                "movement S0 capture t972,061: an idle guard steps off the doorway inside the west exit" {
                let guard = "guard-970892-Spawn11"
                let doorway = RoomPos.at "W17S25" { X = 1; Y = 15 }

                let tasks, trace =
                    (doorwayJam (), [ 0..19 ])
                    ||> List.mapFold (fun a _ ->
                        let next, t = step a
                        (Map.tryFind guard next.Carried.Assignments, t), next)
                    |> fst
                    |> List.unzip

                Expect.isTrue
                    (tasks |> List.forall Option.isNone)
                    $"the guard holds no Task\n{tasks}"

                Expect.isTrue
                    (pathOf guard trace |> List.exists (fun (_, s) -> s.At <> doorway))
                    $"and leaves (1,15)\n{describe trace}"
            }

            // Characterisation (movement S0): pinned how a body
            // released inside a Reach walks out of it, which read no time:
            // its way home to W17S25 runs through (48,14), beside Idol768.
            // #563 made it the #533 acceptance before S2 did, and S2 keeps
            // it: a fighter walking to another room's fight goes round what
            // out-damages it where it stands (movement S2). Live
            // (t973,268-973,294) the ranger stood idle on (46,14) and was
            // struck to death: that was before
            // #533, which gave a released fighter a Task the same tick, so
            // the live outcome no longer reproduces by design. Here it takes
            // the home's Guard at t0 and, standing outside the Guard's room,
            // shoots Idol768 by the weapon reflex from t0 (#563): it dies at
            // t7, before the ranger passes it (struck t8-t10 before #563,
            // when the ranger struck nothing on its way and came back to
            // kill it); the towers kill Shade461 on the exit tile.
            test
                "#533 capture t973,267: a ranger released beside a melee shoots it dead on its way home, and is never struck" {
                let ranger = "ranger-972464-Spawn8"

                let tasks, trace =
                    (idleRangerInReach (), [ 0..29 ])
                    ||> List.mapFold (fun a _ ->
                        let next, t = step a
                        (Map.tryFind ranger next.Carried.Assignments, t), next)
                    |> fst
                    |> List.unzip

                let failure = describe trace

                Expect.isTrue
                    (tasks |> List.forall Option.isSome)
                    $"the ranger is never idle\n{tasks}"

                Expect.equal tasks.Head (Some "guard:W17S25") "its first Task is the home's Guard"

                let struck =
                    pathOf ranger trace
                    |> List.pairwise
                    |> List.choose (fun ((_, before), (tick, now)) ->
                        if now.Hits < before.Hits then Some tick else None)
                    |> List.filter (fun tick -> tick < 12)

                Expect.equal struck [] $"the melee never strikes it on its way out\n{failure}"

                Expect.equal
                    (diedOn "Idol768" trace)
                    (Some 7)
                    $"Idol768 dies of its shots on the way\n{failure}"

                Expect.isTrue (survived ranger trace) $"and the ranger lives\n{failure}"
            }

            // Live (t1,037,958-1,038,034) Trepidimous's haulers Synapse7330
            // and Nova7626 stood at our W17S26 container while W17S25's one
            // ranger idled across the border: Wraith7582 had killed our
            // reserver, the observer's last look found it gone and the
            // haulers there, and the next tick, dark, the room's kill left
            // the pool (task-gone) until a reserver walked in 73 ticks later.
            // Reproduced on 783fa815: the Guard at t0, gone at t1, the ranger
            // idle at (16,47) as live. A body taking from a room we mine stays
            // the room's kill until a look finds it gone (`Quarry.ghosts`).
            test
                "capture t1,037,961: the haulers the last look saw at our container stay the dark outpost's kill, and the ranger shoots one dead" {
                let ranger = "ranger-1037139-Spawn11"
                let trace, tasks = haulersAtTheContainer () |> taskEachTick ranger 150
                let died = diedOn "Synapse7330" trace

                Expect.isSome died $"Synapse7330 dies\n{describe trace}"

                Expect.isTrue
                    (tasks |> List.take died.Value |> List.forall ((=) "guard:W17S26"))
                    $"""the ranger holds W17S26's Guard until it does: {String.concat "," tasks}"""
            }

            // #618 (live, #617 part b): Trepidimous's Launch5733 stepping
            // between W18S25 and W19S25 made the trespass Task appear and
            // vanish at each crossing, and ranger-1015096 flitted guard →
            // task-gone → guard, walking without arriving. The same shape on
            // the t1,037,961 capture: Synapse7330 alone walks off our W17S26
            // container out over the west border into W18S26, which nothing
            // of ours sees, and back, twice. On main the Guard left the pool
            // each time the hauler stepped out of sight (task-gone); a body
            // the world remembers out of sight stays the room's kill
            // (`Quarry.ghosts`), so the ranger holds the Guard throughout.
            test
                "#618: a hauler dancing out over a dark border stays the outpost's kill, and the ranger keeps its Guard" {
                hunterKeepsTheDancer Set.empty
            }

            // The #663 S2 review: the same dance with W18S26 in sight (a
            // look), a room no colony of ours prices a trespass in. The
            // hauler two tiles over the border it crossed out of is still
            // W17S26's quarry (`Quarry.placed`).
            test
                "#618: a hauler dancing out over a border into a room in sight stays the outpost's kill, and the ranger keeps its Guard" {
                hunterKeepsTheDancer (Set.singleton "W18S26")
            }

            // The #663 S2 review's residual 2, owned by S5's jobs: the same
            // dance with W18S26 a second outpost of W17S25's, so each step
            // across lands the hauler in a room of the same colony, its own
            // sighting there. The kill is one job, held in the room whose
            // Guard the ranger holds: the hauler's crossings move no Task.
            test
                "#663 S5: a hauler dancing between two outposts of one colony stays one job, and the ranger keeps its Guard" {
                hunterDancesWith (Set.singleton "W18S26") (fun a ->
                    { a with
                        Colonies =
                            a.Colonies
                            |> List.map (fun c ->
                                if c.Home = "W17S25" then
                                    { c with
                                        Outposts =
                                            c.Outposts @ [ outpostOf (room "W18S26").Capture ]
                                    }
                                else
                                    c)
                    })
            }

            // The same haulers with W13S28 a living colony. The ranger walks in
            // at t1 and sees them; taken out of the room and the clock moved on
            // six ticks, the room is dark and the haulers only remembered,
            // counted nothing where an observer can look (`Facts.roomWorth`,
            // read for a cast), so the kill asks the observer for that look
            // (#660, `RoomWorth.unseenBodies`) once its sighting is `Facts.lookStride`
            // old — before, nothing asked: W17S26 wanted no eyes once its raid
            // was gone, and the observer looked at the rival room it watches.
            test
                "capture t1,037,961: a dark outpost's remembered haulers ask the observer for a look (#660)" {
                let lit, _ = haulersAtTheContainerObserved () |> step

                let dark =
                    { lit with
                        Time = lit.Time + 6
                        Bodies =
                            lit.Bodies
                            |> List.filter (fun b -> b.Side <> Side.Ours || b.At.Room <> "W17S26")
                    }

                let lookOf (a: Arena) =
                    (viewOf a "W13S28").World.Looks |> Option.bind (Map.tryFind "W13S28")

                Expect.equal
                    (lookOf dark)
                    (Some "W17S26")
                    "dark, its remembered haulers ask W13S28's observer to look"

                Expect.notEqual
                    (lookOf
                        { dark with
                            Carried =
                                { dark.Carried with
                                    Quarries = Map.empty
                                }
                        })
                    (Some "W17S26")
                    "the premise: with nothing remembered there, nothing asks"
            }

            // Live t1,037,139-345 (#585): Trepidimous's Dragon6090 and
            // Stochastic6048 (R3H4M7) stood at W17S26's west edge, the room
            // stood down off them and held the fixed 200 ticks past the first
            // sighting (#513), to t1,037,345, its anchor home, though the pair
            // were gone west by t1,037,210 and W13S28's observer reaches the
            // room. Reproduced: the row opens at t4 as the second harasser
            // steps in, the anchor and the reserver walk home (live, the same);
            // the arena's harassers keep their live tiles but walk them slower,
            // out by t112 where live was ~t70. A look finding the room without
            // them is the evidence they went: the row ends on it, and the
            // anchor goes back.
            test
                "capture t1,037,139: two harassers that walked off W17S26 end its stand-down on the observer's next look, and the anchor goes back" {
                let anchor = "anchor-1036485-Spawn11"

                let rowOf (a: Arena) =
                    Map.tryFind "W17S25" a.Carried.Raids
                    |> Option.bind (fun raids ->
                        raids.Outposts |> List.tryFind (fun row -> row.RoomName = "W17S26"))

                let standing (a: Arena) =
                    rowOf a |> Option.exists (fun row -> Fabot.Core.Observe.standingDown a.Time row)

                let harassed (a: Arena) =
                    a.Bodies |> List.exists (fun b -> b.Side <> Side.Ours && b.At.Room = "W17S26")

                let opened, toOpen = harassersAtTheEdge () |> runUntil standing 10

                Expect.isTrue
                    (standing opened)
                    $"the premise: the pair at the edge stand W17S26 down\n{describe toOpen}"

                let left, toLeave = opened |> runUntil (harassed >> not) 150

                Expect.isFalse
                    (harassed left)
                    $"the premise: the pair walk off west\n{describe toLeave}"

                Expect.isFalse
                    (left.Bodies |> List.exists (fun b -> b.Id = anchor && b.At.Room = "W17S26"))
                    "the premise: the anchor went home while the room was shut"

                let reopened, toReopen = left |> runUntil (standing >> not) 7

                Expect.isFalse
                    (standing reopened)
                    $"the row ends on the observer's next look, one in five, not 200 ticks past the first sighting\n{describe toReopen}"

                Expect.equal
                    (rowOf reopened |> Option.map _.Basis)
                    (Some StandDownBasis.RaidGone)
                    "ended as a raid gone"

                Expect.isLessThan
                    reopened.Time
                    1_037_344
                    "well inside #513's window (t1,037,144 plus 200)"

                let back, toBack =
                    reopened
                    |> runUntil
                        (fun a ->
                            a.Bodies
                            |> List.exists (fun b -> b.Id = anchor && b.At.Room = "W17S26"))
                        60

                Expect.isTrue
                    (back.Bodies |> List.exists (fun b -> b.Id = anchor && b.At.Room = "W17S26"))
                    $"and the anchor walks back to its rock\n{describe toBack}"
            }

            // #585's other half: a look finding the room without the pair is
            // no evidence they went where they only stepped across the border
            // into W18S26, a room nothing of ours sees, or stand a step or
            // two inside it (`Dance`). The same capture, the pair dancing a
            // tile out and back from t8.
            test
                "capture t1,037,139: two harassers dancing one step into an unseen room and back keep W17S26 shut" {
                let dance (back: RoomPos) =
                    Some(
                        Phases
                            [
                                8, GoTo back
                                14, GoTo(RoomPos.at "W18S26" { X = 48; Y = back.Y })
                                22, GoTo back
                                30, GoTo(RoomPos.at "W18S26" { X = 48; Y = back.Y })
                                38, GoTo back
                                46, GoTo(RoomPos.at "W18S26" { X = 47; Y = back.Y })
                                10_000, GoTo back
                            ]
                    )

                let start =
                    let a = harassersAtTheEdge ()

                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                match b.Id with
                                | "Dragon6090" ->
                                    { b with
                                        Script = dance (RoomPos.at "W17S26" { X = 1; Y = 33 })
                                    }
                                | "Stochastic6048" ->
                                    { b with
                                        Script = dance (RoomPos.at "W17S26" { X = 1; Y = 31 })
                                    }
                                | _ -> b)
                    }

                let standing (a: Arena) =
                    Map.tryFind "W17S25" a.Carried.Raids
                    |> Option.bind (fun raids ->
                        raids.Outposts |> List.tryFind (fun row -> row.RoomName = "W17S26"))
                    |> Option.exists (fun row -> Fabot.Core.Observe.standingDown a.Time row)

                let opened, toOpen = start |> runUntil standing 10

                Expect.isTrue (standing opened) $"the premise: the row opens\n{describe toOpen}"

                let rec watch n (a: Arena) shutEvery =
                    if n = 0 then
                        shutEvery
                    else
                        let next, _ = step a
                        watch (n - 1) next (shutEvery && standing next)

                Expect.isTrue
                    (watch 50 opened true)
                    "W17S26 stays shut every tick the pair dances on its border"
            }

            // #585: a raid-shut outpost's trespassers stay ours to kill, but
            // not under the raid. The same capture with a Trepidimous hauler
            // at our container: while the pair the verdict loses stands in
            // W17S26, no body of ours is sent at the hauler (no Guard of the
            // room held); the bodies there walk home. `Facts.trespassRooms`
            // admits a shut room only once no raider stands there; the kill
            // score casts nothing against a kill a raider stands beside either,
            // so this pins the outcome both keep.
            test
                "capture t1,037,139: a hauler at a shut outpost's container is no kill while the raid holds the room" {
                let start =
                    let a = harassersAtTheEdge ()

                    { a with
                        Bodies =
                            a.Bodies
                            @ [
                                { body
                                      "Hauler4242"
                                      (Side.Player "Trepidimous")
                                      (List.replicate 10 Carry @ List.replicate 10 Move)
                                      (RoomPos.at "W17S26" { X = 26; Y = 8 })
                                      (Some Hold) with
                                    TicksToLive = 1_000
                                }
                            ]
                    }

                let raided (a: Arena) =
                    a.Bodies
                    |> List.exists (fun b ->
                        b.At.Room = "W17S26" && (b.Id = "Dragon6090" || b.Id = "Stochastic6048"))

                let rec watch n (a: Arena) sent =
                    if n = 0 || not (raided a) then
                        sent
                    else
                        let next, _ = step a

                        let guards =
                            next.Carried.Assignments
                            |> Map.filter (fun _ task -> task = "guard:W17S26")
                            |> Map.keys
                            |> List.ofSeq

                        watch (n - 1) next (sent @ guards)

                Expect.isEmpty
                    (watch 60 start [] |> List.distinct)
                    "nobody holds W17S26's Guard while the raid stands there"
            }
        ]
