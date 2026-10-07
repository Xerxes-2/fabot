/// The w18s25-crippled capture's outer rooms (#573, `ArenaStandDownTests`),
/// W18S27 out to W17S24, as live t994,917 held them. Split off the fixture
/// for size alone.
module Fabot.Core.Tests.ArenaStandDownOuterRooms

open Fabot.Core.Types
open Fabot.Core.Tests.Arena

/// W18S27 to W17S24, in the capture's room order.
let internal w18s25CrippledOuterRooms () : ArenaRoom list =
    let trepidimous = Side.Player "Trepidimous"
    let giaco = Side.Player "giaco"
    let odiodin = Side.Player "Odiodin"

    let w18s27 = room "W18S27"

    let w18s28 =
        room "W18S28"
        |> withSite giaco "road" None 69 { X = 24; Y = 46 }
        |> withSite giaco "road" None 0 { X = 17; Y = 43 }
        |> withSite giaco "road" None 295 { X = 15; Y = 43 }

    let w18s29 =
        room "W18S29"
        |> withStructures
            [
                { structureOf "container" None 201_100 250_000 { X = 10; Y = 6 } with
                    Energy = 1_074
                }
                structureOf "container" None 190_700 250_000 { X = 37; Y = 37 }
            ]
        |> withRoads
            [
                2, 18, 3_050
                2, 19, 3_050
                2, 20, 3_650
                2, 22, 3_870
                2, 24, 3_600
                2, 25, 4_220
                2, 26, 3_900
                3, 15, 3_650
                3, 17, 2_920
                3, 21, 3_440
                3, 27, 4_000
                3, 28, 4_600
                4, 14, 3_670
                4, 16, 3_290
                5, 13, 3_700
                6, 12, 4_000
                7, 11, 4_150
                7, 13, 3_790
                8, 8, 4_200
                8, 10, 3_650
                8, 14, 4_250
                9, 7, 4_200
                9, 9, 3_900
                9, 15, 4_090
                10, 16, 4_000
                11, 17, 4_500
                12, 18, 4_500
                13, 19, 4_500
                13, 37, 4_900
                13, 39, 4_900
                14, 20, 4_500
                14, 36, 4_800
                14, 38, 4_900
                14, 40, 4_900
                15, 21, 4_500
                15, 35, 4_900
                15, 41, 4_900
                16, 22, 4_500
                16, 34, 4_900
                16, 42, 4_900
                17, 23, 4_500
                17, 33, 4_900
                17, 43, 4_900
                17, 47, 5_000
                18, 24, 4_900
                18, 32, 4_900
                18, 44, 5_000
                18, 46, 4_900
                18, 48, 5_000
                19, 25, 4_900
                19, 29, 4_900
                19, 31, 4_900
                19, 45, 4_900
                19, 47, 4_900
                20, 26, 4_900
                20, 28, 4_900
                20, 30, 4_600
                20, 44, 4_900
                20, 46, 4_900
                21, 27, 4_900
                21, 45, 4_900
                22, 26, 4_900
                22, 44, 5_000
                23, 25, 4_900
                23, 45, 4_900
                24, 24, 4_900
                24, 44, 4_900
                25, 17, 4_900
                25, 18, 4_900
                25, 19, 4_900
                25, 20, 4_900
                25, 21, 4_900
                25, 22, 4_900
                25, 23, 4_900
                25, 43, 4_900
                26, 14, 4_900
                26, 15, 5_000
                26, 16, 5_000
                26, 42, 4_900
                27, 13, 5_000
                27, 41, 4_600
                28, 1, 4_900
                28, 12, 5_000
                28, 40, 4_600
                29, 1, 4_900
                29, 11, 4_900
                29, 39, 4_600
                30, 1, 4_900
                30, 10, 4_900
                30, 40, 4_600
                31, 2, 5_000
                31, 9, 4_900
                31, 41, 4_600
                32, 3, 4_900
                32, 8, 4_900
                32, 40, 4_600
                33, 4, 5_000
                33, 5, 4_600
                33, 7, 4_900
                33, 41, 4_900
                34, 6, 5_000
                34, 40, 4_600
                35, 39, 4_600
                36, 38, 5_000
            ]

    let w19s28 =
        room "W19S28"
        |> withStructures
            [
                { structureOf "container" None 162_000 250_000 { X = 20; Y = 41 } with
                    Energy = 1_316
                }
                { structureOf "container" None 164_600 250_000 { X = 43; Y = 23 } with
                    Energy = 932
                }
            ]
        |> withRoads
            [
                1, 41, 4_900
                2, 40, 4_800
                3, 41, 4_800
                4, 42, 4_900
                5, 41, 4_900
                6, 42, 4_800
                7, 43, 4_800
                8, 44, 4_800
                9, 45, 4_900
                10, 44, 4_800
                11, 45, 4_800
                12, 45, 4_800
                13, 45, 4_800
                14, 45, 4_800
                15, 45, 4_900
                16, 44, 4_900
                17, 43, 4_900
                18, 42, 4_900
                19, 42, 5_000
                20, 42, 5_000
                21, 42, 5_000
                22, 41, 5_000
                23, 40, 5_000
                24, 39, 5_000
                25, 38, 5_000
                26, 37, 5_000
                27, 36, 5_000
                28, 35, 5_000
                29, 34, 5_000
                30, 33, 5_000
                31, 32, 5_000
                32, 31, 5_000
                33, 30, 4_900
                34, 29, 5_000
                35, 28, 5_000
                36, 27, 5_000
                37, 26, 23_600
                38, 25, 5_000
                39, 24, 5_000
                40, 23, 5_000
                40, 26, 5_000
                41, 24, 4_900
                41, 25, 5_000
                42, 23, 4_900
            ]

    let w19s29 =
        room "W19S29"
        |> withController Ownership.Rival (Some "giaco") 5 4
        |> withControllerClocks 0 0 80_000 588_342
        |> withStructures
            [
                for x, y in
                    [
                        36, 17
                        37, 13
                        37, 14
                        37, 15
                        37, 16
                        37, 18
                        38, 15
                        38, 17
                        38, 19
                        39, 18
                        39, 20
                        40, 15
                        40, 17
                        41, 13
                        41, 14
                        41, 15
                        41, 16
                        41, 18
                        42, 15
                        42, 17
                        42, 19
                        43, 12
                        43, 14
                        43, 16
                        43, 18
                        44, 12
                        44, 15
                        44, 17
                        45, 14
                        45, 16
                    ] ->
                    { structureOf "extension" (Some giaco) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 120_400 250_000 { X = 36; Y = 47 } with
                    Energy = 1_146
                }
                structureOf "container" None 250_000 250_000 { X = 38; Y = 14 }
                { structureOf "container" None 250_000 250_000 { X = 40; Y = 14 } with
                    Energy = 100
                }
                { structureOf "container" None 250_000 250_000 { X = 44; Y = 13 } with
                    Energy = 379
                }
                { structureOf "link" (Some giaco) 1_000 1_000 { X = 37; Y = 47 } with
                    Energy = 800
                }
                structureOf "link" (Some giaco) 1_000 1_000 { X = 43; Y = 21 }
                { structureOf "spawn" (Some giaco) 5_000 5_000 { X = 41; Y = 22 } with
                    Energy = 300
                }
                { structureOf "storage" (Some giaco) 10_000 10_000 { X = 41; Y = 21 } with
                    Energy = 76_505
                }
                towerOf giaco { X = 43; Y = 22 } 879
                towerOf giaco { X = 43; Y = 23 } 842
            ]
        |> withRoads
            [
                1, 25, 5_000
                2, 26, 4_900
                3, 25, 5_000
                4, 24, 4_900
                5, 23, 5_000
                6, 22, 4_900
                7, 21, 4_900
                8, 22, 4_900
                9, 23, 4_900
                10, 22, 4_900
                11, 21, 4_900
                12, 20, 5_000
                13, 21, 5_000
                14, 20, 4_900
                15, 21, 5_000
                16, 20, 4_900
                17, 19, 4_900
                18, 18, 4_900
                19, 19, 5_000
                20, 18, 5_000
                21, 17, 4_900
                22, 16, 5_000
                23, 15, 4_900
                24, 14, 4_900
                25, 13, 4_900
                26, 12, 5_000
                27, 11, 5_000
                28, 10, 5_000
                29, 11, 4_900
                30, 12, 5_000
                31, 13, 5_000
                32, 14, 5_000
                33, 15, 4_800
                34, 16, 4_900
                35, 17, 5_000
                35, 31, 4_900
                36, 18, 4_900
                36, 30, 5_000
                36, 32, 4_900
                36, 34, 4_900
                36, 38, 4_900
                36, 40, 4_900
                36, 42, 4_900
                36, 44, 5_000
                37, 17, 4_900
                37, 19, 4_900
                37, 20, 4_900
                37, 21, 4_900
                37, 22, 5_000
                37, 23, 4_900
                37, 24, 4_900
                37, 25, 25_000
                37, 26, 4_900
                37, 27, 4_900
                37, 29, 5_000
                37, 33, 5_000
                37, 35, 4_900
                37, 37, 4_900
                37, 39, 4_900
                37, 41, 4_900
                37, 43, 5_000
                37, 45, 4_900
                38, 16, 4_800
                38, 18, 4_800
                38, 20, 4_900
                38, 25, 4_900
                38, 27, 4_900
                38, 28, 4_900
                38, 36, 4_900
                38, 46, 4_900
                38, 48, 4_700
                39, 15, 4_900
                39, 17, 4_900
                39, 19, 4_700
                39, 47, 4_900
                40, 18, 4_800
                40, 19, 4_700
                40, 20, 4_900
                40, 48, 4_900
                41, 17, 4_600
                41, 19, 4_900
                42, 16, 4_500
                42, 18, 4_700
                42, 20, 4_900
                42, 22, 4_800
                43, 15, 3_800
                43, 17, 4_500
                43, 19, 4_900
                44, 14, 3_900
                44, 16, 3_100
            ]

    let w16s25 = room "W16S25"

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

    let w16s29 =
        room "W16S29"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 16; Y = 42 } with
                    Energy = 1_114
                }
                { structureOf "container" None 171_500 250_000 { X = 22; Y = 21 } with
                    Energy = 1_570
                }
            ]
        |> withRoads
            [
                1, 20, 4_410
                1, 37, 4_310
                2, 19, 4_430
                2, 38, 3_130
                3, 18, 4_010
                3, 39, 3_400
                4, 18, 2_650
                4, 40, 3_100
                5, 18, 4_310
                5, 41, 2_800
                6, 18, 3_310
                6, 42, 2_800
                7, 18, 3_750
                7, 41, 3_500
                8, 18, 3_050
                8, 40, 3_800
                9, 18, 3_450
                9, 39, 4_400
                10, 18, 3_750
                10, 40, 4_400
                11, 18, 4_430
                11, 40, 3_000
                12, 18, 3_130
                12, 40, 4_000
                13, 18, 4_530
                13, 41, 2_600
                14, 18, 4_530
                14, 41, 3_180
                15, 18, 2_780
                15, 41, 3_400
                16, 18, 3_680
                17, 18, 4_600
                18, 18, 4_600
                19, 18, 4_300
                20, 19, 3_700
                21, 20, 4_400
            ]

    let w15s25 =
        room "W15S25"
        |> withReactor "6a901a3bb8684d0008337ed2" { X = 44; Y = 6 } (Some odiodin) 0

    let w15s27 =
        room "W15S27"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 13; Y = 29 } with
                    Energy = 24
                    Id = "container-13-29-W15S27"
                }
            ]
        |> withRoads
            [
                1, 41, 4_900
                2, 40, 3_800
                2, 42, 3_800
                3, 39, 3_700
                3, 43, 2_600
                4, 38, 3_800
                4, 44, 4_900
                5, 37, 3_800
                5, 45, 2_500
                6, 36, 2_800
                6, 46, 3_740
                7, 35, 4_000
                7, 47, 2_800
                8, 34, 4_400
                8, 48, 2_600
                9, 33, 4_400
                10, 32, 4_000
                11, 31, 3_600
                12, 30, 4_000
            ]

    let w15s29 =
        room "W15S29"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 17; Y = 19 } with
                    Energy = 1_020
                }
            ]
        |> withRoads
            [
                4, 6, 2_480
                5, 5, 3_880
                5, 7, 4_500
                6, 4, 2_800
                6, 8, 4_800
                7, 3, 3_300
                7, 9, 2_600
                8, 2, 3_500
                8, 10, 4_800
                9, 1, 4_100
                9, 11, 4_300
                10, 12, 4_300
                11, 13, 3_200
                12, 14, 3_800
                13, 15, 3_200
                14, 16, 3_500
                15, 17, 2_500
                16, 18, 2_400
            ]

    let w14s28 =
        room "W14S28"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 6; Y = 9 } with
                    Energy = 1_150
                }
            ]
        |> withRoads
            [
                1, 21, 2_800
                2, 20, 2_700
                3, 19, 4_600
                4, 18, 3_500
                5, 17, 3_600
                6, 10, 3_700
                6, 16, 4_180
                7, 11, 3_600
                7, 15, 4_200
                8, 12, 2_700
                8, 14, 4_500
                9, 13, 2_900
            ]

    let w18s26 =
        room "W18S26"
        |> withController Ownership.Rival (Some "Trepidimous") 6 4
        |> withControllerClocks 0 0 120_000 1_975_077
        |> withStructures
            [
                for x, y in
                    [
                        15, 11
                        15, 13
                        16, 12
                        17, 11
                        17, 17
                        18, 16
                        18, 18
                        19, 17
                        20, 18
                        21, 17
                        22, 18
                        23, 9
                        23, 11
                        23, 13
                        23, 15
                        23, 17
                        24, 9
                        24, 10
                        24, 12
                        24, 14
                        24, 16
                        24, 18
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        16, 10
                        16, 14
                        16, 16
                        16, 18
                        17, 9
                        17, 13
                        17, 15
                        18, 10
                        19, 9
                        19, 13
                        19, 15
                        20, 10
                        21, 9
                        22, 10
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                        Id = $"extension-{x}-{y}-W18S26"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 18, 12; 19, 11; 21, 11 ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W18S26"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 25, 21; 26, 20; 27, 21 ] ->
                    { structureOf "lab" (Some trepidimous) 500 500 { X = x; Y = y } with
                        Energy = 2_000
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 250_000 250_000 { X = 13; Y = 19 } with
                    Energy = 2_000
                }
                structureOf "container" None 250_000 250_000 { X = 15; Y = 42 }
                { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = 18; Y = 14 } with
                    Energy = 10
                    Id = "extension-18-14-W18S26"
                }
                structureOf "extractor" (Some trepidimous) 500 500 { X = 14; Y = 41 }
                structureOf "link" (Some trepidimous) 1_000 1_000 { X = 20; Y = 16 }
                { structureOf "link" (Some trepidimous) 1_000 1_000 { X = 25; Y = 41 } with
                    Energy = 180
                }
                { structureOf "link" (Some trepidimous) 1_000 1_000 { X = 30; Y = 4 } with
                    Energy = 90
                }
                { structureOf "rampart" (Some trepidimous) 953_601 30_000_000 { X = 1; Y = 10 } with
                    NextDecay = 994_939
                }
                { structureOf "rampart" (Some trepidimous) 962_401 30_000_000 { X = 2; Y = 4 } with
                    NextDecay = 994_949
                }
                { structureOf "rampart" (Some trepidimous) 964_001 30_000_000 { X = 2; Y = 5 } with
                    NextDecay = 994_940
                }
                { structureOf "rampart" (Some trepidimous) 971_801 30_000_000 { X = 2; Y = 6 } with
                    NextDecay = 994_967
                }
                { structureOf "rampart" (Some trepidimous) 966_941 30_000_000 { X = 2; Y = 7 } with
                    NextDecay = 994_998
                }
                { structureOf "rampart" (Some trepidimous) 971_331 30_000_000 { X = 2; Y = 8 } with
                    NextDecay = 994_930
                }
                { structureOf "rampart" (Some trepidimous) 971_801 30_000_000 { X = 2; Y = 9 } with
                    NextDecay = 995_017
                }
                { structureOf "rampart" (Some trepidimous) 980_721 30_000_000 { X = 2; Y = 10 } with
                    NextDecay = 995_016
                }
                { structureOf "rampart" (Some trepidimous) 987_101 30_000_000 { X = 10; Y = 22 } with
                    NextDecay = 994_998
                }
                { structureOf "rampart" (Some trepidimous) 973_201 30_000_000 { X = 11; Y = 22 } with
                    NextDecay = 994_938
                }
                { structureOf "rampart" (Some trepidimous) 950_801 30_000_000 { X = 12; Y = 21 } with
                    NextDecay = 994_969
                }
                { structureOf "rampart" (Some trepidimous) 969_201 30_000_000 { X = 12; Y = 22 } with
                    NextDecay = 994_935
                }
                { structureOf "rampart" (Some trepidimous) 967_901 30_000_000 { X = 15; Y = 47 } with
                    NextDecay = 995_005
                }
                { structureOf "rampart" (Some trepidimous) 969_501 30_000_000 { X = 15; Y = 48 } with
                    NextDecay = 994_974
                }
                { structureOf "rampart" (Some trepidimous) 951_801 30_000_000 { X = 16; Y = 47 } with
                    NextDecay = 994_975
                }
                { structureOf "rampart" (Some trepidimous) 981_501 30_000_000 { X = 17; Y = 47 } with
                    NextDecay = 994_923
                }
                { structureOf "rampart" (Some trepidimous) 943_001 30_000_000 { X = 18; Y = 47 } with
                    NextDecay = 995_005
                }
                { structureOf "rampart" (Some trepidimous) 958_101 30_000_000 { X = 19; Y = 47 } with
                    NextDecay = 994_978
                }
                { structureOf "rampart" (Some trepidimous) 945_081 30_000_000 { X = 20; Y = 47 } with
                    NextDecay = 994_921
                }
                { structureOf "rampart" (Some trepidimous) 953_321 30_000_000 { X = 21; Y = 47 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some trepidimous) 966_971 30_000_000 { X = 22; Y = 47 } with
                    NextDecay = 994_960
                }
                { structureOf "rampart" (Some trepidimous) 967_881 30_000_000 { X = 23; Y = 47 } with
                    NextDecay = 994_997
                }
                { structureOf "rampart" (Some trepidimous) 974_711 30_000_000 { X = 24; Y = 47 } with
                    NextDecay = 994_931
                }
                { structureOf "rampart" (Some trepidimous) 980_931 30_000_000 { X = 25; Y = 47 } with
                    NextDecay = 994_925
                }
                { structureOf "rampart" (Some trepidimous) 984_481 30_000_000 { X = 26; Y = 47 } with
                    NextDecay = 994_956
                }
                { structureOf "rampart" (Some trepidimous) 965_671 30_000_000 { X = 27; Y = 47 } with
                    NextDecay = 994_948
                }
                { structureOf "rampart" (Some trepidimous) 957_431 30_000_000 { X = 28; Y = 47 } with
                    NextDecay = 994_952
                }
                { structureOf "rampart" (Some trepidimous) 944_591 30_000_000 { X = 29; Y = 47 } with
                    NextDecay = 994_942
                }
                { structureOf "rampart" (Some trepidimous) 952_201 30_000_000 { X = 30; Y = 3 } with
                    NextDecay = 994_956
                }
                { structureOf "rampart" (Some trepidimous) 949_301 30_000_000 { X = 30; Y = 4 } with
                    NextDecay = 995_011
                }
                { structureOf "rampart" (Some trepidimous) 963_301 30_000_000 { X = 30; Y = 5 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some trepidimous) 958_501 30_000_000 { X = 30; Y = 6 } with
                    NextDecay = 994_941
                }
                { structureOf "rampart" (Some trepidimous) 947_301 30_000_000 { X = 30; Y = 7 } with
                    NextDecay = 994_977
                }
                { structureOf "rampart" (Some trepidimous) 962_201 30_000_000 { X = 30; Y = 8 } with
                    NextDecay = 995_002
                }
                { structureOf "rampart" (Some trepidimous) 971_301 30_000_000 { X = 30; Y = 9 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some trepidimous) 972_201 30_000_000 { X = 30; Y = 10 } with
                    NextDecay = 995_001
                }
                { structureOf "rampart" (Some trepidimous) 960_801 30_000_000 { X = 30; Y = 11 } with
                    NextDecay = 994_936
                }
                { structureOf "rampart" (Some trepidimous) 957_101 30_000_000 { X = 30; Y = 12 } with
                    NextDecay = 994_980
                }
                { structureOf "rampart" (Some trepidimous) 962_801 30_000_000 { X = 30; Y = 13 } with
                    NextDecay = 994_932
                }
                { structureOf "rampart" (Some trepidimous) 944_701 30_000_000 { X = 30; Y = 14 } with
                    NextDecay = 994_979
                }
                { structureOf "rampart" (Some trepidimous) 969_701 30_000_000 { X = 30; Y = 15 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some trepidimous) 946_101 30_000_000 { X = 30; Y = 16 } with
                    NextDecay = 994_961
                }
                { structureOf "rampart" (Some trepidimous) 967_201 30_000_000 { X = 30; Y = 17 } with
                    NextDecay = 994_991
                }
                { structureOf "rampart" (Some trepidimous) 973_401 30_000_000 { X = 30; Y = 18 } with
                    NextDecay = 994_984
                }
                { structureOf "rampart" (Some trepidimous) 972_101 30_000_000 { X = 30; Y = 19 } with
                    NextDecay = 994_983
                }
                { structureOf "rampart" (Some trepidimous) 951_301 30_000_000 { X = 30; Y = 20 } with
                    NextDecay = 994_962
                }
                { structureOf "rampart" (Some trepidimous) 970_401 30_000_000 { X = 30; Y = 21 } with
                    NextDecay = 994_988
                }
                { structureOf "rampart" (Some trepidimous) 976_201 30_000_000 { X = 30; Y = 22 } with
                    NextDecay = 994_955
                }
                { structureOf "rampart" (Some trepidimous) 946_101 30_000_000 { X = 30; Y = 23 } with
                    NextDecay = 995_013
                }
                { structureOf "rampart" (Some trepidimous) 959_701 30_000_000 { X = 30; Y = 24 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some trepidimous) 977_501 30_000_000 { X = 30; Y = 25 } with
                    NextDecay = 995_004
                }
                { structureOf "rampart" (Some trepidimous) 943_801 30_000_000 { X = 30; Y = 26 } with
                    NextDecay = 995_005
                }
                { structureOf "rampart" (Some trepidimous) 950_081 30_000_000 { X = 30; Y = 27 } with
                    NextDecay = 994_972
                }
                { structureOf "rampart" (Some trepidimous) 973_381 30_000_000 { X = 30; Y = 28 } with
                    NextDecay = 994_972
                }
                { structureOf "rampart" (Some trepidimous) 948_281 30_000_000 { X = 30; Y = 29 } with
                    NextDecay = 994_980
                }
                { structureOf "rampart" (Some trepidimous) 966_281 30_000_000 { X = 30; Y = 30 } with
                    NextDecay = 994_941
                }
                { structureOf "rampart" (Some trepidimous) 956_901 30_000_000 { X = 30; Y = 31 } with
                    NextDecay = 994_984
                }
                { structureOf "rampart" (Some trepidimous) 962_001 30_000_000 { X = 30; Y = 32 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some trepidimous) 984_831 30_000_000 { X = 30; Y = 33 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some trepidimous) 951_081 30_000_000 { X = 30; Y = 34 } with
                    NextDecay = 994_981
                }
                { structureOf "rampart" (Some trepidimous) 948_901 30_000_000 { X = 30; Y = 35 } with
                    NextDecay = 995_001
                }
                { structureOf "rampart" (Some trepidimous) 979_181 30_000_000 { X = 30; Y = 36 } with
                    NextDecay = 995_002
                }
                { structureOf "rampart" (Some trepidimous) 959_041 30_000_000 { X = 30; Y = 37 } with
                    NextDecay = 994_963
                }
                { structureOf "rampart" (Some trepidimous) 980_401 30_000_000 { X = 30; Y = 38 } with
                    NextDecay = 994_969
                }
                { structureOf "rampart" (Some trepidimous) 959_471 30_000_000 { X = 30; Y = 39 } with
                    NextDecay = 994_927
                }
                { structureOf "rampart" (Some trepidimous) 984_401 30_000_000 { X = 30; Y = 40 } with
                    NextDecay = 994_961
                }
                { structureOf "rampart" (Some trepidimous) 977_501 30_000_000 { X = 30; Y = 41 } with
                    NextDecay = 995_009
                }
                { structureOf "rampart" (Some trepidimous) 971_661 30_000_000 { X = 30; Y = 42 } with
                    NextDecay = 994_974
                }
                { structureOf "rampart" (Some trepidimous) 985_601 30_000_000 { X = 30; Y = 43 } with
                    NextDecay = 994_979
                }
                { structureOf "rampart" (Some trepidimous) 964_101 30_000_000 { X = 30; Y = 44 } with
                    NextDecay = 994_930
                }
                { structureOf "rampart" (Some trepidimous) 973_301 30_000_000 { X = 30; Y = 45 } with
                    NextDecay = 994_990
                }
                { structureOf "rampart" (Some trepidimous) 963_201 30_000_000 { X = 30; Y = 46 } with
                    NextDecay = 994_927
                }
                { structureOf "rampart" (Some trepidimous) 947_601 30_000_000 { X = 30; Y = 47 } with
                    NextDecay = 994_993
                }
                { structureOf "spawn" (Some trepidimous) 5_000 5_000 { X = 20; Y = 12 } with
                    Energy = 150
                }
                { structureOf "storage" (Some trepidimous) 10_000 10_000 { X = 20; Y = 14 } with
                    Energy = 37_355
                }
                { structureOf "terminal" (Some trepidimous) 3_000 3_000 { X = 22; Y = 16 } with
                    Energy = 40_284
                }
                towerOf trepidimous { X = 18; Y = 8 } 991
                towerOf trepidimous { X = 22; Y = 8 } 1_000
            ]
        |> withRoads
            [
                1, 8, 4_800
                2, 8, 4_700
                3, 8, 4_700
                4, 8, 4_400
                5, 8, 4_300
                6, 8, 4_300
                7, 8, 4_000
                8, 9, 4_300
                9, 9, 5_000
                10, 9, 4_300
                11, 9, 4_700
                12, 9, 4_000
                13, 9, 4_100
                13, 23, 4_500
                14, 9, 4_200
                14, 22, 4_500
                15, 10, 4_300
                15, 22, 22_500
                16, 11, 5_000
                16, 21, 4_000
                17, 12, 3_900
                17, 18, 4_900
                17, 20, 4_600
                18, 13, 4_800
                18, 15, 4_200
                18, 17, 4_400
                18, 19, 5_000
                18, 24, 4_500
                18, 25, 4_700
                18, 26, 4_300
                18, 27, 4_100
                18, 28, 4_400
                18, 29, 4_700
                18, 30, 4_700
                18, 31, 4_800
                18, 32, 4_000
                18, 33, 4_200
                18, 39, 4_200
                18, 40, 4_800
                18, 41, 4_200
                18, 42, 4_100
                18, 43, 4_000
                18, 44, 4_400
                18, 48, 4_900
                19, 12, 4_900
                19, 14, 4_200
                19, 16, 4_800
                19, 18, 4_500
                19, 20, 4_100
                19, 21, 4_800
                19, 22, 4_100
                19, 23, 4_000
                19, 34, 4_200
                19, 35, 5_000
                19, 36, 4_100
                19, 37, 4_400
                19, 38, 4_600
                19, 45, 4_900
                19, 47, 4_900
                20, 11, 4_700
                20, 13, 4_000
                20, 15, 4_400
                20, 17, 4_600
                20, 19, 4_700
                20, 21, 4_500
                20, 46, 4_700
                21, 10, 4_000
                21, 12, 5_000
                21, 13, 4_400
                21, 14, 4_700
                21, 16, 4_900
                21, 22, 4_000
                21, 47, 4_900
                22, 9, 4_200
                22, 11, 4_800
                22, 15, 5_000
                22, 23, 4_800
                22, 48, 4_400
                23, 8, 4_700
                23, 16, 4_400
                23, 20, 5_000
                23, 21, 4_900
                23, 24, 4_300
                24, 7, 4_100
                24, 17, 5_000
                24, 19, 4_300
                24, 22, 4_900
                24, 25, 4_600
                25, 7, 4_100
                25, 18, 4_200
                25, 20, 23_500
                25, 23, 4_800
                25, 26, 4_400
                25, 35, 4_400
                25, 41, 4_200
                25, 42, 4_300
                26, 7, 4_800
                26, 17, 4_800
                26, 18, 4_200
                26, 21, 4_800
                26, 23, 4_600
                26, 27, 4_700
                26, 28, 4_100
                26, 29, 4_500
                26, 30, 5_000
                26, 31, 4_500
                26, 32, 4_900
                26, 33, 4_800
                26, 34, 4_900
                26, 35, 4_600
                26, 36, 4_100
                26, 37, 4_700
                26, 38, 4_100
                26, 39, 4_700
                26, 40, 4_800
                26, 41, 4_200
                27, 6, 4_300
                27, 16, 5_000
                27, 19, 4_400
                27, 22, 4_500
                28, 5, 4_500
                28, 15, 4_100
                28, 20, 4_800
                28, 21, 5_000
                29, 4, 4_700
                29, 6, 4_700
                29, 14, 4_200
                29, 21, 4_100
                30, 3, 4_800
                30, 5, 4_000
                30, 7, 4_900
                30, 13, 4_300
                30, 22, 4_500
                31, 8, 4_900
                31, 12, 4_300
                31, 23, 3_900
                32, 9, 4_000
                32, 11, 4_400
                32, 24, 4_300
                33, 10, 4_700
                33, 25, 4_700
                34, 11, 4_900
                34, 26, 5_000
                35, 11, 4_600
                35, 27, 4_300
                36, 11, 4_300
                36, 28, 4_700
                37, 10, 4_600
                37, 29, 5_000
                38, 9, 4_000
                38, 30, 4_200
                39, 8, 4_900
                39, 31, 5_000
                40, 7, 4_700
                40, 31, 5_000
                41, 6, 4_300
                41, 7, 4_500
                41, 31, 5_000
                42, 6, 4_700
                42, 7, 5_000
                42, 31, 4_000
                43, 6, 5_000
                43, 7, 4_200
                43, 31, 4_400
                44, 6, 4_000
                44, 7, 4_800
                44, 31, 4_400
                45, 6, 4_100
                45, 7, 4_100
                45, 31, 4_900
                46, 6, 4_700
                46, 7, 4_100
                46, 31, 5_000
                47, 6, 5_000
                47, 7, 5_000
                47, 31, 3_900
                48, 6, 4_300
                48, 7, 4_100
                48, 31, 4_500
            ]

    let w17s24 =
        room "W17S24"
        |> withController Ownership.Rival (Some "Trepidimous") 5 4
        |> withControllerClocks 0 0 80_000 7_950
        |> withStructures
            [
                for x, y in
                    [
                        19, 31
                        19, 33
                        20, 32
                        21, 31
                        21, 33
                        21, 35
                        21, 37
                        22, 36
                        22, 38
                        24, 38
                        25, 39
                        26, 36
                        26, 38
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                        Id = $"extension-{x}-{y}-W17S24"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 19, 35; 19, 39; 20, 36; 20, 38 ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W17S24"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        20, 34
                        22, 32
                        22, 34
                        23, 31
                        23, 39
                        24, 32
                        25, 31
                        26, 32
                        26, 34
                        27, 31
                        27, 33
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 250_000 250_000 { X = 17; Y = 19 } with
                    Energy = 110
                    Id = "container-17-19-W17S24"
                }
                structureOf "extension" (Some trepidimous) 1_000 1_000 { X = 19; Y = 37 }
                structureOf "extension" (Some trepidimous) 1_000 1_000 { X = 21; Y = 39 }
                { structureOf "link" (Some trepidimous) 1_000 1_000 { X = 11; Y = 12 } with
                    Energy = 187
                }
                structureOf "link" (Some trepidimous) 1_000 1_000 { X = 23; Y = 37 }
                { structureOf "rampart" (Some trepidimous) 685_141 10_000_000 { X = 2; Y = 18 } with
                    Id = "rampart-2-18-W17S24"
                    NextDecay = 995_009
                }
                { structureOf "rampart" (Some trepidimous) 691_631 10_000_000 { X = 2; Y = 19 } with
                    Id = "rampart-2-19-W17S24"
                    NextDecay = 995_014
                }
                { structureOf "rampart" (Some trepidimous) 696_561 10_000_000 { X = 2; Y = 20 } with
                    Id = "rampart-2-20-W17S24"
                    NextDecay = 994_992
                }
                { structureOf "rampart" (Some trepidimous) 684_881 10_000_000 { X = 2; Y = 21 } with
                    NextDecay = 995_017
                }
                { structureOf "rampart" (Some trepidimous) 697_011 10_000_000 { X = 6; Y = 2 } with
                    NextDecay = 994_977
                }
                { structureOf "rampart" (Some trepidimous) 685_101 10_000_000 { X = 6; Y = 3 } with
                    NextDecay = 995_002
                }
                { structureOf "rampart" (Some trepidimous) 683_901 10_000_000 { X = 9; Y = 9 } with
                    Id = "rampart-9-9-W17S24"
                    NextDecay = 995_009
                }
                { structureOf "rampart" (Some trepidimous) 695_501 10_000_000 { X = 10; Y = 8 } with
                    Id = "rampart-10-8-W17S24"
                    NextDecay = 994_983
                }
                { structureOf "rampart" (Some trepidimous) 687_401 10_000_000 { X = 10; Y = 9 } with
                    NextDecay = 994_949
                }
                { structureOf "rampart" (Some trepidimous) 696_301 10_000_000 { X = 24; Y = 3 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some trepidimous) 697_301 10_000_000 { X = 24; Y = 4 } with
                    NextDecay = 994_975
                }
                { structureOf "rampart" (Some trepidimous) 688_701 10_000_000 { X = 24; Y = 5 } with
                    NextDecay = 994_942
                }
                { structureOf "rampart" (Some trepidimous) 693_501 10_000_000 { X = 24; Y = 6 } with
                    NextDecay = 994_954
                }
                { structureOf "rampart" (Some trepidimous) 685_201 10_000_000 { X = 30; Y = 10 } with
                    Id = "rampart-30-10-W17S24"
                    NextDecay = 994_921
                }
                { structureOf "rampart" (Some trepidimous) 683_851 10_000_000 { X = 31; Y = 10 } with
                    NextDecay = 995_015
                }
                { structureOf "rampart" (Some trepidimous) 691_271 10_000_000 { X = 32; Y = 10 } with
                    NextDecay = 995_012
                }
                { structureOf "rampart" (Some trepidimous) 691_201 10_000_000 { X = 33; Y = 10 } with
                    NextDecay = 994_922
                }
                { structureOf "rampart" (Some trepidimous) 688_801 10_000_000 { X = 34; Y = 10 } with
                    NextDecay = 994_973
                }
                { structureOf "rampart" (Some trepidimous) 688_851 10_000_000 { X = 43; Y = 33 } with
                    NextDecay = 994_961
                }
                { structureOf "rampart" (Some trepidimous) 684_141 10_000_000 { X = 44; Y = 33 } with
                    NextDecay = 994_979
                }
                { structureOf "rampart" (Some trepidimous) 682_501 10_000_000 { X = 45; Y = 33 } with
                    NextDecay = 994_993
                }
                { structureOf "rampart" (Some trepidimous) 689_801 10_000_000 { X = 46; Y = 33 } with
                    NextDecay = 995_017
                }
                { structureOf "spawn" (Some trepidimous) 5_000 5_000 { X = 23; Y = 33 } with
                    Energy = 300
                }
                { structureOf "storage" (Some trepidimous) 10_000 10_000 { X = 23; Y = 35 } with
                    Energy = 40_437
                }
                towerOf trepidimous { X = 17; Y = 33 } 1_000
                towerOf trepidimous { X = 17; Y = 37 } 1_000
            ]
        |> withRoads
            [
                8, 10, 4_700
                8, 11, 4_700
                8, 12, 4_800
                8, 13, 4_200
                8, 14, 4_500
                8, 15, 4_500
                8, 16, 4_000
                8, 17, 5_000
                8, 18, 4_100
                8, 19, 4_800
                8, 20, 4_800
                8, 21, 4_700
                9, 14, 4_500
                9, 22, 4_400
                9, 30, 4_700
                9, 31, 4_800
                10, 15, 4_500
                10, 23, 4_300
                10, 24, 4_000
                10, 25, 4_900
                10, 26, 4_300
                10, 27, 4_000
                10, 28, 4_300
                10, 29, 4_400
                10, 32, 5_000
                11, 12, 4_800
                11, 16, 4_200
                11, 24, 4_800
                11, 25, 4_200
                11, 26, 4_200
                11, 27, 4_900
                11, 28, 4_900
                11, 32, 4_900
                12, 12, 4_200
                12, 13, 4_100
                12, 14, 4_300
                12, 15, 4_400
                12, 16, 5_000
                12, 17, 4_300
                12, 18, 5_000
                12, 19, 4_600
                12, 20, 4_500
                12, 21, 4_200
                12, 22, 4_200
                12, 23, 4_700
                12, 32, 4_100
                13, 20, 4_500
                13, 22, 4_400
                13, 33, 4_400
                14, 21, 4_800
                14, 34, 4_900
                15, 19, 4_700
                15, 20, 4_200
                15, 34, 4_700
                16, 18, 5_000
                16, 19, 4_800
                16, 34, 4_200
                17, 34, 4_000
                18, 33, 4_600
                19, 32, 4_000
                20, 33, 4_500
                21, 32, 4_800
                21, 34, 4_300
                22, 21, 4_500
                22, 22, 21_500
                22, 23, 20_000
                22, 31, 4_900
                22, 33, 5_000
                22, 35, 4_200
                23, 20, 4_900
                23, 24, 20_500
                23, 30, 5_000
                23, 34, 4_300
                24, 16, 4_400
                24, 17, 4_100
                24, 18, 5_000
                24, 19, 4_900
                24, 25, 4_200
                24, 26, 4_000
                24, 27, 4_100
                24, 28, 4_400
                24, 29, 4_700
                25, 15, 4_500
                26, 14, 4_700
                27, 13, 4_800
                28, 12, 4_700
                29, 11, 4_000
                30, 11, 4_700
                31, 11, 4_500
                32, 10, 23_500
                33, 1, 4_200
                33, 9, 22_400
                34, 2, 4_100
                34, 8, 4_100
                35, 2, 4_500
                35, 3, 4_000
                35, 4, 4_100
                35, 5, 4_800
                35, 6, 5_000
                35, 7, 4_900
                36, 1, 4_900
            ]

    [
        w18s27
        w18s28
        w18s29
        w19s28
        w19s29
        w16s25
        w16s27
        w16s29
        w15s25
        w15s27
        w15s29
        w14s28
        w18s26
        w17s24
    ]
