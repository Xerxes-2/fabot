/// The w18s25-crippled capture's rooms (#573, `ArenaStandDownTests`), W18S25
/// out to W12S26, as live t994,917 held them; the outer ring is
/// `ArenaStandDownOuterRooms`. Split off the fixture for size alone.
module Fabot.Core.Tests.ArenaStandDownRooms

open Fabot.Core.Types
open Fabot.Core.Tests.Arena

/// W18S25 to W12S26, in the capture's room order.
let internal w18s25CrippledRooms () : ArenaRoom list =
    let trepidimous = Side.Player "Trepidimous"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 31; Y = 5 } with
                    Energy = 1_274
                }
            ]
        |> withRoads
            [
                28, 9, 3_300
                29, 8, 2_400
                29, 10, 3_700
                30, 7, 2_500
                30, 11, 4_100
                31, 6, 3_300
                31, 12, 4_200
                32, 13, 4_300
                33, 14, 4_400
                34, 15, 3_000
                35, 16, 2_600
                36, 17, 2_700
                37, 16, 4_400
                38, 15, 2_500
                39, 14, 2_500
                40, 13, 4_000
                41, 12, 3_600
                42, 13, 4_600
                43, 13, 3_200
                44, 13, 3_700
                45, 13, 4_300
                46, 14, 4_200
                47, 15, 3_400
                48, 16, 3_700
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 316_662
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
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                structureOf "container" None 127_000 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_400 250_000 { X = 17; Y = 20 } with
                    Energy = 324
                }
                { structureOf "rampart" (Some Side.Ours) 99_421 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 995_004
                }
                { structureOf "rampart" (Some Side.Ours) 98_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 994_985
                }
                { structureOf "rampart" (Some Side.Ours) 95_511 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 994_988
                }
                { structureOf "rampart" (Some Side.Ours) 98_731 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 994_993
                }
                { structureOf "rampart" (Some Side.Ours) 99_511 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 994_965
                }
                { structureOf "rampart" (Some Side.Ours) 99_431 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 994_967
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 994_992
                }
                { structureOf "rampart" (Some Side.Ours) 98_601 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 994_954
                }
                { structureOf "rampart" (Some Side.Ours) 96_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some Side.Ours) 97_761 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 994_986
                }
                { structureOf "rampart" (Some Side.Ours) 90_461 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 994_935
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 994_985
                }
                { structureOf "rampart" (Some Side.Ours) 97_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 994_970
                }
                { structureOf "rampart" (Some Side.Ours) 98_301 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 994_946
                }
                { structureOf "rampart" (Some Side.Ours) 100_221 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some Side.Ours) 98_101 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 994_991
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 994_936
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 994_932
                }
                { structureOf "rampart" (Some Side.Ours) 96_101 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 994_945
                }
                { structureOf "rampart" (Some Side.Ours) 96_971 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 994_963
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_575
                }
                towerOf Side.Ours { X = 13; Y = 29 } 980
            ]
        |> withRoads
            [
                1, 16, 4_300
                2, 17, 3_800
                3, 18, 4_300
                4, 19, 3_100
                5, 20, 3_000
                6, 21, 3_700
                7, 22, 15_500
                8, 23, 13_800
                9, 24, 12_900
                10, 25, 17_200
                10, 27, 14_900
                10, 29, 12_000
                11, 24, 18_200
                11, 26, 12_500
                11, 28, 19_300
                11, 30, 2_400
                12, 23, 12_900
                12, 25, 15_800
                12, 27, 12_400
                12, 29, 4_300
                13, 24, 3_900
                13, 26, 4_100
                13, 28, 2_700
                13, 30, 4_500
                14, 23, 3_900
                14, 25, 3_600
                14, 27, 3_900
                14, 29, 4_400
                14, 31, 4_300
                15, 22, 2_500
                15, 28, 2_800
                15, 30, 3_600
                15, 32, 4_200
                16, 21, 2_400
                16, 29, 3_600
                16, 31, 3_800
                16, 33, 3_700
                16, 35, 4_300
                16, 37, 3_600
                16, 38, 19_100
                16, 39, 20_100
                16, 41, 3_800
                16, 43, 3_600
                16, 44, 3_600
                17, 20, 2_500
                17, 30, 3_700
                17, 32, 4_400
                17, 34, 3_800
                17, 36, 2_800
                17, 38, 4_200
                17, 39, 20_000
                17, 40, 14_600
                17, 42, 4_000
                17, 45, 2_500
                18, 29, 4_400
                18, 31, 4_800
                18, 35, 3_300
                18, 37, 4_300
                18, 39, 2_500
                18, 41, 3_100
                18, 46, 3_000
                19, 30, 4_200
                19, 34, 2_400
                19, 36, 3_700
                19, 38, 2_600
                19, 40, 2_500
                19, 47, 2_600
                20, 31, 3_300
                20, 33, 2_700
                20, 35, 2_500
                20, 37, 4_300
                20, 39, 2_600
                20, 48, 3_500
                21, 32, 4_300
                21, 34, 3_800
                21, 36, 2_600
                21, 38, 3_800
                22, 33, 4_400
                22, 35, 2_600
                22, 37, 4_500
                23, 34, 4_400
                23, 36, 2_500
                24, 35, 2_600
                25, 34, 3_400
                26, 33, 3_800
                27, 32, 3_500
                28, 31, 15_000
                28, 33, 17_600
                29, 32, 15_000
                29, 34, 20_100
                30, 31, 15_500
                30, 33, 19_900
                31, 32, 19_400
                32, 31, 16_000
                33, 30, 15_840
                33, 32, 13_810
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_126_063
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
                { structureOf "container" None 125_700 250_000 { X = 6; Y = 29 } with
                    Energy = 648
                }
                { structureOf "container" None 196_100 250_000 { X = 11; Y = 18 } with
                    Energy = 612
                }
                { structureOf "container" None 147_500 250_000 { X = 23; Y = 29 } with
                    Energy = 358
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 98_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 995_015
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 994_975
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 994_936
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 994_930
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 994_951
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 994_999
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 994_992
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 30_074
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_400
                5, 4, 3_200
                5, 6, 4_400
                5, 44, 3_500
                6, 3, 4_900
                6, 7, 3_700
                6, 29, 3_000
                6, 43, 3_100
                6, 45, 4_000
                7, 2, 3_900
                7, 8, 4_400
                7, 28, 4_000
                7, 42, 2_900
                7, 46, 3_400
                8, 1, 4_900
                8, 9, 2_600
                8, 28, 2_700
                8, 41, 3_100
                8, 47, 2_600
                9, 10, 3_700
                9, 27, 3_500
                9, 28, 4_400
                9, 40, 2_900
                9, 48, 3_400
                10, 11, 2_800
                10, 26, 4_000
                10, 29, 3_900
                10, 39, 3_200
                11, 12, 4_900
                11, 18, 2_600
                11, 27, 2_600
                11, 30, 4_100
                11, 38, 3_700
                12, 13, 3_300
                12, 17, 3_700
                12, 25, 17_600
                12, 27, 18_500
                12, 28, 4_400
                12, 29, 3_900
                12, 31, 2_800
                12, 33, 14_700
                12, 35, 18_000
                12, 37, 3_100
                13, 14, 3_400
                13, 16, 3_000
                13, 26, 14_300
                13, 28, 2_700
                13, 30, 4_200
                13, 32, 4_800
                13, 34, 3_100
                13, 36, 3_300
                14, 15, 3_800
                14, 25, 17_700
                14, 27, 4_800
                14, 29, 3_300
                14, 31, 3_500
                14, 33, 4_200
                14, 35, 3_600
                15, 15, 3_200
                15, 26, 4_400
                15, 28, 2_900
                15, 30, 4_900
                15, 32, 3_300
                15, 34, 3_200
                16, 16, 3_200
                16, 25, 20_600
                16, 27, 4_200
                16, 29, 4_900
                16, 31, 3_300
                16, 33, 2_700
                16, 35, 2_800
                17, 17, 4_200
                17, 26, 18_400
                17, 28, 4_300
                17, 30, 4_200
                17, 32, 3_900
                17, 34, 3_400
                18, 18, 2_700
                18, 25, 18_700
                18, 27, 18_000
                18, 29, 3_300
                18, 31, 3_000
                18, 33, 3_500
                19, 19, 13_200
                19, 22, 18_100
                19, 24, 14_000
                19, 26, 19_500
                19, 28, 4_600
                19, 30, 3_200
                19, 32, 4_100
                20, 20, 13_500
                20, 21, 19_100
                20, 23, 19_300
                20, 25, 18_900
                20, 27, 14_200
                20, 29, 3_400
                20, 31, 2_600
                20, 33, 20_200
                21, 21, 4_000
                21, 22, 4_700
                21, 23, 4_900
                21, 24, 19_600
                21, 25, 15_600
                21, 26, 20_900
                21, 28, 16_500
                21, 30, 3_800
                21, 32, 15_000
                21, 34, 21_300
                22, 24, 4_200
                22, 25, 15_800
                22, 27, 18_400
                22, 28, 3_800
                22, 29, 3_200
                22, 30, 21_100
                22, 31, 16_300
                22, 32, 18_100
                22, 33, 15_700
                22, 35, 3_800
                23, 26, 17_800
                23, 28, 14_500
                23, 29, 16_700
                23, 30, 17_400
                23, 31, 19_600
                23, 32, 17_400
                23, 33, 21_300
                23, 34, 4_500
                24, 25, 14_600
                24, 28, 18_000
                24, 29, 19_100
                24, 30, 16_900
                24, 33, 13_000
                25, 24, 4_100
                25, 28, 18_700
                25, 29, 18_800
                25, 34, 19_900
                26, 23, 18_000
                27, 22, 15_200
                28, 21, 14_900
                28, 34, 9_100
                29, 20, 16_700
                30, 19, 14_600
                31, 18, 3_600
                32, 17, 3_500
                33, 16, 5_000
                34, 15, 2_900
                35, 14, 3_700
                36, 13, 18_600
                37, 12, 3_800
                38, 11, 2_600
                39, 12, 3_600
                40, 13, 16_400
                41, 14, 14_700
                42, 15, 4_900
                43, 16, 5_000
                44, 17, 3_700
                45, 18, 3_700
                46, 19, 3_700
                47, 20, 2_500
                48, 21, 3_400
            ]

    let w19s25 =
        room "W19S25"
        |> withSite trepidimous "road" None 0 { X = 9; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 10; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 11; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 12; Y = 37 }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 1_455_751
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
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 44
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 42
                        24, 44
                        25, 35
                        25, 37
                        25, 43
                        25, 45
                        26, 36
                        26, 38
                        26, 44
                        27, 35
                        27, 43
                        27, 45
                        28, 44
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 22, 38; 22, 40; 22, 42; 24, 38; 25, 39; 25, 41; 26, 40 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 150_200 250_000 { X = 13; Y = 29 } with
                    Energy = 732
                }
                structureOf "container" None 150_500 250_000 { X = 15; Y = 43 }
                { structureOf "container" None 120_600 250_000 { X = 27; Y = 42 } with
                    Energy = 1_950
                }
                { structureOf "container" None 125_000 250_000 { X = 44; Y = 38 } with
                    Energy = 704
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Energy = 50
                    Id = "extension-21-35-W17S29"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Energy = 50
                    Id = "extension-23-35-W17S29"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 99_001 30_000_000 { X = 13; Y = 29 } with
                    Id = "rampart-13-29-W17S29"
                    NextDecay = 994_981
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 994_992
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 994_928
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 994_970
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 994_925
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 24_518
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 1_000
                towerOf Side.Ours { X = 23; Y = 41 } 1_000
            ]
        |> withRoads
            [
                13, 29, 4_600
                14, 29, 3_200
                15, 30, 3_100
                16, 31, 3_200
                16, 42, 4_600
                17, 32, 3_400
                17, 42, 15_900
                18, 33, 4_300
                18, 41, 12_800
                19, 34, 19_700
                19, 40, 3_400
                19, 42, 4_500
                20, 35, 15_800
                20, 37, 13_200
                20, 39, 3_300
                20, 41, 3_400
                20, 43, 4_100
                21, 36, 4_300
                21, 38, 2_600
                21, 40, 3_800
                21, 42, 4_300
                21, 44, 4_600
                22, 35, 4_000
                22, 37, 3_200
                22, 39, 2_700
                22, 41, 3_700
                22, 43, 3_770
                22, 45, 2_560
                23, 36, 2_600
                23, 38, 4_200
                23, 42, 3_300
                23, 44, 4_070
                24, 35, 13_200
                24, 37, 3_100
                24, 39, 3_300
                24, 41, 3_300
                24, 43, 2_700
                24, 45, 3_070
                25, 36, 17_000
                25, 38, 3_300
                25, 40, 2_600
                25, 42, 3_000
                25, 44, 2_800
                26, 35, 18_200
                26, 37, 16_100
                26, 39, 12_700
                26, 41, 3_700
                26, 43, 3_700
                26, 45, 3_800
                27, 36, 12_500
                27, 37, 15_000
                27, 38, 13_600
                27, 39, 16_800
                27, 40, 13_900
                27, 41, 3_900
                27, 42, 3_000
                27, 44, 2_500
                28, 36, 12_600
                28, 37, 14_400
                28, 38, 13_600
                28, 39, 13_900
                28, 40, 18_500
                28, 41, 3_900
                28, 43, 2_900
                29, 35, 3_100
                29, 36, 14_900
                29, 37, 12_900
                29, 38, 19_100
                29, 39, 12_800
                29, 40, 15_000
                29, 41, 15_700
                29, 42, 3_700
                30, 34, 2_700
                30, 36, 14_900
                30, 37, 18_200
                30, 40, 13_800
                30, 41, 13_200
                30, 42, 14_800
                31, 33, 2_400
                31, 36, 16_600
                31, 41, 14_300
                31, 42, 17_100
                32, 32, 3_700
                32, 36, 14_300
                32, 41, 4_300
                33, 31, 2_700
                33, 36, 18_900
                33, 37, 20_300
                33, 40, 4_000
                34, 30, 3_300
                34, 39, 4_200
                35, 29, 4_100
                35, 38, 3_200
                36, 28, 2_600
                36, 37, 4_300
                37, 27, 3_900
                37, 38, 4_000
                38, 26, 4_000
                38, 39, 3_800
                39, 25, 3_500
                39, 40, 2_700
                40, 24, 2_500
                40, 41, 4_100
                41, 23, 3_900
                41, 40, 4_200
                42, 24, 4_300
                42, 39, 4_300
                43, 25, 2_600
                43, 38, 2_300
                44, 25, 4_300
                44, 37, 3_400
                44, 38, 2_400
                45, 26, 2_900
                45, 36, 3_000
                46, 26, 4_400
                46, 35, 3_400
                47, 25, 4_500
                47, 36, 2_500
                48, 20, 4_600
                48, 21, 4_400
                48, 22, 4_400
                48, 23, 4_540
                48, 24, 3_300
                48, 37, 3_200
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 139_900 250_000 { X = 27; Y = 9 } with
                    Energy = 482
                }
            ]
        |> withRoads
            [
                20, 1, 3_000
                21, 2, 4_480
                22, 3, 2_600
                23, 4, 3_100
                24, 5, 3_500
                25, 6, 3_100
                26, 7, 4_100
                27, 8, 2_500
            ]

    let w19s26 = room "W19S26" |> withRoads [ 9, 43, 700; 27, 27, 300 ]

    let w13s28 =
        room "W13S28"
        |> withController Ownership.Ours None 8 6
        |> withControllerClocks 0 0 200_000 0
        |> withSpawn "Spawn2" { X = 16; Y = 12 } 12_900
        |> withSpawn "Spawn7" { X = 10; Y = 6 } 12_900
        |> withSpawn "Spawn4" { X = 10; Y = 8 } 12_900
        |> withStructures
            [
                for x, y in
                    [
                        9, 5
                        9, 11
                        9, 13
                        9, 15
                        9, 17
                        9, 19
                        10, 12
                        10, 14
                        10, 16
                        10, 18
                        11, 7
                        11, 9
                        11, 11
                        11, 13
                        11, 15
                        11, 17
                        11, 19
                        12, 8
                        12, 10
                        12, 12
                        12, 14
                        12, 16
                        12, 18
                        13, 9
                        13, 11
                        13, 13
                        13, 15
                        13, 17
                        13, 19
                        14, 12
                        14, 14
                        14, 16
                        14, 18
                        15, 15
                        15, 17
                        15, 19
                        16, 10
                        16, 14
                        16, 16
                        16, 18
                        17, 9
                        17, 13
                        17, 15
                        17, 19
                        18, 10
                        18, 12
                        18, 14
                        19, 9
                        19, 11
                        19, 13
                        19, 15
                        20, 8
                        20, 10
                        20, 12
                        20, 14
                        21, 5
                        21, 9
                        21, 11
                        22, 10
                        22, 12
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 200
                    }
            ]
        |> withStructures
            [
                for x, y in [ 9, 7; 9, 9; 10, 10; 15, 13; 17, 11; 21, 13 ] ->
                    towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 205_000 250_000 { X = 6; Y = 25 }
                structureOf "container" None 127_500 250_000 { X = 15; Y = 8 }
                { structureOf "container" None 155_000 250_000 { X = 18; Y = 3 } with
                    Energy = 180
                }
                structureOf "container" None 141_700 250_000 { X = 22; Y = 15 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 7; Y = 25 }
                structureOf "observer" (Some Side.Ours) 500 500 { X = 23; Y = 5 }
                { structureOf "rampart" (Some Side.Ours) 106_061 300_000_000 { X = 9; Y = 7 } with
                    NextDecay = 994_953
                }
                { structureOf "rampart" (Some Side.Ours) 101_561 300_000_000 { X = 9; Y = 9 } with
                    NextDecay = 994_986
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 300_000_000 { X = 10; Y = 6 } with
                    NextDecay = 994_962
                }
                { structureOf "rampart" (Some Side.Ours) 101_381 300_000_000 { X = 10; Y = 8 } with
                    NextDecay = 994_977
                }
                { structureOf "rampart" (Some Side.Ours) 102_381 300_000_000 { X = 10; Y = 10 } with
                    NextDecay = 994_999
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 300_000_000 { X = 14; Y = 10 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some Side.Ours) 101_871 300_000_000 { X = 15; Y = 8 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 300_000_000 { X = 15; Y = 11 } with
                    NextDecay = 994_963
                }
                { structureOf "rampart" (Some Side.Ours) 102_901 300_000_000 { X = 15; Y = 13 } with
                    NextDecay = 994_972
                }
                { structureOf "rampart" (Some Side.Ours) 101_501 300_000_000 { X = 16; Y = 12 } with
                    NextDecay = 994_990
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 300_000_000 { X = 17; Y = 11 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some Side.Ours) 100_381 300_000_000 { X = 18; Y = 3 } with
                    NextDecay = 995_006
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 300_000_000 { X = 21; Y = 13 } with
                    NextDecay = 994_923
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 15; Y = 11 } with
                    Energy = 69_085
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 14; Y = 10 } with
                    Energy = 5_602
                }
            ]
        |> withRoads
            [
                8, 5, 4_000
                9, 4, 4_400
                9, 6, 18_900
                9, 8, 13_100
                9, 10, 14_100
                9, 12, 14_100
                9, 14, 19_600
                9, 16, 3_500
                9, 18, 3_500
                10, 3, 3_700
                10, 4, 3_400
                10, 5, 2_800
                10, 7, 21_200
                10, 9, 19_700
                10, 11, 17_800
                10, 13, 16_400
                10, 15, 4_000
                10, 17, 4_000
                10, 19, 3_900
                11, 2, 4_200
                11, 3, 4_600
                11, 6, 19_700
                11, 8, 20_900
                11, 10, 14_500
                11, 12, 19_800
                11, 14, 15_900
                11, 16, 4_800
                11, 18, 3_700
                12, 1, 3_000
                12, 2, 4_800
                12, 7, 21_100
                12, 9, 15_600
                12, 11, 18_900
                12, 13, 20_100
                12, 15, 4_200
                12, 17, 4_900
                12, 19, 3_900
                13, 1, 4_200
                13, 7, 12_600
                13, 8, 14_200
                13, 10, 14_300
                13, 12, 13_800
                13, 14, 4_200
                13, 16, 3_700
                13, 18, 3_100
                14, 1, 3_100
                14, 7, 19_400
                14, 8, 12_400
                14, 9, 17_900
                14, 11, 20_100
                14, 13, 3_000
                14, 15, 2_500
                14, 17, 2_800
                14, 19, 2_800
                15, 1, 5_000
                15, 8, 21_300
                15, 9, 17_900
                15, 10, 20_600
                15, 12, 5_000
                15, 14, 2_900
                15, 16, 4_100
                15, 18, 3_700
                15, 20, 2_600
                15, 22, 2_500
                15, 24, 4_400
                15, 26, 3_200
                15, 28, 2_500
                15, 30, 2_600
                15, 32, 3_200
                15, 34, 2_600
                16, 1, 2_600
                16, 9, 4_500
                16, 11, 4_800
                16, 13, 4_100
                16, 15, 4_600
                16, 17, 3_100
                16, 19, 2_500
                16, 21, 2_500
                16, 23, 4_200
                16, 25, 2_500
                16, 27, 4_000
                16, 29, 4_800
                16, 31, 2_600
                16, 33, 3_700
                16, 35, 4_900
                17, 2, 3_700
                17, 10, 3_300
                17, 12, 4_200
                17, 14, 4_100
                17, 16, 2_700
                17, 36, 3_300
                17, 48, 3_500
                18, 1, 3_400
                18, 2, 3_400
                18, 3, 4_200
                18, 9, 2_900
                18, 11, 3_300
                18, 13, 3_200
                18, 15, 4_700
                18, 37, 2_700
                18, 39, 2_900
                18, 41, 4_000
                18, 43, 4_400
                18, 45, 3_500
                18, 47, 3_700
                19, 2, 4_400
                19, 3, 17_900
                19, 8, 3_900
                19, 10, 4_700
                19, 12, 5_000
                19, 14, 3_600
                19, 38, 2_600
                19, 40, 2_600
                19, 42, 2_700
                19, 44, 2_900
                19, 46, 4_800
                20, 1, 3_900
                20, 4, 21_600
                20, 9, 4_100
                20, 11, 5_000
                20, 13, 4_000
                20, 15, 4_500
                21, 10, 18_200
                21, 12, 2_900
                21, 14, 4_200
                22, 11, 3_900
                22, 13, 3_500
                22, 15, 3_700
                22, 18, 15_900
                23, 14, 4_500
                23, 18, 15_700
                23, 19, 12_900
                24, 18, 14_500
                24, 19, 20_200
            ]

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 8 7
        |> withControllerClocks 0 0 200_000 0
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 12_900
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 12_900
        |> withSpawn "Spawn5" { X = 18; Y = 36 } 12_900
        |> withStructures
            [
                for x, y in
                    [
                        4, 36
                        7, 37
                        8, 32
                        8, 34
                        8, 36
                        9, 33
                        9, 35
                        9, 37
                        9, 39
                        9, 41
                        10, 32
                        10, 34
                        10, 38
                        10, 42
                        11, 33
                        11, 37
                        11, 39
                        11, 41
                        12, 38
                        12, 42
                        12, 44
                        13, 37
                        13, 39
                        13, 41
                        13, 43
                        14, 36
                        14, 38
                        14, 40
                        14, 44
                        15, 37
                        15, 39
                        15, 43
                        15, 45
                        16, 36
                        16, 38
                        16, 44
                        17, 35
                        17, 37
                        17, 43
                        17, 45
                        18, 42
                        18, 44
                        18, 46
                        19, 39
                        19, 45
                        19, 47
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 200
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 32
                        12, 34
                        13, 33
                        13, 35
                        14, 32
                        14, 34
                        15, 33
                        15, 35
                        16, 34
                        17, 33
                        19, 33
                        19, 35
                        19, 41
                        19, 43
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 200
                        Id = $"extension-{x}-{y}-W12S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 5, 37; 10, 36; 10, 40; 16, 46; 18, 38; 19, 37 ] ->
                    towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 203_900 250_000 { X = 7; Y = 39 } with
                    Energy = 1_985
                }
                structureOf "container" None 205_000 250_000 { X = 10; Y = 8 }
                { structureOf "container" None 136_900 250_000 { X = 10; Y = 43 } with
                    Energy = 1_308
                }
                { structureOf "container" None 130_400 250_000 { X = 16; Y = 39 } with
                    Energy = 1_308
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 11; Y = 7 }
                structureOf "observer" (Some Side.Ours) 500 500 { X = 16; Y = 32 }
                { structureOf "rampart" (Some Side.Ours) 100_661 300_000_000 { X = 5; Y = 37 } with
                    NextDecay = 994_924
                }
                { structureOf "rampart" (Some Side.Ours) 103_801 300_000_000 { X = 10; Y = 36 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 300_000_000 { X = 10; Y = 40 } with
                    NextDecay = 994_946
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 300_000_000 { X = 10; Y = 43 } with
                    NextDecay = 995_001
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 300_000_000 { X = 11; Y = 43 } with
                    NextDecay = 994_995
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 300_000_000 { X = 12; Y = 40 } with
                    NextDecay = 994_999
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 300_000_000 { X = 14; Y = 42 } with
                    NextDecay = 994_995
                }
                { structureOf "rampart" (Some Side.Ours) 102_211 300_000_000 { X = 16; Y = 39 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some Side.Ours) 103_001 300_000_000 { X = 16; Y = 46 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 300_000_000 { X = 18; Y = 34 } with
                    NextDecay = 995_008
                }
                { structureOf "rampart" (Some Side.Ours) 102_841 300_000_000 { X = 18; Y = 36 } with
                    NextDecay = 994_942
                }
                { structureOf "rampart" (Some Side.Ours) 103_021 300_000_000 { X = 18; Y = 38 } with
                    NextDecay = 994_963
                }
                { structureOf "rampart" (Some Side.Ours) 99_861 300_000_000 { X = 19; Y = 37 } with
                    NextDecay = 995_001
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 70_935
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 11; Y = 43 } with
                    Energy = 4_001
                }
            ]
        |> withRoads
            [
                1, 21, 3_900
                1, 22, 3_300
                2, 20, 3_900
                2, 23, 4_500
                2, 39, 17_000
                3, 19, 3_700
                3, 24, 2_900
                3, 39, 15_800
                4, 18, 4_000
                4, 25, 3_800
                4, 39, 15_000
                4, 40, 12_800
                5, 17, 4_100
                5, 26, 3_700
                5, 40, 17_200
                6, 6, 3_000
                6, 7, 3_800
                6, 8, 3_800
                6, 9, 4_000
                6, 10, 3_800
                6, 11, 3_800
                6, 12, 2_600
                6, 13, 3_100
                6, 14, 2_900
                6, 15, 3_000
                6, 16, 2_900
                6, 27, 18_700
                6, 40, 18_500
                6, 41, 16_300
                7, 5, 3_800
                7, 28, 2_500
                7, 39, 3_100
                7, 40, 15_800
                7, 41, 16_700
                8, 4, 2_800
                8, 29, 4_100
                8, 33, 4_800
                8, 35, 16_700
                8, 37, 3_300
                8, 38, 4_300
                8, 39, 3_600
                8, 41, 4_000
                9, 3, 4_700
                9, 30, 2_900
                9, 32, 3_100
                9, 34, 3_300
                9, 36, 4_300
                9, 38, 4_600
                9, 40, 4_400
                9, 42, 5_000
                10, 2, 3_800
                10, 31, 4_100
                10, 33, 4_200
                10, 35, 3_000
                10, 37, 3_200
                10, 39, 3_300
                10, 41, 4_300
                10, 43, 5_000
                11, 1, 4_100
                11, 32, 19_500
                11, 34, 4_100
                11, 38, 2_900
                11, 40, 3_300
                11, 42, 3_900
                12, 33, 4_500
                12, 37, 3_400
                12, 39, 3_200
                12, 41, 4_900
                12, 43, 21_300
                13, 32, 19_500
                13, 34, 2_500
                13, 36, 4_500
                13, 38, 4_900
                13, 40, 2_500
                13, 42, 13_200
                13, 44, 16_800
                14, 33, 4_600
                14, 35, 2_600
                14, 37, 4_100
                14, 39, 4_900
                14, 41, 4_500
                14, 43, 4_200
                14, 45, 3_100
                15, 32, 4_000
                15, 34, 4_000
                15, 36, 4_700
                15, 38, 3_300
                15, 40, 22_000
                15, 44, 3_600
                16, 33, 4_100
                16, 35, 4_900
                16, 37, 3_100
                16, 39, 3_100
                16, 43, 2_600
                16, 45, 3_400
                17, 34, 4_200
                17, 36, 4_800
                17, 38, 4_300
                17, 42, 16_600
                17, 44, 19_500
                17, 46, 4_300
                18, 33, 4_300
                18, 35, 2_800
                18, 37, 4_000
                18, 39, 2_400
                18, 41, 4_000
                18, 43, 2_700
                18, 45, 4_400
                19, 34, 3_400
                19, 36, 4_100
                19, 38, 4_200
                19, 40, 3_300
                19, 42, 21_000
                19, 44, 21_000
                19, 46, 4_200
                20, 40, 3_500
                21, 41, 3_700
                22, 42, 2_700
                23, 43, 3_600
                24, 44, 4_500
                25, 45, 4_700
                26, 46, 3_600
                27, 45, 4_600
                28, 44, 2_500
                29, 45, 4_700
                30, 46, 4_600
                31, 47, 3_800
                32, 48, 3_900
                33, 47, 2_800
                34, 46, 3_800
                35, 45, 4_800
                36, 44, 4_100
                37, 43, 4_200
                38, 42, 3_600
                39, 41, 3_800
                40, 40, 3_700
                41, 39, 5_000
                42, 38, 3_600
                43, 37, 4_900
                44, 36, 3_700
                45, 35, 4_700
                46, 34, 2_900
                47, 33, 3_800
                48, 32, 4_700
            ]

    let w12s26 =
        room "W12S26"
        |> withController Ownership.Ours None 5 4
        |> withControllerClocks 0 0 80_000 34_582
        |> withSpawn "Spawn10" { X = 37; Y = 34 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        30, 33
                        31, 34
                        31, 36
                        31, 38
                        31, 40
                        32, 33
                        32, 35
                        32, 37
                        33, 34
                        33, 36
                        34, 33
                        34, 35
                        35, 28
                        35, 30
                        35, 34
                        35, 36
                        36, 29
                        36, 31
                        37, 28
                        37, 30
                        37, 32
                        37, 36
                        38, 29
                        38, 31
                        38, 35
                        39, 30
                        39, 36
                        42, 37
                        43, 38
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 152_200 250_000 { X = 6; Y = 34 } with
                    Energy = 419
                }
                { structureOf "container" None 125_000 250_000 { X = 42; Y = 39 } with
                    Energy = 960
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 43; Y = 40 }
                { structureOf "rampart" (Some Side.Ours) 101_401 10_000_000 { X = 36; Y = 33 } with
                    NextDecay = 994_957
                }
                { structureOf "rampart" (Some Side.Ours) 85_801 10_000_000 { X = 36; Y = 35 } with
                    NextDecay = 994_996
                }
                { structureOf "rampart" (Some Side.Ours) 84_901 10_000_000 { X = 37; Y = 34 } with
                    NextDecay = 994_952
                }
                { structureOf "rampart" (Some Side.Ours) 67_701 10_000_000 { X = 40; Y = 37 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some Side.Ours) 85_501 10_000_000 { X = 42; Y = 39 } with
                    NextDecay = 994_929
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 36; Y = 33 } with
                    Energy = 9_942
                }
                towerOf Side.Ours { X = 36; Y = 35 } 1_000
                towerOf Side.Ours { X = 40; Y = 37 } 1_000
            ]
        |> withRoads
            [
                1, 36, 3_000
                2, 37, 3_000
                3, 38, 3_000
                4, 38, 3_700
                4, 39, 4_900
                5, 37, 3_100
                5, 40, 3_000
                5, 41, 2_500
                6, 35, 2_900
                6, 36, 20_600
                6, 42, 2_500
                7, 43, 3_100
                8, 44, 2_500
                9, 45, 4_400
                10, 46, 2_500
                11, 46, 4_900
                12, 47, 4_800
                13, 48, 3_800
                14, 47, 4_800
                15, 46, 4_800
                16, 45, 3_300
                17, 45, 3_800
                18, 44, 3_500
                19, 4, 3_200
                19, 5, 3_300
                19, 43, 4_900
                20, 2, 3_200
                20, 3, 3_200
                20, 6, 3_300
                20, 42, 2_400
                21, 1, 3_200
                21, 7, 3_300
                21, 41, 1_700
                22, 8, 3_200
                22, 42, 2_000
                23, 9, 3_200
                23, 13, 3_200
                23, 43, 2_100
                24, 10, 3_200
                24, 11, 3_200
                24, 12, 3_200
                24, 14, 3_200
                24, 43, 2_600
                25, 15, 3_200
                25, 43, 1_600
                26, 16, 3_200
                26, 42, 2_000
                27, 17, 3_100
                27, 43, 2_400
                28, 18, 3_100
                28, 42, 2_500
                29, 19, 3_100
                29, 41, 2_700
                30, 20, 3_100
                30, 40, 2_500
                31, 21, 3_100
                31, 33, 4_800
                31, 35, 24_000
                31, 37, 4_600
                31, 39, 2_500
                32, 22, 3_300
                32, 34, 4_700
                32, 36, 4_600
                32, 38, 2_300
                33, 23, 3_300
                33, 33, 4_700
                33, 35, 4_700
                33, 37, 3_100
                34, 24, 3_300
                34, 32, 4_700
                34, 34, 4_700
                34, 36, 3_100
                35, 25, 3_300
                35, 27, 2_900
                35, 29, 2_800
                35, 31, 4_200
                35, 33, 4_600
                35, 35, 2_100
                36, 26, 3_300
                36, 28, 3_000
                36, 30, 2_500
                36, 32, 4_000
                36, 34, 3_300
                36, 36, 4_800
                37, 29, 4_800
                37, 31, 4_700
                37, 33, 3_200
                37, 35, 4_700
                38, 30, 4_800
                38, 34, 2_400
                38, 36, 4_800
                39, 35, 3_700
                40, 36, 3_800
                41, 37, 3_600
                42, 38, 2_000
                42, 39, 2_700
                43, 39, 4_800
            ]

    [
        w18s25
        w17s25
        w15s28
        w19s25
        w17s29
        w17s26
        w19s26
        w13s28
        w12s28
        w12s26
    ]
