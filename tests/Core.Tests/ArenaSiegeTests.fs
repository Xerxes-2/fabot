/// W17S29 under an NPC siege on captured terrain: its towers starved, the
/// dismantler at its walls, and the healer glued to its raid.
module Fabot.Core.Tests.ArenaSiegeTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

// ---- arena capture w17s29-starved-towers (#591) ---------------------------
// `npm run capture -- W17S29 996765 --rooms W15S28 --name w17s29-starved --no-live-memory`, read at live t999,322.
// The room-history replay at t996,765, the state after that tick ran: arena
// tick 0 runs as t996,766 and decides what our code decided live then.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996700.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/996700.json
// The two Invaders run the engine's NPC script; the Raid log, ArmedSeen,
// Healers and Sightings start empty (the tick is past `--live-memory`).
// Spawn3 is spawning guard-996644-Spawn3 until t996794: not placed.
// Spawn8 is spawning worker-996759-Spawn8 until t996909: not placed.
// not placed: 2 tombstone, 1 ruin (the arena has no drops, tombstones or ruins).

/// W17S29 at t996,765: a dismantler and its healer on the anchor's rampart
/// (44,38, 116k), both towers drained to 300 and 340, three loaded workers
/// and 19k in the Storage.
let private w17s29StarvedTowers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 119_902 1_503_130
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
                        21, 45
                        22, 36
                        22, 44
                        23, 35
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
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 408
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 1_188
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 99_901 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_862
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_774
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_809
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_852
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_851
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_857
                }
                { structureOf "rampart" (Some Side.Ours) 116_311 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_806
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 19_140
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 340
                towerOf Side.Ours { X = 23; Y = 41 } 300
            ]
        |> withRoads
            [
                13, 29, 4_400
                14, 29, 2_900
                15, 30, 2_800
                16, 31, 2_800
                16, 42, 4_500
                17, 32, 3_100
                17, 42, 15_400
                18, 33, 3_900
                18, 41, 20_600
                19, 34, 17_700
                19, 40, 3_200
                19, 42, 4_300
                20, 35, 13_800
                20, 37, 20_500
                20, 39, 3_000
                20, 41, 3_100
                20, 43, 3_900
                21, 36, 3_800
                21, 38, 4_400
                21, 40, 3_600
                21, 42, 4_000
                21, 44, 4_400
                22, 35, 3_700
                22, 37, 2_800
                22, 39, 4_600
                22, 41, 3_400
                22, 43, 3_570
                22, 45, 4_560
                23, 36, 4_500
                23, 38, 3_700
                23, 42, 3_000
                23, 44, 3_770
                24, 35, 20_500
                24, 37, 2_700
                24, 39, 2_800
                24, 41, 2_900
                24, 43, 2_400
                24, 45, 2_870
                25, 36, 15_500
                25, 38, 2_800
                25, 40, 4_200
                25, 42, 2_700
                25, 44, 2_600
                26, 35, 16_700
                26, 37, 14_100
                26, 39, 19_500
                26, 41, 3_200
                26, 43, 3_400
                26, 45, 3_600
                27, 36, 19_300
                27, 37, 13_500
                27, 38, 12_100
                27, 39, 15_300
                27, 40, 20_100
                27, 41, 3_600
                27, 42, 2_800
                27, 44, 4_400
                28, 36, 19_900
                28, 37, 13_400
                28, 38, 12_100
                28, 39, 12_900
                28, 40, 17_500
                28, 41, 3_700
                28, 43, 2_700
                29, 35, 2_800
                29, 36, 13_400
                29, 37, 20_700
                29, 38, 18_100
                29, 39, 20_100
                29, 40, 14_000
                29, 41, 14_200
                29, 42, 3_400
                30, 34, 4_600
                30, 36, 13_400
                30, 37, 17_200
                30, 40, 21_100
                30, 41, 20_500
                30, 42, 13_800
                31, 33, 4_300
                31, 36, 15_600
                31, 41, 21_100
                31, 42, 15_600
                32, 32, 3_500
                32, 36, 19_400
                32, 41, 3_900
                33, 31, 2_400
                33, 36, 17_400
                33, 37, 18_800
                33, 40, 3_700
                34, 30, 3_000
                34, 39, 3_800
                35, 29, 3_700
                35, 38, 2_700
                36, 28, 4_500
                36, 37, 3_900
                37, 27, 3_600
                37, 38, 3_500
                38, 26, 3_700
                38, 39, 3_300
                39, 25, 3_200
                39, 40, 4_800
                40, 24, 4_400
                40, 41, 3_500
                41, 23, 3_600
                41, 40, 3_600
                42, 24, 4_000
                42, 39, 3_800
                43, 25, 3_460
                44, 25, 3_080
                44, 37, 3_000
                44, 38, 4_400
                45, 26, 1_060
                45, 36, 2_600
                46, 26, 3_770
                46, 35, 3_000
                47, 25, 3_870
                47, 36, 4_400
                48, 20, 4_170
                48, 21, 3_970
                48, 22, 3_770
                48, 23, 4_010
                48, 24, 2_670
                48, 37, 3_000
            ]
        |> withSite Side.Ours "road" None 0 { X = 43; Y = 38 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_990 6_183_616
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        13, 25
                        13, 27
                        13, 29
                        14, 26
                        14, 28
                        15, 25
                        15, 27
                        15, 29
                        16, 26
                        16, 30
                        17, 25
                        17, 27
                        18, 26
                        18, 28
                        19, 27
                        20, 26
                        20, 28
                        21, 27
                        21, 29
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 30
                        12, 32
                        12, 34
                        12, 36
                        13, 31
                        13, 33
                        13, 35
                        14, 30
                        14, 32
                        14, 34
                        15, 31
                        15, 33
                        15, 35
                        16, 32
                        16, 34
                        17, 33
                        18, 32
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 31
                        21, 33
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 708
                }
                { structureOf "container" None 176_100 250_000 { X = 11; Y = 18 } with
                    Energy = 324
                }
                structureOf "container" None 132_500 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Id = "extension-21-35-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Energy = 100
                    Id = "extension-23-35-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 996_797
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 996_856
                }
                { structureOf "rampart" (Some Side.Ours) 102_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 996_815
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 996_784
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 996_857
                }
                { structureOf "rampart" (Some Side.Ours) 102_801 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 996_811
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 996_832
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 996_781
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 996_774
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 24_211
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_200
                5, 4, 2_900
                5, 6, 4_100
                5, 44, 3_200
                6, 3, 4_700
                6, 7, 3_400
                6, 29, 2_900
                6, 43, 2_800
                6, 45, 3_700
                7, 2, 3_600
                7, 8, 4_200
                7, 28, 3_700
                7, 42, 2_600
                7, 46, 3_100
                8, 1, 4_700
                8, 9, 5_000
                8, 28, 5_000
                8, 41, 2_900
                8, 47, 4_900
                9, 10, 3_400
                9, 27, 3_200
                9, 28, 4_100
                9, 40, 2_600
                9, 48, 3_200
                10, 11, 2_600
                10, 26, 3_700
                10, 29, 3_600
                10, 39, 2_900
                11, 12, 4_700
                11, 18, 2_500
                11, 27, 4_900
                11, 30, 3_800
                11, 38, 3_400
                12, 13, 3_000
                12, 17, 3_400
                12, 25, 16_600
                12, 27, 16_500
                12, 28, 4_200
                12, 29, 3_700
                12, 31, 2_600
                12, 33, 13_700
                12, 35, 17_000
                12, 37, 2_800
                13, 14, 3_100
                13, 16, 2_700
                13, 26, 21_900
                13, 28, 2_500
                13, 30, 4_000
                13, 32, 4_600
                13, 34, 2_900
                13, 36, 3_000
                14, 15, 3_400
                14, 25, 16_700
                14, 27, 4_500
                14, 29, 3_100
                14, 31, 3_200
                14, 33, 3_900
                14, 35, 3_300
                15, 15, 2_700
                15, 26, 4_100
                15, 28, 2_500
                15, 30, 4_600
                15, 32, 2_900
                15, 34, 2_900
                16, 16, 2_700
                16, 25, 19_100
                16, 27, 3_700
                16, 29, 4_500
                16, 31, 2_900
                16, 33, 4_900
                16, 35, 2_500
                17, 17, 3_700
                17, 26, 15_400
                17, 28, 3_700
                17, 30, 3_800
                17, 32, 3_600
                17, 34, 3_100
                18, 18, 4_900
                18, 25, 15_700
                18, 27, 15_000
                18, 29, 2_900
                18, 31, 2_700
                18, 33, 3_200
                19, 19, 20_300
                19, 22, 16_600
                19, 24, 21_100
                19, 26, 16_000
                19, 28, 4_200
                19, 30, 2_900
                19, 32, 3_800
                20, 20, 20_600
                20, 21, 17_100
                20, 23, 17_300
                20, 25, 16_400
                20, 27, 20_800
                20, 29, 3_100
                20, 31, 4_900
                20, 33, 19_200
                21, 21, 3_800
                21, 22, 4_500
                21, 23, 4_700
                21, 24, 18_100
                21, 25, 14_600
                21, 26, 18_400
                21, 28, 13_500
                21, 30, 3_500
                21, 32, 14_000
                21, 34, 20_300
                22, 24, 4_000
                22, 25, 14_800
                22, 27, 16_400
                22, 28, 3_400
                22, 29, 2_900
                22, 30, 20_100
                22, 31, 15_300
                22, 32, 17_100
                22, 33, 14_700
                22, 35, 3_600
                23, 26, 16_300
                23, 28, 13_000
                23, 29, 15_700
                23, 30, 16_400
                23, 31, 18_600
                23, 32, 16_900
                23, 33, 20_300
                23, 34, 4_300
                24, 25, 13_100
                24, 28, 17_000
                24, 29, 18_600
                24, 30, 15_900
                24, 33, 12_500
                25, 24, 3_800
                25, 28, 18_200
                25, 29, 17_800
                25, 34, 18_900
                26, 23, 16_500
                27, 22, 13_700
                28, 21, 13_400
                28, 34, 8_100
                29, 20, 15_200
                30, 19, 13_100
                31, 18, 3_300
                32, 17, 3_200
                33, 16, 4_700
                34, 15, 2_500
                35, 14, 3_400
                36, 13, 17_100
                37, 12, 3_500
                38, 11, 2_300
                39, 12, 3_300
                40, 13, 14_900
                41, 14, 13_200
                42, 15, 4_600
                43, 16, 4_700
                44, 17, 3_400
                45, 18, 3_400
                46, 19, 3_400
                47, 20, 4_900
                48, 21, 3_100
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W15S28" ])

    let bodies: Body list =
        [
            // invader_W17S29_832: live: attack→44,38 rangedAttack→44,38 healed→41,40
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 43 39)
                  (Some Invader) with
                TicksToLive = 1_342
            }
            // invader_W17S29_976: live: rangedHeal→43,39
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 40 41)
                  (Some Invader) with
                TicksToLive = 1_342
            }
            { body
                  "worker-995234-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 37 38)
                  None with
                TicksToLive = 72
                Energy = 600
            }
            { body
                  "worker-995430-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 37 39)
                  None with
                TicksToLive = 268
                Energy = 504
            }
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 460
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 818
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 842
                Energy = 50
            }
            { body
                  "worker-996318-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 44 33)
                  None with
                TicksToLive = 1_156
                Energy = 600
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_249
            }
            { body
                  "worker-996636-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 48 23)
                  None with
                TicksToLive = 1_474
            }
            { body
                  "reserver-996741-Spawn9"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W17S29" 43 25)
                  None with
                TicksToLive = 580
            }
            { body
                  "hauler-995926-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 48 35)
                  None with
                TicksToLive = 794
                Energy = 1_500
            }
            { body
                  "worker-995684-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 13 26)
                  None with
                TicksToLive = 567
            }
            { body
                  "anchor-996028-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "hauler-996036-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 28)
                  None with
                TicksToLive = 913
                Energy = 1_600
            }
            { body
                  "anchor-996217-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 974
                Energy = 50
            }
            { body
                  "hauler-996263-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 19 26)
                  None with
                TicksToLive = 1_140
            }
            { body
                  "upgrader-996497-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_377
            }
            { body
                  "guard-996609-Spawn8"
                  Side.Ours
                  (parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ])
                  (at "W15S28" 10 39)
                  None with
                TicksToLive = 1_492
            }
            { body
                  "hauler-996091-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 15 30)
                  None with
                TicksToLive = 968
                Energy = 1_400
            }
            { body
                  "hauler-995872-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 13)
                  None with
                TicksToLive = 749
                Energy = 1_544
            }
        ]

    // No Transition-log verdict reaches back to t996,765: every body starts
    // unassigned and the first tick hands out what live held (the workers
    // on rampart-44-38's Repair, the hauler on tower-23-41's Refill).
    arena 996_766 [ w17s29; w15s28 ] colonies bodies

[<Tests>]
let w17s29StarvedTowersTests =
    testList
        "arena capture w17s29-starved-towers"
        [
            // Live from t996,770 the towers sat at their 300 reserve for a
            // hundred ticks with 19k in the Storage: three loaded workers
            // held the struck rampart's Repair parked at the Reach's edge,
            // no walk round to it (the arena reproduced it unchanged), and
            // the towers had spent ~2,000 energy repairing a 112k rampart.
            test
                "#591 capture t996,765: the raided home keeps both towers fed from the Storage, and no tower repairs a rampart that outlasts the raid" {
                let final, trace = w17s29StarvedTowers () |> run 30
                let failure = describe trace

                Expect.isEmpty
                    (trace
                     |> List.collect (fun t -> t.Ours)
                     |> List.filter (function
                         | RepairWithTower _ -> true
                         | _ -> false))
                    $"116k against the dismantler's 200 a tick outlasts the horizon: the energy held\n{failure}"

                for id in [ "tower-21-39"; "tower-23-41" ] do
                    let energy =
                        final.Rooms["W17S29"].Structures
                        |> List.find (fun s -> s.Id = id)
                        |> fun s -> s.Energy

                    Expect.isGreaterThan
                        energy
                        600
                        $"{id} fed (live: 300 for a hundred ticks)\n{failure}"
            }
        ]

// ---- arena capture w17s29-dismantler (#589) ----------------------------------
// `npm run capture -- W17S29 996620 --rooms W15S28 --name w17s29-dismantler`,
// read at live t999,213. The room-history replay at t996,620, the state after
// that tick ran: arena tick 0 runs as t996,621.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/996600.json
// Left out, as the capture says: the Raid log, ArmedSeen, Healers and
// Sightings start empty; no body holds a Task (the Transition log holds none
// that early); every body not ours Holds; Spawn3's upgrader (until t996,644)
// and Spawn8's guard-996609 (until t996,759) are not placed, so both spawns
// stand idle; nor are three tombstones.

/// W17S29 (RCL6, a 2,300 bank) at t996,620: an Invader dismantler, its
/// 25-HEAL healer and an 18-RANGED_ATTACK shooter in the north of the home,
/// two towers at 910. Live, W17S29 cast workers, a reserver and a hauler for
/// 230 ticks while its mother W15S28, two rooms off, cast the only guard
/// (t996,609, in the home at t996,838): `homeUnheld` handed a child's home to
/// its mother whatever the child could cast (#589).
let private w17s29Dismantler () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 1_501_935
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
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 38
                        22, 40
                        22, 42
                        22, 44
                        23, 35
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 38
                        24, 42
                        25, 35
                        25, 37
                        25, 39
                        25, 41
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 20, 44; 24, 44; 25, 43; 25, 45; 26, 44; 27, 43; 27, 45; 28, 44 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 372
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 48
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_664
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_675
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_710
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_654
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_653
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_707
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 21_240
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 910
                towerOf Side.Ours { X = 23; Y = 41 } 910
            ]
        |> withRoads
            [
                13, 29, 4_400
                14, 29, 2_900
                15, 30, 2_800
                16, 31, 2_800
                16, 42, 4_500
                17, 32, 3_100
                17, 42, 15_400
                18, 33, 3_900
                18, 41, 20_600
                19, 34, 17_700
                19, 40, 3_200
                19, 42, 4_300
                20, 35, 13_800
                20, 37, 20_500
                20, 39, 3_100
                20, 41, 3_100
                20, 43, 3_900
                21, 36, 3_900
                21, 38, 4_500
                21, 40, 3_600
                21, 42, 4_100
                21, 44, 4_400
                22, 35, 3_800
                22, 37, 2_800
                22, 39, 4_600
                22, 41, 3_400
                22, 43, 3_570
                22, 45, 4_560
                23, 36, 4_500
                23, 38, 3_800
                23, 42, 3_100
                23, 44, 3_770
                24, 35, 20_500
                24, 37, 2_800
                24, 39, 2_900
                24, 41, 2_900
                24, 43, 2_500
                24, 45, 2_870
                25, 36, 15_500
                25, 38, 2_800
                25, 40, 4_300
                25, 42, 2_700
                25, 44, 2_600
                26, 35, 16_700
                26, 37, 14_100
                26, 39, 20_000
                26, 41, 3_200
                26, 43, 3_400
                26, 45, 3_600
                27, 36, 19_300
                27, 37, 13_500
                27, 38, 12_600
                27, 39, 15_800
                27, 40, 20_100
                27, 41, 3_600
                27, 42, 2_800
                27, 44, 4_500
                28, 36, 19_900
                28, 37, 13_400
                28, 38, 12_600
                28, 39, 12_900
                28, 40, 17_500
                28, 41, 3_700
                28, 43, 2_700
                29, 35, 2_900
                29, 36, 13_400
                29, 37, 20_700
                29, 38, 18_100
                29, 39, 20_100
                29, 40, 14_000
                29, 41, 14_700
                29, 42, 3_400
                30, 34, 4_600
                30, 36, 13_900
                30, 37, 17_200
                30, 40, 21_100
                30, 41, 20_500
                30, 42, 13_800
                31, 33, 4_400
                31, 36, 15_600
                31, 41, 21_100
                31, 42, 16_100
                32, 32, 3_500
                32, 36, 19_400
                32, 41, 4_000
                33, 31, 2_400
                33, 36, 17_900
                33, 37, 19_300
                33, 40, 3_700
                34, 30, 3_000
                34, 39, 3_800
                35, 29, 3_800
                35, 38, 2_800
                36, 28, 4_500
                36, 37, 3_900
                37, 27, 3_700
                37, 38, 3_600
                38, 26, 3_700
                38, 39, 3_400
                39, 25, 3_200
                39, 40, 4_800
                40, 24, 4_400
                40, 41, 3_600
                41, 23, 3_600
                41, 40, 3_700
                42, 24, 4_000
                42, 39, 3_800
                43, 25, 4_500
                43, 38, 4_100
                44, 25, 4_000
                44, 37, 3_100
                44, 38, 4_400
                45, 26, 2_500
                45, 36, 2_700
                46, 26, 4_100
                46, 35, 3_100
                47, 25, 4_200
                47, 36, 4_400
                48, 20, 4_400
                48, 21, 4_200
                48, 22, 4_100
                48, 23, 4_240
                48, 24, 3_000
                48, 37, 3_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_180_606
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 36
                        13, 25
                        13, 27
                        13, 35
                        14, 26
                        14, 28
                        15, 25
                        15, 27
                        15, 29
                        16, 26
                        16, 30
                        16, 32
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        20, 26
                        21, 27
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 30
                        12, 32
                        12, 34
                        13, 29
                        13, 31
                        13, 33
                        14, 30
                        14, 32
                        14, 34
                        15, 31
                        15, 33
                        15, 35
                        16, 34
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 29
                        21, 31
                        21, 33
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 36
                }
                { structureOf "container" None 176_100 250_000 { X = 11; Y = 18 } with
                    Energy = 876
                }
                { structureOf "container" None 132_500 250_000 { X = 23; Y = 29 } with
                    Energy = 742
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 20; Y = 28 } with
                    Energy = 50
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Id = "extension-21-35-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Id = "extension-23-35-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 996_698
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 996_658
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 996_716
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 996_629
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 996_712
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 996_634
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 996_682
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 996_675
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 24_317
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_200
                5, 4, 2_900
                5, 6, 4_100
                5, 44, 3_200
                6, 3, 4_700
                6, 7, 3_400
                6, 29, 2_900
                6, 43, 2_800
                6, 45, 3_700
                7, 2, 3_600
                7, 8, 4_200
                7, 28, 3_800
                7, 42, 2_600
                7, 46, 3_100
                8, 1, 4_800
                8, 9, 5_000
                8, 28, 5_000
                8, 41, 2_900
                8, 47, 4_900
                9, 10, 3_500
                9, 27, 3_300
                9, 28, 4_200
                9, 40, 2_600
                9, 48, 3_200
                10, 11, 2_600
                10, 26, 3_800
                10, 29, 3_700
                10, 39, 2_900
                11, 12, 4_700
                11, 18, 2_500
                11, 27, 5_000
                11, 30, 3_800
                11, 38, 3_400
                12, 13, 3_100
                12, 17, 3_400
                12, 25, 16_600
                12, 27, 17_000
                12, 28, 4_200
                12, 29, 3_700
                12, 31, 2_600
                12, 33, 13_700
                12, 35, 17_000
                12, 37, 2_800
                13, 14, 3_100
                13, 16, 2_700
                13, 26, 21_900
                13, 28, 2_500
                13, 30, 4_000
                13, 32, 4_600
                13, 34, 2_900
                13, 36, 3_000
                14, 15, 3_400
                14, 25, 16_700
                14, 27, 4_500
                14, 29, 3_100
                14, 31, 3_200
                14, 33, 3_900
                14, 35, 3_400
                15, 15, 2_800
                15, 26, 4_100
                15, 28, 2_500
                15, 30, 4_600
                15, 32, 3_000
                15, 34, 2_900
                16, 16, 2_800
                16, 25, 19_100
                16, 27, 3_800
                16, 29, 4_500
                16, 31, 3_000
                16, 33, 4_900
                16, 35, 2_600
                17, 17, 3_800
                17, 26, 15_900
                17, 28, 3_800
                17, 30, 3_900
                17, 32, 3_600
                17, 34, 3_100
                18, 18, 4_900
                18, 25, 16_200
                18, 27, 15_500
                18, 29, 2_900
                18, 31, 2_700
                18, 33, 3_200
                19, 19, 20_800
                19, 22, 16_600
                19, 24, 21_600
                19, 26, 16_500
                19, 28, 4_200
                19, 30, 2_900
                19, 32, 3_800
                20, 20, 21_100
                20, 21, 17_600
                20, 23, 17_800
                20, 25, 16_900
                20, 27, 20_800
                20, 29, 3_100
                20, 31, 4_900
                20, 33, 19_200
                21, 21, 3_800
                21, 22, 4_500
                21, 23, 4_700
                21, 24, 18_100
                21, 25, 14_600
                21, 26, 18_900
                21, 28, 14_000
                21, 30, 3_500
                21, 32, 14_000
                21, 34, 20_300
                22, 24, 4_000
                22, 25, 14_800
                22, 27, 16_400
                22, 28, 3_400
                22, 29, 2_900
                22, 30, 20_100
                22, 31, 15_300
                22, 32, 17_100
                22, 33, 14_700
                22, 35, 3_600
                23, 26, 16_300
                23, 28, 13_000
                23, 29, 15_700
                23, 30, 16_400
                23, 31, 18_600
                23, 32, 16_900
                23, 33, 20_300
                23, 34, 4_300
                24, 25, 13_100
                24, 28, 17_000
                24, 29, 18_600
                24, 30, 15_900
                24, 33, 12_500
                25, 24, 3_800
                25, 28, 18_200
                25, 29, 17_800
                25, 34, 18_900
                26, 23, 16_500
                27, 22, 13_700
                28, 21, 13_400
                28, 34, 8_100
                29, 20, 15_200
                30, 19, 13_600
                31, 18, 3_300
                32, 17, 3_300
                33, 16, 4_700
                34, 15, 2_600
                35, 14, 3_400
                36, 13, 17_100
                37, 12, 3_600
                38, 11, 2_400
                39, 12, 3_400
                40, 13, 14_900
                41, 14, 13_700
                42, 15, 4_700
                43, 16, 4_700
                44, 17, 3_500
                45, 18, 3_500
                46, 19, 3_500
                47, 20, 4_900
                48, 21, 3_200
            ]

    // Today's declarations, not the tick's: the two homes' are unchanged.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-995080-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 36)
                  None with
                TicksToLive = 63
                Energy = 435
            }
            { body
                  "worker-995234-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 40)
                  None with
                TicksToLive = 217
                Energy = 248
            }
            { body
                  "worker-995430-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 40 41)
                  None with
                TicksToLive = 413
                Energy = 516
            }
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 605
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 963
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 987
                Energy = 50
            }
            { body
                  "hauler-996183-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 22 43)
                  None with
                TicksToLive = 1_196
                Energy = 423
            }
            { body
                  "worker-996318-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 36)
                  None with
                TicksToLive = 1_301
                Energy = 512
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_394
            }
            { body
                  "ranger-996535-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W17S29" 42 23)
                  None with
                TicksToLive = 1_467
            }
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 41 8)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // live: rangedHeal→42,9
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 41 9)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // live: attacked→21,39 healed→40,8
            { body
                  "invader_W17S29_855"
                  (Side.Npc "Invader")
                  (parts [ Tough, 6; Move, 24; RangedAttack, 18; Work, 1; Move, 1 ])
                  (at "W17S29" 42 9)
                  (Some Invader) with
                Hits = 3_100
                TicksToLive = 1_487
            }
            { body
                  "upgrader-995000-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 25
                Energy = 26
            }
            { body
                  "worker-995684-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 31)
                  None with
                TicksToLive = 712
                Energy = 402
            }
            { body
                  "anchor-996028-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 930
                Energy = 50
            }
            { body
                  "hauler-996036-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 30)
                  None with
                TicksToLive = 1_058
                Energy = 1_300
            }
            { body
                  "hauler-996091-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 29)
                  None with
                TicksToLive = 1_113
                Energy = 1_300
            }
            { body
                  "anchor-996217-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_119
                Energy = 48
            }
            { body
                  "hauler-996263-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 20 27)
                  None with
                TicksToLive = 1_285
                Energy = 1_600
            }
            { body
                  "worker-996313-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 12 35)
                  None with
                TicksToLive = 1_341
                Energy = 650
            }
            { body
                  "hauler-995872-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 12)
                  None with
                TicksToLive = 894
                Energy = 1_402
            }
        ]

    arena 996_621 [ w17s29; w15s28 ] colonies bodies

/// The rows our spawns ask to cast on a run's first tick, by spawn.
let private firstCasts (trace: TickTrace list) =
    trace
    |> List.head
    |> fun t ->
        t.Ours
        |> List.choose (function
            | SpawnCreep(spawn, _, name) -> Some(spawn, name.Split('-')[0])
            | _ -> None)

let private isFighterRow (_, row) = row = "guard" || row = "ranger"

[<Tests>]
let w17s29DismantlerTests =
    testList
        "arena capture w17s29-dismantler"
        [
            // The decision side reproduces live: W15S28 casts the guard,
            // Spawn9 a hauler. Now by cost, not kinship: the trio's 270 a
            // tick, focused, kills a 3-block guard in ~13 ticks, so neither
            // one body a 2,300 bank buys nor the two its row casts win, and
            // W15S28's do. The arena's fight does not reproduce live
            // (#592); only the cast is pinned.
            test
                "#589 capture t996,620: a raid the home's own cast loses is cast by the colony whose cast wins" {
                let start = w17s29Dismantler ()
                let _, trace = start |> run 1
                let casts = firstCasts trace

                let wins home =
                    fst (
                        Fabot.Core.Decide.Facts.castPrice
                            (viewOf start home)
                            (CastRow.Fighter BodyTemplate.Melee)
                            "W17S29"
                    )

                // W15S28's guard wins holding the post, cast unseated at a
                // home of ours, which is fought for whatever the gate (ADR
                // 0092's home exception).
                Expect.equal
                    (wins "W17S29", wins "W15S28")
                    (false, true)
                    $"the premise: the home's own guard cast loses, W15S28's wins\n{casts}"

                Expect.exists
                    casts
                    (fun (spawn, row) -> (spawn = "Spawn3" || spawn = "Spawn8") && row = "guard")
                    $"W15S28 casts the guard\n{casts}"

                Expect.isFalse
                    (casts
                     |> List.exists (fun (spawn, row) ->
                         spawn = "Spawn9" && isFighterRow (spawn, row)))
                    $"and W17S29 none\n{casts}"
            }

            // What the pricing above says, fought: two 3-block guards, the
            // pair W17S29's row would cast, standing beside the trio and
            // holding its Guard (named W15S28's, whose pool holds it), lose.
            test "#589 capture t996,620: two of the home's 3-block guards lose the trio" {
                let start = w17s29Dismantler ()

                let block =
                    List.replicate 3 Fabot.Core.Decide.Bodies.guardPattern.Block |> List.concat

                let pair =
                    [ "guard-996600-Spawn3", 40, 11; "guard-996601-Spawn3", 42, 11 ]
                    |> List.map (fun (name, x, y) ->
                        body name Side.Ours block (RoomPos.at "W17S29" { X = x; Y = y }) None)

                let fought =
                    { start with
                        Bodies = start.Bodies @ pair
                        Carried =
                            { start.Carried with
                                Assignments =
                                    pair |> List.map (fun b -> b.Id, "guard:W17S29") |> Map.ofList
                            }
                    }

                let _, trace = fought |> run 150
                let failure = describe trace

                Expect.isTrue
                    (pair |> List.forall (fun b -> Option.isSome (diedOn b.Id trace)))
                    $"both guards die\n{failure}"
            }

            // The flip: live the shooter fell to the towers at t996,658,
            // leaving the dismantler and its healer, which a body the home
            // buys wins beside its towers (#590's pricing); and the home
            // still built workers, its mother's guard 180 ticks away, as
            // `homeUnheld` refused a child's home its own row. Red before.
            test
                "#589 capture t996,620, the shooter dead: the home casts its own defence, and its mother none" {
                let start = w17s29Dismantler ()

                let shrunk =
                    { start with
                        Bodies = start.Bodies |> List.filter (fun b -> b.Id <> "invader_W17S29_855")
                    }

                let _, trace = shrunk |> run 1
                let casts = firstCasts trace

                Expect.exists
                    casts
                    (fun (spawn, row) -> spawn = "Spawn9" && isFighterRow (spawn, row))
                    $"Spawn9 casts a fighter for its home\n{casts}"

                Expect.isFalse
                    (casts
                     |> List.exists (fun (spawn, row) ->
                         spawn <> "Spawn9" && isFighterRow (spawn, row)))
                    $"and W15S28, which lands later, none\n{casts}"
            }
        ]

// ---- arena capture w17s29-glued-healer (#592) -------------------------------
// `npm run capture -- W17S29 996620 --rooms W15S28,W15S29,W16S29 --name w17s29-glued-healer`, read at live t1,000,473.
// The room-history replay at t996,620, the state after that tick ran: arena
// tick 0 runs as t996,621 and decides what our code decided live then.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/996600.json
//   W15S29  https://screeps.com/room-history/shardSeason/W15S29/996600.json
//   W16S29  https://screeps.com/room-history/shardSeason/W16S29/996600.json
// The three Invaders run the engine's NPC script; the Raid log, ArmedSeen,
// Healers and Sightings start empty (the tick is past `--live-memory`).
// W15S29 and W16S29 are reserved by us: the arena has no reservations.
// not placed: 3 tombstone (the arena has no drops, tombstones or ruins).
// Left out: ranger-996535-Spawn9 (42,23), which live walked off west at
// t996,621 and out of the north exit at t996,714 on a task the Transition
// log no longer holds, shooting nothing; the arena's idle one shoots the
// dismantler from 3.

/// W17S29 at t996,620: an Invader dismantler, its 25-HEAL healer and a hurt
/// shooter on the north edge, two towers at 910, and W15S28's guard in its
/// spawn (live it reached W17S29 at t996,838 by W15S29 and W16S29).
let private w17s29GluedHealer () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 1_501_935
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
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 38
                        22, 40
                        22, 42
                        22, 44
                        23, 35
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 38
                        24, 42
                        25, 35
                        25, 37
                        25, 39
                        25, 41
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 20, 44; 24, 44; 25, 43; 25, 45; 26, 44; 27, 43; 27, 45; 28, 44 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 372
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 48
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_664
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_675
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_710
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_654
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_653
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_707
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 21_240
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 910
                towerOf Side.Ours { X = 23; Y = 41 } 910
            ]
        |> withRoads
            [
                13, 29, 4_400
                14, 29, 2_900
                15, 30, 2_800
                16, 31, 2_800
                16, 42, 4_500
                17, 32, 3_100
                17, 42, 15_400
                18, 33, 3_900
                18, 41, 20_600
                19, 34, 17_700
                19, 40, 3_200
                19, 42, 4_300
                20, 35, 13_800
                20, 37, 20_500
                20, 39, 3_100
                20, 41, 3_100
                20, 43, 3_900
                21, 36, 3_900
                21, 38, 4_500
                21, 40, 3_600
                21, 42, 4_100
                21, 44, 4_400
                22, 35, 3_800
                22, 37, 2_800
                22, 39, 4_600
                22, 41, 3_400
                22, 43, 3_570
                22, 45, 4_560
                23, 36, 4_500
                23, 38, 3_800
                23, 42, 3_100
                23, 44, 3_770
                24, 35, 20_500
                24, 37, 2_800
                24, 39, 2_900
                24, 41, 2_900
                24, 43, 2_500
                24, 45, 2_870
                25, 36, 15_500
                25, 38, 2_800
                25, 40, 4_300
                25, 42, 2_700
                25, 44, 2_600
                26, 35, 16_700
                26, 37, 14_100
                26, 39, 20_000
                26, 41, 3_200
                26, 43, 3_400
                26, 45, 3_600
                27, 36, 19_300
                27, 37, 13_500
                27, 38, 12_600
                27, 39, 15_800
                27, 40, 20_100
                27, 41, 3_600
                27, 42, 2_800
                27, 44, 4_500
                28, 36, 19_900
                28, 37, 13_400
                28, 38, 12_600
                28, 39, 12_900
                28, 40, 17_500
                28, 41, 3_700
                28, 43, 2_700
                29, 35, 2_900
                29, 36, 13_400
                29, 37, 20_700
                29, 38, 18_100
                29, 39, 20_100
                29, 40, 14_000
                29, 41, 14_700
                29, 42, 3_400
                30, 34, 4_600
                30, 36, 13_900
                30, 37, 17_200
                30, 40, 21_100
                30, 41, 20_500
                30, 42, 13_800
                31, 33, 4_400
                31, 36, 15_600
                31, 41, 21_100
                31, 42, 16_100
                32, 32, 3_500
                32, 36, 19_400
                32, 41, 4_000
                33, 31, 2_400
                33, 36, 17_900
                33, 37, 19_300
                33, 40, 3_700
                34, 30, 3_000
                34, 39, 3_800
                35, 29, 3_800
                35, 38, 2_800
                36, 28, 4_500
                36, 37, 3_900
                37, 27, 3_700
                37, 38, 3_600
                38, 26, 3_700
                38, 39, 3_400
                39, 25, 3_200
                39, 40, 4_800
                40, 24, 4_400
                40, 41, 3_600
                41, 23, 3_600
                41, 40, 3_700
                42, 24, 4_000
                42, 39, 3_800
                43, 25, 4_500
                43, 38, 4_100
                44, 25, 4_000
                44, 37, 3_100
                44, 38, 4_400
                45, 26, 2_500
                45, 36, 2_700
                46, 26, 4_100
                46, 35, 3_100
                47, 25, 4_200
                47, 36, 4_400
                48, 20, 4_400
                48, 21, 4_200
                48, 22, 4_100
                48, 23, 4_240
                48, 24, 3_000
                48, 37, 3_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_180_606
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 36
                        13, 25
                        13, 27
                        13, 35
                        14, 26
                        14, 28
                        15, 25
                        15, 27
                        15, 29
                        16, 26
                        16, 30
                        16, 32
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        20, 26
                        21, 27
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 30
                        12, 32
                        12, 34
                        13, 29
                        13, 31
                        13, 33
                        14, 30
                        14, 32
                        14, 34
                        15, 31
                        15, 33
                        15, 35
                        16, 34
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 29
                        21, 31
                        21, 33
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 36
                }
                { structureOf "container" None 176_100 250_000 { X = 11; Y = 18 } with
                    Energy = 876
                }
                { structureOf "container" None 132_500 250_000 { X = 23; Y = 29 } with
                    Energy = 742
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 20; Y = 28 } with
                    Energy = 50
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Id = "extension-21-35-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Id = "extension-23-35-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 996_698
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 996_658
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 996_716
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 996_629
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 996_712
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 996_634
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 996_682
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 996_675
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 24_317
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_200
                5, 4, 2_900
                5, 6, 4_100
                5, 44, 3_200
                6, 3, 4_700
                6, 7, 3_400
                6, 29, 2_900
                6, 43, 2_800
                6, 45, 3_700
                7, 2, 3_600
                7, 8, 4_200
                7, 28, 3_800
                7, 42, 2_600
                7, 46, 3_100
                8, 1, 4_800
                8, 9, 5_000
                8, 28, 5_000
                8, 41, 2_900
                8, 47, 4_900
                9, 10, 3_500
                9, 27, 3_300
                9, 28, 4_200
                9, 40, 2_600
                9, 48, 3_200
                10, 11, 2_600
                10, 26, 3_800
                10, 29, 3_700
                10, 39, 2_900
                11, 12, 4_700
                11, 18, 2_500
                11, 27, 5_000
                11, 30, 3_800
                11, 38, 3_400
                12, 13, 3_100
                12, 17, 3_400
                12, 25, 16_600
                12, 27, 17_000
                12, 28, 4_200
                12, 29, 3_700
                12, 31, 2_600
                12, 33, 13_700
                12, 35, 17_000
                12, 37, 2_800
                13, 14, 3_100
                13, 16, 2_700
                13, 26, 21_900
                13, 28, 2_500
                13, 30, 4_000
                13, 32, 4_600
                13, 34, 2_900
                13, 36, 3_000
                14, 15, 3_400
                14, 25, 16_700
                14, 27, 4_500
                14, 29, 3_100
                14, 31, 3_200
                14, 33, 3_900
                14, 35, 3_400
                15, 15, 2_800
                15, 26, 4_100
                15, 28, 2_500
                15, 30, 4_600
                15, 32, 3_000
                15, 34, 2_900
                16, 16, 2_800
                16, 25, 19_100
                16, 27, 3_800
                16, 29, 4_500
                16, 31, 3_000
                16, 33, 4_900
                16, 35, 2_600
                17, 17, 3_800
                17, 26, 15_900
                17, 28, 3_800
                17, 30, 3_900
                17, 32, 3_600
                17, 34, 3_100
                18, 18, 4_900
                18, 25, 16_200
                18, 27, 15_500
                18, 29, 2_900
                18, 31, 2_700
                18, 33, 3_200
                19, 19, 20_800
                19, 22, 16_600
                19, 24, 21_600
                19, 26, 16_500
                19, 28, 4_200
                19, 30, 2_900
                19, 32, 3_800
                20, 20, 21_100
                20, 21, 17_600
                20, 23, 17_800
                20, 25, 16_900
                20, 27, 20_800
                20, 29, 3_100
                20, 31, 4_900
                20, 33, 19_200
                21, 21, 3_800
                21, 22, 4_500
                21, 23, 4_700
                21, 24, 18_100
                21, 25, 14_600
                21, 26, 18_900
                21, 28, 14_000
                21, 30, 3_500
                21, 32, 14_000
                21, 34, 20_300
                22, 24, 4_000
                22, 25, 14_800
                22, 27, 16_400
                22, 28, 3_400
                22, 29, 2_900
                22, 30, 20_100
                22, 31, 15_300
                22, 32, 17_100
                22, 33, 14_700
                22, 35, 3_600
                23, 26, 16_300
                23, 28, 13_000
                23, 29, 15_700
                23, 30, 16_400
                23, 31, 18_600
                23, 32, 16_900
                23, 33, 20_300
                23, 34, 4_300
                24, 25, 13_100
                24, 28, 17_000
                24, 29, 18_600
                24, 30, 15_900
                24, 33, 12_500
                25, 24, 3_800
                25, 28, 18_200
                25, 29, 17_800
                25, 34, 18_900
                26, 23, 16_500
                27, 22, 13_700
                28, 21, 13_400
                28, 34, 8_100
                29, 20, 15_200
                30, 19, 13_600
                31, 18, 3_300
                32, 17, 3_300
                33, 16, 4_700
                34, 15, 2_600
                35, 14, 3_400
                36, 13, 17_100
                37, 12, 3_600
                38, 11, 2_400
                39, 12, 3_400
                40, 13, 14_900
                41, 14, 13_700
                42, 15, 4_700
                43, 16, 4_700
                44, 17, 3_500
                45, 18, 3_500
                46, 19, 3_500
                47, 20, 4_900
                48, 21, 3_200
            ]

    let w15s29 =
        room "W15S29"
        |> withStructures
            [
                { structureOf "container" None 121_300 250_000 { X = 17; Y = 19 } with
                    Energy = 552
                }
            ]
        |> withRoads
            [
                4, 6, 4_700
                5, 5, 3_580
                5, 7, 4_200
                6, 4, 2_500
                6, 8, 4_500
                7, 3, 3_000
                7, 9, 4_900
                8, 2, 3_300
                8, 10, 4_500
                9, 1, 3_800
                9, 11, 4_000
                10, 12, 4_000
                11, 13, 2_800
                12, 14, 3_500
                13, 15, 2_900
                14, 16, 3_200
                15, 17, 4_800
                16, 18, 4_800
            ]

    let w16s29 =
        room "W16S29"
        |> withStructures
            [
                { structureOf "container" None 141_400 250_000 { X = 16; Y = 42 } with
                    Energy = 618
                }
                { structureOf "container" None 125_100 250_000 { X = 22; Y = 21 } with
                    Energy = 872
                }
            ]
        |> withRoads
            [
                1, 20, 4_110
                1, 37, 4_010
                2, 19, 4_130
                2, 38, 2_830
                3, 18, 3_710
                3, 39, 3_100
                4, 18, 4_900
                4, 40, 2_800
                5, 18, 4_110
                5, 41, 2_500
                6, 18, 3_110
                6, 42, 2_500
                7, 18, 3_450
                7, 41, 3_200
                8, 18, 2_750
                8, 40, 3_500
                9, 18, 3_150
                9, 39, 4_100
                10, 18, 3_550
                10, 40, 4_100
                11, 18, 4_130
                11, 40, 2_700
                12, 18, 2_930
                12, 40, 3_700
                13, 18, 4_230
                13, 41, 4_500
                14, 18, 4_230
                14, 41, 2_880
                15, 18, 4_680
                15, 41, 3_200
                16, 18, 3_380
                17, 18, 4_300
                18, 18, 4_400
                19, 18, 4_100
                20, 19, 3_400
                21, 20, 4_200
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-995080-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 36)
                  None with
                TicksToLive = 63
                Energy = 435
            }
            { body
                  "worker-995234-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 40)
                  None with
                TicksToLive = 217
                Energy = 248
            }
            { body
                  "worker-995430-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 40 41)
                  None with
                TicksToLive = 413
                Energy = 516
            }
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 605
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 963
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 987
                Energy = 50
            }
            { body
                  "hauler-996183-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 22 43)
                  None with
                TicksToLive = 1_196
                Energy = 423
            }
            { body
                  "worker-996318-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 36)
                  None with
                TicksToLive = 1_301
                Energy = 512
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_394
            }
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 41 8)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // invader_W17S29_976: live: rangedHeal→42,9
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 41 9)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // invader_W17S29_855: live: 12 MOVE, 18 RANGED_ATTACK, 1 WORK
            // invader_W17S29_855: live: attacked→21,39 healed→40,8
            { body
                  "invader_W17S29_855"
                  (Side.Npc "Invader")
                  (parts [ Tough, 6; Move, 24; RangedAttack, 18; Work, 1; Move, 1 ])
                  (at "W17S29" 42 9)
                  (Some Invader) with
                Hits = 3_100
                TicksToLive = 1_487
            }
            { body
                  "upgrader-995000-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 25
                Energy = 26
            }
            { body
                  "worker-995684-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 31)
                  None with
                TicksToLive = 712
                Energy = 402
            }
            { body
                  "anchor-996028-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 930
                Energy = 50
            }
            { body
                  "hauler-996036-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 30)
                  None with
                TicksToLive = 1_058
                Energy = 1_300
            }
            { body
                  "hauler-996091-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 29)
                  None with
                TicksToLive = 1_113
                Energy = 1_300
            }
            { body
                  "anchor-996217-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_119
                Energy = 48
            }
            { body
                  "hauler-996263-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 20 27)
                  None with
                TicksToLive = 1_285
                Energy = 1_600
            }
            { body
                  "worker-996313-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 12 35)
                  None with
                TicksToLive = 1_341
                Energy = 650
            }
            { body
                  "hauler-995872-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 12)
                  None with
                TicksToLive = 894
                Energy = 1_402
            }
            { body
                  "reserver-996235-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S29" 13 35)
                  None with
                TicksToLive = 219
            }
            { body
                  "anchor-996407-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S29" 17 19)
                  None with
                TicksToLive = 1_315
                Energy = 38
            }
            { body
                  "anchor-995393-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 16 42)
                  None with
                TicksToLive = 301
                Energy = 50
            }
            { body
                  "anchor-995835-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 22 21)
                  None with
                TicksToLive = 743
                Energy = 50
            }
            { body
                  "reserver-996109-Spawn9"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W16S29" 22 33)
                  None with
                TicksToLive = 93
            }
            { body
                  "hauler-995926-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W16S29" 7 14)
                  None with
                TicksToLive = 939
            }
        ]

    let a = arena 996_621 [ w17s29; w15s28; w15s29; w16s29 ] colonies bodies

    // Both W15S28 spawns were busy at the tick (the skeleton places neither):
    // the bodies and finishing ticks from the replay after it.
    { a with
        Ovens =
            [
                {
                    Spawn = "spawn-Spawn3"
                    Room = "W15S28"
                    Name = "upgrader-996497-Spawn3"
                    Parts = parts [ Work, 24; Carry, 1; Move, 24 ]
                    Done = 996_644
                }
                {
                    Spawn = "spawn-Spawn8"
                    Room = "W15S28"
                    Name = "guard-996609-Spawn8"
                    Parts = parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ]
                    Done = 996_759
                }
            ]
    }

[<Tests>]
let w17s29GluedHealerTests =
    testList
        "arena capture w17s29-glued-healer"
        [
            // Live (t996,621-679) the shooter died at t996,657 under both
            // towers; the dismantler then walked the x=48 swamp gap, the towers
            // took it to 600, and from t996,675 its healer stood beside it:
            // 300 a tick against 2×150, the towers held their fire and it
            // healed to full. The pin ends at t58: then live the dismantler
            // turned east for the anchor while here it turns west for
            // upgrader-995634, and the rest (it lived to the guard at
            // t996,838) is not reproduced: our economy bodies stand where a
            // capture without the tick's Memory puts them, and the guard
            // stays home.
            test
                "#592 capture t996,620: the Invader healer glues to its hurt dismantler and out-heals two towers, which hold their fire, as live" {
                let _, trace = w17s29GluedHealer () |> run 59
                let failure = describe trace
                let dismantler = "invader_W17S29_832"
                let healer = "invader_W17S29_976"
                let path = pathOf dismantler trace |> Map.ofList

                Expect.equal
                    (diedOn "invader_W17S29_855" trace)
                    (Some 36)
                    $"the shooter dies at t996,657\n{failure}"

                for tick, (x, y), hits in
                    [
                        8, (46, 13), 5_000
                        24, (48, 15), 4_000
                        50, (44, 25), 1_300
                        53, (44, 25), 600
                        58, (43, 25), 2_100
                    ] do
                    let at = path[tick]

                    Expect.equal
                        (at.At.X, at.At.Y, at.Hits)
                        (x, y, hits)
                        $"the dismantler at t{tick} as live\n{failure}"

                for t in trace |> List.filter (fun t -> t.Tick >= 54) do
                    let near =
                        t.Bodies
                        |> List.filter (fun b -> b.Id = dismantler || b.Id = healer)
                        |> List.map (fun b -> b.At)

                    Expect.equal
                        (near |> List.length,
                         near
                         |> List.pairwise
                         |> List.forall (fun (p, q) -> max (abs (p.X - q.X)) (abs (p.Y - q.Y)) <= 1))
                        (2, true)
                        $"t{t.Tick}: the healer beside the dismantler\n{failure}"

                    Expect.contains
                        t.Theirs
                        (healer, Act.Heal dismantler)
                        $"t{t.Tick}: healing it\n{failure}"

                    Expect.isEmpty
                        (t.Ours
                         |> List.filter (function
                             | FireTower _ -> true
                             | _ -> false))
                        $"t{t.Tick}: 300 healed against 2×150, the towers hold\n{failure}"
            }
        ]
