/// Live captures of our bodies moving round hostiles (the movement ADR's S0
/// and S4 symptoms): arena worlds `ArenaBorderTests` and `ArenaLaneTests` replay.
module Fabot.Core.Tests.ArenaMovementCaptures

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

// ---- arena capture w17s26-t979950 (#551) ------------------------------------
// `npm run capture -- W17S26 979950 --rooms W17S25,W15S28`, read at live t980,066.
// The room-history replay at t979,950, the state after that tick ran: arena
// tick 0 runs as t979,951.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/979900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/979900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/979900.json
// Healers, ExitWatches and Sightings start empty; the Raid log, ArmedSeen and
// Towered are live Memory's at t980,066, less what postdates the tick.
// W17S26's reservation, 262 roads and 2 tombstones are not placed; a ranger
// spawning at Spawn3 until t980,040 is not placed.

/// A body that steps one way on even arena ticks and back on odd ones.
let internal swaying (out: Direction) (back: Direction) =
    Phases [ for t in 0..99 -> t + 1, Do [ Act.Move(if t % 2 = 0 then out else back) ] ]

/// W17S26 at t979,950 (#551's "Why"): Trepidimous' Onyx632 sways between
/// (22,3) and (21,2), and Morgana682 between (21,4) and (22,4), every tick
/// (live t979,951-979,970), round hauler-979756-Spawn11 at (21,3) on its way
/// to the container at (27,9). Live, the hauler stepped every tick onto the
/// tile one of them was entering, the engine refused it, and it stood.
let internal swayingTrespassers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let rampart hits hitsMax (x, y, decay) =
        { structureOf "rampart" (Some Side.Ours) hits hitsMax { X = x; Y = y } with
            NextDecay = decay
        }

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 1_088
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_940 121_695
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
                structureOf "container" None 166_600 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 378
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_339
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withStructures
            [
                for hits, tile in
                    [
                        91_021, (2, 15, 979_956)
                        91_861, (2, 16, 980_036)
                        99_561, (2, 17, 980_039)
                        99_631, (2, 18, 980_044)
                        90_271, (2, 19, 980_016)
                        99_731, (2, 20, 980_018)
                        99_521, (2, 23, 980_043)
                        93_501, (2, 24, 980_005)
                        92_401, (2, 25, 980_022)
                        92_061, (2, 26, 980_037)
                        92_561, (2, 27, 979_986)
                        82_401, (13, 27, 980_036)
                        62_201, (13, 29, 980_021)
                        13_501, (14, 28, 979_997)
                        93_921, (16, 44, 979_985)
                        12_201, (17, 20, 980_042)
                        93_901, (17, 44, 979_987)
                        97_301, (18, 44, 979_983)
                        91_531, (32, 31, 979_996)
                        90_481, (32, 32, 980_014)
                    ] -> rampart hits 3_000_000 tile
            ]
        |> withSite Side.Ours "road" None 682 { X = 33; Y = 30 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_217 5_857_785
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [ 12, 30; 13, 25; 13, 31; 14, 26; 14, 30; 15, 27; 15, 29; 15, 31; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
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
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 177_100 250_000 { X = 6; Y = 29 } with
                    Energy = 1_578
                }
                { structureOf "container" None 145_100 250_000 { X = 11; Y = 18 } with
                    Energy = 718
                }
                structureOf "container" None 142_300 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 24 } with
                    Energy = 100
                    Id = "extension-12-24-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 26 } with
                    Energy = 100
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 32 } with
                    Energy = 100
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 34 } with
                    Energy = 1
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 14_513
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 1_688
                }
            ]
        |> withStructures
            [
                for hits, tile in
                    [
                        99_501, (6, 29, 979_967)
                        101_201, (11, 18, 980_026)
                        97_601, (16, 28, 979_985)
                        88_501, (17, 29, 980_044)
                        98_301, (17, 31, 980_027)
                        97_801, (17, 35, 979_981)
                        98_901, (18, 30, 980_002)
                        98_301, (19, 29, 980_050)
                        100_301, (20, 34, 980_043)
                    ] -> rampart hits 100_000_000 tile
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let hauler (a, b) = parts [ Move, a; Carry, b ]

    let bodies: Body list =
        [
            // Live: sways (23,12)-(24,11), no act.
            { body
                  "Gambit229"
                  trepidimous
                  (hauler (23, 23))
                  (at "W17S26" 23 12)
                  (Some(swaying TopRight BottomLeft)) with
                Hits = 4_160
                TicksToLive = 915
            }
            // Live: no act.
            { body
                  "Infinity425"
                  trepidimous
                  (parts [ Move, 3; BodyPart.Claim, 3 ])
                  (at "W17S26" 10 23)
                  (Some Hold) with
                TicksToLive = 91
                Fatigue = 18
            }
            // Live 3 MOVE, 5 WORK, swaying on a road the arena does not lay:
            // two MOVE more keep its step every tick on plain.
            { body
                  "Onyx632"
                  trepidimous
                  (parts [ Move, 5; Work, 5 ])
                  (at "W17S26" 22 3)
                  (Some(swaying TopLeft BottomRight)) with
                TicksToLive = 1_204
            }
            // Live: sways (21,4)-(22,4), no act.
            { body
                  "Morgana682"
                  trepidimous
                  (hauler (23, 23))
                  (at "W17S26" 21 4)
                  (Some(swaying Right Left)) with
                TicksToLive = 1_368
            }
            { body
                  "anchor-978575-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 153
                Energy = 50
            }
            { body
                  "guard-978780-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 26 7)
                  None with
                TicksToLive = 358
            }
            { body
                  "reserver-979607-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 267
            }
            { body
                  "hauler-979756-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S26" 21 3)
                  None with
                TicksToLive = 1_376
            }
            { body
                  "anchor-978551-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 123
                Energy = 50
            }
            { body
                  "defender-978831-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 4 19)
                  None with
                TicksToLive = 439
            }
            { body
                  "guard-979625-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 1 15)
                  None with
                TicksToLive = 1_203
            }
            { body
                  "upgrader-979829-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_428
            }
            { body
                  "hauler-978947-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 29)
                  None with
                TicksToLive = 567
            }
            { body
                  "guard-979116-Spawn8"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 48 20)
                  None with
                TicksToLive = 694
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_203
                Energy = 50
            }
            { body
                  "ranger-979811-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W15S28" 20 20)
                  None with
                TicksToLive = 1_485
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 14 15)
                  None with
                TicksToLive = 1_454
            }
            { body
                  "hauler-979425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 35 14)
                  None with
                TicksToLive = 1_117
                Energy = 1_600
            }
            { body
                  "hauler-979281-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 36 13)
                  None with
                TicksToLive = 973
                Energy = 536
            }
            { body
                  "worker-979732-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 9 40)
                  None with
                TicksToLive = 1_430
                Energy = 850
            }
        ]

    let a = arena 979_951 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t979,950,
                // read at live t980,066; it held none that early for
                // ranger-979811-Spawn8.
                Assignments =
                    Map.ofList
                        [
                            "anchor-978575-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "guard-978780-Spawn11", "guard:W17S26"
                            "reserver-979607-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "hauler-979756-Spawn11", "withdraw:container-27-9"
                            "anchor-978551-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-978947-Spawn11", "withdraw:container-27-9"
                            "anchor-979625-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "anchor-979882-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-979425-Spawn3", "refill:spawn-Spawn3"
                            "hauler-979281-Spawn3", "refill:spawn-Spawn3"
                            "worker-979732-Spawn3", "refill:spawn-Spawn3"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 979_698; "W17S26", 979_497 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 979_950
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                                Fought =
                                    Map.ofList
                                        [
                                            "W17S25",
                                            ({
                                                Seen = 979_698
                                                Raiders = Set.ofList [ "6ac3dbeec7926d7481bf9b87" ]
                                            }
                                            : FightRecord)
                                        ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 979_698
                            }
                        ]
            }
    }

// ---- arena capture ranger-into-melee (movement S0) -------------------------
// `npm run capture -- W17S26 986005 --rooms W17S25,W15S28 --name rangerIntoMelee`, read at live t986,233.
// The room-history replay at t986,005, the state after that tick ran: arena
// tick 0 runs as t986,006.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/986000.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/986000.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/986000.json
// Healers, ExitWatches and Sightings start empty; ArmedSeen, Towered and the
// Raid log are live Memory's at t986,233, less what postdates the tick.
// The 263 roads are laid (#560); W17S26's reservation and a tombstone are not
// placed, nor the ranger Spawn8 was casting; the invader core stands as a structure only (the
// arena lays no `InvaderCores`, #558). The Transition log no longer held the
// two W17S26 rangers' Tasks, nor the anchor's or the guards'.

// ---- arena capture: W17S26 t1,006,904 (movement S4, #583) -------------------
// `npm run capture -- W17S26 1006904 --rooms W17S25`, read at live t1,007,728.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1006900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1006900.json
// W17S26's reservation, 6 ruins and a tombstone are not placed, nor the
// anchor Spawn11 was casting. The Transition log no longer held any Task of
// the tick's. W17S25's ramparts are laid without their decay clocks.

/// W17S26 at t1,006,904: Trepidimous' Coefficient6755 (17 MOVE + 1, 17
/// ATTACK, a step a tick) walks in along y = 1 from (14,1). Live,
/// anchor-1005773 stepped from (22,3) to the exit road's (20,1), toward the
/// melee, then back to (22,3), where it was struck dead (t1,006,911-912);
/// reserver-1006558 (2 CLAIM 2 MOVE, the melee's pace) walked north-east from
/// (16,20) at the melee coming down the road, turned at (26,9) and was caught
/// at (22,11) (t1,006,924).
let internal trepEntersW17S26 (melee: Behaviour) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 169_900 250_000 { X = 27; Y = 9 } with
                    Energy = 420
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    100_000
                    100_000
                    { X = 12; Y = 25 }
            ]
        |> withRoads
            [
                20, 1, 3_600
                21, 2, 2_680
                22, 3, 3_200
                23, 4, 3_700
                24, 5, 1_700
                25, 6, 3_700
                26, 7, 2_300
                27, 8, 3_500
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 79_811 7_093
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
                        15, 31
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
                for x, y in [ 15, 27; 15, 29; 16, 28; 16, 30 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 142_000 250_000 { X = 16; Y = 34 } with
                    Energy = 1_000
                }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 518
                }
                yield!
                    [
                        for x, y, hits in
                            [
                                2, 15, 99_721
                                2, 16, 99_661
                                2, 17, 90_931
                                2, 18, 99_231
                                2, 19, 98_811
                                2, 20, 98_731
                                2, 23, 93_321
                                2, 24, 93_901
                                2, 25, 93_401
                                2, 26, 93_661
                                2, 27, 94_261
                                11, 29, 25_001
                                13, 27, 87_201
                                13, 29, 87_201
                                14, 28, 86_601
                                16, 44, 100_321
                                17, 20, 87_001
                                17, 44, 100_301
                                18, 44, 100_101
                                32, 31, 99_911
                                32, 32, 93_201
                            ] ->
                            structureOf "rampart" (Some Side.Ours) hits 10_000_000 { X = x; Y = y }
                    ]
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 5_982
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 2_300
                2, 17, 1_800
                3, 18, 2_200
                4, 19, 3_400
                5, 20, 3_300
                6, 21, 3_800
                7, 22, 12_900
                8, 23, 10_700
                9, 24, 11_000
                9, 26, 24_500
                9, 28, 24_000
                10, 23, 24_000
                10, 25, 12_600
                10, 27, 13_300
                10, 29, 14_600
                11, 24, 15_600
                11, 26, 7_600
                11, 28, 8_300
                11, 30, 3_400
                12, 23, 13_300
                12, 25, 9_700
                12, 27, 10_500
                12, 29, 1_800
                12, 31, 4_600
                13, 24, 1_500
                13, 26, 1_900
                13, 28, 2_800
                13, 30, 1_900
                14, 23, 1_800
                14, 25, 2_300
                14, 27, 1_900
                14, 29, 2_100
                14, 31, 4_900
                15, 22, 2_900
                15, 28, 3_100
                15, 30, 3_900
                15, 32, 4_800
                16, 21, 3_200
                16, 29, 4_000
                16, 31, 4_900
                16, 33, 3_700
                16, 35, 2_600
                16, 37, 4_000
                16, 38, 8_600
                16, 39, 9_100
                16, 41, 4_800
                16, 43, 4_600
                16, 44, 3_800
                17, 20, 3_700
                17, 30, 1_800
                17, 32, 2_600
                17, 34, 5_000
                17, 36, 3_200
                17, 38, 2_800
                17, 39, 14_000
                17, 40, 19_100
                17, 42, 5_000
                17, 45, 5_000
                18, 29, 2_900
                18, 31, 3_300
                18, 35, 3_900
                18, 37, 3_000
                18, 39, 3_700
                18, 41, 1_900
                18, 46, 3_700
                19, 30, 2_600
                19, 34, 3_200
                19, 36, 2_500
                19, 38, 3_800
                19, 40, 3_700
                19, 47, 3_600
                20, 31, 4_400
                20, 33, 3_000
                20, 35, 3_700
                20, 37, 3_100
                20, 39, 3_800
                20, 48, 3_000
                21, 32, 2_800
                21, 34, 2_600
                21, 36, 3_800
                21, 38, 2_600
                22, 33, 3_100
                22, 35, 3_800
                22, 37, 3_200
                23, 34, 3_100
                23, 36, 3_600
                24, 35, 3_700
                25, 34, 2_100
                26, 33, 2_500
                27, 32, 2_200
                28, 31, 7_000
                28, 33, 11_600
                29, 32, 15_900
                29, 34, 14_100
                30, 31, 8_000
                30, 33, 13_400
                31, 32, 11_400
                32, 31, 20_800
                33, 30, 15_140
                33, 32, 7_380
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            // Ocean6422 (23 MOVE 23 CARRY): live, no act.
            { body
                  "Ocean6422"
                  trepidimous
                  (parts [ Move, 23; Carry, 23 ])
                  (at "W17S26" 28 8)
                  (Some Hold) with
                TicksToLive = 1_154
            }
            { body
                  "Coefficient6755"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 14 1)
                  (Some melee) with
                TicksToLive = 1_454
            }
            { body
                  "anchor-1005773-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 22 3)
                  None with
                TicksToLive = 397
                Energy = 50
            }
            { body
                  "reserver-1006558-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 16 20)
                  None with
                TicksToLive = 264
            }
            { body
                  "hauler-1005435-Spawn11"
                  Side.Ours
                  (parts [ Carry, 20; Move, 10 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 119
            }
            { body
                  "guard-1005661-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 17 47)
                  None with
                TicksToLive = 315
            }
            { body
                  "worker-1006267-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 10; Move, 9 ])
                  (at "W17S25" 16 31)
                  None with
                TicksToLive = 942
                Energy = 201
            }
            { body
                  "hauler-1006605-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 15 22)
                  None with
                TicksToLive = 1_307
            }
            { body
                  "hauler-1006713-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 1_415
                Energy = 50
            }
            { body
                  "ranger-1006821-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S25" 18 47)
                  None with
                TicksToLive = 1_451
            }
        ]

    arena 1_006_905 [ w17s26; w17s25 ] colonies bodies

/// W17S26 at t986,005 (the movement ADR's W17S26 symptom): Trepidimous' HalfLife847
/// (17 MOVE + 1, 17 ATTACK, a step a tick) walks in from (20,1) on its way to
/// the container at (27,8); our ranger-984393-Spawn3 (21M 14R 7H) at (25,7)
/// walked toward it (24,6), (24,5) to shoot at range 2 (t986,008), the melee
/// stepped beside it and struck it four times, 4,200 to 2,496 hits
/// (t986,009-986,012).
let internal rangerIntoMelee () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_800 250_000 { X = 27; Y = 9 } with
                    Energy = 586
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    57_550
                    100_000
                    { X = 12; Y = 25 }
            ]
        |> withRoads
            [
                20, 1, 4_600
                21, 2, 4_200
                22, 3, 4_200
                23, 4, 4_700
                24, 5, 2_700
                25, 6, 4_700
                26, 7, 3_200
                27, 8, 3_600
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 205_038
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
                { structureOf "container" None 139_200 250_000 { X = 16; Y = 34 } with
                    Energy = 366
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 360
                }
                { structureOf "rampart" (Some Side.Ours) 98_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 986_094
                }
                { structureOf "rampart" (Some Side.Ours) 98_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 986_075
                }
                { structureOf "rampart" (Some Side.Ours) 99_261 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 986_078
                }
                { structureOf "rampart" (Some Side.Ours) 98_131 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 986_083
                }
                { structureOf "rampart" (Some Side.Ours) 98_641 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 986_055
                }
                { structureOf "rampart" (Some Side.Ours) 98_831 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 986_057
                }
                { structureOf "rampart" (Some Side.Ours) 98_621 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 986_082
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 986_044
                }
                { structureOf "rampart" (Some Side.Ours) 98_701 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 986_061
                }
                { structureOf "rampart" (Some Side.Ours) 98_961 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 986_076
                }
                { structureOf "rampart" (Some Side.Ours) 98_261 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 986_025
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 986_075
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 986_060
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 986_036
                }
                { structureOf "rampart" (Some Side.Ours) 99_621 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 986_024
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 986_081
                }
                { structureOf "rampart" (Some Side.Ours) 99_601 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 986_026
                }
                { structureOf "rampart" (Some Side.Ours) 94_001 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 986_022
                }
                { structureOf "rampart" (Some Side.Ours) 92_801 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 986_035
                }
                { structureOf "rampart" (Some Side.Ours) 92_291 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 986_053
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_277
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 3_400
                2, 17, 3_500
                3, 18, 3_400
                4, 19, 4_800
                5, 20, 4_800
                6, 21, 3_000
                7, 22, 14_900
                8, 23, 13_900
                9, 24, 13_500
                10, 25, 18_800
                10, 27, 13_500
                10, 29, 17_000
                11, 24, 17_300
                11, 26, 15_100
                11, 28, 19_400
                11, 30, 3_400
                12, 23, 18_400
                12, 25, 18_900
                12, 27, 13_500
                12, 29, 3_800
                13, 24, 3_200
                13, 26, 3_500
                13, 28, 4_500
                13, 30, 4_000
                14, 23, 3_100
                14, 25, 2_800
                14, 27, 3_600
                14, 29, 3_700
                14, 31, 3_800
                15, 22, 4_100
                15, 28, 4_400
                15, 30, 2_900
                15, 32, 3_700
                16, 21, 3_700
                16, 29, 2_700
                16, 31, 3_000
                16, 33, 3_000
                16, 35, 3_100
                16, 37, 2_500
                16, 38, 16_500
                16, 39, 17_800
                16, 41, 3_000
                16, 43, 2_900
                16, 44, 2_900
                17, 20, 3_400
                17, 30, 2_700
                17, 32, 3_400
                17, 34, 2_700
                17, 36, 4_400
                17, 38, 2_900
                17, 39, 14_900
                17, 40, 12_000
                17, 42, 2_900
                17, 45, 3_800
                18, 29, 3_100
                18, 31, 3_500
                18, 35, 2_500
                18, 37, 3_000
                18, 39, 3_400
                18, 41, 4_000
                18, 46, 4_300
                19, 30, 2_900
                19, 34, 3_700
                19, 36, 4_700
                19, 38, 3_500
                19, 40, 3_400
                19, 47, 3_900
                20, 31, 4_400
                20, 33, 3_900
                20, 35, 3_400
                20, 37, 2_800
                20, 39, 3_500
                20, 48, 4_500
                21, 32, 3_000
                21, 34, 4_700
                21, 36, 3_500
                21, 38, 2_300
                22, 33, 3_000
                22, 35, 3_500
                22, 37, 3_000
                23, 34, 2_900
                23, 36, 3_400
                24, 35, 3_600
                25, 34, 4_400
                26, 33, 2_400
                27, 32, 4_600
                28, 31, 20_500
                28, 33, 12_500
                29, 32, 20_500
                29, 34, 15_000
                30, 31, 21_000
                30, 33, 16_000
                31, 32, 16_000
                32, 31, 21_000
                33, 30, 22_500
                33, 32, 18_810
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_935_955
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
                        16, 26
                        16, 32
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
                { structureOf "container" None 126_100 250_000 { X = 6; Y = 29 } with
                    Energy = 96
                }
                { structureOf "container" None 165_300 250_000 { X = 11; Y = 18 } with
                    Energy = 312
                }
                { structureOf "container" None 159_500 250_000 { X = 23; Y = 29 } with
                    Energy = 52
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 35 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 34 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 986_105
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 986_065
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 986_024
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 986_064
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 986_066
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 986_020
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 986_041
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 986_089
                }
                { structureOf "rampart" (Some Side.Ours) 99_601 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 986_082
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 28_321
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 3_100
                5, 4, 4_500
                5, 6, 3_100
                5, 44, 4_900
                6, 3, 3_500
                6, 7, 5_000
                6, 29, 4_000
                6, 43, 4_500
                6, 45, 2_800
                7, 2, 2_500
                7, 8, 3_100
                7, 28, 2_800
                7, 42, 4_300
                7, 46, 4_700
                8, 1, 3_300
                8, 9, 3_900
                8, 28, 4_300
                8, 41, 4_600
                8, 47, 3_900
                9, 10, 2_400
                9, 27, 5_000
                9, 28, 2_800
                9, 40, 4_300
                9, 48, 2_900
                10, 11, 4_200
                10, 26, 2_800
                10, 29, 5_000
                10, 39, 4_600
                11, 12, 3_700
                11, 18, 3_500
                11, 27, 4_000
                11, 30, 2_600
                11, 38, 2_500
                12, 13, 4_600
                12, 17, 2_400
                12, 25, 13_000
                12, 27, 17_900
                12, 28, 2_600
                12, 29, 2_500
                12, 31, 4_000
                12, 33, 20_700
                12, 35, 14_900
                12, 37, 4_500
                13, 14, 4_700
                13, 16, 4_800
                13, 26, 13_700
                13, 28, 3_900
                13, 30, 2_900
                13, 32, 3_500
                13, 34, 4_600
                13, 36, 4_800
                14, 15, 3_400
                14, 25, 15_100
                14, 27, 3_800
                14, 29, 4_500
                14, 31, 4_900
                14, 33, 3_100
                14, 35, 4_900
                15, 15, 3_100
                15, 26, 3_500
                15, 28, 4_800
                15, 30, 3_500
                15, 32, 3_400
                15, 34, 4_700
                16, 16, 3_100
                16, 25, 18_500
                16, 27, 3_800
                16, 29, 3_800
                16, 31, 4_900
                16, 33, 4_400
                16, 35, 4_000
                17, 17, 4_100
                17, 26, 19_800
                17, 28, 4_000
                17, 30, 3_300
                17, 32, 2_900
                17, 34, 3_400
                18, 18, 2_600
                18, 25, 20_100
                18, 27, 18_400
                18, 29, 2_500
                18, 31, 4_700
                18, 33, 5_000
                19, 19, 16_100
                19, 22, 18_000
                19, 24, 14_400
                19, 26, 13_900
                19, 28, 4_000
                19, 30, 4_800
                19, 32, 3_100
                20, 20, 16_400
                20, 21, 20_000
                20, 23, 19_700
                20, 25, 19_300
                20, 27, 16_100
                20, 29, 3_500
                20, 31, 4_000
                20, 33, 17_100
                21, 21, 4_900
                21, 22, 3_200
                21, 23, 3_200
                21, 24, 17_000
                21, 25, 20_600
                21, 26, 12_800
                21, 28, 20_000
                21, 30, 2_700
                21, 32, 20_500
                21, 34, 17_200
                22, 24, 2_600
                22, 25, 12_700
                22, 27, 18_800
                22, 28, 2_600
                22, 29, 4_600
                22, 30, 17_000
                22, 31, 21_300
                22, 32, 12_500
                22, 33, 12_700
                22, 35, 4_800
                23, 26, 15_200
                23, 28, 21_000
                23, 29, 12_600
                23, 30, 13_900
                23, 31, 14_500
                23, 32, 12_300
                23, 33, 16_200
                23, 34, 2_800
                24, 25, 13_000
                24, 28, 13_400
                24, 29, 14_500
                24, 30, 12_900
                24, 33, 17_500
                25, 24, 2_800
                25, 28, 13_600
                25, 29, 13_700
                25, 34, 14_800
                26, 23, 15_400
                27, 22, 12_600
                28, 21, 12_300
                28, 34, 13_600
                29, 20, 13_600
                30, 19, 12_000
                31, 18, 2_000
                32, 17, 2_500
                33, 16, 3_800
                34, 15, 4_400
                35, 14, 2_600
                36, 13, 16_500
                37, 12, 2_700
                38, 11, 4_100
                39, 12, 2_100
                40, 13, 13_800
                41, 14, 12_100
                42, 15, 3_600
                43, 16, 3_600
                44, 17, 2_000
                45, 18, 2_000
                46, 19, 2_000
                47, 20, 3_800
                48, 21, 4_400
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Obsidian522: live 23 CARRY, 22 MOVE; walked north out over
            // (20,0) and bounced on the W17S25 border under its tower.
            { body
                  "Obsidian522"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W17S26" 21 2)
                  (Some(
                      Waypoints
                          [
                              at "W17S26" 21 1
                              at "W17S26" 20 0
                              at "W17S25" 20 48
                              at "W17S25" 21 49
                          ]
                  )) with
                Hits = 4_420
                TicksToLive = 196
            }
            // HalfLife847: live, down the road a step a tick, swinging at
            // the ranger, then off toward the core: its live tiles to
            // t986,030.
            { body
                  "HalfLife847"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 20 1)
                  (Some(
                      Waypoints
                          [
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
                                  ] -> at "W17S26" x y
                          ]
                  )) with
                TicksToLive = 1_446
            }
            { body
                  "ranger-984393-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 25 7)
                  None with
                TicksToLive = 12
            }
            { body
                  "reserver-985414-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 19
            }
            { body
                  "ranger-985441-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S26" 25 10)
                  None with
                TicksToLive = 952
            }
            { body
                  "anchor-985849-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 1_372
                Energy = 14
            }
            { body
                  "reserver-985957-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 27 8)
                  None with
                TicksToLive = 562
            }
            { body
                  "worker-984557-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 110
                Energy = 320
            }
            { body
                  "anchor-984621-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 138
                Energy = 50
            }
            { body
                  "hauler-984663-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 47)
                  None with
                TicksToLive = 228
                Energy = 250
            }
            { body
                  "ranger-984683-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 302
            }
            { body
                  "guard-985115-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 28 47)
                  None with
                TicksToLive = 638
            }
            { body
                  "upgrader-985149-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 693
                Energy = 42
            }
            { body
                  "hauler-985200-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 8 23)
                  None with
                TicksToLive = 765
            }
            { body
                  "upgrader-985272-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 816
                Energy = 42
            }
            { body
                  "guard-985559-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 1_082
            }
            { body
                  "ranger-985616-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 17 36)
                  None with
                TicksToLive = 1_235
            }
            { body
                  "guard-984525-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 6 1)
                  None with
                TicksToLive = 48
            }
            { body
                  "hauler-984785-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 25)
                  None with
                TicksToLive = 422
            }
            { body
                  "upgrader-984881-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 521
                Energy = 26
            }
            { body
                  "hauler-985259-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 896
                Energy = 1_164
            }
            { body
                  "hauler-985401-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 33)
                  None with
                TicksToLive = 1_038
                Energy = 470
            }
            { body
                  "brawler-985442-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_085
            }
            { body
                  "hauler-985545-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 41)
                  None with
                TicksToLive = 1_182
                Energy = 58
            }
            { body
                  "anchor-985592-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_109
                Energy = 50
            }
            { body
                  "anchor-985719-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 1_242
                Energy = 50
            }
            { body
                  "worker-985819-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 5 44)
                  None with
                TicksToLive = 1_462
            }
            { body
                  "reserver-985969-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S28" 45 18)
                  None with
                TicksToLive = 568
            }
        ]

    let a = arena 986_006 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "reserver-985957-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "hauler-984663-Spawn11", "flee"
                            "ranger-984683-Spawn3", "guard:W17S25"
                            "upgrader-985149-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-985200-Spawn11", "withdraw:container-27-9"
                            "upgrader-985272-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "ranger-985616-Spawn3", "guard:W17S25"
                            "hauler-984785-Spawn8", "withdraw:6ac0e2f1e169cce45673df61"
                            "upgrader-984881-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-985259-Spawn3", "refill:storage-17-29"
                            "hauler-985401-Spawn8", "refill:spawn-Spawn3"
                            "hauler-985545-Spawn8", "withdraw:6ab55d5a936dc16f644ebc58"
                            "anchor-985592-Spawn3", "harvest:6a8caa95dd4872bccd319014"
                            "anchor-985719-Spawn8", "harvest:6a8caa95dd4872bccd319013"
                            "worker-985819-Spawn3", "withdraw:6ab55d5a936dc16f644ebc58"
                            "reserver-985969-Spawn3", "reserve:6a8caaa1dd4872bccd3191fa"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 985_452 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 985_452
                            }
                        ]
            }
    }

// ---- arena capture slow-melee-haulers (#527, movement S0) ------------------
// `npm run capture -- W18S25 971045 --rooms W17S25,W15S28 --name slowMeleeHaulers`, read at live t986,316.
// The room-history replay at t971,045, the state after that tick ran: arena
// tick 0 runs as t971,046.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/971000.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/971000.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/971000.json
// Read too late for live Memory: the Raid log, ArmedSeen, Healers, ExitWatches
// and Sightings start empty, and every body of ours starts idle. The 264
// roads are laid (#560); W18S25's reservation and 2 tombstones are not
// placed, nor the hauler Spawn3 was casting.

/// W18S25 at t971,045 (#527's "Why"): Trepidimous' Virtual458 at (39,14),
/// 14 live ATTACK and 1 live MOVE, steps once every ~9 ticks toward the
/// south-west; W17S25's loaded haulers hauler-969730-Spawn11 (36,13) and
/// hauler-970335-Spawn11 (36,17), walking home east past it, stand at its
/// Reach's edge and step in and out of it (live t971,030-971,060).
let internal slowMeleeHaulers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let kazkel = Side.Player "Kazkel"

    let w18s25 =
        room "W18S25"
        |> withStructures [ structureOf "container" None 120_000 250_000 { X = 31; Y = 5 } ]
        |> withRoads
            [
                28, 9, 3_100
                29, 8, 3_300
                29, 10, 3_000
                30, 7, 3_300
                30, 11, 3_300
                31, 6, 3_500
                31, 12, 2_900
                32, 13, 3_000
                33, 14, 2_600
                34, 15, 3_400
                35, 16, 2_600
                36, 17, 2_600
                37, 16, 2_600
                38, 15, 2_600
                39, 14, 2_500
                40, 13, 2_300
                41, 12, 2_500
                42, 13, 2_300
                43, 13, 2_000
                44, 13, 2_000
                45, 13, 2_200
                46, 14, 2_100
                47, 15, 2_100
                48, 16, 2_200
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_973 69_540
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 850
        |> withStructures
            [
                for x, y in
                    [ 11, 25; 11, 27; 12, 28; 14, 26; 14, 30; 15, 29; 16, 28; 16, 30; 17, 31 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 71_200 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 108
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 27 }
                { structureOf "rampart" (Some Side.Ours) 97_321 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 971_145
                }
                { structureOf "rampart" (Some Side.Ours) 97_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 971_126
                }
                { structureOf "rampart" (Some Side.Ours) 93_021 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 971_129
                }
                { structureOf "rampart" (Some Side.Ours) 99_931 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 971_134
                }
                { structureOf "rampart" (Some Side.Ours) 99_871 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 971_106
                }
                { structureOf "rampart" (Some Side.Ours) 94_631 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 971_108
                }
                { structureOf "rampart" (Some Side.Ours) 95_921 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 971_133
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 971_095
                }
                { structureOf "rampart" (Some Side.Ours) 96_701 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 971_112
                }
                { structureOf "rampart" (Some Side.Ours) 97_461 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 971_127
                }
                { structureOf "rampart" (Some Side.Ours) 97_961 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 971_076
                }
                { structureOf "rampart" (Some Side.Ours) 15_401 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 971_111
                }
                { structureOf "rampart" (Some Side.Ours) 5_701 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 971_087
                }
                { structureOf "rampart" (Some Side.Ours) 99_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 971_075
                }
                { structureOf "rampart" (Some Side.Ours) 39_201 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 971_132
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 971_077
                }
                { structureOf "rampart" (Some Side.Ours) 92_101 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 971_073
                }
                { structureOf "rampart" (Some Side.Ours) 98_071 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 971_086
                }
                { structureOf "rampart" (Some Side.Ours) 96_681 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 971_104
                }
                towerOf Side.Ours { X = 13; Y = 29 } 980
            ]
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 32 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 33 }
        |> withSite Side.Ours "road" None 156 { X = 23; Y = 34 }
        |> withSite Side.Ours "road" None 182 { X = 22; Y = 35 }
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 753 { X = 13; Y = 31 }
        |> withSite Side.Ours "extension" None 2_742 { X = 15; Y = 31 }
        |> withSite Side.Ours "extension" None 2_346 { X = 10; Y = 24 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 30 }
        |> withSite Side.Ours "extension" None 1_765 { X = 12; Y = 24 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 29 }
        |> withSite Side.Ours "road" None 0 { X = 11; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 27 }
        |> withSite Side.Ours "extension" None 107 { X = 17; Y = 29 }
        |> withSite Side.Ours "extension" None 1_800 { X = 13; Y = 25 }
        |> withSite Side.Ours "road" None 271 { X = 18; Y = 29 }
        |> withRoads
            [
                2, 17, 3_500
                3, 18, 3_500
                4, 19, 3_100
                5, 20, 3_100
                6, 21, 2_800
                7, 22, 14_000
                8, 23, 14_000
                9, 24, 13_000
                10, 25, 12_000
                11, 24, 18_000
                11, 26, 10_300
                11, 28, 18_500
                12, 23, 18_500
                12, 25, 12_600
                12, 27, 13_000
                12, 29, 3_400
                13, 24, 2_500
                13, 26, 3_200
                13, 28, 3_800
                13, 30, 2_100
                14, 23, 2_600
                14, 25, 1_400
                14, 27, 3_700
                14, 29, 1_300
                14, 31, 1_900
                15, 22, 2_200
                15, 28, 1_300
                15, 30, 3_800
                15, 32, 2_000
                16, 21, 3_300
                16, 29, 1_600
                16, 31, 3_100
                16, 33, 1_600
                16, 35, 3_500
                16, 37, 4_900
                16, 38, 20_000
                16, 39, 9_000
                16, 41, 4_000
                16, 43, 2_700
                16, 44, 2_700
                17, 20, 1_900
                17, 30, 5_000
                17, 32, 3_300
                17, 34, 3_100
                17, 36, 5_000
                17, 38, 4_900
                17, 39, 8_500
                17, 40, 24_500
                17, 42, 2_900
                17, 45, 3_100
                18, 31, 3_600
                18, 35, 2_800
                18, 37, 5_000
                18, 39, 5_000
                18, 41, 2_600
                18, 46, 3_100
                19, 30, 5_000
                19, 34, 3_400
                19, 36, 3_900
                19, 38, 5_000
                19, 40, 2_600
                19, 47, 3_100
                20, 31, 3_900
                20, 33, 3_400
                20, 35, 5_000
                20, 37, 4_400
                20, 39, 2_100
                20, 48, 2_800
                21, 34, 3_900
                21, 36, 5_000
                21, 38, 3_800
                22, 37, 1_500
                23, 36, 1_400
                24, 35, 3_300
                25, 34, 4_000
                26, 33, 2_400
                27, 32, 1_300
                28, 33, 7_500
                29, 34, 8_500
                30, 33, 9_500
                31, 32, 9_500
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_710_197
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 31
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
                for x, y in [ 12, 30; 14, 26; 14, 30; 15, 27; 15, 29; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 12
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 360
                }
                structureOf "container" None 168_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 971_057
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 971_116
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 971_075
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 971_144
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 971_117
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 971_071
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 971_092
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 971_140
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 971_133
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 29_952
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 3_900
                5, 4, 4_300
                5, 6, 3_900
                5, 44, 4_700
                6, 3, 3_300
                6, 7, 4_800
                6, 29, 2_900
                6, 43, 4_300
                6, 45, 2_300
                7, 2, 3_200
                7, 8, 3_900
                7, 28, 2_500
                7, 42, 3_900
                7, 46, 2_800
                8, 1, 3_800
                8, 9, 3_700
                8, 28, 4_500
                8, 41, 4_300
                8, 47, 3_700
                9, 10, 4_800
                9, 27, 4_900
                9, 28, 4_700
                9, 40, 3_900
                9, 48, 4_800
                10, 11, 3_800
                10, 26, 2_700
                10, 29, 2_800
                10, 39, 4_300
                11, 12, 3_500
                11, 18, 3_400
                11, 27, 3_900
                11, 30, 4_600
                11, 38, 4_900
                12, 13, 2_800
                12, 17, 4_600
                12, 25, 21_000
                12, 27, 14_200
                12, 28, 2_700
                12, 29, 4_500
                12, 31, 3_400
                12, 33, 20_600
                12, 35, 17_900
                12, 37, 4_300
                13, 14, 2_900
                13, 16, 2_500
                13, 26, 14_200
                13, 28, 3_300
                13, 30, 2_600
                13, 32, 3_200
                13, 34, 3_000
                13, 36, 4_600
                14, 15, 4_500
                14, 25, 17_500
                14, 27, 4_300
                14, 29, 4_000
                14, 31, 4_800
                14, 33, 3_000
                14, 35, 4_600
                15, 15, 4_700
                15, 26, 3_900
                15, 28, 3_900
                15, 30, 4_300
                15, 32, 3_800
                15, 34, 3_100
                16, 16, 4_700
                16, 25, 14_400
                16, 27, 4_400
                16, 29, 4_400
                16, 31, 2_500
                16, 33, 4_900
                16, 35, 3_400
                17, 17, 2_500
                17, 26, 19_200
                17, 28, 3_300
                17, 30, 3_600
                17, 32, 3_000
                17, 34, 3_300
                18, 18, 3_700
                18, 25, 18_500
                18, 27, 16_700
                18, 29, 2_800
                18, 31, 5_000
                18, 33, 4_700
                19, 19, 18_400
                19, 22, 14_800
                19, 24, 20_400
                19, 26, 15_800
                19, 28, 2_900
                19, 30, 4_900
                19, 32, 4_100
                20, 20, 19_200
                20, 21, 17_800
                20, 23, 16_500
                20, 25, 17_100
                20, 27, 17_000
                20, 29, 4_600
                20, 31, 4_700
                20, 33, 17_500
                21, 21, 3_800
                21, 22, 2_600
                21, 23, 5_000
                21, 24, 20_400
                21, 25, 18_900
                21, 26, 21_300
                21, 28, 19_400
                21, 30, 2_500
                21, 32, 18_800
                21, 34, 17_100
                22, 24, 4_400
                22, 25, 13_600
                22, 27, 13_000
                22, 28, 5_000
                22, 29, 4_100
                22, 30, 16_400
                22, 31, 19_700
                22, 32, 20_500
                22, 33, 20_700
                22, 35, 3_800
                23, 26, 17_600
                23, 28, 18_200
                23, 29, 20_600
                23, 30, 21_400
                23, 31, 14_000
                23, 32, 20_300
                23, 33, 15_700
                23, 34, 4_300
                24, 25, 14_900
                24, 28, 21_400
                24, 29, 22_000
                24, 30, 20_400
                24, 33, 15_900
                25, 24, 2_500
                25, 28, 21_600
                25, 29, 21_200
                25, 34, 12_700
                26, 23, 16_800
                27, 22, 14_000
                28, 21, 13_700
                28, 34, 21_100
                29, 20, 15_500
                30, 19, 13_900
                31, 18, 4_500
                32, 17, 5_000
                33, 16, 3_600
                34, 15, 2_600
                35, 14, 3_500
                36, 13, 19_400
                37, 12, 2_400
                38, 11, 3_700
                39, 12, 4_400
                40, 13, 13_600
                41, 14, 13_500
                42, 15, 3_100
                43, 16, 3_100
                44, 17, 2_600
                45, 18, 2_600
                46, 19, 4_200
                47, 20, 4_400
                48, 21, 3_400
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Virtual458: live 14 ATTACK, 1 MOVE; a road step each time its
            // fatigue runs out, its live tiles to t971,136.
            { body
                  "Virtual458"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 39 14)
                  (Some(
                      Waypoints
                          [
                              for x, y in
                                  [
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
                                  ] -> at "W18S25" x y
                          ]
                  )) with
                Hits = 1_454
                TicksToLive = 1_016
            }
            { body
                  "hauler-969730-Spawn11"
                  Side.Ours
                  (parts [ Carry, 10; Move, 5 ])
                  (at "W18S25" 36 13)
                  None with
                TicksToLive = 228
                Energy = 457
            }
            { body
                  "hauler-970335-Spawn11"
                  Side.Ours
                  (parts [ Carry, 10; Move, 5 ])
                  (at "W18S25" 36 17)
                  None with
                TicksToLive = 833
                Energy = 500
            }
            { body
                  "reserver-970973-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 532
            }
            { body
                  "anchor-970998-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 46 14)
                  None with
                TicksToLive = 1_481
                Fatigue = 1
                Energy = 15
            }
            // scout-W17S18-969668: live 1 MOVE, no act.
            { body "scout-W17S18-969668" kazkel (parts [ Move, 1 ]) (at "W18S25" 25 1) (Some Hold) with
                Hits = 80
                TicksToLive = 124
            }
            { body
                  "worker-969637-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 15 32)
                  None with
                TicksToLive = 126
                Energy = 148
            }
            { body
                  "ranger-969847-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 36)
                  None with
                TicksToLive = 426
            }
            { body
                  "upgrader-970047-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 1; Move, 5 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 533
            }
            { body
                  "ranger-970126-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 705
            }
            { body
                  "hauler-970276-Spawn11"
                  Side.Ours
                  (parts [ Carry, 10; Move, 5 ])
                  (at "W17S25" 16 43)
                  None with
                TicksToLive = 774
            }
            { body
                  "worker-970391-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 880
            }
            { body
                  "worker-970447-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 13 22)
                  None with
                TicksToLive = 936
                Energy = 32
            }
            { body
                  "worker-970795-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 5 ])
                  (at "W17S25" 13 26)
                  None with
                TicksToLive = 1_287
                Energy = 64
            }
            { body
                  "defender-970852-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; Attack, 6 ])
                  (at "W17S25" 20 31)
                  None with
                TicksToLive = 1_341
            }
            { body
                  "guard-970892-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 13 24)
                  None with
                TicksToLive = 1_375
            }
            { body
                  "anchor-970943-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_426
                Energy = 50
            }
            { body
                  "worker-970483-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 23 36)
                  None with
                TicksToLive = 972
                Energy = 188
            }
            { body
                  "anchor-969618-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 95
                Energy = 50
            }
            { body
                  "worker-969759-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 7 28)
                  None with
                TicksToLive = 362
                Energy = 638
            }
            { body
                  "hauler-969982-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 32)
                  None with
                TicksToLive = 579
                Energy = 890
            }
            { body
                  "hauler-970270-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 5 5)
                  None with
                TicksToLive = 867
            }
            { body
                  "anchor-970434-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 911
                Energy = 50
            }
            { body
                  "upgrader-970821-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 14 37)
                  None with
                TicksToLive = 1_421
            }
            { body
                  "worker-970069-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 672
                Energy = 650
            }
            { body
                  "reserver-971002-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W15S28" 6 7)
                  None with
                TicksToLive = 567
            }
            { body
                  "hauler-969423-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 27)
                  None with
                TicksToLive = 20
                Energy = 1_464
            }
        ]

    arena 971_046 [ w18s25; w17s25; w15s28 ] colonies bodies
