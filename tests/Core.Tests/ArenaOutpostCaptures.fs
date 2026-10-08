/// Live captures of raids on our outposts, as `npm run capture` wrote them:
/// arena worlds `ArenaBorderTests` and `ArenaLaneTests` replay.
module Fabot.Core.Tests.ArenaOutpostCaptures

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

// ---- arena capture two-raids-beside-w17s25 (#532) ---------------------------
// `npm run capture -- W17S25 972617 --rooms W18S25,W17S26,W15S28 --name two-raids-beside-w17s25`.
// The room-history replay at t972,617, the state after that tick ran: arena
// tick 0 runs as t972,618.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/972600.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/972600.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/972600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/972600.json
// Left out, as the capture says: Healers, ExitWatches and Sightings start
// empty, and the Raid log too: its live ring opened no stand-down after
// t970,910, and its threat and fight latches for the tick were since
// overwritten. W18S25's reservation, roads, drops and ruins are not placed.
// Assignments off the Transition log, read t972,742.

/// W17S25 at t972,617 (#531's "Why"): Crunchy047, 14 live ATTACK on one live
/// MOVE, in the west outpost W18S25; Wraith403, its RANGED_ATTACK shot off and
/// healing itself, in the south outpost W17S26 beside our melee guard; two
/// resident rangers by the controller, cast by the mother W15S28. Their
/// bodies stand still (`Hold`).
let internal twoRaidsBesideW17S25 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_961 70_919
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
                { structureOf "container" None 76_200 250_000 { X = 16; Y = 34 } with
                    Energy = 45
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 288
                }
                { structureOf "rampart" (Some Side.Ours) 92_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 972_630
                }
                { structureOf "rampart" (Some Side.Ours) 92_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 972_710
                }
                { structureOf "rampart" (Some Side.Ours) 94_601 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 972_713
                }
                { structureOf "rampart" (Some Side.Ours) 95_431 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 972_619
                }
                { structureOf "rampart" (Some Side.Ours) 95_071 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 972_690
                }
                { structureOf "rampart" (Some Side.Ours) 94_831 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 972_692
                }
                { structureOf "rampart" (Some Side.Ours) 91_121 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 972_717
                }
                { structureOf "rampart" (Some Side.Ours) 95_301 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 972_679
                }
                { structureOf "rampart" (Some Side.Ours) 91_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 972_696
                }
                { structureOf "rampart" (Some Side.Ours) 92_661 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 972_711
                }
                { structureOf "rampart" (Some Side.Ours) 93_161 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 972_660
                }
                { structureOf "rampart" (Some Side.Ours) 10_601 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 972_695
                }
                { structureOf "rampart" (Some Side.Ours) 901 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 972_671
                }
                { structureOf "rampart" (Some Side.Ours) 94_521 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 972_659
                }
                { structureOf "rampart" (Some Side.Ours) 34_401 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 972_716
                }
                { structureOf "rampart" (Some Side.Ours) 94_501 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 972_661
                }
                { structureOf "rampart" (Some Side.Ours) 98_501 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 972_657
                }
                { structureOf "rampart" (Some Side.Ours) 93_271 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 972_670
                }
                { structureOf "rampart" (Some Side.Ours) 91_881 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 972_688
                }
                towerOf Side.Ours { X = 13; Y = 29 } 840
            ]
        |> withSite Side.Ours "storage" None 2_670 { X = 13; Y = 27 }

    let w18s25 =
        room "W18S25"
        |> withStructures [ structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 510
                }
            ]

    // The mother: the residents are Spawn3's, so hers and not the child's.
    // Without her the child adopts them and lends them at once.
    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_739_967
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
                    Energy = 324
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 624
                }
                structureOf "container" None 153_100 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 19; Y = 31 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 972_641
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 972_700
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 972_659
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 972_626
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 972_701
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 972_655
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 972_676
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 972_625
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 972_717
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 31_076
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "ranger-971130-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 137
            }
            { body
                  "ranger-971410-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 417
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 935
                Energy = 184
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 18 37)
                  None with
                TicksToLive = 1_313
                Energy = 15
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_369
                Energy = 50
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 42)
                  None with
                TicksToLive = 1_469
            }
            { body
                  "reserver-972604-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 597
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 881
                Energy = 350
            }
            // live: 14 ATTACK, 1 MOVE
            { body
                  "Crunchy047"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 40 13)
                  (Some Hold) with
                Hits = 1_454
                TicksToLive = 1_033
                Fatigue = 15
            }
            { body
                  "worker-971252-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 6; Move, 5 ])
                  (at "W18S25" 38 14)
                  None with
                TicksToLive = 178
                Energy = 300
            }
            { body
                  "hauler-971381-Spawn11"
                  Side.Ours
                  (parts [ Carry, 12; Move, 6 ])
                  (at "W18S25" 38 15)
                  None with
                TicksToLive = 316
                Energy = 364
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W18S25" 38 10)
                  None with
                TicksToLive = 699
                Energy = 700
            }
            { body
                  "reserver-972084-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 71
            }
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 28 9)
                  None with
                TicksToLive = 1_399
            }
            { body
                  "Synapse250"
                  trepidimous
                  (List.replicate 23 [ Move; Carry ] |> List.concat)
                  (at "W17S26" 26 9)
                  (Some Hold) with
                TicksToLive = 1_269
            }
            // live: 1 HEAL, 1 MOVE
            { body
                  "Wraith403"
                  trepidimous
                  (parts [ Move, 1; RangedAttack, 1; Move, 1; Heal, 1; Move, 1 ])
                  (at "W17S26" 13 19)
                  (Some Hold) with
                Hits = 188
                TicksToLive = 1_299
                Fatigue = 12
            }
            // live: 2 MOVE, 5 WORK
            { body
                  "Torpedo487"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 27 9)
                  (Some Hold) with
                Hits = 620
                TicksToLive = 1_392
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 24 5)
                  None with
                TicksToLive = 965
                Energy = 50
            }
            // live: 1 TOUGH, 5 MOVE, 3 ATTACK, 1 HEAL
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 12 19)
                  None with
                Hits = 970
                TicksToLive = 1_439
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S26" 1 15)
                  None with
                TicksToLive = 762
            }
            { body
                  "worker-971258-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 289
                Energy = 534
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 809
                Energy = 50
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_345
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 1_425
            }
            { body
                  "ranger-972464-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W15S28" 6 7)
                  None with
                TicksToLive = 1_471
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 12 17)
                  None with
                TicksToLive = 1_449
                Fatigue = 2
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 4 5)
                  None with
                TicksToLive = 1_281
                Energy = 807
            }
        ]

    let a = arena 972_618 [ w17s25; w18s25; w17s26; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t972,617; ranger-971130's off its timeline read at t972,742
                // (matched guard:W17S25 at t972,500).
                Assignments =
                    Map.ofList
                        [
                            "ranger-971130-Spawn3", "guard:W17S25"
                            "ranger-971410-Spawn3", "guard:W17S25"
                            "worker-972000-Spawn11", "repair:rampart-2-20"
                            "worker-972372-Spawn11", "withdraw:container-27-9"
                            "anchor-972458-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "anchor-972558-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "hauler-971755-Spawn11", "refill:spawn-Spawn11"
                            "anchor-972488-Spawn11", "harvest:6a8caa71dd4872bccd318b28"
                            "guard-972528-Spawn11", "guard:W17S26"
                        ]
            }
    }

// ---- arena capture shut-outpost-cripple (#534, the world layer's S0) -------
// `npm run capture -- W18S25 973320 --rooms W17S25,W15S28,W17S26 --name shutOutpostCripple`.
// The room-history replay at t973,320, the state after that tick ran: arena
// tick 0 runs as t973,321.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/973300.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/973300.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/973300.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/973300.json
// Read at live t974,751, too late for live Memory: W17S25's stand-down of
// W18S25 is seated by hand off #534's "Why" (opened t973,265 on a Lose while
// Shade461 healed Idol768, held to t973,465). Healers, ExitWatches and
// Sightings start empty. W18S25's and W17S26's reservations, roads and
// tombstones are not placed; spawning bodies (a defender and a ranger due
// t973,355, a hauler t973,409) are not placed. The Transition log held no
// verdict that far back: the resident's and W17S26's guard's Tasks are
// seated off the t972,617 capture, every other body starts idle.

/// W18S25 at t973,320 (#534's "Why"): the healer gone, Idol768 stands alone
/// at 818 hits with 8 live ATTACK and 1 live MOVE in the child W17S25's
/// outpost, which W17S25's stand-down holds shut; the mother W15S28's
/// resident ranger-972663-Spawn3 stands in W17S25 next door.
let internal shutOutpostCripple () : Arena =
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
        |> withControllerClocks 0 0 38_258 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in [ 10, 24; 12, 30; 13, 31; 14, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        13, 25
                        14, 26
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
                { structureOf "container" None 66_200 250_000 { X = 16; Y = 34 } with
                    Energy = 100
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 348
                }
                { structureOf "rampart" (Some Side.Ours) 90_721 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 973_323
                }
                { structureOf "rampart" (Some Side.Ours) 90_661 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 973_403
                }
                { structureOf "rampart" (Some Side.Ours) 99_711 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 973_406
                }
                { structureOf "rampart" (Some Side.Ours) 93_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 973_411
                }
                { structureOf "rampart" (Some Side.Ours) 92_971 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 973_383
                }
                { structureOf "rampart" (Some Side.Ours) 98_231 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 973_385
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 973_410
                }
                { structureOf "rampart" (Some Side.Ours) 93_201 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 973_372
                }
                { structureOf "rampart" (Some Side.Ours) 89_801 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 973_389
                }
                { structureOf "rampart" (Some Side.Ours) 90_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 973_404
                }
                { structureOf "rampart" (Some Side.Ours) 91_061 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 973_353
                }
                { structureOf "rampart" (Some Side.Ours) 8_501 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 973_388
                }
                { structureOf "rampart" (Some Side.Ours) 1_201 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 973_364
                }
                { structureOf "rampart" (Some Side.Ours) 92_421 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 32_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 973_409
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
                { structureOf "rampart" (Some Side.Ours) 91_781 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 973_381
                }
                towerOf Side.Ours { X = 13; Y = 29 } 940
            ]
        |> withSite Side.Ours "storage" None 13_863 { X = 13; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_753_992
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
                    Energy = 792
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 756
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 973_334
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 973_393
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 973_384
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 973_394
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 973_348
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 973_369
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 973_417
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 973_410
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 28_361
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 346
                }
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 480
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Idol768: live: 8 ATTACK, 1 MOVE; it wanders W18S25 until a
            // resident kills it after t973,430, so it holds here.
            { body
                  "Idol768"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 44 13)
                  (Some Hold) with
                Hits = 818
                TicksToLive = 1_051
                Fatigue = 13
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 18 30)
                  None with
                TicksToLive = 59
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 29 34)
                  None with
                TicksToLive = 178
                Energy = 325
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 29 33)
                  None with
                TicksToLive = 232
                Fatigue = 86
                Energy = 285
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 16 42)
                  None with
                TicksToLive = 262
                Energy = 50
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 24 35)
                  None with
                TicksToLive = 610
                Energy = 314
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 666
                Energy = 50
            }
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 736
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 967
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 9 24)
                  None with
                TicksToLive = 1_111
                Energy = 552
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_435
            }
            // scout-W17S22-972936: live: no act
            { body "scout-W17S22-972936" odiodin (parts [ Move, 1 ]) (at "W17S25" 13 24) (Some Hold) with
                TicksToLive = 1_117
            }
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 3 18)
                  None with
                TicksToLive = 696
                Energy = 50
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 29)
                  None with
                TicksToLive = 86
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 106
                Energy = 50
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 578
                Energy = 594
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 642
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 37 16)
                  None with
                TicksToLive = 722
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 746
                Energy = 50
            }
            { body
                  "hauler-972938-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 28)
                  None with
                TicksToLive = 1_260
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 766
                Energy = 50
            }
            { body
                  "reserver-973148-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 438
            }
        ]

    let a = arena 973_321 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    { a with
        // W13S28's observer, which the arena does not hold, on W18S25 as live.
        Lookout = set [ "W18S25" ]
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-972663-Spawn3", "guard:W17S25"
                            "guard-972528-Spawn11", "guard:W17S26"
                        ]
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

// ---- arena capture w15s27-t974800 (#536) ------------------------------------
// `npm run capture -- W15S27 974800 --rooms W15S28`, read at live t975,006.
// The room-history replay at t974,800, the state after that tick ran: arena
// tick 0 runs as t974,801.
//   W15S27  https://screeps.com/room-history/shardSeason/W15S27/974800.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/974800.json
// Healers, ExitWatches and Sightings start empty; the Raid log, ArmedSeen and
// Towered are live Memory's at t975,006, less what postdates the tick.
// W15S27's reservation, 178 roads and a tombstone are not placed; spawning
// bodies (a hauler due t974,875, a ranger t974,888) are not placed.

/// W15S27 at t974,800 (#536's "Why"): invader_W15S27_250, 10 hits left and
/// one live MOVE, stands rooted on (6,36), the first diagonal step of
/// anchor-974661-Spawn3's line from (5,37) to its seat on the container at
/// (13,29). Live, the anchor stepped into it for ~230 ticks.
let internal rootedInvaderOnAnchorStep () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w15s27 =
        room "W15S27"
        |> withStructures [ structureOf "container" None 130_800 250_000 { X = 13; Y = 29 } ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_779_864
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 30
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 26
                        14, 28
                        14, 30
                        14, 32
                        14, 34
                        15, 25
                        15, 27
                        15, 29
                        15, 31
                        15, 33
                        15, 35
                        16, 26
                        16, 30
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
                        20, 26
                        20, 28
                        20, 30
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
                for x, y in [ 18, 34; 19, 33; 20, 32; 21, 31; 21, 33; 21, 35; 23, 35 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 492
                }
                { structureOf "container" None 195_100 250_000 { X = 11; Y = 18 } with
                    Energy = 864
                }
                structureOf "container" None 133_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 974_819
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 974_878
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 974_837
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 974_814
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 974_879
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 974_833
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 974_854
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 974_803
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 974_895
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 29_322
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 701
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W15S28" ])

    let bodies: Body list =
        [
            // Live: 1 MOVE; it swung at the anchor on (5,37), which the arena's
            // hits-ordered parts leave it no ATTACK to do. The invader AI
            // walks it no nearer than 3 to a hostile without one (#541).
            { body
                  "invader_W15S27_250"
                  (Side.Npc "Invader")
                  (parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ])
                  (at "W15S27" 6 36)
                  (Some Invader) with
                Hits = 10
                TicksToLive = 1_287
            }
            { body
                  "reserver-974641-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S27" 5 9)
                  None with
                TicksToLive = 445
            }
            { body
                  "anchor-974661-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S27" 5 37)
                  None with
                TicksToLive = 1_389
            }
            { body
                  "hauler-973265-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 33 16)
                  None with
                TicksToLive = 107
            }
            { body
                  "anchor-973374-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 96
                Energy = 50
            }
            { body
                  "anchor-973647-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 5 1)
                  None with
                TicksToLive = 375
                Energy = 50
            }
            { body
                  "worker-973746-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 23 28)
                  None with
                TicksToLive = 594
                Energy = 338
            }
            { body
                  "upgrader-973875-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 21 24)
                  None with
                TicksToLive = 720
            }
            { body
                  "hauler-973896-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 35)
                  None with
                TicksToLive = 738
                Energy = 640
            }
            { body
                  "anchor-974040-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 762
                Energy = 50
            }
            { body
                  "brawler-974217-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_065
            }
            { body
                  "hauler-974425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 1_267
            }
            { body
                  "worker-974514-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 15 34)
                  None with
                TicksToLive = 1_362
                Energy = 680
            }
            { body
                  "guard-974611-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 7 0)
                  None with
                TicksToLive = 1_339
            }
        ]

    let a = arena 974_801 [ w15s27; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t974,800,
                // read at live t975,006; it held none that early for
                // hauler-973265, anchor-973374 and brawler-974217.
                Assignments =
                    Map.ofList
                        [
                            "reserver-974641-Spawn3", "reserve:6a8caa95dd4872bccd319010"
                            "anchor-974661-Spawn3", "harvest:6a8caa95dd4872bccd319011"
                            "worker-973746-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-973896-Spawn3", "refill:spawn-Spawn3"
                            "anchor-974040-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-974425-Spawn3", "withdraw:container-11-18"
                            "worker-974514-Spawn8", "refill:spawn-Spawn3"
                            "guard-974611-Spawn3", "guard:W18S25"
                        ]
                ArmedSeen = Map.ofList [ "W15S27", 974_694 ]
                Towered = Set.ofList [ "W15S28" ]
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
                                            LastSeen = 974_800
                                            Expiry = 977_506
                                            Basis = StandDownBasis.Fallback
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture t976,955 (#542) --------------------------------------------
// `npm run capture -- W17S26 976955 --rooms W17S25,W15S28`, read at live t977,069.
// The room-history replay at t976,955, the state after that tick ran: arena
// tick 0 runs as t976,956 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/976900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/976900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/976900.json
// Specter836 Holds: live it dug on its seat. Healers and Sightings start empty
// (heap only); the ExitWatch is seeded below. The Raid log, ArmedSeen and
// Towered are live Memory's at t977,069, less what postdates the tick.
// W17S26 is reserved by us to t980242: the arena has no reservations.
// Spawn3 is spawning hauler-976850-Spawn3 until t976994: not placed.
// Spawn8 is spawning anchor-976950-Spawn8 until t976974: not placed.
// not placed: 257 road, 5 tombstone (the arena has no roads, drops or ruins).

/// Trepidimous' Specter836, a 5W3M digger, on W17S26's source beside our
/// anchor, while W17S26's north exit is still held for Paragon731, an armed
/// body seen leaving by it at t976,871, and nobody holds the Guard.
let internal diggerBesideHeldExit () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 1_140
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 40_000 90_767
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
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
                for x, y in [ 10, 28; 10, 30; 15, 27; 15, 31; 16, 28 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 91_600 250_000 { X = 16; Y = 34 } with
                    Energy = 1_984
                }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 408
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 29 } with
                    Energy = 26
                }
                { structureOf "rampart" (Some Side.Ours) 89_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 976_986
                }
                { structureOf "rampart" (Some Side.Ours) 99_261 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 976_967
                }
                { structureOf "rampart" (Some Side.Ours) 98_541 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 976_970
                }
                { structureOf "rampart" (Some Side.Ours) 99_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 976_975
                }
                { structureOf "rampart" (Some Side.Ours) 99_271 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 977_046
                }
                { structureOf "rampart" (Some Side.Ours) 97_331 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 977_048
                }
                { structureOf "rampart" (Some Side.Ours) 98_621 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 976_974
                }
                { structureOf "rampart" (Some Side.Ours) 92_301 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 977_035
                }
                { structureOf "rampart" (Some Side.Ours) 90_601 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 977_052
                }
                { structureOf "rampart" (Some Side.Ours) 90_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 976_968
                }
                { structureOf "rampart" (Some Side.Ours) 90_761 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 977_016
                }
                { structureOf "rampart" (Some Side.Ours) 71_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 976_967
                }
                { structureOf "rampart" (Some Side.Ours) 601 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 977_051
                }
                { structureOf "rampart" (Some Side.Ours) 22_501 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 977_027
                }
                { structureOf "rampart" (Some Side.Ours) 92_121 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 977_015
                }
                { structureOf "rampart" (Some Side.Ours) 21_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 976_973
                }
                { structureOf "rampart" (Some Side.Ours) 92_101 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 977_017
                }
                { structureOf "rampart" (Some Side.Ours) 95_501 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 977_013
                }
                { structureOf "rampart" (Some Side.Ours) 100_531 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 977_026
                }
                { structureOf "rampart" (Some Side.Ours) 99_481 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 977_044
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_820
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_818_322
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
                { structureOf "container" None 125_500 250_000 { X = 6; Y = 29 } with
                    Energy = 516
                }
                { structureOf "container" None 175_100 250_000 { X = 11; Y = 18 } with
                    Energy = 810
                }
                structureOf "container" None 172_300 250_000 { X = 23; Y = 29 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 976_997
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 976_957
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 977_015
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 977_009
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 976_958
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 977_011
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 977_032
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 976_981
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 976_974
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 32_010
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Specter836: live, it dug its seat beside the anchor.
            { body
                  "Specter836"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 26 8)
                  (Some Hold) with
                TicksToLive = 1_403
            }
            { body
                  "anchor-975456-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 23
                Energy = 50
            }
            { body
                  "ranger-975348-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 17
            }
            { body
                  "upgrader-975489-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 83
                Energy = 42
            }
            { body
                  "anchor-975540-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 113
                Energy = 50
            }
            { body
                  "anchor-975570-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 16 44)
                  None with
                TicksToLive = 143
                Energy = 6
            }
            { body
                  "guard-975682-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 15 45)
                  None with
                TicksToLive = 255
            }
            { body
                  "guard-975730-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 7 20)
                  None with
                TicksToLive = 303
            }
            { body
                  "hauler-975863-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 14 27)
                  None with
                TicksToLive = 478
                Energy = 308
            }
            { body
                  "ranger-976065-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 734
            }
            { body
                  "hauler-976395-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 1_010
                Energy = 247
            }
            { body
                  "hauler-976467-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_082
            }
            { body
                  "upgrader-976611-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 1_205
                Energy = 42
            }
            { body
                  "defender-976863-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 1_466
            }
            { body
                  "reserver-976923-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 17 43)
                  None with
                TicksToLive = 578
                Fatigue = 12
            }
            { body
                  "anchor-975498-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 71
                Energy = 50
            }
            { body
                  "upgrader-975527-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 217
            }
            { body
                  "worker-976003-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 696
                Energy = 258
            }
            { body
                  "worker-976518-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_211
                Energy = 176
            }
            { body
                  "anchor-976603-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_176
                Energy = 50
            }
            { body
                  "hauler-975921-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 25 24)
                  None with
                TicksToLive = 608
                Energy = 1_600
            }
        ]

    let a = arena 976_956 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t976,955,
                // read from Memory at live t977,069. No Guard of W17S26's: both
                // guards released it task-gone at t976,867. The log held none
                // for anchor-975456, ranger-975348, upgrader-975489,
                // defender-976863 and anchor-975498 that early, and
                // hauler-976467's withdraw names a tombstone the arena does not place.
                Assignments =
                    Map.ofList
                        [
                            "anchor-975540-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "anchor-975570-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "hauler-975863-Spawn11", "refill:spawn-Spawn11"
                            "ranger-976065-Spawn3", "guard:W17S25"
                            "hauler-976395-Spawn11", "refill:container-16-34"
                            "upgrader-976611-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "reserver-976923-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "worker-976003-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "worker-976518-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-976603-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "hauler-975921-Spawn3", "refill:storage-17-29"
                        ]
                // Heap only, so seeded: Paragon731 (18M 17A), seen on W17S26's
                // north exit at 6,0 at t976,871 and gone, held the run it left
                // by until t976,972, as live.
                ExitWatches =
                    Map.ofList
                        [
                            "W17S26",
                            ExitWatch.Held
                                {
                                    Run = [ for x in 3..21 -> { X = x; Y = 0 } ]
                                    Exit = { X = 6; Y = 0 }
                                    Until = 976_972
                                }
                        ]
                // From live Memory at t977,069, less what is stamped after the
                // tick: W15S28's stand-down of W16S26, seen t977,027, is seated
                // with its LastSeen clamped to the tick.
                ArmedSeen = Map.ofList [ "W17S25", 976_918; "W17S26", 976_871 ]
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
                                            LastSeen = 976_955
                                            Expiry = 979_527
                                            Basis = StandDownBasis.Fallback
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 976_918
                            }
                        ]
            }
    }

// ---- arena capture t985,660 (#558) --------------------------------------------
// `npm run capture -- W17S26 985660 --rooms W17S25,W15S28`, read at live t985,731.
// The room-history replay at t985,660, the state after that tick ran: arena
// tick 0 runs as t985,661 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/985600.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/985600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/985600.json
// Obsidian522 Holds: live it stood parked. Healers, ExitWatches and Sightings
// start empty (heap only). The Raid log, ArmedSeen and Towered are live
// Memory's at t985,731, less what postdates the tick.
// W17S26 is reserved by us to t989217: the arena has no reservations.
// Spawn3 is spawning ranger-985616-Spawn3 until t985742: not placed.
// Spawn8 is spawning hauler-985545-Spawn8 until t985689: not placed.
// not placed: 263 road, 1 energy, 4 tombstone (the arena has no roads, drops or ruins).
// No task verdict that early for guard-985559-Spawn11, brawler-985442-Spawn3
// and hauler-984023-Spawn8; hauler-985043-Spawn11's withdraw named a tombstone.

/// Trepidimous' Obsidian522, a 23C22M hauler, parked in W17S26 eight tiles
/// from a level-0 invader core at (12,25), while two rangers hold
/// guard:W17S26: live they stood beside the core for 100+ ticks, shooting
/// nothing.
let internal haulerBesideCore () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 27; Y = 9 } with
                    Energy = 180
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    66_670
                    100_000
                    { X = 12; Y = 25 }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 199_288
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
                { structureOf "container" None 144_200 250_000 { X = 16; Y = 34 } with
                    Energy = 1_466
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 384
                }
                { structureOf "rampart" (Some Side.Ours) 100_021 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 985_698
                }
                { structureOf "rampart" (Some Side.Ours) 99_961 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 985_679
                }
                { structureOf "rampart" (Some Side.Ours) 100_461 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 985_682
                }
                { structureOf "rampart" (Some Side.Ours) 99_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 985_687
                }
                { structureOf "rampart" (Some Side.Ours) 99_541 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 985_758
                }
                { structureOf "rampart" (Some Side.Ours) 99_731 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 985_760
                }
                { structureOf "rampart" (Some Side.Ours) 99_821 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 985_686
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 985_747
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 985_665
                }
                { structureOf "rampart" (Some Side.Ours) 100_161 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 985_680
                }
                { structureOf "rampart" (Some Side.Ours) 99_161 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 985_728
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 985_679
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 985_664
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 985_739
                }
                { structureOf "rampart" (Some Side.Ours) 95_121 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 985_727
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 985_685
                }
                { structureOf "rampart" (Some Side.Ours) 95_101 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 985_729
                }
                { structureOf "rampart" (Some Side.Ours) 94_901 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 985_725
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 985_738
                }
                { structureOf "rampart" (Some Side.Ours) 93_191 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 985_756
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_102
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_993 5_931_169
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
                        18, 34
                        19, 27
                        21, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        18, 32
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
                structureOf "container" None 126_100 250_000 { X = 6; Y = 29 }
                { structureOf "container" None 165_300 250_000 { X = 11; Y = 18 } with
                    Energy = 1_154
                }
                structureOf "container" None 164_500 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 985_709
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 985_669
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 985_727
                }
                { structureOf "rampart" (Some Side.Ours) 98_101 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 985_760
                }
                { structureOf "rampart" (Some Side.Ours) 98_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 985_670
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 985_723
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 985_744
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 985_693
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 985_686
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 27_983
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]
    let guard = parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ]
    let hauler16 = parts [ Carry, 16; Move, 8 ]
    let hauler32 = parts [ Carry, 32; Move, 16 ]

    let bodies: Body list =
        [
            // Obsidian522: live 23 CARRY, 22 MOVE, parked.
            { body
                  "Obsidian522"
                  trepidimous
                  (List.replicate 23 [ Move; Carry ] |> List.concat)
                  (at "W17S26" 19 26)
                  (Some Hold) with
                Hits = 4_420
                TicksToLive = 541
            }
            { body "ranger-984393-Spawn3" Side.Ours r7 (at "W17S26" 12 24) None with
                TicksToLive = 357
            }
            { body
                  "ranger-985441-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S26" 16 20)
                  None with
                TicksToLive = 1_297
            }
            { body
                  "reserver-985414-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 22 14)
                  None with
                TicksToLive = 364
            }
            { body "hauler-985043-Spawn11" Side.Ours hauler16 (at "W17S26" 19 13) None with
                TicksToLive = 953
            }
            { body
                  "worker-984557-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 455
                Energy = 20
            }
            { body
                  "anchor-984621-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 483
                Energy = 50
            }
            { body "ranger-984683-Spawn3" Side.Ours r7 (at "W17S25" 16 36) None with
                TicksToLive = 647
            }
            { body "hauler-984971-Spawn11" Side.Ours hauler16 (at "W17S25" 18 29) None with
                TicksToLive = 881
            }
            { body "guard-985115-Spawn11" Side.Ours guard (at "W17S25" 27 46) None with
                TicksToLive = 983
            }
            { body
                  "upgrader-985149-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_038
                Energy = 42
            }
            { body "hauler-985200-Spawn11" Side.Ours hauler16 (at "W17S25" 9 22) None with
                TicksToLive = 1_110
            }
            { body
                  "upgrader-985272-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 1_161
                Energy = 42
            }
            { body "guard-985559-Spawn11" Side.Ours guard (at "W17S25" 14 23) None with
                TicksToLive = 1_427
            }
            { body
                  "anchor-985629-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 1_497
            }
            { body
                  "worker-984223-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 16)
                  None with
                TicksToLive = 211
            }
            { body
                  "anchor-984253-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 121
                Energy = 50
            }
            { body "guard-984525-Spawn3" Side.Ours guard (at "W15S28" 6 1) None with
                TicksToLive = 393
            }
            { body
                  "upgrader-984881-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 21 24)
                  None with
                TicksToLive = 866
            }
            { body "hauler-985401-Spawn8" Side.Ours hauler32 (at "W15S28" 17 32) None with
                TicksToLive = 1_383
                Energy = 100
            }
            { body
                  "brawler-985442-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_430
            }
            { body
                  "anchor-985592-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_454
                Energy = 12
            }
            { body "hauler-984023-Spawn8" Side.Ours hauler32 (at "W15S28" 17 34) None with
                TicksToLive = 5
            }
            { body "hauler-985259-Spawn3" Side.Ours hauler32 (at "W15S28" 7 28) None with
                TicksToLive = 1_241
                Energy = 866
            }
            { body "hauler-984785-Spawn8" Side.Ours hauler32 (at "W15S28" 19 26) None with
                TicksToLive = 767
                Energy = 1_296
            }
        ]

    let a = arena 985_661 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-984393-Spawn3", "guard:W17S26"
                            "ranger-985441-Spawn11", "guard:W17S26"
                            "reserver-985414-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "worker-984557-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "anchor-984621-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "ranger-984683-Spawn3", "guard:W17S25"
                            "upgrader-985149-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "upgrader-985272-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "anchor-985629-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "worker-984223-Spawn8", "withdraw:container-11-18"
                            "anchor-984253-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-985401-Spawn8", "refill:spawn-Spawn3"
                            "anchor-985592-Spawn3", "harvest:6a8caa95dd4872bccd319014"
                            "hauler-985259-Spawn3", "withdraw:container-6-29"
                            "hauler-984785-Spawn8", "refill:spawn-Spawn3"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 985_452; "W17S26", 985_625 ]
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

// ---- arena capture kiter-across-held-exit (#656) ----------------------------
// `npm run capture -- W17S26 1020780 --rooms W17S25,W18S26 --name w17s26-held-exit-prey`,
// read at live t1,035,725. The room-history replay at t1,020,780, the state
// after that tick ran: arena tick 0 runs as t1,020,781.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1020700.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1020700.json
//   W18S26  https://screeps.com/room-history/shardSeason/W18S26/1020700.json
// Read too late for live Memory and for the Transition log: the Raid log,
// ArmedSeen, Healers and Sightings start empty; the ranger's Guard and the
// ExitWatch are seeded below (heap only), off the replay. Trimmed: W18S26 is
// Trepidimous' home, laid as its controller and towers alone, and of its
// bodies only the two armed ones by the border stand; its haulers, diggers and
// healers deep in their own room bear on nothing here. W17S26 is reserved by
// us to t1023906 (the arena has no reservations); Spawn11's hauler in its
// oven, 10 ruins and 2 tombstones are not placed.

/// W17S26 at t1,020,780 (#615's live case, t1,020,580-921): ranger-1019663
/// holds the Guard on the west exit, three tiles from Trepidimous' Virtual9490
/// (2R 5M 3H), which left by (0,40) at ~t1,020,745 and bounces just across in
/// W18S26, while Infinity0537 (5W3M) digs beside our anchor, Hammer0709
/// (3CLAIM) walks to the controller and the haulers Dragon9899 and Kang0228
/// load the room. Live the ranger stood on (1,42)-(1,45) through t1,020,830,
/// the kiter coming back in at t1,020,800 and leaving again.
let internal kiterAcrossHeldExit () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let hauling = List.replicate 23 [ Move, 1; Carry, 1 ] |> List.concat

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 128_200 250_000 { X = 27; Y = 9 } with
                    Energy = 362
                }
            ]
        |> withRoads
            [
                20, 1, 3_100
                21, 2, 3_100
                22, 3, 3_700
                23, 4, 3_200
                24, 5, 3_000
                25, 6, 3_300
                26, 7, 3_200
                27, 8, 3_400
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 131_824
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
                structureOf "container" None 155_900 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 660
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_239
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w18s26 =
        room "W18S26"
        |> withController Ownership.Rival (Some "Trepidimous") 6 4
        |> withControllerClocks 0 0 120_000 2_000_304
        |> withStructures
            [
                towerOf trepidimous { X = 18; Y = 8 } 1_000
                towerOf trepidimous { X = 22; Y = 8 } 1_000
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body "Dragon9899" trepidimous (parts hauling) (at "W17S26" 8 2) (Some Hold) with
                Hits = 2_060
                TicksToLive = 755
                Fatigue = 2
                Energy = 550
            }
            // Infinity0537: live: harvest beside the anchor.
            { body
                  "Infinity0537"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 26 9)
                  (Some Hold) with
                TicksToLive = 1_279
            }
            { body
                  "ranger-1019663-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 1 43)
                  None with
                TicksToLive = 417
            }
            { body
                  "anchor-1019739-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 487
                Energy = 50
            }
            { body
                  "reserver-1020243-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 73
            }
            { body "Kang0228" trepidimous (parts hauling) (at "W17S26" 26 8) (Some Hold) with
                Hits = 4_396
                TicksToLive = 1_084
            }
            { body
                  "Hammer0709"
                  trepidimous
                  (parts [ Move, 3; BodyPart.Claim, 3 ])
                  (at "W17S26" 21 2)
                  (Some Hold) with
                TicksToLive = 545
            }
            { body
                  "worker-1019528-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 327
                Energy = 108
            }
            { body
                  "upgrader-1020051-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 19 35)
                  None with
                TicksToLive = 838
            }
            { body
                  "anchor-1020501-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_243
                Energy = 50
            }
            { body
                  "upgrader-1020525-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_312
            }
            // Midnight0651: live: healed itself at the south road.
            { body
                  "Midnight0651"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W18S26" 28 48)
                  (Some Hold) with
                TicksToLive = 1_411
            }
            // Virtual9490: live: holds 1-3 tiles across W17S26's west exit,
            // stepping back in at t1,020,800.
            { body
                  "Virtual9490"
                  trepidimous
                  (parts [ RangedAttack, 2; Move, 4; Heal, 3; Move, 1 ])
                  (at "W18S26" 48 41)
                  (Some Hold) with
                TicksToLive = 238
            }
        ]

    let a = arena 1_020_781 [ w17s26; w17s25; w18s26 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments = Map.ofList [ "ranger-1019663-Spawn11", "guard:W17S26" ]
                ExitWatches =
                    Map.ofList
                        [
                            "W17S26",
                            ExitWatch.Held
                                {
                                    Run = [ for y in 36..48 -> { X = 0; Y = y } ]
                                    Exit = { X = 0; Y = 40 }
                                    Until = 1_020_845
                                }
                        ]
            }
    }
