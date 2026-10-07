/// The ranger in the arena: its reflex beside a shooter, its stand at a held
/// line, and the ranger row's pair against a kiting raid.
module Fabot.Core.Tests.ArenaRangerTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

// ---- arena capture synapse-beside-ranger (#535) ----------------------------
// `npm run capture -- W17S26 965400 --rooms W17S25,W15S28 --name synapse`.
// The room-history replay at t965,400: arena tick 0 runs as t965,401.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/965400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/965400.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/965400.json
// Left out: the Raid log, ArmedSeen, Healers and Sightings start empty; roads,
// a ruin and tombstones are not placed.

/// W17S26 at t965,400 (#535, today's combat stats): Synapse673, 13 RANGED_ATTACK on
/// 7 MOVE, standing still and shooting ranger-965017 beside it, which had its
/// MOVE shot off, 4 RANGED_ATTACK and 7 HEAL left, healed itself and never shot
/// until it died at t965,417; Synapse673 never lost a hit.
let private synapseBeside () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures [ structureOf "container" None 90_000 250_000 { X = 27; Y = 9 } ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_876 32_600
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 800
        |> withStructures
            [
                for x, y in [ 11, 25; 11, 27; 12, 28; 14, 26; 15, 29; 16, 28; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 66_200 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 532
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 14; Y = 30 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 27 } with
                    Energy = 42
                }
                { structureOf "rampart" (Some Side.Ours) 781 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 965_403
                }
                { structureOf "rampart" (Some Side.Ours) 96_901 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 965_486
                }
                { structureOf "rampart" (Some Side.Ours) 35_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 965_491
                }
                { structureOf "rampart" (Some Side.Ours) 54_971 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 965_463
                }
                { structureOf "rampart" (Some Side.Ours) 7_731 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 965_465
                }
                { structureOf "rampart" (Some Side.Ours) 14_021 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 965_490
                }
                { structureOf "rampart" (Some Side.Ours) 3_141 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 965_452
                }
                { structureOf "rampart" (Some Side.Ours) 14_601 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 965_469
                }
                { structureOf "rampart" (Some Side.Ours) 1_041 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 965_484
                }
                { structureOf "rampart" (Some Side.Ours) 2_501 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 965_433
                }
                { structureOf "rampart" (Some Side.Ours) 11_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 965_468
                }
                { structureOf "rampart" (Some Side.Ours) 6_001 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 965_444
                }
                { structureOf "rampart" (Some Side.Ours) 66_711 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 965_432
                }
                { structureOf "rampart" (Some Side.Ours) 56_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 965_489
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 965_434
                }
                { structureOf "rampart" (Some Side.Ours) 3_601 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 965_430
                }
                { structureOf "rampart" (Some Side.Ours) 19_161 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 965_443
                }
                { structureOf "rampart" (Some Side.Ours) 1_461 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 965_461
                }
                towerOf Side.Ours { X = 13; Y = 29 } 780
            ]
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 32 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 33 }
        |> withSite Side.Ours "road" None 0 { X = 23; Y = 34 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 35 }
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 36 }
        |> withSite Side.Ours "road" None 200 { X = 20; Y = 35 }
        |> withSite Side.Ours "road" None 200 { X = 17; Y = 36 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 37 }
        |> withSite Side.Ours "road" None 0 { X = 19; Y = 38 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 39 }
        |> withSite Side.Ours "road" None 0 { X = 17; Y = 38 }
        |> withSite Side.Ours "road" None 180 { X = 16; Y = 37 }
        |> withSite Side.Ours "road" None 988 { X = 17; Y = 40 }
        |> withSite Side.Ours "road" None 0 { X = 19; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 17; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 29 }
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 0 { X = 13; Y = 25 }
        |> withSite Side.Ours "extension" None 0 { X = 13; Y = 31 }
        |> withSite Side.Ours "extension" None 0 { X = 15; Y = 31 }
        |> withSite Side.Ours "extension" None 0 { X = 17; Y = 29 }
        |> withSite Side.Ours "extension" None 750 { X = 17; Y = 31 }
        |> withSite Side.Ours "extension" None 1_346 { X = 10; Y = 24 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 30 }
        |> withSite Side.Ours "extension" None 1_765 { X = 12; Y = 24 }
        |> withSite Side.Ours "rampart" None 0 { X = 2; Y = 16 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 29 }
        |> withSite Side.Ours "road" None 0 { X = 11; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_645_583
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
                { structureOf "container" None 141_100 250_000 { X = 6; Y = 29 } with
                    Energy = 444
                }
                { structureOf "container" None 142_900 250_000 { X = 11; Y = 18 } with
                    Energy = 228
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 965_414
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 965_473
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 965_432
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 965_479
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 965_474
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 965_428
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 965_449
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 965_497
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 965_490
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 20_281
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
            // live: rangedAttack→37,4 every tick, standing still.
            { body
                  "Synapse673"
                  trepidimous
                  (parts [ Move, 7; RangedAttack, 13 ])
                  (at "W17S26" 36 3)
                  (Some(Do [ Act.RangedAttack "ranger-965017-Spawn3" ])) with
                TicksToLive = 831
            }
            // ranger-965017-Spawn3: live: 4 RANGED_ATTACK, 7 HEAL
            { body
                  "ranger-965017-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 37 4)
                  None with
                Hits = 1_018
                TicksToLive = 1_241
            }
            // The other resident, at W17S25's controller; the economy of both
            // homes is left out: it decides nothing of this fight.
            { body
                  "ranger-964741-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 36)
                  None with
                TicksToLive = 965
            }
        ]

    let a = arena 965_401 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log holds no verdict that early for any body:
                // they start unassigned.
                Assignments = Map.empty
            }
    }

[<Tests>]
let arenaRangerReflexTests =
    testList
        "arena capture synapse-beside-ranger"
        [
            // #535 (combat stats, 2026-10-06): four 4,900-energy rangers died in
            // reach of their killer, healing themselves, never shooting.
            // On main b6c986a1 it never shoots: unassigned (its Task gone) it
            // strikes nothing, the heal reflex alone acting. Now the weapon
            // reflex shoots every tick a RANGED_ATTACK still acts; its last
            // one goes under 700 hits (the seven HEAL at its tail).
            test
                "#535 capture t965,400: a rooted ranger beside Synapse673 shoots it every tick a gun of its still acts" {
                let _, trace = synapseBeside () |> run 12
                let failure = describe trace
                let ranger = "ranger-965017-Spawn3"
                let healTail = 7 * Engine.partHits

                // Each tick with the ranger's hits at its start, the capture's first.
                let armedTicks =
                    trace
                    |> List.scan
                        (fun _ t ->
                            Some t,
                            t.Bodies
                            |> List.tryFind (fun b -> b.Id = ranger)
                            |> Option.map (fun b -> b.Hits)
                            |> Option.defaultValue 0)
                        (None, 1_018)
                    |> List.pairwise
                    |> List.choose (fun ((_, before), (tick, _)) ->
                        tick |> Option.filter (fun _ -> before > healTail))

                Expect.isNonEmpty armedTicks $"the premise: it is armed a while\n{failure}"

                for t in armedTicks do
                    Expect.contains
                        t.Ours
                        (RangedAttackCreep(ranger, "Synapse673"))
                        $"t{t.Tick}: it shoots\n{failure}"
            }
        ]

// `npm run capture -- W17S25 981106 --rooms W18S25,W15S28 --name ranger-at-the-line`, read at live t981,917.
// The room-history replay at t981,106, the state after that tick ran: arena
// tick 0 runs as t981,107 and decides what our code decided live then.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/981100.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/981100.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/981100.json
// W18S25 is reserved by us to t985515: the arena has no reservations.
// Spawn3 is spawning hauler-980982-Spawn3 until t981126: not placed.
// Spawn8 is spawning brawler-981080-Spawn8 until t981230: not placed.
// not placed: 279 road, 4 tombstone, 1 energy (the arena has no roads, drops or ruins).

/// #552, live t981,106: Trepidimous' 18M17A melee and 11M7H healer bounce on
/// W17S25's west exit tile 0,15, two from the 2,15–2,17 ramparts, draining the
/// tower; W15S28's resident ranger stands by the controller at 18,35 and the
/// defender at 14,23. Both stand on their exit tile, so the border carries
/// them across every tick: the bounce. The tower holds `towerEnergy` (990
/// live).
let private rangerAtTheLine (towerEnergy: int) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 40_000 134_101
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
                { structureOf "container" None 156_600 250_000 { X = 16; Y = 34 } with
                    Energy = 1_984
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 718
                }
                { structureOf "rampart" (Some Side.Ours) 98_221 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 981_144
                }
                { structureOf "rampart" (Some Side.Ours) 100_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 981_125
                }
                { structureOf "rampart" (Some Side.Ours) 98_271 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 981_128
                }
                { structureOf "rampart" (Some Side.Ours) 96_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 981_133
                }
                { structureOf "rampart" (Some Side.Ours) 97_471 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 981_204
                }
                { structureOf "rampart" (Some Side.Ours) 98_531 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 981_206
                }
                { structureOf "rampart" (Some Side.Ours) 99_821 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 981_132
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 981_193
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 981_111
                }
                { structureOf "rampart" (Some Side.Ours) 99_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 981_126
                }
                { structureOf "rampart" (Some Side.Ours) 99_761 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 981_174
                }
                { structureOf "rampart" (Some Side.Ours) 97_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 981_125
                }
                { structureOf "rampart" (Some Side.Ours) 58_901 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 981_110
                }
                { structureOf "rampart" (Some Side.Ours) 9_901 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 981_185
                }
                { structureOf "rampart" (Some Side.Ours) 90_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 981_173
                }
                { structureOf "rampart" (Some Side.Ours) 76_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 981_131
                }
                { structureOf "rampart" (Some Side.Ours) 90_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 981_175
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 981_171
                }
                { structureOf "rampart" (Some Side.Ours) 97_901 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 981_184
                }
                { structureOf "rampart" (Some Side.Ours) 96_191 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 981_202
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_094
                }
                towerOf Side.Ours { X = 13; Y = 29 } towerEnergy
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 347
                }
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_859_035
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
                        13, 27
                        13, 29
                        13, 33
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        16, 26
                        16, 32
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
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 36
                        13, 35
                        15, 35
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
                        21, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 167_100 250_000 { X = 6; Y = 29 } with
                    Energy = 168
                }
                { structureOf "container" None 130_100 250_000 { X = 11; Y = 18 } with
                    Energy = 672
                }
                structureOf "container" None 132_300 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 981_155
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 981_115
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 981_173
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 981_195
                }
                { structureOf "rampart" (Some Side.Ours) 103_001 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 981_116
                }
                { structureOf "rampart" (Some Side.Ours) 94_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 981_169
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 981_190
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 981_139
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 981_132
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 13_905
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 487
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "hauler-979756-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 10 25)
                  None with
                TicksToLive = 220
                Energy = 248
            }
            { body
                  "upgrader-979829-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 272
                Energy = 42
            }
            { body
                  "anchor-980086-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 508
                Energy = 50
            }
            { body
                  "hauler-980131-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 595
                Energy = 550
            }
            { body
                  "worker-980320-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 20 48)
                  None with
                TicksToLive = 772
                Energy = 350
            }
            { body
                  "hauler-980420-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 884
                Energy = 209
            }
            { body
                  "upgrader-980563-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 1_006
                Energy = 42
            }
            { body
                  "ranger-980660-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 18 35)
                  None with
                TicksToLive = 1_178
            }
            { body
                  "reserver-980840-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W17S25" 2 17)
                  None with
                TicksToLive = 338
            }
            { body
                  "defender-981038-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 1_490
            }
            // Splinter546: live: 4 MOVE, 17 ATTACK; attacked by the tower.
            { body
                  "Splinter546"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S25" 0 15)
                  (Some Hold) with
                Hits = 2_084
                TicksToLive = 1_043
            }
            // Oblivion822: live: crossing to W17S25 0,15; at t981,111 and
            // after, healing itself there on the exit.
            { body
                  "Oblivion822"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S25" 49 15)
                  (Some(Do [ Act.Heal "Oblivion822" ])) with
                TicksToLive = 268
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 47
                Energy = 50
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 298
                Energy = 50
            }
            { body
                  "hauler-980337-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 32)
                  None with
                TicksToLive = 873
                Energy = 850
            }
            { body
                  "worker-980588-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 1_130
                Energy = 450
            }
            { body
                  "hauler-980792-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 6)
                  None with
                TicksToLive = 1_328
            }
            { body
                  "hauler-980936-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 33)
                  None with
                TicksToLive = 1_472
                Energy = 800
            }
        ]

    let a = arena 981_107 [ w17s25; w18s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t981,106; it holds none that early for the other bodies.
                Assignments =
                    Map.ofList
                        [
                            "upgrader-980563-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-980792-Spawn8", "withdraw:6aba1ca3293c6f833d8b43f5"
                        ]
            }
    }

[<Tests>]
let arenaRangerAtTheLineTests =
    testList
        "arena capture ranger-at-the-line"
        [
            // #552, with the live tower (990). Live it held fire after
            // t981,111 and the pair survived: with only the healer in the room
            // no full tick was forced and the light ticks' towers stayed
            // silent, the regression f6875e52 fixed. The arena runs every
            // tick full, so on main 6d2a37d4 as now the tower kills the melee
            // at t12 and the healer at t19, before the resident's 23-tick
            // walk to the line ends.
            test
                "#552 capture t981,106, the live tower: it kills the pair before the resident reaches the line" {
                let _, trace = rangerAtTheLine 990 |> run 30
                let failure = describe trace

                let dies id =
                    trace
                    |> List.tryFind (fun t -> t.Bodies |> List.forall (fun b -> b.Id <> id))
                    |> Option.map (fun t -> t.Tick)

                let reached =
                    trace
                    |> List.tryFind (fun t ->
                        t.Bodies
                        |> List.exists (fun b ->
                            b.Id = "ranger-980660-Spawn8"
                            && b.At.Room = "W17S25"
                            && b.At.X = 2
                            && b.At.Y >= 15
                            && b.At.Y <= 18))
                    |> Option.map (fun t -> t.Tick)

                Expect.isSome (dies "Splinter546") $"the melee dies\n{failure}"
                Expect.isSome (dies "Oblivion822") $"the healer dies\n{failure}"

                Expect.isTrue
                    (match dies "Oblivion822", reached with
                     | Some died, Some came -> died < came
                     | Some _, None -> true
                     | None, _ -> false)
                    $"before the resident stands on the line\n{failure}"
            }

            // #552, the tower dry at start so the pair outlives the walk, as it
            // did live; the haulers' refill brings the tower back.
            //
            // On main 6d2a37d4 the resident thrashed between the line and the
            // controller's ring with the bounce (its kite ground laid only on
            // the ticks the melee stood in the room) and never shot.
            test
                "#552 capture t981,106: the mother's resident takes a rampart within three of the exit tile and shoots the bouncing pair, the defender holding 2,15" {
                let _, trace = rangerAtTheLine 0 |> run 30
                let failure = describe trace
                let ranger = "ranger-980660-Spawn8"
                let defender = "defender-981038-Spawn11"
                let at x y = RoomPos.at "W17S25" (tile x y)
                let exitTile = at 0 15

                // The west line's ramparts, 2,15–2,20 and 2,23–2,27.
                let atLine =
                    [ for y in [ 15..20 ] @ [ 23..27 ] -> at 2 y ]
                    |> List.filter (fun rampart ->
                        RoomPos.range rampart exitTile
                        |> Option.exists (fun r -> r <= Engine.rangedRange))
                    |> Set.ofList

                let standing id (t: TickTrace) =
                    t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map (fun b -> b.At)

                let shots =
                    trace
                    |> List.filter (fun t ->
                        t.Ours
                        |> List.exists (function
                            | RangedAttackCreep(name, target) ->
                                name = ranger && (target = "Splinter546" || target = "Oblivion822")
                            | _ -> false))

                Expect.isNonEmpty shots $"the ranger shoots the pair\n{failure}"

                for t in shots do
                    Expect.isTrue
                        (standing ranger t |> Option.exists (fun at -> Set.contains at atLine))
                        $"t{t.Tick}: from a rampart within three of the exit tile\n{failure}"

                let holds = trace |> List.skipWhile (fun t -> standing defender t <> Some(at 2 15))

                Expect.isNonEmpty holds $"the defender takes 2,15\n{failure}"

                Expect.isTrue
                    (holds |> List.forall (fun t -> standing defender t = Some(at 2 15)))
                    $"and holds it\n{failure}"
            }
        ]
// ---- arena capture w19s25-t1012579 -------------------------------------------
// `npm run capture -- W19S25 1012579 --rooms W17S25,W15S28,W17S26,W18S25`, read at live t1,012,645.
// The room-history replay at t1,012,579, the state after that tick ran: arena
// tick 0 runs as t1,012,580 and decides what our code decided live then.
//   W19S25  https://screeps.com/room-history/shardSeason/W19S25/1012500.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1012500.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/1012500.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1012500.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/1012500.json
// Spawn11 is spawning worker-1012533-Spawn11 until t1012614: not placed.
// Spawn3 is spawning worker-1012447-Spawn3 until t1012597: not placed.
// Spawn8 is spawning hauler-1012517-Spawn8 until t1012661: not placed.
// W17S26 is reserved by us to t1012751: the arena has no reservations.
// W18S25 is reserved by us to t1017579: the arena has no reservations.
// not placed: 8 ruin, 1 energy, 1 tombstone (the arena has no drops, tombstones or ruins).
/// Two Trepidimous 3RA4H7M kiters, Shadow2363 in W17S26 and Deepening1722
/// looping W19S25's west exit, against W17S25's two 4RA2H rangers and melee
/// guards (#609).
let private trepKiters () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w19s25 =
        room "W19S25"
        |> withSite trepidimous "road" None 0 { X = 9; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 10; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 11; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 12; Y = 37 }

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 44_240
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
                        15, 29
                        15, 31
                        16, 30
                        16, 32
                        17, 31
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 15, 27; 16, 28; 17, 29; 18, 30 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 160_800 250_000 { X = 16; Y = 34 } with
                    Energy = 348
                }
                { structureOf "container" None 160_000 250_000 { X = 17; Y = 20 } with
                    Energy = 468
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 10_000_000 { X = 2; Y = 15 } with
                    NextDecay = 1_012_626
                }
                { structureOf "rampart" (Some Side.Ours) 99_161 10_000_000 { X = 2; Y = 16 } with
                    NextDecay = 1_012_607
                }
                { structureOf "rampart" (Some Side.Ours) 99_231 10_000_000 { X = 2; Y = 17 } with
                    NextDecay = 1_012_610
                }
                { structureOf "rampart" (Some Side.Ours) 99_731 10_000_000 { X = 2; Y = 18 } with
                    NextDecay = 1_012_615
                }
                { structureOf "rampart" (Some Side.Ours) 99_111 10_000_000 { X = 2; Y = 19 } with
                    NextDecay = 1_012_587
                }
                { structureOf "rampart" (Some Side.Ours) 99_031 10_000_000 { X = 2; Y = 20 } with
                    NextDecay = 1_012_589
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 10_000_000 { X = 2; Y = 23 } with
                    NextDecay = 1_012_614
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 10_000_000 { X = 2; Y = 24 } with
                    NextDecay = 1_012_675
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 10_000_000 { X = 2; Y = 25 } with
                    NextDecay = 1_012_593
                }
                { structureOf "rampart" (Some Side.Ours) 98_761 10_000_000 { X = 2; Y = 26 } with
                    NextDecay = 1_012_608
                }
                { structureOf "rampart" (Some Side.Ours) 99_961 10_000_000 { X = 2; Y = 27 } with
                    NextDecay = 1_012_656
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 10_000_000 { X = 11; Y = 29 } with
                    NextDecay = 1_012_590
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 10_000_000 { X = 13; Y = 27 } with
                    NextDecay = 1_012_607
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 10_000_000 { X = 13; Y = 29 } with
                    NextDecay = 1_012_592
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 10_000_000 { X = 14; Y = 28 } with
                    NextDecay = 1_012_667
                }
                { structureOf "rampart" (Some Side.Ours) 93_721 10_000_000 { X = 16; Y = 44 } with
                    NextDecay = 1_012_655
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 10_000_000 { X = 17; Y = 20 } with
                    NextDecay = 1_012_613
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 10_000_000 { X = 17; Y = 44 } with
                    NextDecay = 1_012_657
                }
                { structureOf "rampart" (Some Side.Ours) 93_501 10_000_000 { X = 18; Y = 44 } with
                    NextDecay = 1_012_653
                }
                { structureOf "rampart" (Some Side.Ours) 93_311 10_000_000 { X = 32; Y = 31 } with
                    NextDecay = 1_012_666
                }
                { structureOf "rampart" (Some Side.Ours) 94_101 10_000_000 { X = 32; Y = 32 } with
                    NextDecay = 1_012_585
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_348
                }
                towerOf Side.Ours { X = 11; Y = 29 } 980
                towerOf Side.Ours { X = 13; Y = 29 } 935
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
                12, 25, 13_400
                12, 27, 16_200
                12, 29, 4_000
                12, 31, 4_000
                13, 24, 3_300
                13, 26, 4_000
                13, 28, 3_600
                13, 30, 4_000
                14, 23, 3_800
                14, 25, 4_400
                14, 27, 4_200
                14, 29, 4_000
                14, 31, 3_900
                15, 22, 3_400
                15, 28, 4_000
                15, 30, 2_900
                15, 32, 3_800
                16, 21, 4_100
                16, 29, 3_100
                16, 31, 4_000
                16, 33, 2_800
                16, 35, 3_600
                16, 37, 3_200
                16, 38, 19_000
                16, 39, 21_300
                16, 41, 4_000
                16, 43, 3_800
                16, 44, 3_000
                17, 20, 3_100
                17, 30, 4_200
                17, 32, 3_600
                17, 34, 4_300
                17, 36, 2_500
                17, 38, 2_300
                17, 39, 20_000
                17, 40, 15_600
                17, 42, 4_300
                17, 45, 4_300
                18, 29, 4_100
                18, 31, 2_600
                18, 35, 3_200
                18, 37, 2_400
                18, 39, 3_100
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
                21, 32, 4_000
                21, 34, 3_800
                21, 36, 3_200
                21, 38, 3_800
                22, 33, 2_600
                22, 35, 3_200
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
                32, 31, 17_800
                33, 30, 12_140
                33, 32, 19_280
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_696_895
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
                { structureOf "container" None 162_700 250_000 { X = 6; Y = 29 } with
                    Energy = 12
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 264
                }
                { structureOf "container" None 135_700 250_000 { X = 23; Y = 29 } with
                    Energy = 1_976
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 1_012_637
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 1_012_597
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 1_012_655
                }
                { structureOf "rampart" (Some Side.Ours) 97_801 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 1_012_593
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 1_012_598
                }
                { structureOf "rampart" (Some Side.Ours) 99_601 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 1_012_651
                }
                { structureOf "rampart" (Some Side.Ours) 99_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 1_012_672
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 1_012_621
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 1_012_614
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 34_135
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 319
                }
            ]
        |> withRoads
            [
                4, 5, 4_500
                5, 4, 3_300
                5, 6, 4_500
                5, 44, 3_500
                6, 3, 2_500
                6, 7, 3_800
                6, 29, 3_800
                6, 43, 3_000
                6, 45, 3_900
                7, 2, 4_300
                7, 8, 4_500
                7, 28, 4_200
                7, 42, 2_700
                7, 46, 3_300
                8, 1, 3_000
                8, 9, 2_700
                8, 28, 3_800
                8, 41, 4_600
                8, 47, 2_600
                9, 10, 3_800
                9, 27, 3_300
                9, 28, 4_800
                9, 40, 2_700
                9, 48, 4_000
                10, 11, 3_000
                10, 26, 3_800
                10, 29, 4_300
                10, 39, 4_600
                11, 12, 2_500
                11, 18, 3_500
                11, 27, 2_500
                11, 30, 4_400
                11, 38, 3_500
                12, 13, 3_400
                12, 17, 3_600
                12, 25, 17_700
                12, 27, 19_100
                12, 28, 2_500
                12, 29, 4_300
                12, 31, 3_100
                12, 33, 21_800
                12, 35, 15_100
                12, 37, 2_900
                13, 14, 3_500
                13, 16, 5_000
                13, 26, 16_000
                13, 28, 3_100
                13, 30, 4_400
                13, 32, 4_900
                13, 34, 4_600
                13, 36, 3_000
                14, 15, 5_000
                14, 25, 14_800
                14, 27, 4_200
                14, 29, 3_500
                14, 31, 3_300
                14, 33, 4_200
                14, 35, 3_800
                15, 15, 3_700
                15, 26, 4_000
                15, 28, 4_600
                15, 30, 4_800
                15, 32, 2_700
                15, 34, 4_600
                16, 16, 3_900
                16, 25, 15_700
                16, 27, 2_500
                16, 29, 4_100
                16, 31, 2_800
                16, 33, 4_400
                16, 35, 3_200
                17, 17, 4_900
                17, 26, 14_600
                17, 28, 2_500
                17, 30, 3_700
                17, 32, 3_400
                17, 34, 3_000
                18, 18, 3_600
                18, 25, 13_900
                18, 27, 16_700
                18, 29, 4_800
                18, 31, 4_900
                18, 33, 3_300
                19, 19, 18_000
                19, 22, 18_800
                19, 24, 13_900
                19, 26, 14_600
                19, 28, 2_800
                19, 30, 4_400
                19, 32, 3_700
                20, 20, 18_300
                20, 21, 18_800
                20, 23, 20_000
                20, 25, 16_600
                20, 27, 17_500
                20, 29, 2_700
                20, 31, 4_900
                20, 33, 18_300
                21, 21, 4_800
                21, 22, 2_600
                21, 23, 3_000
                21, 24, 15_200
                21, 25, 15_200
                21, 26, 16_000
                21, 28, 21_800
                21, 30, 3_200
                21, 32, 13_600
                21, 34, 20_400
                22, 24, 4_900
                22, 25, 13_400
                22, 27, 15_100
                22, 28, 3_300
                22, 29, 2_900
                22, 30, 19_200
                22, 31, 15_400
                22, 32, 18_700
                22, 33, 16_300
                22, 35, 4_500
                23, 26, 13_400
                23, 28, 18_700
                23, 29, 15_300
                23, 30, 17_000
                23, 31, 20_200
                23, 32, 18_500
                23, 33, 21_900
                23, 34, 2_700
                24, 25, 19_300
                24, 28, 17_100
                24, 29, 20_200
                24, 30, 17_500
                24, 33, 14_100
                25, 24, 4_100
                25, 28, 19_800
                25, 29, 17_800
                25, 34, 20_500
                26, 23, 14_100
                27, 22, 11_800
                28, 21, 19_000
                28, 34, 16_100
                29, 20, 12_800
                30, 19, 11_200
                31, 18, 3_400
                32, 17, 3_300
                33, 16, 2_100
                34, 15, 4_400
                35, 14, 3_400
                36, 13, 13_700
                37, 12, 4_100
                38, 11, 2_500
                39, 12, 3_500
                40, 13, 13_000
                41, 14, 11_300
                42, 15, 2_300
                43, 16, 2_400
                44, 17, 4_200
                45, 18, 4_200
                46, 19, 4_200
                47, 20, 2_600
                48, 21, 4_000
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_700 250_000 { X = 27; Y = 9 } with
                    Energy = 814
                }
            ]
        |> withRoads
            [
                20, 1, 2_700
                21, 2, 4_500
                22, 3, 2_100
                23, 4, 2_700
                24, 5, 4_400
                25, 6, 2_900
                26, 7, 4_600
                27, 8, 2_700
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 31; Y = 5 } with
                    Energy = 312
                }
            ]
        |> withRoads
            [
                28, 9, 2_500
                29, 8, 3_850
                29, 10, 3_580
                30, 7, 4_000
                30, 11, 3_300
                31, 6, 2_900
                31, 12, 4_000
                32, 13, 3_200
                33, 14, 3_600
                34, 15, 3_900
                35, 16, 3_600
                36, 17, 2_400
                37, 16, 3_300
                38, 15, 4_000
                39, 14, 4_000
                40, 13, 4_200
                41, 12, 4_100
                42, 13, 3_700
                43, 13, 2_970
                44, 13, 2_800
                45, 13, 3_200
                46, 14, 4_200
                47, 15, 3_100
                48, 16, 4_200
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Fourier1584: live: no act
            { body
                  "Fourier1584"
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
                  (at "W19S25" 27 26)
                  (Some Hold) with
                TicksToLive = 641
            }
            // Reptile2176: live: no act
            { body
                  "Reptile2176"
                  trepidimous
                  (parts [ Move, 1; Work, 1; Carry, 1 ])
                  (at "W19S25" 34 26)
                  (Some Hold) with
                TicksToLive = 1_104
            }
            { body
                  "ranger-1011255-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 27 20)
                  None with
                TicksToLive = 210
            }
            { body
                  "ranger-1011720-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 7 12)
                  None with
                TicksToLive = 675
            }
            // Deepening1722: live: healed→2,17 heal→2,17. Live it loops the
            // west exit: in at 0,19, up the diagonal to 3,16, out at 0,13,
            // about eight ticks in W20S25, and in again (t1,012,505-530).
            { body
                  "Deepening1722"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W19S25" 3 16)
                  (Some(
                      Phases
                          [
                              for k in 0..39 do
                                  16 * k + 4,
                                  Waypoints
                                      [ at "W19S25" 2 15; at "W19S25" 1 14; at "W20S25" 48 13 ]

                                  16 * k + 12, GoTo(at "W20S25" 47 17)

                                  16 * k + 16,
                                  Waypoints [ at "W19S25" 1 18; at "W19S25" 2 17; at "W19S25" 3 16 ]
                          ]
                  )) with
                TicksToLive = 683
            }
            { body
                  "anchor-1011403-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 352
                Energy = 50
            }
            { body
                  "hauler-1011503-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 530
            }
            { body
                  "worker-1011790-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 18 31)
                  None with
                TicksToLive = 790
                Energy = 400
            }
            { body
                  "upgrader-1012042-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_030
                Energy = 39
            }
            // guard-1012445-Spawn11: crossing to W17S26 3,0
            { body
                  "guard-1012445-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 3 49)
                  None with
                TicksToLive = 1_424
            }
            { body
                  "ranger-1011014-Spawn3"
                  Side.Ours
                  (parts [ Move, 12; RangedAttack, 8; Heal, 4 ])
                  (at "W15S28" 4 45)
                  None with
                TicksToLive = 5
            }
            { body
                  "hauler-1011018-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 81
                Energy = 12
            }
            { body
                  "hauler-1011086-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 2 1)
                  None with
                TicksToLive = 149
            }
            { body
                  "anchor-1011168-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 117
                Energy = 50
            }
            { body
                  "upgrader-1011653-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 23 28)
                  None with
                TicksToLive = 719
                Energy = 26
            }
            { body
                  "anchor-1011928-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 871
                Energy = 50
            }
            { body
                  "hauler-1012086-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 27)
                  None with
                TicksToLive = 1_149
            }
            { body
                  "worker-1012144-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_213
                Energy = 386
            }
            { body
                  "hauler-1011688-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 7 28)
                  None with
                TicksToLive = 751
                Energy = 1_092
            }
            // Shadow2363: live: healed→5,3 heal→5,3 rangedAttack→7,0
            { body
                  "Shadow2363"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W17S26" 5 3)
                  (Some(Waypoints [ at "W17S26" 5 3 ])) with
                TicksToLive = 1_324
            }
            // guard-1011187-Spawn11: live: 2 TOUGH, 10 MOVE, 6 ATTACK, 2 HEAL
            // guard-1011187-Spawn11: crossing to W17S25 7,49
            { body
                  "guard-1011187-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S26" 7 0)
                  None with
                Hits = 1_994
                TicksToLive = 166
            }
            { body
                  "anchor-1011647-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 596
                Energy = 50
            }
            { body
                  "reserver-1012178-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 209
            }
            // hauler-1011295-Spawn11: crossing to W17S25 0,15
            { body
                  "hauler-1011295-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W18S25" 49 15)
                  None with
                TicksToLive = 322
                Energy = 1_008
            }
        ]

    // W19S25 is a harassment room W17S25 casts (live `outposts`).
    let a =
        arena 1_012_580 [ w19s25; w17s25; w15s28; w17s26; w18s25; room "W20S25" ] colonies bodies
        |> withHarass Colony.harass

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t1,012,579,
                // read from Memory at live t1,012,645; the log held none for
                // guard-1012445, ranger-1011014 or guard-1011187.
                Assignments =
                    Map.ofList
                        [
                            "ranger-1011255-Spawn11", "guard:W19S25"
                            "ranger-1011720-Spawn11", "guard:W19S25"
                            "anchor-1011403-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-1011503-Spawn11", "refill:tower-13-29"
                            "worker-1011790-Spawn11", "refill:spawn-Spawn11"
                            "upgrader-1012042-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-1011018-Spawn8", "refill:container-23-29"
                            "hauler-1011086-Spawn3", "withdraw:6ac0e2f1e169cce45673df61"
                            "anchor-1011168-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "upgrader-1011653-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-1011928-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-1012086-Spawn3", "withdraw:6aba1ca3293c6f833d8b43f5"
                            "worker-1012144-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-1011688-Spawn8", "withdraw:container-6-29"
                            "anchor-1011647-Spawn11", "harvest:6a8caa71dd4872bccd318b28"
                            "reserver-1012178-Spawn11", "reserve:6a8caa71dd4872bccd318b29"
                            "hauler-1011295-Spawn11", "refill:tower-11-29"
                        ]
                // From live Memory at t1,012,645, 66 ticks after the capture,
                // less what is stamped after the tick. Healers, ExitWatches
                // and Sightings start empty.
                ArmedSeen = Map.ofList [ "W17S25", 1_012_536 ]
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
                                            LastSeen = 1_012_579
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
                                            LastSeen = 1_012_579
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
                                HomeRaided = Some 1_012_536
                            }
                        ]
            }
    }

[<Tests>]
let rangerPairTests =
    testList
        "arena a raid the ranger row's pair beats"
        [
            // #609: Shadow2363, 3 RANGED_ATTACK 4 HEAL 7 MOVE, out-heals the
            // 4RA2H ranger W17S25's 1,800 bank buys (40 < 48) and loses to two
            // (80). Before, the room was the melee guard's, which never
            // catches a 7M kiter: as live, guard-1011187 bounced on the
            // W17S25/W17S26 border and Shadow2363 stood at full hits; and in
            // W19S25 ranger-1011255 sat on the Stand's seat while
            // ranger-1011720 hovered four off Deepening1722, neither ever
            // shooting (live t1,012,580-605). Now the room is the ranger
            // row's at two bodies: ranger-1011720 is sent, the cast waits for
            // it at W17S25's south exit, the pair goes in together and kills
            // Shadow2363 at t175, neither below 1,186.
            //
            // Since W6 S3 W19S25 is cast by nobody (W17S25 holds no stock here,
            // about 5k live, under `Tuning.CasterReserveHold`), so W17S25
            // projects it for its rangers alone, and its Guard is no fight
            // they keep (`Planner.planFightTasks`): ranger-1011720 is free
            // for its colony's own outpost, as before.
            test
                "#609 capture t1,012,579: a kiter one ranger loses to and two beat is answered by two rangers, which kill it" {
                let start = trepKiters () |> withCasts [ "ranger" ]
                let first, _ = step start

                Expect.notEqual
                    (Map.tryFind "ranger-1011720-Spawn11" first.Carried.Assignments)
                    (Some "guard:W19S25")
                    "the premise: no ranger keeps the Guard of the room nobody casts"

                let _, trace = start |> run 200
                let failure = describe trace

                Expect.isLessThanOrEqual
                    (diedOn "Shadow2363" trace |> Option.defaultValue System.Int32.MaxValue)
                    190
                    $"the pair kills Shadow2363 by t190\n{failure}"

                let least =
                    trace
                    |> List.collect (fun t ->
                        t.Bodies
                        |> List.filter (fun b -> b.At.Room = "W17S26" && b.Id.StartsWith "ranger-"))
                    |> List.map _.Hits

                Expect.isNonEmpty least $"rangers fight in W17S26\n{failure}"

                Expect.isGreaterThanOrEqual
                    (List.min least)
                    1_100
                    $"they go in together: none trades its hits away alone\n{failure}"
            }
        ]

/// Two rangers of ours and Trepidimous' empty 23C23M hauler standing still
/// in W17S26, eleven tiles off the container: the trespass's ground is the
/// eight tiles beside it, a small ground two holders share (#650).
let private standingHauler () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let outpost =
        room "W17S26"
        |> withStructures [ structureOf "container" None 123_000 250_000 { X = 27; Y = 9 } ]

    let home =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800

    let declared =
        { colony "W17S25" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    let ranger id tile =
        body id Side.Ours (parts [ Move, 6; RangedAttack, 4; Heal, 2 ]) tile None

    arena
        900_000
        [ home; outpost ]
        [ declared ]
        [
            ranger "ranger-1-Spawn11" (at "W17S26" 26 13)
            ranger "ranger-2-Spawn11" (at "W17S26" 28 13)
            body
                "Wolf1"
                (Side.Player "Trepidimous")
                (List.concat (List.replicate 23 [ Move; Carry ]))
                (at "W17S26" 30 20)
                None
        ]

[<Tests>]
let sharedGroundTests =
    testList
        "arena two rangers on a small ground (#650)"
        [
            // A partner shoved off its tile takes the mover's only inside its
            // own Work Area, so two holders seated on one small ground never
            // trade tiles back and forth, paying fatigue for nothing.
            test "neither ranger flips between two tiles tick to tick once both stand on the ground" {
                let _, trace = standingHauler () |> run 30
                let failure = describe trace

                for ranger in [ "ranger-1-Spawn11"; "ranger-2-Spawn11" ] do
                    let path = pathOf ranger trace |> Map.ofList

                    Expect.equal
                        (Map.tryFind 29 path
                         |> Option.bind (fun body ->
                             RoomPos.range body.At (RoomPos.at "W17S26" { X = 30; Y = 20 })))
                        (Some 1)
                        $"the premise: {ranger} stands on the ground beside Wolf1\n{failure}"

                    let flips =
                        [ 2..29 ]
                        |> List.filter (fun tick ->
                            match
                                Map.tryFind (tick - 2) path,
                                Map.tryFind (tick - 1) path,
                                Map.tryFind tick path
                            with
                            | Some a, Some b, Some c -> a.At = c.At && a.At <> b.At
                            | _ -> false)

                    Expect.isEmpty
                        flips
                        $"{ranger} steps back onto the tile it left the tick before\n{failure}"
            }
        ]
