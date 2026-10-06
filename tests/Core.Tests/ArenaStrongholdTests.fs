/// An Invader stronghold in the arena (#565): `stronghold.js`' bunker2 run by
/// the engine's rules, against the W16S26 captures where two of W15S28's
/// rangers died under its towers (t980,650 and t980,759, #566).
module Fabot.Core.Tests.ArenaStrongholdTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Arena

let private at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

let private invader = Side.Npc "Invader"

let private core = "invaderCore-8-36"

/// W16S26's four Source Keepers as the replay holds them: 17 TOUGH, 13 MOVE
/// and ten ATTACK and RANGED_ATTACK pairs, each at its lair (live: no act),
/// `ttl` left. The one at 14,43 stands nearer the core than any body of ours.
let private keepers (ttl: int) : Body list =
    [
        "Keeper6a8caa89dd4872bccd318e3e", 4, 6
        "Keeper6a8caa89dd4872bccd318e3f", 42, 8
        "Keeper6a8caa89dd4872bccd318e40", 36, 38
        "Keeper6a8caa89dd4872bccd318e42", 14, 43
    ]
    |> List.map (fun (id, x, y) ->
        { body
              id
              (Side.Npc "Source Keeper")
              (parts [ Tough, 17; Move, 13 ]
               @ List.concat (List.replicate 10 [ Attack; RangedAttack ]))
              (at "W16S26" x y)
              (Some Hold) with
            TicksToLive = ttl
        })

/// The bunker2's one defender, `weakDefender` 15A 15M, where it stood all
/// through both fights: on the rampart at 8,35, above the core.
let private defender (ttl: int) : Body =
    { body "defender0" invader (parts [ Attack, 15; Move, 15 ]) (at "W16S26" 8 35) (Some Defend) with
        TicksToLive = ttl
    }

/// W16S26's bunker2 as the replay holds it (deployed t977,731, collapsing
/// t1,045,681): the core at 8,36, towers at 7,35 and 9,37 holding `west` and
/// `east` energy, nine 200k ramparts, two containers and six roads.
let private bunker2 (west: int) (east: int) : ArenaRoom =
    room "W16S26"
    |> withStructures
        [
            for x, y in [ 7, 35; 7, 37; 8, 35; 8, 36; 8, 37; 9, 35; 9, 36; 9, 37 ] ->
                { structureOf "rampart" (Some invader) 200_000 300_000_000 { X = x; Y = y } with
                    NextDecay = 1_045_681
                }
        ]
    |> withStructures
        [
            { structureOf "container" None 250_000 250_000 { X = 7; Y = 36 } with
                Thorium = 63
            }
            structureOf "container" None 250_000 250_000 { X = 9; Y = 36 }
            { structureOf "invaderCore" (Some invader) 100_000 100_000 { X = 8; Y = 36 } with
                Level = 2
            }
            { structureOf "rampart" (Some invader) 200_000 300_000_000 { X = 7; Y = 36 } with
                NextDecay = 1_043_103
            }
            towerOf invader { X = 7; Y = 35 } west
            towerOf invader { X = 9; Y = 37 } east
        ]
    |> withRoads
        [
            7, 36, 5_000
            7, 37, 5_000
            8, 35, 5_000
            8, 37, 5_000
            9, 35, 5_000
            9, 36, 5_000
        ]
    |> withTowerDuties [ FocusCore ]
    |> withStronghold 1_045_681 None

/// One of W15S28's resident rangers, 21M 14R 7H, as both stood one tile in
/// from W16S26's south exit after their first step (live: 15 MOVE, 14
/// RANGED_ATTACK, 7 HEAL acting).
let private ranger (id: string) (ttl: int) (script: Behaviour option) : Body =
    { body id Side.Ours (parts [ Move, 21; RangedAttack, 14; Heal, 7 ]) (at "W16S26" 26 47) script with
        Hits = 3_564
        TicksToLive = ttl
        Fatigue = 6
    }

/// The live ranger's tiles over its last ten ticks: one step to 25,46 and
/// held there, healing itself (`observe history W16S26`).
let private liveTiles = Some(Waypoints [ at "W16S26" 25 46 ])

/// A body's hits at the end of each tick it lived through.
let private hitsOf (id: string) (trace: TickTrace list) : int list =
    pathOf id trace |> List.map (fun (_, b) -> b.Hits)

/// The energy a tower of the stronghold holds at the end of a run.
let private towerEnergy (a: Arena) (x: int) (y: int) : int =
    a.Rooms["W16S26"].Structures
    |> List.find (fun s -> s.Kind = "tower" && s.At = { X = x; Y = y })
    |> fun s -> s.Energy

// ---- arena capture w16s26-t980640 --------------------------------------------
// `npm run capture -- W16S26 980640 --rooms W15S28 --no-live-memory`, read at live t990,242.
//   W16S26  https://screeps.com/room-history/shardSeason/W16S26/980600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/980600.json
// W16S27, the room the ranger came in from, has no replay (404). Spawn3 was
// spawning worker-980588-Spawn3 and Spawn8 ranger-980534-Spawn8: not placed.
// The second ranger is `npm run capture -- W16S26 980749`: the same room
// but for the towers' stores and every clock.

/// The bunker2 at t980,640 and W15S28's resident ranger at 26,47, with its
/// home and colony, our pipeline deciding it: no Transition log entry is
/// left that early, so it starts with no Task.
let private w16s26T980640 () : Arena =
    let w16s26 = bunker2 980 870

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 148_527 5_857_785
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
                        16, 34
                        17, 25
                        17, 27
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
                for x, y in
                    [ 16, 32; 17, 33; 18, 34; 19, 33; 20, 32; 21, 31; 21, 33; 21, 35; 23, 35 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 167_100 250_000 { X = 6; Y = 29 } with
                    Energy = 976
                }
                { structureOf "container" None 135_100 250_000 { X = 11; Y = 18 } with
                    Energy = 2_000
                }
                structureOf "container" None 137_300 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 980_660
                }
                { structureOf "rampart" (Some Side.Ours) 99_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 980_719
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 980_678
                }
                { structureOf "rampart" (Some Side.Ours) 94_901 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 980_644
                }
                { structureOf "rampart" (Some Side.Ours) 96_201 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 980_720
                }
                { structureOf "rampart" (Some Side.Ours) 95_701 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 980_674
                }
                { structureOf "rampart" (Some Side.Ours) 96_801 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 980_695
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 980_644
                }
                { structureOf "rampart" (Some Side.Ours) 98_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 980_736
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 13_877
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 2_400
                5, 4, 2_800
                5, 6, 2_400
                5, 44, 3_200
                6, 3, 4_400
                6, 7, 3_300
                6, 29, 4_500
                6, 43, 2_800
                6, 45, 3_600
                7, 2, 3_300
                7, 8, 2_400
                7, 28, 3_700
                7, 42, 2_300
                7, 46, 2_800
                8, 1, 2_400
                8, 9, 4_800
                8, 28, 2_800
                8, 41, 2_800
                8, 47, 4_700
                9, 10, 3_300
                9, 27, 3_300
                9, 28, 3_500
                9, 40, 2_300
                9, 48, 3_500
                10, 11, 2_300
                10, 26, 3_700
                10, 29, 3_100
                10, 39, 2_800
                11, 12, 4_500
                11, 18, 2_500
                11, 27, 4_900
                11, 30, 3_400
                11, 38, 3_400
                12, 13, 2_900
                12, 17, 3_200
                12, 25, 16_000
                12, 27, 14_300
                12, 28, 3_200
                12, 29, 3_200
                12, 31, 4_700
                12, 33, 14_600
                12, 35, 18_900
                12, 37, 2_800
                13, 14, 2_900
                13, 16, 3_300
                13, 26, 19_700
                13, 28, 4_600
                13, 30, 3_700
                13, 32, 4_300
                13, 34, 3_000
                13, 36, 3_100
                14, 15, 4_800
                14, 25, 19_100
                14, 27, 4_900
                14, 29, 2_700
                14, 31, 3_100
                14, 33, 4_100
                14, 35, 3_100
                15, 15, 4_700
                15, 26, 4_500
                15, 28, 3_400
                15, 30, 2_800
                15, 32, 4_500
                15, 34, 3_000
                16, 16, 4_700
                16, 25, 13_400
                16, 27, 2_300
                16, 29, 4_900
                16, 31, 3_300
                16, 33, 2_900
                16, 35, 4_700
                17, 17, 3_000
                17, 26, 16_200
                17, 28, 2_700
                17, 30, 4_400
                17, 32, 3_900
                17, 34, 4_300
                18, 18, 2_600
                18, 25, 16_500
                18, 27, 15_300
                18, 29, 3_500
                18, 31, 3_100
                18, 33, 3_200
                19, 19, 14_500
                19, 22, 13_900
                19, 24, 19_400
                19, 26, 12_900
                19, 28, 2_500
                19, 30, 3_100
                19, 32, 2_500
                20, 20, 13_200
                20, 21, 16_900
                20, 23, 15_600
                20, 25, 15_700
                20, 27, 13_000
                20, 29, 2_800
                20, 31, 3_200
                20, 33, 20_600
                21, 21, 2_800
                21, 22, 4_000
                21, 23, 3_900
                21, 24, 11_900
                21, 25, 12_900
                21, 26, 18_300
                21, 28, 16_400
                21, 30, 3_500
                21, 32, 12_800
                21, 34, 20_700
                22, 24, 3_300
                22, 25, 16_700
                22, 27, 12_100
                22, 28, 3_400
                22, 29, 2_800
                22, 30, 20_000
                22, 31, 14_700
                22, 32, 15_500
                22, 33, 15_200
                22, 35, 2_800
                23, 26, 19_700
                23, 28, 11_700
                23, 29, 15_600
                23, 30, 16_900
                23, 31, 17_000
                23, 32, 15_300
                23, 33, 18_700
                23, 34, 3_300
                24, 25, 17_000
                24, 28, 16_400
                24, 29, 17_000
                24, 30, 15_900
                24, 33, 20_500
                25, 24, 3_700
                25, 28, 16_600
                25, 29, 16_200
                25, 34, 17_300
                26, 23, 19_400
                27, 22, 16_600
                28, 21, 16_300
                28, 34, 16_600
                29, 20, 18_100
                30, 19, 16_500
                31, 18, 2_900
                32, 17, 3_400
                33, 16, 4_700
                34, 15, 2_700
                35, 14, 3_500
                36, 13, 11_400
                37, 12, 3_700
                38, 11, 2_200
                39, 12, 2_900
                40, 13, 17_800
                41, 14, 16_100
                42, 15, 4_400
                43, 16, 4_400
                44, 17, 2_800
                45, 18, 2_800
                46, 19, 2_800
                47, 20, 3_000
                48, 21, 2_200
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W15S28" ])

    let bodies: Body list =
        [
            defender 65_040
            yield! keepers 1_036
            ranger "ranger-980431-Spawn3" 1_415 None
            { body
                  "guard-979116-Spawn8"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 6 1)
                  None with
                TicksToLive = 4
            }
            { body
                  "hauler-979281-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 15)
                  None with
                TicksToLive = 283
            }
            { body
                  "hauler-979481-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 15 34)
                  None with
                TicksToLive = 483
                Energy = 733
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 513
                Energy = 50
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 764
                Energy = 50
            }
            { body
                  "hauler-979425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 32)
                  None with
                TicksToLive = 427
                Energy = 870
            }
        ]

    arena 980_641 [ w16s26; w15s28 ] colonies bodies

/// The bunker2 alone with a hostile of Trepidimous's, no keeper and no
/// colony: the stronghold's own rules, a tick from t980,641.
let private siege (r: ArenaRoom) (bodies: Body list) : Arena = arena 980_641 [ r ] [] bodies

let private trep = Side.Player "Trepidimous"

/// A hostile that only stands: 10 TOUGH, 10 MOVE.
let private dummy (x: int) (y: int) : Body =
    body "dummy" trep (parts [ Tough, 10; Move, 10 ]) (at "W16S26" x y) (Some(Do []))

/// The tiles a bunker2 defender may stand on: its ramparts but the towers'
/// and the core's.
let private walk =
    set [ for x, y in [ 7, 36; 7, 37; 8, 35; 8, 37; 9, 35; 9, 36 ] -> at "W16S26" x y ]

[<Tests>]
let strongholdTests =
    testList
        "arena stronghold"
        [
            // The live fight, the ranger walked on its live tiles: both towers
            // on it every tick (480 at 25,46, 420 at 26,47 the first two, its
            // 84 heal off), never on the keeper at 14,43 though it stands
            // nearer the core, and the defender held on its rampart.
            for slug, time, west, east, id, life, keeperLife, defenderLife in
                [
                    "t980,640", 980_641, 980, 870, "ranger-980431-Spawn3", 1_415, 1_036, 65_040
                    "t980,749", 980_750, 860, 750, "ranger-980534-Spawn8", 1_409, 927, 64_931
                ] do
                test
                    $"capture {slug}: the towers kill the ranger on the live ticks and spend the live energy" {
                    let start =
                        arena
                            time
                            [ bunker2 west east ]
                            []
                            ([ defender defenderLife ]
                             @ keepers keeperLife
                             @ [ ranger id life liveTiles ])

                    let final, trace = start |> run 12

                    Expect.equal
                        (hitsOf id trace)
                        [ 3_228; 2_892; 2_496; 2_100; 1_704; 1_308; 912; 516; 108 ]
                        $"the live hits a tick\n{describe trace}"

                    Expect.equal (diedOn id trace) (Some 9) "dead on the live tick, the tenth"

                    Expect.equal
                        (towerEnergy final 7 35, towerEnergy final 9 37)
                        (west - 100, east - 100)
                        "ten shots each, the death tick's among them, and none after"

                    for k in keepers keeperLife do
                        Expect.isTrue
                            (pathOf k.Id trace |> List.forall (fun (_, b) -> b.Hits = 5_000))
                            $"{k.Id} draws no fire"

                    Expect.isTrue
                        (pathOf "defender0" trace
                         |> List.forall (fun (_, b) -> b.At = at "W16S26" 8 35))
                        "the defender holds: no rampart tile stands beside the ranger"
                }

            // Characterisation (movement S0, `docs/adr/0088`): a ranger losing
            // 336 a tick to the towers over its 84 heal stands and heals until
            // it dies (#566). Live it stepped once more, to 25,46, and died a
            // tick sooner (t980,650): its Task is gone from the Transition log,
            // and the code then routed it in, which 8b4a313f has since barred.
            // Not flipped at movement S2, which lays the towers' reach and
            // walks a body out over the exit by it (#566, ThreatTests' "a
            // ranger under a stronghold's towers"): since 8b4a313f no chain
            // enters a stronghold's room, so W15S28 does not project W16S26
            // and the ranger standing there is placed by nothing — no Task,
            // no Flee, no step. Only a body that stood there before that
            // barrier can be in this state.
            test
                "Characterisation (movement S0): the ranger in a stronghold room no colony projects stands under its towers until it dies" {
                let _, trace = w16s26T980640 () |> run 12
                let id = "ranger-980431-Spawn3"

                Expect.equal
                    (hitsOf id trace)
                    [ 3_228; 2_892; 2_556; 2_220; 1_884; 1_548; 1_212; 876; 540; 192 ]
                    $"336 a tick net, healing all the way\n{describe trace}"

                Expect.equal (diedOn id trace) (Some 10) "dead at t980,651"

                Expect.isTrue
                    (pathOf id trace |> List.forall (fun (_, b) -> b.At = at "W16S26" 26 47))
                    "it never steps back over the exit one tile away"
            }

            test "the defender walks only its ramparts and swings at a hostile beside them" {
                let _, trace = siege (bunker2 1_000 1_000) [ defender 65_040; dummy 10 34 ] |> run 3

                Expect.contains
                    (trace[0].Theirs)
                    ("defender0", Act.Move Right)
                    "onto 9,35, the rampart beside the hostile"

                Expect.isTrue
                    (pathOf "defender0" trace |> List.forall (fun (_, b) -> Set.contains b.At walk))
                    $"never off the ramparts\n{describe trace}"

                Expect.contains
                    (trace[1].Theirs)
                    ("defender0", Act.Attack "dummy")
                    "beside it, it swings"
            }

            test "the defender stands still when no rampart tile is beside a hostile" {
                let _, trace = siege (bunker2 1_000 1_000) [ defender 65_040; dummy 12 36 ] |> run 3

                Expect.isEmpty
                    (trace
                     |> List.collect (fun t -> t.Theirs)
                     |> List.filter (fun (id, _) -> id = "defender0"))
                    "no act"
            }

            test
                "a dead defender is respawned by the core 180 ticks after its death, onto the rampart above it" {
                let _, trace = siege (bunker2 1_000 1_000) [ defender 1 ] |> run 181

                Expect.equal (diedOn "defender0" trace) (Some 0) "its life ran out"

                Expect.equal
                    (trace
                     |> List.tryFind (fun t -> List.contains (Born "defender0") t.Events)
                     |> Option.map _.Tick)
                    (Some 180)
                    "born at INVADER_CORE_CREEP_SPAWN_TIME 6 a part, 30 parts, on"

                Expect.equal
                    (standingAt 180 trace
                     |> List.tryFind (fun b -> b.Id = "defender0")
                     |> Option.map _.At)
                    (Some(at "W16S26" 8 35))
                    "TOP first: the rampart above the core"
            }

            test
                "the core refills its emptiest tower at 20 energy or less from nowhere, so a drain never empties it" {
                // t980,650 and t980,660 are the arena's refill ticks at L2.
                let after10, _ = siege (bunker2 30 10) [] |> run 10
                let after20, _ = siege (bunker2 20 10) [] |> run 20

                Expect.equal (towerEnergy after10 9 37) 1_000 "the emptiest, topped up"
                Expect.equal (towerEnergy after10 7 35) 30 "above 20: left"

                Expect.equal
                    (towerEnergy after20 7 35, towerEnergy after20 9 37)
                    (1_000, 1_000)
                    "one tower a refill tick"
            }

            test "the core's fall leaves its loot and silences the towers" {
                let open' =
                    bunker2 1_000 1_000
                    |> fun r ->
                        { r with
                            Structures =
                                r.Structures
                                |> List.choose (fun s ->
                                    if s.Id = "rampart-8-36" then None
                                    elif s.Id = core then Some { s with Hits = 10 }
                                    else Some s)
                        }

                let shooter =
                    body
                        "shooter"
                        trep
                        (parts [ RangedAttack, 1; Tough, 49 ])
                        (at "W16S26" 8 39)
                        (Some(Do [ Act.RangedAttack core ]))

                let final, trace = siege open' [ shooter ] |> run 4

                Expect.containsAll
                    trace[0].Events
                    [ Destroyed core; CoreLooted(core, 16_000) ]
                    "the core falls on its 10 hits, its ruin worth an L2's density"

                Expect.equal
                    (hitsOf "shooter" trace)
                    [ 3_800; 3_800; 3_800; 3_800 ]
                    "both towers fire the tick it falls, its pretick already run, and never again"

                Expect.equal
                    (final.Rooms["W16S26"].Stronghold |> Option.bind _.Spawning)
                    None
                    "no defender is spawned"
            }

            test "the collapse takes every structure of the stronghold and its defender" {
                let r = bunker2 1_000 1_000 |> withStronghold 980_644 None
                let final, trace = siege r [ defender 3 ] |> run 5

                Expect.contains trace[3].Events (Collapsed "W16S26") "at its collapse tick"

                Expect.isEmpty
                    final.Rooms["W16S26"].Structures
                    "core, towers, ramparts, containers and roads"

                Expect.isSome (diedOn "defender0" trace) "its life ran to the collapse"
            }
        ]
