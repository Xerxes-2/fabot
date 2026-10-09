/// The arena's whole-fight scenarios: the W17S25 siege, Odiodin's replay, the
/// storage flip and the W11S28 invasion, played through our live fight code.
module Fabot.Core.Tests.ArenaScenarioTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

/// The W17S25 siege at t880,341 (§1.1): the parked 2×2 block beside the
/// controller at 15,36 with the tapper on it, against what we field. W17S25
/// is a nursery raised by W17S26 here — live its mother was W15S28, five
/// crossings off; the arena's mother is the neighbour so one seam joins them,
/// which moves nothing inside W17S25.
let private besieged (raid: Body list) (ours: Body list) =
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

    arena 880_341 [ mother; child ] colonies (ours @ raid)

let private siege (ours: Body list) =
    let controller = w17s25 15 36

    let raid =
        [
            body
                "Eternity536"
                trep
                trepMelee
                (w17s25 16 36)
                (Some(Chase(Nearest, Some(w17s25 16 36, 4))))
            body
                "Prime803"
                trep
                trepMelee
                (w17s25 17 37)
                (Some(Chase(Nearest, Some(w17s25 17 37, 4))))
            body "Prism305" trep trepHealer (w17s25 16 35) (Some(Follow "Eternity536"))
            body "Paragon722" trep trepHealer (w17s25 17 36) (Some(Follow "Prime803"))
            body "Rune908" trep trepTapper (w17s25 16 37) (Some(Tap controller))
        ]

    besieged raid ours

let private w17s22 (x: int) (y: int) = RoomPos.at "W17S22" { X = x; Y = y }

/// W17S22 alone, Odiodin's room at RCL2 (§1.5), and no colony of ours: both
/// sides scripted, the replay's own numbers checked against the physics.
let private replay (bodies: Body list) =
    let r = room "W17S22" |> withController Ownership.Rival (Some "Odiodin") 2 0

    arena 878_959 [ r ] [] bodies

/// The hits one body ends each tick on.
let private hitsOf (id: string) (trace: TickTrace list) =
    pathOf id trace |> List.map (fun (_, s) -> s.Hits)

/// The ticks a body of ours ended within one of any of these melee bodies,
/// each with the melee beside it.
let private caughtBeside (melee: string list) (ours: string) (trace: TickTrace list) =
    trace
    |> List.choose (fun t ->
        match t.Bodies |> List.tryFind (fun b -> b.Id = ours) with
        | Some me ->
            let beside =
                t.Bodies
                |> List.filter (fun b ->
                    List.contains b.Id melee
                    && RoomPos.range me.At b.At |> Option.exists (fun r -> r <= 1))
                |> List.map (fun b -> b.Id)

            if List.isEmpty beside then None else Some(t.Tick, beside)
        | None -> None)

/// The raid's bodies, by id.
let private raidIds =
    [ "Eternity536"; "Prime803"; "Prism305"; "Paragon722"; "Rune908" ]

/// `npm run capture -- W12S28 972170 --name storageFlip`: W12S28 at t972,170,
/// in the middle of #530's flip (hauler-970894/970938-Spawn6 drew the Storage
/// and poured it back eight times over t972,169-185). Roads (130) are not
/// placed; every body starts idle, the Transition log holding nothing that
/// early.
let private storageFlip () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withControllerClocks 0 0 149_982 10_696_734
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        7, 37
                        8, 34
                        8, 36
                        9, 33
                        9, 35
                        9, 37
                        9, 39
                        9, 41
                        10, 34
                        10, 38
                        10, 42
                        11, 33
                        11, 37
                        11, 39
                        11, 41
                        12, 34
                        12, 38
                        12, 42
                        12, 44
                        13, 33
                        13, 35
                        13, 37
                        13, 39
                        13, 41
                        13, 43
                        14, 34
                        14, 38
                        14, 40
                        14, 44
                        15, 33
                        15, 35
                        15, 37
                        15, 39
                        15, 43
                        15, 45
                        16, 34
                        16, 38
                        17, 33
                        17, 35
                        17, 37
                        17, 43
                        17, 45
                        18, 42
                        18, 44
                        18, 46
                        19, 33
                        19, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 133_100 250_000 { X = 7; Y = 39 }
                { structureOf "container" None 136_500 250_000 { X = 10; Y = 43 } with
                    Energy = 576
                }
                { structureOf "container" None 158_200 250_000 { X = 16; Y = 39 } with
                    Energy = 348
                }
                structureOf "container" None 180_300 250_000 { X = 25; Y = 6 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 14; Y = 36 } with
                    Energy = 88
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 36 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 44 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 26; Y = 5 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 30_569
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 11; Y = 43 } with
                    Energy = 4_000
                }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W12S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-970554-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 12 43)
                  None with
                TicksToLive = 32
                Energy = 450
            }
            { body
                  "worker-970746-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 13 44)
                  None with
                TicksToLive = 224
                Energy = 236
            }
            { body
                  "hauler-970894-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 13 42)
                  None with
                TicksToLive = 366
            }
            { body
                  "hauler-970938-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 43 25)
                  None with
                TicksToLive = 410
            }
            { body
                  "upgrader-971328-Spawn1"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W12S28" 5 37)
                  None with
                TicksToLive = 803
            }
            { body
                  "anchor-971374-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W12S28" 10 43)
                  None with
                TicksToLive = 732
                Energy = 50
            }
            { body
                  "anchor-971427-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W12S28" 16 39)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "hauler-970592-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 14 41)
                  None with
                TicksToLive = 64
            }
        ]

    arena 972_171 [ w12s28 ] colonies bodies

/// Each body's draws from a store and pours back into it with nothing poured
/// anywhere else between: the flip #530 is about, as (body, store, tick) rows.
let private pouredBack (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Ours
        |> List.choose (function
            | WithdrawFromStore(creep, store, Energy, _) -> Some(creep, (t.Tick, Choice1Of2 store))
            | TransferEnergyToStructure(creep, target, Energy) ->
                Some(creep, (t.Tick, Choice2Of2 target))
            | _ -> None))
    |> List.groupBy fst
    |> List.collect (fun (creep, acts) ->
        acts
        |> List.map snd
        |> List.pairwise
        |> List.choose (function
            | (_, Choice1Of2 drawn), (tick, Choice2Of2 poured) when drawn = poured ->
                Some(creep, drawn, tick)
            | _ -> None))

// ---- arena capture w11s28-invasion (#535) -----------------------------------
// `npm run capture -- W11S28 973670 --rooms W12S28 --name w11s28Invasion`.
// The room-history replay at t973,670, the state after that tick ran: arena
// tick 0 runs as t973,671.
//   W11S28  https://screeps.com/room-history/shardSeason/W11S28/973600.json
//   W12S28  https://screeps.com/room-history/shardSeason/W12S28/973600.json
// Left out, as the capture says: Healers, ExitWatches and Sightings start
// empty; W11S28's reservation, roads and drops are not placed; two invaders'
// boosts are not placed. W12S28 is cut to what the fight reads: its
// spawns, towers and storage. The two guards hold no verdict that early in
// the Transition log, so they start unassigned.

/// W11S28 at t973,670 (#535's "Why"): an NPC invasion — two 2T5M3R shooters,
/// each followed by a 5M5H healer, and a small mixed body — answered by two
/// 30-part melee guards cast at W12S28, one at the east exit and one by the
/// spawns. Live, the first fought alone, had its MOVE shot off and sat rooted.
/// All five run the engine's invader AI (`Invader`, #541): the shooters flee
/// a guard nearer than 3 and shoot the lowest hits, the healers go to the
/// hurt, the small one walks in.
let private w11s28Invasion () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w11s28 =
        room "W11S28"
        |> withStructures [ structureOf "container" None 120_100 250_000 { X = 32; Y = 14 } ]

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withControllerClocks 0 0 149_922 10_730_254
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
        |> withStructures
            [
                for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 29_037
                }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W12S28" ])

    let invader = Side.Npc "Invader"
    let shooter = parts [ Tough, 2; Move, 4; RangedAttack, 3; Move, 1 ]
    let healer = parts [ Move, 4; Heal, 5; Move, 1 ]
    let guard = parts [ Tough, 3; Move, 15; Attack, 9; Heal, 3 ]

    let bodies: Body list =
        [
            // live: healed→12,19; then closes and dies to the first guard.
            { body
                  "invader_W11S28_735"
                  invader
                  (parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ])
                  (at "W11S28" 10 22)
                  (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_768" invader shooter (at "W11S28" 11 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_779" invader shooter (at "W11S28" 10 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_495" invader healer (at "W11S28" 12 18) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_657" invader healer (at "W11S28" 12 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "guard-973545-Spawn1" Side.Ours guard (at "W12S28" 47 33) None with
                TicksToLive = 1_463
            }
            { body "guard-973545-Spawn6" Side.Ours guard (at "W12S28" 19 39) None with
                TicksToLive = 1_463
            }
            // The economy is staffed live; one hauler of the capture's stands
            // for it, so no supply floor jumps the fighting rows' queue.
            { body
                  "hauler-972391-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 17 34)
                  None with
                TicksToLive = 363
            }
        ]

    let a = arena 973_671 [ w11s28; w12s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Towered = Set.ofList [ "W12S28" ]
                // Nothing of ours stands in W11S28 at the tick: live, the
                // raid was remembered (the threatened latch the guards were
                // cast on at t973,545), not seen.
                Raids =
                    Map.ofList
                        [
                            "W12S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Threatened =
                                    Map.ofList
                                        [
                                            "W11S28",
                                            {
                                                Until = 973_545 + Engine.creepLifetime
                                            }
                                        ]
                            }
                        ]
            }
    }

[<Tests>]
let arenaScenarioTests =
    testList
        "arena scenarios"
        [
            // #530 capture t972,170: live, W12S28's haulers drew the Storage for
            // sinks other loads already covered and poured the energy straight
            // back, every tick or two.
            test "#530 capture t972,170: no hauler pours a Storage draw back into the Storage" {
                let _, trace = storageFlip () |> run 40

                Expect.isEmpty
                    (pouredBack trace)
                    $"a load goes on to a sink, never back where it came from\n{describe trace}"
            }

            // Reproduced live before #535 (main feaee027, no casts): both guards
            // took the Guard and the first went in alone at t6, swung the
            // small mixed body down and stood kept off by the shooters, the
            // second in at t32. #535 read penned Invaders as in a melee's
            // reach and the two guards ran the shooters into the walls. #568:
            // an Invader shooter flees every hostile inside three, so penned
            // is no reach (spar's lone guard kited to death by one), and the
            // room is the ranger row's as a player's shooters are
            // (`engagementTests`). The guards kill the mixed body at t9 and
            // are sent no further; the ranger cast at t1 clears the rest by
            // t198 (768, 779, 495, 657 at t184/189/198/198).
            test
                "#535 capture t973,670: the W11S28 invasion's shooters keep off a melee guard, so a ranger is cast for them" {
                let start = w11s28Invasion () |> withCasts [ "guard"; "ranger" ]
                let _, trace = start |> run 300
                crossValidate "w11s28-invasion" start trace
                let failure = describe trace

                let born =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | Born id -> Some id
                            | _ -> None))

                Expect.isNonEmpty born $"a ranger is cast\n{failure}"

                Expect.all
                    born
                    (fun id -> id.StartsWith "ranger-")
                    $"and nothing but rangers\n{failure}"

                for invader in
                    [
                        "invader_W11S28_735"
                        "invader_W11S28_768"
                        "invader_W11S28_779"
                        "invader_W11S28_495"
                        "invader_W11S28_657"
                    ] do
                    Expect.isSome (diedOn invader trace) $"{invader} dies\n{failure}"

                for guard in [ "guard-973545-Spawn1"; "guard-973545-Spawn6" ] do
                    Expect.isTrue (survived guard trace) $"{guard} lives\n{failure}"
            }

            // #535 invariant 3, the melee's rule: attacks resolve before moves,
            // so a guard swinging at the body beside it steps onto that body's
            // tile too. The move lands when the body steps away and fails
            // harmlessly when it stays.
            test
                "a melee guard beside a raider that steps away ends each tick beside it, and beside one that stays it stands and swings" {
                let world (behaviour: Behaviour) =
                    let home =
                        room "W12S28"
                        |> withController Ownership.Ours None 7 0
                        |> withSpawn "Spawn1" { X = 12; Y = 38 } 5600

                    let outpost = room "W12S27"

                    let declared =
                        { colony "W12S28" with
                            Outposts = [ outpostOf outpost.Capture ]
                        }

                    let guard =
                        body
                            "guard-1-Spawn1"
                            Side.Ours
                            (ourGuard 1)
                            (RoomPos.at "W12S27" { X = 19; Y = 25 })
                            None

                    let raider =
                        body
                            "invader-1"
                            (Side.Npc "Invader")
                            Fabot.Core.Tests.Decide.Fixtures.smallMelee
                            (RoomPos.at "W12S27" { X = 20; Y = 25 })
                            (Some behaviour)

                    arena 900_000 [ home; outpost ] [ declared ] [ guard; raider ]

                let _, fleeing = world (Do [ Act.Move Right ]) |> run 6
                let failure = describe fleeing

                for (tick, raider) in pathOf "invader-1" fleeing do
                    let guard =
                        pathOf "guard-1-Spawn1" fleeing |> List.find (fst >> (=) tick) |> snd

                    Expect.equal
                        (RoomPos.range guard.At raider.At)
                        (Some 1)
                        $"t{tick}: beside it again\n{failure}"

                Expect.isLessThan
                    (pathOf "invader-1" fleeing |> List.last |> snd |> (fun s -> s.Hits))
                    (Engine.partHits * List.length Fabot.Core.Tests.Decide.Fixtures.smallMelee)
                    $"and struck as it goes\n{failure}"

                let _, staying = world Hold |> run 4
                let failure = describe staying

                Expect.equal
                    (pathOf "guard-1-Spawn1" staying
                     |> List.map (fun (_, s) -> s.At)
                     |> List.distinct)
                    [ RoomPos.at "W12S27" { X = 19; Y = 25 } ]
                    $"beside one that stays the step fails and it stands\n{failure}"

                // Its swing and the strike-back its ATTACK lands on the
                // raider's own swing, every tick.
                Expect.equal
                    (pathOf "invader-1" staying |> List.map (fun (_, s) -> s.Hits))
                    [ 820; 640; 460; 280 ]
                    $"and swings every tick\n{failure}"
            }
            // The Brief's S7, rule 1 (a hostile no heal reaches is read live): the
            // guard swings the invader down to its last part, a MOVE, unarmed,
            // and its room's Guard as a raid's goes. #535 flips what S7 left:
            // the fight it began is fought to the death, so it kills it.
            test "an outpost guard walks in, disarms a smallMelee invader and fights on to kill it" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

                let _, trace = outpostRaid invader |> run 40

                Expect.isSome (diedOn "invader-1" trace) $"the invader dies\n{describe trace}"

                Expect.isTrue (survived "guard-1-Spawn1" trace) "the guard lives"
            }

            // #510, the W18S25 shape: Torque404 left at 1,454 hits, 14 ATTACK on
            // one MOVE under the whole body's weight, against an 800 bank whose
            // one guard block it out-swings, and one ranger of ours at home.
            test "a ranger walks out and kills a crippled melee from range, untouched" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 800

                let outpost = room "W12S27"

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let ranger =
                    body
                        "ranger-1-Spawn1"
                        Side.Ours
                        (ourRanger 1)
                        (RoomPos.at "W12S28" { X = 20; Y = 5 })
                        None

                let hauler =
                    body
                        "hauler-1-Spawn1"
                        Side.Ours
                        (parts [ Carry, 2; Move, 2 ])
                        (RoomPos.at "W12S27" { X = 13; Y = 46 })
                        None

                let torque =
                    { body
                          "Torque404"
                          trep
                          (parts [ Move, 17; Attack, 3; Move, 1; Attack, 14 ])
                          (RoomPos.at "W12S27" { X = 8; Y = 48 })
                          (Some(Chase(Nearest, None))) with
                        Hits = 1_454
                    }

                let _, trace =
                    arena 900_000 [ home; outpost ] [ declared ] [ ranger; hauler; torque ]
                    |> run 150

                Expect.isSome
                    (diedOn "Torque404" trace)
                    $"the crippled melee dies\n{describe trace}"

                Expect.isTrue
                    (hitsOf "ranger-1-Spawn1" trace
                     |> List.forall (
                         (=) (
                             Engine.partHits
                             * List.length Fabot.Core.Decide.Bodies.rangerPattern.Block
                         )
                     ))
                    $"and the ranger never takes a swing\n{describe trace}"

                Expect.isEmpty
                    (caughtBeside [ "Torque404" ] "ranger-1-Spawn1" trace)
                    "nor ends a tick beside it"
            }

            // The user's chase (live 2026-10-07): a ranger chasing stepped onto
            // swamp, its fatigue held it, and two ticks later its target had
            // walked out of reach. Here a shooter walks east along a lane with
            // a two-row swamp band between it and the ranger, three rows off:
            // the plain lane beside the band keeps it in reach.
            test
                "a ranger chasing a shooter along a swamp band keeps to the plain lane and keeps it in reach" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 5_600

                // A field laid on W12S27's south-west floor: plain, but for
                // the swamp band on rows 29 and 30.
                let outpost =
                    let r = room "W12S27"

                    { r with
                        Capture =
                            { r.Capture with
                                Terrain =
                                    (r.Capture.Terrain,
                                     [
                                         for x in 5..30 do
                                             for y in 26..34 -> { X = x; Y = y }
                                     ])
                                    ||> List.fold (fun grid tile ->
                                        TerrainGrid.add
                                            tile
                                            (if tile.Y = 29 || tile.Y = 30 then Swamp else Plain)
                                            grid)
                            }
                    }

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let ranger =
                    body
                        "ranger-1-Spawn1"
                        Side.Ours
                        (ourRanger 5)
                        (RoomPos.at "W12S27" { X = 8; Y = 31 })
                        None

                let shooter =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallRanged
                        (RoomPos.at "W12S27" { X = 8; Y = 28 })
                        (Some(Do [ Act.Move Right ]))

                let _, trace =
                    arena 900_000 [ home; outpost ] [ declared ] [ ranger; shooter ] |> run 20

                let failure = describe trace

                // Main stepped onto the band at t0 and again at t5, each time
                // held five ticks while the shooter walked off: it was shot
                // twice and lived.
                Expect.isEmpty
                    (pathOf "invader-1" trace
                     |> List.choose (fun (tick, target) ->
                         pathOf "ranger-1-Spawn1" trace
                         |> List.tryFind (fun (at, ranger) ->
                             at = tick
                             && ranger.Fatigue > 0
                             && RoomPos.range ranger.At target.At
                                |> Option.exists (fun r -> r > Engine.rangedRange))
                         |> Option.map fst))
                    $"its fatigue never holds it while the shooter stands out of its reach\n{failure}"

                Expect.equal
                    (diedOn "invader-1" trace)
                    (Some 12)
                    $"and it shoots it dead\n{failure}"
            }

            // #571, the user's cripple nobody chased: an Invader shot down to
            // its ATTACK and one HEAL in an outpost the stand-down shuts, no
            // body of ours there, no ranger anywhere, an observer's look on
            // the room. The owner's guard row wins it, so that row casts the
            // guard while the room is shut, and the guard hunts it down.
            test
                "#571: a cripple in a shut outpost its owner's row wins is hunted by the body that row casts" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 5_600

                let outpost = room "W12S27"

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let lair = RoomPos.at "W12S27" { X = 20; Y = 30 }

                // 2 TOUGH, 4 MOVE, 4 ATTACK, 1 HEAL at 700 of 1,100 hits: the
                // TOUGH and two MOVE are gone.
                let cripple =
                    { body
                          "invader-cripple"
                          (Side.Npc "Invader")
                          (parts [ Tough, 2; Move, 4; Attack, 4; Heal, 1 ])
                          lair
                          (Some(Chase(Nearest, Some(lair, 4)))) with
                        Hits = 700
                        TicksToLive = 1_000
                    }

                // The economy is staffed: one hauler stands for it, so no
                // supply floor jumps the fighting row's queue.
                let hauler =
                    body
                        "hauler-1-Spawn1"
                        Side.Ours
                        (parts [ Carry, 32; Move, 16 ])
                        (RoomPos.at "W12S28" { X = 17; Y = 34 })
                        None

                let start =
                    let a =
                        arena 900_000 [ home; outpost ] [ declared ] [ cripple; hauler ]
                        |> withCasts [ "guard"; "ranger" ]

                    { a with
                        Lookout = Set.singleton "W12S27"
                        Carried =
                            { a.Carried with
                                Raids =
                                    Map.ofList
                                        [
                                            "W12S28",
                                            { Fabot.Core.Observe.RaidState.empty with
                                                Outposts =
                                                    [
                                                        {
                                                            Fabot.Core.Observe.OutpostEpisode.RoomName =
                                                                "W12S27"
                                                            Opened = 899_900
                                                            LastSeen = 899_999
                                                            Expiry = 901_000
                                                            Basis = StandDownBasis.InvaderRaid
                                                            Stronghold = false
                                                            Bounced = false
                                                            HolderUnread = false
                                                            Raiders =
                                                                Set.singleton "invader-cripple"
                                                            PlayerSince = None
                                                            Chased = false
                                                        }
                                                    ]
                                            }
                                        ]
                            }
                    }

                let _, trace = start |> run 300
                let failure = describe trace

                let born =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | Born id -> Some id
                            | _ -> None))

                // Moved by #654: the row cast a guard; the cheapest body that
                // ends the cripple is a one-block ranger, which kites it down
                // (its 20 a tick past the cripple's 12 of heal).
                Expect.exists
                    born
                    (fun id -> id.StartsWith "ranger-")
                    $"a ranger is cast\n{failure}"

                Expect.isSome (diedOn "invader-cripple" trace) $"and the cripple dies\n{failure}"
            }

            test
                "W17S22 replay: two garrisons on Genie669 beside Vibe199 take 188 a tick, the replay's own hits" {
                // t878,960: Genie669 (6M4H) at 672, healing itself, Vibe199
                // (11M7H) beside it healing it, two garrisons at range 2-3.
                let shoot target = Some(Do [ Act.RangedAttack target ])

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) (shoot "Genie669")
                        body "g2" odiodin garrison (w17s22 25 12) (shoot "Genie669")
                        { body
                              "Genie669"
                              trep
                              (parts [ Move, 6; Heal, 4 ])
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Genie669" ])) with
                            Hits = 672
                        }
                        body
                            "Vibe199"
                            trep
                            trepHealer
                            (w17s22 28 13)
                            (Some(Do [ Act.Heal "Genie669" ]))
                    ]

                let _, trace = replay bodies |> run 2
                Expect.equal (hitsOf "Genie669" trace) [ 484; 296 ] "672→484→296, t878,960–962"
            }

            test "W17S22 replay: Vibe199 alone under two garrisons loses 236 a tick" {
                let bodies =
                    [
                        body
                            "g1"
                            odiodin
                            garrison
                            (w17s22 24 12)
                            (Some(Do [ Act.RangedAttack "Vibe199" ]))
                        body
                            "g2"
                            odiodin
                            garrison
                            (w17s22 25 12)
                            (Some(Do [ Act.RangedAttack "Vibe199" ]))
                        { body
                              "Vibe199"
                              trep
                              trepHealer
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Vibe199" ])) with
                            Hits = 1640
                        }
                    ]

                let _, trace = replay bodies |> run 3

                Expect.equal
                    (hitsOf "Vibe199" trace)
                    [ 1404; 1168; 932 ]
                    "320 − 84 a tick, t878,966 on"
            }

            test "W17S23 replay: three garrisons on Failsafe204 take 396 a tick" {
                let shoot = Some(Do [ Act.RangedAttack "Failsafe204" ])

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) shoot
                        body "g2" odiodin garrison (w17s22 25 12) shoot
                        body "g3" odiodin garrison (w17s22 26 11) shoot
                        { body
                              "Failsafe204"
                              trep
                              trepHealer
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Failsafe204" ])) with
                            Hits = 1564
                        }
                    ]

                let _, trace = replay bodies |> run 3

                Expect.equal
                    (hitsOf "Failsafe204" trace)
                    [ 1168; 772; 376 ]
                    "1564→1168→772→376, t878,466–469"
            }

            test "W17S22 hold: three kiting garrisons, healers first, beat the melee squad" {
                let kite = Some(Kite(HealersFirst, 3))
                let controller = w17s22 30 14

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) kite
                        body "g2" odiodin garrison (w17s22 25 12) kite
                        body "g3" odiodin garrison (w17s22 26 10) kite
                        body "Torque592" trep trepMelee (w17s22 30 20) (Some(Chase(Nearest, None)))
                        body "Vibe199" trep trepHealer (w17s22 30 21) (Some(Follow "Torque592"))
                        body
                            "Genie669"
                            trep
                            (parts [ Move, 6; Heal, 4 ])
                            (w17s22 30 22)
                            (Some(Follow "Vibe199"))
                        body
                            "Quake773"
                            trep
                            (parts [ Move, 2; BodyPart.Claim, 2 ])
                            (w17s22 28 28)
                            (Some(Tap controller))
                    ]

                let _, trace = replay bodies |> run 40

                let died id =
                    diedOn id trace |> Option.defaultValue System.Int32.MaxValue

                Expect.isLessThan
                    (died "Vibe199")
                    (died "Torque592")
                    $"Vibe199 before Torque592\n{describe trace}"

                Expect.isLessThan (died "Genie669") (died "Torque592") "Genie669 before Torque592"
                Expect.isSome (diedOn "Torque592" trace) "the melee dies"

                for g in [ "g1"; "g2"; "g3" ] do
                    Expect.isTrue (survived g trace) $"{g} holds"
            }

            test
                "the W17S25 siege: one resident R7 cannot break the raid, and is never caught beside a melee" {
                let ranger = "ranger-879853-Spawn8"

                // Two starts: walking in from the mother, as the live body did,
                // and standing on the controller's ring when the block parks.
                for start in [ RoomPos.at "W17S26" { X = 10; Y = 2 }; w17s25 20 38 ] do
                    let _, trace = siege [ body ranger Side.Ours r7 start None ] |> run 60

                    // (a) The live outcome: nothing of the raid dies, the
                    // tapper lands on the controller. Since S0 (#451) the
                    // resident holds safe ground rather than dying.
                    for id in raidIds do
                        Expect.isTrue
                            (survived id trace)
                            $"{id} stands, from {start}\n{describe trace}"

                    Expect.isTrue
                        (trace
                         |> List.exists (fun t ->
                             t.Events |> List.contains (ControllerAttacked("W17S25", "Rune908"))))
                        "the tapper lands on the controller"

                    // (b) S0's kite ground: never a tick ended beside a melee.
                    Expect.isEmpty
                        (caughtBeside [ "Eternity536"; "Prime803" ] ranger trace)
                        $"caught beside a melee, from {start}\n{describe trace}"

                    Expect.isTrue (survived ranger trace) "the resident lives"
            }

        ]
