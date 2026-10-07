/// The light tick: what a quiet tick replays of the last full one, what it
/// walks, and the cadence of full decides it runs between.
module Fabot.Core.Tests.LightTickTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick
open Fabot.Core.Tests.LightTickFixtures

[<Tests>]
let replayTests =
    testList
        "light tick replay"
        [
            test "a creep re-issues every act of the full tick whose target still stands" {
                // #529: a light tick replays by target, not by tile; the engine
                // checks range, store and cooldown, and a refusal costs no CPU.
                Expect.equal
                    (LightTick.intents last stepped)
                    [
                        HarvestSource("digger", "src-a")
                        TransferEnergyToStructure("digger", "link-1", Energy)
                        MoveCreep("hauler", Right)
                    ]
                    "the harvest, the transfer and the next step; never the spawn"

                Expect.equal
                    (LightTick.intents
                        last
                        { stepped with
                            Objects = Set.remove "link-1" stepped.Objects
                        })
                    [ HarvestSource("digger", "src-a"); MoveCreep("hauler", Right) ]
                    "a target gone from the glance: that act is dropped"
            }

            test
                "a hauler that stepped beside its Refill target on the full tick transfers on the light tick" {
                // #529: out of reach at the start of the full tick it issued
                // nothing; the full tick lays its pour on arrival
                // (`Decision.Next`), which the light tick issues.
                let arriving =
                    LightTick.lastFullWith
                        quiet
                        Map.empty
                        [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                        [ MoveCreep("hauler", Left) ]

                let arrived =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "hauler" (creepAt 19 20 500)
                    }

                Expect.equal
                    (LightTick.intents arriving arrived)
                    [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                    "the pour, from the tile it stepped onto"

                Expect.isEmpty
                    (LightTick.intents
                        arriving
                        { arrived with
                            Objects = Set.remove "ext-9" arrived.Objects
                        })
                    "its target gone: nothing"
            }

            test "the full tick's one-shots by a creep, a tower or a planner are never replayed" {
                let oneShots =
                    [
                        SpawnCreep("Spawn1", [ Move ], "x")
                        PlaceConstructionSite(tile 5 5, StructureKind.Road)
                        PlaceSpawnSite(tile 6 6, "Spawn9")
                        ClaimController("digger", "ctrl")
                        SignController("digger", "ctrl", "hi")
                        SayCreep("digger", "o/")
                        SetRampartPublic("rampart", true)
                        SendFromTerminal("term", Energy, 100, "W2N2")
                        // A named amount: one that landed would be drawn twice.
                        WithdrawFromStore("hauler", "store", Energy, Some 100)
                        MoveCreep("digger", Top)
                    ]

                for oneShot in oneShots do
                    let full = LightTick.lastFull quiet Map.empty [ oneShot ]

                    Expect.isEmpty (solo full).Acts $"%A{oneShot} is not kept"
                    Expect.isEmpty (LightTick.intents full quiet) $"%A{oneShot} is never replayed"
            }

            test "an observer's look is issued again, so its vision lands on the next full tick" {
                // #529: vision lands the tick after the call; a look on the full
                // tick lands on the light tick, which reads nothing.
                let looked = LightTick.lastFull quiet Map.empty [ ObserveRoom("obs-1", "W5N5") ]

                Expect.equal
                    (LightTick.intents looked quiet)
                    [ ObserveRoom("obs-1", "W5N5") ]
                    "the same look"

                Expect.equal (LightTick.lookups looked) [ "obs-1" ] "the observer is looked up"

                Expect.isEmpty
                    (LightTick.intents
                        looked
                        { quiet with
                            Objects = Set.remove "obs-1" quiet.Objects
                        })
                    "an observer gone: nothing"
            }

            test "a lab's reaction is asked again, so the run the cooldown held back lands" {
                // The engine counts each run off the labs' own stores and
                // refuses one on cooldown: a replay draws nothing twice.
                let glance =
                    { quiet with
                        Objects = Set.add "lab-c" quiet.Objects
                    }

                let reacted =
                    LightTick.lastFull glance Map.empty [ RunReaction("lab-c", "lab-a", "lab-b") ]

                Expect.equal
                    (LightTick.intents reacted glance)
                    [ RunReaction("lab-c", "lab-a", "lab-b") ]
                    "the same reaction"

                Expect.isEmpty (LightTick.intents reacted quiet) "the output lab gone: nothing"

                Expect.isEmpty
                    (LightTick.forcing reacted glance)
                    "a reaction is no structure's fight: the next tick stays light"
            }

            test "a swing at a core, a re-claim and a pickup are replayed while their targets stand" {
                // #529: a level-0 core swung half the time took twice as long.
                let full =
                    LightTick.lastFull
                        quiet
                        Map.empty
                        [
                            AttackStructure("digger", "core")
                            ClaimReactor("digger", "reactor")
                            PickupPile("hauler", "pile")
                        ]

                Expect.equal
                    (LightTick.intents full stepped)
                    [
                        AttackStructure("digger", "core")
                        ClaimReactor("digger", "reactor")
                        PickupPile("hauler", "pile")
                    ]
                    "each again"

                Expect.equal
                    (LightTick.lookups full |> List.sort)
                    [ "core"; "pile"; "reactor" ]
                    "and each target looked up"
            }

            test "a ranged shot at a core is replayed while the core stands" {
                // #558: a ranger seated at three shoots it as a guard swings.
                let full =
                    LightTick.lastFull quiet Map.empty [ RangedAttackStructure("hauler", "core") ]

                Expect.equal
                    (LightTick.intents full stepped)
                    [ RangedAttackStructure("hauler", "core") ]
                    "the same shot"

                Expect.isEmpty
                    (LightTick.intents
                        full
                        { stepped with
                            Objects = Set.remove "core" stepped.Objects
                        })
                    "a core gone: nothing"
            }

            test "a drop is replayed only from the tile it was decided on" {
                // A drop has no target but the ground under the body.
                let dropped = LightTick.lastFull quiet Map.empty [ DropEnergy "hauler" ]

                Expect.equal (LightTick.intents dropped quiet) [ DropEnergy "hauler" ] "standing"
                Expect.isEmpty (LightTick.intents dropped stepped) "a step off: never"
            }

            test
                "a guard that pre-healed out of reach swings at its aim beside it, as on a full tick" {
                // The full tick's priority (`IntentPlan.arbitrate`): a heal is a
                // reflex, and a swing or a shot beats it for the channel.
                let guarded =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "guard" (creepAt 12 10 1000)
                        Hostiles = [ hostile 13 10 "Trepidimous" false ]
                    }

                let preHealed heal aim =
                    LightTick.lastFull guarded Map.empty [ heal ] |> aiming [ aim ] Map.empty

                let ofGuard record =
                    LightTick.intents record guarded
                    |> List.filter (fun intent ->
                        match intent with
                        | HealCreep("guard", _)
                        | RangedHealCreep("guard", _)
                        | AttackCreep("guard", _)
                        | RangedAttackCreep("guard", _) -> true
                        | _ -> false)

                Expect.equal
                    (ofGuard (preHealed (HealCreep("guard", "guard")) (AttackCreep("guard", "h-1"))))
                    [ AttackCreep("guard", "h-1") ]
                    "the swing, not the pre-heal"

                Expect.equal
                    (ofGuard (
                        preHealed
                            (RangedHealCreep("guard", "digger"))
                            (RangedAttackCreep("guard", "h-1"))
                    ))
                    [ RangedAttackCreep("guard", "h-1") ]
                    "the shot, not the ranged heal"

                Expect.equal
                    (ofGuard (
                        preHealed (HealCreep("guard", "guard")) (RangedAttackCreep("guard", "h-1"))
                    ))
                    [ RangedAttackCreep("guard", "h-1"); HealCreep("guard", "guard") ]
                    "a heal on a channel the shot leaves free stands beside it"
            }

            test
                "a ranger with two aims beside it mass attacks; with a body beside it that is none of its aims, it shoots" {
                // #537: the full tick's strike choice off the glance
                // (`Emitter.pickStrike`), its guns and shields the full tick's
                // (`LastFull.Volleys`).
                let shooting extra =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "ranger" (creepAt 12 10 1000)
                        Hostiles =
                            [
                                { hostile 13 10 "Trepidimous" true with
                                    Id = "h-1"
                                }
                                { hostile 11 10 "Trepidimous" true with
                                    Id = "h-2"
                                }
                            ]
                            @ extra
                    }

                let full volleys =
                    LightTick.lastFull
                        (shooting [])
                        Map.empty
                        [ RangedAttackCreep("ranger", "h-1") ]
                    |> aiming
                        [ RangedAttackCreep("ranger", "h-1"); RangedAttackCreep("ranger", "h-2") ]
                        volleys

                let gunned =
                    Map.ofList
                        [
                            "ranger",
                            {
                                Room = room
                                Guns = 2
                                Shields = Set.empty
                                Bands = Map.ofList [ "h-1", 2; "h-2", 2 ]
                            }
                        ]

                let ofRanger record glance =
                    LightTick.intents record glance
                    |> List.filter (fun intent ->
                        match intent with
                        | RangedAttackCreep("ranger", _)
                        | RangedMassAttack "ranger" -> true
                        | _ -> false)

                Expect.equal
                    (ofRanger (full gunned) (shooting []))
                    [ RangedMassAttack "ranger" ]
                    "two at range 1: the mass attack, in place of the full tick's shot"

                Expect.equal
                    (ofRanger
                        (full gunned)
                        (shooting
                            [
                                { hostile 12 13 "Odiodin" false with
                                    Id = "ally"
                                }
                            ]))
                    [ RangedAttackCreep("ranger", "h-1") ]
                    "an ally at range 3: the shot"

                Expect.equal
                    (ofRanger
                        (full (
                            gunned
                            |> Map.map (fun _ volley ->
                                { volley with
                                    Shields = Set.ofList [ { X = 12; Y = 12 } ]
                                })
                        ))
                        (shooting []))
                    [ RangedAttackCreep("ranger", "h-1") ]
                    "a shield at range 2: the shot"

                Expect.equal
                    (ofRanger
                        (full gunned)
                        { shooting [] with
                            PowerCreeps = [ tile 12 13 ]
                        })
                    [ RangedAttackCreep("ranger", "h-1") ]
                    "a neutral's power creep glanced at range 3: the shot"

                Expect.equal
                    (ofRanger (full Map.empty) (shooting []))
                    [ RangedAttackCreep("ranger", "h-1") ]
                    "no volley recorded: the shot"
            }

            test
                "a ranger with its head at range 2 and a lower band's aim beside it shoots the head (#563)" {
                // The full tick's bands (`Volley.Bands`): the mass's 20 on
                // the hauler beside it is no blow to the raid, whose melee
                // at range 2 it deals 8 where the shot deals 20.
                let glance =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "ranger" (creepAt 12 10 1000)
                        Hostiles =
                            [
                                { hostile 14 10 "Trepidimous" true with
                                    Id = "melee"
                                }
                                { hostile 11 10 "Trepidimous" false with
                                    Id = "hauler"
                                }
                            ]
                    }

                let record =
                    LightTick.lastFull glance Map.empty [ RangedAttackCreep("ranger", "melee") ]
                    |> aiming
                        [
                            RangedAttackCreep("ranger", "melee")
                            RangedAttackCreep("ranger", "hauler")
                        ]
                        (Map.ofList
                            [
                                "ranger",
                                {
                                    Room = room
                                    Guns = 2
                                    Shields = Set.empty
                                    Bands = Map.ofList [ "melee", 2; "hauler", 4 ]
                                }
                            ])

                Expect.equal
                    (LightTick.intents record glance
                     |> List.filter (fun intent ->
                         match intent with
                         | RangedAttackCreep("ranger", _)
                         | RangedMassAttack "ranger" -> true
                         | _ -> false))
                    [ RangedAttackCreep("ranger", "melee") ]
                    "the shot at the melee"
            }

            test
                "a guard with a target beside it and its head three off swings at the one beside it, and steps onto it" {
                // The full tick's order, head first (`Emitter.aims`); the full
                // tick swung at the head, then beside it, which has since
                // stepped three off while another came beside the guard.
                let guarded =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "guard" (creepAt 12 10 1000)
                        Hostiles =
                            [
                                { hostile 15 10 "Trepidimous" true with
                                    Id = "h-head"
                                }
                                { hostile 12 11 "Trepidimous" true with
                                    Id = "h-near"
                                }
                            ]
                    }

                let full =
                    LightTick.lastFull
                        guarded
                        (Map.ofList [ "guard", [ tile 12 10; tile 13 10 ] ])
                        [ AttackCreep("guard", "h-head") ]
                    |> aiming
                        [ AttackCreep("guard", "h-head"); AttackCreep("guard", "h-near") ]
                        Map.empty

                let ofGuard =
                    LightTick.intents full guarded
                    |> List.filter (fun intent ->
                        match intent with
                        | AttackCreep("guard", _)
                        | MoveCreep("guard", _) -> true
                        | _ -> false)

                Expect.equal
                    ofGuard
                    [ AttackCreep("guard", "h-near"); MoveCreep("guard", Bottom) ]
                    "the one in reach, not the head, and a step onto its tile in place of the plan's"

                let gone =
                    { guarded with
                        Hostiles = [ List.head guarded.Hostiles ]
                    }

                Expect.equal
                    (LightTick.intents full gone
                     |> List.filter (fun intent ->
                         match intent with
                         | AttackCreep("guard", _)
                         | MoveCreep("guard", _) -> true
                         | _ -> false))
                    [ AttackCreep("guard", "h-head"); MoveCreep("guard", Right) ]
                    "with none in reach, the head, and the plan's step toward it"
            }

            test
                "a guard beside its target swings at it but never steps onto it where the full tick reads the tile as mired (#614)" {
                let guarded =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "guard" (creepAt 12 10 1000)
                        Hostiles = [ hostile 12 11 "Trepidimous" true ]
                    }

                let full mired =
                    LightTick.lastFull guarded Map.empty [ AttackCreep("guard", "h-1") ]
                    |> aiming [ AttackCreep("guard", "h-1") ] Map.empty
                    |> fun record ->
                        { record with
                            Plans =
                                record.Plans |> Map.map (fun _ plan -> { plan with Mired = mired })
                        }

                let ofGuard record =
                    LightTick.intents record guarded
                    |> List.filter (fun intent ->
                        match intent with
                        | AttackCreep("guard", _)
                        | MoveCreep("guard", _) -> true
                        | _ -> false)

                Expect.equal
                    (ofGuard (full Map.empty))
                    [ AttackCreep("guard", "h-1"); MoveCreep("guard", Bottom) ]
                    "the premise: on plain it steps onto its target's tile"

                Expect.equal
                    (ofGuard (full (Map.ofList [ "guard", Set.ofList [ tile 12 11 ] ])))
                    [ AttackCreep("guard", "h-1") ]
                    "on a tile that holds it, it swings and stands"
            }

            // Melee glue steps onto the target's tile from range 1. Movement S3
            // moved what keeps it off a tile into the caller's reading: the
            // full tick's maps (`Emitter.glueKeeps`, a tile struck no harder
            // than where it stands or than its heal), the light tick's glance
            // (no second melee swinging at it) until S5.
            test
                "the melee's step onto its target never lands on an exit, off our rampart, nor where its caller reads it struck harder" {
                // `Emitter.glueStep`, the one rule both ticks read (#535).
                Expect.equal
                    (Decide.Emitter.glueStep (tile 12 10) (tile 13 10) false true)
                    (Some Right)
                    "the premise: beside its target, it steps onto its tile"

                Expect.isNone
                    (Decide.Emitter.glueStep (tile 1 10) (tile 0 10) false true)
                    "never onto an exit tile: the engine carries the body across"

                Expect.isNone
                    (Decide.Emitter.glueStep (tile 12 10) (tile 13 10) true true)
                    "never off our rampart"

                Expect.isNone
                    (Decide.Emitter.glueStep (tile 12 10) (tile 13 10) false false)
                    "never onto a tile its caller reads as struck harder"

                let glancedWith name hostiles =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add name (creepAt 12 10 1000)
                        Hostiles = hostiles
                    }

                let glanced name =
                    glancedWith name [ hostile 13 10 "Trepidimous" true ]

                let stepsAmong name now =
                    let full =
                        LightTick.lastFull now Map.empty []
                        |> aiming [ AttackCreep(name, "h-1") ] Map.empty

                    LightTick.intents full now
                    |> List.filter (function
                        | MoveCreep(who, _) -> who = name
                        | _ -> false)

                Expect.isEmpty
                    (stepsAmong
                        "guard-2"
                        (glancedWith
                            "guard-2"
                            [
                                hostile 13 10 "Trepidimous" true
                                { hostile 14 11 "Trepidimous" true with
                                    Id = "h-2"
                                }
                            ]))
                    "the light tick's glance: never onto a tile a second melee swings at"

                let stepsOf name =
                    let now = glanced name

                    let full =
                        LightTick.lastFull now Map.empty []
                        |> aiming [ AttackCreep(name, "h-1") ] Map.empty

                    LightTick.intents full now
                    |> List.filter (function
                        | MoveCreep(who, _) -> who = name
                        | _ -> false)

                Expect.equal
                    (stepsOf "guard-1")
                    [ MoveCreep("guard-1", Right) ]
                    "a guard steps onto it"
            }

            test "a light tick never lays two acts on one channel of one creep" {
                // The full tick's acts passed `IntentPlan`; a planned act or an
                // aim on a channel one of them holds is dropped, and a next pour
                // supersedes the full tick's own.
                let guarded =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "guard" (creepAt 12 10 1000)
                        Hostiles = [ hostile 13 10 "Trepidimous" false ]
                    }

                let full =
                    LightTick.lastFullWith
                        guarded
                        Map.empty
                        [ TransferEnergyToStructure("digger", "ext-9", Energy) ]
                        [
                            HarvestSource("digger", "src-a")
                            TransferEnergyToStructure("digger", "link-1", Energy)
                            RepairStructure("guard", "road")
                        ]
                    |> aiming
                        [ AttackCreep("guard", "h-1"); RangedAttackCreep("hauler", "h-1") ]
                        Map.empty

                let replayed = LightTick.intents full guarded

                Expect.equal
                    replayed
                    [
                        HarvestSource("digger", "src-a")
                        TransferEnergyToStructure("digger", "ext-9", Energy)
                        RepairStructure("guard", "road")
                        RangedAttackCreep("hauler", "h-1")
                    ]
                    "the next pour over the full tick's; no swing beside the repair"

                Expect.isOk (IntentPlan.create replayed) "one act per channel"
            }

            test "a refiller still on its tile issues the full tick's next pour, once" {
                // User, 2026-10-04: a pour is one-shot, so the light tick after it
                // stood still; the full tick lays the next one (`Decision.Next`).
                let withNext =
                    LightTick.lastFullWith
                        quiet
                        Map.empty
                        [ TransferEnergyToStructure("digger", "ext-9", Energy) ]
                        [ TransferEnergyToStructure("digger", "link-1", Energy) ]

                Expect.equal
                    (LightTick.intents withNext quiet)
                    [ TransferEnergyToStructure("digger", "ext-9", Energy) ]
                    "the next pour, and never the full tick's own"
            }

            test "every act on a target is kept, in the full tick's order" {
                let acts =
                    [
                        HarvestSource("digger", "src-a")
                        UpgradeController("digger", "ctrl")
                        ReserveController("digger", "ctrl")
                        AttackController("digger", "ctrl")
                        BuildSite("digger", "site")
                        RepairStructure("digger", "road")
                        DismantleStructure("digger", "wall")
                        TransferEnergyToStructure("digger", "spawn", Energy)
                        WithdrawFromStore("hauler", "store", Energy, None)
                        PickupPile("digger", "pile")
                        ClaimReactor("digger", "reactor")
                        AttackStructure("digger", "core")
                    ]

                let full = LightTick.lastFull quiet Map.empty acts

                Expect.equal (solo full).Acts acts "each, in order"
                Expect.isFalse (solo full).Fought "none of them a structure's fight"
            }

            test "a standing upgrader's draw is replayed with its upgrade, a step off or not" {
                // #498's pair, now under the one rule: the engine checks the
                // buffer's range.
                let paired =
                    LightTick.lastFull
                        quiet
                        Map.empty
                        [
                            UpgradeController("digger", "ctrl")
                            WithdrawFromStore("digger", "can-buf", Energy, None)
                        ]

                let pushed =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "digger" (creepAt 11 10 1000)
                    }

                for glance in [ quiet; pushed ] do
                    Expect.equal
                        (LightTick.intents paired glance)
                        [
                            UpgradeController("digger", "ctrl")
                            WithdrawFromStore("digger", "can-buf", Energy, None)
                        ]
                        "both acts for the upgrader"
            }

            test "a creep off the tile it worked from re-issues its acts all the same" {
                let pushed =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "digger" (creepAt 11 10 1000)
                    }

                Expect.equal
                    (LightTick.intents last pushed)
                    [
                        HarvestSource("digger", "src-a")
                        TransferEnergyToStructure("digger", "link-1", Energy)
                        MoveCreep("hauler", Right)
                    ]
                    "out of reach, the engine refuses them for nothing"
            }

            test "a walker steps toward its plan's second tile only while it stands on the first" {
                Expect.contains
                    (LightTick.intents last stepped)
                    (MoveCreep("hauler", Right))
                    "on (21,20), toward (22,20)"

                let diagonal =
                    LightTick.lastFull
                        quiet
                        (Map.ofList [ "hauler", [ tile 21 20; tile 22 21 ] ])
                        []

                Expect.equal
                    (LightTick.intents diagonal stepped)
                    [ MoveCreep("hauler", BottomRight) ]
                    "the direction is the plan's, not last tick's"

                // Blocked, fatigued or pushed: it never reached the first tile.
                Expect.isFalse
                    (LightTick.intents last quiet
                     |> List.exists (function
                         | MoveCreep _ -> true
                         | _ -> false))
                    "a blocked walker waits"
            }

            test
                "a longer cadence's light ticks walk the plan on a tile a tick, from wherever on it the body stands" {
                let walk =
                    LightTick.lastFull
                        quiet
                        (Map.ofList [ "hauler", [ tile 21 20; tile 22 20; tile 23 21; tile 24 21 ] ])
                        []

                let at run x y =
                    { quiet with
                        Time = run
                        Creeps = quiet.Creeps |> Map.add "hauler" (creepAt x y 500)
                    }

                for run, x, y, direction in
                    [ 1, 21, 20, Right; 2, 22, 20, BottomRight; 3, 23, 21, Right ] do
                    Expect.equal
                        (LightTick.intents walk (at run x y))
                        [ MoveCreep("hauler", direction) ]
                        $"light tick {run}: from ({x},{y}) toward the plan's next tile"

                Expect.equal
                    (LightTick.intents walk (at 2 21 20))
                    [ MoveCreep("hauler", Right) ]
                    "a tick late, it walks on from where it stands"

                Expect.isEmpty (LightTick.intents walk (at 3 24 21)) "at the plan's end it waits"
            }

            test
                "a plan's later acts are issued on their own light tick, each superseding the one before" {
                let planAt time =
                    LightTick.plan
                        { quiet with Time = time }
                        (Set.ofList [ "hauler" ])
                        Map.empty
                        [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                        [ [ TransferEnergyToStructure("hauler", "spawn", Energy) ] ]
                        [ TransferEnergyToStructure("hauler", "link-1", Energy) ]
                        []
                        Map.empty
                        Map.empty

                let issued time (plan: Plan) =
                    LightTick.intents
                        (LightTick.ofPlans quiet Set.empty [ room, plan ])
                        { quiet with Time = time }

                Expect.equal
                    (issued 1 (planAt 0))
                    [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                    "the first light tick: the next pour, superseding the full tick's"

                Expect.equal
                    (issued 2 (planAt 0))
                    [ TransferEnergyToStructure("hauler", "spawn", Energy) ]
                    "the second: the later pour"

                Expect.equal
                    (issued 2 (planAt 1))
                    [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                    "a plan a forced tick made a tick later is a tick younger"

                Expect.contains
                    (LightTick.lookups (LightTick.ofPlans quiet Set.empty [ room, planAt 0 ]))
                    "spawn"
                    "and the later act's object is looked up"
            }

            test
                "a walker's later act is the one planned for the tile it stands on, a blocked body issuing its first tile's on the second light tick (#574)" {
                // Cadence 3: the hauler steps onto (21,20) and pours into the
                // extension there, then onto (22,20) and pours into the spawn.
                let walk =
                    LightTick.ofPlans
                        quiet
                        Set.empty
                        [
                            room,
                            LightTick.plan
                                quiet
                                (Set.ofList [ "hauler" ])
                                (Map.ofList [ "hauler", [ tile 21 20; tile 22 20 ] ])
                                [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                                [ [ TransferEnergyToStructure("hauler", "spawn", Energy) ] ]
                                []
                                []
                                Map.empty
                                Map.empty
                        ]

                let at x y =
                    { quiet with
                        Time = 2
                        Creeps = Map.ofList [ "hauler", creepAt x y 500 ]
                    }

                Expect.equal
                    (LightTick.intents walk (at 22 20))
                    [ TransferEnergyToStructure("hauler", "spawn", Energy) ]
                    "on time at the second tile: the pour planned there"

                Expect.equal
                    (LightTick.intents walk (at 21 20))
                    [
                        TransferEnergyToStructure("hauler", "ext-9", Energy)
                        MoveCreep("hauler", Right)
                    ]
                    "blocked a tick on the first tile: the pour planned there, and on"

                Expect.equal
                    (LightTick.intents walk (at 20 20))
                    [ TransferEnergyToStructure("hauler", "ext-9", Energy) ]
                    "off its plan: no later act"
            }

            test "a light tick issues in creep-name order" {
                let many =
                    LightTick.lastFull
                        quiet
                        Map.empty
                        [ UpgradeController("hauler", "ctrl"); HarvestSource("digger", "src-a") ]

                Expect.equal
                    (LightTick.intents many quiet)
                    [ HarvestSource("digger", "src-a"); UpgradeController("hauler", "ctrl") ]
                    "digger before hauler, whatever the full tick's order"
            }

            test
                "a fighter swings at its full tick's aim two tiles off, the tick it steps beside it (#528)" {
                // Live t971,345 W17S26: a guard chasing an unarmed digger came
                // beside it on light ticks and never swung.
                let guarded =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "guard" (creepAt 10 14 1000)
                    }

                let aimed =
                    LightTick.lastFull guarded Map.empty []
                    |> aiming [ AttackCreep("guard", "h-1") ] Map.empty

                let beside =
                    { guarded with
                        Creeps = guarded.Creeps |> Map.add "guard" (creepAt 11 14 1000)
                        Hostiles = [ hostile 12 14 "Trepidimous" false ]
                    }

                Expect.equal (LightTick.forced aimed beside) None "an unarmed digger forces nothing"

                Expect.contains
                    (LightTick.intents aimed beside)
                    (AttackCreep("guard", "h-1"))
                    "the light tick swings at the aim"

                Expect.isFalse
                    (LightTick.intents (LightTick.lastFull guarded Map.empty []) beside
                     |> List.exists (function
                         | AttackCreep _ -> true
                         | _ -> false))
                    "no aim recorded, no swing"

                Expect.isFalse
                    (LightTick.intents aimed { beside with Hostiles = [] }
                     |> List.exists (function
                         | AttackCreep _ -> true
                         | _ -> false))
                    "and none at a target gone from sight"
            }
        ]

[<Tests>]
let cadenceTests =
    testList
        "the cadence: ticks per full decide"
        [
            test "a live or tuned cadence is clamped to 2..4" {
                Expect.equal
                    ([ -3; 0; 1; 2; 3; 4; 5; 99 ] |> List.map LightTick.clampCadence)
                    [ 2; 2; 2; 2; 3; 4; 4; 4 ]
                    "under 2 no light tick, past 4 staler than a step plan walks"
            }

            test
                "the tick after a full one replays only inside the cadence, and only while every tick since did" {
                Expect.isTrue (LightTick.replays 2 1 0) "a cadence of 2: the one light tick"
                Expect.isFalse (LightTick.replays 2 2 1) "and never two in a row"
                Expect.isTrue (LightTick.replays 3 2 1) "a cadence of 3: two"
                Expect.isFalse (LightTick.replays 3 3 2) "and never three"
                Expect.isFalse (LightTick.replays 3 2 0) "a skipped tick decides"
                Expect.isFalse (LightTick.replays 4 0 0) "the full tick's own"
            }

            test "the bucket raises the cadence under its line and lowers it only past the band" {
                let tuning =
                    { Tuning.defaults with
                        Cadence = 2
                        CadenceBuckets = [ 5000, 3; 2000, 4 ]
                        CadenceBand = 1000
                    }

                let walk buckets =
                    (Tuning.defaults.Cadence, buckets)
                    ||> List.scan (LightTick.bucketCadence tuning)
                    |> List.tail

                Expect.equal
                    (walk [ 9000; 4999; 5500; 5999; 6000; 4000; 1999; 2500; 3000; 10000 ])
                    [ 2; 3; 3; 3; 2; 3; 4; 4; 3; 2 ]
                    "each line crossed once going down, and again a band over it going up"

                Expect.equal
                    (LightTick.bucketCadence { tuning with Cadence = 3 } 3 10000)
                    3
                    "never under the tuned cadence"

                Expect.equal
                    (LightTick.bucketCadence Tuning.defaults 2 4999)
                    3
                    "the shipped line: the old Lean, one full in three under half a bucket"

                Expect.equal
                    (LightTick.bucketCadence Tuning.defaults 2 10000)
                    2
                    "a full bucket: today's"
            }

            test
                "the cadence in effect names its source: a live override over the bucket's over the tuned (#575)" {
                let tuned = Tuning.defaults.Cadence

                Expect.equal
                    (LightTick.cadenceFrom Tuning.defaults tuned None)
                    (tuned, LightTick.SwitchFrom.Tuned)
                    "a full bucket: the tuned"

                Expect.equal
                    (LightTick.cadenceFrom Tuning.defaults 3 None)
                    (3, LightTick.SwitchFrom.Bucket)
                    "a raised one: the bucket's"

                Expect.equal
                    (LightTick.cadenceFrom Tuning.defaults 3 (Some tuned))
                    (tuned, LightTick.SwitchFrom.Override)
                    "an override stands over the bucket's, even at the tuned value"

                Expect.equal
                    (LightTick.switchFrom false (Some true))
                    (true, LightTick.SwitchFrom.Override)
                    "a switch's override"

                Expect.equal
                    (LightTick.switchFrom false None)
                    (false, LightTick.SwitchFrom.Tuned)
                    "a switch unset: the tuned"
            }
        ]
