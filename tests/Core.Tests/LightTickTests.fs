/// The light tick: what a quiet tick replays of the last full one, what it
/// walks, and every fact that hands it over to a full tick instead.
module Fabot.Core.Tests.LightTickTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick

let private room = "W1N1"
let private tile x y = RoomPos.at room { X = x; Y = y }

let private creepAt x y hits : GlanceCreep =
    {
        Tile = tile x y
        Hits = hits
        Inward = None
        Heals = 0
    }

/// The room our controller stands in: not `room`, whose forcing rules are the
/// reach rules (a raider in a home forces every tick, `homeTests`).
let private homeRoom = "W9N9"

/// A quiet world: two creeps of ours, a spawn, one controller at level 5, and
/// nobody else. Every object an act below names still stands, as the glance
/// looks it up (`LightTick.lookups`).
let private quiet: Glance =
    {
        Creeps = Map.ofList [ "digger", creepAt 10 10 1000; "hauler", creepAt 20 20 500 ]
        Hostiles = []
        Structures = [ tile 30 30 ]
        Controllers = Map.ofList [ homeRoom, (5, false) ]
        Lean = false
        PowerCreeps = []
        Objects =
            Set.ofList
                [
                    "src-a"
                    "link-1"
                    "ext-9"
                    "ctrl"
                    "site"
                    "road"
                    "wall"
                    "store"
                    "can-buf"
                    "pile"
                    "reactor"
                    "core"
                    "obs-1"
                    "spawn"
                ]
    }

/// The full tick that saw `quiet`: the digger harvesting where it stands and
/// handing its load on, the hauler one step into a walk.
let private last: LastFull =
    LightTick.lastFull
        quiet
        (Map.ofList [ "hauler", (tile 21 20, tile 22 20) ])
        [
            HarvestSource("digger", "src-a")
            TransferEnergyToStructure("digger", "link-1", Energy)
            MoveCreep("hauler", Right)
            SpawnCreep("Spawn1", [ Work; Carry; Move ], "next")
        ]

/// A one-colony record's plan.
let private solo (record: LastFull) : Plan =
    record.Plans |> Map.values |> Seq.exactlyOne

/// The record with its plan's aims and volleys, as a fighter's full tick
/// lays them.
let private aiming
    (aims: Intent list)
    (volleys: Map<string, Volley>)
    (record: LastFull)
    : LastFull =
    { record with
        Plans =
            record.Plans
            |> Map.map (fun _ plan ->
                { plan with
                    Aims = aims
                    Volleys = volleys
                })
    }

/// A hostile at a MOVE a part: never stationary.
let private mobile: Gait = { Weight = 1; Moves = 1 }

let private hostile x y owner armed : GlanceHostile =
    {
        Id = "h-1"
        Tile = tile x y
        Owner = owner
        Armed = armed
        Heals = false
        Weapon = if armed then Some Engine.meleeRange else None
        Gait = mobile
        Breaks = false
        Hits = 1000
    }

/// The world one tick on: the hauler took its step and everything else held.
let private stepped =
    { quiet with
        Creeps = quiet.Creeps |> Map.add "hauler" (creepAt 21 20 500)
    }

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
                        (Map.ofList [ "guard", (tile 12 10, tile 13 10) ])
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
                "the melee's step onto its target never lands on an exit, off our rampart, beside a second melee, nor for a squad's cast" {
                // `Emitter.glueStep`, the one rule both ticks read (#535).
                Expect.equal
                    (Decide.Emitter.glueStep (tile 12 10) (tile 13 10) false [])
                    (Some Right)
                    "the premise: beside its target, it steps onto its tile"

                Expect.isNone
                    (Decide.Emitter.glueStep (tile 1 10) (tile 0 10) false [])
                    "never onto an exit tile: the engine carries the body across"

                Expect.isNone
                    (Decide.Emitter.glueStep (tile 12 10) (tile 13 10) true [])
                    "never off our rampart"

                Expect.isNone
                    (Decide.Emitter.glueStep
                        (tile 12 10)
                        (tile 13 10)
                        false
                        [ tile 13 10; tile 14 11 ])
                    "never onto a tile a second melee swings at"

                let glanced name =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add name (creepAt 12 10 1000)
                        Hostiles = [ hostile 13 10 "Trepidimous" true ]
                    }

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

                Expect.isEmpty
                    (stepsOf "brawler-1-Spawn1")
                    "a squad's brawler does not: its Formation moves it"
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
                    LightTick.lastFull quiet (Map.ofList [ "hauler", (tile 21 20, tile 22 21) ]) []

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
let forcedTests =
    testList
        "light tick forced full"
        [
            test "a quiet world one step on forces nothing" {
                Expect.equal (LightTick.forced last stepped) None "the light tick runs"
            }

            test
                "an armed hostile in a home room out of reach of everything of ours forces nothing; within reach of a creep, a full tick" {
                // #549: armed on paper is no fact a replay misses; a body
                // that can hurt something of ours this tick is.
                let raided x y =
                    { stepped with
                        Hostiles = [ hostile x y "Invader" true ]
                    }

                Expect.equal (LightTick.forced last (raided 45 45)) None "far off at home"

                Expect.equal
                    (LightTick.forced last (raided 12 10))
                    (Some(LightForce.Hostile(room, "Invader")))
                    "beside the digger"
            }

            // Moved 2026-10-06 (live t981,05x, W17S25): the Brief's S7 rule 3
            // let a healer alone at home force nothing, but the towers shoot a
            // healer they break (#466), and one left between its pair's swaps
            // on the exit healed back every light tick's silence.
            test "a heal-only body at home forces a full tick, as a shooter does" {
                let medic: HostileInfo =
                    {
                        Id = "medic"
                        Owner = "Trepidimous"
                        Pos = tile 33 33
                        Body = [ Move; Move; Heal; Heal ]
                        Hits = 400
                        TicksToLive = 1000
                    }

                let shooter =
                    { medic with
                        Id = "shooter"
                        Pos = tile 34 33
                        Body = [ Move; RangedAttack ]
                        Hits = 200
                    }

                let glanced side =
                    { stepped with
                        Controllers = Map.ofList [ room, (5, false) ]
                        Hostiles = side |> List.map (LightTick.glanceOf side)
                    }

                let home = Some(LightForce.Home(room, "Trepidimous"))

                Expect.equal (LightTick.forced last (glanced [ medic ])) home "the healer alone"

                Expect.equal
                    (LightTick.forced last (glanced [ medic; shooter ]))
                    home
                    "the shooter beside it is decided"

                Expect.contains
                    (LightTick.forcing last (glanced [ medic; shooter ]))
                    (LightForce.Hostile(room, "Trepidimous"))
                    "and reaches the spawn"
            }

            test "an armed hostile beside ours in a room under our safe mode forces nothing" {
                // Live t931,466: Trepidimous' pair in W17S25 under its safe mode
                // held every tick full at 95 ms.
                let safe =
                    { stepped with
                        Hostiles = [ hostile 21 21 "Invader" true ]
                        Controllers = Map.ofList [ room, (5, true) ]
                    }

                Expect.equal
                    (LightTick.forced
                        { last with
                            Controllers = safe.Controllers
                        }
                        safe)
                    None
                    "it can act on nothing of ours"
            }

            test "an armed ally beside ours forces nothing" {
                let ally = Colony.allies |> Seq.head

                let guarded =
                    { stepped with
                        Hostiles = [ hostile 21 21 ally true ]
                    }

                Expect.equal (LightTick.forced last guarded) None "an ally is no threat"
            }

            test
                "an armed hostile in a harassment room, out of reach of every creep of ours, forces nothing" {
                // #461: a defender kept standing in a room we only harass took
                // the whole shard's light ticks away.
                let harassed = "W3N1"

                let withRanger =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "ranger"
                                {
                                    Tile = RoomPos.at harassed { X = 10; Y = 10 }
                                    Hits = 500
                                    Inward = None
                                    Heals = 0
                                }
                    }

                let full = LightTick.lastFull withRanger Map.empty []

                let defenderAt x =
                    { withRanger with
                        Hostiles =
                            [
                                {
                                    Id = "trep"
                                    Tile = RoomPos.at harassed { X = x; Y = 10 }
                                    Owner = "Trepidimous"
                                    Armed = true
                                    Heals = false
                                    Weapon = Some Engine.meleeRange
                                    Gait = mobile
                                    Breaks = false
                                    Hits = 1000
                                }
                            ]
                    }

                Expect.equal
                    (LightTick.forced full (defenderAt (10 + Engine.rangedRange + 3)))
                    None
                    "standing off: the tick may be light"

                Expect.equal
                    (LightTick.forced full (defenderAt 13))
                    None
                    "a melee three off: not a swing after one step"

                Expect.equal
                    (LightTick.forced full (defenderAt 12))
                    (Some(LightForce.Hostile(harassed, "Trepidimous")))
                    "a step and a swing off our ranger: full"

                Expect.equal
                    (LightTick.forced full (defenderAt 11))
                    (Some(LightForce.Hostile(harassed, "Trepidimous")))
                    "beside it: full"
            }

            test
                "a hostile our ranger shoots, beside it, forces no full tick; beside our anchor, our digger or a hauler crossing it does" {
                // #520: live t963,4xx, 98% of ticks forced full by harass
                // rangers standing by Trepidimous' creeps in W19S25 and W18S27;
                // #549 reads it as the one invariant, in any room: a body our
                // plan fights where it fights it is accounted for, one that
                // reaches anything else of ours is not.
                let harassed = "W3N1"
                let outpost = "W2N1"
                let at roomName x = RoomPos.at roomName { X = x; Y = 10 }

                let ours =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "ranger"
                                {
                                    Tile = at harassed 10
                                    Hits = 500
                                    Inward = None
                                    Heals = 0
                                }
                            |> Map.add
                                "anchor"
                                {
                                    Tile = at outpost 10
                                    Hits = 500
                                    Inward = None
                                    Heals = 0
                                }
                    }

                // The ranger shot on the full tick: its target still beside it
                // is no fight to decide there either.
                let full = LightTick.lastFull ours Map.empty [ RangedAttackCreep("ranger", "trep") ]

                let trepAt tile =
                    { ours with
                        Hostiles =
                            [
                                {
                                    Id = "trep"
                                    Tile = tile
                                    Owner = "Trepidimous"
                                    Armed = true
                                    Heals = false
                                    Weapon = Some Engine.meleeRange
                                    Gait = mobile
                                    Breaks = true
                                    Hits = 1000
                                }
                            ]
                    }

                Expect.equal
                    (LightTick.forced full (trepAt (at harassed 12)))
                    None
                    "beside our harass ranger: the light tick replays its shot"

                Expect.equal
                    (LightTick.forced full (trepAt (at outpost 12)))
                    (Some(LightForce.Hostile(outpost, "Trepidimous")))
                    "the same body beside our anchor in an outpost, its shooter rooms away"

                Expect.equal
                    (LightTick.forced full (trepAt (tile 12 10)))
                    (Some(LightForce.Hostile(room, "Trepidimous")))
                    "the same body beside our digger at home"

                // The lean stop-gaps (2026-10-05) are gone: a lean bucket
                // reads the same invariant.
                Expect.equal
                    (LightTick.forced
                        full
                        { trepAt (at outpost 12) with
                            Lean = true
                        })
                    (Some(LightForce.Hostile(outpost, "Trepidimous")))
                    "a lean bucket alike"

                let crossing =
                    { trepAt (at harassed 12) with
                        Creeps =
                            ours.Creeps
                            |> Map.add
                                "hauler"
                                {
                                    Tile = at harassed 30
                                    Hits = 500
                                    Inward = None
                                    Heals = 0
                                }
                            |> Map.add
                                "ranger"
                                {
                                    Tile = at harassed 32
                                    Hits = 500
                                    Inward = None
                                    Heals = 0
                                }
                    }

                Expect.equal
                    (LightTick.forced
                        { full with
                            Creeps = Set.add "hauler" full.Creeps
                        }
                        { crossing with
                            Hostiles =
                                [
                                    { (List.head crossing.Hostiles) with
                                        Tile = at harassed 31
                                    }
                                ]
                        })
                    (Some(LightForce.Hostile(harassed, "Trepidimous")))
                    "a hauler crossing the harassment room beside the Trepidimous melee our ranger shoots: it reaches a body that does not fight it"
            }

            test
                "a harass ranger shoots and heals itself again on a light tick, wherever it stepped" {
                let harassed = "W3N1"
                let at x = RoomPos.at harassed { X = x; Y = 10 }

                let withRanger (x: int) =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "ranger"
                                {
                                    Tile = at x
                                    Hits = 500
                                    Inward = None
                                    Heals = 0
                                }
                    }

                let full =
                    LightTick.lastFull
                        (withRanger 10)
                        Map.empty
                        [
                            RangedAttackCreep("ranger", "trep")
                            HealCreep("ranger", "ranger")
                            RangedAttackCreep("digger", "h")
                            HealCreep("digger", "hauler")
                        ]

                let replayed (glance: Glance) (record: LastFull) =
                    LightTick.intents record glance
                    |> List.filter (function
                        | RangedAttackCreep _
                        | HealCreep _ -> true
                        | _ -> false)

                let trepSeen =
                    { withRanger 11 with
                        Hostiles =
                            [
                                { hostile 14 10 "Trepidimous" false with
                                    Id = "trep"
                                    Tile = at 14
                                }
                            ]
                    }

                // #529 took #520's replay into the one rule: every shot and
                // heal whose target still stands.
                Expect.equal
                    (replayed trepSeen full)
                    [
                        HealCreep("digger", "hauler")
                        RangedAttackCreep("ranger", "trep")
                        HealCreep("ranger", "ranger")
                    ]
                    "the shot and the self-heal a step off the tile they were decided on; the digger's heal on a hauler still standing, its shot at a body out of sight never"
            }

            test "an armed Source Keeper far from us forces nothing" {
                let keeper =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Source Keeper" true ]
                    }

                Expect.equal (LightTick.forced last keeper) None "a keeper on its rock is terrain"
            }

            test "an unarmed hostile off in the room forces nothing" {
                let miner =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Rival" false ]
                    }

                Expect.equal
                    (LightTick.forced last miner)
                    None
                    "the harassment rooms hold one nearly every tick"
            }

            test
                "a keeper a step and a swing off a creep of ours forces a full tick; one further, nothing" {
                let keeperAt x =
                    { stepped with
                        Hostiles = [ hostile x 10 "Source Keeper" true ]
                    }

                Expect.equal
                    (LightTick.forced last (keeperAt 12))
                    (Some(LightForce.Hostile(room, "Source Keeper")))
                    "a keeper that comes near"

                Expect.equal
                    (LightTick.forced last (keeperAt 13))
                    None
                    "one tile further is not near"
            }

            test
                "a ranged body walking in forces on the tick it can shoot after a step, and again on the tick it can shoot, a lean bucket alike (#549 review)" {
                // A flat ring keyed once per room read 5, 4 and 3 as one fact:
                // the plan made at 5 never saw the shot at 3.
                let shooterAt lean x =
                    { stepped with
                        Lean = lean
                        Hostiles =
                            [
                                { hostile x 10 "Trepidimous" true with
                                    Weapon = Some Engine.rangedRange
                                }
                            ]
                    }

                for lean in [ false; true ] do
                    let at5 = shooterAt lean 15
                    let at4 = shooterAt lean 14
                    let at3 = shooterAt lean 13

                    Expect.isEmpty
                        (LightTick.forcing (LightTick.lastFull at5 Map.empty []) at5)
                        $"lean {lean}: five off, out of a shot after a step"

                    Expect.equal
                        (LightTick.forced (LightTick.lastFull at5 Map.empty []) at4)
                        (Some(LightForce.Hostile(room, "Trepidimous")))
                        $"lean {lean}: four off, a shot after one step"

                    Expect.equal
                        (LightTick.forced (LightTick.lastFull at4 Map.empty []) at3)
                        (Some(LightForce.Hostile(room, "Trepidimous")))
                        $"lean {lean}: three off, a shot now, though the plan saw it a step away"

                    Expect.isEmpty
                        (LightTick.forcing (LightTick.lastFull at3 Map.empty []) at3)
                        $"lean {lean}: the plan made at three accounts for it"
            }

            test
                "a hostile that can dismantle or claim, near a structure of ours, forces a full tick" {
                let atTheSpawn breaks =
                    { stepped with
                        Hostiles =
                            [
                                { hostile 32 32 "Rival" false with
                                    Breaks = breaks
                                }
                            ]
                    }

                Expect.equal
                    (LightTick.forced last (atTheSpawn true))
                    (Some(LightForce.Hostile(room, "Rival")))
                    "a WORK or CLAIM body at the gate"

                Expect.equal
                    (LightTick.forced last (atTheSpawn false))
                    None
                    "a hauler at the gate takes nothing down"
            }

            test
                "a stationary hostile is near only within its weapon's range; a mobile one a step past it" {
                // Live t964,5xx: Trepidimous' 14A1M sat 4 tiles from our hauler in
                // W18S25 and held ~31% of ticks full (#521). #549: a body as good
                // as rooted strikes from where it stands, not after a step.
                let bodyAt (weapons, part, moves) r =
                    let body = List.replicate weapons part @ List.replicate moves Move

                    { stepped with
                        Structures = []
                        Hostiles =
                            [
                                { hostile (10 + r) 10 "Trepidimous" true with
                                    Weapon = HostileInfo.weaponRangeOf body
                                    Gait = Gait.ofBody body body
                                }
                            ]
                    }

                let nearAt body r =
                    let forced = LightTick.forced last (bodyAt body r)
                    forced = Some(LightForce.Hostile(room, "Trepidimous"))

                let rootedSwing = 14, Attack, 1
                let rootedShot = 14, RangedAttack, 1

                Expect.isFalse (nearAt rootedSwing 2) "a rooted swing a step and a swing off"
                Expect.isTrue (nearAt rootedSwing 1) "a rooted swing beside it"
                Expect.isFalse (nearAt rootedShot 4) "a rooted shot a step and a shot off"
                Expect.isTrue (nearAt rootedShot 3) "a rooted shot in range"

                Expect.isTrue
                    (nearAt (10, Attack, 10) 2)
                    "a body at a MOVE a part: a step and a swing"

                Expect.isFalse (nearAt (10, Attack, 10) 3) "and no further"
            }

            test "a MOVE-only hostile within range of our creep forces nothing" {
                let scout =
                    { stepped with
                        Hostiles = [ hostile 11 10 "Mirroar" false ]
                    }

                Expect.equal (LightTick.forced last scout) None "a scout changes no decision"
            }

            test "near a creep of ours, only a body that can hurt it forces a full tick" {
                // User, 2026-10-04: a Trepidimous worker beside a harassing ranger
                // held every tick full. No structure of ours in the room, so an
                // armed body is the near rule's and not the armed rule's.
                for part, hurts, breaks in
                    [
                        Attack, true, false
                        RangedAttack, true, false
                        Work, false, true
                        Carry, false, false
                        BodyPart.Claim, false, true
                        Heal, false, false
                    ] do
                    let body =
                        { stepped with
                            Structures = []
                            Hostiles =
                                [
                                    { hostile 11 10 "Mirroar" hurts with
                                        Breaks = breaks
                                    }
                                ]
                        }

                    Expect.equal
                        (LightTick.forced last body)
                        (if hurts then
                             Some(LightForce.Hostile(room, "Mirroar"))
                         else
                             None)
                        $"one %A{part} part beside a creep of ours"
            }

            test "a hostile at the same coordinates in another room is not near" {
                let elsewhere =
                    { stepped with
                        Hostiles =
                            [
                                {
                                    Id = "rival"
                                    Tile = RoomPos.at "W2N1" { X = 10; Y = 10 }
                                    Owner = "Rival"
                                    Armed = true
                                    Heals = false
                                    Weapon = Some Engine.meleeRange
                                    Gait = mobile
                                    Breaks = false
                                    Hits = 1000
                                }
                            ]
                    }

                Expect.equal (LightTick.forced last elsewhere) None "range is within one room"
            }

            test "a full tick on which a structure fought forces the next one full" {
                for fight in
                    [
                        FireTower("tower", "h")
                        HealWithTower("tower", "hauler")
                        RepairWithTower("tower", "rampart")
                        ActivateSafeMode "ctrl"
                    ] do
                    let fought = LightTick.lastFull quiet Map.empty [ fight ]

                    Expect.isTrue (solo fought).Fought $"%A{fight} is a fight"
                    Expect.isEmpty (LightTick.intents fought quiet) $"%A{fight} is never replayed"

                    Expect.equal
                        (LightTick.forced fought quiet)
                        (Some(LightForce.Fought ""))
                        $"%A{fight} forces the next tick full, its colony's"
            }

            test
                "a creep's shot or heal forces nothing on its own, and is replayed while its target stands" {
                // A target still in reach is the plan's fight; one dead or gone
                // leaves nothing to decide.
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                { hostile 40 40 "Enemy" false with
                                    Id = "h"
                                }
                            ]
                    }

                for shot in
                    [
                        AttackCreep("digger", "h")
                        RangedAttackCreep("digger", "h")
                        HealCreep("digger", "hauler")
                        RangedHealCreep("digger", "hauler")
                    ] do
                    let full = LightTick.lastFull quiet Map.empty [ shot ]

                    Expect.isFalse (solo full).Fought $"%A{shot} is no structure's fight"
                    Expect.equal (LightTick.forced full quiet) None $"%A{shot} forces nothing"
                    Expect.equal (LightTick.intents full seen) [ shot ] $"%A{shot} is replayed"

                    // The hostile out of sight, and the healed body gone.
                    let gone =
                        { seen with
                            Hostiles = []
                            Creeps = seen.Creeps |> Map.remove "hauler"
                        }

                    Expect.isEmpty (LightTick.intents full gone) $"%A{shot}: its target gone, never"

                // #549: an armed target still beside its shooter is the plan's
                // fight; an armed body the plan does not shoot, new beside it,
                // is not.
                let shooting =
                    LightTick.lastFull quiet Map.empty [ RangedAttackCreep("digger", "h-1") ]

                let stillThere =
                    { quiet with
                        Hostiles = [ hostile 12 12 "Enemy" true ]
                    }

                Expect.equal (LightTick.forced shooting stillThere) None "the target still near"

                Expect.equal
                    (LightTick.forced
                        shooting
                        { stillThere with
                            Hostiles =
                                [
                                    { hostile 12 12 "Enemy" true with
                                        Id = "h-2"
                                    }
                                ]
                        })
                    (Some(LightForce.Hostile(room, "Enemy")))
                    "another body beside the shooter"
            }

            test "a creep born or gone since the full tick forces a full tick" {
                let born =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "next" (creepAt 30 31 100)
                    }

                let gone =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.remove "digger"
                    }

                Expect.equal
                    (LightTick.forced last born)
                    (Some(LightForce.CreepsChanged [ "next" ]))
                    "born"

                Expect.equal
                    (LightTick.forced last gone)
                    (Some(LightForce.CreepsChanged [ "digger" ]))
                    "gone"
            }

            test
                "a creep on the border ring with no ground inward forces a full tick, on every edge" {
                // It crossed on the full tick and stands on the neighbour's
                // ring; standing still there, the engine carries it straight
                // back, so a light tick with no step for it may not run.
                for x, y in [ 0, 20; 49, 20; 20, 0; 20, 49 ] do
                    let crossed =
                        { stepped with
                            Creeps =
                                stepped.Creeps
                                |> Map.add
                                    "hauler"
                                    {
                                        Tile = RoomPos.at "W2N1" { X = x; Y = y }
                                        Hits = 500
                                        Inward = None
                                        Heals = 0
                                    }
                        }

                    Expect.equal
                        (LightTick.forced last crossed)
                        (Some(LightForce.OnBorder "hauler"))
                        $"on ({x},{y})"

                Expect.equal (LightTick.forced last stepped) None "and one tile in, nothing"
            }

            test "a creep on the border ring with ground inward steps onto it, and forces nothing" {
                let crossed =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "hauler"
                                {
                                    Tile = RoomPos.at "W2N1" { X = 0; Y = 20 }
                                    Hits = 500
                                    Inward = Some Right
                                    Heals = 0
                                }
                    }

                Expect.equal
                    (LightTick.forced last crossed)
                    None
                    "the step off the ring is the light tick's"

                Expect.contains
                    (LightTick.intents last crossed)
                    (MoveCreep("hauler", Right))
                    "and it takes it"
            }

            test "inward is straight in, else an inward diagonal, onto ground, never onto the ring" {
                let open' _ = true
                let wall (walled: Pos list) (tile: Pos) = not (List.contains tile walled)

                Expect.equal (LightTick.inward open' { X = 0; Y = 20 }) (Some Right) "west edge"
                Expect.equal (LightTick.inward open' { X = 49; Y = 20 }) (Some Left) "east edge"
                Expect.equal (LightTick.inward open' { X = 20; Y = 0 }) (Some Bottom) "north edge"
                Expect.equal (LightTick.inward open' { X = 20; Y = 49 }) (Some Top) "south edge"

                Expect.equal
                    (LightTick.inward (wall [ { X = 1; Y = 20 } ]) { X = 0; Y = 20 })
                    (Some TopRight)
                    "straight in walled: the first diagonal"

                Expect.equal
                    (LightTick.inward
                        (wall [ { X = 1; Y = 20 }; { X = 1; Y = 19 } ])
                        { X = 0; Y = 20 })
                    (Some BottomRight)
                    "and the other"

                Expect.isNone
                    (LightTick.inward
                        (wall [ { X = 1; Y = 19 }; { X = 1; Y = 20 }; { X = 1; Y = 21 } ])
                        { X = 0; Y = 20 })
                    "all three walled: none"

                Expect.equal
                    (LightTick.inward open' { X = 0; Y = 1 })
                    (Some Right)
                    "beside a corner, the diagonal onto the ring is never offered"

                Expect.isNone
                    (LightTick.inward open' { X = 20; Y = 20 })
                    "off the ring there is no inward step"
            }

            test "the CPU line's reason names the rule first, then where it fired" {
                Expect.equal
                    (LightTick.tag (LightForce.Hostile("W17S26", "Trepidimous")))
                    "hostile W17S26 Trepidimous"
                    "a hostile: the room and its owner"

                Expect.equal
                    (LightTick.tag (LightForce.OnBorder "hauler"))
                    "border hauler"
                    "the creep on the ring"

                Expect.equal
                    (LightTick.tag (LightForce.HitsLost "hauler"))
                    "hurt hauler"
                    "the creep hurt"

                Expect.equal
                    (LightTick.tag (LightForce.ControllerChanged "W1N1"))
                    "controller W1N1"
                    "the room"

                Expect.equal
                    (LightTick.tag (LightForce.Fought "W1N1"))
                    "fought W1N1"
                    "a fight names its colony"

                Expect.equal
                    (LightTick.tag (LightForce.CreepsChanged [ "hauler" ]))
                    "creeps"
                    "the creep census names nobody"
            }

            test
                "a creep that lost hits to nothing in sight forces a full tick, and one healed does not" {
                let hurt =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "hauler" (creepAt 21 20 499)
                    }

                let healed =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "hauler" (creepAt 21 20 600)
                    }

                Expect.equal
                    (LightTick.forced last hurt)
                    (Some(LightForce.HitsLost "hauler"))
                    "hurt by a tower or a body gone"

                Expect.equal (LightTick.forced last healed) None "healed"
            }

            test
                "a creep hurt by a body a striker of ours aims at and reaches, within the healing it gets, forces nothing; else a full tick" {
                // #549 review: the plan's fight is accounted for only while it
                // is fought from in reach and healed (#533's out-damaged test).
                let medic = { creepAt 11 11 1000 with Heals = 2 }

                let fighting =
                    { quiet with
                        Creeps =
                            quiet.Creeps
                            |> Map.add "guard" (creepAt 12 10 1000)
                            |> Map.add "medic" medic
                        Hostiles = [ hostile 13 10 "Trepidimous" true ]
                    }

                let full =
                    LightTick.lastFull
                        fighting
                        Map.empty
                        [ AttackCreep("guard", "h-1"); HealCreep("medic", "guard") ]

                let lost hits =
                    { fighting with
                        Creeps = fighting.Creeps |> Map.add "guard" (creepAt 12 10 (1000 - hits))
                    }

                Expect.equal
                    (LightTick.forced full (lost (2 * Engine.healPower)))
                    None
                    "struck back by its target, within the medic's heal"

                Expect.equal
                    (LightTick.forced full (lost (2 * Engine.healPower + 1)))
                    (Some(LightForce.HitsLost "guard"))
                    "out-damaged: a flight to decide"

                Expect.equal
                    (LightTick.forced
                        full
                        { lost 10 with
                            Creeps = (lost 10).Creeps |> Map.add "guard" (creepAt 15 10 990)
                        })
                    (Some(LightForce.HitsLost "guard"))
                    "its striker stepped out of reach of the target"

                Expect.equal
                    (LightTick.forced
                        (LightTick.lastFull fighting Map.empty [ HealCreep("medic", "guard") ])
                        (lost 10))
                    (Some(LightForce.HitsLost "guard"))
                    "struck by a body the plan only saw"
            }

            test
                "a crippled melee our guard strikes forces nothing, its blows on the guard included (#549)" {
                // User, 2026-10-06: W18S25's paralysed melee kept forcing full
                // ticks while our guard beat it.
                let cripple =
                    { hostile 13 10 "Trepidimous" true with
                        Gait = { Weight = 10; Moves = 0 }
                    }

                let beating =
                    { quiet with
                        Creeps =
                            quiet.Creeps |> Map.add "guard" { creepAt 12 10 1000 with Heals = 1 }
                        Hostiles = [ cripple ]
                    }

                let full =
                    LightTick.lastFull
                        beating
                        Map.empty
                        [ AttackCreep("guard", "h-1"); HealCreep("guard", "guard") ]
                    |> aiming [ AttackCreep("guard", "h-1") ] Map.empty

                let struck hits =
                    { beating with
                        Creeps =
                            beating.Creeps
                            |> Map.add
                                "guard"
                                { creepAt 12 10 (1000 - hits) with
                                    Heals = 1
                                }
                    }

                Expect.isEmpty
                    (LightTick.forcing full (struck Engine.healPower))
                    "the light tick swings again, its blows within the guard's heal"

                Expect.equal
                    (LightTick.forced full (struck 100))
                    (Some(LightForce.HitsLost "guard"))
                    "blows past the heal: the guard's colony decides"

                let struck = struck Engine.healPower

                Expect.contains
                    (LightTick.intents full struck)
                    (AttackCreep("guard", "h-1"))
                    "and does"

                Expect.isEmpty
                    (LightTick.forcing
                        (LightTick.lastFull { beating with Hostiles = [] } Map.empty [])
                        { beating with
                            Hostiles =
                                [
                                    { cripple with
                                        Weapon = None
                                        Armed = false
                                    }
                                ]
                        })
                    "disarmed, new beside the guard and struck by nothing, it reaches nothing at all"
            }

            test
                "an armed raider the plan already saw in reach of a creep forces nothing the tick after" {
                // Its reach unchanged since the plan: the plan accounted for it.
                let raided =
                    { stepped with
                        Hostiles = [ hostile 23 20 "Invader" true ]
                    }

                let full = LightTick.lastFull raided Map.empty []

                Expect.equal (LightTick.forced full raided) None "the same reach"

                Expect.equal
                    (LightTick.forced
                        full
                        { raided with
                            Hostiles = [ hostile 12 10 "Invader" true ]
                        })
                    (Some(LightForce.Hostile(room, "Invader")))
                    "come within a step and a swing of the digger it could not reach"
            }

            test "a controller whose level or safe mode changed forces a full tick" {
                let levelled =
                    { stepped with
                        Controllers = Map.ofList [ homeRoom, (6, false) ]
                    }

                let safe =
                    { stepped with
                        Controllers = Map.ofList [ homeRoom, (5, true) ]
                    }

                let lost = { stepped with Controllers = Map.empty }

                Expect.equal
                    (LightTick.forced last levelled)
                    (Some(LightForce.ControllerChanged homeRoom))
                    "level"

                Expect.equal
                    (LightTick.forced last safe)
                    (Some(LightForce.ControllerChanged homeRoom))
                    "safe mode"

                Expect.equal
                    (LightTick.forced last lost)
                    (Some(LightForce.ControllerChanged homeRoom))
                    "lost"
            }

            test "the reasons are read in the ADR's order" {
                let everything =
                    { stepped with
                        Hostiles = [ hostile 11 10 "Invader" true ]
                        Creeps = Map.ofList [ "digger", creepAt 10 10 1 ]
                        Controllers = Map.empty
                    }

                Expect.equal
                    (LightTick.forcing last everything)
                    [
                        LightForce.Hostile(room, "Invader")
                        LightForce.CreepsChanged [ "hauler" ]
                        LightForce.HitsLost "digger"
                        LightForce.ControllerChanged homeRoom
                    ]
                    "every fact, the hostile first"
            }

            test "a guard holding the exit a raid left by, with no hostile in reach, stays light" {
                // #450: the hold's ground is a step inside the border ring, never
                // on it, so the guard standing there trips neither `OnBorder` nor
                // anything a hostile would.
                let holding =
                    { quiet with
                        Creeps = Map.ofList [ "guard", creepAt 1 22 1000 ]
                    }

                let held = LightTick.lastFull holding Map.empty []

                Expect.isNone (LightTick.forced held holding) "nothing forces the tick full"
            }
        ]

/// Four colonies, each projecting its home and one outpost.
let private fourColonies =
    [
        "W1N1", Set.ofList [ "W1N1"; "W2N1" ]
        "W3N1", Set.ofList [ "W3N1"; "W4N1" ]
        "W5N1", Set.ofList [ "W5N1"; "W6N1" ]
        "W7N1", Set.ofList [ "W7N1"; "W8N1" ]
    ]

let private hostileIn roomName x y owner armed : GlanceHostile =
    {
        Id = "h-1"
        Tile = RoomPos.at roomName { X = x; Y = y }
        Owner = owner
        Armed = armed
        Heals = false
        Weapon = if armed then Some Engine.meleeRange else None
        Gait = mobile
        Breaks = false
        Hits = 1000
    }

[<Tests>]
let hurtingTests =
    testList
        "who can hurt us this tick"
        [
            test
                "an armed body within reach of a creep of ours can; one far off, a scout beside it and an ally cannot" {
                let near =
                    { hostile 12 10 "Trepidimous" true with
                        Id = "near"
                    }

                let far =
                    { hostile 45 45 "Trepidimous" true with
                        Id = "far"
                    }

                let scout =
                    { hostile 11 10 "Trepidimous" false with
                        Id = "scout"
                    }

                let ally =
                    { hostile 11 11 "Odiodin" true with
                        Id = "ally"
                    }

                let seen =
                    { quiet with
                        Hostiles = [ near; far; scout; ally ]
                    }

                Expect.equal
                    (LightTick.hurting seen |> List.map _.Id)
                    [ "near" ]
                    "the armed body in reach alone"
            }

            test "a body in a room under our safe mode can hurt nothing" {
                let seen =
                    { quiet with
                        Hostiles = [ hostile 12 10 "Trepidimous" true ]
                        Controllers = Map.ofList [ room, (5, true) ]
                    }

                Expect.isEmpty (LightTick.hurting seen) "safe mode holds it off"
            }

            test "a dismantler beside a structure of ours can, beside only a creep it cannot" {
                let breaker =
                    { hostile 31 30 "Trepidimous" false with
                        Breaks = true
                    }

                Expect.equal
                    (LightTick.hurting { quiet with Hostiles = [ breaker ] } |> List.length)
                    1
                    "beside the spawn"

                Expect.isEmpty
                    (LightTick.hurting
                        { quiet with
                            Hostiles = [ { breaker with Tile = tile 11 10 } ]
                        })
                    "beside the digger alone"
            }
        ]

[<Tests>]
let resetSplitTests =
    testList
        "reset split"
        [
            test
                "a reset with one threatened colony decides it first, then the rest in order up to half" {
                let seen =
                    { quiet with
                        Hostiles = [ hostileIn "W8N1" 25 25 "Trepidimous" true ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal
                    first
                    [ "W7N1"; "W1N1" ]
                    "the threatened colony, then the first in order"

                Expect.equal
                    rest
                    [ "W3N1"; "W5N1" ]
                    "the remaining colonies decide on the next tick"
            }

            test "an ally's or a keeper's armed body threatens nobody, and the split stays in order" {
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                hostileIn "W8N1" 25 25 "Odiodin" true
                                hostileIn "W6N1" 25 25 "Source Keeper" true
                            ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal first [ "W1N1"; "W3N1" ] "the first half in order"
                Expect.equal rest [ "W5N1"; "W7N1" ] "the second half"
            }

            test "an unarmed hostile that takes nothing down, beside our creep, threatens no colony" {
                // User, 2026-10-04: a body that cannot hurt a creep is no reason
                // to decide.
                let seen =
                    { quiet with
                        Creeps =
                            Map.ofList
                                [
                                    "miner",
                                    { creepAt 10 10 1000 with
                                        Tile = RoomPos.at "W6N1" { X = 10; Y = 10 }
                                    }
                                ]
                        Hostiles = [ hostileIn "W6N1" 12 10 "Trepidimous" false ]
                    }

                let first, _ = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal first [ "W1N1"; "W3N1" ] "the colonies in their order, none picked out"
            }

            test "more threatened colonies than half all decide first" {
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                hostileIn "W2N1" 25 25 "Invader" true
                                hostileIn "W4N1" 25 25 "Invader" true
                                hostileIn "W8N1" 25 25 "Invader" true
                            ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal first [ "W1N1"; "W3N1"; "W7N1" ] "every threatened colony"
                Expect.equal rest [ "W5N1" ] "only the quiet one waits"
            }

            test "a colony with a room the world danger log holds danger in decides first, unseen" {
                // A shut outpost watched by vision alone: nothing of the raid is
                // in sight on the reset tick, and the log still says whose it is.
                let first, rest =
                    LightTick.resetSplit 0.5 quiet (Set.ofList [ "W6N1" ]) fourColonies

                Expect.equal
                    first
                    [ "W5N1"; "W1N1" ]
                    "the endangered colony, then the first in order"

                Expect.equal rest [ "W3N1"; "W7N1" ] "the rest wait"
            }

            test "a lone colony decides on the reset tick" {
                let first, rest =
                    LightTick.resetSplit 0.5 quiet Set.empty [ List.head fourColonies ]

                Expect.equal first [ "W1N1" ] "one colony is its own half"
                Expect.isEmpty rest "nothing waits"
            }

            test "the tick after a reset that deferred colonies is full" {
                let deferring =
                    { LightTick.lastFull quiet Map.empty [] with
                        Deferred = Set.ofList [ "W5N1" ]
                    }

                Expect.equal
                    (LightTick.forced deferring quiet)
                    (Some LightForce.Deferred)
                    "a quiet world still decides the deferred colonies"
            }
        ]

/// Two colonies: W1N1 (and its outpost W2N1) holding the digger and the
/// hauler, W3N1 (and W4N1) holding a miner standing in W4N1 and a ranger
/// standing in W5N1, a room neither answers for.
let private pair =
    [ "W1N1", Set.ofList [ "W1N1"; "W2N1" ]; "W3N1", Set.ofList [ "W3N1"; "W4N1" ] ]

let private split: Glance =
    { quiet with
        Objects = Set.add "src-b" quiet.Objects
        Creeps =
            quiet.Creeps
            |> Map.add
                "miner"
                {
                    Tile = RoomPos.at "W4N1" { X = 10; Y = 10 }
                    Hits = 1000
                    Inward = None
                    Heals = 0
                }
            |> Map.add
                "ranger"
                {
                    Tile = RoomPos.at "W5N1" { X = 10; Y = 10 }
                    Hits = 500
                    Inward = None
                    Heals = 0
                }
    }

let private holders =
    Map.ofList [ "digger", "W1N1"; "hauler", "W1N1"; "miner", "W3N1"; "ranger", "W3N1" ]

/// Each colony's plan off `split`: the digger harvesting and the hauler a
/// step into a walk; the miner harvesting, the observer looking.
let private planned: LastFull =
    let planOf creeps steps intents =
        LightTick.plan split (Set.ofList creeps) steps [] intents [] Map.empty

    LightTick.ofPlans
        split
        Set.empty
        [
            "W1N1",
            planOf
                [ "digger"; "hauler" ]
                (Map.ofList [ "hauler", (tile 20 20, tile 21 20) ])
                [ HarvestSource("digger", "src-a") ]
            "W3N1",
            planOf
                [ "miner"; "ranger" ]
                Map.empty
                [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
        ]

[<Tests>]
let decidersTests =
    testList
        "forced tick decides by colony"
        [
            test
                "a hostile decides the colonies answering its room and those whose bodies stand there" {
                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.Hostile("W2N1", "Invader") ])
                    (Set.ofList [ "W1N1" ])
                    "W1N1's outpost: W1N1 alone"

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.Hostile("W5N1", "Invader") ])
                    (Set.ofList [ "W3N1" ])
                    "a room nobody answers for, where W3N1's ranger stands"
            }

            test "a creep's fact decides its holder, wherever it stands" {
                Expect.equal
                    (LightTick.deciders pair holders planned split [ LightForce.OnBorder "miner" ])
                    (Set.ofList [ "W3N1" ])
                    "the miner's colony"

                let crossing =
                    { split with
                        Creeps =
                            split.Creeps
                            |> Map.add
                                "miner"
                                {
                                    Tile = tile 15 15
                                    Hits = 900
                                    Inward = None
                                    Heals = 0
                                }
                    }

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        crossing
                        [ LightForce.HitsLost "miner" ])
                    (Set.ofList [ "W3N1" ])
                    "hurt crossing W1N1: its own colony decides its flight"

                Expect.equal
                    (LightTick.deciders
                        pair
                        (Map.remove "hauler" holders |> Map.add "next" "W3N1")
                        planned
                        split
                        [ LightForce.CreepsChanged [ "hauler" ] ])
                    (Set.ofList [ "W1N1" ])
                    "a body gone: the colony whose plan held it"

                Expect.equal
                    (LightTick.deciders
                        pair
                        (Map.add "next" "W3N1" holders)
                        planned
                        split
                        [ LightForce.CreepsChanged [ "next" ] ])
                    (Set.ofList [ "W3N1" ])
                    "a body born: its holder"
            }

            test
                "a fight decides its colony; a fact no colony owns, everyone; a colony with no plan always decides" {
                Expect.equal
                    (LightTick.deciders pair holders planned split [ LightForce.Fought "W3N1" ])
                    (Set.ofList [ "W3N1" ])
                    "the colony whose tower fought"

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.ControllerChanged "W9N9" ])
                    (Set.ofList [ "W1N1"; "W3N1" ])
                    "nobody's room: everyone"

                Expect.equal
                    (LightTick.deciders
                        (pair @ [ "W7N1", Set.ofList [ "W7N1" ] ])
                        holders
                        planned
                        split
                        [ LightForce.Fought "W3N1" ])
                    (Set.ofList [ "W3N1"; "W7N1" ])
                    "W7N1 holds no plan"
            }

            test "a replayed colony issues its own plan for its own bodies, and its looks" {
                Expect.equal
                    (LightTick.replay (Set.ofList [ "W3N1" ]) holders planned split)
                    [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
                    "the miner and the observer; nothing of W1N1's"

                Expect.equal
                    (LightTick.replay (Set.ofList [ "W1N1" ]) holders planned split)
                    [ HarvestSource("digger", "src-a"); MoveCreep("hauler", Right) ]
                    "W1N1's harvest and step"

                Expect.equal
                    (LightTick.replay
                        (Set.ofList [ "W3N1" ])
                        (Map.add "hauler" "W3N1" holders)
                        planned
                        split)
                    [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
                    "a body W3N1 adopted whose plan was W1N1's stands"

                Expect.equal
                    (LightTick.intents planned split)
                    [
                        HarvestSource("digger", "src-a")
                        MoveCreep("hauler", Right)
                        HarvestSource("miner", "src-b")
                        ObserveRoom("obs-1", "W6N1")
                    ]
                    "a light tick replays every plan"
            }

            test "a forced tick's fresh plan claims a body before the plan it carried" {
                // A body a deciding colony adopted is that colony's alone: two
                // plans replaying one body would lay two acts on its channels.
                let fresh =
                    LightTick.plan
                        split
                        (Set.ofList [ "hauler" ])
                        Map.empty
                        []
                        [ PickupPile("hauler", "pile") ]
                        []
                        Map.empty

                let carried = planned.Plans["W1N1"]
                let record = LightTick.ofPlans split Set.empty [ "W3N1", fresh; "W1N1", carried ]

                Expect.equal
                    record.Plans["W1N1"].Creeps
                    (Set.ofList [ "digger" ])
                    "the carried plan less the hauler"

                Expect.isEmpty record.Plans["W1N1"].Steps "and its step plan"

                Expect.equal
                    (LightTick.intents record split)
                    [ HarvestSource("digger", "src-a"); PickupPile("hauler", "pile") ]
                    "the hauler's pickup alone"
            }
        ]

/// `stepped` with our controller in `room`: a home.
let private atHome (hostiles: GlanceHostile list) : Glance =
    { stepped with
        Controllers = Map.ofList [ room, (5, false) ]
        Hostiles = hostiles
    }

[<Tests>]
let homeTests =
    testList
        "a raided home is never a tick late"
        [
            test
                "a raider anywhere in a home forces every tick, the plan having seen it or not; its colony alone decides" {
                // #549 review: a drain then a raze (Meow) and #545's razed
                // economy are read on the tick they happen, at home.
                let far = atHome [ hostile 45 45 "Trepidimous" true ]
                let seen = LightTick.lastFull far Map.empty []

                Expect.equal
                    (LightTick.forced seen far)
                    (Some(LightForce.Home(room, "Trepidimous")))
                    "far across the home, already seen"

                Expect.equal
                    (LightTick.deciders
                        [ room, Set.ofList [ room ]; "W3N1", Set.ofList [ "W3N1"; room ] ]
                        Map.empty
                        (LightTick.ofPlans
                            far
                            Set.empty
                            [
                                for home in [ room; "W3N1" ] ->
                                    home, LightTick.plan far Set.empty Map.empty [] [] [] Map.empty
                            ])
                        far
                        [ LightForce.Home(room, "Trepidimous") ])
                    (Set.ofList [ room ])
                    "the home's own colony, not a mother answering it too"

                Expect.equal
                    (LightTick.tag (LightForce.Home(room, "Trepidimous")))
                    "home W1N1 Trepidimous"
                    "tagged"
            }

            test
                "a healer forces at home; a keeper, an ally, a scout or a body under our safe mode does not" {
                let healer =
                    { hostile 45 45 "Trepidimous" false with
                        Heals = true
                    }

                let quietHome glance =
                    LightTick.forcing (LightTick.lastFull glance Map.empty []) glance
                    |> List.filter (function
                        | LightForce.Home _ -> true
                        | _ -> false)

                Expect.isNonEmpty (quietHome (atHome [ healer ])) "a raid's sustain"

                Expect.isEmpty
                    (quietHome (atHome [ hostile 45 45 "Source Keeper" true ]))
                    "a keeper"

                Expect.isEmpty (quietHome (atHome [ hostile 45 45 "Odiodin" true ])) "an ally"
                Expect.isEmpty (quietHome (atHome [ hostile 45 45 "Mirroar" false ])) "a scout"

                Expect.isEmpty
                    (quietHome
                        { atHome [ hostile 45 45 "Trepidimous" true ] with
                            Controllers = Map.ofList [ room, (5, true) ]
                        })
                    "held off by our safe mode"
            }
        ]
