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
    }

/// A quiet world: two creeps of ours, a spawn, one controller at level 5, and
/// nobody else. Every object an act below names still stands, as the glance
/// looks it up (`LightTick.lookups`).
let private quiet: Glance =
    {
        Creeps = Map.ofList [ "digger", creepAt 10 10 1000; "hauler", creepAt 20 20 500 ]
        Hostiles = []
        Structures = [ tile 30 30 ]
        Controllers = Map.ofList [ room, (5, false) ]
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

/// A hostile at a MOVE a part: never stationary.
let private mobile: Gait = { Weight = 1; Moves = 1 }

let private hostile x y owner armed : GlanceHostile =
    {
        Id = "h-1"
        Tile = tile x y
        Owner = owner
        Armed = armed
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

                    Expect.isEmpty full.Acts $"%A{oneShot} is not kept"
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
                    { LightTick.lastFull guarded Map.empty [ heal ] with
                        Aims = [ aim ]
                    }

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
                    { LightTick.lastFull
                          (shooting [])
                          Map.empty
                          [ RangedAttackCreep("ranger", "h-1") ] with
                        Aims =
                            [
                                RangedAttackCreep("ranger", "h-1")
                                RangedAttackCreep("ranger", "h-2")
                            ]
                        Volleys = volleys
                    }

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
                    { LightTick.lastFull
                          guarded
                          (Map.ofList [ "guard", (tile 12 10, tile 13 10) ])
                          [ AttackCreep("guard", "h-head") ] with
                        Aims = [ AttackCreep("guard", "h-head"); AttackCreep("guard", "h-near") ]
                    }

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
                        { LightTick.lastFull now Map.empty [] with
                            Aims = [ AttackCreep(name, "h-1") ]
                        }

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
                    { LightTick.lastFullWith
                          guarded
                          Map.empty
                          [ TransferEnergyToStructure("digger", "ext-9", Energy) ]
                          [
                              HarvestSource("digger", "src-a")
                              TransferEnergyToStructure("digger", "link-1", Energy)
                              RepairStructure("guard", "road")
                          ] with
                        Aims = [ AttackCreep("guard", "h-1"); RangedAttackCreep("hauler", "h-1") ]
                    }

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

                Expect.equal full.Acts acts "each, in order"
                Expect.isFalse full.Fought "none of them a structure's fight"
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
                    { LightTick.lastFull guarded Map.empty [] with
                        Aims = [ AttackCreep("guard", "h-1") ]
                    }

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

            test "an armed hostile in a home room, far from everything, forces a full tick" {
                let raided =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Invader" true ]
                    }

                Expect.equal
                    (LightTick.forced last raided)
                    (Some(LightForce.ArmedHostile(room, "Invader")))
                    "combat at home is decided"
            }

            // The Brief's S7, rule 3: the glance asks "armed" as the full tick
            // does, so a healer is not; it used to count one and force a full
            // tick that then found nothing armed.
            test "a heal-only body at home, glanced, forces no full tick; a shooter beside it does" {
                let medic: HostileInfo =
                    {
                        Id = "medic"
                        Owner = "Trepidimous"
                        Pos = tile 45 45
                        Body = [ Move; Move; Heal; Heal ]
                        Hits = 400
                        TicksToLive = 1000
                    }

                let shooter =
                    { medic with
                        Id = "shooter"
                        Pos = tile 46 45
                        Body = [ Move; RangedAttack ]
                        Hits = 200
                    }

                let glanced side =
                    { stepped with
                        Hostiles = side |> List.map (LightTick.glanceOf side)
                    }

                Expect.equal (LightTick.forced last (glanced [ medic ])) None "the healer alone"

                Expect.equal
                    (LightTick.forced last (glanced [ medic; shooter ]))
                    (Some(LightForce.ArmedHostile(room, "Trepidimous")))
                    "the shooter beside it is decided"
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
                    (LightTick.forced full (defenderAt (10 + Engine.rangedRange)))
                    (Some(LightForce.HostileNear(harassed, "Trepidimous")))
                    "trading shots with our ranger at range 3: the near rule"

                Expect.equal
                    (LightTick.forced full (defenderAt (10 + Engine.rangedRange + 2)))
                    (Some(LightForce.HostileNear(harassed, "Trepidimous")))
                    "at the near rule's edge: still full"
            }

            test
                "a hostile near our ranger in a loose room forces no full tick; near our anchor or at home it does" {
                // #520: live t963,4xx, 98% of ticks forced full by harass
                // rangers standing by Trepidimous' creeps in W19S25 and W18S27.
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
                                }
                            |> Map.add
                                "anchor"
                                {
                                    Tile = at outpost 10
                                    Hits = 500
                                    Inward = None
                                }
                    }

                // The ranger shot on the full tick: its target still beside it
                // is no fight to decide there either.
                let full =
                    { LightTick.lastFull ours Map.empty [ RangedAttackCreep("ranger", "trep") ] with
                        Loose = Set.ofList [ harassed ]
                        Rangers = Set.ofList [ "ranger" ]
                    }

                let trepAt tile =
                    { ours with
                        Hostiles =
                            [
                                {
                                    Id = "trep"
                                    Tile = tile
                                    Owner = "Trepidimous"
                                    Armed = true
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
                    (Some(LightForce.HostileNear(outpost, "Trepidimous")))
                    "the same body beside our anchor in an outpost"

                // A lean bucket (2026-10-05): an outpost's loiterer stops
                // holding every tick full; home keeps every rule.
                Expect.equal
                    (LightTick.forced
                        full
                        { trepAt (at outpost 12) with
                            Lean = true
                        })
                    None
                    "on a lean bucket the outpost's body forces nothing"

                Expect.equal
                    (LightTick.forced full { trepAt (tile 12 10) with Lean = true })
                    (Some(LightForce.ArmedHostile(room, "Trepidimous")))
                    "and home still decides"

                // The armed rule reads home first; the room stays a full tick's.
                Expect.equal
                    (LightTick.forced full (trepAt (tile 12 10)))
                    (Some(LightForce.ArmedHostile(room, "Trepidimous")))
                    "the same body beside our digger at home"

                Expect.equal
                    (LightTick.forced
                        { full with
                            Loose = Set.ofList [ harassed; room ]
                        }
                        (trepAt (tile 12 10)))
                    (Some(LightForce.ArmedHostile(room, "Trepidimous")))
                    "a loose room passes the near rule over, never the armed one"

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
                                }
                            |> Map.add
                                "ranger"
                                {
                                    Tile = at harassed 32
                                    Hits = 500
                                    Inward = None
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
                    (Some(LightForce.HostileNear(harassed, "Trepidimous")))
                    "a hauler crossing the harassment room beside a Trepidimous melee: our bodies there are not rangers alone"
            }

            test
                "a harass ranger in a loose room shoots and heals itself again on a light tick, wherever it stepped" {
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
                                }
                    }

                let full =
                    { LightTick.lastFull
                          (withRanger 10)
                          Map.empty
                          [
                              RangedAttackCreep("ranger", "trep")
                              HealCreep("ranger", "ranger")
                              RangedAttackCreep("digger", "h")
                              HealCreep("digger", "hauler")
                          ] with
                        Loose = Set.ofList [ harassed ]
                        Rangers = Set.ofList [ "ranger" ]
                    }

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
                // heal whose target still stands, a loose room or not.
                Expect.equal
                    (replayed trepSeen full)
                    [
                        HealCreep("digger", "hauler")
                        RangedAttackCreep("ranger", "trep")
                        HealCreep("ranger", "ranger")
                    ]
                    "the shot and the self-heal a step off the tile they were decided on; the digger's heal on a hauler still standing, its shot at a body out of sight never"

                Expect.equal
                    (replayed trepSeen { full with Loose = Set.empty })
                    (replayed trepSeen full)
                    "outside a loose room alike"
            }

            test "the harass rangers are the bodies holding a harassment room's Guard" {
                let roles: LightTick.RoomRoles =
                    {
                        Worked = Set.ofList [ "W1N1"; "W3N1"; "W4N1" ]
                        Mined = Set.empty
                        Harassed = Set.ofList [ "W3N1" ]
                        Evacuating = Set.empty
                        Assignments =
                            Map.ofList
                                [
                                    "ranger", Decide.Facts.taskId (Guard "W3N1")
                                    "guard", Decide.Facts.taskId (Guard "W4N1")
                                    "digger", Decide.Facts.taskId (Harvest "src-a")
                                ]
                    }

                Expect.equal
                    (LightTick.harassRangers [ roles ])
                    (Set.ofList [ "ranger" ])
                    "the ranger alone"
            }

            test "a loose room is a harassment room no colony works for anything else" {
                let roles home worked harassed : LightTick.RoomRoles =
                    {
                        Worked = Set.ofList (home :: worked @ harassed)
                        Mined = Set.ofList worked
                        Harassed = Set.ofList harassed
                        Evacuating = Set.empty
                        Assignments = Map.empty
                    }

                Expect.equal
                    (LightTick.loose [ roles "W1N1" [ "W2N1" ] [ "W3N1" ] ])
                    (Set.ofList [ "W3N1" ])
                    "a room only harassed is loose"

                Expect.isEmpty
                    (LightTick.loose [ roles "W1N1" [ "W3N1" ] [ "W3N1" ] ])
                    "a harassment room the colony also mines keeps the near rule"

                Expect.isEmpty
                    (LightTick.loose
                        [
                            roles "W1N1" [] [ "W3N1" ]
                            { roles "W5N1" [] [] with
                                Worked = Set.ofList [ "W5N1"; "W3N1" ]
                            }
                        ])
                    "a harassment room another colony works (an errand there) keeps the near rule"

                Expect.isEmpty
                    (LightTick.loose
                        [
                            roles "W1N1" [] [ "W3N1" ]
                            // W5N1's outpost, shut: out of its scan set.
                            { roles "W5N1" [] [] with
                                Mined = Set.ofList [ "W3N1" ]
                            }
                        ])
                    "a harassment room any colony declares as an outpost keeps the near rule, shut or not"

                Expect.isEmpty
                    (LightTick.loose
                        [
                            { roles "W1N1" [] [ "W3N1" ] with
                                Evacuating = Set.ofList [ "W3N1" ]
                            }
                        ])
                    "an evacuating room keeps the near rule"

                Expect.isEmpty
                    (LightTick.loose [ roles "W1N1" [] [ "W3N1" ]; roles "W3N1" [] [] ])
                    "a colony's home keeps the near rule"
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

            test "any hostile within ranged range plus two of a creep of ours forces a full tick" {
                let edge = Engine.rangedRange + 2

                let near =
                    { stepped with
                        Hostiles = [ hostile (10 + edge) 10 "Source Keeper" true ]
                    }

                let far =
                    { stepped with
                        Hostiles = [ hostile (10 + edge + 1) 10 "Rival" false ]
                    }

                Expect.equal
                    (LightTick.forced last near)
                    (Some(LightForce.HostileNear(room, "Source Keeper")))
                    "a keeper that comes near"

                Expect.equal (LightTick.forced last far) None "one tile further is not near"
            }

            test
                "a hostile that can dismantle or claim, near a structure of ours, forces a full tick" {
                let atTheSpawn breaks =
                    { stepped with
                        Hostiles =
                            [
                                { hostile 33 33 "Rival" false with
                                    Breaks = breaks
                                }
                            ]
                    }

                Expect.equal
                    (LightTick.forced last (atTheSpawn true))
                    (Some(LightForce.HostileNear(room, "Rival")))
                    "a WORK or CLAIM body at the gate"

                Expect.equal
                    (LightTick.forced last (atTheSpawn false))
                    None
                    "a hauler at the gate takes nothing down"
            }

            test "a stationary hostile is near within a step past its weapon's range" {
                // Live t964,5xx: Trepidimous' 14A1M sat 4 tiles from our hauler in
                // W18S25 and held ~31% of ticks full (#521). No structure of ours
                // in the room, so it is the near rule's and not the armed rule's.
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
                    forced = Some(LightForce.HostileNear(room, "Trepidimous"))

                let rootedSwing = 14, Attack, 1
                let rootedShot = 14, RangedAttack, 1

                Expect.isFalse (nearAt rootedSwing 3) "a rooted swing three tiles off"
                Expect.isFalse (nearAt rootedSwing 4) "a rooted swing four tiles off"
                Expect.isTrue (nearAt rootedSwing 2) "a rooted swing a step and a swing off"
                Expect.isTrue (nearAt rootedShot 4) "a rooted shot a step and a shot off"
                Expect.isFalse (nearAt rootedShot 5) "a rooted shot five tiles off"

                Expect.isTrue
                    (nearAt (10, Attack, 10) 4)
                    "a body at a MOVE a part keeps the usual reach"
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
                             Some(LightForce.HostileNear(room, "Mirroar"))
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

                    Expect.isTrue fought.Fought $"%A{fight} is a fight"
                    Expect.isEmpty (LightTick.intents fought quiet) $"%A{fight} is never replayed"

                    Expect.equal
                        (LightTick.forced fought quiet)
                        (Some LightForce.Fought)
                        $"%A{fight} forces the next tick full"
            }

            test
                "a creep's shot or heal forces nothing on its own, and is replayed while its target stands" {
                // A target still in reach forces the tick on the near rule;
                // one dead or gone leaves nothing to decide.
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

                    Expect.isFalse full.Fought $"%A{shot} is no structure's fight"
                    Expect.equal (LightTick.forced full quiet) None $"%A{shot} forces nothing"
                    Expect.equal (LightTick.intents full seen) [ shot ] $"%A{shot} is replayed"

                    // The hostile out of sight, and the healed body gone.
                    let gone =
                        { seen with
                            Hostiles = []
                            Creeps = seen.Creeps |> Map.remove "hauler"
                        }

                    Expect.isEmpty (LightTick.intents full gone) $"%A{shot}: its target gone, never"

                // And a hostile still within reach forces the tick all the same.
                let shooting =
                    LightTick.lastFull quiet Map.empty [ RangedAttackCreep("digger", "h") ]

                let stillThere =
                    { quiet with
                        Hostiles = [ hostile 12 12 "Enemy" false ]
                    }

                Expect.equal
                    (LightTick.forced shooting stillThere)
                    (Some(LightForce.HostileNear(room, "Enemy")))
                    "the target still near is the near rule's"
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

                Expect.equal (LightTick.forced last born) (Some LightForce.CreepsChanged) "born"
                Expect.equal (LightTick.forced last gone) (Some LightForce.CreepsChanged) "gone"
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
                    (LightTick.tag (LightForce.ArmedHostile("W17S26", "Trepidimous")))
                    "armed W17S26 Trepidimous"
                    "an armed hostile: the room and its owner"

                Expect.equal
                    (LightTick.tag (LightForce.HostileNear("W1N1", "Invader")))
                    "near W1N1 Invader"
                    "near"

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

                Expect.equal (LightTick.tag LightForce.Fought) "fought" "a fight names nobody"

                Expect.equal
                    (LightTick.tag LightForce.CreepsChanged)
                    "creeps"
                    "nor does the creep census"
            }

            test "a creep that lost hits forces a full tick, and one healed does not" {
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
                    "hurt"

                Expect.equal (LightTick.forced last healed) None "healed"
            }

            test "a controller whose level or safe mode changed forces a full tick" {
                let levelled =
                    { stepped with
                        Controllers = Map.ofList [ room, (6, false) ]
                    }

                let safe =
                    { stepped with
                        Controllers = Map.ofList [ room, (5, true) ]
                    }

                let lost = { stepped with Controllers = Map.empty }

                Expect.equal
                    (LightTick.forced last levelled)
                    (Some(LightForce.ControllerChanged room))
                    "level"

                Expect.equal
                    (LightTick.forced last safe)
                    (Some(LightForce.ControllerChanged room))
                    "safe mode"

                Expect.equal
                    (LightTick.forced last lost)
                    (Some(LightForce.ControllerChanged room))
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
                    (LightTick.forced last everything)
                    (Some(LightForce.ArmedHostile(room, "Invader")))
                    "the armed hostile comes first"
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
        Weapon = if armed then Some Engine.meleeRange else None
        Gait = mobile
        Breaks = false
        Hits = 1000
    }

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

                let first, rest = LightTick.resetSplit 0.5 seen fourColonies

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

                let first, rest = LightTick.resetSplit 0.5 seen fourColonies

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

                let first, _ = LightTick.resetSplit 0.5 seen fourColonies

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

                let first, rest = LightTick.resetSplit 0.5 seen fourColonies

                Expect.equal first [ "W1N1"; "W3N1"; "W7N1" ] "every threatened colony"
                Expect.equal rest [ "W5N1" ] "only the quiet one waits"
            }

            test "a lone colony decides on the reset tick" {
                let first, rest = LightTick.resetSplit 0.5 quiet [ List.head fourColonies ]

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
