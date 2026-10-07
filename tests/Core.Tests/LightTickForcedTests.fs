/// Every fact that hands a quiet tick over to a full one instead: a hostile
/// that can hurt us, a structure's fight, a creep born, gone, hurt or on the
/// border ring, a controller changed, a chase, and a raided home.
module Fabot.Core.Tests.LightTickForcedTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick
open Fabot.Core.Tests.LightTickFixtures

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
                        Boosts = []
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

                let breaker =
                    { medic with
                        Id = "breaker"
                        Body = [ Move; Work ]
                    }

                Expect.equal
                    (LightTick.forced last (glanced [ breaker ]))
                    home
                    "a dismantler alone, as the towers may be drained"

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
                                    Inward = []
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
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
                                    Inward = []
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
                                }
                            |> Map.add
                                "anchor"
                                {
                                    Tile = at outpost 10
                                    Hits = 500
                                    Inward = []
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
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

                // The lean stop-gaps (2026-10-05) are gone: a later light
                // tick of a longer cadence reads the same invariant.
                Expect.equal
                    (LightTick.forced
                        full
                        { trepAt (at outpost 12) with
                            Time = Tuning.maxCadence - 1
                        })
                    (Some(LightForce.Hostile(outpost, "Trepidimous")))
                    "the last light tick of the longest cadence alike"

                let crossing =
                    { trepAt (at harassed 12) with
                        Creeps =
                            ours.Creeps
                            |> Map.add
                                "hauler"
                                {
                                    Tile = at harassed 30
                                    Hits = 500
                                    Inward = []
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
                                }
                            |> Map.add
                                "ranger"
                                {
                                    Tile = at harassed 32
                                    Hits = 500
                                    Inward = []
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
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
                                    Inward = []
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
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
                "a ranged body walking in forces on the tick it can shoot after a step, and again on the tick it can shoot, on any light tick of the cadence (#549 review)" {
                // A flat ring keyed once per room read 5, 4 and 3 as one fact:
                // the plan made at 5 never saw the shot at 3.
                let shooterAt run x =
                    { stepped with
                        Time = run
                        Hostiles =
                            [
                                { hostile x 10 "Trepidimous" true with
                                    Weapon = Some Engine.rangedRange
                                }
                            ]
                    }

                for run in 1 .. Tuning.maxCadence - 1 do
                    let at5 = shooterAt run 15
                    let at4 = shooterAt run 14
                    let at3 = shooterAt run 13

                    Expect.isEmpty
                        (LightTick.forcing (LightTick.lastFull at5 Map.empty []) at5)
                        $"run {run}: five off, out of a shot after a step"

                    Expect.equal
                        (LightTick.forced (LightTick.lastFull at5 Map.empty []) at4)
                        (Some(LightForce.Hostile(room, "Trepidimous")))
                        $"run {run}: four off, a shot after one step"

                    Expect.equal
                        (LightTick.forced (LightTick.lastFull at4 Map.empty []) at3)
                        (Some(LightForce.Hostile(room, "Trepidimous")))
                        $"run {run}: three off, a shot now, though the plan saw it a step away"

                    Expect.isEmpty
                        (LightTick.forcing (LightTick.lastFull at3 Map.empty []) at3)
                        $"run {run}: the plan made at three accounts for it"
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

            // Characterisation (movement S0, `docs/adr/0088`): light-tick forcing
            // at the weapon's range plus one step (`LightTick.strikes`). Flips at
            // S5, where it reads the margin recorded at the planned tile.
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
                                        Inward = []
                                        Heals = 0
                                        Strikes = 0
                                        Gait = None
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
                                    Inward = [ Right ]
                                    Heals = 0
                                    Strikes = 0
                                    Gait = None
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

            // #562, live t987,159-987,529: a hauler crossing into W17S25
            // stepped straight in onto an idle guard's tile, failed, ended on
            // the ring and was carried back, every light tick.
            test
                "#562 a creep on the ring takes an inward tile no body stands on, and forces a full tick when none is free" {
                // The digger is the body of ours in the way, an unarmed
                // stranger the body not ours.
                let crossed (digger: int * int) (strangers: (int * int) list) =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add "digger" (creepAt (fst digger) (snd digger) 1000)
                            |> Map.add
                                "hauler"
                                { creepAt 0 20 500 with
                                    Inward = [ Right; TopRight; BottomRight ]
                                }
                        Hostiles = strangers |> List.map (fun (x, y) -> hostile x y "Someone" false)
                    }

                let straightHeld = crossed (1, 20) []

                Expect.equal
                    (LightTick.forced last straightHeld)
                    None
                    "a free diagonal: the tick stays light"

                Expect.contains
                    (LightTick.intents last straightHeld)
                    (MoveCreep("hauler", TopRight))
                    "and the hauler takes it rather than the guard's tile"

                Expect.isFalse
                    (LightTick.intents last straightHeld
                     |> List.contains (MoveCreep("hauler", Right)))
                    "never the step into the guard"

                Expect.contains
                    (LightTick.intents last (crossed (1, 20) [ 1, 19 ]))
                    (MoveCreep("hauler", BottomRight))
                    "a body not ours holds a tile too"

                Expect.equal
                    (LightTick.forced last (crossed (1, 20) [ 1, 19; 1, 21 ]))
                    (Some(LightForce.OnBorder "hauler"))
                    "all three held: the tick is full, and the arbitration settles it"
            }

            test
                "#562 two crossers never pick one inward tile: the second picks another or forces a full tick" {
                // The digger on (0,22) and the hauler on (0,20) share (1,21)
                // as a diagonal; strangers hold the rest but (1,19).
                let crossed strangers =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "digger"
                                { creepAt 0 22 1000 with
                                    Inward = [ Right; TopRight; BottomRight ]
                                }
                            |> Map.add
                                "hauler"
                                { creepAt 0 20 500 with
                                    Inward = [ Right; TopRight; BottomRight ]
                                }
                        Hostiles = strangers |> List.map (fun (x, y) -> hostile x y "Someone" false)
                    }

                let oneFree = crossed [ 1, 20; 1, 22; 1, 23 ]
                let moves = LightTick.intents last oneFree

                Expect.contains
                    moves
                    (MoveCreep("digger", TopRight))
                    "the digger, first by name, takes (1,21)"

                Expect.contains
                    moves
                    (MoveCreep("hauler", TopRight))
                    "and the hauler (1,19), not (1,21) behind it"

                Expect.equal
                    (LightTick.forcing last (crossed [ 1, 19; 1, 20; 1, 22; 1, 23 ]))
                    [ LightForce.OnBorder "hauler" ]
                    "with (1,19) held too the hauler has nothing: the tick is full"
            }

            test "inward is straight in, else an inward diagonal, onto ground, never onto the ring" {
                let open' _ = true
                let wall (walled: Pos list) (tile: Pos) = not (List.contains tile walled)

                // #562: every step the terrain allows, in that order, so the
                // light tick can pass over one a body stands on.
                Expect.equal
                    (LightTick.inward open' { X = 0; Y = 20 })
                    [ Right; TopRight; BottomRight ]
                    "west edge"

                Expect.equal
                    (LightTick.inward open' { X = 49; Y = 20 })
                    [ Left; TopLeft; BottomLeft ]
                    "east edge"

                Expect.equal
                    (LightTick.inward open' { X = 20; Y = 0 })
                    [ Bottom; BottomRight; BottomLeft ]
                    "north edge"

                Expect.equal
                    (LightTick.inward open' { X = 20; Y = 49 })
                    [ Top; TopRight; TopLeft ]
                    "south edge"

                Expect.equal
                    (LightTick.inward (wall [ { X = 1; Y = 20 } ]) { X = 0; Y = 20 })
                    [ TopRight; BottomRight ]
                    "straight in walled: the diagonals"

                Expect.equal
                    (LightTick.inward
                        (wall [ { X = 1; Y = 20 }; { X = 1; Y = 19 } ])
                        { X = 0; Y = 20 })
                    [ BottomRight ]
                    "and the other"

                Expect.isEmpty
                    (LightTick.inward
                        (wall [ { X = 1; Y = 19 }; { X = 1; Y = 20 }; { X = 1; Y = 21 } ])
                        { X = 0; Y = 20 })
                    "all three walled: none"

                Expect.equal
                    (LightTick.inward open' { X = 0; Y = 1 })
                    [ Right; BottomRight ]
                    "beside a corner, the diagonal onto the ring is never offered"

                Expect.isEmpty
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

            test
                "a fighter its plan stands while the unarmed body it aims at walks off forces its tick; one stepping on, or an armed quarry, does not (#619)" {
                // Live W17S26 t1,017,287: a light tick replayed two rangers'
                // stand while the hauler they chased stepped away.
                let chasing =
                    { quiet with
                        Creeps = Map.ofList [ "ranger", creepAt 12 10 1000 ]
                        Hostiles = [ hostile 13 10 "Trepidimous" false ]
                    }

                let full steps =
                    LightTick.lastFull chasing steps [ RangedAttackCreep("ranger", "h-1") ]
                    |> aiming [ RangedAttackCreep("ranger", "h-1") ] Map.empty

                let walkedOff =
                    { chasing with
                        Hostiles = [ hostile 14 10 "Trepidimous" false ]
                    }

                Expect.isNone
                    (LightTick.forced (full Map.empty) chasing)
                    "a quarry standing forces nothing"

                Expect.equal
                    (LightTick.forced (full Map.empty) walkedOff)
                    (Some(LightForce.Chase "ranger"))
                    "a quarry walked off a stood chaser: decide the chase"

                Expect.isNone
                    (LightTick.forced
                        (full (Map.ofList [ "ranger", [ tile 12 10; tile 13 10 ] ]))
                        walkedOff)
                    "a chaser its plan steps on walks after it"

                let armed =
                    { chasing with
                        Hostiles = [ hostile 13 10 "Trepidimous" true ]
                    }

                Expect.isNone
                    (LightTick.forced
                        (LightTick.lastFull armed Map.empty []
                         |> aiming [ RangedAttackCreep("ranger", "h-1") ] Map.empty)
                        { armed with
                            Hostiles = [ hostile 15 10 "Trepidimous" true ]
                        })
                    "an armed quarry stepping off is the kite's"

                Expect.equal
                    (LightTick.tag (LightForce.Chase "ranger"))
                    "chase ranger"
                    "its CPU line"
            }

            test "a chase forces its tick only while the chaser can close on its quarry (#626)" {
                // Live W18S27 t1,021,5xx: a ranger kiting two armed rangers,
                // its aim on a scout five off that stepped every tick, forced
                // 61 fight-only ticks of 100.
                let scout x y : GlanceHostile =
                    { hostile x y "Trepidimous" false with
                        Id = "scout"
                    }

                let ranged x y : GlanceHostile =
                    { hostile x y "Trepidimous" true with
                        Id = "ranger-t"
                        Weapon = Some Engine.rangedRange
                    }

                let ranger x =
                    { creepAt x 10 1000 with
                        Gait = Some { Weight = 1; Moves = 1 }
                    }

                let at hostiles =
                    { quiet with
                        Creeps = Map.ofList [ "ranger", ranger 12 ]
                        Hostiles = hostiles
                    }

                let full glance =
                    LightTick.lastFull glance Map.empty [ RangedAttackCreep("ranger", "scout") ]
                    |> aiming [ RangedAttackCreep("ranger", "scout") ] Map.empty

                let forcedAfter before after =
                    LightTick.forced (full (at before)) (at after)

                Expect.equal
                    (forcedAfter [ scout 14 10 ] [ scout 15 10 ])
                    (Some(LightForce.Chase "ranger"))
                    "a quarry a step past its reach: decide the chase"

                Expect.isNone
                    (forcedAfter [ scout 14 10; ranged 18 10 ] [ scout 15 10; ranged 18 10 ])
                    "an armed body that strikes it within the cadence: the stand is the kite's"

                Expect.equal
                    (forcedAfter [ scout 14 10; ranged 30 10 ] [ scout 15 10; ranged 30 10 ])
                    (Some(LightForce.Chase "ranger"))
                    "an armed body across the room menaces nothing"

                // A ranged body six off: rooted, it walks nowhere by the
                // cadence; at two ticks a tile, two tiles: range 5 either way.
                let gaited gait x = { ranged x 10 with Gait = gait }

                let rooted = gaited { Weight = 10; Moves = 1 }
                let paced = gaited { Weight = 2; Moves = 1 }

                Expect.equal
                    (forcedAfter [ scout 14 10; rooted 18 ] [ scout 15 10; rooted 18 ])
                    (Some(LightForce.Chase "ranger"))
                    "a rooted body six off menaces nothing"

                Expect.equal
                    (forcedAfter [ scout 14 10; paced 18 ] [ scout 15 10; paced 18 ])
                    (Some(LightForce.Chase "ranger"))
                    "a body two ticks a tile, six off, reaches five by the cadence"

                Expect.isNone
                    (forcedAfter [ scout 14 10; paced 17 ] [ scout 15 10; paced 17 ])
                    "five off, it strikes within the cadence"

                let slow x y =
                    { scout x y with
                        Gait = { Weight = 2; Moves = 1 }
                    }

                // The plan walked the ranger from 11,10 to 12,10 after it.
                let walkedAfter before after =
                    LightTick.forced
                        (full
                            { at before with
                                Creeps = Map.ofList [ "ranger", ranger 11 ]
                            })
                        (at after)

                Expect.isNone
                    (walkedAfter [ scout 16 10 ] [ scout 17 10 ])
                    "a quarry at its pace five off stays as far, decided or not"

                Expect.equal
                    (walkedAfter [ slow 16 10 ] [ slow 17 10 ])
                    (Some(LightForce.Chase "ranger"))
                    "a slower quarry it walked after: it closes"

                Expect.isNone
                    (forcedAfter [ slow 16 10 ] [ slow 17 10 ])
                    "a slower quarry out of reach that the plan stood off: the decide stood it"
            }

            test
                "a chase's track: a quarry walking on as the decide assumed forces nothing, one off it forces again (#626)" {
                // The live shape the profile harness never raises: a forced
                // decide that walked the ranger one tile after a scout walking
                // off, then the scout's next steps.
                let scout x y : GlanceHostile =
                    { hostile x y "Trepidimous" false with
                        Id = "scout"
                    }

                let at time rangerX quarry =
                    { quiet with
                        Creeps =
                            Map.ofList
                                [
                                    "ranger",
                                    { creepAt rangerX 10 1000 with
                                        Gait = Some { Weight = 1; Moves = 1 }
                                    }
                                ]
                        Hostiles = quarry
                        Time = time
                    }

                let aims = [ RangedAttackCreep("ranger", "scout") ]

                let before =
                    LightTick.lastFull (at 9 12 [ scout 14 10 ]) Map.empty aims
                    |> aiming aims Map.empty

                let now = at 10 12 [ scout 15 10 ]

                Expect.equal
                    (LightTick.forcing before now)
                    [ LightForce.Chase "ranger" ]
                    "the quarry walked off: forced"

                let decided steps tracks =
                    LightTick.ofPlans
                        now
                        Set.empty
                        [
                            "",
                            LightTick.plan
                                now
                                (Set.singleton "ranger")
                                steps
                                []
                                []
                                [ MoveCreep("ranger", Right) ]
                                aims
                                Map.empty
                                Map.empty
                            |> LightTick.tracking now tracks
                        ]

                // The decide's walk with no chain past its step, and the one
                // its chase lays (`Resolver.chainOf`): on after the scout,
                // which it assumed at 16, 17, 18 on the three light ticks.
                let unchained = decided Map.empty Map.empty

                let chained =
                    decided
                        (Map.ofList [ "ranger", [ tile 13 10; tile 14 10; tile 15 10; tile 16 10 ] ])
                        (Map.ofList [ "ranger", [ tile 16 10; tile 17 10; tile 18 10 ] ])

                let inLine = at 11 13 [ scout 16 10 ]

                Expect.equal
                    (LightTick.forced unchained inLine)
                    (Some(LightForce.Chase "ranger"))
                    "unchained, the plan stands it again: the next forcing (the live loop)"

                Expect.isNone (LightTick.forced chained inLine) "on its track, nothing forces"

                Expect.contains
                    (LightTick.intents chained inLine)
                    (MoveCreep("ranger", Right))
                    "the light tick walks it on"

                Expect.isNone
                    (LightTick.forced chained (at 12 14 [ scout 17 10 ]))
                    "nor on the tick after"

                Expect.isNone
                    (LightTick.forced chained (at 12 13 [ scout 17 10 ]))
                    "a chaser a tick behind its walk (blocked, or two ticks a tile): the track is by tick"

                Expect.equal
                    (LightTick.forced chained (at 11 13 [ scout 16 11 ]))
                    (Some(LightForce.Chase "ranger"))
                    "a quarry that turns off its track: decide the chase again"

                Expect.equal
                    (LightTick.forced chained (at 14 16 [ scout 20 10 ]))
                    (Some(LightForce.Chase "ranger"))
                    "past its track, the chase is the mark's again"

                // A raider in sight of no plan, a MOVE a part, walking up the
                // chain: three ticks to the next decide, a tile each, and its
                // reach of one.
                let raider x =
                    { hostile x 10 "Trepidimous" true with
                        Id = "raider"
                    }

                Expect.equal
                    (LightTick.forced chained (at 11 13 [ scout 16 10; raider 20 ]))
                    (Some(LightForce.Chase "ranger"))
                    "a raider that reaches the walk before the next decide: decide it before its swing"

                Expect.isNone
                    (LightTick.forced chained (at 11 13 [ scout 16 10; raider 21 ]))
                    "one that reaches it after: nothing forces"
            }

            test "a chaser standing in reach of a quarry walking on stands on its light tick (#626)" {
                let scout x =
                    { hostile x 10 "Trepidimous" false with
                        Id = "scout"
                    }

                let at time quarry =
                    { quiet with
                        Creeps = Map.ofList [ "ranger", creepAt 12 10 1000 ]
                        Hostiles = [ quarry ]
                        Time = time
                    }

                let aims = [ RangedAttackCreep("ranger", "scout") ]

                // The decide stood it two off the scout, which walks on: the
                // track holds while it stays in reach.
                let record =
                    LightTick.ofPlans
                        (at 10 (scout 14))
                        Set.empty
                        [
                            "",
                            LightTick.plan
                                (at 10 (scout 14))
                                (Set.singleton "ranger")
                                Map.empty
                                []
                                []
                                []
                                aims
                                Map.empty
                                Map.empty
                            |> LightTick.tracking
                                (at 10 (scout 14))
                                (Map.ofList [ "ranger", [ tile 15 10 ] ])
                        ]

                Expect.isNone
                    (LightTick.forced record (at 11 (scout 15)))
                    "in reach on its track: no tick"

                Expect.isFalse
                    (LightTick.intents record (at 11 (scout 15))
                     |> List.exists (function
                         | MoveCreep("ranger", _) -> true
                         | _ -> false))
                    "and it does not move"

                Expect.equal
                    (LightTick.forced record (at 12 (scout 16)))
                    (Some(LightForce.Chase "ranger"))
                    "walked out of its reach past the track: decide the chase"
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
                                    home,
                                    LightTick.plan
                                        far
                                        Set.empty
                                        Map.empty
                                        []
                                        []
                                        []
                                        []
                                        Map.empty
                                        Map.empty
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
