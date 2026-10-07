/// The reflexes of last resort: safe mode, the controller's downgrade
/// deadline beside it, and the spawn hold.
module Fabot.Core.Tests.Decide.ThreatSafeModeTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ThreatFixtures

/// The same colony reading its safe-mode gates off the given controller.
let governedBy controller (snapshot: ColonyView) =
    { snapshot with
        Controller = Some controller
    }

/// A colony whose whole Keep stands at full hits.
let wholeKeep =
    bareRespawn
    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
    |> withHits "tower-1" BuiltKind.Tower 5000 5000
    |> withHits "sto-1" BuiltKind.Storage 5000 5000

/// The same Keep one hit off max on the spawn — all the second arm reads:
/// the Keep does not decay, so below max means it was damaged.
let dentedSpawn = wholeKeep |> withHits "spawn-1" BuiltKind.Spawn 4999 5000

/// The same colony with two resident rangers standing in the named room, as
/// the view carries them: held by whichever colony, so in nobody's `Creeps`
/// here.
let private garrisonedIn room (colony: ColonyView) =
    { colony with
        Defenders = Map.ofList [ room, [ resident "ranger-1"; resident "ranger-2" ] ]
    }

/// A mother raising W1N2 as her [[nursery]] (#449), its controller
/// "ctrl-child", with nothing yet built there.
let private nurseryMother =
    { ferryMother Nursery with
        NurseryControllers =
            Map.ofList
                [
                    "W1N2",
                    { controllerAt 1 with
                        Id = "ctrl-child"
                    }
                ]
    }

/// The same mother with a spawn site standing in her nursery, `paid` energy
/// already built into it.
let private withNurserySite paid (colony: ColonyView) =
    { colony with
        ConstructionSites =
            [
                {
                    Id = "site-child"
                    Left = 15_000 - paid
                    Begun = paid > 0
                }
            ]
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "site-child" (Site BuiltKind.Spawn) colony.Spatial.TargetKinds
                Rooms =
                    colony.Spatial.Rooms
                    |> Map.change
                        "W1N2"
                        (Option.map (fun layer ->
                            { layer with
                                TargetPositions =
                                    Map.add "site-child" { X = 12; Y = 46 } layer.TargetPositions
                            }))
            }
    }

/// The two rangers in the colony's own home.
let private garrisoned (colony: ColonyView) =
    garrisonedIn (SpatialInfo.homeName colony.Spatial) colony

/// The same colony with its spawn at full hits and no tower: the undefended
/// arm's room.
let private towerless = bareRespawn |> withHits "spawn-1" BuiltKind.Spawn 5000 5000

/// A 2R5M3H skirmisher.
let private skirmisher =
    hostileAt
        "h-1"
        { X = 25; Y = 27 }
        (List.replicate 2 RangedAttack @ List.replicate 5 Move @ List.replicate 3 Heal)

/// The 18M17A + 11M7H + 6M4H squad.
let private squad =
    [
        hostileAt "h-1" { X = 25; Y = 27 } (List.replicate 18 Move @ List.replicate 17 Attack)
        hostileAt "h-2" { X = 25; Y = 28 } (List.replicate 11 Move @ List.replicate 7 Heal)
        hostileAt "h-3" { X = 26; Y = 28 } (List.replicate 6 Move @ List.replicate 4 Heal)
    ]

/// #545's home: its tower at (25,20) holding `energy`, the spawn at (25,25)
/// under a 100k rampart of ours, six bare extensions in a row at (33..38,
/// 30), and a perimeter rampart at (10,40). The Keep stands whole.
let private economyHome (energy: int) =
    let colony =
        wholeKeep
        |> withHits "ram-s" BuiltKind.Rampart 100_000 3_000_000
        |> withHits "ram-p" BuiltKind.Rampart 100_000 3_000_000

    { colony with
        Refillables =
            {
                Id = "tower-1"
                FreeCapacity = Engine.towerCapacity - energy
                Kind = BuiltKind.Tower
            }
            :: colony.Refillables
        Spatial =
            colony.Spatial
            |> withTargets (
                [
                    "tower-1", { X = 25; Y = 20 }, Structure BuiltKind.Tower
                    "spawn-1", { X = 25; Y = 25 }, Structure BuiltKind.Spawn
                    "ram-s", { X = 25; Y = 25 }, Structure BuiltKind.Rampart
                    "ram-p", { X = 10; Y = 40 }, Structure BuiltKind.Rampart
                ]
                @ [
                    for x in 33..38 -> $"ext-{x}", { X = x; Y = 30 }, Structure BuiltKind.Extension
                ]
            )
    }

/// The same home with empty bodies of ours of `carry` CARRY and half as
/// much MOVE on these tiles, `refiller-1` on.
let private withCarriersAt (carry: int) (tiles: Pos list) (colony: ColonyView) =
    let named = tiles |> List.mapi (fun i pos -> $"refiller-{i + 1}", pos)

    { colony with
        Creeps =
            (named
             |> List.map (fun (name, _) ->
                 creepWith
                     name
                     0
                     (50 * carry)
                     (List.replicate carry Carry @ List.replicate (carry / 2) Move)))
            @ colony.Creeps
        Spatial = colony.Spatial |> withCreepsAt named
    }

/// 16C8M refillers (2,400 hits each): two of them are the refilling floor.
let private withRefillersAt = withCarriersAt 16

/// The tick decided with every `refiller-…` body of the colony holding the
/// spawn's Refill: the bodies that refill this home.
let private decideRefilling (colony: ColonyView) =
    let held =
        colony.Creeps
        |> List.filter (fun c -> c.Name.StartsWith "refiller-")
        |> List.map (fun c -> c.Name, taskId (Refill("spawn-1", Energy)))
        |> Map.ofList

    let { Intents = intents } = decideFrom held colony
    activations intents

/// The spawn's rampart worn to 1,000: the spawn falls to two shooters inside
/// the horizon.
let private wornSpawn (colony: ColonyView) =
    colony |> withHits "ram-s" BuiltKind.Rampart 1_000 3_000_000

/// MeowKittyWow's largest WhatIsAShooter: 1T + (M R M H) x 11, 4,600 hits,
/// 110 a tick at range 3 and 132 of its own heal.
let private meowShooterNamed id (pos: Pos) =
    hostileAt id pos (Tough :: List.concat (List.replicate 11 [ Move; RangedAttack; Move; Heal ]))

/// The same, alone in the room.
let private meowShooter (pos: Pos) = meowShooterNamed "h-1" pos

/// Three of them abreast at (34..36, 33), every extension in their reach.
let private meowPack =
    [ for x in 34..36 -> meowShooterNamed $"h-{x}" { X = x; Y = 33 } ]

/// Two shooters at the spawn's rampart.
let private spawnPair =
    [
        meowShooterNamed "h-s1" { X = 25; Y = 28 }
        meowShooterNamed "h-s2" { X = 26; Y = 28 }
    ]

[<Tests>]
let safeModeTests =
    testList
        "safe-mode reflex"
        [
            test "an unplaced controller falls back to firing on sight" {
                // No controller tile in the projection means no deadline to
                // measure — the reflex keeps the old conservative answer.
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostile [ BodyPart.Claim; BodyPart.Claim; Move; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.equal (activations intents) [ "ctrl-1" ] "safe mode fires immediately"
            }

            test "with no tower standing, the first armed hostile fires safe mode" {
                // #217. Pairwise on the tower and on the arms: the same
                // armed hostile fires the reflex in a tower-less room with
                // a whole Keep, does not in the same room with a tower
                // standing (the tower's job; the Keep arm still waits for
                // a dent), and an unarmed scout fires nothing in either.
                let armed = hostile [ Tough; Move; Move; RangedAttack; Attack; Move ]
                let scout = hostile [ Move; Move ]

                let towerless hostiles =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
                    |> fun colony -> { colony with Hostiles = hostiles }

                let towered hostiles =
                    { wholeKeep with
                        Spatial =
                            wholeKeep.Spatial
                            |> withTargets
                                [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
                        Hostiles = hostiles
                    }

                let fires (colony: ColonyView) =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires (towerless [ armed ]))
                    [ "ctrl-1" ]
                    "no tower: the invader is met with safe mode at once"

                Expect.isEmpty
                    (fires (towered [ armed ]))
                    "a tower stands: the Keep arm waits for a dent, as before"

                Expect.isEmpty
                    (fires (towerless [ scout ]))
                    "a scout is not armed and spends nothing"

                Expect.isEmpty (fires (towerless [])) "and a quiet room fires nothing"
            }

            test "a claimer beyond reach holds the stock — the towers get their window" {
                // attackController is range 1 and judged from tick-start position, so
                // a claimer 4 tiles out cannot tap for at least 3 more ticks; holding
                // costs nothing.
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (activations intents) "range 4: the tap cannot land yet"
            }

            test "a claimer at the reach deadline fires safe mode" {
                // Range 3 = the precise deadline (2) plus one tile of margin for a
                // skipped tick.
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.equal (activations intents) [ "ctrl-1" ] "the deadline is now"
            }

            test "a hostile without CLAIM parts does not spend the activation while a tower stands" {
                // The tower is the answer to a fighter; the stock is for the controller
                // and the Keep. Without a tower the undefended arm fires instead (the
                // test below).
                let snapshot =
                    { bareRespawn with
                        Spatial =
                            bareRespawn.Spatial
                            |> withTargets
                                [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
                        Hostiles = [ hostile [ Tough; Attack; RangedAttack; Heal; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty
                    (activations intents)
                    "fighters cannot touch the controller; the stock is kept"
            }

            test "an empty stock fires nothing" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeAvailable = 0
                                }
                        Hostiles = [ hostile [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (activations intents) "nothing to activate with"
            }

            test "safe mode already running is not re-fired" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeActive = true
                                }
                        Hostiles = [ hostile [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (activations intents) "the room is already protected"
            }

            test "a controller's safe-mode cooldown silences the reflex until it ends" {
                // #474: the engine refuses `activateSafeMode` while
                // `safeModeCooldown` runs, so asking each tick of it is a
                // refusal logged and a tick forced full. Home and nursery alike.
                let cooling (until: int) (colony: ColonyView) =
                    { colony with
                        Controller =
                            colony.Controller
                            |> Option.map (fun c -> { c with SafeModeCooldownUntil = until })
                        NurseryControllers =
                            colony.NurseryControllers
                            |> Map.map (fun _ c -> { c with SafeModeCooldownUntil = until })
                    }

                let fires (colony: ColonyView) =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let home =
                    { bareRespawn with
                        Hostiles = [ hostile [ BodyPart.Claim; Move ] ]
                    }

                Expect.isEmpty
                    (fires (cooling (home.Time + 1) home))
                    "home: one tick of cooldown left"

                Expect.equal (fires (cooling home.Time home)) [ "ctrl-1" ] "home: the tick it ends"

                let nursery =
                    { nurseryMother with
                        Hostiles =
                            [
                                { hostileAt "h-1" { X = 10; Y = 44 } [ BodyPart.Claim; Move ] with
                                    Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                                }
                            ]
                    }

                Expect.isEmpty (fires (cooling (nursery.Time + 1) nursery)) "nursery: cooling"

                Expect.equal
                    (fires (cooling nursery.Time nursery))
                    [ "ctrl-child" ]
                    "nursery: ended"
            }

            test "a dented Keep with a hostile in the room fires" {
                // The second arm stands on its own: this hostile carries no CLAIM, so
                // only the damage speaks. Each in turn; the tower is the
                // dismantler test below. The tower stands, or the undefended
                // arm would fire on the armed hostile whatever the hits read.
                let fires snapshot =
                    let { Intents = intents } =
                        decideOn (towered snapshot |> facing [ hostile [ Tough; Attack; Move ] ])

                    activations intents

                Expect.equal
                    (fires dentedSpawn)
                    [ "ctrl-1" ]
                    "the spawn is losing hits with someone here"

                Expect.equal
                    (fires (wholeKeep |> withHits "sto-1" BuiltKind.Storage 4999 5000))
                    [ "ctrl-1" ]
                    "the Storage is the room's largest store, and it is of the Keep"

                Expect.equal
                    (fires (wholeKeep |> withHits "term-1" BuiltKind.Terminal 2999 3000))
                    [ "ctrl-1" ]
                    "the terminal holds the season's ore, and it is of the Keep (#422)"
            }

            test "a dented Keep in an empty room holds the stock" {
                // Damage alone is not a raid: the window between a raid
                // leaving and a worker patching the Keep spends nothing.
                let { Intents = intents } = decideOn dentedSpawn
                Expect.isEmpty (activations intents) "nobody is here to be held off"
            }

            test "a WORK-only hostile in a room with a dented tower fires" {
                // A dismantler hurts a structure without ever qualifying as a
                // Threat — the arm reads any hostile for exactly this case.
                let snapshot =
                    wholeKeep
                    |> withHits "tower-1" BuiltKind.Tower 4999 5000
                    |> facing [ hostile [ Work; Work; Move ] ]

                let { Intents = intents } = decideOn snapshot
                Expect.equal (activations intents) [ "ctrl-1" ] "a dismantler is doing the harm"
            }

            test "a hungry Post container and a hungry rampart are not the Keep" {
                // Invaders chew containers as a matter of routine, and the stock is not
                // for that. A rampart sits below its floor for most of its life, so a
                // Keep arm that read every hungry structure would spend the stock on the
                // first hostile to wander past.
                let snapshot =
                    wholeKeep
                    |> withHits "cont-1" BuiltKind.Container 1 125_000
                    |> withHits "ram-1" BuiltKind.Rampart 50_000 3_000_000
                    |> facing [ hostile [ Work; Move ] ]

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty
                    (activations intents)
                    "hits off the Keep's list never spend the stock"
            }

            test "the gates hold over a dented Keep under attack" {
                // The second arm is gated exactly as the first is: there is
                // one pair, not a pair each.
                let dismantled = dentedSpawn |> facing [ hostile [ Work; Move ] ]

                let empty =
                    dismantled
                    |> governedBy
                        { controllerAt 1 with
                            SafeModeAvailable = 0
                        }

                let running =
                    dismantled
                    |> governedBy
                        { controllerAt 1 with
                            SafeModeActive = true
                        }

                let { Intents = onEmpty } = decideOn empty
                let { Intents = onRunning } = decideOn running

                Expect.isEmpty (activations onEmpty) "an empty stock has nothing to spend"
                Expect.isEmpty (activations onRunning) "already protected, whichever arm asks"
            }

            test "a quiet room fires nothing" {
                let { Intents = intents } = decideOn bareRespawn
                Expect.isEmpty (activations intents) "no hostiles, no reflex"
            }

            test "a claimer within reach fires even with a winning garrison" {
                // #448: the claimer arm is unconditional — a tap blocks
                // activation for 1,000 ticks, and no exchange is gambled on.
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }
                    |> garrisoned

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (activations intents)
                    [ "ctrl-1" ]
                    "the tap is never left to the rangers"
            }

            test "a dented Keep fires whatever stands at home" {
                // #448: a garrison buys time only while the Keep is untouched.
                // The skirmisher loses to either ranger, and the spawn losing
                // hits under it still means fire.
                let { Intents = intents } =
                    decideOn (towered dentedSpawn |> garrisoned |> facing [ skirmisher ])

                Expect.equal (activations intents) [ "ctrl-1" ] "the Keep is already paying"
            }

            test "a dismantler denting the Keep fires beside an armed body of ours" {
                // #448, the Keep arm's own case: a WORK-only body deals no damage
                // an exchange can price, so a garrison would always "win" it.
                let { Intents = intents } =
                    decideOn (dentedSpawn |> garrisoned |> facing [ hostile [ Work; Work; Move ] ])

                Expect.equal (activations intents) [ "ctrl-1" ] "the dismantler is doing the harm"
            }

            test "with no tower, one armed body of ours that wins the raid alone holds the stock" {
                // #448: the skirmisher's 20 a tick is out-healed by either
                // ranger; the squad's 510 melee beats each of them alone,
                // and they are never summed (the raid focuses one body).
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.isEmpty
                    (fires (towerless |> garrisoned |> facing [ skirmisher ]))
                    "the garrison answers a poke"

                Expect.equal
                    (fires (towerless |> garrisoned |> facing squad))
                    [ "ctrl-1" ]
                    "the squad wins against each ranger"

                Expect.equal
                    (fires (towerless |> facing [ skirmisher ]))
                    [ "ctrl-1" ]
                    "and with nobody at home the undefended arm fires as before"
            }

            test
                "with no tower and nobody at home, a dismantler fires the undefended arm, a neutral's only caught at it" {
                // #429: a WORK body hurts no creep, but it razes the home a
                // towerless room has nothing to keep it off. A neutral's
                // counts only beside a rampart of ours that lost hits since
                // the last tick, never its decay: a pioneer crossing the
                // home spends nothing.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let worker owner =
                    { hostile [ Work; Work; Move ] with
                        Owner = owner
                    }

                let beside lost =
                    { towerless with
                        Spatial =
                            towerless.Spatial
                            |> withTargets
                                [ "ram-1", { X = 25; Y = 26 }, Structure BuiltKind.Rampart ]
                        KeepHitsBefore = Map.ofList [ "ram-1", 100_000 ]
                    }
                    |> withHits "ram-1" BuiltKind.Rampart (100_000 - lost) 3_000_000

                Expect.equal
                    (fires (towerless |> facing [ worker "Trepidimous" ]))
                    [ "ctrl-1" ]
                    "an enemy's dismantler: the spawn is its to raze"

                Expect.isEmpty
                    (fires (towerless |> facing [ worker "raider" ]))
                    "a neutral's crossing: nothing"

                Expect.equal
                    (fires (beside 100 |> facing [ worker "raider" ]))
                    [ "ctrl-1" ]
                    "a neutral's beside a rampart of ours it took 100 off: dismantling"

                Expect.isEmpty
                    (fires (beside Engine.rampartDecayAmount |> facing [ worker "raider" ]))
                    "a rampart's decay is no dismantle"

                Expect.isEmpty
                    (fires (beside (3 * Engine.rampartDecayAmount) |> facing [ worker "raider" ]))
                    "nor three decays on a baseline a light colony left stale"

                Expect.isEmpty (fires (beside 100 |> facing [ worker "Odiodin" ])) "an ally's never"

                // An armed body within three of the rampart takes the blame:
                // the neutral beside it is not caught. Asked of the fact
                // itself, since the shooter would fire the arm on its own.
                let shooter =
                    { hostileAt "h-2" { X = 25; Y = 29 } [ RangedAttack; Move ] with
                        Owner = "raider"
                    }

                let caught colony =
                    Fabot.Core.Decide.Facts.dismantling
                        colony
                        (colony.Hostiles |> List.find (fun h -> h.Id = "h-1"))

                Expect.isTrue
                    (caught (beside 100 |> facing [ worker "raider" ]))
                    "the premise: caught alone"

                Expect.isFalse
                    (caught (beside 100 |> facing [ worker "raider"; shooter ]))
                    "a shooter three off the rampart struck it"
            }

            test
                "with no tower, a weak body of ours does not hold a dismantler its healers keep alive" {
                // #429 review: a WORK raid priced as a creep fight dealt
                // nothing, so any body of ours "won" it and the undefended
                // arm stayed silent while the home was razed.
                let weak =
                    { towerless with
                        Defenders =
                            Map.ofList
                                [
                                    SpatialInfo.homeName towerless.Spatial,
                                    [ creepWith "guard-1" 0 0 [ RangedAttack; Move ] ]
                                ]
                    }

                let raid =
                    [
                        { hostileAt
                              "h-1"
                              { X = 25; Y = 27 }
                              (List.replicate 10 Work @ List.replicate 10 Move) with
                            Owner = "Trepidimous"
                        }
                        { hostileAt
                              "med-1"
                              { X = 25; Y = 28 }
                              (List.replicate 7 Heal @ List.replicate 7 Move) with
                            Owner = "Trepidimous"
                        }
                    ]

                let { Intents = intents } = decideOn (weak |> facing raid)

                Expect.equal
                    (activations intents)
                    [ "ctrl-1" ]
                    "10 a tick against 84 healed: the home is razed"
            }

            test "a garrison standing in another room is no defence of this one" {
                // #448: the exchange is the home room's.
                let { Intents = intents } =
                    decideOn (towerless |> garrisonedIn "W9N9" |> facing [ skirmisher ])

                Expect.equal (activations intents) [ "ctrl-1" ] "a ranger elsewhere is not at home"
            }

            test "a mother fires her nursery's safe mode under the same arms" {
                // #449: a Nursery runs no tick of its own, so its mother asks
                // the one reflex over its room and controller.
                let mother = nurseryMother

                let inNursery body =
                    { hostileAt "h-1" { X = 10; Y = 44 } body with
                        Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                    }

                let fires hostiles (colony: ColonyView) =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }
                    activations intents

                let building = mother |> withNurserySite 1

                Expect.equal
                    (fires [ inNursery [ Attack; Move ] ] building)
                    [ "ctrl-child" ]
                    "no tower stands in a nursery: the first armed hostile fires it"

                Expect.equal
                    (fires [ inNursery [ BodyPart.Claim; Move ] ] mother)
                    [ "ctrl-child" ]
                    "and a claimer within reach of its controller"

                Expect.isEmpty
                    (fires [ inNursery [ Attack; Move ] ] (building |> garrisonedIn "W1N2"))
                    "a resident of ours that wins the raid alone holds the stock"

                Expect.isEmpty
                    (fires
                        [ inNursery [ Attack; Move ] ]
                        { building with
                            NurseryControllers = Map.empty
                        })
                    "and a room that is no nursery of hers is not her reflex's"
            }

            test "a nursery's undefended arm fires only over a begun site" {
                // A squad crossing an empty claimed room must not spend the
                // shard's one activation: the undefended arm guards paid-in
                // work, and the claimer arm guards the room itself.
                let squadThrough =
                    [
                        { hostileAt "h-1" { X = 10; Y = 44 } [ Attack; Attack; Move ] with
                            Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                        }
                    ]

                let claimer =
                    [
                        { hostileAt "h-2" { X = 10; Y = 44 } [ BodyPart.Claim; Move ] with
                            Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                        }
                    ]

                let fires hostiles colony =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }
                    activations intents

                Expect.isEmpty
                    (fires squadThrough nurseryMother)
                    "nothing stands in the nursery to protect"

                Expect.isEmpty
                    (fires squadThrough (nurseryMother |> withNurserySite 0))
                    "a site nothing has been paid into is not yet worth the stock"

                Expect.equal
                    (fires squadThrough (nurseryMother |> withNurserySite 1))
                    [ "ctrl-child" ]
                    "a begun site is"

                Expect.equal
                    (fires claimer nurseryMother)
                    [ "ctrl-child" ]
                    "a claimer within reach fires with nothing built"
            }

            test "another room's running safe mode silences every arm" {
                // One activation per shard: the engine answers a second with
                // ERR_BUSY, so asking again each tick only logs a failure and
                // forces the tick full.
                let claimerHome =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }

                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires claimerHome)
                    [ "ctrl-1" ]
                    "the premise: the claimer arm fires on its own"

                Expect.isEmpty
                    (fires
                        { claimerHome with
                            SafeModeRunning = true
                        })
                    "the claimer arm included"

                Expect.isEmpty
                    (fires
                        { (towerless |> facing [ skirmisher ]) with
                            SafeModeRunning = true
                        })
                    "and the undefended arm"
            }

            test "a mother with a firing home and a firing nursery activates one" {
                // The engine takes one per shard; the home's goes first.
                let raided =
                    { (nurseryMother |> withNurserySite 1) with
                        Hostiles =
                            [
                                { hostileAt "h-1" { X = 10; Y = 44 } [ Attack; Attack; Move ] with
                                    Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                                }
                                { hostileAt "h-2" { X = 10; Y = 4 } [ Attack; Attack; Move ] with
                                    Pos = RoomPos.at "W1N1" { X = 10; Y = 4 }
                                }
                            ]
                    }

                let { Intents = intents } = decideOn raided
                Expect.equal (activations intents) [ "ctrl-1" ] "the home's, and only it"
            }

            test
                "half the extensions razed under a rampart-covered Keep fires; one extension never does" {
                // #545: the Keep stands whole behind its rampart, so the Keep
                // arm is silent, and the extensions are not of the Keep. Three
                // shooters heal the one the tower shoots for 396 a tick, as much
                // as it deals from range 11, and take an extension every three
                // ticks: half of the six fall with all three standing.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires (economyHome 1000 |> facing meowPack))
                    [ "ctrl-1" ]
                    "half the bank is being razed faster than the tower kills"

                Expect.isEmpty
                    (fires (economyHome 0 |> facing [ meowShooter { X = 41; Y = 30 } ]))
                    "a shooter that reaches one extension, even past a dry tower, spends nothing"

                Expect.isEmpty
                    (fires (
                        economyHome 1000
                        |> facing
                            [
                                { skirmisher with
                                    Pos = RoomPos.at "" { X = 35; Y = 33 }
                                }
                            ]
                    ))
                    "a striker the tower kills before half the extensions fall spends nothing"
            }

            test "a lone poke at a rampart spends nothing while the tower holds a shot" {
                // #545 keeps the Keep's rejected proximity trigger rejected:
                // the rampart absorbs the fire, and the loaded tower keeps it.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.isEmpty
                    (fires (economyHome 1000 |> facing [ meowShooter { X = 10; Y = 43 } ]))
                    "at the perimeter rampart"

                Expect.isEmpty
                    (fires (economyHome 1000 |> facing [ meowShooter { X = 25; Y = 28 } ]))
                    "at the spawn's rampart"
            }

            test "a dry tower with a shooter inside reaching the spawn or the refillers fires" {
                // #545: Meow enters the tick the tower hits 0. With no shot
                // left and nobody carrying a refill, two shooters take the
                // worn rampart and the spawn (6,000 hits at 220 a tick) in 28
                // ticks; a tower holding less than one shot is dry.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires (economyHome 0 |> wornSpawn |> facing spawnPair))
                    [ "ctrl-1" ]
                    "the spawn, with the tower empty"

                Expect.equal
                    (fires (economyHome 5 |> wornSpawn |> facing spawnPair))
                    [ "ctrl-1" ]
                    "and with the tower short of one shot"

                Expect.isEmpty
                    (fires (economyHome 1000 |> wornSpawn |> facing spawnPair))
                    "the loaded tower kills both before the spawn falls"

                Expect.isEmpty
                    (fires (economyHome 0 |> facing spawnPair))
                    "a spawn behind 100k of rampart does not fall inside the horizon"

                let shooter = meowShooter { X = 20; Y = 33 }
                let twoRefillers = withRefillersAt [ { X = 20; Y = 30 }; { X = 21; Y = 30 } ]

                Expect.equal
                    (decideRefilling (economyHome 0 |> twoRefillers |> facing [ shooter ]))
                    [ "ctrl-1" ]
                    "both refillers under fire with the tower empty, 44 ticks"

                Expect.isEmpty
                    (decideRefilling (
                        economyHome 0
                        |> withRefillersAt [ { X = 20; Y = 30 }; { X = 40; Y = 10 } ]
                        |> facing [ shooter ]
                    ))
                    "one refiller of two is never enough"

                Expect.isEmpty
                    (decideRefilling (economyHome 1000 |> twoRefillers |> facing [ shooter ]))
                    "the same shooter with the tower loaded is the tower's"
            }

            test
                "Kalgen T3-boosted at the worn spawn fires where the loaded tower kills him unboosted" {
                // Unboosted: 220 a tick takes the worn rampart
                // and the spawn in 28 ticks, the tower's 510 less his 144
                // heal kills him in 14. T3-boosted: 880 a tick takes them in
                // 7, and the tower's 510 lands 153 through his TOUGH under
                // 576 of heal: he never dies.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let kalgen = hostileAt "k-1" { X = 25; Y = 28 } kalgenBody

                Expect.isEmpty
                    (fires (economyHome 1000 |> wornSpawn |> facing [ kalgen ]))
                    "the premise: unboosted, the tower is enough"

                Expect.equal
                    (fires (economyHome 1000 |> wornSpawn |> facing [ boostedAt Tier3 kalgen ]))
                    [ "ctrl-1" ]
                    "T3-boosted, safe mode"
            }

            test "the cripple, the lone melee and the poor pair past a low tower spend nothing" {
                // #545 re-review C2-C5 on W17S25's t974,040 shape: a 1-RANGED
                // cripple at the spawn takes it at t+571, past the horizon; a
                // lone melee or a melee and healer beside a worn spawn with the
                // tower at 50 or 60 die to the shots a hauler's 800 refills
                // before the spawn falls.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let cripple = hostileAt "c-1" { X = 27; Y = 27 } [ Move; RangedAttack ]

                let melee =
                    hostileAt
                        "m-1"
                        { X = 25; Y = 26 }
                        (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ])

                let healer =
                    hostileAt
                        "m-2"
                        { X = 26; Y = 27 }
                        (List.replicate 11 Move @ List.replicate 7 Heal)

                let haulerAt (energy: int) (colony: ColonyView) =
                    { colony with
                        Creeps =
                            creepWith
                                "hauler-1"
                                energy
                                (800 - energy)
                                (List.replicate 16 Carry @ List.replicate 8 Move)
                            :: colony.Creeps
                        Spatial = colony.Spatial |> withCreepsAt [ "hauler-1", { X = 30; Y = 22 } ]
                    }

                Expect.isEmpty
                    (fires (economyHome 0 |> wornSpawn |> facing [ cripple ]))
                    "C2: the cripple, tower at 0"

                Expect.isEmpty
                    (fires (economyHome 5 |> wornSpawn |> facing [ cripple ]))
                    "C3: the cripple, tower at 5"

                // The hauler holds the tower's Refill, eight ticks' walk off.
                let refilling colony =
                    let { Intents = intents } =
                        decideFrom
                            (Map.ofList [ "hauler-1", taskId (Refill("tower-1", Energy)) ])
                            colony

                    activations intents

                Expect.isEmpty
                    (refilling (economyHome 50 |> wornSpawn |> haulerAt 800 |> facing [ melee ]))
                    "C4: a lone melee, tower at 50, its refiller holding 800"

                Expect.isEmpty
                    (refilling (
                        economyHome 60 |> wornSpawn |> haulerAt 800 |> facing [ melee; healer ]
                    ))
                    "C5: the pair, tower at 60, its refiller holding 800"

                Expect.equal
                    (fires (economyHome 0 |> wornSpawn |> haulerAt 800 |> facing [ melee; healer ]))
                    [ "ctrl-1" ]
                    "C with a hauler refilling nothing here: its 800 buys the dry tower no shot"

                Expect.equal
                    (fires (economyHome 0 |> wornSpawn |> facing [ melee; healer ]))
                    [ "ctrl-1" ]
                    "C: the pair, the tower at 0 and the carriers dead"
            }

            test "two cheap bodies, or haulers in transit, are never the refillers" {
                // #545 re-review D4: two 10C5M haulers beside a melee and a
                // healer at the west gate, the tower full. 20 CARRY is under
                // the floor even holding the spawn's Refill, and bodies holding
                // none are in transit, not refilling.
                let raid =
                    [
                        hostileAt
                            "m-1"
                            { X = 20; Y = 33 }
                            (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ])
                        hostileAt
                            "m-2"
                            { X = 19; Y = 34 }
                            (List.replicate 11 Move @ List.replicate 7 Heal)
                    ]

                let cheap =
                    economyHome 1000
                    |> withCarriersAt 10 [ { X = 20; Y = 32 }; { X = 21; Y = 32 } ]
                    |> facing raid

                let big =
                    economyHome 0
                    |> withRefillersAt [ { X = 20; Y = 32 }; { X = 21; Y = 32 } ]
                    |> facing raid

                Expect.isEmpty (decideRefilling cheap) "two 10-CARRY refillers are under the floor"

                let { Intents = transit } = decideOn big

                Expect.isEmpty
                    (activations transit)
                    "16-CARRY bodies holding no Refill are in transit"

                Expect.equal
                    (decideRefilling big)
                    [ "ctrl-1" ]
                    "the same bodies refilling, the tower dry, are the economy"
            }

            test "a dry tower refills from its refiller once it has walked there" {
                // #545: a tower between loads reads 0 while the hauler holding
                // its Refill walks the energy in. Beside the tower, its 1,000
                // kills both shooters before the worn spawn falls; 15 tiles
                // off at a loaded gait of two, it lands too late; holding no
                // Refill of this home, it buys nothing.
                let dry = economyHome 0 |> wornSpawn |> facing spawnPair

                let carrying (at: Pos) =
                    { dry with
                        Creeps =
                            creepWith
                                "hauler-1"
                                1000
                                0
                                (List.replicate 20 Carry @ List.replicate 10 Move)
                            :: dry.Creeps
                        Spatial = dry.Spatial |> withCreepsAt [ "hauler-1", at ]
                    }

                let refill = Map.ofList [ "hauler-1", taskId (Refill("tower-1", Energy)) ]

                let fired assigned colony =
                    activations (decideFrom assigned colony).Intents

                Expect.isEmpty (fired refill (carrying { X = 26; Y = 21 })) "its refiller beside it"

                Expect.equal
                    (fired refill (carrying { X = 40; Y = 20 }))
                    [ "ctrl-1" ]
                    "its refiller 15 tiles off arrives after the spawn is lost"

                Expect.equal
                    (fired Map.empty (carrying { X = 26; Y = 21 }))
                    [ "ctrl-1" ]
                    "a body refilling nothing here buys no shot"

                Expect.equal
                    (fired Map.empty dry)
                    [ "ctrl-1" ]
                    "and nobody at all: the tower is dry"
            }

            test "the refillers being killed in the home while the tower cannot stop it fire" {
                // #545: three shooters heal the focus as fast as the loaded
                // tower hurts it, and both refillers fall in 15 ticks.
                Expect.equal
                    (decideRefilling (
                        economyHome 1000
                        |> withRefillersAt [ { X = 35; Y = 35 }; { X = 36; Y = 35 } ]
                        |> facing meowPack
                    ))
                    [ "ctrl-1" ]
                    "the hands that refill are dying"
            }

            test "a refiller on the exit ring is its walk's to save, not the stock's" {
                // #545: W17S25 t963,615 and t965,317, a lone melee killing a
                // hauler on the west exit, held without safe mode.
                Expect.isEmpty
                    (decideRefilling (
                        economyHome 0
                        |> withRefillersAt [ { X = 20; Y = 0 }; { X = 21; Y = 0 } ]
                        |> facing [ meowShooter { X = 20; Y = 2 } ]
                    ))
                    "the ring's bodies are not counted"
            }

            test "an Invader razing the economy never spends the stock" {
                // #545: Invaders recur; 20k ticks of safe mode for what one
                // raid takes is a bad trade. The same pack, the NPC's.
                let { Intents = intents } =
                    decideOn (
                        economyHome 1000
                        |> facing (meowPack |> List.map (fun h -> { h with Owner = "Invader" }))
                    )

                Expect.isEmpty (activations intents) "the NPC's raid is the towers'"
            }

            test "Meow's entry on a drained tower, shooters on the spawn and the extensions, fires" {
                // #545's real need, W24S23 at t909,500: the tower bled to three
                // shots over the edge dance, no Refill held, and four shooters
                // walk in on the spawn and the extensions.
                let entry = meowShooterNamed "h-s" { X = 25; Y = 28 } :: meowPack

                let { Intents = intents } = decideOn (economyHome 30 |> facing entry)
                Expect.equal (activations intents) [ "ctrl-1" ] "nothing stops the raze in time"
            }

            test "every gate holds over the razing arm" {
                // #545 adds an arm, never a way past a gate: the stock, safe
                // mode running here or in another room, the cooldown, the home
                // room alone (#201), and a towerless home is the undefended
                // arm's (#448: a winning garrison holds it there).
                let razed = economyHome 0 |> wornSpawn |> facing spawnPair

                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let governed change =
                    razed |> governedBy (change (controllerAt 1))

                Expect.equal (fires razed) [ "ctrl-1" ] "the premise: the arm fires on its own"

                Expect.isEmpty
                    (fires (governed (fun c -> { c with SafeModeAvailable = 0 })))
                    "an empty stock"

                Expect.isEmpty
                    (fires (governed (fun c -> { c with SafeModeActive = true })))
                    "already running here"

                Expect.isEmpty
                    (fires (
                        governed (fun c ->
                            { c with
                                SafeModeCooldownUntil = razed.Time + 1
                            })
                    ))
                    "the controller's cooldown"

                Expect.isEmpty
                    (fires { razed with SafeModeRunning = true })
                    "running in another room of ours"

                Expect.isEmpty
                    (fires
                        { razed with
                            Hostiles =
                                [
                                    { meowShooter { X = 25; Y = 28 } with
                                        Pos = RoomPos.at "W1N2" { X = 25; Y = 28 }
                                    }
                                ]
                        })
                    "the same shooter in another room"

                Expect.isEmpty
                    (fires (
                        { razed with
                            Spatial =
                                { razed.Spatial with
                                    TargetKinds = Map.remove "tower-1" razed.Spatial.TargetKinds
                                }
                        }
                        |> garrisoned
                        |> facing
                            [
                                { skirmisher with
                                    Pos = RoomPos.at "" { X = 25; Y = 28 }
                                }
                            ]
                    ))
                    "no tower, and a garrison that wins the skirmisher: the undefended arm's call"
            }

            test "across colonies the first activation is the one issued" {
                let intents =
                    [ ActivateSafeMode "ctrl-a"; SayCreep("c-1", "hi"); ActivateSafeMode "ctrl-b" ]

                Expect.equal
                    (firstActivationOnly intents)
                    [ ActivateSafeMode "ctrl-a"; SayCreep("c-1", "hi") ]
                    "the second would be refused with ERR_BUSY"
            }
        ]

[<Tests>]
let downgradeDeadlineTests =
    testList
        "downgrade deadline"
        [
            test "a controller near downgrade outranks refill for a loaded creep" {
                // A downgrade zeroes the safe-mode stock, so the timer is a
                // hard deadline, not surplus-rank work.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = 4000
                                }
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the deadline escalates Upgrade above the feeding tier"
            }

            test "the deadline scales with level: RCL4 at 15,000 is already urgent" {
                // The engine refuses activateSafeMode below half the level's
                // full timer minus 5,000 — at RCL4 that is 15,000. Escalating
                // at half (20,000) keeps the reflex's activation legal with
                // the whole 5,000-tick grace intact.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 15000
                                }
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a flat deadline would sleep through RCL4's refusal threshold"
            }

            test "RCL4 above half its timer is not urgent" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 25000
                                }
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "above half the timer, upgrade stays surplus work"
            }

            test "far from the deadline upgrade stays surplus work" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "a fresh timer changes nothing"
            }

            // A busy colony on the tier corridor (RCL1, a 20,000 timer): a
            // home site and a worn road beside its own controller and buffer.
            let busyAt ticks =
                let colony =
                    { bareRespawn with
                        ConstructionSites =
                            [
                                {
                                    Id = "site-1"
                                    Left = 5_000
                                    Begun = false
                                }
                            ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = ticks
                                }
                        Spatial =
                            tierRoom
                            |> withTargets
                                [
                                    "site-1", { X = 16; Y = 11 }, Site BuiltKind.Extension
                                    "road-1", { X = 12; Y = 11 }, Structure BuiltKind.Road
                                ]
                    }
                    |> withHits "road-1" BuiltKind.Road 500 5_000

                let pool = poolBusyOn [ SitesOwed ] colony

                fun task ->
                    pool
                    |> List.tryPick (fun entry ->
                        if entry.Task = task then Some entry.Priority else None)
                    |> Option.defaultWith (fun () -> failtest $"{task} not pooled")

            test "inside the deadline the colony's own Upgrade ranks above its Build and Repair" {
                let inside = busyAt 9_800
                let outside = busyAt 10_200

                Expect.isLessThan
                    (outside (Build "site-1"))
                    (outside (Upgrade "ctrl-1"))
                    "51% of the timer: the site first, as today"

                Expect.isLessThan
                    (outside (Repair "road-1"))
                    (outside (Upgrade "ctrl-1"))
                    "and the road"

                Expect.equal (inside (Upgrade "ctrl-1")) deadlineRank "49%: the deadline rank"

                Expect.isLessThan
                    (inside (Upgrade "ctrl-1"))
                    (inside (Build "site-1"))
                    "over the site"

                Expect.isLessThan
                    (inside (Upgrade "ctrl-1"))
                    (inside (Repair "road-1"))
                    "and the road"
            }

            // A home inside its deadline (RCL1 at 9,800), a hungry tower, a
            // rampart, and a loaded worker on the corridor; raided, a melee
            // raider stands beside the rampart at the corridor's west end
            // (attackController can sink the timer mid-siege).
            let defended raided =
                let colony =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" 0 BuiltKind.Spawn
                                refillable "tower-1" 500 BuiltKind.Tower
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = 9_800
                                }
                        Spatial =
                            tierRoom
                            |> withTargets
                                [ "ram-1", { X = 10; Y = 11 }, Structure BuiltKind.Rampart ]
                            |> withCreepsAt [ "w1", { X = 16; Y = 11 } ]
                    }
                    |> withHits "ram-1" BuiltKind.Rampart 5_000 1_000_000

                if raided then
                    facing [ hostileAt "raider" { X = 9; Y = 11 } smallMelee ] colony
                else
                    colony

            test
                "a raided home inside the deadline ranks its tower and struck rampart over the Upgrade" {
                let rankIn busy colony =
                    let pool = poolBusyOn busy colony

                    fun task ->
                        pool
                        |> List.tryPick (fun entry ->
                            if entry.Task = task then Some entry.Priority else None)
                        |> Option.defaultWith (fun () -> failtest $"{task} not pooled")

                let raided = rankIn [ Raided ] (defended true)
                let quiet = rankIn [] (defended false)

                Expect.isLessThan
                    (raided (Refill("tower-1", Energy)))
                    (raided (Upgrade "ctrl-1"))
                    "raided: the tower first"

                Expect.isLessThan
                    (raided (Repair "ram-1"))
                    (raided (Upgrade "ctrl-1"))
                    "and the struck rampart"

                Expect.isLessThan
                    (quiet (Upgrade "ctrl-1"))
                    (quiet (Refill("tower-1", Energy)))
                    "quiet: the Upgrade first, as before"
            }

            test "a loaded worker in a raided home inside the deadline refills the tower first" {
                let taskOf raided =
                    Map.tryFind "w1" (decideOn (defended raided)).Assignments

                Expect.equal
                    (taskOf true)
                    (Some(taskId (Refill("tower-1", Energy))))
                    "raided: the tower"

                Expect.equal
                    (taskOf false)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "quiet: the controller"
            }

            test "inside the deadline a busy colony's buffer keeps its own tier" {
                // The #523 yield of the buffer would starve the very upgrader
                // the deadline stands.
                let buffer ticks =
                    busyAt ticks (Refill("can-ctrl", Energy))

                Expect.isGreaterThan
                    (buffer 10_200)
                    (priorityOfTier UpgradeBuffer)
                    "outside the deadline: yielded under the buffer tier"

                Expect.equal
                    (buffer 9_800)
                    (priorityOfTier UpgradeBuffer)
                    "inside it: the buffer tier itself"
            }
        ]

[<Tests>]
let spawnHoldTests =
    testList
        "the spawn hold"
        [
            test "a Threat beside the spawn holds the cast; one across the room does not" {
                // A creep born into a Reach is a kill delivered.
                let staffed room =
                    { atLevel 2 room with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial = room |> withCreepsAt [ "w1", { X = 25; Y = 27 } ]
                    }

                let colony = staffed (openRoom 6)

                let { Intents = quiet } = decideOn colony
                Expect.isNonEmpty (spawnIntents quiet) "a quiet colony casts its deficit"

                let { Intents = beside } =
                    decideOn (
                        colony |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ]
                    )

                Expect.isEmpty
                    (spawnIntents beside)
                    "the doorstep is in the Reach: nothing is cast into it"

                let { Intents = across } =
                    decideOn (
                        colony |> facing [ hostileAt "h-1" { X = 31; Y = 31 } [ Attack; Move ] ]
                    )

                Expect.isNonEmpty
                    (spawnIntents across)
                    "a Threat that reaches no tile beside the spawn holds nothing"
            }

            test "the disaster fallback holds too: an empty colony casts nothing under fire" {
                // The one cast that ignores the bank's capacity still does not
                // ignore the Reach — the first creep of an empty colony is the
                // one that can least afford to be born under fire.
                let empty = atLevel 2 (openRoom 6)

                let { Intents = quiet } = decideOn empty

                Expect.isNonEmpty
                    (spawnIntents quiet)
                    "an empty colony casts from whatever is banked"

                let { Intents = raided } =
                    decideOn (
                        empty |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ]
                    )

                Expect.isEmpty (spawnIntents raided) "and holds while the doorstep is hot"
            }
        ]
