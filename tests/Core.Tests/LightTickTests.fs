/// The light tick: what a quiet tick replays of the last full one, what it
/// walks, and every fact that hands it over to a full tick instead.
module Fabot.Core.Tests.LightTickTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick

let private room = "W1N1"
let private tile x y = RoomPos.at room { X = x; Y = y }

let private creepAt x y hits : GlanceCreep = { Tile = tile x y; Hits = hits }

/// A quiet world: two creeps of ours, a spawn, one controller at level 5, and
/// nobody else.
let private quiet: Glance =
    {
        Creeps = Map.ofList [ "digger", creepAt 10 10 1000; "hauler", creepAt 20 20 500 ]
        Hostiles = []
        Structures = [ tile 30 30 ]
        Controllers = Map.ofList [ room, (5, false) ]
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

let private hostile x y owner armed : GlanceHostile =
    {
        Tile = tile x y
        Owner = owner
        Armed = armed
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
            test "a creep on the tile it worked from re-issues its repeatable work and nothing else" {
                Expect.equal
                    (LightTick.intents last stepped)
                    [ HarvestSource("digger", "src-a"); MoveCreep("hauler", Right) ]
                    "the harvest again and the next step; never the transfer or the spawn"
            }

            test "every repeatable work intent is kept and every one-shot one dropped" {
                let work =
                    [
                        HarvestSource("digger", "src-a")
                        UpgradeController("digger", "ctrl")
                        ReserveController("digger", "ctrl")
                        BuildSite("digger", "site")
                        RepairStructure("digger", "road")
                        DismantleStructure("digger", "wall")
                    ]

                let oneShot =
                    [
                        TransferEnergyToStructure("digger", "spawn", Energy)
                        WithdrawFromStore("digger", "store", Energy, None)
                        PickupPile("digger", "pile")
                        SpawnCreep("Spawn1", [ Move ], "x")
                        PlaceConstructionSite(tile 5 5, StructureKind.Road)
                        ClaimController("digger", "ctrl")
                        ClaimReactor("digger", "reactor")
                        SignController("digger", "ctrl", "hi")
                        SendFromTerminal("term", Energy, 100, "W2N2")
                        SayCreep("digger", "o/")
                    ]

                let full = LightTick.lastFull quiet Map.empty (work @ oneShot)

                Expect.equal full.Work work "the six repeatable verbs, in the full tick's order"
                Expect.isFalse full.Fought "none of them shoots or heals"

                Expect.equal
                    (LightTick.intents full quiet)
                    work
                    "and a light tick replays exactly those"
            }

            test "a creep off the tile it worked from re-issues nothing" {
                let pushed =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "digger" (creepAt 11 10 1000)
                    }

                Expect.equal
                    (LightTick.intents last pushed)
                    [ MoveCreep("hauler", Right) ]
                    "the digger waits for the next full tick"
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
                Expect.equal
                    (LightTick.intents last quiet)
                    [ HarvestSource("digger", "src-a") ]
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
        ]

[<Tests>]
let forcedTests =
    testList
        "light tick forced full"
        [
            test "a quiet world one step on forces nothing" {
                Expect.equal (LightTick.forced last stepped) None "the light tick runs"
            }

            test "an armed hostile anywhere in sight forces a full tick" {
                let raided =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Invader" true ]
                    }

                Expect.equal
                    (LightTick.forced last raided)
                    (Some(LightForce.ArmedHostile room))
                    "combat is decided"
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
                    (Some(LightForce.HostileNear room))
                    "a keeper that comes near"

                Expect.equal (LightTick.forced last far) None "one tile further is not near"
            }

            test "any hostile near a structure of ours forces a full tick" {
                let atTheSpawn =
                    { stepped with
                        Hostiles = [ hostile 33 33 "Rival" false ]
                    }

                Expect.equal
                    (LightTick.forced last atTheSpawn)
                    (Some(LightForce.HostileNear room))
                    "a body at the gate"
            }

            test "a hostile at the same coordinates in another room is not near" {
                let elsewhere =
                    { stepped with
                        Hostiles =
                            [
                                {
                                    Tile = RoomPos.at "W2N1" { X = 10; Y = 10 }
                                    Owner = "Rival"
                                    Armed = false
                                }
                            ]
                    }

                Expect.equal (LightTick.forced last elsewhere) None "range is within one room"
            }

            test "a full tick that shot or healed forces the next one full" {
                for fight in
                    [
                        AttackCreep("digger", "h")
                        RangedAttackCreep("digger", "h")
                        HealCreep("digger", "hauler")
                        RangedHealCreep("digger", "hauler")
                        FireTower("tower", "h")
                        HealWithTower("tower", "hauler")
                        ActivateSafeMode "ctrl"
                    ] do
                    let fought = LightTick.lastFull quiet Map.empty [ fight ]

                    Expect.isTrue fought.Fought $"%A{fight} is a fight"

                    Expect.equal
                        (LightTick.forced fought quiet)
                        (Some LightForce.Fought)
                        $"%A{fight} is never replayed"
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

            test "a creep standing on its room's border ring forces a full tick, on every edge" {
                // It crossed on the full tick and stands on the neighbour's
                // ring; standing still there, the engine carries it straight
                // back, so the light tick may not leave it be.
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
                                    }
                        }

                    Expect.equal
                        (LightTick.forced last crossed)
                        (Some(LightForce.OnBorder "hauler"))
                        $"on ({x},{y})"

                Expect.equal (LightTick.forced last stepped) None "and one tile in, nothing"
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
                    (Some(LightForce.ArmedHostile room))
                    "the armed hostile comes first"
            }
        ]
