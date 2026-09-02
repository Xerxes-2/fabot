module Fabot.Core.Tests.DecideTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

let bareRespawn =
    {
        Time = 42
        Spawns =
            [
                {
                    Name = "Spawn1"
                    EnergyAvailable = 300
                    IsSpawning = false
                }
            ]
        Refillables = [ { Id = "spawn-1"; FreeCapacity = 0 } ]
        Sources = [ { Id = "src-a" }; { Id = "src-b" } ]
        Controller = Some { Id = "ctrl-1" }
        ConstructionSites = []
        Creeps = []
    }

let worker name energy freeCapacity =
    {
        Name = name
        Energy = energy
        FreeCapacity = freeCapacity
    }

let spawnIntents intents =
    intents
    |> List.choose (function
        | SpawnCreep(s, b, c) -> Some(s, b, c)
        | _ -> None)

[<Tests>]
let plannerTests =
    testList
        "planner"
        [
            test "one Harvest task per source" {
                let tasks = planTasks bareRespawn

                let harvests =
                    tasks
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal
                    harvests
                    [ "src-a"; "src-b" ]
                    "each source gets exactly one Harvest task"
            }

            test "a controller yields an Upgrade task" {
                let upgrades =
                    planTasks bareRespawn
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.equal upgrades [ "ctrl-1" ] "the controller gets exactly one Upgrade task"
            }

            test "no Upgrade task without a controller" {
                let tasks = planTasks { bareRespawn with Controller = None }

                let upgrades =
                    tasks
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.isEmpty upgrades "nothing to upgrade"
            }

            test "each construction site yields a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" }; { Id = "site-2" } ]
                    }

                let builds =
                    planTasks snapshot
                    |> List.choose (function
                        | Build siteId -> Some siteId
                        | _ -> None)

                Expect.equal builds [ "site-1"; "site-2" ] "one Build task per construction site"
            }

            test "a structure missing energy gets a Refill task; a full structure gets none" {
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                { Id = "spawn-1"; FreeCapacity = 50 }
                                { Id = "ext-1"; FreeCapacity = 0 }
                                { Id = "ext-2"; FreeCapacity = 50 }
                            ]
                    }

                let refills =
                    planTasks snapshot
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "spawn-1"; "ext-2" ]
                    "only structures with free capacity need a Refill"
            }
        ]

[<Tests>]
let tests =
    testList
        "decide"
        [
            test "an empty creep is matched to a Harvest task and remembered" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let intents, assignments = decide snapshot Map.empty
                Expect.contains intents (HarvestSource("w1", "src-a")) "empty creep goes harvesting"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-a")
                    "assignment is remembered"
            }

            test "bare respawn yields exactly one spawn Intent" {
                let intents, _ = decide bareRespawn Map.empty

                match spawnIntents intents with
                | [ (spawnName, body, creepName) ] ->
                    Expect.equal spawnName "Spawn1" "spawns from the only spawn"
                    Expect.isNonEmpty body "body must not be empty"
                    Expect.isNotEmpty creepName "creep needs a name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawn Intent body is affordable at bare-respawn energy" {
                let intents, _ = decide bareRespawn Map.empty

                for (_, body, _) in spawnIntents intents do
                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        300
                        "body cost within bare-respawn energy"
            }

            test "no spawn Intent when energy is below a worker body cost" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                {
                                    Name = "Spawn1"
                                    EnergyAvailable = 100
                                    IsSpawning = false
                                }
                            ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
            }

            test "no spawn Intent while the spawn is already spawning" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                {
                                    Name = "Spawn1"
                                    EnergyAvailable = 300
                                    IsSpawning = true
                                }
                            ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "spawn is busy"
            }

            test "one worker is below minimum: a second is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.hasLength (spawnIntents intents) 1 "a lone worker cannot keep the loop going"
            }

            test "no spawn Intent when workforce is at minimum" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50; worker "worker-2" 0 50 ]
                    }

                let intents, _ = decide snapshot Map.empty
                Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
            }

            test "empty creeps spread across sources instead of piling on one" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let _, assignments = decide snapshot Map.empty
                let assigned = assignments |> Map.toList |> List.map snd |> List.sort

                Expect.equal
                    assigned
                    [ "harvest:src-a"; "harvest:src-b" ]
                    "greedy matching balances load per task"
            }

            test "greedy matching counts kept assignments as load" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30; worker "w2" 0 50 ]
                    }

                let _, assignments = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some "harvest:src-a")
                    "w1 keeps its source"

                Expect.equal
                    (Map.tryFind "w2" assignments)
                    (Some "harvest:src-b")
                    "w2 avoids the occupied source"
            }

            test "assignments pass through unchanged when no creeps died" {
                let assignments = Map.ofList [ "worker-1", "harvest:src-a" ]

                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let _, kept = decide snapshot assignments
                Expect.equal kept assignments "assignments survive the tick"
            }

            test "an assignment sticks across ticks even when greedy would rebalance" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30 ]
                    }

                let assignments = Map.ofList [ "w1", "harvest:src-b" ]
                let intents, kept = decide snapshot assignments

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "harvest:src-b")
                    "no thrash: creep stays on its source"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-b"))
                    "intent follows the sticky assignment"
            }

            test "a creep that fills up is reassigned from Harvest to Refill" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "refill:spawn-1")
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1"))
                    "delivery intent emitted"
            }

            test "a creep that empties is reassigned from Refill back to Harvest" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let _, kept = decide snapshot (Map.ofList [ "w1", "refill:spawn-1" ])

                match Map.tryFind "w1" kept with
                | Some tid -> Expect.stringStarts tid "harvest:" "empty creep goes back to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "surplus: a full creep with a full spawn switches to upgrading" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "upgrade:ctrl-1")
                    "nothing to refill, so surplus goes to the controller"

                Expect.contains intents (UpgradeController("w1", "ctrl-1")) "upgrade intent emitted"
            }

            test "a hungry structure beats the controller for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let _, kept = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "refill:spawn-1")
                    "refill outranks upgrade while a structure is missing energy"
            }

            test "an upgrading creep that empties goes back to harvest" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let _, kept = decide snapshot (Map.ofList [ "w1", "upgrade:ctrl-1" ])

                match Map.tryFind "w1" kept with
                | Some tid -> Expect.stringStarts tid "harvest:" "spent creep returns to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test
                "a full creep with a full spawn and no controller is left unassigned with no intent" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])
                Expect.isEmpty (Map.toList kept) "no applicable task"

                let creepIntents =
                    intents
                    |> List.filter (function
                        | SpawnCreep _ -> false
                        | _ -> true)

                Expect.isEmpty creepIntents "idle creep emits nothing"
            }

            test "a full creep with a construction site and a full spawn goes building" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let intents, kept = decide snapshot (Map.ofList [ "w1", "harvest:src-a" ])

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "build:site-1")
                    "surplus energy goes into construction"

                Expect.contains intents (BuildSite("w1", "site-1")) "build intent emitted"
            }

            test "an empty creep is never matched to a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let _, kept = decide snapshot (Map.ofList [ "w1", "build:site-1" ])

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.stringStarts tid "harvest:" "empty creep goes harvesting instead"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "a hungry structure beats a construction site for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ { Id = "spawn-1"; FreeCapacity = 50 } ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let _, kept = decide snapshot Map.empty

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some "refill:spawn-1")
                    "refill outranks build while a structure is missing energy"
            }

            test "assignments of dead creeps are dropped" {
                let assignments = Map.ofList [ "ghost", "task-a" ]
                let _, kept = decide bareRespawn assignments
                Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
            }
        ]
