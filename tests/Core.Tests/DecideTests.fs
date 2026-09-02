module Fabot.Core.Tests.DecideTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

let bareRespawn =
    { Time = 42
      Spawns = [ { Name = "Spawn1"; EnergyAvailable = 300; IsSpawning = false } ]
      Creeps = [] }

let spawnIntents intents =
    intents |> List.choose (function SpawnCreep (s, b, c) -> Some (s, b, c))

[<Tests>]
let tests =
    testList "decide" [
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
                Expect.isLessThanOrEqual (bodyCost body) 300 "body cost within bare-respawn energy"
        }

        test "no spawn Intent when energy is below a worker body cost" {
            let snapshot =
                { bareRespawn with
                    Spawns = [ { Name = "Spawn1"; EnergyAvailable = 100; IsSpawning = false } ] }
            let intents, _ = decide snapshot Map.empty
            Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
        }

        test "no spawn Intent while the spawn is already spawning" {
            let snapshot =
                { bareRespawn with
                    Spawns = [ { Name = "Spawn1"; EnergyAvailable = 300; IsSpawning = true } ] }
            let intents, _ = decide snapshot Map.empty
            Expect.isEmpty (spawnIntents intents) "spawn is busy"
        }

        test "no spawn Intent when workforce is at minimum" {
            let snapshot = { bareRespawn with Creeps = [ { Name = "worker-1" } ] }
            let intents, _ = decide snapshot Map.empty
            Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
        }

        test "assignments pass through unchanged when no creeps died" {
            let assignments = Map.ofList [ "worker-1", "task-a" ]
            let snapshot = { bareRespawn with Creeps = [ { Name = "worker-1" } ] }
            let _, kept = decide snapshot assignments
            Expect.equal kept assignments "assignments survive the tick"
        }

        test "assignments of dead creeps are dropped" {
            let assignments = Map.ofList [ "ghost", "task-a" ]
            let _, kept = decide bareRespawn assignments
            Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
        }
    ]
