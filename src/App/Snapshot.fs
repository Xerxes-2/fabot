// Builds the tick's Snapshot projection from the live game objects.
module Fabot.Snapshot

open Fabot.Bindings
open Fabot.Core.Types

let build () : Snapshot =
    let spawns = objectValues<ISpawn> Game.spawns
    { Time = Game.time
      Spawns =
        spawns
        |> Array.map (fun s ->
            { Name = s.name
              EnergyAvailable = s.room.energyAvailable
              FreeCapacity = s.store.getFreeCapacity "energy"
              IsSpawning = not (isNull s.spawning) })
        |> Array.toList
      Sources =
        spawns
        |> Array.collect (fun s -> s.room.find findSources)
        |> Array.map (fun o -> { Id = (o :?> ISource).id })
        |> Array.distinctBy (fun s -> s.Id)
        |> Array.toList
      Creeps =
        objectValues<ICreep> Game.creeps
        // A creep still inside the spawn cannot act; keep it out of the pool.
        |> Array.filter (fun c -> not c.spawning)
        |> Array.map (fun c ->
            { Name = c.name
              Energy = c.store.getUsedCapacity "energy"
              FreeCapacity = c.store.getFreeCapacity "energy" })
        |> Array.toList }
