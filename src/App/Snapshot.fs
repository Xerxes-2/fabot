// Builds the tick's Snapshot projection from the live game objects.
module Fabot.Snapshot

open Fabot.Bindings
open Fabot.Core.Types

let build () : Snapshot =
    { Time = Game.time
      Spawns =
        objectValues<ISpawn> Game.spawns
        |> Array.map (fun s ->
            { Name = s.name
              EnergyAvailable = s.room.energyAvailable
              IsSpawning = not (isNull s.spawning) })
        |> Array.toList
      Creeps =
        objectValues<ICreep> Game.creeps
        |> Array.map (fun c -> { Name = c.name })
        |> Array.toList }
