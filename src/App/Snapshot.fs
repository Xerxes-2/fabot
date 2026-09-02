// Builds the tick's Snapshot projection from the live game objects.
module Fabot.Snapshot

open Fabot.Bindings
open Fabot.Core.Types

let build () : Snapshot =
    let spawns = objectValues<ISpawn> Game.spawns

    {
        Time = Game.time
        Spawns =
            spawns
            |> Array.map (fun s ->
                {
                    Name = s.name
                    EnergyAvailable = s.room.energyAvailable
                    IsSpawning = not (isNull s.spawning)
                })
            |> Array.toList
        Refillables =
            spawns
            |> Array.collect (fun s -> s.room.find findMyStructures)
            |> Array.map (fun o -> o :?> IStructure)
            |> Array.filter (fun st ->
                st.structureType = structureSpawn || st.structureType = structureExtension)
            |> Array.distinctBy (fun st -> st.id)
            |> Array.map (fun st ->
                {
                    Id = st.id
                    FreeCapacity = st.store.getFreeCapacity "energy"
                }
                : RefillableInfo)
            |> Array.toList
        Sources =
            spawns
            |> Array.collect (fun s -> s.room.find findSources)
            |> Array.map (fun o -> ({ Id = (o :?> ISource).id }: SourceInfo))
            |> Array.distinctBy (fun s -> s.Id)
            |> Array.toList
        Controller =
            spawns
            |> Array.tryPick (fun s ->
                let c = s.room.controller

                if not (isNull (box c)) && c.my then
                    Some({ Id = c.id }: ControllerInfo)
                else
                    None)
        ConstructionSites =
            spawns
            |> Array.collect (fun s -> s.room.find findMyConstructionSites)
            |> Array.map (fun o -> ({ Id = (o :?> IConstructionSite).id }: ConstructionSiteInfo))
            |> Array.distinctBy (fun site -> site.Id)
            |> Array.toList
        Creeps =
            objectValues<ICreep> Game.creeps
            // A creep still inside the spawn cannot act; keep it out of the pool.
            |> Array.filter (fun c -> not c.spawning)
            |> Array.map (fun c ->
                {
                    Name = c.name
                    Energy = c.store.getUsedCapacity "energy"
                    FreeCapacity = c.store.getFreeCapacity "energy"
                })
            |> Array.toList
    }
