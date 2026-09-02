// Builds the tick's Snapshot projection from the live game objects.
module Fabot.Snapshot

open Fabot.Bindings
open Fabot.Core.Types

/// How far from the spawn (Chebyshev) the placement projection looks.
/// Room 6 covers the full RCL2/RCL3 extension checkerboard with slack.
let private planningRadius = 6

let private buildPlacement (spawn: ISpawn) : PlacementInfo =
    let room = spawn.room
    let terrain = Game.map.getRoomTerrain room.name
    let center = spawn.pos

    let walkable =
        Set.ofList
            [
                // Stay off row/column 0 and 49: exit tiles cannot hold structures.
                for x in max 1 (center.x - planningRadius) .. min 48 (center.x + planningRadius) do
                    for y in max 1 (center.y - planningRadius) .. min 48 (center.y + planningRadius) do
                        if terrain.get (x, y) <> terrainMaskWall then
                            { X = x; Y = y }
            ]

    let structures = room.find findStructures |> Array.map (fun o -> o :?> IStructure)

    let sites =
        room.find findMyConstructionSites
        |> Array.map (fun o -> o :?> IConstructionSite)

    let occupied =
        Set.ofArray (
            Array.append
                (structures |> Array.map (fun st -> { X = st.pos.x; Y = st.pos.y }))
                (sites |> Array.map (fun site -> { X = site.pos.x; Y = site.pos.y }))
        )

    {
        RoomName = room.name
        SpawnPos = { X = center.x; Y = center.y }
        Walkable = walkable
        Occupied = occupied
        BuiltExtensions =
            structures
            |> Array.filter (fun st -> st.structureType = structureExtension)
            |> Array.length
        PendingExtensions =
            sites
            |> Array.filter (fun site -> site.structureType = structureExtension)
            |> Array.length
    }

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
                    Some({ Id = c.id; Level = c.level }: ControllerInfo)
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
        // Single-colony assumption: only the first spawn's room gets planned.
        Placement = spawns |> Array.tryHead |> Option.map buildPlacement
    }
