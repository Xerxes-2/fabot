// Builds the tick's Snapshot projection from the live game objects.
module Fabot.Snapshot

open Fabot.Bindings
open Fabot.Core.Types

/// Classify one tile of engine terrain into the Core's three states.
let private terrainAt (terrain: ITerrain) x y =
    let mask = terrain.get (x, y)

    if mask &&& terrainMaskWall <> 0 then Wall
    elif mask &&& terrainMaskSwamp <> 0 then Swamp
    else Plain

let private posOf (p: IRoomPosition) : Pos = { X = p.x; Y = p.y }

/// Classify an engine part-type string into the Core's body vocabulary:
/// the reverse of the Core's one part-name table. The engine's part set
/// is closed, so the fallback for an unmatched string is unreachable;
/// Tough keeps the classification total without inventing a case.
let private bodyPartOf =
    let byName = allBodyParts |> List.map (fun p -> partName p, p) |> Map.ofList
    fun partType -> byName |> Map.tryFind partType |> Option.defaultValue Tough

/// Classify an engine STRUCTURE_* string into the Core's built kinds.
let private builtKindOf structureType =
    if structureType = structureSpawn then
        BuiltKind.Spawn
    elif structureType = structureExtension then
        BuiltKind.Extension
    else
        BuiltKind.Other

let private buildSpatial (spawn: ISpawn) : SpatialInfo =
    let room = spawn.room
    let terrain = Game.map.getRoomTerrain room.name

    // Rows and columns 0/49 are exit tiles — stepping on one teleports the
    // creep into the next room. They stay out of the projection: an absent
    // tile is impassable, so no path or Work Area ever uses an exit.
    let tiles =
        Map.ofList
            [
                for x in 1..48 do
                    for y in 1..48 do
                        { X = x; Y = y }, terrainAt terrain x y
            ]

    let structures = room.find findStructures |> Array.map (fun o -> o :?> IStructure)

    let sites =
        room.find findMyConstructionSites
        |> Array.map (fun o -> o :?> IConstructionSite)

    let sources = room.find findSources |> Array.map (fun o -> o :?> ISource)

    // The controller travels through FIND_STRUCTURES on live servers, but
    // is projected explicitly so nothing depends on that detail.
    let controllers =
        if isNull (box room.controller) then
            [||]
        else
            [| room.controller |]

    // Structures a creep can stand on; every other kind blocks its tile
    // (Screeps OBSTACLE_OBJECT_TYPES).
    let walkableStructures = [ structureRoad; structureContainer; structureRampart ]

    {
        RoomName = Some room.name
        Terrain = tiles
        TargetPositions =
            Map.ofArray (
                Array.concat
                    [
                        sources |> Array.map (fun s -> s.id, posOf s.pos)
                        structures |> Array.map (fun st -> st.id, posOf st.pos)
                        sites |> Array.map (fun site -> site.id, posOf site.pos)
                        controllers |> Array.map (fun c -> c.id, posOf c.pos)
                    ]
            )
        // Same array order as TargetPositions, so a controller that also
        // travels through FIND_STRUCTURES resolves to Controller both times.
        TargetKinds =
            Map.ofArray (
                Array.concat
                    [
                        sources |> Array.map (fun s -> s.id, Source)
                        structures
                        |> Array.map (fun st -> st.id, Structure(builtKindOf st.structureType))
                        sites
                        |> Array.map (fun site -> site.id, Site(builtKindOf site.structureType))
                        controllers |> Array.map (fun c -> c.id, Controller)
                    ]
            )
        CreepPositions =
            objectValues<ICreep> Game.creeps
            |> Array.filter (fun c -> not c.spawning)
            |> Array.map (fun c -> c.name, posOf c.pos)
            |> Map.ofArray
        Obstacles =
            Set.ofArray (
                Array.concat
                    [
                        structures
                        |> Array.filter (fun st ->
                            not (List.contains st.structureType walkableStructures))
                        |> Array.map (fun st -> posOf st.pos)
                        // The engine refuses to move a creep onto its own
                        // obstacle-type construction site, so those tiles
                        // block exactly like the finished structure would.
                        sites
                        |> Array.filter (fun site ->
                            not (List.contains site.structureType walkableStructures))
                        |> Array.map (fun site -> posOf site.pos)
                        controllers |> Array.map (fun c -> posOf c.pos)
                    ]
            )
    }

let build () : Snapshot =
    let spawns = objectValues<ISpawn> Game.spawns

    let spawnRooms =
        spawns |> Array.map (fun s -> s.room) |> Array.distinctBy (fun r -> r.name)

    {
        Time = Game.time
        Spawns =
            spawns
            |> Array.map (fun s ->
                {
                    Name = s.name
                    Id = s.id
                    RoomName = s.room.name
                    IsSpawning = not (isNull s.spawning)
                })
            |> Array.toList
        RoomEnergy =
            spawnRooms
            |> Array.map (fun r ->
                r.name,
                {
                    Available = r.energyAvailable
                    Capacity = r.energyCapacityAvailable
                })
            |> Map.ofArray
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
                    Some(
                        {
                            Id = c.id
                            Level = c.level
                            TicksToDowngrade = c.ticksToDowngrade
                            SafeModeAvailable = c.safeModeAvailable
                            // `safeMode` is the tick count remaining,
                            // undefined when safe mode is off.
                            SafeModeActive = not (isNull (box c.safeMode))
                        }
                        : ControllerInfo
                    )
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
        Hostiles =
            spawnRooms
            |> Array.collect (fun r -> r.find findHostileCreeps)
            |> Array.map (fun o ->
                let c = o :?> ICreep

                {
                    Body = c.body |> Array.map (fun p -> bodyPartOf p.``type``) |> Array.toList
                }
                : HostileInfo)
            |> Array.toList
        // Single-colony assumption: only the first spawn's room is projected.
        Spatial =
            spawns
            |> Array.tryHead
            |> Option.map buildSpatial
            |> Option.defaultValue SpatialInfo.empty
    }
