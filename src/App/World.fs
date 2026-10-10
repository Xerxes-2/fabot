/// The one reader of `Game`: builds `Core.World.World` once a tick.
module Fabot.World

open Fabot.Bindings
open Fabot.Core.World

let private posOf (p: IRoomPosition) : Pos = { Room = p.roomName; X = p.x; Y = p.y }

let private roomOf (room: IRoom) : Room =
    let controller =
        match room.controller with
        | c when isNull (box c) || not c.my -> None
        | c ->
            Some
                {
                    Id = c.id
                    Level = c.level
                    TicksToDowngrade =
                        if isNull (box c.ticksToDowngrade) then
                            0
                        else
                            c.ticksToDowngrade
                }

    {
        Name = room.name
        Controller = controller
        EnergyAvailable = room.energyAvailable
        EnergyCapacity = room.energyCapacityAvailable
    }

let private spawnOf (spawn: ISpawn) : Spawn =
    {
        Id = spawn.id
        Name = spawn.name
        Room = spawn.room.name
        Spawning =
            if isNull spawn.spawning then
                0
            else
                spawn.spawning.remainingTime
    }

let private creepOf (creep: ICreep) : Creep =
    {
        Name = creep.name
        Pos = posOf creep.pos
        Hits = creep.hits
        HitsMax = creep.hitsMax
        TicksToLive = creep.ticksToLive
    }

let ofGame () : World =
    {
        Time = Game.time
        CpuLimit = Game.cpu.limit
        Bucket = Game.cpu.bucket
        Rooms = objectValues<IRoom> Game.rooms |> Array.map roomOf
        Spawns = objectValues<ISpawn> Game.spawns |> Array.map spawnOf
        Creeps =
            objectValues<ICreep> Game.creeps
            |> Array.filter (fun c -> not c.spawning)
            |> Array.map creepOf
    }
