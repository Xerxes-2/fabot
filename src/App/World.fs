module Fabot.World

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Vocabulary
open Fabot.Core.World

/// FIND_CONSTRUCTION_SITES: any owner's.
let private findConstructionSites = 111

[<Emit("$0.getRawBuffer()")>]
let private rawBuffer (_terrain: ITerrain) : byte[] = jsNative

[<Emit("($0 == null ? 0 : ($0.getCapacity() ?? $0.getCapacity('energy') ?? 0))")>]
let private capacityOf (_store: obj) : int = jsNative

[<Emit("($0 === undefined || $0 === null)")>]
let private absent (_value: obj) : bool = jsNative

let private orZero (value: int) = if absent (box value) then 0 else value

let private posOf (p: IRoomPosition) : Pos = { Room = p.roomName; X = p.x; Y = p.y }

let private ownerOf (my: obj) (owner: IOwner) : Owner =
    if my = box true then Ours
    elif absent (box owner) then Nobody
    else Theirs owner.username

let private storeOf (store: obj) : Store =
    if absent store then
        Store.empty
    else
        {
            Held =
                objectEntries store
                |> Array.choose (fun (name, amount) ->
                    match amount with
                    | :? float as n when n > 0.0 -> Some(resourceOfName name, int n)
                    | _ -> None)
            Capacity = capacityOf store
        }

let private structureOf (s: IStructure) : Structure =
    let kind = Kind.ofName s.structureType
    let owner = ownerOf s?my s.owner

    {
        Id = s.id
        Kind = kind
        Pos = posOf s.pos
        Owner = owner
        Hits = orZero s.hits
        HitsMax = orZero s.hitsMax
        Store = storeOf s?store
        Cooldown = orZero s.cooldown
        Walkable =
            match kind with
            | Kind.Rampart -> owner = Ours || s.isPublic
            | _ -> Kind.walkable kind
    }

let private siteOf (s: obj) : Site =
    {
        Id = s?id
        Kind = Kind.ofName s?structureType
        Pos = posOf s?pos
        Owner = ownerOf s?my s?owner
        Progress = s?progress
        ProgressTotal = s?progressTotal
    }

let private controllerOf (c: IController) : Controller =
    {
        Id = c.id
        Pos = posOf c.pos
        Owner = ownerOf (box c.my) c.owner
        Level = c.level
        Progress = orZero c?progress
        ProgressTotal = orZero c?progressTotal
        TicksToDowngrade = orZero c.ticksToDowngrade
        Reservation =
            if absent (box c.reservation) then
                None
            else
                Some(c.reservation.username, c.reservation.ticksToEnd)
        SafeMode = orZero c.safeMode
        SafeModeAvailable = orZero c.safeModeAvailable
    }

let private partOf (p: IBodyPartDef) : Part =
    let kind = partOfName p.``type`` |> Option.defaultValue Tough

    {
        Kind = kind
        Hits = p.hits
        Boost =
            if absent (box p.boost) then
                Unboosted
            else
                Boost.ofCompound kind p.boost
    }

let private creepOf (creep: ICreep) : Creep =
    {
        Id = creep.id
        Name = creep.name
        Owner = ownerOf creep?my creep.owner
        Pos = posOf creep.pos
        Body = creep.body |> Array.map partOf
        Hits = creep.hits
        HitsMax = creep.hitsMax
        TicksToLive = orZero creep.ticksToLive
        Fatigue = creep.fatigue
        Store = storeOf creep?store
    }

let private find<'T> (room: IRoom) (kind: int) : 'T[] = room.find kind |> Array.map unbox<'T>

let private roomOf (room: IRoom) : Room =
    {
        Name = room.name
        Terrain = rawBuffer (Game.map.getRoomTerrain room.name)
        Controller =
            if absent (box room.controller) then
                None
            else
                Some(controllerOf room.controller)
        EnergyAvailable = room.energyAvailable
        EnergyCapacity = room.energyCapacityAvailable
        Sources =
            find<ISource> room findSources
            |> Array.map (fun s ->
                {
                    Id = s.id
                    Pos = posOf s.pos
                    Energy = s.energy
                    EnergyCapacity = orZero s?energyCapacity
                    TicksToRegeneration = orZero s.ticksToRegeneration
                })
        Minerals =
            find<IMineral> room findMinerals
            |> Array.map (fun m ->
                {
                    Id = m.id
                    Pos = posOf m.pos
                    Resource = resourceOfName m.mineralType
                    Amount = m.mineralAmount
                })
        Structures = find<IStructure> room findStructures |> Array.map structureOf
        Sites = find<obj> room findConstructionSites |> Array.map siteOf
        Piles =
            find<IResource> room findDroppedResources
            |> Array.map (fun r ->
                {
                    Id = r.id
                    Pos = posOf r.pos
                    Resource = resourceOfName r.resourceType
                    Amount = r.amount
                })
        Remains =
            Array.append (find<ITombstone> room findTombstones) (find<ITombstone> room findRuins)
            |> Array.map (fun t ->
                {
                    Id = t.id
                    Pos = posOf t.pos
                    Store = storeOf (box t.store)
                })
        Hostiles = find<ICreep> room findHostileCreeps |> Array.map creepOf
    }

let private spawnOf (spawn: ISpawn) : Spawn =
    {
        Id = spawn.id
        Name = spawn.name
        Pos = posOf spawn.pos
        Spawning =
            if isNull spawn.spawning then
                None
            else
                let name = spawn.spawning.name
                let creep: ICreep = Game.creeps?(name)

                Some
                    {
                        Name = name
                        Remaining = spawn.spawning.remainingTime
                        Body =
                            if absent (box creep) then
                                [||]
                            else
                                creep.body |> Array.map partOf
                    }
    }

let private me (spawns: ISpawn[]) (creeps: ICreep[]) (rooms: IRoom[]) : string =
    match spawns |> Array.tryHead, creeps |> Array.tryHead with
    | Some spawn, _ -> spawn.owner.username
    | None, Some creep -> creep.owner.username
    | None, None ->
        rooms
        |> Array.tryPick (fun room ->
            let c = room.controller

            if not (absent (box c)) && c.my then
                Some c.owner.username
            else
                None)
        |> Option.defaultValue ""

let ofGame () : World =
    let spawns = objectValues<ISpawn> Game.spawns
    let creeps = objectValues<ICreep> Game.creeps
    let rooms = objectValues<IRoom> Game.rooms

    {
        Time = Game.time
        Me = me spawns creeps rooms
        CpuLimit = Game.cpu.limit
        Bucket = Game.cpu.bucket
        Rooms = rooms |> Array.map roomOf
        Spawns = spawns |> Array.map spawnOf
        Creeps = creeps |> Array.filter (fun c -> not c.spawning) |> Array.map creepOf
        OurSites = (objectEntries Game.constructionSites).Length
    }
