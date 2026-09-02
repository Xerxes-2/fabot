// Minimal hand-written bindings: only the API surface the bot uses.
module Fabot.Bindings

open Fable.Core

type ICpu =
    abstract getUsed: unit -> float

/// Screeps `FIND_SOURCES` constant.
let findSources = 105

/// Screeps `FIND_MY_STRUCTURES` constant.
let findMyStructures = 108

/// Screeps `FIND_STRUCTURES` constant (any owner, includes neutral).
let findStructures = 107

/// Screeps `FIND_MY_CONSTRUCTION_SITES` constant.
let findMyConstructionSites = 114

/// Screeps `TERRAIN_MASK_WALL` constant.
let terrainMaskWall = 1

/// Screeps `STRUCTURE_SPAWN` constant.
let structureSpawn = "spawn"

/// Screeps `STRUCTURE_EXTENSION` constant.
let structureExtension = "extension"

type IStore =
    abstract getFreeCapacity: resource: string -> int
    abstract getUsedCapacity: resource: string -> int

type ISource =
    abstract id: string

type IRoomPosition =
    abstract x: int
    abstract y: int

type IStructure =
    abstract id: string
    /// Screeps STRUCTURE_* string, e.g. "spawn" or "extension".
    abstract structureType: string
    abstract store: IStore
    abstract pos: IRoomPosition

type IConstructionSite =
    abstract id: string
    /// Screeps STRUCTURE_* string of what is being built.
    abstract structureType: string
    abstract pos: IRoomPosition

type IController =
    abstract id: string
    /// True when this controller is owned by us.
    abstract my: bool
    /// Controller level (RCL).
    abstract level: int

type IRoom =
    abstract name: string
    /// Energy available for spawning in this room (spawn + extensions).
    abstract energyAvailable: int
    abstract find: findType: int -> obj[]
    /// Null in rooms without a controller.
    abstract controller: IController
    abstract createConstructionSite: x: int * y: int * structureType: string -> int

type ISpawn =
    abstract name: string
    /// Null when the spawn is idle.
    abstract spawning: obj
    abstract room: IRoom
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract spawnCreep: body: string[] * name: string -> int

type ICreep =
    abstract name: string
    /// True while the creep is still being built inside the spawn.
    abstract spawning: bool
    abstract store: IStore
    abstract harvest: target: obj -> int
    abstract transfer: target: obj * resource: string -> int
    abstract build: target: obj -> int
    abstract upgradeController: target: obj -> int
    abstract moveTo: target: obj -> int

type ITerrain =
    /// 0 plain, TERRAIN_MASK_WALL wall, 2 swamp.
    abstract get: x: int * y: int -> int

type IGameMap =
    abstract getRoomTerrain: roomName: string -> ITerrain

type IGame =
    abstract time: int
    abstract cpu: ICpu
    abstract map: IGameMap
    /// Hash of room name -> room (visible rooms only).
    abstract rooms: obj
    /// Hash of spawn name -> spawn.
    abstract spawns: obj
    /// Hash of creep name -> creep.
    abstract creeps: obj
    /// Null when no object with that id exists (or it is out of sight).
    abstract getObjectById: id: string -> obj

[<Global("Game")>]
let Game: IGame = jsNative

[<Global("Memory")>]
let Memory: obj = jsNative

[<Emit("Object.values($0)")>]
let objectValues<'T> (_o: obj) : 'T[] = jsNative

[<Emit("Object.entries($0)")>]
let objectEntries (_o: obj) : (string * obj)[] = jsNative
