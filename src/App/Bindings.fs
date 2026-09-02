// Minimal hand-written bindings: only the API surface the bot uses.
module Fabot.Bindings

open Fable.Core

type ICpu =
    abstract getUsed: unit -> float

/// Screeps `FIND_SOURCES` constant.
let findSources = 105

type IStore =
    abstract getFreeCapacity: resource: string -> int
    abstract getUsedCapacity: resource: string -> int

type ISource =
    abstract id: string

type IController =
    abstract id: string
    /// True when this controller is owned by us.
    abstract my: bool

type IRoom =
    /// Energy available for spawning in this room (spawn + extensions).
    abstract energyAvailable: int
    abstract find: findType: int -> obj[]
    /// Null in rooms without a controller.
    abstract controller: IController

type ISpawn =
    abstract name: string
    /// Null when the spawn is idle.
    abstract spawning: obj
    abstract room: IRoom
    abstract store: IStore
    abstract spawnCreep: body: string[] * name: string -> int

type ICreep =
    abstract name: string
    /// True while the creep is still being built inside the spawn.
    abstract spawning: bool
    abstract store: IStore
    abstract harvest: target: obj -> int
    abstract transfer: target: obj * resource: string -> int
    abstract upgradeController: target: obj -> int
    abstract moveTo: target: obj -> int

type IGame =
    abstract time: int
    abstract cpu: ICpu
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
let objectValues<'T> (o: obj) : 'T[] = jsNative

[<Emit("Object.entries($0)")>]
let objectEntries (o: obj) : (string * obj)[] = jsNative
