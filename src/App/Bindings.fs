// Minimal hand-written bindings: only the API surface the bot uses.
module Fabot.Bindings

open Fable.Core

type ICpu =
    abstract getUsed: unit -> float

type IRoom =
    /// Energy available for spawning in this room (spawn + extensions).
    abstract energyAvailable: int

type ISpawn =
    abstract name: string
    /// Null when the spawn is idle.
    abstract spawning: obj
    abstract room: IRoom
    abstract spawnCreep: body: string[] * name: string -> int

type ICreep =
    abstract name: string

type IGame =
    abstract time: int
    abstract cpu: ICpu
    /// Hash of spawn name -> spawn.
    abstract spawns: obj
    /// Hash of creep name -> creep.
    abstract creeps: obj

[<Global("Game")>]
let Game: IGame = jsNative

[<Global("Memory")>]
let Memory: obj = jsNative

[<Emit("Object.values($0)")>]
let objectValues<'T> (o: obj) : 'T[] = jsNative

[<Emit("Object.entries($0)")>]
let objectEntries (o: obj) : (string * obj)[] = jsNative
