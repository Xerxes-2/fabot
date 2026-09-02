// Minimal hand-written bindings: only what the heartbeat needs.
module Fabot.Bindings

open Fable.Core

type ICpu =
    abstract getUsed: unit -> float

type IGame =
    abstract time: int
    abstract cpu: ICpu

[<Global("Game")>]
let Game: IGame = jsNative
