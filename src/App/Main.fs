module Fabot.Main

open Fable.Core
open Fabot.Bindings

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    JS.console.log ($"fabot heartbeat: tick {Game.time} cpu {Game.cpu.getUsed ()}")
