/// The loop: read, decide, act, write, each phase timed and each module
/// isolated (`ARCHITECTURE.md` §1, §2).
module Fabot.Main

open Fable.Core
open Fabot.Bindings
open Fabot.Core

/// The modules, in the order they decide. Each lands here as it is built:
/// Economy, Layout, Strategy, Fight.
let private modules: Tick.Module list = []

/// The change this bundle was built from, off `scripts/bundle.mjs`'s banner.
[<Emit("(typeof __FABOT_VERSION__ === 'string' ? __FABOT_VERSION__ : 'unbuilt')")>]
let private version: string = jsNative

let private faultsKept = 20

/// The fault ring lives on the heap and starts empty after a reset; observe
/// carries it out each tick.
let mutable private faults: Wire.Fault list = []

let private recordFault (where: string) (message: string) =
    let fault: Wire.Fault =
        {
            Tick = Game.time
            Where = where
            Message = message
        }

    faults <- fault :: faults |> List.truncate faultsKept

let private clock () = Game.cpu.getUsed ()

let private runTick () =
    let phases = ResizeArray<string * float>()

    let timed name (f: unit -> 'a) : 'a =
        let start = clock ()
        let result = f ()
        phases.Add(name, clock () - start)
        result

    let state = timed "memory" Wire.readState
    let world = timed "world" World.ofGame
    let result = Tick.run clock modules world state

    for report in result.Reports do
        phases.Add(report.Module, report.Cpu)

        match report.Outcome with
        | Tick.Faulted message -> recordFault report.Module message
        | Tick.Ran
        | Tick.Off -> ()

    let refusals = timed "execute" (fun () -> Executor.run result.Intents)

    if Wire.recording () then
        timed "record" (fun () -> Wire.record world.Time world)
        |> Option.iter (recordFault "record")

    Wire.writeObserve
        {
            Tick = world.Time
            Version = version
            Bucket = world.Bucket
            Total = clock ()
            Phases = List.ofSeq phases
            Off = Set.toList state.Off
            Faults = faults
            Refusals = refusals
        }

/// Exported as `loop` on the bundled `main` module; the engine calls it every
/// tick. A throw outside any module is recorded, written, and rethrown.
let loop () =
    try
        runTick ()
    with error ->
        try
            recordFault "tick" error.Message

            Wire.writeObserve
                {
                    Tick = Game.time
                    Version = version
                    Bucket = Game.cpu.bucket
                    Total = clock ()
                    Phases = []
                    Off = []
                    Faults = faults
                    Refusals = []
                }
        with _ ->
            ()

        raise error
