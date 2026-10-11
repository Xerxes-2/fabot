module Fabot.Main

open Fable.Core
open Fabot.Bindings
open Fabot.Core

/// In decision order: Economy, Layout, Strategy, Fight.
let private modules: Tick.Module list = [ Economy.Run.economy; Layout.Run.layout ]

/// Set by `scripts/bundle.mjs`'s banner.
[<Emit("(typeof __FABOT_VERSION__ === 'string' ? __FABOT_VERSION__ : 'unbuilt')")>]
let private version: string = jsNative

let private faultsKept = 20

/// Heap only, so empty after a reset; observe carries it out each tick.
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

/// Heap only, so empty after a reset, when each module re-derives it.
let mutable private standing: Map<string, Layout.Shape.Standing> = Map.empty

let private runTick () =
    let phases = ResizeArray<string * float>()

    let timed name (f: unit -> 'a) : 'a =
        let start = clock ()
        let result = f ()
        phases.Add(name, clock () - start)
        result

    let state =
        { timed "memory" Wire.readState with
            Layout = standing
        }

    let world = timed "world" World.ofGame
    let atlas = timed "atlas" (fun () -> Atlas.ofWorld world)

    let result =
        Tick.run
            clock
            modules
            {
                World = world
                Atlas = atlas
                Declarations = Wire.readDeclarations ()
            }
            state

    for report in result.Reports do
        phases.Add(report.Module, report.Cpu)

        match report.Outcome with
        | Tick.Faulted message -> recordFault report.Module message
        | Tick.Ran
        | Tick.Off -> ()

    standing <- result.State.Layout
    let refusals = timed "execute" (fun () -> Executor.run result.Intents)
    timed "write" (fun () -> Wire.writeState result.State)

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

/// Exported as `loop` on the bundled `main` module, which the engine calls.
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
