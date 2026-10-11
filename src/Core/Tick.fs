/// One tick over the modules, in order: each is timed, can be switched off,
/// and a throw in one costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in, so the decision stays
/// deterministic and the accounting is testable.
module Fabot.Core.Tick

open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State

/// What every module reads: the tick's facts and the geometry built from them
/// once (`ARCHITECTURE.md` §1).
[<NoEquality; NoComparison>]
type Context = { World: World; Atlas: Atlas }

type Module =
    {
        Name: string
        Run: Context -> State -> Intent list * State
    }

type Outcome =
    | Ran
    | Off
    | Faulted of message: string

type Report =
    {
        Module: string
        Cpu: float
        Outcome: Outcome
    }

type Result =
    {
        Intents: Intent list
        State: State
        Reports: Report list
    }

let run (clock: unit -> float) (modules: Module list) (context: Context) (state: State) : Result =
    let step (intents, state, reports) (m: Module) =
        if Set.contains m.Name state.Off then
            intents,
            state,
            {
                Module = m.Name
                Cpu = 0.0
                Outcome = Off
            }
            :: reports
        else
            let start = clock ()

            let produced, next, outcome =
                try
                    let produced, next = m.Run context state
                    produced, next, Ran
                with error ->
                    [], state, Faulted error.Message

            let report =
                {
                    Module = m.Name
                    Cpu = clock () - start
                    Outcome = outcome
                }

            List.rev produced @ intents, next, report :: reports

    let intents, state, reports = List.fold step ([], state, []) modules

    {
        Intents = List.rev intents
        State = state
        Reports = List.rev reports
    }
