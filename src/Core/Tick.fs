/// A throw in one module costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in to keep `decide` deterministic.
module Fabot.Core.Tick

open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State

type Declarations =
    {
        /// The creep lifetimes of tower fire a colony's reserve holds.
        ReserveLifetimes: float
    }

let undeclared = { ReserveLifetimes = 2.0 }

[<NoEquality; NoComparison>]
type Context =
    {
        World: World
        Atlas: Atlas
        Declarations: Declarations
    }

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
