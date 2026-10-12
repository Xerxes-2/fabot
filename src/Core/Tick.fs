/// A throw in one module costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in to keep `decide` deterministic.
module Fabot.Core.Tick

open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State

type Declared = { Id: string; Pos: Pos }

/// What is known of a room before it is seen.
type Outpost =
    {
        Room: string
        Controller: Declared
        Sources: Declared list
    }

type Declarations =
    {
        /// The creep lifetimes of tower fire a colony's reserve holds.
        ReserveLifetimes: float
        Outposts: Outpost list
    }

let undeclared =
    {
        ReserveLifetimes = 2.0
        Outposts = []
    }

[<NoEquality; NoComparison>]
type Context =
    {
        World: World
        Atlas: Atlas
        Declarations: Declarations
        Sightings: Sightings
    }

/// The route search needs a bound; nothing is worked further than this.
let reachHops = 3

let colonies (world: World) : string[] =
    world.Rooms
    |> Array.filter (fun r ->
        r.Controller |> Option.exists (fun c -> c.Owner.IsOurs)
        && world.Spawns |> Array.exists (fun s -> s.Pos.Room = r.Name))
    |> Array.map (fun r -> r.Name)

/// The declared outposts a colony's spawn reaches and no hostile seen there
/// outlives (`ARCHITECTURE.md` §13, M1b): a stood-down room is no ground.
let worked (context: Context) : Outpost list =
    let homes = colonies context.World

    context.Declarations.Outposts
    |> List.filter (fun o ->
        Sightings.clear context.Sightings context.World.Time o.Room
        && homes
           |> Array.exists (fun home ->
               Option.isSome (Atlas.route context.Atlas Set.empty reachHops home o.Room)))

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
