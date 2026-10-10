/// Turns intents into API calls and reports every refusal. It never decides.
module Fabot.Executor

open Fabot.Bindings
open Fabot.Core.Intents

/// An intent the engine answered with a non-OK code, or whose actor is gone.
type Refusal =
    {
        Actor: string
        Act: string
        Code: int
    }

/// The engine's ERR_NOT_FOUND, used for an actor that no longer stands.
let private notFound = -5

let private act (intent: Intent) : string * int =
    match intent with
    | Creep(name, act) ->
        match objectItem<ICreep> Game.creeps name with
        | creep when isNull (box creep) -> "creep", notFound
        | creep ->
            match act with
            | Move direction -> "move", creep.move direction
            | Say message -> "say", creep.say message
    | Spawn(id, SpawnCreep(body, name)) ->
        match Game.getObjectById id with
        | spawn when isNull spawn -> "spawn", notFound
        | spawn -> "spawn", (unbox<ISpawn> spawn).spawnCreep (Array.ofList body, name)

let run (intents: Intent list) : Refusal list =
    intents
    |> List.choose (fun intent ->
        let name, code = act intent

        if code = 0 then
            None
        else
            Some
                {
                    Actor = actorOf intent
                    Act = name
                    Code = code
                })
