// The only layer allowed to call game methods: turns Intents into API calls.
module Fabot.Executor

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types

let private partName =
    function
    | Work -> "work"
    | Carry -> "carry"
    | Move -> "move"

let private execute (intent: Intent) =
    match intent with
    | SpawnCreep (spawnName, body, creepName) ->
        let spawn: ISpawn = Game.spawns?(spawnName)
        if not (isNull (box spawn)) then
            let code = spawn.spawnCreep (body |> List.map partName |> List.toArray, creepName)
            if code <> 0 then
                JS.console.log ($"spawnCreep {creepName} at {spawnName} failed: {code}")

let run (intents: Intent list) = intents |> List.iter execute
