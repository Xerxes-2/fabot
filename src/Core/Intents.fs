/// What `decide` asks the game to do, typed by the actor that does it
/// (`ARCHITECTURE.md` §1). `Executor` turns each into one API call and
/// reports the engine's answer.
module Fabot.Core.Intents

type CreepAct =
    /// TOP = 1, clockwise to TOP_LEFT = 8.
    | Move of direction: int
    | Say of message: string

type SpawnAct = SpawnCreep of body: string list * name: string

type Intent =
    | Creep of name: string * CreepAct
    | Spawn of id: string * SpawnAct

/// The actor an intent names, for a refusal's report.
let actorOf =
    function
    | Creep(name, _) -> name
    | Spawn(id, _) -> id
