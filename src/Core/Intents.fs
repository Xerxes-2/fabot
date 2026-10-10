/// What `decide` asks the game to do, typed by the actor that does it
/// (`ARCHITECTURE.md` §1). `Executor` turns each into one API call and
/// reports the engine's answer.
module Fabot.Core.Intents

open Fabot.Core.Vocabulary

type CreepAct =
    /// TOP = 1, clockwise to TOP_LEFT = 8.
    | Move of direction: int
    | Say of message: string
    | Harvest of target: string
    | Transfer of target: string * resource: Resource
    | Withdraw of target: string * resource: Resource
    | Pickup of target: string
    | Build of site: string
    | Repair of target: string
    | Upgrade of controller: string
    | Reserve of controller: string
    | ClaimController of controller: string

type SpawnAct = SpawnCreep of body: BodyPart list * name: string

type Intent =
    | Creep of name: string * CreepAct
    | Spawn of id: string * SpawnAct

/// The actor an intent names, for a refusal's report.
let actorOf =
    function
    | Creep(name, _) -> name
    | Spawn(id, _) -> id
