/// `Executor` turns each intent into one API call (`ARCHITECTURE.md` §1).
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

type RoomAct = CreateSite of x: int * y: int * kind: World.Kind

type LinkAct = TransferEnergy of target: string * amount: int

type TowerAct =
    | Attack of target: string
    | Heal of target: string

type Intent =
    | Creep of name: string * CreepAct
    | Spawn of id: string * SpawnAct
    | Room of name: string * RoomAct
    | Tower of id: string * TowerAct
    | Link of id: string * LinkAct

let actorOf =
    function
    | Creep(name, _) -> name
    | Spawn(id, _) -> id
    | Room(name, _) -> name
    | Tower(id, _) -> id
    | Link(id, _) -> id
