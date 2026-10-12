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
    | AttackCreep of target: string
    | RangedAttackCreep of target: string
    | RangedMassAttack
    | HealCreep of target: string
    | RangedHealCreep of target: string

type SpawnAct = SpawnCreep of body: BodyPart list * name: string

type RoomAct = CreateSite of x: int * y: int * kind: World.Kind

type LinkAct = TransferEnergy of target: string * amount: int

type ControllerAct = ActivateSafeMode

type TowerAct =
    | Attack of target: string
    | Heal of target: string

/// Where an ask stands in its room's queue (§6.5): a floor is never displaced,
/// a fight's goes before the economy's and holds it while it waits on the bank.
[<RequireQualifiedAccess>]
type Rank =
    | Floor
    | Fight
    | Economy

type Ask =
    {
        Body: BodyPart list
        Name: string
        Rank: Rank
    }

type Intent =
    | Creep of name: string * CreepAct
    | Spawn of id: string * SpawnAct
    | Room of name: string * RoomAct
    | Tower of id: string * TowerAct
    | Controller of id: string * ControllerAct
    | Link of id: string * LinkAct
    /// Tick serves these from the room's queue (`Tick.queue`); none reaches `Executor`.
    | Queue of room: string * Ask
    /// Tick arbitrates every module's wishes in a room as one (`Tick.arbitrate`).
    | Wish of room: string * Move.Wish

let actorOf =
    function
    | Creep(name, _) -> name
    | Spawn(id, _) -> id
    | Room(name, _) -> name
    | Tower(id, _) -> id
    | Controller(id, _) -> id
    | Link(id, _) -> id
    | Queue(room, _) -> room
    | Wish(_, wish) -> wish.Creep
