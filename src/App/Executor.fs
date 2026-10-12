module Fabot.Executor

open Fabot.Bindings
open Fabot.Core.Vocabulary
open Fabot.Core.Intents

type Refusal =
    {
        Actor: string
        Act: string
        Code: int
    }

let private notFound = -5

let private act (intent: Intent) : string * int =
    match intent with
    | Creep(name, act) ->
        match objectItem<ICreep> Game.creeps name with
        | creep when isNull (box creep) -> "creep", notFound
        | creep ->
            // Refused here as the engine would refuse it.
            let on (verb: string) (id: string) (f: obj -> int) =
                match Game.getObjectById id with
                | target when isNull target -> verb, notFound
                | target -> verb, f target

            match act with
            | Move direction -> "move", creep.move direction
            | Say message -> "say", creep.say message
            | Harvest id -> on "harvest" id creep.harvest
            | Transfer(id, r) -> on "transfer" id (fun t -> creep.transfer (t, resourceName r))
            | Withdraw(id, r) -> on "withdraw" id (fun t -> creep.withdraw (t, resourceName r))
            | Pickup id -> on "pickup" id creep.pickup
            | Build id -> on "build" id creep.build
            | Repair id -> on "repair" id creep.repair
            | Upgrade id -> on "upgrade" id creep.upgradeController
            | Reserve id -> on "reserve" id creep.reserveController
            | Unreserve id -> on "unreserve" id creep.attackController
            | ClaimController id -> on "claim" id creep.claimController
            | AttackCreep id -> on "attack" id creep.attack
            | RangedAttackCreep id -> on "rangedAttack" id creep.rangedAttack
            | RangedMassAttack -> "rangedMassAttack", creep.rangedMassAttack ()
            | HealCreep id -> on "heal" id creep.heal
            | RangedHealCreep id -> on "rangedHeal" id creep.rangedHeal
    | Room(name, CreateSite(x, y, kind)) ->
        let verb = "site:" + Fabot.Core.World.Kind.name kind

        match objectItem<IRoom> Game.rooms name with
        | room when isNull (box room) -> verb, notFound
        | room -> verb, room.createConstructionSite (x, y, Fabot.Core.World.Kind.name kind)
    | Tower(id, act) ->
        match Game.getObjectById id with
        | tower when isNull tower -> "tower", notFound
        | tower ->
            let tower = unbox<ITower> tower

            let on (verb: string) (target: string) (f: obj -> int) =
                match Game.getObjectById target with
                | t when isNull t -> verb, notFound
                | t -> verb, f t

            match act with
            | Attack target -> on "attack" target tower.attack
            | Heal target -> on "heal" target tower.heal
    | Controller(id, ActivateSafeMode) ->
        match Game.getObjectById id with
        | controller when isNull controller -> "safeMode", notFound
        | controller -> "safeMode", (unbox<IController> controller).activateSafeMode ()
    | Link(id, TransferEnergy(target, amount)) ->
        match Game.getObjectById id, Game.getObjectById target with
        | link, _ when isNull link -> "link", notFound
        | _, t when isNull t -> "link", notFound
        | link, t -> "link", (unbox<ILink> link).transferEnergy (t, amount)
    | Queue _
    | Wish _ -> "unsettled", notFound
    | Spawn(id, SpawnCreep(body, name)) ->
        match Game.getObjectById id with
        | spawn when isNull spawn -> "spawn", notFound
        | spawn ->
            "spawn",
            (unbox<ISpawn> spawn).spawnCreep (body |> List.map partName |> Array.ofList, name)

/// One bad intent costs itself, never the rest of the tick's.
let threw = -100

let run (intents: Intent list) : Refusal list =
    intents
    |> List.choose (fun intent ->
        let name, code =
            try
                act intent
            with _ ->
                "threw", threw

        if code = 0 then
            None
        else
            Some
                {
                    Actor = actorOf intent
                    Act = name
                    Code = code
                })
