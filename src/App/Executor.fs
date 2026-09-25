// The only layer allowed to call game methods: turns Intents into API calls.
module Fabot.Executor

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types

/// What the engine said about one Intent's replay. Engine result codes are
/// App vocabulary; Core never reads them.
type Outcome =
    | Ok
    | Failed of code: int
    | ActorMissing

/// A placement Intent's kind as `createConstructionSite` spells it, through
/// the Core's one kind-name table so no case drifts from the projection's.
let private structureName = builtKindOfPlaceable >> builtKindName

let private outcomeOf code = if code = 0 then Ok else Failed code

// The null-guard written once. Actors and targets come from this tick's
// colony view, so a missing one is an upstream bug worth reporting, never a
// routine skip.
let private withActor (actor: 'a) (act: 'a -> int) : Outcome =
    if isNull (box actor) then
        ActorMissing
    else
        outcomeOf (act actor)

let private withCreep (name: string) (act: ICreep -> int) : Outcome =
    withActor (Game.creeps?(name): ICreep) act

let private withTarget (targetId: string) (act: obj -> Outcome) : Outcome =
    let target = Game.getObjectById targetId

    if isNull target then ActorMissing else act target

let private withCreepTarget
    (name: string)
    (targetId: string)
    (act: ICreep -> obj -> int)
    : Outcome =
    withTarget targetId (fun target -> withCreep name (fun creep -> act creep target))

/// A creep of ours named as a target, through `Game.creeps` rather than by id.
let private withOurCreep (targetName: string) (act: ICreep -> Outcome) : Outcome =
    let target: ICreep = Game.creeps?(targetName)

    if isNull (box target) then ActorMissing else act target

/// The same pair where the target is a creep of ours.
let private withOurCreepTarget
    (name: string)
    (targetName: string)
    (act: ICreep -> ICreep -> int)
    : Outcome =
    withOurCreep targetName (fun target -> withCreep name (fun creep -> act creep target))

let private execute (intent: Intent) : Outcome =
    match intent with
    | SpawnCreep(spawnName, body, creepName) ->
        withActor (Game.spawns?(spawnName): ISpawn) (fun spawn ->
            spawn.spawnCreep (body |> List.map partName |> List.toArray, creepName))
    | PlaceConstructionSite(tile, kind) ->
        withActor (Game.rooms?(tile.Room): IRoom) (fun room ->
            room.createConstructionSite (tile.X, tile.Y, structureName kind))
    | HarvestSource(creepName, sourceId) ->
        withCreepTarget creepName sourceId (fun c t -> c.harvest t)
    | TransferEnergyToStructure(creepName, structureId, resource) ->
        withCreepTarget creepName structureId (fun c t -> c.transfer (t, resourceName resource))
    // `None` is the engine's default (as much as the body holds), spelled by
    // the two-argument form.
    | WithdrawFromStore(creepName, storeId, resource, amount) ->
        withCreepTarget creepName storeId (fun c t ->
            match amount with
            | None -> c.withdraw (t, resourceName resource)
            | Some units -> withdrawAmount c t (resourceName resource) units)
    | BuildSite(creepName, siteId) -> withCreepTarget creepName siteId (fun c t -> c.build t)
    // The text rides the Intent (`Colony.signature` decides it); a sign refused
    // for range is offered again the next time anybody passes.
    | SignController(creepName, controllerId, text) ->
        withCreepTarget creepName controllerId (fun c t -> c.signController (t, text))
    | RepairStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.repair t)
    | UpgradeController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.upgradeController t)
    // A declared outpost controller is in the projection without vision, so
    // `getObjectById` can answer null for it: the shared guard's ActorMissing.
    | ReserveController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.reserveController t)
    // The one act with a precondition Core has no model of: no GCL level left
    // answers ERR_GCL_NOT_ENOUGH, the Task is pooled again next tick, and this
    // can repeat forever with the log line as the only place a human sees it.
    | ClaimController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.claimController t)
    // The reactor is the errand's declared id, in the projection without
    // vision, so `getObjectById` answers null until the body arrives: the
    // shared guard's ActorMissing. The engine checks a live CLAIM part and
    // Chebyshev 1, and the Emitter gates on both.
    | ClaimReactor(creepName, reactorId) ->
        withCreepTarget creepName reactorId (fun c t -> c.claimReactor t)
    | PickupPile(creepName, resourceId) ->
        withCreepTarget creepName resourceId (fun c t -> c.pickup t)
    // The hostile arrives by id; one that died between decision and replay is
    // ActorMissing. The heal names one of ours twice, through `Game.creeps`.
    | AttackCreep(creepName, hostileId) ->
        withCreepTarget creepName hostileId (fun c t -> c.attack t)
    | RangedAttackCreep(creepName, hostileId) ->
        withCreepTarget creepName hostileId (fun c t -> c.rangedAttack t)
    | HealCreep(creepName, targetName) ->
        withOurCreepTarget creepName targetName (fun c t -> c.heal (box t))
    | RangedHealCreep(creepName, targetName) ->
        withOurCreepTarget creepName targetName (fun c t -> c.rangedHeal (box t))
    | MoveCreep(creepName, direction) ->
        withCreep creepName (fun c -> c.move (directionCode direction))
    | SayCreep(creepName, message) -> withCreep creepName (fun c -> c.say message)
    | ActivateSafeMode controllerId ->
        withActor (Game.getObjectById controllerId :?> IController) (fun controller ->
            controller.activateSafeMode ())
    | FireTower(towerId, hostileId) ->
        withTarget hostileId (fun target ->
            withActor (Game.getObjectById towerId :?> ITower) (fun tower -> tower.attack target))
    | HealWithTower(towerId, targetName) ->
        withOurCreep targetName (fun target ->
            withActor (Game.getObjectById towerId :?> ITower) (fun tower ->
                tower.heal (box target)))
    | SendFromTerminal(terminalId, resource, amount, destination) ->
        withActor (Game.getObjectById terminalId :?> ITerminal) (fun terminal ->
            terminal.send (resourceName resource, amount, destination))

/// Replay every Intent and answer back what the engine said. Failures are
/// logged here, once; the outcome list is what `Main.loop` counts accepted
/// intents off.
let run (plan: Fabot.Core.IntentPlan.Plan) : (Intent * Outcome) list =
    Fabot.Core.IntentPlan.intents plan
    |> List.map (fun intent ->
        let outcome = execute intent

        match outcome with
        | Ok -> ()
        | Failed code -> JS.console.log $"%A{intent} failed: {code}"
        | ActorMissing -> JS.console.log $"%A{intent}: actor or target not found"

        intent, outcome)
