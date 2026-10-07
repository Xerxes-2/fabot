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
    /// The engine call itself threw (#628): this Intent is lost, the rest of
    /// the plan runs.
    | Threw of error: exn

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
    | PlaceSpawnSite(tile, name) ->
        withActor (Game.rooms?(tile.Room): IRoom) (fun room ->
            createSpawnSite room tile.X tile.Y name)
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
    | DismantleStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.dismantle t)
    | UpgradeController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.upgradeController t)
    // A declared outpost controller is in the projection without vision, so
    // `getObjectById` can answer null for it: the shared guard's ActorMissing.
    | ReserveController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.reserveController t)
    | AttackController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.attackController t)
    // The one act with a precondition Core has no model of: no GCL level left
    // answers ERR_GCL_NOT_ENOUGH, which reaches the next decide as a refusal
    // (`refusals`) and the breach log as a `RefusalStorm`.
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
    | DropEnergy creepName -> withCreep creepName (fun c -> c.drop (resourceName Energy))
    // The hostile arrives by id; one that died between decision and replay is
    // ActorMissing. The heal names one of ours twice, through `Game.creeps`.
    | AttackCreep(creepName, hostileId) ->
        withCreepTarget creepName hostileId (fun c t -> c.attack t)
    | AttackStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.attack t)
    | RangedAttackCreep(creepName, hostileId) ->
        withCreepTarget creepName hostileId (fun c t -> c.rangedAttack t)
    | RangedAttackStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.rangedAttack t)
    | RangedMassAttack creepName -> withCreep creepName (fun c -> c.rangedMassAttack ())
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
    | RepairWithTower(towerId, structureId) ->
        withTarget structureId (fun target ->
            withActor (Game.getObjectById towerId :?> ITower) (fun tower -> tower.repair target))
    | SetRampartPublic(rampartId, isPublic) ->
        withActor (Game.getObjectById rampartId) (fun rampart -> setPublic rampart isPublic)
    | DestroyStructure structureId -> withActor (Game.getObjectById structureId) destroyStructure
    | RemoveSite siteId -> withActor (Game.getObjectById siteId) removeSite
    | ObserveRoom(observerId, room) ->
        withActor (Game.getObjectById observerId) (fun observer -> observeRoom observer room)
    | RunReaction(labId, reagentLab, otherReagentLab) ->
        withTarget reagentLab (fun first ->
            withTarget otherReagentLab (fun second ->
                withActor (Game.getObjectById labId) (fun lab -> runReaction lab first second)))
    | SendFromTerminal(terminalId, resource, amount, destination) ->
        withActor (Game.getObjectById terminalId :?> ITerminal) (fun terminal ->
            terminal.send (resourceName resource, amount, destination))

let private replay (logged: bool) (plan: Fabot.Core.IntentPlan.Plan) : (Intent * Outcome) list =
    Fabot.Core.IntentPlan.intents plan
    |> List.map (fun intent ->
        // One Intent's throw costs that Intent: V8 optimises a function
        // holding a `try`, so the guard costs nothing on the calls that return.
        let outcome =
            try
                execute intent
            with error ->
                Threw error

        if logged then
            match outcome with
            | Ok -> ()
            | Failed code -> JS.console.log $"%A{intent} failed: {code}"
            | ActorMissing -> JS.console.log $"%A{intent}: actor or target not found"
            // The fault ring's writer logs a throw (`Main.recordFaults`).
            | Threw _ -> ()

        intent, outcome)

/// Replay every Intent and answer back what the engine said. Failures are
/// logged here, once; the outcome list is what `Main.loop` counts accepted
/// intents and folds refusals (`refusals`) off.
let run (plan: Fabot.Core.IntentPlan.Plan) : (Intent * Outcome) list = replay true plan

/// The refusals a light tick's replay expects (#529): it re-issues every act
/// whose target stands for the engine to refuse out of range
/// (ERR_NOT_IN_RANGE), full (ERR_FULL), empty (ERR_NOT_ENOUGH_*) or tired
/// (ERR_TIRED).
let private expected code =
    code = -9 || code = -8 || code = -6 || code = -11

/// `run` for a light tick: an expected refusal is not news, so only the
/// rest are logged, on one line for the tick.
let runQuiet (plan: Fabot.Core.IntentPlan.Plan) : (Intent * Outcome) list =
    let outcomes = replay false plan

    let unexpected =
        outcomes
        |> List.choose (fun (intent, outcome) ->
            match outcome with
            | Failed code when not (expected code) -> Some $"%A{intent} failed: {code}"
            | ActorMissing -> Some $"%A{intent}: actor or target not found"
            | Threw _
            | Ok
            | Failed _ -> None)

    if not (List.isEmpty unexpected) then
        let shown = String.concat "; " unexpected
        JS.console.log $"light tick, {List.length unexpected} unexpected: {shown}"

    outcomes

/// The actor a refused Intent is filed under (#629), and whether it is a
/// body: a body's act under its name, a spawn's under its name, a site's
/// under its tile, any other structure's under its id. None for a body's
/// `say`, a glyph no decision stands on.
let private actorOf (intent: Intent) : (string * bool) option =
    let structure (id: string) = Some(id, false)

    let site (tile: RoomPos) =
        Some($"{tile.Room}:{tile.X},{tile.Y}", false)

    match intent with
    | SayCreep _ -> None
    | SpawnCreep(spawnName, _, _) -> structure spawnName
    | PlaceConstructionSite(tile, _) -> site tile
    | PlaceSpawnSite(tile, _) -> site tile
    | ActivateSafeMode id
    | FireTower(id, _)
    | HealWithTower(id, _)
    | RepairWithTower(id, _)
    | SetRampartPublic(id, _)
    | DestroyStructure id
    | RemoveSite id
    | ObserveRoom(id, _)
    | RunReaction(id, _, _)
    | SendFromTerminal(id, _, _, _) -> structure id
    | _ -> Fabot.Core.IntentPlan.actor intent |> Option.map (fun name -> name, true)

/// Whether an outcome is a refusal fed back to the next decide (#629): every
/// `Failed` code but the four a plan expects of any act (`expected`: the
/// walk, the store and the fatigue it already models). An actor or target
/// gone is the view's lag, and a throw the fault ring's.
let private refusedWith outcome =
    match outcome with
    | Failed code when not (expected code) -> Some code
    | Failed _
    | Ok
    | ActorMissing
    | Threw _ -> None

/// The engine's refusals among a replay's outcomes (`refusedWith`), each one
/// refusal (`Refusal.one`) of its actor: a body's holding `taskOf` its
/// Task, a structure's filed under `issuedBy`, the colony whose plan made
/// the act, asked of a refused act alone.
let refusals
    (taskOf: string -> string)
    (issuedBy: Intent -> string)
    (tick: int)
    (outcomes: (Intent * Outcome) list)
    : Refusal list =
    outcomes
    |> List.choose (fun (intent, outcome) ->
        match refusedWith outcome, actorOf intent with
        | Some code, Some(actor, true) -> Some(Refusal.one actor true "" (taskOf actor) code tick)
        | Some code, Some(actor, false) ->
            Some(Refusal.one actor false (issuedBy intent) "" code tick)
        | _ -> None)

/// Each spawn a replay's `SpawnCreep` reached, and whether the engine
/// refused that cast (`refusedWith`): what a spawn's last cast was.
let casts (outcomes: (Intent * Outcome) list) : (string * bool) list =
    outcomes
    |> List.choose (fun (intent, outcome) ->
        match intent with
        | SpawnCreep(spawn, _, _) -> Some(spawn, Option.isSome (refusedWith outcome))
        | _ -> None)
