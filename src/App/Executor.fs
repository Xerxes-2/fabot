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

/// A placement Intent's kind as `createConstructionSite` spells it: the
/// placeable kind widened to its built kind, then spelled by the Core's
/// one kind-name table (#75) — as a spawned body's parts are. Nothing is
/// restated here, so no case can drift out of step with the projection's.
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
    | TransferEnergyToStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.transfer (t, "energy"))
    | WithdrawEnergyFromStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.withdraw (t, "energy"))
    | BuildSite(creepName, siteId) -> withCreepTarget creepName siteId (fun c t -> c.build t)
    | RepairStructure(creepName, structureId) ->
        withCreepTarget creepName structureId (fun c t -> c.repair t)
    | UpgradeController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.upgradeController t)
    // The outpost controller is a target like any other: a declared one is
    // in the projection without vision (ADR 0041), so the id can name an
    // object this tick's `getObjectById` cannot answer for, and that is
    // exactly the ActorMissing the shared guard already reports.
    | ReserveController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.reserveController t)
    // The claim (ADR 0047), the one act with a precondition the decision
    // layer has no model of: a controller the account has no GCL level
    // left for answers ERR_GCL_NOT_ENOUGH, and the view carries no GCL
    // fact for Core to have planned around it. The code is logged here and
    // read nowhere else, as every result code is (`Outcome` above) — the
    // room stays unowned, so the Task is pooled again next tick and the
    // creep walks back to it, which is what every other failed act does
    // too. The difference is that this one can repeat forever, and the log
    // line is the only place a human sees it.
    | ClaimController(creepName, controllerId) ->
        withCreepTarget creepName controllerId (fun c t -> c.claimController t)
    | PickupEnergy(creepName, resourceId) ->
        withCreepTarget creepName resourceId (fun c t -> c.pickup t)
    | MoveCreep(creepName, direction) ->
        withCreep creepName (fun c -> c.move (directionCode direction))
    | SayCreep(creepName, message) -> withCreep creepName (fun c -> c.say message)
    | ActivateSafeMode controllerId ->
        withActor (Game.getObjectById controllerId :?> IController) (fun controller ->
            controller.activateSafeMode ())
    | FireTower(towerId, hostileId) ->
        withTarget hostileId (fun target ->
            withActor (Game.getObjectById towerId :?> ITower) (fun tower -> tower.attack target))

/// Replay every Intent and answer back what the engine said. Failures are
/// logged here, once and uniformly; the outcome list is the seam `Main.loop`
/// counts the engine's accepted intents off (#170), and a future sim harness
/// reads.
let run (intents: Intent list) : (Intent * Outcome) list =
    intents
    |> List.map (fun intent ->
        let outcome = execute intent

        match outcome with
        | Ok -> ()
        | Failed code -> JS.console.log $"%A{intent} failed: {code}"
        | ActorMissing -> JS.console.log $"%A{intent}: actor or target not found"

        intent, outcome)
