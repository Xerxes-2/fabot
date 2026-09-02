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

/// Screeps `ERR_NOT_IN_RANGE`.
let private errNotInRange = -9

/// Act on a target if the creep is adjacent, otherwise walk toward it.
let private actOrApproach (creep: ICreep) (target: obj) (act: obj -> int) =
    if act target = errNotInRange then
        creep.moveTo target |> ignore

let private execute (intent: Intent) =
    match intent with
    | SpawnCreep(spawnName, body, creepName) ->
        let spawn: ISpawn = Game.spawns?(spawnName)

        if not (isNull (box spawn)) then
            let code = spawn.spawnCreep (body |> List.map partName |> List.toArray, creepName)

            if code <> 0 then
                JS.console.log ($"spawnCreep {creepName} at {spawnName} failed: {code}")
    | HarvestSource(creepName, sourceId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let source = Game.getObjectById sourceId

        if not (isNull (box creep)) && not (isNull source) then
            actOrApproach creep source (fun t -> creep.harvest t)
    | TransferEnergyToStructure(creepName, structureId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let structure = Game.getObjectById structureId

        if not (isNull (box creep)) && not (isNull structure) then
            actOrApproach creep structure (fun t -> creep.transfer (t, "energy"))
    | BuildSite(creepName, siteId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let site = Game.getObjectById siteId

        if not (isNull (box creep)) && not (isNull site) then
            actOrApproach creep site (fun t -> creep.build t)
    | UpgradeController(creepName, controllerId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let controller = Game.getObjectById controllerId

        if not (isNull (box creep)) && not (isNull controller) then
            actOrApproach creep controller (fun t -> creep.upgradeController t)

let run (intents: Intent list) = intents |> List.iter execute
