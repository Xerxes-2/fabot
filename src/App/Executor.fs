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

let private structureName =
    function
    | Extension -> structureExtension

let private execute (intent: Intent) =
    match intent with
    | SpawnCreep(spawnName, body, creepName) ->
        let spawn: ISpawn = Game.spawns?(spawnName)

        if not (isNull (box spawn)) then
            let code = spawn.spawnCreep (body |> List.map partName |> List.toArray, creepName)

            if code <> 0 then
                JS.console.log ($"spawnCreep {creepName} at {spawnName} failed: {code}")
    | PlaceConstructionSite(roomName, pos, kind) ->
        let room: IRoom = Game.rooms?(roomName)

        if not (isNull (box room)) then
            let code = room.createConstructionSite (pos.X, pos.Y, structureName kind)

            if code <> 0 then
                JS.console.log (
                    $"createConstructionSite {structureName kind} at {roomName} ({pos.X},{pos.Y}) failed: {code}"
                )
    | HarvestSource(creepName, sourceId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let source = Game.getObjectById sourceId

        if not (isNull (box creep)) && not (isNull source) then
            creep.harvest source |> ignore
    | TransferEnergyToStructure(creepName, structureId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let structure = Game.getObjectById structureId

        if not (isNull (box creep)) && not (isNull structure) then
            creep.transfer (structure, "energy") |> ignore
    | BuildSite(creepName, siteId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let site = Game.getObjectById siteId

        if not (isNull (box creep)) && not (isNull site) then
            creep.build site |> ignore
    | UpgradeController(creepName, controllerId) ->
        let creep: ICreep = Game.creeps?(creepName)
        let controller = Game.getObjectById controllerId

        if not (isNull (box creep)) && not (isNull controller) then
            creep.upgradeController controller |> ignore
    | MoveCreep(creepName, direction) ->
        let creep: ICreep = Game.creeps?(creepName)

        if not (isNull (box creep)) then
            creep.move (directionCode direction) |> ignore
    | SayCreep(creepName, message) ->
        let creep: ICreep = Game.creeps?(creepName)

        if not (isNull (box creep)) then
            creep.say message |> ignore

let run (intents: Intent list) = intents |> List.iter execute
