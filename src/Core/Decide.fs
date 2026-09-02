module Fabot.Core.Decide

open Fabot.Core.Types

/// Colony never plans below this many living creeps. Two keep the
/// harvest/refill loop running while one is in transit or being replaced.
let minWorkforce = 2

/// The MVP worker body and its energy cost.
let workerBody = [ Work; Carry; Move ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50)

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Refill structureId -> $"refill:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"

/// Planner: rebuild this tick's full Task pool from the Snapshot. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (snapshot: Snapshot) : Task list =
    let harvests = snapshot.Sources |> List.map (fun s -> Harvest s.Id)

    let refills =
        snapshot.Refillables
        |> List.filter (fun r -> r.FreeCapacity > 0)
        |> List.map (fun r -> Refill r.Id)

    let upgrades =
        snapshot.Controller |> Option.toList |> List.map (fun c -> Upgrade c.Id)

    harvests @ refills @ upgrades

/// Pre-Task bootstrap step: spawn Intents needed to keep the workforce at
/// minimum. Spawning is a colony-level need, not a Task creeps get matched to,
/// so it sits beside the Planner/Matcher pipeline rather than inside it.
let private planSpawns (snapshot: Snapshot) : Intent list =
    let deficit = minWorkforce - List.length snapshot.Creeps

    if deficit <= 0 then
        []
    else
        snapshot.Spawns
        |> List.filter (fun s -> not s.IsSpawning && s.EnergyAvailable >= bodyCost workerBody)
        |> List.truncate deficit
        |> List.map (fun s -> SpawnCreep(s.Name, workerBody, $"worker-{snapshot.Time}-{s.Name}"))

/// Whether a creep can usefully work this Task right now. A full creep is
/// done harvesting; an empty creep has nothing to deliver.
let private applicable (creep: CreepInfo) task =
    match task with
    | Harvest _ -> creep.FreeCapacity > 0
    | Refill _ -> creep.Energy > 0
    | Upgrade _ -> creep.Energy > 0

let private intentFor (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> HarvestSource(creep.Name, sourceId)
    | Refill structureId -> TransferEnergyToStructure(creep.Name, structureId)
    | Upgrade controllerId -> UpgradeController(creep.Name, controllerId)

/// Matching tier between applicable tasks (lower wins): feeding the economy
/// (Harvest, Refill) outranks sinking surplus into the controller (Upgrade).
let private rank =
    function
    | Harvest _ -> 0
    | Refill _ -> 0
    | Upgrade _ -> 1

/// Matcher: keep still-valid assignments (anti-thrash), greedily assign the
/// rest, and emit one Intent per assigned creep.
let private matchCreeps
    (snapshot: Snapshot)
    (tasks: Task list)
    (assignments: Assignments)
    : Intent list * Assignments =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    let kept =
        assignments
        |> Map.filter (fun name tid ->
            match
                snapshot.Creeps |> List.tryFind (fun c -> c.Name = name), Map.tryFind tid byId
            with
            | Some creep, Some task -> applicable creep task
            | _ -> false)

    let assignOne acc (creep: CreepInfo) =
        if Map.containsKey creep.Name acc then
            acc
        else
            let load tid =
                acc |> Map.filter (fun _ assigned -> assigned = tid) |> Map.count

            match tasks |> List.filter (applicable creep) with
            | [] -> acc
            | candidates ->
                let task = candidates |> List.minBy (fun t -> rank t, load (taskId t))
                Map.add creep.Name (taskId task) acc

    let final = snapshot.Creeps |> List.fold assignOne kept

    let intents =
        snapshot.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name final
            |> Option.bind (fun tid -> Map.tryFind tid byId)
            |> Option.map (intentFor creep))

    intents, final

/// The single seam: Snapshot in, Intents plus next tick's Assignments out.
let decide (snapshot: Snapshot) (assignments: Assignments) : Intent list * Assignments =
    let spawnIntents = planSpawns snapshot
    let creepIntents, next = matchCreeps snapshot (planTasks snapshot) assignments
    spawnIntents @ creepIntents, next
