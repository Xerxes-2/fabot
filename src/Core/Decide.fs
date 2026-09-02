module Fabot.Core.Decide

open Fabot.Core.Types

/// Colony never plans below this many living creeps.
let minWorkforce = 1

/// The MVP worker body and its energy cost.
let workerBody = [ Work; Carry; Move ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50)

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

/// Matcher's anti-thrash half: keep only assignments of creeps still alive.
/// (Task matching itself arrives with the first creep Tasks.)
let private matchCreeps (snapshot: Snapshot) (assignments: Assignments) : Intent list * Assignments =
    let alive = snapshot.Creeps |> List.map (fun c -> c.Name) |> Set.ofList
    let surviving = assignments |> Map.filter (fun name _ -> Set.contains name alive)
    [], surviving

/// The single seam: Snapshot in, Intents plus surviving Assignments out.
let decide (snapshot: Snapshot) (assignments: Assignments) : Intent list * Assignments =
    let spawnIntents = planSpawns snapshot
    let creepIntents, surviving = matchCreeps snapshot assignments
    spawnIntents @ creepIntents, surviving
