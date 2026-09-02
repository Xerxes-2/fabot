module Fabot.Core.Decide

open Fabot.Core.Types

/// Colony never plans below this many living creeps. Two keep the
/// harvest/refill loop running while one is in transit or being replaced.
let minWorkforce = 2

/// The repeating unit worker bodies are built from.
let workerUnit = [ Work; Carry; Move ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50)

/// Largest affordable repetition of the worker unit within `capacity`,
/// never below one unit.
let workerBodyFor capacity =
    let units = max 1 (capacity / bodyCost workerUnit)
    List.replicate units workerUnit |> List.concat

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Refill structureId -> $"refill:{structureId}"
    | Build siteId -> $"build:{siteId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"

/// Planner: rebuild this tick's full Task pool from the Snapshot. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (snapshot: Snapshot) : Task list =
    let harvests = snapshot.Sources |> List.map (fun s -> Harvest s.Id)

    let refills =
        snapshot.Refillables
        |> List.filter (fun r -> r.FreeCapacity > 0)
        |> List.map (fun r -> Refill r.Id)

    let builds = snapshot.ConstructionSites |> List.map (fun site -> Build site.Id)

    let upgrades =
        snapshot.Controller |> Option.toList |> List.map (fun c -> Upgrade c.Id)

    harvests @ refills @ builds @ upgrades

/// Pre-Task bootstrap step: spawn Intents needed to keep the workforce at
/// minimum. Spawning is a colony-level need, not a Task creeps get matched to,
/// so it sits beside the Planner/Matcher pipeline rather than inside it.
let private planSpawns (snapshot: Snapshot) : Intent list =
    let deficit = minWorkforce - List.length snapshot.Creeps

    // Disaster fallback: an empty colony can never refill extensions, so
    // waiting for full capacity would wait forever — spawn a minimal unit
    // from whatever energy is banked right now.
    let bodyFor (s: SpawnInfo) =
        if List.isEmpty snapshot.Creeps then
            if s.EnergyAvailable >= bodyCost workerUnit then
                Some workerUnit
            else
                None
        elif s.EnergyAvailable >= s.EnergyCapacity then
            Some(workerBodyFor s.EnergyCapacity)
        else
            None

    if deficit <= 0 then
        []
    else
        snapshot.Spawns
        |> List.filter (fun s -> not s.IsSpawning)
        |> List.choose (fun s ->
            bodyFor s
            |> Option.map (fun body ->
                SpawnCreep(s.Name, body, $"worker-{snapshot.Time}-{s.Name}")))
        |> List.truncate deficit

/// Extensions the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "extension").
let private extensionAllowance level =
    match level with
    | 0
    | 1 -> 0
    | 2 -> 5
    | 3 -> 10
    | 4 -> 20
    | 5 -> 30
    | 6 -> 40
    | 7 -> 50
    | _ -> 60

/// Screeps range: Chebyshev distance between two tiles.
let private range a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// Colony-level planning step beside the Planner/Matcher pipeline: fill the
/// controller level's extension allowance with construction sites on a
/// checkerboard around the spawn, nearest tiles first. Sites are not creep
/// work, so this emits Intents directly rather than Tasks.
let private planConstructionSites (snapshot: Snapshot) : Intent list =
    match snapshot.Placement, snapshot.Controller with
    | Some plan, Some controller ->
        let missing =
            extensionAllowance controller.Level
            - plan.BuiltExtensions
            - plan.PendingExtensions

        if missing <= 0 then
            []
        else
            // Same checkerboard colour as the spawn: extensions cluster on the
            // spawn's colour, leaving the other colour free for movement.
            let parity = (plan.SpawnPos.X + plan.SpawnPos.Y) % 2

            plan.Walkable
            |> Set.toList
            |> List.filter (fun tile ->
                (tile.X + tile.Y) % 2 = parity && not (Set.contains tile plan.Occupied))
            |> List.sortBy (fun tile -> range tile plan.SpawnPos, tile.X, tile.Y)
            |> List.truncate missing
            |> List.map (fun tile -> PlaceConstructionSite(plan.RoomName, tile, Extension))
    | _ -> []

/// Whether a creep can usefully work this Task right now. A full creep is
/// done harvesting; an empty creep has nothing to deliver.
let private applicable (creep: CreepInfo) task =
    match task with
    | Harvest _ -> creep.FreeCapacity > 0
    | Refill _
    | Build _
    | Upgrade _ -> creep.Energy > 0

let private intentFor (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> HarvestSource(creep.Name, sourceId)
    | Refill structureId -> TransferEnergyToStructure(creep.Name, structureId)
    | Build siteId -> BuildSite(creep.Name, siteId)
    | Upgrade controllerId -> UpgradeController(creep.Name, controllerId)

/// Matching tier between applicable tasks (lower wins): feeding the economy
/// (Harvest, Refill) outranks sinking surplus into construction (Build) or
/// the controller (Upgrade).
let private rank =
    function
    | Harvest _ -> 0
    | Refill _ -> 0
    | Build _ -> 1
    | Upgrade _ -> 1

let private neighbours pos =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if (dx, dy) <> (0, 0) then
                    { X = pos.X + dx; Y = pos.Y + dy }
    ]

/// Seats of a source: walkable (non-wall) tiles adjacent to its position.
/// Terrain only, per ADR 0001 — structures and creeps do not consume Seats.
let private seatCount (spatial: SpatialInfo) (pos: Pos) =
    neighbours pos
    |> List.filter (fun tile ->
        match Map.tryFind tile spatial.Terrain with
        | Some Plain
        | Some Swamp -> true
        | Some Wall
        | None -> false)
    |> List.length

/// Concurrent-worker cap per task id; tasks absent from the map are
/// unbounded. Harvest is capped by its source's Seat count — a snapshot
/// without a spatial projection (or a source it does not place) stays
/// uncapped, so behaviour without terrain data is unchanged.
let private taskCapacities (snapshot: Snapshot) : Map<string, int> =
    match snapshot.Spatial with
    | None -> Map.empty
    | Some spatial ->
        snapshot.Sources
        |> List.choose (fun s ->
            Map.tryFind s.Id spatial.TargetPositions
            |> Option.map (fun pos -> taskId (Harvest s.Id), seatCount spatial pos))
        |> Map.ofList

/// Chebyshev range at which a Task's action reaches its target (Screeps:
/// harvest and transfer act at range 1, build and upgrade at range 3).
let private actionRange =
    function
    | Harvest _
    | Refill _ -> 1
    | Build _
    | Upgrade _ -> 3

/// Id of the game object a Task acts on.
let private targetOf =
    function
    | Harvest id
    | Refill id
    | Build id
    | Upgrade id -> id

/// Cost of stepping onto a tile: plain 1, swamp 5; walls, obstacle
/// structures and tiles outside the projection are impassable (ADR 0001).
let private stepCost (spatial: SpatialInfo) tile =
    if Set.contains tile spatial.Obstacles then
        None
    else
        match Map.tryFind tile spatial.Terrain with
        | Some Plain -> Some 1
        | Some Swamp -> Some 5
        | Some Wall
        | None -> None

/// Work Area of a Task: the tiles a creep may stand on while performing it —
/// passable tiles within the action's range of its target. Derived fresh
/// each tick, never persisted; empty when the projection cannot place the
/// target.
let private workArea (spatial: SpatialInfo) (task: Task) : Set<Pos> =
    match Map.tryFind (targetOf task) spatial.TargetPositions with
    | None -> Set.empty
    | Some target ->
        let r = actionRange task

        Set.ofList
            [
                for x in target.X - r .. target.X + r do
                    for y in target.Y - r .. target.Y + r do
                        let tile = { X = x; Y = y }

                        if (stepCost spatial tile).IsSome then
                            tile
            ]

/// Dijkstra over the terrain: the first step of a cheapest path from
/// `start` to any goal tile, None when no goal is reachable. A Set of
/// (distance, tile) doubles as the priority queue; its ordering also makes
/// tie-breaking deterministic.
let private firstStepToward (spatial: SpatialInfo) (start: Pos) (goals: Set<Pos>) : Pos option =
    let rec firstStepOf tile (parents: Map<Pos, Pos>) =
        match Map.tryFind tile parents with
        | Some parent when parent = start -> tile
        | Some parent -> firstStepOf parent parents
        | None -> tile

    let rec search (frontier: Set<int * Pos>) (dist: Map<Pos, int>) (parents: Map<Pos, Pos>) =
        if Set.isEmpty frontier then
            None
        else
            let (d, tile) as entry = Set.minElement frontier
            let frontier = Set.remove entry frontier

            if Map.tryFind tile dist <> Some d then
                // Stale queue entry: the tile was reached cheaper meanwhile.
                search frontier dist parents
            elif Set.contains tile goals then
                Some(firstStepOf tile parents)
            else
                let step (frontier, dist, parents) next =
                    match stepCost spatial next with
                    | None -> frontier, dist, parents
                    | Some cost ->
                        let candidate = d + cost

                        let improves =
                            match Map.tryFind next dist with
                            | Some best -> candidate < best
                            | None -> true

                        if improves then
                            Set.add (candidate, next) frontier,
                            Map.add next candidate dist,
                            Map.add next tile parents
                        else
                            frontier, dist, parents

                let frontier, dist, parents =
                    ((frontier, dist, parents), neighbours tile) ||> List.fold step

                search frontier dist parents

    if Set.isEmpty goals || Set.contains start goals then
        None
    else
        search (Set.singleton (0, start)) (Map.ofList [ start, 0 ]) Map.empty

/// Direction of a single step between adjacent tiles.
let private directionTo (from: Pos) (dest: Pos) : Direction option =
    match sign (dest.X - from.X), sign (dest.Y - from.Y) with
    | 0, -1 -> Some Top
    | 1, -1 -> Some TopRight
    | 1, 0 -> Some Right
    | 1, 1 -> Some BottomRight
    | 0, 1 -> Some Bottom
    | -1, 1 -> Some BottomLeft
    | -1, 0 -> Some Left
    | -1, -1 -> Some TopLeft
    | _ -> None

/// Resolver, conflict-free case (ADR 0001): a creep in action range acts
/// (the engine judges range by the tick-start position); one standing off
/// its Work Area also gets a single-step move Intent along the cheapest
/// path there — the two combine when the creep's own tile stopped being
/// legal standing ground. An unreachable Work Area yields no move Intent
/// at all (no thrash). Without a spatial fix on both creep and target the
/// action is emitted unconditionally — no movement can be derived,
/// matching the projection-less behaviour elsewhere.
let private resolve (snapshot: Snapshot) (creep: CreepInfo) (task: Task) : Intent list =
    let placed =
        snapshot.Spatial
        |> Option.bind (fun spatial ->
            match
                Map.tryFind creep.Name spatial.CreepPositions,
                Map.tryFind (targetOf task) spatial.TargetPositions
            with
            | Some creepPos, Some targetPos -> Some(spatial, creepPos, targetPos)
            | _ -> None)

    match placed with
    | None -> [ intentFor creep task ]
    | Some(spatial, creepPos, targetPos) ->
        let act =
            if range creepPos targetPos <= actionRange task then
                [ intentFor creep task ]
            else
                []

        let area = workArea spatial task

        let move =
            if Set.contains creepPos area then
                []
            else
                firstStepToward spatial creepPos area
                |> Option.bind (directionTo creepPos)
                |> Option.map (fun direction -> MoveCreep(creep.Name, direction))
                |> Option.toList

        act @ move

/// Matcher: keep still-valid assignments (anti-thrash), greedily assign the
/// rest, and emit one Intent per assigned creep.
let private matchCreeps
    (snapshot: Snapshot)
    (tasks: Task list)
    (assignments: Assignments)
    : Intent list * Assignments =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList
    let capacities = taskCapacities snapshot

    let load (acc: Assignments) tid =
        acc |> Map.filter (fun _ assigned -> assigned = tid) |> Map.count

    let hasCapacity acc tid =
        match Map.tryFind tid capacities with
        | Some cap -> load acc tid < cap
        | None -> true

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed (e.g. across a redeploy).
    let kept =
        (Map.empty, assignments)
        ||> Map.fold (fun acc name tid ->
            match
                snapshot.Creeps |> List.tryFind (fun c -> c.Name = name), Map.tryFind tid byId
            with
            | Some creep, Some task when applicable creep task && hasCapacity acc tid ->
                Map.add name tid acc
            | _ -> acc)

    let assignOne acc (creep: CreepInfo) =
        if Map.containsKey creep.Name acc then
            acc
        else
            match
                tasks |> List.filter (fun t -> applicable creep t && hasCapacity acc (taskId t))
            with
            | [] -> acc
            | candidates ->
                let task = candidates |> List.minBy (fun t -> rank t, load acc (taskId t))
                Map.add creep.Name (taskId task) acc

    let final = snapshot.Creeps |> List.fold assignOne kept

    let intents =
        snapshot.Creeps
        |> List.collect (fun creep ->
            match
                Map.tryFind creep.Name final |> Option.bind (fun tid -> Map.tryFind tid byId)
            with
            | Some task -> resolve snapshot creep task
            | None -> [])

    intents, final

/// The single seam: Snapshot in, Intents plus next tick's Assignments out.
let decide (snapshot: Snapshot) (assignments: Assignments) : Intent list * Assignments =
    let spawnIntents = planSpawns snapshot
    let siteIntents = planConstructionSites snapshot
    let creepIntents, next = matchCreeps snapshot (planTasks snapshot) assignments
    spawnIntents @ siteIntents @ creepIntents, next
