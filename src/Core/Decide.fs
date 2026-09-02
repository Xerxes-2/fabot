module Fabot.Core.Decide

open Fabot.Core.Types

/// The Workforce target's floor: the colony never plans below this many
/// living creeps. Two keep the harvest/refill loop running while one is in
/// transit or being replaced.
let minWorkforce = 2

/// The repeating unit worker bodies are built from.
let workerUnit = [ Work; Carry; Move ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50)

/// Worker body for an energy capacity: the largest affordable repetition
/// of the worker unit (never below one), with the remainder spent on
/// Carry/Move at fatigue parity — the padded body is never slower than
/// the pure-unit body, empty or loaded, and within that buys as much
/// Carry as possible (ADR 0003). Parts are grouped Work, Carry, Move so
/// damage strips Work first and mobility last.
let workerBodyFor capacity =
    // Screeps MAX_CREEP_SIZE: the engine rejects bodies over 50 parts.
    let maxBodyParts = 50
    let unitSize = List.length workerUnit
    let carryCost = bodyCost [ Carry ]
    let moveCost = bodyCost [ Move ]

    let units = capacity / bodyCost workerUnit |> max 1 |> min (maxBodyParts / unitSize)

    // Loaded parity is work + carry <= 2 * move: a lone Carry is added
    // only under that bound, a Carry+Move pair preserves it, and a lone
    // Move (the trailing 50) only widens it.
    let rec pad work carry move budget slots =
        if slots >= 1 && budget >= carryCost && work + carry + 1 <= 2 * move then
            pad work (carry + 1) move (budget - carryCost) (slots - 1)
        elif slots >= 2 && budget >= carryCost + moveCost then
            pad work (carry + 1) (move + 1) (budget - carryCost - moveCost) (slots - 2)
        elif slots >= 1 && budget >= moveCost then
            pad work carry (move + 1) (budget - moveCost) (slots - 1)
        else
            work, carry, move

    let work, carry, move =
        pad
            units
            units
            units
            (capacity - units * bodyCost workerUnit)
            (maxBodyParts - units * unitSize)

    List.replicate work Work @ List.replicate carry Carry @ List.replicate move Move

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

/// Workforce target: how many creeps the colony maintains — the total Seat
/// count across all sources, floored at minWorkforce. Derived fresh each
/// tick; a source the projection does not place contributes no Seats.
let private workforceTarget (snapshot: Snapshot) atlas =
    snapshot.Sources
    |> List.sumBy (fun s -> Atlas.seats atlas s.Id |> Option.defaultValue 0)
    |> max minWorkforce

/// Pre-Task bootstrap step: spawn Intents needed to keep the workforce at
/// the Workforce target. Spawning is a colony-level need, not a Task creeps
/// get matched to, so it sits beside the Planner/Matcher pipeline rather
/// than inside it.
let private planSpawns (snapshot: Snapshot) atlas : Intent list =
    let deficit = workforceTarget snapshot atlas - List.length snapshot.Creeps

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
        // Each spawn is gated against the shared room energy, so several
        // idle spawns can emit intents the same energy only affords once;
        // the engine fails the surplus and the deficit re-plans next tick.
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

/// Chat-bubble glyph of a Task: the whole colony's current matching is
/// legible in the viewer at one glyph per creep.
let private glyphFor =
    function
    | Harvest _ -> "⛏"
    | Refill _ -> "🔋"
    | Build _ -> "🔨"
    | Upgrade _ -> "⚡"

/// Matching tier between applicable tasks (lower wins): feeding the economy
/// (Harvest, Refill) outranks sinking surplus into construction (Build) or
/// the controller (Upgrade).
let private rank =
    function
    | Harvest _ -> 0
    | Refill _ -> 0
    | Build _ -> 1
    | Upgrade _ -> 1

/// Concurrent-worker cap per task id; tasks absent from the map are
/// unbounded. Harvest is capped by its source's Seat count — a source the
/// projection does not place derives no cap, so behaviour without terrain
/// data is unchanged.
let private taskCapacities (snapshot: Snapshot) atlas : Map<string, int> =
    snapshot.Sources
    |> List.choose (fun s ->
        Atlas.seats atlas s.Id |> Option.map (fun count -> taskId (Harvest s.Id), count))
    |> Map.ofList

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position.
let private actionIntents atlas (creep: CreepInfo) (task: Task) : Intent list =
    if Atlas.mayAct atlas creep.Name task then
        [ intentFor creep task ]
    else
        []

/// A creep's Move Intent: candidate standing tiles for next tick in
/// preference order, plus a priority (the task rank). Input to the
/// Resolver — not an Intent; the Resolver's output is what becomes one.
type private MoveIntent =
    {
        Creep: string
        Pos: Pos
        Rank: int
        Candidates: Pos list
    }

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// Register one creep's Move Intent — every creep gets one (ADR 0001).
/// A creep travelling toward its Work Area wants exactly its next path
/// step; one already inside is force-registered "stay put, displaceable
/// within the Work Area"; one with no Task — or no way to reach its
/// area, which is just as immobilising — is parked: stay put,
/// displaceable to any adjacent walkable tile.
let private moveIntentFor atlas (creep: string) (pos: Pos) (task: Task option) : MoveIntent =
    let parked rank =
        {
            Creep = creep
            Pos = pos
            Rank = rank
            Candidates = pos :: Atlas.adjacentWalkable atlas pos
        }

    match task with
    | None -> parked idleRank
    | Some task ->
        let area = Atlas.workArea atlas task

        if Set.contains pos area then
            {
                Creep = creep
                Pos = pos
                Rank = rank task
                Candidates =
                    pos
                    :: (Atlas.adjacentWalkable atlas pos
                        |> List.filter (fun tile -> Set.contains tile area))
            }
        else
            match Atlas.firstStep atlas creep task with
            | Some step ->
                {
                    Creep = creep
                    Pos = pos
                    Rank = rank task
                    Candidates = [ step ]
                }
            | None -> parked (rank task)

/// Resolver core (per screeps-cartographer): claim tiles priority
/// descending, most-constrained first within a priority. Claiming a tile
/// somebody stands on displaces that occupant: the claimed tile leaves the
/// occupant's candidates and the claimant's vacated tile joins them as a
/// last resort, so an occupant that cannot stand elsewhere swaps with its
/// displacer. An occupant left with fewer than two open candidates
/// resolves immediately, ahead of every rank, locking the exchange in
/// before the vacated tile is claimed by anyone else.
let private arbitrate
    (occupants: Map<Pos, string>)
    (moveIntents: MoveIntent list)
    : Map<string, Pos> =
    let openCandidates (claimed: Set<Pos>) (intent: MoveIntent) =
        intent.Candidates |> List.filter (fun tile -> not (Set.contains tile claimed))

    let rec settle (pending: Map<string, MoveIntent>) urgent claimed resolved =
        let next =
            match urgent |> List.filter (fun name -> Map.containsKey name pending) with
            | name :: rest -> Some(Map.find name pending, rest)
            | [] ->
                if Map.isEmpty pending then
                    None
                else
                    pending
                    |> Map.toList
                    |> List.map snd
                    |> List.minBy (fun i -> i.Rank, List.length (openCandidates claimed i), i.Creep)
                    |> fun intent -> Some(intent, [])

        match next with
        | None -> resolved
        | Some(intent, urgent) ->
            let pending = Map.remove intent.Creep pending

            let chosen =
                match openCandidates claimed intent with
                | tile :: _ -> tile
                // Nowhere left to stand: stay put and let the engine fail
                // whichever move contests this tile.
                | [] -> intent.Pos

            let claimed = Set.add chosen claimed
            let resolved = Map.add intent.Creep chosen resolved

            match Map.tryFind chosen occupants with
            | Some other when Map.containsKey other pending ->
                let occupant = Map.find other pending

                let displaced =
                    { occupant with
                        Candidates =
                            (occupant.Candidates |> List.filter ((<>) chosen))
                            @ (if List.contains intent.Pos occupant.Candidates then
                                   []
                               else
                                   [ intent.Pos ])
                    }

                let pending = Map.add other displaced pending

                let urgent =
                    if List.length (openCandidates claimed displaced) < 2 then
                        other :: urgent
                    else
                        urgent

                settle pending urgent claimed resolved
            | _ -> settle pending urgent claimed resolved

    let pending = moveIntents |> List.map (fun i -> i.Creep, i) |> Map.ofList
    settle pending [] Set.empty Map.empty

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

/// Resolver, room pass: every creep the Atlas places registers a Move
/// Intent, arbitration settles them into at most one single-step move per
/// creep, and the settled standing tiles become move Intents in Snapshot
/// creep order.
let private resolvedMoves atlas (taskFor: string -> Task option) : Intent list =
    let placed = Atlas.placedCreeps atlas

    let moveIntents =
        placed
        |> List.map (fun (name, pos) -> moveIntentFor atlas name pos (taskFor name))

    let occupants = placed |> List.map (fun (name, pos) -> pos, name) |> Map.ofList
    let standing = arbitrate occupants moveIntents

    placed
    |> List.choose (fun (name, pos) ->
        Map.tryFind name standing
        |> Option.bind (directionTo pos)
        |> Option.map (fun direction -> MoveCreep(name, direction)))

/// Matcher: keep still-valid assignments (anti-thrash), greedily assign the
/// rest, and emit each assigned creep's Intents (action, chat bubble, move).
let private matchCreeps
    (snapshot: Snapshot)
    atlas
    (tasks: Task list)
    (assignments: Assignments)
    : Intent list * Assignments =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList
    let capacities = taskCapacities snapshot atlas

    let load (acc: Assignments) tid =
        acc |> Map.filter (fun _ assigned -> assigned = tid) |> Map.count

    let hasCapacity acc tid =
        match Map.tryFind tid capacities with
        | Some cap -> load acc tid < cap
        | None -> true

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed (e.g. across a redeploy). So does
    // reachability: a Work Area the Atlas can no longer reach releases the
    // assignment, freeing its capacity for creeps that can get there —
    // deliberately with no range-based fallback (ADR 0002).
    let kept =
        (Map.empty, assignments)
        ||> Map.fold (fun acc name tid ->
            match
                snapshot.Creeps |> List.tryFind (fun c -> c.Name = name), Map.tryFind tid byId
            with
            | Some creep, Some task when
                applicable creep task
                && hasCapacity acc tid
                && (Atlas.travelCost atlas creep.Name task).IsSome
                ->
                Map.add name tid acc
            | _ -> acc)

    let assignOne acc (creep: CreepInfo) =
        if Map.containsKey creep.Name acc then
            acc
        else
            let candidates =
                tasks
                |> List.choose (fun t ->
                    if applicable creep t && hasCapacity acc (taskId t) then
                        Atlas.travelCost atlas creep.Name t |> Option.map (fun cost -> t, cost)
                    else
                        None)

            match candidates with
            | [] -> acc
            | candidates ->
                let task, _ =
                    candidates |> List.minBy (fun (t, cost) -> rank t, cost, load acc (taskId t))

                Map.add creep.Name (taskId task) acc

    let final = snapshot.Creeps |> List.fold assignOne kept

    let taskFor name =
        Map.tryFind name final |> Option.bind (fun tid -> Map.tryFind tid byId)

    let actions =
        snapshot.Creeps
        |> List.collect (fun creep ->
            match taskFor creep.Name with
            | Some task -> actionIntents atlas creep task
            | None -> [])

    // Every assigned creep says its Task's glyph every tick; unassigned
    // creeps say nothing.
    let says =
        snapshot.Creeps
        |> List.choose (fun creep ->
            taskFor creep.Name
            |> Option.map (fun task -> SayCreep(creep.Name, glyphFor task)))

    actions @ says @ resolvedMoves atlas taskFor, final

/// The decision seam: Snapshot in, Intents plus next tick's Assignments
/// out. Geometry is consulted through one Atlas built here, so every step
/// prices from the same flood (ADR 0004).
let decide (snapshot: Snapshot) (assignments: Assignments) : Intent list * Assignments =
    let atlas = Atlas.ofSnapshot snapshot
    let spawnIntents = planSpawns snapshot atlas
    let siteIntents = planConstructionSites snapshot
    let creepIntents, next = matchCreeps snapshot atlas (planTasks snapshot) assignments
    spawnIntents @ siteIntents @ creepIntents, next
