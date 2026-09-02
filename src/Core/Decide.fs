module Fabot.Core.Decide

open Fabot.Core.Types

/// The Workforce target's floor: the colony never plans below this many
/// living creeps. Two keep the harvest/refill loop running while one is in
/// transit or being replaced.
let private minWorkforce = 2

/// A Body pattern: the repeating part block a body is generated from.
/// Which pattern a spawn casts is a colony decision; the pattern shapes
/// what a creep is good at, never what it is assigned (ADR 0006).
type BodyPattern = { Name: string; Block: BodyPart list }

/// The generalist pattern: 200 energy, full speed empty, half speed loaded.
let workerPattern =
    {
        Name = "worker"
        Block = [ Work; Carry; Move ]
    }

/// The pattern table: every body the colony casts is a row here repeated
/// by energy. A future pattern is one more data row plus its own quota
/// rule — a colony fact deciding when it is cast — never a new code path
/// (ADR 0006).
let patternTable = [ workerPattern ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50
        | Attack -> 80
        | RangedAttack -> 150
        | Heal -> 250
        | Claim -> 600
        | Tough -> 10)

/// Body for a pattern at an energy capacity: the largest affordable
/// repetition of the pattern's block (never below one repeat), with the
/// remainder spent on Carry/Move at fatigue parity — the padded body is
/// never slower than the pure-block body, empty or loaded, and within
/// that buys as much Carry as possible. Fatigue parity is the worker
/// pattern's padding policy (ADR 0003, narrowed to that pattern by ADR
/// 0006); a future row arrives with its own padding rule alongside its
/// quota rule. Parts are grouped Work, Carry, Move so damage strips Work
/// first and mobility last.
let bodyFor pattern capacity =
    // Screeps MAX_CREEP_SIZE: the engine rejects bodies over 50 parts.
    let maxBodyParts = 50
    let block = pattern.Block
    let blockSize = List.length block
    let carryCost = bodyCost [ Carry ]
    let moveCost = bodyCost [ Move ]

    let blockCount part =
        block |> List.filter ((=) part) |> List.length

    let repeats = capacity / bodyCost block |> max 1 |> min (maxBodyParts / blockSize)

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
            (repeats * blockCount Work)
            (repeats * blockCount Carry)
            (repeats * blockCount Move)
            (capacity - repeats * bodyCost block)
            (maxBodyParts - repeats * blockSize)

    List.replicate work Work @ List.replicate carry Carry @ List.replicate move Move

/// The generalist body: the worker row of the pattern table, sized to
/// capacity.
let workerBodyFor capacity = bodyFor workerPattern capacity

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

    // Which row of the pattern table this colony casts: the first row
    // whose quota admits another creep. The worker row's quota is the
    // whole workforce target, so today it always wins; a future row
    // arrives with its own quota rule deciding when it is chosen
    // instead (ADR 0006).
    let pattern = List.head patternTable

    // Disaster fallback: an empty colony can never refill extensions, so
    // waiting for full capacity would wait forever — spawn a minimal unit
    // from whatever energy is banked right now.
    let bodyFromBank (bank: RoomEnergy) =
        if List.isEmpty snapshot.Creeps then
            if bank.Available >= bodyCost pattern.Block then
                Some pattern.Block
            else
                None
        elif bank.Available >= bank.Capacity then
            Some(bodyFor pattern bank.Capacity)
        else
            None

    if deficit <= 0 then
        []
    else
        // Idle spawns draw from their room's one bank in list order — each
        // body debits the budget the next spawn sees, so the same energy is
        // never committed twice.
        let intents, _ =
            snapshot.Spawns
            |> List.filter (fun s -> not s.IsSpawning)
            |> List.fold
                (fun (intents, banks: Map<string, RoomEnergy>) s ->
                    let bank =
                        banks
                        |> Map.tryFind s.RoomName
                        |> Option.defaultValue { Available = 0; Capacity = 0 }

                    match bodyFromBank bank with
                    | Some body when List.length intents < deficit ->
                        SpawnCreep(s.Name, body, $"{pattern.Name}-{snapshot.Time}-{s.Name}")
                        :: intents,
                        banks
                        |> Map.add
                            s.RoomName
                            { bank with
                                Available = bank.Available - bodyCost body
                            }
                    | _ -> intents, banks)
                ([], snapshot.RoomEnergy)

        List.rev intents

/// Colony reflex beside the pipeline: a CLAIM-part hostile is the one
/// threat that can disarm safe mode itself — attackController blocks
/// activation for 1,000 ticks — so the activation fires the tick such a
/// hostile is seen, while firing is still possible. Fighters without
/// CLAIM cannot touch the controller and never spend the stock: at RCL2
/// safe mode outlasts any invader raid 13×, so it keeps for when the
/// room is actually being taken (ADR 0007).
let private planSafeMode (snapshot: Snapshot) : Intent list =
    match snapshot.Controller with
    | Some controller when
        controller.SafeModeAvailable > 0
        && not controller.SafeModeActive
        && snapshot.Hostiles |> List.exists (fun h -> List.contains Claim h.Body)
        ->
        [ ActivateSafeMode controller.Id ]
    | _ -> []

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
/// checkerboard around the first placed spawn, nearest tiles first. Sites
/// are not creep work, so this emits Intents directly rather than Tasks.
let private planConstructionSites (snapshot: Snapshot) atlas : Intent list =
    let anchor = snapshot.Spawns |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id)

    match Atlas.roomName atlas, anchor, snapshot.Controller with
    | Some room, Some spawnPos, Some controller ->
        let missing =
            extensionAllowance controller.Level
            - Atlas.builtExtensions atlas
            - Atlas.pendingExtensions atlas

        if missing <= 0 then
            []
        else
            // Same checkerboard colour as the spawn: extensions cluster on the
            // spawn's colour, leaving the other colour free for movement.
            let parity = (spawnPos.X + spawnPos.Y) % 2

            Atlas.buildableTiles atlas
            |> List.filter (fun tile -> (tile.X + tile.Y) % 2 = parity)
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)
            |> List.truncate missing
            |> List.map (fun tile -> PlaceConstructionSite(room, tile, Extension))
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

/// The full downgrade timer per controller level (Screeps
/// CONTROLLER_DOWNGRADE).
let private fullDowngradeTimer level =
    match level with
    | 1 -> 20000
    | 2 -> 10000
    | 3 -> 20000
    | 4 -> 40000
    | 5 -> 80000
    | 6 -> 120000
    | 7 -> 150000
    | _ -> 200000

/// The hard deadline on the controller's downgrade timer: half the
/// level's full timer. The engine refuses activateSafeMode once the
/// timer sinks below half minus 5,000 (its
/// CONTROLLER_DOWNGRADE_SAFEMODE_THRESHOLD grace), so escalating at half
/// keeps the safe-mode reflex fireable with the whole grace still banked
/// — a downgrade costs a level and zeroes the stock, so neither line is
/// ever approached (ADR 0007).
let private downgradeDeadline level = fullDowngradeTimer level / 2

/// Matching tier between applicable tasks (lower wins): feeding the economy
/// (Harvest, Refill) outranks sinking surplus into construction (Build) or
/// the controller (Upgrade). One exception: a controller inside the
/// downgrade deadline makes Upgrade the colony's most urgent work,
/// outranking even the feeding tier (ADR 0007).
let private rank (snapshot: Snapshot) task =
    match task with
    | Harvest _ -> 0
    | Refill _ -> 0
    | Build _ -> 1
    | Upgrade _ ->
        let urgent =
            snapshot.Controller
            |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

        if urgent then -1 else 1

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

/// Emitter: each assigned creep's action Intent, then every assigned
/// creep's chat bubble, both in Snapshot creep order. Judges actions from
/// tick-start geometry — it must run against the same Atlas the Matcher
/// used, never against resolved positions.
let emit (snapshot: Snapshot) atlas (assigned: Map<string, Task>) : Intent list =
    let actions =
        snapshot.Creeps
        |> List.collect (fun creep ->
            match Map.tryFind creep.Name assigned with
            | Some task -> actionIntents atlas creep task
            | None -> [])

    // Every assigned creep says its Task's glyph every tick; unassigned
    // creeps say nothing.
    let says =
        snapshot.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name assigned
            |> Option.map (fun task -> SayCreep(creep.Name, glyphFor task)))

    actions @ says

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
let private moveIntentFor
    (rankOf: Task -> int)
    atlas
    (creep: string)
    (pos: Pos)
    (task: Task option)
    : MoveIntent =
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
                Rank = rankOf task
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
                    Rank = rankOf task
                    Candidates = [ step ]
                }
            | None -> parked (rankOf task)

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
/// creep order. Takes the tick's assigned Task per creep as data; a creep
/// absent from the map is idle.
let resolve (snapshot: Snapshot) atlas (assigned: Map<string, Task>) : Intent list =
    let placed = Atlas.placedCreeps atlas

    let moveIntents =
        placed
        |> List.map (fun (name, pos) ->
            moveIntentFor (rank snapshot) atlas name pos (Map.tryFind name assigned))

    let occupants = placed |> List.map (fun (name, pos) -> pos, name) |> Map.ofList
    let standing = arbitrate occupants moveIntents

    placed
    |> List.choose (fun (name, pos) ->
        Map.tryFind name standing
        |> Option.bind (directionTo pos)
        |> Option.map (fun direction -> MoveCreep(name, direction)))

/// Matcher: keep still-valid assignments (anti-thrash) and greedily assign
/// the rest. Assignments in, Assignments out — emission belongs to the
/// Emitter, movement to the Resolver.
let matchCreeps
    (snapshot: Snapshot)
    atlas
    (tasks: Task list)
    (assignments: Assignments)
    : Assignments =
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
                    candidates
                    |> List.minBy (fun (t, cost) -> rank snapshot t, cost, load acc (taskId t))

                Map.add creep.Name (taskId task) acc

    snapshot.Creeps |> List.fold assignOne kept

/// Join the Matcher's Assignments back onto the Planner's pool: the tick's
/// assigned Task per creep, as data for the Emitter and the Resolver.
let private assignedTasks (tasks: Task list) (assignments: Assignments) : Map<string, Task> =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> Map.tryFind tid byId |> Option.map (fun t -> name, t))
    |> Map.ofList

/// The decision seam: Snapshot in, Intents plus next tick's Assignments
/// out. The tick's pipeline is visible here — plan, match, emit, resolve —
/// beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same
/// flood (ADR 0004).
let decide (snapshot: Snapshot) (assignments: Assignments) : Intent list * Assignments =
    let atlas = Atlas.ofSnapshot snapshot
    let defenseIntents = planSafeMode snapshot
    let spawnIntents = planSpawns snapshot atlas
    let siteIntents = planConstructionSites snapshot atlas
    let tasks = planTasks snapshot
    let next = matchCreeps snapshot atlas tasks assignments
    let assigned = assignedTasks tasks next

    defenseIntents
    @ spawnIntents
    @ siteIntents
    @ emit snapshot atlas assigned
    @ resolve snapshot atlas assigned,
    next
