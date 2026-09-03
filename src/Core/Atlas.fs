module Fabot.Core.Atlas

open Fabot.Core.Types

/// A body's fatigue factor (ADR 0006): the parts that generate fatigue
/// when moving and the Move parts that pay it off. Terrain weight scales
/// by their ratio to price travel in whole ticks.
type private FatigueFactor = { FatigueParts: int; MoveParts: int }

/// The per-tick, task-aware query interface over the spatial projection
/// (ADR 0004). Total: geometry the projection cannot place gets one
/// documented answer per query — it never counts against a Task and never
/// blocks an action.
type Atlas =
    private
        {
            Spatial: SpatialInfo
            /// Placed creeps in Snapshot order — the canonical iteration
            /// order for everything derived per creep.
            Placed: (string * Pos) list
            /// Each creep's fatigue factor — what turns terrain weight
            /// into ticks for that body (ADR 0006).
            Factors: Map<string, FatigueFactor>
            /// Memoised Dijkstra flood per placed creep's tile and fatigue
            /// factor, forced at most once per tick and shared by every
            /// query pricing from it (ADR 0002, extended to the whole
            /// tick). Bodies of the same factor at the same tile share one
            /// flood.
            Floods: Map<Pos * FatigueFactor, Lazy<Map<Pos, int> * Map<Pos, Pos>>>
        }

let private neighbours pos =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if (dx, dy) <> (0, 0) then
                    { X = pos.X + dx; Y = pos.Y + dy }
    ]

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

/// A creep's fatigue factor from its body and current load: every part
/// except Move and except empty Carry generates fatigue — the engine
/// loads Carry parts 50 energy apiece, and the empty ones ride free.
let private fatigueFactorOf (creep: CreepInfo) : FatigueFactor =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    let carry = count Carry
    let loadedCarry = min carry ((creep.Energy + 49) / 50)
    let parts = creep.Body |> Map.toList |> List.sumBy snd

    {
        FatigueParts = parts - count Move - (carry - loadedCarry)
        MoveParts = count Move
    }

/// Ticks the body needs to step onto a tile of the given terrain weight
/// (Screeps fatigue): the step generates 2 × weight fatigue per
/// fatigue-generating part, each Move part pays off 2 per tick, and no
/// step takes less than one tick. A body without Move parts cannot step
/// at all (the engine's move refuses with ERR_NO_BODYPART).
let private tickCost (factor: FatigueFactor) weight =
    if factor.MoveParts = 0 then
        None
    else
        Some(max 1 ((weight * factor.FatigueParts + factor.MoveParts - 1) / factor.MoveParts))

/// Dijkstra flood over the terrain from `start`, priced in ticks for one
/// fatigue factor: cheapest travel cost to every reachable tile, plus
/// each tile's predecessor on a cheapest path. A Set of (distance, tile)
/// doubles as the priority queue; its ordering also makes tie-breaking
/// deterministic. The start tile costs 0 even when it cannot be stepped
/// onto — the creep already stands there.
let private floodFrom
    (spatial: SpatialInfo)
    (factor: FatigueFactor)
    (start: Pos)
    : Map<Pos, int> * Map<Pos, Pos> =
    let rec search (frontier: Set<int * Pos>) (dist: Map<Pos, int>) (parents: Map<Pos, Pos>) =
        if Set.isEmpty frontier then
            dist, parents
        else
            let (d, tile) as entry = Set.minElement frontier
            let frontier = Set.remove entry frontier

            if Map.tryFind tile dist <> Some d then
                // Stale queue entry: the tile was reached cheaper meanwhile.
                search frontier dist parents
            else
                let step (frontier, dist, parents) next =
                    match stepCost spatial next |> Option.bind (tickCost factor) with
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

    search (Set.singleton (0, start)) (Map.ofList [ start, 0 ]) Map.empty

let ofSnapshot (snapshot: Snapshot) : Atlas =
    let spatial = snapshot.Spatial

    let placed =
        snapshot.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name spatial.CreepPositions
            |> Option.map (fun pos -> creep.Name, pos))

    let factors =
        snapshot.Creeps
        |> List.map (fun creep -> creep.Name, fatigueFactorOf creep)
        |> Map.ofList

    {
        Spatial = spatial
        Placed = placed
        Factors = factors
        Floods =
            placed
            |> List.map (fun (name, pos) ->
                let factor = Map.find name factors
                (pos, factor), lazy (floodFrom spatial factor pos))
            |> Map.ofList
    }

/// A creep's fatigue factor; a creep the Snapshot does not carry prices
/// as a bare one-part-one-Move body — terrain weight verbatim.
let private factorOf (atlas: Atlas) (creep: string) : FatigueFactor =
    Map.tryFind creep atlas.Factors
    |> Option.defaultValue { FatigueParts = 1; MoveParts = 1 }

/// The memoised flood for a creep from a tile; placed creeps' own tiles
/// hit the memo.
let private flood (atlas: Atlas) (creep: string) (pos: Pos) =
    let factor = factorOf atlas creep

    match Map.tryFind (pos, factor) atlas.Floods with
    | Some memo -> memo.Value
    | None -> floodFrom atlas.Spatial factor pos

/// The creeps the projection places, in Snapshot creep order.
let placedCreeps (atlas: Atlas) : (string * Pos) list = atlas.Placed

/// Name of the room the projection covers; None when the projection is empty.
let roomName (atlas: Atlas) : string option = atlas.Spatial.RoomName

/// Tile of a projected target (source, structure, site, controller).
let positionOf (atlas: Atlas) (targetId: string) : Pos option =
    Map.tryFind targetId atlas.Spatial.TargetPositions

/// Tiles a construction site may occupy: non-Wall terrain holding no
/// projected target — anything standing (or being built) on a tile keeps a
/// site off it; creeps do not. Deterministic (X, Y) order.
let buildableTiles (atlas: Atlas) : Pos list =
    let taken =
        atlas.Spatial.TargetPositions |> Map.toList |> List.map snd |> Set.ofList

    atlas.Spatial.Terrain
    |> Map.toList
    |> List.choose (fun (tile, terrain) ->
        if terrain <> Wall && not (Set.contains tile taken) then
            Some tile
        else
            None)

/// Ids of the projected targets of one kind, in id order.
let private targetsOfKind (atlas: Atlas) (kind: TargetKind) : string list =
    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, k) -> if k = kind then Some id else None)

/// Extensions already standing in the room.
let builtExtensions (atlas: Atlas) : int =
    targetsOfKind atlas (Structure BuiltKind.Extension) |> List.length

/// Extension construction sites already placed in the room.
let pendingExtensions (atlas: Atlas) : int =
    targetsOfKind atlas (Site BuiltKind.Extension) |> List.length

/// Walkable tiles adjacent to `pos`, in deterministic (X, Y) order.
/// Standing respects obstacles, unlike Seat counting.
let adjacentWalkable (atlas: Atlas) (pos: Pos) : Pos list =
    neighbours pos |> List.filter (fun tile -> (stepCost atlas.Spatial tile).IsSome)

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

/// Seat tiles of a placed source: walkable (non-wall) neighbours of its
/// tile, by terrain alone — structures and creeps do not consume Seats
/// (ADR 0001).
let private seatTiles (spatial: SpatialInfo) (pos: Pos) : Set<Pos> =
    neighbours pos
    |> List.filter (fun tile ->
        match Map.tryFind tile spatial.Terrain with
        | Some Plain
        | Some Swamp -> true
        | Some Wall
        | None -> false)
    |> Set.ofList

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    Map.tryFind sourceId atlas.Spatial.TargetPositions
    |> Option.map (seatTiles atlas.Spatial >> Set.count)

/// Work Area of a Task: the tiles a creep may stand on while performing it —
/// passable tiles within the action's range of its target. Empty when the
/// projection cannot place the target.
let workArea (atlas: Atlas) (task: Task) : Set<Pos> =
    match Map.tryFind (targetOf task) atlas.Spatial.TargetPositions with
    | None -> Set.empty
    | Some target ->
        let r = actionRange task

        Set.ofList
            [
                for x in target.X - r .. target.X + r do
                    for y in target.Y - r .. target.Y + r do
                        let tile = { X = x; Y = y }

                        if (stepCost atlas.Spatial tile).IsSome then
                            tile
            ]

/// Dual Seats of the room: tiles inside both some projected source's Seats
/// and a projected controller's Upgrade Work Area — a creep standing on one
/// harvests and upgrades without ever moving. Total: a room with no
/// controller, no sources, or a disjoint pair answers with the empty set,
/// which never punishes anything (ADR 0004). Derived fresh each tick,
/// never persisted.
let dualSeats (atlas: Atlas) : Set<Pos> =
    let seatUnion =
        targetsOfKind atlas Source
        |> List.choose (fun id ->
            Map.tryFind id atlas.Spatial.TargetPositions
            |> Option.map (seatTiles atlas.Spatial))
        |> List.fold Set.union Set.empty

    let upgradeArea =
        targetsOfKind atlas Controller
        |> List.map (Upgrade >> workArea atlas)
        |> List.fold Set.union Set.empty

    Set.intersect seatUnion upgradeArea

/// Travel cost of a Task for a creep (ADR 0002, revised by ADR 0006): the
/// ticks the creep's body needs along a cheapest path to any Work Area
/// tile — terrain weights scaled by the body's fatigue factor — 0 for a
/// creep already inside. None — a placed Work Area the creep cannot reach
/// (a body without Move parts reaches nothing), or an empty one — makes
/// the Task inapplicable to that creep. An unplaced creep or target
/// prices at 0: unpriceable geometry never counts against a Task (ADR
/// 0004).
let travelCost (atlas: Atlas) (creep: string) (task: Task) : int option =
    match
        Map.tryFind creep atlas.Spatial.CreepPositions,
        Map.tryFind (targetOf task) atlas.Spatial.TargetPositions
    with
    | None, _
    | _, None -> Some 0
    | Some pos, Some _ ->
        let area = workArea atlas task

        if Set.contains pos area then
            Some 0
        else
            let dist, _ = flood atlas creep pos

            area
            |> Set.toList
            |> List.choose (fun tile -> Map.tryFind tile dist)
            |> function
                | [] -> None
                | costs -> Some(List.min costs)

/// Screeps range: Chebyshev distance between two tiles.
let private range a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// Whether a creep may perform its Task's action this tick: within the
/// action's range at tick start (the engine judges range by that
/// position). A creep or target the projection cannot place never blocks
/// the action — unpriceable geometry is permissive (ADR 0004).
let mayAct (atlas: Atlas) (creep: string) (task: Task) : bool =
    match
        Map.tryFind creep atlas.Spatial.CreepPositions,
        Map.tryFind (targetOf task) atlas.Spatial.TargetPositions
    with
    | Some creepPos, Some targetPos -> range creepPos targetPos <= actionRange task
    | _ -> true

/// The first step of a cheapest path from a creep to its Task's Work Area,
/// priced in the creep's own ticks — a slow body may detour differently
/// than a fast one over the same ground. None when there is nothing
/// derivable: the creep is unplaced, already inside the area, or the area
/// is empty or unreachable. Of equally cheap goals the lowest (cost, tile)
/// wins, matching the flood's tie-breaking.
let firstStep (atlas: Atlas) (creep: string) (task: Task) : Pos option =
    let rec firstStepOf tile start (parents: Map<Pos, Pos>) =
        match Map.tryFind tile parents with
        | Some parent when parent = start -> tile
        | Some parent -> firstStepOf parent start parents
        | None -> tile

    match Map.tryFind creep atlas.Spatial.CreepPositions with
    | None -> None
    | Some pos ->
        let goals = workArea atlas task

        if Set.isEmpty goals || Set.contains pos goals then
            None
        else
            let dist, parents = flood atlas creep pos

            goals
            |> Set.toList
            |> List.choose (fun goal -> Map.tryFind goal dist |> Option.map (fun d -> d, goal))
            |> function
                | [] -> None
                | reachable ->
                    let _, goal = List.min reachable
                    Some(firstStepOf goal pos parents)
