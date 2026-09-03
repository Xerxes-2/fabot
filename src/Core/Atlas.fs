module Fabot.Core.Atlas

open Fabot.Core.Types

/// A body's fatigue factor (ADR 0006): the parts that generate fatigue
/// when moving and the Move parts that pay it off. Terrain weight scales
/// by their ratio to price travel in cost units — half-ticks under the
/// engine-native weights (ADR 0010).
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
            /// into travel cost for that body (ADR 0006).
            Factors: Map<string, FatigueFactor>
            /// Step weight per tile index (stepCost flattened once for the
            /// flood's hot loop): -1 impassable, else the terrain weight.
            Weights: int[]
            /// Whether a creep stands on each tile index this tick; the
            /// flood prices these tiles dearer so paths detour around
            /// standing traffic.
            Occupied: bool[]
            /// Memoised Dijkstra flood per placed creep's tile and fatigue
            /// factor, forced at most once per tick and shared by every
            /// query pricing from it (ADR 0002, extended to the whole
            /// tick). Bodies of the same factor at the same tile share one
            /// flood. Each flood is a distance and a predecessor-index
            /// array over tile indices.
            Floods: Map<Pos * FatigueFactor, Lazy<int[] * int[]>>
        }

let private roomSide = 50
let private tileCount = roomSide * roomSide
let private indexOf pos = pos.X * roomSide + pos.Y

let private posAt index =
    {
        X = index / roomSide
        Y = index % roomSide
    }

/// Unreached marker in a flood's distance array.
let private unreached = System.Int32.MaxValue

/// Swamp's terrain weight — the dearest passable ground (ADR 0010).
let private swampWeight = 10

/// Extra cost priced onto a step landing on a tile some creep occupies
/// this tick — one swamp step by definition (ADR 0008, re-expressed by
/// ADR 0010): a crowd usually means waiting or displacing, so a modest
/// detour is preferred over pushing through — yet the tile stays
/// passable, unlike an obstacle, so traffic never makes a Task
/// inapplicable.
let private occupancyPenalty = swampWeight

let private neighbours pos =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if (dx, dy) <> (0, 0) then
                    { X = pos.X + dx; Y = pos.Y + dy }
    ]

/// Cost of stepping onto a tile — the engine's own per-part fatigue
/// values (ADR 0010): road 1, plain 2, swamp 10; walls, obstacle
/// structures and tiles outside the projection are impassable (ADR 0001).
/// A built road overrides the terrain under it; a road on a wall (a
/// tunnel) is not modeled and stays impassable.
let private stepCost (spatial: SpatialInfo) tile =
    if Set.contains tile spatial.Obstacles then
        None
    else
        match Map.tryFind tile spatial.Terrain with
        | Some Plain
        | Some Swamp when Set.contains tile spatial.Roads -> Some 1
        | Some Plain -> Some 2
        | Some Swamp -> Some swampWeight
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

/// Cost units the body needs to step onto a tile of the given terrain
/// weight (Screeps fatigue): the step generates weight fatigue per
/// fatigue-generating part, each Move part pays off 2 per tick — so the
/// unit is a half-tick (ADR 0010) — and no step prices below one unit.
/// Deliberately priced at unit granularity, not whole ticks: a Move
/// surplus may price a step below a whole tick, which keeps a road step
/// cheaper than plain for every body. A body without Move parts cannot
/// step at all (the engine's move refuses with ERR_NO_BODYPART).
let private stepUnits (factor: FatigueFactor) weight =
    if factor.MoveParts = 0 then
        None
    else
        Some(max 1 ((weight * factor.FatigueParts + factor.MoveParts - 1) / factor.MoveParts))

/// Dijkstra flood over the weight grid from `start`, priced in cost units
/// for one fatigue factor: cheapest travel cost to every reachable tile
/// (`unreached` elsewhere), plus each tile's predecessor index on a
/// cheapest path (-1 elsewhere). This is the tick's hottest loop, so it
/// runs on flat arrays with a binary min-heap of dist-then-index keys —
/// the key ordering also keeps tie-breaking deterministic. The start tile
/// costs 0 even when it cannot be stepped onto — the creep already stands
/// there. A tile some creep occupies costs occupancyPenalty extra, so
/// paths detour around standing traffic when a detour is cheaper.
let private floodFrom
    (weights: int[])
    (occupied: bool[])
    (factor: FatigueFactor)
    (start: Pos)
    : int[] * int[] =
    let dist = Array.create tileCount unreached
    let parents = Array.create tileCount -1

    // Binary min-heap over dist * tileCount + index: one int per entry.
    let heap = ResizeArray<int>()

    let swap i j =
        let t = heap.[i]
        heap.[i] <- heap.[j]
        heap.[j] <- t

    let push key =
        heap.Add key
        let mutable i = heap.Count - 1

        while i > 0 && heap.[(i - 1) / 2] > heap.[i] do
            swap ((i - 1) / 2) i
            i <- (i - 1) / 2

    let pop () =
        let top = heap.[0]
        heap.[0] <- heap.[heap.Count - 1]
        heap.RemoveAt(heap.Count - 1)
        let mutable i = 0
        let mutable sinking = true

        while sinking do
            let l = 2 * i + 1
            let r = 2 * i + 2
            let mutable smallest = i

            if l < heap.Count && heap.[l] < heap.[smallest] then
                smallest <- l

            if r < heap.Count && heap.[r] < heap.[smallest] then
                smallest <- r

            if smallest = i then
                sinking <- false
            else
                swap i smallest
                i <- smallest

        top

    let startIndex = indexOf start
    dist.[startIndex] <- 0
    push startIndex

    while heap.Count > 0 do
        let key = pop ()
        let index = key % tileCount
        let d = key / tileCount

        // Stale heap entry when unequal: the tile was reached cheaper meanwhile.
        if dist.[index] = d then
            let x = index / roomSide
            let y = index % roomSide

            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    let nx = x + dx
                    let ny = y + dy

                    if
                        (dx <> 0 || dy <> 0) && nx >= 0 && nx < roomSide && ny >= 0 && ny < roomSide
                    then
                        let next = nx * roomSide + ny

                        if weights.[next] >= 0 then
                            match stepUnits factor weights.[next] with
                            | None -> ()
                            | Some units ->
                                let candidate =
                                    d + units + (if occupied.[next] then occupancyPenalty else 0)

                                if candidate < dist.[next] then
                                    dist.[next] <- candidate
                                    parents.[next] <- index
                                    push (candidate * tileCount + next)

    dist, parents

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

    let weights = Array.create tileCount -1

    spatial.Terrain
    |> Map.iter (fun tile _ ->
        weights.[indexOf tile] <- stepCost spatial tile |> Option.defaultValue -1)

    let occupied = Array.create tileCount false

    spatial.CreepPositions
    |> Map.iter (fun _ tile -> occupied.[indexOf tile] <- true)

    {
        Spatial = spatial
        Placed = placed
        Factors = factors
        Weights = weights
        Occupied = occupied
        Floods =
            placed
            |> List.map (fun (name, pos) ->
                let factor = Map.find name factors
                (pos, factor), lazy (floodFrom weights occupied factor pos))
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
    | None -> floodFrom atlas.Weights atlas.Occupied factor pos

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

/// Towers already standing in the room.
let builtTowers (atlas: Atlas) : int =
    targetsOfKind atlas (Structure BuiltKind.Tower) |> List.length

/// Tower construction sites already placed in the room.
let pendingTowers (atlas: Atlas) : int =
    targetsOfKind atlas (Site BuiltKind.Tower) |> List.length

/// Tiles holding a built road — the projection's road census, one half of
/// what the Layout's road gap subtracts (ADR 0011).
let roadTiles (atlas: Atlas) : Set<Pos> = atlas.Spatial.Roads

/// Tiles holding a road construction site — the census's other half: a
/// pending road is not yet a road (ADR 0010) but its tile needs no new site.
let pendingRoadTiles (atlas: Atlas) : Set<Pos> =
    targetsOfKind atlas (Site BuiltKind.Road)
    |> List.choose (fun id -> Map.tryFind id atlas.Spatial.TargetPositions)
    |> Set.ofList

/// Whether a tile's terrain is swamp; a tile outside the projection is not.
let isSwamp (atlas: Atlas) (tile: Pos) : bool =
    Map.tryFind tile atlas.Spatial.Terrain = Some Swamp

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

/// Travel cost of a Task for a creep (ADR 0002, revised by ADRs 0006 and
/// 0010): the cost units — half-ticks — the creep's body needs along a
/// cheapest path to any Work Area
/// tile — terrain weights scaled by the body's fatigue factor, tiles
/// under standing creeps priced occupancyPenalty dearer — 0 for a creep
/// already inside. None — a placed Work Area the creep cannot reach
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
            |> List.choose (fun tile ->
                let d = dist.[indexOf tile]
                if d = unreached then None else Some d)
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

/// First step toward a Task's Work Area over the given flood, sharing
/// firstStep's whole contract — that doc governs both public wrappers.
let private firstStepVia
    (atlas: Atlas)
    (floodOf: Pos -> int[] * int[])
    (creep: string)
    (task: Task)
    : Pos option =
    let rec firstStepOf index startIndex (parents: int[]) =
        let parent = parents.[index]

        if parent = startIndex || parent < 0 then
            index
        else
            firstStepOf parent startIndex parents

    match Map.tryFind creep atlas.Spatial.CreepPositions with
    | None -> None
    | Some pos ->
        let goals = workArea atlas task

        if Set.isEmpty goals || Set.contains pos goals then
            None
        else
            let dist, parents = floodOf pos

            goals
            |> Set.toList
            |> List.choose (fun goal ->
                let d = dist.[indexOf goal]
                if d = unreached then None else Some(d, goal))
            |> function
                | [] -> None
                | reachable ->
                    let _, goal = List.min reachable
                    Some(posAt (firstStepOf (indexOf goal) (indexOf pos) parents))

/// The first step of a cheapest path from a creep to its Task's Work Area,
/// priced in the creep's own cost — a slow body may detour differently
/// than a fast one over the same ground. None when there is nothing
/// derivable: the creep is unplaced, already inside the area, or the area
/// is empty or unreachable. Of equally cheap goals the lowest (cost, tile)
/// wins, matching the flood's tie-breaking.
let firstStep (atlas: Atlas) (creep: string) (task: Task) : Pos option =
    firstStepVia atlas (flood atlas creep) creep task

/// No tile occupied: the flood baseline the occupancy surcharge is judged
/// against.
let private noTraffic: bool[] = Array.create tileCount false

/// The first step the same body would take were no tile occupied — the
/// traffic-blind route, otherwise priced exactly like firstStep. The
/// Resolver compares the two: a difference attributes the detour to the
/// occupancy surcharge, which is the only pricing the two floods do not
/// share (ADR 0008, ADR 0009).
let firstStepIgnoringTraffic (atlas: Atlas) (creep: string) (task: Task) : Pos option =
    firstStepVia atlas (floodFrom atlas.Weights noTraffic (factorOf atlas creep)) creep task

/// Cheapest raw-terrain path for a trunk road (ADR 0011): plain 2, swamp
/// 10 — no road discount and no occupancy surcharge, so the line neither
/// shifts as its own roads get built nor bends around today's traffic.
/// Walls, obstacle structures and the `avoid` tiles (the Layout's
/// reservations) are impassable; the origin prices 0 though it cannot be
/// stood on — a source sits in wall terrain, yet its trunk starts beside
/// it. Answers the path tiles from the first step beside the origin to the
/// cheapest reachable goal, or [] when no goal is reachable — unpriceable
/// geometry paves nothing. Deterministic: the flood's dist-then-index heap
/// keys and the lowest (cost, tile) goal break every tie.
let trunkPath (atlas: Atlas) (avoid: Set<Pos>) (origin: Pos) (goals: Set<Pos>) : Pos list =
    let weights = Array.create tileCount -1

    atlas.Spatial.Terrain
    |> Map.iter (fun tile terrain ->
        if not (Set.contains tile atlas.Spatial.Obstacles) && not (Set.contains tile avoid) then
            match terrain with
            | Plain -> weights.[indexOf tile] <- 2
            | Swamp -> weights.[indexOf tile] <- swampWeight
            | Wall -> ())

    let dist, parents =
        floodFrom weights noTraffic { FatigueParts = 1; MoveParts = 1 } origin

    goals
    |> Set.toList
    |> List.choose (fun goal ->
        let d = dist.[indexOf goal]
        if d = unreached then None else Some(d, goal))
    |> function
        | [] -> []
        | reachable ->
            let _, goal = List.min reachable
            let originIndex = indexOf origin

            let rec walk index acc =
                if index = originIndex then
                    acc
                else
                    walk parents.[index] (posAt index :: acc)

            walk (indexOf goal) []
