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
            /// Work Area per Task, built at most once per tick and shared
            /// by every query that stands a creep in one — the Floods memo
            /// on a key set the Snapshot does not carry, so a mutable
            /// table rather than a pre-laid Lazy map. The Atlas is rebuilt
            /// every tick, so the table is per-tick by construction:
            /// "Derived fresh each tick, never persisted" stands.
            WorkAreas: System.Collections.Generic.Dictionary<Task, Set<Pos>>
            /// The Work-heavy variant of the same table (ADR 0020): the
            /// narrowed area per Task, built at most once per tick. Only
            /// Harvest narrows, so this holds at most one entry per source,
            /// and deriving `posts` costs one derivation per source per
            /// tick rather than one per creep priced.
            HeavyAreas: System.Collections.Generic.Dictionary<Task, Set<Pos>>
            /// The creeps whose bodies carry more Work parts than Move
            /// parts — ADR 0016's predicate, read from the body and never
            /// from a name or a birth row. The Withdraw gate, the Anchor
            /// census and Harvest's narrowed Work Area (ADR 0020) all ask
            /// it, so the arithmetic lives here once.
            Heavy: Set<string>
            /// Memoised controller-container census, built at most once per
            /// tick (ADR 0019): the applicability gate asks per creep and
            /// per Withdraw candidate, and the answer is a colony fact, not
            /// a per-creep one. A key set of one, so a cell rather than the
            /// table WorkAreas needs.
            mutable Buffers: Set<string> option
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
        WorkAreas = System.Collections.Generic.Dictionary()
        HeavyAreas = System.Collections.Generic.Dictionary()
        Heavy =
            snapshot.Creeps
            |> List.filter (fun creep ->
                let count part =
                    creep.Body |> Map.tryFind part |> Option.defaultValue 0

                count Work > count Move)
            |> List.map (fun creep -> creep.Name)
            |> Set.ofList
        Buffers = None
    }

/// Whether a creep's body was cast from a heavy-Work row: more Work parts
/// than Move (ADR 0016). Fatigue parity keeps every worker body at
/// Work <= Move (ADR 0003) and the Anchor row's floor of two Work over one
/// Move clears it, so the casting pattern is readable off the body itself —
/// what a creep is is decided from what it is made of; the row name in a
/// creep's name is observability only, never read back (ADR 0006). A creep
/// the Snapshot does not carry is not heavy: an unknown body claims no
/// Post and no exemption.
let workHeavy (atlas: Atlas) (creep: string) : bool = Set.contains creep atlas.Heavy

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
/// site off it; creeps do not, and neither does a dropped pile — a
/// transient pile perturbing the ordering would break the Layout's
/// determinism (ADR 0011). Deterministic (X, Y) order.
let buildableTiles (atlas: Atlas) : Pos list =
    let taken =
        atlas.Spatial.TargetPositions
        |> Map.toList
        |> List.filter (fun (id, _) -> Map.tryFind id atlas.Spatial.TargetKinds <> Some Dropped)
        |> List.map snd
        |> Set.ofList

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

/// Placed targets of one kind: id and tile, in id order.
let private placedOfKind (atlas: Atlas) (kind: TargetKind) : (string * Pos) list =
    targetsOfKind atlas kind
    |> List.choose (fun id ->
        Map.tryFind id atlas.Spatial.TargetPositions |> Option.map (fun pos -> id, pos))

/// Towers standing in the room: id and tile, in id order — the fire
/// reflex's whole view of a tower (ADR 0014): no store is projected, a
/// dry tower's shot simply fails at the engine.
let placedTowers (atlas: Atlas) : (string * Pos) list =
    placedOfKind atlas (Structure BuiltKind.Tower)

/// Dropped energy piles the projection places: id and tile, in id order.
/// The pickup reflex's whole view of a pile — no amount is projected.
let droppedEnergy (atlas: Atlas) : (string * Pos) list = placedOfKind atlas Dropped

/// Tiles holding a built road — the projection's road census, one half of
/// what the Layout's road gap subtracts (ADR 0011).
let roadTiles (atlas: Atlas) : Set<Pos> = atlas.Spatial.Roads

/// Tiles of every placed target of one kind.
let private tilesOfKind (atlas: Atlas) (kind: TargetKind) : Set<Pos> =
    targetsOfKind atlas kind
    |> List.choose (fun id -> Map.tryFind id atlas.Spatial.TargetPositions)
    |> Set.ofList

/// Tiles holding a road construction site — the census's other half: a
/// pending road is not yet a road (ADR 0010) but its tile needs no new site.
let pendingRoadTiles (atlas: Atlas) : Set<Pos> = tilesOfKind atlas (Site BuiltKind.Road)

/// Tiles holding a built container — the container census's standing half
/// (ADR 0012): a built container keeps the Layout from re-dropping its site.
let containerTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Structure BuiltKind.Container)

/// Tiles holding a container construction site — the census's pending
/// half: a pending container is not yet a container but its tile needs no
/// new site.
let pendingContainerTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Site BuiltKind.Container)

/// Whether a tile's terrain is swamp; a tile outside the projection is not.
let isSwamp (atlas: Atlas) (tile: Pos) : bool =
    Map.tryFind tile atlas.Spatial.Terrain = Some Swamp

/// Walkable tiles adjacent to `pos`, in deterministic (X, Y) order.
/// Standing respects obstacles, unlike Seat counting.
let adjacentWalkable (atlas: Atlas) (pos: Pos) : Pos list =
    neighbours pos |> List.filter (fun tile -> (stepCost atlas.Spatial tile).IsSome)

/// Chebyshev range at which a Task's action reaches its target (Screeps:
/// harvest, withdraw and transfer act at range 1; build, repair and
/// upgrade at range 3).
let private actionRange =
    function
    | Harvest _
    | Withdraw _
    | Refill _ -> 1
    | Build _
    | Repair _
    | Upgrade _ -> 3

/// Id of the game object a Task acts on.
let private targetOf =
    function
    | Harvest id
    | Withdraw id
    | Refill id
    | Build id
    | Repair id
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

/// Seat tiles of a source — the geometry behind `seats`, for the Layout's
/// source-container pick (ADR 0012). Empty for a source the projection
/// does not place: an unplaceable source anchors nothing.
let seatTilesOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    Map.tryFind sourceId atlas.Spatial.TargetPositions
    |> Option.map (seatTiles atlas.Spatial)
    |> Option.defaultValue Set.empty

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    Map.tryFind sourceId atlas.Spatial.TargetPositions
    |> Option.map (seatTiles atlas.Spatial >> Set.count)

let private buildWorkArea (atlas: Atlas) (task: Task) : Set<Pos> =
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

/// Work Area of a Task, body-blind: the passable tiles within the action's
/// range of its target. The base geometry `posts` itself is derived from
/// (through the controller's Upgrade area), so it stays a pure function of
/// the Task; readers that hold a creep want `workAreaFor`, which narrows it
/// for a Work-heavy harvester (ADR 0020). Empty when the
/// projection cannot place the target. Memoised per Task for the tick: the
/// same area is asked for once per creep the Matcher prices and again by
/// the Emitter and Resolver, and none of those readers can observe whether
/// the set was built or recalled.
let private memoised
    (table: System.Collections.Generic.Dictionary<Task, Set<Pos>>)
    (task: Task)
    (build: unit -> Set<Pos>)
    : Set<Pos> =
    match table.TryGetValue task with
    | true, area -> area
    | _ ->
        let area = build ()
        table.[task] <- area
        area

let workArea (atlas: Atlas) (task: Task) : Set<Pos> =
    memoised atlas.WorkAreas task (fun () -> buildWorkArea atlas task)

/// Every projected source's Seat tiles, unioned — the seat half behind
/// dualSeats and posts.
let private seatUnion (atlas: Atlas) : Set<Pos> =
    targetsOfKind atlas Source
    |> List.choose (fun id ->
        Map.tryFind id atlas.Spatial.TargetPositions
        |> Option.map (seatTiles atlas.Spatial))
    |> List.fold Set.union Set.empty

/// Every projected controller's Upgrade Work Area, unioned — the tiles a
/// creep can upgrade from, behind dualSeats and controllerContainers.
let private upgradeArea (atlas: Atlas) : Set<Pos> =
    targetsOfKind atlas Controller
    |> List.map (Upgrade >> workArea atlas)
    |> List.fold Set.union Set.empty

/// The working ground of the room (ADR 0022): every projected source's
/// Seats plus every projected controller's Upgrade Work Area — the tiles
/// the colony works from. Off-limits to the Layout's clustered ordering: a
/// tower or extension there eats a tile an Anchor or an upgrader stands
/// on. Total: a room with neither kind of geometry answers with the empty
/// set, which reserves nothing rather than blocking every tile (ADR
/// 0004). Derived fresh each tick, never persisted.
let workingGround (atlas: Atlas) : Set<Pos> =
    Set.union (seatUnion atlas) (upgradeArea atlas)

/// Dual Seats of the room: tiles inside both some projected source's Seats
/// and a projected controller's Upgrade Work Area — a creep standing on one
/// harvests and upgrades without ever moving. Total: a room with no
/// controller, no sources, or a disjoint pair answers with the empty set,
/// which never punishes anything (ADR 0004). Derived fresh each tick,
/// never persisted.
let dualSeats (atlas: Atlas) : Set<Pos> =
    Set.intersect (seatUnion atlas) (upgradeArea atlas)

/// Posts of the room: the tiles worth garrisoning with a heavy-WORK body
/// (ADR 0012) — the Dual Seats plus every Seat under a built container
/// (sites don't count: a pending container catches no overflow). A
/// Seat-standing container is a source container by the Layout's
/// geometry — a controller container's tile that were also a Seat would
/// already be a Dual Seat. The
/// capacity unit of the Anchor quota. Total: a room with neither kind
/// answers with the empty set (ADR 0004). Derived fresh each tick, never
/// persisted.
let posts (atlas: Atlas) : Set<Pos> =
    Set.intersect (seatUnion atlas) (containerTiles atlas)
    |> Set.union (dualSeats atlas)

/// The Posts of one source: its own Seats that are Posts. Empty for a
/// source the projection does not place, and for one with neither a built
/// container on a Seat nor a Dual Seat.
let postsOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    Set.intersect (seatTilesOf atlas sourceId) (posts atlas)

/// Work Area of a Task for one creep — the body-aware query every reader
/// that has a creep uses (ADR 0020). Ordinarily the Task's own area, but
/// Harvest for a Work-heavy body is narrowed to that source's Posts when
/// the source has any: a heavy body digs from the tile that catches its
/// overflow or lets it upgrade in place, and travel cost walks it there
/// rather than leaving it on whichever Seat it happened to land on. A
/// source with no Post narrows nothing — the fallback that carries the
/// colony before the first container is built. A source that has a Post
/// narrows to it even when the projection blocks it: an area with nothing
/// standable in it makes the Task inapplicable, exactly as an unreachable
/// one does for every Task, rather than silently widening back to the
/// Seats (ADR 0020). Only Harvest narrows, so
/// this never re-enters the `posts` derivation that reads the Upgrade
/// area. Memoised per Task for the tick beside the unnarrowed areas.
let workAreaFor (atlas: Atlas) (creep: string) (task: Task) : Set<Pos> =
    match task with
    | Harvest sourceId when workHeavy atlas creep ->
        memoised atlas.HeavyAreas task (fun () ->
            let postTiles = postsOf atlas sourceId

            if Set.isEmpty postTiles then
                workArea atlas task
            else
                Set.intersect (workArea atlas task) postTiles)
    | _ -> workArea atlas task

/// The controller's upgrade buffers, by id: built containers standing
/// inside a controller's Upgrade Work Area and on no source's Seat — the
/// Layout places one (ADR 0012), and the Withdraw gate reads it (ADR
/// 0019). The Planner spells the same judgement out again over the
/// Snapshot for its Refill layering; the two agree on every tile a
/// container can stand on and are not the same predicate off it — an
/// accepted duplication, named in ADR 0019. Total: a room with no
/// controller, none placed, or no built container answers with the empty
/// set, which opens the gate rather than closing it — unplaceable
/// geometry never costs a creep a Task (ADR 0004).
let controllerContainers (atlas: Atlas) : Set<string> =
    match atlas.Buffers with
    | Some memo -> memo
    | None ->
        let area = upgradeArea atlas
        let seats = seatUnion atlas

        let buffers =
            targetsOfKind atlas (Structure BuiltKind.Container)
            |> List.filter (fun id ->
                match Map.tryFind id atlas.Spatial.TargetPositions with
                | Some pos -> Set.contains pos area && not (Set.contains pos seats)
                | None -> false)
            |> Set.ofList

        atlas.Buffers <- Some buffers
        buffers

/// Whether a creep's tile catches its harvest overflow: a built container
/// standing on one of the source's own Seats — the container Post's
/// footing, judged from the same census `posts` reads (ADR 0012). There
/// the engine drops harvest past a full store into the container under
/// the creep, so a full store never ends the dig. A site catches nothing,
/// and an unplaced creep or source widens nothing — absence of geometry
/// leaves the ordinary full-store rule standing rather than blocking a
/// Task, keeping the query total (ADR 0004).
let catchesOverflow (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    match Map.tryFind creep atlas.Spatial.CreepPositions with
    | None -> false
    | Some pos ->
        Set.contains pos (containerTiles atlas)
        && Set.contains pos (seatTilesOf atlas sourceId)

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
        let area = workAreaFor atlas creep task

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

/// Whether a creep may perform its Task's action this tick: standing
/// inside the Task's Work Area for its body at tick start (ADR 0020) — a
/// creep acts only from where it may stand, which for every ordinary Task
/// is the passable tiles within the action's range, and for a Work-heavy
/// harvester is its Post. The gate is what keeps such a body empty on the
/// way to its Post, so a full store never ends the walk. Two permissive
/// escapes keep the query total (ADR 0004): a creep or target the
/// projection cannot place never blocks the action, and neither does a
/// creep standing on a tile the projection calls impassable — an
/// obstacle-type construction site dropped under a standing creep, which
/// the engine lets stay — judged by range as before.
let mayAct (atlas: Atlas) (creep: string) (task: Task) : bool =
    match
        Map.tryFind creep atlas.Spatial.CreepPositions,
        Map.tryFind (targetOf task) atlas.Spatial.TargetPositions
    with
    | Some creepPos, Some targetPos ->
        if (stepCost atlas.Spatial creepPos).IsNone then
            range creepPos targetPos <= actionRange task
        else
            Set.contains creepPos (workAreaFor atlas creep task)
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
        let goals = workAreaFor atlas creep task

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
/// share (ADR 0008, ADR 0009). This is the one flood ADR 0004's memo
/// cannot serve — each creep's tile is its own key — so the Resolver runs
/// it only for creeps on the verbose list (ADR 0018).
let firstStepIgnoringTraffic (atlas: Atlas) (creep: string) (task: Task) : Pos option =
    firstStepVia atlas (floodFrom atlas.Weights noTraffic (factorOf atlas creep)) creep task

/// Round-trip haul cost in whole ticks for a body between a container's
/// tile and a sink structure's tile (ADR 0012): the leg out prices every
/// Carry part loaded, the leg back prices them all empty, both floods over
/// the same weights as travel cost — a road discounts a road-parity body
/// exactly as it discounts travel — but traffic-blind: the hauler quota
/// this feeds is capacity planning, not routing, and today's standing
/// creeps must never resize the fleet. Goals are the sink's adjacent
/// walkable tiles (transfer acts at range 1); the origin prices 0 as every
/// flood origin does. Cost units are half-ticks; the two legs sum and
/// round up. None when no goal is reachable — unpriceable geometry hires
/// nobody (ADR 0004).
let haulRoundTripTicks (atlas: Atlas) (body: BodyPart list) (from: Pos) (sink: Pos) : int option =
    let count part =
        body |> List.filter ((=) part) |> List.length

    let goals = adjacentWalkable atlas sink

    let legUnits factor =
        let dist, _ = floodFrom atlas.Weights noTraffic factor from

        goals
        |> List.choose (fun goal ->
            let d = dist.[indexOf goal]
            if d = unreached then None else Some d)
        |> function
            | [] -> None
            | costs -> Some(List.min costs)

    let loaded =
        legUnits
            {
                FatigueParts = List.length body - count Move
                MoveParts = count Move
            }

    let empty =
        legUnits
            {
                FatigueParts = List.length body - count Move - count Carry
                MoveParts = count Move
            }

    match loaded, empty with
    | Some out, Some back -> Some((out + back + 1) / 2)
    | _ -> None

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
