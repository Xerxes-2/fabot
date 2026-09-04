module Fabot.Core.Atlas

open Fabot.Core.Types
open Fable.Core

/// Which of the tick's floods a memo entry holds (ADR 0029, widened by
/// ADR 0030). One dimension separates them because they differ in the two
/// ways their readers differ — granularity and traffic — and no reader
/// wants a combination the dimension cannot name: travel cost is a
/// ranking price, so it wants sub-tick granularity and wants to see
/// today's crowds; the walk is a clock, so it wants a whole tick a tile
/// and wants to be blind to them; the baseline is the ranking price over
/// empty ground, which is what makes a detour attributable. Widening the
/// memo's key by this one dimension, rather than laying a second map
/// beside it, is what keeps the floods from drifting apart.
type private Pricing =
    /// Travel cost's units — half-ticks, floored at one unit a step, with
    /// the occupancy surcharge on occupied tiles (ADR 0010, ADR 0008).
    /// The ranking price: it breaks rank ties in the Matcher.
    | TravelCost
    /// The walk's whole ticks — floored at one tick a step, traffic-blind
    /// (ADR 0029). The clock: the horizon every time-aware judgement is
    /// made at.
    | Walk
    /// Travel cost's own units over empty ground (ADR 0030): the route the
    /// body would take were no tile occupied. The baseline the occupancy
    /// surcharge is judged against — it differs from TravelCost in traffic
    /// alone, which is what lets the reroute attribution blame the
    /// difference on traffic and nothing else (ADR 0008, ADR 0009).
    | Baseline

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
            /// Step weight per tile index, laid once a tick for the
            /// flood's hot loop: -1 impassable, else the terrain weight.
            /// stepCost's rules over the whole room, reached by walking
            /// the projection's collections rather than by querying it a
            /// tile at a time (#96) — the same rules, not the same code,
            /// so the two are held together by terrainWeight and by the
            /// road and obstacle precedence ofSnapshot spells out.
            Weights: int[]
            /// Whether a creep stands on each tile index this tick; the
            /// flood prices these tiles dearer so paths detour around
            /// standing traffic.
            Occupied: bool[]
            /// Memoised Dijkstra flood per placed creep's tile, fatigue
            /// factor and pricing, forced at most once per tick and shared
            /// by every query pricing from it (ADR 0002, extended to the
            /// whole tick). Bodies of the same factor at the same tile
            /// share one flood. One entry per pricing per creep since ADR
            /// 0029 — the ranking price travel cost ranks on, the clock the
            /// walk reads, and since ADR 0030 the baseline the reroute
            /// attribution compares against — laid lazily, so a tick that
            /// asks for one of them pays for one. Each flood is a distance
            /// and a predecessor-index array over tile indices.
            Floods: Map<Pos * FatigueFactor * Pricing, Lazy<int[] * int[]>>
            /// Memoised traffic-blind flood out of a spawner's tile, per
            /// (spawner tile, fatigue factor), for bodies the Snapshot does
            /// not carry: a lead prices a replacement that has not been
            /// cast yet (ADR 0026), so its factor is in no creep's entry
            /// and the Floods memo cannot be laid for it in advance. One
            /// flood per row per spawn, however many creeps that row is
            /// deriving a lead for — and, since ADR 0032, one per census
            /// rather than one per tick: this is the table the Atlas is
            /// handed rather than one it lays, recalled from the plan memo
            /// while the census signature holds and dropped whole when it
            /// moves. Every input the flood reads is in that
            /// signature — the weights, and the successor's body through
            /// the Capacity that sizes it (ADR 0017) — so a recalled entry
            /// is the entry this tick would have flooded. The hauler
            /// quota's round trip, the other traffic-blind query, keeps its
            /// own uncached floods: its origins are containers rather than
            /// spawners, so it shares no key, and it is itself memoised on
            /// the census signature — it runs only when the room changes. A
            /// mutable table for the same reason WorkAreas is one. Priced
            /// in the walk's whole ticks (ADR 0029), like every clock in
            /// the colony; it is the origin set, never the pricing, that
            /// keeps this memo beside the Floods table rather than inside
            /// it.
            Walks: WalkTable
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

/// No tile occupied: the flood baseline the occupancy surcharge is judged
/// against — the ground the walk is priced over (ADR 0029), and the ground
/// the `Baseline` pricing the attribution compares against is priced over
/// (ADR 0030).
let private noTraffic: bool[] = Array.create tileCount false

let private neighbours pos =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if (dx, dy) <> (0, 0) then
                    { X = pos.X + dx; Y = pos.Y + dy }
    ]

/// The weight of raw ground (ADR 0010): plain 2, swamp 10, wall
/// impassable — written as the -1 the flood's weight table marks an
/// impassable tile with. The one place the engine's terrain prices live:
/// stepCost's single-tile answer, the table the Atlas lays and the
/// trunk's raw-terrain flood all price off it, which is what keeps them
/// from drifting apart.
let private terrainWeight terrain =
    match terrain with
    | Plain -> 2
    | Swamp -> swampWeight
    | Wall -> -1

/// Cost of stepping onto a tile — the engine's own per-part fatigue
/// values (ADR 0010): road 1, plain 2, swamp 10; walls, obstacle
/// structures and tiles outside the projection are impassable (ADR 0001).
/// A built road overrides the terrain under it; a road on a wall (a
/// tunnel) is not modeled and stays impassable. The single-tile query —
/// the flood reads the table the Atlas lays to the same rules.
let private stepCost (spatial: SpatialInfo) tile =
    if Set.contains tile spatial.Obstacles then
        None
    else
        match Map.tryFind tile spatial.Terrain |> Option.map terrainWeight with
        | Some weight when weight > 0 -> Some(if Set.contains tile spatial.Roads then 1 else weight)
        | _ -> None

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

/// The fatigue factor of a body list carrying nothing — the shape a body
/// leaves the spawner in and comes back to a container in. Beside
/// fatigueFactorOf, which reads a living creep's parts and the load it is
/// carrying right now; this one reads a body the projection carries no
/// creep for: the hauler quota's candidate body (ADR 0012) and the
/// replacement a lead prices (ADR 0026).
let private emptyFactorOf (body: BodyPart list) : FatigueFactor =
    let count part =
        body |> List.filter ((=) part) |> List.length

    {
        FatigueParts = List.length body - count Move - count Carry
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

/// Whole ticks the body needs to step onto a tile of the given terrain
/// weight — the walk's price (ADR 0029). Two cost units make a tick and a
/// part of one still costs a whole tick, so the unit price rounds up; and
/// no step costs less than a tick, because no body crosses a tile faster
/// than that however much Move it carries. The nested rounding is exact
/// rather than an approximation — ceil(ceil(w·F / M) / 2) = ceil(w·F /
/// 2M), the fatigue the step generates over what the body pays off in a
/// tick — so this is the physical time of the step, which is why the
/// per-step floor belongs here and not on the total (#79). The outer floor
/// is written as ADR 0029 states the rule; `stepUnits`' own floor of one
/// unit already implies it, and spelling it out is what keeps the two
/// floors from having to be read together. A body without Move parts steps
/// nowhere, exactly as travel cost has it.
let private stepTicks (factor: FatigueFactor) weight =
    stepUnits factor weight |> Option.map (fun units -> max 1 ((units + 1) / 2))

/// The flood's array accessors: checked on .NET (the F# body is the
/// ordinary index, so `dotnet test` runs the flood bounds-checked) and a
/// bare JS index under Fable, where the `[<Emit>]` template replaces the
/// call. Fable 4.12+ compiles every `arr.[i]` to a helper that re-tests
/// the index and carries a throw path, and offers no switch to drop it;
/// in the flood that helper was ~28% of the tick (#91), re-checking
/// indices the loop has already proven in range — a neighbour index is
/// built only after the `0 <= n < roomSide` guard, and the heap's come
/// from its own `Count`. Used only inside `floodFromAll`; every other
/// array read in the Atlas stays checked, since none is on the profile.
[<Emit("$1[$0]")>]
let private at (index: int) (array: int[]) : int = array.[index]

[<Emit("$1[$0] = $2")>]
let private setAt (index: int) (array: int[]) (value: int) : unit = array.[index] <- value

[<Emit("$1[$0]")>]
let private flagAt (index: int) (array: bool[]) : bool = array.[index]

[<Emit("$1[$0]")>]
let private heapAt (index: int) (heap: ResizeArray<int>) : int = heap.[index]

[<Emit("$1[$0] = $2")>]
let private setHeapAt (index: int) (heap: ResizeArray<int>) (value: int) : unit =
    heap.[index] <- value

/// Dijkstra flood over the weight grid from every tile in `starts`,
/// priced by `stepPrice` — one body's price for a step onto a tile of a
/// given terrain weight, and, beside the occupancy the caller passes, the
/// only thing that differs between the tick's floods (ADR 0029, ADR 0030):
/// cheapest cost to every reachable tile (`unreached` elsewhere), plus
/// each tile's predecessor index on a
/// cheapest path (-1 elsewhere). This is the tick's hottest loop, so it
/// runs on flat arrays with a binary min-heap of dist-then-index keys —
/// the key ordering also keeps tie-breaking deterministic. Every start
/// tile costs 0 even when it cannot be stepped onto — a creep already
/// stands there, or is about to be placed there. Several starts price a
/// body that may begin anywhere in a set at no step's cost, which is how
/// a spawner places a finished creep beside itself (ADR 0026). A tile
/// marked occupied costs occupancyPenalty extra, so paths detour around
/// standing traffic when a detour is cheaper. The penalty is a number of
/// cost units, so a caller pricing steps in anything else must pass no
/// occupancy at all: every traffic-blind caller here passes `noTraffic`,
/// and for the tick's memoised floods `floodPriced` pairs the two choices
/// per pricing, so neither can be made without the other.
let private floodFromAll
    (weights: int[])
    (occupied: bool[])
    (stepPrice: int -> int option)
    (starts: Pos list)
    : int[] * int[] =
    let dist = Array.create tileCount unreached
    let parents = Array.create tileCount -1

    // Binary min-heap over dist * tileCount + index: one int per entry.
    let heap = ResizeArray<int>()

    let swap i j =
        let t = heapAt i heap
        setHeapAt i heap (heapAt j heap)
        setHeapAt j heap t

    let push key =
        heap.Add key
        let mutable i = heap.Count - 1

        while i > 0 && heapAt ((i - 1) / 2) heap > heapAt i heap do
            swap ((i - 1) / 2) i
            i <- (i - 1) / 2

    let pop () =
        let top = heapAt 0 heap
        setHeapAt 0 heap (heapAt (heap.Count - 1) heap)
        heap.RemoveAt(heap.Count - 1)
        let mutable i = 0
        let mutable sinking = true

        while sinking do
            let l = 2 * i + 1
            let r = 2 * i + 2
            let mutable smallest = i

            if l < heap.Count && heapAt l heap < heapAt smallest heap then
                smallest <- l

            if r < heap.Count && heapAt r heap < heapAt smallest heap then
                smallest <- r

            if smallest = i then
                sinking <- false
            else
                swap i smallest
                i <- smallest

        top

    for start in starts do
        let startIndex = indexOf start

        // Checked: a start is the caller's Pos, not an index the flood
        // built, so this is the one access the in-range argument for the
        // accessors above does not cover — and it runs once per start.
        if dist.[startIndex] <> 0 then
            dist.[startIndex] <- 0
            push startIndex

    while heap.Count > 0 do
        let key = pop ()
        let index = key % tileCount
        let d = key / tileCount

        // Stale heap entry when unequal: the tile was reached cheaper meanwhile.
        if at index dist = d then
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
                        let weight = at next weights

                        if weight >= 0 then
                            match stepPrice weight with
                            | None -> ()
                            | Some step ->
                                let candidate =
                                    d
                                    + step
                                    + (if flagAt next occupied then occupancyPenalty else 0)

                                if candidate < at next dist then
                                    setAt next dist candidate
                                    setAt next parents index
                                    push (candidate * tileCount + next)

    dist, parents

/// The one-origin flood every query but the lead's walk wants: a creep
/// prices from the tile it stands on.
let private floodFrom weights occupied stepPrice (start: Pos) =
    floodFromAll weights occupied stepPrice [ start ]

/// The walk's flood over one body, from anywhere in `starts` (ADR 0029):
/// whole ticks a step and blind to today's traffic. The one place the
/// walk's two differences from travel cost are spelled out, so no reader
/// can take one without the other — every clock in the colony floods
/// through here, however its origins are chosen.
let private walkFloodFromAll weights factor (starts: Pos list) =
    floodFromAll weights noTraffic (stepTicks factor) starts

/// The one-origin walk: a creep, or a container, prices from the tile it
/// sits on.
let private walkFloodFrom weights factor (start: Pos) =
    walkFloodFromAll weights factor [ start ]

/// The flood one pricing wants over one body: the ranking price sees
/// today's traffic and counts half-ticks, the clock is blind to it and
/// counts whole ticks (ADR 0029), and the baseline counts the ranking
/// price's own half-ticks with the crowd taken out (ADR 0030). The one
/// place the set is laid side by side, so the memo cannot hold one where
/// a reader expects another.
let private floodPriced weights occupied factor pricing (start: Pos) =
    match pricing with
    | TravelCost -> floodFrom weights occupied (stepUnits factor) start
    | Walk -> walkFloodFrom weights factor start
    | Baseline -> floodFrom weights noTraffic (stepUnits factor) start

/// The Atlas over a Snapshot, recalling a spawn walk table rather than
/// laying an empty one (ADR 0032). The caller hands the table the plan
/// memo carried when the census signature is unchanged, and a fresh one
/// when it moved: every entry is a pure function of the census, so a
/// recalled flood is the flood this tick would have run, and a table
/// dropped at a signature change leaves nothing stale to reason about.
/// Every other table here is still laid empty — they key on this tick's
/// creeps, or on this tick's traffic.
let ofSnapshotRecalling (walks: WalkTable) (snapshot: Snapshot) : Atlas =
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

    // The flood's weight table, filled by walking the three collections
    // rather than by asking stepCost per tile. Walking a tree compares
    // nothing; only a lookup does, and the per-tile form cost three
    // Pos-keyed lookups a tile — 2500 tiles' worth of structural
    // comparison, the largest single cost in the tick (#96). The passes
    // layer in stepCost's own precedence: terrain first, then roads over
    // the passable ground they discount, then obstacles over everything.
    // The array's initial -1 is the answer for every tile outside the
    // projection, which is stepCost's answer for one too.
    let weights = Array.create tileCount -1

    spatial.Terrain
    |> Map.iter (fun tile terrain -> weights.[indexOf tile] <- terrainWeight terrain)

    // A road discounts the ground under it, never ground the projection
    // calls impassable: a road on a wall (a tunnel, which ADR 0010 does
    // not model) or off the terrain projection stays impassable.
    spatial.Roads
    |> Set.iter (fun tile ->
        let index = indexOf tile

        if weights.[index] > 0 then
            weights.[index] <- 1)

    spatial.Obstacles |> Set.iter (fun tile -> weights.[indexOf tile] <- -1)

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
            |> List.collect (fun (name, pos) ->
                let factor = Map.find name factors

                [ TravelCost; Walk; Baseline ]
                |> List.map (fun pricing ->
                    (pos, factor, pricing), lazy (floodPriced weights occupied factor pricing pos)))
            |> Map.ofList
        Walks = walks
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

/// The Atlas over a Snapshot with nothing recalled: a fresh spawn walk
/// table, filled from scratch as this tick prices its leads. The tick loop
/// always has a memo to hand over, so this is the shape a reader building
/// an Atlas over a Snapshot alone — a test, or a one-off — asks for.
let ofSnapshot (snapshot: Snapshot) : Atlas =
    ofSnapshotRecalling (WalkTable()) snapshot

/// A copy of the step weight per tile index — the grid every flood in the
/// Atlas prices from, -1 impassable. Read by the census guard (ADR 0032)
/// and nothing else: the spawn walks are recalled across ticks on the
/// census signature alone, so two Snapshots the signature calls equal have
/// to lay the same grid, and a weights input the signature misses would
/// price leads off a stale one until a global reset. A copy because the
/// grid is the flood's own working state: read it, never hold it.
let stepWeights (atlas: Atlas) : int[] = Array.copy atlas.Weights

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

/// The memoised flood for a creep from a tile, under one pricing; placed
/// creeps' own tiles hit the memo.
let private flood (atlas: Atlas) (pricing: Pricing) (creep: string) (pos: Pos) =
    let factor = factorOf atlas creep

    match Map.tryFind (pos, factor, pricing) atlas.Floods with
    | Some memo -> memo.Value
    | None -> floodPriced atlas.Weights atlas.Occupied factor pricing pos

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

/// Storages already standing in the room — at most one, but counted the
/// way the tower and the extensions are so one gap rule sizes every kind
/// the ordering picks for (ADR 0022).
let builtStorages (atlas: Atlas) : int =
    targetsOfKind atlas (Structure BuiltKind.Storage) |> List.length

/// Storage construction sites already placed in the room.
let pendingStorages (atlas: Atlas) : int =
    targetsOfKind atlas (Site BuiltKind.Storage) |> List.length

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

/// Tiles of every placed target whose kind answers a predicate — the one
/// join between the projection's two maps, for the censuses read as tiles
/// rather than as counts.
let private tilesWhere (atlas: Atlas) (matches: TargetKind -> bool) : Set<Pos> =
    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if matches kind then
            Map.tryFind id atlas.Spatial.TargetPositions
        else
            None)
    |> Set.ofList

/// Tiles of every placed target of one kind.
let private tilesOfKind (atlas: Atlas) (kind: TargetKind) : Set<Pos> = tilesWhere atlas ((=) kind)

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

/// Tiles holding a built Storage — the tile a Link footing is anchored on
/// once the reservation has become a structure (ADR 0022).
let storageTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Structure BuiltKind.Storage)

/// Tiles holding a Storage construction site — the same anchor while the
/// site is still being built.
let pendingStorageTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Site BuiltKind.Storage)

/// Tiles holding a standing rampart — the covering census (ADR 0034): a
/// tile already ramparted needs no rampart site. Ownership is not asked,
/// unlike the hits (which are ours alone): a tile takes one rampart
/// whoever raised it, so a foreign one left over in a room we took is a
/// tile the engine would refuse a second site on anyway.
let rampartTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Structure BuiltKind.Rampart)

/// Tiles holding a standing rampart of ours — the same census asked with
/// ownership on (ADR 0033). The projection carries hits for an ownable
/// kind only when it is ours (ADR 0034), so the hits are what tell our
/// rampart from one somebody else left standing in a room we took: cover
/// for our creeps is cover we own, while the covering census above, which
/// only asks whether a tile can take another rampart, is right to ignore
/// the question.
let ourRampartTiles (atlas: Atlas) : Set<Pos> =
    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if kind = Structure BuiltKind.Rampart && Map.containsKey id atlas.Spatial.Hits then
            Map.tryFind id atlas.Spatial.TargetPositions
        else
            None)
    |> Set.ofList

/// Tiles holding a rampart construction site — the census's pending half,
/// exactly as a road's is: a site standing there is not yet cover, but its
/// tile needs no second site.
let pendingRampartTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Site BuiltKind.Rampart)

/// Tiles holding a standing Keep structure — the spawn, the tower and the
/// Storage (ADR 0034): what a rampart covers, the tick the structure
/// stands. A site is not covered until it is a structure.
let keepTiles (atlas: Atlas) : Set<Pos> =
    tilesWhere atlas (function
        | Structure built -> isKeep built
        | _ -> false)

/// Tiles holding a standing link. A link is a target, so its tile is no
/// longer buildable; the Layout adds these back as footing candidates so
/// a footing does not jump the tick its link goes up (ADR 0022).
let linkTiles (atlas: Atlas) : Set<Pos> =
    tilesOfKind atlas (Structure BuiltKind.Link)

/// Whether a tile's terrain is swamp; a tile outside the projection is not.
let isSwamp (atlas: Atlas) (tile: Pos) : bool =
    Map.tryFind tile atlas.Spatial.Terrain = Some Swamp

/// Walkable tiles adjacent to `pos`, in deterministic (X, Y) order.
/// Standing respects obstacles, unlike Seat counting.
let adjacentWalkable (atlas: Atlas) (pos: Pos) : Pos list =
    neighbours pos |> List.filter (fun tile -> (stepCost atlas.Spatial tile).IsSome)

/// Every tile of the room a creep may stand on — `adjacentWalkable`'s
/// answer over the whole projection, read off the weight grid rather than
/// a tile at a time: the same rules, held together the way the grid itself
/// is (`stepCost`, and the road and obstacle precedence `ofSnapshot`
/// spells out). The room-wide half nothing wanted until a Task's Work
/// Area was the room itself (ADR 0033).
let walkableTiles (atlas: Atlas) : Set<Pos> =
    Set.ofList
        [
            for index in 0 .. tileCount - 1 do
                if at index atlas.Weights >= 0 then
                    posAt index
        ]

/// The tile a creep stands on; None for a creep the projection does not
/// place. What a judgement about where a creep *is* reads — as
/// `positionOf` is the same question about a target.
let creepTile (atlas: Atlas) (creep: string) : Pos option =
    Map.tryFind creep atlas.Spatial.CreepPositions

/// What a Task acts on, and the Chebyshev range its action reaches from
/// (Screeps: harvest, withdraw and transfer act at range 1; build, repair
/// and upgrade at range 3) — the one pair every geometry query starts
/// from. None for a Task that acts on nothing: Flee has no target and no
/// action (ADR 0033), so no area of the projection's own is derived for
/// it and no action is ever permitted.
let private actionOn =
    function
    | Harvest id
    | Withdraw id
    | Refill id -> Some(id, 1)
    | Build id
    | Repair id
    | Upgrade id -> Some(id, 3)
    | Flee -> None

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

/// The Work Area geometry behind `workArea`: the passable tiles within
/// the action's range of its target. Empty for a Task the projection
/// cannot place a target for — and for Flee, which has no target at all:
/// the tiles outside every Reach are a colony fact the decision layer
/// derives and hands its mover, never geometry the projection carries
/// (ADR 0033).
let private buildWorkArea (atlas: Atlas) (task: Task) : Set<Pos> =
    match actionOn task with
    | None -> Set.empty
    | Some(targetId, r) ->
        match Map.tryFind targetId atlas.Spatial.TargetPositions with
        | None -> Set.empty
        | Some target ->
            Set.ofList
                [
                    for x in target.X - r .. target.X + r do
                        for y in target.Y - r .. target.Y + r do
                            let tile = { X = x; Y = y }

                            if (stepCost atlas.Spatial tile).IsSome then
                                tile
                ]

/// Build-once-per-tick over one of the Atlas's mutable tables: the shape
/// every key set the Snapshot does not carry is memoised through — Work
/// Areas, their Work-heavy narrowing, and the lead's walks. No reader can
/// observe whether the answer was built or recalled, and the Atlas is
/// rebuilt every tick, so each table is per-tick by construction.
let private memoised
    (table: System.Collections.Generic.Dictionary<'key, 'value>)
    (key: 'key)
    (build: unit -> 'value)
    : 'value =
    match table.TryGetValue key with
    | true, value -> value
    | _ ->
        let value = build ()
        table.[key] <- value
        value

/// Work Area of a Task, body-blind: the passable tiles within the action's
/// range of its target. The base geometry `posts` itself is derived from
/// (through the controller's Upgrade area), so it stays a pure function of
/// the Task; readers that hold a creep want `workAreaFor`, which narrows it
/// for a Work-heavy harvester (ADR 0020). Empty when the
/// projection cannot place the target. Memoised per Task for the tick: the
/// same area is asked for once per creep the Matcher prices and again by
/// the Emitter and Resolver.
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

/// Tiles holding a standing container on a Post — the tiles a work-heavy
/// body garrisons and cannot flee from (ADR 0033), ramparted beside the
/// Keep (ADR 0034). A Post that is a bare Dual Seat is not one of these:
/// what the rule covers is a structure standing, and there is none there.
let postContainerTiles (atlas: Atlas) : Set<Pos> =
    Set.intersect (containerTiles atlas) (posts atlas)

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

/// A room's place on the world grid, read off its name — `W12S28` is
/// (-13, 28). West and North count outward from the origin, so they run
/// negative (`W n` is x = -n-1, `N n` is y = -n-1) and East and South run
/// straight up, which turns "are these two rooms neighbours, and across
/// which border" into subtraction. None for a name outside the engine's
/// grammar, which is unplaceable geometry like any other (ADR 0004).
///
/// The Seam reads adjacency out of the two names rather than taking a
/// border direction from its caller, and that is the decision this query
/// makes: which edge two rooms share is already a fact about their names,
/// so a caller that declared it separately could declare it wrong — an
/// outpost constant carrying a room name *and* an edge is two facts that
/// can disagree, and the disagreement would silently build a band out of
/// two rooms' opposite walls. The arithmetic is the engine's own and
/// costs these few lines once; a second field on every outpost, and a
/// rule for what to do when it contradicts the name, costs forever.
let private worldCoordsOf (roomName: string) : (int * int) option =
    let isDigit index =
        index < roomName.Length && roomName.[index] >= '0' && roomName.[index] <= '9'

    let rec endOfDigits index =
        if isDigit index then endOfDigits (index + 1) else index

    let number start stop =
        if stop <= start then
            None
        else
            let mutable value = 0

            for index in start .. stop - 1 do
                value <- value * 10 + (int roomName.[index] - int '0')

            Some value

    // Outward from the origin is negative, towards it positive.
    let axis letter outward inward value =
        if letter = outward then Some(-value - 1)
        elif letter = inward then Some value
        else None

    let xEnd = endOfDigits 1
    let yEnd = endOfDigits (xEnd + 1)

    if yEnd <> roomName.Length then
        None
    else
        match number 1 xEnd, number (xEnd + 1) yEnd with
        | Some x, Some y ->
            match axis roomName.[0] 'W' 'E' x, axis roomName.[xEnd] 'N' 'S' y with
            | Some worldX, Some worldY -> Some(worldX, worldY)
            | _ -> None
        | _ -> None

/// The far exit row and column of a room — index 49, the outer of the two
/// the projection's ground stops short of (ADR 0036).
let private exitEdge = roomSide - 1

/// The tile pairs the engine joins across the border two rooms share,
/// before terrain has a say: this room's exit tile beside the tile a
/// creep stepping onto it lands on, which is the same coordinate on the
/// opposite row or column. `offset` is the neighbour's world position
/// minus this room's, so only the four unit steps name a shared border —
/// a diagonal pair touches at a corner the engine joins nothing across,
/// and anything further apart shares no border at all. The four corner
/// tiles are left out of every row and column: a corner lies on two
/// borders at once, so offering it would hand the same tile two different
/// landings, and the engine makes at most one of them. Every room the
/// engine generates walls its corners, so this drops no crossing that
/// exists — it declines to invent one where the terrain cannot say (ADR
/// 0004). Listed in (X, Y) order, which is the order the band answers in.
let private borderPairs offset : (Pos * Pos) list =
    let alongEdge = [ 1 .. exitEdge - 1 ]

    match offset with
    | 0, -1 -> [ for x in alongEdge -> { X = x; Y = 0 }, { X = x; Y = exitEdge } ]
    | 0, 1 -> [ for x in alongEdge -> { X = x; Y = exitEdge }, { X = x; Y = 0 } ]
    | -1, 0 -> [ for y in alongEdge -> { X = 0; Y = y }, { X = exitEdge; Y = y } ]
    | 1, 0 -> [ for y in alongEdge -> { X = exitEdge; Y = y }, { X = 0; Y = y } ]
    | _ -> []

/// The Seam band joining two rooms: the passable exit-tile pairs, each
/// this room's border tile beside the tile it lands a creep on in the
/// neighbour (ADR 0041). The third kind of geometry beside the Seat and
/// the Post — those are tiles a creep works from, a Seam is one it can
/// only pass through — and never a tile anything offers to stand on: it
/// is answered from the projection's border layer, which enters no weight
/// grid, no walkable or buildable set and no Work Area, so the Matcher
/// cannot pick one and have the engine empty it the tick a creep arrives.
/// A pair is in the band when neither side is wall; a swamp exit is in
/// it, dearly, exactly as swamp ground is. Deterministic (X, Y) order.
/// Total (ADR 0004): two rooms that are not orthogonal neighbours, and a
/// room the projection carries no border for, answer with the empty
/// band — an unpriceable Seam is no Seam, never a blocked one, so it
/// costs nothing and blocks nothing.
let seams (atlas: Atlas) (fromRoom: string) (toRoom: string) : (Pos * Pos) list =
    let passable (ring: Map<Pos, Terrain>) tile =
        match Map.tryFind tile ring with
        | Some terrain -> terrainWeight terrain > 0
        | None -> false

    match
        Map.tryFind fromRoom atlas.Spatial.Borders,
        Map.tryFind toRoom atlas.Spatial.Borders,
        worldCoordsOf fromRoom,
        worldCoordsOf toRoom
    with
    | Some near, Some far, Some(hereX, hereY), Some(thereX, thereY) ->
        borderPairs (thereX - hereX, thereY - hereY)
        |> List.filter (fun (here, there) -> passable near here && passable far there)
    | _ -> []

/// The cheapest path from a creep to a set of tiles under one pricing —
/// the shape travel cost and the walk share, so the two can disagree on
/// what a step costs and on nothing else (ADR 0029). The tiles are the
/// caller's, not a Task's: what a creep may stand on this tick is the
/// decision layer's judgement, which takes a Reach out of a Work Area and
/// gives Flee an area of its own (ADR 0033). A creep the projection
/// cannot place prices at 0; an empty or unreachable set has no price at
/// all. Its totality contract is ADR 0004's and is documented on every
/// wrapper.
let private pricedPathTo
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (area: Set<Pos>)
    : int option =
    match Map.tryFind creep atlas.Spatial.CreepPositions with
    | None -> Some 0
    | Some pos ->
        if Set.isEmpty area then
            None
        elif Set.contains pos area then
            Some 0
        else
            let dist, _ = flood atlas pricing creep pos

            area
            |> Set.toList
            |> List.choose (fun tile ->
                let d = dist.[indexOf tile]
                if d = unreached then None else Some d)
            |> function
                | [] -> None
                | costs -> Some(List.min costs)

/// The same path priced for a Task: over the Task's own Work Area, and
/// with the one escape a bare tile set cannot carry — a target the
/// projection does not place prices at 0 rather than reading as
/// unreachable geometry (ADR 0004).
let private pricedPath (atlas: Atlas) (pricing: Pricing) (creep: string) (task: Task) : int option =
    match actionOn task with
    | Some(targetId, _) when (Map.tryFind targetId atlas.Spatial.TargetPositions).IsNone -> Some 0
    | _ -> pricedPathTo atlas pricing creep (workAreaFor atlas creep task)

/// Travel cost of a Task for a creep (ADR 0002, revised by ADRs 0006 and
/// 0010): the cost units — half-ticks — the creep's body needs along a
/// cheapest path to any Work Area
/// tile — terrain weights scaled by the body's fatigue factor, tiles
/// under standing creeps priced occupancyPenalty dearer — 0 for a creep
/// already inside. None — a placed Work Area the creep cannot reach
/// (a body without Move parts reaches nothing), or an empty one — makes
/// the Task inapplicable to that creep. An unplaced creep or target
/// prices at 0: unpriceable geometry never counts against a Task (ADR
/// 0004). A ranking price and nothing else since ADR 0029: it breaks rank
/// ties in the Matcher, and no time-aware judgement is made on it — that
/// is the walk's job, and halving this number is not the walk.
let travelCost (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas TravelCost creep task

/// Travel cost to an explicit set of tiles: the same ranking price over
/// the area the caller hands in rather than the one the Task derives —
/// what prices a Task over the tiles the Reach left it, and Flee, whose
/// Work Area is the safe set and no target's surroundings (ADR 0033). An
/// unplaced creep prices at 0 as it does for a Task; there is no target,
/// so there is no unplaced-target escape — an empty or unreachable set is
/// unreachable, which is the answer a Task with nowhere to stand gets too.
let travelCostWithin (atlas: Atlas) (creep: string) (area: Set<Pos>) : int option =
    pricedPathTo atlas TravelCost creep area

/// The creep's walk to a Task's Work Area (ADR 0029): the whole ticks its
/// body needs along a cheapest path, every step floored at one tick and
/// today's standing creeps priced at nothing — the horizon every
/// time-aware judgement is made at. Beside travel cost, not derived from
/// it: a clock must not read a crowd that will have moved on, and must
/// never price a tile below the tick it takes to cross. 0 for a creep
/// already inside the area — there is no walk left to cover anything
/// with. Totality is travel cost's own contract (ADR 0004): an unplaced
/// creep or target prices 0, and an unreachable or empty Work Area has no
/// walk at all, which readers take as "no arrival" and count from now.
let walkTicks (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas Walk creep task

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
/// the engine lets stay — judged by range as before. The standing tiles
/// are the caller's, as the mover's and the price's are (ADR 0033): a
/// creep acts from the area it was actually judged applicable over, so a
/// tile the Reach took is no more a tile to work from than to walk to.
/// One case is the Atlas's own and answers before the geometry: a Task
/// that acts on nothing never acts — Flee is movement and nothing else.
let mayAct (atlas: Atlas) (creep: string) (task: Task) (area: Set<Pos>) : bool =
    match actionOn task with
    | None -> false
    | Some(targetId, actionRange) ->
        match
            Map.tryFind creep atlas.Spatial.CreepPositions,
            Map.tryFind targetId atlas.Spatial.TargetPositions
        with
        | Some creepPos, Some targetPos ->
            if (stepCost atlas.Spatial creepPos).IsNone then
                range creepPos targetPos <= actionRange
            else
                Set.contains creepPos area
        | _ -> true

/// First step toward a Task's Work Area over the given flood, sharing
/// firstStep's whole contract — that doc governs both public wrappers.
let private firstStepVia
    (atlas: Atlas)
    (floodOf: Pos -> int[] * int[])
    (creep: string)
    (goals: Set<Pos>)
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

/// The first step of a cheapest path from a creep to a set of goal tiles,
/// priced in the creep's own cost — a slow body may detour differently
/// than a fast one over the same ground. The goals are the caller's: a
/// mover is handed the tiles it may stand on this tick, which is its Work
/// Area less the Reach and, for Flee, the safe set (ADR 0033) — the
/// Atlas prices the walk and judges none of that. None when there is
/// nothing derivable: the creep is unplaced, already inside the goals, or
/// they are empty or unreachable. Of equally cheap goals the lowest
/// (cost, tile) wins, matching the flood's tie-breaking.
let firstStep (atlas: Atlas) (creep: string) (goals: Set<Pos>) : Pos option =
    firstStepVia atlas (flood atlas TravelCost creep) creep goals

/// The first step the same body would take were no tile occupied — the
/// traffic-blind route, otherwise priced exactly like firstStep. The
/// Resolver compares the two: a difference attributes the detour to the
/// occupancy surcharge, which is the only pricing the two floods do not
/// share (ADR 0008, ADR 0009). Off the shared memo since ADR 0030, under
/// the Baseline pricing: ADR 0018 called this the one flood ADR 0004's
/// memo could not serve, and neither half of that holds any more — the key
/// carries this creep's own tile since ADR 0029, and the pricing dimension
/// now names the units this route wants rather than only the walk's whole
/// ticks. ADR 0018's decision stands regardless, being about log noise:
/// the Resolver still asks only for creeps on the verbose list, and the
/// entry is lazy, so a tick that watches nobody floods for nobody.
let firstStepIgnoringTraffic (atlas: Atlas) (creep: string) (goals: Set<Pos>) : Pos option =
    firstStepVia atlas (flood atlas Baseline creep) creep goals

/// Round-trip haul cost in whole ticks for a body between a container's
/// tile and a sink structure's tile (ADR 0012): the leg out prices every
/// Carry part loaded, the leg back prices them all empty, both floods over
/// the same weights as travel cost — a road discounts a road-parity body
/// exactly as it discounts travel — but traffic-blind: the hauler quota
/// this feeds is capacity planning, not routing, and today's standing
/// creeps must never resize the fleet. Goals are the sink's adjacent
/// walkable tiles (transfer acts at range 1); the origin prices 0 as every
/// flood origin does. Each leg is priced as a walk (ADR 0029) — whole
/// ticks, no tile below one — so the two simply sum: there is no trailing
/// conversion, and one rule turns units into ticks for the whole colony.
/// None when no goal is reachable — unpriceable geometry hires nobody
/// (ADR 0004).
let haulRoundTripTicks (atlas: Atlas) (body: BodyPart list) (from: Pos) (sink: Pos) : int option =
    let count part =
        body |> List.filter ((=) part) |> List.length

    let goals = adjacentWalkable atlas sink

    let legTicks factor =
        let dist, _ = walkFloodFrom atlas.Weights factor from

        goals
        |> List.choose (fun goal ->
            let d = dist.[indexOf goal]
            if d = unreached then None else Some d)
        |> function
            | [] -> None
            | costs -> Some(List.min costs)

    let loaded =
        legTicks
            {
                FatigueParts = List.length body - count Move
                MoveParts = count Move
            }

    let empty = legTicks (emptyFactorOf body)

    match loaded, empty with
    | Some out, Some back -> Some(out + back)
    | _ -> None

/// The walk in whole ticks a freshly cast body needs to stand on a tile
/// (ADR 0026) — the half of a lead that is paid after the spawner is done.
/// Keyed on a body rather than a creep name, because the body being priced
/// has not been cast yet: nothing in the projection carries its factor,
/// and travel cost would price an unknown name as a bare
/// one-part-one-Move body. The body is priced empty, as a creep leaves the
/// spawner. The walk starts on the tiles *beside* the spawner, not on the
/// spawner's own tile: the engine places a finished creep on a free
/// neighbour and it pays no step to get there, so charging that step would
/// buy a lead ticks the replacement never walks — and, since the goal tile
/// is the incumbent's, would sell the successor a cap its predecessor
/// still reads as full. Over the same weights as travel cost — a road
/// discounts the walk exactly as it discounts a creep's — but
/// traffic-blind, like the hauler quota's round trip: a lead is planning,
/// not routing, the walk it prices does not start until the body is cast,
/// and the goal tile is the very tile the creep being replaced stands on —
/// so the occupancy surcharge would add its own step to every lead, every
/// tick, for a crowd of one that will be dead. Priced as a walk (ADR
/// 0029): whole ticks, no tile below one, the same rule every other
/// time-aware judgement in the colony is made on — a lead is a clock, and
/// nothing here converts units to ticks of its own. None when the goal is
/// unreachable, and none when the spawner has no free neighbour to be born
/// on — unpriceable geometry leads nobody (ADR 0004).
let castWalkTicks (atlas: Atlas) (body: BodyPart list) (spawn: Pos) (goal: Pos) : int option =
    let factor = emptyFactorOf body

    let dist =
        memoised atlas.Walks (spawn, factor) (fun () ->
            let dist, _ = walkFloodFromAll atlas.Weights factor (adjacentWalkable atlas spawn)

            dist)

    match dist.[indexOf goal] with
    | d when d = unreached -> None
    | d -> Some d

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
            weights.[indexOf tile] <- terrainWeight terrain)

    let dist, parents =
        floodFrom weights noTraffic (stepUnits { FatigueParts = 1; MoveParts = 1 }) origin

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
