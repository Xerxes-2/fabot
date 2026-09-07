module Fabot.Core.Atlas

open Fabot.Core.Types
open Fable.Core

/// Which of the tick's floods a memo entry holds (ADR 0029, widened by ADR
/// 0030): granularity and traffic are the two ways their readers differ, and
/// keying the one memo by this dimension is what keeps the floods together.
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
    /// body would take were no tile occupied. It differs from TravelCost in
    /// traffic alone, which is what lets the reroute attribution blame the
    /// difference on traffic and nothing else (ADR 0008, ADR 0009).
    | Baseline

/// A Dijkstra flood the readers advance rather than a finished pair of arrays:
/// the distance and predecessor grids, plus the heap and the live length the
/// loop left off at. Dijkstra settles a tile for good the moment it leaves the
/// heap, so a flood may stop anywhere and be resumed, and every settled tile
/// holds the number the whole flood would have left there — the invariant this
/// shape rests on.
type private Flood =
    {
        /// Cheapest cost to each settled tile, and `unreached` elsewhere
        /// — a tile nothing reaches and a tile nobody has asked about are
        /// one value here, parted only by whether the heap is empty.
        Dist: int[]
        /// Predecessor index on a cheapest path, -1 where there is none.
        /// Final for every tile whose distance is final.
        Parents: int[]
        /// The three read-only inputs the relaxation prices over, held so
        /// a resumed flood charges exactly what the interrupted one did.
        Weights: int[]
        Occupied: bool[]
        StepPrices: int[]
        /// The binary min-heap of dist-then-index keys, and the live length
        /// within it: the heap only grows, so a pop is a decrement rather
        /// than a splice, and every slot at or past `Size` is stale.
        Heap: ResizeArray<int>
        mutable Size: int
    }

/// The per-tick, task-aware query interface over the spatial projection
/// (ADR 0004). Total: geometry the projection cannot place gets one
/// documented answer per query — it never counts against a Task and never
/// blocks an action.
type Atlas =
    private
        {
            Spatial: SpatialInfo
            /// The room every query that names none of its own answers for: the
            /// projection's `RoomName`, empty when it names none (ADR 0041,
            /// which keeps the room off `Pos` and puts it on the API).
            Home: string
            /// The colony's tunables (ADR 0052 decision 5), carried off the
            /// view this Atlas was laid from — here rather than an argument
            /// to `trunkPath`, its one reader, because a number threaded
            /// through each ask is one two call sites can disagree about.
            Tuning: Tuning
            /// Placed creeps in view order — the canonical iteration
            /// order for everything derived per creep — each beside the
            /// room the projection files it under, because the flood it
            /// seeds is that room's.
            Placed: (string * string * Pos) list
            /// Each creep's fatigue factor — what turns terrain weight
            /// into travel cost for that body (ADR 0006).
            Factors: Map<string, FatigueFactor>
            /// Creep name -> the room the projection files it under and the
            /// tile it stands on there: the id-to-room join ADR 0041 puts on
            /// the API, resolved once so a query costs one lookup.
            CreepAt: Map<string, string * Pos>
            /// Target id -> the room the projection files it under and its tile
            /// there — the same join over the other id space, and the reason
            /// `TargetKinds` stays flat: an object id is already unique, so the
            /// kind census needs no room.
            TargetAt: Map<string, string * Pos>
            /// Step weight per tile index, per room name, laid once a tick for
            /// the flood's hot loop: -1 impassable, else the price of stepping
            /// onto the tile — road 1, plain 2, swamp 10; walls, obstacle
            /// structures and tiles outside the projection impassable (ADR
            /// 0001, ADR 0010). The only form the rule has: the single-tile
            /// query reads this grid too (`weightAt`).
            Weights: Map<string, int[]>
            /// Raw terrain weight per tile index, per room name: the ground
            /// before a road discounts it and before an obstacle blocks it. A
            /// grid of its own because a Seat is counted by terrain alone — a
            /// structure on a source's neighbour does not consume the Seat (ADR
            /// 0001) — and the Layout's three ground readers price off it too:
            /// a site's tile is terrain holding nothing, a swamp under a road
            /// is still swamp, and a trunk is priced before any road discount.
            Ground: Map<string, int[]>
            /// Terrain weight per tile index of each room's border ring — the
            /// exit rows and columns the layers' ground leaves out (ADR 0036) —
            /// and -1 everywhere else: the table form of `SpatialInfo.Borders`,
            /// for the Seam band and the crossing price. Never merged into the
            /// two grids above: a ring tile is one a creep passes through and
            /// never one it may stand on.
            Rings: Map<string, int[]>
            /// Whether a creep stands on each tile index this tick, per
            /// room name; the flood prices these tiles dearer so paths
            /// detour around standing traffic.
            Occupied: Map<string, bool[]>
            /// Memoised Dijkstra flood per placed creep's tile, fatigue factor
            /// and pricing, forced at most once per tick and shared by every
            /// query pricing from it (ADR 0002). Bodies of the same factor at
            /// the same tile share one flood; one entry per pricing (ADR 0029,
            /// ADR 0030), laid lazily, so a tick that asks for one pays for
            /// one. Each is a seeded, unadvanced `Flood` that each reader
            /// pushes out only as far as the tile it asks about.
            Floods: Map<string, Map<Pos * FatigueFactor * Pricing, Lazy<Flood>>>
            /// Memoised flood *into* a Task's ground — the far leg of a
            /// cross-room walk (ADR 0041). Its origin is the target and not a
            /// creep, which is why it is a table beside Floods: one flood
            /// answers every creep in the colony pricing that Task, the
            /// arithmetic ADR 0041 rests its cost argument on. Distances only;
            /// nothing steps along a far leg.
            FarFloods:
                System.Collections.Generic.Dictionary<
                    string * Task * bool * FatigueFactor * Pricing,
                    int[]
                 >
            /// Memoised flood *into* a Seam band — the walk out of every tile
            /// of one room onto the crossings joining it to a named neighbour
            /// (ADR 0042's container pick). One flood per ordered room pair,
            /// however many tiles are read off it, so the Seats of every source
            /// share one answer.
            SeamWalks: System.Collections.Generic.Dictionary<string * string, int[]>
            /// Memoised traffic-blind cast walk out of a spawner's tile, per
            /// (spawner tile, fatigue factor, goal's room), for bodies the view
            /// does not carry: a lead prices a replacement not yet cast (ADR
            /// 0026), whose factor is in no creep's entry.
            Walks: WalkTable
            /// Work Area per Task, built at most once per tick and shared by
            /// every query that stands a creep in one — the Floods memo on a
            /// key set the view does not carry, so a mutable table; the Atlas
            /// is rebuilt every tick, so it is per-tick by construction. Each
            /// entry holds the area in both shapes from one write (ADR 0052
            /// decision 2): the room with that room's own grid tiles, and the
            /// same tiles joined to it, because what leaves the Atlas carries
            /// its room and what stays inside indexes one room's grid.
            WorkAreas:
                System.Collections.Generic.Dictionary<
                    Task,
                    (string * Set<Pos>) option * Set<RoomPos>
                 >
            /// The Work-heavy variant of the same table (ADR 0020): the
            /// narrowed area per Task, built at most once per tick. Only
            /// Harvest narrows, so `posts` is derived once per source.
            HeavyAreas: System.Collections.Generic.Dictionary<Task, Set<RoomPos>>
            /// The creeps whose bodies carry more Work parts than Move —
            /// ADR 0016's predicate, read from the body and never a name.
            /// Three readers ask it, so the arithmetic lives here once.
            Heavy: Set<string>
            /// Memoised controller-container census, built at most once per
            /// tick (ADR 0019): the gate asks per creep and per candidate,
            /// and the answer is a colony fact. A key set of one, so a cell.
            mutable Buffers: Set<string> option
            /// The colony's [[refill cluster]] as the view spelled it
            /// (`RefillCluster.ofRefillables`, ADR 0054): which structures are
            /// the flow's one sink, and how much room each has left. `None` for
            /// a colony whose Refillables hold no spawn to key a cluster (ADR
            /// 0004).
            Cluster: RefillCluster option
        }

let private tileCount = Engine.roomSide * Engine.roomSide
let private indexOf pos = pos.X * Engine.roomSide + pos.Y

let private posAt index =
    {
        X = index / Engine.roomSide
        Y = index % Engine.roomSide
    }

/// Unreached marker in a flood's distance array.
let private unreached = System.Int32.MaxValue

/// Extra cost priced onto a step landing on a tile some creep occupies this
/// tick — one swamp step by definition (ADR 0008, ADR 0010): a crowd usually
/// means waiting, so a modest detour is preferred; the tile stays passable,
/// so traffic never makes a Task inapplicable.
let private occupancyPenalty = Engine.swampWeight

/// No tile occupied: the flood baseline the occupancy surcharge is judged
/// against — the ground the walk is priced over (ADR 0029), and the ground
/// the `Baseline` pricing the attribution compares against is priced over
/// (ADR 0030).
let private noTraffic: bool[] = Array.create tileCount false

/// The grid of a room the projection does not carry: every tile impassable,
/// read a whole room at a time (ADR 0004, ADR 0041). Absence of a room and
/// absence of every tile in it are one answer — unpriceable geometry, never
/// blocked geometry. Shared by all three grids and never written.
let private noGround: int[] = Array.create tileCount -1

/// Whether a tile is one of the room's own fifty-by-fifty — the guard every
/// grid read passes through, because a `Pos` off the grid indexes off the
/// array: under Fable that reads `undefined`, which the weight comparisons
/// would call walkable, while .NET throws.
let private inGrid (tile: Pos) =
    tile.X >= 0
    && tile.X < Engine.roomSide
    && tile.Y >= 0
    && tile.Y < Engine.roomSide

/// The eight tiles touching this one, in (X, Y) order — the order every answer
/// derived from them is listed in. Written out rather than generated, this
/// being the innermost list the Atlas builds.
let private neighbours pos =
    let x = pos.X
    let y = pos.Y

    [
        { X = x - 1; Y = y - 1 }
        { X = x - 1; Y = y }
        { X = x - 1; Y = y + 1 }
        { X = x; Y = y - 1 }
        { X = x; Y = y + 1 }
        { X = x + 1; Y = y - 1 }
        { X = x + 1; Y = y }
        { X = x + 1; Y = y + 1 }
    ]

/// The weight of raw ground (ADR 0010): plain 2, swamp 10, wall impassable —
/// written as the -1 the weight table marks impassable with. The one place
/// the engine's terrain prices live, so no grid drifts from another.
let private terrainWeight terrain =
    match terrain with
    | Plain -> 2
    | Swamp -> Engine.swampWeight
    | Wall -> -1

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
/// leaves the spawner in. Beside `fatigueFactorOf`, which reads a living
/// creep; this one reads a body the projection carries no creep for: the
/// hauler quota's candidate (ADR 0012) and a lead's replacement (ADR 0026).
let private emptyFactorOf (body: BodyPart list) : FatigueFactor =
    let count part =
        body |> List.filter ((=) part) |> List.length

    {
        FatigueParts = List.length body - count Move - count Carry
        MoveParts = count Move
    }

/// Cost units the body needs to step onto a tile of the given terrain weight
/// (Screeps fatigue): the step generates weight fatigue per fatigue-generating
/// part, each Move part pays off 2 per tick — so the unit is a half-tick (ADR
/// 0010) — and no step prices below one unit. At unit granularity and not whole
/// ticks, so a Move surplus keeps a road step cheaper than plain for every
/// body. A body without Move parts cannot step at all.
let private stepUnits (factor: FatigueFactor) weight =
    if factor.MoveParts = 0 then
        None
    else
        let units = (weight * factor.FatigueParts + factor.MoveParts - 1) / factor.MoveParts
        Some(if units < 1 then 1 else units)

/// Whole ticks the body needs to step onto a tile of the given terrain weight
/// — the walk's price (ADR 0029). Two cost units make a tick and a part of
/// one still costs a whole tick, and no step costs less than a tick however
/// much Move it carries. The nested rounding is exact — ceil(ceil(w*F / M) /
/// 2) = ceil(w*F / 2M) — so this is the step's physical time, which is why
/// the floor belongs per step and not on the total. No Move parts, no step.
let private stepTicks (factor: FatigueFactor) weight =
    stepUnits factor weight
    |> Option.map (fun units ->
        let ticks = (units + 1) / 2
        if ticks < 1 then 1 else ticks)

/// What a step costs this body on every weight the ground can carry, laid out
/// once per pricing: the index is the tile's weight and the value the price of
/// stepping onto it, written as the same -1 the weight grid marks impassable
/// with. That shared sentinel is the point — the flood's inner loop tests one
/// integer instead of calling a pricing closure. Filled by
/// `stepUnits`/`stepTicks`, the one place a step's price is computed (ADR 0010,
/// ADR 0029). Swamp must stay the dearest weight a grid can hold: the table's
/// length follows `Engine.swampWeight`, and a weight past its end reads as a
/// free step under Fable.
let private stepTable (stepPrice: int -> int option) : int[] =
    Array.init (Engine.swampWeight + 1) (fun weight -> stepPrice weight |> Option.defaultValue -1)

/// The flood's array accessors: checked on .NET (so `dotnet test` runs the
/// flood bounds-checked) and a bare JS index under Fable, where the `[<Emit>]`
/// template replaces the call — Fable's own indexer re-tests indices the loop
/// has already proven in range, and was ~28% of the tick in the flood.
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

/// One tile's weight in one of the Atlas's grids, and -1 — impassable — for a
/// tile off the grid. The single-tile ground query: the grids are laid once a
/// tick, so asking one about a tile is an array index rather than a `Pos`
/// compared down a tree. The room is the caller's, as on every query below (ADR
/// 0041).
let private weightAt (grid: int[]) (tile: Pos) : int =
    if inGrid tile then at (indexOf tile) grid else -1

/// Whether a tile is passable in one of the Atlas's grids — the -1 above
/// read as the one thing it means. A tile off the grid, off the
/// projection, walled, or blocked in whichever grid is being asked is not
/// walkable in it, which is one answer and not four (ADR 0004).
let private walkableAt (grid: int[]) (tile: Pos) : bool = weightAt grid tile >= 0

/// The heap's push: sift up by moving the hole, not by swapping. The climbing
/// key is held in a local and each dearer parent is copied one level down, so
/// a climb of k levels writes k + 1 slots instead of 3k, and the key itself is
/// written exactly once, at the hole it settles in (#168).
let private push (flood: Flood) (key: int) =
    let heap = flood.Heap

    if flood.Size >= heap.Count then
        heap.Add 0

    let mutable hole = flood.Size
    flood.Size <- flood.Size + 1
    let mutable climbing = hole > 0

    while climbing do
        let parent = (hole - 1) / 2
        let parentKey = heapAt parent heap

        if parentKey > key then
            setHeapAt hole heap parentKey
            hole <- parent
            climbing <- hole > 0
        else
            climbing <- false

    setHeapAt hole heap key

/// The mirror of `push`: the root is the answer, the last entry becomes the key
/// looking for a home, and the cheaper child of each pair is pulled up while it
/// undercuts that key. No two keys in the heap are ever equal — a tile is
/// pushed only where its dist strictly falls, and the index term parts two
/// tiles at one cost — so pop order, and with it every path the flood picks
/// between equal costs, is fixed by the key encoding.
let private pop (flood: Flood) =
    let heap = flood.Heap
    let top = heapAt 0 heap
    flood.Size <- flood.Size - 1
    let size = flood.Size

    if size > 0 then
        let key = heapAt size heap
        let mutable hole = 0
        let mutable sinking = true

        while sinking do
            let left = 2 * hole + 1

            if left >= size then
                sinking <- false
            else
                let right = left + 1
                let mutable child = left
                let mutable childKey = heapAt left heap

                if right < size then
                    let rightKey = heapAt right heap

                    if rightKey < childKey then
                        child <- right
                        childKey <- rightKey

                if childKey < key then
                    setHeapAt hole heap childKey
                    hole <- child
                else
                    sinking <- false

        setHeapAt hole heap key

    top

/// Dijkstra flood over the weight grid from every tile in `starts`, each seeded
/// at the cost the caller gives it and priced by `stepPrices` — one body's
/// `stepTable`, and, beside the occupancy the caller passes, the only thing
/// that differs between the tick's floods (ADR 0029, ADR 0030). Nothing is
/// relaxed here: what comes back is seeded and unadvanced, and `settleTo` runs
/// it, so the memo can lay one flood per creep per pricing and charge only the
/// ones a reader asks about. A start takes its seed even when it cannot be
/// stepped onto — a creep stands there, or on the border ring, which is no tile
/// of the projection's ground. Several starts price a body that may begin
/// anywhere in a set (ADR 0026). An occupied tile costs `occupancyPenalty`
/// extra, in cost units, so a caller pricing steps in anything else must pass
/// `noTraffic`, which `pricingOf` pairs per pricing.
let private floodFromAllSeeded
    (weights: int[])
    (occupied: bool[])
    (stepPrices: int[])
    (starts: (Pos * int) list)
    : Flood =
    let flood =
        {
            Dist = Array.create tileCount unreached
            Parents = Array.create tileCount -1
            Weights = weights
            Occupied = occupied
            StepPrices = stepPrices
            Heap = ResizeArray<int>()
            Size = 0
        }

    for start, seed in starts do
        let startIndex = indexOf start

        // Checked: a start is the caller's Pos, not an index the flood
        // built, so this is the one access the in-range argument for the
        // accessors above does not cover — and it runs once per start.
        if seed < flood.Dist.[startIndex] then
            flood.Dist.[startIndex] <- seed
            push flood (seed * tileCount + startIndex)

    flood

/// The goal a flood is drained for: no tile at all, so the frontier test
/// never stops it and it settles the whole room. What `drained` asks for,
/// and the only way the loop below runs to exhaustion.
let private everyTile = -1

/// Advance a flood until `goal`'s distance is final — or, for `everyTile`,
/// until the heap is empty. It fills the flood's two grids: cheapest cost to
/// every settled tile (`unreached` elsewhere), and each one's predecessor on a
/// cheapest path. The tick's hottest loop, so it runs on flat arrays with a
/// binary min-heap of dist-then-index keys, whose ordering also fixes
/// tie-breaking; the price is a table read and not a closure call, and the
/// grids come off the flood, so a resumed flood charges what the interrupted
/// one did. The stopping rule is Dijkstra's own invariant: no unsettled tile
/// can end up cheaper than the cheapest key left in the heap, because every
/// step costs at least one (ADR 0010, ADR 0029), so once `dist[goal]` is at or
/// under that frontier the tile is finished, with the number and the
/// predecessor the whole flood would have left there.
let private settleTo (flood: Flood) (goal: int) =
    let dist = flood.Dist
    let parents = flood.Parents
    let weights = flood.Weights
    let occupied = flood.Occupied
    let stepPrices = flood.StepPrices
    let mutable settling = true

    while settling do
        if flood.Size = 0 then
            settling <- false
        elif goal >= 0 && at goal dist <= heapAt 0 flood.Heap / tileCount then
            settling <- false
        else
            let key = pop flood
            let index = key % tileCount
            let d = key / tileCount

            // Stale heap entry when unequal: the tile was reached cheaper meanwhile.
            // A resumed flood meets these exactly as a running one does — the
            // heap it left holds the same duplicates it would have (#174).
            if at index dist = d then
                let x = index / Engine.roomSide
                let y = index % Engine.roomSide

                for dx in -1 .. 1 do
                    for dy in -1 .. 1 do
                        let nx = x + dx
                        let ny = y + dy

                        if
                            (dx <> 0 || dy <> 0)
                            && nx >= 0
                            && nx < Engine.roomSide
                            && ny >= 0
                            && ny < Engine.roomSide
                        then
                            let next = nx * Engine.roomSide + ny
                            let weight = at next weights

                            if weight >= 0 then
                                // -1 in the price table is a body that cannot
                                // step onto this weight at all, written as the
                                // -1 the weight grid marks impassable ground
                                // with: one test settles both.
                                let step = at weight stepPrices

                                if step >= 0 then
                                    let candidate =
                                        d
                                        + step
                                        + (if flagAt next occupied then occupancyPenalty else 0)

                                    if candidate < at next dist then
                                        setAt next dist candidate
                                        setAt next parents index
                                        push flood (candidate * tileCount + next)

/// The whole room settled, handed back as the two grids: the shape every reader
/// that reads a flood a room at a time wants — the trunk's router, the spawn
/// walk table, a far leg, the Seam band's walk.
let private drained (flood: Flood) : int[] * int[] =
    settleTo flood everyTile
    flood.Dist, flood.Parents

/// What a resumable flood reaches one tile at, settling it first: the one
/// read every per-tile question goes through, so no reader can mistake the
/// `unreached` of an unsettled tile for the one that means unreachable. A
/// tile off the grid is `unreached` too — the guard is what makes the reads
/// below in-range, and it hands unplaceable geometry ADR 0004's answer.
let private reachedBy (flood: Flood) (tile: Pos) : int =
    if not (inGrid tile) then
        unreached
    else
        let index = indexOf tile
        settleTo flood index
        at index flood.Dist

/// What a resumable flood has *already* reached one tile at, advancing it not
/// one pop: the grid read `reachedBy` guards, handed out raw and therefore
/// never an answer — the number it holds may still fall. Only the bound below
/// reads it, and only ever as an upper one.
let private glimpsedBy (flood: Flood) (tile: Pos) : int =
    if not (inGrid tile) then
        unreached
    else
        at (indexOf tile) flood.Dist

/// The cheapest distance any tile the flood has *not* settled can still turn
/// out to have: the dist at the top of its heap, and `unreached` when the heap
/// has run dry. A reader asks this to decide whether a tile is worth settling
/// at all.
let private frontierOf (flood: Flood) : int =
    if flood.Size = 0 then
        unreached
    else
        heapAt 0 flood.Heap / tileCount

/// The same read on a flood already settled whole. Spelled beside
/// `reachedBy` so the two answer off one arithmetic, and a reader handed
/// one instead of the other changes nothing but when the work was done.
let private reachedIn (dist: int[]) (tile: Pos) : int = dist.[indexOf tile]

/// The first tile of a cheapest path out of `startIndex` toward a goal,
/// walked back down the predecessor chain. Only ever asked of a goal the
/// flood has settled, and that is enough: every tile of a cheapest path is
/// strictly cheaper than its end, so the chain is final when the goal is.
let private firstStepOn (flood: Flood) (startIndex: int) (goalIndex: int) : int =
    let rec walk index =
        let parent = flood.Parents.[index]

        if parent = startIndex || parent < 0 then
            index
        else
            walk parent

    walk goalIndex

/// The flood every origin starts free at — the shape every caller but the far
/// leg of a cross-room walk wants, since a creep pays nothing to be where it
/// already is. Settled whole here (`drained`), because everyone who reaches
/// the flood this way reads it a room at a time (#174).
let private floodFromAll weights occupied stepPrices (starts: Pos list) =
    floodFromAllSeeded weights occupied stepPrices [ for start in starts -> start, 0 ]
    |> drained

/// The one-origin flood the trunk's router wants: a raw-terrain flood out of a
/// source's tile with no creep in it and no traffic seen (`trunkPath`, its only
/// caller).
let private floodFrom weights occupied stepPrices (start: Pos) =
    floodFromAll weights occupied stepPrices [ start ]

/// What a step costs and whether the crowd is seen, for one pricing over one
/// body: the ranking price sees today's traffic and counts half-ticks, the
/// clock is blind to it and counts whole ticks (ADR 0029), and the baseline
/// counts half-ticks with the crowd taken out (ADR 0030). The one place the
/// pair is laid side by side, so no flood can take one half without the other.
let private pricingOf (occupied: bool[]) (factor: FatigueFactor) (pricing: Pricing) =
    match pricing with
    | TravelCost -> stepTable (stepUnits factor), occupied
    | Walk -> stepTable (stepTicks factor), noTraffic
    | Baseline -> stepTable (stepUnits factor), noTraffic

/// The walk's flood over one body, from anywhere in `starts` (ADR 0029):
/// whole ticks a step and blind to today's traffic — the `Walk` row of
/// `pricingOf`, reached by the clocks whose origins keep them outside the
/// tick's pricing memo (the lead's cast walk, the hauler quota's round trip).
let private walkFloodFromAll weights factor (starts: Pos list) =
    let stepPrices, traffic = pricingOf noTraffic factor Walk
    floodFromAll weights traffic stepPrices starts

/// The one-origin walk: a creep, or a container, prices from the tile it
/// sits on.
let private walkFloodFrom weights factor (start: Pos) =
    walkFloodFromAll weights factor [ start ]

/// The flood one pricing wants over one body, out of one origin: the memoised
/// flood a placed creep prices from, and the one flood the Atlas leaves
/// resumable (#174) — it is read at a Work Area's few tiles, at a goal and its
/// predecessor chain, or at a Seam band's thirty-odd, never a room at a time.
let private floodPriced weights occupied factor pricing (start: Pos) : Flood =
    let stepPrices, traffic = pricingOf occupied factor pricing
    floodFromAllSeeded weights traffic stepPrices [ start, 0 ]

/// What the flood charges for a step landing on a tile — the step price plus
/// the occupancy surcharge, exactly as `floodFromAllSeeded`'s relaxation
/// charges it. None for a tile outside the projection or one this body cannot
/// step onto.
let private entryCost
    (weights: int[])
    (occupied: bool[])
    (stepPrices: int[])
    (tile: Pos)
    : int option =
    let index = indexOf tile
    let weight = weights.[index]

    if weight < 0 then
        None
    else
        let step = stepPrices.[weight]

        if step < 0 then
            None
        else
            Some(step + (if occupied.[index] then occupancyPenalty else 0))

/// The same pricing flooded *into* a set of goals rather than out of one
/// origin: cheapest cost from every tile of the room to the nearest goal,
/// counting the step onto the tile it is read at and the step onto the goal it
/// ends on (ADR 0041).
let private floodPricedInto weights occupied factor pricing (goals: Pos list) : int[] =
    let stepPrices, traffic = pricingOf occupied factor pricing

    goals
    |> List.choose (fun goal ->
        entryCost weights traffic stepPrices goal |> Option.map (fun cost -> goal, cost))
    |> floodFromAllSeeded weights traffic stepPrices
    |> drained
    |> fst

/// The Atlas over a view, recalling a spawn walk table rather than laying an
/// empty one (ADR 0032). The caller hands in the plan memo's table while the
/// census signature is unchanged, and a fresh one when it moved: every entry
/// is a pure function of the census. Every other table is laid empty — they
/// key on this tick's creeps, or on this tick's traffic.
let ofViewRecalling (walks: WalkTable) (view: ColonyView) : Atlas =
    let spatial = view.Spatial

    // The home room, spelled the one way the convention is spelled
    // (`SpatialInfo.homeName`): the projection's name, and the empty name
    // when it names none.
    let home = SpatialInfo.homeName spatial

    let tuning = view.Tuning

    // The two id-to-room joins, resolved once. An id is unique across the
    // world, so the layer that holds it is the room it is in (ADR 0041) —
    // which is what makes searching every layer the right answer here and the
    // wrong one for a query that starts from a bare `Pos`.
    let locate select =
        spatial.Rooms
        |> Map.fold
            (fun found room layer ->
                select layer
                |> Map.fold (fun found id pos -> Map.add id (room, pos) found) found)
            Map.empty

    let creepAt = locate (fun (layer: RoomLayer) -> layer.CreepPositions)
    let targetAt = locate (fun (layer: RoomLayer) -> layer.TargetPositions)

    let placed =
        view.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name creepAt
            |> Option.map (fun (room, pos) -> creep.Name, room, pos))

    let factors =
        view.Creeps
        |> List.map (fun creep -> creep.Name, fatigueFactorOf creep)
        |> Map.ofList

    // The room's grids, one set per projected room, filled by walking that
    // room's four collections rather than asking a rule per tile. These are
    // also the only form of the rules — every single-tile query reads one of
    // them (`weightAt`) — so the precedence spelled here is spelled nowhere
    // else: terrain first, then roads over the passable ground they discount,
    // then obstacles over everything; the initial -1 answers every tile
    // outside the projection. The occupancy surcharge marks **standing**
    // traffic (ADR 0008 as #225 amends it) — a body that did not move last
    // tick, a fatigued one, and every body of another colony's — because two
    // travellers each pricing the other's tile never pass.
    let standing =
        view.Creeps
        |> List.filter (fun c -> not c.Moved || c.Fatigue > 0)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    let gridOf (foreign: Set<Pos>) (layer: RoomLayer) =
        let ground = Array.create tileCount -1

        layer.Terrain
        |> Map.iter (fun tile terrain -> ground.[indexOf tile] <- terrainWeight terrain)

        // The walking grid starts as the raw ground and takes the two
        // overriding passes; the ground itself keeps neither, because a
        // Seat is counted by terrain alone (ADR 0001).
        let weights = Array.copy ground

        // A road discounts the ground under it, never ground the projection
        // calls impassable: a road on a wall (a tunnel, which ADR 0010 does
        // not model) or off the terrain projection stays impassable.
        layer.Roads
        |> Set.iter (fun tile ->
            let index = indexOf tile

            if weights.[index] > 0 then
                weights.[index] <- 1)

        layer.Obstacles |> Set.iter (fun tile -> weights.[indexOf tile] <- -1)

        let occupied = Array.create tileCount false

        layer.CreepPositions
        |> Map.iter (fun name tile ->
            if Set.contains name standing then
                occupied.[indexOf tile] <- true)

        // The bodies this colony does not hold stand here too (ADR 0052
        // decision 1): the layer carries only its own fleet, so a [[mother
        // colony]]'s [[pioneer]] on the child's [[anchor]] tile would price at
        // nothing and the flood would send a traveller into a creep it can
        // never displace.
        foreign |> Set.iter (fun tile -> occupied.[indexOf tile] <- true)

        ground, weights, occupied

    // The border ring's own grid, laid off the border layer and keyed by
    // its rooms rather than by `Rooms`: a room the projection carries a
    // ring for but no ground, or ground but no ring, is each half a room
    // and answers -1 for the half it has not got (ADR 0004).
    let ringOf (ring: Map<Pos, Terrain>) =
        let grid = Array.create tileCount -1

        ring
        |> Map.iter (fun tile terrain -> grid.[indexOf tile] <- terrainWeight terrain)

        grid

    let grids =
        spatial.Rooms
        |> Map.map (fun room layer -> gridOf (RoomPos.inRoom room view.Foreign) layer)

    let ground = grids |> Map.map (fun _ (bare, _, _) -> bare)
    let weights = grids |> Map.map (fun _ (_, grid, _) -> grid)
    let occupied = grids |> Map.map (fun _ (_, _, standing) -> standing)
    let rings = spatial.Borders |> Map.map (fun _ ring -> ringOf ring)

    {
        Spatial = spatial
        Home = home
        Tuning = tuning
        Placed = placed
        Factors = factors
        CreepAt = creepAt
        TargetAt = targetAt
        Weights = weights
        Ground = ground
        Rings = rings
        Occupied = occupied
        Floods =
            placed
            |> List.fold
                (fun table (name, room, pos) ->
                    let factor = Map.find name factors
                    let roomWeights = Map.tryFind room weights |> Option.defaultValue noGround
                    let roomOccupied = Map.tryFind room occupied |> Option.defaultValue noTraffic
                    let inRoom = Map.tryFind room table |> Option.defaultValue Map.empty

                    let laid =
                        [ TravelCost; Walk; Baseline ]
                        |> List.fold
                            (fun entries pricing ->
                                Map.add
                                    (pos, factor, pricing)
                                    (lazy
                                        (floodPriced roomWeights roomOccupied factor pricing pos))
                                    entries)
                            inRoom

                    Map.add room laid table)
                Map.empty
        FarFloods = System.Collections.Generic.Dictionary()
        SeamWalks = System.Collections.Generic.Dictionary()
        Walks = walks
        WorkAreas = System.Collections.Generic.Dictionary()
        HeavyAreas = System.Collections.Generic.Dictionary()
        Heavy =
            view.Creeps
            |> List.filter (fun creep ->
                let count part =
                    creep.Body |> Map.tryFind part |> Option.defaultValue 0

                count Work > count Move)
            |> List.map (fun creep -> creep.Name)
            |> Set.ofList
        Buffers = None
        Cluster = RefillCluster.ofRefillables view.Refillables
    }

/// The Atlas over a view with nothing recalled: a fresh spawn walk
/// table, filled from scratch as this tick prices its leads. The tick loop
/// always has a memo to hand over, so this is the shape a reader building
/// an Atlas over a view alone — a test, or a one-off — asks for.
let ofView (view: ColonyView) : Atlas = ofViewRecalling (WalkTable()) view

/// One room's geometry, read the way ADR 0041 says a layer is read: a room the
/// projection carries no geometry for has no entry, which is the same answer as
/// an entry whose every container is empty (ADR 0004) — never the indexer,
/// which throws on exactly that room. The rule is `SpatialInfo.layerOf`'s.
let private layerOf (atlas: Atlas) (room: string) : RoomLayer =
    SpatialInfo.layerOf atlas.Spatial room

/// One room's step-weight grid, and the all-impassable grid for a room the
/// projection does not carry — which is the same answer `layerOf` gives
/// that room, read a whole room at a time: an empty layer has no passable
/// tile in it either.
let private weightsOf (atlas: Atlas) (room: string) : int[] =
    Map.tryFind room atlas.Weights |> Option.defaultValue noGround

/// One room's raw terrain grid — the ground before roads and obstacles —
/// and the all-impassable grid for a room the projection does not carry.
let private groundOf (atlas: Atlas) (room: string) : int[] =
    Map.tryFind room atlas.Ground |> Option.defaultValue noGround

/// One room's border-ring grid, and the all-impassable grid for a room the
/// projection carries no border for: a room with no ring has no crossing
/// on it, which is the empty band `seams` already answered with (ADR 0004).
let private ringOf (atlas: Atlas) (room: string) : int[] =
    Map.tryFind room atlas.Rings |> Option.defaultValue noGround

/// One room's standing traffic, and no traffic at all for a room the
/// projection does not carry — which is what an empty room holds anyway.
let private occupiedOf (atlas: Atlas) (room: string) : bool[] =
    Map.tryFind room atlas.Occupied |> Option.defaultValue noTraffic

/// A copy of one room's step weight per tile index — the grid that room's
/// floods price from, -1 impassable. Read by the census guard (ADR 0032) and
/// nothing else: spawn walks are recalled on the census signature alone, so two
/// views the signature calls equal have to lay the same grid.
let stepWeights (atlas: Atlas) (room: string) : int[] = Array.copy (weightsOf atlas room)

/// Whether a creep's body was cast from a heavy-Work row: more Work parts than
/// Move (ADR 0016).
let workHeavy (atlas: Atlas) (creep: string) : bool = Set.contains creep atlas.Heavy

/// A creep's fatigue factor; a creep the view does not carry prices
/// as a bare one-part-one-Move body — terrain weight verbatim.
let private factorOf (atlas: Atlas) (creep: string) : FatigueFactor =
    Map.tryFind creep atlas.Factors
    |> Option.defaultValue { FatigueParts = 1; MoveParts = 1 }

/// The memoised flood for a creep from a tile of one room, under one pricing;
/// placed creeps' own tiles hit the memo. The room is the caller's, and it is
/// always the room the creep stands in: a flood runs inside one room and stops
/// at its border (ADR 0041).
let private flood (atlas: Atlas) (pricing: Pricing) (room: string) (creep: string) (pos: Pos) =
    let factor = factorOf atlas creep

    match
        atlas.Floods
        |> Map.tryFind room
        |> Option.bind (Map.tryFind (pos, factor, pricing))
    with
    | Some memo -> memo.Value
    | None -> floodPriced (weightsOf atlas room) (occupiedOf atlas room) factor pricing pos

/// The creeps the projection places, each beside the tile it stands on and the
/// room that tile is in, in view creep order — the canonical order for
/// everything derived per creep. This is the Resolver's list: arbitrated
/// movement (ADR 0001, ADR 0008) is a room's and stays single-room (ADR 0041),
/// so the pass groups these by `.Room`. The tiles carry their rooms (ADR 0052
/// decision 2), which is what makes a set of blocked tiles safe to build across
/// the list: keyed on a bare coordinate, two creeps standing in two rooms would
/// collapse into one occupant. An unplaceable creep is in no group (ADR 0004).
let placedCreeps (atlas: Atlas) : (string * RoomPos) list =
    atlas.Placed |> List.map (fun (name, room, pos) -> name, RoomPos.at room pos)

/// Name of the colony's own room — the entry of the layer that is home (ADR
/// 0041), which the Layout gates on and stamps onto every site it places (ADR
/// 0017). None when the projection names no room, which since ADR 0041 is a
/// separate question from whether it carries geometry.
let homeRoom (atlas: Atlas) : string option = atlas.Spatial.RoomName

/// Tile of a projected target (source, structure, site, controller) — in
/// whichever room the projection files that id under, since an id is unique
/// across the world. Room and tile in one (ADR 0052 decision 2), so no join
/// can read one room's coordinates as another's.
let positionOf (atlas: Atlas) (targetId: string) : RoomPos option =
    Map.tryFind targetId atlas.TargetAt
    |> Option.map (fun (room, pos) -> RoomPos.at room pos)

/// Tiles a construction site may occupy in the colony's own room: non-Wall
/// terrain holding no projected target — anything standing or being built keeps
/// a site off a tile; creeps do not, and neither do the two transient kinds
/// (`isTransient`), because a tombstone stands wherever a creep died and the
/// Layout's ordering must not be a function of that (ADR 0011). Deterministic
/// (X, Y) order, which is the grid's own flat index consed down from the last
/// one, so the list is built straight and never reversed. One room and no
/// other (ADR 0041): a second room's tiles
/// unioned in would offer the Layout a coordinate it does not own.
let buildableTilesIn (atlas: Atlas) (room: string) : Pos list =
    let ground = groundOf atlas room

    // A grid rather than a `Set<Pos>` for the same reason the scan is one:
    // it is asked about every tile of the room. Which targets stand on a
    // tile and which do not is the rule above, unchanged.
    let taken = Array.create tileCount false

    (layerOf atlas room).TargetPositions
    |> Map.iter (fun id tile ->
        if not (Map.tryFind id atlas.Spatial.TargetKinds |> Option.exists isTransient) then
            taken.[indexOf tile] <- true)

    let mutable tiles = []

    for index = tileCount - 1 downto 0 do
        if at index ground >= 0 && not (flagAt index taken) then
            tiles <- posAt index :: tiles

    tiles

/// Ids of the projected targets of one kind, in id order — across every room
/// the projection carries. The kind census is not layered and does not need to
/// be: an object id is unique across the world (ADR 0041), and this answers
/// ids, never tiles. Every reader that turns these into tiles joins a room first.
let private targetsOfKind (atlas: Atlas) (kind: TargetKind) : string list =
    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, k) -> if k = kind then Some id else None)

/// Placed targets of one kind in one named room: id and tile, in id order.
/// One of the joins between the flat kind census and the layered positions,
/// with the room named rather than searched (ADR 0041): its readers are the
/// reflexes, which measure a tile against a creep's, and a tile drawn from
/// whichever layer held the id would aim them at another room's coordinate. A
/// room the projection does not carry places nothing (ADR 0004).
let private placedOfKindIn
    (atlas: Atlas)
    (room: string)
    (kind: TargetKind)
    : (string * RoomPos) list =
    let layer = layerOf atlas room

    targetsOfKind atlas kind
    |> List.choose (fun id ->
        Map.tryFind id layer.TargetPositions
        |> Option.map (fun pos -> id, RoomPos.at room pos))

// The six counts below are one half of the Layout's gap rule — `allowed at RCL
// - built - pending` — and the allowance is a fact about one room's controller,
// so the census subtracted from it has to be one room's too.

/// Extensions already standing in the named room.
let builtExtensionsIn (atlas: Atlas) (room: string) : int =
    placedOfKindIn atlas room (Structure BuiltKind.Extension) |> List.length

/// Extension construction sites already placed in the named room.
let pendingExtensionsIn (atlas: Atlas) (room: string) : int =
    placedOfKindIn atlas room (Site BuiltKind.Extension) |> List.length

/// Towers already standing in the named room.
let builtTowersIn (atlas: Atlas) (room: string) : int =
    placedOfKindIn atlas room (Structure BuiltKind.Tower) |> List.length

/// Tower construction sites already placed in the named room.
let pendingTowersIn (atlas: Atlas) (room: string) : int =
    placedOfKindIn atlas room (Site BuiltKind.Tower) |> List.length

/// Storages already standing in the named room — at most one, but counted
/// the way the tower and the extensions are so one gap rule sizes every
/// kind the ordering picks for (ADR 0022).
let builtStoragesIn (atlas: Atlas) (room: string) : int =
    placedOfKindIn atlas room (Structure BuiltKind.Storage) |> List.length

/// Storage construction sites already placed in the named room.
let pendingStoragesIn (atlas: Atlas) (room: string) : int =
    placedOfKindIn atlas room (Site BuiltKind.Storage) |> List.length

/// Towers standing in the colony's own room: id and tile, in id order — the
/// fire reflex's whole view of a tower (ADR 0014): no store is projected, a
/// dry tower's shot simply fails at the engine. Home and no other room,
/// because a tower stands only in a room we own (ADR 0042).
let placedTowers (atlas: Atlas) : (string * RoomPos) list =
    placedOfKindIn atlas atlas.Home (Structure BuiltKind.Tower)

/// Dropped energy piles one room's layer places: id and tile, in id order. The
/// pickup reflex's whole view of a pile — no amount is projected, since a pile
/// worth more than one carry is several trips, which is a Task's arithmetic and
/// not a reflex's.
let droppedEnergyIn (atlas: Atlas) (room: string) : (string * RoomPos) list =
    placedOfKindIn atlas room Dropped

/// Tiles holding a built road in the named room — the projection's road
/// census, one half of what the Layout's road gap subtracts (ADR 0011). The
/// room is the caller's, like every placement census below (ADR 0052 decision
/// 2), so no census answers for a room the caller never named.
let roadTilesIn (atlas: Atlas) (room: string) : Set<Pos> = (layerOf atlas room).Roads

/// Tiles of one room's placed targets whose kind answers a predicate — the
/// join between the flat kind census and that room's positions, for the
/// censuses read as tiles rather than as counts. The room is named rather
/// than searched (ADR 0041): a `Set<Pos>` has no room dimension, so two
/// rooms' tiles unioned would stand in neither room alone.
let private tilesWhereIn (atlas: Atlas) (room: string) (matches: TargetKind -> bool) : Set<Pos> =
    let layer = layerOf atlas room

    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if matches kind then
            Map.tryFind id layer.TargetPositions
        else
            None)
    |> Set.ofList

/// The same census over one room, by kind.
let private tilesOfKindIn (atlas: Atlas) (room: string) (kind: TargetKind) : Set<Pos> =
    tilesWhereIn atlas room ((=) kind)

/// Tiles holding a road construction site — the census's other half: a
/// pending road is not yet a road (ADR 0010) but its tile needs no new site.
let pendingRoadTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Road)

/// Tiles of one room holding a built container — the container census's
/// standing half (ADR 0012): a built container keeps a plan from re-dropping
/// its site.
let containerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Container)

/// Tiles of one room holding a container construction site — the census's
/// pending half: a pending container is not yet a container but its tile
/// needs no new site.
let pendingContainerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Container)

/// ADR 0040's container census in one room: the tiles a container stands on
/// united with the tiles one is pending on — the set every "must another
/// container be built?" question is asked against, at home and in an outpost
/// alike. One name because it is one rule.
let containerCensusIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.union (containerTilesIn atlas room) (pendingContainerTilesIn atlas room)

/// Tiles of one room already taken by a construction site of some **other**
/// kind — the tiles a container site cannot go down on today, whatever the
/// plan wants there. The engine takes one construction site per tile, so a
/// pick onto an occupied tile is answered ERR_INVALID_TARGET once a tick for
/// as long as that site stands (#244, live in W13S29). **Our own sites and no
/// one else's**: the projection's site census comes off
/// `FIND_MY_CONSTRUCTION_SITES` (`World.seenFacts`), so this is our half of the
/// engine's rule — a rival's site in a room nobody owns is invisible to it and
/// would collide unseen. A **built** structure
/// is not in it and must not be: a container site goes down on a standing
/// road perfectly well, and on an outpost [[seat]] a road is the best tile
/// there is. The container kind is left out because a container site is the
/// *target* clause's business (ADR 0040) — one on a Seat is within range 1 of
/// that Seat's source, so "must another one be built?" has already answered
/// no before this census is asked, and answering it a second time here would
/// turn a collision rule into a silent second target rule.
let nonContainerSiteTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesWhereIn atlas room (function
        | Site BuiltKind.Container -> false
        | Site _ -> true
        | _ -> false)

/// Tiles holding a built Storage — the tile a Link footing is anchored on
/// once the reservation has become a structure (ADR 0022).
let storageTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Storage)

/// Tiles holding a Storage construction site — the same anchor while the
/// site is still being built.
let pendingStorageTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Storage)

/// Tiles holding a standing rampart — the covering census (ADR 0034): a tile
/// already ramparted needs no rampart site. Ownership is not asked, unlike
/// the hits: a tile takes one rampart whoever raised it.
let rampartTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Rampart)

/// Tiles holding a standing rampart of ours — the same census asked with
/// ownership on (ADR 0033). The projection carries hits for an ownable kind
/// only when it is ours (ADR 0034), so the hits are what tell our rampart from
/// one somebody else left standing in a room we took: cover for our creeps is
/// cover we own.
let ourRampartTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let layer = layerOf atlas room

    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if kind = Structure BuiltKind.Rampart && Map.containsKey id atlas.Spatial.Hits then
            Map.tryFind id layer.TargetPositions
        else
            None)
    |> Set.ofList

/// Tiles holding a rampart construction site — the census's pending half,
/// exactly as a road's is: a site standing there is not yet cover, but its
/// tile needs no second site.
let pendingRampartTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Rampart)

/// Tiles holding a standing Keep structure — the spawn, the tower and the
/// Storage (ADR 0034): what a rampart covers, the tick the structure
/// stands. A site is not covered until it is a structure.
let keepTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesWhereIn atlas room (function
        | Structure built -> isKeep built
        | _ -> false)

/// Tiles holding a standing link. A link is a target, so its tile is no
/// longer buildable; the Layout adds these back as footing candidates so
/// a footing does not jump the tick its link goes up (ADR 0022).
let linkTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Link)

/// Whether a tile's terrain is swamp; a tile outside the projection is not.
/// The room is the caller's (ADR 0052 decision 2). Read off the raw ground
/// grid and not the walking one: swamp is what the terrain is, so a road laid
/// over it must not answer plain.
let isSwampIn (atlas: Atlas) (room: string) (tile: Pos) : bool =
    weightAt (groundOf atlas room) tile = Engine.swampWeight

/// Walkable tiles adjacent to `pos` read as a tile of `room`, in deterministic
/// (X, Y) order. Standing respects obstacles, unlike Seat counting. The tile
/// handed in carries no room of its own (ADR 0041), so the room rides on the
/// API and a creep filed under an outpost is offered that room's ground and
/// never home's.
let adjacentWalkableIn (atlas: Atlas) (room: string) (pos: Pos) : Pos list =
    let weights = weightsOf atlas room
    neighbours pos |> List.filter (walkableAt weights)

/// Every tile of the room a creep may stand on — `adjacentWalkableIn`'s
/// answer over the whole room, off the same grid and so under the same
/// terrain, road and obstacle precedence. This is Flee's safe ground (ADR
/// 0033), and a creep runs over the ground of the room it stands in, which is
/// whichever room a hostile's Reach is filed under (ADR 0041).
let walkableTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let weights = weightsOf atlas room

    Set.ofList
        [
            for index in 0 .. tileCount - 1 do
                if at index weights >= 0 then
                    posAt index
        ]

/// The tile a creep stands on; None for a creep the projection does not
/// place. What a judgement about where a creep *is* reads — as
/// `positionOf` is the same question about a target — and, like it, room
/// and tile in one (ADR 0052 decision 2).
let creepTile (atlas: Atlas) (creep: string) : RoomPos option =
    Map.tryFind creep atlas.CreepAt
    |> Option.map (fun (room, pos) -> RoomPos.at room pos)

/// The room a creep stands in; None for a creep the projection does not
/// place. `creepTile`'s room alone, kept as a query of its own for the
/// readers that want only it — a Reach, a safe set, a grid or flood indexed
/// by that room. An unplaced creep names no room, which is ADR 0004's answer.
let creepRoom (atlas: Atlas) (creep: string) : string option =
    Map.tryFind creep atlas.CreepAt |> Option.map fst

/// The room the projection files a target under; None for one it does not
/// place. `positionOf`'s room alone, as `creepRoom` is `creepTile`'s: the
/// room a target's Work Area lies in, and so the room whose Reach is taken
/// out of that area (#138), and the room a spawn's doorstep is read in.
let targetRoom (atlas: Atlas) (targetId: string) : string option =
    Map.tryFind targetId atlas.TargetAt |> Option.map fst

/// What a Task acts on, and the Chebyshev range its action reaches from
/// (Screeps: harvest, withdraw, transfer and reserveController at range 1;
/// build, repair and upgrade at range 3) — the one pair every geometry query
/// starts from. None for a Task that acts on nothing: Flee has no target and no
/// action (ADR 0033).
let private actionOn =
    function
    | Harvest id
    | Withdraw id
    | Reserve id
    | Claim id
    | Pickup id
    | Refill id -> Some(id, 1)
    | Build id
    | Repair id
    | Upgrade id -> Some(id, 3)
    | Flee -> None

/// The [[refill cluster]] this Task *is*, if it is one (ADR 0054): a Refill
/// whose target is the cluster's spawn is the whole ring's, and every other
/// Refill — a tower's, the [[buffer]]'s, the [[storage]]'s, a [[ferry]] sink's
/// — is the single structure's it always was.
let private clusterOf (atlas: Atlas) (task: Task) : RefillCluster option =
    match task, atlas.Cluster with
    | Refill id, Some cluster when cluster.Spawn = id -> Some cluster
    | _ -> None

/// The tiles a Task's action is measured from, beside the room they stand in:
/// the target's own tile for every Task there is, and the **hungry** members'
/// tiles for a [[refill cluster]] (ADR 0054) — a body is in position when it
/// stands beside any structure of the cluster it can still pour into, which is
/// what makes one Task out of a ring of ten. The room is the target's, and a
/// member the projection places elsewhere or not at all contributes no tile
/// (ADR 0004).
let private actionTilesOf (atlas: Atlas) (task: Task) : (string * Pos list) option =
    match actionOn task with
    | None -> None
    | Some(targetId, _) ->
        match Map.tryFind targetId atlas.TargetAt with
        | None -> None
        | Some(room, target) ->
            match clusterOf atlas task with
            | None -> Some(room, [ target ])
            | Some cluster ->
                Some(
                    room,
                    RefillCluster.hungry cluster
                    |> List.choose (fun id ->
                        match Map.tryFind id atlas.TargetAt with
                        | Some(memberRoom, tile) when memberRoom = room -> Some tile
                        | _ -> None)
                )

/// Seat tiles of a placed source: walkable (non-wall) neighbours of its tile,
/// by terrain alone — structures and creeps do not consume Seats (ADR 0001).
let private seatTiles (ground: int[]) (pos: Pos) : Set<Pos> =
    neighbours pos |> List.filter (walkableAt ground) |> Set.ofList

/// Seat tiles of a source — the geometry behind `seats`, for the Layout's
/// source-container pick (ADR 0012). Empty for a source the projection does not
/// place (ADR 0004). The source's own room answers, not the colony's: the id
/// resolves the room (ADR 0041), so an outpost source's Seats are never a home
/// tile of the same coordinate.
let private seatTilesIn (atlas: Atlas) (sourceId: string) : (string * Set<Pos>) option =
    Map.tryFind sourceId atlas.TargetAt
    |> Option.map (fun (room, pos) -> room, seatTiles (groundOf atlas room) pos)

let seatTilesOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> =
    seatTilesIn atlas sourceId
    |> Option.map (fun (room, tiles) -> RoomPos.setAt room tiles)
    |> Option.defaultValue Set.empty

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    Map.tryFind sourceId atlas.TargetAt
    |> Option.map (fun (room, pos) -> seatTiles (groundOf atlas room) pos |> Set.count)

/// The Work Area geometry behind `workArea`: the passable tiles within the
/// action's range of its target. Empty for a Task the projection cannot place a
/// target for — and for Flee, whose safe ground is a colony fact the decision
/// layer derives rather than geometry the projection carries (ADR 0033).
let private buildWorkArea (atlas: Atlas) (task: Task) : (string * Set<Pos>) option =
    match actionOn task with
    | None -> None
    | Some(_, r) ->
        match actionTilesOf atlas task with
        | None -> None
        // The target's own room, resolved off its id (ADR 0041): which ground
        // an area is is settled by where the target stands, never by which room
        // the reader is working in.
        | Some(room, targets) ->
            let weights = weightsOf atlas room

            Some(
                room,
                Set.ofList
                    [
                        for target in targets do
                            for x in target.X - r .. target.X + r do
                                for y in target.Y - r .. target.Y + r do
                                    let tile = { X = x; Y = y }

                                    if walkableAt weights tile then
                                        tile
                    ]
            )

/// Build-once-per-tick over one of the Atlas's mutable tables: the shape
/// every key set the view does not carry is memoised through. No reader can
/// observe whether the answer was built or recalled, and the Atlas is rebuilt
/// every tick, so each table is per-tick by construction.
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
/// range of its target. The base geometry `posts` is itself derived from, so it
/// stays a pure function of the Task; readers that hold a creep want
/// `workAreaFor`, which narrows it for a Work-heavy harvester (ADR 0020). Empty
/// when the projection cannot place the target.
let private areaOf (atlas: Atlas) (task: Task) =
    memoised atlas.WorkAreas task (fun () ->
        let tiles = buildWorkArea atlas task

        tiles,
        (match tiles with
         | Some(room, grid) -> RoomPos.setAt room grid
         | None -> Set.empty))

/// The area as its room and that room's grid tiles: what every reader
/// *inside* the Atlas takes, so the join is never paid inside a per-creep
/// query. None for a Task the projection cannot place a target for, and
/// for Flee.
let private areaTilesOf (atlas: Atlas) (task: Task) : (string * Set<Pos>) option =
    fst (areaOf atlas task)

let workArea (atlas: Atlas) (task: Task) : Set<RoomPos> = snd (areaOf atlas task)

/// Every source of one room's Seat tiles, unioned — the seat half behind
/// `dualSeatsIn` and posts. Named room and not every layer (ADR 0041): the
/// union is intersected with an Upgrade area below, and two rooms' Seats
/// unioned would meet it at a coordinate that is a Dual Seat in neither.
let private seatUnionIn (atlas: Atlas) (room: string) : Set<Pos> =
    let ground = groundOf atlas room

    targetsOfKind atlas Source
    |> List.choose (fun id ->
        match Map.tryFind id atlas.TargetAt with
        | Some(where, pos) when where = room -> Some(seatTiles ground pos)
        | _ -> None)
    |> List.fold Set.union Set.empty

/// Every controller of one room's Upgrade Work Area, unioned — the tiles a
/// creep can upgrade from, behind `dualSeatsIn` and controllerContainers. One
/// room for the same reason the Seat union is one room's.
let private upgradeAreaIn (atlas: Atlas) (room: string) : Set<Pos> =
    targetsOfKind atlas Controller
    |> List.filter (fun id ->
        match Map.tryFind id atlas.TargetAt with
        | Some(where, _) -> where = room
        | None -> false)
    |> List.map (fun id ->
        match areaTilesOf atlas (Upgrade id) with
        | Some(_, tiles) -> tiles
        | None -> Set.empty)
    |> List.fold Set.union Set.empty

/// The working ground of the room (ADR 0022): every projected source's Seats
/// plus, in the colony's own room, its controller's Upgrade Work Area — the
/// tiles the colony works from, off-limits to the Layout's clustered ordering,
/// since a tower or extension there eats a tile an Anchor or an upgrader
/// stands on. The Upgrade half is the home room's alone, for the reason
/// `standingPostsIn` splits the Dual Seats on: the colony upgrades one
/// controller, its own, and *reserves* an [[outpost]]'s, so an outpost
/// controller's area is ground nobody upgrades from (ADR 0042) — a set the
/// Layout, asking only about home, never saw the width of until the mover
/// began asking room by room (#241). Total: a room with neither kind of
/// geometry reserves nothing (ADR 0004).
let workingGroundIn (atlas: Atlas) (room: string) : Set<Pos> =
    if room = atlas.Home then
        Set.union (seatUnionIn atlas room) (upgradeAreaIn atlas room)
    else
        seatUnionIn atlas room

/// Dual Seats of the room: tiles inside both some projected source's Seats and
/// a projected controller's Upgrade Work Area — a creep standing on one
/// harvests and upgrades without ever moving. Total: no controller, no sources,
/// or a disjoint pair answers with the empty set (ADR 0004).
let dualSeatsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (seatUnionIn atlas room) (upgradeAreaIn atlas room)

/// Whether a creep stands on a Dual Seat: the one tile where a heavy body has a
/// second thing to do without moving, which is why ADR 0025 gives it no
/// reprieve through its source's empty window and ADR 0048 leaves that
/// exclusion standing. An unplaced creep stands on nothing (ADR 0004).
let standsOnDualSeat (atlas: Atlas) (creep: string) : bool =
    match Map.tryFind creep atlas.CreepAt with
    | Some(room, tile) when room = atlas.Home -> Set.contains tile (dualSeatsIn atlas room)
    | _ -> false

/// The **standing** half of the Post census: the Dual Seats plus every Seat
/// under a built container, which by the Layout's geometry is a source
/// container. Total, room-local and derived fresh each tick: a Post is one
/// tile carrying a Seat and a container (ADR 0041). The Dual Seat half is the
/// colony's own room's alone and only the container half crosses a border
/// (ADR 0042), because a Dual Seat is a tile a creep harvests *and upgrades*
/// from and the colony upgrades its own controller: counted in an outpost it
/// would name an income share for a source with no container under it,
/// precisely the switch ADR 0042 makes the container be. Separated from
/// `postsIn` along that same split between what a room is *worth* and what it
/// is *worked* from: this is the switch that admits a source into the quotas,
/// and a site throws none, producing nothing anybody hauls.
let private standingPostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    let containerPosts =
        Set.intersect
            (seatUnionIn atlas room)
            (tilesOfKindIn atlas room (Structure BuiltKind.Container))

    if room = atlas.Home then
        Set.union containerPosts (dualSeatsIn atlas room)
    else
        containerPosts

/// Seats carrying a container **construction site** — the Post a heavy body is
/// hired for before the container it will dig into exists (amending ADR 0045
/// and ADR 0046). An Anchor digs twelve a tick and spends it into the site
/// under its own feet, so the container goes up off a source that is otherwise
/// producing nothing, where without it the worker row commutes a Seam apart at
/// fifty energy a trip. Read off the Seats and never off the site's range: a
/// site a step off this source's Seats belongs to whatever source seats *it*.
let private containerSitePostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (seatUnionIn atlas room) (pendingContainerTilesIn atlas room)

/// Posts of the room: the tiles worth garrisoning with a heavy-WORK body (ADR
/// 0012) — the standing census above, plus the Seats carrying a container site.
/// The capacity unit of the Anchor quota and of Harvest's own concurrency (ADR
/// 0024), and the only footing a Work-heavy body harvests from (ADR 0020).
/// Total, room-local and derived fresh each tick.
let postsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.union (standingPostsIn atlas room) (containerSitePostsIn atlas room)

/// Every projected room's Posts, counted: the Anchor row's quota (ADR 0012,
/// widened to the outpost layer by ADR 0042). An outpost's Post is the same
/// garrison tile a home Post is and hires the same row, which is why the
/// outpost needs no remote-miner concept of its own. Counted room by room and
/// summed, never unioned (ADR 0041): a `Pos` carries no room, so two rooms
/// whose Posts share a coordinate are two garrison tiles a border apart. A
/// Post is a vision fact through the *container* and its site, never through
/// the layer — a declared outpost always carries one, and reading absence
/// onto the declaration is a deadlock — so a blind outpost's Seat hires no
/// Anchor (ADR 0004), and a room leaves this fold only when the scan set
/// drops it (ADR 0043).
let postCount (atlas: Atlas) : int =
    atlas.Spatial.Rooms
    |> Map.fold (fun total room _ -> total + Set.count (postsIn atlas room)) 0

/// Tiles holding a standing container on a Post — the tiles a work-heavy
/// body garrisons and cannot flee from (ADR 0033), ramparted beside the Keep
/// (ADR 0034). A Post that is a bare Dual Seat is not one of these: what the
/// rule covers is a structure standing. The room is the caller's.
let postContainerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (containerTilesIn atlas room) (postsIn atlas room)

/// The Posts of one source: its own Seats that are Posts. Empty for a source
/// the projection does not place, and for one with none of the three — a
/// built container on a Seat, a container site on a Seat, or a Dual Seat.
/// Every half is read in the source's own room (ADR 0041), and the Seat join
/// is what keeps a neighbouring source's site out: a Post belongs to the rock
/// it seats, not to the rock it is near.
let private postsOfIn (atlas: Atlas) (sourceId: string) : (string * Set<Pos>) option =
    seatTilesIn atlas sourceId
    |> Option.map (fun (room, seats) -> room, Set.intersect seats (postsIn atlas room))

let postsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> =
    postsOfIn atlas sourceId
    |> Option.map (fun (room, tiles) -> RoomPos.setAt room tiles)
    |> Option.defaultValue Set.empty

/// The **standing** Posts of one source: `postsOf` above less the Seats whose
/// container is still a site — the switch that admits a source into the quotas
/// (ADR 0042).
let standingPostsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> =
    seatTilesIn atlas sourceId
    |> Option.map (fun (room, seats) ->
        Set.intersect seats (standingPostsIn atlas room) |> RoomPos.setAt room)
    |> Option.defaultValue Set.empty

/// The tile of a container construction site standing on a [[post]] — the one
/// site a body may build from under its own feet (amending ADR 0045 and ADR
/// 0046), as a tile rather than as a question about a creep. `None` for a site
/// of any other kind, one the projection does not place, and one on a Seat no
/// source is served from. Three joins, all load-bearing: the **kind**, because
/// a Post carries other sites and ADR 0034 ramparts a Post container; the
/// **Seat**, because a container site that seats no source is the controller's
/// buffer and a delivery like any other; and the **room**, because a `Pos`
/// carries none (ADR 0041). `standsOnPostSite` below is the same fact asked of
/// one creep, written in terms of this one so the two can never part; total
/// (ADR 0004).
let postSiteTile (atlas: Atlas) (siteId: string) : RoomPos option =
    if Map.tryFind siteId atlas.Spatial.TargetKinds <> Some(Site BuiltKind.Container) then
        None
    else
        match Map.tryFind siteId atlas.TargetAt with
        | Some(room, tile) when Set.contains tile (containerSitePostsIn atlas room) ->
            Some(RoomPos.at room tile)
        | _ -> None

let standsOnPostSite (atlas: Atlas) (creep: string) (siteId: string) : bool =
    match postSiteTile atlas siteId with
    | Some tile -> creepTile atlas creep = Some tile
    | None -> false

/// The named source's Posts whose container is still a site — the tiles whose
/// garrison is read off where a body *is* and never off what it holds this
/// tick. Harvest's Post cap is what reads it (ADR 0024): on a standing
/// container the overflow reprieves a full store, so the garrison holds the
/// source's one Harvest slot from arrival to death; on a site Harvest falls
/// away while the store is full and Build takes over, and a cap counting
/// Harvest's holders alone would read the tile as free on every build tick and
/// admit a second heavy body onto it. Room-joined and Seat-joined like every
/// other half of the census; total (ADR 0004).
let sitePostsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> =
    seatTilesIn atlas sourceId
    |> Option.map (fun (room, seats) ->
        Set.intersect seats (containerSitePostsIn atlas room) |> RoomPos.setAt room)
    |> Option.defaultValue Set.empty

/// Whether a creep and a Task's target stand in one room — the question every
/// join between a creep and a target's geometry has to settle while no flood
/// leaves its room (ADR 0041). Absence is permissive (ADR 0004): a Task acting
/// on nothing, an unplaced creep and an unplaced target are each not a border
/// crossing.
let private sharesRoom (atlas: Atlas) (creep: string) (task: Task) : bool =
    match actionOn task with
    | None -> true
    | Some(targetId, _) ->
        match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
        | Some(creepRoom, _), Some(targetRoom, _) -> creepRoom = targetRoom
        | _ -> true

/// The body-aware Work Area, in the target's own room and blind to where the
/// creep is standing (ADR 0020). Ordinarily the Task's own area, but Harvest
/// for a Work-heavy body is narrowed to that source's Posts when the source has
/// any: a heavy body digs from the tile that catches its overflow or lets it
/// upgrade in place, and a container site on a Seat is such a Post, so the tile
/// is sometimes the one the body is about to build. A source that has a Post
/// narrows to it even when the projection blocks it — an area with nothing
/// standable in it makes the Task inapplicable rather than silently widening
/// back to the Seats. Only Harvest narrows. Memoised per Task. A source with
/// **no** Post narrows nothing at home and narrows to nothing everywhere else,
/// and the room is the whole of what separates the two: ADR 0020's fallback to
/// the bare Seats is a *bootstrap* rule for the colony's own room, where a
/// stranded Anchor is a few tiles from a spawn that can replace it, while an
/// outpost bootstraps through a reserver and a light builder (ADR 0042) and a
/// heavy body on a containerless outpost Seat would dig onto the ground in a
/// room whose haul quota the container is what switches on. So this is the
/// geometric dual of ADR 0042's "an unposted outpost source is worth nothing to
/// the workforce". A source the projection does not place keeps the fallback
/// (ADR 0004).
let private narrowedArea (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
    match task with
    | Harvest sourceId when workHeavy atlas creep ->
        memoised atlas.HeavyAreas task (fun () ->
            let postTiles = postsOf atlas sourceId

            if not (Set.isEmpty postTiles) then
                Set.intersect (workArea atlas task) postTiles
            else
                // Absence is home's answer and not an outpost's: only a
                // source the projection places in another room loses the
                // fallback.
                match targetRoom atlas sourceId with
                | Some room when room <> atlas.Home -> Set.empty
                | _ -> workArea atlas task)
    // A Post's Seat is the garrison's (ADR 0051): a light body's Harvest Work
    // Area is the source's Seats less its Posts — the complement of the heavy
    // arm above, so the two kinds of body stand on disjoint tiles of one source
    // and a light crowd cannot squat the tile the Anchor was hired for.
    | Harvest sourceId ->
        match postsOfIn atlas sourceId with
        | Some(room, postTiles) when not (Set.isEmpty postTiles) ->
            // Over the room's own grid and joined once: this runs per creep
            // per candidate, and a `Set<RoomPos>` difference would compare
            // a room name at every node of it.
            workArea atlas task
            |> Set.filter (fun tile ->
                not (tile.Room = room && Set.contains (RoomPos.pos tile) postTiles))
        | _ -> workArea atlas task
    | _ -> workArea atlas task

/// Work Area of a Task for one creep — the body-aware query every reader that
/// has a creep uses, which is `narrowedArea` above once the rooms agree. Empty
/// for a creep standing in a different room from the Task's target (ADR 0041).
/// The body-blind `workArea` stays honest — those tiles are the target's room's
/// and say so — and what this rule narrows is the *permission*: standing and
/// acting are in-room acts, so a creep a border away has nowhere to work this
/// Task from, which is what makes the action gate refuse rather than mislead.
/// The cross-room *price* is a minimum over the Seam band (`pricedAcross`),
/// joined where the rooms are both in hand; the *tiles* stay the creep's own
/// room's, and a caller that wants the far room's origins asks `narrowedArea`.
/// The mover crosses around this query rather than through it: `firstStep`
/// answers the near side of the winning Seam when these tiles are empty, so the
/// action and reachability gates grew no border-crossing answer of their own.
let workAreaFor (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
    if not (sharesRoom atlas creep task) then
        Set.empty
    else
        narrowedArea atlas creep task

/// The controller's upgrade buffers, by id: built containers standing inside
/// a controller's Upgrade Work Area and on no source's Seat — the Layout
/// places one (ADR 0012) and the Withdraw gate reads it (ADR 0019). The
/// Planner spells the same judgement out over the view for its Refill
/// layering, an accepted duplication named in ADR 0019. Total: no controller,
/// none placed or no built container answers with the empty set, which opens
/// the gate rather than closing it (ADR 0004).
let controllerContainers (atlas: Atlas) : Set<string> =
    match atlas.Buffers with
    | Some memo -> memo
    | None ->
        let home = atlas.Home
        let area = upgradeAreaIn atlas home
        let seats = seatUnionIn atlas home

        // The colony's own room, and the container's tile is read out of
        // that room's layer rather than resolved off its id (ADR 0041): a
        // container standing on the same coordinate of an outpost would
        // otherwise test as standing in this controller's Upgrade area.
        let placed = (layerOf atlas home).TargetPositions

        let buffers =
            targetsOfKind atlas (Structure BuiltKind.Container)
            |> List.filter (fun id ->
                match Map.tryFind id placed with
                | Some pos -> Set.contains pos area && not (Set.contains pos seats)
                | None -> false)
            |> Set.ofList

        atlas.Buffers <- Some buffers
        buffers

/// Whether a creep's tile catches its harvest overflow: a built container
/// standing on one of the source's own Seats — the container Post's footing,
/// judged from the same census `posts` reads (ADR 0012). There the engine
/// drops harvest past a full store into the container under the creep, so a
/// full store never ends the dig. A site catches nothing, an unplaced creep
/// or source widens nothing (ADR 0004), and the two have to stand in one room
/// for the answer to mean anything (ADR 0041).
let catchesOverflow (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind sourceId atlas.TargetAt with
    | Some(creepRoom, pos), Some(sourceRoom, _) when creepRoom = sourceRoom ->
        Set.contains pos (tilesOfKindIn atlas creepRoom (Structure BuiltKind.Container))
        && (match seatTilesIn atlas sourceId with
            | Some(_, seats) -> Set.contains pos seats
            | None -> false)
    | _ -> false

/// Whether a creep stands where it could dig a source: in that source's own
/// room and within the engine's harvest range of it. The widened half of
/// `catchesOverflow` above and deliberately weaker (ADR 0048): that one asks
/// whether the tile catches a full store's overflow, which is a fact about the
/// container underfoot, while this asks only whether the creep is in position
/// the tick the energy lands. Measured by range rather than by Seat membership,
/// so a creep the engine has put on ground the projection carries none for is
/// in position all the same (ADR 0004).
let standsAtSource (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind sourceId atlas.TargetAt with
    | Some(creepRoom, tile), Some(sourceRoom, source) when creepRoom = sourceRoom ->
        range tile source <= 1
    | _ -> false

/// The far exit row and column of a room — index 49, the outer of the two
/// the projection's ground stops short of (ADR 0036).
let private exitEdge = Engine.roomSide - 1

/// The tile pairs the engine joins across the border two rooms share, before
/// terrain has a say: this room's exit tile beside the tile a creep stepping
/// onto it lands on, the same coordinate on the opposite row or column.
/// `offset` is the neighbour's world position minus this room's
/// (`RoomName.offsetOf`), so only the four unit steps name a shared border —
/// which is the tile half of the rule `RoomName.neighbouring` states over the
/// names alone. The four corner tiles are left out of every row and column: a
/// corner lies on two borders at once, and the engine makes at most one
/// landing.
let private borderPairs offset : (Pos * Pos) list =
    let alongEdge = [ 1 .. exitEdge - 1 ]

    match offset with
    | 0, -1 -> [ for x in alongEdge -> { X = x; Y = 0 }, { X = x; Y = exitEdge } ]
    | 0, 1 -> [ for x in alongEdge -> { X = x; Y = exitEdge }, { X = x; Y = 0 } ]
    | -1, 0 -> [ for y in alongEdge -> { X = 0; Y = y }, { X = exitEdge; Y = y } ]
    | 1, 0 -> [ for y in alongEdge -> { X = exitEdge; Y = y }, { X = 0; Y = y } ]
    | _ -> []

/// The Seam band joining two rooms: the passable exit-tile pairs, each this
/// room's border tile beside the tile it lands a creep on in the neighbour (ADR
/// 0041). The third kind of geometry beside the Seat and the Post — those are
/// tiles a creep works from, a Seam is one it can only pass through — and never
/// a tile anything offers to stand on: it is answered from the border layer,
/// which enters no walking grid, walkable or buildable set and no Work Area, so
/// the Matcher cannot pick one and have the engine empty it the tick a creep
/// arrives. Deterministic (X, Y) order, total (ADR 0004).
let seams (atlas: Atlas) (fromRoom: string) (toRoom: string) : (Pos * Pos) list =
    match RoomName.offsetOf fromRoom toRoom with
    | Some offset ->
        let near = ringOf atlas fromRoom
        let far = ringOf atlas toRoom

        borderPairs offset
        |> List.filter (fun (here, there) -> walkableAt near here && walkableAt far there)
    | None -> []

/// Whether a creep stands on a Seam — its room's border ring, the tile the
/// engine put it down on the tick it crossed. Read off the coordinate alone;
/// total (ADR 0004).
let standsOnSeam (atlas: Atlas) (creep: string) : bool =
    match Map.tryFind creep atlas.CreepAt with
    | Some(_, pos) -> pos.X = 0 || pos.X = exitEdge || pos.Y = 0 || pos.Y = exitEdge
    | None -> false

/// The tiles of a room's own ground next to one of its exit tiles — the only
/// tiles a flood can price a Seam's near side from, or step off its far side
/// onto, because the border ring is not ground and no flood ever enters it (ADR
/// 0036, ADR 0041). Diagonals included: the engine lets a creep step onto an
/// exit diagonally, and onto its first tile in the new room the same way.
/// Clipped to the room's *ground* and not merely to the grid: the answer is the
/// same either way, but a resumable flood asked about a tile nothing reaches
/// settles the whole room, and half of every exit's neighbourhood is more ring.
let private besideExit (grid: int[]) (tile: Pos) : Pos list =
    neighbours tile |> List.filter (walkableAt grid)

/// The same tiles for the leg a flood is *seeded* on, which is one tile wider:
/// a flood seeds its origin whatever that tile weighs, so a creep the engine
/// parked on the border ring the tick it crossed reaches the crossings beside
/// it at no cost — the one tile off a room's ground a near leg can honestly be
/// read at.
let private besideExitFrom (grid: int[]) (origin: Pos) (tile: Pos) : Pos list =
    if walkableAt grid origin then
        besideExit grid tile
    else
        neighbours tile
        |> List.filter (fun near -> near = origin || walkableAt grid near)

/// The cheapest a flood reached any tile of a set at, and None when it reached
/// none of them — the one read every arrival at a set of tiles is taken
/// through, so a Work Area, a Seam band and a sink's approach are one question
/// asked of three tile sets and the near leg's arrival is the same arithmetic
/// however the far leg is joined. Unreachable is an absence and never a number
/// (ADR 0004).
let private nearestReached (reached: Pos -> int) (tiles: Pos list) : int option =
    tiles
    |> List.choose (fun tile ->
        let d = reached tile
        if d = unreached then None else Some d)
    |> function
        | [] -> None
        | costs -> Some(List.min costs)

/// What this body pays to step onto an exit tile, priced by the same rule every
/// other step is (ADR 0029's walk, travel cost's units) — the narrowing of ADR
/// 0041's literal `+1`, which is one tick only for a plain exit under a body at
/// fatigue parity, and a swamp exit is not free. Read off the border ring, the
/// only terrain the projection has for an exit, and priced at the bare step,
/// the ring carrying no road to discount. None for an exit the projection has
/// no terrain for, a wall, or a body that cannot step at all (ADR 0004).
let private exitPrice (atlas: Atlas) (stepPrices: int[]) room tile =
    let weight = weightAt (ringOf atlas room) tile

    if weight > 0 then
        let step = stepPrices.[weight]
        if step >= 0 then Some step else None
    else
        None

/// The body a *plan* is priced for: fatigue parity, one fatigue-generating part
/// to one Move (ADR 0003), which under the walk's rounding is a tick on plain
/// and five on swamp.
let private planningFactor: FatigueFactor = { FatigueParts = 1; MoveParts = 1 }

/// The walk out to a Seam, from every tile of one room's ground: the smallest,
/// over the whole band joining that room to the named neighbour, of the walk to
/// a tile beside a crossing plus the price of stepping onto the crossing
/// itself.
let private seamWalkFlood (atlas: Atlas) (fromRoom: string) (toRoom: string) : int[] =
    memoised atlas.SeamWalks (fromRoom, toRoom) (fun () ->
        let weights = weightsOf atlas fromRoom
        let stepPrices, traffic = pricingOf noTraffic planningFactor Walk

        seams atlas fromRoom toRoom
        |> List.collect (fun (exitTile, _) ->
            match exitPrice atlas stepPrices fromRoom exitTile with
            | None -> []
            | Some crossing ->
                besideExit weights exitTile
                |> List.choose (fun tile ->
                    entryCost weights traffic stepPrices tile
                    |> Option.map (fun cost -> tile, cost + crossing)))
        |> floodFromAllSeeded weights traffic stepPrices
        |> drained
        |> fst)

/// The walk in whole ticks from one tile of a room's own ground out to the Seam
/// joining it to a neighbour — a walk *to* the border and not across it, the
/// near half of `pricedAcross` with the far leg left off. No creep ever walks
/// it: it is the anchor ADR 0042's outpost container pick is made against, an
/// outpost having no spawn for a trunk to anchor on. Total (ADR 0004): `None`
/// for two rooms with no band between them, a room the projection carries no
/// ground for, a tile off the grid and a tile no crossing reaches — an
/// unpriceable Seam is no Seam, never a blocked one.
let seamWalkTicks (atlas: Atlas) (fromRoom: string) (toRoom: string) (from: Pos) : int option =
    if not (inGrid from) then
        None
    else
        let stepPrices, traffic = pricingOf noTraffic planningFactor Walk
        let reached = (seamWalkFlood atlas fromRoom toRoom).[indexOf from]

        if reached = unreached then
            None
        else
            entryCost (weightsOf atlas fromRoom) traffic stepPrices from
            |> Option.map (fun own -> reached - own)

/// The far leg's flood for one Task and one body, memoised colony-wide: the
/// price, from every tile of the target's room, of stepping onto that tile
/// and walking in to the Task's Work Area there (`floodPricedInto`). Its
/// origin is the target, so one entry answers every creep the colony prices
/// this Task for — ADR 0041's reason the cross-room walk is a minimum over
/// additions rather than over floods.
let private farFlood (atlas: Atlas) (pricing: Pricing) (creep: string) (room: string) (task: Task) =
    let factor = factorOf atlas creep

    memoised atlas.FarFloods (room, task, workHeavy atlas creep, factor, pricing) (fun () ->
        floodPricedInto
            (weightsOf atlas room)
            (occupiedOf atlas room)
            factor
            pricing
            (narrowedArea atlas creep task |> RoomPos.tilesIn room))

/// The near leg of a cross-room join, in the two shapes its callers hand it:
/// the tick's own per-creep flood, which the join may push further, and one
/// some caller already settled whole, which it may only read. Both answer
/// what a tile is finally reached at and what it cannot possibly beat, so the
/// join below is written once (ADR 0030).
type private NearLeg =
    /// The resumable memo of one creep under one pricing: a read may cost
    /// relaxation, and the whole of #176 is asking for as few of them as
    /// the answer allows.
    | Resuming of Flood
    /// A flood already drained — the hauler quota's own legs. Every read
    /// is final and free, so the bound below is exact and prunes nothing
    /// that could have won.
    | Drained of int[]

/// What a near leg finally reaches a tile at — `unreached` for a tile
/// nothing reaches, and never for one merely unsettled (#174).
let private reachedOn (leg: NearLeg) : Pos -> int =
    match leg with
    | Resuming flood -> reachedBy flood
    | Drained dist -> reachedIn dist

/// A lower bound on what the near leg will finally reach the cheapest tile of a
/// set at, taken without advancing it a single pop — the licence for the early
/// stop, and the argument that it moves no answer. Per tile: the flood's own
/// frontier bounds every tile it has not settled (`frontierOf`), while a
/// settled tile already holds its final number and the grid read is an upper
/// bound on it, so `min(frontier, glimpse)` is at or below the tile's final
/// distance either way, and the smallest over the set is at or below the set's
/// own minimum. Both halves matter: a tile settled cheaply while the flood ran
/// past it toward another crossing sits *below* the frontier, and adjacent
/// crossings share their approach tiles. An empty set bounds at `unreached`,
/// which keeps the addition out of overflow.
let private boundOn (leg: NearLeg) (tiles: Pos list) : int =
    match leg, tiles with
    | _, [] -> unreached
    | Drained dist, _ ->
        tiles |> List.fold (fun bound tile -> min bound (reachedIn dist tile)) unreached
    | Resuming flood, _ ->
        min
            (frontierOf flood)
            (tiles
             |> List.fold (fun bound tile -> min bound (glimpsedBy flood tile)) unreached)

/// A cross-room price, joined on the Seam: the smallest, over the whole band
/// between the two rooms, of *walk to the exit tile* + *the exit tile's own
/// price* + *walk in from the tile it lands on* (ADR 0041). Each leg is a
/// single-room flood the caller has already run, so no flood ever leaves its
/// room and the join is a minimum over thirty-odd additions rather than over
/// thirty-odd floods. The join itself and not a second one (ADR 0030): a creep
/// priced toward a Task (`pricedAcross`) and the hauler quota's round trip
/// (`haulRoundTripTicks`) differ in nothing but which two floods they hand it,
/// and a lead's cast walk folds the same three terms into the seeds of one
/// flood (`castAcross`), so a change here is a change there. What the two
/// floods owe is fixed: the near one is `fromRoom`'s and charges every tile it
/// enters, the far one is run *into* its goals with each goal seeded at its own
/// entry cost (`floodPricedInto`), and a far leg flooded the ordinary way round
/// leaves the sum short by a tile every time. **The convention**: a step costs
/// what the tile it *lands on* costs, so exactly three things are charged
/// beyond the two floods' interiors — the exit tile, the far room's first tile,
/// and the Work-Area tile the walk ends on — and the landing tile nothing,
/// which charges every tile the creep steps onto once and none twice. It is a
/// tile cheaper than a flood over the two rooms laid side by side, and that
/// tile is real: crossing a border displaces a creep twice for one move. Total
/// (ADR 0004). The winning exit tile comes back beside the price, so the mover
/// aims a crossing creep at the Seam it was ranked on and no second argmin can
/// split a tie; the minimum is over `(sum, exit)` pairs, so the price is the
/// number it always was and ties fall to the lowest (X, Y) exit.
let private joinedAcross
    (atlas: Atlas)
    (pricing: Pricing)
    (factor: FatigueFactor)
    (fromRoom: string)
    (from: Pos)
    (toRoom: string)
    (band: (Pos * Pos) list)
    (near: NearLeg)
    (far: Pos -> int)
    : (int * Pos) option =
    // One price table for the whole band: every crossing in it is priced
    // for the same body under the same pricing (#168).
    let stepPrices, _ = pricingOf noTraffic factor pricing
    let nearGround = weightsOf atlas fromRoom
    let farGround = weightsOf atlas toRoom

    // Everything but the near leg, priced first, and the band ordered by it.
    // The far leg is a flood settled whole and the exit's own price a table
    // read, so this costs the band a read apiece and no relaxation — and it is
    // what makes the bound below bite early.
    let crossings =
        band
        |> List.choose (fun (exitTile, landing) ->
            match
                exitPrice atlas stepPrices fromRoom exitTile,
                nearestReached far (besideExit farGround landing)
            with
            | Some crossing, Some departure ->
                Some(crossing + departure, exitTile, besideExitFrom nearGround from exitTile)
            | _ -> None)
        // On the sum of the two terms alone, a primitive key over a list the
        // band already ordered: the answer does not depend on this order —
        // every crossing left unsettled is one the bound proved cannot win —
        // so ordering is work saved, never what is answered.
        |> List.sortBy (fun (rest, _, _) -> rest)

    // One closure for the whole band, not one per crossing: the read is
    // the same read every time and the band is walked a crossing at a
    // time (#168).
    let reached = reachedOn near
    let mutable best = None

    for rest, exitTile, approach in crossings do
        let bound = boundOn near approach

        let worthSettling =
            bound < unreached
            && match best with
               // At or under the best sum and not merely under: the answer
               // is the smallest `(sum, exit)` pair and not the smallest
               // sum, so a crossing that can only tie still has to be looked
               // at, the tie falling to the lowest exit.
               | Some(bestSum, _) -> bound + rest <= bestSum
               | None -> true

        if worthSettling then
            match nearestReached reached approach with
            | None -> ()
            | Some arrival ->
                let sum = arrival + rest

                match best with
                | Some(bestSum, bestExit) when
                    bestSum < sum || (bestSum = sum && bestExit <= exitTile)
                    ->
                    ()
                | _ -> best <- Some(sum, exitTile)

    best

/// A creep's cross-room price toward a Task: the join above over this creep's
/// own memoised flood and the Task's far leg, the one flooded out of the
/// target and shared colony-wide, so a second creep pricing the same Task
/// across the same border pays for no second flood (ADR 0041). The band is
/// read before either flood is forced, a pair of rooms with no Seam having
/// nothing to pay for (ADR 0004).
let private pricedAcross
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (creepRoom: string)
    (from: Pos)
    (targetRoom: string)
    : (int * Pos) option =
    match seams atlas creepRoom targetRoom with
    | [] -> None
    | band ->
        let near = flood atlas pricing creepRoom creep from
        let far = farFlood atlas pricing creep targetRoom task

        joinedAcross
            atlas
            pricing
            (factorOf atlas creep)
            creepRoom
            from
            targetRoom
            band
            (Resuming near)
            (reachedIn far)

/// The cheapest path from a creep to a set of tiles under one pricing — the
/// shape travel cost and the walk share, so the two can disagree on what a step
/// costs and on nothing else (ADR 0029). The tiles are the caller's, not a
/// Task's: what a creep may stand on this tick is the decision layer's
/// judgement, which takes a Reach out of a Work Area and gives Flee an area of
/// its own (ADR 0033). A creep the projection cannot place prices at 0; an
/// empty or unreachable set has no price at all (ADR 0004).
let private pricedPathTo
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (area: Set<RoomPos>)
    : int option =
    match Map.tryFind creep atlas.CreepAt with
    | None -> Some 0
    | Some(room, pos) ->
        // Read, never rebuilt: this runs once per creep per candidate Task in
        // the Matcher, so the room filter is `RoomPos.tilesIn`'s fold into a
        // list rather than a second `Set<RoomPos>`, and the membership test
        // goes the other way — one joined key against the caller's own set.
        if Set.contains (RoomPos.at room pos) area then
            Some 0
        else
            let here = RoomPos.tilesIn room area

            if List.isEmpty here then
                None
            else
                nearestReached (reachedBy (flood atlas pricing room creep pos)) here

/// The border a Task asks its creep to cross, or None when it asks for none:
/// the creep's room, the tile it stands on, and the target's room, once the
/// two names have been read and found different (ADR 0041). One spelling,
/// because the price (`pricedPath`) and the mover's step (`stepAcross`) must
/// settle the rooms alike. Absence is not a crossing — a Task acting on
/// nothing, an unplaced creep and an unplaced target each keep the permissive
/// reading `sharesRoom` gives (ADR 0004).
let private borderCrossing
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    : (string * Pos * string) option =
    match actionOn task with
    | None -> None
    | Some(targetId, _) ->
        match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
        | Some(creepRoom, from), Some(targetRoom, _) when creepRoom <> targetRoom ->
            Some(creepRoom, from, targetRoom)
        | _ -> None

/// The same path priced for a Task: over the Task's own Work Area, and with the
/// one escape a bare tile set cannot carry — a target the projection does not
/// place prices at 0 rather than reading as unreachable geometry (ADR 0004). A
/// Task in an unprojected room prices at 0 too: it never counts against the
/// creep and, having no Work Area, never lets it act.
let private pricedPath (atlas: Atlas) (pricing: Pricing) (creep: string) (task: Task) : int option =
    match actionOn task with
    | Some(targetId, _) when not (Map.containsKey targetId atlas.TargetAt) -> Some 0
    | _ ->
        match borderCrossing atlas creep task with
        | Some(creepRoom, from, targetRoom) ->
            pricedAcross atlas pricing creep task creepRoom from targetRoom
            |> Option.map fst
        | None -> pricedPathTo atlas pricing creep (workAreaFor atlas creep task)

/// Travel cost of a Task for a creep (ADR 0002, revised by ADRs 0006 and 0010):
/// the cost units — half-ticks — the creep's body needs along a cheapest path
/// to any Work Area tile, terrain weights scaled by the body's fatigue factor
/// and tiles under standing creeps priced `occupancyPenalty` dearer; 0 for a
/// creep already inside. None — a placed Work Area the creep cannot reach, or
/// an empty one — makes the Task inapplicable to that creep. An unplaced creep
/// or target prices at 0 (ADR 0004). A ranking price and nothing else (ADR
/// 0029): it breaks rank ties in the Matcher, and halving it is not the walk.
let travelCost (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas TravelCost creep task

/// Travel cost to an explicit set of tiles: the same ranking price over the
/// area the caller hands in rather than the one the Task derives — what prices
/// a Task over the tiles the Reach left it, and Flee, whose Work Area is the
/// safe set and no target's surroundings (ADR 0033). An unplaced creep prices
/// at 0; with no target there is no unplaced-target escape.
let travelCostWithin (atlas: Atlas) (creep: string) (area: Set<RoomPos>) : int option =
    pricedPathTo atlas TravelCost creep area

/// The creep's walk to a Task's Work Area (ADR 0029): the whole ticks its body
/// needs along a cheapest path, every step floored at one tick and today's
/// standing creeps priced at nothing — the horizon every time-aware judgement
/// is made at. Beside travel cost, not derived from it: a clock must not read a
/// crowd that will have moved on, nor price a tile below the tick it takes to
/// cross. 0 for a creep already inside the area, and a missing walk reads as
/// "no arrival" (ADR 0004).
let walkTicks (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas Walk creep task

/// Whether a creep may perform its Task's action this tick: standing inside the
/// Task's Work Area for its body at tick start (ADR 0020) — a creep acts only
/// from where it may stand, which for a Work-heavy harvester is its Post, so
/// the gate keeps such a body empty on the way there and a full store never
/// ends the walk. Two permissive escapes keep the query total (ADR 0004): a
/// creep or target the projection cannot place never blocks the action, and
/// neither does a creep standing on a tile the projection calls impassable — an
/// obstacle-type site dropped under it — judged by range instead.
let mayAct (atlas: Atlas) (creep: string) (task: Task) (area: Set<RoomPos>) : bool =
    match actionOn task with
    | None -> false
    // No action reaches across a border: the engine's ranges are measured
    // inside one room, and `range` takes two tiles of one grid (ADR 0052
    // decision 2). Asked here rather than inferred from an empty area, which
    // is what keeps the gate shut while the mover walks a creep at the Seam:
    // it opens by itself the tick the engine puts the creep down.
    | Some _ when not (sharesRoom atlas creep task) -> false
    // The range escape is measured against every tile the action reaches
    // from, which for a [[refill cluster]] is its hungry members and for
    // every other Task is the one target it always was (ADR 0054).
    | Some(_, actionRange) ->
        match Map.tryFind creep atlas.CreepAt, actionTilesOf atlas task with
        | Some(creepRoom, creepPos), Some(_, targetTiles) ->
            if not (walkableAt (weightsOf atlas creepRoom) creepPos) then
                targetTiles |> List.exists (fun target -> range creepPos target <= actionRange)
            else
                Set.contains (RoomPos.at creepRoom creepPos) area
        | _ -> true

/// The structure a Refill's transfer actually names (ADR 0054), which for
/// every Refill but the [[refill cluster]]'s is its target and for that one
/// is decided **here, at arrival**: the hungry member nearest the tile the
/// body is standing on, ties by id. This is the whole of what makes a ring of
/// ten extensions one Task — the Planner names a place and the Emitter names
/// the structure, so an extension somebody else topped up while this body
/// walked costs it a neighbour and not its Task. Range-bounded by the
/// action's own reach and total the way `mayAct` is (ADR 0004): a body
/// through the gate ahead either stands inside the hungry members' rings, in
/// which case one really is within reach, or could not be placed at all, in
/// which case the range query sees nothing and the cluster's own hungry pick
/// answers, the spawn first. It takes the Refill's structure id and not the
/// whole Task, so the only way to answer `None` is a cluster with nothing left
/// to pour into, which the Emitter reads as the silence a drained Harvest
/// keeps.
let refillTarget (atlas: Atlas) (creep: string) (structureId: string) : string option =
    match clusterOf atlas (Refill structureId) with
    | None -> Some structureId
    | Some cluster ->
        let hungry = RefillCluster.hungry cluster

        let inReach =
            match actionOn (Refill structureId), Map.tryFind creep atlas.CreepAt with
            | Some(_, actionRange), Some(creepRoom, creepPos) ->
                hungry
                |> List.choose (fun id ->
                    match Map.tryFind id atlas.TargetAt with
                    | Some(room, tile) when room = creepRoom && range creepPos tile <= actionRange ->
                        Some(range creepPos tile, id)
                    | _ -> None)
            | _ -> []

        match inReach, hungry with
        // Nearest first and the lower id after it: a tuple's own order is
        // the tie-break, so two members equally close resolve the way every
        // other id-ordered rule here does.
        | _ :: _, _ -> inReach |> List.min |> snd |> Some
        | [], [] -> None
        | [], first :: _ ->
            Some(
                if List.contains cluster.Spawn hungry then
                    cluster.Spawn
                else
                    first
            )

/// First step toward a set of goal tiles under one pricing: the in-room half
/// of `firstStep`'s contract, whose doc governs the floods, the tie-breaking
/// and the totality here. Only that half — the border-crossing fallback is
/// the public wrappers' own — so a creep whose target is a room away answers
/// `None` here.
let private firstStepVia
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (goalTiles: Set<RoomPos>)
    : RoomPos option =
    match Map.tryFind creep atlas.CreepAt with
    | None -> None
    | Some(room, pos) ->
        // The creep's own room's share, for the reason `pricedPathTo`
        // narrows there — and read the same way, without building a second
        // set: a step is a step inside one room, and this room's grid is
        // what the flood indexes (ADR 0052 decision 2).
        let goals = RoomPos.tilesIn room goalTiles

        if List.isEmpty goals || Set.contains (RoomPos.at room pos) goalTiles then
            None
        else
            let near = flood atlas pricing room creep pos

            goals
            |> List.choose (fun goal ->
                let d = reachedBy near goal
                if d = unreached then None else Some(d, goal))
            |> function
                | [] -> None
                | reachable ->
                    let _, goal = List.min reachable
                    Some(RoomPos.at room (posAt (firstStepOn near (indexOf pos) (indexOf goal))))

/// The step a creep takes toward a Task whose target stands in another room:
/// toward the near side of the Seam the price was paid at. The exit tile is the
/// creep's *own* room's border tile, so aiming at it asks nothing of the
/// neighbour and arbitrates nothing across the Seam — ADR 0041's boundary
/// stands exactly where it stood — and the engine puts the creep down in the
/// neighbour at the end of the tick it steps on, from where every rule already
/// written takes it on. The exit is the one `pricedAcross` won on, taken out of
/// that same minimisation rather than looked for again: a second argmin agrees
/// on every number and splits on every tie, which walks a creep to one crossing
/// while ranking it at another. Total (ADR 0004): no crossing, no step.
let private stepAcross
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    : RoomPos option =
    match borderCrossing atlas creep task with
    | None -> None
    | Some(creepRoom, from, targetRoom) ->
        pricedAcross atlas pricing creep task creepRoom from targetRoom
        |> Option.bind (fun (_, exitTile) ->
            // The near side the price was taken over, origin and all
            // (`besideExitFrom`, #175): a creep the engine parked on the
            // ring beside the winning crossing steps onto it from there,
            // exactly as it was priced to.
            let approach = besideExitFrom (weightsOf atlas creepRoom) from exitTile

            if List.contains from approach then
                Some(RoomPos.at creepRoom exitTile)
            else
                firstStepVia atlas pricing creep (RoomPos.setAt creepRoom (Set.ofList approach)))

/// The first step of a cheapest path from a creep to a set of goal tiles,
/// priced in the creep's own cost — a slow body may detour differently than a
/// fast one over the same ground. The goals are the caller's: a mover is handed
/// the tiles it may stand on this tick, its Work Area less the Reach and, for
/// Flee, the safe set (ADR 0033). None when there is nothing derivable: the
/// creep is unplaced, already inside the goals, or they are empty or
/// unreachable. Of equally cheap goals the lowest (cost, tile) wins, matching
/// the flood's tie-breaking. The goals are read as tiles of the creep's own
/// room (ADR 0041); a creep on the border ring is not on ground and still gets
/// a step, the flood seeding its start tile regardless of weight. The Task
/// rides beside them for the one case the goal tiles cannot carry: a target
/// filed under another room name leaves the creep-aware Work Area empty, and
/// the step is then toward the near side of the winning Seam.
let firstStep (atlas: Atlas) (creep: string) (task: Task) (goals: Set<RoomPos>) : RoomPos option =
    match firstStepVia atlas TravelCost creep goals with
    | Some step -> Some step
    | None -> stepAcross atlas TravelCost creep task

/// The same first step toward an explicit set of tiles, with no Task beside it:
/// `firstStep`'s answer for a body that has none to cross a Seam for, which is
/// what an idle one stepping off the [[working ground]] is (#241). `travelCost`
/// and `travelCostWithin` stand in the same pair for the same reason — the Task
/// buys the cross-room fallback and nothing else, so a caller whose goals are
/// tiles of the creep's own room by construction has no use for it.
let firstStepWithin (atlas: Atlas) (creep: string) (goals: Set<RoomPos>) : RoomPos option =
    firstStepVia atlas TravelCost creep goals

/// The first step the same body would take were no tile occupied — the
/// traffic-blind route, otherwise priced exactly like `firstStep`. The Resolver
/// compares the two: a difference attributes the detour to the occupancy
/// surcharge, the only pricing the two floods do not share (ADR 0008, ADR
/// 0009). Off the shared memo under the Baseline pricing (ADR 0030); the entry
/// is lazy and the Resolver asks only for creeps on the verbose list, so a tick
/// that watches nobody floods for nobody (ADR 0018).
let firstStepIgnoringTraffic
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (goals: Set<RoomPos>)
    : RoomPos option =
    match firstStepVia atlas Baseline creep goals with
    | Some step -> Some step
    | None -> stepAcross atlas Baseline creep task

/// Round-trip haul cost in whole ticks for a body between a container's tile
/// and a sink structure's tile (ADR 0012): the leg out prices every Carry part
/// loaded, the leg back prices them all empty, both over travel cost's weights
/// but traffic-blind — the hauler quota this feeds is capacity planning, not
/// routing, and today's standing creeps must never resize the fleet. Goals are
/// the sink's adjacent walkable tiles; the origin prices 0 as every flood
/// origin does. Each leg is priced as a walk (ADR 0029), so the two simply sum.
/// None when no goal is reachable (ADR 0004). Both ends carry their rooms (ADR
/// 0052 decision 2), and an outpost's container is priced across the border
/// rather than walked over home terrain (ADR 0042): each leg is then
/// `joinedAcross`, the same minimum the Matcher and the mover read, and never a
/// second cross-room arithmetic of this rule's own (ADR 0030). **Two crossings
/// and not one**, because the two legs are two journeys (ADR 0029): the loaded
/// factor and the empty one price a swamp exit differently. Both legs are
/// flooded out of the container and in to the sink, the leg *back* being the
/// same direction priced on the empty body — reversing it would charge the sink
/// room's exit rather than the container room's.
let haulRoundTripTicks
    (atlas: Atlas)
    (body: BodyPart list)
    (container: RoomPos)
    (sink: RoomPos)
    : int option =
    let count part =
        body |> List.filter ((=) part) |> List.length

    let fromRoom = container.Room
    let sinkRoom = sink.Room
    let from = RoomPos.pos container
    let goals = adjacentWalkableIn atlas sinkRoom (RoomPos.pos sink)
    let weights = weightsOf atlas fromRoom

    let legTicks factor =
        if fromRoom = sinkRoom then
            let dist, _ = walkFloodFrom weights factor from
            nearestReached (reachedIn dist) goals
        else
            match seams atlas fromRoom sinkRoom with
            | [] -> None
            | band ->
                let near, _ = walkFloodFrom weights factor from
                let far = floodPricedInto (weightsOf atlas sinkRoom) noTraffic factor Walk goals

                joinedAcross
                    atlas
                    Walk
                    factor
                    fromRoom
                    from
                    sinkRoom
                    band
                    (Drained near)
                    (reachedIn far)
                |> Option.map fst

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

/// A cast walk carried across a Seam and on into every tile of the far room at
/// once: the answer `joinedAcross` gives for one goal, given for all of them by
/// one flood. The three terms are the same three, charged to the same tiles,
/// but read forwards rather than summed backwards — every tile the far room
/// puts a creep down on is *seeded* at what it costs to arrive standing on it,
/// so flooding on from there charges each further tile once and a tile `g`
/// answers the whole lead to `g`. Why this shape and not the join: a lead's far
/// leg is flooded out of the *goal*, so the join pays one flood per goal tile,
/// and `expiring` asks for a lead per creep twice a tick. Seeded from the band
/// instead, the flood does not depend on the goal at all, which is what lets
/// the answer go in the walk table under the census (ADR 0032).
let private castAcross
    (atlas: Atlas)
    (factor: FatigueFactor)
    (near: int[])
    (band: (Pos * Pos) list)
    (goalRoom: string)
    : int[] =
    let weights = weightsOf atlas goalRoom
    let homeGround = weightsOf atlas atlas.Home
    let stepPrices, traffic = pricingOf noTraffic factor Walk

    band
    |> List.collect (fun (exitTile, landing) ->
        match
            nearestReached (reachedIn near) (besideExit homeGround exitTile),
            exitPrice atlas stepPrices atlas.Home exitTile
        with
        | Some approach, Some crossing ->
            besideExit weights landing
            |> List.choose (fun tile ->
                entryCost weights traffic stepPrices tile
                |> Option.map (fun cost -> tile, approach + crossing + cost))
        | _ -> [])
    |> floodFromAllSeeded weights traffic stepPrices
    |> drained
    |> fst

/// The walk in whole ticks a freshly cast body needs to stand on a tile (ADR
/// 0026) — the half of a lead that is paid after the spawner is done. Keyed on
/// a body rather than a creep name, because the body has not been cast yet and
/// nothing in the projection carries its factor. Priced empty, as a creep
/// leaves the spawner, and starting on the tiles *beside* the spawner, since
/// the engine places a finished creep on a free neighbour for no step, and
/// charging it would buy a lead ticks the replacement never walks.
/// Traffic-blind like the hauler quota's round trip — a lead is planning, not
/// routing, and the goal is the very tile the creep being replaced stands on.
/// Priced as a walk (ADR 0029). None when the goal is unreachable, and none
/// when the spawner has no free neighbour to be born on (ADR 0004). The spawner
/// floods its own room and the *goal's* room is the caller's, because a row's
/// creeps do not all live at home: an outpost's Post hires its Anchor off the
/// home row (ADR 0042) and a reserver's whole life is the far side of a Seam,
/// so a lead that could only price home tiles left ADR 0026's succession
/// switched off for exactly those creeps. A goal across a border is the minimum
/// over the Seam band, the one join every cross-room price is read off (ADR
/// 0030), through `castAcross` for the reason written there.
let castWalkTicks
    (atlas: Atlas)
    (body: BodyPart list)
    (spawnTile: Pos)
    (target: RoomPos)
    : int option =
    let factor = emptyFactorOf body
    let spawn = spawnTile
    let goalRoom = target.Room
    let goal = RoomPos.pos target

    let arrival (table: int[]) =
        match table.[indexOf goal] with
        | d when d = unreached -> None
        | d -> Some d

    // The near leg, and the whole of a home-room lead: the flood out of the
    // tiles beside the spawner, over the colony's own room's weights,
    // recalled from the plan memo while the census holds (ADR 0032).
    let near () =
        memoised atlas.Walks (spawn, factor, atlas.Home) (fun () ->
            let dist, _ =
                walkFloodFromAll
                    (weightsOf atlas atlas.Home)
                    factor
                    (adjacentWalkableIn atlas atlas.Home spawn)

            dist)

    if goalRoom = atlas.Home then
        arrival (near ())
    else
        // Not `memoised`: a miss has to read the band first and answer
        // absent without writing anything, which that shape cannot do — it
        // fills every key it is asked with. The lookup still comes first, so
        // the band is walked once per census rather than once per ask.
        match atlas.Walks.TryGetValue((spawn, factor, goalRoom)) with
        | true, table -> arrival table
        | _ ->
            match seams atlas atlas.Home goalRoom with
            | [] -> None
            | band ->
                let table = castAcross atlas factor (near ()) band goalRoom
                atlas.Walks.[(spawn, factor, goalRoom)] <- table
                arrival table

/// Cheapest raw-terrain path for a trunk road (ADR 0011): plain 2, swamp
/// `Tuning.TrunkSwampWeight` — no road discount and no occupancy surcharge, so
/// the line neither shifts as its own roads get built nor bends around today's
/// traffic. Walls, obstacle structures and the `avoid` tiles (the Layout's
/// reservations) are impassable; the origin prices 0 though it cannot be stood
/// on, a source sitting in wall terrain. Answers the path tiles from the first
/// step beside the origin to the cheapest reachable goal, or [] when no goal is
/// reachable — and that trunk is *recorded* rather than dropped. Deterministic
/// through the flood's heap keys and the lowest (cost, tile) goal.
let trunkPath
    (atlas: Atlas)
    (avoidTiles: Set<RoomPos>)
    (start: RoomPos)
    (goalTiles: Set<RoomPos>)
    : RoomPos list =
    let room = start.Room
    let origin = RoomPos.pos start

    // Both sets are read and never rebuilt: the Layout asks for a trunk per
    // source per goal on a census tick, and a room's share taken as a fresh
    // `Set<Pos>` at each ask is a copy of the reservation per line
    // (`RoomPos.tilesIn`). Only the room's own tiles are taken.
    let avoid = RoomPos.tilesIn room avoidTiles
    let goals = RoomPos.tilesIn room goalTiles
    // Raw terrain is the *price*, never what blocks: the trunk starts from the
    // ground grid — plain 2, swamp `Tuning.TrunkSwampWeight`, wall -1, and no
    // road discount, the walking grid's one disqualifying difference — and then
    // takes the obstacle pass back off the layer, because a rampart or a spawn
    // standing in the line is as impassable to a planned road as a wall (ADR
    // 0011).
    let weights = Array.copy (groundOf atlas room)

    // The swamp repriced for a road before the obstacle pass, so a swamp under
    // an obstacle still reads -1 after it. Held at `Engine.swampWeight` where
    // the tunable is read past it: `stepTable` is sized to that weight, so a
    // heavier one would index off its end — an exception on .NET and an
    // `undefined` price through `at`'s `[<Emit>]` accessor on the deployed
    // bundle.
    let trunkSwamp = min Engine.swampWeight atlas.Tuning.TrunkSwampWeight

    for index in 0 .. weights.Length - 1 do
        if weights.[index] = Engine.swampWeight then
            weights.[index] <- trunkSwamp

    (layerOf atlas room).Obstacles
    |> Set.iter (fun tile -> weights.[indexOf tile] <- -1)

    // Through the grid's guard, unlike the pass above: `avoid` is the Layout's
    // own reservation set rather than the projection's geometry, and `indexOf`
    // checks nothing (#173) — so a tile off the fifty-by-fifty reserves
    // nothing instead of indexing off the array.
    avoid
    |> List.iter (fun tile ->
        if inGrid tile then
            weights.[indexOf tile] <- -1)

    let dist, parents =
        floodFrom weights noTraffic (stepTable (stepUnits planningFactor)) origin

    goals
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
                    walk parents.[index] (RoomPos.at room (posAt index) :: acc)

            walk (indexOf goal) []
