/// The room grid and the Dijkstra flood over it: tile indexing, terrain and
/// fatigue pricing, and the resumable flood the Atlas advances. Knows nothing
/// of a colony or of the Atlas — it prices raw arrays, which is what lets the
/// Atlas keep its own representation private.
module Fabot.Core.Grid

open Fabot.Core.Types
open Fable.Core

type internal Pricing =
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
type internal Flood =
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

let internal tileCount = Engine.roomSide * Engine.roomSide
let internal indexOf pos = pos.X * Engine.roomSide + pos.Y

let internal posAt index =
    {
        X = index / Engine.roomSide
        Y = index % Engine.roomSide
    }

/// Unreached marker in a flood's distance array.
let internal unreached = System.Int32.MaxValue

/// Extra cost priced onto a step landing on a tile some creep occupies this
/// tick — one swamp step by definition (ADR 0008, ADR 0010): a crowd usually
/// means waiting, so a modest detour is preferred; the tile stays passable,
/// so traffic never makes a Task inapplicable.
let private occupancyPenalty = Engine.swampWeight

/// No tile occupied: the flood baseline the occupancy surcharge is judged
/// against — the ground the walk is priced over (ADR 0029), and the ground
/// the `Baseline` pricing the attribution compares against is priced over
/// (ADR 0030).
let internal noTraffic: bool[] = Array.create tileCount false

/// The grid of a room the projection does not carry: every tile impassable,
/// read a whole room at a time (ADR 0004, ADR 0041). Absence of a room and
/// absence of every tile in it are one answer — unpriceable geometry, never
/// blocked geometry. Shared by all three grids and never written.
let internal noGround: int[] = Array.create tileCount -1

/// Whether a tile is one of the room's own fifty-by-fifty — the guard every
/// grid read passes through, because a `Pos` off the grid indexes off the
/// array: under Fable that reads `undefined`, which the weight comparisons
/// would call walkable, while .NET throws.
let internal inGrid (tile: Pos) =
    tile.X >= 0
    && tile.X < Engine.roomSide
    && tile.Y >= 0
    && tile.Y < Engine.roomSide

/// The eight tiles touching this one, in (X, Y) order — the order every answer
/// derived from them is listed in. Written out rather than generated, this
/// being the innermost list the Atlas builds.
let internal neighbours pos =
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
let internal terrainWeight terrain =
    match terrain with
    | Plain -> 2
    | Swamp -> Engine.swampWeight
    | Wall -> -1

/// A creep's fatigue factor from its body and current load: every part
/// except Move and except empty Carry generates fatigue — the engine
/// loads Carry parts 50 energy apiece, and the empty ones ride free.
let internal fatigueFactorOf (creep: CreepInfo) : FatigueFactor =
    let carry = partCount creep.Body Carry
    let loadedCarry = min carry ((creep.Energy + 49) / 50)
    let parts = creep.Body |> Map.toList |> List.sumBy snd

    {
        FatigueParts = parts - partCount creep.Body Move - (carry - loadedCarry)
        MoveParts = partCount creep.Body Move
    }

/// The fatigue factor of a body list carrying nothing — the shape a body
/// leaves the spawner in. Beside `fatigueFactorOf`, which reads a living
/// creep; this one reads a body the projection carries no creep for: the
/// hauler quota's candidate (ADR 0012) and a lead's replacement (ADR 0026).
let internal emptyFactorOf (body: BodyPart list) : FatigueFactor =
    let parts = partsOf body
    let moves = partCount parts Move

    {
        FatigueParts = List.length body - moves - partCount parts Carry
        MoveParts = moves
    }

/// The fatigue factor of the same body carrying a full load — every part but
/// Move generating fatigue, the empty Carry's free ride spent. Beside
/// `emptyFactorOf` because the two are one body's two journeys (ADR 0029) and
/// a round trip prices both; written apart from it, the loaded half lived
/// inside `Atlas.haulRoundTripTicks` and no reader of this file could see that
/// the pair existed.
let internal loadedFactorOf (body: BodyPart list) : FatigueFactor =
    let moves = partCountIn body Move

    {
        FatigueParts = List.length body - moves
        MoveParts = moves
    }

/// Cost units the body needs to step onto a tile of the given terrain weight
/// (Screeps fatigue): the step generates weight fatigue per fatigue-generating
/// part, each Move part pays off 2 per tick — so the unit is a half-tick (ADR
/// 0010) — and no step prices below one unit. At unit granularity and not whole
/// ticks, so a Move surplus keeps a road step cheaper than plain for every
/// body. A body without Move parts cannot step at all.
let internal stepUnits (factor: FatigueFactor) weight =
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
let internal stepTable (stepPrice: int -> int option) : int[] =
    Array.init (Engine.swampWeight + 1) (fun weight -> stepPrice weight |> Option.defaultValue -1)

/// The flood's array accessors: checked on .NET (so `dotnet test` runs the
/// flood bounds-checked) and a bare JS index under Fable, where the `[<Emit>]`
/// template replaces the call — Fable's own indexer re-tests indices the loop
/// has already proven in range, and was ~28% of the tick in the flood.
[<Emit("$1[$0]")>]
let internal at (index: int) (array: int[]) : int = array.[index]

[<Emit("$1[$0] = $2")>]
let private setAt (index: int) (array: int[]) (value: int) : unit = array.[index] <- value

[<Emit("$1[$0]")>]
let internal flagAt (index: int) (array: bool[]) : bool = array.[index]

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
let internal weightAt (grid: int[]) (tile: Pos) : int =
    if inGrid tile then at (indexOf tile) grid else -1

/// Whether a tile is passable in one of the Atlas's grids — the -1 above
/// read as the one thing it means. A tile off the grid, off the
/// projection, walled, or blocked in whichever grid is being asked is not
/// walkable in it, which is one answer and not four (ADR 0004).
let internal walkableAt (grid: int[]) (tile: Pos) : bool = weightAt grid tile >= 0

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
let internal floodFromAllSeeded
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
let internal drained (flood: Flood) : int[] * int[] =
    settleTo flood everyTile
    flood.Dist, flood.Parents

/// What a resumable flood reaches one tile at, settling it first: the one
/// read every per-tile question goes through, so no reader can mistake the
/// `unreached` of an unsettled tile for the one that means unreachable. A
/// tile off the grid is `unreached` too — the guard is what makes the reads
/// below in-range, and it hands unplaceable geometry ADR 0004's answer.
let internal reachedBy (flood: Flood) (tile: Pos) : int =
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
let internal glimpsedBy (flood: Flood) (tile: Pos) : int =
    if not (inGrid tile) then
        unreached
    else
        at (indexOf tile) flood.Dist

/// The cheapest distance any tile the flood has *not* settled can still turn
/// out to have: the dist at the top of its heap, and `unreached` when the heap
/// has run dry. A reader asks this to decide whether a tile is worth settling
/// at all.
let internal frontierOf (flood: Flood) : int =
    if flood.Size = 0 then
        unreached
    else
        heapAt 0 flood.Heap / tileCount

/// The same read on a flood already settled whole. Spelled beside
/// `reachedBy` so the two answer off one arithmetic, and a reader handed
/// one instead of the other changes nothing but when the work was done.
let internal reachedIn (dist: int[]) (tile: Pos) : int = dist.[indexOf tile]

/// The first tile of a cheapest path out of `startIndex` toward a goal,
/// walked back down the predecessor chain. Only ever asked of a goal the
/// flood has settled, and that is enough: every tile of a cheapest path is
/// strictly cheaper than its end, so the chain is final when the goal is.
let internal firstStepOn (flood: Flood) (startIndex: int) (goalIndex: int) : int =
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
let internal floodFrom weights occupied stepPrices (start: Pos) =
    floodFromAll weights occupied stepPrices [ start ]

/// What a step costs and whether the crowd is seen, for one pricing over one
/// body: the ranking price sees today's traffic and counts half-ticks, the
/// clock is blind to it and counts whole ticks (ADR 0029), and the baseline
/// counts half-ticks with the crowd taken out (ADR 0030). The one place the
/// pair is laid side by side, so no flood can take one half without the other.
let internal pricingOf (occupied: bool[]) (factor: FatigueFactor) (pricing: Pricing) =
    match pricing with
    | TravelCost -> stepTable (stepUnits factor), occupied
    | Walk -> stepTable (stepTicks factor), noTraffic
    | Baseline -> stepTable (stepUnits factor), noTraffic

/// The walk's flood over one body, from anywhere in `starts` (ADR 0029):
/// whole ticks a step and blind to today's traffic — the `Walk` row of
/// `pricingOf`, reached by the clocks whose origins keep them outside the
/// tick's pricing memo (the lead's cast walk, the hauler quota's round trip).
let internal walkFloodFromAll weights factor (starts: Pos list) =
    let stepPrices, traffic = pricingOf noTraffic factor Walk
    floodFromAll weights traffic stepPrices starts

/// The one-origin walk: a creep, or a container, prices from the tile it
/// sits on.
let internal walkFloodFrom weights factor (start: Pos) =
    walkFloodFromAll weights factor [ start ]

/// The flood one pricing wants over one body, out of one origin: the memoised
/// flood a placed creep prices from, and the one flood the Atlas leaves
/// resumable (#174) — it is read at a Work Area's few tiles, at a goal and its
/// predecessor chain, or at a Seam band's thirty-odd, never a room at a time.
let internal floodPriced weights occupied factor pricing (start: Pos) : Flood =
    let stepPrices, traffic = pricingOf occupied factor pricing
    floodFromAllSeeded weights traffic stepPrices [ start, 0 ]

/// What the flood charges for a step landing on a tile — the step price plus
/// the occupancy surcharge, exactly as `floodFromAllSeeded`'s relaxation
/// charges it. None for a tile outside the projection or one this body cannot
/// step onto.
let internal entryCost
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
let internal floodPricedInto weights occupied factor pricing (goals: Pos list) : int[] =
    let stepPrices, traffic = pricingOf occupied factor pricing

    goals
    |> List.choose (fun goal ->
        entryCost weights traffic stepPrices goal |> Option.map (fun cost -> goal, cost))
    |> floodFromAllSeeded weights traffic stepPrices
    |> drained
    |> fst
