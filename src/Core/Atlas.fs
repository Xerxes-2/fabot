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
            /// The room every query that names none of its own answers
            /// for: the projection's `RoomName`, and the empty name when it
            /// names none — the room `Decide.censusSignature` already
            /// spells that way (ADR 0041). ADR 0041 keeps the room off
            /// `Pos` and puts it on the API instead, and a query taking no
            /// creep name and no target id has no other place to read one
            /// from: the Layout's placement censuses, the reflexes' tile
            /// sets and the raw weight grid are the colony's own room's
            /// business. Answering them here once, rather than leaving
            /// each to pick a layer, is also what keeps a second projected
            /// room from unioning its tiles into theirs — a Seat union
            /// crossing two rooms would invent a Dual Seat out of one
            /// coordinate standing in both.
            Home: string
            /// Placed creeps in Snapshot order — the canonical iteration
            /// order for everything derived per creep — each beside the
            /// room the projection files it under, because the flood it
            /// seeds is that room's.
            Placed: (string * string * Pos) list
            /// Each creep's fatigue factor — what turns terrain weight
            /// into travel cost for that body (ADR 0006).
            Factors: Map<string, FatigueFactor>
            /// Creep name -> the room the projection files it under and
            /// the tile it stands on there. The id-to-room join ADR 0041
            /// puts on the API rather than on `Pos`: a creep name is
            /// unique across the world, so the layer holding it *is* the
            /// room it stands in. Resolved once here so that every query
            /// starting from a creep costs one lookup rather than one per
            /// projected room.
            CreepAt: Map<string, string * Pos>
            /// Target id -> the room the projection files it under and its
            /// tile there — the same join over the other id space, and the
            /// reason `TargetKinds` stays flat while `TargetPositions`
            /// layers: an object id is already unique across the world, so
            /// the kind census needs no room, and the position is what a
            /// room hangs off. A join between the two that must *find* a
            /// target's room resolves it through this; a join that has
            /// already fixed its room reads that room's layer directly and
            /// drops what is not in it, which is the stronger form where
            /// the room is the answer's whole point (`tilesWhereIn`,
            /// `controllerContainers`). What no join may do is pair a kind
            /// with whichever layer happens to hold the id.
            TargetAt: Map<string, string * Pos>
            /// Step weight per tile index, per room name, laid once a tick
            /// for the flood's hot loop: -1 impassable, else the terrain
            /// weight. stepCost's rules over a whole room, reached by
            /// walking that room's collections rather than by querying it a
            /// tile at a time (#96) — the same rules, not the same code,
            /// so the two are held together by terrainWeight and by the
            /// road and obstacle precedence ofSnapshot spells out. One grid
            /// per room rather than one grid keyed by room and tile: a
            /// flood never leaves its room (ADR 0041), so the room is
            /// chosen once, outside the hot loop, and the loop keeps the
            /// flat `x * 50 + y` index it had.
            Weights: Map<string, int[]>
            /// Whether a creep stands on each tile index this tick, per
            /// room name; the flood prices these tiles dearer so paths
            /// detour around standing traffic.
            Occupied: Map<string, bool[]>
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
            ///
            /// One table per room, and the key tuple gains no field
            /// (ADR 0041, #115's user story 11): no flood ever leaves its
            /// room, so the room is not one of the things that tell two
            /// floods of a room apart — but two rooms hold the same
            /// coordinates, so a room in the table and not in the key is
            /// what keeps two creeps of one fatigue factor standing on the
            /// same tile of different rooms from colliding on one entry and
            /// one of them reading the other room's distances. The room
            /// picks the table; the tuple keys inside it.
            Floods: Map<string, Map<Pos * FatigueFactor * Pricing, Lazy<int[] * int[]>>>
            /// Memoised flood *into* a Task's ground — the far leg of a
            /// cross-room walk (ADR 0041, #123). Its origin is the target
            /// and not a creep, which is the whole reason it is a table
            /// beside Floods rather than an entry inside one: one flood
            /// answers every creep in the colony that prices that Task, so
            /// a second outpost source costs one more flood and not one
            /// more per creep — the arithmetic ADR 0041 rests its cost
            /// argument on. Distances only; the far leg is a price, and
            /// nothing steps along it (movement stays single-room).
            ///
            /// Five fields, each earning its place. The **room**, because a
            /// flood is one room's weight grid and two rooms hold the same
            /// coordinates — the trap the Floods table above answers by
            /// splitting per room, answered here by naming the room in the
            /// key. The **Task**, because its Work Area is the origin set.
            /// The **Work-heavy bit**, because ADR 0020 narrows Harvest's
            /// origins to that source's Posts for such a body, and two
            /// bodies of one fatigue factor can differ in it — the factor
            /// alone would hand a heavy body the light body's flood. The
            /// **factor** and the **pricing** for ADR 0029's own reasons. It
            /// is deliberately not the ADR 0032 spawn-walk table: that one
            /// lives across ticks and its key carries no room at all.
            FarFloods:
                System.Collections.Generic.Dictionary<
                    string * Task * bool * FatigueFactor * Pricing,
                    int[]
                 >
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

/// The weight grid of a room the projection does not carry: every tile
/// impassable, which is the answer stepCost gives a tile outside the
/// projection, read a whole room at a time (ADR 0004, ADR 0041). Absence
/// of a room and absence of every tile in it are one answer — unpriceable
/// geometry, never blocked geometry, so nothing is reachable through it
/// and nothing is refused because of it. Shared and never written: the
/// grids are the flood's read-only input, and the one query that hands one
/// out hands out a copy.
let private noGround: int[] = Array.create tileCount -1

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
/// the flood reads the table the Atlas lays to the same rules. Over one
/// room's layer, because a bare `Pos` says which tile and never which room
/// (ADR 0041): the caller has already chosen the room, here and everywhere
/// below.
let private stepCost (layer: RoomLayer) tile =
    if Set.contains tile layer.Obstacles then
        None
    else
        match Map.tryFind tile layer.Terrain |> Option.map terrainWeight with
        | Some weight when weight > 0 -> Some(if Set.contains tile layer.Roads then 1 else weight)
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
/// from its own `Count`. Used only inside `floodFromAllSeeded`; every
/// other array read in the Atlas stays checked, since none is on the
/// profile.
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

/// Dijkstra flood over the weight grid from every tile in `starts`, each
/// starting at the cost the caller seeds it with, priced by `stepPrice` —
/// one body's price for a step onto a tile of a given terrain weight, and,
/// beside the occupancy the caller passes, the only thing that differs
/// between the tick's floods (ADR 0029, ADR 0030): cheapest cost to every
/// reachable tile (`unreached` elsewhere), plus each tile's predecessor
/// index on a
/// cheapest path (-1 elsewhere). This is the tick's hottest loop, so it
/// runs on flat arrays with a binary min-heap of dist-then-index keys —
/// the key ordering also keeps tie-breaking deterministic. A start tile
/// takes its seed even when it cannot be stepped onto — a creep already
/// stands there, or is about to be placed there. Several starts price a
/// body that may begin anywhere in a set, which is how a spawner places a
/// finished creep beside itself (ADR 0026). A tile marked occupied costs
/// occupancyPenalty extra, so paths detour around standing traffic when a
/// detour is cheaper. The penalty is a number of cost units, so a caller
/// pricing steps in anything else must pass no occupancy at all: every
/// traffic-blind caller here passes `noTraffic`, and for the tick's
/// memoised floods `pricingOf` pairs the two choices per pricing, so
/// neither can be made without the other.
///
/// A non-zero seed is what turns a flood *out of* a set into a flood
/// *into* it (`floodPricedInto`, ADR 0041): the step price is charged on
/// the tile a step lands on, so a flood read backwards charges the tile it
/// started from and not the one it ends on. Seeding each origin with its
/// own entry price puts that missing charge back at the start, where it
/// cancels the one the read end must drop — see `floodPricedInto` below,
/// the one caller that seeds anything, whose answer `pricedAcross` then
/// adds to the near leg's arrival at the border.
let private floodFromAllSeeded
    (weights: int[])
    (occupied: bool[])
    (stepPrice: int -> int option)
    (starts: (Pos * int) list)
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

    for start, seed in starts do
        let startIndex = indexOf start

        // Checked: a start is the caller's Pos, not an index the flood
        // built, so this is the one access the in-range argument for the
        // accessors above does not cover — and it runs once per start.
        if seed < dist.[startIndex] then
            dist.[startIndex] <- seed
            push (seed * tileCount + startIndex)

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

/// The flood every origin starts free at — the shape every caller but the
/// far leg of a cross-room walk wants, since a creep pays nothing to be
/// where it already is.
let private floodFromAll weights occupied stepPrice (starts: Pos list) =
    floodFromAllSeeded weights occupied stepPrice [ for start in starts -> start, 0 ]

/// The one-origin flood the trunk's router wants, and nothing else does
/// any more: a raw-terrain flood out of a source's tile with no creep in
/// it and no traffic seen (`trunkPath`, its only caller). Every priced
/// flood in the tick reaches `floodFromAll` through `floodPriced` instead,
/// so a new `Pricing` row is wired into `pricingOf` and never here.
let private floodFrom weights occupied stepPrice (start: Pos) =
    floodFromAll weights occupied stepPrice [ start ]

/// What a step costs and whether the crowd is seen, for one pricing over
/// one body: the ranking price sees today's traffic and counts half-ticks,
/// the clock is blind to it and counts whole ticks (ADR 0029), and the
/// baseline counts the ranking price's own half-ticks with the crowd taken
/// out (ADR 0030). The one place the pair is laid side by side, so no
/// flood can take one half without the other and the memo cannot hold one
/// where a reader expects another. A caller with no room's occupancy in
/// hand passes `noTraffic`, which is what two of the three rows answer
/// anyway.
let private pricingOf (occupied: bool[]) (factor: FatigueFactor) (pricing: Pricing) =
    match pricing with
    | TravelCost -> stepUnits factor, occupied
    | Walk -> stepTicks factor, noTraffic
    | Baseline -> stepUnits factor, noTraffic

/// The walk's flood over one body, from anywhere in `starts` (ADR 0029):
/// whole ticks a step and blind to today's traffic — the `Walk` row of
/// `pricingOf`, reached by the clocks whose origins keep them outside the
/// tick's pricing memo (the lead's cast walk, the hauler quota's round
/// trip). Every clock in the colony floods through here or through that
/// row, and there is only the one row.
let private walkFloodFromAll weights factor (starts: Pos list) =
    let stepPrice, traffic = pricingOf noTraffic factor Walk
    floodFromAll weights traffic stepPrice starts

/// The one-origin walk: a creep, or a container, prices from the tile it
/// sits on.
let private walkFloodFrom weights factor (start: Pos) =
    walkFloodFromAll weights factor [ start ]

/// The flood one pricing wants over one body, out of one origin: the
/// memoised flood a placed creep prices from.
let private floodPriced weights occupied factor pricing (start: Pos) =
    let stepPrice, traffic = pricingOf occupied factor pricing
    floodFromAll weights traffic stepPrice [ start ]

/// What the flood charges for a step landing on a tile — the step price
/// plus the occupancy surcharge, exactly as the relaxation inside
/// `floodFromAllSeeded` charges it. None for a tile outside the
/// projection or one this body cannot step onto at all. Spelled once here
/// so a seeded flood's origins carry the same charge the loop would have
/// put on them.
let private entryCost (weights: int[]) (occupied: bool[]) stepPrice (tile: Pos) : int option =
    let index = indexOf tile
    let weight = weights.[index]

    if weight < 0 then
        None
    else
        stepPrice weight
        |> Option.map (fun step -> step + (if occupied.[index] then occupancyPenalty else 0))

/// The same pricing flooded *into* a set of goals rather than out of one
/// origin: cheapest cost from every tile of the room to the nearest goal,
/// counting the step onto the tile it is read at and the step onto the
/// goal it ends on (ADR 0041, #123). The engine's cost is charged on the
/// tile a step lands on, so a flood run outward from the goals charges the
/// wrong end by exactly one tile; seeding each goal with its own entry
/// cost restores it, and what comes back at a tile `f` is then
/// `cost(f) + walk(f -> goals)` — the price of standing on `f` *and*
/// walking in from it. `pricedAcross` adds that to the near leg's arrival
/// at the border, and every tile the creep steps onto is charged once and
/// none twice.
///
/// Distances only: a route into the far room is not a route anything
/// steps along, because arbitrated movement stays single-room (ADR 0041).
let private floodPricedInto weights occupied factor pricing (goals: Pos list) : int[] =
    let stepPrice, traffic = pricingOf occupied factor pricing

    goals
    |> List.choose (fun goal ->
        entryCost weights traffic stepPrice goal |> Option.map (fun cost -> goal, cost))
    |> floodFromAllSeeded weights traffic stepPrice
    |> fst

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

    // The home room, spelled the one way the convention is spelled
    // (`SpatialInfo.homeName`): the projection's name, and the empty name
    // when it names none.
    let home = SpatialInfo.homeName spatial

    // The two id-to-room joins, resolved once. An id is unique across the
    // world, so the layer that holds it is the room it is in (ADR 0041) —
    // there is nothing to disambiguate and no room to prefer, which is
    // what makes searching every layer the right answer here and the wrong
    // one for a query that starts from a bare `Pos`.
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
        snapshot.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name creepAt
            |> Option.map (fun (room, pos) -> creep.Name, room, pos))

    let factors =
        snapshot.Creeps
        |> List.map (fun creep -> creep.Name, fatigueFactorOf creep)
        |> Map.ofList

    // The flood's weight table, one grid per projected room, filled by
    // walking that room's three collections rather than by asking stepCost
    // per tile. Walking a tree compares nothing; only a lookup does, and
    // the per-tile form cost three Pos-keyed lookups a tile — 2500 tiles'
    // worth of structural comparison, the largest single cost in the tick
    // (#96). Layering by room name keeps that: the room is chosen once,
    // and inside a grid nothing is keyed by `Pos` at all. The passes layer
    // in stepCost's own precedence: terrain first, then roads over the
    // passable ground they discount, then obstacles over everything. The
    // array's initial -1 is the answer for every tile outside the
    // projection, which is stepCost's answer for one too.
    let gridOf (layer: RoomLayer) =
        let weights = Array.create tileCount -1

        layer.Terrain
        |> Map.iter (fun tile terrain -> weights.[indexOf tile] <- terrainWeight terrain)

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

        layer.CreepPositions |> Map.iter (fun _ tile -> occupied.[indexOf tile] <- true)

        weights, occupied

    let grids = spatial.Rooms |> Map.map (fun _ layer -> gridOf layer)
    let weights = grids |> Map.map (fun _ (grid, _) -> grid)
    let occupied = grids |> Map.map (fun _ (_, standing) -> standing)

    {
        Spatial = spatial
        Home = home
        Placed = placed
        Factors = factors
        CreepAt = creepAt
        TargetAt = targetAt
        Weights = weights
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

/// One room's geometry, read the way ADR 0041 says a layer is read: a room
/// the projection carries no geometry for has no entry at all, and that is
/// the same answer as an entry whose every container is empty (ADR 0004) —
/// so `tryFind` and the empty layer, never the indexer, which throws on
/// exactly the room a projection names and holds nothing for.
///
/// The rule itself is `SpatialInfo.layerOf`'s, spelled once there and
/// reached from here with the Atlas's own projection, so the tick that
/// changes what a room with no entry answers there is the tick these
/// fourteen call sites change with it.
let private layerOf (atlas: Atlas) (room: string) : RoomLayer =
    SpatialInfo.layerOf atlas.Spatial room

/// One room's step-weight grid, and the all-impassable grid for a room the
/// projection does not carry.
let private weightsOf (atlas: Atlas) (room: string) : int[] =
    Map.tryFind room atlas.Weights |> Option.defaultValue noGround

/// One room's standing traffic, and no traffic at all for a room the
/// projection does not carry — which is what an empty room holds anyway.
let private occupiedOf (atlas: Atlas) (room: string) : bool[] =
    Map.tryFind room atlas.Occupied |> Option.defaultValue noTraffic

/// A copy of one room's step weight per tile index — the grid that room's
/// floods price from, -1 impassable. Read by the census guard (ADR 0032)
/// and nothing else: the spawn walks are recalled across ticks on the
/// census signature alone, so two Snapshots the signature calls equal have
/// to lay the same grid, and a weights input the signature misses would
/// price leads off a stale one until a global reset. The room is the
/// caller's since ADR 0041, because there is now one grid per projected
/// room and the guard has to be able to ask about each: a signature
/// covering the colony's own room alone is a fact about the signature, not
/// something this query should decide by answering only for it. A room the
/// projection does not carry answers every tile impassable. A copy because
/// the grid is the flood's own working state: read it, never hold it.
let stepWeights (atlas: Atlas) (room: string) : int[] = Array.copy (weightsOf atlas room)

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

/// The memoised flood for a creep from a tile of one room, under one
/// pricing; placed creeps' own tiles hit the memo. The room is the
/// caller's, and it is always the room the creep stands in: a flood runs
/// inside one room and stops at its border (ADR 0041), so there is no
/// pricing a tile of another room off it.
let private flood (atlas: Atlas) (pricing: Pricing) (room: string) (creep: string) (pos: Pos) =
    let factor = factorOf atlas creep

    match
        atlas.Floods
        |> Map.tryFind room
        |> Option.bind (Map.tryFind (pos, factor, pricing))
    with
    | Some memo -> memo.Value
    | None -> floodPriced (weightsOf atlas room) (occupiedOf atlas room) factor pricing pos

/// The creeps the projection places in the colony's own room, in Snapshot
/// creep order. `Placed` carries every room's, because each seeds a flood
/// in the room the projection files it under; this hands out a bare `Pos`,
/// and ADR 0041's Consequences keep arbitrated movement (ADR 0001, ADR
/// 0008) and the occupancy surcharge single-room, unchanged — the Seam
/// is where geometry and arbitration part company. Its three readers each
/// key on `Pos` alone: the Resolver unions these tiles into a `Set<Pos>`
/// of blocked tiles and a `Map<Pos, string>` of occupants, the pickup
/// reflex measures range against home-room piles, and the lead prices the
/// tile off the home room's flood. A second room's creep unioned in would
/// ground a home creep from another room, collapse two creeps onto one
/// occupant, and price an outpost tile on home terrain. The mover does not
/// learn the room later either: ADR 0041 settles arbitration at one room,
/// so this is the answer, not a placeholder for one. A creep the colony's
/// own room does not place is simply not arbitrated and not led — the
/// answer ADR 0004 gives for geometry a query cannot place, here reached
/// by picking the room rather than by failing to find the tile.
let placedCreeps (atlas: Atlas) : (string * Pos) list =
    atlas.Placed
    |> List.choose (fun (name, room, pos) -> if room = atlas.Home then Some(name, pos) else None)

/// Name of the colony's own room — the entry of the layer that is home
/// (ADR 0041), which the Layout gates on and stamps onto every site it
/// places (ADR 0017). None when the projection names no room, which since
/// ADR 0041 is a separate question from whether it carries geometry: a
/// projection can hold an outpost's layer and still name its home.
let homeRoom (atlas: Atlas) : string option = atlas.Spatial.RoomName

/// Tile of a projected target (source, structure, site, controller) — in
/// whichever room the projection files that id under, since an id is
/// unique across the world. The `Pos` is bare (ADR 0041), so a caller
/// joining it to other geometry must already know the room; every such
/// join inside the Atlas goes through the same resolution this reads.
let positionOf (atlas: Atlas) (targetId: string) : Pos option =
    Map.tryFind targetId atlas.TargetAt |> Option.map snd

/// Tiles a construction site may occupy in the colony's own room: non-Wall
/// terrain holding no projected target — anything standing (or being
/// built) on a tile keeps a site off it; creeps do not, and neither does a
/// dropped pile — a transient pile perturbing the ordering would break the
/// Layout's determinism (ADR 0011). Deterministic (X, Y) order. The home
/// room and no other (ADR 0041): the Layout builds in the room it is
/// anchored in, and a second room's tiles unioned in would offer the
/// Layout a coordinate it does not own.
let buildableTiles (atlas: Atlas) : Pos list =
    let home = layerOf atlas atlas.Home

    let taken =
        home.TargetPositions
        |> Map.toList
        |> List.filter (fun (id, _) -> Map.tryFind id atlas.Spatial.TargetKinds <> Some Dropped)
        |> List.map snd
        |> Set.ofList

    home.Terrain
    |> Map.toList
    |> List.choose (fun (tile, terrain) ->
        if terrain <> Wall && not (Set.contains tile taken) then
            Some tile
        else
            None)

/// Ids of the projected targets of one kind, in id order — across every
/// room the projection carries. The kind census is not layered and does
/// not need to be: an object id is unique across the world (ADR 0041), and
/// this answers ids, never tiles, so nothing here can confuse one room's
/// coordinate for another's. Every reader that turns these into tiles goes
/// through a join that picks a room first.
let private targetsOfKind (atlas: Atlas) (kind: TargetKind) : string list =
    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, k) -> if k = kind then Some id else None)

// The six counts below read the flat kind census and nothing else, so
// each answers for *every* room the projection carries rather than for the
// colony's own (ADR 0041 leaves the id-keyed containers unlayered). That
// is exact today and only today: the shell projects one owned room, and an
// outpost is an unowned room whose layer carries a container and nothing
// else (ADR 0042) — no extension, tower or storage can enter the census
// from one. Their reader is the Layout's gap rule, `allowed at RCL − built
// − pending`, which is a fact about the home controller: the tick a
// projected room can hold one of these kinds, these six have to join the
// home layer the way `placedOfKind` does, or the Layout counts a
// neighbour's structures against its own allowance.

/// Extensions already standing.
let builtExtensions (atlas: Atlas) : int =
    targetsOfKind atlas (Structure BuiltKind.Extension) |> List.length

/// Extension construction sites already placed.
let pendingExtensions (atlas: Atlas) : int =
    targetsOfKind atlas (Site BuiltKind.Extension) |> List.length

/// Towers already standing.
let builtTowers (atlas: Atlas) : int =
    targetsOfKind atlas (Structure BuiltKind.Tower) |> List.length

/// Tower construction sites already placed.
let pendingTowers (atlas: Atlas) : int =
    targetsOfKind atlas (Site BuiltKind.Tower) |> List.length

/// Storages already standing — at most one, but counted the way the tower
/// and the extensions are so one gap rule sizes every kind the ordering
/// picks for (ADR 0022).
let builtStorages (atlas: Atlas) : int =
    targetsOfKind atlas (Structure BuiltKind.Storage) |> List.length

/// Storage construction sites already placed.
let pendingStorages (atlas: Atlas) : int =
    targetsOfKind atlas (Site BuiltKind.Storage) |> List.length

/// Placed targets of one kind in the colony's own room: id and tile, in id
/// order. One of the joins between the flat kind census and the layered
/// positions, and it picks the home room (ADR 0041): its readers are the
/// reflexes, which aim at a bare `Pos`, and a tile from a second room
/// would aim them at the same coordinate at home.
let private placedOfKind (atlas: Atlas) (kind: TargetKind) : (string * Pos) list =
    let home = layerOf atlas atlas.Home

    targetsOfKind atlas kind
    |> List.choose (fun id ->
        Map.tryFind id home.TargetPositions |> Option.map (fun pos -> id, pos))

/// Towers standing in the room: id and tile, in id order — the fire
/// reflex's whole view of a tower (ADR 0014): no store is projected, a
/// dry tower's shot simply fails at the engine.
let placedTowers (atlas: Atlas) : (string * Pos) list =
    placedOfKind atlas (Structure BuiltKind.Tower)

/// Dropped energy piles the projection places: id and tile, in id order.
/// The pickup reflex's whole view of a pile — no amount is projected.
let droppedEnergy (atlas: Atlas) : (string * Pos) list = placedOfKind atlas Dropped

/// Tiles holding a built road in the colony's own room — the projection's
/// road census, one half of what the Layout's road gap subtracts
/// (ADR 0011). The home room, like every placement census below.
let roadTiles (atlas: Atlas) : Set<Pos> = (layerOf atlas atlas.Home).Roads

/// Tiles of one room's placed targets whose kind answers a predicate — the
/// join between the flat kind census and that room's positions, for the
/// censuses read as tiles rather than as counts. The room is named rather
/// than searched (ADR 0041): a `Set<Pos>` has no room dimension, so a
/// census unioning two rooms' tiles would hand its reader coordinates that
/// stand in neither room alone.
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

/// Tiles of every placed target whose kind answers a predicate, in the
/// colony's own room — what the Layout's censuses read.
let private tilesWhere (atlas: Atlas) (matches: TargetKind -> bool) : Set<Pos> =
    tilesWhereIn atlas atlas.Home matches

/// Tiles of every placed target of one kind, in the colony's own room.
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
///
/// Ownership is a fact about the id, so it is asked of the flat hits
/// census; the tile is a fact about the room, so it is read out of the
/// home room's layer (ADR 0041).
let ourRampartTiles (atlas: Atlas) : Set<Pos> =
    let home = layerOf atlas atlas.Home

    atlas.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if kind = Structure BuiltKind.Rampart && Map.containsKey id atlas.Spatial.Hits then
            Map.tryFind id home.TargetPositions
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

/// Whether a tile's terrain is swamp; a tile outside the projection is
/// not. Of the colony's own room, because a bare `Pos` names no room
/// (ADR 0041) and this query's readers — the Layout's road plan — work at
/// home.
let isSwamp (atlas: Atlas) (tile: Pos) : bool =
    Map.tryFind tile (layerOf atlas atlas.Home).Terrain = Some Swamp

/// Walkable tiles adjacent to `pos`, in deterministic (X, Y) order.
/// Standing respects obstacles, unlike Seat counting. Of the colony's own
/// room: the tile handed in carries no room of its own, and every caller —
/// the mover's candidates, the spawner's birth tiles, a sink's approach —
/// is home-room geometry.
let adjacentWalkable (atlas: Atlas) (pos: Pos) : Pos list =
    let home = layerOf atlas atlas.Home
    neighbours pos |> List.filter (fun tile -> (stepCost home tile).IsSome)

/// Every tile of the room a creep may stand on — `adjacentWalkable`'s
/// answer over the whole projection, read off the weight grid rather than
/// a tile at a time: the same rules, held together the way the grid itself
/// is (`stepCost`, and the road and obstacle precedence `ofSnapshot`
/// spells out). The room-wide half nothing wanted until a Task's Work
/// Area was the room itself (ADR 0033). The colony's own room: this is the
/// Flee reflex's safe ground, and a hostile's Reach is measured in the
/// room it stands in.
let walkableTiles (atlas: Atlas) : Set<Pos> =
    let weights = weightsOf atlas atlas.Home

    Set.ofList
        [
            for index in 0 .. tileCount - 1 do
                if at index weights >= 0 then
                    posAt index
        ]

/// The tile a creep stands on; None for a creep the projection does not
/// place. What a judgement about where a creep *is* reads — as
/// `positionOf` is the same question about a target — and, like it, the
/// tile in whichever room the projection files that name under, bare of
/// the room itself (ADR 0041).
let creepTile (atlas: Atlas) (creep: string) : Pos option =
    Map.tryFind creep atlas.CreepAt |> Option.map snd

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
let private seatTiles (layer: RoomLayer) (pos: Pos) : Set<Pos> =
    neighbours pos
    |> List.filter (fun tile ->
        match Map.tryFind tile layer.Terrain with
        | Some Plain
        | Some Swamp -> true
        | Some Wall
        | None -> false)
    |> Set.ofList

/// Seat tiles of a source — the geometry behind `seats`, for the Layout's
/// source-container pick (ADR 0012). Empty for a source the projection
/// does not place: an unplaceable source anchors nothing, and a source in
/// a room the projection does not carry is not placed (ADR 0004). The
/// source's own room answers, not the colony's: the id resolves the room
/// (ADR 0041), so an outpost source's Seats are that room's ground and
/// never a home tile of the same coordinate.
let seatTilesOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    Map.tryFind sourceId atlas.TargetAt
    |> Option.map (fun (room, pos) -> seatTiles (layerOf atlas room) pos)
    |> Option.defaultValue Set.empty

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    Map.tryFind sourceId atlas.TargetAt
    |> Option.map (fun (room, pos) -> seatTiles (layerOf atlas room) pos |> Set.count)

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
        match Map.tryFind targetId atlas.TargetAt with
        | None -> Set.empty
        // The target's own room, resolved off its id (ADR 0041): an area is
        // the ground around a target, and which ground that is is settled
        // by where the target stands, never by which room the reader is
        // working in.
        | Some(room, target) ->
            let layer = layerOf atlas room

            Set.ofList
                [
                    for x in target.X - r .. target.X + r do
                        for y in target.Y - r .. target.Y + r do
                            let tile = { X = x; Y = y }

                            if (stepCost layer tile).IsSome then
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
/// the Emitter and Resolver. The tiles are the target's room's (ADR 0041),
/// and a caller comparing them against a creep's tile has to have checked
/// that the two are the same room — which is what pricing a path does
/// before it floods.
let workArea (atlas: Atlas) (task: Task) : Set<Pos> =
    memoised atlas.WorkAreas task (fun () -> buildWorkArea atlas task)

/// Every source of one room's Seat tiles, unioned — the seat half behind
/// dualSeats and posts. Named room and not every layer (ADR 0041): the
/// union is intersected with an Upgrade area below, and two rooms' Seats
/// unioned would meet a second room's Upgrade area at a coordinate that is
/// a Dual Seat in neither — a phantom Post, a phantom Anchor place, and an
/// outpost source reading as posted without a container.
let private seatUnionIn (atlas: Atlas) (room: string) : Set<Pos> =
    let layer = layerOf atlas room

    targetsOfKind atlas Source
    |> List.choose (fun id ->
        match Map.tryFind id atlas.TargetAt with
        | Some(where, pos) when where = room -> Some(seatTiles layer pos)
        | _ -> None)
    |> List.fold Set.union Set.empty

/// Every controller of one room's Upgrade Work Area, unioned — the tiles a
/// creep can upgrade from, behind dualSeats and controllerContainers. One
/// room for the same reason the Seat union is one room's.
let private upgradeAreaIn (atlas: Atlas) (room: string) : Set<Pos> =
    targetsOfKind atlas Controller
    |> List.filter (fun id ->
        match Map.tryFind id atlas.TargetAt with
        | Some(where, _) -> where = room
        | None -> false)
    |> List.map (Upgrade >> workArea atlas)
    |> List.fold Set.union Set.empty

/// The working ground of the room (ADR 0022): every projected source's
/// Seats plus every projected controller's Upgrade Work Area — the tiles
/// the colony works from. Off-limits to the Layout's clustered ordering: a
/// tower or extension there eats a tile an Anchor or an upgrader stands
/// on. Total: a room with neither kind of geometry answers with the empty
/// set, which reserves nothing rather than blocking every tile (ADR
/// 0004). Derived fresh each tick, never persisted. The colony's own room:
/// what it reserves is what the Layout may not build on, and the Layout
/// builds at home.
let workingGround (atlas: Atlas) : Set<Pos> =
    Set.union (seatUnionIn atlas atlas.Home) (upgradeAreaIn atlas atlas.Home)

/// Dual Seats of the room: tiles inside both some projected source's Seats
/// and a projected controller's Upgrade Work Area — a creep standing on one
/// harvests and upgrades without ever moving. Total: a room with no
/// controller, no sources, or a disjoint pair answers with the empty set,
/// which never punishes anything (ADR 0004). Derived fresh each tick,
/// never persisted. Within one room, and the public query answers for the
/// colony's own: a Dual Seat is a tile a creep works two things from
/// without moving, which two rooms' geometry can never make between them.
let private dualSeatsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (seatUnionIn atlas room) (upgradeAreaIn atlas room)

/// The Dual Seats of the colony's own room — the doc above governs both.
let dualSeats (atlas: Atlas) : Set<Pos> = dualSeatsIn atlas atlas.Home

/// Posts of the room: the tiles worth garrisoning with a heavy-WORK body
/// (ADR 0012) — the Dual Seats plus every Seat under a built container
/// (sites don't count: a pending container catches no overflow). A
/// Seat-standing container is a source container by the Layout's
/// geometry — a controller container's tile that were also a Seat would
/// already be a Dual Seat. The
/// capacity unit of the Anchor quota. Total: a room with neither kind
/// answers with the empty set (ADR 0004). Derived fresh each tick, never
/// persisted. Within one room, three room-local censuses intersected: a
/// Post is one tile carrying a Seat and a container (ADR 0041).
let private postsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect
        (seatUnionIn atlas room)
        (tilesOfKindIn atlas room (Structure BuiltKind.Container))
    |> Set.union (dualSeatsIn atlas room)

/// The Posts of the colony's own room — the doc above governs both.
let posts (atlas: Atlas) : Set<Pos> = postsIn atlas atlas.Home

/// Tiles holding a standing container on a Post — the tiles a work-heavy
/// body garrisons and cannot flee from (ADR 0033), ramparted beside the
/// Keep (ADR 0034). A Post that is a bare Dual Seat is not one of these:
/// what the rule covers is a structure standing, and there is none there.
/// The colony's own room, like the ramparts it is raised under.
let postContainerTiles (atlas: Atlas) : Set<Pos> =
    Set.intersect (containerTiles atlas) (posts atlas)

/// The Posts of one source: its own Seats that are Posts. Empty for a
/// source the projection does not place, and for one with neither a built
/// container on a Seat nor a Dual Seat. Both halves are read in the
/// source's own room (ADR 0041) — intersecting an outpost source's Seats
/// with the home room's Posts would answer a tile standing in neither.
let postsOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    match Map.tryFind sourceId atlas.TargetAt with
    | None -> Set.empty
    | Some(room, _) -> Set.intersect (seatTilesOf atlas sourceId) (postsIn atlas room)

/// Whether a creep and a Task's target stand in one room — the question
/// every join between a creep and a target's geometry has to settle while
/// no flood leaves its room (ADR 0041). Absence is permissive, as ADR 0004
/// has it everywhere else: a Task acting on nothing, an unplaced creep and
/// an unplaced target are each not a border crossing, and keep the answer
/// they had before the projection layered. Where the rule lives: one
/// spelling, read by the creep-aware Work Area below and by the action
/// gate, so nothing derived from either can be joined across a border.
let private sharesRoom (atlas: Atlas) (creep: string) (task: Task) : bool =
    match actionOn task with
    | None -> true
    | Some(targetId, _) ->
        match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
        | Some(creepRoom, _), Some(targetRoom, _) -> creepRoom = targetRoom
        | _ -> true

/// The body-aware Work Area, in the target's own room and blind to where
/// the creep is standing (ADR 0020). Ordinarily the Task's own area, but
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
///
/// Two readers, and the split between them is the room: `workAreaFor`
/// below hands these tiles to a creep already standing in the room they
/// belong to, and `pricedAcross` floods the far leg of a cross-room price
/// out of them without handing a creep anything. Only the body decides
/// what comes back, which is why the far leg's memo carries the Work-heavy
/// bit and not a creep.
let private narrowedArea (atlas: Atlas) (creep: string) (task: Task) : Set<Pos> =
    match task with
    | Harvest sourceId when workHeavy atlas creep ->
        memoised atlas.HeavyAreas task (fun () ->
            let postTiles = postsOf atlas sourceId

            if Set.isEmpty postTiles then
                workArea atlas task
            else
                Set.intersect (workArea atlas task) postTiles)
    | _ -> workArea atlas task

/// Work Area of a Task for one creep — the body-aware query every reader
/// that has a creep uses, which is `narrowedArea` above once the rooms
/// agree.
///
/// Empty for a creep standing in a different room from the Task's target
/// (ADR 0041). The body-blind `workArea` above stays honest — those tiles
/// are the target's room's ground and it is really there — but a `Set<Pos>`
/// carries no room, and this is the query every reader that holds a creep
/// steps and acts over: the mover's candidates are its own room's tiles,
/// and a goal read out of the wrong room's grid answers a step nobody may
/// take. So the creep is told what it is told when the Reach takes its
/// last standing tile — it has nowhere to work this Task from, which is
/// what makes the action gate refuse rather than mislead.
///
/// #123 did not widen this, and that is the decision: the cross-room
/// *price* is a minimum over the Seam band (`pricedAcross`), joined where
/// the rooms are both in hand, while the *tiles* a creep is handed stay
/// its own room's. Geometry crosses the border; standing, stepping and
/// acting do not (ADR 0041's Consequences). A caller that wants the far
/// room's origins asks `narrowedArea` above, which is the same narrowing
/// with no creep's room in it.
///
/// Guarded outside the memo, which keys on the Task alone: the room is a
/// fact about the creep, and two creeps of one Task must not share an
/// answer that depends on it.
let workAreaFor (atlas: Atlas) (creep: string) (task: Task) : Set<Pos> =
    if not (sharesRoom atlas creep task) then
        Set.empty
    else
        narrowedArea atlas creep task

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
/// standing on one of the source's own Seats — the container Post's
/// footing, judged from the same census `posts` reads (ADR 0012). There
/// the engine drops harvest past a full store into the container under
/// the creep, so a full store never ends the dig. A site catches nothing,
/// and an unplaced creep or source widens nothing — absence of geometry
/// leaves the ordinary full-store rule standing rather than blocking a
/// Task, keeping the query total (ADR 0004).
///
/// The creep and the source have to stand in one room for the answer to
/// mean anything (ADR 0041): a creep at home on the coordinate an outpost
/// source seats catches nothing of that source's.
let catchesOverflow (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind sourceId atlas.TargetAt with
    | Some(creepRoom, pos), Some(sourceRoom, _) when creepRoom = sourceRoom ->
        Set.contains pos (tilesOfKindIn atlas creepRoom (Structure BuiltKind.Container))
        && Set.contains pos (seatTilesOf atlas sourceId)
    | _ -> false

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

/// The tiles of a room's own ground next to one of its exit tiles — the
/// only tiles a flood can price a Seam's near side from, or step off its
/// far side onto, because the border ring is not ground and no flood ever
/// enters it (ADR 0036, ADR 0041). Clipped to the grid rather than to the
/// projection: a tile the projection does not carry is impassable, so the
/// flood already answers `unreached` there, and an index off the grid is
/// no index at all. Diagonals included — the engine lets a creep step onto
/// an exit diagonally, and onto its first tile in the new room the same
/// way.
let private besideExit (tile: Pos) : Pos list =
    neighbours tile
    |> List.filter (fun n -> n.X >= 0 && n.X < roomSide && n.Y >= 0 && n.Y < roomSide)

/// What this body pays to step onto an exit tile, priced by the same rule
/// every other step is (ADR 0029's `max(1, ceil(units / 2))` for the walk,
/// travel cost's units for the ranking price). This is #123's narrowing of
/// ADR 0041's literal `+1`: the `+1` is the price of *walking onto the
/// Seam*, which is one tick only for a plain exit under a body at fatigue
/// parity — the case the ADR was written on, where the two agree — and a
/// swamp exit is not free. Read off the border ring, which is the only
/// terrain the projection has for an exit, and priced at the bare step:
/// the ring carries no road, so there is no discount to apply, and the
/// occupancy surcharge is deliberately not charged here even though
/// the ring can hold a creep — the engine parks one on the far room's ring
/// tile the tick it crosses, and `Snapshot` files it there. A surcharge
/// re-ranks a step so a traveller detours around standing traffic, and
/// there is no detour to buy at a Seam: which crossing is cheapest is a
/// price the mover never spends, because arbitrated movement stays
/// single-room (ADR 0041's Consequences). None for an exit the
/// projection has no terrain for, or a wall, or a body that cannot step at
/// all — an unpriceable crossing is no crossing (ADR 0004).
let private exitPrice (atlas: Atlas) (pricing: Pricing) (factor: FatigueFactor) room tile =
    let stepPrice, _ = pricingOf noTraffic factor pricing

    Map.tryFind room atlas.Spatial.Borders
    |> Option.bind (Map.tryFind tile)
    |> Option.map terrainWeight
    |> Option.filter (fun weight -> weight > 0)
    |> Option.bind stepPrice

/// The far leg's flood for one Task and one body, memoised colony-wide:
/// the price, from every tile of the target's room, of stepping onto that
/// tile and walking in to the Task's Work Area there (`floodPricedInto`).
/// Its origin is the target, so one entry answers every creep the colony
/// prices this Task for — ADR 0041's reason the cross-room walk is a
/// minimum over additions rather than over floods.
let private farFlood (atlas: Atlas) (pricing: Pricing) (creep: string) (room: string) (task: Task) =
    let factor = factorOf atlas creep

    memoised atlas.FarFloods (room, task, workHeavy atlas creep, factor, pricing) (fun () ->
        floodPricedInto
            (weightsOf atlas room)
            (occupiedOf atlas room)
            factor
            pricing
            (narrowedArea atlas creep task |> Set.toList))

/// A cross-room price, joined on the Seam: the smallest, over the whole
/// band between the two rooms, of *walk to the exit tile* + *the exit
/// tile's own price* + *walk in from the tile it lands on* (ADR 0041,
/// narrowed by #123). Each leg is a single-room flood on the tables
/// already laid — the near one out of the creep, the far one into the
/// Task — so no flood ever leaves its room and the join is a minimum over
/// thirty-odd additions rather than over thirty-odd floods.
///
/// **The convention, spelled out**, because the two legs are read from
/// opposite ends and a reader has to know which tiles each charges. A step
/// costs what the tile it *lands on* costs, and the creep's journey is:
/// walk to a ground tile beside the exit; step onto the exit; be moved to
/// the landing tile by the engine at the end of that tick, for nothing;
/// step off it onto the far room's ground; walk in. So exactly three
/// things are charged beyond the two floods' own interiors — the exit
/// tile, the far room's first tile, and the Work-Area tile the walk ends
/// on — and the landing tile is charged nothing, because arriving on it is
/// the engine's move and not the creep's.
///
/// The near flood charges every tile it enters, so `near[n]` is honest as
/// it stands. The far flood is run *into* the Work Area with each of its
/// tiles seeded at its own entry cost, so `far[f]` is the price of
/// stepping onto `f` **plus** the walk in from it, ending with the charge
/// for the Work-Area tile itself. Adding the two and the exit's price
/// charges every tile the creep steps onto exactly once and none twice —
/// which is what the engine charges, and what lets the walk and travel
/// cost be read off the same join under their own pricings (ADR 0030). It
/// is a tile cheaper than a flood over the two rooms laid side by side
/// would answer, and that tile is real: crossing a border displaces a
/// creep twice for one move, so a cross-room walk can come in one under
/// the two rooms' own Chebyshev distance (`RoomInvariantTests`).
///
/// Total (ADR 0004): a band with no crossing the body can pay for, a near
/// side no ground of this room reaches, and a far side whose landing tile
/// opens onto nothing all answer with no price at all — the same answer an
/// unreachable Work Area in the creep's own room gets, which is the Task
/// being inapplicable to this creep.
let private pricedAcross
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (creepRoom: string)
    (from: Pos)
    (targetRoom: string)
    : int option =
    match seams atlas creepRoom targetRoom with
    | [] -> None
    | band ->
        let factor = factorOf atlas creep
        let near, _ = flood atlas pricing creepRoom creep from
        let far = farFlood atlas pricing creep targetRoom task

        let reached (dist: int[]) tiles =
            tiles
            |> List.choose (fun tile ->
                let d = dist.[indexOf tile]
                if d = unreached then None else Some d)
            |> function
                | [] -> None
                | costs -> Some(List.min costs)

        band
        |> List.choose (fun (exitTile, landing) ->
            match
                reached near (besideExit exitTile),
                exitPrice atlas pricing factor creepRoom exitTile,
                reached far (besideExit landing)
            with
            | Some approach, Some crossing, Some departure -> Some(approach + crossing + departure)
            | _ -> None)
        |> function
            | [] -> None
            | sums -> Some(List.min sums)

/// The cheapest path from a creep to a set of tiles under one pricing —
/// the shape travel cost and the walk share, so the two can disagree on
/// what a step costs and on nothing else (ADR 0029). The tiles are the
/// caller's, not a Task's: what a creep may stand on this tick is the
/// decision layer's judgement, which takes a Reach out of a Work Area and
/// gives Flee an area of its own (ADR 0033). A creep the projection
/// cannot place prices at 0; an empty or unreachable set has no price at
/// all. Its totality contract is ADR 0004's and is documented on every
/// wrapper.
///
/// The tiles are read as tiles of the creep's own room, because that is
/// the room the flood runs in and it stops at that room's border
/// (ADR 0041). Nothing here joins two rooms: `pricedPath` settles the
/// rooms before it prices, and sends a border crossing to `pricedAcross`.
let private pricedPathTo
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (area: Set<Pos>)
    : int option =
    match Map.tryFind creep atlas.CreepAt with
    | None -> Some 0
    | Some(room, pos) ->
        if Set.isEmpty area then
            None
        elif Set.contains pos area then
            Some 0
        else
            let dist, _ = flood atlas pricing room creep pos

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
/// unreachable geometry (ADR 0004). A target in a room the projection does
/// not carry is not placed, so a Task in an unprojected room prices at 0
/// too: it never counts against the creep, and, having no Work Area, never
/// lets it act.
///
/// This is where the two rooms are settled, and the one place they are
/// (ADR 0041, #123): a creep and a target the projection files under
/// different names are priced by the minimum over their Seam band
/// (`pricedAcross`), and everything else — one room, an unplaced creep, an
/// unplaced target, a Task acting on nothing — prices exactly as it did
/// before there was a second room, off the creep's own flood and the tiles
/// `workAreaFor` hands it. Travel cost and the walk both arrive here, so
/// the colony has one join and not two, and one rule turning geometry into
/// a number across a border as it has one turning units into ticks
/// (ADR 0030).
let private pricedPath (atlas: Atlas) (pricing: Pricing) (creep: string) (task: Task) : int option =
    match actionOn task with
    | Some(targetId, _) when not (Map.containsKey targetId atlas.TargetAt) -> Some 0
    | Some(targetId, _) ->
        match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
        | Some(creepRoom, from), Some(targetRoom, _) when creepRoom <> targetRoom ->
            pricedAcross atlas pricing creep task creepRoom from targetRoom
        | _ -> pricedPathTo atlas pricing creep (workAreaFor atlas creep task)
    | None -> pricedPathTo atlas pricing creep (workAreaFor atlas creep task)

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
///
/// Across a border it is the minimum over the Seam band (`pricedAcross`,
/// ADR 0041), in its own units and off its own floods: the join is shared
/// with the walk so that an outpost's Task ranks in the same pool by the
/// same arithmetic the home room's does, which is what makes "go dig
/// there" one comparison rather than two.
let travelCost (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas TravelCost creep task

/// Travel cost to an explicit set of tiles: the same ranking price over
/// the area the caller hands in rather than the one the Task derives —
/// what prices a Task over the tiles the Reach left it, and Flee, whose
/// Work Area is the safe set and no target's surroundings (ADR 0033). An
/// unplaced creep prices at 0 as it does for a Task; there is no target,
/// so there is no unplaced-target escape — an empty or unreachable set is
/// unreachable, which is the answer a Task with nowhere to stand gets too.
/// The tiles are the creep's own room's, the room its flood runs in
/// (ADR 0041).
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
///
/// Across a border it is the minimum over the Seam band (`pricedAcross`,
/// ADR 0041): the near leg, the exit tile's own price under this same
/// per-step rule — so a swamp exit costs what a swamp step costs, which is
/// #123's narrowing of the ADR's literal `+1` — and the far leg, flooded
/// out of the target so one memo entry serves the whole colony. Same join
/// as travel cost, different pricing, exactly as at home.
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
    // No action reaches across a border: the engine's ranges are measured
    // inside one room, and `range` over two bare tiles would read two
    // rooms' coordinates as one (ADR 0041). The area is the caller's, so
    // this is asked here rather than inferred from an empty one.
    | Some _ when not (sharesRoom atlas creep task) -> false
    | Some(targetId, actionRange) ->
        match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
        | Some(creepRoom, creepPos), Some(_, targetPos) ->
            if (stepCost (layerOf atlas creepRoom) creepPos).IsNone then
                range creepPos targetPos <= actionRange
            else
                Set.contains creepPos area
        | _ -> true

/// First step toward a Task's Work Area over the given flood, sharing
/// firstStep's whole contract — that doc governs both public wrappers.
let private firstStepVia
    (atlas: Atlas)
    (floodOf: string -> Pos -> int[] * int[])
    (creep: string)
    (goals: Set<Pos>)
    : Pos option =
    let rec firstStepOf index startIndex (parents: int[]) =
        let parent = parents.[index]

        if parent = startIndex || parent < 0 then
            index
        else
            firstStepOf parent startIndex parents

    match Map.tryFind creep atlas.CreepAt with
    | None -> None
    | Some(room, pos) ->
        if Set.isEmpty goals || Set.contains pos goals then
            None
        else
            let dist, parents = floodOf room pos

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
/// (cost, tile) wins, matching the flood's tie-breaking. The goals are
/// read as tiles of the creep's own room, the room its flood runs in
/// (ADR 0041): a step is a step inside a room, and a goal on another
/// room's ground is unreachable from here until #123 joins the two.
let firstStep (atlas: Atlas) (creep: string) (goals: Set<Pos>) : Pos option =
    firstStepVia atlas (fun room -> flood atlas TravelCost room creep) creep goals

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
    firstStepVia atlas (fun room -> flood atlas Baseline room creep) creep goals

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
/// (ADR 0004). Both tiles are read in the colony's own room: they are bare
/// `Pos`es with no room on them (ADR 0041), and a round trip that left the
/// room would be two legs joined at a Seam, which is #123's arithmetic and
/// not a flood.
let haulRoundTripTicks (atlas: Atlas) (body: BodyPart list) (from: Pos) (sink: Pos) : int option =
    let count part =
        body |> List.filter ((=) part) |> List.length

    let goals = adjacentWalkable atlas sink
    let weights = weightsOf atlas atlas.Home

    let legTicks factor =
        let dist, _ = walkFloodFrom weights factor from

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
/// on — unpriceable geometry leads nobody (ADR 0004). Floods the colony's
/// own room: a spawner stands at home, and the memo this rides is keyed by
/// tile and factor with no room on it (ADR 0032) — which is safe exactly
/// because every origin it holds is a home-room spawner's tile, and stays
/// safe only while nothing else is memoised here.
let castWalkTicks (atlas: Atlas) (body: BodyPart list) (spawn: Pos) (goal: Pos) : int option =
    let factor = emptyFactorOf body

    let dist =
        memoised atlas.Walks (spawn, factor) (fun () ->
            let dist, _ =
                walkFloodFromAll (weightsOf atlas atlas.Home) factor (adjacentWalkable atlas spawn)

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
/// keys and the lowest (cost, tile) goal break every tie. Over the
/// colony's own room's raw terrain: a trunk is a road the Layout plans,
/// and the Layout plans at home (ADR 0041).
let trunkPath (atlas: Atlas) (avoid: Set<Pos>) (origin: Pos) (goals: Set<Pos>) : Pos list =
    let weights = Array.create tileCount -1
    let home = layerOf atlas atlas.Home

    home.Terrain
    |> Map.iter (fun tile terrain ->
        if not (Set.contains tile home.Obstacles) && not (Set.contains tile avoid) then
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
