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

/// A Dijkstra flood the readers advance rather than a finished pair of
/// arrays: the distance and predecessor grids, plus the heap and the live
/// length within it that the loop left off at (#174). Dijkstra settles a
/// tile for good the moment it leaves the heap — nothing cheaper can
/// reach it afterwards — so a flood may stop anywhere and be picked up
/// again, and every tile it has already settled holds the number the
/// whole flood would have left there. That invariant is the entire
/// licence this shape rests on: the seeds, the key encoding, the
/// stale-entry test and every tie-break are the flood's own and untouched,
/// so what a reader gets is what the full flood gave it, tile for tile.
///
/// Every reader goes through `reachedBy`, and the predecessor chain
/// through `firstStepOn`, never through the grids themselves: a tile the
/// flood has not settled yet still reads `unreached`, which in a whole
/// flood means unreachable and here means only "not asked for yet", and
/// nothing may confuse the two. That is why the memo holds this and no
/// bare array — the compiler is what keeps the distinction (#174).
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
        /// The binary min-heap of dist-then-index keys, and the live
        /// length within it: the heap only ever grows, so a pop is a
        /// decrement rather than a splice (#168), and every slot at or
        /// past `Size` is stale and never read. A heap per flood and never
        /// a shared one — the Atlas is rebuilt every tick and its memos
        /// die with it, so there is no state here to keep across ticks.
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
            /// for the flood's hot loop: -1 impassable, else the price of
            /// stepping onto the tile — road 1 over the ground it
            /// discounts, plain 2, swamp 10; walls, obstacle structures and
            /// tiles outside the projection impassable (ADR 0010,
            /// ADR 0001). Reached by walking that room's collections rather
            /// than by querying it a tile at a time (#96), and since #173
            /// the *only* form the rule has: the single-tile query reads
            /// this grid too (`weightAt`), so there is one rule and one
            /// place it is written rather than a table and a tile query
            /// free to drift apart. One grid per room rather than one grid
            /// keyed by room and tile: a flood never leaves its room
            /// (ADR 0041), so the room is chosen once, outside the hot
            /// loop, and the loop keeps the flat `x * 50 + y` index it had.
            Weights: Map<string, int[]>
            /// Raw terrain weight per tile index, per room name: the ground
            /// before a road discounts it and before an obstacle blocks it
            /// — -1 wall or off the projection, else `terrainWeight`. The
            /// grid above minus its two overriding passes, and a grid of
            /// its own because a Seat is counted by terrain alone: a
            /// structure standing on a source's neighbour does not consume
            /// the Seat (ADR 0001), so the Seat query cannot read the
            /// walking grid and cannot afford to read the layer a `Pos` at
            /// a time either (#173).
            ///
            /// The Layout's three ground readers price off it too (#177):
            /// a construction site's tile is terrain that holds nothing
            /// (`buildableTiles`), a swamp under a road is still swamp
            /// (`isSwamp`), and a trunk is priced over the ground before
            /// any road discounts it (`trunkPath`, which lays the obstacle
            /// pass back on for itself). Two of them sweep the whole room
            /// on a census tick, which is what the grid is for; `isSwamp`
            /// reads it a tile at a time through `weightAt`, for the
            /// reason every single-tile query does since #173 — an index
            /// rather than a `Pos` compared down a tree.
            Ground: Map<string, int[]>
            /// Terrain weight per tile index of each room's border ring —
            /// the exit rows and columns the layers' ground deliberately
            /// leaves out (ADR 0036) — and -1 everywhere else, which every
            /// interior tile of this grid is. The table form of
            /// `SpatialInfo.Borders`, laid for the Seam band and the
            /// crossing price, whose readers ask it once per exit tile of a
            /// band of thirty-odd, once per creep priced across a border
            /// (#173).
            ///
            /// A grid of its own and never merged into the two above: a
            /// ring tile is one a creep passes through and never one it may
            /// stand on, so admitting it to the walking grid would offer a
            /// Work Area or a standing candidate the engine empties the
            /// tick a creep arrives (ADR 0041). Two grids that answer
            /// "impassable" for every tile of the other is exactly the
            /// separation the two layers already have.
            Rings: Map<string, int[]>
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
            /// asks for one of them pays for one. Each is a `Flood` and
            /// not a pair of finished grids: it is laid seeded and
            /// unadvanced, and each reader pushes it out only as far as
            /// the tile it asks about (#174). A creep's Task is usually a
            /// dozen tiles off while the room is two and a half thousand,
            /// and a flood that stops early answers what a flood that ran
            /// on would have — so the memo is what makes the saving
            /// compound too: a creep asked again this tick resumes from
            /// where the last read left the heap rather than starting
            /// over.
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
            Floods: Map<string, Map<Pos * FatigueFactor * Pricing, Lazy<Flood>>>
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
            /// is deliberately not the ADR 0032 spawn-walk table, which
            /// names a room too since #169: that one is keyed on a
            /// *spawner* and lives across ticks under the census, this one
            /// on a Task's Work Area and dies with the tick.
            FarFloods:
                System.Collections.Generic.Dictionary<
                    string * Task * bool * FatigueFactor * Pricing,
                    int[]
                 >
            /// Memoised flood *into* a Seam band — the walk out of every
            /// tile of one room onto the crossings joining it to a named
            /// neighbour (ADR 0042's container pick, `seamWalkTicks`). One
            /// flood per ordered room pair, however many tiles are read off
            /// it: the Seats of every source in the room share the one
            /// answer, which is the same arithmetic ADR 0041 rests the
            /// cross-room walk on — a minimum over additions rather than
            /// over floods.
            ///
            /// Two fields and no more. The **ordered pair**, because the
            /// band is one room's exits toward one neighbour and a room
            /// with two of them has two bands. No body and no pricing:
            /// unlike every other flood here this one prices a *plan* and
            /// not a creep, so it is run once for a body at fatigue parity
            /// and traffic-blind, and there is nothing left for a key to
            /// tell two of them apart by.
            SeamWalks: System.Collections.Generic.Dictionary<string * string, int[]>
            /// Memoised traffic-blind cast walk out of a spawner's tile,
            /// per (spawner tile, fatigue factor, goal's room), for bodies
            /// the Snapshot does not carry: a lead prices a replacement
            /// that has not been cast yet (ADR 0026), so its factor is in
            /// no creep's entry and the Floods memo cannot be laid for it
            /// in advance. One entry per row per spawn per room a lead is
            /// asked over, however many creeps that row is deriving a lead
            /// for — and, since ADR 0032, one per census rather than one
            /// per tick: this is the table the Atlas is handed rather than
            /// one it lays, recalled from the plan memo while the census
            /// signature holds and dropped whole when it moves. Every input
            /// it reads is in that signature — the weights of *every*
            /// projected room, which rooms are projected at all, and the
            /// successor's body through the Capacity that sizes it
            /// (ADR 0017) — so a recalled entry is the entry this tick
            /// would have run. The hauler
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

/// The grid of a room the projection does not carry: every tile
/// impassable, which is the answer a single-tile query gives a tile
/// outside the projection, read a whole room at a time (ADR 0004,
/// ADR 0041). Absence of a room and absence of every tile in it are one
/// answer — unpriceable geometry, never blocked geometry, so nothing is
/// reachable through it and nothing is refused because of it. Shared by
/// all three of the Atlas's grids and never written: they are the flood's
/// and the tile queries' read-only input, and the one query that hands one
/// out hands out a copy.
let private noGround: int[] = Array.create tileCount -1

/// Whether a tile is one of the room's own fifty-by-fifty — the guard
/// every grid read passes through, because `indexOf` does no checking of
/// its own and a `Pos` off the grid indexes off the array: under Fable
/// that reads `undefined`, which the weight comparisons below would call
/// walkable, while .NET throws. `neighbours` produces -1 and 50 at the
/// edges, so this is the ordinary case and not the exotic one. The rule
/// once, here, rather than at each of its readers.
let private inGrid (tile: Pos) =
    tile.X >= 0 && tile.X < roomSide && tile.Y >= 0 && tile.Y < roomSide

/// The eight tiles touching this one, in (X, Y) order — the order every
/// answer derived from them is listed in. Written out rather than
/// generated: a comprehension compiles to a sequence and a `toList` under
/// Fable, and this is the innermost list the Atlas builds — every Seat,
/// every standing candidate and every approach to a Seam is eight of
/// these (#173). Tiles off the grid are left in: what a neighbour is is
/// geometry, and whether it can be read is the grid's own answer
/// (`weightAt`), given once so no caller has to remember to ask.
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

/// The weight of raw ground (ADR 0010): plain 2, swamp 10, wall
/// impassable — written as the -1 the flood's weight table marks an
/// impassable tile with. The one place the engine's terrain prices live:
/// the grids the Atlas lays and the trunk's raw-terrain flood all price
/// off it, which is what keeps them from drifting apart.
let private terrainWeight terrain =
    match terrain with
    | Plain -> 2
    | Swamp -> swampWeight
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
///
/// The one-unit floor is a branch and not `max`: F#'s `max` is generic, so
/// Fable compiles it to a call through the structural comparer, and this
/// was 3.5% of the tick when the flood asked for a price per relaxation
/// (#168). The table below asks once per weight in the domain instead —
/// `swampWeight + 1` times a pricing, never once a relaxation — but the
/// branch is what the arithmetic means anyway.
let private stepUnits (factor: FatigueFactor) weight =
    if factor.MoveParts = 0 then
        None
    else
        let units = (weight * factor.FatigueParts + factor.MoveParts - 1) / factor.MoveParts
        Some(if units < 1 then 1 else units)

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
    stepUnits factor weight
    |> Option.map (fun units ->
        let ticks = (units + 1) / 2
        if ticks < 1 then 1 else ticks)

/// What a step costs this body on every weight the ground can carry, laid
/// out once per pricing — `pricingOf` lays it, and two of its callers
/// price a crossing without ever flooding: the index is the tile's weight
/// in the grid and the value is the price of stepping onto it, written as
/// the same -1 the weight grid marks impassable ground with when the body
/// cannot step at all. That shared sentinel is the point: the flood's
/// inner loop tests one integer instead of calling a pricing closure,
/// unwrapping an `int option` and comparing through Fable's generic `max`
/// — together half the tick before #168, and all of it constant overhead
/// per relaxation rather than algorithm.
///
/// Filled by `stepUnits`/`stepTicks`, which stay the one place a step's
/// price is computed (ADR 0010, ADR 0029): the table is nothing but their
/// answers cached over a domain of three values — road 1, plain 2, swamp
/// `swampWeight`. Its length follows `swampWeight` rather than a literal,
/// so swamp growing dearer carries the table with it; every index in
/// between is filled by the same arithmetic and simply never read. What
/// the length does not follow is a *dearer terrain than swamp*, so
/// `swampWeight` has to stay the dearest weight a grid can hold — a
/// weight past the table's end reads as a free step under Fable, where
/// the index is unchecked, while .NET throws. `stepWeights` hands the
/// whole domain out, and a test pins it there.
let private stepTable (stepPrice: int -> int option) : int[] =
    Array.init (swampWeight + 1) (fun weight -> stepPrice weight |> Option.defaultValue -1)

/// The flood's array accessors: checked on .NET (the F# body is the
/// ordinary index, so `dotnet test` runs the flood bounds-checked) and a
/// bare JS index under Fable, where the `[<Emit>]` template replaces the
/// call. Fable 4.12+ compiles every `arr.[i]` to a helper that re-tests
/// the index and carries a throw path, and offers no switch to drop it;
/// in the flood that helper was ~28% of the tick (#91), re-checking
/// indices the loop has already proven in range — a neighbour index is
/// built only after the `0 <= n < roomSide` guard, the heap's come from
/// its own live size, and a step-price index is a weight the grid holds,
/// which `stepTable` is built long enough for by construction. Used where
/// the caller has already proven the index in range and the read is on the
/// profile — the flood's inner loop, and the single-tile grid read below,
/// which passes `inGrid` first for exactly this reason (#173). Every other
/// array read in the Atlas stays checked.
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

/// One tile's weight in one of the Atlas's grids, and -1 — impassable —
/// for a tile off the grid. The single-tile ground query (#173): the grids
/// are laid once a tick by walking each room's collections, so asking one
/// about a tile is an array index, where asking the layer was a `Pos`
/// compared down a tree three times over — structural comparison the
/// profile put at about a fifth of the tick across the readers below,
/// none of it algorithm. The rules are the grid's, spelled where it
/// is laid (`ofSnapshotRecalling`), so the tile query and the flood answer
/// off the same numbers rather than off two copies of one rule.
///
/// The room is the caller's, as it is on every query below (ADR 0041): a
/// bare `Pos` says which tile and never which room, so the caller chooses
/// the grid and this reads it.
let private weightAt (grid: int[]) (tile: Pos) : int =
    if inGrid tile then at (indexOf tile) grid else -1

/// Whether a tile is passable in one of the Atlas's grids — the -1 above
/// read as the one thing it means. A tile off the grid, off the
/// projection, walled, or blocked in whichever grid is being asked is not
/// walkable in it, which is one answer and not four (ADR 0004).
let private walkableAt (grid: int[]) (tile: Pos) : bool = weightAt grid tile >= 0

/// The heap's push: sift up by moving the hole, not by swapping. The
/// climbing key is held in a local and each dearer parent is copied one
/// level down, so a climb of k levels writes k + 1 slots instead of 3k. A
/// push past the high-water mark grows the backing array with a slot the
/// sift is about to fill anyway, so the key itself is written exactly
/// once, at the hole it settles in (#168).
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

/// The mirror of `push`: the root is the answer, the last entry becomes
/// the key looking for a home, and the cheaper child of each pair is
/// pulled up into the hole while it undercuts that key. No two keys in
/// the heap are ever equal — a tile is pushed only where its dist
/// strictly falls, and the index term parts two tiles at one cost — so
/// pop order is fixed by the key encoding whatever shape the sift takes,
/// and with it every path the flood picks between equal costs is the one
/// it always was. The left child's win on a tie, which the swapping
/// form's `smallest` also gave it, is belt and braces. It pops by
/// shrinking the live length rather than by splicing the array, which
/// Fable compiles to a linear-time rebuild (#168).
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

/// Dijkstra flood over the weight grid from every tile in `starts`, each
/// starting at the cost the caller seeds it with, priced by `stepPrices`
/// — one body's `stepTable`, its price for a step onto a tile of each
/// terrain weight, and, beside the occupancy the caller passes, the only
/// thing that differs between the tick's floods (ADR 0029, ADR 0030).
///
/// Nothing is relaxed here: what comes back is the flood seeded and
/// unadvanced, and `settleTo` below is what runs it — as far as one tile,
/// or over the whole room (#174). Seeding is the whole of the cost a
/// flood pays to exist, which is what lets the tick's memo lay one per
/// creep per pricing and charge only the ones a reader actually asks
/// about. A start tile
/// takes its seed even when it cannot be stepped onto — a creep already
/// stands there, or is about to be placed there, or stands on the border
/// ring the engine put it down on, which is no tile of the projection's
/// ground at all: the far-side mover floods from there (#145), and
/// `firstStep` from a ring tile answers the ground beside it only because
/// the seeding does not ask what the start tile weighs. Several starts price a
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

/// Advance a flood until `goal`'s distance is final — or, for
/// `everyTile`, until the heap is empty (#174). What it fills in as it
/// goes is the flood's two grids: cheapest cost to every tile it has
/// settled (`unreached` elsewhere), plus each of those tiles'
/// predecessor index on a cheapest path (-1 elsewhere).
///
/// This is the tick's hottest loop, so it runs on flat arrays with a
/// binary min-heap of dist-then-index keys — the key ordering also keeps
/// tie-breaking deterministic — and every per-relaxation cost that is not
/// the algorithm is kept out of it: the price is a table read and not a
/// closure call through an `int option`, and the heap is the
/// hole-sifting, length-shrinking one above (#168). The three grids it
/// prices over come off the flood rather than off a closure for the same
/// reason, and because a resumed flood must charge exactly what the
/// interrupted one did.
///
/// The stopping rule is Dijkstra's own invariant, not a new one: no
/// unsettled tile can end up cheaper than the cheapest key left in the
/// heap, because every step costs at least one (ADR 0029, ADR 0010). So
/// once `dist[goal]` is at or under that frontier, nothing can lower it
/// again and the tile is finished — with the number, and the predecessor,
/// the whole flood would have left there. `unreached` is above every
/// frontier, so a tile nothing reaches drains the flood, which is the only
/// honest answer: "unreachable" is not knowable early.
///
/// The relaxation itself is untouched — the same neighbour order, the same
/// price table read, the same stale-entry test, the same heap — and the
/// only thing #174 changed is where the loop is allowed to stop. A flood
/// re-entered here picks up the heap the last read left it, so a creep
/// asked twice in one tick continues rather than starts over.
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
                let x = index / roomSide
                let y = index % roomSide

                for dx in -1 .. 1 do
                    for dy in -1 .. 1 do
                        let nx = x + dx
                        let ny = y + dy

                        if
                            (dx <> 0 || dy <> 0)
                            && nx >= 0
                            && nx < roomSide
                            && ny >= 0
                            && ny < roomSide
                        then
                            let next = nx * roomSide + ny
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

/// The whole room settled, handed back as the two grids it always was —
/// the shape every reader that reads a flood a room at a time wants: the
/// trunk's router, the spawn walk table, the far leg of a cross-room
/// price, the Seam band's walk. Those stay whole deliberately (#174):
/// each is memoised once for a whole room or a whole colony rather than
/// read at a handful of tiles, so there is nothing for an early stop to
/// save there and a second grid semantics to reason about would be all
/// cost. Only the tick's per-creep memo is resumable.
let private drained (flood: Flood) : int[] * int[] =
    settleTo flood everyTile
    flood.Dist, flood.Parents

/// What a resumable flood reaches one tile at, settling it first (#174):
/// the one read every per-tile question of such a flood goes through, so
/// no reader can see the `unreached` an unsettled tile still holds and
/// mistake it for the one that means unreachable. A tile off the grid is
/// `unreached` too, and never reaches the flood at all: the tile is the
/// caller's `Pos` and not an index the flood built, so the guard is what
/// makes the frontier test inside `settleTo` — and the read below — the
/// in-range accesses the accessors above ask for, exactly as `weightAt`
/// guards the single-tile grid read (#173) and `seamWalkTicks` guards its
/// own. It also hands unplaceable geometry the absent answer ADR 0004
/// asks of every query rather than an index off the end of the grids.
let private reachedBy (flood: Flood) (tile: Pos) : int =
    if not (inGrid tile) then
        unreached
    else
        let index = indexOf tile
        settleTo flood index
        at index flood.Dist

/// What a resumable flood has *already* reached one tile at, advancing it
/// not one pop (#176): the grid read `reachedBy` guards, handed out raw
/// and therefore never an answer — `unreached` here still means "nobody
/// has asked yet" and the number it holds may still fall. Only the bound
/// below reads it, and only ever as an upper one; nothing outside this
/// file can see it, which is the whole of what keeps #174's distinction
/// safe.
let private glimpsedBy (flood: Flood) (tile: Pos) : int =
    if not (inGrid tile) then
        unreached
    else
        at (indexOf tile) flood.Dist

/// The cheapest distance any tile the flood has *not* settled can still
/// turn out to have: the dist at the top of its heap, and `unreached` when
/// the heap has run dry — a flood with nothing left to pop has settled
/// everything it ever will, so there is no unsettled tile left for this to
/// bound and every grid read is already final. Read-only, and that is the
/// point (#176): a reader asks this to decide whether a tile is worth
/// settling at all.
///
/// It is Dijkstra's own invariant read from the other side of `settleTo`'s
/// stopping rule. A tile the flood has not popped is reached, if at all,
/// through some tile still in the heap, and no step costs less than one
/// (ADR 0029, ADR 0010) — so no unsettled tile ends up cheaper than the
/// cheapest key left. A stale duplicate sitting at the top does not weaken
/// it: the tile that will really be popped next is at or below that key,
/// and this is a lower bound either way.
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
/// flood has settled, and that is enough: every tile of a cheapest path
/// is strictly cheaper than its end — a step costs at least one — so each
/// of them settled before the goal did and the whole chain is final by the
/// time the goal is (#174).
let private firstStepOn (flood: Flood) (startIndex: int) (goalIndex: int) : int =
    let rec walk index =
        let parent = flood.Parents.[index]

        if parent = startIndex || parent < 0 then
            index
        else
            walk parent

    walk goalIndex

/// The flood every origin starts free at — the shape every caller but the
/// far leg of a cross-room walk wants, since a creep pays nothing to be
/// where it already is. Settled whole here (`drained`), because everyone
/// who reaches the flood this way reads it a room at a time; the tick's
/// per-creep memo goes through `floodPriced` below and stays resumable
/// (#174).
let private floodFromAll weights occupied stepPrices (starts: Pos list) =
    floodFromAllSeeded weights occupied stepPrices [ for start in starts -> start, 0 ]
    |> drained

/// The one-origin flood the trunk's router wants, and nothing else does
/// any more: a raw-terrain flood out of a source's tile with no creep in
/// it and no traffic seen (`trunkPath`, its only caller). Every priced
/// flood in the tick reaches `floodFromAllSeeded` through `floodPriced`
/// instead — and stays resumable there rather than being drained here
/// (#174) — so a new `Pricing` row is wired into `pricingOf` and never
/// here.
let private floodFrom weights occupied stepPrices (start: Pos) =
    floodFromAll weights occupied stepPrices [ start ]

/// What a step costs and whether the crowd is seen, for one pricing over
/// one body: the ranking price sees today's traffic and counts half-ticks,
/// the clock is blind to it and counts whole ticks (ADR 0029), and the
/// baseline counts the ranking price's own half-ticks with the crowd taken
/// out (ADR 0030). The one place the pair is laid side by side, so no
/// flood can take one half without the other and the memo cannot hold one
/// where a reader expects another. A caller with no room's occupancy in
/// hand passes `noTraffic`, which is what two of the three rows answer
/// anyway.
///
/// The price half comes out as a `stepTable` rather than as the pricing
/// function itself, because this is already the one place the two choices
/// are made together and a table is a step price the flood can read
/// without calling anything (#168). Laying it costs one pass over the
/// weight domain per flood, against one closure call per relaxation — of
/// which a 2,500-tile flood does some twenty thousand.
let private pricingOf (occupied: bool[]) (factor: FatigueFactor) (pricing: Pricing) =
    match pricing with
    | TravelCost -> stepTable (stepUnits factor), occupied
    | Walk -> stepTable (stepTicks factor), noTraffic
    | Baseline -> stepTable (stepUnits factor), noTraffic

/// The walk's flood over one body, from anywhere in `starts` (ADR 0029):
/// whole ticks a step and blind to today's traffic — the `Walk` row of
/// `pricingOf`, reached by the clocks whose origins keep them outside the
/// tick's pricing memo (the lead's cast walk, the hauler quota's round
/// trip). Every clock in the colony floods through here or through that
/// row, and there is only the one row.
let private walkFloodFromAll weights factor (starts: Pos list) =
    let stepPrices, traffic = pricingOf noTraffic factor Walk
    floodFromAll weights traffic stepPrices starts

/// The one-origin walk: a creep, or a container, prices from the tile it
/// sits on.
let private walkFloodFrom weights factor (start: Pos) =
    walkFloodFromAll weights factor [ start ]

/// The flood one pricing wants over one body, out of one origin: the
/// memoised flood a placed creep prices from. The one flood the Atlas
/// leaves resumable (#174) — it is read at a Work Area's few tiles, at a
/// goal and its predecessor chain, or at a Seam band's thirty-odd, never
/// a room at a time, so it is handed back seeded and its readers push it
/// out to the tiles they ask about and no further.
let private floodPriced weights occupied factor pricing (start: Pos) : Flood =
    let stepPrices, traffic = pricingOf occupied factor pricing
    floodFromAllSeeded weights traffic stepPrices [ start, 0 ]

/// What the flood charges for a step landing on a tile — the step price
/// plus the occupancy surcharge, exactly as the relaxation inside
/// `floodFromAllSeeded` charges it. None for a tile outside the
/// projection or one this body cannot step onto at all. Spelled once here
/// so a seeded flood's origins carry the same charge the loop would have
/// put on them — off the same `stepTable`, so no origin can be seeded at a
/// price the loop would not have charged (#168). Checked indexing: this
/// runs once a seed, not once a relaxation.
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
    let stepPrices, traffic = pricingOf occupied factor pricing

    goals
    |> List.choose (fun goal ->
        entryCost weights traffic stepPrices goal |> Option.map (fun cost -> goal, cost))
    |> floodFromAllSeeded weights traffic stepPrices
    |> drained
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

    // The room's grids, one set per projected room, filled by walking that
    // room's four collections rather than by asking a rule per tile.
    // Walking a tree compares nothing; only a lookup does, and the per-tile
    // form cost three Pos-keyed lookups a tile — 2500 tiles' worth of
    // structural comparison, the largest single cost in the tick (#96).
    // Layering by room name keeps that: the room is chosen once, and inside
    // a grid nothing is keyed by `Pos` at all. Since #173 these are also
    // the *only* form of the rules — every single-tile query reads one of
    // them (`weightAt`) — so the precedence spelled here is spelled
    // nowhere else: terrain first, then roads over the passable ground
    // they discount, then obstacles over everything. The array's initial
    // -1 is the answer for every tile outside the projection.
    let gridOf (layer: RoomLayer) =
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

        layer.CreepPositions |> Map.iter (fun _ tile -> occupied.[indexOf tile] <- true)

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

    let grids = spatial.Rooms |> Map.map (fun _ layer -> gridOf layer)
    let ground = grids |> Map.map (fun _ (bare, _, _) -> bare)
    let weights = grids |> Map.map (fun _ (_, grid, _) -> grid)
    let occupied = grids |> Map.map (fun _ (_, _, standing) -> standing)
    let rings = spatial.Borders |> Map.map (fun _ ring -> ringOf ring)

    {
        Spatial = spatial
        Home = home
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
/// seven call sites change with it — seven and not fourteen since #173 and
/// #177, the tile-at-a-time readers and the Layout's whole-room ground
/// scans having moved onto the grids, where the same absence is the same
/// all-impassable answer (`noGround`). What is left reads a layer's
/// *placements* rather than its ground, which no grid holds.
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

/// The creeps the projection places, grouped under the room each is filed
/// in — every room the projection places a creep in, and each room's
/// creeps in Snapshot creep order, the canonical order for everything
/// derived per creep. This is the Resolver's list (#145): arbitrated
/// movement (ADR 0001, ADR 0008) is a room's — ADR 0041's Consequences
/// keep it single-room, decomposed strictly per room as
/// screeps-cartographer decomposes `reconcileTraffic` — so it runs once
/// per group here, each over that room's tiles and no other's. A group
/// hands out bare `Pos`es, and they are safe to key a `Set<Pos>` of
/// blocked tiles or a `Map<Pos, string>` of occupants on precisely because
/// the group is one room's: unioned across groups, two creeps standing on
/// one coordinate of two rooms would collapse into one occupant, and a
/// fatigued outpost creep would pre-claim a home tile. The rooms come in
/// the order their first creep does, which no reader depends on: the
/// Resolver emits in Snapshot creep order across the groups. An
/// unplaceable creep is in no group — the answer ADR 0004 gives for
/// geometry a query cannot place.
///
/// The pickup reflex reads the same grouping for the same reason (#166):
/// it measures a creep's tile against a pile's, and it pairs a group with
/// `droppedEnergyIn` for that group's room, so the two bare `Pos`es it
/// compares always come out of one layer. There is deliberately no bare
/// home-only list beside this one any more — it existed for the reflex,
/// and answering home was exactly the bug.
let placedCreepsByRoom (atlas: Atlas) : (string * (string * Pos) list) list =
    atlas.Placed
    |> List.groupBy (fun (_, room, _) -> room)
    |> List.map (fun (room, creeps) -> room, creeps |> List.map (fun (name, _, pos) -> name, pos))

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
/// built) on a tile keeps a site off it; creeps do not, and neither do the
/// two transient kinds (`isTransient`) — a pile or a tombstone perturbing
/// the ordering would break the Layout's determinism (ADR 0011), and a
/// tombstone stands wherever a creep happened to die, which is exactly
/// the kind of accident a plan must not be a function of (#167).
/// Deterministic (X, Y) order. The home
/// room and no other (ADR 0041): the Layout builds in the room it is
/// anchored in, and a second room's tiles unioned in would offer the
/// Layout a coordinate it does not own.
///
/// Scanned off the raw ground grid rather than off the terrain layer a
/// `Pos` at a time (#177): the Layout asks for this whole list on every
/// census tick, and the layer form compared a `Pos` down a tree once to
/// read each of two and a half thousand tiles and again to test it against
/// the taken set. The scan runs the grid's flat index, which is
/// `x * roomSide + y` — the very (X, Y) order the layer's key order gave,
/// so the list comes out tile for tile as it did and ADR 0011's
/// determinism is untouched. Built by consing down from the last index so
/// the result is a list and never a Fable sequence.
let buildableTiles (atlas: Atlas) : Pos list =
    let ground = groundOf atlas atlas.Home

    // A grid rather than a `Set<Pos>` for the same reason the scan is one:
    // it is asked about every tile of the room. Which targets stand on a
    // tile and which do not is the rule above, unchanged.
    let taken = Array.create tileCount false

    (layerOf atlas atlas.Home).TargetPositions
    |> Map.iter (fun id tile ->
        if not (Map.tryFind id atlas.Spatial.TargetKinds |> Option.exists isTransient) then
            taken.[indexOf tile] <- true)

    let mutable tiles = []

    for index = tileCount - 1 downto 0 do
        if at index ground >= 0 && not (flagAt index taken) then
            tiles <- posAt index :: tiles

    tiles

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
// projected room can hold one of these kinds, these six have to join a
// named layer the way `placedOfKindIn` does, or the Layout counts a
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

/// Placed targets of one kind in one named room: id and tile, in id order.
/// One of the joins between the flat kind census and the layered positions,
/// and the room is named rather than searched (ADR 0041, the rule
/// `tilesWhereIn` states): its readers are the reflexes, which aim at a
/// bare `Pos` and measure it against a creep's, so a tile drawn from
/// whichever layer happened to hold the id would aim them at the same
/// coordinate of another room. A room the projection does not carry places
/// nothing — ADR 0004's absence, reached here by reading an empty layer.
let private placedOfKindIn (atlas: Atlas) (room: string) (kind: TargetKind) : (string * Pos) list =
    let layer = layerOf atlas room

    targetsOfKind atlas kind
    |> List.choose (fun id ->
        Map.tryFind id layer.TargetPositions |> Option.map (fun pos -> id, pos))

/// Towers standing in the colony's own room: id and tile, in id order —
/// the fire reflex's whole view of a tower (ADR 0014): no store is
/// projected, a dry tower's shot simply fails at the engine. Home, and
/// asking for no other room, because a tower stands only in a room we own
/// and an outpost is one we do not (ADR 0042).
let placedTowers (atlas: Atlas) : (string * Pos) list =
    placedOfKindIn atlas atlas.Home (Structure BuiltKind.Tower)

/// Dropped energy piles one room's layer places: id and tile, in id order.
/// The pickup reflex's whole view of a pile — no amount is projected, since
/// no decision reads one; a pile worth more than one carry is several trips,
/// which is a Task's arithmetic and not a reflex's.
///
/// The room is the caller's, and the reflex asks once for each room it has
/// a creep in (#166). Before that both sides of the pairing answered home
/// and an outpost's overflow lay where it fell: an Anchor standing on its
/// container spills onto the tile it stands on, the hauler that comes for
/// the container stands on that same tile, and the two never met — 3,000
/// energy on the ground across two outposts at t140,810, decaying at a
/// thousandth a tick.
let droppedEnergyIn (atlas: Atlas) (room: string) : (string * Pos) list =
    placedOfKindIn atlas room Dropped

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

/// Tiles of one room holding a built container — the container census's
/// standing half (ADR 0012): a built container keeps a plan from
/// re-dropping its site. The room is named because ADR 0040's target
/// clause is asked in the room the target stands in, and since ADR 0042
/// there are two such rooms: an outpost source's census is its own room's,
/// and a home container on the same coordinates serves nothing of it
/// (ADR 0041).
let containerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Container)

/// The same census in the colony's own room — what the Layout reads.
let containerTiles (atlas: Atlas) : Set<Pos> = containerTilesIn atlas atlas.Home

/// Tiles of one room holding a container construction site — the census's
/// pending half: a pending container is not yet a container but its tile
/// needs no new site.
let pendingContainerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Container)

/// ADR 0040's container census in one room: the tiles a container stands
/// on united with the tiles one is pending on — the set every "must
/// another container be built?" question is asked against, at home and in
/// an outpost alike. One name because it is one rule: a third member (a
/// container being dismantled, say) joins it here, and ADR 0040 cannot
/// then come to mean two different things in two rooms.
///
/// The asymmetry with `standingPostsIn`, which counts standing containers
/// alone, is the deliberate one ADR 0040 draws — a site already going up
/// answers *another one is handled*, and catches no overflow at all. Since
/// #205 a Seat's site is a Post all the same (`postsIn`), which is a claim
/// about garrisoning and not about overflow: the body standing there digs
/// and raises the container it will later dig into.
let containerCensusIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.union (containerTilesIn atlas room) (pendingContainerTilesIn atlas room)

/// The same census in the colony's own room — what the Layout reads.
let containerCensus (atlas: Atlas) : Set<Pos> = containerCensusIn atlas atlas.Home

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
/// named room's layer (ADR 0041) — the room a Reach is measured in, since
/// #138 whichever room the hostile stands in, so the cover taken out of
/// that Reach is that room's own. A room the projection does not carry
/// holds no rampart of ours.
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
///
/// Read off the raw ground grid and not the walking one (#177): swamp is
/// what the terrain is, so a road laid over it must not answer plain, and
/// the ground grid is the terrain with neither overriding pass on it. The
/// Layout asks this once per tile of the Upgrade Work Area on every census
/// tick, which was that many `Pos` comparisons down a tree (#173).
let isSwamp (atlas: Atlas) (tile: Pos) : bool =
    weightAt (groundOf atlas atlas.Home) tile = swampWeight

/// Walkable tiles adjacent to `pos` read as a tile of `room`, in
/// deterministic (X, Y) order. Standing respects obstacles, unlike Seat
/// counting. The tile handed in carries no room of its own (ADR 0041), so
/// the room rides on the API: the mover's standing candidates are the
/// creep's own room's, and since #145 the Resolver arbitrates every
/// projected room, so a creep filed under an outpost is offered that
/// room's ground and never home's. A room the projection does not carry
/// has no walkable tile beside anything. Read off that room's weight grid,
/// which is where the rule lives (#173) — so a tile off the fifty-by-fifty
/// is not walkable, exactly as a tile the projection does not carry is
/// not: `neighbours` produces both at the room's edge.
let adjacentWalkableIn (atlas: Atlas) (room: string) (pos: Pos) : Pos list =
    let weights = weightsOf atlas room
    neighbours pos |> List.filter (walkableAt weights)

/// `adjacentWalkableIn` for the colony's own room: every caller left on
/// this spelling — the spawner's birth tiles, a sink's approach — is
/// home-room geometry.
let adjacentWalkable (atlas: Atlas) (pos: Pos) : Pos list = adjacentWalkableIn atlas atlas.Home pos

/// Every tile of the room a creep may stand on — `adjacentWalkable`'s
/// answer over the whole room rather than around one tile, and the same
/// rules because it is the same grid: the terrain, road and obstacle
/// precedence `ofSnapshotRecalling` spells out, which since #173 is the
/// only place that rule is written. The room-wide half nothing wanted
/// until a Task's Work
/// Area was the room itself (ADR 0033). The room rides on the API, as
/// `adjacentWalkableIn`'s does: this is Flee's safe ground, and a creep
/// runs over the ground of the room it stands in — which since #138 is
/// whichever room a hostile's Reach is filed under, not the colony's own
/// (ADR 0041). A room the projection does not carry has no ground.
let walkableTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let weights = weightsOf atlas room

    Set.ofList
        [
            for index in 0 .. tileCount - 1 do
                if at index weights >= 0 then
                    posAt index
        ]

/// `walkableTilesIn` for the colony's own room.
let walkableTiles (atlas: Atlas) : Set<Pos> = walkableTilesIn atlas atlas.Home

/// The tile a creep stands on; None for a creep the projection does not
/// place. What a judgement about where a creep *is* reads — as
/// `positionOf` is the same question about a target — and, like it, the
/// tile in whichever room the projection files that name under, bare of
/// the room itself (ADR 0041).
let creepTile (atlas: Atlas) (creep: string) : Pos option =
    Map.tryFind creep atlas.CreepAt |> Option.map snd

/// The room a creep stands in; None for a creep the projection does not
/// place. The other half of `creepTile`, handed out on its own because a
/// bare `Pos` cannot carry it (ADR 0041): what a reader holding a
/// room-keyed fact of the colony's — a Reach, a safe set (#138) — picks
/// its room's share with. An unplaced creep names no room, and so stands
/// in no Reach and has no ground to run over, which is the answer ADR
/// 0004 gives for geometry a query cannot place.
let creepRoom (atlas: Atlas) (creep: string) : string option =
    Map.tryFind creep atlas.CreepAt |> Option.map fst

/// The room the projection files a target under; None for one it does not
/// place. `positionOf`'s other half, as `creepRoom` is `creepTile`'s: the
/// room a target's Work Area lies in, and so the room whose Reach is taken
/// out of that area (#138), and the room a spawn's doorstep is read in.
let targetRoom (atlas: Atlas) (targetId: string) : string option =
    Map.tryFind targetId atlas.TargetAt |> Option.map fst

/// What a Task acts on, and the Chebyshev range its action reaches from
/// (Screeps: harvest, withdraw, transfer and reserveController act at
/// range 1; build, repair and upgrade at range 3) — the one pair every
/// geometry query starts from. None for a Task that acts on nothing: Flee
/// has no target and no action (ADR 0033), so no area of the projection's
/// own is derived for it and no action is ever permitted.
///
/// Reserve is a range-1 act, and its target is an obstacle: a controller's
/// own tile is in `Obstacles` whether the projection saw it or a
/// declaration laid it (`Outpost.place`), so the Work Area below is its
/// walkable neighbours and the reserver stands beside the controller and
/// never on it. At W12S27's `37,43` that area is two tiles, both swamp
/// (ADR 0042) — a fact about that room's ground, not a special case here.
/// Claim is the same act on the same kind of target (ADR 0047), so it is
/// the same pair: a claimer walks to a tile beside the controller of the
/// candidate colony and takes it from there.
///
/// Pickup is a range-1 act like the other four, and its target is the one
/// that is not an obstacle: a pile lies on ground a creep may stand on, so
/// the Work Area below is that tile and its walkable neighbours — the
/// pile's own tile included, which is where a hauler that walked the whole
/// way for it ends up (#167).
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

/// Seat tiles of a placed source: walkable (non-wall) neighbours of its
/// tile, by terrain alone — structures and creeps do not consume Seats
/// (ADR 0001). Read off that room's raw terrain grid and not its weight
/// grid, which is the whole of "by terrain alone" in table form: the
/// weight grid has taken the road and obstacle passes, and a Seat with an
/// extension standing on it is still a Seat (#173).
let private seatTiles (ground: int[]) (pos: Pos) : Set<Pos> =
    neighbours pos |> List.filter (walkableAt ground) |> Set.ofList

/// Seat tiles of a source — the geometry behind `seats`, for the Layout's
/// source-container pick (ADR 0012). Empty for a source the projection
/// does not place: an unplaceable source anchors nothing, and a source in
/// a room the projection does not carry is not placed (ADR 0004). The
/// source's own room answers, not the colony's: the id resolves the room
/// (ADR 0041), so an outpost source's Seats are that room's ground and
/// never a home tile of the same coordinate.
let seatTilesOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    Map.tryFind sourceId atlas.TargetAt
    |> Option.map (fun (room, pos) -> seatTiles (groundOf atlas room) pos)
    |> Option.defaultValue Set.empty

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    Map.tryFind sourceId atlas.TargetAt
    |> Option.map (fun (room, pos) -> seatTiles (groundOf atlas room) pos |> Set.count)

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
            let weights = weightsOf atlas room

            Set.ofList
                [
                    for x in target.X - r .. target.X + r do
                        for y in target.Y - r .. target.Y + r do
                            let tile = { X = x; Y = y }

                            if walkableAt weights tile then
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
    let ground = groundOf atlas room

    targetsOfKind atlas Source
    |> List.choose (fun id ->
        match Map.tryFind id atlas.TargetAt with
        | Some(where, pos) when where = room -> Some(seatTiles ground pos)
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

/// Whether a creep stands on a Dual Seat: the one tile where a heavy body
/// has a second thing to do without moving, which is why ADR 0025 gives
/// it no reprieve through its source's empty window and ADR 0048 leaves
/// that exclusion standing. Read in the creep's own room and answered for
/// the colony's own room alone, for `postsIn`'s reason: the colony
/// upgrades one controller, so a Seat beside an outpost's is a tile
/// nobody ever upgrades from (ADR 0042). An unplaced creep stands on
/// nothing (ADR 0004).
let standsOnDualSeat (atlas: Atlas) (creep: string) : bool =
    match Map.tryFind creep atlas.CreepAt with
    | Some(room, tile) when room = atlas.Home -> Set.contains tile (dualSeats atlas)
    | _ -> false

/// The **standing** half of the Post census: the Dual Seats plus every
/// Seat under a built container. A Seat-standing container is a source
/// container by the Layout's geometry — a controller container's tile that
/// were also a Seat would already be a Dual Seat. Total: a room with
/// neither kind answers with the empty set (ADR 0004). Derived fresh each
/// tick, never persisted. Within one room, room-local censuses
/// intersected: a Post is one tile carrying a Seat and a container (ADR
/// 0041).
///
/// The Dual Seat half is the colony's own room's alone, and only the
/// container half crosses a border (ADR 0042). A Dual Seat is a tile a
/// creep harvests *and upgrades* from without moving, and the colony
/// upgrades one controller — its own (`planTasks` pools an Upgrade for
/// `snapshot.Controller`, never for a declared outpost's controller,
/// which it reserves instead). Counted in an outpost the intersection
/// would name a tile nobody ever upgrades from, and that tile would be a
/// Post: an Anchor place and an income share for an outpost source with
/// no container standing under it — precisely the switch ADR 0042 makes
/// the container be. So a room the colony does not upgrade in has exactly
/// the Posts its built containers give it.
///
/// Separated from `postsIn` below by #205, and the split is the one ADR
/// 0042 already draws between what a room is *worth* and what it is
/// *worked* from: this is the switch that admits a source into the quotas
/// — a haul term, an income share — and it is a standing container that
/// throws it, because a site produces nothing anybody hauls. The Anchor's
/// garrison is the other question and it answers it one tick earlier.
let private standingPostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    let containerPosts =
        Set.intersect
            (seatUnionIn atlas room)
            (tilesOfKindIn atlas room (Structure BuiltKind.Container))

    if room = atlas.Home then
        Set.union containerPosts (dualSeatsIn atlas room)
    else
        containerPosts

/// Seats carrying a container **construction site** — the Post a heavy
/// body is hired for before the container it will dig into exists (#205,
/// amending ADR 0045 and ADR 0046).
///
/// The colony used to raise these containers with the body that stands on
/// them: an Anchor digs twelve a tick and spends it into the site under
/// its own feet, so a 5,000-progress container goes up in a few hundred
/// ticks off a source that is otherwise producing nothing. Two later rules
/// closed that door between them — ADR 0045 emptied the Work Area of an
/// unposted outpost source, so no heavy body would walk there at all, and
/// ADR 0046 shut Build to a standing body — and what was left was the
/// worker row commuting fifty tiles a Seam apart at fifty energy a trip.
/// An invader that demolishes three outpost containers then costs the
/// colony thousands of ticks of income rather than hundreds.
///
/// So a Seat with a container site on it is a tile worth garrisoning, on
/// the same terms every other Post is: one Anchor, its Harvest narrowed to
/// it, and travel cost to walk it there. Read off the Seats and never off
/// the site's range, which is the trap #205 names: a site a step off this
/// source's Seats belongs to whatever source seats *it*, and counting it
/// here would hire a garrison for a rock nobody can dig from that tile.
///
/// Room-local like every other half of the census (ADR 0041), and the
/// home room is inside the rule rather than outside it: an RCL2 colony's
/// own source container goes up the same way, and a source with no site
/// and no container keeps ADR 0020's bare-Seat fallback at home exactly as
/// it had it.
let private containerSitePostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (seatUnionIn atlas room) (pendingContainerTilesIn atlas room)

/// Posts of the room: the tiles worth garrisoning with a heavy-WORK body
/// (ADR 0012) — the standing census above, plus the Seats carrying a
/// container site (#205). The capacity unit of the Anchor quota and of
/// Harvest's own concurrency (ADR 0024), and the only footing a Work-heavy
/// body harvests from (ADR 0020). Total, room-local and derived fresh each
/// tick, exactly as its two halves are.
///
/// What the two halves are for is what keeps them apart: this one is
/// *ground* — where a heavy body stands and what it may dig from — and
/// `standingPostsIn` is *income*, the switch a haul term and an income
/// share hang off (`Decide.isPosted`). A site is a garrison place and not
/// yet an economy, so it counts here and not there.
let private postsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.union (standingPostsIn atlas room) (containerSitePostsIn atlas room)

/// The Posts of the colony's own room — the doc above governs both.
let posts (atlas: Atlas) : Set<Pos> = postsIn atlas atlas.Home

/// Every projected room's Posts, counted: the Anchor row's quota (ADR
/// 0012, widened to the outpost layer by ADR 0042). An outpost's Post is
/// the same garrison tile a home Post is and hires the same row — one
/// Anchor apiece, sized by the same rule and walked there by travel cost
/// like any other body, which is why the outpost needs no remote-miner
/// concept of its own.
///
/// Counted room by room and summed, never unioned (ADR 0041): a `Pos`
/// carries no room, so two rooms whose Posts share a coordinate are two
/// garrison tiles fifty tiles and a border apart, and a union would hire
/// one Anchor for the pair.
///
/// A Post is a **vision fact**, and this row flaps with vision. It is not
/// the layer that gates it: a declared outpost always carries one, terrain
/// and all, whether or not the colony can see the room this tick — that is
/// the half of ADR 0041 vision may not gate, and reading absence onto the
/// declaration instead is the deadlock #148 broke. What vision gates is
/// the *container* — and, since #205, the site standing in for it: both
/// are seen entities, absent from the census entry by entry until vision
/// returns (ADR 0004), so a blind outpost's Seat has nothing on it here
/// and hires no Anchor — including on the tick its own Anchor died and
/// stopped supplying the vision that counted it. A room leaves this fold
/// altogether only when the scan set drops it (ADR 0043).
let postCount (atlas: Atlas) : int =
    atlas.Spatial.Rooms
    |> Map.fold (fun total room _ -> total + Set.count (postsIn atlas room)) 0

/// Tiles holding a standing container on a Post — the tiles a work-heavy
/// body garrisons and cannot flee from (ADR 0033), ramparted beside the
/// Keep (ADR 0034). A Post that is a bare Dual Seat is not one of these:
/// what the rule covers is a structure standing, and there is none there.
/// The colony's own room, like the ramparts it is raised under.
let postContainerTiles (atlas: Atlas) : Set<Pos> =
    Set.intersect (containerTiles atlas) (posts atlas)

/// The Posts of one source: its own Seats that are Posts. Empty for a
/// source the projection does not place, and for one with none of the
/// three — a built container on a Seat, a container site on a Seat (#205),
/// or a Dual Seat. Every half is read in the source's own room (ADR 0041)
/// — intersecting an outpost source's Seats with the home room's Posts
/// would answer a tile standing in neither — and the Seat join is what
/// keeps a neighbouring source's site out: a Post belongs to the rock it
/// seats, not to the rock it is near.
let postsOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    match Map.tryFind sourceId atlas.TargetAt with
    | None -> Set.empty
    | Some(room, _) -> Set.intersect (seatTilesOf atlas sourceId) (postsIn atlas room)

/// The **standing** Posts of one source: `postsOf` above less the Seats
/// whose container is still a site — the switch that admits a source into
/// the quotas (ADR 0042, and #205's split). One reader, `Decide.isPosted`,
/// which is itself the one spelling the hauler term and the income base
/// share: a site produces nothing anybody hauls, so it hires the garrison
/// that raises it and buys no mouths at home against income that does not
/// exist yet. The tick the container stands, both answers agree again.
let standingPostsOf (atlas: Atlas) (sourceId: string) : Set<Pos> =
    match Map.tryFind sourceId atlas.TargetAt with
    | None -> Set.empty
    | Some(room, _) -> Set.intersect (seatTilesOf atlas sourceId) (standingPostsIn atlas room)

/// Whether a creep stands on the container construction site named — its
/// own Post with the container still going up (#205). The one geometry
/// that reopens Build to a standing, Work-heavy body: the site is under
/// its feet, so what ADR 0046 forbids — a delivery walked to, one tick of
/// spending bought with two of commute — is not what this body is being
/// asked to do, and what ADR 0020 pins it to is the very tile it is
/// already on.
///
/// Three joins, all of them load-bearing. The **kind**, because a Post
/// carries other sites: ADR 0034 ramparts a Post container, so a rampart
/// site on this tile would otherwise open the gate on the strength of the
/// container site beside it. The **Seat**, through `containerSitePostsIn`,
/// because a container site that seats no source is the controller's
/// buffer and a delivery like any other. And the **room**, because a `Pos`
/// carries none (ADR 0041): a creep at home on an outpost site's
/// coordinates stands on nothing of that room's.
///
/// Total (ADR 0004): an unplaced creep, an unplaced site and a site of any
/// other kind each answer false, leaving the gate exactly as ADR 0046 had
/// it.
let standsOnPostSite (atlas: Atlas) (creep: string) (siteId: string) : bool =
    Map.tryFind siteId atlas.Spatial.TargetKinds = Some(Site BuiltKind.Container)
    && (match Map.tryFind creep atlas.CreepAt, Map.tryFind siteId atlas.TargetAt with
        | Some(creepRoom, tile), Some(siteRoom, sitePos) when creepRoom = siteRoom && tile = sitePos ->
            Set.contains tile (containerSitePostsIn atlas creepRoom)
        | _ -> false)

/// Whether a creep is standing on one of the named source's Posts whose
/// container is still a site (#205) — that Post's garrison, read off where
/// the body *is* and never off what it holds this tick.
///
/// Which is the one place a site Post differs from a standing one, and
/// Harvest's Post cap is what reads it (ADR 0024). On a standing container
/// the garrison never lets its Post go: a full store is reprieved by the
/// overflow the engine catches, so it holds the source's one Harvest slot
/// from arrival to death. On a site there is no overflow to catch — the
/// store fills, Harvest falls away and Build takes over until it is empty
/// again — so a cap counting Harvest's holders alone would read the tile
/// as free on every build tick and admit a second heavy body onto the one
/// tile the first is standing on. "One Anchor per Post" (ADR 0012) is a
/// claim about the tile, so the tile is what this answers.
///
/// Room-joined like every other half of the census, because a `Pos`
/// carries no room (ADR 0041), and the Seat join is the same one
/// `postsOf` makes: a garrison belongs to the rock it seats. Total (ADR
/// 0004): an unplaced creep and a source the projection does not place
/// each answer false, leaving the cap exactly as ADR 0024 had it.
let standsOnSitePost (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind sourceId atlas.TargetAt with
    | Some(creepRoom, tile), Some(sourceRoom, _) when creepRoom = sourceRoom ->
        Set.contains tile (seatTilesOf atlas sourceId)
        && Set.contains tile (containerSitePostsIn atlas creepRoom)
    | _ -> false

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
/// source that has a Post narrows to it even when the projection blocks
/// it: an area with nothing standable in it makes the Task inapplicable,
/// exactly as an unreachable one does for every Task, rather than silently
/// widening back to the Seats (ADR 0020). Only Harvest narrows, so
/// this never re-enters the `posts` derivation that reads the Upgrade
/// area. Memoised per Task for the tick beside the unnarrowed areas.
///
/// A container **site** on a Seat is a Post since #205, so the tile a
/// heavy body is narrowed to is sometimes the tile it is about to build:
/// it digs from the site, fills its one Carry, and spends it into the
/// progress under its feet (`Decide.applicable`'s Build arm). That is the
/// amendment to the paragraph below — an outpost source whose container
/// the invaders demolished has a Work Area again the tick the plan drops
/// the site back, and the body that stands there is the one that raises
/// it.
///
/// A source with **no** Post narrows nothing at home and narrows to
/// nothing everywhere else, and the room is the whole of what separates
/// the two. ADR 0020's fallback to the bare Seats is a *bootstrap* rule —
/// it is what carries the colony's own room before its first container is
/// built, and there a stranded Anchor is a few tiles from a spawn that can
/// replace it and from haulers already working the room. An outpost
/// bootstraps through neither: its first steps are a reserver and a light
/// builder raising the container (ADR 0042, #157), and a heavy body on an
/// outpost Seat with no container under it digs twelve a tick onto the
/// ground, in a room whose haul quota does not exist yet because that
/// quota is what the container switches on. Travel cost, which reads only
/// that the Seat is near, is exactly what walks it there — an Anchor hired
/// for one outpost's Post spent on another outpost's bare rock. So this is
/// the geometric dual of ADR 0042's "an unposted outpost source is worth
/// nothing to the workforce": worth nothing in the quotas, and standable
/// on by nobody in the geometry. Two switches on one tile since #205, and
/// this is the ground one: the container **or its site** makes the Post a
/// heavy body may stand and dig on, where income waits for the container
/// alone to stand (`standingPostsIn`, `Decide.isPosted`). A rock with
/// neither is outside both, which is the source this rule is about.
///
/// A source the projection does not place keeps the fallback (ADR 0004):
/// it has no room to be outside of, and its Work Area is empty in either
/// reading, so absence answers as it did before there was a second room
/// rather than becoming a third answer.
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

            if not (Set.isEmpty postTiles) then
                Set.intersect (workArea atlas task) postTiles
            else
                // Absence is home's answer and not an outpost's: only a
                // source the projection places in another room loses the
                // fallback.
                match targetRoom atlas sourceId with
                | Some room when room <> atlas.Home -> Set.empty
                | _ -> workArea atlas task)
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
/// Neither #123 nor #142 widened this, and that is the decision: the
/// cross-room *price* is a minimum over the Seam band (`pricedAcross`),
/// joined where the rooms are both in hand, while the *tiles* a creep is
/// handed stay its own room's. Geometry crosses the border; standing and
/// acting do not (ADR 0041's Consequences). A caller that wants the far
/// room's origins asks `narrowedArea` above, which is the same narrowing
/// with no creep's room in it.
///
/// The mover crosses too, and it does so *around* this query rather than
/// through it (#142): `firstStep` takes the Task beside these tiles and
/// answers the near side of the winning Seam when they are empty, so the
/// one thing that had to grow a border-crossing answer got one without the
/// action gate and the reachability gate — which read this very set —
/// growing one as a side effect. Widening the area to the Seam's near
/// neighbours instead would have told a creep standing on one that it may
/// dig a source a room away.
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

/// Whether a creep stands where it could dig a source: in that source's
/// own room and within the engine's harvest range of it (range 1). The
/// widened half of `catchesOverflow` above and deliberately weaker (ADR
/// 0048): that one asks whether the tile catches a full store's overflow,
/// which is a fact about the container underfoot and about nothing else,
/// while this asks only whether the creep is in position to dig the tick
/// the energy lands — the question a source's empty window puts, where
/// the container is beside the point.
///
/// Measured by range rather than by Seat membership, for the reason
/// `mayAct` keeps its own range fallback: the Seats are read off the
/// projection's terrain grid, and a creep the engine has put on ground
/// the projection carries none for — a tile outside the terrain it was
/// handed — is in position all the same, and the engine will let it dig
/// from there (ADR 0004). Total in the same way, and the room is
/// load-bearing: a creep at home on the coordinate an outpost source
/// seats is nowhere near it (ADR 0041).
let standsAtSource (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind sourceId atlas.TargetAt with
    | Some(creepRoom, tile), Some(sourceRoom, source) when creepRoom = sourceRoom ->
        range tile source <= 1
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
/// is answered from the projection's border layer, which enters no
/// walking weight grid, no walkable or buildable set and no Work Area —
/// the Atlas lays it a grid of its own beside those (`Atlas.Rings`) and
/// never inside them — so the Matcher cannot pick one and have the engine
/// empty it the tick a creep arrives.
/// A pair is in the band when neither side is wall; a swamp exit is in
/// it, dearly, exactly as swamp ground is. Deterministic (X, Y) order.
/// Total (ADR 0004): two rooms that are not orthogonal neighbours, and a
/// room the projection carries no border for, answer with the empty
/// band — an unpriceable Seam is no Seam, never a blocked one, so it
/// costs nothing and blocks nothing.
///
/// Both sides are read off the ring grids (#173), which is the border
/// layer in the same table form the ground has: the band is asked for once
/// per creep priced across a border, and each of its forty-eight candidate
/// pairs cost a `Pos` compared down two terrain trees before that.
let seams (atlas: Atlas) (fromRoom: string) (toRoom: string) : (Pos * Pos) list =
    match worldCoordsOf fromRoom, worldCoordsOf toRoom with
    | Some(hereX, hereY), Some(thereX, thereY) ->
        let near = ringOf atlas fromRoom
        let far = ringOf atlas toRoom

        borderPairs (thereX - hereX, thereY - hereY)
        |> List.filter (fun (here, there) -> walkableAt near here && walkableAt far there)
    | _ -> []

/// Whether a creep stands on a Seam — its room's border ring, the tile the
/// engine put it down on the tick it crossed (#142). Never ground: the
/// projection's floor stops at 1..48 (ADR 0036), and a creep that ends a
/// tick on the ring is moved out of the room again by the engine, so a
/// ring tile is no place the Resolver may settle a creep on — the far-side
/// mover's rule (#145) reads this to walk a landed creep inward rather
/// than leave it where it stands. Read off the coordinate alone, because
/// the ring is the ring whatever its terrain: a creep is only ever on a
/// passable tile of it. Total (ADR 0004): a creep the projection does not
/// place stands on no Seam.
let standsOnSeam (atlas: Atlas) (creep: string) : bool =
    match Map.tryFind creep atlas.CreepAt with
    | Some(_, pos) -> pos.X = 0 || pos.X = exitEdge || pos.Y = 0 || pos.Y = exitEdge
    | None -> false

/// The tiles of a room's own ground next to one of its exit tiles — the
/// only tiles a flood can price a Seam's near side from, or step off its
/// far side onto, because the border ring is not ground and no flood ever
/// enters it (ADR 0036, ADR 0041). Diagonals included — the engine lets a
/// creep step onto an exit diagonally, and onto its first tile in the new
/// room the same way.
///
/// Clipped to the room's *ground* and not merely to the grid (#175). The
/// answer is the same either way — a neighbour the grid marks impassable
/// is a tile the flood's relaxation never enters (`settleTo`), so it holds
/// `unreached` and adds nothing to a minimum, and a seeded far leg drops
/// it at `entryCost` before it is ever a seed — but the cost is not: the
/// per-creep flood is resumable since #174, so `reachedBy` on a tile
/// nothing reaches settles the whole room. Half of every exit's
/// neighbourhood is more ring, so a near leg read tile by tile over a
/// thirty-odd crossing band drained 2,500 tiles at its first crossing and
/// handed an outpost creep none of #174's saving. What the read still pays
/// after this is the band's own width and not the ring's — and since #176
/// not the whole of that width either: the join walks the band cheapest
/// half first and never settles for a crossing whose lower bound already
/// loses (`boundOn`). What is left is the price of the answer rather than
/// of the read — a creep whose winning crossing is genuinely far from it
/// still settles most of its room, and one whose is near no longer pays
/// for the dearest crossing in the band. The grid does the clipping,
/// `inGrid` and all (`weightAt`), so there is one rule here and not two.
let private besideExit (grid: int[]) (tile: Pos) : Pos list =
    neighbours tile |> List.filter (walkableAt grid)

/// The same tiles for the leg a flood is *seeded* on, which is one tile
/// wider: a flood seeds its origin whatever that tile weighs, so a creep
/// the engine parked on the border ring the tick it crossed (#142, #145)
/// reaches the crossings beside it at no cost — the one tile off a room's
/// ground a near leg can honestly be read at. Dropping it with the rest of
/// the ring would price such a creep as though it had to walk back inward
/// before it could cross, which is a different answer and not a cheaper
/// route to the same one. Every other tile of the ring stays out, so the
/// drain #175 removed stays removed: the exception is one tile the flood
/// has already settled, never a tile it would have to run to reach.
///
/// Where such a creep is then aimed is not ruled on here: that a creep on
/// the ring is stepped sideways along it to an adjacent crossing is #146's
/// open question, and #175 keeps the answer the tree already gave rather
/// than settling it.
///
/// The origin is read once and not once per neighbour: a creep standing on
/// its own room's ground has no exception to make, and the test is a `Pos`
/// comparison inside the band loop this ticket exists to cheapen.
let private besideExitFrom (grid: int[]) (origin: Pos) (tile: Pos) : Pos list =
    if walkableAt grid origin then
        besideExit grid tile
    else
        neighbours tile
        |> List.filter (fun near -> near = origin || walkableAt grid near)

/// The cheapest a flood reached any tile of a set at, and None when it
/// reached none of them — the one read every arrival at a set of tiles is
/// taken through, so the near leg's arrival at a crossing is the same
/// arithmetic whether the far leg is joined tile by tile (`joinedAcross`)
/// or seeded into one flood (`castAcross`, #169), and the hauler quota's
/// same-room leg reads its sink's approach by the same rule. Since #174
/// the in-room price reads it too (`pricedPathTo`), which is what makes
/// "the one read" literal: a Work Area, a Seam band and a sink's approach
/// are one question asked of three tile sets. Unreachable is an absence
/// and never a number, exactly as it is inside one room (ADR 0004).
///
/// The flood arrives as the read itself — `reachedBy` for the tick's
/// resumable per-creep floods, `reachedIn` for the ones settled whole —
/// so the arithmetic is written once for both and a resumable flood is
/// pushed out to every tile of the set, the farthest deciding how far it
/// runs (#174).
let private nearestReached (reached: Pos -> int) (tiles: Pos list) : int option =
    tiles
    |> List.choose (fun tile ->
        let d = reached tile
        if d = unreached then None else Some d)
    |> function
        | [] -> None
        | costs -> Some(List.min costs)

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
/// tile the tick it crosses, and `Snapshot` files it there. The choice of
/// crossing is spent: since #142 the mover aims a cross-room creep at the
/// near side of whichever exit this minimum won at, so the omission is not
/// "nobody reads it". It is that the ring is not arbitrated ground — the
/// Resolver settles one room's tiles (ADR 0041's Consequences) and an exit
/// is in no room's — and that an occupant of one is gone by the next tick,
/// the engine moving it off the border row it ended on. A surcharge buys a
/// detour around traffic that stands; a transiently occupied exit costs at
/// most a retry, and a permanent detour priced off it would be wrong the
/// tick after. None for an exit the
/// projection has no terrain for, or a wall, or a body that cannot step at
/// all — an unpriceable crossing is no crossing (ADR 0004).
///
/// Priced off the body's `stepTable`, which the caller hands in already
/// laid: both callers ask this once per crossing over a band of thirty-odd
/// (#168), and the table is the same for every one of them — and off the
/// room's ring grid rather than its border map, for the same reason (#173).
let private exitPrice (atlas: Atlas) (stepPrices: int[]) room tile =
    let weight = weightAt (ringOf atlas room) tile

    if weight > 0 then
        let step = stepPrices.[weight]
        if step >= 0 then Some step else None
    else
        None

/// The body a *plan* is priced for: fatigue parity, one fatigue-generating
/// part to one Move (ADR 0003), which under the walk's rounding is a tick
/// on plain and five on swamp. The trunk router prices its line on the
/// same neutral body (`trunkPath`) and for the same reason — a placement
/// that moved with whichever creep happened to be alive this tick would
/// not be a plan — and the Layout's determinism (ADR 0011) is what that
/// buys.
let private planningFactor: FatigueFactor = { FatigueParts = 1; MoveParts = 1 }

/// The walk out to a Seam, from every tile of one room's ground: the
/// smallest, over the whole band joining that room to the named
/// neighbour, of the walk to a tile beside a crossing plus the price of
/// stepping onto the crossing itself. Flooded *into* the band rather than
/// out of each tile, so one flood answers every Seat of every source in
/// the room, and seeded at each origin's own entry cost for the reason
/// `floodPricedInto` is — the engine charges a step on the tile it lands
/// on, so a flood run backwards charges the wrong end by exactly one
/// tile. What comes back at a tile `t` is therefore `cost(t) + walk(t ->
/// the Seam)`, with the tile's own step still in it; the public query
/// takes that back off.
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

/// The walk in whole ticks from one tile of a room's own ground out to the
/// Seam joining it to a neighbour — a walk *to* the border and not across
/// it, which is the near half of `pricedAcross` with the far leg left off.
/// No creep ever walks it: it is the anchor ADR 0042's outpost container
/// pick is made against, an outpost having no spawn for a trunk to anchor
/// on, and the Seam is the one fixed thing in that room home lies beyond.
///
/// Priced as a walk (ADR 0029) — whole ticks, nothing below one, blind to
/// today's traffic — for a body at fatigue parity (`planningFactor`). The
/// blindness is the hauler quota's round trip and the lead's cast walk
/// again, and here it is load-bearing twice over: a creep standing on a
/// Seat must not move a container plan, and the plan must answer the same
/// tile on the tick the container it planned is finally stood on.
///
/// The tile it is asked at is charged nothing, exactly as every other walk
/// in the colony charges the tile a creep already stands on nothing: what
/// a Seat costs to step onto is paid by whatever walks *in* to it, never
/// by the haul leaving it. That is the whole of the subtraction below, and
/// it is what keeps two Seats of one source compared on the ground between
/// them rather than on their own terrain.
///
/// Total (ADR 0004): two rooms with no band between them, a room the
/// projection carries no ground for, a tile off the grid and a tile no
/// crossing reaches all answer with no walk at all — an unpriceable Seam
/// is no Seam, never a blocked one, so it costs nothing and blocks
/// nothing.
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

/// The near leg of a cross-room join, in the two shapes its two callers
/// hand it (#174): the tick's own per-creep flood, which the join may
/// push further, and a flood some caller already settled whole, which it
/// may only read. Both answer the same two questions — what a tile is
/// finally reached at, and what it cannot possibly beat — so the join
/// below is written once and neither caller gets an arithmetic of its own
/// (ADR 0030).
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

/// A lower bound on what the near leg will finally reach the cheapest
/// tile of a set at, taken without advancing it a single pop — the
/// licence for #176's early stop, and the argument that it moves no
/// answer.
///
/// Per tile: the flood's own frontier bounds every tile it has not
/// settled (`frontierOf`), while a tile it *has* settled already holds
/// its final number, and the grid read is an upper bound on that number
/// for every tile whatever its state. So `min(frontier, glimpse)` is at or
/// below the tile's final distance in both cases, and the smallest of
/// those over the set is at or below the set's own minimum. Both halves
/// matter: the frontier alone is not a bound, because a tile settled
/// cheaply while the flood ran past it toward another crossing sits
/// *below* the frontier, and adjacent crossings in a band share their
/// approach tiles, so that is the ordinary case here and not the exotic
/// one.
///
/// What the join does with it: the three terms are non-negative — an exit
/// costs at least one step (`exitPrice`), a far leg at least nothing — so
/// `bound + crossing + departure` is at or below the sum any crossing can
/// still produce. A crossing whose bound already exceeds the best sum in
/// hand can therefore never win it and is never settled for; every other
/// crossing is settled and compared exactly as it was before. The answer
/// is `List.min` over the whole band either way, because the crossings
/// skipped are only ever crossings that lose (#176).
///
/// An empty set — an exit with no ground of this room beside it — bounds
/// at `unreached`, which is the same absence `nearestReached` would answer
/// it with, and keeps the addition above out of overflow.
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

/// A cross-room price, joined on the Seam: the smallest, over the whole
/// band between the two rooms, of *walk to the exit tile* + *the exit
/// tile's own price* + *walk in from the tile it lands on* (ADR 0041,
/// narrowed by #123). Each leg is a single-room flood the caller has
/// already run — so no flood ever leaves its room and the join is a
/// minimum over thirty-odd additions rather than over thirty-odd floods.
///
/// The join itself and not a second one (ADR 0030). Two callers reach
/// it, and they differ in nothing but which two floods they hand it: a
/// creep priced toward a Task (`pricedAcross`) floods out of the creep and
/// into that Task's Work Area, and the hauler quota's round trip
/// (`haulRoundTripTicks`) floods out of a container and into the sink's
/// approach. A lead's cast walk (`castWalkTicks`) is the third reader of
/// this arithmetic and no longer a caller: it wants the answer at *every*
/// tile of the far room rather than at one, so since #169 it folds the
/// same three terms into the seeds of one flood (`castAcross`) instead of
/// summing them per goal. The sum below is the statement of the rule and
/// that seeding is the same rule read forwards; a change here is a change
/// there. What the two floods owe this rule
/// is fixed: the near one is
/// `fromRoom`'s and charges every tile it enters, the far one is the
/// other room's and is run *into* its goals with each goal seeded at its
/// own entry cost (`floodPricedInto`). Hand it a far leg flooded the
/// ordinary way round and the sum below is short by a tile, every time.
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
/// opens onto nothing all answer with no price at all — the same answer
/// unreachable geometry gets inside one room.
///
/// The winning exit tile comes back beside the price, and that is #142's
/// whole decision: the mover aims a cross-room creep at the near side of
/// the crossing the price was paid at, so the Seam it walks and the Seam it
/// is ranked on are the same one. Taking a second minimum somewhere else
/// would agree on every number and diverge on every tie — two argmins over
/// one tied band pick two exits — which is a creep sent to one crossing
/// while priced at another. The minimum is over `(sum, exit)` pairs, so the
/// price is the same number it always was and the tie falls to the lowest
/// (X, Y) exit, exactly as every other tie in the Atlas falls.
///
/// Neither leg arrives as a grid (#174): the far leg is a tile read, and
/// the near one a `NearLeg` (#176), which carries the flood itself so the
/// join can bound it without advancing it. The near leg of a creep's
/// price is the tick's resumable memo and pushes out over the band as the
/// band is walked, while the far leg and both legs of the hauler quota's
/// round trip are floods already settled whole. Each side is read at its
/// own room's ground alone and never at the ring between them
/// (`besideExit`, #175) — which is the same minimum, taken without
/// settling a resumable near leg over a room to learn that a ring tile is
/// unreachable. The near leg's origin rides in for the one tile
/// that rule has to spare: the flood is seeded there whatever it weighs,
/// so a creep standing on the ring prices from where it stands.
///
/// And the near leg is no longer pushed out as far as the dearest
/// crossing in the band (#176). What it is pushed out to is the first
/// crossing the band's `crossing + departure` order admits — which is not
/// in general the one that wins — and after that only the crossings whose
/// bound leaves them able to tie the best sum in hand; every other
/// crossing is skipped without a single pop. See `NearLeg` above for the
/// bound and why it moves no answer.
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

    // Everything but the near leg, priced first, and the band ordered by
    // it. The far leg is a flood settled whole and the exit's own price a
    // table read, so this costs the band a read apiece and no relaxation
    // at all — and it is what makes the bound below bite early: a crossing
    // whose own two terms already beat every other's is the one most
    // likely to set a best sum the rest cannot reach. A crossing the body
    // cannot pay for, or whose landing tile opens onto nothing, drops out
    // here exactly as it always did.
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
        // On the sum of the two terms alone, which is a primitive key over
        // a list the band already ordered (#96): the answer below does not
        // depend on this order — every crossing left unsettled is one the
        // bound proved cannot win — so ordering is a matter of how much
        // work is saved and never of what is answered.
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
               // sum, so a crossing that can only *tie* still has to be
               // looked at — the tie falls to the lowest exit, exactly as
               // `List.min` over the whole band fell.
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

/// A creep's cross-room price toward a Task: the join above over this
/// creep's own memoised flood and the Task's far leg, the one flooded out
/// of the target and shared colony-wide, so a second creep pricing the
/// same Task across the same border pays for no second flood (ADR 0041).
/// The band is read before either flood is forced, because a pair of
/// rooms with no Seam between them has no price to pay for and no flood
/// to run for it (ADR 0004).
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
            nearestReached (reachedBy (flood atlas pricing room creep pos)) (Set.toList area)

/// The border a Task asks its creep to cross, or None when it asks for
/// none: the creep's room, the tile it stands on, and the target's room,
/// once the two names have been read and found different (ADR 0041).
///
/// One spelling, because two readers settle the rooms and they must settle
/// them alike: the price (`pricedPath`) and the mover's step
/// (`stepAcross`, #142). Absence is not a crossing — a Task acting on
/// nothing, an unplaced creep and an unplaced target each keep the answer
/// they had before the projection layered, which is the same permissive
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
    | _ ->
        match borderCrossing atlas creep task with
        | Some(creepRoom, from, targetRoom) ->
            pricedAcross atlas pricing creep task creepRoom from targetRoom
            |> Option.map fst
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
    // this is asked here rather than inferred from an empty one — which is
    // also what keeps the gate shut while the mover walks a creep at the
    // Seam (#142): a creep beside an exit tile, or on one, is still a room
    // away from its target, and the gate opens by itself the tick the
    // engine puts it down on the far side.
    | Some _ when not (sharesRoom atlas creep task) -> false
    | Some(targetId, actionRange) ->
        match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
        | Some(creepRoom, creepPos), Some(_, targetPos) ->
            if not (walkableAt (weightsOf atlas creepRoom) creepPos) then
                range creepPos targetPos <= actionRange
            else
                Set.contains creepPos area
        | _ -> true

/// First step toward a set of goal tiles under one pricing: the in-room
/// half of `firstStep`'s contract, whose doc governs the floods, the
/// tie-breaking and the totality here. Only that half — the border-
/// crossing fallback is the public wrappers' own (`stepAcross`, #142), so
/// a creep whose target is a room away answers `None` here while
/// `firstStep` answers the near side of the winning Seam. Three callers,
/// not two: both wrappers, and `stepAcross`, which reuses it for the
/// approach leg over a set of exit-adjacent tiles that is no Work Area.
let private firstStepVia
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (goals: Set<Pos>)
    : Pos option =
    match Map.tryFind creep atlas.CreepAt with
    | None -> None
    | Some(room, pos) ->
        if Set.isEmpty goals || Set.contains pos goals then
            None
        else
            let near = flood atlas pricing room creep pos

            goals
            |> Set.toList
            |> List.choose (fun goal ->
                let d = reachedBy near goal
                if d = unreached then None else Some(d, goal))
            |> function
                | [] -> None
                | reachable ->
                    let _, goal = List.min reachable
                    Some(posAt (firstStepOn near (indexOf pos) (indexOf goal)))

/// The step a creep takes toward a Task whose target stands in another
/// room: toward the near side of the Seam the price was paid at (#142).
/// The exit tile is the creep's *own* room's border tile, so aiming at it
/// asks nothing of the neighbour and arbitrates nothing across the Seam —
/// ADR 0041's boundary stands exactly where it stood. The creep walks to a
/// ground tile beside that exit, steps onto the exit, and the engine puts
/// it down in the neighbour at the end of that tick; from there it shares
/// the target's room and every rule already written takes the creep on.
///
/// The exit is the one `pricedAcross` won on, taken out of that same
/// minimisation rather than looked for again: a second argmin agrees on
/// every number and splits on every tie, which walks a creep to one
/// crossing while ranking it at another. Under the caller's own pricing,
/// as every route in the Atlas is — so the traffic-blind route may pick a
/// different crossing than the priced one, and that difference is the
/// occupancy surcharge's, which is precisely what the reroute attribution
/// reports (ADR 0008, ADR 0018).
///
/// Two legs, because the exit tile is not ground and no flood enters it
/// (ADR 0036): from anywhere else the goals are the ground tiles beside
/// the exit, and from one of *those* the step is the exit tile itself. It
/// is the one tile the mover ever aims at that nothing may stand on, and
/// it never becomes a Seat, a Work Area member or a standing candidate —
/// the projection carries no ground there, so no query can offer it.
/// Total (ADR 0004): no crossing, no step.
let private stepAcross (atlas: Atlas) (pricing: Pricing) (creep: string) (task: Task) : Pos option =
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
                Some exitTile
            else
                firstStepVia atlas pricing creep (Set.ofList approach))

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
/// (ADR 0041): a step is a step inside a room. A creep standing on the
/// room's border ring — where the engine lands it the tick after a
/// crossing — is not on ground, and still gets a step: the flood seeds
/// its start tile regardless of weight, so the answer is the ground tile
/// beside the ring that the cheapest path leaves by (#145).
///
/// The Task rides beside them for the one case a bare tile set cannot
/// carry (#142): a target the projection files under another room name
/// leaves the creep-aware Work Area empty — the tiles are the neighbour's
/// and a `Set<Pos>` cannot say so — and the step is then toward the near
/// side of the winning Seam instead. The same shape travel cost has had
/// since #123, and for the same reason: the tiles a creep is handed stay
/// its own room's while the *price* crosses, so the Task the Matcher
/// ranked across a border is a Task the mover can also walk toward. The
/// goals win whenever they yield a step, so a creep with somewhere to
/// stand is never pulled toward a border instead.
let firstStep (atlas: Atlas) (creep: string) (task: Task) (goals: Set<Pos>) : Pos option =
    match firstStepVia atlas TravelCost creep goals with
    | Some step -> Some step
    | None -> stepAcross atlas TravelCost creep task

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
///
/// Across a border it takes the same two-legged route firstStep takes and
/// chooses its crossing under its own pricing (#142), so a traveller that
/// detours to a different Seam because one exit's approach is crowded is
/// attributed to the surcharge like any other detour.
let firstStepIgnoringTraffic
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (goals: Set<Pos>)
    : Pos option =
    match firstStepVia atlas Baseline creep goals with
    | Some step -> Some step
    | None -> stepAcross atlas Baseline creep task

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
///
/// A `Pos` carries no room (ADR 0041), so both rooms ride on the API, and
/// an outpost's container is priced across the border rather than walked
/// over home terrain (ADR 0042): each leg is then `joinedAcross`, the same
/// minimum over the same Seam band the Matcher's ranking price and the
/// mover's walk are read off, and never a second cross-room arithmetic of
/// this rule's own (ADR 0030). Same room, the flood and the tiles are the
/// ones this rule always ran, byte for byte.
///
/// **Two crossings and not one**, because the two legs are two journeys
/// (ADR 0029): the loaded factor and the empty one price a swamp exit
/// differently, and a body heavy enough to pay five ticks a swamp tile
/// loaded and one empty can be right to cross at one Seam full and
/// another empty. Each leg therefore runs both of its own floods under its
/// own factor and takes its own minimum over the whole band; the two share
/// nothing but the band itself.
///
/// Both legs are flooded out of the container and in to the sink, and the
/// leg *back* is the same direction priced on the empty body — the rule
/// ADR 0012 has always had, kept verbatim here. Reversing the return leg
/// would price one round trip by two joins: the exit charged would be the
/// sink room's rather than the container room's, so the same haul over the
/// same tiles would answer two numbers depending on which way it was read.
///
/// It runs its own floods rather than the tick's memoised ones, exactly as
/// it always has — its origins are containers and its goals a sink's
/// approach, so it shares a key with nothing here — and it is itself
/// memoised where it counts, on the census signature that gates the hauler
/// quota (ADR 0017). A container in a second room costs this rule two more
/// floods on the ticks the census moves and none on any other.
let haulRoundTripTicks
    (atlas: Atlas)
    (body: BodyPart list)
    (fromRoom: string)
    (from: Pos)
    (sinkRoom: string)
    (sink: Pos)
    : int option =
    let count part =
        body |> List.filter ((=) part) |> List.length

    let goals = adjacentWalkableIn atlas sinkRoom sink
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

/// A cast walk carried across a Seam and on into every tile of the far
/// room at once: the answer `joinedAcross` gives for one goal, given for
/// all of them by one flood (#169). The three terms are the same three,
/// in the same order and charged to the same tiles — walk to a tile beside
/// an exit, the exit's own price, walk in from the tile it lands on — but
/// read forwards rather than summed backwards. Every tile the far room
/// puts a creep down on is *seeded* at what it costs to arrive standing on
/// it, the cheapest crossing that reaches it; flooding on from there
/// charges each further tile once, so what comes back at a tile `g` is the
/// whole lead to `g`, and it is the number the per-goal join answered,
/// tile for tile.
///
/// Why this shape and not the join: a lead's far leg is flooded out of the
/// *goal*, so the join pays one flood per goal tile — and `expiring` asks
/// for a lead per creep, twice a tick, over a goal that only moves when a
/// creep does. Seeded from the band instead, the flood no longer depends
/// on the goal at all, which is what lets the whole answer go in the walk
/// table under the census (`castWalkTicks`, ADR 0032). The minimum a join
/// takes over `(exit, landing tile)` pairs the flood takes over its seeds,
/// and it is the same minimum over the same pairs: a seed keeps the
/// cheapest arrival offered it (`floodFromAllSeeded`), and every pair the
/// band admits offers one.
///
/// The near leg is the colony's own room's, always: a spawner stands at
/// home, so `fromRoom` here is `atlas.Home` and never the caller's.
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
///
/// The spawner floods its own room and the *goal's* room is the caller's
/// (#153), because a row's creeps do not all live at home: an outpost's
/// Post hires its Anchor off the home row (ADR 0042) and a reserver's
/// whole life is the far side of a Seam, so a lead that could only price
/// home tiles left ADR 0026's succession switched off for exactly those
/// creeps — never expiring, replaced only once dead. A goal in the
/// colony's own room prices as it always did, byte for byte: the same
/// flood, the same memo entry, the same lookup. A goal across a border is
/// the minimum over the Seam band — the one join the Matcher's ranking
/// price, the mover's step and the hauler quota's round trip are all read
/// off, never a second cross-room arithmetic of this rule's own (ADR
/// 0030), and read here through `castAcross` rather than `joinedAcross`
/// for the reason written there. Two rooms with no band between them lead
/// nobody, which is the answer an unreachable tile inside one room already
/// gets.
///
/// **Both legs enter the memo**, under the room the goal stands in
/// (#169). ADR 0032's condition is that every input of an entry is in the
/// census signature, and the far leg meets it exactly as the near leg
/// does: an outpost's ground needs no vision at all
/// (`Snapshot.projectRoom` reads the engine's terrain for any room in the
/// world, ADR 0031, ADR 0041), the roads and obstacles vision does pay
/// for are signed per projected room exactly as the home room's are —
/// standing structures and obstacle-kind construction sites alike, the
/// pending half of `censusSignature` having widened to every projected
/// room for this entry's sake (#169) — the
/// Seam band is border terrain and never moves, and *which* rooms are
/// projected is signed too — so a room the stand-down gate withdraws
/// (ADR 0043) moves the signature and drops this table whole, rather than
/// leaving behind the answer it had while the room was still worked
/// (`censusSignature`). What stood in the way was never staleness but the
/// key: the far leg's answers are another room's, filed against a bare
/// `Pos` the home room holds too, so before the room joined the key an
/// entry for them would have collided with a home walk (#120's forward
/// warning). With the room in it, an outpost lead costs one flood per
/// census instead of one per ask — and `expiring` asks twice per creep
/// per tick, for five outpost creeps, which is what put this rule 23% of
/// a quiet tick.
///
/// The band is still read before anything is flooded, and before anything
/// is written: a pair of rooms with no crossing has no flood to pay for
/// (`pricedAcross`'s rule, for `pricedAcross`'s reason) and no answer to
/// remember either — the memo holds what a band answered, never that one
/// answered nothing.
let castWalkTicks
    (atlas: Atlas)
    (body: BodyPart list)
    (spawn: Pos)
    (goalRoom: string)
    (goal: Pos)
    : int option =
    let factor = emptyFactorOf body

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
                walkFloodFromAll (weightsOf atlas atlas.Home) factor (adjacentWalkable atlas spawn)

            dist)

    if goalRoom = atlas.Home then
        arrival (near ())
    else
        // Not `memoised`: a miss has to read the band first and answer
        // absent without writing anything, which that shape cannot do —
        // it fills every key it is asked with. The lookup comes first all
        // the same, so the band is walked once per census rather than
        // once per ask.
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
    // Raw terrain is the *price*, never what blocks: the trunk starts from
    // the ground grid — plain 2, swamp 10, wall -1, and no road discount,
    // which is the walking grid's one disqualifying difference — and then
    // takes the obstacle pass back off the layer, because a rampart or a
    // spawn standing in the line is as impassable to a planned road as a
    // wall (ADR 0011). Marked from `Obstacles` rather than inferred from
    // the walking grid's -1: the set is a few dozen tiles against the
    // grid's two and a half thousand, and it is the rule itself rather
    // than a reading of a grid that has already applied it.
    //
    // A copy off the grid and not a fresh pass over the terrain layer
    // (#177): the layer form iterated every tile of the room and tested
    // two `Set<Pos>` membership trees at each, three structural
    // comparisons a tile per trunk, and the Layout plans one trunk per
    // source per goal on every census tick.
    let weights = Array.copy (groundOf atlas atlas.Home)

    (layerOf atlas atlas.Home).Obstacles
    |> Set.iter (fun tile -> weights.[indexOf tile] <- -1)

    // Through the grid's guard, unlike the pass above: `avoid` is the
    // Layout's own reservation set rather than the projection's geometry,
    // and `indexOf` checks nothing (#173) — so a tile off the
    // fifty-by-fifty reserves nothing instead of indexing off the array.
    avoid
    |> Set.iter (fun tile ->
        if inGrid tile then
            weights.[indexOf tile] <- -1)

    let dist, parents =
        floodFrom weights noTraffic (stepTable (stepUnits planningFactor)) origin

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
