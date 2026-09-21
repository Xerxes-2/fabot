module Fabot.Core.Atlas

open Fabot.Core.Types
open Fabot.Core.Grid

/// The per-tick, task-aware query interface over the spatial projection.
/// Total: geometry the projection cannot place gets one documented answer per
/// query — it never counts against a Task and never blocks an action.
type Atlas =
    private
        {
            Spatial: SpatialInfo
            /// The room every query that names none of its own answers for: the
            /// projection's `RoomName`, empty when it names none.
            Home: string
            /// The colony's tunables, carried off the view this Atlas was laid from —
            /// here rather than an argument to `trunkPath`, its one reader, because a
            /// number threaded through each ask is one two call sites can disagree
            /// about.
            Tuning: Tuning
            /// Placed creeps in view order — the canonical iteration
            /// order for everything derived per creep — each beside the
            /// room the projection files it under, because the flood it
            /// seeds is that room's.
            Placed: (string * string * Pos) list
            /// Each creep's fatigue factor — what turns terrain weight
            /// into travel cost for that body.
            Factors: Map<string, FatigueFactor>
            /// Creep name -> the room the projection files it under and the
            /// tile it stands on there, resolved once so a query costs one lookup.
            CreepAt: System.Collections.Generic.Dictionary<string, string * Pos>
            /// Target id -> the room the projection files it under and its tile
            /// there — the same join over the other id space, and the reason
            /// `TargetKinds` stays flat: an object id is already unique, so the
            /// kind census needs no room.
            TargetAt: System.Collections.Generic.Dictionary<string, string * Pos>
            /// The kind census read the other way round: kind -> the ids of that
            /// kind, in id order. Every census on this API asks "which ids are
            /// Towers", which used to mean walking the whole census per ask, a dozen
            /// asks a tick. Flat because an object id is unique across the world.
            KindIds: Map<TargetKind, string list>
            /// Step weight per tile index, per room name, laid once a tick for the
            /// flood's hot loop: -1 impassable, else the price of stepping onto the
            /// tile — road 1, plain 2, swamp 10; walls, obstacle structures and tiles
            /// outside the projection impassable. The only form the rule has: the
            /// single-tile query reads this grid too (`weightAt`). The [[keeper
            /// margin]] is already in the ground it is copied from, so a masked tile
            /// is impassable here without a pass of its own (`Keepers`).
            Weights: Map<string, int[]>
            /// Raw terrain weight per tile index, per room name: the ground before a
            /// road discounts it and before an obstacle blocks it. A grid of its own
            /// because a Seat is counted by terrain alone — a structure on a source's
            /// neighbour does not consume the Seat — and the Layout's ground readers
            /// price off it too: a site's tile is terrain holding nothing, a swamp
            /// under a road is still swamp, and a trunk is priced before any road
            /// discount. Raw in every sense but one: the [[keeper margin]] is masked
            /// out of it (`Keepers`).
            Ground: Map<string, int[]>
            /// Terrain weight per tile index of each room's border ring — the exit
            /// rows and columns the layers' ground leaves out — and -1 everywhere
            /// else: the table form of `SpatialInfo.Borders`, for the Seam band and
            /// the crossing price. Never merged into the two grids above: a ring tile
            /// is one a creep passes through and never one it may stand on.
            Rings: Map<string, int[]>
            /// Whether a creep stands on each tile index this tick, per
            /// room name; the flood prices these tiles dearer so paths
            /// detour around standing traffic.
            Occupied: Map<string, bool[]>
            /// Memoised Dijkstra flood per placed creep's tile, fatigue factor and
            /// pricing, forced at most once per tick and shared by every query pricing
            /// from it. Bodies of the same factor at the same tile share one flood;
            /// one entry per pricing, laid lazily, so a tick that asks for one pays
            /// for one. Each is a seeded, unadvanced `Flood` that each reader pushes
            /// out only as far as the tile it asks about.
            Floods: Map<string, Map<Pos * FatigueFactor * Pricing, Lazy<Flood>>>
            /// Memoised flood *into* a Seam band — the walk out of every tile of one
            /// room onto the crossings joining it to a named neighbour. One flood per
            /// ordered room pair, however many tiles are read off it — and one per
            /// **census** rather than per tick, since the table is the plan memo's
            /// (`SeamWalkTable`): it is handed in with the far fields and recalled
            /// with them.
            SeamWalks: SeamWalkTable
            /// Memoised Seam band per ordered room pair — the tile pairs
            /// `seams` answers with, which every hop of every chain priced
            /// reads and which nothing about a creep or a Task can move
            /// (`docs/research/cpu-headroom.md` §5.4).
            Seams: System.Collections.Generic.Dictionary<string * string, (Pos * Pos) list>
            /// Memoised far leg per **chain** of rooms — `FarFloods` keyed by the route
            /// the walk crosses. One table whatever the pricing: the far leg floods over
            /// empty ground under every one of them, so every field is a function of the
            /// census (`FarFieldMemo`, `docs/research/cpu-headroom.md` §5.1). Handed
            /// back by `Decide.decideUnarbitrated` while the census signature stands.
            FarFields: FarFieldMemo
            /// Memoised room chains per ordered room pair — every chain of the fewest
            /// crossings a walk between them could take, ends included. Answered off
            /// the border rings and the raw ground behind them and off no walking grid
            /// at all, so it is settled before any flood is forced and a pair with no
            /// chain costs the tick one search and no grid. **Ordered**, and the order
            /// is load-bearing: a band asks the *far* room's ground, so the two
            /// entries of a swapped pair are two questions and may answer differently.
            /// The empty list is an answer and is memoised as one.
            Routes: System.Collections.Generic.Dictionary<string * string, string list list>
            /// Memoised traffic-blind cast walk out of a spawner's tile, per (spawner
            /// tile, fatigue factor, goal's room), for bodies the view does not carry:
            /// a lead prices a replacement not yet cast, whose factor is in no creep's
            /// entry.
            Walks: WalkTable
            /// The far fields whose origins are the decision layer's per-tick
            /// judgement rather than the census's — a Guard's ring, a Flee set — held
            /// for the tick and dropped with the Atlas. A caller narrows the tiles off
            /// `Threats`, which move every tick: filed beside the census's fields they
            /// would mint a key a tick and nothing would evict it. Same shape and same
            /// keying as `FarFields.PerCensus`, so the two can never answer
            /// differently for one key; only the lifetime differs.
            TickFarFields: FarFieldTable
            /// Work Area per Task, built at most once per tick and shared by every
            /// query that stands a creep in one — a mutable table because the key set
            /// is one the view does not carry; the Atlas is rebuilt every tick, so it
            /// is per-tick by construction. Each entry holds the area in both shapes
            /// from one write: the room with that room's own grid tiles, and the same
            /// tiles joined to it, because what leaves the Atlas carries its room and
            /// what stays inside indexes one room's grid.
            WorkAreas:
                System.Collections.Generic.Dictionary<
                    Task,
                    (string * Set<Pos>) option * Set<RoomPos>
                 >
            /// The Work-heavy variant of the same table: the narrowed area per Task,
            /// built at most once per tick. Only Harvest narrows, so `posts` is
            /// derived once per source.
            HeavyAreas: System.Collections.Generic.Dictionary<Task, Set<RoomPos>>
            /// The [[post]] census per room — the standing Posts with their sites
            /// counted in — built at most once per tick. A pure function of this
            /// tick's grids and kind census, and asked per rock, per candidate and
            /// per row: `postsOf` alone was 5.8% of a `pair --level 7` tick (#370,
            /// 2026-09-18), four set intersections over a hundred tiles, re-derived
            /// on every ask. Keyed by room name.
            Posts: System.Collections.Generic.Dictionary<string, Set<Pos>>
            /// The standing half of the census above: same key, same lifetime,
            /// its own table because `standingPostsOf` asks for it alone.
            StandingPosts: System.Collections.Generic.Dictionary<string, Set<Pos>>
            /// The union of every source's Seats in a room, which both of the
            /// above are cut from and the [[working ground]] reads on its own.
            SeatUnions: System.Collections.Generic.Dictionary<string, Set<Pos>>
            /// The creeps whose bodies carry more Work parts than Move, read from the
            /// body and never a name. Three readers ask it, so the arithmetic lives
            /// here once.
            Heavy: Set<string>
            /// Memoised controller-container census, built at most once per tick: the
            /// gate asks per creep and per candidate, and the answer is a colony
            /// fact. A key set of one, so a cell.
            mutable Buffers: Set<string> option
            /// The colony's [[refill cluster]] as the view spelled it
            /// (`RefillCluster.ofRefillables`): which structures are the flow's one
            /// sink, and how much room each has left. `None` for a colony whose
            /// Refillables hold no spawn to key a cluster.
            Cluster: RefillCluster option
            /// The ids of every structure this colony pours energy into — the
            /// cluster's spawn and extensions and the [[tower]]s beside them, the
            /// view's whole Refillable census rather than the ring the cluster keys
            /// (#277). **Ids** and not tiles, because the tile is a per-room question
            /// every census here answers the same way. Read off the view and never
            /// off the kind census: `FIND_STRUCTURES` carries every owner's, and an
            /// abandoned room's tower is a structure nothing of ours ever queues at.
            RefillableIds: Set<string>
        }

/// The Atlas over a view, recalling the tables the census keys rather than
/// laying empty ones: the spawn walk table, the Seam walks, and the far
/// fields beside them (`docs/research/cpu-headroom.md` §5.1). The caller
/// hands in the plan memo's tables while the census signature is unchanged,
/// and fresh ones when it moved: every entry in any of them is a pure
/// function of the census and of nothing else. Every other table is laid
/// empty: they key on this tick's creeps.
let ofViewRecalling (walks: WalkTable) (farFields: FarFieldMemo) (view: ColonyView) : Atlas =
    let spatial = view.Spatial

    // The home room, spelled the one way the convention is spelled
    // (`SpatialInfo.homeName`).
    let home = SpatialInfo.homeName spatial

    let tuning = view.Tuning

    // The two id-to-room joins, resolved once. An id is unique across the
    // world, so the layer that holds it is the room it is in — which is what
    // makes searching every layer the right answer here and the wrong one for
    // a query that starts from a bare `Pos`.
    // A string-keyed `Dictionary` and not a `Map`: an id is a string, which
    // Fable hashes into a native JS `Map` for nothing, where an F# `Map`
    // compares the string per tree level on every one of the dozens of asks a
    // candidate makes (#370's lead 1).
    let locate select =
        let found = System.Collections.Generic.Dictionary<string, string * Pos>()

        for KeyValue(room, layer) in spatial.Rooms do
            for KeyValue(id, pos) in (select layer: Map<string, Pos>) do
                found.[id] <- (room, pos)

        found

    let creepsFound = locate (fun (layer: RoomLayer) -> layer.CreepPositions)
    let targetsFound = locate (fun (layer: RoomLayer) -> layer.TargetPositions)

    // The kind census inverted, once. `Map.fold` walks the census in ascending
    // id order and each id is prepended, so reversing each bucket leaves the
    // ids in the id order every census on this API promises — the same order
    // the scan it replaces answered in.
    let kindIds =
        spatial.TargetKinds
        |> Map.fold
            (fun index id kind ->
                Map.add kind (id :: (Map.tryFind kind index |> Option.defaultValue [])) index)
            Map.empty
        |> Map.map (fun _ ids -> List.rev ids)

    let placed =
        view.Creeps
        |> List.choose (fun creep ->
            if creepsFound.ContainsKey creep.Name then
                let room, pos = creepsFound.[creep.Name]
                Some(creep.Name, room, pos)
            else
                None)

    let factors =
        view.Creeps
        |> List.map (fun creep -> creep.Name, fatigueFactorOf creep)
        |> Map.ofList

    // The room's grids, one set per projected room. These are the only form of
    // the rules — every single-tile query reads one of them (`weightAt`) — so
    // the precedence is spelled here and nowhere else: terrain, then roads over
    // the passable ground they discount, then obstacles over everything; the
    // initial -1 answers every tile outside the projection. The occupancy
    // surcharge marks **standing** traffic (#225) — a body that did not move
    // last tick, a fatigued one, and every body of another colony's — because
    // two travellers each pricing the other's tile never pass.
    let standing =
        view.Creeps
        |> List.filter (fun c -> not c.Moved || c.Fatigue > 0)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    // The [[keeper margin]], the one edit this colony makes to a fact about
    // the world before a rule reads it (`Keepers`). It goes on the **raw
    // ground**, ahead of the road and obstacle passes and ahead of the copy
    // the walking grid is made from, so a masked tile is impassable to every
    // query off either grid and the two cannot disagree about it. Asked in
    // bulk, because a room the declaration names none of yields nothing here
    // and costs the tick one lookup.
    let keeperMargin = Tuning.keeperMargin tuning

    let maskKeepers (room: string) (grid: int[]) =
        Keepers.maskedTilesIn keeperMargin room
        |> List.iter (fun tile -> grid.[indexOf tile] <- -1)

    let gridOf (room: string) (foreign: Set<Pos>) (layer: RoomLayer) =
        let ground = Array.create tileCount -1

        // The grid is strided exactly as this array is (`Geometry.indexOf`),
        // so the terrain walks straight in by index: no `Pos` is built and no
        // tree is walked, which is the whole of #278. The bounds guard lives
        // in `TerrainGrid`'s own entries, and a slot it holds is in range by
        // construction.
        layer.Terrain
        |> TerrainGrid.iterIndexed (fun index terrain -> ground.[index] <- terrainWeight terrain)

        maskKeepers room ground

        // The walking grid starts as the raw ground and takes the two
        // overriding passes; the ground itself keeps neither, because a
        // Seat is counted by terrain alone.
        let weights = Array.copy ground

        // A road discounts the ground under it, never ground the projection
        // calls impassable: a road on a wall (a tunnel, not modelled) or off
        // the terrain projection stays impassable.
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

        // The bodies this colony does not hold stand here too: the layer carries
        // only its own fleet, so a [[mother colony]]'s [[pioneer]] on the child's
        // [[anchor]] tile would price at nothing and the flood would send a
        // traveller into a creep it can never displace.
        foreign |> Set.iter (fun tile -> occupied.[indexOf tile] <- true)

        ground, weights, occupied

    // The border ring's own grid, keyed by the border layer's rooms rather than
    // by `Rooms`: a room with a ring and no ground, or ground and no ring, answers
    // -1 for the half it has not got. The shell never builds the first
    // (`World.ofGame` reads terrain and ring off one memoised read).
    //
    // The mask runs over the ring too: an exit tile within the margin of a rock
    // leaves the band, so a chain that existed over raw terrain may not exist
    // over masked terrain. `World.linked` masks the same tiles off the world's
    // own border maps, so the scan set and the price cannot disagree.
    let ringOf (room: string) (ring: Map<Pos, Terrain>) =
        let grid = Array.create tileCount -1

        ring
        |> Map.iter (fun tile terrain -> grid.[indexOf tile] <- terrainWeight terrain)

        maskKeepers room grid

        grid

    let grids =
        spatial.Rooms
        |> Map.map (fun room layer -> gridOf room (RoomPos.inRoom room view.Foreign) layer)

    let ground = grids |> Map.map (fun _ (bare, _, _) -> bare)
    let weights = grids |> Map.map (fun _ (_, grid, _) -> grid)
    let occupied = grids |> Map.map (fun _ (_, _, standing) -> standing)
    let rings = spatial.Borders |> Map.map ringOf

    {
        Spatial = spatial
        Home = home
        Tuning = tuning
        Placed = placed
        Factors = factors
        CreepAt = creepsFound
        TargetAt = targetsFound
        KindIds = kindIds
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
        SeamWalks = farFields.SeamWalks
        Seams = System.Collections.Generic.Dictionary()
        Routes = System.Collections.Generic.Dictionary()
        FarFields = farFields
        Walks = walks
        TickFarFields = FarFieldTable()
        WorkAreas = System.Collections.Generic.Dictionary()
        HeavyAreas = System.Collections.Generic.Dictionary()
        Posts = System.Collections.Generic.Dictionary()
        StandingPosts = System.Collections.Generic.Dictionary()
        SeatUnions = System.Collections.Generic.Dictionary()
        Heavy =
            view.Creeps
            |> List.filter (fun creep -> partCount creep.Body Work > partCount creep.Body Move)
            |> List.map (fun creep -> creep.Name)
            |> Set.ofList
        Buffers = None
        Cluster = RefillCluster.ofRefillables view.Refillables
        RefillableIds = view.Refillables |> List.map (fun r -> r.Id) |> Set.ofList
    }

/// The Atlas over a view with nothing recalled: fresh tables, filled from
/// scratch as this tick prices its leads and its crossings. The tick loop
/// always has a memo to hand over, so this is the shape a reader building
/// an Atlas over a view alone — a test, or a one-off — asks for.
let ofView (view: ColonyView) : Atlas =
    ofViewRecalling (WalkTable()) (FarFieldMemo.empty ()) view

/// One room's geometry: a room the projection carries no geometry for reads
/// as an empty layer — never the indexer, which throws on exactly that room.
/// The rule is `SpatialInfo.layerOf`'s.
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
/// on it, which is the empty band `seams` already answered with.
let private ringOf (atlas: Atlas) (room: string) : int[] =
    Map.tryFind room atlas.Rings |> Option.defaultValue noGround

/// One room's standing traffic, and no traffic at all for a room the
/// projection does not carry — which is what an empty room holds anyway.
let private occupiedOf (atlas: Atlas) (room: string) : bool[] =
    Map.tryFind room atlas.Occupied |> Option.defaultValue noTraffic

/// The room and tile a target id stands at, or None for an id the projection
/// does not place. `ContainsKey` then the indexer — three native `Map` probes
/// in Fable, the indexer re-checking membership before it reads, and still
/// none of the four allocations `TryGetValue` in a match costs (`memoised`'s
/// note).
let private targetAt (atlas: Atlas) (id: string) : (string * Pos) option =
    if atlas.TargetAt.ContainsKey id then
        Some atlas.TargetAt.[id]
    else
        None

/// The same read over the creeps' join.
let private creepAt (atlas: Atlas) (creep: string) : (string * Pos) option =
    if atlas.CreepAt.ContainsKey creep then
        Some atlas.CreepAt.[creep]
    else
        None

/// A copy of one room's step weight per tile index — the grid that room's
/// floods price from, -1 impassable. Read by the census guard and nothing
/// else: spawn walks are recalled on the census signature alone, so two views
/// the signature calls equal have to lay the same grid.
let stepWeights (atlas: Atlas) (room: string) : int[] = Array.copy (weightsOf atlas room)

/// Whether a creep's body was cast from a heavy-Work row: more Work parts than
/// Move.
let workHeavy (atlas: Atlas) (creep: string) : bool = Set.contains creep atlas.Heavy

/// A creep's fatigue factor; a creep the view does not carry prices
/// as a bare one-part-one-Move body — terrain weight verbatim.
let private factorOf (atlas: Atlas) (creep: string) : FatigueFactor =
    Map.tryFind creep atlas.Factors
    |> Option.defaultValue { FatigueParts = 1; MoveParts = 1 }

/// The memoised flood for a creep from a tile of one room, under one pricing;
/// placed creeps' own tiles hit the memo. The room is the caller's, and it is
/// always the room the creep stands in: a flood runs inside one room and stops
/// at its border.
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
/// movement is a room's and stays single-room, so the pass groups these by
/// `.Room`. The tiles carry their rooms, which is what makes a set of blocked
/// tiles safe to build across the list: keyed on a bare coordinate, two creeps
/// standing in two rooms would collapse into one occupant.
let placedCreeps (atlas: Atlas) : (string * RoomPos) list =
    atlas.Placed |> List.map (fun (name, room, pos) -> name, RoomPos.at room pos)

/// Name of the colony's own room — the entry of the layer that is home, which
/// the Layout gates on and stamps onto every site it places. None when the
/// projection names no room, which is a separate question from whether it
/// carries geometry.
let homeRoom (atlas: Atlas) : string option = atlas.Spatial.RoomName

/// Tile of a projected target (source, structure, site, controller) — in
/// whichever room the projection files that id under, since an id is unique
/// across the world. Room and tile in one, so no join can read one room's
/// coordinates as another's.
let positionOf (atlas: Atlas) (targetId: string) : RoomPos option =
    targetAt atlas targetId |> Option.map (fun (room, pos) -> RoomPos.at room pos)

/// Tiles a construction site may occupy in the colony's own room: non-Wall
/// terrain holding no projected target — anything standing or being built keeps
/// a site off a tile; creeps do not, and neither do the two transient kinds
/// (`isTransient`), because a tombstone stands wherever a creep died and the
/// Layout's ordering must not be a function of that. Deterministic (X, Y)
/// order, which is the grid's own flat index consed down from the last one,
/// so the list is built straight and never reversed. One room and no other: a
/// second room's tiles unioned in would offer the Layout a coordinate it does
/// not own.
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
/// the projection carries. The kind census is not layered: an object id is
/// unique across the world, and this answers ids, never tiles. Every reader
/// that turns these into tiles joins a room first.
let private targetsOfKind (atlas: Atlas) (kind: TargetKind) : string list =
    Map.tryFind kind atlas.KindIds |> Option.defaultValue []

/// The ids of one kind, in id order — `SpatialInfo.idsOfKind`'s answer read
/// off the census inverted once for the tick (`KindIds`) rather than walked
/// again per ask. A reader holding an Atlas asks here: the walk is a
/// `Map.toList` over the whole census per call, and the two pool readers that
/// asked it six times a tick were 2.3% of a `pair --level 7` tick.
let idsOfKind (atlas: Atlas) (kind: TargetKind) : string list = targetsOfKind atlas kind

/// The tile an id stands on **in the named room**: None for an id the
/// projection places in another room or does not place at all. A `Pos`
/// carries no room, so every census that unions tiles has to drop the other
/// rooms' before it unions.
let private tileIn (atlas: Atlas) (room: string) (id: string) : Pos option =
    match targetAt atlas id with
    | Some(where, tile) when where = room -> Some tile
    | _ -> None

/// One room's tiles stamped with their room, and empty for geometry the
/// projection places nowhere — the tail every `…In` census wears on its way
/// out of the Atlas, because a caller outside holds no room to stamp a bare
/// `Pos` with.
let private stamped (tiles: (string * Set<Pos>) option) : Set<RoomPos> =
    tiles
    |> Option.map (fun (room, grid) -> RoomPos.setAt room grid)
    |> Option.defaultValue Set.empty

/// Placed targets of one kind in one named room: id and tile, in id order.
/// The room is named rather than searched: its readers are the reflexes,
/// which measure a tile against a creep's, and a tile drawn from whichever
/// layer held the id would aim them at another room's coordinate.
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

// The two counts below are one half of the Layout's gap rule — `allowed at RCL
// - built - pending` — and the allowance is a fact about one room's
// controller, so the census subtracted from it has to be one room's too. One
// pair over the kind rather than a pair per kind, so a fourth clustered kind
// is a caller's argument rather than two more exports.

/// Structures of one kind already standing in the named room.
let builtIn (atlas: Atlas) (room: string) (kind: BuiltKind) : int =
    placedOfKindIn atlas room (Structure kind) |> List.length

/// Construction sites of one kind already placed in the named room.
let pendingIn (atlas: Atlas) (room: string) (kind: BuiltKind) : int =
    placedOfKindIn atlas room (Site kind) |> List.length

/// Towers standing in the colony's own room: id and tile, in id order — the
/// fire reflex's whole view of a tower: no store is projected, a dry tower's
/// shot simply fails at the engine. Home and no other room, because a tower
/// stands only in a room we own.
let placedTowers (atlas: Atlas) : (string * RoomPos) list =
    placedOfKindIn atlas atlas.Home (Structure BuiltKind.Tower)

/// Dropped **energy** piles one room's layer places: id and tile, in id order.
/// The pickup reflex's whole view of a pile — no amount is projected, since a
/// pile worth more than one carry is several trips, which is a Task's
/// arithmetic and not a reflex's.
///
/// Energy by the kind it asks for and not by accident (#311): the reflex runs
/// beside the pipeline and asks nothing of `applicable`, so a Thorium pile in
/// this census would be scooped by whatever body happened to stand beside it —
/// including one already carrying energy, which is a forbidden mixed load.
let droppedEnergyIn (atlas: Atlas) (room: string) : (string * RoomPos) list =
    placedOfKindIn atlas room (Dropped Energy)

/// Tiles holding a built road in the named room — the projection's road
/// census, one half of what the Layout's road gap subtracts. The room is the
/// caller's, like every placement census below, so no census answers for a
/// room the caller never named.
let roadTilesIn (atlas: Atlas) (room: string) : Set<Pos> = (layerOf atlas room).Roads

/// The tiles some ids stand on **in the named room**, unioned — the join
/// between the flat id censuses and one room's positions, written once because
/// every tile census below is that join with a different list of ids in front
/// of it. Ids the room does not place drop out, which is what keeps another
/// room's coordinates out of a `Set<Pos>` that has no room dimension.
let private tilesOfIdsIn (atlas: Atlas) (room: string) (ids: string list) : Set<Pos> =
    let layer = layerOf atlas room

    ids
    |> List.choose (fun id -> Map.tryFind id layer.TargetPositions)
    |> Set.ofList

/// Tiles of one room's placed targets whose kind answers a predicate, for the
/// censuses read as tiles rather than as counts. The room is named rather
/// than searched: a `Set<Pos>` has no room dimension, so two rooms' tiles
/// unioned would stand in neither room alone.
let private tilesWhereIn (atlas: Atlas) (room: string) (matches: TargetKind -> bool) : Set<Pos> =
    atlas.KindIds
    |> Map.toList
    |> List.filter (fun (kind, _) -> matches kind)
    |> List.collect snd
    |> tilesOfIdsIn atlas room

/// The same census over one room, by kind — asked of the census keyed by that
/// kind rather than by walking every bucket, which is the same answer and the
/// lookup `targetsOfKind` exists to be.
let private tilesOfKindIn (atlas: Atlas) (room: string) (kind: TargetKind) : Set<Pos> =
    targetsOfKind atlas kind |> tilesOfIdsIn atlas room

/// Tiles holding a road construction site — the census's other half: a
/// pending road is not yet a road but its tile needs no new site.
let pendingRoadTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Road)

/// Tiles of one room holding a built container — the container census's
/// standing half: a built container keeps a plan from re-dropping its site.
let private containerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Container)

/// Tiles of one room holding a container construction site — the census's
/// pending half: a pending container is not yet a container but its tile
/// needs no new site.
let pendingContainerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Container)

/// The container census in one room: the tiles a container stands on united
/// with the tiles one is pending on — the set every "must another container
/// be built?" question is asked against, at home and in an outpost alike.
let containerCensusIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.union (containerTilesIn atlas room) (pendingContainerTilesIn atlas room)

/// The extractor census of one room: the tiles an extractor stands on united
/// with the tiles one is pending on. Read exactly as the container census is:
/// the engine takes one extractor per room and refuses a second
/// `createConstructionSite`, so a plan that could not see the one already
/// there would ask once a tick for ever. Tiles and not a count because the
/// extractor's tile is its mineral's: the question the Layout asks is "is
/// this deposit's own tile taken".
let extractorCensusIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.union
        (tilesOfKindIn atlas room (Structure BuiltKind.Extractor))
        (tilesOfKindIn atlas room (Site BuiltKind.Extractor))

/// The Thorium minerals one room holds, each beside its tile, in id order.
/// Only Thorium is ever projected, the shell filtering `FIND_MINERALS` on
/// `mineralType`, so this is the whole of what the Layout plans an extractor
/// and a container for. A room the projection places none in answers with the
/// empty list.
let mineralsIn (atlas: Atlas) (room: string) : (string * Pos) list =
    targetsOfKind atlas Mineral
    |> List.choose (fun id -> tileIn atlas room id |> Option.map (fun tile -> id, tile))

/// Whether a target id is a Thorium deposit. The one join between a Task's
/// bare id and the fact that decides which set of rules answers for it: a
/// `Harvest` names a rock, and a rock is a source or a deposit, which share
/// the act and share almost nothing else — no regeneration, no store to
/// overflow into, and an extractor's cooldown between one dig and the next.
let isMineral (atlas: Atlas) (targetId: string) : bool =
    Map.tryFind targetId atlas.Spatial.TargetKinds = Some Mineral

/// The extractor standing on a deposit's own tile, whose cooldown decides
/// whether this tick's harvest is issued at all. The extractor's tile **is**
/// the mineral's, so the join is a tile equality inside the deposit's own room
/// and never a range. `None` while only a site stands there: a site extracts
/// nothing.
let extractorOn (atlas: Atlas) (mineralId: string) : string option =
    match targetAt atlas mineralId with
    | None -> None
    | Some(room, tile) ->
        targetsOfKind atlas (Structure BuiltKind.Extractor)
        |> List.tryFind (fun id -> tileIn atlas room id = Some tile)

/// Ticks before a structure may act again, and 0 for one the projection carries
/// no clock for — which is what "now" reads as.
let cooldownOf (atlas: Atlas) (targetId: string) : int =
    Map.tryFind targetId atlas.Spatial.Cooldowns |> Option.defaultValue 0

/// Tiles of one room a construction site cannot go down on today. The engine
/// takes one construction site per tile, so a pick onto an occupied tile is
/// answered ERR_INVALID_TARGET once a tick for as long as that site stands
/// (#244, live in W13S29). Two halves:
///
/// - **ours**, every kind but the container's: a container site on a Seat has
///   already answered "must another one be built?" in the target clause, and
///   answering it again here would turn a collision rule into a silent second
///   target rule.
/// - **everybody else's**, every kind including the container's (#248),
///   carried as a tile and no kind (`RoomLayer.RivalSites`): another player's
///   container serves no rock of ours, so it reaches the target clause as
///   nothing while refusing the tile like anything else.
///
/// A **built** structure is in neither half: a container site goes down on a
/// standing road perfectly well, and on an outpost [[seat]] a road is the best
/// tile there is.
let collidingSiteTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let ours =
        tilesWhereIn atlas room (function
            | Site BuiltKind.Container -> false
            | Site _ -> true
            | _ -> false)

    Set.union ours (layerOf atlas room).RivalSites

/// Tiles holding a built Storage — the tile a Link footing is anchored on
/// once the reservation has become a structure.
let storageTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Storage)

/// Tiles holding a Storage construction site — the same anchor while the
/// site is still being built.
let pendingStorageTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Storage)

/// Tiles holding a standing rampart — the covering census: a tile already
/// ramparted needs no rampart site. Ownership is not asked, unlike the hits:
/// a tile takes one rampart whoever raised it.
let rampartTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Rampart)

/// Tiles holding a standing rampart of ours — the same census asked with
/// ownership on. The projection carries hits for an ownable kind only when it
/// is ours, so the hits are what tell our rampart from one somebody else left
/// standing in a room we took: cover for our creeps is cover we own.
let ourRampartTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    targetsOfKind atlas (Structure BuiltKind.Rampart)
    |> List.filter (fun id -> Map.containsKey id atlas.Spatial.Hits)
    |> tilesOfIdsIn atlas room

/// Tiles holding a rampart construction site — the census's pending half,
/// exactly as a road's is: a site standing there is not yet cover, but its
/// tile needs no second site.
let pendingRampartTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Rampart)

/// Tiles holding a standing Keep structure — the spawn, the tower and the
/// Storage: what a rampart covers, the tick the structure stands. A site is
/// not covered until it is a structure.
let keepTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesWhereIn atlas room (function
        | Structure built -> isKeep built
        | _ -> false)

/// Tiles holding a standing link. A link is a target, so its tile is no
/// longer buildable; the Layout adds these back as footing candidates so
/// a footing does not jump the tick its link goes up.
let linkTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Structure BuiltKind.Link)

/// Whether a tile's terrain is swamp; a tile outside the projection is not.
/// Read off the raw ground grid and not the walking one: swamp is what the
/// terrain is, so a road laid over it must not answer plain.
let isSwampIn (atlas: Atlas) (room: string) (tile: Pos) : bool =
    weightAt (groundOf atlas room) tile = Engine.swampWeight

/// Walkable tiles adjacent to `pos` read as a tile of `room`, in deterministic
/// (X, Y) order. Standing respects obstacles, unlike Seat counting. The room
/// rides on the API, so a creep filed under an outpost is offered that room's
/// ground and never home's.
let adjacentWalkableIn (atlas: Atlas) (room: string) (pos: Pos) : Pos list =
    let weights = weightsOf atlas room
    neighbours pos |> List.filter (walkableAt weights)

/// Every tile of the room a creep may stand on — `adjacentWalkableIn`'s
/// answer over the whole room, off the same grid and so under the same
/// terrain, road and obstacle precedence. This is Flee's safe ground, and a
/// creep runs over the ground of the room it stands in.
let walkableTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let weights = weightsOf atlas room

    Set.ofList
        [
            for index in 0 .. tileCount - 1 do
                if at index weights >= 0 then
                    posAt index
        ]

/// The tile a creep stands on; None for a creep the projection does not
/// place — as `positionOf` is the same question about a target — and, like
/// it, room and tile in one.
let creepTile (atlas: Atlas) (creep: string) : RoomPos option =
    creepAt atlas creep |> Option.map (fun (room, pos) -> RoomPos.at room pos)

/// The room a creep stands in; None for a creep the projection does not
/// place. `creepTile`'s room alone, kept as a query of its own for the
/// readers that want only it — a Reach, a safe set, a grid or flood indexed
/// by that room.
let creepRoom (atlas: Atlas) (creep: string) : string option = creepAt atlas creep |> Option.map fst

/// The room the projection files a target under; None for one it does not
/// place. `positionOf`'s room alone, as `creepRoom` is `creepTile`'s: the
/// room a target's Work Area lies in, and so the room whose Reach is taken
/// out of that area (#138), and the room a spawn's doorstep is read in.
let targetRoom (atlas: Atlas) (targetId: string) : string option =
    targetAt atlas targetId |> Option.map fst

/// What a Task acts on, and the Chebyshev range its action reaches from
/// (Screeps: harvest, withdraw, transfer and reserveController at range 1;
/// build, repair and upgrade at range 3) — the one pair every geometry query
/// starts from. None for a Task the projection places nothing for: Flee has no
/// target and no action, and a Guard's target is a hostile creep, which is no
/// target of the projection's at all — its geometry is the colony's own
/// `Threats` and its act is the Emitter's, so every query below gives it
/// Flee's answer.
let private actionOn =
    function
    | Harvest id
    | Reserve id
    | Claim id
    // `claimReactor` is a Chebyshev-1 act like the two CLAIM acts beside it:
    // the engine checks `target.pos.isNearTo(this.pos)` and the processor
    // re-checks `|dx| <= 1 && |dy| <= 1` (`creep.claimReactor.js`). So the
    // [[work area]] is the reactor's own ring, which is nine plain tiles in
    // W15S25.
    | Reclaim id -> Some(id, 1)
    | Pickup(id, _)
    | Withdraw(id, _)
    | Refill(id, _) -> Some(id, 1)
    | Build id
    | Repair id
    | Upgrade id -> Some(id, 3)
    | Flee
    | Guard _ -> None

/// The colony's [[refill cluster]] as this tick's Atlas holds it — the one
/// `RefillCluster.ofRefillables` laid at construction. The Planner's Refill
/// and the pool's bound read it here rather than laying it again.
let cluster (atlas: Atlas) : RefillCluster option = atlas.Cluster

let private clusterOf (atlas: Atlas) (task: Task) : RefillCluster option =
    match task, atlas.Cluster with
    // The resource is not asked: a cluster is a ring of energy feeders and the
    // Thorium Refill's target is the [[storage]], so no Thorium Refill ever
    // names a cluster's spawn.
    | Refill(id, _), Some cluster when cluster.Spawn = id -> Some cluster
    | _ -> None

/// The tiles a Task's action is measured from, beside the room they stand in:
/// the target's own tile for every Task there is, and the **hungry** members'
/// tiles for a [[refill cluster]] — a body is in position when it stands
/// beside any structure of the cluster it can still pour into. The room is
/// the target's, and a member the projection places elsewhere or not at all
/// contributes no tile.
let private actionTilesOf (atlas: Atlas) (task: Task) : (string * Pos list) option =
    match actionOn task with
    | None -> None
    | Some(targetId, _) ->
        match targetAt atlas targetId with
        | None -> None
        | Some(room, target) ->
            match clusterOf atlas task with
            | None -> Some(room, [ target ])
            | Some cluster ->
                Some(room, RefillCluster.hungry cluster |> List.choose (tileIn atlas room))

/// Seat tiles of a placed source: walkable (non-wall) neighbours of its tile,
/// by terrain alone — structures and creeps do not consume Seats.
let private seatTiles (ground: int[]) (pos: Pos) : Set<Pos> =
    neighbours pos |> List.filter (walkableAt ground) |> Set.ofList

/// Seat tiles of a placed **rock** — the geometry behind `seats`, for the
/// Layout's source-container pick and for the mineral container's: a
/// deposit's Seats are its neighbours by the same terrain rule. Empty for a
/// target the projection does not place. The rock's own room answers, not
/// the colony's, so an outpost source's Seats are never a home tile of the
/// same coordinate.
let private seatTilesIn (atlas: Atlas) (rockId: string) : (string * Set<Pos>) option =
    targetAt atlas rockId
    |> Option.map (fun (room, pos) -> room, seatTiles (groundOf atlas room) pos)

let seatTilesOf (atlas: Atlas) (rockId: string) : Set<RoomPos> = seatTilesIn atlas rockId |> stamped

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    seatTilesIn atlas sourceId |> Option.map (snd >> Set.count)

/// The Work Area geometry behind `workArea`: the passable tiles within the
/// action's range of its target. Empty for a Task the projection cannot place a
/// target for — and for Flee, whose safe ground is a colony fact the decision
/// layer derives rather than geometry the projection carries.
let private buildWorkArea (atlas: Atlas) (task: Task) : (string * Set<Pos>) option =
    match actionOn task with
    | None -> None
    | Some(_, r) ->
        match actionTilesOf atlas task with
        | None -> None
        // The target's own room, resolved off its id: which ground an area is is
        // settled by where the target stands, never by which room the reader is
        // working in.
        | Some(room, targets) ->
            let weights = weightsOf atlas room

            Some(
                room,
                targets
                |> List.collect (tilesWithin r)
                |> List.filter (walkableAt weights)
                |> Set.ofList
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
    // `ContainsKey` then the indexer, and never `TryGetValue` in a match:
    // Fable compiles the out-parameter pattern into an `FSharpRef` wrapping a
    // getter and a setter closure plus the tuple the match destructures — four
    // allocations on the one read every memoised table in the Atlas makes,
    // against two native `Map` probes and none.
    if table.ContainsKey key then
        table.[key]
    else
        let value = build ()
        table.[key] <- value
        value

/// Work Area of a Task, body-blind: the passable tiles within the action's
/// range of its target. The base geometry `posts` is itself derived from, so it
/// stays a pure function of the Task; readers that hold a creep want
/// `workAreaFor`, which narrows it for a Work-heavy harvester. Empty when the
/// projection cannot place the target.
let private areaOf (atlas: Atlas) (task: Task) =
    memoised atlas.WorkAreas task (fun () ->
        let tiles = buildWorkArea atlas task

        tiles, stamped tiles)

/// The area as its room and that room's grid tiles: what every reader
/// *inside* the Atlas takes, so the join is never paid inside a per-creep
/// query. None for a Task the projection cannot place a target for, and
/// for Flee.
let private areaTilesOf (atlas: Atlas) (task: Task) : (string * Set<Pos>) option =
    fst (areaOf atlas task)

let workArea (atlas: Atlas) (task: Task) : Set<RoomPos> = snd (areaOf atlas task)

/// Every source of one room's Seat tiles, unioned — the seat half behind
/// `dualSeatsIn` and posts. Named room and not every layer: the union is
/// intersected with an Upgrade area below, and two rooms' Seats unioned would
/// meet it at a coordinate that is a Dual Seat in neither.
let private seatUnionIn (atlas: Atlas) (room: string) : Set<Pos> =
    memoised atlas.SeatUnions room (fun () ->
        let ground = groundOf atlas room

        targetsOfKind atlas Source
        |> List.choose (tileIn atlas room)
        |> List.map (seatTiles ground)
        |> List.fold Set.union Set.empty)

/// The **standable** ring of one room's projected sources, joined to that
/// room: the [[guard]]'s Work Area on the ticks it has no [[threat]] to ring
/// (#366) — the room is dark, so there is no Reach to derive one from, and
/// `Outpost.Sources` places a source's tile without vision. `Threats.ringIn`
/// takes over the tick vision returns.
///
/// Off the **weights** and not the raw ground, unlike `seatUnionIn`: this is
/// a set of tiles a body is asked to stand on, and a tile under an obstacle
/// is one it cannot.
let sourceRingIn (atlas: Atlas) (room: string) : Set<RoomPos> =
    let weights = weightsOf atlas room

    targetsOfKind atlas Source
    |> List.choose (tileIn atlas room)
    |> List.collect (fun pos -> neighbours pos |> List.filter (walkableAt weights))
    |> List.map (RoomPos.at room)
    |> Set.ofList

/// Every controller of one room's Upgrade Work Area, unioned — the tiles a
/// creep can upgrade from, behind `dualSeatsIn` and controllerContainers. One
/// room for the same reason the Seat union is one room's.
let private upgradeAreaIn (atlas: Atlas) (room: string) : Set<Pos> =
    targetsOfKind atlas Controller
    |> List.filter (tileIn atlas room >> Option.isSome)
    |> List.map (fun id ->
        match areaTilesOf atlas (Upgrade id) with
        | Some(_, tiles) -> tiles
        | None -> Set.empty)
    |> List.fold Set.union Set.empty

/// The ground a room's Thorium minerals hold: each deposit's own tile (the
/// extractor's) and every Seat of it (the miner's, and its container's).
/// Every Seat and not the one the plan picked: at a wall mouth there may be
/// only one accessible tile, and an extension landing on it would cost the
/// room its whole deposit.
///
/// Deliberately **not** folded into `seatUnionIn`: a mineral's Seat under a
/// container would otherwise count as a [[post]], hire an [[anchor]] and
/// enter the Anchor row's quota. Off `mineralsIn`, never a second census.
let private mineralGroundIn (atlas: Atlas) (room: string) : Set<Pos> =
    let ground = groundOf atlas room

    mineralsIn atlas room
    |> List.map (fun (_, tile) -> Set.add tile (seatTiles ground tile))
    |> List.fold Set.union Set.empty

/// The working ground of the room: every projected source's Seats, every
/// projected Thorium mineral's tile and Seats, plus, at home only, the
/// controller's Upgrade Work Area — off-limits to the Layout's clustered
/// ordering. The Upgrade half is home's alone because the colony *reserves*
/// an outpost's controller and upgrades nobody's but its own (#241). This is
/// the **Layout's** question: the mover asks `idleGroundIn`, a strictly wider
/// set (#268), and widening this one instead would move every clustered pick.
let workingGroundIn (atlas: Atlas) (room: string) : Set<Pos> =
    let mined = mineralGroundIn atlas room

    if room = atlas.Home then
        Set.unionMany [ seatUnionIn atlas room; upgradeAreaIn atlas room; mined ]
    else
        Set.union (seatUnionIn atlas room) mined

/// The tiles of one room's **stores**: a built [[container]], the
/// [[storage]], and every structure the colony pours a [[refill]] into,
/// **tower** included (#277 correcting #268): a hauler holding a tower's
/// Refill queues on its range-1 ring as the Storage's does, so a wall-tucked
/// tower jams the same way. Read off the view's Refillable census
/// (`atlas.RefillableIds`), never the kind census, which carries every
/// owner's. A construction *site* is not a store.
let private storeTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let refillableTiles =
        atlas.RefillableIds
        |> Set.toList
        |> List.choose (tileIn atlas room)
        |> Set.ofList

    Set.unionMany [ containerTilesIn atlas room; storageTilesIn atlas room; refillableTiles ]

/// The [[idle ground]] of the room (#268): the working ground plus the
/// walkable range-1 ring of every store. The working ground is "where work
/// happens"; this is "where standing idle blocks somebody", a fact about
/// traffic and strictly larger: a [[storage]] against a wall has two standing
/// tiles, and two idle bodies on them shut the hauler holding its [[refill]]
/// out, as W13S28's pocket did. Only the *ring* is added and never the
/// store's own tile: every store a body can stand on is already inside the
/// working ground by construction, and the rest are obstacles.
let idleGroundIn (atlas: Atlas) (room: string) : Set<Pos> =
    let rings =
        storeTilesIn atlas room
        |> Set.toList
        |> List.collect (adjacentWalkableIn atlas room)
        |> Set.ofList

    Set.union (workingGroundIn atlas room) rings

/// Dual Seats of the room: tiles inside both some projected source's Seats and
/// a projected controller's Upgrade Work Area — a creep standing on one
/// harvests and upgrades without ever moving. No controller, no sources, or a
/// disjoint pair answers with the empty set.
let dualSeatsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (seatUnionIn atlas room) (upgradeAreaIn atlas room)

/// Whether a creep stands on a Dual Seat: the one tile where a heavy body has a
/// second thing to do without moving. An unplaced creep stands on nothing.
let standsOnDualSeat (atlas: Atlas) (creep: string) : bool =
    match creepAt atlas creep with
    | Some(room, tile) when room = atlas.Home -> Set.contains tile (dualSeatsIn atlas room)
    | _ -> false

/// The **standing** half of the Post census: the Dual Seats plus every Seat
/// under a built container, which by the Layout's geometry is a source
/// container. The Dual Seat half is the colony's own room's alone and only the
/// container half crosses a border, because a Dual Seat is a tile a creep
/// harvests *and upgrades* from and the colony upgrades its own controller:
/// counted in an outpost it would name an income share for a source with no
/// container under it. Separated from `postsIn` along the split between what
/// a room is *worth* and what it is *worked* from: this is the switch that
/// admits a source into the quotas, and a site throws none.
let private standingPostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    memoised atlas.StandingPosts room (fun () ->
        let containerPosts =
            Set.intersect (seatUnionIn atlas room) (containerTilesIn atlas room)

        if room = atlas.Home then
            Set.union containerPosts (dualSeatsIn atlas room)
        else
            containerPosts)

/// Seats carrying a container **construction site** — the Post a heavy body is
/// hired for before the container it will dig into exists. An Anchor digs
/// twelve a tick and spends it into the site under its own feet, where without
/// it the worker row commutes a Seam apart at fifty energy a trip. Read off
/// the Seats and never off the site's range: a site a step off this source's
/// Seats belongs to whatever source seats *it*.
let private containerSitePostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (seatUnionIn atlas room) (pendingContainerTilesIn atlas room)

/// ADR-0012
/// Posts of the room: the tiles worth garrisoning with a heavy-WORK body — the
/// standing census above, plus the Seats carrying a container site. The
/// capacity unit of the Anchor quota and of Harvest's own concurrency, and the
/// only footing a Work-heavy body harvests from. Room-local and derived fresh
/// each tick.
let postsIn (atlas: Atlas) (room: string) : Set<Pos> =
    memoised atlas.Posts room (fun () ->
        Set.union (standingPostsIn atlas room) (containerSitePostsIn atlas room))

/// Every projected room's Posts, counted: the Anchor row's quota. An outpost's
/// Post is the same garrison tile a home Post is and hires the same row.
/// Counted room by room and summed, never unioned: a `Pos` carries no room, so
/// two rooms whose Posts share a coordinate are two garrison tiles a border
/// apart. A Post is a vision fact through the *container* and its site, never
/// through the layer — a declared outpost always carries one, and reading
/// absence onto the declaration is a deadlock — so a blind outpost's Seat
/// hires no Anchor, and a room leaves this fold only when the scan set drops
/// it.
let postCount (atlas: Atlas) : int =
    atlas.Spatial.Rooms
    |> Map.fold (fun total room _ -> total + Set.count (postsIn atlas room)) 0

/// Tiles holding a standing container on a Post — the tiles a work-heavy
/// body garrisons and cannot flee from, ramparted beside the Keep. A Post
/// that is a bare Dual Seat is not one of these: what the rule covers is a
/// structure standing. The room is the caller's.
let postContainerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    Set.intersect (containerTilesIn atlas room) (postsIn atlas room)

/// The **mine Posts** of one room: every Thorium deposit's Seat carrying a
/// built container — the tile the [[miner]] stands on, a store-less body
/// whose dig lands *in* the container under it, which is why the row has no
/// Carry.
///
/// Not folded into `standingPostsIn`: `postsIn` is the Anchor row's unit, a
/// deposit has no rate to saturate and hires no Anchor, and a mineral Seat
/// in that census would hire a six-Work body with a Carry part to stand on a
/// tile whose whole yield is Thorium. The **built** container alone, never
/// its site: a miner cannot raise one, Build being shut to a Work-heavy body.
let private minePostsIn (atlas: Atlas) (room: string) : Set<Pos> =
    let ground = groundOf atlas room

    let seats =
        mineralsIn atlas room
        |> List.map (fun (_, tile) -> seatTiles ground tile)
        |> List.fold Set.union Set.empty

    Set.intersect seats (containerTilesIn atlas room)

/// One source's own Seats that the named census counts as Posts — the join
/// both readings of "this rock's Posts" are made of, differing in nothing but
/// which census they intersect with.
let private postsOfBy
    (census: Atlas -> string -> Set<Pos>)
    (atlas: Atlas)
    (sourceId: string)
    : (string * Set<Pos>) option =
    seatTilesIn atlas sourceId
    |> Option.map (fun (room, seats) -> room, Set.intersect seats (census atlas room))

/// The Posts of one source: its own Seats that are Posts. Empty for a source
/// the projection does not place. The Seat join keeps a neighbouring
/// source's site out: a Post belongs to the rock it seats, not the rock it
/// is near.
///
/// Both the **number** Harvest's Post cap admits and the **tiles** it reads
/// its garrison off, so a Post can never be full as a number while reading
/// vacant as a tile.
///
/// **A deposit reads its own census**, settled here once rather than at each
/// of the five readers. Two of those — the Emitter's walk disjunct and
/// `hasUnmannedPost` behind it — are **unreachable** for a deposit, the
/// mineral arm standing in front of them, and are right by never being
/// asked; said out loud because that is where a later widening would land
/// silently.
let private postsOfIn (atlas: Atlas) (rockId: string) : (string * Set<Pos>) option =
    postsOfBy (if isMineral atlas rockId then minePostsIn else postsIn) atlas rockId

let postsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> = postsOfIn atlas sourceId |> stamped

/// The **standing** Posts of one source: `postsOf` above less the Seats whose
/// container is still a site — the switch that admits a source into the
/// quotas.
let standingPostsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> =
    postsOfBy standingPostsIn atlas sourceId |> stamped

/// The tile of a container construction site standing on a [[post]] — the one
/// site a body may build from under its own feet. `None` for a site of any
/// other kind, one the projection does not place, and one on a Seat no source
/// is served from (that is the controller's buffer, a delivery like any
/// other). `standsOnPostSite` below is the same fact asked of one creep,
/// written in terms of this one so the two can never part.
let postSiteTile (atlas: Atlas) (siteId: string) : RoomPos option =
    if Map.tryFind siteId atlas.Spatial.TargetKinds <> Some(Site BuiltKind.Container) then
        None
    else
        match targetAt atlas siteId with
        | Some(room, tile) when Set.contains tile (containerSitePostsIn atlas room) ->
            Some(RoomPos.at room tile)
        | _ -> None

let standsOnPostSite (atlas: Atlas) (creep: string) (siteId: string) : bool =
    match postSiteTile atlas siteId with
    | Some tile -> creepTile atlas creep = Some tile
    | None -> false

/// A creep and a target read as one room's geometry: the room they share, the
/// tile the creep stands on and the tile the target stands on. None when
/// either is unplaced or the two stand in different rooms — "one room or no
/// answer" written once, because every reader that joined the two maps itself
/// wrote this guard again.
let private together
    (atlas: Atlas)
    (creep: string)
    (targetId: string)
    : (string * Pos * Pos) option =
    match creepAt atlas creep, targetAt atlas targetId with
    | Some(creepRoom, tile), Some(targetRoom, target) when creepRoom = targetRoom ->
        Some(creepRoom, tile, target)
    | _ -> None

/// The mirror: the creep's room, the tile it stands on and the target's room,
/// once the two names have been read and found **different**. None for an
/// unplaced half as well, absence being no crossing.
let private apart
    (atlas: Atlas)
    (creep: string)
    (targetId: string)
    : (string * Pos * string) option =
    match creepAt atlas creep, targetAt atlas targetId with
    | Some(creepRoom, from), Some(targetRoom, _) when creepRoom <> targetRoom ->
        Some(creepRoom, from, targetRoom)
    | _ -> None

/// Whether a creep and a Task's target stand in one room — the question every
/// join between a creep and a target's geometry has to settle while no flood
/// leaves its room. Absence is permissive: a Task acting on nothing, an
/// unplaced creep and an unplaced target are each not a border crossing.
let private sharesRoom (atlas: Atlas) (creep: string) (task: Task) : bool =
    actionOn task
    |> Option.forall (fun (targetId, _) -> (apart atlas creep targetId).IsNone)

/// ADR-0020, ADR-0045, ADR-0051, ADR-0057
/// The body-aware Work Area, in the target's own room and blind to where the
/// creep stands. Harvest for a Work-heavy body is narrowed to that source's
/// Posts when it has any — even when the projection blocks them, so the Task
/// goes inapplicable rather than silently widening back to the Seats. A
/// source with no Post keeps the bare Seats at home and narrows to nothing
/// elsewhere; a deposit narrows to its own Post for either body (#261). Only
/// Harvest narrows. Memoised per Task.
let private narrowedArea (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
    match task with
    | Harvest rockId when workHeavy atlas creep ->
        memoised atlas.HeavyAreas task (fun () ->
            let postTiles = postsOf atlas rockId

            if not (Set.isEmpty postTiles) then
                Set.intersect (workArea atlas task) postTiles
            // A deposit keeps no fallback at all: a dropped Thorium pile bleeds
            // `ceil(amount / 1000)` a tick for the whole of the walk #311's Pickup
            // sends a hauler on, where the energy an Anchor drops waits for free, and
            // the only body that would take the widened area is an [[anchor]] that has
            // lost its own rock, which would then garrison a deposit with a Carry part
            // and age under the Thorium it holds.
            elif isMineral atlas rockId then
                Set.empty
            else
                // Absence is home's answer and not an outpost's: only a
                // source the projection places in another room loses the
                // fallback.
                match targetRoom atlas rockId with
                | Some room when room <> atlas.Home -> Set.empty
                | _ -> workArea atlas task)
    // A deposit narrows to nothing for a light body either (#261): its bare
    // Seats are exactly the tiles a dig drops the Thorium on the ground from.
    // The body this reaches is real: a `[16 Work; 4 Move]` miner loses Work
    // parts head-first to damage, and at `[3 Work; 4 Move]` it is no longer
    // `workHeavy`, still answers the Emitter's mineral arm, and would be
    // steered deliberately off the container it was standing on.
    | Harvest rockId when isMineral atlas rockId -> Set.empty
    // A Post's Seat is the garrison's: a light body's Harvest Work Area is the
    // source's Seats less its Posts — the complement of the heavy arm above, so
    // the two kinds of body stand on disjoint tiles of one source and a light
    // crowd cannot squat the tile the Anchor was hired for.
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

/// Work Area of a Task for one creep — `narrowedArea` once the rooms agree,
/// and empty for a creep a border away from the target: standing and acting
/// are in-room acts, so the action gate refuses rather than misleads. The
/// cross-room *price* is `pricedAcross`'s minimum over the Seam band; the
/// far room's tiles are `workAreaAcross`'s; and `firstStep` answers the near
/// side of the winning Seam when these tiles are empty.
let workAreaFor (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
    if not (sharesRoom atlas creep task) then
        Set.empty
    else
        narrowedArea atlas creep task

/// The same Work Area, narrowed for the same body, in the room the Task's own
/// target stands in — `workAreaFor` without the in-room gate. Its one reader
/// is the threat gate (#147): "is every tile this Task can be worked from
/// inside a [[reach]]" is asked of the **target's** room and not of the
/// creep's share of it, so asked through `workAreaFor` a creep a border away
/// is handed the empty set by construction and the answer comes back "no"
/// however hot that room is. A *reading* and never a licence — nothing stands
/// or acts on these tiles from another room, and no walk is priced over them.
let workAreaAcross (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
    narrowedArea atlas creep task

/// The controller's upgrade buffers, by id: built containers standing inside
/// a controller's Upgrade Work Area and on no source's Seat. The Planner
/// spells the same judgement out over the view for its Refill layering. No
/// controller, none placed or no built container answers with the empty set,
/// which opens the gate rather than closing it.
let controllerContainers (atlas: Atlas) : Set<string> =
    match atlas.Buffers with
    | Some memo -> memo
    | None ->
        let home = atlas.Home
        let area = upgradeAreaIn atlas home
        let seats = seatUnionIn atlas home

        // The colony's own room, and the container's tile is read out of that
        // room's layer rather than resolved off its id: a container standing on
        // the same coordinate of an outpost would otherwise test as standing in
        // this controller's Upgrade area.
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
/// judged from the same census `posts` reads. There the engine drops harvest
/// past a full store into the container under the creep, so a full store
/// never ends the dig. A site catches nothing, an unplaced creep or source
/// widens nothing, and the two have to stand in one room for the answer to
/// mean anything.
let catchesOverflow (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    together atlas creep sourceId
    |> Option.exists (fun (room, tile, _) ->
        Set.contains tile (containerTilesIn atlas room)
        && (match seatTilesIn atlas sourceId with
            | Some(_, seats) -> Set.contains tile seats
            | None -> false))

/// Whether a creep stands where it could dig a source: in that source's own
/// room and within the engine's harvest range of it. The widened half of
/// `catchesOverflow` above and deliberately weaker: that one asks whether the
/// tile catches a full store's overflow, while this asks only whether the
/// creep is in position the tick the energy lands. Measured by range rather
/// than by Seat membership, so a creep the engine has put on ground the
/// projection carries none for is in position all the same.
let standsAtSource (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    together atlas creep sourceId
    |> Option.exists (fun (_, tile, source) -> range tile source <= 1)

/// How the Atlas reads a border ring: off the grid it lays per room, where
/// `terrainWeight` has already given wall -1 and every other terrain a
/// positive weight. The other reading of the same question is the [[world]]'s
/// (`World.linked`), off the border map itself before any grid exists — one
/// `Seam` module answers both so they cannot disagree.
let private ringWalkable (atlas: Atlas) (room: string) : Pos -> bool =
    let ring = ringOf atlas room
    fun tile -> walkableAt ring tile

/// How the Atlas reads the **ground** a landing tile is left beside: off the
/// raw terrain grid, which is the room's terrain with the [[keeper margin]]
/// taken off it and nothing else. Deliberately not the walking grid: a band is
/// geometry, and a road laid or a rampart raised this tick must not change
/// which rooms are joined, or the scan set would move with the furniture. The
/// residual — a landing whose only ground neighbours are blocked by a
/// structure — is the price's to drop.
let private groundWalkable (atlas: Atlas) (room: string) : Pos -> bool =
    let ground = groundOf atlas room
    fun tile -> walkableAt ground tile

/// The Seam band joining two rooms: the passable exit-tile pairs, each this
/// room's border tile beside the tile it lands a creep on in the neighbour,
/// with ground of the neighbour's own beside the landing. Never a tile
/// anything offers to stand on: it comes off the border layer, which enters
/// no walking grid, walkable or buildable set and no Work Area, so the
/// Matcher cannot pick one and have the engine empty it the tick a creep
/// arrives. Deterministic (X, Y) order, total.
///
/// Memoised per ordered pair, the empty band included: twenty-two calls over
/// seven pairs in a `pair --level 7` tick (`docs/research/cpu-headroom.md`
/// §5.4). Per tick, though the band is terrain-constant: a table outliving
/// the tick has to be handed in and back, for about 1% of a tick.
let seams (atlas: Atlas) (fromRoom: string) (toRoom: string) : (Pos * Pos) list =
    memoised atlas.Seams (fromRoom, toRoom) (fun () ->
        Seam.bandBy
            (ringWalkable atlas fromRoom)
            (ringWalkable atlas toRoom)
            (groundWalkable atlas toRoom)
            fromRoom
            toRoom)

/// ADR-0058
/// Every chain of rooms a walk from one room to another could cross at the
/// fewest crossings, ends included, and empty where none lies inside the hop
/// budget: `RoomName.routesBy` over `seams`, out to `Tuning.MaxHops`. A room
/// the projection does not carry has no ring and is joined to nothing, so the
/// search stays inside the rooms `RoomName.transitBetween` put in the world.
/// Memoised per ordered pair, the empty answer included.
///
/// **Which chain is walked is not decided here** (#288): two chains of the
/// same hop length are not the same number of ticks — the corner an L-shaped
/// target is turned in is worth up to +91% on rooms this colony works — so
/// the readers that *have* a price (`joinedAlong`, `castWalkTicks`) keep the
/// cheapest. The search's order survives as the order of this list, so a tie
/// on the price falls where it always fell.
let routes (atlas: Atlas) (fromRoom: string) (toRoom: string) : string list list =
    memoised atlas.Routes (fromRoom, toRoom) (fun () ->
        RoomName.routesBy
            (fun here there ->
                Seam.joinedBy
                    (ringWalkable atlas here)
                    (ringWalkable atlas there)
                    (groundWalkable atlas there)
                    here
                    there)
            atlas.Tuning.MaxHops
            fromRoom
            toRoom)

/// The table with only the entries `keep` answers for, rebuilt in place
/// through `Clear` rather than removed entry by entry (#397): the bundled
/// `Dictionary.Remove` splices an entry out of its bucket and leaves the
/// emptied bucket in the hash map, on the order of 100 B per key hash ever held, where
/// `Clear` resets the map. Walked on a census move only, hundreds of ticks
/// apart, so the per-tick eviction's leftovers are swept here.
let private rebuilt (table: System.Collections.Generic.Dictionary<'k, 'v>) (keep: 'k -> bool) =
    let kept = ResizeArray()

    for KeyValue(key, value) in table do
        if keep key then
            kept.Add((key, value))

    if kept.Count < table.Count then
        table.Clear()

        for key, value in kept do
            table.[key] <- value

/// ADR-0032
/// Drop, from the three census-keyed tables this Atlas was handed, every
/// entry that read a room whose census moved — in place, because the tables
/// are the plan memo's. A spawn walk reads home and every room of every chain
/// `routes` answers to its goal; since `routes` answers a chain only while the
/// projection carries every room of it, a departed room drops every
/// cross-room spawn walk. A Seam walk reads its first room and is evicted on
/// either, over-invalidating being the cheap error. A far field reads exactly
/// its chain.
///
/// Keys are collected before anything is removed: a `Dictionary` may not be
/// mutated under its own enumeration.
let evictRooms (atlas: Atlas) (moved: Set<string>) : unit =
    let touches (rooms: string list) = rooms |> List.exists moved.Contains

    let departed =
        moved |> Set.exists (fun room -> not (Map.containsKey room atlas.Spatial.Rooms))

    rebuilt atlas.Walks (fun (_, _, goalRoom) ->
        if goalRoom = atlas.Home then
            not (moved.Contains atlas.Home)
        else
            not departed
            && not (
                touches (atlas.Home :: goalRoom :: List.concat (routes atlas atlas.Home goalRoom))
            ))

    rebuilt atlas.SeamWalks (fun (fromRoom, toRoom) -> not (touches [ fromRoom; toRoom ]))
    rebuilt atlas.FarFields.PerCensus (fun (chain, _, _, _, _, _) -> not (touches chain))

/// Drop every far field whose Task is not in `live` (#392): a Task carries an
/// object id, so a quiet census leaks one field per Task that ever priced a
/// far leg. A Task that comes back costs one re-flood. In place and not
/// `rebuilt`: this runs every tick, and the buckets it leaves are swept by
/// the next census move (#397).
let evictFarFieldsExcept (atlas: Atlas) (live: Set<Task>) : unit =
    let stale = ResizeArray()

    for KeyValue((_, task, _, _, _, _) as key, _) in atlas.FarFields.PerCensus do
        if not (Set.contains task live) then
            stale.Add key

    for key in stale do
        atlas.FarFields.PerCensus.Remove key |> ignore

/// The first of those chains, or `None` where there is none — what a reader
/// with no price to choose one with takes (`stepTowardRoom`, whose room is
/// dark and prices nothing, and which is therefore the one mover that can
/// contradict a price: #297), and the answer `route` gave before #288.
let route (atlas: Atlas) (fromRoom: string) (toRoom: string) : string list option =
    routes atlas fromRoom toRoom |> List.tryHead

/// Whether a creep stands on a Seam — its room's border ring, the tile the
/// engine put it down on the tick it crossed. Read off the coordinate alone.
let standsOnSeam (atlas: Atlas) (creep: string) : bool =
    match creepAt atlas creep with
    | Some(_, pos) -> pos.X = 0 || pos.X = Seam.exitEdge || pos.Y = 0 || pos.Y = Seam.exitEdge
    | None -> false

/// The tiles of a room's own ground next to one of its exit tiles — the only
/// tiles a flood can price a Seam's near side from, or step off its far side
/// onto, because the border ring is not ground and no flood ever enters it.
/// Diagonals included: the engine lets a creep step onto an exit diagonally,
/// and onto its first tile in the new room the same way. Clipped to the
/// room's *ground* and not merely to the grid: the answer is the same either
/// way, but a resumable flood asked about a tile nothing reaches settles the
/// whole room, and half of every exit's neighbourhood is more ring.
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
/// none — the one read every arrival at a set of tiles goes through.
/// Unreachable is an absence and never a number. The tile comes back beside
/// the price: of equally cheap goals the lowest tile wins, and `List.min`
/// over the pair settles that tie the same way everywhere. A second argmin
/// elsewhere agrees on every number and splits on every tie, which is how a
/// body comes to be walked toward one goal and ranked at another.
let private cheapestReached (reached: Pos -> int) (tiles: Pos list) : (int * Pos) option =
    tiles
    |> List.choose (fun tile ->
        let d = reached tile
        if d = unreached then None else Some(d, tile))
    |> function
        | [] -> None
        | reachable -> Some(List.min reachable)

/// The price alone, for the readers that arrive at a set and never name which
/// tile they arrived on.
let private nearestReached (reached: Pos -> int) (tiles: Pos list) : int option =
    cheapestReached reached tiles |> Option.map fst

/// What this body pays to step onto an exit tile, priced by the same rule every
/// other step is: one tick only for a plain exit under a body at fatigue
/// parity, and a swamp exit is not free. Read off the border ring, the only
/// terrain the projection has for an exit, and priced at the bare step, the
/// ring carrying no road to discount. None for an exit the projection has no
/// terrain for, a wall, or a body that cannot step at all.
let private exitPrice (atlas: Atlas) (stepPrices: int[]) room tile =
    let weight = weightAt (ringOf atlas room) tile

    if weight > 0 then
        let step = stepPrices.[weight]
        if step >= 0 then Some step else None
    else
        None

/// The body a *plan* is priced for: fatigue parity, one fatigue-generating part
/// to one Move, which under the walk's rounding is a tick on plain and five
/// on swamp.
let private planningFactor: FatigueFactor = { FatigueParts = 1; MoveParts = 1 }

/// The pricing a plan's walk is measured under: that body, on a walk, over
/// empty ground. A constant, and stated once because the flood that lays a
/// Seam walk and the reader that undoes its entry cost have to be measuring
/// the same walk.
let private planningWalk = pricingOf noTraffic planningFactor Walk

/// The walk out to a Seam, from every tile of one room's ground: the smallest,
/// over the whole band joining that room to the named neighbour, of the walk to
/// a tile beside a crossing plus the price of stepping onto the crossing
/// itself.
let private seamWalkFlood (atlas: Atlas) (fromRoom: string) (toRoom: string) : int[] =
    memoised atlas.SeamWalks (fromRoom, toRoom) (fun () ->
        let weights = weightsOf atlas fromRoom
        let stepPrices, traffic = planningWalk

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
/// it: it is the anchor the outpost container pick is made against, an
/// outpost having no spawn for a trunk to anchor on. `None` for two rooms
/// with no band between them, a room the projection carries no ground for, a
/// tile off the grid and a tile no crossing reaches — an unpriceable Seam is
/// no Seam, never a blocked one.
let seamWalkTicks (atlas: Atlas) (fromRoom: string) (toRoom: string) (from: Pos) : int option =
    if not (inGrid from) then
        None
    else
        let stepPrices, traffic = planningWalk
        let reached = (seamWalkFlood atlas fromRoom toRoom).[indexOf from]

        if reached = unreached then
            None
        else
            entryCost (weightsOf atlas fromRoom) traffic stepPrices from
            |> Option.map (fun own -> reached - own)

/// One crossing of a chain, named by the room it leaves. The Seam band a hop
/// is joined over is `seams From To`, so a pair's first tile is always
/// `From`'s and its second `To`'s — which is what lets a fold say only which
/// room its field is over and have every selector follow from that.
type private Hop = { From: string; To: string }

/// The chain as the hops it crosses, in the order a walk crosses them.
let private hopsAlong (chain: string list) : Hop list =
    chain
    |> List.pairwise
    |> List.map (fun (here, there) -> { From = here; To = there })

/// A cost field over one room's ground carried across a Seam onto the other
/// room's: every tile the crossing puts a creep beside, seeded at what it
/// costs to be standing there — `joinedAcross`'s three terms charged to the
/// same tiles.
///
/// Which way the hop is crossed is **read off `fieldRoom`**, never passed, so
/// one function serves a chain built backwards from a Task's ground and one
/// built forwards from a spawn. The exit's price belongs to `Hop.From` either
/// way: it is the tile stepped **onto**, and the landing costs nothing. The
/// seed room comes back beside the seeds so the fold cannot derive it twice.
///
/// A band is **directed** and this reader consumes it backwards too, which
/// looks like a bug and is not: the band keeps a pair only where `hop.To`'s
/// ground lies beside the landing, and a backward fold walks `To → From`,
/// which is exactly the ground it needs to reach the exit tile from. The
/// other end is `besideExit seedGround`, forwards or backwards. Both legs are
/// asked of both directions, so the band's direction adds and takes nothing.
let private carriedAcross
    (atlas: Atlas)
    (factor: FatigueFactor)
    (pricing: Pricing)
    (hop: Hop)
    (fieldRoom: string)
    (field: Pos -> int)
    : string * (Pos * int) list =
    let forwards = fieldRoom = hop.From
    let seedRoom = if forwards then hop.To else hop.From
    let fieldTileOf = if forwards then fst else snd
    let seedTileOf = if forwards then snd else fst
    let fieldGround = weightsOf atlas fieldRoom
    let seedGround = weightsOf atlas seedRoom

    // Empty ground, whatever the pricing: the far leg of a cross-room price
    // prices no standing crowd. The surcharge is the near leg's — the creep's
    // own flood, over the room it is walking now. Across a border the mover
    // reads only the exit the join won (`stepAcross` aims at that tile and at
    // nothing beyond it). The step table is the same table either way: it is a
    // function of the body and the pricing alone.
    let stepPrices, traffic = pricingOf noTraffic factor pricing

    let seeds =
        seams atlas hop.From hop.To
        |> List.collect (fun pair ->
            match
                exitPrice atlas stepPrices hop.From (fst pair),
                nearestReached field (besideExit fieldGround (fieldTileOf pair))
            with
            | Some crossing, Some onward ->
                besideExit seedGround (seedTileOf pair)
                |> List.choose (fun tile ->
                    entryCost seedGround traffic stepPrices tile
                    |> Option.map (fun entry -> tile, onward + crossing + entry))
            | _ -> [])

    seedRoom, seeds

/// A field carried the length of a chain, hop by hop: each crossing seeds the
/// next room's ground and a single-room flood settles it — `chainedInto`
/// hands it the hops reversed, `castAlong` in order. Three single-room floods
/// laid end to end, never a flood over three rooms. Unreachable stays an
/// absence throughout: a hop whose band is empty seeds nothing, and the join
/// gets `None` rather than a number with a gap in it.
let private foldChain
    (atlas: Atlas)
    (factor: FatigueFactor)
    (pricing: Pricing)
    (start: string * int[])
    (hops: Hop list)
    : int[] =
    hops
    |> List.fold
        (fun (fieldRoom, field) hop ->
            let seedRoom, seeds =
                carriedAcross atlas factor pricing hop fieldRoom (reachedIn field)

            // Empty ground, as `carriedAcross` above seeds it.
            let stepPrices, traffic = pricingOf noTraffic factor pricing

            let settled =
                floodFromAllSeeded (weightsOf atlas seedRoom) traffic stepPrices seeds
                |> drained
                |> fst

            seedRoom, settled)
        start
    |> snd

/// The far leg over a whole chain of rooms: today's flood into the Task's own
/// ground in the last room, carried **back** across each Seam of the chain in
/// turn. What comes back is a field over the **first** room of the chain,
/// which is the room `joinedAcross` joins the creep's own near leg to — so the
/// join does not know how many borders are behind the number.
///
/// A chain of one room is `floodPricedInto` and nothing else: there are no
/// hops, the fold runs zero times, and the array is the same array.
let private chainedInto
    (atlas: Atlas)
    (factor: FatigueFactor)
    (pricing: Pricing)
    (chain: string list)
    (origins: Pos list)
    : int[] =
    match List.rev chain with
    | [] -> Array.create tileCount unreached
    | last :: _ ->
        let target = floodPricedInto (weightsOf atlas last) factor pricing origins

        foldChain atlas factor pricing (last, target) (hopsAlong chain |> List.rev)

/// The same chain memoised colony-wide for one Task and one body, in the
/// **plan memo's** table under every pricing, so a chain is flooded once per
/// census rather than once per tick (`docs/research/cpu-headroom.md` §5.1).
///
/// The **pricing in the key is normalised**: `TravelCost` and `Baseline`
/// differ in traffic and in nothing else, and the far leg is traffic-blind,
/// so `Baseline` reads and files under the `TravelCost` entry. `Walk` keeps
/// its own: whole ticks a step is a different route, not a different number.
///
/// The **origins** are in the key: `pricedAcross` hands the Task's narrowed
/// area while `crossingToward` hands the caller's tiles, and under the old
/// key whichever flooded first answered for the other (#358).
///
/// The **table is the caller's**: origins the census signs go to
/// `atlas.FarFields.PerCensus`; origins narrowed off `Threats` move every
/// tick and would mint a new key a tick that nothing evicts, so
/// `crossingToward` files into `atlas.TickFarFields`, dropped with the Atlas.
///
/// **A chain is priced off its own suffix's field** (`cpu-headroom.md` §5.3):
/// two chains toward one target share a tail, so the recursion memoises the
/// suffix under its own key and a chain costs the one hop it adds. Since
/// `chainedInto` folds the hops reversed, this is that fold cut at a room
/// boundary, not an approximation of it. Measured bit-identical.
let rec private farFieldAlong
    (atlas: Atlas)
    (table: FarFieldTable)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (chain: string list)
    (origins: Pos list)
    : int[] =
    let factor = factorOf atlas creep

    // `Baseline` is `TravelCost` over empty ground: one entry, filed and read
    // under the ranking price's name — and **flooded** under it, below, so the
    // field a `Baseline` ask files is the very field a `TravelCost` ask would
    // have flooded and neither can depend on which of them asked first.
    let keyed =
        match pricing with
        | Baseline -> TravelCost
        | other -> other

    let key = chain, task, workHeavy atlas creep, factor, keyed, origins

    let flood () =
        match chain with
        | first :: (next :: _ as suffix) ->
            let onward = farFieldAlong atlas table keyed creep task suffix origins

            // The last step of `chainedInto`'s own fold, hop for hop: the
            // field over the suffix's first room, carried back over the one
            // crossing this chain adds, and settled across the room it lands
            // in.
            let seedRoom, seeds =
                carriedAcross atlas factor keyed { From = first; To = next } next (reachedIn onward)

            // Empty ground, as every other leg of the chain is.
            let stepPrices, traffic = pricingOf noTraffic factor keyed

            floodFromAllSeeded (weightsOf atlas seedRoom) traffic stepPrices seeds
            |> drained
            |> fst
        // A chain of one room has no suffix to share and no hop to carry:
        // the flood into the Task's own ground, which is what the fold
        // bottoms out at anyway.
        | _ -> chainedInto atlas factor keyed chain origins

    memoised table key flood

/// The near leg of a cross-room join, in the two shapes its callers hand it:
/// the tick's own per-creep flood, which the join may push further, and one
/// some caller already settled whole, which it may only read. Both answer
/// what a tile is finally reached at and what it cannot possibly beat, so the
/// join below is written once.
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
    match tiles with
    | [] -> unreached
    | _ ->
        let glimpse, frontier =
            match leg with
            | Drained dist -> reachedIn dist, unreached
            | Resuming flood -> glimpsedBy flood, frontierOf flood

        tiles |> List.fold (fun bound tile -> min bound (glimpse tile)) frontier

/// A cross-room price, joined on the Seam: the smallest, over the band, of
/// *walk to the exit tile* + *the exit tile's own price* + *walk in from the
/// tile it lands on*. Each leg is a single-room flood the caller already
/// ran, so the join is a minimum over thirty-odd additions and not floods.
/// The one join: `pricedAcross`, `haulRoundTripTicks` and `castAlong` all
/// fold these three terms. The far leg is run *into* its goals with each
/// goal seeded at its own entry cost (`floodPricedInto`); flooded the
/// ordinary way round it is short by a tile. **The convention**: a step
/// costs the tile it *lands on*, so exactly three things are charged beyond
/// the floods' interiors — the exit tile, the far room's first tile, and the
/// Work-Area tile — and the landing tile nothing. That is a tile cheaper than
/// the two rooms laid side by side, and real: crossing a border displaces a
/// creep twice for one move. The winning exit comes back beside the price,
/// so the mover aims at the Seam it was ranked on; the minimum is over
/// `(sum, exit)` pairs, so ties fall to the lowest (X, Y) exit.
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

/// A creep's cross-room price toward ground in another room: the join above
/// over this creep's own memoised flood and the far leg shared colony-wide.
/// The route is read before either flood is forced; the far ground is the
/// caller's (`farFieldAlong`).
///
/// The join is handed the **next** room of the chain and the band into it,
/// never the target and its own band — the one thing about `joinedAcross`'
/// contract a second call site can get wrong. The legs arrive as thunks:
/// neither may be flooded before there is a band, and the far leg is a fold
/// along the chain. The near leg is the creep's, not the chain's, so the same
/// thunk goes to every chain and buys one flood however many there are.
let private joinedOn
    (atlas: Atlas)
    (pricing: Pricing)
    (factor: FatigueFactor)
    (fromRoom: string)
    (from: Pos)
    (near: unit -> NearLeg)
    (far: string list -> Pos -> int)
    (chain: string list)
    : (int * Pos) option =
    match chain with
    | _ :: (next :: _ as onward) ->
        match seams atlas fromRoom next with
        | [] -> None
        | band -> joinedAcross atlas pricing factor fromRoom from next band (near ()) (far onward)
    | _ -> None

/// The same join over **every** shortest chain, keeping the cheapest `(sum,
/// exit)` pair (#288) — the minimum `joinedAcross` takes over one band, taken
/// once more over the chains. A one-hop pair is the call it always was, to
/// the digit and to the flood. Cost: one memoised far leg per chain, and per
/// creep one band scan per chain plus whatever relaxation a `Resuming` near
/// leg owes the second band; at `Tuning.MaxHops` = 3 that is at most three.
///
/// A reader that prices two journeys over one chain must not call this
/// twice: two independent minima sum to a trip no chain realises
/// (`haulRoundTripTicks`). `Walk` and `TravelCost` may win on different
/// chains — the surcharge's doing, and it lasts as long as the crowd does.
let private joinedAlong
    (atlas: Atlas)
    (pricing: Pricing)
    (factor: FatigueFactor)
    (fromRoom: string)
    (from: Pos)
    (toRoom: string)
    (near: unit -> NearLeg)
    (far: string list -> Pos -> int)
    : (int * Pos) option =
    // One flood for however many chains are priced. Every cross-room price every
    // creep asks for comes through here, so the wrapper per call was profiled
    // rather than assumed: at or under the harness's own run-to-run spread on
    // all five scenarios.
    let leg = lazy (near ())

    routes atlas fromRoom toRoom
    |> List.fold
        (fun best chain ->
            match best, joinedOn atlas pricing factor fromRoom from leg.Force far chain with
            | None, priced -> priced
            | best, None -> best
            // The smallest `(sum, exit)` pair, so a tie on the price falls to
            // the lowest exit tile exactly as it does inside one band — and a
            // chain that merely ties changes nothing the mover then walks.
            | Some won, Some other -> Some(min won other))
        None

/// The cross-room price toward an explicit set of origins, filed in the table
/// the caller says: the census-held one for origins the census signs, the
/// Atlas's own tick table for origins the decision layer narrowed
/// (`farFieldAlong`, which states which is which and why).
let private pricedAcrossInto
    (atlas: Atlas)
    (table: FarFieldTable)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (creepRoom: string)
    (from: Pos)
    (targetRoom: string)
    (origins: Pos list)
    : (int * Pos) option =
    joinedAlong
        atlas
        pricing
        (factorOf atlas creep)
        creepRoom
        from
        targetRoom
        (fun () -> Resuming(flood atlas pricing creepRoom creep from))
        (fun onward -> reachedIn (farFieldAlong atlas table pricing creep task onward origins))

/// The same price toward the ground a Task's own target names.
let private pricedAcross
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (creepRoom: string)
    (from: Pos)
    (targetRoom: string)
    : (int * Pos) option =
    pricedAcrossInto
        atlas
        atlas.FarFields.PerCensus
        pricing
        creep
        task
        creepRoom
        from
        targetRoom
        (narrowedArea atlas creep task |> RoomPos.tilesIn targetRoom)

/// The cheapest path from a creep to a set of tiles under one pricing — the
/// shape travel cost and the walk share, so the two can disagree on what a step
/// costs and on nothing else. The tiles are the caller's, not a Task's: what a
/// creep may stand on this tick is the decision layer's judgement, which takes
/// a Reach out of a Work Area and gives Flee an area of its own. A creep the
/// projection cannot place prices at 0; an empty or unreachable set has no
/// price at all.
let private pricedPathTo
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (area: Set<RoomPos>)
    : int option =
    match creepAt atlas creep with
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
/// two names have been read and found different. One spelling, because the
/// price (`pricedPath`) and the mover's step (`stepAcross`) must settle the
/// rooms alike. Absence is not a crossing — a Task acting on nothing, an
/// unplaced creep and an unplaced target each keep the permissive reading
/// `sharesRoom` gives.
let private borderCrossing
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    : (string * Pos * string) option =
    actionOn task |> Option.bind (fun (targetId, _) -> apart atlas creep targetId)

/// The crossing a Task asks for, priced: the creep's room and tile, and the
/// winning `(price, exit)` of the Seam band joining it to the target's room —
/// None for a Task that asks no crossing at all. One derivation, because the
/// price (`pricedPath`) and the mover's step (`stepAcross`) have to be reading
/// the same minimisation: a second one agrees on every number and splits on
/// every tie, which walks a creep to one crossing while ranking it at another.
/// That holds under each pricing a mover runs at, which is `TravelCost` and
/// `Baseline`; `Walk` has no mover, which is what lets `pricedOffField` read
/// its number off a field with no exit in it.
let private crossingFor
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    : (string * Pos * (int * Pos) option) option =
    borderCrossing atlas creep task
    |> Option.map (fun (creepRoom, from, targetRoom) ->
        creepRoom, from, pricedAcross atlas pricing creep task creepRoom from targetRoom)

/// The same, toward an explicit set of tiles that names its own room. None for
/// a creep already in that room, or an unplaced one: neither is crossing, and
/// both are answered by the in-room reading beside every caller.
///
/// The tiles are the caller's judgement and move with the tick — a Work Area
/// less a Reach, a Flee set — so the far fields this prices ride the Atlas's
/// own `TickFarFields` and never the census-held table (`farFieldAlong`).
let private crossingToward
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (room: string)
    (area: Set<RoomPos>)
    : (string * Pos * (int * Pos) option) option =
    match creepAt atlas creep with
    | Some(creepRoom, from) when creepRoom <> room ->
        Some(
            creepRoom,
            from,
            pricedAcrossInto
                atlas
                atlas.TickFarFields
                pricing
                creep
                task
                creepRoom
                from
                room
                (RoomPos.tilesIn room area)
        )
    | _ -> None

/// The cross-room **walk** — `walkTicks` across a border — read off the far
/// field carried one hop further, into the creep's own room, instead of
/// joined to a flood out of the creep (#171's second direction). The number
/// is the same by `joinedAcross`' convention: a field flooded *into* a goal
/// charges the tile it is read at (`floodPricedInto`) where a flood *out of*
/// the creep does not, so the creep's own standing cost comes off again
/// here. A field holds the sum and not the `(sum, exit)` pair, so this serves
/// no pricing a mover runs at; no `Walk` reader ever wanted the exit (#171's
/// goal-side direction is what is left).
///
/// Measured per `docs/profiling.md`: per-colony `decide` on `reactor --level
/// 7` 3.19–3.34 ms against 2.73–2.89 (−13%), `pair --level 7` −7%, `outpost`
/// −10%. The near leg it replaced was a flood out of the creep relaxed until
/// the band settled — most of the room, per creep per tick, 44% of every
/// heap pop on `reactor`. The field instead costs one whole-room flood per
/// (chain, Task, body, origins) on first ask, shared while the census stands;
/// a Task one creep ever prices over a census that moves every few ticks can
/// pay more.
///
/// One edge: a creep standing where the grid prices no step — the border
/// ring, a site dropped under it — has no standing cost to take off, and is
/// priced by the join, which seeds the flood's start whatever the ground says.
let private pricedOffField
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (creepRoom: string)
    (from: Pos)
    (targetRoom: string)
    : int option =
    let factor = factorOf atlas creep
    let stepPrices, traffic = pricingOf noTraffic factor Walk

    match entryCost (weightsOf atlas creepRoom) traffic stepPrices from with
    | None -> pricedAcross atlas Walk creep task creepRoom from targetRoom |> Option.map fst
    | Some standing ->
        let origins = narrowedArea atlas creep task |> RoomPos.tilesIn targetRoom

        routes atlas creepRoom targetRoom
        |> List.fold
            (fun best chain ->
                match
                    reachedIn
                        (farFieldAlong
                            atlas
                            atlas.FarFields.PerCensus
                            Walk
                            creep
                            task
                            chain
                            origins)
                        from
                with
                | d when d = unreached -> best
                | d ->
                    let price = d - standing

                    match best with
                    | Some won when won <= price -> best
                    | _ -> Some price)
            None

/// The same path priced for a Task: over the Task's own Work Area, and with the
/// one escape a bare tile set cannot carry — a target the projection does not
/// place prices at 0 rather than reading as unreachable geometry. A Task in an
/// unprojected room prices at 0 too: it never counts against the creep and,
/// having no Work Area, never lets it act. Across a border the crossing is
/// read once and priced per pricing: the two a mover runs at take the join's
/// `(sum, exit)` pair (`crossingFor`), and the walk takes the field
/// (`pricedOffField`).
let private pricedPath (atlas: Atlas) (pricing: Pricing) (creep: string) (task: Task) : int option =
    match actionOn task with
    | Some(targetId, _) when not (atlas.TargetAt.ContainsKey targetId) -> Some 0
    | _ ->
        match borderCrossing atlas creep task with
        | None -> pricedPathTo atlas pricing creep (workAreaFor atlas creep task)
        | Some(creepRoom, from, targetRoom) ->
            match pricing with
            | Walk -> pricedOffField atlas creep task creepRoom from targetRoom
            | TravelCost
            | Baseline ->
                pricedAcross atlas pricing creep task creepRoom from targetRoom
                |> Option.map fst

/// ADR-0002
/// Travel cost of a Task for a creep: the cost units — half-ticks — the
/// creep's body needs along a cheapest path to any Work Area tile, terrain
/// weights scaled by the body's fatigue factor and tiles under standing creeps
/// priced `occupancyPenalty` dearer; 0 for a creep already inside. None — a
/// placed Work Area the creep cannot reach, or an empty one — makes the Task
/// inapplicable to that creep. An unplaced creep or target prices at 0. A
/// ranking price and nothing else: halving it is not the walk.
let travelCost (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas TravelCost creep task

/// Travel cost to an explicit set of tiles: the same ranking price over the
/// area the caller hands in rather than the one the Task derives — what prices
/// a Task over the tiles the Reach left it, and Flee, whose Work Area is the
/// safe set and no target's surroundings. An unplaced creep prices at 0; with
/// no target there is no unplaced-target escape.
let travelCostWithin (atlas: Atlas) (creep: string) (area: Set<RoomPos>) : int option =
    pricedPathTo atlas TravelCost creep area

/// Travel cost to an explicit set of tiles that names **its own room** — the
/// one a Seam does not stop: `travelCostWithin` inside that room, and
/// `pricedAcross`'s minimum over the Seam band from outside it. It exists for
/// the [[guard]], whose Work Area is the colony's own `Threats` and whose
/// Task the projection places no target for, so `travelCost` would price it
/// unreachable to every body not already in the raided room. An unplaced
/// creep prices at 0; a room no Seam joins has no price at all.
let travelCostToward
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (room: string)
    (area: Set<RoomPos>)
    : int option =
    match crossingToward atlas TravelCost creep task room area with
    | Some(_, _, won) -> won |> Option.map fst
    | None -> pricedPathTo atlas TravelCost creep area

/// ADR-0029
/// The creep's walk to a Task's Work Area: the whole ticks its body needs
/// along a cheapest path, every step floored at one tick and today's standing
/// creeps priced at nothing — the horizon every time-aware judgement is made
/// at. Beside travel cost, not derived from it. 0 for a creep already inside
/// the area, and a missing walk reads as "no arrival".
let walkTicks (atlas: Atlas) (creep: string) (task: Task) : int option =
    pricedPath atlas Walk creep task

/// Whether a creep may perform its Task's action this tick: standing inside the
/// Task's Work Area for its body at tick start — a creep acts only from where
/// it may stand, which for a Work-heavy harvester is its Post, so the gate
/// keeps such a body empty on the way there and a full store never ends the
/// walk. Two permissive escapes keep the query total: a creep or target the
/// projection cannot place never blocks the action, and neither does a creep
/// standing on a tile the projection calls impassable — an obstacle-type site
/// dropped under it — judged by range instead.
let mayAct (atlas: Atlas) (creep: string) (task: Task) (area: Set<RoomPos>) : bool =
    match actionOn task with
    | None -> false
    // No action reaches across a border: the engine's ranges are measured
    // inside one room, and `range` takes two tiles of one grid. Asked here
    // rather than inferred from an empty area, which is what keeps the gate
    // shut while the mover walks a creep at the Seam: it opens by itself the
    // tick the engine puts the creep down.
    | Some _ when not (sharesRoom atlas creep task) -> false
    // The range escape is measured against every tile the action reaches
    // from, which for a [[refill cluster]] is its hungry members and for
    // every other Task is the one target it always was.
    | Some(_, actionRange) ->
        match creepAt atlas creep, actionTilesOf atlas task with
        | Some(creepRoom, creepPos), Some(_, targetTiles) ->
            if not (walkableAt (weightsOf atlas creepRoom) creepPos) then
                targetTiles |> List.exists (fun target -> range creepPos target <= actionRange)
            else
                Set.contains (RoomPos.at creepRoom creepPos) area
        | _ -> true

/// The structure a Refill's transfer actually names: its target, or for the
/// [[refill cluster]] the hungry member nearest the body, decided **here, at
/// arrival**, ties by id — the Planner names a place and the Emitter names
/// the structure, so an extension somebody else topped up while this body
/// walked costs it a neighbour and not its Task. Range-bounded by the
/// action's reach and total the way `mayAct` is: a body that could not be
/// placed gets the cluster's own hungry pick, the spawn first. `None` only
/// for a cluster with nothing left to pour into, which the Emitter reads as
/// the silence a drained Harvest keeps. The **resource** rides along because
/// the question is about the Task and not the id alone.
let refillTarget
    (atlas: Atlas)
    (creep: string)
    (structureId: string)
    (resource: Resource)
    : string option =
    let task = Refill(structureId, resource)

    match clusterOf atlas task with
    | None -> Some structureId
    | Some cluster ->
        let hungry = RefillCluster.hungry cluster

        let inReach =
            match actionOn task, creepAt atlas creep with
            | Some(_, actionRange), Some(creepRoom, creepPos) ->
                hungry
                |> List.choose (fun id ->
                    match targetAt atlas id with
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
    match creepAt atlas creep with
    | None -> None
    | Some(room, pos) ->
        // The creep's own room's share, for the reason `pricedPathTo` narrows
        // there — and read the same way, without building a second set: a step is
        // a step inside one room, and this room's grid is what the flood indexes.
        let goals = RoomPos.tilesIn room goalTiles

        if List.isEmpty goals || Set.contains (RoomPos.at room pos) goalTiles then
            None
        else
            let near = flood atlas pricing room creep pos

            cheapestReached (reachedBy near) goals
            |> Option.map (fun (_, goal) ->
                RoomPos.at room (posAt (firstStepOn near (indexOf pos) (indexOf goal))))

/// The step toward the near side of a crossing already won: the exit tile is
/// the creep's *own* room's border tile, so aiming at it asks nothing of the
/// neighbour and arbitrates nothing across the Seam, and the engine puts the
/// creep down in the neighbour at the end of the tick it steps on. The exit
/// is the one the price was minimised at, handed in rather than looked for
/// again: a second argmin agrees on every number and splits on every tie,
/// which walks a creep to one crossing while ranking it at another. Written
/// once because the two crossings below — a Task's target's, and a named
/// room's — differ in what they price and in nothing they walk. No winning
/// crossing, no step.
let private stepOnto
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (creepRoom: string)
    (from: Pos)
    (won: (int * Pos) option)
    : RoomPos option =
    won
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

/// The step a creep takes toward a Task whose target stands in another room:
/// the near side of the Seam `pricedAcross` paid at, taken out of that same
/// minimisation. No crossing, no step.
let private stepAcross
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    : RoomPos option =
    crossingFor atlas pricing creep task
    |> Option.bind (fun (creepRoom, from, won) -> stepOnto atlas pricing creep creepRoom from won)

/// The first step toward a set of goal tiles under one pricing: the in-room
/// step where the goals are reachable, and otherwise the near side of the Seam
/// the Task's own crossing was won at. The pricing is the whole of what the two
/// exports below differ in — the Resolver reads a detour off the pair, and a
/// pair that drifted apart in anything else would attribute one where none
/// was paid.
let private firstStepUnder
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (goals: Set<RoomPos>)
    : RoomPos option =
    firstStepVia atlas pricing creep goals
    |> Option.orElseWith (fun () -> stepAcross atlas pricing creep task)

/// The same crossing step toward an explicit set of tiles that names its own
/// room — `travelCostToward`'s mover, so the body walks the Seam it was priced
/// over and no second rule decides where a [[guard]] crosses. A creep already
/// in that room asks no crossing and is answered by the in-room step above it.
let private stepToward
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (room: string)
    (area: Set<RoomPos>)
    : RoomPos option =
    crossingToward atlas TravelCost creep task room area
    |> Option.bind (fun (creepRoom, from, won) ->
        stepOnto atlas TravelCost creep creepRoom from won)

/// The first step of a cheapest path from a creep to a set of goal tiles,
/// priced in the creep's own cost. The goals are the caller's: its Work Area
/// less the Reach and, for Flee, the safe set. None when the creep is
/// unplaced, already inside the goals, or they are empty or unreachable. Of
/// equally cheap goals the lowest (cost, tile) wins, matching the flood. A
/// creep on the border ring still gets a step, the flood seeding its start
/// tile regardless of weight. The Task rides beside the goals for the one
/// case they cannot carry: a target filed under another room leaves the
/// creep-aware Work Area empty, and the step is then toward the near side of
/// the winning Seam.
let firstStep (atlas: Atlas) (creep: string) (task: Task) (goals: Set<RoomPos>) : RoomPos option =
    firstStepUnder atlas TravelCost creep task goals

/// The same first step toward an explicit set of tiles, with no Task beside it:
/// `firstStep`'s answer for a body that has none to cross a Seam for, which is
/// what an idle one stepping off the [[idle ground]] is (#241, #268). The Task
/// buys the cross-room fallback and nothing else, so a caller whose goals are
/// tiles of the creep's own room by construction has no use for it.
let firstStepWithin (atlas: Atlas) (creep: string) (goals: Set<RoomPos>) : RoomPos option =
    firstStepVia atlas TravelCost creep goals

/// The step a creep takes toward a **room**, with nothing placed in it to aim
/// at: `stepAcross` without its far leg, the exit being the one this room's
/// own flood reaches cheapest. It reads the border layer and the memoised
/// terrain, neither of which waits for vision — written for the vision
/// grace's crossing creep (#151), whose target left the projection while the
/// border it is walking at stayed. **A crossing counts only where it lands
/// the body on ground it can walk off again** (#317): a mover with no far leg
/// to price is the one reader that can otherwise walk a body into a tile it
/// can never leave.
///
/// **It walks the compass's chain while the price walks the cheapest** (#288,
/// #297): `route` is the first chain of `routes`, so for a target reachable
/// two ways this mover can aim at the other border than the one the price
/// won, and which a creep obeys flips with its target room's vision.
/// `AtlasCrossRoomTests` pins the divergence so closing it turns a test red.
let stepTowardRoom (atlas: Atlas) (creep: string) (room: string) : RoomPos option =
    match creepAt atlas creep with
    | Some(creepRoom, from) when creepRoom <> room ->
        // The **next** room of the chain and not the goal: what a creep crossing
        // toward a room two hops out can aim at is the border it reaches first,
        // and the hop after that is the same question asked again from the room
        // it lands in.
        match route atlas creepRoom room |> Option.bind (List.tryItem 1) with
        | None -> None
        | Some next ->

            // The band already drops a crossing whose landing has no *terrain* beside
            // it — the [[keeper margin]]'s artefact #317 found here: a rock six tiles
            // inside a border masks the row behind an exit row it does not reach. What
            // the band must not carry is the rest: a landing whose only ground
            // neighbours are held by **structures** is still a crossing this mover
            // cannot use. `joinedAcross` drops it on the far leg; this mover has none,
            // its room being dark, so it asks the walking grid through
            // `Seam.landsOnGround`, and the two readings cannot drift on "beside".
            let farGround = weightsOf atlas next

            let landable =
                seams atlas creepRoom next
                |> List.filter (fun (_, landing) ->
                    Seam.landsOnGround (walkableAt farGround) landing)

            match landable with
            | [] -> None
            | band ->
                let ground = weightsOf atlas creepRoom
                let exits = band |> List.map fst

                // Standing beside a crossing already: step onto it, exactly as
                // `stepAcross` does for the exit it priced. The band's own (X, Y)
                // order settles a body standing beside two of them.
                let beside =
                    exits
                    |> List.tryFind (fun exit ->
                        List.contains from (besideExitFrom ground from exit))

                match beside with
                | Some exit -> Some(RoomPos.at creepRoom exit)
                | None ->
                    exits
                    |> List.collect (besideExit ground)
                    |> Set.ofList
                    |> RoomPos.setAt creepRoom
                    |> firstStepVia atlas TravelCost creep
    | _ -> None

/// The first step toward an explicit set of tiles that names its own room:
/// `firstStep`'s answer for a Task whose ground the projection places no target
/// for, and so the mover half of `travelCostToward`. In the room it is the
/// in-room step; from outside it is the near side of the Seam the price was
/// won at. It is what walks a [[guard]] out of the home room the row cast it
/// in and into the raided outpost.
let firstStepToward
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (room: string)
    (goals: Set<RoomPos>)
    : RoomPos option =
    firstStepVia atlas TravelCost creep goals
    |> Option.orElseWith (fun () -> stepToward atlas creep task room goals)

/// ADR-0018
/// The first step the same body would take were no tile occupied — the
/// traffic-blind route, otherwise priced exactly like `firstStep`. The Resolver
/// compares the two: a difference attributes the detour to the occupancy
/// surcharge, the only pricing the two floods do not share. Off the shared
/// memo under the Baseline pricing; the entry is lazy and the Resolver asks
/// only for creeps on the verbose list, so a tick that watches nobody floods
/// for nobody.
///
/// Across a border the two floods share the far leg outright — it prices no
/// crowd under either pricing, and the normalised key makes it the same array
/// — so a detour attributed here is the creep's **own** room's traffic alone.
let firstStepIgnoringTraffic
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (goals: Set<RoomPos>)
    : RoomPos option =
    firstStepUnder atlas Baseline creep task goals

/// Round-trip haul cost in whole ticks for a body between a container's tile
/// and a sink's: the leg out with every Carry loaded, the leg back empty,
/// both traffic-blind — the hauler quota is capacity planning, not routing,
/// and today's standing creeps must never resize the fleet. Goals are the
/// sink's adjacent walkable tiles. None when no goal is reachable. Across a
/// border each leg is `joinedAcross`, **two crossings and not one** because
/// the loaded and empty factors price a swamp exit differently, and both
/// flooded out of the container and in to the sink — reversing the leg back
/// would charge the sink room's exit rather than the container room's.
///
/// **One chain carries both legs** (#288): the cheapest is taken over the
/// round trip and never per leg, since two minima summed sit *below* every
/// chain's own trip — 113 against a real 114 on the pinned fixture, 147
/// against 182 with the corners further apart — and this is what the hauler
/// quota sums. A band is **directed**, so the leg back is walked over the
/// reverse bands and priced over the forward ones: an under-estimate one
/// border deeper, never `None` against a walk that exists, because
/// `Declaration.routable` admits a declaration only where a chain runs both
/// ways. This leg's far field is `chainedInto` direct, there being no creep to
/// key `farFieldAlong` on, so a second chain is a second chained flood per
/// leg, bounded by `Tuning.MaxHops` = 3 inside a census-keyed row.
let haulRoundTripTicks
    (atlas: Atlas)
    (body: BodyPart list)
    (container: RoomPos)
    (sink: RoomPos)
    : int option =
    let fromRoom = container.Room
    let sinkRoom = sink.Room
    let from = RoomPos.pos container
    let goals = adjacentWalkableIn atlas sinkRoom (RoomPos.pos sink)
    let weights = weightsOf atlas fromRoom

    let loadedFactor = loadedFactorOf body
    let emptyFactor = emptyFactorOf body

    if fromRoom = sinkRoom then
        let legTicks factor =
            let dist, _ = walkFloodFrom weights factor from
            nearestReached (reachedIn dist) goals

        match legTicks loadedFactor, legTicks emptyFactor with
        | Some out, Some back -> Some(out + back)
        | _ -> None
    else
        match routes atlas fromRoom sinkRoom with
        | [] -> None
        | chains ->
            // The chains first and the floods after, so a pair no chain joins still
            // costs the tick one search and no grid — and one flood per leg however
            // many chains there are, the flood out of the container being this body's
            // and not the chain's.
            let legOn factor =
                let near = Drained(fst (walkFloodFrom weights factor from))

                fun chain ->
                    joinedOn
                        atlas
                        Walk
                        factor
                        fromRoom
                        from
                        (fun () -> near)
                        (fun onward -> reachedIn (chainedInto atlas factor Walk onward goals))
                        chain
                    |> Option.map fst

            let out = legOn loadedFactor
            let back = legOn emptyFactor

            chains
            |> List.choose (fun chain ->
                match out chain, back chain with
                | Some loaded, Some empty -> Some(loaded + empty)
                | _ -> None)
            |> function
                | [] -> None
                | trips -> Some(List.min trips)

/// A cast walk carried across a Seam and on into every tile of the far room at
/// once: the answer `joinedAcross` gives for one goal, given for all of them by
/// one flood. The three terms are the same three, charged to the same tiles,
/// but read forwards rather than summed backwards — every tile the far room
/// puts a creep down on is *seeded* at what it costs to arrive standing on it,
/// so a tile `g` answers the whole lead to `g`. Why this shape and not the
/// join: a lead's far leg is flooded out of the *goal*, so the join pays one
/// flood per goal tile, and `expiring` asks for a lead per creep twice a tick.
/// Seeded from the band instead, the flood does not depend on the goal at all,
/// which is what lets the answer go in the walk table under the census.
/// Carried the length of the chain by the same fold the far leg is carried
/// back along; the walk table holds the last of the floods.
let private castAlong
    (atlas: Atlas)
    (factor: FatigueFactor)
    (near: int[])
    (chain: string list)
    : int[] =
    match chain with
    | []
    | [ _ ] -> Array.create tileCount unreached
    | first :: _ -> foldChain atlas factor Walk (first, near) (hopsAlong chain)

/// The table behind `castWalkTicks` and `walkTicksFrom`: every tile of
/// `goalRoom` priced for one fatigue factor from the free neighbours of one
/// home tile, memoised on the tile, the factor and the goal room, so two
/// bodies of one shape asking from one tile share it. That sharing is what
/// lets the delivery draw's gate (#373) price its leg from the Storage for
/// every candidate on every tick it is pooled: the key is the store's tile
/// and the body's *loaded* shape, neither of which moves with the candidate.
/// None when the goal room has no route from home; a tile with no free
/// neighbour answers a table nobody reaches, which every reader turns into
/// None. A goal across a border goes through `castAlong`.
let private castTable
    (atlas: Atlas)
    (factor: FatigueFactor)
    (from: Pos)
    (goalRoom: string)
    : int[] option =
    let near () =
        memoised atlas.Walks (from, factor, atlas.Home) (fun () ->
            let dist, _ =
                walkFloodFromAll
                    (weightsOf atlas atlas.Home)
                    factor
                    (adjacentWalkableIn atlas atlas.Home from)

            dist)

    if goalRoom = atlas.Home then
        Some(near ())
    else
        match atlas.Walks.TryGetValue((from, factor, goalRoom)) with
        | true, table -> Some table
        | _ ->
            match routes atlas atlas.Home goalRoom with
            | [] -> None
            | chains ->
                // The cheapest chain per **tile**, which is the same choice
                // the join makes and the shape a lead is answered in (#288):
                // one table holds every goal in the room at once, so the
                // minimum is taken elementwise rather than over one goal's
                // price. A single chain reduces to the table it always was,
                // untouched and uncopied.
                let table =
                    chains
                    |> List.map (castAlong atlas factor (near ()))
                    |> List.reduce (Array.map2 min)

                atlas.Walks.[(from, factor, goalRoom)] <- table
                Some table

/// The walk in whole ticks a freshly cast body needs to stand on a tile — the
/// half of a lead paid after the spawner is done. Keyed on a body, not a
/// creep name: it has not been cast yet. Priced empty and starting on the
/// tiles *beside* the spawner, since the engine places a finished creep on a
/// free neighbour for no step. Traffic-blind, like every lead. None when the
/// goal is unreachable or the spawner has no free neighbour. The *goal's*
/// room is the caller's, because a row's creeps do not all live at home: an
/// outpost's Post hires its Anchor off the home row, and a reserver's whole
/// life is the far side of a Seam.
let castWalkTicks
    (atlas: Atlas)
    (body: BodyPart list)
    (spawnTile: Pos)
    (target: RoomPos)
    : int option =
    castTable atlas (emptyFactorOf body) spawnTile target.Room
    |> Option.bind (fun table ->
        match table.[indexOf (RoomPos.pos target)] with
        | d when d = unreached -> None
        | d -> Some d)

/// The walk in whole ticks a body of one fatigue factor makes from beside a
/// home tile to beside a target (#373): the delivery's loaded leg, priced from
/// the store the load is drawn at and for the body **as loaded**, rather than
/// from wherever the candidate stands and for the body as it stands. The
/// candidate stands empty when it asks — it has to, to draw — and an empty
/// body is not the one that walks the leg (`Grid.factorCarrying`). Starts on
/// the store's free neighbours and ends on the target's, which is where a
/// Refill acts from. Traffic-blind and priced as a walk, like every lead. None
/// for a store outside the home room — the delivery draws from the home
/// Storage and nothing else is this function's to price — and for a target no
/// route reaches: an unpriceable leg refuses nobody.
let walkTicksFrom
    (atlas: Atlas)
    (factor: FatigueFactor)
    (from: RoomPos)
    (target: RoomPos)
    : int option =
    if from.Room <> atlas.Home then
        None
    else
        castTable atlas factor (RoomPos.pos from) target.Room
        |> Option.bind (fun table ->
            nearestReached
                (reachedIn table)
                (adjacentWalkableIn atlas target.Room (RoomPos.pos target)))

/// Cheapest raw-terrain path for a trunk road: plain 2, swamp
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
    // standing in the line is as impassable to a planned road as a wall.
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

    match cheapestReached (reachedIn dist) goals with
    | None -> []
    | Some(_, goal) ->
        let originIndex = indexOf origin

        let rec walk index acc =
            if index = originIndex then
                acc
            else
                walk parents.[index] (RoomPos.at room (posAt index) :: acc)

        walk (indexOf goal) []
