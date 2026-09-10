module Fabot.Core.Atlas

open Fabot.Core.Types
open Fabot.Core.Grid

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
            /// The kind census read the other way round: kind -> the ids of
            /// that kind, in id order. `SpatialInfo.TargetKinds` answers "what
            /// kind is this id", and every census on this API asks the
            /// opposite — "which ids are Towers" — which used to mean walking
            /// the whole census and comparing a union case per entry, once per
            /// ask and a dozen asks a tick. Inverted once beside the joins
            /// above, so an ask is one lookup. Flat for the same reason
            /// `TargetKinds` is: an object id is unique across the world, so
            /// the kind census needs no room (ADR 0041).
            KindIds: Map<TargetKind, string list>
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
            /// Memoised flood *into* a Seam band — the walk out of every tile
            /// of one room onto the crossings joining it to a named neighbour
            /// (ADR 0042's container pick). One flood per ordered room pair,
            /// however many tiles are read off it, so the Seats of every source
            /// share one answer.
            SeamWalks: System.Collections.Generic.Dictionary<string * string, int[]>
            /// Memoised far leg per **chain** of rooms — `FarFloods` with the
            /// room replaced by the route the walk crosses (ADR 0058). A
            /// chain of one room is the one-hop far leg the join has always
            /// read, keyed the same way beside the same Task, body and
            /// pricing; a longer chain is that flood with a hop's seeds
            /// folded on per further room. One table and not two, because a
            /// reader that had to know which it wanted would be a reader that
            /// could ask for the wrong one.
            FarFields:
                System.Collections.Generic.Dictionary<
                    string list * Task * bool * FatigueFactor * Pricing,
                    int[]
                 >
            /// Memoised room chain per ordered room pair — the rooms a walk
            /// between them crosses, ends included (ADR 0058). Answered off
            /// the border rings alone, so it is settled before any flood is
            /// forced and a pair with no chain costs the tick one search and
            /// no grid. `None` is an answer and is memoised as one: a pair
            /// beyond the hop budget is asked about once per creep that
            /// prices toward it.
            Routes: System.Collections.Generic.Dictionary<string * string, string list option>
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
        SeamWalks = System.Collections.Generic.Dictionary()
        Routes = System.Collections.Generic.Dictionary()
        FarFields = System.Collections.Generic.Dictionary()
        Walks = walks
        WorkAreas = System.Collections.Generic.Dictionary()
        HeavyAreas = System.Collections.Generic.Dictionary()
        Heavy =
            view.Creeps
            |> List.filter (fun creep -> partCount creep.Body Work > partCount creep.Body Move)
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
    Map.tryFind kind atlas.KindIds |> Option.defaultValue []

/// The tile an id stands on **in the named room**: None for an id the
/// projection places in another room or does not place at all (ADR 0004). The
/// room half is not a nicety — a `Pos` carries no room (ADR 0041), so every
/// census that unions tiles has to drop the other rooms' before it unions, and
/// that is the whole of what its readers ask `TargetAt`.
let private tileIn (atlas: Atlas) (room: string) (id: string) : Pos option =
    match Map.tryFind id atlas.TargetAt with
    | Some(where, tile) when where = room -> Some tile
    | _ -> None

/// One room's tiles stamped with their room, and empty for geometry the
/// projection places nowhere (ADR 0004) — the tail every `…In` census wears on
/// its way out of the Atlas, because a caller outside holds no room to stamp
/// a bare `Pos` with.
let private stamped (tiles: (string * Set<Pos>) option) : Set<RoomPos> =
    tiles
    |> Option.map (fun (room, grid) -> RoomPos.setAt room grid)
    |> Option.defaultValue Set.empty

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

// The two counts below are one half of the Layout's gap rule — `allowed at RCL
// - built - pending` — and the allowance is a fact about one room's
// controller, so the census subtracted from it has to be one room's too. One
// pair over the kind rather than a pair per kind, so one gap rule sizes every
// kind the ordering picks for (ADR 0022) and a fourth clustered kind is a
// caller's argument rather than two more exports.

/// Structures of one kind already standing in the named room.
let builtIn (atlas: Atlas) (room: string) (kind: BuiltKind) : int =
    placedOfKindIn atlas room (Structure kind) |> List.length

/// Construction sites of one kind already placed in the named room.
let pendingIn (atlas: Atlas) (room: string) (kind: BuiltKind) : int =
    placedOfKindIn atlas room (Site kind) |> List.length

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

/// The tiles some ids stand on **in the named room**, unioned — the join
/// between the flat id censuses and one room's positions, written once because
/// every tile census below is that join with a different list of ids in front
/// of it. Ids the room does not place drop out (ADR 0004), which is what keeps
/// another room's coordinates out of a `Set<Pos>` that has no room dimension.
let private tilesOfIdsIn (atlas: Atlas) (room: string) (ids: string list) : Set<Pos> =
    let layer = layerOf atlas room

    ids
    |> List.choose (fun id -> Map.tryFind id layer.TargetPositions)
    |> Set.ofList

/// Tiles of one room's placed targets whose kind answers a predicate — the
/// join between the flat kind census and that room's positions, for the
/// censuses read as tiles rather than as counts. The room is named rather
/// than searched (ADR 0041): a `Set<Pos>` has no room dimension, so two
/// rooms' tiles unioned would stand in neither room alone.
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
/// pending road is not yet a road (ADR 0010) but its tile needs no new site.
let pendingRoadTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    tilesOfKindIn atlas room (Site BuiltKind.Road)

/// Tiles of one room holding a built container — the container census's
/// standing half (ADR 0012): a built container keeps a plan from re-dropping
/// its site.
let private containerTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
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
    targetsOfKind atlas (Structure BuiltKind.Rampart)
    |> List.filter (fun id -> Map.containsKey id atlas.Spatial.Hits)
    |> tilesOfIdsIn atlas room

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
/// starts from. None for a Task the projection places nothing for: Flee has no
/// target and no action (ADR 0033), and a Guard's target is a hostile creep,
/// which is no target of the projection's at all — its geometry is the colony's
/// own `Threats` and its act is the Emitter's, so every query below gives it
/// Flee's answer and the decision layer carries both (ADR 0056).
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
    | Flee
    | Guard _ -> None

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
                Some(room, RefillCluster.hungry cluster |> List.choose (tileIn atlas room))

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
    seatTilesIn atlas sourceId |> stamped

/// Seats of a source: its Seat tile count. None for a source the
/// projection does not place: no capacity is derivable, and unpriceable
/// geometry never counts against a Task.
let seats (atlas: Atlas) (sourceId: string) : int option =
    seatTilesIn atlas sourceId |> Option.map (snd >> Set.count)

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
/// `workAreaFor`, which narrows it for a Work-heavy harvester (ADR 0020). Empty
/// when the projection cannot place the target.
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
/// `dualSeatsIn` and posts. Named room and not every layer (ADR 0041): the
/// union is intersected with an Upgrade area below, and two rooms' Seats
/// unioned would meet it at a coordinate that is a Dual Seat in neither.
let private seatUnionIn (atlas: Atlas) (room: string) : Set<Pos> =
    let ground = groundOf atlas room

    targetsOfKind atlas Source
    |> List.choose (tileIn atlas room)
    |> List.map (seatTiles ground)
    |> List.fold Set.union Set.empty

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
/// geometry reserves nothing (ADR 0004). This is the **Layout's** question and
/// stays it: what the mover asks is `idleGroundIn` below, a strictly wider set
/// (#268), and widening this one instead would move every clustered pick.
let workingGroundIn (atlas: Atlas) (room: string) : Set<Pos> =
    if room = atlas.Home then
        Set.union (seatUnionIn atlas room) (upgradeAreaIn atlas room)
    else
        seatUnionIn atlas room

/// The tiles of one room's **stores**, as #268 enumerates them: a built
/// [[container]], which is a source container or the [[buffer]] (ADR 0012);
/// the [[storage]] (ADR 0023); and the [[refill cluster]]'s members, the spawn
/// and its extensions read as one ring (ADR 0054). A tower is **not** in that
/// enumeration and is not held out for a reason of its own: a tower is a
/// [[refill]] target like any other — ADR 0010 puts a tower's Refill in the
/// pool at the surplus tier, and a hauler holding one stands on its range-1
/// ring exactly as the Storage's does, so a wall-tucked tower jams the same
/// way. It is left for a follow-up rather than smuggled in here. A
/// construction *site* is not one either — nothing is poured into or taken out
/// of a site. Filed by room like every other census (ADR 0041): a member the
/// projection places in another room, or not at all, contributes no tile (ADR
/// 0004).
let private storeTilesIn (atlas: Atlas) (room: string) : Set<Pos> =
    let clusterTiles =
        match atlas.Cluster with
        | None -> Set.empty
        | Some cluster ->
            cluster.Members
            |> Map.toList
            |> List.choose (fst >> tileIn atlas room)
            |> Set.ofList

    Set.unionMany [ containerTilesIn atlas room; storageTilesIn atlas room; clusterTiles ]

/// The [[idle ground]] of the room (#268): the working ground above, plus the
/// walkable range-1 ring of every store in the room. The two
/// questions are different and this is the one the [[resolver]] asks — the
/// working ground is "where work happens", which is what the Layout needs;
/// what the mover needs is "where standing idle blocks somebody", which is a
/// fact about traffic and strictly larger. #241 gave the mover the Layout's
/// set on the strength of the stores the live jam was about already standing
/// inside it — a source container stands on a Seat, the [[buffer]] stands in
/// the Upgrade Work Area — and the two it left out came back as their own jam:
/// a [[storage]] against a wall has two standing tiles, and two idle bodies on
/// them shut the hauler holding its [[refill]] out exactly as W13S28's pocket
/// did. Only the *ring* is added and never the store's own tile, because every
/// store a body can stand on is already inside the working-ground half by the
/// Layout's construction (a container serves a source from one of its Seats or
/// the controller from its Upgrade Work Area, ADR 0040) and the rest — the
/// Storage, the spawn, an extension — are obstacles nothing stands on at all.
/// Total: a room with no working ground and no store answers empty (ADR 0004).
let idleGroundIn (atlas: Atlas) (room: string) : Set<Pos> =
    let rings =
        storeTilesIn atlas room
        |> Set.toList
        |> List.collect (adjacentWalkableIn atlas room)
        |> Set.ofList

    Set.union (workingGroundIn atlas room) rings

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
        Set.intersect (seatUnionIn atlas room) (containerTilesIn atlas room)

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
/// the projection does not place, and for one with none of the three — a
/// built container on a Seat, a container site on a Seat, or a Dual Seat.
/// Every half is read in the source's own room (ADR 0041), and the Seat join
/// is what keeps a neighbouring source's site out: a Post belongs to the rock
/// it seats, not to the rock it is near.
///
/// Both the **number** Harvest's Post cap admits and the **tiles** it reads its
/// garrison off (ADR 0024 as #269 widened it): a Post is taken while a heavy
/// body stands on it, whatever that body holds this tick, so the cap's two
/// halves are read off one census and a Post can never be full as a number
/// while reading vacant as a tile.
let private postsOfIn (atlas: Atlas) (sourceId: string) : (string * Set<Pos>) option =
    postsOfBy postsIn atlas sourceId

let postsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> = postsOfIn atlas sourceId |> stamped

/// The **standing** Posts of one source: `postsOf` above less the Seats whose
/// container is still a site — the switch that admits a source into the quotas
/// (ADR 0042).
let standingPostsOf (atlas: Atlas) (sourceId: string) : Set<RoomPos> =
    postsOfBy standingPostsIn atlas sourceId |> stamped

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

/// A creep and a target read as one room's geometry: the room they share, the
/// tile the creep stands on and the tile the target stands on. None when
/// either is unplaced or the two stand in different rooms — ADR 0041's "one
/// room or no answer" written once, because a `Pos` carries no room and every
/// reader that joined the two maps itself wrote this guard again.
let private together
    (atlas: Atlas)
    (creep: string)
    (targetId: string)
    : (string * Pos * Pos) option =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
    | Some(creepRoom, tile), Some(targetRoom, target) when creepRoom = targetRoom ->
        Some(creepRoom, tile, target)
    | _ -> None

/// The mirror: the creep's room, the tile it stands on and the target's room,
/// once the two names have been read and found **different**. None for an
/// unplaced half as well, absence being no crossing (ADR 0004).
let private apart
    (atlas: Atlas)
    (creep: string)
    (targetId: string)
    : (string * Pos * string) option =
    match Map.tryFind creep atlas.CreepAt, Map.tryFind targetId atlas.TargetAt with
    | Some(creepRoom, from), Some(targetRoom, _) when creepRoom <> targetRoom ->
        Some(creepRoom, from, targetRoom)
    | _ -> None

/// Whether a creep and a Task's target stand in one room — the question every
/// join between a creep and a target's geometry has to settle while no flood
/// leaves its room (ADR 0041). Absence is permissive (ADR 0004): a Task acting
/// on nothing, an unplaced creep and an unplaced target are each not a border
/// crossing.
let private sharesRoom (atlas: Atlas) (creep: string) (task: Task) : bool =
    actionOn task
    |> Option.forall (fun (targetId, _) -> (apart atlas creep targetId).IsNone)

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
/// room's, and a caller that wants the far room's origins asks `narrowedArea`
/// — from outside this module, `workAreaAcross` below.
/// The mover crosses around this query rather than through it: `firstStep`
/// answers the near side of the winning Seam when these tiles are empty, so the
/// action and reachability gates grew no border-crossing answer of their own.
let workAreaFor (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
    if not (sharesRoom atlas creep task) then
        Set.empty
    else
        narrowedArea atlas creep task

/// The same Work Area, narrowed for the same body, in the room the Task's own
/// target stands in — whichever room the creep is standing in. `workAreaFor`
/// without the in-room gate above, which is `narrowedArea` itself. Its one
/// reader is the threat gate (ADR 0033, #147), and what it is there for is that
/// gate's question: "is every tile this Task can be worked from inside a
/// [[reach]]" is asked of the **target's** room and not of the creep's share of
/// it, so asked through `workAreaFor` a creep a border away is handed the empty
/// set by construction and the answer comes back "no" however hot that room is.
/// A *reading* and never a licence — the permission above is untouched, nothing
/// stands or acts on these tiles from another room, and no walk is priced over
/// them.
let workAreaAcross (atlas: Atlas) (creep: string) (task: Task) : Set<RoomPos> =
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
    together atlas creep sourceId
    |> Option.exists (fun (room, tile, _) ->
        Set.contains tile (containerTilesIn atlas room)
        && (match seatTilesIn atlas sourceId with
            | Some(_, seats) -> Set.contains tile seats
            | None -> false))

/// Whether a creep stands where it could dig a source: in that source's own
/// room and within the engine's harvest range of it. The widened half of
/// `catchesOverflow` above and deliberately weaker (ADR 0048): that one asks
/// whether the tile catches a full store's overflow, which is a fact about the
/// container underfoot, while this asks only whether the creep is in position
/// the tick the energy lands. Measured by range rather than by Seat membership,
/// so a creep the engine has put on ground the projection carries none for is
/// in position all the same (ADR 0004).
let standsAtSource (atlas: Atlas) (creep: string) (sourceId: string) : bool =
    together atlas creep sourceId
    |> Option.exists (fun (_, tile, source) -> range tile source <= 1)

/// How the Atlas reads a border ring: off the grid it lays per room, where
/// `terrainWeight` has already given wall -1 and every other terrain a
/// positive weight. The other reading of the same question is the [[world]]'s
/// (`World.linked`), off the border map itself before any grid exists — one
/// `Seam` module answers both so they cannot disagree (ADR 0058).
let private ringWalkable (atlas: Atlas) (room: string) : Pos -> bool =
    let ring = ringOf atlas room
    fun tile -> walkableAt ring tile

/// The Seam band joining two rooms: the passable exit-tile pairs, each this
/// room's border tile beside the tile it lands a creep on in the neighbour (ADR
/// 0041). The third kind of geometry beside the Seat and the Post — those are
/// tiles a creep works from, a Seam is one it can only pass through — and never
/// a tile anything offers to stand on: it is answered from the border layer,
/// which enters no walking grid, walkable or buildable set and no Work Area, so
/// the Matcher cannot pick one and have the engine empty it the tick a creep
/// arrives. Deterministic (X, Y) order, total (ADR 0004).
let seams (atlas: Atlas) (fromRoom: string) (toRoom: string) : (Pos * Pos) list =
    Seam.bandBy (ringWalkable atlas fromRoom) (ringWalkable atlas toRoom) fromRoom toRoom

/// The rooms a walk from one room to another crosses, ends included, or `None`
/// where the projection joins them by no chain inside the hop budget (ADR
/// 0058). The Seam model's one-hop answer generalised: `seams` says which of
/// two rooms' four grid neighbours a creep can actually step into, and
/// `RoomName.routeBy` walks that relation breadth first out to
/// `Tuning.MaxHops`.
///
/// Two rooms with a band between them answer `[from; to]`, which is every
/// chain this bot could name before this ADR — so a one-hop price is the same
/// price, joined over the same band, and the whole of what multi-hop adds is
/// the chains that used to be `None`. A room the projection does not carry has
/// no ring, `seams` gives it an empty band, and it is joined to nothing: the
/// search therefore stays inside the rooms `RoomName.transitBetween` put in
/// the world and cannot wander the sector on a tick that memoised a long
/// chain. Memoised per ordered pair, `None` included (ADR 0004: the absence is
/// the answer, and it is answered once).
let route (atlas: Atlas) (fromRoom: string) (toRoom: string) : string list option =
    memoised atlas.Routes (fromRoom, toRoom) (fun () ->
        RoomName.routeBy
            (fun here there ->
                Seam.joinedBy (ringWalkable atlas here) (ringWalkable atlas there) here there)
            atlas.Tuning.MaxHops
            fromRoom
            toRoom)

/// Whether a creep stands on a Seam — its room's border ring, the tile the
/// engine put it down on the tick it crossed. Read off the coordinate alone;
/// total (ADR 0004).
let standsOnSeam (atlas: Atlas) (creep: string) : bool =
    match Map.tryFind creep atlas.CreepAt with
    | Some(_, pos) -> pos.X = 0 || pos.X = Seam.exitEdge || pos.Y = 0 || pos.Y = Seam.exitEdge
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
///
/// The tile comes back beside the price, because the readers that walk a body
/// rather than price it need to know **which** goal won: of equally cheap goals
/// the lowest tile wins, and `List.min` over the pair settles that tie the same
/// way everywhere it is asked. A second argmin written out elsewhere agrees on
/// every number and splits on every tie, which is how a body comes to be walked
/// toward one goal and ranked at another.
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
/// it: it is the anchor ADR 0042's outpost container pick is made against, an
/// outpost having no spawn for a trunk to anchor on. Total (ADR 0004): `None`
/// for two rooms with no band between them, a room the projection carries no
/// ground for, a tile off the grid and a tile no crossing reaches — an
/// unpriceable Seam is no Seam, never a blocked one.
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

/// One crossing of a chain, named by the room it leaves (ADR 0058). The Seam
/// band a hop is joined over is `seams From To`, so a pair's first tile is
/// always `From`'s and its second `To`'s — which is what lets a fold say only
/// which room its field is over and have every selector follow from that.
type private Hop = { From: string; To: string }

/// The chain as the hops it crosses, in the order a walk crosses them.
let private hopsAlong (chain: string list) : Hop list =
    chain
    |> List.pairwise
    |> List.map (fun (here, there) -> { From = here; To = there })

/// A cost field over one room's ground carried across a Seam onto the other
/// room's: every tile the crossing puts a creep beside, seeded at what it costs
/// to be standing on it (ADR 0058). The whole of what multi-hop adds to ADR
/// 0041's arithmetic, and it is `joinedAcross`'s three terms again, charged to
/// the same tiles — the field's own side, the exit tile, and the tile the far
/// side is entered on.
///
/// Which way the hop is crossed is **read off `fieldRoom`** and never passed:
/// the field is over one of the hop's two rooms, the seeds go to the other, and
/// each side's tile of a Seam pair follows from which end of the hop it is. That
/// is what makes one function serve a chain built backwards from a Task's ground
/// and one built forwards from a spawn — they differ in nothing but which end
/// the field starts at (ADR 0030). The exit's own price belongs to `Hop.From`
/// whichever way the field runs: it is the tile the creep steps **onto**, and
/// the landing it is put down on afterwards costs nothing, which is the
/// convention `joinedAcross` states in full. The room the seeds are for comes
/// back beside them, because the fold's next step is over that room and
/// deriving it twice is how the two could disagree.
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

    // The seed room's own traffic, read the way `floodPricedInto` reads it: the
    // crowd is priced in a transit room exactly as it is in the room a Task
    // stands in, or in neither, and which of the two is the pricing's to say
    // (ADR 0029, ADR 0030) and never this hop's. The step table is the same
    // table either way — it is a function of the body and the pricing alone.
    let stepPrices, traffic = pricingOf (occupiedOf atlas seedRoom) factor pricing

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
/// next room's ground and a single-room flood settles it (ADR 0058). The one
/// fold, whichever end the chain is walked from — `chainedInto` hands it the
/// hops reversed and a field over the last room, `castAlong` hands it the same
/// hops in order and a field over the first, and neither knows anything the
/// other does not.
///
/// So a three-hop field is three single-room floods laid end to end and never a
/// flood over three rooms: ADR 0041's "no flood ever leaves its room" is as
/// literally true of a chain as of one crossing. Unreachable stays an absence
/// throughout (ADR 0004) — a hop whose band the terrain leaves empty seeds
/// nothing, and a field nothing seeded reaches no tile, so what the join asks
/// for is `None` rather than a number with a gap in it.
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

            let stepPrices, traffic = pricingOf (occupiedOf atlas seedRoom) factor pricing

            let settled =
                floodFromAllSeeded (weightsOf atlas seedRoom) traffic stepPrices seeds
                |> drained
                |> fst

            seedRoom, settled)
        start
    |> snd

/// The far leg over a whole chain of rooms: today's flood into the Task's own
/// ground in the last room, carried **back** across each Seam of the chain in
/// turn (ADR 0058). What comes back is a field over the **first** room of the
/// chain, which is the room `joinedAcross` joins the creep's own near leg to —
/// so the join is handed exactly what it was handed before and does not know
/// how many borders are behind the number.
///
/// A chain of one room is `floodPricedInto` and nothing else, which is every
/// walk this bot priced before ADR 0058: there are no hops, the fold runs zero
/// times, and the array is the same array.
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
        let target =
            floodPricedInto (weightsOf atlas last) (occupiedOf atlas last) factor pricing origins

        foldChain atlas factor pricing (last, target) (hopsAlong chain |> List.rev)

/// The same chain memoised colony-wide for one Task and one body — the shape
/// every per-creep price reads it in, and the reason a second creep pricing the
/// same Task across the same rooms pays for no second chain.
let private farFieldAlong
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (chain: string list)
    (origins: Pos list)
    : int[] =
    let factor = factorOf atlas creep

    memoised atlas.FarFields (chain, task, workHeavy atlas creep, factor, pricing) (fun () ->
        chainedInto atlas factor pricing chain origins)

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
    match tiles with
    | [] -> unreached
    | _ ->
        let glimpse, frontier =
            match leg with
            | Drained dist -> reachedIn dist, unreached
            | Resuming flood -> glimpsedBy flood, frontierOf flood

        tiles |> List.fold (fun bound tile -> min bound (glimpse tile)) frontier

/// A cross-room price, joined on the Seam: the smallest, over the whole band
/// between the two rooms, of *walk to the exit tile* + *the exit tile's own
/// price* + *walk in from the tile it lands on* (ADR 0041). Each leg is a
/// single-room flood the caller has already run, so no flood ever leaves its
/// room and the join is a minimum over thirty-odd additions rather than over
/// thirty-odd floods. The join itself and not a second one (ADR 0030): a creep
/// priced toward a Task (`pricedAcross`) and the hauler quota's round trip
/// (`haulRoundTripTicks`) differ in nothing but which two floods they hand it,
/// and a lead's cast walk folds the same three terms into the seeds of one
/// flood (`castAlong`), so a change here is a change there. What the two
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

/// A creep's cross-room price toward ground in another room: the join above
/// over this creep's own memoised flood and the far leg, the one carried back
/// along the route and shared colony-wide, so a second creep pricing the same
/// Task across the same rooms pays for no second flood (ADR 0041, ADR 0058).
/// The route is read before either flood is forced, a pair of rooms with no
/// chain having nothing to pay for (ADR 0004). The far ground is the caller's,
/// for the reason `farFieldAlong` above carries.
///
/// The join is handed the **next** room of the chain and the band into it, not
/// the target and its own band: what the far leg answers is a field over that
/// next room, whatever is behind it. A one-hop route makes the two the same
/// room and this is the call it always was.
/// The prelude both cross-room joins wear: the route to the far room, the next
/// hop along it, the Seam band between here and *that hop* — never the target,
/// which is the one thing about `joinedAcross`' contract a second call site can
/// get wrong — and the join over the two legs. Each missing piece is an absence
/// of its own (ADR 0004): no route joins nothing, and neither does an empty
/// band.
///
/// The legs arrive as functions of what the prelude found rather than as values,
/// for two reasons that are both the callers': neither leg may be flooded before
/// there is a band to join it on, and the far leg is a fold along the chain the
/// route named. Which bound the near leg carries stays the caller's too — a walk
/// hands over a flood settled whole (`Drained`), a price one it can go on
/// relaxing (`Resuming`).
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
    match route atlas fromRoom toRoom with
    | Some(_ :: (next :: _ as onward)) ->
        match seams atlas fromRoom next with
        | [] -> None
        | band -> joinedAcross atlas pricing factor fromRoom from next band (near ()) (far onward)
    | _ -> None

let private pricedAcrossInto
    (atlas: Atlas)
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
        (fun onward -> reachedIn (farFieldAlong atlas pricing creep task onward origins))

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
        pricing
        creep
        task
        creepRoom
        from
        targetRoom
        (narrowedArea atlas creep task |> RoomPos.tilesIn targetRoom)

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
    actionOn task |> Option.bind (fun (targetId, _) -> apart atlas creep targetId)

/// The crossing a Task asks for, priced: the creep's room and tile, and the
/// winning `(price, exit)` of the Seam band joining it to the target's room —
/// None for a Task that asks no crossing at all. One derivation, because the
/// price (`pricedPath`) and the mover's step (`stepAcross`) have to be reading
/// the same minimisation: a second one agrees on every number and splits on
/// every tie, which walks a creep to one crossing while ranking it at another.
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
let private crossingToward
    (atlas: Atlas)
    (pricing: Pricing)
    (creep: string)
    (task: Task)
    (room: string)
    (area: Set<RoomPos>)
    : (string * Pos * (int * Pos) option) option =
    match Map.tryFind creep atlas.CreepAt with
    | Some(creepRoom, from) when creepRoom <> room ->
        Some(
            creepRoom,
            from,
            pricedAcrossInto
                atlas
                pricing
                creep
                task
                creepRoom
                from
                room
                (RoomPos.tilesIn room area)
        )
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
        match crossingFor atlas pricing creep task with
        | Some(_, _, won) -> won |> Option.map fst
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

/// Travel cost to an explicit set of tiles that names **its own room** — the
/// third member of the pair above, and the one a Seam does not stop. Inside
/// that room it is `travelCostWithin`'s answer; from outside it is the same
/// minimum over the Seam band every cross-room Task is priced by
/// (`pricedAcross`), taken toward the tiles the caller handed in rather than
/// toward a target's surroundings.
///
/// It exists for the [[guard]] (ADR 0056), whose Work Area is the colony's own
/// `Threats` and whose Task the projection places no target for: `travelCost`'s
/// crossing is derived from that target, so without this a Guard would price as
/// unreachable to every body not already standing in the raided room — which is
/// every body the guard row casts, the spawn being at home. An unplaced creep
/// prices at 0; a room no Seam joins, or a ring nothing reaches, has no price
/// at all (ADR 0004).
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

            cheapestReached (reachedBy near) goals
            |> Option.map (fun (_, goal) ->
                RoomPos.at room (posAt (firstStepOn near (indexOf pos) (indexOf goal))))

/// The step toward the near side of a crossing already won: the exit tile is
/// the creep's *own* room's border tile, so aiming at it asks nothing of the
/// neighbour and arbitrates nothing across the Seam — ADR 0041's boundary
/// stands exactly where it stood — and the engine puts the creep down in the
/// neighbour at the end of the tick it steps on, from where every rule already
/// written takes it on. The exit is the one the price was minimised at, handed
/// in rather than looked for again: a second argmin agrees on every number and
/// splits on every tie, which walks a creep to one crossing while ranking it at
/// another. Written once because the two crossings below — a Task's target's,
/// and a named room's — differ in what they price and in nothing they walk.
/// Total (ADR 0004): no winning crossing, no step.
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
/// minimisation. Total (ADR 0004): no crossing, no step.
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
/// exports below differ in — the Resolver reads a detour off the pair (ADR
/// 0008), and a pair that drifted apart in anything else would attribute one
/// where none was paid.
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
/// over and no second rule decides where a [[guard]] crosses (ADR 0056). Total:
/// a creep already in that room asks no crossing and is answered by the in-room
/// step above it.
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
    firstStepUnder atlas TravelCost creep task goals

/// The same first step toward an explicit set of tiles, with no Task beside it:
/// `firstStep`'s answer for a body that has none to cross a Seam for, which is
/// what an idle one stepping off the [[idle ground]] is (#241, #268).
/// `travelCost`
/// and `travelCostWithin` stand in the same pair for the same reason — the Task
/// buys the cross-room fallback and nothing else, so a caller whose goals are
/// tiles of the creep's own room by construction has no use for it.
let firstStepWithin (atlas: Atlas) (creep: string) (goals: Set<RoomPos>) : RoomPos option =
    firstStepVia atlas TravelCost creep goals

/// The step a creep takes toward a **room**, with nothing placed in it to aim
/// at: toward the near side of any Seam joining the two. `stepAcross` without
/// its far leg, and that is the whole of the difference — the exit is the one
/// this room's own flood reaches cheapest instead of the crossing a price was
/// won at, because a room nobody can see prices nothing (ADR 0004). What it
/// reads is the border layer and the memoised terrain, and neither waits for
/// vision (ADR 0031, ADR 0041): no remembered tile enters a walking grid and no
/// Work Area grows one. Written for the vision grace's crossing creep (#151),
/// whose target left the projection with its room's vision while the border it
/// is walking at stayed exactly where it was. Total (ADR 0004): no Seam, no
/// step, and a creep already in the room is not crossing to it.
let stepTowardRoom (atlas: Atlas) (creep: string) (room: string) : RoomPos option =
    match Map.tryFind creep atlas.CreepAt with
    | Some(creepRoom, from) when creepRoom <> room ->
        // The **next** room of the chain and not the goal (ADR 0058): what a
        // creep crossing toward a room two hops out can aim at is the border
        // it reaches first, and the hop after that is the same question asked
        // again from the room it lands in.
        match route atlas creepRoom room |> Option.bind (List.tryItem 1) with
        | None -> None
        | Some next ->

            match seams atlas creepRoom next with
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
/// won at. It is what walks a [[guard]] out of the home room the row cast it in
/// and into the raided outpost (ADR 0056), which is ADR 0033's "the mover walks
/// it into the Work Area like any other Task".
let firstStepToward
    (atlas: Atlas)
    (creep: string)
    (task: Task)
    (room: string)
    (goals: Set<RoomPos>)
    : RoomPos option =
    firstStepVia atlas TravelCost creep goals
    |> Option.orElseWith (fun () -> stepToward atlas creep task room goals)

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
    firstStepUnder atlas Baseline creep task goals

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
            joinedAlong
                atlas
                Walk
                factor
                fromRoom
                from
                sinkRoom
                (fun () -> Drained(fst (walkFloodFrom weights factor from)))
                (fun onward -> reachedIn (chainedInto atlas factor Walk onward goals))
            |> Option.map fst

    let loaded = legTicks (loadedFactorOf body)
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
///
/// Carried the length of the chain since ADR 0058, and by the same fold the
/// far leg is carried back along — a lead to a room three hops out is three
/// seedings and three floods, and the walk table holds the last of them.
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
/// 0030), through `castAlong` for the reason written there.
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
            match route atlas atlas.Home goalRoom with
            | None -> None
            | Some chain ->
                let table = castAlong atlas factor (near ()) chain
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
