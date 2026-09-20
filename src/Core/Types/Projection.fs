/// The spatial projection: one tick of one room laid out as layers of tiles by
/// kind (`RoomLayer`), and the whole projection over every room a colony works
/// (`SpatialInfo`). Total — what it cannot place, it omits.
///
/// ADR-0004 (total; absence per entry, never a throw)
/// ADR-0005 (one projection, one shape of it)
/// ADR-0041 (layered by room name; the border ring is its own layer)
[<AutoOpen>]
module Fabot.Core.Types.Projection

/// Current and maximum hit points of a repairable structure.
type HitsInfo = { Hits: int; HitsMax: int }

/// Three-state terrain of one room tile.
type Terrain =
    | Plain
    | Swamp
    | Wall

/// One room's terrain as a **flat grid** rather than a map: two thousand five
/// hundred slots indexed by `Geometry.indexOf`, an absent tile holding `None`
/// (#278). Absence included, which is what the projection's totality rests on:
/// a tile this does not carry is impassable, and so is a tile off the grid.
///
/// A map was the wrong shape for a whole-room table fixed for the life of the
/// server and rebuilt into the Atlas's weight grids every tick of every room:
/// that rebuild walked a 2,500-node tree per room per tick (#278: 4.13% of a
/// tick on the `pair` scenario). The grid is read with a bounded `for`, and
/// `World.terrainMemo` memoises the finished array once per room per reset.
///
/// A record around the array and not the bare array, so the projection cannot
/// be handed a differently-strided array by accident and equality is the
/// record's, which is what the fixtures compare.
type TerrainGrid = internal { Tiles: Terrain option[] }

/// Reading and writing a `TerrainGrid`. The names and the argument order are
/// `Map`'s on purpose: this module replaced a `Map<Pos, Terrain>` at forty-odd
/// call sites. Every entry guards the index with `inGrid` (`Geometry`), so an
/// off-grid `Pos` reads as absent and a write to one is dropped.
[<RequireQualifiedAccess>]
module TerrainGrid =

    /// The grid no room carries: every tile absent. Never written to, being
    /// shared by every caller that asks for one.
    let empty: TerrainGrid = { Tiles = Array.create tileCount None }

    let tryFind (pos: Pos) (grid: TerrainGrid) : Terrain option =
        if inGrid pos then grid.Tiles.[indexOf pos] else None

    let containsKey (pos: Pos) (grid: TerrainGrid) : bool = (tryFind pos grid).IsSome

    /// A copy with one tile written, never a mutation: the projection is a
    /// value the whole tick reads and the fixtures build variants off one
    /// another. The 2,500-slot copy is paid only where a `Map.add` was paid
    /// before — never in the tick's own path.
    let add (pos: Pos) (terrain: Terrain) (grid: TerrainGrid) : TerrainGrid =
        if not (inGrid pos) then
            grid
        else
            let tiles = Array.copy grid.Tiles
            tiles.[indexOf pos] <- Some terrain
            { Tiles = tiles }

    let remove (pos: Pos) (grid: TerrainGrid) : TerrainGrid =
        if not (inGrid pos) then
            grid
        else
            let tiles = Array.copy grid.Tiles
            tiles.[indexOf pos] <- None
            { Tiles = tiles }

    let ofList (tiles: (Pos * Terrain) list) : TerrainGrid =
        let grid = Array.create tileCount None

        for pos, terrain in tiles do
            if inGrid pos then
                grid.[indexOf pos] <- Some terrain

        { Tiles = grid }

    /// Every tile the grid carries, in `indexOf` order — which is (X, Y)
    /// order, the order `Map.toList` answered in and every "ties by (X, Y)"
    /// rule in the colony rests on.
    let toList (grid: TerrainGrid) : (Pos * Terrain) list =
        [
            for index in 0 .. tileCount - 1 do
                match grid.Tiles.[index] with
                | Some terrain -> yield posAt index, terrain
                | None -> ()
        ]

    let count (grid: TerrainGrid) : int =
        let mutable total = 0

        for index in 0 .. tileCount - 1 do
            if grid.Tiles.[index].IsSome then
                total <- total + 1

        total

    let forall (predicate: Pos -> Terrain -> bool) (grid: TerrainGrid) : bool =
        let mutable holds = true
        let mutable index = 0

        while holds && index < tileCount do
            match grid.Tiles.[index] with
            | Some terrain when not (predicate (posAt index) terrain) -> holds <- false
            | _ -> index <- index + 1

        holds

    /// A grid with every present tile's terrain rewritten, the absent ones
    /// left absent — `Map.map` for the same shape.
    let map (change: Pos -> Terrain -> Terrain) (grid: TerrainGrid) : TerrainGrid =
        let tiles = Array.create tileCount None

        for index in 0 .. tileCount - 1 do
            match grid.Tiles.[index] with
            | Some terrain -> tiles.[index] <- Some(change (posAt index) terrain)
            | None -> ()

        { Tiles = tiles }

    let exists (predicate: Pos -> Terrain -> bool) (grid: TerrainGrid) : bool =
        let mutable found = false
        let mutable index = 0

        while not found && index < tileCount do
            match grid.Tiles.[index] with
            | Some terrain when predicate (posAt index) terrain -> found <- true
            | _ -> index <- index + 1

        found

    /// The tick's own reader: the present tiles, index and terrain, without
    /// building a `Pos` or a list. The index is handed over raw because the
    /// caller's array (`Atlas.gridOf`) is strided the same way.
    let inline internal iterIndexed
        ([<InlineIfLambda>] handle: int -> Terrain -> unit)
        (grid: TerrainGrid)
        : unit =
        for index in 0 .. tileCount - 1 do
            match grid.Tiles.[index] with
            | Some terrain -> handle index terrain
            | None -> ()

    /// The same walk keyed by tile, for the readers that want a `Pos`.
    let inline internal iter
        ([<InlineIfLambda>] handle: Pos -> Terrain -> unit)
        (grid: TerrainGrid)
        : unit =
        iterIndexed (fun index terrain -> handle (posAt index) terrain) grid

/// What kind of thing a projected target is.
type TargetKind =
    | Source
    | Controller
    | Structure of BuiltKind
    | Site of BuiltKind
    /// A dropped pile, and **which resource it is** (#311). The resource rides
    /// **in** the kind because a pile holds exactly one: the engine's dropped
    /// resource keeps its amount in `object[resourceType]` and not in a
    /// `store`, so two resources on one tile are two objects with two ids —
    /// and that missing `store` is why a pile costs a standing creep no TTL
    /// where the container beside it does (`docs/research/thorium-reactor.md`
    /// §2). Filtering `FIND_DROPPED_RESOURCES` down to energy is what left the
    /// season's ore on the floor with no Task that could name it (#311).
    | Dropped of resource: Resource
    /// A tombstone or a ruin: a store with a clock on it. One kind for both
    /// engine objects, because the only thing any reader decides on is that it
    /// holds energy and will be gone, and `Withdraw` is the verb for either.
    | Tombstone
    /// A Thorium mineral — a fact read off `FIND_MINERALS`, not a declaration.
    /// Only Thorium ever reaches the projection, the shell filtering on
    /// `mineralType`, so the case carries no resource. What it has left to give
    /// rides in `SpatialInfo.Thorium`. The day the mod deletes an exhausted
    /// deposit the target leaves the projection, which retires everything hung
    /// off it.
    | Mineral

/// Whether a projected target is one of the two transient kinds — a pile or a
/// tombstone/ruin — that stand on a tile without holding it. Both vanish
/// within a few hundred ticks, so a census that let one keep a construction
/// site off its tile would make the Layout's ordering depend on where a creep
/// happened to die.
let isTransient =
    function
    | Dropped _
    | Tombstone -> true
    | Source
    | Controller
    | Structure _
    | Site _
    | Mineral -> false

/// One room's geometry, filed under that room's name: every container the
/// projection keys by `Pos`, gathered into one record so reading a room's
/// geometry is one lookup. The id-keyed containers stay outside it, an object
/// id being unique across the world already. Geometry is read through
/// `SpatialInfo.layerOf` and never as `.[name]`, which throws on a room the
/// projection names but holds none for.
type RoomLayer =
    {
        /// Terrain per tile over this room's ground (x,y in 1..48); a tile
        /// absent from the map is impassable. The border ring is not here and
        /// is not ground: it rides in `SpatialInfo.Borders`.
        Terrain: TerrainGrid
        /// Target id -> that target's tile in this room: the Task targets, and
        /// the piles and tombstones a hauler is sent to. The two transient
        /// kinds are filtered out by kind where standing on a tile is not the
        /// same as holding it (`isTransient`).
        TargetPositions: Map<string, Pos>
        /// Creep name -> the tile the creep stands on in this room.
        CreepPositions: Map<string, Pos>
        /// Tiles blocked by obstacle structures and by their construction
        /// sites — the engine refuses to move a creep onto its own
        /// obstacle-type site; impassable regardless of terrain.
        Obstacles: Set<Pos>
        /// Tiles holding a built road — built structures only, a road
        /// construction site is not yet a road.
        Roads: Set<Pos>
        /// Tiles held by a construction site **somebody else** placed. Tiles
        /// and nothing else — no id, no kind, no owner (#248): the engine takes
        /// one construction site per tile whoever owns it, so the only thing
        /// this colony can decide about one is not to ask for a site under it
        /// (`Atlas.collidingSiteTilesIn`). Not an obstacle either: the engine
        /// blocks a creep on an obstacle-type site of its **own owner's** and a
        /// hostile creep walking onto one destroys it, so these tiles stay out
        /// of `Obstacles` and out of the pricing.
        RivalSites: Set<Pos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomLayer =
    /// A room with nothing in it — every entry absent. What a `tryFind` on
    /// `SpatialInfo.Rooms` defaults to.
    let empty: RoomLayer =
        {
            Terrain = TerrainGrid.empty
            TargetPositions = Map.empty
            CreepPositions = Map.empty
            Obstacles = Set.empty
            Roads = Set.empty
            RivalSites = Set.empty
        }

/// A [[colony view]]'s spatial projection: the terrain of the rooms the colony
/// works plus positions of the entities decisions need to place on them.
type SpatialInfo =
    {
        /// Which entry of `Rooms` is the home room — the room the colony plans
        /// for, which is the room its spawn happens to stand in and is never
        /// defined by that. None for a projection that names no room, whose
        /// geometry is filed under the empty name.
        RoomName: string option
        /// Room name -> that room's geometry, and the *only* place a
        /// `Pos`-keyed container lives. `RoomName` says which entry is home;
        /// every other entry is an outpost. Read an entry through
        /// `SpatialInfo.layerOf`.
        Rooms: Map<string, RoomLayer>
        /// Room name -> the terrain of that room's border ring (x or y of 0
        /// or 49), which a layer's `Terrain` deliberately leaves out. Never
        /// ground: the engine moves a creep that ends its tick on an exit tile
        /// into the neighbouring room, so admitting one as walkable would let
        /// a Seat or a Work Area teleport the creep out from under its Task.
        /// It enters no weight grid or walkable set — the Atlas lays it a grid
        /// of its own — and only the Seam query and the crossing's price read
        /// it.
        Borders: Map<string, Map<Pos, Terrain>>
        /// Task-target id -> what kind of thing stands (or will stand) there.
        /// Id-keyed and so unlayered: the layer that places the id *is* the
        /// room it stands in (`SpatialInfo.placementOf`).
        TargetKinds: Map<string, TargetKind>
        /// Target id -> current/max hits, repairable kinds only; fields nobody
        /// decides on stay out.
        Hits: Map<string, HitsInfo>
        /// Target id -> energy currently stored: the standing stores, and the
        /// two transient ones on the same key — a tombstone's or a ruin's
        /// energy, and an **energy** pile's amount. A Thorium pile's rides in
        /// `Thorium` below (#311).
        Stores: Map<string, int>
        /// Target id -> Thorium currently held there: the mineral container's,
        /// the storage's, the deposit's own remaining amount, and a dropped
        /// Thorium pile's (#311) — the one entry whose target holds no `store`
        /// and so the one the contact penalty never prices. The sector Reactor
        /// is deliberately **not** among them: its store and streak ride
        /// `RoomFacts.Reactors`. A second id-keyed map beside `Stores` and not
        /// a `Map<string, Map<Resource, int>>`, which would make every energy
        /// reader ask a question it never asks; the generalisation is one
        /// commit away on the day a third resource has a reader.
        Thorium: Map<string, int>
        /// Target id -> ticks before this structure may act again — today the
        /// extractor's alone (`EXTRACTOR_COOLDOWN` is 5, so successive
        /// harvests land six ticks apart). Absent per entry: a structure with
        /// no clock on it has no entry here, and 0 means "now", which is a
        /// different answer.
        Cooldowns: Map<string, int>
        /// Target id -> whose that **object** is (#318): the per-object twin of
        /// `RoomControlInfo.Owner`, which answers nothing for a sector centre,
        /// there being no controller there to read. Absent is "we cannot see
        /// it" — for the errand's target, every tick the relay gaps — and the
        /// one reader treats that absence as **not ours**, because a withheld
        /// act on a missing fact leaves a rival's flag standing a tick longer.
        ///
        /// Filled for the sector Reactor and for nothing else today: the mod
        /// registers it as a custom object under `FIND_REACTORS`
        /// (`reactor.roomObject.js`), so it reaches neither `FIND_STRUCTURES`
        /// nor any other sweep this bot makes, and the sweep that finds it
        /// finds nothing else.
        Owners: Map<string, Ownership>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpatialInfo =
    /// The empty projection: no room, no tiles, no entities — every entry absent.
    let empty =
        {
            RoomName = None
            Rooms = Map.empty
            Borders = Map.empty
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
            Thorium = Map.empty
            Cooldowns = Map.empty
            Owners = Map.empty
        }

    /// The name the projection's own room is filed under: `RoomName`, and the
    /// empty name when it names none. Decided here once, so a site cannot file
    /// the home room under one name and read it under another.
    let homeName (spatial: SpatialInfo) : string =
        spatial.RoomName |> Option.defaultValue ""

    /// One room's geometry; a room the projection carries no layer for reads
    /// as a room whose every entry is absent, never as a lookup that throws.
    let layerOf (spatial: SpatialInfo) (room: string) : RoomLayer =
        Map.tryFind room spatial.Rooms |> Option.defaultValue RoomLayer.empty

    /// The room the projection files a target id under, with its tile there,
    /// and None for a target it does not place. The projection-side twin of
    /// the Atlas's `TargetAt`; the two answer alike.
    let placementOf (spatial: SpatialInfo) (id: string) : RoomPos option =
        spatial.Rooms
        |> Map.tryPick (fun room (layer: RoomLayer) ->
            Map.tryFind id layer.TargetPositions |> Option.map (RoomPos.at room))

    /// `placementOf` with the tile dropped: the projection-side twin of the
    /// Atlas's `targetRoom`.
    let roomOf (spatial: SpatialInfo) (id: string) : string option =
        placementOf spatial id |> Option.map (fun tile -> tile.Room)

    /// Where the projection places one of this colony's **creeps**, and None
    /// for a body it does not place — `placementOf`'s twin down the
    /// `CreepPositions` column.
    let creepPlacementOf (spatial: SpatialInfo) (name: string) : RoomPos option =
        spatial.Rooms
        |> Map.tryPick (fun room (layer: RoomLayer) ->
            Map.tryFind name layer.CreepPositions |> Option.map (RoomPos.at room))

    /// `creepPlacementOf` with the tile dropped.
    let creepRoomOf (spatial: SpatialInfo) (name: string) : string option =
        creepPlacementOf spatial name |> Option.map (fun tile -> tile.Room)

    /// Every target the projection carries hits for, joined to the structure
    /// kind it is filed under, in id order. Hits with no kind, and hits on a
    /// target of a non-structure kind, drop out. The Repair pool's hungry
    /// census and the Raid log's damage differencing both key off these ids
    /// and must agree on the ordering.
    let structureHits (spatial: SpatialInfo) : (string * BuiltKind * HitsInfo) list =
        spatial.Hits
        |> Map.toList
        |> List.choose (fun (id, hits) ->
            match Map.tryFind id spatial.TargetKinds with
            | Some(Structure kind) -> Some(id, kind, hits)
            | _ -> None)

    /// The ids the projection files under one kind, in id order.
    let idsOfKindIn (kinds: Map<string, TargetKind>) (kind: TargetKind) : string list =
        kinds
        |> Map.toList
        |> List.choose (fun (id, k) -> if k = kind then Some id else None)

    /// The same walk over a whole projection's census.
    let idsOfKind (spatial: SpatialInfo) (kind: TargetKind) : string list =
        idsOfKindIn spatial.TargetKinds kind

    /// What one store holds this tick, and 0 for a target the projection
    /// carries no store for.
    let storedIn (spatial: SpatialInfo) (id: string) : int =
        spatial.Stores |> Map.tryFind id |> Option.defaultValue 0

    /// What one store holds of one **resource** this tick, and 0 for a target
    /// the projection carries none of it for. The two id-keyed maps read
    /// through one name, so a rule written over a resource asks one question
    /// instead of branching on the resource at every site.
    let heldIn (spatial: SpatialInfo) (resource: Resource) (id: string) : int =
        match resource with
        | Energy -> storedIn spatial id
        | Thorium -> spatial.Thorium |> Map.tryFind id |> Option.defaultValue 0

    /// Whether one **object** is ours this tick (#318): an entry that says
    /// `Ours`, and false for a rival's, nobody's, and the entry missing
    /// altogether. Absence reads as *not ours* on purpose: the only object
    /// this is asked about is the sector Reactor, the only vision of it is a
    /// body of ours on its ring, and a withheld act on a fact we cannot see
    /// would leave whoever planted the flag holding it another tick at 5 score
    /// a tick.
    let ownsTarget (spatial: SpatialInfo) (id: string) : bool =
        Map.tryFind id spatial.Owners = Some Ownership.Ours
