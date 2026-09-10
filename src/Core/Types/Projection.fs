/// The spatial projection (ADR 0004): one tick of one room laid out as layers
/// of tiles by kind (`RoomLayer`), and the whole projection over every room a
/// colony works (`SpatialInfo`). Total — what it cannot place, it omits.
[<AutoOpen>]
module Fabot.Core.Types.Projection

/// Current and maximum hit points of a repairable structure — what a
/// kind's whole line is judged against (ADR 0010, ADR 0034).
type HitsInfo = { Hits: int; HitsMax: int }

/// Three-state terrain of one room tile.
type Terrain =
    | Plain
    | Swamp
    | Wall

/// What kind of thing a projected target is.
type TargetKind =
    | Source
    | Controller
    | Structure of BuiltKind
    | Site of BuiltKind
    /// A dropped energy pile. Two readers: the [[pickup reflex]], which takes
    /// what is already at a creep's feet and reads no amount, and the Pickup
    /// Task (#167), which walks a hauler to a pile big enough to be worth the
    /// trip and reads the amount out of `SpatialInfo.Stores`.
    | Dropped
    /// A tombstone or a ruin: a store with a clock on it. One kind for both
    /// engine objects, because the only thing any reader decides on is that it
    /// holds energy and will be gone, and `Withdraw` is the verb for either.
    | Tombstone

/// Whether a projected target is one of the two transient kinds — a pile or a
/// tombstone/ruin — that stand on a tile without holding it. Both vanish
/// within a few hundred ticks, so a census that let one keep a construction
/// site off its tile would make the Layout's ordering depend on where a creep
/// happened to die (ADR 0011's determinism).
let isTransient =
    function
    | Dropped
    | Tombstone -> true
    | Source
    | Controller
    | Structure _
    | Site _ -> false

/// One room's geometry, filed under that room's name (ADR 0041): every
/// container the projection keys by `Pos`, gathered into one record rather
/// than five maps side by side, so reading a room's geometry is one lookup.
/// The id-keyed containers stay outside it, an object id being unique across
/// the world already. Absence stays per entry (ADR 0004): a room missing entry
/// by entry inside its layer and a room with no layer at all are the same
/// answer, so geometry is read through `SpatialInfo.layerOf` and never as
/// `.[name]`, which throws on a room the projection names but holds none for.
type RoomLayer =
    {
        /// Terrain per tile over this room's ground (x,y in 1..48); a tile
        /// absent from the map is impassable. The border ring is not here and
        /// is not ground: it rides in `SpatialInfo.Borders`, which the Seam
        /// query alone is priced off (ADR 0036, ADR 0041).
        Terrain: Map<Pos, Terrain>
        /// Target id -> that target's tile in this room: the Task targets, and
        /// the piles and tombstones a hauler is sent to. The two
        /// transient kinds are filtered out by kind where standing on a tile
        /// is not the same as holding it (`isTransient`).
        TargetPositions: Map<string, Pos>
        /// Creep name -> the tile the creep stands on in this room.
        CreepPositions: Map<string, Pos>
        /// Tiles blocked by obstacle structures and by their construction
        /// sites — the engine refuses to move a creep onto its own
        /// obstacle-type site; impassable regardless of terrain.
        Obstacles: Set<Pos>
        /// Tiles holding a built road — built structures only, a road
        /// construction site is not yet a road (ADR 0010).
        Roads: Set<Pos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomLayer =
    /// A room with nothing in it — every entry absent. What a `tryFind` on
    /// `SpatialInfo.Rooms` defaults to, so a room the projection holds no
    /// geometry for reads the same as one whose every container is empty (ADR
    /// 0004).
    let empty: RoomLayer =
        {
            Terrain = Map.empty
            TargetPositions = Map.empty
            CreepPositions = Map.empty
            Obstacles = Set.empty
            Roads = Set.empty
        }

/// A [[colony view]]'s spatial projection: the terrain of the rooms the colony
/// works plus positions of the entities decisions need to place on them.
type SpatialInfo =
    {
        /// Which entry of `Rooms` is the home room — the room the colony plans
        /// for, which is the room its spawn happens to stand in and is never
        /// defined by that (ADR 0041), and the room name the census signature
        /// and the Layout read (ADR 0017). None for a projection that names no
        /// room, whose geometry is filed under the empty name.
        RoomName: string option
        /// Room name -> that room's geometry, and the *only* place a
        /// `Pos`-keyed container lives: there is one projection and one shape
        /// of it (ADR 0005, ADR 0041). `RoomName` says which entry is home;
        /// every other entry is an outpost. Read an entry through
        /// `SpatialInfo.layerOf`: a room with no geometry has no entry here at
        /// all, and that is the same answer (ADR 0004).
        Rooms: Map<string, RoomLayer>
        /// Room name -> the terrain of that room's border ring (x or y of 0
        /// or 49), which a layer's `Terrain` deliberately leaves out. A layer
        /// of its own and never ground (ADR 0041): the engine moves a creep
        /// that ends its tick on an exit tile into the neighbouring room, so
        /// admitting one as walkable would let a Seat or a Work Area teleport
        /// the creep out from under its Task. It enters no weight grid or
        /// walkable set — the Atlas lays it a grid of its own — and only the
        /// Seam query and the crossing's price read it (ADR 0004).
        Borders: Map<string, Map<Pos, Terrain>>
        /// Task-target id -> what kind of thing stands (or will stand)
        /// there. Id-keyed and so unlayered (ADR 0041): an object id is
        /// already unique across the world, and the layer that places the
        /// id *is* the room it stands in (`SpatialInfo.placementOf`).
        TargetKinds: Map<string, TargetKind>
        /// Target id -> current/max hits, repairable kinds only — the decaying
        /// roads and containers (ADR 0010, ADR 0012), the Keep and our own
        /// ramparts (ADR 0034); fields nobody decides on stay out.
        Hits: Map<string, HitsInfo>
        /// Target id -> energy currently stored: the stock the logistics Tasks
        /// judge a store by. The containers (ADR 0012) and the Storage (ADR
        /// 0023) are the standing stores, and the two transient ones are here
        /// on the same key — a tombstone's or a ruin's energy, and a pile's
        /// amount.
        Stores: Map<string, int>
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
        }

    /// The name the projection's own room is filed under: `RoomName`, and the
    /// empty name when it names none — the name the census signature has
    /// always spelled that way. Decided here once, so a site cannot file the
    /// home room under one name and read it under another, which ADR 0004
    /// would answer with the empty set rather than a throw.
    let homeName (spatial: SpatialInfo) : string =
        spatial.RoomName |> Option.defaultValue ""

    /// One room's geometry, as ADR 0004 has every other absence: a room the
    /// projection carries no layer for reads as a room whose every entry is
    /// absent, never as a lookup that throws. The one spelling of that read,
    /// so no reader has to remember the default.
    let layerOf (spatial: SpatialInfo) (room: string) : RoomLayer =
        Map.tryFind room spatial.Rooms |> Option.defaultValue RoomLayer.empty

    /// The room the projection files a target id under, with its tile there,
    /// and None for a target it does not place, which classifies nothing and
    /// blocks nothing (ADR 0004). The id-to-room join on the projection itself,
    /// beside the one the Atlas precomputes (`TargetAt`); the two answer alike,
    /// the Atlas filling `TargetAt` by walking these same layers.
    let placementOf (spatial: SpatialInfo) (id: string) : RoomPos option =
        spatial.Rooms
        |> Map.tryPick (fun room (layer: RoomLayer) ->
            Map.tryFind id layer.TargetPositions |> Option.map (RoomPos.at room))

    /// The room the projection files a target id under, and None for a target it
    /// does not place (ADR 0004). `placementOf` with the tile dropped, which is
    /// what most of its callers wanted: the projection-side twin of the
    /// Atlas's `targetRoom`, so "which room is this id in" is one named join on
    /// both sides of the Atlas boundary rather than a lambda re-typed at each
    /// site.
    let roomOf (spatial: SpatialInfo) (id: string) : string option =
        placementOf spatial id |> Option.map (fun tile -> tile.Room)

    /// Every target the projection carries hits for, joined to the structure
    /// kind it is filed under, in id order. Hits with no kind, and hits on a
    /// target of a non-structure kind, drop out (ADR 0004). One walk over the
    /// two maps, which its readers used to make privately and had to agree on
    /// the ordering of: the Repair pool's hungry census keys off these ids and
    /// so does the Raid log's damage differencing.
    let structureHits (spatial: SpatialInfo) : (string * BuiltKind * HitsInfo) list =
        spatial.Hits
        |> Map.toList
        |> List.choose (fun (id, hits) ->
            match Map.tryFind id spatial.TargetKinds with
            | Some(Structure kind) -> Some(id, kind, hits)
            | _ -> None)

    /// The ids the projection files under one kind, in id order. The
    /// containers, the Storage and the controllers are all pooled by the
    /// projection's kind — never by position, never by name — so the walk is
    /// written here once, beside the kind census it reads. It had been a local
    /// helper of one pool with the promise in its comment, and two other
    /// modules re-derived it anyway.
    let idsOfKind (spatial: SpatialInfo) (kind: TargetKind) : string list =
        spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, k) -> if k = kind then Some id else None)

    /// What one store holds this tick, and 0 for a target the projection
    /// carries no store for — the reading its three readers each want and each
    /// used to spell for itself.
    let storedIn (spatial: SpatialInfo) (id: string) : int =
        spatial.Stores |> Map.tryFind id |> Option.defaultValue 0
