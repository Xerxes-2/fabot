module Fabot.Core.Types

/// A creep body part, engine vocabulary kept minimal for the MVP worker.
type BodyPart =
    | Work
    | Carry
    | Move

/// What the decision layer knows about one spawn this tick.
type SpawnInfo =
    {
        Name: string
        /// Game-object id of the spawn structure — the key that locates
        /// this spawn in the spatial projection's target maps.
        Id: string
        /// Name of the room the spawn stands in — the key into the
        /// Snapshot's RoomEnergy banks.
        RoomName: string
        IsSpawning: bool
    }

/// One room's shared spawn-energy account this tick. Colony state, not
/// spawn state: every spawn in the room draws from the same bank.
type RoomEnergy =
    {
        /// Energy banked for spawning right now (spawn + extensions).
        Available: int
        /// Energy the room banks when every feeder is full (spawn + built extensions).
        Capacity: int
    }

/// What the decision layer knows about one structure that feeds spawning
/// (spawn or extension) this tick.
type RefillableInfo =
    {
        Id: string
        /// Energy the structure's store can still take (0 = full).
        FreeCapacity: int
    }

/// What the decision layer knows about one energy source this tick.
type SourceInfo = { Id: string }

/// What the decision layer knows about the room controller this tick.
type ControllerInfo =
    {
        Id: string
        /// Controller level (RCL); gates how many extensions may exist.
        Level: int
    }

/// A tile coordinate inside a room.
type Pos = { X: int; Y: int }

/// Three-state terrain of one room tile.
type Terrain =
    | Plain
    | Swamp
    | Wall

/// What a built structure is — or what a construction site will become
/// once built. Projection vocabulary, distinct from the Intent vocabulary
/// of placeable kinds (StructureKind).
[<RequireQualifiedAccess>]
type BuiltKind =
    | Spawn
    | Extension
    /// Any structure kind the decision layer has no rules for yet.
    | Other

/// What kind of thing a projected target is.
type TargetKind =
    | Source
    | Controller
    | Structure of BuiltKind
    | Site of BuiltKind

/// The Snapshot's spatial projection: the spawn room's terrain plus
/// positions of the entities decisions need to place on it.
type SpatialInfo =
    {
        /// Name of the room the projection covers. None when the
        /// projection is empty — absence is per-entry (ADR 0004).
        RoomName: string option
        /// Terrain per tile; a tile absent from the map lies outside the
        /// projected room and is impassable.
        Terrain: Map<Pos, Terrain>
        /// Task-target id (source, refillable structure, construction site,
        /// controller) -> that target's tile.
        TargetPositions: Map<string, Pos>
        /// Task-target id -> what kind of thing stands (or will stand) there.
        TargetKinds: Map<string, TargetKind>
        /// Creep name -> the tile the creep stands on.
        CreepPositions: Map<string, Pos>
        /// Tiles blocked by obstacle structures (spawn, extension,
        /// controller, ...); impassable regardless of terrain.
        Obstacles: Set<Pos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpatialInfo =
    /// The empty projection: no room, no tiles, no entities — every entry absent.
    let empty =
        {
            RoomName = None
            Terrain = Map.empty
            TargetPositions = Map.empty
            TargetKinds = Map.empty
            CreepPositions = Map.empty
            Obstacles = Set.empty
        }

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo = { Id: string }

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Energy currently carried.
        Energy: int
        /// Carry capacity still free (0 = full).
        FreeCapacity: int
    }

/// Immutable projection of the current tick's game state; only what decisions need.
type Snapshot =
    {
        Time: int
        Spawns: SpawnInfo list
        /// Room name -> that room's shared spawn-energy bank. A room absent
        /// from the map banks nothing: its spawns wait.
        RoomEnergy: Map<string, RoomEnergy>
        /// Structures that feed spawning, whether or not they currently have room.
        Refillables: RefillableInfo list
        Sources: SourceInfo list
        /// None when no spawn room has an owned controller (should not happen in practice).
        Controller: ControllerInfo option
        ConstructionSites: ConstructionSiteInfo list
        Creeps: CreepInfo list
        /// The spawn room's spatial projection. Always present, possibly
        /// empty — absence is per-entry, never per-projection (ADR 0004).
        Spatial: SpatialInfo
    }

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    | Refill of structureId: string
    | Build of siteId: string
    | Upgrade of controllerId: string

/// What kind of structure a placement Intent asks for.
type StructureKind = | Extension

/// One step of creep movement, engine vocabulary: Top decreases Y.
type Direction =
    | Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | TopLeft

/// Screeps direction constants as `Creep.move` expects them: TOP = 1, then clockwise.
let directionCode =
    function
    | Top -> 1
    | TopRight -> 2
    | Right -> 3
    | BottomRight -> 4
    | Bottom -> 5
    | BottomLeft -> 6
    | Left -> 7
    | TopLeft -> 8

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | PlaceConstructionSite of roomName: string * pos: Pos * kind: StructureKind
    | HarvestSource of creepName: string * sourceId: string
    | TransferEnergyToStructure of creepName: string * structureId: string
    | BuildSite of creepName: string * siteId: string
    | UpgradeController of creepName: string * controllerId: string
    | MoveCreep of creepName: string * direction: Direction
    | SayCreep of creepName: string * message: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>
