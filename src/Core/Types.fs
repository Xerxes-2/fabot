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
        /// Energy available for spawning in the spawn's room (spawn + extensions).
        EnergyAvailable: int
        IsSpawning: bool
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

/// What the decision layer knows about the spawn room's local geography,
/// sufficient for construction placement.
type PlacementInfo =
    {
        RoomName: string
        SpawnPos: Pos
        /// Terrain-walkable tiles within the planning window around the spawn.
        Walkable: Set<Pos>
        /// Tiles already taken by structures or construction sites.
        Occupied: Set<Pos>
        /// Extensions already built in the room.
        BuiltExtensions: int
        /// Extension construction sites already placed.
        PendingExtensions: int
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
        /// Structures that feed spawning, whether or not they currently have room.
        Refillables: RefillableInfo list
        Sources: SourceInfo list
        /// None when no spawn room has an owned controller (should not happen in practice).
        Controller: ControllerInfo option
        ConstructionSites: ConstructionSiteInfo list
        Creeps: CreepInfo list
        /// None when there is no spawn to plan around.
        Placement: PlacementInfo option
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

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | PlaceConstructionSite of roomName: string * pos: Pos * kind: StructureKind
    | HarvestSource of creepName: string * sourceId: string
    | TransferEnergyToStructure of creepName: string * structureId: string
    | BuildSite of creepName: string * siteId: string
    | UpgradeController of creepName: string * controllerId: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>
