module Fabot.Core.Types

/// A creep body part, the engine's full vocabulary. Our own bodies use
/// only Work/Carry/Move today; the rest arrive on hostile creeps, whose
/// parts the Snapshot projects verbatim.
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

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

/// What a built structure is — or what a construction site will become
/// once built. Projection vocabulary, distinct from the Intent vocabulary
/// of placeable kinds (StructureKind).
[<RequireQualifiedAccess>]
type BuiltKind =
    | Spawn
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    /// Any structure kind the decision layer has no rules for yet.
    | Other

/// What the decision layer knows about one energy-hungry structure
/// (spawn, extension, or tower) this tick.
type RefillableInfo =
    {
        Id: string
        /// Energy the structure's store can still take (0 = full).
        FreeCapacity: int
        /// What kind of structure this is — the Refill rank layer's key
        /// (ADR 0010): spawn-feeding kinds are feeding-tier work, towers
        /// surplus-tier. To a creep both are the same transfer.
        Kind: BuiltKind
    }

/// What the decision layer knows about one energy source this tick.
type SourceInfo =
    {
        Id: string
        /// Whether the source holds any energy right now (ADR 0013). A
        /// boolean, not the amount — the Planner pools Harvest only for
        /// a stocked source, and nothing decided reads more than that.
        Stocked: bool
    }

/// What the decision layer knows about the room controller this tick.
type ControllerInfo =
    {
        Id: string
        /// Controller level (RCL); gates how many extensions may exist.
        Level: int
        /// Ticks left on the downgrade timer. A downgrade costs a level
        /// AND zeroes the safe-mode stock, so this is a hard deadline.
        TicksToDowngrade: int
        /// Safe-mode activations banked (one is granted per level-up;
        /// the stock is zeroed by any downgrade).
        SafeModeAvailable: int
        /// True while safe mode is running in the room.
        SafeModeActive: bool
    }

/// A tile coordinate inside a room.
type Pos = { X: int; Y: int }

/// Current and maximum hit points of a repairable structure — what the
/// Repair trigger is judged from (ADR 0010).
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
    /// A dropped energy pile — read only by the pickup reflex, never a
    /// Task target; projected as position and kind alone, no amount.
    | Dropped

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
        /// Tiles holding a built road — built structures only, a road
        /// construction site is not yet a road (ADR 0010).
        Roads: Set<Pos>
        /// Target id -> current/max hits, repairable kinds only (roads
        /// and containers — ADR 0010, ADR 0012); fields nobody decides
        /// on stay out.
        Hits: Map<string, HitsInfo>
        /// Target id -> energy currently stored, containers only (ADR
        /// 0012): the stock the logistics Tasks judge a container by.
        Stores: Map<string, int>
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
            Roads = Set.empty
            Hits = Map.empty
            Stores = Map.empty
        }

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo = { Id: string }

/// What the decision layer knows about one hostile creep in a spawn room
/// this tick: its id and tile — what the fire reflex aims at (ADR 0014) —
/// and its body parts, verbatim — what a hostile can do is decided from
/// what it is made of. Hostiles stay out of the spatial projection: they
/// block no tiles, price no paths, gate no tasks.
type HostileInfo =
    {
        Id: string
        Pos: Pos
        Body: BodyPart list
    }

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Fatigue points still to pay off; a creep with any cannot step
        /// this tick — the engine's move answers ERR_TIRED.
        Fatigue: int
        /// Energy currently carried.
        Energy: int
        /// Carry capacity still free (0 = full).
        FreeCapacity: int
        /// Part count per body part; a part absent from the map is a part
        /// the body does not have. What a creep can do is decided from
        /// what it is made of.
        Body: Map<BodyPart, int>
    }

/// Immutable projection of the current tick's game state; only what decisions need.
type Snapshot =
    {
        Time: int
        Spawns: SpawnInfo list
        /// Room name -> that room's shared spawn-energy bank. A room absent
        /// from the map banks nothing: its spawns wait.
        RoomEnergy: Map<string, RoomEnergy>
        /// Energy-hungry structures (spawn, extension, tower), whether or
        /// not they currently have room.
        Refillables: RefillableInfo list
        Sources: SourceInfo list
        /// None when no spawn room has an owned controller (should not happen in practice).
        Controller: ControllerInfo option
        ConstructionSites: ConstructionSiteInfo list
        Creeps: CreepInfo list
        /// Hostile creeps standing in the spawn rooms this tick.
        Hostiles: HostileInfo list
        /// The spawn room's spatial projection. Always present, possibly
        /// empty — absence is per-entry, never per-projection (ADR 0004).
        Spatial: SpatialInfo
    }

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    /// Take stored energy out of a stocked container (ADR 0012) — the
    /// haul cycle's intake, judged over stores rather than energy's name.
    | Withdraw of containerId: string
    | Refill of structureId: string
    | Build of siteId: string
    | Repair of structureId: string
    | Upgrade of controllerId: string

/// What kind of structure a placement Intent asks for.
type StructureKind =
    | Extension
    | Tower
    | Road
    | Container
    | Storage

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

/// Every BodyPart — the closed set, for building tables over the vocabulary.
let allBodyParts = [ Work; Carry; Move; Attack; RangedAttack; Heal; Claim; Tough ]

/// Screeps body-part strings as the engine spells them, in `spawnCreep`
/// bodies and `creep.body` entries alike — the one place the spelling
/// lives (its reverse is derived from this table, never written twice).
let partName =
    function
    | Work -> "work"
    | Carry -> "carry"
    | Move -> "move"
    | Attack -> "attack"
    | RangedAttack -> "ranged_attack"
    | Heal -> "heal"
    | Claim -> "claim"
    | Tough -> "tough"

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
    | WithdrawEnergyFromStructure of creepName: string * structureId: string
    | BuildSite of creepName: string * siteId: string
    | RepairStructure of creepName: string * structureId: string
    | UpgradeController of creepName: string * controllerId: string
    | PickupEnergy of creepName: string * resourceId: string
    | MoveCreep of creepName: string * direction: Direction
    | SayCreep of creepName: string * message: string
    | ActivateSafeMode of controllerId: string
    | FireTower of towerId: string * hostileId: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>

/// The census-keyed plan memo (ADR 0017): the census signature beside the
/// plans derived from exactly that census — the Layout's site Intents and
/// the hauler quota. Held by the host in heap across ticks, never written
/// to Memory: a global reset discards it and the next tick recomputes from
/// scratch. Same census, same plan, so reuse never changes behaviour.
type PlanMemo =
    {
        Signature: string
        SiteIntents: Intent list
        HaulerQuota: int
    }

/// What decided a fresh match: the first comparison that separated the
/// winning Task from its closest rival — rank tier, then travel cost, then
/// current load — or the tie-break when none did (pool order), or the fact
/// that no rival existed at all.
[<RequireQualifiedAccess>]
type MatchFactor =
    | OnlyCandidate
    | Rank
    | TravelCost
    | Load
    | PoolOrder

/// Why a remembered assignment was released: its Task left the pool, the
/// creep can no longer usefully work it (body parts or energy state), the
/// Task's worker cap was already full, or its Work Area is unreachable or
/// empty (ADR 0002).
[<RequireQualifiedAccess>]
type ReleaseReason =
    | TaskGone
    | Inapplicable
    | OverCapacity
    | Unreachable

/// Why an unassigned creep got nothing: the pool was empty, no Task fit
/// its body or energy state, every fitting Task's worker cap was full, or
/// every fitting Task with room had an unreachable Work Area. Reports how
/// far the best Task got through the matching gates.
[<RequireQualifiedAccess>]
type IdleReason =
    | NoTasks
    | NoneApplicable
    | NoneFree
    | NoneReachable

/// Why a Task in the pool was rejected for a creep, in a verbose scoring:
/// it did not fit the creep's body or energy state, its worker cap was
/// already full, or its Work Area is unreachable — the matching gates, in
/// the order they are tried.
[<RequireQualifiedAccess>]
type RejectReason =
    | Inapplicable
    | CapacityFull
    | Unreachable

/// One row of a verbose scoring: a Task in the pool, either scored on the
/// full matching key — rank tier, travel cost, current load — or rejected
/// at the first gate it failed. The answer to "why *not* that Task".
[<RequireQualifiedAccess>]
type Candidate =
    | Scored of task: string * rank: int * cost: int * load: int
    | Rejected of task: string * reason: RejectReason

/// The reasoned outcome a decision step returns beside its decision — data,
/// never a log line (ADR 0009). The Matcher speaks at conclusion level:
/// which Task won a creep and what decided it, a remembered assignment kept
/// (anti-thrash) as distinct from a fresh match, a release with its reason,
/// or why nothing was applicable. The Resolver speaks only when something
/// became of a creep's movement: grounded by fatigue (ADR 0008), yielded —
/// settled off its preferred tile, naming the counterpart creep that holds
/// it — or rerouted, detoured by the occupancy surcharge. A creep that
/// simply steps toward its Work Area says nothing: conclusion level means
/// events, not every step. Tasks are named by task id. A creep on the
/// verbose list additionally gets a Scoring Verdict: the whole pool as
/// Candidates, judged against the state its match was decided from.
[<RequireQualifiedAccess>]
type Verdict =
    | Matched of creep: string * task: string * factor: MatchFactor
    | Kept of creep: string * task: string
    | Released of creep: string * task: string * reason: ReleaseReason
    | Unassigned of creep: string * reason: IdleReason
    | Scoring of creep: string * candidates: Candidate list
    | Grounded of creep: string
    | Yielded of creep: string * counterpart: string
    | Rerouted of creep: string

/// What one tick of deciding returns: the Intents to execute, the
/// Assignments to remember for next tick, the plan memo to hold in heap
/// for next tick (ADR 0017), and the Verdicts explaining them (ADR 0009).
type Decision =
    {
        Intents: Intent list
        Assignments: Assignments
        Memo: PlanMemo
        Verdicts: Verdict list
    }
