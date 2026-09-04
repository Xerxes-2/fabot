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
/// of placeable kinds (StructureKind): every placeable kind widens into
/// one of these (`builtKindOfPlaceable`), never the other way.
[<RequireQualifiedAccess>]
type BuiltKind =
    | Spawn
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    /// A link. Projection-only: no counterpart in the placeable kinds,
    /// because the Layout holds a footing for one but never places it
    /// (ADR 0022).
    | Link
    /// A rampart. No Task targets one and the Layout never places one,
    /// but walkability must answer for it: a creep may stand on a
    /// rampart, and folding it into Other would make every kind the
    /// decision layer does not model walkable with it.
    | Rampart
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
        /// Ticks until the source holds energy again — its restock
        /// (ADR 0013, widened by ADR 0025); 0 while it holds energy now.
        /// Not the amount: the one time fact a decision reads about a
        /// source, so that a drained source's Harvest can be judged at
        /// the creep's arrival rather than at the current tick. Stocked
        /// is a restock of zero, never a field of its own.
        TicksToRestock: int
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

/// Screeps range: Chebyshev distance between two tiles. The one
/// definition — the Atlas's geometry, the two hostile reflexes and the
/// Raid log's closest approach all measure with it.
let range (a: Pos) (b: Pos) = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

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
        /// Target id -> energy currently stored, on the containers (ADR
        /// 0012) and the Storage (ADR 0023): the stock the logistics
        /// Tasks judge a store by.
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
/// its body parts, verbatim, because what a hostile can do is decided
/// from what it is made of, and its owner, which the Raid log's roster
/// reads (ADR 0028). Hostiles stay out of the spatial projection: they
/// block no tiles, price no paths, gate no tasks.
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username
        /// ("Invader" for the NPCs). The field the projection grew the
        /// tick a reader for it existed (ADR 0007's rule, ADR 0028): the
        /// Raid log's roster is attribution, and attribution is a name.
        /// No reflex reads it.
        Owner: string
        Pos: Pos
        Body: BodyPart list
    }

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Ticks the creep still has to live — the engine counts down from
        /// CREEP_LIFE_TIME. A creep still spawning is outside the
        /// projection, so a projected creep always carries a real count.
        /// The fact, not the judgement: whether it is expiring is this
        /// count measured against the lead its replacement needs
        /// (ADR 0026).
        TicksToLive: int
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
    /// Take stored energy out of a stocked container (ADR 0012), or out of
    /// the Storage a tier below them (ADR 0023) — the haul cycle's intake,
    /// judged over stores rather than energy's name.
    | Withdraw of storeId: string
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

/// Every BodyPart — the closed set, for building tables over the
/// vocabulary. A literal, and so not compiler-checked: a part added to
/// the union has to be added here by hand. A successor chain does not
/// close that — the compiler checks such a function for exhaustiveness,
/// never for reachability, so a dangling `| NewPart -> None` compiles
/// clean and still leaves the list short. What closes it is `Core.Tests`,
/// which enumerates the union itself and fails when this list is short.
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

/// Every BuiltKind the engine spells — the modelled set, not the engine's
/// whole structure vocabulary, for building tables over the kinds. Every
/// spelling outside it classifies to Other, which is why Other is not one
/// of them: it is the absence of a modelled kind, never a kind with a
/// spelling of its own. A literal, and so not compiler-checked: a kind
/// added to the union has to be added here by hand, and `Core.Tests`
/// closes that the same way it does for `allBodyParts`.
let allBuiltKinds =
    [
        BuiltKind.Spawn
        BuiltKind.Extension
        BuiltKind.Tower
        BuiltKind.Road
        BuiltKind.Container
        BuiltKind.Storage
        BuiltKind.Link
        BuiltKind.Rampart
    ]

/// Screeps STRUCTURE_* strings as the engine spells them, in `structureType`
/// on structures and construction sites alike and in `createConstructionSite`
/// — the one place the spelling lives (its reverse is derived from this
/// table, never written twice). Other spells to nothing: it is the absence
/// of a modelled kind, so it stays out of `allBuiltKinds` and the empty
/// string never reaches the engine.
let builtKindName =
    function
    | BuiltKind.Spawn -> "spawn"
    | BuiltKind.Extension -> "extension"
    | BuiltKind.Tower -> "tower"
    | BuiltKind.Road -> "road"
    | BuiltKind.Container -> "container"
    | BuiltKind.Storage -> "storage"
    | BuiltKind.Link -> "link"
    | BuiltKind.Rampart -> "rampart"
    | BuiltKind.Other -> ""

/// The built kind a placement Intent's kind names: the one crossing
/// between the Intent vocabulary and the projection's, stated in Core
/// beside both unions rather than respelled wherever the two meet — the
/// Executor's site placement and any projection built on the .NET side
/// read the same widening (#75). Every placeable kind is a built kind;
/// the reverse does not hold — a Link is projected but never placed (ADR
/// 0022) — so the crossing runs this way only.
let builtKindOfPlaceable =
    function
    | Extension -> BuiltKind.Extension
    | Tower -> BuiltKind.Tower
    | Road -> BuiltKind.Road
    | Container -> BuiltKind.Container
    | Storage -> BuiltKind.Storage

/// The kinds Refill keeps fed (ADR 0010): the spawn-energy feeders and the
/// towers, the structures the Snapshot projects as Refillables. The
/// controller container and the Storage are Refill targets too, but the
/// Planner pools them off the projection's stores (ADR 0012, ADR 0023), so
/// they are not one of these.
let isRefillable =
    function
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower -> true
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Storage
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The kinds Repair keeps whole (ADR 0010, ADR 0012) — the decaying ones,
/// and so the only kinds whose hits the projection carries at all. The
/// Storage is deliberately not one: it does not decay, so nothing repairs
/// it (ADR 0023).
let isRepairable =
    function
    | BuiltKind.Road
    | BuiltKind.Container -> true
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The kinds whose stored energy enters the projection: the containers,
/// whose stock the logistics Tasks judge (ADR 0012), and the Storage,
/// whose Withdraw and Refill tiers read the same field (ADR 0023) — a
/// standing Storage's store is read exactly like a container's.
let isStored =
    function
    | BuiltKind.Container
    | BuiltKind.Storage -> true
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower
    | BuiltKind.Road
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The kinds a creep can stand on; every other kind blocks its tile
/// (Screeps OBSTACLE_OBJECT_TYPES). Other is not walkable: a kind the
/// decision layer has no rules for is the one thing that must not quietly
/// open a tile, which is why Rampart is a case of its own.
let isWalkable =
    function
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Rampart -> true
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Link
    | BuiltKind.Other -> false

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

/// The reverse of a wire-name table, derived from the table itself: each
/// spelling is written once, in the name table, and the decoder reads
/// back what falls out of it. A name the vocabulary does not have reads
/// as None — the caller decides what a miss costs. The one builder: the
/// vocabularies below, the serialization shell's part table and the test
/// that round-trips them all call this, so no reverse is hand-rolled a
/// second time.
let reverseOf toName cases =
    let byName = cases |> List.map (fun case -> toName case, case) |> Map.ofList
    fun name -> Map.tryFind name byName

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

/// The wire spelling of each MatchFactor, in the observe channel's Memory
/// subtree (ADR 0009) — the one place the spelling lives, beside the
/// union it spells, the way `partName` holds the engine's part spelling.
let matchFactorName =
    function
    | MatchFactor.OnlyCandidate -> "only-candidate"
    | MatchFactor.Rank -> "rank"
    | MatchFactor.TravelCost -> "travel-cost"
    | MatchFactor.Load -> "load"
    | MatchFactor.PoolOrder -> "pool-order"

/// The MatchFactor a wire name spells, or None for a name this vocabulary
/// does not have. The case list is a literal, so a case added without its
/// entry decodes to nothing; `Core.Tests` round-trips the union itself and
/// fails on exactly that.
let matchFactorOf =
    reverseOf
        matchFactorName
        [
            MatchFactor.OnlyCandidate
            MatchFactor.Rank
            MatchFactor.TravelCost
            MatchFactor.Load
            MatchFactor.PoolOrder
        ]

/// Why a remembered assignment was released: its Task left the pool, the
/// creep can no longer usefully work it (body parts or energy state), the
/// Task's worker cap was already full, its Work Area is unreachable or
/// empty (ADR 0002), or its time has not come — the creep's walk no
/// longer covers a drained source's restock wait (ADR 0025), which is how
/// a creep beside a dry rock leaves it now that the Task stays pooled.
[<RequireQualifiedAccess>]
type ReleaseReason =
    | TaskGone
    | Inapplicable
    | OverCapacity
    | Unreachable
    | TooEarly

/// The wire spelling of each ReleaseReason, as `matchFactorName` is
/// MatchFactor's.
let releaseReasonName =
    function
    | ReleaseReason.TaskGone -> "task-gone"
    | ReleaseReason.Inapplicable -> "inapplicable"
    | ReleaseReason.OverCapacity -> "over-capacity"
    | ReleaseReason.Unreachable -> "unreachable"
    | ReleaseReason.TooEarly -> "too-early"

/// The ReleaseReason a wire name spells, or None for a name this
/// vocabulary does not have.
let releaseReasonOf =
    reverseOf
        releaseReasonName
        [
            ReleaseReason.TaskGone
            ReleaseReason.Inapplicable
            ReleaseReason.OverCapacity
            ReleaseReason.Unreachable
            ReleaseReason.TooEarly
        ]

/// Why an unassigned creep got nothing: the pool was empty, no Task fit
/// its body or energy state, every fitting Task's worker cap was full,
/// every fitting Task with room had an unreachable Work Area, or every
/// Task it could otherwise have taken is one whose time has not come
/// (ADR 0025). Reports the deepest matching gate any Task reached, so a
/// creep waiting out a drained source's restock says exactly that rather
/// than claiming nothing fit its body.
[<RequireQualifiedAccess>]
type IdleReason =
    | NoTasks
    | NoneApplicable
    | NoneFree
    | NoneReachable
    | NoneInTime

/// The wire spelling of each IdleReason, as `matchFactorName` is
/// MatchFactor's.
let idleReasonName =
    function
    | IdleReason.NoTasks -> "no-tasks"
    | IdleReason.NoneApplicable -> "none-applicable"
    | IdleReason.NoneFree -> "none-free"
    | IdleReason.NoneReachable -> "none-reachable"
    | IdleReason.NoneInTime -> "none-in-time"

/// The IdleReason a wire name spells, or None for a name this vocabulary
/// does not have.
let idleReasonOf =
    reverseOf
        idleReasonName
        [
            IdleReason.NoTasks
            IdleReason.NoneApplicable
            IdleReason.NoneFree
            IdleReason.NoneReachable
            IdleReason.NoneInTime
        ]

/// Why a Task in the pool was rejected for a creep, in a verbose scoring:
/// it did not fit the creep's body or energy state, its worker cap was
/// already full, its Work Area is unreachable, or its time has not come —
/// the matching gates, in the order they are tried. The last is its own
/// reason rather than Inapplicable (ADR 0025): the body and the energy
/// state fit, only the arrival doesn't, and the transition log would lie.
[<RequireQualifiedAccess>]
type RejectReason =
    | Inapplicable
    | CapacityFull
    | Unreachable
    | TooEarly

/// The wire spelling of each RejectReason, as `matchFactorName` is
/// MatchFactor's.
let rejectReasonName =
    function
    | RejectReason.Inapplicable -> "inapplicable"
    | RejectReason.CapacityFull -> "capacity-full"
    | RejectReason.Unreachable -> "unreachable"
    | RejectReason.TooEarly -> "too-early"

/// The RejectReason a wire name spells, or None for a name this
/// vocabulary does not have.
let rejectReasonOf =
    reverseOf
        rejectReasonName
        [
            RejectReason.Inapplicable
            RejectReason.CapacityFull
            RejectReason.Unreachable
            RejectReason.TooEarly
        ]

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
