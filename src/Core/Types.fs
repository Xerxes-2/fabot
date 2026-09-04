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
    /// A rampart, the walkable defence over the Keep and the Posts (ADR
    /// 0034). Walkability answers for it before anything else does: a
    /// creep may stand on a rampart, and folding it into Other would make
    /// every kind the decision layer does not model walkable with it.
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
        /// Target id -> current/max hits, repairable kinds only — the
        /// decaying roads and containers (ADR 0010, ADR 0012), the Keep
        /// and our own ramparts (ADR 0034); fields nobody decides on stay
        /// out. Each kind is judged against its own whole line
        /// (`wholeLine`), and three readers now share these hits: the
        /// Repair pool, the safe-mode reflex and the Raid log's damage.
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
    /// Getting out of a Threat's Reach (ADR 0033). The one Task with no
    /// target and no action: its Work Area is the tiles no Threat can
    /// hurt, and the Emitter issues movement for it and nothing else.
    | Flee

/// What kind of structure a placement Intent asks for.
type StructureKind =
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    /// A rampart, over the Keep and the Posts (ADR 0034). The one
    /// defensive kind the Layout places, and the only placeable kind that
    /// goes on a tile something already stands on.
    | Rampart

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
    | Rampart -> BuiltKind.Rampart

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

/// The Keep (ADR 0034): the structures worth defending — the spawn, the
/// tower and the Storage. One list, three rules hang off it: a rampart
/// covers each of them, Repair keeps each at full hits, and any one of
/// them below full while a hostile stands in the room fires the safe-mode
/// reflex. The Posts are ramparted with the Keep but are not of it: a
/// container's hits never spend the stock.
let isKeep =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The kinds a raid's damage is charged on (ADR 0034): the Keep and the
/// ramparts that cover it. Not the roads and the containers, whose hits
/// the projection also carries — a chewed road is the colony's ordinary
/// decay, and charging it would drown the number the Raid log exists for.
/// Enumerated rather than written as "the Keep or a rampart" so that a
/// kind added to the union has to answer this question too.
let isDefence =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Rampart -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Other -> false

/// The kinds whose projection has to ask the engine who owns them: every
/// ownable kind whose hits a decision reads (ADR 0034). A structure of
/// another owner left standing in a room we took is neither ours to repair
/// nor ours to charge a raid's damage on, and "it stands in our spawn
/// room" is not the same fact as "it is ours". The decaying kinds are
/// deliberately not among them: a road and a container have no owner in
/// the engine at all, so asking would drop every one of them.
let needsOwner =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Rampart -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Other -> false

/// Where a kind is whole — which of the three rules judges its hits (ADR
/// 0034), never the numbers themselves: the fraction and the floor are the
/// Repair pool's tunables, stated where the pool that reads them is.
[<RequireQualifiedAccess>]
type WholeLine =
    /// A fraction of max hits: the decaying kinds (ADR 0010) — a road and
    /// a container are hungry below half of max and whole at it.
    | Fraction
    /// A fixed floor of hits: the rampart (ADR 0034). Half of max is the
    /// wrong shape for a structure whose max is three million at RCL4 and
    /// grows to three hundred — it would be hungry forever.
    | Floor
    /// Full hits: the Keep (ADR 0034). It does not decay, so below max
    /// means it was damaged and nothing else — and the safe-mode arm
    /// reads that same fact off the same hits.
    | Full

/// The line a kind is whole at, or None for a kind Repair never touches —
/// the extensions, a link, and every kind the decision layer does not
/// model (ADR 0010, widened by ADR 0034). The repairable kinds are exactly
/// the kinds whose hits the projection carries at all: fields nobody
/// decides on stay out.
let wholeLine =
    function
    | BuiltKind.Road
    | BuiltKind.Container -> Some WholeLine.Fraction
    | BuiltKind.Rampart -> Some WholeLine.Floor
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage -> Some WholeLine.Full
    | BuiltKind.Extension
    | BuiltKind.Link
    | BuiltKind.Other -> None

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

/// A body's fatigue factor (ADR 0006): the parts that generate fatigue
/// when moving and the Move parts that pay it off. Terrain weight scales
/// by their ratio to price travel in cost units — half-ticks under the
/// engine-native weights (ADR 0010). The Atlas's own arithmetic, spelled
/// out here because the walk table below is keyed on it and outlives the
/// Atlas that fills it (ADR 0032).
type FatigueFactor = { FatigueParts: int; MoveParts: int }

/// The spawn-origin walk table (ADR 0032): the traffic-blind flood out of
/// the tiles beside a spawner, for a body's fatigue factor, as whole-tick
/// distances per tile index (ADR 0026, ADR 0029) — the half of a lead paid
/// after the cast. Filled on demand by the Atlas as leads are priced, and
/// handed to the next tick's Atlas while the census signature holds: every
/// input the flood reads is in the census, so it runs once per census
/// rather than once per tick. Mutable, and heap-only like the memo that
/// carries it.
type WalkTable = System.Collections.Generic.Dictionary<Pos * FatigueFactor, int[]>

/// What a Link footing is held beside (ADR 0022, ADR 0027): each planned
/// source container, the controller container, the Storage. The Layout
/// knows a target's kind by construction — the target list is assembled
/// from exactly those three — and carries it so a footing the fold cannot
/// serve names the guarantee that was lost, not merely a tile.
[<RequireQualifiedAccess>]
type FootingKind =
    | SourceContainer
    | ControllerContainer
    | Storage

/// A footing target the Layout could not serve (#77): every tile within
/// range 1 of it was a trunk, another target, already taken by a footing,
/// or not buildable at all, so nothing was reserved for it. Recorded
/// rather than dropped — one footing per target is a guarantee, and a
/// guarantee that can degrade in silence is not one.
type UnservedFooting = { Target: Pos; Kind: FootingKind }

/// A footing target the Layout served (#106): the tile it reserved, beside
/// the target that tile is held for and that target's kind. The served
/// counterpart of `UnservedFooting`, which names a target and a kind and
/// no tile because there was none.
///
/// The pairing rather than the bare set of tiles, because the set is a
/// one-line projection of the pairing and the reverse is a search: a
/// reservation the bot never emits can otherwise only be cross-checked by
/// a second derivation (ADR 0035), and handing back tiles alone would
/// leave that derivation to be written by hand. The fold holds the target
/// and the kind in scope at the instant it picks the tile, so carrying
/// them costs nothing.
///
/// Two records rather than one whose tile is optional: only the unserved
/// half crosses the Memory boundary, as the layout record (ADR 0035), and
/// an optional tile would make every reader of either half ask which case
/// it holds — the partition is what the two names say.
type ServedFooting =
    {
        Target: Pos
        Kind: FootingKind
        Tile: Pos
    }

/// The census-keyed plan memo (ADR 0017): the census signature beside the
/// plans derived from exactly that census — the Layout's site Intents,
/// the footings it placed and the ones it could not, the hauler quota,
/// and the spawn walks behind the leads (ADR 0032). Held by the host in
/// heap across ticks, never written to Memory: a global reset discards it
/// and the next tick recomputes from scratch. Same census, same plan, so
/// reuse never changes behaviour.
type PlanMemo =
    {
        Signature: string
        SiteIntents: Intent list
        /// The footing targets this plan left unserved (#77), derived from
        /// the same census as the site Intents and recomputed with them.
        /// Empty is the healthy answer and rides here all the same: the
        /// App writes it every tick, because a channel that says nothing
        /// when nothing is lost cannot be told from one that is not there.
        UnservedFootings: UnservedFooting list
        /// The footings this plan placed (#106), each naming its target,
        /// that target's kind and the tile reserved for it — derived from
        /// the same census as the site Intents and recomputed with them.
        /// No Intent ever names a link (ADR 0022) and this never crosses
        /// the Memory boundary, so the heap is the only place the tiles
        /// the fold reserved are observable at all: the whole-room
        /// invariant that a footing is off every trunk, off every target
        /// and off every other footing reads them here (ADR 0036).
        ServedFootings: ServedFooting list
        HaulerQuota: int
        /// The walks flooded under this signature, filled through the tick
        /// by the Atlas the table was handed to. Dropped whole when the
        /// signature moves — the Layout's own granularity, never per entry:
        /// a moved signature may have moved the weights or the body the
        /// walk is priced for, and telling which is a dependency tracker
        /// this memo deliberately does not have.
        Walks: WalkTable
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

/// The same reversal for a vocabulary whose cases carry numbers beside
/// their name (#88). The entries are the cases' own constructors rather
/// than the cases, so each spelling is still written once — the name is
/// read off the case a constructor builds from a sample payload — and the
/// numbers the wire actually carried are handed back in on the way out: a
/// bare tag ignores them, a case that needs them reads as nothing without
/// them. So a name whose numbers are missing decodes to None exactly as
/// an unknown name does, and the caller decides what that costs rather
/// than restating a number nobody wrote.
let reverseCarrying toName sample (builders: ('p option -> 'a option) list) =
    let byName =
        builders
        |> List.choose (fun build ->
            build (Some sample) |> Option.map (fun case -> toName case, build))
        |> Map.ofList

    fun payload name -> Map.tryFind name byName |> Option.bind (fun build -> build payload)

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

/// Why a remembered assignment was released: its Task left the pool, a
/// Threat's Reach has taken the whole of its Work Area (ADR 0033) — the
/// release a raid writes to the transition log, and the reason asked
/// first, because a Task with nowhere to stand is gone for this creep
/// however well its body fits — the creep can no longer usefully work it
/// (body parts or energy state), the Task's worker cap was already full,
/// its Work Area is unreachable or empty (ADR 0002), or its time has not
/// come — the creep's walk no
/// longer covers a drained source's restock wait (ADR 0025), which is how
/// a creep beside a dry rock leaves it now that the Task stays pooled.
/// That last reason carries the two numbers the gate compared, the walk
/// and the wait (#88): a creep released mid-trip owes the same
/// explanation as a candidate rejected at the gate, and since ADR 0029
/// the walk cannot be recovered by halving anything.
[<RequireQualifiedAccess>]
type ReleaseReason =
    | TaskGone
    | Inapplicable
    | OverCapacity
    | Unreachable
    | Threatened
    | TooEarly of walk: int * wait: int

/// The wire spelling of each ReleaseReason, as `matchFactorName` is
/// MatchFactor's.
let releaseReasonName =
    function
    | ReleaseReason.TaskGone -> "task-gone"
    | ReleaseReason.Inapplicable -> "inapplicable"
    | ReleaseReason.OverCapacity -> "over-capacity"
    | ReleaseReason.Unreachable -> "unreachable"
    | ReleaseReason.Threatened -> "threatened"
    | ReleaseReason.TooEarly _ -> "too-early"

/// The numbers a ReleaseReason carries beside its wire name, or None for
/// a bare tag. The encoder's half of what `releaseReasonOf` reads back,
/// beside the union the way the name table is: a case's payload is spelt
/// out in one place, not once per row shape that carries it.
let releaseReasonNumbers =
    function
    | ReleaseReason.TooEarly(walk, wait) -> Some(walk, wait)
    | ReleaseReason.TaskGone
    | ReleaseReason.Inapplicable
    | ReleaseReason.OverCapacity
    | ReleaseReason.Unreachable
    | ReleaseReason.Threatened -> None

/// The ReleaseReason a wire name spells for the numbers the wire carried
/// beside it, or None for a name this vocabulary does not have — and for
/// `too-early` with no numbers to be about.
let releaseReasonOf =
    reverseCarrying
        releaseReasonName
        (0, 0)
        [
            (fun _ -> Some ReleaseReason.TaskGone)
            (fun _ -> Some ReleaseReason.Inapplicable)
            (fun _ -> Some ReleaseReason.OverCapacity)
            (fun _ -> Some ReleaseReason.Unreachable)
            (fun _ -> Some ReleaseReason.Threatened)
            Option.map ReleaseReason.TooEarly
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
/// a Threat's Reach has taken the whole of its Work Area (ADR 0033), it
/// did not fit the creep's body or energy state, its worker cap was
/// already full, its Work Area is unreachable, or its time has not come —
/// the matching gates, in the order they are tried. The Reach is asked
/// ahead of the body because it is not a fact about the creep at all: an
/// area nobody may stand in is no Task for anyone. The last is its own
/// reason rather than Inapplicable (ADR 0025): the body and the energy
/// state fit, only the arrival doesn't, and the transition log would lie.
/// It carries the walk and the wait the gate compared (#88) — the scored
/// row is not widened for it, because only a rejected row raises the
/// question of how long the creep still has to wait.
[<RequireQualifiedAccess>]
type RejectReason =
    | Inapplicable
    | CapacityFull
    | Unreachable
    | Threatened
    | TooEarly of walk: int * wait: int

/// The wire spelling of each RejectReason, as `matchFactorName` is
/// MatchFactor's.
let rejectReasonName =
    function
    | RejectReason.Inapplicable -> "inapplicable"
    | RejectReason.CapacityFull -> "capacity-full"
    | RejectReason.Unreachable -> "unreachable"
    | RejectReason.Threatened -> "threatened"
    | RejectReason.TooEarly _ -> "too-early"

/// The numbers a RejectReason carries, as `releaseReasonNumbers` is
/// ReleaseReason's.
let rejectReasonNumbers =
    function
    | RejectReason.TooEarly(walk, wait) -> Some(walk, wait)
    | RejectReason.Inapplicable
    | RejectReason.CapacityFull
    | RejectReason.Unreachable
    | RejectReason.Threatened -> None

/// The RejectReason a wire name spells for the numbers the wire carried
/// beside it, as `releaseReasonOf` is ReleaseReason's.
let rejectReasonOf =
    reverseCarrying
        rejectReasonName
        (0, 0)
        [
            (fun _ -> Some RejectReason.Inapplicable)
            (fun _ -> Some RejectReason.CapacityFull)
            (fun _ -> Some RejectReason.Unreachable)
            (fun _ -> Some RejectReason.Threatened)
            Option.map RejectReason.TooEarly
        ]

/// The wire spelling of each FootingKind, on the Layout channel's Memory
/// leaf (#77), as `matchFactorName` is MatchFactor's. Not a Verdict
/// vocabulary — the Layout speaks no Verdicts, which is the whole reason
/// its losses need a channel — but the same rule: one spelling, written
/// once, round-tripped against the union itself by `Core.Tests`.
let footingKindName =
    function
    | FootingKind.SourceContainer -> "source-container"
    | FootingKind.ControllerContainer -> "controller-container"
    | FootingKind.Storage -> "storage"

/// The FootingKind a wire name spells, or None for a name this vocabulary
/// does not have.
let footingKindOf =
    reverseOf
        footingKindName
        [
            FootingKind.SourceContainer
            FootingKind.ControllerContainer
            FootingKind.Storage
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
