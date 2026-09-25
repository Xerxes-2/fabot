/// What a tick produces: the described `Intent`s the Executor performs, the
/// `Assignments` carried to the next tick, and the plan memo with the Layout
/// rows (footings, trunks, containers) it is keyed on.
[<AutoOpen>]
module Fabot.Core.Types.Intents

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | PlaceConstructionSite of tile: RoomPos * kind: StructureKind
    /// The dig act, over either rock the `Harvest` Task can name: a source, or
    /// a Thorium deposit under an extractor of ours. The Intent's name is the
    /// frozen one (a raid log and every `observe` channel reads it) and the
    /// field's is the honest one.
    | HarvestSource of creepName: string * rockId: string
    /// The transfer act, over the resource the refill Task names. The name is
    /// frozen, as `HarvestSource`'s is.
    | TransferEnergyToStructure of creepName: string * structureId: string * resource: Resource
    /// The withdraw act: a container, the Storage, a tombstone or a ruin
    /// (#183), and the Task's own resource. `None` for the amount takes as
    /// much as the body has room for; the delivery's 999-unit load is the one
    /// Withdraw that names a number. Resource and amount are one argument list
    /// on the engine's own `withdraw`.
    | WithdrawFromStore of
        creepName: string *
        storeId: string *
        resource: Resource *
        amount: int option
    | BuildSite of creepName: string * siteId: string
    | RepairStructure of creepName: string * structureId: string
    | UpgradeController of creepName: string * controllerId: string
    /// The reserve act: a CLAIM body standing beside a neutral controller
    /// pushes its reservation up by one tick per CLAIM part. Range 1.
    | ReserveController of creepName: string * controllerId: string
    /// The claim act: a CLAIM body standing beside a neutral controller takes
    /// the room for this player. Range 1.
    | ClaimController of creepName: string * controllerId: string
    /// The re-claim act: a CLAIM body standing beside the sector Reactor takes
    /// it for this player. Range 1, and a custom intent of the season mod
    /// (`creep.claimReactor.js`): no cooldown, no ownership precondition,
    /// `launchTime` untouched.
    | ClaimReactor of creepName: string * reactorId: string
    /// The pickup act: a creep within range 1 of a dropped pile takes as much
    /// of it as its store has room for. Named for the pile and not for energy
    /// (#311): the engine's `pickup` takes no resource argument, and this
    /// spelling had no reader outside this module — the persisted Task id is
    /// `Facts.taskId`'s `pickup:<id>`, unchanged.
    | PickupPile of creepName: string * resourceId: string
    /// One creep writing the colony's signature onto a controller it stands
    /// beside. Carries the text so the Executor needs no declaration of its
    /// own: what a room says is a human's to write (`Colony.signature`), and
    /// the shell's job is to put it there.
    | SignController of creepName: string * controllerId: string * text: string
    /// The melee act: a body with ATTACK parts standing within range 1 of a
    /// hostile creep deals `Engine.attackPower` a part. The one Intent that
    /// names a creep this colony does not own — by id, as the fire reflex's
    /// target is.
    | AttackCreep of creepName: string * hostileId: string
    /// The heal act: a body with HEAL parts restores `Engine.healPower` a part
    /// to a creep of ours within range 1, itself included. Heal suppresses
    /// attack in the engine, so it is emitted only with no melee target in
    /// range. Both creeps are named, the target by name: an Intent whose
    /// target rode implicitly on the actor would say nothing in the
    /// Executor's own failure line.
    | HealCreep of creepName: string * targetName: string
    /// The same act at range 2..3 for `Engine.rangedHealPower` a part (#409).
    /// The engine lets `heal` suppress it, and it suppresses `attack`.
    | RangedHealCreep of creepName: string * targetName: string
    /// `rangedAttack` (#411): `Engine.rangedAttackPower` a part on one hostile
    /// within range 3, by id as `AttackCreep`'s is. It stands beside heal,
    /// attack and harvest in the engine's table, and not beside ranged heal,
    /// repair or build.
    | RangedAttackCreep of creepName: string * hostileId: string
    | MoveCreep of creepName: string * direction: Direction
    | SayCreep of creepName: string * message: string
    | ActivateSafeMode of controllerId: string
    | FireTower of towerId: string * hostileId: string
    /// A tower putting hits back on a creep of ours (#410), named by name as
    /// `HealCreep`'s target is. The engine runs a tower's heal before its
    /// attack and drops the attack, so the two are never planned together.
    | HealWithTower of towerId: string * targetName: string
    /// A terminal shipping a resource to another room's terminal (#349). The
    /// amount and the destination are named here for `HealCreep`'s reason, and
    /// this is the one intent whose refusal is expected in normal running: a
    /// terminal is on cooldown for ten ticks after every send, and `RoomFacts`
    /// carries no cooldown, so about a tenth of the asks are refused.
    | SendFromTerminal of
        terminalId: string *
        resource: Resource *
        amount: int *
        destination: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>

/// A body's fatigue factor: the parts that generate fatigue when moving and
/// the Move parts that pay it off. Terrain weight scales by their ratio to
/// price travel in cost units.
type FatigueFactor = { FatigueParts: int; MoveParts: int }

/// ADR-0032. The spawn-origin walk table: the traffic-blind walk out of the
/// tiles beside a spawner, for a body's fatigue factor, as whole-tick
/// distances per tile index of one room. Filled on demand by the Atlas and
/// handed to the next tick's while the census signature holds. Mutable, and
/// heap-only like the memo carrying it.
type WalkTable = System.Collections.Generic.Dictionary<Pos * FatigueFactor * string, int[]>

/// What a step costs a body, as the flood prices it. It lives here beside the
/// tables keyed on it rather than in `Grid`: a record the host holds across
/// ticks cannot name a type declared in a module compiled after it.
///
/// The split that matters to every reader is traffic: `TravelCost` prices
/// this tick's standing creeps and the other two are blind to them
/// (`Grid.pricingOf` substitutes `noTraffic`). That split is the near leg's
/// alone; the far leg of a cross-room price floods over empty ground under
/// every pricing.
type Pricing =
    /// Travel cost's units — half-ticks, floored at one unit a step, with
    /// the occupancy surcharge on occupied tiles. The ranking price.
    | TravelCost
    /// The walk's whole ticks — floored at one tick a step, traffic-blind.
    /// The clock every time-aware judgement is made at.
    | Walk
    /// Travel cost's own units over empty ground. It differs from TravelCost
    /// in traffic alone, which is what lets the reroute attribution blame the
    /// difference on traffic and nothing else.
    | Baseline

/// ADR-0070. Far fields flooded under one census signature
/// (`docs/research/cpu-headroom.md` §5.1): the cost from every tile of the
/// first room of a chain to the origins the walk ends at, carried across the
/// chain's Seams, per tile index of that first room.
///
/// Held across ticks on the plan memo like `WalkTable` and for the same
/// reason: every input it reads is signed by the census signature — the
/// walking grid of each room in the chain, and the Seam bands. A controller's
/// tile is either the declaration's furniture or a fact of vision, and vision
/// moving in a projected room moves that room's entry in the signature.
///
/// The key is the field's whole derivation, including the origins the flood
/// is seeded from, because two callers hand different ones under the same
/// Task (#358). `TravelCost` and `Baseline` differ in traffic alone, so with
/// the traffic gone they are one field, filed under the `TravelCost` entry
/// (`Atlas.farFieldAlong`).
type FarFieldTable =
    System.Collections.Generic.Dictionary<
        string list * Task * bool * FatigueFactor * Pricing * Pos list,
        int[]
     >

/// The walk out to a Seam from every tile of one room's ground, per ordered
/// room pair, as the flood's whole-tick distance per tile index — the tile's
/// own entry cost included, which `Atlas.seamWalkTicks` takes back off. Held
/// on the same terms as `WalkTable`: flooded over the room's walking grid and
/// its Seam band under one constant planning body, all of which the census
/// signature signs.
type SeamWalkTable = System.Collections.Generic.Dictionary<string * string, int[]>

/// The tables an Atlas prices its cross-room legs out of and the census memo
/// recalls: the far fields, and beside them the Seam walks. One lifetime
/// between them, the census's. A record, because it is the one name the whole
/// far side of a cross-room price is handed around under
/// (`docs/research/cpu-headroom.md` §5.1, §5.3).
type FarFieldMemo =
    {
        /// The Seam walks flooded under this census signature. Laid per Atlas
        /// until the profile put the outpost budget's ordering at 5% of a
        /// `reactor --level 7` tick, all of it the same whole-room flood.
        SeamWalks: SeamWalkTable
        /// The far fields, every pricing's, held while the census signature
        /// stands. Task-derived origins only: an ask the decision layer
        /// narrowed for itself (a Guard's ring cut out of this tick's Threats)
        /// keys on tiles that move every tick and would mint a key a tick here
        /// with nothing to evict it. Those ride the Atlas's per-tick table
        /// (`Atlas.TickFarFields`, `Atlas.farFieldAlong`).
        PerCensus: FarFieldTable
    }

[<RequireQualifiedAccess>]
module FarFieldMemo =

    /// A spawn walk table built where nothing is captured (#401, `Fresh`):
    /// the memo's tables outlive the tick, and one built in
    /// `decideUnarbitrated` would keep that tick's view and Atlas alive.
    let walks () : WalkTable = WalkTable()

    /// Two empty tables: the Atlas of a caller holding no memo at all — a
    /// test, or a one-off. A function and never a value, because a table
    /// shared by two parallel test lists is two threads writing one
    /// `Dictionary` (#310, `AGENTS.md` § Code hygiene).
    let empty () : FarFieldMemo =
        {
            SeamWalks = SeamWalkTable()
            PerCensus = FarFieldTable()
        }

/// What a Link footing is held beside: each planned source container, the
/// controller container, the Storage. Carried so a footing the fold cannot
/// serve names the guarantee that was lost, not merely a tile.
[<RequireQualifiedAccess>]
type FootingKind =
    | SourceContainer
    | ControllerContainer
    | Storage

/// A footing target the Layout could not serve: every tile within range 1 of
/// it was a trunk, another target, already taken by a footing, or not buildable
/// at all, so nothing was reserved for it. Recorded rather than dropped — one
/// footing per target is a guarantee, and one that degrades in silence is not.
type UnservedFooting = { Target: RoomPos; Kind: FootingKind }

/// A footing target the Layout served: the tile it reserved, beside the target
/// that tile is held for and that target's kind. The pairing rather than the
/// bare set of tiles, because the set is a one-line projection of the pairing
/// and the reverse is a search.
type ServedFooting =
    {
        Target: RoomPos
        Kind: FootingKind
        Tile: RoomPos
    }

/// The two ends a trunk is routed to: the controller's Upgrade Work Area, and
/// each spawn's walkable ring. A type of its own because the loss is per goal
/// and not per source.
[<RequireQualifiedAccess>]
type TrunkGoal =
    | UpgradeArea
    | Spawn of spawn: string

/// A trunk the Layout could not route: no tile of the goal was reachable from
/// the source once the clustered reservation was marked impassable, or the
/// goal holds no tile at all. One answer on purpose: which way the geometry
/// failed is not something the colony can act on differently. Recorded rather
/// than dropped, because an empty path unions into the road plan in silence.
type UnroutedTrunk = { Source: string; Goal: TrunkGoal }

/// What a container is planned for: a source, the controller, or a mineral.
/// Judged independently — a Seat inside the Upgrade area can satisfy a source
/// and the controller at once. A room has one controller and several sources,
/// so only the source carries an id.
[<RequireQualifiedAccess>]
type ContainerTarget =
    | Source of source: string
    | Controller
    /// A Thorium mineral. A target of its own and not a source: served by the
    /// same rule, planned by a different one — the mineral's container seats
    /// on the trunk out to the Storage, the mineral having no trunk of its own.
    | Mineral of mineral: string

/// ADR-0040. A container pick the plan did not place because its target is
/// already served by a container standing somewhere else: the target, the
/// tile the plan picked, and the tile actually serving it.
type DeferredContainer =
    {
        Target: ContainerTarget
        Pick: RoomPos
        Serving: RoomPos
    }

/// One sink group the hauler quota priced a container's flow to: the
/// spawn cluster, an upgrade buffer or the Storage, and the round trip in
/// ticks — None where no place of that group was reachable, which is a
/// group that carries none of the flow.
type HaulSink = { Kind: string; Trip: int option }

/// One source container's line in the hauler quota: the output it is
/// priced at, its sinks, and the demand that line adds to the sum
/// (output × the mean reachable round trip). Observability only.
type HaulDemandRow =
    {
        Container: RoomPos
        Output: int
        Sinks: HaulSink list
        Demand: int
    }

/// ADR-0017. The census-keyed plan memo: the census signature beside the plans
/// derived from exactly that census. Held by the host in heap across ticks,
/// never written to Memory: a global reset discards it and the next tick
/// recomputes.
type PlanMemo =
    {
        Signature: string
        /// The census signature per projected room the three walk tables
        /// below were filled under (#388, `Decide.roomSignatures`). A walk
        /// table entry reads the grids of the rooms it names and nothing else,
        /// so it is kept while those rooms' entries hold and dropped the tick
        /// one of them moves (`Atlas.evictRooms`); the numbers are in
        /// `docs/profiling.md`.
        ///
        /// Stamped with the tick the tables were filled on, not with the
        /// plan's signature: on a deferred turn (#357) the plan served is
        /// stale while the tables are this tick's, and a census that moved
        /// and moved back would otherwise recall tables flooded under the
        /// intermediate one (#372).
        RoomSignatures: Map<string, string>
        SiteIntents: Intent list
        /// The footing targets this plan left unserved (#77). Empty is the
        /// healthy answer and rides here all the same: a channel that says
        /// nothing when nothing is lost cannot be told from one that is not
        /// there.
        UnservedFootings: UnservedFooting list
        /// The footings this plan placed. No Intent ever names a link and this
        /// never crosses the Memory boundary, so the heap is the only place
        /// the reserved tiles are observable at all.
        ServedFootings: ServedFooting list
        /// The trunks this plan could not route (#107). Empty is the healthy
        /// answer and rides here all the same, as `UnservedFootings` does.
        UnroutedTrunks: UnroutedTrunk list
        /// The container picks this plan deferred to a container already
        /// serving their targets. Empty rides here all the same.
        DeferredContainers: DeferredContainer list
        HaulerQuota: int
        /// The quota's per-container arithmetic, kept beside it for the
        /// `quotas` view.
        HaulerDemand: HaulDemandRow list
        HaulerLoad: int
        /// The walks flooded under this signature, filled through the tick by
        /// the Atlas the table was handed to.
        Walks: WalkTable
        /// The Seam walks flooded under this signature, on the same terms as
        /// `Walks`.
        SeamWalks: SeamWalkTable
        /// The far fields flooded under this signature, on the same terms as
        /// `Walks` (`docs/research/cpu-headroom.md` §5.1).
        FarFields: FarFieldTable
    }

/// Whether this tick is a colony's turn to re-plan its layout (#357), and a DU
/// rather than a `bool` because a missing DU argument cannot read as one of
/// its cases: `not undefined` is `true` in JavaScript, so a `bool` that failed
/// to arrive would read as "not my turn" and defer a colony forever, silently.
/// A tag read off `undefined` throws instead, which is what an argument that
/// did not arrive should do.
[<RequireQualifiedAccess>]
type ReplanTurn =
    /// This colony re-plans if its memo is stale.
    | Now
    /// It serves what it has and owes a plan (`PlanMemo.deferred`).
    | Waiting

[<RequireQualifiedAccess>]
module PlanMemo =

    /// A colony's plan when its turn to re-plan has not come round (#357: the
    /// tick that re-planned four colonies at once cost 487 ms of the engine's
    /// 500 ms ceiling). Nothing placed, no footing reserved, no hauler asked
    /// for — all safe stand-ins: the engine holds construction sites, an
    /// unreserved footing blocks nothing, and a hauler row of zero casts no
    /// body rather than dismissing one.
    ///
    /// The signature is deliberately empty: `censusSignature` composes eight
    /// fields with `|` separators and can never produce the empty string, so
    /// this memo can never match and the next tick must replace it. A memo
    /// stamped with the signature it declined to plan against would be served
    /// forever.
    ///
    /// The three tables are handed in and not defaulted: a deferred colony
    /// declines to plan, not to price. The per-room signatures come with them
    /// because they say which census the tables were filled under, and that
    /// is this tick's, whatever the plan's is (#372).
    let deferred
        (roomSignatures: Map<string, string>)
        (walks: WalkTable)
        (seamWalks: SeamWalkTable)
        (farFields: FarFieldTable)
        : PlanMemo =
        {
            Signature = ""
            RoomSignatures = roomSignatures
            SiteIntents = []
            UnservedFootings = []
            ServedFootings = []
            UnroutedTrunks = []
            DeferredContainers = []
            HaulerQuota = 0
            HaulerDemand = []
            HaulerLoad = 0
            Walks = walks
            SeamWalks = seamWalks
            FarFields = farFields
        }
