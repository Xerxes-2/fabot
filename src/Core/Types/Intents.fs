/// What a tick produces: the described `Intent`s the Executor performs, the
/// `Assignments` carried to the next tick, and the plan memo with the Layout
/// rows (footings, trunks, containers) it is keyed on (ADR 0017, ADR 0044).
[<AutoOpen>]
module Fabot.Core.Types.Intents

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | PlaceConstructionSite of tile: RoomPos * kind: StructureKind
    | HarvestSource of creepName: string * sourceId: string
    | TransferEnergyToStructure of creepName: string * structureId: string
    | WithdrawEnergyFromStructure of creepName: string * structureId: string
    | BuildSite of creepName: string * siteId: string
    | RepairStructure of creepName: string * structureId: string
    | UpgradeController of creepName: string * controllerId: string
    /// The reserve act (ADR 0042): a CLAIM body standing beside a neutral
    /// controller pushes its reservation up by one tick per CLAIM part,
    /// which is what doubles that room's sources. Range 1, like the
    /// engine's other three touching acts.
    | ReserveController of creepName: string * controllerId: string
    /// The claim act (ADR 0047): a CLAIM body standing beside a neutral
    /// controller takes the room for this player. Range 1, like the engine's
    /// other four touching acts.
    | ClaimController of creepName: string * controllerId: string
    | PickupEnergy of creepName: string * resourceId: string
    /// The melee act (ADR 0056): a body with ATTACK parts standing within
    /// range 1 of a hostile creep deals `Engine.attackPower` a part. The
    /// [[guard]]'s own act, and the one Intent that names a creep this colony
    /// does not own — by id, as the [[fire reflex]]'s target is, a hostile
    /// being no target of the projection's.
    | AttackCreep of creepName: string * hostileId: string
    /// The heal act (ADR 0056): a body with HEAL parts restores
    /// `Engine.healPower` a part to a creep of ours within range 1, itself
    /// included, and the engine settles it against the same tick's damage. The
    /// self-heal reflex emits this only for an injured creep with active HEAL
    /// and no conflicting selected action: heal suppresses attack in the engine.
    /// Both creeps are named, and both by **name** — the target is one of ours,
    /// and an Intent whose target rode implicitly
    /// on the actor would say nothing in the Executor's own failure line.
    | HealCreep of creepName: string * targetName: string
    | MoveCreep of creepName: string * direction: Direction
    | SayCreep of creepName: string * message: string
    | ActivateSafeMode of controllerId: string
    | FireTower of towerId: string * hostileId: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>

/// A body's fatigue factor (ADR 0006): the parts that generate fatigue when
/// moving and the Move parts that pay it off. Terrain weight scales by their
/// ratio to price travel in cost units — half-ticks under the engine-native
/// weights (ADR 0010).
type FatigueFactor = { FatigueParts: int; MoveParts: int }

/// The spawn-origin walk table (ADR 0032): the traffic-blind walk out of the
/// tiles beside a spawner, for a body's fatigue factor, as whole-tick distances
/// per tile index of one room (ADR 0026, ADR 0029) — the half of a lead paid
/// after the cast. Filled on demand by the Atlas and handed to the next tick's
/// while the census signature holds. Mutable, and heap-only like the memo
/// carrying it.
type WalkTable = System.Collections.Generic.Dictionary<Pos * FatigueFactor * string, int[]>

/// What a Link footing is held beside (ADR 0022, ADR 0027): each planned
/// source container, the controller container, the Storage. The Layout knows a
/// target's kind by construction and carries it, so a footing the fold cannot
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
/// that tile is held for and that target's kind. The served counterpart of
/// `UnservedFooting`, which names a target and a kind and no tile because there
/// was none. The pairing rather than the bare set of tiles, because the set is
/// a one-line projection of the pairing and the reverse is a search: a
/// reservation the bot never emits can otherwise only be cross-checked by a
/// second derivation (ADR 0035).
type ServedFooting =
    {
        Target: RoomPos
        Kind: FootingKind
        Tile: RoomPos
    }

/// The two ends a trunk is routed to (ADR 0011): the controller's Upgrade Work
/// Area, and each spawn's walkable ring. A type of its own because the loss is
/// per goal and not per source — one source can lose its line to the spawn and
/// keep the one to the controller.
[<RequireQualifiedAccess>]
type TrunkGoal =
    | UpgradeArea
    | Spawn of spawn: string

/// A trunk the Layout could not route: the router paved nothing for this goal,
/// because no tile of it was reachable from the source once the clustered
/// reservation was marked impassable — or because the goal holds no tile at
/// all. The two are one answer on purpose: a line that carries nothing is the
/// loss, and which way the geometry failed is not something the colony can act
/// on differently. Recorded rather than dropped in silence, because an empty
/// path unions into the road plan contributing nothing (ADR 0035's channel,
/// since a trunk has no creep to key a Verdict on).
type UnroutedTrunk = { Source: string; Goal: TrunkGoal }

/// What a container is planned for (ADR 0012): a source, named by its id, or
/// the controller. The two targets the container plan judges, and it judges
/// them independently — a tile can satisfy both at once (a [[dual seat]] is
/// within range 1 of a source and inside the Upgrade Work Area), and ADR 0040
/// names that edge and leaves it. The source carries its id where the
/// controller needs none: a room has one controller (ADR 0005) and several
/// sources.
[<RequireQualifiedAccess>]
type ContainerTarget =
    | Source of source: string
    | Controller

/// A container pick the plan did not place because its target is already served
/// by a container standing somewhere else (ADR 0040): the target, the tile the
/// plan picked, and the tile actually serving it. The pick moves when the trunk
/// moves — a commit, not a tick — so the colony carries a container on a worse
/// tile rather than two containers.
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

/// The census-keyed plan memo (ADR 0017): the census signature beside the plans
/// derived from exactly that census — the Layout's site Intents, the footings
/// it placed and the ones it could not, the hauler quota, and the spawn walks
/// behind the leads (ADR 0032). Held by the host in heap across ticks, never
/// written to Memory: a global reset discards it and the next tick recomputes.
type PlanMemo =
    {
        Signature: string
        SiteIntents: Intent list
        /// The footing targets this plan left unserved (#77), derived from
        /// the same census as the site Intents. Empty is the healthy answer
        /// and rides here all the same: a channel that says nothing when
        /// nothing is lost cannot be told from one that is not there.
        UnservedFootings: UnservedFooting list
        /// The footings this plan placed, each naming its target, that
        /// target's kind and the tile reserved for it. No Intent ever names a
        /// link (ADR 0022) and this never crosses the Memory boundary, so the
        /// heap is the only place the reserved tiles are observable at all —
        /// the whole-room invariant that a footing is off every trunk, target
        /// and other footing reads them here (ADR 0036).
        ServedFootings: ServedFooting list
        /// The trunks this plan could not route (#107), one entry per
        /// (source, goal) the router found no path for. Empty is the healthy
        /// answer and rides here all the same, as `UnservedFootings` does.
        UnroutedTrunks: UnroutedTrunk list
        /// The container picks this plan deferred to a container already
        /// serving their targets (ADR 0040). Empty is the healthy answer and
        /// rides here all the same, as `UnservedFootings` does.
        DeferredContainers: DeferredContainer list
        HaulerQuota: int
        /// The quota's per-container arithmetic, kept beside it for the
        /// `quotas` view; the same census the quota rides.
        HaulerDemand: HaulDemandRow list
        HaulerLoad: int
        /// The walks flooded under this signature, filled through the tick by
        /// the Atlas the table was handed to.
        Walks: WalkTable
    }
