/// What a tick produces: the described `Intent`s the Executor performs, the
/// `Assignments` carried to the next tick, and the plan memo with the Layout
/// rows (footings, trunks, containers) it is keyed on (ADR 0017, ADR 0044).
[<AutoOpen>]
module Fabot.Core.Types.Intents

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | PlaceConstructionSite of tile: RoomPos * kind: StructureKind
    /// The dig act, over either rock the `Harvest` Task can name (ADR 0057
    /// decision 2): a source, or a Thorium deposit under an extractor of ours.
    /// The Intent's name is the frozen one and the field's is the honest one.
    | HarvestSource of creepName: string * rockId: string
    /// The transfer act, over the resource the [[refill]] Task names (ADR 0057
    /// decision 3): `transfer` has taken one all along, and what changed is that
    /// the colony now says which rather than passing energy implicitly. The
    /// Intent's **name** is frozen, exactly as `HarvestSource`'s is over a rock
    /// that is no longer always a source: it is the spelling a [[raid log]] and
    /// every `observe` channel already reads.
    | TransferEnergyToStructure of creepName: string * structureId: string * resource: Resource
    /// The withdraw act, whose target has not been a structure alone since
    /// ADR 0023 widened it: a [[container]], the [[storage]], a tombstone or a
    /// ruin — the same store the [[withdraw]] Task already names (#183) — and
    /// whose resource is the Task's own since ADR 0057 decision 3.
    ///
    /// The **amount** is `int option`, and `None` — take as much as the body
    /// has room for, which is what every construction of this Intent has meant
    /// until now — is the whole of what this colony asks for today. The one
    /// place a Withdraw will ever name a number is the delivery's 999-unit load
    /// (ADR 0057 decision 4), whose decade cliff is worth 275 ticks of a
    /// courier's life; the field arrives here with the resource beside it
    /// because the two are one argument list on the engine's own `withdraw`.
    | WithdrawFromStore of
        creepName: string *
        storeId: string *
        resource: Resource *
        amount: int option
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
    /// The re-claim act (ADR 0057 decision 5, ADR 0060 decision 3): a CLAIM
    /// body standing beside the sector **Reactor** takes it for this player.
    /// Range 1, like the engine's other five touching acts, and a **custom
    /// intent** of the season mod rather than one of the engine's own —
    /// `creep.claimReactor.js` registers `claimReactor` on the Creep prototype
    /// and handles it in `processObjectIntents`, where it does exactly one
    /// thing: `bulk.update(target, {user: object.user})`.
    ///
    /// What that one line is worth knowing for: there is **no cooldown, no
    /// ownership precondition, and `launchTime` is untouched**. So the act is
    /// never refused for having been made recently, is made against a rival's
    /// flag as readily as against none, and does not break the streak it takes
    /// — continuity of the reactor's score depends on its store never emptying
    /// and not on who owns it. A theft is undone on the tick it is seen, and we
    /// are stolen from on the same terms.
    | ClaimReactor of creepName: string * reactorId: string
    /// The pickup act: a creep within range 1 of a dropped pile takes as much
    /// of it as its store has room for. Named for the **pile** and not for
    /// energy since #311, the way #183 renamed the Withdraw's: the engine's
    /// `pickup` takes the object and no resource argument, so one call answers
    /// for an energy pile and a Thorium one alike, and a log line that said
    /// energy while the body walked off with the season's ore was a lie the
    /// reader had to reconcile.
    ///
    /// **Why this one moved where `HarvestSource` and
    /// `TransferEnergyToStructure` are frozen**: the freeze is a rule about
    /// *readers* and not about names. Those two spellings are already out in
    /// the world — a human greps them — so renaming them would cost somebody a
    /// reconciliation the honesty is not worth. This one had no reader at all:
    /// no `PickupEnergy` was written anywhere outside this module, and the one
    /// spelling that *is* persisted, the Task id a Memory key carries, is
    /// `Facts.taskId`'s `pickup:<id>` and has not moved a byte (#167). A name
    /// with a reader stays a lie; a name with none is corrected. That is #183's
    /// `WithdrawFromStore` precedent read out, and it is the whole of the
    /// difference between this paragraph and the one above.
    | PickupPile of creepName: string * resourceId: string
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

/// What a step costs a body, as the flood prices it. It lives here beside the
/// tables keyed on it rather than in `Grid`, where it was declared until the
/// far-field memo below joined the plan memo: a record the host holds across
/// ticks cannot name a type declared in a module compiled after it.
///
/// The split that matters to every reader of it is **traffic**: `TravelCost`
/// prices this tick's standing creeps and the other two are blind to them
/// (`Grid.pricingOf` substitutes `noTraffic`), which is what decides whether
/// an answer may outlive the tick that computed it.
type Pricing =
    /// Travel cost's units — half-ticks, floored at one unit a step, with
    /// the occupancy surcharge on occupied tiles (ADR 0010, ADR 0008).
    /// The ranking price: it breaks rank ties in the Matcher.
    | TravelCost
    /// The walk's whole ticks — floored at one tick a step, traffic-blind
    /// (ADR 0029). The clock: the horizon every time-aware judgement is
    /// made at.
    | Walk
    /// Travel cost's own units over empty ground (ADR 0030): the route the
    /// body would take were no tile occupied. It differs from TravelCost in
    /// traffic alone, which is what lets the reroute attribution blame the
    /// difference on traffic and nothing else (ADR 0008, ADR 0009).
    | Baseline

/// Far fields flooded under one census signature
/// (`docs/research/cpu-headroom.md` §5.1): the cost from every tile of the
/// first room of a chain to the origins the walk ends at, carried across the
/// chain's Seams (ADR 0058), per tile index of that first room. The far leg of
/// every cross-room price, and the eighteen whole-room floods the survey found
/// a `pair --level 7` tick spending three quarters of its flood work
/// recomputing from scratch every tick.
///
/// Held across ticks on the plan memo like `WalkTable` above and for the same
/// reason (ADR 0032): every input it reads is signed by the census signature —
/// the walking grid of each room in the chain, and the Seam bands, which are
/// terrain. A grid is terrain plus roads, obstacle-kind structures and sites,
/// minerals and the controller's own tile; the signature names the first four
/// per projected room, and a controller is either the declaration's furniture,
/// which no tick moves, or a fact of vision — and vision moving in a projected
/// room adds or drops that room's entry in the signature's per-room rate
/// (`Decide.censusSignature`, `ColonyView.ofWorld` filing `Control` for every
/// room it works, transit rooms included). So a signature that has not moved
/// is a field that cannot have.
///
/// The key is the field's whole derivation: the chain of rooms, the Task and
/// whether the body is Work-heavy, the fatigue factor, the pricing, the
/// **origins** the flood is seeded from — that one because two callers hand
/// different ones under the same Task (#358) — and the **standing traffic**
/// the flood priced, as `Atlas`' occupancy sign spells it: the occupied tiles
/// of every room of the chain, in one string, and the empty string for the
/// two traffic-blind pricings, which read no occupancy at all
/// (`Grid.pricingOf` substitutes `noTraffic`).
///
/// A traffic-blind field therefore keys on the census alone and a
/// traffic-aware one keys on this tick's crowd as well, which is what decides
/// the lifetime of each — `FarFieldMemo` below is where that split is spelled.
type FarFieldTable =
    System.Collections.Generic.Dictionary<
        string list * Task * bool * FatigueFactor * Pricing * Pos list * string,
        int[]
     >

/// The far-field tables an Atlas prices its cross-room legs out of, one per
/// **lifetime** — which is the only thing that distinguishes them, so they are
/// named for it and not for a caller (`docs/research/cpu-headroom.md` §5.1,
/// §5.3).
///
/// A record and not three arguments of one type, because three
/// `FarFieldTable`s in a row is a swap the compiler cannot see: laying the
/// tick's table where the census's belongs would hold this tick's traffic
/// forever and read as a stale travel cost, never as an error.
type FarFieldMemo =
    {
        /// The traffic-blind fields (`Walk`, `Baseline`), held while the
        /// census signature stands (ADR 0032). Grows with the census: the
        /// chains a colony's declarations reach over, times the Tasks at the
        /// end of them.
        PerCensus: FarFieldTable
        /// The traffic-aware fields (`TravelCost`) the **previous** tick
        /// flooded, read here and never written. An entry is readable only
        /// under a key naming the same standing traffic, so a crowd that
        /// moved is a miss rather than a stale number.
        LastTick: FarFieldTable
        /// The traffic-aware fields **this** tick floods — every one it
        /// recalls from `LastTick` included, so an answer stays alive as long
        /// as it goes on being asked for. Handed to the next tick on the plan
        /// memo, where it becomes that tick's `LastTick`.
        ///
        /// One tick of carry and not an unbounded table, because a key
        /// carrying the crowd's position is a key that moves when the crowd
        /// does: a table holding every one of them would grow with the ticks
        /// where this one is bounded by what a single tick asks.
        ThisTick: FarFieldTable
    }

[<RequireQualifiedAccess>]
module FarFieldMemo =
    /// Three empty tables: the Atlas of a caller holding no memo at all — a
    /// test, or a one-off. A function and never a value, because a table
    /// shared by two parallel test lists is two threads writing one
    /// `Dictionary` (#310, `AGENTS.md` § Code hygiene).
    let empty () : FarFieldMemo =
        {
            PerCensus = FarFieldTable()
            LastTick = FarFieldTable()
            ThisTick = FarFieldTable()
        }

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
    /// A Thorium mineral, named by its id (ADR 0057 decision 1). A target of
    /// its own and not a source: the two are served by the same rule — a
    /// container standing or pending within range 1 — and planned by two, the
    /// source's container seating on that source's own trunk and the mineral's
    /// on the trunk out to the Storage, the mineral having no trunk of its
    /// own. A room may hold several, so it carries its id as a source does.
    | Mineral of mineral: string

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
        /// The traffic-blind far fields flooded under this signature, filled
        /// through the tick by that same Atlas — `Walks`' rule one query over
        /// (`docs/research/cpu-headroom.md` §5.1).
        FarFields: FarFieldTable
        /// The traffic-aware far fields this tick flooded, for the next tick
        /// to read under a key naming the same crowd (`FarFieldMemo.ThisTick`
        /// above, `docs/research/cpu-headroom.md` §5.1). It rides the
        /// signature with the two tables above it because a moved census is a
        /// moved walking grid, which stales a priced field whatever the crowd
        /// is doing; it is replaced every tick rather than added to, because
        /// its keys move with the crowd.
        TrafficFarFields: FarFieldTable
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

    /// A colony's plan when its turn to re-plan has not come round (#357). The
    /// tick that re-planned four colonies at once cost **487 ms of the
    /// engine's 500 ms ceiling** — 164 in the projection and 248 deciding —
    /// against a mean of 84, and a re-planning tick averages 209 against 84.
    /// So a colony re-plans on its turn, and this is what it holds until then:
    /// nothing placed, no footing reserved, no hauler asked for.
    ///
    /// The signature is **deliberately empty**, which is why this is a value
    /// and not a record literal at the call site. `censusSignature` composes
    /// eight fields with `|` separators, so it can never produce the empty
    /// string — and a memo whose signature can never match is one the next
    /// tick must replace. A memo stamped with the signature it *declined* to
    /// plan against would be served forever.
    ///
    /// Empty is the right stand-in and not merely the cheap one: a site
    /// already in the world does not need this tick's Intent to survive (the
    /// engine holds construction sites), an unreserved footing blocks nothing,
    /// and a hauler row of zero casts no body rather than dismissing one. What
    /// is lost is one tick of *new* placement per colony per turn — measured
    /// against a tick that the engine kills outright.
    /// The three tables are handed in and not defaulted, because every one of
    /// them is a fact this tick paid for: a deferred colony declines to
    /// **plan**, not to price. Handing in an empty table here would throw away
    /// the tick's own walks and far fields, and handing in last tick's
    /// traffic-aware table would stop that carry dead on every turn a colony
    /// skips (ADR 0032, `docs/research/cpu-headroom.md`).
    let deferred
        (walks: WalkTable)
        (farFields: FarFieldTable)
        (trafficFarFields: FarFieldTable)
        : PlanMemo =
        {
            Signature = ""
            SiteIntents = []
            UnservedFootings = []
            ServedFootings = []
            UnroutedTrunks = []
            DeferredContainers = []
            HaulerQuota = 0
            HaulerDemand = []
            HaulerLoad = 0
            Walks = walks
            FarFields = farFields
            TrafficFarFields = trafficFarFields
        }
