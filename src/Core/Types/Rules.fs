/// The engine's fixed numbers and the colony's tunables: what each act costs
/// and yields (`Engine`), and the knobs a colony is allowed to turn
/// (`Tuning`). Constants, no shape of our own.
[<AutoOpen>]
module Fabot.Core.Types.Rules

module Engine =
    /// MAX_CREEP_SIZE: the parts a body may hold. A body over it is
    /// refused outright and the spawn silently does nothing that tick, so
    /// every row's sizing rule caps here.
    let maxBodyParts = 50

    /// HARVEST_POWER: the energy one Work part digs out of a source in a
    /// tick.
    let harvestPerWork = 2

    /// CARRY_CAPACITY: the energy one Carry part holds.
    let carryPartCapacity = 50

    /// CREEP_LIFE_TIME: the ticks a spawned creep lives — the horizon a
    /// body's replacement cost is amortized over.
    let creepLifetime = 1500

    /// CREEP_SPAWN_TIME: the ticks a spawner spends per body part — the
    /// half of a lead that is paid before the replacement takes its first
    /// step.
    let spawnTicksPerPart = 3

    /// CREEP_CLAIM_LIFE_TIME: the ticks a body carrying a CLAIM part lives,
    /// well short of the 1,500 every other row gets. The reservation deficit's
    /// divisor and the reserver row's amortization both read it.
    let claimLifetime = 600

    /// CONTROLLER_RESERVE_MAX: the ticks a reservation caps at, and so the
    /// top of the deficit the reserver row sizes off (ADR 0042).
    let reservationCap = 5000

    /// CONTAINER_CAPACITY: what a container's store holds — the line past
    /// which a buffer needs no Refill.
    let containerCapacity = 2000

    /// STORAGE_CAPACITY: what the Storage's store holds. Read against
    /// stored *energy*, because energy is the only resource this colony
    /// ever holds.
    let storageCapacity = 1_000_000

    /// ATTACK's range: a melee hostile strikes at one tile.
    let meleeRange = 1

    /// RANGED_ATTACK's range: three tiles.
    let rangedRange = 3

    /// ATTACK_POWER: the hits one ATTACK part takes off a creep at range 1.
    /// The guard row's count rule prices our own damage with it (ADR 0056) —
    /// melee is 0.231 damage per energy against ranged's 0.050, which is why
    /// that row's block is an ATTACK block.
    let attackPower = 30

    /// RANGED_ATTACK_POWER: the hits one RANGED_ATTACK part takes off a single
    /// target at range 1..3. Read beside `attackPower` over our own standing
    /// guards, so a body carrying one is priced for what it can actually do
    /// even though the row never buys one (ADR 0056).
    let rangedAttackPower = 10

    /// HEAL_POWER: the hits one HEAL part puts back at range 1 — the rate a
    /// raid's healing is priced at, and never `RANGED_HEAL_POWER`'s 4: a
    /// healer standing beside its own invader heals at 12, and pricing the
    /// raid at its cheapest is the wrong direction for a count that decides
    /// whether we fight at all (ADR 0056).
    let healPower = 12

    /// Hits a body part carries, unboosted (Screeps `BODYPART_HITS`). The
    /// projection carries a hostile's parts and not its hits, so this is how a
    /// raid's durability is priced — at full, which is the safe direction for a
    /// count that decides whether we fight at all (ADR 0056 as #280 amends it).
    let partHits = 100

    /// The most guard blocks one raided [[outpost]] ever buys (ADR 0056
    /// decision 1). A raid two blocks cannot beat is a room to leave rather
    /// than a fight to lose, which is what ADR 0043's clock reads it for
    /// (#257): the cap is where "hire another" stops and "withdraw" begins.
    let guardCap = 2

    /// The regeneration of a source in a room carrying an owner or a
    /// reservation: 3,000 energy per 300 ticks — what a continuously drained
    /// rock yields there, and the ceiling on what a body over it can take out.
    let heldOutputPerTick = 10

    /// The same source in a room nobody holds: 1,500 per 300 ticks, half
    /// the rate.
    let neutralOutputPerTick = 5

    /// UPGRADE_CONTROLLER_POWER's energy cost: what one Work part drains
    /// per upgrade tick — the rate an upgrade mouth eats income at.
    let upgradeDrainPerWork = 1

    /// What a swamp tile costs a walking creep against plain's two: the
    /// dearest weight a grid can hold, which the flood's step table is
    /// sized by.
    let swampWeight = 10

    /// The side of a Screeps room in tiles, and so the stride of every
    /// flat `x * 50 + y` grid the Atlas lays.
    let roomSide = 50

/// The colony's **tunables**, in one record (ADR 0052 decision 5): every number
/// the bot chose rather than read off the server, carried on the [[colony
/// view]] so a rule reads its colony's own.
type Tuning =
    {
        /// The Workforce target's floor: the colony never plans below this
        /// many living creeps — two keep the harvest/refill loop running while
        /// one is in transit. A count and not a price, so the same at any bank.
        MinWorkforce: int
        /// The Repair trigger of the decaying kinds: a road or a container
        /// enters the pool below this fraction of max hits and leaves it once
        /// repaired over the line. A tunable, not part of ADR 0010, and bank-
        /// and stage-blind — a fraction of the structure's own max.
        RepairTrigger: float
        /// The **rescue line** (#284): the fraction of max hits a decaying
        /// structure is so far below its own trigger that repairing it stops
        /// being surplus work and becomes a rescue. Travel cost alone orders
        /// the surplus tier, and the base cluster always holds a road two tiles
        /// from a loaded body, so a road out on the trunk or across a Seam
        /// loses every comparison it is in until it is destroyed — a quarter of
        /// max is where the colony stops letting it. A fraction of the
        /// structure's own max, like the trigger it sits under.
        RepairRescueLine: float
        /// How many rescues the colony runs at once (#284), the shape of the
        /// outpost builders' budget one Task over: the most damaged structures
        /// are lifted a rung over the rest of the surplus, one body apiece, and
        /// the rest wait their turn. Two, so a colony that has let a whole
        /// trunk rot still spends most of its surplus at home.
        RepairRescues: int
        /// The rampart floor (ADR 0034): a rampart is hungry below this many
        /// hits and whole at it — the ticks the room must hold times the damage
        /// per tick. No hysteresis, one Repair visit clearing the line.
        RampartFloor: int
        /// The pile a Pickup is worth walking for: a dropped pile enters the
        /// pool at this many energy and never below it. A hundred, derived at
        /// the **300 bank** — two Carry parts' worth, the smallest load that
        /// pays for a walk made for the pile alone.
        PickupThreshold: int
        /// The Reach margin (ADR 0033): the tiles a Threat's weapon range is
        /// widened by — one for the hostile's next step, one for our own tick
        /// of lag. Tiles of lag, so the same at every stage and bank.
        ReachMargin: int
        /// The [[standing body]]'s line (ADR 0046): the Carry parts per Work at
        /// which a delivery stops being work and becomes a commute. A ratio
        /// over one body's own parts, so bank-free as written; what the bank
        /// moves is which casts fall on either side.
        StandingCarryPerWork: int
        /// The **pioneers**: how many more [[worker unit]]s a mother hires
        /// while a [[nursery]] or a bootstrapping child of hers stands (ADR
        /// 0047 decision 4) — the addend on the worker row's own share and the
        /// cap on the borrowed Upgrade and Build.
        PioneerCount: int
        /// The **[[ferry]]**: the hauler bodies a mother hires against a
        /// bootstrapping child's upgrade buffer, over and above the haul her
        /// own containers ask for (ADR 0052 decision 7). One, derived at her
        /// `Independent` **1,800 bank** and read at no other stage, because a
        /// lend is bounded by what is written down and never by what the child
        /// could absorb.
        FerryLoads: int
        /// The claimer range at which safe mode fires (ADR 0015): the precise
        /// deadline is 2 — `attackController` is a range-1 act judged from
        /// tick-start position and a creep steps one tile a tick — plus one
        /// tile of margin for a skipped tick. Tiles, so the same at any stage.
        SafeModeDeadline: int
        /// The level the engine unlocks the Storage at (CONTROLLER_STRUCTURES
        /// for "storage"). The Layout reserves the Storage's whole allowance
        /// here rather than at the horizon (ADR 0022): its tile never comes
        /// back once an extension takes it.
        StorageLevel: int
        /// The level the engine unlocks the extractor at
        /// (`CONTROLLER_STRUCTURES` for "extractor": 1 at RCL6, 7 and 8, and 0
        /// below). Beside `StorageLevel` and for its reason — it is the level
        /// the engine unlocks the kind at — and the Layout filters the
        /// extractor and its container here rather than reserving anything at
        /// the horizon (ADR 0057 decision 1): the deposit sits on a **wall**
        /// tile at the mouth of a wall, off the clustered checkerboard and
        /// unbuildable for every other kind, so there is no window for an
        /// extension to take and nothing to hold open.
        ExtractorLevel: int
        /// The Work parts one Move carries on the **miner** row (ADR 0057
        /// decision 2). Five, derived at the **2,300 bank**: the body walks to
        /// one tile once and then never leaves it, so its Move is bought for a
        /// single commute and not for ADR 0003's fatigue parity, which is a
        /// rule about a body that keeps moving. Changing it would be a
        /// different colony — a miner that reaches its Post sooner and digs
        /// less — where `EXTRACTOR_COOLDOWN` beside it would be a lie about the
        /// server.
        MinerWorkPerMove: int
        /// The Layout horizon (ADR 0011, moved to RCL5 by ADR 0039 and to RCL6
        /// by ADR 0055): the whole plan is computed up to this level regardless
        /// of the current one, so today's roads route around tomorrow's
        /// structures. One level of lookahead, and it is moved **before** the
        /// room reaches it: the clustered kinds are sized here and only
        /// filtered at the current level, so a room standing at RCL6 under a
        /// horizon of 5 computes an extension gap of zero and plans none of the
        /// ten the engine just unlocked.
        HorizonLevel: int
        /// How many creeps the colony has building in its [[outpost]]s at once
        /// — a budget over every site out there together and never a per-site
        /// number, the Planner placing one container site per unserved outpost
        /// source on the same tick and a human paving the rest by hand. It is
        /// also how many of those sites the budget lifts onto the feeding tier
        /// at a time (#266): the crowd that may cross a [[seam]] and the number
        /// of sites worth crossing for are one number, so a trunk is paved
        /// outward from the crossing rather than all at once.
        OutpostBuilders: int
        /// The controller level a child colony stops being bootstrapped at (ADR
        /// 0047 decision 4), and so the line `Colony.stageOf` cuts
        /// `Bootstrapping` from `Independent` on: **the one place this number
        /// is read** (ADR 0052 decision 3). Three, the level a colony can
        /// defend and feed itself at.
        BootstrapLevel: int
        /// How many [[seam]]s one cross-room walk may cross (ADR 0058): the
        /// hop budget a declared [[outpost]] has to sit inside, and the depth
        /// the route search stops at. Three, which is what reaches the rooms
        /// beyond the ring of five this colony's one-hop model could name —
        /// a number a human moves in a commit, exactly as the Layout's horizon
        /// is (ADR 0039), because every hop past the first is a transit room
        /// projected and a flood chained onto the price. One would be the
        /// model before this ADR; there is no upper bound in the arithmetic,
        /// only in the CPU, which is why the bound is written down.
        MaxHops: int
        /// What a swamp tile costs a **trunk** (ADR 0011 as #211 amends it):
        /// three against plain's two, where a walking creep pays
        /// `Engine.swampWeight`, ten. Once paved a swamp tile walks at what a
        /// paved plain tile does (ADR 0010), so the only thing swamp costs a
        /// road is a one-off construction, and three amortizes it.
        TrunkSwampWeight: int
        /// The [[stand-down]] a threat gave no readable deadline for: 2,500
        /// ticks, the stronghold expansion period (ADR 0043).
        StandDownFallback: int
        /// How often a [[stand-down]] latched on another player's **ownership**
        /// is looked at again (#165): once this many ticks have passed since the
        /// last look, the room is re-admitted to the colony's scan set for one
        /// tick, and to nothing else — it stays out of the Task pool and out of
        /// the quotas throughout — so one tick of vision can clear a latch the
        /// gate's own withdrawal would otherwise make permanent. A floor on the
        /// gap and not a cadence (#275): the look is owed from that tick onwards
        /// and the first tick the gate is evaluated on pays it, so a tick the
        /// loop never ran delays a look rather than forfeiting a whole stride of
        /// an outpost's income. 5,000 ticks, twice ADR 0043's stronghold
        /// expansion period: a room another player walks away from is a thing
        /// that happens over hours, so a stride this long costs at most one
        /// such window of an outpost's income and keeps the withdrawal what
        /// ADR 0043 made it — a conclusion **held** rather than a judgement
        /// re-taken every tick. What the stride does not ration is a room
        /// read: the shell already reads every declared room the engine will
        /// answer for (`World.ofGame`), so the look itself is a map lookup,
        /// and this number is how often the colony is willing to question a
        /// conclusion of its own. Ticks and not a price, so the same at any
        /// bank and any [[stage]].
        RivalRecheck: int
        /// Ticks of silence that close a [[raid]] episode (ADR 0028). It has
        /// to outlast a poke-and-heal cycle — #66's squad worked one room
        /// across ~220 ticks, and that is one raid, not forty — and fifty is
        /// about the round trip a retreating squad makes before it is back.
        QuietGap: int
        /// How long a held assignment outlives its target's room going dark
        /// (#151): the ticks the Matcher keeps an assignment whose Task left
        /// the pool with the vision that carried it, before releasing it
        /// `task-gone` after all. **150**, a [[reserver]] relief's lead — the
        /// cast plus the walk out — because an [[outpost]] goes dark every
        /// time its reserver dies, a CLAIM body living 600 ticks, and the
        /// vision is back the tick the next one lands. Ticks of blindness and
        /// not a price, so the same at any bank and any [[stage]]; what a
        /// longer number would buy is a body holding a container that was
        /// destroyed while nobody could see it go.
        VisionGrace: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tuning =
    /// The numbers this bot ships with: what the shell hands every colony, and
    /// what a test starts from before it moves the one field it is about. One
    /// set and not one per stage — each field states the stage it was derived
    /// at, and the rules that read it branch on the [[stage]] themselves.
    let defaults =
        {
            MinWorkforce = 2
            RepairTrigger = 0.5
            RepairRescueLine = 0.25
            RepairRescues = 2
            RampartFloor = 100_000
            PickupThreshold = 100
            ReachMargin = 2
            StandingCarryPerWork = 4
            PioneerCount = 3
            FerryLoads = 1
            SafeModeDeadline = 3
            StorageLevel = 4
            ExtractorLevel = 6
            MinerWorkPerMove = 5
            HorizonLevel = 6
            OutpostBuilders = 2
            BootstrapLevel = 3
            MaxHops = 3
            TrunkSwampWeight = 3
            StandDownFallback = 2500
            RivalRecheck = 5000
            QuietGap = 50
            VisionGrace = 150
        }
