/// The engine's fixed numbers and the colony's tunables: what each act costs
/// and yields (`Engine`), and the knobs a colony is allowed to turn
/// (`Tuning`). A number belongs in `Engine` when changing it would be a **lie
/// about the server**, and in `Tuning` when changing it would be a
/// **different colony**.
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

    /// HARVEST_MINERAL_POWER: the Thorium one Work part takes out of a deposit
    /// in one act — one, against a source's two.
    let mineralHarvestPerWork = 1

    /// EXTRACTOR_COOLDOWN: the ticks an extractor is refused after a harvest.
    /// The **cycle** is one longer (`mineralHarvestCycle`): the intent pass runs
    /// before the object pass, so the cooldown written at the end of the harvest
    /// tick is decremented five times and successive digs land six ticks apart.
    let extractorCooldown = 5

    /// The ticks between one dig of a deposit and the next, which is what turns
    /// a [[miner]]'s Work parts into a rate: `Work / 6` Thorium a tick.
    let mineralHarvestCycle = extractorCooldown + 1

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
    /// top of the deficit the reserver row sizes off.
    let reservationCap = 5000

    /// CONTAINER_CAPACITY: what a container's store holds — the line past
    /// which a buffer needs no Refill.
    let containerCapacity = 2000

    /// STORAGE_CAPACITY: what the Storage's store holds. Read against
    /// stored *energy*, because energy is the only resource this colony
    /// ever holds.
    let storageCapacity = 1_000_000

    /// TERMINAL_CAPACITY: what a terminal's store holds, over all resources at
    /// once. The cap on one consignment leg (#349): 19,848 T fits in one
    /// terminal with room to spare.
    let terminalCapacity = 300_000

    /// TERMINAL_COOLDOWN: the ticks a terminal is refused after a `send`, so
    /// the decision to send has to survive being refused — the store it reads
    /// is the store the engine will still hold next tick.
    let terminalCooldown = 10

    /// TERMINAL_MIN_SEND: the smallest amount `send` accepts. Below it the
    /// intent is refused outright, which is why the last few hundred units of a
    /// bank are shipped in one lot or not at all.
    let terminalMinSend = 100

    /// The energy a `send` costs the sending terminal:
    /// `ceil(amount · (1 − e^(−range/30)))` over the **linear** room distance
    /// (`calcTerminalEnergyCost`; `Game.map.getRoomLinearDistance` is
    /// Chebyshev and not the hop count this tree walks by). Three rooms is
    /// about 95 energy a thousand (#349).
    ///
    /// Read off the engine source and **not yet confirmed against a live
    /// send** — the one number in this module that has never been paid.
    let sendFee (range: int) (amount: int) =
        float amount * (1.0 - exp (-float range / 30.0)) |> ceil |> int

    /// The season Reactor's Thorium store capacity. It consumes one a tick;
    /// an empty 999-unit delivery therefore buys 999 ticks of continuity.
    let reactorCapacity = 1_000

    /// ATTACK's range: a melee hostile strikes at one tile.
    let meleeRange = 1

    /// RANGED_ATTACK's range: three tiles.
    let rangedRange = 3

    /// The Source Keeper's leash where it comes to rest: `keepers/pretick.js`
    /// binds each keeper to `memory_sourceId` and moves it to within range **1**
    /// of that source or mineral, and it never pursues (`Keepers`).
    ///
    /// A leash and not a tether: the same file adopts a rock within range
    /// **5** and then walks to range 1 of it, so a keeper freshly cast on its
    /// lair is outside this for the two to four ticks the walk takes (#327).
    let keeperPin = 1

    /// ATTACK_POWER: the hits one ATTACK part takes off a creep at range 1.
    /// Melee is 0.231 damage per energy against ranged's 0.050, which is why
    /// the guard row's block is an ATTACK block.
    let attackPower = 30

    /// RANGED_ATTACK_POWER: the hits one RANGED_ATTACK part takes off a single
    /// target at range 1..3. Read over our own standing guards, so a body
    /// carrying one is priced for what it can do even though the row never
    /// buys one.
    let rangedAttackPower = 10

    /// HEAL_POWER: the hits one HEAL part puts back at range 1 — the rate a
    /// raid's healing is priced at, and never `RANGED_HEAL_POWER`'s 4: a
    /// healer standing beside its own invader heals at 12, and pricing the
    /// raid at its cheapest is the wrong direction for a count that decides
    /// whether we fight at all.
    let healPower = 12

    /// RANGED_HEAL_POWER: what one HEAL part puts back at range 2..3.
    let rangedHealPower = 4

    /// TOWER_POWER_HEAL: what a tower puts back within its optimal range.
    let towerPowerHeal = 400

    /// TOWER_OPTIMAL_RANGE: the range a tower acts at full power out to.
    let towerOptimalRange = 5

    /// TOWER_FALLOFF_RANGE: the range past which a tower's power stops falling.
    let towerFalloffRange = 20

    /// TOWER_FALLOFF: the share of its power a tower has lost at that range.
    let towerFalloff = 0.75

    /// A tower's heal at this range (`processor/intents/towers/heal.js`): full
    /// out to the optimal range, falling linearly to the falloff range and flat
    /// beyond it, floored.
    let towerHealAt (range: int) =
        if range <= towerOptimalRange then
            towerPowerHeal
        else
            let full = float towerPowerHeal
            let r = min range towerFalloffRange

            full
            - full * towerFalloff * float (r - towerOptimalRange)
              / float (towerFalloffRange - towerOptimalRange)
            |> floor
            |> int

    /// Hits a body part carries, unboosted (Screeps `BODYPART_HITS`). The
    /// projection carries a hostile's parts and not its hits, so a raid's
    /// durability is priced at full — the safe direction.
    let partHits = 100

    /// The most guard blocks one raided [[outpost]] ever buys: the cap is
    /// where "hire another" stops and "withdraw" begins. ADR-0056
    let guardCap = 2

    /// The regeneration of a source in a room carrying an owner or a
    /// reservation: 3,000 energy per 300 ticks — what a continuously drained
    /// rock yields there, and the ceiling on what a body over it can take out.
    let heldOutputPerTick = 10

    /// The same source in a room nobody holds: 1,500 per 300 ticks, half
    /// the rate.
    let neutralOutputPerTick = 5

    /// BUILD_POWER: the energy one Work part puts into a site per build tick,
    /// five times what the same part drains upgrading. It is what turns a
    /// site's outstanding cost into ticks of labour, which is the worker row's
    /// backlog term (#364).
    let buildPerWork = 5

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

/// The colony's **tunables**, in one record: every number the bot chose
/// rather than read off the server, carried on the [[colony view]] so a rule
/// reads its colony's own. Each field states the stage and bank it was
/// derived at.
type Tuning =
    {
        /// The Workforce target's floor: the colony never plans below this
        /// many living creeps — two keep the harvest/refill loop running while
        /// one is in transit. A count and not a price, so the same at any bank.
        MinWorkforce: int
        /// The **hungry line** of the decaying kinds: a road or a container
        /// **nobody holds a Repair on** enters the pool below this fraction of
        /// max hits. Bank- and stage-blind. The entry alone; what a held
        /// structure is judged whole at is `RepairWholeLine`.
        RepairTrigger: float
        /// The **whole line** of the decaying kinds: the fraction of max hits a
        /// road or a container a creep **is already repairing** leaves the
        /// pool at. ADR-0061
        ///
        /// Eight tenths, derived at **RCL6 against a ~1,800 bank and the live
        /// worker row's 11 Work / 12 Carry / 12 Move**, and so **not
        /// bank-blind**: a band a **single load** closes is what makes the
        /// ratchet a ratchet. Every road closes its band in one trip; the
        /// container knowingly does not (750 energy, one and a quarter loads)
        /// and is released around 0.74, still over the hungry line, so the
        /// churn is removed either way. Re-derived on #323 rather than moved
        /// here. The decaying kinds alone (`WholeLine.Fraction`).
        RepairWholeLine: float
        /// The **rescue line** (#284): the fraction of max hits a decaying
        /// structure is so far below its own trigger that repairing it stops
        /// being surplus work and becomes a rescue. Travel cost alone orders
        /// the surplus tier, and the base cluster always holds a road two tiles
        /// from a loaded body, so a road out on the trunk or across a Seam
        /// loses every comparison it is in until it is destroyed — a quarter of
        /// max is where the colony stops letting it.
        RepairRescueLine: float
        /// How many rescues the colony runs at once (#284): the most damaged
        /// structures are lifted a rung over the rest of the surplus, one body
        /// apiece. Two, so a colony that has let a whole trunk rot still
        /// spends most of its surplus at home.
        RepairRescues: int
        /// The rampart floor: a rampart is hungry below this many hits and
        /// whole at it — the ticks the room must hold times the damage per
        /// tick. No hysteresis, one Repair visit clearing the line. ADR-0034
        RampartFloor: int
        /// How many whole bodies of the colony's own bank the Storage keeps
        /// back before the upgrader row may spend any of it (#385). **Counted
        /// in bodies rather than in energy** so it scales with the room:
        /// 36,000 at RCL5's 1,800 bank up to 246,000 at RCL8's 12,300. Twenty
        /// bodies is the whole workforce of the largest colony this bot has
        /// run, once over — **argued and not measured**, the harness cannot
        /// stand a full Storage yet (#343). The worker row's backlog term
        /// (#364) draws on the same remainder; this floor bounds the overlap.
        UpgradeStockBodies: int
        /// The pile a Pickup is worth walking for: a dropped pile enters the
        /// pool at this many units and never below it. A hundred, derived at
        /// the **300 bank** — two Carry parts' worth, the smallest load that
        /// pays for a walk made for the pile alone. One number over **both**
        /// resources (#311), because what it prices is the trip and not the
        /// cargo; a Thorium pile under the line is gone inside `amount` ticks
        /// by its own decay.
        PickupThreshold: int
        /// The Reach margin: the tiles a Threat's weapon range is widened by —
        /// one for the hostile's next step, one for our own tick of lag.
        /// Tiles of lag, so the same at every stage and bank.
        ReachMargin: int
        /// The [[standing body]]'s line: the Carry parts per Work at which a
        /// delivery stops being work and becomes a commute. A ratio over one
        /// body's own parts, so bank-free as written. ADR-0046
        StandingCarryPerWork: int
        /// The **pioneers**: how many more [[worker unit]]s a mother hires
        /// while a [[nursery]] or a bootstrapping child of hers stands — the
        /// addend on the worker row's own share and the cap on the borrowed
        /// Upgrade and Build.
        PioneerCount: int
        /// The **[[ferry]]**: the hauler bodies a mother hires against a
        /// bootstrapping child's upgrade buffer, over and above the haul her
        /// own containers ask for. One, derived at her `Independent` **1,800
        /// bank** and read at no other stage: a lend is bounded by what is
        /// written down and never by what the child could absorb.
        FerryLoads: int
        /// The claimer range at which safe mode fires: the precise deadline is
        /// 2 — `attackController` is a range-1 act judged from tick-start
        /// position and a creep steps one tile a tick — plus one tile of
        /// margin for a skipped tick. ADR-0015
        SafeModeDeadline: int
        /// The level the engine unlocks the Storage at (CONTROLLER_STRUCTURES
        /// for "storage"). The Layout reserves the Storage's whole allowance
        /// here rather than at the horizon: its tile never comes back once an
        /// extension takes it. ADR-0022
        StorageLevel: int
        /// The level the engine unlocks the extractor at
        /// (`CONTROLLER_STRUCTURES` for "extractor": 1 at RCL6, 7 and 8, and 0
        /// below). The Layout filters the extractor and its container here
        /// rather than reserving anything at the horizon: the deposit sits on
        /// a **wall** tile, off the clustered checkerboard and unbuildable for
        /// every other kind, so there is no window to hold open.
        ExtractorLevel: int
        /// The level the engine unlocks the terminal at (`CONTROLLER_STRUCTURES`
        /// for "terminal": 1 from RCL6). Read as `StorageLevel` is (#349): the
        /// Layout **reserves** the terminal's tile at this level from level 0,
        /// because its pick sits inside the clustered ordering and an
        /// extension that takes that tile never gives it back — and it
        /// *places* at the room's own level.
        TerminalLevel: int
        /// The Work parts one Move carries on the **miner** row. Five, derived
        /// at the **2,300 bank**: the body walks to one tile once and then
        /// never leaves it, so its Move is bought for a single commute and not
        /// for fatigue parity, which is a rule about a body that keeps moving.
        /// ADR-0057
        MinerWorkPerMove: int
        /// The ticks of life a [[miner]] burns in one tick, which turns
        /// `CREEP_LIFE_TIME` into the 500-tick life its replacement is
        /// amortized at. The **engine** fact is a formula: `thorium.js` sums
        /// `store.T` over a tile, takes `p = floor(log10 total)`, and a body
        /// there burns `1 + p`. Three is `p = 2`, a fact about **our haul** —
        /// the container held in the 100..999 band by `MineContactCliff` —
        /// which is why it is a knob and not an `Engine` binding. A policy
        /// assumption and not a bound: the rung wins only *within*
        /// `StockDraw`, so a colony saturated on Feeding-tier work still
        /// leaves the container to fill (#313).
        MineContactAgeing: int
        /// The load on the mine tile at which the colony escalates the mineral
        /// container's [[withdraw]] two rungs (#306). The engine's cliff is at
        /// *every* decade (`docs/research/thorium-reactor.md` §2); which
        /// decade the haul is run at is this colony's policy. A thousand:
        /// drawn here the container cycles 0..999 and the [[miner]] averages
        /// `1 + p ≈ 2.9`; left to the 2,000 cap it averages **3.67** and buys
        /// the row a third more bodies for the same ore.
        MineContactCliff: int
        /// Thorium carried on one Reactor delivery (#319, resized by #354):
        /// under the 1,000-unit contact cliff, and sized off the **Reactor's**
        /// store. It holds 1,000 and burns 1 T a tick, so a load must satisfy
        /// `load + walk < reactorCapacity` — the draw is gated on room for a
        /// whole load (`Planner`), and the store drains for the whole loaded
        /// walk. 500 against W15S28's 318-tick leg leaves a **182-tick
        /// margin** against the store reaching zero. 999 was the first value:
        /// no gate could admit it and leave the store non-empty at arrival,
        /// so the courier waited on its own hot tile, died holding ore, and
        /// 915 T reached the Reactor room's floor (#354).
        ReactorLoad: int
        /// The slack a delivery gate leaves over its own arithmetic, as a
        /// percentage (#378). `Emitter.outlivesTheLoadedLeg` admits a body on
        /// `TicksToLive >= walk * MineContactAgeing`, and the bare equality
        /// fails on any difference between the walk the Atlas prices and the
        /// walk the body makes — a [[flee]], a [[keeper margin]] detour, one
        /// swamp step. Live, `hauler-558190` drew with two ticks of margin
        /// over a 196-tick leg and died in the Reactor's room with 500 T
        /// aboard. A percentage because the risk scales with the leg.
        DeliveryLifeMargin: int
        /// The energy a terminal is kept stocked with, to pay `send`'s fee out
        /// of (#349). Sized off the job and not off the store: about 95
        /// energy a thousand units over three rooms, so 4,000 ships the whole
        /// 36,484 T banked in the two shipping colonies with room to spare.
        TerminalEnergy: int
        /// How many of a generalist's 1,500 ticks the worker row's backlog term
        /// assumes it spends **building** (#364). Not the lifetime, which made
        /// the term dead code. Measured live at W13S28 on 2026-09-17 over the
        /// terminal site: **33/tick** with three workers and **15.4/tick** with
        /// one to two, against a nominal 80 a tick per 16-Work body — a tenth
        /// to a fifth, the rest being the carry cycle (ten ticks of building
        /// against a twenty-tick round trip). 300 is the measured fifth, at
        /// the **optimistic** end on purpose: too small a figure hires a
        /// crowd.
        BuildTicksPerLife: int
        /// Ticks between courier casts while the delivery programme is open
        /// (#319): the loaded walk is 318 ticks over W15S28's route, so a
        /// courier cast this often is always fresh enough. It is **not** what
        /// regulates supply (#354) — the draw gate on the Reactor's store is —
        /// and only keeps a *body* in the programme.
        DeliveryInterval: int
        /// Ticks the [[re-claimer]] relief is meant to stand beside its
        /// incumbent (#329). Added both to the incumbent's replacement lead
        /// and to `Reclaim`'s capacity handover window; either half alone is
        /// inert.
        ReclaimerOverlap: int
        /// **How far ahead** the Layout *places*, in controller levels: the
        /// horizon is `controller.Level + this` (`Tuning.horizonOf`), so the
        /// clustered kinds are sized one level above the room's own. The
        /// lookahead is the half of the horizon this bot chose; the level it
        /// is added to is read off the server. Not what the trunks route
        /// around: the road reservation reads no level. One, because placing
        /// four levels out draws tiles for a colony that may never get there;
        /// zero means no lookahead, which is what a *stale* absolute constant
        /// produced by accident at RCL7 (#341). ADR-0011
        HorizonLookahead: int
        /// How many creeps the colony has building in its [[outpost]]s at once
        /// — a budget over every site out there together and never a per-site
        /// number. It is also how many of those sites the budget lifts onto
        /// the feeding tier at a time (#266), so a trunk is paved outward from
        /// the crossing rather than all at once.
        OutpostBuilders: int
        /// The controller level a child colony stops being bootstrapped at,
        /// and so the line `Colony.stageOf` cuts `Bootstrapping` from
        /// `Independent` on: **the one place this number is read**. Three,
        /// the level a colony can defend and feed itself at.
        BootstrapLevel: int
        /// How many [[seam]]s one cross-room walk may cross: the hop budget a
        /// declared [[outpost]] has to sit inside, and the depth the route
        /// search stops at. Three, which is what reaches the rooms beyond the
        /// ring of five the one-hop model could name; there is no upper bound
        /// in the arithmetic, only in the CPU, which is why the bound is
        /// written down. ADR-0058
        MaxHops: int
        /// What a swamp tile costs a **trunk**: three against plain's two,
        /// where a walking creep pays `Engine.swampWeight`, ten. Once paved a
        /// swamp tile walks at what a paved plain tile does, so the only thing
        /// swamp costs a road is a one-off construction, and three amortizes
        /// it. ADR-0010
        TrunkSwampWeight: int
        /// The [[stand-down]] a threat gave no readable deadline for: 2,500
        /// ticks, the stronghold expansion period.
        StandDownFallback: int
        /// How long an armed [[threat]] seen in a declared [[outpost]] is
        /// remembered once the room goes dark (#366): the ticks the guard row
        /// and the Guard Task keep answering for a room nothing of ours can
        /// see any more, counted from the last tick vision showed the threat.
        ///
        /// **1,500: the longest a raider can still be standing there.** It was
        /// 300 (oven plus walk), the right size for **sending** the guard and
        /// the wrong size for **remembering**: while the room is dark, expiry
        /// reads as a room that is clear, and an invader living out its full
        /// life outlasts 300 four times over — W15S29 killed four of W15S28's
        /// bodies that way, to one invader of 1 ATTACK and 1 RANGED_ATTACK.
        /// **A remembered threat with too short a clock is worse than no
        /// memory at all.** So the clock is a backstop: what ends a memory in
        /// ordinary running is a **look**, and the guard standing in the room
        /// is itself that look. The cost is a guard hired for a room that went
        /// quiet unseen, 750 energy a block.
        ThreatMemory: int
        /// How often a [[stand-down]] latched on another player's **ownership**
        /// is looked at again (#165): the room is re-admitted to the scan set
        /// for one tick, and to nothing else, so one tick of vision can clear
        /// a latch the withdrawal would otherwise make permanent. A floor on
        /// the gap and not a cadence (#275): the first tick the gate is
        /// evaluated on pays it. 5,000 ticks, twice the stronghold expansion
        /// period: a room another player walks away from is a thing that
        /// happens over hours, and the look itself is only a map lookup.
        RivalRecheck: int
        /// Ticks of silence that close a [[raid]] episode. It has to outlast a
        /// poke-and-heal cycle — #66's squad worked one room across ~220
        /// ticks, and that is one raid, not forty — and fifty is about the
        /// round trip a retreating squad makes before it is back. ADR-0028
        QuietGap: int
        /// How long a held assignment outlives its target's room going dark
        /// (#151): the ticks the Matcher keeps an assignment whose Task left
        /// the pool with the vision that carried it, before releasing it
        /// `task-gone` after all. **150**, a [[reserver]] relief's lead — the
        /// cast plus the walk out — because an [[outpost]] goes dark every
        /// time its reserver dies, a CLAIM body living 600 ticks, and the
        /// vision is back the tick the next one lands. What a longer number
        /// would buy is a body holding a container that was destroyed while
        /// nobody could see it go.
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
            RepairWholeLine = 0.8
            RepairRescueLine = 0.25
            RepairRescues = 2
            RampartFloor = 100_000
            UpgradeStockBodies = 20
            PickupThreshold = 100
            ReachMargin = 2
            StandingCarryPerWork = 4
            PioneerCount = 3
            FerryLoads = 1
            SafeModeDeadline = 3
            StorageLevel = 4
            ExtractorLevel = 6
            TerminalLevel = 6
            MinerWorkPerMove = 5
            MineContactAgeing = 3
            MineContactCliff = 1000
            ReactorLoad = 500
            DeliveryLifeMargin = 25
            TerminalEnergy = 4_000
            BuildTicksPerLife = 300
            DeliveryInterval = 636
            ReclaimerOverlap = 25
            HorizonLookahead = 1
            OutpostBuilders = 2
            BootstrapLevel = 3
            MaxHops = 3
            TrunkSwampWeight = 3
            StandDownFallback = 2500
            ThreatMemory = Engine.creepLifetime
            RivalRecheck = 5000
            QuietGap = 50
            VisionGrace = 150
        }

    /// The **[[keeper margin]]**: the tiles masked out of a Source Keeper
    /// room's walkable ground around every rock a keeper is pinned to
    /// (`Keepers`). Six today, and **derived rather than chosen** — a function
    /// beside the record and not a field in it, so a human who moves
    /// `ReachMargin` moves this too: ADR-0060
    ///
    ///     the keeper's pin (1) + its longest weapon (3) + ReachMargin (2)
    ///
    /// Five is the right number for **survival** and the wrong number for what
    /// this buys: a Reach is `weapon + ReachMargin` = 5 for a RANGED_ATTACK
    /// body, so at five a crossing courier is inside a Reach and [[flee]]
    /// applies. At six Flee is inapplicable by geometry — for the steady
    /// state, not the ticks after a respawn (`Engine.keeperPin`, #327); past
    /// seven W15S26 cannot be crossed at all.
    let keeperMargin (tuning: Tuning) : int =
        Engine.keeperPin + Engine.rangedRange + tuning.ReachMargin

    /// The **Layout horizon**: the controller level the clustered kinds are
    /// *placed* at, where the level they are *filtered* at is the room's own.
    /// It sizes the placement alone — the reservation the trunks route around
    /// is `Layout.allowanceCeiling`'s and reads no level (ADR-0064). Derived
    /// rather than chosen, a function beside the record: ADR-0063
    ///
    ///     max 0 (controller.Level + HorizonLookahead)
    ///
    /// The **top** needs no clamp: `allowanceOf`'s catch-all answers 9 what
    /// it answers 8. The **bottom** is clamped because that catch-all is
    /// two-sided and answers a *negative* level the same sixty extensions it
    /// answers RCL8, so a lookahead of −2 would hand a young room the
    /// **widest** window instead of the narrowest (`QuotaTuningTests` reads
    /// the clamp).
    let horizonOf (tuning: Tuning) (level: int) : int = max 0 (level + tuning.HorizonLookahead)
