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

    /// HARVEST_MINERAL_POWER: the Thorium one Work part takes out of a deposit
    /// in one act — one, against a source's two (ADR 0057).
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
    /// top of the deficit the reserver row sizes off (ADR 0042).
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
    /// terminal with room to spare, so the ore moves in as few sends as the
    /// hauling allows and never in 500-unit courier loads.
    let terminalCapacity = 300_000

    /// TERMINAL_COOLDOWN: the ticks a terminal is refused after a `send`.
    /// Ten, so a colony shipping a bank moves it in bursts and the decision to
    /// send has to survive being refused — the store it reads is the store the
    /// engine will still hold next tick.
    let terminalCooldown = 10

    /// TERMINAL_MIN_SEND: the smallest amount `send` accepts. Below it the
    /// intent is refused outright, which is why the last few hundred units of a
    /// bank are shipped in one lot or not at all.
    let terminalMinSend = 100

    /// The energy a `send` costs the sending terminal:
    /// `ceil(amount · (1 − e^(−range/30)))` over the **linear** room distance
    /// (`calcTerminalEnergyCost`, and Screeps' `Game.map.getRoomLinearDistance`
    /// is Chebyshev and not the hop count this tree walks by). Three rooms is
    /// about 95 energy a thousand; the whole 36,484 T banked in the two homes
    /// moves for under 4,000 (#349).
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
    /// of that source or mineral, and it never pursues. The engine's own rule
    /// and no tunable of ours, which is the whole reason a keeper is a hostile
    /// the map can answer before the tick begins (ADR 0060 decision 2,
    /// `Keepers`).
    ///
    /// A leash and not a tether: the same file adopts a rock within range
    /// **5** and then walks to range 1 of it, so a keeper freshly cast on its
    /// lair is outside this for the two to four ticks the walk takes, and the
    /// mask derived from this number covers the steady state alone (#327).
    let keeperPin = 1

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

/// The colony's **tunables**, in one record (ADR 0052 decision 5): every number
/// the bot chose rather than read off the server, carried on the [[colony
/// view]] so a rule reads its colony's own.
type Tuning =
    {
        /// The Workforce target's floor: the colony never plans below this
        /// many living creeps — two keep the harvest/refill loop running while
        /// one is in transit. A count and not a price, so the same at any bank.
        MinWorkforce: int
        /// The **hungry line** of the decaying kinds (ADR 0061): a road or a
        /// container **nobody holds a Repair on** enters the pool below this
        /// fraction of max hits. A tunable, not part of ADR 0010, and bank- and
        /// stage-blind — a fraction of the structure's own max. Since ADR 0061
        /// it is the entry alone: what a held structure is judged whole at is
        /// `RepairWholeLine`.
        RepairTrigger: float
        /// The **whole line** of the decaying kinds (ADR 0061): the fraction of
        /// max hits a road or a container a creep **is already repairing**
        /// leaves the pool at. Two lines and not one, because a repair tick is
        /// `Work × 100` hits whatever the structure's max — 22% of a plain
        /// road and 0.44% of a container at the live worker's 11 Work — so a
        /// single line makes every repair a one-tick top-up that goes
        /// `task-gone` the tick after it started, and leaves a colony's paving
        /// pinned at the line (live 2026-09-13: 116 roads in W13S28 at a median
        /// of exactly 50.0%, two source containers at a third of max).
        ///
        /// Eight tenths, derived at **RCL6 against a ~1,800 bank and the live
        /// worker row's 11 Work / 12 Carry / 12 Move**. Unlike `RepairTrigger`
        /// this number is **not bank-blind**: what it reads below the stage and
        /// the bank is the 600 energy that body carries, and a smaller bank
        /// casts a smaller worker that buys fewer of the band's hits per trip.
        /// A band a **single load** closes is what makes the ratchet a ratchet
        /// — a body that empties mid-repair is released `inapplicable`, its
        /// target is unheld the next tick and is judged at the hungry line
        /// again, wherever the load ran out. At 100 hits an energy a 600-energy
        /// load is 60,000 hits, against a plain road's whole band of 1,500 hits
        /// (15 energy) and a swamp road's 7,500 (75): every road this colony
        /// has closes its band in one trip many times over.
        ///
        /// **The container knowingly does not, and that is the price this
        /// number is chosen at.** 0.3 × 250,000 = 75,000 hits = 750 energy,
        /// one and a quarter loads, so a container entered at the hungry line
        /// runs its holder dry around 0.74 and is released `inapplicable`
        /// rather than `task-gone`; from the live containers' 30% no whole line
        /// above 0.524 closes in one trip at all. What the two lines exist to
        /// remove is the **churn**, and that is removed either way — one match
        /// and one release per repair job instead of per repair tick, and a
        /// container left at 0.74 is over the hungry line and does not re-enter
        /// the pool. Dropping the line to 0.74 would make the one-load reading
        /// true of the container and would cost every road the 0.74–0.8 band it
        /// closes for three energy, which is 3,000 ticks of passive decay
        /// apiece; the number is left where the decision put it and re-derived
        /// on #323 rather than moved here. ADR 0061's landing check reads per
        /// kind because of this: `task-gone` on the roads, `inapplicable` on
        /// the container.
        ///
        /// The decaying kinds alone (`WholeLine.Fraction`): a [[rampart]] is
        /// judged against `RampartFloor` and a [[keep]] structure against full
        /// hits, and neither has a second number to make.
        RepairWholeLine: float
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
        /// How many whole bodies of the colony's own bank the Storage keeps
        /// back before the upgrader row may spend any of it (#385). **Counted
        /// in bodies rather than in energy** so it scales with the room:
        /// 36,000 at RCL5's 1,800 bank, 46,000 at RCL6's 2,300, 106,000 at
        /// RCL7's 5,300, and 246,000 at RCL8's 12,300 — a quarter of a full
        /// Storage there, which is the one level where this floor is large
        /// enough to be worth re-reading, and also the level where an extra
        /// mouth is worth least (the engine caps an owned RCL8 controller at
        /// 15 e/t however many stand at it).
        ///
        /// What it is a floor *for* is the colony's ability to re-cast itself:
        /// twenty bodies is the whole workforce of the largest colony this bot
        /// has run, once over. **Twenty is argued and not measured** — the
        /// harness cannot stand a full Storage yet (#343), which is what would
        /// let it be. The stock the worker row's backlog term draws on (#364)
        /// is *not* reserved beneath it: both terms charge the sites first and
        /// then draw on the same remainder, and what bounds the overlap is this
        /// floor rather than an accounting between them.
        UpgradeStockBodies: int
        /// The pile a Pickup is worth walking for: a dropped pile enters the
        /// pool at this many units and never below it. A hundred, derived at
        /// the **300 bank** — two Carry parts' worth, the smallest load that
        /// pays for a walk made for the pile alone.
        ///
        /// One number over **both** resources (#311), because what it prices is
        /// the trip and not the cargo: a walk made for a pile alone has to
        /// carry more than two Carry parts' worth whatever is in it. A Thorium
        /// pile under the line is left where it lies and is gone inside
        /// `amount` ticks by its own decay, the penalty being a flat one a tick
        /// below a thousand.
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
        /// The level the engine unlocks the terminal at (`CONTROLLER_STRUCTURES`
        /// for "terminal": 1 from RCL6). Beside `StorageLevel` and read the
        /// same way (#349): the Layout **reserves** the terminal's tile at this
        /// level from level 0, because its pick sits inside the clustered
        /// ordering and an extension that takes that tile never gives it back —
        /// and it *places* at the room's own level, so nothing goes up before
        /// the engine allows it.
        TerminalLevel: int
        /// The Work parts one Move carries on the **miner** row (ADR 0057
        /// decision 2). Five, derived at the **2,300 bank**: the body walks to
        /// one tile once and then never leaves it, so its Move is bought for a
        /// single commute and not for ADR 0003's fatigue parity, which is a
        /// rule about a body that keeps moving. Changing it would be a
        /// different colony — a miner that reaches its Post sooner and digs
        /// less — where `EXTRACTOR_COOLDOWN` beside it would be a lie about the
        /// server.
        MinerWorkPerMove: int
        /// The ticks of life a [[miner]] burns in one tick, which is what
        /// turns `CREEP_LIFE_TIME` into the 500-tick life its replacement is
        /// amortized at (ADR 0057's Consequences, where the programme's
        /// ~220,000 energy is priced off that same number and its twenty-six
        /// bodies). The **engine** fact beside it is a formula and not a
        /// number: `thorium.js` sums `store.T` over a tile, takes
        /// `p = floor(log10 total)` and subtracts it from the `ageTime` of
        /// everything standing there, so a body burns `1 + p`. Three is what
        /// `p = 2` makes of that, and `p = 2` is a fact about **our haul** —
        /// the mineral container held in the 100..999 band — which is why the
        /// number is a knob of this colony's where `EXTRACTOR_COOLDOWN` beside
        /// it would be a lie about the server.
        ///
        /// It is a **policy assumption and not a bound**, and since #306 the
        /// policy is one the code states: the draw is escalated two rungs at
        /// `MineContactCliff` below, so the haul it assumes is the haul the
        /// ladder now asks for, and a container drawn there cycles 0..999 at a
        /// mean `1 + p` of ≈2.9 — ≈3.0 counting the window it goes on filling
        /// through while a hauler walks in. **Three stands**; the honest four
        /// of a container left at its 2,000 cap is the state #306 ended, and the
        /// 3.44 once written here was the arithmetic of a fill run all the way
        /// to that cap.
        ///
        /// It is a floor and not a bound all the same, and #313 is **unblocked
        /// rather than settled** by that: the rung wins only *within*
        /// `StockDraw`, so a colony saturated on Feeding-tier work still leaves
        /// the container to fill, and what #313 has to decide is what to charge
        /// for that colony rather than for this one.
        MineContactAgeing: int
        /// The load on the mine tile at which the colony escalates the mineral
        /// container's [[withdraw]] two rungs (#306) — the line the field above
        /// is the other end of, which is why the two stand together here and
        /// neither is an `Engine` binding. The **engine** fact is the formula
        /// (`thorium.js`, `docs/research/thorium-reactor.md` §2): `p = floor(log10
        /// total)` over everything with a `store` on the tile, a body burning
        /// `1 + p` ticks of life a tick, and a cliff at *every* decade. Which
        /// decade the haul is run at is this colony's policy and no constant of
        /// the server's — there is no `CONTACT_PENALTY_CLIFF` to name.
        ///
        /// A thousand, on the arithmetic of the two states rather than on the
        /// decade being the tidiest: drawn here the container cycles 0..999 and
        /// the [[miner]] over it averages `1 + p ≈ 2.9` (≈3.0 counting the
        /// window the container goes on filling while a hauler walks in), which
        /// is the three `MineContactAgeing` is written for; left to the 2,000
        /// cap it cycles 500..2,000 — one 1,500-carry load off the cap — and
        /// ~300 of those ~450 ticks stand over the cliff, averaging **3.67** and
        /// buying the row a third more bodies for the same ore. Moving it up a
        /// decade would be the second of those, and moving it down spends trips
        /// on a container that is not yet bleeding.
        MineContactCliff: int
        /// Thorium carried on one Reactor delivery (#319, resized by #354).
        /// Under the 1,000-unit contact cliff, so the loaded courier burns
        /// three ticks of life per movement tick rather than four — every
        /// value from 100 to 999 buys that, and which one is chosen is a
        /// question about the **Reactor's** store, not the courier's.
        ///
        /// The Reactor holds 1,000 and burns exactly 1 T a tick, so what a
        /// load must satisfy is `load + walk < reactorCapacity`: the draw is
        /// gated on the store having room for a whole load (`Planner`), and
        /// the store goes on draining for the whole loaded walk. 500 against
        /// W15S28's 318-tick leg leaves the store oscillating 182..682 — a
        /// **182-tick margin** against the one thing that must never happen,
        /// the store reaching zero and the streak resetting to 1 pt/T.
        ///
        /// 999 was the first value and it was too large by exactly this
        /// argument: no gate can both admit a 999 load into a 1,000 store and
        /// leave the store non-empty at the courier's arrival, so the load
        /// arrived unplaceable, the courier stood on its own hot tile at 3×
        /// ageing waiting for room, and died holding ore. 915 T reached the
        /// Reactor room's floor that way (#354).
        ReactorLoad: int
        /// The slack a delivery gate leaves over its own arithmetic, as a
        /// percentage (#378). `Emitter.outlivesTheLoadedLeg` admits a body on
        /// `TicksToLive >= walk * MineContactAgeing`, and written as that bare
        /// equality it is a gate that fails on any difference between the walk
        /// the Atlas prices and the walk the body makes: a [[flee]] (ADR 0033),
        /// a [[keeper margin]] detour, one swamp step, a tick spent yielding.
        /// Live, `hauler-558190` drew with about two ticks of margin over a
        /// 196-tick leg and died in the Reactor's own room with 500 T aboard,
        /// which is the whole cost of the missing slack: not a body, the ore.
        /// A percentage rather than a fixed number of ticks because what it
        /// insures against scales with the leg — a three-crossing delivery has
        /// three borders to be pushed back from and a short haul has none.
        DeliveryLifeMargin: int
        /// The energy a terminal is kept stocked with, to pay `send`'s fee out
        /// of (#349). Energy sitting in a terminal is energy out of the
        /// economy — it buys no body and upgrades no controller — so this is
        /// sized off the job and not off the store: the fee is
        /// `Engine.sendFee`, about 95 energy a thousand units over the three
        /// rooms between W12S28 and W15S28, so 4,000 ships the whole 36,484 T
        /// banked in the two shipping colonies with room to spare.
        ///
        /// Not sized off the terminal's 300,000 capacity, which is the mistake
        /// the mirror of this field would make: a terminal stocked to capacity
        /// would hold four colonies' worth of spawning energy to move ore that
        /// costs a twentieth of it.
        TerminalEnergy: int
        /// How many of a generalist's 1,500 ticks the worker row's backlog term
        /// assumes it spends **building** (#364). Not the lifetime, which is
        /// what the term shipped with and what made it dead code on the colony
        /// it was written for.
        ///
        /// Measured live at W13S28 on 2026-09-17, two windows over the terminal
        /// site: **+3,985 over 120 ticks (33/tick)** with three workers on the
        /// row, and **+1,666 over 108 ticks (15.4/tick)** with one to two. One
        /// 16-Work body ought to put 80 a tick into a site, so the row was
        /// running at a tenth to a fifth of its nominal rate, and a term that
        /// divided by the whole lifetime answered "one body is enough" for a
        /// backlog of 96,465 that had not moved in thousands of ticks.
        ///
        /// Where the rest of the time goes is the body's own carry cycle, and
        /// the arithmetic agrees with the measurement: 16 Work drains the 16
        /// Carry beside it in ten ticks, and then the body walks to the Storage
        /// and back. Ten ticks of building against a twenty-tick round trip is
        /// a third at best, less as the site gets further from the stock — so
        /// 300 is the measured fifth and not a guess at the geometry.
        ///
        /// Sized at the **optimistic** end of the measured band on purpose: too
        /// small a figure hires a crowd, and a crowd costs crowding (ADR 0020),
        /// CPU (`docs/research/cpu-headroom.md`) and the energy of the bodies
        /// themselves. The term is allowed to under-hire.
        BuildTicksPerLife: int
        /// Ticks between courier casts while the delivery programme is open
        /// (#319). The loaded body's Atlas walk is 318 ticks over W15S28's
        /// 154-unit route, so a courier cast this often is always fresh
        /// enough to carry one.
        ///
        /// It is **not** what regulates supply (#354): 999 T every 636 ticks
        /// against a 1 T/tick burn is 1.57 T a tick, and the surplus has
        /// nowhere to be but a courier's store or the floor. Supply is
        /// regulated by the draw gate on the Reactor's own store instead, and
        /// this interval now only keeps a *body* in the programme.
        DeliveryInterval: int
        /// Ticks the [[re-claimer]] relief is meant to stand beside its
        /// incumbent (#329). Added both to the incumbent's replacement lead
        /// and to `Reclaim`'s capacity handover window; either half alone is
        /// inert. Twenty-five preserves ADR 0057's chosen disruption margin
        /// while the oven and route remain derived from the Atlas.
        ReclaimerOverlap: int
        /// **How far ahead** the Layout *places*, in controller levels (ADR
        /// 0011, ADR 0063, ADR 0064): the horizon is `controller.Level + this`,
        /// so the clustered kinds are sized one level above the room's own and
        /// a room that levels tonight has already drawn the tiles tomorrow
        /// unlocks. The lookahead is the half of the horizon this bot chose;
        /// the level it is added to is read off the server, and that is why the
        /// *level* is no longer a field here (`Tuning.horizonOf` derives the
        /// horizon, ADR 0063 replacing ADR 0039's and ADR 0055's absolute
        /// constants).
        ///
        /// It is **not** what the trunks route around. Since ADR 0064 the road
        /// reservation is sized at `allowanceOf`'s ceiling and reads no level
        /// and no lookahead at all, so moving this field moves which tiles the
        /// cluster takes and never which tiles the roads avoid — a road plan
        /// that moved with the level was 589 orphaned tiles over the capture
        /// sweep (#341, #342).
        ///
        /// One, which is ADR 0011's standing bargain re-stated relatively and
        /// unchanged by ADR 0063: placing four levels out draws tiles for a
        /// colony that may never get there. Zero is a meaningful setting and
        /// means no lookahead at all — the clustered kinds sized at the level
        /// they are filtered at — which is what the horizon existed to avoid,
        /// and what a *stale* absolute constant used to produce by accident:
        /// sized at 6 and filtered at 7, an RCL7 room
        /// computed an extension gap of zero and planned none of the ten the
        /// engine had just unlocked (#341).
        HorizonLookahead: int
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
        /// How long an armed [[threat]] seen in a declared [[outpost]] is
        /// remembered once the room goes dark (#366): the ticks the guard row
        /// and the Guard Task keep answering for a room nothing of ours can
        /// see any more, counted from the last tick vision showed the threat.
        ///
        /// **1,500: the longest a raider can still be standing there.** The
        /// clock's job is not what #366 first sized it for.
        ///
        /// It was 300 — 150 ticks of oven (3 a part, five `guardPattern`
        /// blocks) plus 150 of walk (about 50 a room crossing at
        /// `Tuning.MaxHops` of 3) — on the argument that the memory has to
        /// cover the guard's cast and its journey. That is the right size for
        /// **sending** the guard and the wrong size for **remembering**,
        /// because of what expiry means while the room is dark: the entry
        /// disappears, nothing looked, and the row reads the absence as a room
        /// that is clear. An invader living out its full life outlasts 300 four
        /// times over.
        ///
        /// Live, the same day #366 landed, W15S29 killed four of W15S28's
        /// bodies that way — anchor-516370 and reserver-517630 (t517,880 and
        /// t517,964), reserver-518659 (t518,726), reserver-519082 (t519,150),
        /// all at range 1, all to one invader of 1 ATTACK and 1 RANGED_ATTACK.
        /// The guard went in, the memory expired while the raider stayed, the
        /// room read clear because nobody could see it, and the row sent the
        /// next unarmed body. **A remembered threat with too short a clock is
        /// worse than no memory at all**: it buys a body, walks it in, and then
        /// forgets.
        ///
        /// So the clock is now a backstop and not a schedule:
        /// `Engine.creepLifetime`, the most life any raider can have left when
        /// it was last seen. What ends a memory in ordinary running is a
        /// **look** — a tick of vision that finds the room clear removes it,
        /// and the guard standing in the room is itself that look, so arrival
        /// either clears the entry or renews it. This number only stops a latch
        /// outliving every possible raider, which matters for a room nothing of
        /// ours ever visits again.
        ///
        /// Being this long costs a guard hired for a room that went quiet
        /// unseen: 750 energy a block, one trip, cleared on arrival. It is now
        /// under `StandDownFallback` (2,500) rather than far under it, and the
        /// difference from a [[stand-down]] is unchanged — this memory **sends
        /// a body in**.
        ThreatMemory: int
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
    /// (`Keepers`, ADR 0060 decision 2). Six today, and **derived rather than
    /// chosen** — a function beside the record and not a field in it, so a
    /// human who moves `ReachMargin` moves this too and cannot re-open the
    /// decision in silence:
    ///
    ///     the keeper's pin (1) + its longest weapon (3) + ReachMargin (2)
    ///
    /// Five is the right number for **survival** — a keeper's longest reach is
    /// ranged 3, so a tile 6 from its rock is at least 5 from the keeper and
    /// out of range — and it is the wrong number for what this buys. ADR 0033's
    /// Reach is `weapon + ReachMargin`, which is 5 for a RANGED_ATTACK body, so
    /// a tile at 5 from a keeper is *inside* that Reach: at a margin of five a
    /// courier crossing the room is a creep inside a Reach, [[flee]] is
    /// applicable to it, and the whole decision buys nothing.
    ///
    /// At six, a walkable tile is at least 7 from every rock, at least 6 from
    /// every **pinned** keeper, and so outside its Reach — no Work Area is
    /// subtracted to empty out there, no body of ours stands inside a Reach,
    /// and Flee is inapplicable in that room by geometry rather than by
    /// exempting any row from ADR 0033. ADR 0060 wrote "by construction" and
    /// that is one word too strong: it holds for the steady state, which a
    /// keeper is in on almost every tick but not on the two to four after each
    /// respawn, when it is walking from its lair to the rock it just adopted
    /// (`Engine.keeperPin`, #327). No larger margin repairs it — past seven
    /// W15S26 cannot be crossed at all — so the exposure is a decision and not
    /// an oversight, and ADR 0033 covers what is left because nothing here
    /// takes a keeper off any hostile list.
    let keeperMargin (tuning: Tuning) : int =
        Engine.keeperPin + Engine.rangedRange + tuning.ReachMargin

    /// The **Layout horizon**: the controller level the clustered kinds are
    /// *placed* at, where the level they are *filtered* at is the room's own
    /// (ADR 0011, ADR 0063). It sizes the placement alone — the reservation the
    /// trunks route around is `Layout.allowanceCeiling`'s and reads no level
    /// (ADR 0064). Derived rather than chosen, for `keeperMargin`'s reason and
    /// one of its own — a function beside the record and not a field in it:
    ///
    ///     max 0 (controller.Level + HorizonLookahead)
    ///
    /// The horizon had been an absolute constant since ADR 0011, and an
    /// absolute constant naming a level the room will reach is a number that
    /// goes stale the tick the room reaches it. It did, three times: ADR 0011
    /// set it at RCL4, ADR 0039 moved it to RCL5, ADR 0055 to RCL6, and #341
    /// caught the fourth — W12S28 standing at RCL7 under a horizon of 6,
    /// computing an extension gap of `40 − 40 = 0` and asking for none of the
    /// ten the engine had unlocked. Derived off the level, there is no fourth
    /// time: the horizon moves when the room does, on the same tick, without a
    /// human in the loop.
    ///
    /// It stops moving on its own at the **top**, and needs no clamp to: RCL8
    /// asks for a horizon of 9, and `allowanceOf`'s catch-all answers 9 exactly
    /// what it answers 8, so a maxed room places its own terminal allowance
    /// and nothing beyond it — the level `Layout.allowanceCeiling` climbs to,
    /// reached here from below instead of read off the table.
    ///
    /// The **bottom** is clamped, because `allowanceOf`'s catch-all is
    /// two-sided and answers a *negative* level the same sixty extensions and
    /// six towers it answers RCL8: without the clamp a lookahead of −2 or lower
    /// would hand a young room the **widest** window in the table instead of
    /// the narrowest, and place sixty extensions in a room the engine allows
    /// five. That is the opposite of what the field says it does — and it is a
    /// claim about the *placement* only, since ADR 0064: the reservation is the
    /// widest window at every level by design, and no lookahead reaches it. A
    /// horizon of zero allows no clustered structure at all, which is the floor
    /// the level itself has, and `QuotaTuningTests` reads the clamp at a
    /// lookahead that reaches past it.
    ///
    /// A level is a server fact and the lookahead is this bot's choice, which
    /// is the cut ADR 0052 decision 5 draws through `Tuning`: the chosen half
    /// stays a field, the read half never becomes one.
    let horizonOf (tuning: Tuning) (level: int) : int = max 0 (level + tuning.HorizonLookahead)
