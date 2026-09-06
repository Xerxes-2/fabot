module Fabot.Core.Types

/// A creep body part, the engine's full vocabulary. Our own bodies use
/// only Work/Carry/Move today; the rest arrive on hostile creeps, whose
/// parts the [[colony view]] projects verbatim.
///
/// `Claim` is spelled `BodyPart.Claim` wherever it means a part, because
/// `Task.Claim` (ADR 0047) shares the name and is declared later, so the
/// bare word resolves to the Task. Both names are the engine's own — the
/// part is CLAIM and the act is `claimController` — so neither was renamed
/// to dodge the other, and the qualification is where the two are told
/// apart. The whole union is not `RequireQualifiedAccess` for the reason
/// the qualification is bearable: `Work`, `Carry` and `Move` are written
/// in every body literal in the codebase and collide with nothing.
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

/// The engine's own numbers (ADR 0052 decision 5): the constants Screeps
/// fixes, each named for the server constant it spells so a reader can
/// check it against the game rather than against this file.
///
/// The line between this module and `Tuning` below is the whole point of
/// having two: a number belongs here when changing it would be a **lie
/// about the server**, and there when changing it would be a **different
/// colony**. So nothing here carries a stage or a bank it was derived at,
/// and nothing here needs a pairwise test — there is nothing to choose.
/// A number that turned out to be a choice wearing an engine constant's
/// clothes moves the other way, and the move is visible in the diff.
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

    /// CREEP_CLAIM_LIFE_TIME: the ticks a body carrying a CLAIM part
    /// lives, well short of the 1,500 every other row gets. The
    /// reservation deficit's divisor and the reserver row's amortization
    /// both read it.
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

    /// The regeneration of a source in a room carrying an owner or a
    /// reservation: 3,000 energy per 300 ticks. What a continuously
    /// drained rock yields there, and the ceiling on what any body
    /// standing over it can take out (ADR 0042).
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

/// The colony's **tunables**, in one record (ADR 0052 decision 5): every
/// number the bot chose rather than read off the server, carried on the
/// [[colony view]] so a rule reads its colony's own and a test can hand it
/// another.
///
/// Each field says the [[stage]] and the bank it was derived at, and what
/// it reads below them — because that is exactly the debt ADR 0052 was
/// written against: eight tickets in two days, each a constant of the one
/// RCL5 home spelled as if it were a colony fact. **A number without a
/// pairwise test is not a tunable; it is a bug that has not happened yet**
/// (decision 5), so every field below is pinned at two banks or at two
/// stages in `tuningTests`.
///
/// What is *not* here is the engine's own arithmetic (`Engine` above):
/// retuning HARVEST_POWER does not describe a colony that plays
/// differently, it describes a server that does not exist.
type Tuning =
    {
        /// The Workforce target's floor: the colony never plans below this
        /// many living creeps. Two keep the harvest/refill loop running
        /// while one is in transit or being replaced.
        ///
        /// A body count and not a price, so it is the same two at a 300
        /// bank and at an 1,800 one, and the same two at every stage: what
        /// changes with the bank is what those two bodies are, which is
        /// each row's sizing rule and not this floor.
        MinWorkforce: int
        /// The Repair trigger of the decaying kinds: a road or a container
        /// enters the pool when its hits sink strictly below this fraction
        /// of max, and leaves it once repaired back over the line. A
        /// tunable, not part of ADR 0010.
        ///
        /// A fraction of the structure's own max, so it is bank-blind and
        /// stage-blind by construction — what it costs to hold the line
        /// scales with the structure and never with the colony.
        RepairTrigger: float
        /// The rampart floor (ADR 0034): a rampart is hungry below this
        /// many hits and whole at it. A derived tunable — the ticks the
        /// room must hold times the damage per tick it must hold against.
        /// Against the squad of #66, 180 hits a tick, 100,000 hits is 555
        /// ticks, two and a half times the raid that was seen. That costs
        /// 1,000 energy to raise and, at 300 hits of decay per 100 ticks,
        /// one Repair visit per rampart every 200 ticks to hold — so no
        /// hysteresis is needed: one visit at 600 hits a tick puts a
        /// rampart that just dipped back over the line.
        ///
        /// Derived at `Independent` with a tower and a Storage behind it,
        /// and **read at no other stage**: a colony under the line keeps no
        /// rampart at all (`keepsRamparts`, #214), so the number is never
        /// asked for at a 300 bank rather than being asked and answered
        /// wrongly there.
        RampartFloor: int
        /// The pile a Pickup is worth walking for (#167): a dropped pile
        /// enters the pool at this many energy and never below it. A
        /// tunable beside `RepairTrigger`, not part of any ADR.
        ///
        /// A hundred, derived at the **300 bank**, which is the poorest
        /// colony that can hire a hauler at all: the row's floor body is
        /// one `[Carry; Carry; Move]` block, so two Carry parts' worth is
        /// the smallest load that pays for a walk made for the pile alone.
        /// A richer bank buys a bigger body and only makes the same walk
        /// pay sooner, so the line does not move with the bank; under the
        /// line the pile is left to decay at a thousandth a tick, or to
        /// the next creep that passes it — the [[pickup reflex]] already
        /// takes every pile a creep happens to stand beside.
        PickupThreshold: int
        /// The Reach margin (ADR 0033): the tiles a Threat's weapon range
        /// is widened by — one for the hostile's next step, one for our own
        /// tick of lag. A tunable beside `RepairTrigger`, not a term of the
        /// decision.
        ///
        /// Tiles of lag, so it is the same two at every stage and every
        /// bank: what a raider covers in a tick is its body's business and
        /// not the colony's.
        ReachMargin: int
        /// The [[standing body]]'s line (ADR 0046): the Carry parts per
        /// Work at which a delivery stops being work and becomes a commute.
        /// A body under it carries fifty energy a trip against eleven Work,
        /// so a Build or a Refill it walks to spends one tick delivering
        /// for every tick of the walk out and the walk back.
        ///
        /// A ratio over one body's own parts, so it is bank-free as
        /// written; what the bank moves is which casts fall on either side
        /// of it. The band it governs is between the two rows that are
        /// outside it by construction — a hauler is `Carry * n < 0`, false
        /// whatever `n` is, and the worker row's parity buys one Carry per
        /// Work, four times clear at every bank — and the one live edge is
        /// the **800 bank**, where the upgrader row's own cast reaches five
        /// pairs and becomes a standing body (`upgraderBodyFor`,
        /// `isStandingCast`). Under it the row is not countable and its
        /// quota is zero.
        StandingCarryPerWork: int
        /// The **pioneers**: how many more [[worker unit]]s a mother hires
        /// while a [[nursery]] or a bootstrapping child of hers stands (ADR
        /// 0047 decision 4). The addend on the worker row's own share of
        /// the target, the cap on the borrowed Upgrade and the borrowed
        /// Build (`planPool`'s Capacity, #213), and the whole of what a child
        /// costs the mother in bodies.
        ///
        /// Three, derived at the mother's `Independent` **1,800 bank**,
        /// which is the only stage that has bodies to lend: a spawn is
        /// 15,000 progress against a generalist's fifty energy a trip, so
        /// the child's first tick is many round trips away whatever the
        /// crowd, and the crowd decides whether that is this cycle or the
        /// next. Small because each of these bodies is one the mother's own
        /// surplus work does without for the length of the walk. A **flat**
        /// addend and not one per child: two children at once share these
        /// three, which is a state a human who declared the second can see
        /// and retune.
        PioneerCount: int
        /// The **[[ferry]]**: the hauler bodies a mother hires against a
        /// bootstrapping child's upgrade buffer, over and above the haul
        /// her own containers ask for (#222, ADR 0052 decision 7).
        ///
        /// One, derived at the mother's `Independent` **1,800 bank** and
        /// read at no other stage — it is hired for a child at
        /// `Bootstrapping` and a colony with no child hires none. One body
        /// because the ferry is a lend and not a second economy: the
        /// user's target is "RCL3 不用几小时" against a child earning
        /// eight a tick on its own, and one hauler of the mother's 1,800
        /// bank carries ten Carry parts — five hundred energy a trip
        /// against the four hundred and fifty a pioneer walks home for.
        /// The cap is the point of it (decision 7): what a mother lends is
        /// written down and bounded, never derived from how much the child
        /// could absorb.
        ///
        /// **The two halves ship together** (#216 R5). Until the Refill
        /// half landed this field was zero, because a body hired here had
        /// no Task to be matched to — the mother's view carried none of a
        /// child's stores, so nothing pooled the buffer she was hiring
        /// against, and the hire would have come ahead of the upgrader and
        /// worker rows that would otherwise have spent the energy. Since
        /// R5 `ColonyView.ofWorld` carries that one store, `planTasks`
        /// pools its Refill and denies its Withdraw, and the quota's own
        /// term prices the round trip to the very tile the Refill is on
        /// (`ferryBuffers`), so the row and the pool cannot disagree about
        /// how many bodies are crossing or where they are going.
        FerryLoads: int
        /// The claimer range at which safe mode fires (ADR 0015): the
        /// precise deadline is 2 — `attackController` is a range-1 act and
        /// judged from tick-start position, and a creep steps at most one
        /// tile a tick, so activating at 2 always lands before the tap —
        /// plus one tile of margin for a skipped tick.
        ///
        /// Tiles, and the same tiles at every stage: what a bootstrapping
        /// colony has less of is safe modes to spend, which is the
        /// reflex's own `SafeModeAvailable` gate and not this range.
        SafeModeDeadline: int
        /// The level the engine unlocks the Storage at (Screeps
        /// CONTROLLER_STRUCTURES for "storage"). The Layout reserves the
        /// Storage's whole allowance here rather than at the horizon (ADR
        /// 0022): the Storage is not a clustered kind, and its tile never
        /// comes back once an extension takes it, so the reservation must
        /// outlive any revisit of the horizon.
        ///
        /// A **level** and not a stage, deliberately: it is the engine's
        /// own unlock read back as a reservation, so it tracks the server's
        /// table and moves only when that does.
        StorageLevel: int
        /// The Layout horizon (ADR 0011, moved to RCL5 by ADR 0039): the
        /// whole plan is computed up to this level regardless of the
        /// current one, so today's roads route around tomorrow's
        /// structures. One level of lookahead is the standing bargain —
        /// RCL8 would tax today's trunks with detours for structures four
        /// levels away, and a horizon the room has already passed sizes
        /// every clustered gap at zero, so the room stops growing without
        /// saying why.
        ///
        /// Declared and not computed from the current level, which is what
        /// keeps it stepping once, in a commit (ADR 0039) — and so it is
        /// the one field here a `Bootstrapping` colony reads exactly as an
        /// `Independent` one does: a child plans the whole RCL5 room from
        /// its first tick and places only what its level unlocks.
        HorizonLevel: int
        /// How many creeps the colony will have building outpost container
        /// sites at once (#157) — a budget over all of them together and
        /// never a per-site number, because the Planner places one site per
        /// unserved outpost source and places them all on the same tick.
        ///
        /// Two, derived at the `Independent` **1,800 bank** where an
        /// outpost exists at all: these sites sit on the feeding tier and
        /// outbid the home Upgrade for every loaded worker, and travel cost
        /// cannot thin the crowd, so without the budget the whole worker
        /// row crosses the Seam together and the home room's surplus work
        /// stops for the fifty ticks each of them spends walking. A
        /// bootstrapping colony declares no outposts and never reads it.
        OutpostContainerBuilders: int
        /// The controller level a child colony stops being bootstrapped at
        /// (ADR 0047 decision 4) and so the line `Colony.stageOf` cuts
        /// `Bootstrapping` from `Independent` on: **the one place this
        /// number is read** (ADR 0052 decision 3).
        ///
        /// Three, because that is the level a colony can defend and feed
        /// itself at: RCL3 unlocks the first tower and the tenth extension
        /// — 800 energy of bank, enough to cast a body that is not the
        /// 300-energy starter. At it the borrowing rule closes, the
        /// [[pioneer]] addend falls away and the mother stops projecting
        /// the room; under it roads and ramparts are the wrong spend
        /// (#209, #214). A level and not a bank, because what it says is
        /// which rules a colony has grown into, and the bank is one of the
        /// things that follows from it.
        BootstrapLevel: int
        /// What a swamp tile costs a **trunk** (ADR 0011 as #211 amends
        /// it): three against plain's two, where a walking creep pays
        /// `Engine.swampWeight`, ten.
        ///
        /// A trunk is a road, and once paved a swamp tile walks at exactly
        /// what a paved plain tile walks at (road 1, ADR 0010); the only
        /// thing swamp costs a road is construction — 1,500 against 300, a
        /// one-off 1,200 — and repair is identical either way. Three is the
        /// surcharge that amortizes that: a swamp step is one more than a
        /// plain step, so the router takes swamp when it saves a step and
        /// avoids it when a plain step is free. Not equal weights: with a
        /// tie the flood's index order picks, and a plain step really is
        /// 1,200 cheaper. It stays at or under `Engine.swampWeight`,
        /// which the flood's step table is sized by — `Atlas.trunkPath`
        /// holds it there rather than trusting the number, because a
        /// weight past the table's length is a price index off the end of
        /// it: a throw on .NET and an `undefined` price on the deployed
        /// bundle, from a field a human may move.
        ///
        /// Derived at `Independent`, the only stage that places a road at
        /// all (`placesRoads`, #209); the trunks are still *routed* at
        /// every stage (ADR 0011's "computed whole"), so a bootstrapping
        /// colony pays the number in its container picks and not in
        /// construction.
        TrunkSwampWeight: int
        /// The [[stand-down]] a threat gave no readable deadline for:
        /// 2,500 ticks, the stronghold expansion period (ADR 0043). The
        /// last of the three answers and the only one the colony chose
        /// rather than read, so it is the one number there that had to be
        /// justified: it is the cadence on which the thing that put the
        /// core there puts another one somewhere, and it errs long by
        /// construction, which is the only direction the gate is allowed to
        /// be wrong in — a stale stand-down costs an outpost's income until
        /// the clock runs out, and the failure it prevents costs a creep a
        /// cycle for the life of the core.
        ///
        /// Derived at `Independent`, the only stage that declares an
        /// outpost to stand down from.
        StandDownFallback: int
        /// Ticks of silence that close a [[raid]] episode (ADR 0028). It
        /// has to outlast a poke-and-heal cycle: giaco's squad in #66
        /// stepped in for a tick or two at the tower's minimum damage and
        /// back out to heal, over and over across ~220 ticks, and that is
        /// one raid, not forty. Fifty ticks is also about the round trip a
        /// squad retreating off-room makes before it can be back — a
        /// shorter absence is the same squad still working the room, a
        /// longer one is a decision to leave.
        ///
        /// A raider's clock and not a colony's, so it is the same fifty at
        /// every stage and every bank.
        QuietGap: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tuning =
    /// The numbers this bot ships with: what the shell hands every colony
    /// (`World.ofGame`), and what a test starts from before it moves the
    /// one field it is about. There is one set and not one per stage —
    /// each field states the stage it was derived at, and the rules that
    /// read it branch on the [[stage]] themselves (ADR 0052 decision 3),
    /// so a second table would be a second place for a stage to be
    /// decided.
    let defaults =
        {
            MinWorkforce = 2
            RepairTrigger = 0.5
            RampartFloor = 100_000
            PickupThreshold = 100
            ReachMargin = 2
            StandingCarryPerWork = 4
            PioneerCount = 3
            FerryLoads = 1
            SafeModeDeadline = 3
            StorageLevel = 4
            HorizonLevel = 5
            OutpostContainerBuilders = 2
            BootstrapLevel = 3
            TrunkSwampWeight = 3
            StandDownFallback = 2500
            QuietGap = 50
        }

/// What the decision layer knows about one spawn this tick.
type SpawnInfo =
    {
        Name: string
        /// Game-object id of the spawn structure — the key that locates
        /// this spawn in the spatial projection's target maps.
        Id: string
        /// Name of the room the spawn stands in — the key into the
        /// world's per-room banks (`RoomFacts.Energy`).
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

/// Whose CLAIM parts hold one room's reservation, as the colony reads it:
/// three answers and not a username, the same closed shape and for the
/// same reason as `Ownership` below.
///
/// The third answer is load-bearing and is not a refinement of the second.
/// ADR 0043 gives an NPC invader's reservation and another *player's*
/// opposite meanings: the Invader's is the **clock** a [[stand-down]] runs
/// to where the core carries no collapse timer ("the end of the
/// reservation it has taken"), and a player's is the **clockless**
/// withdrawal, a room that has stopped being ours to work and is never
/// re-entered on a timer. Read through one "not ours" flag the two are the
/// same value, and no correct answer exists for either: an Invader's
/// reservation outliving its core would shut an outpost forever, and a
/// player's credited to a core would reopen a room somebody else holds.
/// So the shell separates them where it holds the username, and Core still
/// never sees one.
[<RequireQualifiedAccess>]
type ReservationHolder =
    /// This colony's own CLAIM parts. The one answer that doubles the
    /// room's sources and the one the reserver row sizes itself from.
    | Ours
    /// The NPC Invader — the user an invader core belongs to, and the
    /// holder of the reservation a level-0 core takes with
    /// `attackController` in a room it expanded into (ADR 0043,
    /// docs/research/remote-mining.md §8.4). Worth the neutral rate like
    /// any hold that is not ours, and, unlike a rival's, an expiry: this
    /// one lapses.
    | Invader
    /// Another player. Worth the neutral rate, and the clockless
    /// withdrawal of ADR 0043 — the one abandonment trigger every mature
    /// bot implements.
    | Rival

/// The reservation standing on one room's controller this tick (ADR
/// 0042): a neutral controller held by CLAIM parts, which doubles every
/// source in that room, decays by one a tick and caps at 5,000.
type ReservationInfo =
    {
        /// Whose CLAIM parts hold it. Whose it is, rather than whose name
        /// it carries: the engine answers holding with a username, the
        /// colony's own name is the shell's to know (the owner of the room
        /// its spawns stand in), the NPC's is a name the shell knows too,
        /// and every rule reading this asks which of the three rather than
        /// which string.
        ///
        /// A reservation somebody else holds reads for *pricing* exactly
        /// as no reservation at all does, and that is a colony decision,
        /// not the engine's arithmetic: `sources/tick.js` switches a
        /// source to 3,000 a cycle on
        /// `roomController.user || roomController.reservation` — **any**
        /// owner, **any** reservation — so a creep of ours digging in a
        /// room a rival holds really would draw ten a tick
        /// (docs/research/remote-mining.md §1.1). The colony prices it at
        /// five deliberately and conservatively: a room somebody else
        /// owns or reserves has stopped being ours to work, it is the one
        /// withdrawal trigger every mature bot implements, and the
        /// [[stand-down]] (ADR 0043) is where the withdrawal itself
        /// lands. Nothing should size a fleet against energy the colony
        /// is about to walk away from. For *withdrawing* the two are not
        /// one answer — see `ReservationHolder`.
        Holder: ReservationHolder
        /// Ticks left on the reservation — what the reserver row's one
        /// rule sizes and quotas from, `ceil((5000 - this) / 600)` CLAIM
        /// parts (ADR 0042, `Decide.reserverClaimsOf`). Read as the
        /// colony's own hold only where `Holder` is `Ours`: a reservation
        /// somebody else holds leaves this colony's own hold at zero,
        /// exactly as it leaves the room's sources at the neutral rate.
        /// Under `Invader` it is the other thing this field is: the
        /// deadline ADR 0043 falls back to when a core carries no collapse
        /// timer.
        ///
        /// The holder and the ticks left are a single engine fact off a
        /// single binding — the reservation object arrives whole or not at
        /// all — so the pair is projected together and the exception the
        /// sentence under `Reservation` below does not cover.
        TicksToEnd: int
    }

/// Whose a room's controller is, as the colony reads it: three answers and
/// not a username. Two of them are what ADR 0042 prices a source from —
/// ours is the held rate, nobody's is half — and the third is what ADR
/// 0043's clockless withdrawal is judged on: a room another player has
/// taken has stopped being ours to work, whatever it yields.
///
/// A closed vocabulary rather than a pair of booleans, because "ours" and
/// "somebody else's" are answers to one question and two flags could carry
/// both at once. It is not the fourth answer, "we cannot see": that one is
/// the absence of the whole entry (ADR 0004), because the question is only
/// asked of a room vision answered for.
[<RequireQualifiedAccess>]
type Ownership =
    /// Nobody owns the controller — the shape every neutral room and every
    /// outpost the colony works arrives in, and the shape a room with no
    /// controller at all is projected as. Reservable, and worth half until
    /// it is reserved.
    | Unowned
    /// This colony owns it: the spawn room, and nothing else while there
    /// is one colony. Worth the held ten a tick, and never reserved — the
    /// engine refuses `reserveController` on a room anybody owns.
    | Ours
    /// Another player owns it. The engine yields ten a tick in a rival's
    /// room exactly as in ours, and the colony prices it at five all the
    /// same, for the reason `ReservationInfo.Holder` gives: a room
    /// somebody else holds is one the colony is withdrawing from (ADR
    /// 0043). No NPC case here beside `ReservationHolder`'s: an invader
    /// core *reserves* and never owns — `expandStronghold` tests
    /// `!controller.user` and `attackController` leaves the owner
    /// untouched — so the NPC is a holder the colony can meet and never an
    /// owner.
    | Rival

/// Who holds one room the colony can see this tick — the fact a source's
/// output is read from (ADR 0042), because ten energy a tick is the
/// *held* rate and a neutral room's source yields five.
///
/// One entry per room vision answered for, home included. A room the
/// colony cannot see has no entry at all, and that absence is not "half":
/// who holds a room we cannot look into is not a fact this tick, so its
/// sources are unpriceable and enter no quota (ADR 0004).
type RoomControlInfo =
    {
        /// Whose the room's controller is (the engine's `controller.my`
        /// and `controller.owner`). Read *beside* the reservation and
        /// never instead of it: the engine gives a room with an owner the
        /// same 3,000 a cycle it gives a reserved one, so a rule spelled
        /// "reserved, or half" would price the colony's own two sources at
        /// five and halve its hauler quota and its income base together.
        Owner: Ownership
        /// The reservation standing on the room's controller; None where
        /// nothing reserves it. *Which* rival holds it is still
        /// deliberately not carried, and that is now the whole of what is
        /// left out: naming one rival apart from another is a name no rule
        /// reads. What the pair above and here do carry is every
        /// *question* ADR 0043 asks of a controller — whether somebody
        /// else holds this room, as `Ownership.Rival` or as a
        /// `ReservationHolder.Rival` reservation, and whether the holder
        /// is instead the NPC whose reservation is a clock rather than an
        /// exit. #133 is the tick both widenings arrived on, and they
        /// arrived as closed three-state answers rather than as usernames
        /// for the reason `ReservationHolder` gives.
        Reservation: ReservationInfo option
        /// Whether the room's controller is under safe mode this tick
        /// (the engine's `controller.safeMode`, a tick count while it
        /// runs). Carried per room and not on the colony's own controller
        /// alone (#218): safe mode shields the room it is in, whoever is
        /// looking — a mother's pioneer standing in a child's room under
        /// safe mode is as safe as the child's own creeps — and only a
        /// room *we* own shields us; a rival's safe mode protects the
        /// rival. False where no controller stands.
        SafeMode: bool
    }

/// A tile of a **named** room: the coordinate a value carries once it
/// leaves the grid it indexes (ADR 0052 decision 2). Two rooms hold the
/// same fifty-by-fifty coordinates, so a bare `Pos` handed between
/// functions is joined to a room by convention alone — the convention that
/// produced a phantom [[post]] (#191), a trunk drawn to another room's
/// spawn, and a hostile in an [[outpost]] measured at range 0 from home
/// (ADR 0041 kept a `RoomName` beside each such field for exactly this).
/// A `RoomPos` is that join written as a type, so the compiler asks the
/// question the convention left to the reader.
///
/// The room is a field of the record rather than a `string * Pos` pair
/// because every reader wants one of the three components and pattern
/// matching a pair to reach a coordinate is what made the pairs unreadable
/// (`Outpost.Sources`, `Movement.Placed`).
///
/// Declared **before** `Pos` and never after it, which is load-bearing and
/// not a matter of order: F# resolves a bare `.X` on an un-annotated value
/// to the *last* record type declaring that field, so a `RoomPos` declared
/// second would silently retype every grid coordinate in the Atlas's
/// unannotated arithmetic. `Pos` last means an unannotated `.X` is a grid
/// tile's, exactly as it was, and a `RoomPos`'s own `.X` resolves off the
/// type its binding already carries.
type RoomPos = { Room: string; X: int; Y: int }

/// A tile coordinate inside a room. Kept as the **grid** coordinate (ADR
/// 0052 decision 2): a key of `RoomLayer.Terrain`, of `Obstacles`, of the
/// flood arrays and of every Seat, Reach and Work-Area grid the Atlas lays
/// per room. It is meaningful only beside the name of the room whose grid
/// it indexes — which is why nothing that crosses a function boundary
/// carries one any more; that is `RoomPos`.
type Pos = { X: int; Y: int }

/// Screeps range: Chebyshev distance between two tiles of **one** room.
/// The one definition — the Atlas's geometry, the two hostile reflexes and
/// the Raid log's closest approach all measure with it. Takes grid
/// coordinates, so every caller has already established the room the two
/// tiles are in; `RoomPos.range` is the same measure for tiles that carry
/// their own rooms and may not share one.
let range (a: Pos) (b: Pos) = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomPos =
    /// The grid coordinate, for indexing that room's own tables. The one
    /// direction that is always safe: dropping the room is fine at the
    /// moment a reader has just decided which room's grid it is reading.
    let pos (tile: RoomPos) : Pos = { X = tile.X; Y = tile.Y }

    /// A room's grid coordinate joined to that room — the conversion every
    /// Atlas query spells as it hands a tile out.
    let at (room: string) (tile: Pos) : RoomPos = { Room = room; X = tile.X; Y = tile.Y }

    /// The whole set of a room's grid tiles, joined to it.
    let setAt (room: string) (tiles: Set<Pos>) : Set<RoomPos> = tiles |> Set.map (at room)

    /// The tiles of one room out of a mixed set, back as grid
    /// coordinates: the read at the other end of `setAt`, for a grid or a
    /// flood that indexes one room.
    let inRoom (room: string) (tiles: Set<RoomPos>) : Set<Pos> =
        tiles |> Set.filter (fun tile -> tile.Room = room) |> Set.map pos

    /// The same narrowing as `inRoom`, answered as a **list** and not a
    /// second set — the shape every flood takes its goals in. Written once
    /// here because it is the hot one (#216 R3): the price, the first
    /// step, the far flood's goals and the Layout's trunk all take a room's
    /// share of an area they were handed, and they take it once per creep
    /// per candidate Task and once per source per goal on a census tick.
    /// Building a `Set<Pos>` at each ask would copy the area per ask and
    /// pay a tree of comparisons for an answer nothing looks anything up
    /// in — the callers index a grid array with it. The order is the set's
    /// reversed, which every caller settles with a total key
    /// (`List.min`/`List.minBy`, or a distance-and-index heap) rather than
    /// with the order it arrived in.
    let tilesIn (room: string) (tiles: Set<RoomPos>) : Pos list =
        ([], tiles)
        ||> Set.fold (fun acc tile -> if tile.Room = room then pos tile :: acc else acc)

    /// Chebyshev range between two tiles that carry their rooms, and
    /// **None** across a border (ADR 0052 decision 2). Not a large number
    /// and not an error: two rooms' coordinate systems are not one metric
    /// space, so there is no range to answer with — a reader measuring a
    /// hostile against something of ours has to decide what a hostile in
    /// another room means, and every reader that decided it by accident
    /// decided "range 0" (ADR 0041, #204).
    let range (a: RoomPos) (b: RoomPos) : int option =
        if a.Room = b.Room then
            Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
        else
            None

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
    /// A dropped energy pile. Two readers now: the [[pickup reflex]],
    /// which takes what is already at a creep's feet and reads no amount
    /// at all, and the Pickup Task (#167), which walks a hauler to a pile
    /// big enough to be worth the trip and reads the amount out of
    /// `SpatialInfo.Stores` like any other store. The amount is the one
    /// field this kind grew for the Task; the reflex is unchanged and
    /// still asks only where a pile is.
    | Dropped
    /// A tombstone or a ruin: a store with a clock on it. One kind for
    /// both engine objects (#167), because the only thing any reader
    /// decides on is that it holds energy and will be gone — a tombstone
    /// in a hundred ticks, a ruin on its own decay — and `Withdraw` is
    /// the verb for either. Its energy rides `SpatialInfo.Stores` beside
    /// the containers', so the Withdraw pool, its stock cap (#161) and
    /// its tier read it through the rules they already had.
    ///
    /// Never an obstacle: a tombstone stands on the tile a creep died on,
    /// which may be a Seat or a Post, and the engine lets a creep walk
    /// over both objects. Transient like a pile, and kept off the
    /// Layout's ground for the same reason (`isTransient`).
    | Tombstone

/// Whether a projected target is one of the two transient kinds — a pile
/// or a tombstone/ruin — that stand on a tile without holding it. Both
/// vanish on their own within a few hundred ticks, so a census that let
/// one keep a construction site off its tile would make the Layout's
/// ordering depend on where a creep happened to die (ADR 0011's
/// determinism). Read by `Atlas.buildableTilesIn`, which is the one census
/// that walks every placed target rather than picking a kind.
let isTransient =
    function
    | Dropped
    | Tombstone -> true
    | Source
    | Controller
    | Structure _
    | Site _ -> false

/// One room's geometry, filed under that room's name (ADR 0041): every
/// container the projection keys by `Pos` or fills with `Pos`es, gathered
/// into one record rather than five maps side by side, so reading a
/// room's geometry is one lookup and not five. The id-keyed containers
/// (target kinds, hits, stores) stay outside it, because an object id is
/// already unique across the world and layering it would key a unique
/// thing twice.
///
/// Absence stays per entry (ADR 0004) and now says one more thing: a room
/// missing entry by entry inside its layer and a room with no layer at all
/// are the same answer, so a room's geometry is read as
/// `Map.tryFind name spatial.Rooms |> Option.defaultValue RoomLayer.empty`
/// and never as `.[name]`, which throws on a room the projection names but
/// has no geometry for. Neither absence is a state of its own — an outpost
/// the colony cannot currently see is unpriceable geometry and nothing
/// more.
type RoomLayer =
    {
        /// Terrain per tile over this room's ground (x,y in 1..48); a tile
        /// absent from the map is impassable. The border ring is not here
        /// and is not ground: it rides in `SpatialInfo.Borders`, which the
        /// Seam query alone is priced off (ADR 0036, ADR 0041).
        Terrain: Map<Pos, Terrain>
        /// Target id -> that target's tile in this room: the Task targets
        /// (source, refillable structure, construction site, controller,
        /// and since #167 the piles and tombstones a hauler is sent to)
        /// and the dropped piles the pickup reflex reads. The two
        /// transient kinds are filtered out by kind where standing on a
        /// tile is not the same as holding it (`isTransient`,
        /// `Atlas.buildableTilesIn`).
        TargetPositions: Map<string, Pos>
        /// Creep name -> the tile the creep stands on in this room.
        CreepPositions: Map<string, Pos>
        /// Tiles blocked by obstacle structures (spawn, extension,
        /// controller, ...) and by their construction sites — the engine
        /// refuses to move a creep onto its own obstacle-type site;
        /// impassable regardless of terrain.
        Obstacles: Set<Pos>
        /// Tiles holding a built road — built structures only, a road
        /// construction site is not yet a road (ADR 0010).
        Roads: Set<Pos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomLayer =
    /// A room with nothing in it — every entry absent. What a `tryFind` on
    /// `SpatialInfo.Rooms` defaults to, so a room the projection holds no
    /// geometry for reads the same as one whose every container is empty
    /// (ADR 0004).
    let empty: RoomLayer =
        {
            Terrain = Map.empty
            TargetPositions = Map.empty
            CreepPositions = Map.empty
            Obstacles = Set.empty
            Roads = Set.empty
        }

/// A [[colony view]]'s spatial projection: the terrain of the rooms the colony
/// works plus positions of the entities decisions need to place on them.
type SpatialInfo =
    {
        /// Which entry of `Rooms` is the home room — the room the colony
        /// plans for, which is the room its spawn happens to stand in and
        /// is never defined by that (ADR 0041) — and still the room name
        /// the census signature and the Layout read (ADR 0017). None for a projection that does not say which
        /// room it is, whose geometry is filed under the empty name,
        /// exactly as `Decide.censusSignature` spells that room.
        RoomName: string option
        /// Room name -> that room's geometry: every container the
        /// projection keys by `Pos` or fills with `Pos`es, and since ADR
        /// 0041's contract step the *only* place any of them lives. There
        /// is one projection and one shape of it (ADR 0005) — the flat
        /// copies of these five that carried the home room through the
        /// migration are gone, and with them the bridge that filled them.
        /// `RoomName` says which entry is home; every other entry is an
        /// outpost — so a projection carrying a `Borders` entry has to name
        /// its home room too, or `SpatialInfo.homeName` is the empty name
        /// and every home query reads `RoomLayer.empty` however the
        /// geometry here is filed. Read an entry with `Map.tryFind`,
        /// defaulting to `RoomLayer.empty`: a room with no geometry has no
        /// entry here at all, and that is the same answer (ADR 0004).
        Rooms: Map<string, RoomLayer>
        /// Room name -> the terrain of that room's border ring: the exit
        /// rows and columns (x or y of 0 or 49) a layer's `Terrain`
        /// deliberately leaves out. A layer of its own and never ground
        /// (ADR 0041): a creep that ends its tick on an exit tile is moved
        /// into the neighbouring room by the engine, so admitting one as
        /// walkable would let a Seat, a Work Area or a standing candidate
        /// teleport the creep out from under its Task — which is what ADR
        /// 0036's 1..48 trim prevents and this layer must not undo. It
        /// enters no weight grid, no walkable or buildable set and no Work
        /// Area; the Atlas lays it a grid of its own (`Atlas.Rings`, #173)
        /// beside those and never inside them, and the Seam query and the
        /// crossing's price are all that read it. Keyed by room
        /// name because a Seam joins two rooms: a room the projection does
        /// not cover is simply absent here, and answers no Seam at all
        /// (ADR 0004).
        Borders: Map<string, Map<Pos, Terrain>>
        /// Task-target id -> what kind of thing stands (or will stand)
        /// there. Id-keyed and so unlayered (ADR 0041): an object id is
        /// already unique across the world, and the layer that places the
        /// id *is* the room it stands in (`SpatialInfo.placementOf`), so a
        /// room dimension here would key a unique thing twice.
        TargetKinds: Map<string, TargetKind>
        /// Target id -> current/max hits, repairable kinds only — the
        /// decaying roads and containers (ADR 0010, ADR 0012), the Keep
        /// and our own ramparts (ADR 0034); fields nobody decides on stay
        /// out. Each kind is judged against its own whole line
        /// (`wholeLine`), and three readers now share these hits: the
        /// Repair pool, the safe-mode reflex and the Raid log's damage.
        Hits: Map<string, HitsInfo>
        /// Target id -> energy currently stored: the stock the logistics
        /// Tasks judge a store by. The containers (ADR 0012) and the
        /// Storage (ADR 0023) are the standing stores; since #167 the two
        /// transient ones are here on the same key — a tombstone's or a
        /// ruin's energy, which `Withdraw` draws exactly as it draws a
        /// container's, and a dropped pile's amount, which is what tells
        /// the Pickup Task whether the pile is worth a walk. One table
        /// and no second reading: a store is a store whatever will
        /// become of the thing holding it.
        Stores: Map<string, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpatialInfo =
    /// The empty projection: no room, no tiles, no entities — every entry absent.
    let empty =
        {
            RoomName = None
            Rooms = Map.empty
            Borders = Map.empty
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
        }

    /// The name the projection's own room is filed under: `RoomName`, and
    /// the empty name when it names none — the name the census signature
    /// has always spelled that way (`Decide.censusSignature`). Decided
    /// here, once, so the convention has one implementation rather than a
    /// copy at every reader that has to resolve the home layer; a site
    /// that spelled it differently would file the home room under one name
    /// and read it under another, and ADR 0004 would answer every home
    /// query with the empty set rather than throwing.
    let homeName (spatial: SpatialInfo) : string =
        spatial.RoomName |> Option.defaultValue ""

    /// One room's geometry, as ADR 0004 has every other absence: a room
    /// the projection carries no layer for reads as a room whose every
    /// entry is absent, never as a lookup that throws. The one spelling of
    /// the read the `RoomLayer` doc prescribes, so no reader has to
    /// remember the default.
    let layerOf (spatial: SpatialInfo) (room: string) : RoomLayer =
        Map.tryFind room spatial.Rooms |> Option.defaultValue RoomLayer.empty

    /// The room the projection files a target id under, with its tile
    /// there. The id-to-room join on the projection itself, beside the one
    /// the Atlas precomputes (`TargetAt`): a target id is unique across
    /// the world, so the layer holding it *is* the room it stands in, and
    /// the two answer alike because the Atlas fills `TargetAt` by walking
    /// these same layers. Which one a reader spells is therefore about
    /// what it is answerable to, not about what it holds. A reader handed
    /// no Atlas — the Planner, and `censusSignature` — has only this one.
    /// A reader *guarded* by that signature spells it this way too, even
    /// holding an Atlas: the hauler quota resolves its containers here so
    /// that the join the memo signs and the join the memo's value reads
    /// are the same line, rather than two spellings kept in step by hand.
    /// Everything else — a Task priced against the Atlas's own tables —
    /// takes the precomputed join. None for a target the projection does
    /// not place, which classifies nothing and blocks nothing (ADR 0004).
    ///
    /// Deterministic under a collision that cannot happen: `Map.tryPick`
    /// walks the rooms in name order, and one id stands in one room.
    let placementOf (spatial: SpatialInfo) (id: string) : RoomPos option =
        spatial.Rooms
        |> Map.tryPick (fun room (layer: RoomLayer) ->
            Map.tryFind id layer.TargetPositions |> Option.map (RoomPos.at room))

/// One outpost: a neighbouring room this colony mines and does not own.
/// Declared, never discovered (ADR 0041) — a constant a human moves in a
/// commit, exactly as the Layout's horizon is (ADR 0039) — because every
/// "the first creep to walk in writes it down" scheme has to answer what
/// sent the first creep, and answering it means inventing scouting,
/// persistent room intel and staleness discounting for a colony with two
/// candidate neighbours already committed as fixtures.
///
/// What is declared is exactly what vision cannot be waited for: the
/// room's name, and the id and tile of each source and of the controller.
/// Everything that actually changes — the reservation remaining, container
/// and road hits, stores, hostiles — is read off the projection where
/// there is vision and is absent entry by entry where there is none (ADR
/// 0004). That is the whole of what "we cannot see it this tick" means
/// here: no second state, and nothing to discount.
///
/// The ids are the engine's own, and this is the decision the rest of the
/// outpost work is built on. Every id in the projection is the server's —
/// `TargetKinds`, `Hits` and `Stores` are keyed by it and `ColonyView.Sources`
/// carries it — so a declaration written in the room captures' readable
/// short names (`RoomFixtures` renames `6a8c…4a6` to `src-0` for a person
/// to read) would match nothing on a live server, and would do it in
/// silence: an id the projection does not place is unpriceable geometry,
/// so the outpost would simply never enter a Task rather than fail (ADR
/// 0004). The captures keep the server's ids beside the readable ones
/// (`RoomCapture.RealSources`) so a test can build the view a
/// declaration matches.
///
/// No adjacency field, deliberately: which border an outpost shares with
/// home is already a fact about the two room names, and the Seam query
/// reads it out of them (`Atlas.seams`). A room name and an edge are two
/// facts that can disagree, and the disagreement would build a band out of
/// two rooms' opposite walls.
type Outpost =
    {
        RoomName: string
        /// The room's sources, each under the id the engine knows it by,
        /// and each tile joined to the room it is a tile of (ADR 0052
        /// decision 2). The join restates `RoomName` and is meant to: a
        /// declaration is a constant a human edits, and the tiles it lays
        /// are the ones that reach the projection — a coordinate that
        /// wandered into the wrong room's layer is the phantom Post #191
        /// was, and it read as a real one because nothing beside it said
        /// which room it was for.
        Sources: (string * RoomPos) list
        /// The room's controller, whose reservation is what doubles those
        /// sources (ADR 0042). Not optional: an unreserved source is worth
        /// half, so a room with no controller to reserve — a sector centre
        /// or a Source Keeper room — is not a candidate outpost at all.
        Controller: string * RoomPos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Outpost =
    /// The declarations the colony works this tick: the declared list,
    /// less every room a [[stand-down]] is withholding (ADR 0043). The
    /// gate, and the one place the set is narrowed.
    ///
    /// It narrows the *declarations* and never the scan set directly,
    /// because the three readings below — the rooms projected, the
    /// furniture laid in and the rocks pooled — are three readings of one
    /// list, and a gate applied to one of them would be a room whose
    /// terrain nobody read carrying furniture, or a rock in the pool with
    /// no layer to price it against (ADR 0004's escape prices an unplaced
    /// target at 0, so it would *win* its tier). Narrowed here, the three
    /// narrow together or not at all, and everything downstream — the
    /// projection, the Task pool, the four quota rows, the Atlas — sees
    /// exactly what it sees for a room nobody declared. That is the whole
    /// of "withdraw" in an architecture that keeps no state and recomputes
    /// every tick, and it is semantics ADR 0004 has already paid for.
    ///
    /// The shut set is the previous tick's conclusion (`Observe.standDown`
    /// over the [[raid log]]), because the deadline it holds was read on
    /// the last tick that had vision to read one with — and the creeps
    /// that paid for that vision are the ones this gate withdraws.
    let worked (shut: Set<string>) (outposts: Outpost list) : Outpost list =
        outposts
        |> List.filter (fun outpost -> not (Set.contains outpost.RoomName shut))

    /// The rooms the shell projects this tick: the home room, and every
    /// declared outpost beside it (ADR 0041). One projection covering
    /// several rooms, never a second one (ADR 0005) — the union is taken
    /// here so the rule has one statement rather than a copy in the shell.
    ///
    /// The outposts are handed in rather than read from `declared`
    /// straight, for two reasons: the union rule is then checkable against
    /// any declaration — the empty one #124 shipped, the two rooms #126
    /// filled in, a third a human adds — rather than only against the one
    /// the colony happens to ship, and the stand-down gate (ADR 0043) has
    /// exactly one place to narrow the set
    /// — a room withdrawn from does not enter the projection at all, which
    /// is the whole of "retreat" in an architecture that keeps no state.
    ///
    /// Home first, then the declarations in their own order, each room
    /// once: a declaration naming the home room is a human's slip, and
    /// projecting that room twice would file one room's geometry under one
    /// name twice over rather than say so.
    let roomsProjected (outposts: Outpost list) (home: string) : string list =
        home :: (outposts |> List.map (fun outpost -> outpost.RoomName))
        |> List.distinct

    /// One declaration as projection entries: the controller and then the
    /// sources in their declared order, each id paired with the tile the
    /// declaration names and the kind it is. Position and kind are read off
    /// one list rather than two, so the two folds below cannot place an id
    /// the kind census then misses or classify one nothing places. The
    /// Harvest pool reads it too (`pooledSources`), for the same reason
    /// widened by one reader: a rock this drops is a rock nothing places,
    /// and a rock nothing places must not be pooled.
    /// One declaration's tiles are its own room's, and this is where that
    /// is checked rather than assumed (ADR 0052 decision 2): the layer
    /// these are laid into is one room's (ADR 0041), so a declared tile
    /// filed under another room name is dropped here instead of being
    /// written onto this room's coordinate. That misjoin is exactly the
    /// phantom #191 stood on, and it used to be unsayable — a bare `Pos`
    /// beside `RoomName` agreed with the room by construction because
    /// there was nothing for it to disagree with.
    let private furnitureOf (outpost: Outpost) : (string * Pos * TargetKind) list =
        (fst outpost.Controller, snd outpost.Controller, Controller)
        :: (outpost.Sources |> List.map (fun (id, tile) -> id, tile, Source))
        |> List.filter (fun (_, tile, _) -> tile.Room = outpost.RoomName)
        |> List.map (fun (id, tile, kind) -> id, RoomPos.pos tile, kind)

    /// The declared furniture, laid into the projection: for every scanned
    /// outpost, its sources and its controller at the tiles and under the
    /// ids the declaration names — whether or not the colony has vision in
    /// that room this tick.
    ///
    /// This is the half of ADR 0041 that vision may not gate, and the
    /// deadlock the ADR spends a paragraph breaking: *"A source's position
    /// needs vision; vision needs a creep there; a creep goes there because
    /// a Task exists; the Task exists because the source is in the
    /// projection."* A declared fact — a source's id and tile, the
    /// controller's — is in the projection because a human wrote it down;
    /// only what actually changes (reservation remaining, container and
    /// road hits, stores, creeps, hostiles) waits for vision, and that is
    /// what is absent entry by entry where there is none (ADR 0004). #124
    /// read that absence onto the declaration as well, which left the whole
    /// ADR 0042 chain without its first step: no Harvest could name an
    /// outpost, so nothing walked there, so vision never came (#148).
    ///
    /// Vision wins every entry it holds: the declaration is laid *under*
    /// what the room's `find` families answered and never over it. The two
    /// agree by construction — the ids are the engine's own and a rock does
    /// not move — so this decides which truth is authoritative rather than
    /// resolving a conflict that can arise.
    ///
    /// The controller's tile joins `Obstacles`, exactly as the seen half
    /// files it (`World.ofGame`): a controller is an obstacle
    /// structure, so a reserver stands beside it and never on it, and a
    /// Work Area built over ground that ignored it would offer a tile the
    /// engine refuses to move onto.
    ///
    /// Only rooms the projection already carries a layer for. The scan set
    /// is the one gate on which rooms the colony works (`roomsProjected`,
    /// narrowed by the stand-down of ADR 0043), and a declaration able to
    /// conjure a room the scan left out would be a second gate free to
    /// disagree with the first — furniture standing on terrain nobody read.
    let place (outposts: Outpost list) (spatial: SpatialInfo) : SpatialInfo =
        (spatial, outposts)
        ||> List.fold (fun spatial outpost ->
            match Map.tryFind outpost.RoomName spatial.Rooms with
            | None -> spatial
            | Some layer ->
                let furniture = furnitureOf outpost

                { spatial with
                    Rooms =
                        Map.add
                            outpost.RoomName
                            { layer with
                                TargetPositions =
                                    (layer.TargetPositions, furniture)
                                    ||> List.fold (fun placed (id, pos, _) ->
                                        if Map.containsKey id placed then
                                            placed
                                        else
                                            Map.add id pos placed)
                                // The controller's own tile, from the
                                // furniture that has already been checked
                                // against this room: a declaration whose
                                // controller names another room blocks no
                                // tile here.
                                Obstacles =
                                    (layer.Obstacles, furniture)
                                    ||> List.fold (fun blocked (_, pos, kind) ->
                                        if kind = Controller then Set.add pos blocked else blocked)
                            }
                            spatial.Rooms
                    TargetKinds =
                        (spatial.TargetKinds, furniture)
                        ||> List.fold (fun kinds (id, _, kind) ->
                            if Map.containsKey id kinds then
                                kinds
                            else
                                Map.add id kind kinds)
                })

    /// The sources the Harvest pool is built from: the ones vision answered
    /// with, and every declared outpost rock beside them. One pool ranked
    /// in one order (ADR 0041), so a rock the colony cannot see this tick
    /// is a Task all the same — the declaration is what breaks the vision
    /// deadlock, and a pool that waited for vision would never see one.
    /// Deduplicated by id with the seen list first, because a declared rock
    /// in a room we *can* see arrives twice under one engine id and the
    /// engine's answer is the one carrying this tick's restock.
    ///
    /// An unseen rock restocks in 0 ticks: ADR 0025's "holds energy"
    /// default, the same one the shell gives a source whose regeneration
    /// timer the engine has not started. A restock is a *time* fact, and
    /// the unknown one is not "for ever" — priced at 0 the source is judged
    /// at arrival like any other (ADR 0025), and the Emitter's own gate is
    /// what withholds the dig from a rock that turns out to be empty when
    /// the creep gets there. Priced at anything else it would be a source
    /// no walk could cover, which is the same deadlock in a second place.
    ///
    /// Scanned rooms only, which is the gate `place` reads off the
    /// projection it is handed: the scan set is the one gate on which rooms
    /// the colony works (`roomsProjected`, narrowed by the stand-down of
    /// ADR 0043), and the pool has to pass through it too. A pool that took
    /// the declaration straight would name rocks nothing places — and an
    /// unplaced target is not inert to the Matcher: it prices at 0 (ADR
    /// 0004's escape), so it *wins* its tier, and the Emitter then aims a
    /// Harvest at an object `Game.getObjectById` cannot answer for while
    /// anti-thrash holds the creep on it. So the rocks are pooled exactly
    /// where the furniture is laid, and a stand-down narrows both at once.
    ///
    /// "Exactly where the furniture is laid" is read off `furnitureOf`
    /// itself and not restated here (#216 R3): the room check that entry
    /// carries since ADR 0052 decision 2 is a *second* gate on a declared
    /// tile, and a pool that read `outpost.Sources` straight would pass a
    /// mis-roomed rock the projection drops — pooled and unplaced, which is
    /// the very half-state the paragraph above says cannot arise.
    let pooledSources
        (rooms: string list)
        (outposts: Outpost list)
        (seen: SourceInfo list)
        : SourceInfo list =
        seen
        @ [
            for outpost in outposts do
                if List.contains outpost.RoomName rooms then
                    for id, _, kind in furnitureOf outpost do
                        if kind = Source then
                            { Id = id; TicksToRestock = 0 }
        ]
        |> List.distinctBy (fun source -> source.Id)

    /// The two rooms ADR 0042 measured, as the outposts they were declared
    /// as: the real-terrain fixtures (`RoomInvariantTests`) read the
    /// captures relative to W12S28 with both of them laid in, and keep
    /// doing so after W13S28 became a colony of its own — the geometry the
    /// tests pin did not move when the declaration did. The ids and tiles
    /// are the engine's, pinned against the committed captures.
    let adr0042: Outpost list =
        [
            {
                RoomName = "W12S27"
                Sources = [ "6a8caabadd4872bccd3194a6", { Room = "W12S27"; X = 16; Y = 45 } ]
                Controller = "6a8caabadd4872bccd3194a5", { Room = "W12S27"; X = 37; Y = 43 }
            }
            {
                RoomName = "W13S28"
                Sources =
                    [
                        "6a8caaaddd4872bccd319362", { Room = "W13S28"; X = 16; Y = 7 }
                        "6a8caaaddd4872bccd319361", { Room = "W13S28"; X = 18; Y = 4 }
                    ]
                Controller = "6a8caaaddd4872bccd319363", { Room = "W13S28"; X = 24; Y = 17 }
            }
        ]


/// Where one colony stands in its life (ADR 0052 decision 3). Three
/// answers to one question — how much of its own economy a colony has
/// bought yet — and the fact five rules used to read as a controller
/// level apiece: whether it places roads and keeps ramparts, whether its
/// sites come before its controller, whether a [[mother colony]] is still
/// raising it and hires [[pioneer]]s for it.
///
/// Derived once (`Colony.stageOf`) and never stored: it is a fact about
/// the world this tick, like everything else the shell reads, and the
/// level it is derived from appears nowhere else.
type ColonyStage =
    /// Claimed, and no spawn of ours standing in it yet: a [[nursery]] —
    /// a room that is a colony by declaration and by ownership, and by
    /// nothing else it can do for itself. What ends it is a spawn, which
    /// is what independence *is* (ADR 0047 decision 4).
    | Nursery
    /// Its own spawn standing and its controller still under
    /// `Tuning.BootstrapLevel`: running its own `decide`, casting its own
    /// bodies, and still being raised — the **bootstrap window**.
    | Bootstrapping
    /// At `Tuning.BootstrapLevel` or past it: the first tower and the
    /// tenth extension, a bank that casts a body which is not the
    /// 300-energy starter. The stage every rule written for the one home
    /// this bot grew up in was written at (ADR 0052).
    | Independent

/// One colony: a [[home room]] and the [[outpost]]s worked from it (ADR
/// 0047). The unit the whole decision layer is written in — one Atlas, one
/// Layout, one set of quotas, one Task pool — and so the unit a
/// declaration is written in too, replacing the bare outpost list that
/// said the same thing while there was only ever one home.
///
/// The home is a room *name* and never a spawn: a colony outlives every
/// spawn standing in it, and the room is what the projection files
/// everything under (ADR 0041). Which colonies actually run is a fact
/// about the world and not about this constant — a declared home the
/// colony has not claimed yet is a **candidate colony**, and one with no
/// spawn of its own is not independent — so nothing here is a promise that
/// a colony exists, only that a human means it to.
type Colony =
    {
        /// The room the colony is run from: the room its spawns stand in,
        /// its Layout is planned in, and its quotas are banked in.
        Home: string
        /// The rooms it mines but does not own. A **candidate colony**'s
        /// home appears here as well, in its *mother* colony's list, until
        /// the day it is independent: the room is projected and worked as
        /// an outpost while it is being claimed and built up, and one room
        /// projected by two colonies at once is exactly what the mother's
        /// outpost declaration already means (ADR 0047).
        Outposts: Outpost list
        /// The home room of the [[mother colony]] that raised this one, for
        /// as long as it is still being raised (ADR 0047 decision 4): the
        /// **bootstrap** window, which runs from the day the child leaves
        /// its mother's outpost list to the tick its controller reaches
        /// `Tuning.BootstrapLevel`. `None` for a colony that was never anybody's
        /// child and for one that has outgrown its mother.
        ///
        /// A field, and the one part of the mother–child relation that has
        /// to be one. Until the child is independent the relation is the
        /// mother's `Outposts` entry naming the child's home, and nothing
        /// else is needed: one room, two declarations, and each of them
        /// says the whole of it. The day the human splits the declaration
        /// that entry goes, and with it every trace of which colony raised
        /// which — while the borrowing rule below still has two whole
        /// levels to run. So the field carries exactly what the outpost
        /// entry carried and nothing more: the name of the colony whose
        /// workers may cross for this one's Upgrade and Build.
        ///
        /// A human's, like the rest of the declaration, and cleared by a
        /// human: the bot never edits it, and leaving it in past
        /// `Tuning.BootstrapLevel` costs nothing, because the rule that reads it
        /// (`bootstrapping`) asks the world for the child's [[stage]] and
        /// stops on its own.
        Mother: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Colony =
    /// The colonies a human has declared (ADR 0047): today the one home
    /// room this bot has ever had, W12S28, with ADR 0042's two outposts —
    /// W12S27 across the north edge and W13S28 across the west, three
    /// sources and two controllers between them.
    ///
    /// Chosen by a human in an ADR and moved by a human in a commit,
    /// exactly as the Layout's horizon is (ADR 0039) — the types above
    /// carry why there is no discovery and why the ids are the engine's.
    /// That is why claiming a second room begins here and not in the bot:
    /// a second entry `{ Home = "W13S28"; Outposts = [] }` beside this one
    /// — W13S28 staying in W12S28's outposts until it stands on its own —
    /// is the whole of "I mean to take that room", and it is a human's
    /// sentence to write (ADR 0047's user story 1). The bot never edits
    /// it: independence is an event a person can see, not a constant a
    /// program rewrites.
    ///
    /// The outposts are still ADR 0042's, and the reasons they are those
    /// rooms have not moved. Filling that list was half of ADR 0042's
    /// first step and never the whole of it: the other half is in
    /// `Decide.workforceTarget`, which counts an unposted source's Seats
    /// into the target on the grounds that its output is spoken for by the
    /// crews that walk it — a rule that, taken across these three sources'
    /// six Seats, five of them swamp, hires six generalists to commute
    /// forty-seven to fifty-six tiles.
    ///
    /// W13S28's sources are paired to their tiles and never to their
    /// order: they are written `16,7` before `18,4`, the reverse of the
    /// order ADR 0042's prose reads them in, because that is the order the
    /// server answered the room in and the order the committed capture
    /// keeps (`RoomFixtures.RealSources`, pinned in `RoomInvariantTests`).
    /// `16,7` is the single-Seat far source, not the two-Seat one.
    let declared: Colony list =
        [
            {
                Home = "W12S28"
                // W12S27 alone since W13S28 stood its own spawn (below); the
                // room is ADR 0042's north outpost, read off the pair above.
                Outposts = Outpost.adr0042 |> List.filter (fun o -> o.RoomName = "W12S27")
                Mother = None
            }
            // The second colony (ADR 0047). W13S28 was the first colony's
            // outpost until its spawn stood at (16,12) on 2026-09-06
            // (t~167,5xx); that tick it became a living colony of its own
            // and left the mother's list above, so one room is projected
            // by one colony. It works no outposts of its own yet. Moved by
            // a human, like every declaration here.
            //
            // W12S28 raised it and goes on raising it: the mother's workers
            // may cross the Seam for this room's Upgrade and Build, and her
            // worker row hires `Tuning.PioneerCount` bodies for the job, until
            // the controller here reaches `Tuning.BootstrapLevel` (ADR 0047 decision
            // 4). The day it does, this room leaves her projection on its
            // own; the name may then be cleared by a human or left standing.
            {
                Home = "W13S28"
                Outposts = []
                Mother = Some "W12S28"
            }
        ]

    /// The outposts one home room works: its own declaration's, and none
    /// at all for a room nobody declared. That last answer is the one that
    /// matters — a home the constant does not name projects the room it
    /// stands in and nothing else, which is exactly the behaviour the
    /// empty declaration shipped with (#124), so a slip in the constant
    /// costs the colony its outposts rather than putting it in a state
    /// nothing downstream has a rule for.
    ///
    /// The list is handed in rather than read off `declared` straight, for
    /// the reason `Outpost.roomsProjected` is: the rule is then checkable
    /// against any declaration a human might write rather than only
    /// against the one this colony happens to ship.
    let outpostsOf (colonies: Colony list) (home: string) : Outpost list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Outposts)
        |> Option.defaultValue []

    /// Every declared colony's home room, in declaration order. What the
    /// shell hands the decision layer (`ColonyView.Declared`), because
    /// which rooms a human means to own is not a thing vision can answer
    /// and not a thing the projection carries — the ownership half of
    /// "candidate colony" is read off `RoomControl` in Core, and this is
    /// the half that can only be declared.
    let homes (colonies: Colony list) : string list =
        colonies |> List.map (fun colony -> colony.Home)

    /// The **living** colonies: the ones `Main.loop` builds a view for
    /// and runs `decide` once for, one whose home room is ours *and* holds
    /// one of our spawns (ADR 0047 decision 1). Declaration order, so the
    /// first entry is the one a creep no spawn name claims falls to
    /// (`creepColonies`).
    ///
    /// Two facts and not one, though the engine lets nobody stand a spawn
    /// in a room another player owns: they are what a declared colony
    /// passes *through* on its way to running, and each of the two states
    /// on the way fails exactly one of them. A [[candidate colony]] owns
    /// nothing and spawns nothing; a [[nursery]] is owned and has no spawn
    /// of its own, and is run by its [[mother colony]] rather than by
    /// itself (ADR 0047 decision 4) — so "owned" alone would start a
    /// colony with no spawn to cast from, no bank to cast out of and no
    /// Layout anchor, and the mother would keep building a room that was
    /// already deciding for itself.
    ///
    /// Both facts are the shell's to read and neither is this constant's:
    /// a declaration is a human's intent and ownership is something vision
    /// pays for (ADR 0004), so what arrives here is the rooms our spawns
    /// stand in and the rooms we own, and the rule over them is one
    /// sentence rather than a filter written out in `Main.loop`.
    ///
    /// **A spawn room no declaration names is a colony of its own**, and
    /// only when the declaration answers with nothing at all: `outpostsOf`
    /// already says that a home nobody declared works no outposts rather
    /// than entering a state nothing has a rule for (#124), and this is
    /// that sentence one level up — a slip in a constant a human moves
    /// costs the colony its outposts, never its whole tick. Without it a
    /// bot standing in a room the declaration does not mention runs no
    /// `decide` at all: nothing cast, nothing harvested, nothing moved,
    /// and no Verdict to say why. That is a state a respawn reaches by
    /// itself, and it is the world every harness stub is (`npm run
    /// profile`).
    ///
    /// The first such room and not all of them, because that is exactly
    /// what the shell read before there were colonies to declare — one
    /// room, with no outposts — so a world the declaration does not
    /// describe decides today what it decided yesterday. **First in the
    /// order it is handed**, which since #216 R2a is the world's own:
    /// room-name order (`World.spawnRooms`), where the shell used to hand
    /// down `Game.spawns` enumeration order. The two differ only where the
    /// fallback fires with spawns standing in two owned rooms at once, and
    /// what ADR 0047 asked of this branch is that it name one room and
    /// keep its outposts empty, not which of two an engine enumerates
    /// first. And *only* when nothing declared is living, so this can
    /// never add a colony beside a declared one: in a world the
    /// declaration does describe it is inert, and a room a human left out
    /// of the constant stays out.
    let living
        (owned: Set<string>)
        (spawnRooms: string list)
        (colonies: Colony list)
        : Colony list =
        let declared =
            colonies
            |> List.filter (fun colony ->
                Set.contains colony.Home owned && List.contains colony.Home spawnRooms)

        match declared with
        | [] ->
            spawnRooms
            |> List.filter (fun room -> Set.contains room owned)
            |> List.tryHead
            |> Option.map (fun home ->
                {
                    Home = home
                    Outposts = []
                    // Nobody's child: a room the declaration does not
                    // describe is one no human wrote a mother for, and a
                    // fallback that invented one would hire pioneers for a
                    // colony that exists only because a constant slipped.
                    Mother = None
                })
            |> Option.toList
        | living -> living

    /// One colony's [[stage]] this tick, off the three facts that decide
    /// it (ADR 0052 decision 3). **The one place `Tuning.BootstrapLevel`
    /// is read**: every rule that used to compare a controller level of
    /// its own asks for a stage instead, so the line moves in one field
    /// and cannot drift between the five readers it had.
    ///
    /// The tunables arrive as an argument rather than off a constant of
    /// this module (ADR 0052 decision 5): the line is a colony's choice
    /// and not the engine's, so a test moves it by handing another
    /// `Tuning` and never by editing the rule.
    ///
    /// `None` for a room that is not a colony at all. Not owned by us is
    /// not a stage: a declared home nobody has claimed yet is a
    /// **candidate colony**, whose one rule is the Claim pool
    /// (`Decide.claimTargets`), and a room a rival holds is a declaration
    /// a human has not caught up with. Owned with no controller level to
    /// read is `None` too — the shape a colony whose controller the
    /// projection cannot place arrives in — and every reader's answer for
    /// `None` is the one it already gives that colony today: no rampart
    /// kept, no road placed, nothing bootstrapped.
    ///
    /// The level is asked for last and only once the spawn stands,
    /// because that is the order the stages are in: a [[nursery]] is a
    /// nursery at any level, and RCL3 is what a colony that runs itself
    /// climbs to.
    let stageOf
        (tuning: Tuning)
        (owned: bool)
        (spawnStanding: bool)
        (level: int option)
        : ColonyStage option =
        if not owned then
            None
        elif not spawnStanding then
            Some Nursery
        else
            level
            |> Option.map (fun level ->
                if level >= tuning.BootstrapLevel then
                    Independent
                else
                    Bootstrapping)

    /// The rooms one colony **bootstraps** this tick (ADR 0047 decision
    /// 4): the homes of the colonies it is the [[mother colony]] of, while
    /// those colonies are not yet `Independent`. The mother projects each
    /// of them beside her own rooms and works two Tasks there — the
    /// child's Upgrade and its Build — which is the one cross-colony
    /// borrowing rule there is.
    ///
    /// The stages are handed in, derived off the world in Core for the
    /// declared homes (`World.stages`, `stageOf`), because a colony's own
    /// view cannot answer for a room that is not in its scan set and this
    /// is the rule that *decides* that set. Three readers take the same
    /// answer — the scan set, the borrowed layer those rooms are narrowed
    /// to (`ColonyView.borrowed`) and the view the pool is built from —
    /// and they agree by construction rather than by being called once:
    /// this is a pure function of the declaration and the world, so a
    /// reader that derives it again derives the same rooms. What must not
    /// be written twice is the *rule* (`World.scanOf` is the one place the
    /// union is spelled), because a second rule is a second answer free to
    /// disagree, and here it would be a room projected with nothing pooled
    /// in it, or pooled with nothing projecting it.
    ///
    /// **Both of the stages before independence**, and not the bootstrap
    /// window alone. A child that has left its mother's outpost list and
    /// has no spawn standing — one whose spawn was destroyed, or one a
    /// human split off early — is a [[nursery]] again, and the mother is
    /// the only colony that can raise it: dropped from her projection it
    /// would be a claimed room with no spawn, no vision and nobody
    /// building the spawn site that ends the state. The tick a child
    /// crosses the line it is `Independent` and leaves, exactly as before.
    ///
    /// **That is wider than the level rule it replaces, deliberately and
    /// in one shape.** A nursery is a nursery at any level (`stageOf`),
    /// where `level < Tuning.BootstrapLevel` stopped at RCL3 — so a child that
    /// stood its own spawn, reached RCL3 and then *lost* that spawn is
    /// raised again here where the level rule orphaned it. That is the
    /// right answer for the case and the reason it is one: the room is
    /// claimed, it can cast nothing, and its mother is the only colony
    /// that can put a spawn site back up. What it costs is the nursery's
    /// own price paid over a grown room — every site in it is
    /// feeding-tier and uncapped in the mother's pool
    /// (`Decide.isNurserySite`), and her worker row carries the
    /// [[pioneer]] addend — until the spawn stands again.
    ///
    /// A room with no stage is not bootstrapped. Absence classifies
    /// nothing (ADR 0004): a room we cannot see has no entry, and neither
    /// has one we do not own — which **narrows** this rule against the
    /// level it used to read, where an unowned room read level 0 and was
    /// bootstrapped like any young one. Two shapes leave with it, and
    /// both go to the `Outposts` list rather than here. A **candidate**
    /// colony that names a mother and is in no outpost list of hers is no
    /// longer projected by her, so its Claim is pooled by nobody; ADR
    /// 0047's own user story keeps a child in its mother's `Outposts`
    /// until the day it stands its own spawn, which is where every claim
    /// this bot has made was pooled from. And a child that stops being
    /// ours — a rival's claim, or an RCL1 controller left to its 20,000
    /// ticks — leaves her projection the same way, so no Claim is pooled
    /// to take it back and a human's edit is what recovers it.
    ///
    /// **A room the mother still declares as an outpost is worked as one**,
    /// and never as a bootstrap layer. That is the [[nursery]] and the
    /// window after it (ADR 0047's Consequences): while the room is in the
    /// outpost list its rocks are the mother's to mine, its Seats hire her
    /// Anchors and the container rule places her container there, and a
    /// second, narrower projection of the same room would take all of that
    /// away on the strength of the same human's other sentence. The
    /// bootstrap layer begins exactly where the outpost declaration ends.
    ///
    /// The colony's own home is excluded by name, for `isNurseryRoom`'s
    /// reason: a declaration naming itself its own mother is a human's
    /// slip, and the home room narrowed to a bootstrap layer would be a
    /// colony that cannot see its own rocks.
    let bootstrapping
        (stages: Map<string, ColonyStage>)
        (colonies: Colony list)
        (colony: Colony)
        : string list =
        let worked = colony.Outposts |> List.map (fun outpost -> outpost.RoomName)

        colonies
        |> List.filter (fun child ->
            child.Mother = Some colony.Home
            && child.Home <> colony.Home
            && not (List.contains child.Home worked)
            && (Map.tryFind child.Home stages
                |> Option.exists (fun stage -> stage <> Independent)))
        |> List.map (fun child -> child.Home)

    /// The declared children of this colony that have stopped being ours,
    /// and are nobody else's either (#221): the second half of what a
    /// mother projects for a child, and the one that has nothing to do with
    /// raising it.
    ///
    /// A [[stage]] is `None` for a room we do not own — that is what makes
    /// a room nobody claimed one no mother projects (ADR 0052 decision 3)
    /// — so a child whose spawn is destroyed and whose controller is then
    /// lost, to a rival's claim or to the 20,000-tick RCL1 downgrade, left
    /// every projection there was: the room stood empty, no [[claim]] was
    /// pooled anywhere, and only a human's edit to the declaration could
    /// take it back. The level map the stage replaced carried a **seen**
    /// controller at 0 and kept the room, which is the behaviour restored
    /// here — off the ownership the world reads back rather than off a
    /// level, because "ours to take" is what the [[claim]] asks.
    ///
    /// **Unowned and never a rival's**: a room somebody else holds is ADR
    /// 0043's business and not a projection's, and a room with no control
    /// entry is one nothing looked into this tick, which classifies nothing
    /// (ADR 0004). What the mother then carries of it is the borrowing's
    /// own narrowing — the controller, the sites and the spawn tile — which
    /// is exactly what `Decide.claimTargets` reads and no more.
    ///
    /// The same three declaration clauses as `bootstrapping` above, and for
    /// the same reasons: a room still in her outpost list is worked as one,
    /// and a declaration naming itself its own mother is a human's slip.
    let reclaiming (unowned: Set<string>) (colonies: Colony list) (colony: Colony) : string list =
        let worked = colony.Outposts |> List.map (fun outpost -> outpost.RoomName)

        colonies
        |> List.filter (fun child ->
            child.Mother = Some colony.Home
            && child.Home <> colony.Home
            && not (List.contains child.Home worked)
            && Set.contains child.Home unowned)
        |> List.map (fun child -> child.Home)

    /// The rooms one colony projects this tick: its home and its worked
    /// [[outpost]]s (`Outpost.roomsProjected`), and beside them the rooms
    /// it bootstraps (`bootstrapping`). The whole scan set in one sentence,
    /// here and not in the shell, for the reason the outpost union is
    /// Core's: the projection is not the set's only reader — the entity
    /// lists the Task pool is built from are swept over it too — so the
    /// rule is stated once and read twice rather than copied.
    ///
    /// The bootstrapped rooms come last and the list is deduplicated, so a
    /// room that is somehow both an outpost and a child's home is projected
    /// once, under the outpost reading that named it first (`bootstrapping`
    /// refuses that pair at the source; this is the union's own guard, the
    /// one `roomsProjected` already has for a declaration naming home).
    let roomsProjected
        (outposts: Outpost list)
        (bootstrap: string list)
        (home: string)
        : string list =
        Outpost.roomsProjected outposts home @ bootstrap |> List.distinct

    /// The colony that cast one creep, read off its own name: creep names
    /// are `{pattern}-{tick}-{spawn}` (`Decide.planSpawns`), so the spawn
    /// that made it is spelt in the name and the room that spawn stands in
    /// is its home (ADR 0047 decision 2). None when no known spawn's name
    /// is in it — a creep from an older naming scheme, or one a human made
    /// by hand.
    ///
    /// The longest matching spawn name wins, so `Spawn1` cannot claim a
    /// creep `Spawn11` cast: the names are the engine's own and nothing
    /// stops one being a prefix of another.
    let private castBy (spawnHomes: (string * string) list) (creep: string) : string option =
        spawnHomes
        |> List.filter (fun (spawn, _) -> creep.Contains spawn)
        |> List.sortByDescending (fun (spawn, _) -> (spawn: string).Length)
        |> List.tryHead
        |> Option.map snd

    /// Which colony each creep belongs to this tick (ADR 0047 decision 2),
    /// keyed by creep name: a creep belongs to the colony that **cast**
    /// it, unless it is standing in a room only some *other* colony
    /// projects, in which case that colony **adopts** it for the tick.
    /// What a colony's `ColonyView.Creeps`, its layers' `CreepPositions` and
    /// therefore its census are cut by, so a creep is one colony's
    /// business and never two's — two colonies matching one creep would
    /// write two Tasks into one flat `assignments` leaf and move it twice.
    ///
    /// Adoption is what answers the creep a colony cannot place: a body
    /// standing outside every room its own colony projects has no tile
    /// there, and the colony that *does* project the room it stands in can
    /// price it, match it and move it (#164's tile-less creep, answered
    /// for the rooms some colony projects). It is a fact about this tick
    /// and nothing is kept: the tick the creep walks home its caster has
    /// it back.
    ///
    /// **Only** another colony's, and only when exactly one projects it:
    /// a room its own colony projects too is its own colony's business —
    /// the [[mother colony]] goes on working a [[nursery]] it also
    /// projects — and a room two others project at once names no single
    /// adopter, so the creep stays where it was cast rather than being
    /// handed to whichever came first in the declaration. A room *nobody*
    /// projects — a [[stand-down]]'s withheld outpost (ADR 0043) — adopts
    /// nobody either, so the creep standing there is still its caster's
    /// and still has no tile: the gate withdrew the room, and giving it
    /// away would be a second rule about a room the colony deliberately
    /// stopped looking at.
    ///
    /// A creep no spawn name claims falls to the first living colony,
    /// which is declaration order: a name the shell cannot read is not a
    /// creep to drop — dropped, it would be in no colony's Creeps, hold no
    /// assignment and stand still for the rest of its life.
    ///
    /// A creep whose caster **is** readable but whose colony is not living
    /// this tick falls there too, and for the same reason: the projections
    /// handed in are the living colonies' (`Colony.living`), so a home
    /// absent from them runs no `decide`, and a creep filed under it would
    /// be in no view at all — the identical fate, reached through a
    /// spawn standing in a room no living colony declares rather than
    /// through an unreadable name. The first *living* colony and never the
    /// first declared: a declared home that is not running would file the
    /// creep where nothing decides for it, which is the very outcome this
    /// fallback exists to refuse.
    let creepColonies
        (projections: (string * string list) list)
        (spawnHomes: (string * string) list)
        (creeps: (string * string option) list)
        : Map<string, string> =
        match projections with
        | [] -> Map.empty
        | (first, _) :: _ ->
            let homes = projections |> List.map fst

            creeps
            |> List.map (fun (name, standing) ->
                // A caster that is not one of the living colonies is no
                // answer at all — a spawn standing in a room nothing runs
                // this tick — and falls to `first` beside the unreadable
                // names, because a creep filed under a home with no
                // view is a creep in nobody's Creeps.
                let cast =
                    castBy spawnHomes name
                    |> Option.filter (fun home -> List.contains home homes)
                    |> Option.defaultValue first

                let projecting =
                    match standing with
                    | None -> []
                    | Some room ->
                        projections
                        |> List.filter (fun (_, rooms) -> List.contains room rooms)
                        |> List.map fst

                match projecting with
                | [ adopter ] when adopter <> cast -> name, adopter
                | _ -> name, cast)
            |> Map.ofList

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo = { Id: string }

/// What the decision layer knows about one hostile creep in a room the
/// colony is looking into this tick: its id and tile — what the fire
/// reflex aims at, in the colony's own room (ADR 0014) — its body parts,
/// verbatim, because what a hostile can do is decided from what it is made
/// of, its owner, which the Raid log's roster reads (ADR 0028), and the
/// room it stands in, which the Raid log's closest approach reads (ADR
/// 0041). Hostiles stay out of the spatial projection: they block no tiles
/// and price no paths. They do gate Tasks, and have since ADR 0033 — but
/// through a Threat's Reach, a colony-level fact the pipeline reads, never
/// a change to the map.
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username
        /// ("Invader" for the NPCs). The field the projection grew the
        /// tick a reader for it existed (ADR 0007's rule, ADR 0028): the
        /// Raid log's roster is attribution, and attribution is a name.
        /// No reflex reads it.
        Owner: string
        /// Where it stands, room and tile in one (ADR 0052 decision 2).
        /// ADR 0028 left the room out in as many words — "a room name on
        /// `HostileInfo` is a field no decision reads, and there is one
        /// spawn" — and ADR 0041 is what gave it a reader: a bare `Pos`
        /// carries no room, so the Raid log's closest approach measures a
        /// hostile against the tiles of *its* room, and one of ours
        /// standing on the same coordinate of another room is not at range
        /// 0. That was a `RoomName` field beside the tile, kept in step by
        /// hand at every reader; since #216 R3 it is the tile's own type,
        /// and `RoomPos.range` answers None across the border rather than
        /// leaving the join to whoever remembered to test it.
        /// Load-bearing for every reader since #201 widened the sweep
        /// behind the list to every room the colony can see: a Threat's
        /// Reach is filed under this room (ADR 0033, #138), and the two
        /// colony reflexes read the home room out of the list by it
        /// (`Decide.hostilesAtHome`).
        Pos: RoomPos
        Body: BodyPart list
    }

/// An NPC invader core standing in a room the colony works this tick (ADR
/// 0043). A **structure**, not a creep, which is why it reaches the
/// projection through neither `Hostiles` nor the fire reflex: the sweep
/// behind those is `FIND_HOSTILE_CREEPS` and a core has never been in it.
/// It is the threat an [[outpost]] is stood down from — 100,000 hits, no
/// creeps at level 0, and it never leaves — and the clock the stand-down
/// runs to is read off it while there is still vision to read it with.
///
/// One room per entry and no tile: the gate ADR 0043 describes admits or
/// withholds a whole room, so where in the room the core stands is a fact
/// nothing asks for, and the projection grows a field the tick a reader
/// exists and not before (ADR 0007's rule). A room the colony cannot see
/// contributes no entry — a core standing unwatched is absent here rather
/// than "no core" (ADR 0004), which is exactly why the expiry below is
/// sampled while the room is still in sight.
type InvaderCoreInfo =
    {
        /// The room it stands in. The colony works rooms, not tiles, so
        /// this is the whole of where.
        RoomName: string
        /// The **absolute** tick the core's collapse timer runs out at, or
        /// None where it carries none — an expanded level-0 core has no
        /// stronghold to collapse, so the deadline has to be read off the
        /// reservation it took instead (ADR 0043's fallback order), which
        /// is the room's `RoomControlInfo.Reservation` under
        /// `ReservationHolder.Invader` and is why that holder is a case of
        /// its own. This is the common case on the frontier, not the rare
        /// one: the measured core two rooms from W12S27 is level 0 and
        /// carries no timer (docs/research/remote-mining.md §8.4).
        ///
        /// Absolute because the shell adds the current tick to what the
        /// engine hands back, and the engine hands back a **relative**
        /// count: `RoomObject.effects[].ticksRemaining` is "how many ticks
        /// will the effect last" (docs.screeps.com, confirmed for #133).
        /// The `endTime` in `docs/research/remote-mining.md` — 170,283 for
        /// W15S24's stronghold — is a field of the read-only HTTP API's
        /// raw database documents and is *not* what the runtime answers
        /// with; stored as read it would be a deadline a hundred thousand
        /// ticks wrong, and the gate that reads it would hold an outpost
        /// shut for the life of the colony.
        CollapseTick: int option
    }

/// Which of ADR 0043's deadlines an [[outpost]]'s [[stand-down]] runs to
/// — the provenance of the tick, carried beside it because it cannot be
/// recovered from the tick afterwards. The fold that picked it is the only
/// place that still knows whether 2,600 was a collapse timer, a
/// reservation or the fallback, and an operator asking why an outpost is
/// shut is asking exactly that (#117).
///
/// The three answers are the ADR's own fallback order, best first: the
/// core's collapse timer, the end of the reservation the core took, and
/// the stronghold expansion period. A closed vocabulary and not a string,
/// for the reason `Ownership` gives — these are answers to one question —
/// and it crosses the wire, so it is spelt once in `standDownBasisName`
/// and round-tripped against the union itself by `Core.Tests` (#80).
///
/// The other withdrawal — a room another player owns or reserves — is
/// deliberately not a fourth case. ADR 0043 makes it the clockless
/// trigger, "not a threat episode": it opens no episode and carries no
/// expiry for a basis to explain. It is read off the view's
/// `RoomControlInfo` on every tick with vision and *remembered* between
/// them (`RaidState.RivalHeld`, #136), because the gate's own effect is to
/// take away the vision that judged it. What it remembers beside the room
/// is a tick, and not one anything compares against: the trace the gate's
/// closing leaves in the observe channel, where a basis would have nothing
/// to explain.
[<RequireQualifiedAccess>]
type StandDownBasis =
    /// The core's own `EFFECT_COLLAPSE_TIMER`: the tick the engine put on
    /// the stronghold that expanded here, and the first answer wherever
    /// it can be read.
    | CollapseTimer
    /// The end of the reservation the core took with `attackController` —
    /// what a level-0 core answers with, having no stronghold to collapse
    /// and so no timer. The measured case on this colony's frontier, not
    /// the rare one (docs/research/remote-mining.md §8.4).
    | Reservation
    /// Neither deadline was readable, so the clock is the 2,500-tick
    /// stronghold expansion period. The one answer the colony chose
    /// rather than read, and it errs long deliberately: ADR 0043's gate
    /// may be wrong only in the direction that costs an outpost's income
    /// rather than a creep a cycle.
    | Fallback

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

/// What one room holds this tick, to everybody: the half a declaration
/// carries and the half vision pays for, filed under the room's own name
/// and saying nothing about who is looking at it (ADR 0052 decision 1).
/// The unit the `World` is a map of, and the unit a [[colony view]] takes
/// its share of — a room two colonies both work contributes one of these
/// and never two, so a mother and her child cannot disagree about what
/// stands in it.
///
/// Every list here is *this room's*: the shell scopes what the engine does
/// not scope itself (our creeps out of the world-wide `Game.creeps`, the
/// controller, a structure's store) rather than filtering it downstream, so
/// a fact filed under a room name is a fact about that room's tiles.
///
/// Absence stays per entry (ADR 0004), and reaches down two levels: a room
/// the world holds no facts for at all reads `RoomFacts.empty`, and a room
/// it holds terrain for but has no vision in reads that terrain beside
/// empty everything else. Neither is a state of its own — unplaced
/// geometry is unpriceable, enters no Task and blocks no action.
type RoomFacts =
    {
        /// This room's geometry (ADR 0041): terrain, the targets standing
        /// on it, the tiles our creeps stand on — **every** creep of ours
        /// in the room and not one colony's, which is what makes this the
        /// world's answer and lets a view file the rest under `Foreign`.
        Layer: RoomLayer
        /// The room's border ring, the Seam's terrain and never ground
        /// (ADR 0036, ADR 0041).
        Border: Map<Pos, Terrain>
        /// The kind of each target standing in this room, under the
        /// engine's own id. Id-keyed within the room and merged unlayered
        /// into a view's `SpatialInfo` (ADR 0041): an object id is unique
        /// across the world, so the layer that holds it *is* the room it
        /// stands in, and the world files it where it stands rather than
        /// keying a unique thing twice.
        TargetKinds: Map<string, TargetKind>
        /// Current/max hits of the repairable kinds standing here (ADR
        /// 0010, ADR 0012, ADR 0034).
        Hits: Map<string, HitsInfo>
        /// Energy currently stored, per store standing here: the
        /// containers and the Storage, and since #167 the piles, the
        /// tombstones and the ruins.
        Stores: Map<string, int>
        /// Who holds the room, and whether its safe mode is running (ADR
        /// 0042) — `None` for a room nothing looked into this tick, which
        /// is not the same fact as a room nobody holds (ADR 0004).
        Control: RoomControlInfo option
        /// The controller of this room **while it is ours**: the fact a
        /// colony's own Upgrade, its downgrade clock and its safe-mode
        /// reflex are read off, and the level a [[stage]] is derived from
        /// (`World.stages`). `None` for a room we do not own, whose
        /// controller carries no `ticksToDowngrade` and no safe mode to
        /// read — its ownership and its reservation are `Control`'s, which
        /// is what prices a source there (ADR 0042).
        Controller: ControllerInfo option
        /// The room's shared spawn-energy account. Zero for a room with no
        /// spawn or extension in it, which is every room we do not own —
        /// the engine's own answer, and the one a colony reads as an empty
        /// bank.
        Energy: RoomEnergy
        /// Our spawns standing in this room. The world's, so a spawn
        /// standing in a [[nursery]] a mother is raising is a fact about
        /// that room and not about her — which is exactly what ends the
        /// nursery (`World.stages`).
        Spawns: SpawnInfo list
        /// The bodies still gestating in this room's spawns: energy the
        /// colony has **already spent** on a creep that is not alive yet
        /// (#156).
        ///
        /// Bodies and not creeps, and filed under the room rather than
        /// under the spawn that is building them, because that is exactly
        /// how far the fact reaches: a colony banks in one room (ADR 0052
        /// decision 1) and every row's gap is a colony number, so which
        /// oven a body is in changes nothing any reader asks. A creep still
        /// spawning is outside `World.Creeps` — it can act, stand and be
        /// matched to nothing — and it is here instead so the rows can
        /// count what they bought without the Matcher ever seeing it.
        ///
        /// Empty for every room but a home of ours with a busy spawn, and
        /// empty is the whole of "nothing is being cast here".
        Casting: BodyPart list list
        /// Our energy-hungry structures standing here (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources in this room, as vision answered for them. What a
        /// *declaration* answers for is laid over a colony's projection
        /// instead (`Outpost.place`, `Outpost.pooledSources`), because
        /// which rooms are worked is a colony's question and not the
        /// world's.
        Sources: SourceInfo list
        /// Our construction sites standing here (#150).
        ConstructionSites: ConstructionSiteInfo list
        /// Hostile creeps standing here this tick (ADR 0033, #201).
        Hostiles: HostileInfo list
        /// The invader cores standing here (ADR 0043, #201).
        InvaderCores: InvaderCoreInfo list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomFacts =
    /// A room the world holds nothing for — every entry absent, which is
    /// what a room outside the scan set and a room with no vision both
    /// read as (ADR 0004).
    let empty: RoomFacts =
        {
            Layer = RoomLayer.empty
            Border = Map.empty
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
            Control = None
            Controller = None
            Energy = { Available = 0; Capacity = 0 }
            Spawns = []
            Casting = []
            Refillables = []
            Sources = []
            ConstructionSites = []
            Hostiles = []
            InvaderCores = []
        }

/// One creep of ours, in the world's reading: what it is made of and where
/// it stands, before any colony has claimed it (ADR 0052 decision 1).
///
/// A list and not a map, because the order is the engine's and the Matcher
/// ranks in it: `Game.creeps` hands its creeps back in one order every
/// tick, and a colony's `Creeps` is this list filtered, so nothing about
/// who holds a body moves the order the bodies are matched in.
type WorldCreep =
    {
        /// The room the engine says the creep stands in — its own answer,
        /// so a creep standing in a room no colony works still carries the
        /// name of the room it is in (`World.creepColonies` is what decides
        /// whose it then is).
        Room: string
        /// What the decision layer knows about the body itself.
        Info: CreepInfo
    }

/// Everything this tick was seen to hold, once (ADR 0052 decision 1). The
/// shell builds one (`World.ofGame`, the only code that touches `Game`) and
/// `ColonyView.ofWorld` cuts one colony's share of it; `decide` is written
/// against a view and never against the world, so no rule can reach a room
/// its colony does not work by accident.
///
/// What is **not** here is every conclusion a colony draws: which rooms it
/// works, which creeps are its own, what its bank is, which of its
/// neighbours are children it is raising. Those are the view's, derived
/// from these facts plus the declaration (`Colony.declared`) and the
/// [[stand-down]] gate — so two colonies looking at one room see one set of
/// facts and two answers, which is the whole of ADR 0047 decision 1 in a
/// type.
type World =
    {
        Time: int
        /// Every room the world holds anything for this tick, under its own
        /// name: the declared rooms, whose terrain and furniture need no
        /// vision (ADR 0041), and every room the engine answered
        /// `Game.rooms` with. A room absent here reads `RoomFacts.empty`
        /// (ADR 0004).
        Rooms: Map<string, RoomFacts>
        /// Every creep we own that is not still gestating, in the engine's
        /// own order. Whose each one is this tick is `World.creepColonies`'
        /// answer and is not stored here: the rule needs the [[stand-down]]
        /// gate, which is read out of Memory rather than off the world (ADR
        /// 0043), so a `World` carrying it would be a world that is only
        /// valid after a second pass.
        Creeps: WorldCreep list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module World =
    /// A world holding nothing: no room, no creep. What a test builds up
    /// from, and what a tick before any vision would read as.
    let empty: World =
        {
            Time = 0
            Rooms = Map.empty
            Creeps = []
        }

    /// One room's facts, as ADR 0004 has every other absence: a room the
    /// world carries nothing for reads as a room whose every entry is
    /// absent, never as a lookup that throws.
    let roomOf (world: World) (room: string) : RoomFacts =
        Map.tryFind room world.Rooms |> Option.defaultValue RoomFacts.empty

    /// The rooms we own this tick, off the control entry vision paid for
    /// (ADR 0042). One of the two facts a colony has to pass to be
    /// **living** (`Colony.living`), and the fact a [[stage]] starts from.
    let ownedRooms (world: World) : Set<string> =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) ->
            facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours))
        |> List.map fst
        |> Set.ofList

    /// The rooms one of our spawns stands in, in room-name order. The
    /// other fact `Colony.living` asks for, and the one a [[stage]] reads
    /// as "this colony can cast for itself".
    ///
    /// **Room-name order, and one reader is order-sensitive**: the
    /// undeclared-world fallback takes the head of this list
    /// (`Colony.living`), so which room a bot with no living declaration
    /// runs is the alphabetically first owned spawn room. Before the world
    /// existed the shell handed that list down in `Game.spawns`
    /// enumeration order, so in the one state the fallback fires in — no
    /// declared colony living *and* spawns standing in two owned rooms —
    /// the answer can differ from the pre-`World` bot's (#216 R2a, ADR
    /// 0052). The world has no engine order to offer: it files rooms under
    /// their names, and a spawn sweep's order carried beside them would be
    /// a field for one branch nothing else can read. What it buys is that
    /// the fallback is now a fact about the rooms rather than about the
    /// order an engine happened to enumerate its objects in, and a test
    /// can state it.
    let spawnRooms (world: World) : string list =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) -> not (List.isEmpty facts.Spawns))
        |> List.map fst

    /// The [[stage]] of every declared colony that is one this tick (ADR
    /// 0052 decision 3), derived off the world for the reason the shell
    /// derived it before there was one: a stage decides whether a mother
    /// scans her child's room at all (`Colony.bootstrapping`), so it cannot
    /// be read off a projection the scan set does not exist yet to build.
    ///
    /// Asked of the **declared** homes and of every **living** colony's
    /// home, which is a handful of names either way. The declared ones
    /// because that is what the raising rule asks about — a child a mother
    /// projects is one she may not be able to see the inside of otherwise
    /// — and the living ones because a spawn room no declaration names is
    /// a colony of its own (`Colony.living`'s fallback, ADR 0047), and a
    /// colony with no stage would place no road and keep no rampart
    /// however old it is.
    ///
    /// Not every owned room the world holds, though `stageOf` would answer
    /// for one: a stage entry is read as "a colony of ours lives here", and
    /// the one room that can hold a stage without a declaration is the
    /// fallback's own home, which every reader that is about *another* room
    /// excludes by name (`Decide.isNurseryRoom`). Swept wider, a room we
    /// claimed by hand and never declared would arrive in some colony's
    /// scan set as a [[nursery]] to raise.
    ///
    /// A stage entry therefore still does not say whose business that room
    /// is: that stays the reader's own question (`isNurseryRoom` carries
    /// `colonyOwns` beside the stage), or two mothers would hire
    /// [[pioneer]]s for one child.
    let rec stages
        (tuning: Tuning)
        (colonies: Colony list)
        (world: World)
        : Map<string, ColonyStage> =
        Colony.homes colonies
        @ (living colonies world |> List.map (fun colony -> colony.Home))
        |> List.distinct
        |> List.choose (fun name ->
            let facts = roomOf world name

            let owned =
                facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours)

            Colony.stageOf
                tuning
                owned
                (not (List.isEmpty facts.Spawns))
                (facts.Controller |> Option.map (fun c -> c.Level))
            |> Option.map (fun stage -> name, stage))
        |> Map.ofList

    /// The colonies that run this tick (`Colony.living`, ADR 0047 decision
    /// 1), read off the world's two facts rather than off two sweeps of
    /// `Game.spawns` in the shell.
    ///
    /// Mutually recursive with `stages` above, which asks for the living
    /// homes and nothing else of it: living is decided off ownership and a
    /// standing spawn (ADR 0047), never off a stage, so the pair bottoms
    /// out here and cannot circle.
    and living (colonies: Colony list) (world: World) : Colony list =
        Colony.living (ownedRooms world) (spawnRooms world) colonies

    /// The declaration's two narrowings and the union they make, for one
    /// colony: the [[outpost]]s the [[stand-down]] gate leaves it (ADR
    /// 0043), the rooms it is [[bootstrapping]] for a child of its own
    /// (ADR 0047 decision 4), and its scan set — its home and both of
    /// those (`Colony.roomsProjected`).
    ///
    /// **Written here once and read by both readers there are.** The scan
    /// set decides which creeps this colony adopts (`creepColonies`) and
    /// which rooms its view is cut over (`ColonyView.ofWorld`), and the
    /// two parts are what the cut is made *of* — the outposts place the
    /// declared furniture and pool the rocks, the bootstrapped rooms are
    /// the ones narrowed to the borrowing. A second derivation of the
    /// union is a second answer free to disagree: a room projected with
    /// nothing pooled in it, or pooled with nothing projecting it.
    let scanOf
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        (shut: Set<string>)
        (colony: Colony)
        : Outpost list * string list * string list =
        let outposts = Outpost.worked shut colony.Outposts

        // The two halves of what a mother projects for a child of hers, and
        // they are disjoint by construction: a room she is raising is one we
        // own, and a room she may take back is one we do not (#221).
        let borrowed =
            Colony.bootstrapping stages colonies colony
            @ Colony.reclaiming unowned colonies colony

        outposts, borrowed, Colony.roomsProjected outposts borrowed colony.Home

    /// The declared homes that stand empty this tick: ours to take back if
    /// they ever were ours, and the candidates a human means to take
    /// (#221). Read off the same control entry ownership is read off
    /// everywhere (ADR 0042); a room nothing looked into answers no, which
    /// is ADR 0004's absence and not a claim that somebody holds it.
    let unownedHomes (colonies: Colony list) (world: World) : Set<string> =
        Colony.homes colonies
        |> List.filter (fun name ->
            (roomOf world name).Control
            |> Option.exists (fun control -> control.Owner = Ownership.Unowned))
        |> Set.ofList

    /// The rooms one colony projects this tick, off the world: `scanOf`'s
    /// union with the stages and the ownership it needs read for it.
    let roomsProjected
        (tuning: Tuning)
        (colonies: Colony list)
        (shut: Set<string>)
        (world: World)
        (colony: Colony)
        : string list =
        let _, _, scanned =
            scanOf (stages tuning colonies world) (unownedHomes colonies world) colonies shut colony

        scanned

    /// Which colony holds each creep this tick (`Colony.creepColonies`, ADR
    /// 0047 decision 2), decided over every living colony's scan set at
    /// once and handed to each view: a creep is one colony's business, or
    /// two decisions would write two Tasks into the one flat `assignments`
    /// leaf and move one body twice.
    ///
    /// Two colony lists and not one, for the reason `ofWorld` takes the
    /// declaration too: the scan sets are cut with the **declaration**,
    /// because which of a mother's children she is still raising is read
    /// off it (`stages`, `Colony.bootstrapping`), while the projections
    /// adoption is decided over are the **running** colonies', because a
    /// creep filed under a home that runs no `decide` is a creep in
    /// nobody's `Creeps`. The shut sets are the running colonies' by home
    /// room: a room a gate withheld is projected by nobody, so nobody
    /// adopts the creep standing in it (ADR 0043).
    let creepColonies
        (tuning: Tuning)
        (colonies: Colony list)
        (running: Colony list)
        (shut: Map<string, Set<string>>)
        (world: World)
        : Map<string, string> =
        let projections =
            running
            |> List.map (fun colony ->
                colony.Home,
                roomsProjected
                    tuning
                    colonies
                    (Map.tryFind colony.Home shut |> Option.defaultValue Set.empty)
                    world
                    colony)

        let spawnHomes =
            world.Rooms
            |> Map.toList
            |> List.collect (fun (name, facts) ->
                facts.Spawns |> List.map (fun spawn -> spawn.Name, name))

        Colony.creepColonies
            projections
            spawnHomes
            (world.Creeps |> List.map (fun creep -> creep.Info.Name, Some creep.Room))

/// The cross-colony work one colony may take this tick, named and bounded
/// (ADR 0052 decision 7). Borrowing is an explicit exception and never a
/// narrowed layer: what a [[mother colony]] may do in a child's room is
/// written down here, and everything else the child's room holds stays the
/// child's.
type BorrowedWork =
    {
        /// The home rooms of the children this colony carries in its
        /// projection for a reason that is not mining them, and it is two
        /// reasons: the children it is **raising**, whose Upgrade and Build
        /// its bodies may cross for (ADR 0047 decision 4,
        /// `Colony.bootstrapping`), and the children it has **lost**, whose
        /// controller is a [[claim]] to make (#221, `Colony.reclaiming`).
        /// The two are disjoint by construction — one is a room we own and
        /// the other a room we do not — and they narrow to the same three
        /// kinds, because a Claim asks for exactly what an Upgrade does: a
        /// controller placed on a tile a body can stand beside.
        ///
        /// The view carries only those kinds of target for these rooms, so
        /// the mother pools no Harvest on the child's rock, hires no Anchor
        /// for its Post, counts none of its Seats into her quotas and hauls
        /// none of its energy home.
        ///
        /// The **cap** on the borrowing is a field of `Tuning` rather than
        /// of this list: how many bodies cross is `Tuning.PioneerCount`
        /// (#213) and how much haul is lent is `Tuning.FerryLoads` (#222),
        /// each of them a quota rather than a fact about the child's room.
        /// What stays here is the rooms, which is the fact.
        Rooms: string list
    }

/// One colony's whole reading of this tick: its home room's projection,
/// the rooms it works beside it, the bodies it holds, the bank it casts
/// from and the explicit little it may take of its neighbours' (ADR 0052
/// decision 1). Cut from the `World` by `ColonyView.ofWorld`, one per
/// living colony, and the only argument `decide` has: every function in
/// Decide takes one of these and nothing else, so what a rule can reach is
/// what this colony works.
///
/// Named `Snapshot` until ADR 0052: it was one colony's projection from
/// the tick #191 gave the bot a second colony, and the name went on saying
/// "the game state" while the type said "this colony's share of it".
type ColonyView =
    {
        Time: int
        /// This colony's spawns: the ones it casts from and anchors its
        /// Layout on. A spawn standing in another colony's home is that
        /// colony's, and the spawn a [[nursery]] is waiting for is not
        /// looked for here — whether one stands in a declared home reaches
        /// this colony as that room's [[stage]].
        Spawns: SpawnInfo list
        /// The bodies this colony has in its ovens this tick (#156): its
        /// home room's `RoomFacts.Casting`.
        ///
        /// Its home room's alone, for the reason `Bank` is one account: a
        /// colony casts from the spawns of the room it banks in, so a body
        /// gestating anywhere else was bought by somebody else. Read by the
        /// casting cascade and by nothing else — a body in an oven stands
        /// on no tile, holds no Task and answers no Verdict, so every rule
        /// but the one deciding what to buy next is right to be blind to it
        /// (ADR 0026).
        Casting: BodyPart list list
        /// The **tunables** this colony decides under (ADR 0052 decision
        /// 5): every number the bot chose rather than read off the engine,
        /// arriving on the view like every other fact so that a rule reads
        /// its colony's own and a test moves one field instead of editing
        /// the rule.
        ///
        /// One record per colony and not one per bot, though the shell
        /// hands every colony `Tuning.defaults` today: the fields state the
        /// [[stage]] and the bank they were derived at, and the day a human
        /// wants a child raised on different numbers than its mother the
        /// place to say so is here rather than in a second table inside the
        /// rules.
        Tuning: Tuning
        /// The colony's bank: its **home room's** shared spawn-energy
        /// account, and no other room's (ADR 0052 decision 1). Every spawn
        /// it casts from stands in that room, so one account is the whole
        /// of what it can spend.
        ///
        /// One bank and no longer a fold over the projected rooms, which is
        /// what `richestCapacity` was: the fold read the largest capacity of
        /// any room the colony projected, and a mother projecting a child's
        /// home would have read the child's 300 beside her own 1,800 the
        /// day the rooms swapped places. Four readers take this capacity and
        /// must not disagree — `workforceTarget` prices every row's
        /// replacement at the body this bank casts, the reserver row refuses
        /// to hire where it cannot buy the row's floor body, a Withdraw's
        /// cap divides its store's stock by the body this bank would cast
        /// for the row drawing there (#161), and the hauler row divides the
        /// colony's summed haul by that same hauler's carry (ADR 0049) — so
        /// it is one field rather than four readings.
        Bank: RoomEnergy
        /// Energy-hungry structures in the home room (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources this colony **mines**: every room it works but the
        /// ones it merely [[bootstrap]]s, whose rocks are the child's own
        /// (ADR 0047 decision 4), with every declared outpost rock beside
        /// them whether or not there is vision (`Outpost.pooledSources`,
        /// ADR 0041).
        Sources: SourceInfo list
        /// This colony's own controller — the one it upgrades, whose
        /// downgrade clock it runs against and whose safe mode it fires
        /// (ADR 0047 decision 1). Never a child's, which reaches the pool
        /// as a target in a layer she projects (`Decide.isBorrowedUpgrade`).
        ///
        /// `None` where the projection cannot place it, which is ADR 0004's
        /// absence and not a state: a living colony owns its home room and
        /// so always has one, and every rule that reads this gives the
        /// unplaceable case the answer it gives an unpriceable target.
        Controller: ControllerInfo option
        /// Who holds each room this colony works and has vision in this
        /// tick, under that room's name — what a source's output per tick
        /// is priced from (ADR 0042), and the fact a rule reads to say
        /// whether a room is this colony's business at all
        /// (`Decide.colonyOwns`). Absent for a room vision did not answer
        /// for, per-entry as every other absence is (ADR 0004).
        RoomControl: Map<string, RoomControlInfo>
        /// Our construction sites in every room this colony works and has
        /// vision in (#150): the Build pool is this list one to one
        /// (`Decide.planTasks`), so an outpost's site is a Task like the
        /// home room's, and a bootstrapped child's site is the second half
        /// of what a [[pioneer]] crosses for.
        ConstructionSites: ConstructionSiteInfo list
        /// The creeps this colony holds this tick: the ones it cast, plus
        /// the ones it has adopted, less the ones another colony has
        /// adopted from it (`World.creepColonies`, ADR 0047 decision 2). In
        /// the world's own order, so who holds a body does not move the
        /// order the Matcher ranks bodies in.
        Creeps: CreepInfo list
        /// Hostile creeps standing in any room this colony works and has
        /// vision in, each under its own room's name (ADR 0033, #201).
        Hostiles: HostileInfo list
        /// The invader cores standing in the rooms this colony works and
        /// can see (ADR 0043). Its own list and not a widening of
        /// `Hostiles`: a raider is something a creep runs from this tick, a
        /// core is something a whole room is withheld from for thousands,
        /// and a core is a structure that `FIND_HOSTILE_CREEPS` can never
        /// answer with.
        ///
        /// Read while there is still vision, because that is the only time
        /// it is readable: the creeps paying for the vision in an outpost
        /// are exactly the ones a stand-down withdraws.
        InvaderCores: InvaderCoreInfo list
        /// This colony's spatial projection: the home room and every room
        /// it works beside it, in one projection (ADR 0041, ADR 0005).
        /// `RoomName` is the home room and the `Rooms` keys are the scan
        /// set, so the view's home and the rooms it works are read off the
        /// projection rather than stored a second time beside it.
        ///
        /// Always present, possibly empty — absence is per-entry, never
        /// per-projection (ADR 0004).
        Spatial: SpatialInfo
        /// Every home room a human has declared a colony for
        /// (`Colony.homes`, ADR 0047), this colony's own included and in
        /// declaration order. The **candidate colonies** are the ones
        /// nobody owns yet, and that second half is read off `RoomControl`
        /// in Core (`Decide.claimTargets`) rather than decided when the
        /// view is cut: which rooms a human means to own is declared,
        /// whether we own one is seen, and a view carries facts rather
        /// than conclusions.
        ///
        /// The declaration reaches Core through the view and off no
        /// constant of its own — the rule the outpost furniture already
        /// travels under (`Outpost.place`, ADR 0041) — and off the same
        /// list `ofWorld` cut the rooms and the [[stage]]s from, so
        /// `Colony.declared` is read once for the tick and there is no
        /// second copy of the declaration for it to disagree with (ADR
        /// 0052 decision 1). Empty is the whole of "no colony is
        /// declared": nothing is claimed, and every controller in the
        /// projection is the [[reserve]] it always was.
        Declared: string list
        /// The [[stage]] of every room that is a colony of ours this tick
        /// (`World.stages`, ADR 0052 decision 3) — this colony's own and
        /// its children's alike, the same map handed to every colony
        /// because a stage is a fact about a room and not about who is
        /// looking. A room with no entry is one that is not a colony this
        /// tick, and every reader gives it the answer it gives a room it
        /// knows nothing about.
        Stages: Map<string, ColonyStage>
        /// Where **other colonies'** creeps stand in the rooms this colony
        /// works, each tile carrying its room (ADR 0052 decisions 1 and
        /// 2 — one set since #216 R3, where it was a map keyed by room
        /// name). The bodies this colony
        /// does not hold and cannot move: they are in no `Creeps` list of
        /// hers, on no tile of her layers, and in nobody's Task pool but
        /// their own colony's.
        ///
        /// Carried and not yet arbitrated: today a mother's [[pioneer]]
        /// standing on the child's Anchor tile is invisible to the child's
        /// Resolver, which claims the tile every tick for a body that never
        /// moves (#220). The arbitration that reads this is one movement
        /// pass per room over every creep of ours, which is R2b's — this
        /// field is the fact it needs, cut where the fleet is cut so the two
        /// cannot disagree about who is standing where.
        Foreign: Set<RoomPos>
        /// What this colony may take of a neighbour's, explicitly and
        /// bounded (ADR 0052 decision 7): today the Upgrade and the Build
        /// of a child it is still raising (ADR 0047 decision 4).
        Borrowed: BorrowedWork
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColonyView =
    /// What a colony may see of a room it carries for a child of its own:
    /// the controller its workers upgrade — or, where the child has been
    /// lost, [[claim]] (#221) — the sites they build, and the spawn, not a
    /// target of hers at all but the tile her [[pioneer]]s walk up to and
    /// the structure that says a colony lives here (ADR 0047 decision 4).
    /// Both halves of `BorrowedWork.Rooms` narrow through this one filter:
    /// a Claim asks for exactly what an Upgrade does. Beside the kinds, at
    /// most one store: the [[ferry]]'s sink, picked out by geometry and by
    /// [[stage]] in `ferrySink` below.
    ///
    /// Everything else the room holds is the child's own business, and a
    /// mother carrying it would pool a Harvest on the child's rock, hire an
    /// Anchor for the child's Post, count the child's Seats into her own
    /// quotas and haul the child's energy back across the Seam to her
    /// Storage — the single projection over two colonies ADR 0047's
    /// Considered Options rejected.
    let private borrowable (kind: TargetKind) =
        match kind with
        | Controller
        | Site _
        | Structure BuiltKind.Spawn -> true
        | Source
        | Dropped
        | Tombstone
        | Structure _ -> false

    /// One bootstrapped room's facts, cut down to the borrowed work (ADR
    /// 0052 decision 7). Taken off the whole facts rather than gated when
    /// the world is read: the world reads a room once for everybody, so
    /// what this chooses is not what to *read* but what this colony may
    /// **carry** — one filter over the kind census instead of six
    /// conditions spread through the shell.
    ///
    /// What is left is exactly ADR 0004's per-entry absence — the shape a
    /// room with no vision arrives in — so every rule downstream already
    /// answers correctly for it. The geometry is kept whole, because it is
    /// what the mother's workers walk over: terrain, the border ring the
    /// Seam is read off, and the obstacles and roads that price the
    /// crossing.
    ///
    /// The room's structures' hits go, so no Repair of the child's reaches
    /// her pool; its sources go too, and the pool below drops the room from
    /// the mined set for the same one reason. Of its stores at most one
    /// survives, and it is the [[ferry]]'s (#222, ADR 0052 decision 7) —
    /// see `ferrySink`.
    ///
    /// **The child's upgrade buffer, and no other store of its** (#222): a
    /// built container standing inside the child's own controller's Upgrade
    /// area and on none of its Seats. That store is what a [[ferry]] fills,
    /// so the mother has to be able to see how much room is left in it —
    /// and it is the *only* one she may see, because a source container of
    /// the child's carried here would be a Withdraw in her pool and the
    /// child's income hauled across the Seam to her Storage, which is the
    /// single projection over two colonies ADR 0047 rejected.
    ///
    /// **`Bootstrapping` alone of the rooms this list holds**, which is
    /// where the [[ferry]] lends (`Decide.ferryBuffers`, `haulerQuota`'s
    /// ferry term): `BorrowedWork.Rooms` is the raised children *and* the
    /// lost ones (`Colony.reclaiming`, #221), and a [[nursery]]'s buffer or
    /// a lost child's is a store no rule of the mother's fills. Carried
    /// anyway it is a store of another colony's standing in her projection
    /// with nothing but a Withdraw to be made of it — the cross-Seam drain
    /// this whole narrowing exists to refuse. So the one store that reaches
    /// her is the one store she is hired against, and the reader in Core
    /// can name it by that fact alone rather than re-deriving the geometry
    /// (which it cannot: the sources this join subtracts are dropped by the
    /// very filter it feeds).
    ///
    /// Geometry and not a kind, which is why it is spelled here rather than
    /// in `borrowable` above: the range-3 join is the same one the Planner
    /// makes over her own room (ADR 0019's accepted duplication), and it is
    /// read off the **whole** facts — before the kind filter runs — because
    /// it needs the controller and the sources the filter is about to drop.
    let private ferrySink (stage: ColonyStage option) (facts: RoomFacts) : Set<string> =
        let placed = facts.Layer.TargetPositions
        let tileOf id = Map.tryFind id placed

        let idsOfKind kind =
            facts.TargetKinds
            |> Map.toList
            |> List.choose (fun (id, k) -> if k = kind then Some id else None)

        match stage, idsOfKind Controller |> List.tryPick tileOf with
        | Some Bootstrapping, Some controller ->
            let sources = idsOfKind Source |> List.choose tileOf

            idsOfKind (Structure BuiltKind.Container)
            |> List.filter (fun id ->
                match tileOf id with
                | Some pos ->
                    range pos controller <= 3
                    && not (sources |> List.exists (fun s -> range pos s <= 1))
                | None -> false)
            |> Set.ofList
        | _ -> Set.empty

    let private borrowed (stage: ColonyStage option) (facts: RoomFacts) : RoomFacts =
        let sink = ferrySink stage facts

        let kinds =
            facts.TargetKinds
            |> Map.filter (fun id kind -> borrowable kind || Set.contains id sink)

        { facts with
            Layer =
                { facts.Layer with
                    TargetPositions =
                        facts.Layer.TargetPositions
                        |> Map.filter (fun id _ -> Map.containsKey id kinds)
                }
            TargetKinds = kinds
            Hits = Map.empty
            Stores = facts.Stores |> Map.filter (fun id _ -> Set.contains id sink)
            Sources = []
        }

    /// One colony's view of this tick (ADR 0052 decision 1): the rooms it
    /// works cut out of the `World`, the bodies it holds cut out of the
    /// world's creeps, its own bank and controller, and the explicit little
    /// it may take of a child's.
    ///
    /// **Pure, and that is the point of it** (ADR 0052 decision 8): the
    /// shell reads the engine once (`World.ofGame`) and every rule about
    /// which rooms a colony works, which creeps are its own and what it may
    /// borrow is here, where a test can hand it a two-colony world and read
    /// the answer back — the layer that used to be reachable only by
    /// deploying (#137).
    ///
    /// Five facts are handed in and none is decided here. The **tunables**
    /// are the colony's own numbers (ADR 0052 decision 5), handed in for
    /// the reason the declaration is: a view can be cut under any of them
    /// and not only the ones this bot ships, and the [[stage]] line lives
    /// in one of their fields (`Colony.stageOf`). The
    /// **declaration** is a human's sentence (`Colony.declared`), and it is
    /// handed in rather than read so a view can be built for any
    /// declaration and not only the one this bot ships. The **shut** set is
    /// the [[stand-down]]'s, derived by Core off the previous tick's
    /// [[raid log]] (`Observe.standDown`, ADR 0043) — Memory's answer, not
    /// the world's. The **holders** are `World.creepColonies`' answer, cut
    /// once over every living colony's scan set because no single colony
    /// can see that table. And the **world** is the tick's facts.
    let ofWorld
        (tuning: Tuning)
        (colonies: Colony list)
        (shut: Set<string>)
        (holders: Map<string, string>)
        (world: World)
        (colony: Colony)
        : ColonyView =
        let home = colony.Home
        let stages = World.stages tuning colonies world

        // The declaration's two narrowings and their union, off the one
        // derivation the creep adoption reads too (`World.scanOf`): the
        // outposts the gate leaves (ADR 0043) and the children this colony
        // is still raising (ADR 0047 decision 4). Written here a second
        // time it would be a second answer free to disagree — a room
        // projected with nothing pooled in it, or pooled with nothing
        // projecting it.
        let outposts, bootstrap, scanned =
            World.scanOf stages (World.unownedHomes colonies world) colonies shut colony

        // The scan set with each room's facts beside it, in scan order —
        // a room the world holds nothing for reads empty (ADR 0004), and a
        // room this colony only bootstraps reads the borrowed work alone.
        let worked =
            scanned
            |> List.map (fun room ->
                let facts = World.roomOf world room

                room,
                (if List.contains room bootstrap then
                     borrowed (Map.tryFind room stages) facts
                 else
                     facts))

        // This colony's bodies, and the names to cut its geometry by: a
        // colony's fleet and its layers' occupants are one set, so the two
        // cannot disagree about who is standing where.
        let mine =
            world.Creeps
            |> List.filter (fun creep -> Map.tryFind creep.Info.Name holders = Some home)

        let names = mine |> List.map (fun creep -> creep.Info.Name) |> Set.ofList

        // The three id-keyed tables, merged flat across the worked rooms,
        // because an object id is already unique across the world and
        // layering it would key a unique thing twice (ADR 0041).
        // Deterministic under a collision that cannot happen: the fold
        // walks the scan set in order, and one object stands in one room.
        let mergedBy (select: RoomFacts -> Map<string, 'v>) =
            (Map.empty, worked)
            ||> List.fold (fun acc (_, facts) ->
                (acc, select facts) ||> Map.fold (fun acc id value -> Map.add id value acc))

        let homeFacts = World.roomOf world home

        {
            Time = world.Time
            Spawns = homeFacts.Spawns
            Casting = homeFacts.Casting
            Tuning = tuning
            Bank = homeFacts.Energy
            Refillables = homeFacts.Refillables
            // Every worked room's sources but a bootstrapped child's, whose
            // rocks are the child's to pool (ADR 0047 decision 4, #192),
            // with the declared outpost rocks laid in beside them whether
            // or not there is vision (ADR 0041).
            Sources =
                worked
                |> List.collect (fun (_, facts) -> facts.Sources)
                |> Outpost.pooledSources scanned outposts
            Controller = homeFacts.Controller
            RoomControl =
                worked
                |> List.choose (fun (room, facts) ->
                    facts.Control |> Option.map (fun control -> room, control))
                |> Map.ofList
            ConstructionSites = worked |> List.collect (fun (_, facts) -> facts.ConstructionSites)
            Creeps = mine |> List.map (fun creep -> creep.Info)
            Hostiles = worked |> List.collect (fun (_, facts) -> facts.Hostiles)
            InvaderCores = worked |> List.collect (fun (_, facts) -> facts.InvaderCores)
            Spatial =
                {
                    RoomName = Some home
                    Rooms =
                        worked
                        |> List.map (fun (room, facts) ->
                            room,
                            { facts.Layer with
                                CreepPositions =
                                    facts.Layer.CreepPositions
                                    |> Map.filter (fun name _ -> Set.contains name names)
                            })
                        |> Map.ofList
                    Borders =
                        worked |> List.map (fun (room, facts) -> room, facts.Border) |> Map.ofList
                    TargetKinds = mergedBy (fun facts -> facts.TargetKinds)
                    Hits = mergedBy (fun facts -> facts.Hits)
                    Stores = mergedBy (fun facts -> facts.Stores)
                }
                // The declared furniture goes in last, over the whole
                // assembled projection rather than room by room inside it
                // (`Outpost.place`, ADR 0041): a source's and a
                // controller's id and tile do not wait for vision.
                |> Outpost.place outposts
            Declared = Colony.homes colonies
            Stages = stages
            // The bodies in these rooms that are not this colony's, each
            // tile joined to the room it stands in (ADR 0052 decision 2):
            // a room with none of them contributes nothing, which is the
            // per-entry absence every other room-keyed fact is read under
            // (ADR 0004) and here is simply an empty set.
            Foreign =
                worked
                |> List.collect (fun (room, facts) ->
                    facts.Layer.CreepPositions
                    |> Map.toList
                    |> List.filter (fun (name, _) -> not (Set.contains name names))
                    |> List.map (snd >> RoomPos.at room))
                |> Set.ofList
            Borrowed = { Rooms = bootstrap }
        }

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    /// Take stored energy out of a stocked container (ADR 0012), or out of
    /// the Storage a tier below them (ADR 0023) — the haul cycle's intake,
    /// judged over stores rather than energy's name.
    ///
    /// A tombstone and a ruin are stores too (#167), and this is the verb
    /// for them: the engine's `withdraw` takes either, so the store's kind
    /// changes nothing here — the pool reads the stock, the cap divides it
    /// (#161) and the tier is the containers' own. What is different about
    /// them is only that they end: a store that decays away mid-walk is
    /// gone from the projection and releases its holder through the
    /// task-gone path every vanished Task uses.
    | Withdraw of storeId: string
    /// Walk to a dropped energy pile and take it (#167). The Task half of
    /// what the [[pickup reflex]] does by hand: the reflex takes what is
    /// already within range 1 of a creep standing somewhere for its own
    /// reasons, and this is what sends a creep to a pile that no reflex
    /// will ever reach — a death drop in the open, an [[anchor]]'s
    /// overflow on a [[container]] no hauler is due at.
    ///
    /// Pooled on the pile's amount alone, and only from a threshold
    /// (`Tuning.PickupThreshold`), because a pile under one is not worth a
    /// walk that the reflex would cover for free if anyone ever passed
    /// it. Feeding tier and hauler-shaped applicability, the same as the
    /// Withdraw beside it: which of a pile and a container an empty
    /// carrier goes for is travel cost's call and never a rule's.
    | Pickup of pileId: string
    | Refill of structureId: string
    | Build of siteId: string
    | Repair of structureId: string
    | Upgrade of controllerId: string
    /// Holding a neutral controller with CLAIM parts (ADR 0042): a
    /// reservation is what makes that room's sources worth the held ten a
    /// tick rather than the neutral five, and it decays by one a tick, so
    /// this is work that is never finished. One per projected controller
    /// that is not the colony's own — the engine refuses reserveController
    /// on a room we own, and the colony's own controller is Upgraded, not
    /// reserved.
    ///
    /// Pooled whatever the reservation has left on it: the ticks remaining
    /// size the *body* (`ceil((5000 - ticksToEnd) / 600)` CLAIM, ADR 0042,
    /// #131) and never the Task, because a Task that vanished at the
    /// 5,000 cap would release its holder there and re-match it the tick
    /// after — a flicker ADR 0013 took out of Harvest for the same reason.
    /// Which rooms the colony works at all is the one gate above this, and
    /// it is the projection's: an outpost withdrawn from is out of the
    /// scan set entirely (ADR 0043).
    | Reserve of controllerId: string
    /// Taking a **candidate colony**'s controller for our own with CLAIM
    /// parts (ADR 0047): the act that turns a declared home room into an
    /// owned one, and so the first tick of a second colony. One per
    /// candidate colony — a declared home this colony does not own yet —
    /// and never for a plain [[outpost]], whose controller is [[reserve]]d
    /// instead: claiming costs a GCL level and asks the colony to run the
    /// room, which is a human's decision written in `Colony.declared` and
    /// never a rule the projection can infer.
    ///
    /// A controller carries exactly one of the three Tasks that act on
    /// one, and this is the one that wins: our own is Upgraded, a
    /// neutral controller is Reserved, and a candidate colony's is
    /// Claimed. Pooling Reserve beside it would put two Tasks on one
    /// target for one body to be matched to either, and the reservation is
    /// the work that becomes pointless the tick the claim lands.
    ///
    /// Unlike a reservation, which decays by one a tick, this is work that
    /// is finished the moment it succeeds: the room is ours, the Task is
    /// gone from the next tick's pool because the room is no longer a
    /// candidate, and the body that did it is a `[Claim; Move]` with
    /// nothing left to do. That is the price of the row sharing the
    /// [[reserver]]'s body (ADR 0047) and it is paid once per colony.
    | Claim of controllerId: string
    /// Getting out of a Threat's Reach (ADR 0033). The one Task with no
    /// target and no action: its Work Area is the tiles no Threat can
    /// hurt, and the Emitter issues movement for it and nothing else.
    | Flee

/// The four shapes a body takes as far as a [[capacity]] is concerned (ADR
/// 0052 decision 6) — part arithmetic and never a row's name (ADR 0006),
/// so the classes are the ones the existing gates already cut the fleet
/// along and not a second taxonomy beside them.
///
/// **Ordered and exhaustive**: the tests are asked in this order, because
/// two of them overlap on a real body — the [[anchor]]'s `6W/1C/1M` is
/// Work-heavy *and* carries fewer than one Carry per four Work — and every
/// rule that reads both today reads the heavy one first (ADR 0016 shuts
/// Withdraw before ADR 0046 ever asks about the delivery). A body is
/// exactly one class, so a per-class cap can be counted by folding the
/// holders once.
type BodyClass =
    /// More Work than Move (ADR 0016): the garrison's shape. Its intake is
    /// digging and its work is a [[post]], so it is the class every cap
    /// that is about standing room on a tile is written for.
    | Heavy
    /// Fewer than one Carry per `Tuning.StandingCarryPerWork` Work and not
    /// Heavy (ADR 0046): the [[upgrader]] row, which lives beside the
    /// [[buffer]] and carries one trip's worth. It shares a store with the
    /// generalists and drinks it fifty energy at a time, which is why a
    /// store divides into two different numbers of drawers depending on
    /// which of the two asked (#196).
    | Standing
    /// No Work part at all: the [[hauler unit]]'s shape, and the
    /// [[reserver]]'s beside it — neither can spend anything at a
    /// controller or into a site, so neither is ever what a Work-shaped
    /// capacity is dividing for.
    | Carrier
    /// Everything else — the [[worker unit]], the generalist the colony's
    /// surplus work is done by.
    | Light

/// How many creeps a pooled Task admits at once, set by the Planner and
/// counted by the Matcher (ADR 0052 decision 6). The Matcher knows no Task
/// kinds: every seat rule the colony has — a source's [[seat]]s, a
/// [[post]]'s standing room, a store's stock divided by its drawers' load,
/// the outpost container builders' budget, one holder per controller, the
/// [[pioneer]]s' ceiling on borrowed work — arrives here as numbers and
/// tiles, and `hasCapacity` counts holders against them.
///
/// **Five scopes, and each is a set of [[body class]]es a rule already
/// talks about.** They overlap on purpose, because the colony's own rules
/// overlap: a source says "three [[seat]]s, at most one of them a garrison
/// and at most two of them anybody else" in one breath (ADR 0024, ADR
/// 0051), and the [[buffer]] says "two generalists and eighteen standing
/// bodies, counted apart" in another (#196). A candidate is judged against
/// every cap whose scope its own class falls in, and every one of them
/// must hold; a scope with no number is unbounded and costs nothing.
type Capacity =
    {
        /// Holders of every class together. `None` is unbounded — the
        /// Refills and the surplus work the pool is mostly made of.
        Total: int option
        /// Holders that are `Heavy`: the garrisons, who compete for
        /// standing room with each other and with nobody else (ADR 0024).
        Garrisons: int option
        /// Holders that are **not** `Heavy`: ADR 0051's light crowd, kept
        /// off the Seats a [[post]] has claimed. One number over the group
        /// and not one apiece, because "the Seats beyond the Posts" is a
        /// count of tiles and any body but a garrison may stand on one.
        Commuters: int option
        /// Holders that are `Standing`: the row that lives at the
        /// [[buffer]] and drinks it fifty energy at a time (#196).
        Standing: int option
        /// Holders that are neither `Heavy` nor `Standing`: the
        /// generalists' own share of a store the standing row also drinks
        /// from, divided by the load *they* carry (#196).
        Generalists: int option
        /// Tiles whose standing **heavy** occupant holds a slot against
        /// `Garrisons` whatever Task it holds this tick (#205): the Post
        /// whose container is still a site, where the garrison alternates
        /// Harvest and Build and a cap counting Harvest's holders alone
        /// would read the tile as free on every build tick. Counted
        /// against that one cap and not against `Total`, which counts the
        /// Task's holders exactly as it always did. Empty for every other
        /// Task, which is all of them but one.
        Garrison: Set<RoomPos>
        /// Tiles a candidate standing on is outside every cap above
        /// (#205): the container site under a garrison's own feet, which
        /// the outpost builders' budget does not price because that budget
        /// prices a commute and this body made none. Empty for every other
        /// Task.
        Exempt: Set<RoomPos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Capacity =
    /// A Task with no cap at all: the shape most of the pool takes, and the
    /// one the Matcher answers without ever walking the assignment map.
    let unbounded =
        {
            Total = None
            Garrisons = None
            Commuters = None
            Standing = None
            Generalists = None
            Garrison = Set.empty
            Exempt = Set.empty
        }

    /// One number over every class: a Seat count, a store's stock divided
    /// by one load, one holder per controller.
    let total n = { unbounded with Total = Some n }

    /// Whether any cap at all is set — the question that decides whether
    /// the Matcher pays for a walk over the holders (ADR 0029).
    let isBounded (capacity: Capacity) =
        capacity.Total.IsSome
        || capacity.Garrisons.IsSome
        || capacity.Commuters.IsSome
        || capacity.Standing.IsSome
        || capacity.Generalists.IsSome

/// One entry of this tick's Task pool: the Task, where it ranks and how
/// many bodies it admits (ADR 0052 decision 6). The Planner sets all three
/// and the Matcher reads them — it compares `Priority` and [[travel cost]]
/// and counts holders against `Capacity`, and knows no Task kinds at all.
///
/// Before this the ordering lived in `tierOf`/`rankOfTier` and the caps in
/// `taskCapacities`/`postCapacities`, three tables the Matcher consulted
/// by matching on the Task — so every exception the colony learned (an
/// outpost container site's tier, a [[nursery]]'s, a borrowed Upgrade's
/// ceiling, a light body's share of the Seats) had to be spelled twice,
/// once where the pool was built and once where it was scored, and the two
/// spellings were free to disagree about which Task they meant.
type PooledTask =
    {
        Task: Task
        /// Where this Task ranks against every other, lower first — the
        /// ladder that used to be `tierOf` composed with `rankOfTier`, plus
        /// the [[downgrade deadline]]'s one lift above it (ADR 0007). The
        /// Matcher's first key component; `MatchFactor.Rank` names it.
        Priority: int
        Capacity: Capacity
        /// Whether this is work in a room another colony of ours runs —
        /// the borrowed Upgrade a [[mother colony]] pools for her
        /// [[pioneer]]s, and the child's [[build]]s beside it (ADR 0047
        /// decision 4, #213). Read by one body gate: a [[standing body]]
        /// holds no commuting work, and a Seam crossing is the longest
        /// commute the colony has.
        ///
        /// Set on the Builds too, though that gate never asks them: the
        /// field is a fact about the Task and not an argument to its one
        /// reader, and the Build arm of `applicable` refuses a standing
        /// body every borrowed site anyway — unconditionally, unless it is
        /// standing on that site's own [[post]] (ADR 0046, #205). A reader
        /// asking "is this the child's work?" gets the same answer for both
        /// kinds, which is the answer.
        Borrowed: bool
    }

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
let allBodyParts =
    [ Work; Carry; Move; Attack; RangedAttack; Heal; BodyPart.Claim; Tough ]

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
    | BodyPart.Claim -> "claim"
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
/// towers, the structures a view projects as Refillables. The
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
    /// controller takes the room for this player. Range 1, like the
    /// engine's other four touching acts. The engine's own preconditions
    /// are not restated in Core: a claim needs a GCL level to spare and
    /// answers ERR_GCL_NOT_ENOUGH without one, and that code is read by
    /// nobody but the Executor's log — a Task pooled off the declaration
    /// and a room that stays unowned is the whole of what the decision
    /// layer sees, and it re-pools the Task next tick as it would after
    /// any other failure.
    | ClaimController of creepName: string * controllerId: string
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

/// The spawn-origin walk table (ADR 0032): the traffic-blind walk out of
/// the tiles beside a spawner, for a body's fatigue factor, as whole-tick
/// distances per tile index of one room (ADR 0026, ADR 0029) — the half of
/// a lead paid after the cast. Filled on demand by the Atlas as leads are
/// priced, and handed to the next tick's Atlas while the census signature
/// holds: every input it reads is in the census, so it runs once per
/// census rather than once per tick. Mutable, and heap-only like the memo
/// that carries it.
///
/// **The room in the key is the room the goal stands in**, and it is what
/// lets an outpost's lead ride here too (#169). Under the home room the
/// entry is the spawner's own flood; under a neighbour's it is the whole
/// cross-Seam walk — near leg, crossing and far leg already joined
/// (`Atlas.castWalkTicks`) — so a goal beyond a border costs one array
/// read rather than a flood per creep per tick. Two rooms hold the same
/// coordinates, so without the room a spawner's tile and an outpost's
/// answer would collide on one key; with it an entry means one thing under
/// either name: the ticks a body cast at this spawner needs to stand on
/// each tile of *that* room.
type WalkTable = System.Collections.Generic.Dictionary<Pos * FatigueFactor * string, int[]>

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
type UnservedFooting = { Target: RoomPos; Kind: FootingKind }

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
        Target: RoomPos
        Kind: FootingKind
        Tile: RoomPos
    }

/// The two ends a trunk is routed to (ADR 0011): the controller's
/// Upgrade Work Area, and each spawn's walkable ring. A type of its own
/// because the loss is per goal and not per source — the goals are
/// collected per source, so one source can lose its line to the spawn and
/// keep the one to the controller. The spawn carries its id — the spawn
/// list is a list, and RCL7 adds a second one — where the Upgrade Work
/// Area is the controller's alone (ADR 0005) and needs no name beside its
/// own.
[<RequireQualifiedAccess>]
type TrunkGoal =
    | UpgradeArea
    | Spawn of spawn: string

/// A trunk the Layout could not route (#107): the router paved nothing
/// for this goal, because no tile of it was reachable from the source
/// once the clustered reservation was marked impassable — or because the
/// goal holds no tile at all, an unprojected controller or a spawn whose
/// every neighbour is wall. The two are one answer on purpose: a line
/// that carries nothing is the loss, and which way the geometry failed is
/// not something the colony can act on differently.
///
/// Recorded rather than dropped in silence — an empty path unions into
/// the road plan contributing nothing, so a source paved to nothing is
/// indistinguishable from a trunk that was never asked for. The room is
/// not fixed by saying so: the tiles paved and the tiles reserved are
/// exactly what they were (#105 owns the fix). ADR 0035's argument for
/// the footing shortfall, on the same channel and unchanged: a trunk has
/// no creep to key a Verdict on either.
type UnroutedTrunk = { Source: string; Goal: TrunkGoal }

/// What a container is planned for (ADR 0012): a source, named by its id,
/// or the controller. The two targets the container plan judges, and it
/// judges them independently — a tile can satisfy both at once (a [[dual
/// seat]] is within range 1 of a source and inside the Upgrade Work Area),
/// and ADR 0040 names that edge and leaves it rather than merging them.
/// The source carries its id where the controller needs none, the way a
/// `TrunkGoal`'s spawn does: a room has one controller (ADR 0005) and
/// several sources.
[<RequireQualifiedAccess>]
type ContainerTarget =
    | Source of source: string
    | Controller

/// A container pick the plan did not place because its target is already
/// served by a container standing somewhere else (ADR 0040): the target,
/// the tile the plan picked, and the tile actually serving it. The pick
/// moves when the trunk moves — a commit, not a tick — and the container
/// left on the old tile keeps serving the target, so the colony carries a
/// container on a worse tile rather than two containers.
///
/// Recorded rather than dropped, on the layout record beside the unserved
/// footings (#106) and the unroutable trunks (#107): nothing in this
/// colony demolishes anything (ADR 0040 keeps the orphan and #114 owns the
/// removal), so the difference between the plan and the room is permanent,
/// and an orphan no line anywhere names is a room whose Post and hauler
/// counts are read off geometry the plan no longer wants. Not a Verdict —
/// a container has no creep to key one on (ADR 0035).
type DeferredContainer =
    {
        Target: ContainerTarget
        Pick: RoomPos
        Serving: RoomPos
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
        /// The trunks this plan could not route (#107), one entry per
        /// (source, goal) the router found no path for — derived from the
        /// same census as the site Intents and recomputed with them. Empty
        /// is the healthy answer and rides here all the same, for the
        /// reason `UnservedFootings` does: the App writes it every tick.
        UnroutedTrunks: UnroutedTrunk list
        /// The container picks this plan deferred to a container already
        /// serving their targets (ADR 0040), derived from the same census
        /// as the site Intents and recomputed with them. Empty is the
        /// healthy answer and rides here all the same, for the reason
        /// `UnservedFootings` does: the App writes it every tick.
        DeferredContainers: DeferredContainer list
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

/// The wire spelling of each TrunkGoal, on the Layout channel's Memory
/// leaf beside `footingKindName` (#107). A carrying vocabulary, like the
/// two reason vocabularies (#88): the spawn's id rides beside the name
/// rather than inside it, so a goal is one spelling and not one per
/// spawn.
let trunkGoalName =
    function
    | TrunkGoal.UpgradeArea -> "upgrade-area"
    | TrunkGoal.Spawn _ -> "spawn"

/// The spawn a TrunkGoal names beside its wire name, or None for the goal
/// that names none. The encoder's half of what `trunkGoalOf` reads back,
/// as `releaseReasonNumbers` is ReleaseReason's.
let trunkGoalSpawn =
    function
    | TrunkGoal.Spawn spawn -> Some spawn
    | TrunkGoal.UpgradeArea -> None

/// The TrunkGoal a wire name spells for the spawn the wire carried beside
/// it, or None for a name this vocabulary does not have — and for `spawn`
/// with no id carried beside it at all, which is a row that lost its
/// spawn rather than a goal. An id that is carried but empty is a spawn
/// like any other here; the vocabulary spells names, and what counts as a
/// usable id is the caller's question.
let trunkGoalOf =
    reverseCarrying
        trunkGoalName
        ""
        [ (fun _ -> Some TrunkGoal.UpgradeArea); Option.map TrunkGoal.Spawn ]

/// The wire spelling of each ContainerTarget, on the Layout channel's
/// Memory leaf beside `trunkGoalName` (ADR 0040). A carrying vocabulary
/// like it, and for the same reason: the source's id rides beside the
/// name rather than inside it, so a target is one spelling and not one
/// per source.
let containerTargetName =
    function
    | ContainerTarget.Source _ -> "source"
    | ContainerTarget.Controller -> "controller"

/// The source a ContainerTarget names beside its wire name, or None for
/// the controller, which names none. The encoder's half of what
/// `containerTargetOf` reads back, as `trunkGoalSpawn` is TrunkGoal's.
let containerTargetSource =
    function
    | ContainerTarget.Source source -> Some source
    | ContainerTarget.Controller -> None

/// The ContainerTarget a wire name spells for the source the wire carried
/// beside it, or None for a name this vocabulary does not have — and for
/// `source` with no id carried beside it at all, which is a row that lost
/// its source rather than another target.
let containerTargetOf =
    reverseCarrying
        containerTargetName
        ""
        [
            Option.map ContainerTarget.Source
            (fun _ -> Some ContainerTarget.Controller)
        ]

/// The wire spelling of each StandDownBasis, on the Raid log's Memory
/// leaf (ADR 0043), as `footingKindName` is the Layout channel's. The
/// Raid log's own first vocabulary beside the body parts its roster
/// already carries, and under the same rule: one spelling, written once
/// here, reversed by the table below and round-tripped against the union
/// itself by `Core.Tests`, so a fourth basis added without a name is a red
/// test rather than a stand-down that decodes to nothing.
let standDownBasisName =
    function
    | StandDownBasis.CollapseTimer -> "collapse-timer"
    | StandDownBasis.Reservation -> "reservation"
    | StandDownBasis.Fallback -> "fallback"

/// The StandDownBasis a wire name spells, or None for a name this
/// vocabulary does not have — a row whose basis will not read back is a
/// stand-down that cannot say why, and the shell drops that row rather
/// than inventing a reason for it.
let standDownBasisOf =
    reverseOf
        standDownBasisName
        [
            StandDownBasis.CollapseTimer
            StandDownBasis.Reservation
            StandDownBasis.Fallback
        ]

/// A creep's Move Intent: candidate standing tiles for next tick in
/// preference order, plus a priority (the task rank). Input to the
/// Resolver — not an Intent; the Resolver's output is what becomes one.
///
/// Standing tiles but one (#142): a creep walked at a Seam is given the
/// exit tile itself as its last step, and that is a destination rather
/// than a place to stand — the engine moves a creep off a border tile at
/// the end of the tick, which is why no Seat, Work Area or standing
/// candidate query will ever name one. It is a tile of this room, so the
/// arbitration below settles it exactly as it settles ground.
///
/// Public since #216 R2b, and carrying the room it was registered in: a
/// room's arbitration is one pass over **every** creep of ours standing in
/// it, and the intents that pass folds together come from as many `decide`
/// calls as there are colonies working that room (ADR 0052 decision 7).
/// Since #216 R3 that room is the tiles' own (ADR 0052 decision 2) rather
/// than a `Room` field beside them: the merge keys on `Pos.Room`, and a
/// candidate list is a list of tiles of that same room (#145) because the
/// mover only ever offers a creep its own room's ground.
type MoveIntent =
    {
        Creep: string
        Pos: RoomPos
        Rank: int
        Candidates: RoomPos list
    }

/// One colony's movement for the tick, before a tile of it is arbitrated
/// (#216 R2b): where this colony's bodies stand, which of them fatigue
/// keeps out of the arbitration, what each rested one asked for, and the
/// two attributions only this colony's Atlas can answer.
///
/// It exists because **a room's movement is not one colony's decision**.
/// Two colonies work one room whenever a [[mother colony]] is raising a
/// child (ADR 0047 decision 4), and each `decide` used to arbitrate its own
/// half of that room's traffic against the other half's tiles read as empty
/// — the mother claiming the tile the child's [[anchor]] stands on, every
/// tick, for a body the engine then refuses to move (#220). So `decide`
/// hands its colony's movement out unarbitrated and the shell folds every
/// colony's together, one pass per room over every creep of ours in it
/// (`resolveRooms`).
///
/// Everything here is an input to the room's pass, and the pass that reads
/// it is free to see more of the room than the colony that wrote it did.
/// Everything but `Rerouted`, which is the one Verdict only this colony's
/// Atlas can answer and so rides along already settled.
type Movement =
    {
        /// This colony's creeps in view order — the order its move Intents
        /// and its movement Verdicts leave in (ADR 0009).
        Order: string list
        /// Where the projection places each of this colony's bodies:
        /// creep and tile, the tile carrying its room
        /// (`Atlas.placedCreeps`). A creep the projection cannot place is
        /// in no room's pass, exactly as before.
        Placed: (string * RoomPos) list
        /// The creeps fatigue takes out of arbitration this tick (ADR 0008
        /// decision 1). Their tiles are the pass's walls.
        Tired: Set<string>
        /// The tiles held by bodies this colony does not hold
        /// ([[foreign bodies]], ADR 0052 decision 1), each carrying its
        /// room (decision 2). A wall to the pass
        /// **only while no movement in the fold actually holds the body
        /// standing there**: on the single-colony path — the suite's, and a
        /// tick in which nobody else works this room — a foreign body is a
        /// pre-claimed tile nobody may step into (#220), and in the fold
        /// that raised it the same body is an ordinary occupant its own
        /// colony registered an intent for, so blocking it would freeze the
        /// very creep the fold exists to arbitrate.
        Foreign: Set<RoomPos>
        /// Each rested creep's Move Intent, each a tile of the room the
        /// creep stands in.
        Intents: MoveIntent list
        /// The creeps on the [[verbose list]] whose priced step differs
        /// from their traffic-blind one (ADR 0018, ADR 0030) — the one
        /// movement Verdict that is not the arbitration's own answer and
        /// the one that needs this colony's Atlas, so it is settled here
        /// and carried.
        Rerouted: Set<string>
    }

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
/// it — rerouted, detoured by the occupancy surcharge; or stalled, settled
/// off the tile it asked for by a holder its room's pass cannot name. A
/// creep that simply steps toward its Work Area says nothing: conclusion
/// level means events, not every step. Tasks are named by task id. A creep on the
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
    /// Rested, settled off the tile it asked for first — with nobody the
    /// pass can name holding that tile (#219). Yielded's own case with the
    /// counterpart missing rather than omitted: what holds the tile is a
    /// [[foreign body]] this colony cannot move, or a tile blocked with no
    /// occupant filed against it, and a Verdict that invented a name for it
    /// would name the wrong creep. Whether the creep stood still or took
    /// its tail and sidestepped is not the distinction — where it *went* is
    /// the Intent's business, and this says what it did not get. What it
    /// fills is the timeline's silence — a kept traveller that fails to
    /// move, tick after tick, used to emit nothing at all.
    | Stalled of creep: string

/// What one tick of deciding returns: the Intents to execute, the
/// Assignments to remember for next tick, the plan memo to hold in heap
/// for next tick (ADR 0017), the Verdicts explaining them (ADR 0009), and
/// this colony's [[move intent]]s before anybody arbitrated them.
type Decision =
    {
        Intents: Intent list
        Assignments: Assignments
        Memo: PlanMemo
        Verdicts: Verdict list
        /// This colony's unarbitrated movement (#216 R2b). `decide` folds
        /// it through the one-colony pass itself, so `Intents` and
        /// `Verdicts` are a whole answer on their own; a shell running more
        /// than one colony hands every colony's here to `resolveRooms`
        /// instead, arbitrating each room once over every creep of ours in
        /// it (`decideUnarbitrated`).
        Movement: Movement
    }
