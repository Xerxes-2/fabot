module Fabot.Core.Types

/// A creep body part, the engine's full vocabulary. Our own bodies use only
/// Work/Carry/Move; the rest arrive on hostile creeps. `BodyPart.Claim` is
/// qualified wherever it means a part, because `Task.Claim` (ADR 0047) shares
/// the engine's name and is declared later.
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

/// The engine's own numbers (ADR 0052 decision 5), each named for the server
/// constant it spells. A number belongs here when changing it would be a **lie
/// about the server**, and in `Tuning` below when changing it would be a
/// **different colony**.
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
        /// is looked at again (#165): the room is re-admitted to the colony's
        /// scan set on one tick in every this many, and to nothing else — it
        /// stays out of the Task pool and out of the quotas throughout — so one
        /// tick of vision can clear a latch the gate's own withdrawal would
        /// otherwise make permanent. 5,000 ticks, twice ADR 0043's stronghold
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
            RampartFloor = 100_000
            PickupThreshold = 100
            ReachMargin = 2
            StandingCarryPerWork = 4
            PioneerCount = 3
            FerryLoads = 1
            SafeModeDeadline = 3
            StorageLevel = 4
            HorizonLevel = 6
            OutpostBuilders = 2
            BootstrapLevel = 3
            TrunkSwampWeight = 3
            StandDownFallback = 2500
            RivalRecheck = 5000
            QuietGap = 50
            VisionGrace = 150
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

/// What a built structure is — or what a construction site will become once
/// built. Projection vocabulary, distinct from the Intent vocabulary of
/// placeable kinds: every placeable kind widens into one of these
/// (`builtKindOfPlaceable`), never the other way.
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
    /// 0034). Walkability answers for it before anything else does: a creep
    /// may stand on a rampart, and folding it into Other would make every kind
    /// the decision layer does not model walkable with it.
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

/// The colony's [[refill cluster]] (ADR 0054): the colony's spawn and every
/// extension of it, read as **one** Refill target. One Task is one place a body
/// walks to once, and `task-gone` fires when the whole ring is full, where one
/// Task per structure had a loaded body lose its extension to whoever filled it
/// while it walked. **The spawn is the key**: the member every cluster has, and
/// the door the [[hauler unit]] quota already prices its leg to (ADR 0052
/// decision 4).
type RefillCluster =
    {
        /// The Task's target id: the cluster's spawn, and the id every
        /// Assignment, [[verdict]] and [[transition log]] line names this
        /// Refill by.
        Spawn: string
        /// Member id -> the energy it can still take this tick. Every member,
        /// full ones included, so the map is the ring's membership and not
        /// this tick's fill level; what the [[work area]] and the
        /// [[emitter]]'s pick read is the **hungry** subset (`hungry`).
        Members: Map<string, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RefillCluster =
    /// The cluster this colony's Refillables make, or None where no spawn
    /// stands among them to key one (ADR 0054). Membership is by kind and not
    /// by geometry: every spawn-feeding structure is in the one cluster however
    /// far the [[layout]] put it, and the walk is still short because the Work
    /// Area is the union of the members' rings.
    let ofRefillables (refillables: RefillableInfo list) : RefillCluster option =
        let members =
            refillables
            |> List.filter (fun r -> r.Kind = BuiltKind.Spawn || r.Kind = BuiltKind.Extension)

        let spawns =
            members
            |> List.filter (fun r -> r.Kind = BuiltKind.Spawn)
            |> List.map (fun r -> r.Id)

        match spawns with
        | [] -> None
        | spawns ->
            Some
                {
                    Spawn = List.min spawns
                    Members = members |> List.map (fun r -> r.Id, r.FreeCapacity) |> Map.ofList
                }

    /// The energy the whole cluster can still take: what pools the Task at
    /// all and what its [[capacity]] divides into holders (ADR 0054).
    let free (cluster: RefillCluster) =
        cluster.Members |> Map.fold (fun total _ room -> total + room) 0

    /// The members with room left, in id order — the structures the Work Area
    /// is laid over and the only ones the Emitter may transfer into (ADR
    /// 0054). A body stopped beside a full extension has nothing to pour,
    /// which is this Task shape's own churn re-entered through the geometry.
    let hungry (cluster: RefillCluster) =
        cluster.Members
        |> Map.toList
        |> List.choose (fun (id, room) -> if room > 0 then Some id else None)

/// What the decision layer knows about one energy source this tick.
type SourceInfo =
    {
        Id: string
        /// Ticks until the source holds energy again — its restock (ADR 0013,
        /// widened by ADR 0025); 0 while it holds energy now. Not the amount:
        /// the one time fact a decision reads about a source, so a drained
        /// source's Harvest is judged at the creep's arrival.
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
/// three answers and not a username, the same closed shape as `Ownership`
/// below. The third is load-bearing and not a refinement of the second — ADR
/// 0043 gives an NPC invader's reservation and another *player's* different
/// meanings, and each is a **clock** a [[stand-down]] runs to that the other's
/// rule would read wrong: the Invader's is the core's deadline and never
/// answers earlier than the fallback, because the core re-takes the hold it
/// lets lapse (#136); a player's is the hold itself, read literally and with no
/// floor, because a claimer that stops coming re-takes nothing (#165). What
/// carries no clock at all is a player's *ownership*, which is `Ownership`'s
/// answer and not this one's.
[<RequireQualifiedAccess>]
type ReservationHolder =
    /// This colony's own CLAIM parts. The one answer that doubles the
    /// room's sources and the one the reserver row sizes itself from.
    | Ours
    /// The NPC Invader — the holder of the reservation a level-0 core takes
    /// with `attackController` in a room it expanded into (ADR 0043,
    /// docs/research/remote-mining.md §8.4). Worth the neutral rate like any
    /// hold that is not ours, and, unlike a rival's, an expiry: this lapses.
    | Invader
    /// Another player. Worth the neutral rate, and a [[stand-down]] that
    /// runs to the end of the hold itself (#165) — the room is being worked
    /// by somebody else for exactly as long as the engine says it is. The
    /// abandonment trigger every mature bot implements; its permanent half
    /// is `Ownership.Rival`.
    | Rival

/// The reservation standing on one room's controller this tick (ADR
/// 0042): a neutral controller held by CLAIM parts, which doubles every
/// source in that room, decays by one a tick and caps at 5,000.
type ReservationInfo =
    {
        /// Whose CLAIM parts hold it — which of the three, never which
        /// string: the engine answers holding with a username, and both names
        /// that would have to be compared are the shell's to know. A
        /// reservation somebody else holds reads for *pricing* exactly as no
        /// reservation does, which is a colony decision and not the engine's
        /// arithmetic: the colony prices it at five because a room somebody
        /// else holds has stopped being ours to work (ADR 0043).
        Holder: ReservationHolder
        /// Ticks left on the reservation — what the reserver row sizes and
        /// quotas from, `ceil((5000 - this) / 600)` CLAIM parts (ADR 0042).
        /// Read as the colony's own hold only where `Holder` is `Ours`; under
        /// `Invader` it is the deadline ADR 0043 falls back to, and under
        /// `Rival` the one the stand-down #165 clocks runs to.
        TicksToEnd: int
    }

/// Whose a room's controller is, as the colony reads it: three answers and not
/// a username. Two of them are what ADR 0042 prices a source from — ours is
/// the held rate, nobody's is half — and the third is what ADR 0043's
/// clockless withdrawal is judged on, the one trigger the engine gives no end
/// for (#165). A closed vocabulary rather than a pair of booleans, because
/// "ours" and "somebody else's" answer one question. "We cannot see" is the
/// absence of the whole entry (ADR 0004).
[<RequireQualifiedAccess>]
type Ownership =
    /// Nobody owns the controller — the shape every neutral room and every
    /// outpost the colony works arrives in, and the shape a room with no
    /// controller at all is projected as. Reservable, and worth half until it
    /// is reserved.
    | Unowned
    /// This colony owns it: the spawn room, and nothing else while there
    /// is one colony. Worth the held ten a tick, and never reserved — the
    /// engine refuses `reserveController` on a room anybody owns.
    | Ours
    /// Another player owns it. The engine yields ten a tick in a rival's room
    /// exactly as in ours, and the colony prices it at five all the same, for
    /// the reason `ReservationInfo.Holder` gives (ADR 0043). No NPC case: an
    /// invader core *reserves* and never owns.
    | Rival

/// Who holds one room the colony can see this tick — the fact a source's
/// output is read from (ADR 0042), ten energy a tick being the *held* rate and
/// a neutral room's source yielding five. One entry per room vision answered
/// for; a room the colony cannot see has no entry, and that absence is not
/// "half" but unpriceable (ADR 0004).
type RoomControlInfo =
    {
        /// Whose the room's controller is. Read *beside* the reservation and
        /// never instead of it: the engine gives a room with an owner the same
        /// 3,000 a cycle it gives a reserved one, so "reserved, or half" would
        /// price the colony's own sources at five.
        Owner: Ownership
        /// The reservation standing on the room's controller; None where
        /// nothing reserves it. *Which* rival holds it is deliberately not
        /// carried, no rule reading a rival's name. What the pair does carry is
        /// every question ADR 0043 asks: whether somebody else holds this room,
        /// and whether the holder is the NPC whose reservation is a clock.
        Reservation: ReservationInfo option
        /// Whether the room's controller is under safe mode this tick.
        /// Carried per room and not on the colony's own controller alone:
        /// safe mode shields the room it is in, whoever is looking, and only
        /// a room *we* own shields us. False where no controller stands.
        SafeMode: bool
    }

/// A tile of a **named** room: the coordinate a value carries once it leaves
/// the grid it indexes (ADR 0052 decision 2). Two rooms hold the same
/// fifty-by-fifty coordinates, so a bare `Pos` handed between functions is
/// joined to a room by convention alone — the convention that produced a
/// phantom [[post]] and a hostile in an [[outpost]] measured at range 0 from
/// home. Declared **before** `Pos` and never after it: F# resolves a bare `.X`
/// on an un-annotated value to the *last* record type declaring that field.
type RoomPos = { Room: string; X: int; Y: int }

/// A tile coordinate inside a room. Kept as the **grid** coordinate (ADR 0052
/// decision 2): a key of `RoomLayer.Terrain`, of `Obstacles`, of the flood
/// arrays and of every Seat, Reach and Work-Area grid the Atlas lays per room.
type Pos = { X: int; Y: int }

/// Screeps range: Chebyshev distance between two tiles of **one** room. The
/// one definition — the Atlas's geometry, the two hostile reflexes and the
/// Raid log's closest approach all measure with it. Takes grid coordinates;
/// `RoomPos.range` is the same measure for tiles that carry their own rooms.
let range (a: Pos) (b: Pos) = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomPos =
    /// The grid coordinate, for indexing that room's own tables — always
    /// safe, at the moment a reader has decided which room's grid it reads.
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

    /// The same narrowing as `inRoom`, answered as a **list** and not a second
    /// set — the shape every flood takes its goals in, and the hot one:
    /// building a `Set<Pos>` per ask would copy the area and pay a tree of
    /// comparisons for an answer the callers only index a grid array with. The
    /// order is the set's reversed, which every caller settles with a total
    /// key.
    let tilesIn (room: string) (tiles: Set<RoomPos>) : Pos list =
        ([], tiles)
        ||> Set.fold (fun acc tile -> if tile.Room = room then pos tile :: acc else acc)

    /// Chebyshev range between two tiles that carry their rooms, and **None**
    /// across a border (ADR 0052 decision 2). Not a large number and not an
    /// error: two rooms' coordinate systems are not one metric space, and
    /// every reader that decided it by accident decided "range 0" (#204).
    let range (a: RoomPos) (b: RoomPos) : int option =
        if a.Room = b.Room then
            Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
        else
            None

/// What a room's **name** says about where the room is, and nothing else it
/// says: the engine's own grammar, read here so that the two questions the
/// colony asks of a pair of names — which border they share, and whether they
/// share one at all — are one subtraction and not two rules (ADR 0041).
module RoomName =
    /// A room's place on the world grid, read off its name — `W12S28` is
    /// (-13, 28). West and North count outward from the origin, so they run
    /// negative (`W n` is x = -n-1, `N n` is y = -n-1) and East and South run
    /// straight up, which turns "are these two rooms neighbours, and across
    /// which border" into subtraction. None for a name outside the engine's
    /// grammar, which is unplaceable geometry like any other (ADR 0004).
    let private worldCoordsOf (roomName: string) : (int * int) option =
        let isDigit index =
            index < roomName.Length && roomName.[index] >= '0' && roomName.[index] <= '9'

        let rec endOfDigits index =
            if isDigit index then endOfDigits (index + 1) else index

        let number start stop =
            if stop <= start then
                None
            else
                let mutable value = 0

                for index in start .. stop - 1 do
                    value <- value * 10 + (int roomName.[index] - int '0')

                Some value

        // Outward from the origin is negative, towards it positive.
        let axis letter outward inward value =
            if letter = outward then Some(-value - 1)
            elif letter = inward then Some value
            else None

        let xEnd = endOfDigits 1
        let yEnd = endOfDigits (xEnd + 1)

        if yEnd <> roomName.Length then
            None
        else
            match number 1 xEnd, number (xEnd + 1) yEnd with
            | Some x, Some y ->
                match axis roomName.[0] 'W' 'E' x, axis roomName.[xEnd] 'N' 'S' y with
                | Some worldX, Some worldY -> Some(worldX, worldY)
                | _ -> None
            | _ -> None

    /// The step from one room to another on that grid — the neighbour's world
    /// position minus this room's, which is what says *which* border they
    /// share. None where either name is outside the grammar.
    let offsetOf (fromRoom: string) (toRoom: string) : (int * int) option =
        match worldCoordsOf fromRoom, worldCoordsOf toRoom with
        | Some(hereX, hereY), Some(thereX, thereY) -> Some(thereX - hereX, thereY - hereY)
        | _ -> None

    /// Whether two rooms share a border: exactly one axis apart by one, which
    /// is the whole of what a [[seam]] can join (ADR 0041). Screeps has no
    /// diagonal exit, so a room a single axis step away is the only kind a
    /// creep reaches without crossing a third room — `Atlas.borderPairs` names
    /// tiles for those four offsets and for no other, so a pair this refuses
    /// has an empty Seam band by construction and every cross-room price over
    /// it is `None` (ADR 0004). The implication runs **one way only**, and the
    /// difference is load-bearing: this reading is over names, so it can be
    /// asked of a declaration before any terrain is read; the Seam is over
    /// tiles, so a bordering pair whose shared column or row the engine walled
    /// end to end is a neighbour here and has no band there — W12S27's west
    /// column in `tests/Core.Tests/rooms/` is exactly that, and `AtlasTests`'
    /// "a walled border is a neighbour with no band" pins it. So this answers
    /// whether the declaration is *shaped* like one a Seam could join, never
    /// whether one does. A room is not its own neighbour, and a name outside
    /// the grammar neighbours nothing.
    let neighbouring (fromRoom: string) (toRoom: string) : bool =
        offsetOf fromRoom toRoom |> Option.exists (fun (dx, dy) -> abs dx + abs dy = 1)

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
    /// A dropped energy pile. Two readers: the [[pickup reflex]], which takes
    /// what is already at a creep's feet and reads no amount, and the Pickup
    /// Task (#167), which walks a hauler to a pile big enough to be worth the
    /// trip and reads the amount out of `SpatialInfo.Stores`.
    | Dropped
    /// A tombstone or a ruin: a store with a clock on it. One kind for both
    /// engine objects, because the only thing any reader decides on is that it
    /// holds energy and will be gone, and `Withdraw` is the verb for either.
    | Tombstone

/// Whether a projected target is one of the two transient kinds — a pile or a
/// tombstone/ruin — that stand on a tile without holding it. Both vanish
/// within a few hundred ticks, so a census that let one keep a construction
/// site off its tile would make the Layout's ordering depend on where a creep
/// happened to die (ADR 0011's determinism).
let isTransient =
    function
    | Dropped
    | Tombstone -> true
    | Source
    | Controller
    | Structure _
    | Site _ -> false

/// One room's geometry, filed under that room's name (ADR 0041): every
/// container the projection keys by `Pos`, gathered into one record rather
/// than five maps side by side, so reading a room's geometry is one lookup.
/// The id-keyed containers stay outside it, an object id being unique across
/// the world already. Absence stays per entry (ADR 0004): a room missing entry
/// by entry inside its layer and a room with no layer at all are the same
/// answer, so geometry is read through `SpatialInfo.layerOf` and never as
/// `.[name]`, which throws on a room the projection names but holds none for.
type RoomLayer =
    {
        /// Terrain per tile over this room's ground (x,y in 1..48); a tile
        /// absent from the map is impassable. The border ring is not here and
        /// is not ground: it rides in `SpatialInfo.Borders`, which the Seam
        /// query alone is priced off (ADR 0036, ADR 0041).
        Terrain: Map<Pos, Terrain>
        /// Target id -> that target's tile in this room: the Task targets, and
        /// the piles and tombstones a hauler is sent to. The two
        /// transient kinds are filtered out by kind where standing on a tile
        /// is not the same as holding it (`isTransient`).
        TargetPositions: Map<string, Pos>
        /// Creep name -> the tile the creep stands on in this room.
        CreepPositions: Map<string, Pos>
        /// Tiles blocked by obstacle structures and by their construction
        /// sites — the engine refuses to move a creep onto its own
        /// obstacle-type site; impassable regardless of terrain.
        Obstacles: Set<Pos>
        /// Tiles holding a built road — built structures only, a road
        /// construction site is not yet a road (ADR 0010).
        Roads: Set<Pos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomLayer =
    /// A room with nothing in it — every entry absent. What a `tryFind` on
    /// `SpatialInfo.Rooms` defaults to, so a room the projection holds no
    /// geometry for reads the same as one whose every container is empty (ADR
    /// 0004).
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
        /// Which entry of `Rooms` is the home room — the room the colony plans
        /// for, which is the room its spawn happens to stand in and is never
        /// defined by that (ADR 0041), and the room name the census signature
        /// and the Layout read (ADR 0017). None for a projection that names no
        /// room, whose geometry is filed under the empty name.
        RoomName: string option
        /// Room name -> that room's geometry, and the *only* place a
        /// `Pos`-keyed container lives: there is one projection and one shape
        /// of it (ADR 0005, ADR 0041). `RoomName` says which entry is home;
        /// every other entry is an outpost. Read an entry through
        /// `SpatialInfo.layerOf`: a room with no geometry has no entry here at
        /// all, and that is the same answer (ADR 0004).
        Rooms: Map<string, RoomLayer>
        /// Room name -> the terrain of that room's border ring (x or y of 0
        /// or 49), which a layer's `Terrain` deliberately leaves out. A layer
        /// of its own and never ground (ADR 0041): the engine moves a creep
        /// that ends its tick on an exit tile into the neighbouring room, so
        /// admitting one as walkable would let a Seat or a Work Area teleport
        /// the creep out from under its Task. It enters no weight grid or
        /// walkable set — the Atlas lays it a grid of its own — and only the
        /// Seam query and the crossing's price read it (ADR 0004).
        Borders: Map<string, Map<Pos, Terrain>>
        /// Task-target id -> what kind of thing stands (or will stand)
        /// there. Id-keyed and so unlayered (ADR 0041): an object id is
        /// already unique across the world, and the layer that places the
        /// id *is* the room it stands in (`SpatialInfo.placementOf`).
        TargetKinds: Map<string, TargetKind>
        /// Target id -> current/max hits, repairable kinds only — the decaying
        /// roads and containers (ADR 0010, ADR 0012), the Keep and our own
        /// ramparts (ADR 0034); fields nobody decides on stay out.
        Hits: Map<string, HitsInfo>
        /// Target id -> energy currently stored: the stock the logistics Tasks
        /// judge a store by. The containers (ADR 0012) and the Storage (ADR
        /// 0023) are the standing stores, and the two transient ones are here
        /// on the same key — a tombstone's or a ruin's energy, and a pile's
        /// amount.
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

    /// The name the projection's own room is filed under: `RoomName`, and the
    /// empty name when it names none — the name the census signature has
    /// always spelled that way. Decided here once, so a site cannot file the
    /// home room under one name and read it under another, which ADR 0004
    /// would answer with the empty set rather than a throw.
    let homeName (spatial: SpatialInfo) : string =
        spatial.RoomName |> Option.defaultValue ""

    /// One room's geometry, as ADR 0004 has every other absence: a room the
    /// projection carries no layer for reads as a room whose every entry is
    /// absent, never as a lookup that throws. The one spelling of that read,
    /// so no reader has to remember the default.
    let layerOf (spatial: SpatialInfo) (room: string) : RoomLayer =
        Map.tryFind room spatial.Rooms |> Option.defaultValue RoomLayer.empty

    /// The room the projection files a target id under, with its tile there,
    /// and None for a target it does not place, which classifies nothing and
    /// blocks nothing (ADR 0004). The id-to-room join on the projection itself,
    /// beside the one the Atlas precomputes (`TargetAt`); the two answer alike,
    /// the Atlas filling `TargetAt` by walking these same layers.
    let placementOf (spatial: SpatialInfo) (id: string) : RoomPos option =
        spatial.Rooms
        |> Map.tryPick (fun room (layer: RoomLayer) ->
            Map.tryFind id layer.TargetPositions |> Option.map (RoomPos.at room))

/// One outpost: a neighbouring room this colony mines and does not own.
/// Declared, never discovered (ADR 0041) — a constant a human moves in a
/// commit, exactly as the Layout's horizon is (ADR 0039) — because every "the
/// first creep to walk in writes it down" scheme has to answer what sent the
/// first creep, and answering it means inventing scouting and room intel. What
/// is declared is exactly what vision cannot be waited for: the room's name,
/// and the id and tile of each source and of the controller. Everything that
/// actually changes is read off the projection where there is vision and is
/// absent entry by entry where there is none (ADR 0004). The ids are the
/// **engine's own**: a declaration written in readable short names would match
/// nothing on a live server, and would do it in silence.
type Outpost =
    {
        RoomName: string
        /// The room's sources, each under the id the engine knows it by, and
        /// each tile joined to the room it is a tile of (ADR 0052 decision 2).
        Sources: (string * RoomPos) list
        /// The room's controller, whose reservation is what doubles those
        /// sources (ADR 0042). Not optional: an unreserved source is worth
        /// half, so a room with no controller to reserve — a sector centre
        /// or a Source Keeper room — is not a candidate outpost at all.
        Controller: string * RoomPos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Outpost =
    /// The declarations the colony works this tick: the declared list, less
    /// every room a [[stand-down]] is withholding (ADR 0043). The gate, and the
    /// one place *that* gate narrows the set — `World.scanOf` narrows it once
    /// more beside this, on the declaration's own geometry (`neighbouring`,
    /// #243), and the two are one clause apiece there. The gate's other half
    /// (`StandDown.Rechecked`, #165) never reaches here: a room it re-admits to
    /// the scan is still withheld from the work, so a room this drops stays
    /// dropped whatever tick the recheck falls on.
    let worked (shut: Set<string>) (outposts: Outpost list) : Outpost list =
        outposts
        |> List.filter (fun outpost -> not (Set.contains outpost.RoomName shut))

    /// Whether a declared outpost is one its home can work **at all**: the two
    /// rooms share a border, so a [[seam]] joins them (`RoomName.neighbouring`,
    /// ADR 0041). Not a gate that opens and shuts like the [[stand-down]]'s —
    /// it is a fact about the declaration a human wrote, and it answers the
    /// same on every tick of that declaration's life.
    let neighbouring (home: string) (outpost: Outpost) : bool =
        RoomName.neighbouring home outpost.RoomName

    /// The declared outposts a home shares no border with, by name (#243).
    /// ADR 0041 prices a crossing over one Seam band and no more — `walk =
    /// min over seams (near leg + 1 + far leg)`, each leg a flood that never
    /// leaves its room — so a room two hops out is not a badly-priced outpost
    /// but an unpriceable one: `pricedAcross` and `haulRoundTripTicks` answer
    /// `None` for every target in it, and by ADR 0004 unpriceable geometry
    /// never counts against a Task. Worked anyway, such a room is projected,
    /// pooled and hired for — ADR 0042's reserver row hires one body per
    /// declared outpost — and every body bought for it stands beside the spawn
    /// for its whole life with nothing to say why. So the declaration is
    /// **refused** here rather than accepted and never worked, and the refusal
    /// is named: `ColonyView.Refused` carries it to the colony's [[layout
    /// record]], and the test over `Colony.declared` is what makes a human's
    /// slip red before it is deployed. Read off the whole declaration and not
    /// off `worked`'s survivors: a room this refuses is wrong whatever the
    /// stand-down is doing about it this tick. Multi-hop outposts are a
    /// feature and not a bug fix — they need a Seam join of their own — and
    /// they are deliberately not this rule's business.
    let refused (home: string) (outposts: Outpost list) : string list =
        outposts
        |> List.filter (neighbouring home >> not)
        |> List.map (fun outpost -> outpost.RoomName)

    /// The rooms the shell projects this tick: the home room, and every
    /// declared outpost beside it (ADR 0041). One projection covering several
    /// rooms, never a second one (ADR 0005) — the union is taken here so the
    /// rule has one statement, and the outposts are handed in rather than read
    /// off the constant, so the stand-down gate (ADR 0043) has exactly one
    /// place to narrow the set.
    let roomsProjected (outposts: Outpost list) (home: string) : string list =
        home :: (outposts |> List.map (fun outpost -> outpost.RoomName))
        |> List.distinct

    /// One declaration as projection entries: the controller and then the
    /// sources in their declared order, each id paired with the tile the
    /// declaration names and the kind it is. Position and kind are read off one
    /// list rather than two, so the folds below cannot place an id the kind
    /// census misses or classify one nothing places; the Harvest pool reads it
    /// too (`pooledSources`), because a rock nothing places must not be pooled.
    /// A declared tile filed under another room name is **dropped** here rather
    /// than written onto this room's coordinate (ADR 0052 decision 2).
    let private furnitureOf (outpost: Outpost) : (string * Pos * TargetKind) list =
        (fst outpost.Controller, snd outpost.Controller, Controller)
        :: (outpost.Sources |> List.map (fun (id, tile) -> id, tile, Source))
        |> List.filter (fun (_, tile, _) -> tile.Room = outpost.RoomName)
        |> List.map (fun (id, tile, kind) -> id, RoomPos.pos tile, kind)

    /// The declared furniture, laid into the projection: for every scanned
    /// outpost, its sources and its controller at the tiles and under the ids
    /// the declaration names — whether or not the colony has vision there. This
    /// is the half of ADR 0041 that vision may not gate, and the deadlock the
    /// ADR breaks: a source's position needs vision, vision needs a creep
    /// there, a creep goes there because a Task exists, and the Task exists
    /// because the source is in the projection. A declared fact is in the
    /// projection because a human wrote it down; only what actually changes
    /// waits for vision (ADR 0004). Vision wins every entry it holds: the
    /// declaration is laid *under* what the room's `find` families answered.
    /// The two agree by construction — the ids are the engine's own and a rock
    /// does not move — so this decides which truth is authoritative rather than
    /// resolving a conflict. The controller's tile joins `Obstacles`, so a
    /// reserver stands beside it and never on it.
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
                                // furniture already checked against this
                                // room: a declaration whose controller names
                                // another room blocks no tile here.
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
    /// with, and every declared outpost rock beside them. One pool ranked in
    /// one order (ADR 0041), so a rock the colony cannot see this tick is a
    /// Task all the same. Deduplicated by id with the seen list first, the
    /// engine's answer carrying this tick's restock. An unseen rock restocks in
    /// 0 ticks: ADR 0025's "holds energy" default. A restock is a *time* fact
    /// and the unknown one is not "for ever" — priced at 0 the source is judged
    /// at arrival like any other, and the Emitter's own gate withholds the dig
    /// from a rock that turns out empty. Scanned rooms only, and read off
    /// `furnitureOf` rather than off `outpost.Sources`, so the rocks are pooled
    /// exactly where the furniture is laid and a stand-down narrows both at
    /// once. A pool that took the declaration straight would name rocks nothing
    /// places — and an unplaced target prices at 0 (ADR 0004's escape), so it
    /// would *win* its tier.
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
    /// captures relative to W12S28 with both of them laid in. The ids and
    /// tiles are the engine's, pinned against the committed captures.
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

    /// W13S28's south outpost, declared 2026-09-07 off the remote survey
    /// (`docs/research/remote-candidates.md`): two sources, a 12-tile south
    /// Seam, the best net income of the five rooms a Seam reaches. The ids
    /// and tiles are the engine's, read the day it was declared.
    let w13s29: Outpost =
        {
            RoomName = "W13S29"
            Sources =
                [
                    "6a8caaaddd4872bccd319365", { Room = "W13S29"; X = 29; Y = 6 }
                    "6a8caaaddd4872bccd319366", { Room = "W13S29"; X = 14; Y = 29 }
                ]
            Controller = "6a8caaaddd4872bccd319367", { Room = "W13S29"; X = 15; Y = 41 }
        }

/// The [[stand-down]] gate's whole answer for one colony this tick (ADR 0043 as
/// #165 narrows it), derived once off that colony's [[raid log]]
/// (`Observe.standDown`) and handed to `ColonyView.ofWorld`: two sets rather
/// than one, because after #165 the gate has two strengths and not one. A room
/// is withdrawn from the work the colony does, and a room whose withdrawal
/// **latched** on another player's ownership is looked into all the same, once
/// in every `Tuning.RivalRecheck` ticks, so the conclusion that shut it can be
/// contradicted by the only thing that ever could — a tick with vision. One
/// record and not two derivations: the two answers are read off one log and one
/// tick, and split apart they would be free to disagree about which rooms the
/// colony has withdrawn from.
type StandDown =
    {
        /// Every room the gate withholds from the declaration this colony works
        /// (`Outpost.worked`): no Task pools there, no quota counts it and
        /// nothing walks toward it, because the room does not enter the
        /// [[spatial projection]] at all — the whole of "withdraw" in an
        /// architecture that recomputes every tick (ADR 0004, ADR 0043).
        Shut: Set<string>
        /// The rooms of `Shut` this tick takes one look into (#165): a
        /// **subset** of it and never a room leaving it. The look re-admits the
        /// room to the scan — the colony reads its controller, so the next
        /// [[raid log]] can drop a latch the rival has walked away from — and
        /// to nothing else: no furniture, no pooled rock, no Task and no quota
        /// row, which is what keeps ADR 0043's withdrawal in force on the very
        /// tick the gate is being questioned.
        Rechecked: Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StandDown =
    /// The open gate: nothing withheld and nothing to look into — what a colony
    /// with no [[raid log]] yet, and every colony on an ordinary tick, decides
    /// under.
    let none =
        {
            Shut = Set.empty
            Rechecked = Set.empty
        }

/// Where one colony stands in its life (ADR 0052 decision 3). Three answers to
/// one question — how much of its own economy a colony has bought yet — and the
/// one fact five rules read instead of a controller level apiece: whether it
/// places roads and keeps ramparts, whether its sites come before its
/// controller, whether a [[mother colony]] is still raising it.
type ColonyStage =
    /// Claimed, and no spawn of ours standing in it yet: a [[nursery]] — a
    /// colony by declaration and by ownership, and by nothing else it can do
    /// for itself. What ends it is a spawn (ADR 0047 decision 4).
    | Nursery
    /// Its own spawn standing and its controller still under
    /// `Tuning.BootstrapLevel`: running its own `decide`, casting its own
    /// bodies, and still being raised — the **bootstrap window**.
    | Bootstrapping
    /// At `Tuning.BootstrapLevel` or past it: the first tower and the tenth
    /// extension, a bank that casts a body which is not the 300-energy
    /// starter. The stage every rule written for the one home this bot grew
    /// up in was written at (ADR 0052).
    | Independent

/// One colony: a [[home room]] and the [[outpost]]s worked from it (ADR 0047).
/// The unit the whole decision layer is written in — one Atlas, one Layout, one
/// set of quotas, one Task pool — and so the unit a declaration is written in.
type Colony =
    {
        /// The room the colony is run from: the room its spawns stand in,
        /// its Layout is planned in, and its quotas are banked in.
        Home: string
        /// The rooms it mines but does not own. A **candidate colony**'s home
        /// appears here as well, in its *mother* colony's list, until the day
        /// it is independent: one room projected by two colonies at once is
        /// what the mother's outpost declaration already means (ADR 0047).
        Outposts: Outpost list
        /// The home room of the [[mother colony]] that raised this one, for as
        /// long as it is still being raised (ADR 0047 decision 4): the
        /// **bootstrap** window, from the day the child leaves its mother's
        /// outpost list to the tick its controller reaches
        /// `Tuning.BootstrapLevel`. `None` for a colony that was never
        /// anybody's child and for one that has outgrown its mother.
        Mother: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Colony =
    /// The colonies a human has declared (ADR 0047): W12S28 with ADR 0042's
    /// north outpost W12S27, and W13S28, the colony it raised, beside it.
    /// Chosen by a human in an ADR and moved by a human in a commit, exactly as
    /// the Layout's horizon is (ADR 0039). Claiming a second room therefore
    /// begins here and not in the bot: a second entry beside this one is the
    /// whole of "I mean to take that room" (ADR 0047's user story 1).
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
            // outpost until its own spawn stood; that tick it became a living
            // colony and left the mother's list, so one room is projected by
            // one colony.
            {
                Home = "W13S28"
                // W13S29 to the south (2026-09-07): two sources across a
                // twelve-tile Seam, the survey's first pick.
                Outposts = [ Outpost.w13s29 ]
                Mother = Some "W12S28"
            }
        ]

    /// The outposts one home room works: its own declaration's, and none at all
    /// for a room nobody declared. That last answer is the one that matters — a
    /// home the constant does not name projects the room it stands in and
    /// nothing else, so a slip in the constant costs the colony its outposts
    /// rather than putting it in a state nothing has a rule for.
    let outpostsOf (colonies: Colony list) (home: string) : Outpost list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Outposts)
        |> Option.defaultValue []

    /// Every declared colony's home room, in declaration order. What the
    /// shell hands the decision layer (`ColonyView.Declared`): which rooms a
    /// human means to own is not a thing vision can answer, and this is the
    /// half of "candidate colony" that can only be declared.
    let homes (colonies: Colony list) : string list =
        colonies |> List.map (fun colony -> colony.Home)

    /// The **living** colonies: the ones `Main.loop` builds a view for and runs
    /// `decide` once for — those whose home room is ours *and* holds one of our
    /// spawns (ADR 0047 decision 1). Declaration order, so the first entry is
    /// the one a creep no spawn name claims falls to. Two facts and not one,
    /// because each state a declared colony passes through on its way to
    /// running fails exactly one: a [[candidate colony]] owns nothing and
    /// spawns nothing, and a [[nursery]] is owned with no spawn of its own and
    /// is run by its [[mother colony]] (ADR 0047 decision 4). A spawn room no
    /// declaration names is a colony of its own, with no outposts and no
    /// mother, and only when nothing declared is living — the first such room
    /// in the order `spawnRooms` hands them (ADR 0047). Empty when the world
    /// holds no owned spawn room at all.
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
                    // describe is one no human wrote a mother for, and an
                    // invented one would hire pioneers for a constant's slip.
                    Mother = None
                })
            |> Option.toList
        | living -> living

    /// One colony's [[stage]] this tick, off the three facts that decide it
    /// (ADR 0052 decision 3). **The one place `Tuning.BootstrapLevel` is
    /// read**: no rule compares a controller level of its own — each asks for a
    /// stage instead, so the line moves in one field and cannot
    /// drift between its readers. The tunables arrive as an argument rather
    /// than off a constant (decision 5), so a test moves the line by handing
    /// another `Tuning`. `None` for a room that is not a colony at all: not
    /// owned by us is not a stage — a declared home nobody has claimed yet is a
    /// **candidate colony**, whose one rule is the Claim pool — and owned with
    /// no controller level to read is `None` too. Every reader's answer for
    /// `None` is the one it already gives that colony: no rampart kept, no road
    /// placed, nothing bootstrapped.
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

    /// The rooms one colony **bootstraps** this tick (ADR 0047 decision 4): the
    /// homes of the colonies it is the [[mother colony]] of, while those
    /// colonies are not yet `Independent`. The mother projects each of them
    /// beside her own rooms and works two Tasks there — the child's Upgrade and
    /// its Build — which is the one cross-colony borrowing rule there is. The
    /// stages are handed in, derived off the world (`World.stages`), because a
    /// colony's own view cannot answer for a room outside its scan set and this
    /// is the rule that *decides* that set. Three readers take the same answer
    /// and agree by construction; what must not be written twice is the *rule*
    /// (`World.scanOf` is the one place the union is spelled), because a second
    /// rule would be a room projected with nothing pooled in it, or pooled with
    /// nothing projecting it. **Both of the stages before independence**, and
    /// not the bootstrap window alone: a child that has left its mother's
    /// outpost list with no spawn standing is a [[nursery]] again, and the
    /// mother is the only colony that can put the spawn site back up. That is
    /// wider than the level rule it replaces, deliberately, and the price is
    /// the nursery's own paid over a grown room until the spawn stands again. A
    /// room with no stage is not bootstrapped: absence classifies nothing (ADR
    /// 0004), so a room we cannot see or do not own is left to the `Outposts`
    /// list, and a child that stops being ours is `reclaiming`'s.
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

    /// The declared children of this colony that have stopped being ours, and
    /// are nobody else's either: the second half of what a mother projects for
    /// a child, and the one that has nothing to do with raising it. A [[stage]]
    /// is `None` for a room we do not own (ADR 0052 decision 3), so without
    /// this a child whose spawn was destroyed and whose controller was then
    /// lost — to a rival's claim or to the RCL1 downgrade — left every
    /// projection there was: no [[claim]] pooled anywhere, and only a human's
    /// edit could take the room back. **Unowned and never a rival's**: a room
    /// somebody else holds is ADR 0043's business, and a room with no control
    /// entry is one nothing looked into, which classifies nothing (ADR 0004).
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
    /// [[outpost]]s (`Outpost.roomsProjected`), and beside them the rooms it
    /// bootstraps. The whole scan set in one sentence, here and not in the
    /// shell, because the projection is not the set's only reader — the entity
    /// lists the Task pool is built from are swept over it too.
    let roomsProjected
        (outposts: Outpost list)
        (bootstrap: string list)
        (home: string)
        : string list =
        Outpost.roomsProjected outposts home @ bootstrap |> List.distinct

    /// The colony that cast one creep, read off its own name: creep names are
    /// `{pattern}-{tick}-{spawn}`, so the room the named spawn stands in is its
    /// home (ADR 0047 decision 2). None when no known spawn's name is in it — a
    /// creep from an older naming scheme, or one a human made by hand.
    let private castBy (spawnHomes: (string * string) list) (creep: string) : string option =
        spawnHomes
        |> List.filter (fun (spawn, _) -> creep.Contains spawn)
        |> List.sortByDescending (fun (spawn, _) -> (spawn: string).Length)
        |> List.tryHead
        |> Option.map snd

    /// Which colony each creep belongs to this tick (ADR 0047 decision 2),
    /// keyed by creep name: a creep belongs to the colony that **cast** it,
    /// unless it is standing in a room only some *other* colony projects, in
    /// which case that colony **adopts** it for the tick. What a colony's
    /// `ColonyView.Creeps` and its census are cut by, so a creep is one
    /// colony's business and never two's — two colonies matching one creep
    /// would write two Tasks into one flat `assignments` leaf and move it
    /// twice. Adoption answers the creep a colony cannot place: a body outside
    /// every room its own colony projects has no tile there, and the colony
    /// that *does* project the room it stands in can price it, match it and
    /// move it. A fact about this tick, kept nowhere. **Only** another
    /// colony's, and only when exactly one projects it: a room its own colony
    /// projects too is its own colony's business, and a room two others project
    /// names no single adopter. A room *nobody* projects — a [[stand-down]]'s
    /// withheld outpost (ADR 0043) — adopts nobody either.
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
                // answer at all, and falls to `first` beside the unreadable
                // names: a creep filed under a home with no view is nobody's.
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

/// What the decision layer knows about one hostile creep in a room the colony
/// is looking into this tick: its id and tile (what the fire reflex aims at,
/// ADR 0014), its body parts verbatim because what a hostile can do is decided
/// from what it is made of, its owner for the Raid log's roster (ADR 0028), and
/// the room it stands in for that log's closest approach (ADR 0041).
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username ("Invader"
        /// for the NPCs). The Raid log's roster is attribution, and
        /// attribution is a name (ADR 0028); no reflex reads it.
        Owner: string
        /// Where it stands, room and tile in one (ADR 0052 decision 2). A bare
        /// `Pos` carries no room, so the Raid log's closest approach would read
        /// one of ours on the same coordinate of another room as range 0;
        /// `RoomPos.range` answers None across the border instead.
        Pos: RoomPos
        Body: BodyPart list
    }

/// An NPC invader core standing in a room the colony works this tick (ADR
/// 0043). A **structure**, not a creep, so it reaches the projection through
/// neither `Hostiles` nor the fire reflex, whose sweep is
/// `FIND_HOSTILE_CREEPS`. It is the threat an [[outpost]] is stood down from —
/// 100,000 hits, no creeps at level 0, and it never leaves — and the clock the
/// stand-down runs to is read off it while there is still vision. A room the
/// colony cannot see contributes no entry (ADR 0004).
type InvaderCoreInfo =
    {
        /// The room it stands in. The colony works rooms, not tiles, so
        /// this is the whole of where.
        RoomName: string
        /// The **absolute** tick the core's collapse timer runs out at, or None
        /// where it carries none — an expanded level-0 core has no stronghold
        /// to collapse, so the deadline is read off the reservation it took
        /// instead (ADR 0043's fallback order), which is why
        /// `ReservationHolder.Invader` is a case of its own. That is the common
        /// case on the frontier.
        CollapseTick: int option
    }

/// Which deadline an [[outpost]]'s [[stand-down]] runs to — the provenance of
/// the tick, carried beside it because it cannot be recovered from the tick
/// afterwards, and an operator asking why an outpost is shut is asking exactly
/// that. The first three are ADR 0043's own fallback order for a threat, best
/// first; the fourth is not a threat at all and joined them with #165. A closed
/// vocabulary and not a string, for the reason `Ownership` gives, and it crosses
/// the wire, so it is spelt once in `standDownBasisName` and round-tripped
/// against the union itself by `Core.Tests`.
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
    /// stronghold expansion period. The one answer the colony chose rather than
    /// read, and it errs long deliberately: ADR 0043's gate may be wrong only
    /// in the direction that costs an outpost's income.
    | Fallback
    /// The end of the [[reservation]] **another player** holds on the room
    /// (#165). Not a threat with a clock but a room somebody else is working,
    /// and the engine's own countdown says when it stops being one — which is
    /// why this withdrawal is a clock where ADR 0043 wrote a latch: a passing
    /// reserver is a routine event where an owner is not, and a hold that
    /// decays is reversible where ownership is not. It takes no floor under
    /// it, unlike `Reservation` above: the Invader's core re-takes the hold it
    /// lets lapse, so the hold is never the end of the core, while a player's
    /// claimer that stops coming leaves nothing behind it at all.
    | RivalReservation

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Ticks the creep still has to live. A creep still spawning is
        /// outside the projection, so a projected creep always carries a real
        /// count. The fact, not the judgement: whether it is expiring is this
        /// count measured against the lead its replacement needs (ADR 0026).
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
        /// Whether the creep stands on a different tile from last tick — the
        /// shell's reading of Memory's last positions, false for a body born
        /// this tick and in every hand-built fixture. The one reader is the
        /// occupancy surcharge (ADR 0008 as #225 amends it): two bodies each
        /// detouring around the other's last tile never pass.
        Moved: bool
    }

/// What one room holds this tick, to everybody: the half a declaration carries
/// and the half vision pays for, filed under the room's own name and saying
/// nothing about who is looking at it (ADR 0052 decision 1). The unit the
/// `World` is a map of and a [[colony view]] takes its share of — a room two
/// colonies both work contributes one of these and never two. Every list here
/// is *this room's*: the shell scopes what the engine does not scope itself.
/// Absence stays per entry (ADR 0004) and reaches down two levels — a room the
/// world holds nothing for reads `RoomFacts.empty`, and one it holds terrain
/// for but has no vision in reads that terrain beside empty everything else.
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
        /// The kind of each target standing in this room, under the engine's
        /// own id. Id-keyed within the room and merged unlayered into a
        /// view's `SpatialInfo` (ADR 0041): an object id is unique across the
        /// world, so the layer that holds it *is* the room it stands in.
        TargetKinds: Map<string, TargetKind>
        /// Current/max hits of the repairable kinds standing here (ADR
        /// 0010, ADR 0012, ADR 0034).
        Hits: Map<string, HitsInfo>
        /// Energy currently stored, per store standing here: the
        /// containers and the Storage, the piles, the tombstones and the
        /// ruins.
        Stores: Map<string, int>
        /// Who holds the room, and whether its safe mode is running (ADR
        /// 0042) — `None` for a room nothing looked into this tick, which
        /// is not the same fact as a room nobody holds (ADR 0004).
        Control: RoomControlInfo option
        /// The controller of this room **while it is ours**: the fact a
        /// colony's own Upgrade, its downgrade clock and its safe-mode reflex
        /// are read off, and the level a [[stage]] is derived from. `None` for
        /// a room we do not own, whose ownership and reservation are
        /// `Control`'s and are what price a source there (ADR 0042).
        Controller: ControllerInfo option
        /// The room's shared spawn-energy account. Zero for a room with no
        /// spawn or extension in it, which is every room we do not own —
        /// the engine's own answer, and the one a colony reads as an empty
        /// bank.
        Energy: RoomEnergy
        /// Our spawns standing in this room. The world's, so a spawn standing
        /// in a [[nursery]] a mother is raising is a fact about that room and
        /// not about her — which is exactly what ends the nursery.
        Spawns: SpawnInfo list
        /// The bodies still gestating in this room's spawns: energy the colony
        /// has **already spent** on a creep that is not alive yet. Bodies and
        /// not creeps, and filed under the room rather than under the spawn
        /// building them, because a colony banks in one room (ADR 0052 decision
        /// 1) and every row's gap is a colony number. Empty wherever no spawn
        /// of ours is mid-cast, and empty is the whole of "nothing is being
        /// cast here".
        Casting: BodyPart list list
        /// Our energy-hungry structures standing here (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources in this room, as vision answered for them. What a
        /// *declaration* answers for is laid over a colony's projection
        /// instead (`Outpost.place`, `Outpost.pooledSources`), because which
        /// rooms are worked is a colony's question and not the world's.
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

/// One creep of ours, in the world's reading: what it is made of and where it
/// stands, before any colony has claimed it (ADR 0052 decision 1).
type WorldCreep =
    {
        /// The room the engine says the creep stands in — its own answer, so
        /// a creep in a room no colony works still carries the name of the
        /// room it is in (`World.creepColonies` decides whose it then is).
        Room: string
        /// What the decision layer knows about the body itself.
        Info: CreepInfo
    }

/// What the world last saw standing in one room, and when (#151): the tick
/// vision last answered for that room, and the bare ids it answered with. The
/// one thing carried **across** ticks about a room, and it is carried for one
/// question only. ADR 0004's absence is per-entry and per-tick, and it is the
/// right answer for everything a rule prices — geometry that cannot be priced
/// counts against no Task and blocks no action — but it cannot say *why* an id
/// left the pool, and the two answers are opposite work: a container destroyed
/// is a Task gone, a room gone dark is a Task waiting. So this is read by the
/// vision grace and by nothing else, and what the grace hands on is a **room
/// name**: the Matcher keeps the assignment and the mover walks its holder at
/// that room's Seam, which is border layer and terrain and needs no vision.
/// Nothing is placed or priced off a sighting, and no stale fact reaches a
/// decision through it.
type RoomSighting =
    {
        /// The tick vision last answered for the room. Equal to the world's
        /// own `Time` for a room seen this tick, which is how a reader tells
        /// a room it can see from one it is remembering.
        Tick: int
        /// The ids that stood in the room that tick, and nothing about them:
        /// the keys of its `RoomFacts.TargetKinds`, with the kinds dropped on
        /// the way in. Set membership is the only question the grace asks, so
        /// carrying the kinds would be a field no decision reads, which is the
        /// growth ADR 0007's rule refuses — and it is the narrowing that puts
        /// the promise above into the *type*: there is no stale kind here for
        /// the next rule to read one out of.
        Targets: Set<string>
    }

/// Everything this tick was seen to hold, once (ADR 0052 decision 1). The shell
/// builds one (`World.ofGame`, the only code that touches `Game`) and
/// `ColonyView.ofWorld` cuts one colony's share of it; `decide` is written
/// against a view and never against the world, so no rule can reach a room its
/// colony does not work.
type World =
    {
        Time: int
        /// Every room the world holds anything for this tick, under its own
        /// name: the declared rooms, whose terrain and furniture need no vision
        /// (ADR 0041), and every room the engine answered `Game.rooms` with. A
        /// room absent here reads `RoomFacts.empty` (ADR 0004).
        Rooms: Map<string, RoomFacts>
        /// Every creep we own that is not still gestating, in the engine's
        /// own order. Whose each one is this tick is `World.creepColonies`'
        /// answer and is not stored here: the rule needs the [[stand-down]]
        /// gate, read out of Memory rather than off the world (ADR 0043).
        Creeps: WorldCreep list
        /// What each room was last seen to carry, under its own name (#151):
        /// this tick's census for every room vision answered for, and the last
        /// one taken for a room it did not. Heap state in the shell — beside
        /// the plan memos and the terrain memo, and deliberately not a Memory
        /// leaf — so a global reset empties it and the colony decides exactly
        /// as it did before the grace existed. A room never seen has no entry:
        /// the absence is per-entry here too (ADR 0004).
        Sightings: Map<string, RoomSighting>
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
            Sightings = Map.empty
        }

    /// This tick's world with what it saw **before** laid under it (#151):
    /// every room vision answered for this tick keeps this tick's sighting,
    /// and a room it did not answer for keeps the last one taken. The shell
    /// carries the previous map on the heap and hands it back here, so the
    /// merge is one pure function a test can drive rather than a line buried
    /// in the only code that reads `Game`. This tick wins every entry it
    /// holds, exactly as vision wins over a declaration (ADR 0041): a
    /// remembered census is what is left where there was nothing to read.
    /// What is remembered is bounded by the rooms this tick's world holds —
    /// the declared ones and the seen ones — so a room that leaves the world
    /// leaves the memory with it rather than riding a global's whole life in
    /// a map nobody can ask about: every reader of a sighting narrows it to a
    /// colony's scan set, and a scan set is drawn from these rooms.
    let recalling (previous: Map<string, RoomSighting>) (world: World) : World =
        { world with
            Sightings =
                (previous |> Map.filter (fun room _ -> Map.containsKey room world.Rooms),
                 world.Sightings)
                ||> Map.fold (fun carried room sighting -> Map.add room sighting carried)
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

    /// The rooms one of our spawns stands in, in room-name order. The other
    /// fact `Colony.living` asks for, and the one a [[stage]] reads as "this
    /// colony can cast for itself".
    let spawnRooms (world: World) : string list =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) -> not (List.isEmpty facts.Spawns))
        |> List.map fst

    /// The [[stage]] of every declared colony that is one this tick (ADR 0052
    /// decision 3), derived off the world because a stage decides whether a
    /// mother scans her child's room at all (`Colony.bootstrapping`) — it
    /// cannot be read off a projection the scan set does not exist yet. Asked
    /// of the **declared** homes, which is what the raising rule asks about,
    /// and of every **living** colony's home, because a spawn room no
    /// declaration names is a colony of its own. Not every owned room the world
    /// holds: swept wider, a room we claimed by hand and never declared would
    /// arrive in some colony's scan set as a [[nursery]] to raise.
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

    /// The colonies that run this tick (`Colony.living`, ADR 0047 decision 1),
    /// read off the world's two facts.
    and living (colonies: Colony list) (world: World) : Colony list =
        Colony.living (ownedRooms world) (spawnRooms world) colonies

    /// The declaration's narrowings and the union they make, for one colony:
    /// the [[outpost]]s the [[stand-down]] gate leaves it (ADR 0043) and its
    /// home shares a border with (`Outpost.neighbouring`, #243), the rooms it is
    /// bootstrapping for a child of its own (ADR 0047 decision 4), and its scan
    /// set — its home and both of those. The two outpost narrowings are one
    /// clause apiece and answer different questions: the gate is this tick's
    /// and reopens, the border is the declaration's and never does — so a
    /// refused room leaves the scan set for good, taking its furniture, its
    /// pooled rock, its Reserve and the reserver the row would have hired for
    /// it (ADR 0042) with it, which is the whole of "refuse it loudly" that a
    /// scan set can carry. What says so out loud is `ColonyView.Refused`.
    let scanOf
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        (shut: Set<string>)
        (colony: Colony)
        : Outpost list * string list * string list =
        let outposts =
            Outpost.worked shut colony.Outposts
            |> List.filter (Outpost.neighbouring colony.Home)

        // The two halves of what a mother projects for a child of hers, and
        // they are disjoint by construction: a room she is raising is one we
        // own, and a room she may take back is one we do not (#221).
        let borrowed =
            Colony.bootstrapping stages colonies colony
            @ Colony.reclaiming unowned colonies colony

        outposts, borrowed, Colony.roomsProjected outposts borrowed colony.Home

    /// The declared homes that stand empty this tick: ours to take back if
    /// they ever were ours, and the candidates a human means to take. Read off
    /// the same control entry ownership is read off everywhere (ADR 0042); a
    /// room nothing looked into answers no, which is ADR 0004's absence and not
    /// a claim that somebody holds it.
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
    /// 0047 decision 2), decided over every living colony's scan set at once
    /// and handed to each view: a creep is one colony's business, or two
    /// decisions would move one body twice.
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
/// written down here, and everything else the room holds stays the child's.
type BorrowedWork =
    {
        /// The home rooms of the children this colony carries in its projection
        /// for a reason that is not mining them, and it is two reasons: the
        /// children it is **raising**, whose Upgrade and Build its bodies may
        /// cross for (ADR 0047 decision 4), and the children it has **lost**,
        /// whose controller is a [[claim]] to make. The two are disjoint by
        /// construction and narrow to the same three kinds, because a Claim
        /// asks for exactly what an Upgrade does. The view carries only those
        /// kinds for these rooms, so the mother pools no Harvest on the child's
        /// rock and hauls none of its energy home.
        Rooms: string list
    }

/// One colony's whole reading of this tick: its home room's projection, the
/// rooms it works beside it, the bodies it holds, the bank it casts from and
/// the explicit little it may take of its neighbours' (ADR 0052 decision 1).
type ColonyView =
    {
        Time: int
        /// This colony's spawns: the ones it casts from and anchors its
        /// Layout on. A spawn standing in another colony's home is that
        /// colony's, and whether one stands in a declared home reaches this
        /// colony as that room's [[stage]].
        Spawns: SpawnInfo list
        /// The bodies this colony has in its ovens this tick: its **home
        /// room's** `RoomFacts.Casting` alone, for the reason `Bank` is one
        /// account — a colony casts from the spawns of the room it banks in.
        /// Read by the casting cascade and by nothing else: a body in an oven
        /// stands on no tile, holds no Task and answers no Verdict (ADR 0026).
        Casting: BodyPart list list
        /// The **tunables** this colony decides under (ADR 0052 decision 5),
        /// arriving on the view like every other fact so that a rule reads its
        /// colony's own and a test moves one field instead of editing the rule.
        Tuning: Tuning
        /// The colony's bank: its **home room's** shared spawn-energy account,
        /// and no other room's (ADR 0052 decision 1). Every spawn it casts from
        /// stands in that room, so one account is the whole of what it can
        /// spend — and not a fold over the projected rooms, which would have
        /// read a child's 300 beside a mother's 1,800.
        Bank: RoomEnergy
        /// Energy-hungry structures in the home room (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources this colony **mines**: every room it works but the ones
        /// it merely [[bootstrap]]s, whose rocks are the child's own (ADR 0047
        /// decision 4), with every declared outpost rock beside them whether or
        /// not there is vision (`Outpost.pooledSources`, ADR 0041).
        Sources: SourceInfo list
        /// This colony's own controller — the one it upgrades, whose downgrade
        /// clock it runs against and whose safe mode it fires (ADR 0047
        /// decision 1). Never a child's, which reaches the pool as a target in
        /// a layer she projects. `None` where the projection cannot place it,
        /// which is ADR 0004's absence and not a state.
        Controller: ControllerInfo option
        /// Who holds each room this colony works and has vision in this tick,
        /// under that room's name — what a source's output per tick is priced
        /// from (ADR 0042), and the fact a rule reads to say whether a room is
        /// this colony's business at all. Absent for a room vision did not
        /// answer for, per-entry as every other absence is (ADR 0004). One
        /// entry can be a room the colony does **not** work: the [[stand-down]]
        /// gate re-admits a room it has latched to the scan for one tick in
        /// every `Tuning.RivalRecheck` (#165), and this is the whole of what
        /// such a look reads. Nothing else of that room is here — no layer, no
        /// rock, no Task — so every reader below finds it nowhere, which is why
        /// the look moves no decision and only the next [[raid log]] is any
        /// wiser for it.
        RoomControl: Map<string, RoomControlInfo>
        /// Our construction sites in every room this colony works and has
        /// vision in: the Build pool is this list one to one, so an outpost's
        /// site is a Task like the home room's, and a bootstrapped child's site
        /// is the second half of what a [[pioneer]] crosses for.
        ConstructionSites: ConstructionSiteInfo list
        /// The creeps this colony holds this tick: the ones it cast, plus the
        /// ones it has adopted, less the ones another colony has adopted from
        /// it (`World.creepColonies`, ADR 0047 decision 2). In the world's own
        /// order, so who holds a body does not move the Matcher's order.
        Creeps: CreepInfo list
        /// Hostile creeps standing in any room this colony works and has
        /// vision in, each under its own room's name (ADR 0033, #201).
        Hostiles: HostileInfo list
        /// The invader cores standing in the rooms this colony works and can
        /// see (ADR 0043). Its own list and not a widening of `Hostiles`: a
        /// raider is something a creep runs from this tick, a core is something
        /// a whole room is withheld from for thousands, and
        /// `FIND_HOSTILE_CREEPS` can never answer with a structure.
        InvaderCores: InvaderCoreInfo list
        /// This colony's spatial projection: the home room and every room it
        /// works beside it, in one projection (ADR 0041, ADR 0005). `RoomName`
        /// is the home room and the `Rooms` keys are the scan set, so the
        /// view's home and the rooms it works are read off the projection
        /// rather than stored a second time beside it. Always present, possibly
        /// empty — absence is per-entry, never per-projection (ADR 0004).
        Spatial: SpatialInfo
        /// Every home room a human has declared a colony for (`Colony.homes`,
        /// ADR 0047), this colony's own included and in declaration order. The
        /// **candidate colonies** are the ones nobody owns yet, and that second
        /// half is read off `RoomControl` in Core: which rooms a human means to
        /// own is declared, whether we own one is seen, and a view carries
        /// facts rather than conclusions.
        Declared: string list
        /// The [[stage]] of every room that is a colony of ours this tick
        /// (`World.stages`, ADR 0052 decision 3) — this colony's own and its
        /// children's alike, the same map handed to every colony because a
        /// stage is a fact about a room and not about who is looking.
        Stages: Map<string, ColonyStage>
        /// Where **other colonies'** creeps stand in the rooms this colony
        /// works, each tile carrying its room (ADR 0052 decisions 1 and 2): the
        /// bodies this colony does not hold and cannot move — in no `Creeps`
        /// list of hers, on no tile of her layers, and in nobody's Task pool
        /// but their own colony's.
        Foreign: Set<RoomPos>
        /// What this colony may take of a neighbour's, explicitly and
        /// bounded (ADR 0052 decision 7): today the Upgrade and the Build
        /// of a child it is still raising (ADR 0047 decision 4).
        Borrowed: BorrowedWork
        /// The [[outpost]]s this colony's declaration names that its home
        /// shares no border with, and that it therefore **refuses**
        /// (`Outpost.refused`, #243): no [[seam]] joins them, so nothing in
        /// them can be priced, walked to or worked, and they are out of the
        /// scan set rather than in it unworkable. Carried on the view because
        /// the refusal has to be *said*: it is the colony's own reading of its
        /// declaration, it reaches the operator on the [[layout record]] beside
        /// the plan's other losses, and the silence it replaces is what #243
        /// was filed for. Empty is the healthy answer and rides here all the
        /// same, as the Layout's own loss lists do (ADR 0035).
        Refused: string list
        /// What each room this colony **works** was last seen to carry (#151):
        /// the world's sightings, narrowed to the scan set. The narrowing is
        /// the rule and not housekeeping — a room a [[stand-down]] withholds
        /// leaves the scan set (ADR 0043) and leaves this map with it, so the
        /// withdrawal that ADR spells through `task-gone` keeps working
        /// unchanged. A withheld room is one the colony stops holding
        /// assignments in; a dark one is a room it is still working and
        /// cannot see this tick.
        Sightings: Map<string, RoomSighting>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColonyView =
    /// What a colony may see of a room it carries for a child of its own: the
    /// controller its workers upgrade — or, where the child has been lost,
    /// [[claim]] — the sites they build, and the spawn, not a target of hers at
    /// all but the tile her [[pioneer]]s walk up to and the structure that says
    /// a colony lives here (ADR 0047 decision 4). Both halves of
    /// `BorrowedWork.Rooms` narrow through this one filter: a Claim asks for
    /// exactly what an Upgrade does. Beside the kinds, at most one store: the
    /// [[ferry]]'s sink.
    let private borrowable (kind: TargetKind) =
        match kind with
        | Controller
        | Site _
        | Structure BuiltKind.Spawn -> true
        | Source
        | Dropped
        | Tombstone
        | Structure _ -> false

    /// One bootstrapped room's facts, cut down to the borrowed work (ADR 0052
    /// decision 7). Taken off the whole facts rather than gated when the world
    /// is read: the world reads a room once for everybody, so what this chooses
    /// is not what to *read* but what this colony may **carry**. What is left
    /// is exactly ADR 0004's per-entry absence — the shape a room with no
    /// vision arrives in — so every rule downstream already answers correctly
    /// for it. The geometry is kept whole, being what the mother's workers walk
    /// over. The room's hits go, so no Repair of the child's reaches her pool,
    /// and its sources go with them. Of its stores at most one survives: **the
    /// child's upgrade buffer, and no other store of its** — a built container
    /// inside the child's own controller's Upgrade area and on none of its
    /// Seats. That is what a [[ferry]] fills, so the mother has to see how much
    /// room is left in it; a source container of the child's carried here would
    /// be a Withdraw in her pool and the child's income hauled across the Seam.
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

    /// One colony's view of this tick (ADR 0052 decision 1): the rooms it works
    /// cut out of the `World`, the bodies it holds cut out of the world's
    /// creeps, its own bank and controller, and the explicit little it may take
    /// of a child's. **Pure, and that is the point of it** (ADR 0052 decision
    /// 8): the shell reads the engine once (`World.ofGame`) and every rule
    /// about which rooms a colony works, which creeps are its own and what it
    /// may borrow is here, where a test can hand it a two-colony world and read
    /// the answer back. Five facts are handed in and none is decided here: the
    /// **tunables** (decision 5), the **declaration**, the **gate** the
    /// [[stand-down]] derives off the previous tick's [[raid log]] (ADR 0043 —
    /// Memory's answer, not the world's), the **holders** `World.creepColonies`
    /// cut over every living colony's scan set at once, and the **world**
    /// itself.
    let ofWorld
        (tuning: Tuning)
        (colonies: Colony list)
        (gate: StandDown)
        (holders: Map<string, string>)
        (world: World)
        (colony: Colony)
        : ColonyView =
        let home = colony.Home
        let stages = World.stages tuning colonies world

        // The declaration's narrowings and their union, off the one
        // derivation the creep adoption reads too (`World.scanOf`). Written
        // here a second time it would be a second answer free to disagree.
        let outposts, bootstrap, scanned =
            World.scanOf stages (World.unownedHomes colonies world) colonies gate.Shut colony

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
        // because an object id is already unique across the world (ADR 0041).
        // Deterministic under a collision that cannot happen: the fold walks
        // the scan set in order, and one object stands in one room.
        let mergedBy (select: RoomFacts -> Map<string, 'v>) =
            (Map.empty, worked)
            ||> List.fold (fun acc (_, facts) ->
                (acc, select facts) ||> Map.fold (fun acc id value -> Map.add id value acc))

        let homeFacts = World.roomOf world home

        // The scan set's own control entries, and beside them the one look
        // #165 buys a room the gate has latched on another player's ownership:
        // whatever vision answered for that room this tick, and nothing else it
        // holds. This is the whole of "re-admitted to the scan set only" — the
        // room contributes no furniture, no rock, no hostile and no layer, so
        // nothing pools there and no quota counts it while the look happens,
        // and ADR 0043's withdrawal stands through the tick that questions it.
        // Read off the **declaration** and never off the latch's own room
        // names: a hand-edited `rivalHeld` leaf is a room name a human wrote,
        // and the only rooms this colony may look into are the ones it
        // declared. A room vision did not answer for adds no entry at all,
        // which is ADR 0004's absence and the reason the latch survives every
        // recheck the colony is blind on.
        let control =
            (worked
             |> List.choose (fun (room, facts) ->
                 facts.Control |> Option.map (fun control -> room, control))
             |> Map.ofList,
             colony.Outposts
             |> List.filter (fun outpost -> Set.contains outpost.RoomName gate.Rechecked))
            ||> List.fold (fun control outpost ->
                match (World.roomOf world outpost.RoomName).Control with
                | Some seen -> Map.add outpost.RoomName seen control
                | None -> control)

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
            RoomControl = control
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
            // tile joined to the room it stands in (ADR 0052 decision 2): a
            // room with none contributes nothing (ADR 0004), an empty set.
            Foreign =
                worked
                |> List.collect (fun (room, facts) ->
                    facts.Layer.CreepPositions
                    |> Map.toList
                    |> List.filter (fun (name, _) -> not (Set.contains name names))
                    |> List.map (snd >> RoomPos.at room))
                |> Set.ofList
            Borrowed = { Rooms = bootstrap }
            // Read off the whole declaration and not off `scanned`, which is
            // where these rooms have just been subtracted: what the channel
            // must name is the room a human declared and this colony cannot
            // work, and by the time the scan set is cut the name is gone
            // (#243).
            Refused = Outpost.refused home colony.Outposts
            // The world's memory of these rooms and of no others (#151):
            // narrowed by the scan set the [[stand-down]] gate has already
            // cut, so a withheld room's remembered census cannot hold a
            // creep to a Task in a room the colony has withdrawn from.
            Sightings = world.Sightings |> Map.filter (fun room _ -> List.contains room scanned)
        }

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    /// Take stored energy out of a stocked container (ADR 0012), or out of the
    /// Storage a tier below them (ADR 0023) — the haul cycle's intake, judged
    /// over stores rather than energy's name.
    | Withdraw of storeId: string
    /// Walk to a dropped energy pile and take it. The Task half of what the
    /// [[pickup reflex]] does by hand: the reflex takes what is already within
    /// range 1 of a creep standing there for its own reasons, and this is what
    /// sends a creep to a pile no reflex will ever reach. Pooled on the pile's
    /// amount alone and only from a threshold (`Tuning.PickupThreshold`).
    /// Feeding tier and hauler-shaped, the same as the Withdraw beside it:
    /// which of the two an empty carrier goes for is travel cost's call.
    | Pickup of pileId: string
    /// Deliver energy into an energy-hungry structure (ADR 0010, widened by ADR
    /// 0012 and ADR 0023): a tower, the upgrade [[buffer]], the [[storage]], a
    /// [[ferry]]'s sink — and the flow's own sink, which since ADR 0054 is not
    /// a structure but a **place**: the id is the [[refill cluster]]'s spawn,
    /// and that spawn and every extension of the colony are one Task with one
    /// [[capacity]].
    | Refill of structureId: string
    | Build of siteId: string
    | Repair of structureId: string
    | Upgrade of controllerId: string
    /// Holding a neutral controller with CLAIM parts (ADR 0042): a reservation
    /// is what makes that room's sources worth the held ten a tick rather than
    /// the neutral five, and it decays by one a tick, so this is work that is
    /// never finished. One per projected controller that is not the colony's
    /// own.
    | Reserve of controllerId: string
    /// Taking a **candidate colony**'s controller for our own with CLAIM parts
    /// (ADR 0047): the act that turns a declared home room into an owned one,
    /// and so the first tick of a second colony. One per candidate colony — a
    /// declared home this colony does not own yet — and never for a plain
    /// [[outpost]], whose controller is [[reserve]]d instead: claiming costs a
    /// GCL level and asks the colony to run the room, which is a human's
    /// decision written in `Colony.declared`.
    | Claim of controllerId: string
    /// Getting out of a Threat's Reach (ADR 0033). The one Task with no
    /// target and no action: its Work Area is the tiles no Threat can
    /// hurt, and the Emitter issues movement for it and nothing else.
    | Flee
    /// Killing what stands in a declared [[outpost]] (ADR 0056): one Task per
    /// outpost a [[threat]] stands in, keyed on the **room** and never on the
    /// hostile, which is ADR 0054's split applied where it was learnt — the
    /// Planner names a place and the [[emitter]] names the target at arrival.
    /// Keyed on the hostile, the 2% multi-creep raid would pool five Tasks and
    /// re-match the [[guard]] between them every time one moved. Its Work Area
    /// is the walkable range-1 ring of every Threat standing in that room — a
    /// colony fact derived off `Threats` as [[flee]]'s safe set is, and no
    /// target's surroundings — so it is the second Task the projection places
    /// nothing for, and the one that acts anyway.
    | Guard of roomName: string

/// The five shapes a body takes as far as a [[capacity]] is concerned (ADR
/// 0052 decision 6) — part arithmetic and never a row's name (ADR 0006), so the
/// classes are the ones the existing gates already cut the fleet along.
type BodyClass =
    /// An ATTACK part (ADR 0056): the guard row's shape, and **first** in
    /// the ladder because it is the one cut no other class makes. A guard
    /// carries no Work and no Carry, so read through the four classes
    /// below it would fall into `Carrier` beside the [[hauler unit]]s and
    /// the [[reserver]] — a class whose whole meaning is "nothing a
    /// Work-shaped capacity is dividing for" — and the one [[capacity]]
    /// written for a guard would be answering for them too.
    | Fighter
    /// More Work than Move (ADR 0016): the garrison's shape. Its intake is
    /// digging and its work is a [[post]], so it is the class every cap
    /// that is about standing room on a tile is written for.
    | Heavy
    /// Fewer than one Carry per `Tuning.StandingCarryPerWork` Work and not
    /// Heavy (ADR 0046): the [[upgrader]] row, which lives beside the
    /// [[buffer]] and carries one trip's worth.
    | Standing
    /// No Work part at all: the [[hauler unit]]'s shape, and the
    /// [[reserver]]'s beside it — neither can spend anything at a
    /// controller or into a site, so neither is ever what a Work-shaped
    /// capacity is dividing for.
    | Carrier
    /// Everything else — the [[worker unit]], the generalist the colony's
    /// surplus work is done by.
    | Light

/// How many creeps a pooled Task admits at once, set by the Planner and counted
/// by the Matcher (ADR 0052 decision 6). The Matcher knows no Task kinds: every
/// seat rule the colony has — a source's [[seat]]s, a [[post]]'s standing room,
/// a store's stock over its drawers' load, the outpost container builders'
/// budget, one holder per controller, the [[pioneer]]s' ceiling on borrowed
/// work — arrives here as numbers and tiles, and `hasCapacity` counts holders
/// against them.
type Capacity =
    {
        /// Holders of every class together. `None` is unbounded — the *deeper*
        /// Refills (the [[buffer]]'s, the [[storage]]'s, a [[ferry]]'s sink)
        /// and the surplus work the pool is mostly made of. The flow's own
        /// Refill left that set in ADR 0054: the [[refill cluster]] carries a
        /// number here, its free energy over one [[hauler unit]] load.
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
        /// Holders that are `Fighter`, **and no holder of any other class at
        /// all**: the [[guard]]'s share of a Guard, which ADR 0056 states as
        /// "`Fighter -> the room's quota`, every other class 0". Both halves
        /// ride one field because the four scopes above cannot spell "not a
        /// Fighter" between them — `Commuters` and `Generalists` each contain
        /// the class — and a zero written as two of them would be a number
        /// about somebody else's crowd (the `None` scope) rather than a
        /// refusal. The one cap written for a class rather than for a crowd,
        /// and the second lock on a Task whose applicability already asks for
        /// an ATTACK part: the Matcher recognises no Task kinds (ADR 0052
        /// decision 6), so what keeps a [[hauler unit]] out of a fight has to
        /// be sayable in numbers.
        Fighters: int option
        /// Tiles whose standing **heavy** occupant holds a slot against
        /// `Garrisons` whatever Task it holds this tick — **every** [[post]] of
        /// the rock since #269, where #205 carried only the Posts whose
        /// container was still a site. Standing room is a fact about where a
        /// body is (ADR 0024), so the tile is taken while a heavy body stands
        /// on it and free only when none does: a cap counting the Task's
        /// holders alone reads the tile as free on every tick its occupant
        /// happens to hold something else — a build tick on a site Post, an
        /// Upgrade through a drained rock's window on a bare [[dual seat]].
        /// **Unioned** with those holders and never added to them, one body
        /// that both holds the Task and stands on its Post being one garrison
        /// and not two. Counted against that one cap and not against `Total`.
        /// Empty for every other Task.
        Garrison: Set<RoomPos>
        /// Tiles a candidate standing on is outside every cap above: the
        /// container site under a garrison's own feet, which the outpost
        /// builders' budget does not price because that budget prices a
        /// commute and this body made none. Empty for every other Task.
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
            Fighters = None
            Garrison = Set.empty
            Exempt = Set.empty
        }

    /// One number over every class: a Seat count, a store's stock divided
    /// by one load, one holder per controller.
    let total n = { unbounded with Total = Some n }

    /// The Guard's cap (ADR 0056): that room's quota of `Fighter` bodies,
    /// and nobody else at all.
    let fighters n = { unbounded with Fighters = Some n }

    /// Whether any cap at all is set — the question that decides whether
    /// the Matcher pays for a walk over the holders (ADR 0029).
    let isBounded (capacity: Capacity) =
        capacity.Total.IsSome
        || capacity.Garrisons.IsSome
        || capacity.Commuters.IsSome
        || capacity.Standing.IsSome
        || capacity.Generalists.IsSome
        || capacity.Fighters.IsSome

/// One entry of this tick's Task pool: the Task, where it ranks and how many
/// bodies it admits (ADR 0052 decision 6).
type PooledTask =
    {
        Task: Task
        /// Where this Task ranks against every other, lower first — the tier
        /// ladder plus the [[downgrade deadline]]'s one lift above it (ADR
        /// 0007). The Matcher's first key component; `MatchFactor.Rank` names
        /// it.
        Priority: int
        Capacity: Capacity
        /// Whether this is work in a room another colony of ours runs — the
        /// borrowed Upgrade a [[mother colony]] pools for her [[pioneer]]s, and
        /// the child's [[build]]s beside it (ADR 0047 decision 4). Read by one
        /// body gate: a [[standing body]] holds no commuting work, and a Seam
        /// crossing is the longest commute the colony has.
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

/// Every BodyPart — the closed set, for building tables over the vocabulary.
/// A literal, and so not compiler-checked: a part added to the union has to be
/// added here by hand. What closes it is `Core.Tests`, which enumerates the
/// union and fails when this list is short.
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

/// Every BuiltKind the engine spells — the modelled set, not the engine's whole
/// structure vocabulary. Every spelling outside it classifies to Other, which is
/// why Other is not one of them: it is the absence of a modelled kind. A
/// literal, closed by `Core.Tests` the same way `allBodyParts` is.
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
/// — the one place the spelling lives (its reverse is derived from this table).
/// Other spells to nothing: it is the absence of a modelled kind, so it stays
/// out of `allBuiltKinds` and the empty string never reaches the engine.
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

/// The built kind a placement Intent's kind names: the one crossing between
/// the Intent vocabulary and the projection's, stated in Core beside both
/// unions rather than respelled wherever the two meet. Every placeable kind is
/// a built kind; the reverse does not hold — a Link is projected but never
/// placed (ADR 0022) — so the crossing runs this way only.
let builtKindOfPlaceable =
    function
    | Extension -> BuiltKind.Extension
    | Tower -> BuiltKind.Tower
    | Road -> BuiltKind.Road
    | Container -> BuiltKind.Container
    | Storage -> BuiltKind.Storage
    | Rampart -> BuiltKind.Rampart

/// The kinds Refill keeps fed (ADR 0010): the spawn-energy feeders and the
/// towers, the structures a view projects as Refillables. The controller
/// container and the Storage are Refill targets too, but the Planner pools them
/// off the projection's stores (ADR 0012, ADR 0023).
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

/// The Keep (ADR 0034): the structures worth defending — the spawn, the tower
/// and the Storage. One list, three rules hang off it: a rampart covers each of
/// them, Repair keeps each at full hits, and any one of them below full while a
/// hostile stands in the room fires the safe-mode reflex.
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
/// ramparts that cover it. Not the roads and the containers, whose hits the
/// projection also carries — a chewed road is the colony's ordinary decay, and
/// charging it would drown the number the Raid log exists for.
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
/// ownable kind whose hits a decision reads (ADR 0034). A structure of another
/// owner left standing in a room we took is neither ours to repair nor ours to
/// charge a raid's damage on.
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

/// The line a kind is whole at, or None for a kind Repair never touches — the
/// extensions, a link, and every kind the decision layer does not model (ADR
/// 0010, widened by ADR 0034). The repairable kinds are exactly the kinds whose
/// hits the projection carries at all: fields nobody decides on stay out.
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
    /// [[guard]] heals itself every tick it holds its Task, which is what the
    /// row's one HEAL part is for. Both creeps are named, and both by **name**
    /// — the target is one of ours, and an Intent whose target rode implicitly
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

/// The reverse of a wire-name table, derived from the table itself: each
/// spelling is written once, in the name table, and the decoder reads back what
/// falls out of it. A name the vocabulary does not have reads as None — the
/// caller decides what a miss costs.
let reverseOf toName cases =
    let byName = cases |> List.map (fun case -> toName case, case) |> Map.ofList
    fun name -> Map.tryFind name byName

/// The same reversal for a vocabulary whose cases carry numbers beside their
/// name. The entries are the cases' own constructors rather than the cases, so
/// each spelling is still written once, and the numbers the wire carried are
/// handed back in on the way out. A name whose numbers are missing decodes to
/// None exactly as an unknown name does.
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

/// Why a remembered assignment was released: its Task left the pool, a Threat's
/// Reach has taken the whole of its Work Area (ADR 0033) — asked first, because
/// a Task with nowhere to stand is gone for this creep however well its body
/// fits — the creep can no longer usefully work it, the Task's worker cap was
/// already full, its Work Area is unreachable or empty (ADR 0002), or its time
/// has not come: the creep's walk no longer covers a drained source's restock
/// wait (ADR 0025).
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

/// Why an unassigned creep got nothing: the pool was empty, no Task fit its
/// body or energy state, every fitting Task's worker cap was full, every
/// fitting Task with room had an unreachable Work Area, or every Task it could
/// otherwise have taken is one whose time has not come (ADR 0025).
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

/// Why a Task in the pool was rejected for a creep, in a verbose scoring: a
/// Threat's Reach has taken the whole of its Work Area (ADR 0033), it did not
/// fit the creep's body or energy state, its worker cap was already full, its
/// Work Area is unreachable, or its time has not come — the matching gates, in
/// the order they are tried.
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

/// The wire spelling of each FootingKind, on the Layout channel's Memory leaf.
/// Not a Verdict vocabulary — the Layout speaks no Verdicts, which is the whole
/// reason its losses need a channel — but the same rule: one spelling, written
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

/// The wire spelling of each TrunkGoal, on the Layout channel's Memory leaf
/// beside `footingKindName`. A carrying vocabulary, like the two reason
/// vocabularies: the spawn's id rides beside the name rather than inside it, so
/// a goal is one spelling and not one per spawn.
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

/// The TrunkGoal a wire name spells for the spawn the wire carried beside it,
/// or None for a name this vocabulary does not have — and for `spawn` with no
/// id carried beside it at all, which is a row that lost its spawn rather than
/// a goal. An id that is carried but empty is a spawn like any other here: the
/// vocabulary spells names, and what counts as a usable id is the caller's.
let trunkGoalOf =
    reverseCarrying
        trunkGoalName
        ""
        [ (fun _ -> Some TrunkGoal.UpgradeArea); Option.map TrunkGoal.Spawn ]

/// The wire spelling of each ContainerTarget, on the Layout channel's Memory
/// leaf beside `trunkGoalName` (ADR 0040). A carrying vocabulary like it, and
/// for the same reason: the source's id rides beside the name rather than
/// inside it, so a target is one spelling and not one per source.
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

/// The wire spelling of each StandDownBasis, on the Raid log's Memory leaf
/// (ADR 0043), as `footingKindName` is the Layout channel's, and under the same
/// rule: one spelling, written once here, reversed by the table below and
/// round-tripped against the union itself by `Core.Tests`, so a fifth basis
/// added without a name is a red test rather than a stand-down that decodes to
/// nothing.
let standDownBasisName =
    function
    | StandDownBasis.CollapseTimer -> "collapse-timer"
    | StandDownBasis.Reservation -> "reservation"
    | StandDownBasis.Fallback -> "fallback"
    | StandDownBasis.RivalReservation -> "rival-reservation"

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
            StandDownBasis.RivalReservation
        ]

/// A creep's Move Intent: candidate standing tiles for next tick in preference
/// order, plus a priority (the task rank). Input to the Resolver — not an
/// Intent; the Resolver's output is what becomes one. Standing tiles but one: a
/// creep walked at a Seam is given the exit tile itself as its last step, and
/// that is a destination rather than a place to stand — the engine moves a
/// creep off a border tile at the end of the tick — which is why no Seat, Work
/// Area or standing candidate query will ever name one. It is a tile of this
/// room, so the arbitration settles it exactly as it settles ground. It carries
/// the room it was registered in, on the tiles themselves (ADR 0052 decision
/// 2): a room's arbitration is one pass over **every** creep of ours standing
/// in it, and the intents that pass folds together come from as many `decide`
/// calls as there are colonies working that room.
type MoveIntent =
    {
        Creep: string
        Pos: RoomPos
        Rank: int
        Candidates: RoomPos list
    }

/// One colony's movement for the tick, before a tile of it is arbitrated: where
/// this colony's bodies stand, which of them fatigue keeps out of the
/// arbitration, what each rested one asked for, and the two attributions only
/// this colony's Atlas can answer. It exists because **a room's movement is not
/// one colony's decision**. Two colonies work one room whenever a [[mother
/// colony]] is raising a child (ADR 0047 decision 4), and a `decide`
/// arbitrating its own half of that room's traffic against the other half's
/// tiles read as empty claimed the tile the child's [[anchor]] stood on, every
/// tick.
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
        /// The tiles held by bodies this colony does not hold ([[foreign
        /// bodies]], ADR 0052 decision 1), each carrying its room (decision 2).
        Foreign: Set<RoomPos>
        /// Each rested creep's Move Intent, each a tile of the room the
        /// creep stands in.
        Intents: MoveIntent list
        /// The creeps on the [[verbose list]] whose priced step differs from
        /// their traffic-blind one (ADR 0018, ADR 0030) — the one movement
        /// Verdict that is not the arbitration's own answer and the one that
        /// needs this colony's Atlas, so it is settled here and carried.
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
/// never a log line (ADR 0009). The Matcher speaks at conclusion level: which
/// Task won a creep and what decided it, a remembered assignment kept
/// (anti-thrash) as distinct from a fresh match, a release with its reason, or
/// why nothing was applicable.
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
    /// Rested, settled off the tile it asked for first — with nobody the pass
    /// can name holding that tile.
    | Stalled of creep: string

/// One casting row's count this tick: what the quota asks for, how many living
/// bodies the row reads back (`patternOf`), and how many are in the oven for
/// it. Observability only (ADR 0009): the cascade reads the same numbers itself,
/// and this is what `observe.mjs quotas` prints so a human can see why a spawn
/// stands idle at a full bank.
type RowQuota =
    {
        Row: string
        Quota: int
        Living: int
        Casting: int
    }

/// The tick's workforce arithmetic as the cascade saw it: the whole
/// target, the living and casting counts it was measured against, and one
/// `RowQuota` per row. `Rows` empty and `Target` zero is the cascade's
/// silence — a spawn's doorstep inside a Reach — and the view says so.
type Quotas =
    {
        Target: int
        Living: int
        Casting: int
        Rows: RowQuota list
        /// One hauler load at this bank, the divisor of the haul sum.
        HaulerLoad: int
        /// The hauler quota's own arithmetic, per source container.
        HaulerDemand: HaulDemandRow list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Quotas =
    let silent: Quotas =
        {
            Target = 0
            Living = 0
            Casting = 0
            Rows = []
            HaulerLoad = 0
            HaulerDemand = []
        }

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
        /// This colony's unarbitrated movement. `decide` folds it through the
        /// one-colony pass itself, so `Intents` and `Verdicts` are a whole
        /// answer on their own; a shell running more than one colony hands
        /// every colony's here to `resolveRooms` instead.
        Movement: Movement
        /// The cascade's own reading of the workforce this tick, for the
        /// `quotas` view; nothing downstream reads it.
        Quotas: Quotas
    }
