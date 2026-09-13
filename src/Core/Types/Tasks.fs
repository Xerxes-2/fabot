/// How a body is judged against a Task: the `BodyClass` a creep falls in,
/// store `Capacity`, and the pooled Task the Matcher ranks. The `Task`
/// vocabulary itself is in `Vocabulary`, which says why it lives there.
[<AutoOpen>]
module Fabot.Core.Types.Tasks

/// The four shapes a body takes as far as a [[capacity]] is concerned (ADR
/// 0052 decision 6) — part arithmetic and never a row's name (ADR 0006), so the
/// classes are the ones the existing gates already cut the fleet along. A class
/// exists here because some [[capacity]] scope cuts along it; `Carrier` — no
/// Work part at all — never found that reader and was folded into `Light`
/// (#233), the two having been one bucket to every scope ever written.
type BodyClass =
    /// An ATTACK part (ADR 0056): the guard row's shape, and **first** in
    /// the ladder because it is the one cut no other class makes. A guard
    /// carries no Work and no Carry, so read through the three classes
    /// below it would fall into `Light` beside the [[hauler unit]]s and the
    /// [[reserver]], and the one [[capacity]] written for a guard would be
    /// answering for them too.
    | Fighter
    /// More Work than Move (ADR 0016): the garrison's shape. Its intake is
    /// digging and its work is a [[post]], so it is the class every cap
    /// that is about standing room on a tile is written for.
    | Heavy
    /// Fewer than one Carry per `Tuning.StandingCarryPerWork` Work and not
    /// Heavy (ADR 0046): the [[upgrader]] row, which lives beside the
    /// [[buffer]] and carries one trip's worth.
    | Standing
    /// Everything else — the [[worker unit]] the colony's surplus work is
    /// done by, and beside it the bodies with no Work part at all, the
    /// [[hauler unit]] and the [[reserver]]. Those two carried a class of
    /// their own until #233: no scope ever asked which of the two a holder
    /// was, every one of them cutting at `Heavy` or at `Standing` instead,
    /// so the distinction was a case a reader had to answer twice and a
    /// second name for one crowd.
    | Light

/// How many creeps a pooled Task admits at once, set by the Planner and counted
/// by the Matcher (ADR 0052 decision 6). The Matcher knows no Task kinds: every
/// seat rule the colony has — a source's [[seat]]s, a [[post]]'s standing room,
/// a store's stock over its drawers' load, the outpost container builders'
/// budget, one holder per controller, the [[pioneer]]s' ceiling on borrowed
/// work — arrives here as numbers and tiles, and `hasCapacity` counts holders
/// against them.
/// Which crowd a cap is a number about. The scope half of a cap, carried beside
/// its number rather than spelled in a field name on one side and a class
/// predicate on the other: the pairing is the whole of what a cap means, and a
/// reader that re-asserts it is a reader that can get it wrong.
[<RequireQualifiedAccess>]
type CapScope =
    /// Holders of every class together (the [[refill cluster]]'s free energy
    /// over one [[hauler unit]] load, a Seat count, one holder per controller).
    | Everyone
    /// Holders that are `Heavy`: the garrisons, who compete for standing room
    /// with each other and with nobody else (ADR 0024).
    | Garrisons
    /// Holders that are **not** `Heavy`: ADR 0051's light crowd, kept off the
    /// Seats a [[post]] has claimed. One number over the group and not one
    /// apiece, because "the Seats beyond the Posts" is a count of tiles and any
    /// body but a garrison may stand on one.
    | Commuters
    /// Holders that are `Standing`: the row that lives at the [[buffer]] and
    /// drinks it fifty energy at a time (#196).
    | Standing
    /// Holders that are neither `Heavy` nor `Standing`: the generalists' own
    /// share of a store the standing row also drinks from, divided by the load
    /// *they* carry (#196).
    | Generalists
    /// Holders that are `Fighter`, **and no holder of any other class at all**:
    /// the [[guard]]'s share of a Guard, which ADR 0056 states as `Fighter ->
    /// the room's quota`, every other class 0". The exclusivity rides this scope
    /// because the five above cannot spell "not a Fighter" between them —
    /// `Commuters` and `Generalists` each contain the class — and a zero written
    /// as two of them would be a number about somebody else's crowd rather than
    /// a refusal. The one cap written for a class rather than for a crowd, and
    /// the second lock on a Task whose applicability already asks for an ATTACK
    /// part: the Matcher recognises no Task kinds (ADR 0052 decision 6), so what
    /// keeps a [[hauler unit]] out of a fight has to be sayable in numbers.
    | Fighters

type Capacity =
    {
        /// The caps this Task carries, each under the crowd it is a number
        /// about. A scope with no entry is unbounded — which is most of the
        /// pool, and the shape the Matcher answers without ever walking the
        /// assignment map. One map rather than a field per scope, because a cap
        /// *is* a (crowd, number) pair: spelt as a field name on one side and a
        /// class predicate on the other, the association had to be re-asserted
        /// by every reader, and "is anything capped at all" had to list them
        /// all again.
        Caps: Map<CapScope, int>
        /// Tiles whose standing **heavy** occupant holds a slot against
        /// `CapScope.Garrisons` whatever Task it holds this tick — **every**
        /// [[post]] of the rock since #269, where #205 carried only the Posts
        /// whose container was still a site. Standing room is a fact about where
        /// a body is (ADR 0024), so the tile is taken while a heavy body stands
        /// on it and free only when none does: a cap counting the Task's holders
        /// alone reads the tile as free on every tick its occupant happens to
        /// hold something else — a build tick on a site Post, an Upgrade through
        /// a drained rock's window on a bare [[dual seat]]. **Unioned** with
        /// those holders and never added to them, one body that both holds the
        /// Task and stands on its Post being one garrison and not two. Counted
        /// against that one cap and not against `CapScope.Everyone`. Empty for
        /// every other Task.
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
            Caps = Map.empty
            Garrison = Set.empty
            Exempt = Set.empty
        }

    /// One crowd's cap written onto a Task.
    let capping scope limit (capacity: Capacity) =
        { capacity with
            Caps = Map.add scope limit capacity.Caps
        }

    /// The same for a number that may be absent: no number is no cap, never a
    /// cap of nothing — an unbounded scope has no entry at all, which is what
    /// keeps "somebody else's crowd" and "a crowd admitting nobody" apart.
    let cappingMaybe scope (limit: int option) (capacity: Capacity) =
        match limit with
        | Some n -> capping scope n capacity
        | None -> capacity

    /// The Post tiles whose heavy occupant takes a garrison slot (#269).
    let garrisoning tiles (capacity: Capacity) = { capacity with Garrison = tiles }

    /// The tiles a candidate standing on is outside every cap (#205).
    let exempting tiles (capacity: Capacity) = { capacity with Exempt = tiles }

    /// One number over every class: a Seat count, a store's stock divided
    /// by one load, one holder per controller.
    let total n =
        unbounded |> capping CapScope.Everyone n

    /// The Guard's cap (ADR 0056): that room's quota of `Fighter` bodies,
    /// and nobody else at all.
    let fighters n =
        unbounded |> capping CapScope.Fighters n

    /// The number one crowd is capped at, or None where that crowd is
    /// unbounded: the read at the other end of `capping`.
    let capOf scope (capacity: Capacity) = Map.tryFind scope capacity.Caps

    /// Whether any cap at all is set — the question that decides whether
    /// the Matcher pays for a walk over the holders (ADR 0029).
    let isBounded (capacity: Capacity) = not (Map.isEmpty capacity.Caps)

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
    /// The extractor over a Thorium mineral (ADR 0057 decision 1). The one
    /// placeable kind whose tile is its *target's* — it goes on the mineral
    /// itself, which is a wall tile at the mouth of a wall and so outside the
    /// clustered ordering by construction — and the only one the Layout places
    /// exactly once in a room's life.
    | Extractor

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

/// Every Resource the colony names — the closed set, for building tables over
/// the vocabulary, and closed by `Core.Tests` exactly as `allBodyParts` is.
let allResources = [ Energy; Thorium ]

/// Screeps RESOURCE_* strings as the engine spells them, in `store` keys and in
/// the `withdraw`/`transfer` calls that have taken one all along — the one
/// place the spelling lives (its reverse is derived from this table).
/// `RESOURCE_THORIUM` is the season mod's own one-letter key, which is what
/// `mineralType` reads on the deposit and what the reactor's store is filed
/// under (ADR 0057).
let resourceName =
    function
    | Energy -> "energy"
    | Thorium -> "T"

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
        BuiltKind.Extractor
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
    | BuiltKind.Extractor -> "extractor"
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
    | Extractor -> BuiltKind.Extractor

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
    | BuiltKind.Extractor
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
    | BuiltKind.Extractor
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
    | BuiltKind.Extractor
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
    | BuiltKind.Extractor
    | BuiltKind.Other -> false

/// Where a kind is whole — which of the three rules judges its hits (ADR
/// 0034), never the numbers themselves: the fraction and the floor are the
/// Repair pool's tunables, stated where the pool that reads them is.
[<RequireQualifiedAccess>]
type WholeLine =
    /// A fraction of max hits: the decaying kinds (ADR 0010) — a road and
    /// a container. **Two fractions since ADR 0061**, and which one is
    /// read is the pool's business and not this type's: hungry below
    /// `Tuning.RepairTrigger` when nobody holds the Repair, whole at
    /// `Tuning.RepairWholeLine` when somebody does. This is the one arm
    /// of the three the held fact reaches at all.
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
    | BuiltKind.Extractor
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
    | BuiltKind.Extractor
    | BuiltKind.Other -> false

/// The kinds a creep can stand on; every other kind blocks its tile
/// (Screeps OBSTACLE_OBJECT_TYPES). Other is not walkable: a kind the
/// decision layer has no rules for is the one thing that must not quietly
/// open a tile, which is why Rampart is a case of its own.
let isWalkable =
    function
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Rampart
    // The extractor is not one of OBSTACLE_OBJECT_TYPES: the miner that works
    // it stands beside the mineral, never on it, but a body crossing a room
    // walks over an extractor as it walks over a road.
    | BuiltKind.Extractor -> true
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
