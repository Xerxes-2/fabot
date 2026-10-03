/// How a body is judged against a Task: the `BodyClass` a creep falls in,
/// store `Capacity`, and the pooled Task the Matcher ranks. The `Task`
/// vocabulary itself is in `Vocabulary`, which says why it lives there.
[<AutoOpen>]
module Fabot.Core.Types.Tasks

/// The four shapes a body takes as far as a capacity is concerned — part
/// arithmetic and never a row's name. A class exists here because some
/// capacity scope cuts along it; `Carrier` never found that reader and was
/// folded into `Light` (#233).
type BodyClass =
    /// ADR-0056. A fighting row's body — an ATTACK or RANGED_ATTACK part, or
    /// a squad's medic — first in the ladder because a fighter carries no
    /// Work and no Carry and would otherwise fall into `Light`.
    | Fighter
    /// More Work than Move: the garrison's shape.
    | Heavy
    /// Fewer than one Carry per `Tuning.StandingCarryPerWork` Work and not
    /// Heavy: the upgrader row.
    | Standing
    /// Everything else: the worker unit, and beside it the bodies with no
    /// Work part at all (the hauler unit and the reserver).
    | Light

/// A [[squad]] member's place in it: what a `Fight`'s or an `Assault`'s
/// capacity counts per role. A cast carries its role in its name
/// (`Bodies.squadRoleOf`).
type SquadRole =
    /// The melee front.
    | Brawler
    /// An [[assault]]'s front: WORK that takes a rampart down (#490).
    | Sapper
    /// An [[assault]]'s cheap lone sapper (#493): dismantles until the room
    /// shows safe mode or it dies, never falling back on its hits.
    | Probe
    /// The healer walking behind it.
    | Medic
    /// The ranged member.
    | Kiter

[<RequireQualifiedAccess>]
module SquadRole =
    /// Every role, in the order a squad casts them: the fronts the medic
    /// walks behind first.
    let all = [ Brawler; Sapper; Probe; Medic; Kiter ]

    /// Whether the role takes structures down: an assault's front.
    let dismantles (role: SquadRole) = role = Sapper || role = Probe

    /// How many of these slots are this role's.
    let slots (role: SquadRole) (roles: SquadRole list) =
        roles |> List.filter ((=) role) |> List.length

/// Which crowd a cap is a number about. Carried beside its number rather than
/// spelled in a field name on one side and a class predicate on the other:
/// the pairing is the whole of what a cap means.
[<RequireQualifiedAccess>]
type CapScope =
    /// Holders of every class together.
    | Everyone
    /// Holders that are `Heavy`: the garrisons.
    | Garrisons
    /// Holders that are not `Heavy`. One number over the group and not one
    /// apiece, because "the Seats beyond the Posts" is a count of tiles.
    | Commuters
    /// Holders that are `Standing` (#196).
    | Standing
    /// Holders that are neither `Heavy` nor `Standing` (#196).
    | Generalists
    /// Holders that are `Fighter`, and no holder of any other class at all.
    /// The exclusivity rides this scope because the five above cannot spell
    /// "not a Fighter" between them — `Commuters` and `Generalists` each
    /// contain the class.
    | Fighters
    /// Holders of one squad role. A Task carrying any role cap is
    /// held by role bodies alone: a candidate of a role with no slot, or of
    /// no role, is refused.
    | Role of SquadRole

/// ADR-0052. How many creeps a pooled Task admits at once, set by the Planner
/// and counted by the Matcher, which knows no Task kinds: every seat rule
/// arrives here as numbers and tiles.
type Capacity =
    {
        /// The caps this Task carries, each under the crowd it is a number
        /// about. A scope with no entry is unbounded — most of the pool, and
        /// the shape the Matcher answers without walking the assignment map.
        Caps: Map<CapScope, int>
        /// Tiles whose standing heavy occupant holds a slot against
        /// `CapScope.Garrisons` whatever Task it holds this tick — every Post
        /// of the rock (#269). A cap counting the Task's holders alone reads
        /// the tile as free on every tick its occupant holds something else.
        /// Unioned with those holders, never added: one body that both holds
        /// the Task and stands on its Post is one garrison. Empty for every
        /// other Task.
        Garrison: Set<RoomPos>
        /// Tiles a candidate standing on is outside every cap above: the
        /// container site under a garrison's own feet, which the outpost
        /// builders' budget does not price because that budget prices a
        /// commute and this body made none. Empty for every other Task.
        Exempt: Set<RoomPos>
        /// Ticks for which a relief and its incumbent are meant to coexist at
        /// this Task (#329). Zero keeps the ordinary arrival-counted cap.
        Handover: int
        /// Whether a holder inside its lead (the spawn row's `expiring`) is
        /// out of every cap above, so the relief cast at that lead takes the
        /// seat beside it (#439). For a Task no arrival is priced for, where
        /// `Handover` has nothing to be read against. False for every other.
        Relieved: bool
        /// ADR-0071. An energy budget the holders' loads are counted against,
        /// where every cap above counts holders. The refill cluster's free
        /// energy; None for every other Task.
        Budget: int option
        /// The same over the holders' live Work parts (#483): an RCL8
        /// controller's `Engine.maxUpgradePerTick`. None for every other Task.
        WorkBudget: int option
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
            Handover = 0
            Relieved = false
            Budget = None
            WorkBudget = None
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

    /// Admit a relief early enough to overlap its incumbent for this many
    /// ticks. The cap remains the permanent seat count; this is only its
    /// handover window (#329).
    let handingOver ticks (capacity: Capacity) = { capacity with Handover = ticks }

    /// Admit a relief beside a holder inside its lead, and no second body
    /// beside one outside it (#439).
    let relieving (capacity: Capacity) = { capacity with Relieved = true }

    /// A budget the holders' loads are counted against (#374): admitted while
    /// what they carry together falls short of `energy`.
    let budgeting energy (capacity: Capacity) = { capacity with Budget = Some energy }

    /// A budget the holders' Work parts are counted against (#483): admitted
    /// while the Work they hold together falls short of `work`.
    let budgetingWork work (capacity: Capacity) =
        { capacity with WorkBudget = Some work }

    /// One number over every class: a Seat count, a store's stock divided
    /// by one load, one holder per controller.
    let total n =
        unbounded |> capping CapScope.Everyone n

    /// The Guard's cap: that room's quota of `Fighter` bodies, and nobody
    /// else at all.
    let fighters n =
        unbounded |> capping CapScope.Fighters n

    /// A Fight's or an Assault's cap: its squad's slots per role, every role written
    /// so a role the squad has no slot for is a cap of nothing.
    let roles (slots: SquadRole list) =
        SquadRole.all
        |> List.fold
            (fun capacity role ->
                capping (CapScope.Role role) (SquadRole.slots role slots) capacity)
            unbounded

    /// The number one crowd is capped at, or None where that crowd is
    /// unbounded: the read at the other end of `capping`.
    let capOf scope (capacity: Capacity) = Map.tryFind scope capacity.Caps

    /// Whether any cap at all is set — the question that decides whether
    /// the Matcher pays for a walk over the holders.
    let isBounded (capacity: Capacity) =
        not (Map.isEmpty capacity.Caps)
        || Option.isSome capacity.Budget
        || Option.isSome capacity.WorkBudget

/// One entry of this tick's Task pool: the Task, where it ranks and how many
/// bodies it admits.
type PooledTask =
    {
        Task: Task
        /// Where this Task ranks against every other, lower first. The
        /// Matcher's first key component, through `Pool.matchRank`;
        /// `MatchFactor.Rank` names it.
        Priority: int
        Capacity: Capacity
        /// Whether this is work in a room another colony of ours runs — the
        /// borrowed Upgrade a mother colony pools for her pioneers, and the
        /// child's builds beside it. Read by one body gate: a standing body
        /// holds no commuting work.
        Borrowed: bool
    }

/// What kind of structure a placement Intent asks for.
type StructureKind =
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    /// A rampart, over the Keep and the Posts: the only placeable kind that
    /// goes on a tile something already stands on.
    | Rampart
    /// The extractor over a Thorium mineral: the one placeable kind whose
    /// tile is its target's (the mineral is a wall tile, so outside the
    /// clustered ordering by construction).
    | Extractor
    /// The terminal (#349). One per room from RCL6, placed behind the
    /// Storage's own pick.
    | Terminal
    /// A spawn beyond the first (#408). The first is a human's, and the whole
    /// plan is oriented on it.
    | Spawn
    /// The observer (#484), at RCL8, placed after the extensions.
    | Observer

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

/// Direction of a single step between adjacent tiles; `None` for a tile and
/// itself. The Resolver's arbitrated moves and a light tick's replayed ones
/// both spell a step with it.
let directionTo (from: Pos) (dest: Pos) : Direction option =
    match sign (dest.X - from.X), sign (dest.Y - from.Y) with
    | 0, -1 -> Some Top
    | 1, -1 -> Some TopRight
    | 1, 0 -> Some Right
    | 1, 1 -> Some BottomRight
    | 0, 1 -> Some Bottom
    | -1, 1 -> Some BottomLeft
    | -1, 0 -> Some Left
    | -1, -1 -> Some TopLeft
    | _ -> None

/// Every BodyPart — the closed set, for building tables over the vocabulary.
/// A literal, closed by `Core.Tests`, which fails when this list is short.
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
/// under.
let resourceName =
    function
    | Energy -> "energy"
    | Thorium -> "T"

/// Every BuiltKind the engine spells — the modelled set. Other is not one of
/// them: it is the absence of a modelled kind. Closed by `Core.Tests`.
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
        BuiltKind.Terminal
        BuiltKind.Observer
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
    | BuiltKind.Terminal -> "terminal"
    | BuiltKind.Observer -> "observer"
    | BuiltKind.Other -> ""

/// The built kind a placement Intent's kind names: the one crossing between
/// the Intent vocabulary and the projection's, stated in Core beside both
/// unions rather than respelled wherever the two meet. Every placeable kind is
/// a built kind; the reverse does not hold — a Link is projected but never
/// placed — so the crossing runs this way only.
let builtKindOfPlaceable =
    function
    | Extension -> BuiltKind.Extension
    | Tower -> BuiltKind.Tower
    | Road -> BuiltKind.Road
    | Container -> BuiltKind.Container
    | Storage -> BuiltKind.Storage
    | Rampart -> BuiltKind.Rampart
    | Extractor -> BuiltKind.Extractor
    | Terminal -> BuiltKind.Terminal
    | Spawn -> BuiltKind.Spawn
    | Observer -> BuiltKind.Observer

/// The kinds Refill keeps fed: the spawn-energy feeders and the towers, the
/// structures a view projects as Refillables. The controller container and the
/// Storage are Refill targets too, but the Planner pools them off the
/// projection's stores.
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
    // The terminal is fed by no Refill (#349): its energy and its ore are the
    // send rule's business, pooled off its store the way the Storage's are.
    | BuiltKind.Terminal
    | BuiltKind.Observer
    | BuiltKind.Other -> false

/// ADR-0034. The Keep: the structures worth defending. One list, three rules
/// hang off it: a rampart covers each, Repair keeps each at full hits, and any
/// one below full while a hostile stands in the room fires the safe-mode reflex.
let isKeep =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Terminal -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Extractor
    | BuiltKind.Observer
    | BuiltKind.Other -> false

/// The kinds a raid's damage is charged on: the Keep and the ramparts that
/// cover it. Not the roads and the containers — a chewed road is ordinary decay.
let isDefence =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Terminal
    | BuiltKind.Rampart -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Extractor
    | BuiltKind.Observer
    | BuiltKind.Other -> false

/// The kinds whose projection has to ask the engine who owns them: every
/// ownable kind whose hits a decision reads. A structure of another owner left
/// standing in a room we took is neither ours to repair nor ours to charge a
/// raid's damage on.
let needsOwner =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Terminal
    | BuiltKind.Rampart -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Extractor
    | BuiltKind.Observer
    | BuiltKind.Other -> false

/// Where a kind is whole — which of the three rules judges its hits, never the
/// numbers themselves: the fraction and the floor are the Repair pool's
/// tunables, stated where the pool that reads them is.
[<RequireQualifiedAccess>]
type WholeLine =
    /// A fraction of max hits: the decaying kinds, a road and a container.
    /// Two fractions (hungry and whole), and which one is read is the pool's
    /// business.
    | Fraction
    /// A fixed floor of hits: the rampart.
    | Floor
    /// Full hits: the Keep. It does not decay, so below max means damaged.
    | Full

/// The line a kind is whole at, or None for a kind Repair never touches. The
/// repairable kinds are exactly the kinds whose hits the projection carries.
let wholeLine =
    function
    | BuiltKind.Road
    | BuiltKind.Container -> Some WholeLine.Fraction
    | BuiltKind.Rampart -> Some WholeLine.Floor
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Terminal -> Some WholeLine.Full
    | BuiltKind.Extension
    | BuiltKind.Link
    | BuiltKind.Extractor
    | BuiltKind.Observer
    | BuiltKind.Other -> None

/// The kinds whose stored energy enters the projection: the containers, the
/// Storage, and the terminal.
let isStored =
    function
    | BuiltKind.Container
    | BuiltKind.Storage
    // The terminal's store is read for the Thorium waiting to be sent and the
    // energy the fee is paid out of (#349), never as an energy intake or a
    // hauling sink: a store in the projection that no rule names is a store
    // every generic rule may pick up.
    | BuiltKind.Terminal -> true
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower
    | BuiltKind.Road
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Extractor
    | BuiltKind.Observer
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
    // In OBSTACLE_OBJECT_TYPES, like the Storage it stands beside
    // (`docs/research/creep-positioning-traffic.md`).
    | BuiltKind.Terminal
    | BuiltKind.Observer
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
