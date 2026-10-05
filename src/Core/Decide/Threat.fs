/// The tick's threat facts: the tiles a hostile can reach, layered by room
/// name. Colony facts, never a change to the spatial projection.
[<AutoOpen>]
module Fabot.Core.Decide.Threat

open Fabot.Core
open Fabot.Core.Types

/// One `Fight`'s ground this tick (#453), off the squad's holders: the rally
/// ground until the squad launches, the fighting ground after.
type FightGround =
    {
        /// The squad the room's Fight is fought with (`HeldTaskFacts.Fights`),
        /// latched by its fight record.
        Squad: Squad
        /// Derived each tick from where the casts stand, never stored: every
        /// slot filled by a living body, a cast holding the Fight or a
        /// resident fitting it, and each cast holder on the rally ground or
        /// with the squad — within two of the leader, the leader within two
        /// of another; or, once in, every cast holder standing in the room or
        /// at its crossing. Never without a rally ground, nor with a slot
        /// empty.
        Launched: bool
        /// The rally ground: tiles beside the crossing toward the room, in
        /// the last room on the chain from home short of it that is neither a
        /// Source Keeper's nor a rival's, clear of the border and of that
        /// room's Reach. Empty where no chain reaches the room.
        Rally: Set<RoomPos>
        /// A brawler's ground: the rally ground, or, launched, the kill
        /// order's head's ring, and the room's resident ring while no target
        /// stands there.
        Front: Set<RoomPos>
        /// A medic's: the rally ground, or, launched, the tiles beside the
        /// leader no melee body stands beside — behind the brawler it
        /// pre-heals, where the raid's melee strike the brawler and take its
        /// strike-back — else the tiles beside the leader; the rally ground
        /// with no leader to walk behind, never the front.
        Behind: Set<RoomPos>
        /// A kiter's: the rally ground, or, launched, the kite ground.
        Ranged: Set<RoomPos>
        /// The bodies the Fight admits, by name and role: every squad cast,
        /// and once launched each resident that fits a slot.
        Roles: Map<string, SquadRole>
        /// The room's residents — holders of its Guard or its Fight that are
        /// no squad cast — whose parts fit one of its slots, by role.
        Residents: Map<string, SquadRole>
    }

/// One `Assault`'s ground this tick (#490), off its squad's holders: the
/// rally ground until the squad launches, beside its target after.
type AssaultGround =
    {
        /// The roles the declared squad casts (`Assault.Squad`).
        Slots: SquadRole list
        /// A `Fight`'s launch, over its casts alone, or while they stand as
        /// one file; never while one of them is hurt — under half its hits
        /// once in, under four fifths before — or while safe mode runs in the
        /// room.
        Launched: bool
        /// The rally ground: at home, beside the exit its chain into the
        /// rival's room leaves by (`musterAtHome`).
        Rally: Set<RoomPos>
        /// What the sappers take down this tick (`AssaultFacts.Targets`'
        /// head), None while the room is dark or nothing is left.
        Target: (string * RoomPos) option
        /// A sapper's ground: the rally ground — the way out (`wayOut`) first
        /// while a cast of a squad not launched stands in the room — or,
        /// launched, the tiles beside the target: beside the first declared
        /// breach tile while the room is dark, and beside the last once a
        /// `Provoke`'s breach has fallen.
        Front: Set<RoomPos>
        /// A medic's: the sappers' ground not launched, or, launched, the
        /// tiles beside the leading sapper that are not the front.
        Behind: Set<RoomPos>
        /// Its casts holding it, by role.
        Roles: Map<string, SquadRole>
    }

/// Which flavour one room's `guard:<room>` Task is this tick, its `Brief`'s
/// `Kind`: classified once (`threatsHeldBy`), and read in place of the room's
/// class. One kind a room, the first in `BriefKind.order` to claim it.
[<RequireQualifiedAccess>]
type BriefKind =
    /// The melee guard on a raid's ring, or walked to a threatened outpost
    /// the colony is blind in (#366): every Guard no other kind claims.
    | Outpost
    | Core
    | ExitHold
    | Resident
    | Harass
    | Chase
    | Trespass

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BriefKind =
    /// The order a room's flavour is read in, written once: a resident room,
    /// a [[harassment room]], a chase (#510), a raid's ring, a held exit
    /// (#450), a level-0 core (#487), a trespass's kill (#516). The first to
    /// claim a room is its kind; a kind claims only a room still open to it
    /// (`opens`), so a held exit, a core and a trespass lay no ground under a
    /// kind ahead of them (`threatsHeldBy`); and a Guard's ground is the first
    /// laid in its room from its kind down (`Brief.Ground`).
    let order =
        [
            BriefKind.Resident
            BriefKind.Harass
            BriefKind.Chase
            BriefKind.Outpost
            BriefKind.ExitHold
            BriefKind.Core
            BriefKind.Trespass
        ]

    /// The kinds whose ground is the room's `Ring`.
    let laysRing (kind: BriefKind) =
        match kind with
        | BriefKind.Outpost
        | BriefKind.ExitHold
        | BriefKind.Core -> true
        | _ -> false

    /// Whether a room a kind ahead claims is still open to `kind`: only a ring
    /// kind under a chase. Two halves. Load-bearing: a chase in sight is
    /// always a raid's room, so the raid still claims it as `Outpost`, and
    /// that claim shuts the held exit, the core and the trespass out of it.
    /// Quirk, kept (the Brief's S0): out of sight no raid claims it, so a held
    /// exit or a core lays its ring there, and the chase, with no kite
    /// ground, takes that melee guard's ring.
    let opens (ahead: BriefKind) (kind: BriefKind) =
        ahead = BriefKind.Chase && laysRing kind

    /// The ranger row's kinds (`Facts.rangerRooms`): every other is the guard's.
    let isRangers (kind: BriefKind) =
        match kind with
        | BriefKind.Resident
        | BriefKind.Harass -> true
        | _ -> false

    /// A kill (#510, #516): one body, a rung up, cast for by no row, and a
    /// quiet holder called to it.
    let isKill (kind: BriefKind) =
        match kind with
        | BriefKind.Chase
        | BriefKind.Trespass -> true
        | _ -> false

/// The rungs a Task may be stepped by inside its tier, a union rather than an
/// int for the reason `Tier` is one (#237): the Resolver's `weightOfRank` has
/// to round every one of them back onto the tier's own push weight, and the
/// test that checks that walks the cases off the union. A rung always steps a
/// Task up, so the ranks below are negative.
type Rung =
    | OnTheTier
    | OneRungUp
    | TwoRungsUp

/// What a Guard's holder walks round on its way to its ground.
[<RequireQualifiedAccess>]
type Avoid =
    /// Nothing of its own: a body that runs from a Threat walks round the
    /// Reach as every runner does.
    | Runner
    /// Every Reach on the way (#516): its target strikes nothing back, and no
    /// Threat may.
    | Reach
    /// A ranger's kite (#510): what each melee in the room it stands in
    /// reaches in a step and a swing rather than the whole Reach, so the ring
    /// at three it shoots from stays open to it, and a step and a swing off
    /// the exit a raid left by, where a body camping across lands beside it.
    /// Across the border too.
    | MeleeStepAndSwing

/// Who a kill may call off another room's Guard (#510, #516), and within what
/// walk.
type Recall =
    {
        /// How far the holder walks for it, in travel cost.
        Walk: int
        /// Whether the holder of one room's Guard may be called off it for the
        /// kill (`Facts.mayBeCalled`): the colony, every kill's room, the
        /// held room, the kill's.
        MayBeCalled: ColonyView -> Set<string> -> string -> string -> bool
    }

/// What a Guard's holder shoots in its room, and in what kill order.
type Selector =
    {
        /// Whether it shoots a hostile (`Facts.guardShoots`): the colony, its
        /// resident rooms and the rooms a Fight is pooled for, then the hostile.
        Shoots: ColonyView -> Set<string> -> Set<string> -> HostileInfo -> bool
        /// The head of the kill order among the shots in a room: by
        /// `Facts.killRank`, then by the holder's walk to each, then by id.
        /// The colony, the room, the walk and the shots; each rank priced once.
        Head: ColonyView -> string -> (HostileInfo -> int) -> HostileInfo list -> HostileInfo option
    }

/// The ground each source laid in one room this tick, before the room's kind
/// picks its own (`Brief.Ground`); None where it laid none.
type Laid =
    {
        /// Under a raid or a rival's claimer (#451) in a ranger room
        /// (`Facts.rangerRooms`), or in a chased room a raid stands in, the
        /// ranger's ground there ahead of every other: under an armed raid,
        /// our ramparts within three of the kill order's head (#467); else in
        /// a resident room whose raid no ranger wins (`Facts.outmatched`), the
        /// room's safe set, laid only while a ranger of ours stands there or
        /// holds its Guard (`threatsOfHeld`); else, while a melee body stands
        /// there or the kill order's head is a claimer, the kite ground — the
        /// tiles within three of that head, less every tile a melee body
        /// reaches in a step and a swing, and less swamp near one. With a
        /// melee body there and that empty, the tiles farthest from it, never
        /// the threats' ring beside it. A room whose squad has launched holds
        /// the squad's kite ground (`threatsOfHeld`).
        Kite: Set<RoomPos> option
        /// In a resident room (`Facts.residentRooms`), the garrison's ground,
        /// which it holds and shoots from: in a declared errand room the
        /// Reactor's own ring (#414, #411), in a raised child's home its
        /// controller's while no armed Threat stands there (#447).
        Resident: Set<RoomPos> option
        /// In a [[harassment room]] (#432), the ranger's: an ambush (#439) —
        /// the walkable tiles within ranged reach of every enemy creep on a
        /// work spot, beside every armed target, and the declared `Stand`'s
        /// seats while neither stands there.
        Harass: Set<RoomPos> option
        /// The walkable tiles within range 1 of a Threat standing in the room,
        /// less the tiles the Threats stand on — the guard's ground. In a room
        /// whose last Threat has gone, the ground beside the exit it left by
        /// while that is held (#450); and a level-0 core's ring (#487).
        Ring: Set<RoomPos> option
        /// Where a trespasser stands (`Facts.trespasser`, #516) and no kind
        /// ahead claims the room, the tiles beside each trespasser off the
        /// exits, or a hauler's store ground.
        Trespass: Set<RoomPos> option
    }

/// ADR-0085
/// One room's `guard:<room>` Task this tick: its flavour and every answer a
/// Guard reader asks of it, derived each tick (`threatsHeldBy`) and never
/// stored. A room no source claims reads `Brief.unclaimed`. Equal only to
/// itself: its answers are functions.
[<ReferenceEquality>]
type Brief =
    {
        Kind: BriefKind
        /// Every kind that claimed the room, down `BriefKind.order`: `Kind`
        /// first. A held exit (#450) among them is where the Planner keeps a
        /// living guard's Guard pooled, and hires for none.
        Claims: BriefKind list
        /// What its holder shoots (`Emitter.guardTarget`).
        Targets: Selector
        /// Whether a body may take it: the Emitter's gate, the colony and the
        /// Atlas given. The walk to the ground is what travel cost prices.
        Admits: ColonyView -> Atlas.Atlas -> CreepInfo -> bool
        /// For a kill, who it calls off another room's Guard (the Matcher's).
        Recall: Recall option
        Laid: Laid
        /// Its holder's ground: the first laid in the room from its kind down
        /// `BriefKind.order`. A ranger's kind and a chase hold their kite
        /// ground (#451) first, so a melee guard's kind is never handed it; a
        /// ranger's declared ground next; a ring kind the room's `Ring`; a
        /// trespass its own (#516). None where none is, and the pool falls
        /// back to the source ring (#366).
        Ground: Set<RoomPos> option
        /// With no ground and no source placed, whether the walk in is to the
        /// room's ground off the border: a chased room shut on its raid (#510).
        WalksIn: bool
        Avoid: Avoid
        Rung: Rung
        /// How many bodies it admits, off the row's own count, the colony and
        /// the tick's Threats given.
        Capacity: ColonyView -> Threats -> Capacity
    }

/// ADR-0033. Derived once a tick and shared by the applicability gate, Flee's
/// Work Area and the spawn hold. Keyed by room: a `Set<Pos>` cannot say which
/// room's tiles it holds, so the room rides on the outer key. A room with no
/// entry answers the empty set.
and Threats =
    {
        /// Per room, the tiles a Threat can hurt there — one standing in it,
        /// or one beside an exit into it (#514), so a room with no Threat
        /// can hold a Reach. Never an
        /// empty set under a room: a room whose whole Reach our ramparts
        /// took back has no entry, so `Map.isEmpty` is "no Reach
        /// anywhere" — the one question the pool asks of it.
        Reach: Map<string, Set<Pos>>
        /// Per room, the walkable tiles no Threat reaches — Flee's Work Area
        /// for a creep standing there. Derived only for the rooms with a Reach:
        /// absence means "not derived", not "nowhere is safe".
        /// `Lazy` because it is 2,000 tiles a room and its only reader is
        /// Flee's Work Area, which most ticks has no creep to offer it to (#371).
        Safe: Map<string, Lazy<Set<RoomPos>>>
        /// Per room a Fight is pooled for (`Facts.fights`), its ground
        /// (`threatsOfHeld`); empty off `threatsOf`.
        Fight: Map<string, FightGround>
        /// Per room an Assault is pooled for (`ColonyView.Assaults`), its
        /// ground (`threatsOfHeld`); empty off `threatsOf`.
        Assault: Map<string, AssaultGround>
        /// The raid at the home's line this tick (`Facts.raidAtLine`, #504):
        /// derived once, read by the Planner, the defender row and the
        /// Defend's capacity.
        AtLine: HostileInfo list
        /// Under the home, the tiles outside its line a raid melee there
        /// reaches in a step and a swing: what a [[defender]] walks round
        /// (#504), where every other runner walks round the Reach.
        DefendAvoid: Map<string, Set<Pos>>
        /// Under the home, while a raid stands at its line, the
        /// [[defender]]'s ground (#504, `defendGroundOf`).
        Defend: Map<string, Set<RoomPos>>
        /// Under the home, while an armed Threat stands in a room beside it
        /// or a raid this colony stands down from there is out of sight, the
        /// ground every idle body at home but a Fighter walks in to and stays
        /// on (#514): the walkable tiles behind the declared perimeter, else
        /// those five or more from every exit, less the home's Reach and the
        /// tiles beside a spawn (`shelterOf`). None under the
        /// home's safe mode. `Lazy` for `Safe`'s reason: most raid ticks no
        /// body idles at home.
        Shelter: Map<string, Lazy<Set<RoomPos>>>
        /// Per room some source claims or lays ground in, its Guard's `Brief`.
        Briefs: Map<string, Brief>
    }

/// The tick with nothing to run from: what the pipeline is handed for a quiet
/// colony.
let noThreats =
    {
        Reach = Map.empty
        Safe = Map.empty
        Fight = Map.empty
        Assault = Map.empty
        AtLine = []
        DefendAvoid = Map.empty
        Defend = Map.empty
        Shelter = Map.empty
        Briefs = Map.empty
    }

/// ADR-0056
/// How many guards one guarded room wants: one, two where one block loses
/// the exchange. The count reads the raid and never our own answer to it
/// (#272): priced against the guards standing in the room it was not monotone,
/// and the reinforcement it bought was evicted on arrival. Vision is the whole
/// of what this reads: an outpost the colony cannot see carries no hostiles.
/// A room remembered and not seen is one guard (#366): a room with no
/// `RoomControl` entry contributes no hostile to the view, so neither number
/// of the two-guard clause exists. Written as its own clause rather than left
/// to `guardBlocksBeat`'s zero-damage arm: that one is a statement about a
/// visible raid dealing nothing.
let internal guardsWanted (view: ColonyView) (room: string) : int =
    if
        Set.contains room view.ThreatenedOutposts
        && not (Map.containsKey room view.RoomControl)
    then
        1
    elif guardBlocksBeat view room 1 then
        1
    else
        Engine.guardCap

/// How many rangers one resident room wants: the standing garrison
/// (`Tuning.RangerResidents`). A raised home whose towers cannot hold its raid
/// (`BorrowedWork.Defended`, #428) and whose resident loses it alone adds
/// #428's relief, `Engine.guardCap` raid-sized bodies, on top of the garrison
/// (#447). A resident room whose raid no ranger size wins (`outmatched`)
/// wants none (#451): a body cast into it only feeds the raid, so the home
/// is the child's safe mode's, and the garrison already standing keeps its
/// Guard (the pool's cap counts it) on safe ground. One in a harassment room
/// (#432): a raid that one loses is the room's [[stand-down]]. A launched
/// squad is the relief: none is cast beside it, and none is held back while
/// it musters.
let internal rangersWanted (view: ColonyView) (threats: Threats) (room: string) : int =
    if Set.contains room (harassRooms view) then
        1
    elif outmatched view room then
        0
    elif Map.tryFind room threats.Fight |> Option.exists (fun ground -> ground.Launched) then
        view.Tuning.RangerResidents
    elif
        Set.contains room (raisedHomes view)
        && List.contains room view.Borrowed.Defended
        && not (rangerBlocksBeat view room view.Tuning.RangerResidentBlocks)
    then
        view.Tuning.RangerResidents + Engine.guardCap
    else
        view.Tuning.RangerResidents

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Brief =
    /// How far a quiet ranger walks for a kill (#510), in travel cost (two
    /// units a tick): two rooms' side, where the kill is made and it is home
    /// again inside the time a raid takes to reach the room it left.
    let private killWalk = 2 * 2 * Engine.roomSide

    let private bare =
        {
            Kite = None
            Resident = None
            Harass = None
            Ring = None
            Trespass = None
        }

    /// Every kind's targets today: what `Facts.guardShoots` passes, in the kill
    /// order.
    let private guardTargets =
        {
            Shoots = guardShoots
            Head =
                fun view room walk shots ->
                    match shots with
                    | [] -> None
                    | _ ->
                        let rank = killRank view room
                        shots |> List.sortBy (fun h -> rank h, walk h, h.Id) |> List.tryHead
        }

    let private groundOf (kind: BriefKind) (laid: Laid) =
        let own kind =
            match kind with
            | BriefKind.Resident -> laid.Kite |> Option.orElse laid.Resident
            | BriefKind.Harass -> laid.Kite |> Option.orElse laid.Harass
            | BriefKind.Chase -> laid.Kite
            | BriefKind.Outpost
            | BriefKind.ExitHold
            | BriefKind.Core -> laid.Ring |> Option.filter (Set.isEmpty >> not)
            | BriefKind.Trespass -> laid.Trespass

        BriefKind.order |> List.skipWhile ((<>) kind) |> List.tryPick own

    /// A resident room's Guard is the ranger row's (#411), and a harassment
    /// room's (#432); every ring kind's the melee guard's.
    let private admitsOf
        (kind: BriefKind)
        (room: string)
        (view: ColonyView)
        (atlas: Atlas.Atlas)
        (creep: CreepInfo)
        : bool =
        match kind with
        | BriefKind.Resident
        | BriefKind.Harass -> isRangerRowBody creep
        // A ranger every raider there falls behind (#510): kited down by this
        // body's own live parts, or out of sight; and a ranger the resident
        // rule lends (#516).
        | BriefKind.Chase ->
            isRangerRowBody creep
            && (Facts.kitedDown view (partCount creep.Body) room
                || not (
                    view.Hostiles
                    |> List.exists (fun h -> h.Pos.Room = room && HostileInfo.raiderOnPaper h)
                ))
            && Facts.rangerMayKill view room (Atlas.creepRoom atlas creep.Name)
        // A trespass's kill (#516), defence first: the guard row's body, or a
        // ranger the resident rule lends. A child's outpost is the child's
        // guard's, and the residents' beside it (#517).
        | BriefKind.Trespass ->
            not (Facts.underRaid view)
            && (isGuardRowBody creep && not (List.contains room view.Borrowed.ChildOutposts)
                || isRangerRowBody creep
                   && Facts.rangerMayKill view room (Atlas.creepRoom atlas creep.Name))
        | BriefKind.Outpost
        | BriefKind.Core
        | BriefKind.ExitHold -> isGuardRowBody creep

    /// The guards a ring kind's room wants and nobody else at all:
    /// `guardsWanted` is the row's own arithmetic, read here a second time so
    /// the number the cascade hires against and the number the Matcher counts
    /// holders against are one number.
    let private capacityOf
        (kind: BriefKind)
        (room: string)
        (view: ColonyView)
        (threats: Threats)
        : Capacity =
        match kind with
        // One over per garrison body in a resident room (#414, #419, #447): its
        // rangers are resident, and each relief — cast at its incumbent's
        // lead, two of them together when the garrison was cast together —
        // must take the Task and walk three crossings while the incumbent
        // still holds the ring. A Guard has no arrival price for a handover
        // window to be read against, and the row's count, not this cap, is
        // what buys bodies.
        | BriefKind.Resident ->
            Capacity.fighters (rangersWanted view threats room + view.Tuning.RangerResidents)
        // One ranger per harassment room (#439), and its relief beside it
        // (`Capacity.Relieved`).
        | BriefKind.Harass ->
            Capacity.fighters (rangersWanted view threats room) |> Capacity.relieving
        // One ranger kites what cannot close on it (#510).
        | BriefKind.Chase -> Capacity.fighters 1
        // One body kills a trespasser, which strikes nothing back (#516), and
        // never fewer than the room's own guards.
        | BriefKind.Trespass -> Capacity.fighters (max 1 (guardsWanted view room))
        | BriefKind.Outpost
        | BriefKind.Core
        | BriefKind.ExitHold -> Capacity.fighters (guardsWanted view room)

    /// One room's Brief, off the kinds that claimed it down `BriefKind.order`
    /// and the ground each source laid there. A kill (#510, #516) is stepped a
    /// rung up — its kill is the room's whole income back, and a quiet holder
    /// is let go for it — and calls that holder off within its walk.
    let internal claimed (room: string) (claims: BriefKind list) (laid: Laid) : Brief =
        let kind = List.tryHead claims |> Option.defaultValue BriefKind.Outpost
        let kill = BriefKind.isKill kind

        {
            Kind = kind
            Claims = claims
            Targets = guardTargets
            Admits = admitsOf kind room
            Recall =
                if kill then
                    Some
                        {
                            Walk = killWalk
                            MayBeCalled = mayBeCalled
                        }
                else
                    None
            Laid = laid
            Ground = groundOf kind laid
            WalksIn = kind = BriefKind.Chase
            Avoid =
                match kind with
                | BriefKind.Chase -> Avoid.MeleeStepAndSwing
                | BriefKind.Trespass -> Avoid.Reach
                | _ -> Avoid.Runner
            Rung = if kill then OneRungUp else OnTheTier
            Capacity = capacityOf kind room
        }

    /// A room no source claims or lays ground in: a melee guard's `Outpost`
    /// on no ground, the one a remembered raid pools in a room the colony is
    /// blind in (#366).
    let unclaimed (room: string) : Brief = claimed room [] bare

    /// The same Brief on this kite ground: a launched squad's (`withFights`).
    let internal kitingOn (ground: Set<RoomPos>) (brief: Brief) : Brief =
        let laid = { brief.Laid with Kite = Some ground }

        { brief with
            Laid = laid
            Ground = groundOf brief.Kind laid
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// One room's Guard `Brief` this tick.
    let briefOf (threats: Threats) (room: string) : Brief =
        match Map.tryFind room threats.Briefs with
        | Some brief -> brief
        | None -> Brief.unclaimed room

    /// One room's shelter (#514); empty for a room with none.
    let shelterIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Shelter
        |> Option.map (fun ground -> ground.Force())
        |> Option.defaultValue Set.empty

    /// One room's `DefendAvoid`; empty for a room with none.
    let defendAvoidIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.DefendAvoid |> Option.defaultValue Set.empty

    /// The [[defender]]'s ground in one room; empty for a room it holds none
    /// in (#504).
    let defendGroundIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Defend |> Option.defaultValue Set.empty

    /// How many [[defender]]s the home wants this tick (#504): one a body of
    /// the raid at its line, at most `Tuning.DefenderCap`; none in a home
    /// that keeps no ramparts, has no perimeter rampart standing, or runs
    /// safe mode.
    let defendersWanted (view: ColonyView) (threats: Threats) : int =
        let home = SpatialInfo.homeName view.Spatial

        let shielded =
            Map.tryFind home view.RoomControl
            |> Option.exists (fun control -> control.SafeMode)

        match threats.AtLine with
        | [] -> 0
        | _ when not (keepsRamparts view) || shielded -> 0
        | _ when List.isEmpty (perimeterRamparts view) -> 0
        | raid -> min view.Tuning.DefenderCap (List.length raid)

    /// One room's Reach; empty for a room no Threat stands in or reaches
    /// across a border into (#514).
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// One room's safe set, already joined to that room; empty for a room
    /// no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Safe
        |> Option.map (fun ground -> ground.Force())
        |> Option.defaultValue Set.empty

    /// One room's range-1 ring (`Laid.Ring`), already joined to that room;
    /// empty for a room no Threat stands in.
    let ringIn (threats: Threats) (room: string) : Set<RoomPos> =
        (briefOf threats room).Laid.Ring |> Option.defaultValue Set.empty

    /// The garrison's ground in one resident room (`Laid.Resident`), or None
    /// for any other room (#414, #447).
    let residentRingIn (threats: Threats) (room: string) : Set<RoomPos> option =
        (briefOf threats room).Laid.Resident

    /// The ranger's ground in one harassment room (`Laid.Harass`), or None for
    /// any other room.
    let harassRingIn (threats: Threats) (room: string) : Set<RoomPos> option =
        (briefOf threats room).Laid.Harass

    /// One room's Guard ground (`Brief.Ground`).
    let guardGroundIn (threats: Threats) (room: string) : Set<RoomPos> option =
        (briefOf threats room).Ground

    /// The role a body holds a room's Fight in, or None where the Fight does
    /// not admit it.
    let fightRoleOf (threats: Threats) (room: string) (creep: string) : SquadRole option =
        Map.tryFind room threats.Fight
        |> Option.bind (fun ground -> Map.tryFind creep ground.Roles)

    /// One member's ground in a Fight's room, by its role; empty for a room
    /// with no Fight ground or a body it does not admit.
    let fightGroundIn (threats: Threats) (room: string) (creep: string) : Set<RoomPos> =
        match Map.tryFind room threats.Fight, fightRoleOf threats room creep with
        | Some ground, Some Medic -> ground.Behind
        | Some ground, Some Kiter -> ground.Ranged
        | Some ground, Some Brawler -> ground.Front
        | _ -> Set.empty

    /// The rooms a Fight is pooled for.
    let fightRooms (threats: Threats) : Set<string> = threats.Fight |> Map.keys |> Set.ofSeq

    /// The role a cast holds a room's Assault in, or None where the Assault
    /// does not admit it.
    let assaultRoleOf (threats: Threats) (room: string) (creep: string) : SquadRole option =
        Map.tryFind room threats.Assault
        |> Option.bind (fun ground -> Map.tryFind creep ground.Roles)

    /// One member's ground in an Assault's room, by its role; empty for a
    /// room with no Assault ground or a body it does not admit.
    let assaultGroundIn (threats: Threats) (room: string) (creep: string) : Set<RoomPos> =
        match Map.tryFind room threats.Assault, assaultRoleOf threats room creep with
        | Some ground, Some Medic -> ground.Behind
        | Some ground, Some _ -> ground.Front
        | _ -> Set.empty

/// The kill order's head among one room's targets (`Facts.killRank`), ties by
/// id; each target's rank priced once, not once a comparison.
let private killHead (view: ColonyView) (room: string) (targets: HostileInfo list) =
    match targets with
    | [] -> None
    | _ ->
        let rank = killRank view room

        targets
        |> List.map (fun hostile -> (rank hostile, hostile.Id), hostile)
        |> List.minBy fst
        |> snd
        |> Some

// The ranger's ground under a raid (#451). Engine actions resolve on the
// tick's starting tiles and a melee body must start adjacent to swing, so a
// ranger ending each tick two clear of every melee body is never hit while it
// has a free step; a swamp tile costs it that step.
//
// A melee body hits what ends a tick within a step and a swing of it, and on
// swamp within a few tiles of one the ranger loses the step it would have
// stepped away with.
let private stepAndSwing = Engine.meleeRange + 1
let private swampReach = Engine.meleeRange + 3

/// How far a kite tile's escape room is flooded (#510): the steps a ranger
/// takes while a melee closes on it from beyond a step and a swing.
let private escapeDepth = 3

/// The tiles of one room within three of the kill order's head that no melee
/// body reaches in a step and a swing, less swamp near one and the exit
/// tiles, which carry a body out of the room: the kite ground. Of those, the
/// ones with escape room (#510): a short flood of the open ground beyond each
/// — off swamp and the exits, and out of every melee's step and swing — at
/// least half the widest's, so the retreat never backs into a corridor,
/// corner or swamp pocket a melee pins it in.
let private kiteGroundIn
    atlas
    (room: string)
    (inRoom: HostileInfo list)
    (head: HostileInfo option)
    =
    let melee =
        inRoom
        |> List.filter HostileInfo.meleeLive
        |> List.map (fun hostile -> RoomPos.pos hostile.Pos)

    let standing =
        inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

    let clearOfMelee reach (tile: Pos) =
        melee |> List.forall (fun m -> range m tile > reach)

    let isOpen (tile: Pos) =
        not (Set.contains tile standing)
        && not (Seam.onRing tile)
        && clearOfMelee stepAndSwing tile
        && not (Atlas.isSwampIn atlas room tile)

    let escapeRoom (tile: Pos) =
        let rec flood depth (seen: Set<Pos>) (front: Pos list) =
            if depth = 0 || List.isEmpty front then
                Set.count seen
            else
                let next =
                    front
                    |> List.collect (Atlas.adjacentWalkableIn atlas room)
                    |> List.filter (fun step -> isOpen step && not (Set.contains step seen))
                    |> List.distinct

                flood (depth - 1) (Set.union seen (Set.ofList next)) next

        flood escapeDepth (Set.singleton tile) [ tile ]

    match head with
    | None -> Set.empty
    | Some target ->
        let tiles =
            Atlas.walkableWithinIn atlas room Engine.rangedRange (RoomPos.pos target.Pos)
            |> List.filter (fun tile ->
                not (Set.contains tile standing)
                && not (Seam.onRing tile)
                && clearOfMelee stepAndSwing tile
                && not (Atlas.isSwampIn atlas room tile && not (clearOfMelee swampReach tile)))

        match melee, tiles with
        | [], _
        | _, [] -> tiles
        | _ ->
            let roomOf = tiles |> List.map (fun tile -> tile, escapeRoom tile)
            let widest = roomOf |> List.map snd |> List.max

            roomOf |> List.filter (fun (_, space) -> 2 * space >= widest) |> List.map fst
        |> Set.ofList
        |> RoomPos.setAt room

/// The [[defender]]'s ground in the home (#504), off this tick's raid at the
/// line: every perimeter rampart of ours beside a raid body that no other
/// creep of ours stands on — the mother's lent residents among them
/// (`ColonyView.Foreign`) — or, with none beside one, those nearest the raid.
let private defendGroundOf
    (view: ColonyView)
    (atLine: HostileInfo list)
    : Map<string, Set<RoomPos>> =
    match atLine with
    | [] -> Map.empty
    | raid ->
        let home = SpatialInfo.homeName view.Spatial

        let taken =
            view.Creeps
            |> List.filter (fun creep -> not (isNamedFor defenderPattern creep.Name))
            |> List.choose (fun creep -> SpatialInfo.creepPlacementOf view.Spatial creep.Name)
            |> Set.ofList
            |> Set.union view.Foreign

        let free =
            perimeterRamparts view
            |> List.map (fun (_, tile, _) -> tile)
            |> List.filter (fun tile -> not (Set.contains tile taken))

        let gapTo (tile: RoomPos) =
            raid
            |> List.choose (fun hostile -> RoomPos.range tile hostile.Pos)
            |> List.fold min System.Int32.MaxValue

        let ground =
            match free |> List.filter (fun tile -> gapTo tile <= Engine.meleeRange) with
            | [] ->
                match free with
                | [] -> Set.empty
                | _ ->
                    let nearest = free |> List.map gapTo |> List.min
                    free |> List.filter (fun tile -> gapTo tile = nearest) |> Set.ofList
            | beside -> Set.ofList beside

        if Set.isEmpty ground then
            Map.empty
        else
            Map.ofList [ home, ground ]

/// The tiles a Threat at `pos` reaches across its room's borders within `r`
/// (#514), by neighbouring room: from each passable crossing (`Atlas.seams`),
/// what is left of `r` after the walk to the exit and the carry across,
/// measured from the tile it lands on. None for one farther than `r` from
/// every edge, the quiet case asked first.
let private reachAcross atlas (room: string) (pos: Pos) (r: int) : (string * Pos list) list =
    let edge = Seam.exitEdge

    if min (min pos.X (edge - pos.X)) (min pos.Y (edge - pos.Y)) >= r then
        []
    else
        RoomName.adjacent room
        |> List.choose (fun next ->
            match
                Atlas.seams atlas room next
                |> List.collect (fun (exit, landing) ->
                    tilesWithin (r - range pos exit - 1) landing |> List.filter inGrid)
            with
            | [] -> None
            | tiles -> Some(next, tiles))

// Past a melee's step and swing plus the margin from the exit it comes in
// by, so a body steps in only as far as that and the interior stays put.
let private shelterDepth = 5

/// The home's shelter (#514): its walkable tiles behind the declared
/// perimeter (`Colony.Perimeter`) — neither outside the line nor on it —
/// else those at least `shelterDepth` from every exit tile
/// (`Atlas.shelterGround`); in either case less `hot` and every tile beside a
/// spawn, which a cast needs free to step out onto. The home is the Atlas's
/// own, the one `shelterGround` is laid for.
let private shelterOf (view: ColonyView) atlas (hot: Set<Pos>) : Set<RoomPos> =
    let home = SpatialInfo.homeName view.Spatial

    let besideSpawns =
        view.Spawns
        |> List.choose (fun s -> Atlas.positionOf atlas s.Id)
        |> List.filter (fun tile -> tile.Room = home)
        |> List.collect (RoomPos.pos >> tilesWithin 1)
        |> Set.ofList

    Set.difference
        (Atlas.shelterGround atlas view.Perimeter shelterDepth)
        (Set.union hot besideSpawns |> RoomPos.setAt home)

/// This tick's Threats, off the view's hostiles and the rampart census, room by
/// room: weapon range plus the margin in Chebyshev tiles, and across a border
/// as far onto the next room's tiles (#514), less every tile under one of our
/// standing ramparts in the room it lands in. The held facts say which rangers
/// hold each room's Guard, which an outmatched room's safe ground is laid for.
let private threatsHeldBy (view: ColonyView) atlas (held: HeldTaskFacts) : Threats =
    // Under safe mode a hostile in a room of ours can hurt nothing, so it is no
    // Threat and has no Reach.
    let shielded room =
        match Map.tryFind room view.RoomControl with
        | Some control -> control.Owner = Ownership.Ours && control.SafeMode
        | None -> false

    // The hostiles are asked first, so a quiet colony walks nothing for its
    // Threats: neither the rampart census nor any room's own tiles are read on
    // a tick with nothing in it to run from. Its errand rooms' ground, below,
    // is a ring of eight tiles a Reactor.
    let reach, ring =
        match
            view.Hostiles
            |> List.filter (fun hostile -> not (shielded hostile.Pos.Room))
            |> List.choose (fun hostile ->
                // A body that can barely move reaches one past its weapon's
                // range (#510): with its fatigue spent it steps once, then
                // waits out the weight of that step.
                let margin =
                    if HostileInfo.stationary hostile then
                        Engine.meleeRange
                    else
                        view.Tuning.ReachMargin

                HostileInfo.weaponRangeOnPaper hostile
                |> Option.map (fun r -> hostile.Pos.Room, RoomPos.pos hostile.Pos, r + margin))
        with
        | [] -> Map.empty, Map.empty
        | threats ->
            // The tick's Threats partitioned by the room they stand in, once: all
            // three maps below are keyed on this one room set, and derived from
            // three separate regroupings nothing said so.
            let byRoom = threats |> List.groupBy (fun (room, _, _) -> room)

            // Filed by the room each tile lies in, which across a border is not
            // the room the Threat stands in, and none in a room of ours under
            // safe mode, where it could hurt nothing either.
            let reach =
                threats
                |> List.collect (fun (room, pos, r) ->
                    (room, tilesWithin r pos) :: reachAcross atlas room pos r)
                |> List.groupBy fst
                |> List.filter (fun (room, _) -> not (shielded room))
                |> List.choose (fun (room, shares) ->
                    let ramparts = Atlas.ourRampartTilesIn atlas room

                    let tiles =
                        shares
                        |> List.collect snd
                        |> List.filter (fun tile -> not (Set.contains tile ramparts))
                        |> Set.ofList

                    // Nothing left to run from once our own ramparts have taken
                    // the whole Reach back: no Reach, no entry, no safe set to
                    // derive.
                    if Set.isEmpty tiles then None else Some(room, tiles))
                |> Map.ofList

            // Less the tiles the Threats stand on: with two of them adjacent each
            // is a tile of the other's ring. Off the Threat list and not off the
            // Reach above, because the Reach has our ramparts subtracted and a
            // rampart is the best tile a guard can fight from.
            let ring =
                byRoom
                |> List.map (fun (room, inRoom) ->
                    let standing = inRoom |> List.map (fun (_, pos, _) -> pos) |> Set.ofList

                    // Asked of the grid a tile at a time rather than against the
                    // room's materialised walkable ground (#371): a ring is nine
                    // tiles a Threat and the ground is two thousand.
                    let tiles =
                        inRoom
                        |> List.collect (fun (_, pos, _) ->
                            Atlas.adjacentWalkableIn atlas room pos
                            |> List.filter (fun tile -> not (Set.contains tile standing)))
                        |> Set.ofList

                    room, RoomPos.setAt room tiles)
                |> Map.ofList

            reach, ring

    let safeGround =
        reach
        |> Map.map (fun room tiles -> lazy (Atlas.walkableTilesExcept atlas room tiles))

    // The errand rooms' ranger ground (#414, #411): the Reactor's own ring,
    // raid or none. The ranger holding it shoots out to three, which covers a
    // claimer at the Reactor and any body close enough to shoot back.
    let ringAround room pos =
        room, Atlas.adjacentWalkableIn atlas room pos |> Set.ofList |> RoomPos.setAt room

    let errandRing =
        view.Errands
        |> List.map (fun errand -> ringAround errand.RoomName (RoomPos.pos (snd errand.Target)))

    // A raised home's (#447): its controller's ring, which a declared
    // perimeter seals inside (#446) — the one tile the claim lives or dies by,
    // and what a rival's tapper walks to. In peace only: with an armed Threat
    // in the home there is no resident ground, and the garrison takes the
    // Threats' ring as #428's guard did, or its `Kite` ground (#451).
    let raised =
        raisedHomes view |> Set.filter (fun room -> not (Map.containsKey room ring))

    let homeRing =
        if Set.isEmpty raised then
            []
        else
            Atlas.idsOfKind atlas Controller
            |> List.choose (Atlas.positionOf atlas)
            |> List.filter (fun tile -> Set.contains tile.Room raised)
            |> List.map (fun tile -> ringAround tile.Room (RoomPos.pos tile))

    let residentRing = errandRing @ homeRing |> Map.ofList

    let resident = residentRooms view

    // A work spot is where the enemy's bodies must stop: within two of the
    // source, where a hauler draws from the miner's pile, and around the
    // controller, within three because a reserver shuffles about it. The
    // source's come first, and while an enemy miner is anywhere in the room
    // the ranger holds the Stand's seats rather than turn to the controller:
    // the miner steps just out of reach and back, and ground that followed
    // it off the source turned the ranger between it and the reserver, out
    // of reach of both (W18S27, t840,6xx-8xx). The seats are within reach of
    // every work spot of the source.
    let harassRing =
        view.Harass
        |> List.map (fun h ->
            let room = h.RoomName

            let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)

            let within (spot: RoomPos) reach (hostile: HostileInfo) =
                RoomPos.range spot hostile.Pos |> Option.exists (fun r -> r <= reach)

            let armedTargets, unarmedTargets =
                inRoom
                |> List.filter (guardShoots view resident Set.empty)
                |> List.partition HostileInfo.armedOnPaper

            // A room we mine keeps its source's seats for our miners: the
            // ranger stands off them, within reach of every one (W17S26,
            // 2026-10-04: one seat, which the ranger outranked the anchor on).
            let mined = Set.contains room view.MinedRooms

            let seats =
                if mined then
                    [ Engine.rangedRange, RoomPos.pos h.Stand ]
                else
                    [ 1, RoomPos.pos h.Stand ]

            let offTheSeats (tile: Pos) =
                not mined || range tile (RoomPos.pos h.Stand) > 1

            // Our shooters in the room, where they stand.
            let shooters =
                view.Creeps
                |> List.filter (fun creep -> partCount creep.Body RangedAttack > 0)
                |> List.choose (fun creep -> Atlas.creepTile atlas creep.Name)
                |> List.filter (fun tile -> tile.Room = room)

            // A body off the work spots is chased when the ranger catches it
            // (user, 2026-10-04): slower than a ranger's step a tick on plain
            // (its Carry read empty), or already a step from a shooter's
            // reach, where a chase at one pace keeps it under fire.
            let catchable (hostile: HostileInfo) =
                let moves = hostile.Body |> List.filter ((=) Move) |> List.length

                let weighed =
                    hostile.Body
                    |> List.filter (fun part -> part <> Move && part <> Carry)
                    |> List.length

                weighed > moves
                || shooters
                   |> List.exists (fun tile -> within tile (Engine.rangedRange + 1) hostile)

            let chased =
                unarmedTargets
                |> List.filter (fun hostile ->
                    not (within h.Stand 2 hostile)
                    && not (within h.Controller 3 hostile)
                    && catchable hostile)

            // A digger first, at the source or caught off it: the income is
            // the miner's. A hauler stepping on and off the source's work
            // spot otherwise turned the ground between it and a slow miner at
            // the room's other source every few ticks, the ranger caught
            // between the two (W19S25, t943,777).
            let atSource = unarmedTargets |> List.filter (within h.Stand 2)
            let digs = HostileInfo.worksOnPaper

            let reachOf targets =
                targets
                |> List.map (fun (hostile: HostileInfo) ->
                    Engine.rangedRange, RoomPos.pos hostile.Pos)

            let working =
                match List.filter digs atSource, List.filter digs chased with
                | (_ :: _ as diggers), _
                | [], (_ :: _ as diggers) -> reachOf diggers
                | [], [] ->
                    match atSource with
                    | _ :: _ -> reachOf atSource
                    | [] when not (List.isEmpty chased) -> reachOf chased
                    | [] when unarmedTargets |> List.exists digs -> seats
                    | [] -> unarmedTargets |> List.filter (within h.Controller 3) |> reachOf

            let ambushed =
                (armedTargets |> List.map (fun hostile -> 1, RoomPos.pos hostile.Pos)) @ working

            let around = if List.isEmpty ambushed then seats else ambushed

            let standing =
                inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

            room,
            around
            |> List.collect (fun (radius, pos) -> Atlas.walkableWithinIn atlas room radius pos)
            |> List.filter (fun tile -> not (Set.contains tile standing) && offTheSeats tile)
            |> Set.ofList
            |> RoomPos.setAt room)
        |> Map.ofList

    let rangers = rangerRooms view

    // The rooms a ranger chases a raid in (#510): in sight, `chased`'s; out
    // of it, an outpost shut on a raid a ranger kites down.
    let chasedRooms =
        view.Hostiles
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> List.distinct
        |> List.filter (chased view)
        |> Set.ofList
        |> Set.union view.ChasedOutposts

    // The ranger rooms an armed raid, or a rival's claimer, stands in, and
    // the chased rooms a raid stands in.
    let contested =
        (view.Hostiles
         |> List.filter (fun hostile ->
             Set.contains hostile.Pos.Room rangers
             && (Map.containsKey hostile.Pos.Room ring || claimsAFlag resident hostile))
         |> List.map (fun hostile -> hostile.Pos.Room))
        @ (chasedRooms |> Set.filter ring.ContainsKey |> Set.toList)
        |> List.distinct

    let kite =
        contested
        |> List.choose (fun room ->
            let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)

            let melee =
                inRoom
                |> List.filter HostileInfo.meleeLive
                |> List.map (fun hostile -> RoomPos.pos hostile.Pos)

            let standing =
                inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

            let gap (tile: Pos) =
                melee |> List.map (fun m -> range m tile) |> List.min

            let head =
                inRoom
                |> List.filter (guardShoots view resident Set.empty)
                |> killHead view room

            let aroundHead () = kiteGroundIn atlas room inRoom head

            // Under an armed raid, our standing ramparts within three of the
            // head (#467): a creep on its own rampart takes no damage, so
            // they are safe ground whatever the melee reaches.
            let onRamparts () =
                match head with
                | Some target when Map.containsKey room ring ->
                    Atlas.ourRampartTilesIn atlas room
                    |> Set.filter (fun tile ->
                        range tile (RoomPos.pos target.Pos) <= Engine.rangedRange
                        && not (Set.contains tile standing))
                    |> RoomPos.setAt room
                | _ -> Set.empty

            // The safe set, two thousand tiles (#371), is the outmatched
            // room's answer alone, and only for a ranger of ours to hold it:
            // one standing there, or one holding its Guard on the way in.
            let rangerHere =
                lazy
                    (let guarding =
                        HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList

                     view.Creeps
                     |> List.exists (fun creep ->
                         partCount creep.Body RangedAttack > 0
                         && (Set.contains creep.Name guarding
                             || SpatialInfo.creepPlacementOf view.Spatial creep.Name
                                |> Option.exists (fun tile -> tile.Room = room))))

            let safe () =
                Map.tryFind room safeGround
                |> Option.map (fun ground -> ground.Force())
                |> Option.defaultValue Set.empty

            // Nowhere to kite and nowhere safe: the room's tiles farthest from
            // the melee, or, where every one is within a step and a swing,
            // the tiles our rangers already hold.
            let farthest () =
                let ground =
                    Atlas.walkableTilesIn atlas room
                    |> Set.filter (fun tile -> not (Set.contains tile standing))

                let held =
                    view.Creeps
                    |> List.filter (fun creep -> partCount creep.Body RangedAttack > 0)
                    |> List.choose (fun creep ->
                        SpatialInfo.creepPlacementOf view.Spatial creep.Name)
                    |> List.filter (fun tile -> tile.Room = room)
                    |> Set.ofList

                if Set.isEmpty ground then
                    held
                else
                    let widest = ground |> Seq.map gap |> Seq.max

                    if widest <= stepAndSwing && not (Set.isEmpty held) then
                        held
                    else
                        ground |> Set.filter (fun tile -> gap tile = widest) |> RoomPos.setAt room

            let claimerLeads = head |> Option.exists (claimsAFlag resident)

            let outmatchedHere = Set.contains room resident && outmatched view room

            [
                onRamparts

                if outmatchedHere then
                    if rangerHere.Value then
                        safe
                elif not (List.isEmpty melee) || claimerLeads then
                    aroundHead
                if not (List.isEmpty melee) && (not outmatchedHere || rangerHere.Value) then
                    farthest
            ]
            |> List.tryPick (fun ground ->
                let tiles = ground ()
                if Set.isEmpty tiles then None else Some(room, tiles)))
        |> Map.ofList

    // The held exits' ground (#450): the tiles within a melee guard's reach of
    // the run its room's last armed Threat left by, less the ring, whose exit
    // tiles the engine carries a body standing on across the border. A hold
    // with no ground is no hold.
    let holdsIn (open': string -> bool) =
        view.ExitHolds
        |> Map.toList
        |> List.filter (fst >> open')
        |> List.choose (fun (room, hold) ->
            let ground =
                hold.Run
                |> List.collect (Atlas.walkableWithinIn atlas room Engine.meleeRange)
                |> List.filter (Seam.onRing >> not)
                |> Set.ofList

            if Set.isEmpty ground then
                None
            else
                Some(room, RoomPos.setAt room ground))

    // A level-0 core's ring (#487), the tiles a guard swings at it from.
    let coresIn (open': string -> bool) =
        Facts.expansionCores view
        |> List.filter (fun core -> open' core.RoomName)
        |> List.map (fun core -> ringAround core.RoomName core.Tile)

    let outside, atLine = raidAtLine view atlas

    // The tiles outside the line a raid melee there reaches in a step and a
    // swing: what a defender's walk keeps off (#504). Inside the line a
    // melee outside it reaches nothing but the rampart it strikes.
    let defendAvoid =
        match atLine |> List.filter HostileInfo.meleeLive with
        | [] -> Map.empty
        | melee ->
            let home = SpatialInfo.homeName view.Spatial

            Map.ofList
                [
                    home,
                    outside
                    |> Set.filter (fun tile ->
                        melee
                        |> List.exists (fun m -> range (RoomPos.pos m.Pos) tile <= stepAndSwing))
                ]

    // The home's shelter (#514): laid while an armed Threat stands in a room
    // beside it, or a raid this colony stands down from there is out of sight,
    // and never under the home's own safe mode.
    let shelter =
        let home = SpatialInfo.homeName view.Spatial
        let beside room = RoomName.neighbouring home room

        let nextDoor =
            view.Hostiles
            |> List.exists (fun hostile ->
                beside hostile.Pos.Room
                && not (shielded hostile.Pos.Room)
                && HostileInfo.armedOnPaper hostile)
            || Set.exists beside view.RaidedOutposts

        if nextDoor && not (shielded home) then
            let hot = Map.tryFind home reach |> Option.defaultValue Set.empty
            Map.ofList [ home, lazy (shelterOf view atlas hot) ]
        else
            Map.empty

    // The trespassers' ground (#516), off the exits.
    //
    // A digger or a claimer stands at its work: the tiles beside it. A hauler
    // keeps a ranger's pace, so it is waited for, not chased: within
    // `haulGround` of the store nearest it, which it comes within a ranger's
    // shot of to draw at all; one `storeReach` or more from every store is
    // drawing from none, and gets the tiles beside it, a melee guard's chance.
    //
    // A trespasser on the exit row of a room beside one we mine is carried
    // into it next tick, and back if it is bouncing: keyed to that room, the
    // tiles beside where it lands, so the kill holds across the bounce.
    let trespassIn (open': string -> bool) =
        let haulGround = 2
        let storeReach = 5

        let hauls (hostile: HostileInfo) = not (HostileInfo.breaksLive hostile)

        let storesIn room =
            [ Structure BuiltKind.Container; Dropped Energy; Dropped Thorium; Tombstone ]
            |> List.collect (Atlas.idsOfKind atlas)
            |> List.choose (Atlas.positionOf atlas)
            |> List.filter (fun tile -> tile.Room = room)
            |> List.map RoomPos.pos

        let around (hostile: HostileInfo) : (string * Pos list) list =
            let room = hostile.Pos.Room
            let at = RoomPos.pos hostile.Pos

            if trespasser view hostile then
                let beside () = Atlas.adjacentWalkableIn atlas room at

                let tiles =
                    if hauls hostile then
                        match
                            storesIn room |> List.filter (fun store -> range store at < storeReach)
                        with
                        | [] -> beside ()
                        | near ->
                            let store = near |> List.minBy (fun store -> range store at, store)

                            Atlas.walkableWithinIn atlas room haulGround store
                            |> List.filter ((<>) store)
                    else
                        beside ()

                [ room, tiles ]
            elif Seam.onRing at && trespassingBody hostile then
                reachAcross atlas room at 1
                |> List.filter (fst >> trespassRooms view)
                |> List.map (fun (next, landings) ->
                    next, landings |> List.collect (Atlas.adjacentWalkableIn atlas next))
            else
                []

        view.Hostiles
        |> List.collect around
        |> List.filter (fst >> open')
        |> List.groupBy fst
        |> List.choose (fun (room, tiles) ->
            let standing =
                view.Hostiles
                |> List.filter (fun hostile -> hostile.Pos.Room = room)
                |> List.map (fun hostile -> RoomPos.pos hostile.Pos)
                |> Set.ofList

            let ground =
                tiles
                |> List.collect snd
                |> List.filter (fun tile -> not (Seam.onRing tile || Set.contains tile standing))
                |> Set.ofList

            if Set.isEmpty ground then
                None
            else
                Some(room, RoomPos.setAt room ground))

    // Each kind's claim, down `BriefKind.order`, on the rooms still open to
    // it: the first four claim their rooms outright, a held exit, a core and
    // a trespass only the rooms they lay ground in. Per room, its claimants
    // in that order, the first its kind; per kind, the ground it laid.
    let claimants, laid =
        let outright rooms open' =
            rooms |> Seq.filter open' |> Seq.map (fun room -> room, Set.empty) |> List.ofSeq

        let claimOf kind =
            match kind with
            | BriefKind.Resident -> outright resident
            | BriefKind.Harass -> outright (harassRooms view)
            | BriefKind.Chase -> outright chasedRooms
            | BriefKind.Outpost -> outright ring.Keys
            | BriefKind.ExitHold -> holdsIn
            | BriefKind.Core -> coresIn
            | BriefKind.Trespass -> trespassIn

        ((Map.empty, Map.empty), BriefKind.order)
        ||> List.fold (fun (claimants: Map<string, BriefKind list>, laid) kind ->
            let open' room =
                Map.tryFind room claimants
                |> Option.forall (List.forall (fun ahead -> BriefKind.opens ahead kind))

            let claims = claimOf kind open'

            let claimants =
                (claimants, claims)
                ||> List.fold (fun claimants (room, _) ->
                    Map.change
                        room
                        (fun prior -> Some(Option.defaultValue [] prior @ [ kind ]))
                        claimants)

            claimants, Map.add kind claims laid)

    let laidBy kind = Map.find kind laid

    // The raid's ring, and beside it each held exit's ground and each core's.
    let rings =
        (ring, laidBy BriefKind.ExitHold @ laidBy BriefKind.Core)
        ||> List.fold (fun ring (room, ground) ->
            Map.change
                room
                (fun prior -> Some(Set.union ground (Option.defaultValue Set.empty prior)))
                ring)

    let trespass = laidBy BriefKind.Trespass |> Map.ofList

    // Every room a source claimed or laid ground in; one no source claims is
    // the default kind's, `Outpost`.
    let briefs =
        [
            Map.keys claimants
            Map.keys rings
            Map.keys residentRing
            Map.keys harassRing
            Map.keys kite
            Map.keys trespass
        ]
        |> Seq.concat
        |> Set.ofSeq
        |> Seq.map (fun room ->
            room,
            Brief.claimed
                room
                (Map.tryFind room claimants |> Option.defaultValue [])
                {
                    Kite = Map.tryFind room kite
                    Resident = Map.tryFind room residentRing
                    Harass = Map.tryFind room harassRing
                    Ring = Map.tryFind room rings
                    Trespass = Map.tryFind room trespass
                })
        |> Map.ofSeq

    {
        Reach = reach
        Safe = safeGround
        Fight = Map.empty
        Assault = Map.empty
        AtLine = atLine
        DefendAvoid = defendAvoid
        Defend = defendGroundOf view atLine
        Shelter = shelter
        Briefs = briefs
    }

/// This tick's Threats as a colony holding nothing would see them: no Fight,
/// and an outmatched room's safe ground only for a ranger standing in it.
let threatsOf (view: ColonyView) atlas : Threats =
    threatsHeldBy view atlas HeldTaskFacts.empty

/// How near the crossing it waits beside the rally ground lies, in tiles:
/// room for a squad on the near side of the exit.
let private rallyReach = 3

/// How far from the leader a member may stand and still be with it.
let private squadSpread = 2

/// How close to a room's border a body in the room beside it stands at its
/// crossing: the exit tile and the one inside it. The rally ground keeps
/// clear of both, so a body there has left it.
let private crossingGap = 1

/// Tiles from a tile to the nearest edge of its room.
let private edgeGap (tile: Pos) =
    min (min tile.X (Engine.roomSide - 1 - tile.X)) (min tile.Y (Engine.roomSide - 1 - tile.Y))

/// The range between two tiles across a border too: a member a step behind
/// its leader over a crossing is beside it. The exit tiles either side of a
/// border are one tile, the engine carrying a body from one to the other, so
/// the rooms overlap by a row. None between rooms that share no border.
let internal rangeAcross (a: RoomPos) (b: RoomPos) : int option =
    if a.Room = b.Room then
        RoomPos.range a b
    else
        let span = Engine.roomSide - 1

        RoomName.offsetOf a.Room b.Room
        |> Option.filter (fun (dx, dy) -> abs dx + abs dy = 1)
        |> Option.map (fun (dx, dy) ->
            max (abs (b.X + span * dx - a.X)) (abs (b.Y + span * dy - a.Y)))

/// Whether a tile stands at the crossing into a room from the room beside it:
/// the room's own nearest tile, across the border, within a step of the exit.
let private atCrossingInto (room: string) (tile: RoomPos) =
    RoomName.offsetOf tile.Room room
    |> Option.filter (fun (dx, dy) -> abs dx + abs dy = 1)
    |> Option.exists (fun (dx, dy) ->
        let across value offset =
            let low = Engine.roomSide * offset
            max 0 (max (low - value) (value - (low + Engine.roomSide - 1)))

        max (across tile.X dx) (across tile.Y dy) <= crossingGap + 1)

/// The crossing a Fight's squad musters beside on a chain from home to its target:
/// out of the last room short of the target that is neither a Source Keeper's
/// nor a rival's — home, which is neither, at the last — toward the next room
/// on the chain. None for a chain of one room.
let rallyHop (rivals: Set<string>) (chain: string list) : (string * string) option =
    chain
    |> List.pairwise
    |> List.rev
    |> List.tryFind (fun (from, _) ->
        not (Keepers.isKeeperRoom from) && not (Set.contains from rivals))

/// The ground beside the middle of the crossing from one room into the next,
/// clear of the border and of the room's Reach.
let private besideCrossing atlas (threats: Threats) (from: string) (into: string) =
    match Atlas.seams atlas from into |> List.map fst with
    | [] -> Set.empty
    | exits ->
        let middle = List.item (List.length exits / 2) exits
        let reach = Threats.reachIn threats from

        Atlas.walkableWithinIn atlas from rallyReach middle
        |> List.filter (fun tile -> edgeGap tile > crossingGap && not (Set.contains tile reach))
        |> Set.ofList
        |> RoomPos.setAt from

/// A squad's rally ground on its chain from home (`rallyHop`); empty where no
/// chain reaches its room.
let private rallyOn (view: ColonyView) atlas (threats: Threats) (chain: string list option) =
    chain
    |> Option.bind (rallyHop view.Spatial.RivalRooms)
    |> Option.map (fun (from, into) -> besideCrossing atlas threats from into)
    |> Option.defaultValue Set.empty

/// An assault's rally ground: at home, beside the crossing out toward the
/// next room on its chain, off the idle ground (`Atlas.idleGroundIn`) — the
/// rings of the spawns, extensions and stores — where any of it is left.
/// Empty where no chain reaches its room.
let private musterAtHome atlas (threats: Threats) (chain: string list option) =
    match chain with
    | Some(home :: next :: _) ->
        let ground = besideCrossing atlas threats home next
        let idle = Atlas.idleGroundIn atlas home |> RoomPos.setAt home

        match Set.difference ground idle with
        | clear when Set.isEmpty clear -> ground
        | clear -> clear
    | _ -> Set.empty

/// A step onto a tile under these towers' fire, priced one plus a unit per
/// 50 hits of it: fine enough that a tile a range further off is cheaper,
/// and small enough that a walk across a room fits the flood's integers.
let private underFire (towers: Pos list) (tile: Pos) =
    1
    + (towers |> List.sumBy (fun tower -> Engine.towerAttackAt (range tile tower)))
      / 50

/// The way out of an assault's room for a squad falling back with casts
/// still in it (#492): the ground beside the landing of the one exit whose
/// walks from them cost least under the room's towers (`underFire`), into
/// any room beside it that is no rival's. Under the rival's safe mode our
/// heal is refused and its towers fire on,
/// so the walk out is what the squad takes, and the way to rally may lead
/// toward them; a squad out by a room with no chain home waits there alive.
/// Empty where no room beside it will do.
let private wayOut (view: ColonyView) atlas (facts: AssaultFacts) (inside: Pos list) =
    let room = facts.Assault.RoomName

    let walks =
        inside |> List.map (Atlas.pricedWalksFrom atlas room (underFire facts.Towers))

    // Every cast's cheapest walk onto the ground beside the exit, summed;
    // None where one of them has none.
    let priced (exit: Pos) =
        let beside = Atlas.adjacentWalkableIn atlas room exit

        let costs =
            walks
            |> List.map (fun walk ->
                match List.choose walk beside with
                | [] -> None
                | reached -> Some(List.min reached))

        // Past the falloff range every tile costs the same: on a tie, the exit
        // farther from the towers, then the straighter walk.
        let clear =
            match facts.Towers with
            | [] -> 0
            | towers -> towers |> List.map (range exit) |> List.min

        if List.forall Option.isSome costs then
            Some(
                List.sumBy Option.get costs,
                -clear,
                inside |> List.sumBy (fun tile -> abs (tile.X - exit.X) + abs (tile.Y - exit.Y))
            )
        else
            None

    RoomName.adjacent room
    |> List.filter (fun next ->
        not (Set.contains next view.Spatial.RivalRooms) && Keepers.enterable next)
    |> List.collect (fun next ->
        Atlas.seams atlas room next
        |> List.choose (fun (exit, landing) ->
            priced exit |> Option.map (fun price -> price, (next, landing))))
    |> function
        | [] -> Set.empty
        | exits ->
            let _, (next, landing) = List.minBy fst exits

            Atlas.adjacentWalkableIn atlas next landing
            |> List.map (RoomPos.at next)
            |> Set.ofList

/// A squad's casts, the front leading: a brawler, else a sapper or a probe,
/// else a kiter, else whoever holds.
let private frontFirst (members: (string * SquadRole) list) =
    members
    |> List.sortBy (fun (name, role) ->
        (match role with
         | Brawler -> 0
         | Sapper
         | Probe -> 1
         | Kiter -> 2
         | Medic -> 3),
        name)

/// Whether each cast stands on the rally ground or with the squad: the
/// others within two of the leader, and the leader within two of one of
/// them. A leader is never with the squad by standing on its own tile, so a
/// recast front far off the rally ground is not.
let private together (view: ColonyView) (rally: Set<RoomPos>) (members: (string * SquadRole) list) =
    let tileOf name =
        SpatialInfo.creepPlacementOf view.Spatial name

    let leader = frontFirst members |> List.tryHead |> Option.map fst

    members
    |> List.forall (fun (name, _) ->
        tileOf name
        |> Option.exists (fun tile ->
            Set.contains tile rally
            || members
               |> List.exists (fun (other, _) ->
                   other <> name
                   && (Some name = leader || Some other = leader)
                   && tileOf other
                      |> Option.bind (rangeAcross tile)
                      |> Option.exists (fun r -> r <= squadSpread))))

/// Whether the casts stand as one file (#491): each within two of another,
/// every one joined to the rest. A squad that walks single file off its
/// rally ground strings out further than two from its leader without
/// leaving one of its own behind.
let private linked (view: ColonyView) (members: (string * SquadRole) list) =
    let tiles =
        members
        |> List.choose (fun (name, _) -> SpatialInfo.creepPlacementOf view.Spatial name)

    let near (a: RoomPos) (b: RoomPos) =
        rangeAcross a b |> Option.exists (fun r -> r <= squadSpread)

    let rec grow (joined: RoomPos list) (rest: RoomPos list) =
        match rest |> List.partition (fun tile -> joined |> List.exists (near tile)) with
        | [], left -> List.isEmpty left
        | added, left -> grow (added @ joined) left

    match tiles with
    | first :: rest when List.length tiles = List.length members -> grow [ first ] rest
    | _ -> false

/// Whether every cast stands in the room or at its crossing: a front chasing
/// its target, or carried over the border off an exit tile, is not called
/// back to the rally ground.
let private entered (view: ColonyView) (room: string) (members: (string * SquadRole) list) =
    members
    |> List.forall (fun (name, _) ->
        SpatialInfo.creepPlacementOf view.Spatial name
        |> Option.exists (fun tile -> tile.Room = room || atCrossingInto room tile))

/// Every squad cast of this colony, by the role its name carries.
let private squadCasts (view: ColonyView) : Map<string, SquadRole> =
    view.Creeps
    |> List.choose (fun creep ->
        squadRoleByName creep.Name |> Option.map (fun role -> creep.Name, role))
    |> Map.ofList

/// The tick's Threats with each pooled Fight's ground on it
/// (`HeldTaskFacts.Fights`), its squad whoever holds the room's Fight. A
/// launched squad's room keeps its residents on the kite ground.
let private withFights (view: ColonyView) atlas (held: HeldTaskFacts) (threats: Threats) : Threats =
    if Map.isEmpty held.Fights then
        threats
    else
        let home = SpatialInfo.homeName view.Spatial
        let resident = residentRooms view
        let fighting = held.Fights |> Map.keys |> Set.ofSeq

        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let bodies =
            view.Creeps |> List.map (fun creep -> creep.Name, creep.Body) |> Map.ofList

        let casts = squadCasts view

        let rallyFor room =
            rallyOn view atlas threats (Atlas.route atlas home room)

        let groundOf room (squad: Squad) =
            let slots = squadRoles squad
            let tid = taskId (Fight room)

            // The casts holding the room's Fight, by role.
            let members =
                HeldTaskFacts.holdersOf held tid
                |> List.choose (fun name ->
                    Map.tryFind name casts |> Option.map (fun role -> name, role))
                |> List.sort

            // The room's residents whose parts fit one of its slots.
            let residents =
                HeldTaskFacts.holdersOf held (taskId (Guard room))
                @ HeldTaskFacts.holdersOf held tid
                |> List.filter (fun name -> not (Map.containsKey name casts))
                |> List.choose (fun name ->
                    Map.tryFind name bodies
                    |> Option.bind squadRoleOfParts
                    |> Option.filter (fun role -> List.contains role slots)
                    |> Option.map (fun role -> name, role))
                |> Map.ofList

            let inRoom = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
            let targets = inRoom |> List.filter (guardShoots view resident fighting)
            let head = killHead view room targets
            let rally = rallyFor room

            // Every slot filled, by a cast holding the Fight or a resident
            // that fits it.
            let filled =
                SquadRole.all
                |> List.forall (fun role ->
                    SquadRole.slots role (List.map snd members)
                    + (residents |> Map.filter (fun _ fits -> fits = role) |> Map.count)
                    >= SquadRole.slots role slots)

            let ranked = frontFirst members

            // Never with a slot empty: a squad that loses a member, or whose
            // replacement is on its way, waits on the rally ground until it is
            // whole again. Gone in, it is held there while every cast stands
            // in the room or at its crossing.
            let launched =
                not (Set.isEmpty rally)
                && not (List.isEmpty members)
                && filled
                && (together view rally members || entered view room members)

            // The tiles beside a tile no hostile stands on, in its room.
            let besideIn (tile: RoomPos) =
                let taken =
                    view.Hostiles
                    |> List.filter (fun h -> h.Pos.Room = tile.Room)
                    |> List.map (fun h -> RoomPos.pos h.Pos)
                    |> Set.ofList

                Atlas.adjacentWalkableIn atlas tile.Room (RoomPos.pos tile)
                |> List.filter (fun pos -> not (Set.contains pos taken))
                |> List.map (RoomPos.at tile.Room)

            // The head's ring, and the tile of every brawler of ours already
            // swinging at a target or lamed: a body that can no longer walk
            // is not walked off the fight, where its medic heals its legs back.
            let swinging =
                members
                |> List.filter (snd >> (=) Brawler)
                |> List.choose (fun (name, _) ->
                    tileOf name |> Option.map (fun tile -> tile, Map.tryFind name bodies))
                |> List.filter (fun (tile, body) ->
                    body |> Option.exists (fun parts -> partCount parts Move = 0)
                    || targets
                       |> List.exists (fun h ->
                           RoomPos.range tile h.Pos
                           |> Option.exists (fun r -> r <= Engine.meleeRange)))
                |> List.map fst
                |> Set.ofList

            // With no target in the room, its resident ring — the
            // controller's of a raised home, the Reactor's of an errand room
            // — which the squad holds while the Fight stays pooled.
            let front =
                match head with
                | Some target -> besideIn target.Pos |> Set.ofList |> Set.union swinging
                | None ->
                    Threats.residentRingIn threats room
                    |> Option.defaultWith (fun () -> Threats.ringIn threats room)

            // Behind the leader, out of every melee body's swing where the
            // ground allows: the raid then strikes the brawler, whose ATTACK
            // strikes back, and not the medic.
            let behind =
                // The member a medic walks behind: the leading cast that is no
                // medic.
                match ranked |> List.tryFind (snd >> (<>) Medic) |> Option.bind (fst >> tileOf) with
                | Some tile ->
                    // Clear of every melee body by `reach`: past a step and a
                    // swing first, so one that closes this tick cannot strike
                    // the next, then past a swing.
                    let clearBy reach (pos: RoomPos) =
                        view.Hostiles
                        |> List.forall (fun h ->
                            not (HostileInfo.meleeLive h)
                            || RoomPos.range pos h.Pos |> Option.forall (fun r -> r > reach))

                    let around = besideIn tile

                    [ Engine.meleeRange + 1; Engine.meleeRange ]
                    |> List.tryPick (fun reach ->
                        match around |> List.filter (clearBy reach) with
                        | [] -> None
                        | clear -> Some(Set.ofList clear))
                    |> Option.defaultValue (Set.ofList around)
                // No one to walk behind: never the front.
                | None -> rally

            let kite =
                match kiteGroundIn atlas room inRoom head with
                | tiles when Set.isEmpty tiles ->
                    (Threats.briefOf threats room).Laid.Kite |> Option.defaultValue front
                | tiles -> tiles

            let launchedOr ground = if launched then ground else rally

            {
                Squad = squad
                Launched = launched
                Rally = rally
                Front = launchedOr front
                Behind = launchedOr behind
                Ranged = launchedOr kite
                Roles =
                    if launched then
                        Map.fold (fun roles name role -> Map.add name role roles) casts residents
                    else
                        casts
                Residents = residents
            }

        let grounds = held.Fights |> Map.map groundOf

        // A launched squad's residents that fit no slot hold the kite ground
        // beside it, as its kiters do, and never the safe set.
        let briefs =
            (threats.Briefs, grounds)
            ||> Map.fold (fun briefs room ground ->
                if ground.Launched && not (Set.isEmpty ground.Ranged) then
                    Map.add
                        room
                        (Brief.kitingOn ground.Ranged (Threats.briefOf threats room))
                        briefs
                else
                    briefs)

        { threats with
            Fight = grounds
            Briefs = briefs
        }

/// The share of its hits under which a launched assault's cast sends its
/// squad back to rally, and the share every cast must hold to launch from
/// there: the gap is what its medics heal at rally, so a squad that turned
/// back at one does not walk straight back in.
let private assaultRetreatShare = 0.5

let private assaultLaunchShare = 0.8

/// Whether an assault is pooled this tick (#491): a `Provoke` whenever it
/// runs; a launched `Strike` until its window closes; one not launched only
/// inside a window that outlasts its squad's casts, in one oven, and its
/// walk from home, a room's side a crossing.
let internal assaultPooled (view: ColonyView) atlas (launched: bool) (facts: AssaultFacts) : bool =
    match facts.Assault.Mode with
    | Provoke -> true
    | Strike when launched -> facts.BarredUntil |> Option.exists (fun until -> until > view.Time)
    | Strike ->
        let cast =
            facts.Assault.Squad
            |> List.choose squadRoleOfRow
            |> List.sumBy (fun role -> List.length (squadPatternOf role).Block)

        match
            facts.BarredUntil,
            Atlas.siegeRoute atlas (SpatialInfo.homeName view.Spatial) facts.Assault.RoomName
        with
        | Some until, Some chain ->
            until > view.Time
                    + Engine.spawnTicksPerPart * cast
                    + Engine.roomSide * (List.length chain - 1)
        | _ -> false

/// The tick's Threats with each pooled Assault's ground on it
/// (`ColonyView.Assaults`, `assaultPooled`): a Fight's rally and launch over
/// the casts holding it, kept whole on the walk in.
let private withAssaults
    (view: ColonyView)
    atlas
    (held: HeldTaskFacts)
    (threats: Threats)
    : Threats =
    // Grounded while a window may be open at all; `assaultPooled` keeps those
    // its launch and its margin allow.
    match view.Assaults |> List.filter (assaultPooled view atlas true) with
    | [] -> threats
    | assaults ->
        let home = SpatialInfo.homeName view.Spatial
        let casts = squadCasts view

        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let share =
            view.Creeps
            |> List.map (fun creep ->
                creep.Name, float creep.Hits.Hits / float (max 1 creep.Hits.HitsMax))
            |> Map.ofList

        // The walkable tiles beside a tile, in its room.
        let besideIn (tile: RoomPos) =
            Atlas.adjacentWalkableIn atlas tile.Room (RoomPos.pos tile)
            |> List.map (RoomPos.at tile.Room)
            |> Set.ofList

        let groundOf (facts: AssaultFacts) =
            let room = facts.Assault.RoomName
            let slots = facts.Assault.Squad |> List.choose squadRoleOfRow

            // The casts holding the room's Assault, by role.
            let members =
                HeldTaskFacts.holdersOf held (taskId (Assault room))
                |> List.choose (fun name ->
                    Map.tryFind name casts |> Option.map (fun role -> name, role))
                |> List.sort

            let rally = musterAtHome atlas threats (Atlas.siegeRoute atlas home room)

            let filled =
                SquadRole.all
                |> List.forall (fun role ->
                    SquadRole.slots role (List.map snd members) >= SquadRole.slots role slots)

            let inside = entered view room members

            // Every cast out of home: on the walk it launched on, however the
            // file strings out over the rooms between; a recast at home calls
            // it back.
            let away =
                members
                |> List.forall (fun (name, _) ->
                    tileOf name |> Option.exists (fun tile -> tile.Room <> home))

            // Hurt: under half once in, so the squad turns back; under four
            // fifths at rally, so it waits for its medics. A probe has none
            // to wait for (#493): it holds on until safe mode or its death.
            let fit =
                let least = if inside then assaultRetreatShare else assaultLaunchShare

                members
                |> List.forall (fun (name, role) ->
                    role = Probe || Map.tryFind name share |> Option.exists (fun s -> s >= least))

            let launched =
                not (Set.isEmpty rally)
                && not (List.isEmpty members)
                && filled
                && fit
                && not facts.SafeMode
                && not facts.Taken
                && (together view rally members || inside || linked view members || away)

            let breach = facts.Assault.Breach |> List.map (RoomPos.at room)

            // The target: vision's head, else the first declared breach tile
            // while the room is dark.
            let target =
                match facts.Targets with
                | Some targets ->
                    targets
                    |> List.tryHead
                    |> Option.map (fun (id, tile) -> id, RoomPos.at room tile)
                | None -> None

            let front =
                match target, facts.Targets with
                | Some(_, tile), _ -> besideIn tile
                | None, None ->
                    breach |> List.tryHead |> Option.map besideIn |> Option.defaultValue Set.empty
                // Nothing left standing: a `Provoke` holds the breach it made,
                // and a `Strike` the last one too.
                | None, Some _ ->
                    breach
                    |> List.tryLast
                    |> Option.map (fun tile -> Set.add tile (besideIn tile))
                    |> Option.defaultValue Set.empty

            let leader =
                frontFirst members
                |> List.tryFind (snd >> (<>) Medic)
                |> Option.bind (fun (name, _) -> tileOf name |> Option.map (fun tile -> name, tile))

            let held = members |> List.choose (fun (name, _) -> tileOf name) |> Set.ofList

            // Whether a cast this far from the leader has a step nearer it:
            // onto a free tile off the border and off the front, which is
            // the sappers'. In a one-tile corridor the file's tail has none,
            // and holding for it holds for good (live t930,290). Across a
            // border it is still walking in.
            let closes (at: RoomPos) (tile: RoomPos) =
                tile.Room <> at.Room
                || besideIn tile
                   |> Set.exists (fun step ->
                       edgeGap (RoomPos.pos step) > 0
                       && not (Set.contains step held)
                       && not (Set.contains step front)
                       && RoomPos.range step at < RoomPos.range tile at)

            // Strung out under fire, the squad's medics cannot reach the body
            // the towers pick: every cast within two of the leader, or the
            // leader holds its tile and the rest close on it: at home, in the
            // room or at its crossing — never on the front, where it works,
            // and only for a cast with a step nearer it. In the rooms between
            // it holds only for a broken file: a one-tile corridor fits no
            // four within two of one.
            let strung =
                match leader with
                | Some(_, at) when Set.contains at front -> false
                | Some(name, at) when at.Room = home || at.Room = room || atCrossingInto room at ->
                    members
                    |> List.exists (fun (other, _) ->
                        other <> name
                        && tileOf other
                           |> Option.forall (fun tile ->
                               rangeAcross at tile
                               |> Option.forall (fun r -> r > squadSpread && closes at tile)))
                | Some _ -> not (linked view members)
                | None -> false

            let front =
                match leader with
                | Some(_, at) when strung -> Set.add at (besideIn at)
                | _ -> front

            // Beside the leading sapper, off the front it works from; and
            // while the leader is short of the front, beside the front and
            // the leader's own tile too: a medic the file put between the two
            // trades places with the leader for nothing and steps through to
            // the far side of the target, rather than standing in its way.
            let behind =
                match leader with
                | Some(_, tile) ->
                    let besideFront =
                        if strung || Set.contains tile front then
                            Set.empty
                        else
                            front
                            |> Seq.collect besideIn
                            |> Seq.filter (fun step -> edgeGap (RoomPos.pos step) > 0)
                            |> Set.ofSeq
                            |> Set.add tile

                    match Set.difference (Set.union (besideIn tile) besideFront) front with
                    | clear when Set.isEmpty clear -> besideIn tile
                    | clear -> clear
                | None -> rally

            let inRoom =
                members
                |> List.choose (fun (name, _) -> tileOf name)
                |> List.filter (fun tile -> tile.Room = room)

            // Falling back with casts still in the room: out of it first,
            // then to rally.
            let back =
                if launched then
                    rally
                else
                    match inRoom with
                    | [] -> rally
                    | inside ->
                        match wayOut view atlas facts (List.map RoomPos.pos inside) with
                        | out when Set.isEmpty out -> rally
                        | out -> out

            let launchedOr ground = if launched then ground else back

            {
                Slots = slots
                Launched = launched
                Rally = rally
                Target = target
                Front = launchedOr front
                Behind = launchedOr behind
                Roles = casts |> Map.filter (fun _ role -> List.contains role slots)
            },
            // Its targets down (#496): its work is done once its casts are out.
            not facts.Taken || not (List.isEmpty inRoom)

        { threats with
            Assault =
                assaults
                |> List.map (fun facts -> facts, groundOf facts)
                |> List.filter (fun (facts, (ground, working)) ->
                    working && assaultPooled view atlas ground.Launched facts)
                |> List.map (fun (facts, (ground, _)) -> facts, ground)
                |> List.map (fun (facts, ground) -> facts.Assault.RoomName, ground)
                |> Map.ofList
        }

/// The tick's Threats off what the colony holds (`heldTaskFacts`): an
/// outmatched room's safe ground for every ranger holding its Guard, wherever
/// it stands, each pooled Fight's ground and each Assault's.
let threatsOfHeld (view: ColonyView) atlas (held: HeldTaskFacts) : Threats =
    threatsHeldBy view atlas held
    |> withFights view atlas held
    |> withAssaults view atlas held
