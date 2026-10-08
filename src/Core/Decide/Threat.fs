/// The tick's threat facts: the tiles a hostile can reach, layered by room
/// name. Colony facts, never a change to the spatial projection.
[<AutoOpen>]
module Fabot.Core.Decide.Threat

open Fabot.Core
open Fabot.Core.Types

/// An `Assault`'s squad ground this tick (#490), off its casts holding it:
/// the rally ground until it launches, each role's ground after, launched
/// through `Formation`.
type Formation =
    {
        /// The roles its squad casts: its declared rows'.
        Slots: SquadRole list
        /// Derived each tick from where the casts stand, never stored
        /// (`Formation.launched`): every slot filled, and each cast on the
        /// rally ground or with the squad — within two of the leader, the
        /// leader within two of another — or, once in, every cast standing in
        /// the room or at its crossing. Never without a rally ground. Also
        /// holds while its casts stand as one file or are all out of home,
        /// and never while one of them is hurt — under half its hits once
        /// in, under four fifths before — or while safe mode runs in the
        /// room.
        Launched: bool
        /// At home, beside the exit its chain into the rival's room leaves by
        /// (`Formation.musterAtHome`). Empty where no chain reaches the room.
        Rally: Set<RoomPos>
        /// What its sappers take down this tick (`AssaultFacts.Targets`'
        /// head), None while the room is dark or nothing is left.
        Target: (string * RoomPos) option
        /// A sapper's and a probe's ground: the rally ground — the way out
        /// (`wayOut`) first while a cast of a squad not launched stands in
        /// the room — or, launched, the tiles beside its target: beside the
        /// first declared breach tile while the room is dark, and beside the
        /// last once a `Provoke`'s breach has fallen.
        Front: Set<RoomPos>
        /// A medic's: the rally ground, or, launched, the tiles beside the
        /// leading sapper that are not the front.
        Behind: Set<RoomPos>
        /// The bodies it admits, by name and role: the casts of its slots.
        Roles: Map<string, SquadRole>
    }

/// Which flavour one room's `guard:<room>` Task is this tick: classified once
/// (`threatsHeldBy`), and read in place of the room's class. One kind a room,
/// the first in `BriefKind.order` whose claim pools a Task this tick.
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

/// Which combat Task a `Brief` describes: a room's Guard, of its flavour, or
/// a Task of its own — the home's Defend, an Assault.
[<RequireQualifiedAccess>]
type Duty =
    | Guard of BriefKind
    | Defend
    | Assault

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BriefKind =
    /// The order a room's flavour is read in, written once: a resident room,
    /// a [[harassment room]], a chase (#510), a raid's ring, a held exit
    /// (#450), a trespass's kill (#516), a level-0 core (#487): a creep that
    /// can step away is struck before a core that stands, which takes the room
    /// back once the creep is gone (#558). The order
    /// decides only among claims that pool a Task this tick (#542): the first
    /// of those is the room's kind, and a claim that pools nothing shuts no
    /// kind out. A kind claims only a room still open to it (`opens`), so a
    /// held exit, a trespass and a core lay no ground under a pooling kind
    /// ahead of them (`threatsHeldBy`); and a Guard's ground is the first laid
    /// in its room from its kind down (`Brief.Ground`).
    let order =
        [
            BriefKind.Resident
            BriefKind.Harass
            BriefKind.Chase
            BriefKind.Outpost
            BriefKind.ExitHold
            BriefKind.Trespass
            BriefKind.Core
        ]

    /// The kinds whose ground is the room's `Ring`.
    let laysRing (kind: BriefKind) =
        match kind with
        | BriefKind.Outpost
        | BriefKind.ExitHold
        | BriefKind.Core -> true
        | _ -> false

    /// Whether a room a pooling kind ahead claims is still open to `kind`:
    /// only a ring kind under a chase. Two halves. Load-bearing: a chase in sight is
    /// always a raid's room, so the raid still claims it as `Outpost`, and
    /// that claim shuts the held exit, the core and the trespass out of it.
    /// Quirk, kept (the Brief's S0): out of sight no raid claims it, so a held
    /// exit or a core lays its ring there, and the chase, with no kite
    /// ground, takes that melee guard's ring.
    let opens (ahead: BriefKind) (kind: BriefKind) =
        ahead = BriefKind.Chase && laysRing kind

    /// A kill (#510, #516): one body, a rung up, and a quiet holder called to
    /// it; a chase cast for by no row, a trespass only where its cast pays
    /// (#652).
    let isKill (kind: BriefKind) =
        match kind with
        | BriefKind.Chase
        | BriefKind.Trespass -> true
        | _ -> false

/// The rungs a Task may be stepped by inside its tier, a union rather than an
/// int for the reason `Tier` is one (#237): the Resolver's `weightOfRank` has
/// to round every one of them back onto the tier's own push weight, and the
/// test that checks that walks the cases off the union. Every rung but one
/// steps a Task up; `OneRungDown` holds an outpost's road site under its tier
/// while the home builds (#548).
type Rung =
    | OneRungDown
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
    /// The kite (#510, movement S3), for a holder that outranges a swing
    /// (`Fighter.outranges`, #615 U1; a melee holder walks as a runner and
    /// glues): the tiles of the room it stands in that a class it stands off
    /// strikes before this body, its own legs and fatigue counted, could be
    /// on them and gone (`Threats.kiteUnsafeFor`), rather than the whole
    /// Reach, so the ring it shoots from stays open to it; and about the exit
    /// a raid left by, where a body camping across lands beside it. Across
    /// the border too.
    | Kite
    /// A [[defender]]'s (#504): the tiles outside the home's line a raid
    /// melee there reaches in a step and a swing, where every other runner
    /// walks round the Reach.
    | Line of Set<Pos>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CastRow =
    /// The row's own cut over a name and parts: who stands in it.
    let internal cut (row: CastRow) (name: string) (parts: Map<BodyPart, int>) : bool =
        match row with
        | CastRow.Guard -> isGuardRowCut name parts
        | CastRow.Ranger -> isRangerRowCut name parts
        | CastRow.Defender -> isNamedFor defenderPattern name
        | CastRow.Squad role -> squadRoleByName name = Some role

/// What a Guard's holder shoots in its room, and in what kill order, read
/// off the colony that built it.
type Selector =
    {
        /// Whether it shoots a hostile (`Facts.guardShoots`): its resident
        /// rooms and the rooms whose fight it holds, then the hostile.
        Shoots: Set<string> -> Set<string> -> HostileInfo -> bool
        /// The kill order among the shots in a room, its head first: by
        /// `Facts.killRank`, the bodies that strike nothing back by the
        /// holder's kill score on each beside its partners (#621), then by
        /// our damage's focus, the holder's walk to each, then by id. The
        /// room, the holder (None prices the side's best, a ranger block's
        /// where none holds it), the walk and the shots; each rank priced
        /// once. What a
        /// holder strikes is `Emitter.pickStrike`'s choice off it.
        Order:
            string
                -> CreepInfo option
                -> (HostileInfo -> int)
                -> HostileInfo list
                -> HostileInfo list
        /// Each shot's band of that order (`Facts.killBandOf`), first band
        /// first, the room applied once: a mass attack counts only what it
        /// deals the band of the shot it would replace (#563).
        Band: string -> HostileInfo -> int
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
        /// there, the kill order's head is a claimer, or in a resident room an
        /// unarmed head its kill score prices worth the kill (#648), the kite
        /// ground — the
        /// tiles within three of that head, less every tile a melee body
        /// reaches in a step and a swing, and less swamp near one. With a
        /// melee body there and that empty, the tiles farthest from it, never
        /// the threats' ring beside it.
        Kite: Set<RoomPos> option
        /// With a kite ground laid about the kill order's head, the tiles one
        /// to three off it that a kiter with no safe walk onto it closes on
        /// (#595): off the exits and the hostiles' tiles, a melee arriving
        /// later than the tick after, with escape room at least half the
        /// ground's widest, the ground's own tests. Empty with any other
        /// ground.
        Approach: Lazy<Set<RoomPos>>
        /// In a resident room (`Facts.residentRooms`), the garrison's ground,
        /// which it holds and shoots from: in a declared errand room the
        /// Reactor's own ring (#414, #411), in a raised child's home its
        /// controller's while no armed Threat stands there (#447).
        Resident: Set<RoomPos> option
        /// In a [[harassment room]] (#432), the ranger's: an ambush (#439) —
        /// beside every armed target, and only there while a holder fits its
        /// fight (#611); the walkable tiles within ranged reach of the enemy
        /// creep on a work spot, or caught off one, that the side's kill
        /// score leads (#621); and the declared `Stand`'s seats while
        /// neither stands there.
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
/// One combat Task this tick — a room's `guard:<room>`, the home's Defend or
/// an Assault —: its flavour and every answer a reader asks of it, derived
/// each tick (`threatsHeldBy`, `withAssaults`) and never
/// stored. A Task no source claims reads `Brief.unclaimedFor`. Equal only to
/// itself: its answers are functions, each closed over the colony view and
/// the Atlas that built it, so it lives no longer than the tick.
[<ReferenceEquality>]
type Brief =
    {
        Duty: Duty
        /// The home of the colony whose view built it. Not `Home`: a record
        /// literal with that label would read as this type.
        BuiltBy: string
        /// A Guard's: every kind that claimed the room, down
        /// `BriefKind.order`, those that pool first (#542), its own first. A
        /// held exit (#450) among them is where the Planner keeps a living
        /// guard's Guard pooled, and hires for none. None for a Task of its own.
        Claims: BriefKind list
        /// What its holder shoots (`Emitter.guardTarget`); a defender, the
        /// raid at the line, fewest hits first (`Emitter.defendIntent`).
        Targets: Selector
        /// Whether a body may take it: the Emitter's gate. The walk to the
        /// ground is what travel cost prices.
        Admits: CreepInfo -> bool
        /// How a body ends the raid it is fought against alone, its walk's
        /// ticks and its kill's (`Facts.endsIn`, #596): what the world lane
        /// seats its soonest done by. None where no raider stands, and for
        /// an Assault, which is not priced so.
        Ends: CreepInfo -> (int * int) option
        /// For a kill, how far a holder of another room's Guard is called off
        /// it (the Pool's `RecallPolicy.Kill`).
        Recall: Recall option
        Laid: Laid
        /// Its holder's ground: the first laid in the room from its kind down
        /// `BriefKind.order`. A ranger's kind and a chase hold their kite
        /// ground (#451) first, so a melee guard's kind is never handed it; a
        /// ranger's declared ground next; a ring kind the room's `Ring`; a
        /// trespass its own (#516); with none laid, the room's declared source
        /// ring (#366). None where none is. A Defend's, the perimeter ground
        /// (`defendGroundOf`); a squad's is per role, on its `Formation`.
        Ground: Set<RoomPos> option
        /// With no ground and no source placed, whether the walk in is to the
        /// room's ground off the border: a chased room shut on its raid (#510).
        WalksIn: bool
        Avoid: Avoid
        Rung: Rung
        /// What it asks the fighting rows for: one ask a Guard's room and a
        /// Defend, one a role a squad; none for a chase (#510), which no row
        /// casts for.
        Asks: RowAsk list
        /// How many bodies it admits, off the row's own count, the tick's
        /// Threats given.
        Capacity: Threats -> Capacity
        /// The room's verdict (`Facts.answer`), priced once a tick on first
        /// read; `Win` in a room no raider stands in, and for an Assault,
        /// whose answer is its safe-mode window.
        Verdict: Lazy<Answer>
        /// An Assault's squad ground, role by role; None for every
        /// single-body kind.
        Formation: Formation option
        /// A Guard's holders held off `Ground` this tick, each on its own:
        /// the muster ground while the holders that do not win alone wait
        /// for the rest (`withMusters`, #535). The home's Defend's: each body
        /// of the rows it asks on its own row's ground (#546). Empty for every
        /// other Task.
        Stances: Map<string, Set<RoomPos>>
        /// A Guard's holders seated on its ground (`Brief.seatsAmong`, #650):
        /// each holder's own ground less every other holder's seat, where
        /// that costs it no tick. Its own
        /// is a ranged holder of a level-0 core's Guard the core's range-3
        /// ring, out of the swing of a melee body breaking the core (#558);
        /// a melee holder of a chase the ring beside the raid (#571); a
        /// trespass's holder the tiles of the ground within its reach of the
        /// body its kill score leads, where it reaches one (#621); every
        /// other `Ground`. Read after `Stances`, priced on first read. Empty
        /// for every other Task.
        Seats: Lazy<Map<string, Set<RoomPos>>>
    }

/// What one Brief asks a row for: the row, how many bodies, and the blocks
/// each must carry to win the room alone. The guard and ranger rows sum the
/// Guards' over the guarded rooms (`OutpostFacts.Guarded`), so a held exit's
/// ask is read only where its room is guarded too, and a trespass's
/// elsewhere only priced (`Prey`, #652); the defender and squad rows every
/// other Brief's.
and [<NoEquality; NoComparison>] RowAsk =
    {
        Row: CastRow
        /// How many bodies, the tick's Threats given.
        Wanted: Threats -> int
        Blocks: unit -> int
        /// A trespass's kill's: each trespasser the row answers
        /// (`trespassSplit`) by id, with its kill value (`Facts.killValue`),
        /// which prices its cast outside the guarded rooms (#652). None for
        /// every other ask.
        Prey: (string * float) list option
    }

/// The Brief of each combat Task no source claims, read off one colony's
/// view and Atlas; None for a Task no Brief describes. Equal only to itself.
and [<ReferenceEquality>] Unclaimed = { BriefOf: Task -> Brief option }

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
        /// Under the home, while an armed Threat stands in a room beside it
        /// or a raid this colony stands down from there is out of sight, the
        /// ground every idle body at home but a Fighter walks in to and stays
        /// on (#514): the walkable tiles behind the declared perimeter, else
        /// those five or more from every exit, less the home's Reach and the
        /// tiles beside a spawn (`shelterOf`). None under the
        /// home's safe mode. `Lazy` for `Safe`'s reason: most raid ticks no
        /// body idles at home.
        Shelter: Map<string, Lazy<Set<RoomPos>>>
        /// Per combat Task some source claims, its `Brief`: a Guard per room a
        /// source claims or lays ground in, the home's Defend while a raid
        /// stands at its line (`Facts.raidAtLine`, #504), and each pooled
        /// Assault (`threatsOfHeld`). Keyed by the Task, so the
        /// home's Defend and the Guard a raid in the home claims are two.
        Briefs: Map<Task, Brief>
        /// A combat Task's Brief where no source claims it
        /// (`Brief.unclaimedFor`), on the view and Atlas these Threats were
        /// derived off.
        Unclaimed: Unclaimed
        /// The rooms a hostile stands in whose Guard a living body of ours
        /// holds (#535): a fight begun there is fought to its target's death,
        /// so a cripple or a healer left there is still its target.
        Engaged: Set<string>
        /// Per room with a danger (`WorldDanger`), its hostile maps and their
        /// members priced (`Arrival.strikersOf`, on first read): what the
        /// Reach, the damage gate, Flee and the walk round read.
        Arrivals: Map<string, ArrivalMaps * Lazy<Striker list>>
        /// Per room with a danger, our standing ramparts' tiles as the
        /// world's danger reads them (`RoomDanger.Ramparts`): the tiles the
        /// Reach, the walk round, the kite and Flee all count as unstruck,
        /// whatever this colony's cut of the room keeps (`rampartsIn`).
        Ramparts: Map<string, Set<Pos>>
        /// The rooms vision answers for this tick (`WorldView.sees`): an
        /// unseen one is no room to flee into (#511, Decided 2).
        Seen: Set<string>
        /// The colony's bodies this tick, whose walks `Threats.bodyWalkOf`
        /// floods.
        Bodies: CreepInfo list
        /// The reach horizon the Reach was laid at (`Tuning.ReachHorizon`).
        Horizon: int
        /// The homes of ours the colony knows (`Facts.homesOfOurs`): a body
        /// caught in one flees in it, never off home to its own (#602).
        Homes: Set<string>
    }

/// The quiet tick's Briefs: none. One value, so two quiet ticks are equal.
let private briefsNothing: Unclaimed = { BriefOf = fun _ -> None }

/// The tick with nothing to run from: what a test hands the Planner for a
/// quiet colony. No view derived it, so no Task is briefed. A function, like
/// an Atlas fixture (#310): every caller gets its own.
let noThreats () =
    {
        Reach = Map.empty
        Safe = Map.empty
        Shelter = Map.empty
        Briefs = Map.empty
        Unclaimed = briefsNothing
        Engaged = Set.empty
        Arrivals = Map.empty
        Ramparts = Map.empty
        Seen = Set.empty
        Bodies = []
        Horizon = Tuning.defaults.ReachHorizon
        Homes = Set.empty
    }

/// Whether two Threats are one tick's same inputs: every field the same
/// value, by reference, as a `with` copy keeps those it does not set. The
/// Briefs come first, being what such a copy sets (`withAssaults`,
/// `withMusters`, the world's fight). A miss here only re-derives.
let private sameInputs (a: Threats) (b: Threats) =
    obj.ReferenceEquals(a.Briefs, b.Briefs)
    && obj.ReferenceEquals(a.Arrivals, b.Arrivals)
    && obj.ReferenceEquals(a.Reach, b.Reach)
    && obj.ReferenceEquals(a.Safe, b.Safe)
    && obj.ReferenceEquals(a.Shelter, b.Shelter)
    && obj.ReferenceEquals(a.Unclaimed, b.Unclaimed)
    && obj.ReferenceEquals(a.Engaged, b.Engaged)
    && obj.ReferenceEquals(a.Ramparts, b.Ramparts)
    && obj.ReferenceEquals(a.Seen, b.Seen)
    && obj.ReferenceEquals(a.Bodies, b.Bodies)
    && obj.ReferenceEquals(a.Homes, b.Homes)
    && a.Horizon = b.Horizon

/// Each body's Flee ground (`Threats.fleeAreaFor`) and its travel cost to
/// it (`Pool.travelCostOf`), laid on first read this tick: the Matcher and
/// the verdicts ask them again and again.
type private Flights =
    {
        Grounds: System.Collections.Generic.Dictionary<string, Set<RoomPos>>
        Costs: System.Collections.Generic.Dictionary<string, int option>
    }

/// Our bodies' own arrival (`Arrival.ofBody`), by the Atlas they are
/// flooded over: each Atlas is one colony's one view, whose bodies those
/// are, and no Threats field enters the flood.
let private walksMemo
    : TickMemo.Memo<
          Atlas.Atlas,
          System.Collections.Generic.Dictionary<string, Arrival.BodyArrival option>
       > =
    TickMemo.create 32 TickMemo.byReference

/// One body's own arrival over the room it stands in, flooded at most once a
/// tick over its Atlas (`walksMemo`); None for a body the projection does not
/// place.
let private bodyWalk atlas (creep: CreepInfo) : Arrival.BodyArrival option =
    let walks =
        TickMemo.recall walksMemo atlas (fun _ -> System.Collections.Generic.Dictionary())

    match walks.TryGetValue creep.Name with
    | true, walk -> walk
    | _ ->
        let walk = Arrival.ofBody atlas creep
        walks.[creep.Name] <- walk
        walk

/// A body of ours' kill score on each hostile of one room (#621,
/// `Facts.killScoresIn`, the room's `scores`): its fight at its hits, and its
/// arrival within its reach of the hostile, over its own walk (`bodyWalk`)
/// where it stands in that room, else its tiles to that reach at its pace on
/// plain.
let private scoreOf
    atlas
    (scores: Fighter -> RoomPos option -> int -> HostileInfo -> float)
    (creep: CreepInfo)
    : HostileInfo -> float =
    let fighter = Fighter.ofCreep creep
    let from = Atlas.creepTile atlas creep.Name
    let pace = Gait.ticks Gait.plain fighter.Gait |> Option.defaultValue 1

    let straight (hostile: HostileInfo) =
        from
        |> Option.bind (fun tile -> RoomName.tileRange tile hostile.Pos)
        |> Option.map (fun tiles -> max 0 (tiles - fighter.Reach) * pace)
        |> Option.defaultValue 0

    let arrival (hostile: HostileInfo) =
        match bodyWalk atlas creep with
        | Some walk when walk.Room = hostile.Pos.Room ->
            let centre = RoomPos.pos hostile.Pos
            let reach = fighter.Reach
            let mutable soonest = System.Int32.MaxValue

            for x in centre.X - reach .. centre.X + reach do
                for y in centre.Y - reach .. centre.Y + reach do
                    let tile = { X = x; Y = y }

                    if inGrid tile then
                        soonest <- min soonest (Grid.at (indexOf tile) walk.Ticks)

            if soonest = System.Int32.MaxValue then
                straight hostile
            else
                soonest
        | _ -> straight hostile

    if fighter.Reach = 0 then
        fun _ -> 0.0
    else
        fun hostile -> scores fighter from (arrival hostile) hostile

/// The kill scores of one room for a side of ours (#621,
/// `Facts.killScoresIn`): each kill priced at the damage of every body of
/// `side` together, each body's arrival its own.
let private sideScoresIn (view: ColonyView) (side: CreepInfo list) (room: string) =
    Facts.killScoresIn view room (side |> List.map Fighter.ofCreep)

/// The kill score a room's side prices each hostile at (#621): the best of
/// these bodies' (`scoreOf`), each beside the whole `side`, or with none a
/// ranger block's from where it stands, the row that strikes any body it
/// does not fall behind.
let private scoreAmong
    (view: ColonyView)
    atlas
    (side: CreepInfo list)
    (bodies: CreepInfo list)
    (room: string)
    =
    let scores = sideScoresIn view side room

    match bodies |> List.map (scoreOf atlas scores) with
    | [] -> scores (Facts.blockFighter rangerPattern) None 0
    | each -> fun hostile -> each |> List.fold (fun best score -> max best (score hostile)) 0.0

/// The Flights by every input they are read off: the Threats' fields
/// (`sameInputs`) and the Atlas, which tells one colony's from another's
/// where both Threats are quiet and share every empty field.
let private flightsMemo: TickMemo.Memo<Threats * Atlas.Atlas, Flights> =
    TickMemo.create 32 (fun (threats, atlas) (threats', atlas') ->
        obj.ReferenceEquals(atlas, atlas') && sameInputs threats threats')

let private flightsOf (threats: Threats) (atlas: Atlas.Atlas) : Flights =
    TickMemo.recall flightsMemo (threats, atlas) (fun _ ->
        {
            Grounds = System.Collections.Generic.Dictionary()
            Costs = System.Collections.Generic.Dictionary()
        })

/// ADR-0056
/// How many guards one guarded room wants: one, two where one block loses
/// the exchange. The count reads the raid and never our own answer to it
/// (#272): priced against the guards standing in the room it was not monotone,
/// and the reinforcement it bought was evicted on arrival. Vision, and the raid
/// last seen through the blind ticks since (`ColonyView.RecalledRaiders`,
/// #569), is the whole of what this reads. A room remembered as threatened
/// with no raid recalled is one guard (#366): a room with no `RoomControl`
/// entry contributes no hostile of its own to the view, so neither number of
/// the two-guard clause exists. Written as its own clause rather than left to
/// `blocksBeat`'s zero-damage arm: that one is a statement about a
/// visible raid dealing nothing.
let internal guardsWanted (view: ColonyView) (room: string) : int =
    if
        Set.contains room view.ThreatenedOutposts
        && not (Map.containsKey room view.RoomControl)
        && not (view.RecalledRaiders |> List.exists (fun h -> h.Pos.Room = room))
    then
        1
    elif blocksBeat guardPattern Stance.Hold view room 1 then
        1
    else
        Engine.guardCap

/// ADR-0072
/// The whole guard blocks one raided room's exchange takes to win: the
/// smallest count `blocksBeat` answers yes to holding its post, the largest body where
/// none wins. A room the colony is blind in prices its raid as last seen
/// (#569), or at one where none is recalled: enough to go and look. A room holding a
/// level-0 core takes the biggest body the bank buys (#487): one block's 90 a
/// tick spends most of a body's life on the core's 100,000 hits.
let internal guardBlocksFor (view: ColonyView) (room: string) : int =
    let fight =
        [ 1..guardBlocksMost ]
        |> List.tryFind (blocksBeat guardPattern Stance.Hold view room)
        |> Option.defaultValue guardBlocksMost

    if Facts.expansionCores view |> List.exists (fun core -> core.RoomName = room) then
        max fight (guardBlocksReach view)
    else
        fight

/// The rangers one room's raid takes, `guardsWanted`'s twin: one where the
/// biggest ranger the bank buys wins it alone, else `Engine.guardCap`, the
/// count the row's verdict prices (`Facts.rowAnswer`, #589). One body a
/// raid out-heals is fed to it one at a time.
let internal rangersWanted (view: ColonyView) (room: string) : int =
    if blocksBeat rangerPattern Stance.Standoff view room (max 1 (rangerBlocksReach view)) then
        1
    else
        Engine.guardCap

/// The ranger blocks one ranger room wants (#411): the smallest body that wins
/// its raid alone, never below the room's floor, and the largest body where
/// none wins.
let internal rangerBlocksFor (floor: int) (view: ColonyView) (room: string) : int =
    [ 1..rangerBlocksMost ]
    |> List.tryFind (blocksBeat rangerPattern Stance.Standoff view room)
    |> Option.defaultValue rangerBlocksMost
    |> max floor
    |> min rangerBlocksMost

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Brief =
    /// How far a quiet ranger walks for a kill (#510), in travel cost (two
    /// units a tick): the reaction window of a lend, inside which no raid
    /// reaches the home it left (`Facts.homeHoldReleases`).
    let private killWalk = 2 * Reaction.lend

    let private bare =
        {
            Kite = None
            Approach = lazy Set.empty
            Resident = None
            Harass = None
            Ring = None
            Trespass = None
        }

    /// One fight's ground divided into seats (#650): each holder's own
    /// ground, the tiles it reaches by its arrival (`arrival`, None for
    /// unreached), is matched a tile a holder, soonest arrival first (then
    /// by name and tile), so a holder standing on its ground keeps its tile
    /// tick to tick and two walking to one tile are sent to two. A matched
    /// holder's seats are the tiles it reaches of its ground less every
    /// other holder's match, where it reaches one of those as soon as any
    /// tile of its ground: a seat that would cost it ticks (one tile of a
    /// kite ground left on its side) is no seat, and the arbitration orders
    /// the two at the tile. One whose ground is its own (`own`, read only
    /// through its seats) has the tiles it reaches whatever, none reached
    /// being no ground at all. Absent for any other holder, which keeps the
    /// ground it would read without seats.
    let seatsAmong
        (holders: (string * Set<RoomPos> * bool * (RoomPos -> int option)) list)
        : Map<string, Set<RoomPos>> =
        let reached =
            holders
            |> List.map (fun (name, ground, own, arrival) ->
                name,
                own,
                ground
                |> Seq.choose (fun tile -> arrival tile |> Option.map (fun ticks -> ticks, tile))
                |> List.ofSeq)

        let matched =
            match reached with
            | []
            | [ _ ] -> Map.empty
            | _ ->
                reached
                |> List.collect (fun (name, _, tiles) ->
                    tiles |> List.map (fun (ticks, tile) -> ticks, name, tile))
                |> List.sort
                |> List.fold
                    (fun (seated: Map<string, RoomPos>, taken: Set<RoomPos>) (_, name, tile) ->
                        if Map.containsKey name seated || Set.contains tile taken then
                            seated, taken
                        else
                            Map.add name tile seated, Set.add tile taken)
                    (Map.empty, Set.empty)
                |> fst

        reached
        |> List.choose (fun (name, own, tiles) ->
            let others =
                matched
                |> Map.toSeq
                |> Seq.choose (fun (other, tile) -> if other <> name then Some tile else None)
                |> Set.ofSeq

            let soonest (tiles: (int * RoomPos) list) =
                tiles
                |> List.fold (fun soonest (ticks, _) -> min soonest ticks) System.Int32.MaxValue

            let kept, off =
                tiles |> List.partition (fun (_, tile) -> not (Set.contains tile others))

            // Kept off its partners' seats only where that costs it no tick.
            let keptOff = not off.IsEmpty && not kept.IsEmpty && soonest kept = soonest tiles

            let seats = (if keptOff then kept else tiles) |> List.map snd |> Set.ofList

            if own || (Map.containsKey name matched && keptOff) then
                Some(name, seats)
            else
                None)
        |> Map.ofList

    /// The rows a trespass's kill asks (#550), each with the trespassers
    /// standing in its room it answers: the guard those its swing catches
    /// (`Facts.strikesAtStandoff`), the ranger the rest — a hauler at the guard's
    /// pace — wherever its block is castable or a ranger of ours may be lent
    /// to the kill, striking each of them. The guard answers the rest too
    /// until a ranger holds the kill (`holders`, sticky, so no tick-to-tick
    /// flip): on the store ring it swings at every draw, and the ranger
    /// replaces it on them, never doubles it.
    let internal trespassSplit (view: ColonyView) (holders: string list) (room: string) =
        let trespassers =
            view.Hostiles
            |> List.filter (fun hostile ->
                hostile.Pos.Room = room && Facts.trespasser view (Set.singleton room) hostile)

        let caught, rest =
            trespassers
            |> List.partition (Facts.strikesAtStandoff view (Facts.blockFighter guardPattern))

        let rangerMay () =
            ColonyView.bodies view
            |> List.exists (fun creep ->
                isRangerRowBody creep
                && Facts.mayLeaveFor
                    view
                    room
                    (SpatialInfo.creepPlacementOf view.Spatial creep.Name
                     |> Option.map (fun at -> at.Room))
                    creep
                && rest |> List.forall (Facts.canStrike view (partCount creep.Body)))

        let rangerHolds =
            ColonyView.bodies view
            |> List.exists (fun creep -> isRangerRowBody creep && List.contains creep.Name holders)

        let asked = not (List.isEmpty rest) && (castable view rangerPattern || rangerMay ())

        [
            CastRow.Guard, (if asked && rangerHolds then caught else trespassers)
            CastRow.Ranger, (if asked then rest else [])
        ]

    /// Every kind's targets today: what `Facts.guardShoots` passes, in the kill
    /// order. One a Brief: a room's `killRank` is built once for a view and
    /// a holder (its kill scores, #621, each kill priced at the damage of the
    /// holder and every body `holding` the fight together), and each shot
    /// ranked once. With no holder named, the side's best score.
    let private guardTargets (view: ColonyView) atlas (holding: Set<string>) =
        let side =
            lazy (ColonyView.bodies view |> List.filter (fun creep -> holding.Contains creep.Name))

        let built =
            System.Collections.Generic.Dictionary<
                string * string,
                ColonyView *
                (HostileInfo -> KillRank) *
                System.Collections.Generic.Dictionary<string, HostileInfo * KillRank>
             >()

        let rankIn (view: ColonyView) (room: string) (holder: CreepInfo option) =
            let key =
                room, holder |> Option.map (fun creep -> creep.Name) |> Option.defaultValue ""

            let rank, ranked =
                match built.TryGetValue key with
                | true, (seen, rank, ranked) when obj.ReferenceEquals(seen, view) -> rank, ranked
                | _ ->
                    let bodies =
                        match holder with
                        | Some creep -> [ creep ]
                        | None -> side.Value

                    let rank = killRank view room (scoreAmong view atlas side.Value bodies room)

                    let ranked =
                        System.Collections.Generic.Dictionary<string, HostileInfo * KillRank>()

                    built.[key] <- (view, rank, ranked)
                    rank, ranked

            fun (hostile: HostileInfo) ->
                match ranked.TryGetValue hostile.Id with
                | true, (seen, kept) when obj.ReferenceEquals(seen, hostile) -> kept
                | _ ->
                    let kept = rank hostile
                    ranked.[hostile.Id] <- (hostile, kept)
                    kept

        {
            Shoots = guardShoots view
            Order =
                fun room holder walk shots ->
                    match shots with
                    | []
                    | [ _ ] -> shots
                    | _ ->
                        let rank = rankIn view room holder

                        shots
                        |> List.map (fun h -> (rank h, walk h, h.Id), h)
                        |> List.sortBy fst
                        |> List.map snd
            Band = fun room -> rankIn view room None >> killBandOf
        }

    /// An outpost a ranger answers (`ranged`, #535) holds the kite ground
    /// ahead of the ring, as a ranger's kind does.
    let private groundOf (kind: BriefKind) (ranged: bool) (laid: Laid) =
        let ring = laid.Ring |> Option.filter (Set.isEmpty >> not)

        let own kind =
            match kind with
            | BriefKind.Resident -> laid.Kite |> Option.orElse laid.Resident
            | BriefKind.Harass -> laid.Kite |> Option.orElse laid.Harass
            | BriefKind.Chase -> laid.Kite
            | BriefKind.Outpost when ranged -> laid.Kite |> Option.orElse ring
            | BriefKind.Outpost
            | BriefKind.ExitHold
            | BriefKind.Core -> ring
            | BriefKind.Trespass -> laid.Trespass

        BriefKind.order |> List.skipWhile ((<>) kind) |> List.tryPick own

    /// A resident room's Guard is the ranger row's (#411), and a harassment
    /// room's (#432); every ring kind's the melee guard's, which holds the
    /// post where it cannot catch the raid (#531), but an outpost a ranger
    /// answers (`ranged`, #535) is the ranger row's alone: the guard brings
    /// nothing to bear there. A kill admits only a body that strikes every
    /// target it counts (`Facts.strikesEvery`), the rule its verdict prices
    /// the rangers by. Whether the body fits the fight is no kind's: the
    /// Matcher asks it of every Guard (`Facts.fits`, #615 U3).
    let private admitsOf
        (kind: BriefKind)
        (room: string)
        (ranged: bool)
        (answered: Lazy<(CastRow * HostileInfo list) list>)
        (view: ColonyView)
        (atlas: Atlas.Atlas)
        (creep: CreepInfo)
        : bool =
        let strikes counts =
            Facts.strikesEvery view (partCount creep.Body) counts room

        match kind with
        | BriefKind.Resident
        | BriefKind.Harass -> isRangerRowBody creep
        // In sight, any fighting row's body that strikes every raider
        // (#516), whichever row cast it; HomeHold the match's gate
        // (`Matcher`). A stand-down shuts the economy, never the kill (#571).
        // Out of sight, the kite it was shut on (#510): a body that outranges
        // a swing, as no fight there is priced.
        | BriefKind.Chase ->
            (isRangerRowBody creep || isGuardRowBody creep)
            && (Facts.raided view room || Fighter.outranges (Fighter.ofCreep creep))
            && strikes (HostileInfo.raider view.Hostiles)
        // A trespass's kill (#516): the guard row's body that strikes every
        // trespasser its row answers (`trespassSplit`, #550), or a ranger
        // that strikes them all, whichever colony cast it: a mother's guard
        // hunts in her child's outpost too (#616). Defence first is the
        // rank's: a raid's Guard outranks it (`claimed`, #528).
        | BriefKind.Trespass ->
            if isRangerRowBody creep then
                strikes (Facts.trespasser view (Set.singleton room))
            else
                isGuardRowBody creep
                && answered.Value
                   |> List.forall (fun (row, trespassers) ->
                       row <> CastRow.Guard
                       || trespassers |> List.forall (Facts.canStrike view (partCount creep.Body)))
        | BriefKind.Outpost when ranged -> isRangerRowBody creep
        // A level-0 core is the guard row's to break (#487), and a ranger's
        // already standing in its room with nothing better to strike (#558),
        // on a seat of its own.
        | BriefKind.Core ->
            isGuardRowBody creep
            || isRangerRowBody creep
               && (match Atlas.creepRoom atlas creep.Name with
                   | Some here -> here = room
                   // Another colony's body, which this Atlas does not
                   // place: where the fleet stands it.
                   | None ->
                       Map.tryFind room view.World.Bodies.Value
                       |> Option.exists (List.exists (fun (body, _) -> body.Name = creep.Name)))
        | BriefKind.Outpost
        | BriefKind.ExitHold -> isGuardRowBody creep

    /// How many rangers one resident room wants: the standing garrison
    /// (`Tuning.RangerResidents`). A raised home whose towers cannot hold its
    /// raid, which this colony may defend too (`BorrowedWork.Defended`,
    /// #589), and whose resident loses it alone adds #428's relief,
    /// `Engine.guardCap` raid-sized bodies, on top of the
    /// garrison (#447). A resident room whose raid no ranger size wins
    /// (`outmatched`) wants none (#451): a body cast into it only feeds the
    /// raid, so the home is the child's safe mode's, and the garrison already
    /// standing keeps its Guard (the pool's cap counts it) on safe ground.
    let private residentsWanted (room: string) (view: ColonyView) (threats: Threats) : int =
        let lost =
            match Map.tryFind (Guard room) threats.Briefs with
            | Some brief -> brief.Verdict.Value = Answer.Lose
            | None -> outmatched view room

        if lost then
            0
        elif
            Set.contains room (raisedHomes view)
            && List.contains room view.Borrowed.Defended
            && not (
                blocksBeat rangerPattern Stance.Standoff view room view.Tuning.RangerResidentBlocks
            )
        then
            view.Tuning.RangerResidents + Engine.guardCap
        else
            view.Tuning.RangerResidents

    /// A harassment room's ranger (#432), one: a raid that one loses is the
    /// room's [[stand-down]]. Its floor is the one it was cast at
    /// (`ColonyView.HarassFloors`, #457).
    let private harassAsk (view: ColonyView) (room: string) : RowAsk =
        {
            Row = CastRow.Ranger
            Wanted = fun _ -> 1
            Blocks =
                fun () ->
                    let floor =
                        Map.tryFind room view.HarassFloors
                        |> Option.defaultValue view.Tuning.HarassBlocks

                    rangerBlocksFor floor view room
            Prey = None
        }

    /// What each kind asks its row for. A ranger kind the ranger row's, never
    /// below `Tuning.RangerResidentBlocks` for a resident, the resident a raid
    /// meets first; a chase nothing; every other kind the guard row's
    /// `guardsWanted`, at `guardBlocksFor`; a trespass one body a trespasser
    /// of the row that answers it (`trespassSplit`, #550), up to
    /// `Engine.guardCap` a row, the guards never
    /// fewer than `guardsWanted` where they answer one or the trespasser only
    /// bounces on the exit into the room.
    let private askOf
        (kind: BriefKind)
        (ranged: bool)
        (rowCast: Lazy<CastRow option>)
        (answered: Lazy<(CastRow * HostileInfo list) list>)
        (room: string)
        (view: ColonyView)
        : RowAsk list =
        match kind with
        // Quirk, kept (the Brief's S0), for the B-stage to settle: a resident
        // room that is a harassment room too is the resident's kind, ground
        // and cap, but casts as the harassment: one ranger, not the garrison,
        // at the harassment's floor.
        | BriefKind.Resident ->
            [
                {
                    Row = CastRow.Ranger
                    Wanted =
                        fun threats ->
                            if Set.contains room (harassRooms view) then
                                (harassAsk view room).Wanted threats
                            else
                                residentsWanted room view threats
                    Blocks =
                        fun () ->
                            if Set.contains room (harassRooms view) then
                                (harassAsk view room).Blocks()
                            else
                                rangerBlocksFor view.Tuning.RangerResidentBlocks view room
                    Prey = None
                }
            ]
        | BriefKind.Harass -> [ harassAsk view room ]
        // An outpost a ranger answers (#535): the smallest that wins it
        // alone, or the pair where none the bank buys does (`rangersWanted`).
        | BriefKind.Outpost when ranged ->
            [
                {
                    Row = CastRow.Ranger
                    Wanted = fun _ -> rangersWanted view room
                    Blocks = fun () -> rangerBlocksFor 1 view room
                    Prey = None
                }
            ]
        | BriefKind.Trespass ->
            let bouncing = answered.Value |> List.forall (snd >> List.isEmpty)

            // A priced cast's prey is the enemy's economy alone, and what
            // the ranger row answers is no guard's: one body a trespasser,
            // never two (#652).
            let shot =
                answered.Value
                |> List.collect (fun (row, trespassers) ->
                    if row = CastRow.Ranger then trespassers else [])

            answered.Value
            |> List.map (fun (row, trespassers) ->
                {
                    Row = row
                    // At most `Engine.guardCap` a row: bodies that strike
                    // nothing back never buy more spawns than a raid's room.
                    Wanted =
                        fun _ ->
                            match row, trespassers with
                            | CastRow.Guard, [] when not bouncing -> 0
                            | CastRow.Guard, _ ->
                                max (guardsWanted view room) (List.length trespassers)
                                |> min Engine.guardCap
                            | _ -> min Engine.guardCap (List.length trespassers)
                    Blocks =
                        fun () ->
                            if row = CastRow.Ranger then
                                rangerBlocksFor 1 view room
                            else
                                guardBlocksFor view room
                    Prey =
                        trespassers
                        |> List.filter (fun hostile ->
                            Facts.enemyEconomy view.Hostiles hostile
                            && (row = CastRow.Ranger || not (List.contains hostile shot)))
                        |> List.map (fun hostile -> hostile.Id, Facts.killValue view hostile)
                        |> Some
                })
        | BriefKind.Outpost
        | BriefKind.Core
        | BriefKind.ExitHold ->
            [
                {
                    Row = CastRow.Guard
                    Wanted = fun _ -> guardsWanted view room
                    Blocks = fun () -> guardBlocksFor view room
                    Prey = None
                }
            ]
        // A chase is cast for by nobody, but in a room shut on a raid the
        // colony's own row wins (`rowCast`, #571): the stand-down shuts that
        // room's Guard out of the pool, never the row's cast, so the chase
        // carries the row's ask.
        | BriefKind.Chase ->
            match rowCast.Value with
            | Some CastRow.Ranger ->
                [
                    {
                        Row = CastRow.Ranger
                        Wanted = fun _ -> rangersWanted view room
                        Blocks = fun () -> rangerBlocksFor 1 view room
                        Prey = None
                    }
                ]
            | Some _ ->
                [
                    {
                        Row = CastRow.Guard
                        Wanted = fun _ -> guardsWanted view room
                        Blocks = fun () -> guardBlocksFor view room
                        Prey = None
                    }
                ]
            | None -> []

    /// The bodies a kind's room admits and nobody else at all: its row ask's
    /// count is the row's own arithmetic, read here a second time so the number
    /// the cascade hires against and the number the Matcher counts holders
    /// against are one number.
    let private capacityOf
        (kind: BriefKind)
        (room: string)
        (verdict: Lazy<Answer>)
        (rangers: Lazy<int option>)
        (holding: Set<string>)
        (asks: RowAsk list)
        (view: ColonyView)
        (threats: Threats)
        : Capacity =
        let wanted () =
            asks
            |> List.tryHead
            |> Option.map (fun a -> a.Wanted threats)
            |> Option.defaultValue 1

        match kind with
        // One over per garrison body in a resident room (#414, #419, #447): its
        // rangers are resident, and each relief — cast at its incumbent's
        // lead, two of them together when the garrison was cast together —
        // must take the Task and walk three crossings while the incumbent
        // still holds the ring. A Guard has no arrival price for a handover
        // window to be read against, and the row's count, not this cap, is
        // what buys bodies.
        | BriefKind.Resident -> Capacity.fighters (wanted () + view.Tuning.RangerResidents)
        // One ranger per harassment room (#439), and its relief beside it
        // (`Capacity.Relieved`).
        // A raid one ranger loses seats the rangers that win it together
        // (`Facts.rangersWin`, #609): one seat left the second at the
        // harassment room beside it while the first bounced on the border.
        | BriefKind.Harass ->
            let seats =
                match verdict.Value with
                | Answer.Win -> max (wanted ()) (rangers.Value |> Option.defaultValue 1)
                | _ -> wanted ()

            Capacity.fighters seats |> Capacity.relieving
        // A kite seats as many rangers as kill what cannot close on them
        // before it ages out, and every one at the fight (`Facts.kiteSeats`,
        // #510, #596); a raid the rangers
        // there and beside it win together takes as many as win it; one a row
        // casts for, as many as it asks.
        | BriefKind.Chase ->
            match verdict.Value with
            | Answer.Win when not (List.isEmpty asks) ->
                Capacity.fighters (max (wanted ()) (rangers.Value |> Option.defaultValue 1))
            | Answer.Win -> Capacity.fighters (rangers.Value |> Option.defaultValue 1)
            | Answer.Kite -> Capacity.fighters (Facts.kiteSeats view room holding)
            | Answer.Lose -> Capacity.fighters 1
        // One body kills a trespasser, which strikes nothing back (#516): its
        // row asks summed (#550). Where the ranger row answers one, the guards
        // are capped at their own row's count and a ranger takes any seat, as
        // its shot reaches what a swing catches too.
        | BriefKind.Trespass ->
            let count row =
                asks
                |> List.filter (fun a -> a.Row = row)
                |> List.sumBy (fun a -> a.Wanted threats)

            let guards, rangers = count CastRow.Guard, count CastRow.Ranger
            let capacity = Capacity.fighters (guards + rangers)

            if rangers = 0 then
                capacity
            else
                capacity
                |> Capacity.capping (CapScope.Row CastRow.Guard) guards
                |> Capacity.capping (CapScope.Row CastRow.Ranger) (guards + rangers)
        // The guard row's count, and a seat beside it for each ranger it
        // admits standing in the room (#558), so a ranger never takes a
        // guard's.
        | BriefKind.Core ->
            let guards = wanted ()

            let seated =
                ColonyView.bodies view
                |> List.filter (fun creep ->
                    isRangerRowBody creep
                    && SpatialInfo.creepPlacementOf view.Spatial creep.Name
                       |> Option.exists (fun at -> at.Room = room))
                |> List.length

            if seated = 0 then
                Capacity.fighters guards
            else
                Capacity.fighters (guards + seated)
                |> Capacity.capping (CapScope.Row CastRow.Guard) guards
                |> Capacity.capping (CapScope.Row CastRow.Ranger) seated
        | BriefKind.Outpost
        | BriefKind.ExitHold -> Capacity.fighters (wanted ())

    /// A Guard's own kind: the first of its claims, `Outpost` where none
    /// claims the room.
    let private kindOf (claims: BriefKind list) =
        List.tryHead claims |> Option.defaultValue BriefKind.Outpost

    /// One room's Brief, off the kinds that claimed it down `BriefKind.order`
    /// and the ground each source laid there. A kill (#510, #516) is stepped a
    /// rung up — its kill is the room's whole income back, and a quiet holder
    /// is let go for it — and calls that holder off within its walk. A raid's
    /// Guard two: the melee guard's ring, or a garrison's or a chase's where
    /// a Threat stands (`raided`). Defence first, so a body the raid wants
    /// never goes to a kill, and only one the raid leaves free does (#528).
    /// A harassment room's Threat is the enemy's own. The room's verdict, the
    /// rangers that win it (`Facts.rangersWin`) and whether a ranger answers
    /// it (`Facts.rangerAnswers`, read for an outpost alone) and whether the
    /// colony's own row casts for its chase (`casts`) come priced lazily,
    /// once a tick. Its answers read the view and Atlas given.
    let internal claimed
        (view: ColonyView)
        (atlas: Atlas.Atlas)
        (room: string)
        (raided: bool)
        (verdict: Lazy<Answer>)
        (rangers: Lazy<int option>)
        (ranger: Lazy<bool>)
        (casts: Lazy<bool>)
        (answered: Lazy<(CastRow * HostileInfo list) list>)
        (holding: Set<string>)
        (claims: BriefKind list)
        (laid: Laid)
        : Brief =
        let kind = kindOf claims
        let kill = BriefKind.isKill kind
        let ranged = kind = BriefKind.Outpost && ranger.Value

        let rowCast =
            lazy
                (if kind = BriefKind.Chase && casts.Value then
                     Some(if ranger.Value then CastRow.Ranger else CastRow.Guard)
                 else
                     None)

        let ask = askOf kind ranged rowCast answered room view

        {
            Duty = Duty.Guard kind
            BuiltBy = SpatialInfo.homeName view.Spatial
            Claims = claims
            Targets = guardTargets view atlas holding
            Admits = admitsOf kind room ranged answered view atlas
            Ends = Facts.endsIn view room
            Recall = if kill then Some { Walk = killWalk } else None
            Laid = laid
            // With none laid, the ring of the room's declared sources, which
            // `Outpost.Sources` places without vision (#366): off the
            // builder's Atlas, so a body of another colony the world hands
            // the Brief walks to it too (a transit room carries no source).
            Ground =
                groundOf kind ranged laid
                |> Option.orElseWith (fun () ->
                    Some(Atlas.sourceRingIn atlas room) |> Option.filter (Set.isEmpty >> not))
            WalksIn = kind = BriefKind.Chase
            Avoid =
                match kind with
                | BriefKind.Chase -> Avoid.Kite
                | BriefKind.Trespass -> Avoid.Reach
                | _ -> Avoid.Runner
            Rung =
                match kind with
                | BriefKind.Outpost -> TwoRungsUp
                | BriefKind.Resident
                | BriefKind.Chase when raided -> TwoRungsUp
                // A raid in a harassment room our side wins is the raid's
                // rung, a ranger idling on a quiet Guard sent to it (#609).
                | BriefKind.Harass when raided && verdict.Value = Answer.Win -> TwoRungsUp
                | _ when kill -> OneRungUp
                | _ -> OnTheTier
            Asks = ask
            Capacity = capacityOf kind room verdict rangers holding ask view
            Verdict = verdict
            Formation = None
            Stances = Map.empty
            Seats = lazy Map.empty
        }

    /// The stance a body meets its Guard's raid at (#615 U3): a swing on a
    /// ring kind holds the post (`Stance.Hold`, #531), which the raid must
    /// come to to hurt our work, where its post is all it can hold — a held
    /// exit, or a raider it does not strike at their standoff
    /// (`Facts.strikesAtStandoff`: one that keeps off a swing); every other
    /// body fights where it goes, at the standoff.
    let stanceOf (view: ColonyView) (room: string) (brief: Brief) (creep: CreepInfo) : Stance =
        match brief.Duty with
        | Duty.Guard kind when BriefKind.laysRing kind && Fighter.glues (Fighter.ofCreep creep) ->
            let fighter = Fighter.ofCreep creep

            let caught (h: HostileInfo) =
                h.Pos.Room <> room
                || not (HostileInfo.raider view.Hostiles h)
                || Facts.strikesAtStandoff view fighter h

            if kind = BriefKind.ExitHold || not (List.forall caught view.Hostiles) then
                Stance.Hold
            else
                Stance.Standoff
        | _ -> Stance.Standoff

    /// A room no source claims or lays ground in: a melee guard's `Outpost`
    /// on no ground, the one a remembered raid pools in a room the colony is
    /// blind in (#366). No raider stands in it, so its verdict is `Win`.
    let unclaimed (view: ColonyView) (atlas: Atlas.Atlas) (room: string) : Brief =
        claimed
            view
            atlas
            room
            false
            (lazy Answer.Win)
            (lazy None)
            (lazy false)
            (lazy false)
            (lazy [])
            Set.empty
            []
            bare

    /// How many [[defender]]s the home wants this tick (#504): one a body of
    /// the raid at its line, at most `Tuning.DefenderCap`; none in a home
    /// that keeps no ramparts, has no perimeter rampart standing, or runs
    /// safe mode.
    let private defendersWanted (atLine: HostileInfo list) (view: ColonyView) : int =
        let home = SpatialInfo.homeName view.Spatial

        let shielded =
            Map.tryFind home view.RoomControl
            |> Option.exists (fun control -> control.SafeMode)

        match atLine with
        | [] -> 0
        | _ when not (keepsRamparts view) || shielded -> 0
        | _ when List.isEmpty (perimeterRamparts view) -> 0
        | raid -> min view.Tuning.DefenderCap (List.length raid)

    /// Another colony's ranger-row bodies holding the home's ground (#552),
    /// the mother's residents: standing in the home (`WorldView.Bodies`) with
    /// its Guard or its Defend held (`holders`, the shard's table), not
    /// passing through. They answer its raid from its ramparts, and the home
    /// casts none for them.
    let internal lentRangers (view: ColonyView) (holders: Set<string>) : int =
        let home = SpatialInfo.homeName view.Spatial

        let mine =
            ColonyView.bodies view |> List.map (fun creep -> creep.Name) |> Set.ofList

        Map.tryFind home view.World.Bodies.Value
        |> Option.defaultValue []
        |> List.filter (fun (creep, _) ->
            not (Set.contains creep.Name mine)
            && isRangerRowBody creep
            && Set.contains creep.Name holders)
        |> List.length

    /// The rows the home's Defend asks (#546), each with the bodies of the
    /// raid at the line it answers: the defender those it meets — a raider it
    /// strikes at their standoff (`Facts.strikesAtStandoff`, #552), which
    /// must close to hurt our work and so comes to its rampart, a body
    /// working with a WORK beside it, or a healer, which comes with its
    /// melee —, waiting on its rampart for the ones not yet
    /// in its swing; the ranger those the defender never strikes from the
    /// line (#552) — a shooter keeping its range, a healer, and a body out
    /// of every perimeter rampart's swing that is parked (`ColonyView.Parked`)
    /// or on the exit tile, never one walking in. Where no ranger is cast
    /// (`Facts.castable`), held — holding the Defend or standing in the home
    /// — or lent (`lentRangers`), the defender answers them all.
    let internal defendSplit (view: ColonyView) (holders: Set<string>) (atLine: HostileInfo list) =
        let home = SpatialInfo.homeName view.Spatial
        let ramparts = perimeterRamparts view |> List.map (fun (_, tile, _) -> tile)

        let inSwing (hostile: HostileInfo) =
            ramparts
            |> List.exists (fun tile ->
                RoomPos.range tile hostile.Pos
                |> Option.exists (fun r -> r <= Engine.meleeRange))

        let defender = Facts.blockFighter defenderPattern

        let meets (hostile: HostileInfo) =
            if HostileInfo.raider view.Hostiles hostile then
                Facts.strikesAtStandoff view defender hostile
            else
                HostileInfo.works view.Hostiles hostile
                || HostileInfo.heals view.Hostiles hostile

        let met = atLine |> List.filter meets

        let neverStruck (hostile: HostileInfo) =
            not (meets hostile)
            || not (
                HostileInfo.armed view.Hostiles hostile
                || HostileInfo.works view.Hostiles hostile
            )
            || not (inSwing hostile)
               && (Set.contains hostile.Id view.Parked || Seam.isExit (RoomPos.pos hostile.Pos))

        let held =
            ColonyView.bodies view
            |> List.exists (fun creep ->
                isRangerRowBody creep
                && (Set.contains creep.Name holders
                    || SpatialInfo.creepPlacementOf view.Spatial creep.Name
                       |> Option.exists (fun at -> at.Room = home)))

        if castable view rangerPattern || held || lentRangers view holders > 0 then
            [ CastRow.Defender, met; CastRow.Ranger, atLine |> List.filter neverStruck ]
        else
            [ CastRow.Defender, atLine; CastRow.Ranger, [] ]

    /// The home's Defend (#504), off the raid at its line (`Facts.raidAtLine`)
    /// as `defendSplit` shares it between the rows: one ask a row, for the
    /// raiders it answers, each row's bodies admitted under a cap of its own
    /// (`CapScope.Row`) and no other body, each on its row's perimeter ground
    /// (`stances`, by body), walking round the tiles the raid's melee reach
    /// (`Avoid.Line`), striking the body at the line with the fewest hits in
    /// reach. A ranger's is the one that wins the home's raid alone, as an
    /// outpost's. Its row asks are the capacity too, read a second time as a
    /// Guard's is.
    let internal defending
        (view: ColonyView)
        (answered: (CastRow * HostileInfo list) list)
        (lent: int)
        (ground: Set<RoomPos> option)
        (stances: Map<string, Set<RoomPos>>)
        (avoid: Set<Pos>)
        (verdict: Lazy<Answer>)
        : Brief =
        let line =
            answered
            |> List.collect snd
            |> List.map (fun hostile -> hostile.Id)
            |> Set.ofList

        let asks =
            answered
            |> List.map (fun (row, raid) ->
                {
                    Row = row
                    Wanted =
                        fun _ ->
                            if row = CastRow.Ranger then
                                max 0 (defendersWanted raid view - lent)
                            else
                                defendersWanted raid view
                    Blocks =
                        fun () ->
                            if row = CastRow.Ranger then
                                rangerBlocksFor 1 view (SpatialInfo.homeName view.Spatial)
                            else
                                view.Tuning.DefenderBlocks
                    Prey = None
                })

        let capacity threats =
            (Capacity.fighters (asks |> List.sumBy (fun ask -> ask.Wanted threats)), asks)
            ||> List.fold (fun capacity ask ->
                Capacity.capping (CapScope.Row ask.Row) (ask.Wanted threats) capacity)

        {
            Duty = Duty.Defend
            BuiltBy = SpatialInfo.homeName view.Spatial
            Claims = []
            Targets =
                {
                    Shoots = fun _ _ hostile -> Set.contains hostile.Id line
                    Order = fun _ _ _ shots -> shots |> List.sortBy (fun h -> h.Hits, h.Id)
                    Band = fun _ _ -> 0
                }
            Admits =
                fun creep ->
                    answered |> List.exists (fun (row, _) -> CastRow.cut row creep.Name creep.Body)
            Ends = Facts.endsIn view (SpatialInfo.homeName view.Spatial)
            Recall = None
            Laid = bare
            Ground = ground
            WalksIn = false
            Avoid = Avoid.Line avoid
            Rung = OnTheTier
            Asks = asks
            Capacity = capacity
            Verdict = verdict
            Formation = None
            Stances = stances
            Seats = lazy Map.empty
        }

    /// An Assault's Brief on its squad's `Formation`: it admits the bodies
    /// the formation casts by role, one cap a role, and asks each squad
    /// role's row for that role's slots while `casting` — no squad is bought
    /// for a room it cannot rally for.
    let internal formed
        (view: ColonyView)
        (atlas: Atlas.Atlas)
        (duty: Duty)
        (rung: Rung)
        (verdict: Lazy<Answer>)
        (casting: ColonyView -> bool)
        (formation: Formation)
        : Brief =
        let asks =
            formation.Slots
            |> List.distinct
            |> List.map (fun role ->
                {
                    Row = CastRow.Squad role
                    Wanted =
                        fun _ ->
                            if casting view then
                                SquadRole.slots role formation.Slots
                            else
                                0
                    Blocks = fun () -> 1
                    Prey = None
                })

        {
            Duty = duty
            BuiltBy = SpatialInfo.homeName view.Spatial
            Claims = []
            Targets = guardTargets view atlas (formation.Roles |> Map.keys |> Set.ofSeq)
            Admits = fun creep -> Map.containsKey creep.Name formation.Roles
            Ends = fun _ -> None
            Recall = None
            Laid = bare
            Ground = None
            WalksIn = false
            Avoid = Avoid.Runner
            Rung = rung
            Asks = asks
            Capacity = fun _ -> Capacity.roles formation.Slots
            Verdict = verdict
            Formation = Some formation
            Stances = Map.empty
            Seats = lazy Map.empty
        }

    /// A combat Task no source claims this tick: a Guard's `unclaimed`; a
    /// Defend with no raid at the line, which still admits the defender row
    /// and wants none; an Assault with no ground, which admits no body. None
    /// for every other Task.
    let unclaimedFor (view: ColonyView) (atlas: Atlas.Atlas) (task: Task) : Brief option =
        let unformed duty =
            let none =
                {
                    Slots = []
                    Launched = false
                    Rally = Set.empty
                    Target = None
                    Front = Set.empty
                    Behind = Set.empty
                    Roles = Map.empty
                }

            Some
                { formed view atlas duty OnTheTier (lazy Answer.Win) (fun _ -> false) none with
                    Formation = None
                }

        match task with
        | Guard room -> Some(unclaimed view atlas room)
        | Defend _ -> Some(defending view [] 0 None Map.empty Set.empty (lazy Answer.Win))
        | Assault _ -> unformed Duty.Assault
        | _ -> None

/// The one path a squad launches through, an Assault's and a Guard's muster
/// alike (the Brief's S8): its casts, where it rallies, whether it has
/// launched, and whether its file has strung out.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Formation =
    /// How near the crossing it waits beside the rally ground lies, in tiles:
    /// room for a squad on the near side of the exit.
    let private rallyReach = 3

    /// How far from the leader a member may stand and still be with it.
    let private squadSpread = 2

    /// How close to a room's border a body in the room beside it stands at
    /// its crossing: the exit tile and the one inside it. The rally ground
    /// keeps clear of both, so a body there has left it.
    let private crossingGap = 1

    /// Tiles from a tile to the nearest edge of its room.
    let internal edgeGap (tile: Pos) =
        min (min tile.X (Engine.roomSide - 1 - tile.X)) (min tile.Y (Engine.roomSide - 1 - tile.Y))

    /// The range between two tiles across a border too: a member a step
    /// behind its leader over a crossing is beside it. The exit tiles either
    /// side of a border are one tile, the engine carrying a body from one to
    /// the other, so the rooms overlap by a row. None between rooms that
    /// share no border.
    let internal rangeAcross (a: RoomPos) (b: RoomPos) : int option =
        if a.Room = b.Room then
            RoomPos.range a b
        else
            let span = Engine.roomSide - 1

            RoomName.offsetOf a.Room b.Room
            |> Option.filter (fun (dx, dy) -> abs dx + abs dy = 1)
            |> Option.map (fun (dx, dy) ->
                max (abs (b.X + span * dx - a.X)) (abs (b.Y + span * dy - a.Y)))

    /// Whether a tile stands at the crossing into a room from the room beside
    /// it: the room's own nearest tile, across the border, within a step of
    /// the exit.
    let internal atCrossingInto (room: string) (tile: RoomPos) =
        RoomName.offsetOf tile.Room room
        |> Option.filter (fun (dx, dy) -> abs dx + abs dy = 1)
        |> Option.exists (fun (dx, dy) ->
            let across value offset =
                let low = Engine.roomSide * offset
                max 0 (max (low - value) (value - (low + Engine.roomSide - 1)))

            max (across tile.X dx) (across tile.Y dy) <= crossingGap + 1)

    /// The crossing a muster waits beside on a chain from home to its
    /// room: out of the last room short of the room that is neither
    /// a Source Keeper's nor a rival's — home, which is neither, at the last
    /// — toward the next room on the chain. None for a chain of one room.
    let rallyHop (rivals: Set<string>) (chain: string list) : (string * string) option =
        chain
        |> List.pairwise
        |> List.rev
        |> List.tryFind (fun (from, _) ->
            not (Keepers.isKeeperRoom from) && not (Set.contains from rivals))

    /// The ground beside the middle of the crossing from one room into the
    /// next, clear of the border and of the room's Reach.
    let private besideCrossing atlas (reachIn: string -> Set<Pos>) (from: string) (into: string) =
        match Atlas.seams atlas from into |> List.map fst with
        | [] -> Set.empty
        | exits ->
            let middle = List.item (List.length exits / 2) exits
            let reach = reachIn from

            Atlas.walkableWithinIn atlas from rallyReach middle
            |> List.filter (fun tile -> edgeGap tile > crossingGap && not (Set.contains tile reach))
            |> Set.ofList
            |> RoomPos.setAt from

    /// A Guard's muster ground on its chain from home (`rallyHop`, #535);
    /// empty where no chain reaches its room.
    let internal rallyOn
        (view: ColonyView)
        atlas
        (reachIn: string -> Set<Pos>)
        (chain: string list option)
        =
        chain
        |> Option.bind (rallyHop view.Spatial.RivalRooms)
        |> Option.map (fun (from, into) -> besideCrossing atlas reachIn from into)
        |> Option.defaultValue Set.empty

    /// An Assault's rally ground: at home, beside the crossing out toward the
    /// next room on its chain, off the idle ground (`Atlas.idleGroundIn`) —
    /// the rings of the spawns, extensions and stores — where any of it is
    /// left. Empty where no chain reaches its room.
    let internal musterAtHome atlas (reachIn: string -> Set<Pos>) (chain: string list option) =
        match chain with
        | Some(home :: next :: _) ->
            let ground = besideCrossing atlas reachIn home next
            let idle = Atlas.idleGroundIn atlas home |> RoomPos.setAt home

            match Set.difference ground idle with
            | clear when Set.isEmpty clear -> ground
            | clear -> clear
        | _ -> Set.empty

    /// Every squad cast of this colony, by the role its name carries.
    let internal casts (view: ColonyView) : Map<string, SquadRole> =
        ColonyView.bodies view
        |> List.choose (fun creep ->
            squadRoleByName creep.Name |> Option.map (fun role -> creep.Name, role))
        |> Map.ofList

    /// The casts holding a Task, by role, by name.
    let internal holding (held: HeldTaskFacts) (task: Task) (casts: Map<string, SquadRole>) =
        HeldTaskFacts.holdersOf held (taskId task)
        |> List.choose (fun name -> Map.tryFind name casts |> Option.map (fun role -> name, role))
        |> List.sort

    /// A squad's casts' names, the front leading: a sapper or a probe, else
    /// whoever holds. Every rule below takes its members in this order, the
    /// head the leader.
    let internal frontFirst (members: (string * SquadRole) list) : string list =
        members
        |> List.sortBy (fun (name, role) -> (if role = Medic then 1 else 0), name)
        |> List.map fst

    /// The leading cast that is no medic, and where it stands: the body a
    /// medic walks behind.
    let internal leader (view: ColonyView) (members: (string * SquadRole) list) =
        members
        |> List.filter (snd >> (<>) Medic)
        |> frontFirst
        |> List.tryHead
        |> Option.bind (fun name ->
            SpatialInfo.creepPlacementOf view.Spatial name
            |> Option.map (fun tile -> name, tile))

    /// Whether each member stands on the rally ground or with the rest: the
    /// others within two of the leader, the head of `members`, and the leader
    /// within two of one of them. A leader is never with the rest by standing
    /// on its own tile, so a recast front far off the rally ground is not.
    let private together (view: ColonyView) (rally: Set<RoomPos>) (members: string list) =
        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let leader = List.tryHead members

        members
        |> List.forall (fun name ->
            tileOf name
            |> Option.exists (fun tile ->
                Set.contains tile rally
                || members
                   |> List.exists (fun other ->
                       other <> name
                       && (Some name = leader || Some other = leader)
                       && tileOf other
                          |> Option.bind (rangeAcross tile)
                          |> Option.exists (fun r -> r <= squadSpread))))

    /// Whether the members stand as one file (#491): each within two of
    /// another, every one joined to the rest. A squad that walks single file
    /// off its rally ground strings out further than two from its leader
    /// without leaving one of its own behind.
    let internal linked (view: ColonyView) (members: string list) =
        let tiles = members |> List.choose (SpatialInfo.creepPlacementOf view.Spatial)

        let near (a: RoomPos) (b: RoomPos) =
            rangeAcross a b |> Option.exists (fun r -> r <= squadSpread)

        let rec grow (joined: RoomPos list) (rest: RoomPos list) =
            match rest |> List.partition (fun tile -> joined |> List.exists (near tile)) with
            | [], left -> List.isEmpty left
            | added, left -> grow (added @ joined) left

        match tiles with
        | first :: rest when List.length tiles = List.length members -> grow [ first ] rest
        | _ -> false

    /// Whether every cast stands in the room or at its crossing: a front
    /// chasing its target, or carried over the border off an exit tile, is
    /// not called back to the rally ground.
    let internal entered (view: ColonyView) (room: string) (members: string list) =
        members
        |> List.forall (fun name ->
            SpatialInfo.creepPlacementOf view.Spatial name
            |> Option.exists (fun tile -> tile.Room = room || atCrossingInto room tile))

    /// Whether a squad has launched, the one rule: a rally ground to wait on,
    /// a cast holding it, its slots filled and the squad `ready`, and every
    /// cast on the rally ground or with the squad, or every one in the room
    /// or at its crossing, or the squad still `onTheWalk` it launched on.
    let internal launched
        (view: ColonyView)
        (room: string)
        (rally: Set<RoomPos>)
        (members: string list)
        (ready: bool)
        (onTheWalk: bool)
        =
        not (Set.isEmpty rally)
        && not (List.isEmpty members)
        && ready
        && (together view rally members || entered view room members || onTheWalk)

    /// The walkable tiles beside a tile, in its room.
    let internal besideIn atlas (tile: RoomPos) =
        Atlas.adjacentWalkableIn atlas tile.Room (RoomPos.pos tile)
        |> List.map (RoomPos.at tile.Room)
        |> Set.ofList

    /// Whether a squad's file has strung out under fire, so its medics cannot
    /// reach the body the towers pick: at home, in the room or at its
    /// crossing, some cast farther than two from the leader that has a step
    /// nearer it — onto a free tile off the border and off the `front`; in
    /// the rooms between, only a broken file, since a one-tile corridor fits
    /// no four within two of one. Never while the leader stands on the front,
    /// where it works.
    let internal strung
        (view: ColonyView)
        atlas
        (home: string)
        (room: string)
        (front: Set<RoomPos>)
        (members: string list)
        (leading: (string * RoomPos) option)
        =
        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let held = members |> List.choose tileOf |> Set.ofList

        // Whether a cast this far from the leader has a step nearer it. In a
        // one-tile corridor the file's tail has none, and holding for it
        // holds for good (live t930,290). Across a border it is still walking
        // in.
        let closes (at: RoomPos) (tile: RoomPos) =
            tile.Room <> at.Room
            || besideIn atlas tile
               |> Set.exists (fun step ->
                   edgeGap (RoomPos.pos step) > 0
                   && not (Set.contains step held)
                   && not (Set.contains step front)
                   && RoomPos.range step at < RoomPos.range tile at)

        match leading with
        | Some(_, at) when Set.contains at front -> false
        | Some(name, at) when at.Room = home || at.Room = room || atCrossingInto room at ->
            members
            |> List.exists (fun other ->
                other <> name
                && tileOf other
                   |> Option.forall (fun tile ->
                       rangeAcross at tile
                       |> Option.forall (fun r -> r > squadSpread && closes at tile)))
        | Some _ -> not (linked view members)
        | None -> false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// A quiet tick (`noThreats`) read off one colony's view and Atlas, so
    /// a combat Task no source claims still has its Brief: what a test
    /// hands a reader of the Briefs.
    let quietOn (view: ColonyView) (atlas: Atlas.Atlas) : Threats =
        { noThreats () with
            Unclaimed =
                {
                    BriefOf = Brief.unclaimedFor view atlas
                }
        }

    /// One room's Guard `Brief` this tick.
    let briefOf (threats: Threats) (room: string) : Brief =
        match Map.tryFind (Guard room) threats.Briefs with
        | Some brief -> brief
        | None ->
            match threats.Unclaimed.BriefOf(Guard room) with
            | Some brief -> brief
            | None -> invalidOp $"No view derived these Threats to brief {room}'s Guard"

    /// One combat Task's `Brief` this tick (`Brief.unclaimedFor` where no
    /// source claims it); None for a Task no Brief describes.
    let briefFor (threats: Threats) (task: Task) : Brief option =
        match Map.tryFind task threats.Briefs with
        | Some brief -> Some brief
        | None -> threats.Unclaimed.BriefOf task

    /// How many bodies a combat Task asks its rows for this tick, every ask
    /// summed; none for a Task no Brief describes.
    let wants (threats: Threats) (task: Task) : int =
        briefFor threats task
        |> Option.map (fun brief -> brief.Asks |> List.sumBy (fun ask -> ask.Wanted threats))
        |> Option.defaultValue 0

    /// One room's shelter (#514); empty for a room with none.
    let shelterIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Shelter
        |> Option.map (fun ground -> ground.Force())
        |> Option.defaultValue Set.empty

    /// What a [[defender]] in one room walks round (`Avoid.Line`, #504);
    /// empty for a room no raid stands at the line of.
    let defendAvoidIn (threats: Threats) (room: string) : Set<Pos> =
        match briefFor threats (Defend room) |> Option.map (fun brief -> brief.Avoid) with
        | Some(Avoid.Line tiles) -> tiles
        | _ -> Set.empty

    /// The home's Defend's ground in one room, every row's together; empty
    /// for a room it holds none in (#504).
    let defendGroundIn (threats: Threats) (room: string) : Set<RoomPos> =
        briefFor threats (Defend room)
        |> Option.bind (fun brief -> brief.Ground)
        |> Option.defaultValue Set.empty

    /// One body's ground in a room's Defend: its own row's (#546), else
    /// every row's.
    let defendGroundFor (threats: Threats) (room: string) (creep: string) : Set<RoomPos> =
        briefFor threats (Defend room)
        |> Option.bind (fun brief -> Map.tryFind creep brief.Stances |> Option.orElse brief.Ground)
        |> Option.defaultValue Set.empty

    /// One room's Reach; empty for a room no Threat stands in or reaches
    /// across a border into (#514).
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// Our standing ramparts' tiles in a room as its danger reads them
    /// (`Threats.Ramparts`), so a walk, a kite and Flee count the tiles the
    /// Reach counts; in a room with no danger, as this colony's cut has them.
    let rampartsIn (threats: Threats) atlas (room: string) : Set<Pos> =
        match Map.tryFind room threats.Ramparts with
        | Some ramparts -> ramparts
        | None -> Atlas.ourRampartTilesIn atlas room

    /// One room's safe set, already joined to that room; empty for a room
    /// no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Safe
        |> Option.map (fun ground -> ground.Force())
        |> Option.defaultValue Set.empty

    /// What the hostiles whose maps reach a tile of a room land a tick on it
    /// within `ticks` (`Arrival.damageAt`); 0 in a room with no Reach.
    let damageAt (threats: Threats) (room: string) (ticks: int) (tile: Pos) : int =
        match Map.tryFind room threats.Arrivals with
        | Some(_, strikers) -> Arrival.damageAt room (strikers.Force()) ticks tile
        | None -> 0

    /// One of the colony's bodies by name, as the Threats were derived over.
    let bodyOf (threats: Threats) (creep: string) : CreepInfo option =
        threats.Bodies |> List.tryFind (fun body -> body.Name = creep)

    /// The ticks a body cannot leave the tile it stands on (its fatigue
    /// waited out, `Arrival.waitOf`), and the most a step of it takes on any
    /// ground: what its own timing adds to the horizon.
    let lagOf (creep: CreepInfo) : int * int =
        let gait = Arrival.gaitOf creep

        Arrival.waitOf creep.Fatigue gait,
        Arrival.paceOf gait
        |> Option.map (fun pace -> pace.Swamp)
        |> Option.defaultValue 0

    /// Whether a hostile strikes the tile a body stands on before it can step
    /// off it at its next decision: the room's Reach, or, for a body its
    /// fatigue holds, a map striking the tile within the horizon and its wait
    /// (movement S2). False for a body the projection does not place.
    let standsStruck (threats: Threats) atlas (creep: CreepInfo) : bool =
        match Atlas.creepTile atlas creep.Name with
        | None -> false
        | Some tile ->
            Set.contains (RoomPos.pos tile) (reachIn threats tile.Room)
            || (match Map.tryFind tile.Room threats.Arrivals, fst (lagOf creep) with
                | Some(maps, _), wait when wait > 0 ->
                    let index = indexOf (RoomPos.pos tile)

                    maps.Classes
                    |> List.exists (fun map -> Grid.at index map.Ticks <= threats.Horizon + wait)
                    && not (Set.contains (RoomPos.pos tile) (rampartsIn threats atlas tile.Room))
                | _ -> false)

    /// One body's own arrival over the room it stands in, flooded at most
    /// once a tick over its Atlas (`walksMemo`), Reach or none; None for a
    /// body the projection does not place.
    let bodyWalkOf (_: Threats) atlas (creep: CreepInfo) : Arrival.BodyArrival option =
        bodyWalk atlas creep

    /// `bodyWalkOf` on a tick with a Reach; None for any body on a tick
    /// with none, which floods nothing.
    let walkOf (threats: Threats) atlas (creep: CreepInfo) : Arrival.BodyArrival option =
        if Map.isEmpty threats.Arrivals then
            None
        else
            bodyWalkOf threats atlas creep

    /// By tile index of the room a body stands in, whether a hostile strikes
    /// the tile before the body could be on it and gone (movement S2:
    /// `A <= T_b + held`, ties unsafe), off our ramparts: what its walk goes
    /// round. A non-fighter counts every class; a fighter (`fighter`) a tile
    /// only where what strikes it then out-damages its heal (Decided 3).
    /// None in a room with no Reach, or for a body the projection does not
    /// place. Asked a tile at a time by the walk's flood, never built whole.
    let unsafeFor
        (threats: Threats)
        atlas
        (creep: CreepInfo)
        (fighter: bool)
        : (int -> bool) option =
        // Flooded only for a body a hostile can strike by the decision after
        // next, its own lag counted (movement's "only for bodies where A is
        // within the horizon"): its fatigue and its slowest step, a swamp
        // tile held long. One farther off walks round the Reach and decides
        // again nearer.
        let near =
            Atlas.creepTile atlas creep.Name
            |> Option.exists (fun at ->
                match Map.tryFind at.Room threats.Arrivals with
                | Some(maps, _) ->
                    let index = indexOf (RoomPos.pos at)
                    let wait, slowest = lagOf creep
                    let within = 2 * threats.Horizon + wait + slowest

                    maps.Classes |> List.exists (fun map -> Grid.at index map.Ticks <= within)
                | None -> false)

        match (if near then walkOf threats atlas creep else None) with
        | None -> None
        | Some walk ->
            match Map.tryFind walk.Room threats.Arrivals with
            | None -> None
            | Some(maps, strikers) ->
                let heal = Engine.healPower * partCount creep.Body Heal
                let room = walk.Room
                let ramparts = lazy (rampartsIn threats atlas room)

                Some(fun index ->
                    let leaves = Grid.at index walk.Leaves

                    leaves < System.Int32.MaxValue
                    && Arrival.soonestAmong index maps.Classes System.Int32.MaxValue <= leaves
                    && (not fighter
                        || heal = 0
                        || Arrival.damageAt room (strikers.Force()) leaves (posAt index) > heal)
                    && not (Set.contains (posAt index) ramparts.Value))

    /// Whether a fighter wins the exchange (`Facts.bodiesWin`) against every
    /// hostile whose map reaches a room, in the room each stands in (#568,
    /// movement S3): what its walk gate weighs, not a tile's damage against
    /// its heal alone. False where a tower is a member (no exchange is won
    /// under towers not ours) and in a room with no Reach.
    let winsIn (view: ColonyView) (threats: Threats) (room: string) (creep: CreepInfo) : bool =
        match Map.tryFind room threats.Arrivals with
        | None -> false
        | Some(maps, _) ->
            let members = maps.Classes |> List.collect (fun map -> map.Members) |> List.distinct

            let rooms =
                members
                |> List.map (fun id ->
                    view.Hostiles
                    |> List.tryFind (fun hostile -> hostile.Id = id)
                    |> Option.map (fun hostile -> hostile.Pos.Room))
                |> List.distinct

            not (List.isEmpty rooms)
            && rooms
               |> List.forall (function
                   | Some at -> Facts.bodiesWin view at [ creep ]
                   | None -> false)

    /// By tile index of a room, the soonest any of its maps strikes the tile:
    /// what a fighter's walk breaks an equal-tick tie by, toward the later
    /// (movement S3). None in a room with no Reach.
    let soonestIn (threats: Threats) (room: string) : (int -> int) option =
        Map.tryFind room threats.Arrivals
        |> Option.map (fun (maps, _) ->
            fun index -> Arrival.soonestAmong index maps.Classes System.Int32.MaxValue)

    /// A body's Fighter, the maps of its room whose class strikes shorter
    /// than it reaches, and the camp where it outranges the swing a camp is
    /// read as.
    let private shorterIn (threats: Threats) atlas (creep: CreepInfo) (camp: Pos option) =
        let fighter = Fighter.ofCreep creep

        let shorter =
            Atlas.creepTile atlas creep.Name
            |> Option.bind (fun at -> Map.tryFind at.Room threats.Arrivals)
            |> Option.map (fun (maps, _) ->
                maps.Classes |> List.filter (fun map -> map.Class.Strike < fighter.Reach))
            |> Option.defaultValue []

        fighter, shorter, camp |> Option.filter (fun _ -> Fighter.outranges fighter)

    /// Whether a body stands off a class of its room's maps: it keeps off
    /// every member it sees (`Facts.keepsOff`, #625), by its pace alone
    /// (`Fighter.standsOff`) where it sees none.
    let private stoodOff (view: ColonyView) (fighter: Fighter) (map: ClassArrival) : bool =
        match
            map.Members
            |> List.choose (fun id -> view.Hostiles |> List.tryFind (fun h -> h.Id = id))
        with
        | [] -> Fighter.standsOff fighter map.Class
        | members -> members |> List.forall (Facts.keepsOff view fighter)

    /// Whether a kiting body stands off none of the classes shorter than its
    /// reach in its room (#615 U1), some standing there and no camp: none
    /// it keeps off, so each picks the standoff.
    /// What the body may close by, where it also wins the fight (`Resolver`).
    let standsOffNothing
        (view: ColonyView)
        (threats: Threats)
        atlas
        (creep: CreepInfo)
        (camp: Pos option)
        : bool =
        let fighter, shorter, camp = shorterIn threats atlas creep camp

        not (List.isEmpty shorter)
        && Option.isNone camp
        && not (List.exists (stoodOff view fighter) shorter)

    /// Whether a body keeps off the raiders of some class shorter than its
    /// reach in its room (`Facts.keepsOff`, #625), one standing there at
    /// least: raiders it is priced as keeping off, which its walk must keep
    /// off too. A keeper or an unarmed body is no raider and asks no kite.
    let standsOffSome (view: ColonyView) (threats: Threats) atlas (creep: CreepInfo) : bool =
        let fighter, shorter, _ = shorterIn threats atlas creep None

        shorter
        |> List.exists (fun map ->
            match
                view.Hostiles
                |> List.filter (fun h ->
                    List.contains h.Id map.Members && HostileInfo.raider view.Hostiles h)
            with
            | [] -> false
            | raiders -> raiders |> List.forall (Facts.keepsOff view fighter))

    /// A kiting body's walk (#510, #524, movement S3), by tile index of the
    /// room it stands in: whether a map whose class strikes shorter than the
    /// body reaches (#615 U1: a melee's, against a body that shoots) strikes
    /// the tile before the body could be on it and gone, its own legs, load,
    /// parts shot off and fatigue counted (`walkOf`), never sooner than the
    /// tick after (a body that stays put moves again no sooner). A class it
    /// keeps off (`Facts.keepsOff`, #615) makes a tile unsafe only where,
    /// stepping off it, the body holds less run-off than the class presses it
    /// (`Facts.pressedFor`, of its members in the body's room;
    /// `Arrival.heldFor`), which a tile the class strikes before the body is
    /// off it never holds: the reading the body is priced as keeping it off
    /// by, so a kiter that keeps a raider off walks in as close as one that
    /// closes, and never into a pocket. And, about
    /// `camp` (the exit a raid left by, `ColonyView.ExitHolds`), a step and a
    /// swing, and a step more for each tick past the first the tile holds the
    /// body. Off our ramparts. With it, the soonest such a map strikes each
    /// tile, which an equal-tick walk breaks toward the later. None where no
    /// such map nor a camp it outranges stands in its room, or for a body the
    /// projection does not place.
    let kiteUnsafeFor
        (view: ColonyView)
        (threats: Threats)
        atlas
        (creep: CreepInfo)
        (camp: Pos option)
        : ((int -> bool) * (int -> int)) option =
        let fighter, shorter, camp = shorterIn threats atlas creep camp
        let kept, raced = shorter |> List.partition (stoodOff view fighter)

        if List.isEmpty shorter && Option.isNone camp then
            None
        else
            // A camp on a tick with no map anywhere floods the body here,
            // `walkOf` keeping none.
            match
                walkOf threats atlas creep
                |> Option.orElseWith (fun () -> Arrival.ofBody atlas creep)
            with
            | None -> None
            | Some walk ->
                let ramparts = lazy (rampartsIn threats atlas walk.Room)

                let soonest index =
                    Arrival.soonestAmong index shorter System.Int32.MaxValue

                let gait = Arrival.gaitOf creep
                let wait = Arrival.waitOf creep.Fatigue gait

                // The run-off the body must hold against what it keeps off,
                // walked from each tile as it stands there.
                let held =
                    lazy
                        (let need =
                            kept
                            |> List.collect (fun map -> map.Members)
                            |> List.choose (fun id ->
                                view.Hostiles
                                |> List.tryFind (fun h -> h.Id = id && h.Pos.Room = walk.Room))
                            |> List.map (Facts.pressedFor view fighter)
                            |> List.fold max 0

                         match
                             Map.tryFind walk.Room threats.Arrivals,
                             Arrival.paceOf gait,
                             Atlas.arrivalGroundIn atlas walk.Room false,
                             Atlas.creepTile atlas creep.Name
                         with
                         | Some(maps, _), Some pace, Some ground, Some at ->
                             Arrival.heldFor
                                 ground
                                 shorter
                                 maps.Unseen
                                 ramparts.Value
                                 pace
                                 (indexOf (RoomPos.pos at), wait)
                                 need
                         | _ -> Array.create tileCount -1)

                // A raider it keeps off that stands where it stood the full
                // tick before (`ColonyView.Parked`) is raced from the tick
                // the body stands on the tile, its fatigue waited out: the
                // walk is decided again each tick, and the raider's first
                // step is seen before the body walks on. One on the move is
                // raced from now.
                let parked =
                    kept
                    |> List.forall (fun map ->
                        map.Members |> List.forall (fun id -> Set.contains id view.Parked))

                let unsafe index =
                    let leaves = Grid.at index walk.Leaves

                    let from =
                        if parked then
                            max 0 (Grid.at index walk.Ticks - wait)
                        else
                            0

                    leaves < System.Int32.MaxValue
                    && (Arrival.soonestAmong index raced System.Int32.MaxValue <= max 1 leaves
                        || not (List.isEmpty kept)
                           && Grid.at index held.Value < max 1 (leaves - from)
                        || (match camp with
                            | Some exit ->
                                range exit (posAt index)
                                <= Engine.meleeRange + 1 + leaves - Grid.at index walk.Ticks
                            | None -> false))
                    && not (Set.contains (posAt index) ramparts.Value)

                Some(unsafe, soonest)

    /// A fleeing body's travel cost to its Flee ground, priced by `price` on
    /// its first ask over these Threats and this Atlas, recalled after.
    let fleeCostOf
        (threats: Threats)
        atlas
        (creep: string)
        (price: unit -> int option)
        : int option =
        let costs = (flightsOf threats atlas).Costs

        match costs.TryGetValue creep with
        | true, cost -> cost
        | _ ->
            let cost = price ()
            costs.[creep] <- cost
            cost

    /// Flee's ground for a body (movement S4): every tile it is on before any
    /// hostile strikes it (`T_b < A`), widest escape first — away from home,
    /// home across the border (`homeward`, #511); else the refuges it stands a
    /// decision out on, else the tiles out of the Reach it is on first, else
    /// those the hostiles reach last; never its own tile while it is struck
    /// there before it can leave. With none, the room's safe set (`safeIn`),
    /// and where the room has no safe tile at all — a room its hostile towers
    /// cover whole (#566) — the ground beside the landings across the nearest
    /// open border (`acrossFrom`). Empty for a body the projection does not
    /// place, or with nowhere to go.
    let rec fleeAreaFor (threats: Threats) atlas (creep: CreepInfo) : Set<RoomPos> =
        if Map.isEmpty threats.Arrivals then
            Set.empty
        else
            let grounds = (flightsOf threats atlas).Grounds

            match grounds.TryGetValue creep.Name with
            | true, ground -> ground
            | _ ->
                let ground = fleeGroundOf threats atlas creep
                grounds.[creep.Name] <- ground
                ground

    and private fleeGroundOf (threats: Threats) atlas (creep: CreepInfo) : Set<RoomPos> =
        match Atlas.creepTile atlas creep.Name with
        | None -> Set.empty
        | Some at ->
            let room = at.Room

            let firsts =
                lazy
                    (match walkOf threats atlas creep, Map.tryFind room threats.Arrivals with
                     | Some walk, Some(maps, _) ->
                         Some(
                             Arrival.reachedFirst walk maps.Classes (rampartsIn threats atlas room)
                         )
                     | _ -> None)

            // One pass over the grid, by the body's own arrival, its fatigue,
            // load and parts shot off counted (`walkOf`), never the safe set
            // built whole. A refuge is a tile it can be on and stand a
            // decision out before any map strikes it (its departure plus the
            // horizon, before the soonest arrival: its own tile judged by its
            // stay like any other); its ground is the refuges a horizon's
            // ticks or less past the nearest, a band, so a held tile parks
            // nobody. With no refuge, the tiles out of the Reach it is on and
            // gone before any map strikes them, as a band; with none, the
            // tiles but its own it is on and gone first, a horizon or less
            // short of the latest arrival among them: the widest escape, run
            // toward, never stood on where it is. Where it is on no tile
            // first, the nearest band out of the Reach but its own tile.
            let nearest () =
                match
                    walkOf threats atlas creep, Map.tryFind room threats.Arrivals, firsts.Value
                with
                | Some walk, Some(maps, _), Some first ->
                    let horizon = threats.Horizon
                    let wait = fst (lagOf creep)
                    let stands = Atlas.standsAt atlas room
                    let ramparts = lazy (rampartsIn threats atlas room)
                    let mutable least = System.Int32.MaxValue
                    let mutable leastNear = System.Int32.MaxValue
                    let mutable leastOut = System.Int32.MaxValue
                    let mutable latest = 0
                    // Indices, not tiles: most lie past the band, so a tile is
                    // made only for one that falls within it.
                    let refuges = ResizeArray<int>()
                    let near = ResizeArray<int>()
                    let ahead = ResizeArray<int>()
                    let out = ResizeArray<int>()

                    for index in 0 .. tileCount - 1 do
                        let mine = Grid.at index walk.Ticks

                        if mine < System.Int32.MaxValue && stands index then
                            let leaves = Grid.at index walk.Leaves

                            let soonest =
                                Arrival.soonestAmong index maps.Classes System.Int32.MaxValue

                            // The horizon is the body's next decision: this
                            // full tick and the light one after it, not Lean's
                            // third (ADR 0082), which no reader here takes.
                            if
                                first.[index]
                                && (leaves + horizon < soonest
                                    || Set.contains (posAt index) ramparts.Value)
                            then
                                least <- min least mine
                                refuges.Add index
                            elif first.[index] && leaves < soonest && horizon + wait < soonest then
                                leastNear <- min leastNear mine
                                near.Add index
                            elif first.[index] && leaves < soonest && mine > 0 then
                                latest <- max latest soonest
                                ahead.Add index
                            elif horizon < soonest && mine > 0 then
                                leastOut <- min leastOut mine
                                out.Add index

                    let mutable found = []

                    let band (tiles: ResizeArray<int>) nearest =
                        for i in 0 .. tiles.Count - 1 do
                            let index = tiles.[i]

                            if Grid.at index walk.Ticks <= nearest + horizon then
                                found <- RoomPos.at room (posAt index) :: found

                    if refuges.Count > 0 then
                        band refuges least
                    elif near.Count > 0 then
                        band near leastNear
                    elif ahead.Count = 0 then
                        band out leastOut
                    else
                        for i in 0 .. ahead.Count - 1 do
                            let index = ahead.[i]

                            if
                                Arrival.soonestAmong index maps.Classes System.Int32.MaxValue
                                >= latest - horizon
                            then
                                found <- RoomPos.at room (posAt index) :: found

                    found
                | _ -> []

            if Set.isEmpty (reachIn threats room) then
                Set.empty
            else
                match homeward threats atlas creep at firsts.Value with
                | home when not (Set.isEmpty home) -> home
                | _ ->
                    match nearest () with
                    | _ :: _ as tiles -> Set.ofList tiles
                    | [] ->
                        match safeIn threats room with
                        | safe when not (Set.isEmpty safe) -> safe
                        | _ -> acrossFrom threats atlas at (walkOf threats atlas creep)

    /// Flee's ground for a body caught in a room not its colony's home (#511):
    /// the ground beside the landings into the next room on a chain home,
    /// home or a room seen this tick, across the exits it reaches by a walk
    /// through tiles it is on first (`Arrival.reachedFirst`), whose landings lie in
    /// no Reach of that room; the nearest such room by its own walk. Home has
    /// the towers, the ramparts and the fighters; an outpost's safe set is
    /// small and moves with the raid. An armed body flees only with no fight
    /// to hold (#533), and then home as any other (#602), but in a home of
    /// ours (`Threats.Homes`), which it defends: there it flees in the room.
    /// A body on no tile but its own first — a chaser at its pace already
    /// striking it — outruns nothing in the room: its way out is an exit home
    /// it walks to that no striker there stands nearer than it, never one
    /// past the chaser, onto a landing struck later than it is on it.
    /// Empty at home, and where every way home crosses the raid: it flees in
    /// the room.
    and private homeward
        (threats: Threats)
        atlas
        (creep: CreepInfo)
        (at: RoomPos)
        (firsts: bool[] option)
        : Set<RoomPos> =
        let room = at.Room

        let defends =
            Set.contains room threats.Homes
            && (partCount creep.Body Attack > 0
                || partCount creep.Body RangedAttack > 0
                || partCount creep.Body Heal > 0)

        match Atlas.homeRoom atlas, walkOf threats atlas creep, firsts with
        | Some home, Some walk, Some(first: bool[]) when home <> room && not defends ->
            let outruns = (first |> Array.sumBy (fun f -> if f then 1 else 0)) > 1

            let strikers =
                lazy
                    (match Map.tryFind room threats.Arrivals with
                     | Some(_, strikers) -> strikers.Force()
                     | None -> [])

            let ahead (exit: Pos) =
                let mine = range (RoomPos.pos at) exit

                strikers.Value
                |> List.forall (fun striker ->
                    striker.At.Room <> room || range (RoomPos.pos striker.At) exit >= mine)

            let leaves (exit: Pos) =
                if outruns then
                    first.[indexOf exit]
                else
                    Grid.at (indexOf exit) walk.Ticks < System.Int32.MaxValue && ahead exit

            // Where it lands: a body that outruns the raid lands only out of
            // the next room's Reach; one struck where it stands is kept from
            // a tile across where a hostile standing in that room strikes it
            // by the tick it is on it (`on`, its walk to the exit and the
            // crossing), or anything strikes it no later than its own tile.
            // The chaser it leaves behind, crossing on its heels, ties the
            // crossing (the maps call a tie struck) and is no reason to stay
            // where it already strikes (#608).
            let here =
                lazy
                    (match soonestIn threats room with
                     | Some soonest -> soonest (indexOf (RoomPos.pos at))
                     | None -> System.Int32.MaxValue)

            let struckIn (next: string) : int -> Pos -> bool =
                if outruns then
                    let hot = reachIn threats next
                    fun _ tile -> Set.contains tile hot
                else
                    match soonestIn threats next, Map.tryFind next threats.Arrivals with
                    | Some soonest, Some(_, strikers) ->
                        let camped =
                            lazy
                                (strikers.Force()
                                 |> List.filter (fun striker -> striker.At.Room = next))

                        fun on tile ->
                            soonest (indexOf tile) <= here.Value
                            || Arrival.damageAt next camped.Value on tile > 0
                    | _ -> fun _ _ -> false

            let onAcross (exit: Pos) (steps: int) =
                let ticks = Grid.at (indexOf exit) walk.Ticks

                if ticks = System.Int32.MaxValue then
                    ticks
                else
                    ticks + steps

            Atlas.routes atlas room home
            |> List.choose (function
                | _ :: next :: _ -> Some next
                | _ -> None)
            |> List.distinct
            // A room no vision answers for this tick stays as risky as the
            // danger log last left it (Decided 2): only home, or a room seen
            // now, is landed in.
            |> List.filter (fun next -> next = home || Set.contains next threats.Seen)
            |> List.choose (fun next ->
                let hot = struckIn next

                match
                    Atlas.seams atlas room next
                    |> List.filter (fun (exit, landing) ->
                        leaves exit && not (hot (onAcross exit 1) landing))
                with
                | [] -> None
                | band ->
                    let ground =
                        band
                        |> List.collect (fun (exit, landing) ->
                            Atlas.adjacentWalkableIn atlas next landing
                            |> List.filter (hot (onAcross exit 2) >> not))
                        |> Set.ofList
                        |> RoomPos.setAt next

                    if Set.isEmpty ground then
                        None
                    else
                        Some(
                            (band
                             |> List.map (fun (exit, _) -> Grid.at (indexOf exit) walk.Ticks)
                             |> List.min,
                             next),
                            ground
                        ))
            |> List.sortBy fst
            |> List.tryHead
            |> Option.map snd
            |> Option.defaultValue Set.empty
        | _ -> Set.empty

    /// Flee's ground out of a room with no safe tile (#566): the ground
    /// beside each landing across the nearest border whose landings lie in no
    /// Reach of the room beyond, into a room a route enters (`Atlas.routes`:
    /// never a rival's, a stronghold's or one withheld from passage, which an
    /// unseen one would read as safe and no step would reach), nearest by the
    /// body's own walk to its exits (`walk`), an exit it cannot walk to none.
    and private acrossFrom
        (threats: Threats)
        atlas
        (at: RoomPos)
        (walk: Arrival.BodyArrival option)
        : Set<RoomPos> =
        let room = at.Room
        let pos = RoomPos.pos at

        let ticksTo (exit: Pos) =
            match walk with
            | Some walk -> Grid.at (indexOf exit) walk.Ticks
            | None -> range pos exit

        RoomName.adjacent room
        |> List.filter (fun next -> not (List.isEmpty (Atlas.routes atlas room next)))
        |> List.choose (fun next ->
            let hot = reachIn threats next

            match
                Atlas.seams atlas room next
                |> List.filter (fun (_, landing) -> not (Set.contains landing hot))
            with
            | [] -> None
            | band ->
                let ground =
                    band
                    |> List.collect (snd >> Atlas.adjacentWalkableIn atlas next)
                    |> List.filter (fun tile -> not (Set.contains tile hot))
                    |> Set.ofList
                    |> RoomPos.setAt next

                let nearest = band |> List.map (fst >> ticksTo) |> List.min

                if Set.isEmpty ground || nearest = System.Int32.MaxValue then
                    None
                else
                    Some((nearest, next), ground))
        |> List.sortBy fst
        |> List.tryHead
        |> Option.map snd
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

    /// An Assault's squad ground this tick, None where it has none.
    let formationOf (threats: Threats) (task: Task) : Formation option =
        Map.tryFind task threats.Briefs |> Option.bind (fun brief -> brief.Formation)

    /// The role a body holds an Assault in, or None where it does not admit
    /// it.
    let squadRoleOf (threats: Threats) (task: Task) (creep: string) : SquadRole option =
        formationOf threats task
        |> Option.bind (fun formation -> Map.tryFind creep formation.Roles)

    /// One member's ground in an Assault, by its role: a medic's behind,
    /// every other role's the front. Empty for a Task with no ground or a
    /// body it does not admit.
    let squadGroundIn (threats: Threats) (task: Task) (creep: string) : Set<RoomPos> =
        match formationOf threats task, squadRoleOf threats task creep with
        | Some formation, Some Medic -> formation.Behind
        | Some formation, Some _ -> formation.Front
        | _ -> Set.empty

    /// The rooms whose fight we hold: each whose Guard a body of ours holds
    /// (`Engaged`, #535).
    let fightRooms (threats: Threats) : Set<string> = threats.Engaged

    /// The one hold rule (#535): a body holding a room's Guard keeps
    /// it — admitted and counted whatever the room's kind, verdict, capacity
    /// or muster — while a target its Brief shoots stands in the room that it
    /// can strike (`Facts.canStrike`: it does not outrun the body). The
    /// verdict starts a fight and a stand-down, never ends one; it ends on the
    /// target's death, its leaving or outrunning, a lost fight — `Lose`, which
    /// its holder flees (#533) — or, for a resident lent out of its room, its
    /// lender's recall (`Facts.mayLeaveFor`).
    let holdsFight
        (view: ColonyView)
        (atlas: Atlas.Atlas)
        (threats: Threats)
        (creep: CreepInfo)
        (task: Task)
        : bool =
        match task with
        | Guard room ->
            match briefFor threats task with
            | Some brief when brief.Verdict.Value <> Answer.Lose ->
                let resident = residentRooms view
                let engaged = fightRooms threats

                let lent =
                    not (isRangerRowBody creep)
                    || Set.contains room resident
                    || Facts.mayLeaveFor view room (Atlas.creepRoom atlas creep.Name) creep

                // A fight is held by a body at it: in the room or beside
                // it, never one still walking from afar.
                let there =
                    Atlas.creepRoom atlas creep.Name
                    |> Option.exists (fun here -> here = room || RoomName.neighbouring here room)

                lent
                && there
                && view.Hostiles
                   |> List.exists (fun h ->
                       h.Pos.Room = room
                       && brief.Targets.Shoots resident engaged h
                       && Facts.canStrike view (partCount creep.Body) h)
            | _ -> false
        | _ -> false

/// The kill order's head among one room's targets (`Facts.killRank`, its
/// side's kill scores `score`), ties by id; each target's rank priced once,
/// not once a comparison.
let private killHead
    (view: ColonyView)
    (room: string)
    (score: HostileInfo -> float)
    (targets: HostileInfo list)
    =
    match targets with
    | [] -> None
    | _ ->
        let rank = killRank view room score

        targets
        |> List.map (fun hostile -> (rank hostile, hostile.Id), hostile)
        |> List.minBy fst
        |> snd
        |> Some

/// A melee's step and swing: the tiles outside the home's line a raid melee
/// at it reaches, which a defender's walk keeps off (#504).
let private stepAndSwing = Engine.meleeRange + 1

/// The reach the kite ground is laid for: a shot's, the body that outranges
/// a swing (`Fighter.outranges`).
let private kiteReach = Engine.rangedRange

/// By tile index, the soonest a class of a room's maps that strikes shorter
/// than `reach` strikes the tile (`Arrival`, movement S3; #615 U1: the
/// classes a body of that reach stands off); `Int32.MaxValue` where none
/// ever does.
let private shorterSoonest (reach: int) (maps: ArrivalMaps option) : int -> int =
    match
        maps
        |> Option.map (fun maps ->
            maps.Classes |> List.filter (fun map -> map.Class.Strike < reach))
    with
    | None
    | Some [] -> fun _ -> System.Int32.MaxValue
    | Some melee -> fun index -> Arrival.soonestAmong index melee System.Int32.MaxValue

/// The kite ground (#510, movement S3): the tiles of one room within the
/// head's reach that a body standing there steps off before any map shorter
/// than its reach strikes them (#615 U1: a melee arrival later than the tick
/// after), off the exit
/// tiles, which carry a body out of the room. Within a tile less of the head
/// where any is, while the head can step this tick (#535): its step away
/// leaves such a tile in reach, where one at the edge is out of it the tick
/// after. Of those, the ones with escape room: the open tiles about each,
/// within the horizon's ticks, that a melee reaches no sooner than the tile
/// itself (off swamp and the exits, read off the maps and never flooded), at
/// least half the widest's, so the retreat never backs into a corridor,
/// corner or swamp pocket a melee pins it in. Which of them one body may
/// stand on, its own legs and fatigue counted, is its walk's
/// (`Threats.kiteUnsafeFor`).
let private kiteGroundIn
    atlas
    (room: string)
    (maps: ArrivalMaps option)
    (horizon: int)
    (inRoom: HostileInfo list)
    (head: HostileInfo option)
    (stepsNow: HostileInfo -> bool)
    =
    let anyMelee = inRoom |> List.exists (HostileInfo.melee inRoom)

    let standing =
        inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

    let soonest = shorterSoonest kiteReach maps

    let isOpen (tile: Pos) =
        not (Set.contains tile standing)
        && not (Seam.onRing tile)
        && soonest (indexOf tile) > 1

    let escapeRoom (tile: Pos) =
        let from = soonest (indexOf tile)

        Atlas.walkableWithinIn atlas room horizon tile
        |> List.sumBy (fun near ->
            if
                near <> tile
                && isOpen near
                && soonest (indexOf near) >= from
                && not (Atlas.isSwampIn atlas room near)
            then
                1
            else
                0)

    match head with
    | None -> Set.empty, lazy Set.empty
    | Some target ->
        let at = RoomPos.pos target.Pos

        let within reach =
            Atlas.walkableWithinIn atlas room reach at |> List.filter isOpen

        let reach, tiles =
            match (if stepsNow target then within (kiteReach - 1) else []) with
            | [] -> kiteReach, within kiteReach
            | near -> kiteReach - 1, near

        let ground, widest =
            match anyMelee, tiles with
            | false, _
            | _, [] -> tiles, 0
            | _ ->
                let roomOf = tiles |> List.map (fun tile -> tile, escapeRoom tile)
                let widest = roomOf |> List.map snd |> List.max

                roomOf |> List.filter (fun (_, space) -> 2 * space >= widest) |> List.map fst,
                widest

        let ground = ground |> Set.ofList

        // The band a kiter closes on the ground by (#595): the same tests,
        // one to three tiles past it.
        let approach =
            lazy
                (if Set.isEmpty ground then
                     Set.empty
                 else
                     Atlas.walkableWithinIn atlas room (reach + 3) at
                     |> List.filter (fun tile ->
                         range tile at > reach
                         && isOpen tile
                         && (widest = 0 || 2 * escapeRoom tile >= widest))
                     |> Set.ofList
                     |> RoomPos.setAt room)

        RoomPos.setAt room ground, approach

/// The home's Defend's ground (#504), off this tick's raid at the line: every
/// perimeter rampart of ours within its row's reach of a raid body — a
/// defender's swing, a ranger's shot (#546) — that no body but the row's
/// stands on — the mother's lent residents and an ally on a public rampart
/// among them (`ColonyView.Foreign`) — or, with none in reach, those nearest
/// the raid.
let private defendGroundOf
    (view: ColonyView)
    (row: CastRow)
    (atLine: HostileInfo list)
    : Set<RoomPos> option =
    match atLine with
    | [] -> None
    | raid ->
        let reach =
            if row = CastRow.Ranger then
                Engine.rangedRange
            else
                Engine.meleeRange

        let taken =
            ColonyView.bodies view
            |> List.filter (fun creep -> not (CastRow.cut row creep.Name creep.Body))
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
            match free |> List.filter (fun tile -> gapTo tile <= reach) with
            | [] ->
                match free with
                | [] -> Set.empty
                | _ ->
                    let nearest = free |> List.map gapTo |> List.min
                    free |> List.filter (fun tile -> gapTo tile = nearest) |> Set.ofList
            | inReach -> Set.ofList inReach

        if Set.isEmpty ground then None else Some ground

/// The world's danger (`WorldDanger.read`) in every room of the projection
/// it reads one in, less a room of ours under safe mode (`shielded`), where
/// nothing hurts.
let private dangersOf (view: ColonyView) (shielded: string -> bool) : Map<string, RoomDanger> =
    WorldDanger.read view
    |> Map.toList
    |> List.choose (fun (room, danger) ->
        if Map.containsKey room view.Spatial.Rooms && not (shielded room) then
            danger.Force() |> Option.map (fun danger -> room, danger)
        else
            None)
    |> Map.ofList

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

/// This tick's Threats, off the world's danger room by room (`dangersOf`): a
/// room's Reach is where a map strikes within the horizon, across a border
/// too (#514), less every tile under one of our standing ramparts. The held
/// facts say which rangers hold each room's Guard, which an outmatched room's
/// safe ground is laid for.
let private threatsHeldBy (view: ColonyView) atlas (held: HeldTaskFacts) : Threats =
    // Under safe mode a hostile in a room of ours can hurt nothing, so it is no
    // Threat and has no Reach.
    let shielded room =
        match Map.tryFind room view.RoomControl with
        | Some control -> control.Owner = Ownership.Ours && control.SafeMode
        | None -> false

    // The world's danger, built once a tick a contested room for every
    // colony (`WorldDanger`): a quiet tick builds none. Its errand rooms'
    // ground, below, is a ring of eight tiles a Reactor.
    let dangers = dangersOf view shielded

    let arrivals =
        dangers |> Map.map (fun _ danger -> danger.HostileMaps, danger.Strikers)

    // Nothing left to run from once our own ramparts have taken the whole
    // Reach back: no Reach, no entry, no safe set.
    let reach =
        dangers
        |> Map.filter (fun _ danger -> not (Set.isEmpty danger.Reach))
        |> Map.map (fun _ danger -> danger.Reach)

    let ring =
        match
            view.Hostiles
            |> List.filter (fun hostile ->
                not (shielded hostile.Pos.Room)
                && Option.isSome (HostileInfo.weaponRange view.Hostiles hostile))
            |> List.map (fun hostile -> hostile.Pos.Room, RoomPos.pos hostile.Pos)
        with
        | [] -> Map.empty
        | threats ->
            let byRoom = threats |> List.groupBy fst

            // Less the tiles the Threats stand on: with two of them adjacent each
            // is a tile of the other's ring. Off the Threat list and not off the
            // Reach above, because the Reach has our ramparts subtracted and a
            // rampart is the best tile a guard can fight from.
            byRoom
            |> List.map (fun (room, inRoom) ->
                let standing = inRoom |> List.map snd |> Set.ofList

                // Asked of the grid a tile at a time rather than against the
                // room's materialised walkable ground (#371): a ring is nine
                // tiles a Threat and the ground is two thousand.
                let around (tiles: Pos list) =
                    tiles
                    |> List.collect (fun pos ->
                        Atlas.adjacentWalkableIn atlas room pos
                        |> List.filter (fun tile -> not (Set.contains tile standing)))
                    |> Set.ofList

                // A raid that outruns the row's guard (`Facts.canStrike`,
                // #531) is held off our work, not chased: the guard holds
                // the tiles beside the room's Posts, which a raider must
                // come within reach of to hurt the work there.
                let guard part =
                    max 1 (guardBlocksReach view) * partCountIn guardPattern.Block part

                let posts = Atlas.postsIn atlas room |> Set.toList

                let tiles =
                    if
                        List.isEmpty posts
                        || Facts.strikesEvery view guard (HostileInfo.raider view.Hostiles) room
                    then
                        around (inRoom |> List.map snd)
                    else
                        Set.difference (around posts) (Set.ofList posts)

                room, RoomPos.setAt room tiles)
            |> Map.ofList

    let safeGround = reach |> Map.map (fun room _ -> dangers.[room].Safe)

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

    // The kill scores each room's side prices its hostiles at (#621): its
    // Guard's holders', priced on first read, once a room.
    let sideScores =
        view.Hostiles
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> List.distinct
        |> List.map (fun room ->
            room,
            lazy
                (let holders = HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList

                 let side =
                     ColonyView.bodies view
                     |> List.filter (fun creep -> holders.Contains creep.Name)

                 scoreAmong view atlas side side room))
        |> Map.ofList

    let sideScore room : HostileInfo -> float =
        match Map.tryFind room sideScores with
        | Some score -> score.Value
        | None -> fun _ -> 0.0

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
                |> List.partition (HostileInfo.armed inRoom)

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
                ColonyView.bodies view
                |> List.filter (fun creep -> partCount creep.Body RangedAttack > 0)
                |> List.choose (fun creep -> Atlas.creepTile atlas creep.Name)
                |> List.filter (fun tile -> tile.Room = room)

            // A body off the work spots is chased when the ranger catches it
            // (user, 2026-10-04): slower than a ranger's step a tick on plain
            // (`HostileInfo.gait`), or already a step from a shooter's reach,
            // where a chase at one pace keeps it under fire.
            let catchable (hostile: HostileInfo) =
                Gait.ticks Gait.plain (HostileInfo.gait inRoom hostile)
                |> Option.forall (fun ticks -> ticks > 1)
                || shooters
                   |> List.exists (fun tile -> within tile (Engine.rangedRange + 1) hostile)

            let chased =
                unarmedTargets
                |> List.filter (fun hostile ->
                    not (within h.Stand 2 hostile)
                    && not (within h.Controller 3 hostile)
                    && catchable hostile)

            // The one body at the source or caught off it worth most a tick
            // of the side's effort (`sideScore`, #621), which the shot's kill
            // order leads with too. Ground round two bodies turned the ranger
            // between them: a hauler stepping on and off the source's work
            // spot and a slow miner at the room's other source (W19S25,
            // t943,777).
            let atSource = unarmedTargets |> List.filter (within h.Stand 2)
            let digs = HostileInfo.works inRoom
            let score = sideScore room

            let reachOf targets =
                targets
                |> List.map (fun (hostile: HostileInfo) ->
                    Engine.rangedRange, RoomPos.pos hostile.Pos)

            let working =
                match atSource @ chased with
                | _ :: _ as caught ->
                    caught
                    |> List.sortBy (fun hostile -> -(score hostile), hostile.Id)
                    |> List.truncate 1
                    |> reachOf
                | [] when unarmedTargets |> List.exists digs -> seats
                | [] -> unarmedTargets |> List.filter (within h.Controller 3) |> reachOf

            // An armed target turns the ambush into its fight where a holder
            // of the Guard fits that fight beside the others (`Facts.fits`,
            // #611): the ground is beside it alone, and the work spots wait
            // for the room to empty of it. Where none fits, the harassment
            // carries on beside it, and its verdict stands it down or not.
            let fought =
                lazy
                    (let holders = HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList

                     let holding =
                         ColonyView.bodies view
                         |> List.filter (fun creep -> holders.Contains creep.Name)

                     holding
                     |> List.exists (fun creep ->
                         Facts.fits
                             view
                             Stance.Standoff
                             room
                             view.Tuning.ReachHorizon
                             true
                             creep
                             (holding
                              |> List.filter (fun other -> other.Name <> creep.Name)
                              |> List.map (fun other -> other, true, Some 0))))

            let besideArmed =
                armedTargets |> List.map (fun hostile -> 1, RoomPos.pos hostile.Pos)

            let ambushed =
                match armedTargets with
                | [] -> working
                | _ when fought.Value -> besideArmed
                | _ -> besideArmed @ working

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

    // The rooms a hostile stands in whose Guard a living body of ours holds
    // (`Threats.Engaged`, #535).
    let engaged =
        view.Hostiles
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> List.filter (fun room ->
            not (List.isEmpty (HeldTaskFacts.holdersOf held (taskId (Guard room)))))
        |> Set.ofList

    // Each room a hostile stands in: this colony's own row there
    // (`Facts.rowAnswer`, #535), as the room's verdict priced it where it
    // weighed this colony's row, the room's one verdict (`Facts.answered`, the
    // world's) and the rangers that win it, each read on first use. Any other
    // room has no raider, and is `Win`.
    let home = SpatialInfo.homeName view.Spatial

    let judged =
        view.Hostiles
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> List.distinct
        |> List.map (fun room -> room, lazy (Facts.verdictOf view room))
        |> Map.ofList

    let rows =
        judged
        |> Map.map (fun room verdict ->
            lazy
                (match Map.tryFind home verdict.Value.Rows with
                 | Some row -> row
                 | None -> rowAnswer view room))

    let verdicts =
        judged
        |> Map.map (fun _ verdict ->
            lazy (verdict.Value.Answer, not (Set.isEmpty verdict.Value.Own)),
            lazy verdict.Value.Rangers.Value)

    let verdictOf room =
        Map.tryFind room verdicts
        |> Option.map (fun (verdict, _) -> verdict.Value)
        |> Option.defaultValue (Answer.Win, true)

    // Whether a ranger answers a room a hostile stands in; any other room is
    // the guard's.
    let answeredByRanger room =
        Map.tryFind room rows |> Option.exists (fun row -> row.Value.Ranger)

    // The rooms a ranger chases a raid in (#510): in sight, `chased`'s; out
    // of it, an outpost shut on a raid a ranger kites down, while no raid
    // is seen there that its own row answers. The row is the gate: a look
    // that finds the room empty ends the chase there (`Observe.strikeGone`).
    let chasedRooms =
        let kept room =
            let holders = HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList

            ColonyView.bodies view
            |> List.exists (fun creep -> Set.contains creep.Name holders && isRangerRowBody creep)

        verdicts
        |> Map.keys
        |> Seq.filter (fun room -> chased view (verdictOf room) (kept room) room)
        |> Set.ofSeq
        |> Set.union (view.ChasedOutposts |> Set.filter (raided view >> not))

    // A resident room's raid (#552): every body we shoot there that fights
    // or heals, or bounces on its exit (`Facts.bouncingInto`), which stands
    // at the room's line on the tick it is across too.
    let residentRaid room =
        Facts.bouncingInto view room
        |> List.filter (fun hostile ->
            (HostileInfo.armed view.Hostiles hostile
             || HostileInfo.heals view.Hostiles hostile)
            && guardShoots view resident Set.empty hostile)

    // Whether a body worth its kill stands unarmed in a room (#648): one
    // shot disarmed or bare, which its healer heals back into a raider, is
    // struck while its side's kill score on it pays (#621), armed or not.
    let worthIn room (hostile: HostileInfo) =
        hostile.Pos.Room = room
        && not (HostileInfo.armed view.Hostiles hostile)
        && sideScore room hostile > 0.0

    // The ranger rooms an armed raid, or a rival's claimer, stands in, the
    // chased rooms a raid stands in, the raided rooms a ranger answers
    // (#535), the resident rooms a raid stands at (#552), and those a body
    // worth its kill stands in (`worthIn`).
    let contested =
        (view.Hostiles
         |> List.filter (fun hostile ->
             Set.contains hostile.Pos.Room rangers
             && (Map.containsKey hostile.Pos.Room ring
                 || claimsAFlag view.Hostiles resident hostile))
         |> List.map (fun hostile -> hostile.Pos.Room))
        @ (chasedRooms |> Set.filter ring.ContainsKey |> Set.toList)
        @ (ring.Keys
           |> Seq.filter (fun room -> not (Set.contains room rangers) && answeredByRanger room)
           |> List.ofSeq)
        @ (match view.Hostiles, view.RecalledHealers with
           | [], [] -> []
           | _ -> resident |> Set.filter (residentRaid >> List.isEmpty >> not) |> Set.toList)
        @ (view.Hostiles
           |> List.filter (fun hostile ->
               let room = hostile.Pos.Room

               Set.contains room resident
               && guardShoots view resident engaged hostile
               && worthIn room hostile)
           |> List.map (fun hostile -> hostile.Pos.Room))
        |> List.distinct

    let kite =
        contested
        |> List.choose (fun room ->
            let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)

            let melee =
                inRoom
                |> List.filter (HostileInfo.melee inRoom)
                |> List.map (fun hostile -> RoomPos.pos hostile.Pos)

            let standing =
                inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

            let maps = Map.tryFind room arrivals |> Option.map fst
            let soonest = shorterSoonest kiteReach maps

            let head =
                inRoom
                // What the Emitter shoots there (`Threats.fightRooms`): a
                // body left in a room whose fight we hold — a cripple shot
                // disarmed, a healer — is the head the ground follows (#648).
                |> List.filter (guardShoots view resident engaged)
                |> killHead view room (sideScore room)

            // Whether a hostile can step this tick: a MOVE acting and its
            // fatigue paid (`Arrival.waitOf`).
            let stepsNow (hostile: HostileInfo) =
                let gait = HostileInfo.gait inRoom hostile

                let fatigue =
                    Map.tryFind room view.World.Rooms
                    |> Option.bind (fun seen -> Map.tryFind hostile.Id seen.HostileFatigue)
                    |> Option.defaultValue 0

                gait.Moves > 0 && Arrival.waitOf fatigue gait = 0

            let aroundHead () =
                kiteGroundIn atlas room maps view.Tuning.ReachHorizon inRoom head stepsNow

            // Under an armed raid, our standing ramparts within three of the
            // head (#467): a creep on its own rampart takes no damage, so
            // they are safe ground whatever the melee reaches. In a resident
            // room with no head to read, within three of its raid (#552).
            let onRamparts () =
                let near (raid: HostileInfo list) =
                    (match Map.tryFind room dangers with
                     | Some danger -> danger.Ramparts
                     | None -> Atlas.ourRampartTilesIn atlas room)
                    |> Set.filter (fun tile ->
                        raid
                        |> List.exists (fun target ->
                            range tile (RoomPos.pos target.Pos) <= Engine.rangedRange)
                        && not (Set.contains tile standing))
                    |> RoomPos.setAt room

                match head with
                | Some target when Map.containsKey room ring -> near [ target ]
                | _ when Set.contains room resident -> near (residentRaid room)
                | _ -> Set.empty

            // The safe set, two thousand tiles (#371), is the outmatched
            // room's answer alone, and only for a ranger of ours to hold it:
            // one standing there, or one holding its Guard on the way in.
            let rangerHere =
                lazy
                    (let guarding =
                        HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList

                     ColonyView.bodies view
                     |> List.exists (fun creep ->
                         partCount creep.Body RangedAttack > 0
                         && (Set.contains creep.Name guarding
                             || SpatialInfo.creepPlacementOf view.Spatial creep.Name
                                |> Option.exists (fun tile -> tile.Room = room))))

            let safe () =
                Map.tryFind room safeGround
                |> Option.map (fun ground -> ground.Force())
                |> Option.defaultValue Set.empty

            // Nowhere to kite and nowhere safe: the room's tiles a melee
            // strikes last, or, where one strikes every one by the tick after,
            // the tiles our rangers already hold.
            let farthest () =
                let ground =
                    Atlas.walkableTilesIn atlas room
                    |> Set.filter (fun tile -> not (Set.contains tile standing))

                let held =
                    ColonyView.bodies view
                    |> List.filter (fun creep -> partCount creep.Body RangedAttack > 0)
                    |> List.choose (fun creep ->
                        SpatialInfo.creepPlacementOf view.Spatial creep.Name)
                    |> List.filter (fun tile -> tile.Room = room)
                    |> Set.ofList

                if Set.isEmpty ground then
                    held
                else
                    let last (tile: Pos) = soonest (indexOf tile)
                    let widest = ground |> Seq.map last |> Seq.max

                    if widest <= 1 && not (Set.isEmpty held) then
                        held
                    else
                        ground |> Set.filter (fun tile -> last tile = widest) |> RoomPos.setAt room

            let claimerLeads = head |> Option.exists (claimsAFlag inRoom resident)

            // A resident room's head worth its kill (`worthIn`): the
            // garrison's ground follows it off its ring (#648), but never
            // while an armed raider stands at the room's line, across its
            // exit too (`residentRaid`): the ring is the raid's to hold
            // first. A healer whose raid is gone is no raider: it is a
            // target itself, and stands in no kill's way.
            let worthLeads =
                Set.contains room resident
                && head |> Option.exists (worthIn room)
                && not (residentRaid room |> List.exists (HostileInfo.armed view.Hostiles))

            let outmatchedHere =
                Set.contains room resident && fst (verdictOf room) = Answer.Lose

            // Only the ground about the head has an approach band (#595).
            let bare ground () = ground (), lazy Set.empty

            [
                bare onRamparts

                if outmatchedHere then
                    if rangerHere.Value then
                        bare safe
                elif not (List.isEmpty melee) || claimerLeads || worthLeads then
                    aroundHead
                if not (List.isEmpty melee) && (not outmatchedHere || rangerHere.Value) then
                    bare farthest
            ]
            |> List.tryPick (fun ground ->
                let tiles, approach = ground ()

                if Set.isEmpty tiles then
                    None
                else
                    Some(room, (tiles, approach))))
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
        match atLine |> List.filter (HostileInfo.melee view.Hostiles) with
        | [] -> Set.empty
        | melee ->
            outside
            |> Set.filter (fun tile ->
                melee |> List.exists (fun m -> range (RoomPos.pos m.Pos) tile <= stepAndSwing))

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
                && HostileInfo.armed view.Hostiles hostile)
            || Set.exists beside view.RaidedOutposts

        if nextDoor && not (shielded home) then
            let hot = Map.tryFind home reach |> Option.defaultValue Set.empty
            Map.ofList [ home, lazy (shelterOf view atlas hot) ]
        else
            Map.empty

    // The trespassers' ground (#516), off the exits.
    //
    // A digger or a claimer stands at its work: the tiles beside it. So does
    // a trespasser walking off: chased, the ground about the body closing to
    // our reach (#619). A hauler keeps a ranger's pace, so one coming to draw
    // is waited for at the store nearest it, within `haulGround` of it, which
    // it comes within a ranger's shot of to draw at all — only while it is
    // within a step of that ring and its last step (`ColonyView.Stepped`) took
    // it no farther from the store. One stepping away is leaving, loaded or
    // not (W17S26 t1,017,292: an empty hauler walked past the container and
    // out while two rangers stood on its ring), and one farther off is beside
    // itself: the switch sits where the two grounds meet, so a hauler swaying
    // on it moves no body standing on both (a 5-tile switch flipped the ground
    // 4 tiles every step, t1,017,270–275). Only while no trespasser the
    // guard's swing catches stands in its room (`Facts.strikesAtStandoff`,
    // #550): beside one, a body on the store ring has no target in reach nor
    // a step away, so the hauler gets the tiles beside it too.
    //
    // A trespasser on the exit row of a room beside one we mine is carried
    // into it next tick, and back if it is bouncing: keyed to that room, the
    // tiles beside where it lands, so the kill holds across the bounce.
    let trespassIn (open': string -> bool) =
        let haulGround = 2
        let waitReach = haulGround + 1

        // A carrier, not a cripple shot disarmed, which is struck where it
        // stands (#535).
        let hauls (hostile: HostileInfo) =
            List.contains Carry hostile.Body
            && not (HostileInfo.breaks view.Hostiles hostile)

        let caught =
            lazy
                (view.Hostiles
                 |> List.filter (fun hostile ->
                     trespasser view engaged hostile
                     && Facts.strikesAtStandoff view (Facts.blockFighter guardPattern) hostile)
                 |> List.map (fun hostile -> hostile.Pos.Room)
                 |> Set.ofList)

        let storesIn room =
            [ Structure BuiltKind.Container; Dropped Energy; Dropped Thorium; Tombstone ]
            |> List.collect (Atlas.idsOfKind atlas)
            |> List.choose (Atlas.positionOf atlas)
            |> List.filter (fun tile -> tile.Room = room)
            |> List.map RoomPos.pos

        let around (hostile: HostileInfo) : (string * Pos list) list =
            let room = hostile.Pos.Room
            let at = RoomPos.pos hostile.Pos

            if trespasser view engaged hostile then
                let beside () = Atlas.adjacentWalkableIn atlas room at

                let tiles =
                    if hauls hostile && not (Set.contains room caught.Value) then
                        let leaving store =
                            Map.tryFind hostile.Id view.Stepped
                            |> Option.exists (fun left ->
                                range store (RoomPos.pos left) < range store at)

                        match
                            storesIn room |> List.filter (fun store -> range store at <= waitReach)
                        with
                        | [] -> beside ()
                        | near ->
                            let store = near |> List.minBy (fun store -> range store at, store)

                            if leaving store then
                                beside ()
                            else
                                Atlas.walkableWithinIn atlas room haulGround store
                                |> List.filter ((<>) store)
                    else
                        beside ()

                [ room, tiles ]
            elif
                Seam.onRing at
                && trespassingBody view.Hostiles hostile
                // Struck as if its room were ours: never an ally's, nor a
                // neutral's that carried no weapon (#597).
                && mayStrike view (Set.singleton room) hostile
            then
                // The tile it lands on across the exit it stands on.
                RoomName.adjacent room
                |> List.filter (trespassRooms view)
                |> List.choose (fun next ->
                    match
                        Atlas.seams atlas room next
                        |> List.filter (fun (exit, _) -> exit = at)
                        |> List.collect (snd >> Atlas.adjacentWalkableIn atlas next)
                    with
                    | [] -> None
                    | tiles -> Some(next, tiles))
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

    // Whether a kind's claim on a room pools a Task this tick, as `planTasks`
    // pools it (#542): a kill always; any kind where the room's Guard is
    // pooled whatever claims it (`Facts.guardedRooms`), or where a body of
    // ours holds that Guard beside a hostile (`engaged`, the held fight); a
    // held exit where a body holds its Guard. `engaged` is a superset of the
    // Planner's `fought` (which also asks `holdsFight` — circular here, as it
    // reads the Brief): where they differ the kill pools a tick later, never
    // stalls. The guarded rooms are derived
    // only where a claim ahead would otherwise shut a later one out.
    let guarded =
        lazy
            (Facts.guardedRooms view atlas (Facts.declaredOutposts view) (Facts.fuelledErrands view)
             |> Set.ofList)

    let pools kind room =
        BriefKind.isKill kind
        || Set.contains room engaged
        || kind = BriefKind.ExitHold && Set.contains (taskId (Guard room)) held.All
        || Set.contains room guarded.Value

    // Each kind's claim, down `BriefKind.order`, on the rooms still open to
    // it: the first four claim their rooms outright, a held exit, a core and
    // a trespass only the rooms they lay ground in. A claim that pools nothing
    // shuts nobody out. Per room, its claimants in that order; per kind, the
    // ground it laid.
    // Whether a level-0 core in a room comes before its trespassers (#558):
    // where only the guard row answers them and its swing catches none of
    // them, a trespass kill would hold the guard where it strikes nothing, and
    // the core would stand for ever. The guard breaks the core, swinging at
    // a trespasser that comes in reach first; a body it catches, or one a
    // ranger answers, is still struck first.
    let coreFirst room =
        Facts.expansionCores view |> List.exists (fun core -> core.RoomName = room)
        && Brief.trespassSplit view (HeldTaskFacts.holdersOf held (taskId (Guard room))) room
           |> List.forall (fun (row, trespassers) ->
               match row with
               | CastRow.Guard ->
                   let guard = Facts.blockFighter guardPattern
                   trespassers |> List.forall (Facts.strikesAtStandoff view guard >> not)
               | _ -> List.isEmpty trespassers)

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
            | BriefKind.Trespass ->
                fun open' -> trespassIn (fun room -> open' room && not (coreFirst room))

        ((Map.empty, Map.empty), BriefKind.order)
        ||> List.fold (fun (claimants: Map<string, BriefKind list>, laid) kind ->
            let open' room =
                Map.tryFind room claimants
                |> Option.forall (
                    List.forall (fun ahead -> BriefKind.opens ahead kind || not (pools ahead room))
                )

            let claims = claimOf kind open'

            let claimants =
                (claimants, claims)
                ||> List.fold (fun claimants (room, _) ->
                    Map.change
                        room
                        (fun prior -> Some(Option.defaultValue [] prior @ [ kind ]))
                        claimants)

            claimants, Map.add kind claims laid)

    // A room's claims, those that pool first, so its kind is the first in
    // order that pools; where none does, the first, which pools nothing.
    let claimsIn room =
        match Map.tryFind room claimants with
        | Some(_ :: _ :: _ as claims) ->
            let pooling, idle = claims |> List.partition (fun kind -> pools kind room)
            pooling @ idle
        | Some claims -> claims
        | None -> []

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
            let verdict, winners =
                match Map.tryFind room verdicts with
                | Some(verdict, winners) -> lazy (fst verdict.Value), winners
                | None -> lazy Answer.Win, lazy None

            Guard room,
            Brief.claimed
                view
                atlas
                room
                (Map.containsKey room ring)
                verdict
                winners
                (lazy (answeredByRanger room))
                (lazy
                    (Set.contains room view.RaidedOutposts
                     && Map.tryFind room judged
                        |> Option.exists (fun verdict -> verdict.Value.Casting.Contains home)))
                (lazy
                    (Brief.trespassSplit
                        view
                        (HeldTaskFacts.holdersOf held (taskId (Guard room)))
                        room))
                (HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList)
                (claimsIn room)
                {
                    Kite = Map.tryFind room kite |> Option.map fst
                    Approach =
                        Map.tryFind room kite
                        |> Option.map snd
                        |> Option.defaultValue (lazy Set.empty)
                    Resident = Map.tryFind room residentRing
                    Harass = Map.tryFind room harassRing
                    Ring = Map.tryFind room rings
                    Trespass = Map.tryFind room trespass
                })
        |> Map.ofSeq

    // Every Guard's holders seated on its ground (`Brief.seatsAmong`, #650),
    // each on its own: a core's ranged holder on the core's range-3 ring
    // (#558), shooting it from out of the swing of a melee body beside it,
    // never the ring beside the core; a chase's melee holder on the ring
    // beside the raid (#571); every other on the Brief's ground. Matched by
    // each holder's own arrival over the room it stands in.
    let briefs =
        let seatsIn room (brief: Brief) =
            let coreRing =
                lazy
                    (Facts.expansionCores view
                     |> List.filter (fun core -> core.RoomName = room)
                     |> List.collect (fun core ->
                         Atlas.walkableWithinIn atlas room Engine.rangedRange core.Tile
                         |> List.filter (fun tile ->
                             range tile core.Tile = Engine.rangedRange && not (Seam.onRing tile)))
                     |> Set.ofList
                     |> RoomPos.setAt room)

            let groundOf (creep: CreepInfo) =
                match brief.Duty, brief.Laid.Ring with
                | Duty.Guard BriefKind.Core, _ when isRangerRowBody creep ->
                    Some(coreRing.Value, true)
                | Duty.Guard BriefKind.Chase, Some ring when
                    not (Set.isEmpty ring) && isGuardRowBody creep
                    ->
                    Some(ring, false)
                | _ -> brief.Ground |> Option.map (fun ground -> ground, false)

            let arrivalOf (creep: CreepInfo) =
                match bodyWalk atlas creep with
                | Some walk when walk.Room = room ->
                    fun (tile: RoomPos) ->
                        if tile.Room = room then
                            match Grid.at (indexOf (RoomPos.pos tile)) walk.Ticks with
                            | ticks when ticks = System.Int32.MaxValue -> None
                            | ticks -> Some ticks
                        else
                            None
                | _ -> fun _ -> None

            // A kill's holder is seated about the body worth most a tick of
            // its own effort (#621, `scoreOf`): the ground within its reach
            // of that body, where it reaches a tile of it; else, or where it
            // kills none, the whole ground.
            let holders =
                HeldTaskFacts.holdersOf held (taskId (Guard room))
                |> List.choose (fun name ->
                    ColonyView.bodies view |> List.tryFind (fun creep -> creep.Name = name))

            let scores = lazy (sideScoresIn view holders room)

            let seatGroundOf (creep: CreepInfo) =
                match brief.Duty, groundOf creep with
                | Duty.Guard BriefKind.Trespass, Some(ground, false) ->
                    let score = scoreOf atlas scores.Value creep
                    let reach = Fighter.reachOf creep
                    let arrival = arrivalOf creep

                    let head =
                        view.Hostiles
                        |> List.filter (fun hostile ->
                            hostile.Pos.Room = room
                            && brief.Targets.Shoots resident engaged hostile)
                        |> List.map (fun hostile -> score hostile, hostile)
                        |> List.filter (fun (worth, _) -> worth > 0.0)
                        |> List.sortBy (fun (worth, hostile) -> -worth, hostile.Id)
                        |> List.tryHead

                    let near =
                        match head with
                        | Some(_, target) ->
                            ground
                            |> Set.filter (fun tile ->
                                RoomPos.range tile target.Pos
                                |> Option.exists (fun r -> r <= reach))
                        | None -> Set.empty

                    if
                        near.Count < ground.Count && near |> Set.exists (arrival >> Option.isSome)
                    then
                        Some(near, true)
                    else
                        Some(ground, false)
                | _, seated -> seated

            holders
            |> List.choose (fun creep ->
                seatGroundOf creep
                |> Option.map (fun (ground, own) -> creep.Name, ground, own, arrivalOf creep))
            |> Brief.seatsAmong

        briefs
        |> Map.map (fun task brief ->
            match task, brief.Duty with
            | Guard room, Duty.Guard _ ->
                { brief with
                    Seats = lazy (seatsIn room brief)
                }
            | _ -> brief)

    // The home's Defend while a raid stands at its line (#504).
    let briefs =
        match atLine with
        | [] -> briefs
        | _ ->
            let home = SpatialInfo.homeName view.Spatial

            let verdict =
                match Map.tryFind home verdicts with
                | Some(verdict, _) -> lazy (fst verdict.Value)
                | None -> lazy Answer.Win

            // Each row's ground off the raiders it answers (#546), and each
            // body of either row held to its own row's.
            let holders =
                HeldTaskFacts.holdersOf held (taskId (Guard home))
                @ HeldTaskFacts.holdersOf held (taskId (Defend home))
                |> Set.ofList

            let answered = Brief.defendSplit view holders atLine

            let grounds =
                answered
                |> List.choose (fun (row, raid) ->
                    defendGroundOf view row raid |> Option.map (fun ground -> row, ground))

            let stances =
                ColonyView.bodies view
                |> List.choose (fun creep ->
                    grounds
                    |> List.tryFind (fun (row, _) -> CastRow.cut row creep.Name creep.Body)
                    |> Option.map (fun (_, ground) -> creep.Name, ground))
                |> Map.ofList

            let ground =
                match grounds with
                | [] -> None
                | _ -> Some(grounds |> List.map snd |> Set.unionMany)

            Map.add
                (Defend home)
                (Brief.defending
                    view
                    answered
                    (Brief.lentRangers view holders)
                    ground
                    stances
                    defendAvoid
                    verdict)
                briefs

    {
        Reach = reach
        Safe = safeGround
        Shelter = shelter
        Briefs = briefs
        Unclaimed =
            {
                BriefOf = Brief.unclaimedFor view atlas
            }
        Engaged = engaged
        Arrivals = arrivals
        Ramparts = dangers |> Map.map (fun _ danger -> danger.Ramparts)
        Seen = view.World.Rooms |> Map.keys |> Set.ofSeq
        Bodies = ColonyView.bodies view
        Horizon = view.Tuning.ReachHorizon
        Homes = Facts.homesOfOurs view
    }

/// This tick's Threats as a colony holding nothing would see them: no
/// Assault ground, and an outmatched room's safe ground only for a ranger
/// standing in it.
let threatsOf (view: ColonyView) atlas : Threats =
    threatsHeldBy view atlas HeldTaskFacts.empty

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
/// (`ColonyView.Assaults`, `assaultPooled`): a rally and launch over the
/// casts holding it, kept whole on the walk in.
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
        let casts = Formation.casts view

        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let share =
            ColonyView.bodies view
            |> List.map (fun creep ->
                creep.Name, float creep.Hits.Hits / float (max 1 creep.Hits.HitsMax))
            |> Map.ofList

        let besideIn = Formation.besideIn atlas

        let groundOf (facts: AssaultFacts) =
            let room = facts.Assault.RoomName
            let slots = facts.Assault.Squad |> List.choose squadRoleOfRow
            let members = Formation.holding held (Assault room) casts
            let file = Formation.frontFirst members

            let rally =
                Formation.musterAtHome
                    atlas
                    (Threats.reachIn threats)
                    (Atlas.siegeRoute atlas home room)

            let filled =
                SquadRole.all
                |> List.forall (fun role ->
                    SquadRole.slots role (List.map snd members) >= SquadRole.slots role slots)

            let inside = Formation.entered view room file

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
                Formation.launched
                    view
                    room
                    rally
                    file
                    (filled && fit && not facts.SafeMode && not facts.Taken)
                    (Formation.linked view file || away)

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

            let leader = Formation.leader view members

            // Strung out under fire: every cast within two of the leader, or
            // the leader holds its tile and the rest close on it.
            let strung = Formation.strung view atlas home room front file leader

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
                            |> Seq.filter (fun step -> Formation.edgeGap (RoomPos.pos step) > 0)
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

        // No squad is cast while safe mode runs in its room, which a squad
        // bought then waits out, nor once its targets are down (#496).
        let moded =
            view.Assaults
            |> List.filter (fun facts -> facts.SafeMode || facts.Taken)
            |> List.map (fun facts -> facts.Assault.RoomName)
            |> Set.ofList

        { threats with
            Briefs =
                assaults
                |> List.map (fun facts -> facts, groundOf facts)
                |> List.filter (fun (facts, (ground, working)) ->
                    working && assaultPooled view atlas ground.Launched facts)
                |> List.fold
                    (fun briefs (facts, (formation, _)) ->
                        let room = facts.Assault.RoomName

                        let casting _ =
                            not (Set.isEmpty formation.Rally) && not (Set.contains room moded)

                        Map.add
                            (Assault room)
                            (Brief.formed
                                view
                                atlas
                                Duty.Assault
                                OnTheTier
                                (lazy Answer.Win)
                                casting
                                formation)
                            briefs)
                    threats.Briefs
        }

/// Engaging together (#535): a raided room's Guard won by the exchange (its
/// verdict `Win`) none of whose holders wins alone (`Facts.bodiesWin`) or
/// kites the raid down alone (`Facts.kitedDown`), and that the holders and
/// the bodies its rows still cast win together, holds them all on the rally
/// ground out of reach (`Formation.rallyOn`, on the chain from home) until
/// they launch as a squad does (`Formation.launched`): every holder on it or
/// with the others, and the holders together winning. Never once one stands
/// in the room or at its crossing: the launch latches there without state,
/// and a holder in reach is never pulled back. Never where even the bodies
/// that can be assembled lose: nothing coming wins it, so nobody waits for
/// ever. A holder that wins alone engages at once, and the rest with it. An
/// outpost's guards, a chase's lent residents and pursuers alike; a
/// resident's and a harassment ranger's room keeps its own ground.
let private withMusters
    (view: ColonyView)
    atlas
    (held: HeldTaskFacts)
    (threats: Threats)
    : Threats =
    match view.Hostiles with
    | [] -> threats
    | _ ->
        let home = SpatialInfo.homeName view.Spatial

        let bodies =
            ColonyView.bodies view
            |> List.map (fun creep -> creep.Name, creep)
            |> Map.ofList

        // The bodies the room's rows still cast for it beyond its holders,
        // each at the row's blocks: what a muster may wait for.
        let castable (brief: Brief) (holders: CreepInfo list) =
            brief.Asks
            |> List.collect (fun ask ->
                let block =
                    match ask.Row with
                    | CastRow.Ranger -> Some rangerPattern.Block
                    | CastRow.Guard -> Some guardPattern.Block
                    | _ -> None

                match block with
                | Some block ->
                    let blocks = ask.Blocks()
                    let parts = partCountIn block
                    let coming = ask.Wanted threats - List.length holders

                    List.replicate
                        (max 0 coming)
                        ((fun part -> blocks * parts part),
                         blocks * Engine.partHits * List.length block)
                | None -> [])

        let stancesOf (room: string) (brief: Brief) (holders: CreepInfo list) =
            // Launched, stateless: a holder in the room or at its crossing is
            // engaged and is never pulled out of reach, nor the rest held.
            let engagedHere =
                holders
                |> List.exists (fun creep ->
                    SpatialInfo.creepPlacementOf view.Spatial creep.Name
                    |> Option.exists (fun tile ->
                        tile.Room = room || Formation.atCrossingInto room tile))

            // What the muster waits for, priced against the bodies that can
            // be assembled: the holders and those still to be cast. Where
            // even they lose, no body is coming that wins it, and nobody
            // waits: the holders engage, and the room's own verdict stands
            // it down when it loses.
            let assembled =
                lazy
                    (exchangeWon
                        view
                        room
                        ((holders |> List.map (fun creep -> partCount creep.Body, creep.Hits.Hits))
                         @ castable brief holders))

            match holders with
            | [] -> Map.empty
            | _ when engagedHere -> Map.empty
            | _ when holders |> List.exists (fun creep -> bodiesWin view room [ creep ]) ->
                Map.empty
            | _ when not assembled.Value -> Map.empty
            | _ ->
                let rally =
                    Formation.rallyOn
                        view
                        atlas
                        (Threats.reachIn threats)
                        (Atlas.route atlas home room)

                // The melee holders lead, then the ranged.
                let members =
                    holders
                    |> List.sortBy (fun creep -> isRangerParts creep.Body, creep.Name)
                    |> List.map _.Name

                if
                    Set.isEmpty rally
                    || Formation.launched
                        view
                        room
                        rally
                        members
                        (bodiesWin view room holders)
                        // On the walk it launched on while the holders stand
                        // as one file (#491's rule for the squads): strung out
                        // between the rally ground and the crossing, they are
                        // not pulled back.
                        (Formation.linked view members)
                then
                    Map.empty
                else
                    holders |> List.map (fun creep -> creep.Name, rally) |> Map.ofList

        // Only the rooms a raider stands in, and only a Guard held there: the
        // Briefs map is rebuilt for none of the rest.
        let mustered =
            view.Hostiles
            |> List.filter (HostileInfo.raider view.Hostiles)
            |> List.map (fun hostile -> hostile.Pos.Room)
            |> List.distinct
            |> List.choose (fun room ->
                let holders =
                    HeldTaskFacts.holdersOf held (taskId (Guard room))
                    |> List.choose (fun name -> Map.tryFind name bodies)

                match holders, Map.tryFind (Guard room) threats.Briefs with
                | [], _
                | _, None -> None
                | _, Some brief ->
                    match brief.Duty with
                    | Duty.Guard BriefKind.Resident
                    | Duty.Guard BriefKind.Harass -> None
                    | Duty.Guard _ when brief.Verdict.Value = Answer.Win ->
                        match stancesOf room brief holders with
                        | stances when Map.isEmpty stances -> None
                        | stances -> Some(Guard room, { brief with Stances = stances })
                    | _ -> None)

        match mustered with
        | [] -> threats
        | _ ->
            { threats with
                Briefs =
                    (threats.Briefs, mustered)
                    ||> List.fold (fun briefs (task, brief) -> Map.add task brief briefs)
            }

/// The tick's Threats off what the colony holds (`heldTaskFacts`): an
/// outmatched room's safe ground for every ranger holding its Guard, wherever
/// it stands, each Assault's ground, and the holders a Guard musters
/// (`withMusters`).
let threatsOfHeld (view: ColonyView) atlas (held: HeldTaskFacts) : Threats =
    threatsHeldBy view atlas held
    |> withAssaults view atlas held
    |> withMusters view atlas held
