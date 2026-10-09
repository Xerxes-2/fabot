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
    /// A [[contested room]] (#656): one our economy and a rival's meet in —
    /// our outpost a raid or a rival's economy body works, a rival's remote
    /// we harass, a threatened outpost the colony is blind in (#366) — one
    /// fight whatever it is over this tick (`Phase`), priced by the room's
    /// worth (`RoomWorth`); every Guard no other kind claims.
    | Contested
    | Core
    | ExitHold
    | Resident
    /// An outpost gone dark on a raid a ranger kited down there
    /// (`StandDown.ChasedOutposts`, #510): no raid in sight to price, so a
    /// kite holder walks in after it. A raid in sight is the contested
    /// room's, pursued (`Contention.Pursued`).
    | Chase

/// What a contested room is fought over this tick (#656), off its facts.
[<RequireQualifiedAccess>]
type Phase =
    /// A raid whose Guard pools stands there (`Facts.guardedRooms`, or a body
    /// of ours holds it beside a hostile): the raid first, on its ring.
    | Raid
    /// No raid whose Guard pools, and an economy body we may strike works it
    /// (`Facts.trespasser`), and no held exit keeps the room over it (its
    /// holders not back in time, `Facts.preyBeforeReturn`): a kill, a rung
    /// up, a quiet holder called to it.
    | Prey
    /// Neither: a blind outpost's look (#366), or a rival's remote with
    /// nobody working it.
    | Quiet

/// A contested room's Guard (#656): what it is fought over, whether it is
/// ours to keep (`Facts.contestOurs`), and its price, read on first use.
[<NoEquality; NoComparison>]
type Contention =
    {
        Phase: Phase
        Ours: bool
        /// A raid in sight rangers kite or are called to in a room no row's
        /// Guard otherwise pools (`Facts.chased`, #510): pooled whatever
        /// guards it, its holders kiting on the way in, its asks read where
        /// no guarded room's are.
        Pursued: bool
        Value: Lazy<RoomWorth>
    }

/// Which combat Task a `Brief` describes: a room's Guard, of its flavour, or
/// a Task of its own — the home's Defend, an Assault, an operation's room
/// (`Operate`, #663 S7c), fought as a contested room that is not ours.
[<RequireQualifiedAccess>]
type Duty =
    | Guard of BriefKind
    | Defend
    | Assault
    | Operation of opId: string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BriefKind =
    /// The order a room's flavour is read in, written once: a resident room,
    /// a chase (#510), a contested room (#656), a held exit (#450), a level-0
    /// core (#487): a creep that can step away is struck before a core that
    /// stands, which takes the room back once the creep is gone (#558), and
    /// an economy body working the room before the exit a raid left by where
    /// the exit's holders kill it and are back before the raid that left by
    /// it is (`Facts.preyBeforeReturn`), else the hold first. The
    /// order decides only among claims that pool a Task this tick (#542): the
    /// first of those is the room's kind, and a claim that pools nothing
    /// shuts no kind out. A kind claims only a room still open to it
    /// (`opens`), so a held exit and a core lay no ground under a pooling
    /// kind ahead of them (`threatsHeldBy`); and a Guard's ground is the
    /// first laid in its room from its kind down (`Brief.Ground`).
    let order =
        [
            BriefKind.Resident
            BriefKind.Chase
            BriefKind.Contested
            BriefKind.ExitHold
            BriefKind.Core
        ]

    /// The kinds whose ground is the room's `Ring`: a contested room's under
    /// its raid.
    let laysRing (kind: BriefKind) =
        match kind with
        | BriefKind.Contested
        | BriefKind.ExitHold
        | BriefKind.Core -> true
        | _ -> false

    /// Whether a room a pooling kind ahead claims is still open to `kind`:
    /// only a ring kind under a chase, and a contested room only for its raid
    /// (`threatsHeldBy`). Two halves. Load-bearing: a chase in sight is
    /// always a raid's room, so the raid still claims it as `Contested`, and
    /// that claim shuts the held exit and the core out of it. Quirk, kept
    /// (the Brief's S0): out of sight no raid claims it, so a held exit or a
    /// core lays its ring there, and the chase, with no kite ground, takes
    /// that melee guard's ring.
    let opens (ahead: BriefKind) (kind: BriefKind) =
        ahead = BriefKind.Chase && laysRing kind

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
        | CastRow.Fighter BodyTemplate.Melee -> isGuardRowCut name parts
        | CastRow.Fighter BodyTemplate.Ranged -> isRangerRowCut name parts
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

/// How many bodies one job seats this tick, and the rows that may sit them
/// (#550, #558, #546); no rows, any row its Brief admits.
type Seating = { Count: int; Rows: CastRow list }

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
        /// A contested room's (#656): its phase, whose economy it is and its
        /// price. None for every other Task.
        Contention: Contention option
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
        /// Its jobs (#663 M3), the one its holders do first: the jobs laid in
        /// the room each kind reads, from its own kind down
        /// `BriefKind.order`, each once. A resident and a chase hold their
        /// kite `Strike` (#451) first, so a melee guard's kind is never
        /// handed it; a resident its `Resident` next; a contested room by its
        /// phase (#656): under its raid its `Ring`, its kite first where a
        /// ranger answers it, else its `Camp` on a dancer or its prey's
        /// `Strike` (#516); a held exit and a core the room's `Ring` and
        /// `HoldExit`. A Guard's seats are its first job's, and its prey's
        /// ranged `Strike` beside it where a ranger answers a trespasser
        /// (#550), or a core's ranged `Ring` (#558); the rest is ground it
        /// falls back on and seats nobody. A Defend's, one a row at the line
        /// (#546); none for an Assault, whose squad is its `Formation`'s.
        Jobs: Job list
        /// Its holder's ground: its first job's tiles, every `Ring` and
        /// `HoldExit` together where it is one; with none, the room's
        /// declared source ring (#366). None where none is. A Defend's, the
        /// perimeter ground (`defendGroundOf`); a squad's is per role, on its
        /// `Formation`.
        Ground: Set<RoomPos> option
        /// With no ground and no source placed, whether the walk in is to the
        /// room's ground off the border: a chased room shut on its raid (#510),
        /// and the [[sweep]]'s room while nothing in it is in sight (#658).
        WalksIn: bool
        Avoid: Avoid
        Rung: Rung
        /// What it asks the fighting rows for: one ask a Guard's room and a
        /// Defend, one a role a squad; none for a chase (#510), which no row
        /// casts for.
        Asks: RowAsk list
        /// How many bodies it admits, the tick's Threats given: every job's
        /// seats summed (`Job.Seats`), each row a job names capped at the
        /// seats of the jobs it may sit (`Brief.capacityOfJobs`).
        Capacity: Threats -> Capacity
        /// The room's verdict (`Facts.answer`), priced once a tick on first
        /// read; `Win` in a room no raider stands in, and for an Assault,
        /// whose answer is its safe-mode window.
        Verdict: Lazy<Answer>
        /// An Assault's squad ground, role by role; None for every
        /// single-body kind.
        Formation: Formation option
        /// A Guard's holders held off `Ground` this tick, each on its own:
        /// its `Muster` job's crossing while they wait for the rest of their
        /// group (`withMusters`, #535, #658). The home's Defend's: each body
        /// of the rows it asks on its own row's ground (#546). Empty for every
        /// other Task.
        Stances: Map<string, Set<RoomPos>>
        /// A Guard's holders seated on its ground (`Brief.seatsAmong`, #650):
        /// each holder's own ground less every other holder's seat, where
        /// that costs it no tick. Its own
        /// is a ranged holder of a level-0 core's Guard the core's range-3
        /// ring, out of the swing of a melee body breaking the core (#558);
        /// a melee holder of a chase the ring beside the raid (#571); a
        /// holder of a contested room's prey the tiles of the ground within
        /// its reach of the body its kill score leads, where it reaches one
        /// (#621); every
        /// other `Ground`. Read after `Stances`, priced on first read. Empty
        /// for every other Task.
        Seats: Lazy<Map<string, Set<RoomPos>>>
    }

/// One job of a fight this tick (#663 M3): its kind, the tiles its bodies
/// stand on — fixed off the quarry's trajectory where it has one (a camp on
/// the tiles a dancer steps in on, a hold on the exit it left by), never its
/// tile this tick —, how many bodies it seats, what it earns and on whom.
/// Derived each tick, never stored; equal to nothing, as the Brief.
and [<NoEquality; NoComparison>] Job =
    {
        Kind: JobKind
        Tiles: Set<RoomPos>
        /// A kite `Strike`'s approach band (#595): the tiles one to three off
        /// its head that a kiter with no safe walk onto it closes on — off
        /// the exits and the hostiles' tiles, a melee arriving later than the
        /// tick after, with escape room at least half the ground's widest.
        /// Empty for every other job.
        Approach: Lazy<Set<RoomPos>>
        /// How many bodies it seats and of which rows, the tick's Threats
        /// given: none for a job its Brief only falls back on.
        Seats: Threats -> Seating
        /// What a tick of its bodies' effort earns, in energy (#663 M3), read
        /// on first use.
        Value: Lazy<float>
        /// The odds its value is priced at, where it is priced at any (a
        /// camp's, `Facts.campPrice`), read with it: what the operation
        /// ledger records once the value is read (#662).
        Odds: Lazy<float option>
        /// The hostiles it is about, by id: a kill's head or its prey, a
        /// camp's dancer, a ring's raid, a muster's escort.
        Quarry: string list
        /// Whether its holders are seated on its head's band by the
        /// exchange (`kiteSeating`, #615): a kite `Strike`'s; false for every
        /// other job.
        ByExchange: bool
    }

/// What one Brief asks a row for: the row, how many bodies, and the blocks
/// each must carry to win the room alone. The guard and ranger rows sum the
/// Guards' over the guarded rooms (`OutpostFacts.Guarded`), so a held exit's
/// ask is read only where its room is guarded too, and a contested room's
/// prey elsewhere only priced (`Prey`, #652, #656); the defender and squad
/// rows every other Brief's.
and [<NoEquality; NoComparison>] RowAsk =
    {
        Row: CastRow
        /// How many bodies, the tick's Threats given.
        Wanted: Threats -> int
        Blocks: unit -> int
        /// A contested room's kill: each trespasser the row answers
        /// (`trespassSplit`) by id, with its kill value (`RoomWorth.values`),
        /// which prices its cast wherever no economy of ours is guarded there
        /// (#652, #656). None for every other ask.
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

/// The fighter row's ask for one room's fight (#654): the bodies, blocks and
/// template of its cheapest cast among `templates` (`Facts.fighterCast`),
/// read off the raid and never our own answer to it (#272), the raid last
/// seen through the blind ticks since (`ColonyView.RecalledRaiders`, #569);
/// a room with no raid seen or recalled casts one melee block, enough to go
/// and look (#366). A melee body in a room holding a level-0 core is the
/// biggest the bank buys (#487): one block's 90 a tick spends most of a
/// body's life on the core's 100,000 hits. Priced on first read where the
/// template is given. None wanted where the cast does not pay (`pays`).
let internal fighterAsk
    (view: ColonyView)
    (room: string)
    (templates: BodyTemplate list)
    (prey: (string * float) list option)
    (pays: FighterCast -> bool)
    : RowAsk =
    let cast = lazy (Facts.fighterCast view room templates Engine.guardCap)

    let template =
        match templates with
        | [ one ] -> one
        | _ -> cast.Value.Template

    let cored () =
        template = BodyTemplate.Melee
        && Facts.expansionCores view |> List.exists (fun core -> core.RoomName = room)

    {
        Row = CastRow.Fighter template
        Wanted = fun _ -> if pays cast.Value then cast.Value.Bodies else 0
        Blocks =
            fun () ->
                if cored () then
                    max cast.Value.Blocks (guardBlocksReach view)
                else
                    cast.Value.Blocks
        Prey = prey
    }

/// The ranger blocks one ranger room or rampart row wants (#411): the
/// smallest one ranger the bank buys that wins its raid alone
/// (`Facts.fighterCast`, one body), never below the room's floor, and the
/// largest body where none wins.
let internal rangedBlocks (floor: int) (view: ColonyView) (room: string) : int =
    (Facts.fighterCast view room [ BodyTemplate.Ranged ] 1).Blocks
    |> max floor
    |> min rangerBlocksMost

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Job =
    /// These jobs with their seats summing to `most` at most, every row
    /// together, the first job's first (an operation's `OpLimits.MaxBodies`,
    /// #663 S7c).
    let capped (most: int) (jobs: Job list) : Job list =
        jobs
        |> List.mapi (fun index job ->
            { job with
                Seats =
                    fun threats ->
                        let before =
                            jobs
                            |> List.take index
                            |> List.sumBy (fun earlier -> (earlier.Seats threats).Count)

                        let seating = job.Seats threats

                        { seating with
                            Count = min seating.Count (max 0 (most - before))
                        }
            })

    /// A job laid in a room on these tiles, about these hostiles, at this
    /// price, seating nobody until its Brief seats it.
    let laid
        (kind: JobKind)
        (tiles: Set<RoomPos>)
        (quarry: string list)
        (value: Lazy<float>)
        : Job =
        {
            Kind = kind
            Tiles = tiles
            Approach = lazy Set.empty
            Seats = fun _ -> { Count = 0; Rows = [] }
            Value = value
            Odds = lazy None
            Quarry = quarry
            ByExchange = false
        }

    /// Whether a job is ground beside a raid or the exit it left by (its
    /// intercept included): the kinds a ring Brief stands on together.
    let isRing (job: Job) =
        job.Kind = JobKind.Ring
        || job.Kind = JobKind.HoldExit
        || job.Kind = JobKind.Intercept

    /// These jobs with each one only once, the first kept: a kind further
    /// down `BriefKind.order` reads ground a kind ahead read too.
    let internal once (jobs: Job list) : Job list =
        (jobs, [])
        ||> List.foldBack (fun job kept ->
            job
            :: (kept |> List.filter (fun other -> not (obj.ReferenceEquals(other, job)))))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Brief =
    /// How far a quiet ranger walks for a kill (#510), in travel cost (two
    /// units a tick): the reaction window of a lend, inside which no raid
    /// reaches the home it left (`Facts.homeHoldReleases`).
    let private killWalk = 2 * Reaction.lend

    /// Its ground off its jobs (#663 M3): the first job's tiles, every
    /// `Ring` and `HoldExit` heading the list together; None with no job on
    /// any tile.
    let groundOfJobs (jobs: Job list) : Set<RoomPos> option =
        match jobs |> List.filter (fun job -> not job.Tiles.IsEmpty) with
        | [] -> None
        | head :: _ as laid when Job.isRing head ->
            Some(laid |> List.takeWhile Job.isRing |> List.map _.Tiles |> Set.unionMany)
        | head :: _ -> Some head.Tiles

    /// The room's ring under a Brief (#571, #558): every `Ring` about a
    /// hostile — a raid, a core — and every `HoldExit` or `Intercept` among
    /// its jobs, together; None where it has none. The declared sources'
    /// post a Guard with nothing laid stands on (#366) is about none, and no
    /// ring.
    let ringOf (brief: Brief) : Set<RoomPos> option =
        match
            brief.Jobs
            |> List.filter (fun job ->
                Job.isRing job && (job.Kind <> JobKind.Ring || not job.Quarry.IsEmpty))
        with
        | [] -> None
        | rings -> Some(rings |> List.map _.Tiles |> Set.unionMany)

    /// A kite's approach band (#595) under a Brief: its kite `Strike`'s;
    /// empty with none.
    let approachOf (brief: Brief) : Lazy<Set<RoomPos>> =
        brief.Jobs
        |> List.tryFind (fun job -> job.Kind = JobKind.Strike)
        |> Option.map _.Approach
        |> Option.defaultValue (lazy Set.empty)

    /// Its capacity off its jobs (#663 M3): every job's seats summed, and
    /// where a job names the rows that sit it, each row named capped at the
    /// seats of the jobs naming it (`CapScope.Row`, which refuses a body of
    /// no capped row).
    let capacityOfJobs (jobs: Job list) (threats: Threats) : Capacity =
        let seatings = jobs |> List.map (fun job -> job.Seats threats)

        (Capacity.fighters (seatings |> List.sumBy _.Count),
         seatings |> List.collect _.Rows |> List.distinct)
        ||> List.fold (fun capacity row ->
            let seats =
                seatings
                |> List.filter (fun seating -> List.contains row seating.Rows)
                |> List.sumBy _.Count

            Capacity.capping (CapScope.Row row) seats capacity)

    /// Whether a Brief is a kill (#510, #516): one body, a rung up, a quiet
    /// holder called to it; a blind chase cast for by no row, a contested
    /// room's raid pursued, a contested room's prey only where its cast pays
    /// (#652, #656).
    let isKill (brief: Brief) : bool =
        brief.Duty = Duty.Guard BriefKind.Chase
        || brief.Contention |> Option.exists (fun c -> c.Phase = Phase.Prey || c.Pursued)

    /// Whether a Brief is a contested room's raid pursued
    /// (`Contention.Pursued`): a kill, pooled and its asks read whatever
    /// guards it.
    let pursued (brief: Brief) : bool =
        brief.Contention |> Option.exists _.Pursued

    /// Whether a Brief is a contested room's prey (#656): the kill its
    /// kills alone price where no economy of ours is guarded.
    let isPrey (brief: Brief) : bool =
        brief.Contention |> Option.exists (fun c -> c.Phase = Phase.Prey)

    /// The flavour a room's fight is read as: a Guard's own; an operation's
    /// room (#663 S7c) a contested room's, which it is fought as; None for a
    /// Task of its own.
    let flavourOf (brief: Brief) : BriefKind option =
        match brief.Duty with
        | Duty.Guard kind -> Some kind
        | Duty.Operation _ -> Some BriefKind.Contested
        | Duty.Defend
        | Duty.Assault -> None

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
            CastRow.Fighter BodyTemplate.Melee,
            (if asked && rangerHolds then caught else trespassers)
            CastRow.Fighter BodyTemplate.Ranged, (if asked then rest else [])
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

    /// A resident room's Guard is the ranger row's (#411); a contested
    /// room's the melee guard's, which holds the post where it cannot catch
    /// the raid (#531), but one a ranger answers (`ranged`: an outpost whose
    /// raid a ranger answers, #535, a harassment room, #432) the ranger
    /// row's alone: the guard brings nothing to bear there; and its prey's
    /// (#656) is a kill's. A kill admits only a body that strikes every
    /// target it counts (`Facts.strikesEvery`), the rule its verdict prices
    /// the rangers by. Whether the body fits the fight is no kind's: the
    /// Matcher asks it of every Guard (`Facts.fits`, #615 U3).
    let private admitsOf
        (kind: BriefKind)
        (phase: Phase)
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
        | BriefKind.Resident -> isRangerRowBody creep
        // Out of sight, the kite it was shut on (#510): a fighting row's body
        // that outranges a swing, as no fight there is priced.
        | BriefKind.Chase ->
            (isRangerRowBody creep || isGuardRowBody creep)
            && Fighter.outranges (Fighter.ofCreep creep)
            && strikes (HostileInfo.raider view.Hostiles)
        // A contested room's prey (#516, #656): the guard row's body that
        // strikes every trespasser its row answers (`trespassSplit`, #550),
        // or a ranger that strikes them all, whichever colony cast it and
        // whoever's room it is: a mother's guard hunts in her child's outpost
        // too (#616). Defence first is the rank's: a raid's Guard outranks it
        // (`claimed`, #528).
        | BriefKind.Contested when phase = Phase.Prey ->
            if isRangerRowBody creep then
                strikes (Facts.trespasser view (Set.singleton room))
            else
                isGuardRowBody creep
                && answered.Value
                   |> List.forall (fun (row, trespassers) ->
                       row <> CastRow.Fighter BodyTemplate.Melee
                       || trespassers |> List.forall (Facts.canStrike view (partCount creep.Body)))
        | BriefKind.Contested
        | BriefKind.ExitHold when ranged -> isRangerRowBody creep
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
        | BriefKind.Contested
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
            match Map.tryFind (Facts.fightTask view room) threats.Briefs with
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

    /// What each kind asks the fighter row for. A resident ranged bodies,
    /// never below `Tuning.RangerResidentBlocks`, the resident a raid meets
    /// first; a chase nothing but its own row's cast; every other kind its
    /// fight's cheapest cast (`fighterAsk`, #654): a held exit's and a core's
    /// of melee bodies; a contested room's (#656) of the template its row
    /// answers with under its raid or nobody working it, of either for its
    /// prey, whose kills price the cast where no economy of ours is guarded
    /// (#652). A room that is not ours to keep — a rival's remote we harass —
    /// asks only a cast that ends its fight, and only while a fight there
    /// moves its economic delta (`RoomWorth.delta`) or holds prey: there is
    /// no post of ours to hold (#531), no raid there is ours to look for
    /// (#366), and the room's value is what a fight denies the rival.
    ///
    /// A contested room's raid is answered by the bodies whose fight with it
    /// converges (#673, `Facts.castConverging`): a holder of the row that
    /// ages out before its kill lands, or trades for ever against the raid's
    /// heal, is no answer alone, so where its holders do not converge the ask
    /// is every holder and the fewest fresh bodies that converge beside them,
    /// as the Caster counts every living body of the row against it; a
    /// pursued raid that is the rangers' by it (`standing`: they win it and
    /// live through, `Facts.rangersWin`, and its own row casts nothing for
    /// it, #571, #655) asks no body for itself (#510). Under its raid, in a
    /// room of ours, its prey is priced beside it (#663): as many more bodies
    /// as its kills, at their kill values, pay for (`Facts.castBodiesPaid`),
    /// one a trespasser at most: an armed body standing there takes nothing
    /// off an economy body's value.
    let private askOf
        (kind: BriefKind)
        (contention: Contention option)
        (ranged: bool)
        (standing: Lazy<bool>)
        (holders: CreepInfo list)
        (preyed: bool)
        (answered: Lazy<(CastRow * HostileInfo list) list>)
        (room: string)
        (view: ColonyView)
        : RowAsk list =
        // Each trespasser its rows answer that is the enemy's economy, at its
        // kill value (`RoomWorth.values`, read for a cast).
        let preyOf (contention: Contention) =
            let values = RoomWorth.values Quarry.Reading.Cast contention.Value.Value

            answered.Value
            |> List.collect snd
            |> List.distinctBy _.Id
            |> List.filter (Facts.enemyEconomy view.Hostiles)
            |> List.map (fun hostile ->
                hostile.Id,
                Map.tryFind hostile.Id values
                |> Option.defaultWith (fun () -> Facts.killValue view hostile))

        match kind, contention with
        | BriefKind.Resident, _ ->
            [
                {
                    Row = CastRow.Fighter BodyTemplate.Ranged
                    Wanted = residentsWanted room view
                    Blocks = fun () -> rangedBlocks view.Tuning.RangerResidentBlocks view room
                    Prey = None
                }
            ]
        // A contested room's prey (#654): one ask, the cheapest set that
        // kills the most of its trespassers, of whichever template; its prey
        // every one of them that is the enemy's economy, at its kill value
        // (`RoomWorth.values`, read for a cast). A trespasser only bouncing
        // on the exit is met by one melee block.
        | BriefKind.Contested, Some contention when contention.Phase = Phase.Prey ->
            [ fighterAsk view room bodyTemplates (Some(preyOf contention)) (fun _ -> true) ]
        // Under its raid, or nobody working it: the cheapest cast of its
        // row's template, a ranger's where a ranger answers it (#535), else a
        // swing holding the post (`admitsOf`).
        | BriefKind.Contested, contention ->
            let template = if ranged then BodyTemplate.Ranged else BodyTemplate.Melee

            // The room's delta, or its prey (each one taking, `Takes`): the
            // escort's own kill buys no cast for a room not ours.
            let pays (cast: FighterCast) =
                match contention with
                | Some contention when not contention.Ours ->
                    let worth = contention.Value.Value

                    cast.Ends
                    && (RoomWorth.delta Quarry.Reading.Cast worth > 0.0
                        || not (Map.isEmpty worth.Takes))
                | _ -> true

            let raid = fighterAsk view room [ template ] None pays

            // The cast that converges beside the holders of its row
            // (`Facts.castConverging`).
            let converging =
                lazy
                    (Facts.castConverging
                        view
                        room
                        (Facts.fighterCast view room [ template ] Engine.guardCap)
                        (holders
                         |> List.filter (fun creep -> CastRow.cut raid.Row creep.Name creep.Body)))

            // Under its raid, the bodies of the cast's row its prey's kills
            // pay for (`Facts.castBodiesPaid`), landed after the body's spawn
            // and its walk, one a trespasser at most: in a room of ours, and
            // in an operation's room, where the raid and its prey are one
            // price (#663 S7c). Any other room not ours prices its prey into
            // whether its cast pays at all (`pays`, `Takes`).
            let preyPaid =
                lazy
                    (match contention with
                     | Some contention when
                         preyed
                         && (contention.Ours || Option.isSome (Facts.operationIn view room))
                         && contention.Phase = Phase.Raid
                         ->
                         match preyOf contention with
                         | [] -> 0
                         | prey ->
                             let blocks = raid.Blocks()

                             let landing =
                                 Facts.castSpawnTicks template blocks
                                 + CastBid.walk
                                     view.Tuning
                                     (RoomName.hopsBetween (SpatialInfo.homeName view.Spatial) room)

                             Facts.castBodiesPaid view raid.Row blocks room landing prey
                             |> min (List.length prey)
                             |> min Engine.guardCap
                     | _ -> 0)

            let standingWin () =
                contention |> Option.exists _.Pursued && standing.Value

            [
                { raid with
                    Wanted =
                        fun threats ->
                            match raid.Wanted threats with
                            | 0 -> 0
                            | _ when standingWin () -> preyPaid.Value
                            | _ -> converging.Value + preyPaid.Value
                }
            ]
        // A held exit held by a ranger is the ranger's (`claimed`).
        | BriefKind.ExitHold, _ when ranged ->
            [ fighterAsk view room [ BodyTemplate.Ranged ] None (fun _ -> true) ]
        | BriefKind.Core, _
        | BriefKind.ExitHold, _ ->
            [ fighterAsk view room [ BodyTemplate.Melee ] None (fun _ -> true) ]
        // A blind chase prices no fight: nothing is cast for it, and the
        // kite it was shut on walks in after the raid.
        | BriefKind.Chase, _ -> []

    /// A contested room's group (#663 M3): the bodies of this colony holding
    /// its fight, where each stands, and where an operation works the room,
    /// every colony's body of its group (`OperationWork.Group`): the bodies
    /// on its jobs.
    let private groupIn (view: ColonyView) (room: string) (holding: Set<string>) =
        let own =
            ColonyView.bodies view
            |> List.filter (fun creep -> holding.Contains creep.Name)
            |> List.choose (fun creep ->
                SpatialInfo.creepPlacementOf view.Spatial creep.Name
                |> Option.map (fun at -> creep, at))

        let operating =
            Facts.operationIn view room |> Option.map _.Group |> Option.defaultValue []

        own @ operating |> List.distinctBy (fun (creep, _) -> creep.Name)

    /// A Guard's jobs seated (#663 M3): its first job seats the bodies its
    /// kind's count admits, a row's ask's count read here a second time so
    /// the number the cascade hires against and the number the Matcher
    /// counts holders against are one number; the jobs after it seat
    /// nobody. Its capacity is their sum (`capacityOfJobs`).
    let private seated
        (kind: BriefKind)
        (pursued: bool)
        (phase: Phase)
        (room: string)
        (verdict: Lazy<Answer>)
        (rangers: Lazy<int option>)
        (holding: Set<string>)
        (answered: Lazy<(CastRow * HostileInfo list) list>)
        (asks: RowAsk list)
        (view: ColonyView)
        (jobs: Job list)
        : Job list =
        let wanted (threats: Threats) =
            asks
            |> List.tryHead
            |> Option.map (fun a -> a.Wanted threats)
            |> Option.defaultValue 1

        let any count = { Count = count; Rows = [] }

        let seat (seats: Threats -> Seating) (job: Job) = { job with Seats = seats }

        // A contested room's job seats its whole group, every colony's, and
        // a free fighter more while concentrating pays (`Facts.seatsPaid`,
        // #658): one group on one job, past the seats its own count gives
        // (live t1,038,786: a free ranger idled beside W19S26's two seats).
        let grouped = lazy (Facts.seatsPaid view room (groupIn view room holding))

        let melee, ranged =
            CastRow.Fighter BodyTemplate.Melee, CastRow.Fighter BodyTemplate.Ranged

        match jobs with
        | [] -> []
        | head :: rest ->
            match kind with
            // One over per garrison body in a resident room (#414, #419,
            // #447): its rangers are resident, and each relief — cast at its
            // incumbent's lead, two of them together when the garrison was
            // cast together — must take the Task and walk three crossings
            // while the incumbent still holds the ring. A Guard has no arrival
            // price for a handover window to be read against, and the row's
            // count, not this cap, is what buys bodies.
            | BriefKind.Resident ->
                seat (fun threats -> any (wanted threats + view.Tuning.RangerResidents)) head
                :: rest
            // A blind chase prices no fight: the one kite that walks in.
            | BriefKind.Chase -> seat (fun _ -> any 1) head :: rest
            // One body kills a contested room's prey, which strikes nothing
            // back (#516): a seat a trespasser each template answers
            // (`trespassSplit`, #550), at most `Engine.guardCap` a template, a
            // melee body's at least one where a trespasser only bounces on
            // the exit, and never fewer than the cast asks of its own (#654).
            // Where a ranged body answers one, the melee bodies sit the first
            // job, capped at their own count, and a ranged one sits either,
            // as its shot reaches what a swing catches too: seats are who may
            // stand, the cast what is bought.
            //
            // An operation's prey (#663 S7c) seats what its price pays for
            // (`JobCount.Priced`): its group's paid seats (`Facts.seatsPaid`),
            // never fewer than its cast buys, any row its Brief admits — not
            // the trespass split, which seats a body a trespasser whatever
            // the price.
            | BriefKind.Contested when
                phase = Phase.Prey && Option.isSome (Facts.operationIn view room)
                ->
                let cast (threats: Threats) =
                    asks |> List.sumBy (fun a -> a.Wanted threats)

                seat (fun threats -> any (max grouped.Value (cast threats))) head :: rest
            | BriefKind.Contested when phase = Phase.Prey ->
                let bouncing = answered.Value |> List.forall (snd >> List.isEmpty)

                let split (threats: Threats) =
                    let cast row =
                        asks
                        |> List.filter (fun a -> a.Row = row)
                        |> List.sumBy (fun a -> a.Wanted threats)

                    let count row =
                        let seats =
                            answered.Value
                            |> List.filter (fun (answers, _) -> answers = row)
                            |> List.sumBy (snd >> List.length)
                            |> min Engine.guardCap

                        let floor = if bouncing && row = melee then 1 else 0

                        max seats floor

                    let guards = max (count melee) (cast melee)

                    guards, max (grouped.Value - guards) (max (count ranged) (cast ranged - guards))

                let guards threats =
                    match split threats with
                    | guards, 0 -> any guards
                    | guards, _ ->
                        {
                            Count = guards
                            Rows = [ melee; ranged ]
                        }

                let rangers threats =
                    match split threats with
                    | _, 0 -> any 0
                    | _, rangers -> { Count = rangers; Rows = [ ranged ] }

                seat guards head :: seat rangers head :: rest
            // The guard row's count, and a seat beside it for each ranger it
            // admits standing in the room (#558), so a ranger never takes a
            // guard's.
            | BriefKind.Core ->
                let standing =
                    ColonyView.bodies view
                    |> List.filter (fun creep ->
                        isRangerRowBody creep
                        && SpatialInfo.creepPlacementOf view.Spatial creep.Name
                           |> Option.exists (fun at -> at.Room = room))
                    |> List.length

                if standing = 0 then
                    seat (wanted >> any) head :: rest
                else
                    seat
                        (fun threats ->
                            {
                                Count = wanted threats
                                Rows = [ melee ]
                            })
                        head
                    :: seat (fun _ -> { Count = standing; Rows = [ ranged ] }) head
                    :: rest
            // The cast's count (#654), or the fighters holding the fight or
            // standing in its room that do not win it alone and a seat beside
            // them, at most `Engine.guardCap`: the cast is what is bought, not
            // who may stand; a body the fight needs beside another is no body
            // to evict (#272), and none takes the seat of the body cast to win
            // it. A contested room's raid, harassed or ours, alike (#656):
            // rangers that win it only together are seated together (#609).
            | BriefKind.Contested
            | BriefKind.ExitHold ->
                // Bodies of the cast's template: the ones its kind admits.
                let cut =
                    match asks with
                    | ask :: _ -> CastRow.cut ask.Row
                    | [] -> fun _ _ -> false

                let held =
                    lazy
                        (ColonyView.bodies view
                         |> List.filter (fun creep ->
                             cut creep.Name creep.Body
                             && (holding.Contains creep.Name
                                 || SpatialInfo.creepPlacementOf view.Spatial creep.Name
                                    |> Option.exists (fun at -> at.Room = room))))

                let losers =
                    lazy
                        (held.Value
                         |> List.filter (fun creep -> not (Facts.bodiesWin view room [ creep ]))
                         |> List.length)

                // A held exit's raid stands across it, where no exchange in
                // the room reads it: its holders, the bodies that held the
                // raid's Guard, keep their seats.
                let keeping () =
                    if kind = BriefKind.ExitHold then
                        held.Value
                        |> List.filter (fun creep -> holding.Contains creep.Name)
                        |> List.length
                    else
                        0

                // A pursued raid (#510): a kite seats as many rangers as kill
                // what cannot close on them before it ages out, and every one
                // at the fight (`Facts.kiteSeats`, #596); one the rangers
                // there and beside it win together takes as many as win it.
                let pursuing () =
                    if not pursued then
                        0
                    else
                        match verdict.Value with
                        | Answer.Kite -> Facts.kiteSeats view room holding
                        | Answer.Win -> rangers.Value |> Option.defaultValue 0
                        | Answer.Lose -> 0

                let count threats =
                    max
                        (max grouped.Value (pursuing ()))
                        (max
                            (wanted threats)
                            (min Engine.guardCap (max (keeping ()) (losers.Value + 1))))

                seat (count >> any) head :: rest

    /// A Guard's own kind: the first of its claims, `Contested` where none
    /// claims the room.
    let internal kindOf (claims: BriefKind list) =
        List.tryHead claims |> Option.defaultValue BriefKind.Contested

    /// One room's Brief, off the kinds that claimed it down `BriefKind.order`
    /// and the jobs laid there its kind reads (`laid`, given whether a
    /// ranger answers it), seated (`seated`). A kill (#510, a contested
    /// room's prey, #516) is stepped a rung up — its kill is the room's whole
    /// income back, and a quiet holder is let go for it — and calls that
    /// holder off within its walk. A raid's Guard two: a contested room's
    /// raid, or a garrison's or a chase's where a Threat stands (`raided`);
    /// and a room of ours nobody works, the remembered raid's (#366).
    /// Defence first, so a body the raid wants never goes to a kill, and only
    /// one the raid leaves free does (#528). The room's verdict, the rangers
    /// that win it (`Facts.rangersWin`), whether this colony's row casts for
    /// it where they do (`casts`) and whether a ranger answers it
    /// (`Facts.rangerAnswers`, or the room is a harassment room) come priced
    /// lazily, once a tick; a contested room's `phase`, whether its raid is
    /// pursued (`Contention.Pursued`) and whether prey works it under its
    /// raid (`preyed`) beside them. Its answers read the view and Atlas
    /// given.
    let internal claimed
        (view: ColonyView)
        (atlas: Atlas.Atlas)
        (room: string)
        (raided: bool)
        (verdict: Lazy<Answer>)
        (rangers: Lazy<int option>)
        (casts: Lazy<bool>)
        (ranger: Lazy<bool>)
        (pursued: bool)
        (preyed: bool)
        (answered: Lazy<(CastRow * HostileInfo list) list>)
        (holding: Set<string>)
        (claims: BriefKind list)
        (phase: Phase)
        (laid: bool -> Job list)
        : Brief =
        let kind = kindOf claims

        let contention =
            if kind = BriefKind.Contested then
                let value = lazy (Facts.roomWorth view room)

                Some
                    {
                        Phase = phase
                        Ours = Facts.contestOurs view room
                        Pursued = pursued
                        Value = value
                    }
            else
                None

        let pursued = Option.isSome contention && pursued

        let kill =
            kind = BriefKind.Chase
            || kind = BriefKind.Contested && (phase = Phase.Prey || pursued)

        // A held exit is the row's that held the raid's Guard: a ranger
        // holding it keeps it while the raid bounces across the exit (live
        // W17S26 t1,038,737-1,039,037: Wraith7582 stepped in and out of
        // W18S26, and the rangers holding the Guard were let go
        // "inapplicable" each time it stepped out and taken back each time it
        // stepped in).
        let heldByRanger =
            lazy
                (ColonyView.bodies view
                 |> List.exists (fun creep -> holding.Contains creep.Name && isRangerRowBody creep))

        let ranged =
            kind = BriefKind.Contested && ranger.Value
            || kind = BriefKind.ExitHold && heldByRanger.Value

        let holders =
            ColonyView.bodies view |> List.filter (fun creep -> holding.Contains creep.Name)

        let ask =
            askOf
                kind
                contention
                ranged
                (lazy (Option.isSome rangers.Value && not casts.Value))
                holders
                preyed
                answered
                room
                view

        // With no job laid on any tile, one on the ring of the room's
        // declared sources, which `Outpost.Sources` places without vision
        // (#366): off the builder's Atlas, so a body of another colony the
        // world hands the Brief walks to it too (a transit room carries no
        // source). A post to hold or a room to look into, priced at what a
        // fight there keeps.
        // An operation's room (#663 S7c): its Task the operation's, its asks
        // and its seats at most the bodies the operation seats
        // (`OpLimits.MaxBodies`), all its rows together, and its rank at most
        // a kill's, under a home's raid.
        let operation =
            match kind with
            | BriefKind.Contested -> Facts.operationIn view room
            | _ -> None

        let ask =
            match operation with
            | Some op ->
                ask
                |> List.mapi (fun index a ->
                    { a with
                        Wanted =
                            fun threats ->
                                let before =
                                    ask
                                    |> List.take index
                                    |> List.sumBy (fun b -> b.Wanted threats)

                                a.Wanted threats |> min (max 0 (op.MaxBodies - before))
                    })
            | None -> ask

        let jobs =
            match laid ranged |> List.filter (fun job -> not job.Tiles.IsEmpty) with
            | [] ->
                [
                    Job.laid
                        JobKind.Ring
                        (Atlas.sourceRingIn atlas room)
                        []
                        (lazy (RoomWorth.delta Quarry.Reading.Value (Facts.roomWorth view room)))
                ]
            | jobs -> jobs
            |> seated kind pursued phase room verdict rangers holding answered ask view
            |> fun jobs ->
                match operation with
                | Some op -> Job.capped op.MaxBodies jobs
                | None -> jobs

        {
            Duty =
                match operation with
                | Some op -> Duty.Operation op.Id
                | None -> Duty.Guard kind
            BuiltBy = SpatialInfo.homeName view.Spatial
            Claims = claims
            Contention = contention
            Targets = guardTargets view atlas holding
            Admits = admitsOf kind phase room ranged answered view atlas
            Ends = Facts.endsIn view room
            Recall = if kill then Some { Walk = killWalk } else None
            Jobs = jobs
            Ground = groundOfJobs jobs
            WalksIn =
                kind = BriefKind.Chase
                || kind = BriefKind.Contested
                   && phase = Phase.Quiet
                   && Set.contains room (operationRooms view)
            Avoid =
                match kind with
                | BriefKind.Chase -> Avoid.Kite
                | _ when pursued -> Avoid.Kite
                | _ when kill -> Avoid.Reach
                | _ -> Avoid.Runner
            Rung =
                let rung =
                    match kind with
                    // A contested room's raid, harassed or ours (#609, #656):
                    // a ranger idling on a quiet Guard is sent to it; and a
                    // room nobody works pooled for the raid remembered there
                    // (#366). A harassment room nobody works is worked target
                    // or none, on the tier.
                    | BriefKind.Contested when
                        phase = Phase.Raid
                        || phase = Phase.Quiet && not (Set.contains room (operationRooms view))
                        ->
                        TwoRungsUp
                    | BriefKind.Resident
                    | BriefKind.Chase when raided -> TwoRungsUp
                    | _ when kill -> OneRungUp
                    | _ -> OnTheTier

                if Option.isSome operation then min rung OneRungUp else rung
            Asks = ask
            Capacity = capacityOfJobs jobs
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
    /// body fights where it goes, at the standoff, a contested room's prey's
    /// holder among them.
    let stanceOf (view: ColonyView) (room: string) (brief: Brief) (creep: CreepInfo) : Stance =
        match flavourOf brief with
        | Some kind when
            BriefKind.laysRing kind
            && not (isKill brief)
            && Fighter.glues (Fighter.ofCreep creep)
            ->
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

    /// A room no source claims or lays ground in: a quiet contested room on
    /// no ground, the one a remembered raid pools in a room the colony is
    /// blind in (#366), a melee guard's. No raider stands in it, so its
    /// verdict is `Win`.
    let unclaimed (view: ColonyView) (atlas: Atlas.Atlas) (room: string) : Brief =
        claimed
            view
            atlas
            room
            false
            (lazy Answer.Win)
            (lazy None)
            (lazy false)
            (lazy (Set.contains room (operationRooms view)))
            false
            false
            (lazy [])
            Set.empty
            []
            Phase.Quiet
            (fun _ -> [])

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
            [
                CastRow.Defender, met
                CastRow.Fighter BodyTemplate.Ranged, atLine |> List.filter neverStruck
            ]
        else
            [ CastRow.Defender, atLine; CastRow.Fighter BodyTemplate.Ranged, [] ]

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
                            if row = CastRow.Fighter BodyTemplate.Ranged then
                                max 0 (defendersWanted raid view - lent)
                            else
                                defendersWanted raid view
                    Blocks =
                        fun () ->
                            if row = CastRow.Fighter BodyTemplate.Ranged then
                                rangedBlocks 1 view (SpatialInfo.homeName view.Spatial)
                            else
                                view.Tuning.DefenderBlocks
                    Prey = None
                })

        // One job a row at the line (#546), on the perimeter ground, each
        // seating its ask's bodies of its own row alone.
        let home = SpatialInfo.homeName view.Spatial

        let jobs =
            List.zip answered asks
            |> List.map (fun ((row, raid), ask) ->
                { Job.laid
                      JobKind.Ring
                      (Option.defaultValue Set.empty ground)
                      (raid |> List.map _.Id)
                      (lazy (RoomWorth.delta Quarry.Reading.Value (Facts.roomWorth view home))) with
                    Seats =
                        fun threats ->
                            {
                                Count = ask.Wanted threats
                                Rows = [ row ]
                            }
                })

        {
            Duty = Duty.Defend
            BuiltBy = SpatialInfo.homeName view.Spatial
            Claims = []
            Contention = None
            Targets =
                {
                    Shoots = fun _ _ hostile -> Set.contains hostile.Id line
                    Order = fun _ _ _ shots -> shots |> List.sortBy (fun h -> h.Hits, h.Id)
                    Band = fun _ _ -> 0
                }
            Admits =
                fun creep ->
                    answered |> List.exists (fun (row, _) -> CastRow.cut row creep.Name creep.Body)
            Ends = Facts.endsIn view home
            Recall = None
            Jobs = jobs
            Ground = ground
            WalksIn = false
            Avoid = Avoid.Line avoid
            Rung = OnTheTier
            Asks = asks
            Capacity = capacityOfJobs jobs
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
            Contention = None
            Targets = guardTargets view atlas (formation.Roles |> Map.keys |> Set.ofSeq)
            Admits = fun creep -> Map.containsKey creep.Name formation.Roles
            Ends = fun _ -> None
            Recall = None
            Jobs = []
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
        | Guard room
        | Operate(_, room) -> Some(unclaimed view atlas room)
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

    /// Whether a body standing at `a` is with one at `b`: within two of it,
    /// across a border too (`rangeAcross`).
    let internal withEachOther (a: RoomPos) (b: RoomPos) =
        rangeAcross a b |> Option.exists (fun r -> r <= squadSpread)

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
            |> List.filter (fun tile ->
                Dance.toEdge tile > crossingGap && not (Set.contains tile reach))
            |> Set.ofList
            |> RoomPos.setAt from

    /// A Guard's muster ground (#535, #658), one for its whole group, every
    /// colony's (`group`, where each stands): beside the crossing out of the
    /// room beside it the group stands nearest, none a rival's or a keeper's,
    /// so bodies of two colonies go in together and not one alone on each
    /// side; where that crossing lays none, on the chain from home
    /// (`rallyHop`). Empty where neither does.
    let internal musterGround
        (view: ColonyView)
        atlas
        (reachIn: string -> Set<Pos>)
        (room: string)
        (group: RoomPos list)
        =
        let onChain (chain: string list option) =
            chain
            |> Option.bind (rallyHop view.Spatial.RivalRooms)
            |> Option.map (fun (from, into) -> besideCrossing atlas reachIn from into)
            |> Option.defaultValue Set.empty

        let shared =
            RoomName.adjacent room
            |> List.filter (fun beside ->
                not (Keepers.isKeeperRoom beside)
                && not (Set.contains beside view.Spatial.RivalRooms)
                && not (List.isEmpty (Atlas.seams atlas beside room)))
            |> List.sortBy (fun beside ->
                group
                |> List.sumBy (fun at ->
                    RoomName.tileRange at (RoomPos.at beside { X = 25; Y = 25 })
                    |> Option.defaultValue Engine.roomSide),
                beside)
            |> List.tryHead

        match shared |> Option.map (fun beside -> onChain (Some [ beside; room ])) with
        | Some rally when not (Set.isEmpty rally) -> rally
        | _ -> onChain (Atlas.route atlas (SpatialInfo.homeName view.Spatial) room)

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
                   Dance.toEdge (RoomPos.pos step) > 0
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

    /// One room's fight's `Brief` this tick: its Guard's, or the operation's
    /// working it (`Operate`, #663 S7c).
    let briefOf (threats: Threats) (room: string) : Brief =
        match Map.tryFind (Guard room) threats.Briefs with
        | Some brief -> brief
        | None ->
            threats.Briefs
            |> Map.tryPick (fun task brief ->
                match task with
                | Operate(_, worked) when worked = room -> Some brief
                | _ -> None)
            |> Option.orElseWith (fun () -> threats.Unclaimed.BriefOf(Guard room))
            |> Option.defaultWith (fun () ->
                invalidOp $"No view derived these Threats to brief {room}'s Guard")

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

    /// One room's range-1 ring (`Brief.ringOf`), already joined to that
    /// room; empty for a room no Threat stands in.
    let ringIn (threats: Threats) (room: string) : Set<RoomPos> =
        Brief.ringOf (briefOf threats room) |> Option.defaultValue Set.empty

    /// The garrison's ground in one resident room (its `Resident` job), or
    /// None for any other room (#414, #447).
    let residentRingIn (threats: Threats) (room: string) : Set<RoomPos> option =
        (briefOf threats room).Jobs
        |> List.tryFind (fun job -> job.Kind = JobKind.Resident)
        |> Option.map _.Tiles

    /// One room's Guard's jobs (#663 M3); a room no source claims has its
    /// unclaimed Brief's.
    let jobsIn (threats: Threats) (room: string) : Job list = (briefOf threats room).Jobs

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
        | Guard room
        | Operate(_, room) ->
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

/// A hostile's fire as the kite ground prices it (#615): its tile, and the
/// RANGED_ATTACK and ATTACK power it fights with, boosts counted
/// (`HostileInfo.effectivePower`), read off its body and never its owner,
/// and whether it can step this tick (a MOVE acting and its fatigue paid,
/// `Arrival.waitOf`): a step brings its weapons a tile nearer before they
/// land.
type KiteFire =
    {
        At: Pos
        Guns: int
        Swings: int
        Stepping: bool
    }

/// Whether a hostile can step this tick, its fatigue as the room last read
/// it (`WorldRoom.HostileFatigue`).
let private stepsNowIn (view: ColonyView) (inRoom: HostileInfo list) (hostile: HostileInfo) =
    let gait = HostileInfo.gait inRoom hostile

    let fatigue =
        Map.tryFind hostile.Pos.Room view.World.Rooms
        |> Option.bind (fun seen -> Map.tryFind hostile.Id seen.HostileFatigue)
        |> Option.defaultValue 0

    gait.Moves > 0 && Arrival.waitOf fatigue gait = 0

/// The fire of every armed hostile in a room (`KiteFire`).
let private firesIn (view: ColonyView) (inRoom: HostileInfo list) : KiteFire list =
    inRoom
    |> List.choose (fun hostile ->
        let power = HostileInfo.effectivePower inRoom hostile
        let guns, swings = power RangedAttack, power Attack

        if guns = 0 && swings = 0 then
            None
        else
            Some
                {
                    At = RoomPos.pos hostile.Pos
                    Guns = guns
                    Swings = swings
                    Stepping = stepsNowIn view inRoom hostile
                })

/// What the hostiles land a tick on our bodies standing on `ours` (#615),
/// each at its best response, a tile nearer where it can step first: a
/// shooter the more of a shot on one body and its mass attack on every body
/// within three (`Engine.massAttackPowerAt`), a melee its swing on one
/// beside it, a rival tower its shot on the nearest (`Engine.towerAttackAt`).
let kiteIncoming (fires: KiteFire list) (towers: Pos list) (ours: Pos list) : int =
    match ours with
    | [] -> 0
    | _ ->
        let fired =
            fires
            |> List.sumBy (fun fire ->
                let step = if fire.Stepping then 1 else 0

                let near =
                    ours
                    |> List.map (fun tile -> max 1 (range fire.At tile - step))
                    |> List.filter (fun r -> r <= Engine.rangedRange)

                let shot =
                    if List.isEmpty near then
                        0
                    else
                        Engine.rangedAttackPower * fire.Guns

                let mass = near |> List.sumBy (fun r -> Engine.massAttackPowerAt r * fire.Guns)

                let swing =
                    if List.exists (fun r -> r <= Engine.meleeRange) near then
                        Engine.attackPower * fire.Swings
                    else
                        0

                max shot mass + swing)

        fired
        + (towers
           |> List.sumBy (fun tower ->
               ours |> List.map (range tower >> Engine.towerAttackAt) |> List.max))

/// One room's kite exchange (#615): what a fighter standing on a tile of the
/// kite ground and our bodies already `seated` there trade with the room's
/// fire, priced off the bodies alone. Over `kiteReach` ticks: what the body
/// lands on the head at the tile's range, its swing, its shot or its mass
/// attack on the targets within three where no body it may not strike stands
/// there, for the ticks the head stays in its reach were it to stand (a tick
/// a tile off for a head that steps), less the fire its standing there adds
/// to what the room lands on us (`kiteIncoming`). So an unarmed head, or a lone
/// shooter whose shot lands alike at one to three, is closed on to one where
/// it steps; a melee holds it off its swing; a shooter crowded by our bodies
/// is met where its mass attack gains least.
type KiteExchange =
    {
        Head: Pos
        HeadSteps: bool
        /// The heal the head and its friends land on it a tick, in the
        /// damage that undoes it (`Facts.healUndoingIn`): what of our blows
        /// on it does not stick.
        HealOn: int
        Fires: KiteFire list
        Towers: Pos list
        Targets: Pos list
        Bystanders: Pos list
    }

/// What `fighter` trades on `tile` beside `seated` (`KiteExchange`).
let kiteExchangeOn (exchange: KiteExchange) (fighter: Fighter) (seated: Pos list) (tile: Pos) =
    let r = range tile exchange.Head
    let guns = fighter.DamageAt Engine.rangedRange / Engine.rangedAttackPower

    let mass =
        if
            guns = 0
            || exchange.Bystanders
               |> List.exists (fun at -> range tile at <= Engine.rangedRange)
        then
            0
        else
            guns
            * (exchange.Targets
               |> List.sumBy (fun at -> Engine.massAttackPowerAt (range tile at)))

    let stays =
        if exchange.HeadSteps then
            max 0 (min kiteReach (fighter.Reach - r + 1))
        else
            kiteReach

    let added =
        kiteIncoming exchange.Fires exchange.Towers (tile :: seated)
        - kiteIncoming exchange.Fires exchange.Towers seated

    // Its blows at what sticks, net of the head's heal: a head that out-heals
    // it is worth no standing in its gun.
    max 0 (max (fighter.DamageAt r) mass - exchange.HealOn) * stays
    - kiteReach * added

/// Fighters seated on a kite band (#615), each with the tile it stands on
/// and the tiles it reaches by its arrival in ticks: one priced pass, the
/// soonest arrived first (then by name), each on the free tile it trades best
/// on beside every body seated before it (`kiteExchangeOn`); a tile taken
/// before it is no seat (two bodies never stand on one). Of a tie, the tile
/// it stands on (its walk is spent: no churn between equal tiles), else the
/// soonest reached: its tile, the one the next body is priced beside. Its
/// seats are the tied best less every tile another body takes, its own
/// tile among them. A fighter reaching no free tile is seated nowhere.
let kiteSeating
    (exchange: KiteExchange)
    (fighters: (string * Fighter * Pos option * (int * Pos) list) list)
    : Map<string, Pos * Set<Pos>> =
    let claims =
        fighters
        |> List.filter (fun (_, _, _, reached) -> not (List.isEmpty reached))
        |> List.sortBy (fun (name, _, _, reached) -> List.min reached |> fst, name)
        |> List.fold
            (fun (taken: Pos list, claims) (name, fighter, standing, reached) ->
                match reached |> List.filter (fun (_, tile) -> not (List.contains tile taken)) with
                | [] -> taken, claims
                | free ->
                    let priced =
                        free
                        |> List.map (fun (ticks, tile) ->
                            kiteExchangeOn exchange fighter taken tile, ticks, tile)

                    let best = priced |> List.map (fun (worth, _, _) -> worth) |> List.max
                    let tops = priced |> List.filter (fun (worth, _, _) -> worth = best)

                    let chosen =
                        match standing with
                        | Some here when tops |> List.exists (fun (_, _, tile) -> tile = here) ->
                            here
                        | _ ->
                            let _, _, tile =
                                tops |> List.minBy (fun (_, ticks, tile) -> ticks, tile)

                            tile

                    chosen :: taken,
                    (name, chosen, tops |> List.map (fun (_, _, tile) -> tile) |> Set.ofList)
                    :: claims)
            ([], [])
        |> snd

    let chosen = claims |> List.map (fun (_, tile, _) -> tile) |> Set.ofList

    claims
    |> List.map (fun (name, tile, tops) -> name, (tile, Set.add tile (Set.difference tops chosen)))
    |> Map.ofList

/// Of `tiles`, the ones a body's walk reaches (`reached`) priced best
/// (`price`): an unreached tile, however priced, is no seat and never
/// outprices one it can stand on. Empty where it reaches none.
let bestReached (reached: 'tile -> bool) (price: 'tile -> int) (tiles: Set<'tile>) : Set<'tile> =
    match
        tiles
        |> Set.filter reached
        |> Set.toList
        |> List.map (fun tile -> price tile, tile)
    with
    | [] -> Set.empty
    | priced ->
        let best = priced |> List.map fst |> List.max
        priced |> List.filter (fst >> (=) best) |> List.map snd |> Set.ofList

/// One room's exchange about `head` (`KiteExchange`), off this tick's view:
/// whether the head can step (a MOVE acting and its fatigue paid,
/// `Arrival.waitOf`), the room's fire and rival towers, our `targets` there,
/// and every body there a mass attack may not reach (one `mayStrike` refuses
/// in `engaged`, an ally's).
let private exchangeAbout
    (view: ColonyView)
    (engaged: Set<string>)
    (targets: HostileInfo list)
    (head: HostileInfo)
    : KiteExchange =
    let room = head.Pos.Room
    let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)
    let seen = Map.tryFind room view.World.Rooms

    {
        Head = RoomPos.pos head.Pos
        HeadSteps = stepsNowIn view inRoom head
        HealOn = Facts.healUndoingIn view room head
        Fires = firesIn view inRoom
        Towers =
            seen
            |> Option.map (fun seen -> seen.Towers |> List.map snd)
            |> Option.defaultValue []
        Targets = targets |> List.map (fun h -> RoomPos.pos h.Pos)
        Bystanders =
            (inRoom
             |> List.filter (fun h -> not (Facts.mayStrike view engaged h))
             |> List.map (fun h -> RoomPos.pos h.Pos))
            @ (view.Allies
               |> List.filter (fun h -> h.Pos.Room = room)
               |> List.map (fun h -> RoomPos.pos h.Pos))
    }

/// The kite ground (#510, movement S3): the tiles of one room within the
/// head's reach that a body standing there steps off before any map shorter
/// than its reach strikes them (#615 U1: a melee arrival later than the tick
/// after), off the exit
/// tiles, which carry a body out of the room. Of those, the ones with escape
/// room: the open tiles about each, within the horizon's ticks, that a melee
/// reaches no sooner than the tile itself (off swamp and the exits, read off
/// the maps and never flooded), at least half the widest's, so the retreat
/// never backs into a corridor, corner or swamp pocket a melee pins it in:
/// the band (`candidates`). The ground is the band's tiles a lone ranger
/// block trades best on (`worth`, the room's `KiteExchange`, #615): where an
/// unarmed head steps, one beside it, where it stood within a tile less of
/// it (#535). Which of them one body may stand on, its own legs and fatigue
/// counted, is its walk's (`Threats.kiteUnsafeFor`); where our bodies stand
/// together, each one's its seat's (`seatsIn`).
let private kiteGroundIn
    atlas
    (room: string)
    (maps: ArrivalMaps option)
    (horizon: int)
    (inRoom: HostileInfo list)
    (head: HostileInfo option)
    (worth: Pos -> int)
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
    | None -> Set.empty, lazy Set.empty, []
    | Some target ->
        let at = RoomPos.pos target.Pos

        let tiles = Atlas.walkableWithinIn atlas room kiteReach at |> List.filter isOpen

        let candidates, widest =
            match anyMelee, tiles with
            | false, _
            | _, [] -> tiles, 0
            | _ ->
                let roomOf = tiles |> List.map (fun tile -> tile, escapeRoom tile)
                let widest = roomOf |> List.map snd |> List.max

                roomOf |> List.filter (fun (_, space) -> 2 * space >= widest) |> List.map fst,
                widest

        let ground =
            match candidates |> List.map (fun tile -> worth tile, tile) with
            | [] -> Set.empty
            | priced ->
                let best = priced |> List.map fst |> List.max
                priced |> List.filter (fun (w, _) -> w = best) |> List.map snd |> Set.ofList

        let reach =
            if Set.isEmpty ground then
                kiteReach
            else
                ground |> Seq.map (range at) |> Seq.max

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

        RoomPos.setAt room ground, approach, candidates

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
            if row = CastRow.Fighter BodyTemplate.Ranged then
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
                (let holders =
                    HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room)) |> Set.ofList

                 let side =
                     ColonyView.bodies view
                     |> List.filter (fun creep -> holders.Contains creep.Name)

                 scoreAmong view atlas side side room))
        |> Map.ofList

    let sideScore room : HostileInfo -> float =
        match Map.tryFind room sideScores with
        | Some score -> score.Value
        | None -> fun _ -> 0.0


    let rangers = rangerRooms view

    // The rooms a hostile stands in whose fight a living body of ours holds
    // (`Threats.Engaged`, #535). Never a candidate an operation names and
    // does not work (#658): its fight is the room the operation works, under
    // the operation's Task, and a body still holding a candidate's Guard (one
    // held from before the operation's Task, #663 S7c) is let go to it, not
    // latched where it stood (live t1,038,786: a ranger engaged in W18S27
    // kept that room pooled and cast for beside the sweep's W19S26).
    let engaged =
        let worked = operationRooms view

        view.Hostiles
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> List.filter (fun room ->
            not (List.isEmpty (HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))))
            && (worked.Contains room || not (view.HarassCast.Contains room)))
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

    // Whether a ranger holds a room's Guard.
    let kept room =
        let holders =
            HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room)) |> Set.ofList

        ColonyView.bodies view
        |> List.exists (fun creep -> Set.contains creep.Name holders && isRangerRowBody creep)

    // The raids in sight a ranger pursues (#510, `chased`): each a contested
    // room's, pooled whatever guards it (`Contention.Pursued`), priced with
    // its prey beside it.
    let pursuedRooms =
        verdicts
        |> Map.keys
        |> Seq.filter (fun room -> chased view (verdictOf room) (kept room) room)
        |> Set.ofSeq

    // Whether a ranger answers a pursued raid: rangers win it and not its
    // own row, a ranger holds it, or it is kited (#655: a verdict casts the
    // body it counts); else its own row's swing does (#571).
    let pursuedByRanger room =
        Set.contains room pursuedRooms
        && (match verdictOf room with
            | Answer.Kite, _
            | _, false -> true
            | _, true -> kept room)

    // The rooms a ranger chases a raid in out of sight (#510): an outpost
    // shut on a raid a ranger kites down, while no raid is seen there. The
    // row is the gate: a look that finds the room empty ends the chase there
    // (`Observe.strikeGone`).
    let chasedRooms = view.ChasedOutposts |> Set.filter (raided view >> not)

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
    // pursued rooms a raid stands in, the raided rooms a ranger answers
    // (#535), the resident rooms a raid stands at (#552), and those a body
    // worth its kill stands in (`worthIn`).
    let contested =
        (view.Hostiles
         |> List.filter (fun hostile ->
             Set.contains hostile.Pos.Room rangers
             && (Map.containsKey hostile.Pos.Room ring
                 || claimsAFlag view.Hostiles resident hostile))
         |> List.map (fun hostile -> hostile.Pos.Room))
        @ (pursuedRooms |> Set.filter ring.ContainsKey |> Set.toList)
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

    // Each contested room's kite head and its exchange (#615): the head the
    // ground follows, what the Emitter shoots there (`Threats.fightRooms`): a
    // body left in a room whose fight we hold — a cripple shot disarmed, a
    // healer — is the head the ground follows (#648). Read by the room's kite
    // ground and by its seats (`seatsIn`).
    let kiteHeads =
        contested
        |> List.map (fun room ->
            room,
            lazy
                (let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)
                 let targets = inRoom |> List.filter (guardShoots view resident engaged)

                 targets
                 |> killHead view room (sideScore room)
                 |> Option.map (fun head -> head, exchangeAbout view engaged targets head)))
        |> Map.ofList

    // The body the kite ground is laid for: a ranger block (`kiteReach`).
    let kiteBody = Facts.blockFighter rangerPattern

    // Each contested room's kite ground, its approach band and the band it
    // is priced out of (`kiteGroundIn`), on first read.
    let kiteGrounds =
        kiteHeads
        |> Map.map (fun room headed ->
            lazy
                (let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)
                 let maps = Map.tryFind room arrivals |> Option.map fst

                 let worth =
                     match headed.Value with
                     | Some(_, exchange) -> kiteExchangeOn exchange kiteBody []
                     | None -> fun _ -> 0

                 kiteGroundIn
                     atlas
                     room
                     maps
                     view.Tuning.ReachHorizon
                     inRoom
                     (headed.Value |> Option.map fst)
                     worth))

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

            let headed = kiteHeads.[room].Value
            let head = headed |> Option.map fst

            let aroundHead () =
                let ground, approach, _ = kiteGrounds.[room].Value
                ground, approach, true

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
                        HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))
                        |> Set.ofList

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
            let bare ground () = ground (), lazy Set.empty, false

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
                let tiles, approach, byExchange = ground ()

                if Set.isEmpty tiles then
                    None
                else
                    // A kill of the head (#663 M3), at its side's kill score.
                    Some(
                        room,
                        { Job.laid
                              JobKind.Strike
                              tiles
                              (head |> Option.map _.Id |> Option.toList)
                              (lazy
                                  (head |> Option.map (sideScore room) |> Option.defaultValue 0.0)) with
                            Approach = approach
                            ByExchange = byExchange
                        }
                    )))
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
    //
    // One job a quarry (#663 S5): a trespasser dancing between two rooms of
    // ours, standing in one while its trajectory still holds it the other's
    // (`Quarry.placed`: crossed out of it and lingering by the border), is
    // kept in the other where a body of ours holds that room's Guard and
    // none holds its own: keyed there, the tiles beside the one it crossed
    // out from, so its hunter holds one Task across each crossing.
    //
    // Each room's ground, with the trespassers it is about.
    let trespassIn (open': string -> bool) =
        let huntedIn room =
            not (List.isEmpty (HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))))

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

        let around (hostile: HostileInfo) : (string * Pos list * string) list =
            let room = hostile.Pos.Room
            let at = RoomPos.pos hostile.Pos

            let heldElsewhere () =
                if huntedIn room then
                    None
                else
                    RoomName.adjacent room
                    |> List.filter (fun other -> trespassRooms view other && huntedIn other)
                    |> List.tryPick (fun other ->
                        Quarry.placed other hostile.Pos (Map.tryFind hostile.Id view.Bounces)
                        |> Option.map (fun tile -> other, RoomPos.pos tile))

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

                match heldElsewhere () with
                | Some(other, came) ->
                    [ other, Atlas.adjacentWalkableIn atlas other came, hostile.Id ]
                | None -> [ room, tiles, hostile.Id ]
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
                    | tiles -> Some(next, tiles, hostile.Id))
            else
                []

        view.Hostiles
        |> List.collect around
        |> List.filter (fun (room, _, _) -> open' room)
        |> List.groupBy (fun (room, _, _) -> room)
        |> List.choose (fun (room, laid) ->
            let standing =
                view.Hostiles
                |> List.filter (fun hostile -> hostile.Pos.Room = room)
                |> List.map (fun hostile -> RoomPos.pos hostile.Pos)
                |> Set.ofList

            let ground =
                laid
                |> List.collect (fun (_, tiles, _) -> tiles)
                |> List.filter (fun tile -> not (Seam.onRing tile || Set.contains tile standing))
                |> Set.ofList

            if Set.isEmpty ground then
                None
            else
                Some(
                    room,
                    (RoomPos.setAt room ground,
                     laid |> List.map (fun (_, _, id) -> id) |> List.distinct)
                ))

    // Whether a level-0 core in a room comes before its trespassers (#558):
    // where only the guard row answers them and its swing catches none of
    // them, a trespass kill would hold the guard where it strikes nothing, and
    // the core would stand for ever. The guard breaks the core, swinging at
    // a trespasser that comes in reach first; a body it catches, or one a
    // ranger answers, is still struck first.
    let coreFirst room =
        Facts.expansionCores view |> List.exists (fun core -> core.RoomName = room)
        && Brief.trespassSplit
            view
            (HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room)))
            room
           |> List.forall (fun (row, trespassers) ->
               match row with
               | CastRow.Fighter BodyTemplate.Melee ->
                   let guard = Facts.blockFighter guardPattern
                   trespassers |> List.forall (Facts.strikesAtStandoff view guard >> not)
               | _ -> List.isEmpty trespassers)

    // Whether a held exit keeps its room over its prey (#656): its holders
    // would not kill a trespasser and be back at the exit before the raid
    // that left by it is (`Facts.preyBeforeReturn`).
    let holdKeeps room =
        match Map.tryFind room view.ExitHolds with
        | Some hold ->
            let holders =
                HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))
                |> List.choose (fun name ->
                    ColonyView.bodies view |> List.tryFind (fun creep -> creep.Name = name))

            not (Facts.preyBeforeReturn view room hold holders)
        | None -> false

    // A contested room's prey's ground (#516, #656): about every trespasser
    // in a room of ours or a harassed one, a raid's beside it or none (#663),
    // never a blind chase's, whose chase claims it first, nor a cored room's
    // the core comes first in, nor a held exit's its holders would not be
    // back at in time.
    let prey =
        trespassIn (fun room ->
            not (Set.contains room chasedRooms)
            && not (coreFirst room)
            && not (holdKeeps room))
        |> Map.ofList

    // Whether a kind's claim on a room pools a Task this tick, as `planTasks`
    // pools it (#542): a kill always (a chase, a contested room's prey);
    // any kind where the room's Guard is pooled whatever claims it
    // (`Facts.guardedRooms`), or where a body of ours holds that Guard
    // beside a hostile (`engaged`, the held fight), or a ranger pursues its
    // raid (`pursuedRooms`); a held exit where a body holds its Guard.
    // `engaged` is a superset of the Planner's `fought` (which also asks
    // `holdsFight` — circular here, as it reads the Brief): where they
    // differ the kill pools a tick later, never stalls. The
    // guarded rooms are derived only where a claim ahead would otherwise
    // shut a later one out.
    let guarded =
        lazy
            (Facts.guardedRooms view atlas (Facts.declaredOutposts view) (Facts.fuelledErrands view)
             |> Set.ofList)

    let raidPools room =
        Set.contains room engaged
        || Set.contains room pursuedRooms
        || Set.contains room guarded.Value

    let pools kind room =
        kind = BriefKind.Chase
        || kind = BriefKind.Contested && Map.containsKey room prey
        || raidPools room
        || kind = BriefKind.ExitHold
           && Set.contains (taskId (Facts.fightTask view room)) held.All

    // A contested room's phase (#656): its raid where a ranger pursues it
    // (a cripple shot disarmed among it, #510), where that pools, or where
    // no prey works it; else its prey; else quiet.
    let phaseOf room =
        if Set.contains room pursuedRooms then
            Phase.Raid
        elif Map.containsKey room ring && (raidPools room || not (Map.containsKey room prey)) then
            Phase.Raid
        elif Map.containsKey room prey then
            Phase.Prey
        else
            Phase.Quiet

    // Each kind's claim, down `BriefKind.order`, on the rooms still open to
    // it: a resident and a chase claim their rooms outright; a contested
    // room its raid's rooms and its pursued ones outright, open under a
    // chase as a ring is, and the rooms its prey works or that we harass, not
    // under one; a held exit and a core only the rooms they lay ground in. A
    // claim that pools nothing shuts nobody out. Per room, its claimants in
    // that order; per kind, the ground it laid.
    let claimants, laid =
        let outright rooms open' =
            rooms |> Seq.filter open' |> Seq.map (fun room -> room, Set.empty) |> List.ofSeq

        let claimOf kind =
            match kind with
            | BriefKind.Resident -> outright resident
            | BriefKind.Chase -> outright chasedRooms
            | BriefKind.Contested ->
                fun open' ->
                    let unchased room =
                        open' room && not (Set.contains room chasedRooms)

                    Seq.concat
                        [
                            ring.Keys |> Seq.filter open'
                            pursuedRooms |> Seq.filter open'
                            prey.Keys |> Seq.filter unchased
                            operationRooms view |> Seq.filter unchased
                        ]
                    |> Seq.distinct
                    |> Seq.map (fun room -> room, Set.empty)
                    |> List.ofSeq
            | BriefKind.ExitHold -> holdsIn
            | BriefKind.Core -> coresIn

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

    // The jobs laid in each room (#663 M3), each once a room, its price read
    // on first use: what a fight there keeps a tick (`RoomWorth.delta`) for
    // a ring and a held exit.
    let keeps room =
        lazy (RoomWorth.delta Quarry.Reading.Value (Facts.roomWorth view room))

    let ringJobs =
        ring
        |> Map.map (fun room tiles ->
            let raid =
                view.Hostiles
                |> List.filter (fun h ->
                    h.Pos.Room = room && Option.isSome (HostileInfo.weaponRange view.Hostiles h))
                |> List.map _.Id

            Job.laid JobKind.Ring tiles raid (keeps room))

    // Each held exit's ground (#450) and each core's (#487), where open.
    // A held exit is about the bodies that left (#585). One seen lingering
    // across the hold's own border, its way back in is the hold's: an
    // intercept on the hold's ground within a ranged reach of the exit tile
    // it steps back in by — the run's tile its place along that border
    // meets — priced at the kill score the hold's holders land on it where
    // it stands (`scoreAmong`, read in its room). Else the ground beside the
    // whole run, at what the room keeps.
    let holdJobs =
        laidBy BriefKind.ExitHold
        |> List.map (fun (room, ground) ->
            let hold = Map.tryFind room view.ExitHolds

            let quarry =
                hold
                |> Option.map (fun hold -> hold.Leavers |> List.map _.Id)
                |> Option.defaultValue []

            let lingering =
                hold
                |> Option.bind (fun hold ->
                    ExitHold.across room hold
                    |> Option.bind (fun across ->
                        view.Hostiles
                        |> List.filter (fun h ->
                            List.contains h.Id quarry
                            && h.Pos.Room = across
                            && Dance.besideOf room h.Pos)
                        |> List.sortBy (fun h -> Dance.toEdge (RoomPos.pos h.Pos), h.Id)
                        |> List.tryHead)
                    |> Option.map (fun h -> hold, h))

            match lingering with
            | Some(hold, h) ->
                let along (tile: Pos) =
                    if hold.Exit.X = 0 || hold.Exit.X = Engine.roomSide - 1 then
                        abs (tile.Y - h.Pos.Y)
                    else
                        abs (tile.X - h.Pos.X)

                let back = hold.Run |> List.minBy (fun tile -> along tile, tile.Y, tile.X)

                let holders =
                    HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room)) |> Set.ofList

                let side =
                    ColonyView.bodies view
                    |> List.filter (fun creep -> holders.Contains creep.Name)

                match
                    ground
                    |> Set.filter (fun tile -> range (RoomPos.pos tile) back <= Engine.rangedRange)
                with
                | seats when not seats.IsEmpty ->
                    room,
                    Job.laid
                        JobKind.Intercept
                        seats
                        quarry
                        (lazy (scoreAmong view atlas side side h.Pos.Room h))
                | _ -> room, Job.laid JobKind.HoldExit ground quarry (keeps room)
            | None -> room, Job.laid JobKind.HoldExit ground quarry (keeps room))
        |> Map.ofList

    let coreJobs =
        laidBy BriefKind.Core
        |> List.groupBy fst
        |> List.map (fun (room, grounds) ->
            let cores =
                Facts.expansionCores view
                |> List.filter (fun core -> core.RoomName = room)
                |> List.map _.Id

            room,
            Job.laid JobKind.Ring (grounds |> List.map snd |> Set.unionMany) cores (keeps room))
        |> Map.ofList

    let residentJobs =
        residentRing
        |> Map.map (fun room tiles ->
            Job.laid JobKind.Resident tiles [] (lazy (Facts.roomWorth view room).Kept))

    // A contested room's prey where its claim was open (#656), what it is
    // fought over or beside its raid (#663): a kill of each trespasser there,
    // at the kill score of the one its side's score leads.
    let preyJobs =
        prey
        |> Map.filter (fun room _ ->
            match Map.tryFind room claimants with
            | Some claims ->
                List.contains BriefKind.Contested claims && phaseOf room <> Phase.Quiet
            | None -> false)
        |> Map.map (fun room (tiles, ids) ->
            Job.laid
                JobKind.Strike
                tiles
                ids
                (lazy
                    (view.Hostiles
                     |> List.filter (fun h -> List.contains h.Id ids)
                     |> List.fold (fun best h -> max best (sideScore room h)) 0.0)))

    // The camp on a border dancer in the [[sweep]]'s room (#658,
    // `Facts.sweepDancer`): tiles off the border ring and every armed body's
    // reach, within the group's reach of the tiles the dancer steps in on,
    // covering the most of them, the one it uses most first, then nearest
    // it; one a holder, those a holder already stands on first (the walk
    // spent is the camp's hysteresis). The dancer's tile now never enters the
    // choice: the tiles follow the trajectory it dances on. Priced at its
    // kill at the camp's odds (`Facts.campValue`).
    let campJob room =
        if not (Set.contains room (operationRooms view)) || phaseOf room = Phase.Raid then
            None
        else
            let holders =
                HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))
                |> List.choose (fun name ->
                    ColonyView.bodies view |> List.tryFind (fun creep -> creep.Name = name))

            // With no holder, the bodies the chooser casts for the room
            // (`Facts.fighterCast`, #663 M4): a swing where its camp pays
            // more than a shot's.
            let team =
                match holders with
                | [] ->
                    Facts.fighterCast view room bodyTemplates Engine.guardCap |> Facts.castFighters
                | holders -> holders |> List.map Fighter.ofCreep

            let reach = team |> List.map _.Reach |> List.fold max 0

            match Facts.sweepDancer view room team with
            | None -> None
            | Some(dancer, entries) ->
                let standing =
                    holders
                    |> List.choose (fun creep ->
                        SpatialInfo.creepPlacementOf view.Spatial creep.Name)
                    |> List.filter (fun at -> at.Room = room)
                    |> List.map RoomPos.pos
                    |> Set.ofList

                let used = entries |> List.tryHead |> Option.defaultValue dancer.Pos |> RoomPos.pos

                // The camp's ground, the one its price reads
                // (`Facts.campGround`): none where a step in has no tile
                // the team's reach covers it from.
                match
                    Facts.campGround view reach dancer
                    |> List.sortBy (fun (tile, share) ->
                        not (standing.Contains tile), -share, range tile used, tile.Y, tile.X)
                    |> List.map fst
                    |> List.truncate (max 1 (List.length holders))
                with
                | [] -> None
                | tiles ->
                    let price = lazy (Facts.campPrice view room team dancer)

                    Some(
                        { Job.laid
                              JobKind.Camp
                              (tiles |> Set.ofList |> RoomPos.setAt room)
                              [ dancer.Id ]
                              (lazy (fst price.Value)) with
                            Odds = lazy (snd price.Value)
                        }
                    )

    // The jobs one room's Guard reads (#663 M3), from its kind down
    // `BriefKind.order`, each kind's own in order, given whether a ranger
    // answers the room: a resident its kite, then its garrison ground; a
    // chase its kite; a contested room by its phase — under its raid its
    // kite where a ranger answers it, then its rings, then its prey's kill
    // (#663); else its camp on a dancer, else its prey's kill —; a held exit
    // and a core the rings: the raid's, each held exit's, each core's.
    let jobsIn room (kind: BriefKind) (phase: Phase) (ranged: bool) : Job list =
        let found (jobs: Map<string, Job>) = Map.tryFind room jobs |> Option.toList

        let rings = found ringJobs @ found holdJobs @ found coreJobs

        let own kind =
            match kind with
            | BriefKind.Resident -> found kite @ found residentJobs
            | BriefKind.Chase -> found kite
            | BriefKind.Contested ->
                match phase with
                | Phase.Raid when ranged -> found kite @ rings @ found preyJobs
                | Phase.Raid -> rings @ found preyJobs
                | Phase.Prey
                | Phase.Quiet ->
                    match campJob room with
                    | Some camp -> [ camp ]
                    | None when phase = Phase.Prey -> found preyJobs
                    | None -> []
            | BriefKind.ExitHold
            | BriefKind.Core -> rings

        BriefKind.order |> List.skipWhile ((<>) kind) |> List.collect own |> Job.once

    // Every room a source claimed or laid ground in; one no source claims is
    // the default kind's, `Contested`.
    let briefs =
        [
            Map.keys claimants
            Map.keys ring
            Map.keys holdJobs
            Map.keys coreJobs
            Map.keys residentRing
            Map.keys kite
        ]
        |> Seq.concat
        |> Set.ofSeq
        |> Seq.map (fun room ->
            let verdict, winners =
                match Map.tryFind room verdicts with
                | Some(verdict, winners) -> lazy (fst verdict.Value), winners
                | None -> lazy Answer.Win, lazy None

            Facts.fightTask view room,
            Brief.claimed
                view
                atlas
                room
                (Map.containsKey room ring)
                verdict
                winners
                // Its own row casts for it where rangers by it win it: a
                // room shut on a raid this colony's row wins (#571), or a
                // kite its biggest ranger is counted for (#655: a verdict
                // casts the body it counts).
                (lazy
                    (Map.tryFind room judged
                     |> Option.exists (fun verdict ->
                         verdict.Value.Casting.Contains home
                         && (Set.contains room view.RaidedOutposts
                             || verdict.Value.Answer = Answer.Kite))))
                // A harassment room is the ranger's, a hostile in it or none
                // (#432); so is a raid a ranger pursues.
                (lazy
                    (answeredByRanger room
                     || Set.contains room (operationRooms view)
                     || pursuedByRanger room))
                (Set.contains room pursuedRooms)
                (Map.containsKey room prey)
                (lazy
                    (Brief.trespassSplit
                        view
                        (HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room)))
                        room))
                (HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room)) |> Set.ofList)
                (claimsIn room)
                (phaseOf room)
                (jobsIn room (Brief.kindOf (claimsIn room)) (phaseOf room)))
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
                match brief.Duty, Brief.ringOf brief with
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
                HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))
                |> List.choose (fun name ->
                    ColonyView.bodies view |> List.tryFind (fun creep -> creep.Name = name))

            let scores = lazy (sideScoresIn view holders room)

            let seatGroundOf (creep: CreepInfo) =
                match groundOf creep with
                | Some(ground, false) when Brief.isPrey brief ->
                    let score = scoreOf atlas scores.Value creep
                    let reach = Fighter.reachOf creep
                    let arrival = arrivalOf creep

                    let targets =
                        view.Hostiles
                        |> List.filter (fun hostile ->
                            hostile.Pos.Room = room
                            && brief.Targets.Shoots resident engaged hostile)

                    // Its arrival within its reach of a hostile, as its kill
                    // score reads it (`scoreOf`): the soonest tile of that
                    // reach its own walk reaches.
                    let inReachBy (hostile: HostileInfo) =
                        Atlas.walkableWithinIn atlas room reach (RoomPos.pos hostile.Pos)
                        |> List.choose (fun tile -> arrival (RoomPos.at room tile))
                        |> function
                            | [] -> None
                            | ticks -> Some(List.min ticks)

                    // Whether the holders together kill it at all: a nought
                    // score of one they never kill is its heal's, and no
                    // reason to stand by it.
                    let killable =
                        lazy (Facts.killableIn view room (holders |> List.map Fighter.ofCreep))

                    // The kill worth most a tick of its effort; where none
                    // is priced at any for its arrival or its catch odds
                    // (each quarry walking off out of its reach), the one of
                    // those it reaches soonest within its reach, the score's
                    // own arrival term: it keeps pace on the one it is on,
                    // never the nearest tile of every quarry's ground,
                    // another's store ring among them (W17S26
                    // t1052656-676: a ranger four off Midnight walked for
                    // Onyx's tiles eleven off, every score nought).
                    let head =
                        match
                            targets
                            |> List.map (fun hostile -> score hostile, hostile)
                            |> List.filter (fun (worth, _) -> worth > 0.0)
                            |> List.sortBy (fun (worth, hostile) -> -worth, hostile.Id)
                        with
                        | (_, best) :: _ -> Some best
                        | [] ->
                            targets
                            |> List.filter killable.Value
                            |> List.choose (fun hostile ->
                                inReachBy hostile
                                |> Option.map (fun ticks -> (ticks, hostile.Id), hostile))
                            |> List.sortBy fst
                            |> List.tryHead
                            |> Option.map snd

                    // Of the ground within its reach of the head that its
                    // walk reaches, the tiles it trades best on there
                    // (`kiteExchangeOn`, the room's fire priced off the
                    // bodies): beside a quarry that steps and cannot hurt it,
                    // anywhere in its reach of one that stands, off a swing.
                    let near =
                        match head with
                        | Some target ->
                            let exchange = lazy (exchangeAbout view engaged targets target)
                            let fighter = Fighter.ofCreep creep

                            ground
                            |> Set.filter (fun tile ->
                                RoomPos.range tile target.Pos
                                |> Option.exists (fun r -> r <= reach))
                            |> bestReached (arrival >> Option.isSome) (fun tile ->
                                kiteExchangeOn exchange.Value fighter [] (RoomPos.pos tile))
                        | None -> Set.empty

                    if
                        near.Count < ground.Count && near |> Set.exists (arrival >> Option.isSome)
                    then
                        Some(near, true)
                    else
                        Some(ground, false)
                | seated -> seated

            let seated =
                holders
                |> List.choose (fun creep ->
                    seatGroundOf creep
                    |> Option.map (fun (ground, own) -> creep.Name, ground, own, arrivalOf creep))
                |> Brief.seatsAmong

            // On the room's kite ground (#615, its first job marked
            // `ByExchange`) every body holding it, a swing's or a shot's, is
            // seated in one priced pass on the tile of the ground's band
            // (every open tile within three that keeps its escape room) it
            // trades best on beside every body seated before it
            // (`kiteSeating`, its own damage by range): one more body
            // crowding a shooter pays for the mass attack it turns it to, a
            // body already on a tied-best tile keeps it.
            match brief.Jobs, Map.tryFind room kiteGrounds, Map.tryFind room kiteHeads with
            | job :: _, Some grounds, Some headed when job.ByExchange ->
                let _, _, band = grounds.Value

                match headed.Value with
                | Some(_, exchange) ->
                    holders
                    |> List.map (fun creep ->
                        let arrival = arrivalOf creep

                        creep.Name,
                        Fighter.ofCreep creep,
                        Atlas.creepTile atlas creep.Name
                        |> Option.filter (fun tile -> tile.Room = room)
                        |> Option.map RoomPos.pos,
                        band
                        |> List.choose (fun tile ->
                            arrival (RoomPos.at room tile)
                            |> Option.map (fun ticks -> ticks, tile)))
                    |> kiteSeating exchange
                    |> Map.fold
                        (fun seats name (_, tops) -> Map.add name (RoomPos.setAt room tops) seats)
                        seated
                | None -> seated
            | _ -> seated

        briefs
        |> Map.map (fun task brief ->
            match task, brief.Duty with
            | Guard room, Duty.Guard _
            | Operate(_, room), Duty.Operation _ ->
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
                            |> Seq.filter (fun step -> Dance.toEdge (RoomPos.pos step) > 0)
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

/// Engaging together (#535, #658, #663 M3): a Guard whose group's first
/// come would lose more alone than waiting for the rest costs
/// (`Facts.muster`, the one rule) holds its holders on the group's crossing
/// out of reach (`Formation.musterGround`), a `Muster` job ahead of its
/// others, until the rest land. An outpost's guards, a chase's lent
/// residents and pursuers, and the [[sweep]]'s group alike.
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

        // The bodies the room's rows still cast for it beyond the group on it
        // (`onIt`: its holders and the sweep's group), and only those the
        // facts say will land, so a cast that never begins never holds a
        // muster: each of the row in an oven, landing as the longest spawn
        // under way finishes and its walk from home (`CastBid.walk`); and of
        // the rest, one a spawn standing idle that the bank funds now, at
        // the row's blocks, landing after its spawn time and its walk.
        let castable (room: string) (brief: Brief) (onIt: int) : Facts.Mustering list =
            let walk = CastBid.walk view.Tuning (RoomName.hopsBetween home room)

            let underWay =
                view.Spawns
                |> List.filter _.IsSpawning
                |> List.map _.Remaining
                |> List.fold max 0

            let idle =
                view.Spawns |> List.filter (fun spawn -> not spawn.IsSpawning) |> List.length

            brief.Asks
            |> List.collect (fun ask ->
                match ask.Row with
                | CastRow.Fighter template ->
                    let oven =
                        view.Casting
                        |> List.filter (fun cast ->
                            CastRow.cut ask.Row cast.Name (partsOf cast.Body))
                        |> List.map (fun cast ->
                            let parts = partsOf cast.Body

                            {
                                Facts.Fighter =
                                    Fighter.ofCounts
                                        (fun part ->
                                            Map.tryFind part parts |> Option.defaultValue 0)
                                        (List.length cast.Body * Engine.partHits)
                                Facts.Lands = underWay + walk
                                Facts.Energy = float (bodyCost cast.Body)
                            })

                    let blocks = ask.Blocks()
                    let pattern = patternOfTemplate template
                    let cost = blocks * bodyCost pattern.Block

                    let funded = if cost <= 0 then 0 else view.Bank.Available / cost

                    let fresh =
                        ask.Wanted threats - onIt - List.length oven
                        |> min idle
                        |> min funded
                        |> max 0

                    oven
                    @ List.replicate
                        fresh
                        {
                            Facts.Fighter = Facts.blocksFighter pattern blocks
                            Facts.Lands = Facts.castSpawnTicks template blocks + walk
                            Facts.Energy = float cost
                        }
                | _ -> [])

        // A Guard's muster (#535, #658, `Facts.muster`): while no body of its
        // group stands in the room or at its crossing — the launch latches
        // there without state, and a body in reach is never pulled back —
        // the holders wait on the group's crossing (`Formation.musterGround`)
        // where the first come alone risks more than the waiting costs. A
        // body on that ground has landed, so has one standing as near the
        // room in a room beside it as the ground's farthest tile (another
        // colony's crossing, on its own chain), and so has one with a body
        // that has: the muster waits for the rest, never for bodies already
        // waiting. A resident's room keeps its
        // own ground, and a fight its verdict loses (`Lose`) is no fight to
        // gather for.
        let musterOf (room: string) (brief: Brief) =
            let holders =
                HeldTaskFacts.holdersOf held (taskId (Facts.fightTask view room))
                |> List.choose (fun name -> Map.tryFind name bodies)

            // A fight lost is no fight to gather for; a won one and a kited
            // one are, its vanguard priced alone either way (`Facts.muster`).
            let won =
                match brief.Duty with
                | Duty.Guard BriefKind.Resident -> false
                | Duty.Guard _
                | Duty.Operation _ -> brief.Verdict.Value <> Answer.Lose
                | _ -> false

            if List.isEmpty holders || not won then
                None
            else
                let group =
                    (holders
                     |> List.choose (fun creep ->
                         SpatialInfo.creepPlacementOf view.Spatial creep.Name
                         |> Option.map (fun at -> creep, at)))
                    @ (Facts.operationIn view room |> Option.map _.Group |> Option.defaultValue [])
                    |> List.distinctBy (fun (creep, _) -> creep.Name)

                let engaged =
                    group
                    |> List.exists (fun (_, at) ->
                        at.Room = room || Formation.atCrossingInto room at)

                let rally =
                    lazy
                        (Formation.musterGround
                            view
                            atlas
                            (Threats.reachIn threats)
                            room
                            (List.map snd group))

                if engaged || Set.isEmpty rally.Value then
                    None
                else
                    let centre = RoomPos.at room { X = 25; Y = 25 }

                    let depth =
                        rally.Value
                        |> Seq.map (fun tile ->
                            RoomName.tileRange tile centre
                            |> Option.defaultValue System.Int32.MaxValue)
                        |> Seq.fold max 0

                    let onGround (at: RoomPos) =
                        rally.Value.Contains at
                        || RoomName.neighbouring at.Room room
                           && RoomName.tileRange at centre
                              |> Option.exists (fun tiles -> tiles <= depth)

                    // And one with a body that has, as a squad's member is
                    // with its leader (live arena #609: a ranger a tile short
                    // of a crowded ground beside its partner on it).
                    let landed = group |> List.map snd |> List.filter onGround

                    let arrived (at: RoomPos) =
                        onGround at || landed |> List.exists (Formation.withEachOther at)

                    let standing =
                        group
                        |> List.map (fun (creep, at) ->
                            let fighter = Fighter.ofCreep creep

                            {
                                Facts.Fighter = fighter
                                Facts.Lands =
                                    if arrived at then 0 else Facts.landsIn at fighter room
                                Facts.Energy =
                                    creep.Body
                                    |> Map.toList
                                    |> List.collect (fun (part, n) -> List.replicate n part)
                                    |> bodyCost
                                    |> float
                            })

                    match
                        Facts.muster view room (standing @ castable room brief (List.length group))
                    with
                    | None -> None
                    | Some value ->
                        let job =
                            Job.laid
                                JobKind.Muster
                                rally.Value
                                (brief.Jobs |> List.collect _.Quarry |> List.distinct)
                                (lazy value)

                        Some(
                            Facts.fightTask view room,
                            { brief with
                                Jobs = job :: brief.Jobs
                                Stances =
                                    holders
                                    |> List.map (fun creep -> creep.Name, rally.Value)
                                    |> Map.ofList
                            }
                        )

        // Only the rooms a raider stands in or an operation works, and only
        // a fight held there: the Briefs map is rebuilt for none of the rest.
        let waiting =
            (view.Hostiles
             |> List.filter (HostileInfo.raider view.Hostiles)
             |> List.map (fun hostile -> hostile.Pos.Room))
            @ Set.toList (operationRooms view)
            |> List.distinct
            |> List.choose (fun room ->
                Map.tryFind (Facts.fightTask view room) threats.Briefs
                |> Option.bind (musterOf room))

        match waiting with
        | [] -> threats
        | waiting ->
            { threats with
                Briefs =
                    (threats.Briefs, waiting)
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
