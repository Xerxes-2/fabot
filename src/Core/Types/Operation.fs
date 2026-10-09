/// The [[operation]] vocabulary (#663 M7): a fight we start, written as data
/// (`OperationSpec`), and the one record of it Memory keeps (`OpRecord`).
/// A spec is either code's (`Operation.Templates`) or published through
/// Memory (`Memory.fabot.ops.specs`), and then admitted only by
/// `Operation.decode`. The interpreter is `Operations.step`.
[<AutoOpen>]
module Fabot.Core.Types.OperationTypes

/// What one job of a fight is (#663 M3): where its bodies stand and what
/// they are there for. Every kind is priced in one currency, the energy a
/// tick its bodies' effort earns (`Job.Value`).
[<RequireQualifiedAccess>]
type JobKind =
    /// A kill: under a raid or a rival's claimer (#451) in a ranger room
    /// (`Facts.rangerRooms`), or in a chased room a raid stands in, the
    /// ranger's ground about the kill order's head — our ramparts within
    /// three of it under an armed raid (#467); else in a resident room whose
    /// raid no ranger wins (`Facts.outmatched`), the room's safe set, laid
    /// only while a ranger of ours stands there or holds its Guard; else,
    /// while a melee body stands there, the head is a claimer, or in a
    /// resident room an unarmed head its kill score prices worth the kill
    /// (#648), the kite ground: the tiles within three of that head, less
    /// every tile a melee body reaches in a step and a swing, and less swamp
    /// near one; with a melee body there and that empty, the tiles farthest
    /// from it. Or a contested room's prey (#516, #656): where a trespasser
    /// stands (`Facts.trespasser`) and no kind ahead claims the room, the
    /// tiles beside each one off the exits, or a hauler's store ground.
    /// Priced at its head's kill score (#621).
    | Strike
    /// The walkable tiles within range 1 of a Threat standing in the room,
    /// less the tiles the Threats stand on — the guard's ground; and a
    /// level-0 core's ring (#487). Priced at the room's economic delta a tick
    /// it keeps (`RoomWorth.delta`).
    | Ring
    /// In a resident room (`Facts.residentRooms`), the garrison's ground,
    /// which it holds and shoots from: in a declared errand room the
    /// Reactor's own ring (#414, #411), in a raised child's home its
    /// controller's while no armed Threat stands there (#447). Priced at our
    /// income kept there.
    | Resident
    /// Where the kill is a dancer on the border (`Facts.sweepDancer`, #658):
    /// fixed tiles off the exits, out of every armed body's reach, covering
    /// the tiles it steps in on, the one it uses most first, one a holder
    /// (those it already stands on first). The dancer's tile now never moves
    /// it. Priced at its kill at the camp's odds over the kill's ticks
    /// (`Facts.campValue`).
    | Camp
    /// The tile a quarry seen lingering across the border steps back in on
    /// (#585). Priced at its kill score.
    | Intercept
    /// In a room whose last armed Threat has gone, the ground beside the exit
    /// it left by (#450), until a look finds it gone (#585). Priced at the
    /// room's economic delta a tick it keeps.
    | HoldExit
    /// The crossing the holders of a fight wait on for the rest of their
    /// group (#535, #658, `Facts.musters`): priced at what the first come
    /// alone would lose over the waiting it saves, a tick of the spread.
    | Muster

/// A guard's answer: three-valued, so a fact nobody can read this tick
/// (a room out of sight, a quarry window still refilling after a reset)
/// is `Unknown`, and `Unknown` never moves an operation.
[<RequireQualifiedAccess>]
type Truth =
    | Yes
    | No
    | Unknown

/// One phase of an operation.
[<RequireQualifiedAccess>]
type OpPhase =
    | Muster
    | Approach
    | Execute
    | Hold
    | Retreat

/// How an operation ended. Absorbing: an ended record never moves again.
[<RequireQualifiedAccess>]
type OpEnd =
    /// An exit said its work is done.
    | Done
    /// An exit gave it up (its exchange lost, its losses past the limit).
    | Abandoned
    /// Its deadline came: hard, whatever its phase and its exits; its bodies
    /// go back to their ordinary duty.
    | Expired
    /// Its energy budget (`OpLimits.MaxEnergy`) ran out.
    | Budget

/// A guard: a predicate over the tick's facts (`OpFacts`), plain data so a
/// spec can be written, compared and published. Every case is evaluated by
/// `Operations.truth`; a case marked "Unknown in S7a" has its vocabulary
/// but no reading yet, and answers `Unknown`, so it never moves an
/// operation until a later slice reads it.
[<RequireQualifiedAccess>]
type OpGuard =
    | Always
    | Not of OpGuard
    | All of OpGuard list
    | Any of OpGuard list
    /// A body worth its kill stands in one of the target's rooms
    /// (`RoomWorth.values` at its odds, seen this tick). Unknown while only
    /// bodies out of sight could answer it, or the quarry window has not
    /// refilled since a reset (`OpFacts.Warm`).
    | QuarryIn
    /// `Not QuarryIn`.
    | QuarryGone
    /// The best of the target's rooms earns a cast of ours more than this,
    /// energy a tick (`OpFacts.Rate`).
    | RateAbove of float
    /// The operation has stood in its phase this many ticks.
    | InPhaseFor of int
    /// Its deadline has come (`OpLimits.Deadline`).
    | PastDeadline
    /// The group loses the exchange against the escort of the room its record
    /// is committed to (`OpFacts.Loses`); Unknown with no room committed.
    | ExchangeLoses
    /// Unknown in S7a: the target structure still stands.
    | StructureStands
    /// The group's muster lets it go into the room its record is committed
    /// to: no body of it waits for the rest (`OpFacts.Assembled`); Unknown
    /// with no room committed.
    | Assembled
    /// A body of the group stands in the room its record is committed to,
    /// or on its doorstep (`OpFacts.Arrived`); Unknown with no room
    /// committed.
    | Arrived
    /// Unknown in S7a: a body of the cast is under a fraction of its hits.
    | CastHurt of float
    /// Unknown in S7a: the ledger's loss ratio past `OpLimits.MaxLossRatio`.
    | LedgerLoss
    /// Every colony that may cast for it has its own home threatened
    /// (`OpFacts.HomeThreatened`): none of them bids, home defence first.
    | HomeThreatened
    /// Another of the target's rooms earns the group more a tick, over its
    /// own spell, than the room its record is committed to: the other room
    /// read on evidence (`OpFacts.Challenge`: only a body seen there on two
    /// full ticks or more counts, so a body crossing it for a tick moves
    /// nothing), its rate net of the group's walk there; the committed room
    /// read with the effort already spent in it (`OpFacts.Rate`) — its group
    /// in or by it walks nothing, the volleys landed are the hits its quarry
    /// lacks, its escort as it stands, and a body seen rooted at its work
    /// there since it committed is priced on its work
    /// (`WorldView.Committed`). Yes with no room committed and one room
    /// earning anything.
    | Outpaid
    /// The room its record is committed to holds nothing more for the
    /// group: no body there worth its kill, in sight or remembered at its
    /// odds (`OpFacts.Holds`), or the group loses its escort
    /// (`OpFacts.Loses`). Evidence, never one tick's rate: a quarry the
    /// group cannot catch this tick still holds the room. Yes with no room
    /// committed.
    | Exhausted
    /// Unknown in S7a: safe mode runs in the target room.
    | SafeModeIn
    /// Unknown in S7a: a code-registered predicate, by name.
    | Named of string

/// Where an exit leads.
[<RequireQualifiedAccess>]
type OpNext =
    | To of OpPhase
    | Finish of OpEnd
    /// The run is over; on an `Every` schedule the next is due a period
    /// after this one started, on any other the operation is done.
    | Recur

/// The rooms an `Among` target chooses between.
[<RequireQualifiedAccess>]
type RoomClass =
    | Listed of string list
    /// Unresolved in S7a: the enemy's remotes the rival dossier sees worked.
    | RivalRemotes
    /// Unresolved in S7a: their remotes' exits, for an interdiction camp.
    | RivalRemoteExits

/// What an operation is against.
[<RequireQualifiedAccess>]
type OpTarget =
    | Room of string
    /// The room of the class that earns the most (`OpFacts.Rate`).
    | Among of RoomClass
    /// Unresolved in S7a: a structure, by room and id.
    | Structure of room: string * id: string
    /// Unresolved in S7a: one body, by id.
    | Quarry of string

/// When an operation runs.
[<RequireQualifiedAccess>]
type OpSchedule =
    /// From the tick it is admitted until it ends.
    | Once
    /// A run every `period` ticks, each at most `window` ticks long.
    | Every of period: int * window: int
    /// From the first tick the guard answers `Yes`.
    | When of OpGuard

/// Which colonies may cast for it: only ever those past the Caster's floors.
[<RequireQualifiedAccess>]
type CastFrom =
    | AnyPastFloors
    | Homes of string list
    | WithinHops of int

/// The row a job seats.
[<RequireQualifiedAccess>]
type JobRole =
    | Fighter
    | Dismantler
    | Healer

/// How many bodies a job seats.
[<RequireQualifiedAccess>]
type JobCount =
    | Fixed of int
    | UpTo of int
    /// As many as the job's price pays for (`Facts.fighterCast`).
    | Priced

type JobSpec =
    {
        Kind: JobKind
        Role: JobRole
        Count: JobCount
    }

/// One phase: its jobs, and its exits in order, the first whose guard
/// answers `Yes` taken; an `Unknown` before it takes none. An exit into the
/// phase it leaves re-enters it: the record's room is chosen again.
type PhaseSpec =
    {
        Phase: OpPhase
        Jobs: JobSpec list
        Exits: (OpGuard * OpNext) list
        /// Guards a glance can read that may force one full tick (S7c).
        Urgent: OpGuard list
    }

/// What a tick of the operation is worth.
[<RequireQualifiedAccess>]
type OpValue =
    /// The target room's worth (`RoomWorth`, `Facts.roomRate`).
    | Worth
    | PerTick of float

type OpLimits =
    {
        MaxBodies: int
        /// The energy it may spend over its life, its casts summed.
        MaxEnergy: int
        /// The tick it ends on, whatever its phase.
        Deadline: int
        MaxLossRatio: float
    }

type OperationSpec =
    {
        Id: string
        /// Whose bodies it is about; narrows `Facts.mayStrike`, never widens it.
        Enemy: string
        Target: OpTarget
        Schedule: OpSchedule
        Casting: CastFrom
        /// The first is where a run starts.
        Phases: PhaseSpec list
        Value: OpValue
        Limits: OpLimits
    }

/// The one record of an operation Memory keeps (`Memory.fabot.ops.live`),
/// about sixty bytes: its latch. What its group is, is the assignments
/// leaf's (each Task names the operation); its room is latched here and
/// moves only as its phase does (#663 S7c).
type OpRecord =
    {
        Id: string
        /// The tick its run started; later than now while an `Every`
        /// schedule waits for its next run.
        Started: int
        Phase: OpPhase
        /// The tick it entered `Phase`.
        Since: int
        /// The room it is committed to: chosen as it enters a phase — the
        /// room it was committed to while that room still pays and nothing
        /// outpays it, else of its target's rooms the one earning most
        /// (`Operations.target`), None where none earns anything — and kept
        /// through every phase until the next entry, a hold among them. So
        /// the room changes only on an exit (`OpGuard.Outpaid`, or the room
        /// exhausted, `OpGuard.Exhausted`, re-entering the phase).
        Room: string option
        /// The tick `Room` was chosen: what its commitment is priced from
        /// (`WorldView.Committed`).
        RoomSince: int
        /// The energy its casts spent; nothing before S7c casts.
        Spent: int
        /// The energy of its bodies lost; nothing before S7c casts.
        Lost: int
        /// An `Every` schedule's next run, a period after this one started;
        /// 0 for every other schedule.
        NextDue: int
        Ended: OpEnd option
    }

/// What an operation's guards and target read this tick, over the world.
type OpFacts =
    {
        Now: int
        /// A room's rate a tick for the operation's group there, or with no
        /// group for a cast of ours (`Operations.ownFacts`); None where no
        /// colony prices it.
        Rate: string -> float option
        /// A room's worth this tick (`Facts.roomWorth`); None where no
        /// colony prices it.
        Worth: string -> RoomWorth option
        /// Whether the room has been seen since the heap's reset: until
        /// then a body not in the quarry memory may only be not yet
        /// remembered.
        Warm: string -> bool
        /// Whether a body of the group stands in the room or on its doorstep.
        Arrived: string -> Truth
        /// Whether the group's muster on the room lets it go in.
        Assembled: string -> Truth
        /// Whether the group loses the exchange against the room's escort.
        Loses: string -> Truth
        /// Whether a body worth its kill stands in the room, in sight or
        /// remembered at its odds (`RoomWorth.values`).
        Holds: string -> Truth
        /// A room's rate read on evidence (`OpGuard.Outpaid`): only its
        /// bodies seen there on two full ticks or more counted.
        Challenge: string -> float option
        /// Whether every colony that may cast for the operation has its home
        /// threatened.
        HomeThreatened: Truth
    }

/// Who holds a room that is not ours, as far as anything we keep knows.
[<RequireQualifiedAccess>]
type RoomHolder =
    /// Seen this tick, owned and reserved by nobody.
    | Nobody
    /// Owned or reserved by this player, seen this tick or remembered.
    | Player of string
    /// Neither seen this tick nor remembered held.
    | Unknown

/// Who stands in the rooms a spec names, the facts `Operation.decode`
/// refuses one on (`Operations.standingOf`).
type OpStanding =
    {
        Now: int
        /// A room of ours: a declared home or outpost, or one we own or
        /// reserve.
        Ours: string -> bool
        Holder: string -> RoomHolder
    }

/// Why a spec is not admitted this tick.
[<RequireQualifiedAccess>]
type OpRefused =
    /// It may read otherwise on a later tick, or is the operator's to fix:
    /// a running operation keeps its record, unmoved.
    | Holds of string
    /// Against an ally, a neutral or ours: its record is dropped.
    | Unsafe of string

/// A spec `Operation.decode` would not admit, and why
/// (`Memory.fabot.ops.refused`).
type OpRefusal = { Id: string; Why: string }

/// The specs admitted this tick (`Operation.admit`), those refused, and
/// the ids whose records stand unmoved though their spec is not admitted.
type OpAdmission =
    {
        Admitted: OperationSpec list
        Refused: OpRefusal list
        Kept: Set<string>
    }

/// What the interpreter did this tick (`Memory.fabot.ops.events`). In the
/// dry run (S7a, S7b) they are all it does.
[<RequireQualifiedAccess>]
type OpEvent =
    | Began of id: string * phase: OpPhase * target: string option
    | Moved of id: string * from: OpPhase * into: OpPhase * exit: int * target: string option
    | Recurred of id: string * due: int
    | Ended of id: string * OpEnd
    /// Its spec left Memory, or is refused now.
    | Dropped of id: string

/// The [[sweep]]'s answers for one full tick (#663 S7b), as the decide gave
/// them (`Facts.sweepAll` and the room's Guard Brief) or as the operation
/// interpreter resolves the code spec (`Operations.sweepShadow`): the room
/// worked, every body of the group, its jobs' kinds, first first, and the
/// seats they sum to.
type SweepAnswer =
    {
        Room: string option
        Group: Set<string>
        /// The shadow's alone: the bodies of its group whose own colony does
        /// not cast the room they are counted in. The operation's roster is
        /// the world's (S7c); the sweep's group is each colony's over the
        /// candidates it casts (`Facts.sweepGroup`). Empty for the sweep.
        Scoped: Set<string>
        Jobs: JobKind list
        Seats: int
    }

/// Which answer a shadow tick disagrees on. Since S7c-1 every kind compares
/// computations made apart: the shadow prices its rooms with its own group
/// and keeps the room its record committed to (`OpRecord.Room`), where the
/// sweep re-picks each tick off its own group's prices.
[<RequireQualifiedAccess>]
type Divergence =
    | Room
    | Group
    /// A group difference made wholly of bodies `SweepAnswer.Scoped` names:
    /// the roster's scope, not a defect.
    | Scope
    | Jobs
    | Seats

/// One disagreement: the tick, the room (the sweep's, else the shadow's,
/// else none), both answers as text and why they differ.
type ShadowExample =
    {
        Tick: int
        Kind: Divergence
        Room: string
        Shadow: string
        Sweep: string
        Why: string
    }

/// The shadow's bounded log (`Memory.fabot.shadow`): the full ticks it
/// compared, a count of each kind of disagreement, and the latest few.
/// It outlives the tick (#401): `Counts` is built by `Fresh`.
type ShadowLog =
    {
        Compared: int
        /// Of those, the ticks the sweep worked a room.
        Working: int
        Counts: Map<Divergence, int>
        Last: ShadowExample list
        /// This tick's refusals of the code spec (`Operation.ofCode`): a
        /// room dropped from it, or the whole spec.
        Refused: OpRefusal list
    }

[<RequireQualifiedAccess>]
module Operation =
    /// The ticks a Memory spec runs when it names no deadline (user, #663).
    let defaultSpan = 5000

    /// The operations that may be live at once, ended ones aside: a named
    /// safety margin on what the CLI may start (design, #663).
    let liveCap = 4

    /// The events `Memory.fabot.ops.events` keeps, newest last.
    let eventCap = 20

    /// The longest id a spec may carry: it rides in every record and event.
    let idLength = 24

    /// The longest a Memory spec may run, from its first tick to its
    /// deadline: a named safety margin, ten default spans, which also keeps
    /// the budget ceiling's arithmetic far inside a 32-bit integer on both
    /// runtimes.
    let maxSpan = 10 * defaultSpan

    /// BODYPART_COST[HEAL]: the dearest part, the ceiling a body's cost is
    /// priced at before its template is chosen.
    let private dearestPart = 250

    /// The most energy `bodies` bodies spend kept alive over `span` ticks:
    /// each a whole body of the dearest part, recast each lifetime. A
    /// template's budget ceiling, which a Memory spec only lowers.
    let ceiling (bodies: int) (span: int) : int =
        let lives = (max 0 span + Engine.creepLifetime - 1) / Engine.creepLifetime
        bodies * Engine.maxBodyParts * dearestPart * lives

    /// Operations written in code, each a function of the parameters a
    /// Memory spec may give it.
    module Templates =
        /// The [[sweep]] as a spec: one group over the listed rooms, working
        /// the one its group earns most in, committed to it until another
        /// pays more on evidence net of the walk and the effort spent there
        /// (`OpGuard.Outpaid`), or the room is exhausted (`OpGuard.Exhausted`:
        /// nothing worth its kill there, or an escort the group loses) and
        /// another pays; holding, the room kept, while an exhausted room has
        /// no successor or no colony that may cast for it has its home
        /// clear. Its muster is a job of its execute phase, priced as today;
        /// so is the ring a contested room lays under its escort's raid
        /// (#656), which the sweep's room is fought as. The most bodies its
        /// group seats: a named safety margin, the guard cap's twice over
        /// (`Engine.guardCap`).
        let sweepBodies = 2 * Engine.guardCap

        let sweep
            (id: string)
            (enemy: string)
            (rooms: string list)
            (limits: OpLimits)
            : OperationSpec =
            let fighter kind =
                {
                    Kind = kind
                    Role = JobRole.Fighter
                    Count = JobCount.Priced
                }

            {
                Id = id
                Enemy = enemy
                Target = OpTarget.Among(RoomClass.Listed rooms)
                Schedule = OpSchedule.Once
                Casting = CastFrom.AnyPastFloors
                Phases =
                    [
                        {
                            Phase = OpPhase.Execute
                            Jobs =
                                [
                                    fighter JobKind.Strike
                                    fighter JobKind.Camp
                                    fighter JobKind.Ring
                                    fighter JobKind.Muster
                                ]
                            Exits =
                                [
                                    OpGuard.HomeThreatened, OpNext.To OpPhase.Hold
                                    OpGuard.Outpaid, OpNext.To OpPhase.Execute
                                    OpGuard.All [ OpGuard.Exhausted; OpGuard.RateAbove 0.0 ],
                                    OpNext.To OpPhase.Execute
                                    OpGuard.Exhausted, OpNext.To OpPhase.Hold
                                ]
                            Urgent = []
                        }
                        {
                            Phase = OpPhase.Hold
                            Jobs = []
                            Exits =
                                [
                                    OpGuard.All
                                        [
                                            OpGuard.Not OpGuard.HomeThreatened
                                            OpGuard.RateAbove 0.0
                                        ],
                                    OpNext.To OpPhase.Execute
                                ]
                            Urgent = []
                        }
                    ]
                Value = OpValue.Worth
                Limits = limits
            }

        /// A template by the name a Memory spec gives, with the most bodies
        /// it seats.
        let byName (name: string) =
            match name with
            | "sweep" -> Some(sweep, sweepBodies)
            | _ -> None

    /// The disagreements `ShadowLog.Last` keeps, newest first.
    let shadowCap = 8

    /// A log that has compared nothing.
    let shadowEmpty: ShadowLog =
        {
            Compared = 0
            Working = 0
            Counts = Fresh.mapOfList []
            Last = []
            Refused = []
        }

    /// A fault's quarantine: the keys already recorded with this one added,
    /// where it is new; None for one recorded before, which is not recorded
    /// again (a repeat would rewrite the fault log every tick).
    let firstSeen (seen: Set<string>) (key: string) : Set<string> option =
        if seen.Contains key then
            None
        else
            Some(Fresh.setOfSeq (Seq.append seen [ key ]))

    /// Whether a schedule can run: an `Every` run's period at least a tick,
    /// its window no longer than its period.
    let validSchedule (schedule: OpSchedule) : bool =
        match schedule with
        | OpSchedule.Every(period, window) -> period > 0 && window >= 0 && window <= period
        | OpSchedule.Once
        | OpSchedule.When _ -> true

    /// Whether a room may be an operation's target: a room's one spelling
    /// (`RoomName.isCanonical`), never ours, and held by nobody or by a
    /// player we strike anywhere (`Colony.strikesAnywhere`): an ally's
    /// never, a neutral's never, and one whose holder nothing we keep
    /// knows not yet (it holds until a look or a memory names one).
    let private roomRefusal (standing: OpStanding) (room: string) : OpRefused option =
        if not (RoomName.isCanonical room) then
            Some(OpRefused.Holds("no room: " + room))
        elif standing.Ours room then
            Some(OpRefused.Unsafe("ours: " + room))
        else
            match standing.Holder room with
            | RoomHolder.Player holder when Colony.isAlly holder ->
                Some(OpRefused.Unsafe("an ally's: " + room))
            | RoomHolder.Player holder when not (Colony.strikesAnywhere holder) ->
                Some(OpRefused.Unsafe("a neutral's: " + room))
            | RoomHolder.Unknown -> Some(OpRefused.Holds("holder unknown: " + room))
            | RoomHolder.Player _
            | RoomHolder.Nobody -> None

    /// A spec code declares (`Colony.operations`) as its template writes
    /// it, through the same safety a Memory spec passes (`decode`): never
    /// against an ally nor a neutral (the whole spec refused), and a room
    /// that is ours, an ally's or a neutral's dropped from it with a refusal
    /// of its own. Nothing else of `decode` binds code: a room whose holder
    /// is unknown out of sight stays, as the declaration does; no deadline
    /// nor budget but what it names, for it runs until code drops it.
    let ofCode
        (standing: OpStanding)
        (wire: OpSpecWire)
        : Result<OperationSpec * OpRefusal list, OpRefusal> =
        let refused (why: string) = { Id = wire.Id; Why = why }

        match Templates.byName wire.Template with
        | None -> Error(refused ("no template: " + wire.Template))
        | Some _ when Colony.isAlly wire.Enemy -> Error(refused ("an ally: " + wire.Enemy))
        | Some _ when not (Colony.strikesAnywhere wire.Enemy) ->
            Error(refused ("a neutral: " + wire.Enemy))
        | Some(template, bodies) ->
            let unsafe (room: string) =
                match roomRefusal standing room with
                | Some(OpRefused.Unsafe why) -> Some why
                | Some(OpRefused.Holds _)
                | None -> None

            let dropped = wire.Rooms |> List.choose unsafe

            Ok(
                template
                    wire.Id
                    wire.Enemy
                    (wire.Rooms |> List.filter (unsafe >> Option.isNone))
                    {
                        MaxBodies = wire.MaxBodies |> Option.defaultValue bodies
                        MaxEnergy = wire.MaxEnergy |> Option.defaultValue System.Int32.MaxValue
                        Deadline = wire.Deadline |> Option.defaultValue System.Int32.MaxValue
                        MaxLossRatio = 1.0
                    },
                dropped |> List.map refused
            )

    /// A Memory spec admitted, or why not: the one gate between Memory and
    /// the interpreter. Its template must be code's; its enemy one we strike
    /// anywhere, never an ally nor a neutral; every room a target
    /// (`roomRefusal`). Each parameter only narrows the template: the budget
    /// and the bodies are the template's ceiling at most, which is also
    /// what they are when none is given. Every span is read from the
    /// operation's first tick, `begun` (its record's start), or now for one
    /// not begun, so nothing about a running operation is re-priced as its
    /// life passes: the deadline, when none is given, is `defaultSpan` past
    /// it (the shell writes it back), never past `maxSpan`; the budget
    /// ceiling is priced over that span once. A fresh spec whose deadline
    /// has come is refused; a running one is never refused on elapsed
    /// time, `Operations.step` ends it.
    let decode
        (standing: OpStanding)
        (begun: int option)
        (wire: OpSpecWire)
        : Result<OperationSpec, OpRefused> =
        let limit (given: int option) (most: int) =
            match given with
            | Some n -> min n most
            | None -> most

        let from = begun |> Option.defaultValue standing.Now

        if wire.Id = "" || wire.Id.Length > idLength then
            Error(OpRefused.Holds "id")
        else
            match Templates.byName wire.Template with
            | None -> Error(OpRefused.Holds("no template: " + wire.Template))
            | Some(template, bodies) ->
                if Colony.isAlly wire.Enemy then
                    Error(OpRefused.Unsafe("an ally: " + wire.Enemy))
                elif not (Colony.strikesAnywhere wire.Enemy) then
                    Error(OpRefused.Unsafe("a neutral: " + wire.Enemy))
                elif List.isEmpty wire.Rooms then
                    Error(OpRefused.Holds "no rooms")
                else
                    let refusals = wire.Rooms |> List.choose (roomRefusal standing)

                    let unsafe =
                        refusals
                        |> List.tryFind (function
                            | OpRefused.Unsafe _ -> true
                            | OpRefused.Holds _ -> false)

                    match unsafe |> Option.orElse (List.tryHead refusals) with
                    | Some refused -> Error refused
                    | None ->
                        let deadline = wire.Deadline |> Option.defaultValue (from + defaultSpan)
                        let maxBodies = limit wire.MaxBodies bodies

                        if Option.isNone begun && deadline <= standing.Now then
                            Error(OpRefused.Holds "deadline past")
                        elif deadline - from > maxSpan then
                            Error(OpRefused.Holds "deadline too far")
                        elif maxBodies <= 0 then
                            Error(OpRefused.Holds "no bodies")
                        else
                            let maxEnergy =
                                limit wire.MaxEnergy (ceiling maxBodies (deadline - from))

                            let spec =
                                template
                                    wire.Id
                                    wire.Enemy
                                    (List.distinct wire.Rooms)
                                    {
                                        MaxBodies = maxBodies
                                        MaxEnergy = maxEnergy
                                        Deadline = deadline
                                        MaxLossRatio = 1.0
                                    }

                            if maxEnergy <= 0 then
                                Error(OpRefused.Holds "no budget")
                            elif not (validSchedule spec.Schedule) then
                                Error(OpRefused.Holds "schedule")
                            else
                                Ok spec

    /// Every Memory spec decoded (`decode`), in Memory's order: each id
    /// once, the first. An id whose operation has ended is refused ("id
    /// ended"): its record stands, ended, and a new operation takes a new
    /// id. No more live at once than `liveCap`: the operations already
    /// running keep their seats, and only a newcomer is refused for want of
    /// one. A refusal drops a running operation's record only where its spec
    /// turned unsafe (`OpRefused.Unsafe`); any other keeps it, unmoved.
    let admit
        (standing: OpStanding)
        (records: OpRecord list)
        (wires: OpSpecWire list)
        : OpAdmission =
        let byId = records |> List.map (fun record -> record.Id, record) |> Map.ofList

        let ended (id: string) =
            Map.tryFind id byId |> Option.exists (fun record -> Option.isSome record.Ended)

        let running (id: string) =
            Map.tryFind id byId |> Option.exists (fun record -> Option.isNone record.Ended)

        let decoded =
            wires
            |> List.mapFold
                (fun (seen: Set<string>) (wire: OpSpecWire) ->
                    let answer =
                        if seen.Contains wire.Id then
                            Error(OpRefused.Holds "duplicate id")
                        elif ended wire.Id then
                            Error(OpRefused.Holds "id ended")
                        else
                            decode standing (Map.tryFind wire.Id byId |> Option.map _.Started) wire

                    (wire.Id, answer), seen.Add wire.Id)
                Set.empty
            |> fst

        let unsafe =
            decoded
            |> List.choose (fun (id, answer) ->
                match answer with
                | Error(OpRefused.Unsafe _) -> Some id
                | _ -> None)
            |> Set.ofList

        // Every id Memory lists whose spec has not turned unsafe.
        let listed =
            decoded |> List.map fst |> List.distinct |> List.filter (unsafe.Contains >> not)

        let seated =
            let live =
                decoded
                |> List.choose (fun (id, answer) ->
                    match answer with
                    | Ok _ -> Some id
                    | Error _ -> None)

            // Every running operation whose spec stands keeps its seat, its
            // spec admitted this tick or held.
            let occupied = listed |> List.filter running |> List.length

            let newcomers =
                live
                |> List.filter (running >> not)
                |> List.truncate (max 0 (liveCap - occupied))

            Set.ofList (List.filter running live @ newcomers)

        let refusal (id: string) (refused: OpRefused) =
            match refused with
            | OpRefused.Holds why
            | OpRefused.Unsafe why -> { Id = id; Why = why }

        {
            Admitted =
                decoded
                |> List.choose (fun (id, answer) ->
                    match answer with
                    | Ok spec when seated.Contains id -> Some spec
                    | _ -> None)
            Refused =
                decoded
                |> List.choose (fun (id, answer) ->
                    match answer with
                    | Ok _ when seated.Contains id -> None
                    | Ok _ -> Some { Id = id; Why = "live-op cap" }
                    | Error refused -> Some(refusal id refused))
            Kept = listed |> List.filter (seated.Contains >> not) |> Set.ofList
        }
