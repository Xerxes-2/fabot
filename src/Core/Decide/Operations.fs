/// The [[operation]] interpreter (#663 M7): once a full tick, over the
/// world, each admitted spec's record moved on by its schedule, its target
/// and its exits. S7a runs it dry: what it answers is its records and its
/// events, and nothing reads them back into a decision — no Brief, no cast,
/// no Task.
module Fabot.Core.Decide.Operations

open Fabot.Core.Types

/// Kleene's negation: `Unknown` stays unknown.
let private negate =
    function
    | Truth.Yes -> Truth.No
    | Truth.No -> Truth.Yes
    | Truth.Unknown -> Truth.Unknown

/// The rooms a target names; None where S7a cannot resolve it.
let private roomsOf (target: OpTarget) : string list option =
    match target with
    | OpTarget.Room room -> Some [ room ]
    | OpTarget.Structure(room, _) -> Some [ room ]
    | OpTarget.Among(RoomClass.Listed rooms) -> Some rooms
    | OpTarget.Among RoomClass.RivalRemotes
    | OpTarget.Among RoomClass.RivalRemoteExits
    | OpTarget.Quarry _ -> None

/// The target's room this tick: of its rooms, the one a cast of ours earns
/// most in (`OpFacts.Rate`), then by name; None where none earns anything,
/// or none is priced.
let target (facts: OpFacts) (spec: OperationSpec) : string option =
    roomsOf spec.Target
    |> Option.bind (fun rooms ->
        rooms
        |> List.choose (fun room -> facts.Rate room |> Option.map (fun rate -> room, rate))
        |> List.filter (fun (_, rate) -> rate > 0.0)
        |> List.sortBy (fun (room, rate) -> -rate, room)
        |> List.tryHead
        |> Option.map fst)

/// Whether a body worth its kill stands in sight in the room: Unknown where
/// the room is not priced, or only bodies out of sight could answer.
let private quarryIn (facts: OpFacts) (room: string) : Truth =
    match facts.Worth room with
    | None -> Truth.Unknown
    | Some worth ->
        let values = RoomWorth.values Quarry.Reading.Value worth

        if
            values
            |> Map.exists (fun id value -> value > 0.0 && not (worth.Dark.Contains id))
        then
            Truth.Yes
        elif Set.isEmpty worth.Dark then
            Truth.No
        else
            Truth.Unknown

/// A guard's answer for an operation as its record stands this tick.
let rec truth (facts: OpFacts) (spec: OperationSpec) (record: OpRecord) (guard: OpGuard) : Truth =
    match guard with
    | OpGuard.Always -> Truth.Yes
    | OpGuard.Not inner -> negate (truth facts spec record inner)
    | OpGuard.All guards ->
        let answers = guards |> List.map (truth facts spec record)

        if List.contains Truth.No answers then Truth.No
        elif List.contains Truth.Unknown answers then Truth.Unknown
        else Truth.Yes
    | OpGuard.Any guards ->
        let answers = guards |> List.map (truth facts spec record)

        if List.contains Truth.Yes answers then Truth.Yes
        elif List.contains Truth.Unknown answers then Truth.Unknown
        else Truth.No
    | OpGuard.QuarryIn
    | OpGuard.QuarryGone ->
        let seen =
            match roomsOf spec.Target with
            | Some rooms ->
                let answers =
                    rooms
                    |> List.map (fun room ->
                        if facts.Warm room then
                            quarryIn facts room
                        else
                            Truth.Unknown)

                if List.contains Truth.Yes answers then Truth.Yes
                elif List.contains Truth.Unknown answers then Truth.Unknown
                else Truth.No
            | _ -> Truth.Unknown

        if guard = OpGuard.QuarryIn then seen else negate seen
    | OpGuard.RateAbove floor ->
        match roomsOf spec.Target with
        | None -> Truth.Unknown
        | Some rooms ->
            let rates = rooms |> List.map facts.Rate

            if rates |> List.exists (Option.exists (fun rate -> rate > floor)) then
                Truth.Yes
            elif rates |> List.exists Option.isNone then
                Truth.Unknown
            else
                Truth.No
    | OpGuard.InPhaseFor ticks ->
        if facts.Now - record.Since >= ticks then
            Truth.Yes
        else
            Truth.No
    | OpGuard.PastDeadline ->
        if facts.Now >= spec.Limits.Deadline then
            Truth.Yes
        else
            Truth.No
    | OpGuard.ExchangeLoses
    | OpGuard.StructureStands
    | OpGuard.Assembled
    | OpGuard.Arrived
    | OpGuard.CastHurt _
    | OpGuard.LedgerLoss
    | OpGuard.HomeThreatened
    | OpGuard.SafeModeIn
    | OpGuard.Named _ -> Truth.Unknown

/// The exit a phase takes this tick, by its index: its exits read in order,
/// the first `Yes` taken; an `Unknown` before any `Yes` takes none, for the
/// exit it hides might have been the one.
let exitOf
    (facts: OpFacts)
    (spec: OperationSpec)
    (record: OpRecord)
    (exits: (OpGuard * OpNext) list)
    : (int * OpNext) option =
    let rec first index =
        function
        | [] -> None
        | (guard, next) :: rest ->
            match truth facts spec record guard with
            | Truth.Yes -> Some(index, next)
            | Truth.Unknown -> None
            | Truth.No -> first (index + 1) rest

    first 0 exits

/// One operation's record a tick on, and what moved: at most one change a
/// tick. Ended is absorbing. A record is begun by its schedule (`Once` and
/// `Every` at once, `When` on its guard's first `Yes`) and moves no further
/// that tick. Then its budget (spent past `MaxEnergy` ends it), then one
/// exit: an `Every` run past its window recurs, else its phase's first
/// exit taken. With none taken, past its deadline it expires.
let advance
    (facts: OpFacts)
    (spec: OperationSpec)
    (record: OpRecord option)
    : OpRecord option * OpEvent list =
    let first = spec.Phases |> List.tryHead |> Option.map _.Phase
    let now = facts.Now

    let begin' (phase: OpPhase) =
        let due =
            match spec.Schedule with
            | OpSchedule.Every(period, _) -> now + period
            | _ -> 0

        Some
            {
                Id = spec.Id
                Started = now
                Phase = phase
                Since = now
                Spent = 0
                Lost = 0
                NextDue = due
                Ended = None
            },
        [ OpEvent.Began(spec.Id, phase, target facts spec) ]

    let ended (record: OpRecord) (how: OpEnd) =
        Some { record with Ended = Some how }, [ OpEvent.Ended(spec.Id, how) ]

    match record, first with
    | _, None -> record, []
    | Some record, _ when Option.isSome record.Ended -> Some record, []
    | None, Some phase ->
        match spec.Schedule with
        | OpSchedule.When guard ->
            let unbegun =
                {
                    Id = spec.Id
                    Started = now
                    Phase = phase
                    Since = now
                    Spent = 0
                    Lost = 0
                    NextDue = 0
                    Ended = None
                }

            if truth facts spec unbegun guard = Truth.Yes then
                begin' phase
            else
                None, []
        | _ -> begin' phase
    // Waiting for its next run, or begun this tick.
    | Some record, _ when record.Started >= now -> Some record, []
    | Some record, Some phase ->
        let recur () =
            match spec.Schedule with
            | OpSchedule.Every(period, _) ->
                let next = max record.NextDue now

                Some
                    { record with
                        Started = next
                        Phase = phase
                        Since = next
                        NextDue = next + period
                    },
                [ OpEvent.Recurred(spec.Id, next) ]
            | _ -> ended record OpEnd.Done

        let windowed =
            match spec.Schedule with
            | OpSchedule.Every(_, window) -> now >= record.Started + window
            | _ -> false

        if record.Spent >= spec.Limits.MaxEnergy then
            ended record OpEnd.Budget
        elif windowed then
            recur ()
        else
            let exits =
                spec.Phases
                |> List.tryFind (fun p -> p.Phase = record.Phase)
                |> Option.map _.Exits
                |> Option.defaultValue []

            match exitOf facts spec record exits with
            | Some(_, OpNext.Finish how) -> ended record how
            | Some(_, OpNext.Recur) -> recur ()
            | Some(index, OpNext.To into) when spec.Phases |> List.exists (fun p -> p.Phase = into) ->
                Some
                    { record with
                        Phase = into
                        Since = now
                    },
                [ OpEvent.Moved(spec.Id, record.Phase, into, index, target facts spec) ]
            // The deadline ends it hard, whatever its phase. At S7c an ended
            // operation's bodies go back to their ordinary duty.
            | _ when now >= spec.Limits.Deadline -> ended record OpEnd.Expired
            | _ -> Some record, []

/// The interpreter's tick (#663 S7a, dry run): every admitted spec's record
/// advanced (`advance`), in the specs' order; each record `Operation.admit`
/// keeps (its spec held this tick, or ended) left as it stands; every other
/// record dropped: its spec gone from Memory, or turned unsafe.
let step
    (facts: OpFacts)
    (admission: OpAdmission)
    (records: OpRecord list)
    : OpRecord list * OpEvent list =
    let byId = records |> List.map (fun record -> record.Id, record) |> Map.ofList
    let admitted = admission.Admitted |> List.map _.Id |> Set.ofList

    let advanced =
        admission.Admitted
        |> List.map (fun spec -> advance facts spec (Map.tryFind spec.Id byId))

    let kept =
        records
        |> List.filter (fun record ->
            not (admitted.Contains record.Id) && admission.Kept.Contains record.Id)

    let dropped =
        records
        |> List.filter (fun record ->
            not (admitted.Contains record.Id || admission.Kept.Contains record.Id))
        |> List.map (fun record -> OpEvent.Dropped record.Id)

    (advanced |> List.choose fst) @ kept, (advanced |> List.collect snd) @ dropped

/// Who holds each room as far as anything we keep knows: this tick's look
/// (`RoomFacts.Control`: its owner, else its reservation's player, else
/// nobody), else a holder remembered past the tick — the world's sighting
/// of its owner, the declared rivals' rooms, and `remembered` (the rival
/// dossier's standing reservations) — else unknown. An ally's reservation
/// is remembered nowhere, so an ally's room out of sight is unknown, never
/// nobody's. Ours is a declared room, or one we own or reserve this tick.
let standingOf
    (now: int)
    (declared: Set<string>)
    (remembered: string -> string option)
    (world: World)
    : OpStanding =
    let facts (room: string) = Map.tryFind room world.Rooms
    let control (room: string) = facts room |> Option.bind _.Control

    let owner (room: string) =
        facts room
        |> Option.bind _.RivalController
        |> Option.map _.Owner
        |> Option.orElse (Map.tryFind room world.Sightings |> Option.bind _.Rival)

    let rememberedHolder (room: string) =
        Map.tryFind room world.Sightings
        |> Option.bind _.Rival
        |> Option.orElse (Colony.rivals |> List.tryFind (fst >> (=) room) |> Option.map snd)
        |> Option.orElse (remembered room)

    {
        Now = now
        Ours =
            fun room ->
                declared.Contains room
                || control room
                   |> Option.exists (fun c ->
                       c.Owner = Ownership.Ours
                       || Option.isSome (RoomControlInfo.heldBy ReservationHolder.Ours c))
        Holder =
            fun room ->
                match control room with
                | Some c when c.Owner = Ownership.Rival ->
                    match owner room with
                    | Some player -> RoomHolder.Player player
                    | None -> RoomHolder.Unknown
                | Some c ->
                    match RoomControlInfo.heldByOther c with
                    | Some held -> RoomHolder.Player held.Username
                    | None -> RoomHolder.Nobody
                | None ->
                    match rememberedHolder room with
                    | Some player -> RoomHolder.Player player
                    | None -> RoomHolder.Unknown
    }

/// Whether a room has been seen since the heap's reset at `since`: a
/// sighting at or after it (a reset seeds sightings at tick 0).
let warmSince (since: int) (world: World) (room: string) : bool =
    Map.tryFind room world.Sightings
    |> Option.exists (fun sighting -> sighting.Tick >= since)

/// The facts an operation reads, over the colonies' views as decided this
/// tick: a room's rate the best cast of a colony that prices it earns there
/// (`Facts.castRateIn`); its worth off the first such colony by home.
let factsOf (now: int) (warm: string -> bool) (views: ColonyView list) : OpFacts =
    let pricing (room: string) =
        views
        |> List.filter (fun view ->
            Map.containsKey room view.Spatial.Rooms
            || view.HarassCast.Contains room
            || view.Harass |> List.exists (fun h -> h.RoomName = room))
        |> List.sortBy (fun view -> SpatialInfo.homeName view.Spatial)

    {
        Now = now
        Rate =
            fun room ->
                match pricing room with
                | [] -> None
                | priced ->
                    priced |> List.map (fun view -> Facts.castRateIn view room) |> List.max |> Some
        Worth =
            fun room ->
                pricing room
                |> List.tryHead
                |> Option.map (fun view -> Facts.roomWorth view room)
        Warm = warm
    }

// ---- the sweep's shadow (#663 S7b) ----------------------------------------

/// The facts the [[sweep]]'s code spec reads in shadow: each listed room's
/// rate as the decide priced it this tick (`WorldView.SweepRates`, the price
/// `Facts.sweepChoice` reads), never priced a second time; a listed room it
/// priced none for — no colony casts it, or a stand-down shuts it — earns a
/// cast nothing. A room's worth is the world's table's.
let sweepFacts (now: int) (warm: string -> bool) (views: ColonyView list) : OpFacts =
    let rates =
        views
        |> List.tryPick (fun view ->
            match view.World.SweepRates with
            | [] -> None
            | rates -> Some rates)
        |> Option.defaultValue []

    {
        Now = now
        Rate =
            fun room ->
                rates
                |> List.tryFind (fst >> (=) room)
                |> Option.map snd
                |> Option.defaultValue 0.0
                |> Some
        Worth =
            fun room ->
                views
                |> List.tryPick (fun view -> view.World.Worths |> Option.bind (Map.tryFind room))
                |> Option.map _.Value
        Warm = warm
    }

/// One body of an operation's group: where it stands, and whether its own
/// colony casts the room it is counted in (`SweepAnswer.Scoped`).
type Member =
    {
        Creep: CreepInfo
        At: RoomPos
        OwnCast: bool
    }

/// An operation's group over its rooms (#663 M7), the world's roster
/// whichever colony a body is of — the rule the operation owns at S7c:
/// every body of ours that held the Guard of one of them last tick
/// (`held`), wherever it stands, and every armed one standing in one, each
/// once.
let groupIn (rooms: Set<string>) (held: Assignments) (views: ColonyView list) : Member list =
    if Set.isEmpty rooms then
        []
    else
        let guards =
            rooms |> Seq.map (fun room -> Facts.taskId (Guard room), room) |> Map.ofSeq

        views
        |> List.collect (fun view ->
            ColonyView.bodies view
            |> List.choose (fun creep ->
                let counted =
                    Map.tryFind creep.Name held
                    |> Option.bind (fun tid -> Map.tryFind tid guards)
                    |> Option.orElseWith (fun () ->
                        // Where it stands first: the body's reach is the
                        // dearer read.
                        SpatialInfo.creepPlacementOf view.Spatial creep.Name
                        |> Option.map _.Room
                        |> Option.filter (fun room ->
                            rooms.Contains room && (Fighter.ofCreep creep).Reach > 0))

                counted
                |> Option.bind (fun room ->
                    Facts.placementOf view creep.Name
                    |> Option.map (fun at ->
                        {
                            Creep = creep
                            At = at
                            OwnCast = view.HarassCast.Contains room
                        }))))
        |> List.distinctBy _.Creep.Name

/// The listed rooms the sweep works this tick, the only ones its group is
/// read over: of the rooms the decide priced (`WorldView.SweepRates`: a
/// colony casts it and no stand-down shuts it), the room its phase works,
/// each whose Guard a body held last tick, and each a body of the sweep's
/// group stands in. None while it works none: nothing is scanned.
let private workedRooms
    (listed: Set<string>)
    (room: string option)
    (held: Assignments)
    (views: ColonyView list)
    : Set<string> =
    let priced =
        views
        |> List.collect (fun view -> view.World.SweepRates |> List.map fst)
        |> Set.ofList
        |> Set.intersect listed

    if Set.isEmpty priced then
        Set.empty
    else
        let guards =
            priced |> Seq.map (fun room -> Facts.taskId (Guard room), room) |> Map.ofSeq

        let holding =
            held |> Map.toSeq |> Seq.choose (fun (_, tid) -> Map.tryFind tid guards)

        let standing =
            views
            |> Seq.collect (fun view -> view.World.SweepGroup |> Seq.map (fun (_, at) -> at.Room))

        Seq.concat [ Option.toList room |> Seq.ofList; holding; standing ]
        |> Set.ofSeq
        |> Set.intersect priced

/// The sweep's code spec resolved for one full tick, in shadow: its record
/// moved on by its exits over the decide's prices (`sweepFacts`,
/// `advance`), and what it would lay — the room its phase works (`target`
/// in `Execute`, none in `Hold`), its group over the rooms it works
/// (`groupIn`, `workedRooms`), its phase's declared jobs, and the seats its
/// priced count gives: the group's paid seats (`Facts.seatsPaid`), never
/// fewer than the chooser's cast for the room buys (`Facts.fighterCast`,
/// its bodies where it ends the fight). `priced` is the view a room's fight
/// is priced off, the Guard's owner's.
let sweepShadow
    (facts: OpFacts)
    (spec: OperationSpec)
    (record: OpRecord option)
    (held: Assignments)
    (views: ColonyView list)
    (priced: string -> ColonyView option)
    : OpRecord option * SweepAnswer =
    let record, _ = advance facts spec record

    let phase =
        record
        |> Option.filter (fun record -> Option.isNone record.Ended && record.Started <= facts.Now)
        |> Option.map _.Phase

    let room =
        match phase with
        | Some OpPhase.Execute -> target facts spec
        | _ -> None

    let listed = roomsOf spec.Target |> Option.defaultValue [] |> Set.ofList
    let group = groupIn (workedRooms listed room held views) held views

    let declared =
        phase
        |> Option.bind (fun phase -> spec.Phases |> List.tryFind (fun p -> p.Phase = phase))
        |> Option.map (fun p -> p.Jobs |> List.map _.Kind)
        |> Option.defaultValue []

    let seats =
        room
        |> Option.bind (fun room -> priced room |> Option.map (fun view -> room, view))
        |> Option.map (fun (room, view) ->
            let cast = Facts.fighterCast view room Bodies.bodyTemplates Engine.guardCap
            let wanted = if cast.Ends then cast.Bodies else 0
            let placed = group |> List.map (fun m -> m.Creep, m.At)
            max (Facts.seatsPaid view room placed) wanted)

    let names (members: Member list) =
        members |> List.map _.Creep.Name |> Set.ofList

    record,
    {
        Room = room
        Group = names group
        Scoped = group |> List.filter (fun m -> not m.OwnCast) |> names
        Jobs = if Option.isSome room then declared else []
        Seats = seats |> Option.defaultValue 0
    }

let private namesText (names: Set<string>) =
    if Set.isEmpty names then "-" else String.concat "," names

let private jobText (kind: JobKind) =
    match kind with
    | JobKind.Strike -> "Strike"
    | JobKind.Ring -> "Ring"
    | JobKind.Resident -> "Resident"
    | JobKind.Camp -> "Camp"
    | JobKind.Intercept -> "Intercept"
    | JobKind.HoldExit -> "HoldExit"
    | JobKind.Muster -> "Muster"

let private jobsText (jobs: JobKind list) =
    if List.isEmpty jobs then
        "-"
    else
        jobs |> List.map jobText |> String.concat ","

/// Where the shadow and the sweep disagree this tick, each kind once: the
/// room (a consistency check, `Divergence`), then the group whatever the
/// room — `Scope` where only bodies the shadow counts from a colony not
/// casting their room make the difference —; the jobs and the seats only
/// where both work the same room. The jobs disagree where the sweep lays a
/// kind the shadow's phase does not declare: a declared kind nothing lays
/// (no quarry for it) is no disagreement.
let disagreements (now: int) (shadow: SweepAnswer) (sweep: SweepAnswer) : ShadowExample list =
    let room = sweep.Room |> Option.orElse shadow.Room |> Option.defaultValue ""

    let example kind (ours: string) (theirs: string) (why: string) =
        {
            Tick = now
            Kind = kind
            Room = room
            Shadow = ours
            Sweep = theirs
            Why = why
        }

    let roomText = Option.defaultValue "none"

    let rooms =
        if shadow.Room = sweep.Room then
            []
        else
            // One price picks both rooms: a difference is the spec's own
            // filter after the pick.
            let why =
                match shadow.Room, sweep.Room with
                | None, Some _ -> "consistency: the spec works no room (its phase, its rooms)"
                | Some _, None -> "consistency: the sweep works no room"
                | _ -> "consistency: another room off one price"

            [ example Divergence.Room (roomText shadow.Room) (roomText sweep.Room) why ]

    let group =
        if shadow.Group = sweep.Group then
            []
        else
            // Each side's bodies the other lacks: the difference, not the
            // whole group, keeps the example short.
            let ours = Set.difference shadow.Group sweep.Group
            let theirs = Set.difference sweep.Group shadow.Group

            let kind, why =
                if Set.isEmpty theirs && Set.isSubset ours shadow.Scoped then
                    Divergence.Scope,
                    "the world's roster counts bodies their own colony does not cast for"
                else
                    Divergence.Group, "bodies only one side counts"

            [ example kind (namesText ours) (namesText theirs) why ]

    let worked =
        match shadow.Room, sweep.Room with
        | Some ours, Some theirs when ours = theirs ->
            let undeclared =
                sweep.Jobs |> List.filter (fun kind -> not (List.contains kind shadow.Jobs))

            let jobs =
                if List.isEmpty undeclared then
                    []
                else
                    [
                        example
                            Divergence.Jobs
                            (jobsText shadow.Jobs)
                            (jobsText sweep.Jobs)
                            $"laid but not declared: {jobsText undeclared}"
                    ]

            let seats =
                if shadow.Seats = sweep.Seats then
                    []
                else
                    let more = if sweep.Seats > shadow.Seats then "more" else "fewer"

                    [
                        example
                            Divergence.Seats
                            (string shadow.Seats)
                            (string sweep.Seats)
                            $"the sweep seats {more} than its group pays for and its cast buys"
                    ]

            jobs @ seats
        | _ -> []

    rooms @ group @ worked

/// The log a compared tick on: one more tick compared (and worked, where
/// the sweep worked a room), each disagreement counted, the latest kept
/// newest first, at most `Operation.shadowCap`, an older one saying what a
/// newer one says (its kind, room and answers) dropped, so a disagreement
/// that stands tick after tick is one example.
let logged (log: ShadowLog) (sweep: SweepAnswer) (examples: ShadowExample list) : ShadowLog =
    let same (a: ShadowExample) (b: ShadowExample) =
        a.Kind = b.Kind && a.Room = b.Room && a.Shadow = b.Shadow && a.Sweep = b.Sweep

    {
        Compared = log.Compared + 1
        Working = log.Working + (if Option.isSome sweep.Room then 1 else 0)
        Counts =
            examples
            |> List.fold
                (fun (counts: Map<Divergence, int>) example ->
                    counts
                    |> Map.add
                        example.Kind
                        (1 + (Map.tryFind example.Kind counts |> Option.defaultValue 0)))
                log.Counts
            |> Map.toSeq
            |> Fresh.mapOfSeq
        Last =
            examples
            @ (log.Last |> List.filter (fun old -> not (List.exists (same old) examples)))
            |> List.truncate Operation.shadowCap
        Refused = log.Refused
    }

/// The sweep the code declares (`Colony.operations`), through the code
/// spec's safety (`Operation.ofCode`): the spec, or None, and this tick's
/// refusals — the declaration's own (candidates of two enemies), the whole
/// spec's, or each room dropped from it.
let declaredSweep
    (standing: OpStanding)
    (declared: Result<OpSpecWire list, string>)
    : OperationSpec option * OpRefusal list =
    match declared with
    | Error why -> None, [ { Id = "sweep"; Why = why } ]
    | Ok wires ->
        match wires |> List.tryFind (fun wire -> wire.Template = "sweep") with
        | None -> None, []
        | Some wire ->
            match Operation.ofCode standing wire with
            | Ok(spec, dropped) -> Some spec, dropped
            | Error refused -> None, [ refused ]

/// One whole full tick of the sweep's shadow: the declared spec
/// (`declaredSweep`) resolved (`sweepShadow`) and compared with what the
/// sweep decided (`sweep`), the record moved on and the log grown
/// (`disagreements`, `logged`), this tick's refusals laid in. With no spec
/// the record stands and nothing is compared.
let shadowStep
    (standing: OpStanding)
    (declared: Result<OpSpecWire list, string>)
    (facts: OpFacts)
    (record: OpRecord option, log: ShadowLog)
    (held: Assignments)
    (views: ColonyView list)
    (priced: string -> ColonyView option)
    (sweep: SweepAnswer)
    : OpRecord option * ShadowLog =
    match declaredSweep standing declared with
    | None, refused -> record, { log with Refused = refused }
    | Some spec, refused ->
        let record, shadow = sweepShadow facts spec record held views priced

        record,
        { logged log sweep (disagreements facts.Now shadow sweep) with
            Refused = refused
        }
