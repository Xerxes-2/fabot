/// The [[operation]] interpreter (#663 M7): once a full tick, over the
/// world, each spec's record moved on by its schedule, its target and its
/// exits. The code's sweep is laid for every colony (`lay`, #663 S7c): the
/// room its record is committed to is fought under its Task (`Operate`),
/// a Brief per operation; so is each room an exit camp code declares works
/// (#663 S7d), one Task an exit. A Memory spec still runs dry (`step`): its
/// records and its events, and nothing read back into a decision.
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
    | OpTarget.Exits exits -> Some(exits |> List.map _.Camp)
    | OpTarget.Among RoomClass.RivalRemotes
    | OpTarget.Among RoomClass.RivalRemoteExits
    | OpTarget.Quarry _ -> None

/// The target's room this tick: of its rooms, the one a cast of ours earns
/// most in (`OpFacts.Rate`), then by name; None where none earns anything,
/// or none is priced. None for exits, which are worked all at once.
let target (facts: OpFacts) (spec: OperationSpec) : string option =
    match spec.Target with
    | OpTarget.Exits _ -> None
    | _ ->
        roomsOf spec.Target
        |> Option.bind (fun rooms ->
            rooms
            |> List.choose (fun room -> facts.Rate room |> Option.map (fun rate -> room, rate))
            |> List.filter (fun (_, rate) -> rate > 0.0)
            |> List.sortBy (fun (room, rate) -> -rate, room)
            |> List.tryHead
            |> Option.map fst)

/// One of the target's other rooms read this tick as a bid for the latch
/// off the room committed to: what it pays on evidence (`OpFacts.Challenge`),
/// the ticks a move there takes to realise (`OpFacts.Arrival`), and whether
/// a move there pays for itself on what this tick saw (`Bids`).
type private Bid =
    {
        Room: string
        Paid: float
        Horizon: int
        /// Yes where, in sight this tick, the move pays for itself on
        /// evidence; No where, in sight, it does not even on the room's plain
        /// rate: seen not out-paying. Unknown otherwise — out of sight, its
        /// price is memory, and in sight on a plain rate evidence has yet to
        /// confirm — which neither confirms a bid nor resets it, nor moves
        /// the latch.
        Bids: Truth
    }

/// The target's other rooms read as bids off `held`, the room committed to;
/// none where the committed room is not priced, and a room not priced is no
/// bid. A move pays for itself where the room's pay over the ticks the group
/// takes to stand at its quarry (`Bid.Horizon`, a tick at least: the move is
/// a tick's act) beats what `held` pays (`OpFacts.Rate`) over those ticks and
/// over the walk the move adds beyond the one to `held`'s own quarry. So a
/// move whose quarry lies no farther than the held room's pays on any
/// surplus, and one from the held room's quarry, the whole horizon a walk,
/// must earn the held room's yield over that walk as well as its surplus.
let private bidsOn (facts: OpFacts) (spec: OperationSpec) (held: string) : Bid list =
    match roomsOf spec.Target, facts.Rate held with
    | Some rooms, Some rate ->
        let stay = facts.Arrival held |> Option.defaultValue 0

        rooms
        |> List.filter ((<>) held)
        |> List.choose (fun room ->
            match facts.Challenge room, facts.Rate room, facts.Arrival room with
            | Some paid, Some plain, Some horizon ->
                let span = float (max 1 horizon)
                let rewalk = float (max 0 (horizon - stay))
                let pays (yields: float) = yields * span > rate * (span + rewalk)

                Some
                    {
                        Room = room
                        Paid = paid
                        Horizon = horizon
                        Bids =
                            if not (facts.Sighted room) then Truth.Unknown
                            elif pays paid then Truth.Yes
                            elif pays plain then Truth.Unknown
                            else Truth.No
                    }
            | _ -> None)
    | _ -> []

/// The room that outpays `held`, the room committed to (`OpGuard.Outpaid`):
/// of the rooms whose bid has stood for as long as a move there takes to
/// realise (`OpRecord.Bids` against its horizon) and is seen out-paying
/// this tick, the one paying most, then by name. A bid stands from the
/// tick the room was seen out-paying until a tick it is seen not to: a
/// comparison noise reverses sooner is no evidence for a move that cannot
/// land before it, and a tick nothing sees the room says nothing either
/// way, so a room seen only on looks counts its horizon between them and
/// moves on a look. None before then. The room an `Outpaid` exit moves to.
let private challenger (facts: OpFacts) (spec: OperationSpec) (record: OpRecord) : string option =
    match record.Room with
    | None -> None
    | Some held ->
        bidsOn facts spec held
        |> List.filter (fun bid ->
            bid.Bids = Truth.Yes
            && record.Bids
               |> List.exists (fun (room, since) ->
                   room = bid.Room && facts.Now - since >= bid.Horizon))
        |> List.sortBy (fun bid -> -bid.Paid, bid.Room)
        |> List.tryHead
        |> Option.map _.Room

/// The record with each room's bid for its latch moved on a tick
/// (`bidsOn`), each on its own: one seen out-paying stands from the tick it
/// first did, one seen not out-paying is dropped, and one nothing reads
/// this tick keeps what it had.
let private bidding (facts: OpFacts) (spec: OperationSpec) (record: OpRecord) : OpRecord =
    let bids =
        match record.Room with
        | None -> []
        | Some held ->
            let standing (room: string) =
                record.Bids |> List.tryFind (fst >> (=) room)

            let read = bidsOn facts spec held

            let moved =
                read
                |> List.choose (fun bid ->
                    match bid.Bids, standing bid.Room with
                    | Truth.Yes, Some kept -> Some kept
                    | Truth.Yes, None -> Some(bid.Room, facts.Now)
                    | Truth.Unknown, kept -> kept
                    | Truth.No, _ -> None)

            // A room not priced this tick keeps what it had: nothing saw it
            // fail.
            let unread =
                record.Bids
                |> List.filter (fun (room, _) ->
                    room <> held && not (read |> List.exists (fun bid -> bid.Room = room)))

            moved @ unread |> List.sortBy fst

    if bids = record.Bids then
        record
    else
        { record with Bids = bids }

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

/// Kleene's conjunction of answers read in turn: the first `No` ends it
/// (none after it is read), else any `Unknown`, else `Yes`.
let private allOf (answers: (unit -> Truth) list) : Truth =
    let rec go unknown =
        function
        | [] -> if unknown then Truth.Unknown else Truth.Yes
        | answer :: rest ->
            match answer () with
            | Truth.No -> Truth.No
            | Truth.Unknown -> go true rest
            | Truth.Yes -> go unknown rest

    go false answers

/// Kleene's disjunction of answers read in turn: `allOf` negated.
let private anyOf (answers: (unit -> Truth) list) : Truth =
    answers |> List.map (fun answer () -> negate (answer ())) |> allOf |> negate

/// A reading of the room a record is committed to; Unknown with none.
/// Against exits, of every room they land in, combined by `over`: the
/// exits have no one room.
let private committed
    (spec: OperationSpec)
    (record: OpRecord)
    (over: (unit -> Truth) list -> Truth)
    (read: string -> Truth)
    : Truth =
    match spec.Target with
    | OpTarget.Exits exits -> exits |> List.map (fun exit () -> read exit.Camp) |> over
    | _ -> record.Room |> Option.map read |> Option.defaultValue Truth.Unknown

/// A guard's answer for an operation as its record stands this tick.
let rec truth (facts: OpFacts) (spec: OperationSpec) (record: OpRecord) (guard: OpGuard) : Truth =
    match guard with
    | OpGuard.Always -> Truth.Yes
    | OpGuard.Not inner -> negate (truth facts spec record inner)
    | OpGuard.All guards ->
        guards |> List.map (fun guard () -> truth facts spec record guard) |> allOf
    | OpGuard.Any guards ->
        guards |> List.map (fun guard () -> truth facts spec record guard) |> anyOf
    | OpGuard.ExitsUsed -> facts.ExitsUsed.Value
    | OpGuard.Pays -> facts.Pays.Value
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
    | OpGuard.ExchangeLoses -> committed spec record anyOf facts.Loses
    | OpGuard.Assembled -> committed spec record allOf facts.Assembled
    | OpGuard.Arrived -> committed spec record allOf facts.Arrived
    | OpGuard.HomeThreatened -> facts.HomeThreatened
    | OpGuard.Outpaid ->
        match roomsOf spec.Target with
        | None -> Truth.Unknown
        | Some rooms ->
            match record.Room with
            | None -> truth facts spec record (OpGuard.RateAbove 0.0)
            | Some room ->
                match facts.Rate room, challenger facts spec record with
                | None, _ -> Truth.Unknown
                | Some _, Some _ -> Truth.Yes
                // None that has paid long enough: Unknown where a room
                // unpriced might have.
                | Some _, None ->
                    if
                        rooms
                        |> List.exists (fun other ->
                            other <> room
                            && (Option.isNone (facts.Challenge other)
                                || Option.isNone (facts.Arrival other)))
                    then
                        Truth.Unknown
                    else
                        Truth.No
    | OpGuard.Exhausted ->
        match record.Room with
        | None -> Truth.Yes
        | Some room ->
            match facts.Holds room, facts.Loses room with
            | Truth.No, _
            | _, Truth.Yes -> Truth.Yes
            | Truth.Yes, Truth.No -> Truth.No
            | _ -> Truth.Unknown
    | OpGuard.StructureStands
    | OpGuard.CastHurt _
    | OpGuard.LedgerLoss
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
/// tick. Ended is absorbing, but for an `Armed` run ended short of its
/// deadline, which begins again on its guard's first `Yes` once its next
/// run is due (`NextDue`). A record is begun by its schedule (`Once` and
/// `Every` at once, `When` and `Armed` on its guard's first `Yes`) and
/// moves no further that tick. Then its deadline, hard: on it the operation
/// expires, whatever its phase. Then its budget (spent past `MaxEnergy`
/// ends it), then one exit: an `Every` or `Armed` run past its window
/// recurs (an `Armed` one ends done), else its phase's first exit taken.
/// Each phase it enters, its room is chosen and latched there until
/// the next: the room it was committed to, its commitment kept, while
/// nothing outpays it (`challenger`) and it is not exhausted; else the
/// target's best (`target`). The room bidding to outpay it is kept on the
/// record every tick it runs its exits (`bidding`).
let advance
    (facts: OpFacts)
    (spec: OperationSpec)
    (record: OpRecord option)
    : OpRecord option * OpEvent list =
    let first = spec.Phases |> List.tryHead |> Option.map _.Phase
    let now = facts.Now
    let room = lazy (target facts spec)

    let begin' (phase: OpPhase) =
        let due =
            match spec.Schedule with
            | OpSchedule.Every(period, _)
            | OpSchedule.Armed(_, period, _) -> now + period
            | _ -> 0

        Some
            {
                Id = spec.Id
                Started = now
                Phase = phase
                Since = now
                Room = room.Value
                RoomSince = now
                Spent = 0
                Lost = 0
                NextDue = due
                Ended = None
                Bids = []
            },
        [ OpEvent.Began(spec.Id, phase, room.Value) ]

    let ended (record: OpRecord) (how: OpEnd) =
        Some { record with Ended = Some how }, [ OpEvent.Ended(spec.Id, how) ]

    // A run begun on its guard's first `Yes`, read as a record not begun
    // would; else the record as it stands.
    let armed (guard: OpGuard) (phase: OpPhase) (standing: OpRecord option) =
        let unbegun =
            {
                Id = spec.Id
                Started = now
                Phase = phase
                Since = now
                Room = None
                RoomSince = now
                Spent = 0
                Lost = 0
                NextDue = 0
                Ended = None
                Bids = []
            }

        if truth facts spec unbegun guard = Truth.Yes then
            begin' phase
        else
            standing, []

    match record, first with
    | _, None -> record, []
    // An armed schedule's run ended by an exit, its window or its budget (a
    // run's) waits out its period, then on its guard again; its deadline
    // ends it for good.
    | Some record, Some phase when
        Option.isSome record.Ended
        && record.Ended <> Some OpEnd.Expired
        && now >= record.NextDue
        && now < spec.Limits.Deadline
        ->
        match spec.Schedule with
        | OpSchedule.Armed(guard, _, _) -> armed guard phase (Some record)
        | _ -> Some record, []
    | Some record, _ when Option.isSome record.Ended -> Some record, []
    | None, Some phase ->
        match spec.Schedule with
        | OpSchedule.When guard
        | OpSchedule.Armed(guard, _, _) -> armed guard phase None
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
                        Room = None
                        RoomSince = next
                        NextDue = next + period
                        Bids = []
                    },
                [ OpEvent.Recurred(spec.Id, next) ]
            // An armed run's end: the next is due when it began (`NextDue`).
            | _ -> ended record OpEnd.Done

        let windowed =
            match spec.Schedule with
            | OpSchedule.Every(_, window)
            | OpSchedule.Armed(_, _, window) -> now >= record.Started + window
            | _ -> false

        // The deadline ends it hard, whatever its phase and its exits: an
        // ended operation lays no Brief, so its bodies go back to their
        // ordinary duty.
        if now >= spec.Limits.Deadline then
            ended record OpEnd.Expired
        elif record.Spent >= spec.Limits.MaxEnergy then
            ended record OpEnd.Budget
        elif windowed then
            recur ()
        else
            // The room bidding for its latch, counted on from its first bid.
            let record = bidding facts spec record

            let exits =
                spec.Phases
                |> List.tryFind (fun p -> p.Phase = record.Phase)
                |> Option.map _.Exits
                |> Option.defaultValue []

            // The room it enters the phase with: the one it is committed to,
            // its commitment kept through every phase, a hold among them,
            // unless another room outpays it (that room, `challenger`), or it
            // is exhausted and another room pays (the target's best); else
            // the target's best, committed from now.
            let entered () =
                match record.Room with
                | Some kept when truth facts spec record OpGuard.Outpaid = Truth.Yes ->
                    match challenger facts spec record with
                    | Some other -> Some other, now
                    | None -> Some kept, record.RoomSince
                | Some kept when truth facts spec record OpGuard.Exhausted = Truth.Yes ->
                    match room.Value with
                    | Some other when other <> kept -> Some other, now
                    | _ -> Some kept, record.RoomSince
                | Some kept -> Some kept, record.RoomSince
                | None -> room.Value, now

            match exitOf facts spec record exits with
            | Some(_, OpNext.Finish how) -> ended record how
            | Some(_, OpNext.Recur) -> recur ()
            | Some(index, OpNext.To into) when spec.Phases |> List.exists (fun p -> p.Phase = into) ->
                match entered () with
                // Into the phase it is in, with the room it holds: no
                // transition, so nothing moves — its phase's tick, its
                // commitment and the Task its room is fought under.
                | into', _ when into = record.Phase && into' = record.Room -> Some record, []
                | into', since ->
                    // What decided a move of its room: the rate it left, what
                    // the room it entered paid, and the move's horizon.
                    let why =
                        into'
                        |> Option.filter (fun _ -> into' <> record.Room)
                        |> Option.map (fun room ->
                            {
                                Held =
                                    record.Room
                                    |> Option.bind facts.Rate
                                    |> Option.defaultValue 0.0
                                Paid =
                                    if challenger facts spec record = Some room then
                                        facts.Challenge room
                                    else
                                        facts.Rate room
                                    |> Option.defaultValue 0.0
                                Horizon = facts.Arrival room |> Option.defaultValue 0
                            })

                    Some
                        { record with
                            Phase = into
                            Since = now
                            Room = into'
                            RoomSince = since
                            Bids = if into' = record.Room then record.Bids else []
                        },
                    [ OpEvent.Moved(spec.Id, record.Phase, into, index, into', why) ]
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

/// Who holds a room this tick's look found (`RoomFacts.Control`): its
/// owner, else its reservation's player, else nobody; None out of sight,
/// and for a rival's room whose owner nothing names.
let holderSeen (world: World) (room: string) : RoomHolder option =
    let facts = Map.tryFind room world.Rooms

    match facts |> Option.bind _.Control with
    | Some c when c.Owner = Ownership.Rival ->
        facts
        |> Option.bind _.RivalController
        |> Option.map _.Owner
        |> Option.orElse (Map.tryFind room world.Sightings |> Option.bind _.Rival)
        |> Option.map RoomHolder.Player
    | Some c ->
        match RoomControlInfo.heldByOther c with
        | Some held -> Some(RoomHolder.Player held.Username)
        | None -> Some RoomHolder.Nobody
    | None -> None

/// Who holds each room as far as anything we keep knows: this tick's look
/// (`holderSeen`), else the world's sighting of its owner and the declared
/// rivals' rooms, else `remembered` — the last look at it a shell keeps
/// (its last sighting this heap, else the rival dossier's: the rival whose
/// reservation stood there, or nobody where that reservation was seen gone)
/// — else unknown: a room never looked at since the reset, and one an ally
/// or a neutral held at its last look out of the dossier's sight. Ours is
/// a declared room, or one we own or reserve this tick.
let standingOf
    (now: int)
    (declared: Set<string>)
    (remembered: string -> RoomHolder option)
    (world: World)
    : OpStanding =
    let control (room: string) =
        Map.tryFind room world.Rooms |> Option.bind _.Control

    let owned (room: string) =
        Map.tryFind room world.Sightings
        |> Option.bind _.Rival
        |> Option.orElse (Colony.rivals |> List.tryFind (fst >> (=) room) |> Option.map snd)
        |> Option.map RoomHolder.Player

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
                | Some _ -> holderSeen world room |> Option.defaultValue RoomHolder.Unknown
                | None ->
                    owned room
                    |> Option.orElse (remembered room)
                    |> Option.defaultValue RoomHolder.Unknown
    }

/// The rooms the colonies declare ours (`standingOf`'s `declared`): each
/// one's declared rooms but those it declares to strike. An assault's room
/// (W18S26 among them) is the enemy's, never ours: read as ours it refused
/// the exit camp on its own target (#663 S7d).
let declaredOurs (colonies: Colony list) : Set<string> =
    colonies
    |> Seq.collect (fun colony ->
        Set.difference
            (Colony.declaredRooms colony)
            (colony.Assaults |> List.map _.RoomName |> Set.ofList))
    |> Fresh.setOfSeq

/// Whether a room has been seen since the heap's reset at `since`: a
/// sighting at or after it (a reset seeds sightings at tick 0).
let warmSince (since: int) (world: World) (room: string) : bool =
    Map.tryFind room world.Sightings
    |> Option.exists (fun sighting -> sighting.Tick >= since)

/// Whether a room is in sight this tick for any of the views
/// (`ColonyView.Seen`: a room seen this tick carries its time).
let private sightedIn (now: int) (views: ColonyView list) (room: string) : bool =
    views
    |> List.exists (fun view ->
        Map.tryFind room view.Seen
        |> Option.exists (fun sighting -> sighting.Tick >= now))

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
        // A Memory spec's dry run casts no group: nothing of one is read.
        Arrived = fun _ -> Truth.Unknown
        Assembled = fun _ -> Truth.Unknown
        Loses = fun _ -> Truth.Unknown
        Holds =
            fun room ->
                match pricing room |> List.tryHead with
                | Some view ->
                    if
                        RoomWorth.values Quarry.Reading.Value (Facts.roomWorth view room)
                        |> Map.exists (fun _ value -> value > 0.0)
                    then
                        Truth.Yes
                    else
                        Truth.No
                | None -> Truth.Unknown
        Challenge =
            fun room ->
                match pricing room with
                | [] -> None
                | priced ->
                    priced |> List.map (fun view -> Facts.castRateIn view room) |> List.max |> Some
        // No group: the cast lands wherever it is sent alike.
        Arrival =
            fun room ->
                match pricing room with
                | [] -> None
                | _ -> Some 0
        Sighted = sightedIn now views
        HomeThreatened =
            match views with
            | [] -> Truth.No
            | views when List.forall Facts.homeThreatened views -> Truth.Yes
            | _ -> Truth.No
        // A Memory spec's dry run watches no exit and prices no camp.
        ExitsUsed = lazy Truth.Unknown
        Pays = lazy Truth.Unknown
    }

// ---- the operations laid (#663 S7c) ---------------------------------------

/// One body of an operation's group, with where it stands.
type Member = { Creep: CreepInfo; At: RoomPos }

/// An operation's group (#663 M7), the world's roster whichever colony a
/// body is of: every body of ours holding its Task last tick (`held`,
/// `op:<id>`) or the Guard of one of its rooms, wherever it stands, and
/// every armed one standing in one of its rooms, each once.
let groupIn
    (id: string)
    (rooms: Set<string>)
    (held: Assignments)
    (views: ColonyView list)
    : Member list =
    if Set.isEmpty rooms then
        []
    else
        let tasks =
            rooms
            |> Seq.map (fun room -> Facts.taskId (Guard room))
            |> Seq.append [ Facts.taskId (Operate(id, "")) ]
            |> Set.ofSeq

        views
        |> List.collect (fun view ->
            ColonyView.bodies view
            |> List.filter (fun creep ->
                Map.tryFind creep.Name held |> Option.exists tasks.Contains
                // Where it stands first: the body's reach is the dearer read.
                || SpatialInfo.creepPlacementOf view.Spatial creep.Name
                   |> Option.exists (fun at -> rooms.Contains at.Room)
                   && (Fighter.ofCreep creep).Reach > 0)
            |> List.choose (fun creep ->
                Facts.placementOf view creep.Name
                |> Option.map (fun at -> { Creep = creep; At = at })))
        |> List.distinctBy _.Creep.Name

/// The room a record works this tick: its committed room while it runs in
/// `Execute`, none in any other phase, before its run, or once it ended.
let workedRoom (now: int) (record: OpRecord option) : string option =
    record
    |> Option.filter (fun record ->
        Option.isNone record.Ended
        && record.Started <= now
        && record.Phase = OpPhase.Execute)
    |> Option.bind _.Room

/// The room a record is committed to while it runs, whatever its phase (a
/// hold keeps it, `OpRecord.Room`); none before its run or once it ended.
let committedRoom (now: int) (record: OpRecord option) : string option =
    record
    |> Option.filter (fun record -> Option.isNone record.Ended && record.Started <= now)
    |> Option.bind _.Room

/// Whether a body stood in its room on two full ticks or more, as its track
/// keeps them (`Track.Trail`): evidence it works there, where one sample is a
/// body crossing it.
let private sampled (view: ColonyView) (hostile: HostileInfo) : bool =
    Map.tryFind hostile.Id view.World.Quarries
    |> Option.exists (fun track ->
        track.Trail
        |> List.filter (fun (_, at) -> at.Room = hostile.Pos.Room)
        |> List.map fst
        |> List.distinct
        |> List.length
        |> fun samples -> samples >= 2)

/// A view of a room on evidence (`sampled`): its bodies seen there once
/// dropped, the view itself where none is.
let private evidenced (room: string) (view: ColonyView) : ColonyView =
    let kept =
        view.Hostiles
        |> List.filter (fun hostile -> hostile.Pos.Room <> room || sampled view hostile)

    if List.length kept = List.length view.Hostiles then
        view
    else
        { view with Hostiles = kept }

/// A body of the group as a muster weighs it (`Facts.Mustering`): its fight,
/// the ticks it lands in the room at its pace, its energy.
let private musteringOf (room: string) (body: Member) : Facts.Mustering =
    let fighter = Fighter.ofCreep body.Creep

    {
        Facts.Fighter = fighter
        Facts.Lands = Facts.landsIn body.At fighter room
        Facts.Energy =
            body.Creep.Body
            |> Map.toList
            |> List.collect (fun (part, n) -> List.replicate n part)
            |> bodyCost
            |> float
    }

/// An operation's own facts this tick (#663 S7c), priced with its own group
/// (`group`, the world's roster, `groupIn`) over the colonies' views, and
/// never off another's price: each listed room's rate is the group's where
/// it stands (`Facts.groupRate`, off the room's pricing view,
/// `Facts.pricedBy`), its arrival its slowest body's walk, so a room away
/// pays net of the walk there; with no group, the best cast of a colony
/// that casts the room (`casters`, `Facts.castRateIn`); in a room no colony
/// casts, nothing. The room its record works (`workedRoom`) is priced
/// committed (`WorldView.Committed`, from the tick its record entered the
/// phase): a body seen rooted at its work there since is priced on its
/// work. The ticks the group takes to stand at each room's quarry are its
/// slowest body's (`Facts.groupArrival`). The group's arrival, muster and
/// exchange are read in the room the guards ask of (`OpFacts`); its home is
/// threatened where every colony
/// that casts one of its rooms has a raider at home
/// (`Facts.homeThreatened`). A room's worth is the world's table's.
let ownFacts
    (now: int)
    (warm: string -> bool)
    (spec: OperationSpec)
    (record: OpRecord option)
    (casters: string -> ColonyView list)
    (group: Member list)
    (views: ColonyView list)
    : OpFacts =
    let listed = roomsOf spec.Target |> Option.defaultValue []

    let committed =
        match committedRoom now record, record with
        | Some room, Some record -> Map.ofList [ room, record.RoomSince ]
        | _ -> Map.empty

    // A view already laid with the commitment (`lay`) is priced as it is:
    // a copy would price every room again.
    let lay (view: ColonyView) =
        if view.World.Committed = committed then
            view
        else
            { view with
                World =
                    { view.World with
                        Committed = committed
                    }
            }

    let laid = List.map lay views
    let placed = group |> List.map (fun m -> m.Creep, m.At)

    let pricing (room: string) =
        Facts.pricedBy laid room
        |> Option.orElse (casters room |> List.tryHead |> Option.map lay)

    let rate (room: string) =
        match casters room with
        | [] -> 0.0
        | casting ->
            match placed with
            | [] -> casting |> List.map (fun view -> Facts.castRateIn (lay view) room) |> List.max
            | placed ->
                pricing room
                |> Option.map (fun view -> Facts.groupRate view placed room)
                |> Option.defaultValue 0.0

    let rates = listed |> List.map (fun room -> room, lazy (rate room)) |> Map.ofList

    // A room's rate on evidence (`evidenced`): what another room must show
    // to outpay the committed one. Its rate, less what evidence withholds:
    // a body seen there once earns the room nothing, and striking it never
    // earns the room more (an escort it costs time against, a heal, a slow
    // kill its rate is diluted by), so a room is never priced above its own
    // rate, the basis the committed room is priced on.
    let challenge (room: string) =
        let onEvidence () =
            match casters room with
            | [] -> 0.0
            | casting ->
                match placed with
                | [] ->
                    casting
                    |> List.map (fun view -> Facts.castRateIn (evidenced room (lay view)) room)
                    |> List.max
                | placed ->
                    pricing room
                    |> Option.map (fun view -> Facts.groupRate (evidenced room view) placed room)
                    |> Option.defaultValue 0.0

        let plain =
            Map.tryFind room rates
            |> Option.map _.Value
            |> Option.defaultWith (fun () -> rate room)

        if plain <= 0.0 then plain else min plain (onEvidence ())

    // Each listed room's challenge and the group's arrival there, once a
    // tick: the latch reads them on every tick it bids.
    let challenges =
        listed |> List.map (fun room -> room, lazy (challenge room)) |> Map.ofList

    let arrival (room: string) =
        match placed with
        | [] -> 0
        | placed ->
            pricing room
            |> Option.bind (fun view -> Facts.groupArrival view placed room)
            |> Option.defaultValue 0

    let arrivals =
        listed |> List.map (fun room -> room, lazy (arrival room)) |> Map.ofList

    let once (memo: Map<string, Lazy<'a>>) (read: string -> 'a) (room: string) : 'a =
        match Map.tryFind room memo with
        | Some answer -> answer.Value
        | None -> read room

    let truthOf (answer: bool) = if answer then Truth.Yes else Truth.No

    let reading (read: ColonyView -> string -> bool) (room: string) =
        match pricing room with
        | Some view -> truthOf (read view room)
        | None -> Truth.Unknown

    // In it or on its doorstep.
    let landed (room: string) (m: Member) =
        Facts.landsIn m.At (Fighter.ofCreep m.Creep) room = 0

    let casting =
        listed
        |> List.collect casters
        |> List.distinctBy (fun view -> SpatialInfo.homeName view.Spatial)

    // A look at its committed room is on its way while it executes there with
    // a group: the room's Brief walks the group in, and its arrival is the
    // look (`Facts.holdsQuarry`).
    let awaited (room: string) =
        not (List.isEmpty placed)
        && record
           |> Option.exists (fun record ->
               record.Phase = OpPhase.Execute && record.Room = Some room)

    // The group's bodies, or with none the cast a colony would send.
    let holdsOf =
        reading (fun view room ->
            let team =
                match placed with
                | [] ->
                    Facts.fighterCast view room Bodies.bodyTemplates Engine.guardCap
                    |> Facts.castFighters
                | placed -> placed |> List.map (fst >> Fighter.ofCreep)

            Facts.holdsQuarry view room team (awaited room))

    let loses = reading (fun view room -> Facts.groupLoses view placed room)

    // Each listed room's readings, once a tick: the guards and the rate read
    // them alike.
    let held = listed |> List.map (fun room -> room, lazy (holdsOf room)) |> Map.ofList

    let holds (room: string) =
        match Map.tryFind room held with
        | Some reading -> reading.Value
        | None -> holdsOf room

    // `Exhausted`'s own reading of a room: the rate reads it too, so a room
    // the guard calls exhausted pays nothing and no exit re-enters on a price
    // the guard has just refused.
    let exhausted =
        listed
        |> List.map (fun room -> room, lazy (holds room = Truth.No || loses room = Truth.Yes))
        |> Map.ofList

    let paying (room: string) (price: unit -> float) =
        match Map.tryFind room exhausted with
        | Some spent when spent.Value -> 0.0
        | _ -> price ()

    {
        Now = now
        Rate =
            fun room ->
                paying room (fun () ->
                    Map.tryFind room rates
                    |> Option.map _.Value
                    |> Option.defaultWith (fun () -> rate room))
                |> Some
        Worth =
            fun room ->
                views
                |> List.tryPick (fun view -> view.World.Worths |> Option.bind (Map.tryFind room))
                |> Option.map _.Value
        Warm = warm
        Arrived = fun room -> group |> List.exists (landed room) |> truthOf
        // The muster's one rule (`Facts.muster`): nobody waits once a body
        // of the group has landed, nor where the first come risks less
        // alone than the wait costs.
        Assembled =
            reading (fun view room ->
                List.exists (landed room) group
                || Option.isNone (Facts.muster view room (group |> List.map (musteringOf room))))
        Loses = loses
        Holds = holds
        Challenge = fun room -> Some(paying room (fun () -> once challenges challenge room))
        // No group, a cast lands wherever it is sent alike; a room no view
        // prices pays nothing, so its arrival bids nothing.
        Arrival = fun room -> Some(once arrivals arrival room)
        Sighted = sightedIn now views
        HomeThreatened =
            match casting with
            | [] -> Truth.No
            | casting -> truthOf (casting |> List.forall Facts.homeThreatened)
        // The sweep camps no exit: its spec reads neither.
        ExitsUsed = lazy Truth.Unknown
        Pays = lazy Truth.Unknown
    }

/// The [[sweep]] the colonies' views cast this tick (#663 S7c): the code's
/// spec (`Colony.operations`, admitted by the shell through the Memory
/// spec's safety, `Operation.ofCode`) reaches a colony as the candidates it
/// casts (`ColonyView.Harass`), so the sweep over every candidate any of
/// them casts, against their one enemy (`Colony.sweepOf`), under the limits
/// the admitted code spec names (`declared`, its deadline, budget and
/// bodies; none named, open-ended, the template's most bodies). None with no
/// candidate cast, or candidates of two enemies.
let sweepSpec (declared: OpSpecWire list) (views: ColonyView list) : OperationSpec option =
    // A room only an exit camp names is its camp's to cast (#663 S7d), no
    // candidate of the sweep's.
    let campOnly =
        declared
        |> List.filter (fun wire -> Operation.Templates.anchored wire.Template)
        |> List.collect _.Rooms
        |> List.filter (fun room ->
            not (
                declared
                |> List.exists (fun wire ->
                    wire.Template = "sweep" && List.contains room wire.Rooms)
            ))
        |> Set.ofList

    let candidates =
        views
        |> List.collect _.Harass
        |> List.distinctBy (fun h -> h.RoomName)
        |> List.filter (fun h -> not (campOnly.Contains h.RoomName))

    match Colony.sweepOf candidates with
    | Ok [ wire ] ->
        let named = declared |> List.tryFind (fun code -> code.Id = wire.Id)

        let limit (read: OpSpecWire -> int option) (open': int) =
            named |> Option.bind read |> Option.defaultValue open'

        Some(
            Operation.Templates.sweep
                wire.Id
                wire.Enemy
                wire.Rooms
                {
                    MaxBodies = limit _.MaxBodies Operation.Templates.sweepBodies
                    MaxEnergy = limit _.MaxEnergy System.Int32.MaxValue
                    Deadline = limit _.Deadline System.Int32.MaxValue
                    MaxLossRatio = 1.0
                }
        )
    | _ -> None

/// A code operation's record as its spec reads it now: ended on its
/// deadline or its budget while the spec no longer ends it there (a
/// deadline moved on, a budget raised, the always-on sweep under a stale
/// end) it begins again; ended by an exit, or on a deadline or budget the
/// spec still names, it stays ended.
let private reopened (now: int) (spec: OperationSpec) (record: OpRecord) : OpRecord option =
    match record.Ended with
    | Some OpEnd.Expired when now < spec.Limits.Deadline -> None
    | Some OpEnd.Budget when record.Spent < spec.Limits.MaxEnergy -> None
    | _ -> Some record

// ---- the exit camp laid (#663 S7d) ----------------------------------------

/// The id an exit camp's work in one room carries, its Task's
/// (`op:<id>@<room>`): one Task an exit, so a body holding one exit's is
/// never read as another's.
let campWorkId (id: string) (camp: string) : string = id + "@" + camp

/// The exit camps code declares, as their template writes them, under the
/// limits each names (none named, open-ended).
let private campSpecs (declared: OpSpecWire list) : OperationSpec list =
    declared
    |> List.filter (fun wire -> wire.Template = "exitCamp")
    |> List.map (fun wire ->
        Operation.Templates.exitCamp
            wire.Id
            wire.Enemy
            wire.Rooms
            {
                MaxBodies = wire.MaxBodies |> Option.defaultValue Engine.guardCap
                MaxEnergy = wire.MaxEnergy |> Option.defaultValue System.Int32.MaxValue
                Deadline = wire.Deadline |> Option.defaultValue System.Int32.MaxValue
                MaxLossRatio = 1.0
            })

/// Each exit camp code declares, by its id, with the rooms its exits land
/// in: what its spend is read over (`Ledger.account`).
let campRooms (declared: OpSpecWire list) : (string * Set<string>) list =
    campSpecs declared
    |> List.map (fun spec -> spec.Id, roomsOf spec.Target |> Option.defaultValue [] |> Set.ofList)

/// One exit as its camp reads it this tick (#663 S7d).
type private ExitRead =
    {
        Exit: OpExit
        /// The view that prices it: the first colony by home that casts its
        /// room; None where none does.
        View: ColonyView option
        /// Its group: every body holding its Task (`campHolders`), and none
        /// else — a body of ours standing in its room is another fight's.
        Group: Member list
        /// Whether prey uses it: two arrivals of the enemy over a watched
        /// window, a return among them; Unknown while its watch has not run
        /// a window since the heap's reset, or watched nothing.
        Used: Truth
        /// What its watch read (`ExitReading`): the ticks it has run, the
        /// full ticks watched, the enemy's landings and distinct bodies.
        Age: int
        Watched: int
        Landings: int
        Bodies: int
        /// Its landing tiles by use (`ExitCamp.Entries`); none known, the
        /// landing tiles at one each.
        Entries: (Pos * int) list
        /// The bodies that may land, each with its odds (`Facts.exitLanders`); and
        /// whether the home has been seen since the heap's reset, which they
        /// are known by.
        Landers: (HostileInfo * float) list
        Warm: bool
        /// Its camp priced (`Facts.exitCampPrice`); None with no view.
        Price: Lazy<Facts.ExitCampPrice option>
    }

/// The bodies holding one exit's Task (`op:<id>@<room>`, held last tick),
/// with where each stands: an exit camp's group, whichever colony cast it.
let private campHolders (id: string) (held: Assignments) (views: ColonyView list) : Member list =
    let task = Facts.taskId (Operate(id, ""))

    views
    |> List.collect (fun view ->
        ColonyView.bodies view
        |> List.filter (fun creep -> Map.tryFind creep.Name held = Some task)
        |> List.choose (fun creep ->
            Facts.placementOf view creep.Name
            |> Option.map (fun at -> { Creep = creep; At = at })))
    |> List.distinctBy _.Creep.Name

/// Whether a phase of a spec lays work: one with a job.
let private laysWork (spec: OperationSpec) (phase: OpPhase) : bool =
    spec.Phases
    |> List.exists (fun p -> p.Phase = phase && not (List.isEmpty p.Jobs))

/// Each exit of a camp read this tick (#663 S7d) over the priced views:
/// its group off last tick's Tasks (`held`), its watch off the world's
/// (`WorldView.Traffic`), and its camp priced over what the run has left of
/// its window (all of it before the run), against what a cast of the
/// colony pricing it earns in its best candidate (`Facts.castRateIn`): the
/// camp's own room among them, whose sweep the camp displaces while it
/// works it.
let private exitReads
    (now: int)
    (held: Assignments)
    (spec: OperationSpec)
    (record: OpRecord option)
    (priced: ColonyView list)
    : ExitRead list =
    let exits =
        match spec.Target with
        | OpTarget.Exits exits -> exits
        | _ -> []

    let window =
        match spec.Schedule with
        | OpSchedule.Armed(_, _, window)
        | OpSchedule.Every(_, window) -> window
        | _ -> Operation.Templates.campSpan

    let running =
        record
        |> Option.exists (fun record -> Option.isNone record.Ended && record.Started <= now)

    let left =
        match record with
        | Some record when running -> max 0 (record.Started + window - now)
        | _ -> window

    exits
    |> List.map (fun exit ->
        let view =
            priced
            |> List.filter (fun view ->
                view.Harass |> List.exists (fun h -> h.RoomName = exit.Camp))
            |> List.sortBy (fun view -> SpatialInfo.homeName view.Spatial)
            |> List.tryHead

        // Only the bodies holding its Task: one of ours standing in the room
        // is another fight's, and with none the camp prices its own cast.
        let group =
            if running then
                campHolders (campWorkId spec.Id exit.Camp) held priced
            else
                []

        let log =
            priced
            |> List.tryPick (fun view ->
                Map.tryFind (ExitTraffic.key exit.Home exit.Camp) view.World.Traffic)

        let enemy (h: HostileInfo) =
            h.Owner = spec.Enemy && Facts.mayStrikeAnywhere h.Owner

        let arrivals =
            log
            |> Option.map (fun log -> log.Arrivals |> List.filter (snd >> enemy))
            |> Option.defaultValue []

        let watched = log |> Option.map _.Watched |> Option.defaultValue []
        let landings = List.length arrivals

        let bodies =
            arrivals |> List.map (fun (_, h) -> h.Id) |> List.distinct |> List.length

        // The watch is heap only: where resets come oftener than its window,
        // it never runs one, and the camp never starts.
        let used =
            match log with
            | Some log when now - log.Since >= ExitTraffic.window && not (List.isEmpty log.Watched) ->
                if List.length arrivals >= 2 then Truth.Yes else Truth.No
            | _ -> Truth.Unknown

        // The traffic it is priced off: the window's bodies still in the
        // stream — none known gone (a kill takes one out), and in its run
        // none silent for longer than its own mean gap between landings
        // (the full ticks watched over its landings), so traffic deterred
        // stops paying within each body's own gap.
        let gone (id: string) =
            priced
            |> List.exists (fun view ->
                Map.tryFind id view.World.Quarries
                |> Option.exists (fun track -> track.Gone.IsSome))

        let arrivals =
            match used with
            | Truth.Unknown -> []
            | _ ->
                arrivals
                |> List.groupBy (fun (_, h) -> h.Id)
                |> List.filter (fun (id, landings) ->
                    not (gone id)
                    && (not running
                        || (let last = landings |> List.map fst |> List.max

                            let silent =
                                watched |> List.filter (fun tick -> tick > last) |> List.length

                            float silent < float (List.length watched)
                                           / float (List.length landings))))
                |> List.collect snd
                |> List.sortByDescending fst

        let border (room: string) =
            view
            |> Option.bind (fun view ->
                Map.tryFind room view.Spatial.Borders
                |> Option.orElse (Map.tryFind room view.World.Facts |> Option.map _.Border))

        let landing = ExitTraffic.landingsOver border exit.Home exit.Camp

        let entries =
            landing
            |> List.map (fun tile ->
                tile,
                1
                + (arrivals |> List.filter (fun (_, h) -> RoomPos.pos h.Pos = tile) |> List.length))

        let landers =
            view
            |> Option.map (fun view ->
                Facts.exitLanders
                    view
                    spec.Enemy
                    exit.Home
                    exit.Camp
                    landing
                    arrivals
                    watched
                    (record |> Option.filter (fun _ -> running) |> Option.map _.Started)
                    left)
            |> Option.defaultValue []

        let warm =
            view
            |> Option.bind (fun view -> Map.tryFind exit.Home view.Seen)
            |> Option.exists (fun sighting -> sighting.Tick > 0)

        let price =
            lazy
                (view
                 |> Option.map (fun view ->
                     // What a cast earns over so many ticks in its best
                     // candidate room: its rate there, never past what the
                     // room holds over those ticks — its bodies' kills once
                     // and the income its holding moves a tick.
                     let candidates =
                         view.Harass
                         |> List.map (fun h ->
                             let worth = Facts.roomWorth view h.RoomName

                             Facts.castRateIn view h.RoomName,
                             RoomWorth.values Quarry.Reading.Cast worth
                             |> Map.fold (fun sum _ v -> sum + v) 0.0,
                             RoomWorth.delta Quarry.Reading.Cast worth)

                     let elsewhere (ticks: int) =
                         candidates
                         |> List.map (fun (rate, kills, delta) ->
                             min (rate * float ticks) (kills + max 0.0 delta * float ticks))
                         |> List.fold max 0.0

                     Facts.exitCampPrice
                         view
                         exit.Camp
                         entries
                         arrivals
                         watched
                         landers
                         left
                         elsewhere
                         (group |> List.map (fun m -> m.Creep, m.At))))

        {
            Exit = exit
            View = view
            Group = group
            Used = used
            Age = log |> Option.map (fun log -> now - log.Since) |> Option.defaultValue 0
            Watched = List.length watched
            Landings = landings
            Bodies = bodies
            Entries = entries
            Landers = landers
            Warm = warm
            Price = price
        })

/// Whether an exit's camp pays (`OpGuard.Pays`): its price nets more than
/// nothing and it wins its exchange; Unknown while its crossings are, or no
/// colony prices it.
let private exitPays (read: ExitRead) : Truth =
    match read.Used with
    | Truth.Unknown -> Truth.Unknown
    | _ ->
        match read.Price.Value with
        | None -> Truth.Unknown
        | Some price ->
            if price.Net > 0.0 && not price.Loses then
                Truth.Yes
            else
                Truth.No

/// Whether an exit's camp loses its exchange (`OpGuard.ExchangeLoses`): its
/// body, or its group, against the bodies landing at odds 1
/// (`Facts.exitLanders`: one seen at the exits, or whose shape landed in
/// the run; the rest are priced as its expected loss), at an
/// exit prey uses or one its group stands at; at any other, no. Unknown
/// while the home has not been seen since the heap's reset (what may come
/// out is not known), while whether prey uses an exit no group stands at
/// is, or where no colony prices it.
let private exitLoses (read: ExitRead) : Truth =
    let standing = not (List.isEmpty read.Group)

    if not read.Warm then
        Truth.Unknown
    else
        match read.Used with
        | Truth.No when not standing -> Truth.No
        | Truth.Unknown when not standing -> Truth.Unknown
        | _ ->
            match read.Price.Value with
            | None -> Truth.Unknown
            | Some price -> if price.Loses then Truth.Yes else Truth.No

/// Whether a running camp works an exit this tick: while it pays, or while
/// its group stands for it and what it pays is not known (Unknown never
/// ends it).
let private exitWorked (read: ExitRead) : bool =
    match exitPays read with
    | Truth.Yes -> true
    | Truth.Unknown -> not (List.isEmpty read.Group)
    | Truth.No -> false

/// An exit camp's facts this tick (#663 S7d), off its exits' reads: prey
/// uses it where it uses any exit; it pays where any exit pays; its exchange
/// read at each exit's room; at an exit it does not work its muster and its
/// arrival ask nothing, at one it works a body of its group cast, and one
/// landed in the room.
let private campFacts (now: int) (reads: ExitRead list) (priced: ColonyView list) : OpFacts =
    let truthOf (answer: bool) = if answer then Truth.Yes else Truth.No

    let kleeneAny (answers: Truth list) =
        if List.contains Truth.Yes answers then Truth.Yes
        elif List.contains Truth.Unknown answers then Truth.Unknown
        else Truth.No

    let readOf (room: string) =
        reads |> List.tryFind (fun read -> read.Exit.Camp = room)

    let atWorked (answer: ExitRead -> Truth) (room: string) =
        match readOf room with
        | Some read when exitWorked read -> answer read
        | Some _ -> Truth.Yes
        | None -> Truth.Unknown

    let landed (room: string) (m: Member) =
        Facts.landsIn m.At (Fighter.ofCreep m.Creep) room = 0

    {
        Now = now
        Rate =
            fun room ->
                readOf room |> Option.bind (fun read -> read.Price.Value) |> Option.map _.Rate
        Worth =
            fun room ->
                priced
                |> List.tryPick (fun view -> view.World.Worths |> Option.bind (Map.tryFind room))
                |> Option.map _.Value
        Warm = fun room -> readOf room |> Option.exists (fun read -> read.Used <> Truth.Unknown)
        Arrived =
            atWorked (fun read -> read.Group |> List.exists (landed read.Exit.Camp) |> truthOf)
        Assembled = atWorked (fun read -> truthOf (not (List.isEmpty read.Group)))
        Loses = fun room -> readOf room |> Option.map exitLoses |> Option.defaultValue Truth.Unknown
        Holds = fun _ -> Truth.Unknown
        Challenge = fun _ -> None
        Arrival = fun _ -> None
        Sighted = sightedIn now priced
        HomeThreatened =
            match reads |> List.choose _.View with
            | [] -> Truth.No
            | views -> truthOf (views |> List.forall Facts.homeThreatened)
        ExitsUsed = lazy (reads |> List.map _.Used |> kleeneAny)
        Pays = lazy (reads |> List.map exitPays |> kleeneAny)
    }

/// One exit's reading (#663 S7e) off its read this tick: its watch, and
/// each guard as the trigger reads it. What it pays is read where prey uses
/// the exit, the one exit its price is already read at; elsewhere it is
/// left unread (Unknown), never priced for the reading alone.
let private readingOf (read: ExitRead) : ExitReading =
    let priced = read.Used = Truth.Yes

    {
        Camp = read.Exit.Camp
        Age = read.Age
        Watched = read.Watched
        Arrivals = read.Landings
        Bodies = read.Bodies
        Used = read.Used
        Pays = if priced then exitPays read else Truth.Unknown
        Priced =
            if priced then
                read.Price.Value |> Option.map (fun price -> price.Value, price.Cost)
            else
                None
        Loses = exitLoses read
        Landers = read.Landers |> List.map fst
    }

/// A body by its parts, most first, each count and the part's letter
/// (`17A18M`): what an operator reads of a body that may come out.
let partsText (body: BodyPart list) : string =
    let letter =
        function
        | BodyPart.Work -> "W"
        | BodyPart.Carry -> "C"
        | BodyPart.Move -> "M"
        | BodyPart.Attack -> "A"
        | BodyPart.RangedAttack -> "R"
        | BodyPart.Heal -> "H"
        | BodyPart.Claim -> "K"
        | BodyPart.Tough -> "T"

    body
    |> List.countBy letter
    |> List.sortBy (fun (part, n) -> -n, part)
    |> List.map (fun (part, n) -> $"{n}{part}")
    |> String.concat ""

/// Why an armed operation is not running (#663 S7e), "" where it is: its
/// deadline past, its last run's period not yet out, or each exit's guard
/// that holds its trigger — a watch warming or out of sight, too few
/// landings, a camp that does not pay, an exchange it loses and to what.
let idleWhy (now: int) (record: OpRecord option) (exits: ExitReading list) : string =
    let whole (v: float) = string (int (System.Math.Round v))

    match record with
    | Some record when record.Ended = Some OpEnd.Expired -> "ended: its deadline passed"
    | Some record when record.Ended.IsNone && record.Started <= now -> ""
    | Some record when record.NextDue > now ->
        let last =
            match record.Ended with
            | Some OpEnd.Done -> "done"
            | Some OpEnd.Abandoned -> "abandoned"
            | Some OpEnd.Budget -> "out of budget"
            | _ -> "waiting"

        $"re-arms at t{record.NextDue} (last run {last})"
    | _ ->
        let blocks (e: ExitReading) =
            [
                match e.Used with
                | Truth.Unknown when e.Watched = 0 -> $"{e.Camp}: not in sight"
                | Truth.Unknown -> $"{e.Camp}: watch warming {e.Age}/{ExitTraffic.window} ticks"
                | Truth.No when e.Arrivals = 0 ->
                    $"{e.Camp}: no arrivals in {e.Watched} ticks watched"
                | Truth.No ->
                    $"{e.Camp}: {e.Arrivals} arrival in {e.Watched} ticks watched, a return needed"
                | Truth.Yes -> ()

                match e.Pays, e.Priced with
                | Truth.No, Some(value, cost) ->
                    $"{e.Camp}: pays {whole value} against {whole cost}"
                | Truth.No, None -> $"{e.Camp}: does not pay"
                | Truth.Unknown, _ when e.Used = Truth.Yes -> $"{e.Camp}: not priced"
                | _ -> ()

                match e.Loses with
                | Truth.Yes ->
                    let landers =
                        e.Landers |> List.map (fun h -> partsText h.Body) |> String.concat "+"

                    $"{e.Camp}: loses to {landers}"
                | _ -> ()
            ]

        match exits |> List.collect blocks with
        | [] when List.isEmpty exits ->
            "no exit to camp: each landing room refused (`op list` names why)"
        | [] -> "trigger met"
        | reasons -> String.concat "; " reasons

/// One exit camp moved on and laid (#663 S7d): its record advanced over its
/// facts (`campFacts`), and, while its phase lays work, a work in the room
/// of each exit it works (`exitWorked`) — its body the price's, its tile
/// the one priced for that body, its most bodies its share of the spec's —
/// with the looks it asks: each room of an exit prey uses, and the home,
/// at what its camp earns over a look's stride.
let private layCamp
    (now: int)
    (held: Assignments)
    (priced: ColonyView list)
    (record: OpRecord option)
    (spec: OperationSpec)
    : OpRecord option * OpEvent list * OperationWork list * (string * float) list * OpReading =
    let reads = exitReads now held spec record priced
    let facts = campFacts now reads priced
    let moved, events = advance facts spec record

    let running =
        moved
        |> Option.exists (fun record ->
            Option.isNone record.Ended
            && record.Started <= now
            && laysWork spec record.Phase)

    // The reads again with the run as it stands: a run begun this tick
    // prices what its window has left.
    let reads =
        if running && moved <> record then
            exitReads now held spec moved priced
        else
            reads

    let share = max 1 (spec.Limits.MaxBodies / max 1 (List.length reads))

    let works =
        if not running then
            []
        else
            reads
            |> List.filter exitWorked
            |> List.choose (fun read ->
                read.Price.Value
                |> Option.map (fun price ->
                    {
                        Id = campWorkId spec.Id read.Exit.Camp
                        Enemy = spec.Enemy
                        Room = read.Exit.Camp
                        Group = read.Group |> List.map (fun m -> m.Creep, m.At)
                        Rate = price.Rate
                        MaxBodies = share
                        Camp =
                            Some
                                {
                                    Home = read.Exit.Home
                                    Entries = read.Entries
                                    Landers = read.Landers |> List.map fst
                                    Template = price.Template
                                    Blocks = price.Blocks
                                    Tile = price.Tile
                                }
                    }))

    // Ended for good, it asks for nothing more.
    let looks =
        match moved with
        | Some record when record.Ended = Some OpEnd.Expired -> []
        | _ ->
            reads
            |> List.filter (fun read -> read.Used = Truth.Yes)
            |> List.choose (fun read ->
                read.Price.Value
                |> Option.map (fun price -> read, price.Rate * float Facts.lookStride))
            |> List.filter (fun (_, value) -> value > 0.0)
            |> List.collect (fun (read, value) -> [ read.Exit.Camp, value; read.Exit.Home, value ])

    let exits = reads |> List.map readingOf

    moved,
    events,
    works,
    looks,
    {
        Id = spec.Id
        Exits = exits
        Idle = idleWhy now moved exits
    }

/// ADR-0096
/// The operations laid in every colony's view (#663 S7c), once a full tick
/// before the world's verdicts are priced (`Entry.judgeColonies`): the
/// sweep (`sweepSpec`) moved on from its record (`records`, the stored
/// latch: `advance` over its own prices, `ownFacts`, with its group,
/// `groupIn`, read off last tick's Tasks, `held`), and the room it works
/// laid for every colony (`WorldView.Operations`: its room, its group, its
/// rate, its most bodies) beside the room it is committed to
/// (`WorldView.Committed`). Every room's worth is laid first, one table for
/// every view (`Facts.layWorths`), priced while every candidate reads as
/// worked: the choice and every reader after it price a room once. A view
/// that casts candidates has its dismantler's targets narrowed to the room
/// worked (an enemy container in a candidate the operation does not work
/// is nobody's target this tick; a salvage room's stays). The records come
/// back moved on, with what moved; with no sweep cast this tick its record
/// stands unmoved (a caster's bank dipping keeps the latch) and nothing is
/// laid but the worths.
///
/// The exit camps code declares (#663 S7d) are moved on and laid first
/// (`layCamp`), over the same priced views: one fight a room, so a room an
/// exit camp works is no room of the sweep's this tick — no colony casts it
/// for the sweep, no body standing there is of its group, and its work
/// there is not laid while its record keeps the room. The looks they ask
/// are laid beside (`WorldView.OperationLooks`), and each camp's trigger
/// as read (#663 S7e, `OpReading`).
let layRead
    (declared: OpSpecWire list)
    (held: Assignments)
    (records: OpRecord list)
    (views: ColonyView list)
    : ColonyView list * OpRecord list * OpEvent list * OpReading list =
    let now =
        match views with
        | view :: _ -> view.Time
        | [] -> 0

    let laidAs (operations: OperationWork list) (committed: Map<string, int>) (view: ColonyView) =
        { view with
            World =
                { view.World with
                    Operations = Some operations
                    Committed = committed
                }
        }

    let camps = campSpecs declared

    match sweepSpec declared views, camps with
    | None, [] -> Facts.layWorths views |> List.map (laidAs [] Map.empty), records, [], []
    | sweep, camps ->
        let recordOf (spec: OperationSpec) =
            records
            |> List.tryFind (fun record -> record.Id = spec.Id)
            |> Option.bind (reopened now spec)

        let record = sweep |> Option.bind recordOf

        // The room it is committed to, whatever its phase, from the tick
        // that room was chosen: one map for every view.
        let commitment (record: OpRecord option) =
            match committedRoom now record, record with
            | Some room, Some record -> Map.ofList [ room, record.RoomSince ]
            | _ -> Map.empty

        let before = commitment record

        let priced =
            views
            |> List.map (fun view ->
                { view with
                    World =
                        { view.World with
                            Operations = None
                            Committed = before
                        }
                })
            |> Facts.layWorths

        // The exit camps first: a room one works is no room of the sweep's.
        let camped =
            camps
            |> List.map (fun spec -> spec, layCamp now held priced (recordOf spec) spec)

        let campWorks = camped |> List.collect (fun (_, (_, _, works, _, _)) -> works)
        let worked = campWorks |> List.map _.Room |> Set.ofList

        let looks =
            camped
            |> List.collect (fun (_, (_, _, _, looks, _)) -> looks)
            |> List.groupBy fst
            |> List.map (fun (room, values) -> room, List.sumBy snd values)
            |> Fresh.mapOfList

        let sweepRun =
            sweep
            |> Option.map (fun spec ->
                let listed = roomsOf spec.Target |> Option.defaultValue []

                let casters (room: string) =
                    if worked.Contains room then
                        []
                    else
                        priced
                        |> List.filter (fun view ->
                            view.Harass |> List.exists (fun h -> h.RoomName = room))

                let group =
                    groupIn
                        spec.Id
                        (listed |> List.filter (worked.Contains >> not) |> Set.ofList)
                        held
                        priced

                // The sweep reads no quarry guard: every room counts as warm.
                let facts = ownFacts now (fun _ -> true) spec record casters group priced
                let moved, events = advance facts spec record

                let work =
                    workedRoom now moved
                    |> Option.filter (worked.Contains >> not)
                    |> Option.map (fun room ->
                        {
                            Id = spec.Id
                            Enemy = spec.Enemy
                            Room = room
                            Group = group |> List.map (fun m -> m.Creep, m.At)
                            Rate = facts.Rate room |> Option.defaultValue 0.0
                            MaxBodies = spec.Limits.MaxBodies
                            Camp = None
                        })

                spec, moved, events, work)

        let committed =
            match sweepRun with
            | Some(_, moved, _, _) ->
                match commitment moved with
                | same when same = before -> before
                | other -> other
            | None -> Map.empty

        let work = sweepRun |> Option.bind (fun (_, _, _, work) -> work)
        let operations = campWorks @ Option.toList work
        let workRooms = operations |> List.map _.Room |> Set.ofList

        let laid =
            priced
            |> List.map (fun view ->
                let elsewhere =
                    view.Harass
                    |> List.map _.RoomName
                    |> List.filter (fun candidate -> not (workRooms.Contains candidate))
                    |> Set.ofList

                let view = laidAs operations committed view

                { view with
                    World =
                        if Map.isEmpty looks then
                            view.World
                        else
                            { view.World with
                                OperationLooks = looks
                            }
                    Dismantles =
                        if Set.isEmpty elsewhere then
                            view.Dismantles
                        else
                            view.Dismantles
                            |> List.filter (fun id ->
                                SpatialInfo.placementOf view.Spatial id
                                |> Option.forall (fun at -> not (elsewhere.Contains at.Room)))
                })

        let moved =
            (sweepRun
             |> Option.map (fun (spec, moved, events, _) -> spec, moved, events)
             |> Option.toList)
            @ (camped |> List.map (fun (spec, (moved, events, _, _, _)) -> spec, moved, events))

        let ids = moved |> List.map (fun (spec, _, _) -> spec.Id) |> Set.ofList

        let records =
            (records |> List.filter (fun record -> not (ids.Contains record.Id)))
            @ (moved |> List.collect (fun (_, record, _) -> Option.toList record))

        laid,
        records,
        moved |> List.collect (fun (_, _, events) -> events),
        camped |> List.map (fun (_, (_, _, _, _, reading)) -> reading)

/// `layRead` without the camps' readings.
let lay
    (declared: OpSpecWire list)
    (held: Assignments)
    (records: OpRecord list)
    (views: ColonyView list)
    : ColonyView list * OpRecord list * OpEvent list =
    let laid, records, events, _ = layRead declared held records views
    laid, records, events

/// `lay` over one colony's view alone, with no stored latch: the shape a
/// decide of one colony asks in.
let layOne (held: Assignments) (view: ColonyView) : ColonyView =
    let laid, _, _ = lay [] held [] [ view ]
    List.head laid


/// The code's operations admitted (#663 S7c): each spec code declares
/// (`Colony.operations`) through the Memory spec's safety
/// (`Operation.ofCode`) — its rooms less each one dropped, with a refusal
/// for each, or the whole spec refused. What the shell casts
/// (`Colony.candidatesOf`).
let declared (standing: OpStanding) (wires: OpSpecWire list) : OpSpecWire list * OpRefusal list =
    wires
    |> List.map (fun wire ->
        match Operation.ofCode standing wire with
        | Ok(spec, dropped) ->
            let rooms = roomsOf spec.Target |> Option.defaultValue []

            // A room whose holder nothing we keep knows is cast into by
            // nobody until a look or a memory names one: one never seen
            // since the reset, and an ally's reservation out of sight on a
            // cold heap (no dossier keeps it).
            let unknown =
                rooms |> List.filter (fun room -> standing.Holder room = RoomHolder.Unknown)

            let refusals =
                unknown
                |> List.map (fun room ->
                    {
                        Id = wire.Id
                        Why = "holder unknown: " + room
                    })

            let kept = rooms |> List.filter (fun room -> not (List.contains room unknown))

            // An anchored template (an exit camp, #663 S7d) keeps its anchor
            // first, its rooms about it; an anchor whose holder nothing
            // names casts nothing.
            match spec.Target with
            | OpTarget.Exits(exit :: _) when Operation.Templates.anchored wire.Template ->
                if standing.Holder exit.Home = RoomHolder.Unknown then
                    None,
                    dropped
                    @ [
                        {
                            Id = wire.Id
                            Why = "holder unknown: " + exit.Home
                        }
                    ]
                else
                    Some { wire with Rooms = exit.Home :: kept }, dropped @ refusals
            | _ -> Some { wire with Rooms = kept }, dropped @ refusals
        | Error refused -> None, [ refused ])
    |> fun answers -> List.choose fst answers, List.collect snd answers

/// The code's latch as the next tick keeps it (#663 S7e): each record of a
/// code spec, of a live Memory spec laid this tick, or of one flagged live
/// whose record stands unmoved (`OpPublication.LiveKept`); any other — a
/// live spec gone from Memory, turned dry or unsafe — dropped, with its
/// event, as the dry run drops one (`step`).
let retire
    (code: OpSpecWire list)
    (publication: OpPublication)
    (records: OpRecord list)
    : OpRecord list * OpEvent list =
    let kept =
        (code @ publication.Live)
        |> List.map _.Id
        |> Set.ofList
        |> Set.union publication.LiveKept

    records |> List.filter (fun record -> kept.Contains record.Id),
    records
    |> List.filter (fun record -> not (kept.Contains record.Id))
    |> List.map (fun record -> OpEvent.Dropped record.Id)
