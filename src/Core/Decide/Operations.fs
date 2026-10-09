/// The [[operation]] interpreter (#663 M7): once a full tick, over the
/// world, each spec's record moved on by its schedule, its target and its
/// exits. The code's sweep is laid for every colony (`lay`, #663 S7c): the
/// room its record is committed to is fought under its Task (`Operate`),
/// a Brief per operation. A Memory spec still runs dry (`step`): its records
/// and its events, and nothing read back into a decision.
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

/// A reading of the room a record is committed to; Unknown with none.
let private committed (record: OpRecord) (read: string -> Truth) : Truth =
    record.Room |> Option.map read |> Option.defaultValue Truth.Unknown

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
    | OpGuard.ExchangeLoses -> committed record facts.Loses
    | OpGuard.Assembled -> committed record facts.Assembled
    | OpGuard.Arrived -> committed record facts.Arrived
    | OpGuard.HomeThreatened -> facts.HomeThreatened
    | OpGuard.Outpaid ->
        match roomsOf spec.Target with
        | None -> Truth.Unknown
        | Some rooms ->
            let beats (floor: float) (rates: float option list) =
                if rates |> List.exists (Option.exists (fun rate -> rate > floor)) then
                    Truth.Yes
                elif rates |> List.exists Option.isNone then
                    Truth.Unknown
                else
                    Truth.No

            match record.Room with
            | None -> rooms |> List.map facts.Rate |> beats 0.0
            | Some room ->
                match facts.Rate room with
                | None -> Truth.Unknown
                | Some held ->
                    rooms |> List.filter ((<>) room) |> List.map facts.Challenge |> beats held
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
/// tick. Ended is absorbing. A record is begun by its schedule (`Once` and
/// `Every` at once, `When` on its guard's first `Yes`) and moves no further
/// that tick. Then its deadline, hard: on it the operation expires, whatever
/// its phase. Then its budget (spent past `MaxEnergy` ends it), then one
/// exit: an `Every` run past its window recurs, else its phase's first exit
/// taken. Each phase it enters, its room is chosen and latched there until
/// the next: the room it was committed to, its commitment kept, while
/// nothing outpays it and it is not exhausted; else the target's best
/// (`target`).
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
            | OpSchedule.Every(period, _) -> now + period
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
            },
        [ OpEvent.Began(spec.Id, phase, room.Value) ]

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
                    Room = None
                    RoomSince = now
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
                        Room = None
                        RoomSince = next
                        NextDue = next + period
                    },
                [ OpEvent.Recurred(spec.Id, next) ]
            | _ -> ended record OpEnd.Done

        let windowed =
            match spec.Schedule with
            | OpSchedule.Every(_, window) -> now >= record.Started + window
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
            let exits =
                spec.Phases
                |> List.tryFind (fun p -> p.Phase = record.Phase)
                |> Option.map _.Exits
                |> Option.defaultValue []

            // The room it enters the phase with: the one it is committed to,
            // its commitment kept through every phase, a hold among them,
            // unless another room outpays it, or it is exhausted and another
            // room pays; else the target's best, committed from now.
            let entered () =
                match record.Room with
                | Some _ when truth facts spec record OpGuard.Outpaid = Truth.Yes -> room.Value, now
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
                let into', since = entered ()

                Some
                    { record with
                        Phase = into
                        Since = now
                        Room = into'
                        RoomSince = since
                    },
                [ OpEvent.Moved(spec.Id, record.Phase, into, index, into') ]
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
        HomeThreatened =
            match views with
            | [] -> Truth.No
            | views when List.forall Facts.homeThreatened views -> Truth.Yes
            | _ -> Truth.No
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
/// work. The group's arrival, muster and exchange are read in the room the
/// guards ask of (`OpFacts`); its home is threatened where every colony
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
    // to outpay the committed one.
    let challenge (room: string) =
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
        Challenge = fun room -> Some(paying room (fun () -> challenge room))
        HomeThreatened =
            match casting with
            | [] -> Truth.No
            | casting -> truthOf (casting |> List.forall Facts.homeThreatened)
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
    let candidates =
        views |> List.collect _.Harass |> List.distinctBy (fun h -> h.RoomName)

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
let lay
    (declared: OpSpecWire list)
    (held: Assignments)
    (records: OpRecord list)
    (views: ColonyView list)
    : ColonyView list * OpRecord list * OpEvent list =
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

    match sweepSpec declared views with
    | None -> Facts.layWorths views |> List.map (laidAs [] Map.empty), records, []
    | Some spec ->
        let record =
            records
            |> List.tryFind (fun record -> record.Id = spec.Id)
            |> Option.bind (reopened now spec)

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

        let listed = roomsOf spec.Target |> Option.defaultValue []

        let casters (room: string) =
            priced
            |> List.filter (fun view -> view.Harass |> List.exists (fun h -> h.RoomName = room))

        let group = groupIn spec.Id (Set.ofList listed) held priced
        // The sweep reads no quarry guard: every room counts as warm.
        let facts = ownFacts now (fun _ -> true) spec record casters group priced
        let moved, events = advance facts spec record

        let work =
            workedRoom now moved
            |> Option.map (fun room ->
                {
                    Id = spec.Id
                    Enemy = spec.Enemy
                    Room = room
                    Group = group |> List.map (fun m -> m.Creep, m.At)
                    Rate = facts.Rate room |> Option.defaultValue 0.0
                    MaxBodies = spec.Limits.MaxBodies
                })

        let committed =
            match commitment moved with
            | same when same = before -> before
            | other -> other

        let laid =
            priced
            |> List.map (fun view ->
                let elsewhere =
                    view.Harass
                    |> List.map _.RoomName
                    |> List.filter (fun candidate -> Some candidate <> (work |> Option.map _.Room))
                    |> Set.ofList

                { laidAs (Option.toList work) committed view with
                    Dismantles =
                        if Set.isEmpty elsewhere then
                            view.Dismantles
                        else
                            view.Dismantles
                            |> List.filter (fun id ->
                                SpatialInfo.placementOf view.Spatial id
                                |> Option.forall (fun at -> not (elsewhere.Contains at.Room)))
                })

        let records =
            (records |> List.filter (fun record -> record.Id <> spec.Id))
            @ Option.toList moved

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

            Some
                { wire with
                    Rooms = rooms |> List.filter (fun room -> not (List.contains room unknown))
                },
            dropped
            @ (unknown
               |> List.map (fun room ->
                   {
                       Id = wire.Id
                       Why = "holder unknown: " + room
                   }))
        | Error refused -> None, [ refused ])
    |> fun answers -> List.choose fst answers, List.collect snd answers
