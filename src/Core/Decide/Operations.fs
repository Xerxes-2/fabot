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
