/// The [[operation ledger]] (#662, #663 M6): one row per offensive or
/// contested operation — a [[sweep]] room spell, a camp, a trespass kill, a
/// defended outpost raid — with what it cost us against what it cost the
/// rival, beside the price the decision read at its open. Observation only:
/// nothing decides on it. Read off what the tick's decide already priced
/// (`observe`), folded on full ticks (`fold`), stored in its memory segment.
[<RequireQualifiedAccess>]
module Fabot.Core.Decide.Ledger

open Fabot.Core
open Fabot.Core.Types

/// Which job or spell an operation is.
[<RequireQualifiedAccess>]
type OperationKind =
    /// The [[sweep]]'s room spell: the ticks `Facts.sweepChoice` works one
    /// room. A camp in its room is a job of it, recorded on its row.
    | Sweep
    /// A camp on a border dancer (`JobKind.Camp`) outside the sweep's room.
    | Camp
    /// A [[contested room]] fought over its prey outside the sweep's room: a
    /// trespasser's kill.
    | Trespass
    /// A contested room fought over its raid outside a home of ours and the
    /// sweep's room: a defended outpost raid, a contested hold.
    | Hold

/// An operation's identity: its kind, its room and the first full tick it
/// was seen on. S7's Operation takes this key over.
type OperationKey =
    {
        Kind: OperationKind
        Room: string
        Start: int
    }

/// The price the decision read as an operation opened, each only where the
/// decide priced it that tick — the ledger prices nothing itself: the
/// sweep's rate for its room (`WorldView.SweepRate`), its first job's value
/// a tick (`Job.Value`), and a camp's odds (`Job.Odds`).
type Prediction =
    {
        Rate: float option
        Value: float option
        Odds: float option
    }

/// A body of ours holding an operation's Task this tick: its cost, its
/// spawn's ticks, its life left and the room it stands in.
type Holder =
    {
        Name: string
        Cost: int
        SpawnTicks: int
        Life: int
        Room: string
    }

/// One operation as this full tick's decide priced it: its kind, its room,
/// its price, read once, at its open, and the bodies a camp in it is laid
/// on (a sweep's camp, a job of it).
[<NoEquality; NoComparison>]
type Seen =
    {
        Kind: OperationKind
        Room: string
        Predicted: Lazy<Prediction>
        Camp: string list
    }

/// An armed body of the room's rival standing there: the tick its spawn
/// began (off its life left and its parts) and its body's cost.
type Armed = { Began: int; Cost: int }

/// What the ledger reads of one room this tick, off its worth.
type Reading =
    {
        /// Each body priced there by id, at its kill value now (#621, by
        /// its remaining life): the room's worth's (`RoomWorth.Values`) and
        /// each armed body's standing there.
        Values: Map<string, float>
        /// The room's rival: the enemy a sweep candidate names, else the
        /// player reserving it, else the player whose bodies are priced
        /// there; none for an NPC's alone.
        Rival: string option
        /// Each armed body of the rival's standing there, by id: the
        /// defenders an answer casts.
        Armed: Map<string, Armed>
        /// The income a tick the room yields its rival while worked, where
        /// it is the rival's remote (a sweep candidate naming it, or
        /// reserved by it): its sources at their held output; else nothing.
        Yield: float
        /// Whether an economy body of the rival works it in sight this tick
        /// (a take of its worth's on a body in sight).
        Worked: bool
        /// Whether vision answers for the room this tick: the evidence a
        /// span of downtime is read between.
        InSight: bool
    }

/// What `fold` reads of one whole full tick's decide: every operation seen;
/// the reading of each room an operation is seen in or a body of ours holds
/// the Guard of, and those bodies by room.
[<NoEquality; NoComparison>]
type Observed =
    {
        Seen: Seen list
        /// Each read on first use: only a room a row is open in is; none
        /// where the decide priced no worth for it this tick.
        Readings: Map<string, Lazy<Reading option>>
        Holders: Map<string, Holder list>
    }

/// What forced a full tick out of a light one: the rooms and the creeps
/// its reasons name, and the CPU that tick had spent by the ledger's turn.
type Forcing =
    {
        Rooms: Set<string>
        Creeps: Set<string>
        Cpu: float
    }

/// One whole full tick, every colony's fight decided, as the fold reads it.
[<NoEquality; NoComparison>]
type Tick =
    {
        Time: int
        /// The bounce window (`Tuning.RaidBounceTicks`), as a body out of
        /// sight that long is one trip: a row seen again within it of its
        /// close resumes, and a row neither seen nor held for longer closes,
        /// since no sighting after it could resume it either.
        Window: int
        /// Every creep of ours alive this tick, by name.
        Living: Set<string>
        Quarries: Map<string, Track>
        /// The bodies known fallen on the forced ticks since the last fold,
        /// which folds none (`Gone.Fallen` may be read on one tick alone).
        Fallen: Set<string>
        /// What forced this tick, and each forced tick since the last fold.
        Forcing: Forcing list
        Observed: Observed
    }

/// A body on an operation's roster: its cost, its life left and the tick
/// that was read, whether it holds the operation's Task, and whether it was
/// cast for it (its spawn began at or after the operation's start).
type Enlisted =
    {
        Cost: int
        Life: int
        Seen: int
        Holds: bool
        Cast: bool
    }

/// One operation's row.
type Row =
    {
        Key: OperationKey
        /// The last full tick it was folded on.
        Last: int
        /// The last full tick it was seen or held.
        Active: int
        /// The last full tick vision answered for its room.
        Looked: int
        /// The full tick it closed on; None while open.
        Ended: int option
        Predicted: Prediction
        /// Our spend: the bodies cast for it, their energy and spawn ticks;
        /// the bodies lent it that were cast before it; the bodies lost
        /// holding it, cast and lent apart, their cost and the share of it
        /// their life had left; the full ticks it forced and those ticks'
        /// CPU by the fold (the whole tick's, not the forcing's margin).
        Cast: int
        CastEnergy: int
        SpawnTicks: int
        Joined: int
        CastLost: int
        CastLostCost: int
        CastLostLife: float
        LentLost: int
        LentLostCost: int
        LentLostLife: float
        Forced: int
        ForcedCpu: float
        /// The rival's loss: its bodies that fell there (`Gone.Fallen`) and
        /// their kill value, of them those a camp in the sweep's room was laid
        /// on; the ticks its remote stood unworked while we held it and that
        /// income; the armed bodies it cast after the open and sent there,
        /// the cost of those not killed (a killed one is a kill), and the
        /// ticks to the first.
        Kills: int
        KillValue: float
        CampKills: int
        CampValue: float
        Downtime: int
        DowntimeValue: float
        Answered: int
        AnsweredCost: int
        Response: int option
        /// The fold's own state: the roster by name, each priced body's
        /// latest kill value by id and the bodies a camp was laid on (both
        /// emptied as the row closes), and the answers by id at their spawn
        /// and cost —
        /// past the cap the earliest cast go, and the latest spawn tick of
        /// those gone is the floor at or under which no body is a new answer;
        /// all emptied once a closed row can no longer resume.
        Roster: Map<string, Enlisted>
        /// The latest birth tick of a holder the roster dropped past its cap
        /// (the bodies that left go first): a holder born at or before it,
        /// not on the roster, is never enlisted again.
        RosterFloor: int option
        Marks: Map<string, float>
        Camped: Set<string>
        Answers: Map<string, Armed>
        AnswerFloor: int option
    }

/// The rows open, oldest first, the latest closed, newest first, and the
/// open rows evicted to closed to make room for a new one.
type Ledger =
    {
        Open: Row list
        Closed: Row list
        Evicted: int
    }

/// The most rows open at once and closed kept, the bodies a roster keeps,
/// and the values, camped bodies and answers a row keeps: together they keep the ledger inside its memory segment
/// (`ObserveMemory.ledgerSegments`; a wire-check case writes it full).
let capOpen = 12
let capClosed = 30
let capRoster = 12
let capMarks = 12

/// A ledger with no row.
let empty: Ledger = { Open = []; Closed = []; Evicted = 0 }

// ---- the fold ---------------------------------------------------------------

/// A row's roster on this tick's holders: each body that held its Task at
/// the last fold and is gone before its life ran out is lost, at its cost
/// and the share of it its life had left, cast and lent apart (a cast body's
/// energy is already its spend); every other body gone is dropped;
/// each holder new to it enlists, cast for it where its spawn began at or
/// after the start, else lent; a body that left keeps its place, so a
/// return is not counted twice.
let private muster (now: int) (living: Set<string>) (holders: Holder list) (row: Row) : Row =
    let lost =
        row.Roster
        |> Map.filter (fun name body ->
            body.Holds && not (living.Contains name) && body.Life > now - body.Seen)

    let kept = row.Roster |> Map.filter (fun name _ -> living.Contains name)
    let holding = holders |> List.map _.Name |> Set.ofList
    let born (life: int) (seen: int) = seen - (Engine.creepLifetime - life)

    // A holder the roster dropped past its cap was born at or before its
    // floor: it is not enlisted again.
    let admitted =
        holders
        |> List.filter (fun holder ->
            kept.ContainsKey holder.Name
            || row.RosterFloor |> Option.forall (fun floor -> born holder.Life now > floor))

    let entered =
        admitted |> List.filter (fun holder -> not (kept.ContainsKey holder.Name))

    let cast (holder: Holder) =
        now - (Engine.creepLifetime - holder.Life) - holder.SpawnTicks >= row.Key.Start

    let newcomers, lent = entered |> List.partition cast

    let roster =
        admitted
        |> List.fold
            (fun (roster: Map<string, Enlisted>) holder ->
                let was = Map.tryFind holder.Name roster

                roster.Add(
                    holder.Name,
                    {
                        Cost = holder.Cost
                        Life = holder.Life
                        Seen = now
                        Holds = true
                        Cast =
                            was |> Option.map _.Cast |> Option.defaultWith (fun () -> cast holder)
                    }
                ))
            (kept
             |> Map.map (fun name body ->
                 { body with
                     Holds = holding.Contains name
                 }))

    // Past the cap, the bodies that left longest ago go first; then, with
    // more holders than the cap, the earliest born of them, the floor rising
    // to the latest of those, so none of them enlists again.
    let dropped =
        let over = roster.Count - capRoster

        if over <= 0 then
            []
        else
            roster
            |> Map.toList
            |> List.sortBy (fun (name, body) ->
                body.Holds, (if body.Holds then born body.Life body.Seen else body.Seen), name)
            |> List.truncate over

    let roster =
        dropped
        |> List.fold (fun (roster: Map<string, Enlisted>) (name, _) -> roster.Remove name) roster

    let floor =
        dropped
        |> List.filter (fun (_, body) -> body.Holds)
        |> List.fold
            (fun floor (_, body) ->
                Some(
                    max
                        (born body.Life body.Seen)
                        (Option.defaultValue System.Int32.MinValue floor)
                ))
            row.RosterFloor

    let castLost, lentLost = lost |> Map.partition (fun _ body -> body.Cast)

    let sum (bodies: Map<string, Enlisted>) (value: Enlisted -> float) =
        bodies |> Map.fold (fun total _ body -> total + value body) 0.0

    let cost (body: Enlisted) = float body.Cost

    let lifeLeft (body: Enlisted) =
        float body.Cost * float (body.Life - (now - body.Seen))
        / float Engine.creepLifetime

    { row with
        Roster = roster
        RosterFloor = floor
        Cast = row.Cast + newcomers.Length
        CastEnergy = row.CastEnergy + (newcomers |> List.sumBy _.Cost)
        SpawnTicks = row.SpawnTicks + (newcomers |> List.sumBy _.SpawnTicks)
        Joined = row.Joined + lent.Length
        CastLost = row.CastLost + castLost.Count
        CastLostCost = row.CastLostCost + int (sum castLost cost)
        CastLostLife = row.CastLostLife + sum castLost lifeLeft
        LentLost = row.LentLost + lentLost.Count
        LentLostCost = row.LentLostCost + int (sum lentLost cost)
        LentLostLife = row.LentLostLife + sum lentLost lifeLeft
    }

/// A row's kills: each body it priced whose track fell (`Gone.Fallen`),
/// at the kill value last priced, apart those a camp was laid on; an answer
/// killed leaves the answers' cost, being a kill; a body known gone
/// otherwise, or no longer remembered, unmarked; then this tick's values
/// marked, the most valued kept past the cap, and the camp's bodies.
let private killsOn
    (quarries: Map<string, Track>)
    (fallen: Set<string>)
    (reading: Reading option)
    (seen: Seen option)
    (row: Row)
    : Row =
    let goneOf id =
        if fallen.Contains id then
            Some Gone.Fallen
        else
            Map.tryFind id quarries
            |> Option.map _.Gone
            |> Option.defaultValue (Some Gone.Aged)

    let fell = row.Marks |> Map.filter (fun id _ -> goneOf id = Some Gone.Fallen)
    let camped = fell |> Map.filter (fun id _ -> row.Camped.Contains id)
    let standing = row.Marks |> Map.filter (fun id _ -> (goneOf id).IsNone)

    let total (bodies: Map<string, float>) =
        bodies |> Map.fold (fun sum _ value -> sum + value) 0.0

    let answerersKilled =
        fell
        |> Map.fold
            (fun cost id _ ->
                match Map.tryFind id row.Answers with
                | Some answer -> cost + answer.Cost
                | None -> cost)
            0

    let marks =
        match reading with
        | None -> standing
        | Some reading ->
            reading.Values
            |> Map.fold (fun marks id value -> Map.add id value marks) standing

    let marks =
        if marks.Count <= capMarks then
            marks
        else
            marks
            |> Map.toSeq
            |> Seq.sortBy (fun (_, value) -> -value)
            |> Seq.truncate capMarks
            |> Fresh.mapOfSeq

    let camp = seen |> Option.map _.Camp |> Option.defaultValue []

    { row with
        Marks = marks
        Kills = row.Kills + fell.Count
        KillValue = row.KillValue + total fell
        CampKills = row.CampKills + camped.Count
        CampValue = row.CampValue + total camped
        AnsweredCost = row.AnsweredCost - answerersKilled
        Camped =
            camp
            |> List.filter (row.Camped.Contains >> not)
            |> List.truncate (capMarks - row.Camped.Count)
            |> List.fold (fun set id -> Set.add id set) row.Camped
    }

/// A row's rival side off its room's reading. Its downtime: where the room
/// is the rival's remote, in sight now, and a holder of ours stands in it
/// unworked, the ticks since the last look at it (the evidence spans no
/// further) at the remote's yield. Its answers: each armed body of the
/// rival's whose spawn began at or after the row's start (a body standing
/// at the open, or cast before it, never is) and above the floor, not
/// counted yet; past the cap the earliest cast are forgotten and the floor
/// rises to the latest of them, so no forgotten body is counted again.
let private rivalOn (now: int) (holders: Holder list) (reading: Reading option) (row: Row) : Row =
    match reading with
    | None -> row
    | Some reading ->
        let unworked =
            reading.InSight
            && reading.Yield > 0.0
            && not reading.Worked
            && holders |> List.exists (fun holder -> holder.Room = row.Key.Room)

        let span = if unworked then now - row.Looked else 0

        let fresh =
            reading.Armed
            |> Map.filter (fun id body ->
                body.Began >= row.Key.Start
                && row.AnswerFloor |> Option.forall (fun floor -> body.Began > floor)
                && not (row.Answers.ContainsKey id))

        let answers =
            fresh |> Map.fold (fun answers id body -> Map.add id body answers) row.Answers

        let over = answers.Count - capMarks

        let forgotten =
            if over <= 0 then
                []
            else
                answers
                |> Map.toList
                |> List.sortBy (fun (id, body) -> body.Began, id)
                |> List.truncate over

        { row with
            Looked = if reading.InSight then now else row.Looked
            Downtime = row.Downtime + span
            DowntimeValue = row.DowntimeValue + float span * reading.Yield
            Answered = row.Answered + fresh.Count
            AnsweredCost =
                row.AnsweredCost + (fresh |> Map.fold (fun total _ body -> total + body.Cost) 0)
            Response =
                row.Response
                |> Option.orElse (if fresh.IsEmpty then None else Some(now - row.Key.Start))
            Answers = forgotten |> List.fold (fun answers (id, _) -> Map.remove id answers) answers
            AnswerFloor =
                forgotten
                |> List.fold
                    (fun floor (_, body) ->
                        Some(max body.Began (Option.defaultValue System.Int32.MinValue floor)))
                    row.AnswerFloor
        }

/// Whether a forced tick's reasons name a row: its room, or a body holding it.
let private forcedBy (forcing: Forcing) (row: Row) : bool =
    forcing.Rooms.Contains row.Key.Room
    || row.Roster
       |> Map.exists (fun name body -> body.Holds && forcing.Creeps.Contains name)

/// The operation a kind is one of: a contested room's prey and its raid are
/// one fight over the room whichever phase it is in this tick (#656), its
/// kind the phase it opened in.
let private family (kind: OperationKind) : OperationKind =
    match kind with
    | OperationKind.Trespass -> OperationKind.Hold
    | kind -> kind

/// A row with none of the fold's own state.
let private stripped (row: Row) : Row =
    { row with
        Roster = Fresh.mapOfSeq Seq.empty
        Marks = Fresh.mapOfSeq Seq.empty
        Camped = Fresh.setOfSeq Seq.empty
        Answers = Fresh.mapOfSeq Seq.empty
    }

/// A row opened on its first sighting, its price read now.
let private opened (now: int) (seen: Seen) : Row =
    {
        Key =
            {
                Kind = seen.Kind
                Room = seen.Room
                Start = now
            }
        Last = now
        Active = now
        Looked = now
        Ended = None
        Predicted = seen.Predicted.Force()
        Cast = 0
        CastEnergy = 0
        SpawnTicks = 0
        Joined = 0
        CastLost = 0
        CastLostCost = 0
        CastLostLife = 0.0
        LentLost = 0
        LentLostCost = 0
        LentLostLife = 0.0
        Forced = 0
        ForcedCpu = 0.0
        Kills = 0
        KillValue = 0.0
        CampKills = 0
        CampValue = 0.0
        Downtime = 0
        DowntimeValue = 0.0
        Answered = 0
        AnsweredCost = 0
        Response = None
        Roster = Fresh.mapOfSeq Seq.empty
        RosterFloor = None
        Marks = Fresh.mapOfSeq Seq.empty
        Camped = Fresh.setOfSeq Seq.empty
        Answers = Fresh.mapOfSeq Seq.empty
        AnswerFloor = None
    }

/// The ledger after one whole full tick (#662), every colony's fight
/// decided — a forced tick some colony replayed folds nothing, its forcing
/// and its fallen bodies carried to this one —, and each row whose fold
/// threw by its key and message. Each open row folds: its roster
/// (`muster`), its kills (`killsOn`), its rival side (`rivalOn`), and a
/// share of each forced tick's CPU whose reasons name it. A row neither
/// seen nor held closes — the group left, or the fight ended: every colony
/// decided, so an operation unseen is gone. A contested room's fight is one row whichever phase it
/// is seen in (`family`). Each operation seen with no row open resumes the
/// one of its kind and room closed within the window, else opens one, its
/// price read then; past the cap, the open row seen or held longest ago is
/// evicted to the closed rows, and counted. A closed row past the window
/// keeps none of the fold's state. A row whose fold throws stands as it
/// was, and one whose price throws does not open.
let fold (tick: Tick) (ledger: Ledger) : Ledger * (OperationKey * string) list =
    let now = tick.Time
    let observed = tick.Observed

    let holdersIn room =
        Map.tryFind room observed.Holders |> Option.defaultValue []

    let readingOf room =
        Map.tryFind room observed.Readings |> Option.bind _.Value

    let fights (seen: Seen) (row: Row) =
        family row.Key.Kind = family seen.Kind && row.Key.Room = seen.Room

    let seenAs (row: Row) =
        observed.Seen |> List.tryFind (fun seen -> fights seen row)

    let isOpen (seen: Seen) =
        ledger.Open |> List.exists (fights seen)

    let resumable (seen: Seen) (row: Row) =
        fights seen row
        && row.Ended |> Option.exists (fun ended -> now - ended <= tick.Window)

    // Each operation seen with no row open: the row it resumes, else a new
    // one, while the open rows are under their cap.
    let entering = observed.Seen |> List.filter (isOpen >> not)

    let resuming =
        entering
        |> List.map (fun seen -> seen, ledger.Closed |> List.tryFind (resumable seen))

    let resumed =
        resuming
        |> List.choose snd
        |> List.map (fun row ->
            { row with
                Ended = None
                Last = now
                Active = now
            })

    let faults = ResizeArray<OperationKey * string>()

    let fresh =
        resuming
        |> List.filter (snd >> Option.isNone)
        |> List.choose (fun (seen, _) ->
            try
                Some(opened now seen)
            with error ->
                faults.Add(
                    {
                        Kind = seen.Kind
                        Room = seen.Room
                        Start = now
                    },
                    error.Message
                )

                None)

    // Past the cap, the rows seen or held longest ago make room for the
    // operations entering.
    let entered = resumed @ fresh
    let over = ledger.Open.Length + entered.Length - capOpen

    let evicted =
        if over <= 0 then
            []
        else
            ledger.Open
            |> List.sortBy (fun row -> row.Active, row.Key.Start)
            |> List.truncate over

    let evictedKeys = evicted |> List.map _.Key |> Set.ofList

    let rows =
        (ledger.Open |> List.filter (fun row -> not (evictedKeys.Contains row.Key)))
        @ entered
        |> List.truncate capOpen

    // Each forcing's CPU, shared by the rows it names.
    let shares =
        tick.Forcing
        |> List.map (fun forcing -> forcing, rows |> List.filter (forcedBy forcing) |> List.length)

    let folded =
        rows
        |> List.map (fun row ->
            let seen = seenAs row
            let holders = holdersIn row.Key.Room

            let forced = shares |> List.filter (fun (forcing, _) -> forcedBy forcing row)

            // Every colony decided this tick: unseen and unheld, the group
            // left or the fight ended.
            let active = seen.IsSome || not holders.IsEmpty
            let lastActive = if active then now else row.Active

            try
                let row =
                    row
                    |> rivalOn now holders (readingOf row.Key.Room)
                    |> killsOn tick.Quarries tick.Fallen (readingOf row.Key.Room) seen
                    |> muster now tick.Living holders

                { row with
                    Last = now
                    Active = lastActive
                    Forced = row.Forced + forced.Length
                    ForcedCpu =
                        row.ForcedCpu
                        + (forced |> List.sumBy (fun (forcing, share) -> forcing.Cpu / float share))
                },
                active
            with error ->
                faults.Add(row.Key, error.Message)
                { row with Active = lastActive }, active)

    // Its marks and camped bodies go: a resumed row reads its room's values
    // and its camp afresh.
    let close (row: Row) =
        { row with
            Ended = Some now
            Marks = Fresh.mapOfSeq Seq.empty
            Camped = Fresh.setOfSeq Seq.empty
        }

    let closing =
        (folded |> List.filter (snd >> not) |> List.map fst) @ evicted |> List.map close

    // The closed rows that resumed within the cap leave the closed list.
    let resumedKeys =
        resumed
        |> List.map _.Key
        |> List.filter (fun key -> rows |> List.exists (fun row -> row.Key = key))
        |> Set.ofList

    {
        Open = folded |> List.filter snd |> List.map fst
        Closed =
            (List.rev closing
             @ (ledger.Closed |> List.filter (fun row -> not (resumedKeys.Contains row.Key))))
            |> List.truncate capClosed
            |> List.map (fun row ->
                if row.Ended |> Option.exists (fun ended -> now - ended > tick.Window) then
                    stripped row
                else
                    row)
        Evicted = ledger.Evicted + evicted.Length
    },
    List.ofSeq faults

// ---- what the decide priced -------------------------------------------------

/// The prefix of every Guard's Task id (`taskId`).
let private guardPrefix = taskId (Guard "")

/// What a body of ours costs and spawns in, off its parts.
let private holderOf (creep: CreepInfo) (room: string) : Holder =
    let parts =
        creep.Body
        |> Map.toList
        |> List.collect (fun (part, n) -> List.replicate n part)

    {
        Name = creep.Name
        Cost = bodyCost parts
        SpawnTicks = Engine.spawnTicksPerPart * List.length parts
        Life = creep.TicksToLive
        Room = room
    }

/// The tick a hostile's spawn began, off its life left (a CLAIM body's
/// lifetime the shorter) and its parts: a spawning body, which reads no
/// life, began its spawn its parts' ticks ago.
let private spawnBegan (now: int) (hostile: HostileInfo) : int =
    let lifetime =
        if List.contains BodyPart.Claim hostile.Body then
            Engine.claimLifetime
        else
            Engine.creepLifetime

    let lived =
        if hostile.TicksToLive >= 1 then
            lifetime - min lifetime hostile.TicksToLive
        else
            0

    now - lived - Engine.spawnTicksPerPart * List.length hostile.Body

/// `readingIn` off the worth itself.
let private readingOf (view: ColonyView) (room: string) (worth: RoomWorth) : Reading =
    let values =
        worth.Escort
        |> List.fold
            (fun values h ->
                if Map.containsKey h.Id values then
                    values
                else
                    Map.add h.Id 0.0 values)
            worth.Values

    let candidate =
        view.Harass |> List.tryFind (fun h -> h.RoomName = room) |> Option.map _.Enemy

    let reserver =
        Map.tryFind room view.World.Rooms
        |> Option.bind (fun seen -> seen.Control.Reservation)
        |> Option.filter (fun reservation ->
            reservation.Holder <> ReservationHolder.Ours
            && reservation.Holder <> ReservationHolder.Invader
            && not (Colony.isAlly reservation.Username))
        |> Option.map _.Username

    let player (h: HostileInfo) =
        HostileInfo.crossesExits h && not (Colony.isAlly h.Owner)

    // The owner of the most valued player's body priced there.
    let rival =
        candidate
        |> Option.orElse reserver
        |> Option.orElse (
            view.Hostiles
            |> List.filter (fun h -> h.Pos.Room = room && player h && values.ContainsKey h.Id)
            |> List.fold
                (fun best h ->
                    match best with
                    | Some(b: HostileInfo) when (values.[b.Id], b.Id) >= (values.[h.Id], h.Id) ->
                        best
                    | _ -> Some h)
                None
            |> Option.map _.Owner
        )

    let remote = rival.IsSome && (candidate.IsSome || reserver = rival)

    // Its sources off the projection's targets: a count, never a price.
    let sources =
        (SpatialInfo.layerOf view.Spatial room).TargetPositions
        |> Map.filter (fun id _ -> Map.tryFind id view.Spatial.TargetKinds = Some Source)
        |> Map.count

    {
        Values = values
        Rival = rival
        Armed =
            worth.Escort
            |> List.filter (fun h -> player h && Some h.Owner = rival)
            |> List.map (fun h ->
                h.Id,
                {
                    Began = spawnBegan view.Time h
                    Cost = bodyCost h.Body
                })
            |> Map.ofList
        Yield =
            if remote then
                float (sources * Engine.heldOutputPerTick)
            else
                0.0
        Worked =
            worth.Takes
            |> Map.exists (fun id take -> take > 0.0 && not (worth.Dark.Contains id))
        InSight = Map.containsKey room view.World.Rooms
    }

/// What the ledger reads of a room off the worth the decide priced it at
/// this tick (#662, `readingOf`), none where it priced none: the ledger
/// prices nothing itself (`WorldView.Worths`). Each body the worth values
/// at that value, and each armed body there it does not value at none (its
/// fall a kill, unpriced); its rival — the enemy a candidate names, else the
/// player reserving it, else the player owning the most valued body priced
/// there — and that rival's armed bodies, at their spawn and cost; and,
/// where it is the rival's remote (a candidate naming it, or reserved by
/// it), its sources' held yield, whether a body in sight takes from it, and
/// whether vision answers for it this tick.
let private readingIn (view: ColonyView) (room: string) : Reading option =
    view.World.Worths
    |> Option.bind (Map.tryFind room)
    |> Option.filter _.IsValueCreated
    |> Option.map (fun worth -> readingOf view room worth.Value)

/// Every operation this whole full tick's decide priced (#662), off the
/// judged views (`views`), the world's Brief of each combat Task with its
/// owner's view (`briefs`) and the Tasks this tick's match handed the
/// fighters (`assigned`): the [[sweep]]'s room while it works one, at the
/// rate its choice priced it at (`WorldView.SweepRate`), a camp in it a job
/// of it (its odds and the bodies it is laid on); and outside the sweep's
/// room, of each [[contested room]]'s Guard, a camp where its jobs lay one,
/// else its prey phase as a trespass and its raid phase outside a home as a
/// hold. Each at its first job's value and a camp's odds where the decide
/// read them (`Job.Value`, `Job.Odds`): the ledger prices nothing itself.
/// And the bodies holding each room's Guard by room.
let observe
    (views: ColonyView list)
    (briefs: (Task * Brief * ColonyView) list)
    (assigned: Assignments)
    : Observed =
    // Each Guard's holder by name, and where the world's census of our
    // bodies stands it: read only where a Guard is held.
    let guards =
        assigned
        |> Map.filter (fun _ tid -> tid.StartsWith guardPrefix)
        |> Map.map (fun _ tid -> tid.Substring guardPrefix.Length)

    let placed =
        match views with
        | view :: _ when not guards.IsEmpty ->
            view.World.Bodies.Value
            |> Map.toSeq
            |> Seq.collect (fun (room, bodies) ->
                bodies
                |> Seq.filter (fun (creep, _) -> guards.ContainsKey creep.Name)
                |> Seq.map (fun (creep, _) -> creep.Name, (creep, room)))
            |> Map.ofSeq
        | _ -> Map.empty

    let holders =
        guards
        |> Map.toSeq
        |> Seq.choose (fun (name, guarded) ->
            Map.tryFind name placed
            |> Option.map (fun (creep, room) -> guarded, holderOf creep room))
        |> Seq.groupBy fst
        |> Seq.map (fun (room, held) -> room, held |> Seq.map snd |> List.ofSeq)
        |> Map.ofSeq

    let sweep =
        views
        |> List.tryPick (fun view ->
            view.Harass |> List.tryHead |> Option.map (fun h -> h.RoomName, view))

    let homes = lazy (views |> List.map Facts.homesOfOurs |> Set.unionMany)

    // A job's value, only where the decide read it this tick.
    let valueOf (job: Job) =
        if job.Value.IsValueCreated then
            Some job.Value.Value
        else
            None

    let firstValue (brief: Brief) =
        brief.Jobs |> List.tryHead |> Option.bind valueOf

    let campOf (brief: Brief) =
        brief.Jobs |> List.tryFind (fun job -> job.Kind = JobKind.Camp)

    // A camp's odds, read with its value where the decide read that: the
    // two are one price (`Facts.campPrice`).
    let campOdds (camp: Job) =
        if camp.Value.IsValueCreated then camp.Odds.Value else None

    let contested =
        briefs
        |> List.collect (fun (task, brief, view) ->
            match task, brief.Duty, brief.Contention with
            | Guard room, Duty.Guard BriefKind.Contested, Some contention ->
                let swept = sweep |> Option.exists (fun (chosen, _) -> chosen = room)

                // In the sweep's room a camp is a job of the sweep, on its row.
                let kind =
                    match campOf brief with
                    | _ when swept -> None
                    | Some camp -> Some(OperationKind.Camp, Some camp)
                    | None when contention.Phase = Phase.Prey -> Some(OperationKind.Trespass, None)
                    | None when contention.Phase = Phase.Raid && not (homes.Value.Contains room) ->
                        Some(OperationKind.Hold, None)
                    | None -> None

                kind
                |> Option.map (fun (kind, camp) ->
                    view,
                    {
                        Kind = kind
                        Room = room
                        Predicted =
                            lazy
                                {
                                    // The decide prices no rate for a
                                    // contested room: its jobs carry its price.
                                    Rate = None
                                    Value = firstValue brief
                                    Odds = camp |> Option.bind campOdds
                                }
                        Camp = []
                    })
                |> Option.toList
            | _ -> [])

    let swept =
        sweep
        |> Option.map (fun (room, view) ->
            let brief =
                briefs
                |> List.tryPick (fun (task, brief, _) ->
                    if task = Guard room then Some brief else None)

            let camp = brief |> Option.bind campOf

            view,
            {
                Kind = OperationKind.Sweep
                Room = room
                Predicted =
                    lazy
                        {
                            Rate = Some view.World.SweepRate
                            Value = brief |> Option.bind firstValue
                            Odds = camp |> Option.bind campOdds
                        }
                Camp = camp |> Option.map _.Quarry |> Option.defaultValue []
            })
        |> Option.toList

    let seen = swept @ contested

    // Each room read once, off the view that priced its operation, else
    // the first that projects it.
    let viewOf room =
        seen
        |> List.tryPick (fun (view, op) -> if op.Room = room then Some view else None)
        |> Option.orElse (views |> List.tryFind (fun view -> view.Spatial.Rooms.ContainsKey room))

    {
        Seen = List.map snd seen
        Readings =
            (seen |> List.map (fun (_, op) -> op.Room))
            @ (holders |> Map.keys |> List.ofSeq)
            |> List.distinct
            |> List.choose (fun room ->
                viewOf room |> Option.map (fun view -> room, lazy (readingIn view room)))
            |> Map.ofList
        Holders = holders
    }
