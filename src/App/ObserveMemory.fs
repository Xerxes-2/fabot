/// Serialization shell for the observe channels: read each prior log from
/// `Memory.fabot.observe`, hand it to its pure Core fold and write the result
/// back; the Layout leaf is written outright. The Transition log and the CPU
/// line, read by the operator alone, live on the heap and in memory segments
/// instead (#639). Absent or unreadable state is
/// discarded, never repaired, so telemetry cannot take the colony down.
module Fabot.ObserveMemory

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types
open Fabot.Core.Observe

module Ledger = Fabot.Core.Decide.Ledger

// Body parts ride the Core's own part-name table in both directions, so a
// case added without its wire name fails a test rather than decoding silently.
let private partOf = reverseOf partName allBodyParts

// ADR-0028
// A wire array read row by row: an absent leaf is the empty list and a row
// that will not decode costs its own row and no more. A decoder says "not
// this row" by answering None or by throwing; both read the same from here.
// The null guard matters: `unbox` is erased by Fable, so a missing key walks
// an `undefined`, throws, and would hit the enclosing `try` that discards the
// whole leaf.
let private rowsOf (decode: obj -> 'a option) (raw: obj) : 'a list =
    if isNull raw then
        []
    else
        raw
        |> unbox<obj[]>
        |> Array.choose (fun row ->
            try
                decode row
            with _ ->
                None)
        |> Array.toList

// A keyed wire object built out of pairs: the shape every flat leaf of this
// bundle writes its map as, so a reader of `Memory` finds one spelling.
let private hashOf (encode: 'v -> obj) (entries: seq<string * 'v>) : obj =
    let o = createEmpty<obj>

    for key, value in entries do
        o?(key) <- encode value

    o

// A wire leaf read whole: an absent leaf and one that will not decode both
// read as the empty value and cost that leaf and no more. The leaf arrives as
// a thunk because reading it is itself part of what may throw.
let private leafOr (empty: 'a) (leaf: unit -> obj) (decode: obj -> 'a) : 'a =
    try
        let raw = leaf ()
        if isNull raw then empty else decode raw
    with _ ->
        empty

// A whole number off the wire, and a throw for anything else. `unbox<int>`
// is erased by Fable, so an off-shape value compiles to `| 0` and reads back
// as tick 0: a stand-down's expiry read as spent (#294). A throw and not a
// default, because every caller is inside a row decoder and a default would
// be the invented tick all over again.
let private numberValue (value: obj) : int option =
    if jsTypeof value <> "number" then
        None
    else
        Some(unbox<int> value)

let private numberOf (raw: obj) (key: string) : int =
    match numberValue raw?(key) with
    | Some number -> number
    // Named, because the gate's output is read by somebody holding a leaf and
    // asking which field of it will not read.
    | None -> failwith ("not a number: " + key)

let private floatOf (raw: obj) (key: string) : float =
    if jsTypeof raw?(key) = "number" then
        unbox<float> raw?(key)
    else
        failwith ("not a number: " + key)

/// A key a later bundle added to a row (#389, #391): absent reads as the
/// zero it means, present and not a number costs the row.
let private numberOrZero (raw: obj) (key: string) : int =
    if jsTypeof raw?(key) = "undefined" then
        0
    else
        numberOf raw key

/// The same for a fraction, the heap in MB.
let private floatOrZero (raw: obj) (key: string) : float =
    if jsTypeof raw?(key) = "undefined" then
        0.0
    else
        floatOf raw key

// A keyed wire object read back as whole numbers — `hashOf box`'s decode
// partner, absent reading as empty the way an absent row array does.
let private intMapOf (raw: obj) : Map<string, int> =
    if isNull raw then
        Map.empty
    else
        objectEntries raw
        // A value that is not a number costs its own entry: dropping a
        // baseline is a tick of silence, keeping it is a difference taken
        // against nonsense.
        |> Array.choose (fun (key, value) -> numberValue value |> Option.map (fun n -> key, n))
        |> Map.ofArray

// A reason's numbers sit on the row that names it, beside the reason
// rather than inside it, so a bare tag adds no fields; a row naming a
// reason that needs numbers and carrying none is dropped the way a row
// with an unknown name is.
let private writeNumbers (o: obj) =
    function
    | Some(walk, wait) ->
        o?walk <- walk
        o?wait <- wait
    | None -> ()

// Two degradations in one row, on purpose: a reason carrying neither number is
// a bare tag, which reads as `None` and is what every reason without numbers
// writes; a reason carrying one that will not read is a row that will not
// restate itself, and costs itself the way every other off-shape number does.
let private readNumbers (raw: obj) =
    if isNull raw?walk || isNull raw?wait then
        None
    else
        Some(numberOf raw "walk", numberOf raw "wait")

// A Candidate on the wire: a scored row carries the full matching key, a
// rejected row its reason — the presence of `reason` tells them apart.
let private encodeCandidate candidate =
    let o = createEmpty<obj>

    match candidate with
    | Candidate.Scored(task, rank, cost, load) ->
        o?task <- task
        o?rank <- rank
        o?cost <- cost
        o?load <- load
    | Candidate.Rejected(task, reason) ->
        o?task <- task
        o?reason <- rejectReasonName reason
        writeNumbers o (rejectReasonNumbers reason)

    o

let private decodeCandidate (raw: obj) : Candidate =
    if isNull raw?reason then
        Candidate.Scored(
            string raw?task,
            numberOf raw "rank",
            numberOf raw "cost",
            numberOf raw "load"
        )
    else
        match rejectReasonOf (readNumbers raw) (string raw?reason) with
        | Some reason -> Candidate.Rejected(string raw?task, reason)
        | None -> failwith "unknown wire name"

// A Verdict on the wire is a tagged plain object; the creep name is the
// map key one level up, so it is dropped here and restored on decode.
let private encodeVerdict verdict =
    let o = createEmpty<obj>

    match verdict with
    | Verdict.Matched(_, task, factor) ->
        o?kind <- "matched"
        o?task <- task
        o?factor <- matchFactorName factor
    | Verdict.Kept(_, task) ->
        o?kind <- "kept"
        o?task <- task
    | Verdict.Released(_, task, reason) ->
        o?kind <- "released"
        o?task <- task
        o?reason <- releaseReasonName reason
        writeNumbers o (releaseReasonNumbers reason)
    | Verdict.Unassigned(_, reason) ->
        o?kind <- "unassigned"
        o?reason <- idleReasonName reason
    | Verdict.Scoring(_, candidates) ->
        o?kind <- "scoring"
        o?candidates <- candidates |> List.map encodeCandidate |> List.toArray
    | Verdict.Grounded _ -> o?kind <- "grounded"
    | Verdict.Yielded(_, counterpart) ->
        o?kind <- "yielded"
        o?counterpart <- counterpart
    | Verdict.Rerouted _ -> o?kind <- "rerouted"
    | Verdict.Stalled _ -> o?kind <- "stalled"
    | Verdict.Refused(_, task, code, count) ->
        o?kind <- "refused"
        o?task <- task
        o?code <- code
        o?count <- count

    o

// Anything off the expected shape throws, and `decodeCreepLog` drops that
// one row — bad state is discarded, never repaired.
let private decodeVerdict creep (raw: obj) : Verdict =
    // A wire name outside the vocabulary, or one whose numbers are
    // missing, is not a Verdict we can restate, so it throws: a row says
    // what the gate compared or it does not survive the read.
    let look ofName name =
        match ofName name with
        | Some value -> value
        | None -> failwith "unknown wire name"

    match string raw?kind with
    | "matched" -> Verdict.Matched(creep, string raw?task, look matchFactorOf (string raw?factor))
    | "kept" -> Verdict.Kept(creep, string raw?task)
    | "released" ->
        Verdict.Released(
            creep,
            string raw?task,
            look (releaseReasonOf (readNumbers raw)) (string raw?reason)
        )
    | "unassigned" -> Verdict.Unassigned(creep, look idleReasonOf (string raw?reason))
    | "scoring" ->
        Verdict.Scoring(
            creep,
            raw?candidates |> unbox<obj[]> |> Array.map decodeCandidate |> Array.toList
        )
    | "grounded" -> Verdict.Grounded creep
    | "yielded" -> Verdict.Yielded(creep, string raw?counterpart)
    | "rerouted" -> Verdict.Rerouted creep
    | "stalled" -> Verdict.Stalled creep
    // A refusal names its Task, empty for none: a row with no string there
    // is off the shape, not a Task called "undefined".
    | "refused" when jsTypeof raw?task = "string" ->
        Verdict.Refused(creep, string raw?task, numberOf raw "code", numberOf raw "count")
    | _ -> failwith "unknown verdict kind"

let private encodeCreepLog (log: CreepLog) =
    let o = createEmpty<obj>

    o?log <-
        log.Entries
        |> List.map (fun entry ->
            let e = createEmpty<obj>
            e?t <- entry.Tick
            e?v <- encodeVerdict entry.Verdict
            e)
        |> List.toArray

    match log.LastTask with
    | Some verdict -> o?lastTask <- encodeVerdict verdict
    | None -> ()

    match log.LastScoring with
    | Some verdict -> o?lastScoring <- encodeVerdict verdict
    | None -> ()

    o?lastMove <- log.LastMove |> List.map encodeVerdict |> List.toArray
    o

// A Verdict this bundle cannot restate costs its own row and no more. A
// cursor that will not decode reads as no cursor.
let private decodeCreepLog creep (raw: obj) : CreepLog =
    let tryVerdict (raw: obj) =
        try
            Some(decodeVerdict creep raw)
        with _ ->
            None

    {
        Entries =
            raw?log
            |> rowsOf (fun e ->
                tryVerdict e?v
                |> Option.map (fun verdict ->
                    {
                        Tick = numberOf e "t"
                        Verdict = verdict
                    }))
        LastTask =
            if isNull raw?lastTask then
                None
            else
                tryVerdict raw?lastTask
        LastScoring =
            if isNull raw?lastScoring then
                None
            else
                tryVerdict raw?lastScoring
        LastMove = raw?lastMove |> rowsOf tryVerdict
    }

/// A tile with its room as a wire object, and the read of one: the Raid log's
/// coordinates, where the Layout's `tileObject` below leaves the room off. An
/// episode names no room of its own, so every coordinate it carries says its
/// own.
let private roomPosObject (tile: RoomPos) =
    let o = createEmpty<obj>
    o?room <- tile.Room
    o?x <- tile.X
    o?y <- tile.Y
    o

let private roomPosOf (raw: obj) : RoomPos =
    RoomPos.at
        (string raw?room)
        {
            X = numberOf raw "x"
            Y = numberOf raw "y"
        }

// A body as `{ <part>: count }`.
let private bodyObject (body: Map<BodyPart, int>) =
    let o = createEmpty<obj>

    for KeyValue(part, count) in body do
        o?(partName part) <- count

    o

// The read of one: a part this bundle does not name, or a count that is no
// number, costs the row around it.
let private bodyOf (raw: obj) : Map<BodyPart, int> =
    objectEntries raw
    |> Array.map (fun (name, count) ->
        match partOf name, numberValue count with
        | Some part, Some number -> part, number
        | Some _, None -> failwith ("not a number: " + name)
        | None, _ -> failwith "unknown wire name")
    |> Map.ofArray

// One raid episode on the wire: `{ opened, last, rooms, closest?, losses,
// damage }`. Who the raid was is the world roster's (`saveRoster`); a legacy
// episode's own `roster` and `killed` are read as none and not written back.
let private encodeEpisode (episode: RaidEpisode) =
    let o = createEmpty<obj>
    o?opened <- episode.Opened
    o?last <- episode.LastSeen
    o?rooms <- episode.Rooms |> Set.toArray

    match episode.Closest with
    | Some approach ->
        // The room rides the tile; without it the coordinate would read as
        // home's.
        let c = roomPosObject approach.Pos
        c?range <- approach.Range
        c?t <- approach.Tick
        o?closest <- c
    | None -> ()

    o?losses <-
        episode.Losses
        |> List.map (fun loss ->
            let d = createEmpty<obj>
            d?creep <- loss.Creep
            d?t <- loss.Tick

            match loss.Where with
            | Some tile ->
                d?room <- tile.Room
                d?x <- tile.X
                d?y <- tile.Y
            | None -> ()

            if loss.Cast > 0 then
                d?cast <- loss.Cast

            d)
        |> List.toArray

    o?damage <- episode.Damage
    o

// A list of creep ids or room names off the wire (#507): anything not a list
// reads as none, and an entry that is no name costs the entry and never the
// row around it.
let private idsOf (raw: obj) : string[] =
    if not (JS.Constructors.Array.isArray raw) then
        [||]
    else
        raw
        |> unbox<obj[]>
        |> Array.filter (fun id -> jsTypeof id = "string" && (unbox<string> id).Length > 0)
        |> Array.map unbox<string>

let private decodeEpisode (raw: obj) : RaidEpisode =
    {
        Opened = numberOf raw "opened"
        LastSeen = numberOf raw "last"
        // Absent on a legacy episode, which named its hostiles and no room:
        // its roster is not carried over, having no room to file it under.
        Rooms = idsOf raw?rooms |> Fresh.setOfSeq
        Closest =
            if isNull raw?closest then
                None
            else
                let closest = raw?closest

                Some
                    {
                        Range = numberOf closest "range"
                        Pos =
                            {
                                // An approach with no room reads as the
                                // empty name, as a projection naming no
                                // room does, rather than costing the row.
                                Room = if isNull closest?room then "" else string closest?room
                                X = numberOf closest "x"
                                Y = numberOf closest "y"
                            }
                        Tick = numberOf closest "t"
                    }
        Losses =
            raw?losses
            |> unbox<obj[]>
            |> Array.map (fun d ->
                {
                    Creep = string d?creep
                    Tick = numberOf d "t"
                    // Absent on a legacy row and on a body the projection
                    // never placed: both read as no tile.
                    Where = if isNull d?room then None else Some(roomPosOf d)
                    // Absent on a legacy row and where no fighter stood (#612).
                    Cast = if isNull d?cast then 0 else numberOf d "cast"
                })
            |> Array.toList
        // Absent on a legacy episode, and zero is what that says; present and
        // off-shape costs the row like every number.
        Damage = if isNull raw?damage then 0 else numberOf raw "damage"
    }

// One outpost episode on the wire. A key of its own beside `episodes`, so a
// bad row of one family costs the other nothing.
let private encodeOutpost (episode: OutpostEpisode) =
    let o = createEmpty<obj>
    o?room <- episode.RoomName
    o?opened <- episode.Opened
    o?last <- episode.LastSeen
    o?expiry <- episode.Expiry
    o?basis <- standDownBasisName episode.Basis
    // Written only when true, so a legacy row reads `false`.
    if episode.Stronghold then
        o?stronghold <- true

    // Written only while a raid body is unaccounted for, so a legacy row and
    // a spent one read alike: no body to wait on, the clock alone.
    if not (Set.isEmpty episode.Raiders) then
        o?raiders <- episode.Raiders |> Set.toArray

    // Written only on a player's raid, so a legacy row reads as none.
    match episode.PlayerSince with
    | Some since -> o?player <- since
    | None -> ()

    // Written only when true (#510), so a legacy row reads as no chase.
    if episode.Chased then
        o?chased <- true

    // Written only when true, so a legacy row reads as no bounce.
    if episode.Bounced then
        o?bounced <- true

    // Written only once a look has read the holder (#604), so a legacy row
    // reads unread and is questioned.
    if not episode.HolderUnread then
        o?read <- true

    o

let private decodeOutpost (raw: obj) : OutpostEpisode =
    {
        RoomName = string raw?room
        Opened = numberOf raw "opened"
        LastSeen = numberOf raw "last"
        // The one field with no honest default: a missing or off-shape expiry
        // would read as `undefined` or 0 and `standingDown` would report a
        // running stand-down as spent, so the row is dropped instead.
        Expiry = numberOf raw "expiry"
        // A basis the vocabulary does not have costs its row rather than
        // reading as another basis.
        Basis =
            match standDownBasisOf (string raw?basis) with
            | Some basis -> basis
            | None -> failwith "unknown wire name"
        // Absent on a legacy row and on one that never saw a bunker: both
        // mean "no bunker here".
        Stronghold = not (isNull raw?stronghold) && unbox<bool> raw?stronghold
        // Absent on a legacy row and on one no body holds: the clock alone.
        Raiders = idsOf raw?raiders |> Fresh.setOfSeq
        // Absent on a legacy row and on one no player's raid stood in;
        // present and off-shape costs the row like every number.
        PlayerSince =
            if isNull raw?player then
                None
            else
                Some(numberOf raw "player")
        // Absent on a legacy row and on one no ranger chases; anything but
        // `true` reads as no chase, which projects nothing.
        Chased = (raw?chased: obj) = box true
        // Absent on a legacy row and on one no raid came back to.
        Bounced = (raw?bounced: obj) = box true
        // Absent on a row written before #604: its holder is unread.
        HolderUnread = (raw?read: obj) <> box true
    }

// One room held by somebody else's reservation on the wire: `{ holder,
// until }`, the absolute tick the engine's countdown ends on. Under the
// room's own key, not a row of the ring: this withdraws only the reservation.
let private encodeHold (hold: OutpostHold) =
    let o = createEmpty<obj>
    o?holder <- reservationHolderName hold.Holder
    o?until <- hold.Until
    o

// A checker rather than a cast: an `until` that is not a number would hold
// the room in the record for ever, `view.Time < until` comparing false against
// a string. An entry that will not read costs a body: the room reads as
// reservable and the reserver row buys the deficit until the next tick with
// vision writes the entry back, which is why `observe.mjs` fails the whole
// command over one such entry.
let private decodeHold (raw: obj) : OutpostHold =
    if isNull raw || jsTypeof raw <> "object" then
        failwith "not a hold"
    else
        let holder =
            match reservationHolderOf (string raw?holder) with
            | Some holder -> holder
            | None -> failwith "unknown wire name"

        {
            Holder = holder
            Until = numberOf raw "until"
        }

// One remembered raid on the wire: `{ until }`, the tick the guard row stops
// answering for a room it has gone blind in. An object and not a bare number:
// `rivalHeld` was a bare number once and growing it a second field on a live
// bundle cost a migration clause that is still there (`decodeLatch`).
let private encodeThreat (latch: ThreatLatch) =
    let o = createEmpty<obj>
    o?until <- latch.Until
    o

// A checker and not a cast, for `decodeHold`'s reason: an `until` that is not
// a number would hire a guard for ever. A dropped entry costs the room its
// memory, and the guard row falls back to what vision says.
let private decodeThreat (raw: obj) : ThreatLatch =
    if isNull raw || jsTypeof raw <> "object" then
        failwith "not a threat latch"
    else
        { Until = numberOf raw "until" }

// One fought room on the wire: `{ seen, raiders }`, the tick its raid was
// last seen and the ids standing at that sighting, `raiders` absent while
// none are known. An object for `encodeThreat`'s reason.
let private encodeFight (record: FightRecord) =
    let o = createEmpty<obj>
    o?seen <- record.Seen

    // Written only while known, so a legacy row reads as none: a hold that
    // runs its clock, as before.
    if not (Set.isEmpty record.Raiders) then
        o?raiders <- record.Raiders |> Set.toArray

    o

// A checker and not a cast: a `seen` that is not a number would hold the
// doors for ever. A dropped entry costs the room its record. The `squad` a
// record from before the squad's retirement carries is not read, and not
// written back.
let private decodeFight (value: obj) : FightRecord =
    if isNull value || jsTypeof value <> "object" then
        failwith "not a fight record"
    else
        {
            Seen = numberOf value "seen"
            Raiders = idsOf value?raiders |> Fresh.setOfSeq
        }

// One room's probe log (#493) on the wire: `{ probe, first, hits, expires,
// fate, hitAt?, safeMode?, ended?, home? }`, `fate` one of probing, out, died,
// expired and `ended` its tick, absent while probing; `home` the colony whose
// probe it is, absent on a log before the world held it.
let private encodeProbe (log: ProbeLog) =
    let o = createEmpty<obj>
    o?probe <- log.Probe
    o?first <- log.FirstHit
    o?hits <- log.Hits
    o?expires <- log.Expires

    if log.Home <> "" then
        o?home <- log.Home

    log.HitAt |> Option.iter (fun at -> o?hitAt <- at)
    log.SafeModeAt |> Option.iter (fun at -> o?safeMode <- at)

    match log.Fate with
    | Probing -> o?fate <- "probing"
    | Out tick ->
        o?fate <- "out"
        o?ended <- tick
    | Died tick ->
        o?fate <- "died"
        o?ended <- tick
    | Expired tick ->
        o?fate <- "expired"
        o?ended <- tick

    o

// A checker and not a cast: a name that is no string, a tick that is no
// number or a fate the encoder never writes costs the room its log, which
// the next probe's first dismantle opens afresh.
let private decodeProbe (value: obj) : ProbeLog =
    if isNull value || jsTypeof value <> "object" || jsTypeof value?probe <> "string" then
        failwith "not a probe log"
    else
        let optional key =
            if jsTypeof value?(key) = "undefined" then
                None
            else
                Some(numberOf value key)

        let named =
            if jsTypeof value?fate = "string" then
                unbox<string> value?fate
            else
                ""

        let fate =
            match named with
            | "probing" -> Probing
            | "out" -> Out(numberOf value "ended")
            | "died" -> Died(numberOf value "ended")
            | "expired" -> Expired(numberOf value "ended")
            | _ -> failwith "not a probe's fate"

        {
            Probe = unbox<string> value?probe
            FirstHit = numberOf value "first"
            Hits = numberOf value "hits"
            HitAt = optional "hitAt"
            SafeModeAt = optional "safeMode"
            Fate = fate
            Expires = numberOf value "expires"
            // Absent on a log before the world held it: anybody's to move.
            Home =
                if jsTypeof value?home = "string" then
                    unbox<string> value?home
                else
                    ""
        }

// One latched room on the wire: `{ since, lastLooked }`, the tick the gate
// shut on and the tick of the last look, which the next look's stride is
// measured from.
let private encodeLatch (latch: RivalLatch) =
    let o = createEmpty<obj>
    o?since <- latch.Since
    o?lastLooked <- latch.LastLooked
    o

// A latch written as a bare number is the legacy shape (pre-#275): the tick
// the gate shut on, which was also the tick the stride was counted from, so
// it reads as both fields. A record with `since` and no `lastLooked` reads
// the same way. Every other shape throws and costs this entry alone: `unbox`
// is a cast, so `{}`, `[100, 100]` and `"100"` would all decode to the epoch
// and leave `lookDue` comparing a tick against a string for ever. A dropped
// latch re-enters the scan and the next look with vision decides it again.
let private decodeLatch (raw: obj) : RivalLatch =
    let tickOf (field: obj) =
        if jsTypeof field = "number" then
            unbox<int> field
        else
            failwith "not a tick"

    if jsTypeof raw = "number" then
        let since = unbox<int> raw

        { Since = since; LastLooked = since }
    elif isNull raw || jsTypeof raw <> "object" then
        failwith "not a latch"
    else
        let since = tickOf raw?since

        {
            Since = since
            LastLooked =
                if isNull raw?lastLooked then
                    since
                else
                    tickOf raw?lastLooked
        }

// The observe subtree is created on demand and replaced whole only when what
// stands there is not an object; each writer then assigns its own leaf.
let private ensureObserve () =
    if isNull Memory?fabot then
        Memory?fabot <- createEmpty<obj>

    if jsTypeof Memory?fabot?observe <> "object" || isNull Memory?fabot?observe then
        Memory?fabot?observe <- createEmpty<obj>

// One colony's own subtree under `Memory.fabot.observe.colonies.<home>`.
let private ensureColony (home: string) =
    ensureObserve ()

    if
        jsTypeof Memory?fabot?observe?colonies <> "object"
        || isNull Memory?fabot?observe?colonies
    then
        Memory?fabot?observe?colonies <- createEmpty<obj>

    let colonies = Memory?fabot?observe?colonies

    if jsTypeof colonies?(home) <> "object" || isNull colonies?(home) then
        colonies?(home) <- createEmpty<obj>

// One flat leaf of `Memory.fabot.observe` as it stands, or null when the
// subtree or the leaf is absent.
let private observeLeaf (leaf: string) : obj =
    let fabot = Memory?fabot
    let observe = if isNull fabot then null else fabot?observe

    if isNull observe then null else observe?(leaf)

// Write one flat leaf, leaving the rest of the observe subtree alone — unless
// the subtree itself is not an object, in which case the bad state is replaced.
let private writeObserveLeaf (leaf: string) (value: obj) =
    ensureObserve ()
    Memory?fabot?observe?(leaf) <- value

// Write one leaf of one colony's own subtree, leaving every other leaf alone.
let private writeColonyLeaf (home: string) (leaf: string) (value: obj) =
    ensureColony home
    Memory?fabot?observe?colonies?(home)?(leaf) <- value

// One colony's leaf as it stands in Memory, or null when the subtree, the
// colony or the leaf is absent.
let private colonyLeaf (home: string) (leaf: string) : obj =
    let colonies = observeLeaf "colonies"
    let colony = if isNull colonies then null else colonies?(home)

    if isNull colony then null else colony?(leaf)

/// The live cadence from `Memory.fabot.observe.cadence` (ticks per full
/// decide, `Tuning.Cadence`), clamped to `Tuning.minCadence`..`Tuning.maxCadence`: set, it
/// takes the place of the tuned and the bucket's. Read each tick, so a flip
/// through the Memory HTTP API lands on the next; anything but a whole
/// number reads as unset.
let loadCadence () : int option =
    leafOr None (fun () -> observeLeaf "cadence") (fun raw ->
        if jsTypeof raw <> "number" then
            None
        else
            let value = unbox<float> raw

            if System.Double.IsNaN value || value <> floor value then
                None
            else
                // Clamped as a float first: a huge number never reaches `int`.
                Some(Fabot.Core.LightTick.clampCadence (int (max 0.0 (min 99.0 value)))))

/// The live fight-only switch from `Memory.fabot.observe.fightOnly`
/// (`Tuning.FightOnly`, `LightTick.fightOnly`): set, it takes the tuned one's place.
/// Read each tick, so a flip through the Memory HTTP API lands on the next;
/// anything but a boolean reads as unset.
let loadFightOnly () : bool option =
    leafOr None (fun () -> observeLeaf "fightOnly") (fun raw ->
        if jsTypeof raw = "boolean" then
            Some(unbox<bool> raw)
        else
            None)

/// The live switches in effect this tick and where each comes from, at
/// `Memory.fabot.observe.switches` (#575): `observe health` and `observe cpu`
/// print them, an override standing flagged. Written each tick, read by
/// nothing in the bot.
let saveSwitches
    (cadence: int * Fabot.Core.LightTick.SwitchFrom)
    (fightOnly: bool * Fabot.Core.LightTick.SwitchFrom)
    =
    let from source =
        match source with
        | Fabot.Core.LightTick.SwitchFrom.Tuned -> "tuned"
        | Fabot.Core.LightTick.SwitchFrom.Bucket -> "bucket"
        | Fabot.Core.LightTick.SwitchFrom.Override -> "override"

    let raw = createEmpty<obj>
    raw?cadence <- fst cadence
    raw?cadenceFrom <- from (snd cadence)
    raw?fightOnly <- fst fightOnly
    raw?fightOnlyFrom <- from (snd fightOnly)
    writeObserveLeaf "switches" raw

/// A list of names off one observe leaf, read fresh each tick so a flip
/// through the Memory HTTP API takes effect on the next tick with no
/// redeploy; absent or malformed means off.
let private nameList (leaf: string) : Set<string> =
    leafOr Set.empty (fun () -> observeLeaf leaf) (fun names ->
        if not (JS.Constructors.Array.isArray names) then
            Set.empty
        else
            // Malformed means off entry-wise too: anything but a string
            // array reads as the empty list, never as a repaired one.
            let entries = names |> unbox<obj[]>

            if entries |> Array.forall (fun e -> jsTypeof e = "string") then
                entries |> Array.map unbox<string> |> Set.ofArray
            else
                Set.empty)

/// The verbose list from `Memory.fabot.observe.verbose`: creep names owed
/// full candidate scoring this tick.
let loadVerbose () : Set<string> = nameList "verbose"

/// The draw list from `Memory.fabot.observe.visuals` (#638): rooms and
/// creeps, by name, whose creeps the room visuals overlay this tick.
let loadVisuals () : Set<string> = nameList "visuals"

// ---- the operator-only logs' segments (#639) ------------------------------
//
// The Transition log and the CPU line are read by `observe.mjs` alone, so
// they live in memory segments, not in the Memory the engine parses and
// serialises every tick. Their wire is kept on the heap and written out to
// the segments every `segmentEvery` ticks and on a tick that caught a fault.
// The ids and the cadence are mirrored by `scripts/observe-segments.mjs`.

/// The segment ids each log's JSON is cut across, in order: room for twice
/// the live size of each (the Transition log 198 KB, the CPU line 56 KB, on
/// 2026-10-08), and the rival dossier's, whose caps (`Observe.capRivals`)
/// keep it inside them at 139 KB; and the operation ledger's (#662), whose
/// caps (`Ledger.capClosed`) keep it inside one: nine of the engine's ten a
/// tick.
let creepSegments = [| 0; 1; 2; 3 |]
let cpuSegments = [| 4; 5 |]
let rivalSegments = [| 6; 7 |]
let ledgerSegments = [| 8 |]

let private segmentIds =
    Array.concat [ creepSegments; cpuSegments; rivalSegments; ledgerSegments ]

/// The engine's cap on one segment, in string length.
let private segmentCap = 100 * 1024

/// Ticks between two segment writes: a write stringifies both logs whole
/// (about a millisecond), so this spends a tenth of that a tick, and
/// `observe` reads a log at most this many ticks stale.
let segmentEvery = 10

// The two logs' wire on the heap, null until restored: the leaf the codec
// below appends to and the segments are written from.
let mutable private creepsWire: obj = null
let mutable private cpuWire: obj = null

// The rival dossiers on the heap, None until restored: encoded only on a
// tick the segments are written.
let mutable private rivalsHeld: Map<string, Dossier> option = None

// The operation ledger on the heap, None until restored: encoded only on a
// tick the segments are written.
let mutable private ledgerHeld: Ledger.Ledger option = None

// The tick a fault was last recorded on, which writes the segments that tick.
let mutable private faultedAt = -1

// Whether a legacy leaf was read and deleted since the last write: the
// segments are written that same tick, so no reset can find neither.
let mutable private migrated = false

// The logs found past their segments' room on this heap, each reported once.
let mutable private oversized: Set<string> = Fresh.setOfSeq Seq.empty

// Whether every segment the logs ride is active, and so readable, this tick.
let private segmentsVisible () =
    let segments: obj = RawMemory?segments

    segmentIds |> Array.forall (fun id -> jsTypeof segments?(string id) = "string")

// Boxed, so Fable emits a plain array and not an `Int32Array`, which the
// engine's `_.isArray` check refuses. Takes effect on the next tick, and
// stays until changed, so a cold heap finds them already loaded.
let private requestSegments () =
    RawMemory?setActiveSegments(segmentIds |> Array.map box)

/// A log's stored wire on a cold heap: the legacy Memory leaf once, deleted as
/// it is read (the migration off Memory) and written to the segments that
/// tick, else its segments joined and parsed — null for none written or one
/// that will not parse. None while the segments are not active this tick:
/// they are requested, and the caller starts from empty and writes nothing
/// until they arrive, so the stored log is never overwritten by an empty one.
let private restoreWire (leaf: string) (ids: int[]) : obj option =
    let legacy = observeLeaf leaf

    if not (isNull legacy) then
        emitJsStatement (Memory?fabot?observe, leaf) "delete $0[$1]"
        migrated <- true
        Some legacy
    elif segmentsVisible () then
        let text =
            ids
            |> Array.map (fun id -> unbox<string> RawMemory?segments?(string id))
            |> String.concat ""

        try
            Some(if text = "" then null else JS.JSON.parse text)
        with _ ->
            Some null
    else
        requestSegments ()
        None

/// The Transition log off its wire, or empty when the wire is absent or
/// unreadable. A creep whose log will not decode costs that creep alone, and
/// inside a log an unreadable row costs itself.
let decodeLog (wire: obj) : ObserveState =
    leafOr Map.empty (fun () -> wire) (fun creeps ->
        objectEntries creeps
        |> Array.choose (fun (name, raw) ->
            try
                Some(name, decodeCreepLog name raw)
            with _ ->
                None)
        |> Fresh.mapOfArray)

/// The stored log on a cold heap (`restoreWire`); None while its segments
/// are not active yet.
let restoreLog () : ObserveState option =
    restoreWire "creeps" creepSegments |> Option.map decodeLog

/// The whole log on the wire, every creep.
let encodeLog (state: ObserveState) : obj =
    state |> Map.toSeq |> hashOf encodeCreepLog

/// Set the heap wire to the whole log; what `saveChanged` below falls back to.
let save (state: ObserveState) = creepsWire <- encodeLog state

/// Whether a creep's timeline is the one already written: the same entries —
/// by reference, which is what `Observe.step` keeps when a tick appends
/// nothing — and the same three cursors. A creep the fold handed back
/// unchanged is one whose row in the leaf is already right.
let private sameLog (a: CreepLog) (b: CreepLog) : bool =
    obj.ReferenceEquals(a.Entries, b.Entries)
    && a.LastTask = b.LastTask
    && a.LastScoring = b.LastScoring
    && a.LastMove = b.LastMove

/// Write the creeps whose timeline moved this tick onto `creeps` and no
/// other, and drop the ones the fold pruned; the shape is the one `encodeLog`
/// writes, key for key. Encoding the log whole every tick cost 1.5 ms of a
/// live tick (#370). Answers the wire to keep: `creeps` itself, or the log
/// encoded whole when `creeps` does not agree with `prior` by key count.
let saveChangedOnto (creeps: obj) (prior: ObserveState) (state: ObserveState) : obj =
    if
        isNull creeps
        || jsTypeof creeps <> "object"
        || JS.Constructors.Array.isArray creeps
        || (JS.Constructors.Object.keys creeps).Count <> Map.count prior
    then
        encodeLog state
    else
        for KeyValue(name, log) in state do
            let unchanged =
                match Map.tryFind name prior with
                | Some before -> sameLog before log
                | None -> false

            if not unchanged then
                creeps?(name) <- encodeCreepLog log

        // Off the leaf's own keys and not the prior's, so a row the load
        // could not restate — in the leaf, in no state — is dropped as the
        // whole write dropped it, rather than kept for ever.
        for name in JS.Constructors.Object.keys creeps do
            if not (Map.containsKey name state) then
                emitJsStatement (creeps, name) "delete $0[$1]"

        creeps

/// `saveChangedOnto` the heap wire.
let saveChanged (prior: ObserveState) (state: ObserveState) =
    creepsWire <- saveChangedOnto creepsWire prior state

let private reactorOwnerName =
    function
    | ReactorOwner.Ours -> "ours"
    | ReactorOwner.Unowned -> "none"
    | ReactorOwner.Rival username -> "rival:" + username

let private reactorOwnerOf name =
    match name with
    | "ours" -> ReactorOwner.Ours
    | "none" -> ReactorOwner.Unowned
    | value when value.StartsWith("rival:") && value.Length > 6 ->
        ReactorOwner.Rival(value.Substring 6)
    | _ -> failwith "unknown reactor owner"

let private optionalInt (raw: obj) : int option =
    if jsTypeof raw = "undefined" then
        failwith "missing number-or-null field"
    elif isNull raw then
        None
    elif jsTypeof raw = "number" then
        Some(unbox<int> raw)
    else
        failwith "expected a number or null"

/// The prior global Reactor programme record. All seven fields form one
/// sample, so an absent, legacy or malformed leaf degrades whole to the empty
/// state rather than combining dates and values from different wire shapes.
let loadReactor () : ReactorState =
    leafOr ReactorState.empty (fun () -> observeLeaf "reactor") (fun raw ->
        if
            jsTypeof raw?owner <> "string"
            || jsTypeof raw?storeT <> "number"
            || jsTypeof raw?continuousWork <> "number"
            || jsTypeof raw?bankedT <> "number"
            || jsTypeof raw?dryTicks <> "number"
        then
            failwith "malformed reactor leaf"

        {
            Owner = reactorOwnerOf (unbox<string> raw?owner)
            StoreT = unbox<int> raw?storeT
            ContinuousWork = unbox<int> raw?continuousWork
            Seen = optionalInt raw?seen
            BankedT = unbox<int> raw?bankedT
            LastDelivery = optionalInt raw?lastDelivery
            DryTicks = unbox<int> raw?dryTicks
        })

/// Write the one sector Reactor programme as a flat observe leaf. Optional
/// dates are explicit nulls, so every write carries the complete seven-field
/// wire shape even before the first sight or delivery.
let saveReactor (state: ReactorState) =
    let raw = createEmpty<obj>
    raw?owner <- reactorOwnerName state.Owner
    raw?storeT <- state.StoreT
    raw?continuousWork <- state.ContinuousWork
    raw?seen <- state.Seen |> Option.map box |> Option.defaultValue null
    raw?bankedT <- state.BankedT
    raw?lastDelivery <- state.LastDelivery |> Option.map box |> Option.defaultValue null
    raw?dryTicks <- state.DryTicks
    writeObserveLeaf "reactor" raw

/// The named colony's prior Raid log, or empty when its subtree is absent or
/// unreadable; an episode that will not decode costs that episode alone. A
/// flat legacy `observe.raids` leaf is not migrated: it reads as absent, and
/// `pruneColonies` drops it.
let loadRaids (home: string) : RaidState =
    leafOr RaidState.empty (fun () -> colonyLeaf home "raids") (fun raids ->
        {
            Episodes = raids?episodes |> rowsOf (decodeEpisode >> Some)
            // The stand-down rows, latches, holds, threats, fight records,
            // probes and taken rooms are the world's (`loadDanger`,
            // `loadRoomLatches`): a key an older bundle left here is not read.
            Outposts = RaidState.empty.Outposts
            RivalHeld = RaidState.empty.RivalHeld
            Holds = RaidState.empty.Holds
            Threatened = RaidState.empty.Threatened
            Fought = RaidState.empty.Fought
            Probes = RaidState.empty.Probes
            Taken = RaidState.empty.Taken
            // `true` and nothing else: `unbox` is erased, so a truthy
            // non-boolean would latch a colony under its reserve (#505).
            ReserveShort =
                jsTypeof raids?reserveShort = "boolean" && unbox<bool> raids?reserveShort
            // The ore dig's latch: the engine's letters, any other entry read
            // as shut. A missing key or a non-list is a latch never written,
            // read as every dig open, so a deploy shuts no running dig.
            DigOpen =
                if not (JS.Constructors.Array.isArray raids?digOpen) then
                    None
                else
                    raids?digOpen
                    |> unbox<obj[]>
                    |> Array.choose (fun letter ->
                        allOres
                        |> List.tryFind (fun ore ->
                            jsTypeof letter = "string"
                            && unbox<string> letter = resourceName (Ore ore)))
                    |> Set.ofArray
                    |> Some
            // The haul overflow reading (#508), on the same terms: a
            // non-number fill reads empty, and one off 0..1 is clamped into
            // it, so a hand-edit cannot hold the extra body for good.
            HaulOverflow =
                {
                    Fill =
                        if jsTypeof raids?haulFill = "number" then
                            unbox<float> raids?haulFill |> max 0.0 |> min 1.0
                        else
                            0.0
                    Owed = jsTypeof raids?haulOwed = "boolean" && unbox<bool> raids?haulOwed
                }
            // The home's last raided tick (#526): a non-number reads never.
            HomeRaided = numberValue raids?homeRaided
            // `unbox` is erased: without the filter a number under `living`
            // becomes a creep that "dies" next tick and charges the episode a
            // loss nobody suffered, and a string walks character by character
            // as one creep per letter.
            Living =
                if not (JS.Constructors.Array.isArray raids?living) then
                    Set.empty
                else
                    raids?living
                    |> unbox<obj[]>
                    |> Array.choose (fun name ->
                        if jsTypeof name = "string" then
                            Some(unbox<string> name)
                        else
                            None)
                    |> Set.ofArray
            // The tiles beside the names, absent from a legacy bundle: the
            // first loss after a deploy carries no tile.
            Placed =
                if isNull raids?placed then
                    Map.empty
                else
                    objectEntries raids?placed
                    |> Array.choose (fun (name, tile) ->
                        // Entry by entry: a bad tile costs its body, not the
                        // episode ring beside it.
                        try
                            Some(name, roomPosOf tile)
                        with _ ->
                            None)
                    |> Map.ofArray
            // The damage baseline; an empty one charges the next tick nothing.
            Hits = intMapOf raids?hits
        })

/// Write one colony's Raid log back under
/// `Memory.fabot.observe.colonies.<home>.raids`, with the colony's declared
/// outpost names beside it (#162): write-only, so `observe.mjs outposts` can
/// name an outpost the log holds no row for.
let saveRaids (home: string) (declared: string list) (state: RaidState) =
    let raids = createEmpty<obj>
    raids?declared <- declared |> List.toArray
    raids?episodes <- state.Episodes |> List.map encodeEpisode |> List.toArray
    // The stand-down rows, latches, holds, threats, fight records, probes
    // and taken rooms are the world's (`saveDanger`, `saveRoomLatches`), and
    // not written here.
    // The emergency reserve's latch (#505).
    raids?reserveShort <- state.ReserveShort
    // The ore dig's latch, in the engine's letters, `[]` while every dig is
    // shut; absent only while never written.
    state.DigOpen
    |> Option.iter (fun opened ->
        raids?digOpen <- opened |> Set.toArray |> Array.map (Ore >> resourceName))
    // The haul overflow reading (#508).
    raids?haulFill <- state.HaulOverflow.Fill
    raids?haulOwed <- state.HaulOverflow.Owed
    // The home's last raided tick (#526), absent for never.
    state.HomeRaided |> Option.iter (fun tick -> raids?homeRaided <- tick)
    raids?living <- state.Living |> Set.toArray

    raids?placed <- state.Placed |> Map.toSeq |> hashOf (roomPosObject >> box)

    raids?hits <- state.Hits |> Map.toSeq |> hashOf box
    writeColonyLeaf home "raids" raids

// A tile of an exit watch as `[x, y]`: a held run is up to 48 of them. Boxed
// apiece, so Fable writes a plain array and not an `Int32Array`, which
// `JSON.stringify` spells as an object.
let private pairOf (tile: Pos) : obj = box [| box tile.X; box tile.Y |]

let private tileOfPair (raw: obj) : Pos =
    if not (JS.Constructors.Array.isArray raw) then
        failwith "not a tile"
    else
        let pair = unbox<obj[]> raw

        match pair |> Array.map numberValue with
        | [| Some x; Some y |] -> { X = x; Y = y }
        | _ -> failwith "not a tile"

// One exit watch on the wire (#450, #542): `{ seen: [{ id, x, y, dies }] }`
// while its armed bodies stand, `{ run, exit, until }` once their exit is
// held. `id` came with #561: a row without it reads as no id, written so.
// One armed Threat as seen: `{ id?, x, y, dies, trip }`.
let private encodeSeenThreat (threat: SeenThreat) =
    let t = createEmpty<obj>

    if threat.Id <> "" then
        t?id <- threat.Id

    t?x <- threat.At.X
    t?y <- threat.At.Y
    t?dies <- threat.Dies
    t?trip <- threat.Trip
    t

// A checker: an id that is no string or a tick that is no number costs the
// watch it stands in.
let private decodeSeenThreat (threat: obj) : SeenThreat =
    {
        Id =
            match jsTypeof threat?id with
            | "undefined" -> ""
            | "string" -> unbox<string> threat?id
            | _ -> failwith "not a string: id"
        At =
            {
                X = numberOf threat "x"
                Y = numberOf threat "y"
            }
        Dies = numberOf threat "dies"
        // A row a bundle before #663 S5 wrote carries no trip: a room's
        // round trip, what nothing known of a raid is held for
        // (`WorldView.raidTrip`).
        Trip =
            match jsTypeof threat?trip with
            | "undefined" -> 2 * Engine.roomSide
            | _ -> numberOf threat "trip"
    }

let private encodeWatch (watch: ExitWatch) =
    let o = createEmpty<obj>

    match watch with
    | ExitWatch.Seen threats -> o?seen <- threats |> List.map encodeSeenThreat |> List.toArray
    | ExitWatch.Held hold ->
        o?run <- hold.Run |> List.map pairOf |> List.toArray
        o?exit <- pairOf hold.Exit
        o?left <- hold.Left

        if not hold.Leavers.IsEmpty then
            o?leavers <- hold.Leavers |> List.map encodeSeenThreat |> List.toArray

        o?until <- hold.Until

    o

// A checker: a tick that is no number or a tile that is no pair costs the
// watch, and the room's Guard falls back to what vision says.
let private decodeWatch (raw: obj) : ExitWatch =
    if isNull raw || jsTypeof raw <> "object" then
        failwith "not a watch"
    elif JS.Constructors.Array.isArray raw?seen then
        raw?seen
        |> unbox<obj[]>
        |> Array.map decodeSeenThreat
        |> List.ofArray
        |> ExitWatch.Seen
    elif JS.Constructors.Array.isArray raw?run then
        let until = numberOf raw "until"

        ExitWatch.Held
            {
                Run = raw?run |> unbox<obj[]> |> Array.map tileOfPair |> List.ofArray
                Exit = tileOfPair raw?exit
                // A hold a bundle before #663 S5 wrote carries neither when
                // its bodies left nor which: left at its last tick (back at
                // once, `Facts.preyBeforeReturn`), and no look decides it.
                Left =
                    match jsTypeof raw?left with
                    | "undefined" -> until
                    | _ -> numberOf raw "left"
                Leavers =
                    match raw?leavers with
                    | leavers when JS.Constructors.Array.isArray leavers ->
                        leavers |> unbox<obj[]> |> Array.map decodeSeenThreat |> List.ofArray
                    | leavers when jsTypeof leavers = "undefined" -> []
                    | _ -> failwith "not a list: leavers"
                Until = until
            }
    else
        failwith "not a watch"

// One room of the world danger log on the wire: `{ down?, hold?, threat?,
// watch?, fight?, taken?, probe? }`, each part written only while it holds
// and read alone, so a bad part costs that part and never the room's others.
// `fight` is a fought room's `{ seen, raiders? }`, `taken` the tick an
// assault room's targets were first seen down, `probe` its probe log.
let private encodeDangerRoom (entry: DangerRoom) =
    let o = createEmpty<obj>
    entry.Down |> Option.iter (fun down -> o?down <- encodeOutpost down)
    entry.Hold |> Option.iter (fun hold -> o?hold <- encodeHold hold)
    entry.Threat |> Option.iter (fun threat -> o?threat <- encodeThreat threat)
    entry.Watch |> Option.iter (fun watch -> o?watch <- encodeWatch watch)
    entry.Fight |> Option.iter (fun fight -> o?fight <- encodeFight fight)
    entry.Taken |> Option.iter (fun tick -> o?taken <- tick)
    entry.Probe |> Option.iter (fun probe -> o?probe <- encodeProbe probe)
    o

let private decodeDangerRoom (room: string) (raw: obj) : DangerRoom option =
    let part (key: string) (decode: obj -> 'a) =
        let value = raw?(key)

        if isNull value then
            None
        else
            try
                Some(decode value)
            with _ ->
                None

    if isNull raw || jsTypeof raw <> "object" then
        None
    else
        let entry =
            {
                // Filed under its room: a row naming another is the room's.
                Down =
                    part "down" (fun value ->
                        { decodeOutpost value with
                            RoomName = room
                        })
                Hold = part "hold" decodeHold
                Threat = part "threat" decodeThreat
                Watch = part "watch" decodeWatch
                Fight = part "fight" decodeFight
                Taken =
                    part "taken" (fun value ->
                        match numberValue value with
                        | Some tick -> tick
                        | None -> failwith "not a number: taken")
                Probe = part "probe" decodeProbe
            }

        if Danger.held entry then Some entry else None

// One leaf of `Memory.fabot.observe.world` as it stands, or null.
let private worldLeaf (leaf: string) : obj =
    let world = observeLeaf "world"

    if isNull world || jsTypeof world <> "object" then
        null
    else
        world?(leaf)

// `Memory.fabot.observe.world.danger` when it holds an object, as every
// bundle that writes it does, or null.
let private dangerLeaf () : obj =
    let value = worldLeaf "danger"

    if jsTypeof value = "object" && not (JS.Constructors.Array.isArray value) then
        value
    else
        null

/// The world danger log's rooms (`Danger`), the latches being the room
/// latches' (`loadRoomLatches`). An absent leaf reads empty, and the tick's
/// write makes it stand. A room that will not decode costs that room alone.
let loadDanger () : Danger =
    leafOr Danger.empty dangerLeaf (fun leaf ->
        { Danger.empty with
            Rooms =
                objectEntries leaf
                |> Array.choose (fun (room, raw) ->
                    decodeDangerRoom room raw |> Option.map (fun entry -> room, entry))
                |> Fresh.mapOfArray
        })

// The `world` subtree, made an object if what stands there is not one.
let private ensureWorld () =
    ensureObserve ()

    let world = Memory?fabot?observe?world

    if
        isNull world
        || jsTypeof world <> "object"
        || JS.Constructors.Array.isArray world
    then
        Memory?fabot?observe?world <- createEmpty<obj>

/// Write the world danger log's rooms under `Memory.fabot.observe.world.danger`,
/// every tick and whole, leaving the rest of `world` alone.
let saveDanger (danger: Danger) =
    ensureWorld ()
    Memory?fabot?observe?world?danger <- danger.Rooms |> Map.toSeq |> hashOf encodeDangerRoom

// One roster row on the wire: `{ o, b, f, l, e, d? }` — the owner, the body
// as first seen there (`{ <part>: count }`), the first and last ticks seen
// there, `e` the tick its life runs out on, and `d` the tick its tombstone
// was first seen (a kill when before `e`). Short keys: a siege writes
// hundreds of rows.
let private encodeRosterRow (row: RosterRow) =
    let o = createEmpty<obj>
    o?o <- row.Owner
    o?b <- bodyObject row.Body
    o?f <- row.First
    o?l <- row.Last
    o?e <- row.Dies
    row.Fell |> Option.iter (fun tick -> o?d <- tick)
    o

// A checker: an owner that is no string, a tick that is no number or a body
// off its shape costs the row.
let private decodeRosterRow (raw: obj) : RosterRow =
    if isNull raw || jsTypeof raw <> "object" || jsTypeof raw?o <> "string" then
        failwith "not a roster row"
    else
        {
            Owner = unbox<string> raw?o
            Body = bodyOf raw?b
            First = numberOf raw "f"
            Last = numberOf raw "l"
            Dies = numberOf raw "e"
            Fell = numberValue raw?d
        }

/// The world [[raid roster]] under `Memory.fabot.observe.world.roster`:
/// `{ <room>: { <hostile id>: row } }`. A row that will not decode costs that
/// row alone, a room that is no object that room.
let loadRoster () : Roster =
    leafOr Roster.empty (fun () -> worldLeaf "roster") (fun leaf ->
        if jsTypeof leaf <> "object" || JS.Constructors.Array.isArray leaf then
            Roster.empty
        else
            objectEntries leaf
            |> Array.choose (fun (room, rows) ->
                if isNull rows || jsTypeof rows <> "object" then
                    None
                else
                    let decoded =
                        objectEntries rows
                        |> Array.choose (fun (id, raw) ->
                            try
                                Some(id, decodeRosterRow raw)
                            with _ ->
                                None)
                        |> Fresh.mapOfArray

                    if Map.isEmpty decoded then None else Some(room, decoded))
            |> Fresh.mapOfArray)

/// Write the world roster back every full tick, empty or not.
let saveRoster (roster: Roster) =
    ensureWorld ()

    Memory?fabot?observe?world?roster <-
        roster
        |> Map.toSeq
        |> hashOf (fun rows -> rows |> Map.toSeq |> hashOf encodeRosterRow)

/// Drop the colony leaves of every home not among `homes` (a given-up
/// colony's log, which nothing writes again) and the flat `observe.raids` a
/// bundle before the colony split wrote. Nothing is dropped while `homes` is
/// empty, so a declaration emptied by mistake costs no record.
let pruneColonies (homes: string list) =
    if not (List.isEmpty homes) then
        let colonies = observeLeaf "colonies"

        if not (isNull colonies) && jsTypeof colonies = "object" then
            for home, _ in objectEntries colonies do
                if not (List.contains home homes) then
                    emitJsStatement (colonies, home) "delete $0[$1]"

        if not (isNull (observeLeaf "raids")) then
            emitJsStatement Memory?fabot?observe "delete $0.raids"

/// One tile as a wire object; the deferral rows carry two of them. The room
/// is not written: the leaf is already filed under the colony's home name.
let private tileObject (tile: RoomPos) =
    let o = createEmpty<obj>
    o?x <- tile.X
    o?y <- tile.Y
    o

/// The cascade's workforce arithmetic this tick, one leaf per colony
/// beside the Layout record: `{ target, living, casting, rows: [{ row,
/// quota, living, casting }], load, haul, overflow: { fill, owed }, asks:
/// [{ task, row, wanted, blocks }] }`.
/// Written every tick and read by
/// `observe.mjs quotas`; silence writes `rows: []` and `target 0`.
let saveQuotas (home: string) (quotas: Quotas) =
    let o = createEmpty<obj>
    o?target <- quotas.Target
    o?living <- quotas.Living
    o?casting <- quotas.Casting

    o?rows <-
        quotas.Rows
        |> List.map (fun row ->
            let r = createEmpty<obj>
            r?row <- row.Row
            r?quota <- row.Quota
            r?living <- row.Living
            r?casting <- row.Casting
            r)
        |> List.toArray

    o?load <- quotas.HaulerLoad

    o?haul <-
        quotas.HaulerDemand
        |> List.map (fun row ->
            let r = createEmpty<obj>
            r?room <- row.Container.Room
            r?x <- row.Container.X
            r?y <- row.Container.Y
            r?output <- row.Output
            r?demand <- row.Demand

            r?sinks <-
                row.Sinks
                |> List.map (fun sink ->
                    let k = createEmpty<obj>
                    k?kind <- sink.Kind

                    k?trip <-
                        (match sink.Trip with
                         | Some t -> box t
                         | None -> null)

                    k)
                |> List.toArray

            r)
        |> List.toArray

    // The haul overflow reading beside the haul lines (#508).
    let overflow = createEmpty<obj>
    overflow?fill <- quotas.Overflow.Fill
    overflow?owed <- quotas.Overflow.Owed
    o?overflow <- overflow

    // The fight asks this colony casts for, the Caster's share (#580).
    o?asks <-
        quotas.Casts
        |> List.map (fun cast ->
            let a = createEmpty<obj>
            a?task <- Fabot.Core.Decide.Facts.taskId cast.For

            a?row <-
                match cast.Ask.Row with
                | CastRow.Fighter BodyTemplate.Melee -> "guard"
                | CastRow.Fighter BodyTemplate.Ranged -> "ranger"
                | CastRow.Defender -> "defender"
                | CastRow.Squad Sapper -> "sapper"
                | CastRow.Squad Probe -> "probe"
                | CastRow.Squad Medic -> "medic"

            a?wanted <- cast.Ask.Wanted
            a?blocks <- cast.Ask.Blocks
            a)
        |> List.toArray

    writeColonyLeaf home "quotas" o

/// Write one colony's losses this tick under `observe.colonies.<home>.layout`:
/// six lists in one leaf, written every tick, empty lists included, so
/// `observe.mjs layout` can tell "nothing is lost" from "not recorded".
let saveLayout
    (home: string)
    (unserved: UnservedFooting list)
    (unrouted: UnroutedTrunk list)
    (deferred: DeferredContainer list)
    (refused: RefusedDeclaration list)
    (waiting: WaitingContainer list)
    (roads: OutpostRoad list)
    =
    let layout = createEmpty<obj>

    layout?unserved <-
        unserved
        |> List.map (fun footing ->
            let o = createEmpty<obj>
            o?x <- footing.Target.X
            o?y <- footing.Target.Y
            o?kind <- footingKindName footing.Kind
            o)
        |> List.toArray

    // The goal's spawn rides beside its name rather than inside it: a row
    // whose goal is the Upgrade Work Area carries no `spawn` key at all,
    // and a `spawn` row without one decodes to nothing rather than to
    // some other goal.
    layout?unrouted <-
        unrouted
        |> List.map (fun trunk ->
            let o = createEmpty<obj>
            o?source <- trunk.Source
            o?goal <- trunkGoalName trunk.Goal

            match trunkGoalSpawn trunk.Goal with
            | Some spawn -> o?spawn <- spawn
            | None -> ()

            o)
        |> List.toArray

    // The two tiles ride as objects rather than four flat keys: `pick`
    // and `serving` say which is which where `x2` would not. The target's
    // source rides beside its name; a `controller` row carries none.
    layout?deferred <-
        deferred
        |> List.map (fun entry ->
            let o = createEmpty<obj>
            o?target <- containerTargetName entry.Target

            match containerTargetSource entry.Target with
            | Some source -> o?source <- source
            | None -> ()

            o?pick <- tileObject entry.Pick
            o?serving <- tileObject entry.Serving
            o)
        |> List.toArray

    // The room and the kind it was declared as: the fix is a human moving the
    // declaration, and the kind says which list to move it in.
    layout?refused <-
        refused
        |> List.map (fun entry ->
            let o = createEmpty<obj>
            o?room <- entry.RoomName
            o?kind <- declarationKindName entry.Kind
            o)
        |> List.toArray

    // The outpost room rides once per row, every Seat of a source being in
    // its room; `ours` and `rival` say whose site holds each Seat (#247).
    let seatObject (seat: Pos) =
        let o = createEmpty<obj>
        o?x <- seat.X
        o?y <- seat.Y
        o

    layout?waiting <-
        waiting
        |> List.map (fun entry ->
            let o = createEmpty<obj>
            o?source <- entry.Source
            o?room <- entry.Room
            o?ours <- entry.Ours |> List.map seatObject |> List.toArray
            o?rival <- entry.Rival |> List.map seatObject |> List.toArray
            o)
        |> List.toArray

    // Each outpost source's road: its state, and the path's tiles counted by
    // what stands on them.
    layout?roads <-
        roads
        |> List.map (fun road ->
            let o = createEmpty<obj>
            o?source <- road.Source
            o?room <- road.Room
            o?state <- outpostRoadStateName road.State
            o?tiles <- road.Tiles
            o?standing <- road.Standing
            o?pending <- road.Pending
            o?skipped <- road.Skipped
            o)
        |> List.toArray

    writeColonyLeaf home "layout" layout

// A breach kind on the wire, spelled once in both directions over a closed
// set: a kind added without its wire name fails to compile or to decode,
// never prints as a blank.
let private breachKindName =
    function
    | BreachKind.LightStarved -> "light-starved"
    | BreachKind.BucketFalling -> "bucket-falling"
    | BreachKind.Fault -> "fault"
    | BreachKind.SpawnIdle -> "spawn-idle"
    | BreachKind.BodyIdle -> "body-idle"
    | BreachKind.RefusalStorm -> "refusal-storm"
    | BreachKind.OreOnTheFloor -> "ore-on-the-floor"
    | BreachKind.OreUnplaceable -> "ore-unplaceable"
    | BreachKind.ReactorRunningDry -> "reactor-running-dry"
    | BreachKind.ReactorStarved -> "reactor-starved"
    | BreachKind.ReactorLost -> "reactor-lost"
    | BreachKind.IncomingNuke -> "incoming-nuke"

let private breachKindOf name =
    match name with
    | "incoming-nuke" -> Some BreachKind.IncomingNuke
    | "light-starved" -> Some BreachKind.LightStarved
    | "bucket-falling" -> Some BreachKind.BucketFalling
    | "fault" -> Some BreachKind.Fault
    | "spawn-idle" -> Some BreachKind.SpawnIdle
    | "body-idle" -> Some BreachKind.BodyIdle
    | "refusal-storm" -> Some BreachKind.RefusalStorm
    | "ore-on-the-floor" -> Some BreachKind.OreOnTheFloor
    | "ore-unplaceable" -> Some BreachKind.OreUnplaceable
    | "reactor-running-dry" -> Some BreachKind.ReactorRunningDry
    | "reactor-starved" -> Some BreachKind.ReactorStarved
    | "reactor-lost" -> Some BreachKind.ReactorLost
    | _ -> None

// An incoming nuke's impact on the wire (#657): its centre, landing tick and
// launch room, each structure inside the 5×5 priced, and our creeps in the
// room.
let private encodeImpact (impact: NukeImpact) =
    let o = createEmpty<obj>
    o?x <- impact.GroundZero.X
    o?y <- impact.GroundZero.Y
    o?lands <- impact.Lands
    o?launch <- impact.Launcher

    o?exposed <-
        impact.Priced
        |> List.map (fun row ->
            let e = createEmpty<obj>
            e?id <- row.Exposed.Structure
            e?kind <- row.Exposed.Built
            e?x <- row.Exposed.Square.X
            e?y <- row.Exposed.Square.Y
            e?hits <- row.Exposed.HitsLeft
            e?blast <- row.Blast
            e?rampart <- row.Rampart
            e?shortfall <- row.Shortfall
            e)
        |> List.toArray

    o?creeps <- List.toArray impact.Killed
    o

// The impact back off the wire; a throw for any key off its shape, which
// costs the row (`rowsOf`), and the next full tick writes it back.
let private decodeImpact (raw: obj) : NukeImpact =
    let text (o: obj) (key: string) =
        if jsTypeof o?(key) = "string" then
            unbox<string> o?(key)
        else
            failwith ("not a string: " + key)

    let array (key: string) =
        if JS.Constructors.Array.isArray raw?(key) then
            unbox<obj[]> raw?(key) |> Array.toList
        else
            failwith ("not an array: " + key)

    {
        GroundZero =
            {
                X = numberOf raw "x"
                Y = numberOf raw "y"
            }
        Lands = numberOf raw "lands"
        Launcher = text raw "launch"
        Priced =
            array "exposed"
            |> List.map (fun e ->
                {
                    Exposed =
                        {
                            Structure = text e "id"
                            Built = text e "kind"
                            Square =
                                {
                                    X = numberOf e "x"
                                    Y = numberOf e "y"
                                }
                            HitsLeft = numberOf e "hits"
                        }
                    Blast = numberOf e "blast"
                    Rampart = numberOf e "rampart"
                    Shortfall = numberOf e "shortfall"
                })
        Killed =
            array "creeps"
            |> List.map (fun name ->
                if jsTypeof name = "string" then
                    unbox<string> name
                else
                    failwith "not a string: creeps")
    }

// One standing breach on the wire. Both ticks ride, though `last` is always
// this tick: it lets a reader with no game clock compute an age and tell
// whether the bundle that wrote the row is still running. `window` rides only
// on a row that has one (#630), and `nuke` only on an incoming nuke's (#657),
// so a row with neither keeps its old wire.
let private encodeBreach (row: StandingBreach) =
    let o = createEmpty<obj>
    o?kind <- breachKindName row.Breach.Kind
    o?room <- row.Breach.Room
    o?subject <- row.Breach.Subject
    o?amount <- row.Breach.Amount
    o?first <- row.FirstSeen
    o?last <- row.LastSeen

    if row.Breach.Window > 0 then
        o?window <- row.Breach.Window

    row.Breach.Impact |> Option.iter (fun impact -> o?nuke <- encodeImpact impact)

    o

// A checker rather than a cast: a `first` that is not a number would date a
// breach to the epoch. A dropped row is one tick of silence here, since the
// next tick re-reads the projection and writes every live breach back.
let private decodeBreach (raw: obj) : StandingBreach option =
    if isNull raw || jsTypeof raw <> "object" then
        None
    else
        match breachKindOf (string raw?kind) with
        | None -> None
        | Some kind ->
            if
                jsTypeof raw?room <> "string"
                || jsTypeof raw?subject <> "string"
                || jsTypeof raw?amount <> "number"
                || jsTypeof raw?first <> "number"
                || jsTypeof raw?last <> "number"
                || not (isNull raw?window || jsTypeof raw?window = "number")
            then
                None
            else
                Some
                    {
                        FirstSeen = unbox<int> raw?first
                        LastSeen = unbox<int> raw?last
                        Breach =
                            {
                                Kind = kind
                                Room = unbox<string> raw?room
                                Subject = unbox<string> raw?subject
                                Amount = unbox<int> raw?amount
                                Window = if isNull raw?window then 0 else unbox<int> raw?window
                                Impact =
                                    if isNull raw?nuke then
                                        None
                                    else
                                        Some(decodeImpact raw?nuke)
                            }
                    }

// A breach log off its wire leaf.
let private breachesOf (breaches: obj) : BreachState =
    {
        Standing =
            breaches?rows
            |> rowsOf decodeBreach
            |> List.map (fun row -> (row.Breach.Kind, row.Breach.Subject), row)
            |> Map.ofList
    }

// A breach log on the wire, rows in `breachRows`' order, so the ordering is
// Core's.
let private breachLeaf (state: BreachState) : obj =
    let breaches = createEmpty<obj>
    breaches?rows <- state |> breachRows |> List.map encodeBreach |> List.toArray
    breaches

/// The named colony's prior breach log, or empty when its subtree is absent
/// or unreadable. A discarded log costs the ages and nothing else: every live
/// breach is re-read off this tick's view.
let loadBreaches (home: string) : BreachState =
    leafOr BreachState.empty (fun () -> colonyLeaf home "breaches") breachesOf

/// Write one colony's breach log back under
/// `Memory.fabot.observe.colonies.<home>.breaches`, every tick, empty list
/// included, so `observe.mjs breaches` can tell "no channel" from "nothing is
/// broken".
let saveBreaches (home: string) (state: BreachState) =
    writeColonyLeaf home "breaches" (breachLeaf state)

/// The world's breach log at `Memory.fabot.observe.breaches` (#630): the rows
/// no one colony owns, on the colony log's wire.
let loadWorldBreaches () : BreachState =
    leafOr BreachState.empty (fun () -> observeLeaf "breaches") breachesOf

/// Write the world's breach log back, every full tick, empty list included.
let saveWorldBreaches (state: BreachState) =
    writeObserveLeaf "breaches" (breachLeaf state)

/// The keys a coarse span was written under before #538, in the order
/// `encodeCpuSpan` now writes its cells.
let private cpuSpanKeys =
    [|
        "f"
        "t"
        "n"
        "max"
        "sum"
        "b"
        "r"
        "p"
        "h"
        "w"
        "hmin"
        "x"
        "ss"
        "sd"
        "sv"
        "sx"
    |]

/// The keys one CPU row's phase group is written under, each beside the
/// reader that answers it. One list, so the guard that admits a group and the
/// encoder that writes one cannot come to disagree about what "all six" is.
let private cpuPhaseFields: (string * (CpuPhases -> obj)) list =
    [
        "entry", (fun p -> box p.Entry)
        "snapshot", (fun p -> box p.Snapshot)
        "decide", (fun p -> box p.Decide)
        "save", (fun p -> box p.Save)
        "execute", (fun p -> box p.Execute)
        "intents", (fun p -> box p.Intents)
        "bucket", (fun p -> box p.Bucket)
        "replans", (fun p -> box p.Replans)
    ]

/// A sub-object of numbers off a CPU row, or the empty list when the row has
/// none. Decoded on its own and never folded into `cpuPhaseFields`'
/// all-or-none guard, so rows written by a bundle that did not measure the
/// split keep their phases.
let private decodeCpuSplit (raw: obj) (key: string) : (string * float) list =
    let split = raw?(key)

    if jsTypeof split <> "object" || isNull split then
        []
    else
        JS.Constructors.Object.keys split
        |> Seq.filter (fun name -> jsTypeof split?(name) = "number")
        |> Seq.map (fun name -> name, unbox<float> split?(name))
        |> List.ofSeq

/// The flood split off one CPU row: `{ home: [floods, free, pops] }`. Absent
/// or malformed is the empty list, and a colony whose triple is not three
/// numbers is left out rather than read as zeros.
let private decodeCpuFloods (raw: obj) : (string * FloodCounts) list =
    let split = raw?floods

    if jsTypeof split <> "object" || isNull split then
        []
    else
        JS.Constructors.Object.keys split
        |> Seq.choose (fun name ->
            let triple = split?(name)

            if JS.Constructors.Array.isArray triple && (unbox<obj[]> triple).Length = 3 then
                let cells = unbox<obj[]> triple

                match numberValue cells.[0], numberValue cells.[1], numberValue cells.[2] with
                | Some floods, Some free, Some pops ->
                    Some(
                        name,
                        {
                            Floods = floods
                            Free = free
                            Pops = pops
                        }
                    )
                | _ -> None
            else
                None)
        |> List.ofSeq

/// What moved each replanning colony's signature (#463): `{ home: [part] }`.
/// Absent or malformed is the empty list; a colony whose entry is not an
/// array of strings is left out.
let private decodeCpuMoved (raw: obj) : (string * string list) list =
    let split = raw?moved

    if jsTypeof split <> "object" || isNull split then
        []
    else
        JS.Constructors.Object.keys split
        |> Seq.choose (fun name ->
            let parts = split?(name)

            if
                JS.Constructors.Array.isArray parts
                && unbox<obj[]> parts |> Array.forall (fun part -> jsTypeof part = "string")
            then
                Some(name, unbox<string[]> parts |> List.ofArray)
            else
                None)
        |> List.ofSeq

/// The phase split off one CPU row, or `None` when the row carries none. All
/// or none: a half-decoded group would price a phase against a boundary that
/// was never read, while the row's `ms` still counts.
let private decodeCpuPhases (raw: obj) : CpuPhases option =
    if cpuPhaseFields |> List.forall (fun (key, _) -> jsTypeof raw?(key) = "number") then
        Some
            {
                Entry = unbox<float> raw?entry
                Snapshot = unbox<float> raw?snapshot
                Decide = unbox<float> raw?decide
                Save = unbox<float> raw?save
                Execute = unbox<float> raw?execute
                Intents = unbox<int> raw?intents
                Bucket = unbox<int> raw?bucket
                Replans = unbox<int> raw?replans
            }
    else
        None

/// Last tick's tile of every creep of ours, the fact `CreepInfo.Walking` is
/// read against. One flat leaf, rewritten every tick; a missing or
/// malformed entry reads as "did not move", the conservative answer.
let loadPositions () : Map<string, RoomPos> =
    leafOr Map.empty (fun () -> observeLeaf "positions") (fun positions ->
        if jsTypeof positions <> "object" then
            Map.empty
        else
            JS.Constructors.Object.keys positions
            |> Seq.choose (fun name ->
                let p = positions?(name)

                if
                    isNull p
                    || jsTypeof p?r <> "string"
                    || jsTypeof p?x <> "number"
                    || jsTypeof p?y <> "number"
                then
                    None
                else
                    Some(
                        name,
                        ({
                            Room = unbox<string> p?r
                            X = unbox<int> p?x
                            Y = unbox<int> p?y
                        }
                        : RoomPos)
                    ))
            |> Map.ofSeq)

let savePositions (creeps: (string * RoomPos) list) =
    creeps
    |> hashOf (fun (tile: RoomPos) ->
        let p = createEmpty<obj>
        p?r <- tile.Room
        p?x <- tile.X
        p?y <- tile.Y
        p)
    |> writeObserveLeaf "positions"

let private nonEmptyString (value: obj) =
    jsTypeof value = "string" && (unbox<string> value).Length > 0

/// The store each loaded creep's energy was drawn from and the tick it drew
/// (#530), the fact `CreepInfo.DrawnFrom` is read off: one flat leaf of
/// `{ s: id, t: tick }` rows, rewritten every full tick. A missing or
/// malformed row reads as no record, which only lets a load go to any sink.
let loadDrawn () : Map<string, DrawRecord> =
    leafOr Map.empty (fun () -> observeLeaf "drawn") (fun drawn ->
        if jsTypeof drawn <> "object" then
            Map.empty
        else
            JS.Constructors.Object.keys drawn
            |> Seq.choose (fun name ->
                let row = drawn?(name)

                if
                    isNull row
                    || jsTypeof row <> "object"
                    || not (nonEmptyString row?s)
                    || jsTypeof row?t <> "number"
                then
                    None
                else
                    Some(
                        name,
                        {
                            Store = unbox<string> row?s
                            Tick = unbox<int> row?t
                        }
                    ))
            |> Map.ofSeq)

let saveDrawn (drawn: Map<string, DrawRecord>) =
    drawn
    |> Map.toList
    |> hashOf (fun (record: DrawRecord) ->
        let row = createEmpty<obj>
        row?s <- record.Store
        row?t <- record.Tick
        row)
    |> writeObserveLeaf "drawn"

/// The two room facts the heap keeps that a global reset must not forget
/// (`Memory.fabot.observe.rooms`): who owns each rival room as last seen
/// (`RoomSighting.Rival`, #444), dark rooms included, and the homes a tower
/// of ours has stood full in (`World.Towered`, #445), and the last tick an
/// armed rival was seen in each room a non-fighter walks round
/// (`World.ArmedSeen`, #485), and every rival controller's safe-mode facts as
/// last seen (`World.RivalControllers`, #489). Beside the owners under
/// `rivals`, the world's rival latches (`Danger.Rivals`): one latch
/// and one look stride a room, whichever colony saw it.
type RoomLatches =
    {
        Rivals: Map<string, string>
        Latched: Map<string, RivalLatch>
        Towered: Set<string>
        Armed: Map<string, int>
        Controllers: Map<string, RivalController>
    }

/// One rival controller off the wire, or None for an entry that is not the
/// shape: the owner a non-empty string, every other key a number.
let private decodeRivalController (raw: obj) : RivalController option =
    if isNull raw || jsTypeof raw <> "object" || not (nonEmptyString raw?o) then
        None
    else
        try
            Some
                {
                    Owner = unbox<string> raw?o
                    Level = numberOf raw "l"
                    SafeModeUntil = numberOf raw "s"
                    SafeModeCooldownUntil = numberOf raw "c"
                    SafeModeAvailable = numberOf raw "a"
                    UpgradeBlockedUntil = numberOf raw "b"
                    TicksToDowngrade = numberOf raw "d"
                    Seen = numberOf raw "t"
                }
        with _ ->
            None

let private encodeRivalController (c: RivalController) : obj =
    let raw = createEmpty<obj>
    raw?o <- c.Owner
    raw?l <- c.Level
    raw?s <- c.SafeModeUntil
    raw?c <- c.SafeModeCooldownUntil
    raw?a <- c.SafeModeAvailable
    raw?b <- c.UpgradeBlockedUntil
    raw?d <- c.TicksToDowngrade
    raw?t <- c.Seen
    raw

/// The room latches, each half read alone: a half that is not the shape it
/// should be reads empty, and an entry that is not a non-empty string costs
/// itself and no other.
let loadRoomLatches () : RoomLatches =
    let empty =
        {
            Rivals = Map.empty
            Latched = Map.empty
            Towered = Set.empty
            Armed = Map.empty
            Controllers = Map.empty
        }

    leafOr empty (fun () -> observeLeaf "rooms") (fun rooms ->
        if jsTypeof rooms <> "object" || JS.Constructors.Array.isArray rooms then
            empty
        else
            let rivals = rooms?rivals
            let towered = rooms?towered
            let armed = rooms?armed
            let controllers = rooms?controllers

            // Each entry the owner's name, or `{ o?, since, lastLooked }`
            // for a room latched (`decodeLatch`'s rules), its owner
            // beside the latch while a sighting names one.
            let entries =
                if
                    isNull rivals
                    || jsTypeof rivals <> "object"
                    || JS.Constructors.Array.isArray rivals
                then
                    [||]
                else
                    objectEntries rivals |> Array.filter (fun (room, _) -> room.Length > 0)

            let ownerOf (raw: obj) =
                if nonEmptyString raw then
                    Some(unbox<string> raw)
                elif not (isNull raw) && jsTypeof raw = "object" && nonEmptyString raw?o then
                    Some(unbox<string> raw?o)
                else
                    None

            {
                Rivals =
                    entries
                    |> Array.choose (fun (room, raw) ->
                        ownerOf raw |> Option.map (fun owner -> room, owner))
                    |> Fresh.mapOfArray
                Latched =
                    entries
                    |> Array.choose (fun (room, raw) ->
                        if isNull raw || jsTypeof raw <> "object" || isNull raw?since then
                            None
                        else
                            try
                                Some(room, decodeLatch raw)
                            with _ ->
                                None)
                    |> Fresh.mapOfArray
                Towered =
                    if isNull towered || not (JS.Constructors.Array.isArray towered) then
                        Set.empty
                    else
                        towered
                        |> unbox<obj[]>
                        |> Array.filter nonEmptyString
                        |> Array.map unbox<string>
                        |> Fresh.setOfSeq
                // Absent on a leaf an older bundle wrote: nothing remembered.
                Armed =
                    if
                        isNull armed
                        || jsTypeof armed <> "object"
                        || JS.Constructors.Array.isArray armed
                    then
                        Map.empty
                    else
                        intMapOf armed
                        |> Map.toArray
                        |> Array.filter (fun (room, _) -> room.Length > 0)
                        |> Fresh.mapOfArray
                // Absent on a leaf an older bundle wrote: nothing remembered.
                Controllers =
                    if
                        isNull controllers
                        || jsTypeof controllers <> "object"
                        || JS.Constructors.Array.isArray controllers
                    then
                        Map.empty
                    else
                        objectEntries controllers
                        |> Array.filter (fun (room, _) -> room.Length > 0)
                        |> Array.choose (fun (room, raw) ->
                            decodeRivalController raw |> Option.map (fun c -> room, c))
                        |> Fresh.mapOfArray
            })

/// Write the room latches whole: the rivals as a room-to-owner object, each
/// latched room's entry `{ o?, since, lastLooked }`, the towers as a room-name
/// array, the armed rooms as a room-to-tick object, the rival controllers as a
/// room-to-facts object.
let saveRoomLatches (latches: RoomLatches) =
    let raw = createEmpty<obj>

    raw?rivals <-
        Seq.append (Map.keys latches.Rivals) (Map.keys latches.Latched)
        |> Seq.distinct
        |> Seq.map (fun room ->
            let owner = Map.tryFind room latches.Rivals

            room,
            match Map.tryFind room latches.Latched with
            | Some latch ->
                let entry = encodeLatch latch
                owner |> Option.iter (fun owner -> entry?o <- owner)
                entry
            | None -> box owner.Value)
        |> hashOf id

    raw?towered <- latches.Towered |> Set.toArray
    raw?armed <- latches.Armed |> Map.toSeq |> hashOf box
    raw?controllers <- latches.Controllers |> Map.toSeq |> hashOf encodeRivalController
    writeObserveLeaf "rooms" raw

/// The CPU line off its wire, or empty when the wire is absent or unreadable.
/// A row that will not decode costs that row alone: the window shortens.
let decodeCpu (wire: obj) : CpuState =
    leafOr CpuState.empty (fun () -> wire) (fun cpu ->
        {
            Ticks =
                cpu?ticks
                |> rowsOf (fun raw ->
                    // Checked rather than assumed: `unbox` is erased by Fable,
                    // so without the check a foreign row is built, not
                    // rejected, and crowds out the window.
                    if jsTypeof raw?t = "number" && jsTypeof raw?ms = "number" then
                        let floods = decodeCpuFloods raw

                        Some
                            {
                                Tick = unbox<int> raw?t
                                Ms = unbox<float> raw?ms
                                Phases = decodeCpuPhases raw
                                Colonies = decodeCpuSplit raw "colonies"
                                Rooms = decodeCpuSplit raw "rooms"
                                Projects = decodeCpuSplit raw "projects"
                                Floods = floods
                                // Read off the split while the row has one,
                                // and off `pops` once it has aged out of it.
                                Pops =
                                    if List.isEmpty floods then
                                        numberOrZero raw "pops"
                                    else
                                        FloodCounts.totalPops floods
                                HeapMb = floatOrZero raw "heap"
                                MemoRows = numberOrZero raw "rows"
                                ExternalMb = floatOrZero raw "ext"
                                // Absent on a full tick's row and on every
                                // row written before light ticks: a full tick.
                                Light = jsTypeof raw?light = "boolean" && unbox<bool> raw?light
                                // Absent on every row but a forced full tick's;
                                // `facts` and `replayed` absent on a row
                                // forced by one fact that replayed nobody, and
                                // on every row written before #549;
                                // `fightOnly` on a row no colony fought alone
                                // on; `replay` on every row written before
                                // #581.
                                Forced =
                                    if jsTypeof raw?forced = "string" then
                                        Some(
                                            {
                                                Reason = unbox<string> raw?forced
                                                Facts = max 1 (numberOrZero raw "facts")
                                                Replayed = numberOrZero raw "replayed"
                                                FightOnly = numberOrZero raw "fightOnly"
                                                ReplayMs = floatOrZero raw "replay"
                                            }
                                            : Forcing
                                        )
                                    else
                                        None
                                // Absent on every row but a replan's.
                                Moved = decodeCpuMoved raw
                                // A bare number, decoded on its own: a legacy
                                // row reads 0.0, told apart from a headless
                                // sweep by whether `rooms` is there at all.
                                SweepHead =
                                    if jsTypeof raw?head = "number" then
                                        unbox<float> raw?head
                                    else
                                        0.0
                            }
                    else
                        None)
            // The coarse spans, absent from a legacy leaf: the first tick
            // after a deploy opens the first span. A span is a positional
            // array (`encodeCpuSpan`); a keyed span, written before #538, is
            // read into the same cells, so the deploy keeps the record. Its
            // keys past `r` were added later still, and absent reads zero.
            Spans =
                cpu?spans
                |> rowsOf (fun raw ->
                    let cells: obj[] =
                        if JS.Constructors.Array.isArray raw then
                            unbox raw
                        else
                            cpuSpanKeys
                            |> Array.mapi (fun i key ->
                                if i > 6 && jsTypeof raw?(key) = "undefined" then
                                    box 0
                                else
                                    raw?(key))

                    if
                        cells.Length = cpuSpanKeys.Length
                        && cells |> Array.forall (fun cell -> jsTypeof cell = "number")
                    then
                        let cells = unbox<float[]> cells

                        Some
                            {
                                From = int cells.[0]
                                To = int cells.[1]
                                Ticks = int cells.[2]
                                Max = cells.[3]
                                Sum = cells.[4]
                                Bucket = int cells.[5]
                                Replans = int cells.[6]
                                MaxPops = int cells.[7]
                                MaxHeapMb = cells.[8]
                                MaxMemoRows = int cells.[9]
                                MinHeapMb = cells.[10]
                                MaxExternalMb = cells.[11]
                                SnapshotSum = cells.[12]
                                DecideSum = cells.[13]
                                SaveSum = cells.[14]
                                ExecuteSum = cells.[15]
                            }
                    else
                        None)
        })

/// One CPU row as the wire carries it: `{ t, ms }`, the phase keys when the
/// row was measured with them, and the three splits beside them. Rows ride as
/// `{ t, ms }` because the tick number is the half a reader cannot reconstruct.
let private encodeCpuSample (sample: CpuSample) : obj =
    let o = createEmpty<obj>
    o?t <- sample.Tick
    o?ms <- sample.Ms

    match sample.Phases with
    | Some phases ->
        for key, read in cpuPhaseFields do
            o?(key) <- read phases
    | None -> ()

    // One sub-object per split rather than a key per colony, so a home room's
    // name can never collide with a phase's and the group is absent whole on
    // a row that has none. Separate keys per split because a home room is
    // both a colony that decides and a room that is swept.
    let writeSplit key rows =
        if not (List.isEmpty rows) then
            let split = createEmpty<obj>

            for name, ms in rows do
                split?(name) <- ms

            o?(key) <- split

    writeSplit "colonies" sample.Colonies
    writeSplit "rooms" sample.Rooms
    writeSplit "projects" sample.Projects

    // The flood counts, a triple per colony, as an array rather than three
    // keys because a hundred rows pay for every character of every key.
    if not (List.isEmpty sample.Floods) then
        let split = createEmpty<obj>

        for home, counts in sample.Floods do
            // Boxed, so Fable emits a plain array and not an `Int32Array`,
            // which `JSON.stringify` — Memory's own serialiser — writes as
            // an object keyed "0", "1", "2".
            split?(home) <- [| box counts.Floods; box counts.Free; box counts.Pops |]

        o?floods <- split

    // A row aged out of `capCpuDetail` keeps the one flood number a reader
    // prints for every row.
    if List.isEmpty sample.Floods && sample.Pops > 0 then
        o?pops <- sample.Pops

    if sample.SweepHead > 0.0 then
        o?head <- sample.SweepHead

    // Written only when measured, so a legacy row re-encodes as itself. The
    // heap says whether the tick measured: a live heap is never 0, where a
    // memo with no cross-room Task and no lead cast has 0 rows.
    if sample.HeapMb > 0.0 then
        o?heap <- sample.HeapMb
        o?rows <- sample.MemoRows
        o?ext <- sample.ExternalMb

    // Written only on a light tick, so a full tick's row and a legacy row
    // re-encode as themselves.
    if sample.Light then
        o?light <- true

    match sample.Forced with
    | Some forcing ->
        o?forced <- forcing.Reason

        if forcing.Facts > 1 then
            o?facts <- forcing.Facts

        if forcing.Replayed > 0 then
            o?replayed <- forcing.Replayed

        if forcing.FightOnly > 0 then
            o?fightOnly <- forcing.FightOnly

        if forcing.ReplayMs > 0.0 then
            o?replay <- forcing.ReplayMs
    | None -> ()

    // Written only on a replan's row, as the floods are: one array per home.
    if not (List.isEmpty sample.Moved) then
        let split = createEmpty<obj>

        for home, parts in sample.Moved do
            split?(home) <- Array.ofList parts

        o?moved <- split

    o

/// One coarse span on the wire: sixteen numbers in a positional array, because
/// two hundred of these ride in the same leaf as the fine ring and their keys
/// were half of it (#538). The order is `observe.mjs`'s `SPAN_FIELDS`.
let private encodeCpuSpan (span: CpuSpan) : obj =
    // Boxed, so Fable emits a plain array and not a `Float64Array`.
    box
        [|
            box span.From
            box span.To
            box span.Ticks
            box span.Max
            box span.Sum
            box span.Bucket
            box span.Replans
            box span.MaxPops
            box span.MaxHeapMb
            box span.MaxMemoRows
            box span.MinHeapMb
            box span.MaxExternalMb
            box span.SnapshotSum
            box span.DecideSum
            box span.SaveSum
            box span.ExecuteSum
        |]

/// The stored line on a cold heap (`restoreWire`); None while its segments
/// are not active yet.
let restoreCpu () : CpuState option =
    restoreWire "cpu" cpuSegments |> Option.map decodeCpu

/// The whole line on the wire.
let encodeCpu (state: CpuState) : obj =
    let cpu = createEmpty<obj>
    cpu?ticks <- state.Ticks |> List.map encodeCpuSample |> List.toArray
    cpu?spans <- state.Spans |> List.map encodeCpuSpan |> List.toArray
    cpu

/// Set the heap wire to the whole line.
let saveCpu (state: CpuState) = cpuWire <- encodeCpu state

/// Write this tick's row onto `cpu` and no other: the newest row is pushed
/// onto its `ticks` array and, at the cap, the oldest shifted off. Encoding
/// the whole line every tick cost 1.36 ms of a live tick (#370). The append is
/// taken only when the wire agrees with the line, checked on the tick numbers
/// at both ends; anything else is encoded whole. Answers the wire to keep.
let appendCpuOnto (cpu: obj) (state: CpuState) : obj =
    let ticks = if isNull cpu then null else cpu?ticks

    match List.rev state.Ticks with
    | newest :: older when JS.Constructors.Array.isArray ticks ->
        let rows = unbox<obj[]> ticks
        let held = rows.Length
        let count = List.length older

        let tickAt index =
            let row = rows.[index]

            if isNull row || jsTypeof row?t <> "number" then
                -1
            else
                unbox<int> row?t

        let agrees =
            (held = count || held = count + 1)
            && (count = 0
                || (tickAt (held - 1) = (List.head older).Tick
                    && tickAt (held - count) = (List.head state.Ticks).Tick))

        if agrees then
            if held = count + 1 then
                emitJsStatement rows "$0.shift()"

            emitJsStatement (rows, encodeCpuSample newest) "$0.push($1)"

            // The row that aged out of `capCpuDetail` this tick is rewritten
            // bare, as the fold left it; the rows before it already are.
            let aged = count - capCpuDetail

            if aged >= 0 then
                emitJsStatement
                    (rows, aged, encodeCpuSample (List.item aged state.Ticks))
                    "$0[$1] = $2"

            // The coarse spans ride the same append: the open span is
            // rewritten every tick and a span that just opened is pushed.
            // Before this, `spans` was written by `saveCpu` alone, once per
            // upload, and reached Memory frozen at the first tick (#390). A
            // leaf that disagrees is written whole, as the rows are.
            let spans = cpu?spans

            match List.rev state.Spans with
            | open' :: closed when JS.Constructors.Array.isArray spans ->
                let rows = unbox<obj[]> spans
                let held = rows.Length
                let count = List.length closed

                // Told apart by the open span's `From`, not by length (#394):
                // at the cap the state holds `capCpuSpans` either way. A last
                // slot that is null or off the shape disagrees, like a row.
                let continues =
                    held > 0
                    && not (isNull rows.[held - 1])
                    && JS.Constructors.Array.isArray rows.[held - 1]
                    && unbox<int> rows.[held - 1]?("0") = open'.From

                if continues && held = count + 1 then
                    emitJsStatement (spans, held - 1, encodeCpuSpan open') "$0[$1] = $2"
                elif
                    not continues
                    && (held = count || (held = capCpuSpans && count = capCpuSpans - 1))
                then
                    // A span opened this tick; the one before it was last
                    // written a tick ago, closed. At the cap the fold
                    // dropped the oldest, and so does the leaf.
                    if held = capCpuSpans then
                        emitJsStatement spans "$0.shift()"

                    emitJsStatement (spans, encodeCpuSpan open') "$0.push($1)"
                else
                    cpu?spans <- state.Spans |> List.map encodeCpuSpan |> List.toArray
            | _ -> cpu?spans <- state.Spans |> List.map encodeCpuSpan |> List.toArray

            cpu
        else
            encodeCpu state
    | _ -> encodeCpu state

/// `appendCpuOnto` the heap wire.
let appendCpu (state: CpuState) = cpuWire <- appendCpuOnto cpuWire state

/// One throw the bot caught (#628): the tick, the colony it cost (empty
/// where it cost the whole tick, or an intent), the step it threw in, its
/// message and the top frames of its stack.
type FaultRow =
    {
        Tick: int
        Colony: string
        Phase: string
        Message: string
        Frames: string list
    }

/// How many faults the ring keeps, newest first.
let capFaults = 20

let private encodeFault (row: FaultRow) =
    let o = createEmpty<obj>
    o?t <- row.Tick
    o?colony <- row.Colony
    o?phase <- row.Phase
    o?message <- row.Message
    o?frames <- row.Frames |> List.toArray
    o

let private decodeFault (raw: obj) : FaultRow option =
    if
        isNull raw
        || jsTypeof raw <> "object"
        || jsTypeof raw?colony <> "string"
        || jsTypeof raw?phase <> "string"
        || jsTypeof raw?message <> "string"
        || not (JS.Constructors.Array.isArray raw?frames)
    then
        None
    else
        Some
            {
                Tick = numberOf raw "t"
                Colony = unbox<string> raw?colony
                Phase = unbox<string> raw?phase
                Message = unbox<string> raw?message
                Frames =
                    raw?frames
                    |> unbox<obj[]>
                    |> Array.filter (fun frame -> jsTypeof frame = "string")
                    |> Array.map unbox<string>
                    |> Array.toList
            }

/// The fault ring at `Memory.fabot.observe.faults`, newest first, or empty
/// when it is absent or unreadable.
let loadFaults () : FaultRow list =
    leafOr [] (fun () -> observeLeaf "faults") (fun faults -> faults?rows |> rowsOf decodeFault)

/// Write the ring back whole, capped at `capFaults`.
let saveFaults (rows: FaultRow list) =
    let faults = createEmpty<obj>
    faults?rows <- rows |> List.truncate capFaults |> List.map encodeFault |> List.toArray
    writeObserveLeaf "faults" faults

/// This tick's faults laid on the ring, newest first. Written only on a tick
/// that caught one: a clean tick pays nothing for the leaf.
let recordFaults (fresh: FaultRow list) =
    if not (List.isEmpty fresh) then
        saveFaults (fresh @ loadFaults ())
        faultedAt <- (List.head fresh).Tick

/// The live-probe tool's sums (`scripts/probe.mjs`), dropped on the tick after
/// a global reset: a clean bundle never writes them again, and every tick's
/// Memory parse paid for them after the probe was undone (#538). A probed
/// bundle writes them after this, in the wrapper around `loop`.
let dropProbe () =
    emitJsStatement Memory "delete $0.__probe"

/// The change the running bundle was built from (`scripts/bundle.mjs`) and the
/// tick it was loaded on, at `Memory.fabot.observe.version` (#634): written on
/// the tick after a global reset, read by `observe health` and `observe cpu`
/// and by nothing in the bot.
let saveVersion (build: string) (reset: int) =
    let raw = createEmpty<obj>
    raw?build <- build
    raw?reset <- reset
    writeObserveLeaf "version" raw

// ---- the rival dossier (#653) ---------------------------------------------
//
// One object keyed by rival username, read by `observe.mjs rivals` alone.
// A tally is `[count, last]`, a span `[episodes, sum, shortest, longest,
// ended]`, an aim `[open, hit]`. A key absent reads as the zero it means;
// one present and off its shape costs its rival's dossier and no more.

let private numbers (count: int) (raw: obj) : int[] =
    if not (JS.Constructors.Array.isArray raw) then
        failwith "not an array"

    let items = unbox<obj[]> raw

    if items.Length <> count then
        failwith "off its length"

    items
    |> Array.map (fun item ->
        match numberValue item with
        | Some number -> number
        | None -> failwith "not a number")

// A key a reader may find absent: the zero, else its decode.
let private orZero (zero: 'a) (decode: obj -> 'a) (raw: obj) (key: string) : 'a =
    if jsTypeof raw?(key) = "undefined" then
        zero
    else
        decode raw?(key)

let private encodeTally (tally: Tally) : obj =
    box [| box tally.Count; box tally.Last |]

let private decodeTally (raw: obj) : Tally =
    let n = numbers 2 raw
    { Count = n.[0]; Last = n.[1] }

let private encodeSpan (span: Span) : obj =
    box
        [|
            box span.Episodes
            box span.Sum
            box span.Shortest
            box span.Longest
            box span.Ended
        |]

let private decodeSpan (raw: obj) : Span =
    let n = numbers 5 raw

    {
        Episodes = n.[0]
        Sum = n.[1]
        Shortest = n.[2]
        Longest = n.[3]
        Ended = n.[4]
    }

let private encodeAim (aim: Aim) : obj = box [| box aim.Open; box aim.Hit |]

let private decodeAim (raw: obj) : Aim =
    let n = numbers 2 raw
    { Open = n.[0]; Hit = n.[1] }

let private tallyAt = orZero Tally.zero decodeTally
let private spanAt = orZero Span.zero decodeSpan
let private aimAt = orZero Aim.zero decodeAim

let private encodeRivalRoom (row: RivalRoom) : obj =
    let o = createEmpty<obj>
    o?seen <- row.Seen
    o?res <- row.Reserving
    o?tReads <- row.TowerReads
    o?tLow <- row.TowerLow
    o?tLast <- row.TowerLast
    o?refills <- encodeTally row.Refills
    o?fireGain <- row.FireGain
    o?fireTicks <- row.FireTicks
    o?peaceGain <- row.PeaceGain
    o?peaceTicks <- row.PeaceTicks
    o?lowFirst <- encodeAim row.LowestFirst
    o?resp <- encodeSpan row.Response
    o?unans <- encodeTally row.Unanswered
    o?standing <- encodeTally row.Standing
    o?killed <- row.KilledAt
    o?unworked <- encodeSpan row.Unworked
    o?lapsed <- row.LapsedAt
    o?rereserve <- encodeSpan row.Rereserved
    o

let private decodeRivalRoom (raw: obj) : RivalRoom =
    {
        Seen = numberOf raw "seen"
        Reserving =
            match jsTypeof raw?res with
            | "undefined" -> false
            | "boolean" -> unbox<bool> raw?res
            | _ -> failwith "not a boolean: res"
        TowerReads = numberOrZero raw "tReads"
        TowerLow = numberOrZero raw "tLow"
        TowerLast = numberOrZero raw "tLast"
        Refills = tallyAt raw "refills"
        FireGain = floatOrZero raw "fireGain"
        FireTicks = floatOrZero raw "fireTicks"
        PeaceGain = floatOrZero raw "peaceGain"
        PeaceTicks = floatOrZero raw "peaceTicks"
        LowestFirst = aimAt raw "lowFirst"
        Response = spanAt raw "resp"
        Unanswered = tallyAt raw "unans"
        Standing = tallyAt raw "standing"
        KilledAt = numberOrZero raw "killed"
        Unworked = spanAt raw "unworked"
        LapsedAt = numberOrZero raw "lapsed"
        Rereserved = spanAt raw "rereserve"
    }

let private encodeActivation (seen: SafeModeSeen) : obj =
    let o = createEmpty<obj>
    o?room <- seen.Room
    o?at <- seen.At
    o?noticed <- seen.Noticed
    o?rcl <- seen.Level
    o?ours <- seen.Ours
    o?armed <- seen.OursArmed
    seen.Since |> Option.iter (fun since -> o?since <- since)
    o?struck <- hashOf box seen.Struck
    o

let private decodeActivation (raw: obj) : SafeModeSeen option =
    if isNull raw || jsTypeof raw?room <> "string" then
        None
    else
        Some
            {
                Room = unbox<string> raw?room
                At = numberOf raw "at"
                Noticed = numberOf raw "noticed"
                Level = numberOrZero raw "rcl"
                Ours = numberOrZero raw "ours"
                OursArmed = numberOrZero raw "armed"
                Since = numberValue raw?since
                Struck = intMapOf raw?struck |> Map.toList
            }

let private encodeDossier (dossier: Dossier) : obj =
    let aim = createEmpty<obj>
    aim?shots <- encodeTally dossier.Aim.Shots
    aim?alone <- dossier.Aim.Alone
    aim?near <- encodeAim dossier.Aim.Nearest
    aim?weak <- encodeAim dossier.Aim.Weakest
    aim?heal <- encodeAim dossier.Aim.Healer
    aim?armed <- encodeAim dossier.Aim.Attacker
    aim?edge <- encodeAim dossier.Aim.Edge
    let o = createEmpty<obj>
    o?since <- dossier.Since
    o?latest <- dossier.Latest
    o?aim <- aim
    o?safe <- encodeTally dossier.SafeModes
    o?acts <- dossier.Activations |> List.map encodeActivation |> List.toArray
    o?cool <- dossier.CooldownUntil
    o?armed <- encodeTally dossier.Armed
    o?shapes <- hashOf box (Map.toSeq dossier.Shapes)
    o?cross <- encodeTally dossier.Crossings
    o?sortie <- encodeTally dossier.Sorties
    o?stayed <- encodeTally dossier.Stayed
    o?fled <- encodeTally dossier.Fled
    o?fell <- encodeTally dossier.Fell

    o?homeRaid <-
        dossier.HomeRaiders
        |> Map.toSeq
        |> Seq.map (fun (level, tally) -> string level, tally)
        |> hashOf encodeTally

    o?outRaid <- encodeTally dossier.OutpostRaiders
    o?rooms <- dossier.Rooms |> Map.toSeq |> hashOf encodeRivalRoom
    o

let private decodeDossier (raw: obj) : Dossier =
    let aim = if isNull raw?aim then createEmpty<obj> else raw?aim

    let entries (key: string) =
        if isNull raw?(key) || jsTypeof raw?(key) <> "object" then
            [||]
        else
            objectEntries raw?(key)

    {
        Since = numberOf raw "since"
        Latest = numberOf raw "latest"
        Aim =
            {
                Shots = tallyAt aim "shots"
                Alone = numberOrZero aim "alone"
                Nearest = aimAt aim "near"
                Weakest = aimAt aim "weak"
                Healer = aimAt aim "heal"
                Attacker = aimAt aim "armed"
                Edge = aimAt aim "edge"
            }
        SafeModes = tallyAt raw "safe"
        Activations = rowsOf decodeActivation raw?acts |> List.truncate capActivations
        CooldownUntil = numberOrZero raw "cool"
        Armed = tallyAt raw "armed"
        Shapes = intMapOf raw?shapes |> Map.toSeq |> Fresh.mapOfSeq
        Crossings = tallyAt raw "cross"
        Sorties = tallyAt raw "sortie"
        Stayed = tallyAt raw "stayed"
        Fled = tallyAt raw "fled"
        Fell = tallyAt raw "fell"
        HomeRaiders =
            entries "homeRaid"
            |> Array.map (fun (level, tally) ->
                match System.Int32.TryParse level with
                | true, number -> number, decodeTally tally
                | _ -> failwith ("not a level: " + level))
            |> Fresh.mapOfArray
        OutpostRaiders = tallyAt raw "outRaid"
        Rooms =
            entries "rooms"
            |> Array.map (fun (room, row) -> room, decodeRivalRoom row)
            |> Fresh.mapOfArray
    }

/// The rival dossiers off their wire, or none when it is absent or
/// unreadable; a rival's that will not decode costs that rival alone.
let decodeRivals (wire: obj) : Map<string, Dossier> =
    leafOr (Fresh.mapOfSeq Seq.empty) (fun () -> wire) (fun rivals ->
        if jsTypeof rivals <> "object" || JS.Constructors.Array.isArray rivals then
            Fresh.mapOfSeq Seq.empty
        else
            objectEntries rivals
            |> Array.choose (fun (owner, raw) ->
                try
                    if isNull raw then None else Some(owner, decodeDossier raw)
                with _ ->
                    None)
            |> Fresh.mapOfArray)

/// Every rival's dossier on the wire.
let encodeRivals (dossiers: Map<string, Dossier>) : obj =
    dossiers |> Map.toSeq |> hashOf encodeDossier

/// The stored dossiers on a cold heap (`restoreWire`), the fold's heap state
/// from nothing; None while their segment is not active yet.
let restoreRivals () : Rivals option =
    restoreWire "rivals" rivalSegments
    |> Option.map (decodeRivals >> Rivals.ofDossiers)

/// Hold the dossiers on the heap for the next segment write.
let saveRivals (rivals: Rivals) = rivalsHeld <- Some rivals.Dossiers

// ---- the operation ledger (#662) ------------------------------------------

/// Each operation kind's wire name, both ways.
let private kindName =
    function
    | Ledger.OperationKind.Sweep -> "sweep"
    | Ledger.OperationKind.Camp -> "camp"
    | Ledger.OperationKind.Trespass -> "trespass"
    | Ledger.OperationKind.Hold -> "hold"

let private kindOf =
    reverseOf
        kindName
        [
            Ledger.OperationKind.Sweep
            Ledger.OperationKind.Camp
            Ledger.OperationKind.Trespass
            Ledger.OperationKind.Hold
        ]

// A fraction to the hundredth: the wire's, read back as written.
let private hundredths (value: float) : float = System.Math.Round(value, 2)

// A row's counts ride arrays, each read whole or as zeros where absent; one
// present and off its shape costs the row.
let private countsAt (raw: obj) (key: string) (length: int) : float[] =
    let value = raw?(key)

    if jsTypeof value = "undefined" then
        Array.zeroCreate length
    elif
        JS.Constructors.Array.isArray value
        && (unbox<obj[]> value).Length = length
        && unbox<obj[]> value |> Array.forall (fun n -> jsTypeof n = "number")
    then
        unbox<float[]> value
    else
        failwith ("not counts: " + key)

let private encodeRow (row: Ledger.Row) : obj =
    let o = createEmpty<obj>
    o?k <- kindName row.Key.Kind
    o?r <- row.Key.Room
    o?s <- row.Key.Start
    o?l <- row.Last
    o?act <- row.Active
    o?lk <- row.Looked
    row.Ended |> Option.iter (fun ended -> o?e <- ended)
    let p = createEmpty<obj>
    row.Predicted.Rate |> Option.iter (fun rate -> p?rate <- hundredths rate)
    row.Predicted.Value |> Option.iter (fun value -> p?value <- hundredths value)
    row.Predicted.Odds |> Option.iter (fun odds -> p?odds <- hundredths odds)
    o?p <- p
    o?cast <- [| box row.Cast; box row.CastEnergy; box row.SpawnTicks |]
    o?join <- row.Joined
    o?lostCast <- [| box row.CastLost; box row.CastLostCost; box (hundredths row.CastLostLife) |]

    o?lostLent <- [| box row.LentLost; box row.LentLostCost; box (hundredths row.LentLostLife) |]

    o?forced <- [| box row.Forced; box (hundredths row.ForcedCpu) |]
    o?kills <- [| box row.Kills; box (hundredths row.KillValue) |]
    o?camp <- [| box row.CampKills; box (hundredths row.CampValue) |]
    o?down <- [| box row.Downtime; box (hundredths row.DowntimeValue) |]
    o?ans <- [| box row.Answered; box row.AnsweredCost |]
    row.Response |> Option.iter (fun ticks -> o?resp <- ticks)

    if not row.Roster.IsEmpty then
        o?roster <-
            row.Roster
            |> Map.toSeq
            |> hashOf (fun (body: Ledger.Enlisted) ->
                box
                    [|
                        box body.Cost
                        box body.Life
                        box body.Seen
                        box (if body.Holds then 1 else 0)
                        box (if body.Cast then 1 else 0)
                    |])

    if not row.Marks.IsEmpty then
        o?marks <- row.Marks |> Map.toSeq |> hashOf (hundredths >> box)

    if not row.Camped.IsEmpty then
        o?camped <- row.Camped |> Set.toArray

    if not row.Answers.IsEmpty then
        o?answers <-
            row.Answers
            |> Map.toSeq
            |> hashOf (fun (body: Ledger.Armed) -> box [| box body.Began; box body.Cost |])

    row.AnswerFloor |> Option.iter (fun floor -> o?afloor <- floor)
    row.RosterFloor |> Option.iter (fun floor -> o?rfloor <- floor)

    o

let private decodeRow (raw: obj) : Ledger.Row option =
    if isNull raw || jsTypeof raw?r <> "string" || jsTypeof raw?k <> "string" then
        None
    else
        let p = raw?p

        if isNull p || jsTypeof p <> "object" then
            failwith "no prediction"

        let cast = countsAt raw "cast" 3
        let lostCast = countsAt raw "lostCast" 3
        let lostLent = countsAt raw "lostLent" 3
        let forced = countsAt raw "forced" 2
        let kills = countsAt raw "kills" 2
        let camp = countsAt raw "camp" 2
        let last = numberOf raw "l"
        let down = countsAt raw "down" 2
        let ans = countsAt raw "ans" 2

        let entries (key: string) =
            if isNull raw?(key) || jsTypeof raw?(key) <> "object" then
                [||]
            else
                objectEntries raw?(key)

        let numbers (length: int) (value: obj) =
            JS.Constructors.Array.isArray value
            && (unbox<obj[]> value).Length = length
            && unbox<obj[]> value |> Array.forall (fun n -> jsTypeof n = "number")

        // A later key absent reads as the last fold's tick.
        let tickOr (key: string) =
            if jsTypeof raw?(key) = "undefined" then
                last
            else
                numberOf raw key

        // A price absent is one the decide did not read; present, it must.
        let priceOf (key: string) =
            if jsTypeof p?(key) = "undefined" then
                None
            else
                Some(floatOf p key)

        // A floor absent is none risen; present, it must read.
        let floorOf (key: string) =
            if jsTypeof raw?(key) = "undefined" then
                None
            else
                Some(numberOf raw key)

        let ids (key: string) =
            if JS.Constructors.Array.isArray raw?(key) then
                unbox<obj[]> raw?(key)
                |> Array.choose (fun id ->
                    if jsTypeof id = "string" then
                        Some(unbox<string> id)
                    else
                        None)
                |> Fresh.setOfSeq
            else
                Fresh.setOfSeq Seq.empty

        let enlisted (name: string, value: obj) =
            if numbers 5 value then
                let n = unbox<int[]> value

                Some(
                    name,
                    ({
                        Cost = n.[0]
                        Life = n.[1]
                        Seen = n.[2]
                        Holds = n.[3] = 1
                        Cast = n.[4] = 1
                    }
                    : Ledger.Enlisted)
                )
            else
                None

        Some
            {
                Key =
                    {
                        Kind =
                            match kindOf (unbox<string> raw?k) with
                            | Some kind -> kind
                            | None -> failwith ("not a kind: " + unbox<string> raw?k)
                        Room = unbox<string> raw?r
                        Start = numberOf raw "s"
                    }
                Last = last
                Active = tickOr "act"
                Looked = tickOr "lk"
                Ended = numberValue raw?e
                Predicted =
                    {
                        Rate = priceOf "rate"
                        Value = priceOf "value"
                        Odds = priceOf "odds"
                    }
                Cast = int cast.[0]
                CastEnergy = int cast.[1]
                SpawnTicks = int cast.[2]
                Joined = numberOrZero raw "join"
                CastLost = int lostCast.[0]
                CastLostCost = int lostCast.[1]
                CastLostLife = lostCast.[2]
                LentLost = int lostLent.[0]
                LentLostCost = int lostLent.[1]
                LentLostLife = lostLent.[2]
                Forced = int forced.[0]
                ForcedCpu = forced.[1]
                Kills = int kills.[0]
                KillValue = kills.[1]
                CampKills = int camp.[0]
                CampValue = camp.[1]
                Downtime = int down.[0]
                DowntimeValue = down.[1]
                Answered = int ans.[0]
                AnsweredCost = int ans.[1]
                Response = numberValue raw?resp
                Roster = entries "roster" |> Array.choose enlisted |> Fresh.mapOfArray
                RosterFloor = floorOf "rfloor"
                Marks =
                    entries "marks"
                    |> Array.choose (fun (id, value) ->
                        if jsTypeof value = "number" then
                            Some(id, unbox<float> value)
                        else
                            None)
                    |> Fresh.mapOfArray
                Camped = ids "camped"
                Answers =
                    entries "answers"
                    |> Array.choose (fun (id, value) ->
                        if numbers 2 value then
                            let n = unbox<int[]> value
                            Some(id, ({ Began = n.[0]; Cost = n.[1] }: Ledger.Armed))
                        else
                            None)
                    |> Fresh.mapOfArray
                AnswerFloor = floorOf "afloor"
            }

/// The ledger off its wire, or empty when the wire is absent or unreadable;
/// a row that will not decode costs that row alone.
let decodeLedger (wire: obj) : Ledger.Ledger =
    leafOr Ledger.empty (fun () -> wire) (fun raw ->
        if jsTypeof raw <> "object" || JS.Constructors.Array.isArray raw then
            Ledger.empty
        else
            {
                Open = rowsOf decodeRow raw?``open`` |> List.truncate Ledger.capOpen
                Closed = rowsOf decodeRow raw?closed |> List.truncate Ledger.capClosed
                Evicted = numberOrZero raw "evicted"
            })

/// The ledger on the wire: the open rows, oldest first, then the closed,
/// newest first, and the evictions.
let encodeLedger (ledger: Ledger.Ledger) : obj =
    let o = createEmpty<obj>
    o?``open`` <- ledger.Open |> List.map encodeRow |> List.toArray
    o?closed <- ledger.Closed |> List.map encodeRow |> List.toArray
    o?evicted <- ledger.Evicted
    o

/// The stored ledger on a cold heap (`restoreWire`), the fold's state from
/// nothing; None while its segment is not active yet.
let restoreLedger () : Ledger.Ledger option =
    restoreWire "ledger" ledgerSegments |> Option.map decodeLedger

/// Hold the ledger on the heap for the next segment write.
let saveLedger (ledger: Ledger.Ledger) = ledgerHeld <- Some ledger

// A key whose value is read only when asked for: the harness asks for the
// CPU line every tick, and the ledger's wire once.
[<Emit("Object.defineProperty($0, $1, { get: $2, enumerable: true })")>]
let private lazyKey (_target: obj) (_key: string) (_read: unit -> obj) : unit = jsNative

/// The logs' wires as the heap holds them, for the console and the profile
/// harness (`__fabot.wire()`); read by nothing in the loop. The ledger's is
/// encoded only when read.
let wires () : obj =
    let o = createObj [ "creeps" ==> creepsWire; "cpu" ==> cpuWire ]
    lazyKey o "ledger" (fun () -> ledgerHeld |> Option.map encodeLedger |> Option.toObj)
    o

/// A log's JSON cut into `count` chunks in order, each within the segment cap
/// and never between the two halves of a surrogate pair, the unused ones
/// empty; None when it does not fit.
let segmentChunks (count: int) (text: string) : string[] option =
    let chunks = Array.create count ""
    let mutable from = 0

    for i in 0 .. count - 1 do
        let mutable until = min text.Length (from + segmentCap)

        // A cut after a high surrogate would leave each segment half a
        // character; one code unit earlier keeps the pair together.
        if
            until < text.Length
            && (let unit = int text.[until - 1] in unit >= 0xD800 && unit <= 0xDBFF)
        then
            until <- until - 1

        chunks.[i] <- text.Substring(from, until - from)
        from <- until

    if from < text.Length then None else Some chunks

/// Write the two logs' wire out to their segments on every `segmentEvery`th
/// tick, on a tick that recorded a fault and on the tick a legacy leaf was
/// migrated, the unused segments emptied, so no stale chunk follows the JSON.
/// A log not restored yet is not written; one past its segments' room is not
/// written either, and is answered as a fault, once a heap, for the shell's
/// ring. A write needs no active segment, but a cold heap's read does, so
/// they are requested again whenever they are not active.
let flushSegments (tick: int) : FaultRow list =
    if not (segmentsVisible ()) then
        requestSegments ()

    if tick % segmentEvery = 0 || faultedAt = tick || migrated then
        migrated <- false

        [
            "creeps", creepSegments, creepsWire
            "cpu", cpuSegments, cpuWire
            "rivals", rivalSegments, (rivalsHeld |> Option.map encodeRivals |> Option.toObj)
            "ledger", ledgerSegments, (ledgerHeld |> Option.map encodeLedger |> Option.toObj)
        ]
        |> List.filter (fun (_, _, wire) -> not (isNull wire))
        |> List.choose (fun (leaf, ids, wire) ->
            let text: string = JS.JSON.stringify wire

            match segmentChunks ids.Length text with
            | Some chunks ->
                ids |> Array.iteri (fun i id -> RawMemory?segments?(string id) <- chunks.[i])

                None
            | None when Set.contains leaf oversized -> None
            | None ->
                oversized <- Fresh.setOfSeq (Seq.append oversized [ leaf ])

                Some
                    {
                        Tick = tick
                        Colony = ""
                        Phase = "segments"
                        Message =
                            $"the {leaf} log is {text.Length} long, past its {ids.Length} segments"
                        Frames = []
                    })
    else
        []

// ---- the operations (#663 S7a) ----------------------------------------------
//
// `Memory.fabot.ops`: `specs` and `off` are the operator's (`observe op`),
// `live`, `refused` and `events` the bot's. An absent leaf, or one that is
// not an object, is no operations: nothing is read and nothing is written,
// so a Memory with no ops leaf is today's Memory byte for byte.

/// The ops leaf as read: every spec row that decodes, a refusal for each
/// that does not, every record that decodes, and the kill switch.
type OpsLeaf =
    {
        Specs: OpSpecWire list
        Garbled: OpRefusal list
        Live: OpRecord list
        Off: bool
    }

let private phaseNames =
    [
        OpPhase.Muster, "muster"
        OpPhase.Approach, "approach"
        OpPhase.Execute, "execute"
        OpPhase.Hold, "hold"
        OpPhase.Retreat, "retreat"
    ]

let private endNames =
    [
        OpEnd.Done, "done"
        OpEnd.Abandoned, "abandoned"
        OpEnd.Expired, "expired"
        OpEnd.Budget, "budget"
    ]

let private nameIn (names: ('a * string) list) (value: 'a) : string =
    names |> List.find (fun (case, _) -> case = value) |> snd

let private caseIn (names: ('a * string) list) (name: obj) : 'a option =
    names |> List.tryFind (fun (_, spelt) -> box spelt = name) |> Option.map fst

let private ops () : obj =
    let fabot = Memory?fabot

    if
        isNull fabot
        || jsTypeof fabot?ops <> "object"
        || isNull fabot?ops
        || JS.Constructors.Array.isArray fabot?ops
    then
        null
    else
        fabot?ops

// A non-empty string off the wire, and a throw for anything else.
let private textOf (raw: obj) (key: string) : string =
    if jsTypeof raw?(key) = "string" && (unbox<string> raw?(key)).Length > 0 then
        unbox<string> raw?(key)
    else
        failwith ("not a name: " + key)

// A key absent or null reads as None, a whole number a 32-bit integer holds
// as itself (so .NET and JavaScript read the same arithmetic off it);
// anything else throws.
let private numberOption (raw: obj) (key: string) : int option =
    let value = raw?(key)

    if isNull value then
        None
    elif
        jsTypeof value = "number"
        && emitJsExpr value "Number.isInteger($0) && Math.abs($0) <= 2147483647"
    then
        Some(unbox<int> value)
    else
        failwith ("not a whole number: " + key)

let private specOf (raw: obj) : OpSpecWire =
    let rooms = raw?rooms

    if
        not (JS.Constructors.Array.isArray rooms)
        || not (unbox<obj[]> rooms |> Array.forall (fun room -> jsTypeof room = "string"))
    then
        failwith "not rooms"

    {
        Id = textOf raw "id"
        Template = textOf raw "template"
        Enemy = textOf raw "enemy"
        Rooms = unbox<string[]> rooms |> List.ofArray
        Deadline = numberOption raw "deadline"
        MaxEnergy = numberOption raw "maxEnergy"
        MaxBodies = numberOption raw "maxBodies"
    }

let private recordOf (raw: obj) : OpRecord option =
    caseIn phaseNames raw?p
    |> Option.bind (fun phase ->
        let ended =
            if isNull raw?x then
                Some None
            else
                caseIn endNames raw?x |> Option.map Some

        ended
        |> Option.map (fun ended ->
            {
                Id = textOf raw "i"
                Started = numberOf raw "s"
                Phase = phase
                Since = numberOf raw "a"
                Spent = numberOf raw "e"
                Lost = numberOf raw "l"
                NextDue = numberOf raw "n"
                Ended = ended
            }))

/// The ops leaf (`Memory.fabot.ops`), None where it is absent or no object:
/// no operations, never a fault. A spec row that will not decode is refused
/// as garbled, by its id where it has one; a record that will not decode is
/// dropped; the switch is on only for `true`.
let loadOps () : OpsLeaf option =
    try
        let raw = ops ()

        if isNull raw then
            None
        else
            let rows =
                if JS.Constructors.Array.isArray raw?specs then
                    unbox<obj[]> raw?specs |> List.ofArray
                else
                    []

            let decoded =
                rows
                |> List.map (fun row ->
                    try
                        Ok(specOf row)
                    with _ ->
                        let id =
                            if not (isNull row) && jsTypeof row?id = "string" then
                                unbox<string> row?id
                            else
                                "?"

                        Error { Id = id; Why = "garbled" })

            Some
                {
                    Specs =
                        decoded
                        |> List.choose (function
                            | Ok spec -> Some spec
                            | Error _ -> None)
                    Garbled =
                        decoded
                        |> List.choose (function
                            | Error refusal -> Some refusal
                            | Ok _ -> None)
                    Live = rowsOf recordOf raw?live
                    // Fails closed: any value JavaScript reads as true.
                    Off = emitJsExpr raw?off "!!$0"
                }
    with _ ->
        None

let private recordWire (record: OpRecord) : obj =
    let o =
        createObj
            [
                "i" ==> record.Id
                "s" ==> record.Started
                "p" ==> nameIn phaseNames record.Phase
                "a" ==> record.Since
                "e" ==> record.Spent
                "l" ==> record.Lost
                "n" ==> record.NextDue
            ]

    record.Ended |> Option.iter (fun how -> o?x <- nameIn endNames how)
    o

let private eventWire (tick: int) (event: OpEvent) : obj =
    let o = createObj [ "t" ==> tick ]

    let target (room: string option) =
        room |> Option.iter (fun room -> o?r <- room)

    match event with
    | OpEvent.Began(id, phase, room) ->
        o?i <- id
        o?k <- "began"
        o?p <- nameIn phaseNames phase
        target room
    | OpEvent.Moved(id, from, into, exit, room) ->
        o?i <- id
        o?k <- "moved"
        o?f <- nameIn phaseNames from
        o?p <- nameIn phaseNames into
        o?x <- exit
        target room
    | OpEvent.Recurred(id, due) ->
        o?i <- id
        o?k <- "recurred"
        o?n <- due
    | OpEvent.Ended(id, how) ->
        o?i <- id
        o?k <- "ended"
        o?e <- nameIn endNames how
    | OpEvent.Dropped id ->
        o?i <- id
        o?k <- "dropped"

    o

/// The bot's half of the ops leaf written back, onto a leaf `loadOps` read:
/// the records, this tick's refusals, this tick's events after the last
/// `Operation.eventCap` kept, and each defaulted deadline written into its
/// spec once, so it never moves. Nothing where the leaf is gone.
let saveOps
    (tick: int)
    (records: OpRecord list)
    (refused: OpRefusal list)
    (events: OpEvent list)
    (deadlines: (string * int) list)
    =
    let raw = ops ()

    if not (isNull raw) then
        raw?live <- records |> List.map recordWire |> Array.ofList

        raw?refused <-
            refused
            |> List.map (fun refusal -> createObj [ "id" ==> refusal.Id; "why" ==> refusal.Why ])
            |> Array.ofList

        let prior =
            if JS.Constructors.Array.isArray raw?events then
                unbox<obj[]> raw?events
                |> Array.filter (fun row -> not (isNull row) && jsTypeof row = "object")
            else
                [||]

        let all = Array.append prior (events |> List.map (eventWire tick) |> Array.ofList)

        raw?events <-
            Array.sub
                all
                (max 0 (all.Length - Operation.eventCap))
                (min all.Length Operation.eventCap)

        if not (List.isEmpty deadlines) && JS.Constructors.Array.isArray raw?specs then
            for row in unbox<obj[]> raw?specs do
                if not (isNull row) && jsTypeof row?id = "string" && isNull row?deadline then
                    deadlines
                    |> List.tryFind (fun (id, _) -> id = unbox<string> row?id)
                    |> Option.iter (fun (_, deadline) -> row?deadline <- deadline)

// ---- the sweep's shadow (#663 S7b) -------------------------------------------
//
// `Memory.fabot.shadow`, the bot's alone: the full ticks the operation
// interpreter's shadow of the sweep compared (`n`), those the sweep worked a
// room on (`w`), a count per kind of disagreement (`c`), the latest few
// (`e`), newest first, and this tick's refusals of the code spec (`x`). Read
// back on a cold heap so the counts run on across a reset.

let private divergenceNames =
    [
        Divergence.Room, "room"
        Divergence.Group, "group"
        Divergence.Scope, "scope"
        Divergence.Jobs, "jobs"
        Divergence.Seats, "seats"
    ]

let private exampleOf (raw: obj) : ShadowExample option =
    caseIn divergenceNames raw?k
    |> Option.map (fun kind ->
        let text (key: string) =
            if jsTypeof raw?(key) = "string" then
                unbox<string> raw?(key)
            else
                failwith ("not text: " + key)

        {
            Tick = numberOf raw "t"
            Kind = kind
            Room = text "r"
            Shadow = text "o"
            Sweep = text "s"
            Why = text "w"
        })

/// The shadow's log (`Memory.fabot.shadow`): absent, not an object or with
/// no whole count of ticks, an empty log; a count that is not a whole number
/// costs its kind, an example that will not decode its own row.
let loadShadow () : ShadowLog =
    leafOr
        Operation.shadowEmpty
        (fun () -> if isNull Memory?fabot then null else Memory?fabot?shadow)
        (fun raw ->
            if jsTypeof raw <> "object" || JS.Constructors.Array.isArray raw then
                Operation.shadowEmpty
            else
                {
                    Compared = numberOf raw "n"
                    // Absent or off its shape, it costs itself alone.
                    Working = numberValue raw?w |> Option.defaultValue 0
                    Counts =
                        intMapOf raw?c
                        |> Map.toSeq
                        |> Seq.choose (fun (name, count) ->
                            caseIn divergenceNames (box name)
                            |> Option.map (fun kind -> kind, count))
                        |> Fresh.mapOfSeq
                    Last =
                        if JS.Constructors.Array.isArray raw?e then
                            rowsOf exampleOf raw?e |> List.truncate Operation.shadowCap
                        else
                            []
                    Refused =
                        if JS.Constructors.Array.isArray raw?x then
                            rowsOf
                                (fun row ->
                                    Some
                                        {
                                            Id = textOf row "id"
                                            Why = textOf row "why"
                                        })
                                raw?x
                        else
                            []
                })

/// The shadow's log written whole.
let saveShadow (log: ShadowLog) =
    if isNull Memory?fabot then
        Memory?fabot <- createEmpty<obj>

    Memory?fabot?shadow <-
        createObj
            [
                "n" ==> log.Compared
                "w" ==> log.Working
                "c"
                ==> hashOf
                        box
                        (log.Counts
                         |> Map.toSeq
                         |> Seq.map (fun (kind, n) -> nameIn divergenceNames kind, n))
                "e"
                ==> (log.Last
                     |> List.map (fun example ->
                         createObj
                             [
                                 "t" ==> example.Tick
                                 "k" ==> nameIn divergenceNames example.Kind
                                 "r" ==> example.Room
                                 "o" ==> example.Shadow
                                 "s" ==> example.Sweep
                                 "w" ==> example.Why
                             ])
                     |> Array.ofList)
                "x"
                ==> (log.Refused
                     |> List.map (fun refusal ->
                         createObj [ "id" ==> refusal.Id; "why" ==> refusal.Why ])
                     |> Array.ofList)
            ]
