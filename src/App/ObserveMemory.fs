/// Serialization shell for the observe channels: read each prior log from
/// `Memory.fabot.observe`, hand it to its pure Core fold and write the result
/// back; the Layout leaf is written outright. Absent or unreadable state is
/// discarded, never repaired, so telemetry cannot take the colony down.
module Fabot.ObserveMemory

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types
open Fabot.Core.Observe

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

let private encodeEpisode (episode: RaidEpisode) =
    let o = createEmpty<obj>
    o?opened <- episode.Opened
    o?last <- episode.LastSeen

    o?roster <-
        episode.Roster
        |> Map.toList
        |> List.map (fun (id, row) ->
            let r = createEmpty<obj>
            r?id <- id
            r?owner <- row.Owner
            let body = createEmpty<obj>

            for KeyValue(part, count) in row.Body do
                body?(partName part) <- count

            r?body <- body
            r)
        |> List.toArray

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

            d)
        |> List.toArray

    o?damage <- episode.Damage
    o

let private decodeEpisode (raw: obj) : RaidEpisode =
    {
        Opened = numberOf raw "opened"
        LastSeen = numberOf raw "last"
        Roster =
            raw?roster
            |> unbox<obj[]>
            |> Array.map (fun row ->
                string row?id,
                {
                    Owner = string row?owner
                    Body =
                        objectEntries row?body
                        |> Array.map (fun (name, count) ->
                            match partOf name, numberValue count with
                            | Some part, Some number -> part, number
                            | Some _, None -> failwith ("not a number: " + name)
                            | None, _ -> failwith "unknown wire name")
                        |> Map.ofArray
                })
            |> Map.ofArray
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

// The hold map read back, entry by entry as the latch map is.
let private holdMapOf (raw: obj) : Map<string, OutpostHold> =
    if isNull raw then
        Map.empty
    else
        objectEntries raw
        |> Array.choose (fun (key, value) ->
            try
                Some(key, decodeHold value)
            with _ ->
                None)
        |> Map.ofArray

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

// The threat map read back, entry by entry as the two maps above are.
let private threatMapOf (raw: obj) : Map<string, ThreatLatch> =
    if isNull raw then
        Map.empty
    else
        objectEntries raw
        |> Array.choose (fun (key, value) ->
            try
                Some(key, decodeThreat value)
            with _ ->
                None)
        |> Map.ofArray

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

// The latch map read back, entry by entry: the only `try` above this one is
// `loadRaids`' `leafOr`, which wraps the whole bundle, so an entry throwing
// past here would empty the episode ring and every stand-down row, and
// `saveRaids` would write that emptiness back the same tick. A `null` under
// one room is the likely hand edit (it is how the Memory HTTP API removes a
// path).
let private latchMapOf (raw: obj) : Map<string, RivalLatch> =
    if isNull raw then
        Map.empty
    else
        objectEntries raw
        |> Array.choose (fun (key, value) ->
            try
                Some(key, decodeLatch value)
            with _ ->
                None)
        |> Map.ofArray

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

/// The verbose list from `Memory.fabot.observe.verbose`: creep names owed
/// full candidate scoring this tick. Read fresh each tick so a flip
/// through the Memory HTTP API takes effect on the next tick with no
/// redeploy; absent or malformed means off.
let loadVerbose () : Set<string> =
    leafOr Set.empty (fun () -> observeLeaf "verbose") (fun verbose ->
        if not (JS.Constructors.Array.isArray verbose) then
            Set.empty
        else
            // Malformed means off entry-wise too: anything but a string
            // array reads as the empty list, never as a repaired one.
            let entries = verbose |> unbox<obj[]>

            if entries |> Array.forall (fun e -> jsTypeof e = "string") then
                entries |> Array.map unbox<string> |> Set.ofArray
            else
                Set.empty)

/// The prior observe state, or empty when the subtree is absent or
/// unreadable. A creep whose log will not decode costs that creep alone, and
/// inside a log an unreadable row costs itself.
let load () : ObserveState =
    leafOr Map.empty (fun () -> observeLeaf "creeps") (fun creeps ->
        objectEntries creeps
        |> Array.choose (fun (name, raw) ->
            try
                Some(name, decodeCreepLog name raw)
            with _ ->
                None)
        |> Map.ofArray)

/// Write the folded state back under `Memory.fabot.observe.creeps`,
/// leaving the rest of the observe subtree alone — unless the subtree
/// itself is not an object, in which case the bad state is replaced. The
/// whole log, every creep; what `saveChanged` below falls back to.
let save (state: ObserveState) =
    state |> Map.toSeq |> hashOf encodeCreepLog |> writeObserveLeaf "creeps"

/// Whether the leaf holds a log at all. Asked by the shell before it trusts
/// the log on the heap, for `cpuLineStands`' reason.
let observeLogStands () : bool =
    let creeps = observeLeaf "creeps"

    not (isNull creeps)
    && jsTypeof creeps = "object"
    && not (JS.Constructors.Array.isArray creeps)

/// Whether a creep's timeline is the one already written: the same entries —
/// by reference, which is what `Observe.step` keeps when a tick appends
/// nothing — and the same three cursors. A creep the fold handed back
/// unchanged is one whose row in the leaf is already right.
let private sameLog (a: CreepLog) (b: CreepLog) : bool =
    obj.ReferenceEquals(a.Entries, b.Entries)
    && a.LastTask = b.LastTask
    && a.LastScoring = b.LastScoring
    && a.LastMove = b.LastMove

/// Write the creeps whose timeline moved this tick and no other, and drop the
/// ones the fold pruned; the stored shape is the one `save` writes, key for
/// key. The log is the largest leaf in Memory (a hundred kilobytes over fifty
/// creeps) and encoding it whole every tick cost 1.5 ms of a live tick (#370).
///
/// The leaf is taken at its word only when it agrees with `prior` by key
/// count; otherwise it is written whole. What the handshake cannot see, and
/// is accepted: a hand-edited row under an unchanged creep stays as edited,
/// and a tick whose Memory the engine did not commit leaves that tick's
/// entries out for the creeps that then stay quiet.
let saveChanged (prior: ObserveState) (state: ObserveState) =
    let creeps = observeLeaf "creeps"

    if
        isNull creeps
        || jsTypeof creeps <> "object"
        || JS.Constructors.Array.isArray creeps
        || (JS.Constructors.Object.keys creeps).Count <> Map.count prior
    then
        save state
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
/// flat legacy `observe.raids` leaf is not migrated: it reads as absent.
let loadRaids (home: string) : RaidState =
    leafOr RaidState.empty (fun () -> colonyLeaf home "raids") (fun raids ->
        {
            Episodes = raids?episodes |> rowsOf (decodeEpisode >> Some)
            // Each of these maps is absent from a bundle that predates it,
            // and empty is what that says.
            Outposts = raids?outposts |> rowsOf (decodeOutpost >> Some)
            RivalHeld = latchMapOf raids?rivalHeld
            Holds = holdMapOf raids?holds
            Threatened = threatMapOf raids?threatened
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
/// `Memory.fabot.observe.colonies.<home>.raids`.
let saveRaids (home: string) (state: RaidState) =
    let raids = createEmpty<obj>
    raids?episodes <- state.Episodes |> List.map encodeEpisode |> List.toArray
    raids?outposts <- state.Outposts |> List.map encodeOutpost |> List.toArray
    // The three per-room maps: none has a window, expiry or basis, so each
    // stays under the room's own key rather than a row of the ring.
    raids?rivalHeld <- state.RivalHeld |> Map.toSeq |> hashOf encodeLatch
    raids?holds <- state.Holds |> Map.toSeq |> hashOf encodeHold
    raids?threatened <- state.Threatened |> Map.toSeq |> hashOf encodeThreat
    raids?living <- state.Living |> Set.toArray

    raids?placed <- state.Placed |> Map.toSeq |> hashOf (roomPosObject >> box)

    raids?hits <- state.Hits |> Map.toSeq |> hashOf box
    writeColonyLeaf home "raids" raids

/// One tile as a wire object; the deferral rows carry two of them. The room
/// is not written: the leaf is already filed under the colony's home name.
let private tileObject (tile: RoomPos) =
    let o = createEmpty<obj>
    o?x <- tile.X
    o?y <- tile.Y
    o

/// The cascade's workforce arithmetic this tick, one leaf per colony
/// beside the Layout record: `{ target, living, casting, rows: [{ row,
/// quota, living, casting }] }`. Written every tick and read by
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

    writeColonyLeaf home "quotas" o

/// Write one colony's losses this tick under `observe.colonies.<home>.layout`:
/// four lists in one leaf, written every tick, empty lists included, so
/// `observe.mjs layout` can tell "nothing is lost" from "not recorded".
let saveLayout
    (home: string)
    (unserved: UnservedFooting list)
    (unrouted: UnroutedTrunk list)
    (deferred: DeferredContainer list)
    (refused: RefusedDeclaration list)
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

    writeColonyLeaf home "layout" layout

// A breach kind on the wire, spelled once in both directions over a closed
// set: a kind added without its wire name fails to compile or to decode,
// never prints as a blank.
let private breachKindName =
    function
    | BreachKind.OreOnTheFloor -> "ore-on-the-floor"
    | BreachKind.OreUnplaceable -> "ore-unplaceable"
    | BreachKind.ReactorRunningDry -> "reactor-running-dry"
    | BreachKind.ReactorStarved -> "reactor-starved"
    | BreachKind.ReactorLost -> "reactor-lost"

let private breachKindOf name =
    match name with
    | "ore-on-the-floor" -> Some BreachKind.OreOnTheFloor
    | "ore-unplaceable" -> Some BreachKind.OreUnplaceable
    | "reactor-running-dry" -> Some BreachKind.ReactorRunningDry
    | "reactor-starved" -> Some BreachKind.ReactorStarved
    | "reactor-lost" -> Some BreachKind.ReactorLost
    | _ -> None

// One standing breach on the wire. Both ticks ride, though `last` is always
// this tick: it lets a reader with no game clock compute an age and tell
// whether the bundle that wrote the row is still running.
let private encodeBreach (row: StandingBreach) =
    let o = createEmpty<obj>
    o?kind <- breachKindName row.Breach.Kind
    o?room <- row.Breach.Room
    o?subject <- row.Breach.Subject
    o?amount <- row.Breach.Amount
    o?first <- row.FirstSeen
    o?last <- row.LastSeen
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
                            }
                    }

/// The named colony's prior breach log, or empty when its subtree is absent
/// or unreadable. A discarded log costs the ages and nothing else: every live
/// breach is re-read off this tick's view.
let loadBreaches (home: string) : BreachState =
    leafOr BreachState.empty (fun () -> colonyLeaf home "breaches") (fun breaches ->
        {
            Standing =
                breaches?rows
                |> rowsOf decodeBreach
                |> List.map (fun row -> (row.Breach.Kind, row.Breach.Subject), row)
                |> Map.ofList
        })

/// Write one colony's breach log back under
/// `Memory.fabot.observe.colonies.<home>.breaches`, every tick, empty list
/// included, so `observe.mjs breaches` can tell "no channel" from "nothing is
/// broken". Rows in `breachRows`' order, so the ordering is Core's.
let saveBreaches (home: string) (state: BreachState) =
    let breaches = createEmpty<obj>
    breaches?rows <- state |> breachRows |> List.map encodeBreach |> List.toArray
    writeColonyLeaf home "breaches" breaches

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

/// Last tick's tile of every creep of ours, the fact `CreepInfo.Moved` is
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

/// The prior CPU line, or empty when the leaf is absent or unreadable. A row
/// that will not decode costs that row alone: the window shortens.
let loadCpu () : CpuState =
    leafOr CpuState.empty (fun () -> observeLeaf "cpu") (fun cpu ->
        {
            Ticks =
                cpu?ticks
                |> rowsOf (fun raw ->
                    // Checked rather than assumed: `unbox` is erased by Fable,
                    // so without the check a foreign row is built, not
                    // rejected, and crowds out the window.
                    if jsTypeof raw?t = "number" && jsTypeof raw?ms = "number" then
                        Some
                            {
                                Tick = unbox<int> raw?t
                                Ms = unbox<float> raw?ms
                                Phases = decodeCpuPhases raw
                                Colonies = decodeCpuSplit raw "colonies"
                                Rooms = decodeCpuSplit raw "rooms"
                                Projects = decodeCpuSplit raw "projects"
                                Floods = decodeCpuFloods raw
                                HeapMb = floatOrZero raw "heap"
                                MemoRows = numberOrZero raw "rows"
                                ExternalMb = floatOrZero raw "ext"
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
            // after a deploy opens the first span.
            Spans =
                cpu?spans
                |> rowsOf (fun raw ->
                    if
                        jsTypeof raw?f = "number"
                        && jsTypeof raw?t = "number"
                        && jsTypeof raw?n = "number"
                        && jsTypeof raw?max = "number"
                        && jsTypeof raw?sum = "number"
                        && jsTypeof raw?b = "number"
                        && jsTypeof raw?r = "number"
                    then
                        Some
                            {
                                From = numberOf raw "f"
                                To = numberOf raw "t"
                                Ticks = numberOf raw "n"
                                Max = unbox<float> raw?max
                                Sum = unbox<float> raw?sum
                                Bucket = numberOf raw "b"
                                Replans = numberOf raw "r"
                                // Absent from a legacy span, and zero is what
                                // that says; present and not a number costs
                                // the span.
                                MaxPops = numberOrZero raw "p"
                                MaxHeapMb = floatOrZero raw "h"
                                MaxMemoRows = numberOrZero raw "w"
                                MinHeapMb = floatOrZero raw "hmin"
                                MaxExternalMb = floatOrZero raw "x"
                                SnapshotSum = floatOrZero raw "ss"
                                DecideSum = floatOrZero raw "sd"
                                SaveSum = floatOrZero raw "sv"
                                ExecuteSum = floatOrZero raw "sx"
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

    if sample.SweepHead > 0.0 then
        o?head <- sample.SweepHead

    // Written only when measured, so a legacy row re-encodes as itself. The
    // heap says whether the tick measured: a live heap is never 0, where a
    // memo with no cross-room Task and no lead cast has 0 rows.
    if sample.HeapMb > 0.0 then
        o?heap <- sample.HeapMb
        o?rows <- sample.MemoRows
        o?ext <- sample.ExternalMb

    o

/// One coarse span on the wire: sixteen numbers under short keys, because
/// two hundred of these ride in the same leaf as the fine ring.
let private encodeCpuSpan (span: CpuSpan) =
    let o = createEmpty<obj>
    o?f <- span.From
    o?t <- span.To
    o?n <- span.Ticks
    o?max <- span.Max
    o?sum <- span.Sum
    o?b <- span.Bucket
    o?r <- span.Replans
    o?p <- span.MaxPops
    o?h <- span.MaxHeapMb
    o?w <- span.MaxMemoRows
    o?hmin <- span.MinHeapMb
    o?x <- span.MaxExternalMb
    o?ss <- span.SnapshotSum
    o?sd <- span.DecideSum
    o?sv <- span.SaveSum
    o?sx <- span.ExecuteSum
    o

let saveCpu (state: CpuState) =
    let cpu = createEmpty<obj>
    cpu?ticks <- state.Ticks |> List.map encodeCpuSample |> List.toArray
    cpu?spans <- state.Spans |> List.map encodeCpuSpan |> List.toArray
    writeObserveLeaf "cpu" cpu

/// Whether the leaf holds a line at all: a `ticks` array with a row in it.
/// The shell asks before it trusts the line on the heap: a leaf somebody
/// removed or emptied is a line discarded on purpose, and restarts from
/// `CpuState.empty`. Empty counts as discarded because `foldCpu` appends on
/// every tick.
let cpuLineStands () : bool =
    let cpu = observeLeaf "cpu"

    not (isNull cpu)
    && JS.Constructors.Array.isArray cpu?ticks
    && (unbox<obj[]> cpu?ticks).Length > 0

/// Write this tick's row and no other: the newest row is pushed onto the
/// leaf's own `ticks` array and, at the cap, the oldest shifted off. Encoding
/// the whole line every tick cost 1.36 ms of a live tick (#370). The append is
/// taken only when the leaf agrees with the line, checked on the tick numbers
/// at both ends; anything else is written whole. A row an edit changed inside
/// the window is not repaired and stands until the ring shifts it off.
let appendCpu (state: CpuState) =
    let cpu = observeLeaf "cpu"
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
                    && jsTypeof rows.[held - 1]?f = "number"
                    && unbox<int> rows.[held - 1]?f = open'.From

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
        else
            saveCpu state
    | _ -> saveCpu state
