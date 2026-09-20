/// Serialization shell for the observe channels (ADR 0009, ADR 0028, ADR 0035,
/// ADR 0041, #278): read the prior Transition log, Raid log, CPU line and
/// breach log from `Memory.fabot.observe`, hand each to its pure Core fold and
/// write the results back; the Layout leaf is written outright, having no prior
/// state because it records this tick's plan. Absent or unreadable state is
/// discarded, never repaired, so telemetry cannot take the colony down.
module Fabot.ObserveMemory

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types
open Fabot.Core.Observe

// Body parts ride the Core's own part-name table in both directions over
// its closed set, so `Core.Tests` round-trips every one of them and a case
// added without its wire name fails a test rather than decoding silently.
let private partOf = reverseOf partName allBodyParts

// A wire array read row by row: an absent leaf is the empty list and a row
// that will not decode costs its own row and no more (ADR 0028). Bad state is
// discarded rather than repaired, and the discard is one row and never the
// whole channel — the discipline six leaves used to re-apply by hand, each one
// `try`/`with` away from getting it wrong. A decoder says "not this row" by
// answering None or by throwing; both read the same from here.
//
// The **absent** leaf is where this widens what the six sites used to do, and
// deliberately. Four of them guarded no nulls at all: `unbox` is erased by
// Fable, so a missing `episodes` key walked an `undefined`, threw, and hit the
// enclosing `try` — which discarded the whole `RaidState`, `Outposts` and
// `Living` and `Hits` along with it, and the whole of a creep's log for a
// missing `log` key. That is precisely the failure ADR 0028 is written against
// and precisely what `loadRaids`' own doc-comment says does not happen, so the
// guard here is the rule those comments already claimed.
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

// A wire leaf read whole: an absent leaf and one that will not decode are the
// same answer — the empty value — so a bundle written before the leaf existed
// and a bundle written wrong both cost that leaf and no more (ADR 0028). The
// discipline `rowsOf` applies row by row, applied one level up, where five
// loaders used to re-apply it by hand and each one was a `try`/`with` away
// from getting it wrong. The leaf arrives as a thunk because reading it is
// itself part of what may throw.
let private leafOr (empty: 'a) (leaf: unit -> obj) (decode: obj -> 'a) : 'a =
    try
        let raw = leaf ()
        if isNull raw then empty else decode raw
    with _ ->
        empty

// **A whole number off the wire, and a throw for anything else** (#294).
// `unbox<int>` is erased by Fable, so an off-shape value compiles to `| 0` and
// reads back as tick 0 — a stand-down whose `expiry` was hand-edited to an
// object decoded to a row that `standingDown` then read as *spent*, which is
// the one direction ADR 0043 says the gate may not be wrong in. Every decoder
// below that took a tick or a coordinate took it this way; the wire gate
// (`scripts/wire-check.mjs`) is what found it, and #275 is where the same cast
// cost a rival latch its tick.
//
// A throw and not a default, because every caller of this is inside a row
// decoder: a row that will not read costs its own row (ADR 0028), and a
// default would be the invented tick all over again.
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

// A keyed wire object read back as whole numbers — `hashOf box`'s decode
// partner, absent reading as empty the way an absent row array does.
let private intMapOf (raw: obj) : Map<string, int> =
    if isNull raw then
        Map.empty
    else
        objectEntries raw
        // A value that is not a number costs its own entry (#294): these are
        // baselines a later tick re-reads, so dropping one is a tick of
        // silence where keeping it is a difference taken against nonsense.
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

// A Verdict this bundle cannot restate costs its own row and no more, the
// way an undecodable episode costs one episode (ADR 0028): bad state is
// discarded rather than repaired, but the discard is one row and never the
// whole log. A cursor that will not decode reads as no cursor.
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

// One raid episode on the wire: the window, the roster as an array of rows
// (the id is a field, so a roster reads in order), the closest approach
// when one was measured, the losses, and the damage in hits (ADR 0034).
/// A tile **with its room** as a wire object, and the read of one: the shape
/// the Raid log's coordinates take (#204, #216 R3, #376), where the Layout's
/// `tileObject` below leaves the room off on purpose. The episode is the
/// colony's and names no room (ADR 0028), so every coordinate it carries
/// says its own.
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
        // The room the approach was measured in rides the tile (#204): the
        // episode is the colony's and names none of its own (ADR 0028), so
        // without it the coordinate would read as home's.
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
                    // Absent on a row written before #376, and on a body the
                    // projection never placed: both read as no tile.
                    Where = if isNull d?room then None else Some(roomPosOf d)
                })
            |> Array.toList
        // Absent on an episode written before ADR 0034, and zero is what that
        // says: the field is missing rather than wrong, and the roster and
        // approach beside it were written correctly (ADR 0028). Present and
        // off-shape is the other case, and costs the row like every number.
        Damage = if isNull raw?damage then 0 else numberOf raw "damage"
    }

// One outpost episode on the wire (ADR 0043): the room it shuts, the
// window, the tick the stand-down runs to, and which deadline that tick
// was read off. A key of its own beside `episodes`, so a reader of either
// family reads it whole and a bad row of one costs the other nothing.
let private encodeOutpost (episode: OutpostEpisode) =
    let o = createEmpty<obj>
    o?room <- episode.RoomName
    o?opened <- episode.Opened
    o?last <- episode.LastSeen
    o?expiry <- episode.Expiry
    o?basis <- standDownBasisName episode.Basis
    // Written only when true (#382), so a row from a bundle that predates the
    // field reads `false` — "no bunker seen here", which is what every row
    // written before this change meant.
    if episode.Stronghold then
        o?stronghold <- true

    o

let private decodeOutpost (raw: obj) : OutpostEpisode =
    {
        RoomName = string raw?room
        Opened = numberOf raw "opened"
        LastSeen = numberOf raw "last"
        // The one field with no honest default: `unbox` is a cast and not a
        // check, so a row without this key would decode to `undefined`, every
        // comparison against it would answer false, and `standingDown` would
        // report a running stand-down as spent — the one direction ADR 0043
        // does not allow the gate to be wrong in.
        // A row with no expiry, or one whose expiry is not a number, costs its
        // own row. `unbox` is a cast and not a check, so either would have
        // decoded to `undefined` or to 0, every comparison against it would
        // answer false, and `standingDown` would report a running stand-down
        // as spent — the one direction ADR 0043 does not allow the gate to be
        // wrong in. The off-shape half of that was live until #294's wire gate
        // asked.
        Expiry = numberOf raw "expiry"
        // A basis the vocabulary does not have costs its row rather than
        // reading as another basis: a stand-down that cannot say why it
        // holds an outpost is no stand-down, and the load already degrades
        // episode by episode (ADR 0028).
        Basis =
            match standDownBasisOf (string raw?basis) with
            | Some basis -> basis
            | None -> failwith "unknown wire name"
        // Absent on every row written before #382, and absent on a row that
        // never saw one: both mean "no bunker here", which is the reading the
        // colony had for the whole of its life until this field existed.
        Stronghold = not (isNull raw?stronghold) && unbox<bool> raw?stronghold
    }

// One room held by somebody else's reservation on the wire (#333):
// `{ holder, until }` — whose CLAIM parts stand on the controller, and the
// absolute tick the engine's countdown ends on. Under the room's own key like
// the latch below it and not a row of the ring: the ring's rows withdraw a
// room, and this one withdraws only the reservation on it.
let private encodeHold (hold: OutpostHold) =
    let o = createEmpty<obj>
    o?holder <- reservationHolderName hold.Holder
    o?until <- hold.Until
    o

// `encodeHold`'s partner, and — like `decodeLatch` — a checker rather than a
// cast. Both fields answer for the entry: a `holder` this vocabulary does not
// have would name the wrong player, and an `until` that is not a number would
// read as a tick and hold the room in the record for ever, `view.Time < until`
// comparing against a string being false in the one direction that never ends.
// An entry that will not read **costs a body** (`holdMapOf`, ADR 0028): the
// room drops out of the record, and the record is what the Reserve pool and
// the reserver row are narrowed by on the ticks nothing is looking into the
// room (`ColonyView.HeldOutposts`, #333) — so the room reads as reservable and
// the row buys the deficit. That is why the wire guard in `observe.mjs` fails
// the whole command over one such entry instead of printing the room open: the
// bot drops what it cannot decode, and the operator is the only one who can be
// told. Dropping it is still the right shape here — the next tick with vision
// writes the entry back — but it is a loss and not a free one.
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

// The hold map read back, entry by entry as the latch map is and for the same
// reason: one unreadable entry must not take the leaf's whole history with it.
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

// One remembered raid on the wire (#366): `{ until }`, the tick the guard row
// stops answering for a room it has gone blind in. One field and an object all
// the same, matching `encodeHold` beside it rather than a bare number: the leaf
// beside this one was written as a bare number once and #275 had to grow it a
// second field on a live bundle, which cost a migration clause that is still
// there.
let private encodeThreat (latch: ThreatLatch) =
    let o = createEmpty<obj>
    o?until <- latch.Until
    o

// `encodeThreat`'s partner, a checker and not a cast for `decodeHold`'s reason:
// an `until` that is not a number would compare false against `view.Time <
// until` for ever, which here means a guard hired for a room nothing has looked
// into since the log was hand-edited. An entry that will not read is dropped
// (`threatMapOf`) and costs the room its memory — the guard row falls back to
// what vision says, which is the pre-#366 behaviour and not a wrong answer
// about a different room.
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

// One latched room on the wire (#275): `{ since, lastLooked }` — the tick the
// gate shut on, and the tick the last look into the room was taken on, which is
// what the stride to the next look is measured from.
let private encodeLatch (latch: RivalLatch) =
    let o = createEmpty<obj>
    o?since <- latch.Since
    o?lastLooked <- latch.LastLooked
    o

// A latch written as a bare number is the shape this leaf carried before #275,
// and a live bundle meets one on the tick it is deployed: every room the running
// colony has latched is recorded that way. The number is the tick the gate shut
// on, and under the old rule it was also the tick the stride was counted from,
// so it reads as both fields — the first look under the new rule then falls a
// full `RivalRecheck` after the shutting, which is where the old rule would have
// put it too. A record carrying `since` and no `lastLooked` reads the same way,
// for the same reason: the only look such a log can vouch for is the one that
// shut the gate.
//
// Every *other* shape throws, and the throw costs this entry alone
// (`latchMapOf`). `unbox` is a cast and not a check — the discipline
// `decodeOutpost` states against itself two decoders up — so a `since` that is
// not a number would read back as a tick all the same: `{}`, `[100, 100]` and
// `"100"` all decode to the epoch, dating the withdrawal to tick 0 where #117's
// US-20 asks for the real one, and leaving `lookDue`'s elapsed test to compare
// a tick against a string for ever. An unfalsifiable latch is precisely what
// #275 exists to kill, so a latch that cannot be read is dropped instead: the
// room leaves the gate, re-enters the scan, and the next look with vision
// decides it again. Both ticks are checked the same way, because a
// `lastLooked` that is not a number is the same lie about the same gate.
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

// The latch map read back — `hashOf encodeLatch`'s decode partner, absent
// reading as empty the way an absent row array does, and an entry that will not
// decode costing that entry and no more, the way a row of `rowsOf` does (ADR
// 0028).
//
// Entry by entry and not leaf by leaf, which is the difference between one
// reopened room and a lost history. The only `try` above this one is
// `loadRaids`' own `leafOr`, which wraps the whole bundle: a single unreadable
// entry throwing past here would take the episode ring, every clocked
// stand-down row, `Living` and `Hits` with it, and `saveRaids` would write that
// emptiness back on the same tick, making it permanent. A `null` under one room
// is the likely hand edit rather than an exotic one — editing this leaf is the
// documented way out of a stuck latch, and writing `null` is how the Memory
// HTTP API is told to remove a path — and the rows it would cost include every
// live stand-down, which is the safety channel.
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

// The observe subtree is created on demand and replaced whole only when
// what stands there is not an object; each writer then assigns its own
// leaf, so `creeps`, `verbose`, `cpu` and the `colonies` subtree never
// clobber one another (ADR 0047).
let private ensureObserve () =
    if isNull Memory?fabot then
        Memory?fabot <- createEmpty<obj>

    if jsTypeof Memory?fabot?observe <> "object" || isNull Memory?fabot?observe then
        Memory?fabot?observe <- createEmpty<obj>

// One colony's own subtree under `Memory.fabot.observe.colonies.<home>` (ADR
// 0047): the [[raid log]] and the [[layout record]] are a colony's record and
// not the world's, so they live under one home room's key because `decide` runs
// once per colony. `assignments`, `observe.creeps` and `observe.cpu` stay flat
// and keyed as they were, because no two colonies can collide on those keys.
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
// subtree or the leaf is absent — the descent every reader below used to spell
// out for itself. Null is what they all degrade from, so an absent leaf reads
// as empty and is rebuilt from this tick on.
let private observeLeaf (leaf: string) : obj =
    let fabot = Memory?fabot
    let observe = if isNull fabot then null else fabot?observe

    if isNull observe then null else observe?(leaf)

// Write one flat leaf, leaving the rest of the observe subtree alone — unless
// the subtree itself is not an object, in which case the bad state is replaced.
let private writeObserveLeaf (leaf: string) (value: obj) =
    ensureObserve ()
    Memory?fabot?observe?(leaf) <- value

// Write one leaf of one colony's own subtree, leaving every other colony's
// leaves and every flat leaf alone (ADR 0047).
let private writeColonyLeaf (home: string) (leaf: string) (value: obj) =
    ensureColony home
    Memory?fabot?observe?colonies?(home)?(leaf) <- value

// One colony's leaf as it stands in Memory, or null when the subtree, the
// colony or the leaf is absent — a colony this bundle has not written for
// yet (ADR 0047). Null is what every reader below already degrades from,
// so an absent leaf reads as empty and is rebuilt from this tick on.
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
/// unreadable — a discarded log only costs a restarted timeline. A creep
/// whose log will not decode costs that creep alone, and inside a log an
/// unreadable row costs itself, so a wire-shape change reads as a gap
/// rather than as amnesia (ADR 0028).
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

/// Whether the leaf holds a log at all — an object under
/// `Memory.fabot.observe.creeps`. Asked by the shell before it trusts the log
/// it holds on the heap, for the CPU line's reason (`cpuLineStands`): a leaf
/// somebody removed is a log discarded on purpose, and it restarts from this
/// tick as it always did rather than being written back whole off the heap.
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
/// ones the fold pruned (#370): the leaf's own object is what `save` wrote a
/// tick ago, so a creep whose log the fold handed back unchanged already has
/// its row, and only the changed rows are encoded. The stored shape is the one
/// `save` writes, key for key, so `observe.mjs` and `load` read what they
/// always read.
///
/// Why: the log is the largest leaf in Memory — a hundred kilobytes over
/// fifty creeps — and it was decoded whole and encoded whole every tick to
/// move a few creeps' cursors: 1.5 ms of a live tick, read by the probe that
/// measured it (#370), most of it standing bodies whose story had not moved.
///
/// `prior` is the state the fold was handed, which is what the leaf holds when
/// the shell keeps the log on the heap and wrote it whole on its first tick.
/// The leaf is taken at its word only when it agrees with that prior by its
/// key count — the handshake `appendCpu` makes on tick numbers — and a leaf
/// that is not an object, is an array, or holds another number of creeps is
/// written whole as `save` does. What the handshake cannot see, and is
/// accepted: a row somebody hand-edited under a creep the fold leaves
/// unchanged stays as edited until that creep's story moves, and a tick whose
/// Memory the engine did not commit leaves that tick's entries out of the leaf
/// for the creeps that then stay quiet.
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

/// The named colony's prior Raid log, or empty when its subtree is absent
/// or unreadable. An episode that will not decode costs that episode
/// alone: the ring degrades row by row rather than vanishing (ADR 0028),
/// so a hand edit or a rollback leaves the rest readable. One log per
/// colony (ADR 0047), because the log answers for the rooms one colony
/// works; a flat `observe.raids` leaf is not migrated — it reads as
/// absent, and the ring refills within one window.
let loadRaids (home: string) : RaidState =
    leafOr RaidState.empty (fun () -> colonyLeaf home "raids") (fun raids ->
        {
            Episodes = raids?episodes |> rowsOf (decodeEpisode >> Some)
            // The outpost family's ring (ADR 0043), absent from a
            // bundle that predates it — and an empty ring is what
            // that says. Degraded row by row: a stand-down that will
            // not decode costs its own room's gate and no other.
            Outposts = raids?outposts |> rowsOf (decodeOutpost >> Some)
            // The clockless withdrawal's memory (ADR 0043): the rooms last
            // seen **owned** by another player — a rival's reservation is a
            // clocked row of `outposts` since #165 — each against the tick
            // that look was taken on, and — since #275 — the tick the last
            // look was taken on beside it, which is what the stride between
            // rechecks is counted from. An empty map is honest — the room is
            // still scanned, so the next look with vision re-decides it.
            RivalHeld = latchMapOf raids?rivalHeld
            // The rooms somebody else's reservation stands on (#333),
            // absent from a bundle that predates the read and an empty
            // map being what that says: the rule the reserver row obeys
            // is read off the view every tick, so an empty record costs
            // the colony nothing and the operator one tick of silence.
            Holds = holdMapOf raids?holds
            // The guard row's memory of a raid in a room it has gone blind in
            // (#366), absent from a bundle that predates it and an empty map
            // being what that says: the ticks with vision write it back, and
            // until they do the row answers off vision alone, which is what it
            // did before the record existed.
            Threatened = threatMapOf raids?threatened
            // Names, and a name is a string: `unbox` is erased, so without
            // the filter a number under `living` becomes a creep we think
            // stands somewhere, and the tick after it "dies" and charges the
            // episode a loss nobody suffered.
            Living =
                // An array and not merely a truthy thing: `unbox` is a cast,
                // so a string under this key walks character by character and
                // reads as one creep per letter (#294).
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
            // The tiles beside the names (#376), absent from a bundle that
            // predates them: the first loss after a deploy carries no tile.
            Placed =
                if isNull raids?placed then
                    Map.empty
                else
                    objectEntries raids?placed
                    |> Array.choose (fun (name, tile) ->
                        // Entry by entry, the way `latchMapOf` reads: a tile
                        // that will not decode costs the body it belongs to
                        // and not the episode ring beside it.
                        try
                            Some(name, roomPosOf tile)
                        with _ ->
                            None)
                    |> Map.ofArray
            // The damage baseline, absent from a bundle written
            // before it existed: an empty baseline charges the next
            // tick nothing, which is where a fresh episode starts.
            Hits = intMapOf raids?hits
        })

/// Write one colony's Raid log back under
/// `Memory.fabot.observe.colonies.<home>.raids`, leaving the rest of the
/// observe subtree and every other colony's leaves alone (ADR 0047).
let saveRaids (home: string) (state: RaidState) =
    let raids = createEmpty<obj>
    raids?episodes <- state.Episodes |> List.map encodeEpisode |> List.toArray
    raids?outposts <- state.Outposts |> List.map encodeOutpost |> List.toArray
    // Room name to the two ticks the clockless withdrawal keeps: the one the
    // gate shut on and the one the last look was taken on (#275). Still no
    // window, expiry or basis — this withdrawal has none — so the entry stays a
    // pair of dates under the room's own key rather than a row of the ring.
    raids?rivalHeld <- state.RivalHeld |> Map.toSeq |> hashOf encodeLatch
    // Room name to whose reservation stands on its controller and the tick
    // that hold ends (#333). A clock and no window or basis — this withdraws
    // nothing, so there is no episode to date — and under the room's own key,
    // one hold per controller being all the engine allows.
    raids?holds <- state.Holds |> Map.toSeq |> hashOf encodeHold
    // Room name to the tick the guard row stops answering for a raid it can no
    // longer see (#366). A clock and no window, basis or roster: what a raid
    // was is the episode ring's business, and this is one bit — armed, and
    // still worth a body — under the room's own key.
    raids?threatened <- state.Threatened |> Map.toSeq |> hashOf encodeThreat
    raids?living <- state.Living |> Set.toArray

    raids?placed <- state.Placed |> Map.toSeq |> hashOf (roomPosObject >> box)

    raids?hits <- state.Hits |> Map.toSeq |> hashOf box
    writeColonyLeaf home "raids" raids

/// One tile as a wire object; the deferral rows carry two of them, and a
/// tile named `x`/`y` twice over would say which is which nowhere. The
/// room is not written: every row of this leaf is one colony's Layout
/// (ADR 0011) and the leaf is already filed under that colony's home
/// name, so two spellings of one fact are what the record avoids.
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

/// Write one colony's losses this tick — the footing targets the Layout could
/// not serve, the trunks it could not route, the container picks it deferred to
/// a container already serving their target (ADR 0040), and the declarations the
/// colony refuses because no chain of [[seam]]s joins them to its home (#243,
/// ADR 0060 decision 1) — under
/// `observe.colonies.<home>.layout`, leaving every other leaf
/// alone the way `saveRaids` does. Four lists in one leaf, so a reader asking
/// what this room lost asks once, which is ADR 0035's own reason for putting
/// more than one there; written every tick, empty lists included, so
/// `observe.mjs layout` can tell "nothing is lost" from "this bundle does not
/// record it" (ADR 0028). The fourth is the declaration's loss and not the
/// Layout's, and it joins this channel rather than opening a fourth for one
/// list because it shares every other property of the three: colony-level,
/// this tick's rather than history, and with no creep for a [[verdict]] to
/// name.
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

    // The room and the kind it was declared as (ADR 0060 decision 1). The
    // room alone was the whole of what a reader could act on while there was
    // one kind of declaration; with two, a bare "W15S25" under a heading that
    // reads "declared outposts" is a second silence wearing the first one's
    // clothes. The fix is still a human moving the declaration (ADR 0041's
    // constant), and the kind is what says which list to move it in; the home
    // this leaf is filed under is the other half of the pair already.
    layout?refused <-
        refused
        |> List.map (fun entry ->
            let o = createEmpty<obj>
            o?room <- entry.RoomName
            o?kind <- declarationKindName entry.Kind
            o)
        |> List.toArray

    writeColonyLeaf home "layout" layout

// A breach kind on the wire (#278). Spelled once, in both directions, over a
// closed set — `reactorOwnerName`'s idiom two leaves up, and for its reason: a
// kind added without its wire name must fail to compile or fail to decode,
// never print as a blank. Hyphenated lower case, the spelling
// `standDownBasisName` and `declarationKindName` already use on this subtree.
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

// One standing breach on the wire: what broke, where, on which engine object,
// the number that makes it actionable, and the two ticks that date it. Plain
// fields and no union, the way every other leaf of this subtree is written.
//
// Both ticks ride, though one of them is always this tick: `last` is what lets
// a reader of the leaf compute an age — `last - first` — with no game clock of
// its own, and what tells that reader whether the bundle that wrote the row is
// still running. `observe.mjs breaches` reads exactly those two.
let private encodeBreach (row: StandingBreach) =
    let o = createEmpty<obj>
    o?kind <- breachKindName row.Breach.Kind
    o?room <- row.Breach.Room
    o?subject <- row.Breach.Subject
    o?amount <- row.Breach.Amount
    o?first <- row.FirstSeen
    o?last <- row.LastSeen
    o

// `encodeBreach`'s partner, and a checker rather than a cast: `unbox` is erased
// by Fable, so a `first` that is not a number would read back as a tick and
// date a breach to the epoch — an age of a hundred thousand ticks printed
// against a pile dropped this minute. A row that will not read costs its own
// row and no more (`rowsOf`, ADR 0028), which is the safe direction here in a
// way it is not for a stand-down: the next tick re-reads the projection and
// writes every live breach back, so a dropped row is one tick of silence rather
// than a room left open.
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

/// The named colony's prior breach log, or empty when its subtree is absent or
/// unreadable (#278). One log per colony, like the Raid log and the Layout
/// record beside it (ADR 0047): the checks read one colony's view.
///
/// What a discarded log costs is the **ages** and nothing else — every live
/// breach is re-read off this tick's view and written back, so the rows return
/// on the next tick reading zero ticks old. That is the cheapest degradation of
/// the four channels, and it is why nothing here is repaired.
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
/// `Memory.fabot.observe.colonies.<home>.breaches`, leaving the rest of the
/// observe subtree and every other colony's leaves alone (ADR 0047).
///
/// Written every tick, empty list included, so the leaf's presence is itself
/// the signal that this bundle is live — which is what lets `observe.mjs
/// breaches` tell "no channel" from "nothing is broken", the distinction ADR
/// 0028 made for the Raid log and ADR 0035 for the Layout record.
///
/// Rows in `breachRows`' order — oldest breach first — so the ordering is
/// Core's, computed once and under test, rather than a second opinion in the
/// writer and a third in the reader.
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

/// What each colony spent inside `decide`, read off one CPU row, or the empty
/// list when the row carries none (#370).
///
/// Decoded on its own and never folded into `cpuPhaseFields`' all-six-or-none
/// guard: the rows standing when this landed were written by a bundle that did
/// not measure it, and they are exactly the window the change is compared
/// against, so admitting them with their phases intact is the point. Every
/// entry must be a number — a key whose value is not says the writer and the
/// reader disagree about the shape, and half a split would price one colony
/// against a boundary nobody read.
/// A sub-object of numbers off a CPU row, or the empty list when the row has
/// none. Shared by the two splits that hang beside the phases — the colonies'
/// `decide` and the rooms' `snapshot` — because they are the same shape read
/// from two keys, and a second copy of this would be the next place the two
/// drift apart.
let private decodeCpuSplit (raw: obj) (key: string) : (string * float) list =
    let split = raw?(key)

    if jsTypeof split <> "object" || isNull split then
        []
    else
        JS.Constructors.Object.keys split
        |> Seq.filter (fun name -> jsTypeof split?(name) = "number")
        |> Seq.map (fun name -> name, unbox<float> split?(name))
        |> List.ofSeq

/// The flood split off one CPU row (#389): `{ home: [floods, free, pops] }`,
/// three integers per colony under one sub-object, read the way
/// `decodeCpuSplit` reads the millisecond splits — absent or malformed is the
/// empty list, and a colony whose triple is not three numbers is left out
/// rather than read as zeros.
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

/// The phase split off one CPU row, or `None` when the row carries none.
/// Absent and malformed answer alike: a row that predates the split has no
/// phase keys, and one whose keys will not decode was measured by nobody,
/// while its `ms` is still the number ADR 0041's trigger is read off. All
/// six or none — a half-decoded group would price a phase against a
/// boundary that was never read.
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

/// The prior CPU line, or empty when the leaf is absent or unreadable — a
/// discarded line costs the ticks it held and nothing else. A row that
/// will not decode costs that row alone (ADR 0028): the window shortens
/// rather than vanishing.
let loadCpu () : CpuState =
    leafOr CpuState.empty (fun () -> observeLeaf "cpu") (fun cpu ->
        {
            Ticks =
                cpu?ticks
                |> rowsOf (fun raw ->
                    // The wire types are checked rather than assumed, which
                    // is what makes `rowsOf`'s degradation real here:
                    // `unbox` is erased by Fable, so without the check a
                    // row of a foreign shape is not rejected but built, and
                    // then crowds out the window ADR 0041's mean is read
                    // off.
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
                                // A bare number and not a group, so it decodes
                                // on its own: a row from a bundle that did not
                                // measure the head reads 0.0, which is what a
                                // sweep with no head would also read — and the
                                // two are told apart by whether `rooms` is
                                // there at all.
                                SweepHead =
                                    if jsTypeof raw?head = "number" then
                                        unbox<float> raw?head
                                    else
                                        0.0
                            }
                    else
                        None)
            // The coarse spans (#386), absent from a leaf written before they
            // existed and an empty list being what that says: the fine ring
            // beside them is untouched, and the first tick after a deploy
            // opens the first span.
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
                                // Absent from a span written before #389,
                                // and zero is what that says: no tick of
                                // it was counted. Present and not a number
                                // costs the span, as `b` and `r` do.
                                MaxPops =
                                    if jsTypeof raw?p = "undefined" then 0 else numberOf raw "p"
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

    // One sub-object rather than a key per colony, so a home room's
    // name can never collide with a phase's (#370) — and so the group
    // is absent as a whole on a row that has none, which is what
    // `decodeCpuSplit` reads as "nobody measured this".
    //
    // Two of them now, and they are written the same way for the same
    // reasons: the colonies' share of `decide` and the rooms' share of
    // `snapshot`. Separate keys rather than one table of names, because
    // a home room appears in **both** — W15S28 is a colony that decides
    // and a room that is swept — and one table would have to choose
    // which of its two prices to keep.
    let writeSplit key rows =
        if not (List.isEmpty rows) then
            let split = createEmpty<obj>

            for name, ms in rows do
                split?(name) <- ms

            o?(key) <- split

    writeSplit "colonies" sample.Colonies
    writeSplit "rooms" sample.Rooms
    writeSplit "projects" sample.Projects

    // The flood counts (#389), a triple per colony under one key for the
    // same collision reason, and as an array rather than three keys because
    // a hundred rows pay for every character of every key.
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

    o

/// Write the CPU line back under `Memory.fabot.observe.cpu`, leaving the rest
/// of the observe subtree alone the way `saveRaids` does — the whole line,
/// every row. What `appendCpu` below falls back to.
/// One coarse span on the wire (#386): eight numbers under short keys, because
/// two hundred of these ride in the same leaf as the fine ring and every
/// character of every key is paid for two hundred times.
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
    o

let saveCpu (state: CpuState) =
    let cpu = createEmpty<obj>
    cpu?ticks <- state.Ticks |> List.map encodeCpuSample |> List.toArray
    cpu?spans <- state.Spans |> List.map encodeCpuSpan |> List.toArray
    writeObserveLeaf "cpu" cpu

/// Whether the leaf holds a line at all — a `ticks` array with a row in it
/// under `Memory.fabot.observe.cpu`. The shell asks this before it trusts the
/// line it holds on the heap: a leaf somebody removed or emptied through the
/// Memory HTTP API is a line discarded on purpose, and it restarts from this
/// tick as it always did — from `CpuState.empty` — rather than being written
/// back whole off the heap. Empty counts as discarded because the bot never
/// leaves it so: `foldCpu` appends on every tick.
let cpuLineStands () : bool =
    let cpu = observeLeaf "cpu"

    not (isNull cpu)
    && JS.Constructors.Array.isArray cpu?ticks
    && (unbox<obj[]> cpu?ticks).Length > 0

/// Write this tick's row and no other (#370): the leaf's own `ticks` array is
/// what `saveCpu` would have written a tick ago, so the newest row is pushed
/// onto it and, once the ring is at its cap, the oldest shifted off the front.
/// The stored shape is the one `saveCpu` writes, row for row, for every row
/// this bundle wrote, so `observe.mjs cpu` and `loadCpu` read what they always
/// read. A row an edit changed inside the window is not repaired — the
/// agreement below is read at the ends — and stands until the ring shifts it
/// off; the rows another bundle wrote are re-encoded once, by the whole write
/// the shell makes on its first tick.
///
/// Why: the line is a hundred rows of some thirty-five values each, and it was
/// decoded whole and encoded whole on every tick to add one row — 1.36 ms of
/// a live tick, read by the probe that measured it (#370), and outside every
/// phase column because it is the one write the line does not price. ADR
/// 0041's rule stands untouched: one row a tick, every tick, unconditionally;
/// what changes is that ninety-nine rows nobody asked about this tick are no
/// longer rebuilt.
///
/// The append is taken only when the leaf agrees with the line: its rows are
/// the line's but the newest — one longer when the fold dropped the oldest
/// this tick — checked on the tick numbers at both ends rather than assumed.
/// Anything else (no leaf, a hand-edited one, a line the heap and the leaf
/// disagree about) is written whole, which is what every earlier tick did.
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

            // The coarse spans ride the same append (#390): the open span is
            // rewritten every tick and a span that just opened is pushed,
            // against a leaf whose span array is the state's but the newest
            // — one row a tick against the two hundred the whole write does.
            // Before this, `spans` was written by `saveCpu` alone, once per
            // upload, and the record #386 sized for five hours reached
            // Memory frozen at the first tick. A leaf that disagrees — no
            // array, a length the fold could not have produced — is written
            // whole, as the rows are.
            let spans = cpu?spans

            match List.rev state.Spans with
            | open' :: closed when JS.Constructors.Array.isArray spans ->
                let held = (unbox<obj[]> spans).Length
                let count = List.length closed

                if held = count + 1 then
                    emitJsStatement (spans, held - 1, encodeCpuSpan open') "$0[$1] = $2"
                elif held = count then
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
