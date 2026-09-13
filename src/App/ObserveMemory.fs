/// Serialization shell for the observe channels (ADR 0009, ADR 0028, ADR 0035,
/// ADR 0041): read the prior Transition log, Raid log and CPU line from
/// `Memory.fabot.observe`, hand each to its pure Core fold and write the
/// results back; the Layout leaf is written outright, having no prior state
/// because it records this tick's plan. Absent or unreadable state is
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

// A keyed wire object read back as whole numbers — `hashOf box`'s decode
// partner, absent reading as empty the way an absent row array does.
let private intMapOf (raw: obj) : Map<string, int> =
    if isNull raw then
        Map.empty
    else
        objectEntries raw
        |> Array.map (fun (key, value) -> key, unbox<int> value)
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

let private readNumbers (raw: obj) =
    if isNull raw?walk || isNull raw?wait then
        None
    else
        Some(unbox<int> raw?walk, unbox<int> raw?wait)

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
            unbox<int> raw?rank,
            unbox<int> raw?cost,
            unbox<int> raw?load
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
                        Tick = unbox<int> e?t
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
        let c = createEmpty<obj>
        c?range <- approach.Range
        // The room the approach was measured in: the episode is the
        // colony's and names none of its own (ADR 0028), so without this
        // the tile would read as home's coordinates.
        c?room <- approach.Pos.Room
        c?x <- approach.Pos.X
        c?y <- approach.Pos.Y
        c?t <- approach.Tick
        o?closest <- c
    | None -> ()

    o?losses <-
        episode.Losses
        |> List.map (fun loss ->
            let d = createEmpty<obj>
            d?creep <- loss.Creep
            d?t <- loss.Tick
            d)
        |> List.toArray

    o?damage <- episode.Damage
    o

let private decodeEpisode (raw: obj) : RaidEpisode =
    {
        Opened = unbox<int> raw?opened
        LastSeen = unbox<int> raw?last
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
                            match partOf name with
                            | Some part -> part, unbox<int> count
                            | None -> failwith "unknown wire name")
                        |> Map.ofArray
                })
            |> Map.ofArray
        Closest =
            if isNull raw?closest then
                None
            else
                Some
                    {
                        Range = unbox<int> raw?closest?range
                        Pos =
                            {
                                // An approach with no room reads as the
                                // empty name, as a projection naming no
                                // room does, rather than costing the row.
                                Room =
                                    if isNull raw?closest?room then
                                        ""
                                    else
                                        string raw?closest?room
                                X = unbox<int> raw?closest?x
                                Y = unbox<int> raw?closest?y
                            }
                        Tick = unbox<int> raw?closest?t
                    }
        Losses =
            raw?losses
            |> unbox<obj[]>
            |> Array.map (fun d ->
                {
                    Creep = string d?creep
                    Tick = unbox<int> d?t
                })
            |> Array.toList
        // An episode written before the damage was recorded reads as zero
        // rather than costing its row: the field is missing, not wrong,
        // and the roster and approach beside it were written correctly
        // (ADR 0028).
        Damage = if isNull raw?damage then 0 else unbox<int> raw?damage
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
    o

let private decodeOutpost (raw: obj) : OutpostEpisode =
    {
        RoomName = string raw?room
        Opened = unbox<int> raw?opened
        LastSeen = unbox<int> raw?last
        // The one field with no honest default: `unbox` is a cast and not a
        // check, so a row without this key would decode to `undefined`, every
        // comparison against it would answer false, and `standingDown` would
        // report a running stand-down as spent — the one direction ADR 0043
        // does not allow the gate to be wrong in.
        Expiry =
            if isNull raw?expiry then
                failwith "missing expiry"
            else
                unbox<int> raw?expiry
        // A basis the vocabulary does not have costs its row rather than
        // reading as another basis: a stand-down that cannot say why it
        // holds an outpost is no stand-down, and the load already degrades
        // episode by episode (ADR 0028).
        Basis =
            match standDownBasisOf (string raw?basis) with
            | Some basis -> basis
            | None -> failwith "unknown wire name"
    }

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
/// itself is not an object, in which case the bad state is replaced.
let save (state: ObserveState) =
    state |> Map.toSeq |> hashOf encodeCreepLog |> writeObserveLeaf "creeps"

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
            Living = raids?living |> unbox<string[]> |> Set.ofArray
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
    raids?living <- state.Living |> Set.toArray
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
    ]

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
                            }
                    else
                        None)
        })

/// Write the CPU line back under `Memory.fabot.observe.cpu`, leaving the rest
/// of the observe subtree alone the way `saveRaids` does. Every tick, so
/// `observe.mjs cpu` can tell "this bundle keeps the line" from "the colony has
/// been quiet", and a tick that throws first leaves a gap in the tick numbers.
/// Rows ride as `{ t, ms }` because the tick number is the half a reader cannot
/// reconstruct.
let saveCpu (state: CpuState) =
    let cpu = createEmpty<obj>

    cpu?ticks <-
        state.Ticks
        |> List.map (fun sample ->
            let o = createEmpty<obj>
            o?t <- sample.Tick
            o?ms <- sample.Ms

            match sample.Phases with
            | Some phases ->
                for key, read in cpuPhaseFields do
                    o?(key) <- read phases
            | None -> ()

            o)
        |> List.toArray

    writeObserveLeaf "cpu" cpu
