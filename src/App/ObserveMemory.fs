/// Serialization shell for the observe channels (ADR 0009, ADR 0028,
/// ADR 0035): read the prior Transition log and Raid log from
/// `Memory.fabot.observe`, hand each to its pure Core fold, write the
/// results back to their own leaves — and write the Layout's own leaf,
/// which has no prior state to read because it records this tick's plan
/// rather than a history.
/// The subtree is disposable by construction — absent or unreadable state
/// is discarded, never repaired, so telemetry can never take the colony
/// down.
module Fabot.ObserveMemory

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types
open Fabot.Core.Observe

// Body parts ride the Core's own part-name table in both directions, over
// its own closed set, reversed by the Core's own builder — the same shape
// as the Verdict vocabularies, whose tables Core now holds outright.
// `Core.Tests` round-trips every one of them against the union itself, so
// a case added without its wire name is a test failure rather than a
// silent decode miss.
let private partOf = reverseOf partName allBodyParts

// Where a reason's numbers sit on the wire (#88): `walk` and `wait` on
// the row that names it, beside the reason rather than inside it, so a
// bare tag adds no fields. Which numbers a case carries is Core's to say,
// the way its name is — this shell only places them. A row that names a
// reason needing numbers and carries none reads as no reason at all, and
// is dropped on decode the way a row with an unknown name is.
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
// The scored row is not widened by #88: only a rejected one raises the
// question its numbers answer.
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

    o

// Anything off the expected shape throws, and `decodeCreepLog` drops that
// one row — bad state is discarded, never repaired.
let private decodeVerdict creep (raw: obj) : Verdict =
    // A wire name outside the vocabulary is not a Verdict we can restate,
    // so it throws; Core owns the vocabulary, this shell owns the cost. A
    // name whose numbers are missing misses the same way (#88): a row says
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

// A Verdict this bundle cannot restate costs its own row and no more,
// the way an undecodable episode costs one episode. The wire shape moves
// with the vocabularies — a `too-early` row written before the reason
// carried its numbers (#88) is exactly that — and a ring that vanished
// whole on meeting one would take a creep's history with it on the tick
// a new bundle lands, which is the history the change wants read. Bad
// state is still discarded rather than repaired; the discard is just no
// longer the whole log. A cursor that will not decode reads as no cursor,
// and costs at most one entry the next tick appends unchanged.
let private decodeCreepLog creep (raw: obj) : CreepLog =
    let tryVerdict (raw: obj) =
        try
            Some(decodeVerdict creep raw)
        with _ ->
            None

    {
        Entries =
            raw?log
            |> unbox<obj[]>
            |> Array.choose (fun e ->
                tryVerdict e?v
                |> Option.map (fun verdict ->
                    {
                        Tick = unbox<int> e?t
                        Verdict = verdict
                    }))
            |> Array.toList
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
        LastMove = raw?lastMove |> unbox<obj[]> |> Array.choose tryVerdict |> Array.toList
    }

// One raid episode on the wire: the window, the roster as an array of
// rows (the id is a field here, not a key, so a roster reads in order),
// the closest approach when one was measured, the losses, and the damage
// in hits (ADR 0034).
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
        // and dropping the episode would lose the roster and the approach
        // that were written correctly (ADR 0028's per-episode degradation).
        Damage = if isNull raw?damage then 0 else unbox<int> raw?damage
    }

// The observe subtree is created on demand and replaced whole only when
// what stands there is not an object. Each writer then assigns its own
// leaf, so `creeps`, `verbose` and `raids` never clobber one another.
let private ensureObserve () =
    if isNull Memory?fabot then
        Memory?fabot <- createEmpty<obj>

    if jsTypeof Memory?fabot?observe <> "object" || isNull Memory?fabot?observe then
        Memory?fabot?observe <- createEmpty<obj>

/// The verbose list from `Memory.fabot.observe.verbose`: creep names owed
/// full candidate scoring this tick. Written by the CLI through the Memory
/// HTTP API, read fresh each tick so a terminal flip takes effect on the
/// next tick with no redeploy; absent or malformed means off.
let loadVerbose () : Set<string> =
    try
        let fabot = Memory?fabot
        let observe = if isNull fabot then null else fabot?observe
        let verbose = if isNull observe then null else observe?verbose

        if isNull verbose || not (JS.Constructors.Array.isArray verbose) then
            Set.empty
        else
            // Malformed means off, entry-wise too: anything but a string
            // array reads as an empty list, never as a repaired one.
            let entries = verbose |> unbox<obj[]>

            if entries |> Array.forall (fun e -> jsTypeof e = "string") then
                entries |> Array.map unbox<string> |> Set.ofArray
            else
                Set.empty
    with _ ->
        Set.empty

/// The prior observe state, or empty when the subtree is absent, from an
/// older bundle, or otherwise unreadable — a discarded log only costs a
/// restarted timeline. A creep whose log will not decode costs that
/// creep's timeline alone, the way `loadRaids` degrades episode by
/// episode: the rest of the map loads. Inside a log the same holds one
/// level down — an unreadable row costs itself, not the timeline around
/// it — so a wire-shape change reads as a gap rather than as amnesia.
let load () : ObserveState =
    try
        let fabot = Memory?fabot
        let observe = if isNull fabot then null else fabot?observe
        let creeps = if isNull observe then null else observe?creeps

        if isNull creeps then
            Map.empty
        else
            objectEntries creeps
            |> Array.choose (fun (name, raw) ->
                try
                    Some(name, decodeCreepLog name raw)
                with _ ->
                    None)
            |> Map.ofArray
    with _ ->
        Map.empty

/// Write the folded state back under `Memory.fabot.observe.creeps`, leaving
/// the rest of the observe subtree (the verbose list included) alone —
/// unless the subtree itself is not an object, in which case the bad state
/// is replaced outright.
let save (state: ObserveState) =
    let creeps = createEmpty<obj>

    for KeyValue(name, log) in state do
        creeps?(name) <- encodeCreepLog log

    ensureObserve ()
    Memory?fabot?observe?creeps <- creeps

/// The prior Raid log, or empty when the subtree is absent, from an older
/// bundle, or otherwise unreadable — a discarded log costs the episodes it
/// held and nothing else. An episode that will not decode costs that
/// episode alone: the ring degrades row by row rather than vanishing, so a
/// hand-edit through the Memory HTTP API, or a rollback across a
/// wire-shape change, leaves the rest of the history readable.
let loadRaids () : RaidState =
    try
        let fabot = Memory?fabot
        let observe = if isNull fabot then null else fabot?observe
        let raids = if isNull observe then null else observe?raids

        if isNull raids then
            RaidState.empty
        else
            {
                Episodes =
                    raids?episodes
                    |> unbox<obj[]>
                    |> Array.choose (fun raw ->
                        try
                            Some(decodeEpisode raw)
                        with _ ->
                            None)
                    |> Array.toList
                Living = raids?living |> unbox<string[]> |> Set.ofArray
                // The damage baseline, absent from a bundle written before
                // it existed: an empty baseline charges the next tick
                // nothing, which is what a fresh episode starts from
                // anyway.
                Hits =
                    if isNull raids?hits then
                        Map.empty
                    else
                        objectEntries raids?hits
                        |> Array.map (fun (id, hits) -> id, unbox<int> hits)
                        |> Map.ofArray
            }
    with _ ->
        RaidState.empty

/// Write the Raid log back under `Memory.fabot.observe.raids`, leaving the
/// rest of the observe subtree — the Transition log, the verbose list and
/// the Layout record — alone, the same way `save` leaves this leaf alone.
let saveRaids (state: RaidState) =
    let raids = createEmpty<obj>
    raids?episodes <- state.Episodes |> List.map encodeEpisode |> List.toArray
    raids?living <- state.Living |> Set.toArray

    let hits = createEmpty<obj>

    for KeyValue(id, value) in state.Hits do
        hits?(id) <- value

    raids?hits <- hits
    ensureObserve ()
    Memory?fabot?observe?raids <- raids

/// Write the Layout's own losses under `Memory.fabot.observe.layout`,
/// leaving the rest of the observe subtree alone the way `saveRaids` does:
/// the footing targets it could not serve (#77) and the trunks it could
/// not route (#107). Two lists in one leaf and not two leaves — both are
/// the Layout's, both are the current plan's, and a reader asking what
/// this room lost asks once (ADR 0035). Every tick, the empty lists
/// included: the leaf's presence is what lets `observe.mjs layout` tell
/// "this bundle records the loss" from "nothing is lost", ADR 0028's
/// reason applied to a second record. No load and no fold — the record is
/// the current plan's, so there is no prior state to degrade from and
/// nothing a bad leaf could cost.
let saveLayout (unserved: UnservedFooting list) (unrouted: UnroutedTrunk list) =
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

    // The goal's spawn rides beside its name rather than inside it, the
    // way a Verdict's numbers do (#88): a row whose goal is the Upgrade
    // Work Area carries no `spawn` key at all, and a `spawn` row without
    // one decodes to nothing rather than to some other goal.
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

    ensureObserve ()
    Memory?fabot?observe?layout <- layout
