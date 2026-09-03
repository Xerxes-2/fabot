/// Serialization shell for the observe channel (ADR 0009, ADR 0028): read
/// the prior Transition log and Raid log from `Memory.fabot.observe`, hand
/// each to its pure Core fold, write the results back to their own leaves.
/// The subtree is disposable by construction — absent or unreadable state
/// is discarded, never repaired, so telemetry can never take the colony
/// down.
module Fabot.ObserveMemory

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types
open Fabot.Core.Observe

// The wire spelling of each closed vocabulary, one table per type; the
// reverse maps are derived from these, never written twice.
let private factorName =
    function
    | MatchFactor.OnlyCandidate -> "only-candidate"
    | MatchFactor.Rank -> "rank"
    | MatchFactor.TravelCost -> "travel-cost"
    | MatchFactor.Load -> "load"
    | MatchFactor.PoolOrder -> "pool-order"

let private releaseName =
    function
    | ReleaseReason.TaskGone -> "task-gone"
    | ReleaseReason.Inapplicable -> "inapplicable"
    | ReleaseReason.OverCapacity -> "over-capacity"
    | ReleaseReason.Unreachable -> "unreachable"
    | ReleaseReason.TooEarly -> "too-early"

let private idleName =
    function
    | IdleReason.NoTasks -> "no-tasks"
    | IdleReason.NoneApplicable -> "none-applicable"
    | IdleReason.NoneFree -> "none-free"
    | IdleReason.NoneReachable -> "none-reachable"
    | IdleReason.NoneInTime -> "none-in-time"

let private rejectName =
    function
    | RejectReason.Inapplicable -> "inapplicable"
    | RejectReason.CapacityFull -> "capacity-full"
    | RejectReason.Unreachable -> "unreachable"
    | RejectReason.TooEarly -> "too-early"

let private reverse toName values =
    values |> List.map (fun v -> toName v, v) |> Map.ofList

let private factorOf =
    reverse
        factorName
        [
            MatchFactor.OnlyCandidate
            MatchFactor.Rank
            MatchFactor.TravelCost
            MatchFactor.Load
            MatchFactor.PoolOrder
        ]

let private releaseOf =
    reverse
        releaseName
        [
            ReleaseReason.TaskGone
            ReleaseReason.Inapplicable
            ReleaseReason.OverCapacity
            ReleaseReason.Unreachable
            ReleaseReason.TooEarly
        ]

let private idleOf =
    reverse
        idleName
        [
            IdleReason.NoTasks
            IdleReason.NoneApplicable
            IdleReason.NoneFree
            IdleReason.NoneReachable
            IdleReason.NoneInTime
        ]

let private rejectOf =
    reverse
        rejectName
        [
            RejectReason.Inapplicable
            RejectReason.CapacityFull
            RejectReason.Unreachable
            RejectReason.TooEarly
        ]

// Body parts ride the Core's own part-name table in both directions, over
// its own closed set. `allBodyParts` is a hand-written literal, so this
// reverse can fall behind the union (#80); what is bounded here instead is
// the damage — `loadRaids` decodes episode by episode, so a name this
// table lacks costs that episode and not the ring.
let private partOf = reverse partName allBodyParts

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
        o?reason <- rejectName reason

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
        match Map.tryFind (string raw?reason) rejectOf with
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
        o?factor <- factorName factor
    | Verdict.Kept(_, task) ->
        o?kind <- "kept"
        o?task <- task
    | Verdict.Released(_, task, reason) ->
        o?kind <- "released"
        o?task <- task
        o?reason <- releaseName reason
    | Verdict.Unassigned(_, reason) ->
        o?kind <- "unassigned"
        o?reason <- idleName reason
    | Verdict.Scoring(_, candidates) ->
        o?kind <- "scoring"
        o?candidates <- candidates |> List.map encodeCandidate |> List.toArray
    | Verdict.Grounded _ -> o?kind <- "grounded"
    | Verdict.Yielded(_, counterpart) ->
        o?kind <- "yielded"
        o?counterpart <- counterpart
    | Verdict.Rerouted _ -> o?kind <- "rerouted"

    o

// Anything off the expected shape throws, and load's catch discards the
// whole subtree — bad state is dropped, never repaired.
let private decodeVerdict creep (raw: obj) : Verdict =
    let look (table: Map<string, 'a>) key =
        match Map.tryFind key table with
        | Some value -> value
        | None -> failwith "unknown wire name"

    match string raw?kind with
    | "matched" -> Verdict.Matched(creep, string raw?task, look factorOf (string raw?factor))
    | "kept" -> Verdict.Kept(creep, string raw?task)
    | "released" -> Verdict.Released(creep, string raw?task, look releaseOf (string raw?reason))
    | "unassigned" -> Verdict.Unassigned(creep, look idleOf (string raw?reason))
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

let private decodeCreepLog creep (raw: obj) : CreepLog =
    {
        Entries =
            raw?log
            |> unbox<obj[]>
            |> Array.map (fun e ->
                {
                    Tick = unbox<int> e?t
                    Verdict = decodeVerdict creep e?v
                })
            |> Array.toList
        LastTask =
            if isNull raw?lastTask then
                None
            else
                Some(decodeVerdict creep raw?lastTask)
        LastScoring =
            if isNull raw?lastScoring then
                None
            else
                Some(decodeVerdict creep raw?lastScoring)
        LastMove = raw?lastMove |> unbox<obj[]> |> Array.map (decodeVerdict creep) |> Array.toList
    }

// One raid episode on the wire: the window, the roster as an array of
// rows (the id is a field here, not a key, so a roster reads in order),
// the closest approach when one was measured, and the losses.
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
                            match Map.tryFind name partOf with
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
/// restarted timeline.
let load () : ObserveState =
    try
        let fabot = Memory?fabot
        let observe = if isNull fabot then null else fabot?observe
        let creeps = if isNull observe then null else observe?creeps

        if isNull creeps then
            Map.empty
        else
            objectEntries creeps
            |> Array.map (fun (name, raw) -> name, decodeCreepLog name raw)
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
            }
    with _ ->
        RaidState.empty

/// Write the Raid log back under `Memory.fabot.observe.raids`, leaving the
/// rest of the observe subtree — the Transition log and the verbose list —
/// alone, the same way `save` leaves this leaf alone.
let saveRaids (state: RaidState) =
    let raids = createEmpty<obj>
    raids?episodes <- state.Episodes |> List.map encodeEpisode |> List.toArray
    raids?living <- state.Living |> Set.toArray
    ensureObserve ()
    Memory?fabot?observe?raids <- raids
