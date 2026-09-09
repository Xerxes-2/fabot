/// The Matcher: keep still-valid assignments, then greedily assign the rest.
/// Assignments in, Assignments and Verdicts out. **It knows no Task kinds**
/// (ADR 0052 decision 6).
[<AutoOpen>]
module Fabot.Core.Decide.Matcher

open Fabot.Core
open Fabot.Core.Types

/// Matcher: keep still-valid assignments (anti-thrash) and greedily assign the
/// rest. Assignments in, Assignments and the Verdicts explaining them out (ADR
/// 0009): releases first in memory order, then one status Verdict per living
/// creep in view order — each preceded, for a creep on the verbose list, by its
/// Scoring Verdict, the whole pool judged against the same state its status was
/// decided from. Emission belongs to the Emitter, movement to the Resolver.
/// **It knows no Task kinds** (ADR 0052 decision 6).
let matchCreeps
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (threats: Threats)
    (pool: PooledTask list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : Assignments * Verdict list =
    let byId = pool |> List.map (fun p -> taskId p.Task, p) |> Map.ofList

    // Each living creep's remaining life and its [[body class]], hoisted
    // for the tick: the capacity gate asks the first once per holder per
    // judged pair and the second once per holder and once per candidate,
    // and both are view facts that cannot move inside a tick.
    let lives = view.Creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    let classes =
        view.Creeps
        |> List.map (fun c -> c.Name, bodyClassOf view.Tuning atlas c)
        |> Map.ofList

    let classOf name = Map.tryFind name classes

    // The crowding component of the matching key (ADR 0002): every holder,
    // counted at this tick. Arrival discounts what a Task's cap counts (ADR
    // 0026), never what the key does — spreading creeps over Tasks is a
    // judgement about now.
    let load (loads: Map<string, int>) tid =
        Map.tryFind tid loads |> Option.defaultValue 0

    let hold (loads: Map<string, int>) tid = Map.add tid (load loads tid + 1) loads

    // The holders a candidate actually competes with, counted at arrival (ADR
    // 0026): two creeps hold the same standing room against each other only
    // while both are standing on it, so a holder counts against a candidate
    // exactly when their two stays overlap.
    let overlaps (candidate: CreepInfo) task arrival name =
        let alive =
            match arrival with
            | None -> true
            | Some ticks -> Map.tryFind name lives |> Option.forall (fun life -> life >= ticks)

        let arrived =
            match Atlas.walkTicks atlas name task with
            | None -> true
            | Some ticks -> ticks <= candidate.TicksToLive

        alive && arrived

    let holdersAt (acc: Assignments) (candidate: CreepInfo) task arrival =
        let tid = taskId task

        acc
        |> Map.toList
        |> List.choose (fun (name, assigned) ->
            if assigned = tid && overlaps candidate task arrival name then
                Some name
            else
                None)

    // Every heavy body and the tile it stands on, folded once for the tick: the
    // Post census below asks this of every (candidate, Harvest) pair, and since
    // #269 every posted rock carries tiles where only a rock mid-build did.
    let heavyStanders =
        view.Creeps
        |> List.choose (fun c ->
            if classOf c.Name = Some Heavy then
                Atlas.creepTile atlas c.Name |> Option.map (fun tile -> c.Name, tile)
            else
                None)

    // The bodies standing on the Task's `Garrison` tiles, whatever Task they
    // hold this tick (#205, widened to every Post by #269) — **unioned** with
    // the Heavy holders below rather than added to them, because on a standing
    // container the two sets are ordinarily the same body: the overflow
    // reprieve keeps a garrison's Harvest applicable through a full store, so
    // it holds the Task it is standing on and a sum would spend two of the
    // rock's Posts on one Anchor. What the tiles add is the tick the two part —
    // a build tick on a Post whose container is still a site, an Upgrade
    // through the empty window on a bare dual seat — where a cap counting
    // assignments alone reads a manned Post as free. Counted at arrival like
    // every other holder (ADR 0026), which is what keeps a succession's
    // successor admissible; the candidate never counts against itself.
    let garrisons (candidate: CreepInfo) task arrival (tiles: Set<RoomPos>) =
        if Set.isEmpty tiles then
            Set.empty
        else
            heavyStanders
            |> List.choose (fun (name, tile) ->
                if
                    name <> candidate.Name
                    && Set.contains tile tiles
                    && overlaps candidate task arrival name
                then
                    Some name
                else
                    None)
            |> Set.ofList

    // Holders against numbers, and nothing else (ADR 0052 decision 6): the
    // total the Task admits, the share each scope the candidate falls in
    // admits, and the tiles whose standing bodies hold a slot without holding
    // the Task. A candidate standing on an `Exempt` tile is outside all of it —
    // the one body a budget that prices a commute never priced (#205).
    let hasCapacity (creep: CreepInfo) acc (pooled: PooledTask) (arrival: Lazy<int option>) =
        let capacity = pooled.Capacity

        let standing =
            not (Set.isEmpty capacity.Exempt)
            && (Atlas.creepTile atlas creep.Name
                |> Option.exists (fun tile -> Set.contains tile capacity.Exempt))

        if standing || not (Capacity.isBounded capacity) then
            // Only a capped Task forces the walk: the Refills and the
            // surplus work the pool is mostly made of neither walk the
            // assignment map nor pay for an arrival (ADR 0029).
            true
        else
            let holders = holdersAt acc creep pooled.Task arrival.Value
            let cls = classOf creep.Name

            let inClass wanted =
                holders |> List.filter (fun name -> classOf name = Some wanted)

            let heavyHolders = inClass Heavy
            let heavy = List.length heavyHolders
            let standingRow = inClass Standing |> List.length
            let all = List.length holders

            // The Post cap's crowd, by **name**: the heavy bodies holding this
            // Task and the heavy bodies standing on its Posts are one crowd,
            // and the ordinary garrison is in both lists (#269). Summed, it
            // would spend two of a two-Post rock's slots on the one Anchor that
            // is both, and the second Post would read full while it stands
            // empty. Only the Heavy cap reads tiles; the class shares below
            // stay counts of holders.
            let garrisoned =
                garrisons creep pooled.Task arrival.Value capacity.Garrison
                |> Set.union (Set.ofList heavyHolders)
                |> Set.count

            // A cap the candidate's own class does not fall in is not its
            // cap: the `None` scope is how a rule says "this number is
            // about somebody else's crowd".
            let within scope cap counted =
                match cap with
                | Some limit when cls |> Option.exists scope -> counted < limit
                | _ -> true

            within (fun _ -> true) capacity.Total all
            && within ((=) Heavy) capacity.Garrisons garrisoned
            && within ((<>) Heavy) capacity.Commuters (all - heavy)
            && within ((=) Standing) capacity.Standing standingRow
            && within
                (fun c -> c <> Heavy && c <> Standing)
                capacity.Generalists
                (all - heavy - standingRow)
            // The one cap that refuses a class outright rather than counting it
            // (ADR 0056): a `Fighters` number admits that many Fighters and no
            // body of any other class, because the scopes above cannot spell
            // "not a Fighter" — `Commuters` and `Generalists` both contain it —
            // and `within`'s `None` scope means "somebody else's crowd", which
            // is the opposite of a refusal.
            && (match capacity.Fighters with
                | Some limit -> cls = Some Fighter && (inClass Fighter |> List.length) < limit
                | None -> true)

    // The vision grace (#151): a Task leaves the pool for two opposite reasons
    // and its id alone cannot tell them apart — the target was destroyed, or
    // the room carrying it went dark. The pool stays gated on vision and
    // rightly so (ADR 0004): a blind room hands over an empty site list, and
    // no rule here may price what nobody can see. What this asks instead is
    // about **looking**, never about the target — the id stood in a room this
    // colony works, that room has not been seen since, and it went dark inside
    // `Tuning.VisionGrace`. Then the assignment is kept: the [[outpost]] whose
    // [[reserver]] just died is dark for the relief's lead and no longer, and
    // a builder released here walks a full load home to start the crossing
    // again on the tick the vision returns. Past the grace the release is
    // `task-gone` exactly as it was, because a container that really was
    // destroyed must not be held for ever by a body that cannot see the tile.
    // One rule over every Task kind that vision pays for, and Harvest needs
    // none of it: a declared rock is placed and pooled without vision at all
    // (ADR 0041, #148).
    //
    // One thing the grace may not outrank, and it is the one thing nothing
    // below could have asked for it: Safety (ADR 0033). A Task in no pool has
    // no Work Area, so `threatened` — which reads the *Task's* tiles — answers
    // false for it whatever stands where; the question that keeps a body alive
    // has to be asked of the **creep**. A holder standing inside a Reach is
    // therefore denied the grace, falls through to `task-gone`, and rematches
    // in the cascade below exactly as it did before the grace existed — to
    // Flee, unless it is a `Fighter`, which ADR 0056 decision 3 refuses Flee on
    // purpose: the body bought to stand in the ring is left standing where it
    // is rather than walked out of the fight by a dark room.
    let graced (creep: CreepInfo) tid =
        if standsInReach threats atlas creep.Name then
            None
        else
            lastSeenIn view tid

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed. So does reachability — a Work Area
    // the Atlas can no longer reach releases the assignment, freeing its
    // capacity for creeps that can get there, deliberately with no range-based
    // fallback (ADR 0002) — and so does the arrival gate: a drained source's
    // Harvest whose wait the holder's walk no longer covers releases it (ADR
    // 0025). Each failed gate names the release; a dead creep's assignment
    // drops silently.
    // One gate cascade judges every (creep, Task) pair, for both readings of
    // one: the fresh candidate the Matcher scores and the assignment it is
    // deciding whether to keep. Rejected at the first gate it fails
    // (threatened, applicable, capacity, reachable, in time) or the travel cost
    // when none does. The two used to be written out in full a few lines apart,
    // with prose at each promising the other ran the same gates in the same
    // order — and the order is load-bearing twice over: which rejection a pair
    // earns is what `IdleReason` reads below, and the two numbers bound above
    // the capacity gate (the travel cost, which the reachability gate and the
    // scored key both read, and the walk, which capacity counts holders at per
    // ADR 0026 and the arrival gate spends after it) are priced at most once,
    // and only if a gate asks.
    //
    // `escape` is the one gate the two readings do not share: a holder whose
    // body is expiring is kept over capacity (ADR 0026) where a fresh candidate
    // is refused. It is a `Lazy` for the same reason the walk beside it is —
    // the fresh cascade must not price it, and the keep path must not price it
    // unless the capacity gate has already failed.
    //
    // `RejectReason` is `ReleaseReason` less `TaskGone` (`Types/Verdicts.fs`),
    // and `TaskGone` is answered above this cascade, where the Task is in no
    // pool at all and no gate here can be asked about it.
    let gate
        (escape: Lazy<bool>)
        acc
        (creep: CreepInfo)
        (pooled: PooledTask)
        : Result<int, RejectReason> =
        if threatened threats atlas creep pooled.Task then
            Error RejectReason.Threatened
        elif not (applicable view threats atlas creep pooled) then
            Error RejectReason.Inapplicable
        else
            let cost = travelCostOf threats atlas creep.Name pooled.Task
            let arrival = lazy (Atlas.walkTicks atlas creep.Name pooled.Task)

            if not (hasCapacity creep acc pooled arrival) && not escape.Value then
                Error RejectReason.CapacityFull
            else
                match cost with
                | None -> Error RejectReason.Unreachable
                | Some cost ->
                    match tooEarly view atlas creep pooled.Task arrival with
                    | Some(walk, wait) -> Error(RejectReason.TooEarly(walk, wait))
                    | None -> Ok cost

    // The same gate said as a release rather than as a refusal: the two unions
    // carry the same five failures under two names, one per reading.
    let asRelease =
        function
        | RejectReason.Threatened -> ReleaseReason.Threatened
        | RejectReason.Inapplicable -> ReleaseReason.Inapplicable
        | RejectReason.CapacityFull -> ReleaseReason.OverCapacity
        | RejectReason.Unreachable -> ReleaseReason.Unreachable
        | RejectReason.TooEarly(walk, wait) -> ReleaseReason.TooEarly(walk, wait)

    let kept, keptLoads, released =
        ((Map.empty, Map.empty, []), assignments)
        ||> Map.fold (fun (acc, loads, released) name tid ->
            let release reason =
                acc, loads, Verdict.Released(name, tid, reason) :: released

            match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
            | None -> acc, loads, released
            | Some creep ->
                match Map.tryFind tid byId with
                // Kept, and by the Verdict every other steady assignment
                // answers with (ADR 0009): what the colony did this tick about
                // this creep is nothing, and there is no second word for it.
                // The Task is in no pool, so no gate below can be asked about
                // it: `graced` is the whole judgement, and the one gate it
                // carries is the one a Work Area could not have answered for
                // (Safety, ADR 0033). The holder counts against its own Task's
                // crowd the tick the vision returns, on the ordinary path.
                | None when Option.isSome (graced creep tid) ->
                    Map.add name tid acc, hold loads tid, released
                | None -> release ReleaseReason.TaskGone
                // An expiring holder is kept over capacity where a fresh
                // candidate would be refused (ADR 0026) — the one gate this
                // reading does not share with the cascade below.
                | Some pooled ->
                    match gate (lazy (expiring view atlas sizing creep)) acc creep pooled with
                    | Error reason -> release (asRelease reason)
                    | Ok _ -> Map.add name tid acc, hold loads tid, released)

    // The fresh candidate's reading of the same cascade: scored on the full key
    // where a holder is merely kept, and with no escape from the capacity gate.
    let judge acc loads (creep: CreepInfo) (pooled: PooledTask) =
        let tid = taskId pooled.Task

        match gate (lazy false) acc creep pooled with
        | Error reason -> Candidate.Rejected(tid, reason)
        | Ok cost -> Candidate.Scored(tid, pooled.Priority, cost, load loads tid)

    let assignOne (acc, loads, verdicts) (creep: CreepInfo) =
        let verdicts =
            if Set.contains creep.Name verbose then
                // The creep's own claim is set aside for its scoring: a held
                // single-Seat Task must read as the winning row, never as
                // capacity-full against its own holder's seat. The crowding
                // table is set aside with it, the two being one fact (#95).
                let without =
                    match Map.tryFind creep.Name acc with
                    | Some tid -> Map.add tid (max 0 (load loads tid - 1)) loads
                    | None -> loads

                let rows = pool |> List.map (judge (Map.remove creep.Name acc) without creep)
                Verdict.Scoring(creep.Name, rows) :: verdicts
            else
                verdicts

        match Map.tryFind creep.Name acc with
        | Some tid -> acc, loads, Verdict.Kept(creep.Name, tid) :: verdicts
        | None ->
            let judged = pool |> List.map (fun p -> p.Task, judge acc loads creep p)

            let keyed =
                judged
                |> List.choose (function
                    | t, Candidate.Scored(_, rank, cost, load) -> Some((rank, cost, load), t)
                    | _ -> None)

            match keyed with
            | [] ->
                // How far the best Task got through the gates — applicable,
                // capacity, reachable, in time — is why the creep sits idle,
                // deepest gate first: a creep whose only rejection is the arrival
                // gate is waiting out a restock (ADR 0025), and saying nothing
                // fit its body would be the same lie the rejection reason
                // refuses.
                let rejectedWith wanted =
                    judged
                    |> List.exists (function
                        | _, Candidate.Rejected(_, reason) -> wanted reason
                        | _ -> false)

                // The arrival gate's reason carries the numbers it compared
                // (#88), so the depth question asks after the case rather
                // than after a value it would have to invent to compare to.
                let isTooEarly =
                    function
                    | RejectReason.TooEarly _ -> true
                    | _ -> false

                let reason =
                    if List.isEmpty pool then
                        IdleReason.NoTasks
                    elif rejectedWith isTooEarly then
                        IdleReason.NoneInTime
                    elif rejectedWith ((=) RejectReason.Unreachable) then
                        IdleReason.NoneReachable
                    elif rejectedWith ((=) RejectReason.CapacityFull) then
                        IdleReason.NoneFree
                    else
                        IdleReason.NoneApplicable

                acc, loads, Verdict.Unassigned(creep.Name, reason) :: verdicts
            | keyed ->
                let bestKey, task = keyed |> List.minBy fst

                // The deciding factor: the first component separating the
                // winner from its closest rival, or the pool-order tie-break
                // when the whole key ties.
                let factor =
                    match keyed |> List.filter (fun (_, t) -> t <> task) with
                    | [] -> MatchFactor.OnlyCandidate
                    | rivals ->
                        let bestRank, bestCost, bestLoad = bestKey
                        let rivalRank, rivalCost, rivalLoad = rivals |> List.map fst |> List.min

                        if rivalRank <> bestRank then MatchFactor.Rank
                        elif rivalCost <> bestCost then MatchFactor.TravelCost
                        elif rivalLoad <> bestLoad then MatchFactor.Load
                        else MatchFactor.PoolOrder

                Map.add creep.Name (taskId task) acc,
                hold loads (taskId task),
                Verdict.Matched(creep.Name, taskId task, factor) :: verdicts

    let next, _, statuses = view.Creeps |> List.fold assignOne (kept, keptLoads, [])

    next, List.rev released @ List.rev statuses
