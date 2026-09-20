/// The Matcher: keep still-valid assignments, then greedily assign the rest.
/// Assignments in, Assignments and Verdicts out. It knows no Task kinds.
[<AutoOpen>]
module Fabot.Core.Decide.Matcher

open Fabot.Core
open Fabot.Core.Types

/// Keep still-valid assignments (anti-thrash) and greedily assign the rest.
/// Verdicts out: releases first in memory order, then one status Verdict per
/// living creep in view order — each preceded, for a creep on the verbose
/// list, by its Scoring Verdict, the whole pool judged against the same state
/// its status was decided from.
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

    // Each living creep's remaining life and its body class, hoisted for the
    // tick: the capacity gate asks them once per holder per judged pair.
    let lives = view.Creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    // What each body carries, for the budget cap. Energy alone, because the
    // refill ring takes nothing else.
    let carried = view.Creeps |> List.map (fun c -> c.Name, c.Energy) |> Map.ofList

    let carriedBy name =
        Map.tryFind name carried |> Option.defaultValue 0

    let classes =
        view.Creeps
        |> List.map (fun c -> c.Name, bodyClassOf view.Tuning atlas c)
        |> Map.ofList

    let classOf name = Map.tryFind name classes

    // ADR-0002. The crowding component of the matching key: every holder,
    // counted at this tick, never at arrival — spreading creeps over Tasks is
    // a judgement about now.
    let load (loads: Map<string, int>) tid =
        Map.tryFind tid loads |> Option.defaultValue 0

    let hold (loads: Map<string, int>) tid = Map.add tid (load loads tid + 1) loads

    // ADR-0026. The holders a candidate actually competes with, counted at
    // arrival: a holder counts against a candidate exactly when their two
    // stays overlap. One relation, asked twice with the two bodies swapped.
    // An unpriceable walk is no overlap to refuse, and neither is a life the
    // projection does not carry.
    let outlives (walk: int option) (life: int option) =
        match walk with
        | None -> true
        | Some ticks -> life |> Option.forall (fun remaining -> remaining >= ticks)

    let outlivesHandover handover arrival life =
        match arrival with
        | None -> true
        | Some ticks ->
            life
            |> Option.forall (fun remaining ->
                if handover = 0 then
                    remaining >= ticks
                else
                    remaining > ticks + handover)

    let overlaps (candidate: CreepInfo) task handover arrival name =
        outlivesHandover handover arrival (Map.tryFind name lives)
        && outlives (Atlas.walkTicks atlas name task) (Some candidate.TicksToLive)

    let holdersAt (acc: Assignments) (candidate: CreepInfo) task handover arrival =
        let tid = taskId task

        acc
        |> Map.toList
        |> List.choose (fun (name, assigned) ->
            if assigned = tid && overlaps candidate task handover arrival name then
                Some name
            else
                None)

    // Every heavy body and the tile it stands on, folded once for the tick: the
    // Post census below asks this of every (candidate, Harvest) pair.
    let heavyStanders =
        view.Creeps
        |> List.choose (fun c ->
            if classOf c.Name = Some Heavy then
                Atlas.creepTile atlas c.Name |> Option.map (fun tile -> c.Name, tile)
            else
                None)

    // The bodies standing on the Task's `Garrison` tiles, whatever Task they
    // hold this tick. Counted at arrival like every other holder, which is
    // what keeps a succession's successor admissible; the candidate never
    // counts against itself.
    let garrisons (candidate: CreepInfo) task arrival (tiles: Set<RoomPos>) =
        if Set.isEmpty tiles then
            Set.empty
        else
            heavyStanders
            |> List.choose (fun (name, tile) ->
                if
                    name <> candidate.Name
                    && Set.contains tile tiles
                    && overlaps candidate task 0 arrival name
                then
                    Some name
                else
                    None)
            |> Set.ofList

    // Holders against numbers, and nothing else: the caps, the garrison tiles
    // and the budget. A candidate standing on an `Exempt` tile is outside all
    // of it.
    let hasCapacity (creep: CreepInfo) acc (pooled: PooledTask) (arrival: Lazy<int option>) =
        let capacity = pooled.Capacity

        let standing =
            not (Set.isEmpty capacity.Exempt)
            && (Atlas.creepTile atlas creep.Name
                |> Option.exists (fun tile -> Set.contains tile capacity.Exempt))

        if standing || not (Capacity.isBounded capacity) then
            // Only a capped Task forces the walk: most of the pool neither
            // walks the assignment map nor pays for an arrival.
            true
        else
            let holders = holdersAt acc creep pooled.Task capacity.Handover arrival.Value
            let cls = classOf creep.Name

            let inClass wanted =
                holders |> List.filter (fun name -> classOf name = Some wanted)

            let heavyHolders = inClass Heavy
            let heavy = List.length heavyHolders
            let standingRow = inClass Standing |> List.length
            let all = List.length holders

            // The Post cap's crowd, by name: the heavy holders and the heavy
            // bodies standing on its Posts are one crowd, and the ordinary
            // garrison is in both lists. Only the Heavy cap reads tiles.
            let garrisoned =
                garrisons creep pooled.Task arrival.Value capacity.Garrison
                |> Set.union (Set.ofList heavyHolders)
                |> Set.count

            // Whose crowd each scope is a number about, and how many of them
            // this cap is counted against.
            let appliesTo =
                function
                | CapScope.Everyone -> fun _ -> true
                | CapScope.Garrisons -> (=) Heavy
                | CapScope.Commuters -> (<>) Heavy
                | CapScope.Standing -> (=) Standing
                | CapScope.Generalists -> fun c -> c <> Heavy && c <> Standing
                | CapScope.Fighters -> (=) Fighter

            let counted =
                function
                | CapScope.Everyone -> all
                | CapScope.Garrisons -> garrisoned
                | CapScope.Commuters -> all - heavy
                | CapScope.Standing -> standingRow
                | CapScope.Generalists -> all - heavy - standingRow
                | CapScope.Fighters -> inClass Fighter |> List.length

            // Every cap on the Task holds, or the candidate is refused. A cap
            // whose crowd the candidate's own class does not fall in is not its
            // cap, and neither is any cap for a class the Atlas cannot name.
            let capsHold =
                capacity.Caps
                |> Map.forall (fun scope limit ->
                    match scope with
                    // The one cap that refuses a class outright rather than
                    // counting it (`CapScope.Fighters`).
                    | CapScope.Fighters -> cls = Some Fighter && counted scope < limit
                    | _ ->
                        match cls with
                        | Some c when appliesTo scope c -> counted scope < limit
                        | _ -> true)

            // The budget is a number about loads: strict, like every cap
            // above, and read over the same arrival-counted holders, so a body
            // still walking counts what it carries and a body that has poured
            // counts nothing.
            let budgetHolds =
                capacity.Budget
                |> Option.forall (fun budget -> (holders |> List.sumBy carriedBy) < budget)

            capsHold && budgetHolds

    // The vision grace (#151): an assignment whose Task left the pool because
    // its room went dark inside `Tuning.VisionGrace` is kept (`lastSeenIn`);
    // past the grace the release is `task-gone`, because a container that
    // really was destroyed must not be held for ever.
    //
    // The one thing the grace may not outrank is Safety. A Task in no pool has
    // no Work Area, so `threatened` answers false for it whatever stands
    // where; the question has to be asked of the creep. A holder inside a
    // Reach is denied the grace and rematches in the cascade below.
    let graced (creep: CreepInfo) tid =
        if standsInReach threats atlas creep.Name then
            None
        else
            lastSeenIn view tid

    // One gate cascade judges every (creep, Task) pair, for both readings of
    // one: the fresh candidate the Matcher scores and the assignment it is
    // deciding whether to keep. Rejected at the first gate it fails
    // (threatened, applicable, capacity, reachable, in time) or the travel
    // cost when none does. The order is load-bearing twice over: which
    // rejection a pair earns is what `IdleReason` reads below, and the travel
    // cost and the walk are priced at most once, and only if a gate asks.
    //
    // `escape` is the one gate the two readings do not share: an expiring
    // holder is kept over capacity where a fresh candidate is refused. A
    // `Lazy` for the same reason the walk beside it is: priced only if the
    // capacity gate has already failed. `TaskGone` is answered above this
    // cascade, where the Task is in no pool at all.
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

    // Which holder a cap that has shrunk gives up (#230): the fold below
    // judges each remembered assignment against the ones already kept, so the
    // order it walks one Task's holders in is the rule for who keeps the slot.
    // The nearest keeps: the body already standing in the Work Area is the one
    // whose next tick is work. Ordered only where it could decide something,
    // a bounded Task with more than one holder, so most of the pool is never
    // walked for this. The floods it adds (holders that never reached the
    // gate's `arrival` lazy) are bounded by the holders of capped Tasks and
    // memoised per start tile within the tick; the profile does not move on
    // them.
    //
    // A body the Atlas cannot place sorts last, because `Atlas.walkTicks`
    // answers `Some 0` for an unplaced creep — the same number as a body
    // standing in the Work Area — and ranked on the walk alone the ghost would
    // take the slot on its name. The creep name breaks a tie so the fold stays
    // a function of the view. Sorting it last also keeps the reason right: it
    // meets the cap the kept holders filled and is released `CapacityFull`.
    let tier (pooled: PooledTask) (name: string) =
        match Atlas.creepTile atlas name with
        | Some _ ->
            match Atlas.walkTicks atlas name pooled.Task with
            | Some walk -> 0, walk
            | None -> 1, 0
        | None -> 1, 0

    let remembered =
        assignments
        |> Map.toList
        |> List.groupBy snd
        |> List.collect (fun (tid, holders) ->
            // A lone holder is the whole order, and is not worth the lookup.
            match holders with
            | []
            | [ _ ] -> holders
            | _ ->
                match Map.tryFind tid byId with
                | Some pooled when Capacity.isBounded pooled.Capacity ->
                    holders |> List.sortBy (fun (name, _) -> tier pooled name, name)
                | _ -> holders)

    // The releases are reported in the assignment map's order whatever order
    // they were decided in, so the Verdict list stays in memory order.
    let kept, keptLoads, released =
        ((Map.empty, Map.empty, []), remembered)
        ||> List.fold (fun (acc, loads, released) (name, tid) ->
            let release reason =
                acc, loads, (name, Verdict.Released(name, tid, reason)) :: released

            match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
            | None -> acc, loads, released
            | Some creep ->
                match Map.tryFind tid byId with
                // Kept, by the Verdict every other steady assignment answers
                // with. The Task is in no pool, so `graced` is the whole
                // judgement.
                | None when Option.isSome (graced creep tid) ->
                    Map.add name tid acc, hold loads tid, released
                | None -> release ReleaseReason.TaskGone
                // An expiring holder is kept over capacity where a fresh
                // candidate would be refused.
                | Some pooled ->
                    match gate (lazy (expiring view atlas sizing creep)) acc creep pooled with
                    | Error reason -> release (ReleaseReason.Rejected reason)
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
                // How far the best Task got through the gates is why the creep
                // sits idle, deepest gate first: a creep whose only rejection
                // is the arrival gate is waiting out a restock, and saying
                // nothing fit its body would be a lie.
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

    next, (released |> List.sortBy fst |> List.map snd) @ List.rev statuses
