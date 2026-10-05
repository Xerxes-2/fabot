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
    let fleeing = Map.tryFind (taskId Flee) byId

    // Each living creep's remaining life and its body class, hoisted for the
    // tick: the capacity gate asks them once per holder per judged pair.
    let lives = view.Creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    // What each body carries, for the budget cap. Energy alone, because the
    // refill ring takes nothing else.
    let carried = view.Creeps |> List.map (fun c -> c.Name, c.Energy) |> Map.ofList

    let carriedBy name =
        Map.tryFind name carried |> Option.defaultValue 0

    // What each body's live Work comes to, for the Work budget.
    let works =
        view.Creeps |> List.map (fun c -> c.Name, partCount c.Body Work) |> Map.ofList

    let workBy name =
        Map.tryFind name works |> Option.defaultValue 0

    let classes =
        view.Creeps
        |> List.map (fun c -> c.Name, bodyClassOf view.Tuning atlas c)
        |> Map.ofList

    let classOf name = Map.tryFind name classes

    // Each living body's squad role, for a Fight's per-role caps.
    let roles =
        view.Creeps
        |> List.choose (fun c -> squadRoleOf c.Name c.Body |> Option.map (fun role -> c.Name, role))
        |> Map.ofList

    let roleOf name = Map.tryFind name roles

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

    // Whether each living creep is inside its lead, priced only if asked.
    let expiringNow =
        view.Creeps
        |> List.map (fun c -> c.Name, lazy (expiring view atlas sizing c))
        |> Map.ofList

    let isExpiring name =
        Map.tryFind name expiringNow |> Option.exists (fun e -> e.Value)

    // A holder inside its lead is out of a relieved Task's caps (#439): the
    // relief its row casts at that lead takes the seat beside it.
    let relieved (capacity: Capacity) name = capacity.Relieved && isExpiring name

    // A relief and its incumbent stay together once both hold a relieved Task,
    // whatever the lead reads: it moves with the colony's worst room, and a
    // lead that shrank mid-walk would release the relief. One holding under
    // half the other's life is a relief pair; two of one generation are not.
    let reliefPair (acc: Assignments) (creep: CreepInfo) (pooled: PooledTask) =
        pooled.Capacity.Relieved
        && (let tid = taskId pooled.Task

            acc
            |> Map.exists (fun name assigned ->
                assigned = tid
                && Map.tryFind name lives
                   |> Option.exists (fun life ->
                       2 * min life creep.TicksToLive < max life creep.TicksToLive)))

    let holdersAt (acc: Assignments) (candidate: CreepInfo) task (capacity: Capacity) arrival =
        let tid = taskId task

        acc
        |> Map.toList
        |> List.choose (fun (name, assigned) ->
            if
                assigned = tid
                && overlaps candidate task capacity.Handover arrival name
                && not (relieved capacity name)
            then
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
            let holders = holdersAt acc creep pooled.Task capacity arrival.Value
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
                // Counted by role or by parts and never by class (`capsHold`).
                | CapScope.Workers
                | CapScope.Carriers
                | CapScope.Role _ -> fun _ -> false

            let counted =
                function
                | CapScope.Everyone -> all
                | CapScope.Garrisons -> garrisoned
                | CapScope.Commuters -> all - heavy
                | CapScope.Standing -> standingRow
                | CapScope.Generalists -> all - heavy - standingRow
                | CapScope.Fighters -> inClass Fighter |> List.length
                | CapScope.Workers ->
                    holders |> List.filter (fun name -> workBy name > 0) |> List.length
                | CapScope.Carriers ->
                    holders |> List.filter (fun name -> workBy name = 0) |> List.length
                | CapScope.Role role ->
                    holders |> List.filter (fun name -> roleOf name = Some role) |> List.length

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
                    // A body counts in the one of the two Work crowds it is in.
                    | CapScope.Workers -> partCount creep.Body Work = 0 || counted scope < limit
                    | CapScope.Carriers -> partCount creep.Body Work > 0 || counted scope < limit
                    // A role's slots are its own bodies'; a body of no role is
                    // refused by every one of them, and the Task writes all three.
                    | CapScope.Role role ->
                        match roleOf creep.Name with
                        | Some own when own = role -> counted scope < limit
                        | Some _ -> true
                        | None -> false
                    | _ ->
                        match cls with
                        | Some c when appliesTo scope c -> counted scope < limit
                        | _ -> true)

            // The budget is a number about loads: strict, like every cap
            // above, and read over the same arrival-counted holders, so a body
            // still walking counts what it carries and a body that has poured
            // counts nothing. A holder arriving after the candidate does not
            // count: its load lands second, and a loaded body beside the ring
            // must not wait on one still crossing the swamp.
            let arrivesFirst name =
                match arrival.Value, Atlas.walkTicks atlas name pooled.Task with
                | Some mine, Some theirs -> theirs <= mine
                | _ -> true

            let budgetHolds =
                capacity.Budget
                |> Option.forall (fun budget ->
                    (holders |> List.filter arrivesFirst |> List.sumBy carriedBy) < budget)

            let workBudgetHolds =
                capacity.WorkBudget
                |> Option.forall (fun budget -> (holders |> List.sumBy workBy) < budget)

            capsHold && budgetHolds && workBudgetHolds

    // The vision grace (#151): an assignment whose Task left the pool because
    // its room went dark inside `Tuning.VisionGrace` is kept (`lastSeenIn`);
    // past the grace the release is `task-gone`, because a container that
    // really was destroyed must not be held for ever.
    //
    // The one thing the grace may not outrank is Safety. A Task in no pool has
    // no Work Area, so `threatened` answers false for it whatever stands
    // where; the question has to be asked of the creep. A holder inside a
    // Reach is denied the grace and rematches in the cascade below.
    // The struck ramparts' Repairs (#467), by room: what a non-fighter holding
    // lesser work in that room is let go for (#481). Empty on a quiet tick:
    // the Pool marks none then, and the match skips the scan.
    let struckRepairs =
        match view.Hostiles with
        | [] -> []
        | _ ->
            pool
            |> List.choose (fun pooled ->
                match pooled.Recall with
                | Some(RecallPolicy.Struck room) -> Some(room, pooled)
                | _ -> None)

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
                | Some _ when not (outlivesTheStay view atlas creep pooled.Task arrival) ->
                    Error RejectReason.Inapplicable
                | Some cost ->
                    match tooEarly view atlas creep pooled.Task arrival with
                    | Some(walk, wait) -> Error(RejectReason.TooEarly(walk, wait))
                    | None -> Ok cost

    // Whether a holder is let go for a struck rampart's Repair (#481): a
    // non-fighter in the rampart's room whose Task that Repair outranks, and
    // which the cascade would give it. Scoped to struck ramparts, so an
    // ordinary change of rank churns nobody.
    let outranked acc (creep: CreepInfo) (pooled: PooledTask) =
        not (List.isEmpty struckRepairs)
        && classOf creep.Name <> Some Fighter
        && (match Atlas.creepRoom atlas creep.Name with
            | Some room ->
                struckRepairs
                |> List.exists (fun (struckIn, repair) ->
                    struckIn = room
                    && repair.Priority < pooled.Priority
                    && Result.isOk (gate (lazy false) acc creep repair))
            | None -> false)

    // The holders let go for a kill this tick (#510, #516), decided once
    // against the remembered assignments, so the order the fold walks them in
    // moves nothing. Per kill nobody holds (a `RecallPolicy.Kill`): the
    // nearest holder within its walk of a Task it may be called off, for a
    // kill that outranks that Task and that the cascade would give it (the
    // gate says which bodies a kill admits). It is back once the kill is made.
    let called =
        let recalls =
            pool
            |> List.choose (fun kill ->
                match kill.Recall with
                | Some(RecallPolicy.Kill(room, recall)) -> Some(kill, room, recall)
                | _ -> None)

        let kills = recalls |> List.map (fun (_, room, _) -> room) |> Set.ofList

        if Set.isEmpty kills then
            Set.empty
        else

            // Each holder of a Task it may be called off, beside the room it
            // holds.
            let holders =
                assignments
                |> Map.toList
                |> List.choose (fun (name, tid) ->
                    match
                        Map.tryFind tid byId, view.Creeps |> List.tryFind (fun c -> c.Name = name)
                    with
                    | Some({
                               Recall = Some(RecallPolicy.Callable room | RecallPolicy.Kill(room, _))
                           } as pooled),
                      Some creep -> Some(creep, pooled, room)
                    | _ -> None)

            recalls
            |> List.filter (fun (kill, _, _) ->
                not (assignments |> Map.exists (fun _ tid -> tid = taskId kill.Task)))
            |> List.fold
                (fun (taken: Set<string>) (chase, killRoom, recall) ->
                    holders
                    |> List.filter (fun (creep, pooled, held) ->
                        not (Set.contains creep.Name taken)
                        && chase.Priority < pooled.Priority
                        && Facts.mayBeCalled view kills held killRoom)
                    |> List.choose (fun (creep, _, _) ->
                        travelCostOf threats atlas creep.Name chase.Task
                        |> Option.filter (fun cost ->
                            cost <= recall.Walk
                            && Result.isOk (
                                gate (lazy false) (Map.remove creep.Name assignments) creep chase
                            ))
                        |> Option.map (fun cost -> (cost, creep.Name), creep.Name))
                    |> List.sortBy fst
                    |> List.tryHead
                    |> Option.map (fun (_, name) -> Set.add name taken)
                    |> Option.defaultValue taken)
                Set.empty

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
                // An expiring holder, or one of a relief pair, is kept over
                // capacity where a fresh candidate would be refused.
                // A holder Flee applies to runs, whatever its Task's ground
                // (#472): `threatened` reads the Task's tiles, and a body
                // walking past a raid toward safe ground is struck all the way.
                // A fight Task's holder keeps it (#533): its ground is the
                // fight's answer to the Reach.
                | Some pooled when
                    not (safetyTier pooled.Task)
                    && fleeing
                       |> Option.exists (fun flee -> applicable view threats atlas creep flee)
                    ->
                    release (ReleaseReason.Rejected RejectReason.Threatened)
                | Some pooled when outranked acc creep pooled -> release ReleaseReason.Outranked
                | Some _ when Set.contains creep.Name called -> release ReleaseReason.Called
                | Some pooled ->
                    let escape = lazy (isExpiring creep.Name || reliefPair acc creep pooled)

                    match gate escape acc creep pooled with
                    | Error reason -> release (ReleaseReason.Rejected reason)
                    | Ok _ -> Map.add name tid acc, hold loads tid, released)

    // The fresh candidate's reading of the same cascade, over the whole pool:
    // scored on the full key where a holder is merely kept, and with no escape
    // from the capacity gate. The key's rank is `matchRank`'s, which reads the
    // creep's cheapest intake, so the pool is gated before any row is scored.
    // Each pooled Task beside its id, worked out once for the tick.
    let ids = pool |> List.map (fun pooled -> taskId pooled.Task, pooled)

    let gateAll acc (creep: CreepInfo) =
        ids
        |> List.map (fun (tid, pooled) -> tid, pooled, gate (lazy false) acc creep pooled)

    let score
        loads
        (creep: CreepInfo)
        (gated: (string * PooledTask * Result<int, RejectReason>) list)
        =
        let nearest =
            if workBy creep.Name = 0 then
                None
            else
                gated
                |> List.choose (fun (_, pooled, result) ->
                    match result with
                    | Ok cost when isIntake pooled.Task -> Some cost
                    | _ -> None)
                |> List.fold (fun near cost -> Some(near |> Option.fold min cost)) None

        // A fight Task the body passes outranks Flee (#533), which the two
        // share a tier with and would take on travel cost or pool order.
        let fights =
            gated
            |> List.exists (fun (_, pooled, result) ->
                pooled.Task <> Flee && safetyTier pooled.Task && Result.isOk result)

        gated
        |> List.map (fun (tid, pooled, result) ->
            let result =
                if fights && pooled.Task = Flee then
                    Error RejectReason.Inapplicable
                else
                    result

            pooled,
            match result with
            | Error reason -> Candidate.Rejected(tid, reason)
            | Ok cost ->
                Candidate.Scored(
                    tid,
                    matchRank view.Tuning nearest pooled cost,
                    cost,
                    load loads tid
                ))

    // The scored rows' keys: the Matcher's key beside the Task and its rank.
    let keyedOf (rows: (PooledTask * Candidate) list) =
        rows
        |> List.choose (function
            | p, Candidate.Scored(_, rank, cost, load) ->
                Some((rank, cost, load), p.Task, p.Priority)
            | _ -> None)

    // How far the best Task got through the gates is why a creep sits idle,
    // deepest gate first: a creep whose only rejection is the arrival gate is
    // waiting out a restock, and saying nothing fit its body would be a lie.
    let idleReason (rows: (PooledTask * Candidate) list) =
        let rejectedWith wanted =
            rows
            |> List.exists (function
                | _, Candidate.Rejected(_, reason) -> wanted reason
                | _ -> false)

        // The arrival gate's reason carries the numbers it compared (#88), so
        // the depth question asks after the case rather than after a value it
        // would have to invent to compare to.
        let isTooEarly =
            function
            | RejectReason.TooEarly _ -> true
            | _ -> false

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

    // The deciding factor: the first component separating the winner from its
    // closest rival, or the pool-order tie-break when the whole key ties. A
    // rank the pooled ranks do not order the same way was turned by travel.
    let factorOf keyed (bestKey, task, bestPooled) =
        match keyed |> List.filter (fun (_, t, _) -> t <> task) with
        | [] -> MatchFactor.OnlyCandidate
        | rivals ->
            let bestRank, bestCost, bestLoad = bestKey

            let (rivalRank, rivalCost, rivalLoad), _, rivalPooled =
                rivals |> List.minBy (fun (key, _, _) -> key)

            if rivalRank <> bestRank then
                if bestPooled < rivalPooled then
                    MatchFactor.Rank
                else
                    MatchFactor.TravelCost
            elif rivalCost <> bestCost then
                MatchFactor.TravelCost
            elif rivalLoad <> bestLoad then
                MatchFactor.Load
            else
                MatchFactor.PoolOrder

    // The fresh assignments (#530): the pair the colony ranks best is matched
    // first, whichever body it is, so a contested Task goes to the body that
    // ranks it best and reaches it cheapest, and a far body is not sent past a
    // near one because the view named it first. Ties go to the view's order.
    // A match moves one Task's holders, so only the bodies that could still
    // take that Task are gated again, and only on it.
    let entryOf loads i (creep: CreepInfo) gated =
        let rows = score loads creep gated
        let keyed = keyedOf rows

        let top =
            match keyed with
            | [] -> None
            | _ -> Some(keyed |> List.minBy (fun (key, _, _) -> key))

        i, creep, gated, rows, keyed, top

    let rec fresh acc loads pending decided =
        let best =
            pending
            |> List.choose (fun ((i, _, _, _, _, top) as entry) ->
                top |> Option.map (fun (key, _, _) -> (key, i), entry))

        match best with
        | [] ->
            let idle =
                pending
                |> List.map (fun (_, creep: CreepInfo, _, rows, _, _) ->
                    creep.Name, (Verdict.Unassigned(creep.Name, idleReason rows), rows))

            acc, loads, (decided, idle) ||> List.fold (fun m (name, entry) -> Map.add name entry m)
        | _ ->
            let _, (i, creep, _, rows, keyed, top) = best |> List.minBy fst
            let (_, task, _) as top = Option.get top
            let tid = taskId task
            let acc = Map.add creep.Name tid acc
            let loads = hold loads tid

            // A body that could take the Task is gated on it again and its row
            // rewritten in place. Only two readings reach past the row: the
            // Task was its best (its top is read again), or the Task just
            // closed to it and was its nearest intake or its one fight —
            // `matchRank` and Flee read those — and then its rows are scored
            // again whole.
            let rest =
                pending
                |> List.filter (fun (j, _, _, _, _, _) -> j <> i)
                |> List.map (fun ((j, other, gated, rows, _, otherTop) as entry) ->
                    match
                        gated
                        |> List.tryPick (fun (id, pooled, result) ->
                            match result with
                            | Ok cost when id = tid -> Some(pooled, cost)
                            | _ -> None)
                    with
                    | None -> entry
                    | Some(pooled, cost) ->
                        let result =
                            if Capacity.isBounded pooled.Capacity then
                                gate (lazy false) acc other pooled
                            else
                                Ok cost

                        let gated =
                            gated
                            |> List.map (fun ((id, p, _) as row) ->
                                if id = tid then id, p, result else row)

                        let wasTop = otherTop |> Option.exists (fun (_, t, _) -> t = pooled.Task)

                        let reaches =
                            match result with
                            | Ok _ -> false
                            | Error _ ->
                                let okCosts wanted =
                                    gated
                                    |> List.choose (fun (_, p, r) ->
                                        match r with
                                        | Ok c when wanted p.Task -> Some c
                                        | _ -> None)

                                let nearestIntake =
                                    workBy other.Name > 0
                                    && isIntake pooled.Task
                                    && okCosts isIntake |> List.forall (fun c -> c > cost)

                                let onlyFight =
                                    pooled.Task <> Flee
                                    && safetyTier pooled.Task
                                    && okCosts (fun t -> t <> Flee && safetyTier t) |> List.isEmpty

                                nearestIntake || onlyFight

                        if reaches then
                            entryOf loads j other gated
                        else
                            let rows =
                                rows
                                |> List.map (fun ((p, candidate) as row) ->
                                    match candidate, result with
                                    | Candidate.Scored(id, rank, c, _), Ok _ when id = tid ->
                                        p, Candidate.Scored(id, rank, c, load loads tid)
                                    | Candidate.Scored(id, _, _, _), Error reason when id = tid ->
                                        p, Candidate.Rejected(id, reason)
                                    | _ -> row)

                            let keyed = keyedOf rows

                            let top =
                                if wasTop then
                                    match keyed with
                                    | [] -> None
                                    | _ -> Some(keyed |> List.minBy (fun (key, _, _) -> key))
                                else
                                    otherTop

                            j, other, gated, rows, keyed, top)

            fresh
                acc
                loads
                rest
                (Map.add
                    creep.Name
                    (Verdict.Matched(creep.Name, tid, factorOf keyed top), rows)
                    decided)

    let next, nextLoads, decided =
        view.Creeps
        |> List.mapi (fun i creep -> i, creep)
        |> List.filter (fun (_, creep) -> not (Map.containsKey creep.Name kept))
        |> List.map (fun (i, creep) -> entryOf keptLoads i creep (gateAll kept creep))
        |> fun pending -> fresh kept keptLoads pending Map.empty

    // One status Verdict per living creep in view order, a verbose creep's
    // Scoring before it: a fresh creep's rows are the ones it was matched on,
    // its own claim not yet among them; a kept creep's are the whole pool
    // judged against the match with its own claim and crowding set aside
    // (#95), so a held single-Seat Task reads as the winning row.
    let statuses =
        view.Creeps
        |> List.collect (fun creep ->
            let status, rows =
                match Map.tryFind creep.Name kept with
                | Some tid ->
                    Verdict.Kept(creep.Name, tid),
                    lazy
                        (let without = Map.add tid (max 0 (load nextLoads tid - 1)) nextLoads

                         score without creep (gateAll (Map.remove creep.Name next) creep))
                | None ->
                    let status, rows = Map.find creep.Name decided
                    status, lazy rows

            if Set.contains creep.Name verbose then
                [ Verdict.Scoring(creep.Name, rows.Value |> List.map snd); status ]
            else
                [ status ])

    next, (released |> List.sortBy fst |> List.map snd) @ statuses
