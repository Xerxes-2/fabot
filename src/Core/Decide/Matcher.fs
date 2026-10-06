/// The Matcher: keep still-valid assignments, then greedily assign the rest.
/// Assignments in, Assignments and Verdicts out. It knows no Task kinds.
[<AutoOpen>]
module Fabot.Core.Decide.Matcher

open Fabot.Core
open Fabot.Core.Types

/// One pass of the match: the bodies it keeps and assigns, and the Tasks it
/// offers them. The fight pass's share is the fighters and the fight pool,
/// the economy's every other body and the rest of the pool, Flee in both.
type MatchShare =
    {
        Bodies: CreepInfo list
        Offered: PooledTask list
    }

/// The price's mate to the walk round (#472, #591): whether a body that runs
/// from a Threat, standing outside its room's Reach, has no walk round the
/// Reach to a Task's tiles in that room. The mover walks it round or parks
/// it at the edge (`Resolver.moveIntentFor`), so such work is out of its
/// reach, not merely dear. One closure a decide, recalled by its Threats so
/// the Matcher and the fight carry share it: each room's grid with its Reach
/// shut (`Atlas.weightsAvoiding`) built once, one flood a body
/// (`Atlas.wayRound`), and nothing asked on a tick with no Reach.
let private walledOffMemo: TickMemo.Memo<Threats, CreepInfo -> Task -> bool> =
    TickMemo.create 8 TickMemo.byReference

let private walledOffAfresh
    (view: ColonyView)
    atlas
    (threats: Threats)
    : CreepInfo -> Task -> bool =
    if Map.isEmpty threats.Reach then
        fun _ _ -> false
    else
        let grids = System.Collections.Generic.Dictionary<string, int[]>()

        let ways =
            System.Collections.Generic.Dictionary<string, (Set<RoomPos> -> bool) option>()

        fun (creep: CreepInfo) task ->
            not (safetyTier task)
            && runsFromThreats view atlas creep
            && match Atlas.creepTile atlas creep.Name with
               | None -> false
               | Some at ->
                   let reach = Threats.reachIn threats at.Room

                   not (Set.isEmpty reach)
                   && not (Set.contains (RoomPos.pos at) reach)
                   && (let area = areaFor threats atlas creep.Name task

                       not (Set.isEmpty area)
                       && Set.forall (fun (tile: RoomPos) -> tile.Room = at.Room) area
                       && (let way =
                               match ways.TryGetValue creep.Name with
                               | true, way -> way
                               | _ ->
                                   let weights =
                                       match grids.TryGetValue at.Room with
                                       | true, weights -> weights
                                       | _ ->
                                           let weights = Atlas.weightsAvoiding atlas at.Room reach
                                           grids.[at.Room] <- weights
                                           weights

                                   let way = Atlas.wayRound atlas creep.Name reach weights
                                   ways.[creep.Name] <- way
                                   way

                           way |> Option.exists (fun reaches -> not (reaches area))))

let internal walledOffBy (view: ColonyView) atlas (threats: Threats) : CreepInfo -> Task -> bool =
    TickMemo.recall walledOffMemo threats (fun _ -> walledOffAfresh view atlas threats)

/// One colony's side of a match: its view, the Atlas and the Threats its
/// bodies are priced in, its row sizing, and its whole pool — what a
/// remembered Task is looked up in, what a verbose creep is scored against,
/// and whether there are Tasks at all.
[<NoEquality; NoComparison>]
type MatchContext =
    {
        View: ColonyView
        Atlas: Atlas.Atlas
        Sizing: RowSizing
        Threats: Threats
        Pool: PooledTask list
        /// Whether a part of the pool not handed in holds Tasks — a
        /// fight-only decide's economy (`decideFight`) — so an idle body
        /// there is never told there are none.
        PooledElsewhere: bool
    }

/// What a match leaves: the assignments, each release beside the name it
/// let go, and each matched body's status Verdicts, a verbose creep's
/// Scoring before its status.
[<NoEquality; NoComparison>]
type MatchOutcome =
    {
        Assigned: Assignments
        Released: (string * Verdict) list
        Statuses: Map<string, Verdict list>
    }

/// One colony's Verdicts off the matches its bodies were judged in:
/// releases first in memory order, then its creeps' statuses in view order.
let verdictsOf (creeps: CreepInfo list) (outcomes: MatchOutcome list) : Verdict list =
    let names = creeps |> List.map _.Name |> Set.ofList

    let released =
        outcomes
        |> List.collect _.Released
        |> List.filter (fun (name, _) -> Set.contains name names)
        |> List.sortBy fst
        |> List.map snd

    released
    @ (creeps
       |> List.collect (fun creep ->
           outcomes
           |> List.tryPick (fun outcome -> Map.tryFind creep.Name outcome.Statuses)
           |> Option.defaultValue []))

/// The room a combat Task is fought in; None for every other Task.
let private fightRoomOf (task: Task) : string option =
    match task with
    | Guard room
    | Defend room
    | Assault room -> Some room
    | _ -> None

/// Keep still-valid assignments (anti-thrash) and greedily assign the rest,
/// one pass after another, each starting from the holdings the passes before
/// it left, `seed` before them all. A pass is one greedy match over the
/// shares it names, each a colony's (`contexts`, by index): its bodies are
/// priced in their own colony's view and Atlas, and the holders of a Task
/// are counted whichever colony's they are, its load keyed by the Task's id
/// alone (the world lane's FightMatch). A body in no share is
/// neither kept nor matched. `bystanders` are bodies no context decides
/// whose holdings `seed` counts.
///
/// On the world lane (`Tuning.FightLane`) a garrison body is held by
/// HomeHold (`Facts.homeHoldReleases`), the bodies already matched out of a home
/// leaving it.
let matchAcross
    (contexts: MatchContext list)
    (bystanders: CreepInfo list)
    (seed: Assignments)
    (passes: (int * MatchShare) list list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : MatchOutcome =
    let contexts = Array.ofList contexts

    // Each body's colony, as an index into `contexts`.
    let sides = System.Collections.Generic.Dictionary<string, int>()

    contexts
    |> Array.iteri (fun k ctx ->
        for creep in ctx.View.Creeps do
            sides.[creep.Name] <- k)

    let sideOf name =
        match sides.TryGetValue name with
        | true, k -> Some k
        | _ -> None

    let creeps =
        (contexts |> Seq.collect (fun ctx -> ctx.View.Creeps) |> List.ofSeq)
        @ bystanders

    let byIds =
        contexts
        |> Array.map (fun ctx -> ctx.Pool |> List.map (fun p -> taskId p.Task, p) |> Map.ofList)

    let fleeings = byIds |> Array.map (Map.tryFind (taskId Flee))

    // A Task's entry in the first pool that holds it: the fight Tasks' caps
    // are the world's whichever colony pools them (`Entry`).
    let pooledAnywhere tid =
        byIds |> Array.tryPick (Map.tryFind tid)

    let walkOf name task =
        match sideOf name with
        | Some k -> Atlas.walkTicks contexts.[k].Atlas name task
        | None -> None

    let tileOf name =
        match sideOf name with
        | Some k -> Atlas.creepTile contexts.[k].Atlas name
        | None -> None

    // Each living creep's remaining life and its body class, hoisted for the
    // tick: the capacity gate asks them once per holder per judged pair.
    let lives = creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    // What each body carries, for the budget cap. Energy alone, because the
    // refill ring takes nothing else.
    let carried = creeps |> List.map (fun c -> c.Name, c.Energy) |> Map.ofList

    let carriedBy name =
        Map.tryFind name carried |> Option.defaultValue 0

    // What each body's live Work comes to, for the Work budget.
    let works =
        creeps |> List.map (fun c -> c.Name, partCount c.Body Work) |> Map.ofList

    let workBy name =
        Map.tryFind name works |> Option.defaultValue 0

    let classes =
        (contexts
         |> Seq.collect (fun ctx ->
             ctx.View.Creeps
             |> List.map (fun c -> c.Name, bodyClassOf ctx.View.Tuning ctx.Atlas c))
         |> List.ofSeq)
        @ (bystanders
           |> List.map (fun c -> c.Name, (if isFighterBody c then Fighter else Light)))
        |> Map.ofList

    let classOf name = Map.tryFind name classes

    // Each living body's squad role, for an Assault's per-role caps.
    let roles =
        creeps
        |> List.choose (fun c -> squadRoleByName c.Name |> Option.map (fun role -> c.Name, role))
        |> Map.ofList

    let roleOf name = Map.tryFind name roles

    // Each living body's parts, for a Defend's per-row caps (#546).
    let bodies = creeps |> List.map (fun c -> c.Name, c.Body) |> Map.ofList

    let inRow row name =
        Map.tryFind name bodies |> Option.exists (CastRow.cut row name)

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
        && outlives (walkOf name task) (Some candidate.TicksToLive)

    // Whether each living creep is inside its lead, priced only if asked.
    let expiringNow =
        contexts
        |> Seq.collect (fun ctx ->
            ctx.View.Creeps
            |> List.map (fun c -> c.Name, lazy (expiring ctx.View ctx.Atlas ctx.Sizing c)))
        |> Map.ofSeq

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
        creeps
        |> List.choose (fun c ->
            if classOf c.Name = Some Heavy then
                tileOf c.Name |> Option.map (fun tile -> c.Name, tile)
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
            && (tileOf creep.Name
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
                | CapScope.Role _
                | CapScope.Row _ -> fun _ -> false

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
                | CapScope.Row row -> holders |> List.filter (inRow row) |> List.length

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
                    // A row's cap is its own bodies'; a body of another capped
                    // row passes it, and one of no capped row is refused.
                    | CapScope.Row row ->
                        if inRow row creep.Name then
                            counted scope < limit
                        else
                            capacity.Caps
                            |> Map.exists (fun other _ ->
                                match other with
                                | CapScope.Row own -> inRow own creep.Name
                                | _ -> false)
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
                match arrival.Value, walkOf name pooled.Task with
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
        contexts
        |> Array.map (fun ctx ->
            match ctx.View.Hostiles with
            | [] -> []
            | _ ->
                ctx.Pool
                |> List.choose (fun pooled ->
                    match pooled.Recall with
                    | Some(RecallPolicy.Struck room) -> Some(room, pooled)
                    | _ -> None))

    let graced k (creep: CreepInfo) tid =
        let ctx = contexts.[k]

        if standsInReach ctx.Threats ctx.Atlas creep.Name then
            None
        else
            lastSeenIn ctx.View tid

    // `threatened` asked once per `threatenedKey` (a Task and a body kind)
    // rather than once a pair, a colony's Atlas each.
    let threatenedByKey =
        contexts
        |> Array.map (fun _ -> System.Collections.Generic.Dictionary<string, bool>())

    let threatenedFor k (creep: CreepInfo) task =
        let ctx = contexts.[k]
        let key = threatenedKey ctx.Atlas creep task

        match threatenedByKey.[k].TryGetValue key with
        | true, answer -> answer
        | _ ->
            let answer = threatened ctx.Threats ctx.Atlas creep task
            threatenedByKey.[k].[key] <- answer
            answer

    // Work a runner has no walk round the Reach to (`walledOffBy`) is out of
    // its reach while it has other work it could take: priced as
    // `travelCostOf` crossed for it, a body was ranked onto work it was
    // never carried to and held it parked (W17S29 t996,770: loaded workers
    // on a struck rampart the raid stood round, the tower at its reserve for
    // a hundred ticks). A body with nothing else keeps it, parked at the
    // edge until a way opens, as before.
    let walledOffFrom =
        contexts |> Array.map (fun ctx -> walledOffBy ctx.View ctx.Atlas ctx.Threats)

    let otherWork = System.Collections.Generic.Dictionary<string, bool>()

    let walledOff k (creep: CreepInfo) task =
        let ctx = contexts.[k]

        walledOffFrom.[k] creep task
        && (match otherWork.TryGetValue creep.Name with
            | true, answer -> answer
            | _ ->
                let answer =
                    ctx.Pool
                    |> List.exists (fun other ->
                        not (safetyTier other.Task)
                        && not (threatenedFor k creep other.Task)
                        && applicable ctx.View ctx.Threats ctx.Atlas creep other
                        && Option.isSome (travelCostOf ctx.Threats ctx.Atlas creep.Name other.Task)
                        && not (walledOffFrom.[k] creep other.Task))

                otherWork.[creep.Name] <- answer
                answer)

    // HomeHold, the world lane's: a garrison body
    // leaves its homes only while each holds without it and without every
    // body already matched out of it (`Facts.homeHoldReleases`). A body leaves a
    // home for a fight in another room.
    let leaving (acc: Assignments) (body: CreepInfo) home =
        Map.tryFind body.Name acc
        |> Option.bind pooledAnywhere
        |> Option.bind (fun pooled -> fightRoomOf pooled.Task)
        |> Option.exists (fun room -> room <> home)

    let homeHeld k (acc: Assignments) (creep: CreepInfo) (pooled: PooledTask) =
        let ctx = contexts.[k]

        ctx.View.Tuning.FightLane = FightLane.World
        && match fightRoomOf pooled.Task with
           | Some room ->
               not (
                   Facts.homeHoldReleases
                       ctx.View
                       room
                       (Atlas.creepRoom ctx.Atlas creep.Name)
                       (leaving acc)
                       (fun home -> Map.tryFind home ctx.Threats.Arrivals |> Option.map fst)
                       creep
               )
           | None -> false

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
        k
        (escape: Lazy<bool>)
        acc
        (creep: CreepInfo)
        (pooled: PooledTask)
        : Result<int, RejectReason> =
        let ctx = contexts.[k]

        if threatenedFor k creep pooled.Task then
            Error RejectReason.Threatened
        elif not (applicable ctx.View ctx.Threats ctx.Atlas creep pooled) then
            Error RejectReason.Inapplicable
        elif homeHeld k acc creep pooled then
            Error RejectReason.Inapplicable
        else
            let cost = travelCostOf ctx.Threats ctx.Atlas creep.Name pooled.Task
            let arrival = lazy (Atlas.walkTicks ctx.Atlas creep.Name pooled.Task)

            if not (hasCapacity creep acc pooled arrival) && not escape.Value then
                Error RejectReason.CapacityFull
            else
                match cost with
                | None -> Error RejectReason.Unreachable
                | Some _ when walledOff k creep pooled.Task -> Error RejectReason.Unreachable
                | Some _ when not (outlivesTheStay ctx.View ctx.Atlas creep pooled.Task arrival) ->
                    Error RejectReason.Inapplicable
                | Some cost ->
                    match tooEarly ctx.View ctx.Atlas creep pooled.Task arrival with
                    | Some(walk, wait) -> Error(RejectReason.TooEarly(walk, wait))
                    | None -> Ok cost

    // Whether a holder is let go for a struck rampart's Repair (#481): a
    // non-fighter in the rampart's room whose Task that Repair outranks, and
    // which the cascade would give it. Scoped to struck ramparts, so an
    // ordinary change of rank churns nobody.
    let outranked k acc (creep: CreepInfo) (pooled: PooledTask) =
        not (List.isEmpty struckRepairs.[k])
        && classOf creep.Name <> Some Fighter
        && (match Atlas.creepRoom contexts.[k].Atlas creep.Name with
            | Some room ->
                struckRepairs.[k]
                |> List.exists (fun (struckIn, repair) ->
                    struckIn = room
                    && repair.Priority < pooled.Priority
                    && Result.isOk (gate k (lazy false) acc creep repair))
            | None -> false)

    // The holders let go for a kill this tick (#510, #516), decided once
    // against the remembered assignments, so the order the fold walks them in
    // moves nothing. Per kill nobody holds (a `RecallPolicy.Kill`): the
    // nearest holder within its walk of a Task it may be called off, for a
    // kill that outranks that Task and that the cascade would give it (the
    // gate says which bodies a kill admits). It is back once the kill is made.
    let calledOn k =
        let ctx = contexts.[k]
        let view = ctx.View

        let recalls =
            ctx.Pool
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
                        Map.tryFind tid byIds.[k],
                        view.Creeps |> List.tryFind (fun c -> c.Name = name)
                    with
                    | Some({
                               Recall = Some(RecallPolicy.Callable room | RecallPolicy.Kill(room, _))
                           } as pooled),
                      Some creep -> Some(creep, pooled, room)
                    | _ -> None)

            // Held by a body of this colony: the table is the shard's, and
            // another colony's guard on the same room's kill — one that
            // cannot catch what our ranger is called for — left our call
            // unmade (live t981,500, W17S26's haulers). On the world lane a
            // kill already held calls whoever its caps still seat, its holders
            // counted whichever colony's (the gate below): one pool.
            let world = view.Tuning.FightLane = FightLane.World

            recalls
            |> List.filter (fun (kill, _, _) ->
                world
                || not (
                    assignments
                    |> Map.exists (fun name tid ->
                        tid = taskId kill.Task
                        && view.Creeps |> List.exists (fun c -> c.Name = name))
                ))
            |> List.fold
                (fun (taken: Set<string>) (chase, killRoom, recall) ->
                    holders
                    |> List.filter (fun (creep, pooled, held) ->
                        not (Set.contains creep.Name taken)
                        && chase.Priority < pooled.Priority
                        && Facts.mayBeCalled view kills held killRoom creep)
                    |> List.choose (fun (creep, _, _) ->
                        travelCostOf ctx.Threats ctx.Atlas creep.Name chase.Task
                        |> Option.filter (fun cost ->
                            cost <= recall.Walk
                            && Result.isOk (
                                gate
                                    k
                                    (lazy false)
                                    (Map.remove creep.Name assignments)
                                    creep
                                    chase
                            ))
                        |> Option.map (fun cost -> (cost, creep.Name), creep.Name))
                    |> List.sortBy fst
                    |> List.tryHead
                    |> Option.map (fun (_, name) -> Set.add name taken)
                    |> Option.defaultValue taken)
                Set.empty

    let called = contexts |> Array.mapi (fun k _ -> calledOn k)

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
        match tileOf name with
        | Some _ ->
            match walkOf name pooled.Task with
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
                match pooledAnywhere tid with
                | Some pooled when Capacity.isBounded pooled.Capacity ->
                    holders |> List.sortBy (fun (name, _) -> tier pooled name, name)
                | _ -> holders)

    // The releases are reported in the assignment map's order whatever order
    // they were decided in, so the Verdict list stays in memory order. One
    // pass's bodies at a time, each beside its colony and the Tasks its
    // share offers.
    let keep
        (bodies: Map<string, int * CreepInfo * Set<string>>)
        (acc, loads, released)
        (name, tid)
        =
        let release reason =
            acc, loads, (name, Verdict.Released(name, tid, reason)) :: released

        match Map.tryFind name bodies with
        | None -> acc, loads, released
        | Some(k, creep, offered) ->
            let ctx = contexts.[k]
            let view, atlas, threats = ctx.View, ctx.Atlas, ctx.Threats
            let fleeing = fleeings.[k]

            match Map.tryFind tid byIds.[k] with
            // Kept, by the Verdict every other steady assignment answers
            // with. The Task is in no pool, so `graced` is the whole
            // judgement.
            | None when Option.isSome (graced k creep tid) ->
                Map.add name tid acc, hold loads tid, released
            | None -> release ReleaseReason.TaskGone
            // A Task across the line from the body, which its share does
            // not offer: a fighter's economy Task, or the fight Task of a
            // body disarmed since (`CreepInfo.Body` counts live parts).
            | Some _ when not (Set.contains tid offered) ->
                release (ReleaseReason.Rejected RejectReason.Inapplicable)
            // An expiring holder, or one of a relief pair, is kept over
            // capacity where a fresh candidate would be refused.
            // A holder Flee applies to runs, whatever its Task's ground
            // (#472): `threatened` reads the Task's tiles, and a body
            // walking past a raid toward safe ground is struck all the way.
            // A fight Task's holder keeps it (#533): its ground is the
            // fight's answer to the Reach.
            | Some pooled when
                not (safetyTier pooled.Task)
                && fleeing |> Option.exists (fun flee -> applicable view threats atlas creep flee)
                ->
                release (ReleaseReason.Rejected RejectReason.Threatened)
            | Some pooled when outranked k acc creep pooled -> release ReleaseReason.Outranked
            | Some _ when Set.contains creep.Name called.[k] -> release ReleaseReason.Called
            // The one hold rule (#535, `Threats.holdsFight`): a fight begun
            // is kept while its holder can still strike a target there,
            // whatever the room's kind, capacity or muster.
            // It ends for a holder #533's flee takes: out-damaged where it
            // stands and losing its own exchange there (a guard shot
            // rooted beside shooters it can no longer reach). On the world
            // lane the hold keeps a seat the caps still have, holders read
            // nearest first (`remembered`): a far holder yields to one at
            // the fight.
            | Some pooled when
                Threats.holdsFight view atlas threats creep pooled.Task
                && (view.Tuning.FightLane = FightLane.PerColony
                    || hasCapacity
                        creep
                        acc
                        pooled
                        (lazy (Atlas.walkTicks atlas creep.Name pooled.Task)))
                && not (
                    fleeing |> Option.exists (fun flee -> applicable view threats atlas creep flee)
                    && (match pooled.Task with
                        | Guard room ->
                            not (Facts.bodyWins view room (partCount creep.Body) creep.Hits.Hits)
                        | _ -> false)
                )
                ->
                Map.add name tid acc, hold loads tid, released
            | Some pooled ->
                let escape = lazy (isExpiring creep.Name || reliefPair acc creep pooled)

                match gate k escape acc creep pooled with
                | Error reason -> release (ReleaseReason.Rejected reason)
                | Ok _ -> Map.add name tid acc, hold loads tid, released

    // The fresh candidate's reading of the same cascade, over the Tasks its
    // share offers: scored on the full key where a holder is merely kept, and
    // with no escape from the capacity gate. The key's rank is `matchRank`'s,
    // which reads the creep's cheapest intake, so the pool is gated before any
    // row is scored. Each pooled Task beside its id, worked out once for the
    // tick, a colony's pool each.
    let ids =
        contexts
        |> Array.map (fun ctx -> ctx.Pool |> List.map (fun pooled -> taskId pooled.Task, pooled))

    let gateAmong k offered acc (creep: CreepInfo) =
        offered
        |> List.map (fun (tid, pooled) -> tid, pooled, gate k (lazy false) acc creep pooled)

    let score
        k
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
                    matchRank contexts.[k].View.Tuning nearest pooled cost,
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
    let idleReason k (rows: (PooledTask * Candidate) list) =
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

        if List.isEmpty contexts.[k].Pool && not contexts.[k].PooledElsewhere then
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
    // take that Task are gated again, and only on it — but where the match
    // takes a garrison body out of a room on the world lane, every ranger
    // still pending is gated again whole, the bodies left there being fewer
    // (HomeHold).
    let entryOf loads i k (creep: CreepInfo) offered gated =
        let rows = score k loads creep gated
        let keyed = keyedOf rows

        let top =
            match keyed with
            | [] -> None
            | _ -> Some(keyed |> List.minBy (fun (key, _, _) -> key))

        i, k, creep, offered, gated, rows, keyed, top

    let rec fresh acc loads pending decided =
        let best =
            pending
            |> List.choose (fun ((i, _, _, _, _, _, _, top) as entry) ->
                top |> Option.map (fun (key, _, _) -> (key, i), entry))

        match best with
        | [] ->
            let idle =
                pending
                |> List.map (fun (_, k, creep: CreepInfo, _, _, rows, _, _) ->
                    creep.Name, (Verdict.Unassigned(creep.Name, idleReason k rows), rows))

            acc, loads, (decided, idle) ||> List.fold (fun m (name, entry) -> Map.add name entry m)
        | _ ->
            let _, (i, k, creep, _, _, rows, keyed, top) = best |> List.minBy fst
            let (_, task, _) as top = Option.get top
            let tid = taskId task
            let acc = Map.add creep.Name tid acc
            let loads = hold loads tid

            let leaves =
                contexts.[k].View.Tuning.FightLane = FightLane.World
                && isRangerRowBody creep
                && match fightRoomOf task, Atlas.creepRoom contexts.[k].Atlas creep.Name with
                   | Some room, Some here -> room <> here
                   | _ -> false

            // A body that could take the Task is gated on it again and its row
            // rewritten in place. Only two readings reach past the row: the
            // Task was its best (its top is read again), or the Task just
            // closed to it and was its nearest intake or its one fight —
            // `matchRank` and Flee read those — and then its rows are scored
            // again whole.
            let rest =
                pending
                |> List.filter (fun (j, _, _, _, _, _, _, _) -> j <> i)
                |> List.map (fun ((j, side, other, offered, gated, rows, _, otherTop) as entry) ->
                    match
                        gated
                        |> List.tryPick (fun (id, pooled, result) ->
                            match result with
                            | Ok cost when id = tid -> Some(pooled, cost)
                            | _ -> None)
                    with
                    | _ when leaves && isRangerRowBody other ->
                        entryOf loads j side other offered (gateAmong side offered acc other)
                    | None -> entry
                    | Some(pooled, cost) ->
                        let result =
                            if Capacity.isBounded pooled.Capacity then
                                gate side (lazy false) acc other pooled
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
                            entryOf loads j side other offered gated
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

                            j, side, other, offered, gated, rows, keyed, top)

            fresh
                acc
                loads
                rest
                (Map.add
                    creep.Name
                    (Verdict.Matched(creep.Name, tid, factorOf keyed top), rows)
                    decided)

    // One pass: its bodies' remembered holdings judged, then its fresh match
    // over the Tasks each one's share offers, from the holdings the passes
    // before it left. A body is in one share, so what one pass keeps or
    // matches is never a body another judges; they meet only on a Task both
    // offer (Flee, which no cap bounds).
    let pass (acc, loads, released, kept, decided) (shares: (int * MatchShare) list) =
        let offers =
            shares
            |> List.map (fun (k, share) ->
                k, share, share.Offered |> List.map (fun pooled -> taskId pooled.Task, pooled))

        let bodies =
            offers
            |> List.collect (fun (k, share, offered) ->
                let ids = offered |> List.map fst |> Set.ofList
                share.Bodies |> List.map (fun c -> c.Name, (k, c, ids)))
            |> Map.ofList

        let offeredTo =
            offers
            |> List.collect (fun (_, share, offered) ->
                share.Bodies |> List.map (fun c -> c.Name, offered))
            |> Map.ofList

        let acc, loads, released =
            ((acc, loads, released), remembered) ||> List.fold (keep bodies)

        let kept =
            (kept, acc)
            ||> Map.fold (fun kept name tid ->
                if Map.containsKey name bodies then
                    Map.add name tid kept
                else
                    kept)

        // Every body in view order, colony after colony: the order a tie
        // goes to.
        let next, loads, decided =
            contexts
            |> Seq.mapi (fun k ctx -> ctx.View.Creeps |> List.map (fun creep -> k, creep))
            |> List.concat
            |> List.mapi (fun i (k, creep) -> i, k, creep)
            |> List.filter (fun (_, k, creep) ->
                match Map.tryFind creep.Name bodies with
                | Some(side, _, _) -> side = k && not (Map.containsKey creep.Name acc)
                | None -> false)
            |> List.map (fun (i, k, creep) ->
                let offered = offeredTo.[creep.Name]
                entryOf loads i k creep offered (gateAmong k offered acc creep))
            |> fun pending -> fresh acc loads pending decided

        next, loads, released, kept, decided

    let seedLoads = (Map.empty, seed) ||> Map.fold (fun loads _ tid -> hold loads tid)

    let next, nextLoads, released, kept, decided =
        ((seed, seedLoads, [], Map.empty, Map.empty), passes) ||> List.fold pass

    // A verbose fresh creep's rows over the whole pool, in pool order: its
    // share's, then the Tasks it was not offered, each at the gate it fails
    // against the match as it ended. Most fail ahead of capacity, where that
    // state moves nothing; a sapper lamed to more Work than Move passes the
    // Thorium mine's Harvest that far and reads its capacity as it ended.
    let wholeRows k (creep: CreepInfo) (rows: (PooledTask * Candidate) list) =
        let offered = rows |> List.map (fun (pooled, _) -> taskId pooled.Task) |> Set.ofList
        let others = ids.[k] |> List.filter (fun (tid, _) -> not (Set.contains tid offered))

        match others with
        | [] -> rows
        | _ ->
            let order = ids.[k] |> List.mapi (fun i (tid, _) -> tid, i) |> Map.ofList

            rows @ score k nextLoads creep (gateAmong k others next creep)
            |> List.sortBy (fun (pooled, _) ->
                Map.tryFind (taskId pooled.Task) order
                |> Option.defaultValue System.Int32.MaxValue)

    // One status Verdict per creep of a share, a verbose creep's Scoring
    // before it: a fresh creep's rows are the ones it was matched on, its own
    // claim not yet among them; a kept creep's are the whole pool judged
    // against the match with its own claim and crowding set aside (#95), so a
    // held single-Seat Task reads as the winning row.
    let statuses =
        contexts
        |> Seq.mapi (fun k ctx ->
            ctx.View.Creeps
            |> List.choose (fun creep ->
                let judged =
                    match Map.tryFind creep.Name kept with
                    | Some tid ->
                        Some(
                            Verdict.Kept(creep.Name, tid),
                            lazy
                                (let without =
                                    Map.add tid (max 0 (load nextLoads tid - 1)) nextLoads

                                 score
                                     k
                                     without
                                     creep
                                     (gateAmong k ids.[k] (Map.remove creep.Name next) creep))
                        )
                    | None ->
                        Map.tryFind creep.Name decided
                        |> Option.map (fun (status, rows) ->
                            status, lazy (wholeRows k creep rows))

                judged
                |> Option.map (fun (status, rows) ->
                    creep.Name,
                    if Set.contains creep.Name verbose then
                        [ Verdict.Scoring(creep.Name, rows.Value |> List.map snd); status ]
                    else
                        [ status ])))
        |> List.concat
        |> Map.ofList

    {
        Assigned = next
        Released = released
        Statuses = statuses
    }

/// One colony's match, its shares one pass after another (`matchAcross`):
/// its next assignments and its Verdicts — releases first in memory order,
/// then one status Verdict per creep of a share in view order, each
/// preceded, for a creep on the verbose list, by its Scoring Verdict, the
/// whole pool judged against the same state its status was decided from.
///
/// `pooledElsewhere`: whether a part of the pool not handed in holds Tasks —
/// a fight-only decide's economy (`decideFight`) — so an idle body there is
/// never told there are none.
let matchCreepsBeside
    (pooledElsewhere: bool)
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (threats: Threats)
    (pool: PooledTask list)
    (shares: MatchShare list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : Assignments * Verdict list =
    let outcome =
        matchAcross
            [
                {
                    View = view
                    Atlas = atlas
                    Sizing = sizing
                    Threats = threats
                    Pool = pool
                    PooledElsewhere = pooledElsewhere
                }
            ]
            []
            Map.empty
            (shares |> List.map (fun share -> [ 0, share ]))
            assignments
            verbose

    outcome.Assigned, verdictsOf view.Creeps [ outcome ]

/// `matchCreepsBeside` with the whole pool handed in.
let matchCreeps
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (threats: Threats)
    (pool: PooledTask list)
    (shares: MatchShare list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : Assignments * Verdict list =
    matchCreepsBeside false view atlas sizing threats pool shares assignments verbose
