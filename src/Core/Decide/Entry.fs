/// The seam: `decide : ColonyView -> Assignments -> Intent list * Assignments`,
/// the single entry the shell calls, and the census signature the plan memo is
/// keyed on.
[<AutoOpen>]
module Fabot.Core.Decide.Entry

open Fabot.Core
open Fabot.Core.Types

/// Join the Matcher's Assignments back onto the Planner's pool: the tick's
/// assigned Task per creep, as data for the Emitter and the Resolver.
let private assignedTasks (tasks: Task list) (assignments: Assignments) : Map<string, Task> =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> Map.tryFind tid byId |> Option.map (fun t -> name, t))
    |> Map.ofList

/// ADR-0017
/// ADR-0044
/// The census signature: a string over exactly the inputs the census-derived
/// plans read — the standing structures, our pending sites, the tiles another
/// player's sites hold (#248), the controller level, the home room, who holds
/// each projected room, and every colony's stage. Creeps, stores, hits, piles,
/// hostiles, the bank and the tick are invisible to it. The hold is signed as
/// the *rate* and not the reservation's `TicksToEnd`, which decays every tick.
/// It signs every projected room, because the hauler quota reads every one and
/// the round trip floods that room's grid; the pending census spans every room
/// too, because the walk table's far leg floods the goal room's grid.
///
/// Per room first, flat second (#388): the plan reads the whole census, but the
/// walk tables beside it read one room's grid each and are evicted per room
/// (`PlanMemo.RoomSignatures`, `Atlas.evictRooms`). One walk builds both; what
/// a reader compares is equality, so the layout of the string is free.
let private signaturesOf (view: ColonyView) : Map<string, string> * string =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial

    let joined (ids: ResizeArray<string>) =
        ids |> List.ofSeq |> List.sort |> String.concat ";"

    // The home controller's level: it gates the allowance the placement
    // filters on and, derived, the horizon the clustered placement is sized
    // at, so a level-up moves which tiles the plan holds. The road
    // reservation no longer reads it, so removing this key would be wrong
    // for the cluster alone. The bank Capacity the hauler quota prices at is
    // a function of it too.
    let level =
        view.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    // Every colony of ours and its stage, in room-name order: a room leaving
    // the map moves the string as surely as one changing stage does.
    let stages =
        view.Stages
        |> Map.toList
        |> List.map (fun (room, stage) -> $"{room}:{stage}")
        |> String.concat ","

    // One walk for the three id-keyed halves: each room's placed ids, each
    // looked up once in the flat kind census, sorted per half afterwards —
    // the same strings in the same order as three walks of the census with a
    // placement search per id, because a placed id reaches exactly one layer.
    // An id placed in a layer but absent from the census is skipped. The old
    // shape was 4% of a `pair --level 7` tick by inclusive samples
    // (2026-09-18, #370), most of it `placementOf` searching every room per
    // id; the A/B came back inside the clock's spread.
    let rooms =
        spatial.Rooms
        |> Map.map (fun room layer ->
            let standingIds = ResizeArray<string>()
            let pendingIds = ResizeArray<string>()
            let mineralIds = ResizeArray<string>()

            // The Thorium deposits on their tiles: the Layout reads them
            // twice (the extractor's tile and the mineral container's Seats)
            // and `Atlas.workingGroundIn` at every level. The mod deletes an
            // exhausted deposit outright, and unsigned, the memo would keep
            // handing back a plan naming a container on the Seat of a rock
            // that is gone.
            for KeyValue(id, tile) in layer.TargetPositions do
                match Map.tryFind id spatial.TargetKinds with
                | Some(Structure built) -> standingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
                | Some(Site built) -> pendingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
                | Some Mineral -> mineralIds.Add $"Mineral@{room}:{tile.X},{tile.Y}"
                | _ -> ()

            // The tiles another player's construction sites hold (#248), with
            // the one thing known about such a site in the kind slot: a
            // rival's site reaches the projection as a tile and nothing else
            // (`RoomLayer.RivalSites`). The Layout's tile clause reads these,
            // so a rival building on the container's pick between two ticks
            // must throw the plan away; unsigned, the memo would hand back
            // the very `PlaceConstructionSite` the engine is refusing. Signed
            // for every projected room, deliberately wider than the one
            // reader (`planLayout` plans the home room alone): over-invalidating
            // is a recompute where a missed input is a stall until a reset.
            let rivals =
                layer.RivalSites
                |> Set.toList
                |> List.map (fun tile -> $"Rival@{room}:{tile.X},{tile.Y}")
                |> String.concat ";"

            // The rate this room's sources are priced at, and the empty rate
            // for a room vision answered for not at all — a third answer,
            // different from either rate.
            let held =
                Map.tryFind room view.RoomControl
                |> Option.map (heldRateOf >> string)
                |> Option.defaultValue ""

            let stage =
                Map.tryFind room view.Stages |> Option.map string |> Option.defaultValue ""

            $"{home}|{level}|{stage}|{held}|{joined standingIds}|{joined pendingIds}|{rivals}|{joined mineralIds}")

    // The rooms joined on a newline, which no field can carry: the room set
    // itself is signed by the join.
    let flat =
        rooms
        |> Map.toList
        |> List.map (fun (room, signature) -> $"{room}={signature}")
        |> String.concat "\n"

    rooms, $"{home}|{level}|{stages}|{flat}"

let censusSignature (view: ColonyView) : string = snd (signaturesOf view)

/// The per-room half of the census signature (#388): what
/// `PlanMemo.RoomSignatures` carries and `Atlas.evictRooms` is told the
/// difference of.
let roomSignatures (view: ColonyView) : Map<string, string> = fst (signaturesOf view)

/// The decision seam: a colony view in, Decision out, with this colony's
/// movement left unarbitrated on it — a room's traffic is not one colony's
/// decision, so the caller folds every colony's together and arbitrates each
/// room once (`resolveRooms`). Plan, match, emit, move, beside the colony steps
/// (spawns, sites), all pricing from one Atlas built up front.
let decideUnarbitrated
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    (turn: ReplanTurn)
    : Decision =
    let signedRooms, signature = signaturesOf view
    // The signature is read before the Atlas is built, because the Atlas is
    // one of the things it decides: the plan is recalled whole on the flat
    // signature, the tables beside it per room on the per-room half.
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    // ADR-0032
    // The memo's tables go to the Atlas whatever the plan's signature did:
    // every entry is a pure function of the walking grids and Seam bands of
    // the rooms it names, which the per-room signatures sign. An ask the
    // decision layer narrowed for itself — a Guard's ring off this tick's
    // Threats — keys on tiles that move every tick and rides
    // `Atlas.TickFarFields` instead. The tables used to go whole or empty on
    // the flat signature, which made every replan a re-flood of the lot;
    // measured by count 2026-09-20 (#388, `docs/profiling.md`): 109,258 heap
    // pops a tick against 9,554 quiet, and evicting per room took the
    // perturbed tick to 91,920 with the harness moving the home room.
    let walks, farFields =
        match memo with
        | Some m ->
            m.Walks,
            {
                SeamWalks = m.SeamWalks
                PerCensus = m.FarFields
            }
        | None -> WalkTable(), FarFieldMemo.empty ()

    let atlas = Atlas.ofViewRecalling walks farFields view

    // The rooms whose census moved since the tables were filled, including a
    // room on one side only. Evicted before any flood is forced. Read off the
    // memo's per-room stamp and not settled by the plan's recall: on a
    // deferred turn the two are different ticks', and a recalled plan
    // carrying tables a dark tick filled is exactly #372.
    match memo with
    | Some m when m.RoomSignatures <> signedRooms ->
        let moved =
            Set.union (Map.keys m.RoomSignatures |> Set.ofSeq) (Map.keys signedRooms |> Set.ofSeq)
            |> Set.filter (fun room ->
                Map.tryFind room m.RoomSignatures <> Map.tryFind room signedRooms)

        if not moved.IsEmpty then
            Atlas.evictRooms atlas moved
    | _ -> ()

    // Whose turn it is to re-plan (#357, `turn`). A stale memo is not a
    // wrong plan, only an old one — the reservations are level-blind and a
    // site already in the world outlives the Intent that placed it — while
    // four colonies re-planning in the same tick measured 487 ms of the
    // engine's 500 ms ceiling. A colony whose turn has not come serves what
    // it has; both keep a signature this tick's census cannot match, so the
    // plan is owed and the next turn pays it.
    let plan =
        match recalled with
        // The recalled memo goes on with its plan and the tables this tick
        // filled. The room stamp is **this tick's** all the same: on the tick
        // a census returns after a deferred turn (#372) the stamp is the dark
        // tick's, and handed on as it was it would evict and re-flood that
        // room every tick until the next replan.
        | Some m -> { m with RoomSignatures = signedRooms }
        // A colony whose turn has not come (#357) serves its stale plan but
        // carries the tick's own tables on it, stamped with this tick's
        // per-room signatures: a census that moves and moves back recalls the
        // plan and evicts the entries the intermediate tick laid (#372).
        | None when turn = ReplanTurn.Waiting ->
            match memo with
            | Some stale ->
                { stale with
                    RoomSignatures = signedRooms
                    Walks = walks
                    SeamWalks = farFields.SeamWalks
                    FarFields = farFields.PerCensus
                }
            | None -> PlanMemo.deferred signedRooms walks farFields.SeamWalks farFields.PerCensus
        | None ->
            let siteIntents, servedFootings, unservedFootings, unroutedTrunks, deferredContainers =
                planLayout view atlas

            let quota, demandRows, load = haulerDemandOf view atlas

            {
                Signature = signature
                RoomSignatures = signedRooms
                SiteIntents = siteIntents
                UnservedFootings = unservedFootings
                ServedFootings = servedFootings
                UnroutedTrunks = unroutedTrunks
                DeferredContainers = deferredContainers
                HaulerQuota = quota
                HaulerDemand = demandRows
                HaulerLoad = load
                Walks = walks
                SeamWalks = farFields.SeamWalks
                FarFields = farFields.PerCensus
            }

    // The tick's Threats, derived once and shared by every reader.
    let threats = threatsOf view atlas

    // The outpost's source containers, beside the memoised Layout and never
    // inside it: derived fresh every tick for the reason on the rule itself.
    let outpostSiteIntents = planOutpostContainers view atlas

    let defenseIntents = planSafeMode view atlas @ planFire view atlas

    // The consignment's send (#349), beside the defence reflexes because it is
    // the same kind of thing: a structure's own verb, read off the view.
    let consignmentIntents = planConsignment view

    // The pool is derived before the spawns, and the dependency runs one way:
    // the worker row's floor asks the pool, and nothing in the pool reads a
    // spawn Intent. The outpost chain's answers are derived once (#383):
    // before this `claimTargets` ran 11.4 times a tick and
    // `outpostControllers` 9.1, each walking the kind census.
    let outposts = outpostFactsOf view

    let sizing = rowSizingOf view atlas outposts

    // The narrow facts the Planner reads about the colony's own decisions:
    // the task ids its living creeps hold, and which holders still carry
    // Thorium, off the table the Matcher wrote last tick. Derived before the
    // pool, because this is the one place with both the assignments and the
    // fleet.
    let held = heldTaskFacts view assignments

    let tasks = planTasks view atlas threats held outposts

    // Every entry's priority and capacity, set once here and read by the
    // Matcher and the mover.
    let pool = planPool view atlas tasks

    let spawnIntents, quotas =
        planSpawns view atlas outposts sizing threats tasks plan.HaulerQuota

    let next, verdicts = matchCreeps view atlas sizing threats pool assignments verbose
    let assigned = assignedTasks tasks next

    // The other half of the vision grace (#151): the holders the Matcher kept
    // against a Task in no pool, and the room each one's target was last seen
    // in. They reach the Emitter as nothing and the mover as a crossing. A
    // keep with no pooled Task is a graced one by construction.
    let crossings =
        next
        |> Map.toList
        |> List.filter (fun (name, _) -> not (Map.containsKey name assigned))
        |> List.choose (fun (name, tid) ->
            lastSeenIn view tid |> Option.map (fun room -> name, room))
        |> Map.ofList

    let taskIntents = emit view atlas threats assigned

    // A task's pickup owns this creep's channel. Otherwise the reflex picks
    // its last reachable pile, preserving the engine's former last-write choice
    // without emitting overwritten calls. Different creeps may still share a pile.
    let taskPickers =
        taskIntents
        |> List.choose (function
            | PickupPile(name, _) -> Some name
            | _ -> None)
        |> Set.ofList

    let pickupIntents =
        planPickups view atlas
        |> List.filter (function
            | PickupPile(name, _) -> not (Set.contains name taskPickers)
            | _ -> true)

    // The signature (#381), shaped after the pickup reflex: no walk, no Task,
    // no competition.
    let signIntents = planSignatures view atlas Colony.signature

    let intents =
        defenseIntents
        @ consignmentIntents
        @ spawnIntents
        @ plan.SiteIntents
        @ outpostSiteIntents
        @ signIntents
        @ pickupIntents
        @ taskIntents
        |> Fabot.Core.IntentPlan.create
        |> function
            | Ok selected -> selfHeal view selected |> Fabot.Core.IntentPlan.intents
            | Error conflict -> invalidOp $"Conflicting creep intents: %A{conflict}"

    {
        Intents = intents
        Assignments = next
        Memo = plan
        Verdicts = verdicts
        Movement = movementOf view atlas threats pool assigned crossings verbose
        Quotas =
            { quotas with
                HaulerLoad = plan.HaulerLoad
                HaulerDemand = plan.HaulerDemand
            }
    }

/// The decision seam a shell with one colony — and the whole suite — asks for:
/// `decideUnarbitrated`'s answer with this colony's movement folded back in
/// through `resolveRooms`.
let decide
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    // One colony alone is always its own turn to re-plan: the budget #357
    // added staggers colonies against each other.
    let decision = decideUnarbitrated view assignments verbose memo ReplanTurn.Now
    let moveIntents, moveVerdicts = resolveRooms [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }
