/// The seam: `decide : ColonyView -> Assignments -> Intent list * Assignments`,
/// the single entry the shell calls, and the census signature the plan memo is
/// keyed on (ADR 0017).
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

/// The census signature (ADR 0017): a string over exactly the inputs the
/// census-derived plans read — the (kind, position) census of standing
/// structures, the (kind, position) census of pending sites of ours, the tiles
/// another player's sites hold (#248), the controller
/// level, the home room's name, who holds each room the projection carries, and
/// every colony's [[stage]]. Any one input moving moves the signature;
/// everything else a view carries — creeps, stores, hits, piles, hostiles,
/// banked energy, the tick — is invisible to it. The hauler quota rides the
/// same signature on two load-bearing derivations, and both are covered here
/// rather than assumed. The bank Capacity it sizes bodies from is a function of
/// the standing census and the level, both signed above. Since ADR 0042 it also
/// prices each container at its source's own output, read off `RoomControl` — a
/// **vision** fact and not a census one, so it is signed explicitly, and as the
/// *rate* rather than the reservation's `TicksToEnd`, which decays every tick.
/// **It signs every projected room**, a deliberate widening, because ADR 0042's
/// hauler quota is the entry that reads a second one. The standing census names
/// the room in each entry, two rooms holding the same coordinates, and spans
/// every *kind* rather than the containers alone, because the round trip floods
/// that room's step-weight grid. The pending census spans every projected room
/// too: the walk table's far leg floods the *goal* room's grid, and a site
/// outside home closes a tile there. The rival half spans every projected room
/// as well, and that one is wider than its reader on purpose rather than by
/// derivation — the comment on it says why.
///
/// **Per room first, flat second** (#388). The signature is one string over
/// the whole census because the *plan* reads the whole census; the three walk
/// tables the memo carries beside the plan read one room's grid each, or one
/// chain's, and are kept per room (`PlanMemo.RoomSignatures`,
/// `Atlas.evictRooms`). So the per-room map is the primitive — each entry
/// signs everything the census says about that room, with the colony-wide
/// inputs (home, level) folded in so a level-up moves every room — and the
/// flat signature is that map joined, under the colony-wide inputs and every
/// colony's stage. One walk of the census builds both; what a reader compares
/// is equality and never the literal, so the layout of the string is free.
let private signaturesOf (view: ColonyView) : Map<string, string> * string =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial

    let joined (ids: ResizeArray<string>) =
        ids |> List.ofSeq |> List.sort |> String.concat ";"

    // The home controller's level. Two of the Layout's readings of it since
    // ADR 0063 and not one: it gates the allowance the placement filters on,
    // and — derived, `Tuning.horizonOf` — it is the horizon the clustered
    // *placement* is sized at. So a level-up moves which tiles the plan holds
    // and not only which of them go up this tick, and a memo not keyed here
    // would hand a room that had just levelled yesterday's cluster: #341's own
    // failure, arriving through the memo instead of through a constant.
    //
    // The **road** half stopped reading the level with ADR 0064 — the
    // reservation the trunks dodge is sized at `allowanceOf`'s ceiling — so
    // one of the three readings above is gone. The other two remain, so the
    // key is no looser than it was: every level-up still moves the plan,
    // through the allowance gate and through the placement horizon, and the
    // level is read here for the bank Capacity the hauler quota prices
    // besides (ADR 0017, ADR 0044). What is worth knowing is that removing
    // this key would now be wrong for the *cluster* alone and not for the
    // roads.
    let level =
        view.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    // Every colony of ours and the stage it stands at, in room-name order:
    // the map is already sorted by key, and a room leaving it moves the
    // string as surely as one changing stage does. Signed flat over every
    // colony, and per room for the room that is a colony.
    let stages =
        view.Stages
        |> Map.toList
        |> List.map (fun (room, stage) -> $"{room}:{stage}")
        |> String.concat ","

    // One walk for the three id-keyed halves: each room's placed ids, each
    // looked up once in the flat kind census, sorted per half afterwards — so
    // the strings are the same strings in the same order as three walks of
    // the census with a placement search per id used to build, because one
    // object stands in one room (ADR 0041) and a placed id reaches exactly
    // one layer. An id placed in a layer but absent from the census is
    // skipped, as the census-first walk skipped an id the projection placed
    // nowhere. The old shape was 4% of a `pair --level 7` tick by inclusive
    // samples (`npm run profile -- 300 40 --scenario pair --level 7`,
    // 2026-09-18, #370), most of it `placementOf` searching every room per
    // id; the A/B came back inside the clock's spread, and this ships on the
    // exactness argument, not on the clock.
    let rooms =
        spatial.Rooms
        |> Map.map (fun room layer ->
            let standingIds = ResizeArray<string>()
            let pendingIds = ResizeArray<string>()
            let mineralIds = ResizeArray<string>()

            // The Thorium deposits go in the third list, each on its tile
            // (ADR 0057 decision 1). Signed on ADR 0044's rule that the memo
            // signs the union of its readers: since this ticket the Layout
            // reads them twice — the extractor's tile *is* the deposit's,
            // and the mineral container is seated on its Seats — and
            // `Atlas.workingGroundIn` reads them at every level, so a
            // deposit that arrived or left moves the clustered ordering
            // itself. Leaving is the live case and not a hypothetical: the
            // mod deletes an exhausted Thorium deposit outright, and
            // unsigned, the memo would keep handing back a plan naming a
            // container on the Seat of a rock that is gone.
            for KeyValue(id, tile) in layer.TargetPositions do
                match Map.tryFind id spatial.TargetKinds with
                | Some(Structure built) -> standingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
                | Some(Site built) -> pendingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
                | Some Mineral -> mineralIds.Add $"Mineral@{room}:{tile.X},{tile.Y}"
                | _ -> ()

            // The tiles another player's construction sites hold (#248),
            // named the way ADR 0044's consequence names every census input
            // — `{kind}@{room}:{x},{y}` — with the one thing we know about
            // such a site standing in the kind slot, that it is not ours: a
            // rival's site reaches the projection as a tile and nothing else
            // (`RoomLayer.RivalSites`), so there is no id to join it through
            // the census above and no built kind to name. Signed at all
            // because ADR 0044 makes the memo sign the union of its readers:
            // the Layout's tile clause reads these, so a rival building on
            // the container's pick between two ticks must throw the plan
            // away. Unsigned, the memo would hand back the very
            // `PlaceConstructionSite` the engine is refusing.
            //
            // Signed for **every** projected room, which is deliberately
            // wider than that reader — `planLayout` plans the home room
            // alone, and `planOutpostContainers`, the only other caller of
            // `Atlas.collidingSiteTilesIn`, is off this memo by a rule of its
            // own. Wide on ADR 0044's own ground: over-invalidating is the
            // cheap error (a recompute) where a missed input is the expensive
            // one (a stall until a reset), the standing and pending halves
            // widened per room the same way, and a signature cut to today's
            // one reader is a signature gap the tick another rule joins the
            // memo.
            let rivals =
                layer.RivalSites
                |> Set.toList
                |> List.map (fun tile -> $"Rival@{room}:{tile.X},{tile.Y}")
                |> String.concat ";"

            // The rate this room's sources are priced at this tick, and the
            // empty rate for a room vision answered for not at all — the
            // third answer the quota gives, and a different one from either
            // rate (ADR 0004).
            let held =
                Map.tryFind room view.RoomControl
                |> Option.map (heldRateOf >> string)
                |> Option.defaultValue ""

            let stage =
                Map.tryFind room view.Stages |> Option.map string |> Option.defaultValue ""

            $"{home}|{level}|{stage}|{held}|{joined standingIds}|{joined pendingIds}|{rivals}|{joined mineralIds}")

    // The rooms joined on a newline, which no field can carry: the room set
    // itself is signed by the join, as the per-room rate used to sign it.
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

/// The decision seam: a colony view in — with the verbose list of creep names
/// owed the manufactured-evidence Verdicts and the previous tick's plan memo —
/// Decision out, with this colony's movement left unarbitrated on it. A room's
/// traffic is not one colony's decision, so the last step of the pipeline is
/// not taken here: what comes out is the room's Move Intents, and the caller
/// folds every colony's together and arbitrates each room once
/// (`resolveRooms`). The tick's pipeline is visible here — plan, match, emit,
/// move — beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same flood
/// (ADR 0004).
let decideUnarbitrated
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    (turn: ReplanTurn)
    : Decision =
    let signedRooms, signature = signaturesOf view
    // The signature is read before the Atlas is built, because the Atlas
    // is one of the things it decides: the plan is recalled whole on the
    // flat signature, and the three tables beside it are recalled **per
    // room** on the per-room half (#388, ADR 0032).
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    // The memo's tables go to the Atlas whatever the plan's signature did —
    // the spawn walks, and the far legs of every cross-room price on the same
    // terms (ADR 0070, `docs/research/cpu-headroom.md` §5.1): every entry is
    // a pure function of the walking grids and Seam bands of the rooms it
    // names, which the per-room signatures sign, and of nothing else — the
    // far leg prices no standing crowd under any pricing, so nothing here is
    // keyed on the crowd. What crosses this line is the far fields whose
    // **origins** the census signs as well, which is every ask derived from a
    // Task; an ask the decision layer narrowed for itself — a Guard's ring
    // off this tick's Threats — keys on tiles that move every tick, and rides
    // the Atlas's own per-tick table instead (`Atlas.TickFarFields`).
    //
    // The tables used to go whole or empty on the flat signature, which made
    // every replan a re-flood of the lot. Measured by count 2026-09-20
    // (#388, `docs/profiling.md`): `reactor --level 7 --census-every 1` ran
    // 109,258 heap pops a tick against 9,554 quiet, and evicting per room
    // took the perturbed tick to 91,920 — with the harness moving the
    // **home** room, through which every chain runs, so that is the floor of
    // what the eviction keeps. Now the entries whose rooms moved are evicted
    // below and the rest ride on, a plan or no plan.
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

    // The rooms whose census moved since the tables were filled — a room
    // whose entry differs, and a room on one side only, which is a room that
    // joined or left the projection. Evicted before any flood is forced, so
    // nothing this tick prices reads an entry a moved room laid. Read off
    // the memo's per-room stamp and **not** settled by the plan's recall: on
    // a deferred turn the two are different ticks' (the stale plan under its
    // signature, this tick's tables under theirs), and a recalled plan
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
    // wrong plan, only an old one — the reservations are level-blind (ADR
    // 0064) and a site already in the world outlives the Intent that placed
    // it — while four colonies re-planning in the same tick measured 487 ms
    // of the engine's 500 ms ceiling. So a colony whose turn has not come
    // serves what it has: its stale memo, or `PlanMemo.deferred` if it has
    // none at all. Both keep a signature this tick's census cannot match, so
    // the plan is owed and the next turn pays it.
    let plan =
        match recalled with
        // The recalled memo goes on with its plan and its tables: the tables
        // are the ones this tick filled — the Atlas was handed them and
        // writes into them — and every entry in them is the census's, so
        // nothing here is a fact of the tick that has to be swapped out at
        // the boundary (ADR 0070). The room stamp is **this tick's** all the
        // same, because a recalled plan does not say the stamp was: on the
        // tick a census returns after a deferred turn (#372) the plan is
        // recalled while the stamp is the dark tick's, and a stamp handed on
        // as it was would evict and re-flood that room every tick until the
        // next replan — and would match the dark census, should vision drop
        // again, and serve the held grid's floods on the dark tick.
        | Some m -> { m with RoomSignatures = signedRooms }
        // A colony whose turn has not come (#357) serves its stale plan but
        // carries the tick's own tables on it: a deferred colony declines to
        // plan, not to price, and the tables it filled this tick are worth
        // exactly what any other tick's are.
        //
        // The tables are this tick's while the plan's signature is the stale
        // one's, and the tables say so: they are stamped with **this tick's**
        // per-room signatures, so a census that moves and moves back recalls
        // the plan and evicts the entries the intermediate tick laid (#372).
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

    // The tick's Threats, derived once off the view's hostiles and the
    // rampart census, and shared by every reader of them (ADR 0033).
    let threats = threatsOf view atlas

    // The colony's other placement step, beside the memoised Layout and
    // never inside it (ADR 0042): the outpost's source containers, derived
    // fresh every tick for the reason written on the rule itself.
    let outpostSiteIntents = planOutpostContainers view atlas

    let defenseIntents = planSafeMode view atlas @ planFire view atlas

    // The consignment's send (#349), beside the defence reflexes because it is
    // the same kind of thing: a structure's own verb, read off facts on the
    // view, owing nothing to the Matcher or to a body.
    let consignmentIntents = planConsignment view

    // The pool is derived before the spawns, and the dependency runs one way
    // only: the worker row's floor asks the pool whether anything is standing
    // in Build or Repair (ADR 0046), and nothing in the pool reads a spawn
    // Intent.
    // The outpost chain's answers, derived once for the whole tick (#383).
    // Before this, `claimTargets` ran 11.4 times a tick and `outpostControllers`
    // 9.1, each walking the kind census for `Controller`, because every reader
    // of the chain re-entered it whole. Derived here for `RowSizing`'s own
    // reason, stated below: the row hired against a number and the target that
    // counts it must read one set of numbers.
    let outposts = outpostFactsOf view

    let sizing = rowSizingOf view atlas outposts

    // The narrow facts the Planner reads about the colony's own decisions (ADR
    // 0061, ADR 0067): the task ids its living creeps hold, and which holders
    // still carry Thorium, off the table the Matcher wrote last tick. Derived
    // here, before the pool, because this is the one place that has both the
    // assignments and the fleet to filter them by.
    let held = heldTaskFacts view assignments

    let tasks = planTasks view atlas threats held outposts

    // The pool's other half (ADR 0052 decision 6): every entry's priority
    // and capacity, set once here and read by the Matcher and the mover.
    let pool = planPool view atlas tasks

    let spawnIntents, quotas =
        planSpawns view atlas outposts sizing threats tasks plan.HaulerQuota

    let next, verdicts = matchCreeps view atlas sizing threats pool assignments verbose
    let assigned = assignedTasks tasks next

    // The other half of the vision grace (#151): the holders the Matcher kept
    // against a Task that is in no pool, and the room each one's target was
    // last seen in. They reach the Emitter as nothing — there is no act to
    // spell on a target nobody can see — and the mover as a crossing, which is
    // the whole of what the grace buys. Read off the same rule the keep was
    // taken on (`lastSeenIn`), over the assignments that survived it: a keep
    // with no pooled Task is a graced one by construction.
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

    // The signature (#381), beside the pickup reflex it is shaped after: no
    // walk, no Task, no competition, and silent on every tick after the first
    // one that catches a body beside an unsigned controller.
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
/// through the one-room-at-a-time pass (`resolveRooms`), so the `Intents` and
/// `Verdicts` here are the tick's whole answer for this colony. The two are the
/// same call in every world one colony works alone.
let decide
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    // One colony alone is always its own turn to re-plan: the budget #357
    // added exists to stagger colonies against each other, and there is
    // nothing here to stagger against.
    let decision = decideUnarbitrated view assignments verbose memo ReplanTurn.Now
    let moveIntents, moveVerdicts = resolveRooms [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }
