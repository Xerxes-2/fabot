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
let censusSignature (view: ColonyView) : string =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial

    // One join for both halves: a target is read wherever the
    // projection places it, standing or pending alike, because both halves
    // move a grid the memo holds an answer off.
    let census select =
        spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            select kind
            |> Option.bind (fun (built: BuiltKind) ->
                SpatialInfo.placementOf spatial id
                |> Option.map (fun tile -> $"{built}@{tile.Room}:{tile.X},{tile.Y}")))
        |> List.sort
        |> String.concat ";"

    let standing =
        census (function
            | Structure kind -> Some kind
            | _ -> None)

    let pending =
        census (function
            | Site kind -> Some kind
            | _ -> None)

    // The Thorium deposits, each on its tile (ADR 0057 decision 1). Signed on
    // ADR 0044's rule that the memo signs the union of its readers: since this
    // ticket the Layout reads them twice — the extractor's tile *is* the
    // deposit's, and the mineral container is seated on its Seats — and
    // `Atlas.workingGroundIn` reads them at every level, so a deposit that
    // arrived or left moves the clustered ordering itself. Leaving is the live
    // case and not a hypothetical: the mod deletes an exhausted Thorium deposit
    // outright, and unsigned, the memo would keep handing back a plan naming a
    // container on the Seat of a rock that is gone.
    let minerals =
        spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            match kind with
            | Mineral ->
                SpatialInfo.placementOf spatial id
                |> Option.map (fun tile -> $"Mineral@{tile.Room}:{tile.X},{tile.Y}")
            | _ -> None)
        |> List.sort
        |> String.concat ";"

    // The tiles another player's construction sites hold (#248), named the way
    // ADR 0044's consequence names every census input — `{kind}@{room}:{x},{y}`
    // — with the one thing we know about such a site standing in the kind
    // slot, that it is not ours: a rival's site reaches the projection as a
    // tile and nothing else (`RoomLayer.RivalSites`), so there is no id to join
    // it through the census above and no built kind to name. Signed at all
    // because ADR 0044 makes the memo sign the union of its readers: the
    // Layout's tile clause reads these, so a rival building on the container's
    // pick between two ticks must throw the plan away. Unsigned, the memo would
    // hand back the very `PlaceConstructionSite` the engine is refusing.
    //
    // Signed for **every** projected room, which is deliberately wider than
    // that reader — `planLayout` plans the home room alone, and
    // `planOutpostContainers`, the only other caller of
    // `Atlas.collidingSiteTilesIn`, is off this memo by a rule of its own. Wide
    // on ADR 0044's own ground: over-invalidating is the cheap error (a
    // recompute) where a missed input is the expensive one (a stall until a
    // reset), the standing and pending halves widened per room the same way,
    // and a signature cut to today's one reader is a signature gap the tick
    // another rule joins the memo.
    let rivals =
        spatial.Rooms
        |> Map.toList
        |> List.collect (fun (room, layer) ->
            layer.RivalSites
            |> Set.toList
            |> List.map (fun tile -> $"Rival@{room}:{tile.X},{tile.Y}"))
        |> List.sort
        |> String.concat ";"

    let level =
        view.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    // The rate each projected room's sources are priced at this tick, in
    // room-name order, and the empty rate for a room vision answered for not at
    // all — the third answer the quota gives, and a different one from either
    // rate (ADR 0004).
    let held =
        spatial.Rooms
        |> Map.toList
        |> List.map (fun (room, _) ->
            let rate =
                Map.tryFind room view.RoomControl
                |> Option.map (heldRateOf >> string)
                |> Option.defaultValue ""

            $"{room}:{rate}")
        |> String.concat ","

    // Every colony of ours and the stage it stands at, in room-name order:
    // the map is already sorted by key, and a room leaving it moves the
    // string as surely as one changing stage does.
    let stages =
        view.Stages
        |> Map.toList
        |> List.map (fun (room, stage) -> $"{room}:{stage}")
        |> String.concat ","

    $"{home}|{level}|{held}|{stages}|{standing}|{pending}|{rivals}|{minerals}"

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
    : Decision =
    let signature = censusSignature view
    // The signature is read before the Atlas is built, because the Atlas
    // is one of the things it decides: a memo whose census still stands
    // hands over its spawn walk table, and a memo that has gone stale —
    // or none at all — leaves the Atlas a fresh one (ADR 0032).
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    let walks =
        match recalled with
        | Some m -> m.Walks
        | None -> WalkTable()

    let atlas = Atlas.ofViewRecalling walks view

    let plan =
        match recalled with
        | Some m -> m
        | None ->
            let siteIntents, servedFootings, unservedFootings, unroutedTrunks, deferredContainers =
                planLayout view atlas

            let quota, demandRows, load = haulerDemandOf view atlas

            {
                Signature = signature
                SiteIntents = siteIntents
                UnservedFootings = unservedFootings
                ServedFootings = servedFootings
                UnroutedTrunks = unroutedTrunks
                DeferredContainers = deferredContainers
                HaulerQuota = quota
                HaulerDemand = demandRows
                HaulerLoad = load
                Walks = walks
            }

    // The tick's Threats, derived once off the view's hostiles and the
    // rampart census, and shared by every reader of them (ADR 0033).
    let threats = threatsOf view atlas

    // The colony's other placement step, beside the memoised Layout and
    // never inside it (ADR 0042): the outpost's source containers, derived
    // fresh every tick for the reason written on the rule itself.
    let outpostSiteIntents = planOutpostContainers view atlas

    let defenseIntents = planSafeMode view atlas @ planFire view atlas

    // The pool is derived before the spawns, and the dependency runs one way
    // only: the worker row's floor asks the pool whether anything is standing
    // in Build or Repair (ADR 0046), and nothing in the pool reads a spawn
    // Intent.
    let sizing = rowSizingOf view atlas

    let tasks = planTasks view threats

    // The pool's other half (ADR 0052 decision 6): every entry's priority
    // and capacity, set once here and read by the Matcher and the mover.
    let pool = planPool view atlas tasks

    let spawnIntents, quotas =
        planSpawns view atlas sizing threats tasks plan.HaulerQuota

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

    let intents =
        defenseIntents
        @ spawnIntents
        @ plan.SiteIntents
        @ outpostSiteIntents
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
    let decision = decideUnarbitrated view assignments verbose memo
    let moveIntents, moveVerdicts = resolveRooms [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }
