/// The Emitter: which Tasks a given body may act on now, and the engine Intents
/// an assignment turns into. Actions only — the walk that gets a creep there
/// belongs to the Resolver.
[<AutoOpen>]
module Fabot.Core.Decide.Emitter

open Fabot.Core
open Fabot.Core.Types

/// Whether a creep can usefully work this Task right now: the body must be able
/// to do it, and its energy state must call for it. Gates read part arithmetic,
/// never names or roles. The body gates enforced here: ADR-0016 (a Work-heavy
/// body never Withdraws), ADR-0019 (only a Work part draws the upgrade buffer),
/// ADR-0024 (a full garrison keeps Harvest), ADR-0046 (a standing body makes no
/// delivery), ADR-0048 (a Work-heavy body's Upgrade is in place or nowhere) and
/// ADR-0057 (the miner's gate, one resource at a time, the delivery draw).
let internal applicable
    (view: ColonyView)
    (threats: Threats)
    atlas
    (creep: CreepInfo)
    (pooled: PooledTask)
    =
    let task = pooled.Task

    let has part = partCount creep.Body part > 0

    // Read once for the creep rather than at each clause: neither depends on
    // the Task, and a clause that asked twice was asking the Atlas twice.
    let standing = isStandingBody view.Tuning creep
    let heavy = Atlas.workHeavy atlas creep.Name

    // An intake is for a body with at least half its store free (#235): a body
    // past half full is a delivery. A standing body's one Carry is a trip's
    // worth, so for it this is "empty". For a light body's Harvest this prices
    // the walk alone; see the Harvest arm.
    let halfEmpty = creep.FreeCapacity * 2 >= creep.Energy + creep.FreeCapacity

    // Thorium aboard shuts every energy intake: a mixed load pours energy into a
    // reactor that refuses it. Zero for every body in a colony with no mine.
    let carryingThorium = creep.Thorium > 0

    // Read as the two holdings and not as `FreeCapacity` against the body's
    // carry: a hand-built body is free to state a store the engine would never
    // hand back.
    let emptyHanded = creep.Energy = 0 && creep.Thorium = 0

    // A delivery of Work. Refill carries rather than works, so it keeps its own
    // `has Carry`.
    let spending = has Work && creep.Energy > 0

    match task with
    // The miner's gate: a Work part and no Carry at all. A body with a Carry is
    // refused because a released Anchor is applicable to every Harvest and would
    // stand on the mine Post filling a store that ages it by
    // `floor(log10 store.T)` ticks a tick, and never empty it. Thorium never
    // regenerates, so the only thing rationing the dig is the extractor's
    // cooldown, which is `heldByCooldown`'s gate and not applicability's.
    | Harvest rockId when Atlas.isMineral atlas rockId -> has Work && not (has Carry)
    // A full garrison keeps digging where it stands; a heavy body still walking
    // is offered the walk only toward a Post of this source with no garrison on
    // it *now* (#258), because the Post cap is counted at arrival and a long
    // enough walk discounts any incumbent: without that clause a full Anchor
    // released off its rock read every garrisoned Post in the colony as
    // somewhere to go. The narrowing is the full body's alone, so a fresh
    // Anchor is still sent to the Post its expiring incumbent stands on.
    // `has Carry` refuses the store-less miner (#261): it reports
    // `FreeCapacity = 0`, is Work-heavy and has not arrived, so it would
    // otherwise be offered the walk and dribble its Work into a source
    // container for a whole life while the deposit is never dug.
    | Harvest sourceId ->
        has Work
        && has Carry
        && (creep.FreeCapacity > 0
            || garrisons atlas creep sourceId
            || (heavy
                && not (Set.isEmpty (Atlas.postsOf atlas sourceId))
                && not (mayActNow threats atlas creep.Name task)
                && hasUnmannedPost view atlas creep sourceId))
        // Three clauses a light body answers and a garrison does not (#235),
        // all about the walk digging costs a body that does not live at the
        // rock. Half empty *while the walk is still ahead of it*: Harvest is the
        // one intake that runs for dozens of ticks, and this is the release gate
        // as well as the dispatch one, so the store mirror alone evicted a body
        // off the Seat it was digging on the tick it crossed half full. Not a
        // standing body (closes #206 for the one Task it spared: an empty
        // buffer left the row nothing else applicable, and travel cost did not
        // hold it). And something spare in the rock. The last two carry no
        // arrival exemption on purpose: the release is the point of them.
        && (heavy
            || ((halfEmpty || mayActNow threats atlas creep.Name task)
                && not standing
                && hasSpareRate view atlas sourceId))
        // A body carrying ore does not dig energy into the same store. Never
        // true of a garrison; what it refuses is a light body that took a load
        // off the mineral container and would outrank its own delivery.
        && not carryingThorium
    // The body half of this gate is read a second time by `canRefill`, the
    // supply floor's arming condition: a clause narrowing what a body may draw
    // with belongs in front of both readers.
    | Withdraw(storeId, resource) ->
        let buffer = Set.contains storeId (Atlas.controllerContainers atlas)

        // A Withdraw must be worth this body's trip (#232): the store holds at
        // least half of what the body has room for. A fact about the pair, so it
        // is here and not in `capacityOf`. Read off free capacity, and judged
        // every tick, so it gates persistence as well as entry. Not carried to
        // Pickup: a pile decays and a container does not. Three stores it does
        // not price: a store that ends (the Pickup's exemption), the stock (what
        // the line buys is the fall to the tier below, and there is none below
        // the Storage's own Withdraw), and the buffer under a standing body's
        // feet. Priced down the resource's own column.
        //
        // The delivery draw is the Storage's Thorium in a colony that has
        // declared the errand (#373). A consignor's Storage drawn for its own
        // terminal (#349) is a Storage's Thorium too, a leg of a few tiles with
        // no crossing on it, and the errand is what tells the two apart — the
        // same test `intentFor` reads.
        let deliveryDraw =
            Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Storage)
            && not (List.isEmpty view.Errands)

        let stock = SpatialInfo.heldIn view.Spatial resource storeId

        let worthTheTrip =
            stock * 2 >= creep.FreeCapacity
            || (Map.tryFind storeId view.Spatial.TargetKinds |> Option.exists isTransient)
            // The tier and not the bare rank (#306): a rung never leaves its tier
            // (`tierRungs`/`priorityStep`), so the shallowest rank `StockDraw`
            // owns is half a tier above it. This disjunct is the energy column's
            // alone (#380): #367 moved the ore half of the Storage draw to
            // `Feeding`, where the comparison is false, and 376 T stood in the
            // Storage with the Reactor dry for 424 ticks.
            || pooled.Priority >= priorityOfTier StockDraw - tierRungs / 2
            // So the ore half says it in its own words: the delivery's draw is
            // always worth the trip, because a body refused it stands idle while
            // the Reactor burns down.
            || deliveryDraw
            || (buffer && standing)

        // The Thorium arm asks for an *empty* body and not a half-empty one, and
        // drops the two clauses about the controller's container.
        //
        // A load must be deliverable by the body that draws it (#354). Under the
        // 1,000-unit contact cliff the mod spends `floor(log10 store.T)` extra
        // life a tick on every creep whose tile carries ore, and the ore on that
        // tile is the body's own load, so the loaded leg costs
        // `Tuning.MineContactAgeing` ticks of life per tile. Dying loaded is
        // lost score: the tombstone's tile is hot too, and the pile bleeds 1 T a
        // tick. Priced from the store for the body as loaded (#373): the empty
        // candidate is not the body that walks the leg — a worker's `11W 12C 12M`
        // is at parity empty and two ticks a tile under `ReactorLoad`, and read
        // off the empty body one died in the Reactor's room with 500 T aboard —
        // and pricing from the store lets the Atlas price the leg once per body
        // shape (`Atlas.walkTicksFrom`). `Tuning.DeliveryLifeMargin` is the
        // slack a flee, a keeper detour or a swamp step is paid out of (#378):
        // without it the clause was an equality against a priced walk, and
        // `hauler-558190` drew with two ticks over a 196-tick leg.
        let requiredLife walk =
            walk * view.Tuning.MineContactAgeing * (100 + view.Tuning.DeliveryLifeMargin)
            / 100

        let outlivesTheLoadedLeg =
            match SpatialInfo.placementOf view.Spatial storeId with
            | None -> true
            | Some store ->
                let loaded = Grid.factorCarrying creep view.Tuning.ReactorLoad

                view.Errands
                |> List.forall (fun errand ->
                    match Atlas.walkTicksFrom atlas loaded store (snd errand.Target) with
                    | None -> true
                    | Some walk -> creep.TicksToLive >= requiredLife walk)

        // No Work part on the delivery draw (#373): dead weight on a leg that is
        // all carrying, and what puts the body above parity at exactly the load.
        // "One fixed courier body" was a row fact and never a gate, so once #367
        // ranked the draw at the top of Feeding the empty workers beside the
        // Storage took it. Read behind `deliveryDraw` alone: the mine haul and
        // the tombstone draw are a few tiles onto the same floor.
        let carriesOnly = not (has Work)

        match resource with
        | Thorium ->
            has Carry
            && emptyHanded
            && worthTheTrip
            && not heavy
            && not standing
            && (not deliveryDraw || (carriesOnly && outlivesTheLoadedLeg))
        | Energy ->
            has Carry
            && halfEmpty
            && not carryingThorium
            && worthTheTrip
            && not heavy
            && (has Work || not buffer)
            // A standing body fetches from the buffer at its feet and from
            // nowhere else (#206).
            && (buffer || not standing)
    // The Withdraw gate without its target-shaped clauses: a pile is nobody's
    // buffer, and it drops `worthTheTrip` because a pile decays and a store does
    // not, so there is no later body to leave it for (#311).
    | Pickup(_, Thorium) -> has Carry && emptyHanded && not heavy && not standing
    | Pickup(_, Energy) ->
        has Carry && halfEmpty && not carryingThorium && not heavy && not standing
    // The two body clauses are read a second time by `canRefill`, beside
    // Withdraw's; the Energy clause is a state, not a fact about the body. The
    // delivery half reads down the same two columns: what a body took is what
    // it has to put down.
    | Refill(_, Energy) -> has Carry && creep.Energy > 0 && not standing
    | Refill(targetId, Thorium) ->
        let reactor =
            view.Errands |> List.exists (fun errand -> fst errand.Target = targetId)

        let storage =
            Map.tryFind targetId view.Spatial.TargetKinds = Some(Structure BuiltKind.Storage)

        has Carry
        && carryingThorium
        && not standing
        // A mine carrier above the decade-safe load must bank first; only the
        // Storage draw is capped to 999 (#319).
        && (not reactor || creep.Thorium <= view.Tuning.ReactorLoad)
        // The exact delivery load is its body/task marker, not the courier's
        // name: once drawn it waits for the Reactor rather than pouring straight
        // back into Storage. A remainder is not that number and is not refused
        // here (#378): widening this to "any ore, once the mine is out" was
        // tried and taken out — `oreStillComing` is false for every colony with
        // no diggable deposit, so it refused the Storage to the last mine haul,
        // to a crossed room's pile and to a consignment walked in from the
        // terminal (#349). The tier gap sends a remainder to the Reactor while
        // the body can act on it and banks it when it cannot.
        //
        // Except when no Reactor will take it: contested (#406), held or shut
        // (#407). A rival's flag is ours again the tick the re-claimer acts;
        // none of those three is, and a load held through one ages its body
        // to death.
        && (not storage
            || creep.Thorium <> view.Tuning.ReactorLoad
            || Set.isEmpty (Facts.errandRoomsDeliverable view))
    // A Build is a walk a colony-wide rank cannot thin (#157, #234): light
    // bodies' work, and neither a heavy body's nor a standing one's. The one
    // exception is a container site under the body's own feet, on its own Post
    // (#205): both prohibitions are about a walk, and neither reaches a site
    // the body is standing on.
    | Build siteId ->
        spending
        && (Atlas.standsOnPostSite atlas creep.Name siteId || (not standing && not heavy))
    | Repair _ -> spending && not standing
    // The one Task a standing body exists for, so no standing gate; a heavy
    // body spends its Work into the controller only from where it already
    // stands.
    | Upgrade _ ->
        spending
        && (not heavy || mayActNow threats atlas creep.Name task)
        // The borrowed Upgrade is a commute across the Seam (#213): the lift
        // that sends the pioneers must not send the home upgraders after them.
        && not (pooled.Borrowed && standing)
    | Reserve _ -> has BodyPart.Claim
    | Claim _ -> has BodyPart.Claim
    // The engine's `claimReactor` checks a live CLAIM part and nothing else.
    // Ownership is read at `intentFor`, not here: read here, a body whose flag
    // was safe would be released the tick it took it and walk three rooms home.
    | Reclaim _ -> has BodyPart.Claim
    // Spelled through the row predicate the body-class ladder reads, so the
    // gate and `bodyClassOf` cannot disagree. No room clause: the walk to the
    // work area is what travel cost prices.
    | Guard _ -> isGuardBody creep
    // Two bodies are exempt, for opposite reasons: a Work-heavy body cannot run
    // (the answer for its Post is a rampart), and a Fighter will not. Without
    // the second a guard on the ring is offered both Safety-tier Tasks and kept
    // in the fight by travel cost alone, so the tick a raid steps toward it the
    // body bought to stand still walks away.
    | Flee -> not (isGuardBody creep) && not heavy && standsInReach threats atlas creep.Name

/// The action Intent a Task asks of a creep, and `None` where this tick asks
/// for none: Flee is movement and nothing else, and Reclaim withholds its act
/// on the ticks the reactor is already ours — the one act gated on a fact about
/// its target rather than the body, which is why the view is a parameter.
let private intentFor (view: ColonyView) atlas (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> Some(HarvestSource(creep.Name, sourceId))
    // The same Intent for a tombstone or a ruin as for a container: the engine's
    // `withdraw` is one method over every store. `None` for the amount means
    // "as much as the body has room for".
    | Withdraw(storeId, resource) ->
        let amount =
            // The one place a number is named, and since #378 it is the tick's
            // own load rather than the constant: a whole `Tuning.ReactorLoad`
            // while the mine still feeds the bank, the remainder when it does
            // not. Zero means there is no delivery draw, so the Withdraw is the
            // ordinary one.
            if
                resource = Thorium
                && Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Storage)
                && view.Errands |> List.isEmpty |> not
            then
                match Facts.deliveryLoad view atlas with
                | 0 -> None
                | load -> Some load
            else
                None

        Some(WithdrawFromStore(creep.Name, storeId, resource, amount))
    // The reflex's own Intent, issued for a creep that walked: an arriving
    // picker spells it twice and `decide` keeps one. One act for both
    // resources: the engine's `pickup` takes no resource argument.
    | Pickup(pileId, _) -> Some(PickupPile(creep.Name, pileId))
    // A refill cluster's Refill names a place; which member the energy lands in
    // is settled here, at arrival, off the tile the body stands on. Every other
    // Refill resolves through the same call.
    | Refill(structureId, resource) ->
        Atlas.refillTarget atlas creep.Name structureId resource
        |> Option.map (fun target -> TransferEnergyToStructure(creep.Name, target, resource))
    | Build siteId -> Some(BuildSite(creep.Name, siteId))
    | Repair structureId -> Some(RepairStructure(creep.Name, structureId))
    | Upgrade controllerId -> Some(UpgradeController(creep.Name, controllerId))
    | Reserve controllerId -> Some(ReserveController(creep.Name, controllerId))
    | Claim controllerId -> Some(ClaimController(creep.Name, controllerId))
    // Issued on a tick the reactor is not ours and on no other. The engine
    // would take the act either way — `claimReactor` has no ownership
    // precondition and no cooldown — so what this buys is legibility: an act in
    // the Executor's log is a flag that had been taken from us. Absence is not
    // ours: the body standing here is the colony's only vision of the room, and
    // a tick with no answer is a tick to act.
    | Reclaim reactorId ->
        if SpatialInfo.ownsTarget view.Spatial reactorId then
            None
        else
            Some(ClaimReactor(creep.Name, reactorId))
    | Flee -> None
    // The Guard's attack names a hostile chosen at arrival, rather than a
    // placed Task target (`guardIntent`). Healing is the shared reflex's act.
    | Guard _ -> None

/// Chat-bubble glyph of a Task: the whole colony's current matching is
/// legible in the viewer at one glyph per creep.
let private glyphFor =
    function
    | Harvest _ -> "⛏"
    | Withdraw _ -> "📥"
    | Pickup _ -> "🧲"
    | Refill _ -> "🔋"
    | Build _ -> "🔨"
    | Repair _ -> "🔧"
    | Upgrade _ -> "⚡"
    | Reserve _ -> "🚩"
    | Claim _ -> "🏴"
    // Said on every tick the Task is held, not only on the ticks the act
    // fires: standing there holding the Task is the whole of what this body is
    // for. (The miner's is withheld on a cooldown tick; see `emit`.)
    | Reclaim _ -> "☢️"
    | Flee -> "🏃"
    | Guard _ -> "⚔️"

/// The Threat a guard swings at, out of the ones standing in the room its Task
/// names and passing the caller's own gate: the one nearest a Post of that
/// room, ties by id; with no Post standing, the nearest to the guard. None
/// where the room holds none, or where the projection places the guard nowhere.
///
/// The gate is the caller's and stands *ahead* of the choice, which narrows
/// ADR-0056 decision 2 without overturning it: ordered the other way round, a
/// guard standing on the ring of the second invader of a two-creep raid is
/// handed the one nearest the Post, finds it three tiles off, and swings at
/// nothing while the invader beside it deals 40 a tick. Where the nearest-Post
/// Threat is in reach the two readings answer alike.
let private guardTarget
    (view: ColonyView)
    atlas
    (creep: CreepInfo)
    (room: string)
    (among: HostileInfo -> bool)
    =
    let posts = Atlas.postsIn atlas room

    // The guard's own tile, which is only read where the room has no Post; an
    // unplaced body prices every Threat alike and the id order answers.
    let here =
        Atlas.creepTile atlas creep.Name |> Option.filter (fun t -> t.Room = room)

    let distance (hostile: HostileInfo) =
        let from = RoomPos.pos hostile.Pos

        if Set.isEmpty posts then
            here
            |> Option.map (fun tile -> range from (RoomPos.pos tile))
            |> Option.defaultValue 0
        else
            posts |> Set.toList |> List.map (range from) |> List.min

    view.Hostiles
    |> List.filter (fun h -> h.Pos.Room = room && (weaponRange h |> Option.isSome) && among h)
    |> List.sortBy (fun h -> distance h, h.Id)
    |> List.tryHead

/// A Guard chooses one reachable melee target. Self-healing belongs to the
/// colony-wide reflex, which reads damage and the same compatibility rules as
/// execution. Movement remains the mover's alone.
let private guardIntent (view: ColonyView) atlas (creep: CreepInfo) (room: string) : Intent option =
    let inSwing (hostile: HostileInfo) =
        Atlas.creepTile atlas creep.Name
        |> Option.bind (fun tile -> RoomPos.range tile hostile.Pos)
        |> Option.exists (fun r -> r <= Engine.meleeRange)

    guardTarget view atlas creep room inSwing
    |> Option.map (fun hostile -> AttackCreep(creep.Name, hostile.Id))

/// The heal reflex beside any Task or none (#409): a body with an active HEAL
/// part heals itself if it is hurt, else the most-hurt creep of ours beside it,
/// else the most-hurt within `Engine.rangedRange` at the ranged rate. Existing
/// actions own their channels: a reflex never suppresses a swing, a harvest,
/// construction or another chosen action. Each heal is counted off its
/// patient's missing hits as it is planned, so two healers do not both pour
/// into a wound one of them closes.
let healReflex (view: ColonyView) (plan: Fabot.Core.IntentPlan.Plan) =
    let healParts (creep: CreepInfo) =
        Map.tryFind Heal creep.Body |> Option.defaultValue 0

    let tileOf (creep: CreepInfo) =
        SpatialInfo.creepPlacementOf view.Spatial creep.Name

    let missing =
        view.Creeps
        |> List.map (fun creep -> creep.Name, creep.Hits.HitsMax - creep.Hits.Hits)
        |> Map.ofList

    let patientFor (healer: CreepInfo) (owed: Map<string, int>) =
        if Map.find healer.Name owed > 0 then
            Some(healer, 0)
        else
            tileOf healer
            |> Option.bind (fun at ->
                view.Creeps
                |> List.choose (fun other ->
                    if other.Name = healer.Name || Map.find other.Name owed <= 0 then
                        None
                    else
                        tileOf other
                        |> Option.bind (RoomPos.range at)
                        |> Option.filter (fun r -> r <= Engine.rangedRange)
                        |> Option.map (fun r -> other, r))
                // Adjacent first, at three times the rate; then the deepest
                // wound; then the name, so the choice is the same every tick.
                |> List.sortBy (fun (other, r) ->
                    (r > Engine.meleeRange), -(Map.find other.Name owed), other.Name)
                |> List.tryHead)

    ((plan, missing), view.Creeps)
    ||> List.fold (fun (plan, owed) healer ->
        let parts = healParts healer

        if parts = 0 then
            plan, owed
        else
            match patientFor healer owed with
            | None -> plan, owed
            | Some(patient, r) ->
                let intent, power =
                    if r <= Engine.meleeRange then
                        HealCreep(healer.Name, patient.Name), Engine.healPower
                    else
                        RangedHealCreep(healer.Name, patient.Name), Engine.rangedHealPower

                match Fabot.Core.IntentPlan.tryAdd intent plan with
                | Ok healed ->
                    healed, Map.add patient.Name (Map.find patient.Name owed - parts * power) owed
                | Error _ -> plan, owed)
    |> fst

/// Whether a Thorium harvest is held this tick by the extractor's clock.
/// `EXTRACTOR_COOLDOWN` is 5 and the engine runs the intent pass before the
/// object pass — `extractors/tick.js` writes the 5 at the end of the harvest
/// tick and decrements it once per tick after — so successive harvests land
/// **six** ticks apart and the other five are refused outright.
///
/// The gate is here and never in applicability: a Task that vanished and
/// returned every sixth tick would churn the pool for a body that has nowhere
/// else to be. The Task exists exactly while the deposit does; the cooldown
/// decides whether this tick's act is issued.
///
/// A deposit with no extractor standing on it is held on the same footing:
/// `harvest.js` refuses a mineral with no extractor on its tile, and issuing it
/// would be one `ERR_NOT_FOUND` a tick for as long as the site takes to build.
/// Every other Task answers false.
let private heldByCooldown atlas task =
    match task with
    | Harvest rockId when Atlas.isMineral atlas rockId ->
        match Atlas.extractorOn atlas rockId with
        | Some extractor -> Atlas.cooldownOf atlas extractor > 0
        | None -> true
    | _ -> false

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position, and — for Harvest alone —
/// only while the source holds energy. Anticipatory dispatch and the occupancy
/// surcharge both price a walk high enough to land a creep a tick or two early,
/// so the gate is what keeps the engine's ERR_NOT_ENOUGH_RESOURCES spam
/// structurally impossible. The Guard is judged outside that gate: its acts
/// reach a creep the projection places nothing for, so `Atlas.mayAct` answers
/// false for it on every tick, and the range it is really gated on is the
/// swing `guardIntent` measures itself.
let private actionIntents
    (view: ColonyView)
    atlas
    (threats: Threats)
    (creep: CreepInfo)
    (task: Task)
    : Intent list =
    let drained = restockWait view task > 0

    match task with
    | Guard room -> guardIntent view atlas creep room |> Option.toList
    | _ ->
        if
            mayActNow threats atlas creep.Name task
            && not drained
            && not (heldByCooldown atlas task)
        then
            intentFor view atlas creep task |> Option.toList
        else
            []

/// Emitter: each assigned creep's action Intent, then every assigned
/// creep's chat bubble, both in view creep order. Judges actions from
/// tick-start geometry — it must run against the same Atlas the Matcher
/// used, never against resolved positions.
let emit (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) : Intent list =
    let actions =
        view.Creeps
        |> List.collect (fun creep ->
            match Map.tryFind creep.Name assigned with
            | Some task -> actionIntents view atlas threats creep task
            | None -> [])

    // Every assigned creep says its Task's glyph every tick; unassigned creeps
    // say nothing. One exception: a miner says ⛏ on the ticks it digs and
    // nothing on the ticks it waits, so the one-in-six rhythm is legible in the
    // viewer. Read off the same gate the act is withheld by, so the two cannot
    // disagree about the cooldown; the bubble goes on showing the Task through
    // every other reason an act is withheld.
    let says =
        view.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name assigned
            |> Option.filter (heldByCooldown atlas >> not)
            |> Option.map (fun task -> SayCreep(creep.Name, glyphFor task)))

    actions @ says
