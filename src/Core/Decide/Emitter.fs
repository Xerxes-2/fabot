/// The Emitter: which Tasks a given body may act on now, and the engine Intents
/// an assignment turns into. Actions only — the walk that gets a creep there
/// belongs to the Resolver.
[<AutoOpen>]
module Fabot.Core.Decide.Emitter

open Fabot.Core
open Fabot.Core.Types

/// The bodies that run from a Threat: the mover's gate, which walks them round
/// a Reach to their work (#472), and Flee's beside a fighter's (#533). Not a
/// Fighter, whose work is the fight, nor a Work-heavy body standing where it
/// digs (the answer for its Post is a rampart); one walking anywhere runs like
/// any other body (#515).
let internal runsFromThreats (view: ColonyView) atlas (creep: CreepInfo) =
    not (isFighterBody creep)
    && not (
        Atlas.workHeavy atlas creep.Name
        && Atlas.standsWhereItDigs atlas (view.Sources |> List.map (fun s -> s.Id)) creep.Name
    )

/// Whether the hostiles a body stands in the Reach of out-damage its own heal
/// (#533): each armed one within its `reachRadius` of the tile, at its whole
/// damage. Across a border a hostile counts where both stand that near an
/// edge, a reading that errs toward running. A body with no HEAL is
/// out-damaged by any, so the question costs it nothing. The heal of ours
/// reaching it is left out: a healer's heal is its own fight's.
let private outDamaged (view: ColonyView) atlas (creep: CreepInfo) =
    let heal = Engine.healPower * partCount creep.Body Heal

    heal = 0
    || match Atlas.creepTile atlas creep.Name with
       | None -> true
       | Some tile ->
           let at = RoomPos.pos tile
           let neighbours = RoomName.adjacent tile.Room

           let fromEdge (p: Pos) =
               min (min p.X (Seam.exitEdge - p.X)) (min p.Y (Seam.exitEdge - p.Y))

           let reaches (hostile: HostileInfo) r =
               if hostile.Pos.Room = tile.Room then
                   range at (RoomPos.pos hostile.Pos) <= r
               else
                   List.contains hostile.Pos.Room neighbours
                   && fromEdge at <= r
                   && fromEdge (RoomPos.pos hostile.Pos) <= r

           let damage =
               view.Hostiles
               |> List.sumBy (fun hostile ->
                   match reachRadius view hostile with
                   | Some r when reaches hostile r ->
                       let parts = partCountIn (HostileInfo.effectiveParts view.Hostiles hostile)

                       Engine.attackPower * parts Attack
                       + Engine.rangedAttackPower * parts RangedAttack
                   | _ -> 0)

           damage > heal

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
    // past half full is a delivery, but for a top-up (`tops`). A standing
    // body's one Carry is a trip's worth, so for it this is "empty". For a
    // light body's Harvest this prices the walk alone; see the Harvest arm.
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

    // The top-up (#530): a body past half full still draws a store whose whole
    // holding fits in the room it has left, within `Tuning.TopUpReach` ticks of
    // its walk, before the delivery that can wait: the store is emptied, and
    // one that decays is gone with nothing left to rot. Range first, so only a
    // near store pays for a walk.
    let tops storeId stock =
        stock > 0
        && stock <= creep.FreeCapacity
        && (match Atlas.creepTile atlas creep.Name, Atlas.positionOf atlas storeId with
            | Some at, Some store ->
                RoomPos.range at store
                |> Option.exists (fun r -> r <= view.Tuning.TopUpReach + 1)
                && Atlas.walkTicks atlas creep.Name task
                   |> Option.exists (fun walk -> walk <= view.Tuning.TopUpReach)
            | _ -> false)

    match task with
    // The miner's gate: a Work part and no Carry at all. A body with a Carry is
    // refused because a released Anchor is applicable to every Harvest and would
    // stand on the mine Post filling a store that ages it by
    // `floor(log10 store.T)` ticks a tick, and never empty it. Thorium never
    // regenerates, so the only thing rationing the dig is the extractor's
    // cooldown, which is `heldByCooldown`'s gate and not applicability's.
    // Work-heavy as well (#423): the dismantler is a Work body with no Carry
    // too, and would otherwise take the mine Post the tick its salvage ends.
    | Harvest rockId when Atlas.isMineral atlas rockId -> has Work && not (has Carry) && heavy
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
    // A raised child's rock (`ColonyView.BorrowedSources`) is for the pioneer
    // already standing in the child, never a walk out from the mother's home.
    | Harvest sourceId when
        view.BorrowedSources |> List.exists (fun s -> s.Id = sourceId)
        && Atlas.creepRoom atlas creep.Name <> Atlas.targetRoom atlas sourceId
        ->
        false
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
            // And with no stock to fall to (#236), a store at the pile's
            // threshold: what the line buys is the fall, and a colony with no
            // stocked Storage has nowhere to fall.
            || (stock >= view.Tuning.PickupThreshold
                && Atlas.idsOfKind atlas (Structure BuiltKind.Storage)
                   |> List.forall (fun id -> SpatialInfo.storedIn view.Spatial id = 0))

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
            && (halfEmpty || tops storeId stock)
            && not carryingThorium
            && worthTheTrip
            && not heavy
            && (has Work || not buffer)
            // A standing body fetches from the buffer at its feet and from
            // nowhere else (#206), and only empty: carrying any, it holds its
            // Upgrade and draws beside it (`topUp`, #498).
            && (not standing || (buffer && creep.Energy = 0))
    // The Withdraw gate without its target-shaped clauses: a pile is nobody's
    // buffer, and it drops `worthTheTrip` because a pile decays and a store does
    // not, so there is no later body to leave it for (#311).
    | Pickup(_, Thorium) -> has Carry && emptyHanded && not heavy && not standing
    // A child's floor is the ferry's drop (#473): a Work body's to build and
    // upgrade with, and never a hauler's to carry back out.
    | Pickup(pileId, Energy) ->
        has Carry
        && (halfEmpty || tops pileId (SpatialInfo.heldIn view.Spatial Energy pileId))
        && not carryingThorium
        && not heavy
        && not standing
        && (has Work
            || not (
                Atlas.targetRoom atlas pileId
                |> Option.exists (fun room -> isNurseryRoom view room || isBootstrapRoom view room)
            ))
    // The two body clauses are read a second time by `canRefill`, beside
    // Withdraw's; the Energy clause is a state, not a fact about the body. The
    // delivery half reads down the same two columns: what a body took is what
    // it has to put down.
    // The nursery drop is a hauler's (#473): a pioneer carrying energy to
    // the site builds with it.
    //
    // A load never goes back into the store it was drawn from (#530).
    | Refill(targetId, Energy) when
        creep.DrawnFrom |> Option.exists (fun drawn -> drawn.Store = targetId)
        ->
        false
    | Refill(targetId, Energy) when isFerryDrop view targetId ->
        has Carry && creep.Energy > 0 && not standing && not (has Work)
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
    // The row's own cast and no other, read off its name (#426): a generalist
    // walked two crossings out for it is a generalist lost to the colony for
    // the walk, a sapper an assault lost, and a miner stripped to Work = Move
    // by damage has the dismantler's parts and is not one.
    | Dismantle _ -> isNamedFor dismantlerPattern creep.Name
    // Spelled through the row predicates the body-class ladder reads, so the
    // gate and `bodyClassOf` cannot disagree; which row is the Brief's.
    // A Fight admits a squad cast, or once the squad launches a resident of
    // the room whose parts fit a slot; an Assault a cast of a role its squad
    // casts (`Formation.Roles`), how many of each role being the capacity's.
    // A Defend admits its row's bodies and no other (#504, #546).
    | Guard _
    | Fight _
    | Defend _
    | Assault _ ->
        Threats.briefFor threats task
        |> Option.exists (fun brief -> brief.Admits view atlas creep)
    // Any body standing where the Reach out-damages it runs, a fighter
    // included (#533): one holding no fight Task fights nothing there, and a
    // defender with no line to hold is only a target (#504). Which fighter
    // holds a fight instead is the Matcher's: a fight Task it holds or is
    // offered outranks Flee.
    | Flee ->
        standsInReach threats atlas creep.Name
        && (isFighterBody creep || runsFromThreats view atlas creep)
        && outDamaged view atlas creep

/// Whether a controller stands in a room nobody owns: the claim party's
/// (#471), the one Upgrade pooled on such a controller. Read off the room's
/// control entry, never a second `claimTargets` scan (#383).
let private unownedController (view: ColonyView) id =
    SpatialInfo.roomOf view.Spatial id
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.exists (fun control -> control.Owner = Ownership.Unowned)

/// Whether the Invader reserves a controller, off the same control entry.
let private invaderHeld (view: ColonyView) id =
    SpatialInfo.roomOf view.Spatial id
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.bind (RoomControlInfo.heldBy ReservationHolder.Invader)
    |> Option.isSome

/// The claim party's walk (#471): for each candidate's controller, the
/// ticks each loaded holder of its Upgrade still has to walk to reach it.
let private partyWalksOf (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) =
    view.Creeps
    |> List.choose (fun creep ->
        match Map.tryFind creep.Name assigned with
        | Some(Upgrade id as task) when
            creep.Energy > 0
            && unownedController view id
            && not (mayActNow threats atlas creep.Name task)
            ->
            Atlas.walkTicks atlas creep.Name task |> Option.map (fun walk -> id, walk)
        | _ -> None)
    |> List.groupBy fst
    |> List.map (fun (id, walks) -> id, walks |> List.map snd)
    |> Map.ofList

/// Whether a claimer holds its claim for its party: a member still walking
/// arrives inside the claimer's life less `Tuning.ClaimPartyMargin`. A member
/// who cannot, or one that died, holds nothing, so the wait is capped by
/// the claimer's own life and never restarts past it.
let private waitsForParty
    (view: ColonyView)
    (walks: Map<string, int list>)
    (claimer: CreepInfo)
    id
    =
    let inHand = claimer.TicksToLive - view.Tuning.ClaimPartyMargin

    Map.tryFind id walks |> Option.exists (List.exists (fun walk -> walk < inHand))

/// The container under a loaded harvester's feet, when it has decayed below
/// half: an outpost's container loses 5,000 hits a hundred ticks and no worker
/// may come for thousands (W17S26's decayed away on 2026-10-04), while the
/// body standing on it repairs 100 a Work part for one energy each.
let private wornContainerUnder (view: ColonyView) atlas (creep: CreepInfo) =
    if creep.Energy <= 0 then
        None
    else
        Atlas.creepTile atlas creep.Name
        |> Option.bind (fun tile ->
            let placed = (SpatialInfo.layerOf view.Spatial tile.Room).TargetPositions

            SpatialInfo.structureHits view.Spatial
            |> List.tryPick (fun (id, kind, hits) ->
                if
                    kind = BuiltKind.Container
                    && 2 * hits.Hits < hits.HitsMax
                    && Map.tryFind id placed = Some(RoomPos.pos tile)
                then
                    Some id
                else
                    None))

/// The action Intent a Task asks of a creep, and `None` where this tick asks
/// for none: Flee is movement and nothing else, and Reclaim withholds its act
/// on the ticks the reactor is already ours — the one act gated on a fact about
/// its target rather than the body, which is why the view is a parameter.
let private intentFor
    (view: ColonyView)
    atlas
    (partyWalks: Map<string, int list>)
    (creep: CreepInfo)
    task
    =
    match task with
    // The claim party's Upgrade before the claim lands: a wait at the
    // controller, the engine refusing the act on a controller nobody owns.
    | Upgrade controllerId when unownedController view controllerId -> None
    // The claim waits for its party (#471): a tap lands before a party cast
    // after the claim can walk in.
    | Claim controllerId when waitsForParty view partyWalks creep controllerId -> None
    | Harvest sourceId ->
        match wornContainerUnder view atlas creep with
        | Some containerId -> Some(RepairStructure(creep.Name, containerId))
        | None -> Some(HarvestSource(creep.Name, sourceId))
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
            // A terminal's energy above the consignment's fee reserve and no
            // more: the aid's arrivals, never the fee its Refill would put back.
            elif
                resource = Energy
                && Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Terminal)
            then
                let reserve =
                    if Option.isSome view.Consignee then
                        view.Tuning.TerminalEnergy
                    else
                        0

                Some(
                    max
                        0
                        (min
                            creep.FreeCapacity
                            (SpatialInfo.storedIn view.Spatial storeId - reserve))
                )
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
    | Refill(siteId, Energy) when isFerryDrop view siteId -> Some(DropEnergy creep.Name)
    | Refill(structureId, resource) ->
        Atlas.refillTarget atlas creep.Name structureId resource
        |> Option.map (fun target -> TransferEnergyToStructure(creep.Name, target, resource))
    | Build siteId -> Some(BuildSite(creep.Name, siteId))
    | Repair structureId -> Some(RepairStructure(creep.Name, structureId))
    | Upgrade controllerId -> Some(UpgradeController(creep.Name, controllerId))
    // The Invader's hold is attacked off before it is reserved over (#487):
    // `reserveController` is refused on it.
    | Reserve controllerId when invaderHeld view controllerId ->
        Some(AttackController(creep.Name, controllerId))
    | Reserve controllerId -> Some(ReserveController(creep.Name, controllerId))
    | Claim controllerId -> Some(ClaimController(creep.Name, controllerId))
    // Issued on a tick the reactor is not ours and on no other. The engine
    // would take the act either way — `claimReactor` has no ownership
    // precondition and no cooldown — so what this buys is legibility: an act in
    // the Executor's log is a flag that had been taken from us. Absence is not
    // ours: the body standing here is the colony's only vision of the room, and
    // a tick with no answer is a tick to act. An ally's burn pools no Reclaim
    // at all (#413, #415).
    | Reclaim reactorId ->
        if SpatialInfo.ownsTarget view.Spatial reactorId then
            None
        else
            Some(ClaimReactor(creep.Name, reactorId))
    | Dismantle structureId -> Some(DismantleStructure(creep.Name, structureId))
    | Flee -> None
    // The Guard's attack names a hostile chosen at arrival, rather than a
    // placed Task target (`guardIntent`). Healing is the shared reflex's act.
    | Guard _
    | Fight _
    | Defend _
    | Assault _ -> None

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
    | Dismantle _ -> "🪓"
    | Flee -> "🏃"
    | Guard _ -> "⚔️"
    | Fight _ -> "🛡️"
    | Defend _ -> "🧱"
    | Assault _ -> "🏰"

/// A strike a body issues (#537): a shot or a swing at one target, with
/// whether it reaches it, or a mass attack at everything within three.
type Strike<'target> =
    | At of 'target * reaches: bool
    | Mass

/// The tiles of a room a mass attack may not reach (#537): each structure or
/// power creep of `RoomLayer.Foreign` whose owner is not ours to strike
/// anywhere (`Colony.strikesAnywhere`, the diplomacy rule's: an ally's, a
/// neutral's, a power bank), where a declared enemy's or an Invader's is a
/// target too. None for a room whose objects this view does not carry — a
/// transit room's, or one it does not project —, where no mass attack is
/// fired.
let shields (view: ColonyView) (room: string) : Set<Pos> option =
    if not (Map.containsKey room view.Spatial.Rooms) || Set.contains room view.Crossed then
        None
    else
        (SpatialInfo.layerOf view.Spatial room).Foreign
        |> Map.fold
            (fun shields tile owner ->
                if Colony.strikesAnywhere owner then
                    shields
                else
                    Set.add tile shields)
            Set.empty
        |> Some

/// What a mass attack from `from` deals (#537), off the body's live guns, the
/// shields of its room (`shields`), and the creeps within three of it as
/// (tile, hits left, whether we may strike it): each takes its rate a gun
/// (`Engine.massAttackPowerAt`), capped by the hits it has left. None where
/// it may not fire: no gun, a shield within three, or a body within three we
/// may not strike — the engine hits every creep and structure not ours, an
/// ally's and a neutral's alike.
let massDamage
    (guns: int)
    (from: RoomPos)
    (shields: Set<Pos>)
    (near: (RoomPos * int * bool) list)
    : int option =
    let at = RoomPos.pos from

    if
        guns = 0
        || shields |> Set.exists (fun tile -> range at tile <= Engine.rangedRange)
    then
        None
    else
        let reached =
            near
            |> List.choose (fun (tile, hits, may) ->
                RoomPos.range from tile
                |> Option.filter (fun r -> r <= Engine.rangedRange)
                |> Option.map (fun r -> r, hits, may))

        if List.exists (fun (_, _, may) -> not may) reached then
            None
        else
            Some(
                reached
                |> List.sumBy (fun (r, hits, _) -> min hits (guns * Engine.massAttackPowerAt r))
            )

/// The one strike choice (#535, #537), every tick's: the first target in the
/// kill order a body reaches, else the order's head, with whether it reaches
/// it — or the mass attack (`massDamage`, None for a melee body), where it
/// deals more than the best single strike at any target it reaches would
/// (`dealt`, each capped by the target's hits). Ties keep the single strike.
/// The full tick strikes only in reach (`strikeOf`); the light tick picks
/// again off its glance (`LightTick.intents`). In reach first narrows
/// ADR-0056 decision 2 without overturning it: ordered the other way round, a
/// guard standing on the ring of the second invader of a two-creep raid is
/// handed the one nearest the Post, finds it three tiles off, and swings at
/// nothing while the invader beside it deals 40 a tick.
let pickStrike
    (reaches: 'target -> bool)
    (dealt: 'target -> int)
    (mass: int option)
    (order: 'target list)
    : Strike<'target> option =
    let single () =
        match List.tryFind reaches order with
        | Some target -> Some(At(target, true))
        | None -> List.tryHead order |> Option.map (fun head -> At(head, false))

    match mass with
    | Some deals when
        deals > (order
                 |> List.fold (fun best t -> if reaches t then max best (dealt t) else best) 0)
        ->
        Some Mass
    | _ -> single ()

/// A melee fighter's step onto the tile of the target it reaches (#535),
/// beside its swing: attacks resolve before moves, and a move onto a tile
/// still held fails harmlessly, so a body steps after a target that leaves and
/// loses nothing beside one that stays. None off a tile beside it, from our
/// rampart (`onRampart`: off it the body takes every strike), onto an exit
/// tile (the engine carries a body ending a tick there across the border),
/// or onto a tile another melee raider (`melee`, their tiles) swings at.
let glueStep
    (from: RoomPos)
    (target: RoomPos)
    (onRampart: bool)
    (melee: RoomPos list)
    : Direction option =
    let struck =
        melee
        |> List.exists (fun other ->
            other <> target
            && RoomPos.range other target |> Option.exists (fun r -> r <= Engine.meleeRange))

    match RoomPos.range from target with
    | Some r when
        r = Engine.meleeRange
        && not onRampart
        && not (Seam.onRing (RoomPos.pos target))
        && not struck
        ->
        directionTo (RoomPos.pos from) (RoomPos.pos target)
    | _ -> None

/// The Threats a fighter may strike in a room, in its Selector's kill order
/// (`Facts.killRank`, #451), then the one nearest a Post of that room, ties by
/// id; with no Post standing, the nearest to the fighter. Empty where the room
/// holds none.
let private strikeOrder
    (view: ColonyView)
    atlas
    (targets: Selector)
    (fighting: Set<string>)
    (creep: CreepInfo)
    (room: string)
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

    // In a resident room a rival's CLAIM body is a target too (#414): unarmed,
    // it is what takes the flag, or in a raised home the controller (#447).
    let residentRooms = Facts.residentRooms view

    // In a harassment room the declared enemy's unarmed creeps are targets
    // too (#432), after its armed ones.
    view.Hostiles
    |> List.filter (fun h -> h.Pos.Room = room && targets.Shoots view residentRooms fighting h)
    |> targets.Order view room distance

/// A fighter's strike at a hostile: a ranger's shot, a guard's swing (#411).
let private strikeAt (creep: CreepInfo) (hostile: HostileInfo) : Intent =
    if isRangerBody creep then
        RangedAttackCreep(creep.Name, hostile.Id)
    else
        AttackCreep(creep.Name, hostile.Id)

/// How far a body strikes: a ranger shoots within three (#411), a guard
/// swings at what stands beside it.
let private reachOf (creep: CreepInfo) =
    if isRangerBody creep then
        Engine.rangedRange
    else
        Engine.meleeRange

/// A ranger's live guns; a melee body's none, its strike being the swing
/// (`strikeAt`).
let private gunsOf (creep: CreepInfo) =
    if isRangerBody creep then
        partCount creep.Body RangedAttack
    else
        0

/// Every creep not ours in sight, as `massDamage` reads it: a hostile we may
/// strike by the one diplomacy rule (`Facts.mayStrike`, the reflex's rooms),
/// and an ally never.
let private bystanders (view: ColonyView) (fighting: Set<string>) (from: RoomPos) =
    let near (h: HostileInfo) =
        RoomPos.range from h.Pos |> Option.exists (fun r -> r <= Engine.rangedRange)

    (view.Hostiles
     |> List.filter near
     |> List.map (fun h -> h.Pos, h.Hits, Facts.mayStrike view fighting h))
    @ (view.Allies |> List.filter near |> List.map (fun h -> h.Pos, h.Hits, false))

/// What a body strikes in a room off its Selector (`pickStrike` over
/// `strikeOrder`, beside the mass attack from where it stands).
let private strikeOf
    (view: ColonyView)
    atlas
    (targets: Selector)
    (fighting: Set<string>)
    (creep: CreepInfo)
    (room: string)
    =
    let reach = reachOf creep
    let tile = Atlas.creepTile atlas creep.Name
    let guns = gunsOf creep

    let inReach (hostile: HostileInfo) =
        tile
        |> Option.bind (fun tile -> RoomPos.range tile hostile.Pos)
        |> Option.exists (fun r -> r <= reach)

    let mass =
        match tile with
        | Some from when guns > 0 ->
            shields view from.Room
            |> Option.bind (fun shields ->
                massDamage guns from shields (bystanders view fighting from))
        | _ -> None

    strikeOrder view atlas targets fighting creep room
    |> pickStrike inReach (fun h -> min h.Hits (guns * Engine.rangedAttackPower)) mass

/// The intent of a strike `strikeOf` picks that the body reaches.
let private strikeIntent (creep: CreepInfo) (strike: Strike<HostileInfo> option) =
    match strike with
    | Some(At(target, true)) -> Some(strikeAt creep target)
    | Some Mass -> Some(RangedMassAttack creep.Name)
    | _ -> None

/// A Guard strikes the target `strikeOf` picks while it reaches it. Self-
/// healing belongs to the colony-wide reflex, which reads damage and the same
/// compatibility rules as execution. Movement is the mover's, but for the
/// melee's step onto its target (`glue`).
let private guardIntent
    (view: ColonyView)
    atlas
    (targets: Selector)
    (fighting: Set<string>)
    (creep: CreepInfo)
    (room: string)
    : Intent option =
    let reach = reachOf creep

    // With no creep to hit, a body strikes a level-0 core in its reach: a
    // guard's swing (#487), a ranger's shot (#558).
    let coreInReach () =
        Facts.expansionCores view
        |> List.tryFind (fun core ->
            core.RoomName = room
            && Atlas.creepTile atlas creep.Name
               |> Option.bind (fun tile -> RoomPos.range tile (RoomPos.at room core.Tile))
               |> Option.exists (fun r -> r <= reach))
        |> Option.map (fun core ->
            if isRangerBody creep then
                RangedAttackStructure(creep.Name, core.Id)
            else
                AttackStructure(creep.Name, core.Id))

    strikeOf view atlas targets fighting creep room
    |> strikeIntent creep
    |> Option.orElseWith coreInReach

/// The heal reflex beside any Task or none (#409): a body with an active HEAL
/// part heals itself if it is hurt, else the most-hurt creep of ours beside it,
/// else the most-hurt within `Engine.rangedRange` at the ranged rate. Existing
/// actions own their channels: a reflex never suppresses a swing, a harvest,
/// construction or another chosen action. Each heal is counted off its
/// patient's missing hits as it is planned, so two healers do not both pour
/// into a wound one of them closes.
///
/// A fighter that owes nothing pre-heals (#451): first an adjacent fighter of
/// ours standing in a Reach, at full hits too, since the engine credits a heal
/// before its death check; then the reflex above; then itself where it stands
/// in a Reach. A wounded fighter heals itself first.
let healReflex (view: ColonyView) (threats: Threats) (plan: Fabot.Core.IntentPlan.Plan) =
    let healParts (creep: CreepInfo) =
        Map.tryFind Heal creep.Body |> Option.defaultValue 0

    let tileOf (creep: CreepInfo) =
        SpatialInfo.creepPlacementOf view.Spatial creep.Name

    let inReach (creep: CreepInfo) =
        tileOf creep
        |> Option.exists (fun tile ->
            Set.contains (RoomPos.pos tile) (Threats.reachIn threats tile.Room))

    let missing =
        view.Creeps
        |> List.map (fun creep -> creep.Name, creep.Hits.HitsMax - creep.Hits.Hits)
        |> Map.ofList

    // An adjacent fighter in a Reach, the deepest owed first: a pre-heal
    // already planned leaves it owing less, so a second healer turns to
    // another.
    let shieldFor (healer: CreepInfo) (owed: Map<string, int>) =
        tileOf healer
        |> Option.bind (fun at ->
            view.Creeps
            |> List.filter (fun other ->
                other.Name <> healer.Name
                && isFighterBody other
                && inReach other
                && tileOf other
                   |> Option.bind (RoomPos.range at)
                   |> Option.exists (fun r -> r <= Engine.meleeRange))
            |> List.sortBy (fun other -> -(Map.find other.Name owed), other.Name)
            |> List.tryHead
            |> Option.map (fun other -> other, Engine.meleeRange))

    let woundFor (healer: CreepInfo) (owed: Map<string, int>) =
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

    // A shield is spent only by a fighter that owes nothing itself.
    let patientFor (healer: CreepInfo) (owed: Map<string, int>) =
        if isFighterBody healer && Map.find healer.Name owed <= 0 then
            shieldFor healer owed
            |> Option.orElse (woundFor healer owed)
            |> Option.orElse (if inReach healer then Some(healer, 0) else None)
        else
            woundFor healer owed

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

                match Fabot.Core.IntentPlan.tryReflex intent plan with
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

/// Whether a Thorium harvest is held this tick by its mine's container (#497):
/// less room in it than one dig yields. The miner has no Carry, so the dig
/// lands on the floor, where Thorium decays; the deposit does not regenerate,
/// so the wait loses nothing. Withheld here and not in applicability, for the
/// cooldown's reason: the miner keeps its Task and its seat. A full tick that
/// withholds the act leaves no `HarvestSource` for the light ticks to replay.
let private heldByFullContainer (view: ColonyView) atlas (creep: CreepInfo) task =
    match task with
    | Harvest rockId when Atlas.isMineral atlas rockId ->
        let dig = partCount creep.Body Work * Engine.mineralHarvestPerWork

        Facts.ourMineralContainerPairs view
        |> List.tryFind (fun (markId, _) -> markId = rockId)
        |> Option.exists (fun (_, containerId) ->
            let held =
                SpatialInfo.heldIn view.Spatial Thorium containerId
                + SpatialInfo.storedIn view.Spatial containerId

            Engine.containerCapacity - held < dig)
    | _ -> false

/// The standing upgrader's draw beside its Upgrade (#498): a standing body
/// within reach of a buffer holding energy withdraws from it while it has
/// room. The engine runs a creep's `withdraw` before its `upgradeController`
/// (`creepActions` in `processor/intents/creeps/intents.js`), so the draw
/// tops up what the upgrade spends and the Task never changes. Not a
/// generalist's: one that never emptied would never leave for a site.
let private topUp (view: ColonyView) atlas (creep: CreepInfo) : Intent list =
    if creep.FreeCapacity <= 0 || not (isStandingBody view.Tuning creep) then
        []
    else
        match SpatialInfo.creepPlacementOf view.Spatial creep.Name with
        | None -> []
        | Some tile ->
            Atlas.controllerContainers atlas
            |> Seq.tryFind (fun buffer ->
                SpatialInfo.heldIn view.Spatial Energy buffer > 0
                && SpatialInfo.placementOf view.Spatial buffer
                   |> Option.bind (RoomPos.range tile)
                   |> Option.exists (fun r -> r <= Engine.meleeRange))
            |> Option.map (fun buffer -> WithdrawFromStore(creep.Name, buffer, Energy, None))
            |> Option.toList

/// A [[defender]]'s swing (#504), and only from a rampart of ours: off one, a
/// swing at an ATTACK body takes its strike-back. At the body of the raid at
/// the line beside it with the fewest hits, else at any hostile beside it,
/// ties by id: its Defend's `Brief.Targets`.
let private defendIntent
    (view: ColonyView)
    atlas
    (threats: Threats)
    (room: string)
    (creep: CreepInfo)
    : Intent option =
    match SpatialInfo.creepPlacementOf view.Spatial creep.Name with
    | Some here when Set.contains (RoomPos.pos here) (Atlas.ourRampartTilesIn atlas here.Room) ->
        let beside (hostile: HostileInfo) =
            RoomPos.range here hostile.Pos
            |> Option.exists (fun r -> r <= Engine.meleeRange)

        Threats.briefFor threats (Defend room)
        |> Option.bind (fun brief ->
            let line = brief.Targets

            let pick (targets: HostileInfo list) =
                targets
                |> List.filter beside
                |> line.Order view here.Room (fun _ -> 0)
                |> List.tryHead

            pick (view.Hostiles |> List.filter (line.Shoots view Set.empty Set.empty))
            |> Option.orElse (pick view.Hostiles))
        |> Option.map (fun target -> AttackCreep(creep.Name, target.Id))
    | _ -> None

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position, and — for Harvest alone —
/// only while the source holds energy. Anticipatory dispatch and the occupancy
/// surcharge both price a walk high enough to land a creep a tick or two early,
/// so the gate is what keeps the engine's ERR_NOT_ENOUGH_RESOURCES spam
/// structurally impossible. The Guard is judged outside that gate: its acts
/// reach a creep the projection places nothing for, so `Atlas.mayAct` answers
/// false for it on every tick, and the range it is really gated on is the
/// swing `guardIntent` measures itself.
/// A non-fighting Task's act, behind the gates that withhold it: out of its
/// Work Area, a drained rock, a cooldown, a full mine container.
let private taskAct
    (view: ColonyView)
    atlas
    (threats: Threats)
    (partyWalks: Map<string, int list>)
    (creep: CreepInfo)
    (task: Task)
    (drained: bool)
    : Intent option =
    if
        mayActNow threats atlas creep.Name task
        && not drained
        && not (heldByCooldown atlas task)
        && not (heldByFullContainer view atlas creep task)
    then
        intentFor view atlas partyWalks creep task
    else
        None

let private actionIntents
    (view: ColonyView)
    atlas
    (threats: Threats)
    (partyWalks: Map<string, int list>)
    (creep: CreepInfo)
    (task: Task)
    : Intent list =
    let drained = restockWait view task > 0

    let fighting = Threats.fightRooms threats

    match task with
    // A holder mustering (#535) waits out of reach and strikes nothing on its
    // way there, where a swing at a melee takes its strike-back.
    | Guard room when Map.containsKey creep.Name (Threats.briefOf threats room).Stances -> []
    | Guard room ->
        guardIntent view atlas (Threats.briefOf threats room).Targets fighting creep room
        |> Option.toList
    // A medic's act is the heal reflex's; a brawler swings and a kiter shoots
    // as a guard and a ranger do, at the room's Guard's targets.
    | Fight _ when Threats.squadRoleOf threats task creep.Name = Some Medic -> []
    | Fight room ->
        guardIntent view atlas (Threats.briefOf threats room).Targets fighting creep room
        |> Option.toList
    | Defend room when isRangerBody creep ->
        Threats.briefFor threats task
        |> Option.bind (fun brief -> guardIntent view atlas brief.Targets fighting creep room)
        |> Option.toList
    | Defend room -> defendIntent view atlas threats room creep |> Option.toList
    // A launched sapper or probe takes its target down from beside it; a
    // medic's act is the heal reflex's.
    | Assault _ ->
        match Threats.formationOf threats task with
        | Some formation when
            formation.Launched
            && Threats.squadRoleOf threats task creep.Name
               |> Option.exists SquadRole.dismantles
            ->
            formation.Target
            |> Option.filter (fun (_, tile) ->
                SpatialInfo.creepPlacementOf view.Spatial creep.Name
                |> Option.bind (RoomPos.range tile)
                |> Option.exists (fun r -> r <= Engine.meleeRange))
            |> Option.map (fun (id, _) -> DismantleStructure(creep.Name, id))
            |> Option.toList
        | _ -> []
    | _ ->
        match taskAct view atlas threats partyWalks creep task drained with
        | Some(UpgradeController _ as act) -> act :: topUp view atlas creep
        | act -> Option.toList act

/// Emitter: each assigned creep's action Intent, then every assigned
/// creep's chat bubble, both in view creep order. Judges actions from
/// tick-start geometry — it must run against the same Atlas the Matcher
/// used, never against resolved positions.
let emit (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) : Intent list =
    let partyWalks = partyWalksOf view atlas threats assigned

    let actions =
        view.Creeps
        |> List.collect (fun creep ->
            match Map.tryFind creep.Name assigned with
            | Some task -> actionIntents view atlas threats partyWalks creep task
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

/// A hauler's act on arrival (#529): its Withdraw's, Pickup's or Refill's act
/// as this Emitter issues it standing on `landing`, the tile it steps onto
/// this tick — the same gates, the same `intentFor` (`Atlas.standingAt`). The
/// light tick after issues it. None for any other Task, off its room, for a
/// drop, which lands where the body stands, and for a draw naming an amount,
/// which the light tick replays nowhere.
let arrival
    (view: ColonyView)
    atlas
    (threats: Threats)
    (creep: CreepInfo)
    (task: Task)
    (landing: RoomPos)
    : Intent option =
    match task with
    | Withdraw _
    | Pickup _
    | Refill _ ->
        Atlas.standingAt atlas creep.Name landing (fun () ->
            taskAct view atlas threats Map.empty creep task (restockWait view task > 0))
        |> Option.flatten
        |> Option.filter (function
            | DropEnergy _
            | WithdrawFromStore(_, _, _, Some _) -> false
            | _ -> true)
    | _ -> None

/// How a body strikes this tick (#535): the room and the Selector it picks
/// its target by, and whether that strike is its Task's own act (a fighter)
/// or the weapon reflex. A Guard's or a Fight's holder, a medic aside, by its
/// Task's room; any other body with a weapon — on Flee, idle, at work — by the
/// room it stands in and that room's Guard's targets, as the heal reflex heals
/// beside any Task. A ranger holding the home's Defend by its line. None for
/// a melee defender, whose swing from its rampart is its own, a squad's
/// sapper, and a body with no weapon.
let private strikesBy
    (atlas: Atlas.Atlas)
    (threats: Threats)
    (assigned: Map<string, Task>)
    (creep: CreepInfo)
    =
    let armed = partCount creep.Body Attack > 0 || partCount creep.Body RangedAttack > 0

    let own room =
        Some(room, (Threats.briefOf threats room).Targets)

    // The weapon reflex's targets: what the one diplomacy rule lets us strike
    // (`Facts.mayStrike`), in the Guard's kill order.
    let reflexTargets room =
        {
            Shoots = fun view _ engaged hostile -> Facts.mayStrike view engaged hostile
            Order = (Threats.briefOf threats room).Targets.Order
        }

    let reflex () =
        Atlas.creepRoom atlas creep.Name
        |> Option.map (fun room -> room, reflexTargets room, false)

    match Map.tryFind creep.Name assigned with
    // A holder mustering (#535): a ranger fires its reflex — a shot draws no
    // strike-back —, a melee body swings at nothing.
    | Some(Guard room) when Map.containsKey creep.Name (Threats.briefOf threats room).Stances ->
        if isRangerBody creep then reflex () else None
    | Some(Guard room) -> own room |> Option.map (fun (room, targets) -> room, targets, true)
    | Some(Fight _ as task) when Threats.squadRoleOf threats task creep.Name = Some Medic -> None
    | Some(Fight room) -> own room |> Option.map (fun (room, targets) -> room, targets, true)
    // A ranger holding the home's Defend shoots its line as a Guard's
    // holder its room's (#546).
    | Some(Defend room as task) when isRangerBody creep ->
        Threats.briefFor threats task
        |> Option.map (fun brief -> room, brief.Targets, true)
    | Some(Defend _)
    | Some(Assault _) -> None
    | _ when armed -> reflex ()
    | _ -> None

/// Whether a body holds its Guard waiting on the muster ground (#535).
let private mustering (threats: Threats) (assigned: Map<string, Task>) (creep: CreepInfo) =
    match Map.tryFind creep.Name assigned with
    | Some(Guard room) -> Map.containsKey creep.Name (Threats.briefOf threats room).Stances
    | _ -> false

/// Each fighter's aims (#528, #535): a holder of a Guard or a Fight, not
/// mustering, aims at every target of its kill order (`strikeOrder`), head
/// first. The light ticks after pick one of them again off the glance, the
/// one in reach first, else the head (`pickStrike`, `LightTick.intents`), so a
/// body that steps beside a target between full ticks swings at it; out of
/// reach the engine refuses it, and a refused act costs no CPU. A medic aims
/// at nothing, and nor does a body striking by the reflex alone.
let aims (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) : Intent list =
    match view.Hostiles with
    | [] -> []
    | _ ->
        let fighting = Threats.fightRooms threats

        view.Creeps
        |> List.collect (fun creep ->
            match strikesBy atlas threats assigned creep with
            | Some(room, targets, true) when not (mustering threats assigned creep) ->
                strikeOrder view atlas targets fighting creep room |> List.map (strikeAt creep)
            | _ -> [])

/// Each aiming ranger's `Volley` (#537), for the light ticks' mass attack:
/// a body that aims (`aims`) with a gun, in a room whose shields this view
/// carries (`shields`).
let volleys (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) =
    match view.Hostiles with
    | [] -> Map.empty
    | _ ->
        view.Creeps
        |> List.choose (fun creep ->
            match strikesBy atlas threats assigned creep, Atlas.creepRoom atlas creep.Name with
            | Some(_, _, true), Some room when
                gunsOf creep > 0 && not (mustering threats assigned creep)
                ->
                shields view room
                |> Option.map (fun shields ->
                    creep.Name,
                    {
                        Room = room
                        Guns = gunsOf creep
                        Shields = shields
                    })
            | _ -> None)
        |> Map.ofList

/// The weapon reflex beside any Task or none (#535): every body with a weapon
/// that holds no fight of its own strikes the target `strikeOf` picks in its
/// room while it reaches it, on a channel the plan leaves free — the heal
/// reflex's rule (`healReflex`), run before it, so a shot beside a fleeing
/// step and a swing beat a pre-heal. It never moves for it.
let strikeReflex
    (view: ColonyView)
    atlas
    (threats: Threats)
    (assigned: Map<string, Task>)
    (plan: Fabot.Core.IntentPlan.Plan)
    =
    match view.Hostiles with
    | [] -> plan
    | _ ->
        let fighting = Threats.fightRooms threats

        (plan, view.Creeps)
        ||> List.fold (fun plan creep ->
            match strikesBy atlas threats assigned creep with
            | Some(room, targets, false) ->
                match strikeOf view atlas targets fighting creep room |> strikeIntent creep with
                | Some strike ->
                    match Fabot.Core.IntentPlan.tryAdd strike plan with
                    | Ok joined -> joined
                    | Error _ -> plan
                | None -> plan
            | _ -> plan)

/// Each melee fighter's step onto the target it reaches (`glueStep`, #535):
/// a Guard's holder with no gun, not mustering, beside the swing
/// `guardIntent` issues at the same target; never a squad's member, whose
/// movement is its Formation's. The mover leaves these bodies standing.
let glue (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) : Intent list =
    match view.Hostiles with
    | [] -> []
    | _ ->
        let fighting = Threats.fightRooms threats

        view.Creeps
        |> List.choose (fun creep ->
            match Map.tryFind creep.Name assigned, strikesBy atlas threats assigned creep with
            | Some(Guard _), Some(room, targets, true) when
                not (isRangerBody creep)
                && partCount creep.Body Attack > 0
                && not (mustering threats assigned creep)
                ->
                match
                    strikeOf view atlas targets fighting creep room,
                    Atlas.creepTile atlas creep.Name
                with
                | Some(At(target, true)), Some from ->
                    let melee =
                        view.Hostiles
                        |> List.filter (fun h ->
                            h.Pos.Room = from.Room && HostileInfo.melee view.Hostiles h)
                        |> List.map (fun h -> h.Pos)

                    let onRampart =
                        Set.contains (RoomPos.pos from) (Atlas.ourRampartTilesIn atlas from.Room)

                    glueStep from target.Pos onRampart melee
                    |> Option.map (fun direction -> MoveCreep(creep.Name, direction))
                | _ -> None
            | _ -> None)
