/// The Planner: the tick's Task list. The source-container and claim geometry it
/// rests on, which rooms are nursery, bootstrap or outpost, and `planTasks`
/// itself — what there is to do, before anyone is matched to it.
[<AutoOpen>]
module Fabot.Core.Decide.Planner

open Fabot.Core
open Fabot.Core.Types

/// The source container geometry the Layout plans by: a tile within range 1 of
/// the given rock is that rock's container tile. One rule over two kinds of
/// rock — the Layout asks it of a Thorium deposit too. What a *standing*
/// container serves is the Post census's (`Atlas.sourceOfPost`), not this.
let internal servesSource (rockPos: Pos) (tile: Pos) = range tile rockPos <= 1

/// Whether this colony owns the named room. A room with no control entry is
/// one the colony cannot see, and an unseen room is not one it owns.
let private colonyOwns (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// Whether the named room carries an owner at all — one spelling for the two
/// rules of the reserver's that read it. `reserveController` answers
/// ERR_INVALID_TARGET on a controller with an owner, *whoever* it is, so the
/// Reserve pool must not carry its controller and the row must not hire against
/// it. A rival's counts too, because the stand-down reads the *previous* tick's
/// raid log and the one tick between first sight and the withdrawal cast the
/// bank's whole reserver body at a room the engine refuses.
let private roomHasOwner (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner <> Ownership.Unowned)

/// The controllers a Claim is pooled for this tick, each with the room it
/// stands in — one spelling for the two rules that read it: the Task pool
/// offers exactly these and the reserver row hires one body for each. A
/// candidate colony is a declared home this colony does not own yet, and both
/// halves are needed: the declaration, because no projected fact distinguishes
/// a room we mean to own from a neighbour we merely mine; and the ownership,
/// because the tick the claim lands this pool empties itself, with no state
/// kept.
let internal claimTargets (view: ColonyView) : (string * string) list =
    let takeable room =
        match Map.tryFind room view.RoomControl with
        | Some control ->
            control.Owner = Ownership.Unowned
            && RoomControlInfo.heldByOther control |> Option.isNone
        | None -> false

    let candidate room =
        List.contains room view.Declared && takeable room

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.choose (fun id ->
        match SpatialInfo.placementOf view.Spatial id with
        | Some tile when candidate tile.Room -> Some(id, tile.Room)
        | _ -> None)

/// Whether the named room is this colony's nursery: a declared colony of ours
/// that has been claimed and has no spawn of its own yet. Three rules read that
/// state — every site in it is feeding-tier work, the concurrent-builder budget
/// does not reach those sites, and the worker row hires `Tuning.PioneerCount`
/// more bodies — so it is one spelling and not three gates free to disagree.
/// Two facts: the room's stage is `Nursery`, and this colony projects the room
/// (`colonyOwns`), which is what keeps a second declarer from hiring pioneers
/// for a child it never projects.
let internal isNurseryRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && roomStage view room = Some Nursery

/// Whether the named room is a child colony this one is still bootstrapping: a
/// declared colony of ours that stands its own spawn, and that this colony is
/// nonetheless projecting. `isNurseryRoom`'s two facts with the stage inverted,
/// so the two are complements over one room. Both standing stages, because
/// what closes the borrowing is the scan set and never a level read here.
let internal isBootstrapRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && (match roomStage view room with
        | Some Bootstrapping
        | Some Independent -> true
        | Some Nursery
        | None -> false)

/// Whether an Upgrade in this pool is borrowed: its controller is not this
/// colony's own, so it is a bootstrapped child's, pooled for the pioneers.
let internal isBorrowedUpgrade (view: ColonyView) controllerId =
    view.Controller |> Option.exists (fun c -> c.Id = controllerId) |> not

/// The ferry's sinks: the upgrade buffers of the children this colony is
/// bootstrapping. Three readers must name the same store or the colony hires a
/// body for a Task nobody pooled — the hauler quota's ferry term, `planTasks`'
/// Refill, and `planPool`'s bound. Read off what the view carries and not off
/// the geometry again: the join cannot be respelled here because it subtracts
/// the tiles beside the child's sources, which the same narrowing drops.
let internal ferryBuffers (view: ColonyView) : Set<string> =
    let rooms =
        view.Borrowed.Rooms
        |> List.filter (fun room -> roomStage view room = Some Bootstrapping)
        |> Set.ofList

    if Set.isEmpty rooms then
        Set.empty
    else
        view.Spatial.Stores
        |> Map.toList
        |> List.map fst
        // A container and not merely a store: the kind is the one half of the
        // join that survives the narrowing, and it keeps a tombstone or a pile
        // lying in that room out of a Refill it could never be filled through.
        |> List.filter (fun id ->
            Map.tryFind id view.Spatial.TargetKinds = Some(Structure BuiltKind.Container)
            && (SpatialInfo.roomOf view.Spatial id
                |> Option.exists (fun room -> Set.contains room rooms)))
        |> Set.ofList

/// The controllers of the rooms this colony works as outposts: every controller
/// the projection carries that is not this colony's home, stands in no room
/// somebody owns, and is no candidate colony's. The engine refuses
/// `reserveController` on an owned room, so an owned room's controller is one
/// this colony is withdrawing from rather than mining.
///
/// Ids and not rooms, because a controller the projection does not place names
/// no room and is still ours to reserve — it is the Reserve pool that reads it
/// that way, while `declaredOutposts` takes the rooms and loses the unplaced
/// one. One scan for all three readers, or the rows and the pool could
/// disagree about which rooms are ours to work.
let private outpostControllers (view: ColonyView) : string list =
    let home = view.Controller |> Option.map (fun c -> c.Id)
    let claimed = claimTargets view |> List.map fst |> Set.ofList

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.filter (fun id ->
        Some id <> home
        && not (Set.contains id claimed)
        && not (SpatialInfo.roomOf view.Spatial id |> Option.exists (roomHasOwner view)))

/// Those of them this colony may actually hold a reserve on this tick: the ones
/// nobody else's CLAIM parts are holding (#333). The engine refuses
/// `reserveController` on a controller anybody but us reserves, so a body hired
/// for such a room stands adjacent and is refused for its whole 600-tick life.
/// One read for the row and the pool, so a reserver bought for the colony's
/// *other* outpost cannot be handed this controller by travel cost.
///
/// Two reads, because vision is the thing the refusal takes away: in a room
/// with no container the reserver is the only body that ever stands there, so
/// read off vision alone the rule is self-erasing — the entry appears, the
/// Task vanishes, the idle body holds the vision until it dies, and the row
/// hires again. A tick with vision decides the room either way; a tick without
/// it reads the last look's record (`view.HeldOutposts`). Vision first, so a
/// look that finds the controller free opens the room on the tick it takes.
/// A room with no entry and no record is reservable, because the reserver is
/// the creep whose walk buys the look (#131's deadlock). `declaredOutposts` is
/// not narrowed: a room somebody else reserves is still a room this colony
/// mines.
let private reservableControllersOf (view: ColonyView) (controllers: string list) : string list =
    controllers
    |> List.filter (fun id ->
        match SpatialInfo.roomOf view.Spatial id with
        // A controller the projection does not place names no room, and a room
        // name is what both reads below are keyed by — so it stays pooled.
        | None -> true
        | Some room ->
            match Map.tryFind room view.RoomControl with
            | Some control -> RoomControlInfo.heldByOther control |> Option.isNone
            | None -> not (Set.contains room view.HeldOutposts))

/// The declared outposts this colony works this tick — the rooms the guard row
/// is hired per and the rooms the reserver row starts from, written once so
/// the two cannot disagree. A room qualifies by carrying a controller of its
/// own in the projection that is not this colony's home, unowned, and not a
/// candidate colony's.
///
/// Declared and not posted (#131): gating on a standing container deadlocks
/// the outpost chain, since the container needs vision, vision needs a creep,
/// and the reserver is the only creep with a reason to go. The scan set is the
/// gate that remains, narrowed inside `World.scanOf` by the stand-down and by
/// `Outpost.withinHopBudget`; what *says* so is `Outpost.refused` on the
/// layout record.
///
/// Off the projection itself (`SpatialInfo.placementOf`) and not off the
/// Atlas's join of it: the Planner's first half is handed the view and no
/// Atlas. The derivation itself is `outpostControllers`: this takes its rooms,
/// the Reserve pool takes its ids once `reservableControllers` has dropped the
/// controllers somebody else holds, and the reserver row takes the rooms of
/// *that*.
let internal declaredOutposts (view: ColonyView) : string list =
    outpostControllers view
    |> List.choose (SpatialInfo.roomOf view.Spatial)
    |> List.distinct

/// The declared outposts the reserver row hires for: the rooms of
/// `reservableControllers`, which is what makes the row and the Reserve pool
/// one answer rather than two.
let private reservableOutpostsOf (view: ColonyView) (reservable: string list) : string list =
    reservable |> List.choose (SpatialInfo.roomOf view.Spatial) |> List.distinct

/// The declared outposts a threat stands in this tick: the rooms the guard row
/// hires a body for, and the rooms `planTasks` pools a Guard in — one
/// derivation, or a Guard is pooled with no body bought or a body cast with no
/// Task. A Threat and never "a hostile".
///
/// Vision is not the whole of what it reads (#366): the vision in an
/// *unguarded* outpost is the anchor, the hauler and the reserver, which are
/// precisely what a raid kills, and read off vision alone the Guard left the
/// pool while the cast body stood idle at home. So a tick with vision decides
/// the room either way, and a tick without reads the last look's record
/// (`view.ThreatenedOutposts`), which ends by its own clock
/// (`Tuning.ThreatMemory`) because nothing in the engine counts a raid down. A
/// room never looked into is in neither half: this rule buys a body, not a look.
let private guardedOutpostsOf (view: ColonyView) (declared: string list) : string list =
    if List.isEmpty view.Hostiles && Set.isEmpty view.ThreatenedOutposts then
        []
    else
        declared
        |> List.filter (fun room ->
            let seenArmed =
                view.Hostiles
                |> List.exists (fun h -> h.Pos.Room = room && (weaponRange h |> Option.isSome))

            // The union of the two halves and not a two-armed test on vision,
            // so the blind half can never take a room the tick's own hostiles
            // put in: the record is the previous tick's, and this tick's raid
            // outranks it wherever both answer.
            seenArmed
            || (Set.contains room view.ThreatenedOutposts
                && not (Map.containsKey room view.RoomControl)))

/// The outpost chain's answers for this tick, derived once and carried (#383):
/// re-entered per reader, the `Controller` census was walked nineteen times a
/// tick on `--scenario reactor --level 7`. The shape is `RowSizing`'s and
/// `HeldTaskFacts`', because it needs no mutable table and so no staleness
/// rule. Eager: every field is read on a quiet tick, and the one that is not,
/// `Guarded` with no hostile anywhere, short-circuits before it walks anything.
type OutpostFacts =
    {
        /// `claimTargets`' answer: each candidate colony's controller, with
        /// the room it stands in.
        Claims: (string * string) list
        /// `declaredOutposts`' answer: the rooms this colony works.
        Declared: string list
        /// The controller ids the Reserve pool offers, somebody else's hold
        /// already dropped (#333).
        ReservableControllers: string list
        /// The rooms of those controllers, which is what the reserver row
        /// hires per — one body per room the pool offers a controller in.
        ReservableRooms: string list
        /// The declared outposts a threat stands in.
        Guarded: string list
    }

/// Derive the chain once, sharing each stage with the next. That sharing is
/// the other half of the saving: `reservableOutposts` re-entered
/// `reservableControllers`, which re-entered `outpostControllers`, which
/// re-entered `claimTargets`, so one ask at the end of the chain used to walk
/// the census four times.
let outpostFactsOf (view: ColonyView) : OutpostFacts =
    let controllers = outpostControllers view

    let declared =
        controllers |> List.choose (SpatialInfo.roomOf view.Spatial) |> List.distinct

    let reservable = reservableControllersOf view controllers

    {
        Claims = claimTargets view
        Declared = declared
        ReservableControllers = reservable
        ReservableRooms = reservableOutpostsOf view reservable
        Guarded = guardedOutpostsOf view declared
    }

/// Planner: rebuild this tick's full Task pool from the colony view. Pure and
/// from scratch every tick — Tasks are never persisted.
///
/// `held` is the narrow pair of facts this half reads about the colony's own
/// assignment table (ADR-0061, ADR-0067): all task ids living creeps hold, and
/// the subset whose holder still carries Thorium, derived once in `Entry`
/// (`heldTaskFacts`). Repairs read the first to pick a decaying kind's line; a
/// partially poured delivery reads the second so it cannot move to a different
/// carrier when its empty holder releases it. The Planner still sees no body,
/// position or name.
///
/// What the held Repairs change is not the Repair line alone: `Quota.workerFloor`
/// stands the worker row at two while anything stands in the Build or Repair
/// pool, so a held road between the two lines keeps that floor where an empty
/// pool would have dropped it to one. Masked at the shipped tuning, where
/// `MinWorkforce` binds first.
let planTasks
    (view: ColonyView)
    atlas
    (threats: Threats)
    (held: HeldTaskFacts)
    (outposts: OutpostFacts)
    : Task list =
    // Flee exists while a Reach does: one Task for the whole colony, at the
    // head of the pool as its Safety tier is at the head of the ranking.
    let flees = if Map.isEmpty threats.Reach then [] else [ Flee ]

    // One Guard per declared outpost a threat stands in, keyed on the room:
    // the same list the guard row hires against, so the body the cascade buys
    // has a Task waiting for it and no Task waits for a body nobody bought.
    let guards = outposts.Guarded |> List.map Guard

    // Harvest exists for every source, drained or not (ADR-0025): whether a
    // dry rock is worth walking to depends on the walker's body and position —
    // the Matcher's knowledge, and none of this half's.
    let harvests = view.Sources |> List.map (fun s -> Harvest s.Id)

    // One Harvest per Thorium deposit beside them: the same Task kind, so
    // every exhaustive match over `Task` grows no arm. The Task exists exactly
    // while the deposit does — the mod deletes an exhausted deposit outright —
    // and the extractor's cooldown decides only whether this tick's act is
    // issued (the Emitter's gate). A deposit of ours and never a neighbour's
    // (#261): `FIND_MINERALS` carries every owner's, `harvest` refuses a
    // mineral whose extractor is somebody else's, and nothing in the shape of
    // a deposit, extractor and container says whose they are. `ourDeposits`
    // is that join, read here and by the miner row's quota alike.
    let deposits = ourDeposits view |> List.map Harvest

    // The flow's sink, as one Task (ADR-0054): the refill cluster is pooled
    // under the spawn's id and stands while any member has room.
    let cluster = Atlas.cluster atlas

    let clustered =
        cluster
        |> Option.map (fun c -> c.Members |> Map.toList |> List.map fst |> Set.ofList)
        |> Option.defaultValue Set.empty

    let refills =
        (cluster
         |> Option.filter (fun c -> RefillCluster.free c > 0)
         |> Option.map (fun c -> Refill(c.Spawn, Energy))
         |> Option.toList)
        @ (view.Refillables
           |> List.filter (fun r -> r.FreeCapacity > 0 && not (Set.contains r.Id clustered))
           |> List.map (fun r -> Refill(r.Id, Energy)))

    let builds = view.ConstructionSites |> List.map (fun site -> Build site.Id)

    // A Repair per repairable structure below its kind's line, in id order —
    // two lines for the decaying kinds, the hungry one for a structure nobody
    // holds and the whole one for a structure somebody is already repairing.
    let repairs = hungryStructures view held.All |> List.map (fst >> Repair)

    // The ids of one projected kind, in id order, off the Atlas's inverted
    // census and not a walk of the view's: same ids, same order.
    let idsOfKind kind = Atlas.idsOfKind atlas kind

    // The colony's own controller, and the controller of every child it is
    // still bootstrapping: a loaded worker of the mother's may cross the Seam
    // and spend into the child's controller until it reaches
    // `Tuning.BootstrapLevel`. Surplus tier, like the home Upgrade it stands
    // beside, so nothing but travel cost separates the two.
    let upgrades =
        let own = view.Controller |> Option.toList |> List.map (fun c -> c.Id)

        let children =
            idsOfKind Controller
            |> List.filter (fun id ->
                SpatialInfo.roomOf view.Spatial id |> Option.exists (isBootstrapRoom view))

        own @ children |> List.map Upgrade

    // One Claim per candidate colony's controller (`claimTargets`).
    let claims = outposts.Claims |> List.map (fst >> Claim)

    // One Reclaim per declared errand, read off the declaration and off no
    // kind census at all: `Errand.place` lays the target under its engine id
    // with no `TargetKind`, so every pool built by sweeping kinds passes it
    // over. A refused errand is already out of that list (`World.scanOf`).
    let reclaims = view.Errands |> List.map (fun errand -> Reclaim(fst errand.Target))

    // One Reserve per reservable outpost controller. The Task stands whatever
    // the reservation has left on it — the ticks remaining size the body, not
    // the pool. Read off the projection's kind census and never off the
    // declared outposts, so a room a stand-down keeps out of the scan set
    // leaves this pool with it rather than through a second gate free to
    // disagree. The engine refuses `reserveController` on any owned room, and
    // equally on a controller somebody else's CLAIM parts hold (#333). A
    // controller the projection does not place names no room and stays pooled.
    let reserves = outposts.ReservableControllers |> List.map Reserve

    // The haul cycle's intake, shaped over the projection's stores rather than
    // energy's name: every stocked container yields a Withdraw, at feeding tier
    // beside Harvest — whether to dig or to collect is travel cost's call.
    let stored id = SpatialInfo.storedIn view.Spatial id

    let containers = idsOfKind (Structure BuiltKind.Container)
    let storages = idsOfKind (Structure BuiltKind.Storage)

    // The room left in a Storage, over both resources it holds: a store's
    // capacity is one number, so the energy sink and the Thorium sink read one
    // free-capacity question rather than two that can each be satisfied by
    // ignoring the other.
    let storageRoom id =
        Engine.storageCapacity - stored id - SpatialInfo.heldIn view.Spatial Thorium id

    // A tombstone and a ruin are stores the same way, so they pool through the
    // same line: the engine's `withdraw` takes either object, and the cap and
    // the tier are read off the stock and the kind exactly as a container's
    // are. This list is the energy column; `tombstoneOre` below is the same
    // sentence one resource over.
    let tombstones = idsOfKind Tombstone

    // The ferry's sink is the one store in this pool a body of this colony's
    // may fill and may never draw: it is a child's, and the whole of the lend
    // is energy going one way. Left in, the mother's hauler would take the
    // load she just carried across the Seam straight back out.
    let ferrySinks = ferryBuffers view

    // No store of a child's is hers to draw, at any stage: what one colony may
    // take of another is the explicit list `BorrowedWork` carries, and no store
    // is on it.
    let borrowedRooms = Set.ofList view.Borrowed.Rooms

    let inABorrowedRoom id =
        SpatialInfo.roomOf view.Spatial id
        |> Option.exists (fun room -> Set.contains room borrowedRooms)

    let withdraws =
        containers @ tombstones
        |> List.filter (fun id -> stored id > 0 && not (inABorrowedRoom id))
        |> List.map (fun id -> Withdraw(id, Energy))

    // The piles worth walking to: a dropped pile at or over
    // `Tuning.PickupThreshold` is a Feeding-tier Task, and every smaller one is
    // left to the reflex that costs nothing. The amount and nothing else:
    // whether the pile is at somebody's feet already is a fact about a creep.
    let pickups =
        idsOfKind (Dropped Energy)
        |> List.filter (fun id -> stored id >= view.Tuning.PickupThreshold)
        |> List.map (fun id -> Pickup(id, Energy))

    // The haul cycle's outflow: the controller's buffer is one more Refill
    // target, and a source container never is.
    let containerRefills =
        let buffers = Atlas.controllerContainers atlas

        containers
        |> List.filter (fun id -> Set.contains id buffers && stored id < Engine.containerCapacity)
        |> List.map (fun id -> Refill(id, Energy))

    // The colony's stock is the outflow's last stop: a standing Storage with
    // room is one more Refill target, on the deepest tier of all.
    let storageRefills =
        storages
        |> List.filter (fun id -> storageRoom id > 0)
        |> List.map (fun id -> Refill(id, Energy))

    // The mine-to-Storage leg (ADR-0057 decision 3): the one pair of Tasks in
    // this colony that is not about energy. The intake is the mineral container
    // under the miner's feet; the sink is the Storage, an obstacle nothing can
    // stand on, so the contact penalty never reaches it. Read off
    // `ourDeposits` and never off a container census (#261), with the same
    // borrowed-room filter the energy Withdraws carry.
    let mineralContainers =
        ourMineralContainers view |> List.filter (inABorrowedRoom >> not)

    let mineWithdraws =
        mineralContainers
        |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)
        |> List.map (fun id -> Withdraw(id, Thorium))

    // And the same intake off the floor (#311): every tick the haul lags, the
    // next dig lands on the ground as a pile decaying at `ceil(amount / 1000)`
    // a tick. The energy Pickup's two rules read down the Thorium column: our
    // ground (`ourThoriumPiles`) and `Tuning.PickupThreshold`. The pickup
    // reflex is no fallback: it is energy-only (`Atlas.droppedEnergyIn`). No
    // borrowed-room filter, because a child's floor is not `borrowable` at all
    // (`ColonyView.borrowable` refuses `Dropped _`), so there is nothing to
    // filter. The hauler quota prices the miner's output into the *container*
    // (`Quota.mineRows`) and is not widened for the floor.
    let minePickups =
        ourThoriumPiles view
        |> List.filter (fun id ->
            SpatialInfo.heldIn view.Spatial Thorium id >= view.Tuning.PickupThreshold)
        |> List.map (fun id -> Pickup(id, Thorium))

    // And the same ore in a store with a clock on it (#359): a tombstone or a
    // ruin holding Thorium, where a courier that dies loaded leaves it. No
    // threshold, where the pile carries one: no reflex empties a store, so the
    // alternative to a Task is watching it decay, and the Withdraw's own
    // `worthTheTrip` exempts a store that ends. Its sink is `mineRefills`
    // below, or the Reactor where the programme is open.
    let tombstoneOre =
        ourThoriumTombstones view
        |> List.filter (inABorrowedRoom >> not)
        |> List.map (fun id -> Withdraw(id, Thorium))

    // The sink, pooled off the Storage alone and off no fact about the mine
    // (#262): a body already holding Thorium must have somewhere to put it
    // down. Gated on a standing mineral container it stranded a laden hauler —
    // applicable to nothing, counted as living, replaced by nobody — for up to
    // 1,500 ticks. Inapplicable to a body holding no Thorium, so the entry is
    // the whole of what pooling it unconditionally costs.
    let mineRefills =
        storages
        |> List.filter (fun id -> storageRoom id > 0)
        |> List.map (fun id -> Refill(id, Thorium))

    // The delivery pair (#319), the same resource-aware cycle as the mine haul
    // now that the Atlas prices the three-crossing errand. The start sentence
    // gates a new draw; a load already drawn keeps its sink if the mine is
    // exhausted, the Storage falls below one load, or the resident dies, so
    // facts that close the row do not strand work already paid for.
    let deliveryOpen = courierProgrammeOpen view atlas

    // The load this tick's draw takes (#378): a whole `Tuning.ReactorLoad`
    // while the mine still feeds the bank and the remainder once it does not.
    // Read once and used by all three rules below — the amount the draw
    // admits, the amount that marks a delivery in flight and the room the
    // Reactor must have for it are one number.
    let loadNow = deliveryLoad view atlas

    let stillComing = oreStillComing view atlas

    let deliveryInFlight =
        view.Creeps |> List.exists (carryingADelivery view loadNow stillComing)

    // The draw is gated on the Reactor having room for the whole load (#354),
    // which is what meters supply against a store that burns 1 T a tick. A
    // load drawn against a full Reactor cannot be put down on arrival, and a
    // courier holding Thorium it cannot transfer stands on its own hot tile
    // burning three ticks of life a tick until it dies of it — 915 T reached
    // the Reactor room's floor that way. Held work is untouched: the sink
    // below keeps a load already drawn.
    let deliveryWithdraws =
        if deliveryOpen && reactorTakesALoad view loadNow then
            storages
            |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id >= loadNow)
            |> List.map (fun id -> Withdraw(id, Thorium))
        else
            []

    // Not while a rival's claimer stands beside it (#406), held work
    // included: the load would burn for the flag it takes next tick.
    let deliverable = errandRoomsDeliverable view

    let reactorRefills =
        view.Errands
        |> List.filter (fun errand -> Set.contains errand.RoomName deliverable)
        |> List.choose (fun errand ->
            let reactorId = fst errand.Target
            let task = Refill(reactorId, Thorium)
            let owner = Map.tryFind reactorId view.Spatial.Owners
            let stored = SpatialInfo.heldIn view.Spatial Thorium reactorId

            // Ore already lying in the Reactor's own room opens it too (#378):
            // a pile or a tombstone out there is score at the far end of the
            // delivery's walk, and without this clause the only sink the body
            // that picks it up had was the Storage three crossings back.
            if
                (deliveryOpen
                 || deliveryInFlight
                 || oreBesideTheReactor view
                 || Set.contains (taskId task) held.WithThorium)
                && (owner = Some Ownership.Ours || owner = Some Ownership.Unowned)
                && stored < Engine.reactorCapacity
            then
                Some task
            else
                None)

    // The consignment (#349): the two Tasks that put a declaring colony's
    // banked ore into its own terminal, and the one that takes an arriving
    // consignment out of it. What actually crosses the map is
    // `planConsignment` in `Layout` — a structure intent, not a body. Three
    // rooms of `send` replace five and six crossings of walk: W12S28 and
    // W13S28 sit outside `Tuning.MaxHops` of the Reactor, so no courier row of
    // theirs can ever be opened.
    let terminals = idsOfKind (Structure BuiltKind.Terminal)

    let terminalRoom id =
        Engine.terminalCapacity - stored id - SpatialInfo.heldIn view.Spatial Thorium id

    // Outbound, and only for a colony that declares a consignee: the ore goes
    // from the Storage into the terminal. Gated on room in the terminal and on
    // nothing else — not on the Storage's level, because there is no fixed
    // load here and no walk to strand a carrier on: the terminal stands feet
    // from the Storage, and a body that fills up half way puts down what it
    // has.
    let consignWithdraws =
        match view.Consignee with
        | None -> []
        | Some _ ->
            if terminals |> List.exists (fun id -> terminalRoom id > 0) then
                storages
                |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)
                |> List.map (fun id -> Withdraw(id, Thorium))
            else
                []

    // Its sink, and the fee's intake beside it: `send` is paid out of the
    // sending terminal's own energy. Both are gated on the declaration. An
    // ungated ore sink is a second sink in the same room the receiving end
    // draws ore *out* of, so the two rules fed each other and a delivery load
    // drawn for the Reactor was about to go back into the terminal beside it;
    // only a colony that ships offers its terminal as a sink. The energy
    // clause for its own reason: only a sender pays a fee, and a terminal
    // stocked for a send it never makes holds `Tuning.TerminalEnergy` forever.
    let consignRefills =
        match view.Consignee with
        | None -> []
        | Some _ ->
            terminals
            |> List.collect (fun id ->
                [
                    if SpatialInfo.heldIn view.Spatial Thorium id < Engine.terminalCapacity then
                        Refill(id, Thorium)

                    if stored id < view.Tuning.TerminalEnergy then
                        Refill(id, Energy)
                ])

    // Inbound, at the far end: ore that arrived by `send` sits in the terminal,
    // and the courier's draw reads the *Storage*, so it has to be walked across
    // the room. The sink is `mineRefills` above, which knows nothing about
    // where a load came from. Gated on the colony declaring no consignee of its
    // own: a room that both ships out and draws in would cycle its ore between
    // two stores forever.
    let arrivalWithdraws =
        match view.Consignee with
        | Some _ -> []
        | None ->
            terminals
            |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)
            |> List.map (fun id -> Withdraw(id, Thorium))

    // The ferry's other half: a bootstrapping child's upgrade buffer is a
    // Refill target of the mother's, on the same tier her own buffer sits on,
    // so the load crosses the Seam only when there is nowhere nearer to put it.
    let ferryRefills =
        ferrySinks
        |> Set.toList
        |> List.filter (fun id -> stored id < Engine.containerCapacity)
        |> List.map (fun id -> Refill(id, Energy))

    // The stock's other half (ADR-0023): a stocked Storage is a Withdraw source
    // only while the pool holds a Refill whose target is not the stock itself.
    // Its own Refill is deliberately no such sink: counting it would gate the
    // Storage open against itself. Both halves can still be pooled on one
    // tick, and there the tier gap carries the load away.
    let storageWithdraws =
        if
            List.isEmpty refills
            && List.isEmpty containerRefills
            && List.isEmpty ferryRefills
        then
            []
        else
            storages
            |> List.filter (fun id -> stored id > 0)
            |> List.map (fun id -> Withdraw(id, Energy))

    flees
    @ guards
    @ harvests
    // The consignment's rungs sit here, above the energy cycle and below the
    // season's own: ore is the season's score and the hauling it asks for is a
    // few tiles of the home room (#349).
    @ consignWithdraws
    @ arrivalWithdraws
    @ consignRefills
    // Behind the sources' own, which is pool order and so the last rung of the
    // Matcher's ladder: the two never tie for a body anyway, the deposit's
    // Harvest reaching only a body with no Carry at all.
    @ deposits
    // The piles stand before the Withdraws (#242). Pool order is the last rung
    // of the Matcher's ladder — what it falls back to once priority, travel
    // cost and the crowding load have all three tied (`MatchFactor.PoolOrder`)
    // — and of two intakes tied that way the decaying one is the one to take:
    // a pile loses `ceil(amount / 1000)` a tick and a container loses nothing.
    @ pickups
    @ withdraws
    @ refills
    @ builds
    @ repairs
    @ upgrades
    @ reserves
    @ claims
    @ reclaims
    @ containerRefills
    @ ferryRefills
    @ storageRefills
    @ storageWithdraws
    // Last, which costs the pair nothing: pool order reaches only an exact
    // tie, and the one live pairing — an empty body choosing between the
    // mineral container's Thorium and the Storage's energy — is decided by
    // priority since #306. The piles stand before the Withdraws for the reason
    // they do above.
    @ minePickups
    // Beside the piles and for their reason (#359): of the ore this colony can
    // name, the copy in a tombstone is the one on the shortest clock.
    @ tombstoneOre
    @ mineWithdraws
    @ mineRefills
    @ deliveryWithdraws
    @ reactorRefills
