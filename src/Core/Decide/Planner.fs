/// The Planner: the tick's Task list. The source-container and claim geometry it
/// rests on, which rooms are nursery, bootstrap or outpost, and `planTasks`
/// itself — what there is to do, before anyone is matched to it.
[<AutoOpen>]
module Fabot.Core.Decide.Planner

open Fabot.Core
open Fabot.Core.Types

/// The source container geometry (ADR 0012): a tile within range 1 of the given
/// source is that source's container tile — the Seat-standing kind the Layout
/// places, which harvest overflow fills. The one range this colony calls a
/// source container, asked of one source. Since ADR 0057 the Layout asks it of
/// a **Thorium deposit** too: served is a container standing or pending within
/// range 1, and that is one rule over two kinds of rock. Named for the source
/// because every other caller here is a source's, and a deposit's own readers
/// say which rock they meant (`servingRock`).
let internal servesSource (rockPos: Pos) (tile: Pos) = range tile rockPos <= 1

/// The source a tile of the named room is a container's for: the placed source
/// **standing in that same room** within range 1 of it, or None where there is
/// none. The one geometry judgement behind both rules that care about a source
/// container — the Planner keeps them out of Refill, the hauler quota counts
/// them. Unplaced geometry classifies nothing (ADR 0004). The source's identity
/// and not merely its existence, because since ADR 0042 the hauler quota prices
/// a container at *that* source's own output: a room's reservation decides
/// whether the rock under a container is worth ten a tick or five. Of several
/// sources within range 1 the first in view order answers.
let internal sourceContainerServes (view: ColonyView) (room: string) (pos: Pos) : string option =
    view.Sources
    |> List.tryFind (fun s ->
        match SpatialInfo.placementOf view.Spatial s.Id with
        | Some source -> source.Room = room && servesSource (RoomPos.pos source) pos
        | None -> false)
    |> Option.map (fun s -> s.Id)

/// Whether a tile of the named room is a source container's at all — the
/// half of the rule above that the Refill pool asks, which needs to know
/// that the tile is spoken for and never which rock spoke for it.
let private isSourceContainerTile (view: ColonyView) (room: string) (pos: Pos) =
    sourceContainerServes view room pos |> Option.isSome

/// Whether this colony owns the named room. Read by the rules that mean *ours*
/// — a [[nursery]] of this colony's, a child it is bootstrapping — where whose
/// room it is decides whose business it is (ADR 0047 decision 4). A room with no
/// control entry is one the colony cannot see, and an unseen room is not one it
/// owns (ADR 0004).
let private colonyOwns (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// Whether the named room carries an **owner at all** — one spelling for the
/// two rules of the reserver's that read it. `reserveController` answers
/// ERR_INVALID_TARGET on a controller with an owner, *whoever* it is, so the
/// Reserve pool must not carry its controller and the row must not hire against
/// it: one sentence rather than two gates free to disagree. A rival's counts
/// too, because ADR 0043's stand-down reads the *previous* tick's raid log and
/// the one tick between first sight and the withdrawal cast the bank's whole
/// reserver body at a room the engine refuses.
let private roomHasOwner (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner <> Ownership.Unowned)

/// The controllers a Claim is pooled for this tick, each with the room it
/// stands in (ADR 0047) — one spelling for the two rules that read it: the Task
/// pool offers exactly these and the reserver row hires one body for each, so
/// the row and its Task cannot disagree (ADR 0006). A **candidate colony** is a
/// declared home this colony does not own yet, and both halves are needed: the
/// declaration, because claiming a room is a human's decision and no projected
/// fact distinguishes a room we mean to own from a neighbour we merely mine;
/// and the ownership, because the tick the claim lands the room stops being a
/// candidate and this pool empties itself, with no state kept. A room nothing
/// looked into is not one it can claim (ADR 0004).
let internal claimTargets (view: ColonyView) : (string * string) list =
    let takeable room =
        match Map.tryFind room view.RoomControl with
        | Some control ->
            control.Owner = Ownership.Unowned
            && control.Reservation
               |> Option.forall (fun held -> held.Holder = ReservationHolder.Ours)
        | None -> false

    let candidate room =
        List.contains room view.Declared && takeable room

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.choose (fun id ->
        match SpatialInfo.placementOf view.Spatial id with
        | Some tile when candidate tile.Room -> Some(id, tile.Room)
        | _ -> None)

/// Whether the named room is this colony's **nursery**: a declared colony of
/// ours that has been claimed and has no spawn of its own yet (ADR 0047
/// decision 4). Its home goes on being projected as this colony's [[outpost]],
/// and three rules read that state — every site in it is feeding-tier work, the
/// concurrent-builder budget does not reach those sites, and the worker row
/// hires `Tuning.PioneerCount` more bodies — so it is one spelling and not
/// three gates free to disagree. Two facts: the room's **stage** is `Nursery`
/// (ADR 0052 decision 3), which carries the declaration, the ownership and the
/// missing spawn in one derivation, and **this colony projects the room**
/// (`colonyOwns`), which is what keeps a second declarer from hiring
/// [[pioneer]]s for a child it never projects.
let internal isNurseryRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && roomStage view room = Some Nursery

/// Whether the named room is a child colony this one is still **bootstrapping**
/// (ADR 0047 decision 4): a declared colony of ours that stands its own spawn —
/// so it runs its own `decide` and is nobody's [[nursery]] any more — and that
/// this colony is nonetheless projecting. Two rules read it: the child's
/// controller joins this colony's Upgrade pool, and the worker row keeps hiring
/// `Tuning.PioneerCount` bodies. `isNurseryRoom`'s two facts with the stage
/// inverted, so the two are complements over one room. **Both standing
/// stages**, because what closes the borrowing is the scan set and never a
/// level read here (ADR 0047's Consequences): while a human still declares the
/// child's room as one of this colony's `Outposts` the room is in the scan set
/// through the outpost reading, which asks no stage.
let internal isBootstrapRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && (match roomStage view room with
        | Some Bootstrapping
        | Some Independent -> true
        | Some Nursery
        | None -> false)

/// Whether an Upgrade in this pool is **borrowed**: its controller is not this
/// colony's own, so it is a bootstrapped child's, pooled by `planTasks` for the
/// pioneers (ADR 0047 decision 4).
let internal isBorrowedUpgrade (view: ColonyView) controllerId =
    view.Controller |> Option.exists (fun c -> c.Id = controllerId) |> not

/// The [[ferry]]'s sinks (ADR 0052 decision 7): the upgrade buffers of the
/// children this colony is **bootstrapping**. Three readers must name the same
/// store or the colony hires a body for a Task nobody pooled — the hauler
/// quota's ferry term, `planTasks`' Refill, and `planPool`'s bound. Read off
/// what the view carries and not off the geometry again: the one store of a
/// child's a mother's view holds is this one (`ColonyView.ferrySink`), and the
/// join cannot be respelled here because it subtracts the tiles beside the
/// child's **sources**, which the same narrowing drops. Total (ADR 0004).
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
        // A container and not merely a store: a buffer is one, and the
        // kind is the one half of the join that survives the narrowing, so
        // asking it costs nothing and keeps a tombstone or a pile lying in
        // that room out of a Refill it could never be filled through.
        |> List.filter (fun id ->
            Map.tryFind id view.Spatial.TargetKinds = Some(Structure BuiltKind.Container)
            && (SpatialInfo.roomOf view.Spatial id
                |> Option.exists (fun room -> Set.contains room rooms)))
        |> Set.ofList

/// The controllers this colony may hold a [[reserve]] on: every controller the
/// projection carries that is not this colony's home, stands in no room somebody
/// owns, and is no [[candidate colony]]'s — whose controller carries a Claim
/// rather than a Reserve (ADR 0047). The engine refuses `reserveController` on
/// an owned room, so an owned room's controller is one this colony is
/// withdrawing from rather than mining.
///
/// **Ids and not rooms**, because a controller the projection does not place
/// names no room and is still ours to reserve (ADR 0004) — it is the Reserve
/// pool that reads it that way, while `declaredOutposts` takes the rooms and
/// loses the unplaced one. One scan for both, or the rows and the pool could
/// disagree about which rooms are ours to work.
let private reservableControllers (view: ColonyView) : string list =
    let home = view.Controller |> Option.map (fun c -> c.Id)
    let claimed = claimTargets view |> List.map fst |> Set.ofList

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.filter (fun id ->
        Some id <> home
        && not (Set.contains id claimed)
        && not (SpatialInfo.roomOf view.Spatial id |> Option.exists (roomHasOwner view)))

/// The declared [[outpost]]s this colony works this tick — the rooms two rows
/// are hired per, written once because a paraphrase would let the reserver row
/// and the guard row disagree about which rooms are ours to work (ADR 0042, ADR
/// 0056). A room qualifies by carrying **a controller of its own in the
/// projection** that is not this colony's home: an outpost's declaration names
/// its controller and a room with none is no candidate outpost at all. It must
/// be unowned — a room somebody holds is one this colony is withdrawing from,
/// not mining — and it must not be a [[candidate colony]]'s, whose controller
/// carries a Claim rather than a Reserve.
///
/// **Declared and not posted**, which is where #131's correction overrides ADR
/// 0042's "one reserver per posted outpost" clause: gating on a standing
/// container deadlocks the outpost chain, since the container needs vision,
/// vision needs a creep, and the reserver is the only creep with a reason to
/// go. The scan set is the gate that remains, and two things narrow it, both
/// inside `World.scanOf`: ADR 0043's stand-down, and the declaration's own
/// geometry — its `Outpost.withinHopBudget` filter (#243, ADR 0058) — a room further
/// from its home than `Tuning.MaxHops` crossings is joined by no chain of
/// [[seam]]s, so a body hired for it could never walk there, and the room is
/// out of the scan set before anything here counts it. What *says* so is `Outpost.refused` on the [[layout record]]; what
/// narrows the set is the filter.
///
/// Off the projection itself (`SpatialInfo.placementOf`) and not off the
/// [[atlas]]'s join of it, which answers alike: the [[guard]]'s own Task is
/// pooled by `planTasks`, and the Planner's first half is handed the view and
/// no Atlas (ADR 0056 decision 2). One derivation for the two rows and the
/// pool, or the three of them could disagree about which rooms are ours.
///
/// The derivation itself is `reservableControllers`: the two rows want its
/// rooms and the Reserve pool wants its ids, and that difference is all the
/// difference there is between them.
let internal declaredOutposts (view: ColonyView) : string list =
    reservableControllers view
    |> List.choose (SpatialInfo.roomOf view.Spatial)
    |> List.distinct

/// The declared [[outpost]]s a [[threat]] stands in this tick (ADR 0056): the
/// rooms the guard row hires a body for, and the rooms `planTasks` pools a
/// Guard in. One derivation for both halves, which is decision 2's "one number
/// computed once and read as both the row's quota and the Task's cap" stated
/// for the rooms that number is summed over: pooled where nothing is hired, the
/// Guard would hold a body a raided room needs elsewhere; hired where nothing
/// is pooled, the cast body would stand idle in the oven's shadow.
///
/// ADR 0033's own Threat test and never "a hostile": a scout or a healer alone
/// reaches nothing, takes no ground and is no reason to buy a body. Vision is
/// the whole of what it reads (ADR 0004) — an outpost the colony cannot see
/// carries no hostiles and asks for no guard, the same zero a quiet room
/// contributes.
let internal guardedOutposts (view: ColonyView) : string list =
    if List.isEmpty view.Hostiles then
        []
    else
        declaredOutposts view
        |> List.filter (fun room ->
            view.Hostiles
            |> List.exists (fun h -> h.Pos.Room = room && (weaponRange h |> Option.isSome)))

/// Planner: rebuild this tick's full Task pool from the colony view. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (view: ColonyView) (threats: Threats) : Task list =
    // Flee exists while a Reach does (ADR 0033): one Task for the whole
    // colony, at the head of the pool as its Safety tier is at the head of
    // the ranking. No Reach, no Flee — a quiet tick's pool is the pool it
    // always was.
    let flees = if Map.isEmpty threats.Reach then [] else [ Flee ]

    // One Guard per declared [[outpost]] a [[threat]] stands in (ADR 0056),
    // keyed on the room: the same list the guard row hires against, so the body
    // the cascade buys has a Task waiting for it and no Task waits for a body
    // nobody bought. Beside Flee at the head of the pool — the two Tasks of the
    // Safety tier, disjoint by [[body class]], so nothing ever asks how they
    // order.
    let guards = guardedOutposts view |> List.map Guard

    // Harvest exists for every source, drained or not (ADR 0013, revised by
    // ADR 0025): the task no longer flickers with the source's stock, because
    // whether a dry rock is worth walking to depends on the walker's body and
    // position — the Matcher's knowledge, not the creep-blind Planner's.
    let harvests = view.Sources |> List.map (fun s -> Harvest s.Id)

    // And one Harvest per Thorium deposit beside them (ADR 0057 decision 2):
    // the same act, the same Intent and the same Task kind, widened from a
    // source to a mineral — which is ADR 0054's own test for refusing a new
    // kind, so every exhaustive match over `Task` grows no arm. **The Task
    // exists exactly while the deposit does**, and that is the whole of its
    // lifecycle: the mod deletes an exhausted deposit outright, so the target
    // leaves the projection and this list shortens on the same tick the miner
    // row's quota falls to zero. What the extractor's cooldown decides is not
    // whether the Task exists but whether this tick's act is issued, and that
    // gate is the Emitter's — a Task that vanished and returned every sixth
    // tick would churn the pool for a body that has nowhere else to be.
    //
    // **A deposit of ours and never a neighbour's** (#261): `FIND_MINERALS`
    // carries every owner's and a scanned neighbour arrives with its own
    // deposit, extractor and container, which the projection cannot tell from
    // ours by their shape. `harvest` refuses a mineral whose extractor is
    // somebody else's, so a Task pooled for one is a body dispatched across the
    // map to answer `ERR_NOT_OWNER` once a tick for a life. `ourDeposits` is
    // that join, read here and by the [[miner]] row's quota alike.
    let deposits = ourDeposits view |> List.map Harvest

    // The flow's sink, as **one** Task (ADR 0054): the [[refill cluster]] — the
    // colony's spawn and every extension of it — is pooled under the spawn's id
    // and stands while any member has room, so `task-gone` fires when the whole
    // ring is full instead of once per extension somebody else got to first.
    let cluster = RefillCluster.ofRefillables view.Refillables

    let clustered =
        cluster
        |> Option.map (fun c -> c.Members |> Map.toList |> List.map fst |> Set.ofList)
        |> Option.defaultValue Set.empty

    let refills =
        (cluster
         |> Option.filter (fun c -> RefillCluster.free c > 0)
         |> Option.map (fun c -> Refill c.Spawn)
         |> Option.toList)
        @ (view.Refillables
           |> List.filter (fun r -> r.FreeCapacity > 0 && not (Set.contains r.Id clustered))
           |> List.map (fun r -> Refill r.Id))

    let builds = view.ConstructionSites |> List.map (fun site -> Build site.Id)

    // A Repair per repairable structure below its kind's whole line, in id
    // order (ADR 0010, ADR 0034).
    let repairs = hungryStructures view |> List.map (fst >> Repair)

    // The ids of one projected kind, in id order. The containers, the
    // Storage and the controllers are all pooled by the projection's kind
    // — never by position, never by name — so the rule is written once.
    let idsOfKind kind = SpatialInfo.idsOfKind view.Spatial kind

    // The colony's own controller, and the controller of every child it is
    // still bootstrapping (ADR 0047 decision 4) — half of the one cross-colony
    // borrowing rule there is: a loaded worker of the mother's may cross the
    // Seam and spend into the child's controller until that controller reaches
    // `Tuning.BootstrapLevel`. Surplus tier, like the home Upgrade it stands
    // beside, so the mother's own flow is fed first and nothing but travel cost
    // separates the two — which is what leaves the child's to the bodies
    // already standing in its room.
    let upgrades =
        let own = view.Controller |> Option.toList |> List.map (fun c -> c.Id)

        let children =
            idsOfKind Controller
            |> List.filter (fun id ->
                SpatialInfo.roomOf view.Spatial id |> Option.exists (isBootstrapRoom view))

        own @ children |> List.map Upgrade

    // One Claim per candidate colony's controller (ADR 0047), read off
    // the one rule that says which those are (`claimTargets`).
    let claims = claimTargets view |> List.map (fst >> Claim)

    // One Reserve per projected controller that is not the colony's own (ADR
    // 0042): a neutral controller held by CLAIM parts pays its room's sources
    // ten a tick instead of five, and the hold decays by one a tick, so the
    // Task stands whatever the reservation has left on it — the ticks remaining
    // size the body, not the pool. Read off the projection's kind census and
    // never off the declared outposts (ADR 0041), so a room a stand-down keeps
    // out of the scan set (ADR 0043) leaves this pool with it rather than
    // through a second gate free to disagree. The colony's own controller is
    // excluded by id, and every controller in a room that carries an owner by
    // the same `roomHasOwner` the reserver row drops the room with, since the
    // engine refuses reserveController on any owned room. A controller the
    // projection does not place names no room and stays pooled (ADR 0004).
    let reserves = reservableControllers view |> List.map Reserve

    // The haul cycle's intake (ADR 0012), shaped over the projection's
    // stores rather than energy's name: every stocked container yields a
    // Withdraw, at feeding tier beside Harvest — whether to dig or to
    // collect is travel cost's call, never a rule's.
    let stored id = SpatialInfo.storedIn view.Spatial id

    let containers = idsOfKind (Structure BuiltKind.Container)
    let storages = idsOfKind (Structure BuiltKind.Storage)

    // A tombstone and a ruin are stores the same way, so they pool through the
    // same line: a store with energy in it yields a Withdraw, and what will
    // become of the thing holding it is not this pool's question. The engine's
    // `withdraw` takes either object, and the cap and the tier are read off the
    // stock and the kind exactly as a container's are.
    let tombstones = idsOfKind Tombstone

    // The [[ferry]]'s sink is the one store in this pool a body of this
    // colony's may fill and may never draw: it is a child's, and the whole of
    // the lend is energy going one way. Left in, the mother's hauler would take
    // the load she just carried across the Seam straight back out — the ADR 0019
    // cycle over a border.
    let ferrySinks = ferryBuffers view

    // **No store of a child's is hers to draw, at any [[stage]]** (ADR 0047
    // decision 1): what one colony may take of another is the explicit list
    // `BorrowedWork` carries, and no store is on it.
    let borrowedRooms = Set.ofList view.Borrowed.Rooms

    let inABorrowedRoom id =
        SpatialInfo.roomOf view.Spatial id
        |> Option.exists (fun room -> Set.contains room borrowedRooms)

    let withdraws =
        containers @ tombstones
        |> List.filter (fun id -> stored id > 0 && not (inABorrowedRoom id))
        |> List.map Withdraw

    // The piles worth walking to: a dropped pile at or over
    // `Tuning.PickupThreshold` is a Feeding-tier Task, and every smaller one is
    // left to the reflex that costs nothing. The amount and nothing else:
    // whether the pile is at somebody's feet already is a fact about a creep,
    // and the Planner is creep-blind by construction (ADR 0013).
    let pickups =
        idsOfKind Dropped
        |> List.filter (fun id -> stored id >= view.Tuning.PickupThreshold)
        |> List.map Pickup

    // The haul cycle's outflow: the controller container is one more Refill
    // target (ADR 0010's target layering, widened by ADR 0012). Which container
    // is the controller's is judged by geometry — it stands inside the Upgrade
    // Work Area the Layout picked it from, while a source container's tile is
    // never a Refill target.
    let containerRefills =
        view.Controller
        |> Option.bind (fun c -> SpatialInfo.placementOf view.Spatial c.Id)
        |> Option.map (fun controller ->
            let controllerRoom = controller.Room
            let controllerPos = RoomPos.pos controller
            let placed = (SpatialInfo.layerOf view.Spatial controllerRoom).TargetPositions

            containers
            |> List.filter (fun id ->
                match Map.tryFind id placed with
                | Some pos ->
                    range pos controllerPos <= 3
                    && not (isSourceContainerTile view controllerRoom pos)
                    && stored id < Engine.containerCapacity
                | None -> false)
            |> List.map Refill)
        |> Option.defaultValue []

    // The colony's stock is the outflow's last stop (ADR 0023): a standing
    // Storage with room is one more Refill target, on the deepest tier of all.
    let storageRefills =
        storages
        |> List.filter (fun id -> stored id < Engine.storageCapacity)
        |> List.map Refill

    // The [[ferry]]'s other half (ADR 0052 decision 7): a bootstrapping child's
    // upgrade buffer is a Refill target of the mother's, on the same tier her
    // own buffer sits on (ADR 0012) — the deepest but the stock's, so nothing
    // she feeds at home waits on it and the load crosses the Seam only when
    // there is nowhere nearer to put it.
    let ferryRefills =
        ferrySinks
        |> Set.toList
        |> List.filter (fun id -> stored id < Engine.containerCapacity)
        |> List.map Refill

    // The stock's other half (ADR 0023): a stocked Storage is a Withdraw source
    // too, but only while the pool holds a Refill whose target is not the stock
    // itself. Its own Refill is deliberately no such sink: counting it would
    // gate the Storage open against itself, and a hauler beside a store that is
    // both its only intake and its only sink cycles energy in and out of it
    // tick after tick. Both halves can still be pooled on one tick, and there
    // the tier gap carries the load away instead of putting it back.
    let storageWithdraws =
        if
            List.isEmpty refills
            && List.isEmpty containerRefills
            && List.isEmpty ferryRefills
        then
            []
        else
            storages |> List.filter (fun id -> stored id > 0) |> List.map Withdraw

    flees
    @ guards
    @ harvests
    // Behind the sources' own, which is pool order and so the last rung of the
    // Matcher's ladder: the two never tie for a body anyway, the deposit's
    // Harvest reaching only a body with no Carry at all.
    @ deposits
    // **The piles stand before the Withdraws** (#242). Pool order is the last
    // rung of the Matcher's ladder — what it falls back to once [[priority]],
    // travel cost *and* the crowding load have all three tied, the scored key
    // being `(rank, cost, load)` (`MatchFactor.PoolOrder`) — and of two intakes
    // a body could take at the same rank, for the same walk, with the same
    // crowd already on them, the decaying one is the one to take: a pile loses
    // `ceil(amount / 1000)` a tick and a container loses nothing. The order
    // reaches exact ties and nothing else, so it moves no pair the ladder or
    // the flood had already separated.
    @ pickups
    @ withdraws
    @ refills
    @ builds
    @ repairs
    @ upgrades
    @ reserves
    @ claims
    @ containerRefills
    @ ferryRefills
    @ storageRefills
    @ storageWithdraws
