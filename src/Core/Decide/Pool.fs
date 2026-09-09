/// The Pool: every Task priced and tiered for the tick, the one place a Task
/// kind is turned into a number. The restock, garrison and spare-rate rules
/// behind a Post (ADRs 0020, 0024, 0025), the safety tiers, and `planPool`.
[<AutoOpen>]
module Fabot.Core.Decide.Pool

open Fabot.Core
open Fabot.Core.Types

/// Ticks until a source restocks (ADR 0025), 0 while it holds energy —
/// and 0 for a source the view does not carry at all, so a source
/// nothing projects never holds a decision up.
let internal ticksToRestock (view: ColonyView) sourceId =
    view.Sources
    |> List.tryFind (fun s -> s.Id = sourceId)
    |> Option.map (fun s -> s.TicksToRestock)
    |> Option.defaultValue 0

/// Whether a creep garrisons a source's container Post: ADR 0024's condition —
/// a Work-heavy body standing on that source's built container.
let internal garrisons atlas (creep: CreepInfo) sourceId =
    Atlas.workHeavy atlas creep.Name
    && Atlas.catchesOverflow atlas creep.Name sourceId

/// Whether a source's **rate** still outruns what the bodies garrisoning it
/// take — **whether a rock has a dig left in it worth a walk** (#235). A Post's
/// Anchor is sized to saturate its rock (ADR 0021: the Work that drain the
/// whole regeneration, plus one spare), so a manned Post ordinarily leaves
/// nothing over: a light body joining it takes energy the garrison would have
/// taken anyway, the colony earns not one point for the trip, and the seats it
/// fills are seats the garrison itself competes for (ADR 0051's cap is over the
/// source, and live a mother's two workers took the last two of an outpost's
/// three and its own Anchor read `none-free`). Live at t199,88x a worker with
/// nine free walked a Seam for one dig on a rock a six-Work Anchor was already
/// draining.
///
/// Named for the **rate** because that is the number it reads, and the file
/// spells the two apart on purpose (ADR 0042, #208): `sourceOutputOf` is the
/// quota's number — the row's *cast* capped at that rate — and reading it here
/// would have a colony too poor to cast a saturating Anchor read its own
/// half-worked rock as spent and keep its workers off the half nobody is
/// digging. The garrison side is the **living** bodies, for the same reason
/// from the other end: this gate prices one walk this tick, where a quota read
/// off a cast would go on pricing a body that has died. Zero garrison on an
/// unposted rock and on a vacant Post, so both stay open — the safety valve
/// that keeps a colony whose Anchor has just died from starving. A rate the
/// projection cannot price is no evidence of saturation (ADR 0004).
///
/// The **standing** Posts and not `Atlas.postsOf`, which is `Decide.isPosted`'s
/// own query for this same question (ADR 0042 as #205 amended it): a container
/// site is a garrison place and not yet an economy, so the twelve a tick its
/// Anchor digs goes into construction progress and reaches no store — and there
/// is no container standing beside the rock to Withdraw from either, so a rock
/// closed at the site stage leaves the whole light row with no Feeding intake
/// at all while both home Posts are still being raised. The clause starts
/// biting the tick the container stands, which is the tick the rock joins the
/// economy the clause is rationing.
///
/// Read here in `applicable` and not as a `Capacity.Commuters` of zero, where
/// ADR 0052 decision 6 otherwise keeps the per-source numbers: a capacity
/// bounds the crowd a Task admits but never evicts a body already holding it,
/// and #235's case (b) is exactly an eviction — the outpost Anchor stands up
/// and the squatting light body has to be released that tick, not merely
/// refused the next time it asks.
let internal hasSpareRate (view: ColonyView) atlas (sourceId: string) =
    let posts = Atlas.standingPostsOf atlas sourceId

    let dug =
        if Set.isEmpty posts then
            0
        else
            // Read off the bodies *standing* on the Posts, the way `garrisons`
            // above reads one — a Post's garrison is a fact about where a body
            // is (ADR 0024) — and off the heavy ones alone, because ADR 0051
            // keeps the light bodies' Work Area off these tiles: one standing
            // there is squatting the Post, not working it. A body standing on
            // one of these tiles digs *some* source, and where two rocks share
            // a Seat it is charged to both — the ambiguity `Atlas.postsOf` has
            // carried since ADR 0024's cap, not one this gate introduces.
            view.Creeps
            |> List.sumBy (fun creep ->
                if
                    Atlas.workHeavy atlas creep.Name
                    && Atlas.creepTile atlas creep.Name
                       |> Option.exists (fun tile -> Set.contains tile posts)
                then
                    (creep.Body |> Map.tryFind Work |> Option.defaultValue 0)
                    * Engine.harvestPerWork
                else
                    0)

    sourceRateOf view atlas sourceId |> Option.forall (fun rate -> rate > dug)

/// Whether a source has a [[post]] with no garrison standing on it — **whether
/// the walk this body is about to make ends on a tile it can have** (#258). A
/// **heavy** body alone mans a Post, because ADR 0051 keeps every other body
/// off those tiles and one standing there is squatting the Post rather than
/// working it — `hasSpareRate`'s reading of the bodies, and the candidate never
/// counts against itself, the way the Matcher's own garrison count does not.
///
/// **Every** Post of the rock and not the standing ones alone, which is the
/// census the clause in `applicable` that reads this already asks: the question
/// here is standing room, and a Post whose container is still a site is a tile
/// a body stands on and raises (#205). `hasSpareRate` reads the standing census
/// instead because its question is the rock's *economy*, and a site is a
/// garrison place and not yet an economy. Read here, the standing census would
/// strand the body the walk-home clause exists for: a rock carrying a manned
/// container Post and a site Post beside it would read occupied for a full body
/// one step off that site, and Build asks for the exact tile (#205, #234) — so
/// that body would hold no Task at all.
///
/// Read **now** and not at arrival, which is where this parts from every other
/// count of a Post. ADR 0026 discounts a holder that will be dead when the
/// candidate gets there — the Matcher's cap and ADR 0053's `emptyPostCaps` both
/// take that reading, and say so — so a Post ninety ticks away reads free to
/// any heavy body in the colony, and live at 204,966 an Anchor a border away
/// that had just lost its own rock took the walk home on it and stood beside a
/// garrison that outlived its arrival by hundreds of ticks (user, 2026-09-08).
/// The discount is safe for the body a cast was aimed at, and this gate cannot
/// tell one of those from a released squatter, so it is refused here: is there
/// standing room, this tick, on the rock this walk ends at. The price is a full
/// body whose incumbent is genuinely [[expiring]], which waits where it stands
/// rather than timing its walk; the alternative is the live case above. A rock
/// with no Post at all stays open, the same safety valve `hasSpareRate` keeps —
/// the clause below never asks with one, having settled it a conjunct earlier.
let internal hasUnmannedPost (view: ColonyView) atlas (creep: CreepInfo) (sourceId: string) =
    let posts = Atlas.postsOf atlas sourceId

    if Set.isEmpty posts then
        true
    else
        let manned =
            view.Creeps
            |> List.choose (fun c ->
                if c.Name <> creep.Name && Atlas.workHeavy atlas c.Name then
                    Atlas.creepTile atlas c.Name
                else
                    None)
            |> Set.ofList

        posts |> Set.exists (fun tile -> not (Set.contains tile manned))

/// Whether a Work-heavy body holds a source through its empty window: the
/// **empty-source** reprieve, which ADR 0048 widened off the container to the
/// source's whole digging range. Named for the reprieve and not for the Post on
/// purpose: the second option ADR 0048 rejected by name was widening this to
/// the source's Posts, and the tile the Anchor is bumped onto is not one. ADR
/// 0025 wrote the two reprieves as one judgement about one tile, and the room
/// proved them different questions — a hauler drawing the container swaps the
/// Anchor onto the Seat beside it, and on the container-only condition that one
/// step released it TooEarly. Overflow is a fact about the tile underfoot;
/// being in position to dig is a fact about the range.
let private keepsThroughEmptyWindow atlas (creep: CreepInfo) sourceId =
    garrisons atlas creep sourceId
    || (Atlas.workHeavy atlas creep.Name
        && Atlas.standsAtSource atlas creep.Name sourceId
        && not (Atlas.standsOnDualSeat atlas creep.Name))

/// The walk and the wait that hold a Task up for this creep, or None when its
/// time has come (ADR 0025, repriced by ADR 0029): a drained source's Harvest
/// is applicable only when the creep's walk covers the restock wait — walk >=
/// ticks to restock, with no slack, because the wait shrinks by one each tick
/// while the walk stays put, so a creep one tick short departs one tick later
/// and arrives as the energy does. The walk is the Atlas's own query, already
/// whole ticks and blind to today's traffic, so a bystander in the lane cannot
/// dispatch a creep this tick and recall it the next. A creep already beside a
/// dry rock has no walk to cover anything and is released (ADR 0013). One
/// exemption, ADR 0024's condition as ADR 0048 widened it: a Work-heavy body
/// already in digging range keeps its Post through the window, a bare Dual Seat
/// subtracted. **One rule for both bodies** (#258, retiring ADR 0048's heavy
/// arm): the walk covers the wait or it does not, and how many ticks a tile
/// costs this body is already in the walk. ADR 0048 read the same walk as
/// earliness for a Work-heavy body whatever its length, because "the walk
/// covers the wait, so set out now" had dispatched an Anchor across half a room
/// onto a Post another Anchor was standing on — a **capacity** question, which
/// the Post count answers (ADR 0024, ADR 0051) and `applicable` below answers
/// again, this tick and not at arrival, for a *full* body still walking. The
/// window that left — a Post whose garrison holds some *other* Task this tick,
/// a bare [[dual seat]]'s upgrading through this same empty window, counted by
/// neither gate, so an empty heavy body far enough out was dispatched onto it —
/// is closed in the cap and not here (#269): the Post census the Heavy cap
/// counts is the bodies standing on the rock's Posts unioned with the Task's
/// own holders, so a manned Post never reads vacant. Here it stays one
/// question about the walk, because a heavy body with a free store must keep
/// the walk this gate would otherwise refuse it, or no successor could ever be
/// sent to the Post its expiring incumbent is standing on (ADR 0026) — and that
/// discount is the cap's alone to give, to an incumbent that will be **dead**
/// on arrival and never to one that will still be standing there. What the
/// heavy arm had left was the release it caused: an Anchor whose outpost rock
/// was dug out from under it mid-walk was released at ninety tiles of walk
/// against fifty ticks of wait, went `none-in-time`, and re-matched a home rock
/// it had no business on — twice, the vacancy it left behind casting a second
/// Anchor (user, 2026-09-08). Every other Task is judged at the current tick. Two
/// consequences, both ADR 0004's totality.
let internal tooEarly (view: ColonyView) atlas (creep: CreepInfo) task (walk: Lazy<int option>) =
    match task with
    | Harvest sourceId ->
        match walk.Value with
        // No walk at all is unreachable geometry, which is not earliness:
        // the reachability gate stands ahead of this one in both cascades
        // and names that rejection itself (ADR 0002, ADR 0029).
        | None -> None
        | Some ticks ->
            let wait = ticksToRestock view sourceId

            // A stocked source is a wait of zero, which every walk covers:
            // the arm below answers it either way, and asking it first
            // keeps the reprieve's Atlas joins off the pairs a stocked
            // pool is mostly made of.
            if wait = 0 || keepsThroughEmptyWindow atlas creep sourceId then
                None
            elif ticks < wait then
                Some(ticks, wait)
            else
                None
    | Withdraw _
    // A pile is workable the tick a creep reaches it and every tick before. It
    // moves — down by decay, up under an [[anchor]] spilling onto a full
    // [[container]] — but neither direction is a restock, so there is no tick
    // to be early *of*.
    | Pickup _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    // A controller is always there to be reserved: a reservation has no restock
    // and no stock, so a reserver that has walked to one is never early (ADR
    // 0042).
    | Reserve _
    | Claim _
    // A [[threat]] standing in a room is there to be hit the tick a guard
    // arrives and every tick before: a fight has no restock (ADR 0056).
    | Guard _
    | Flee -> None

/// Whether a Task stands in the **Safety** tier — the two Tasks the colony
/// ranks above every kind of work because a creep is being killed (ADR 0033,
/// ADR 0056 decision 3): [[flee]], which walks a body to the tiles nothing
/// reaches, and the [[guard]]'s own Task, which walks one to the tiles that
/// reach back. The tier's membership written as a predicate, because the two
/// rules that turn on it are asked long before `planPool` ranks anything and
/// neither has a `Tier` in hand: the Reach subtraction is skipped for the tier
/// (`areaFor`), and the cross-room threat reading is written beneath it
/// (`threatened`). ADR 0056 puts both against the *tier* and not against the
/// Task kind — Flee is already exempt in effect, its area being the safe set —
/// so the two rules are one sentence and cannot come to disagree, and
/// `planPool`'s own `tierOf` ranks exactly these two into `Safety`. Exhaustive
/// on purpose: a Task added to the union is a build error here, and answering
/// it wrongly is a body sent to a fight it is then refused.
let private safetyTier task =
    match task with
    | Flee
    | Guard _ -> true
    | Harvest _
    | Withdraw _
    | Pickup _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    | Reserve _
    | Claim _ -> false

/// The room a Task's Work Area lies in: its target's, since the area is that
/// target's surroundings and empty across a border (ADR 0020, ADR 0041) — so the
/// Reach taken out of it is that room's share. None for Flee, whose area is the
/// creep's own room's, and for a target the projection does not place. A Guard
/// names its room outright (ADR 0056) — the Planner keyed it on one — and the
/// case is the whole answer for a Task neither caller ever asks it about: both
/// `areaFor` and `threatened` settle the whole Safety tier on `safetyTier`
/// before they ask, no Reach being taken out of that tier's areas at all. The
/// case stands because the match is exhaustive and a Task it forgot would be a
/// build error.
let private roomOfWork atlas task =
    match task with
    | Harvest id
    | Withdraw id
    | Pickup id
    | Refill id
    | Build id
    | Repair id
    | Upgrade id
    | Reserve id
    | Claim id -> Atlas.targetRoom atlas id
    | Guard room -> Some room
    | Flee -> None

/// The Reach standing on a Task's own room, or None when the question does not
/// arise at all — the Safety tier, which `safetyTier` exempts whole (ADR 0056
/// decision 3), and a tick with no Reach anywhere. A Reach is one room's grid
/// (`Threats.Reach`), so the tiles it takes are matched on that room's
/// coordinates and on no other's (ADR 0052 decision 2, #138) — and the room is
/// the **Task's** and never the creep's, a body a border away being judged
/// against the ground it is walking to. The join is written here once because
/// its two readers make it in opposite polarity: `areaFor` thins an area by it
/// and `threatened` asks whether it has taken the area whole, and two copies of
/// a room-scoping rule is exactly how #138 came back.
let private reachOnWork (threats: Threats) atlas task =
    if safetyTier task || Map.isEmpty threats.Reach then
        None
    else
        let room = roomOfWork atlas task

        Some(room, room |> Option.map (Threats.reachIn threats) |> Option.defaultValue Set.empty)

/// The tiles a creep may work a Task from this tick (ADR 0033): its Work Area
/// less its room's Reach — and for Flee, the safe set of the room the creep
/// stands in, an area of the colony's own rather than some target's
/// surroundings. Each is the share of one room: a hostile a room away on the
/// same coordinate takes no tile here.
///
/// **The subtraction is skipped for the whole Safety tier** (ADR 0056 decision
/// 3, `safetyTier`): both of that tier's areas are derived off `Threats`
/// themselves rather than off a target's surroundings, so taking the Reach out
/// of them again is either a tautology (Flee's safe set is the Reach's
/// complement already) or the end of the Task (a Guard's ring is made of Reach
/// tiles). The exemption is the predicate and not the two kinds below it: the
/// ground each Task stands on is derived first, and `safetyTier` alone decides
/// whether the Reach is taken out of it — so a third Task ranked into Safety is
/// answered for once, in the one exhaustive match, and is exempt here without
/// this function being touched. Written against the tier and not against the
/// [[guard]] row, so the clause generalises ADR 0033 rather than carving out
/// one Task kind.
let internal areaFor (threats: Threats) atlas creep task : Set<RoomPos> =
    // The ground before any subtraction. The Safety tier's two areas are the
    // tick's own `Threats` and every other Task's is the [[atlas]]'s.
    let ground =
        match task with
        // Flee's ground: every walkable tile of the creep's own room that no
        // Threat reaches, which is the subtraction already made and filed by
        // the tick's colony-level derivation.
        | Flee ->
            Atlas.creepRoom atlas creep
            |> Option.map (Threats.safeIn threats)
            |> Option.defaultValue Set.empty
        // The [[guard]]'s ground, and the one area the Reach would *empty*
        // rather than thin (ADR 0056): it is made of Reach tiles, one ring
        // around every Threat in the room the Planner keyed the Task on, so the
        // subtraction would take all of it on every tick the Task exists. A
        // colony fact like the safe set beside it and no target's surroundings,
        // so the room is the Task's own and never the creep's — a body a border
        // away is offered the same ring, and it is the price of walking there
        // that decides whether it can have it.
        | Guard room -> Threats.ringIn threats room
        | _ -> Atlas.workAreaFor atlas creep task

    match reachOnWork threats atlas task with
    | None -> ground
    | Some(room, reach) ->
        ground
        |> Set.filter (fun tile ->
            not (Some tile.Room = room && Set.contains (RoomPos.pos tile) reach))

/// Whether a creep may act on a Task from the tile it is standing on this tick
/// — `Atlas.mayAct` over the ground `areaFor` has just thinned (ADR 0033).
/// The two are one question and are asked together at every site that asks
/// either, so they are joined here: written apart, each of the four callers
/// derived the Work Area a second time to put the pair back together.
let internal mayActNow (threats: Threats) atlas (creep: string) task =
    Atlas.mayAct atlas creep task (areaFor threats atlas creep task)

/// The travel cost of a Task for a creep, priced over the tiles it may actually
/// work from this tick (ADR 0033): the safe set for Flee, and every other
/// Task's own Work Area less the Reach — so the reachability gate judges the
/// tiles that are left rather than a tile the creep may not stand on, and a
/// candidate whose cold remainder is walled off is rejected as unreachable
/// instead of being held and never worked. The pricing itself is untouched:
/// same weights, same surcharge, same flood, only the goals are this tick's. An
/// area that is empty here was never taken by the Reach — the threat gate
/// stands ahead of this one in both cascades — so it falls back to the Task's
/// own price, which carries ADR 0004's escape for an unplaceable target and the
/// Seam join for a target in another room. The Work Area a creep is handed is
/// empty across a border by construction (ADR 0041), so an outpost's Task ranks
/// in the one pool through this fallback rather than a case of its own.
///
/// The Guard is priced off its area rather than off a target, for Flee's own
/// reason (ADR 0056): that area is the colony's `Threats` and not a target's
/// surroundings, so there is no unplaceable-target escape to fall back to and an
/// empty ring is honestly nowhere to stand. It crosses a border where Flee never
/// has to, though — the safe set is the creep's own room's, while a Guard's ring
/// is the room the Planner keyed the Task on, which is an outpost and never the
/// room the guard row cast the body in. So it prices through `travelCostToward`,
/// the same Seam-band minimum every cross-room Task is ranked by, taken toward a
/// named room's tiles instead of toward a placed target's: the walk to the fight
/// is what decides whether a body standing at the oven can have it, exactly as an
/// outpost's Harvest is offered to a body standing at home.
let internal travelCostOf (threats: Threats) atlas (creep: string) task =
    match task with
    | Flee -> Atlas.travelCostWithin atlas creep (areaFor threats atlas creep task)
    | Guard room -> Atlas.travelCostToward atlas creep task room (areaFor threats atlas creep task)
    | _ ->
        match areaFor threats atlas creep task with
        | area when Set.isEmpty area -> Atlas.travelCost atlas creep task
        | area -> Atlas.travelCostWithin atlas creep area

/// The mover's step toward the tiles a creep may work its Task from, and the
/// mate of `travelCostOf` above: whatever the price crossed for, the walk has to
/// cross for too, or a body is ranked onto a Task it is never carried to. A
/// Guard names its own room and is stepped toward it (`firstStepToward`); every
/// other Task derives its crossing from the target the projection places
/// (`firstStep`).
let internal stepToward atlas (creep: string) task (area: Set<RoomPos>) =
    match task with
    | Guard room -> Atlas.firstStepToward atlas creep task room area
    | _ -> Atlas.firstStep atlas creep task area

/// Whether the Reach has taken the whole of a Task's Work Area (ADR 0033): it
/// had somewhere to stand and has nowhere left. That makes the Task
/// inapplicable to that creep — a Harvest whose only Seat is hot is no Harvest
/// — and releases a holder under a reason of its own, so the transition log
/// tells a raid's release from a Task that vanished. An area that was empty to
/// begin with is not threatened: unplaceable or blocked geometry is the
/// reachability gate's answer (ADR 0002), and a tick with no Reach anywhere
/// takes nothing from anything.
///
/// **The area read is the target room's, and never the creep's share of it**
/// (#147). Asked through `workAreaFor` — the *permission*, which is empty
/// across a border by construction (ADR 0041) — the rule never fired for a body
/// standing in another room: an empty area is not "threatened", it is
/// unplaceable, so ADR 0033's "inapplicable to everyone" reached everyone
/// except the bodies still walking. Live that is a wasted crossing and a room
/// under attack at the end of it: a home worker was matched to an outpost
/// Harvest whose every Seat was inside a Reach on the very tick that outpost's
/// own crew was fleeing off them, arrived, and was released `NoneApplicable`
/// beside the invader. So the reading is `workAreaAcross` — the same tiles
/// narrowed for the same body, in the room the target stands in — less the
/// Reach of *that* room, which makes it one question asked the same way
/// whichever side of the [[seam]] the body is on.
///
/// **Written beneath the Safety tier, which is the whole of the ordering here**
/// (ADR 0056 decision 3, `safetyTier`): a [[guard]]'s Work Area *is* a ring of
/// Reach tiles, so a reading that judged that tier by its ground would call
/// every Guard threatened on every tick one exists, and the gate that sends a
/// body into the fight would be the one thing keeping it out. Said out loud
/// rather than left to arithmetic: neither Safety Task has a Work Area in the
/// [[atlas]] at all — both areas are the tick's `Threats` (`areaFor`) — so the
/// tiles read below are empty for them today and the clause changes no answer.
/// It is the ordering the decision names, and what keeps this rule right on the
/// day one of those two grows an area the Atlas places.
let internal threatened (threats: Threats) atlas (creep: CreepInfo) task =
    // The negation of the join `areaFor` makes: nowhere left to stand is every
    // tile of the area inside that room's Reach.
    match reachOnWork threats atlas task with
    | None -> false
    | Some(room, reach) ->
        let area = Atlas.workAreaAcross atlas creep.Name task

        not (Set.isEmpty area)
        && area
           |> Set.forall (fun tile ->
               Some tile.Room = room && Set.contains (RoomPos.pos tile) reach)

/// Whether the creep itself is standing where it can be hurt: its own tile
/// inside a Reach of its own room (ADR 0033). Flee's applicability is this and
/// a body that can run, and the vision grace asks it too — the grace is the one
/// keep that answers for a Task no gate below can be asked about, so the
/// question ADR 0033 puts above all work is asked of the creep instead. Total
/// (ADR 0004): a creep the projection cannot place stands in no Reach.
let internal standsInReach (threats: Threats) atlas (creep: string) =
    match Atlas.creepTile atlas creep with
    | Some tile -> Set.contains (RoomPos.pos tile) (Threats.reachIn threats tile.Room)
    | None -> false

/// Whether the room a construction site stands in satisfies a rule — the room
/// join every site predicate below makes, and ADR 0004's totality with it: an
/// unplaced site names no room and answers **false**, the ordinary surplus
/// Build it has always been. Read off the projection and not off the
/// declaration (ADR 0041), exactly as the Reserve pool's room join is, so a
/// room a stand-down drops from the scan set (ADR 0043) leaves this reading
/// with it. Written once because the totality is one decision: spelled out at
/// each site, one of the five had come to route an unplaced site through the
/// sentinel room name `""` instead, and answered right only because no colony
/// borrows a room called that.
let private siteRoomIs atlas (rule: string -> bool) siteId =
    Atlas.targetRoom atlas siteId |> Option.exists rule

/// Whether a construction site stands in a room this colony **mines** — an
/// [[outpost]]'s, and so a site the outpost builders' budget may ration rather
/// than a piece of the home room's surplus. One half of that queue's reading
/// and not the whole of it: `planPool` narrows it again by what no other rule
/// already feeds, because a claimed room a human still names in this colony's
/// outpost list is not `Borrowed` and answers true here (`Colony.bootstrapping`,
/// ADR 0047 decision 1). The room half alone since #266:
/// what the budget covered was the container site this colony places itself
/// (ADR 0042), and out there a human paves too (ADR 0042 as #244 amends it) —
/// live, 45 hand-laid road sites in W13S29 stood at 0/300 for as long as they
/// were surplus, because a loaded worker at home is a step from its own
/// controller and a Seam plus sixty tiles from the trunk. So the kind half is
/// gone from the *reading* and survives as the order the budget is spent in
/// (`planPool`), the container staying the switch it always was. Read off the
/// projection and not off the declaration (ADR 0041), exactly as the Reserve
/// pool's room join is, so a room a stand-down drops from the scan set (ADR
/// 0043) leaves this reading with it. Total (ADR 0004): an unplaced site names
/// no room, answers false, and is the ordinary surplus Build it has always
/// been.
let private isOutpostSite (view: ColonyView) atlas siteId =
    siteRoomIs
        atlas
        (fun room ->
            room <> SpatialInfo.homeName view.Spatial
            // A borrowed room's site is the child's own and not an outpost's (user
            // decision 2026-09-07): it neither draws the outpost builders' budget
            // nor dilutes it — the nursery's and the bootstrapping child's sites
            // reach the pool by their own rules, and the budget is spread over the
            // sites of rooms the colony *mines*.
            && not (List.contains room view.Borrowed.Rooms))
        siteId

/// Whether a construction site stands in a **nursery** — a room this colony has
/// claimed and not yet stood a spawn in (ADR 0047 decision 4). `isOutpostSite`'s
/// room read asked one question deeper — an outpost is a room this colony mines
/// and a nursery is one it has claimed — and answered a rank deeper with it: in
/// a nursery **every** site is feeding-tier outright, where in an outpost only
/// the builders' budget's own head is. Total the same way (ADR 0004).
let private isNurserySite (view: ColonyView) atlas siteId =
    siteRoomIs atlas (isNurseryRoom view) siteId

/// Whether a room is **bootstrapping** as seen from this colony's tick: a child
/// of ours running its own spawn (the mother's reading), or this colony's own
/// home standing at the `Bootstrapping` stage (the child's own reading). One
/// predicate for both ticks, because the rule that reads it is about the room
/// and not about who is looking (ADR 0052 decision 3). The home half reads the
/// stage and not a level of its own.
let private isBootstrappingRoom (view: ColonyView) room =
    isBootstrapRoom view room
    || (room = SpatialInfo.homeName view.Spatial && homeStage view = Some Bootstrapping)

/// A site standing in a bootstrapping room: feeding-tier in both pools (user,
/// 2026-09-06). What a room under RCL3 builds is its containers and its
/// extensions, and the extensions are the bank — 300 to 550 doubles the Anchor
/// body and with it the income the whole window is waiting on — so they come
/// before the controller, for the child's own workers and for the pioneers
/// alike.
let private isBootstrappingSite (view: ColonyView) atlas siteId =
    siteRoomIs atlas (isBootstrappingRoom view) siteId

/// Whether any site stands in the room of the named controller — the
/// borrowed Upgrade's other half: while the child has sites, its
/// controller waits.
let private sitesPendingBeside (view: ColonyView) atlas controllerId =
    match Atlas.targetRoom atlas controllerId with
    | None -> false
    | Some room ->
        view.ConstructionSites
        |> List.exists (fun site -> Atlas.targetRoom atlas site.Id = Some room)

/// Whether this Build is on the feeding tier rather than in the surplus the
/// colony's other sites are spent out of — the three rules that lift one there,
/// said once. One reader is left: `tierOf`, and nothing else. ADR 0052 decision
/// 6 had folded the body gate into this reading too, and then #234 lifted every
/// home site a rung over the Upgrade beside it, leaving no Build on the ladder
/// travel cost still thins, so that gate stopped asking about the target. The
/// outpost sites the builders' budget has picked out this tick — handed in,
/// because which they are is a fact about the whole outpost's queue and not
/// about the one site (#266) — the container among them being ADR 0042's switch
/// on whether that room is in the economy at all; and every site in a nursery,
/// the switch on whether there is going to be a second colony at all (ADR
/// 0047).
let private isFeedingSite (view: ColonyView) atlas (fed: Set<string>) siteId =
    Set.contains siteId fed
    || isNurserySite view atlas siteId
    || isBootstrappingSite view atlas siteId

/// Whether a site stands in this colony's **own home room** — the room #234's
/// surplus rung is scoped to, and the one question that separates the site a
/// colony grows by from a site it would cross a [[seam]] for. The rung lifts a
/// Build over the Upgrade it shares the surplus tier with, and a rank the whole
/// colony shares is exactly what [[travel cost]] can no longer thin. At home
/// that is the point: the sites and the controller stand a few tiles apart. The
/// room join is `isOutpostSite`'s (ADR 0041), and total (ADR 0004)
/// resolved toward home.
///
/// `Option.forall` and not `siteRoomIs`' `Option.exists`, which is why this one
/// of the five does not reach through that helper: its totality resolves the
/// other way, an unplaced site reading as **home** rather than as elsewhere.
let private isHomeSite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId
    |> Option.forall (fun room -> room = SpatialInfo.homeName view.Spatial)

/// The full downgrade timer per controller level (Screeps
/// CONTROLLER_DOWNGRADE).
let private fullDowngradeTimer level =
    match level with
    | 1 -> 20000
    | 2 -> 10000
    | 3 -> 20000
    | 4 -> 40000
    | 5 -> 80000
    | 6 -> 120000
    | 7 -> 150000
    | _ -> 200000

/// The hard deadline on the controller's downgrade timer: half the level's full
/// timer. The engine refuses activateSafeMode once the timer sinks below half
/// minus 5,000 (its grace), so escalating at half keeps the safe-mode reflex
/// fireable with the whole grace still banked — a downgrade costs a level and
/// zeroes the stock, so neither line is ever approached (ADR 0007).
let private downgradeDeadline level = fullDowngradeTimer level / 2

/// Whether the controller stands inside its downgrade deadline (ADR 0007).
let private insideDowngradeDeadline (view: ColonyView) =
    view.Controller
    |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

/// The tier of work a Task belongs to, once its target is taken into account
/// (ADR 0010, ADR 0012, ADR 0023) — the ladder `planPool` sets each entry's
/// [[priority]] off.
type internal Tier =
    /// Getting out of a Reach (ADR 0033): the one Task in it is Flee, and
    /// it sits above every other tier and above the downgrade deadline
    /// too, because no other work matters while a creep is being killed.
    | Safety
    /// Feeding the economy: Harvest, a container's Withdraw, the Refill of a
    /// spawn or an extension, Reserve, the Build of the outpost sites the
    /// builders' budget has picked out this tick (#157, widened by #266) and
    /// every site in a **nursery** (ADR 0047) — the flow the colony's
    /// reproduction runs on, and beside it ADR 0042's two switches on a third of
    /// that flow: the Reserve that decides how fast an outpost's rock gives, and
    /// the Build that decides whether the room is in the economy at all — the
    /// container that makes the rock a Post, and the trunk the haul off it is
    /// priced on. The nursery's sites are the third switch and the deepest of
    /// them.
    | Feeding
    /// The Storage's Withdraw (ADR 0023): the colony's stock as an intake, one
    /// tier below the source containers the flow fills, so a stock standing
    /// beside the spawn never wins the travel-cost tie the containers have to
    /// win.
    | StockDraw
    /// Surplus work: a tower Refill (ADR 0010), Build, Repair and Upgrade. The
    /// colony feeds its own reproduction before its guns, and everything it
    /// merely spends energy on waits behind the flow.
    | Surplus
    /// The controller container's Refill (ADR 0012): a full creep beside the
    /// buffer sinks its load into the controller rather than dumping it back
    /// into the container it just drew from and orbiting in place, so the buffer
    /// is filled by bodies with no surplus work of their own.
    | UpgradeBuffer
    /// The Storage's Refill (ADR 0023): the colony's stock, deeper than every
    /// sink that spends. A load reaches it only when there is nowhere else at
    /// all to put it, the upgrade buffer included, so the stock never outbids
    /// the flow, however close beside the spawn it stands.
    | Stock

/// How far apart two tiers stand on the [[priority]] ladder. Ten and not one,
/// so that a Task can be ordered against another **inside** its tier
/// (`priorityStep`) without ever reaching the tier above or below it.
let internal tierRungs = 10

/// The whole tier order, shallowest first — the one place the ordering lives
/// (ADR 0010, ADR 0012, ADR 0023): the flow is fed, then the stock is drawn on,
/// then surplus is spent, then whatever is left sinks into the upgrade buffer,
/// and what even the buffer cannot hold is stocked. The stock's two roles sit
/// on either side of the surplus work the colony does between them. Exhaustive
/// over Tier on purpose — a tier this match forgets is a build error. The
/// downgrade deadline (ADR 0007) is the one thing above the sequence rather
/// than in it.
let internal priorityOfTier =
    function
    // One tier beneath `deadlineRank`'s, which is itself one beneath the
    // shallowest tier of work: a fleeing creep outbids even a controller
    // about to downgrade (ADR 0033).
    | Safety -> -2 * tierRungs
    | Feeding -> 0
    | StockDraw -> tierRungs
    | Surplus -> 2 * tierRungs
    | UpgradeBuffer -> 3 * tierRungs
    | Stock -> 4 * tierRungs

/// One tier above the shallowest tier of work: where the downgrade
/// deadline puts Upgrade (ADR 0007). Not a tier of its own — "never let it
/// downgrade" is an ordering imposed on the sequence, not a tier of work.
let private deadlineRank = -tierRungs

/// The step a Task is moved by when it is ordered against another inside
/// one tier. One rung of ten, so it never crosses a tier and the tier
/// order is what it always was.
let internal priorityStep = 1

/// Which of the five shapes a body is, as far as a [[capacity]] is concerned
/// (ADR 0052 decision 6, ADR 0006): part arithmetic, asked in the order the
/// existing gates ask it in, because Heavy and Standing overlap on the
/// [[anchor]]'s `6W/1C/1M` and every rule that reads both reads the heavy one
/// first (ADR 0016 before ADR 0046). `Fighter` is asked before all of them (ADR
/// 0056): a guard's `[T; A×3; M×5; H]` carries no Work at all, so the four
/// classes below would answer `Carrier` — the class of the bodies that shift
/// energy, and the one a Guard's capacity must not be sharing a number with.
/// Exported for the same reason `bodyFor` and `patternTable` are (ADR 0006): the
/// ladder is a body fact a test reads directly. The head of the ladder is read
/// in one [[capacity]] scope and one only — the Guard Task's `Fighter -> the
/// room's quota, every other class 0` (`Capacity.Fighters`, ADR 0056) — so a
/// body that stopped answering `Fighter` here would be a body no Guard admits.
let bodyClassOf (tuning: Tuning) atlas (creep: CreepInfo) : BodyClass =
    if isGuardBody creep then Fighter
    elif Atlas.workHeavy atlas creep.Name then Heavy
    elif isStandingBody tuning creep then Standing
    elif partCount creep.Body Work = 0 then Carrier
    else Light

/// Planner, second half: this tick's pool with each entry's [[priority]] and
/// [[capacity]] on it (ADR 0052 decision 6). `planTasks` says **what** is
/// pooled; this says where each entry ranks and how many bodies it admits, and
/// between them they are everything the Matcher knows about a Task — which is
/// why the Matcher can be, and now is, blind to Task kinds. Every exception the
/// colony has learned about ordering and crowding lands here and nowhere else:
/// the tier ladder, the [[downgrade deadline]]'s lift (ADR 0007), a source's
/// [[seat]]s and [[post]]s (ADR 0024, ADR 0051), a store's stock over the load
/// of the row that draws it, one holder per controller (ADR 0042, ADR 0047),
/// the outpost builders' budget — which since #266 rations the feeding tier
/// out there as well as the crowd on it — the [[pioneer]]s' ceiling and the
/// garrison's own tile.
let planPool (view: ColonyView) atlas (tasks: Task list) : PooledTask list =
    let bank = view.Bank.Capacity

    // The three loads a store is divided by, each the row's own cast at the
    // richest bank and never a candidate's own carry: a capacity is a fact about
    // the Task, so one store must not answer two numbers depending on which
    // creep asked — except by [[body class]], which is the one place
    // a store answers two numbers on purpose.
    let haulerLoad = carryCapacityOf (bodyFor haulerPattern bank)
    let workerLoad = carryCapacityOf (workerBodyFor bank)
    let standingLoad = carryCapacityOf (bodyFor upgraderPattern bank)

    let buffers = Atlas.controllerContainers atlas

    // The [[refill cluster]], off the one rule its three readers share
    // (`RefillCluster.ofRefillables`, ADR 0054): `planTasks` pooled the
    // spawn, this bounds it, and the Atlas lays its Work Area.
    let cluster = RefillCluster.ofRefillables view.Refillables

    // The [[ferry]]'s sinks, named by the one rule three readers share
    // (`ferryBuffers`): what a mother lends a bootstrapping child is
    // written down and bounded, so the Refill `planTasks` pooled for the
    // child's buffer carries that bound here.
    let ferrySinks = ferryBuffers view

    // One `Tuning.FerryLoads` budget per child room, spread over that room's
    // buffers in id order (user decision 2026-09-07): the hauler row hires per
    // child, so the pool admits per child — a second buffer in one room shares
    // the lend rather than doubling it, and with a budget smaller than the
    // buffer count the last ones take none.
    let ferryShare: Map<string, int> =
        ferrySinks
        |> Set.toList
        |> List.choose (fun id -> Atlas.targetRoom atlas id |> Option.map (fun room -> room, id))
        |> List.groupBy fst
        |> List.collect (fun (_, buffers) ->
            let ids = buffers |> List.map snd |> List.sort
            let n = List.length ids
            let budget = view.Tuning.FerryLoads

            ids
            |> List.mapi (fun i id -> id, budget / n + (if i < budget % n then 1 else 0)))
        |> Map.ofList

    let stored id = SpatialInfo.storedIn view.Spatial id

    // **The rescue budget** (#284): the decaying structures this colony has let
    // fall so far below their own trigger that a repair is no longer surplus
    // work, lifted two rungs over the rest of the tier and given one body
    // apiece. The failure it answers is not a tie the colony loses but one it
    // cannot ever win: the surplus tier is ordered by travel cost from where a
    // body stands, and since a road is hungry below half its max and whole
    // *at* half (ADR 0010), the cluster a loaded worker stands in regenerates
    // its own supply of two-tile-away Repairs faster than anybody would walk
    // out of it. Live at t239,65x the base cluster's roads sat in a band from
    // 50.0% to 58% while the trunk north (2%) and every road in the outpost
    // (8%) decayed toward destruction — and a destroyed road out there is the
    // human's paving, which no Layout re-places.
    //
    // The shape is the outpost builders' budget one Task over (#157, #266): a
    // small colony-wide number, the worst first, the rest left in the surplus
    // where travel cost goes on keeping the row at home. **The lift reaches the
    // decaying kinds alone** — a rampart is judged against a floor and a Keep
    // structure against full hits, and neither is a thing the colony is letting
    // rot. The order is the fraction of max and never the hits: a plain road
    // and a swamp road five times its max are equally far gone at a quarter.
    // Ties fall to the id, the way every other tie here does.
    //
    // **One body apiece**, because the whole of what a rescue buys is a body
    // that walks out there at all: a second one on the same road is the crowd
    // #157 exists to prevent, and the walk it makes is the expensive half. It
    // needs no second visit — a worker's load repairs a hundred hits an energy,
    // so one trip carries a road from a quarter to over the trigger and out of
    // the pool, and the Matcher's keep holds it there until it is whole.
    let rescued =
        tasks
        |> List.choose (function
            | Repair id ->
                match Map.tryFind id view.Spatial.TargetKinds, Map.tryFind id view.Spatial.Hits with
                | Some(Structure kind), Some hits when
                    wholeLine kind = Some WholeLine.Fraction
                    && hits.HitsMax > 0
                    && float hits.Hits <= view.Tuning.RepairRescueLine * float hits.HitsMax
                    ->
                    Some(float hits.Hits / float hits.HitsMax, id)
                | _ -> None
            | _ -> None)
        |> List.sort
        |> List.truncate view.Tuning.RepairRescues
        |> List.map snd
        |> Set.ofList

    // **The queue the builders' budget rations** (#266): every site the pool
    // holds in a room this colony merely mines and that no other rule already
    // feeds. The second clause is what keeps the budget's head worth having.
    // A [[nursery]]'s site and a bootstrapping child's are feeding-tier
    // outright by their own reading (ADR 0047 decision 4) and capped by
    // nothing, and neither room is always `Borrowed`: while a human still
    // names the child's room in the mother's `Outposts` list
    // `Colony.bootstrapping` drops it from `BorrowedWork.Rooms` — its own
    // docstring spells that state out, and ADR 0047 decision 1 makes it the
    // normal one before the declaration is split — so `isOutpostSite` answers
    // true for its sites. Left in the queue they take places the lift buys
    // them nothing with, and the outpost's own container, ADR 0042's switch on
    // whether that room is in the economy at all, is pushed back into the
    // surplus where travel cost answers a Seam and sixty tiles against an
    // Upgrade underfoot. Asked through `isFeedingSite` with the budget's own
    // answer held empty, so the queue and the tier read one sentence and
    // cannot drift apart.
    let outpostSites =
        tasks
        |> List.choose (function
            | Build siteId when
                isOutpostSite view atlas siteId
                && not (isFeedingSite view atlas Set.empty siteId)
                ->
                Some siteId
            | _ -> None)

    // **The order the budget is spent in** (#266): the container sites first,
    // then the nearest to the crossing. The container is ADR 0042's switch on
    // whether the room is in the economy at all, so it is never queued behind a
    // road; every other site out there is a human's [[trunk]] (ADR 0042 as #244
    // amends it), and a trunk is worth building from the [[seam]] outward,
    // because the paved tiles nearest the crossing are the ones every haul from
    // that room walks over. The order asks the *kind* only for that one
    // question and the lift asks it not at all, which is #266's whole
    // narrowing undone if a kind list were written back in: at RCL0 the engine
    // allows a road and a container out there and nothing else, so a list
    // would name what a human is allowed to want built, and out here as in a
    // [[nursery]] that is not a judgement this colony makes. `Atlas.seamWalkTicks` is the same walk ADR 0042
    // anchors its container pick on — to the border and not across it — so the
    // two rules out here measure one thing. **One queue over every outpost and
    // not one apiece**, which is what keeps the budget the colony-wide number
    // #157 made it: two rooms' sites are ordered against each other on a walk
    // that leaves the home-side leg off both, so what the comparison says is
    // "nearer its own crossing" and not "nearer the spawn" — a tie-break inside
    // a budget, never a price (ADR 0002 does the pricing, from where the body
    // stands). Ties fall to the id, the way every
    // other tie in this colony falls, and a site whose walk cannot be priced
    // sorts last rather than out of the list: unpriceable is not nearest (ADR
    // 0004).
    let siteOrder siteId =
        let container =
            if Map.tryFind siteId view.Spatial.TargetKinds = Some(Site BuiltKind.Container) then
                0
            else
                1

        let walk =
            Atlas.positionOf atlas siteId
            |> Option.bind (fun tile ->
                Atlas.seamWalkTicks
                    atlas
                    tile.Room
                    (SpatialInfo.homeName view.Spatial)
                    (RoomPos.pos tile))
            |> Option.defaultValue System.Int32.MaxValue

        container, walk, siteId

    // **The budget rations the tier, not just the crowd on it** (#266, live:
    // W13S29's 45 hand-laid road sites at 0/300 while two workers refilled and
    // upgraded at home). Lifting *every* outpost site onto the feeding tier is
    // the failure #157's budget was written against — the whole worker row over
    // the Seam at once — and leaving them all in the surplus is the failure
    // above, travel cost answering 120 against an Upgrade underfoot that costs
    // nothing. So the same number does both: the first `Tuning.OutpostBuilders`
    // sites in the order above are lifted and the rest stay surplus, and as each
    // one is finished the next one out takes its place. The head is all that is
    // wanted, so the order is asked for only when the budget cannot cover the
    // list — the walk behind it is a flood over the outpost's whole grid, and a
    // colony whose outpost holds one site pays for none of it.
    let fedOutpostSites =
        if List.length outpostSites <= view.Tuning.OutpostBuilders then
            outpostSites
        else
            outpostSites
            |> List.sortBy siteOrder
            |> List.truncate view.Tuning.OutpostBuilders

    let fedSiteIds = Set.ofList fedOutpostSites

    // **A budget and not a per-site number** (#157): `planOutpostContainers`
    // places a site for *every* unserved outpost source, all on the same tick,
    // so a per-site two is a colony-wide six — the whole worker row, and
    // exactly what the cap exists to prevent. The budget is spread over the
    // sites it has lifted, floored at one apiece, and as each site completes
    // the divisor falls and the survivors get the bodies back. The divisor is
    // the **lifted** list and never the whole pool (#266): spread over the
    // pool, W13S29's 45 sites took one builder apiece and the colony-wide two
    // was no cap at all. Under #266 the divisor and the queue are one list, so
    // a room the budget does not ration no longer dilutes it either — the two
    // used to be separable and are not any more, a place in the list now being
    // the lift itself. **Two is a tunable, and this is the reason for that
    // number**: one is the smallest crowd that builds, and two is the smallest
    // that survives losing a body — a container is 5,000 progress against a
    // generalist's 50, so a lone holder that dies or is released by a Reach
    // (ADR 0033) leaves the switch open for a whole cast-and-walk cycle. Which
    // is why the spread stays a spread rather than becoming one apiece: with a
    // single switch open the pair is what #157 asked for, and the budget is
    // spent either way.
    let builderShare =
        match fedOutpostSites with
        | [] -> 0
        | sites -> view.Tuning.OutpostBuilders / List.length sites |> max 1


    // The tier a Task sits in. Refill, Withdraw and Build are the three Tasks
    // whose tier layers by target (ADR 0010, ADR 0023, ADR 0042). Two of the
    // three read the layer off the projection's kind and nothing else — the
    // stock is recognised for what it is, never for where it stands; the third,
    // Build, asks where as well. On Refill the Storage and the container are
    // each one projected kind and exclude each other by construction, while a
    // tower is read off the Refillables census, which can overlap either — so
    // the kind is asked first, deepest answer first, and the census only of
    // what the kind leaves.
    let tierOf task =
        match task with
        | Flee -> Safety
        // Beside Flee, on Flee's own argument and with no rung of its own (ADR
        // 0056): no other work matters while a creep is being killed. The tier
        // holds two Tasks and no ordering between them, because decision 3
        // makes them disjoint by [[body class]] — Flee inapplicable to a
        // Fighter, a Guard applicable to nothing else — so nothing ever asks
        // how the two compare. These are `safetyTier`'s own two kinds, and the
        // two rules that skip the Reach for this tier read that predicate
        // rather than this ladder, which is asked only of a pooled Task.
        | Guard _ -> Safety
        | Harvest _ -> Feeding
        // **A decision made here, because nothing else made it.** ADR 0042 and
        // #116 both fix the reserver row's *casting* order and neither says a
        // word about its *matching* order, and `priorityOfTier` is exhaustive
        // on purpose, so a tier had to be chosen. Reserve joins the feeding
        // tier on the casting order's own argument: every other row spends the
        // colony's income, and this one decides whether that income is five a
        // tick or ten.
        | Reserve _ -> Feeding
        // Beside the Reserve it replaces, and for a stronger form of the same
        // argument (ADR 0047): a reservation decides whether one room's income
        // is five a tick or ten, and a claim decides whether there is going to
        // be a second colony at all.
        | Claim _ -> Feeding
        | Withdraw storeId ->
            if Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Storage) then
                StockDraw
            else
                Feeding
        // A pile is flow and not stock: it is the haul cycle's energy lying
        // where it fell — an Anchor's overflow, a death drop — so it feeds the
        // colony on the tier the containers do, and which of the two an empty
        // carrier goes for is travel cost's call — for every pile but the two
        // `priorityOf` steps up a rung, the one lying on a drawable store and
        // the one holding half a [[hauler unit]]'s load (#216 R5, #242).
        | Pickup _ -> Feeding
        | Refill structureId ->
            let isTower =
                view.Refillables
                |> List.exists (fun r -> r.Id = structureId && r.Kind = BuiltKind.Tower)

            let kind = Map.tryFind structureId view.Spatial.TargetKinds

            if kind = Some(Structure BuiltKind.Storage) then
                Stock
            elif kind = Some(Structure BuiltKind.Container) then
                UpgradeBuffer
            elif isTower then
                Surplus
            else
                // The flow, and since ADR 0054 the [[refill cluster]] arrives
                // here through the same door rather than a case of its own:
                // the cluster is keyed on a spawn, and the ring it stands for
                // is spawn-feeding to the last extension.
                Feeding
        // The switch ADR 0042 hangs a whole room on, ranked where a switch
        // belongs (#157). A standing container is what admits an outpost into
        // the economy, so building it is not surplus work done with spare
        // energy — it decides whether a third of the colony's income exists at
        // all. Read on the surplus tier, only travel cost separated it from
        // Upgrade, and the home controller is a few tiles from a loaded worker
        // while the site is a Seam and fifty tiles away: every worker upgraded,
        // every tick. **And the same is true of the road beside it** (#266):
        // the trunk a human paves out there is what the [[hauler unit]]'s round
        // trip is priced on, so the argument that lifted the container reaches
        // as far as the budget can pay for — `fedSiteIds` is the head of that
        // queue and the tail stays in the surplus, where travel cost goes on
        // keeping the row at home. The same argument one question deeper for a
        // **nursery**'s sites (ADR 0047 decision 4): the spawn a human has
        // placed in a room this colony has claimed decides whether there is
        // going to be a second colony at all.
        | Build siteId when isFeedingSite view atlas fedSiteIds siteId -> Feeding
        // A bootstrapped child's Upgrade, in the mother's pool (#213): the tier
        // the pioneers were hired for. Left in the surplus beside the home
        // Upgrade, travel cost — a Seam and fifty tiles against five — kept
        // every one of them at home, and the addend was three more home
        // upgraders.
        | Upgrade controllerId when
            isBorrowedUpgrade view controllerId
            && not (sitesPendingBeside view atlas controllerId)
            ->
            Feeding
        | Build _
        | Repair _
        | Upgrade _ -> Surplus

    // The Task's place on the ladder: its tier, with the two orderings that are
    // not tiers laid over it. **The colony's own controller and no other.** The
    // deadline is read off `ColonyView.Controller`, which is this colony's
    // alone, and since ADR 0047 decision 4 the pool can hold a second Upgrade —
    // a bootstrapped child's. Lifting that one on the mother's timer would send
    // her whole loaded fleet across the Seam on the tick her *own* controller
    // was closest to downgrading. The child escalates its own controller in its
    // own tick. Where the pool's Feeding-tier stores stand, so a [[pickup]] can
    // be asked whether one of them is under its own pile. Read off the pool and
    // not off the projection's whole container census: a store the pool holds
    // no Withdraw for is not an alternative to anything.
    let drawableTiles =
        tasks
        |> List.choose (fun task ->
            match task with
            | Withdraw storeId when tierOf task = Feeding ->
                SpatialInfo.placementOf view.Spatial storeId
            | _ -> None)
        |> Set.ofList

    // **A [[pickup]] outbids the [[withdraw]] standing on its own tile** (live:
    // a hauler beside a full container ignored the pile on it). The two share
    // the feeding tier and the tile, so travel cost is equal and, when this was
    // written, the pool's order decided and the container stood first in it;
    // what separates them is decay — a pile loses `ceil(amount / 1000)` a tick
    // and a container loses nothing, so the energy that has to be taken first
    // is the energy that is going away. Since #242 the pool's order says that
    // much on its own — the piles stand before the Withdraws, so the same-tile
    // tie falls to the pile with no rung at all — and what this clause still
    // buys is the rest of the claim: a pile lying on a drawable store outranks
    // every *other* Feeding container in the colony, however much nearer that
    // one is. Written as the **Pickup** stepping up a rung and conditioned on
    // the store under it, so that it stays a claim about that one tile: a
    // [[priority]] is a scalar the whole tier is ordered by, so whichever of
    // the pair moves moves against every other Feeding Task in the colony. Stepping the *Withdraw* down was tried
    // first and is the bug it was meant to cure, inverted — a hundred-energy
    // overflow demoted a full container behind every other store at any
    // distance, and the engine drops that overflow only once the container is
    // full. One rung is inside the tier (`priorityStep`). The second lift, one
    // rung under the Pickup's: a **full source container**, whose income is
    // going away too, and which a hauler row sized to the mean round trip let
    // overflow for hours. Source containers alone: the buffer and the Storage
    // are sinks the haulers fill. The two rungs sit the other way round from
    // the first cut: with the pile above the full container the haulers chased
    // fifty-energy piles all day and never drew the 2,000 beside them, so every
    // pickup bred the next pile. **A [[pickup]] steps up where the pile is
    // worth a trip of its own** (#242, user: "worker 和 hauler 在不满的
    // container 和地上的能量中会优先选择前者") — the same rung as the tile
    // rule, taken by either clause, because the two are one sentence about one
    // pile. Where that clause is a claim about the store *under* the pile, this
    // one is the claim to make where there is no store under it at all: half a
    // [[hauler unit]]'s load or more lying on the ground is a whole trip, and a
    // trip made for it takes the copy that is going away rather than the one
    // that is not. Half a load and read off the row's own cast at this bank,
    // never the candidate's carry — the Planner is creep-blind (ADR 0013) —
    // which makes it the creep-blind mirror of `applicable`'s `worthTheTrip`
    // (#232): half a load is what makes a store worth a body's trip there and
    // what makes a pile worth one here. **The rung reaches the whole Feeding
    // tier** and not the container Withdraws alone, a [[priority]] being one
    // colony-wide scalar: a lifted pile outbids the spawn ring's [[refill]],
    // the [[harvest]], the [[reserve]] and the [[claim]], the Withdraw of a
    // tombstone or a ruin — a store that ends is rank 0 until it holds a
    // container's worth — and the feeding-tier [[build]]s of ADR 0042 and ADR
    // 0047, at any travel cost, a rank being settled before a price is asked.
    // The full source container's two rungs are the only thing above it. That
    // reach is the mechanism's price and not an oversight: the rung has to be
    // carried by the Pickup (above), and there is no rung that separates a
    // Task from one member of its tier and not from the rest. Every smaller
    // pile stays on rank 0 and is ordered by distance alone, which is the whole
    // reason the lift is not given to piles as a class: one hundred energy
    // forty tiles off is no reason to leave the 1,500 under a body's feet.
    // **Below a bank of 450 there is no smaller pile.** The row's cast carries
    // `100 * (bank / 150)`, so at RCL1's 300 half a load is a hundred —
    // `Tuning.PickupThreshold` itself — and every pile the pool holds takes the
    // rung; the distance-only rung exists only once the cast outgrows twice the
    // threshold, from RCL2 up. The line is the one #242 pinned, and whether a
    // bootstrapping colony's one body should walk off its rock for a
    // threshold-sized pile is that question's own issue and not this one's.
    // **A site outranks the controller inside the surplus tier** (#234, live:
    // 42 sites in one colony while every loaded worker upgraded). Build, Repair and Upgrade shared one rung, so travel
    // cost alone ordered them, and a worker that fills at the [[buffer]] is
    // already standing in the controller's Work Area: Upgrade costs it nothing
    // and never goes task-gone. What ADR 0042 and ADR 0047 lifted to Feeding
    // was the site that decides whether income *exists*; this is the ordinary
    // home site, which decides how fast it grows.
    let priorityOf task =
        let step =
            match task with
            | Pickup pileId ->
                let overADrawableStore =
                    SpatialInfo.placementOf view.Spatial pileId
                    |> Option.exists (fun tile -> Set.contains tile drawableTiles)

                let worthATripOfItsOwn = stored pileId * 2 >= haulerLoad

                if overADrawableStore || worthATripOfItsOwn then
                    -priorityStep
                else
                    0
            | Withdraw storeId when
                tierOf task = Feeding && stored storeId >= Engine.containerCapacity
                ->
                -2 * priorityStep
            | Build siteId when tierOf task = Surplus && isHomeSite view atlas siteId ->
                -priorityStep
            // Over the home site as well as over the Upgrade (#284): a site is
            // work the colony chose to start, and a structure a quarter from
            // destruction is work it has already paid for and is about to lose.
            | Repair id when Set.contains id rescued -> -2 * priorityStep
            | _ -> 0

        match task with
        | Upgrade id when
            insideDowngradeDeadline view
            && view.Controller |> Option.exists (fun c -> c.Id = id)
            ->
            deadlineRank
        | _ -> priorityOfTier (tierOf task) + step

    // How many bodies the Task admits, and of which shapes. **Harvest is three
    // numbers over one source** (ADR 0024, ADR 0051): the Seat count every
    // harvester shares, the Post count only the garrisons compete for, and the
    // Seats beyond the Posts the light bodies are left, which sum back to the
    // Seat count exactly. A source with no Post derives neither of the last
    // two, and the two rooms mean different things by that: at home nothing
    // narrows a heavy body's area (ADR 0020's pre-container fallback), so the
    // Seat cap is the only one; in an outpost that area is *empty*, so the
    // reachability gate rejects the pair for every heavy body. An unplaced
    // source derives no cap (ADR 0004). Beside the numbers, the **tiles**
    // (#205, widened by #269): every Post of the rock is held by the body
    // *standing* on it whatever Task it holds this tick, over the same census
    // the Post number is counted from. A cap that counted Harvest's own holders
    // alone read a Post as free on every tick its garrison held something else
    // — a build tick on a Post whose container is still a site, an Upgrade
    // through the empty window on a bare [[dual seat]] — and dispatched a
    // second heavy body across the room onto a tile that was never vacant
    // (#258's accepted window). **A Withdraw is capped by its store's
    // stock** (#161), **and a Pickup by its pile's**: `ceil(stored / one
    // drawer's load)`. Nothing else in the pipeline says it — the matching key
    // puts cost ahead of crowding (ADR 0002), so a container holding 400 draws
    // five haulers while a full one across the room stands unvisited. **The
    // [[buffer]] divides twice** (#196). ADR 0019 shuts every body with no Work
    // part out of the controller's container, so its drawers are the two Work
    // rows: the generalists, carrying 450, and the [[upgrader]]s, carrying
    // fifty. Divided by the generalist's load alone a 900-energy buffer admits
    // two drawers *in total*, so the row hired to stand there took at most two
    // seats; divided by the upgrader's alone it admits eighteen, which re-opens
    // the pile-on the cap is here for. So the store answers both numbers and
    // each class is counted against its own, with deliberately no `Total`.
    let isBorrowedSite siteId =
        siteRoomIs atlas (isBootstrapRoom view) siteId

    let capacityOf task =
        match task with
        | Harvest sourceId ->
            let seats = Atlas.seats atlas sourceId
            // One binding, read twice: the number and the tiles are the same
            // census since #269, and a second call is a second thing to narrow
            // later.
            let postTiles = Atlas.postsOf atlas sourceId
            let posts = Set.count postTiles

            { Capacity.unbounded with
                Total = seats
                Garrisons = (if posts = 0 then None else Some posts)
                Commuters =
                    seats
                    |> Option.filter (fun _ -> posts > 0)
                    |> Option.map (fun n -> max 0 (n - posts))
                Garrison = postTiles
            }
        // The [[guard]]s that room wants and nobody else at all (ADR 0056):
        // `guardsWanted` is the row's own arithmetic, read here a second time
        // rather than restated, so the number the cascade hires against and the
        // number the Matcher counts holders against are one number. One or two
        // — a second body where the raid out-heals the one the row would cast,
        // and a number that reads no guard of ours, so the cap cannot shut on
        // the arrival of the body it bought (#272) — and the class share is
        // what keeps the [[hauler unit]]s and the workers out of a Task whose
        // whole Work Area is a Reach.
        | Guard room -> Capacity.fighters (guardsWanted view room)
        // One holder per controller (ADR 0042, ADR 0047). A reservation is a
        // single capped number one body's CLAIM parts are sized to hold, so a
        // second body there buys nothing while the other outpost stays at five
        // a tick; for the Claim beside it the second body buys even less, a
        // room being claimed by one touch of one CLAIM part.
        | Reserve _
        | Claim _ -> Capacity.total 1
        | Withdraw storeId ->
            let stock = stored storeId

            if Set.contains storeId buffers then
                { Capacity.unbounded with
                    Standing = Some(ceilDiv stock standingLoad)
                    Generalists = Some(ceilDiv stock workerLoad)
                }
            else
                Capacity.total (ceilDiv stock haulerLoad)
        | Pickup pileId -> Capacity.total (ceilDiv (stored pileId) haulerLoad)
        // **The [[refill cluster]] is bounded by what it can still hold** (ADR
        // 0054, amending ADR 0029 for this one Task): as many bodies as the
        // ring's free energy divides into loads, so a second one joins only
        // while what stands empty exceeds what the first is carrying. The bound
        // is what makes one Task out of ten safe: ten Tasks of capacity one
        // apiece spread the crowd by accident, at the cost of a `task-gone`
        // release per creep per tick or two, and unbounded, one Task would
        // gather every loaded body onto one ring and leave the [[buffer]] and
        // the [[storage]] unvisited. Divided by the [[hauler unit]]'s load and
        // never a candidate's own carry, and a `Total` with no per-class share.
        | Refill spawnId when cluster |> Option.exists (fun c -> c.Spawn = spawnId) ->
            let free = cluster |> Option.map RefillCluster.free |> Option.defaultValue 0

            Capacity.total (ceilDiv free haulerLoad)
        // The lend, bounded (ADR 0052 decision 7): `Tuning.FerryLoads` bodies
        // at the child's buffer and no more, the same number the hauler row was
        // raised by, so a human retuning the lend retunes the hire with it. A
        // `Total` and not the hauler class's share alone: what makes this a lend
        // rather than a second economy is that it is *bounded*, and a cap on the
        // carriers would leave every generalist free to cross for the same
        // store. The tier puts this Refill below every sink at home.
        | Refill structureId when Set.contains structureId ferrySinks ->
            Capacity.total (Map.tryFind structureId ferryShare |> Option.defaultValue 0)
        // A borrowed Upgrade takes the bodies hired for it and no more (#213):
        // `Tuning.PioneerCount`, the same constant the worker row is raised by,
        // so a human retuning the hire retunes the lift with it.
        | Upgrade controllerId when isBorrowedUpgrade view controllerId ->
            Capacity.total view.Tuning.PioneerCount
        | Build siteId ->
            // The body standing on the site is outside the builders' budget
            // (#205): every word of that number's argument is about a commute,
            // and this body costs the home room neither a walk nor a surplus
            // tick.
            let exempt = Atlas.postSiteTile atlas siteId |> Option.toList |> Set.ofList

            // A bootstrapped child's site in the mother's pool, per site: the
            // same bodies that were hired for the room, on the site that ends
            // its window sooner than its controller does. The child's own room
            // reads no cap here — its own sites are its own workers' to crowd.
            //
            // And the crowd the budget rations is the list it lifted, with
            // nothing left to subtract from it (#266): the rooms whose sites
            // the budget does not reach — a [[nursery]]'s, a bootstrapping
            // child's, a borrowed room's — are the rooms whose sites never
            // entered the queue, so what the cap covers and what the tier
            // lifts are one list read twice.
            let total =
                if isBorrowedSite siteId then Some view.Tuning.PioneerCount
                elif Set.contains siteId fedSiteIds then Some builderShare
                else None

            { Capacity.unbounded with
                Total = total
                Exempt = exempt
            }
        // A rescue is one body's trip (#284, `rescued`). Every other Repair is
        // uncapped, as it always was: a road under the spawn is worked by
        // whoever is standing over it.
        | Repair id when Set.contains id rescued -> Capacity.total 1
        | _ -> Capacity.unbounded

    tasks
    |> List.map (fun task ->
        {
            Task = task
            Priority = priorityOf task
            Capacity = capacityOf task
            // Work in a room another colony of ours runs (ADR 0047 decision 4).
            // One gate reads it, and only on the Upgrade: a [[standing body]]
            // holds no commuting work (ADR 0046), and a Seam crossing is the
            // longest commute the colony has, so the lift that sends the
            // pioneers must not send the home upgraders after them.
            Borrowed =
                match task with
                | Upgrade controllerId -> isBorrowedUpgrade view controllerId
                | Build siteId -> isBorrowedSite siteId
                | _ -> false
        })
