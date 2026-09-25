/// The Pool: every Task priced and tiered for the tick, the one place a Task
/// kind is turned into a number. The restock, garrison and spare-rate rules
/// behind a Post, the safety tiers, and `planPool`.
[<AutoOpen>]
module Fabot.Core.Decide.Pool

open Fabot.Core
open Fabot.Core.Types

/// Ticks until a source restocks, 0 while it holds energy — and 0 for a
/// source the view does not carry at all, so a source nothing projects never
/// holds a decision up.
let internal ticksToRestock (view: ColonyView) sourceId =
    view.Sources
    |> List.tryFind (fun s -> s.Id = sourceId)
    |> Option.map (fun s -> s.TicksToRestock)
    |> Option.defaultValue 0

/// ADR-0024. Whether a creep garrisons a source's container Post: a
/// Work-heavy body standing on that source's built container.
let internal garrisons atlas (creep: CreepInfo) sourceId =
    Atlas.workHeavy atlas creep.Name
    && Atlas.catchesOverflow atlas creep.Name sourceId

/// Every Work-heavy body of the colony beside the tile it is standing on — the
/// garrison census, a fact about where a body is and not about what it holds.
/// `workHeavy` and not `bodyClassOf`, which tests `isGuardBody` first: a body
/// carrying ATTACK beside `Work > Move` is a Fighter there and heavy here. No
/// row this colony casts is both.
let private heavyStanders (view: ColonyView) atlas : (CreepInfo * RoomPos) list =
    view.Creeps
    |> List.choose (fun creep ->
        if Atlas.workHeavy atlas creep.Name then
            Atlas.creepTile atlas creep.Name |> Option.map (fun tile -> creep, tile)
        else
            None)

/// Whether a source's rate still outruns what the bodies garrisoning it take —
/// whether a rock has a dig left in it worth a walk (#235: live at t199,88x a
/// worker with nine free walked a Seam for one dig on a rock a six-Work Anchor
/// was already draining).
///
/// The rate and not `sourceOutputOf`, the quota's number capped at the row's
/// cast: read here, a colony too poor to cast a saturating Anchor would read
/// its own half-worked rock as spent. The garrison side is the living bodies,
/// not the cast, for the same reason from the other end. Zero garrison on an
/// unposted rock and on a vacant Post, so both stay open — the safety valve
/// that keeps a colony whose Anchor has just died from starving. A rate the
/// projection cannot price is no evidence of saturation.
///
/// The standing Posts and not `Atlas.postsOf`: a container site is a garrison
/// place and not yet an economy — its Anchor's dig goes into construction
/// progress, and there is no container beside the rock to Withdraw from — so a
/// rock closed at the site stage would leave the light row with no Feeding
/// intake at all while both home Posts are being raised.
///
/// Read in `applicable` and not as a `CapScope.Commuters` of zero: a capacity
/// never evicts a body already holding a Task, and #235's case (b) is an
/// eviction — the outpost Anchor stands up and the squatting light body has to
/// be released that tick.
let internal hasSpareRate (view: ColonyView) atlas (sourceId: string) =
    let posts = Atlas.standingPostsOf atlas sourceId

    let dug =
        if Set.isEmpty posts then
            0
        else
            // The heavy standers alone: a light body on a Post is squatting
            // it, not working it. Where two rocks share a Seat the dig is
            // charged to both — `Atlas.postsOf`'s own ambiguity.
            heavyStanders view atlas
            |> List.sumBy (fun (creep, tile) ->
                if Set.contains tile posts then
                    (creep.Body |> Map.tryFind Work |> Option.defaultValue 0)
                    * Engine.harvestPerWork
                else
                    0)

    sourceRateOf view atlas sourceId |> Option.forall (fun rate -> rate > dug)

/// ADR-0048. Whether a source has a Post with no garrison standing on it —
/// whether the walk this body is about to make ends on a tile it can have.
/// A heavy body alone mans a Post; the candidate never counts against itself.
///
/// The rock's Post and not its standing one alone: the question here is
/// standing room, and a Post whose container is still a site is a tile a body
/// stands on and raises. Read off the standing census, a site Post with its
/// garrison on it would read vacant, and a second body would walk onto it.
///
/// Read now and not at arrival, which is where this parts from every other
/// count of a Post: the arrival discount is safe for the body a cast was aimed
/// at, and this gate cannot tell one of those from a released squatter (live
/// at 204,966 an Anchor a border away took the walk home on a Post whose
/// garrison outlived its arrival by hundreds of ticks). The price is a full
/// body whose incumbent is genuinely expiring, which waits where it stands.
let internal hasUnmannedPost (view: ColonyView) atlas (creep: CreepInfo) (sourceId: string) =
    let posts = Atlas.postsOf atlas sourceId

    if Set.isEmpty posts then
        true
    else
        // The candidate never counts against itself, which is this gate's own
        // clause and not the census's.
        let manned =
            heavyStanders view atlas
            |> List.choose (fun (stander, tile) ->
                if stander.Name <> creep.Name then Some tile else None)
            |> Set.ofList

        posts |> Set.exists (fun tile -> not (Set.contains tile manned))

/// Whether a Work-heavy body holds a source through its empty window: the
/// empty-source reprieve, over the source's whole digging range and not the
/// container alone — a hauler drawing the container swaps the Anchor onto the
/// Seat beside it.
let private keepsThroughEmptyWindow atlas (creep: CreepInfo) sourceId =
    Atlas.workHeavy atlas creep.Name
    && Atlas.standsAtSource atlas creep.Name sourceId


/// The ticks a Task waits on a restock before there is anything there to work
/// — the one place the question is asked, so `tooEarly` and
/// `Emitter.actionIntents` cannot disagree about which Tasks wait at all.
/// Exhaustive on purpose: a Task added to the union is a build error here.
let internal restockWait (view: ColonyView) task =
    match task with
    | Harvest sourceId -> ticksToRestock view sourceId
    | Withdraw _
    // A pile moves — down by decay, up under an anchor spilling onto a full
    // container — but neither direction is a restock.
    | Pickup _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    | Reserve _
    | Claim _
    | Reclaim _
    | Guard _
    | Flee -> 0

/// ADR-0025. The walk and the wait that hold a Task up for this creep, or None
/// when its time has come: a drained source's Harvest is applicable only when
/// the creep's walk covers the restock wait. No slack, because the wait
/// shrinks by one each tick while the walk stays put. One rule for both bodies
/// (#258): how many ticks a tile costs this body is already in the walk, and
/// whether the Post at the end is free is the cap's question, not this one's.
let internal tooEarly (view: ColonyView) atlas (creep: CreepInfo) task (walk: Lazy<int option>) =
    match task, restockWait view task with
    // A wait of zero is covered by every walk. Asked first, which keeps the
    // reprieve's Atlas joins off the pairs a stocked pool is mostly made of.
    | _, 0 -> None
    | Harvest sourceId, wait when not (keepsThroughEmptyWindow atlas creep sourceId) ->
        match walk.Value with
        // No walk at all is unreachable geometry, which is not earliness: the
        // reachability gate stands ahead of this one in both cascades.
        | Some ticks when ticks < wait -> Some(ticks, wait)
        | _ -> None
    | _ -> None

/// ADR-0056. Whether a Task stands in the Safety tier: Flee and Guard. A
/// predicate, because the two rules that turn on it (`areaFor` skipping the
/// Reach subtraction, `threatened` written beneath it) are asked before
/// `planPool` ranks anything, and `tierOf` ranks exactly these two into
/// `Safety`. Exhaustive on purpose.
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
    | Claim _
    | Reclaim _ -> false

/// The room a Task's Work Area lies in: its target's, so the Reach taken out of
/// it is that room's share. None for Flee, whose area is the creep's own
/// room's, and for a target the projection does not place. A Guard names its
/// room outright; neither caller asks about the Safety tier, but the match is
/// exhaustive.
let private roomOfWork atlas task =
    match task with
    | Harvest id
    | Build id
    | Repair id
    | Upgrade id
    | Reserve id
    | Claim id
    | Reclaim id -> Atlas.targetRoom atlas id
    | Pickup(id, _)
    | Withdraw(id, _)
    | Refill(id, _) -> Atlas.targetRoom atlas id
    | Guard room -> Some room
    | Flee -> None

/// Whether a tile stands in the Reach on a Task's own room, or None when the
/// question does not arise — the Safety tier, and a tick with no Reach
/// anywhere. The room is the Task's and never the creep's: a body a border
/// away is judged against the ground it is walking to (#138).
///
/// A predicate and not the room and the grid, because its two readers want it
/// in opposite polarity: `areaFor` thins an area by it and `threatened` asks
/// whether it has taken the area whole.
let private reachOnWork (threats: Threats) atlas task : (RoomPos -> bool) option =
    if safetyTier task || Map.isEmpty threats.Reach then
        None
    else
        let room = roomOfWork atlas task

        let reach =
            room |> Option.map (Threats.reachIn threats) |> Option.defaultValue Set.empty

        Some(fun tile -> Some tile.Room = room && Set.contains (RoomPos.pos tile) reach)

/// ADR-0033. The tiles a creep may work a Task from this tick: its Work Area
/// less its room's Reach — and for Flee, the safe set of the room the creep
/// stands in. The subtraction is skipped for the whole Safety tier, whose
/// areas are derived off `Threats` themselves: taking the Reach out again is
/// a tautology for Flee and the end of the Task for a Guard. The exemption is
/// the predicate, so a third Task ranked into Safety is exempt here without
/// this function being touched.
let internal areaFor (threats: Threats) atlas creep task : Set<RoomPos> =
    let ground =
        match task with
        | Flee ->
            Atlas.creepRoom atlas creep
            |> Option.map (Threats.safeIn threats)
            |> Option.defaultValue Set.empty
        // The guard's ring, in the room the Planner keyed the Task on — with
        // one fallback the room's blindness forces (#366): a Guard is pooled
        // for an outpost the colony remembers a raid in, and a room nothing of
        // ours stands in has no Threat, so no ring. The declared source tiles
        // give the walk a destination; the instant the guard arrives the room
        // is lit and this branch is not taken again.
        //
        // In a declared errand room the ground is `Threats.ErrandRing`'s (#414).
        | Guard room ->
            match Threats.errandRingIn threats room, Threats.ringIn threats room with
            | Some ground, _ -> ground
            | None, ring when Set.isEmpty ring -> Atlas.sourceRingIn atlas room
            | None, ring -> ring
        | _ -> Atlas.workAreaFor atlas creep task

    match reachOnWork threats atlas task with
    | None -> ground
    | Some hot -> ground |> Set.filter (hot >> not)

/// Whether a creep may act on a Task from the tile it is standing on this tick
/// — `Atlas.mayAct` over the ground `areaFor` has just thinned. Joined because
/// every caller asks both.
let internal mayActNow (threats: Threats) atlas (creep: string) task =
    Atlas.mayAct atlas creep task (areaFor threats atlas creep task)

/// The travel cost of a Task for a creep, priced over the tiles it may
/// actually work from this tick, so a candidate whose cold remainder is walled
/// off is rejected as unreachable instead of being held and never worked. An
/// area that is empty here was never taken by the Reach (the threat gate
/// stands ahead of this one), so it falls back to the Task's own price, which
/// carries the unplaceable-target escape and the Seam join for a target in
/// another room.
///
/// The Guard is priced off its area, for Flee's reason: there is no target to
/// fall back to, and an empty ring is honestly nowhere to stand. It crosses a
/// border where Flee never has to, so it prices through `travelCostToward`,
/// the same Seam-band minimum every cross-room Task is ranked by.
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

/// Whether the Reach has taken the whole of a Task's Work Area: it had
/// somewhere to stand and has nowhere left. An area that was empty to begin
/// with is not threatened: that is the reachability gate's answer.
///
/// The area read is the target room's, and never the creep's share of it
/// (#147): `workAreaFor` is empty across a border by construction, so read
/// through it the rule never fired for a body still walking — a home worker
/// crossed to an outpost Harvest whose every Seat was inside a Reach and was
/// released `NoneApplicable` beside the invader. `workAreaAcross` asks one
/// question the same way whichever side of the seam the body is on.
///
/// Written beneath the Safety tier: a guard's Work Area is a ring of Reach
/// tiles, so read by its ground every Guard would be threatened on every tick
/// one exists. Today neither Safety Task has a Work Area in the atlas, so the
/// clause changes no answer; it keeps the rule right on the day one does.
let internal threatened (threats: Threats) atlas (creep: CreepInfo) task =
    // The negation of the join `areaFor` makes.
    match reachOnWork threats atlas task with
    | None -> false
    | Some hot ->
        let area = Atlas.workAreaAcross atlas creep.Name task

        not (Set.isEmpty area) && Set.forall hot area

/// Whether the creep itself is standing where it can be hurt: its own tile
/// inside a Reach of its own room. Flee's applicability, and the vision
/// grace's one gate. A creep the projection cannot place stands in no Reach.
let internal standsInReach (threats: Threats) atlas (creep: string) =
    match Atlas.creepTile atlas creep with
    | Some tile -> Set.contains (RoomPos.pos tile) (Threats.reachIn threats tile.Room)
    | None -> false

/// Whether the room a construction site stands in satisfies a rule — the room
/// join every site predicate below makes, read off the projection so a room a
/// stand-down drops from the scan set leaves this reading with it. An
/// unplaced site names no room and answers false, the ordinary surplus Build.
/// Written once: spelled out at each site, one of the five had come to route
/// an unplaced site through the sentinel room name `""`.
let private siteRoomIs atlas (rule: string -> bool) siteId =
    Atlas.targetRoom atlas siteId |> Option.exists rule

/// Whether a construction site stands in a room this colony mines — one the
/// outpost builders' budget may ration. The room half alone (#266): out there
/// a human paves too, and 45 hand-laid road sites in W13S29 stood at 0/300 for
/// as long as they were surplus. The kind survives as the order the budget is
/// spent in. A claimed room a human still names in the outpost list is not
/// `Borrowed` and answers true here; `planPool` narrows it again.
let private isOutpostSite (view: ColonyView) atlas siteId =
    siteRoomIs
        atlas
        (fun room ->
            room <> SpatialInfo.homeName view.Spatial
            // A borrowed room's site is the child's own and not an outpost's
            // (user decision 2026-09-07): it neither draws the budget nor
            // dilutes it.
            && not (List.contains room view.Borrowed.Rooms))
        siteId

/// Whether a construction site stands in a nursery — a room this colony has
/// claimed and not yet stood a spawn in, where every site is feeding-tier
/// outright.
let private isNurserySite (view: ColonyView) atlas siteId =
    siteRoomIs atlas (isNurseryRoom view) siteId

/// Whether a room is bootstrapping as seen from this colony's tick: a child of
/// ours running its own spawn (the mother's reading), or this colony's own
/// home at the `Bootstrapping` stage (the child's own reading). The home half
/// reads the stage and not a level of its own.
let private isBootstrappingRoom (view: ColonyView) room =
    isBootstrapRoom view room
    || (room = SpatialInfo.homeName view.Spatial && homeStage view = Some Bootstrapping)

/// A site standing in a bootstrapping room: feeding-tier in both pools (user,
/// 2026-09-06). What a room under RCL3 builds is its containers and its
/// extensions, and the extensions are the bank — 300 to 550 doubles the Anchor
/// body — so they come before the controller.
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

/// The two of those rules that turn on the **room** a site stands in: a site
/// in a [[nursery]], and one in a room this colony is bootstrapping. Read on
/// its own by the outpost budget below, which is deciding the third rule and
/// so cannot be asked it.
let private isFeedingByRoom (view: ColonyView) atlas siteId =
    isNurserySite view atlas siteId || isBootstrappingSite view atlas siteId

/// Whether this Build is on the feeding tier rather than in the surplus: the
/// outpost sites the builders' budget has picked out this tick (handed in,
/// because which they are is a fact about the whole queue), and every site in
/// a nursery or a bootstrapping room.
let private isFeedingSite (view: ColonyView) atlas (fed: Set<string>) siteId =
    Set.contains siteId fed || isFeedingByRoom view atlas siteId

/// Whether a site stands in this colony's own home room — the room the
/// surplus rung (#234) is scoped to. `Option.forall` and not `siteRoomIs`'
/// `Option.exists`: its totality resolves the other way, an unplaced site
/// reading as home rather than as elsewhere.
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

/// ADR-0007. The hard deadline on the controller's downgrade timer: half the
/// level's full timer. The engine refuses activateSafeMode once the timer
/// sinks below half minus 5,000 (its grace), so escalating at half keeps the
/// safe-mode reflex fireable with the whole grace still banked.
let private downgradeDeadline level = fullDowngradeTimer level / 2

/// Whether the controller stands inside its downgrade deadline.
let private insideDowngradeDeadline (view: ColonyView) =
    view.Controller
    |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

/// ADR-0010. The tier of work a Task belongs to, once its target is taken into
/// account — the ladder `planPool` sets each entry's priority off. Exported
/// with the constants and `Rung` below because the ladder is one fact, and
/// the test that walks every rank and rung of it (#237) reads it here.
type Tier =
    /// Flee and Guard: above every other tier and above the downgrade
    /// deadline too, because no other work matters while a creep is being
    /// killed.
    | Safety
    /// Feeding the economy: Harvest, a container's Withdraw, the Refill of a
    /// spawn or an extension, Reserve, Claim, the outpost sites the builders'
    /// budget has picked out this tick, and every site in a nursery.
    | Feeding
    /// The Storage's Withdraw: the colony's stock as an intake, one tier
    /// below the source containers the flow fills.
    | StockDraw
    /// Surplus work: a tower Refill, Build, Repair and Upgrade.
    | Surplus
    /// The controller container's Refill: filled by bodies with no surplus
    /// work of their own.
    | UpgradeBuffer
    /// The Storage's Refill: deeper than every sink that spends.
    | Stock

/// How far apart two tiers stand on the priority ladder. Ten and not one, so
/// that a Task can be ordered against another inside its tier
/// (`priorityStep`). A rung must stay inside the half-tier the Resolver rounds
/// by, or it buys the Task a push weight its own tier does not have (#237):
/// `weightOfRank` rounds a rank to its nearest tier and gives a tie to the
/// deeper one, so a rung up may be `tierRungs / 2` at the most, and a rung
/// down one less. `Rung` is the vocabulary that keeps the two in step.
let tierRungs = 10

/// The whole tier order, shallowest first — the one place the ordering lives.
/// Exhaustive over Tier on purpose. The downgrade deadline is the one thing
/// above the sequence rather than in it.
let priorityOfTier =
    function
    // One tier beneath `deadlineRank`'s: a fleeing creep outbids even a
    // controller about to downgrade.
    | Safety -> -2 * tierRungs
    | Feeding -> 0
    | StockDraw -> tierRungs
    | Surplus -> 2 * tierRungs
    | UpgradeBuffer -> 3 * tierRungs
    | Stock -> 4 * tierRungs

/// One tier above the shallowest tier of work: where the downgrade deadline
/// puts Upgrade. Not a tier of its own — an ordering imposed on the sequence.
/// Exported for `Tier`'s reason (#237).
let deadlineRank = -tierRungs

/// The step a Task is moved by when it is ordered against another inside one
/// tier: one rung of ten, so it never crosses a tier.
let priorityStep = 1

/// The rungs a Task may be stepped by inside its tier, a union rather than an
/// int for the reason `Tier` is one (#237): the Resolver's `weightOfRank` has
/// to round every one of them back onto the tier's own push weight, and the
/// test that checks that walks the cases off the union. A rung always steps a
/// Task up, so the ranks below are negative.
type Rung =
    | OnTheTier
    | OneRungUp
    | TwoRungsUp

/// What a rung is worth on the ladder, exhaustive over `Rung` on purpose.
let rankOfRung =
    function
    | OnTheTier -> 0
    | OneRungUp -> -priorityStep
    | TwoRungsUp -> -2 * priorityStep

/// ADR-0006. Which of the four shapes a body is, as far as a capacity is
/// concerned: part arithmetic, asked in the order the gates ask it in, because
/// Heavy and Standing overlap on the anchor's `6W/1C/1M` and every rule that
/// reads both reads the heavy one first. `Fighter` first of all: a guard
/// carries no Work, so the three classes below would answer `Light`. Exported
/// because the ladder is a body fact a test reads directly.
let bodyClassOf (tuning: Tuning) atlas (creep: CreepInfo) : BodyClass =
    if isFighterBody creep then Fighter
    elif Atlas.workHeavy atlas creep.Name then Heavy
    elif isStandingBody tuning creep then Standing
    else Light

/// Planner, second half: this tick's pool with each entry's priority and
/// capacity on it. `planTasks` says what is pooled; this says where each
/// entry ranks and how many bodies it admits, and between them they are
/// everything the Matcher knows about a Task. Every exception the colony has
/// learned about ordering and crowding lands here and nowhere else.
let planPool (view: ColonyView) atlas (tasks: Task list) : PooledTask list =
    let bank = view.Bank.Capacity

    // The three loads a store is divided by, each the row's own cast at the
    // richest bank and never a candidate's own carry: a capacity is a fact
    // about the Task, so one store must not answer two numbers depending on
    // which creep asked — except by body class, on purpose.
    let haulerLoad = carryCapacityOf (bodyFor haulerPattern bank)
    let workerLoad = carryCapacityOf (workerBodyFor bank)
    let standingLoad = carryCapacityOf (bodyFor upgraderPattern bank)

    let buffers = Atlas.controllerContainers atlas

    // The refill cluster, off the one value its readers share: the Atlas laid
    // it at construction, `planTasks` pooled the spawn off it, this bounds it,
    // and the Atlas lays its Work Area off it.
    let cluster = Atlas.cluster atlas

    let isStorage id =
        Map.tryFind id view.Spatial.TargetKinds = Some(Structure BuiltKind.Storage)

    // Whether an object stands in a room this colony declared an errand in —
    // the Reactor's own room, where ore on the floor is the delivery's
    // business and not the stock's.
    let errandRooms = Facts.errandRooms view

    let besideTheReactor id =
        SpatialInfo.roomOf view.Spatial id
        |> Option.exists (fun room -> Set.contains room errandRooms)

    // ADR-0071. What the ring can still take, and whether the colony is
    // starved at it: room in the cluster, and a bank that cannot afford the
    // hauler unit it would cast at its own capacity. Two readers below: the
    // Storage draw's tier and its cap.
    let clusterRoom = cluster |> Option.map RefillCluster.free |> Option.defaultValue 0

    let clusterStarved =
        clusterRoom > 0
        && view.Bank.Available < bodyCost (bodyFor haulerPattern view.Bank.Capacity)

    // The ferry's sinks (`ferryBuffers`): what a mother lends a bootstrapping
    // child is written down and bounded.
    let ferrySinks = ferryBuffers view

    // One `Tuning.FerryLoads` budget per child room, spread over that room's
    // buffers in id order (user decision 2026-09-07): the hauler row hires per
    // child, so the pool admits per child.
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

    // The rescue budget (#284): the decaying structures this colony has let
    // fall so far below their own trigger that a repair is no longer surplus
    // work, lifted two rungs over the rest of the tier and given one body
    // apiece. The surplus tier is ordered by travel cost from where a body
    // stands, so the cluster a loaded worker stands in regenerates its own
    // supply of two-tile-away Repairs faster than anybody would walk out of
    // it (live at t239,65x the base roads sat at 50-58% while the trunk north
    // stood at 2% and the outpost's roads at 8%). The decaying kinds alone,
    // ordered by the fraction of max and never the hits, ties by id. One body
    // apiece, because the walk is the expensive half and one load carries a
    // plain road from a quarter to over its whole line.
    //
    // The rescue set deliberately does not read the held fact: it asks which
    // far structure is worth a walk, on hits alone, so a rescue frees its slot
    // at the rescue line while its body works on to the whole line.
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

    // The queue the builders' budget rations: every site the pool holds in a
    // room this colony merely mines and that no other rule already feeds. A
    // nursery's site and a bootstrapping child's are feeding-tier outright
    // and capped by nothing, and neither room is always `Borrowed`; left in
    // the queue they would take places the lift buys them nothing with.
    let outpostSites =
        tasks
        |> List.choose (function
            | Build siteId when
                isOutpostSite view atlas siteId && not (isFeedingByRoom view atlas siteId)
                ->
                Some siteId
            | _ -> None)

    // ADR-0042. The order the budget is spent in: the container sites first,
    // then the nearest to the crossing, ties by id. One queue over every
    // outpost and not one apiece: two rooms' sites are ordered on a walk that
    // leaves the home-side leg off both, so the comparison says "nearer its
    // own crossing" and not "nearer the spawn". A site whose walk cannot be
    // priced sorts last rather than out of the list.
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

    // The budget rations the tier, not just the crowd on it: the first
    // `Tuning.OutpostBuilders` sites in the order above are lifted and the
    // rest stay surplus. The order is asked for only when the budget cannot
    // cover the list — the walk behind it is a flood over the outpost's whole
    // grid, and a colony whose outpost holds one site pays for none of it.
    let fedOutpostSites =
        if List.length outpostSites <= view.Tuning.OutpostBuilders then
            outpostSites
        else
            outpostSites
            |> List.sortBy siteOrder
            |> List.truncate view.Tuning.OutpostBuilders

    let fedSiteIds = Set.ofList fedOutpostSites

    // A budget and not a per-site number: `planOutpostContainers` places a
    // site for every unserved outpost source on the same tick, so a per-site
    // two would be the whole worker row. Spread over the lifted list and
    // never the whole pool (spread over the pool, W13S29's 45 sites took one
    // builder apiece), floored at one apiece. Two, because one is the
    // smallest crowd that builds and two the smallest that survives losing a
    // body: a container is 5,000 progress against a generalist's 50.
    let builderShare =
        match fedOutpostSites with
        | [] -> 0
        | sites -> view.Tuning.OutpostBuilders / List.length sites |> max 1


    // The tier a Task sits in. Refill, Withdraw and Build are the three Tasks
    // whose tier layers by target. On Refill the Storage and the container
    // are each one projected kind and exclude each other by construction,
    // while a tower is read off the Refillables census, which can overlap
    // either — so the kind is asked first, deepest answer first.
    let tierOf task =
        match task with
        | Flee -> Safety
        // The tier holds two Tasks and no ordering between them: Flee is
        // inapplicable to a Fighter and a Guard applicable to nothing else.
        | Guard _ -> Safety
        | Harvest _ -> Feeding
        // A decision made here, because nothing else made it: the ADRs fix the
        // reserver row's casting order and say nothing about its matching
        // order. Reserve joins the feeding tier on the casting order's own
        // argument: it decides whether a room's income is five a tick or ten.
        | Reserve _ -> Feeding
        // A claim decides whether there is going to be a second colony at all.
        | Claim _ -> Feeding
        // The reclaim decides whether the season's whole score accrues to this
        // colony or to a rival. Not Safety tier: nothing out there is killing
        // the body, and a Task ranked there would be offered to it ahead of
        // running from a keeper.
        | Reclaim _ -> Feeding
        // The delivery's own draw is Feeding (#367): that load is the season's
        // score, not stock being topped up. Live at t506,631-507,096 the
        // courier scored the delivery draw at rank 8 while energy hauling
        // scored -2 and 0, hauled energy for 465 ticks, and the Reactor fell
        // 500 -> 47. Told apart by the store, the same test the Emitter's
        // `deliveryDraw` uses, and only while the programme is open.
        | Withdraw(storeId, Thorium) when isStorage storeId && Facts.courierProgrammeOpen view atlas ->
            Feeding
        // Ore lying in the Reactor's own room is the delivery's, not stock:
        // its sink is the Reactor five tiles away (`Planner.reactorRefills`).
        // Ore in a merely crossed room keeps `StockDraw` and its walk home.
        | Withdraw(storeId, Thorium) when besideTheReactor storeId -> Feeding
        | Pickup(pileId, Thorium) when besideTheReactor pileId -> Feeding
        // The mine haul ranks at the Storage's tier: an empty hauler beside
        // the mine must not take the season's ore ahead of the energy the
        // spawn is waiting on. Asked before the kind, because the store it
        // names is a container and would otherwise answer Feeding.
        | Withdraw(_, Thorium) -> StockDraw
        // ADR-0023. The Storage is stock, drawn a tier below the containers.
        // The stock feeds a starved cluster at the flow's own rank: a tie with
        // the containers and not a rank over them, so travel cost sends the
        // body beside the Storage to the Storage, and capped in `capacityOf`
        // at the loads the ring can take.
        | Withdraw(storeId, Energy) ->
            if isStorage storeId then
                if clusterStarved then Feeding else StockDraw
            else
                Feeding
        // A Thorium pile ranks where the Thorium container does: it is that
        // container's next dig, landed on the floor because the store was
        // full. Both arms spelled: a third resource is a build error here.
        | Pickup(_, Thorium) -> StockDraw
        // A pile is flow and not stock: the haul cycle's energy lying where it
        // fell.
        | Pickup(_, Energy) -> Feeding
        | Refill(structureId, _) ->
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
                // The flow; the refill cluster is keyed on a spawn and arrives
                // through the same door.
                Feeding
        | Build siteId when isFeedingSite view atlas fedSiteIds siteId -> Feeding
        // A bootstrapped child's Upgrade, in the mother's pool (#213): the tier
        // the pioneers were hired for. Left in the surplus, travel cost — a
        // Seam and fifty tiles against five — kept every one of them at home.
        | Upgrade controllerId when
            isBorrowedUpgrade view controllerId
            && not (sitesPendingBeside view atlas controllerId)
            ->
            Feeding
        | Build _
        | Repair _
        | Upgrade _ -> Surplus

    // Where the pool's Feeding-tier stores stand, so a pickup can be asked
    // whether one of them is under its own pile. Read off the pool and not
    // off the projection's whole container census: a store the pool holds no
    // Withdraw for is not an alternative to anything.
    let drawableTiles =
        tasks
        |> List.choose (fun task ->
            match task with
            | Withdraw(storeId, _) when tierOf task = Feeding ->
                SpatialInfo.placementOf view.Spatial storeId
            | _ -> None)
        |> Set.ofList

    // The Task's place on the ladder: its tier, with the rungs inside it and
    // the downgrade deadline over it. The deadline lifts the colony's own
    // controller and no other: the pool can hold a bootstrapped child's
    // Upgrade too, and lifting that one on the mother's timer would send her
    // loaded fleet across the Seam on the tick her own controller was closest
    // to downgrading.
    let priorityOf task =
        let tier = tierOf task

        // Which rung inside that tier, never a rank: a rung is a `Rung` case
        // so the Resolver's half-tier rounding is checked against every one
        // (#237).
        let step =
            match task with
            // The energy pile's two rungs. A pile lying on a drawable store
            // outranks every other Feeding container (a hauler beside a full
            // container ignored the pile on it; stepping the Withdraw down
            // instead demoted a full container behind every store at any
            // distance). And a pile worth a trip of its own (#242): half a
            // hauler load, read off the row's cast and never the candidate's
            // carry — the creep-blind mirror of `applicable`'s `worthTheTrip`.
            // Below a bank of 450 half a load is `Tuning.PickupThreshold`
            // itself, so every energy pile takes the rung. Smaller piles stay
            // on the tier: one hundred energy forty tiles off is no reason to
            // leave the 1,500 under a body's feet. `drawableTiles` holds only
            // Feeding-tier Withdraws, so the ore's arm below inherits neither
            // clause.
            | Pickup(pileId, Energy) ->
                let overADrawableStore =
                    SpatialInfo.placementOf view.Spatial pileId
                    |> Option.exists (fun tile -> Set.contains tile drawableTiles)

                let worthATripOfItsOwn = stored pileId * 2 >= haulerLoad

                if overADrawableStore || worthATripOfItsOwn then
                    OneRungUp
                else
                    OnTheTier
            // A full source container, whose income is going away: two rungs,
            // above the pile, because with the pile above the full container
            // the haulers chased fifty-energy piles all day and never drew the
            // 2,000 beside them. Source containers alone: the buffer and the
            // Storage are sinks. The resource is spelt out so this arm and the
            // ore's below are visibly disjoint.
            | Withdraw(storeId, Energy) when
                tier = Feeding && stored storeId >= Engine.containerCapacity
                ->
                TwoRungsUp
            // The same rule one column over (#306): a full mineral container
            // sends the next dig onto the floor too, and charges a second time
            // for it in the miner's life past the contact cliff. The cliff and
            // not the cap, on the arithmetic: lifted at the cap the miner
            // averages `1 + p ≈ 3.67` a tick of ageing, at the cliff ≈3.0,
            // which is the band `Tuning.MineContactAgeing`'s three is written
            // for. What the lift steps over is the Storage's own energy
            // Withdraw, and the bound on how many bodies leave the energy
            // rotation is the pool's own pair of capacities, widest exactly
            // when the mine has backed up (#315). The delivery draw from
            // Storage inherits this lift deliberately: a warehouse holding at
            // least the cliff has more than one delivery available.
            | Withdraw(storeId, Thorium) when
                SpatialInfo.heldIn view.Spatial Thorium storeId >= view.Tuning.MineContactCliff
                ->
                TwoRungsUp
            // The ore in a store that ends, one rung up (#359): going away
            // faster than a pile, if anything, since a decayed tombstone drops
            // its whole store as piles that then bleed. Rungless it shares
            // `StockDraw` with the Storage's energy Withdraw and loses every
            // travel-cost tie to the bank at home. One rung and not two: two
            // would put it over the mine's own full container, and draining
            // the container is what stops the floor filling.
            | Withdraw(storeId, Thorium) when
                Map.tryFind storeId view.Spatial.TargetKinds = Some Tombstone
                ->
                OneRungUp
            // And the floor under it, one rung lower, for the same reason.
            // Unconditional, on a sentence about the resource and not the
            // tile: ore on the floor is going away and nothing else on this
            // tier is, and the colony has no second copy of season score.
            // Neither of the energy pile's clauses is inherited: a mineral
            // container's tile is not in `drawableTiles`, and a hundred-unit
            // pile fails the worth-a-trip line at every bank the extractor
            // stands at. Nor need the pile lie on the container: a hauler that
            // dies mid-route leaves one on a road tile, and it takes this rung
            // too.
            | Pickup(_, Thorium) -> OneRungUp
            // A site outranks the controller inside the surplus tier (#234):
            // a worker that fills at the buffer is already standing in the
            // controller's Work Area, so Upgrade costs it nothing and never
            // goes task-gone.
            | Build siteId when tier = Surplus && isHomeSite view atlas siteId -> OneRungUp
            // Over the home site as well (#284): a structure a quarter from
            // destruction is work the colony has already paid for.
            | Repair id when Set.contains id rescued -> TwoRungsUp
            | _ -> OnTheTier

        match task with
        | Upgrade id when
            insideDowngradeDeadline view
            && view.Controller |> Option.exists (fun c -> c.Id = id)
            ->
            deadlineRank
        | _ -> priorityOfTier tier + rankOfRung step

    let isBorrowedSite siteId =
        siteRoomIs atlas (isBootstrapRoom view) siteId

    // How many bodies the Task admits, and of which shapes.
    let capacityOf task =
        match task with
        // A deposit's is one Garrison, the Post count, and nothing else: one
        // tile can be dug from, the container, and every other Seat is a tile
        // a store-less body drops the Thorium on the ground from. Written as
        // a cap of zero rather than left off: absence is unbounded here.
        | Harvest rockId when Atlas.isMineral atlas rockId ->
            let postTiles = Atlas.postsOf atlas rockId
            let posts = Set.count postTiles

            Capacity.unbounded
            |> Capacity.cappingMaybe CapScope.Garrisons (if posts = 0 then None else Some posts)
            |> Capacity.capping CapScope.Commuters 0
            |> Capacity.garrisoning postTiles
        // ADR-0051. Harvest is three numbers over one source: the Seat count
        // every harvester shares, the Post count only the garrisons compete
        // for, and the Seats beyond the Posts the light bodies are left. A
        // source with no Post derives neither of the last two. Beside the
        // numbers, the tiles: every Post of the rock is held by the body
        // standing on it whatever Task it holds this tick.
        | Harvest sourceId ->
            let seats = Atlas.seats atlas sourceId
            let postTiles = Atlas.postsOf atlas sourceId
            let posts = Set.count postTiles

            Capacity.unbounded
            |> Capacity.cappingMaybe CapScope.Everyone seats
            |> Capacity.cappingMaybe CapScope.Garrisons (if posts = 0 then None else Some posts)
            |> Capacity.cappingMaybe
                CapScope.Commuters
                (seats
                 |> Option.filter (fun _ -> posts > 0)
                 |> Option.map (fun n -> max 0 (n - posts)))
            |> Capacity.garrisoning postTiles
        // The guards that room wants and nobody else at all: `guardsWanted`
        // is the row's own arithmetic, read here a second time so the number
        // the cascade hires against and the number the Matcher counts holders
        // against are one number.
        //
        // One over in an errand room (#414): its ranger is resident, and its
        // relief — cast at the incumbent's lead — must take the Task and walk
        // three crossings while the incumbent still holds the ring. A Guard has
        // no arrival price for a handover window to be read against, and the
        // row's count, not this cap, is what buys bodies.
        | Guard room when Set.contains room (Facts.errandRooms view) ->
            Capacity.fighters (rangersWanted view room + 1)
        | Guard room -> Capacity.fighters (guardsWanted view room)
        // One holder per controller: a second body there buys nothing.
        | Reserve _
        | Claim _ -> Capacity.total 1
        // ADR-0069. One permanent holder per reactor, with a handover window:
        // a fresh second resident still buys nothing, while a relief that
        // arrives with exactly this much incumbent life is admitted.
        | Reclaim _ -> Capacity.total 1 |> Capacity.handingOver view.Tuning.ReclaimerOverlap
        // The delivery's draw admits one body, not one per load (#367): the
        // general arm below divides the store by the load, which for the
        // Reactor's own draw was 34,876 T / 500 = 69 holders at the top of the
        // Feeding tier — every idle Carrier in the colony could take 500 T
        // three rooms out while the spawn cluster went empty (live W15S28
        // stood at 215 of 8,300 with two rows unhired). Read through the same
        // two facts the tier is, so the cap and the rank cannot disagree.
        | Withdraw(storeId, Thorium) when isStorage storeId && Facts.courierProgrammeOpen view atlas ->
            Capacity.total 1
        // The starved cluster's draw on the stock admits the loads the ring
        // can take, not the loads the Storage divides into — #367's hazard on
        // the energy column otherwise. At least one, or the lift would be a
        // rank on a Task nobody may hold.
        | Withdraw(storeId, Energy) when clusterStarved && isStorage storeId ->
            Capacity.total (max 1 (ceilDiv clusterRoom haulerLoad))
        // One body fetches what is lying beside the Reactor: the errand room
        // is three crossings out and the ore is a finite remainder. Above the
        // general arm, which would divide a tombstone's holding into loads.
        | Withdraw(storeId, Thorium) when besideTheReactor storeId -> Capacity.total 1
        // Capped by its store's stock of the resource it names (#161):
        // `ceil(stored / one drawer's load)`, since the matching key puts cost
        // ahead of crowding and a container holding 400 would otherwise draw
        // five haulers while a full one across the room stands unvisited. The
        // buffer divides twice (#196): its drawers are the two Work rows,
        // carrying 450 and fifty, and one divisor admits either two in total
        // or eighteen. Each class counts against its own, with no `Total`.
        | Withdraw(storeId, resource) ->
            let stock = SpatialInfo.heldIn view.Spatial resource storeId

            if Set.contains storeId buffers then
                Capacity.unbounded
                |> Capacity.capping CapScope.Standing (ceilDiv stock standingLoad)
                |> Capacity.capping CapScope.Generalists (ceilDiv stock workerLoad)
            else
                Capacity.total (ceilDiv stock haulerLoad)
        | Pickup(pileId, Thorium) when besideTheReactor pileId -> Capacity.total 1
        // Read down the resource's own column like the Withdraw above it.
        | Pickup(pileId, resource) ->
            Capacity.total (ceilDiv (SpatialInfo.heldIn view.Spatial resource pileId) haulerLoad)
        // The refill cluster is bounded by what it can still hold, as a budget
        // the holders' loads are counted against and not a count of bodies:
        // `ceil(free / one hauler load)` was one body for any ring under a
        // load and a half of room, and which body was whoever got there
        // first — a worker carrying fifty from across the room as readily as
        // the courier beside the Storage with 718 aboard.
        | Refill(spawnId, _) when cluster |> Option.exists (fun c -> c.Spawn = spawnId) ->
            Capacity.unbounded |> Capacity.budgeting clusterRoom
        // The lend, bounded: `Tuning.FerryLoads` bodies at the child's buffer
        // and no more, the same number the hauler row was raised by. A `Total`
        // and not the hauler class's share alone: a cap on the carriers would
        // leave every generalist free to cross for the same store.
        | Refill(structureId, _) when Set.contains structureId ferrySinks ->
            Capacity.total (Map.tryFind structureId ferryShare |> Option.defaultValue 0)
        // A borrowed Upgrade takes the bodies hired for it and no more (#213):
        // `Tuning.PioneerCount`, the same constant the worker row is raised by.
        | Upgrade controllerId when isBorrowedUpgrade view controllerId ->
            Capacity.total view.Tuning.PioneerCount
        | Build siteId ->
            // The body standing on the site is outside the builders' budget:
            // that number prices a commute, and this body made none.
            let exempt = Atlas.postSiteTile atlas siteId |> Option.toList |> Set.ofList

            // A bootstrapped child's site in the mother's pool takes the
            // bodies hired for the room; the child's own room reads no cap
            // here. The crowd the budget rations is the list it lifted, so
            // what the cap covers and what the tier lifts are one list read
            // twice.
            let total =
                if isBorrowedSite siteId then Some view.Tuning.PioneerCount
                elif Set.contains siteId fedSiteIds then Some builderShare
                else None

            Capacity.unbounded
            |> Capacity.cappingMaybe CapScope.Everyone total
            |> Capacity.exempting exempt
        // A rescue is one body's trip (`rescued`). Every other Repair is
        // uncapped: a road under the spawn is worked by whoever is standing
        // over it.
        | Repair id when Set.contains id rescued -> Capacity.total 1
        | _ -> Capacity.unbounded

    tasks
    |> List.map (fun task ->
        {
            Task = task
            Priority = priorityOf task
            Capacity = capacityOf task
            // Work in a room another colony of ours runs. One gate reads it:
            // the lift that sends the pioneers must not send the home
            // upgraders after them.
            Borrowed =
                match task with
                | Upgrade controllerId -> isBorrowedUpgrade view controllerId
                | Build siteId -> isBorrowedSite siteId
                | _ -> false
        })
