/// The Pool: every Task priced for the tick (`Price`), the one place a Task
/// is turned into a number. The restock, garrison and spare-rate rules
/// behind a Post, the safety tiers, and `planPool`.
[<AutoOpen>]
module Fabot.Core.Decide.Pool

open Fabot.Core
open Fabot.Core.Types

/// Ticks until a source restocks, 0 while it holds energy — and 0 for a
/// source the view does not carry at all, so a source nothing projects never
/// holds a decision up.
let internal ticksToRestock (view: ColonyView) sourceId =
    view.Sources @ view.BorrowedSources
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
    | Dismantle _
    | Guard _
    | Defend _
    | Assault _
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

/// Whether a body walking into a child's room (a [[nursery]] or a
/// bootstrapping child) for this Task lives to be of use there: the walk, and
/// for work done in the room `Tuning.PioneerStay` beyond it (`childStay`). A
/// body already in the room, or a Task in no child's room, is not asked.
let internal outlivesTheStay
    (view: ColonyView)
    atlas
    (creep: CreepInfo)
    task
    (walk: Lazy<int option>)
    =
    match childStay view atlas creep task with
    | Some stay -> outlivesWalk creep stay walk.Value
    | None -> true

/// ADR-0056. Whether a Task stands in the Safety tier: Flee, Guard, Defend
/// and Assault. A predicate, because the rules that turn on it (`areaFor`
/// skipping the Reach subtraction, `threatened` written beneath it, the
/// Matcher's fight over Flee) are asked before `planPool` ranks anything,
/// and `planFightPool` ranks exactly these on the fight's band. Exhaustive on
/// purpose.
let internal safetyTier task =
    match task with
    | Flee
    | Guard _
    | Defend _
    | Assault _ -> true
    | Harvest _
    | Withdraw _
    | Pickup _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    | Reserve _
    | Claim _
    | Reclaim _
    | Dismantle _ -> false

/// The room a Task's Work Area lies in: its target's, so the Reach taken out of
/// it is that room's share. None for Flee, whose area is the creep's own
/// room's, and for a target the projection does not place. A Guard names its
/// room outright; neither caller asks about the Safety tier, but the match is
/// exhaustive.
let internal roomOfWork atlas task =
    match task with
    | Harvest id
    | Build id
    | Repair id
    | Upgrade id
    | Reserve id
    | Claim id
    | Reclaim id
    | Dismantle id -> Atlas.targetRoom atlas id
    | Pickup(id, _)
    | Withdraw(id, _)
    | Refill(id, _) -> Atlas.targetRoom atlas id
    | Guard room
    | Defend room
    | Assault room -> Some room
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
/// less its room's Reach — and for Flee, its ground off every Reach
/// (`Threats.fleeAreaFor`). The subtraction is skipped for the whole Safety
/// tier, whose areas are derived off `Threats` themselves: taking the Reach
/// out again is a tautology for Flee and the end of the Task for a Guard. The
/// exemption is the predicate, so a third Task ranked into Safety is exempt
/// here without this function being touched.
let internal areaFor (threats: Threats) atlas creep task : Set<RoomPos> =
    let ground =
        match task with
        | Flee ->
            Threats.bodyOf threats creep
            |> Option.map (Threats.fleeAreaFor threats atlas)
            |> Option.defaultValue Set.empty
        // The guard's ground is its Brief's, built by the colony that briefed
        // the room, whichever colony's body holds it — a blind room's (#366)
        // the declared source ring. One that walks in with no ground
        // (`Brief.WalksIn`) has the room's walkable ground off the border for
        // a destination.
        | Guard room ->
            let brief = Threats.briefOf threats room

            // A melee body on a chase swings from the ring beside the raid,
            // never the ranger's kite ground three off it (#571).
            let swing () =
                match brief.Duty, brief.Laid.Ring with
                | Duty.Guard BriefKind.Chase, Some ring when not (Set.isEmpty ring) ->
                    Threats.bodyOf threats creep
                    |> Option.filter isGuardRowBody
                    |> Option.map (fun _ -> ring)
                | _ -> None

            // A holder mustering waits on its own ground (#535); one seated
            // strikes from its own, no partner's seat (#558, #650).
            Map.tryFind creep brief.Stances
            |> Option.orElse (Map.tryFind creep brief.Seats.Value)
            |> Option.orElse (swing ())
            |> Option.orElse brief.Ground
            |> Option.defaultWith (fun () ->
                if brief.WalksIn then
                    Atlas.walkableTilesIn atlas room
                    |> Set.filter (Seam.onRing >> not)
                    |> RoomPos.setAt room
                else
                    Set.empty)
        // The member's own ground, rally or fight, by its role
        // (`Threats.squadGroundIn`).
        | Assault _ -> Threats.squadGroundIn threats task creep
        | Defend room -> Threats.defendGroundFor threats room creep
        | _ -> Atlas.workAreaFor atlas creep task

    match reachOnWork threats atlas task with
    | None -> ground
    | Some hot -> ground |> Set.filter (hot >> not)

/// Whether a creep may act on a Task from the tile it is standing on this tick
/// — `Atlas.mayAct` over the ground `areaFor` has just thinned. Joined because
/// every caller asks both.
let internal mayActNow (threats: Threats) atlas (creep: string) task =
    Atlas.mayAct atlas creep task (areaFor threats atlas creep task)

/// The room an Assault's or a muster's ground lies in: the rally ground's
/// room until launch, the Task's own after. Read off the tiles, which name it.
let private groundRoom (room: string) (area: Set<RoomPos>) =
    if Set.isEmpty area then
        room
    else
        (Set.minElement area).Room

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
///
/// A holder mustering for its Guard (#535) is priced toward its rally ground's
/// room, as a squad's member is toward its own.
let internal travelCostOf (threats: Threats) atlas (creep: string) task =
    match task with
    | Flee ->
        Threats.fleeCostOf threats atlas creep (fun () ->
            // Out over a border: home (#511), or where its room has no safe
            // tile (#566).
            let area = areaFor threats atlas creep task

            match Atlas.creepRoom atlas creep with
            | Some room when groundRoom room area <> room ->
                Atlas.travelCostToward atlas creep task (groundRoom room area) area
            | _ -> Atlas.travelCostWithin atlas creep area)
    | Guard room when Map.containsKey creep (Threats.briefOf threats room).Stances ->
        let area = areaFor threats atlas creep task
        Atlas.travelCostToward atlas creep task (groundRoom room area) area
    | Guard room
    | Defend room -> Atlas.travelCostToward atlas creep task room (areaFor threats atlas creep task)
    | Assault room ->
        let area = areaFor threats atlas creep task
        Atlas.travelCostToward atlas creep task (groundRoom room area) area
    | _ ->
        match areaFor threats atlas creep task with
        | area when Set.isEmpty area -> Atlas.travelCost atlas creep task
        | area -> Atlas.travelCostWithin atlas creep area

/// What calling a holder off the Guard of `held` to the kill in `kill` costs
/// (#510, #516): its travel cost to the kill, where it may be called off
/// (`Facts.mayBeCalled`, `kills` the rooms of the colony's kills) and walks
/// it within the kill's recall (`walk`).
let private callCost
    (threats: Threats)
    atlas
    (view: ColonyView)
    (kills: Set<string>)
    (held: string)
    (kill: string)
    (walk: int)
    (creep: CreepInfo)
    : int option =
    if Facts.mayBeCalled view kills held kill creep then
        travelCostOf threats atlas creep.Name (Guard kill)
        |> Option.filter (fun cost -> cost <= walk)
    else
        None

/// What a gate says of a holder called to a kill (`killCalls`): it takes
/// the kill, it may (read before the match, on a refusal only the match can
/// settle), or it surely does not.
[<RequireQualifiedAccess>]
type CallVerdict =
    | Admitted
    | Unsure
    | Refused

/// The holders a colony's kills call off the Guards they hold this tick
/// (#510, #516), each to its kill's room, decided once against the
/// remembered assignments, so no order of reading moves it. Over every kill
/// in the pool (a `RecallPolicy.Kill`) and every holder of a pooled Task it
/// may be called off that the kill outranks, the highest kill first, then
/// the cheapest call (`callCost`, then by name and room): one holder a kill
/// and one kill a holder, each pair `admits` lets take the kill — so a
/// holder within walk of two kills of a rank is called to the one it
/// reaches soonest, where the cascade seats it (#652). A pair `admits` is
/// unsure of takes the holder from every later kill and calls none. The one
/// selection the Matcher calls by (`calledOn`, its gate as `admits`, never
/// unsure) and the trespass's cast reads before the match
/// (`Matcher.callAdmits`): a kill no holder is called to casts.
let internal killCalls
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assignments: Assignments)
    (admits: CreepInfo -> PooledTask -> CallVerdict)
    : Map<string, string> =
    let recalls =
        pool
        |> List.choose (fun kill ->
            match kill.Recall with
            | Some(RecallPolicy.Kill(room, recall)) -> Some(kill, room, recall)
            | _ -> None)

    if List.isEmpty recalls then
        Map.empty
    else
        let kills = recalls |> List.map (fun (_, room, _) -> room) |> Set.ofList
        let byId = pool |> List.map (fun p -> taskId p.Task, p) |> Map.ofList

        // Each holder of a Task it may be called off, beside the room it
        // holds.
        let holders =
            assignments
            |> Map.toList
            |> List.choose (fun (name, tid) ->
                match
                    Map.tryFind tid byId, view.Creeps |> List.tryFind (fun c -> c.Name = name)
                with
                | Some({
                           Recall = Some(RecallPolicy.Callable room | RecallPolicy.Kill(room, _))
                       } as pooled),
                  Some creep -> Some(creep, pooled, room)
                | _ -> None)

        // Every pair a call could make, the highest kill first, then the
        // cheapest: a holder goes to the kill it walks to soonest among
        // its best, as the cascade then seats it, and each kill calls one
        // holder. A pair the gate might admit claims the holder all the
        // same, but answers its kill only where the gate surely does.
        recalls
        |> List.collect (fun (kill, killRoom, recall) ->
            holders
            |> List.filter (fun (_, pooled, _) -> kill.Priority < pooled.Priority)
            |> List.choose (fun (creep, _, held) ->
                callCost threats atlas view kills held killRoom recall.Walk creep
                |> Option.map (fun cost ->
                    (kill.Priority, cost, creep.Name, killRoom), (creep, kill))))
        |> List.sortBy fst
        |> List.fold
            (fun
                (claimed: Set<string>, called: Map<string, string>)
                ((_, _, name, killRoom), (creep, kill)) ->
                if
                    claimed.Contains name || called |> Map.exists (fun _ room -> room = killRoom)
                then
                    claimed, called
                else
                    match admits creep kill with
                    | CallVerdict.Admitted -> claimed.Add name, Map.add name killRoom called
                    | CallVerdict.Unsure -> claimed.Add name, called
                    | CallVerdict.Refused -> claimed, called)
            (Set.empty, Map.empty)
        |> snd

/// The mover's step toward the tiles a creep may work its Task from, and the
/// mate of `travelCostOf` above: whatever the price crossed for, the walk has to
/// cross for too, or a body is ranked onto a Task it is never carried to. A
/// Guard names its own room and is stepped toward it (`firstStepToward`); every
/// other Task derives its crossing from the target the projection places
/// (`firstStep`).
let internal stepToward atlas (creep: string) task (area: Set<RoomPos>) =
    match task with
    | Guard room
    | Defend room
    | Assault room -> Atlas.firstStepToward atlas creep task (groundRoom room area) area
    | Flee ->
        match Atlas.creepRoom atlas creep with
        | Some room when groundRoom room area <> room ->
            Atlas.firstStepToward atlas creep task (groundRoom room area) area
        | _ -> Atlas.firstStep atlas creep task area
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

/// Everything `threatened` reads of a pair beyond the tick's Threats and
/// Atlas: the Task, and whether the body is Work-heavy (the one thing of it
/// `Atlas.workAreaAcross` narrows on). A reader asking per pair keys its
/// answers on this.
let internal threatenedKey atlas (creep: CreepInfo) task =
    (if Atlas.workHeavy atlas creep.Name then "h:" else "l:") + taskId task

/// Whether the creep itself is standing where it can be hurt: its own tile
/// inside a Reach of its own room, or struck there before its fatigue lets
/// it step off (`Threats.standsStruck`). Flee's applicability, and the vision
/// grace's one gate. A creep the projection cannot place stands in no Reach.
let internal standsInReach (threats: Threats) atlas (creep: string) =
    match Threats.bodyOf threats creep with
    | Some body -> Threats.standsStruck threats atlas body
    | None ->
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
/// outpost builders' budget may ration. The room half alone: the kind half,
/// no road (#548), is `planPool`'s. A claimed room a human still names in the outpost list is not
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
/// outright — once its controller is past level 1: until then its sites wait
/// in the surplus for its Upgrade (#449).
let private isNurserySite (view: ColonyView) atlas siteId =
    siteRoomIs
        atlas
        (fun room -> isNurseryRoom view room && not (isNurseryFirstLevel view room))
        siteId

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

/// Whether the controller stands inside its downgrade deadline.
let private insideDowngradeDeadline (view: ColonyView) =
    view.Controller |> Option.exists insideDeadline

/// Whether a Task is an intake: a Harvest, Withdraw or Pickup of either
/// resource. What the body may take of them is `applicable`'s.
let internal isIntake task =
    match task with
    | Harvest _
    | Withdraw _
    | Pickup _ -> true
    | _ -> false

/// The rank the Matcher compares a candidate at (#501). For a body that digs,
/// `nearest` is the travel cost of its cheapest intake candidate, and an
/// intake gives back a rank of its lift over its band (`liftOverBand`: a
/// full container's two over the flow, a bleeding pile's one) per
/// `Tuning.IntakeTravelPerRung` it lies beyond that, never more than the lift
/// itself, so travel never drops it under its band. `None` — a body with no
/// Work — compares every Task at its pooled rank.
let internal matchRank (tuning: Tuning) (nearest: int option) (pooled: PooledTask) cost =
    match nearest with
    | Some near when isIntake pooled.Task ->
        let spent = (cost - near) / max 1 tuning.IntakeTravelPerRung
        pooled.Priority + priorityStep * min (liftOverBand pooled.Priority) spent
    | _ -> pooled.Priority

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

/// A Safety-tier Task's entry: Flee's, or a combat Task's off its Brief. It
/// reads no economy fact, so the fight pass prices its pool alone
/// (`planFightPool`).
let private safetyEntry (threats: Threats) (task: Task) : PooledTask =
    let brief = lazy (Threats.briefFor threats task)

    {
        Task = task
        Priority =
            priorityOfTier Safety
            + rankOfRung (
                match task with
                // A kill over a quiet holder's own Guard (#510, #516), and a
                // raid's Guard over a kill (#528): its Brief's.
                | Guard _ ->
                    brief.Value
                    |> Option.map (fun brief -> brief.Rung)
                    |> Option.defaultValue OnTheTier
                | _ -> OnTheTier
            )
        Capacity =
            match task with
            // The bodies the Task's Brief admits: a Guard's and a Defend's off
            // their row's own count, a squad's its slots, one cap per role.
            | Guard _
            | Defend _
            | Assault _ ->
                brief.Value
                |> Option.map (fun brief -> brief.Capacity threats)
                |> Option.defaultValue (Capacity.roles [])
            | _ -> Capacity.unbounded
        Borrowed = false
        // Every Guard's holder may be called off it for a kill, and a Guard
        // whose Brief recalls is a kill.
        Recall =
            match task with
            | Guard room ->
                match (Threats.briefOf threats room).Recall with
                | Some recall -> Some(RecallPolicy.Kill(room, recall))
                | None -> Some(RecallPolicy.Callable room)
            | _ -> None
    }

/// Planner, second half: the economy's pool, every Task below the Safety tier
/// (`planPools` hands it no other), with each entry's priority and
/// capacity on it. `planTasks` says what is pooled; this says where each
/// entry ranks and how many bodies it admits, and between them they are
/// everything the Matcher knows about a Task. Every exception the colony has
/// learned about ordering and crowding lands here and nowhere else. `held` is
/// what the colony's bodies hold: the loads already committed to a sink, which
/// a draw for that sink is capped less (#530).
let planPoolHolding
    (held: HeldTaskFacts)
    (view: ColonyView)
    atlas
    (busy: BusyReason list)
    (tasks: Task list)
    : PooledTask list =
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
    // hauler unit it would cast at its own capacity. Read below by the ring's
    // rank, the Storage draw's price and its cap.
    let clusterRoom = cluster |> Option.map RefillCluster.free |> Option.defaultValue 0

    let clusterStarved =
        clusterRoom > 0
        && view.Bank.Available < bodyCost (bodyFor haulerPattern view.Bank.Capacity)

    // A spawn holding no energy at all: the one refill the ore never jumps.
    let spawnEmpty =
        view.Refillables
        |> List.exists (fun r ->
            r.Kind = BuiltKind.Spawn && r.FreeCapacity >= Engine.spawnEnergyCapacity)

    // A mineral container nine tenths full: the miner's next few digs land on
    // the floor, where the ore bleeds.
    let overflowing storeId =
        Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Container)
        && (SpatialInfo.heldAllIn view.Spatial storeId
            - SpatialInfo.storedIn view.Spatial storeId)
           * 10
           >= Engine.containerCapacity * 9

    // The ferry's sinks (`ferryBuffers`): what a mother lends a bootstrapping
    // child is written down and bounded.
    let ferrySinks = ferryBuffers view

    // A raid at home inside the deadline (#525): attackController sinks the
    // timer mid-siege, and the towers and struck ramparts still come first.
    let raided = List.contains Raided busy
    let raidedInDeadline = raided && insideDowngradeDeadline view

    let isTower id =
        view.Refillables |> List.exists (fun r -> r.Id = id && r.Kind = BuiltKind.Tower)

    // The claim party's controllers (#471): a candidate colony's, pooled as
    // an Upgrade before the claim lands.
    // Read off the pool's own Claims, which are `outposts.Claims` (#383).
    let partyControllers =
        tasks
        |> List.choose (function
            | Claim id -> Some id
            | _ -> None)
        |> Set.ofList

    // One `ferryLoadsFor` budget per child room, spread over that room's
    // buffers in id order (user decision 2026-09-07): the hauler row hires per
    // child, so the pool admits per child.
    let ferryShare: Map<string, int> =
        ferrySinks
        |> Set.toList
        |> List.choose (fun id -> Atlas.targetRoom atlas id |> Option.map (fun room -> room, id))
        |> List.groupBy fst
        |> List.collect (fun (room, buffers) ->
            let ids = buffers |> List.map snd |> List.sort
            let n = List.length ids
            let budget = ferryLoadsFor view room

            ids
            |> List.mapi (fun i id -> id, budget / n + (if i < budget % n then 1 else 0)))
        |> Map.ofList

    let stored id = SpatialInfo.storedIn view.Spatial id

    // The one supply cap a draw on the stock is read through (#530): the loads
    // its sinks still have room for — that room less what the carriers already
    // have aboard, a loaded body being a sink's next delivery — and never more
    // than the store holds. A load drawn past it has one sink left, the store
    // it came from.
    let drawLoads stock room aboard =
        ceilDiv (min stock (max 0 (room - aboard))) haulerLoad

    // What is already aboard for these sinks: the resource the holders of a
    // Refill to one of them carry, supply committed there. A convoy rooms off
    // or a courier bound for the Reactor holds no such Refill and counts for
    // nothing.
    let carriersAboard resource (sinks: string -> bool) =
        let carried =
            view.Creeps
            |> List.map (fun creep -> creep.Name, CreepInfo.holding resource creep)
            |> Map.ofList

        tasks
        |> List.sumBy (function
            | Refill(id, r) as task when r = resource && sinks id ->
                HeldTaskFacts.holdersOf held (taskId task)
                |> List.sumBy (fun name -> Map.tryFind name carried |> Option.defaultValue 0)
            | _ -> 0)

    // The terminal's energy is kept at the larger of the consignment's fee and
    // the aid's load (`Planner.terminalEnergyRefills`).
    let terminalTarget =
        max
            (Facts.feeReserve view)
            (if Facts.givesAid view then
                 view.Tuning.AidTerminalEnergy
             else
                 0)

    // The room one energy sink has left, by what bounds its own Refill below.
    let sinkRoom id =
        if cluster |> Option.exists (fun c -> c.Spawn = id) then
            clusterRoom
        else
            match view.Refillables |> List.tryFind (fun r -> r.Id = id) with
            | Some r -> r.FreeCapacity
            | None ->
                let lent = Map.tryFind id ferryShare |> Option.map (fun share -> share * haulerLoad)

                match Map.tryFind id view.Spatial.TargetKinds with
                | Some(Structure BuiltKind.Terminal) -> terminalTarget - stored id
                | Some(Structure BuiltKind.Container) ->
                    let room = Engine.containerCapacity - SpatialInfo.heldIn view.Spatial Energy id
                    lent |> Option.fold min room
                | _ -> lent |> Option.defaultValue 0

    // The lab cluster's fills (`Facts.labPlan`): by reagent, the room its
    // labs have left for it, and which labs ask. Lazy: only a terminal draw
    // reads them.
    let labFills = lazy (Facts.labPlan view).Fills

    let labRoom =
        lazy
            (labFills.Value
             |> List.groupBy (fun (_, reagent, _) -> reagent)
             |> List.map (fun (reagent, fills) ->
                 reagent, fills |> List.sumBy (fun (_, _, room) -> room))
             |> Map.ofList)

    let labsAsking =
        lazy
            (labFills.Value
             |> List.groupBy (fun (_, reagent, _) -> reagent)
             |> List.map (fun (reagent, fills) ->
                 reagent, fills |> List.map (fun (lab, _, _) -> lab) |> Set.ofList)
             |> Map.ofList)

    // The consignor's ore sink: the room its terminals have left.
    let terminalOreRoom =
        lazy
            (Atlas.idsOfKind atlas (Structure BuiltKind.Terminal)
             |> List.sumBy (fun id ->
                 max 0 (Engine.terminalCapacity - SpatialInfo.heldAllIn view.Spatial id)))

    // The rescue budget (#284): the decaying structures this colony has let
    // fall so far below their own trigger that a repair is a trip of its own,
    // given one body apiece. Their rank is their price, the rebuild decay is
    // about to take (`rebuildAtRisk`), which lifts a structure near nothing
    // over the surplus at any distance (live at t239,65x the base roads sat
    // at 50-58% while the trunk north stood at 2% and the outpost's roads at
    // 8%); this set only caps the crowd. The decaying kinds alone, ordered by
    // the fraction of max and never the hits, ties by id. One body apiece,
    // because the walk is the expensive half and one load carries a plain
    // road from a quarter to over its whole line.
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

    // A dying rampart (`Tuning.RampartDying`) is repaired ahead of every site
    // on the feeding tier: built at 1 hit, it decays away within a hundred
    // ticks, and its site comes back to be built again (live 2026-10-04,
    // W17S25's thirteen fresh ramparts at 1 hit, every worker on a site).
    let dying =
        tasks
        |> List.choose (function
            | Repair id when Facts.isDyingRampart view id -> Some id
            | _ -> None)
        |> Set.ofList

    let underAttack = Facts.rampartsUnderAttack view

    let extensionSited = lazy (Facts.homeSiteStands view BuiltKind.Extension)

    // The emergency reserve (#505): under it the stock's draw is the
    // emergency sinks' — the refill cluster and the towers, and the Repairs
    // of a dying or a struck rampart.
    let reserveShort = Facts.belowReserve view

    let emergencyRepairs =
        tasks
        |> List.sumBy (function
            | Repair id when Set.contains id dying || Set.contains id underAttack -> 1
            | _ -> 0)

    // A site in a room this colony merely mines and that no other rule
    // already feeds. A nursery's site and a bootstrapping child's are
    // feeding-tier outright and capped by nothing, and neither room is always
    // `Borrowed`.
    let isMinedSite siteId =
        isOutpostSite view atlas siteId && not (isFeedingByRoom view atlas siteId)

    let siteKind siteId =
        Map.tryFind siteId view.Spatial.TargetKinds

    // The queue the builders' budget rations: every such site but a road
    // (#548). A road out there, hand-laid or the Layout's, is a surplus Build
    // with a rung of its own (below): the switch is income, the road only a
    // cheaper haul.
    let outpostSites =
        tasks
        |> List.choose (function
            | Build siteId when isMinedSite siteId && siteKind siteId <> Some(Site BuiltKind.Road) ->
                Some siteId
            | _ -> None)

    let homeBuilding = lazy (Facts.homeBuilding view)

    let isOutpostRoadSite siteId =
        siteKind siteId = Some(Site BuiltKind.Road) && isMinedSite siteId

    // The order the budget is spent in: the container sites first,
    // then the nearest to the crossing, ties by id. One queue over every
    // outpost and not one apiece: two rooms' sites are ordered on a walk that
    // leaves the home-side leg off both, so the comparison says "nearer its
    // own crossing" and not "nearer the spawn". A site whose walk cannot be
    // priced sorts last rather than out of the list.
    let siteOrder siteId =
        let container =
            if siteKind siteId = Some(Site BuiltKind.Container) then
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

    // The one outpost road site a colony crosses for at a time (#548), in the
    // budget's order: one body across the Seam for roads, never the row the
    // controller and the repairs are owed. Ordered only when there is a choice.
    let leadRoadSite =
        lazy
            (match
                tasks
                |> List.choose (function
                    | Build siteId when isOutpostRoadSite siteId -> Some siteId
                    | _ -> None)
             with
             | [] -> None
             | [ only ] -> Some only
             | sites -> Some(List.minBy siteOrder sites))

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


    // The work no energy price measures, at the band it is anchored in: the
    // season's score and the ore and reagents beside it, a new colony and the
    // Reactor's claim. None for everything the price below reads off the
    // ground. Thorium is never energy (score is not economy), so its draws
    // stand at the flow's band where the score is about to be lost or
    // delivered, and a band under it where it is stock moved.
    let scoreTierOf task : Tier option =
        match task with
        // A claim decides whether there is going to be a second colony at all.
        | Claim _ -> Some Feeding
        // The reclaim decides whether the season's whole score accrues to this
        // colony or to a rival. Not the fight's band: nothing out there is
        // killing the body, and a Task ranked there would be offered to it
        // ahead of running from a keeper.
        | Reclaim _ -> Some Feeding
        // A deposit's dig: the score's own intake.
        | Harvest rockId when Atlas.isMineral atlas rockId -> Some Feeding
        // The delivery's own draw is Feeding (#367): that load is the season's
        // score, not stock being topped up. Live at t506,631-507,096 the
        // courier scored the delivery draw at rank 8 while energy hauling
        // scored -2 and 0, hauled energy for 465 ticks, and the Reactor fell
        // 500 -> 47. Told apart by the store, the same test the Emitter's
        // `deliveryDraw` uses, and only while the programme is open.
        | Withdraw(storeId, Thorium) when isStorage storeId && Facts.courierProgrammeOpen view atlas ->
            Some Feeding
        // Ore lying in the Reactor's own room is the delivery's, not stock:
        // its sink is the Reactor five tiles away (`Planner.reactorRefills`).
        // Ore in a merely crossed room keeps `StockDraw` and its walk home.
        | Withdraw(storeId, Thorium) when besideTheReactor storeId -> Some Feeding
        | Pickup(pileId, Thorium) when besideTheReactor pileId -> Some Feeding
        // Ore about to land on the floor, or already there, is Feeding (#497):
        // live W17S29 stood at 2,000 T with 885 on the floor while every
        // hauler topped up extensions that were not empty. Every pooled pile
        // is past `Tuning.PickupThreshold`. Not while a spawn is empty: the
        // colony staying alive comes first.
        | Withdraw(storeId, Thorium) when overflowing storeId && not spawnEmpty -> Some Feeding
        | Pickup(_, Thorium) when not spawnEmpty -> Some Feeding
        // The mine haul ranks at the Storage's band: an empty hauler beside
        // the mine must not take the season's ore ahead of the energy the
        // spawn is waiting on.
        | Withdraw(_, Thorium) -> Some StockDraw
        // The ore haul one resource over, and never the season's score: an
        // overflowing ore mine is Feeding for the floor's reason, any other at
        // the Storage's band.
        | Withdraw(storeId, Ore _) when overflowing storeId && not spawnEmpty -> Some Feeding
        | Withdraw(_, Ore _)
        | Pickup(_, Ore _) -> Some StockDraw
        // The lab cluster's haul, terminal to lab and lab to terminal: stock
        // moved between two stores, at the Storage's band.
        | Withdraw(_, Compound _)
        | Pickup(_, Compound _) -> Some StockDraw
        // Under an empty spawn a Thorium pile ranks where the Thorium container
        // does: it is that container's next dig, landed on the floor because
        // the store was full.
        | Pickup(_, Thorium) -> Some StockDraw
        // A store of a resource no energy price covers: the labs' reagents,
        // the terminal's ore, the Reactor's Thorium. The Storage's is the
        // floor, priced below.
        | Refill(structureId, resource) when resource <> Energy && not (isStorage structureId) ->
            Some Feeding
        | Flee
        | Guard _
        | Defend _
        | Assault _
        | Harvest _
        | Reserve _
        | Withdraw(_, Energy)
        | Pickup(_, Energy)
        | Refill _
        | Repair _
        | Build _
        | Upgrade _
        | Dismantle _ -> None

    // The rungs a score Task stands over its band (the only rungs left, for
    // the score has no loss to price): a full mine container, whose next
    // digs land on the floor and age the miner past the contact cliff (#306),
    // two; the ore in a tombstone, which ends (#359), and the ore on the
    // floor, which bleeds, one.
    let scoreLift task =
        match task with
        | Withdraw(storeId, Thorium) when
            SpatialInfo.heldIn view.Spatial Thorium storeId >= view.Tuning.MineContactCliff
            ->
            2
        | Withdraw(storeId, Thorium) when
            Map.tryFind storeId view.Spatial.TargetKinds = Some Tombstone
            ->
            1
        | Pickup(_, Thorium) -> 1
        | _ -> 0

    let home = SpatialInfo.homeName view.Spatial

    // The rate a room's sources are held at, the flow where the room's
    // control is not on the view.
    let rateIn room =
        Map.tryFind room view.RoomControl
        |> Option.map Facts.heldRateOf
        |> Option.defaultValue Engine.heldOutputPerTick
        |> float

    let rateOf id =
        SpatialInfo.roomOf view.Spatial id
        |> Option.map rateIn
        |> Option.defaultValue heldRate

    // A tick the refill ring waits is a tick every spawn waits: the colony's
    // spawn time at what a tick of it is worth (`Facts.spawnTickWorth`), every
    // row casting at the bank's capacity. Never under one source's flow: a
    // colony too poor to price its income is the one whose spawn it is.
    let ringWorth =
        max heldRate (Facts.spawnTickWorth view * float (max 1 (List.length view.Spawns)))

    let isRing id =
        cluster |> Option.exists (fun c -> c.Spawn = id)
        || view.Refillables
           |> List.exists (fun r ->
               r.Id = id && (r.Kind = BuiltKind.Spawn || r.Kind = BuiltKind.Extension))

    // The home's raid hazard, a creep's life where none is on record: what
    // a tower short of energy and a line short of its floor are priced under.
    let homeHazard = raidHazard view.Time view.HomeRaided homeRaidPrior

    // The last tick a hostile walked a room: one standing there now, the
    // home's last raider, a resident room's fight record. No prior: a room no
    // hostile is known to walk puts no site at risk.
    let lastWalked room =
        [
            if view.Hostiles |> List.exists (fun h -> h.Pos.Room = room) then
                yield view.Time
            if room = home then
                yield! Option.toList view.HomeRaided
            yield! Map.tryFind room view.Fought |> Option.map _.Seen |> Option.toList
        ]
        |> function
            | [] -> None
            | seen -> Some(List.max seen)

    let siteProgress siteId =
        match
            siteKind siteId, view.ConstructionSites |> List.tryFind (fun site -> site.Id = siteId)
        with
        | Some(Site kind), Some site -> max 0 (constructionCost kind - site.Left)
        | _ -> 0

    // The bodies the buffer feeds, by their cost: the standing row's
    // (`bodyClassOf`), which drinks nowhere else and whose cost the upgrader
    // row sold it at, and every other light body holding the home
    // controller's Upgrade, whose drink stands beside it.
    let standingCosts =
        lazy
            (let upgrading =
                view.Controller
                |> Option.map (fun c ->
                    HeldTaskFacts.holdersOf held (taskId (Upgrade c.Id)) |> Set.ofList)
                |> Option.defaultValue Set.empty

             view.Creeps
             |> List.filter (fun creep ->
                 match bodyClassOf view.Tuning atlas creep with
                 | Standing -> true
                 | Light -> Set.contains creep.Name upgrading
                 | Heavy
                 | Fighter -> false)
             |> List.map (fun creep ->
                 creep.Body
                 |> Map.toList
                 |> List.collect (fun (part, n) -> List.replicate n part)
                 |> bodyCost))

    let lineShort = lazy (Facts.lineShortfall view)

    // A decaying structure's rebuild and the hits it loses a tick: a road's
    // by its line's multiple of a plain road's hits, a container's by
    // whether the room is ours.
    let decayOf id (kind: BuiltKind) (hits: HitsInfo) =
        match kind with
        | BuiltKind.Road ->
            let ratio = max 1 (hits.HitsMax / Engine.roadHits)

            Some(
                Engine.roadBuildCost * ratio,
                float (Engine.roadDecayAmount * ratio) / float Engine.roadDecayTime
            )
        | BuiltKind.Container ->
            let owned = SpatialInfo.roomOf view.Spatial id |> Option.exists (Facts.ourRoom view)

            Some(
                constructionCost BuiltKind.Container,
                float Engine.containerDecayAmount
                / float (
                    if owned then
                        Engine.containerDecayTimeOwned
                    else
                        Engine.containerDecayTime
                )
            )
        | _ -> None

    // A site's growth: the band its kind and room put it in, and the steps
    // over it the colony grows by. Every site in a nursery or a bootstrapping
    // room, and the outpost sites the builders' budget lifts, are income
    // waiting (the flow's band); every other is the surplus's.
    let siteGrowth siteId =
        let band =
            if isFeedingSite view atlas fedSiteIds siteId then
                priorityOfTier Feeding
            else
                priorityOfTier Surplus

        let surplus = band = priorityOfTier Surplus

        let lift =
            match siteKind siteId with
            // A room's first tower over every other site (user, 2026-10-04:
            // W17S25 at RCL3 raises its tower before its extensions): the
            // room's defence once its safe mode ends.
            | Some(Site BuiltKind.Tower) when
                Atlas.targetRoom atlas siteId
                |> Option.exists (fun room -> Atlas.builtIn atlas room BuiltKind.Tower = 0)
                ->
                2
            // The Storage beside the home's sites and no higher while an
            // extension site stands (#526): the extensions are the bank.
            | Some(Site BuiltKind.Storage) when surplus && extensionSited.Force() -> 0
            // A site outranks the controller (#234): a worker that fills at
            // the buffer already stands in the controller's Work Area.
            | _ when surplus && isHomeSite view atlas siteId -> 1
            // An outpost's road site (#548): under the home's sites while the
            // home builds, the colony's one lead site over the controller
            // after, and every other on the band, where travel decides.
            | _ when isOutpostRoadSite siteId ->
                if homeBuilding.Force() then -1
                elif leadRoadSite.Force() = Some siteId then 1
                else 0
            | _ -> 0

        valueOfRank (band - lift)

    // What a sink loses a tick while it waits, in energy: every Refill, Build,
    // Repair, Upgrade and Dismantle. Apart from the intakes' prices below
    // because the stock's draw is priced off the hungriest of these.
    let sinkValueOf task : float =
        match scoreTierOf task with
        | Some tier -> valueOfRank (priorityOfTier tier)
        | None ->
            match task with
            // The stock's own Refill: nothing waits on it. Under the emergency
            // reserve (#505, a named margin) income banks ahead of the ferry,
            // a rank over its band.
            | Refill(structureId, _) when isStorage structureId ->
                if reserveShort then
                    valueOfRank (priorityOfTier UpgradeBuffer - priorityStep)
                else
                    0.0
            // What a mother lends a bootstrapping child: the child's need is
            // not on her view, so it stands at the buffer's band.
            | Refill(structureId, Energy) when Set.contains structureId ferrySinks ->
                anchorOf UpgradeBuffer
            // A busy colony's own buffer (#523): the upgrader row keeps one
            // body there as downgrade insurance and no mouth, so the buffer
            // is filled after every sink that spends, a rank over the stock —
            // but not inside the deadline, where it feeds that body (#525).
            // The row and the pool read the one fact, `busy`.
            | Refill(structureId, Energy) when
                Set.contains structureId buffers
                && not (List.isEmpty busy)
                && not (insideDowngradeDeadline view)
                ->
                valueOfRank (bottomRank - priorityStep)
            // The buffer: what the bodies it feeds lose while it runs dry
            // (under the line a hauled store reads drained at,
            // `Tuning.HaulDrainedPercent`),
            // the bodies the upgrader row bought among them, so the row never
            // keeps a body the pool will not feed; and never under its band,
            // where whoever upgrades drinks.
            | Refill(structureId, Energy) when Set.contains structureId buffers ->
                max
                    (anchorOf UpgradeBuffer)
                    (idleMouths
                        (float view.Tuning.HaulDrainedPercent / 100.0)
                        standingCosts.Value
                        (SpatialInfo.heldIn view.Spatial Energy structureId)
                        Engine.containerCapacity)
            // A tower short of energy: the expected defence loss.
            | Refill(structureId, Energy) when isTower structureId ->
                let free =
                    view.Refillables
                    |> List.tryFind (fun r -> r.Id = structureId)
                    |> Option.map _.FreeCapacity
                    |> Option.defaultValue 0

                towerShort homeHazard free
            // The ring: every spawn's tick.
            | Refill(structureId, Energy) when isRing structureId ->
                // Starved, it stands in the defence band (`priorityOf`), and
                // what a draw on the stock serves is worth that much.
                if clusterStarved && cluster |> Option.exists (fun c -> c.Spawn = structureId) then
                    valueOfRank urgentRank
                else
                    ringWorth
            // The terminal's fee and aid: a send waits on it whole.
            | Refill _ -> anchorOf Feeding
            // A rampart about to decay away (`Tuning.RampartDying`): its site
            // comes back to be built again (live 2026-10-04, W17S25's thirteen
            // fresh ramparts at 1 hit, every worker on a site), two steps over
            // the flow.
            | Repair id when Set.contains id dying -> valueOfRank (priorityOfTier Feeding - 2)
            // The declared line short of its floor: the hits a raid meets
            // fewer of, at the energy they take, under the home's hazard.
            | Repair id when lineShort.Value.ContainsKey id ->
                max
                    (anchorOf Surplus)
                    (homeHazard * float lineShort.Value.[id] / float Engine.repairHitsPerEnergy)
            | Repair id ->
                match Map.tryFind id view.Spatial.TargetKinds, Map.tryFind id view.Spatial.Hits with
                | Some(Structure kind), Some hits ->
                    match decayOf id kind hits with
                    | Some(rebuild, decay) -> rebuildAtRisk rebuild hits.Hits decay
                    | None -> anchorOf Surplus
                | _ -> anchorOf Surplus
            // A site: its growth, and the progress hostiles walking its room
            // can stomp (#661).
            | Build siteId ->
                let line =
                    lineShort.Value
                    |> Map.tryFind siteId
                    |> Option.map (fun floor ->
                        homeHazard * float floor / float Engine.repairHitsPerEnergy)
                    |> Option.defaultValue 0.0

                let walked =
                    Atlas.targetRoom atlas siteId
                    |> Option.map (fun room -> raidHazard view.Time (lastWalked room) 0.0)
                    |> Option.defaultValue 0.0

                max (siteGrowth siteId) line + stompRisk (siteProgress siteId) walked
            // The claim party's: RCL2 before the tap is what the claim is
            // worth, two steps over the flow (#471): on the band alone travel
            // keeps every loaded pioneer at home.
            | Upgrade controllerId when Set.contains controllerId partyControllers ->
                valueOfRank (priorityOfTier Feeding - 2)
            // A bootstrapped child's Upgrade, in the mother's pool (#213): the
            // band the pioneers were hired for. A level-1 nursery's comes
            // ahead of its sites (#449).
            | Upgrade controllerId when
                isBorrowedUpgrade view controllerId
                && (Atlas.targetRoom atlas controllerId |> Option.exists (isNurseryFirstLevel view)
                    || not (sitesPendingBeside view atlas controllerId))
                ->
                anchorOf Feeding
            // The surplus's mouth: the energy is spent, not lost, and nothing
            // waits on it.
            | Upgrade _
            // The room is only costing CPU while it stands.
            | Dismantle _ -> anchorOf Surplus
            // The intakes, priced below (`valueOf`); the score's, by
            // `scoreTierOf`; and the fight's (`planFightPool`).
            | Harvest _
            | Reserve _
            | Withdraw _
            | Pickup _
            | Claim _
            | Reclaim _
            | Flee
            | Guard _
            | Defend _
            | Assault _ -> 0.0

    // The hungriest sink an energy load serves: what a draw on the stock is
    // worth (`stockDraw`).
    let hungriest =
        lazy
            (tasks
             |> List.fold
                 (fun most task ->
                     match task with
                     | Refill(id, Energy) when not (isStorage id) -> max most (sinkValueOf task)
                     | Build _
                     | Repair _
                     | Upgrade _ -> max most (sinkValueOf task)
                     | _ -> most)
                 0.0)

    // ADR-0094. What a Task loses a tick while it waits, in energy: the price
    // its rank is read off (`economyRank`).
    let valueOf task : float =
        match scoreTierOf task with
        | Some tier -> valueOfRank (priorityOfTier tier - scoreLift task)
        | None ->
            match task with
            // A dig and a reservation are the flow itself, whichever rock or
            // controller: the caster casts one body a Post and one a room,
            // legged for its walk, so which one a body takes is travel's — a
            // rate apart, an outpost's Anchor walked home to a richer rock
            // (#622's S4 capture).
            | Harvest _
            | Reserve _ -> heldRate
            // The stock: worth what the hungriest sink it serves is worth, and
            // never more than the flow.
            | Withdraw(storeId, Energy) when isStorage storeId ->
                stockDraw clusterStarved hungriest.Value
            // A tombstone or a ruin ends whole (`transientLoss`).
            | Withdraw(storeId, Energy) when
                Map.tryFind storeId view.Spatial.TargetKinds |> Option.exists isTransient
                ->
                flowDraw (transientLoss (stored storeId))
            // The buffer fills from the haul, never from a source: it does not
            // overflow.
            | Withdraw(storeId, Energy) when Set.contains storeId buffers -> flowDraw 0.0
            // A source's container: the flow, and the overflow pressure of
            // its source's rate over the line a hauled container sawtooths
            // under (`Tuning.HaulFullPercent`, the haul overflow's own line).
            | Withdraw(storeId, Energy) ->
                flowDraw (
                    overflowLoss
                        (float view.Tuning.HaulFullPercent / 100.0)
                        (rateOf storeId)
                        (stored storeId)
                        Engine.containerCapacity
                )
            // A pile: the flow, and its decay.
            | Pickup(pileId, Energy) -> flowDraw (pileDecay (stored pileId))
            | _ -> sinkValueOf task

    // Every pooled energy sink but the stock's own, which would open the stock
    // against itself, and but a sink nothing waits on, which would carry the
    // stock out to be banked again. Lazy: only a colony with a stock reads it.
    let stockSinkRoom =
        lazy
            (tasks
             |> List.sumBy (function
                 | Refill(id, Energy) as task when not (isStorage id) && sinkValueOf task > 0.0 ->
                     max 0 (sinkRoom id)
                 | _ -> 0))

    // The Task's rank: its price (`economyRank`), and over every price the
    // bands no price reaches — the downgrade deadline (an engine fact) and
    // the home's defence in a raid (home defence first). The deadline lifts
    // the colony's own controller and no other: the pool can hold a
    // bootstrapped child's Upgrade too, and lifting that one on the mother's
    // timer would send her loaded fleet across the Seam on the tick her own
    // controller was closest to downgrading.
    let priorityOf task =
        match task with
        | Upgrade id when
            insideDowngradeDeadline view
            && view.Controller |> Option.exists (fun c -> c.Id = id)
            ->
            deadlineRank
        | Upgrade id when Atlas.targetRoom atlas id |> Option.exists (isNurseryNearDowngrade view) ->
            deadlineRank
        | Refill(id, Energy) when raidedInDeadline && isTower id -> deadlineRank - priorityStep
        | Repair id when raidedInDeadline && Set.contains id underAttack ->
            deadlineRank - priorityStep
        // The raided home's towers and the starved cluster share the band, and
        // the walk decides between them: a colony that cannot buy the body
        // that refills its ring casts nothing at all.
        | Refill(id, Energy) when raided && isTower id -> urgentRank
        | Refill(id, Energy) when clusterStarved && cluster |> Option.exists (fun c -> c.Spawn = id) ->
            urgentRank
        // A rampart under attack (#467), a rung over both: the room's defence
        // while it stands, and the tower's energy is no use once the raid is
        // through the line.
        | Repair id when Set.contains id underAttack -> urgentRank - priorityStep
        | _ -> economyRank (valueOf task)

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
        // Under the reserve the stock admits the loads the emergency sinks
        // take (#505): the carriers the refillables' room over a hauler's
        // load, and a Work body per emergency Repair — a carrier cannot
        // repair, and a Work body's load would otherwise go to routine work.
        // With no carrier of ours alive the Work bodies take the carriers'
        // loads too, or the spawn and the towers would go unfed.
        | Withdraw(storeId, Energy) when reserveShort && isStorage storeId ->
            let room = view.Refillables |> List.sumBy (fun r -> r.FreeCapacity)

            let refillable id =
                view.Refillables |> List.exists (fun r -> r.Id = id)

            let carried = drawLoads (stored storeId) room (carriersAboard Energy refillable)

            let noCarrier =
                view.Creeps
                |> List.forall (fun creep ->
                    partCount creep.Body Work > 0 || partCount creep.Body Carry = 0)

            Capacity.unbounded
            |> Capacity.capping CapScope.Carriers carried
            |> Capacity.capping
                CapScope.Workers
                (emergencyRepairs + (if noCarrier then carried else 0))
        // The starved cluster's draw on the stock admits the loads the ring
        // can take, not the loads the Storage divides into — #367's hazard on
        // the energy column otherwise. At least one, or the lift would be a
        // rank on a Task nobody may hold.
        | Withdraw(storeId, Energy) when clusterStarved && isStorage storeId ->
            let ring id =
                cluster |> Option.exists (fun c -> c.Spawn = id)

            Capacity.total (
                max 1 (drawLoads (stored storeId) clusterRoom (carriersAboard Energy ring))
            )
        // Otherwise the carriers draw for the room the stock's sinks have left
        // (#530), and the Work bodies for their own spending, which the
        // Upgrade keeps open: the stock over a load.
        | Withdraw(storeId, Energy) when isStorage storeId ->
            let stock = stored storeId

            Capacity.unbounded
            |> Capacity.capping
                CapScope.Carriers
                (drawLoads stock stockSinkRoom.Value (carriersAboard Energy (isStorage >> not)))
            |> Capacity.capping CapScope.Workers (ceilDiv stock haulerLoad)
        // A consignor's ore draw, for the room its terminal has left.
        | Withdraw(storeId, Thorium) when isStorage storeId && Option.isSome view.Consignee ->
            Capacity.total (
                drawLoads
                    (SpatialInfo.heldIn view.Spatial Thorium storeId)
                    terminalOreRoom.Value
                    (carriersAboard Thorium (fun id ->
                        Map.tryFind id view.Spatial.TargetKinds = Some(
                            Structure BuiltKind.Terminal
                        )))
            )
        // A reagent drawn out of the terminal for the labs, for the room the
        // labs asking for it have left: a draw past it has one sink left, the
        // terminal it came from.
        | Withdraw(storeId, resource) when
            isMineralResource resource
            && Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Terminal)
            && Map.containsKey resource labRoom.Value
            ->
            let labs = labsAsking.Value |> Map.tryFind resource |> Option.defaultValue Set.empty

            Capacity.total (
                drawLoads
                    (SpatialInfo.heldIn view.Spatial resource storeId)
                    labRoom.Value.[resource]
                    (carriersAboard resource labs.Contains)
            )
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
        // The controller's buffer, bounded by its room as the cluster is: two
        // loads of 465 held a container twenty short of full while the ring
        // the spawn had just emptied waited (W17S25, 2026-10-04).
        | Refill(structureId, Energy) when
            Set.contains structureId (Atlas.controllerContainers atlas)
            ->
            Capacity.unbounded
            |> Capacity.budgeting (
                Engine.containerCapacity - SpatialInfo.heldIn view.Spatial Energy structureId
            )
        // The party: loaded pioneers until they carry `Tuning.ClaimPartyEnergy`
        // between them.
        | Upgrade controllerId when Set.contains controllerId partyControllers ->
            Capacity.total view.Tuning.PioneerCount
            |> Capacity.budgeting view.Tuning.ClaimPartyEnergy
        // A borrowed Upgrade takes the bodies hired for it and no more (#213):
        // `Tuning.PioneerCount`, the same constant the worker row is raised by.
        | Upgrade controllerId when isBorrowedUpgrade view controllerId ->
            Capacity.total view.Tuning.PioneerCount
        // An RCL8 controller takes `Engine.maxUpgradePerTick` a tick from
        // everyone together (#483): Work past it holds a seat and upgrades
        // nothing.
        | Upgrade controllerId when
            view.Controller
            |> Option.exists (fun c -> c.Id = controllerId && c.Level >= Engine.controllerMaxLevel)
            ->
            Capacity.unbounded |> Capacity.budgetingWork Engine.maxUpgradePerTick
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
                // One body: the rung would otherwise send the whole loaded
                // row over the Seam for a 300-point site.
                elif leadRoadSite.Force() = Some siteId then Some 1
                else None

            Capacity.unbounded
            |> Capacity.cappingMaybe CapScope.Everyone total
            |> Capacity.exempting exempt
        // A rescue is one body's trip (`rescued`). Every other Repair is
        // uncapped: a road under the spawn is worked by whoever is standing
        // over it.
        | Repair id when Set.contains id rescued || Set.contains id dying -> Capacity.total 1
        | _ -> Capacity.unbounded

    // The ramparts a hostile strikes this tick, whose Repairs let a holder go.
    let struck =
        match view.Hostiles with
        | [] -> Set.empty
        | _ -> Facts.rampartsUnderAttack view

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
            Recall =
                match task with
                | Repair id when Set.contains id struck ->
                    Atlas.targetRoom atlas id |> Option.map RecallPolicy.Struck
                | _ -> None
        })

/// The fight pass's pool: the Safety-tier Tasks of `tasks`, Flee and the
/// combat Tasks, in their order.
let planFightPool (threats: Threats) (tasks: Task list) : PooledTask list =
    tasks |> List.filter safetyTier |> List.map (safetyEntry threats)

/// The tick's pool in its two halves, each in pool order: the fight pass's
/// (`planFightPool`) and the economy's, every Task below the Safety tier
/// (`planPoolHolding`). The whole pool is the one then the other.
let planPools
    (held: HeldTaskFacts)
    (view: ColonyView)
    atlas
    (threats: Threats)
    (busy: BusyReason list)
    (tasks: Task list)
    : PooledTask list * PooledTask list =
    planFightPool threats tasks,
    planPoolHolding held view atlas busy (List.filter (safetyTier >> not) tasks)

/// The whole pool (`planPools`) over a colony whose bodies hold nothing:
/// every load aboard is uncommitted.
let planPool
    (view: ColonyView)
    atlas
    (threats: Threats)
    (busy: BusyReason list)
    (tasks: Task list)
    =
    let fight, economy = planPools HeldTaskFacts.empty view atlas threats busy tasks
    fight @ economy
