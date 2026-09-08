/// The Emitter: which Tasks a given body may act on now, and the engine Intents
/// an assignment turns into. Actions only — the walk that gets a creep there
/// belongs to the Resolver.
[<AutoOpen>]
module Fabot.Core.Decide.Emitter

open Fabot.Core
open Fabot.Core.Types

/// Whether a creep can usefully work this Task right now. The body must
/// physically be able to do it — Work-part tasks need a Work part, energy
/// delivery needs a Carry part — and the energy state must call for it: a body
/// past half full is not worth *sending* for more, by digging or by drawing
/// (#235), and an empty creep has nothing to deliver. Not all of it is a
/// judgement about the body: the gates below read the target's kind, its
/// geometry and what is standing in it. Gates read part arithmetic, never names
/// or roles (ADR 0006). One geometric widening (ADR
/// 0012), body-aware since ADR 0024: a full Work-heavy creep standing on a
/// built source container keeps Harvest, the engine dropping the overflow into
/// the container underfoot. A light body gets no such reprieve, or it would
/// hold the Post for the rest of its life. A second gate is comparative (ADR
/// 0016): a body with more Work than Move never Withdraws, so its only
/// feeding-tier candidate is Harvest. A third reads the target's kind beside
/// the body (ADR 0019): only a creep with a Work part draws from the
/// controller's upgrade buffer. A fourth reads the body alone and covers
/// **every** Build (#157, widened by #234): a Build is inapplicable to a
/// Work-heavy body. A fifth covers Build, Repair and Refill for a **standing
/// body** (ADR 0046), all three being deliveries and a delivery by a body
/// holding fifty energy against eleven Work being a commute. A sixth reads the
/// geometry beside the body (ADR 0048): Upgrade is applicable to a Work-heavy
/// body only where it may already act on it. A seventh is three clauses over
/// one Task (#235), all of them about the walk a light body makes to a rock
/// and none of them reaching the Work-heavy body ADR 0020 has pinned to a Post:
/// it must be half empty *or* already standing where it may dig, it must not be
/// a standing body — which closes #206 for the one Task that ticket spared —
/// and the rock's rate must still outrun what the bodies garrisoning it take.
let internal applicable
    (view: ColonyView)
    (threats: Threats)
    atlas
    (creep: CreepInfo)
    (pooled: PooledTask)
    =
    let task = pooled.Task

    let has part =
        creep.Body |> Map.tryFind part |> Option.exists (fun n -> n > 0)

    // An intake — a Withdraw, a Pickup or, since #235, a Harvest — is for a body
    // with room to carry it: at least half its store free (live: a hauler holding
    // 1,150 of 1,200 walked forty tiles to pick fifty off a pile while the spawn
    // stood at eighteen energy). A body past half full is a delivery, and its
    // intake waits until it has delivered. A standing body's one Carry is a
    // trip's worth, so for it this is "empty". Harvest joined the other two
    // late and for a light body only (#235), and there it prices the **walk**
    // alone: the other two finish in the tick the body arrives, where a dig
    // runs for dozens, so only Harvest can be half way through when this is
    // read — and a garrison's whole working life is spent past half full.
    let halfEmpty = creep.FreeCapacity * 2 >= creep.Energy + creep.FreeCapacity

    match task with
    // ADR 0024's full-store reprieve, and beside it the clause that keeps ADR
    // 0048's own Consequence reachable ("stands where it is until it can dig
    // again"). A Work-heavy body never empties — ADR 0016 shut Withdraw and
    // Transfer, ADR 0046 shut Refill, Build and Repair, ADR 0048 shuts the walk
    // to the controller — so a store gate that reads fullness as "done here"
    // reads a garrison's ordinary condition as a reason to take its work away.
    // Which it did: a hauler drawing the container swaps the Anchor onto the
    // Seat beside it, and a full body one step off its Post had no Task at all.
    // So the gate is widened by a question and not by a tile: ADR 0024 asks
    // whether a body may keep *digging* where it stands, and a body still
    // walking is not digging.
    //
    // **And the walk has to end somewhere it can stand** (#258). ADR 0048
    // offered it wherever the source has a Post, on the argument that a Post is
    // a tile the arriving body has something to do on — true of the tile and
    // not of the tick, because the Post cap that would otherwise refuse the
    // pair is counted at arrival (ADR 0026) and a long enough walk discounts
    // any incumbent. So a full Anchor released off its own rock read every
    // garrisoned Post in the colony as somewhere to go, and the one live case
    // walked a border home to stand beside another Anchor's Post while the
    // vacancy it left cast a replacement (user, 2026-09-08). The walk is
    // offered while a Post of that source has no garrison standing on it
    // *now* — over the same census this clause's own Post test reads, so the
    // rock a full body is walking to raise the site of is one it can still
    // have (#205). The narrowing is this disjunct's alone and so is a **full**
    // body's alone: a heavy body with a free store is offered the walk by the
    // clause above, which is what lets a fresh Anchor be sent to the Post its
    // expiring incumbent is still standing on (ADR 0026).
    | Harvest sourceId ->
        has Work
        && (creep.FreeCapacity > 0
            || garrisons atlas creep sourceId
            || (Atlas.workHeavy atlas creep.Name
                && not (Set.isEmpty (Atlas.postsOf atlas sourceId))
                && not (Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
                && hasUnmannedPost view atlas creep sourceId))
        // **Three clauses a light body answers and a garrison does not**
        // (#235), drawn at ADR 0016's ratio, which is where every other line
        // that separates the two bodies is drawn. Every one of the three is
        // about the walk digging costs a body that does not live at the rock,
        // so none of them can reach the body ADR 0020 has already pinned to a
        // Post it is standing on: a Work-heavy body's Harvest goes on being
        // decided by the gate above and by ADR 0024's two reprieves alone.
        //
        // **Half empty, like the other two intakes — while the walk is still
        // ahead of it.** The mirror the Withdraw and the Pickup have carried
        // since 75edfee never reached Harvest, so any room at all was room
        // enough: live at t199,88x a `9W/9C/9M` worker with nine free of four
        // hundred and fifty crossed a Seam, dug once, released full, and
        // crossed back — and Harvest being the Feeding tier (ADR 0023) it
        // outranked every Surplus Task the body could have done where it
        // stood. But Harvest is the one intake that does not finish in a tick,
        // and this gate is the *release* gate as well as the dispatch one, so
        // the store mirror alone evicted a body off the Seat it was digging on
        // the tick it crossed half full: half a load carried home for the whole
        // of the walk it had already paid. So it is spelled the way ADR 0048
        // spells its own widening in this same branch — the Emitter's `mayAct`,
        // false while the walk is ahead of the body and true the tick it
        // arrives. A body still walking answers the mirror; a body standing
        // where it may dig has no walk left to price and fills to the brim.
        //
        // **A [[standing body]] does not walk to a rock either**, which closes
        // #206 for the one Task it left open. That ticket shut the Pickup and
        // every non-buffer Withdraw for a body carrying fewer than one Carry per
        // four Work, and spared Harvest on the reasoning that travel cost would
        // keep the upgrader row beside its buffer and that the anchor row is a
        // standing body too. Travel cost did not: an empty buffer leaves the row
        // nothing else applicable at all, and W13S28's `11W/1C/11M` upgraders
        // walked to the sources on the ticks it ran dry, fifty energy a trip
        // against eleven Work. The anchor row's half of that reasoning is what
        // the heavy exemption above already answers.
        //
        // **And something spare in the rock to dig.** Those last two carry no
        // arrival exemption on purpose: what they refuse is a body that should
        // not be at the rock at all, and the release is the point of them —
        // #235's case (b) is a light body squatting the Seat the outpost's own
        // Anchor needs the tick it stands up.
        && (Atlas.workHeavy atlas creep.Name
            || ((halfEmpty
                 || Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
                && not (isStandingBody view.Tuning creep)
                && hasSpareRate view atlas sourceId))
    // The body half of this gate — a Carry part and ADR 0016's comparative
    // clause — is read a second time out of line by `canRefill`, the supply
    // floor's arming condition (ADR 0050): a clause narrowing what a body may
    // draw with belongs in front of both readers, or a colony whose only carrier
    // this gate has just shut out still reads as able to refill.
    | Withdraw storeId ->
        let buffer = Set.contains storeId (Atlas.controllerContainers atlas)

        // **A Withdraw must be worth this body's trip** (#232): the store has
        // to hold at least half of what the body came with room for. It is the
        // mirror of `halfEmpty` above and the second half of the same sentence
        // — half empty is what makes a body worth sending, half a load is what
        // makes a store worth sending it to — and it is a fact about the
        // *pair*, so it belongs here and not in `capacityOf`, whose number is
        // the Task's alone. What it cures is the other end of the haul cycle: a
        // [[capacity]] of `ceil(stock / one load)` admits a drawer to any store
        // holding one energy, and the half-full rule above then keeps the
        // arriving body there until it has drained the Anchor's trickle. Live,
        // a 24C/12M hauler stood forty-two ticks on a container holding ~200 to
        // carry six hundred, while the Storage held 263,803 and the spawn stood
        // at twenty-eight. The tier gap (ADR 0023) cannot break that by itself:
        // a container's Withdraw outranks the stock's while it is applicable.
        // Read off the body's **free** capacity and not its total, so it is the
        // same sentence for a part-loaded body as for an empty one, and judged
        // every tick against a pool rebuilt from scratch, so it gates
        // persistence as well as entry. Not carried to Pickup, which keeps the
        // half-empty clause alone: a pile decays and a container does not.
        // Three stores it does not price. **A store that ends** — a tombstone
        // or a ruin — is the Pickup's exemption word for word. **The stock**
        // (ADR 0023): what this line buys is the fall to the tier below, and
        // there is none below the Storage's own Withdraw. **The [[standing
        // body]] at the buffer under its own feet**: the same exception #205
        // makes of a site on a creep's own Post — this clause prices a trip and
        // that row makes none.
        let stock = view.Spatial.Stores |> Map.tryFind storeId |> Option.defaultValue 0

        let worthTheTrip =
            stock * 2 >= creep.FreeCapacity
            || (Map.tryFind storeId view.Spatial.TargetKinds |> Option.exists isTransient)
            || pooled.Priority >= priorityOfTier StockDraw
            || (buffer && isStandingBody view.Tuning creep)

        has Carry
        && halfEmpty
        && worthTheTrip
        && not (Atlas.workHeavy atlas creep.Name)
        && (has Work || not buffer)
        // A standing body fetches from the buffer at its feet and from nowhere
        // else (#206, ADR 0046): its one Carry is one trip's worth, and a trip
        // to the Storage — or across a Seam to a pile — is the commute the row
        // was shaped to never make.
        && (buffer || not (isStandingBody view.Tuning creep))
    // The Withdraw gate without its one target-shaped clause: a Carry part,
    // room to put the energy, and ADR 0016's comparative gate — a Work-heavy
    // body's intake is digging, and picking a pile up off the ground is no more
    // its work than drawing a container is. The buffer clause has no
    // counterpart here: ADR 0019 shuts a Work-less body out of the
    // *controller's* container, and a pile is nobody's buffer.
    | Pickup _ ->
        has Carry
        && halfEmpty
        && not (Atlas.workHeavy atlas creep.Name)
        && not (isStandingBody view.Tuning creep)
    // Its two body clauses are read a second time out of line by
    // `canRefill`, beside Withdraw's (ADR 0050) — the Energy clause is not,
    // being a state and not a fact about the body.
    | Refill _ -> has Carry && creep.Energy > 0 && not (isStandingBody view.Tuning creep)
    // The body gate on Build (#157, widened to every Build by #234), here for
    // the same reason ADR 0016's Withdraw gate is: the ladder lifts a site over
    // the Task that was pinning the body, and a rank the whole colony shares is
    // exactly what travel cost can no longer thin. A full Anchor whose Post has
    // no standing container under it loses Harvest, and was then outranked off
    // its own controller and walked fifty tiles at four to seven ticks a step
    // to spend one Carry into a 5,000-progress site. A heavy body's cross-room
    // work is a Post and never a delivery (ADR 0020), so the switch is light
    // bodies' work, and what it costs the colony is one body's walk and never a
    // garrison's Post. The gate followed the *tier* and now follows the body,
    // #234 having lifted the ordinary **home** site a rung over the Upgrade
    // that was the whole of what travel cost pinned the Anchor with. And one
    // exception over both gates, which is #205's whole change: a container site
    // **under the body's own feet, on its own Post**
    // (`Atlas.standsOnPostSite`). Both prohibitions are about a walk, and
    // neither reaches a site the body is standing on.
    | Build siteId ->
        has Work
        && creep.Energy > 0
        && (Atlas.standsOnPostSite atlas creep.Name siteId
            || (not (isStandingBody view.Tuning creep) && not (Atlas.workHeavy atlas creep.Name)))
    // Repair leaves Upgrade's arm with ADR 0046's gate (a delivery, and a
    // standing body's Carry is one trip's worth), and the two stay
    // otherwise identical: a Work part and something to spend.
    | Repair _ -> has Work && creep.Energy > 0 && not (isStandingBody view.Tuning creep)
    // The one Task the whole row exists for, and so the one place the standing
    // gate must not appear (ADR 0046): a standing body spends its Work into the
    // controller from where it stands. And the sixth gate, which is that
    // sentence's other half (ADR 0048): a Work-heavy body spends its Work into
    // the controller only from where it already stands, because it is the walk
    // that is the loss. ADR 0016 accepted one commute — "a full Anchor off-post
    // matching Upgrade once empties it and converges" — but there is no *once*:
    // every release puts the same body back at this gate. The Dual Seat and the
    // buffer-side row are exactly the shapes this leaves standing (ADR 0020,
    // ADR 0046), both already inside the Work Area.
    | Upgrade _ ->
        has Work
        && creep.Energy > 0
        && (not (Atlas.workHeavy atlas creep.Name)
            || Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
        // A standing body holds no commuting body (ADR 0046) and the borrowed
        // Upgrade is a commute across the Seam (#213): the lift that sends the
        // pioneers must not send the home upgraders after them. Their own
        // controller stays the one Task the row exists for, ungated.
        && not (pooled.Borrowed && isStandingBody view.Tuning creep)
    // Part arithmetic and nothing else (ADR 0006): a reservation is pushed up
    // by CLAIM parts, so a body without one can no more reserve than a
    // Work-less one can dig, and a body with one asks for no energy state.
    | Reserve _ -> has BodyPart.Claim
    // The same part arithmetic, for the same reason (ADR 0047): the engine's
    // `claimController` is a CLAIM part's act, and a claimer carries nothing.
    | Claim _ -> has BodyPart.Claim
    // The same part arithmetic once more (ADR 0006, ADR 0056): an ATTACK part
    // is what makes a body a Fighter and the only thing that kills an invader,
    // and a body carrying one asks for no energy state — it spends nothing. No
    // room clause beside it: the [[work area]] is that room's ring and the
    // walk to it is what travel cost prices, exactly as an outpost's Harvest is
    // offered to a body standing at home. Spelled through the row predicate the
    // [[body class]] ladder itself reads (`isGuardBody`), so "a Fighter body"
    // is one sentence here and in `bodyClassOf` and the [[capacity]] beside it
    // cannot come to disagree with the gate.
    | Guard _ -> isGuardBody creep
    // Flee asks for no part and no energy state, only for a creep that is being
    // shot at and can run (ADR 0033). Two bodies are exempt, and for opposite
    // reasons. A Work-heavy body **cannot** run: at four to seven ticks a step
    // an Anchor leaving its Post neither escapes nor digs, and the answer for
    // the Post is a rampart (ADR 0034) — which is also why the tile under one
    // is in no Reach. A `Fighter` **will not**: a body carrying an ATTACK part
    // does not run from the creep it was cast to kill, which is the same part
    // test the engine's own `findAttack.js` splits its invaders on (ADR 0056
    // decision 3). Without it a guard standing on the ring is offered both
    // Tasks of the Safety tier and kept in the fight by travel cost alone — the
    // ring being underfoot and any safe tile a walk away — so the tick a raid
    // steps toward it, or a second guard is refused by the room's [[capacity]],
    // the body the colony bought to stand still walks away from the invader it
    // was bought for. Spelled through the row predicate the [[body class]]
    // ladder reads (`isGuardBody`), exactly as the Guard's own gate above is:
    // the two clauses are one sentence about one class, and a fighting body the
    // colony was handed rather than cast answers both.
    | Flee ->
        not (isGuardBody creep)
        && not (Atlas.workHeavy atlas creep.Name)
        && standsInReach threats atlas creep.Name

/// The action Intent a Task asks of a creep, or None for a Task with no
/// action: Flee is movement and nothing else (ADR 0033), and the Emitter
/// issues it none.
let private intentFor atlas (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> Some(HarvestSource(creep.Name, sourceId))
    // The same Intent for a tombstone or a ruin as for a container (#167):
    // the engine's `withdraw` is one method over every store, so the
    // Intent's name is the only thing that says "structure" and the
    // Executor hands it whatever `getObjectById` answers with.
    | Withdraw storeId -> Some(WithdrawEnergyFromStructure(creep.Name, storeId))
    // The reflex's own Intent, issued for a creep that walked: one act, one
    // vocabulary, whether the energy was underfoot already or was the reason the
    // creep came. Which is why an arriving picker spells it twice and `decide`
    // keeps one — this Task owns its own act, and the reflex is what gives way.
    | Pickup pileId -> Some(PickupEnergy(creep.Name, pileId))
    // One Task, one act, and — since ADR 0054 — sometimes many structures: a
    // [[refill cluster]]'s Refill names a place, and *which* member of it the
    // energy lands in is settled here, at arrival, off the tile the body
    // actually stands on (`Atlas.refillTarget`). Every other Refill resolves
    // through the same call, so the Emitter has one line and not a branch.
    | Refill structureId ->
        Atlas.refillTarget atlas creep.Name structureId
        |> Option.map (fun target -> TransferEnergyToStructure(creep.Name, target))
    | Build siteId -> Some(BuildSite(creep.Name, siteId))
    | Repair structureId -> Some(RepairStructure(creep.Name, structureId))
    | Upgrade controllerId -> Some(UpgradeController(creep.Name, controllerId))
    | Reserve controllerId -> Some(ReserveController(creep.Name, controllerId))
    | Claim controllerId -> Some(ClaimController(creep.Name, controllerId))
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
    | Flee -> "🏃"
    | Guard _ -> "⚔️"

/// The [[threat]] a [[guard]] swings at, out of the ones standing in the room
/// its Task names and passing the caller's own gate (ADR 0056): **the one
/// nearest a [[post]] of that room**, ties by id. That is "between the invader
/// and the [[anchor]]" said in this colony's vocabulary and the only place the
/// Anchor enters the geometry — the engine's own `findAttack.js` chases the
/// closest hostile creep by path, so a guard standing on the ring of the invader
/// nearest the Post *is* between it and everything behind it, and a tile set of
/// ours would be a second, weaker spelling of a fact the engine already
/// guarantees. With no Post standing, the nearest Threat to the guard — a room
/// with no garrison in it has nothing to stand in front of, so the body fights
/// what is closest. A Threat and never "a hostile": the healer beside an invader
/// is what the row's count rule prices, not what its ATTACK parts are spent on.
/// None where the room holds none, or where the projection places the guard
/// nowhere (ADR 0004).
///
/// **The gate is the caller's and stands ahead of the choice**, which is where
/// ADR 0056 decision 2's one sentence reads it behind: a Threat the swing cannot
/// reach is not a Threat this answer is about. Ordered the other way round, a
/// guard standing on the ring of the *second* invader of a two-creep raid is
/// handed the one nearest the Post, finds it three tiles off, and swings at
/// nothing while the invader beside it deals 40 a tick — 90 damage a tick
/// forgone for a pick that moves nothing else, the mover aiming at the whole
/// ring either way. Where the nearest-Post Threat is in reach — the 90% raid of
/// one invader, and every case the ADR argues about — the two readings answer
/// alike, which is why this narrows decision 2 rather than overturning it.
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

/// Self-preservation beside any Task or none: only damage and an active HEAL
/// part invite a heal. Existing actions own their channels; a reflex must never
/// suppress a swing, a harvest, construction or another chosen action.
let selfHeal (view: ColonyView) (plan: Fabot.Core.IntentPlan.Plan) =
    (plan, view.Creeps)
    ||> List.fold (fun plan creep ->
        if
            creep.Hits.Hits < creep.Hits.HitsMax
            && (Map.tryFind Heal creep.Body |> Option.defaultValue 0) > 0
        then
            match Fabot.Core.IntentPlan.tryAdd (HealCreep(creep.Name, creep.Name)) plan with
            | Ok healed -> healed
            | Error _ -> plan
        else
            plan)

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position, and — for Harvest alone —
/// only while the source holds energy (ADR 0025). Anticipatory dispatch and the
/// occupancy surcharge (ADR 0008) both price a walk high enough to land a creep
/// a tick or two early, so the gate is what keeps the engine's
/// ERR_NOT_ENOUGH_RESOURCES spam structurally impossible. The Guard is the one
/// Task judged outside that gate: its acts reach a creep the projection places
/// nothing for, so `Atlas.mayAct` — which asks where a Task's *target* stands —
/// answers false for it on every tick, and the range it is really gated on is
/// the swing `guardIntent` measures itself (ADR 0056).
let private actionIntents
    (view: ColonyView)
    atlas
    (threats: Threats)
    (creep: CreepInfo)
    (task: Task)
    : Intent list =
    let drained =
        match task with
        | Harvest sourceId -> ticksToRestock view sourceId > 0
        | Withdraw _
        | Pickup _
        | Refill _
        | Build _
        | Repair _
        | Upgrade _
        | Reserve _
        | Claim _
        | Guard _
        | Flee -> false

    match task with
    | Guard room -> guardIntent view atlas creep room |> Option.toList
    | _ ->
        if
            Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task)
            && not drained
        then
            intentFor atlas creep task |> Option.toList
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

    // Every assigned creep says its Task's glyph every tick; unassigned
    // creeps say nothing.
    let says =
        view.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name assigned
            |> Option.map (fun task -> SayCreep(creep.Name, glyphFor task)))

    actions @ says
