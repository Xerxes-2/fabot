/// The rows and how many bodies each is owed: source output and haul demand
/// (ADR 0012), the guard, reserver and upgrader rows, and the workforce target
/// they sum to. Answers "how many", never "who".
[<AutoOpen>]
module Fabot.Core.Decide.Quota

open Fabot.Core
open Fabot.Core.Types

/// What one body of this shape hauls in a trip: its Carry parts at the engine's
/// per-part capacity. Two readers turn Carry parts into energy — the hauler
/// quota divides a source's output over a round trip by it (ADR 0012), and a
/// Withdraw's cap divides its store's stock by it — so the arithmetic is
/// written once and neither can grow a second per-part rule.
let internal carryCapacityOf body =
    partCountIn body Carry * Engine.carryPartCapacity

/// Ceiling division over the quota rows' arithmetic: a quota that came out a
/// fraction of a body hires the whole body (ADR 0012 for the hauler row, ADR
/// 0037 for the worker row), because the fraction a floor drops is demand
/// nobody is hired for. A numerator at or below zero lands at or below zero —
/// F# divides toward zero — and each row's own floor answers for it.
let internal ceilDiv numerator divisor = (numerator + divisor - 1) / divisor

/// What one source of a room the colony holds this way is worth per tick (ADR
/// 0042): the held rate in a room this colony owns or reserves, half of it in a
/// room nobody holds. The whole rate rule, so the census signature can sign
/// exactly what the memoised quota reads rather than a paraphrase of it. Owned
/// **or** reserved, never reserved alone: the engine gives a room carrying
/// either the same 3,000 a cycle, and the colony's own room is owned while
/// nothing reserves it, so "reserved, or half" would price the two home sources
/// at five each.
let internal heldRateOf (control: RoomControlInfo) =
    if
        control.Owner = Ownership.Ours
        || control.Reservation
           |> Option.exists (fun held -> held.Holder = ReservationHolder.Ours)
    then
        Engine.heldOutputPerTick
    else
        Engine.neutralOutputPerTick

/// One source's **rate** per tick (ADR 0042), read off the room it stands in:
/// what the rock regenerates, and so the ceiling on what anything standing over
/// it can take out. A fact read per source and not a module constant, because a
/// reservation can lapse and quotas sized for the held rate against a source
/// yielding five overbuild their rows twofold. The rate and not the output:
/// what a Post is *worth* is what the body garrisoning it digs, which is this
/// number only while the row's cast can reach it (`sourceOutputOf`). Two
/// readers want the ceiling itself — `postWorkCapsOf`, which would otherwise
/// size the body off a number the body decides, and the cap inside
/// `sourceOutputOf`. None for a source in a room the colony has no vision in,
/// and for one the projection does not place (ADR 0004): unpriceable is not
/// half, and a blind outpost must not hire against income the colony has no
/// evidence for.
let internal sourceRateOf (view: ColonyView) atlas (sourceId: string) : int option =
    Atlas.targetRoom atlas sourceId
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.map heldRateOf

/// Whether a source is posted: whether a container stands on one of its Seats,
/// or a Dual Seat makes one of them a Post without a structure — the switch
/// that admits a source into the quotas at all (ADR 0042). One spelling, read
/// by the anchor row's ceiling and by the income base's own split, so a rule
/// that narrows what counts as posted cannot narrow it for one of the two
/// alone. Judged in the source's own room, by `Atlas.standingPostsOf` and not
/// by testing its Seats against the home room's Posts: a `Pos` carries no room,
/// so a home Post on an outpost Seat's coordinates would read that outpost
/// source as posted with no container under it — a phantom ten a tick in the
/// income base, and a phantom Anchor place beside it.
let private isPosted atlas (s: SourceInfo) =
    Atlas.standingPostsOf atlas s.Id |> Set.isEmpty |> not

/// **Every [[post]]'s own Work ceiling** (ADR 0021 as ADR 0042 narrows it and
/// ADR 0053 pairs it): the saturation of the rock that Post seats, plus the one
/// spare Work. A source under no reservation regenerates half as much, and six
/// Work on it drain it in 125 ticks and then idle for 175. **A Post and no
/// longer the set**, which is the whole of ADR 0053: folded into one
/// colony-wide `List.max`, the answer was the held ceiling in every state a
/// colony with one posted home source can reach, and an outpost whose
/// reservation had lapsed went on being garrisoned at six Work against a rock
/// giving five for ever. What pairs a body to a rock without a role is not the
/// caster's knowledge but the **vacancy** it is casting into (`planSpawns`,
/// which walks the empty Posts richest first). Two other readers ask this map
/// for a Post they already hold — the amortization, and a [[lead]] pricing the
/// incumbent's successor. Richest first, and every fallback answers the largest
/// ceiling the rule gives, because an over-sized Anchor wastes 300 energy once
/// in 1,500 ticks where an under-sized one loses four energy a tick for its
/// whole life; a Post whose room the colony cannot price keeps the **held**
/// ceiling (ADR 0004). The **ground** census and not the income one, so the map
/// has one entry per Anchor the colony hires and the amortization can charge
/// them one for one. Keyed by the Post's own [[room position]] and never a bare
/// tile (ADR 0041): two sources whose Seats overlap share a Post tile, and the
/// richer rate keeps it.
let internal postWorkCapsOf (view: ColonyView) atlas : Map<RoomPos, int> =
    view.Sources
    |> List.collect (fun s ->
        let cap =
            sourceRateOf view atlas s.Id
            |> Option.map workCapOf
            |> Option.defaultValue heldWorkCap

        Atlas.postsOf atlas s.Id |> Set.toList |> List.map (fun tile -> tile, cap))
    |> List.fold
        (fun caps (tile, cap) ->
            match Map.tryFind tile caps with
            | Some held when held >= cap -> caps
            | _ -> Map.add tile cap caps)
        Map.empty

/// What one source is **worth to the quotas that read a store** (ADR 0042 as
/// #208 amends it): what the Anchor row's cast digs there, capped at the rate
/// its room pays. A Post yields what the body garrisoning it takes out of it,
/// and a bank that cannot buy the Work to drain a source does not earn ten a
/// tick because the room would have paid ten — at a 300 bank the row casts
/// `2W/1C/1M`, which digs four, so a child with two Posts read twenty a tick of
/// income it never earned and hired eighteen workers off it. **The row's cast
/// at this bank, and emphatically not the living Anchor's body**: a quota read
/// off a living body oscillates on that body's death, where what the row
/// *casts* is a colony fact (ADR 0006). Unpriceable stays unpriceable (ADR
/// 0004).
let private sourceOutputOf (view: ColonyView) atlas (sourceId: string) : int option =
    sourceRateOf view atlas sourceId
    |> Option.map (fun rate ->
        // The Work the row would cast for this rock's own Post times
        // HARVEST_POWER — the same `anchorBodyFor` triple the amortization
        // charges that Post at, so the two readings cannot drift apart.
        let dug =
            partCountIn (anchorBodyFor (workCapOf rate) view.Bank.Capacity) Work
            * Engine.harvestPerWork

        min rate dug)

/// The hauler row's quota rule (ADR 0012) — the row's colony fact, per ADR
/// 0006's law that a row arrives with its quota or not at all: ceil(Sigma over
/// the source containers of round-trip travel ticks to the colony's **sinks** x
/// that container's own source's output, / the cast body's carry capacity), so
/// a farther container hires proportionally more haul capacity and never
/// quietly overflows. No source containers, or unreachable geometry, hire
/// nothing. **The sinks are where this colony's energy is actually spent** (ADR
/// 0052 decision 4), and there are three: the spawn/extension cluster, the
/// controller's [[buffer]] and the [[storage]]. The cluster is one place and
/// not one per spawn — the extensions ring the spawns and a hauler filling them
/// walks to that ring once — so several spawns resolve at the cheapest. Each
/// contributes a leg while it stands and none while it does not. The spawn
/// alone is what this read before, and a child whose buffer sat thirty tiles
/// from its north Post hired **one** hauler off the spawn leg while its
/// containers overflowed: the energy really was flowing to the controller, and
/// the quota was priced as if it flowed to the spawn. **Each container's flow
/// is spread over the sinks it can price**, and that is an admission rather
/// than a measurement: this layer knows what is produced and where it is spent,
/// and nothing here knows in what proportion. It errs **both ways** — larger
/// wherever a sink stands further off than the cluster, smaller wherever one
/// stands nearer — and neither direction is free: a body too many idles, and a
/// body too few leaves a room's income on the ground. A container that can
/// price **no** sink hires nobody (ADR 0004). **One rounding, for the colony**
/// (ADR 0049, succeeding ADR 0012 and ADR 0037 on the granularity alone): the
/// demands are summed first and the ceiling taken once. Rounding each container
/// up on its own bought a body per fraction, because a hauler is not the
/// property of the container it was hired for: a Withdraw's capacity is its own
/// store's stock divided by a hauler load, so the shared integer is spent where
/// the energy actually stands. The cap is a **capacity and not an order** —
/// `tierOf` files every source container's Withdraw on the feeding tier alike
/// and travel cost ranks inside it. What ADR 0012 rejected was the *flat*
/// quota, one hauler per container regardless of distance, and this is the
/// opposite of that. The output is that source's and not the colony's (ADR
/// 0042), which is why the fold resolves each tile back to the rock it serves:
/// a container over an unreserved source ships half as much. And that output is
/// what the Post's garrison digs, capped at the rock's rate (`sourceOutputOf`).
/// Every room the projection carries, and not the colony's own alone (ADR
/// 0042): an outpost's container ships its source's energy home across a
/// border, so it hires haul capacity exactly as a home container does, against
/// `Atlas.haulRoundTripTicks` joined on the Seam band, run once per leg because
/// the loaded body and the empty one are two journeys (ADR 0029, ADR 0030). The
/// room is the container's own throughout, carried beside its tile rather than
/// assumed, because a `Pos` names none (ADR 0041). A container the projection
/// places in no room is priced by nothing and hires nobody (ADR 0004), as is a
/// Seam band the body cannot pay a crossing on.
let internal haulerDemandOf (view: ColonyView) atlas : int * HaulDemandRow list * int =
    // Each source container beside the room it stands in and the output of the
    // rock it serves: the tile alone cannot be priced, so a container the
    // projection places in no room, or one the fold cannot resolve to a source
    // whose room it can price, leaves the list here rather than entering the sum
    // at some default rate.
    let sourceContainers =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Container)
        |> List.choose (SpatialInfo.placementOf view.Spatial)
        |> List.choose (fun container ->
            sourceContainerServes view container.Room (RoomPos.pos container)
            |> Option.bind (sourceOutputOf view atlas)
            |> Option.map (fun output -> container, output))

    // One load, for the whole colony, and the row's own body cast at the
    // richest bank: rounding once (ADR 0049) sums demands before it divides, so
    // every term has to be a fraction of the *same* body or the integer at the
    // end counts nothing.
    let body = bodyFor haulerPattern (view.Bank.Capacity)

    let capacity = carryCapacityOf body

    let home = SpatialInfo.homeName view.Spatial

    // The three sinks, each a **place** and not a structure: a sink is a list
    // of tiles that stand for one destination, and the cheapest of them is that
    // sink's leg. The spawn/extension cluster is the list with more than one
    // entry today, and taking its minimum is the old rule's "of several spawns
    // the cheapest wins" read as what it always was. Every tile is the
    // projection's and never `SpawnInfo.RoomName` (ADR 0041).
    let cluster =
        view.Spawns |> List.choose (fun s -> SpatialInfo.placementOf view.Spatial s.Id)

    // The upgrade buffer, off the one derivation the Withdraw gate and the
    // upgrader row's own quota read (`Atlas.controllerContainers`, ADR 0019):
    // built and in the home controller's Upgrade area, so a container *site*
    // beside the controller is a promise and not yet a sink.
    let buffers =
        Atlas.controllerContainers atlas
        |> Set.toList
        |> List.choose (SpatialInfo.placementOf view.Spatial)

    // The Storage while one stands, in the home room alone: it is the
    // colony's stock (ADR 0023) and a colony banks in one room (ADR 0052
    // decision 1), so a Storage standing anywhere else is somebody else's.
    let storages =
        Atlas.storageTilesIn atlas home |> Set.toList |> List.map (RoomPos.at home)

    let sinks =
        [ "cluster", cluster; "buffer", buffers; "storage", storages ]
        |> List.filter (snd >> List.isEmpty >> not)

    // Each container's own haul, priced at the **dearest** sink it can reach
    // and summed over the colony — the fraction of a hauler it asks for, never
    // that fraction rounded. The dearest and not the mean: a cluster holds a
    // few hundred energy and fills in a trip, so the flow that goes on all day
    // is the flow to the far sink, and a quota sized to the mean hired one body
    // for a room whose both containers stood full with the buffer at zero.
    let rows =
        sourceContainers
        |> List.map (fun (container, output) ->
            let priced =
                sinks
                |> List.map (fun (kind, places) ->
                    {
                        Kind = kind
                        Trip =
                            places
                            |> List.choose (Atlas.haulRoundTripTicks atlas body container)
                            |> function
                                | [] -> None
                                | trips -> Some(List.min trips)
                    })

            let trips = priced |> List.choose (fun sink -> sink.Trip)

            {
                Container = container
                Output = output
                Sinks = priced
                Demand =
                    match trips with
                    | [] -> 0
                    | trips -> output * List.max trips
            })

    let demand = rows |> List.sumBy (fun row -> row.Demand)

    // The [[ferry]] (ADR 0052 decision 7): the bodies a mother lends a
    // bootstrapping child, over and above the haul her own containers ask for.
    // Hired per child and capped at `Tuning.FerryLoads`, because what one
    // colony takes of another is written down and bounded and never derived
    // from how much the child could absorb. Priced **from her Storage**, which
    // is what makes it a lend and not a second economy: the stock is the only
    // energy a mother has that her own rows are not already hired against (ADR
    // 0023). A child whose room she cannot reach, or that has no buffer
    // standing, hires nobody (ADR 0004).
    let ferry =
        if List.isEmpty storages then
            0
        else
            ferryBuffers view
            |> Set.toList
            |> List.choose (SpatialInfo.placementOf view.Spatial)
            |> List.filter (fun tile ->
                storages
                |> List.exists (fun stock ->
                    Atlas.haulRoundTripTicks atlas body stock tile |> Option.isSome))
            // Per **child** and not per store: the lend is a sentence about
            // a colony, and a room the Layout ever planned two buffers in
            // would otherwise buy two ferries off one declaration.
            |> List.map (fun tile -> tile.Room)
            |> List.distinct
            |> List.length
            |> (*) view.Tuning.FerryLoads

    // **A haul that crosses a Seam is never one body** (#279). The rounding
    // above is honest about throughput and says nothing about redundancy, and
    // at the live reading — 1,170 of demand against a 1,200 load, 97.5% of one
    // body — the colony was one death, one detour or one Threat away from
    // losing a source: what a full container at home does is wait, and what a
    // full container in an [[outpost]] does is drop the [[anchor]]'s next
    // fifty on the floor, where it decays, while the replacement walks forty
    // tiles out. That asymmetry is the whole of the argument, so the floor is
    // read off the rows this function already priced — a container standing in
    // a room that is not home — and not off the declaration: a room a
    // [[stand-down]] withdrew asks for nothing here, exactly as it asks for no
    // reserver. #157's argument for two builders, said again for the haul.
    let remote =
        rows
        |> List.filter (fun row -> row.Container.Room <> SpatialInfo.homeName view.Spatial)
        |> List.sumBy (fun row -> row.Demand)

    // The colony's whole haul, rounded once (ADR 0049), and the ferry's own
    // whole bodies beside it: a lend is counted in bodies rather than in
    // tick-energy, so it is added after the division rather than inside it.
    let hired = ceilDiv demand capacity

    (if remote * 2 >= capacity then max hired 2 else hired) + ferry, rows, capacity

/// The hauler quota alone; `haulerDemandOf` is the same arithmetic with
/// its lines kept.
let private haulerQuota (view: ColonyView) atlas : int =
    let quota, _, _ = haulerDemandOf view atlas
    quota

/// What one body of this shape drinks a tick standing at a controller: its Work
/// parts at the rate above.
let private upgradeDrainOf body =
    body
    |> List.sumBy (function
        | Work -> Engine.upgradeDrainPerWork
        | _ -> 0)

/// The reserver row's body for one outpost (ADR 0042): the deficit sizing and
/// the bank truncation, whichever asks for less, never below one block. The
/// deficit arrives as a second capacity ceiling, because "as many whole blocks
/// as capacity buys" is already `reserverBodyFor`'s rule.
let internal reserverBodyWithin claims capacity =
    reserverBodyFor (min capacity (claims * bodyCost reserverPattern.Block))

/// Whether a living body was cast from the guard row: it carries an ATTACK
/// part (ADR 0056). The same part test `findAttack.js` splits the engine's own
/// invaders on, and the one cut no other row of this colony makes — every other
/// row is built out of Work, Carry, Move and CLAIM — so it is asked **first**,
/// beside `Fighter`'s place at the head of the [[body class]] ladder. Read off
/// the parts like every other row predicate (ADR 0006), so a fighting body the
/// colony was handed rather than cast fills this row's quota exactly as one it
/// cast does.
let internal isGuardBody (creep: CreepInfo) = partCount creep.Body Attack > 0

/// How many guards one raided [[outpost]] wants (ADR 0056 decision 1, as #272
/// amends it), which is **0** for the whole of a colony's ordinary life because
/// no room is raided: one guard per declared outpost a [[threat]] stands in
/// this tick, two where one guard block loses the exchange, capped at two
/// and — for the row's own quota — summed over the
/// outposts. A per-tick fact read off vision and nothing remembered between
/// ticks — vision
/// in a guarded outpost *is* the guard — so it falls to 0 the tick the room is
/// clear; it does not decay in between, and it needs none: a cast is 1,500
/// ticks of body and a raid is 1,500 ticks, so one cast covers one raid by
/// construction and the survivor goes on filling the row's `Living`.
///
/// The count compares one block against the raid, independently of the bank
/// and the guards already standing. A lone smallMelee needs one; backed by a
/// smallHealer, its 40 damage kills our 1,000 hits in 25 ticks, before our 30
/// net damage kills its 1,000 hits. That raid needs the second block.
///
/// Whether `blocks` whole `guardPattern` blocks win the exchange against the
/// raid standing in one room (ADR 0056 decision 1, as #280 amends it). Two
/// clocks compared, cross-multiplied to stay in whole numbers: the ticks our
/// blocks need to chew through the raid's **armed** bodies, against the ticks
/// the raid needs to chew through ours.
///
/// Our melee blocks cannot self-heal while attacking: heal suppresses attack.
/// Their survival uses the raid's full damage. A raid that out-heals our damage
/// can never be killed and is not. Healers are priced in the healing and never
/// in the hits — killing them is not what ends the fight, out-damaging them is,
/// and the last armed body down leaves them taking no ground and dealing
/// nothing. The raid's durability is priced at full off its parts, because the
/// projection carries a hostile's body and not its hits (ADR 0007), and
/// over-stating what it can take is the safe direction for a rule that decides
/// whether we fight at all.
///
/// Two readers, which is why it is a rule and not an expression written twice:
/// the guard row asks it of **one** block to size the crowd (below), and ADR
/// 0043's stand-down asks it of the **cap** to decide whether the room is a
/// fight or a withdrawal (`Observe.raidDeadlines`, #257).
let guardBlocksBeat (view: ColonyView) (room: string) (blocks: int) : bool =
    let parts part body = partCountIn body part

    let armed (h: HostileInfo) =
        parts Attack h.Body + parts RangedAttack h.Body > 0

    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    let raidDamage =
        raid
        |> List.sumBy (fun h ->
            Engine.attackPower * parts Attack h.Body
            + Engine.rangedAttackPower * parts RangedAttack h.Body)

    let raidHealing = raid |> List.sumBy (fun h -> Engine.healPower * parts Heal h.Body)

    let raidHits =
        raid
        |> List.filter armed
        |> List.sumBy (fun h -> Engine.partHits * List.length h.Body)

    let block = guardPattern.Block

    let ourDamage =
        blocks
        * (Engine.attackPower * parts Attack block
           + Engine.rangedAttackPower * parts RangedAttack block)

    let ourHits = blocks * Engine.partHits * List.length block

    if raidDamage = 0 then
        true
    elif ourDamage <= raidHealing then
        false
    else
        raidHits * raidDamage < ourHits * (ourDamage - raidHealing)

/// **The count reads the raid and never our own answer to it** (#272). Priced
/// against the guards *standing* in the room it was not monotone — 2 while one
/// stood, 1 the tick the second arrived — so the escalation cancelled itself:
/// the reinforcement it had just bought was `CapacityFull`-evicted on arrival,
/// onto a [[flee]] whose safe set is that same room, where it stood for its
/// whole life holding the count down with its own damage. Nothing about the
/// bodies already sent enters this, which is what makes the number monotone in
/// the raid: the row hires the second guard, the Task's cap admits it, and
/// neither can retract while the raid is unchanged. Never a living body, for
/// the reason ADR 0042 stopped reading the living Anchor's (#208): a quota
/// priced off a body that stands moves when that body dies. The block is a
/// whole 90, so the damage term is never zero. A raid with enough armed
/// bodies can buy a second guard even without healing.
///
/// Vision is the whole of what this reads (ADR 0004): an outpost the colony
/// cannot see this tick carries no hostiles and asks for no guard, which is the
/// same zero a quiet room contributes. The [[home room]] is not in this list —
/// a raid at home casts no guard and is the [[keep]]'s business (ADR 0034), and
/// the spawn hold would refuse the cast anyway.
///
/// **One room's number, and the row's quota is its sum** (ADR 0056 decision 2):
/// the Guard pooled for that room is capped at exactly this, so the row hires
/// what the pool admits and the Matcher counts holders against the number the
/// Planner set, which is ADR 0052 decision 6. Asked only of a room
/// `guardedOutposts` has already answered for — a room with no Threat in it is
/// not one guard but none.
let internal guardsWanted (view: ColonyView) (room: string) : int =
    if guardBlocksBeat view room 1 then 1 else Engine.guardCap

/// The guard row's quota: `guardsWanted` over every raided outpost, summed.
let internal guardQuota (view: ColonyView) : int =
    guardedOutposts view |> List.sumBy (guardsWanted view)

/// The reserver row's quota and its sizing, which are one rule with two faces
/// (ADR 0042, ADR 0006's law that a row arrives with its quota): one reserver
/// per **declared** outpost, each wanting `ceil((5000 - ticks this colony
/// holds) / 600)` CLAIM parts. The list's length is the quota; each entry is
/// what that outpost's body asks for. No state is kept between ticks — the
/// deficit recomputes from the reservation itself. A **candidate colony** takes
/// one more entry, of a single block (ADR 0047), and its room leaves the
/// reservation demands, because a controller carries one Task and a candidate
/// colony's is the Claim; the body is the same `[Claim; Move]` either way,
/// which is why this is one row and not two. Which rooms count is
/// `declaredOutposts`, the derivation this row shares with the guard row. The
/// *rooms* drop out and every cast this tick is sized at the largest demand in
/// the list: the quota counts bodies, and which controller each finished body
/// holds is the Matcher's, priced by travel cost. Over-buying is the safe
/// direction (ADR 0026), and the bank truncates it anyway. **The bank must
/// afford one block**, or the row hires nobody: a colony that cannot buy a
/// reservation does not hold one, and a row hired against a body it can never
/// buy is an addend of the Workforce target no cast will pay off.
let internal reserverClaimsOf (view: ColonyView) : int list =
    let heldTicks room =
        view.RoomControl
        |> Map.tryFind room
        |> Option.bind (fun control -> control.Reservation)
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Ours)
        |> Option.map (fun held -> held.TicksToEnd)
        |> Option.defaultValue 0

    // The candidate colonies this tick, each asking for **one** block (ADR
    // 0047): the Claim row is this row, because both bodies are CLAIM bodies
    // and a second pattern row would be the same block under a second name (ADR
    // 0006), so `patternOf` reads a claimer back as a reserver and the casting
    // order, the gap and the amortization all count it as one. One block and
    // never the deficit's nine: a claim is one act by one CLAIM part, finished
    // the tick it succeeds. Their rooms are already out of `declaredOutposts`,
    // which is the same fact read from the other end.
    let claims = claimTargets view

    if view.Bank.Capacity < bodyCost reserverPattern.Block then
        []
    else
        let reserved =
            declaredOutposts view
            |> List.map (fun room ->
                ceilDiv (Engine.reservationCap - heldTicks room) Engine.claimLifetime |> max 1)

        reserved @ (claims |> List.map (fun _ -> 1))

/// The colony's surplus over one creep's lifetime: the income the two upgrade
/// rows are hired out of, written once because both read it and a paraphrase
/// would let them hire against different money (ADR 0012, ADR 0046). Income is
/// counted per source at that source's own output and never at a colony-wide
/// ten (ADR 0042): an unreserved source is worth half a held one, and a posted
/// source whose room the colony cannot see is worth nothing at all rather than
/// half (ADR 0004). An output is what the garrison digs, capped at the rock's
/// rate, and the row is charged its replacement at that same body, so credit
/// and charge are one cast — since ADR 0053, Post by Post. From that income the
/// reserver, anchor and hauler rows' amortization is deducted: those three are
/// hired off facts about the *ground*, so their price is settled before the
/// surplus has a number, while the two rows hired out of the surplus itself are
/// charged inside `workforceTarget`.
let internal surplusOverLifetime
    (view: ColonyView)
    atlas
    reserverClaims
    (anchorPostCaps: Map<RoomPos, int>)
    haulerQuota
    =
    let capacity = view.Bank.Capacity

    // The row's own body, once, times the places it hires: every reserver cast
    // this tick carries the largest outstanding demand, so the charge is priced
    // off that same body and never off a per-room one the casting step would not
    // have cast. Scaled from a CLAIM body's own 600-tick life onto the 1,500 the
    // rest of this sum is written in (ADR 0042): a reserver is replaced two and
    // a half times over one worker's life, and charging it once would hire an
    // upgrade mouth the reservation is really paying for.
    let reserverCost =
        if List.isEmpty reserverClaims then
            0
        else
            List.length reserverClaims
            * bodyCost (reserverBodyWithin (List.max reserverClaims) capacity)

    // The anchor row charged **Post by Post**, each at the body the casting
    // step would actually buy for that Post (ADR 0053): a row whose bodies
    // shrank with a lapsed reservation while its amortization went on deducting
    // the six-Work price would hire an upgrade mouth fewer than the income
    // really feeds, and a quota times one ceiling is that same mistake wherever
    // the colony's Posts disagree.
    let amortization =
        (anchorPostCaps
         |> Map.fold (fun total _ cap -> total + bodyCost (anchorBodyFor cap capacity)) 0)
        + haulerQuota * bodyCost (bodyFor haulerPattern capacity)
        + reserverCost * Engine.creepLifetime / Engine.claimLifetime

    // Summed over the posted sources at each one's own output, never a count
    // times a constant (ADR 0042): a source the colony cannot price contributes
    // nothing, the same zero it would contribute by not being posted.
    let income =
        view.Sources
        |> List.filter (isPosted atlas)
        |> List.sumBy (fun s -> sourceOutputOf view atlas s.Id |> Option.defaultValue 0)

    income * Engine.creepLifetime - amortization

/// Whether a body this module has sized is a standing body: `standingParts`
/// over a part list rather than over a living creep's part map.
let private isStandingCast (tuning: Tuning) body = standingParts tuning (partsOf body)


/// Whether a living body is a **standing body** (ADR 0046): it carries fewer
/// than one Carry part per four Work — `Carry * 4 < Work`. Part arithmetic and
/// nothing else, like every other row-reading predicate here (ADR 0006), and a
/// fact about a *body* rather than about a row: the upgrader row's `11W/1C/11M`
/// is one, and so is the anchor row's `6W/1C/1M`. The gate that reads it is
/// `applicable` below, on Build, Repair and Refill — on Pickup and on every
/// Withdraw but the buffer's, and since #235 on Harvest, where the body that
/// keeps it is the Work-heavy one (ADR 0016) and not the standing one, the
/// anchor row's `6W/1C/1M` being both. What is left exactly as its own gates
/// already had it is the working life the upgrader row was shaped for: it draws
/// from the buffer at its feet (ADR 0019, through ADR 0016's gate) and spends
/// into the controller in place. Digging is not part of it — #206 left Harvest
/// open on the reasoning that travel cost would keep the row beside its buffer,
/// and an empty buffer leaves the row nothing else applicable at all.
let internal isStandingBody (tuning: Tuning) (creep: CreepInfo) = standingParts tuning creep.Body

/// What one body of the upgrader row eats per tick: every Work part of the
/// row's cast at the richest bank, at the controller's own per-Work rate
/// (ADR 0046). Never below one — the row's sizing rule floors at a pair —
/// so the quota below always has a divisor.
let private upgraderDrain capacity =
    upgradeDrainOf (bodyFor upgraderPattern capacity)

/// The upgrader row's quota (ADR 0046): the surplus divided by what one
/// standing body **costs the colony over a life** — the energy its Work drinks
/// plus the body itself — rounded **down**, with the remainder handed on to the
/// worker row, whose own division rounds up (ADR 0037). **The divisor carries
/// the row's own replacement cost.** Read as the drain alone, a surplus of
/// 33,100 over a 16,500 drain hires two bodies that cost 36,400 to run and
/// replace: the worker row's income term goes to zero and the colony has
/// promised more over a lifetime than its rocks bring in. What a row pays for
/// is the mouth *and* the body. Only one of the two rows may round up: ADR 0037
/// admits an oversell bounded by *one body's* lifetime drain, and two rows
/// rounding up against the same number sell that bound twice — the rounding
/// goes to the row whose oversold body is smaller, which is the worker row.
/// **Non-zero only while a built controller container stands in the room**: the
/// buffer is this row's working ground (ADR 0046 against ADR 0012's
/// generalization), and a site there is a promise, not a store to withdraw
/// from. A negative surplus hires none.
let internal upgraderQuota (view: ColonyView) atlas surplus =
    let capacity = view.Bank.Capacity

    if
        Set.isEmpty (Atlas.controllerContainers atlas)
        || not (isStandingCast view.Tuning (bodyFor upgraderPattern capacity))
    then
        0
    else
        surplus
        / (upgraderDrain capacity * Engine.creepLifetime
           + bodyCost (bodyFor upgraderPattern capacity))
        |> max 0

/// The worker row's floor (ADR 0046): the row's income term is whatever the
/// upgrader row has not eaten, and beside a buffer that can still be nothing at
/// all — the remainder is bounded by one standing body's lifetime drink, and
/// that row's own replacement is charged against it first. A colony with no
/// generalist builds nothing and repairs nothing: a standing body is shut out
/// of all three deliveries and the hauler row carries no Work. Two while
/// anything stands in the Build or Repair pool, one otherwise. Two, because
/// since ADR 0042 a builder crosses a Seam and the home room's own sites are
/// unattended for the fifty ticks of that walk; one, because hiring the second
/// against no pool would be hiring for a job that does not exist.
let private workerFloor (tasks: Task list) =
    let building =
        tasks
        |> List.exists (function
            | Build _
            | Repair _ -> true
            | _ -> false)

    if building then 2 else 1

/// Workforce target (ADR 0012, ADR 0046, ADR 0056): six addends, each a pattern
/// row's own colony fact — reservers one per declared outpost, guards one or two
/// per raided one, Anchors one per Post, haulers the throughput quota, upgraders
/// the surplus divided by a standing body's drain, workers the income arithmetic
/// that is left and the pioneers a nursery adds to it (ADR 0047) — floored at
/// `Tuning.MinWorkforce` and derived
/// fresh each tick. A source whose Post is provided for retires its other
/// Seats: one heavy body drains it alone. An unposted source of the home room
/// still contributes its Seat count, its output being spoken for by the seat
/// crews that walk it, so only the posted sources' output is income. An
/// unposted source of an **outpost** contributes nothing at all (ADR 0042): the
/// seat-crew justification presumes the walk is cheap, and across a border it
/// is not. A standing container is the switch admitting an outpost into the
/// economy: until one stands the room is invisible to every quota but the
/// reserver's, and the tick it stands the source enters the two that read a
/// store, a hauler term at its own round trip and a share of the income base at
/// its own output. The Anchor place moved one step earlier with the container's
/// *site*. The reserver row is the quota this switch does *not* gate — it is
/// what makes the container possible — arriving as `reserverClaims`, whose
/// length is the addend and whose largest entry prices the amortization. The
/// income and the three ground-hired rows' amortization arrive together as
/// `surplus`, read here and by `upgraderQuota` alike. The guard row is an addend
/// like the rest (ADR 0056) and is charged nowhere else: it is 0 for the whole
/// of an ordinary life, and a guard left out of the target would have the
/// deficit read the body it is alive as one of the generalists the income
/// already paid for — a raid would quietly retire a worker for as long as the
/// guard stood.
let internal workforceTarget
    (view: ColonyView)
    atlas
    (tasks: Task list)
    reserverClaims
    guardQuota
    anchorQuota
    haulerQuota
    upgraderQuota
    surplus
    =
    let home = SpatialInfo.homeName view.Spatial

    let unpostedSeats =
        view.Sources
        |> List.filter (isPosted atlas >> not)
        |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some home)
        |> List.sumBy (fun s -> Atlas.seats atlas s.Id |> Option.defaultValue 0)

    let capacity = view.Bank.Capacity

    let workerDrain = upgradeDrainOf (bodyFor workerPattern capacity)

    // What the standing row takes out of the surplus before the commuting one
    // is hired against the rest (ADR 0046): the energy its Work drinks over a
    // lifetime, and the row's replacement cost over the same lifetime, priced
    // at the body the casting step would actually cast.
    let upgraderCost =
        upgraderQuota * upgraderDrain capacity * Engine.creepLifetime
        + upgraderQuota * bodyCost (bodyFor upgraderPattern capacity)

    // Rounded up through the same ceilDiv as the hauler row (ADR 0037): the
    // granularity a floor would drop is a whole worker body's Work, which grows
    // with RCL, and the income it drops leaks every tick while the body it
    // oversells is paid for out of stock.
    let incomeWorkers =
        ceilDiv (surplus - upgraderCost) (workerDrain * Engine.creepLifetime) |> max 0

    // The pioneers (ADR 0047 decision 4): while a room this colony has claimed
    // still has no spawn in it, the mother hires `Tuning.PioneerCount` more
    // generalists to go and raise one. Hired off a fact about the *world* and
    // not out of the surplus — a nursery is a room a human declared and the
    // colony has taken, exactly as the reserver row is hired off a declared
    // outpost — so it is added to the row rather than divided out of what the
    // upgrader row left. On top of the whole row and outside its floor: the
    // floor is the smallest crowd that can take a delivery at all (ADR 0046),
    // and these bodies are hired for a delivery that exists whatever else the
    // colony is doing. No term of `surplus` answers for them, which is the
    // worker row's pre-existing shape. The addend outlives the nursery and runs
    // on through the bootstrap window, flat over both [[stage]]s for the reason
    // it is flat over two nurseries.
    let pioneers =
        let raising room =
            isNurseryRoom view room || isBootstrapRoom view room

        if view.Stages |> Map.exists (fun room _ -> raising room) then
            view.Tuning.PioneerCount
        else
            0

    // The generalist row's whole share of the target, and the floor sits here
    // rather than on the income term beside it (ADR 0046): both addends hire
    // the same body from the same row, so a colony already running three seat
    // crews has three bodies that can build, and a floor read off the income
    // term alone would hire a fourth against a job that does not exist. What
    // the floor is for is the colony where this sum is *zero*.
    let workerRow =
        (unpostedSeats + incomeWorkers |> max (workerFloor tasks)) + pioneers

    List.length reserverClaims
    + guardQuota
    + anchorQuota
    + haulerQuota
    + upgraderQuota
    + workerRow
    |> max view.Tuning.MinWorkforce
