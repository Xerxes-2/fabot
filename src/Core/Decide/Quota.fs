/// The rows and how many bodies each is owed: source output, haul demand,
/// the guard, reserver, miner, courier and upgrader rows, and the workforce
/// target they sum to. Answers "how many", never "who".
[<AutoOpen>]
module Fabot.Core.Decide.Quota

open Fabot.Core
open Fabot.Core.Types

/// What one body of this shape hauls in a trip. Written once: the hauler
/// quota and a Withdraw's cap both turn Carry parts into energy through it.
let internal carryCapacityOf body =
    partCountIn body Carry * Engine.carryPartCapacity

/// Ceiling division for the quota rows: a fraction of a body hires the whole
/// body. A numerator at or below zero lands at or below zero (F# divides
/// toward zero); each row's own floor answers for it.
let internal ceilDiv numerator divisor = (numerator + divisor - 1) / divisor

/// ADR-0042
/// What one source of a room held this way is worth per tick. Owned **or**
/// reserved, never reserved alone: the colony's own room is owned while
/// nothing reserves it, so "reserved, or half" would price the two home
/// sources at five each.
let internal heldRateOf (control: RoomControlInfo) =
    if
        control.Owner = Ownership.Ours
        || RoomControlInfo.heldBy ReservationHolder.Ours control |> Option.isSome
    then
        Engine.heldOutputPerTick
    else
        Engine.neutralOutputPerTick

/// One source's rate per tick, read off the room it stands in: the ceiling on
/// what anything standing over it can take out. Read per source, not a module
/// constant, because a reservation can lapse. None for a source in a room the
/// colony has no vision in or that the projection does not place: unpriceable
/// is not half.
let internal sourceRateOf (view: ColonyView) atlas (sourceId: string) : int option =
    Atlas.targetRoom atlas sourceId
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.map heldRateOf

/// Whether a source is posted — the switch that admits it into the quotas.
/// One spelling for the anchor row's ceiling and the income base's split.
/// Judged in the source's own room by `Atlas.standingPostsOf`, not by testing
/// its Seats against the home room's Posts: a `Pos` carries no room, so a home
/// Post on an outpost Seat's coordinates would read that outpost source as
/// posted with no container under it.
let private isPosted atlas (s: SourceInfo) =
    Atlas.standingPostsOf atlas s.Id |> Set.isEmpty |> not

/// ADR-0053
/// Every Post's own Work ceiling: the saturation of the rock it seats plus one
/// spare, per Post and never a colony-wide max. Richest first, and every
/// fallback answers the largest ceiling the rule gives; a Post whose room the
/// colony cannot price keeps the held ceiling. Keyed by the Post's room
/// position, never a bare tile: two sources whose Seats overlap share a Post
/// tile, and the richer rate keeps it.
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

/// What one source is worth to the quotas that read a store: what the Anchor
/// row's cast digs there, capped at the rate its room pays. The row's cast at
/// this bank, never the living Anchor's body: a quota read off a living body
/// oscillates on that body's death.
let private sourceOutputOf (view: ColonyView) atlas (sourceId: string) : int option =
    sourceRateOf view atlas sourceId
    |> Option.map (fun rate ->
        // The same `anchorBodyFor` triple the amortization charges that Post
        // at, so the two readings cannot drift apart.
        let dug =
            partCountIn (anchorBodyFor (workCapOf rate) view.Bank.Capacity) Work
            * Engine.harvestPerWork

        min rate dug)

/// ADR-0012
/// The hauler row's quota: the sum over source containers of round-trip ticks
/// to the colony's sinks times that source's output, over the cast body's
/// carry capacity, rounded once for the colony. The three sinks are
/// the spawn/extension cluster, the controller buffer and the Storage; each
/// contributes a leg while it stands. A container that can price no sink hires
/// nobody. Every projected room, not the home room alone: an outpost's
/// container hires haul capacity exactly as a home one does. The cap is a
/// capacity and not an order — `tierOf` files every source container's
/// Withdraw on the feeding tier alike and travel cost ranks inside it.
let internal haulerDemandOf (view: ColonyView) atlas : int * HaulDemandRow list * int =
    // Each source container beside its room and the output of the rock it
    // serves; one the projection places in no room, or that resolves to no
    // priceable source, leaves the list rather than entering at a default.
    let sourceContainers =
        Atlas.idsOfKind atlas (Structure BuiltKind.Container)
        |> List.choose (SpatialInfo.placementOf view.Spatial)
        |> List.choose (fun container ->
            sourceContainerServes view container.Room (RoomPos.pos container)
            |> Option.bind (sourceOutputOf view atlas)
            |> Option.map (fun output -> container, output))

    // One load for the whole colony: the sum is of fractions of the *same*
    // body, or the one rounding at the end counts nothing.
    let body = bodyFor haulerPattern (view.Bank.Capacity)

    let capacity = carryCapacityOf body

    let home = SpatialInfo.homeName view.Spatial

    // Each sink is a list of tiles standing for one destination; the cheapest
    // is that sink's leg. The cluster is one place, not one per spawn.
    let cluster =
        view.Spawns |> List.choose (fun s -> SpatialInfo.placementOf view.Spatial s.Id)

    // The upgrade buffer, off the one derivation the Withdraw gate and the
    // upgrader row read: built, so a container *site* beside the controller is
    // a promise and not yet a sink.
    let buffers =
        Atlas.controllerContainers atlas
        |> Set.toList
        |> List.choose (SpatialInfo.placementOf view.Spatial)

    // The Storage while one stands, in the home room alone: a colony banks in
    // one room, so a Storage standing anywhere else is somebody else's.
    let storages =
        Atlas.storageTilesIn atlas home |> Set.toList |> List.map (RoomPos.at home)

    let sinks =
        [ "cluster", cluster; "buffer", buffers; "storage", storages ]
        |> List.filter (snd >> List.isEmpty >> not)

    // Each container's haul, priced at the **dearest** sink it can reach. The
    // dearest and not the mean: a cluster fills in a trip, so the flow that
    // goes on all day is the flow to the far sink, and a quota sized to the
    // mean hired one body for a room whose both containers stood full with the
    // buffer at zero.
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

    // The mine-to-Storage leg is a sum of its own, rounded apart from the
    // energy's: each mineral container's round trip to the Storage times the
    // miner's cast rate, `Work / 6` Thorium a tick (the extractor's cooldown
    // is five and the intent pass runs before the object pass). The Storage
    // alone and never the three sinks: Thorium goes to the one store nothing
    // can stand on. No Storage standing, or a mine it cannot price, asks for
    // nothing — which is also the tick the pair is not pooled at all.
    let minerRate =
        partCountIn (minerBodyFor view.Tuning.MinerWorkPerMove view.Bank.Capacity) Work
        * Engine.mineralHarvestPerWork

    let mineRows =
        if List.isEmpty storages then
            []
        else
            ourMineralContainerPairs view
            // A mine that cannot be dug asks for no carrier (#262): the same
            // two facts the miner row's own quota reads, so the container
            // alone cannot buy a hauler during the RCL6 build window (the
            // Layout emits extractor and container as two sites) or after the
            // deposit runs to zero under a standing extractor.
            |> List.filter (fun (depositId, _) -> depositIsDiggable view atlas depositId)
            |> List.choose (fun (_, containerId) ->
                SpatialInfo.placementOf view.Spatial containerId)
            // No store of a child's is the mother's to draw — the same filter
            // the Task pool reads over this list.
            |> List.filter (fun container -> not (List.contains container.Room view.Borrowed.Rooms))
            |> List.map (fun container ->
                let trip =
                    storages
                    |> List.choose (Atlas.haulRoundTripTicks atlas body container)
                    |> function
                        | [] -> None
                        | trips -> Some(List.min trips)

                {
                    Container = container
                    Output = minerRate
                    Sinks = [ { Kind = "storage"; Trip = trip } ]
                    Demand =
                        match trip with
                        | None -> 0
                        // One division here rather than in the sum: the rate
                        // is a fraction of a Thorium a tick and every other
                        // term is whole energy a tick. The truncation is
                        // under one unit against a load of hundreds.
                        | Some trip -> minerRate * trip / Engine.mineralHarvestCycle
                })

    let demand = rows |> List.sumBy (fun row -> row.Demand)

    let mineDemand = mineRows |> List.sumBy (fun row -> row.Demand)

    // ADR-0052
    // The ferry: the bodies a mother lends a bootstrapping child, per child
    // and capped at `Tuning.FerryLoads`, priced from her Storage. A child
    // whose room she cannot reach, or that has no buffer standing, hires
    // nobody.
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
            // Per child and not per store: a room the Layout ever planned
            // two buffers in would otherwise buy two ferries off one
            // declaration.
            |> List.map (fun tile -> tile.Room)
            |> List.distinct
            |> List.length
            |> (*) view.Tuning.FerryLoads

    // A haul that crosses a Seam is never one body (#279): a full container
    // at home waits, a full container in an outpost drops the anchor's next
    // fifty on the floor while the replacement walks forty tiles out. Read
    // off the rows priced above — a container in a room that is not home —
    // and not off the declaration, so a stood-down room asks for nothing.
    // The source containers' rows alone: a mineral container overflows onto
    // a tile inside the colony's own room, where the next hauler takes it.
    let remote =
        rows
        |> List.filter (fun row -> row.Container.Room <> home)
        |> List.sumBy (fun row -> row.Demand)

    // ADR-0049
    // The energy rounded once for the colony; the mine and the ferry are
    // counted in bodies of their own, so they are added after its division.
    let hired = ceilDiv demand capacity

    // The mine's lines ride at the end of the reported rows, so
    // `observe.mjs quotas` prints the new term beside the ones it always had.
    (if remote * 2 >= capacity then max hired 2 else hired)
    + ceilDiv mineDemand capacity
    + ferry,
    rows @ mineRows,
    capacity

/// What one body of this shape drinks a tick standing at a controller.
let private upgradeDrainOf body =
    partCountIn body Work * Engine.upgradeDrainPerWork

/// The guard cut, over parts: an ATTACK part, the one cut no other row of
/// this colony makes. One predicate for a living body and one still in an
/// oven, so the two cannot drift.
let internal isGuardParts (parts: Map<BodyPart, int>) = partCount parts Attack > 0

let internal isGuardBody (creep: CreepInfo) = isGuardParts creep.Body

/// Whether `blocks` whole `guardPattern` blocks win the exchange against the
/// raid standing in one room: two clocks compared, cross-multiplied to stay in
/// whole numbers. Our blocks cannot self-heal while attacking (heal
/// suppresses attack), so their survival uses the raid's full damage. A raid
/// that out-heals our damage is never killed. Healers are priced in the
/// healing and never in the hits. The raid's durability is priced at full off
/// its parts, because the projection carries a hostile's body and not its
/// hits; over-stating what it can take is the safe direction.
///
/// Worked example: a lone smallMelee needs one block; backed by a smallHealer,
/// its 40 damage kills our 1,000 hits in 25 ticks, before our 30 net damage
/// kills its 1,000 hits, so that raid needs the second block.
///
/// Two readers: the guard row asks it of one block to size the crowd, and the
/// stand-down asks it of the cap to decide whether the room is a fight or a
/// withdrawal (`Observe.raidDeadlines`).
let guardBlocksBeat (view: ColonyView) (room: string) (blocks: int) : bool =
    let parts part body = partCountIn body part

    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    let raidDamage =
        raid
        |> List.sumBy (fun h ->
            Engine.attackPower * parts Attack h.Body
            + Engine.rangedAttackPower * parts RangedAttack h.Body)

    let raidHealing = raid |> List.sumBy (fun h -> Engine.healPower * parts Heal h.Body)

    let raidHits =
        raid
        |> List.filter isArmed
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

/// ADR-0056
/// How many guards one raided outpost wants: one, two where one block loses
/// the exchange. The count reads the raid and never our own answer to it
/// (#272): priced against the guards standing in the room it was not monotone,
/// and the reinforcement it bought was evicted on arrival. Vision is the whole
/// of what this reads: an outpost the colony cannot see carries no hostiles.
/// A room remembered and not seen is one guard (#366): a room with no
/// `RoomControl` entry contributes no hostile to the view, so neither number
/// of the two-guard clause exists. Written as its own clause rather than left
/// to `guardBlocksBeat`'s zero-damage arm: that one is a statement about a
/// visible raid dealing nothing.
let internal guardsWanted (view: ColonyView) (room: string) : int =
    if
        Set.contains room view.ThreatenedOutposts
        && not (Map.containsKey room view.RoomControl)
    then
        1
    elif guardBlocksBeat view room 1 then
        1
    else
        Engine.guardCap

/// The guard row's quota: `guardsWanted` over every raided outpost, summed.
let internal guardQuota (view: ColonyView) (outposts: OutpostFacts) : int =
    outposts.Guarded |> List.sumBy (guardsWanted view)

/// ADR-0072
/// The whole guard blocks one raided room's exchange takes to win: the
/// smallest count `guardBlocksBeat` answers yes to, the largest body where
/// none wins. A room the colony is blind in prices at one, since `view.Hostiles`
/// carries nothing of its raid: enough to go and look.
let internal guardBlocksFor (view: ColonyView) (room: string) : int =
    [ 1..guardBlocksMost ]
    |> List.tryFind (guardBlocksBeat view room)
    |> Option.defaultValue guardBlocksMost

/// The blocks the guard row casts this tick: the worst of the guarded rooms'
/// answers, one where nothing is guarded. Every cast carries it, as the
/// reserver row's casts carry the largest outstanding claim: the Matcher pairs
/// a finished body to a room by travel cost.
let internal guardBlocksWanted (view: ColonyView) (outposts: OutpostFacts) : int =
    match outposts.Guarded |> List.map (guardBlocksFor view) with
    | [] -> 1
    | blocks -> List.max blocks

/// Whether the guard row is filled — every body it wants standing or in an
/// oven. Read by the reserver row. Counted over every living guard and not the
/// census less its expiring bodies: a guard inside its lead still stands in
/// the room, so the seat stays open while the row buys the relief.
let internal guardStands (view: ColonyView) (outposts: OutpostFacts) : bool =
    let living = view.Creeps |> List.filter isGuardBody |> List.length

    let inOven =
        view.Casting
        |> List.filter (fun cast -> isGuardParts (partsOf cast.Body))
        |> List.length

    living + inOven >= guardQuota view outposts

/// ADR-0057
/// The miner row's quota: one body per deposit the colony can actually dig —
/// it holds Thorium, the extractor stands, and its mine Post stands (#261).
/// The mod deletes an exhausted deposit outright (`postProcessObject` on
/// `mineralType == 'T' && !mineralAmount`); the amount is read as well because
/// a projection that ever carried a deposit at zero would otherwise hire a body
/// for 1,500 ticks. The Post is read off `Atlas.postsOf`, the same census the
/// Work Area narrows to. Summed over `ourDeposits`: only an owned RCL6 room can
/// hold an extractor, and a rival's is as visible as our own.
let internal minerQuota (view: ColonyView) atlas : int =
    ourDeposits view
    // The first two are `depositIsDiggable`, which the hauler's mine term
    // reads off the same sentence (#262).
    |> List.filter (fun id ->
        depositIsDiggable view atlas id && not (Set.isEmpty (Atlas.postsOf atlas id)))
    |> List.length

/// The reserver row's quota and its sizing, one rule with three faces: one
/// reserver per reservable outpost, each wanting `ceil((5000 - ticks held) /
/// 600)` CLAIM parts; one block per candidate colony; one block per declared
/// errand. The list's length is the quota; every cast this tick is sized at
/// the largest entry, since which controller a finished body holds is the
/// Matcher's. No state between ticks: the deficit recomputes from the
/// reservation itself. The bank must afford one block, or the row hires
/// nobody.
///
/// `reservableOutposts` is `declaredOutposts` less the rooms whose controller
/// somebody else's CLAIM parts hold (#333): the engine refuses
/// `reserveController` on such a controller as flatly as on an owned one, so
/// a body hired for one stands adjacent and is refused every tick of its
/// life. W12S27 bought two of them over the 617 ticks the ticket watched
/// (`reserver-411079`, then `reserver-411698`, the reservation unmoved) at
/// 1,950 energy a head, and an invader core's reservation outlives its core
/// by 4,999 ticks. Asked of the room's **record** where no vision answers for
/// it: the reserver is the only body such a room ever holds, so a read off
/// vision alone would go dark the tick the last one died and hire the next
/// (`Planner.reservableControllers`).
let internal reserverClaimsOf (view: ColonyView) (outposts: OutpostFacts) : int list =
    let heldTicks room =
        view.RoomControl
        |> Map.tryFind room
        |> Option.bind (RoomControlInfo.heldBy ReservationHolder.Ours)
        |> Option.map (fun held -> held.TicksToEnd)
        |> Option.defaultValue 0

    // The candidate colonies, each asking for one block: a claim is one act
    // by one CLAIM part. `patternOf` reads a claimer back as a reserver, so
    // this is one row and not two.
    let claims = outposts.Claims

    // A guarded room's seat waits for its guard (#375): while the guard row
    // has a gap, the rooms it is hired for are not this row's to hire for.
    // The Reserve Task is untouched; what stops is the buying of bodies the
    // raider is eating. Keyed to the guard gap and not to the spending, so it
    // flips once and not every cast.
    let withheld =
        if guardStands view outposts then
            Set.empty
        else
            outposts.Guarded |> Set.ofList

    if view.Bank.Capacity < bodyCost reserverPattern.Block then
        []
    else
        let reserved =
            outposts.ReservableRooms
            |> List.filter (fun room -> not (Set.contains room withheld))
            |> List.map (fun room ->
                ceilDiv (Engine.reservationCap - heldTicks room) Engine.claimLifetime |> max 1)

        reserved
        @ (claims |> List.map (fun _ -> 1))
        // The re-claimer, the row's third face: one resident per declared
        // errand, one block each. `patternOfParts` reads a `[Claim; Move]`
        // back as a reserver whatever it was bought for, so a row of its own
        // would be a census no predicate can tell from this one's; folding
        // it here gets it cast in the row's order and charged in
        // `surplusOverLifetime` beside the reserver's.
        //
        // One entry and never two while a relief is in flight: the relief is
        // cast by the incumbent leaving `living` at its own lead
        // (`Spawns.expiring`), which already prices the successor's walk.
        //
        // The start condition is the bank gate above and the chain, nothing
        // else: `view.Errands` carries only the errands a chain of Seams
        // reaches.
        @ (view.Errands |> List.map (fun _ -> 1))

/// The facts the rows whose sizing is not the bank's answer alone read,
/// derived once for the tick in `decideUnarbitrated`: the casting cascade, the
/// amortization and the lead must agree on what this colony's rows will cast.
/// Neither field may be derived from a creep's remaining life: a lead is
/// priced off this record, so which Posts stand *empty* — an arrival-time
/// judgement — cannot be a field of it without closing a circle.
type RowSizing =
    {
        /// `postWorkCapsOf`'s answer this tick, keyed by the Post's own tile.
        AnchorPostCaps: Map<RoomPos, int>
        /// `reserverClaimsOf`'s answer this tick.
        ReserverClaims: int list
        /// `Tuning.MinerWorkPerMove`, carried through to `BodySizing`.
        MinerWorkPerMove: int
        /// `guardBlocksWanted`'s answer this tick, carried through to
        /// `BodySizing` so the guard row is sized by the fight and not the bank.
        GuardBlocks: int
        /// `minerQuota`'s answer this tick. Here for `ReserverClaims`' reason
        /// (#304): the addend of the target and the multiplier of the charge
        /// must be one number, and the walk of the projection is paid once.
        MinerQuota: int
        /// One while the Reactor delivery's current ground facts stand (#319).
        CourierQuota: int
    }

let internal rowSizingOf (view: ColonyView) atlas (outposts: OutpostFacts) : RowSizing =
    {
        AnchorPostCaps = postWorkCapsOf view atlas
        ReserverClaims = reserverClaimsOf view outposts
        MinerWorkPerMove = view.Tuning.MinerWorkPerMove
        GuardBlocks = guardBlocksWanted view outposts
        MinerQuota = minerQuota view atlas
        CourierQuota = if courierProgrammeOpen view atlas then 1 else 0
    }

/// The colony's surplus over one creep's lifetime: the income the two upgrade
/// rows are hired out of, written once because both read it. Income is counted
/// per posted source at that source's own output; a source the colony cannot
/// price contributes nothing. The reserver, anchor, hauler, miner and courier
/// rows' amortization is deducted here: they are hired off facts about the
/// ground, so their price is settled before the surplus has a number.
let internal surplusOverLifetime (view: ColonyView) atlas (sizing: RowSizing) haulerQuota =
    let capacity = view.Bank.Capacity
    let reserverClaims = sizing.ReserverClaims

    // Every reserver cast this tick carries the largest outstanding demand
    // (the errand's one-block seat included, #318), so the charge is priced
    // off that same body. Scaled from a CLAIM body's 600-tick life onto the
    // 1,500 the rest of this sum is written in.
    let reserverCost =
        if List.isEmpty reserverClaims then
            0
        else
            List.length reserverClaims
            * bodyCost (reserverBodyWithin (List.max reserverClaims) capacity)

    // The miner row charged like the rows beside it (#304): it earns nothing
    // and stands as long as the deposit does. Off `sizing` and never
    // re-derived, so the addend and the charge are one number. Scaled by
    // `Tuning.MineContactAgeing`, a policy assumption rather than a bound:
    // the miner stands on the mineral container, and that is what the mod's
    // contact penalty comes to while the haul keeps the container inside the
    // 100..999 band (its own doc carries the two live ways that is optimistic,
    // #306, #313).
    let minerCost =
        sizing.MinerQuota * bodyCost (minerBodyFor sizing.MinerWorkPerMove capacity)

    let courierCost =
        sizing.CourierQuota
        * ceilDiv
            (bodyCost courierPattern.Block * Engine.creepLifetime)
            view.Tuning.DeliveryInterval

    // The anchor row charged Post by Post, each at the body the casting step
    // would actually buy for that Post.
    let amortization =
        (sizing.AnchorPostCaps
         |> Map.fold (fun total _ cap -> total + bodyCost (anchorBodyFor cap capacity)) 0)
        + haulerQuota * bodyCost (bodyFor haulerPattern capacity)
        + reserverCost * Engine.creepLifetime / Engine.claimLifetime
        + minerCost * view.Tuning.MineContactAgeing
        + courierCost

    // Summed over the posted sources at each one's own output, never a count
    // times a constant.
    let income =
        view.Sources
        |> List.filter (isPosted atlas)
        |> List.sumBy (fun s -> sourceOutputOf view atlas s.Id |> Option.defaultValue 0)

    income * Engine.creepLifetime - amortization

/// Whether a living body is a standing body: `Carry * 4 < Work`. A fact about
/// a body, not a row: the upgrader row's `11W/1C/11M` is one, and so is the
/// anchor row's `6W/1C/1M`. Read by `applicable` on Build, Repair, Refill,
/// Pickup and every Withdraw but the buffer's; on Harvest the body that keeps
/// it is the Work-heavy one and not the standing one.
let internal isStandingBody (tuning: Tuning) (creep: CreepInfo) = standingParts tuning creep.Body

/// What one body of the upgrader row eats per tick, at the row's cast at the
/// richest bank. Never below one — the sizing rule floors at a pair — so the
/// quota always has a divisor.
let private upgraderDrain capacity =
    upgradeDrainOf (bodyFor upgraderPattern capacity)

/// What one body of the row costs the colony over a life: the energy its Work
/// drinks plus the body itself. One expression, because the quota *sells*
/// bodies at this price and `workforceTarget` *charges* the surplus at it.
let private upgraderLifetimeCost capacity =
    upgraderDrain capacity * Engine.creepLifetime
    + bodyCost (bodyFor upgraderPattern capacity)

/// Whether this colony may hire the standing row at all: a **built**
/// controller container to stand at, and a bank whose own cast is a standing
/// body. Named once because both halves of the row must answer to it, and a
/// body hired where the row is illegal reads `NoneApplicable` for its whole
/// life.
let private rowStands (view: ColonyView) atlas =
    not (Set.isEmpty (Atlas.controllerContainers atlas))
    && standingParts view.Tuning (partsOf (bodyFor upgraderPattern view.Bank.Capacity))

/// ADR-0046
/// The upgrader row's two halves: what the income buys — the surplus divided
/// by a body's lifetime cost, rounded **down**, the remainder handed to the
/// worker row whose division rounds up — and what the stock buys on top
/// (#385): sites charged first, `Tuning.UpgradeStockBodies` kept back, and one
/// mouth at a time, because `haulerQuota` is fixed before this term so the
/// extra mouth brings no carrier with it. Derived together because they share
/// every expensive term; split apart they measured +2.4% of a tick on the idle
/// box.
let internal upgraderRow (view: ColonyView) atlas surplus : int * int =
    let capacity = view.Bank.Capacity

    if not (rowStands view atlas) then
        0, 0
    else
        let cost = upgraderLifetimeCost capacity
        let onIncome = surplus / cost |> max 0
        let floor = view.Tuning.UpgradeStockBodies * capacity
        let owed = view.ConstructionSites |> List.sumBy (fun site -> site.Left)

        onIncome, (Facts.stockedEnergy view - owed - floor |> max 0) / cost |> min 1

/// The income's half alone.
let internal upgraderQuota (view: ColonyView) atlas surplus = fst (upgraderRow view atlas surplus)


/// The worker row's floor: two while anything stands in the Build or Repair
/// pool, one otherwise. Two, because a builder crosses a Seam and the home
/// room's own sites are unattended for that walk; one, because a second
/// against no pool is hiring for a job that does not exist.
let private workerFloor (tasks: Task list) =
    let building =
        tasks
        |> List.exists (function
            | Build _
            | Repair _ -> true
            | _ -> false)

    if building then 2 else 1

/// Every number the specialist rows are hired against this tick, in the order
/// they depend on each other (`quotaRowsOf`). A record and not positional
/// `int`s: three readers read the same five, and a mis-ordering was silent in
/// every one of them. `Surplus` rides beside them because `upgraderQuota`
/// reads it too.
type QuotaRows =
    {
        /// One entry per room the reserver row hires for: the length is the
        /// addend, the largest entry prices every cast.
        Reserver: int list
        Guard: int
        Anchor: int
        Hauler: int
        /// One miner per diggable deposit — 0 below RCL6.
        Miner: int
        Courier: int
        Upgrader: int
        /// How many of `Upgrader` the stock bought (#385): `workforceTarget`
        /// must charge the surplus for these mouths not at all, since they ate
        /// no income.
        UpgraderOnStock: int
        Surplus: int
    }

/// The tick's rows, in dependency order, written once so the cascade that
/// casts a body, the amortization that charges for it and the target that
/// counts it read one set of numbers.
let internal quotaRowsOf
    (view: ColonyView)
    atlas
    (outposts: OutpostFacts)
    (sizing: RowSizing)
    haulerQuota
    : QuotaRows =
    let surplus = surplusOverLifetime view atlas sizing haulerQuota

    let onIncome, onStock = upgraderRow view atlas surplus

    {
        Reserver = sizing.ReserverClaims
        Guard = guardQuota view outposts
        // One Anchor per Post of every projected room.
        Anchor = Atlas.postCount atlas
        Hauler = haulerQuota
        Miner = sizing.MinerQuota
        Courier = sizing.CourierQuota
        Upgrader = onIncome + onStock
        UpgraderOnStock = onStock
        Surplus = surplus
    }

/// The workforce target: every specialist row's quota plus the worker row —
/// the home room's unposted Seats, the income arithmetic the upgrade row left,
/// the pioneers a nursery adds and the backlog term — floored at
/// `Tuning.MinWorkforce`. An unposted source of an outpost contributes nothing:
/// the seat-crew justification presumes the walk is cheap. The guard and miner
/// rows are addends so that a body hired off the ground is not read as one of
/// the generalists the income paid for.
let internal workforceTarget (view: ColonyView) atlas (tasks: Task list) (rows: QuotaRows) =
    let home = SpatialInfo.homeName view.Spatial

    let unpostedSeats =
        view.Sources
        |> List.filter (isPosted atlas >> not)
        |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some home)
        |> List.sumBy (fun s -> Atlas.seats atlas s.Id |> Option.defaultValue 0)

    let capacity = view.Bank.Capacity

    let workerDrain = upgradeDrainOf (bodyFor workerPattern capacity)

    // What the standing row takes out of the surplus before the commuting one
    // is hired against the rest: the income-bought mouths only (#385), since a
    // stock-bought mouth ate no income and charging it here would take the
    // surplus away twice.
    let upgraderCost =
        (rows.Upgrader - rows.UpgraderOnStock) * upgraderLifetimeCost capacity

    // ADR-0037
    let incomeWorkers =
        ceilDiv (rows.Surplus - upgraderCost) (workerDrain * Engine.creepLifetime)
        |> max 0

    // ADR-0047
    // The pioneers: while a room this colony has claimed still has no spawn,
    // `Tuning.PioneerCount` more generalists. Hired off a fact about the world
    // and added on top of the row, outside its floor; the addend runs on
    // through the bootstrap window.
    let pioneers =
        let raising room =
            isNurseryRoom view room || isBootstrapRoom view room

        if view.Stages |> Map.exists (fun room _ -> raising room) then
            view.Tuning.PioneerCount
        else
            0

    // The floor sits on the whole generalist share and not on the income
    // term: a colony already running three seat crews has three bodies that
    // can build.
    //
    // The backlog term (#364): generalists hired against what the standing
    // sites still owe, paid out of the stock. W13S28 on 2026-09-17: a terminal
    // site at 3,836/100,000 for thousands of ticks with 535,748 banked, two
    // workers, and 16,464 T stranded behind it; `workerFloor` answers two
    // whether the pool holds a road's 300 or a terminal's 100,000. Sized in
    // labour: one body clears `work × buildPerWork × BuildTicksPerLife` over
    // a life, and the term is what clears the backlog inside one. It decays
    // without a rule of its own: the sites become structures and the bodies
    // are not replaced.
    let backlogWorkers =
        let owed = view.ConstructionSites |> List.sumBy (fun site -> site.Left)

        let body = bodyFor workerPattern capacity

        // `Tuning.BuildTicksPerLife` and not `Engine.creepLifetime`: at
        // nominal a 16-Work body clears 120,000 a life, so W13S28's 96,465
        // answered "one body is enough"; live it built at a tenth to a fifth
        // of nominal, because a generalist spends most of its life carrying
        // its own energy (the measured windows are on that field).
        let clearedPerLife =
            partCountIn body Work * Engine.buildPerWork * view.Tuning.BuildTicksPerLife

        if owed = 0 || clearedPerLife = 0 then
            0
        else
            // Floored where every other division here is a ceiling: a
            // ceiling would round up a whole body for a road's 300, and
            // `workerFloor` already stands two whenever anything is in the
            // Build pool.
            let wanted = owed / clearedPerLife

            // Paid out of the stock with the building charged first: bodies
            // standing beside a site nobody can pay for is the wrong side.
            // `Bank` is not this number: that is the spawn account the hauler
            // row keeps full out of this very stock, so reading it here would
            // count the same energy twice.
            (stockedEnergy view - owed) / bodyCost body |> max 0 |> min wanted

    let workerRow =
        (unpostedSeats + incomeWorkers |> max (workerFloor tasks))
        + pioneers
        + backlogWorkers

    List.length rows.Reserver
    + rows.Guard
    + rows.Anchor
    + rows.Hauler
    + rows.Miner
    + rows.Courier
    + rows.Upgrader
    + workerRow
    |> max view.Tuning.MinWorkforce
