module Fabot.Core.Decide

open Fabot.Core.Types

/// The Workforce target's floor: the colony never plans below this many
/// living creeps. Two keep the harvest/refill loop running while one is in
/// transit or being replaced.
let private minWorkforce = 2

/// A Body pattern: the repeating part block a body is generated from.
/// Which pattern a spawn casts is a colony decision; the pattern shapes
/// what a creep is good at, never what it is assigned (ADR 0006).
type BodyPattern = { Name: string; Block: BodyPart list }

/// The generalist pattern: 200 energy, full speed empty, half speed loaded.
let workerPattern =
    {
        Name = "worker"
        Block = [ Work; Carry; Move ]
    }

/// The Anchor pattern: the heavy-WORK body cast for a Dual Seat (ADR
/// 0006). The block is its minimal cast — two Work keep the body readable
/// as an Anchor (Work > Move, which fatigue parity forbids a worker body)
/// beside the single Carry and the single Move that is the one-time price
/// of walking to the seat.
let anchorPattern =
    {
        Name = "anchor"
        Block = [ Work; Work; Carry; Move ]
    }

/// The hauler unit (ADR 0012): 150 energy, full speed loaded on roads —
/// the row carries its own parity declaration (road parity, not the
/// worker row's plain parity), because a hauler's whole life is the
/// trunk. No Work part, so Harvest, Build, Upgrade and Repair are
/// inapplicable by body; it lives in the Withdraw→Refill cycle.
let haulerPattern =
    {
        Name = "hauler"
        Block = [ Carry; Carry; Move ]
    }

/// The reserver row (ADR 0042): the CLAIM body that walks to an outpost's
/// controller and holds its reservation, which is what makes that room's
/// sources worth ten a tick rather than five. `[2Claim;2Move]` is 1,300
/// energy amortised over a CLAIM part's 600-tick life — 2.17 a tick — so
/// it pays for itself twice over on a single source.
///
/// A row of the table below since its quota arrived with it (ADR 0006's
/// law, #131): one reserver per declared outpost, sized off the reservation
/// deficit (`reserverClaimsOf`), cast in front of every other row
/// (`planSpawns`). It is also the row a *living* CLAIM body is read back
/// to (`patternOf`), which is how ADR 0026 prices its succession.
let reserverPattern =
    {
        Name = "reserver"
        Block = [ BodyPart.Claim; Move ]
    }

/// The upgrader row (ADR 0046): the body that stands beside the upgrade
/// buffer and spends the colony's surplus into the controller. Every part
/// slot past its single Carry goes to a Work/Move pair, so an 1,800 bank
/// buys eleven Work where the worker row's nine units buy nine — the same
/// energy, twenty-two per cent more upgrade, because nothing in the body
/// is paying for a commute it does not make (`upgraderBodyFor`).
///
/// The block is the row's minimal cast and not a unit it repeats — the
/// same shape the anchor row's block is — which is why it reads as the
/// worker row's three parts and sizes into something else entirely: this
/// row keeps the Carry at one and buys Work/Move with the rest, where the
/// generalist buys Carry at fatigue parity. One Carry because a body that
/// stands still needs exactly enough store to hold a Withdraw from the
/// buffer at its feet (ADR 0019 keeps that draw open to it: Work ≤ Move,
/// so ADR 0016's gate is not in its way).
///
/// The row arrives with its own colony fact, as ADR 0006's law asks:
/// `upgraderQuota` is the surplus divided by one such body's drain, and it
/// is non-zero only while a built controller container stands in the room
/// — the buffer is this row's working ground, so a colony with none hires
/// none and the generalist row commutes as it always did (ADR 0046, #187).
/// The row that arrived a ticket ahead of that fact (#186) is level with
/// it again.
///
/// It is the row a *living* **standing body** is read back to
/// (`patternOf`) from the moment it is declared, which is what prices such
/// a body's succession off its own row (ADR 0026) rather than off the
/// generalist's — the row the colony casts beside the buffer, and the row
/// it reads a body of that shape back to whether it cast it, inherited it
/// or was handed one by a Screeps player.
///
/// Read back by the ratio and not by this row's own name, so the row and
/// the read meet only from an 800 bank up: `upgraderBodyFor` buys one Carry
/// against `floor((capacity - 50) / 150)` Work and `isStandingBody` wants
/// four Work to the Carry, so five pairs is where the two lines cross.
/// Under it — `3W/1C/3M` at the RCL2 bank of 550 — this row's own cast is
/// no standing body, keeps all three deliveries and is read back to the
/// generalist. That is the ratio saying what it was written to say (three
/// Work against a fifty-energy load is not yet a commute), not a hole in
/// it, and it is where the quota stops (#187): `upgraderQuota` hires none
/// at a bank whose cast this ratio would read back to the generalist,
/// because the row is counted by the same ratio and a body it cannot count
/// pays off no gap. The band loses nothing the row was for — the same body
/// at the same price is what the generalist row buys out of the same
/// surplus there.
let upgraderPattern =
    {
        Name = "upgrader"
        Block = [ Work; Carry; Move ]
    }

/// The pattern table: every body the colony casts is a row here, sized by
/// energy under the row's own sizing rule. A future pattern is one more
/// data row plus its own quota rule — a colony fact deciding when it is
/// cast — never a new code path (ADR 0006).
///
/// Declaration order and not casting order: the rows cast reserver,
/// Anchor, hauler, upgrader, worker (`planSpawns`), and no rule in this
/// module reads this list for a sequence. What it is is the enumeration —
/// every body the colony casts is here, so a row cast from outside it
/// would be a body no reader of the table could account for.
let patternTable =
    [
        workerPattern
        anchorPattern
        haulerPattern
        reserverPattern
        upgraderPattern
    ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50
        | Attack -> 80
        | RangedAttack -> 150
        | Heal -> 250
        | BodyPart.Claim -> 600
        | Tough -> 10)

// Screeps MAX_CREEP_SIZE: the engine rejects bodies over 50 parts.
let private maxBodyParts = 50

/// Screeps source regen in a room whose controller carries an owner or a
/// reservation: 3,000 energy per 300 ticks — the output per tick a
/// continuously drained source yields there, and what its container's
/// hauler share must ship.
let private heldOutputPerTick = 10

/// The same source in a neutral room: 1,500 per 300 ticks, half the rate.
/// Ten is the *held* rate (ADR 0042), which is the whole reason a source's
/// output stopped being a module constant — sizing miners and haulers at
/// ten against a source yielding five overbuilds both rows twofold.
let private neutralOutputPerTick = 5

/// Screeps HARVEST_POWER: energy one Work part digs from a source a tick.
let private harvestPerWork = 2

/// The Anchor row's Work ceiling (ADR 0021): the Work that saturate one
/// source — dig its whole regeneration in the regeneration time — plus
/// one spare. Past saturation a further Work only drains the source
/// sooner and idles until it regenerates; the spare drains it 50 ticks
/// early, and those ticks absorb an unmanned Post's gap (death, recast,
/// the walk back) at no cost in output.
///
/// A rule about one source's regeneration and never about heavy bodies in
/// general, so ADR 0042 narrows it by changing its input and nothing else:
/// "unchanged as a rule and changed as a number". The number a *cast*
/// reads is `anchorWorkCapOf` below, folded off the projection; this is
/// the arithmetic both readings share.
let private workCapOf output = output / harvestPerWork + 1

/// The ceiling in a room the colony holds: six Work, the number ADR 0021
/// derived and the only one the colony's own room ever asks for. Two
/// readers want the largest ceiling the rule can give rather than the one
/// standing beside them — `bodyFor` below, which holds a capacity and no
/// projection, and `anchorWorkCapOf`'s answer where nothing priceable is
/// posted at all.
let private heldWorkCap = workCapOf heldOutputPerTick

/// The worker row's sizing rule: the largest affordable repetition of the
/// block (never below one repeat), with the remainder spent on Carry/Move
/// at fatigue parity — the padded body is never slower than the pure-block
/// body, empty or loaded, and within that buys as much Carry as possible
/// (ADR 0003, narrowed to the worker pattern by ADR 0006). Parts are
/// grouped Work, Carry, Move so damage strips Work first and mobility last.
///
/// It is the rule every row without one of its own falls through to, and
/// it can only place the three parts it counts: a block holding anything
/// else — a guard's Attack, a healer's Heal — would be *silently* rebuilt
/// out of Work, Carry and Move alone, which is how a `[Claim; Move]` row
/// read through here priced eight Carry and four Move at an 1,800 bank
/// (#155, and the reserver row's own note below). An empty block is the
/// same mistake from the other side, and the two runtimes do not even
/// agree on it: .NET divides by zero, while the emitted JS reads
/// `~~(50 / 0)` as no repeats at all and pads a Carry/Move body out of a
/// row that asked for neither. So a *shape* this rule cannot size is a
/// hard stop rather than a quiet omission — the table's own promise above
/// (ADR 0006) is that a row is one more data row plus its own quota rule
/// and never a new code path, and the honest reading of that promise is
/// that a row this rule cannot size is not a row yet.
///
/// Failing at the first cast says so where the mistake is, and it says it
/// loudly: `Main.loop` calls `decide` under no handler, so the throw takes
/// the whole tick with it — no intents, no observe channels, no CPU row,
/// and the gap in the tick numbers is the only trace. That is the price of
/// a row without a sizing rule, and it is meant to be paid in development
/// rather than in the colony: `patternTableTests` sizes every row of the
/// table, so a row declared without a rule is red in `dotnet test` before
/// it is ever cast.
///
/// A stop and not the other shape ADR 0006 allows — a sizing rule carried
/// on `BodyPattern` itself, which would make the table total and delete
/// this fallback. That is the larger change and it is deferred, not
/// rejected, for the reason `castFromBank` records beside the code that
/// embodies it: the two rows that would need such a member read a fact the
/// pattern cannot see. This stop costs one branch and changes no body the
/// colony casts today.
let private parityBodyFor (pattern: BodyPattern) capacity =
    let block = pattern.Block

    if List.isEmpty block then
        failwith (
            $"body pattern '{pattern.Name}' holds no parts at all, which the generalist sizing rule "
            + "cannot size: it buys whole repeats of a block, and an empty block has no repeat to "
            + "buy. Give this row a block, or its own sizing rule beside anchor/hauler/reserver "
            + "(ADR 0006)."
        )

    let unplaceable =
        block
        |> List.filter (fun part -> part <> Work && part <> Carry && part <> Move)
        |> List.distinct

    if not (List.isEmpty unplaceable) then
        let parts = unplaceable |> List.map string |> String.concat ", "

        failwith (
            $"body pattern '{pattern.Name}' holds {parts}, which the generalist sizing rule cannot "
            + "place: it counts Work, Carry and Move out of a block and emits only those, so the "
            + $"body it would return holds no {parts} at all. Give this row its own sizing rule "
            + "beside anchor/hauler/reserver (ADR 0006)."
        )

    let blockSize = List.length block
    let carryCost = bodyCost [ Carry ]
    let moveCost = bodyCost [ Move ]

    let blockCount part =
        block |> List.filter ((=) part) |> List.length

    let repeats = capacity / bodyCost block |> max 1 |> min (maxBodyParts / blockSize)

    // Loaded parity is work + carry <= 2 * move: a lone Carry is added
    // only under that bound, a Carry+Move pair preserves it, and a lone
    // Move (the trailing 50) only widens it.
    let rec pad work carry move budget slots =
        if slots >= 1 && budget >= carryCost && work + carry + 1 <= 2 * move then
            pad work (carry + 1) move (budget - carryCost) (slots - 1)
        elif slots >= 2 && budget >= carryCost + moveCost then
            pad work (carry + 1) (move + 1) (budget - carryCost - moveCost) (slots - 2)
        elif slots >= 1 && budget >= moveCost then
            pad work carry (move + 1) (budget - moveCost) (slots - 1)
        else
            work, carry, move

    let work, carry, move =
        pad
            (repeats * blockCount Work)
            (repeats * blockCount Carry)
            (repeats * blockCount Move)
            (capacity - repeats * bodyCost block)
            (maxBodyParts - repeats * blockSize)

    List.replicate work Work @ List.replicate carry Carry @ List.replicate move Move

/// The anchor row's sizing rule: one Carry, one Move, and every part slot
/// the remaining energy affords on Work up to the row's ceiling — spawn
/// energy buys output rather than mobility the Post never uses, and
/// stops where the source has no more to give (ADR 0021). Exempt from
/// fatigue parity (ADR 0006); never below the row's two-Work block.
///
/// The ceiling arrives as an argument rather than being read from a
/// constant (ADR 0042): it is a fact about the **set of posted sources**
/// the row hires for, not about the row — which holds a capacity and no
/// projection — and emphatically not about the one source the finished
/// body will dig, which no caster knows (`anchorWorkCapOf` is where the
/// set is folded, and why its richest member wins). The same shape the
/// reserver row's sizing takes for a neighbouring reason
/// (`reserverBodyWithin`, whose second fact is one room's reservation
/// deficit): the casting step supplying a rule its caller has already
/// decided.
let private anchorBodyFor workCap capacity =
    let work =
        (capacity - bodyCost [ Carry; Move ]) / bodyCost [ Work ]
        |> max 2
        |> min workCap

    List.replicate work Work @ [ Carry; Move ]

/// The whole-block rows' shared arithmetic: as many whole blocks as the
/// capacity buys, never below one and never past the engine's 50-part cap,
/// with the parts grouped by kind in the block's own order — so damage
/// strips a row's output before its legs, as every row here wants. Two
/// rows size this way (the hauler's and the reserver's) and their *reasons*
/// differ, which is why each keeps its own name and its own doc below; what
/// they must not each keep is a second copy of the cap, since a body over
/// 50 parts is refused outright by the engine and the spawn silently does
/// nothing that tick.
let private wholeBlockBodyFor (block: BodyPart list) capacity =
    let repeats =
        capacity / bodyCost block |> max 1 |> min (maxBodyParts / List.length block)

    block
    |> List.distinct
    |> List.collect (fun part ->
        let perBlock = block |> List.filter ((=) part) |> List.length
        List.replicate (repeats * perBlock) part)

/// The hauler row's sizing rule (ADR 0012): as many whole [Carry; Carry;
/// Move] blocks as capacity buys (never below one), and nothing else. The
/// row's parity declaration is road parity — two loaded Carry generate
/// two fatigue on a road tile, the one Move pays off two a tick — which
/// the whole block meets and a padded lone Carry would break; the
/// remainder stays banked. Parts are grouped Carry then Move so damage
/// strips capacity first and mobility last.
let private haulerBodyFor capacity =
    wholeBlockBodyFor haulerPattern.Block capacity

/// The reserver row's sizing rule: as many whole [Claim; Move] blocks as
/// capacity buys, never below one. The bank's truncation alone, which is
/// half the row's rule — ADR 0042 sizes the body off the reservation
/// deficit, `ceil((5000 - ticksToEnd) / 600)` CLAIM parts *capped by the
/// bank*, and `reserverBodyWithin` below is where the two halves meet.
/// This entry point is the one `bodyFor` exposes, so a reader holding
/// only a capacity — the lead's succession pricing (ADR 0026) — gets the
/// largest body the row could cast and therefore the longest lead, which
/// is the safe direction to be wrong in: a successor is cast early rather
/// than after its incumbent died.
///
/// The row is sized here rather than through the generalist rule because
/// `parityBodyFor` cannot price it: it counts Work, Carry and Move out of
/// a block and emits only those, so a [Claim; Move] row read through it
/// is refused outright (#155). Until that refusal it sized instead — to a
/// body with no CLAIM part in it at all, eight Carry and four Move at an
/// 1,800 bank — and priced the reserver's succession off a body that could
/// not reserve anything, which is the whole reason the refusal is there.
/// Parts are grouped Claim then Move so damage strips the reservation
/// before the legs, as every other row strips its output first.
let private reserverBodyFor capacity =
    wholeBlockBodyFor reserverPattern.Block capacity

/// The upgrader row's sizing rule (ADR 0046): one Carry, and every part
/// slot the rest of the capacity affords spent on Work/Move **pairs** —
/// `W = M = floor((capacity - 50) / 150)`, never below one pair. At the
/// live RCL5 bank of 1,800 that is `11W/1C/11M` for 1,700, against the
/// worker row's nine Work at the same bank: the whole gain is the eighteen
/// parts the generalist spends on carrying energy to work it is not going
/// to do standing still.
///
/// Why the Move parts at all, for a body that stands: ADR 0016's gate is
/// `Work > Move`, and a body over that line may not Withdraw (ADR 0016) —
/// which is the buffer this row exists to drink from (ADR 0019). Pairing
/// each Work with a Move keeps the row at `Work = Move`, inside the gate,
/// and buys the walk out of the spawn as a side effect; the alternative
/// (a heavier body with fewer Move) is ADR 0046's option ③ and is
/// deferred there, not taken here.
///
/// Exempt from fatigue parity like every row with a rule of its own (ADR
/// 0006) — parity is the *generalist* row's invariant and this body is at
/// it by construction anyway. Parts are grouped Work, Carry, Move, the
/// order `anchorBodyFor` above sets: damage strips the output first and
/// the legs last.
///
/// Capped at the engine's 50 parts like every other row, and the overshoot
/// at a rich bank is large: a pair is 150 energy, so an RCL8 bank would ask
/// for eighty-five of them where the body may hold twenty-four beside its
/// Carry. Late to bind rather than early, for the same reason — a pair is
/// dearer per part than a hauler's block or a worker unit, so the cap
/// starts biting at a 3,800 bank here against 2,550 for the hauler row and
/// 3,400 for the generalist. A body over the cap is refused outright by the
/// engine and the spawn silently does nothing that tick, which is the one
/// failure mode no row may have.
///
/// The floor under it is the same one `wholeBlockBodyFor` and
/// `parityBodyFor` carry: a bank too poor for one pair still casts one
/// (ADR 0046's "never below one pair"), so the rule answers a body at every
/// capacity rather than an empty one. No live bank reaches it — an RCL1
/// spawn holds 300 — and it is pinned below 200 all the same, where the
/// clamp is the thing under test.
let private upgraderBodyFor capacity =
    let pairs =
        (capacity - bodyCost [ Carry ]) / bodyCost [ Work; Move ]
        |> max 1
        |> min ((maxBodyParts - 1) / 2)

    List.replicate pairs Work @ [ Carry ] @ List.replicate pairs Move

/// Body for a pattern at an energy capacity, under the row's own sizing
/// rule (ADR 0006): the anchor row spends on Work beside its fixed
/// Carry/Move pair, the hauler row buys whole blocks at its own road
/// parity, the reserver row buys whole blocks of the one part that holds a
/// reservation, the upgrader row buys Work/Move pairs beside one Carry,
/// and every other block-replicating row pads its remainder
/// at plain fatigue parity — or, if its block holds a part that last rule
/// cannot place, is refused rather than sized into some other body (#155).
///
/// A capacity is the whole of what this entry point holds, so the two rows
/// whose real rule reads a second fact — the anchor's source output (ADR
/// 0042) and the reserver's reservation deficit — are answered here at
/// their **largest** body: the held ceiling and the bank's own block
/// count. What holds that reading is this signature and not a preference
/// for long leads: the remaining caller for either row is `leadOf`, which
/// prices a succession here (ADR 0026), and a lead has no row-specific
/// rule to read a second fact through (ADR 0026 under ADR 0006) while a
/// capacity is all this one carries. It is *not* what ADR 0026 asks for —
/// it defines the lead as **the replacement body's** cast time and travel
/// and names the over-long one as a defect: "`IdleReason.NoneFree` on a
/// freshly cast Anchor should no longer appear during a succession; if it
/// does, the lead is mispriced". So both rows' leads run long exactly
/// while their second fact sits below its largest — the anchor's while no
/// posted source is held, the reserver's while no reservation has
/// slipped — and the reserver's has since #131. Making either exact is
/// one decision over both rows and this signature rather than a branch
/// here, and it is deferred, not decided here. The casting step reads the
/// narrower rule — `anchorBodyFor` under `anchorWorkCapOf`,
/// `reserverBodyWithin` under the deficit — so a body bought from either
/// of *those two* rows is never sized from here; the hauler, upgrader and
/// worker rows are sized and cast from this entry point (`planSpawns`),
/// each of them the bank's answer alone.
let bodyFor pattern capacity =
    if pattern.Name = anchorPattern.Name then
        anchorBodyFor heldWorkCap capacity
    elif pattern.Name = haulerPattern.Name then
        haulerBodyFor capacity
    elif pattern.Name = reserverPattern.Name then
        reserverBodyFor capacity
    elif pattern.Name = upgraderPattern.Name then
        upgraderBodyFor capacity
    else
        parityBodyFor pattern capacity

/// The generalist body: the worker row of the pattern table, sized to
/// capacity.
let workerBodyFor capacity = bodyFor workerPattern capacity

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw storeId -> $"withdraw:{storeId}"
    | Refill structureId -> $"refill:{structureId}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"
    | Reserve controllerId -> $"reserve:{controllerId}"
    | Claim controllerId -> $"claim:{controllerId}"
    | Pickup pileId -> $"pickup:{pileId}"
    // One Flee for the whole colony: it has no target to be identified by,
    // and every creep inside a Reach is running from the same thing.
    | Flee -> "flee"

/// The [[stage]] of the colony whose home is the named room, off the one
/// derivation the shell ran for the tick (ADR 0052 decision 3,
/// `Colony.stageOf`). `None` for a room no colony of ours lives in —
/// undeclared, unclaimed, or one nothing could see — which is the answer
/// every reader here already gives such a room.
///
/// What it is not is a claim on the room. The map is the world's and
/// reaches every colony alike, so a rule about a room that is not this
/// colony's home says so beside the stage, with the projection fact that
/// makes it this colony's business (`colonyOwns`, `isNurseryRoom`).
///
/// Named for the lookup and not for the derivation: `Colony.stageOf` is
/// the rule that *decides* a stage, off three facts of the world, and the
/// doc comments below cite it by name a few lines from every call of
/// this one.
let private roomStage (view: ColonyView) room = Map.tryFind room view.Stages

/// This colony's own stage: its home room's entry. Always present for a
/// living colony — `Main.loop` runs `decide` only for a home that is ours
/// and holds a spawn of ours (ADR 0047 decision 1), which is a stage by
/// construction — so `None` is the projection that cannot place its own
/// controller, and every reader gives that colony the answer it gives one
/// standing under the line.
let private homeStage (view: ColonyView) =
    roomStage view (SpatialInfo.homeName view.Spatial)

/// Whether this colony has outgrown its bootstrap window: `Independent`,
/// at `Colony.bootstrapLevel` or past it (ADR 0052 decision 3). The one
/// question the Layout's two gates and the Repair pool's rampart line ask
/// — a colony below it is still buying the economy those spends serve.
let private isIndependent (view: ColonyView) = homeStage view = Some Independent

/// The Repair trigger of the decaying kinds: a road or a container enters
/// the pool when its hits sink strictly below this fraction of max, and
/// leaves it once repaired back over the line. A tunable, not part of
/// ADR 0010.
let private repairTrigger = 0.5

/// The rampart floor (ADR 0034): a rampart is hungry below this many hits
/// and whole at it. A tunable, and a derived one — the ticks the room must
/// hold times the damage per tick it must hold against. Against the squad
/// of #66, 180 hits a tick, 100,000 hits is 555 ticks, two and a half
/// times the raid that was seen. That costs 1,000 energy to raise and, at
/// 300 hits of decay per 100 ticks, one Repair visit per rampart every 200
/// ticks to hold — so no hysteresis is needed: one visit at 600 hits a
/// tick puts a rampart that just dipped back over the line.
let private rampartFloor = 100_000

/// Whether this colony keeps ramparts this tick (ADR 0034 as #214 amends
/// it): it is `Independent`, past the bootstrap line and one stage past
/// the engine's own unlock (Screeps CONTROLLER_STRUCTURES for "rampart":
/// none at RCL1, 2,500 from RCL2 up). The covering rule's one gate and
/// the floor's one gate, one spelling for both: a colony below it places
/// no rampart and counts none of its standing ramparts hungry, so the
/// three a child raised at RCL2 decay away instead of holding four
/// workers to a 100,000-hit floor derived for a home with a tower and a
/// Storage behind it (W13S28, t~170,8xx: four of five loaded workers on
/// Repair while the extension sites — the 550 bank — sat at a few hundred
/// progress). What defends a bootstrapping room's one-spawn Keep is the
/// safe-mode reflex, whose Keep arm reads the spawn's own hits and never
/// a rampart's, so that arm is not gated here. A stage rather than an
/// allowance because the count is never what constrains the cover — the
/// Keep and the Posts are a handful of tiles against thousands — and the
/// [[stage]] rather than a level of its own for the reason the road gate
/// reads it (#209, ADR 0052 decision 3): one derivation, five readers.
///
/// A colony with no stage keeps none, which is the same answer the
/// covering rule gives a room it cannot orient itself in.
let private keepsRamparts (view: ColonyView) = isIndependent view

/// Whether a structure of this kind, carrying these hits, is hungry: its
/// own whole line, read off the kind (ADR 0034). The decaying kinds sit
/// below a fraction of max (ADR 0010), a rampart below the floor, the Keep
/// below full — it does not decay, so below max means damaged. A kind with
/// no line is never hungry. The floor is capped at the structure's own max
/// so a rampart whose max is somehow under it can still be whole; today's
/// engine puts a rampart's max at 300,000 from RCL2, well clear.
let private isHungry kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction -> float hits.Hits < repairTrigger * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min rampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// Every structure the projection carries hits for that stands below its
/// kind's whole line, with its kind, in id order. The one walk over the
/// hits and the kinds that judges them, and the two readers that ask it
/// share it: the Repair pool takes all of them, the safe-mode reflex's
/// Keep arm asks only whether one of them is of the Keep (ADR 0034). The
/// projection carries hits on repairable kinds only, but the kind gate is
/// judged here — the decision layer owns what it reads, off the same table
/// the projection filtered by.
let private hungryStructures (view: ColonyView) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view

    view.Spatial.Hits
    |> Map.toList
    |> List.choose (fun (id, hits) ->
        match Map.tryFind id view.Spatial.TargetKinds with
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | Some(Structure BuiltKind.Rampart) when not ramparts -> None
        | Some(Structure kind) when isHungry kind hits -> Some(id, kind)
        | _ -> None)

/// Screeps CONTAINER_CAPACITY: what a container's store can hold — the
/// line past which the buffer needs no Refill.
let private containerCapacity = 2000

/// Screeps STORAGE_CAPACITY: what the Storage's store can hold — the line
/// past which the stock needs no Refill. Read against stored *energy*, as
/// the container line is, because energy is the only resource this colony
/// ever holds; the day it holds another, the Storage's free capacity has
/// to be projected rather than inferred from one resource.
let private storageCapacity = 1000000

/// The pile a Pickup is worth walking for (#167): a dropped pile enters
/// the pool at this many energy and never below it. A tunable beside
/// `repairTrigger`, not part of any ADR.
///
/// A hundred, for what it costs against what it saves: the [[pickup
/// reflex]] already takes every pile a creep happens to stand beside, so
/// what this number prices is a walk made for the pile alone, and two
/// CARRY parts' worth is the smallest load that pays for one. Under it the
/// pile is left to decay at a thousandth a tick, or to the next creep that
/// passes it. Not ADR 0013's rule wearing a number: that one pools a
/// stocked source and says "no threshold cleverness" in as many words,
/// because a drained rock cannot be worked at all. This is cleverness, and
/// it is a tunable and not a decision for exactly that reason.
///
/// **A pile that falls back under the line loses its holder mid-walk**,
/// and that is accepted rather than overlooked. The number is judged every
/// tick against a pool rebuilt from scratch, so it is the persistence
/// condition as well as the entry one: a pile decaying at one a tick
/// crosses back under within `amount - 99` ticks, and the first of two
/// hired haulers to arrive can take it under the line by itself. Either
/// way the holders still walking are released through the ordinary
/// task-gone path with the walk spent for nothing. The alternative is an
/// entry condition — pool a pile some creep already holds while anything
/// is left in it — and that costs the thing the pool is built on:
/// `planTasks` is creep-blind, which is ADR 0013's own reason for pooling
/// a rock whatever is standing at it. A walk is cheap and a rule that
/// reads its own assignments back is not, so the walk is what is spent.
let private pickupThreshold = 100

/// The Reach margin (ADR 0033): the tiles a Threat's weapon range is
/// widened by — one for the hostile's next step, one for our own tick of
/// lag. A tunable beside `repairTrigger`, not a term of the decision.
let private reachMargin = 2

/// Screeps weapon ranges: ATTACK strikes at range 1, RANGED_ATTACK at 3.
let private meleeRange = 1
let private rangedRange = 3

/// The range a hostile can hurt a creep from, or None for one that cannot
/// (ADR 0033). A Threat is read off the parts and never off the owner — an
/// NPC invader and a player's raider do the same damage per part — and
/// nothing but ATTACK and RANGED_ATTACK hurts a creep at all: a healer, a
/// scout or a claimer is a hostile the fire reflex shoots and the Raid log
/// records, and it gates no Task. A body carrying both weapons reaches the
/// farther of them.
let private weaponRange (hostile: HostileInfo) : int option =
    [
        if List.contains Attack hostile.Body then
            meleeRange
        if List.contains RangedAttack hostile.Body then
            rangedRange
    ]
    |> function
        | [] -> None
        | ranges -> Some(List.max ranges)

/// The tick's colony-level threat facts (ADR 0033): the tiles a Threat can
/// hurt, and the walkable tiles no Threat can. Derived once a tick and
/// shared by the three readers — the applicability gate that takes the
/// Reach out of every Work Area, Flee's own Work Area, and the spawn hold.
/// Colony facts, never a change to the spatial projection: hostiles still
/// block no tiles and price no paths, and nothing in the Atlas reads one.
///
/// Layered by room name, as the projection they are derived from is (ADR
/// 0041, #138): a Reach is a set of one room's tiles, and a `Set<Pos>`
/// cannot say which room's, so the room rides on the outer key — the
/// room the hostile stands in, which `HostileInfo` carries for exactly
/// this join. Without the layer a hostile in one room dug the same hole
/// on the same coordinate of every projected room. Each reader picks its
/// own room's share through `Threats.reachIn` and `Threats.safeIn`, and
/// a room with no entry answers the empty set, which is ADR 0004's
/// absence: it blocks no action and pools no Flee. So the single-room
/// colony's answers are unchanged, and an outpost this tick projects no
/// hostile in is quiet by absence rather than by a second rule. Since
/// #201 that absence is the honest one: the sweep behind `Hostiles`
/// covers every room the colony can see, so a Reach missing from a room
/// means nothing is standing in it rather than that nobody looked.
type Threats =
    {
        /// Per room, the tiles a Threat standing in it can hurt. Never an
        /// empty set under a room: a room whose whole Reach our ramparts
        /// took back has no entry, so `Map.isEmpty` is "no Reach
        /// anywhere" — the one question the pool asks of it.
        Reach: Map<string, Set<Pos>>
        /// Per room, the walkable tiles no Threat reaches — Flee's Work
        /// Area for a creep standing there. Derived only for the rooms
        /// with a Reach, where every other room's absence stands for "not
        /// derived" rather than "nowhere is safe": nothing reads it there,
        /// because a creep with no Reach around it is matched to no Flee.
        ///
        /// Joined to its room here and not at the ask, unlike `Reach`
        /// beside it (#216 R3): this one *leaves* as an area — it is
        /// handed to the price, the first step and the applicability gate
        /// as a Work Area is, so it takes the shape those take. The join
        /// costs one pass over a room the derivation already walks, where
        /// at the ask it is a `Set.map` over a thousand-odd tiles per
        /// creep per candidate — the same reason `Atlas.WorkAreas` holds
        /// both shapes from one write. `Reach` stays a `Set<Pos>` because
        /// it is only ever membership-tested against a grid coordinate.
        Safe: Map<string, Set<RoomPos>>
    }

/// The tick with nothing to run from: every Work Area stands whole and no
/// creep flees. What the pipeline is handed for a quiet colony.
let noThreats = { Reach = Map.empty; Safe = Map.empty }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// One room's Reach; empty for a room no Threat stands in (ADR 0004).
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// One room's safe set, already joined to that room; empty for a room
    /// no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Safe |> Option.defaultValue Set.empty

/// This tick's Threats, off the view's hostiles and the rampart
/// census, room by room. Each Threat reaches its weapon range plus the
/// margin, in the Chebyshev tiles every range in the colony is measured
/// in — less every tile under one of our standing ramparts in that same
/// room, which is in no Reach at all: a creep on its own rampart cannot be
/// attacked, and that exemption is what lets an Anchor keep digging on a
/// ramparted Post (ADR 0034). A room's safe set is that room's walkable
/// ground less that room's Reach, derived only where something is unsafe,
/// so a quiet tick pays for no walk over any room, and a raid in one room
/// walks that room alone. Derived once here and handed down; the layering
/// does not make it once per creep, and neither does the room join — the
/// safe set is built as an area of its room in this one pass (`Threats`).
let threatsOf (view: ColonyView) atlas : Threats =
    // Under safe mode a hostile in a room of ours can hurt nothing — the
    // engine refuses every harmful act there for the whole window — so it
    // is no Threat and has no Reach, and our creeps stand and work beside
    // it (user, 2026-09-06: "safe mode enable 的时候不要 flee"). Read per
    // room off `RoomControl` and not off this colony's own controller
    // (#218): safe mode shields the room it is in whoever is looking, so
    // a mother's pioneer in a child's room under safe mode stands as the
    // child's own creeps do. A room we own, only — a rival's safe mode
    // protects the rival, and a raider in an outpost is as dangerous as
    // ever.
    let shielded room =
        match Map.tryFind room view.RoomControl with
        | Some control -> control.Owner = Ownership.Ours && control.SafeMode
        | None -> false

    // The hostiles are asked first, so a quiet colony walks nothing:
    // neither the rampart census nor any room's own tiles are read on a
    // tick with nothing in it to run from.
    match
        view.Hostiles
        |> List.filter (fun hostile -> not (shielded hostile.Pos.Room))
        |> List.choose (fun hostile ->
            weaponRange hostile
            |> Option.map (fun r -> hostile.Pos.Room, RoomPos.pos hostile.Pos, r))
    with
    | [] -> noThreats
    | threats ->
        let reach =
            threats
            |> List.groupBy (fun (room, _, _) -> room)
            |> List.choose (fun (room, inRoom) ->
                let ramparts = Atlas.ourRampartTilesIn atlas room

                let tiles =
                    inRoom
                    |> List.collect (fun (_, pos, weapon) ->
                        let r = weapon + reachMargin

                        [
                            for x in pos.X - r .. pos.X + r do
                                for y in pos.Y - r .. pos.Y + r do
                                    let tile = { X = x; Y = y }

                                    if not (Set.contains tile ramparts) then
                                        tile
                        ])
                    |> Set.ofList

                // Nothing left to run from once our own ramparts have taken
                // the whole Reach back: no Reach, no entry, no safe set to
                // derive.
                if Set.isEmpty tiles then None else Some(room, tiles))
            |> Map.ofList

        {
            Reach = reach
            Safe =
                reach
                |> Map.map (fun room tiles ->
                    Set.difference (Atlas.walkableTilesIn atlas room) tiles |> RoomPos.setAt room)
        }

/// The source container geometry (ADR 0012): a tile within range 1 of the
/// given source is that source's container tile — the Seat-standing kind
/// the Layout places, which harvest overflow fills. The one range this
/// colony calls a source container, asked of one source: the Layout asks
/// it per target to know whether that source is served (ADR 0040), and
/// the two rules below ask it of every source of one room at once.
let private servesSource (sourcePos: Pos) (tile: Pos) = range tile sourcePos <= 1

/// The source a tile of the named room is a container's for: the placed
/// source **standing in that same room** within range 1 of it, or None
/// where there is none. The one geometry judgement behind both rules that
/// care about a source container — the Planner keeps them out of Refill,
/// the hauler quota counts them. Unplaced geometry classifies nothing
/// (ADR 0004).
///
/// The source's identity and not merely its existence, because since ADR
/// 0042 the hauler quota prices a container at *that* source's own output:
/// a room's reservation decides whether the rock under a container is
/// worth ten a tick or five, and a tile alone cannot say which rock it is.
/// Of several sources within range 1 — geometry this colony has none of,
/// two rocks would have to stand two tiles apart — the first in view
/// order answers, deterministically.
///
/// The room is matched before the range, and it has to be (ADR 0041): a
/// `Pos` carries no room, so a fold over every source compares a home
/// container's coordinates against an outpost source's and answers yes on
/// a collision that is fifty tiles and a room boundary away. That one
/// wrong answer costs twice over — the container enters the hauler quota
/// as a source's, and drops out of the Refill pool as one — so the two
/// rules below hand in the room the tile came out of rather than the tile
/// alone.
let private sourceContainerServes (view: ColonyView) (room: string) (pos: Pos) : string option =
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

/// Whether this colony owns the named room — one spelling for the two
/// rules of the reserver's that read it (#181). `reserveController`
/// answers ERR_INVALID_TARGET on a controller with an owner, so a room we
/// own has no reservation to offer: the Reserve pool must not carry its
/// controller and the reserver row must not hire against it, and those are
/// one sentence rather than two gates free to disagree. A room with no
/// control entry is one the colony cannot see this tick, and an unseen
/// room is not one it owns — absence classifies nothing (ADR 0004).
let private colonyOwns (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// The controllers a Claim is pooled for this tick, each with the room it
/// stands in (ADR 0047) — one spelling for the two rules that read it, the
/// way `colonyOwns` above is: the Task pool below offers exactly these,
/// and the reserver row hires exactly one body for each
/// (`reserverClaimsOf`), so the row and its Task cannot disagree about
/// which rooms are being taken (ADR 0006).
///
/// A **candidate colony** is a declared home this colony does not own yet
/// (`ColonyView.Declared`), and both halves are needed: the declaration,
/// because claiming a room is a human's decision and no projected fact
/// distinguishes a room we mean to own from a neighbour we merely mine;
/// and the ownership, because the tick the claim lands the room stops
/// being a candidate and this pool empties itself, with no state kept and
/// nothing to reset (#181 spelled the same rule for Reserve). The
/// ownership half is `takeable`'s own `Unowned` and not a second gate
/// beside it: `Ownership` has three cases, so "unowned" is the stronger
/// read of the same field `colonyOwns` spells for the Reserve side, and a
/// second conjunct here would be two gates free to disagree about one fact
/// (the sentence `colonyOwns` itself is written under).
///
/// **The controller has to be in the projection already**, and it is there
/// because the candidate room is *also* one of the mother colony's
/// outposts until it stands on its own (ADR 0047): that is what puts its
/// terrain in the scan set, its controller in the kind census under the
/// engine's own id, and its tile in `Obstacles` for the claimer to stand
/// beside. A declared home nobody projects offers no controller here and
/// hires nobody — the same silence ADR 0004 gives every other unplaceable
/// target, and the reason a human declares the candidate as an outpost of
/// the mother colony first.
///
/// **`RoomControl` has to say the room is takeable**: unowned, and
/// reserved by nobody but us. The engine answers ERR_INVALID_TARGET on a
/// controller somebody else owns or reserves, so a Task pooled for one is
/// a Task no body can execute — a claimer would walk fifty tiles and stand
/// there for the rest of its 600-tick life. A rival holding the room is
/// ADR 0043's business and not this pool's: the [[stand-down]] takes that
/// room out of the projection entirely, and until it does the room is the
/// [[reserve]] it always was. A room with no control entry is one the
/// colony cannot see this tick, and an unseen room is not one it can
/// claim — absence classifies nothing (ADR 0004), and the vision the claim
/// waits on is bought by the outpost crews already working the room.
let private claimTargets (view: ColonyView) : (string * string) list =
    let takeable room =
        match Map.tryFind room view.RoomControl with
        | Some control ->
            control.Owner = Ownership.Unowned
            && control.Reservation
               |> Option.forall (fun held -> held.Holder = ReservationHolder.Ours)
        | None -> false

    let candidate room =
        List.contains room view.Declared && takeable room

    view.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if kind = Controller then
            match SpatialInfo.placementOf view.Spatial id with
            | Some tile when candidate tile.Room -> Some(id, tile.Room)
            | _ -> None
        else
            None)

/// Whether the named room is this colony's **nursery**: a declared colony
/// of ours that has been claimed and has no spawn of its own yet, and so
/// is not independent (ADR 0047 decision 4). Its home goes on being
/// projected as this colony's [[outpost]], and three rules read that
/// state — every site in it is feeding-tier work (`isNurserySite`, which
/// `isFeedingSite` carries to the tier and to the body gate together), the
/// concurrent-builder budget does not reach those sites
/// (`taskCapacities`), and the worker row hires `pioneerCount` more bodies
/// (`workforceTarget`) — so it is one spelling and not three gates free to
/// disagree, the sentence `colonyOwns` and `claimTargets` above are both
/// written under. The budget is the one of the three that does not read it
/// through `isFeedingSite`, because what it asks is narrower than the tier
/// (`taskCapacities`), so a change to what a nursery *is* has to be
/// followed to all three from here.
///
/// Two facts, each doing its own work. The room's **stage** is `Nursery`
/// (ADR 0052 decision 3), which is the whole of what a nursery is —
/// declared, owned by us, and no spawn of ours standing in it — derived
/// once for the tick off the world (`Colony.stageOf`) where the three
/// were read one at a time here. Declared, because a room the colony
/// merely mines is nobody's child and its sites are the surplus work
/// every other room's are: the same human sentence `claimTargets` reads,
/// one tick later in the same story. Owned, which is exactly what
/// `claimTargets` stops answering to the tick the claim lands — the pool
/// empties itself and this rule takes over, with no state kept and no
/// constant to reset. And no spawn, because a spawn is what independence
/// *is*: the nursery ends the tick one stands, and the human's edit
/// splitting the declaration in two follows that tick rather than causing
/// it.
///
/// The declaration is carried by the stage and no longer by a
/// `Declared` conjunct here, and the one room that can hold a stage
/// without one is harmless by the exclusion below: the shell derives
/// stages for the declared homes and for every **living** colony's home
/// (`World.stages`), and the only living home no declaration
/// names is `Colony.living`'s fallback, which fires solely when nothing
/// declared is living — so that room is the colony doing the reading, and
/// `room <> home` answers it first.
///
/// Beside it, **this colony projects the room** (`colonyOwns`): the stage
/// says what the room is and the projection says whose business it is.
/// The map is the world's and reaches every colony alike, so the second
/// conjunct is what keeps a second mother — or a colony that merely
/// declares the same room — from hiring [[pioneer]]s for a child it never
/// projects; and it is a control entry, so vision pays for it (ADR 0004)
/// and a declared home nobody projects is no nursery here, the same
/// silence `claimTargets` gives one.
///
/// The colony's own home is excluded by name and not by luck. `Main.loop`
/// runs `decide` only for a **living** colony, one whose home holds a spawn
/// of ours (ADR 0047 decision 1), so a home with no spawn is not a tick
/// this rule is ever asked about; and leaving it in would put a condition
/// on #157's "home Build is untouched and stays Surplus", which is a
/// sentence rather than a sentence with an exception.
let private isNurseryRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && roomStage view room = Some Nursery

/// Whether the named room is a child colony this one is still
/// **bootstrapping** (ADR 0047 decision 4): a declared colony of ours that
/// stands its own spawn — so it is running its own `decide` and is nobody's
/// [[nursery]] any more — and that this colony is nonetheless projecting.
/// Two rules read it: the child's controller joins this colony's Upgrade
/// pool (`planTasks`), and the worker row keeps hiring `pioneerCount`
/// bodies (`workforceTarget`). Its Build needs no rule of its own — a site
/// in a room the colony projects is already pooled by id (#150) — which is
/// why the borrowing rule is two Tasks and one predicate.
///
/// `isNurseryRoom`'s two facts with the stage inverted: this colony
/// projects the room, and the colony living there has a spawn of its own
/// standing — which is every stage but `Nursery` (ADR 0052 decision 3).
/// The two predicates are complements over one room and one tick apart —
/// the nursery ends the tick a spawn stands and the bootstrap window
/// opens on it — so they are written as one shape and cannot both answer
/// for a room.
///
/// **Both standing stages, and that is the whole reason it is written as
/// two.** `Bootstrapping` is the child under `bootstrapLevel`;
/// `Independent` is the same child past it, and this predicate goes on
/// answering true for it, because what closes the borrowing is the scan
/// set and never a level read here (ADR 0047's Consequences, #192). The
/// tick a child that has **left** its mother's outpost list reaches RCL3
/// it stops being bootstrapped (`Colony.bootstrapping`) and the whole
/// room leaves this colony's projection: the `RoomControl` entry
/// `colonyOwns` reads goes with it, and the Upgrade, the Build and the
/// addend fall away from one subtraction. But while a human still
/// declares the child's room as one of this colony's `Outposts`, the room
/// is in the scan set through the *outpost* reading, which asks no stage
/// — so the borrowing and the addend run at any RCL until the commit
/// takes that entry out. That is ADR 0047's already-named window between
/// the spawn standing and the human's edit, where the mother is *also*
/// still mining the room, planning it and hauling its energy home, and
/// what bounds it is the same deploy those cost. A stage read as
/// `Bootstrapping` alone would close it instead, and drop the mother's
/// fleet by three on a tick no human touched, against the flat addend ADR
/// 0047 chose.
let private isBootstrapRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && (match roomStage view room with
        | Some Bootstrapping
        | Some Independent -> true
        | Some Nursery
        | None -> false)

/// Whether an Upgrade in this pool is **borrowed**: its controller is not
/// this colony's own (`ColonyView.Controller`), so it is a bootstrapped
/// child's, pooled by `planTasks` for the pioneers (ADR 0047 decision 4
/// as #213 amends it). Three readers ask it and must agree on which
/// Upgrade they mean: the tier that lifts it, the capacity that bounds
/// the lift at `pioneerCount`, and the body gate that keeps a standing
/// body off it.
let private isBorrowedUpgrade (view: ColonyView) controllerId =
    view.Controller |> Option.exists (fun c -> c.Id = controllerId) |> not

/// Planner: rebuild this tick's full Task pool from the colony view. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (view: ColonyView) (threats: Threats) : Task list =
    // Flee exists while a Reach does (ADR 0033): one Task for the whole
    // colony, at the head of the pool as its Safety tier is at the head of
    // the ranking. No Reach, no Flee — a quiet tick's pool is the pool it
    // always was.
    let flees = if Map.isEmpty threats.Reach then [] else [ Flee ]

    // Harvest exists for every source, drained or not (ADR 0013, revised
    // by ADR 0025): the task no longer flickers with the source's stock,
    // because whether a dry rock is worth walking to depends on the
    // walker's body and position — the Matcher's knowledge, not the
    // creep-blind Planner's.
    let harvests = view.Sources |> List.map (fun s -> Harvest s.Id)

    let refills =
        view.Refillables
        |> List.filter (fun r -> r.FreeCapacity > 0)
        |> List.map (fun r -> Refill r.Id)

    let builds = view.ConstructionSites |> List.map (fun site -> Build site.Id)

    // A Repair per repairable structure below its kind's whole line, in id
    // order (ADR 0010, ADR 0034).
    let repairs = hungryStructures view |> List.map (fst >> Repair)

    // The ids of one projected kind, in id order. The containers, the
    // Storage and the controllers are all pooled by the projection's kind
    // — never by position, never by name — so the rule is written once.
    let idsOfKind kind =
        view.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, k) -> if k = kind then Some id else None)

    // The colony's own controller, and the controller of every child it is
    // still bootstrapping (ADR 0047 decision 4, `isBootstrapRoom`) — the
    // one cross-colony borrowing rule there is, and half of it: a loaded
    // worker of the mother's may cross the Seam and spend into the child's
    // controller until that controller reaches `bootstrapLevel`.
    //
    // Surplus tier, like the home Upgrade it stands beside (`tierOf`), so
    // the mother's own flow is fed first and nothing but travel cost
    // separates the two Upgrades — which is what leaves the child's to the
    // bodies already standing in its room, the [[pioneer]]s, and the home
    // one to everybody else. The child pools the very same Upgrade in its
    // own tick, off its own `ColonyView.Controller`, and the two pools are
    // two colonies' business over one target: each Matcher counts only its
    // own holders, exactly as the [[nursery]] window's two pools do.
    //
    // Read off the projection's kind census in the rooms the predicate
    // names, never off `ColonyView.Controller`, which is this colony's own
    // and nothing else (ADR 0047 decision 1) — the child's controller is
    // a target in a layer she projects, like every other fact she has
    // about that room.
    let upgrades =
        let own = view.Controller |> Option.toList |> List.map (fun c -> c.Id)

        let children =
            idsOfKind Controller
            |> List.filter (fun id ->
                SpatialInfo.placementOf view.Spatial id
                |> Option.map (fun tile -> tile.Room)
                |> Option.exists (isBootstrapRoom view))

        own @ children |> List.map Upgrade

    // One Claim per candidate colony's controller (ADR 0047), read off
    // the one rule that says which those are (`claimTargets`).
    let claims = claimTargets view |> List.map (fst >> Claim)

    // One Reserve per projected controller that is not the colony's own
    // (ADR 0042): a neutral controller held by CLAIM parts pays its room's
    // sources ten a tick instead of five, the hold decays by one a tick,
    // and so the Task stands whatever the reservation has left on it —
    // what the ticks remaining size is the body (#131), not the pool.
    //
    // Read off the projection's kind census and never off the declared
    // outposts (`Colony.declared`): the declaration is the shell's input
    // to the projection and every other rule here derives from what the
    // projection actually carries (ADR 0041), so a room a stand-down keeps
    // out of the scan set (ADR 0043) leaves this pool with it rather than
    // through a second gate free to disagree with the first. The census is
    // id-keyed and so unlayered, which is exactly right here: whose
    // controller it is turns on the id and never on a tile, so no
    // coordinate two rooms share can answer it.
    //
    // The colony's own controller is excluded by id, and every controller
    // standing in a room the colony owns with it. The engine refuses
    // reserveController on a room we own, and the home controller is what
    // Upgrade acts on: pooling both for one target would put a Task in the
    // pool no body can ever execute. The id alone said that while home was
    // the only room the colony owned; the tick a declared outpost is
    // claimed it stops saying it (#181), and the Task left standing there
    // is one the Matcher will happily fill — travel cost knows nothing
    // about ownership — so the rule is spelled by room, off the same
    // `colonyOwns` the reserver row's quota drops the room with, and the
    // pool and the row cannot disagree about which controllers are
    // reservable.
    //
    // The room comes off the projection's own placement and not off an
    // Atlas the Planner is never handed (ADR 0041). A controller the
    // projection does not place names no room and stays pooled, which
    // classifies nothing and blocks nothing (ADR 0004).
    //
    // A controller carries exactly one Task, and Claim is the one that
    // wins (ADR 0047): a candidate colony's controller is the room we are
    // taking, and the reservation on it is work that ends the tick the
    // claim lands. Both Tasks pooled for one target would be two jobs a
    // CLAIM body is applicable to, matched by travel cost alone — which
    // knows nothing about the difference — so the colony would hold the
    // reservation of a room it is trying to own and never take it.
    let reserves =
        let home = view.Controller |> Option.map (fun c -> c.Id)
        let claimed = claimTargets view |> List.map fst |> Set.ofList

        let inRoomWeOwn id =
            SpatialInfo.placementOf view.Spatial id
            |> Option.map (fun tile -> tile.Room)
            |> Option.exists (colonyOwns view)

        idsOfKind Controller
        |> List.filter (fun id ->
            Some id <> home && not (inRoomWeOwn id) && not (Set.contains id claimed))
        |> List.map Reserve

    // The haul cycle's intake (ADR 0012), shaped over the projection's
    // stores rather than energy's name: every stocked container yields a
    // Withdraw, at feeding tier beside Harvest — whether to dig or to
    // collect is travel cost's call, never a rule's.
    let stored id =
        view.Spatial.Stores |> Map.tryFind id |> Option.defaultValue 0

    let containers = idsOfKind (Structure BuiltKind.Container)
    let storages = idsOfKind (Structure BuiltKind.Storage)

    // A tombstone and a ruin are stores the same way (#167), so they pool
    // through the same line: a store with energy in it yields a Withdraw,
    // and what will become of the thing holding it is not this pool's
    // question. The engine's `withdraw` takes either object, and the cap
    // (#161) and the tier are read off the stock and the kind exactly as a
    // container's are.
    //
    // Where the two part is only in how they end. A container is emptied
    // and stays; these are drawn down and then vanish — a tombstone in a
    // hundred ticks or so whether anyone comes for it, a ruin on its own
    // decay — so a holder halfway there loses its Task to the ordinary
    // task-gone release the tick the projection stops carrying it (ADR
    // 0013's shape: the Task exists exactly while its condition does).
    // That churn is the price of the energy, and the energy is a whole
    // Anchor's store: 408 stood in one tombstone at t140,810 while the
    // colony dug.
    let tombstones = idsOfKind Tombstone

    let withdraws =
        containers @ tombstones
        |> List.filter (fun id -> stored id > 0)
        |> List.map Withdraw

    // The piles worth walking to (#167): a dropped pile at or over
    // `pickupThreshold` is a Feeding-tier Task, and every smaller one is
    // left to the reflex that costs nothing.
    //
    // The amount and nothing else. Whether the pile is at somebody's feet
    // already is a fact about a creep, and the Planner is creep-blind by
    // construction (ADR 0013's own reason for pooling a drained rock): the
    // pile a hauler is standing on is one the reflex takes this tick and
    // the pool loses next tick, which is the same release every emptied
    // store gets. Where both fire on one tick they spell the same creep's
    // same act twice, and `decide` drops the second where the two lists
    // meet rather than either producer narrowing its rule — what stays two
    // asks is two *creeps* at one pile, which is the case the reflex
    // deliberately lets the engine settle.
    let pickups =
        idsOfKind Dropped
        |> List.filter (fun id -> stored id >= pickupThreshold)
        |> List.map Pickup

    // The haul cycle's outflow: the controller container is one more
    // Refill target (ADR 0010's target layering, widened by ADR 0012).
    // Which container is the controller's is judged by geometry — it
    // stands inside the Upgrade Work Area (range 3) the Layout picked it
    // from, while a source container's tile (the Seat-standing kind) is
    // never a Refill target.
    //
    // The controller fixes the room, and the containers are then read out
    // of that room's layer rather than out of the kind census's whole id
    // space (ADR 0041). Range 3 is a distance inside one room: a container
    // in another room is not the buffer this controller's upgraders draw
    // from however near its coordinates fall, and pooling it as one would
    // send a hauler to fill a store nobody upgrades out of. A container
    // the controller's room does not place drops out, exactly as an
    // unplaced one always has (ADR 0004).
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
                    && stored id < containerCapacity
                | None -> false)
            |> List.map Refill)
        |> Option.defaultValue []

    // The colony's stock is the outflow's last stop (ADR 0023): a standing
    // Storage with room is one more Refill target, on the deepest tier of
    // all. Recognised by the projection's kind, as the tier is — the
    // Layout puts the one Storage on the cluster's first pick, so no
    // position rule could name it.
    let storageRefills =
        storages
        |> List.filter (fun id -> stored id < storageCapacity)
        |> List.map Refill

    // The stock's other half (ADR 0023): a stocked Storage is a Withdraw
    // source too, but only while the pool holds a Refill whose target is
    // not the stock itself — some sink other than it has room. Its own
    // Refill is deliberately no such sink: counting it would gate the
    // Storage open against itself for as long as it had room, and a hauler
    // beside a store that is both its only intake and its only sink cycles
    // energy in and out of it tick after tick — the ADR 0019 loop in the
    // one shape that gate cannot cure, since the bodies that must feed the
    // spawn from the stock are the ones with no Work part. The ADR 0013
    // shape: the Task exists while the condition holds, so a holder
    // mid-trip is released through task-gone the tick the last other sink
    // fills. What the gate closes is the cycle with nowhere else to go —
    // both halves can still be pooled on one tick, and a part-loaded
    // hauler is applicable to both; there the tier gap is what carries the
    // load away instead of putting it back, the draw shallower than every
    // sink but the flow's own and the stock's Refill deeper than all of
    // them.
    let storageWithdraws =
        if List.isEmpty refills && List.isEmpty containerRefills then
            []
        else
            storages |> List.filter (fun id -> stored id > 0) |> List.map Withdraw

    flees
    @ harvests
    @ withdraws
    @ pickups
    @ refills
    @ builds
    @ repairs
    @ upgrades
    @ reserves
    @ claims
    @ containerRefills
    @ storageRefills
    @ storageWithdraws

/// Screeps CARRY_CAPACITY: energy one Carry part holds.
let private carryPartCapacity = 50

/// What one body of this shape hauls in a trip: its Carry parts at the
/// engine's per-part capacity. Two readers turn Carry parts into energy —
/// the hauler quota divides a source's output over a round trip by it (ADR
/// 0012), and a Withdraw's cap divides its store's stock by it (#161) — so
/// the arithmetic is written once and neither can grow a second per-part
/// rule. It holds nothing about the *bodies* they pass: the quota reads
/// each spawn's own bank room and takes the minimum, the cap reads the
/// richest bank and the row that draws the store, and those two numbers
/// part the moment a second banked room outranks a spawn's own. Each call
/// site argues its own choice of body where it makes it.
let private carryCapacityOf body =
    (body |> List.filter ((=) Carry) |> List.length) * carryPartCapacity

/// Ceiling division over the quota rows' arithmetic: a quota that came
/// out a fraction of a body hires the whole body (ADR 0012 for the hauler
/// row, ADR 0037 for the worker row), because the fraction a floor drops
/// is demand nobody is hired for. A numerator at or below zero lands at
/// or below zero — F# divides toward zero — and each row's own floor
/// answers for it.
///
/// What it never says is *how much* demand one call is rounding: that is
/// the granularity, and it belongs to each row's own rule. The hauler
/// row's is the colony (ADR 0049) — one call over the summed round trips,
/// where it used to be one call per source container.
let private ceilDiv numerator divisor = (numerator + divisor - 1) / divisor

/// What one source of a room the colony holds this way is worth per tick
/// (ADR 0042): the held rate in a room this colony owns or reserves, half
/// of it in a room nobody holds. The whole rate rule, so that the census
/// signature below can sign exactly what the memoised quota reads rather
/// than a paraphrase of it.
///
/// Owned **or** reserved, never reserved alone. The engine gives a room
/// carrying either the same 3,000 a cycle, and the colony's own room is
/// owned while nothing reserves it: read as "reserved, or half", the two
/// home sources would price at five each and halve the hauler quota and
/// the income base together.
///
/// A room another player owns or reserves is priced at the neutral rate,
/// and that is a colony decision rather than an engine fact: the engine
/// gives *any* held room 3,000 a cycle, a rival's included, so a creep
/// digging there really would draw ten a tick. The colony declines to
/// size a fleet against it because a room somebody else holds is one it
/// is withdrawing from (the stand-down, ADR 0043), and energy it is
/// about to walk away from must not hire haulers or workers today. Since
/// #133 the rival's half of that sentence is a case the projection
/// spells (`Ownership.Rival`) rather than one it could only fail to
/// distinguish from an unowned room; the rate it earns is unchanged, and
/// deliberately so — this rule prices, and withdrawing is the gate's.
let private heldRateOf (control: RoomControlInfo) =
    if
        control.Owner = Ownership.Ours
        || control.Reservation
           |> Option.exists (fun held -> held.Holder = ReservationHolder.Ours)
    then
        heldOutputPerTick
    else
        neutralOutputPerTick

/// One source's **rate** per tick (ADR 0042), read off the room it stands
/// in: what the rock regenerates, and so the ceiling on what anything
/// standing over it can take out. A fact read per source and not a module
/// constant, because a reservation can lapse — a reserver dies, an invader
/// core taps the controller — and quotas sized for the held rate against a
/// source yielding five overbuild their rows twofold.
///
/// The rate and not the output: what a Post is *worth* is what the body
/// garrisoning it digs, which is this number only while the row's cast can
/// reach it (`sourceOutputOf` below, #208). Two readers want the ceiling
/// itself rather than the capped answer — `anchorWorkCapOf` just below,
/// which derives the row's Work cap from it and would otherwise size the
/// body off a number the body decides, and the cap inside `sourceOutputOf`
/// itself.
///
/// None for a source in a room the colony has no vision in this tick, and
/// for one the projection does not place: who holds a room we cannot look
/// into is not a fact this tick, so the source is unpriceable, enters no
/// quota and blocks nothing (ADR 0004). Unpriceable is not half — half is
/// what a room we can *see* nobody holds is worth, and the two answers
/// have to stay apart or a blind outpost would hire against income the
/// colony has no evidence for.
///
/// Which room a source stands in is the Atlas's id-to-room join, the layer
/// that places its id (ADR 0041), never the outpost declaration: the quota
/// is derived from the projection, and a source the projection does not
/// place counts nothing wherever it was declared.
let private sourceRateOf (view: ColonyView) atlas (sourceId: string) : int option =
    Atlas.targetRoom atlas sourceId
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.map heldRateOf

/// Whether a source is posted: whether a container stands on one of its
/// Seats, or a Dual Seat makes one of them a Post without a structure —
/// the switch that admits a source into the quotas at all (ADR 0042). One
/// spelling, read by the anchor row's ceiling below and by the income
/// base's own split (`workforceTarget`), so a rule that narrows what
/// counts as posted cannot narrow it for one of the two alone.
///
/// Judged in the source's own room, by `Atlas.standingPostsOf` and not by
/// testing its Seats against the home room's Posts: a `Pos` carries no
/// room, so a home Post standing on an outpost Seat's coordinates would
/// read that outpost source as posted with no container under it — a
/// phantom ten energy a tick in the income base, and a phantom Anchor
/// place beside it.
///
/// The **standing** census and not `Atlas.postsOf`, since #205 made the
/// two differ: a container site on a Seat is a Post — a place to garrison
/// an Anchor that digs and raises it — and it is not yet a container. What
/// this predicate switches on is what a source is worth to the colony's
/// quotas, and a rock whose energy goes into 5,000 progress and never into
/// a store pays no haul term and feeds no mouth at home. The clause of
/// ADR 0042's sentence this predicate spells is unmoved — the tick the
/// container stands, the source enters the income base — and the two
/// beside it, "becomes a Post, gains an Anchor", are the ones #205 moves a
/// few hundred ticks earlier; that ADR carries the amendment.
let private isPosted atlas (s: SourceInfo) =
    Atlas.standingPostsOf atlas s.Id |> Set.isEmpty |> not

/// The anchor row's Work ceiling this tick (ADR 0021 as ADR 0042 narrows
/// it): the saturation of the richest source the row is hiring for, plus
/// the one spare Work. The row's ceiling stops being the held rate written
/// as a constant and becomes a fact read off the projection — a source
/// under no reservation regenerates half as much, and six Work on it drain
/// it in 125 ticks and then idle for 175, buying spawn energy nothing digs.
///
/// **The input is the set of posted sources and not one Post**, and that
/// is ADR 0021's own answer rather than a convenience. It already
/// considered sizing an Anchor by the Post it will man and rejected it:
/// *"a spawn does not know which Post an Anchor will man — the creep
/// chooses by matching after birth (the no-role axiom, ADR 0006)"*. A cast
/// is a body, not a posting; travel cost pins it on the Post nearest it
/// once it is alive, and that Matcher knows nothing of any source's
/// output. So the ceiling has to be one colony-wide number over a *set* of
/// the sources the row hires for and never one of them, and the two
/// questions left are which sources are in the set and which way to be
/// wrong when they disagree with each other.
///
/// **The largest, for the same reason the reserver row casts at its
/// largest outstanding demand** (`reserverClaimsOf`): the two errors are
/// not each other's mirror. An Anchor over-sized for a neutral source
/// wastes 300 energy of body once in 1,500 ticks — a fifth of an energy a
/// tick — and still digs everything the rock has. An Anchor under-sized
/// for a held source digs 6 a tick where the rock gives 10, and loses four
/// energy a tick for its whole life. Over-buying is the safe direction by
/// a factor of twenty, and it is the direction `bodyFor` above is already
/// wrong in for the lead.
///
/// Posted sources and not projected rooms: a room with no Post hires no
/// Anchor, so its rate is not this row's business. A posted source the
/// colony cannot price this tick contributes nothing rather than a rate it
/// has no evidence for (ADR 0004), and a set with nothing priceable in it
/// answers the held ceiling: the largest the rule gives, which is the safe
/// direction above and today's number besides.
///
/// **The rock's rate and never `sourceOutputOf`'s capped answer** (#208).
/// That answer is the rate capped by what this row's own cast digs, so a
/// ceiling derived from it would size the body off a number the body
/// decides: at a 300 bank the cast is `2W`, the capped output four, the
/// ceiling three, the next cast still `2W` — a circle that can only
/// ratchet downwards and never lets a growing bank buy the Work the rock
/// is waiting for. The cap is the room's; the body's dig rate is applied
/// after it, where the quotas read a store.
///
/// **What this does not yet buy is a number the live colony can move.**
/// The colony's own room's Posts are in the set folded here and an owned
/// room prices at the held rate, so `List.max` is `heldOutputPerTick` in
/// every state a colony with one posted home source can reach: while a
/// reservation is lapsed the outpost's Anchor is still cast at six Work
/// against a rock giving five. ADR 0042's "a lapsed reservation is now a
/// visible economic event" is delivered by the hauler quota and the income
/// base, which do shrink on their own (#127), and not yet by this row's
/// body — and since #208 those two shrink only where the cast can dig
/// past the lapsed rate, which is a bank of 400 and up. Under it the row
/// digs four whatever the controller says and the lapse costs the colony
/// nothing to hear about, because it costs the colony nothing: a `2W`
/// Anchor takes the same four a tick out of a rock giving five as out of
/// one giving ten. Narrowing the fold to the Posts a cast is actually filling —
/// #132's other option — is deferred rather than refused, and it is a
/// wider change than it looks: the unmanned Posts have to be paired to
/// their sources *at arrival* (ADR 0026), or an ordinary succession sizes
/// the home room's replacement off an outpost's neutral rock and loses the
/// four energy a tick this fold exists to protect, and the amortization
/// below has to charge per Post rather than one ceiling times the quota.
let private anchorWorkCapOf (view: ColonyView) atlas : int =
    view.Sources
    |> List.filter (isPosted atlas)
    |> List.choose (fun s -> sourceRateOf view atlas s.Id)
    |> function
        | [] -> heldWorkCap
        | rates -> List.max rates |> workCapOf

/// What one source is **worth to the quotas that read a store** (ADR 0042
/// as #208 amends it): what the Anchor row's cast digs there, capped at
/// the rate its room pays. The rock's rate is the ceiling and never the
/// answer — a Post yields what the body garrisoning it takes out of it,
/// and a bank that cannot buy the Work to drain a source does not earn ten
/// a tick because the room would have paid ten.
///
/// The defect that put it here, live at t169,0xx: at a 300 bank the row
/// casts `anchorBodyFor workCap 300` = `2W/1C/1M`, which digs four a tick,
/// while `sourceRateOf` said ten. A child colony with two Posts read
/// twenty a tick of income it never earned, hired eighteen workers off it
/// and left twelve of them standing idle beside four haulers with nothing
/// to ship. The home room never showed it because at an 1,800 bank the
/// cast is `6W` — twelve a tick, over the rate — and the cap is not
/// binding: this rule changes no number a colony past the early banks
/// reads.
///
/// **The row's cast at this bank, and emphatically not the living
/// Anchor's body.** A quota read off a living body oscillates on that
/// body's death — the income base would halve the tick an Anchor expired
/// and double again when its successor arrived, and the worker row would
/// chase it — where what the row *casts* is a colony fact that holds
/// whether the Post is manned or empty (ADR 0006). It is the same call
/// `surplusOverLifetime` already prices the row's amortization by, so what
/// the colony charges the row for and what it credits the row with digging
/// come out of one body.
///
/// One number over the whole colony rather than one per Post, because the
/// body is: `anchorWorkCapOf` folds the posted set into one ceiling for
/// the reason recorded there — a cast is a body and not a posting, and no
/// caster knows which Post the Anchor it casts will man (ADR 0021, ADR
/// 0006).
///
/// Unpriceable stays unpriceable: the cap is applied to a rate that is
/// there, so a source in a room the colony cannot see is still None and
/// still enters no quota (ADR 0004).
let private sourceOutputOf (view: ColonyView) atlas (sourceId: string) : int option =
    // The Work the row would cast this tick times HARVEST_POWER — the same
    // `anchorBodyFor anchorWorkCapOf view.Bank.Capacity` triple the
    // amortization is priced by, so the two readings cannot drift apart.
    let dug =
        anchorBodyFor (anchorWorkCapOf view atlas) (view.Bank.Capacity)
        |> List.filter ((=) Work)
        |> List.length
        |> (*) harvestPerWork

    sourceRateOf view atlas sourceId |> Option.map (min dug)

/// The hauler row's quota rule (ADR 0012) — the row's colony fact, per
/// ADR 0006's law that a row arrives with its quota or not at all:
/// ceil(Σ over the source containers of round-trip travel ticks to the
/// spawn × that container's own source's output, ÷ the cast body's carry
/// capacity), so a farther container hires proportionally more haul
/// capacity and never quietly overflows. The spawn is the canonical sink
/// because the trunks radiate from it; of several spawns the cheapest
/// wins. No source containers, no placed spawns, or unreachable geometry
/// hire nothing.
///
/// **One rounding, for the colony** (ADR 0049, succeeding ADR 0012 and ADR
/// 0037 on the granularity alone): the demands are summed first and the
/// ceiling is taken once. Rounding each container up on its own bought a
/// body per fraction — three outpost containers wanting 1.3 haulers each
/// hired six where the flow asks for four — because a hauler is not the
/// property of the container it was hired for: since #161 a Withdraw's
/// capacity is its own store's stock divided by a hauler load, so a
/// container that fills faster than it is drained admits more drawers than
/// one that does not, and the shared integer is spent where the energy
/// actually stands rather than pinned per container at hiring time. The
/// cap is a **capacity and not an order** — `tierOf` files every source
/// container's Withdraw on the feeding tier alike and travel cost ranks
/// inside it, which is the near container's advantage and not the far
/// one's — so what the cap buys the row is room at the far end, never
/// priority there. What ADR 0012 rejected was the *flat* quota, one hauler
/// per container regardless of distance, and this is the opposite of that:
/// every container's own round trip is still priced, and only the fraction
/// it leaves behind is now added to its neighbours' instead of being
/// bought outright.
///
/// The output is that source's and not the colony's (ADR 0042), which is
/// why the fold resolves each tile back to the rock it serves rather than
/// only asking whether it serves one: a container over an unreserved
/// source ships half as much, and a colony-wide ten would put twice the
/// haul capacity on it. A container whose source's room the colony cannot
/// price hires nobody at all, the same answer unreachable geometry gets
/// (ADR 0004).
///
/// And that output is what the Post's garrison digs, capped at the rock's
/// rate (`sourceOutputOf`, #208): a container is filled by the Anchor
/// standing on it and never by the room's regeneration, so at a 300 bank —
/// where the row casts `2W` and digs four a tick — this row hired three
/// bodies to ship a flow that asks for one, and the four of them stood
/// full beside a spawn already at capacity.
///
/// Every room the projection carries, and not the colony's own alone
/// (ADR 0042): an outpost's container ships its source's energy home
/// across a border, so it hires haul capacity exactly as a home
/// container does, and the round trip it hires against is
/// `Atlas.haulRoundTripTicks` joined on the Seam band — the same
/// arithmetic #123 landed for the walk and the ranking price, run once
/// per leg because the loaded body and the empty one are two journeys
/// (ADR 0029, ADR 0030). ADR 0042 costs that trip at 138–168 ticks
/// unpaved, so such a container puts one to one and a half bodies of
/// demand into the sum where a home one puts a fifth of a body: the
/// number is large because the haul is long.
///
/// **That number does not reopen ADR 0038.** ADR 0038 declined to fill the
/// colony's Link footings and said its refusal flips "when the hauler row
/// leaves its floor", and these containers do lift the row off it. But a
/// link cannot cross rooms — the engine resolves a transfer's target out
/// of the *local* room's objects — so the pair of links ADR 0038 declined
/// can never reach an outpost's container, and no haul they could ever
/// shorten is counted here. ADR 0038's flip condition is about the home
/// room's source containers alone (ADR 0042); a reader arriving at it with
/// this fold's outpost total in hand is holding the wrong number.
///
/// The room is the container's own throughout, carried beside its tile
/// rather than assumed, because a `Pos` names none (ADR 0041): the source
/// judgement is made inside the container's room, so a home container
/// beside an outpost source's *coordinates* serves nothing, and the round
/// trip is flooded from that room's grid, so nothing is ever walked over
/// terrain it does not stand on. A container the projection places in no
/// room at all is priced by nothing and hires nobody, which is ADR 0004's
/// answer for geometry this query cannot see — as is a Seam band the body
/// cannot pay a crossing on.
///
/// Reading a second room's `RoomControl` entry is what the census
/// signature had to widen for, and did (`censusSignature`): every
/// projected room's held rate is signed, because any of them can hold the
/// container this fold prices next tick.
let private haulerQuota (view: ColonyView) atlas : int =
    // Each source container beside the room it stands in and the output of
    // the rock it serves: the tile alone cannot be priced, so a container
    // the projection places in no room, or one the fold cannot resolve to
    // a source, or to a source whose room it cannot price, leaves the list
    // here rather than entering the sum at some default rate.
    let sourceContainers =
        view.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            if kind = Structure BuiltKind.Container then
                SpatialInfo.placementOf view.Spatial id
            else
                None)
        |> List.choose (fun container ->
            sourceContainerServes view container.Room (RoomPos.pos container)
            |> Option.bind (sourceOutputOf view atlas)
            |> Option.map (fun output -> container, output))

    // One load, for the whole colony, and the row's own body cast at the
    // richest bank: rounding once (ADR 0049) sums demands before it
    // divides, so every term has to be a fraction of the *same* body or
    // the integer at the end counts nothing. The bank's capacity is the load
    // the rest of the pipeline already means — `workforceTarget` charges
    // this row's amortization at it and a Withdraw's cap divides its
    // store's stock by it (#161) — so denominating the sum anywhere else
    // would give the colony a third reading of "one hauler load" and let
    // the quota, the charge and the cap disagree with each other. Never
    // zero: the row's sizing casts one whole block at any bank
    // (`wholeBlockBodyFor`), so the divisor is a hundred at worst.
    let body = bodyFor haulerPattern (view.Bank.Capacity)

    let capacity = carryCapacityOf body

    // The sink's room is the projection's and never `SpawnInfo.RoomName`:
    // the two agree on the live colony and a fixture that names one and
    // files the other would flood an empty grid (ADR 0041).
    let sinks =
        view.Spawns |> List.choose (fun s -> SpatialInfo.placementOf view.Spatial s.Id)

    // Each container's own round trip at its own cheapest sink, priced at
    // its own source's output — the fraction of a hauler it asks for, and
    // never that fraction rounded — summed over the colony. The minimum is
    // over travel and nothing else now that one body prices every leg, so
    // "its cheapest spawn" is the near one rather than the rich one.
    let demand =
        sourceContainers
        |> List.sumBy (fun (container, output) ->
            sinks
            |> List.choose (fun sink ->
                Atlas.haulRoundTripTicks atlas body container sink
                |> Option.map (fun ticks -> ticks * output))
            |> function
                | [] -> 0
                | demands -> List.min demands)

    // The colony's whole haul, rounded once (ADR 0049).
    ceilDiv demand capacity

/// Screeps CREEP_LIFE_TIME: the ticks a spawned creep lives — the horizon
/// a body's replacement cost is amortized over.
let private creepLifetime = 1500

/// Screeps CREEP_SPAWN_TIME: the ticks a spawner spends per body part —
/// the half of a lead that is paid before the replacement takes its first
/// step.
let private spawnTicksPerPart = 3

/// Screeps UPGRADE_CONTROLLER_POWER's energy cost: what one Work part
/// drains per upgrade tick — the rate an upgrade mouth eats income at.
let private upgradeDrainPerWork = 1

/// What one body of this shape drinks a tick standing at a controller: its
/// Work parts at the rate above. Two rows are hired out of the same
/// surplus by dividing it by this — the standing row first
/// (`upgraderDrain`) and the commuting row by what is left
/// (`workforceTarget`) — so the arithmetic is written once, as
/// `carryCapacityOf` above is: the quota and the remainder it leaves are
/// two halves of one division, and a second per-part rule would let them
/// be computed against two different drains, with the difference landing
/// silently on the worker row.
let private upgradeDrainOf body =
    body
    |> List.sumBy (function
        | Work -> upgradeDrainPerWork
        | _ -> 0)

/// Screeps CONTROLLER_RESERVE_MAX: the ticks a reservation caps at. The
/// deficit the reserver row sizes off is measured from here down (ADR
/// 0042), so a reservation standing at the cap asks for the smallest body
/// the row can cast and nothing bigger.
let private reservationCap = 5000

/// Screeps CREEP_CLAIM_LIFE_TIME: the ticks a body carrying a CLAIM part
/// lives — well short of the 1,500 every other row gets. Two things read
/// it: the deficit's divisor (one CLAIM part holds the reservation up
/// through a whole such life, which is TooAngel's rule and ADR 0042's),
/// and the reserver row's amortization, which must spread its body cost
/// over *this* life rather than over `creepLifetime` — 1,300 over 600 is
/// the 2.17 energy a tick ADR 0042 prices the row at, and over 1,500 it
/// would read as 0.87 and undercharge the row by a factor of two and a
/// half.
let private claimLifetime = 600

/// The reserver row's body for one outpost (ADR 0042): the deficit sizing
/// and the bank truncation, whichever asks for less, never below one
/// block. The deficit arrives as a second capacity ceiling — `claims`
/// blocks' worth of energy — because "as many whole blocks as capacity
/// buys" is already `reserverBodyFor`'s rule and the smaller of two
/// ceilings is the smaller of two block counts.
///
/// This row is the only one whose body is *not* the bank's answer alone,
/// and ADR 0042 refuses the bank on its own terms: at RCL6 a 2,300 bank
/// buys a third CLAIM for a reservation that caps at 5,000 anyway. Today's
/// RCL5 bank of 1,800 agrees with the deficit at `[2Claim;2Move]`, so the
/// two rules are told apart only at a richer bank.
let private reserverBodyWithin claims capacity =
    reserverBodyFor (min capacity (claims * bodyCost reserverPattern.Block))

/// The reserver row's quota and its sizing, which are one rule with two
/// faces (ADR 0042, ADR 0006's law that a row arrives with its quota):
/// one reserver per **declared** outpost, each wanting
/// `ceil((5000 − ticks this colony holds) / 600)` CLAIM parts. The list's
/// length is the quota; each entry is what that outpost's body asks for.
/// No state is kept between ticks — the deficit recomputes from the
/// reservation itself, shrinks to one block in steady state, and comes
/// back bigger on its own the tick a reservation has slipped, which is
/// also the whole of what "a dead reserver's room hires a bigger
/// replacement" needs.
///
/// The row hires for one more thing than reservations (ADR 0047): a
/// **candidate colony** takes one entry of a single block, and its room
/// leaves the reservation demands, because a controller carries one Task
/// and a candidate colony's is the Claim. The body is the same
/// `[Claim; Move]` block either way, which is why this is one row and not
/// two; the clause is spelled where the demands are built, below.
///
/// **Declared and not posted**, which is where #131's own correction
/// comment overrides its ticket text and ADR 0042's "one reserver per
/// posted outpost" clause. Gating this row on a standing container
/// deadlocks the outpost chain: a container site needs vision (#128),
/// vision needs a creep in the room, and the only creep with a reason to
/// go is this one — a worker picks the cheapest feeding tier and Storage
/// is a few tiles away where an outpost rock is fifty. ADR 0042's own
/// Considered Options settle it against its Consequences clause, having
/// already rejected "mine first, reserve later" because *"reserving from
/// the start also doubles the outpost's output before the first hauler is
/// sized"*. So the row hires for a room that has produced nothing yet:
/// it **is** the bootstrap — it walks there, supplies the vision the
/// container needs, and turns five a tick into ten from the first day.
/// The scan set is the gate that remains, and it is the one ADR 0043's
/// stand-down narrows: a room withdrawn from is not projected, carries no
/// controller here, and hires nobody.
///
/// **Carrying a controller of its own in the projection**, which is
/// the row meeting its Task (ADR 0006): `planTasks` pools one Reserve per
/// projected controller that is not the colony's own, so a room with no
/// such controller offers a CLAIM body nothing to do — it would stand
/// where it was born for its whole 600-tick life, applicable to Flee and
/// to nothing else. The colony's own controller is excluded by id there,
/// and every controller in a room this colony owns by `colonyOwns` — the
/// same read this row's own clause below drops the room with, because the
/// engine refuses reserveController on a room we own and a Task no body
/// can execute must not be pooled for one to be matched to.
///
/// The *rooms* drop out, and every cast this tick is sized at the largest
/// demand in the list rather than at the demand standing beside it. The
/// row cannot do better: the quota counts bodies, and which controller
/// each finished body ends up holding is the Matcher's, through the
/// Reserve Task's one-holder-per-controller capacity (#130), which prices
/// the nearer controller cheapest and knows nothing about a deficit. So a
/// list read positionally would hand the nearer room the body the further
/// one asked for, and a room reserved by one CLAIM against the engine's
/// one tick of decay is a room frozen where it stands for the whole
/// 600-tick life of that body. Over-buying is the safe direction and the
/// same one `reserverBodyFor` is wrong in for the lead (ADR 0026); the
/// bank truncates it anyway, and the demands differ only while a
/// reservation is slipping.
///
/// **The bank must afford one block**, or the row hires nobody at all.
/// Its floor body is 650 — larger than every other row's, and larger than
/// the whole bank below RCL3. A colony that cannot buy a reservation does
/// not hold one.
///
/// The clause's original reason is gone and the rule is not: it read that
/// a gap this row can never fill would stop the cascade under it forever,
/// the home room's Anchors and haulers included, since `planSpawns` gave
/// each idle spawn the first unfilled row and this one would always be it.
/// A row that cannot be afforded now yields the tick to the rows below it
/// (ADR 0050), so nothing stalls behind this one any more; what is left is
/// the quota being honest — a row hired against a body the colony can
/// never buy is an addend of the Workforce target and of its amortization
/// that no cast will ever pay off.
///
/// **The bank's `Capacity` and never its `Available`**, which is the reading
/// #203's report proposed and the one this quota may not take. A row's
/// quota is a colony fact (ADR 0006), and Available is the energy standing
/// in the extensions this tick — a number this colony's own spawning moves.
/// Read that way, casting one 1,300 reserver out of an 1,800 bank would
/// drop the quota to zero on the very next tick, taking one body per
/// declared outpost out of the target and the reserver term out of the
/// amortization, and handing it all back when the extensions refill. The
/// cast's own affordability is checked where it belongs, at the cast
/// (`planSpawns`); this is the question of whether the row exists at all.
/// The capacity is also one field with four readers that must not
/// disagree — `ColonyView.Bank`'s own doc says so — so narrowing it here
/// would be a second
/// reading of the colony's bank beside the one every Withdraw cap uses.
///
/// A reservation another player holds counts as no hold at all, exactly as
/// it does for the source rate: the ticks left on it are theirs, and ours
/// starts from zero. A room the colony cannot see this tick has no control
/// entry and reads the same zero, which is the right answer and not an
/// accident of blindness — a room we have no vision in is a room nothing
/// of ours is standing in to reserve.
///
/// **A room this colony owns is dropped**, and that is the engine's
/// refusal rather than a preference: `reserveController` answers
/// ERR_INVALID_TARGET on a controller with an owner. The Reserve pool
/// drops that room's controller by the same read (`colonyOwns`), so the
/// row hires against exactly the controllers the pool offers; the pool's
/// older exclusion by the home controller's *id* said the same thing only
/// while home was the only room this colony owned, and the tick a declared
/// outpost is claimed it stops saying it (#181). An owned room carries no
/// reservation, again because the engine refuses one, so `heldTicks` reads
/// zero and the deficit reads the whole 5,000: without this clause the row
/// would cast the bank's whole reserver body at it every 600 ticks forever
/// — `[2Claim;2Move]` at today's 1,800, and the deficit's own nine blocks
/// at a bank that affords them — each one walking over to fail at the
/// controller for its whole life. `heldRateOf` above already prices an
/// owned room at the held rate, so the economy's half of the same fact was
/// never wrong; only the hiring was.
///
/// Only `Ownership.Ours` needs the clause. A room another player owns is
/// one the colony is withdrawing from, and ADR 0043's stand-down takes it
/// out of the scan set — from the tick *after* the colony last saw it
/// held, since the gate reads the previous tick's raid log, so the one
/// tick between first sight and the withdrawal is priced here as a room
/// nobody holds. That window is bounded, pre-dates this clause, and is
/// left where the ticket left it.
let private reserverClaimsOf (view: ColonyView) atlas : int list =
    let home = view.Controller |> Option.map (fun c -> c.Id)

    let heldTicks room =
        view.RoomControl
        |> Map.tryFind room
        |> Option.bind (fun control -> control.Reservation)
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Ours)
        |> Option.map (fun held -> held.TicksToEnd)
        |> Option.defaultValue 0

    // The candidate colonies this tick, each asking for **one** block
    // (ADR 0047): the Claim row is this row, because both bodies are CLAIM
    // bodies and a second pattern row would be the same block under a
    // second name (ADR 0006), so `patternOf` reads a claimer back as a
    // reserver and the casting order, the gap and the amortization all
    // count it where they count one.
    //
    // One block and never the reservation deficit's nine: a claim is one
    // act by one CLAIM part, finished the tick it succeeds, so nothing
    // about it scales with a number of ticks. Sizing it off the deficit
    // would buy `[9Claim;9Move]`-worth of body — bank-truncated to the
    // colony's whole reserver budget — for a creep whose whole job is to
    // touch a controller once.
    //
    // The **entry** is one block; the **cast** is still this row's largest
    // outstanding demand, by the rule two paragraphs above. So a claimer
    // hired beside an outpost whose reservation has slipped is bought at
    // that outpost's body and not at one block — deliberately, and for the
    // same reason: which controller a finished CLAIM body ends up holding
    // is the Matcher's, priced by travel cost alone, so a body cast at one
    // block can land on the Reserve instead and freeze that room where it
    // stands. Over-buying the claimer is the safe direction; the demands
    // differ only while a reservation is slipping, and the entry is what
    // keeps the claim from *raising* the size the whole row is cast at.
    //
    // Its room is what drops out of the reserve demands beside it, and
    // that is `planTasks`'s own rule read here rather than restated: a
    // candidate colony's controller carries a Claim and no Reserve, so a
    // reserver hired for that room would arrive at a controller with no
    // Task on it and stand there for its whole 600-tick life. The room's
    // sources are worth five a tick until the claim lands and ten from the
    // tick after (`heldRateOf` prices an owned room at the held rate), and
    // any reservation still standing on it goes on being ours as it
    // decays — so what the colony gives up by not refreshing it is the
    // few hundred ticks between the claimer's cast and its arrival.
    //
    // That bound holds while the account can *pay* for the claim, and
    // nothing here can check that it can: `claimController` answers
    // ERR_GCL_NOT_ENOUGH when there is no GCL level to spare, the view
    // carries no GCL fact at all, and a room that stays unowned stays a
    // candidate — so a colony declared before the level is there loses the
    // reservation for good and reads five a tick until a human sees the
    // Executor's log. The declaration is the human's sentence, and this is
    // the part of it the bot cannot check for them.
    let claims = claimTargets view
    let claimed = claims |> List.map snd |> Set.ofList

    if view.Bank.Capacity < bodyCost reserverPattern.Block then
        []
    else
        let reserved =
            view.Spatial.TargetKinds
            |> Map.toList
            |> List.choose (fun (id, kind) ->
                if kind = Controller && Some id <> home then
                    Atlas.targetRoom atlas id
                else
                    None)
            |> List.distinct
            |> List.filter (colonyOwns view >> not)
            |> List.filter (fun room -> not (Set.contains room claimed))
            |> List.map (fun room ->
                ceilDiv (reservationCap - heldTicks room) claimLifetime |> max 1)

        reserved @ (claims |> List.map (fun _ -> 1))

/// The colony's surplus over one creep's lifetime: the income the two
/// upgrade rows are hired out of, written once here because both of them
/// read it and a paraphrase in either place would let them hire against
/// different money (ADR 0012, ADR 0046).
///
/// Income is counted per source at that source's own output and never at
/// a colony-wide ten (ADR 0042): an unreserved source is worth half a held
/// one, and a posted source whose room the colony cannot see this tick is
/// worth nothing at all rather than half (ADR 0004). This is the reader
/// #116 and ADR 0042 both leave out of their enumeration of the two —
/// their own prose is what puts it in, "posted ones enter the income base
/// at the source's output".
///
/// An output is **what the garrison digs, capped at the source's rate**
/// (`sourceOutputOf`, #208): the rate is what the rock regenerates and the
/// Anchor row's cast at this bank is what leaves it, so a colony whose
/// bank buys `2W` earns four a tick from a Post and not the ten the room
/// would pay a body big enough to take it. The row is charged its
/// replacement here at that same body, so credit and charge are one cast.
///
/// From that income the reserver, anchor and hauler rows' replacement
/// amortization (body cost spread over a creep's lifetime) is deducted.
/// Those three are hired off facts about the *ground* — a declared
/// outpost, a Post, a round trip — so their price is settled before the
/// surplus has a number, while the two rows hired out of the surplus
/// itself (ADR 0046's upgrader row, ADR 0012's worker row) are charged
/// against this number inside `workforceTarget`. That split is what keeps
/// the arithmetic acyclic: the upgrader quota is a function of the
/// surplus, so its own amortization cannot also be a term of it — it is
/// deducted where it is spent, from what the worker row is left. The
/// arithmetic runs scaled by the lifetime so the amortization never rounds
/// away. The **worker** row's own replacement cost is still not deducted —
/// a pre-existing home-room defect ADR 0042 names and deliberately does not
/// pay off here.
///
/// Negative is an answer and not an error: an amortization above income is
/// a colony whose specialists already cost more than its rocks bring in,
/// and both readers below floor their own row rather than clamping here,
/// where a zero would hide which row the shortfall fell on.
let private surplusOverLifetime
    (view: ColonyView)
    atlas
    reserverClaims
    anchorWorkCap
    anchorQuota
    haulerQuota
    =
    let capacity = view.Bank.Capacity

    // The row's own body, once, times the places it hires: every reserver
    // cast this tick carries the largest outstanding demand, so the charge
    // is priced off that same body and never off a per-room one the
    // casting step would not have cast.
    //
    // Scaled from a CLAIM body's own 600-tick life onto the 1,500 the rest
    // of this sum is written in (ADR 0042's 2.17 energy a tick): a reserver
    // is replaced two and a half times over one worker's life, and charging
    // it once would leave the income base hiring an upgrade mouth the
    // reservation is really paying for.
    let reserverCost =
        if List.isEmpty reserverClaims then
            0
        else
            List.length reserverClaims
            * bodyCost (reserverBodyWithin (List.max reserverClaims) capacity)

    // The anchor row charged at the body the casting step would actually
    // cast, under this tick's ceiling and not the held one (ADR 0042): a
    // row whose bodies shrank with a lapsed reservation while its
    // amortization went on deducting the six-Work price would hire an
    // upgrade mouth fewer than the income really feeds. The same rule the
    // reserver term beside it is written under.
    let amortization =
        anchorQuota * bodyCost (anchorBodyFor anchorWorkCap capacity)
        + haulerQuota * bodyCost (bodyFor haulerPattern capacity)
        + reserverCost * creepLifetime / claimLifetime

    // Summed over the posted sources at each one's own output, never a
    // count times a constant (ADR 0042): a source the colony cannot price
    // contributes nothing, which is the same zero it would contribute by
    // not being posted. The output is the garrison's dig rate under the
    // rock's own ceiling (#208), which is the body the amortization above
    // is priced at — one cast, read once as a cost and once as income.
    let income =
        view.Sources
        |> List.filter (isPosted atlas)
        |> List.sumBy (fun s -> sourceOutputOf view atlas s.Id |> Option.defaultValue 0)

    income * creepLifetime - amortization

/// The standing body's line (ADR 0046): four is the ratio at which a
/// delivery stops being work and becomes a commute. A body under it carries
/// fifty energy a trip against eleven Work, so a Build or a Refill it walks
/// to spends one tick delivering for every tick of the walk out and the
/// walk back, and the Work it left standing beside the buffer earns nothing
/// meanwhile.
///
/// A tunable, and named here for that reason: the shape of the rule is not
/// one. A hauler is `Carry * n < 0`, false whatever `n` is, so the row whose
/// whole life is delivery is outside the gate by construction rather than
/// by this number; and the worker row is outside it by its own parity (ADR
/// 0003), which buys one Carry per Work where this line is one per four,
/// four times clear at every bank. What retuning it moves is the band
/// between those two — and, with it, the bank at which the upgrader row's
/// own cast becomes a standing body (five pairs at four, `upgraderBodyFor`).
let private standingCarryPerWork = 4

/// ADR 0046's ratio itself, over two part counts, written once because two
/// readers ask it of two different shapes: `isStandingBody` below of a
/// living creep's part map, and `isStandingCast` of a body this module has
/// just sized. They have to answer alike — a row whose quota and whose
/// living count disagreed about what a standing body is would be hired
/// against a gap it could never close.
let private standingRatio carryParts workParts =
    carryParts * standingCarryPerWork < workParts

/// Whether a body this module has sized is a standing body: the same ratio
/// over a part list rather than over a living creep's part map. The reader
/// is the upgrader row's quota, which asks it of the row's *own cast* at
/// this bank (`upgraderQuota`): below the 800 bank the sizing rule buys
/// `3W/1C/3M`, which is no standing body, so a body cast from that row
/// there is read back to the generalist by `patternOf` and the row's own
/// gap could never be paid off.
let private isStandingCast body =
    let count part =
        body |> List.filter ((=) part) |> List.length

    standingRatio (count Carry) (count Work)


/// Whether a living body is a **standing body** (ADR 0046): it carries
/// fewer than one Carry part per four Work — `Carry * 4 < Work`. Part
/// arithmetic and nothing else, like every other row-reading predicate
/// here (ADR 0006), and it is a fact about a *body* rather than about a
/// row: the upgrader row's `11W/1C/11M` is one, and so is the anchor row's
/// `6W/1C/1M`, and so would be anything else the colony ever casts with
/// the same shape.
///
/// The gate that reads it is `applicable` below, on Build, Repair and
/// Refill — and since #206 on Pickup and on every Withdraw but the
/// buffer's. What is left exactly as its own gates already had it is the
/// working life the upgrader row was shaped for: it draws from the buffer
/// at its feet (ADR 0019, through ADR 0016's gate, which `Work ≤ Move`
/// keeps it inside) and spends into the controller in place, or it digs
/// from its Post. A pile and the Storage are intakes too, but not ones at
/// its feet, and the walk to either is the commute ADR 0046 exists to
/// refuse. Untouched is not applicable, either: Withdraw stays shut to a
/// Work-heavy body by ADR 0016's own gate, so of the two standing bodies
/// the colony casts only the upgrader draws.
let private isStandingBody (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    standingRatio (count Carry) (count Work)

/// What one body of the upgrader row eats per tick: every Work part of the
/// row's cast at the richest bank, at the controller's own per-Work rate
/// (ADR 0046). Never below one — the row's sizing rule floors at a pair —
/// so the quota below always has a divisor.
let private upgraderDrain capacity =
    upgradeDrainOf (bodyFor upgraderPattern capacity)

/// The upgrader row's quota (ADR 0046, amended by #195): the surplus
/// divided by one standing body's drain, rounded **down** — the whole
/// bodies the surplus pays for, and the remainder handed on to the worker
/// row below, whose own division rounds up (ADR 0037) and is what turns a
/// part of a body into a hire.
///
/// The two rows are hired out of one surplus, so only one of them may
/// round up: ADR 0037 admits an oversell bounded by *one body's* lifetime
/// drain, paid out of stock rather than income, and two rows rounding up
/// against the same number sell that bound twice. Which row keeps the
/// rounding is settled by the size of the body it oversells — the bound is
/// the body, and this row's is the larger one. At the live RCL5 bank a
/// remainder of half a body rounded up here promised eleven Work it had no
/// income for: 16,500 more energy over a lifetime against a surplus of
/// 26,800, twenty-two a tick drawn against twenty coming in, the
/// difference made up out of the storage every tick of both lives. The
/// same remainder rounded up on the worker row buys nine Work, which is
/// the bound ADR 0037 argued for and the body it argued about.
///
/// **Non-zero only while a built controller container stands in the
/// room.** The buffer is this row's working ground — the Work Area of one
/// Task, and never a Post (ADR 0046 against ADR 0012's generalization) —
/// so a room with no buffer standing hires none and the worker row
/// commutes as it always did. The container plan places the buffer under
/// no level gate of its own, so "no buffer yet" is a fact about the room
/// rather than about RCL.
///
/// **And only while the row's own cast at this bank is a standing body**,
/// which is the bank floor ADR 0046's Consequences leave to #187 and the
/// half of the gate that makes the row *countable*. `planSpawns` reads the
/// row's living count through `patternOf`, off the parts and never off a
/// name (ADR 0006), so a bank whose cast the ratio reads back to the
/// generalist is a bank where the row's gap is the whole quota every tick:
/// under 800 the sizing rule buys `3W/1C/3M`, `isStandingCast` is false of
/// it, and a quota hired there would cast a body it could never count —
/// one every tick, forever, ahead of the whole-fleet deficit that gates
/// the generalist row. The gate is written as the predicate and not as the
/// number 800 so that it stays true *by* the sizing rule (`upgraderBodyFor`)
/// and the ratio (`standingCarryPerWork`) rather than against them.
///
/// What the colony loses in that band is nothing the row was for: three
/// Work against a fifty-energy load is not yet a commute, so the body the
/// generalist row hires out of the same surplus there is the same body at
/// the same price — which is the ratio saying what it was written to say.
///
/// A negative surplus hires none: `max 0` is this row's floor, and the
/// worker row's floor below is what keeps a body in the colony at all.
/// F#'s integer division truncates toward zero, so a shortfall smaller
/// than one body's lifetime drain already answers 0 unaided and the `max`
/// is the guard over the rest.
///
/// Built and not pending, because `Atlas.controllerContainers` folds the
/// standing census alone — a container *site* at the controller is a
/// promise, and a row hired against it would stand beside a hole with
/// nothing to withdraw from (ADR 0019 shuts it out of every other store's
/// draw by distance, not by rule).
let private upgraderQuota (view: ColonyView) atlas surplus =
    let capacity = view.Bank.Capacity

    if
        Set.isEmpty (Atlas.controllerContainers atlas)
        || not (isStandingCast (bodyFor upgraderPattern capacity))
    then
        0
    else
        surplus / (upgraderDrain capacity * creepLifetime) |> max 0

/// The worker row's floor (ADR 0046): the row's income term is whatever
/// the upgrader row has not eaten, and beside a buffer that can still be
/// nothing at all. The quota above rounds down since #195, so the
/// remainder it leaves is a real one — but it is bounded by one standing
/// body's whole lifetime drink, and the standing row's own replacement is
/// charged against it before the commuting row divides: a surplus landing
/// just past a whole multiple of that drink leaves the worker row nothing
/// at all. At the RCL4 bank one posted source is 13,100 of surplus over a
/// 12,000 drink — one body hired, 1,100 left, and the 1,250 that body
/// costs to replace takes more than the whole of it. A colony with no
/// generalist in it
/// builds nothing and repairs nothing: a standing body is shut out of all
/// three deliveries (ADR 0046), and the hauler row carries no Work part,
/// so a container site or a decaying road would stand while the
/// controller ticked up beside it.
///
/// Two while anything stands in the Build or Repair pool, one otherwise.
/// Both numbers are tunables and the shape of the rule is not: two,
/// because since ADR 0042 a builder crosses a Seam to raise an outpost's
/// container and the home room's own sites are unattended for the fifty
/// ticks of that walk; one, because a colony with nothing to build still
/// wants a body that can start when something appears, and hiring the
/// second against no pool at all would be hiring for a job that does not
/// exist — the same objection ADR 0012 retired the seat base for.
///
/// Read off the **pool** and never off the view's site list: the pool
/// is what the Matcher will actually offer, so work the Planner has
/// already withheld — out of a stood-down outpost (ADR 0043), behind a
/// Threat's reach (ADR 0033) — hires nobody to walk to it.
let private workerFloor (tasks: Task list) =
    let building =
        tasks
        |> List.exists (function
            | Build _
            | Repair _ -> true
            | _ -> false)

    if building then 2 else 1

/// The **pioneers**: how many more [[worker unit]]s the mother hires while
/// a nursery of hers stands (ADR 0047 decision 4). The addend on the worker
/// row's own share of the target, and the whole of what a nursery costs the
/// mother in bodies.
///
/// The worker row and no other, because what a nursery needs is a Build:
/// the hauler row carries no Work part, an [[anchor]]'s cross-room work is a
/// [[post]] and never a delivery (ADR 0020), and a [[standing body]] is shut
/// out of all three deliveries (ADR 0046). The generalist is the one row
/// that crosses a [[seam]] and spends into a site, which is the same reason
/// #157 gave for the outpost [[container]]'s builder.
///
/// **Three is a tunable and this is the reason for it.** A spawn is 15,000
/// progress against a generalist's fifty energy a trip, so the child's first
/// tick is many round trips away whatever the crowd, and the crowd is what
/// decides whether that is this cycle or the next; the user chose the crowd
/// over letting a 300-energy bank bootstrap itself, which ADR 0047 rejects
/// as an order of magnitude slower. Small because each of these bodies is
/// one the mother's own surplus work does without for the length of the
/// walk — the same price #157's builder cap is written against, and the
/// reason the number is a crowd rather than the worker row.
///
/// A **flat** addend and not one per nursery: ADR 0047 says the quota rises
/// by this while the child is not independent, and a declaration is written
/// one candidate at a time (`Colony.declared`). Two nurseries at once would
/// share these three between them, which is a state a human who wrote two
/// candidate colonies into the constant can see and raise this number for.
let private pioneerCount = 3

/// Workforce target (ADR 0012, ADR 0046): five addends, each a pattern
/// row's own colony fact — reservers one per declared outpost, Anchors one
/// per Post, haulers the throughput quota, upgraders the surplus divided
/// by a standing body's drain, workers the income arithmetic that is left
/// and the pioneers a nursery adds to it (ADR 0047)
/// — floored at minWorkforce and derived fresh each tick. A source whose
/// Post is provided for retires its other Seats: one heavy body drains it
/// alone, so counting seats after that is hiring for jobs that no longer
/// exist.
/// An unposted source of the home room still contributes its Seat count
/// — its output is spoken for by the seat crews that walk it — so only the
/// posted sources' output is income.
///
/// An unposted source of an outpost contributes nothing at all (ADR 0042).
/// The seat-crew justification presumes the walk is cheap, and across a
/// border it is not: the three declared outpost sources carry six Seats
/// between them, five of them swamp, and counted here they would hire six
/// generalists to commute forty-seven to fifty-six tiles to dig them. The
/// useful half of that exclusion is that a standing container is the
/// switch admitting an outpost into the economy — until one stands the
/// room is invisible to every quota *but one*, and the tick it stands the
/// source enters the two quotas that read a store: a hauler term at its
/// own round trip across the Seam, and a share of the income base at its
/// own output. The third of the three rows moved one step earlier with
/// #205: an Anchor place on the one row every Post hires from
/// (`Atlas.postCount`, in `planSpawns`) arrives with the container's
/// *site*, because the body that garrisons a Post is the one that raises
/// it. Three existing rows widened, and beside them the one rule an
/// outpost has of its own — and the one quota this switch does *not* gate:
/// the reserver row's, one per declared outpost, arriving here as
/// `reserverClaims`, the CLAIM demand of each. Its length is the addend
/// and its largest entry prices the amortization (ADR 0042). That row
/// hires before the container exists because it is what makes the
/// container possible: it is the only creep with a reason to walk to a
/// room that produces nothing yet, and the vision it brings is what lets
/// the site be placed at all (#128, #131's correction). Which room a
/// source stands in is the Atlas's own id-to-room join — the layer that
/// places its id, precomputed for every reader holding an Atlas (ADR
/// 0041) — never the constant: the projection is what the quota is derived
/// from, and a source the projection does not place is unpriceable and
/// counts nothing wherever it was declared (ADR 0004).
///
/// Being posted is `isPosted`'s one spelling above, room-joined for the
/// reason recorded there: a phantom Post read off a bare coordinate
/// collision would put ten energy a tick into the income base.
///
/// The income and the three ground-hired rows' amortization arrive
/// together as `surplus`, one number derived beside the quotas in
/// `planSpawns` and read here and by `upgraderQuota` alike. What is left
/// of it once the upgrader row has been charged — the energy those bodies
/// will drink, and their own replacement cost beside it, the term ADR 0046
/// adds to the amortization — feeds worker mouths at one worker body's
/// Work drain, rounded up so the mouths cover the surplus rather than fall
/// a body short of it (ADR 0037), bodies priced as the richest bank would
/// cast them. The **worker** row's own replacement cost is still not
/// deducted — a pre-existing home-room defect ADR 0042 names and
/// deliberately does not pay off here.
///
/// The upgrader row is charged where it is spent rather than in the
/// surplus itself, because its quota is a function of that surplus: a term
/// deducted before the division would be an input to the number it is
/// derived from. Both readings hire the same bodies and only one of them
/// terminates.
let private workforceTarget
    (view: ColonyView)
    atlas
    (tasks: Task list)
    reserverClaims
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

    // What the standing row takes out of the surplus before the commuting
    // one is hired against the rest (ADR 0046): the energy its Work drinks
    // over a lifetime, and the row's replacement cost over the same
    // lifetime — the amortization term this row adds, charged on the same
    // terms as the three rows charged inside `surplus` and priced at the
    // body the casting step below would actually cast.
    //
    // The second term bites since #195: the quota rounds down, so the
    // remainder is what is left of a whole body's drain and the standing
    // row's own replacement comes out of it before the commuting row is
    // hired against the rest. Under the old rounding it could not move the
    // target at all — a row hired at `ceil(surplus / drain)` drinks at
    // least the whole surplus, so the remainder was already at or under
    // zero before its bodies were charged — and it was written then for
    // the reason it earns now: what the sum says is the rule, and an
    // upgrade mouth hired out of energy the standing row's own replacement
    // is paying for is a mouth twice sold.
    let upgraderCost =
        upgraderQuota * upgraderDrain capacity * creepLifetime
        + upgraderQuota * bodyCost (bodyFor upgraderPattern capacity)

    // Rounded up through the same ceilDiv as the hauler row (ADR 0037):
    // the granularity a floor would drop is a whole worker body's Work,
    // which grows with RCL, and the income it drops leaks every tick
    // while the body it oversells is paid for out of stock. A surplus the
    // upgrader row has eaten whole — or an amortization above income —
    // leaves the quotient at or under zero, and max 0 is where the term
    // stops.
    let incomeWorkers =
        ceilDiv (surplus - upgraderCost) (workerDrain * creepLifetime) |> max 0

    // The pioneers (ADR 0047 decision 4): while a room this colony has
    // claimed still has no spawn in it, the mother hires `pioneerCount`
    // more generalists to go and raise one. Hired off a fact about the
    // *world* and not out of the surplus — a nursery is a room a human
    // declared and the colony has taken, exactly as the reserver row is
    // hired off a declared outpost — so it is added to the row rather than
    // divided out of what the upgrader row left. The analogy stops at the
    // target: this is an addend on the generalist row and reaches a spawn
    // through that row's whole-fleet gate, where the reserver row hires
    // against a gap of its own, so a specialist row standing over quota
    // holds these three down with the rest of the row (#154, unchanged).
    //
    // On top of the whole row and outside its floor. The floor is the
    // smallest crowd that can take a delivery at all (ADR 0046), and these
    // bodies are hired for a delivery that exists whatever the colony is
    // otherwise doing: a colony sitting at its floor with a nursery to
    // build hires three more, where a floor `max` over the sum would have
    // hired none of them.
    //
    // No term of `surplus` answers for these three, which is deliberate
    // and is the worker row's pre-existing shape (ADR 0042 names it): this
    // row's own replacement cost is not deducted anywhere, so a pioneer is
    // charged like every other generalist — which is to say not at all —
    // and the nursery's cost to the mother is the bodies and not a second
    // arithmetic.
    //
    // And the addend outlives the nursery (ADR 0047 decision 4): it runs
    // on while the child is bootstrapped — its own spawn standing, its own
    // `decide` running, its controller still under `bootstrapLevel` — which
    // is the second half of the same sentence and the crowd the child's
    // first Layout is built by. One addend and not two, flat over both
    // [[stage]]s for the reason it is flat over two nurseries: what a
    // mother spends on children is three bodies, and a human declaring a
    // second child can see the three being shared and retune the number.
    //
    // Swept over the stages, which is the declaration and the world in one
    // map (ADR 0052 decision 3): a declared home with no stage is not a
    // colony this tick and neither predicate can answer for it. The two
    // predicates are complements over one room — a claimed room has a
    // spawn of ours in it or it has not — so the `exists` cannot count one
    // room twice, and a room that leaves this colony's scan set at RCL3
    // answers neither.
    let pioneers =
        let raising room =
            isNurseryRoom view room || isBootstrapRoom view room

        if view.Stages |> Map.exists (fun room _ -> raising room) then
            pioneerCount
        else
            0

    // The generalist row's whole share of the target, and the floor sits
    // here rather than on the income term beside it (ADR 0046): both
    // addends hire the same body from the same row — a seat crew is a
    // worker unit, cast out of the same remainder the income term is —
    // so a colony already running three of them has three bodies that can
    // build, and a floor read off the income term alone would hire a
    // fourth against a job that does not exist. What the floor is for is
    // the colony where this sum is *zero*: every source posted, the
    // surplus eaten whole by the standing row, and nothing left in the
    // fleet that may take a delivery. The pioneers sit outside that `max`
    // for the reason recorded above them.
    let workerRow =
        (unpostedSeats + incomeWorkers |> max (workerFloor tasks)) + pioneers

    List.length reserverClaims
    + anchorQuota
    + haulerQuota
    + upgraderQuota
    + workerRow
    |> max minWorkforce

/// Whether a living body was cast from the hauler row: Carry parts but no
/// Work. The worker and anchor rows both keep at least one Work, and only
/// the hauler row casts none (ADR 0012) — so, like the anchor's
/// Work > Move, the casting pattern is readable off the body itself; the
/// row name in a creep's name stays observability only.
let private isHaulerBody (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    count Work = 0 && count Carry > 0

/// Whether a living body was cast from the reserver row: it carries a
/// CLAIM part. The one part no other row buys (ADR 0042), so it identifies
/// the row on its own and is asked before the other two — a body holding
/// one is out reserving a controller whatever else it is made of, and the
/// two comparative tests below would read a [Claim; Carry; Move] body as a
/// hauler on the strength of a part the reserver merely happens to have.
let private isReserverBody (creep: CreepInfo) =
    creep.Body |> Map.tryFind BodyPart.Claim |> Option.exists (fun n -> n > 0)

/// Whether a living body can take energy out of a store and put it into an
/// extension — the one capability the bank's own refilling depends on, and
/// therefore the one every capacity-sized row depends on (`planSpawns`'s
/// supply floor, ADR 0050).
///
/// Not "has a Carry part". It is the body half of `Refill`'s gate and the
/// body half of `Withdraw`'s read back together, because a body that can
/// deliver but never draw cannot reach the storage the energy is standing
/// in: a Carry part (both gates), no standing-body ratio (ADR 0046 — a
/// delivery by a body holding eleven Work is a commute) and no more Work
/// than Move (ADR 0016 — a heavy body's intake is digging). `Refill`'s
/// third conjunct, `Energy > 0`, is deliberately *not* read: that is a
/// state a hauler passes through twice a trip, not a fact about the body.
///
/// The anchor row's `6W/1C/1M` is exactly why the gate is written this way
/// (#203): it holds a Carry and a Move and answers neither of the other
/// two clauses, so a colony of Anchors reads as full of carriers under
/// "has a Carry part" while no body in it can put a single energy into an
/// extension. That reading is the deadlock the supply floor exists to
/// break, so the floor may not be built on it.
let private canRefill atlas (creep: CreepInfo) =
    (creep.Body |> Map.tryFind Carry |> Option.exists (fun n -> n > 0))
    && not (isStandingBody creep)
    && not (Atlas.workHeavy atlas creep.Name)

/// The pattern row a living body was cast from, read off the parts alone
/// (ADR 0006): a CLAIM part is the reserver row, more Work than Move is
/// the anchor row, a standing body at or under that line is the upgrader
/// row, no Work beside a Carry is the hauler row, and every other body is
/// the generalist. The row is what sizes the replacement a lead prices
/// (ADR 0026), so the one rule serves every row and none of them needs a
/// constant of its own.
///
/// The upgrader arm is written as ADR 0046 states the rule — `Work ≤ Move`
/// **and** a standing body — and reaches here holding half of it already:
/// the anchor arm above is exactly `Work > Move` (`Atlas.workHeavy`), so
/// everything past it is at or under the line and the arm tests only what
/// is left. Order matters between those two and nowhere else in this
/// chain: `6W/1C/1M` satisfies both descriptions, and it is the anchor row
/// that casts it — a body pinned to a Post by ADR 0020's Work Area, which
/// is a stronger claim on it than standing beside the buffer is. Against
/// the hauler arm below the two are disjoint whatever the order: a
/// Work-less body is never standing.
///
/// The reserver arm is what keeps ADR 0026 honest for a CLAIM body (ADR
/// 0042): `[Claim; Move]` has neither Work nor Carry, so before it existed
/// a reserver fell through to the generalist row and had its lead priced
/// off a worker unit's cast time and a worker unit's fatigue factor — the
/// wrong body on both counts, and wrong in the expensive direction, since
/// the worker row sized to the live RCL5 bank of 1,800 is nine whole
/// units — twenty-seven parts, no remainder to pad — against the
/// reserver's four. The rule is written where the body is read and not
/// where it is cast, which is what lets the same arm price a reserver the
/// colony cast under an older rule as readily as one it cast this tick.
let private patternOf atlas (creep: CreepInfo) =
    if isReserverBody creep then reserverPattern
    elif Atlas.workHeavy atlas creep.Name then anchorPattern
    elif isStandingBody creep then upgraderPattern
    elif isHaulerBody creep then haulerPattern
    else workerPattern

/// A creep's lead (ADR 0026): the ticks its replacement needs to stand
/// where it stands — the successor body's cast time plus that body's walk
/// out of the spawn, priced for the successor's own fatigue factor and not
/// the incumbent's. The body is the creep's own row at the bank's
/// capacity, so a slow Anchor earns a long lead and a hauler on a trunk a
/// short one. The walk starts beside the spawner rather than on it, where
/// the engine actually places the finished creep: a lead that charged the
/// step out of the spawner's tile would cast the successor that much too
/// early and leave it reading the incumbent's Post as full for the first
/// ticks of its life — the mispricing ADR 0026 names NoneFree as the
/// symptom of. Several spawns resolve as the hauler quota resolves them,
/// at the cheapest: the shortest lead is the optimistic bound on when a
/// replacement could stand there, and the row's quota is already read that
/// way — which spawn the colony actually casts from is the spawn fold's
/// own business. Geometry that prices nothing leads nobody (ADR 0004) — a
/// creep the projection cannot place, a colony whose spawns it cannot
/// place, and a tile no spawn can reach each answer 0, and a lead of 0
/// leaves every living creep counted.
///
/// Read wherever the creep stands, home or an outpost (#153). The tile
/// carries the room it is in (`Atlas.creepTile`, ADR 0052 decision 2) and
/// is never read out of a query that has already picked a room — and
/// the walk from `Atlas.castWalkTicks`, which floods home for the near leg
/// and joins over the Seam band for a goal beyond it. One row, one rule,
/// both sides of a border: an outpost's Post hires its Anchor off this row
/// (ADR 0042) and a reserver's whole working life is the far side of a
/// Seam, so a lead that could price only home tiles left ADR 0026's
/// succession switched off for exactly the creeps whose replacement has
/// the furthest to walk — the outpost Anchor read as never expiring, its
/// successor cast the tick *after* it died, and its Post unmanned for the
/// cast plus the crossing every 1,500 ticks while the workforce target
/// went on hiring against its nominal output.
///
/// The totality above gains one more absence and no new rule: a creep
/// whose room shares no priceable crossing with home answers 0 too, exactly
/// as a tile no spawn can reach does (ADR 0004).
let private leadOf (view: ColonyView) atlas (creep: CreepInfo) : int =
    let pattern = patternOf atlas creep

    match Atlas.creepTile atlas creep.Name with
    | None -> 0
    | Some tile ->
        view.Spawns
        |> List.choose (fun s ->
            match Atlas.positionOf atlas s.Id with
            | None -> None
            | Some spawnPos ->
                // The colony's one bank, whatever room the spawn is
                // filed under: every spawn a colony casts from stands in
                // its home room (ADR 0052 decision 1), so the capacity a
                // replacement would be cast at is the same number for all
                // of them.
                let body = bodyFor pattern view.Bank.Capacity

                Atlas.castWalkTicks atlas body (RoomPos.pos spawnPos) tile
                |> Option.map (fun walk -> spawnTicksPerPart * List.length body + walk))
        |> function
            | [] -> 0
            | leads -> List.min leads

/// Whether a creep is expiring (ADR 0026): its remaining life is at or
/// under its lead, so it will be dead before a replacement cast now could
/// stand where it stands. It leaves the workforce's living count and its
/// row's gap, which is what casts the successor while it still works. It
/// is never released for it — anti-thrash keeps it on its Task to the last
/// tick, and the Post the two share for the lead's duration is the
/// succession, not an oversell.
let private expiring (view: ColonyView) atlas (creep: CreepInfo) =
    creep.TicksToLive <= leadOf view atlas creep

/// The spawn Intents the Workforce target's rows are owed. The target is
/// the quota the *generalist* row is hired against; every other row is
/// hired against its own unfilled quota and can carry the fleet past the
/// target (#154). Spawning is a colony-level need, not a Task creeps get
/// matched to, so it sits beside the Planner/Matcher pipeline rather than
/// inside it.
///
/// It reads the tick's Task pool all the same, for one number: the worker
/// row's floor is "two while anything stands in the Build or Repair pool"
/// (ADR 0046, `workerFloor`), and the pool is the only honest reading of
/// that — a site the Planner withheld is work no body it hired could take.
/// The step is derived from the pool and never feeds it, so it still runs
/// before the Matcher and the order of the two in `decide` is the pool's
/// alone.
let private planSpawns
    (view: ColonyView)
    atlas
    (threats: Threats)
    (tasks: Task list)
    (haulerQuota: int)
    : Intent list =
    // The spawn holds while its doorstep is hot (ADR 0033): a creep born
    // into a Reach is a kill delivered, so no spawn casts anything while
    // any tile beside any spawn lies in one — the disaster fallback below
    // included, since an empty colony's first creep is the one that can
    // least afford to be born under fire. The doorstep is read against the
    // Reach of the spawn's own room (#138): a Threat on the neighbouring
    // coordinate of another room is a room away from the birth tile.
    let doorstepInReach (s: SpawnInfo) =
        match Atlas.targetRoom atlas s.Id, Atlas.positionOf atlas s.Id with
        | Some room, Some pos ->
            let reach = Threats.reachIn threats room

            [
                for x in pos.X - 1 .. pos.X + 1 do
                    for y in pos.Y - 1 .. pos.Y + 1 -> { X = x; Y = y }
            ]
            |> List.exists (fun tile -> Set.contains tile reach)
        | _ -> false

    // Asked before anything is priced, the way the reflexes ask their
    // hostiles first: a held tick derives no Workforce target and floods
    // no lead.
    if view.Spawns |> List.exists doorstepInReach then
        []
    else

        // The specialist rows' quota rules (ADR 0006, ADR 0012): one Anchor
        // per Post, haulers per the throughput arithmetic — the hauler quota
        // arrives memoised on the census signature (ADR 0017). That
        // signature signs the *union* of what the Layout and the quota read,
        // and since #149 neither input set contains the other: the quota
        // folds every projected room's containers and reads every projected
        // room's held rate (ADR 0042), where the Layout reads the home
        // room's census, level and name. ADR 0017's stated basis — the
        // quota's inputs "a subset of the Layout's" — is what stopped
        // holding, and the memo never rested on it: unchanged, the signature
        // still proves both identical; moved, it may have moved for only one
        // of them. Both quotas are addends of the target itself — inside it
        // by construction, never on top of it.
        //
        // One Anchor per Post of *every* projected room (ADR 0042): an
        // outpost's Post is the same garrison tile a home Post is, so it
        // hires from the same row rather than from a remote-miner row of
        // its own — the body is sized the same way, and travel cost pins
        // each Anchor on the Post nearest it exactly as it pins the home
        // ones. The container **or its site** makes the Post (#205), so
        // the row grows the tick the plan drops the site back and the body
        // it hires is the one that raises the container — an outpost with
        // nothing on any Seat still adds nothing here. What the container
        // standing changes is the other half of the census: the haul term
        // and the income share, which wait for it (`Atlas.standingPostsOf`,
        // `isPosted`).
        let anchorQuota = Atlas.postCount atlas

        // The reserver row's quota and its body in one value (ADR 0042):
        // one entry per declared outpost, each entry that outpost's CLAIM
        // demand, and the largest of them is what every cast this tick
        // carries. Read here beside the other two rows'
        // quotas because it is an addend of the same target — the row's
        // bodies are creeps, and a fleet counting them as generalists would
        // hire an upgrade mouth fewer for every reserver in the room.
        let reserverClaims = reserverClaimsOf view atlas

        // The anchor row's ceiling this tick, read once beside the quotas
        // and for the same reason the reserver's demand list is (ADR
        // 0042): the row's body is what the amortization is charged and
        // what the cast below buys, and the two must be the same body.
        let anchorWorkCap = anchorWorkCapOf view atlas

        // The income the two upgrade rows are hired out of, once (ADR
        // 0046): the standing row's quota is derived from it and the
        // commuting row's is derived from what that quota leaves, so the
        // two must read one number and not two spellings of it.
        let surplus =
            surplusOverLifetime view atlas reserverClaims anchorWorkCap anchorQuota haulerQuota

        // The upgrader row's quota (ADR 0046), read here beside the other
        // rows' for the same reason: it is an addend of the target below
        // and a gap of its own in the cascade, and a body hired for one
        // and not counted in the other would be an oversell every tick.
        let upgraderQuota = upgraderQuota view atlas surplus

        let target =
            workforceTarget
                view
                atlas
                tasks
                reserverClaims
                anchorQuota
                haulerQuota
                upgraderQuota
                surplus

        // The deficit and every row gap count the creeps that will still be
        // alive when a replacement could arrive: an expiring creep is already
        // outside the count (ADR 0026), so its successor is cast while it
        // still works rather than after it dies. The disaster fallback below
        // still reads the creep list itself — an expiring creep can refill an
        // extension, and a colony holding one is not the empty one.
        let living =
            view.Creeps |> List.filter (fun creep -> not (expiring view atlas creep))

        let deficit = target - List.length living

        // A body is sized to the bank's capacity and cast the tick the bank
        // holds its cost (ADR 0021) — a full bank for rows priced at
        // capacity, sooner for the capped Anchor row. Disaster fallback: an
        // empty colony can never refill extensions, so a capacity-sized body
        // would wait forever — spawn a minimal worker unit from whatever is
        // banked right now; time-to-first-creep outranks specialisation, so
        // the anchor gap waits (ADR 0006).
        //
        // The row's sizing rule arrives as a function of the bank rather
        // than being looked up from the pattern, which is the choice ADR
        // 0042's reserver row forces: two rows are the bank's answer alone
        // and `bodyFor` is exactly that, but the reserver's body is
        // `min(reservation deficit, bank)` and the anchor's is capped by
        // `anchorWorkCapOf`'s reading of the posted set — a fact about the
        // **room being reserved** and a fact about a **set of sources**,
        // neither of them about the row. A sizing member on `BodyPattern`
        // — ADR 0006's other shape — would have had nowhere to read either
        // from, so the casting step takes an already-decided sizing instead
        // and each caller supplies the rule its row is written in.
        //
        // The whole bank and not its capacity, because which of the
        // two numbers a row prices at is part of that row's rule and was
        // the invisible half of #203's deadlock: every row here but the
        // supply floor sizes at `Capacity` and is therefore unbuyable until
        // the extensions are full, which is precisely the state a colony
        // with nothing that can refill an extension can never leave. Each
        // call site now says which number it reads, in the one place the
        // reader is asking.
        let castFromBank pattern (sizing: RoomEnergy -> BodyPart list) (bank: RoomEnergy) =
            if List.isEmpty view.Creeps then
                if bank.Available >= bodyCost workerPattern.Block then
                    Some(workerPattern, workerPattern.Block)
                else
                    None
            else
                let body = sizing bank

                if bank.Available >= bodyCost body then
                    Some(pattern, body)
                else
                    None

        // Reserver gaps are filled before Anchor gaps, Anchor gaps before
        // hauler gaps, hauler gaps before upgrader gaps and those before
        // generalist gaps — the casting order runs reserver, Anchor,
        // hauler, upgrader, worker (ADR 0046) — and the worker row's quota
        // is whatever the target has left.
        //
        // The reserver goes in front of all four (ADR 0042): the other
        // rows spend income, and this one decides whether the income is
        // five a tick or ten across every source of an outpost at once.
        // Being first it is asked first, and it no longer *holds* the
        // cascade the tick the bank cannot pay for it: a row the bank
        // cannot afford yields the tick to the rows below it (ADR 0050,
        // #203). Priority here is the order the rows are asked in, not a
        // veto the head row holds over the ones behind it — a distinction
        // that costs nothing while the head is affordable and was a
        // 1,235-tick standstill the once it was not.
        //
        // Each specialist gap is that row's own unfilled quota, and it is
        // answered on its own terms rather than out of the deficit (#154):
        // an empty Post is a fact about the ground, and the row that hires
        // for it does not stop hiring because the headcount overshot some
        // other row's arithmetic. A target can fall under the living count
        // in one tick — a container demolished, an RCL step, or the
        // outpost tick whose lost vision unposts a source and withdraws
        // its Anchor place, its haul and its income share from the target
        // together (ADR 0042, ADR 0004) — and a deficit gate over the
        // whole cascade would then cast *nothing*, in the home room
        // included, until ordinary deaths had paid off the entire
        // overshoot. The gaps below are each floored at zero, so a row
        // standing over its quota still hires nobody; only a row genuinely
        // short of its own quota gets a body.
        //
        // Bodies and not rooms (#130): the row's quota counts CLAIM bodies
        // against the number of declared outposts, and which controller each
        // ends up holding is the Reserve Task's one-holder-per-controller
        // capacity. Counting living reservers per room instead would recast
        // for a room every tick of the fifty its first reserver spends
        // walking there.
        let reserverGap =
            List.length reserverClaims
            - (living |> List.filter isReserverBody |> List.length)
            |> max 0

        let anchorGap =
            anchorQuota
            - (living
               |> List.filter (fun creep -> Atlas.workHeavy atlas creep.Name)
               |> List.length)
            |> max 0

        let haulerGap =
            haulerQuota - (living |> List.filter isHaulerBody |> List.length) |> max 0

        // Bodies and not names (ADR 0006): the row's living count is what
        // `patternOf` reads back off the parts — a standing body at or
        // under ADR 0016's line — so a `11W/1C/11M` the colony inherited,
        // resized or was handed fills this quota exactly as one it cast
        // does. Asking `patternOf` rather than `isStandingBody` alone is
        // what keeps the Anchor row out of it: `6W/1C/1M` answers to both
        // descriptions and it is the anchor arm that claims it, so an
        // Anchor standing at its Post never pays off an upgrader's gap.
        //
        // A count read this way can only close a gap the quota hires at a
        // bank whose cast the same ratio reads back to this row, which is
        // why `upgraderQuota` is gated on exactly that (`isStandingCast`):
        // the two readings are one rule seen from either end, and where
        // they part the row would hire against a gap no body it cast could
        // ever pay off.
        let upgraderGap =
            upgraderQuota
            - (living
               |> List.filter (fun creep -> patternOf atlas creep = upgraderPattern)
               |> List.length)
            |> max 0

        // The supply floor (ADR 0050, #203), and the one row that is not a
        // quota: a colony holding no body that can put energy into an
        // extension hires one hauler in front of every row, sized from what
        // is banked **right now**.
        //
        // It is the disaster fallback's own argument (ADR 0006) carried to
        // the state that fallback cannot see. Every row below it prices
        // its body at `bank.Capacity`, so every one of them is unbuyable until
        // the extensions are full — and the extensions are filled by
        // creeps. With nothing alive that can refill one, the bank is a
        // fixed point rather than a slope: the engine's spawn regeneration
        // only runs while the room holds under 300, so a colony stranded
        // above that line does not even trickle. #203's was 361 against an
        // 1,800 capacity, two Anchors on full containers, 246,818 energy in
        // the storage and 1,235 ticks without a single cast.
        //
        // Read off `view.Creeps` and never `living` for the same reason
        // the fallback is: an expiring hauler can still refill an extension,
        // and a colony holding one is not the stranded one.
        //
        // The gate is `canRefill` and emphatically not "has a Carry part" —
        // an Anchor has one, and answering the gate with it is how the
        // deadlock reproduces itself with this row in place.
        let supplyFloor =
            if view.Creeps |> List.exists (canRefill atlas) then
                0
            else
                1

        // The rows expanded into the seats they are owed, in casting order:
        // the supply floor, then reserver, Anchor, hauler, upgrader (ADR
        // 0042, ADR 0046) and last the generalist, whose seats are whatever
        // the whole-fleet deficit has left once every row above is counted.
        //
        // The deficit gates the *worker* row alone and is spent here rather
        // than over the whole cascade. It stands in for that row's own gap
        // rather than being it: ADR 0012 hires the row against whatever the
        // target has left over once the specialist rows are counted, and the
        // whole-fleet gap less the rows above is exactly that remainder
        // while every specialist row is at or under quota. A row standing
        // over its quota holds the worker row down by the surplus instead —
        // the half of the old gate #154 keeps, pinned by `rowGapTests`.
        //
        // Seats and not an index into the cascade (ADR 0050): a spawn takes
        // the first seat its bank can pay for and the seat it filled is the
        // one that leaves the list, so a row the bank cannot afford is
        // stepped over instead of stopping the tick — and stepping over it
        // cannot make the next spawn buy that row's neighbour twice. The
        // counting the old `planned < gap` chain did is the same counting
        // while every seat in front is affordable, which is every tick that
        // is not #203's.
        //
        // Nothing here is a permanent skip: the seats are rebuilt from the
        // quotas every tick, so the reserver the extensions could not pay
        // for this tick is the head of the cascade again the tick they can.
        let seats =
            List.replicate
                supplyFloor
                // The one row sized from `Available` (with the disaster
                // fallback inside `castFromBank`, for the same reason).
                // Sizing it at capacity would buy the 1,800 body that is
                // exactly what the colony cannot pay for; the hauler row's
                // own rule floors at one `[Carry;Carry;Move]` block, so
                // 150 banked is enough and `castFromBank`'s own
                // affordability check still refuses to cast what is not
                // there.
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Available))
            @ List.replicate
                reserverGap
                // Every cast at the largest outstanding demand and never at
                // the one standing beside it in the list: the Matcher pairs
                // a finished body to a controller by travel cost, so a body
                // sized for the room that has slipped furthest can land on
                // the room that has not (`reserverClaimsOf`).
                // A positive gap is a non-empty demand list, so the
                // `List.max` is total inside this sizing and nowhere else —
                // and it is inside it, because `List.replicate 0` still
                // evaluates the element it is not replicating.
                (castFromBank reserverPattern (fun bank ->
                    reserverBodyWithin (List.max reserverClaims) bank.Capacity))
            @ List.replicate
                anchorGap
                // Sized under this tick's ceiling and never the held
                // constant (ADR 0042): which Post the finished body lands
                // on is the Matcher's, so the cast carries the richest
                // posted source's saturation (`anchorWorkCapOf`).
                (castFromBank anchorPattern (fun bank -> anchorBodyFor anchorWorkCap bank.Capacity))
            @ List.replicate
                haulerGap
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Capacity))
            @ List.replicate
                upgraderGap
                // Ahead of the generalist and behind the three rows hired
                // off the ground (ADR 0046): the upgrader spends the surplus
                // those three produce, so it is cast once they stand, and it
                // spends it at eleven Work against the generalist's nine for
                // the same bank — the gain the row exists for is lost every
                // tick a worker is cast into the surplus instead.
                (castFromBank upgraderPattern (fun bank -> bodyFor upgraderPattern bank.Capacity))
            @ List.replicate
                (deficit - (supplyFloor + reserverGap + anchorGap + haulerGap + upgraderGap)
                 |> max 0)
                (castFromBank workerPattern (fun bank -> bodyFor workerPattern bank.Capacity))

        // Idle spawns draw from the colony's one bank in list order — each
        // body debits the budget the next spawn sees, so the same energy is
        // never committed twice.
        //
        // One bank and no longer a map keyed by the spawn's room (ADR 0052
        // decision 1): every spawn a colony casts from stands in its home
        // room, so the map had one entry and the lookup could only ever
        // answer with it — or, for a spawn the projection filed under
        // another room, with a zero bank that silently cast nothing.
        let intents, _, _ =
            view.Spawns
            |> List.filter (fun s -> not s.IsSpawning)
            |> List.fold
                (fun
                    (intents,
                     bank: RoomEnergy,
                     unfilled: (RoomEnergy -> (BodyPattern * BodyPart list) option) list)
                    s ->
                    // The first seat this bank can pay for, and the rest of
                    // the list with exactly that seat taken out of it.
                    let rec take passed remaining =
                        match remaining with
                        | [] -> None
                        | cast :: rest ->
                            match cast bank with
                            | Some filled -> Some(filled, List.rev passed @ rest)
                            | None -> take (cast :: passed) rest

                    match take [] unfilled with
                    | Some((pattern, body), left) ->
                        SpawnCreep(s.Name, body, $"{pattern.Name}-{view.Time}-{s.Name}") :: intents,
                        { bank with
                            Available = bank.Available - bodyCost body
                        },
                        left
                    | None -> intents, bank, unfilled)
                ([], view.Bank, seats)

        List.rev intents

/// The claimer range at which safe mode fires (ADR 0015): the precise
/// deadline is 2 — attackController is range 1 and judged from
/// tick-start position, and a creep steps at most one tile a tick, so
/// activating at 2 always lands before the tap — plus one tile of
/// margin for a skipped tick.
let private safeModeDeadline = 3

/// The hostiles standing in the colony's own room, which is the whole of
/// what the two reflexes below may read (#201). Since `ColonyView.Hostiles`
/// stopped being the spawn rooms' alone, "a hostile" and "a hostile here"
/// are two different questions, and both reflexes ask the second one: safe
/// mode protects a controller of ours and an outpost has none to protect
/// (ADR 0007), and a tower's shot is a range act inside its own room, so
/// aiming one across a border is an Intent the engine can only refuse (ADR
/// 0014). Everything above them — Reach, Flee, the spawn hold — reads the
/// list whole and files each hostile under its own room instead (ADR
/// 0033, #138); this is the narrowing, stated once, for the two rules that
/// are about a room we own rather than about a creep of ours being shot.
///
/// The home name and not the controller's or a tower's room, because both
/// arms need an answer on a tick the projection places neither: ADR 0004's
/// absence would otherwise widen the reflex back to every room at exactly
/// the moment there is least to read.
let private hostilesAtHome (view: ColonyView) : HostileInfo list =
    let home = SpatialInfo.homeName view.Spatial
    view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

/// Colony reflex beside the pipeline, two arms and one pair of gates —
/// stock remaining, safe mode not already running.
///
/// The CLAIM arm: a CLAIM-part hostile is the one threat that can disarm
/// safe mode itself — attackController blocks activation for 1,000 ticks.
/// But the tap is a range-1 act, so the activation holds until a claimer
/// stands within reach of landing it (ADR 0015) — the hold is free
/// (activation still wins the race) and buys the towers their window to
/// kill the claimer en route. A controller the projection cannot place has
/// no deadline to measure and falls back to firing on sight.
///
/// The Keep arm (ADR 0034): any Keep structure below full hits while any
/// hostile stands in the home room (`hostilesAtHome`, #201 — the ADR wrote
/// "the spawn room", back when the sweep behind the list could name no
/// other). The same shape — hold until the harm is certain — over the
/// other half of the exposure. Any hostile, not only
/// a Threat: a WORK-only dismantler hurts a structure without ever
/// qualifying as one, and "the Keep is losing hits with someone here" is
/// the honest reading whoever is doing it. Stateless on purpose: one
/// tick's hits, never a comparison against the last tick's (ADR 0012,
/// 0017), which is what makes Repair's full-hits line on the Keep part of
/// this reflex — a Keep left dented would keep the arm armed for every
/// hostile that wandered through afterwards.
///
/// A hostile that neither claims nor damages spends nothing: at RCL2 safe
/// mode outlasts any invader raid 13×, so the stock keeps for when the
/// room is actually being taken (ADR 0007).
let private planSafeMode (view: ColonyView) atlas : Intent list =
    match view.Controller with
    | Some controller when controller.SafeModeAvailable > 0 && not controller.SafeModeActive ->
        // The colony's own room and no other (`hostilesAtHome`, #201): a
        // claimer in an outpost is tapping a controller safe mode does not
        // cover, and the Keep it could be denting is not in that room at
        // all.
        let here = hostilesAtHome view

        let withinReach (h: HostileInfo) =
            List.contains BodyPart.Claim h.Body
            && match Atlas.positionOf atlas controller.Id with
               // Across a border there is no range to measure (ADR 0052
               // decision 2), and a claimer in another room is tapping a
               // controller this safe mode does not cover — so None here
               // is "not in reach", where an unplaced controller below is
               // still "fire on sight".
               | Some tile ->
                   RoomPos.range h.Pos tile |> Option.exists (fun r -> r <= safeModeDeadline)
               | None -> true

        let claimerInReach = here |> List.exists withinReach

        // Below full hits, off the walk the Repair pool reads: the Keep's
        // whole line is Full, so "hungry" and "damaged" are one fact and
        // the two readers cannot drift apart. The Posts and the ramparts
        // are hungry on their own lines and are not of the Keep — neither
        // a container's hits nor a rampart's ever spend the stock. The
        // hostiles are asked first, so a quiet room walks nothing.
        let keepDamaged =
            not (List.isEmpty here) && hungryStructures view |> List.exists (snd >> isKeep)

        // The undefended arm (#217, ADR 0034 as it amends it): a colony
        // with no tower standing fires on the first armed hostile in its
        // room. The two arms above were derived for a home with a tower —
        // the tower handles creeps, safe mode is for the Keep — and a
        // bootstrapping colony has nothing that can hurt a hostile, so
        // for it the one thing safe mode protects is the colony's ability
        // to exist: W13S28 at RCL2 lost four workers to one invader over
        // 217 ticks while its Keep stood at full hits and its one safe
        // mode sat unused. Armed means an ATTACK or RANGED_ATTACK part; a
        // scout or a claimer spends nothing here (the claimer has its own
        // arm). The tower is read off the same census the tower reflex
        // fires from (`Atlas.placedTowers`), so the tick a tower stands
        // this arm stops and the Keep arm is the whole of the rule again.
        let undefended =
            List.isEmpty (Atlas.placedTowers atlas)
            && here
               |> List.exists (fun h ->
                   List.contains BodyPart.Attack h.Body
                   || List.contains BodyPart.RangedAttack h.Body)

        if claimerInReach || keepDamaged || undefended then
            [ ActivateSafeMode controller.Id ]
        else
            []
    | _ -> []

/// Colony reflex beside the pipeline (ADR 0014): every tower shoots the
/// hostile nearest to itself, every tick one stands in the room. Attack
/// only — no tower repair or heal — per-tower nearest with no focus fire
/// or anti-drain gate, and no energy gate: unlike safe mode there is no
/// stock to protect, so a dry tower's Intent fails harmlessly. Equal
/// ranges tie-break by hostile id, so the pick is deterministic.
///
/// Both halves of the pairing are the colony's own room's: `placedTowers`
/// has always answered home alone — a tower stands only in a room we own
/// — and the hostiles are narrowed to match (`hostilesAtHome`, #201). That
/// narrowing is the reflex's own rule and not a repair for a missing join:
/// a tower shoots inside its own room (ADR 0014), so a raider in an
/// outpost is not a target it declines to reach but a target it does not
/// have. Since #216 R3 the join is the type as well — `RoomPos.range`
/// answers None across a border rather than the coordinate distance that
/// used to read as nearest — so the narrowing and the measurement now
/// agree by construction instead of by whoever remembered.
let private planFire (view: ColonyView) atlas : Intent list =
    match hostilesAtHome view with
    | [] -> []
    | hostiles ->
        Atlas.placedTowers atlas
        |> List.choose (fun (towerId, tile) ->
            hostiles
            |> List.choose (fun h -> RoomPos.range tile h.Pos |> Option.map (fun r -> r, h))
            |> function
                | [] -> None
                | reachable ->
                    let _, target = reachable |> List.minBy (fun (r, h) -> r, h.Id)
                    Some(FireTower(towerId, target.Id)))

/// Extensions the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "extension").
let private extensionAllowance level =
    match level with
    | 0
    | 1 -> 0
    | 2 -> 5
    | 3 -> 10
    | 4 -> 20
    | 5 -> 30
    | 6 -> 40
    | 7 -> 50
    | _ -> 60

/// Towers the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "tower").
let private towerAllowance level =
    match level with
    | 0
    | 1
    | 2 -> 0
    | 3
    | 4 -> 1
    | 5
    | 6 -> 2
    | 7 -> 3
    | _ -> 6

/// Storages the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "storage").
let private storageAllowance level =
    match level with
    | 0
    | 1
    | 2
    | 3 -> 0
    | _ -> 1

/// The level the engine unlocks the Storage at. The Layout reserves the
/// Storage's whole allowance here rather than at the horizon (ADR 0022):
/// the Storage is not a clustered kind, and its tile never comes back
/// once an extension takes it, so the reservation must outlive any
/// revisit of the horizon.
let private storageLevel = 4

/// Whether the Layout places **road sites** at all this tick (ADR 0011 as
/// #209 amends it): only for an `Independent` colony. Not an engine
/// unlock — the engine allows a road at RCL1 — but the stage below which
/// a road is the wrong spend: the trunk set a
/// bootstrapping room plans is thousands of energy of income placed in one
/// tick, in the same surplus tier as the Upgrade and nearer to hand than
/// the controller (the Matcher orders inside a tier by [[travel cost]]
/// alone), so every worker builds roads and nobody upgrades. W13S28 at
/// RCL1 planned some 19,000 energy of it against 8 a tick — around 2,400
/// ticks of the colony's entire income, spent ahead of the 200 progress
/// that unlocks five extensions and doubles the body.
///
/// This narrows ADR 0010 and does not contradict it. A road is worth
/// exactly what that ADR prices it at, to this body too: `1W/2C/2M`
/// carrying energy generates 3 fatigue a tile on road against 6 on plain
/// and recovers 4, so the loaded commute is one tile a tick paved and one
/// tile per two ticks bare — the half speed ADR 0010 was written about.
/// What #209 says is not that the body cannot feel it but that half a tick
/// a loaded step, on a room whose whole income is 8 a tick, is not worth
/// 2,400 ticks of that income when the same energy buys the level that
/// doubles the body outright.
///
/// It is the [[stage]] and not a level of its own, because it is the same
/// question: ADR 0047 chose RCL3 as the line a colony can feed and defend
/// itself at — the tenth extension and the first tower, 800 energy of
/// bank, a body that is not the 300-energy starter — and ADR 0052
/// decision 3 made that line a stage every rule reads. Below it the colony
/// is still buying its own economy; at it, roads are what the economy is
/// for. One derivation, so this gate and the rampart gate below it cannot
/// drift apart.
///
/// This gates the **placement** and never the plan: the trunks are routed
/// whole every tick regardless of stage (ADR 0011's "computed whole"), so
/// they still route around tomorrow's reserved tiles and a Link footing
/// still dodges them.
let private placesRoads (view: ColonyView) = isIndependent view

/// The Layout horizon (ADR 0011, moved to RCL5 by ADR 0039): the whole
/// plan is computed up to this level regardless of the current one, so
/// today's roads route around tomorrow's structures. One level of
/// lookahead is the standing bargain — RCL8 would tax today's trunks
/// with detours for structures four levels away, and a horizon the room
/// has already passed sizes every clustered gap at zero, so the room
/// stops growing without saying why. Declared and not computed from the
/// current level, which is what keeps it stepping once, in a commit
/// (ADR 0039).
let private horizonLevel = 5

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR 0011), computed whole from the Atlas every
/// tick and placed all at once — no persisted plan, no pacing. One
/// ordering rule eats every clustered structure: buildable tiles on the
/// spawn's checkerboard colour, nearest-to-spawn first, the working
/// ground excluded, the Storage's pick coming before the tower's and both
/// before the extensions' (ADR 0022). Trunk roads pave each source to the
/// controller and to each spawn plus the swamps of the controller's Work
/// Area, priced on raw terrain and routed around every reserved tile —
/// reservations come first, so a road never sits where a structure will.
/// One tile beside each container pick and beside the Storage is held as
/// a Link footing (ADR 0022) and outranks the tower and the extensions;
/// the reservation is widened by the footing count so the tiles they push
/// the cluster onto are reserved too (ADR 0027). No link is ever placed
/// on one — Link is a built kind only, and this room fills none of them
/// (ADR 0038).
/// Placement filters the Layout to what the current level unlocks and
/// what the projection's censuses say is missing. Sites are not creep
/// work, so this emits Intents directly rather than Tasks.
/// Beside all of that runs one rule that reads no tile of the ordering: a
/// rampart covers every standing Keep structure and every standing Post
/// container (ADR 0034), from the level the engine allows one, because a
/// rampart is no footprint and takes no tile from anything.
/// Returned beside the Intents: the footings, served and unserved. A
/// target with no candidate reserves nothing, and the guarantee of ADR
/// 0022 and ADR 0027 — one footing per target — would otherwise degrade
/// with nothing anywhere to say so (#77). Not an Intent, because there is
/// nothing to place; not a Verdict, because a footing has no creep (ADR
/// 0035). The tiles it did reserve ride out for the same reason read the
/// other way (#106): a link is placed by no Intent, so a footing the fold
/// dropped from the accumulator would be a reservation no reader could
/// see — and the rule it was chosen by, off the trunks and off every
/// target and every other footing, could be asserted nowhere (ADR 0036).
/// Beside them, the container picks the plan deferred to a container that
/// already serves their target (ADR 0040): the room keeps the container it
/// has, and what the plan wanted instead is a colony fact no other channel
/// carries — nothing demolishes the orphan, so the difference is permanent.
let private planLayout
    (view: ColonyView)
    atlas
    : Intent list *
      ServedFooting list *
      UnservedFooting list *
      UnroutedTrunk list *
      DeferredContainer list
    =
    let home = Atlas.homeRoom atlas

    // The tile the whole plan is oriented on, and it is a tile of the
    // room being planned (ADR 0052 decision 2). A colony's spawns stand
    // in its home room (ADR 0047), but `view.Spawns` is a list and the
    // first entry's tile used to be read onto the home grid whatever room
    // the projection filed it under — which is #191 exactly: Spawn2
    // standing in the child room set the cluster's parity, the ordering's
    // distance and every trunk goal from a coordinate of another room.
    // The join is the type now, so the mismatch is a filter rather than an
    // assumption, and a colony whose only placed spawn is elsewhere plans
    // nothing, which is what a colony that cannot orient itself has always
    // done.
    let inHome (tile: RoomPos) = Some tile.Room = home

    let anchor =
        view.Spawns
        |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id |> Option.filter inHome)

    match home, anchor, view.Controller with
    | Some room, Some anchorTile, Some controller ->
        let spawnPos = RoomPos.pos anchorTile
        // Same checkerboard colour as the spawn: clustered structures sit on
        // the spawn's colour, leaving the other colour free for movement.
        let parity = (spawnPos.X + spawnPos.Y) % 2

        // The sources this plan is for: the home room's alone (ADR 0041).
        // `view.Sources` is every scanned room's since #124 — the
        // Harvest pool is one pool — while every use of a source below
        // joins a bare `Pos` to the *home* grid: a footing slot widens a
        // home reservation, a trunk is a home flood started from the
        // source's coordinate, and a container pick lands on a home tile.
        // An outpost source read here draws a trunk and plants a container
        // site out of another room's coordinates onto home ground, which
        // is the Layout ADR 0042 promises the outpost will never get ("The
        // outpost gets a container and nothing else. No roads, and no
        // Layout"). The room is resolved through the Atlas's own id join
        // (ADR 0041), as every other reader holding one resolves it.
        let homeSources =
            view.Sources |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some room)

        // The working ground — every source's Seats and the controller's
        // Upgrade Work Area — is off-limits (ADR 0022): a clustered
        // structure there eats a tile an Anchor or an upgrader stands on,
        // so a colony whose nearest same-colour tiles are working ground
        // clusters one ring out instead of eating them.
        let working = Atlas.workingGroundIn atlas room

        let buildable = Atlas.buildableTilesIn atlas room

        let ordering =
            buildable
            |> List.filter (fun tile ->
                (tile.X + tile.Y) % 2 = parity && not (Set.contains tile working))
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)

        // A kind's still-open gap at a level: its allowance there minus the
        // projection's censuses of standing and pending structures. Judged
        // at the level the kind is reserved for it sizes the reservation;
        // at the current level it sizes the placement.
        let gapAt allowanceOf built pending level =
            allowanceOf level - built - pending |> max 0

        // The room being planned, and no other (#140): the allowance is
        // this controller's, so what is subtracted from it is this room's
        // census — a neighbour's site counted here is a site this room
        // never places.
        let storageGap =
            gapAt
                storageAllowance
                (Atlas.builtStoragesIn atlas room)
                (Atlas.pendingStoragesIn atlas room)

        let towerGap =
            gapAt towerAllowance (Atlas.builtTowersIn atlas room) (Atlas.pendingTowersIn atlas room)

        let extensionGap =
            gapAt
                extensionAllowance
                (Atlas.builtExtensionsIn atlas room)
                (Atlas.pendingExtensionsIn atlas room)

        // The still-unclaimed slots, Storage first and tower next: a built
        // or pending structure keeps its tile out of the ordering (it is a
        // target) and its slot off the plan. The clustered kinds are sized
        // at the horizon; the Storage is not one of them and reads none
        // (ADR 0022) — its whole allowance is held from level 0, because
        // once an extension takes that tile it never comes back.
        let storageSlots = storageGap storageLevel
        let towerSlots = towerGap horizonLevel
        let extensionSlots = extensionGap horizonLevel

        // The Link footings cannot be named here — their targets are the
        // container picks, which are derived from the trunks the
        // reservation is for — but their count can: one per source, one
        // for the controller container, one for the Storage. The window is
        // widened by that many, so the tiles the cluster is pushed onto
        // when a footing takes one of its picks are inside the reservation
        // too (ADR 0027). Without the widening a pick lands past the
        // window on ground the flood never dodged, and the tick its site
        // stands the trunk reroutes around it.
        let footingSlots = List.length homeSources + 2

        let clustered =
            ordering
            |> List.truncate (storageSlots + towerSlots + extensionSlots + footingSlots)

        let storagePick = ordering |> List.truncate storageSlots

        // Reserved before trunks: a trunk never crosses a tile a reserved
        // structure will claim, and the widened window holds the footings
        // as well — so the precedence runs one way for every kind the
        // Layout places (ADR 0011). The footings' own tiles are settled
        // below, once the trunks are known.
        let reserved = Set.ofList clustered

        // The reservation as the router reads it, joined once: the trunks
        // ask for it per source per goal, and a room name added to every
        // tile of it at each of those asks is a census tick's worth of
        // rebuilding for an answer that does not move (#216 R3).
        let reservedTiles = RoomPos.setAt room reserved

        // This room's share of the controller's Upgrade Work Area: a
        // controller the projection files under another room contributes
        // nothing here, rather than its coordinates (ADR 0052 decision 2).
        let upgradeArea =
            Atlas.workArea atlas (Upgrade controller.Id) |> RoomPos.inRoom room

        // Each goal beside the name it is recorded under when a source
        // cannot reach it (#107). The Upgrade Work Area first and the
        // spawns after, which is the order the routes are collected in and
        // therefore the order a loss reads in.
        let trunkGoals =
            (TrunkGoal.UpgradeArea, RoomPos.setAt room upgradeArea)
            :: (view.Spawns
                |> List.choose (fun s ->
                    Atlas.positionOf atlas s.Id
                    |> Option.filter inHome
                    |> Option.map (fun spawn ->
                        TrunkGoal.Spawn s.Id,
                        Atlas.adjacentWalkableIn atlas room (RoomPos.pos spawn)
                        |> List.map (RoomPos.at room)
                        |> Set.ofList)))

        // Every route the Layout asks for, kept per source and per goal:
        // the union paves the roads and each source's own trunk anchors
        // its container (ADR 0012), while the goals stay apart for the
        // reason `TrunkGoal` is a type — the loss below is per goal.
        let sourceRoutes =
            homeSources
            |> List.sortBy (fun s -> s.Id)
            |> List.choose (fun s ->
                Atlas.positionOf atlas s.Id
                |> Option.filter inHome
                |> Option.map (fun sourcePos ->
                    s.Id,
                    trunkGoals
                    |> List.map (fun (goal, area) ->
                        goal,
                        Atlas.trunkPath atlas reservedTiles sourcePos area
                        |> List.map RoomPos.pos)))

        let sourceTrunks =
            sourceRoutes
            |> List.map (fun (id, routes) -> id, routes |> List.collect snd |> Set.ofList)

        // The empty path is the router's answer for a goal it paved nothing
        // for, and it unions into the road plan contributing nothing
        // (#107). Recorded here, where the source and the goal are both
        // still in scope: downstream there is only a set of tiles, and a
        // trunk that was dropped whole looks exactly like one that was
        // never asked for. The predicate is the paving and not the flood
        // on purpose — what the colony lost is the line, however the
        // geometry failed to draw it.
        let unroutedTrunks =
            sourceRoutes
            |> List.collect (fun (id, routes) ->
                routes
                |> List.choose (fun (goal, path) ->
                    if List.isEmpty path then
                        Some { Source = id; Goal = goal }
                    else
                        None))

        let trunkTiles = sourceTrunks |> List.map snd |> List.fold Set.union Set.empty

        // The controller's Work Area paves its swamps and only its swamps —
        // upgraders shuttle within it, so the dear ground gets a road and
        // the plain ground does not. No reservation can stand here: the
        // Work Area is working ground, which the ordering never offered
        // (ADR 0022).
        let workAreaSwamps = upgradeArea |> Set.filter (Atlas.isSwampIn atlas room)

        // Every tile the Layout paves: the trunks plus the Work Area's
        // swamps. The road gap measures this against the projection's road
        // census, and a Link footing is chosen off it.
        let roadPlan = Set.union trunkTiles workAreaSwamps

        // The road gap reads the projection's road census: a built road or a
        // pending road site already claims its tile (ADR 0010).
        let roadGap =
            Set.difference roadPlan (Atlas.roadTilesIn atlas room)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTilesIn atlas room)

        // The road sites this tick: the whole gap once the colony is
        // `Independent`, none before it (#209). The stage gate is a filter
        // on the placement and not on the plan — `roadPlan` and `roadGap`
        // above are computed at every stage, so the trunks still route
        // around the reservation and the Link footings still dodge the
        // pavement — and it is a gate rather than pacing, which ADR 0011
        // rejected and still rejects: a stateless planner has no memory to
        // pace with, and this needs none. It is the same shape the
        // clustered kinds already have (`storageGap level`, `towerGap
        // level`), one question coarser: the plan is whole either way, a
        // level says how much of it is placed and the stage says whether
        // any of it is.
        //
        // The container sites are subtracted with the roads' own census,
        // which is ADR 0040's tile clause read in the other direction. It
        // is the direction the gate opened: below the gate a source
        // container drops on an unpaved trunk tile (`owedRoad` below), and
        // the tick the room reaches RCL3 that tile is still in the road
        // gap — no road stands on it and no road site pends — so the
        // Layout would ask for a road on a tile already carrying a
        // container site and the engine would refuse it every tick until
        // the container finished. One construction site per tile is one
        // rule, and both kinds have to read it.
        let placedRoads =
            if placesRoads view then
                Set.difference roadGap (Atlas.pendingContainerTilesIn atlas room)
            else
                Set.empty

        // Containers (ADR 0012), computed whole like everything else and
        // RCL-gated by nothing — the engine allows them from level 0. Each
        // source's container sits on the Seat nearest that source's trunk;
        // the trunk's first tile is itself a Seat, so in practice the
        // container lands where the trunk leaves the source and harvest
        // overflow falls straight in. Seats are terrain geometry and
        // trunks avoid only the reservations, so the pick never shifts as
        // the container itself gets built. Seats need no reservation dodge:
        // they are working ground, which the clustered ordering never
        // offered (ADR 0022).
        //
        // The pick rides beside the source it is for: the target clause
        // below asks whether *that* source is served, and downstream there
        // is only a tile (ADR 0040).
        let sourceContainerPicks =
            sourceTrunks
            |> List.choose (fun (sourceId, trunk) ->
                let seats = Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room

                if Set.isEmpty trunk || Set.isEmpty seats then
                    None
                else
                    seats
                    |> Set.toList
                    |> List.minBy (fun seat ->
                        trunk |> Set.toList |> List.map (range seat) |> List.min, seat.X, seat.Y)
                    |> fun seat -> Some(sourceId, seat))

        let sourceContainerTiles = sourceContainerPicks |> List.map snd

        // The controller container: an Upgrade-Work-Area tile beside a
        // trunk and off the road itself — the buffer upgraders work from
        // standing still, one tile from where the haulers drive. No
        // reservation to dodge either: the Work Area is working ground
        // (ADR 0022). Judged from the same stable geometry as the
        // Seat pick, so a standing container recomputes to its own tile.
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.filter inHome
            |> Option.map RoomPos.pos
            |> Option.bind (fun controllerPos ->
                upgradeArea
                |> Set.filter (fun tile ->
                    not (Set.contains tile trunkTiles)
                    && not (Set.contains tile workAreaSwamps)
                    && trunkTiles |> Set.exists (fun t -> range tile t = 1))
                |> Set.toList
                |> function
                    | [] -> None
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile controllerPos, tile.X, tile.Y)
                        |> Some)

        // The Link footings (ADR 0022): one tile held for a link beside
        // every target a link will ever serve — each planned source
        // container, the controller container, and the Storage. Planned,
        // not built: a Post needs a standing container, so a Post-anchored
        // rule would reserve nothing at level 0 and the tiles would be
        // gone by the time links arrive. A room with three sources gets
        // five footings; the count is the rule's, never a constant
        // (ADR 0027).
        //
        // The tiles are settled here rather than with the reservation
        // because a footing's targets are the container picks, and those
        // are derived from the trunks the reservation is for. Only the
        // count could be held ahead of the trunks, and it was: a footing
        // still yields to them by being chosen off the finished trunk
        // plan, but the tiles it pushes the cluster onto were reserved.
        // Re-flooding the trunks to name the tiles first would pay the
        // tick's dearest step twice for the same answer (ADR 0017).
        //
        // One footing per target, and a tile is only ever one target: a
        // Seat that is also the controller container's pick would
        // otherwise be served twice and hold two tiles for one link.
        // Each target carries the kind it is, which the fold knows by
        // construction — the list is assembled from exactly the three the
        // guarantee names — so an unserved one below says which guarantee
        // was lost rather than only which tile (#77). A tile named twice
        // keeps the first kind that named it, the way `List.distinct`
        // always kept the first tile.
        let footingTargets =
            [
                for tile in sourceContainerTiles -> tile, FootingKind.SourceContainer
                for tile in Option.toList controllerContainerTile ->
                    tile, FootingKind.ControllerContainer
                for tile in storagePick -> tile, FootingKind.Storage
                for tile in
                    Set.union
                        (Atlas.storageTilesIn atlas room)
                        (Atlas.pendingStorageTilesIn atlas room)
                    |> Set.toList -> tile, FootingKind.Storage
            ]
            |> List.distinctBy fst

        let footingTargetTiles = footingTargets |> List.map fst

        // A standing link is a target, so its own footing has stopped
        // being buildable: added back, or the footing would jump the tick
        // the link went up. The working ground is deliberately not
        // subtracted — a footing is the one structure footing allowed
        // there (ADR 0022), because a link on a Seat or an Upgrade tile is
        // exactly what buys the Anchor and the upgraders a transfer
        // without leaving their tile.
        let footingCandidates =
            Set.union (Set.ofList buildable) (Atlas.linkTilesIn atlas room)

        // A target with no candidate at all leaves the tiles alone and is
        // recorded (#77): the fold reserves what it can, exactly as
        // before, and the shortfall rides out beside the plan instead of
        // falling through in silence. What it does reserve rides out too
        // (#106), each tile beside the target and the kind it was
        // reserved for — both are in scope here, and nowhere else is,
        // since no Intent ever names a link. Accumulated head-first and
        // reversed once, so both records read in the targets' own order.
        // The bare set of reserved tiles rides the accumulator beside the
        // served entries because the cluster ordering below needs exactly
        // that, and the filter here reads it per candidate tile.
        let footingTiles, servedFootings, unservedFootings =
            ((Set.empty, [], []), footingTargets)
            ||> List.fold (fun (taken, served, unserved) (target, kind) ->
                footingCandidates
                |> Set.filter (fun tile ->
                    range tile target = 1
                    && not (Set.contains tile roadPlan)
                    && not (List.contains tile footingTargetTiles)
                    && not (Set.contains tile taken))
                |> Set.toList
                |> function
                    | [] ->
                        taken,
                        served,
                        {
                            Target = RoomPos.at room target
                            Kind = kind
                        }
                        :: unserved
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile spawnPos, tile.X, tile.Y)
                        |> fun tile ->
                            Set.add tile taken,
                            {
                                Target = RoomPos.at room target
                                Kind = kind
                                Tile = RoomPos.at room tile
                            }
                            :: served,
                            unserved)

        // The tower and the extensions take the ordering again with the
        // footings held out — a footing outranks both — and the Storage's
        // pick held out with them: it outranks the footings, which are
        // anchored on it. Each footing draws one more tile in behind it,
        // and the reservation was widened by exactly that many, so no pick
        // reaches past the window the trunks dodged.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick) && not (Set.contains tile footingTiles))
            |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clusterPicks |> List.splitAt (min towerSlots clusterPicks.Length)

        // The container census the target clause is judged against
        // (ADR 0040): a container standing, or a site already going up.
        // The home room's, because the Layout plans one room; the outpost
        // rule asks the same census of its own room (`containerCensusIn`),
        // and there is one spelling of it so that ADR 0040 cannot come to
        // mean two things.
        let containerCensus = Atlas.containerCensusIn atlas room

        // The target clause (ADR 0040): a source is served when a
        // container stands or is pending within range 1 of it, the
        // controller when one stands or is pending in its Upgrade Work
        // Area — the geometry each rule already reads a container by, not
        // the tile this plan happens to have picked. A served target is
        // planned for no further container, wherever the thing serving it
        // sits; an unserved one is planned onto its pick as before.
        //
        // A pick the clause defers because something else serves its
        // target is a loss the room keeps — nothing demolishes the
        // orphan — so it rides out beside the footings and the trunks.
        // The tile it names is the lowest of a served target's
        // containers, which is one tile in every room we hold; a target
        // with two already had the defect this closes. A container on the
        // pick itself is the coinciding case and no loss at all: the plan
        // wanted exactly what stands there.
        let servingSource sourceId =
            Atlas.positionOf atlas sourceId
            |> Option.filter inHome
            |> Option.map (fun sourcePos ->
                Set.filter (servesSource (RoomPos.pos sourcePos)) containerCensus)
            |> Option.defaultValue Set.empty

        // Every target beside its pick and the containers already serving
        // it. Both answers below are read off this one list, so each
        // target is judged once and the same judgement decides whether it
        // is planned for and whether it lost its pick.
        let targets =
            [
                for sourceId, pick in sourceContainerPicks ->
                    ContainerTarget.Source sourceId, pick, servingSource sourceId
                for pick in Option.toList controllerContainerTile ->
                    ContainerTarget.Controller, pick, Set.intersect containerCensus upgradeArea
            ]

        let unservedPicks =
            targets
            |> List.choose (fun (_, pick, serving) ->
                if Set.isEmpty serving then Some pick else None)

        let deferredContainers =
            targets
            |> List.choose (fun (target, pick, serving) ->
                if Set.isEmpty serving || Set.contains pick serving then
                    None
                else
                    Some
                        {
                            Target = target
                            Pick = RoomPos.at room pick
                            Serving = RoomPos.at room (Set.minElement serving)
                        })

        // The tile clause (ADR 0040), and only it: a pick whose tile is
        // still owed a road waits, for the reason it always did — the
        // engine takes one construction site per tile, so the source
        // container (planned onto the trunk's first tile) waits for the
        // road to stand and then coexists with it. This is about the tile
        // and moves with no target.
        //
        // It reads the road sites actually placed and not the whole gap
        // (#209): the clause exists because two sites cannot share a tile,
        // so below the gate, where no road site is placed at all, there
        // is nothing for the container to collide with and nothing to wait
        // for. Read off `roadGap` instead, a bootstrapping room would hold
        // its source containers back until RCL3 waiting on a road that is
        // not coming — and a source container **site** on a Seat is already
        // a [[post]] (#205), the one that hires the [[anchor]] whose income
        // the gate exists to protect. Holding it back would spend the gate's
        // own saving.
        let owedRoad = Set.union (Atlas.pendingRoadTilesIn atlas room) placedRoads

        let containerGap =
            unservedPicks |> List.filter (fun tile -> not (Set.contains tile owedRoad))

        // The ramparts (ADR 0034): one over every standing Keep structure
        // and every standing Post container, the tick the thing it covers
        // stands — a site is not covered until it is a structure. No
        // allowance to size against: the rule is the whole plan, so the
        // gap is the covering census alone, standing ramparts and pending
        // sites subtracted the way the roads' is. The one gate is the
        // colony's [[stage]] (`keepsRamparts`, #214), which stands one
        // past the level the engine allows a rampart at: below it a
        // rampart is a floor the young colony's whole loaded crowd is held
        // to, and below the engine's own line every site would be refused,
        // every tick. The working-ground exclusion does not apply — a
        // rampart is no
        // footprint, walkable, blocking nothing, taking no tile from the
        // Post it covers (ADR 0022 as ADR 0034 revises it) — which is why
        // these tiles are read off the census and not off the ordering.
        let covered =
            if keepsRamparts view then
                Set.union (Atlas.keepTilesIn atlas room) (Atlas.postContainerTilesIn atlas room)
            else
                Set.empty

        let rampartGap =
            Set.difference
                covered
                (Set.union
                    (Atlas.rampartTilesIn atlas room)
                    (Atlas.pendingRampartTilesIn atlas room))

        let place kind tiles =
            tiles
            |> List.map (fun tile -> PlaceConstructionSite(RoomPos.at room tile, kind))

        place Storage (storagePick |> List.truncate (storageGap controller.Level))
        @ place Tower (towerTiles |> List.truncate (towerGap controller.Level))
        @ place Extension (extensionTiles |> List.truncate (extensionGap controller.Level))
        @ place Road (Set.toList placedRoads)
        @ place Container containerGap
        @ place Rampart (Set.toList rampartGap),
        List.rev servedFootings,
        List.rev unservedFootings,
        unroutedTrunks,
        deferredContainers
    // A room the Layout cannot even orient itself in plans nothing and
    // loses nothing: there are no targets to serve and no trunk was ever
    // asked for, so every record is empty rather than any of them being a
    // shortfall (#77, #106, #107).
    | _ -> [], [], [], [], []

/// The outpost's source containers (ADR 0042) — the colony's one placement
/// rule that is not the Layout's, and a rule beside it rather than a
/// branch inside it: one container per outpost source, on the Seat whose
/// walk out to the Seam toward home is shortest.
///
/// **Why it is not the Layout's.** The Layout seats a source container on
/// the Seat nearest that source's trunk, and a trunk is a paved line to a
/// spawn. An outpost has no spawn, so the pick needs another anchor, and
/// the Seam is the only fixed thing in that room home lies beyond — a
/// container placed on it is a container the haul leaves by the shortest
/// road there is. Nothing here orders a clustered pick, reserves a Link
/// footing (ADR 0027), paves a trunk or enters any of the layout record's
/// three lists: every one of those is a fact about the home room's plan,
/// which ADR 0042 leaves untouched ("The outpost gets a container and
/// nothing else. No roads, and no Layout").
///
/// **The room is the outpost's own.** The placement Intent has carried a
/// room name since it was written, and the Layout stamps the single room
/// it plans onto every site it emits. An outpost pick routed through that
/// path would drop a container site on the *home* room's tile of the same
/// coordinates — a real tile, in a real room, at a real 5,000 energy — so
/// this rule stamps the room the source stands in, read off the
/// projection's own id-to-room join (ADR 0041).
///
/// **ADR 0040 holds here, and by target rather than by tile.** A source
/// with a container standing, or a site pending, within range 1 of it is
/// served wherever the thing serving it sits, and is planned for no second
/// one. The census is read in that source's own room: a `Pos` carries no
/// room, so a home container on an outpost source's coordinates would
/// otherwise defer the plan forever and leave the outpost with no switch
/// to close (ADR 0041). There is no tile clause and there cannot be one —
/// nothing paves an outpost, so no pick is ever owed a road.
///
/// **Only into a room the colony can see.** Both halves of this rule are
/// paid for by vision: the census that defers it is empty in a room
/// nothing looks into, and the Intent itself is answered by the
/// Executor's `Game.rooms` lookup, which holds the seen rooms alone. A
/// blind room's empty census is a missing entry and not a "no container"
/// (ADR 0004), so planning off it would read an absence as an answer —
/// and would hand the Executor an Intent it can only report as
/// `ActorMissing`, the outcome it reserves for an upstream bug, once a
/// tick per rock for ever. The gate is the room's `RoomControl` entry,
/// which is exactly the view's per-room vision fact and what
/// `sourceOutputOf` reads for the same reason. Nothing is lost by
/// waiting: a Harvest names an outpost rock with no vision at all (ADR
/// 0041), so a creep walks there on its own, and the tick it arrives is
/// both the tick the census can be trusted and the first tick the site
/// could have been created at all.
///
/// **Recomputed every tick, and deliberately not ridden on the plan
/// memo.** Since #149 the signature does name the room in every standing
/// entry and does span the outpost layers — the hauler quota's price, and
/// it has been paid (`censusSignature`). What it still does not sign is
/// the rest of what this rule reads: the outpost's terrain, its declared
/// source tiles, its Seam band, and the *pending* census this rule's own
/// site lands in, which stays the home layer's alone. Riding the memo
/// would mean signing all four, and the pending half is the one that
/// costs — the whole Layout and the whole spawn-walk table (ADR 0032)
/// thrown away the tick an outpost site appears, for geometry nothing
/// reads while it is still a site. One throw-away and not two: the tick
/// it *completes* is paid either way now, because the container it
/// becomes is a standing structure the census spans (`censusSignature`)
/// and the hauler quota is the memo entry that prices it. What signing
/// the site would buy is one flood a room a tick: the walk out to the
/// Seam is memoised inside the Atlas on the room pair, so every Seat of
/// every source in one room shares one flood (`Atlas.seamWalkTicks`),
/// and this is not the cost class that made the Layout worth memoising
/// — the Layout routes trunks and orders 2,500 tiles. Measured on `npm run profile -- --scenario outpost --level 5`
/// (two outposts, three rooms, RCL5, the level named so the figure does
/// not silently re-baseline when the default moves with the colony):
/// **4.84 ms a tick without this rule and 5.35 with it**, against ADR
/// 0041's condition to revisit any of this — a mean tick above 50 ms or a
/// single tick above 80.
///
/// Recomputing also keeps the deferral honest with no signature at all:
/// the tick a container becomes *visible* in an outpost, this stops
/// planning one — and on the blind ticks either side of that it plans
/// nothing at all, so there is no tick on which a stale answer could be
/// handed back.
///
/// Total (ADR 0004): a source the projection does not place, a source in a
/// room the colony cannot see, a room with no Seam band to home, and a
/// source no Seat of which can reach one all plan nothing — unpriceable
/// geometry is never planned onto, and never blocks.
///
/// What finishes what this rule starts, named here because the two halves
/// are read apart: the site placed here becomes a Build Task like any
/// other, because `ColonyView.ConstructionSites` is every room the colony
/// can see and no longer the spawn rooms' alone (#150). The Task, its Work
/// Area and its price stay outpost-blind — it names a site by id, the area
/// is that site's room's (ADR 0041) and the price crosses the Seam like
/// every other cross-room price (#123). Its **tier**, its **cap** and its
/// **applicability** are not, since #157, and all three ask
/// `isOutpostContainerSite` which site this is — the tier and the
/// applicability through `isFeedingSite`, which since ADR 0047 lifts a
/// [[nursery]]'s sites onto the same tier by their room alone, and the cap
/// off the rule below it, which is deliberately the narrower question of
/// the two.
///
/// *Which* creep builds it is the ordinary ranking's answer and nothing
/// this rule arranges — but the ranking had to be corrected before that
/// answer was anybody (#157). This site's Build is **feeding tier**, not
/// surplus: it is the switch that admits a room into the economy, so it
/// ranks with the flow, is capped at two concurrent builders outside a
/// nursery — where the budget lets go of the site this rule places along
/// with every other one in that room (ADR 0047) — and a
/// loaded home worker with no Refill left to do walks the Seam for it
/// (`tierOf`, `taskCapacities`). Read on the surplus tier it lost to the
/// home Upgrade every tick, and the answer #150 wrote down here — that
/// the builder would be a creep which walked out for this source's own
/// Harvest and filled up there — never happened on the deployed colony:
/// the Storage's Withdraw is feeding tier and underfoot, so no
/// cross-Seam Harvest ever won a creep, and the reserver (#131) carries
/// CLAIM and no Work. The switch was laid down and nothing closed it.
/// There is still no outpost builder row and this rule invents none.
/// Until the pool widened, the site was named by no Task at all and the
/// switch could not close however near a creep stood.
let private planOutpostContainers (view: ColonyView) atlas : Intent list =
    let home = SpatialInfo.homeName view.Spatial

    // Every rock the projection places in a room that is not home and that
    // the colony is looking into this tick — `RoomControl` carries one
    // entry per seen room, and vision is what both the census below and
    // the Executor's own `Game.rooms` lookup are paid for with.
    view.Sources
    |> List.choose (fun s ->
        match Atlas.positionOf atlas s.Id with
        | Some tile when tile.Room <> home && Map.containsKey tile.Room view.RoomControl ->
            Some(s.Id, tile)
        | _ -> None)
    |> List.choose (fun (sourceId, source) ->
        let room = source.Room

        let served =
            Atlas.containerCensusIn atlas room
            |> Set.exists (servesSource (RoomPos.pos source))

        if served then
            None
        else
            // The pick, and with it the tie-break — the same trap the
            // Layout's own pick has: three Seats all of swamp can price
            // identically, so the lowest (X, Y) answers, exactly as
            // `sourceContainerPicks` and every other tie in the colony
            // answers. A source with one Seat is the same rule with one
            // candidate, not a case of its own.
            Atlas.seatTilesOf atlas sourceId
            |> RoomPos.inRoom room
            |> Set.toList
            |> List.choose (fun seat ->
                Atlas.seamWalkTicks atlas room home seat |> Option.map (fun walk -> walk, seat))
            |> function
                | [] -> None
                | priced ->
                    let _, seat =
                        priced |> List.minBy (fun (walk, seat: Pos) -> walk, seat.X, seat.Y)

                    Some(PlaceConstructionSite(RoomPos.at room seat, Container)))

/// Colony reflex beside the pipeline, the second after safe mode: every
/// creep with free carry capacity standing within pickup range of a
/// dropped energy pile asks to pick it up — beside its assigned Task's
/// action, since the engine's pickup conflicts with no other action. No
/// movement, no matching, no threshold: the reflex only recaptures what
/// is already in reach (death drops, harvest overflow), and duplicate
/// pickups on one pile are the engine's to settle.
///
/// Paired once per room the projection places a creep in, and never across
/// two (#166): a pickup is a range-1 act inside one room, and a pile in
/// one room and a creep in another on the same coordinate would draw a
/// pickup the engine answers ERR_NOT_IN_RANGE. The pairing walks
/// `Atlas.placedCreeps` grouped by `.Room` against `Atlas.droppedEnergyIn`
/// for that same room, and since #216 R3 both sides carry that room in the
/// tile, so the range is measured or it is not measured at all. Nothing is
/// priced and no walk is needed for this, which is why it does not wait on
/// a cross-room reflex: range 1 is the whole of the geometry, and it is
/// only ever asked inside one layer.
///
/// The room that made it necessary is the outpost (ADR 0042): its hauler
/// runs one container (one creep, a long round trip), so the Anchor's
/// overflow lands on the container's own tile, and a full container turns
/// that overflow into a pile that no later withdrawal takes back. The
/// hauler then stands *on* the pile — range 0 — and, while both sides
/// answered home, walked away from it.
let private planPickups (view: ColonyView) atlas : Intent list =
    let hungry =
        view.Creeps
        |> List.filter (fun c -> c.FreeCapacity > 0)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    Atlas.placedCreeps atlas
    |> List.groupBy (fun (_, tile) -> tile.Room)
    |> List.collect (fun (room, placed) ->
        match Atlas.droppedEnergyIn atlas room with
        | [] -> []
        | piles ->
            placed
            |> List.collect (fun (name, pos) ->
                if Set.contains name hungry then
                    piles
                    |> List.choose (fun (pile, tile) ->
                        if RoomPos.range pos tile |> Option.exists (fun r -> r <= 1) then
                            Some(PickupEnergy(name, pile))
                        else
                            None)
                else
                    []))

/// Ticks until a source restocks (ADR 0025), 0 while it holds energy —
/// and 0 for a source the view does not carry at all, so a source
/// nothing projects never holds a decision up.
let private ticksToRestock (view: ColonyView) sourceId =
    view.Sources
    |> List.tryFind (fun s -> s.Id = sourceId)
    |> Option.map (fun s -> s.TicksToRestock)
    |> Option.defaultValue 0

/// Whether a creep garrisons a source's container Post: ADR 0024's
/// condition — a Work-heavy body standing on that source's built
/// container. The **full-store** reprieve and nothing else since ADR 0048
/// split the pair ADR 0025 had made one: overflow past a full store falls
/// into the container the creep is standing on, so the tile that catches
/// it is the only tile that can earn this, and widening it by a step
/// would keep a full body digging onto the floor (ADR 0012).
let private garrisons atlas (creep: CreepInfo) sourceId =
    Atlas.workHeavy atlas creep.Name
    && Atlas.catchesOverflow atlas creep.Name sourceId

/// Whether a Work-heavy body holds a source through its empty window: the
/// **empty-source** reprieve, which ADR 0048 widened off the container to
/// the source's whole digging range. Named for the reprieve and not for
/// the Post on purpose: the glossary says "man" of a Post, and the second
/// option ADR 0048 rejected by name was widening this to the source's
/// Posts — the tile the Anchor is bumped onto is not one, and that is the
/// whole bug. ADR 0025 wrote the two reprieves as
/// one judgement about one tile, and the room proved them different
/// questions: a hauler drawing the container swaps the Anchor onto the
/// Seat beside it, and on the container-only condition that one step
/// released it TooEarly, freed its Post, and left it competing for
/// somebody else's — the live symptom #193 reports, a full source with
/// nobody on it and a dry one with two. Overflow is a fact about the tile
/// underfoot; being in position to dig is a fact about the range, and
/// range 1 is what the engine asks of a dig. So the widened half asks the
/// range and the narrow half keeps the container.
///
/// Still the body's own reprieve and never a light one's (ADR 0016, ADR
/// 0024): a worker beside a dry rock has other work in reach and is
/// released to go and do it, exactly as ADR 0013 released it. What the
/// window forgives is the body with nowhere else worth being.
///
/// Which is also why ADR 0025's Dual Seat exclusion survives the widening
/// intact: on a Dual Seat the body does have somewhere else worth being,
/// and it is the tile it is already on. Harvest ranks ahead of Upgrade, so
/// an exemption there would hold the Anchor on a Task the Emitter will not
/// let it act on and stand it still for the whole window with a load it
/// could have spent — the option ADR 0025 considered and rejected by name.
/// ADR 0024's own condition is not subtracted with it: a container
/// standing on a Dual Seat catches overflow like any other, and that tile
/// was exempt before this widening.
let private keepsThroughEmptyWindow atlas (creep: CreepInfo) sourceId =
    garrisons atlas creep sourceId
    || (Atlas.workHeavy atlas creep.Name
        && Atlas.standsAtSource atlas creep.Name sourceId
        && not (Atlas.standsOnDualSeat atlas creep.Name))

/// The walk and the wait that hold a Task up for this creep, or None when
/// its time has come (ADR 0025, repriced by ADR 0029): a drained source's
/// Harvest is applicable only when the creep's walk covers the restock
/// wait — walk ≥ ticks to restock, with no slack, because the wait shrinks
/// by one each tick while the walk stays put, so a creep one tick short
/// departs one tick later and arrives as the energy does. The walk is the
/// Atlas's own query and nothing here converts anything: it is already
/// whole ticks, floored at one a tile and
/// blind to today's traffic, so a bystander in the lane cannot dispatch a
/// creep this tick and recall it the next. A creep already beside a dry
/// rock has no walk to cover anything and is released, exactly as ADR 0013
/// released it. One exemption, ADR 0024's condition as ADR 0048 widened
/// it: a Work-heavy body already in digging range of the source keeps its
/// Post through the empty window. A bare Dual Seat is subtracted from that
/// widening and gets none, as ADR 0025 had it: Upgrade is in place there,
/// so it upgrades through the window and rematches Harvest once its Carry
/// is spent.
/// And the dispatch itself is a light body's rule (ADR 0048, narrowing ADR
/// 0025): for a Work-heavy body a drained source is early whatever its
/// walk, unless that exemption holds. "The walk covers the wait, so set
/// out now" was written for a body that pays a tick a tile; an Anchor pays
/// four to seven, so its walk covers almost any wait, and the rule
/// dispatched one across half a room — and, once the projection layered,
/// across a border — onto a Post another Anchor was still standing on,
/// leaving a full source unworked behind it (#193). A heavy body would
/// rather wait the window out where it stands than spend it walking and
/// pay for the walk home as well; a freshly cast one waits beside the
/// spawn instead, at most a restock's fifty ticks once in a 1,500-tick
/// life, which is accepted. Every other Task is judged at the current
/// tick.
/// Two consequences worth naming, both ADR 0004's totality. The walk
/// answers 0 for an unplaced creep or target, and this gate reads that as
/// an arrival of now — the one place unpriceable geometry holds a Task up,
/// waiting out a drained source exactly as a creep on its Seat does.
/// Exempting it would assign a creep to waiting instead, the stranding ADR
/// 0013 fixed, and the outcome is 0013's own: no Task at all for a drained
/// source. And an unreachable Work Area has no walk at all, which is not
/// earliness: the reachability gate stands ahead of this one and names
/// that rejection itself.
/// The walk arrives deferred because only this arm and the capacity gate
/// spend it, and pricing one for every Task in the pool measured at a
/// third of the tick (ADR 0029).
/// The answer is that pair rather than a yes because the reason the
/// rejection and the release carry is exactly what was compared here
/// (#88): read off the gate, never re-derived by a caller from a price
/// that no longer converts to ticks.
let private tooEarly (view: ColonyView) atlas (creep: CreepInfo) task (walk: Lazy<int option>) =
    match task with
    | Harvest sourceId ->
        match walk.Value with
        // No walk at all is unreachable geometry, which is not earliness:
        // the reachability gate stands ahead of this one in both cascades
        // and names that rejection itself (ADR 0002, ADR 0029).
        | None -> None
        | Some ticks ->
            let wait = ticksToRestock view sourceId

            // `wait = 0` became load-bearing when the heavy arm below
            // stopped reading the walk: a stocked source is a wait of zero
            // and every walk covers it, so without this the arm would
            // report a heavy body as early against a wait there is not,
            // and no Anchor would ever be dispatched to any source.
            if wait = 0 || keepsThroughEmptyWindow atlas creep sourceId then
                None
            // The heavy arm reports the same pair every other rejection
            // does — the walk it would have made against the wait it does
            // not cover for it — so the transition log reads as one gate
            // with one reason and never as two (#88).
            elif Atlas.workHeavy atlas creep.Name || ticks < wait then
                Some(ticks, wait)
            else
                None
    | Withdraw _
    // A pile is workable the tick a creep reaches it and every tick
    // before (#167). It moves — down by decay, up under an [[anchor]]
    // spilling onto a full [[container]] — but neither direction is a
    // restock, so there is no tick to be early *of* and nothing for this
    // gate to compare a walk against. A pile that shrinks under the
    // threshold before the walk ends is not earliness either: it leaves
    // the pool, and the release is task-gone's (`pickupThreshold`).
    | Pickup _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    // A controller is always there to be reserved: a reservation has no
    // restock and no stock, so a reserver that has walked to one is never
    // early (ADR 0042). A claim is the same (ADR 0047) — a room is
    // takeable or it is not, and a room that stops being takeable
    // mid-walk leaves the pool rather than making its holder early.
    | Reserve _
    | Claim _
    | Flee -> None

/// The room a Task's Work Area lies in: its target's, since the area is
/// that target's surroundings and empty across a border (ADR 0020, ADR
/// 0041) — so the Reach taken out of it is that room's share (#138). None
/// for Flee, whose area is the creep's own room's, and for a target the
/// projection does not place, whose area is empty and takes nothing.
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
    | Flee -> None

/// The tiles a creep may work a Task from this tick (ADR 0033): its Work
/// Area less its room's Reach — and for Flee, the safe set of the room the
/// creep stands in, an area of the colony's own rather than some target's
/// surroundings. Each is the share of one room (#138): a hostile a room
/// away on the same coordinate takes no tile here. The Atlas's memoised
/// Work Areas are never modified; this is a filter at the point of
/// judgement, and a tick with no Reach anywhere hands the memo back
/// verbatim.
let private areaFor (threats: Threats) atlas creep task : Set<RoomPos> =
    match task with
    | Flee ->
        Atlas.creepRoom atlas creep
        |> Option.map (Threats.safeIn threats)
        |> Option.defaultValue Set.empty
    | _ when Map.isEmpty threats.Reach -> Atlas.workAreaFor atlas creep task
    | _ ->
        // A Reach is one room's grid (`Threats.Reach`), so the tiles it
        // takes are matched on that room's coordinates and on no other's
        // (ADR 0052 decision 2, #138).
        let room = roomOfWork atlas task

        let reach =
            room |> Option.map (Threats.reachIn threats) |> Option.defaultValue Set.empty

        Atlas.workAreaFor atlas creep task
        |> Set.filter (fun tile ->
            not (Some tile.Room = room && Set.contains (RoomPos.pos tile) reach))

/// The travel cost of a Task for a creep, priced over the tiles it may
/// actually work from this tick (ADR 0033): the safe set for Flee, which
/// has no target, and every other Task's own Work Area less the Reach —
/// so the reachability gate judges the tiles that are left rather than a
/// tile the creep may not stand on, and a candidate whose cold remainder
/// is walled off is rejected as unreachable instead of being held and
/// never worked. The pricing itself is untouched: same weights, same
/// surcharge, same flood — only the goals are this tick's.
/// An area that is empty here was never taken by the Reach — the threat
/// gate stands ahead of this one in both cascades — so it falls back to
/// the Task's own price, which carries ADR 0004's escape for a target the
/// projection cannot place, and, since #123, the Seam join for a target
/// in another room. The Work Area a creep is handed is empty across a
/// border by construction (ADR 0041): standing and acting are in-room
/// acts, while the price is a minimum over the Seam band and knows both
/// rooms. So an outpost's Task reaches the Matcher
/// priced and ranks in the one pool, and it does so through this fallback
/// rather than through a case of its own.
let private travelCostOf (threats: Threats) atlas (creep: string) task =
    match task with
    | Flee -> Atlas.travelCostWithin atlas creep (areaFor threats atlas creep task)
    | _ ->
        match areaFor threats atlas creep task with
        | area when Set.isEmpty area -> Atlas.travelCost atlas creep task
        | area -> Atlas.travelCostWithin atlas creep area

/// Whether the Reach has taken a creep's whole Work Area for a Task (ADR
/// 0033): it had somewhere to stand and has nowhere left. That makes the
/// Task inapplicable to that creep — a Harvest whose only Seat is hot is
/// no Harvest — and releases a holder under a reason of its own, so the
/// transition log tells a raid's release from a Task that vanished. An area
/// that was empty to begin with is not threatened: unplaceable or blocked
/// geometry is the reachability gate's answer (ADR 0002, ADR 0020) and a
/// raid must not be blamed for it. Flee is never threatened either — its
/// area is the safe set, which the Reach has already been taken out of.
let private threatened (threats: Threats) atlas (creep: CreepInfo) task =
    not (Set.isEmpty (Atlas.workAreaFor atlas creep.Name task))
    && Set.isEmpty (areaFor threats atlas creep.Name task)

/// Whether a construction site is an outpost's source container: the one
/// site this colony ever places outside its own room
/// (`planOutpostContainers`), and so the one Build that is a switch on a
/// room's whole economy rather than a piece of surplus work (ADR 0042).
/// Three readers, which is why it is a rule and not a line inlined three
/// times — the applicability gate just below, the tier that gate exists
/// because of, and the concurrency cap that keeps the tier from emptying
/// the home room across the Seam. The first two ask it through
/// `isFeedingSite` since ADR 0047, which is the one spelling those two
/// share; the cap asks it here, and asks it alone, because what the budget
/// covers is narrower than what the tier lifts.
///
/// Both halves come off the projection and neither off the declaration
/// (ADR 0041), exactly as the Reserve pool's does: the id-keyed kind
/// census says a container is going up, and the layer that places the id
/// says which room it stands in (`Atlas.targetRoom`). So a room a
/// stand-down drops from the scan set (ADR 0043) leaves this reading with
/// it rather than through a second gate free to disagree with the first.
///
/// The room half is load-bearing and not decoration: a `Pos` carries no
/// room (ADR 0041), so the kind alone would read the home room's own
/// container sites — one per source and one at the controller, all of them
/// the Layout's (ADR 0012) — as outpost switches, and pull the worker row
/// onto them off the feeding tier.
///
/// Total (ADR 0004): a site the projection does not place names no room,
/// answers false, and is the ordinary surplus Build it has always been.
let private isOutpostContainerSite (view: ColonyView) atlas siteId =
    Map.tryFind siteId view.Spatial.TargetKinds = Some(Site BuiltKind.Container)
    && Atlas.targetRoom atlas siteId
       |> Option.exists (fun room -> room <> SpatialInfo.homeName view.Spatial)

/// Whether a construction site stands in a **nursery** — a room this
/// colony has claimed and not yet stood a spawn in (`isNurseryRoom`, ADR
/// 0047 decision 4). The room half of `isOutpostContainerSite` above with
/// its kind half deliberately dropped: in a nursery **every** site is the
/// switch, where in an ordinary outpost only the container is.
///
/// Which is the ADR's own generalisation and not a widening for its own
/// sake. What is being built there is the spawn that ends the nursery, and
/// the site is one a *human* places by hand — the Layout plans the home
/// room alone (ADR 0011) and `planOutpostContainers` places containers —
/// so a rule that picked sites out by kind here would be a list of the
/// kinds a human is allowed to want built, and a spawn site placed beside
/// the extensions that feed it would have raised half of what it needs.
///
/// The room join is `isOutpostContainerSite`'s, for the reason recorded
/// there: a `Pos` carries no room (ADR 0041), so the site's room comes off
/// the layer that places its id. Total (ADR 0004): a site the projection
/// does not place names no room, answers false, and is the ordinary
/// surplus Build every site outside a nursery is.
let private isNurserySite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId |> Option.exists (isNurseryRoom view)

/// Whether a room is **bootstrapping** as seen from this colony's tick: a
/// child of ours running its own spawn (`isBootstrapRoom`, the mother's
/// reading), or this colony's own home standing at the `Bootstrapping`
/// stage (the child's own reading). One predicate for both ticks, because
/// the rule that reads it is about the room and not about who is looking
/// (ADR 0052 decision 3: a stage, read wherever a rule differs by stage).
///
/// The home half reads the stage and no longer a level of its own: a
/// living colony's home has a spawn standing by construction (ADR 0047
/// decision 1), so `Bootstrapping` is exactly "a spawn of ours and a
/// controller under `Colony.bootstrapLevel`", the two facts this used to
/// spell out. The mother's half is deliberately the wider one, at any RCL
/// while she still projects the room — see `isBootstrapRoom` — because
/// what closes her window is her scan set.
let private isBootstrappingRoom (view: ColonyView) room =
    isBootstrapRoom view room
    || (room = SpatialInfo.homeName view.Spatial && homeStage view = Some Bootstrapping)

/// A site standing in a bootstrapping room: feeding-tier in both pools
/// (user, 2026-09-06: "pioneer 都在升级没人建 extension … 房间里很多小
/// worker 也都在升级，不管 extension"). What a room under RCL3 builds is
/// its containers and its extensions, and the extensions are the bank —
/// 300 to 550 doubles the Anchor body and with it the income the whole
/// window is waiting on — so they come before the controller, for the
/// child's own workers and for the pioneers alike. The borrowed Upgrade
/// drops back to surplus while such a site stands (`tierOf`), so a
/// pioneer builds first and upgrades after.
let private isBootstrappingSite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId |> Option.exists (isBootstrappingRoom view)

/// Whether any site stands in the room of the named controller — the
/// borrowed Upgrade's other half: while the child has sites, its
/// controller waits.
let private sitesPendingBeside (view: ColonyView) atlas controllerId =
    match Atlas.targetRoom atlas controllerId with
    | None -> false
    | Some room ->
        view.ConstructionSites
        |> List.exists (fun site -> Atlas.targetRoom atlas site.Id = Some room)

/// Whether this Build is on the feeding tier rather than in the surplus
/// the colony's other sites are spent out of — the two rules that lift one
/// there, said once, because the tier and the body gate that exists
/// *because* of the tier must never be able to disagree about which sites
/// they are talking about.
///
/// An outpost's container site, the switch on whether that room is in the
/// economy at all (ADR 0042, #157); and every site in a nursery, the
/// switch on whether there is going to be a second colony at all (ADR
/// 0047). One question deeper each time, and the same shape of answer: a
/// Build the colony's reproduction is waiting on is not work it does with
/// energy it already has.
let private isFeedingSite (view: ColonyView) atlas siteId =
    isOutpostContainerSite view atlas siteId
    || isNurserySite view atlas siteId
    || isBootstrappingSite view atlas siteId

/// Whether a creep can usefully work this Task right now. The body must
/// physically be able to do it — Work-part tasks need a Work part, energy
/// delivery needs a Carry part — and the energy state must call for it: a
/// full creep is done harvesting; an empty creep has nothing to deliver.
/// One geometric widening (ADR 0012), body-aware since ADR 0024: a full
/// Work-heavy creep standing on a built source container keeps Harvest —
/// the engine drops the overflow into the container underfoot, so the
/// creep effectively has capacity and the Post stays garrisoned. A light
/// body gets no such reprieve: its full store ends its dig wherever it
/// stands, or it would hold the Post for the rest of its life — never
/// Inapplicable, so never released — and lock the garrison out of the one
/// tile it can work from. Gates read part arithmetic, never names or
/// roles (ADR 0006) — including one comparative gate (ADR 0016): a body
/// with more Work than Move never Withdraws, so its only feeding-tier
/// candidate is Harvest and an unmanned Post wins it regardless of
/// distance. Travel cost pins an Anchor that is at its Post; this gate
/// is what walks one home past a nearer stocked container.
/// A second gate stands at the other end of the haul cycle, this one
/// reading the target's kind beside the body (ADR 0019): only a creep
/// with a Work part draws from the controller's upgrade buffer. A body without one can spend nothing at the
/// controller, so its Withdraw there is energy flowing back the way it
/// came — and with every other sink full, the buffer is also its only
/// Refill target, which cycled a hauler in and out of one container tick
/// after tick. Source containers stay open to every carrier, and so does
/// the Storage: the gate is scoped to the buffer by id, and the stock's own
/// in-and-out cycle is closed in the Planner instead (ADR 0023), because
/// the bodies that must feed the spawn from it are the ones with no Work.
/// A third gate reads the target beside the body in the same way (#157):
/// an outpost container site's Build ranks on the feeding tier, where no
/// travel cost separates it from the work a heavy body should be doing,
/// so that one Build is inapplicable to a Work-heavy body — the arm
/// carries why.
/// A fourth reads the body alone and covers three Tasks at once (ADR
/// 0046): Build, Repair and Refill are inapplicable to a **standing body**
/// — one carrying fewer than one Carry per four Work — because every one
/// of the three is a delivery, and a delivery by a body that holds fifty
/// energy against eleven Work is a commute the colony pays for in idle
/// Work at the buffer it walked away from. It is the same kind of gate ADR
/// 0016's is and for the same reason travel cost could not be it: the
/// nearest such Task is often underfoot, so cost separates nothing and
/// only a prohibition moves the body. Every other Task is deliberately
/// left as its own gates already had it — Upgrade, Withdraw and Harvest,
/// the working life the upgrader row was shaped for, and Pickup, which
/// stays open because a pile is an intake and not a delivery — and no new
/// gate is opened on Withdraw in particular (ADR 0016's stands as written,
/// and the upgrader row is at `Work = Move`, inside it; an Anchor is not,
/// so "untouched" leaves that body exactly as shut out as it was). The
/// hauler row is outside this gate by arithmetic and not by exception:
/// `Carry * 4 < 0` is false, so a Carry-only body refills as it always
/// has.
/// A fifth reads the geometry beside the body and lands on the one Task
/// the fourth had to leave open (ADR 0048): Upgrade is applicable to a
/// **Work-heavy** body only where it may already act on it. That is a
/// narrower row than the fourth's — the upgrader is at `Work = Move` and
/// is not Work-heavy, so its own footing beside the buffer is untouched —
/// and it is the walk it refuses and never the Task; the arm carries why.
/// One **exception** crosses the third gate and the fourth together (#205,
/// amending ADR 0045 and ADR 0046): a container construction site standing
/// on the creep's own Post, under its own feet, is applicable to it
/// whatever its shape. Both of those gates refuse a walk and there is no
/// walk here — the body digs the source beside it and spends what it dug
/// into the progress it is standing on, which is how an outpost's
/// container was raised before either rule existed; the arm carries why.
let private applicable (view: ColonyView) (threats: Threats) atlas (creep: CreepInfo) task =
    let has part =
        creep.Body |> Map.tryFind part |> Option.exists (fun n -> n > 0)

    match task with
    // ADR 0024's full-store reprieve, and beside it the clause that keeps
    // ADR 0048's own Consequence reachable ("stands where it is until it
    // can dig again"). A Work-heavy body never empties — ADR 0016 shut
    // Withdraw and Transfer, ADR 0046 shut Refill, Build and Repair, ADR
    // 0048 shuts the walk to the controller — so once its one Carry is
    // full it is full for the rest of its life, and a store gate that
    // reads fullness as "done here" reads a garrison's ordinary condition
    // as a reason to take its work away. Which it did: a hauler drawing
    // the container swaps the Anchor onto the Seat beside it, and a full
    // body one step off its Post had no Task at all and so no walk back
    // to one (#193's own symptom, re-made by the cure).
    //
    // So the gate is not widened by a tile but by a question: ADR 0024
    // asks whether a body may keep *digging* where it stands, and a body
    // that is still walking is not digging. `mayAct` over the same tiles
    // the Emitter acts from is that question — false while the walk is
    // ahead of it, so the full body keeps Harvest and travel cost walks
    // it home; true the tick it arrives, where ADR 0024's condition
    // governs unchanged and the only tile that keeps a full body digging
    // is still the container whose overflow the engine catches (ADR
    // 0012). Nothing digs onto the floor that did not before, and the
    // light body's full store still ends its dig wherever it stands.
    //
    // And the walk is offered only where it ends somewhere: a source with
    // a Post (ADR 0020, ADR 0024). Every Post is a tile the arriving body
    // has something to do on — a container Seat catches its overflow, a
    // Dual Seat spends its load into the controller in place, and a Seat
    // carrying a container site spends the load into the progress under
    // its own feet (#205) — so the reprieve never walks a full body onto a
    // bare Seat it would be released from with nowhere left to go. The
    // third kind is the one whose work is a Task rather than a reflex, so
    // it is the one that has to be kept reachable: the outpost container
    // budget does not price the body standing on the site (`hasCapacity`),
    // or the walk offered here would end on a full store, a shut Build and
    // nothing else. A source with no Post is a light body's rock and the
    // full-store rule there is untouched.
    | Harvest sourceId ->
        has Work
        && (creep.FreeCapacity > 0
            || garrisons atlas creep sourceId
            || (Atlas.workHeavy atlas creep.Name
                && not (Set.isEmpty (Atlas.postsOf atlas sourceId))
                && not (Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))))
    // The body half of this gate — a Carry part and ADR 0016's comparative
    // clause — is read a second time out of line by `canRefill`, the supply
    // floor's arming condition (ADR 0050): a clause narrowing what a body
    // may draw with belongs in front of both readers, or a colony whose only
    // carrier this gate has just shut out still reads as able to refill.
    | Withdraw storeId ->
        let buffer = Set.contains storeId (Atlas.controllerContainers atlas)

        has Carry
        && creep.FreeCapacity > 0
        && not (Atlas.workHeavy atlas creep.Name)
        && (has Work || not buffer)
        // A standing body fetches from the buffer at its feet and from
        // nowhere else (#206, ADR 0046): its one Carry is one trip's worth,
        // and a trip to the Storage — or across a Seam to a pile — is the
        // commute the row was shaped to never make. Live, an `11W/1C/11M`
        // upgrader walked fifty tiles into the child's room for fifty
        // energy the tick its buffer ran dry. Dry, it waits
        // (`NoneApplicable`); the buffer's own Refill is the haulers'.
        && (buffer || not (isStandingBody creep))
    // The Withdraw gate without its one target-shaped clause (#167): a
    // Carry part, room to put the energy, and ADR 0016's comparative gate
    // — a Work-heavy body's intake is digging, and picking a pile up off
    // the ground is no more its work than drawing a container is. The
    // buffer clause has no counterpart here: ADR 0019 shuts a Work-less
    // body out of the *controller's* container, and a pile is nobody's
    // buffer — it is energy on the floor, and any carrier that can lift it
    // is spending it somewhere the buffer's own drawers cannot. What does
    // have a counterpart is the standing-body clause (#206): a pile is an
    // intake, but never one at a standing body's feet — the reflex takes
    // the pile a creep stands beside (#166), and this Task is the walk.
    | Pickup _ ->
        has Carry
        && creep.FreeCapacity > 0
        && not (Atlas.workHeavy atlas creep.Name)
        && not (isStandingBody creep)
    // Its two body clauses are read a second time out of line by
    // `canRefill`, beside Withdraw's (ADR 0050) — the Energy clause is not,
    // being a state and not a fact about the body.
    | Refill _ -> has Carry && creep.Energy > 0 && not (isStandingBody creep)
    // The one Build with a body gate on it (#157), and it is here for the
    // same reason ADR 0016's Withdraw gate is: `tierOf` below lifts this
    // site onto the feeding tier, and a rank the whole colony shares is
    // exactly what travel cost can no longer thin. What travel cost was
    // holding up is written in the doc above — "Travel cost pins an Anchor
    // that is at its Post" — and on this Task alone it stopped holding: a
    // full Anchor whose Post has no standing container under it (a source
    // whose container is still a site, or has decayed) loses Harvest, and
    // was then outranked off its own controller and walked fifty tiles at
    // four to seven ticks a step to spend one Carry into a 5,000-progress
    // site. A heavy body's cross-room work is a Post and never a delivery
    // (ADR 0020), so the switch is light bodies' work; what it costs the
    // colony is one body's walk and never a garrison's Post, because the
    // Post an Anchor is hired for is this very site (#205) and the
    // exception below leaves it building where it stands.
    //
    // The gate follows the *tier* and not the container, which is why it
    // asks `isFeedingSite` and not the container rule alone: ADR 0047's
    // nursery lifts every site in a claimed room onto the same tier, so
    // the same Anchor that was walked off its Post by a container site is
    // walked off it by a spawn site, and the reason it may not go is the
    // one above word for word: a heavy body's cross-room work is a Post
    // and never a delivery (ADR 0020).
    //
    // A nursery is *not* the empty-handed room the sentence above says an
    // outpost with a pending container is. It is still the mother's
    // outpost, so `planOutpostContainers` places a container on its source
    // like any other, and the tick that container stands the room has a
    // Post of its own (`Atlas.postsIn` counts a built container's Seat in
    // every room, and only the Dual Seat half is the home room's) and
    // hires an Anchor to stand on it. That Anchor is the body this gate is
    // written for, and the trip it refuses is off a Post the colony is
    // already paid for and inside the room it digs in.
    //
    // And one exception over both gates, which is #205's whole change: a
    // container site **under the body's own feet, on its own Post**
    // (`Atlas.standsOnPostSite`). Both prohibitions above are about a
    // walk. ADR 0046 refuses a delivery because fifty energy carried by a
    // body holding six Work is one tick of spending against two of
    // commute; #157 refuses this site because the feeding tier leaves no
    // travel cost to pin an Anchor at its Post. Neither reaches a site the
    // body is standing on: there is no walk, the Post it would be pulled
    // off is the tile it is already on, and the energy it spends is the
    // twelve a tick it dug there. What the colony gets back is the way an
    // outpost container was ever raised in the first place — dig a
    // shovelful, build a shovelful, a few hundred ticks for 5,000 progress
    // off a rock that is producing nothing meanwhile — instead of the
    // worker row commuting a Seam and fifty tiles at fifty energy a trip,
    // which is thousands of ticks of lost income every time an invader
    // demolishes one.
    //
    // The exception reads geometry and never a row (ADR 0006), so it is
    // the same rule for the generalist that happens to be standing there,
    // for whom it changes nothing at all. It closes on its own: a Build
    // needs carried energy, and the only thing that fills this body is the
    // Harvest whose Work Area is that same tile (`Atlas.postsOf`, ADR
    // 0020) — so the pair alternates, Harvest until the store is full and
    // Build until it is empty, and the tick the container stands the site
    // is gone and the body is an ordinary garrison on an ordinary Post.
    | Build siteId ->
        has Work
        && creep.Energy > 0
        && (Atlas.standsOnPostSite atlas creep.Name siteId
            || (not (isStandingBody creep)
                && not (isFeedingSite view atlas siteId && Atlas.workHeavy atlas creep.Name)))
    // Repair leaves Upgrade's arm with ADR 0046's gate (a delivery, and a
    // standing body's Carry is one trip's worth), and the two stay
    // otherwise identical: a Work part and something to spend.
    | Repair _ -> has Work && creep.Energy > 0 && not (isStandingBody creep)
    // The one Task the whole row exists for, and so the one place the
    // gate above must not appear (ADR 0046): a standing body spends its
    // Work into the controller from where it stands.
    //
    // And the fifth gate, which is that sentence's other half (ADR 0048):
    // a Work-heavy body spends its Work into the controller only from
    // where it already stands, because it is the walk that is the loss.
    // ADR 0016 accepted one commute — "a full Anchor off-post matching
    // Upgrade once empties it and converges" — but there is no *once*:
    // every release puts the same
    // body back at this gate, so a hauler bumping it off its container, or
    // its source running dry, bought the walk out and the walk home again
    // and again, fifty energy at a time against a body whose Work idles
    // for the whole trip. The Dual Seat and the buffer-side row are
    // exactly the shapes this leaves standing (ADR 0020, ADR 0046): both
    // are already inside the Work Area, so what the gate refuses is the
    // walk and never the Task. Asked as `mayAct` over the same tiles the
    // Emitter acts from, so a body it admits is one that acts this tick
    // rather than one that would have to move first — the Reach included
    // (ADR 0033), because a tile a Threat has taken is not standing room.
    | Upgrade controllerId ->
        has Work
        && creep.Energy > 0
        && (not (Atlas.workHeavy atlas creep.Name)
            || Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
        // A standing body holds no commuting body (ADR 0046) and the
        // borrowed Upgrade is a commute across the Seam (#213): the lift
        // that sends the pioneers must not send the home upgraders after
        // them. Their own controller stays the one Task the row exists
        // for, ungated.
        && not (isBorrowedUpgrade view controllerId && isStandingBody creep)
    // Part arithmetic and nothing else (ADR 0006): a reservation is pushed
    // up by CLAIM parts, so a body without one can no more reserve than a
    // Work-less one can dig, and a body with one asks for no energy state
    // — a reserver carries nothing and spends nothing. The gate cuts both
    // ways and that is the whole of ADR 0042's pairing rule: every other
    // Task needs a Work part or a Carry part, so a `[Claim; Move]` body is
    // applicable to this Task and to Flee and to no other, and a colony
    // that cast one before this Task existed would have stood it on the
    // spawn for its whole 600-tick life.
    | Reserve _ -> has BodyPart.Claim
    // The same part arithmetic, for the same reason (ADR 0047): the
    // engine's `claimController` is a CLAIM part's act, and a claimer
    // carries nothing and spends nothing. The two Tasks are applicable to
    // exactly the same bodies, which is what lets one row cast for both —
    // and what makes the one-Task-per-controller rule in `planTasks` load
    // bearing rather than tidy: with both pooled for one controller,
    // nothing here could tell the Matcher which of them to hand over.
    | Claim _ -> has BodyPart.Claim
    // Flee asks for no part and no energy state, only for a creep that is
    // being shot at and can run (ADR 0033). A Work-heavy body is exempt: at
    // four to seven ticks a step an Anchor leaving its Post neither escapes
    // nor digs, and the answer for the Post is a rampart (ADR 0034) — which
    // is also why the tile under one is in no Reach. The Reach it stands
    // in is its own room's (#138): a Threat on its coordinate a room away
    // is not shooting at it.
    | Flee ->
        not (Atlas.workHeavy atlas creep.Name)
        && (match Atlas.creepTile atlas creep.Name with
            | Some tile -> Set.contains (RoomPos.pos tile) (Threats.reachIn threats tile.Room)
            | None -> false)

/// The action Intent a Task asks of a creep, or None for a Task with no
/// action: Flee is movement and nothing else (ADR 0033), and the Emitter
/// issues it none.
let private intentFor (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> Some(HarvestSource(creep.Name, sourceId))
    // The same Intent for a tombstone or a ruin as for a container (#167):
    // the engine's `withdraw` is one method over every store, so the
    // Intent's name is the only thing that says "structure" and the
    // Executor hands it whatever `getObjectById` answers with.
    | Withdraw storeId -> Some(WithdrawEnergyFromStructure(creep.Name, storeId))
    // The reflex's own Intent, issued for a creep that walked (#167): one
    // act, one vocabulary, whether the energy was underfoot already or was
    // the reason the creep came. Which is why an arriving picker spells it
    // twice and `decide` keeps one — this Task owns its own act, and the
    // reflex is what gives way.
    | Pickup pileId -> Some(PickupEnergy(creep.Name, pileId))
    | Refill structureId -> Some(TransferEnergyToStructure(creep.Name, structureId))
    | Build siteId -> Some(BuildSite(creep.Name, siteId))
    | Repair structureId -> Some(RepairStructure(creep.Name, structureId))
    | Upgrade controllerId -> Some(UpgradeController(creep.Name, controllerId))
    | Reserve controllerId -> Some(ReserveController(creep.Name, controllerId))
    | Claim controllerId -> Some(ClaimController(creep.Name, controllerId))
    | Flee -> None

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

/// The hard deadline on the controller's downgrade timer: half the
/// level's full timer. The engine refuses activateSafeMode once the
/// timer sinks below half minus 5,000 (its
/// CONTROLLER_DOWNGRADE_SAFEMODE_THRESHOLD grace), so escalating at half
/// keeps the safe-mode reflex fireable with the whole grace still banked
/// — a downgrade costs a level and zeroes the stock, so neither line is
/// ever approached (ADR 0007).
let private downgradeDeadline level = fullDowngradeTimer level / 2

/// The tier of work a Task belongs to, once its target is taken into
/// account (ADR 0010, ADR 0012, ADR 0023) — what the matcher ranks by.
type private Tier =
    /// Getting out of a Reach (ADR 0033): the one Task in it is Flee, and
    /// it sits above every other tier and above the downgrade deadline
    /// too, because no other work matters while a creep is being killed.
    | Safety
    /// Feeding the economy: Harvest, a container's Withdraw, the Refill of
    /// a spawn or an extension, Reserve, an **outpost** container site's
    /// Build (#157) and every site in a **nursery** (ADR 0047) — the flow
    /// the colony's reproduction runs on, and beside it ADR 0042's two
    /// switches on a third of that flow: the Reserve that decides how fast
    /// an outpost's rock gives, and the Build that decides whether the room
    /// is in the economy at all. The nursery's sites are the third switch
    /// and the deepest of them: the colony's reproduction is a spawn, and
    /// those are the sites a second one is waiting on.
    | Feeding
    /// The Storage's Withdraw (ADR 0023): the colony's stock as an
    /// intake, one tier below the source containers the flow fills. An
    /// empty creep empties those first and draws on the stock only when
    /// they are dry, so a stock standing beside the spawn never wins the
    /// travel-cost tie the containers have to win. Below the whole
    /// feeding tier, not just its Withdraws: there is no rank between a
    /// container's Withdraw and the spawn Refill it feeds, so a hungry
    /// spawn outbids a stock underfoot too and a part-loaded hauler
    /// delivers what it has rather than topping up first — the price of
    /// ordering the stock under the flow it must never outbid.
    | StockDraw
    /// Surplus work: a tower Refill (ADR 0010), Build, Repair and
    /// Upgrade. The colony feeds its own reproduction before its guns,
    /// and everything it merely spends energy on waits behind the flow.
    /// Build with the switches excepted — an outpost container's site and
    /// a nursery's, which decide whether income exists rather than
    /// spending it (`isFeedingSite`, #157, ADR 0047); every other site the
    /// colony ever has, its own container sites included, is surplus here.
    | Surplus
    /// The controller container's Refill (ADR 0012): a full creep beside
    /// the buffer sinks its load into the controller rather than dumping
    /// it back into the container it just drew from and orbiting in
    /// place, so the buffer is filled by bodies with no surplus work of
    /// their own.
    | UpgradeBuffer
    /// The Storage's Refill (ADR 0023): the colony's stock, deeper than
    /// every sink that spends. A load reaches it only when there is
    /// nowhere else at all to put it, the upgrade buffer included, so the
    /// stock never outbids the flow, however close beside the spawn it
    /// stands (ADR 0023's own motivating case).
    | Stock

/// The matcher's whole tier order, shallowest first — the one place the
/// ordering lives (ADR 0010, ADR 0012, ADR 0023): the flow is fed, then
/// the stock is drawn on, then surplus is spent, then whatever is left
/// sinks into the upgrade buffer, and what even the buffer cannot hold is
/// stocked. The stock's two roles sit on either side of the surplus work
/// the colony does between them. Exhaustive over Tier on purpose — a tier
/// this sequence forgets is a build error, not a Task that silently ranks
/// below every other. The downgrade deadline (ADR 0007) is the one thing
/// above the sequence rather than in it; `rank` carries it.
let private rankOfTier =
    function
    // One rank beneath `deadlineRank`'s -1, which is itself one beneath the
    // shallowest tier of work: a fleeing creep outbids even a controller
    // about to downgrade (ADR 0033).
    | Safety -> -2
    | Feeding -> 0
    | StockDraw -> 1
    | Surplus -> 2
    | UpgradeBuffer -> 3
    | Stock -> 4

/// The tier a Task sits in. Refill, Withdraw and Build are the three
/// Tasks whose tier layers by target (ADR 0010, ADR 0023, ADR 0042). Two
/// of the three read the layer off the projection's kind and nothing else
/// — the stock is recognised for what it is, never for where it stands;
/// the third, Build, is the one that asks where as well. On Refill: the Storage and the container are each
/// one projected kind, and the projection holds one kind per id, so those
/// two answers exclude each other by construction; a tower is read off the
/// Refillables census instead, which can overlap either. So the kind is
/// asked first — deepest answer first — and the census only of what the
/// kind leaves. Any container answers UpgradeBuffer, not the controller's
/// alone; it is the Planner that pools only the controller's (range 3 of
/// the controller, never a source container's tile), so no other container
/// reaches here. Everything the three tests miss is a spawn or an
/// extension: the flow. On Withdraw the layering is the one line: the
/// stock is drawn on a tier below every container, source and controller
/// alike. On Build the layering is by target too, and by the site's room:
/// `isFeedingSite` above, which is the room *and* the kind for an
/// outpost's container (`isOutpostContainerSite`) and the room alone for
/// every site in a nursery (`isNurserySite`), where the kind is never
/// asked.
///
/// Reads the Atlas because that room is a question only the projection's
/// id-to-room join answers.
let private tierOf (view: ColonyView) atlas task =
    match task with
    | Flee -> Safety
    | Harvest _ -> Feeding
    // **A decision made here, because nothing else made it.** ADR 0042 and
    // #116 both fix the reserver row's *casting* order — in front of the
    // Anchor, hauler and worker rows — and neither says a word about its
    // *matching* order, and `rankOfTier` is exhaustive on purpose, so a
    // tier had to be chosen. Reserve joins the feeding tier on the casting
    // order's own argument: every other row spends the colony's income,
    // this one decides whether that income is five a tick or ten, so it
    // ranks with the flow reproduction runs on and above everything that
    // merely spends it.
    //
    // The choice is nearly free today — the only body Reserve applies to
    // is a CLAIM body, and a CLAIM body applies to no other Task but Flee
    // — so what it really settles is two comparisons. Below Safety, so a
    // reserver being shot at runs (ADR 0033) instead of standing at a
    // controller to die; and above Surplus, so the day a body carries
    // CLAIM beside Work it holds the reservation before it spends on
    // anything.
    | Reserve _ -> Feeding
    // Beside the Reserve it replaces, and for a stronger form of the same
    // argument (ADR 0047): a reservation decides whether one room's income
    // is five a tick or ten, and a claim decides whether there is going to
    // be a second colony at all. Nothing the colony merely spends energy
    // on may outrank it, and Safety still does — a claimer being shot at
    // runs, and comes back to a Task that is still pooled, because a room
    // stays takeable while nobody else has taken it.
    | Claim _ -> Feeding
    | Withdraw storeId ->
        let kind = Map.tryFind storeId view.Spatial.TargetKinds

        if kind = Some(Structure BuiltKind.Storage) then
            StockDraw
        else
            Feeding
    // A pile is flow and not stock (#167): it is the haul cycle's energy
    // lying where it fell — an Anchor's overflow, a death drop — so it
    // feeds the colony on the tier the containers do, and which of the two
    // an empty carrier goes for is travel cost's call. Deliberately not a
    // tier of its own: a rank between a pile and a container would decide
    // that before the price was ever asked, and there is no rule saying
    // which of them should win. It is not the stock's tier either — a pile
    // decays at a thousandth a tick, so the one store the colony must
    // never leave standing is this one.
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
            Feeding
    // The switch ADR 0042 hangs a whole room on, ranked where a switch
    // belongs (#157). A standing container is what admits an outpost into
    // the economy; until one stands the room is in no quota, so building
    // it is not surplus work the colony does with spare energy — it
    // decides whether a third of the colony's income exists at all,
    // exactly as the Reserve above decides whether that income is five a
    // tick or ten. So it ranks with the flow and not with the spending.
    //
    // What the surplus reading actually cost, deployed: Build and Upgrade
    // share the surplus tier, so only travel cost separated them, and the
    // home controller is a few tiles from a loaded home worker while the
    // site is a Seam and fifty tiles away. Every worker upgraded, every
    // tick. The answer #150 wrote down — that the builder would be the
    // creep which walked out for the outpost's own Harvest and filled up
    // there — never happened either: the Storage's Withdraw is feeding
    // tier and underfoot, so a cross-Seam Harvest never won a creep, and
    // the reserver (#131) is a CLAIM body with no Work. The switch was
    // laid down and nothing could ever close it.
    //
    // Home Build is untouched and stays Surplus: a home site is placed by
    // the Layout out of a surplus the colony already has, and admits
    // nothing.
    //
    // ADR 0010's Refill layering is untouched, but the tier it is untouched
    // beside has grown, and the two consequences of that are written here
    // rather than assumed away — the ticket's prose claimed neither
    // happened, and both do. A spawn- or extension-feeding Refill shares
    // this tier, so nothing but cost separates the two: the nearer target
    // wins, which is the home one for a creep standing at home and the
    // site for one the cap has already parked in the outpost. The home
    // room feeds itself first through the creeps that are standing in it,
    // and not by any rule. A **tower** Refill is surplus and not feeding
    // (ADR 0010, the Refill arm above), and Repair is surplus too (ADR
    // 0034), so both now rank strictly below this one site rather than
    // race it on distance. That is ADR 0010's own sentence and not an
    // exception to it — a colony feeds its own reproduction before its
    // guns, and this Build is what a third of that reproduction's income
    // is waiting on — but it is a real change of behaviour under a raid on
    // the home room, and the answer for a raid is the stand-down (ADR
    // 0043, #136) and not a rank.
    //
    // And the same argument one question deeper for a **nursery**'s sites
    // (ADR 0047 decision 4): a container decides whether a room is in the
    // economy, and the spawn a human has placed in a room this colony has
    // already claimed decides whether there is going to be a second colony
    // at all. So the tier lifts there too — and lifts *every* site in that
    // room, not the container alone, because the site the whole nursery
    // exists for is one no rule of this colony's places (`isNurserySite`).
    //
    // The builder cap the ordinary outpost site rides does *not* ride here
    // (`taskCapacities`) — the argument for it is that a tenth body buys
    // nothing on a 5,000-progress container, and a nursery's spawn is
    // three times that with a whole colony waiting on it.
    //
    // So what this costs the mother is **not** bounded, and the bound is
    // named here because it is the thing a reader will look for. The
    // pioneers are what the mother *hired* for the job (`pioneerCount`,
    // `workforceTarget`); they are not a ceiling on who takes it, because
    // nothing in the Matcher hires a body to a Task by the row it was cast
    // for. While a site stands in the nursery every loaded Work-part body
    // in the colony outranks the home Upgrade for it and may cross, and
    // the home room's surplus Build and Repair go unheld for as long as
    // that lasts. That is the price ADR 0047 decision 4 was chosen at —
    // the mother's own surplus work stops while the child is raised — and
    // it is bounded in *time* by the site being finished and by nothing
    // else.
    | Build siteId when isFeedingSite view atlas siteId -> Feeding
    // A bootstrapped child's Upgrade, in the mother's pool (#213): the
    // tier the pioneers were hired for. Left in the surplus beside the
    // home Upgrade, travel cost — a Seam and fifty tiles against five —
    // kept every one of them at home, and the addend was three more home
    // upgraders. Feeding lifts it over the mother's own surplus work for
    // exactly `pioneerCount` bodies (`taskCapacities`), which is the hire
    // taking the job it was hired for and not the loaded fleet crossing.
    // The child's own tick pools the same target as its own controller,
    // where this arm does not fire.
    | Upgrade controllerId when
        isBorrowedUpgrade view controllerId
        && not (sitesPendingBeside view atlas controllerId)
        ->
        Feeding
    | Build _
    | Repair _
    | Upgrade _ -> Surplus

/// Whether the controller stands inside its downgrade deadline (ADR 0007).
let private insideDowngradeDeadline (view: ColonyView) =
    view.Controller
    |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

/// One rank above the shallowest tier: where the downgrade deadline puts
/// Upgrade (ADR 0007). Not a tier of its own — "never let it downgrade"
/// is an ordering imposed on the sequence, not a tier of work.
let private deadlineRank = -1

/// Matching tier between applicable tasks (lower wins): the rank of the
/// Task's tier. One exception: a controller inside the downgrade deadline
/// makes Upgrade the colony's most urgent work, outranking even the
/// feeding tier (ADR 0007).
///
/// **The colony's own controller and no other.** The deadline is read off
/// `ColonyView.Controller`, which is this colony's alone, and since ADR 0047
/// decision 4 the pool can hold a second Upgrade — a bootstrapped child's
/// (`planTasks`). Lifting that one on the mother's timer would send her
/// whole loaded fleet across the Seam on the tick her *own* controller was
/// closest to downgrading, which is the exact opposite of what ADR 0007's
/// escalation is for. The child's own colony escalates its own controller
/// on its own timer, in its own tick, off the same rule.
///
/// The id test sits inside the Upgrade arm rather than above the match:
/// this runs once per candidate pair the Matcher prices, and every other
/// Task would be paying for a read it never uses.
let private rank (view: ColonyView) atlas task =
    match task with
    | Upgrade id when
        insideDowngradeDeadline view
        && view.Controller |> Option.exists (fun c -> c.Id = id)
        ->
        deadlineRank
    | _ -> tierOf view atlas task |> rankOfTier

/// How many creeps the colony will have building outpost container sites
/// at once (#157) — a budget over all of them together and never a
/// per-site number, because the Planner places one site per unserved
/// outpost source and places them all on the same tick. A tunable;
/// `taskCapacities` below carries the argument for the number and for
/// how it is spread.
let private outpostContainerBuilders = 2

/// Concurrent-worker cap per task id; tasks absent from the map are
/// unbounded. Harvest is capped by its source's Seat count — a source the
/// projection does not place derives no cap, so behaviour without terrain
/// data is unchanged.
///
/// Reserve is capped at **one holder per controller** (ADR 0042): the ADR
/// casts one reserver per posted outpost — "two reservers at 4.33 energy a
/// tick buy three sources their second five" — and a reservation is a
/// single capped number one body's CLAIM parts are sized to hold, so a
/// second body on a controller the first already holds buys nothing while
/// the other outpost stays at five a tick. Nothing else in the pipeline
/// produces that: the quota (#131) counts bodies and not assignments, and
/// the matching key puts cost ahead of `load`, so two reservers standing
/// together are matched to the same nearest controller and the collapse is
/// silent — both report Matched. The cap is read off the pool rather than
/// re-deriving it, so which controllers are reserved is still said once,
/// in `planTasks`; and it counts holders at arrival like every other cap
/// (ADR 0026), so a reserver's successor is cast and matched while the
/// incumbent still holds the reservation.
///
/// The outpost container sites carry **`outpostContainerBuilders` between
/// them** (#157), the same mechanism the Reserve cap uses and for the
/// opposite reason: not that a second body buys nothing, but that a tenth
/// does. Now that these sites sit on the feeding tier they outbid the home
/// Upgrade for every loaded worker in the colony, and travel cost cannot
/// thin the crowd — every worker in one room is about equally far from a
/// site a Seam away, so without a cap the whole worker row walks out
/// together and the home room's surplus work stops for the fifty ticks
/// each of them spends crossing.
///
/// **A budget and not a per-site number**, which is the one place this cap
/// is not the Reserve cap's shape: a reservation is one per controller and
/// two of them are two separate jobs, but `planOutpostContainers` places a
/// site for *every* unserved outpost source and places them on the same
/// tick — the declaration carries three sources, so a per-site two is a
/// colony-wide six, which is the whole worker row and exactly what the
/// paragraph above says the cap exists to prevent. So the budget is spread
/// over the sites the pool actually holds, floored at one apiece: one site
/// takes two, and three sites take one each. Nothing waits on another's
/// completion — every switch is being closed — and as each site completes
/// the divisor falls and the survivors get the bodies back, so the last
/// one standing is built by the full two.
///
/// **Two is a tunable, and this is the reason for that number**: one is
/// the smallest crowd that builds, and two is the smallest that survives
/// losing a body. A container is 5,000 progress and a generalist carries
/// 50, so the site is many round trips deep whatever the crowd; a lone
/// holder that dies, expires or is released by a Reach (ADR 0033) leaves
/// the switch open for a whole cast-and-walk cycle, and the room stays
/// outside every quota for all of it — which is the loss the floor of one
/// accepts while several switches are open at once, and the reason the
/// budget is spread rather than spent on one site at a time. Beyond two
/// the marginal body buys the same fraction of a long build at the same
/// fifty-tile price, which is why the number is small rather than the Work
/// Area's own room. Raising it is a decision about how much of the home
/// room's surplus the colony will spend to close the switches sooner, and
/// nothing here breaks if it moves.
///
/// Read off the pool like the Reserve cap, so which site this is stays
/// said once (`isOutpostContainerSite`), and counted at arrival like every
/// other cap (ADR 0026).
///
/// **A Withdraw is capped by its store's stock** (#161), **and a Pickup by
/// its pile's** (#167): `ceil(stored / one drawer's load)`, the number of
/// bodies that store can actually fill. Nothing else in the pipeline says
/// it. The matching key puts cost
/// ahead of `load` (ADR 0002), so every hauler with a free slot picks the
/// *nearest* stocked container whatever is in it: a container holding 400
/// draws five haulers, four come home empty, and a full one on the far
/// side of the room stands unvisited. Crowding only decides ties, and
/// these are not ties.
///
/// ADR 0023 rejected *container stock as a matching weight* — an
/// amount-aware third dimension beside rank and travel cost — and this is
/// not that option: the key keeps its two dimensions and the stock enters
/// as a capacity, which is the counterweight ADR 0002's own Consequences
/// named for exactly this pile-on ("Seat caps (and future per-target
/// capacities) are the intended counterweight"). What the live room
/// refutes is only that ADR's reason for the rejection — that two tiers
/// already solve the crowding — and a tier cannot: both containers sit in
/// one, and inside a tier only cost speaks.
///
/// The load is the **row's** body and never the candidate's own carry: a
/// capacity is a fact about the Task, so one store must not answer two
/// numbers depending on which creep asked it. *Which* row is a fact about
/// the store, and so is read here too: ADR 0019 shuts every body with no
/// Work part out of the controller container, so the buffer's drawers are
/// the worker row and its stock divides by `workerBodyFor`'s carry, while
/// every other store belongs to the haul cycle and divides by the hauler
/// row's. Both are the body the colony would cast now — the richest bank,
/// the same reading `workforceTarget` prices every row's replacement at,
/// and so the largest body of that row the fleet holds and the tightest
/// cap the formula gives for it. Pricing the buffer by a hauler instead
/// would cap it on a body that can never draw from it: at an 1,800 bank
/// the cast hauler carries 1,200 and the cast worker 450, so 900 standing
/// in the buffer — two worker bodies' worth — would admit one and send the
/// other back to the rock, a cap 2.67x tighter than the store it claims
/// to describe. Two *worker* bodies and not two upgraders, and the
/// divisor is still the generalist's now that #187 has hired the first
/// standing body beside it — which is not what ADR 0046's Consequences
/// hand this ticket, and is written down rather than done quietly. Both
/// rows draw here: the upgrader row lives at this store and the worker
/// row's floor keeps one or two generalists in the colony (ADR 0046),
/// which ADR 0019's gate admits exactly as it always did. Divided by the
/// upgrader's fifty-energy Carry the same 900 admits eighteen drawers,
/// which is no cap at all — and the crowd a vanished cap lets pile on is
/// the generalists', the defect #161 put the cap here for. Which body a
/// two-row store divides by is a question this ticket does not carry, and
/// it goes back to the tracker with the reading above rather than being
/// settled in passing. `haulerQuota` divides this very load — the same
/// bank-capacity body, on purpose (ADR 0049) — into the same source
/// containers' flow, taking the minimum over the spawns to find each
/// container's cheapest sink; the two must agree, because a row sized
/// against one load and dispatched against another would hire bodies the
/// caps never admit.
///
/// Stock and never flow (ADR 0023): the ten a tick an Anchor drops into
/// the container while the hauler walks is counted on the tick it lands
/// and never anticipated here — so a store holding less than a load can
/// leave a hired hauler idle for the ticks it takes to fill, which is the
/// honest state ADR 0019 already chose over cycling a container, and the
/// stock climbs until the cap admits the second body. The Storage is
/// deliberately not special-cased — the same formula over a 130,000 stock
/// is a cap of hundreds, which is no cap at all — and neither is the
/// controller container: it is capped like every other store, only
/// divided by the row that actually draws from it. A stock of zero caps
/// at zero and not at unbounded; `planTasks` pools only stocked stores, so
/// the pool never asks, but a cap table that answered "no limit" to an
/// empty store would be the one reading of the formula that inverts it.
let private taskCapacities (view: ColonyView) atlas (tasks: Task list) : Map<string, int> =
    let seats =
        view.Sources
        |> List.choose (fun s ->
            Atlas.seats atlas s.Id |> Option.map (fun count -> taskId (Harvest s.Id), count))

    let reserves =
        tasks
        |> List.choose (function
            | Reserve _
            // One holder per controller for the Claim beside it (ADR
            // 0047), and here the second body buys even less than a second
            // reserver does: a room is claimed by one touch of one CLAIM
            // part, so a crowd at the controller is a crowd of bodies with
            // nothing to do the tick the first of them acts. The cap is
            // what sends the second claimer — the successor cast while the
            // incumbent still walks (ADR 0026) — to the *other* candidate
            // colony, since travel cost alone would send both to the
            // nearest.
            | Claim _ as task -> Some(taskId task, 1)
            | _ -> None)

    let draws =
        let capacity = view.Bank.Capacity
        let haulerLoad = carryCapacityOf (bodyFor haulerPattern capacity)
        let workerLoad = carryCapacityOf (workerBodyFor capacity)
        let buffers = Atlas.controllerContainers atlas

        tasks
        |> List.choose (function
            | Withdraw storeId as task ->
                let stock = view.Spatial.Stores |> Map.tryFind storeId |> Option.defaultValue 0

                let load =
                    if Set.contains storeId buffers then
                        workerLoad
                    else
                        haulerLoad

                Some(taskId task, ceilDiv stock load)
            // A pile is capped by the same arithmetic over the same table
            // (#167): as many bodies as the energy on the ground can
            // actually fill, and never the whole row onto one heap. The
            // divisor is the hauler row's with no branch — ADR 0019's
            // buffer is a container the Layout placed at the controller,
            // and a pile is not one, so the reading that picks a row by
            // store has one answer here.
            | Pickup pileId as task ->
                let amount = view.Spatial.Stores |> Map.tryFind pileId |> Option.defaultValue 0

                Some(taskId task, ceilDiv amount haulerLoad)
            | _ -> None)

    let outpostContainers =
        match
            tasks
            |> List.choose (function
                | Build siteId as task when isOutpostContainerSite view atlas siteId ->
                    Some(taskId task, isNurserySite view atlas siteId)
                | _ -> None)
        with
        | [] -> []
        | sites ->
            // A nursery's sites carry no cap at all (ADR 0047 decision 4),
            // and the exclusion is by **room**, so it takes the container
            // site the outpost rule places in that room with it. The
            // reason is the room's and not the spawn's 15,000 progress:
            // once the colony has claimed a room, everything standing in
            // it is the switch on whether there is a second colony, and
            // the budget is an argument about how much of the home room's
            // surplus work a *third of the income* is worth (#157) — a
            // different question, asked about a room the colony merely
            // mines.
            //
            // What the exclusion costs is written at `tierOf`: nothing
            // bounds the crowd that crosses for a nursery's site while it
            // stands. That is the decision, not an oversight, and the
            // pioneers are the bodies hired for the job rather than a
            // ceiling on who takes it.
            //
            // Excluded from the **entries** and not from the divisor. The
            // budget is spread over the outpost container sites the pool
            // holds, and it falls to the survivors only as sites are
            // *finished* (above); a nursery's site is still standing and
            // still drawing builders — more of them than ever — so
            // dropping it from the count would hand a sibling outpost's
            // site two builders where it had one, on the strength of a
            // third room being claimed. Which site this is stays said once
            // (`isOutpostContainerSite`); the nursery decides only which
            // of those sites an entry is emitted for.
            let each = outpostContainerBuilders / List.length sites |> max 1

            sites
            |> List.choose (fun (tid, nursery) -> if nursery then None else Some(tid, each))

    // A borrowed Upgrade takes the bodies hired for it and no more (#213):
    // `pioneerCount`, the same constant the worker row is raised by, so a
    // human retuning the hire retunes the lift with it. Without the cap
    // the feeding tier would send every loaded body across the Seam and
    // the mother's own surplus would stop — the nursery's accepted price
    // for a 15,000-progress spawn, and not one for an open-ended window.
    let borrowed =
        tasks
        |> List.choose (function
            | Upgrade controllerId as task when isBorrowedUpgrade view controllerId ->
                Some(taskId task, pioneerCount)
            // A bootstrapped child's site in the mother's pool, per site:
            // the same bodies that were hired for the room, on the site
            // that ends its window sooner than its controller does. The
            // child's own room reads no cap here — its own sites are its
            // own workers' to crowd.
            | Build siteId as task when
                isBootstrapRoom view (Atlas.targetRoom atlas siteId |> Option.defaultValue "")
                ->
                Some(taskId task, pioneerCount)
            | _ -> None)

    seats @ reserves @ draws @ outpostContainers @ borrowed |> Map.ofList

/// Concurrent Work-heavy-harvester cap per Harvest task id (ADR 0024): the
/// source's Post count, the standing room a heavy body actually has — its
/// Harvest Work Area is that source's Posts alone (ADR 0020), so the Seat
/// cap would admit garrisons to tiles they may not work from and pile two
/// Anchors onto one Post. A source with no Post derives no cap, and the
/// two rooms mean different things by that: at home nothing narrows a
/// heavy body's area (ADR 0020's pre-container fallback), so the Seat cap
/// is the only one; in an outpost that area is *empty* (#159), so the
/// reachability gate rejects the pair for every heavy body — `Unreachable`
/// and not `Inapplicable`, that area being read as a price and not as a
/// body — and a cap of zero would only be a second way to say so. The Seat
/// cap still rides there, as it does for the light bodies that may work
/// the rock, and it is consulted first: the cascade below asks capacity
/// before reachability. Rides beside the Seat cap rather than replacing it
/// — a Post is a capacity unit of its own, and both must hold.
let private postCapacities (view: ColonyView) atlas : Map<string, int> =
    view.Sources
    |> List.choose (fun s ->
        match Atlas.postsOf atlas s.Id |> Set.count with
        | 0 -> None
        | count -> Some(taskId (Harvest s.Id), count))
    |> Map.ofList

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position, and — for Harvest alone
/// — only while the source holds energy (ADR 0025). Anticipatory dispatch
/// and the occupancy surcharge (ADR 0008) both price a walk high enough to
/// land a creep a tick or two early, so the gate is what keeps the
/// engine's ERR_NOT_ENOUGH_RESOURCES spam structurally impossible. The
/// garrison gets no exemption here: it stays kept on its Post through the
/// window, silent, and digs the tick the energy lands. The tiles it may
/// act from are the ones it was judged applicable over — the Work Area
/// less this tick's Reach (ADR 0033) — so a creep no more works from a
/// tile a Threat took than it walks to one.
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
        | Flee -> false

    if
        Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task)
        && not drained
    then
        intentFor creep task |> Option.toList
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

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// Register one creep's Move Intent — every creep gets one (ADR 0001).
/// A creep travelling toward its Work Area wants exactly its next path
/// step; one already inside is force-registered "stay put, displaceable
/// within the Work Area"; one with no Task — or no way to reach its
/// area, which is just as immobilising — is parked: stay put,
/// displaceable to any adjacent walkable tile.
///
/// The displacement tiles — the parked creep's, and the ones a creep
/// inside its area may shuffle to — are its own room's
/// (`Atlas.adjacentWalkableIn`): the Resolver arbitrates each projected
/// room by itself (#145, ADR 0041's Consequences), so the room a creep is
/// filed under rides in beside its tile, and a creep in an outpost is
/// offered that room's ground and never home's.
///
/// **Every candidate list is the head it asked for and a tail after it**
/// (#219). A traveller heads its step and tails the ground that lies
/// beside both it and that step — a sidestep around the tile it wanted. A
/// creep inside its area heads its own tile, then the area's neighbours,
/// then the ground outside the area. Head and tail are read differently by
/// the arbitration below — the head is what the score pays a creep's whole
/// weight for, a tail is a detour worth the least positive thing and only
/// to the creep whose turn it is — so the order is the whole of the
/// preference and nothing else narrows the list.
///
/// The tail is what a lane needs (#219): a traveller whose only candidate
/// was its step stood still for as long as anything held that tile, and
/// with a queue behind each of them two loaded bodies tired every other
/// tick never both rested on the tick a swap needed — eight creeps stood
/// in W13S28's north corridor for ten minutes. It was already the Seam
/// branch's rule (#145), for the same reason on a different tile.
///
/// It is a sidestep and never a retreat, which is the half ADR 0008 keeps:
/// a traveller queued behind a merely fatigued creep has to wait in place
/// — grounding drains two a tick and is always transient — and a tail
/// holding the tile it came from would walk it backwards and forwards
/// every other tick instead. A stayer's out-of-area tail is the mirror of
/// the same rule: a body with no slack left inside its Work Area steps out
/// of it rather than walling the lane, and it prefers every tile inside
/// before it does.
///
/// One tile is never a candidate: the creep's own, when that tile is a
/// Seam (`Atlas.standsOnSeam`) — the border ring the engine put it down
/// on the tick it crossed (#142). The ring is no room's ground (ADR
/// 0036), and a creep that ends its tick on it is moved out of the room
/// by the engine again, so "stay put" there is not a wait but a bounce
/// across the border every other tick. A ring creep therefore walks
/// inward first (#145): parked, its candidates are the ground tiles
/// beside it alone; travelling, its step comes first and *every* other
/// ground tile beside it after — the ordinary tail's "beside the step too"
/// narrowing does not apply here, because the ring has no lane back and
/// every tile beside it is forward — so a contested step becomes a step
/// off the ring rather than a stay on it. Only when no ground lies beside
/// it at all is its own tile offered, so a Move Intent's candidates stay
/// non-empty and arbitration's own answer — stay where you stand — is the
/// answer it always was.
///
/// The Task goes to `Atlas.firstStep` beside the area, and that is what
/// gives a creep matched across a border somewhere to walk (#142): its
/// Work Area is empty here by construction — standing and acting are
/// in-room acts (ADR 0041's Consequences) — so without the Task this
/// creep would park on a Task it was priced for and never move, holding it
/// against anti-thrash for the rest of its life. The step it gets back is
/// the near side of the Seam the price won at, which is a tile of this
/// creep's own room, so arbitration is handed nothing it could not already
/// arbitrate.
let private moveIntentFor
    (rankOf: Task -> int)
    (threats: Threats)
    atlas
    (creep: string)
    (at: RoomPos)
    (task: Task option)
    : MoveIntent =
    // The room the creep stands in and the only room its candidates are
    // tiles of (#145): it rides on the tile now (ADR 0052 decision 2)
    // rather than beside it as a field of its own.
    let room = at.Room
    let pos = RoomPos.pos at
    let here = RoomPos.at room
    let beside = Atlas.adjacentWalkableIn atlas room pos
    let onSeam = Atlas.standsOnSeam atlas creep && not (List.isEmpty beside)

    // Where this creep may stay: its own tile, unless that tile is a Seam
    // with ground beside it to walk onto.
    let staying = if onSeam then [] else [ pos ]

    let parked rank =
        {
            Creep = creep
            Pos = at
            Rank = rank
            Candidates = staying @ beside |> List.map here
        }

    match task with
    | None -> parked idleRank
    | Some task ->
        // The area less this tick's Reach (ADR 0033): a creep works from
        // the safe half of its Work Area rather than abandoning the Task
        // because one corner is hot, and its steps go nowhere else.
        // Read against the set the Atlas already holds rather than a
        // narrowed copy of it: this runs once per creep and the tiles are
        // this room's either way.
        let area = areaFor threats atlas creep task

        if Set.contains (here pos) area then
            let inside, outside =
                beside |> List.partition (fun tile -> Set.contains (here tile) area)

            {
                Creep = creep
                Pos = at
                Rank = rankOf task
                Candidates = pos :: (inside @ outside) |> List.map here
            }
        else
            match Atlas.firstStep atlas creep task area |> Option.map RoomPos.pos with
            | Some step ->
                // The detours: the ground beside this creep that also lies
                // beside the step it asked for — a way *around* the tile it
                // wanted and never a way back down the lane it came up.
                // Both halves are load-bearing. Without the tail a creep
                // whose one candidate is held by a body that cannot move
                // stands still for as long as that body does, which is
                // #219's deadlock; with the whole neighbourhood in it, a
                // traveller queued behind a creep that is merely fatigued
                // would back away and return every other tick, and ADR
                // 0008's answer for that case — wait in place, grounding is
                // transient — is the right one and stays.
                //
                // Except on the ring, where that argument has nothing to
                // stand on: a creep the engine put down on a Seam has no
                // lane behind it to back down — every tile beside it is
                // this room's ground and every one of them is inward — and
                // standing still there is not a wait but a bounce back
                // across the border (#145). So a ring creep keeps the whole
                // neighbourhood as its tail, which is what the Seam branch
                // has offered since #145 and what R2b generalised rather
                // than narrowed.
                let tail =
                    if onSeam then
                        beside |> List.filter ((<>) step)
                    else
                        let around = Atlas.adjacentWalkableIn atlas room step |> Set.ofList
                        beside |> List.filter (fun tile -> Set.contains tile around)

                {
                    Creep = creep
                    Pos = at
                    Rank = rankOf task
                    Candidates = step :: tail |> List.map here
                }
            | None -> parked (rankOf task)

/// The push a rank carries into the arbitration's arithmetic. `Rank` stays
/// the fold's deterministic sort key below — a lexicographic order, in
/// which the shallower tier is offered the room first whatever it costs —
/// and this is the second reading the augmenting search needs: a *weight*,
/// so that a chain seating two bodies on the steps they asked for can
/// outweigh one body pushed off its own.
///
/// Positive and never rising with rank, so the ladder's own order carries
/// over (`rankOfTier`), and `idleRank` lands on the smallest weight there
/// is rather than on none: a body with no Task pushes with something, or a
/// crowd of idle bodies would be a wall no traveller could walk into.
///
/// The ceiling is the ladder's deepest tier and the `+ 2` is what keeps
/// that tier off the floor: `Stock` weighs two and the floor of one is
/// reserved for the bodies carrying no Task at all, so every Task outpushes
/// every idle body whatever tier it sits in.
let private weightOfRank (rank: int) : int = max 1 (rankOfTier Stock + 2 - rank)

/// The room's matching while the arbitration runs: a tile's holder and a
/// holder's tile, one relation written both ways because the search reads
/// it both ways — the tile to find whom to displace, the creep to find
/// what to vacate.
///
/// **Injective in both directions, by construction**: a creep's own entry
/// is rewritten by the assignment that moves it, and the tile it leaves is
/// rewritten by whoever displaced it off — the chain's initiator is the one
/// creep with nobody behind it, and its tile is emptied before the search
/// starts. So no tile holds two creeps and no creep holds two tiles. That
/// is what the
/// settle this replaced could not promise — its "nowhere left to stand"
/// fallback put a creep back on its own tile after another had claimed it,
/// and left the engine to fail whichever move contested it (#219).
type private Matching =
    {
        Holder: Map<RoomPos, string>
        Tile: Map<string, RoomPos>
    }

/// Resolver core: the room's Move Intents matched onto its tiles by a
/// weighted augmenting search (#219, #216 R2b). The algorithm is
/// sy-harabi's traffic manager, read and rewritten rather than linked —
/// that library is unlicensed, and it issues the engine's moves itself,
/// which ADR 0001 (movement is issued only inside a pure Resolver) and ADR
/// 0009 (Core returns Verdicts, it does not log) each refuse on their own.
/// What is deliberately not taken from it: its hash-shuffled candidate
/// order, because every tie in this bot falls to the lowest x then y and
/// the suite is written on that; its cost-matrix threshold, because a
/// crowd is priced here and never made impassable (ADR 0008); and its
/// free-tiles-before-occupied candidate order, which #216's own spec names
/// as the speed of it. That last one is incompatible with the head-and-tail
/// candidate list beside it: a traveller's tail is free ground by
/// construction, so trying free tiles first would take the sidestep before
/// ever asking whether the step it actually wants could be had. Preference
/// order is kept, and what it costs is nil — a room's whole pass is
/// O(creeps × 8) either way and the `pair` and `young` scenarios measure
/// flat against the revision this replaced.
///
/// The state starts as the identity — every creep holds the tile it stands
/// on — and the intents are offered one at a time in a deterministic order:
/// travellers before stayers (a stayer's claim can be honoured later by
/// shuffling or swapping it, but a stayer settled first walls off a
/// traveller's only path for the tick), then by rank, then by name. A creep
/// already holding its first candidate is left where it is; any other is
/// lifted off its tile and searched for an augmenting path.
///
/// A path is a chain of displacements ending on a free tile, and its
/// `score` is the chain's **net** priority: a creep landing on the
/// candidate it asked for first adds its rank's whole weight, the creep the
/// chain started from adds the smallest weight there is for landing on a
/// tail instead — a detour buys nobody their step, but a body that steps
/// aside is what empties a lane — a creep the chain merely shuffled out of
/// the way adds nothing, and a creep pushed off a step *it* had asked for
/// subtracts its own weight. A creep pushed off a tile it merely stands on
/// costs nothing, which is ADR 0001's essential rule ("a creep with slack
/// in its Work Area yields to a creep without") written as arithmetic.
/// Only a strictly positive chain is taken; a chain that dead-ends is
/// dropped whole, and dropping the Map the search returned is the whole of
/// the rollback.
///
/// Two kinds of tile are walls. Tiles in `blocked` — the fatigued creeps'
/// (ADR 0008 decision 1), and on the single-colony path the [[foreign
/// bodies]]' (#220) — are never matched onto and no chain runs through
/// them. So is any tile held by an occupant with no Move Intent of its own:
/// a creep that cannot step this tick can neither be displaced nor asked to
/// move, and the search finds no candidate for it and turns back.
///
/// What this buys over the claim-by-claim settle it replaces: chains of any
/// length with a rollback, where the settle displaced greedily and could
/// not undo a chain that dead-ended; a swap with no special case, since the
/// creep a chain starts from is lifted off its tile before the search and a
/// displaced creep's candidates already hold it; and an injective answer
/// (`RoomInvariantTests`, and the three-deep chain in `DecideTests` that is
/// the shape it can be lost at).
let private arbitrate
    (occupants: Map<RoomPos, string>)
    (blocked: Set<RoomPos>)
    (moveIntents: MoveIntent list)
    : Map<string, RoomPos> =
    let byCreep = moveIntents |> List.map (fun i -> i.Creep, i) |> Map.ofList

    let headOf (intent: MoveIntent) = List.tryHead intent.Candidates

    // A creep whose first candidate is the tile it stands on: it asked to
    // stay, and it is displaceable by anybody.
    let staying (intent: MoveIntent) = headOf intent = Some intent.Pos

    let place creep tile (m: Matching) =
        {
            Holder = Map.add tile creep m.Holder
            Tile = Map.add creep tile m.Tile
        }

    let vacate creep (m: Matching) =
        match Map.tryFind creep m.Tile with
        | Some tile ->
            {
                Holder = Map.remove tile m.Holder
                Tile = Map.remove creep m.Tile
            }
        | None -> m

    // What a creep gains by standing on `tile`, and what displacing an
    // occupant off it costs the chain.
    //
    // A tail tile is worth the smallest weight there is, and only to the
    // creep the search started from: that creep asked to move and could not
    // have the tile it asked for, and stepping aside is what empties a lane
    // (#219). To a creep the chain merely shuffled out of the way it is
    // worth nothing — being shuffled is a favour done to somebody else, and
    // paying for it would make evicting a creep off its own step profitable
    // by the difference, so whoever was offered the room last would always
    // take it from whoever was offered it first.
    let gain initiator (intent: MoveIntent) tile =
        if headOf intent = Some tile then weightOfRank intent.Rank
        elif initiator then 1
        else 0

    let cost (occupant: MoveIntent) tile =
        if headOf occupant = Some tile && not (staying occupant) then
            weightOfRank occupant.Rank
        else
            0

    // `visited` is threaded through and never rolled back, the one thing
    // the search borrows from its mutable original: a creep the chain has
    // already tried to rehouse is not tried again inside the same outer
    // search, which bounds the work at one expansion per creep per creep
    // rather than at every ordering of them.
    let rec augment
        (initiator: bool)
        (visited: Set<string>)
        (score: int)
        (intent: MoveIntent)
        (candidates: RoomPos list)
        (m: Matching)
        : Set<string> * (int * Matching) option =
        let visited = Set.add intent.Creep visited

        let rec walk visited tiles =
            match tiles with
            | [] -> visited, None
            | tile :: rest ->
                if Set.contains tile blocked then
                    walk visited rest
                else
                    let score = score + gain initiator intent tile

                    match Map.tryFind tile m.Holder with
                    | None ->
                        if score > 0 then
                            visited, Some(score, place intent.Creep tile m)
                        else
                            walk visited rest
                    | Some held when Set.contains held visited -> walk visited rest
                    | Some held ->
                        match Map.tryFind held byCreep with
                        | None -> walk visited rest
                        | Some occupant ->
                            // The occupant keeps its tile filed under its
                            // name while its own search runs, and `place`
                            // below overwrites that entry once the chain
                            // comes back. Emptying it first would be a
                            // claim this creep has already staked offered
                            // to the chain a second time: a creep deeper
                            // in it would read the tile as free, take it,
                            // and be overwritten here without ever being
                            // told — two bodies judged onto one tile. The
                            // occupant is in `visited` from its first
                            // line, so nothing deeper walks through it
                            // either.
                            let visited, outcome =
                                augment
                                    false
                                    visited
                                    (score - cost occupant tile)
                                    occupant
                                    (occupant.Candidates |> List.filter ((<>) tile))
                                    m

                            match outcome with
                            | Some(total, settled) when total > 0 ->
                                visited, Some(total, place intent.Creep tile settled)
                            | _ -> walk visited rest

        walk visited candidates

    // The identity matching: everybody standing in the room, whether or not
    // it registered an intent, so an occupant with none is a wall the search
    // finds by looking rather than a tile somebody has to remember to block.
    let start =
        {
            Holder = occupants
            Tile =
                occupants
                |> Map.toList
                |> List.map (fun (tile, creep) -> creep, tile)
                |> Map.ofList
        }

    let settled =
        (start, moveIntents |> List.sortBy (fun i -> staying i, i.Rank, i.Creep))
        ||> List.fold (fun m intent ->
            if Map.tryFind intent.Creep m.Tile = headOf intent then
                m
            else
                match
                    augment true Set.empty 0 intent intent.Candidates (vacate intent.Creep m)
                with
                | _, Some(_, settled) -> settled
                | _, None -> m)

    // The creeps that registered an intent and nobody else: what the pass
    // above reads back is each *rested* creep's settled tile, and a fatigued
    // occupant is answered for out of `Blocked` and `Occupants` instead.
    settled.Tile |> Map.filter (fun creep _ -> Map.containsKey creep byCreep)

/// Direction of a single step between adjacent tiles.
let private directionTo (from: Pos) (dest: Pos) : Direction option =
    match sign (dest.X - from.X), sign (dest.Y - from.Y) with
    | 0, -1 -> Some Top
    | 1, -1 -> Some TopRight
    | 1, 0 -> Some Right
    | 1, 1 -> Some BottomRight
    | 0, 1 -> Some Bottom
    | -1, 1 -> Some BottomLeft
    | -1, 0 -> Some Left
    | -1, -1 -> Some TopLeft
    | _ -> None

/// One room's arbitration, settled: what each of its rested creeps was
/// settled on and what it asked for first, and the fatigued creeps' tiles
/// and the occupants the settlement was made against — the four things
/// the Verdicts read back, all keyed on that room's tiles alone (#145).
type private RoomPass =
    {
        /// Each rested creep's settled standing tile.
        Standing: Map<string, RoomPos>
        /// Each rested creep's preferred standing tile: the head of its
        /// candidate list — a Move Intent's candidates are never empty.
        Preferences: Map<string, RoomPos>
        /// The tiles no intent in this pass may be settled onto and no
        /// chain may run through: the fatigued creeps' (ADR 0008) and the
        /// [[foreign bodies]]' that nobody in the fold holds (#220).
        Blocked: Set<RoomPos>
        /// Who stands where at tick start.
        Occupants: Map<RoomPos, string>
    }

/// Resolver, first half: one colony's Move Intents, unarbitrated. Every
/// rested creep the Atlas places registers one (ADR 0001); a fatigued creep
/// registers none — the engine would answer its move with ERR_TIRED — and
/// its tile is a wall for the tick, so nobody plans a step through it (ADR
/// 0008). Takes the tick's assigned Task per creep as data; a creep absent
/// from the map is idle.
///
/// Rerouted is settled here rather than in the pass, because it is the one
/// movement Verdict the arbitration does not answer: it compares this
/// creep's priced first step against the step the same body would take were
/// no tile occupied, which is a second flood on this colony's Atlas. It is
/// evidence that must be manufactured, so it is computed only for creeps on
/// the verbose list (ADR 0018), whose decision is about log noise and
/// stands now that the flood comes off the Atlas's shared memo (ADR 0030).
let movementOf
    (view: ColonyView)
    atlas
    (threats: Threats)
    (assigned: Map<string, Task>)
    (verbose: Set<string>)
    : Movement =
    let tired =
        view.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let placed = Atlas.placedCreeps atlas

    let rerouted name task =
        let area = areaFor threats atlas name task

        match
            Atlas.firstStep atlas name task area,
            Atlas.firstStepIgnoringTraffic atlas name task area
        with
        | Some priced, Some blind -> priced <> blind
        | _ -> false

    {
        Order = view.Creeps |> List.map (fun c -> c.Name)
        Placed = placed
        Tired = tired
        Foreign = view.Foreign
        Intents =
            placed
            |> List.filter (fun (name, _) -> not (Set.contains name tired))
            |> List.map (fun (name, at) ->
                moveIntentFor (rank view atlas) threats atlas name at (Map.tryFind name assigned))
        Rerouted =
            placed
            |> List.choose (fun (name, _) ->
                if Set.contains name verbose then
                    match Map.tryFind name assigned with
                    | Some task when rerouted name task -> Some name
                    | _ -> None
                else
                    None)
            |> Set.ofList
    }

/// Resolver, second half: the room passes, and the move Intents and
/// movement Verdicts they settle. One pass per room over **every** creep of
/// ours standing in it, whichever colony registered its intent (#216 R2b,
/// #220) — a room two colonies work is one room, and half its traffic
/// arbitrated against the other half read as empty is how a body ends up
/// claiming a tile the engine will never let it into.
///
/// Once per room, and never across two (#145): arbitrated movement is a
/// room's (ADR 0001, ADR 0008), and ADR 0041's Consequences keep it so —
/// geometry crosses the Seam and arbitration does not, decomposed strictly
/// per room as screeps-cartographer decomposes `reconcileTraffic`. Each
/// room's `occupants`, `blocked` and Move Intents are that room's alone,
/// and the tiles keying them carry the room they are in (ADR 0052 decision
/// 2) — which is what a union across rooms would otherwise cost: two
/// creeps standing on one coordinate of two rooms collapsing into one
/// occupant, and a fatigued outpost creep pre-claiming a home tile,
/// deleting a home creep's `MoveCreep` outright. The split is still made
/// by room here and not left to the keys, because the *arbitration* is a
/// room's whatever its tiles could say.
/// What is *not* arbitrated is the border tile: two creeps aiming at one
/// exit from its two sides are never checked against each other, which ADR
/// 0041 accepts in as many words.
///
/// What crosses the Seam is the *destination* (#142): a creep standing at
/// home and matched to an outpost's Task is arbitrated at home over a step
/// that is a home tile — the near side of the Seam it was priced at — and
/// the engine puts it down in the neighbour at the end of that tick. The
/// next tick the projection files it under the neighbour's name, and that
/// room's pass walks it on from its landing tile: the ring is not ground,
/// but a flood seeds its start tile regardless, so `Atlas.firstStep` steps
/// it off the ring onto the room's own floor exactly as it stepped the near
/// side onto the exit. Before #145 the far side was deferred and the creep
/// stood where it landed for the rest of its life; that gap was what #126
/// waited on.
///
/// The Verdicts ride beside the moves (ADR 0009), colony by colony and in
/// each colony's view creep order: grounded for a creep fatigue kept out;
/// rerouted for a traveller the occupancy surcharge detoured (settled by
/// `movementOf`); yielded — naming the counterpart holding the tile — for a
/// creep settled off the candidate it asked for first; and stalled for one
/// that was settled off it by a holder this colony cannot name. That last
/// is the silence #219 recorded: a rested traveller that simply fails to
/// move said nothing at all, exactly when a timeline is worth reading, and
/// the holder it lost to is nameless precisely in the case that matters —
/// a [[foreign body]] on the tile, or a wall the fold could not attribute.
/// A creep that simply steps toward its Work Area, a clean swap included,
/// says nothing: both sides of a swap settle on the tile they asked for.
let resolveRooms (movements: Movement list) : Intent list * Verdict list =
    let tired =
        (Set.empty, movements) ||> List.fold (fun acc m -> Set.union acc m.Tired)

    let everywhere = movements |> List.collect (fun m -> m.Placed)

    let passOf =
        everywhere
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room ->
            let here = everywhere |> List.filter (fun (_, at) -> at.Room = room)
            let occupants = here |> List.map (fun (name, at) -> at, name) |> Map.ofList

            // The bodies no intent in this fold can move: the fatigued, and
            // the foreign tiles nobody standing here answered for.
            let foreign =
                (Set.empty, movements)
                ||> List.fold (fun acc m ->
                    Set.union acc (m.Foreign |> Set.filter (fun tile -> tile.Room = room)))
                |> Set.filter (fun tile -> not (Map.containsKey tile occupants))

            let blocked =
                here
                |> List.choose (fun (name, at) ->
                    if Set.contains name tired then Some at else None)
                |> Set.ofList
                |> Set.union foreign

            let moveIntents =
                movements
                |> List.collect (fun m -> m.Intents |> List.filter (fun i -> i.Pos.Room = room))

            room,
            {
                Standing = arbitrate occupants blocked moveIntents
                Preferences =
                    moveIntents
                    |> List.map (fun i -> i.Creep, List.head i.Candidates)
                    |> Map.ofList
                Blocked = blocked
                Occupants = occupants
            })
        |> Map.ofList

    // Every placed creep beside its room's pass, colony by colony and in
    // each colony's own view creep order: the order the Intents and
    // Verdicts leave in.
    let rows =
        movements
        |> List.collect (fun m ->
            let placed = m.Placed |> Map.ofList

            m.Order
            |> List.choose (fun name ->
                Map.tryFind name placed
                |> Option.map (fun at -> m, name, at, Map.find at.Room passOf)))

    let intents =
        rows
        |> List.choose (fun (_, name, at, pass) ->
            Map.tryFind name pass.Standing
            |> Option.bind (fun settled -> directionTo (RoomPos.pos at) (RoomPos.pos settled))
            |> Option.map (fun direction -> MoveCreep(name, direction)))

    // Who holds a tile this creep did not get, in this creep's room: the
    // creep settled on it, or the fatigued occupant whose blocked tile
    // pre-claimed it. A foreign body's tile is blocked and has no occupant
    // here, which is what leaves a creep stalled rather than yielded.
    let counterpartAt (pass: RoomPass) tile self =
        pass.Standing
        |> Map.tryPick (fun name settled ->
            if settled = tile && name <> self then Some name else None)
        |> Option.orElse (
            if Set.contains tile pass.Blocked then
                Map.tryFind tile pass.Occupants
            else
                None
        )

    let verdicts =
        rows
        |> List.collect (fun (movement, name, _, pass) ->
            if Set.contains name tired then
                [ Verdict.Grounded name ]
            else
                let reroute =
                    if Set.contains name movement.Rerouted then
                        [ Verdict.Rerouted name ]
                    else
                        []

                let yielded =
                    match Map.tryFind name pass.Preferences, Map.tryFind name pass.Standing with
                    | Some preferred, Some settled when settled <> preferred ->
                        match counterpartAt pass preferred name with
                        | Some other -> [ Verdict.Yielded(name, other) ]
                        | None -> [ Verdict.Stalled name ]
                    | _ -> []

                reroute @ yielded)

    intents, verdicts

/// Resolver, single colony: this colony's movement, arbitrated against
/// nobody else's. The seam the suite drives and the answer a tick with one
/// colony working the room gets from `resolveRooms` anyway — with one
/// difference that is the whole of #220's small version: with no other
/// colony's intents in the fold, the [[foreign bodies]] standing in these
/// rooms are walls rather than creeps, so a body this colony cannot move is
/// a tile it cannot claim.
let resolve
    (view: ColonyView)
    atlas
    (threats: Threats)
    (assigned: Map<string, Task>)
    (verbose: Set<string>)
    : Intent list * Verdict list =
    resolveRooms [ movementOf view atlas threats assigned verbose ]

/// Matcher: keep still-valid assignments (anti-thrash) and greedily assign
/// the rest. Assignments in, Assignments and the Verdicts explaining them
/// out (ADR 0009): releases first in memory order, then one status Verdict
/// per living creep in view order — each preceded, for a creep on the
/// verbose list, by its Scoring Verdict: the whole pool judged against the
/// same state its status was decided from. Emission belongs to the
/// Emitter, movement to the Resolver.
let matchCreeps
    (view: ColonyView)
    atlas
    (threats: Threats)
    (tasks: Task list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : Assignments * Verdict list =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList
    let capacities = taskCapacities view atlas tasks
    let postCaps = postCapacities view atlas

    // Each living creep's remaining life, hoisted for the tick as the two
    // cap tables are: the capacity gate asks it once per holder per judged
    // pair, and the answer is a view fact.
    let lives = view.Creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    // The crowding component of the matching key (ADR 0002): every holder,
    // counted at this tick. Arrival discounts what a Task's cap counts
    // (ADR 0026), never what the key does — spreading creeps over Tasks is
    // a judgement about now, and nothing in 0026 revises the key. The
    // count is a fold: the question is how many, and a Map built only to
    // be measured and dropped costs an allocation and a structural insert
    // per holder, once per scored pair.
    let load (acc: Assignments) tid =
        acc |> Map.fold (fun n _ assigned -> if assigned = tid then n + 1 else n) 0

    // The holders a candidate actually competes with, counted at arrival
    // (ADR 0026): two creeps hold the same standing room against each
    // other only while both are standing on it, so a holder counts against
    // a candidate exactly when their two stays overlap. A holder dead
    // before the candidate arrives has left the tile — which is what lets
    // a successor leave the spawn while its predecessor still digs — and a
    // holder still walking when the candidate dies never reaches it. The
    // window is read from both ends, so the fold's creep-name order cannot
    // decide which of a pair keeps the Task: ADR 0026 promises that for a
    // succession, and a one-ended window would have kept it only there,
    // letting a candidate a whole spawn-and-walk away evict a garrison
    // that is nowhere near its own lead. A walk the Atlas cannot price has
    // no arrival, and counts from now.
    let overlaps (candidate: CreepInfo) task arrival name =
        let alive =
            match arrival with
            | None -> true
            | Some ticks -> Map.tryFind name lives |> Option.forall (fun life -> life >= ticks)

        let arrived =
            match Atlas.walkTicks atlas name task with
            | None -> true
            | Some ticks -> ticks <= candidate.TicksToLive

        alive && arrived

    let holdersAt (acc: Assignments) (candidate: CreepInfo) task arrival =
        let tid = taskId task

        acc
        |> Map.toList
        |> List.choose (fun (name, assigned) ->
            if assigned = tid && overlaps candidate task arrival name then
                Some name
            else
                None)

    // The garrison of a Post whose container is still a site (#205),
    // counted against that source's Post cap beside the Harvest holders
    // above. On a standing container the two counts are the same set: the
    // overflow reprieve keeps the garrison's Harvest applicable through a
    // full store, so it never lets the slot go. On a site there is no
    // overflow, so the pair alternates — Harvest until the store is full,
    // Build until it is empty — and a cap counting assignments alone reads
    // the tile as free on every build tick. What that admits is a second
    // heavy body onto the one tile the first is standing on, and the
    // incumbent is then released from Build with its own Harvest full and
    // walks a Seam home to another rock: the cross-room theft ADR 0045
    // records, out of the Post that is supposed to prevent it.
    //
    // So the slot is held by the body *standing* on the Post, whatever
    // Task it holds this tick — "one Anchor per Post" (ADR 0012, ADR 0024)
    // is a claim about the tile. Counted at arrival like every other
    // holder (ADR 0026), so a garrison that dies before the candidate
    // arrives is no longer standing there and its successor is dispatched
    // exactly as it was. The candidate never counts against itself: a body
    // already on its own Post is what the cap is *for*.
    let postSiteGarrisons (candidate: CreepInfo) task arrival sourceId =
        view.Creeps
        |> List.filter (fun c ->
            c.Name <> candidate.Name
            && Atlas.workHeavy atlas c.Name
            && Atlas.standsOnSitePost atlas c.Name sourceId
            && overlaps candidate task arrival c.Name)
        |> List.length

    // A heavy body is judged against both caps (ADR 0024): the Seat count
    // it shares with every other harvester, and the Post count only its own
    // kind competes for. The Post cap is Harvest's alone; Reserve carries
    // the Seat-shaped one at a count of one (ADR 0042) and no Post cap at
    // all, since no heavy body ever holds a CLAIM part. Withdraw carries a
    // Seat-shaped one too, off its store's stock (#161). Holders are
    // gathered inside the capped arms — the Refills and the surplus work
    // the pool is mostly made of never walk the assignment map.
    let hasCapacity (creep: CreepInfo) acc task (arrival: Lazy<int option>) =
        let tid = taskId task

        // One Task-shaped cap does not reach one body (#205): the outpost
        // container budget is `outpostContainerBuilders` spread over the
        // sites, and every word of its argument is about a commute — "the
        // whole worker row walks out together and the home room's surplus
        // work stops for the fifty ticks each of them spends crossing"
        // (`taskCapacities`). The body standing on the site costs the home
        // room neither a walk nor a surplus tick, so it is outside what
        // that number prices; counted inside it, a worker still crossing
        // the Seam holds the slot for the whole fifty and the garrison
        // stands full on the progress with Harvest shut behind it and no
        // Task at all — #197's shape in the case this ticket exists to
        // give an exit to. The budget still bounds the commuters exactly
        // as #157 wrote it: what leaves is one body that was never a
        // commuter.
        let seatCap =
            match task with
            | Build siteId when Atlas.standsOnPostSite atlas creep.Name siteId -> None
            | _ -> Map.tryFind tid capacities

        let heavy = Atlas.workHeavy atlas creep.Name

        let postCap = if heavy then Map.tryFind tid postCaps else None

        // The light body's own cap (ADR 0051): the Seats a posted source
        // has *beyond* its Posts. The Post cap above keeps the garrisons
        // to the Posts; this keeps the light crowd off them, so a source
        // with two Seats and one Post admits one light body and one
        // heavy, never two light and an Anchor `none-free` in the swamp.
        // Harvest's alone, as the Post cap is, and read only where a Post
        // cap exists: an unposted source keeps the Seat cap as its only
        // one, which is ADR 0045's bare-Seat bootstrap unchanged.
        let lightCap =
            match task, seatCap, Map.tryFind tid postCaps with
            | Harvest _, Some seatCount, Some postCount when not heavy ->
                Some(max 0 (seatCount - postCount))
            | _ -> None

        match seatCap, postCap, lightCap with
        // Only a capped Task forces the walk: the Refills and the surplus
        // work the pool is mostly made of neither walk the assignment map
        // nor pay for an arrival (ADR 0029).
        | None, None, None -> true
        | _ ->
            let holders = holdersAt acc creep task arrival.Value

            let withinSeats =
                match seatCap with
                | Some cap -> List.length holders < cap
                | None -> true

            let withinLight =
                match lightCap with
                | Some cap ->
                    let light = holders |> List.filter (Atlas.workHeavy atlas >> not) |> List.length

                    light < cap
                | None -> true

            let withinPosts =
                match postCap with
                | Some cap ->
                    let heavy = holders |> List.filter (Atlas.workHeavy atlas) |> List.length

                    // The Post cap is Harvest's alone, so no other Task
                    // has a source to read a garrison off.
                    let garrison =
                        match task with
                        | Harvest sourceId -> postSiteGarrisons creep task arrival.Value sourceId
                        | _ -> 0

                    heavy + garrison < cap
                | None -> true

            withinSeats && withinPosts && withinLight

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed (e.g. across a redeploy). So does
    // reachability: a Work Area the Atlas can no longer reach releases the
    // assignment, freeing its capacity for creeps that can get there —
    // deliberately with no range-based fallback (ADR 0002). So does the
    // arrival gate: a drained source's Harvest whose wait the holder's
    // walk no longer covers releases it (ADR 0025) — the same behaviour
    // ADR 0013 got from the Task vanishing, now under its own reason. Each
    // failed gate names the release; a dead creep's assignment drops
    // silently — Verdicts attribute to living creeps only.
    // One exemption, ADR 0026's own: an expiring creep is never released
    // over capacity. Its successor is cast and matched while it still
    // works, and where the two do overlap on the tile — a lead longer than
    // the successor's walk — the exemption is what keeps the fold's
    // creep-name order from deciding which of them holds the Post.
    let kept, released =
        ((Map.empty, []), assignments)
        ||> Map.fold (fun (acc, released) name tid ->
            let release reason =
                acc, Verdict.Released(name, tid, reason) :: released

            match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
            | None -> acc, released
            | Some creep ->
                match Map.tryFind tid byId with
                | None -> release ReleaseReason.TaskGone
                // The raid's release stands ahead of the ordinary one: a
                // Task whose whole Work Area is in a Reach is gone for this
                // creep however well its body fits (ADR 0033).
                | Some task when threatened threats atlas creep task ->
                    release ReleaseReason.Threatened
                | Some task when not (applicable view threats atlas creep task) ->
                    release ReleaseReason.Inapplicable
                | Some task ->
                    // The walk is bound one gate before it is spent, exactly
                    // as the fresh cascade below binds it: capacity counts
                    // holders at this holder's own arrival (ADR 0026), and
                    // the arrival gate spends the very same number — priced
                    // at most once, and only if one of the two asks.
                    // Reachability is still travel cost's answer — the two
                    // floods reach the same tiles, and ADR 0002's release
                    // has no other price.
                    let cost = travelCostOf threats atlas creep.Name task
                    let arrival = lazy (Atlas.walkTicks atlas creep.Name task)

                    if
                        not (hasCapacity creep acc task arrival) && not (expiring view atlas creep)
                    then
                        release ReleaseReason.OverCapacity
                    else
                        match cost with
                        | None -> release ReleaseReason.Unreachable
                        | Some _ ->
                            match tooEarly view atlas creep task arrival with
                            | Some(walk, wait) -> release (ReleaseReason.TooEarly(walk, wait))
                            | None -> Map.add name tid acc, released)

    // One gate cascade judges every (creep, Task) pair — rejected at the
    // first matching gate it fails (applicable, capacity, reachable, in
    // time) or scored on the full key when none does. Two numbers are bound
    // above the capacity gate: the travel cost, which the reachability gate
    // and the scored key both read, and the walk, which capacity counts
    // holders at (ADR 0026) and the arrival gate spends after it — one
    // number for both, priced at most once. The walk is bound rather than
    // priced because most of the pool asks for neither gate, and pricing
    // one per pair regardless measured at a third of the tick (ADR 0029
    // split the pair; the cascade did not move). The order of the gates is
    // unchanged by that: a candidate the Atlas cannot price has no arrival
    // to count holders at, every holder counts against it, and it reports
    // the rejection it always did. The Matcher's candidates and a verbose
    // Scoring both read from here, so the narration can never drift from
    // what actually decided the match.
    let judge acc (creep: CreepInfo) task =
        let tid = taskId task

        if threatened threats atlas creep task then
            Candidate.Rejected(tid, RejectReason.Threatened)
        elif not (applicable view threats atlas creep task) then
            Candidate.Rejected(tid, RejectReason.Inapplicable)
        else
            let cost = travelCostOf threats atlas creep.Name task
            let arrival = lazy (Atlas.walkTicks atlas creep.Name task)

            if not (hasCapacity creep acc task arrival) then
                Candidate.Rejected(tid, RejectReason.CapacityFull)
            else
                match cost with
                | None -> Candidate.Rejected(tid, RejectReason.Unreachable)
                | Some cost ->
                    match tooEarly view atlas creep task arrival with
                    | Some(walk, wait) -> Candidate.Rejected(tid, RejectReason.TooEarly(walk, wait))
                    | None -> Candidate.Scored(tid, rank view atlas task, cost, load acc tid)

    let assignOne (acc, verdicts) (creep: CreepInfo) =
        let verdicts =
            if Set.contains creep.Name verbose then
                // The creep's own claim is set aside for its scoring: a held
                // single-Seat Task must read as the winning row, never as
                // capacity-full against its own holder's seat.
                let rows = tasks |> List.map (judge (Map.remove creep.Name acc) creep)
                Verdict.Scoring(creep.Name, rows) :: verdicts
            else
                verdicts

        match Map.tryFind creep.Name acc with
        | Some tid -> acc, Verdict.Kept(creep.Name, tid) :: verdicts
        | None ->
            let judged = tasks |> List.map (fun t -> t, judge acc creep t)

            let keyed =
                judged
                |> List.choose (function
                    | t, Candidate.Scored(_, rank, cost, load) -> Some((rank, cost, load), t)
                    | _ -> None)

            match keyed with
            | [] ->
                // How far the best Task got through the gates — applicable,
                // capacity, reachable, in time — is why the creep sits
                // idle, deepest gate first: a creep whose only rejection is
                // the arrival gate is waiting out a restock (ADR 0025), and
                // saying nothing fit its body would be the same lie the
                // rejection reason refuses.
                let rejectedWith wanted =
                    judged
                    |> List.exists (function
                        | _, Candidate.Rejected(_, reason) -> wanted reason
                        | _ -> false)

                // The arrival gate's reason carries the numbers it compared
                // (#88), so the depth question asks after the case rather
                // than after a value it would have to invent to compare to.
                let isTooEarly =
                    function
                    | RejectReason.TooEarly _ -> true
                    | _ -> false

                let reason =
                    if List.isEmpty tasks then
                        IdleReason.NoTasks
                    elif rejectedWith isTooEarly then
                        IdleReason.NoneInTime
                    elif rejectedWith ((=) RejectReason.Unreachable) then
                        IdleReason.NoneReachable
                    elif rejectedWith ((=) RejectReason.CapacityFull) then
                        IdleReason.NoneFree
                    else
                        IdleReason.NoneApplicable

                acc, Verdict.Unassigned(creep.Name, reason) :: verdicts
            | keyed ->
                let bestKey, task = keyed |> List.minBy fst

                // The deciding factor: the first component separating the
                // winner from its closest rival, or the pool-order tie-break
                // when the whole key ties.
                let factor =
                    match keyed |> List.filter (fun (_, t) -> t <> task) with
                    | [] -> MatchFactor.OnlyCandidate
                    | rivals ->
                        let bestRank, bestCost, bestLoad = bestKey
                        let rivalRank, rivalCost, rivalLoad = rivals |> List.map fst |> List.min

                        if rivalRank <> bestRank then MatchFactor.Rank
                        elif rivalCost <> bestCost then MatchFactor.TravelCost
                        elif rivalLoad <> bestLoad then MatchFactor.Load
                        else MatchFactor.PoolOrder

                Map.add creep.Name (taskId task) acc,
                Verdict.Matched(creep.Name, taskId task, factor) :: verdicts

    let next, statuses = view.Creeps |> List.fold assignOne (kept, [])
    next, List.rev released @ List.rev statuses

/// Join the Matcher's Assignments back onto the Planner's pool: the tick's
/// assigned Task per creep, as data for the Emitter and the Resolver.
let private assignedTasks (tasks: Task list) (assignments: Assignments) : Map<string, Task> =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> Map.tryFind tid byId |> Option.map (fun t -> name, t))
    |> Map.ofList

/// The census signature (ADR 0017): a string over exactly the inputs the
/// census-derived plans read — the (kind, position) census of standing
/// structures, the (kind, position) census of pending sites, the
/// controller level, the home room's name, and who holds each room the
/// projection carries. Any one input moving moves the signature;
/// everything else a view carries —
/// creeps, stores, hits, dropped piles, hostiles, banked energy, the
/// tick — is invisible to it.
///
/// The hauler quota rides the same signature on two load-bearing
/// derivations, and both are covered here rather than assumed. The
/// bank Capacity it sizes bodies from is the engine's
/// energyCapacityAvailable — a function of the standing spawn/extension
/// census and the controller level, both signed above. Since ADR 0042 it
/// also prices each container at its source's own output, which is read
/// off `RoomControl` — a **vision** fact and not a census one, so it is
/// signed explicitly: without it a lapsed reservation would leave the
/// signature byte-identical and the memo would hand back a quota sized
/// for the held rate, which is exactly the signature gap ADR 0017 names
/// as its failure mode. It is signed as the *rate* and never as the
/// reservation's `TicksToEnd`, which decays every tick and would throw
/// the Layout and the walk table away on every one of them.
///
/// **It signs every projected room, and this is the tick it widened to.**
/// ADR 0041 narrowed it to the home layer while everything the memo
/// carried was that one room's; ADR 0042's hauler quota is the entry that
/// reads a second one, folding the containers of *every* room the
/// projection carries and pricing each at the rate its own room is held
/// at. Both halves of that reading are signed here, and the widening is a
/// deliberate change to `PlanMemo.Signature` (#116's forward note), not a
/// break of any byte-for-byte promise:
///
/// - **The standing census spans every room, and names the room in the
///   entry.** Two rooms hold the same coordinates, so `Container@16,44`
///   in either would otherwise be the same census entry, and an outpost
///   container standing up would leave the string untouched while the
///   quota it hires moved. Every *kind* and not the containers alone,
///   because the quota reads more of an outpost than its containers: the
///   round trip it prices them by floods that room's step-weight grid,
///   and `World.ofGame` lays `Roads` and `Obstacles` out of
///   the same every-owner `findStructures` array the kind census comes
///   from. A road paved along the haul lane makes the trip cheaper and
///   an invader core standing on it makes it dearer or impossible, so a
///   census narrowed to `Container` outside home would hand back a quota
///   priced on a grid that has moved — ADR 0017's signature gap, in the
///   one room it can still happen in. The price of signing them whole is
///   a Layout and walk-table recompute on a hostile structure appearing
///   or decaying in an outpost, which is rare and is bounded by the
///   profile figures the outpost container plan records.
/// - **The pending census spans every projected room too**, and this is
///   the tick that half widened (#169). It was the home layer's alone
///   while nothing the memo carried read a site outside it — the Layout
///   is anchored at home, and the outpost's own container plan is derived
///   fresh every tick and rides no memo entry at all (ADR 0042). The
///   walk table's far leg reads one: it floods the *goal* room's grid,
///   and `World.ofGame` closes a tile under an obstacle-kind
///   construction site in whatever room it stands in — the engine refuses
///   a creep its own site everywhere — so a site outside home moves a
///   grid the memo holds an answer off. Left unsigned it would be ADR
///   0017's signature gap in that room: a lead recalled through ground
///   the successor cannot cross, for the life of the census. Both halves
///   name the room, so the entries stay one format, and a room's whole
///   contribution to the string is now read the one way.
/// - **The held rate is signed per projected room**, not for home alone.
///   Which rooms the quota reaches is itself derived from the census —
///   whichever ones place a source container this tick — so a signature
///   that tried to sign only those would have to run the fold it exists
///   to guard. The projected rooms are the rooms a container can be
///   folded out of at all, which makes them the honest key set, and a
///   room whose rate moves while it holds no container costs one Layout
///   recompute: a reservation is won or lost rarely, and a wrong quota
///   handed back is wrong every tick until one does.
///
/// The room name stays out front as well as inside the entries, because
/// it is `SpatialInfo.homeName` that decides which layer the Layout is
/// anchored in and which grid the spawn walks flood (ADR 0032) — a
/// projection that renamed its home room while carrying the same geometry
/// moves both, and the entries alone would not say so.
let censusSignature (view: ColonyView) : string =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial

    // One join for both halves since #169: a target is read wherever the
    // projection places it, standing or pending alike, because both halves
    // move a grid the memo holds an answer off.
    let census select =
        spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            select kind
            |> Option.bind (fun (built: BuiltKind) ->
                SpatialInfo.placementOf spatial id
                |> Option.map (fun tile -> $"{built}@{tile.Room}:{tile.X},{tile.Y}")))
        |> List.sort
        |> String.concat ";"

    let standing =
        census (function
            | Structure kind -> Some kind
            | _ -> None)

    let pending =
        census (function
            | Site kind -> Some kind
            | _ -> None)

    let level =
        view.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    // The rate each projected room's sources are priced at this tick, in
    // room-name order, and the empty rate for a room vision answered for
    // not at all — the third answer the quota gives, and a different one
    // from either rate (ADR 0004). The room is named beside its rate so a
    // room joining or leaving the projection moves the string even when
    // the rates it carries happen to line up.
    let held =
        spatial.Rooms
        |> Map.toList
        |> List.map (fun (room, _) ->
            let rate =
                Map.tryFind room view.RoomControl
                |> Option.map (heldRateOf >> string)
                |> Option.defaultValue ""

            $"{room}:{rate}")
        |> String.concat ","

    $"{home}|{level}|{held}|{standing}|{pending}"

/// The decision seam: a colony view in — with the verbose list of creep names
/// owed the manufactured-evidence Verdicts (full candidate scoring, reroute
/// attribution) and the previous tick's plan memo — Decision out, with this
/// colony's movement left unarbitrated on it (#216 R2b). A room's traffic
/// is not one colony's decision, so the last step of the pipeline is not
/// taken here: what comes out is the room's Move Intents, and the caller
/// folds every colony's together and arbitrates each room once
/// (`resolveRooms`). A shell with one colony gets the whole answer from
/// `decide` below, which is that fold over this one Movement.
/// The tick's pipeline is visible here — plan, match, emit, move —
/// beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same
/// flood (ADR 0004). The census-derived plans — the Layout's site Intents
/// and the hauler quota — are reused verbatim from a memo whose signature
/// matches this tick's census, and recomputed otherwise (ADR 0017); the
/// same memo hands the Atlas the spawn walks behind the leads, recalled
/// under an unchanged signature and dropped whole under a moved one
/// (ADR 0032).
let decideUnarbitrated
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    let signature = censusSignature view
    // The signature is read before the Atlas is built, because the Atlas
    // is one of the things it decides: a memo whose census still stands
    // hands over its spawn walk table, and a memo that has gone stale —
    // or none at all — leaves the Atlas a fresh one (ADR 0032).
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    let walks =
        match recalled with
        | Some m -> m.Walks
        | None -> WalkTable()

    let atlas = Atlas.ofViewRecalling walks view

    let plan =
        match recalled with
        | Some m -> m
        | None ->
            let siteIntents, servedFootings, unservedFootings, unroutedTrunks, deferredContainers =
                planLayout view atlas

            {
                Signature = signature
                SiteIntents = siteIntents
                UnservedFootings = unservedFootings
                ServedFootings = servedFootings
                UnroutedTrunks = unroutedTrunks
                DeferredContainers = deferredContainers
                HaulerQuota = haulerQuota view atlas
                Walks = walks
            }

    // The tick's Threats, derived once off the view's hostiles and the
    // rampart census, and shared by every reader of them (ADR 0033).
    let threats = threatsOf view atlas

    // The colony's other placement step, beside the memoised Layout and
    // never inside it (ADR 0042): the outpost's source containers, derived
    // fresh every tick for the reason written on the rule itself.
    let outpostSiteIntents = planOutpostContainers view atlas

    let defenseIntents = planSafeMode view atlas @ planFire view atlas

    // The pool is derived before the spawns since #187, and the dependency
    // runs one way only: the worker row's floor asks the pool whether
    // anything is standing in Build or Repair (ADR 0046, `workerFloor`),
    // and nothing in the pool reads a spawn Intent. The Matcher still runs
    // after both.
    let tasks = planTasks view threats
    let spawnIntents = planSpawns view atlas threats tasks plan.HaulerQuota
    let next, verdicts = matchCreeps view atlas threats tasks assignments verbose
    let assigned = assignedTasks tasks next
    let taskIntents = emit view atlas threats assigned

    // The reflex, less what a Task already asked for (#167). The Pickup
    // Task's own act is a strict subset of the reflex's: both want a Carry
    // body with room in it standing within range 1 of the pile in that
    // pile's own room, so an arriving picker's `PickupEnergy` was going to
    // be spelt twice — one Intent for the walk it made and one for the
    // reflex it triggered by arriving. The engine executes a creep's
    // second pickup over its first, so the duplicate cost nothing on the
    // server; what it did cost is the accepted-[[intent]] count the CPU
    // line is read off (#170), which over-reported by one per arrival.
    //
    // Deduplicated here rather than gated inside either producer, because
    // neither is wrong: the reflex is deliberately creep-position-shaped
    // and the Planner deliberately is not, and a gate in `applicable`
    // would make Pickup the one Task whose applicability reads a position.
    // The same reason the reflex lets two adjacent creeps both reach for
    // one pile leaves that case alone: those are two Intents, not one
    // spelt twice.
    let pickupIntents =
        planPickups view atlas
        |> List.filter (fun intent -> not (List.contains intent taskIntents))

    {
        Intents =
            defenseIntents
            @ spawnIntents
            @ plan.SiteIntents
            @ outpostSiteIntents
            @ pickupIntents
            @ taskIntents
        Assignments = next
        Memo = plan
        Verdicts = verdicts
        Movement = movementOf view atlas threats assigned verbose
    }

/// The decision seam a shell with one colony — and the whole suite — asks
/// for: `decideUnarbitrated`'s answer with this colony's movement folded
/// back in through the one-room-at-a-time pass (`resolveRooms`), so the
/// `Intents` and `Verdicts` here are the tick's whole answer for this
/// colony.
///
/// The two are the same call in every world one colony works alone. Where
/// they part is a room two colonies both work: this one arbitrates against
/// the [[foreign bodies]] as walls, and the shell's fold arbitrates against
/// them as the creeps they are, each moving on its own colony's intent
/// (#216 R2b, #220).
let decide
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    let decision = decideUnarbitrated view assignments verbose memo
    let moveIntents, moveVerdicts = resolveRooms [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }
