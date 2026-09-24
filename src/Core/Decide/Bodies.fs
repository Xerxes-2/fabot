/// The body patterns and the arithmetic that sizes one to a bank: which
/// pattern a row casts, and how many parts it buys at a given capacity. Knows
/// nothing of a colony.
[<AutoOpen>]
module Fabot.Core.Decide.Bodies

open Fabot.Core
open Fabot.Core.Types

/// A Body pattern: the repeating part block a body is generated from. Which
/// pattern a spawn casts is a colony decision; the pattern shapes what a creep
/// is good at, never what it is assigned.
type BodyPattern = { Name: string; Block: BodyPart list }

/// The generalist pattern: 200 energy, full speed empty, half speed loaded.
let workerPattern =
    {
        Name = "worker"
        Block = [ Work; Carry; Move ]
    }

/// The Anchor pattern: the heavy-WORK body cast for a Post. Two Work keep
/// the block readable as an Anchor (Work > Move, which fatigue parity forbids a
/// worker body) beside the Carry and Move that pay the walk to the seat.
let anchorPattern =
    {
        Name = "anchor"
        Block = [ Work; Work; Carry; Move ]
    }

/// ADR-0012
/// The hauler unit: 150 energy, full speed loaded on roads. No Work part, so
/// it lives in the Withdraw->Refill cycle.
let haulerPattern =
    {
        Name = "hauler"
        Block = [ Carry; Carry; Move ]
    }

/// The season courier (#319): twenty Carry hold a 999-unit Thorium load below
/// the 1,000-unit contact cliff, and ten Move carry it at road parity. One
/// fixed body: a richer bank buys no useful capacity, and a poorer one yields.
let courierPattern =
    {
        Name = "courier"
        Block = List.replicate 20 Carry @ List.replicate 10 Move
    }

/// The reserver row: the CLAIM body that holds an outpost's reservation.
let reserverPattern =
    {
        Name = "reserver"
        Block = [ BodyPart.Claim; Move ]
    }

/// The upgrader row: the body that stands beside the upgrade buffer. One Carry,
/// because a body that stands still needs exactly enough store to hold a
/// Withdraw from the buffer at its feet.
let upgraderPattern =
    {
        Name = "upgrader"
        Block = [ Work; Carry; Move ]
    }

/// ADR-0056
/// The guard row: the melee body cast the tick a threat is seen in a declared
/// outpost. One Move per non-Move part, so fatigue parity holds. The order is
/// a rule and not a layout (#282): the engine destroys parts from the head of
/// the array, so Tough eats first, Move next (a guard that cannot walk is
/// already standing on its target), then Attack, and Heal last.
let guardPattern =
    {
        Name = "guard"
        Block = [ Tough; Move; Move; Move; Move; Move; Attack; Attack; Attack; Heal ]
    }

/// The most whole guard blocks one body can carry under the engine's part cap:
/// the ceiling `guardBlocksFor` searches under and the size a caller holding
/// no colony prices the row at.
let guardBlocksMost = Engine.maxBodyParts / List.length guardPattern.Block

/// ADR-0057
/// The miner row: the store-less Work body over the mineral container. The
/// block is the row's floor rather than its ratio (one Move per
/// `Tuning.MinerWorkPerMove` Work). No Carry, which is the decision: a body
/// holding Thorium ages by `floor(log10 store.T)` ticks a tick, and a harvest
/// with no room for the yield drops it onto the container tile, which lands
/// **in** the container. Work-heavy with no Carry is the one shape no other row
/// casts, and so the cut `patternOfParts` reads it back off.
let minerPattern =
    {
        Name = "miner"
        Block = [ Work; Work; Move ]
    }

/// ADR-0006
/// The pattern table: every body the colony casts is a row here. A future
/// pattern is one more data row plus its own quota rule, never a new code path.
let patternTable =
    [
        workerPattern
        anchorPattern
        haulerPattern
        courierPattern
        reserverPattern
        upgraderPattern
        guardPattern
        minerPattern
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

/// Whether a counted body is a standing body: fewer than one Carry per
/// `StandingCarryPerWork` Work. A fact about a body rather than a row — the
/// upgrader's `11W/1C/11M` is one, and so is the anchor's `6W/1C/1M`.
let internal standingParts (tuning: Tuning) parts =
    partCount parts Carry * tuning.StandingCarryPerWork < partCount parts Work

/// The Work ceiling of the miner row: twenty, the **bank's** ceiling rather
/// than the engine's part cap — twenty Work and four Move is 2,200 of the 2,300
/// an RCL6 spawn holds. Past it a larger body only ends the deposit sooner:
/// `WORK / 6` a tick against a reactor that eats one Thorium a tick. Stated here
/// beside `heldWorkCap`: it is the ceiling one row's sizing rule stops at.
let internal minerWorkCap = 20

/// The miner row's sizing rule: every part slot the bank affords on Work up to
/// `minerWorkCap`, one Move per `perMove` of them — `[16 Work; 4 Move]` at
/// 1,800 and `[20 Work; 4 Move]` at 2,300. Exempt from fatigue parity, which is
/// a rule about a body that keeps moving: this one walks to a tile once. Never
/// below the row's own block: a bank too poor to pay is refused in
/// `castFromBank`, not here.
let internal minerBodyFor perMove capacity =
    // Whole `perMove` Work and the one Move that carries them, then the
    // remainder on a short group, which pays for its own Move as soon as it
    // holds a single Work.
    let group = perMove * bodyCost [ Work ] + bodyCost [ Move ]
    let groups = capacity / group |> min (minerWorkCap / perMove)

    let spare =
        if groups * perMove >= minerWorkCap then
            0
        else
            (capacity - groups * group - bodyCost [ Move ]) / bodyCost [ Work ]
            |> max 0
            |> min (minerWorkCap - groups * perMove)
            |> min (perMove - 1)

    let work = groups * perMove + spare |> max (partCountIn minerPattern.Block Work)
    let move = (work + perMove - 1) / perMove

    List.replicate work Work @ List.replicate move Move

/// The pattern row a body was cast from, read off the parts alone: an ATTACK
/// part is the guard row, a CLAIM part the reserver row, a Work-heavy body with
/// no Carry the miner row, a Work-heavy body with one the anchor row, a
/// standing body at or under that line the upgrader row, no Work beside a Carry
/// the hauler row, and every other body the generalist.
///
/// Order matters between the miner and anchor arms, and between the anchor and
/// upgrader arms, and nowhere else. Miner before anchor: both are Work-heavy
/// and the Carry is the whole difference; without the arm a `[20 Work; 4 Move]`
/// reads back as an Anchor and retires a garrison from a rock for its whole
/// life. Anchor before upgrader: `6W/1C/1M` satisfies both, and a body pinned
/// to a Post is the stronger claim. The reserver and guard arms exist because
/// `[Claim; Move]` and `[T; A×3; M×5; H]` have neither Work nor Carry, so
/// without them a reserver's lead was priced off a worker unit and a guard
/// filled the generalist row's `Living` for 1,500 ticks.
///
/// `heavy` is the one input the two readings cannot share: the Atlas's
/// `workHeavy` set is keyed by creep name, and a body still in the oven has
/// none, so a cast answers it for itself with `Work > Move`.
let internal patternOfParts (tuning: Tuning) heavy parts =
    if partCount parts Attack > 0 then
        guardPattern
    elif partCount parts BodyPart.Claim > 0 then
        reserverPattern
    elif heavy && partCount parts Carry = 0 then
        minerPattern
    elif heavy then
        anchorPattern
    elif standingParts tuning parts then
        upgraderPattern
    elif partCount parts Work = 0 && partCount parts Carry > 0 then
        haulerPattern
    else
        workerPattern

/// Whether a body can take energy out of a store and put it into an extension:
/// the body half of `Refill`'s gate and of `Withdraw`'s read back together,
/// because a body that can deliver but never draw cannot reach the storage the
/// energy is standing in. `Refill`'s third conjunct, `Energy > 0`, is
/// deliberately not read: that is a state a hauler passes through twice a
/// trip.
let internal canRefillParts (tuning: Tuning) heavy parts =
    partCount parts Carry > 0 && not (standingParts tuning parts) && not heavy

/// ADR-0021
/// The Anchor row's Work ceiling: the Work that saturate one source plus one
/// spare. A rule about one source's regeneration, so the outpost layer narrows
/// it by changing its input.
let internal workCapOf output = output / Engine.harvestPerWork + 1

/// The ceiling in a room the colony holds: six Work.
let internal heldWorkCap = workCapOf Engine.heldOutputPerTick

/// ADR-0003
/// The worker row's sizing rule: the largest affordable repetition of the block
/// (never below one), the remainder spent on Carry/Move at fatigue parity.
/// Parts are grouped Work, Carry, Move so damage strips Work first and mobility
/// last. It is the rule every row without one of its own falls through to, and
/// it can only place the three parts it counts, so a shape it cannot size is a
/// hard stop: a block holding a guard's Attack would be silently rebuilt out of
/// Work, Carry and Move, and an empty one divides by zero on .NET while the
/// emitted JS reads `capacity / 0` as no repeats at all and pads a Carry/Move
/// body out of a row that asked for neither.
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

    let repeats =
        capacity / bodyCost block |> max 1 |> min (Engine.maxBodyParts / blockSize)

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
            (repeats * partCountIn block Work)
            (repeats * partCountIn block Carry)
            (repeats * partCountIn block Move)
            (capacity - repeats * bodyCost block)
            (Engine.maxBodyParts - repeats * blockSize)

    List.replicate work Work @ List.replicate carry Carry @ List.replicate move Move

/// The anchor row's sizing rule: one Carry, one Move, and every part slot the
/// remaining energy affords on Work up to the row's ceiling. Exempt from
/// fatigue parity; never below the row's two-Work block.
let internal anchorBodyFor workCap capacity =
    let work =
        (capacity - bodyCost [ Carry; Move ]) / bodyCost [ Work ]
        |> max 2
        |> min workCap

    List.replicate work Work @ [ Carry; Move ]

/// The whole-block rows' shared arithmetic: as many whole blocks as the
/// capacity buys, never below one and never past the engine's 50-part cap, with
/// the parts grouped by kind in the block's own order — so damage strips a
/// row's output before its legs.
let private wholeBlockBodyFor (block: BodyPart list) capacity =
    let repeats =
        capacity / bodyCost block
        |> max 1
        |> min (Engine.maxBodyParts / List.length block)

    block
    |> List.distinct
    |> List.collect (fun part -> List.replicate (repeats * partCountIn block part) part)

/// The hauler row's sizing rule: whole `[Carry; Carry; Move]` blocks and
/// nothing else. Road parity — two loaded Carry generate two fatigue on a road
/// tile, the one Move pays off two a tick — which a padded lone Carry would
/// break.
let private haulerBodyFor capacity =
    wholeBlockBodyFor haulerPattern.Block capacity

/// The courier's fixed body. Affordability belongs to the spawn cascade; body
/// sizing states the row's one useful shape even when a caller is only asking
/// what that shape costs.
let private courierBodyFor () = courierPattern.Block

/// The bank's truncation alone, half the reserver row's rule; `reserverBodyWithin`
/// is where the deficit meets it. This is the entry `bodyFor` exposes, so a
/// reader holding only a capacity gets the largest body the row could cast and
/// therefore the longest lead, which is the safe direction.
let internal reserverBodyFor capacity =
    wholeBlockBodyFor reserverPattern.Block capacity

/// The guard row's sizing rule: whole blocks, never below one and capped at
/// five by the engine's 50 parts. A remainder spent at parity would buy Carry
/// a guard has no use for. `List.distinct` keeps the block's order, so two
/// blocks are `[T;T; A×6; M×10; H;H]` and not a shuffle of them. 300 cannot
/// afford one block and the row yields; 800 and 1,300 buy one, 1,800 two,
/// 2,300 three.
let private guardBodyFor capacity =
    wholeBlockBodyFor guardPattern.Block capacity

/// ADR-0072
/// The guard row's body at the blocks its exchange takes: the bank truncates
/// the blocks and the blocks truncate the bank, so a colony that can afford one
/// block casts one where one wins.
let internal guardBodyWithin blocks capacity =
    guardBodyFor (min capacity (blocks * bodyCost guardPattern.Block))

/// ADR-0046
/// The upgrader row's sizing rule: one Carry, and the rest on Work/Move pairs
/// — `W = M = floor((capacity - 50) / 150)`, never below one pair. Why Move
/// parts at all for a body that stands: the Withdraw gate is `Work > Move`, and
/// the buffer is what this row drinks from, so pairing keeps it at `Work = Move`
/// inside the gate.
let private upgraderBodyFor capacity =
    let pairs =
        (capacity - bodyCost [ Carry ]) / bodyCost [ Work; Move ]
        |> max 1
        |> min ((Engine.maxBodyParts - 1) / 2)

    List.replicate pairs Work @ [ Carry ] @ List.replicate pairs Move

/// ADR-0042
/// The reserver row's body for one outpost: the deficit sizing and the bank
/// truncation, whichever asks for less, never below one block.
let internal reserverBodyWithin claims capacity =
    reserverBodyFor (min capacity (claims * bodyCost reserverPattern.Block))

/// The second fact the rows whose sizing is not the bank's answer alone read:
/// the anchor row's Work ceiling for the Post the body is bought for, the
/// reserver row's outstanding claims, the miner ratio and the guard blocks. One
/// record so the one sizing rule takes one shape from every caller.
type BodySizing =
    {
        AnchorCap: int
        ReserverClaims: int list
        /// `Tuning`'s own number: a tunable rather than a fact of the tick,
        /// so every caster hands over its colony's own.
        MinerWorkPerMove: int
        /// `Quota.guardBlocksWanted`'s answer this tick, at least one. The
        /// guard row was the one row sized by the bank alone — three blocks
        /// at 2,250 against a 1,000-hit invader one block kills — and a body
        /// the bank never reaches is a row that never casts.
        GuardBlocks: int
    }

/// The sizing a caller holding nothing but a capacity can ask for: every row at
/// its largest body. The miner's entry is a tunable and not a ceiling, honest
/// for the same reason `AnchorCap`'s is: every casting path that buys one comes
/// through `RowSizing`, which reads `view.Tuning`.
let largestSizing =
    {
        AnchorCap = heldWorkCap
        ReserverClaims = []
        MinerWorkPerMove = Tuning.defaults.MinerWorkPerMove
        GuardBlocks = guardBlocksMost
    }

/// Body for a pattern at an energy capacity, under the row's own sizing rule.
/// **The** dispatch over the pattern, asked by the rows, by the lead's
/// successor and by `bodyFor`: written per caller it was three tables.
let sizedBodyFor (sizing: BodySizing) pattern capacity =
    if pattern.Name = anchorPattern.Name then
        anchorBodyFor sizing.AnchorCap capacity
    elif pattern.Name = haulerPattern.Name then
        haulerBodyFor capacity
    elif pattern.Name = courierPattern.Name then
        courierBodyFor ()
    elif pattern.Name = reserverPattern.Name then
        match sizing.ReserverClaims with
        | [] -> reserverBodyFor capacity
        // Every cast at the largest outstanding demand: the Matcher pairs a
        // finished body to a controller by travel cost, so a body sized for
        // the room that has slipped furthest can land on the one that has not.
        | claims -> reserverBodyWithin (List.max claims) capacity
    elif pattern.Name = guardPattern.Name then
        guardBodyWithin sizing.GuardBlocks capacity
    elif pattern.Name = upgraderPattern.Name then
        upgraderBodyFor capacity
    elif pattern.Name = minerPattern.Name then
        minerBodyFor sizing.MinerWorkPerMove capacity
    else
        parityBodyFor pattern capacity

/// The same at the largest body either of those rows can take: the anchor's
/// Post and the reserver's deficit are answered at their ceiling.
let bodyFor pattern capacity =
    sizedBodyFor largestSizing pattern capacity

/// The generalist body: the worker row of the pattern table, sized to
/// capacity.
let workerBodyFor capacity = bodyFor workerPattern capacity
