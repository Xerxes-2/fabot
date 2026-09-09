/// The body patterns and the arithmetic that sizes one to a bank (ADR 0006):
/// which pattern a row casts, and how many parts that pattern buys at a given
/// capacity. Knows nothing of a colony — a pattern shapes what a creep is good
/// at, never what it is assigned.
[<AutoOpen>]
module Fabot.Core.Decide.Bodies

open Fabot.Core
open Fabot.Core.Types

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

/// The Anchor pattern: the heavy-WORK body cast for a Dual Seat (ADR 0006).
/// The block is its minimal cast — two Work keep the body readable as an
/// Anchor (Work > Move, which fatigue parity forbids a worker body) beside the
/// single Carry and the single Move that pay the walk to the seat.
let anchorPattern =
    {
        Name = "anchor"
        Block = [ Work; Work; Carry; Move ]
    }

/// The hauler unit (ADR 0012): 150 energy, full speed loaded on roads — the
/// row carries its own road-parity declaration, because a hauler's whole life
/// is the trunk. No Work part, so it lives in the Withdraw->Refill cycle.
let haulerPattern =
    {
        Name = "hauler"
        Block = [ Carry; Carry; Move ]
    }

/// The reserver row (ADR 0042): the CLAIM body that walks to an outpost's
/// controller and holds its reservation, which is what makes that room's
/// sources worth ten a tick rather than five. `[2Claim;2Move]` pays for itself
/// twice over on a single source.
let reserverPattern =
    {
        Name = "reserver"
        Block = [ BodyPart.Claim; Move ]
    }

/// The upgrader row (ADR 0046): the body that stands beside the upgrade buffer
/// and spends the colony's surplus into the controller. Every part slot past
/// its single Carry goes to a Work/Move pair, so nothing in the body pays for a
/// commute it does not make. One Carry, because a body that stands still needs
/// exactly enough store to hold a Withdraw from the buffer at its feet (ADR
/// 0019).
let upgraderPattern =
    {
        Name = "upgrader"
        Block = [ Work; Carry; Move ]
    }

/// The guard row (ADR 0056): the melee body cast the tick a [[threat]] is seen
/// standing in a declared [[outpost]], and never before. 750 energy, ten parts,
/// 1,000 hits, 90 damage and 12 self-heal a tick — the body Overmind and bonzAI
/// converged on independently, and the one melee beats ranged on arithmetic at
/// this budget: an ATTACK part is 0.231 damage per energy counting the Move
/// that carries it, against a RANGED_ATTACK's 0.050. One Move per non-Move
/// part, so ADR 0003's fatigue parity holds with none of the [[anchor]]'s
/// exemption.
///
/// **The order is a rule and not a layout** (#282). The engine destroys body
/// parts from the head of the array, so what stands first is what is spent
/// first. Live, with `Attack` second, the whole of a guard's damage sat inside
/// the first four hundred hits: it was disarmed on the approach and reached
/// range 1 with nothing to swing, healing itself and answering
/// `ERR_NO_BODYPART` every tick while both invaders stayed at full health. So
/// Tough eats first, the Move parts next — a guard that cannot walk is still a
/// guard, because it is standing on its target already — then the Attack parts,
/// and Heal last, which is the ordering the community's own bodies carry. Cast
/// in whole blocks, the second block's Attack sits a further six hundred hits
/// down, so the damage degrades a part at a time rather than all at once.
let guardPattern =
    {
        Name = "guard"
        Block = [ Tough; Move; Move; Move; Move; Move; Attack; Attack; Attack; Heal ]
    }

/// The pattern table: every body the colony casts is a row here, sized by
/// energy under the row's own sizing rule. A future pattern is one more data
/// row plus its own quota rule, never a new code path (ADR 0006).
let patternTable =
    [
        workerPattern
        anchorPattern
        haulerPattern
        reserverPattern
        upgraderPattern
        guardPattern
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

/// Whether a counted body is a **standing body** (ADR 0046): fewer than one
/// Carry per `StandingCarryPerWork` Work. A fact about a *body* rather than
/// about a row — the upgrader row's `11W/1C/11M` is one, and so is the anchor
/// row's `6W/1C/1M`.
let internal standingParts (tuning: Tuning) parts =
    partCount parts Carry * tuning.StandingCarryPerWork < partCount parts Work

/// The pattern row a body was cast from, read off the parts alone (ADR 0006):
/// an ATTACK part is the guard row, a CLAIM part is the reserver row, a
/// Work-heavy body is the anchor row, a standing body at or under that line is
/// the upgrader row, no Work beside a Carry is the hauler row, and every other
/// body is the generalist. The row is what sizes the replacement a lead prices
/// (ADR 0026), so one rule serves every row.
///
/// Order matters between the anchor and upgrader arms and nowhere else:
/// `6W/1C/1M` satisfies both descriptions, and it is the anchor row that casts
/// it — a body pinned to a Post by ADR 0020's Work Area is a stronger claim
/// than standing beside the buffer. The reserver arm is what keeps ADR 0026
/// honest for a CLAIM body: `[Claim; Move]` has neither Work nor Carry, so
/// before it existed a reserver's lead was priced off a worker unit. The guard
/// arm is the same debt paid for a fighting body (ADR 0056): `[T; A×3; M×5; H]`
/// has neither, so without it a guard read back as a **worker**, and the raid
/// that cast it would go on filling the generalist row's `Living` for 1,500
/// ticks. The ATTACK test is asked first, beside `Fighter`'s place at the head
/// of the [[body class]] ladder — it is the one cut no other row of this colony
/// makes, every other row being built out of Work, Carry, Move and CLAIM.
///
/// `heavy` is the one input the two readings cannot share: the Atlas's
/// `workHeavy` set is keyed by creep name, and a body still in the oven has
/// none, so a cast answers the question for itself with `Work > Move` — the
/// ratio fatigue parity forbids a worker body.
let internal patternOfParts (tuning: Tuning) heavy parts =
    if partCount parts Attack > 0 then
        guardPattern
    elif partCount parts BodyPart.Claim > 0 then
        reserverPattern
    elif heavy then
        anchorPattern
    elif standingParts tuning parts then
        upgraderPattern
    elif partCount parts Work = 0 && partCount parts Carry > 0 then
        haulerPattern
    else
        workerPattern

/// Whether a body can take energy out of a store and put it into an extension
/// — the one capability the bank's own refilling depends on, and so the one
/// every capacity-sized row depends on (the supply floor, ADR 0050). Not "has
/// a Carry part": it is the body half of `Refill`'s gate and the body half of
/// `Withdraw`'s read back together, because a body that can deliver but never
/// draw cannot reach the storage the energy is standing in — a Carry part, no
/// standing-body ratio (ADR 0046) and not Work-heavy (ADR 0016). `Refill`'s
/// third conjunct, `Energy > 0`, is deliberately *not* read: that is a state a
/// hauler passes through twice a trip. `heavy` is `patternOfParts`' own input,
/// for its own reason.
let internal canRefillParts (tuning: Tuning) heavy parts =
    partCount parts Carry > 0 && not (standingParts tuning parts) && not heavy

/// The Anchor row's Work ceiling (ADR 0021): the Work that saturate one source
/// — dig its whole regeneration in the regeneration time — plus one spare. Past
/// saturation a further Work only drains the source sooner and idles; the spare
/// drains it 50 ticks early, and those ticks absorb an unmanned Post's gap at
/// no cost. A rule about one source's regeneration and never about heavy bodies
/// in general, so ADR 0042 narrows it by changing its input.
let internal workCapOf output = output / Engine.harvestPerWork + 1

/// The ceiling in a room the colony holds: six Work, the number ADR 0021
/// derived.
let internal heldWorkCap = workCapOf Engine.heldOutputPerTick

/// The worker row's sizing rule: the largest affordable repetition of the block
/// (never below one repeat), with the remainder spent on Carry/Move at fatigue
/// parity — the padded body is never slower than the pure-block body, empty or
/// loaded, and within that buys as much Carry as possible (ADR 0003, narrowed
/// to the worker pattern by ADR 0006). Parts are grouped Work, Carry, Move so
/// damage strips Work first and mobility last. It is the rule every row without
/// one of its own falls through to, and it can only place the three parts it
/// counts, so a *shape* it cannot size is a hard stop rather than a quiet
/// omission: a block holding a guard's Attack would be silently rebuilt out of
/// Work, Carry and Move, and an empty one divides by zero on .NET while the
/// emitted JS reads `capacity / 0` as no repeats at all and pads a Carry/Move
/// body out of a row that asked for neither — which is why the stop is
/// explicit and not left to the arithmetic.
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
/// remaining energy affords on Work up to the row's ceiling — spawn energy buys
/// output rather than mobility the Post never uses, and stops where the source
/// has no more to give (ADR 0021). Exempt from fatigue parity (ADR 0006); never
/// below the row's two-Work block.
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

/// The hauler row's sizing rule (ADR 0012): as many whole [Carry; Carry; Move]
/// blocks as capacity buys (never below one), and nothing else. The row's
/// parity declaration is road parity — two loaded Carry generate two fatigue on
/// a road tile, the one Move pays off two a tick — which the whole block meets
/// and a padded lone Carry would break.
let private haulerBodyFor capacity =
    wholeBlockBodyFor haulerPattern.Block capacity

/// The reserver row's sizing rule: as many whole [Claim; Move] blocks as
/// capacity buys, never below one. The bank's truncation alone, which is half
/// the row's rule — ADR 0042 sizes the body off the reservation deficit *capped
/// by the bank*, and `reserverBodyWithin` is where the two halves meet. This
/// entry point is the one `bodyFor` exposes, so a reader holding only a
/// capacity gets the largest body the row could cast and therefore the longest
/// lead, which is the safe direction: a successor is cast early rather than
/// after its incumbent died.
let internal reserverBodyFor capacity =
    wholeBlockBodyFor reserverPattern.Block capacity

/// The guard row's sizing rule (ADR 0056): as many whole
/// `[Tough; Attack×3; Move×5; Heal]` blocks as capacity buys, never below one
/// and capped at five by the engine's 50 parts — the rule the hauler and
/// reserver rows already share, chosen for this row because the block is
/// already at fatigue parity and a remainder spent at ADR 0003's parity would
/// buy Carry a guard has no use for. `List.distinct` keeps the block's order,
/// so two blocks are `[T;T; A×6; M×10; H;H]` and not a shuffle of them. What
/// the banks buy: 300 cannot afford one block at all and the row **yields**
/// (ADR 0050), 800 and 1,300 buy one, 1,800 two and 2,300 three.
let private guardBodyFor capacity =
    wholeBlockBodyFor guardPattern.Block capacity

/// The upgrader row's sizing rule (ADR 0046): one Carry, and every part slot
/// the rest of the capacity affords spent on Work/Move **pairs** — `W = M =
/// floor((capacity - 50) / 150)`, never below one pair. The gain over the
/// generalist is the parts it would spend carrying energy to work it is not
/// going to do standing still. Why the Move parts at all, for a body that
/// stands: ADR 0016's gate is `Work > Move`, and a body over that line may not
/// Withdraw — which is the buffer this row exists to drink from (ADR 0019). So
/// pairing keeps the row at `Work = Move`, inside the gate.
let private upgraderBodyFor capacity =
    let pairs =
        (capacity - bodyCost [ Carry ]) / bodyCost [ Work; Move ]
        |> max 1
        |> min ((Engine.maxBodyParts - 1) / 2)

    List.replicate pairs Work @ [ Carry ] @ List.replicate pairs Move

/// Body for a pattern at an energy capacity, under the row's own sizing rule
/// (ADR 0006): the anchor row spends on Work beside its fixed Carry/Move pair,
/// the hauler, reserver and guard rows buy whole blocks, the upgrader row buys
/// Work/Move pairs beside one Carry, and every other row pads its remainder at
/// plain fatigue parity — or, if its block holds a part that rule cannot place,
/// is refused rather than sized into some other body. A capacity is the whole
/// of what this entry point holds, so the two rows whose real rule reads a
/// second fact — the anchor's Post (ADR 0053) and the reserver's deficit — are
/// answered here at their **largest** body.
let bodyFor pattern capacity =
    if pattern.Name = anchorPattern.Name then
        anchorBodyFor heldWorkCap capacity
    elif pattern.Name = haulerPattern.Name then
        haulerBodyFor capacity
    elif pattern.Name = reserverPattern.Name then
        reserverBodyFor capacity
    elif pattern.Name = guardPattern.Name then
        guardBodyFor capacity
    elif pattern.Name = upgraderPattern.Name then
        upgraderBodyFor capacity
    else
        parityBodyFor pattern capacity

/// The generalist body: the worker row of the pattern table, sized to
/// capacity.
let workerBodyFor capacity = bodyFor workerPattern capacity
