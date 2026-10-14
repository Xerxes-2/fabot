/// ADR-0006: the body patterns and what each casts at a given bank.
module Fabot.Core.Tests.Decide.QuotaBodyTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

[<Tests>]
let partNameTests =
    testList
        "part names"
        [
            test "matches the engine's spelling, one name per part" {
                // These strings leave the program in spawnCreep bodies and
                // come back in hostile body arrays; the table is the engine's
                // spec, restated so a swapped or misspelt case fails.
                Expect.equal
                    (allBodyParts |> List.map partName)
                    [ "work"; "carry"; "move"; "attack"; "ranged_attack"; "heal"; "claim"; "tough" ]
                    "each BodyPart maps to its Screeps string"
            }
        ]

[<Tests>]
let bodyTests =
    testList
        "worker body"
        [
            test "a 150 remainder buys two Carry and a Move" {
                // 550 = 2 units + 150: the old whole-unit body stranded 150.
                Expect.equal
                    (workerBodyFor 550)
                    [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                    "remainder is spent at parity: max Carry without moving slower than the pure-unit body"
            }

            test "a 50 remainder buys a Move, not a Carry" {
                // A lone Carry would tip loaded fatigue past the pure-unit body's.
                Expect.equal
                    (workerBodyFor 250)
                    [ Work; Carry; Move; Move ]
                    "the trailing 50 goes to Move"
            }

            test "a 100 remainder buys a Carry/Move pair" {
                Expect.equal
                    (workerBodyFor 500)
                    [ Work; Work; Carry; Carry; Carry; Move; Move; Move ]
                    "a pair keeps parity and adds haul"
            }

            test "an exact multiple stays pure units" {
                Expect.equal
                    (workerBodyFor 800)
                    (List.replicate 4 Work @ List.replicate 4 Carry @ List.replicate 4 Move)
                    "no remainder, no pad"
            }

            test "below one unit cost the floor is one unit" {
                Expect.equal (workerBodyFor 150) [ Work; Carry; Move ] "never below one unit"
            }

            test "every capacity is spent to within a part price, at fatigue parity" {
                for capacity in 200..50..1300 do
                    let body = workerBodyFor capacity

                    let count part =
                        body |> List.filter ((=) part) |> List.length

                    let work, carry, move = count Work, count Carry, count Move

                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        capacity
                        $"affordable at capacity {capacity}"

                    Expect.isLessThan
                        (capacity - bodyCost body)
                        50
                        $"nothing a part could buy is stranded at capacity {capacity}"

                    Expect.isLessThanOrEqual
                        (work + carry)
                        (2 * move)
                        $"loaded parity with the pure-unit body at capacity {capacity}"

                    Expect.isLessThanOrEqual
                        work
                        move
                        $"empty parity with the pure-unit body at capacity {capacity}"
            }

            test "the body never exceeds the 50-part engine cap" {
                // RCL8 capacity: unbounded replication would emit 192 parts,
                // which the engine rejects outright.
                Expect.equal
                    (workerBodyFor 12900)
                    (List.replicate 16 Work @ List.replicate 17 Carry @ List.replicate 17 Move)
                    "16 units plus a Carry/Move pair fill exactly 50 parts"
            }
        ]

[<Tests>]
let patternTableTests =
    testList
        "pattern table"
        [
            test
                "the worker unit, the Anchor, the hauler, the reserver, the upgrader, the guard, the ranger, the miner and the dismantler are the table's rows" {
                // The order here is the declaration's and not the casting order
                // (guard, reserver, Anchor, hauler, upgrader, worker), because nothing
                // reads this list for a sequence. Every row is cast off a colony fact.
                // The upgrader's block is the worker unit's three parts and its rule
                // is not (one Carry and Work/Move pairs for the rest); the guard's
                // block is bought for a fight (no Work, no Carry, five Move to carry
                // the five that fight; ten parts, 750 energy, TOUGH first and HEAL
                // last); the ranger's holds an errand room from range (#411), Move
                // first so damage lames before it disarms; the miner's is Work and
                // Move and no Carry at all, the one shape no other row takes and the
                // cut `patternOfParts` tells it from
                // the Anchor by, with one Move per five Work, capped at twenty Work;
                // the dismantler's is Work and Move at parity and no Carry either,
                // told from the miner by not being Work-heavy.
                // A row is a name and a sizing rule before it is a block.
                Expect.equal
                    patternTable
                    [
                        {
                            Name = "worker"
                            Block = [ Work; Carry; Move ]
                        }
                        {
                            Name = "anchor"
                            Block = [ Work; Work; Carry; Move ]
                        }
                        {
                            Name = "hauler"
                            Block = [ Carry; Carry; Move ]
                        }
                        {
                            Name = "courier"
                            Block = List.replicate 20 Carry @ List.replicate 10 Move
                        }
                        {
                            Name = "reserver"
                            Block = [ BodyPart.Claim; Move ]
                        }
                        {
                            Name = "upgrader"
                            Block = [ Work; Carry; Move ]
                        }
                        {
                            Name = "guard"
                            Block =
                                [
                                    Tough
                                    Move
                                    Move
                                    Move
                                    Move
                                    Move
                                    Attack
                                    Attack
                                    Attack
                                    Heal
                                ]
                        }
                        {
                            Name = "ranger"
                            Block = [ Move; Move; Move; RangedAttack; RangedAttack; Heal ]
                        }
                        // The home's rampart defender (#504): MOVE and ATTACK
                        // one to one.
                        {
                            Name = "defender"
                            Block = [ Move; Attack ]
                        }
                        // An assault's three roles, each one fixed body.
                        {
                            Name = "sapper"
                            Block = List.replicate 25 Move @ List.replicate 25 Work
                        }
                        {
                            Name = "probe"
                            Block =
                                List.replicate 4 Tough
                                @ List.replicate 5 Work
                                @ List.replicate 9 Move
                        }
                        {
                            Name = "medic"
                            Block = List.replicate 18 Move @ List.replicate 18 Heal
                        }
                        {
                            Name = "escort"
                            Block =
                                List.replicate 24 Move
                                @ List.replicate 16 RangedAttack
                                @ List.replicate 8 Heal
                        }
                        {
                            Name = "miner"
                            Block = [ Work; Work; Move ]
                        }
                        {
                            Name = "dismantler"
                            Block = [ Work; Move ]
                        }
                    ]
                    "every body the colony casts comes from these rows"

                Expect.equal
                    (bodyCost guardPattern.Block)
                    750
                    "and the guard block is the 750 energy ADR 0056's whole arithmetic is written at"
            }

            test
                "the courier is one fixed 999-unit road body, whatever the bank can afford above it" {
                let expected = List.replicate 20 Carry @ List.replicate 10 Move

                Expect.equal
                    (bodyFor courierPattern 1500)
                    expected
                    "twenty Carry hold the 999-unit load below its decade cliff"

                Expect.equal
                    (bodyFor courierPattern 2300)
                    expected
                    "the row does not spend a richer RCL6 bank on unused capacity"

                Expect.equal (bodyCost expected) 1500 "the fixed body costs 1,500 energy"
            }

            test "every row of the table has a sizing rule that can size it" {
                // The generalist rule refuses a shape it cannot size
                // (#155), and that refusal fires inside `decide`, which
                // `Main.loop` calls under no handler — a row declared
                // without a rule of its own would cost every intent of
                // every tick, not one mis-shaped creep. So the table
                // itself is walked here: a row arriving without a sizing
                // rule is red at this gate the moment it is declared,
                // rather than in the colony the moment it is first cast.
                for row in patternTable do
                    Expect.isNonEmpty
                        (bodyFor row 1800)
                        $"the {row.Name} row sizes at an 1,800 bank"
            }

            test "the anchor row spends everything on Work beside one Carry and one Move" {
                // 550 = the RCL2 full bank: 100 buys the Carry/Move pair, the rest
                // is Work; the Anchor is exempt from fatigue parity.
                Expect.equal
                    (bodyFor anchorPattern 550)
                    [ Work; Work; Work; Work; Carry; Move ]
                    "all remaining energy buys Work"
            }

            test "the anchor row never casts below its block" {
                Expect.equal
                    (bodyFor anchorPattern 300)
                    [ Work; Work; Carry; Move ]
                    "two Work keep the Anchor readable off its body (Work > Move)"
            }

            test "the anchor row stops at source saturation plus one spare Work" {
                // A source regenerates 3,000 energy per 300 ticks and a Work digs 2
                // a tick, so five Work saturate it; the sixth is slack for an
                // unmanned Post's gap. RCL4's 1,300 bank would otherwise buy twelve.
                Expect.equal
                    (bodyFor anchorPattern 1300)
                    [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                    "six Work beside the Carry/Move pair, the rest stays banked"
            }

            test "the anchor cap holds at the richest bank" {
                Expect.equal
                    (bodyFor anchorPattern 12900)
                    [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                    "an RCL8 bank casts the same six-Work Anchor"
            }

            test "an outpost Post's anchor carries a Move per two Work, and a home Post's keeps one" {
                // #515; the rule's why is `anchorBodyFor`'s.
                let outpost =
                    { largestSizing with
                        AnchorLegs = OutpostLegs
                    }

                Expect.equal
                    (sizedBodyFor outpost anchorPattern 800)
                    (List.replicate 6 Work @ [ Carry ] @ List.replicate 3 Move)
                    "800 buys six Work, the Carry and three Move"

                Expect.equal
                    (sizedBodyFor outpost anchorPattern 550)
                    (List.replicate 4 Work @ [ Carry ] @ List.replicate 2 Move)
                    "550 trades no Work for the legs: four Work, two Move"

                Expect.equal
                    (sizedBodyFor outpost anchorPattern 300)
                    [ Work; Work; Carry; Move ]
                    "the block's one Move already carries its two Work"

                Expect.equal
                    (bodyFor anchorPattern 800)
                    (List.replicate 6 Work @ [ Carry; Move ])
                    "the home Post's anchor at the same bank keeps its one Move"
            }

            test "the hauler row builds whole blocks and nothing else" {
                // 500 buys three whole blocks (450) and strands the rest:
                // the row's own declaration is road parity, which a padded
                // lone Carry would break — three Move pay six fatigue a
                // tick, seven loaded Carry on a road would generate seven.
                Expect.equal
                    (bodyFor haulerPattern 500)
                    (List.replicate 6 Carry @ List.replicate 3 Move)
                    "capacity buys whole [Carry;Carry;Move] blocks; the remainder stays banked"
            }

            test "the hauler row never casts below its block" {
                Expect.equal
                    (bodyFor haulerPattern 100)
                    [ Carry; Carry; Move ]
                    "the block is the row's minimal cast"
            }

            test "the hauler body never exceeds the 50-part engine cap" {
                Expect.equal
                    (bodyFor haulerPattern 9000)
                    (List.replicate 32 Carry @ List.replicate 16 Move)
                    "sixteen blocks fill 48 parts"
            }

            test "the reserver row builds whole blocks and nothing else" {
                // 1,800 is the colony's live RCL5 bank and 650 a `[Claim; Move]`
                // block, so the bank buys two and strands 500: `[2Claim;2Move]`. This
                // is the bank's half of the row's rule and the body `bodyFor` answers
                // with, which is what a lead prices its succession off. What the row
                // actually casts is `min(reservation deficit, this)`, pinned in "the
                // reserver row" below.
                Expect.equal
                    (bodyFor reserverPattern 1800)
                    [ BodyPart.Claim; BodyPart.Claim; Move; Move ]
                    "capacity buys whole [Claim; Move] blocks; the remainder stays banked"
            }

            test "the reserver row never casts below its block" {
                // A CLAIM part is indivisible: a body under one block
                // reserves nothing, and an empty body would price a
                // reserver's succession at zero ticks of cast time.
                Expect.equal
                    (bodyFor reserverPattern 100)
                    [ BodyPart.Claim; Move ]
                    "the block is the row's minimal cast"
            }

            test "the reserver body never exceeds the 50-part engine cap" {
                Expect.equal
                    (bodyFor reserverPattern 100000)
                    (List.replicate 25 BodyPart.Claim @ List.replicate 25 Move)
                    "twenty-five blocks fill the 50 parts exactly"
            }

            test "the upgrader row buys Work/Move pairs beside one Carry" {
                // The upgrader body at the live RCL5 bank of 1,800:
                // `floor((1800 - 50) / 150)` is eleven pairs for 1,700, against the
                // worker row's nine Work at the same bank. The Carry stays at one
                // however rich the bank: a body that stands beside the buffer needs
                // one Withdraw's worth of store and nothing more.
                Expect.equal
                    (bodyFor upgraderPattern 1800)
                    (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
                    "eleven Work, one Carry, eleven Move — Work first, legs last"
            }

            test "the upgrader row's minimal cast is one pair beside the Carry" {
                // The RCL1 bank, where `(300 - 50) / 150` is one before any clamp:
                // the row's smallest body is a worker unit's parts under a different
                // rule, and still `Work = Move`, which keeps the Heavy-Work gate open
                // to it at every size.
                Expect.equal
                    (bodyFor upgraderPattern 300)
                    [ Work; Carry; Move ]
                    "one pair beside the Carry is the row's minimal cast"
            }

            test "below one pair's price the floor is one pair" {
                // The clamp itself, which the RCL1 bank above does not
                // reach: under 200 the arithmetic asks for no pair at all
                // and the rule would answer with a bare Carry. Same floor
                // the generalist row carries at 150 — no live bank is ever
                // this poor, and a sizing rule that can answer an empty
                // body is the thing being ruled out.
                Expect.equal
                    (bodyFor upgraderPattern 150)
                    [ Work; Carry; Move ]
                    "never below one pair, however poor the bank"
            }

            test "under an 800 bank the row's own cast is not a standing body" {
                // Where the sizing rule and the standing-body predicate cross: one
                // Carry against `floor((capacity - 50) / 150)` Work meets
                // `Carry * 4 < Work` at five pairs, so the RCL2 bank of 550 casts
                // `3W/1C/3M`, this row's body and outside the gate the row exists
                // for. Deliberate: three Work against a fifty-energy load is not yet
                // the commute the ratio prices. It is also where the quota stops: a
                // row read back to the generalist is a row nothing could count, so
                // `upgraderQuota` hires none at this bank (#187; pinned at the seam
                // by "a bank whose cast is no standing body hires none").
                Expect.equal
                    (bodyFor upgraderPattern 550)
                    [ Work; Work; Work; Carry; Move; Move; Move ]
                    "three pairs beside the Carry at the RCL2 bank"
            }

            test "the upgrader body never exceeds the 50-part engine cap" {
                // A pair is 150 energy, so an RCL8 bank would ask for
                // eighty-five of them — the largest overshoot of any row,
                // though the last to start biting (3,800 here against the
                // hauler row's 2,550). Twenty-four pairs beside the one
                // Carry is 49 parts, and the fiftieth cannot be a Work
                // without leaving it unpaired.
                Expect.equal
                    (bodyFor upgraderPattern 12900)
                    (List.replicate 24 Work @ [ Carry ] @ List.replicate 24 Move)
                    "twenty-four pairs and the Carry fill the body to 49 parts"
            }

            // The guard's five banks, one at a time: 300 (the RCL1 bank, under
            // one block), 800 and 1,300 (one block), 1,800 (two) and 2,300
            // (three). The parts come back grouped in the block's order — TOUGH,
            // MOVE, ATTACK, HEAL — and that order is the rule (#282): damage
            // strips a body from its head, so what stands first is spent first.
            // Live, with ATTACK second, a guard's whole damage sat inside the
            // first four hundred hits and it reached its target disarmed. Move
            // goes ahead of Attack because a guard that cannot walk is still a
            // guard, and Heal comes after Attack. One Move then goes last
            // (#672): a guard stripped to its Heal can still walk away.
            let guardBlock =
                [ Tough; Move; Move; Move; Move; Attack; Attack; Attack; Heal; Move ]

            let guardBlocks n =
                List.replicate n Tough
                @ List.replicate (5 * n - 1) Move
                @ List.replicate (3 * n) Attack
                @ List.replicate n Heal
                @ [ Move ]

            test "the guard row's one block is 90 damage, 12 self-heal and 750 energy" {
                // The 800 bank — the first that can pay for the row at all.
                Expect.equal
                    (bodyFor guardPattern 800)
                    guardBlock
                    "one whole block, parts grouped in the block's own order"

                Expect.equal
                    (bodyCost (bodyFor guardPattern 800))
                    750
                    "and the remaining 50 stays banked: the row buys whole blocks"
            }

            test "a bank under one block still sizes to one block, and the row yields" {
                // 300, the RCL1 bank. `wholeBlockBodyFor` floors at one block
                // exactly as the hauler and reserver rows do, so what happens at this
                // bank is not a smaller guard: it is a 750-energy body the bank
                // cannot pay for, and the cascade's affordability check yields the
                // tick to the rows behind it. The yielding itself is pinned at
                // `decide` in "the guard row" below.
                Expect.equal
                    (bodyFor guardPattern 300)
                    guardBlock
                    "the block is the row's minimal cast, however poor the bank"
            }

            test "the RCL4 bank buys the same one block as the RCL2 one" {
                // 1,300 against 800, the pair that says the row buys *whole*
                // blocks and never spends a remainder: 550 more energy buys
                // nothing at all, where the worker row would have padded it
                // into Carry and Move.
                Expect.equal
                    (bodyFor guardPattern 1300)
                    (bodyFor guardPattern 800)
                    "1,300 is one block short of two, so it casts the 800 bank's body"
            }

            test "the live RCL5 bank buys two blocks" {
                // 1,800: 180 damage, 24 self-heal, 2,000 hits — the pair of
                // Overmind units the research says beats every small raid but
                // the five-creep group.
                Expect.equal
                    (bodyFor guardPattern 1800)
                    (guardBlocks 2)
                    "two blocks: [T;T; M×9; A×6; H;H; M], TOUGH first and one MOVE last"
            }

            test "an RCL6 bank buys three" {
                // 2,300, the last of the five: 2,250 spent, 270 damage and 36
                // self-heal.
                Expect.equal
                    (bodyFor guardPattern 2300)
                    (guardBlocks 3)
                    "three blocks at 2,250 energy"
            }

            test "the guard body never exceeds the 50-part engine cap" {
                // Ten parts a block, so five blocks fill the body exactly and
                // an RCL8 bank's 12,900 would otherwise ask for seventeen.
                Expect.equal
                    (bodyFor guardPattern 12900)
                    (guardBlocks 5)
                    "five blocks fill the 50 parts exactly"
            }

            test "a row the generalist rule cannot size is refused, not quietly rebuilt" {
                // The fallback counts Work, Carry and Move out of a block and emits
                // only those, so a table row that is not one of those three used to
                // get a body with none of its own parts in it and no complaint from
                // the compiler (#155). The stop names the row and the part so the
                // fix (its own sizing rule) is legible from the message alone. A
                // healer and no longer the guard this case was written with: `guard`
                // is a real row with a sizing rule now, so the row that proves the
                // stop must be one the table still does not name.
                let healer =
                    {
                        Name = "healer"
                        Block = [ Heal; Move ]
                    }

                let message =
                    try
                        bodyFor healer 1800 |> ignore
                        "no exception"
                    with ex ->
                        ex.Message

                Expect.stringContains
                    message
                    "healer"
                    "the stop names the row that has no sizing rule"

                Expect.stringContains
                    message
                    "Heal"
                    "and the part the generalist rule would have dropped"
            }

            test "a Work/Carry/Move row the table does not name still sizes at parity" {
                // The stop is for the parts the rule cannot place, not for
                // rows the table has not grown yet: a block of Work, Carry
                // and Move is exactly what the generalist rule is written
                // for, whatever the row is called.
                let scout =
                    {
                        Name = "scout"
                        Block = [ Carry; Move ]
                    }

                Expect.equal
                    (bodyFor scout 300)
                    (List.replicate 3 Carry @ List.replicate 3 Move)
                    "three whole blocks, nothing dropped and nothing padded"
            }

            test "a row with no block at all is refused too, on both runtimes" {
                // The other shape the generalist rule cannot size, and the
                // one the two runtimes disagree about: .NET divides by
                // zero on the repeat count while the emitted JS reads
                // `~~(50 / 0)` as no repeats and pads a Carry/Move body
                // out of a row that asked for neither — the silent rebuild
                // #155 exists to end, visible only in the colony. The stop
                // names the row on both.
                let ghost = { Name = "ghost"; Block = [] }

                let message =
                    try
                        bodyFor ghost 1800 |> ignore
                        "no exception"
                    with ex ->
                        ex.Message

                Expect.stringContains message "ghost" "the stop names the row that has no block"

                Expect.stringContains
                    message
                    "no parts at all"
                    "and says which shape it is refusing to size"
            }

            test "the dismantler row builds whole Work/Move blocks and holds no Carry" {
                // A padded Carry would put the body on every energy Task and take it
                // off its own row's census.
                Expect.equal
                    (bodyFor dismantlerPattern 1000)
                    (List.replicate 6 Work @ List.replicate 6 Move)
                    "1,000 buys six blocks, and the 100 left buys nothing"

                Expect.equal
                    (bodyFor dismantlerPattern 3750)
                    (List.replicate 25 Work @ List.replicate 25 Move)
                    "and the richest bank stops at the 50-part cap"

                Expect.equal (bodyFor dismantlerPattern 100) [ Work; Move ] "never below one block"

                for capacity in 150..50..5600 do
                    Expect.isFalse
                        (bodyFor dismantlerPattern capacity |> List.contains Carry)
                        $"no Carry at capacity {capacity}"
            }

            test "every row's cast ends in a MOVE wherever it holds one" {
                // #672: the engine destroys parts from the head of the array and
                // charges fatigue on a dead part too, so a body whose MOVE all sit
                // in front is pinned for good once they are shot off. A body keeps
                // its last part a MOVE and can walk while any part lives.
                let oneBlock =
                    { largestSizing with
                        AnchorLegs = OutpostLegs
                        ReserverClaims = [ 1 ]
                        GuardBlocks = 1
                        RangerBlocks = 1
                        UpgraderWork = 3
                        DefenderBlocks = 1
                    }

                for sizing in [ largestSizing; oneBlock ] do
                    for row in patternTable do
                        for capacity in [ 300; 550; 800; 1300; 1800; 2300; 5600; 12900 ] do
                            let body = sizedBodyFor sizing row capacity

                            if List.contains Move body then
                                Expect.equal
                                    (List.last body)
                                    Move
                                    $"the {row.Name} row's cast at {capacity} ends in a MOVE: {body}"
            }

            test
                "the MOVE that goes last is the one nearest the tail, and a body without one is left alone" {
                Expect.equal
                    (withMoveLast [ Tough; Move; Move; Attack; Heal ])
                    [ Tough; Move; Attack; Heal; Move ]
                    "one MOVE leaves its place for the tail; the rest keep their order"

                Expect.equal
                    (withMoveLast [ Work; Carry; Move ])
                    [ Work; Carry; Move ]
                    "a body already ending in a MOVE is unchanged"

                Expect.equal
                    (withMoveLast [ Work; Carry ])
                    [ Work; Carry ]
                    "no MOVE, nothing to move"
            }

            test
                "the MOVE at the tail costs no row a part: every row's counts are its sizing rule's" {
                // Each row's part counts at three banks, as `main` cast them
                // before #672 moved one MOVE to the tail.
                let counts =
                    [
                        "worker", 800, [ Work, 4; Carry, 4; Move, 4 ]
                        "worker", 2300, [ Work, 11; Carry, 12; Move, 12 ]
                        "worker", 12900, [ Work, 16; Carry, 17; Move, 17 ]
                        "anchor", 800, [ Work, 6; Carry, 1; Move, 1 ]
                        "anchor", 2300, [ Work, 6; Carry, 1; Move, 1 ]
                        "anchor", 12900, [ Work, 6; Carry, 1; Move, 1 ]
                        "hauler", 800, [ Carry, 10; Move, 5 ]
                        "hauler", 2300, [ Carry, 30; Move, 15 ]
                        "hauler", 12900, [ Carry, 32; Move, 16 ]
                        "courier", 800, [ Carry, 20; Move, 10 ]
                        "courier", 2300, [ Carry, 20; Move, 10 ]
                        "courier", 12900, [ Carry, 20; Move, 10 ]
                        "reserver", 800, [ BodyPart.Claim, 1; Move, 1 ]
                        "reserver", 2300, [ BodyPart.Claim, 3; Move, 3 ]
                        "reserver", 12900, [ BodyPart.Claim, 19; Move, 19 ]
                        "upgrader", 800, [ Work, 5; Carry, 1; Move, 5 ]
                        "upgrader", 2300, [ Work, 15; Carry, 1; Move, 15 ]
                        "upgrader", 12900, [ Work, 24; Carry, 1; Move, 24 ]
                        "guard", 800, [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ]
                        "guard", 2300, [ Tough, 3; Move, 15; Attack, 9; Heal, 3 ]
                        "guard", 12900, [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ]
                        "ranger", 800, [ Move, 3; RangedAttack, 2; Heal, 1 ]
                        "ranger", 2300, [ Move, 9; RangedAttack, 6; Heal, 3 ]
                        "ranger", 12900, [ Move, 24; RangedAttack, 16; Heal, 8 ]
                        "defender", 800, [ Move, 6; Attack, 6 ]
                        "defender", 2300, [ Move, 10; Attack, 10 ]
                        "defender", 12900, [ Move, 10; Attack, 10 ]
                        "sapper", 800, [ Move, 25; Work, 25 ]
                        "probe", 800, [ Tough, 4; Work, 5; Move, 9 ]
                        "medic", 800, [ Move, 18; Heal, 18 ]
                        "miner", 800, [ Work, 7; Move, 2 ]
                        "miner", 2300, [ Work, 20; Move, 4 ]
                        "miner", 12900, [ Work, 20; Move, 4 ]
                        "dismantler", 800, [ Work, 5; Move, 5 ]
                        "dismantler", 2300, [ Work, 15; Move, 15 ]
                        "dismantler", 12900, [ Work, 25; Move, 25 ]
                    ]

                for name, capacity, expected in counts do
                    let row = patternTable |> List.find (fun row -> row.Name = name)

                    Expect.equal
                        (bodyFor row capacity |> List.countBy id |> Map.ofList)
                        (Map.ofList expected)
                        $"the {name} row's part counts at {capacity}"
            }

            test "spawn planning casts from the pattern table's row" {
                // An established colony at full capacity: the spawned body
                // is the table row sized to capacity, and the creep name
                // carries the row's name — not a hard-coded worker shape.
                let snapshot =
                    { bareRespawn with
                        Bank = bank 550 550
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                // The row by its name and not by its place in the table:
                // the declaration order is nobody's rule (see the pattern
                // table's own note), so a reordering must not fail here.
                let row = patternTable |> List.find (fun row -> row.Name = workerPattern.Name)

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body (bodyFor row 550) "body is the row repeated by capacity"
                    Expect.stringStarts creepName $"{row.Name}-" "creep name carries the row's name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// The rows' living counts off one tick with the given named bodies standing,
/// by row name.
let private livingByName (creeps: (string * BodyPart list) list) =
    let colony =
        { bareRespawn with
            Bank = bank 2300 2300
            Creeps = creeps |> List.map (fun (name, body) -> creepWith name 0 0 body)
        }

    (decideOn colony).Quotas.Rows
    |> List.map (fun row -> row.Row, row.Living)
    |> Map.ofList

[<Tests>]
let dismantlerCensusTests =
    testList
        "the dismantler's census"
        [
            test "the row's cast reads back as the dismantler off its name, whatever its size" {
                // Read off its parts it is a standing body — no Carry per Work —
                // and filled the upgrader row for its whole life.
                let rows =
                    livingByName
                        [
                            for capacity in [ 150; 1000; 3750 ] ->
                                $"dismantler-{capacity}-Spawn1", bodyFor dismantlerPattern capacity
                        ]

                Expect.equal
                    (Map.tryFind "dismantler" rows)
                    (Some 3)
                    "all three are the dismantler row's"

                Expect.equal (Map.tryFind "upgrader" rows) (Some 0) "and none is an upgrader"
            }

            test "no other row's body reads back as the dismantler" {
                // Every row the table holds, cast at every bank the colonies stand
                // at under the name the cascade gives it, plus the Anchor and the
                // miner at their smallest and a miner stripped to the dismantler's
                // own Work = Move (#426). A sapper and a probe are the dismantler's
                // parts too, each under its own name.
                let others =
                    patternTable
                    |> List.filter (fun row -> row.Name <> dismantlerPattern.Name)
                    |> List.collect (fun row ->
                        [
                            for capacity in [ 300; 550; 800; 1300; 1800; 2300; 5600 ] ->
                                $"{row.Name}-{capacity}-Spawn1", bodyFor row capacity
                        ])

                let smallest =
                    [
                        "anchor-1-Spawn1", [ Work; Work; Carry; Move ]
                        "miner-1-Spawn1", [ Work; Work; Move ]
                        "miner-2-Spawn1", [ Work; Work; Work; Work; Move; Move; Move; Move ]
                    ]

                Expect.equal
                    (livingByName (others @ smallest) |> Map.tryFind "dismantler")
                    (Some 0)
                    "no body without the row's name is the dismantler's"
            }
        ]

/// A body shot down to `standing` of the `cast` parts it was cast with: the
/// engine keeps a destroyed part in the array at zero hits, so the hits left
/// are the standing parts'.
let private shot name (cast: BodyPart list) (standing: BodyPart list) =
    { creepWith name 0 50 standing with
        Hits =
            {
                Hits = Engine.partHits * List.length standing
                HitsMax = Engine.partHits * List.length cast
            }
    }

[<Tests>]
let castRowCensusTests =
    testList
        "the row a body counts in is the one it was cast for (#664)"
        [
            test
                "an Anchor shot to Work ≤ Move still holds its row and its Post, and no Anchor is cast" {
                // Live t1042216: anchor-1042000-Spawn1 (6W/1C/1M) was shot to
                // one Work beside its Move; read off its standing parts it was
                // no Anchor, its Post read vacant, and a second Anchor was
                // cast for a seat the healed first one took back.
                let cast = [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                let crippled = shot "anchor-1042000-Spawn1" cast [ Work; Carry; Move ]

                let colony =
                    { richIncomeColony with
                        Creeps =
                            [
                                creepWith "anchor-1041000-Spawn2" 0 50 cast
                                crippled
                                hauler "hauler-1041000-Spawn3" 0 100
                            ]
                            @ [ for i in 1..4 -> worker $"worker-104100{i}-Spawn4" 0 50 ]
                        Spatial =
                            richIncomeColony.Spatial
                            |> withCreepsAt
                                [
                                    "anchor-1041000-Spawn2", { X = 11; Y = 10 }
                                    crippled.Name, { X = 29; Y = 10 }
                                ]
                    }

                let decision = decideOn colony

                Expect.isEmpty
                    (anchorCastsBy colony)
                    "no Anchor is cast for a Post its Anchor holds"

                Expect.equal
                    (decision.Quotas.Rows
                     |> List.tryFind (fun row -> row.Row = "anchor")
                     |> Option.map _.Living)
                    (Some 2)
                    "the shot Anchor counts in its row"

                Expect.equal
                    (Map.tryFind crippled.Name decision.Assignments)
                    (Some(taskId (Harvest "src-b")))
                    "and keeps digging at its Post with what still stands"
            }

            test "an Anchor shot to no Work at all still counts in the anchor row" {
                // It holds its Post too: its relief waits on a recycle (#665).
                let rows = livingByName [ "anchor-1042000-Spawn1", [ Move ] ]

                Expect.equal (Map.tryFind "anchor" rows) (Some 1) "the anchor row's"
                Expect.equal (Map.tryFind "worker" rows) (Some 0) "and not the generalist's"
            }

            test "an upgrader cast at four Work or fewer counts in the upgrader row" {
                // `3W/1C/3M` is no standing body by its parts (1 × 4 ≥ 3), so
                // read off them it was the generalist's; it was cast as an
                // upgrader and counts as one.
                let rows =
                    livingByName
                        [ "upgrader-1-Spawn1", [ Work; Work; Work; Carry; Move; Move; Move ] ]

                Expect.equal (Map.tryFind "upgrader" rows) (Some 1) "the upgrader row's"
                Expect.equal (Map.tryFind "worker" rows) (Some 0) "and not the generalist's"
            }

            test "a manned Post digs what its holder's standing Work takes of its rock" {
                // The hauler row's return is priced off this: a whole 6W
                // Anchor digs the held rock's ten, a 2W one its own four, one
                // shot to a single Work two, and a vacant Post nothing.
                let dugWith (holders: (string * BodyPart list * Pos) list) =
                    let colony =
                        { richIncomeColony with
                            Creeps =
                                holders
                                |> List.map (fun (name, body, _) -> creepWith name 0 50 body)
                            Spatial =
                                richIncomeColony.Spatial
                                |> withCreepsAt (
                                    holders |> List.map (fun (name, _, at) -> name, at)
                                )
                        }

                    let home = SpatialInfo.homeName colony.Spatial

                    Spawns.postsDug
                        (fun _ -> Engine.heldOutputPerTick)
                        [ RoomPos.at home { X = 11; Y = 10 }; RoomPos.at home { X = 29; Y = 10 } ]
                        colony.Creeps
                        (Atlas.ofView colony)

                let whole = List.replicate 6 Work @ [ Carry; Move ]

                Expect.equal
                    (dugWith
                        [
                            "anchor-1-Spawn1", whole, { X = 11; Y = 10 }
                            "anchor-2-Spawn1", [ Work; Work; Carry; Move ], { X = 29; Y = 10 }
                        ])
                    14
                    "ten off the whole Anchor's rock and four off the undersized one's"

                Expect.equal
                    (dugWith
                        [
                            "anchor-1-Spawn1", whole, { X = 11; Y = 10 }
                            "anchor-2-Spawn1", [ Work; Carry; Move ], { X = 29; Y = 10 }
                        ])
                    12
                    "two off the shot one's"

                Expect.equal
                    (dugWith [ "anchor-1-Spawn1", whole, { X = 11; Y = 10 } ])
                    10
                    "and nothing off a vacant Post"
            }

            test "a guard stripped of its ATTACK still counts in the guard row" {
                // Damage takes the head first: TOUGH, every MOVE, then ATTACK.
                let rows = livingByName [ "guard-972528-Spawn11", [ Heal ] ]

                Expect.equal (Map.tryFind "guard" rows) (Some 1) "the guard row's"
                Expect.equal (Map.tryFind "worker" rows) (Some 0) "and not the generalist's"
            }

            test "a reserver stripped of its CLAIM still counts in the reserver row" {
                let rows = livingByName [ "reserver-985957-Spawn11", [ Move ] ]

                Expect.equal (Map.tryFind "reserver" rows) (Some 1) "the reserver row's"
                Expect.equal (Map.tryFind "worker" rows) (Some 0) "and not the generalist's"
            }

            test "a body with no row's name is still read off its parts" {
                let rows = livingByName [ "c1", [ BodyPart.Claim; Move ]; "c2", [ Move ] ]

                Expect.equal (Map.tryFind "reserver" rows) (Some 1) "the CLAIM body is a reserver"
                Expect.equal (Map.tryFind "worker" rows) (Some 1) "the bare one a generalist"
            }
        ]

[<Tests>]
let squadRoleBodyTests =
    testList
        "the squad role bodies"
        [
            test "the medic is 18 MOVE then 18 HEAL, 5,400 energy" {
                Expect.equal
                    medicPattern.Block
                    (List.replicate 18 Move @ List.replicate 18 Heal)
                    "legs first, so its heal is the last thing damage takes"

                Expect.equal (bodyCost medicPattern.Block) 5400 "18 × 50 + 18 × 250"
            }

            test "the probe is 4 TOUGH, 5 WORK, then 9 MOVE: legs last, 990 energy (#493)" {
                Expect.equal
                    probePattern.Block
                    (List.replicate 4 Tough @ List.replicate 5 Work @ List.replicate 9 Move)
                    "no medic heals it: its legs are the last thing damage takes"

                Expect.equal (bodyCost probePattern.Block) 990 "4 × 10 + 5 × 100 + 9 × 50"
            }

            test "every role body walks a plain tile a tick: one MOVE per other part" {
                for role in [ probePattern; medicPattern ] do
                    let parts = partCountIn role.Block
                    Expect.equal (parts Move) (List.length role.Block - parts Move) role.Name
            }
        ]
