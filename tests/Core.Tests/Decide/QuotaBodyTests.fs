/// The body patterns and what each casts at a given bank (ADR 0006).
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
                "the worker unit, the Anchor, the hauler, the reserver, the upgrader, the guard and the miner are the table's rows" {
                // The reserver joined the table the tick its quota did (ADR
                // 0006, ADR 0042): a row arrives with the colony fact that
                // says when it is cast, and `reserverClaimsOf` is that
                // fact. The order here is the declaration's and not the
                // casting order — which runs guard, reserver, Anchor,
                // hauler, upgrader, worker — because nothing reads this list
                // for a sequence.
                //
                // The upgrader row spent one ticket ahead of its quota (ADR
                // 0046, #186) and is level with it again: #187 landed
                // `upgraderQuota` and the cascade rung, so every row here
                // is a row the colony casts off a colony fact. Its block is
                // the worker unit's three parts and its rule is not — one
                // Carry and Work/Move pairs for the rest — which is the
                // table saying that a row is a name and a sizing rule
                // before it is a block.
                //
                // The guard is the sixth row, and the one whose block is
                // bought for a fight rather than for energy (ADR 0056): no
                // Work and no Carry, its five Move there to carry the five
                // that fight. Ten parts, 750 energy, and their order is the
                // body's — TOUGH eats damage first, HEAL dies last.
                //
                // The [[miner]] is the seventh (ADR 0057 decision 2), and the
                // one bought for a resource the colony does not eat: Work and
                // Move and **no Carry at all**, which is the one shape no other
                // row here takes and so the cut `patternOfParts` tells it from
                // the Anchor by. Its block is three parts and its rule is not —
                // one Move per five Work, capped at twenty Work — which is this
                // table saying again that a row is a name and a sizing rule
                // before it is a block.
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
                            Name = "miner"
                            Block = [ Work; Work; Move ]
                        }
                    ]
                    "every body the colony casts comes from these rows"

                Expect.equal
                    (bodyCost guardPattern.Block)
                    750
                    "and the guard block is the 750 energy ADR 0056's whole arithmetic is written at"
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
                // 550 = the RCL2 full bank: 100 buys the Carry/Move pair,
                // the rest is Work — no parity padding (ADR 0006 exempts
                // the Anchor from fatigue parity).
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
                // ADR 0021: a source regenerates 3,000 energy per 300 ticks
                // and a Work digs 2 a tick, so five Work saturate it; the
                // sixth is slack for an unmanned Post's gap. RCL4's 1,300
                // bank would otherwise buy twelve.
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
                // 1,800 is the colony's live RCL5 bank (#116's deployment
                // note) and 650 a `[Claim; Move]` block, so the bank buys
                // two and strands 500: ADR 0042's own `[2Claim;2Move]`,
                // the body every arithmetic in that ADR is written for.
                // This is the bank's half of the row's rule and the body
                // `bodyFor` answers with, which is what a lead prices its
                // succession off (ADR 0026). What the row actually casts
                // is `min(reservation deficit, this)`, pinned in "the
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
                // ADR 0046's own body at the live RCL5 bank of 1,800:
                // `floor((1800 - 50) / 150)` is eleven pairs for 1,700,
                // against the worker row's nine Work at the same bank. The
                // Carry stays at one however rich the bank — a body that
                // stands beside the buffer needs one Withdraw's worth of
                // store and nothing more.
                Expect.equal
                    (bodyFor upgraderPattern 1800)
                    (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
                    "eleven Work, one Carry, eleven Move — Work first, legs last"
            }

            test "the upgrader row's minimal cast is one pair beside the Carry" {
                // The RCL1 bank, where `(300 - 50) / 150` is one before
                // any clamp: the row's smallest body is a worker unit's
                // parts under a different rule — and still `Work = Move`,
                // which is what keeps ADR 0016's gate open to it at every
                // size.
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
                // Where the sizing rule and ADR 0046's predicate cross: one
                // Carry against `floor((capacity - 50) / 150)` Work meets
                // `Carry * 4 < Work` at five pairs, so the RCL2 bank of 550
                // casts `3W/1C/3M` — this row's body, and outside the gate
                // the row exists for. Deliberate and not a hole: three Work
                // against a fifty-energy load is not yet the commute the
                // ratio prices. It is also where the quota stops: a row
                // read back to the generalist is a row nothing could count,
                // so `upgraderQuota` hires none at this bank (#187, ADR
                // 0046's amended Consequences; pinned at the seam by "a
                // bank whose cast is no standing body hires none").
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

            // ADR 0056's own five banks, one at a time so each answer is
            // read against exactly one other: 300 (the RCL1 bank, under one
            // block), 800 and 1,300 (one block), 1,800 (two) and 2,300
            // (three). The parts come back grouped in the **block's** order —
            // TOUGH, MOVE, ATTACK, HEAL — and that order is the rule (#282):
            // damage strips a body from its head, so what stands first is
            // spent first. Live, with ATTACK second, a guard's whole damage
            // sat inside the first four hundred hits and it reached its target
            // disarmed. Move goes ahead of Attack because a guard that cannot
            // walk is still a guard, and Heal stays last.
            let guardBlock =
                [ Tough; Move; Move; Move; Move; Move; Attack; Attack; Attack; Heal ]

            let guardBlocks n =
                List.replicate n Tough
                @ List.replicate (5 * n) Move
                @ List.replicate (3 * n) Attack
                @ List.replicate n Heal

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
                // exactly as the hauler and reserver rows do, so what happens
                // at this bank is **not** a smaller guard: it is a 750-energy
                // body the bank cannot pay for, and the cascade's
                // affordability check yields the tick to the rows behind it
                // (ADR 0050). A colony this small has ADR 0043's stand-down
                // and nothing else. The yielding itself is pinned at `decide`
                // in "the guard row" below.
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
                    "two blocks: [T;T; A×6; M×10; H;H], TOUGH first and HEAL last"
            }

            test "an RCL6 bank buys three" {
                // 2,300, and the last of ADR 0056's five: 2,250 spent, 270
                // damage and 36 self-heal.
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
                // The fallback counts Work, Carry and Move out of a block
                // and emits only those, so the next table row that is not
                // one of those three used to get a body with none of its own
                // parts in it and no complaint from the compiler (#155). The
                // stop names the row and the part so the fix (its own sizing
                // rule, ADR 0006) is legible from the message alone.
                //
                // A **healer** and no longer the guard this case was written
                // with: ADR 0056 landed `guard` as a real row with a sizing
                // rule of its own, so the row that proves the stop must be
                // one the table still does not name. The stop is what
                // admitted it safely — the row arrived with `guardBodyFor`
                // beside it because this gate refuses anything else.
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
