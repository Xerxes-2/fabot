/// The rows a colony casts from — hauler, worker, upgrader, Reserver — the
/// workforce target and per-source output they are sized from (ADR 0012, ADR
/// 0042), the body patterns they are cast with (ADR 0006), the supply floor in
/// front of them (ADR 0050), and the Tuning knobs that price them.
module Fabot.Core.Tests.Decide.QuotaTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

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
                "the worker unit, the Anchor, the hauler, the reserver, the upgrader and the guard are the table's rows" {
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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

[<Tests>]
let workforceTests =
    testList
        "workforce target"
        [
            // Two sources spaced apart: src-a with three Seats, src-b with
            // two — a Seat total of five.
            let fiveSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 30; Y = 30 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                        { X = 29; Y = 30 }, Plain
                        { X = 31; Y = 30 }, Plain
                    ]

            test "the Seat total raises the target above the floor" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = fiveSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "five Seats support five creeps; two living is a deficit"
            }

            test "no spawn Intent once the workforce matches the Seat total" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ for i in 1..5 -> worker $"w{i}" 0 50 ]
                        Spatial = fiveSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "workforce already at target"
            }

            test "a Seat total below the floor leaves the floor in charge" {
                let oneSeat = spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = oneSeat
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "one Seat cannot lower the target below the floor of two"
            }

            test "an unplaced source contributes no Seats" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = spatial [] []
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "only the floor applies"
            }
        ]

/// The hauler quota fixture: a 3-wide field y = 9..11 from x = 8 to one
/// tile past the spawn, the source embedded in wall at (10,10) with its
/// eight Seats open — a seat-based target roomy enough to leave the
/// hauler row slots — the built source container "can-src" on the Seat
/// (11,10) (a Post), and the spawn structure standing at (spawnX,10).
let quotaRoom spawnX =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "can-src", { X = 11; Y = 10 }
              "spawn-1", { X = spawnX; Y = 10 }
          ]
          [
              for x in 8 .. spawnX + 1 do
                  for y in 9..11 -> { X = x; Y = y }, (if x = 10 && y = 10 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "can-src", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = spawnX; Y = 10 }
        })

/// The quota fixture's colony: `spawnCount` idle spawns drawing on the one
/// 300-capacity bank holding `available` energy.
let quotaColony spawnX spawnCount available =
    { bareRespawn with
        Spawns =
            [
                for i in 1..spawnCount ->
                    { spawn with
                        Name = $"Spawn{i}"
                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                    }
            ]
        Bank = bank available 300
        Sources = [ source "src-a" ]
        Spatial = quotaRoom spawnX
    }

let haulerCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "hauler-")
    |> List.length

/// A room whose source containers are a two- and a three-step paved haul
/// from the spawn — the live W12S28 geometry the round trip's repricing
/// was measured against (ADR 0029), flattened onto one paved lane. Sources embedded in
/// wall at (8,10) and (17,10), their built containers on the Seats (9,10)
/// and (16,10) — two Posts — and the spawn structure standing at (12,10),
/// whose free neighbours are (11,10) and (13,10): two steps from the first
/// container, three from the second.
let shortHaulRoom =
    { spatial
          [
              "src-a", { X = 8; Y = 10 }
              "src-b", { X = 17; Y = 10 }
              "can-a", { X = 9; Y = 10 }
              "can-b", { X = 16; Y = 10 }
              "spawn-1", { X = 12; Y = 10 }
          ]
          [
              for x in 8..17 -> { X = x; Y = 10 }, (if x = 8 || x = 17 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList [ for x in 9..16 -> { X = x; Y = 10 } ]
            Obstacles = Set.singleton { X = 12; Y = 10 }
        })

[<Tests>]
let haulerTests =
    testList
        "hauler"
        [
            test "a farther source container hires a larger hauler quota" {
                // Near spawn: 8 steps from the container, [4 Carry; 2 Move]
                // at the 300-capacity bank — each leg a walk (ADR 0029),
                // 2 ticks a loaded step and 1 empty, so 24 round-trip ticks
                // and quota ceil(24 x 4 / 200) = 1. Far spawn: 27 steps —
                // 81 ticks, quota 2. The rock ships four and not ten
                // because that is what the Anchor this bank casts digs out
                // of it (#208). The living Anchor fills the Post, so every
                // remaining specialist gap is a hauler cast, and five idle
                // spawns on a 1500 bank can pay for the larger quota.
                //
                // The generalist standing beside the Anchor is the supply
                // floor's premise (ADR 0050) and nothing else: it is not a
                // hauler, so the row this case counts is untouched.
                let decideAt spawnX =
                    decide
                        { quotaColony spawnX 5 1500 with
                            Creeps = [ anchor "a1" 0 50; worker "w1" 0 50 ]
                        }
                        Map.empty
                        Set.empty
                        None

                let near = decideAt 20
                let far = decideAt 39

                Expect.equal (haulerCasts near.Intents) 1 "8 steps ship in one body"
                Expect.equal (haulerCasts far.Intents) 2 "27 steps hire two"
            }

            test "the repricing does not move the measured room's quota: the fix was not a resizing" {
                // ADR 0029 repriced each leg of the round trip as a walk,
                // and both legs got dearer — the live room's two containers
                // move from 3 and 5 round-trip ticks to 4 and 6. The pair's
                // demand rises with them, from 80 to 100 energy-ticks
                // against the 200-carry hauler a 300 bank casts, and stays
                // under the one body that clears it — the live room's
                // 16-Carry body wants a second only past 80 ticks — so the
                // fleet is the size it was. The error this corrects bites
                // at remote-mining distances, not at home; a future reader
                // finding the fleet unchanged is looking at the right
                // outcome, not at a fix that failed to land.
                //
                // One body and not one apiece since ADR 0049: the two
                // containers are summed and rounded once, which is what
                // took this room's row from two down to one. The claim
                // under test is unmoved by that — both roundings, before
                // ADR 0029's repricing and after it, still land on the same
                // fleet.
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                for i in 1..4 ->
                                    { spawn with
                                        Name = $"Spawn{i}"
                                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                                    }
                            ]
                        Bank = bank 1200 300
                        Sources = [ source "src-a"; source "src-b" ]
                        Spatial = shortHaulRoom
                        // The generalist beside the two Anchors is the
                        // supply floor's premise and not this case's (ADR
                        // 0050): an Anchor holds a Carry and can still put
                        // nothing into an extension, so a fleet of Anchors
                        // alone hires a carrier sized from `bank.Available`
                        // in front of every row, and the cast this case
                        // counts would be that one instead of the hauler
                        // row's own capacity-sized body.
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50; worker "w1" 0 50 ]
                    }

                let atlas = Atlas.ofView snapshot
                let haulerBody = [ Carry; Carry; Carry; Carry; Move; Move ]

                // Container and spawn alike stand in the colony's own
                // room, which the rooms now ride on the API for (#149): a
                // home round trip is the one-room flood it always was.
                let home = SpatialInfo.homeName snapshot.Spatial

                let roundTrip from =
                    Atlas.haulRoundTripTicks
                        atlas
                        haulerBody
                        (RoomPos.at home from)
                        (RoomPos.at home { X = 12; Y = 10 })

                Expect.equal
                    (roundTrip { X = 9; Y = 10 })
                    (Some 4)
                    "two paved steps out and back, a tick a tile: 3 ticks became 4"

                Expect.equal
                    (roundTrip { X = 16; Y = 10 })
                    (Some 6)
                    "three paved steps out and back: 5 ticks became 6"

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (haulerCasts intents)
                    1
                    "the same one body the halved round trip hired, summed and rounded once"

                Expect.equal
                    (spawnIntents intents
                     |> List.filter (fun (_, _, name: string) -> name.StartsWith "hauler-")
                     |> List.map (fun (_, body, _) -> body))
                    [ haulerBody ]
                    "and it is the row's own capacity-sized body, so no carrier sized from what \
                     happens to be banked can answer this count (ADR 0050)"
            }

            test "no source containers hires no haulers" {
                let room = quotaRoom 39

                let snapshot =
                    { quotaColony 39 4 1200 with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { room with
                                TargetKinds = Map.remove "can-src" room.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions = Map.remove "can-src" layer.TargetPositions
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal (haulerCasts intents) 0 "no container, nothing to ship"

                Expect.all
                    (spawnIntents intents)
                    (fun (_, _, name) -> name.StartsWith "worker-")
                    "every cast is the generalist row"
            }

            test "the reserver row leads and is empty here: Anchor, hauler, worker follow" {
                // The 27-step round trip hires two haulers, so the order
                // runs Anchor, both haulers, then the generalist — four
                // casts, one per idle spawn, off the one debited bank. The
                // far spawn and not the near one since #208: at four a tick
                // an 8-step haul is one body, and a second hauler is what
                // makes the order readable.
                //
                // The reserver row runs in front of all three (ADR 0042)
                // and casts nothing at all here: a colony projecting one
                // room has no outpost to declare, so that row's quota is
                // zero and its gap with it. A reserver appearing in this list
                // would mean the gap had been computed unconditionally.
                let snapshot =
                    { quotaColony 39 4 1200 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, firstBody, firstName)
                    (_, secondBody, secondName)
                    (_, thirdBody, thirdName)
                    (_, fourthBody, fourthName) ] ->
                    Expect.stringStarts firstName "anchor-" "the Post's gap is filled first"
                    Expect.equal firstBody [ Work; Work; Carry; Move ] "the Anchor row's body"
                    Expect.stringStarts secondName "hauler-" "the hauler quota comes second"

                    Expect.equal
                        secondBody
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        "two whole blocks at the 300 bank"

                    Expect.stringStarts
                        thirdName
                        "hauler-"
                        "the quota is filled before the remainder"

                    Expect.equal thirdBody secondBody "the row casts one body"
                    Expect.stringStarts fourthName "worker-" "the generalist fills the remainder"
                    Expect.equal fourthBody (workerBodyFor 300) "the worker row sized to the bank"
                | other -> failtest $"expected exactly four SpawnCreep intents, got %A{other}"
            }

            test "the disaster fallback still casts a bare worker unit" {
                let snapshot =
                    { quotaColony 20 1 300 with
                        Creeps = []
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body [ Work; Carry; Move ] "time-to-first-creep outranks the rows"
                    Expect.stringStarts creepName "worker-" "the fallback casts the worker row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an empty hauler body is matched into Withdraw" {
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 12; Y = 10 } ]
                                })
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-src")))
                    "the intake half of the haul cycle: free capacity beside a stocked container"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "can-src"))
                    "in range at tick start: the withdraw fires"
            }

            test "a hauler's intake is a source container, never the upgrade buffer" {
                // The buffer stands one step away and the source container
                // six, yet a body with no Work part can spend nothing at the
                // controller: drawing from the buffer only sends energy back
                // the way it came.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 17; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-src")))
                    "the buffer is inapplicable by body; the far source container is the intake"
            }

            test "the buffer stays intake for a Work body: the worker row still draws" {
                // The gate reads the target's kind beside the body, so the
                // colony's own worker row — four Work, four Carry, four
                // Move — keeps the buffer it upgrades from.
                let snapshot =
                    { haulColony with
                        Creeps =
                            [
                                creepWith
                                    "w1"
                                    0
                                    100
                                    [
                                        Work
                                        Work
                                        Work
                                        Work
                                        Carry
                                        Carry
                                        Carry
                                        Carry
                                        Move
                                        Move
                                        Move
                                        Move
                                    ]
                            ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "a body that can spend at the controller draws from the buffer beside it"
            }

            test "with only the buffer stocked, a hauler idles instead of cycling it" {
                // The loop this gate closes: every other sink full, the
                // hauler's only Refill target is the buffer it just drew
                // from, so it emptied and refilled the same container tick
                // after tick without ever delivering.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 17; Y = 10 } ]
                                })
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal (Map.tryFind "h1" assignments) None "no intake a hauler may draw from"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("h1", IdleReason.NoneApplicable))
                    "the idle Verdict names the body gate"
            }

            test "filled, the hauler's Withdraw releases and rematches to Refill" {
                // No Work part: Harvest, Build, Upgrade and Repair are
                // inapplicable by body, so the outflow is the only work
                // left — the same emergent alternation as every Task pair.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 100 0 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 12; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "can-src") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("h1", taskId (Withdraw "can-src"), ReleaseReason.Inapplicable))
                    "the full store releases Withdraw"

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the rematch flips to the outflow"
            }
        ]

/// The arms of the paved cross below, each an id, its container's tile and
/// the rock that container serves — the rock a tile off the lane, range 1
/// from its own container and out of reach of every other. Four arms are
/// fixed and the north one is the caller's, which is the single tile the
/// pairwise case moves.
///
/// The distances are chosen to make the demands read off the page. At a
/// 300 bank the hauler row casts `[4Carry;2Move]` — 200 of carry — the
/// Anchor row casts `2W/1C/1M` and so a Post ships the four a tick that
/// body digs (#208), and a paved step is a tick on either leg: a container
/// `n` steps from the tile a transfer reaches the spawn from is a round
/// trip of `2n` ticks and a demand of `2n × 4 / 200 = n / 25` of a body,
/// 0.52 apiece thirteen steps out and 0.2 apiece five steps out.
let private haulRoundingArms north =
    [
        "e13", { X = 39; Y = 25 }, { X = 39; Y = 24 }
        "e5", { X = 31; Y = 25 }, { X = 31; Y = 24 }
        "w13", { X = 11; Y = 25 }, { X = 11; Y = 24 }
        "w5", { X = 19; Y = 25 }, { X = 19; Y = 24 }
        "n", north, { north with X = 24 }
    ]

/// The colony those arms make: a paved lane down row 25 and column 25 of
/// the colony's own room with the spawn standing at their crossing, and
/// one built source container on each arm. No controller and no
/// refillable, so the hauler quota is the only thing this fixture answers
/// — three outpost containers and two home ones are the live shape the
/// rounding was measured on (#194), and one room's arms are that shape's
/// arithmetic without a Seam to price it through.
let private haulRoundingColony arms =
    let spawnPos = { X = 25; Y = 25 }

    let lane =
        [ for x in 4..46 -> { X = x; Y = 25 } ]
        @ [ for y in 4..24 -> { X = 25; Y = y } ]

    let targets =
        arms
        |> List.collect (fun (name, tile, rock) ->
            [
                $"can-{name}", tile, Structure BuiltKind.Container
                $"src-{name}", rock, Source
            ])

    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = arms |> List.map (fun (name, _, _) -> source $"src-{name}")
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList (
                            [ for tile in lane -> tile, Plain ]
                            @ [ for (_, _, rock) in arms -> rock, Wall ]
                        )
                    TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
                    Obstacles = Set.singleton spawnPos
                    Roads = Set.ofList lane
                })
            |> withTargets targets
    }

/// The hauler body this fixture's bank casts, read from the row's own
/// sizing rule rather than restated as a literal: the premise assertions
/// below price their round trips off the Atlas, and a body spelled by hand
/// here would go on passing them against a body the colony no longer
/// casts while the quota beside them moved.
let private haulRoundingBody = bodyFor haulerPattern 300

[<Tests>]
let haulRoundingTests =
    testList
        "the hauler quota rounds once for the colony"
        [
            test "the row is the sum of the demands, never the sum of their ceilings" {
                // ADR 0049, on the live shape's own arithmetic: three
                // containers wanting 0.52 of a hauler and two wanting 0.2
                // come to 1.96 and hire **two**. A ceiling apiece — the
                // rule until #194 — bought a body for each of those five
                // fractions and hired five: three bodies the flow never
                // asked for, which is the overhire the live colony was
                // seen carrying.
                let colony = haulRoundingColony (haulRoundingArms { X = 25; Y = 11 })
                let atlas = Atlas.ofView colony
                let home = SpatialInfo.homeName colony.Spatial

                let roundTrip from =
                    Atlas.haulRoundTripTicks
                        atlas
                        haulRoundingBody
                        (RoomPos.at home from)
                        (RoomPos.at home { X = 25; Y = 25 })

                Expect.equal
                    (roundTrip { X = 39; Y = 25 })
                    (Some 26)
                    "the premise: thirteen paved steps out and back, a tick a tile"

                Expect.equal
                    (roundTrip { X = 31; Y = 25 })
                    (Some 10)
                    "the premise: and five steps out and back on the short arm"

                Expect.equal
                    (quotaOf colony)
                    2
                    "3 × 0.52 + 2 × 0.2 = 1.96, and the colony hires two"
            }

            test "one container of the same colony still rounds up on its own" {
                // The granularity is all that moved (ADR 0049): a colony
                // with one container rounds one demand up exactly as ADR
                // 0012 always did, because a sum of one is its own
                // summand. Pairwise against the case above, whose long arm
                // this is.
                let alone = haulRoundingColony [ List.head (haulRoundingArms { X = 25; Y = 11 }) ]

                Expect.equal (quotaOf alone) 1 "0.52 of a body alone is still a whole body"
            }

            test "a longer round trip on one arm moves the row by a body" {
                // What the sum buys, read pairwise on one tile: the north
                // container moves from thirteen steps out to twenty, its
                // own demand from 0.52 to 0.8, and the colony's from 1.96
                // to 2.24. Under a ceiling apiece that arm was one body
                // either way and the whole move was invisible; under one
                // rounding the fraction it grew by is a hire.
                let near = haulRoundingColony (haulRoundingArms { X = 25; Y = 11 })
                let far = haulRoundingColony (haulRoundingArms { X = 25; Y = 4 })
                let atlas = Atlas.ofView far
                let home = SpatialInfo.homeName far.Spatial

                Expect.equal
                    (Atlas.haulRoundTripTicks
                        atlas
                        haulRoundingBody
                        (RoomPos.at home { X = 25; Y = 4 })
                        (RoomPos.at home { X = 25; Y = 25 }))
                    (Some 40)
                    "the premise: twenty paved steps out and back"

                Expect.equal (quotaOf near) 2 "the premise: 1.96 of a body hires two"
                Expect.equal (quotaOf far) 3 "and 2.24 hires three, off one arm's seven extra steps"
            }

            test "one divisor for the colony too: the row's own body at the colony's bank" {
                // The other half of "rounded once" (ADR 0049), and the one
                // no arithmetic case above can see: the sum is divided by
                // **one** load and never by a load per spawn. The load is
                // the row's own cast at `richestCapacity` — the same body
                // `workforceTarget` charges this row's amortization at and
                // the same one a Withdraw's cap divides its store's stock
                // by (#161) — so a quota denominated anywhere else would
                // let the three disagree about what one hauler carries and
                // hire bodies the caps never admit.
                //
                // The same five arms, the same 980 energy-ticks of haul,
                // and only the bank moves: a 600 bank's hauler carries 400
                // and the colony hires three, an RCL4 bank's carries 800
                // and it hires two. Road parity holds at every size, so
                // the round trips themselves do not move with the body —
                // asserted, because if they did the two numbers would not
                // be one division apart.
                //
                // The lean bank here is 600 and not this fixture's own 300,
                // so that the *divisor* is the only thing moving (#208). A
                // Post is worth what the Anchor row's cast digs, capped at
                // the rock's rate, and at 300 that cast is `2W` and digs
                // four: the numerator would fall with the bank beside the
                // denominator and the pair would no longer be one division
                // apart. At 600 the cast is five Work — ten a tick, the
                // rate — and at 1,300 it is six, so both banks price these
                // rocks at the same ten and the haul is the same 980
                // either side.
                let colony =
                    { haulRoundingColony (haulRoundingArms { X = 25; Y = 11 }) with
                        Bank = bank 600 600
                    }

                let rich = { colony with Bank = bank 1300 1300 }

                let atlas = Atlas.ofView rich
                let home = SpatialInfo.homeName rich.Spatial

                Expect.equal
                    (Atlas.haulRoundTripTicks
                        atlas
                        (bodyFor haulerPattern 1300)
                        (RoomPos.at home { X = 39; Y = 25 })
                        (RoomPos.at home { X = 25; Y = 25 }))
                    (Some 26)
                    "the premise: the sixteen-Carry body pays the same paved tick a tile"

                Expect.equal (quotaOf colony) 3 "the premise: 980 energy-ticks over a 400 load"

                Expect.equal (quotaOf rich) 2 "and over the 800 load the same bank casts, two"
            }
        ]

/// The same colony at a bank the real W12S28 banks: the geometry is
/// untouched — same sources, same containers, same four idle spawns — and
/// only the bank moves, to 1300 against 1300, so every row's body grows
/// with it. Anchor 6W/1C/1M = 700, hauler 16C/8M = 1200 (16 Carry is 800
/// capacity, so the pair's 480 energy-ticks of demand is 0.6 of a body and
/// the row is one), worker 6W/7C/7M — a Work drain of 6. That drain is the
/// granularity the worker row's rounding is paid in, and it grows with
/// RCL: the fixture the row was pinned at banks 300, where the drain is 1
/// and a lost fraction is worth 0.8 e/tick.
///
/// It is also the rich half of #208's pair. The Anchor row's cast here is
/// 6 Work — twelve a tick, over the ten the owned room pays — so the cap
/// is not binding and each Post is worth the room's rate exactly as it was
/// before that ticket: 20 a tick of income, where the same geometry at the
/// 300 bank above earns 8. Every number in this fixture's cases is
/// therefore unmoved by #208, which is the half of the pair that says the
/// rule is a cap and not a discount.
let richIncomeColony =
    { incomeColony with
        Bank = bank 1300 1300
    }

/// The rich bank's fleet at a given worker count: the rows its quotas
/// pin — one Anchor per Post (2) beside the one hauler this bank's body
/// clears both containers with (ADR 0049) — and as many workers as the
/// case under it is pinning.
let richIncomeFleet workers =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100 ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

[<Tests>]
let incomeWorkforceTests =
    testList
        "income-based workforce"
        [
            test "the W12S28 fleet is the whole target: 2 Anchors + 1 hauler + 8 workers" {
                // Each posted source retires its 8 Seats: a seat base would
                // add 16 on top and the idle spawns would cast into it.
                let snapshot =
                    { incomeColony with
                        Creeps = incomeFleet
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "the fleet already matches the target"
            }

            test "amortization is deducted: one worker short casts exactly one worker" {
                // 30,000 of lifetime income less 2 × 700 of Anchors and
                // 1 × 1,800 of hauler, over the row's own 9 × 1500, is
                // 1.985 bodies and hires two. Undeducted the same income
                // is 2.22 and hires three, so this fleet would be two
                // short and the four idle spawns would draw two casts.
                //
                // Read at the 1,800 bank and not at `incomeColony`'s 300
                // since #208: there the deduction is 900 against 12,000 of
                // income over a worker drain of one, which is eight bodies
                // whether it is taken or not — a case that could no longer
                // fail if the term were dropped altogether.
                let snapshot =
                    { richestIncomeColony with
                        Creeps = richestIncomeFleet 1
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the gap is a worker gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at the 1800 bank the whole fleet is 2 Anchors + 1 hauler + 2 workers" {
                // The premise the case above rests on, and #208's upper
                // half: nothing here is capped by the Anchor row's cast,
                // so this is the fleet the room's own rate hires.
                let snapshot =
                    { richestIncomeColony with
                        Creeps = richestIncomeFleet 2
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "the fleet already matches the target"
            }

            test "at a 1300 bank the whole fleet is 2 Anchors + 1 hauler + 4 workers" {
                // One Anchor per Post (2), one hauler for both containers
                // (0.6 of a body at this bank's 800 carry capacity, rounded
                // once for the colony — ADR 0049), and the income workers
                // — 30,000 of lifetime income less 2 × 700 + 1 × 1200 of
                // amortization over the row's 6 × 1500 → ceil(3.044) = 4.
                let snapshot =
                    { richIncomeColony with
                        Creeps = richIncomeFleet 4
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "the fleet already matches the target"
            }

            test "the worker row rounds up at a Work drain of 6, not down to a body short" {
                // The whole defect at one RCL: 30,000 of lifetime income
                // less 2,600 of anchor and hauler amortization over the
                // worker row's 6 × 1500 is 3.044 bodies. Truncating gives
                // 3 and pins upgrade throughput whatever the surplus is;
                // rounding up gives the fourth body (ADR 0037). The fleet
                // below is three workers, so only the rounded-up target
                // has a gap to cast into.
                let snapshot =
                    { richIncomeColony with
                        Creeps = richIncomeFleet 3
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the third body is the gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

[<Tests>]
let outpostWorkforceTests =
    testList
        "an outpost source and the workforce target"
        [
            // The premise every case below is read against: the W12S28
            // fleet is the whole target, so an empty spawn list means the
            // target did not move and a non-empty one says how far it did.
            let atTarget =
                { incomeColony with
                    Creeps = incomeFleet
                }

            let rock = { X = 40; Y = 40 }

            test "an outpost source with no container leaves the target exactly where it was" {
                // ADR 0042's most important regression, and the reason the
                // constant and this narrowing land in one commit. The
                // unposted-seat rule counts a source's Seats into the target
                // because "its output is spoken for by the seat crews that
                // walk it" — which presumes the walk is cheap. Across a
                // border it is not: the three declared outpost sources carry
                // six Seats between them, five of them swamp, and counted
                // here they would hire six generalists to commute
                // forty-seven to fifty-six tiles to dig them.
                //
                // So a source whose room has no container is a source the
                // quotas cannot see, and the proof of it is that the colony
                // decides what it decides with the room not there at all.
                let withOutpostSource =
                    incomeColonyPlus (
                        withOutpost "W1N2" [ "src-out", rock, Source ] (threeSeatField rock)
                    )

                Expect.isEmpty
                    (spawnIntents (decide atTarget Map.empty Set.empty None).Intents)
                    "the premise: the fleet already matches the target"

                Expect.equal
                    (spawnIntents (decide withOutpostSource Map.empty Set.empty None).Intents)
                    (spawnIntents (decide atTarget Map.empty Set.empty None).Intents)
                    "three Seats a room away hire nobody: the same colony casts the same bodies"
            }

            test "the same rock in the spawn room still contributes its Seats" {
                // The other half of the pair, and the only reading under
                // which the case above says anything: the same source, the
                // same three Plain Seats, the same fleet — filed under the
                // home room's layer rather than an outpost's. ADR 0042
                // narrows the unposted-seat rule to the spawn room and does
                // not repeal it, so this colony hires the three walkers it
                // always did. Without this case a rule that counted no
                // Seats anywhere would pass the one above.
                let atHome =
                    incomeColonyPlus (fun colony ->
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withTargets [ "src-out", rock, Source ]
                                |> withHome (fun layer ->
                                    { layer with
                                        Terrain =
                                            (layer.Terrain, threeSeatField rock)
                                            ||> List.fold (fun acc (tile, terrain) ->
                                                Map.add tile terrain acc)
                                    })
                        })

                Expect.hasLength
                    (spawnIntents (decide atHome Map.empty Set.empty None).Intents)
                    3
                    "three Seats at home raise the target by three, and the idle spawns cast into it"
            }

            test "an outpost Seat on a home Post's coordinates posts nothing" {
                // The trap ADR 0042 names as the most dangerous in the
                // whole set, at the one query that was still asking it
                // room-blind. A `Pos` carries no room, so testing an
                // outpost source's Seats against the *home* room's Posts
                // answers yes on a bare coordinate collision — and that
                // outpost source then reads as posted with no container
                // under it, puts ten energy a tick of income that does not
                // exist into the base, and the colony hires the workers to
                // spend it.
                //
                // The rock stands at (11,11) of the outpost, so its Seat
                // (11,10) is the very tile the home room's `can-a` stands
                // on. Judged in the source's own room (`Atlas.postsOf`) the
                // collision means nothing whatever.
                let collidingRock = { X = 11; Y = 11 }

                let colliding =
                    incomeColonyPlus (
                        withOutpost
                            "W1N2"
                            [ "src-out", collidingRock, Source ]
                            (threeSeatField collidingRock)
                    )

                Expect.contains
                    (Atlas.postsIn (Atlas.ofView atTarget) (SpatialInfo.homeName atTarget.Spatial))
                    { X = 11; Y = 10 }
                    "the premise: (11,10) really is a Post of the home room"

                Expect.equal
                    (spawnIntents (decide colliding Map.empty Set.empty None).Intents)
                    (spawnIntents (decide atTarget Map.empty Set.empty None).Intents)
                    "a home container on the coordinates of an outpost Seat is no Post of that source's"
            }

            test "an outpost source moves no tile of the home room's Layout" {
                // The fourth room-blind `Pos` join of the family the ticket's
                // Traps section says to verify rather than assume, and the
                // one that was still open: the Layout plans off
                // `snapshot.Sources`, which since #124 is every scanned
                // room's. An outpost source counted there widens the footing
                // reservation by a slot and moves the clustered picks with
                // it, floods a trunk *at home* from the outpost's
                // coordinates, and plants a container site on a home tile
                // that is a Seat of a source a room away. ADR 0042: "The
                // outpost gets a container and nothing else. No roads, and
                // no Layout."
                //
                // The rock stands at (15,30), which is plain walkable ground
                // of the *home* fixture as well — that is what makes the
                // phantom trunk routable and the collision real rather than
                // theoretical.
                let colony = trunkColony 2
                let rock = { X = 15; Y = 30 }

                let joined =
                    { colony with
                        Sources = colony.Sources @ [ source "src-out" ]
                    }
                    |> withOutpost "W1N2" [ "src-out", rock, Source ] (threeSeatField rock)

                Expect.isNonEmpty
                    (placementIntents (decide colony Map.empty Set.empty None).Intents)
                    "the premise: this colony really does place a plan to move"

                Expect.equal
                    (placementIntents (decide joined Map.empty Set.empty None).Intents)
                    (placementIntents (decide colony Map.empty Set.empty None).Intents)
                    "the same room plans the same tiles: a source a room away is no source of its"
            }
        ]

[<Tests>]
let sourceOutputTests =
    testList
        "a source's output and the room that holds it"
        [
            // Ten is the *reserved* rate (ADR 0042). The colony that holds
            // W1N2 counts ten energy a tick from its rock and the colony
            // that does not counts five, and five over a 1,500-tick
            // lifetime is two worker places at this bank's Work drain of
            // three. So the fleet below is sized to the *unreserved*
            // target: the unreserved colony has no gap to cast into and
            // the reserved one does, which is a difference no shared cap
            // and no one-body-per-spawn limit can hide.
            //
            // Unreserved the target is 3 Anchors — the outpost's Post
            // hires one since #129 — + 2 haulers +
            // ceil(((20 + 5) × 1500 − 3000) / 4500) = 8 workers = 13;
            // reserved it is 10 workers and 15. The amortization is three
            // Anchors at 600 and two haulers at 600, every one of them the
            // body this 600 bank casts.
            let unreservedWorkers = 8

            test "the same outpost source is worth twice as much reserved" {
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            { midIncomeColony with
                                Creeps = incomeFleetOf 7
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "the premise: without the outpost this bank's whole target is 2 Anchors, \
                     2 haulers and 7 workers, and `incomeFleetOf` spells it a worker count at \
                     a time"

                Expect.isNonEmpty
                    (Atlas.postsOf
                        (Atlas.ofView (postedOutpostColony unreservedWorkers []))
                        "src-out")
                    "the premise: the container standing on its Seat makes the rock a Post"

                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", neutralRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "unreserved, the rock is worth five a tick and the fleet already matches"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony
                                unreservedWorkers
                                [ "W1N2", reservedRoom true 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "reserved, the same rock is worth ten and the colony hires against it"
            }

            test "a reservation another player holds doubles nothing of ours" {
                // Pairwise, one rival at a time: the same room, the same
                // rock, the same reservation standing on the same
                // controller — only whose it is moves. The engine pays
                // ten a tick in a room a rival holds as readily as in one
                // we hold (docs/research/remote-mining.md §1.1); the
                // colony prices it at five anyway, because a room
                // somebody else holds is one it is withdrawing from.
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony
                                unreservedWorkers
                                [ "W1N2", reservedRoom false 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "another player's reservation prices the rock exactly as none at all does"

                // Five and specifically not nothing. The assertion above
                // is sized to the neutral target, so it would hold just
                // as well if a rival's reservation made the rock
                // *unpriceable* — the answer ADR 0004 reserves for a room
                // with no vision. This fleet is the blind target below,
                // which the neutral rate outgrows and the blind one does
                // not, so the branch is pinned strictly between the two.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony 6 [ "W1N2", reservedRoom false 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's reservation prices the rock at five and hires, not at nothing"
            }

            test "a room another player owns doubles nothing of ours either" {
                // The other half of "somebody else holds it", and the one
                // the projection could not tell from an unowned room until
                // #133: a rival's *ownership*. ADR 0043 withdraws from
                // either half — since #165 the owned half is the latch and
                // the reserved half a clock — so either has to be a fact the
                // ColonyView can state, and stating it must not accidentally
                // read as a hold of ours, which is what this pins.
                //
                // Pairwise against the neutral room, one rival at a time:
                // same room, same rock, same container, same fleet. The
                // engine pays ten a tick in a room a rival owns exactly as
                // in one we own (`sources/tick.js` switches on
                // `roomController.user || roomController.reservation`); the
                // colony prices it at five for the same reason it prices a
                // rival's reservation at five.
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", rivalRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's ownership prices the rock exactly as nobody's does"

                // Five and specifically not ten, which is the failure a
                // three-state owner exists to make unrepresentable: read
                // as "owned, therefore held", the same rock would be worth
                // ten and this fleet would be five worker places short.
                // The one input that moves between this and the assertion
                // above is whose the controller is.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", ownedRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned by us the same rock is worth ten, so the fleet above is the neutral one"

                // Five and specifically not nothing, the same strict
                // bracket the reservation case is pinned in: sized to the
                // blind target, the neutral rate hires and unpriceable
                // does not.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony 6 [ "W1N2", rivalRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's ownership prices the rock at five and hires, not at nothing"
            }

            test "the NPC's reservation prices like a rival's and is not the same fact" {
                // The third holder (ADR 0043). A level-0 invader core
                // `attackController`s the room it expanded into and holds
                // the reservation itself — the measured core two rooms
                // from W12S27 does exactly this
                // (docs/research/remote-mining.md §8.4) — and that
                // reservation is the *only* readable deadline it has,
                // because a level-0 core carries no collapse timer.
                //
                // ADR 0043 reads different answers off the NPC's hold and a
                // player's: the NPC's is the clock a core's stand-down runs
                // to under the fallback floor (#136), a player's the clock
                // its own stand-down runs to with no floor at all (#165). So
                // the two must price the same and must stay tellable apart.
                // Pricing first, pairwise against the rival's reservation,
                // one input at a time.
                let priced control =
                    spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", control ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents

                Expect.equal
                    (priced (coreReservedRoom 4000))
                    (priced (reservedRoom false 4000))
                    "the NPC's reservation prices the rock exactly as a rival's does"

                Expect.isEmpty
                    (priced (coreReservedRoom 4000))
                    "and that price is five, not the held ten"

                Expect.isNonEmpty
                    (priced (reservedRoom true 4000))
                    "held by us the same rock is worth ten, so the fleet above is the neutral one"

                // And tellable apart, which is the whole reason the holder
                // is a closed three-state rather than a flag. A ColonyView
                // that answered both with one "not ours" would hand the
                // gate ADR 0043 describes an input on which no correct
                // answer exists: the NPC's hold read as a rival's shuts an
                // outpost for the life of the colony, and a rival's read
                // as the NPC's walks back into a room somebody else holds.
                let holderOf (control: RoomControlInfo) =
                    control.Reservation |> Option.map (fun held -> held.Holder)

                Expect.notEqual
                    (holderOf (coreReservedRoom 4000))
                    (holderOf (reservedRoom false 4000))
                    "the NPC's hold and a rival's are two facts, not one"

                Expect.notEqual
                    (holderOf (coreReservedRoom 4000))
                    (holderOf (reservedRoom true 4000))
                    "and neither of them is ours"
            }

            test "an outpost the colony cannot see this tick prices no source" {
                // ADR 0004, entry by entry: who holds a room we cannot look
                // into is not a fact this tick, so the source is
                // unpriceable and enters no quota. Unpriceable is not
                // half — half is what a room we *can* see and nobody holds
                // is worth, and the pair below is what separates the two.
                //
                // What is blind here is the *control* entry alone, which is
                // the one input this test moves. The fixture's container
                // still stands in the projection, so its Post is still in
                // the Anchor row and the fleet still carries `a-out` — live
                // the two arrive and vanish together, because the shell
                // gates the structure census and the control entry on the
                // same `seen` list.
                let blind = postedOutpostColony 6 []

                Expect.isEmpty
                    (spawnIntents (decide blind Map.empty Set.empty None).Intents)
                    "no entry for W1N2: the rock's output prices at nothing and the fleet still matches"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { blind with
                                RoomControl = Map.add "W1N2" neutralRoom blind.RoomControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "seen and held by nobody, the same rock is worth five and hires"
            }

            test "the colony's own room is priced on its owner, not on a reservation" {
                // The trap #116's prose walks into and ADR 0042's rule does
                // not: taken as "reserved, or half", the spawn room — which
                // is owned and which nothing reserves — would price both its
                // sources at five, halving the income base and the hauler
                // quota together. The engine gives a room with an owner the
                // same 3,000 a cycle it gives a reserved one.
                //
                // Sized to the halved target so the direction is
                // readable, and the hauler row halves with the output it
                // ships: 2 Anchors + 1 hauler, whose amortization is
                // 2 × 400 + 1 × 600 = 1,400, + ceil((10 × 1500 − 1,400) /
                // 4500) = 4 workers = 7. Held it would be 2 + 2 + 7 = 11,
                // which is what the second half reads.
                //
                // The Anchors are 400 and not 600 because a neutral rock
                // lowers the row's own ceiling as well as its output (ADR
                // 0021 as ADR 0042 narrows it): three Work saturate a rock
                // giving five, and this bank would otherwise buy five.
                let halved =
                    { midIncomeColony with
                        Creeps = incomeFleetRows 1 4
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.isEmpty
                    (spawnIntents (decide halved Map.empty Set.empty None).Intents)
                    "the premise: a neutral spawn room's whole target is these seven"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { halved with
                                RoomControl = homeControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned, the same two sources are worth ten each and the fleet is four short"
            }

            test "the hauler quota prices each container at its own source's output" {
                // The quota's other reader (ADR 0042), read here on the
                // colony's own room: it folds every projected room's
                // containers and prices each at *that* container's
                // source, so moving the rate under the home room moves
                // the home containers' half of it and nothing else. The
                // outpost half is `outpostHaulTests`, on a fixture with a
                // Seam to cross. The two containers' demands are summed and
                // rounded once (ADR 0049): ceil((24 + 24) × 10 / 400) is
                // two haulers for the pair and ceil((24 + 24) × 5 / 400)
                // is one.
                Expect.equal (quotaOf midIncomeColony) 2 "the premise: the reserved rate hires two"

                Expect.equal
                    (quotaOf
                        { midIncomeColony with
                            RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                        })
                    1
                    "half the output is half the haul, and the pool is a body lighter"

                Expect.equal
                    (quotaOf
                        { midIncomeColony with
                            RoomControl = Map.empty
                        })
                    0
                    "a container whose source's room prices nothing hires nobody (ADR 0004)"
            }

            test "a quota memoised while the room was held is not handed back when it lapses" {
                // ADR 0017's stated failure mode, at the seam that would
                // ship it: the hauler quota rides the census memo, and
                // since ADR 0042 it reads who holds the room — a per-tick
                // vision fact, not a census one. `Main.fs` keeps the memo
                // in heap and hands `decide` last tick's every tick, so a
                // signature blind to the rate would recall three haulers
                // for a room now worth half, and would size the worker
                // row off that amortization too. Every census input here
                // is byte-identical between the two views: the
                // reservation is the only thing that moved.
                let lapsed =
                    { midIncomeColony with
                        Creeps = incomeFleetRows 1 4
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                let previous = (decide midIncomeColony Map.empty Set.empty None).Memo

                Expect.equal
                    previous.HaulerQuota
                    2
                    "the premise: held, the two home containers hire two"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decide lapsed Map.empty Set.empty None

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the stale memo recomputes to the fresh quota: half the output, half the haul"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet standing at the halved target casts nothing it does not need"
            }
        ]

/// The whole fleet the switch hires: the home rows and the outpost's,
/// twelve bodies standing exactly at `switchPosted`'s target and six over
/// `switchUnposted`'s.
let private switchFleet = switchHomeFleet @ switchOutpostRows

/// The same fleet with the named bodies respelled as generalists — the
/// headcount never moves, so a case reading against it reads a *row gap*
/// and nothing else, the deficit being the same number whichever row the
/// twelve bodies were cast from. The spare bodies are named off a
/// prefix of this helper's own, so growing `switchOutpostRows` can never
/// mint a name twice into one fleet.
let private respelled names fleet =
    fleet
    |> List.mapFold
        (fun n (creep: CreepInfo) ->
            if List.contains creep.Name names then
                worker $"gen{n}" 0 50, n + 1
            else
                creep, n)
        1
    |> fst

/// The fleet with both Anchors respelled: twelve bodies alive and every
/// Post in the colony standing empty.
let private unmannedPosts = respelled [ "a-home"; "a-out" ] switchFleet

/// The fleet with both haulers respelled: twelve bodies alive and no
/// shipping at all.
let private unshippedFleet = respelled [ "h-home1"; "h-out1" ] switchFleet

[<Tests>]
let rowGapTests =
    testList
        "the deficit gates the worker row alone"
        [
            // Read as the switch's own tests are, one body at a time off
            // the one idle spawn `switchHome` stands: a tick casts at most
            // one body, so the list this returns is either empty or names
            // the row whose gap was answered first.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            // The premise every case below rests on, asserted where it is
            // used rather than assumed: at `switchUnposted`'s target of
            // six a fleet of twelve is far over, and one body fewer is
            // still over — so nothing that follows can be the ordinary
            // deficit hiring.
            let overTarget colony fleet =
                Expect.isEmpty
                    (casts colony (List.truncate (List.length fleet - 1) fleet))
                    "the premise: a body short of this fleet the colony is still over target"

            test "the tick a source unposts, the home room's empty Post is cast for anyway" {
                // #154's reproduction, and the reason the gate moved. The
                // colony loses vision of its outpost for one tick: the
                // source there unposts, and its Anchor place, its haul and
                // its income share leave the target together (ADR 0042,
                // ADR 0004), dropping it under the living count. The home
                // room's Post is empty across both ticks and is a fact
                // about the ground either way — gated on the deficit it
                // went unfilled until ordinary deaths had paid off the
                // whole twelve-body overshoot, and the colony cast
                // nothing at all, in its own room included, in the
                // meantime.
                //
                // Pairwise, one rival at a time: the two fleets differ in
                // the two Anchors' bodies and in nothing else.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "with every row manned the same twelve cast nothing"

                overTarget switchUnposted switchFleet

                match casts switchUnposted unmannedPosts with
                | [ (_, body, name) ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "the empty Post is filled from the Anchor row"

                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "and sized to the bank exactly as that row always is"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing container's hauler gap is filled under the target too" {
                // The same rule on the row beside it (ADR 0012): a source
                // container standing wants its round trip shipped whatever
                // the headcount is, and the tick the target fell the
                // container did not stop standing. Both Anchors stay alive
                // here, so the Anchor row has no gap and the hauler row is
                // the only rival the cast can come from.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "with every row manned the same twelve cast nothing"

                overTarget switchUnposted switchFleet

                match casts switchUnposted unshippedFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "hauler-"
                        "the home container's own round trip hires it"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the worker row is the one the deficit is the quota of, and it still stops" {
                // The half of the gate that does not move (ADR 0012): the
                // worker row's quota *is* whatever the target has left over
                // once the specialist rows are counted, so with nothing
                // left over it hires nobody however far the fleet has
                // overshot. Pairwise against the same fleet under a target
                // that reaches it — one room's vision richer, where those
                // twelve are the target — and one body short there is a
                // worker.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "six over target, every row manned, and no generalist"

                overTarget switchUnposted switchFleet

                match casts switchPosted (List.truncate 11 switchFleet) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "posted, the target reaches the fleet and the remainder is the worker row's"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a row standing over its quota still holds the worker row down" {
                // What the deficit is and is not (ADR 0012). It gates the
                // worker row; it is not that row's own gap, and the
                // difference shows the tick a specialist row stands over
                // quota. Under `switchUnposted` the Anchor row wants one
                // and the hauler row one: a fleet of two Anchors, two
                // haulers and two workers is six bodies exactly at the
                // target, two of them surplus specialists, and the worker
                // row is two short of its own quota of four. The surplus
                // holds it there — #154 moves the specialist rows off the
                // deficit and deliberately leaves this half of the gate
                // standing.
                //
                // Pairwise against the same target with the specialist
                // rows at quota, where the whole-fleet gap and the worker
                // row's own gap coincide and one body short is a worker.
                let overSpecialised =
                    switchFleet
                    |> List.filter (fun creep ->
                        not (List.contains creep.Name [ for i in 3..8 -> $"w{i}" ]))

                Expect.hasLength
                    overSpecialised
                    6
                    "the premise: six bodies, standing exactly at the target"

                Expect.isEmpty
                    (casts switchUnposted overSpecialised)
                    "two surplus specialists, and the worker row hires none of its two missing"

                match casts switchUnposted (List.truncate 5 switchHomeFleet) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "with every specialist row at quota the same shortfall is hired"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test
                "the doorstep hold still comes first: an empty Post is no reason to cast into a Reach" {
                // ADR 0033's gate is asked before anything is priced and
                // this ticket does not move it (#154). The row gap that
                // now outlives a negative deficit is exactly the case that
                // could have walked past it — the hold is the outer
                // question, the deficit an inner one.
                let hot =
                    switchUnposted |> facing [ hostileAt "h-1" { X = 25; Y = 13 } [ Attack; Move ] ]

                Expect.isNonEmpty
                    (casts switchUnposted unmannedPosts)
                    "the premise: quiet, the empty Post is cast for"

                Expect.isEmpty
                    (casts hot unmannedPosts)
                    "and under fire the same empty Post casts nothing"
            }
        ]

/// A colony standing exactly at its Workforce target with one reserver in
/// it: no Post, no source container and no placed rock, so the target is
/// the floor of two and the two living creeps meet it. One body leaving
/// the count is therefore one cast, which is what makes a lead readable
/// (ADR 0026). The bank is 1,300 — ADR 0042's own reserver body at
/// capacity — and the reserver stands at (25,29), three plain steps from
/// the tile a replacement is born on.
let private leadColony life =
    let room = atLevel 2 (openRoom 6)

    { room with
        Bank = bank 1300 1300
        Creeps = [ worker "w1" 0 50; reserver "r1" |> withLife life ]
        Spatial =
            room.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ "w1", { X = 25; Y = 27 }; "r1", { X = 25; Y = 29 } ]
                })
    }

[<Tests>]
let reserverLeadTests =
    testList
        "the reserver row's lead"
        [
            test "a CLAIM body's lead is the reserver row's, not the generalist's" {
                // `patternOf` reads a living body back to the row it was
                // cast from (ADR 0006), and the row is what sizes the
                // replacement a lead prices (ADR 0026). A `[Claim; Move]`
                // body has neither Work nor Carry, so before ADR 0042's row
                // existed it fell through to the generalist and was priced
                // as one.
                //
                // The two arithmetics, at this colony's 1,300 bank: the
                // reserver row casts `[2Claim;2Move]`, four parts, 12 ticks
                // in the spawner, and its two Move carry its two fatigue
                // parts over a plain tile in the walk's one-tick floor — 3
                // ticks for the three steps, a lead of 15. The generalist
                // row at the same bank is twenty parts: 60 ticks in the
                // spawner and the same 3 of walking, a lead of 63. Every
                // life between the two is where the rows disagree.
                let casts life =
                    spawnIntents (decide (leadColony life) Map.empty Set.empty None).Intents

                Expect.isEmpty
                    (casts 30)
                    "at 30 ticks the reserver still counts; read off the generalist row it would not"

                Expect.isEmpty (casts 16) "one tick outside its own row's lead it still counts"

                match casts 15 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "at its lead the colony is one short — and casts a generalist, this room declaring no outpost for the reserver row to hire against"

                    Expect.isFalse
                        (List.contains BodyPart.Claim body)
                        "the row that replaces a reserver is the reserver row's quota, and here it is zero"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// ADR 0042's own reserver body, which a deficit of one to 1,200 ticks
/// buys: 1,300 energy, 2.17 a tick over a CLAIM part's 600-tick life.
let private twoBlocks = [ BodyPart.Claim; BodyPart.Claim; Move; Move ]

[<Tests>]
let reserverRowTests =
    testList
        "the reserver row"
        [
            test "a declared outpost hires one reserver, posted or not" {
                // The one quota the container switch does *not* gate
                // (#131's correction comment): gating it deadlocks the
                // chain, because a container site needs vision, vision
                // needs a creep in the room, and this is the only creep
                // with a reason to go. ADR 0042's Considered Options is the
                // authority its Consequences clause contradicts — it
                // rejected "mine first, reserve later" precisely so the
                // reservation is standing before the first hauler is sized.
                //
                // Pairwise on the one structure — same room, same rock,
                // same controller, same reservation — because that is the
                // only input that moves.
                let castsWith posted =
                    let fleet = surplusFleet (if posted then 3 else 2)

                    reserverColony [ northOutpost posted ] fleet [ "W1N2", reservedRoom true 5000 ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> spawnIntents result.Intents

                for posted in [ true; false ] do
                    match castsWith posted with
                    | [ (_, body, creepName) ] ->
                        Expect.stringStarts
                            creepName
                            "reserver-"
                            $"the one cast is the reserver row's (posted: %b{posted})"

                        Expect.equal body oneBlock "and its body holds a CLAIM part"
                    | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a candidate colony hires one body, and it is one block" {
                // ADR 0047's casting clause, and the two things it says at
                // once. The row is *this* row — a claimer is a
                // `[Claim; Move]` body like a reserver, so it is cast, led
                // and amortized where reservers are, and `patternOf` reads
                // one back as the other. And the room hires **one** body,
                // not two: its controller carries a Claim and no Reserve,
                // so a reserver hired for it would arrive at a controller
                // with no Task on it and stand there for its whole
                // 600-tick life.
                //
                // The block count is where the two *demands* are told
                // apart. A reservation sitting 4,000 ticks below its cap
                // asks for seven blocks and takes what the 1,800 bank
                // affords, which is two; a claim is one act by one CLAIM
                // part and asks for one block whatever the reservation has
                // done. Pairwise on the declaration alone — same room, same
                // rock, same reservation, same fleet, same bank.
                //
                // The demand is not the cast: this room's own demand is the
                // whole list here, so the two coincide. The test below is
                // where they come apart.
                let castsWith declared =
                    let colony =
                        reserverColony
                            [ northOutpost false ]
                            (surplusFleet 2)
                            [ "W1N2", reservedRoom true 1000 ]

                    { colony with
                        Declared =
                            if declared then
                                [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
                            else
                                []
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsWith false)
                    [ twoBlocks ]
                    "undeclared, the room is an outpost and its lapsed reservation buys the bank's body"

                Expect.equal
                    (castsWith true)
                    [ oneBlock ]
                    "declared a colony, the same room hires one body of one block, and no reserver beside it"
            }

            test "a claimer beside a slipping reservation is cast at the row's largest demand" {
                // The other half of ADR 0047's casting clause, and the half
                // a one-outpost fixture cannot show: the claim's *entry* is
                // one block, but every body this row casts this tick is
                // sized at the largest demand in the list. Which controller
                // a finished CLAIM body ends up holding is the Matcher's,
                // priced by travel cost alone and knowing nothing about
                // which demand paid for which body, so a claimer cast at
                // one block could land on the reservation that has slipped
                // and freeze that room for its whole 600-tick life. The
                // over-buy is the safe direction, and it is the same one
                // `reserverClaimsOf` takes for two slipping reservations.
                //
                // Pairwise on the second outpost alone: the candidate is
                // the same room under the same declaration with the same
                // reservation in both, and what moves is whether an unheld
                // outpost stands beside it.
                let castsWith slippingNeighbour =
                    let outposts =
                        if slippingNeighbour then
                            [ northOutpost false; westOutpost false ]
                        else
                            [ northOutpost false ]

                    let control =
                        [ "W1N2", reservedRoom true 5000 ]
                        @ if slippingNeighbour then [ "W2N2", neutralRoom ] else []

                    let colony = reserverColony outposts (surplusFleet 2) control

                    { colony with
                        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsWith false)
                    [ oneBlock ]
                    "the candidate alone: its one block is the whole list, so the claimer is one block"

                Expect.equal
                    (castsWith true)
                    [ twoBlocks; twoBlocks ]
                    "beside an unheld outpost the whole row is cast at that room's deficit, the claimer with it"

                // And those two bodies are one reserver and one claimer
                // rather than two reservers: the candidate's controller
                // carries the Claim and no Reserve, so one of the two
                // over-bought bodies is the one that will touch a
                // controller once.
                let declared =
                    let colony =
                        reserverColony
                            [ northOutpost false; westOutpost false ]
                            (surplusFleet 2)
                            [ "W1N2", reservedRoom true 5000; "W2N2", neutralRoom ]

                    { colony with
                        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
                    }

                Expect.equal
                    (planTasks declared noThreats
                     |> List.filter (function
                         | Reserve _
                         | Claim _ -> true
                         | _ -> false))
                    [ Reserve "ctrl-W2N2"; Claim "ctrl-W1N2" ]
                    "the row's two demands are the unheld outpost's Reserve and the candidate's Claim"
            }

            test "two declared outposts hire two reservers, one apiece" {
                // Never one rover: `[4Claim;4Move]` is 2,600 energy and so
                // an RCL7 body, and two outposts diagonal to each other
                // share no exit — a rover would spend its 600-tick life
                // crossing the home room (ADR 0042).
                let colony =
                    reserverColony
                        [ northOutpost true; westOutpost true ]
                        (surplusFleet 4)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decide colony Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "one body per declared outpost, and the four idle spawns cast no third"

                let half =
                    reserverColony
                        [ northOutpost true; westOutpost false ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decide half Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and the one still waiting for its container is hired for just the same"
            }

            test "a bank that cannot buy one block hires no reserver and still casts" {
                // The row's floor body is 650 — larger than every other
                // row's, and larger than the whole bank below RCL3. Being
                // first in the cascade, a gap it can never fill would stop
                // every row under it forever: `planned` counts intents, so
                // an uncast head row leaves every idle spawn asking for the
                // head row again, in the home room included. So a bank that
                // cannot afford the floor hires the row not at all.
                let intentsAt capacity =
                    let colony =
                        reserverColony
                            [ northOutpost false ]
                            [ worker "w1" 0 50 ]
                            [ "W1N2", neutralRoom ]

                    { colony with
                        Bank = bank capacity capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> result.Intents

                Expect.isEmpty
                    (reserverCasts (intentsAt 550))
                    "at an RCL2 bank of 550 the row has no quota at all"

                match spawnIntents (intentsAt 550) with
                | (_, _, creepName) :: _ ->
                    Expect.stringStarts
                        creepName
                        "anchor-"
                        "and the row under it casts rather than the colony freezing"
                | [] -> failtest "expected the rows under the reserver to cast"

                Expect.equal
                    (reserverCasts (intentsAt 650))
                    [ oneBlock ]
                    "one block's worth of capacity is where the row starts hiring"
            }

            test "a living reserver fills the quota; one inside its lead does not" {
                // The quota counts bodies and not rooms (#130): which
                // controller each body ends up holding is the Reserve
                // Task's one-holder-per-controller capacity, so a reserver
                // still walking to its outpost already fills the row's
                // place. Its succession is ADR 0026's existing path and
                // nothing new: inside its lead it leaves the count, and the
                // replacement is cast while it still holds the reservation.
                let colonyWith reservers =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3 @ reservers)
                        [ "W1N2", reservedRoom true 5000 ]

                let placed life =
                    let colony = colonyWith [ reserver "r1" |> withLife life ]

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "r1", { X = 22; Y = 10 } ]
                                })
                    }

                Expect.isEmpty
                    (reserverCasts (decide (placed 1500) Map.empty Set.empty None).Intents)
                    "a reserver with a life ahead of it is the row's one body"

                Expect.equal
                    (reserverCasts (decide (placed 5) Map.empty Set.empty None).Intents)
                    [ oneBlock ]
                    "inside its lead it is already outside the count, so the successor is cast"
            }

            test "the reserver row casts in front of the Anchor, hauler and worker rows" {
                // ADR 0042's ordering: the other three rows spend income
                // and this one decides whether the income is five a tick or
                // ten across every source of an outpost at once — and it is
                // the cheapest body on the table, so the row it displaces
                // for a tick waits on 650 energy.
                //
                // The fleet is short in every row at once: two Anchors
                // against three Posts, no hauler against the one-body
                // quota the two home containers come to at this bank (ADR
                // 0049), and a whole-fleet deficit under all of it. Four
                // idle spawns cast one body each, so the whole order is
                // readable in one tick.
                // The generalist in the fleet is the supply floor's
                // premise and not this case's (ADR 0050): two Anchors are
                // two Carry parts and still nothing that can refill an
                // extension, so without it the row cast first would be the
                // floor's carrier rather than the reservation's.
                let colony =
                    reserverColony
                        [ northOutpost true ]
                        [ anchor "a1" 0 50; anchor "a2" 0 50; worker "w1" 0 50 ]
                        [ "W1N2", reservedRoom true 5000 ]

                match spawnIntents (decide colony Map.empty Set.empty None).Intents with
                | [ (_, firstBody, firstName)
                    (_, _, secondName)
                    (_, _, thirdName)
                    (_, _, fourthName) ] ->
                    Expect.stringStarts firstName "reserver-" "the reservation is cast for first"
                    Expect.equal firstBody oneBlock "at the deficit's own body, not the bank's"
                    Expect.stringStarts secondName "anchor-" "then the empty Post"
                    Expect.stringStarts thirdName "hauler-" "then the throughput quota"
                    Expect.stringStarts fourthName "worker-" "and the generalist last"
                | other -> failtest $"expected exactly four SpawnCreep intents, got %A{other}"
            }

            test "the body grows by a CLAIM part for every 600 ticks the reservation has lost" {
                // ADR 0042's one rule, quota and sizing in the same
                // expression: `ceil((5000 − ticks held) / 600)` CLAIM
                // parts. No state between ticks — the deficit is read off
                // the reservation itself, so the row shrinks to its floor
                // in steady state and comes back bigger on its own the
                // tick a reservation has slipped.
                let castFor held =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true held ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal (castFor 5000) [ oneBlock ] "at the cap the deficit is zero: the floor"

                Expect.equal
                    (castFor 4400)
                    [ oneBlock ]
                    "600 ticks lost is one part: one CLAIM holds a reservation up through a whole CLAIM life"

                Expect.equal (castFor 4399) [ twoBlocks ] "the 601st lost tick is the second part"
                Expect.equal (castFor 3800) [ twoBlocks ] "and 1,200 lost is still the second"
            }

            test "the bank truncates the deficit, and the deficit truncates the bank" {
                // The two halves of the sizing rule, each shown cutting the
                // other off. ADR 0042 refuses the bank *as the rule*: at
                // RCL6 a 2,300 bank would buy a third CLAIM for a
                // reservation that caps at 5,000 anyway — which is why the
                // pair below is read at 2,300 and not at today's 1,800,
                // where the two rules agree.
                let castAt capacity held =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true held ]

                    { colony with
                        Bank = bank 8000 capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castAt 2300 5000)
                    [ oneBlock ]
                    "a full reservation asks for one block, and the RCL6 bank's three do not overrule it"

                Expect.equal
                    (castAt 2300 0)
                    [ List.replicate 3 BodyPart.Claim @ List.replicate 3 Move ]
                    "a reservation on the floor asks for nine parts and gets the three the bank buys"

                Expect.equal
                    (castAt 8000 0)
                    [ List.replicate 9 BodyPart.Claim @ List.replicate 9 Move ]
                    "at a bank that affords them, the deficit's own nine"
            }

            test "a reservation another player holds leaves this colony holding nothing" {
                // Pairwise, one holder at a time: the same room, the same
                // ticks on the same controller, and only whose it is moves.
                // The colony's hold starts at zero under a rival's
                // reservation, exactly as that room's sources stay at the
                // neutral rate — a hold somebody else owns is not one this
                // row can measure its deficit from.
                //
                // Read at a bank that affords the whole deficit and not at
                // the live 1,800, where two parts and nine both truncate to
                // two and the pair could not tell the `Ours` filter from
                // its absence.
                let castWith control =
                    let colony = reserverColony [ northOutpost true ] (surplusFleet 3) control

                    { colony with Bank = bank 8000 8000 }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                let nineBlocks = List.replicate 9 BodyPart.Claim @ List.replicate 9 Move

                Expect.equal
                    (castWith [ "W1N2", reservedRoom true 4000 ])
                    [ twoBlocks ]
                    "1,000 ticks lost of ours is two parts"

                Expect.equal
                    (castWith [ "W1N2", reservedRoom false 4000 ])
                    [ nineBlocks ]
                    "the same 4,000 in a rival's name is a deficit of the whole 5,000: nine parts"

                Expect.equal
                    (castWith [])
                    [ nineBlocks ]
                    "and no reservation at all is that same whole deficit"
            }

            test "every cast this tick carries the largest outstanding demand" {
                // The row casts bodies and the Matcher pairs them to
                // controllers, by travel cost alone and knowing nothing
                // about a deficit (#130). So a demand read room by room
                // would land the *nearer* room's small body on the room
                // that has slipped, and a controller held by one CLAIM
                // against the engine's one tick of decay is frozen where it
                // stands for that body's whole 600-tick life. The row
                // over-buys instead — at most one block per cast, and only
                // while two demands differ.
                let colony =
                    reserverColony
                        [ northOutpost false; westOutpost false ]
                        (surplusFleet 2)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 2000 ]

                let fiveBlocks = List.replicate 5 BodyPart.Claim @ List.replicate 5 Move

                Expect.equal
                    (reserverCasts
                        (decide { colony with Bank = bank 8000 8000 } Map.empty Set.empty None)
                            .Intents)
                    [ fiveBlocks; fiveBlocks ]
                    "the room standing at its cap is cast the five blocks the slipped room asked for"
            }

            test "the reserver row is an addend of the target, amortized over a CLAIM life" {
                // The row's two effects on the Workforce target (ADR 0042),
                // both read off one boundary: it adds a place of its own —
                // a CLAIM body is a creep, and a fleet counting it as a
                // generalist would hire an upgrade mouth fewer — and its
                // replacement cost is deducted from the income base like
                // the Anchor and hauler rows'. Unlike theirs it is spread
                // over a **CLAIM body's own 600 ticks** rather than the
                // 1,500 the rest of the sum is written in: ADR 0042 prices
                // this row at 2.17 energy a tick, and over 1,500 it would
                // read as 0.87.
                //
                // The bank is 8,000 so the deficit's whole nine blocks are
                // affordable and the difference is a worker place wide.
                // W1N2 is seen and held by nobody: its rock is worth five,
                // and its reservation is on the floor, so the row asks for
                // its largest body against its smallest income.
                //
                // Income 10 + 10 + 5 = 25 a tick over 1,500 = 37,500.
                // Amortization: 3 Anchors × 700 = 2,100, one hauler ×
                // 2,400 — the two home containers' demands summed and
                // rounded once (ADR 0049) — and one 9-block reserver at
                // 5,850 spread over 600 and re-scaled onto 1,500 = 14,625
                // — 19,125 in all. The surplus 18,375 over a 16-Work
                // body's drain × 1,500 = 24,000 rounds up to one worker
                // (ADR 0037). Charged over 1,500 instead, the same row
                // would leave 27,150 and hire two.
                let fleetOf workers =
                    [ for i in 1..3 -> anchor $"a{i}" 0 50 ]
                    @ [ hauler "h1" 0 100 ]
                    @ [ reserver "r1" ]
                    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

                let atFleet workers =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (fleetOf workers)
                            [ "W1N2", neutralRoom ]

                    decide { colony with Bank = bank 40000 8000 } Map.empty Set.empty None

                Expect.equal
                    (atFleet 1).Memo.HaulerQuota
                    1
                    "the premise: the two home containers come to one body at this bank"

                Expect.isEmpty
                    (spawnIntents (atFleet 1).Intents)
                    "3 Anchors + 1 hauler + 1 reserver + 1 worker is the whole target: six"

                match spawnIntents (atFleet 0).Intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "one body short, the generalist row fills the remainder"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a declared outpost this colony owns hires no reserver at all" {
                // The tick a declared outpost is claimed, its controller
                // stops being reservable: the engine answers
                // ERR_INVALID_TARGET to `reserveController` on a room
                // anybody owns (#181). An owned room carries no reservation
                // for the same reason, so the deficit read off one is the
                // whole 5,000 and this row would cast a 650-energy body at
                // it every 600 ticks forever, each one walking over to fail
                // for its whole life.
                //
                // Pairwise on the one fact — same room, same rock, same
                // absent reservation, same fleet, same bank — because
                // ownership is the only input that moves. Read at a bank
                // that affords the whole nine-block deficit, so the
                // neutral half is a body the truncation could not have
                // produced by accident.
                let castsUnder control =
                    let colony =
                        reserverColony [ northOutpost false ] (surplusFleet 2) [ "W1N2", control ]

                    { colony with Bank = bank 8000 8000 }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsUnder neutralRoom)
                    [ List.replicate 9 BodyPart.Claim @ List.replicate 9 Move ]
                    "a room nobody holds is the whole 5,000 of deficit: nine parts"

                Expect.isEmpty
                    (castsUnder ownedRoom)
                    "the same room, owned by this colony, is no longer a room to reserve"
            }
        ]

/// Files our own bodies into one named room's layer of the projection (ADR
/// 0041): a creep the projection places nowhere stands in no room at all, and
/// the row's `Living` and the cases that stand a guard beside its raid both
/// want it standing somewhere real — a guard that stands in the raided room is
/// what the row's `Living`, the Task's holders and the Matcher all read, even
/// though since #272 the count itself reads no body of ours at all.
let private standingIn room (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                room
                { layer with
                    CreepPositions =
                        ours |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// The guard row's colony (ADR 0056): `reserverColony`'s W12S28 shape with its
/// north outpost declared, posted and held at the reservation cap — so the
/// reserver row wants exactly one block and the Anchor row is at quota — plus
/// the hostiles the case names standing in that outpost and our own bodies
/// standing there beside them.
///
/// The hostiles are a parameter and not a field of the fixture, so a case reads
/// the quiet tick and the raided one **pairwise** off one geometry: what moves
/// between two calls is the raid and can be nothing else.
let private guardColony hostiles (ours: (CreepInfo * Pos) list) =
    let colony =
        reserverColony
            [ northOutpost true ]
            (surplusFleet 3 @ List.map fst ours)
            [ "W1N2", reservedRoom true 5000 ]

    { colony with Hostiles = hostiles } |> standingIn "W1N2" ours

/// One rock of the north outpost's three-Seat field, which is the whole of the
/// walkable ground `northOutpost` lays: the guard stands on one Seat and the
/// raid on the tile below the rock. The second Seat is for the cases that stand
/// two guards up, the engine putting no two bodies on one tile.
let private outpostSeat = { X = 41; Y = 40 }
let private secondSeat = { X = 40; Y = 39 }
let private raidTile = { X = 40; Y = 41 }

/// The same tile of the *west* outpost's field, for the one case that asks
/// which room a body of the raid is standing in.
let private westSeat = { X = 21; Y = 40 }

/// A raid of one `smallMelee` and the healers the case names, all in the north
/// outpost: the [[threat]] that makes the room guarded at all (ADR 0033's own
/// test, which a healer fails), and beside it the HEAL parts the count rule
/// prices. Each healer carries an id of its own, a raid being a roster and not
/// one creep.
let private raidOf healers =
    hostileIn "W1N2" raidTile smallMelee
    :: [
        for i in 1..healers ->
            { hostileIn "W1N2" raidTile smallHealer with
                Id = $"heal-{i}"
            }
    ]

/// The same colony at a named spawn capacity, for the one case that asks what
/// the bank does to the count: 800 and 1,300 buy one guard block, 1,800 — the
/// live RCL5 capacity `guardColony` itself banks — two, and 2,300 three (ADR
/// 0056 decision 1's own table). The capacity moves and the 8,000 banked does
/// not, keeping `reserverColony`'s own property: restraint in these cases comes
/// from the rows, never from the bank running dry between two casts of one
/// tick.
let private banked capacity (colony: ColonyView) =
    { colony with
        Bank = bank 8000 capacity
    }

/// The `guard` row of the tick's `Quotas`, which is where the cascade writes its
/// own arithmetic down (ADR 0009) — the quota being observability and never a
/// number anything downstream reads.
let private rowOf name colony =
    (decide colony Map.empty Set.empty None).Quotas.Rows
    |> List.tryFind (fun row -> row.Row = name)

let private guardQuotaOf colony =
    rowOf "guard" colony |> Option.map (fun row -> row.Quota)

/// This tick's guard casts, by the row name every creep name carries.
let private guardCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "guard-")
    |> List.map (fun (_, body, _) -> body)

[<Tests>]
let guardRowTests =
    testList
        "the guard row"
        [
            test "a clear outpost hires none, and one armed hostile in it hires one" {
                // ADR 0056's first two banks of the count rule, pairwise on
                // one fixture: the row is **0** for the whole of a colony's
                // ordinary life, and 1 the tick a [[threat]] is seen standing
                // in a declared outpost. Nothing is pre-cast and nothing is
                // remembered — the quota is a per-tick fact read off vision.
                Expect.equal
                    (guardQuotaOf (guardColony [] []))
                    (Some 0)
                    "the premise: a quiet outpost is no reason to buy a body"

                Expect.equal
                    (guardQuotaOf (guardColony [ hostileIn "W1N2" raidTile smallMelee ] []))
                    (Some 1)
                    "and the lone smallMelee nine raids in ten arrive as hires exactly one"
            }

            test "a hostile that reaches nothing is no reason to hire" {
                // The gate is ADR 0033's [[threat]] and never "a hostile": a
                // `smallHealer` carries neither ATTACK nor RANGED_ATTACK, so
                // it takes no ground, kills nothing and buys no body — even
                // though its HEAL parts are exactly what the count rule
                // prices once something armed *is* standing beside it.
                Expect.equal
                    (guardQuotaOf (guardColony [ hostileIn "W1N2" raidTile smallHealer ] []))
                    (Some 0)
                    "a lone healer is a hostile the raid log records and no threat at all"
            }

            test "a raid at home hires no guard" {
                // This row is the [[outpost]]'s and nothing else (ADR 0056):
                // a raid in the home room is the [[keep]]'s business (ADR
                // 0034), and the home room is not a declared outpost. Read at
                // (8,9), far enough from the spawn at (20,10) that the spawn
                // hold is not what is answering — a held tick derives no
                // quotas at all and this case would pass on the wrong reason.
                let athome =
                    { guardColony [] [] with
                        Hostiles = [ hostileIn "W1N1" { X = 8; Y = 9 } smallMelee ]
                    }

                Expect.equal
                    (guardQuotaOf athome)
                    (Some 0)
                    "the same raid that hires one in the outpost hires none at home"

                Expect.isSome
                    (rowOf "guard" athome)
                    "and the cascade ran: the row is written down, it is simply zero"
            }

            test "a raid that out-heals one guard block hires the second" {
                // ADR 0056's count rule at the two readings the arithmetic
                // turns on, one healer apart: `12 × HEAL` over that room's
                // hostiles against `30 × ATTACK + 10 × RANGED_ATTACK` of **one
                // `guardPattern` block** (#272) — the 750-energy, 90-damage
                // body decision 1's worked example is written in. So an
                // unboosted `smallHealer`'s 60 leaves the count at one and a
                // second healer's 120 buys the second body. No guard of ours
                // stands in either reading: since #272 the number is the
                // raid's and reads nothing we have already sent.
                let raid healers = guardColony (raidOf healers) []

                Expect.equal
                    (guardQuotaOf (raid 1))
                    (Some 1)
                    "60 healed against the 90 one block deals: the body we would send out-damages the raid"

                Expect.equal
                    (guardQuotaOf (raid 2))
                    (Some 2)
                    "120 healed against the same 90: the raid out-heals it and the row hires a second"
            }

            test "the count reads the raid and never our own answer to it" {
                // #272, and the amendment's whole point. Priced against the
                // guards *standing* in the room, the number was not monotone:
                // 2 with one guard up and 1 the tick the second arrived, so
                // the reinforcement the escalation had just bought was
                // `CapacityFull`-evicted on arrival — onto a Flee whose safe
                // set is that same room, so it never left, never swung, and
                // held the count at 1 for as long as it lived. 750 energy for
                // a body that does nothing, on exactly the two-healer raid the
                // ADR buys it for. So the damage term is one block of the
                // row's own body and nothing that stands, and the same raid
                // answers the same number with none, one and two guards of
                // ours in the room. Read at the colony's own live 1,800 bank,
                // where a survivor cast at a poorer bank is exactly the body
                // that must not veto its own reinforcement.
                let standing healers ours = guardColony (raidOf healers) ours

                let standing2 = standing 2

                Expect.equal
                    (guardQuotaOf (standing2 []))
                    (Some 2)
                    "the tick the raid is seen, before anything of ours has arrived"

                Expect.equal
                    (guardQuotaOf (standing2 [ guard "g-1", outpostSeat ]))
                    (Some 2)
                    "the tick the first guard stands, which used to be the only tick this read 2"

                Expect.equal
                    (guardQuotaOf (standing2 [ guard "g-1", outpostSeat; guard "g-2", secondSeat ]))
                    (Some 2)
                    "and the tick the second stands beside it, which used to retract to 1"

                Expect.equal
                    (guardQuotaOf (standing 1 [ guard "g-1", outpostSeat ]))
                    (guardQuotaOf (standing 1 []))
                    "and the below-threshold reading is invariant the same way: one healer is one guard, before and after ours arrives"

                Expect.equal
                    (guardQuotaOf (standing 1 [ guard "g-1", outpostSeat ]))
                    (Some 1)
                    "1, and not a second body bought against 60 of healing"

                Expect.equal
                    (guardCasts (decide (standing2 []) Map.empty Set.empty None).Intents
                     |> List.length)
                    2
                    "so the escalation is bought on the tick the raid is seen, an oven earlier than a rule reading our own bodies could"
            }

            test "the count is capped at two per outpost" {
                // The bound decision 4 rests on: two guards die, 1,500
                // energy is spent, and the outpost falls back to ADR 0043's
                // [[stand-down]] rather than feeding an unbounded stream of
                // bodies into a raid we are losing. Four healers is 240
                // against one block's 90 and still asks for two.
                let raid healers = guardColony (raidOf healers) []

                Expect.equal
                    (guardQuotaOf (raid 2))
                    (guardQuotaOf (raid 4))
                    "twice the healing asks for the same two bodies"

                Expect.equal (guardQuotaOf (raid 4)) (Some 2) "and two is the cap"
            }

            test
                "the damage the raid is priced against is one block, so the bank does not move the count" {
                // The other half of "the count reads the raid" (#272): the
                // damage term is **one `guardPattern` block**, a constant of
                // the row, and not the whole body this bank would cast. The
                // whole body grows with the bank while the row's `Living`
                // counts the body that is *standing*, so a guard cast at a
                // poorer bank would veto its own reinforcement — 120 of
                // healing against a two-block 180 reads 1 while a 90-damage
                // survivor holds the row's `Living` at 1 and nothing is cast.
                // It would also take ADR 0056 decision 1's own two-healer case
                // (120 ≥ 90 → 2) out of reach at every bank above 1,300, this
                // colony's live 1,800 included. So the same raid answers the
                // same number across the decision's whole bank table.
                let raid capacity =
                    guardColony (raidOf 2) [ guard "g-1", outpostSeat ] |> banked capacity

                for capacity in [ 800; 1300; 1800; 2300 ] do
                    Expect.equal
                        (guardQuotaOf (raid capacity))
                        (Some 2)
                        $"120 healed against one block's 90 hires the second at a {capacity} bank too"
            }

            test "the count is summed over the declared outposts" {
                // Two rooms, so "one guard per raided outpost" can be told
                // apart from "one guard". The second outpost is unposted, and
                // that is deliberate: the row is hired off a *declaration*
                // and a [[threat]], never off a standing container — an
                // outpost whose crew is being killed before it can build one
                // is the case that most needs the body.
                let twoOutposts hostiles =
                    let colony =
                        reserverColony
                            [ northOutpost true; westOutpost false ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                    { colony with Hostiles = hostiles }

                Expect.equal
                    (guardQuotaOf (twoOutposts [ hostileIn "W1N2" raidTile smallMelee ]))
                    (Some 1)
                    "the premise: one raided outpost of the two hires one"

                Expect.equal
                    (guardQuotaOf (
                        twoOutposts
                            [
                                hostileIn "W1N2" raidTile smallMelee
                                { hostileIn "W2N2" { X = 20; Y = 41 } smallMelee with
                                    Id = "h-2"
                                }
                            ]
                    ))
                    (Some 2)
                    "and a raid in each hires one apiece"
            }

            test "the healing the second guard is priced against is the raided room's own" {
                // The conjunct that keeps the count room-local, in the
                // codebase whose first hazard is room aliasing (ADR 0041): the
                // healing term filters the raid by the room the [[threat]]
                // stands in, and without it two healers forty tiles away in
                // another outpost would price a fight they are not in.
                // Pairwise, one room apart — the same melee, the same two
                // healers, and only the healers' room moving. The other
                // outpost holds no armed hostile of its own, so it is no
                // guarded outpost and adds nothing to the sum from either
                // side.
                let twoOutposts healerRoom healerTile =
                    let raid =
                        hostileIn "W1N2" raidTile smallMelee
                        :: [
                            for i in 1..2 ->
                                { hostileIn healerRoom healerTile smallHealer with
                                    Id = $"heal-{i}"
                                }
                        ]

                    let colony =
                        reserverColony
                            [ northOutpost true; westOutpost false ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                    { colony with Hostiles = raid }

                Expect.equal
                    (guardQuotaOf (twoOutposts "W1N2" raidTile))
                    (Some 2)
                    "the premise: 120 healed in the raided room against the 90 one block deals hires the second"

                Expect.equal
                    (guardQuotaOf (twoOutposts "W2N2" westSeat))
                    (Some 1)
                    "the same two healers standing in the other outpost price nothing here: this room's raid heals nothing"
            }

            test "the row is cast in front of the reserver and reads its own body back" {
                // The cascade slot (ADR 0056): behind the [[supply floor]]
                // and in front of the [[reserver]]. Both gaps are open on
                // this tick — the reservation is at its cap, so the reserver
                // row wants one block — and the colony's four idle spawns
                // draw the seats in order, so the *first* cast says which row
                // was asked first. Pairwise against the quiet tick, where the
                // reserver is the head of the cascade exactly as ADR 0042
                // left it.
                let castNames colony =
                    spawnIntents (decide colony Map.empty Set.empty None).Intents
                    |> List.map (fun (_, _, name: string) -> name.Split('-').[0])

                Expect.equal
                    (castNames (guardColony [] []) |> List.truncate 1)
                    [ "reserver" ]
                    "the premise: with nothing to fight, the reserver is the head of the cascade"

                Expect.equal
                    (castNames (guardColony [ hostileIn "W1N2" raidTile smallMelee ] [])
                     |> List.truncate 2)
                    [ "guard"; "reserver" ]
                    "and a raid puts the guard in front of it, without displacing it"
            }

            test "the guard the row casts is the block the bank buys" {
                // The cast itself and not the quota: at the live RCL5 bank
                // the row buys two whole blocks, which is the body every
                // damage number above is written in.
                Expect.equal
                    (guardCasts
                        (decide
                            (guardColony [ hostileIn "W1N2" raidTile smallMelee ] [])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ bodyFor guardPattern 1800 ]
                    "one cast, at the 1,800 bank `reserverColony` holds"
            }

            test "a bank that cannot afford a block yields the tick" {
                // ADR 0050 through the new row: 750 is more than a 300 bank
                // holds, so the row casts nothing and does not hold the
                // cascade for the rows behind it — a colony this small has
                // ADR 0043's stand-down and nothing else. Pairwise against
                // 800, the first bank that can pay for the row at all, with
                // the same raid standing in the same room.
                let raided = guardColony [ hostileIn "W1N2" raidTile smallMelee ] []

                let castsAt available capacity =
                    { raided with
                        Bank = bank available capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> guardCasts result.Intents

                Expect.equal
                    (castsAt 800 800)
                    [ bodyFor guardPattern 800 ]
                    "the premise: at 800 the row buys its one block"

                Expect.isEmpty (castsAt 300 300) "at 300 it buys nothing and yields the tick"

                Expect.equal
                    (guardQuotaOf { raided with Bank = bank 300 300 })
                    (Some 1)
                    "and the quota is unmoved: what the poor bank refuses is the cast, not the row"
            }

            test "a guard fills the guard row's Living and no other row's" {
                // The row is read back off the parts like every other (ADR
                // 0006), and an ATTACK part is the one cut no other row of
                // this colony makes. Without the arm a `[T; A×3; M×5; H]`
                // has neither Work nor Carry and falls through to the
                // **generalist**, so a raid would quietly retire a worker for
                // the guard's whole 1,500-tick life. Pairwise, one body
                // apart.
                let livingOf colony =
                    (decide colony Map.empty Set.empty None).Quotas.Rows
                    |> List.map (fun row -> row.Row, row.Living)

                let quiet = livingOf (guardColony [] [])
                let standing = livingOf (guardColony [] [ guard "g-1", outpostSeat ])

                Expect.equal
                    (quiet |> List.map fst)
                    (standing |> List.map fst)
                    "the premise: the same rows either side"

                Expect.equal
                    (List.zip quiet standing
                     |> List.filter (fun ((_, before), (_, after)) -> before <> after)
                     |> List.map (fun ((row, before), (_, after)) -> row, before, after))
                    [ "guard", 0, 1 ]
                    "one body arrives and exactly one row's Living moves — the guard's"
            }

            test "an idle survivor keeps the row filled, so the next raid casts nothing" {
                // ADR 0056's "no decay", read at the seam it is about: a
                // guard that outlived its raid is pooled no work, stands
                // idle, and goes on counting in the row's `Living` — so a
                // second raid inside its 1,500 ticks buys nothing and waits
                // no thirty ticks of oven for a body the colony already
                // owns.
                let raid = [ hostileIn "W1N2" raidTile smallMelee ]

                Expect.equal
                    (guardCasts (decide (guardColony raid []) Map.empty Set.empty None).Intents
                     |> List.length)
                    1
                    "the premise: with nothing standing, the raid casts one"

                Expect.isEmpty
                    (guardCasts
                        (decide
                            (guardColony raid [ guard "g-1", outpostSeat ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "with the survivor standing, the same raid casts none"
            }

            test "a guard classifies Fighter, and no other row's body does" {
                // The [[body class]] ladder's new head (ADR 0056), read the
                // only way it is readable today: `Fighter` answers no
                // differently from `Carrier` in every [[capacity]] scope
                // written so far — `(=) Heavy`, `(<>) Heavy`, `(=) Standing`
                // and "neither Heavy nor Standing" — so the Guard Task's
                // `Fighter -> quota` is the first cap that will tell them
                // apart, and until it lands no fixture at the `decide` seam
                // can. Pinned here rather than left to that ticket, because
                // what it is guarding against is the guard falling back into
                // `Carrier` beside the [[hauler unit]]s, which is silent.
                //
                // Both halves of one claim, so both are asserted over one
                // fleet: the guard is a Fighter, and every other row's body
                // at the same bank is the class it was before the arm
                // existed.
                let bodies =
                    [
                        "guard", bodyFor guardPattern 800
                        "anchor", bodyFor anchorPattern 800
                        "upgrader", bodyFor upgraderPattern 800
                        "hauler", bodyFor haulerPattern 800
                        "reserver", bodyFor reserverPattern 800
                        "worker", bodyFor workerPattern 800
                    ]

                let colony =
                    { incomeColony with
                        Creeps = bodies |> List.map (fun (name, body) -> creepWith name 0 50 body)
                    }

                let atlas = Atlas.ofView colony

                Expect.equal
                    (colony.Creeps
                     |> List.map (fun creep -> creep.Name, bodyClassOf colony.Tuning atlas creep))
                    [
                        "guard", Fighter
                        "anchor", Heavy
                        "upgrader", Standing
                        "hauler", Carrier
                        "reserver", Carrier
                        "worker", Light
                    ]
                    "one row's body classifies Fighter and it is the guard's"
            }
        ]

/// The Anchor #203 met, spelled as the colony really held it: `6W/1C/1M`,
/// standing full on a full container. One Carry and one Move, and yet
/// nothing that can put a single energy into an extension — a standing
/// body by ADR 0046's ratio (`1 × 4 < 6`) and a Work-heavy one by ADR
/// 0016's (`6 > 1`), so Refill, Withdraw, Build and Repair are all shut to
/// it and Harvest at its Post is the whole of its working life.
let private liveAnchor name =
    creepWith name 50 0 [ Work; Work; Work; Work; Work; Work; Carry; Move ]

/// #203's colony at the tick the user found it, in the reserver row's own
/// RCL5 shape: both outposts declared, neither posted and neither held, so
/// the row's demand is two bodies at the bank's `[2Claim;2Move]`; the home
/// room's two Posts garrisoned, so the Anchor row is at quota; and a fleet
/// of exactly those two Anchors.
///
/// The bank is the live reading — 361 against a capacity of 1,800 — and it
/// is a **fixed point**, not a slope: nothing alive here can refill an
/// extension, and the engine's spawn regeneration only ticks while the
/// room holds under 300. Every row but the supply floor prices its body at
/// that 1,800 capacity, so the colony stood 1,235 ticks without casting
/// anything at all while 246,818 energy sat in the storage beside it.
let private deadlockColony =
    { reserverColony
          [ northOutpost false; westOutpost false ]
          [ liveAnchor "a1"; liveAnchor "a2" ]
          [] with
        Bank = bank 361 1800
    }

[<Tests>]
let supplyFloorTests =
    testList
        "the supply floor, and a row that cannot be afforded"
        [
            test "#203: 361 in the bank, two Anchors, and the carrier is cast before every row" {
                // ADR 0050's floor, at the reading it was written from.
                // The head of the cascade wants 1,300 and the bank holds
                // 361; every row under it prices at capacity — hauler
                // 1,800, upgrader 1,750, worker 1,800 — and the one row
                // cheap enough to buy with a broken bank is the Anchor's
                // 700, whose gap is zero because the two bodies that made
                // the deadlock are Anchors. So falling through the cascade
                // alone still casts nothing: the floor is the half that
                // moves.
                match spawnIntents (decide deadlockColony Map.empty Set.empty None).Intents with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "hauler-"
                        "the row that can refill an extension is cast before every other"

                    Expect.equal
                        body
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        "sized from what is banked right now — 361 buys two blocks — and never from \
                         the 1,800 capacity, which is the price the deadlock is made of"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an Anchor's lone Carry does not answer the gate" {
                // The counterexample the gate is written against, pairwise
                // against the case above on the reading and not on the
                // fleet: same colony, same bank. `6W/1C/1M` holds a Carry
                // part and a Move part, so a floor gated on "no body with a
                // Carry that can move" is a floor two Anchors hold down for
                // ever — and #203 reproduces itself unchanged with the rule
                // in place. The gate is `Refill`'s own conjunction beside
                // `Withdraw`'s: a Carry, no standing-body ratio, no more
                // Work than Move.
                Expect.isTrue
                    (deadlockColony.Creeps
                     |> List.forall (fun creep -> Map.containsKey Carry creep.Body))
                    "the premise: every body in this fleet carries a Carry part"

                Expect.isNonEmpty
                    (spawnIntents (decide deadlockColony Map.empty Set.empty None).Intents)
                    "and the colony still hires a carrier, because none of them can refill one"
            }

            test "a full bank does not disarm the floor: the carrier is bought first" {
                // Pairwise with the #203 case on the bank alone — the same
                // two Anchors, the same two declared outposts, and 1,800 of
                // 1,800 banked, a bank every row below can pay for. The
                // floor is armed by the *absence* of a body that can put
                // energy into an extension and by nothing else (ADR 0050):
                // firing it only on a short bank was considered and
                // rejected, because it re-opens the cheapest failure the
                // incident showed — the reserver row takes 1,300 first and
                // the carrier is hired out of what is left on the next
                // tick, which is the losing race the live colony ran when
                // the manual hauler filled the bank to 1,900 and two
                // reserver casts took 2,600 back out of it inside 64 ticks.
                let full =
                    { deadlockColony with
                        Bank = bank 1800 1800
                    }

                match spawnIntents (decide full Map.empty Set.empty None).Intents with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "hauler-"
                        "the body the bank depends on is cast before the bodies that depend on the \
                         bank, however full the bank is"

                    Expect.equal
                        body
                        (List.replicate 24 Carry @ List.replicate 12 Move)
                        "sized from what is banked right now, which at a full bank is the whole \
                         1,800 the capacity would have bought"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "one living hauler switches the floor off and the cascade is unchanged" {
                // The floor is a floor and not a new head row: with one
                // body alive that can draw from a store and deliver into
                // an extension, the bank is fillable again and the head of
                // the cascade is the reserver row's, exactly as ADR 0042
                // orders it.
                let withHauler =
                    { deadlockColony with
                        Creeps = hauler "h1" 0 100 :: deadlockColony.Creeps
                        Bank = bank 1800 1800
                    }

                match spawnIntents (decide withHauler Map.empty Set.empty None).Intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "reserver-"
                        "with the bank fillable the declared outposts' row is first again"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the empty colony's disaster fallback is untouched" {
                // ADR 0006's fallback is the floor's ancestor and not its
                // casualty: a colony with no creep at all still casts the
                // minimal worker unit from what is banked, because
                // time-to-first-creep outranks every row including this
                // one. Same colony, same 361, and only the fleet moves.
                match
                    spawnIntents
                        (decide { deadlockColony with Creeps = [] } Map.empty Set.empty None)
                            .Intents
                with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "time-to-first-creep still outranks the row that asked"

                    Expect.equal body [ Work; Carry; Move ] "and it is still the worker unit"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a row the bank cannot pay for yields the tick to the row below it" {
                // ADR 0050's other half, read where the floor is disarmed:
                // the fleet holds four haulers, so nothing here is the
                // supply floor. One outpost declared and unheld is a
                // reserver gap of one at `[2Claim;2Move]` = 1,300; one
                // Anchor against the home room's two Posts is an Anchor gap
                // of one at `6W/1C/1M` = 700; every other row is over
                // quota.
                //
                // Pairwise on the bank alone, and the second reading is why
                // this is not "skip the reserver": at 1,300 the head row is
                // affordable and it is cast, on the very next tick a filled
                // extension would give it.
                let castsAt available =
                    let colony = reserverColony [ northOutpost false ] (surplusFleet 1) []

                    spawnIntents
                        (decide
                            { colony with
                                Bank = bank available 1800
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents

                match castsAt 700 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "anchor-"
                        "the head row is 600 short, so the empty Post below it is filled instead"

                    Expect.equal
                        body
                        [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "and at the Anchor row's own capacity-sized body, unchanged by the fall"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castsAt 1300 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "reserver-"
                        "and the tick the bank affords it, the same head row is first again"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// A colony standing exactly at its Workforce target with one body of the
/// given shape in it: the shape `leadColony` above has, at the live RCL5
/// bank of 1,800 instead of 1,300 — no Post, no source container and no
/// placed rock, so the target is the floor of two and the two living
/// creeps meet it. One body leaving the count is therefore one cast, which
/// is what makes a lead readable (ADR 0026). The body under test stands at
/// (25,29), three plain steps from the tile a replacement is born on.
let private upgraderLeadColony body life =
    let room = atLevel 2 (openRoom 6)

    { room with
        Bank = bank 1800 1800
        Creeps = [ worker "w1" 0 50; creepWith "u1" 0 50 body |> withLife life ]
        Spatial =
            room.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ "w1", { X = 25; Y = 27 }; "u1", { X = 25; Y = 29 } ]
                })
    }

let private leadCasts body life =
    spawnIntents (decide (upgraderLeadColony body life) Map.empty Set.empty None).Intents

[<Tests>]
let upgraderLeadTests =
    testList
        "the upgrader row's lead"
        [
            test "a standing body's lead is the upgrader row's, not the generalist's" {
                // `patternOf` reads a living body back to the row it was
                // cast from (ADR 0006), and the row is what sizes the
                // replacement a lead prices (ADR 0026). A `11W/1C/11M` body
                // has Work at Move and a Carry nine times short of parity,
                // so before ADR 0046's row existed it fell through to the
                // generalist and was priced as one.
                //
                // The two arithmetics at this colony's 1,800 bank: the
                // upgrader row casts twenty-three parts, 69 ticks in the
                // spawner, and its eleven Move carry eleven fatigue parts
                // — an empty Carry rides free — over a plain tile in the
                // walk's one-tick floor, 3 ticks for the three steps, a
                // lead of 72. The generalist row at the same bank is nine
                // whole units, twenty-seven parts: 81 ticks in the spawner
                // and the same 3 of walking, a lead of 84. Every life
                // between the two is where the rows disagree.
                let upgraderShape = bodyFor upgraderPattern 1800

                Expect.isEmpty
                    (leadCasts upgraderShape 80)
                    "at 80 ticks the standing body still counts; read off the generalist row it would not"

                Expect.isEmpty
                    (leadCasts upgraderShape 73)
                    "one tick outside its own row's lead it still counts"

                Expect.hasLength
                    (leadCasts upgraderShape 72)
                    1
                    "at its own row's lead the colony is one short and casts"

                // The generalist at the same tile, one rival at a time: the
                // same colony, the same life, a body of the row this one
                // used to be read as.
                Expect.hasLength
                    (leadCasts (workerBodyFor 1800) 80)
                    1
                    "the generalist at 80 is inside its own longer lead, which is what 80 was chosen to show"
            }

            test "the row that replaces a standing body is not the upgrader row" {
                // The lead is priced off the row and the *replacement* is
                // hired off the quota, and the two are separate readings
                // (ADR 0026 beside ADR 0046). This colony has no controller
                // container, so the upgrader row's quota is zero however
                // many standing bodies stand in it (#187, `upgraderQuota`),
                // and the body the colony is one short of is hired from the
                // generalist row's remainder. A `SpawnCreep` named
                // "upgrader-" here would mean the quota had stopped reading
                // the buffer.
                match leadCasts (bodyFor upgraderPattern 1800) 72 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "with no buffer standing the upgrader row's quota is zero, so the deficit hires the generalist"

                    Expect.equal
                        body
                        (workerBodyFor 1800)
                        "and it is the generalist's own body, sized at the bank"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the Anchor row wins the read over the upgrader row" {
                // `6W/1C/1M` answers to both descriptions — more Work than
                // Move *and* a standing body — and it is the Anchor row
                // that cast it, so that is the row it is read back to and
                // the lead it is priced at. The arms are ordered and the
                // order is the rule (ADR 0021 over ADR 0046).
                //
                // Eight parts is 24 ticks in the spawner, and six fatigue
                // parts on one Move is six ticks a plain step: a lead of
                // 42. Read as an upgrader it would be 72, so a life of 50
                // sits between the two rows and casts under one of them
                // only.
                Expect.isEmpty
                    (leadCasts (bodyFor anchorPattern 1800) 50)
                    "at 50 the Anchor is outside its own row's lead; read as an upgrader it would not be"

                Expect.hasLength
                    (leadCasts (bodyFor anchorPattern 1800) 42)
                    1
                    "and at 42 it is inside it"
            }
        ]

/// The buffer colony at the live RCL5 bank (ADR 0046): the W12S28
/// corridor — a 3-wide plain field y = 9..11 from x = 8 to 32, the two
/// sources embedded in wall at (10,10) and (30,10) with their built
/// containers standing on the Seats (11,10) and (29,10), so two Posts and
/// no Dual Seat — with the controller at (20,11) and the spawn at (20,10)
/// beside it. One spawn and not four, so a tick casts at most one body and
/// the list a case reads names the row whose gap was answered first.
///
/// The bank is 1,800 against 1,800: the row under test is the one whose
/// whole argument is what that bank buys (`11W/1C/11M` against the
/// generalist's `9W/9C/9M`), so a poorer fixture would pin the cascade and
/// not the row.
let private upgraderRoom =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds =
            Map.ofList
                [
                    "spawn-1", Structure BuiltKind.Spawn
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "ctrl-1", Controller
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                Map.ofList
                    [
                        for x in 8..32 do
                            for y in 9..11 ->
                                { X = x; Y = y },
                                (if (x = 10 || x = 30) && y = 10 then Wall else Plain)
                    ]
            TargetPositions =
                Map.ofList
                    [
                        "spawn-1", { X = 20; Y = 10 }
                        "src-a", { X = 10; Y = 10 }
                        "src-b", { X = 30; Y = 10 }
                        "can-a", { X = 11; Y = 10 }
                        "can-b", { X = 29; Y = 10 }
                        "ctrl-1", { X = 20; Y = 11 }
                    ]
            Obstacles = Set.ofList [ { X = 20; Y = 10 }; { X = 20; Y = 11 } ]
        })

/// The same room with the upgrade buffer at (18,11) — two tiles inside the
/// controller's Upgrade Work Area, on no source's Seat and within range 1
/// of neither rock, so it is the controller's container and not a source's
/// (ADR 0012, ADR 0019). Built or pending is the whole of what the
/// pairwise below varies.
let private withBuffer kind =
    upgraderRoom |> withTargets [ "can-buf", { X = 18; Y = 11 }, kind ]

let private upgraderColony room =
    { bareRespawn with
        Bank = bank 1800 1800
        Refillables = []
        Sources = [ source "src-a"; source "src-b" ]
        Controller = Some(controllerAt 5)
        Stages = homeStages room 5
        Spatial = room
    }

/// The two rows the ground hires, and as many of the two surplus rows as
/// the case wants: one Anchor per Post, the one hauler every round trip
/// comes to together at this bank (ADR 0049), then upgraders and
/// generalists. The bank is a parameter and not the literal 1,800 because
/// the standing row's stand-in has to be the body *this* bank's sizing
/// rule casts — a fixture at a poorer bank whose fleet still held
/// `11W/1C/11M` would have `patternOf` read a row the colony could not
/// have cast, and the row's living count is what every reading below is.
let private upgraderFleetAt capacity anchors upgraders workers =
    [ for i in 1..anchors -> anchor $"a{i}" 0 50 ]
    @ [ hauler "h1" 0 100 ]
    @ [
        for i in 1..upgraders -> creepWith $"u{i}" 0 50 (bodyFor upgraderPattern capacity)
    ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

/// The fleet at the live RCL5 bank and this room's two Posts, which is
/// what most of the readings below are read against.
let private upgraderFleet upgraders workers =
    upgraderFleetAt 1800 2 upgraders workers

/// A construction site standing on the corridor's top row, out of the way
/// of the trunk the haulers walk: what puts a Build in the pool, which is
/// the only thing the worker row's floor reads (ADR 0046).
let private withBuildSite (colony: ColonyView) =
    { colony with
        ConstructionSites = [ { Id = "site-1" } ]
        Spatial =
            colony.Spatial
            |> withTargets [ "site-1", { X = 16; Y = 9 }, Site BuiltKind.Extension ]
    }

/// A declared outpost one room north: a controller in a room the colony
/// neither owns nor holds, which is the whole of what the reserver row's
/// quota is derived from (ADR 0042). No source and no container, so it
/// adds a reserver place and nothing else to the target.
let private withDeclaredOutpost (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds = colony.Spatial.TargetKinds |> Map.add "ctrl-out" Controller
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions = Map.ofList [ "ctrl-out", { X = 25; Y = 45 } ]
                }
    }

/// The same colony with a third source embedded in the wall at (14,10)
/// and its built container standing on the Seat at (15,10) — a third Post,
/// and with it a third ten a tick of income. The one knob #195's pairwise
/// turns: two posted sources are a surplus of one and a half standing
/// bodies, three are two and a half.
let private thirdSource (colony: ColonyView) =
    { colony with
        Sources = colony.Sources @ [ source "src-c" ]
        Spatial =
            colony.Spatial
            |> withTargets
                [
                    "src-c", { X = 14; Y = 10 }, Source
                    "can-c", { X = 15; Y = 10 }, Structure BuiltKind.Container
                ]
            |> withHome (fun layer ->
                { layer with
                    Terrain = layer.Terrain |> Map.add { X = 14; Y = 10 } Wall
                })
    }

/// The same colony with one posted source instead of two — `src-b` and its
/// container out of the projection and out of the Sources beside it. The
/// other side of the same pairwise: half the income is a surplus that does
/// not reach one whole standing body.
let private oneSource (colony: ColonyView) =
    { colony with
        Sources = colony.Sources |> List.filter (fun s -> s.Id <> "src-b")
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "src-b" |> Map.remove "can-b"
            }
    }

/// One tick's casts off the one idle spawn: empty, or the one row whose
/// gap came first.
let private buffered upgraders workers =
    spawnIntents
        (decide
            { upgraderColony (withBuffer (Structure BuiltKind.Container)) with
                Creeps = upgraderFleet upgraders workers
            }
            Map.empty
            Set.empty
            None)
            .Intents

let private castName casts =
    match casts with
    | [ (_, _, name: string) ] -> name
    | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

[<Tests>]
let upgraderQuotaTests =
    testList
        "the upgrader row's quota"
        [
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            test "a built buffer hires the standing row out of the surplus" {
                // ADR 0046's whole arithmetic at the live bank, and the
                // numbers are the ADR's own, less the hauler body ADR 0049
                // took off the row. Twenty a tick from two posted sources
                // over a 1,500-tick life is 30,000; the two rows hired off
                // the ground cost 2 × 700 of Anchor and 1 × 1,800 of
                // hauler, so the surplus is 26,800 and one standing body's
                // eleven Work drinks 16,500 of it — a quota of one, the
                // whole bodies the surplus buys and no part of one (#195,
                // pinned below).
                //
                // The body is the row's sizing rule and not the
                // generalist's: eleven Work, one Carry, eleven Move for
                // 1,700 of the 1,800, where `9W/9C/9M` buys nine Work out
                // of the same bank.
                match buffered 0 0 with
                | [ (_, body, name) ] ->
                    Expect.stringStarts name "upgrader-" "the surplus hires the standing row"

                    Expect.equal
                        body
                        (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
                        "11W/1C/11M, the row's own cast at the RCL5 bank"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test
                "the quota is the whole bodies the surplus buys, and the remainder is the generalist row's" {
                // #195, amending ADR 0046: two rows are hired out of one
                // surplus and only one of them may round up. ADR 0037
                // admits an oversell bounded by *one body's* lifetime
                // drain, paid out of stock rather than income, and two
                // rows rounding up against the same number sell that bound
                // twice — so the rounding stays with the smaller body,
                // which is the generalist's.
                //
                // 26,800 of surplus over 16,500 of drink is one and a half
                // standing bodies. One is hired; the remainder — 10,300 of
                // drink less the 1,700 that body costs to replace, so
                // 8,600 — is the generalist row's, and its own rounding up
                // (ADR 0037) turns it against a 13,500 drain into one
                // worker, which is the number ADR 0046's own #195 bullet
                // hands on. Rounded up here
                // instead it was two standing bodies drinking 33,000
                // against 26,800 of surplus — twenty-two a tick against
                // twenty of income, the difference made up out of the
                // storage for the whole of both lives (#187's finding).
                Expect.isEmpty
                    (buffered 1 1)
                    "one standing body and one generalist are the whole of what this surplus hires"

                Expect.stringStarts
                    (castName (buffered 0 1))
                    "upgrader-"
                    "one short of the standing row, the gap is the buffer's"

                Expect.stringStarts
                    (castName (buffered 1 0))
                    "worker-"
                    "and one short of the generalist, the remainder hires it"

                Expect.isEmpty
                    (buffered 2 1)
                    "a second standing body is over the quota, and it holds the generalist row down rather than casting beside it"
            }

            test "a surplus of two and a half standing bodies hires two" {
                // The pairwise on the surplus alone, one rival at a time:
                // the same room, the same 1,800 bank, the same
                // `11W/1C/11M`, and a third posted source. 45,000 in over
                // a lifetime less 2,100 of Anchor and the hauler row's
                // body is a surplus of rather over two whole standing
                // bodies, and the quota is the two — the floor is the
                // *quotient's* whole part and never a cap of one.
                let richer =
                    thirdSource (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                let richFleet = upgraderFleetAt 1800 3

                Expect.stringStarts
                    (castName (casts richer (richFleet 1 1)))
                    "upgrader-"
                    "one standing body in and the surplus still buys a whole second"

                Expect.isEmpty
                    (casts richer (richFleet 2 1))
                    "and the second is where it stops: the half body left over is not a third hire"
            }

            test "a surplus short of one whole standing body hires none of the row" {
                // The other half of the same pairwise, and the case the
                // old rounding got most wrong: one posted source instead
                // of two. 15,000 in less 700 of Anchor and the hauler's
                // body is a surplus of about three quarters of the 16,500
                // one standing body drinks. Rounded up that hired a whole
                // eleven-Work body against three quarters of the income to
                // feed it; rounded down the row is empty and the surplus
                // goes to the generalist row, which is where it went
                // before the buffer stood.
                let lean = oneSource (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                // The standing row is empty in both readings, so the fleet
                // is the one Anchor this room's one Post hires, its hauler
                // and the generalists.
                let leanFleet = upgraderFleetAt 1800 1 0

                Expect.stringStarts
                    (castName (buffered 0 0))
                    "upgrader-"
                    "the premise: at two posted sources this same buffer hires the standing row"

                Expect.stringStarts
                    (castName (casts lean (leanFleet 0)))
                    "worker-"
                    "at one it does not, and the surplus that cannot carry a standing body is the generalist's"

                Expect.isEmpty
                    (casts lean (leanFleet 1))
                    "and that one generalist is the whole of what the half-income colony hires"
            }

            test "a buffer still under construction hires none, and the worker row is unmoved" {
                // The gate, pairwise: the same room, the same bank, the
                // same two Posts, and the buffer a site instead of a
                // structure. A row hired against a promise would stand
                // beside a hole with nothing to withdraw from, so the
                // quota is zero and the surplus goes back to the
                // generalist row — ceil((26,800 − 0) / (9 × 1,500)) = 2
                // workers, which is the count this colony hired before the
                // row existed.
                let pending = upgraderColony (withBuffer (Site BuiltKind.Container))

                Expect.stringStarts
                    (castName (casts pending (upgraderFleet 0 0)))
                    "worker-"
                    "a container site is no buffer, and the row it does not hire is the standing one"

                Expect.isEmpty
                    (casts pending (upgraderFleet 0 2))
                    "and two generalists are the whole of what the surplus feeds"

                Expect.stringStarts
                    (castName (casts pending (upgraderFleet 0 1)))
                    "worker-"
                    "one short of those two, the gap is a worker's"

                // The third reading of the same tile, and the one the
                // fixture existed as before this ticket: no container at
                // the controller at all. The worker count is the site
                // case's, which is what "the buffer is the switch" means
                // — a pending container is not half a buffer.
                Expect.isEmpty
                    (casts (upgraderColony upgraderRoom) (upgraderFleet 0 2))
                    "with no container at the controller the same two generalists are the target"
            }

            test "the standing row is cast after the hauler row and before the generalist" {
                // The cascade's new rung (ADR 0046, #154's shape): the
                // three rows hired off the ground produce the surplus this
                // one spends, so they are cast first; the generalist
                // spends the same surplus at nine Work against eleven, so
                // it is cast last. Pairwise, one rival at a time — the
                // fleets below differ from the fleet above in one body
                // each.
                Expect.stringStarts
                    (castName (buffered 0 0))
                    "upgrader-"
                    "the premise: with every ground row manned the standing row is next"

                let noAnchor = upgraderFleet 0 0 |> List.filter (fun creep -> creep.Name <> "a2")

                Expect.stringStarts
                    (castName (
                        casts (upgraderColony (withBuffer (Structure BuiltKind.Container))) noAnchor
                    ))
                    "anchor-"
                    "an empty Post is filled before the buffer is manned"

                // One generalist stands in this fleet where the others
                // have none, and it is the supply floor's premise rather
                // than this reading's (ADR 0050): with the hauler filtered
                // out the fleet would be two Anchors, which can refill no
                // extension, so the cast read below would be the floor's
                // carrier — and at this bank the floor's body and the
                // hauler row's are the same body, so the name would not
                // say which row answered.
                let noHauler =
                    upgraderFleetAt 1800 2 0 1 |> List.filter (fun creep -> creep.Name <> "h1")

                Expect.stringStarts
                    (castName (
                        casts (upgraderColony (withBuffer (Structure BuiltKind.Container))) noHauler
                    ))
                    "hauler-"
                    "and so is an unshipped round trip"

                Expect.stringStarts
                    (castName (buffered 1 0))
                    "worker-"
                    "with the quota's one standing, what is left of the target is the generalist's"
            }

            test "a declared outpost's reserver is cast before the standing row" {
                // The head of the cascade keeps its place (ADR 0042): the
                // reserver decides whether an outpost's sources are worth
                // five a tick or ten, and the upgrader spends what they
                // bring in. Pairwise on the one body — the same colony
                // with the reserver alive casts the standing row.
                let declared =
                    withDeclaredOutpost (
                        upgraderColony (withBuffer (Structure BuiltKind.Container))
                    )

                Expect.stringStarts
                    (castName (casts declared (upgraderFleet 0 0)))
                    "reserver-"
                    "the row that doubles the income is cast before the row that spends it"

                let withReserver =
                    upgraderFleet 0 0
                    @ [ creepWith "r1" 0 50 [ BodyPart.Claim; BodyPart.Claim; Move; Move ] ]

                Expect.stringStarts
                    (castName (casts declared withReserver))
                    "upgrader-"
                    "and with it standing the next gap is the buffer's"
            }

            test "the worker row's floor is two while a Build stands in the pool and one otherwise" {
                // ADR 0046's floor, pairwise on the pool alone: the same
                // colony, the same fleet, one construction site between
                // the two readings. Without the floor a colony beside a
                // rich buffer would run no body that may build or repair
                // at all — a standing body is shut out of all three
                // deliveries (ADR 0046) and the hauler row has no Work
                // part.
                //
                // This fixture's income term is one since #195: the
                // remainder the standing row leaves feeds one generalist
                // mouth, so what these readings pin is the floor's *upper*
                // half — the step from one to two that a site in the pool
                // buys. The lower half, the one otherwise, is pinned at
                // the poorer bank further down ("the standing row's own
                // replacement can eat the whole remainder"), where the
                // income term really is zero and the floor is the only
                // thing hiring a body that may build.
                Expect.stringStarts
                    (castName (buffered 1 0))
                    "worker-"
                    "the remainder and the floor agree at one, and the colony is one short of it"

                Expect.isEmpty (buffered 1 1) "and with a quiet pool it stops at one"

                let building =
                    withBuildSite (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                Expect.stringStarts
                    (castName (casts building (upgraderFleet 1 1)))
                    "worker-"
                    "a site in the pool raises the floor to two, and the same fleet is one short"

                Expect.isEmpty
                    (casts building (upgraderFleet 1 2))
                    "and stops there: the floor is two, not a body per site"
            }

            test "no surplus hires no standing body, buffer or no buffer" {
                // ADR 0046's trap: the surplus is what the posted sources
                // bring in less the ground rows' amortization, so a colony
                // with no posted source has none to divide. The buffer
                // stands, the controller is there, and the row is still
                // not hired — the container is a *precondition* of the
                // quota and never its cause.
                //
                // A colony with no Post has no amortization either, so the
                // surplus here is exactly zero rather than negative and it
                // is the division's own answer that is being read: the
                // quota's `|> max 0` is the floor under the case this
                // fixture cannot reach, an amortization above income by
                // more than one whole body's lifetime drain (integer
                // division truncates toward zero, so a smaller shortfall
                // answers 0 unaided).
                //
                // The generalist below is *not* the worker row's floor
                // being read: with no Post there is no Anchor and no
                // hauler either, so the sum is one body at most and
                // `max minWorkforce` (ADR 0012) answers two whatever the
                // floor is. ADR 0046's floor is pinned where it is
                // separable — at the RCL4 bank below, where the ground
                // rows carry the sum clear of two on their own.
                let unposted =
                    { upgraderColony (withBuffer (Structure BuiltKind.Container)) with
                        Sources = []
                        Spatial =
                            withBuffer (Structure BuiltKind.Container)
                            |> fun room ->
                                { room with
                                    TargetKinds =
                                        room.TargetKinds |> Map.remove "src-a" |> Map.remove "src-b"
                                }
                    }

                Expect.stringStarts
                    (castName (buffered 0 0))
                    "upgrader-"
                    "the premise: this buffer hires the row when there is income to hire it out of"

                // One living body and not none, so the cast is read
                // through the cascade rather than through the disaster
                // fallback, which answers the generalist row whatever
                // asked (`castFromBank`).
                Expect.stringStarts
                    (castName (casts unposted [ worker "w1" 0 50 ]))
                    "worker-"
                    "with the same buffer standing and nothing coming in, the gap is the floor's"

                Expect.isEmpty
                    (casts unposted [ worker "w1" 0 50; worker "w2" 0 50 ])
                    "and the colony floor is where it stops, with no surplus to hire a standing body"
            }

            // The same room and the same standing buffer at a poorer bank,
            // which is the one knob that moves what the row's own cast is
            // (`bodyFor upgraderPattern`) and, with it, the drink the
            // surplus is divided by. The levels are the banks' real ones:
            // 1,300 is RCL4's, 800 RCL3's ten extensions, 750 the same
            // room one extension short of them, 550 RCL2's five.
            let atBank capacity level =
                { upgraderColony (withBuffer (Structure BuiltKind.Container)) with
                    Bank = bank capacity capacity
                }
                |> withLevel level

            test
                "the standing row's own replacement can eat the whole remainder, and the floor hires the generalist then" {
                // ADR 0046's floor at its lower half, separable at last
                // (#195). Rounding down leaves a remainder, but the
                // remainder is bounded by one whole drink and the standing
                // row's own replacement is charged against it before the
                // commuting row divides — so a surplus landing just past a
                // whole multiple of the drink leaves the generalist row
                // asking for nothing, and only the floor keeps a body that
                // may Build or Repair in the colony.
                //
                // One posted source at the RCL4 bank is that colony:
                // 15,000 in, less 700 of Anchor and 1,200 of hauler, is
                // 13,100 of surplus; the row's `8W/1C/8M` drinks 12,000 of
                // it, so the quota is one and 1,100 is left — and the
                // 1,250 that body costs to replace is more than the whole
                // of it, so the income term is zero. The target is one
                // Anchor, one hauler, one standing body and the floor's
                // one generalist, which is four and clear of ADR 0012's
                // colony floor of two: what is read here is ADR 0046's
                // row floor and nothing else.
                let quiet = oneSource (atBank 1300 4)

                Expect.stringStarts
                    (castName (casts quiet (upgraderFleetAt 1300 1 1 0)))
                    "worker-"
                    "the remainder feeds no generalist mouth, and the floor hires one all the same"

                Expect.isEmpty
                    (casts quiet (upgraderFleetAt 1300 1 1 1))
                    "and with a quiet pool the floor is one, so that generalist is where it stops"
            }

            test
                "the standing row's replacement is charged before the generalist row divides the remainder" {
                // The amortization term ADR 0046 asks `workforceTarget` to
                // grow, which was inert while the quota rounded up and
                // bites since #195: what the commuting row divides is the
                // remainder *less* the standing bodies' own replacement,
                // or the colony hires an upgrade mouth out of energy that
                // replacement is already spending.
                //
                // One posted source at the RCL3 bank is where the charge
                // is the whole of the answer: 15,000 in, less 700 of
                // Anchor and 750 of hauler, is 13,550; the row's
                // `5W/1C/5M` drinks 7,500, so the quota is one and 6,050
                // is left. Charged the 800 that body costs to replace it
                // is 5,250, and ADR 0037's rounding turns that against a
                // 6,000 drain into one generalist. Uncharged it would be
                // 6,050 — over the drain, and a second generalist.
                let poorer = oneSource (atBank 800 3)

                Expect.stringStarts
                    (castName (casts poorer (upgraderFleetAt 800 1 1 0)))
                    "worker-"
                    "5,250 of remainder against a 6,000 drain is one generalist"

                Expect.isEmpty
                    (casts poorer (upgraderFleetAt 800 1 1 1))
                    "and it is one and not two: the 6,050 that would have bought a second is the standing body's replacement"
            }

            test "a bank whose own cast is no standing body hires none of the row" {
                // The gate's other half (#187, ADR 0046's amended
                // Consequences): the row is *counted* by `patternOf`, off
                // the parts, so at a bank where the sizing rule's own cast
                // is read back to the generalist the quota hires nobody.
                // Pairwise on the bank alone — 800 against 750 at the same
                // RCL3 — because 800 is where one Carry against
                // `floor((capacity - 50) / 150)` Work reaches four Work to
                // the Carry.
                // Two haulers standing, as the RCL2 arm below has: the
                // hauler row is priced at the dearest sink it reaches, and
                // at an 800 bank this room's two containers ask for two
                // bodies, whose gap would be cast ahead of the upgrader's.
                Expect.stringStarts
                    (castName (casts (atBank 800 3) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "upgrader-"
                    "at the 800 bank the row's own cast is a standing body, so the surplus hires it"

                Expect.stringStarts
                    (castName (casts (atBank 750 3) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "worker-"
                    "fifty energy poorer the same cast is `4W/1C/4M` and the row is not hired"

                // The RCL2 bank's hauler body carries 300 where the 800
                // bank's carries 500, so the two containers' summed demand
                // comes to two bodies there and one here (ADR 0049); the
                // reading is the row *under* the hauler's, so its own gap
                // is filled before the case is read.
                Expect.stringStarts
                    (castName (casts (atBank 550 2) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "worker-"
                    "and at the RCL2 bank, where the cast is `3W/1C/3M`, the surplus is the generalist's"
            }

            test "the poor band's hire is bounded: a row it cannot count is a row it does not hire" {
                // The failure the gate above exists to prevent, pinned as
                // the trace that would have caught it: hiring at a bank
                // whose cast `patternOf` reads back to the generalist
                // leaves `upgraderGap` at the full quota every tick,
                // however many of that very body are alive — a fresh
                // `3W/1C/3M` cast for ever, ahead of the whole-fleet
                // deficit that is the only gate on the generalist row.
                //
                // So the count of casts must stop growing with the living
                // count. Both Posts are manned and both round trips
                // shipped, so what is left is the two surplus rows.
                let living n =
                    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100; hauler "h2" 0 100 ]
                    @ [ for i in 1..n -> creepWith $"u{i}" 0 50 (bodyFor upgraderPattern 550) ]

                for n in [ 0; 1; 3; 10 ] do
                    Expect.isFalse
                        (casts (atBank 550 2) (living n)
                         |> List.exists (fun (_, _, name: string) -> name.StartsWith "upgrader-"))
                        $"no upgrader is cast at the RCL2 bank with {n} of that body alive"

                // And the colony settles: those bodies are generalists by
                // the ratio and the whole-fleet deficit counts them, so the
                // spawn goes quiet instead of casting into a gap that never
                // closes.
                Expect.isEmpty
                    (casts (atBank 550 2) (living 10))
                    "ten of them fill the target, and nothing is cast at all"
            }
        ]

/// The same colony with one tunable moved — the whole of what a pairwise
/// case on `Tuning` does, and the reason a rule reads its numbers off the
/// [[colony view]] rather than off a module constant (ADR 0052 decision 5).
let private tunedBy (change: Tuning -> Tuning) (colony: ColonyView) =
    { colony with
        Tuning = change colony.Tuning
    }

/// A one-room colony whose whole haul is one source container's, with the
/// colony's controller — and the upgrade [[buffer]] standing one tile off
/// it — at the given x on the same three-wide lane. The spawn stands at
/// (25,25) and the source's container at (21,25), so moving the controller
/// moves the buffer's leg and nothing else about the room.
///
/// The controller and its buffer move **together**, which is the one place
/// this departs from the ticket's wording ("the buffer beside the
/// controller against the buffer hugging the spawn"). A container is a
/// buffer because it stands in the controller's own Upgrade Work Area
/// (`Atlas.controllerContainers`, ADR 0019); a container parked by the
/// spawn with the controller left across the room is no buffer at all and
/// would leave the colony with one sink again, which is the state before
/// the case rather than the other half of it. W13S28's own geometry is the
/// pair as written: a controller thirty tiles from the Post that feeds it.
let private sinkLaneColony available controllerX =
    let spawnPos = { X = 25; Y = 25 }

    { bareRespawn with
        Controller = Some(controllerAt 4)
        Refillables = []
        Sources = [ source "src-a" ]
        Bank = bank available available
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-a", Source
                            "can-src", Structure BuiltKind.Container
                            "ctrl-1", Controller
                            "can-buf", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList
                            [
                                for x in 18..47 do
                                    for y in 24..26 ->
                                        { X = x; Y = y },
                                        (if x = 21 && y = 25 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList
                            [
                                "spawn-1", spawnPos
                                "src-a", { X = 21; Y = 25 }
                                "can-src", { X = 22; Y = 25 }
                                "ctrl-1", { X = controllerX; Y = 25 }
                                "can-buf", { X = controllerX - 1; Y = 25 }
                            ]
                    Obstacles = Set.ofList [ spawnPos; { X = controllerX; Y = 25 } ]
                })
    }

/// A colony whose one [[post]] is an outpost's, so the Anchor row's ceiling
/// is that room's rate and moves with the reservation on it — which is
/// what makes the row's cast, and therefore its lead, readable at this
/// seam. One Anchor alive with `life` ticks left standing two tiles from
/// the spawn, and one hauler beside it so the supply floor is quiet.
let private outpostPostColony held life =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-out" ]
        Bank = bank 1800 1800
        RoomControl =
            Map.ofList
                [
                    "W1N1", ownedRoom
                    "W1N2", (if held then reservedRoom true 5000 else neutralRoom)
                ]
        Creeps = [ anchor "a1" 0 50 |> withLife life; hauler "h1" 0 100 ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-out", Source
                            "can-out", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 1..10 -> { X = x; Y = y }, Plain
                            ]
                    TargetPositions = Map.ofList [ "spawn-1", { X = 10; Y = 3 } ]
                    CreepPositions =
                        Map.ofList [ "a1", { X = 10; Y = 5 }; "h1", { X = 10; Y = 6 } ]
                    Obstacles = Set.singleton { X = 10; Y = 3 }
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 44..48 ->
                                        { X = x; Y = y }, (if x = 10 && y = 45 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 10; Y = 45 }; "can-out", { X = 10; Y = 44 } ]
                }
    }

/// **One [[post]] on each side of a border, over two rocks the colony
/// prices differently**: its own room's source at (10,9) with its
/// container standing on the Seat (10,8), and `outpostPostColony`'s
/// outpost rock at (10,45) with its container on (10,44). An Anchor
/// garrisons each of them, standing on the Post itself, and a hauler keeps
/// the [[supply floor]] quiet.
///
/// The shape ADR 0053 is about and the one no fixture could reach before
/// it: while the home Post is in the projection an owned room prices at
/// the held rate, so the old colony-wide `List.max` answered six Work for
/// *both* rocks however the outpost's controller stood — which is why the
/// only fixture that could move the number was one that deleted the home
/// room's Posts (`withoutHomePosts`).
///
/// Three dials and no others: who holds W1N2, and how long each of the two
/// garrisons has left. The one with the shorter life is the one that goes
/// [[expiring]] and so the one whose Post the row is casting into (ADR
/// 0026), which is the whole of what pairs a body to a rock here.
let private pairedPostColony held homeLife outLife =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-home"; source "src-out" ]
        Bank = bank 1800 1800
        RoomControl =
            Map.ofList
                [
                    "W1N1", ownedRoom
                    "W1N2", (if held then reservedRoom true 5000 else neutralRoom)
                ]
        Creeps =
            [
                anchor "a-home" 0 50 |> withLife homeLife
                anchor "a-out" 0 50 |> withLife outLife
                hauler "h1" 0 100
            ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-home", Source
                            "can-home", Structure BuiltKind.Container
                            "src-out", Source
                            "can-out", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 1..10 ->
                                        { X = x; Y = y }, (if x = 10 && y = 9 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList
                            [
                                "spawn-1", { X = 10; Y = 3 }
                                "src-home", { X = 10; Y = 9 }
                                "can-home", { X = 10; Y = 8 }
                            ]
                    CreepPositions =
                        Map.ofList [ "a-home", { X = 10; Y = 8 }; "h1", { X = 10; Y = 6 } ]
                    Obstacles = Set.singleton { X = 10; Y = 3 }
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 44..48 ->
                                        { X = x; Y = y }, (if x = 10 && y = 45 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 10; Y = 45 }; "can-out", { X = 10; Y = 44 } ]
                    CreepPositions = Map.ofList [ "a-out", { X = 10; Y = 44 } ]
                }
    }

/// `pairedPostColony` with the outpost's garrison never hired, so its Post
/// stands **genuinely** empty beside a home Post whose incumbent is still
/// standing on it.
///
/// Which is what makes the arrival reading discriminate at all (ADR 0026,
/// ADR 0053 trap (i)): with both Posts garrisoned a rule that judged a
/// vacancy by who is standing *now* finds no free Post anywhere and falls
/// back to the richest ceiling — the same six Work arrival gives, for the
/// wrong reason. Leave the outpost's Post empty and the two readings part:
/// arrival counts the expiring home incumbent out and buys for its held
/// rock, where a standing read sees only the neutral hole and buys three
/// Work for a rock giving ten.
let private withoutOutpostGarrison (colony: ColonyView) =
    { colony with
        Creeps = colony.Creeps |> List.filter (fun creep -> creep.Name <> "a-out")
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { Map.find "W1N2" colony.Spatial.Rooms with
                    CreepPositions = Map.empty
                }
    }

/// A colony whose surplus lands in the band #200 is about: five Posts of
/// its own paying ten a tick each, no haul priceable at all — the rocks
/// and the controller sit in two regions of one room with no ground
/// between them — and an 1,800 bank. Income 50 a tick over a lifetime is
/// 75,000; the Anchor row's five 700-energy bodies are the only
/// amortization, so the surplus is exactly **71,500**.
///
/// At that surplus the upgrader row's two divisors part: 71,500 over one
/// body's lifetime drink (16,500) is four, and over the drink plus the
/// body it is spent on (18,200) is three. The fleet is the readout.
let private upgraderBandColony upgraders =
    let rocks =
        [
            { X = 7; Y = 7 }
            { X = 7; Y = 10 }
            { X = 7; Y = 13 }
            { X = 12; Y = 7 }
            { X = 12; Y = 10 }
        ]

    let cans = rocks |> List.map (fun rock -> { rock with X = rock.X + 1 })

    { bareRespawn with
        Controller = Some(controllerAt 5)
        Refillables = []
        Bank = bank 1800 1800
        Sources = [ for i in 1 .. List.length rocks -> source $"src-{i}" ]
        Creeps =
            [ for i in 1..5 -> anchor $"a{i}" 0 50 ]
            @ [ hauler "h1" 0 100 ]
            @ [
                for i in 1..upgraders ->
                    creepWith
                        $"u{i}"
                        0
                        50
                        (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
            ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                TargetKinds =
                    Map.ofList (
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "ctrl-1", Controller
                            "can-buf", Structure BuiltKind.Container
                        ]
                        @ [ for i in 1 .. List.length rocks -> $"src-{i}", Source ]
                        @ [
                            for i in 1 .. List.length cans ->
                                $"can-{i}", Structure BuiltKind.Container
                        ]
                    )
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList (
                            [
                                for x in 5..15 do
                                    for y in 5..15 ->
                                        { X = x; Y = y },
                                        (if List.contains { X = x; Y = y } rocks then
                                             Wall
                                         else
                                             Plain)
                            ]
                            @ [
                                for x in 30..40 do
                                    for y in 30..40 -> { X = x; Y = y }, Plain
                            ]
                        )
                    TargetPositions =
                        Map.ofList (
                            [
                                "spawn-1", { X = 34; Y = 34 }
                                "ctrl-1", { X = 35; Y = 35 }
                                "can-buf", { X = 34; Y = 35 }
                            ]
                            @ [ for i, rock in List.indexed rocks -> $"src-{i + 1}", rock ]
                            @ [ for i, can in List.indexed cans -> $"can-{i + 1}", can ]
                        )
                    Obstacles = Set.ofList [ { X = 34; Y = 34 }; { X = 35; Y = 35 } ]
                })
    }

[<Tests>]
let quotaInputTests =
    testList
        "the quota inputs read this colony's own cast"
        [
            test "the hauler row prices its haul to the buffer as well as to the spawn" {
                // #216 R4's scope note, on the geometry that cost W13S28 a
                // thousand ticks: this colony's sinks are the
                // spawn/extension cluster **and** the controller's
                // [[buffer]], each priced at its own round trip and the
                // flow spread over both. Pairwise on the controller's
                // position alone — the same rock, the same container, the
                // same spawn, the same bank.
                //
                // The third number in each row is the same colony with the
                // buffer taken out of the census, which is the sink set as
                // it was before R4: beside the spawn the buffer's own leg
                // is the cluster's and the answer does not move, and
                // fifteen tiles further off it is most of the colony's haul
                // and the row hires for it. That is the live shape — a
                // buffer at zero, a container overflowing 2,000 with 1,859
                // on the ground beside it, and seven mini workers walking
                // fifty tiles for what one hauler was never hired to bring.
                let withoutBuffer (colony: ColonyView) =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.remove "can-buf" colony.Spatial.TargetKinds
                            }
                    }

                Expect.equal
                    (quotaOf (sinkLaneColony 150 28))
                    1
                    "a controller beside the spawn: one hauler covers both sinks"

                Expect.equal
                    (quotaOf (withoutBuffer (sinkLaneColony 150 28)))
                    1
                    "and the spawn alone answers the same, because the two legs are one trip"

                Expect.equal
                    (quotaOf (sinkLaneColony 150 40))
                    2
                    "a controller across the room is a second haul, priced at its own trip"

                Expect.equal
                    (quotaOf (withoutBuffer (sinkLaneColony 150 40)))
                    1
                    "and the spawn alone still answers one, which is the body the buffer never got"
            }

            test "a nearer sink lowers nothing: the row is sized to the dearest sink it reaches" {
                // The mirror of the case above. Under R4's mean this read
                // *two*: the average was a claim about proportion, and a
                // buffer one tile from the container pulled the long haul
                // to the cluster down. Live (W13S28, 2026-09-07) the near
                // sink was the spawn cluster, which fills in a trip, so the
                // flow that ran all day was the far one and the mean hired
                // a body short while both containers stood full. The row is
                // priced at the dearest reachable sink now, so a nearer sink
                // never lowers the count.
                let clusterAcrossTheRoom (colony: ColonyView) =
                    let away = { X = 45; Y = 25 }

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions = Map.add "spawn-1" away layer.TargetPositions
                                    Obstacles =
                                        layer.Obstacles
                                        |> Set.remove { X = 25; Y = 25 }
                                        |> Set.add away
                                })
                    }

                let colony = clusterAcrossTheRoom (sinkLaneColony 150 24)

                let withoutBuffer =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.remove "can-buf" colony.Spatial.TargetKinds
                            }
                    }

                Expect.equal
                    (quotaOf withoutBuffer)
                    3
                    "priced to the spawn alone the long haul asks for three bodies"

                Expect.equal
                    (quotaOf colony)
                    3
                    "and with a buffer one tile off the container it still asks for three: the cluster across the room is the sink the flow runs to"
            }

            test "the capture the scope note was written from hires the hauler it was missing" {
                // The ticket's own pairwise, on the room it names: W13S28
                // at RCL3 on an 800 bank, the child whose north container
                // held 2,000 with 1,859 decaying beside it while one hauler
                // ran to the spawn. `RoomFixtures.colonyAt` furnishes the
                // captured room as its Layout would have left it — a
                // container on each source's Seat and the upgrade buffer
                // beside the controller — so the legs here are the room's
                // real terrain and not a lane drawn to make a point.
                //
                // Pairwise on the sink set alone, which is the only thing
                // R4 moved: the same room, the same bank, the same fleet,
                // with the buffer in the census and then out of it (the
                // rule as it stood before R4). The numbers are the room's
                // answer rather than a chosen value — if the fixture's own
                // furniture moves they move with it, and what must not
                // move is that the buffer is worth a body here.
                let colony = RoomFixtures.colonyAt (RoomFixtures.load "W13S28") 3 800

                let spawnOnly =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.remove "cont-ctrl" colony.Spatial.TargetKinds
                            }
                    }

                Expect.equal
                    (quotaOf spawnOnly)
                    1
                    "priced to the spawn alone the row hires one, which is what the live colony had"

                Expect.equal
                    (quotaOf colony)
                    2
                    "and priced to the buffer as well it hires the second, which is the whole of the fix"
            }

            test "the ferry is hired for a bootstrapping child and for no other stage" {
                // #222's quota half (ADR 0052 decision 7). A mother hauls
                // her stock into a child that is still being raised and
                // stops the tick it is `Independent`, which is what the
                // stage means. Pairwise on the child's stage alone: the
                // same rooms, the same Storage, the same declaration and
                // the same borrowing list — each of them at the one load
                // the rule was derived at, which is now the shipped default
                // too (#216 R5 landed the Refill half).
                let lending stage =
                    ferryMother stage |> tunedBy (fun t -> { t with FerryLoads = 1 })

                Expect.equal (quotaOf (lending Bootstrapping)) 1 "one ferry body for the child"

                Expect.equal
                    (quotaOf (lending Independent))
                    0
                    "and none once the child feeds itself"

                Expect.equal
                    (quotaOf (lending Nursery))
                    0
                    "nor for a nursery, which has no buffer to fill and no mouth to drink it"

                // The shipped number **is** the derived one since #216 R5:
                // the Refill that spends a ferried load stands beside this
                // term now, pooled for the very tile the round trip above
                // was priced to, so the body hired here has a Task and the
                // two halves of the split feature ship together.
                Expect.equal
                    (quotaOf (ferryMother Bootstrapping))
                    1
                    "and the shipped default lends the one body the rule was derived at"

                // The cap is what makes the borrowing an exception rather
                // than a second economy (ADR 0052 decision 7): what a
                // mother lends is written down, never derived from how much
                // the child could absorb.
                Expect.equal
                    (quotaOf (
                        ferryMother Bootstrapping |> tunedBy (fun t -> { t with FerryLoads = 2 })
                    ))
                    2
                    "and `Tuning.FerryLoads` is the whole of how much she lends"
            }

            test "the ferry's sink is the mother's to fill and never to draw" {
                // #222's pool half (ADR 0052 decision 7), pairwise on the
                // child's [[stage]]. The whole of the lend is energy going
                // one way: the buffer is a Refill target of hers while the
                // child is being raised, and the Withdraw the same store
                // would otherwise carry is denied her — left in, her hauler
                // would take the load it just carried across the Seam
                // straight back out again, the ADR 0019 cycle over a
                // border, with the child's own upgraders drinking against
                // her.
                let poolFor stage = planTasks (ferryMother stage) noThreats

                let lending = poolFor Bootstrapping

                Expect.isTrue
                    (List.contains (Refill "can-child") lending)
                    "the child's buffer is a sink of hers while she is raising it"

                Expect.isFalse
                    (List.contains (Withdraw "can-child") lending)
                    "and never an intake, however much stands in it"

                let grown = poolFor Independent

                Expect.isFalse
                    (List.contains (Refill "can-child") grown)
                    "an independent child feeds itself, which is what the stage means"

                // And the denial does **not** ride on the lend. The two
                // sets are not the same set: the ferry lends to a
                // `Bootstrapping` child alone, while `Borrowed.Rooms` also
                // holds the [[nursery]] she is raising and the child she has
                // lost (#221) — so a Withdraw denied only where a Refill is
                // pooled would make those rooms' stores plain Feeding-tier
                // intakes of hers, which is the cross-Seam drain ADR 0047
                // refuses and the exact inverse of the lend. Pairwise on
                // the stage, over the same store the case above pools the
                // Refill for.
                Expect.isFalse
                    (List.contains (Withdraw "can-child") (poolFor Nursery))
                    "a nursery's store is not hers to draw either"

                Expect.isFalse
                    (List.contains (Withdraw "can-child") grown)
                    "nor a grown child's: no store of a child's is ever her intake"

                // What bounds the lend is the capacity and never the tier
                // (ADR 0052 decision 6): the Refill sits on the buffer's own
                // deep tier like her own, and `Tuning.FerryLoads` is the
                // whole of how many bodies may cross for it — the same
                // number the hauler row was raised by, so the quota and the
                // pool cannot disagree.
                let bound =
                    poolOn (ferryMother Bootstrapping)
                    |> List.tryPick (fun entry ->
                        if entry.Task = Refill "can-child" then
                            Some entry.Capacity.Total
                        else
                            None)

                Expect.equal
                    bound
                    (Some(Some Tuning.defaults.FerryLoads))
                    "the pool carries the lend's bound, and it is the tuned one"
            }

            test "a hungry ferry sink opens the mother's stock" {
                // ADR 0023's gate reads "some Refill target **other than
                // the Storage** has free capacity", and a bootstrapping
                // child's buffer is exactly one (#222). Without it counted,
                // the two conditions were close to mutually exclusive: the
                // ferry's Refill sits on the buffer's own deep tier, so a
                // load reaches it only once the spawn, the extensions and
                // the home buffer are full — which is precisely the state
                // that leaves `refills` and `containerRefills` empty — and
                // the body `haulerQuota` hires for the lend, priced on the
                // round trip from this Storage to that buffer, had a sink
                // and no intake at all.
                //
                // This fixture is that state by construction: no
                // Refillables, no home buffer, and a stocked Storage.
                let stocked stage =
                    let mother = ferryMother stage

                    { mother with
                        Spatial =
                            { mother.Spatial with
                                Stores = Map.add "storage-1" 240000 mother.Spatial.Stores
                            }
                    }

                let lending = planTasks (stocked Bootstrapping) noThreats

                Expect.isTrue
                    (List.contains (Refill "can-child") lending)
                    "the lend is the one hungry sink she has"

                Expect.isTrue
                    (List.contains (Withdraw "storage-1") lending)
                    "so the stock it is priced from is drawable"

                // The pairwise control on the one fact that decides it: the
                // child's stage. With no lend there is no sink at all, and
                // the gate shuts exactly as it always did — the stock is
                // not opened against its own Refill (ADR 0023).
                Expect.isFalse
                    (List.contains
                        (Withdraw "storage-1")
                        (planTasks (stocked Independent) noThreats))
                    "and with nothing to feed, the stock stays shut"
            }

            test "a mother with no stock ferries nothing" {
                // The other half of "priced from her Storage": the stock is
                // the only energy a mother holds that her own rows are not
                // already hired against (ADR 0023), so a colony without one
                // has nothing to send whatever stage its child stands at.
                let stockless =
                    let mother =
                        ferryMother Bootstrapping |> tunedBy (fun t -> { t with FerryLoads = 1 })

                    { mother with
                        Spatial =
                            { mother.Spatial with
                                TargetKinds = Map.remove "storage-1" mother.Spatial.TargetKinds
                            }
                    }

                Expect.equal (quotaOf stockless) 0 "no Storage, no ferry"
            }

            test "the upgrader row's divisor carries the body as well as its drink" {
                // #200, the ADR 0046 correction, read at the fleet. The
                // surplus is 71,500 by construction (`upgraderBandColony`),
                // which the drain alone divides into **four** bodies and
                // the drain plus the body it is spent on into **three**.
                //
                // Read one body at a time, the way the container switch is:
                // the colony standing at the quota casts nothing of this
                // row and one body short casts one, so a quota that had
                // stayed at four would show as a fourth upgrader here.
                //
                // What the fourth cost is the double sale #195 fixed, one
                // order of magnitude smaller: four bodies drink 66,000 and
                // cost 6,800 to replace, 1,300 over an income of 71,500,
                // with the difference coming out of the Storage every tick
                // of both lives.
                Expect.equal
                    (castRows (decide (upgraderBandColony 2) Map.empty Set.empty None).Intents)
                    [ "upgrader" ]
                    "two upgraders standing and the row is a body short"

                Expect.equal
                    (castRows (decide (upgraderBandColony 3) Map.empty Set.empty None).Intents)
                    [ "worker" ]
                    "three, and the row is full: the surplus pays for three mouths and three bodies"

                Expect.isEmpty
                    (castRows (decide (upgraderBandColony 4) Map.empty Set.empty None).Intents)
                    "and four is over every row's quota, so nothing is cast at all"
            }

            test "a lead is priced at the body its row will cast, not the largest it could" {
                // #158's second half, at the seam a lead is observable
                // from: an Anchor is expiring when its life is at or under
                // the ticks its replacement needs to stand where it stands
                // (ADR 0026), and an expiring one leaves its row's count so
                // the successor is cast while it still works.
                //
                // Pairwise on the reservation of the room its one Post
                // stands in, which is the only input that moves. Held, the
                // row's ceiling is six Work and its cast is `6W/1C/1M` —
                // eight parts, 24 ticks in the spawner and a slow walk out
                // — and a twenty-tick-old Anchor is inside that lead. Under
                // no reservation the rock gives five, the ceiling is three,
                // the cast is `3W/1C/1M` — five parts and a faster walk —
                // and the same Anchor is not.
                //
                // Before #158 the lead was `bodyFor`'s answer, which is the
                // held ceiling's six Work whatever the room pays: both
                // arms answered "expiring", the successor was cast some
                // nine ticks plus three quarters of a walk early, and ADR
                // 0024's arrival-priced Post capacity counted the incumbent
                // as the holder — the fresh Anchor standing beside the
                // spawn reading its own Post as full, which is the
                // `IdleReason.NoneFree` ADR 0026 names as the symptom.
                Expect.equal
                    (castRows (decide (outpostPostColony true 20) Map.empty Set.empty None).Intents)
                    [ "anchor" ]
                    "a held rock: the row casts an eight-part body and twenty ticks is inside its lead"

                Expect.equal
                    (castRows (decide (outpostPostColony false 20) Map.empty Set.empty None).Intents)
                    [ "worker" ]
                    "an unheld one: the row casts five parts, the lead is shorter, and the incumbent still counts"
            }

            test "a vacant Post is cast into with a body sized for its own rock" {
                // **ADR 0053's first half**, and the shape #158 filed:
                // `anchorWorkCapOf` folded every posted source into one
                // colony-wide `List.max`, and the colony's own owned room
                // is in that fold at the held rate — so the ceiling was six
                // Work in every state a colony with one posted home source
                // can reach, and an outpost whose reservation had lapsed
                // went on being garrisoned by `6W/1C/1M` against a rock
                // giving five. Twelve a tick bought for a rock that gives
                // five, for the whole of a 1,500-tick life.
                //
                // What pairs a body to a rock without giving a cast a role
                // (ADR 0021, ADR 0006) is the **vacancy** it is filling:
                // the row is casting into one empty Post, that Post seats
                // one rock, and the rock's rate is a fact of the
                // projection.
                //
                // Pairwise on the outpost's reservation alone — the same
                // two Posts, the same two garrisons, the same 1,800 bank,
                // and the outpost's Anchor the expiring one in both arms.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 1500 20))
                    [ threeWork ]
                    "the vacancy is on a rock nobody holds: three Work drain it as fast as it fills"

                Expect.equal
                    (anchorCastsBy (pairedPostColony true 1500 20))
                    [ sixWork ]
                    "reserved, the same vacancy is worth ten a tick and the row buys the six Work that dig it"
            }

            test "an ordinary home succession is not sized off an outpost's neutral rock" {
                // Trap (i) of #158, which is why the vacancy has to be
                // judged at **arrival** (ADR 0026) rather than by who is
                // standing where. The colony's own Post is garrisoned by an
                // expiring Anchor — it is still standing on it, and will be
                // dead before a replacement could arrive — so that Post is
                // the vacancy and its own held rock sizes the successor.
                //
                // Read off who is standing instead, the home Post would
                // read as taken, the row would fall through to the
                // outpost's Post as the only free one, and the home room's
                // replacement would be cast at three Work against a rock
                // giving ten: four energy a tick lost for a whole life,
                // which is the error ADR 0042's fold existed to prevent and
                // the reason it could not simply be narrowed.
                //
                // Pairwise against the arm above, on which of the two
                // garrisons is expiring — the outpost stands unreserved in
                // both, so the colony's cheapest rock is the neutral one in
                // both.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 40 1500))
                    [ sixWork ]
                    "the home room's own Post is the vacancy, and its rock gives ten whatever the outpost pays"

                // And again with the outpost's Post standing genuinely
                // empty, which is the arm that makes the reading a
                // *judgement* rather than a coincidence: with both Posts
                // garrisoned above, a standing read finds no free Post at
                // all and falls back to the richest ceiling, answering six
                // Work for the wrong reason. Here it would find the
                // outpost's hole and only that one, and cast the home
                // room's replacement at three Work against a rock giving
                // ten.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 40 1500 |> withoutOutpostGarrison))
                    [ sixWork ]
                    "an expiring incumbent's own Post is a vacancy even with a neutral one standing open beside it"
            }

            test
                "a bank short of the dearest vacancy casts no Anchor rather than the cheapest one's body" {
                // The hole a per-vacancy seat list opens in the cascade
                // (ADR 0053's rejected option, ADR 0050's step-over): a
                // spawn takes the first seat its bank can pay for and steps
                // over the ones it cannot, so seats sized richest-first at
                // *different* prices are cheapest-first at every bank
                // between two of them.
                //
                // Both garrisons expiring, so both Posts read vacant — the
                // held home rock at six Work and the neutral outpost's at
                // three — and 600 available against an 1,800 capacity. The
                // dearest vacancy's body costs 700 and the cheapest's 400.
                // Sized one seat per vacancy, this tick buys the 400: a
                // three-Work Anchor born beside the home spawn, a few tiles
                // from the held Post it will be pinned to by travel cost
                // and a Seam from the neutral one it was bought for, digging
                // six a tick where the rock gives ten for the whole of a
                // 1,500-tick life. Sized at the dearest vacancy the row
                // yields the tick instead and buys the six Work the tick
                // the bank holds 700.
                //
                // Pairwise on the bank alone, against the same fixture at
                // its own 1,800.
                let atBank available =
                    let colony = pairedPostColony false 20 20

                    { colony with
                        Bank = bank available 1800
                    }
                    |> anchorCastsBy

                Expect.equal
                    (atBank 600)
                    []
                    "600 buys neither the held Post's body nor a cheaper one for a rock this cast cannot be steered to"

                Expect.equal
                    (atBank 1800)
                    [ sixWork ]
                    "and the same two vacancies at a bank that can pay buy the dearer of them"
            }

            test "a lead on a Post is priced at the body that Post will be cast" {
                // #158's second half where ADR 0053 puts it: a [[lead]] is
                // what the successor needs to stand **where this creep
                // stands** (ADR 0026), and where an Anchor stands is its
                // own Post — so the successor is that Post's body and not
                // the row's largest, nor the colony's richest.
                //
                // Pairwise on the outpost's reservation alone, with the
                // same 40 ticks left on the same garrison standing on the
                // same tile a Seam away. Held, its successor is `6W/1C/1M`:
                // eight parts, 24 ticks in the spawner and a slow crossing,
                // a lead of 66 — so at 40 it is expiring and the row casts.
                // Unheld, its successor is `3W/1C/1M`: five parts, 15 ticks
                // and a faster body, a lead of 36 — and at 40 it still
                // counts, so the tick's body goes to the generalist row.
                //
                // The colony's own held Post stands beside it in both arms
                // and moves neither answer, which is the half a lead read
                // off the colony's richest ceiling would get wrong: it
                // would price both arms at 66 and cast a successor 30 ticks
                // early, to stand beside the spawn reading its own Post as
                // full (`IdleReason.NoneFree`, ADR 0026).
                Expect.equal
                    (castRows
                        (decide (pairedPostColony true 1500 40) Map.empty Set.empty None).Intents)
                    [ "anchor" ]
                    "a held rock: the successor is eight parts and forty ticks is inside its lead"

                Expect.equal
                    (castRows
                        (decide (pairedPostColony false 1500 40) Map.empty Set.empty None).Intents)
                    [ "worker" ]
                    "an unheld one: the successor is five parts, the lead is shorter, and the incumbent still counts"
            }

            test "a rival's room hires no reserver on the tick it is first seen held" {
                // #184. The [[stand-down]] withdraws from a room somebody
                // else has taken (ADR 0043), but the gate reads the
                // *previous* tick's [[raid log]] — so on the tick the
                // colony first sees the room held it is still in the scan
                // set, carries no reservation of ours and reads the whole
                // 5,000-tick deficit. The row cast the bank's largest
                // reserver body at it, 1,300 energy for a creep the engine
                // would refuse at the controller.
                //
                // Pairwise on the room's owner alone — the same
                // declaration, the same rock, the same fleet, the same
                // bank — and closed with the same-tick fact rather than
                // with a gate that arrives a tick late.
                let castsIn control =
                    reserverColony [ northOutpost false ] (surplusFleet 2) [ "W1N2", control ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsIn neutralRoom)
                    [ twoBlocks ]
                    "a room nobody holds is the outpost this row exists for"

                Expect.isEmpty
                    (castsIn rivalRoom)
                    "and a room somebody else owns hires nobody: the engine refuses a reservation there"
            }

            test "a body in the oven fills its row's gap: two idle spawns cast one Anchor" {
                // #156. A creep still spawning is in no `Creeps` list — it
                // cannot act, cannot be matched and holds no tile — so
                // every row's living count read straight past it. With one
                // spawn that was harmless, because the spawn casting the
                // body is busy; with two it is not: spawn one casts an
                // Anchor for the empty Post at tick T, and at T+1 the gap
                // is still one, spawn one is still busy, and spawn two
                // casts a second Anchor for the same Post.
                //
                // ADR 0026 rejected counting a gestating body and named the
                // reason that has since expired — "the deficit already
                // stops double-casting through the spawn's own
                // `IsSpawning`" — which was true of one spawn and of no
                // other number of them.
                //
                // Pairwise on the oven alone: the same colony, the same
                // fleet, the same Post, with and without the body already
                // bought for it.
                let colony casting =
                    { quotaColony 15 2 300 with
                        Creeps = [ worker "w1" 0 50 ]
                        Casting = casting
                    }

                Expect.equal
                    (castRows (decide (colony []) Map.empty Set.empty None).Intents |> List.head)
                    "anchor"
                    "an empty Post and nothing bought for it: the Anchor row is a body short"

                Expect.isFalse
                    (List.contains
                        "anchor"
                        (castRows
                            (decide
                                (colony [ [ Work; Work; Carry; Move ] ])
                                Map.empty
                                Set.empty
                                None)
                                .Intents))
                    "and with one already in an oven the row is full: the second spawn buys something else"
            }

            test "the oven's body is read back to the row that bought it" {
                // The counting rule that makes the case above safe: a
                // gestating body fills the gap of the row `patternOfCast`
                // reads it into, and of no other. A hauler in the oven
                // leaves the Anchor row exactly as short as it was — which
                // is what keeps this from being a blanket "one body in
                // flight suppresses one cast".
                let colony casting =
                    { quotaColony 15 2 300 with
                        Creeps = [ worker "w1" 0 50 ]
                        Casting = casting
                    }

                Expect.equal
                    (castRows
                        (decide (colony [ [ Carry; Carry; Move ] ]) Map.empty Set.empty None)
                            .Intents
                     |> List.head)
                    "anchor"
                    "a hauler in the oven pays off no Anchor gap"
            }
        ]

[<Tests>]
let tuningTests =
    testList
        "the tunables"
        [
            test "MinWorkforce is the floor no colony plans below" {
                // A colony with nothing to hire for — no Post, no
                // container, no site — is at its floor and nothing else, so
                // the floor is the whole of its target and the fleet reads
                // it back one body at a time.
                let colony =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                Expect.isEmpty
                    (castRows (decide colony Map.empty Set.empty None).Intents)
                    "two bodies is the shipped floor, and a colony at its floor casts nothing"

                Expect.equal
                    (castRows
                        (decide
                            (colony |> tunedBy (fun t -> { t with MinWorkforce = 3 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ "worker" ]
                    "raise the floor by one and the same colony is one body short"
            }

            test "RepairTrigger is the fraction a decaying kind enters the pool below" {
                let road = bareRespawn |> withHits "road-1" BuiltKind.Road 3000 5000

                Expect.isEmpty
                    (repairTasks (planTasks road noThreats))
                    "three fifths of max is above the shipped half, so the road is left alone"

                Expect.equal
                    (repairTasks (
                        planTasks
                            (road |> tunedBy (fun t -> { t with RepairTrigger = 0.7 }))
                            noThreats
                    ))
                    [ "road-1" ]
                    "a trigger of seven tenths and the same road is hungry"
            }

            test "RampartFloor is the hits a rampart is whole at" {
                // Read at `Independent`, which is the only [[stage]] that
                // keeps a rampart at all (#214): below it the covering rule
                // and this floor are both switched off, so the number is
                // never asked for at a 300 bank rather than answered wrongly
                // there.
                let keep =
                    bareRespawn |> withLevel 5 |> withHits "ram-1" BuiltKind.Rampart 150_000 300_000

                Expect.isEmpty
                    (repairTasks (planTasks keep noThreats))
                    "a hundred and fifty thousand is over the shipped floor"

                Expect.equal
                    (repairTasks (
                        planTasks
                            (keep |> tunedBy (fun t -> { t with RampartFloor = 200_000 }))
                            noThreats
                    ))
                    [ "ram-1" ]
                    "raise the floor past it and the same rampart is hungry"

                Expect.isEmpty
                    (repairTasks (
                        planTasks
                            (bareRespawn
                             |> withLevel 2
                             |> withHits "ram-1" BuiltKind.Rampart 1 300_000
                             |> tunedBy (fun t -> { t with RampartFloor = 200_000 }))
                            noThreats
                    ))
                    "and under `Independent` no floor is read at all: the rampart decays away (#214)"
            }

            test "PickupThreshold is the pile a Pickup is worth walking for" {
                let pile = pileTaskColony 80 []

                Expect.isEmpty
                    (planTasks pile noThreats
                     |> List.filter (function
                         | Pickup _ -> true
                         | _ -> false))
                    "eighty is under the shipped hundred, so the pile is left to decay"

                Expect.equal
                    (planTasks
                        (pile |> tunedBy (fun t -> { t with PickupThreshold = 50 }))
                        noThreats
                     |> List.filter (function
                         | Pickup _ -> true
                         | _ -> false))
                    [ Pickup "pile-a" ]
                    "a threshold of fifty and the same pile is worth the walk"
            }

            test "ReachMargin is the tiles a weapon's range is widened by" {
                let melee = facingBody { X = 25; Y = 29 } [ Attack; Move ]

                Expect.isFalse
                    (Set.contains { X = 25; Y = 25 } (reachIn melee))
                    "range 1 plus the shipped two is three tiles, and four is clear"

                Expect.isTrue
                    (Set.contains
                        { X = 25; Y = 25 }
                        (reachIn (melee |> tunedBy (fun t -> { t with ReachMargin = 3 }))))
                    "one more tile of margin and the same tile is inside the Reach"
            }

            test "StandingCarryPerWork is the line a delivery stops being work at" {
                // Read through the supply floor (ADR 0050), which is the
                // rule that asks whether anything the colony holds can put
                // energy into an extension: a body over the line is a
                // [[standing body]] and may not, so the colony hires a
                // hauler in front of every row.
                let colony =
                    { bareRespawn with
                        Creeps = [ creepWith "b1" 0 50 [ Work; Work; Carry; Move; Move ] ]
                    }

                Expect.equal
                    (castRows (decide colony Map.empty Set.empty None).Intents)
                    [ "worker" ]
                    "one Carry per two Work is under the shipped four, so the body can refill and the floor is quiet"

                Expect.equal
                    (castRows
                        (decide
                            (colony |> tunedBy (fun t -> { t with StandingCarryPerWork = 1 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ "hauler" ]
                    "move the line under it and the same body is standing: nothing here can fill an extension"
            }

            test "PioneerCount is the crowd a mother lends a child" {
                let casts colony fleet =
                    spawnIntents
                        (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let nursery = asNursery switchHome

                Expect.isEmpty
                    (casts nursery (switchHomeFleet @ pioneers))
                    "three is the shipped addend, and thirteen plus three casts nothing"

                Expect.hasLength
                    (casts
                        (nursery |> tunedBy (fun t -> { t with PioneerCount = 4 }))
                        (switchHomeFleet @ pioneers))
                    1
                    "raise the crowd by one and the same fleet is a body short"
            }

            test "SafeModeDeadline is the claimer range the stock is spent at" {
                let claimer =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ]
                    }

                Expect.isEmpty
                    (activations (decide claimer Map.empty Set.empty None).Intents)
                    "range four is outside the shipped deadline of three: the towers get their window"

                Expect.equal
                    (activations
                        (decide
                            (claimer |> tunedBy (fun t -> { t with SafeModeDeadline = 4 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ "ctrl-1" ]
                    "move the deadline out one tile and the same claimer fires it"
            }

            test "StorageLevel is the level the Storage's tile is reserved from" {
                let colony = atLevel 5 (openRoom 6)

                Expect.hasLength
                    (sitesOfKind Storage (decide colony Map.empty Set.empty None).Intents)
                    1
                    "the shipped four is at or under RCL5, so the Storage's pick is held and placed"

                Expect.isEmpty
                    (sitesOfKind
                        Storage
                        (decide
                            (colony |> tunedBy (fun t -> { t with StorageLevel = 3 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "read at a level the engine allows none, the reservation is empty and nothing is placed"
            }

            test "HorizonLevel is the level the clustered kinds are sized at" {
                // Read at the horizon's own level, where the sizing is the
                // whole answer: the placement filter is wide open at RCL6, so
                // what the room asks for is what the reservation held.
                let colony = atLevel 6 (openRoom 6)

                let placed tuned =
                    let { Intents = intents } = decide tuned Map.empty Set.empty None

                    List.length (sitesOfKind Tower intents),
                    List.length (sitesOfKind Extension intents)

                let towers, extensions = placed colony

                Expect.equal towers 2 "the shipped horizon of six sizes two towers"
                Expect.equal extensions 40 "and forty extensions, which RCL6 unlocks in full"

                // The horizon left behind, one field moved (ADR 0055): the
                // same RCL6 room under the shipped-yesterday five sizes thirty
                // and plans none of the ten the engine unlocked. That is the
                // failure this constant exists to prevent, and it is why the
                // move lands before the room does.
                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLevel = 5 })))
                    (2, 30)
                    "a horizon of five sizes the RCL5 cluster, and an RCL6 room may place no more than it planned"

                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLevel = 2 })))
                    (0, 5)
                    "a horizon of two reserves an RCL2 room's cluster, and the room may place no more than it planned"
            }

            test "OutpostBuilders is the crowd the outpost may take" {
                // One number, two rations since #266: how many bodies may be
                // across the Seam at once, and how many of the outpost's sites
                // are worth crossing for — the head of the queue is exactly as
                // long as the crowd that could work it, so a trunk is paved
                // outward from the crossing instead of all at once.
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Creeps = [ for name in [ "w1"; "w2"; "w3" ] -> worker name 50 0 ]
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "w1", { X = 10; Y = 2 }
                                                "w2", { X = 10; Y = 3 }
                                                "w3", { X = 10; Y = 4 }
                                            ]
                                })
                    }

                let tally colony =
                    (decide colony Map.empty Set.empty None).Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.countBy id
                    |> List.sort

                Expect.equal
                    (tally crowd)
                    [ taskId (Build "site-out"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "two is the shipped budget, and the third worker falls to the Upgrade"

                Expect.equal
                    (tally (crowd |> tunedBy (fun t -> { t with OutpostBuilders = 1 })))
                    [ taskId (Build "site-out"), 1; taskId (Upgrade "ctrl-1"), 2 ]
                    "a budget of one and two of the three stay home"

                // The other half of the same number, pinned where the queue is
                // longer than it: four hand-laid road sites down one corridor,
                // and the budget says how many of them are feeding-tier at all.
                // The ones it names are the nearest the Seam, so what moves
                // between the two readings is which site, not only how many.
                let trunk =
                    crowd
                    |> withOutpostTrunk
                        [
                            "site-r1", BuiltKind.Road, { X = 10; Y = 47 }
                            "site-r2", BuiltKind.Road, { X = 10; Y = 46 }
                            "site-r3", BuiltKind.Road, { X = 10; Y = 45 }
                            "site-r4", BuiltKind.Road, { X = 10; Y = 44 }
                        ]

                Expect.equal
                    (tally trunk)
                    [
                        taskId (Build "site-out"), 1
                        taskId (Build "site-r1"), 1
                        taskId (Upgrade "ctrl-1"), 1
                    ]
                    "two lifts the container and the road beside the crossing; the other three roads wait"

                Expect.equal
                    (tally (trunk |> tunedBy (fun t -> { t with OutpostBuilders = 1 })))
                    [ taskId (Build "site-out"), 1; taskId (Upgrade "ctrl-1"), 2 ]
                    "one lifts the container alone — the switch is never queued behind a road"
            }

            test "BootstrapLevel is the line a stage is cut at, and the one place it is read" {
                // `Colony.stageOf`'s own pairwise (ADR 0052 decision 3):
                // the same three facts about a room, read under two lines.
                Expect.equal
                    (Colony.stageOf Tuning.defaults true true (Some 3))
                    (Some Independent)
                    "at the shipped three, an RCL3 colony has outgrown its mother"

                Expect.equal
                    (Colony.stageOf
                        { Tuning.defaults with
                            BootstrapLevel = 5
                        }
                        true
                        true
                        (Some 3))
                    (Some Bootstrapping)
                    "move the line to five and the same room is still being raised"
            }

            test "VisionGrace is how long a held Task outlives the vision that carried it" {
                // #151's knob, pinned on the field and not on the shipped
                // number: one colony, one dark room, one dark tick short of
                // a hundred, read under two graces. The boundary at the
                // shipped 150 is `OutpostTests`' own case; what this owns is
                // that the number is read at all.
                let dark =
                    { bareRespawn with
                        Time = 1000
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                        Sightings =
                            Map.ofList
                                [
                                    "",
                                    {
                                        Tick = 900
                                        Targets = Set.singleton "spawn-1"
                                    }
                                ]
                    }

                let held = taskId (Refill "spawn-1")
                let sticky = Map.ofList [ "w1", held ]

                let verdictsOf colony =
                    (decide colony sticky Set.empty None).Verdicts

                Expect.contains
                    (verdictsOf (dark |> tunedBy (fun t -> { t with VisionGrace = 100 })))
                    (Verdict.Kept("w1", held))
                    "a grace of a hundred covers a hundred dark ticks, and the holder waits for the vision"

                Expect.contains
                    (verdictsOf (dark |> tunedBy (fun t -> { t with VisionGrace = 99 })))
                    (Verdict.Released("w1", held, ReleaseReason.TaskGone))
                    "one shorter and the same darkness is a Task given up on"
            }
        ]

[<Tests>]
let quotasRecordTests =
    testList
        "the quotas record"
        [
            test "the cascade writes down its rows, and they sum to the target" {
                // Observability only (ADR 0009): the record the `observe.mjs
                // quotas` view prints. Six rows in cascade order — the guard
                // at the head of them since ADR 0056, behind only the supply
                // floor, which is a floor and not a row and so has no line
                // here; the worker row is what the target leaves after the
                // specialists, so the quotas sum to the target; the living
                // counts partition the fleet.
                let { Quotas = quotas } = decide bareRespawn Map.empty Set.empty None

                Expect.equal
                    (quotas.Rows |> List.map (fun r -> r.Row))
                    [ "guard"; "reserver"; "anchor"; "hauler"; "upgrader"; "worker" ]
                    "one row per casting row, in the cascade's order"

                Expect.equal
                    (quotas.Rows |> List.sumBy (fun r -> r.Quota))
                    quotas.Target
                    "the rows' quotas are the target"

                Expect.equal
                    (quotas.Rows |> List.sumBy (fun r -> r.Living))
                    quotas.Living
                    "the rows' living counts partition the fleet"
            }
        ]
