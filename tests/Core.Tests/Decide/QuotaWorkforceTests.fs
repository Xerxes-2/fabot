/// The workforce target and the hauler row it is summed from (ADR 0012,
/// ADR 0049).
module Fabot.Core.Tests.Decide.QuotaWorkforceTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

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

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (spawnIntents intents) "only the floor applies"
            }
        ]

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
                    decideOn
                        { quotaColony spawnX 5 1500 with
                            Creeps = [ anchor "a1" 0 50; worker "w1" 0 50 ]
                        }

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

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot

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
                            |> withCreepsAt [ "h1", { X = 12; Y = 10 } ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn snapshot

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
                            |> withCreepsAt [ "h1", { X = 17; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

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
                            |> withCreepsAt [ "w1", { X = 17; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

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
                        Spatial = haulRoom |> withCreepsAt [ "h1", { X = 17; Y = 10 } ]
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideOn snapshot

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
                            |> withCreepsAt [ "h1", { X = 12; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "can-src") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot

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
                    (spawnIntents (decideOn atTarget).Intents)
                    "the premise: the fleet already matches the target"

                Expect.equal
                    (spawnIntents (decideOn withOutpostSource).Intents)
                    (spawnIntents (decideOn atTarget).Intents)
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
                    (spawnIntents (decideOn atHome).Intents)
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
                    (spawnIntents (decideOn colliding).Intents)
                    (spawnIntents (decideOn atTarget).Intents)
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
                    (placementIntents (decideOn colony).Intents)
                    "the premise: this colony really does place a plan to move"

                Expect.equal
                    (placementIntents (decideOn joined).Intents)
                    (placementIntents (decideOn colony).Intents)
                    "the same room plans the same tiles: a source a room away is no source of its"
            }
        ]
