/// Withdraw capacity, the piles and tombstones a Pickup names, and a
/// source container that is full.
module Fabot.Core.Tests.Decide.PoolWithdrawTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.PoolFixtures

[<Tests>]
let withdrawCapacityTests =
    testList
        "withdraw capacity"
        [
            test "a container that fills one hauler takes one; the rest walk to the full one" {
                // The defect (#161): the matching key puts cost ahead of
                // `load` (ADR 0002), so without a capacity every empty
                // hauler picks the *nearest* stocked container whatever is
                // in it — three bodies onto 400 energy, two of them home
                // empty, while 1,800 stands unvisited seventeen tiles away.
                // The stock is the cap: `ceil(400 / 400)` is one seat. The
                // far store is 1,800 and not a full 2,000: a full source
                // container is lifted a rung of its own (its overflow is
                // going to the ground), and this test is about the cap.
                let { Assignments = split } = decideOn (crowdColony 400 1800 crowdOfThree)

                Expect.equal
                    (drawersOf split "can-near")
                    [ "h1" ]
                    "one hauler's worth of stock admits one hauler"

                Expect.equal
                    (drawersOf split "can-far")
                    [ "h2"; "h3" ]
                    "and the crowd it turns away walks to the store that can fill it"

                // The pairwise control: the same three creeps on the same
                // tiles, with nothing changed but the near store's stock.
                // Travel cost still says near for all three, and now the
                // capacity lets it — so the split above is the stock's
                // doing and not the geometry's.
                let { Assignments = whole } = decideOn (crowdColony 2000 2000 crowdOfThree)

                Expect.equal
                    (drawersOf whole "can-near")
                    [ "h1"; "h2"; "h3" ]
                    "stocked for five trips, the near container keeps the whole crowd"
            }

            test "the cap rounds up: one load exactly is one seat, one energy more is two" {
                // The `ceil` (#161), pinned at the boundary the arithmetic
                // turns on: 400 is exactly the cast hauler's load and admits
                // one body, and 401 — a fraction of a second trip — admits
                // the second, because the fraction a floor would drop is
                // energy nobody would be sent for.
                let seatsAt stock =
                    let { Assignments = assignments } =
                        decideOn (crowdColony stock 1800 (List.truncate 2 crowdOfThree))

                    drawersOf assignments "can-near"

                Expect.equal (seatsAt 400) [ "h1" ] "one whole load is one seat"

                Expect.equal
                    (seatsAt 401)
                    [ "h1"; "h2" ]
                    "one energy past it is two: the cap rounds up"

                Expect.equal
                    (seatsAt 800)
                    [ "h1"; "h2" ]
                    "and two whole loads are two, with no third body to prove it wider"
            }

            test "a hauler still walking holds its seat: the second is turned away" {
                // Counted at arrival like every other cap (ADR 0026): the
                // holder is fourteen steps out and has not touched the
                // store, and the candidate is standing on its doorstep. A
                // cap counting only the creeps already on the tile would let
                // the near one in and land both on 400 energy — which is the
                // defect with an extra tick in it.
                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        (crowdColony 400 2000 [ "h1", { X = 25; Y = 10 }; "h2", { X = 11; Y = 10 } ])
                        (Map.ofList [ "h1", taskId (Withdraw("can-near", Energy)) ])
                        (Set.singleton "h2")
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw("can-near", Energy))))
                    "the walking holder keeps the store it was already sent to"

                Expect.equal
                    (drawersOf assignments "can-far")
                    [ "h2" ]
                    "and the creep on the doorstep is sent to the far store instead"

                let rejections =
                    verdicts
                    |> List.tryPick (function
                        | Verdict.Scoring("h2", rows) ->
                            rows
                            |> List.filter (function
                                | Candidate.Rejected _ -> true
                                | Candidate.Scored _ -> false)
                            |> Some
                        | _ -> None)

                Expect.equal
                    rejections
                    (Some
                        [
                            Candidate.Rejected(
                                taskId (Withdraw("can-near", Energy)),
                                RejectReason.CapacityFull
                            )
                            Candidate.Rejected(taskId (Upgrade "ctrl-1"), RejectReason.Inapplicable)
                        ])
                    "the near store names the cap and no gate before it — not the body, not the price; the Upgrade it has no Work for is the pool's only other loss"
            }

            test "the Storage is not special-cased: the same formula, and at 130k no cap" {
                // ADR 0023's stock is one more store and gets one more
                // reading of the same rule (#161) — a Storage down to one
                // trip's worth admits one drawer, exactly as a container
                // does. What keeps that from starving the haul cycle is the
                // number and not an exemption: a real stock divides into
                // hundreds of trips, so the cap is there and is never the
                // thing that binds.
                let { Assignments = thin } = decideOn (stockCrowdColony 400)

                Expect.equal
                    (drawersOf thin "sto-c")
                    [ "h1" ]
                    "a stock holding one trip's worth admits one hauler"

                let { Assignments = full } = decideOn (stockCrowdColony 130000)

                Expect.equal
                    (drawersOf full "sto-c")
                    [ "h1"; "h2"; "h3" ]
                    "and a colony's real stock caps at 325 trips, which is no cap at all"
            }

            test "the upgrade buffer divides by the worker row that draws from it" {
                // Which row draws is a fact about the store (ADR 0019): no
                // body without a Work part may take the buffer, so its
                // drawers are the worker row and its 900 is two cast
                // workers' loads at this bank. Priced by the hauler the
                // colony would cast instead — 1,200 a trip — the same 900
                // reads `ceil(900 / 1200)` = one seat and sends the second
                // upgrader back to a rock while the energy it came to
                // spend stands beside it (#161).
                let { Assignments = split } = decideOn (bufferCrowdColony 900)

                Expect.equal
                    (drawersOf split "can-buf")
                    [ "w1"; "w2" ]
                    "two worker loads standing in the buffer admit two workers"

                // The same 900 in a store the haul cycle owns, judged for
                // the same three bodies: the divisor is the store's and
                // never the candidate's, so the ordinary container admits
                // one and takes the worker the buffer turned away.
                Expect.equal
                    (drawersOf split "can-far")
                    [ "w3" ]
                    "and an ordinary container's 900 is one hauler load, however the body that walks to it is built"

                // The pairwise control: nothing changed but the buffer's
                // stock, three loads instead of two.
                let { Assignments = whole } = decideOn (bufferCrowdColony 1350)

                Expect.equal
                    (drawersOf whole "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "three loads keep the whole crowd upgrading standing still, which is what a buffer is for"
            }

            test "the buffer's two rows are capped apart, each by its own load" {
                // #196, landed as ADR 0052 decision 6's per-[[body class]]
                // capacity. One store, two rows, two loads: the generalist
                // the colony casts at this bank carries 450 and the
                // [[standing body]] beside the buffer carries fifty, so a
                // 400-energy buffer is one trip for the first and eight for
                // the second. Divided by the generalist's load alone — the
                // one number the store used to answer — the row #187 hired
                // to *live* at that store took one seat between the three
                // of them and the rest walked to a rock they are the worst
                // body in the colony at digging.
                //
                // Pairwise on the class of the three bodies and nothing
                // else: the same tiles, the same 400, the same pool.
                let standingCrowd =
                    { bufferCrowdColony 400 with
                        Creeps =
                            [
                                for name, _ in bufferCrowd ->
                                    creepWith name 0 50 (bodyFor upgraderPattern 1800)
                            ]
                    }

                let { Assignments = standing } = decideOn standingCrowd

                Expect.equal
                    (drawersOf standing "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "fifty energy a trip divides 400 into eight seats, so the row that lives there all drinks"

                let { Assignments = generalists } = decideOn (bufferCrowdColony 400)

                Expect.equal
                    (drawersOf generalists "can-buf")
                    [ "w1" ]
                    "and the generalists' own share is unchanged: one load standing is one of them"
            }
        ]

[<Tests>]
let pickupTaskTests =
    testList
        "the pile and the tombstone"
        [
            test "a pile worth a whole trip outbids the container underfoot; a smaller one does not" {
                // The live shape's other half (#242, user 2026-09-07):
                // "worker 和 hauler 在不满的 container 和地上的能量中会优先
                // 选择前者". Nothing but travel cost separates a pile from a
                // half-full container once they are apart, and the container
                // is the one the body is standing on, so the ground kept its
                // energy until it decayed. A pile holding half a [[hauler
                // unit]]'s load or more is a trip of its own, and the copy
                // that is going away is the one to take — so it steps up the
                // rung the pile on a drawable tile already had.
                let matched colony =
                    let { Verdicts = verdicts } = decideOn colony

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched (pileDownTheLane 800 600))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "half the row's load five tiles off beats eight hundred at its feet"

                // The pairwise control: the same colony, the same distance,
                // one energy under the line. A hundred energy is not a trip,
                // and a [[priority]] is a colony-wide scalar — lifting every
                // pile is what would send this body five tiles for a
                // mouthful and, in the live room, forty tiles past the 1,500
                // under its feet.
                Expect.equal
                    (matched (pileDownTheLane 800 599))
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.TravelCost))
                    "one under half a load: rank ties and the near store wins on price"

                Expect.equal
                    (matched (pileDownTheLane 800 100))
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.TravelCost))
                    "and a pile at the threshold is still the distance's to decide"

                // Pairwise on the container's stock alone: a **full** source
                // container is two rungs up (#216 R5) against the pile's one,
                // because its income is going away too — the engine drops a
                // garrison's overflow onto its tile only once it is full —
                // and the pickup reflex takes what lies there for free while
                // the body draws.
                Expect.equal
                    (matched (pileDownTheLane Engine.containerCapacity 600))
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.Rank))
                    "two thousand full outranks the whole trip on the ground"
            }

            test "the whole trip's rung is a rung over the whole feeding tier" {
                // What the rung reaches, pinned pairwise because a rank is a
                // colony-wide scalar and not a pairing (#242 review): the
                // Pickup steps up against *every* Feeding Task and not the
                // container Withdraws the ticket named, so the consequence is
                // read here as a decision rather than met later as a surprise.
                // Each colony below holds exactly two Tasks, and the rival is
                // the one standing under the body's feet.
                let matched colony =
                    let { Verdicts = verdicts } = decideOn colony

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                // The delivery half of the cycle: a half-loaded hauler one
                // step from a spawn with 300 free walks nineteen tiles for
                // the pile instead, and no distance saves the spawn because
                // rank is settled before a price is asked.
                Expect.equal
                    (matched (pileAgainstAHungrySpawn 1800 600))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "half a load on the ground outranks the hungry spawn at the body's feet"

                Expect.equal
                    (matched (pileAgainstAHungrySpawn 1800 599))
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.TravelCost))
                    "one energy under the line the spawn is the near Task again"

                // And the other copy that is going away: a tombstone is a
                // Withdraw like any other and only a *full* container's stock
                // carries a rung, so the pile outranks 1,500 in a store that
                // ends outright.
                Expect.equal
                    (matched (pileAgainstATombstone 600))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "and it outranks a tombstone's 1,500 one tile away"

                Expect.equal
                    (matched (pileAgainstATombstone 599))
                    (Some(taskId (Withdraw("tomb-a", Energy)), MatchFactor.TravelCost))
                    "which under the line is travel cost's again"
            }

            test
                "under a bank of 450 the line is the pooling threshold and every pile takes the rung" {
                // The line's own regime (#242 review). `haulerLoad` is
                // `100 * (bank / 150)`, so half a load is a hundred at RCL1's
                // bank of 300 — `Tuning.PickupThreshold` itself — and the
                // smallest pile the pool will hold is already a whole trip
                // for the row that bank casts. The distance-only rung the
                // rule is written around therefore begins at RCL2, and a
                // bootstrapping colony lifts every pile it pools. Pinned so
                // the regime is a fact of the code and not of a fixture's
                // bank.
                let matched colony =
                    let { Verdicts = verdicts } = decideOn colony

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched (pileAgainstAHungrySpawn 300 100))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "at RCL1 a threshold-sized pile nineteen tiles off takes the rung"

                Expect.equal
                    (matched (pileAgainstAHungrySpawn 550 100))
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.TravelCost))
                    "and at RCL2, where the cast has outgrown twice the threshold, it does not"
            }

            test "a pile on a container's own tile is taken before the container" {
                // The live shape (user, 2026-09-07): a hauler standing at a
                // full container ignored the 1,859 energy lying on it. The
                // two Tasks share the feeding tier, the tile and therefore
                // the travel cost, so pool order decided — and the pool
                // order had the container first. What separates them is
                // decay: the pile loses `ceil(amount / 1000)` a tick and the
                // container loses nothing, so the copy that is going away is
                // the one to take (ADR 0052 decision 6, the [[priority]]
                // ladder's own rung of slack).
                let { Assignments = together } = decideOn (sameTilePileColony { X = 10; Y = 10 })

                Expect.equal
                    (Map.tryFind "h1" together)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "one tile, two stores: the decaying one first"

                // The pairwise control: the same hauler, the same container,
                // the same pile, ten tiles down the lane. The Withdraw is on
                // its own tier again — the step is a claim about *this*
                // store and never about piles in general — and travel cost
                // says what it always said. A hundred and fifty is an eighth
                // of the row's load here, so the other lift a pile can carry
                // (#242) is not what this reads either.
                let { Assignments = apart } = decideOn (sameTilePileColony { X = 20; Y = 10 })

                Expect.equal
                    (Map.tryFind "h1" apart)
                    (Some(taskId (Withdraw("can-a", Energy))))
                    "a pile ten tiles off moves nothing: the container underfoot is still the flow"

                // And a *full* container outranks the pile on its own tile
                // (live, W12S28 2026-09-07): the garrison is overflowing, the
                // 2,000 is the flow, and the pickup reflex takes the pile
                // off the same tile for free while the body draws. Pairwise
                // on the stock alone.
                let full =
                    let colony = sameTilePileColony { X = 10; Y = 10 }

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores =
                                    Map.add "can-a" Engine.containerCapacity colony.Spatial.Stores
                            }
                    }

                let { Assignments = brimming } = decideOn full

                Expect.equal
                    (Map.tryFind "h1" brimming)
                    (Some(taskId (Withdraw("can-a", Energy))))
                    "full, the container is drawn first and the reflex takes the pile beside it"
            }

            test "the piled container keeps its place against every other store" {
                // Which of the pair carries the rung is not a matter of
                // taste (#216 R5 review). A [[priority]] is a scalar the
                // whole tier is ordered by and travel cost never overturns
                // it (ADR 0002), so stepping the *Withdraw* down put that
                // container behind every other store in the colony at any
                // distance — and the engine drops an [[anchor]]'s overflow
                // onto a container's tile only once the container is
                // **full**, so the demotion switched on exactly when the
                // store most needed emptying. Two haulers, a piled near
                // container and an unpiled far one: the first takes the
                // decaying copy, and the second must still take the four
                // hundred at its feet rather than walk twenty tiles past
                // it.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Creeps = [ hauler "h1" 0 100; hauler "h2" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores =
                                    Map.ofList [ "can-near", 400; "can-far", 400; "pile-a", 100 ]
                            }
                            |> withTargets
                                [
                                    "can-near", { X = 10; Y = 10 }, Structure BuiltKind.Container
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                    "pile-a", { X = 10; Y = 10 }, (Dropped Energy)
                                ]
                            |> withCreepsAt [ "h1", { X = 10; Y = 11 }; "h2", { X = 14; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "the pile's own hauler still takes the decaying copy first"

                // The pile's capacity is its amount over one load — one
                // body — so the second hauler is over capacity there and
                // has the two containers to choose between. Only travel
                // cost may decide that, which is the whole of the fix.
                Expect.equal
                    (Map.tryFind "h2" assignments)
                    (Some(taskId (Withdraw("can-near", Energy))))
                    "and the rest of the row is not sent past the full store beside it"
            }

            test "a pile past the threshold hires a hauler ten tiles off; one under it hires nobody" {
                // The live gap's second half (#167): 193 energy of death
                // drop at W13S28 36,21 with nobody near enough for the
                // reflex ever to reach it. A pile at or over the threshold
                // is a Task and gets walked to.
                let walk = decideOn (pileTaskColony 150 [ "h1", { X = 20; Y = 10 } ])

                Expect.equal
                    (Map.tryFind "h1" walk.Assignments)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "150 on the ground is worth ten tiles of walking"

                Expect.isEmpty
                    (pickups walk.Intents)
                    "and out of reach it is walking, not picking: the act waits for arrival"

                Expect.isNonEmpty
                    (moveIntentsFor "h1" walk.Intents)
                    "what a Task buys over the reflex is exactly this step"

                // The pairwise control: the same creep on the same tile
                // with the same everything, and 80 energy on the ground.
                let small = decideOn (pileTaskColony 80 [ "h1", { X = 20; Y = 10 } ])

                Expect.equal
                    (Map.tryFind "h1" small.Assignments)
                    None
                    "under the threshold the pile is the reflex's business and nobody walks"
            }

            test "the threshold is inclusive: a hundred exactly is worth the trip" {
                // Where the tunable turns, pinned on both sides of it: two
                // CARRY parts' worth is the smallest load that pays for a
                // walk made for the pile alone.
                let assignmentAt amount =
                    let { Assignments = assignments } =
                        decideOn (pileTaskColony amount [ "h1", { X = 20; Y = 10 } ])

                    Map.tryFind "h1" assignments

                Expect.equal
                    (assignmentAt 100)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "at the line, pooled"

                Expect.equal (assignmentAt 99) None "one energy short of it, not"
            }

            test "the pile that arrives is picked up once, and the bubble says so" {
                // The Task's own action Intent, at range 1 where the Atlas
                // permits it. The reflex asks for the same act on this
                // tick — its rule is the same range and the same free
                // capacity — so the count is the assertion and not the
                // membership: an arriving picker satisfies both producers,
                // and one creep's one pickup spelt twice would over-report
                // the CPU line's accepted-intent column tick after tick
                // (#167). Two *adjacent creeps* both reaching for one pile
                // stay two asks; this is one creep asking twice.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn (pileTaskColony 150 [ "h1", { X = 10; Y = 11 } ])

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "standing on its doorstep it still holds the Task"

                Expect.equal
                    (pickups intents)
                    [ "h1", "pile-a" ]
                    "it asks the engine for it, exactly once between the Task and the reflex"

                Expect.contains
                    intents
                    (SayCreep("h1", "🧲"))
                    "one glyph per Task, and this Task has its own"
            }

            test
                "the creep beside a hired picker still asks: the pair is deduplicated, not the pile" {
                // The other side of the count above (#167): what the
                // deduplication drops is one creep's own Intent spelt
                // twice, and nothing else. Two haulers stand at one pile
                // of exactly a hundred, so its capacity is one body: h1 is
                // hired and h2 is not, and h2's reflex pickup is energy
                // the colony recovers for free. A filter written over the
                // pile rather than over the (creep, pile) pair would drop
                // it — which is why the assertion is both names and not a
                // count.
                let { Intents = intents } =
                    decideOn (
                        pileTaskColony 100 [ "h1", { X = 10; Y = 11 }; "h2", { X = 11; Y = 10 } ]
                    )

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "h1", "pile-a"; "h2", "pile-a" ]
                    "one ask apiece: the hired picker's own, and the bystander's reflex"
            }

            test "a pile decaying under the threshold releases the hauler still walking to it" {
                // The accepted loss, pinned so it stays a decision
                // (`Tuning.PickupThreshold`, #167). The threshold gates
                // persistence as well as entry, because the pool is
                // rebuilt creep-blind every tick: a pile at 100 holds its
                // holder, and the same pile one energy lighter — a
                // hundredth of the decay a pile spends on its own, or the
                // first of two hired haulers arriving — is gone, and the
                // walk already spent bought nothing.
                let held = Map.ofList [ "h1", taskId (Pickup("pile-a", Energy)) ]

                let standing = decideFrom held (pileTaskColony 100 [ "h1", { X = 20; Y = 10 } ])

                Expect.contains
                    standing.Verdicts
                    (Verdict.Kept("h1", taskId (Pickup("pile-a", Energy))))
                    "at the line the walk stands"

                let decayed = decideFrom held (pileTaskColony 99 [ "h1", { X = 20; Y = 10 } ])

                Expect.contains
                    decayed.Verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Pickup("pile-a", Energy)),
                        ReleaseReason.TaskGone
                    ))
                    "one energy under it, ten tiles from home, and the trip is over"
            }

            test "the pile's amount is its capacity: 150 admits two of the three haulers" {
                // The Withdraw rule over a pile (#161 read by #167):
                // `ceil(150 / 100)` is two bodies, and travel cost cannot
                // thin the crowd because all three stand one step from the
                // pile's Work Area.
                let { Assignments = split } = decideOn (pileTaskColony 150 crowdOfThree)

                Expect.equal
                    (pickersOf split "pile-a")
                    [ "h1"; "h2" ]
                    "one and a half loads on the ground hire two haulers"

                // The pairwise control: the same three creeps on the same
                // tiles, nothing changed but the amount.
                let { Assignments = whole } = decideOn (pileTaskColony 300 crowdOfThree)

                Expect.equal
                    (pickersOf whole "pile-a")
                    [ "h1"; "h2"; "h3" ]
                    "three loads take the whole crowd"
            }

            test "a Work-heavy body never picks a pile up (ADR 0016)" {
                // The gate that keeps an Anchor at its rock, read over the
                // ground as well as over a container: a heavy body's
                // intake is digging, and a pile is not a dig. Pairwise on
                // the body alone — the same parts, one Move more.
                let bodied body =
                    let colony = pileTaskColony 150 [ "a1", { X = 12; Y = 10 } ]

                    { colony with
                        Creeps = [ creepWith "a1" 0 50 body ]
                    }

                let { Assignments = heavy } = decideOn (bodied [ Work; Work; Carry; Move ])

                Expect.equal (Map.tryFind "a1" heavy) None "more Work than Move: no Pickup"

                let { Assignments = balanced } = decideOn (bodied [ Work; Work; Carry; Move; Move ])

                Expect.equal
                    (Map.tryFind "a1" balanced)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "the same body at Work <= Move picks it up"
            }

            test "a tombstone is a store: its 408 is withdrawn, and an empty one pools nothing" {
                // The live gap's first half (#167): 408 energy standing in
                // a tombstone in the home room while the colony dug. The
                // Intent is the container's own — the engine's `withdraw`
                // is one method over every store.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn (tombColony 408 [ "h1", { X = 11; Y = 10 } ])

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw("tomb-1", Energy))))
                    "a store with a clock on it is drawn like any other"

                Expect.contains
                    intents
                    (WithdrawFromStore("h1", "tomb-1", Energy, None))
                    "and the act is withdraw, never pickup"

                // The pairwise control: the same tombstone on the same
                // tile, drawn dry.
                let { Assignments = spent } = decideOn (tombColony 0 [ "h1", { X = 11; Y = 10 } ])

                Expect.equal (Map.tryFind "h1" spent) None "an empty store is no Task"
            }

            test "a tombstone keeps no construction site off its tile" {
                // Layout determinism (ADR 0011), the rule the piles already
                // had (#167): a tombstone stands wherever a creep happened
                // to die, and a plan that moved with it would be a function
                // of that accident.
                let bare = atLevel 2 (openRoom 3)

                let littered =
                    atLevel
                        2
                        (openRoom 3 |> withTargets [ "tomb-1", { X = 24; Y = 24 }, Tombstone ])

                let placedWith = decideOn littered
                let placedWithout = decideOn bare

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see tombstones"
            }

            test "a pile ties a container: one tier, and the tie goes to the pile" {
                // The tier (#167): a pile is the haul cycle's own energy
                // lying where it fell, so it feeds on the containers' tier
                // and the choice between the two is travel cost's. Equal
                // cost is the way to read that off one match — a rank
                // either way would have decided it before the price was
                // asked, and the factor says which happened.
                //
                // Which way an exact tie falls is the pool's order, and
                // since #242 the piles stand in it before the Withdraws
                // (user: "worker 和 hauler 在不满的 container 和地上的能量
                // 中会优先选择前者"). A container keeps what it holds and a
                // pile loses `ceil(amount / 1000)` a tick, so where nothing
                // else separates them the decaying copy is the one to take.
                // The bank is the mother's, so 150 on the ground is well
                // under half the row's 1,200 load and carries no rung of
                // its own: what this reads is pool order and nothing else.
                let colony =
                    { bareRespawn with
                        Bank = bank 1800 1800
                        Sources = []
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 150; "can-far", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 10; Y = 10 }, (Dropped Energy)
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                ]
                            |> withCreepsAt [ "h1", { X = 20; Y = 10 } ]
                    }

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Pickup("pile-a", Energy)),
                            MatchFactor.PoolOrder
                        )
                    ]
                    "ten tiles either way: pool order broke the tie, not rank"
            }

            test "a pile outranks the stock underfoot" {
                // The other half of the tier, and the one that has a rank
                // in it (ADR 0023): the Storage is drawn a tier below the
                // flow, so a pile sixteen tiles away beats a stock the
                // creep is standing beside. A pile decays at a thousandth
                // a tick and a stock does not, which is the reason the
                // ordering is right as well as inherited.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 300; "sto-c", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 30; Y = 10 }, (Dropped Energy)
                                    "sto-c", { X = 13; Y = 10 }, Structure BuiltKind.Storage
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 13; Y = 10 }
                                    CreepPositions = Map.ofList [ "h1", { X = 14; Y = 10 } ]
                                })
                    }

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Pickup("pile-a", Energy)), MatchFactor.Rank) ]
                    "the feeding tier beats the stock draw whatever the distance"
            }

            test "an outpost's pile pools by the rule the home room's does" {
                // The declared outpost is a room of the projection like any
                // other (ADR 0041, ADR 0042): the pool is read off the kind
                // census and the amount, neither of which knows a border.
                // The home pile keeps its own coordinate and no amount, so
                // it stays the reflex's and proves the pairing is not
                // crossing (#166).
                let colony =
                    pileColony [ hauler "h-out" 0 100 ] []
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "h-out", { X = 12; Y = 10 } ]

                let snapshot =
                    { colony with
                        Bank = bank 150 150
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.ofList [ "pile-out", 150 ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn snapshot

                Expect.equal
                    (pickersOf assignments "pile-out")
                    [ "h-out" ]
                    "the outpost's pile hires the hauler standing in the outpost"

                Expect.contains
                    intents
                    (SayCreep("h-out", "🧲"))
                    "and it walks under the Pickup glyph"

                Expect.isEmpty
                    (pickups intents)
                    "two tiles out: no reflex, and no action Intent until it arrives"
            }
        ]

[<Tests>]
let fullContainerTests =
    testList
        "a full source container"
        [
            test "a full source container is drawn before a half-full one nearer to hand" {
                // One lane, two posted sources: can-a three tiles from the
                // hauler at 1,000, can-b twelve tiles away at 2,000 (full,
                // so its garrison's overflow is going to the ground). The
                // full one wins by rank; pairwise on can-b's stock alone,
                // at 1,000 the near one wins by travel cost, which is the
                // nearest-first dispatch that let W13S28's north container
                // overflow for hours (#198).
                let lane stockB =
                    let room =
                        { spatial
                              []
                              [
                                  for x in 9..28 ->
                                      { X = x; Y = 10 }, (if x = 10 || x = 27 then Wall else Plain)
                              ] with
                            Stores = Map.ofList [ "can-a", 1000; "can-b", stockB ]
                        }
                        |> withTargets
                            [
                                "src-a", { X = 10; Y = 10 }, Source
                                "can-a", { X = 11; Y = 10 }, Structure BuiltKind.Container
                                "src-b", { X = 27; Y = 10 }, Source
                                "can-b", { X = 26; Y = 10 }, Structure BuiltKind.Container
                            ]
                        |> withCreepsAt [ "h", { X = 14; Y = 10 } ]

                    { bareRespawn with
                        Sources = [ source "src-a"; source "src-b" ]
                        Refillables = []
                        Controller = None
                        Creeps = [ creepWith "h" 0 200 [ Carry; Carry; Carry; Carry; Move; Move ] ]
                        Spatial = room
                    }

                let matched stockB =
                    let { Verdicts = verdicts } = decideOn (lane stockB)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched 2000)
                    (Some(taskId (Withdraw("can-b", Energy)), MatchFactor.Rank))
                    "the full container outranks the near one"

                Expect.equal
                    (matched 1000)
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.TravelCost))
                    "both half-full: the near one, by travel cost"
            }
        ]

[<Tests>]
let thoriumLegTests =
    testList
        "the Thorium leg's pool"
        [
            test "the mineral container is a Withdraw and the Storage a Refill, both in Thorium" {
                // ADR 0057 decision 3: the mine-to-[[storage]] leg is the
                // existing pair with a resource on it, so what the pool gains is
                // two entries and not two Task kinds. The intake is the
                // container the store-less [[miner]] drops into; the sink is the
                // Storage, the one store nothing can stand on and so the one the
                // contact penalty never reaches.
                let tasks = planTasksOn mineHaulColony noThreats

                Expect.contains
                    tasks
                    (Withdraw("can-min", Thorium))
                    "the mineral container is drawn in Thorium"

                Expect.contains tasks (Refill("sto-1", Thorium)) "and the Storage takes the load"

                // The id carries the resource and the energy spelling is
                // untouched, which is what keeps a standing assignment standing
                // across the deploy that lands this.
                Expect.equal
                    (taskId (Withdraw("can-min", Thorium)))
                    "withdraw:can-min:Thorium"
                    "the Thorium draw is its own identity at that store"
            }

            test "an empty mineral container is drawn by nobody, and the sink stands anyway" {
                // Pairwise, one number apart. The Withdraw follows the stock the
                // way every other store's does; the Refill follows the *ground*,
                // because the Planner is creep-blind (ADR 0013) and what it can
                // see is that this colony has a mine at all — a hauler walking
                // home with a load must still have somewhere to put it on the
                // tick the container it drew from reads zero.
                let tasks = planTasksOn (mineHaulColony |> withMineStock 0) noThreats

                Expect.isFalse
                    (List.contains (Withdraw("can-min", Thorium)) tasks)
                    "nothing in the container is nothing to come for"

                Expect.contains
                    tasks
                    (Refill("sto-1", Thorium))
                    "and the sink is the mine's and not the load's"
            }

            test "a colony with no mineral container draws nothing, and still has a sink" {
                // The intake is the mine's and the sink is the **Storage's**
                // (#262). The draw is read off `ourDeposits` and the container
                // standing on the deposit's Seat (#261), so a deposit whose
                // container is not up yet is a mine with no store to come to.
                // The Refill is read off the Storage alone: a body already
                // holding the ore is applicable to that Task and to nothing else
                // in the colony, so gating the sink on the intake's own ground
                // left a hauler mid-haul with no applicable Task at all for the
                // whole of the window the Layout takes to re-place a destroyed
                // container — and the row's census, counting it living, cast no
                // replacement.
                let tasks = planTasksOn (mineHaulColony |> withoutMineContainer) noThreats

                Expect.isFalse
                    (List.contains (Withdraw("can-min", Thorium)) tasks)
                    "no mineral container, nothing to draw"

                Expect.contains
                    tasks
                    (Refill("sto-1", Thorium))
                    "and the Storage takes a load whatever the ground behind it has become"
            }

            test "the Thorium pair ranks at the Storage's tier, one resource apart" {
                // ADR 0057 decision 3 reading ADR 0023. Pairwise on the
                // **resource alone**: the same container, holding 600 of each,
                // yields a Feeding-tier draw in energy and a StockDraw one in
                // Thorium — so what moves the rank is the resource and not the
                // store's kind, which is a container either way. Read on the
                // container's own tier the mine would be Feeding work, and an
                // empty hauler beside it would take the season's ore ahead of
                // the energy the spawn is waiting on.
                let colony =
                    { mineHaulColony with
                        Spatial =
                            { mineHaulColony.Spatial with
                                Stores = Map.add "can-min" 600 mineHaulColony.Spatial.Stores
                            }
                    }

                let rankOf task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankOf (Withdraw("can-min", Energy)))
                    (Some(priorityOfTier Feeding))
                    "the premise: energy in a container is Feeding-tier intake"

                Expect.equal
                    (rankOf (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw))
                    "and the Thorium beside it is drawn on the Storage's own tier"

                Expect.equal
                    (rankOf (Refill("sto-1", Thorium)))
                    (Some(priorityOfTier Stock))
                    "the sink is the Storage's deepest tier, below every energy sink"
            }

            test "past the contact cliff the mine's draw takes the full container's two rungs" {
                // #306, amending ADR 0057 decision 3. Decision 3 ranked the
                // draw at `StockDraw` so that the season's ore never went ahead
                // of the energy the spawn is waiting on — and the **Storage's
                // own energy Withdraw sits on that same tier** (ADR 0023), so a
                // rungless mine loses the travel-cost tie to the bank in the
                // middle of the room every tick a colony has energy banked.
                // Live at t402,520 that is W13S28: 486k of energy, no Thorium,
                // and a container standing at its 2,000 cap.
                //
                // The rung the energy tier already has for exactly this — a full
                // [[container]] whose income is going away — read down the ore's
                // column, and fired at the **contact cliff** rather than at the
                // cap, because at a thousand the tile has already turned
                // `p = 3` and the [[miner]] over it is burning a fourth tick of
                // life a tick. Pairwise on the stock alone, one unit either side
                // of the line.
                // The bank, and a mouth for it: the Storage's own Withdraw is
                // pooled only where the colony has somewhere to put the energy
                // (ADR 0023), which is the shape of every colony this ticket is
                // about — W13S28 has a whole refill cluster.
                let banked =
                    { mineHaulColony with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Spatial =
                            { mineHaulColony.Spatial with
                                Stores = Map.add "sto-1" 485_916 mineHaulColony.Spatial.Stores
                            }
                    }

                let rankIn colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankIn (banked |> withMineStock 999) (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw))
                    "the premise: under the cliff the draw is decision 3's rungless intake"

                Expect.equal
                    (rankIn
                        (banked |> withMineStock Tuning.defaults.MineContactCliff)
                        (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung TwoRungsUp))
                    "at the cliff it takes the full energy container's own two rungs"

                Expect.equal
                    (rankIn
                        (banked |> withMineStock Tuning.defaults.MineContactCliff)
                        (Withdraw("sto-1", Energy)))
                    (Some(priorityOfTier StockDraw))
                    "and what the lift steps over is the bank's own draw, rungless on that tier"

                // The line the lift does **not** cross, which is the one
                // decision 3 actually drew: every energy Task the spawn is
                // waiting on is Feeding-tier, a whole tier shallower, and a rung
                // never leaves its tier.
                Expect.isGreaterThan
                    (priorityOfTier StockDraw + rankOfRung TwoRungsUp)
                    (priorityOfTier Feeding)
                    "the lifted draw is still deeper than the shallowest energy flow"
            }

            test "the Thorium draw is capped by its own column and not by the energy one" {
                // #161's cap read down ADR 0057 decision 3's second column: the
                // store answers the number its holding of *that* resource
                // divides into loads. The row's cast at this bank is
                // `[4 Carry; 2 Move]` — 200 — so 600 Thorium is three seats and
                // the energy the same container holds none of is no seats at
                // all. Pairwise on the stock alone.
                let seatsAt units =
                    poolOn (mineHaulColony |> withMineStock units)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Withdraw("can-min", Thorium) then
                            Capacity.capOf CapScope.Everyone pooled.Capacity
                        else
                            None)

                Expect.equal
                    (partCountIn (bodyFor haulerPattern mineHaulColony.Bank.Capacity) Carry
                     * Engine.carryPartCapacity)
                    200
                    "the premise: the divisor is the hauler row's own cast at this bank"

                Expect.equal (seatsAt 600) (Some 3) "six hundred of Thorium is three loads"
                Expect.equal (seatsAt 200) (Some 1) "one load is one seat"
            }

            test "the acts the leg emits name the resource, and the Withdraw names no amount" {
                // The Intents behind the pair (ADR 0057 decision 3): the
                // engine's `withdraw` and `transfer` have taken a resource
                // argument all along, and what this ticket changed is that the
                // colony says which rather than passing energy implicitly. The
                // **amount** is `None` — take as much as the body has room for,
                // which is what every Withdraw here has always meant; the one
                // place a number is ever named is the delivery's 999-unit load,
                // and that is decision 4's.
                let intentsFor load tile =
                    let colony =
                        { mineHaulColony with
                            Creeps = [ load ]
                            Spatial = mineHaulColony.Spatial |> withCreepsAt [ "h1", tile ]
                        }

                    (decideOn colony).Intents

                Expect.contains
                    (intentsFor (hauler "h1" 0 200) { X = 12; Y = 10 })
                    (WithdrawFromStore("h1", "can-min", Thorium, None))
                    "the draw names the mineral container and the season's resource"

                Expect.contains
                    (intentsFor (hauler "h1" 0 200 |> carrying 150) { X = 13; Y = 10 })
                    (TransferEnergyToStructure("h1", "sto-1", Thorium))
                    "and the pour names the Storage and the same resource back"
            }
        ]

/// `Fixtures.withMinePile`'s pile in **energy** rather than in the season's ore,
/// one fact apart: the same tile, the same amount, filed under the energy column
/// the kind now names. The pairwise premise for the case about where a Thorium
/// pile ranks, and private to it — the shared tier is for what a second *domain*
/// reads, and the one question this answers is a pool question.
let private withMineEnergyPile units (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Stores = Map.add "pile-min" units colony.Spatial.Stores
            }
            |> withTargets [ "pile-min", minePost, Dropped Energy ]
    }

[<Tests>]
let thoriumPileTests =
    testList
        "the Thorium pile's pool"
        [
            test "the ore on the ground is a Pickup, and the energy pile's id is untouched" {
                // #311: a mineral container caps at 2,000 and the [[miner]]
                // stands on it, so every tick the haul lags the next dig lands
                // on the floor of that same tile as a dropped pile — 630 on
                // W12S28's mine tile and ~300 on W13S28's at t401,850, decaying
                // at `ceil(amount / 1000)` a tick, with no Task in the colony
                // that could name one. Pairwise on the ground alone: the same
                // colony without the pile pools no Pickup at all.
                let piled = planTasksOn (mineHaulColony |> withMinePile 630) noThreats

                Expect.contains
                    piled
                    (Pickup("pile-min", Thorium))
                    "the pile on the mine post is the container's own intake off the floor"

                Expect.isFalse
                    (planTasksOn mineHaulColony noThreats
                     |> List.exists (function
                         | Pickup _ -> true
                         | _ -> false))
                    "the premise: no pile, no Pickup"

                // The id carries the resource and the energy spelling is frozen
                // where #167 left it, a Task id being a Memory key.
                Expect.equal
                    (taskId (Pickup("pile-min", Thorium)))
                    "pickup:pile-min:Thorium"
                    "the Thorium pile is its own identity on that tile"

                Expect.equal
                    (taskId (Pickup("pile-min", Energy)))
                    "pickup:pile-min"
                    "and an energy pile's id has not moved a byte"
            }

            test
                "a Thorium pile is drawn on the Storage's tier, one rung up; the energy pile is flow" {
                // Where the pile ranks, pairwise on the **resource alone**: the
                // same tile, the same 630, the same body. ADR 0057 decision 3
                // put the Thorium *container* at `StockDraw` so that an empty
                // hauler beside the mine never takes the season's ore ahead of
                // the energy the spawn is waiting on, and a pile is that
                // container's next dig lying on the floor — one intake of one
                // resource, so ranking the two apart by *tier* would be the
                // colony saying that where the ore sits changes what it is
                // worth.
                //
                // The **rung** it takes inside that tier is #306's, and it is
                // neither of the energy pile's two clauses: it inherits no
                // same-tile clause (`drawableTiles` holds Feeding-tier
                // Withdraws alone) and no worth-a-trip line (which this pile
                // would fail at a hundred units). It is one sentence about the
                // resource — ore on the floor bleeds `ceil(amount / 1000)` a
                // tick, nothing else on this tier bleeds at all, and there is
                // no second copy of season score. Rungless it tied the
                // Storage's own energy draw and lost every travel-cost tie to
                // it, which is #306's starvation read from the floor's end.
                let rankIn colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankIn (mineHaulColony |> withMineEnergyPile 630) (Pickup("pile-min", Energy)))
                    (Some(priorityOfTier Feeding + rankOfRung OneRungUp))
                    "the premise: an energy pile feeds the economy, and 630 is a trip of its own"

                Expect.equal
                    (rankIn (mineHaulColony |> withMinePile 630) (Pickup("pile-min", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung OneRungUp))
                    "and the season's ore is drawn on the Storage's tier, one rung over the bank"

                // The rung is **unconditional**, and this is the case that says
                // so rather than a word in a comment: neither of the energy
                // pile's two clauses would grant it here. There is no mineral
                // container at all — so nothing drawable lies under the pile,
                // and the ore is ours by the **room**, which is also how a
                // hauler dying mid-route leaves one on a road tile — and a
                // hundred units is well under half a [[hauler unit]]'s load at
                // the 2,300 bank a colony standing an extractor has, which is
                // the line the energy column refuses the rung at.
                let atMineBank (colony: ColonyView) = { colony with Bank = bank 2300 2300 }

                Expect.equal
                    (rankIn
                        (mineHaulColony |> atMineBank |> withoutMineContainer |> withMinePile 100)
                        (Pickup("pile-min", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung OneRungUp))
                    "ore on the floor takes the rung with no store under it and no trip's worth in it"
            }

            test "the full container is drawn before the pile it overflowed onto" {
                // The order inside the mine's own tile, and it is #242's lesson
                // read down the ore's column: with the pile above the full
                // container the haulers chased the small copy all day and never
                // drew the store beside it, so every pickup bred the next pile.
                // Two rungs against one says the same thing here — draining the
                // container is what *stops* the floor filling, and the pile is
                // a finite remainder that the next body takes.
                //
                // Pairwise on the container's stock alone: the same pile, the
                // same body on the same tile, the container either side of the
                // contact cliff. Under it the pair no longer ties at all — the
                // pile's own rung decides, and the copy that is going away is
                // taken first.
                let matchIn colony =
                    let colony =
                        { colony with
                            Creeps = [ hauler "h1" 0 200 ]
                            Spatial = colony.Spatial |> withCreepsAt [ "h1", { X = 12; Y = 10 } ]
                        }

                    (decideOn colony).Verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", tid, factor) -> Some(tid, factor)
                        | _ -> None)

                Expect.equal
                    (matchIn (mineHaulColony |> withMinePile 600))
                    (Some(taskId (Pickup("pile-min", Thorium)), MatchFactor.Rank))
                    "a container under the cliff is not bleeding, so the decaying copy goes first"

                Expect.equal
                    (matchIn (
                        mineHaulColony
                        |> withMineStock Tuning.defaults.MineContactCliff
                        |> withMinePile 600
                    ))
                    (Some(taskId (Withdraw("can-min", Thorium)), MatchFactor.Rank))
                    "past it the store that is feeding the floor is drained first"
            }

            test "a pile past the threshold is pooled; one under it is left to decay" {
                // `Tuning.PickupThreshold`, the energy pile's own number read
                // down the second column (#167, #311): one threshold and not a
                // second knob, because what it prices is the **trip** and not
                // the cargo. Inclusive at the line, like the energy pile's.
                // Pairwise on the amount alone.
                let pooled units =
                    planTasksOn (mineHaulColony |> withMinePile units) noThreats
                    |> List.contains (Pickup("pile-min", Thorium))

                Expect.isTrue (pooled 100) "a hundred exactly is worth the walk"
                Expect.isFalse (pooled 99) "one under it is left where it lies"
            }

            test "the pile's capacity is counted off its own column" {
                // #161's cap, read the way the Thorium Withdraw beside it reads
                // it: the amount of *that* resource over one [[hauler unit]]'s
                // load, which is 200 at this bank. The energy the pile holds
                // none of admits nobody, which is why the pile never reaches the
                // energy Pickup's pool at all.
                let seatsAt units =
                    poolOn (mineHaulColony |> withMinePile units)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Pickup("pile-min", Thorium) then
                            Capacity.capOf CapScope.Everyone pooled.Capacity
                        else
                            None)

                Expect.equal (seatsAt 600) (Some 3) "six hundred of Thorium is three loads"
                Expect.equal (seatsAt 200) (Some 1) "one load is one seat"
            }

            test "a pile in a room somebody else owns is nobody's" {
                // Whose the ore is, is whose the room is (#261, #311). A pile
                // carries no owner at all, and `FIND_DROPPED_RESOURCES` answers
                // for every player's — but an extractor needs an **owned** RCL6
                // room, so the only Thorium that can be lying in a room we own
                // is Thorium a miner of ours dug. Pairwise on the room's owner
                // alone: the same pile, the same amount, filed one room over.
                let withPileIn room control =
                    { mineHaulColony with
                        RoomControl = Map.add room control mineHaulColony.RoomControl
                        Spatial =
                            { mineHaulColony.Spatial with
                                TargetKinds =
                                    Map.add
                                        "pile-r"
                                        (Dropped Thorium)
                                        mineHaulColony.Spatial.TargetKinds
                                Thorium = Map.add "pile-r" 630 mineHaulColony.Spatial.Thorium
                            }
                            |> withNeighbour
                                room
                                { RoomLayer.empty with
                                    Terrain =
                                        Map.ofList [ for x in 8..12 -> { X = x; Y = 10 }, Plain ]
                                    TargetPositions = Map.ofList [ "pile-r", minePost ]
                                }
                    }

                let pooledIn colony =
                    planTasksOn colony noThreats |> List.contains (Pickup("pile-r", Thorium))

                Expect.isTrue
                    (pooledIn (withPileIn "W1N2" ownedRoom))
                    "the premise: a room of ours, and its floor is ours to sweep"

                Expect.isFalse
                    (pooledIn (withPileIn "W1N2" rivalRoom))
                    "another player's mine bleeding is not this colony's trip"
            }
        ]
