/// The Storage as stock rather than flow (ADR 0023), and the gate on
/// drawing from it (ADR 0019).
module Fabot.Core.Tests.Decide.PoolStorageTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.PoolFixtures

[<Tests>]
let stockTests =
    testList
        "storage stock"
        [
            test "a Storage with room is a Refill target; a full one is not" {
                // Judged from the projection's kind, as the buffer's tier is
                // (ADR 0023). The buffer is brimming in both colonies, so the
                // stock is the only thing the pool can be reporting on.
                let hungry = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ])
                let full = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (refillTasks (planTasks hungry noThreats))
                    [ "sto-1" ]
                    "the stock with room pools the deepest Refill of all"

                Expect.isEmpty
                    (refillTasks (planTasks full noThreats))
                    "a full stock pools no Refill: there is nowhere left to put a load"
            }

            test "the upgrade buffer outbids the stock, however close the stock stands" {
                // The hauler stands beside the Storage and a step short of
                // the buffer's Work Area, so travel cost points at the stock
                // and only rank can overrule it: surplus reaches the colony's
                // stock once the upgrade buffer is full and not before (ADR
                // 0023). The tier above the buffer is already pinned by the
                // rank-tier tests, so this one step completes the sequence.
                let { Verdicts = verdicts } =
                    decide
                        (stockColony [] (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "can-ctrl"), MatchFactor.Rank) ]
                    "the buffer is filled before the stock: rank decided"
            }

            test "a hungry tower outbids the stock, however close the stock stands" {
                // The buffer is brimming, so the tower is the stock's one
                // rival and the factor is evidence about that pair alone.
                let { Verdicts = verdicts } =
                    decide
                        (stockColony
                            [ refillable "tower-1" 500 BuiltKind.Tower ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "tower-1"), MatchFactor.Rank) ]
                    "the guns are fed before the stock: rank decided"
            }

            test "with every other sink full the stock takes the load" {
                // Spawn and tower full, buffer brimming: the deepest tier of
                // all is the one live Refill, and it is served by the same
                // transfer Intent, the same bubble and the same Verdict
                // vocabulary as every other Refill (ADR 0023).
                let colony =
                    stockColony
                        [
                            refillable "spawn-1" 0 BuiltKind.Spawn
                            refillable "tower-1" 0 BuiltKind.Tower
                        ]
                        (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ])

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "sto-1")))
                    "the load the colony has nowhere else to put sinks into the stock"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("h1", "sto-1"))
                    "the ordinary transfer Intent serves the Storage"

                Expect.contains
                    intents
                    (SayCreep("h1", "🔋"))
                    "the ordinary battery bubble shows it"

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a stock deposit speaks the Verdicts every other Refill speaks"
            }
        ]

[<Tests>]
let stockGateTests =
    testList
        "storage draw gate"
        [
            test "with every other sink full the stock pools no Withdraw" {
                // The gate (ADR 0023): the stock is an intake only while the
                // pool holds a Refill that is not its own. Here the spawn is
                // full and the buffer brimming, so the stock's own Refill —
                // pooled, because the stock has room — is the only one there
                // is. Counting it would gate the Storage open against itself
                // forever, and a hauler beside it would cycle energy in and
                // out of one store.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]))
                        noThreats

                Expect.equal (refillTasks tasks) [ "sto-1" ] "the stock's own Refill is pooled"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl" ]
                    "and it is not a sink that opens the stock's own Withdraw"
            }

            test "one hungry extension opens it: exactly one Storage Withdraw" {
                // The Planner reads the refillable census, so a hungry
                // extension anywhere in the colony is the sink the stock is
                // drawn for — one Withdraw for the one Storage, never one
                // per hungry sink.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "ext-1" 50 BuiltKind.Extension ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]))
                        noThreats

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "the stocked buffer's intake, and one draw on the stock"
            }

            test "the upgrade buffer counts as a sink: the stock feeds it" {
                // Every refillable full and only the buffer with room, so the
                // buffer's Refill is the whole reason the stock opens —
                // stock flows to the upgrade buffer when the sources cannot
                // keep it full (ADR 0023).
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]))
                        noThreats

                Expect.equal
                    (refillTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "the buffer is the one sink other than the stock"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "and it opens the draw on the stock"
            }

            test "an empty Storage pools no Withdraw, however hungry the colony" {
                // The stock half of ADR 0012's rule, unchanged: a store with
                // nothing in it is nobody's intake.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "ext-1" 50 BuiltKind.Extension ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))
                        noThreats

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl" ]
                    "an open gate draws nothing out of an empty stock"
            }
        ]

[<Tests>]
let stockDrawTests =
    testList
        "storage draw"
        [
            test "the source container outbids the stock, however near the stock stands" {
                // The tier (ADR 0023): the stock sits one tier below the
                // source containers, so an empty hauler empties the flow's
                // own containers first and draws on the stock only when
                // they are dry. The buffer's own hunger is what opened the
                // stock's Withdraw at all. Twice, because rank beating a
                // tie and rank beating a cheaper rival are two claims: from
                // the lane's middle it is three steps to either Work Area,
                // and from inside the stock's the stock costs nothing at
                // all while the container costs six — ADR 0023's own
                // motivating case, a stock that wins every travel-cost
                // contest and must still lose.
                let drawFrom pos =
                    decide
                        (drawColony
                            (Map.ofList [ "can-src", 500; "can-ctrl", 800; "sto-1", 500 ])
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            pos)
                        Map.empty
                        Set.empty
                        None

                let equidistant = drawFrom { X = 13; Y = 10 }

                Expect.equal
                    equidistant.Verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-src"), MatchFactor.Rank) ]
                    "the flow is emptied before the stock: rank decided"

                let underfoot = drawFrom { X = 16; Y = 10 }

                Expect.equal
                    underfoot.Verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-src"), MatchFactor.Rank) ]
                    "and it is emptied first from the stock's own doorstep too"
            }

            test "topping up from the stock outbids surplus work" {
                // The tier's other neighbour: the stock is drawn on above
                // everything the colony merely spends energy on, so a
                // half-loaded creep fills up before it spends. The worker
                // stands inside the controller's Work Area and one step from
                // the stock's, so Upgrade is the cheapest rival of the three
                // and the Verdict's factor is evidence about that pair
                // alone.
                let colony =
                    { drawColony
                          (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ])
                          (worker "w1" 50 50)
                          { X = 19; Y = 10 } with
                        Sources = []
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw "sto-1"), MatchFactor.Rank) ]
                    "a load worth carrying is worth completing first: rank decided"
            }

            test "the flow's own Refill outbids the stock's draw" {
                // The tier's shallow neighbour, and the price of ordering
                // the stock under the flow (ADR 0023): there is no rank
                // between a container's Withdraw and the spawn Refill it
                // feeds, so a stock one tier below the containers is a tier
                // below the spawn too. The hauler stands in the stock's own
                // Work Area with half a load and the hungry spawn is four
                // steps west — it carries what it has rather than topping
                // up first.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the spawn is fed before the stock is drawn on: rank decided"
            }

            test "both halves of the cycle pool on one tick; the tier gap closes it" {
                // What the Planner's gate does not do (ADR 0023): with a
                // sink other than the stock still hungry, a stocked Storage
                // with room pools its Withdraw and its Refill on the same
                // tick, and a part-loaded hauler beside it is applicable to
                // both. What keeps it out of the in-and-out cycle there is
                // the tier gap — the draw at the stock's shallow end, the
                // Refill at the deepest end of all — so it tops up and
                // carries the load away instead of putting it back.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let tasks = planTasks colony noThreats

                Expect.contains (withdrawTasks tasks) "sto-1" "the stock is an intake this tick"
                Expect.contains (refillTasks tasks) "sto-1" "and a sink on the very same tick"

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "sto-1"), MatchFactor.Rank) ]
                    "the draw outranks the load's way back in: rank decided"
            }

            test "the containers dry, the hauler draws on the stock for the spawn" {
                // What the stock is for (ADR 0023): the sources cannot
                // feed the spawn, so the stock does. The hauler already
                // stands beside it, and the ordinary withdraw Intent and the
                // ordinary bubble serve the draw — no Intent of the stock's
                // own, no glyph of its own.
                let colony =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 16; Y = 10 }

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        { colony with
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "sto-1")))
                    "with nothing in the containers the stock is the intake"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "sto-1"))
                    "the ordinary withdraw Intent serves the Storage"

                Expect.contains intents (SayCreep("h1", "📥")) "the ordinary inbox bubble shows it"

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a stock draw speaks the Verdicts every other Withdraw speaks"
            }

            test "with only the buffer hungry, the stock flows to it and never back" {
                // The other half of the ADR 0019 question, with the stock
                // standing where the buffer stood: the hauler draws on the
                // stock because the buffer has room, and the tick it fills
                // up the buffer outranks the store it just emptied — so the
                // pair alternates instead of cycling, exactly as the source
                // containers and the buffer do.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 800; "sto-1", 500 ]
                let beside = { X = 16; Y = 10 }

                let empty =
                    decide
                        (drawColony stores (creepWith "h1" 0 100 [ Carry; Carry; Move ]) beside)
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" empty.Assignments)
                    (Some(taskId (Withdraw "sto-1")))
                    "the buffer's own hunger is what opens the stock"

                let filled =
                    decide
                        (drawColony stores (creepWith "h1" 100 0 [ Carry; Carry; Move ]) beside)
                        (Map.ofList [ "h1", taskId (Withdraw "sto-1") ])
                        Set.empty
                        None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released("h1", taskId (Withdraw "sto-1"), ReleaseReason.Inapplicable))
                    "the full store ends the draw, as it ends every other one"

                Expect.contains
                    filled.Verdicts
                    (Verdict.Matched("h1", taskId (Refill "can-ctrl"), MatchFactor.Rank))
                    "and the load goes on to the buffer, not back into the stock"
            }

            test "beside a stock that is both its intake and its sink, a hauler idles" {
                // The ADR 0019 loop in the shape no body gate could cure —
                // the bodies that feed the spawn from the stock are the ones
                // with no Work part — and the gate that closes it: with
                // every other sink full the stock's Withdraw is not pooled
                // at all, so the hauler that would have emptied and refilled
                // one store tick after tick sits still instead. Idling is
                // the honest state; the stock holds energy the colony has
                // nowhere to put.
                let idleOn stores =
                    decide
                        (drawColony
                            stores
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            { X = 16; Y = 10 })
                        Map.empty
                        Set.empty
                        None

                let withRoom = idleOn (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ])

                Expect.equal
                    (Map.tryFind "h1" withRoom.Assignments)
                    None
                    "a stock that is its own only sink offers no intake"

                Expect.contains
                    withRoom.Verdicts
                    (Verdict.Unassigned("h1", IdleReason.NoneApplicable))
                    "the idle Verdict is the one ADR 0019 left behind"

                let brimming =
                    idleOn (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (Map.tryFind "h1" brimming.Assignments)
                    None
                    "a stock with no room left is no different: still nowhere to carry to"
            }

            test "a Work body draws on the same terms; a Work-heavy body never does" {
                // Nothing about the stock is body-specific (ADR 0023): the
                // ordinary Withdraw gate is the whole rule, so a worker
                // takes the stock exactly as a hauler does, and ADR 0016's
                // comparative gate keeps the Anchor row out of it. The
                // empty buffer is the sink that opens the draw, and holds
                // nothing either body could prefer to it.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ]

                let colonyFor creep =
                    { drawColony stores creep { X = 16; Y = 10 } with
                        Sources = []
                    }

                let worked = decide (colonyFor (worker "w1" 0 50)) Map.empty Set.empty None

                Expect.equal
                    worked.Verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a Work part is neither a bar to the stock nor a ticket to it"

                let heavy = decide (colonyFor (anchor "a1" 0 50)) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" heavy.Assignments)
                    None
                    "a Work-heavy body's intake is digging, whatever the stock holds"

                Expect.contains
                    heavy.Verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "the idle Verdict names the body gate, as ADR 0016 left it"
            }

            test "the tick the last other sink fills, the holder is released task-gone" {
                // The ADR 0013 shape (ADR 0023): the Task exists while the
                // condition holds and is gone otherwise, so a hauler
                // mid-trip is released through the path every vanishing Task
                // already uses — the stock needs no release reason of its
                // own.
                let colonyWithBuffer buffer =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", buffer; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 13; Y = 10 }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "sto-1") ]

                let hungry = decide (colonyWithBuffer 800) remembered Set.empty None

                Expect.contains
                    hungry.Verdicts
                    (Verdict.Kept("h1", taskId (Withdraw "sto-1")))
                    "while one sink still has room the trip stands"

                let filled = decide (colonyWithBuffer 2000) remembered Set.empty None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released("h1", taskId (Withdraw "sto-1"), ReleaseReason.TaskGone))
                    "the tick it fills, the walk it was on is over"
            }

            test "the accepted churn: a load the buffer will not take goes back to the stock" {
                // ADR 0023 accepts one load of this rather than remembering
                // where a load was drawn from. The hauler filled from the
                // stock while the buffer was hungry and the buffer filled
                // while it walked: its Refill is gone, the stock is the only
                // sink left, and the remainder goes back where it came from
                // rather than nowhere at all.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ]
                let loaded = creepWith "h1" 100 0 [ Carry; Carry; Move ]

                let arrived =
                    decide
                        (drawColony stores loaded { X = 20; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill "can-ctrl") ])
                        Set.empty
                        None

                Expect.contains
                    arrived.Verdicts
                    (Verdict.Released("h1", taskId (Refill "can-ctrl"), ReleaseReason.TaskGone))
                    "the buffer filled while the hauler walked to it"

                Expect.equal
                    (Map.tryFind "h1" arrived.Assignments)
                    (Some(taskId (Refill "sto-1")))
                    "the stock is the one sink left: the load turns around"

                let back =
                    decide
                        (drawColony stores loaded { X = 16; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill "sto-1") ])
                        Set.empty
                        None

                Expect.contains
                    back.Intents
                    (TransferEnergyToStructure("h1", "sto-1"))
                    "the ordinary transfer puts the remainder back: nothing is dropped"
            }
        ]
