/// The Storage as stock rather than flow, and the gate on drawing from it.
/// ADR-0023
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
                // The buffer is brimming in both colonies, so the stock is
                // the only thing the pool can be reporting on.
                let hungry = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ])
                let full = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (refillTasks (planTasksOn hungry noThreats))
                    [ "sto-1" ]
                    "the stock with room pools the deepest Refill of all"

                Expect.isEmpty
                    (refillTasks (planTasksOn full noThreats))
                    "a full stock pools no Refill: there is nowhere left to put a load"
            }

            test "the upgrade buffer outbids the stock, however close the stock stands" {
                // The hauler stands beside the Storage and a step short of
                // the buffer's Work Area, so travel cost points at the stock
                // and only rank can overrule it.
                let { Verdicts = verdicts } =
                    decideOn (stockColony [] (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill("can-ctrl", Energy)), MatchFactor.Rank) ]
                    "the buffer is filled before the stock: rank decided"
            }

            test "a hungry tower outbids the stock, however close the stock stands" {
                // The buffer is brimming, so the tower is the stock's one
                // rival and the factor is evidence about that pair alone.
                let { Verdicts = verdicts } =
                    decideOn (
                        stockColony
                            [ refillable "tower-1" 500 BuiltKind.Tower ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ])
                    )

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill("tower-1", Energy)), MatchFactor.Rank) ]
                    "the guns are fed before the stock: rank decided"
            }

            test "with every other sink full the stock takes the load" {
                // Spawn and tower full, buffer brimming: the deepest tier is
                // the one live Refill.
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
                    decideOn colony

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill("sto-1", Energy))))
                    "the load the colony has nowhere else to put sinks into the stock"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("h1", "sto-1", Energy))
                    "the ordinary transfer Intent serves the Storage"

                Expect.contains
                    intents
                    (SayCreep("h1", "🔋"))
                    "the ordinary battery bubble shows it"

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Refill("sto-1", Energy)),
                            MatchFactor.OnlyCandidate
                        )
                    ]
                    "a stock deposit speaks the Verdicts every other Refill speaks"
            }
        ]

[<Tests>]
let stockGateTests =
    testList
        "storage draw gate"
        [
            test "with every other sink full the stock pools no Withdraw" {
                // The spawn is full and the buffer brimming, so the stock's
                // own Refill is the only one there is. Counting it would
                // gate the Storage open against itself forever.
                let tasks =
                    planTasksOn
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
                // One Withdraw for the one Storage, never one per hungry
                // sink.
                let tasks =
                    planTasksOn
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
                // Every refillable full and only the buffer with room: the
                // buffer's Refill is the whole reason the stock opens.
                let tasks =
                    planTasksOn
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
                // A store with nothing in it is nobody's intake.
                let tasks =
                    planTasksOn
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
                // Twice, because rank beating a tie and rank beating a
                // cheaper rival are two claims: from the lane's middle it is
                // three steps to either Work Area; from inside the stock's
                // the stock costs nothing and the container six.
                let drawFrom pos =
                    decideOn (
                        drawColony
                            (Map.ofList [ "can-src", 500; "can-ctrl", 800; "sto-1", 500 ])
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            pos
                    )

                let equidistant = drawFrom { X = 13; Y = 10 }

                Expect.equal
                    equidistant.Verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Withdraw("can-src", Energy)),
                            MatchFactor.Rank
                        )
                    ]
                    "the flow is emptied before the stock: rank decided"

                let underfoot = drawFrom { X = 16; Y = 10 }

                Expect.equal
                    underfoot.Verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Withdraw("can-src", Energy)),
                            MatchFactor.Rank
                        )
                    ]
                    "and it is emptied first from the stock's own doorstep too"
            }

            // The one exception to the tier gap: a colony whose bank
            // cannot afford the hauler unit it would cast, with room in
            // its ring, is starved, and there the stock's draw ranks with
            // the flow's. The 300 bank's `4C/2M` costs 300, so a bank at
            // 100 with fifty of room in the spawn is starved and a full
            // bank is not.
            test
                "a starved cluster lets the stock tie the flow, and travel cost sends the near body to the stock" {
                let starved pos =
                    { drawColony
                          (Map.ofList [ "can-src", 500; "can-ctrl", 800; "sto-1", 500 ])
                          (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                          pos with
                        Bank = bank 100 300
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }

                Expect.equal
                    (decideOn (starved { X = 16; Y = 10 })).Verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Withdraw("sto-1", Energy)),
                            MatchFactor.TravelCost
                        )
                    ]
                    "on the stock's doorstep, starved: the stock, and by travel cost — the ranks tie"

                Expect.equal
                    (decideOn (starved { X = 10; Y = 10 })).Verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Withdraw("can-src", Energy)),
                            MatchFactor.TravelCost
                        )
                    ]
                    "beside the container, starved: the flow, for the same reason"

                let fed =
                    { starved { X = 16; Y = 10 } with
                        Bank = bank 300 300
                    }

                Expect.equal
                    (decideOn fed).Verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Withdraw("can-src", Energy)),
                            MatchFactor.Rank
                        )
                    ]
                    "a bank that can afford its hauler is not starved: ADR 0023's gap stands and rank decides"
            }

            test
                "the starved stock draw admits the loads the ring can take, not the loads the stock holds" {
                // Two empty carriers on the doorstep, fifty of room in the
                // ring and a 200 load: one draw on the stock, the second
                // body goes to the flow.
                let colony =
                    { drawColony
                          (Map.ofList [ "can-src", 500; "can-ctrl", 800; "sto-1", 5_000 ])
                          (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                          { X = 16; Y = 10 } with
                        Bank = bank 100 300
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }

                let second = creepWith "h2" 0 100 [ Carry; Carry; Move ]

                let both =
                    { colony with
                        Creeps = second :: colony.Creeps
                        Spatial = colony.Spatial |> withCreepsAt [ "h2", { X = 15; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn both

                Expect.hasLength
                    (holdersOf (Withdraw("sto-1", Energy)) assignments)
                    1
                    "fifty of room is one load's errand, whatever the stock holds"

                Expect.hasLength
                    (holdersOf (Withdraw("can-src", Energy)) assignments)
                    1
                    "and the other body hauls the flow"
            }

            test "topping up from the stock outbids surplus work" {
                // The worker stands inside the controller's Work Area and one
                // step from the stock's, so Upgrade is the cheapest rival.
                let colony =
                    { drawColony
                          (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ])
                          (worker "w1" 50 50)
                          { X = 19; Y = 10 } with
                        Sources = []
                    }

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw("sto-1", Energy)), MatchFactor.Rank) ]
                    "a load worth carrying is worth completing first: rank decided"
            }

            test "the flow's own Refill outbids the stock's draw" {
                // There is no rank between a container's Withdraw and the
                // spawn Refill it feeds, so a stock a tier below the
                // containers is a tier below the spawn too. The hauler
                // stands in the stock's Work Area with half a load and the
                // hungry spawn is four steps west.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank) ]
                    "the spawn is fed before the stock is drawn on: rank decided"
            }

            test "both halves of the cycle pool on one tick; the tier gap closes it" {
                // With another sink hungry, a stocked Storage with room
                // pools its Withdraw and its Refill on the same tick. What
                // keeps a part-loaded hauler out of the in-and-out cycle is
                // the tier gap.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let tasks = planTasksOn colony noThreats

                Expect.contains (withdrawTasks tasks) "sto-1" "the stock is an intake this tick"
                Expect.contains (refillTasks tasks) "sto-1" "and a sink on the very same tick"

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw("sto-1", Energy)), MatchFactor.Rank) ]
                    "the draw outranks the load's way back in: rank decided"
            }

            test "the containers dry, the hauler draws on the stock for the spawn" {
                // The containers dry, the stock feeds the spawn: the ordinary
                // withdraw Intent and bubble, nothing of the stock's own.
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
                    decideOn
                        { colony with
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw("sto-1", Energy))))
                    "with nothing in the containers the stock is the intake"

                Expect.contains
                    intents
                    (WithdrawFromStore("h1", "sto-1", Energy, None))
                    "the ordinary withdraw Intent serves the Storage"

                Expect.contains intents (SayCreep("h1", "📥")) "the ordinary inbox bubble shows it"

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Withdraw("sto-1", Energy)),
                            MatchFactor.OnlyCandidate
                        )
                    ]
                    "a stock draw speaks the Verdicts every other Withdraw speaks"
            }

            test "with only the buffer hungry, the stock flows to it and never back" {
                // The buffer's hunger opens the stock, and the tick the
                // hauler fills the buffer outranks the store it just
                // emptied: alternation, not a cycle.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 800; "sto-1", 500 ]
                let beside = { X = 16; Y = 10 }

                let empty =
                    decideOn (
                        drawColony stores (creepWith "h1" 0 100 [ Carry; Carry; Move ]) beside
                    )

                Expect.equal
                    (Map.tryFind "h1" empty.Assignments)
                    (Some(taskId (Withdraw("sto-1", Energy))))
                    "the buffer's own hunger is what opens the stock"

                let filled =
                    decide
                        (drawColony stores (creepWith "h1" 100 0 [ Carry; Carry; Move ]) beside)
                        (Map.ofList [ "h1", taskId (Withdraw("sto-1", Energy)) ])
                        Set.empty
                        None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Withdraw("sto-1", Energy)),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "the full store ends the draw, as it ends every other one"

                Expect.contains
                    filled.Verdicts
                    (Verdict.Matched("h1", taskId (Refill("can-ctrl", Energy)), MatchFactor.Rank))
                    "and the load goes on to the buffer, not back into the stock"
            }

            test "beside a stock that is both its intake and its sink, a hauler idles" {
                // With every other sink full the stock's Withdraw is not
                // pooled at all, so the hauler that would empty and refill
                // one store idles instead.
                let idleOn stores =
                    decideOn (
                        drawColony
                            stores
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            { X = 16; Y = 10 }
                    )

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
                // Nothing about the stock is body-specific: the ordinary
                // Withdraw gate is the whole rule. The empty buffer opens
                // the draw.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ]

                let colonyFor creep =
                    { drawColony stores creep { X = 16; Y = 10 } with
                        Sources = []
                    }

                let worked = decideOn (colonyFor (worker "w1" 0 50))

                Expect.equal
                    worked.Verdicts
                    [
                        Verdict.Matched(
                            "w1",
                            taskId (Withdraw("sto-1", Energy)),
                            MatchFactor.OnlyCandidate
                        )
                    ]
                    "a Work part is neither a bar to the stock nor a ticket to it"

                let heavy = decideOn (colonyFor (anchor "a1" 0 50))

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
                // The Task exists while the condition holds: a hauler
                // mid-trip is released task-gone like any vanishing Task.
                let colonyWithBuffer buffer =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", buffer; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 13; Y = 10 }

                let remembered = Map.ofList [ "h1", taskId (Withdraw("sto-1", Energy)) ]

                let hungry = decideFrom remembered (colonyWithBuffer 800)

                Expect.contains
                    hungry.Verdicts
                    (Verdict.Kept("h1", taskId (Withdraw("sto-1", Energy))))
                    "while one sink still has room the trip stands"

                let filled = decideFrom remembered (colonyWithBuffer 2000)

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Withdraw("sto-1", Energy)),
                        ReleaseReason.TaskGone
                    ))
                    "the tick it fills, the walk it was on is over"
            }

            test "the accepted churn: a load the buffer will not take goes back to the stock" {
                // The hauler filled from the stock while the buffer was
                // hungry and the buffer filled while it walked: the stock is
                // the only sink left, so the remainder goes back.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ]
                let loaded = creepWith "h1" 100 0 [ Carry; Carry; Move ]

                let arrived =
                    decide
                        (drawColony stores loaded { X = 20; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill("can-ctrl", Energy)) ])
                        Set.empty
                        None

                Expect.contains
                    arrived.Verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Refill("can-ctrl", Energy)),
                        ReleaseReason.TaskGone
                    ))
                    "the buffer filled while the hauler walked to it"

                Expect.equal
                    (Map.tryFind "h1" arrived.Assignments)
                    (Some(taskId (Refill("sto-1", Energy))))
                    "the stock is the one sink left: the load turns around"

                let back =
                    decide
                        (drawColony stores loaded { X = 16; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill("sto-1", Energy)) ])
                        Set.empty
                        None

                Expect.contains
                    back.Intents
                    (TransferEnergyToStructure("h1", "sto-1", Energy))
                    "the ordinary transfer puts the remainder back: nothing is dropped"
            }
        ]
