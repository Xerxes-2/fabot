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

            test "the upgrade buffer is budgeted at its room, as the ring is" {
                // W17S25, 2026-10-04: two loads of 465 held a container twenty
                // short of full while the ring the spawn had emptied waited.
                let budgetAt stored =
                    poolOn (stockColony [] (Map.ofList [ "can-ctrl", stored; "sto-1", 0 ]))
                    |> List.tryPick (fun entry ->
                        if entry.Task = Refill("can-ctrl", Energy) then
                            Some entry.Capacity.Budget
                        else
                            None)

                Expect.equal (budgetAt 1_980) (Some(Some 20)) "twenty of room: one load covers it"
                Expect.equal (budgetAt 800) (Some(Some 1_200)) "and an emptier buffer admits more"
            }
            test
                "under the emergency reserve the stock outbids the upgrade buffer: income banks first" {
                // RCL4 keeps 10,000 (#505): under it the load goes to the
                // stock, at it the buffer is fed first again.
                let verdictsAt stock =
                    (decideOn (
                        stockColony [] (Map.ofList [ "can-ctrl", 800; "sto-1", stock ])
                        |> withLevel 4
                    ))
                        .Verdicts

                Expect.equal
                    (verdictsAt 5_000)
                    [ Verdict.Matched("h1", taskId (Refill("sto-1", Energy)), MatchFactor.Rank) ]
                    "under the reserve the stock is filled before the buffer: rank decided"

                Expect.equal
                    (verdictsAt 10_000)
                    [ Verdict.Matched("h1", taskId (Refill("can-ctrl", Energy)), MatchFactor.Rank) ]
                    "at it the buffer outbids the stock again"
            }

            test "while the colony is busy the upgrade buffer is filled after builds and repairs" {
                // W17S25, 2026-10-05 (#523): the haulers kept the buffer full
                // while the west line lay broken. The stock's own Refill
                // stays the deepest.
                let colony =
                    stockColony
                        [ refillable "tower-1" 500 BuiltKind.Tower ]
                        (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ])

                let rankIn busy task =
                    poolBusyOn busy colony
                    |> List.tryPick (fun entry ->
                        if entry.Task = task then Some entry.Priority else None)

                let raided = [ Raided ]
                let buffer = rankIn raided (Refill("can-ctrl", Energy))

                Expect.equal
                    (rankIn [] (Refill("can-ctrl", Energy)))
                    (Some(priorityOfTier UpgradeBuffer))
                    "quiet: the buffer's own tier, as before"

                Expect.isTrue
                    (Option.map2 (>) buffer (Some(priorityOfTier UpgradeBuffer)) = Some true)
                    $"busy: below the buffer tier, where a child's ferry sink stands ({buffer})"

                Expect.isTrue
                    (Option.map2 (>) buffer (rankIn raided (Refill("tower-1", Energy))) = Some true)
                    "and below the surplus tier's sinks"

                Expect.isTrue
                    (Option.map2 (<) buffer (rankIn raided (Refill("sto-1", Energy))) = Some true)
                    "and still above the stock's own Refill"
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

            test "a buffer thirty short opens the stock to one hauler, not three" {
                // Live 2026-10-05 (#530): W12S28's and W13S28's haulers drew
                // the Storage for a sink thirty short, found it full and
                // poured the load back, every tick or two. A draw admits the
                // loads its sinks have room for, not the loads the stock
                // divides into.
                let colony =
                    let one =
                        drawColony
                            (Map.ofList [ "can-src", 0; "can-ctrl", 1_970; "sto-1", 50_000 ])
                            (hauler "h1" 0 100)
                            { X = 16; Y = 10 }

                    { one with
                        Creeps = [ hauler "h1" 0 100; hauler "h2" 0 100; hauler "h3" 0 100 ]
                        Spatial =
                            one.Spatial
                            |> withCreepsAt
                                [
                                    "h1", { X = 16; Y = 10 }
                                    "h2", { X = 15; Y = 10 }
                                    "h3", { X = 16; Y = 11 }
                                ]
                    }

                Expect.equal
                    (holdersOf (Withdraw("sto-1", Energy)) (decideOn colony).Assignments
                     |> List.length)
                    1
                    "thirty of room is one load: one hauler draws, two stay put"
            }

            test "a load drawn from the Storage never goes back into it" {
                // The other half of #530's flip: the sink filled under the
                // hauler, and the one sink left was the store it had just
                // emptied. Pairwise with "the accepted churn" below on where
                // the load came from alone.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ]
                let beside = { X = 16; Y = 10 }

                let drawn =
                    decideFrom
                        (Map.ofList [ "h1", taskId (Withdraw("sto-1", Energy)) ])
                        (drawColony stores (hauler "h1" 100 0) beside)

                Expect.equal
                    (Map.tryFind "h1" drawn.Assignments)
                    None
                    "the store it drew from is no sink for the load"

                Expect.equal
                    (Map.tryFind "h1" drawn.Drawn)
                    (Some
                        {
                            Store = "sto-1"
                            Tick = bareRespawn.Time
                        })
                    "and the draw is recorded for the ticks after"

                let recorded tick energy free =
                    decideOn (
                        drawColony
                            stores
                            { hauler "h1" energy free with
                                DrawnFrom = Some { Store = "sto-1"; Tick = tick }
                            }
                            beside
                    )

                Expect.equal
                    (Map.tryFind "h1" (recorded (bareRespawn.Time - 12) 100 0).Assignments)
                    None
                    "a few ticks on, the record still keeps it out"

                Expect.isEmpty
                    (recorded bareRespawn.Time 0 100).Drawn
                    "an empty body carries no record"
            }

            test "only a load bound for the stock's sinks shrinks its draw" {
                // #530: the supply already committed to the sinks is what a
                // holder of their Refill carries. A convoy two rooms off holds
                // no such Refill, and its thousand aboard must not shut the
                // stock to the buffer thirty short.
                let carriersCap (held: (string * int) list) =
                    let colony =
                        let one =
                            drawColony
                                (Map.ofList [ "can-src", 0; "can-ctrl", 1_970; "sto-1", 50_000 ])
                                (hauler "h1" 0 100)
                                { X = 16; Y = 10 }

                        { one with
                            Creeps =
                                hauler "h1" 0 100
                                :: hauler "convoy" 1_000 0
                                :: [ for name, energy in held -> hauler name energy (100 - energy) ]
                        }

                    let holding =
                        { HeldTaskFacts.empty with
                            Holders =
                                Map.ofList
                                    [ taskId (Refill("can-ctrl", Energy)), List.map fst held ]
                        }

                    planPoolHolding
                        holding
                        colony
                        (Atlas.ofView colony)
                        noThreats
                        []
                        (planTasksOn colony noThreats)
                    |> List.tryPick (fun entry ->
                        if entry.Task = Withdraw("sto-1", Energy) then
                            Capacity.capOf CapScope.Carriers entry.Capacity
                        else
                            None)

                Expect.equal (carriersCap []) (Some 1) "the convoy's load is not the buffer's"
                Expect.equal (carriersCap [ "near", 30 ]) (Some 0) "a holder carrying the thirty is"
            }

            test "the draw record breaks the flip and strands nobody" {
                // #530's backstop stops the immediate pour-back; it must not
                // keep a load out of its store for ever, nor stand for a load
                // that is no longer the store's.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ]
                let beside = { X = 16; Y = 10 }
                let record tick = Some { Store = "sto-1"; Tick = tick }

                let expired =
                    decideOn (
                        drawColony
                            stores
                            { hauler "h1" 100 0 with
                                DrawnFrom =
                                    record (bareRespawn.Time - Tuning.defaults.DrawnHold - 1)
                            }
                            beside
                    )

                Expect.equal
                    (Map.tryFind "h1" expired.Assignments)
                    (Some(taskId (Refill("sto-1", Energy))))
                    "past the hold, a body whose only sink is the store pours back"

                // A load topped up off a 600 pile is the pile's as much as the
                // store's: any other intake clears the record.
                let piled =
                    let colony =
                        drawColony
                            (Map.add "pile-a" 550 stores)
                            { hauler "h1" 100 0 with
                                DrawnFrom = record bareRespawn.Time
                            }
                            beside

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "pile-a", { X = 15; Y = 10 }, Dropped Energy ]
                    }

                let mixed =
                    decideFrom (Map.ofList [ "h1", taskId (Pickup("pile-a", Energy)) ]) piled

                Expect.isEmpty mixed.Drawn "the pickup clears the record"

                Expect.equal
                    (Map.tryFind "h1" mixed.Assignments)
                    (Some(taskId (Refill("sto-1", Energy))))
                    "and the mixed load may go into the store"

                // A Withdraw held but not yet drawn records nothing.
                let walking =
                    decideFrom
                        (Map.ofList [ "h1", taskId (Withdraw("sto-1", Energy)) ])
                        (drawColony stores (hauler "h1" 40 60) { X = 10; Y = 10 })

                Expect.isEmpty
                    walking.Drawn
                    "a body still walking to the store has drawn nothing from it"
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

            test "under the emergency reserve the buffer's hunger opens no draw on the stock" {
                // RCL4 keeps 10,000 (#505): one short of it the buffer is
                // routine Upgrade, fed from the flow and never the stock.
                let poolAt stock =
                    planTasksOn
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", stock ])
                         |> withLevel 4)
                        noThreats

                Expect.equal
                    (withdrawTasks (poolAt 9_999))
                    [ "can-ctrl" ]
                    "one short of the reserve: the stock stays shut"

                Expect.equal
                    (withdrawTasks (poolAt 10_000))
                    [ "can-ctrl"; "sto-1" ]
                    "at the reserve: the hungry-sink gate, unchanged"
            }

            test "a colony latched under its reserve stays under until one hauler load past it" {
                // The 300 bank's hauler carries 200 (#505).
                let poolAt stock latched =
                    planTasksOn
                        ({ stockColony
                               [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                               (Map.ofList [ "can-ctrl", 800; "sto-1", stock ]) with
                            ReserveShort = latched
                         }
                         |> withLevel 4)
                        noThreats

                Expect.equal
                    (withdrawTasks (poolAt 10_199 true))
                    [ "can-ctrl" ]
                    "latched, one short of a load past the line: still shut"

                Expect.equal
                    (withdrawTasks (poolAt 10_200 true))
                    [ "can-ctrl"; "sto-1" ]
                    "a load past it: open"

                Expect.equal
                    (withdrawTasks (poolAt 10_000 false))
                    [ "can-ctrl"; "sto-1" ]
                    "unlatched, the reserve itself is the line"
            }

            test "a colony with no Storage of its own is never under its reserve" {
                // A destroyed Storage must not stop the consignor's fee
                // Refill its container income pays.
                let colony =
                    let fed =
                        stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 2000; "term-1", 0 ])

                    { fed with
                        Consignee = Some "W1N9"
                        Spatial =
                            { fed.Spatial with
                                TargetKinds = Map.remove "sto-1" fed.Spatial.TargetKinds
                            }
                            |> withTargets
                                [ "term-1", { X = 12; Y = 11 }, Structure BuiltKind.Terminal ]
                    }
                    |> withLevel 6

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Refill("term-1", Energy))
                    "no Storage: the fee stock is still a sink"
            }

            test "under the emergency reserve the refill cluster and the towers still open the draw" {
                let poolWith refillables =
                    planTasksOn
                        (stockColony refillables (Map.ofList [ "can-ctrl", 2000; "sto-1", 5_000 ])
                         |> withLevel 4)
                        noThreats

                Expect.contains
                    (withdrawTasks (poolWith [ refillable "ext-1" 50 BuiltKind.Extension ]))
                    "sto-1"
                    "a hungry extension: spawning draws below the reserve"

                Expect.contains
                    (withdrawTasks (poolWith [ refillable "tower-1" 500 BuiltKind.Tower ]))
                    "sto-1"
                    "a hungry tower: so does defence"
            }

            test
                "under the emergency reserve a terminal's energy Refill neither pools nor opens the stock" {
                // A consignor keeps its fee stock in the terminal; under the
                // reserve the Storage pays it no more.
                let poolAt stock =
                    let colony =
                        stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", stock; "term-1", 0 ])

                    planTasksOn
                        ({ colony with
                            Consignee = Some "W1N9"
                            Spatial =
                                colony.Spatial
                                |> withTargets
                                    [ "term-1", { X = 12; Y = 11 }, Structure BuiltKind.Terminal ]
                         }
                         |> withLevel 6)
                        noThreats

                Expect.isFalse
                    (List.contains (Refill("term-1", Energy)) (poolAt 19_999))
                    "under RCL6's 20,000 the terminal is no energy sink"

                Expect.equal
                    (withdrawTasks (poolAt 19_999))
                    [ "can-ctrl" ]
                    "and the stock stays shut"

                Expect.contains
                    (poolAt 20_000)
                    (Refill("term-1", Energy))
                    "at the reserve the fee stock is a sink again"
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
                // hauler fills, the store it just emptied is no sink for the
                // load at all (#530): alternation, not a cycle.
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
                    (Verdict.Matched(
                        "h1",
                        taskId (Refill("can-ctrl", Energy)),
                        MatchFactor.OnlyCandidate
                    ))
                    "and the load goes on to the buffer, the stock no rival for it"
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

            test
                "under the emergency reserve a carrier draws the stock for the spawn and a Work body does not" {
                // The hungry spawn opens the stock either side of RCL4's
                // 10,000 (#505); under it the stock is the carriers', because
                // a Work body's load goes to routine work. Both containers
                // dry, so the stock is the one intake there is.
                let colonyFor stock (creeps: (CreepInfo * Pos) list) =
                    let colony =
                        { drawColony
                              (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", stock ])
                              (fst creeps.Head)
                              (snd creeps.Head) with
                            Sources = []
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }

                    { colony with
                        Creeps = creeps |> List.map fst
                        Spatial =
                            colony.Spatial
                            |> withCreepsAt [ for creep, pos in creeps -> creep.Name, pos ]
                    }
                    |> withLevel 4

                let carrier = creepWith "h1" 0 100 [ Carry; Carry; Move ], { X = 16; Y = 10 }
                let digger = worker "w1" 0 50, { X = 17; Y = 10 }

                let drawsStock stock creeps name =
                    (decideOn (colonyFor stock creeps)).Assignments |> Map.tryFind name = Some(
                        taskId (Withdraw("sto-1", Energy))
                    )

                Expect.isTrue
                    (drawsStock 5_000 [ carrier; digger ] "h1")
                    "a carrier draws the reserve for the spawn"

                Expect.isFalse
                    (drawsStock 5_000 [ carrier; digger ] "w1")
                    "a Work body beside it may not"

                Expect.isTrue
                    (drawsStock 10_000 [ carrier; digger ] "w1")
                    "at the reserve the Work body draws on the same terms again"

                // No carrier of ours alive: the Work body is the only body
                // that can feed the spawn from the stock.
                Expect.isTrue
                    (drawsStock 5_000 [ digger ] "w1")
                    "with no carrier alive the Work body draws the reserve for the spawn"
            }

            test "under the emergency reserve a struck rampart's Repair opens the stock" {
                // Every refillable full and the buffer empty, so only the
                // rampart can open the stock; a melee raider beside it
                // strikes it (`rampartsUnderAttack`).
                let colonyWith hostiles =
                    let colony =
                        { drawColony
                              (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 5_000 ])
                              (worker "w1" 0 50)
                              { X = 16; Y = 10 } with
                            Sources = []
                            Hostiles = hostiles
                        }

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets
                                [ "ram-1", { X = 12; Y = 11 }, Structure BuiltKind.Rampart ]
                    }
                    |> withLevel 4
                    |> withHits "ram-1" BuiltKind.Rampart 50_000 300_000

                let struck =
                    planTasksOn
                        (colonyWith [ hostileAt "raider" { X = 12; Y = 10 } [ Attack; Move ] ])
                        noThreats

                Expect.contains (repairTasks struck) "ram-1" "the struck rampart's Repair is pooled"

                Expect.contains
                    (withdrawTasks struck)
                    "sto-1"
                    "and it opens the stock under the reserve"

                Expect.isFalse
                    (List.contains
                        (Withdraw("sto-1", Energy))
                        (planTasksOn (colonyWith []) noThreats))
                    "the same rampart unstruck is routine Repair: the stock stays shut"
            }

            test
                "under the emergency reserve a dying rampart's Repair opens the stock to a Work body" {
                // Every refillable full and the buffer empty, so only the
                // rampart can be what opens the stock under the reserve.
                let colonyWith hits =
                    { drawColony
                          (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 5_000 ])
                          (worker "w1" 0 50)
                          { X = 16; Y = 10 } with
                        Sources = []
                    }
                    |> withLevel 4
                    |> withHits "ram-1" BuiltKind.Rampart hits 300_000

                let dying = colonyWith 1

                Expect.contains
                    (repairTasks (planTasksOn dying noThreats))
                    "ram-1"
                    "the rampart at one hit is pooled"

                Expect.equal
                    (Map.tryFind "w1" (decideOn dying).Assignments)
                    (Some(taskId (Withdraw("sto-1", Energy))))
                    "and its Repair is the emergency the Work body draws the reserve for"

                Expect.isFalse
                    (List.contains
                        (Withdraw("sto-1", Energy))
                        (planTasksOn (colonyWith 50_000) noThreats))
                    "a rampart merely under its floor is routine Repair: the stock stays shut"
            }

            test "under the emergency reserve the draw admits the loads the emergency sinks take" {
                // Fifty of room in the ring and a 200 load at the 300 bank,
                // with the fixture's carrier alive and holding no Refill, so
                // nothing aboard is committed (#530): one carrier, and no Work
                // body at all with no emergency Repair.
                let capOf colony =
                    poolOn colony
                    |> List.tryPick (fun entry ->
                        if entry.Task = Withdraw("sto-1", Energy) then
                            Some(
                                Capacity.capOf CapScope.Carriers entry.Capacity,
                                Capacity.capOf CapScope.Workers entry.Capacity,
                                Capacity.capOf CapScope.Everyone entry.Capacity
                            )
                        else
                            None)

                let colony stock =
                    stockColony
                        [ refillable "ext-1" 50 BuiltKind.Extension ]
                        (Map.ofList [ "can-ctrl", 2000; "sto-1", stock ])
                    |> withLevel 4

                Expect.equal
                    (capOf (colony 5_000))
                    (Some(Some 1, Some 0, None))
                    "one carrier's load, and no Work body"

                // A dying rampart's slot is a Work body's: a carrier cannot
                // repair it.
                Expect.equal
                    (capOf (colony 5_000 |> withHits "ram-1" BuiltKind.Rampart 1 300_000))
                    (Some(Some 1, Some 1, None))
                    "the emergency Repair adds one slot, for a Work body alone"

                Expect.equal
                    (capOf (colony 50_000))
                    (Some(Some 1, Some 250, None))
                    "over the reserve: the sinks' room for a carrier, the stock over a load for a Work body"
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

[<Tests>]
let storeArrivalTests =
    testList
        "a store's act on arrival, laid for the light tick"
        [
            test "a loaded body stepping beside the Storage pours into it on arrival" {
                // #529: (15,10) is two off the Storage at (17,11).
                let loaded = creepWith "h1" 100 0 [ Carry; Carry; Move ]
                let colony = drawColony Map.empty loaded { X = 15; Y = 10 }

                Expect.equal
                    (arrivalOn colony loaded (Refill("sto-1", Energy)) { X = 16; Y = 11 })
                    (Some(TransferEnergyToStructure("h1", "sto-1", Energy)))
                    "the Emitter's pour, off the tile it steps onto"
            }

            test "a terminal draw names an amount, so it never arrives" {
                // The fee reserve's draw (#349): a draw that landed would be
                // drawn twice by a light tick's replay.
                let empty = creepWith "h1" 0 100 [ Carry; Carry; Move ]

                let colony =
                    let drawn = drawColony (Map.ofList [ "sto-1", 500 ]) empty { X = 15; Y = 10 }

                    { drawn with
                        Spatial =
                            { drawn.Spatial with
                                TargetKinds =
                                    Map.add
                                        "sto-1"
                                        (Structure BuiltKind.Terminal)
                                        drawn.Spatial.TargetKinds
                            }
                    }

                let beside =
                    { colony with
                        Spatial = colony.Spatial |> withCreepsAt [ "h1", { X = 16; Y = 11 } ]
                    }

                Expect.contains
                    (emit
                        beside
                        (Atlas.ofView beside)
                        noThreats
                        (Map.ofList [ "h1", Withdraw("sto-1", Energy) ]))
                    (WithdrawFromStore("h1", "sto-1", Energy, Some 100))
                    "the premise: beside it, the draw names the body's room"

                Expect.isNone
                    (arrivalOn colony empty (Withdraw("sto-1", Energy)) { X = 16; Y = 11 })
                    "on arrival: nothing"
            }
        ]
