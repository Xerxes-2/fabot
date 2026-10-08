/// The price of a tick of delay (`Price`): the value scale every rank is read
/// off, and the losses a Task's value is made of.
module Fabot.Core.Tests.Decide.PriceTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

[<Tests>]
let scaleTests =
    testList
        "the value scale"
        [
            test
                "a held source's output a tick is rank 0, and every band a factor of thirty-two under the one above" {
                Expect.equal
                    (rankOf heldRate)
                    (priorityOfTier Feeding)
                    "the flow is the Feeding band"

                for tier in [ Feeding; StockDraw; Surplus; UpgradeBuffer; Stock ] do
                    Expect.equal
                        (rankOf (anchorOf tier))
                        (priorityOfTier tier)
                        $"{tier}'s anchor reads back as its own rank"

                Expect.floatClose
                    Accuracy.high
                    (anchorOf Feeding / anchorOf StockDraw)
                    32.0
                    "a band is a factor of thirty-two"
            }

            test "one rank is a factor of root two, and a value of nothing is the floor" {
                Expect.equal (rankOf (heldRate * 2.0)) -2 "double the flow is two ranks up"
                Expect.equal (rankOf (heldRate * sqrt 2.0)) -1 "root two is one rank"
                Expect.equal (rankOf 0.0) bottomRank "nothing waits on it"
                Expect.equal (rankOf 1e-12) bottomRank "never under the floor"

                Expect.equal
                    (economyRank 1e6)
                    (urgentRank + priorityStep)
                    "no economy price reaches the defence band"
            }

            test "the rank is monotone in the value" {
                let values = [ 0.001; 0.01; 0.1; 1.0; 2.5; 5.0; 10.0; 14.0; 20.0; 40.0 ]
                let ranks = values |> List.map rankOf

                Expect.equal
                    ranks
                    (List.sortDescending ranks)
                    "more value, a lower (shallower) rank"
            }
        ]

[<Tests>]
let lossTests =
    testList
        "what a Task loses by waiting"
        [
            test "a full source container loses its source's rate, two ranks over the flow" {
                let draw stored =
                    flowDraw (overflowLoss 0.75 heldRate stored Engine.containerCapacity)

                Expect.equal (rankOf (draw 0)) 0 "an empty container is the flow alone"
                Expect.equal (draw 1_500) heldRate "under the line it keeps what it holds"

                Expect.equal
                    (rankOf (draw Engine.containerCapacity))
                    -2
                    "full, every dig lands on the floor"

                Expect.floatClose
                    Accuracy.high
                    (draw 1_750)
                    (heldRate * 1.5)
                    "half the way to full, half the rate"
            }

            test "a pile loses a thousandth of itself a tick, rounded up" {
                Expect.equal (pileDecay 0) 0.0 "nothing"
                Expect.equal (pileDecay 1) 1.0 "one"
                Expect.equal (pileDecay 1000) 1.0 "a thousand"
                Expect.equal (pileDecay 1001) 2.0 "a thousand and one"
            }

            test
                "the stock is worth half of what it serves, never more than the flow, and all of it to a starving ring" {
                Expect.equal
                    (stockDraw true 40.0)
                    heldRate
                    "a starving spawn: the stock ties the flow"

                Expect.equal (stockDraw false 40.0) (heldRate / 2.0) "re-hauled: half the flow"
                Expect.equal (stockDraw false 2.0) 1.0 "a tower's top-up: half the top-up"
                Expect.equal (stockDraw false -1.0) 0.0 "nothing hungry, nothing worth"
            }

            test
                "a tower's exchange is the far shot's hits for its energy over the cheapest part's hits an energy" {
                Expect.floatClose Accuracy.high towerExchange 7.5 "150 / 10 × 50 / 100"
            }

            test
                "a store that ends loses its store over a tombstone's longest life, never more than a full container" {
                Expect.floatClose
                    Accuracy.high
                    (transientLoss 500)
                    2.0
                    "five hundred over 250 ticks"

                Expect.equal (transientLoss 100_000) heldRate "a ruin's hoard ties a full container"

                Expect.equal
                    (rankOf (flowDraw (transientLoss 100_000)))
                    (rankOf (flowDraw (overflowLoss 0.75 heldRate 2_000 2_000)))
                    "on one rank, where travel decides"
            }

            test "a price's lift is the ranks it stands over the band it rounds to" {
                Expect.equal (liftOverBand -2) 2 "a full container, two over the flow"
                Expect.equal (liftOverBand 8) 2 "the mine at its cliff, two over the stock's band"
                Expect.equal (liftOverBand 0) 0 "the flow itself"
                Expect.equal (liftOverBand 3) 0 "a price under its band lifts nothing"
                Expect.equal (liftOverBand -5) 0 "half a band up rounds to the band below: no lift"
            }

            test "the raid hazard is one over the ticks since the last, never under the prior" {
                Expect.floatClose
                    Accuracy.high
                    (raidHazard 1_000 (Some 900) 0.0)
                    0.01
                    "a hundred ticks"

                Expect.equal (raidHazard 1_000 (Some 1_000) 0.0) 1.0 "standing there now"
                Expect.equal (raidHazard 1_000 None homeRaidPrior) homeRaidPrior "nothing on record"

                Expect.equal
                    (raidHazard 100_000 (Some 1_000) homeRaidPrior)
                    homeRaidPrior
                    "a raid long gone is no safer than none on record"
            }

            test "an empty tower in a home never raided is priced over the surplus's work" {
                let empty = towerShort homeRaidPrior Engine.towerCapacity

                Expect.isLessThan
                    (rankOf empty)
                    (priorityOfTier Surplus)
                    "a raid finds its towers full: they are filled before the surplus is spent"

                Expect.equal (towerShort 1.0 0) 0.0 "a full tower loses nothing"
            }

            test
                "the bodies drinking at a store lose their cost over a life while it stands drained" {
                let idle = idleMouths 0.4

                Expect.floatClose
                    Accuracy.high
                    (idle [ 1_500; 1_500 ] 0 Engine.containerCapacity)
                    2.0
                    "two bodies of 1,500 each, a store run dry"

                Expect.floatClose
                    Accuracy.high
                    (idle [ 1_500 ] 400 Engine.containerCapacity)
                    0.5
                    "half the way down from the drained line, half the cost"

                Expect.equal
                    (idle [ 1_500 ] 800 Engine.containerCapacity)
                    0.0
                    "at the line it feeds them"

                Expect.equal (idle [] 0 Engine.containerCapacity) 0.0 "nobody drinks there"
            }

            test
                "a road at half its hits is the surplus's own, and one near nothing is worth a walk" {
                let decay = float Engine.roadDecayAmount / float Engine.roadDecayTime
                let half = rebuildAtRisk Engine.roadBuildCost 2_500 decay
                let worn = rebuildAtRisk Engine.roadBuildCost 100 decay

                Expect.isLessThanOrEqual
                    (abs (rankOf half - priorityOfTier Surplus))
                    1
                    "half a road's hits: within a rank of the surplus"

                Expect.isLessThanOrEqual
                    (rankOf worn)
                    (priorityOfTier StockDraw)
                    "a fiftieth: a band higher"
            }

            test "a site's progress is at its room's raid hazard" {
                Expect.equal (stompRisk 2_000 0.0) 0.0 "a room no hostile walks"

                Expect.floatClose
                    Accuracy.high
                    (stompRisk 2_000 0.001)
                    2.0
                    "two thousand built, a raid a thousand ticks"

                Expect.equal
                    (constructionCost BuiltKind.Extension)
                    3_000
                    "CONSTRUCTION_COST extension"
            }

            test "a body returns its life's value over its energy and its spawn time" {
                Expect.floatClose
                    Accuracy.high
                    (bodyReturn 15_000.0 700 24 10.0)
                    (15_000.0 / 940.0)
                    "a six-Work anchor's dig over its cost"

                Expect.equal
                    (bodyReturn 1.0 0 0 0.0)
                    0.0
                    "nothing costs nothing and returns nothing"
            }
        ]
