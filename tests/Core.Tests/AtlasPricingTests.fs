/// Travel cost and the step price table: roads, swamp, traffic (ADR 0008,
/// ADR 0010) and the units each judgement is made in (ADR 0029).
module Fabot.Core.Tests.AtlasPricingTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

[<Tests>]
let travelCostTests =
    testList
        "atlas travelCost"
        [
            test "the cost is the cheapest path to any Work Area tile, swamp priced in" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 4)
                    "the plain Seat at cost 4 beats stepping into the swamp Seat at 12"
            }

            test "a creep already inside the Work Area costs 0" {
                let atlas =
                    corridor [ "w", { X = 11; Y = 13 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal (travelCost atlas "w" (Harvest "src-a")) (Some 0) "already there"
            }

            test "an unreachable Work Area is None: the Task is inapplicable" {
                // The creep sits on a walkable island the corridor cannot reach.
                let projection = corridor [ "w", { X = 20; Y = 20 } ]

                let atlas =
                    projection
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 20; Y = 20 } Plain layer.Terrain
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    None
                    "no path means never matched, never marched"
            }

            test "an unplaced creep prices everything at 0" {
                let atlas = corridor [] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 0)
                    "geometry that cannot be priced never counts against a Task"
            }

            test "a standing creep prices its tile dearer: the free swamp Seat wins" {
                // Another creep parks on the plain Seat at (11,13). Its
                // occupancy surcharge makes that route cost 14, so the
                // untouched swamp Seat at 12 is now the cheapest way in —
                // dearer, but never inapplicable, unlike an obstacle.
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 }; "b", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 12)
                    "standing traffic re-prices the route without ever closing it"
            }

            test "an unplaced target prices at 0, not None" {
                let atlas =
                    corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "ghost"))
                    (Some 0)
                    "an unplaced target is unpriceable, not unreachable"
            }
        ]

[<Tests>]
let roadPricingTests =
    testList
        "atlas road pricing"
        [
            test "a built road prices a step at 1: half a plain step, a tenth of a swamp step" {
                let costOn terrain roads =
                    let atlas = seatPriced terrain roads |> snapshotWith [ worker "w" ] |> ofView

                    travelCost atlas "w" (Harvest "src-a")

                let road = Set.singleton { X = 10; Y = 11 }

                Expect.equal (costOn Plain Set.empty) (Some 2) "a plain step costs 2"
                Expect.equal (costOn Swamp Set.empty) (Some 10) "a swamp step costs 10"
                Expect.equal (costOn Plain road) (Some 1) "a road on plain costs 1: half"

                Expect.equal
                    (costOn Swamp road)
                    (Some 1)
                    "a road on swamp costs 1: the road overrides the terrain under it"
            }

            test "the occupancy surcharge is worth exactly one swamp step" {
                // Another creep parks on the plain Seat: the step onto it
                // costs its plain weight plus the surcharge — 2 + 10, the
                // 10 being the same price as stepping into swamp (ADR 0010).
                let atlas =
                    seatPriced Plain Set.empty
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions =
                                Map.ofList [ "w", { X = 10; Y = 12 }; "b", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 12)
                    "plain weight 2 plus the one-swamp-step surcharge 10"
            }

            test "a road construction site is not yet a road: only Roads tiles price at 1" {
                // A road site is projected as a target of Site kind, never
                // into Roads — the tile keeps pricing by its terrain.
                let atlas =
                    { seatPriced Plain Set.empty with
                        TargetKinds = Map.ofList [ "src-a", Source; "site-1", Site BuiltKind.Other ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            TargetPositions =
                                Map.ofList
                                    [ "src-a", { X = 10; Y = 10 }; "site-1", { X = 10; Y = 11 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 2)
                    "the unbuilt road's tile still prices as plain"
            }

            test "a road discounts passable ground only, and an obstacle overrides it" {
                // The flood prices off a weight table laid once per tick,
                // not off a per-tile query, so the three tiles where a road
                // does not win are pinned here: on a wall (a tunnel, which
                // the projection does not model), off the terrain
                // projection, and under an obstacle.
                let costThrough middle roads obstacles =
                    let atlas =
                        corridorThrough middle roads obstacles
                        |> snapshotWith [ worker "w" ]
                        |> ofView

                    travelCost atlas "w" (Harvest "src-a")

                let road = Set.singleton { X = 10; Y = 12 }
                let blocked = Set.singleton { X = 10; Y = 12 }
                let plain = [ { X = 10; Y = 12 }, Plain ]
                let wall = [ { X = 10; Y = 12 }, Wall ]

                Expect.equal
                    (costThrough plain road Set.empty)
                    (Some 3)
                    "a road on plain carries the corridor: one road step, then a plain Seat"

                Expect.equal
                    (costThrough wall road Set.empty)
                    None
                    "a road on a wall is a tunnel the projection does not model: impassable"

                Expect.equal
                    (costThrough [] road Set.empty)
                    None
                    "a road on a tile outside the terrain projection stays impassable"

                Expect.equal
                    (costThrough plain road blocked)
                    None
                    "an obstacle over a road blocks the tile: the obstacle wins"
            }
        ]

[<Tests>]
let travelUnitTests =
    testList
        "atlas travel units"
        [
            test "the same path costs more units for a body with fewer Move parts per part" {
                // The corridor's cheapest path is two plain steps. The
                // worker unit (1 fatigue part per Move) walks it in 4 units;
                // a heavy body (5 fatigue parts per Move) needs
                // ceil(2 × 5 / 1) = 10 units a step, 20 in all. The empty
                // Carry rides free in both bodies (engine fatigue rules).
                let costFor creep =
                    let atlas =
                        corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ creep ] |> ofView

                    travelCost atlas "w" (Harvest "src-a")

                Expect.equal (costFor (worker "w")) (Some 4) "the worker unit's cost equals terrain"

                Expect.equal
                    (costFor (creepWith "w" 0 [ Work; Work; Work; Work; Work; Carry; Move ]))
                    (Some 20)
                    "five fatigue parts on one Move price each plain step at 10 units"
            }

            test "a Move surplus divides the weight, ceiled, never below one unit a step" {
                // One step onto the only Seat: on swamp (weight 10) the
                // worker pays 10 units, three Moves under one Work pay
                // ceil(10 × 1 / 3) = 4 — the ceil is visible — and on plain
                // (weight 2) the same surplus-Move body pays ceil(2 / 3) =
                // 1: the one-unit floor, never a fraction of a unit.
                let costOn terrain creep =
                    let atlas = seatPriced terrain Set.empty |> snapshotWith [ creep ] |> ofView
                    travelCost atlas "w" (Harvest "src-a")

                Expect.equal
                    (costOn Swamp (worker "w"))
                    (Some 10)
                    "the worker unit's cost equals terrain"

                let surplus = creepWith "w" 0 [ Work; Move; Move; Move ]

                Expect.equal
                    (costOn Swamp surplus)
                    (Some 4)
                    "ceil(10/3) = 4: surplus Moves cannot divide a step below whole units"

                Expect.equal
                    (costOn Plain surplus)
                    (Some 1)
                    "ceil(2/3) = 1: the floor is one unit, never zero"
            }

            test "carried energy loads Carry parts into the fatigue count" {
                // Deliberate choice, documented here: travel is priced from
                // the load the creep carries right now — the engine loads
                // Carry parts 50 energy apiece, and an empty Carry generates
                // no fatigue. The same worker walks the two-plain-step path
                // in 4 units empty and 8 units with its Carry full.
                let costFor energy =
                    let atlas =
                        corridor [ "w", { X = 10; Y = 15 } ]
                        |> snapshotWith [ creepWith "w" energy [ Work; Carry; Move ] ]
                        |> ofView

                    travelCost atlas "w" (Harvest "src-a")

                Expect.equal (costFor 0) (Some 4) "empty: only the Work part generates fatigue"
                Expect.equal (costFor 50) (Some 8) "loaded: the full Carry part joins in"
            }

            test "a body without Move parts reaches nothing beyond where it stands" {
                let atlasAt pos =
                    corridor [ "w", pos ]
                    |> snapshotWith [ creepWith "w" 0 [ Work; Carry ] ]
                    |> ofView

                Expect.equal
                    (travelCost (atlasAt { X = 10; Y = 15 }) "w" (Harvest "src-a"))
                    None
                    "outside the Work Area every path is unwalkable: the Task is inapplicable"

                Expect.equal
                    (travelCost (atlasAt { X = 11; Y = 13 }) "w" (Harvest "src-a"))
                    (Some 0)
                    "already inside, the body works without a step"
            }
        ]

[<Tests>]
let stepPriceTableTests =
    testList
        "atlas step price table"
        [
            test "travel cost prices every weight the ground carries as the body's fatigue" {
                // The flood reads a step's price off a table laid once per
                // flood rather than by asking per relaxation (#168), so the
                // table's whole domain is pinned here against the fatigue
                // arithmetic it stands for: ceil(weight × fatigue parts /
                // Move parts), never below one unit (ADR 0010, ADR 0029).
                // Three weights — road 1, plain 2, swamp 10 — against four
                // bodies, read off one step onto the Seat.
                let unitsOn terrain roads body =
                    let atlas =
                        seatPriced terrain roads |> snapshotWith [ creepWith "w" 0 body ] |> ofView

                    travelCost atlas "w" (Harvest "src-a")

                let seat = Set.singleton { X = 10; Y = 11 }

                let priceOn body =
                    unitsOn Plain seat body,
                    unitsOn Plain Set.empty body,
                    unitsOn Swamp Set.empty body

                Expect.equal
                    (priceOn [ Work; Carry; Move ])
                    (Some 1, Some 2, Some 10)
                    "one fatigue part on one Move pays the weight itself: 1, 2, 10"

                Expect.equal
                    (priceOn [ Work; Work; Work; Work; Work; Carry; Move ])
                    (Some 5, Some 10, Some 50)
                    "five fatigue parts on one Move pay five times it: 5, 10, 50"

                Expect.equal
                    (priceOn [ Work; Move; Move; Move ])
                    (Some 1, Some 1, Some 4)
                    "three Moves under one part: ceil(1/3) and ceil(2/3) hit the one-unit floor, ceil(10/3) = 4"

                Expect.equal
                    (priceOn [ Work; Work; Move; Move ])
                    (Some 1, Some 2, Some 10)
                    "fatigue parity pays the weight again: the ratio is what prices a step, not the part count"
            }

            test "the walk prices the same weights in whole ticks" {
                // The `Walk` row of the same table: two units make a tick,
                // a part of one still costs a whole tick, and no step
                // crosses a tile in less than one (ADR 0029). Same three
                // weights, same four bodies, so the two rows are pinned
                // over one domain and can be read side by side.
                let ticksOn terrain roads body =
                    let atlas =
                        seatPriced terrain roads |> snapshotWith [ creepWith "w" 0 body ] |> ofView

                    walkTicks atlas "w" (Harvest "src-a")

                let seat = Set.singleton { X = 10; Y = 11 }

                let ticksFor body =
                    ticksOn Plain seat body,
                    ticksOn Plain Set.empty body,
                    ticksOn Swamp Set.empty body

                Expect.equal
                    (ticksFor [ Work; Carry; Move ])
                    (Some 1, Some 1, Some 5)
                    "ceil(1/2) and ceil(2/2) are one tick, ceil(10/2) is five"

                Expect.equal
                    (ticksFor [ Work; Work; Work; Work; Work; Carry; Move ])
                    (Some 3, Some 5, Some 25)
                    "ceil(5/2) = 3, ceil(10/2) = 5, ceil(50/2) = 25"

                Expect.equal
                    (ticksFor [ Work; Move; Move; Move ])
                    (Some 1, Some 1, Some 2)
                    "a Move surplus buys the walk nothing below a tick: 1, 1, ceil(4/2) = 2"

                Expect.equal
                    (ticksFor [ Work; Work; Move; Move ])
                    (Some 1, Some 1, Some 5)
                    "fatigue parity walks the worker unit's ticks"
            }

            test "the traffic-blind route prices the same weights, and a Move surplus moves it" {
                // The `Baseline` row (ADR 0030), whose only reader is the
                // reroute attribution's route, so it is read as a choice
                // rather than as a number. Two lanes to one Seat: two steps
                // over swamp, or six over road. The worker unit pays
                // 10 + 2 = 12 for the swamp lane and 5 × 1 + 2 = 7 for the
                // paved one, and takes the long way round; three Moves
                // under one part floor every road step at one unit, so the
                // paved lane costs 5 × 1 + 1 = 6 against the swamp lane's
                // ceil(10/3) + 1 = 5, and the same geometry sends that body
                // the short way. The flip is the table's whole weight
                // domain and the one-unit floor in one assertion.
                let blindStepFor body =
                    let atlas =
                        forkedLanes [ "w", { X = 10; Y = 12 } ]
                        |> snapshotWith [ creepWith "w" 0 body ]
                        |> ofView

                    firstStepBlindFor atlas "w" (Harvest "src-a")

                Expect.equal
                    (blindStepFor [ Work; Carry; Move ])
                    (Some { X = 11; Y = 13 })
                    "the worker unit rounds the paved ring: five road steps beat one swamp step"

                Expect.equal
                    (blindStepFor [ Work; Move; Move; Move ])
                    (Some { X = 10; Y = 11 })
                    "the surplus body cuts across the swamp: its road steps cannot price below one unit"
            }

            test "a body with no Move parts prices no weight at all, under every pricing" {
                // The table's impassable row: `stepUnits` refuses a body
                // the engine's move refuses, and it is written as the same
                // -1 the weight grid marks a wall with, so one test in the
                // flood settles both. Every weight, and all three pricings
                // — travel cost, the walk, and the traffic-blind route.
                let atlasOn terrain roads =
                    seatPriced terrain roads
                    |> snapshotWith [ creepWith "w" 0 [ Work; Carry ] ]
                    |> ofView

                let seat = Set.singleton { X = 10; Y = 11 }

                let answers =
                    [
                        for terrain, roads in [ Plain, seat; Plain, Set.empty; Swamp, Set.empty ] do
                            let atlas = atlasOn terrain roads
                            yield travelCost atlas "w" (Harvest "src-a") |> Option.isSome
                            yield walkTicks atlas "w" (Harvest "src-a") |> Option.isSome
                            yield firstStepBlindFor atlas "w" (Harvest "src-a") |> Option.isSome
                    ]

                Expect.allEqual answers false "no weight is steppable by a body that cannot move"
            }

            test "the weight grid carries no weight the price table has no slot for" {
                // The table spans 0..`Engine.swampWeight`, and the flood reads it
                // unchecked, so it is in range only while swamp stays the
                // dearest ground a grid can hold (ADR 0010). A terrain
                // priced above swamp would index past the end, which under
                // Fable reads as a *free* step where .NET throws — the two
                // halves of one table disagreeing. So the grid's whole
                // weight domain is pinned here, off `stepWeights`: every
                // terrain the projection knows, a road over one and an
                // obstacle over another.
                let weights =
                    spatial
                        []
                        [
                            { X = 1; Y = 1 }, Plain
                            { X = 1; Y = 2 }, Swamp
                            { X = 1; Y = 3 }, Wall
                            { X = 2; Y = 1 }, Plain
                            { X = 2; Y = 2 }, Swamp
                            { X = 2; Y = 3 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Roads = Set.ofList [ { X = 2; Y = 1 }; { X = 2; Y = 2 } ]
                            Obstacles = Set.singleton { X = 2; Y = 3 }
                        })
                    |> snapshotWith []
                    |> ofView
                    // The projection names no room, so its ground is filed
                    // under the empty name (ADR 0041).
                    |> fun atlas -> stepWeights atlas ""

                let swamp = weights.[1 * 50 + 2]

                Expect.equal
                    (Set.ofArray weights)
                    (Set.ofList [ -1; 1; 2; swamp ])
                    "four weights and no more: impassable -1, road 1, plain 2, swamp"

                Expect.isTrue
                    (weights |> Array.forall (fun weight -> weight <= swamp))
                    "and swamp is the dearest of them — the table's last slot is swamp's own"
            }
        ]
