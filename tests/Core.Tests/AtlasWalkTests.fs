/// The walk's whole ticks and the first step it takes.
module Fabot.Core.Tests.AtlasWalkTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

[<Tests>]
let walkTests =
    testList
        "atlas walk"
        [
            test "the walk is at least the Chebyshev distance to the tile it reaches" {
                // ADR 0029's floor, checked over a fixture rather than an
                // example: every body against every source in a room of
                // mixed terrain, roads and scattered walls. No body crosses
                // a tile in less than a tick, so no walk may price below
                // the tiles it must cross — the defect #79 reported, stated
                // as a property the pricing cannot break.
                let bodies =
                    [
                        "worker", 0, [ Work; Carry; Move ]
                        "loaded", 50, [ Work; Carry; Move ]
                        "hauler", 0, [ Carry; Carry; Move ]
                        "anchor", 0, [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "surplus", 0, [ Work; Move; Move; Move ]
                    ]

                let starts =
                    [ { X = 0; Y = 0 }; { X = 14; Y = 0 }; { X = 7; Y = 8 }; { X = 12; Y = 13 } ]

                let sources = [ "src-0"; "src-1"; "src-2" ]

                let violations =
                    [
                        for name, energy, body in bodies do
                            for start in starts do
                                let atlas =
                                    mixedRoom [ name, start ]
                                    |> snapshotWith [ creepWith name energy body ]
                                    |> ofView

                                for source in sources do
                                    let task = Harvest source

                                    let floor =
                                        workArea atlas task
                                        |> tilesHome atlas
                                        |> Set.toList
                                        |> List.map (range start)
                                        |> function
                                            | [] -> 0
                                            | tiles -> List.min tiles

                                    match walkTicks atlas name task with
                                    | Some walk when walk < floor ->
                                        yield
                                            $"%s{name} from %A{start} to %s{source}: walk %d{walk} under floor %d{floor}"
                                    | _ -> ()
                    ]

                Expect.isEmpty violations "no walk prices below the tiles it must cross"
            }

            test "eight road tiles are eight ticks for an empty road-parity body, not four" {
                // #79's worked example. The empty hauler unit generates no
                // fatigue at all, so travel cost floors its road step at
                // one unit — half a tick — and eight steps read as four
                // ticks once halved. The walk floors the same step at a
                // whole tick: eight tiles, eight ticks.
                let atlas =
                    roadCorridor [ "h", { X = 19; Y = 10 } ]
                    |> snapshotWith [ creepWith "h" 0 [ Carry; Carry; Move ] ]
                    |> ofView

                Expect.equal
                    (walkTicks atlas "h" (Harvest "src-a"))
                    (Some 8)
                    "eight tiles cannot be crossed in fewer than eight ticks"

                Expect.equal
                    (travelCost atlas "h" (Harvest "src-a"))
                    (Some 8)
                    "travel cost is untouched: eight units, the ranking price the old rule halved"
            }

            test "the floor lifts a cheap step without capping a dear one" {
                // The floor is a floor, not a rounding: a worker unit's
                // road step is one tick where travel cost priced it half a
                // one, and its swamp step stays the five ticks the engine
                // charges — ceil(10 / 2) — rather than flattening to the
                // floor beside it.
                let walkOn terrain roads =
                    let atlas = seatPriced terrain roads |> snapshotWith [ worker "w" ] |> ofView

                    walkTicks atlas "w" (Harvest "src-a")

                let road = Set.singleton { X = 10; Y = 11 }

                Expect.equal (walkOn Plain road) (Some 1) "a road step is one tick"
                Expect.equal (walkOn Plain Set.empty) (Some 1) "so is a plain step"
                Expect.equal (walkOn Swamp Set.empty) (Some 5) "a swamp step is five, not one"
            }

            test "a Move surplus buys travel cost half a tick a tile and the walk nothing" {
                // The two numbers deliberately disagree. Three Moves under
                // one Work price a plain step at ceil(2 / 3) = 1 unit — the
                // per-unit floor, which is half of what a plain step costs
                // the worker unit beside it — so six tiles cost six units,
                // and the old rule read those as three ticks. The walk
                // floors each step at a whole tick: six tiles, six ticks.
                // Travel cost keeps ranking the fast body ahead; the clock
                // refuses to believe it.
                let atlas =
                    plainCorridor [ "s", { X = 17; Y = 10 } ]
                    |> snapshotWith [ creepWith "s" 0 [ Work; Move; Move; Move ] ]
                    |> ofView

                Expect.equal
                    (travelCost atlas "s" (Harvest "src-a"))
                    (Some 6)
                    "one unit a tile, half what the worker unit pays for the same ground"

                Expect.equal
                    (walkTicks atlas "s" (Harvest "src-a"))
                    (Some 6)
                    "one tick a tile: no body crosses a tile faster than that"
            }

            test "the walk is blind to standing traffic" {
                // #78 inverted at the Atlas seam: the occupancy surcharge
                // re-prices travel cost around a bystander and the walk does
                // not move, because a creep standing in the lane this tick
                // is not part of the path's physical length.
                let clear =
                    corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ worker "w" ] |> ofView

                let crowded =
                    corridor [ "w", { X = 10; Y = 15 }; "b", { X = 11; Y = 13 } ]
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofView

                Expect.equal
                    (travelCost clear "w" (Harvest "src-a"),
                     travelCost crowded "w" (Harvest "src-a"))
                    (Some 4, Some 12)
                    "travel cost sees the crowd and detours into the swamp Seat"

                Expect.equal
                    (walkTicks clear "w" (Harvest "src-a"), walkTicks crowded "w" (Harvest "src-a"))
                    (Some 2, Some 2)
                    "the walk prices the same two tiles either way"
            }

            test "totality follows travel cost's contract" {
                // ADR 0004, verbatim from travel cost (ADR 0029 changes the
                // pricing, never the contract): unplaceable geometry prices
                // 0, an unreachable Work Area has no walk at all, and a
                // creep already inside has none left to walk.
                let unplaced = corridor [] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (walkTicks unplaced "w" (Harvest "src-a"))
                    (Some 0)
                    "an unplaced creep prices at 0"

                let placed =
                    corridor [ "w", { X = 10; Y = 15 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (walkTicks placed "w" (Harvest "ghost"))
                    (Some 0)
                    "an unplaced target prices at 0, not None"

                let inside =
                    corridor [ "w", { X = 11; Y = 13 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal
                    (walkTicks inside "w" (Harvest "src-a"))
                    (Some 0)
                    "already there: no walk left to cover anything"

                let island = corridor [ "w", { X = 20; Y = 20 } ]

                let stranded =
                    island
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 20; Y = 20 } Plain layer.Terrain
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (walkTicks stranded "w" (Harvest "src-a"))
                    None
                    "an unreachable Work Area has no walk: readers count from now"
            }
        ]

[<Tests>]
let firstStepTests =
    testList
        "atlas firstStep"
        [
            test "the first step follows the cheapest path, detouring around swamp" {
                // Straight lane x = 10 is swamp; the lane at x = 11 is plain
                // and reaches a Seat in as many steps.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 10; Y = 10 }, Wall
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Swamp
                            { X = 10; Y = 13 }, Swamp
                            { X = 10; Y = 14 }, Plain
                            { X = 11; Y = 11 }, Plain
                            { X = 11; Y = 12 }, Plain
                            { X = 11; Y = 13 }, Plain
                        ]
                    |> withCreepsAt [ "w", { X = 10; Y = 14 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the swamp lane for the plain one"
            }

            test "the first step detours around a standing creep when a lane is open" {
                // Same shape as the swamp detour, but on all-plain ground
                // with a creep parked mid-lane: the occupancy surcharge
                // sends the first step into the free lane at x = 11.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 10; Y = 10 }, Wall
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Plain
                            { X = 10; Y = 13 }, Plain
                            { X = 10; Y = 14 }, Plain
                            { X = 11; Y = 11 }, Plain
                            { X = 11; Y = 12 }, Plain
                            { X = 11; Y = 13 }, Plain
                        ]
                    |> withCreepsAt [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofView

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the parked creep's lane for the free one"
            }

            test "a creep already inside the Work Area has no step to take" {
                let atlas =
                    corridor [ "w", { X = 11; Y = 13 } ] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal (firstStepFor atlas "w" (Harvest "src-a")) None "already there"
            }

            test "an unreachable Work Area yields no step: waiting beats marching at a wall" {
                let projection = corridor [ "w", { X = 20; Y = 20 } ]

                let atlas =
                    projection
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 20; Y = 20 } Plain layer.Terrain
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal (firstStepFor atlas "w" (Harvest "src-a")) None "no path, no step"
            }

            test "an unplaced creep has no step: no movement without geometry" {
                let atlas = corridor [] |> snapshotWith [ worker "w" ] |> ofView

                Expect.equal (firstStepFor atlas "w" (Harvest "src-a")) None "nothing derivable"
            }
        ]

[<Tests>]
let firstStepIgnoringTrafficTests =
    testList
        "atlas firstStepIgnoringTraffic"
        [
            test "the traffic-blind step keeps the lane the surcharge steers the priced step out of" {
                // The reroute attribution's whole comparison (ADR 0008, ADR
                // 0009): the same body over the same ground, once with
                // today's crowd priced in and once without. A creep parked
                // mid-lane bends the priced step into the parallel lane;
                // the blind step walks straight at it.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 10; Y = 10 }, Wall
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Plain
                            { X = 10; Y = 13 }, Plain
                            { X = 10; Y = 14 }, Plain
                            { X = 11; Y = 11 }, Plain
                            { X = 11; Y = 12 }, Plain
                            { X = 11; Y = 13 }, Plain
                        ]
                    |> withCreepsAt [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                    |> snapshotWith [ worker "w"; worker "b" ]
                    |> ofView

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the priced step leaves the parked creep's lane"

                Expect.equal
                    (firstStepBlindFor atlas "w" (Harvest "src-a"))
                    (Some { X = 10; Y = 13 })
                    "the blind step holds the lane: the detour is the surcharge's doing"
            }

            test "the blind route is priced in travel cost's units, never the walk's ticks" {
                // Two lanes to two Seats, and the two prices choose
                // differently. The paved lane is three road steps (3 units,
                // 3 ticks); the bare lane is two plain steps (4 units, 2
                // ticks). Travel cost's units buy the trunk — which is what
                // the trunk is for — and the walk's whole ticks flatten
                // road and plain for this body and take the short lane
                // instead. The attribution compares against firstStep's
                // route, so it must read the units: the shared memo's
                // traffic-blind entries are two, and this is the other one.
                let atlas =
                    spatial
                        [ "src-a", { X = 10; Y = 10 } ]
                        [
                            { X = 10; Y = 10 }, Wall
                            { X = 11; Y = 9 }, Plain
                            { X = 11; Y = 10 }, Wall
                            { X = 11; Y = 11 }, Plain
                            { X = 12; Y = 8 }, Plain
                            { X = 12; Y = 9 }, Wall
                            { X = 12; Y = 10 }, Wall
                            { X = 12; Y = 11 }, Plain
                            { X = 13; Y = 9 }, Plain
                            { X = 13; Y = 10 }, Plain
                        ]
                    |> withHome (fun layer ->
                        { layer with
                            Roads =
                                Set.ofList
                                    [ { X = 13; Y = 9 }; { X = 12; Y = 8 }; { X = 11; Y = 9 } ]
                            CreepPositions = Map.ofList [ "w", { X = 13; Y = 10 } ]
                        })
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-a"))
                    (Some 3)
                    "three road steps beat two plain ones in units"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-a"))
                    (Some 2)
                    "two plain steps beat three road ones in whole ticks"

                Expect.equal
                    (firstStepBlindFor atlas "w" (Harvest "src-a"))
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    "empty ground: the blind route is the priced one, down the paved lane"

                Expect.equal
                    (firstStepBlindFor atlas "w" (Harvest "src-a"))
                    (Some { X = 13; Y = 9 })
                    "the road lane, not the whole-tick lane at (12,11)"
            }
        ]
