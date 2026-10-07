/// The walk's whole ticks and the first step it takes.
module Fabot.Core.Tests.AtlasWalkTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

/// Each step plan's first two tiles: the step and what a cadence of 2's one
/// light tick walks.
let private firstTwo (plans: (string * RoomPos list) list) =
    plans |> List.map (fun (creep, path) -> creep, List.truncate 2 path)

/// Two lanes to one source: the straight lane at x = 10 from (10,14) up to the
/// Seat at (10,11), and a parallel one at x = 11 reaching a Seat in as many
/// steps. The two mid-lane tiles' terrain and the standing bodies are the caller's.
let private twoLaneAtlas midTerrain creeps =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 12 }, midTerrain
            { X = 10; Y = 13 }, midTerrain
            { X = 10; Y = 14 }, Plain
            { X = 11; Y = 11 }, Plain
            { X = 11; Y = 12 }, Plain
            { X = 11; Y = 13 }, Plain
        ]
    |> withCreepsAt creeps
    |> snapshotWith (creeps |> List.map (fst >> worker))
    |> ofView

/// A room floored plain from (1,1) to (48,48), one body "w" standing in it.
let private floorWalker (start: Pos) =
    spatial
        []
        [
            for x in 1..48 do
                for y in 1..48 -> { X = x; Y = y }, Plain
        ]
    |> withCreepsAt [ "w", start ]
    |> snapshotWith [ worker "w" ]
    |> ofView

/// The tiles of a rectangle.
let private block (xs: int list) (ys: int list) : Set<Pos> =
    Set.ofList
        [
            for x in xs do
                for y in ys -> { X = x; Y = y }
        ]

/// The ground beside a body at (27,1) on the room's first row.
let private diggerGround =
    Set.ofList
        [
            { X = 26; Y = 1 }
            { X = 26; Y = 2 }
            { X = 27; Y = 2 }
            { X = 28; Y = 1 }
            { X = 28; Y = 2 }
        ]

/// `firstStepAvoiding` from a start on the floor, tiles in and out as grid
/// coordinates.
let private avoidingStep (start: Pos) (avoid: Set<Pos>) (goals: Set<Pos>) : Pos option =
    let atlas = floorWalker start

    firstStepAvoiding atlas "w" avoid (RoomPos.setAt (atlasHome atlas) goals)
    |> stepOf atlas "w"

[<Tests>]
let walkTests =
    testList
        "atlas walk"
        [
            // Movement S2 (`docs/adr/0088`) flipped the S0 pin here, which read
            // the half-tick step units (`stepUnits`) pricing a road below plain
            // for every body, so an unloaded 1:1 ranger, a tile a tick on both,
            // took the road lane. An armed body's walk is now priced in whole
            // ticks (`FatigueFactor.Whole`): the lanes cost it alike, and the
            // road decides nothing. An unarmed body keeps the half-tick units.
            test
                "an unloaded ranger at a MOVE a part prices the road lane and an equally quick plain one alike" {
                // Two three-step lanes from (10,14) to the Seat (10,11): up
                // x = 10, or by (11,13) and (11,12); `paved` is the lane's x.
                let stepWith body paved =
                    let atlas =
                        spatial
                            [ "src-a", { X = 10; Y = 10 } ]
                            [
                                { X = 10; Y = 10 }, Wall
                                { X = 10; Y = 11 }, Plain
                                { X = 10; Y = 12 }, Plain
                                { X = 10; Y = 13 }, Plain
                                { X = 10; Y = 14 }, Plain
                                { X = 11; Y = 12 }, Plain
                                { X = 11; Y = 13 }, Plain
                            ]
                        |> withHome (fun layer ->
                            { layer with
                                Roads = Set.ofList [ { X = paved; Y = 12 }; { X = paved; Y = 13 } ]
                            })
                        |> withCreepsAt [ "w", { X = 10; Y = 14 } ]
                        |> snapshotWith [ creepWith "w" 0 body ]
                        |> ofView

                    let seat = RoomPos.setAt (atlasHome atlas) (Set.singleton { X = 10; Y = 11 })
                    firstStepAvoiding atlas "w" Set.empty seat |> stepOf atlas "w"

                let ranger = [ RangedAttack; Move ]

                Expect.equal
                    (stepWith ranger 11)
                    (stepWith ranger 10)
                    "three ticks either way: the same step whichever lane is paved"

                Expect.equal
                    (stepWith [ Work; Move ] 11)
                    (Some { X = 11; Y = 13 })
                    "an unarmed body still takes the road: four half-tick units against plain's six"
            }

            // Movement S3: what the S2 tie fell to the flood's order for, a
            // fighter's walk that reads the maps breaks toward the tile a
            // hostile strikes later (`prefer`, the user's road case: a ranger as
            // quick off the road as on it never takes it into a melee walking it).
            test "an equal-tick tie at the first step goes to the tile the walk prefers" {
                let stepPreferring (rank: Pos -> int) =
                    let atlas =
                        spatial
                            [ "src-a", { X = 10; Y = 10 } ]
                            [
                                { X = 10; Y = 10 }, Wall
                                { X = 10; Y = 11 }, Plain
                                { X = 10; Y = 12 }, Plain
                                { X = 10; Y = 13 }, Plain
                                { X = 10; Y = 14 }, Plain
                                { X = 11; Y = 12 }, Plain
                                { X = 11; Y = 13 }, Plain
                            ]
                        |> withHome (fun layer ->
                            { layer with
                                Roads = Set.ofList [ { X = 10; Y = 12 }; { X = 10; Y = 13 } ]
                            })
                        |> withCreepsAt [ "w", { X = 10; Y = 14 } ]
                        |> snapshotWith [ creepWith "w" 0 [ RangedAttack; Move ] ]
                        |> ofView

                    let seat = RoomPos.setAt (atlasHome atlas) (Set.singleton { X = 10; Y = 11 })

                    let byIndex index =
                        rank
                            {
                                X = index / Engine.roomSide
                                Y = index % Engine.roomSide
                            }

                    firstStepAvoidingWhere atlas "w" Set.empty (fun _ -> false) (Some byIndex) seat
                    |> stepOf atlas "w"

                Expect.equal
                    (stepPreferring (fun tile -> if tile.X = 11 then 9 else 1))
                    (Some { X = 11; Y = 13 })
                    "the plain lane a hostile reaches later, the road lane no quicker"

                Expect.equal
                    (stepPreferring (fun tile -> if tile.X = 10 then 9 else 1))
                    (Some { X = 10; Y = 13 })
                    "and the road lane where that is the later"
            }

            test "the walk is at least the Chebyshev distance to the tile it reaches" {
                // Every body against every source in a room of mixed
                // terrain, roads and scattered walls (#79 as a property).
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
                // #79's worked example: an empty hauler unit generates no
                // fatigue, so travel cost floors its road step at one unit
                // (half a tick); the walk floors it at a whole tick.
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

            test "a Move surplus buys travel cost half a tick a tile and the walk nothing" {
                // Three Moves under one Work price a plain step at
                // ceil(2 / 3) = 1 unit; the walk floors it at a whole tick.
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
                // The surcharge re-prices travel cost around a bystander;
                // the walk does not move (#78 inverted).
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
                // Travel cost's contract, verbatim: unplaceable geometry
                // prices 0, an unreachable Work Area has no walk at all.
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
                            Terrain = TerrainGrid.add { X = 20; Y = 20 } Plain layer.Terrain
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
                let atlas = twoLaneAtlas Swamp [ "w", { X = 10; Y = 14 } ]

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-a"))
                    (Some { X = 11; Y = 13 })
                    "the step leaves the swamp lane for the plain one"
            }

            test "the first step detours around a standing creep when a lane is open" {
                // Same shape as the swamp detour, but on all-plain ground
                // with a creep parked mid-lane: the occupancy surcharge
                // sends the first step into the free lane at x = 11.
                let atlas = twoLaneAtlas Plain [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]

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
                            Terrain = TerrainGrid.add { X = 20; Y = 20 } Plain layer.Terrain
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
                // The reroute attribution's comparison: the same body over
                // the same ground, once with the crowd priced in and once without.
                let atlas = twoLaneAtlas Plain [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]

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
                // The paved lane is three road steps (3 units, 3 ticks); the
                // bare lane two plain steps (4 units, 2 ticks). The
                // attribution compares against firstStep's route, so it
                // must read the units.
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

[<Tests>]
let stepPlanTests =
    testList
        "atlas step plan"
        [
            test "the mover's first step files the tile after it, off the one flood" {
                // The parked body at (10,13) sends the priced path round it:
                // (11,13), then back onto the lane at (10,12), then the Seat.
                let atlas = twoLaneAtlas Plain [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                let home = atlasHome atlas

                firstStepFor atlas "w" (Harvest "src-a") |> ignore

                Expect.equal
                    (firstTwo (stepPlans atlas))
                    [ "w", [ at home { X = 11; Y = 13 }; at home { X = 10; Y = 12 } ] ]
                    "the first tile and the second, both of the priced path"
            }

            test "the traffic-blind step files nothing, and never overwrites the mover's" {
                let atlas = twoLaneAtlas Plain [ "w", { X = 10; Y = 14 }; "b", { X = 10; Y = 13 } ]
                let home = atlasHome atlas

                firstStepBlindFor atlas "w" (Harvest "src-a") |> ignore
                Expect.isEmpty (stepPlans atlas) "Baseline is a verdict's reading, not a walk"

                firstStepFor atlas "w" (Harvest "src-a") |> ignore
                firstStepBlindFor atlas "w" (Harvest "src-a") |> ignore

                Expect.equal
                    (firstTwo (stepPlans atlas))
                    [ "w", [ at home { X = 11; Y = 13 }; at home { X = 10; Y = 12 } ] ]
                    "the priced lane stands after the blind ask"
            }

            test "a first step that is the goal has no second, so no plan" {
                // One plain step onto the Seat: the path ends on its first tile.
                let atlas = seatPriced Plain Set.empty |> snapshotWith [ worker "w" ] |> ofView

                Expect.isSome (firstStepFor atlas "w" (Harvest "src-a")) "the step is taken"
                Expect.isEmpty (stepPlans atlas) "and there is nothing beyond it to plan"
            }

            test "a walk round an avoid set files its own plan, both tiles off the set" {
                // The light tick replays the second tile: a plan left by a
                // plain flood would walk the body into the Reach it went round.
                let atlas = floorWalker { X = 20; Y = 1 }
                let home = atlasHome atlas

                // The plain walk east files its plan first, straight through.
                firstStepWithin atlas "w" (RoomPos.setAt home diggerGround) |> ignore

                firstStepAvoiding
                    atlas
                    "w"
                    (block [ 21..23 ] [ 1..2 ])
                    (RoomPos.setAt home diggerGround)
                |> ignore

                Expect.equal
                    (firstTwo (stepPlans atlas))
                    [ "w", [ at home { X = 20; Y = 2 }; at home { X = 21; Y = 3 } ] ]
                    "the step under the block and the one past its corner"
            }

            test "a walk round whose first step is the goal leaves no plan" {
                let atlas = floorWalker { X = 20; Y = 3 }
                let home = atlasHome atlas

                firstStepAvoiding
                    atlas
                    "w"
                    (block [ 21..23 ] [ 1..2 ])
                    (RoomPos.setAt home (Set.singleton { X = 21; Y = 4 }))
                |> ignore

                Expect.isEmpty (stepPlans atlas) "nothing beyond the goal to plan"
            }
        ]

[<Tests>]
let firstStepAvoidingTests =
    testList
        "atlas firstStepAvoiding"
        [
            test
                "round a 3x2 block on the edge rows to the ground beside a digger, from every tile west of it" {
                // #522's shape: the block touches the room's first rows and
                // the goal ground lies just past it.
                let avoid = block [ 21..23 ] [ 1..2 ]

                let misses =
                    [
                        for x in 15..20 do
                            for y in 1..4 do
                                let start = { X = x; Y = y }

                                match avoidingStep start avoid diggerGround with
                                | Some step when not (Set.contains step avoid) -> ()
                                | step -> yield start, step
                    ]

                Expect.isEmpty misses "every start steps, and never into the block"

                Expect.equal
                    (avoidingStep { X = 20; Y = 1 } avoid diggerGround)
                    (Some { X = 20; Y = 2 })
                    "the edge start steps down, under the block"
            }

            test "round every small shape, to near ground and to far" {
                let shapes =
                    [
                        "one tile", block [ 21 ] [ 1 ]
                        "2x2 on the edge", block [ 21..22 ] [ 1..2 ]
                        "3x3 on the edge", block [ 21..23 ] [ 1..3 ]
                        "3x2 off the edge", block [ 21..23 ] [ 2..3 ]
                        "a wall five high", block [ 21 ] [ 1..5 ]
                    ]

                let goals = [ "near", diggerGround; "far", Set.singleton { X = 40; Y = 40 } ]

                let misses =
                    [
                        for shape, avoid in shapes do
                            for goal, tiles in goals do
                                match avoidingStep { X = 20; Y = 1 } avoid tiles with
                                | Some step when not (Set.contains step avoid) -> ()
                                | step -> yield shape, goal, step
                    ]

                Expect.isEmpty misses "a step round each, never into it"
            }

            test "to a goal tile touching the avoid set" {
                Expect.equal
                    (avoidingStep
                        { X = 20; Y = 1 }
                        (block [ 21..23 ] [ 1..2 ])
                        (Set.singleton { X = 24; Y = 1 }))
                    (Some { X = 20; Y = 2 })
                    "under the block and up its far side"
            }

            test "none where the set walls the goal off, or covers it" {
                Expect.isNone
                    (avoidingStep { X = 20; Y = 1 } (block [ 21 ] [ 1..48 ]) diggerGround)
                    "a wall of it from edge to edge"

                Expect.isNone
                    (avoidingStep { X = 20; Y = 1 } (block [ 25..29 ] [ 1..3 ]) diggerGround)
                    "every goal tile inside it"
            }
        ]
