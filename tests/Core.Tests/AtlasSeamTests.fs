/// Seams between rooms and the walks that cross them (ADR 0041).
module Fabot.Core.Tests.AtlasSeamTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

/// The column both rooms of every fixture below are built on: x = 10, y = 10
/// down to 17, plain and walkable. What each case varies is the ring between
/// the two rooms and what is filed in them — never the ground, because a Seam
/// is a fact about the ring and the two columns it joins.
let private seamColumn =
    Map.ofList (plainLine [ for y in 10..17 -> { X = 10; Y = y } ])

[<Tests>]
let seamTests =
    testList
        "atlas seams"
        [
            test "a north neighbour's band joins this room's y=0 to the neighbour's y=49" {
                // W12S28 sits at world (-13,28) and W12S27 at (-13,27), so
                // W12S27 is the room across the top border: the pairing the
                // engine makes is x for x, y=0 onto y=49. A swamp exit is in
                // the band, dearly, exactly as swamp ground is; the wall is
                // not.
                let atlas =
                    bordered
                        [
                            "W12S28",
                            [
                                { X = 10; Y = 0 }, Plain
                                { X = 11; Y = 0 }, Swamp
                                { X = 12; Y = 0 }, Wall
                            ]
                            "W12S27",
                            [
                                { X = 10; Y = 49 }, Plain
                                { X = 11; Y = 49 }, Plain
                                { X = 12; Y = 49 }, Plain
                            ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (seams atlas "W12S28" "W12S27")
                    [ { X = 10; Y = 0 }, { X = 10; Y = 49 }; { X = 11; Y = 0 }, { X = 11; Y = 49 } ]
                    "the passable exits, each beside the tile it lands on, in (X, Y) order"
            }

            test "a wall on the far side takes the pair out, as one on this side does" {
                // The band is what a creep can cross, so both halves have to
                // be ground: an exit onto a wall lands nowhere.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 10; Y = 0 }, Plain; { X = 11; Y = 0 }, Plain ]
                            "W12S27", [ { X = 10; Y = 49 }, Wall; { X = 11; Y = 49 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (seams atlas "W12S28" "W12S27")
                    [ { X = 11; Y = 0 }, { X = 11; Y = 49 } ]
                    "only the pair that is ground on both sides"
            }

            test "a west neighbour's band joins x=0 to x=49" {
                // W13S28 is world (-14,28): one room further west, so the
                // shared border is a column, and the pairing runs y for y.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 0; Y = 30 }, Plain; { X = 0; Y = 31 }, Plain ]
                            "W13S28", [ { X = 49; Y = 30 }, Plain; { X = 49; Y = 31 }, Swamp ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (seams atlas "W12S28" "W13S28")
                    [ { X = 0; Y = 30 }, { X = 49; Y = 30 }; { X = 0; Y = 31 }, { X = 49; Y = 31 } ]
                    "the west column, in (X, Y) order"
            }

            test "the band reads the same from the far side, every pair swapped" {
                // The south and east borders are the north's and the west's
                // read the other way round, which is the whole of what
                // "adjacent" means here: one band, asked from either end.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 10; Y = 0 }, Plain; { X = 0; Y = 30 }, Plain ]
                            "W12S27", [ { X = 10; Y = 49 }, Plain ]
                            "W13S28", [ { X = 49; Y = 30 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (seams atlas "W12S27" "W12S28")
                    [ { X = 10; Y = 49 }, { X = 10; Y = 0 } ]
                    "the south border is the north border swapped"

                Expect.equal
                    (seams atlas "W13S28" "W12S28")
                    [ { X = 49; Y = 30 }, { X = 0; Y = 30 } ]
                    "the east border is the west border swapped"
            }

            test "rooms that share no border share no band" {
                // Diagonal neighbours touch at a corner the engine joins
                // nothing across, and two rooms apart touch not at all. Both
                // answer empty rather than failing: an unpriceable Seam is
                // no Seam, never a blocked one (ADR 0004).
                let ring room y =
                    room, [ { X = 10; Y = y }, Plain; { X = 0; Y = 30 }, Plain ]

                let atlas =
                    bordered [ ring "W12S28" 0; ring "W13S27" 49; ring "W12S26" 49 ]
                    |> snapshotWith []
                    |> ofView

                Expect.isEmpty (seams atlas "W12S28" "W13S27") "a diagonal pair joins nowhere"
                Expect.isEmpty (seams atlas "W12S28" "W12S26") "two rooms apart share no border"
                Expect.isEmpty (seams atlas "W12S28" "W12S28") "and a room borders no self"
            }

            test "a walled border is a neighbour with no band" {
                // The converse of the test above does **not** hold, and the
                // pair of readings must not be mistaken for one rule at two
                // altitudes. `RoomName.neighbouring` is over names and says
                // a Seam *could* join these two; the band is over terrain
                // and says whether one does. Here the shared column carries
                // no passable tile on either side, so the names agree and
                // the band is empty — which is what the captures already
                // hold: `tests/Core.Tests/rooms/W12S27.room` has not one
                // passable tile on its west column, nor W13S29 on its south
                // row, so a declaration one axis step across either would be
                // a neighbour this bot could still never reach (#243).
                let atlas =
                    bordered
                        [
                            // Exits on the north row of each, and nothing at
                            // all on the column the two of them share.
                            "W12S28", [ { X = 10; Y = 0 }, Plain ]
                            "W13S28", [ { X = 10; Y = 0 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.isTrue
                    (RoomName.neighbouring "W12S28" "W13S28")
                    "W13S28 is one axis step west, so the names say a Seam could join them"

                Expect.isEmpty
                    (seams atlas "W12S28" "W13S28")
                    "and the terrain says none does: the shared column is wall end to end"

                // The same pair with one tile opened either side, so the
                // empty answer above is the wall's and not the fixture's.
                let opened =
                    bordered
                        [
                            "W12S28", [ { X = 0; Y = 30 }, Plain ]
                            "W13S28", [ { X = 49; Y = 30 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (seams opened "W12S28" "W13S28")
                    [ { X = 0; Y = 30 }, { X = 49; Y = 30 } ]
                    "one passable tile either side is the whole of what a band needs"
            }

            test "a corner tile is on two borders at once, so it is a Seam on neither" {
                // (0,0) is the north row and the west column both. Offered
                // as a crossing it would hand the same tile two different
                // landings — (0,49) north and (49,0) west — and the engine
                // makes at most one of them, so pricing a route through it
                // would put the creep in the wrong room. Every room the
                // engine generates walls its four corners (all four
                // captures do), so no band on real terrain loses a tile:
                // what is pinned is that a passable corner invents none.
                let atlas =
                    bordered
                        [
                            "W12S28",
                            [
                                { X = 0; Y = 0 }, Plain
                                { X = 1; Y = 0 }, Plain
                                { X = 0; Y = 1 }, Plain
                            ]
                            "W12S27", [ { X = 0; Y = 49 }, Plain; { X = 1; Y = 49 }, Plain ]
                            "W13S28", [ { X = 49; Y = 0 }, Plain; { X = 49; Y = 1 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (seams atlas "W12S28" "W12S27")
                    [ { X = 1; Y = 0 }, { X = 1; Y = 49 } ]
                    "the north band keeps the row and drops the corner"

                Expect.equal
                    (seams atlas "W12S28" "W13S28")
                    [ { X = 0; Y = 1 }, { X = 49; Y = 1 } ]
                    "and the west band, which would otherwise claim the same tile"
            }

            test "a room the projection has no border for answers the empty band" {
                // The outpost the colony cannot see, entry by entry (ADR
                // 0004) — and a name the engine's grammar does not spell is
                // the same absence, not an error. The ungrammatical name
                // carries a ring of its own here, so the band it answers is
                // empty for the one reason under test: the name places no
                // room. Without the ring the missing layer would empty it
                // first and the assertion would hold however the grammar
                // was read.
                let atlas =
                    bordered
                        [
                            "W12S28", [ { X = 10; Y = 0 }, Plain ]
                            "the outpost", [ { X = 10; Y = 49 }, Plain ]
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.isEmpty
                    (seams atlas "W12S28" "W12S27")
                    "the neighbour is unprojected, so there is nothing to join to"

                Expect.isEmpty
                    (seams atlas "W12S28" "the outpost")
                    "and a name outside the grammar places no room at all"
            }

            test "an exit tile is in nothing the projection offers to stand or build on" {
                // The prohibition ADR 0041 keeps by not admitting the border
                // rows as ground: a source in the room's corner has its
                // exits passable and in the Seam band, and not one of them
                // is a Seat, a Work Area tile, a buildable tile, a walkable
                // tile or a passable entry in the flood's weight table. The
                // engine moves a creep that ends its tick on an exit into
                // the next room, so a Matcher that could pick one would lose
                // the creep out from under its Task.
                let corner =
                    { spatial
                          [ "src-a", { X = 1; Y = 1 } ]
                          [
                              { X = 1; Y = 2 }, Plain
                              { X = 2; Y = 1 }, Plain
                              { X = 2; Y = 2 }, Plain
                          ] with
                        Borders =
                            Map.ofList
                                [
                                    "W12S28",
                                    Map.ofList
                                        [
                                            for x in 0..2 do
                                                { X = x; Y = 0 }, Plain

                                            for y in 0..2 do
                                                { X = 0; Y = y }, Plain
                                        ]
                                    "W12S27",
                                    Map.ofList [ for x in 0..2 -> { X = x; Y = 49 }, Plain ]
                                ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.hasLength
                    (seams corner "W12S28" "W12S27")
                    2
                    "the exits are real Seam tiles, not merely absent ones — x 1 and 2, the corner never"

                let onTheBorder (tile: Pos) =
                    tile.X = 0 || tile.X = 49 || tile.Y = 0 || tile.Y = 49

                Expect.equal
                    (seatTilesOf corner "src-a" |> tilesHome corner)
                    (Set.ofList [ { X = 1; Y = 2 }; { X = 2; Y = 1 }; { X = 2; Y = 2 } ])
                    "the corner source seats three interior tiles and no exit"

                Expect.isEmpty
                    (workArea corner (Harvest "src-a") |> tilesHome corner |> Set.filter onTheBorder)
                    "no exit in a Work Area"

                Expect.isEmpty
                    (buildableTilesIn corner (atlasHome corner) |> List.filter onTheBorder)
                    "no exit is buildable"

                Expect.isEmpty
                    (walkableTilesIn corner (atlasHome corner) |> Set.filter onTheBorder)
                    "no exit is walkable"

                // The projection names no room, so its ground is filed
                // under the empty name — the room the census signature
                // already spells that way (ADR 0041).
                let weights = stepWeights corner ""

                Expect.isTrue
                    (List.forall
                        (fun (tile: Pos) -> weights.[tile.X * 50 + tile.Y] < 0)
                        [
                            for x in 0..49 do
                                for y in 0..49 do
                                    if onTheBorder { X = x; Y = y } then
                                        { X = x; Y = y }
                        ])
                    "and the flood's weight table marks every exit impassable"
            }
        ]

[<Tests>]
let seamWalkTests =
    testList
        "atlas seam walk"
        [
            test "the walk is the ground to a tile beside the exit, plus the step onto it" {
                // The near half of a cross-room price with the far leg left
                // off (ADR 0041, #123), which is what a plan anchored on the
                // Seam is measured with (ADR 0042). Charged the way every
                // walk in the colony is: one tick a plain step, the tile the
                // creep steps onto and never the one it starts on.
                let atlas = seamGround toNorthExit (northExit Plain)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 1 })
                    (Some 1)
                    "from the tile beside the exit, the crossing itself is the whole walk"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 3 })
                    (Some 3)
                    "two tiles further back, two more plain steps and the same crossing"
            }

            test "a swamp exit is not free, which is #123's narrowing of the ADR's +1" {
                // ADR 0041 writes the crossing as `+1`; that is the price of
                // stepping onto a *plain* exit under a body at fatigue
                // parity, and a swamp exit costs five like any other swamp.
                let atlas = seamGround toNorthExit (northExit Swamp)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 1 })
                    (Some 5)
                    "the swamp crossing, and nothing else, from the tile beside it"
            }

            test "the tile asked at is charged nothing, whatever it costs to stand on" {
                // The convention spelled out where it bites: a swamp tile
                // beside a plain exit is one tick from the Seam, not six.
                // Whoever walks *in* to that tile pays for it; the walk out
                // of it does not, and two Seats of one source are therefore
                // compared on the ground between them (ADR 0042's pick).
                let atlas =
                    seamGround
                        [ { X = 10; Y = 1 }, Swamp; { X = 10; Y = 2 }, Plain ]
                        (northExit Plain)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 1 })
                    (Some 1)
                    "the swamp tile's own step belongs to the walk that arrives on it"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 2 })
                    (Some 6)
                    "and the tile behind it does pay for it: five onto the swamp, one onto the exit"
            }

            test "no band, no ground and no path each answer with no walk at all" {
                // Total (ADR 0004), one absence at a time. An unpriceable
                // Seam is no Seam and never a blocked one, so each of these
                // costs nothing and stops nothing.
                let atlas =
                    seamGround (({ X = 30; Y = 30 }, Plain) :: toNorthExit) (northExit Plain)

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W15S25" { X = 10; Y = 1 })
                    None
                    "a room four sectors away shares no border, so there is nothing to walk to"

                Expect.equal
                    (seamWalkTicks atlas "W12S27" "W12S28" { X = 10; Y = 48 })
                    None
                    "and a room the projection carries no ground for reaches no exit of its own"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 30; Y = 30 })
                    None
                    "a tile walled off from every crossing is unpriceable, not far away"

                Expect.equal
                    (seamWalkTicks atlas "W12S28" "W12S27" { X = 10; Y = 40 })
                    None
                    "and so is a tile the projection carries no ground for"
            }
        ]

[<Tests>]
let roomTests =
    testList
        "atlas rooms"
        [
            test "two rooms' floods do not meet on one tile" {
                // ADR 0041's reason for a flood table per room while the
                // memo key keeps the three fields ADR 0029 gave it: two
                // rooms hold the same coordinates, so two creeps of one
                // fatigue factor standing on the same tile of different
                // rooms key alike. One table would hand one of them the
                // other room's distances. The two rooms' ground is shaped
                // differently on purpose — a corridor south at home, a
                // corridor west in the outpost — so a flood run over the
                // wrong grid cannot reach the Work Area at all and answers
                // None rather than a number that happens to agree.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source; "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = seamColumn
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for x in 5..10 -> { X = x; Y = 10 } ])
                        TargetPositions = Map.ofList [ "src-out", { X = 4; Y = 10 } ]
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 10 } ]
                    }

                let atlas =
                    home
                    |> withNeighbour "W2N1" outpost
                    |> snapshotWith [ worker "w-home"; worker "w-out" ]
                    |> ofView

                Expect.equal
                    (creepTile atlas "w-home", creepTile atlas "w-out")
                    (Some(at "W1N1" { X = 10; Y = 10 }), Some(at "W2N1" { X = 10; Y = 10 }))
                    "the premise: one coordinate, two rooms, one body between them"

                Expect.equal
                    (travelCost atlas "w-home" (Harvest "src-home"))
                    (Some 14)
                    "seven plain steps down its own corridor"

                Expect.equal
                    (travelCost atlas "w-out" (Harvest "src-out"))
                    (Some 10)
                    "five plain steps down the other room's, priced off the other room's ground"

                Expect.equal
                    (firstStepFor atlas "w-home" (Harvest "src-home"))
                    (Some { X = 10; Y = 11 })
                    "and the route each one walks is its own room's, predecessors and all"

                Expect.equal
                    (firstStepFor atlas "w-out" (Harvest "src-out"))
                    (Some { X = 9; Y = 10 })
                    "the other way entirely, out of the same coordinate"
            }

            test "a Task in the neighbouring room is inapplicable, not mispriced" {
                // Every flood stops at its room's border (ADR 0041), so a
                // creep here and a target there have no priced path between
                // them — and the honest answer is the one an unreachable
                // Work Area in the creep's own room gets: the Task does not
                // apply to this creep. What must never happen is a number,
                // which is what reading the neighbour's tiles out of this
                // room's flood would produce. Since #123 a border can be
                // crossed for a price, but only where there is a Seam to
                // cross at: this projection carries no border ring at all,
                // so the band is empty, the minimum is over nothing, and
                // the answers below are the ones they always were.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = seamColumn
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = seamColumn
                        TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 18 } ]
                    }

                let atlas =
                    home
                    |> withNeighbour "W2N1" outpost
                    |> snapshotWith [ worker "w-home" ]
                    |> ofView

                Expect.isNonEmpty
                    (workArea atlas (Harvest "src-out") |> tilesIn "W2N1")
                    "the target is placed, and its Work Area is the room it stands in"

                Expect.equal
                    (travelCost atlas "w-home" (Harvest "src-out"))
                    None
                    "no ranking price across a border"

                Expect.equal
                    (walkTicks atlas "w-home" (Harvest "src-out"))
                    None
                    "and no clock either — the walk and the price agree (ADR 0030)"

                Expect.isFalse
                    (mayActFor atlas "w-home" (Harvest "src-out"))
                    "and no action reaches across one: the engine's ranges are room-local"

                // The seam `decide` actually prices through. `travelCost`
                // and `walkTicks` are the Task-shaped wrappers; the
                // Matcher, the Emitter and the mover reach the flood with
                // a bare tile set, taken from `workAreaFor`. Were that set
                // the neighbour's ground, this room's flood would answer
                // it a number and a first step off *home* terrain — a
                // creep priced on ground it is not standing on and walked
                // seven tiles inside its own room. So the creep-aware Work
                // Area is empty across a border while the body-blind one
                // above is not, and #123 left it that way: the price
                // crosses the border, the standing tiles do not.
                let area = workAreaFor atlas "w-home" (Harvest "src-out")

                Expect.isEmpty
                    area
                    "the creep has nowhere it may stand: the Work Area it is handed is the empty one"

                Expect.equal
                    (travelCostWithin atlas "w-home" area)
                    None
                    "so the tile-shaped price refuses too, not only the Task-shaped one"

                Expect.equal
                    (firstStep atlas "w-home" (Harvest "src-out") area)
                    None
                    "and the mover is given no step toward it"

                Expect.equal
                    (firstStepIgnoringTraffic atlas "w-home" (Harvest "src-out") area)
                    None
                    "nor the traffic-blind route the reroute attribution compares against"
            }

            test "one room's traffic never surcharges another room's flood" {
                // The occupancy half of the per-room split (ADR 0008's
                // surcharge inside ADR 0041's layering). Both rooms hold
                // the same corridor, and the outpost parks a creep partway
                // down the coordinate the home creep must cross. One
                // shared occupancy grid would price that step ten dearer —
                // a one-wide corridor has no detour — and reprice a home
                // creep off a creep it can never meet.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = seamColumn
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = seamColumn
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 13 } ]
                    }

                let atlas =
                    home
                    |> withNeighbour "W2N1" outpost
                    |> snapshotWith [ worker "w-home"; worker "w-out" ]
                    |> ofView

                Expect.equal
                    (creepTile atlas "w-out")
                    (Some(at "W2N1" { X = 10; Y = 13 }))
                    "the premise: the other room's creep stands on a coordinate this path crosses"

                Expect.equal
                    (travelCost atlas "w-home" (Harvest "src-home"))
                    (Some 14)
                    "seven plain steps, and not one of them surcharged"
            }

            test "a target in a room the projection does not carry is absent, entry by entry" {
                // ADR 0004 read a room at a time: a room with no layer and a
                // room whose every container is empty are one answer, and it
                // is the answer an unplaced target has always had — not
                // priceable, counted against no Task, blocking no action.
                // Both shapes are asserted because the layer admits both and
                // nothing may tell them apart.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-far", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = seamColumn
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                for label, spatial in
                    [
                        "a room with no layer at all", home
                        "a room named and empty", home |> withNeighbour "W3N1" RoomLayer.empty
                    ] do
                    let atlas = spatial |> snapshotWith [ worker "w-home" ] |> ofView

                    Expect.equal (positionOf atlas "src-far") None $"{label}: nowhere to place it"
                    Expect.equal (seats atlas "src-far") None $"{label}: no Seats to derive"

                    Expect.isEmpty
                        (workArea atlas (Harvest "src-far"))
                        $"{label}: and no ground to work it from"

                    Expect.equal
                        (travelCost atlas "w-home" (Harvest "src-far"))
                        (Some 0)
                        $"{label}: unpriceable geometry never counts against a Task"

                    Expect.isTrue
                        (mayActFor atlas "w-home" (Harvest "src-far"))
                        $"{label}: and never blocks an action"
            }

            test "one coordinate standing in two rooms is no Post and no Dual Seat" {
                // The bleed a `Set<Pos>` invites, refused where the sets are
                // built (ADR 0041): the outpost's controller puts (10,10)
                // inside an Upgrade area and its container stands on that
                // tile, while at home (10,10) is one of a source's Seats.
                // Unioned across rooms that coordinate would read as a Dual
                // Seat and as a container Post — a Post nothing stands on,
                // an Anchor place nothing can fill, and a source reading as
                // posted with no container of its own.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-home", Source
                                    "ctrl-out", Controller
                                    "cont-out", Structure BuiltKind.Container
                                ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList
                                    [
                                        for x in 9..11 do
                                            for y in 9..11 do
                                                { X = x; Y = y }, Plain
                                    ]
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 9 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList
                                [
                                    for x in 9..11 do
                                        for y in 9..12 do
                                            { X = x; Y = y }, Plain
                                ]
                        TargetPositions =
                            Map.ofList
                                [ "ctrl-out", { X = 10; Y = 12 }; "cont-out", { X = 10; Y = 10 } ]
                    }

                let atlas =
                    home
                    |> withNeighbour "W2N1" outpost
                    |> snapshotWith [ worker "w-home" ]
                    |> ofView

                Expect.isTrue
                    (Set.contains
                        { X = 10; Y = 10 }
                        (seatTilesOf atlas "src-home" |> tilesHome atlas))
                    "the premise: the home source seats that coordinate"

                Expect.isTrue
                    (Set.contains
                        { X = 10; Y = 10 }
                        (workArea atlas (Upgrade "ctrl-out") |> tilesIn "W2N1"))
                    "and the outpost controller's Upgrade area holds it too"

                Expect.isEmpty
                    (dualSeatsIn atlas (atlasHome atlas))
                    "no Dual Seat is made out of two rooms"

                Expect.isEmpty (postsIn atlas (atlasHome atlas)) "and no Post"

                Expect.isEmpty
                    (postsOf atlas "src-home" |> tilesHome atlas)
                    "the home source has none of its own"

                Expect.isFalse
                    (catchesOverflow atlas "w-home" "src-home")
                    "and the other room's container catches nothing this creep digs"
            }

            test "two rooms' Posts on one coordinate are two Posts, not one" {
                // The Anchor row's quota crosses the border since ADR 0042
                // — an outpost's Post hires an Anchor exactly as a home
                // Post does — and this is the shape that decides whether
                // it may be counted by unioning the rooms' tiles. It may
                // not: a `Pos` carries no room, so these two garrison
                // tiles are a room apart at one coordinate, and a union
                // would hire one Anchor to stand on both. Counted room by
                // room they are two.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-home", Source
                                    "can-home", Structure BuiltKind.Container
                                    "src-out", Source
                                    "can-out", Structure BuiltKind.Container
                                ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (plainLine [ { X = 10; Y = 10 } ])
                            TargetPositions =
                                Map.ofList
                                    [
                                        "src-home", { X = 10; Y = 9 }
                                        "can-home", { X = 10; Y = 10 }
                                    ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ { X = 10; Y = 10 } ])
                        TargetPositions =
                            Map.ofList
                                [ "src-out", { X = 10; Y = 11 }; "can-out", { X = 10; Y = 10 } ]
                    }

                let atlas = home |> withNeighbour "W2N1" outpost |> snapshotWith [] |> ofView

                Expect.equal
                    (postsIn atlas (atlasHome atlas))
                    (Set.singleton { X = 10; Y = 10 })
                    "the premise: the home room's own Post is that one tile"

                Expect.equal
                    (postsOf atlas "src-out" |> tilesIn "W2N1")
                    (Set.singleton { X = 10; Y = 10 })
                    "and the outpost rock's Post stands on the same coordinate"

                Expect.equal (postCount atlas) 2 "so the Anchor row is two, never the union's one"
            }

            test "an outpost's Dual Seat is no Post: the colony upgrades one controller" {
                // The Dual Seat half of a Post presumes a controller the
                // colony upgrades, and it upgrades its own room's alone —
                // an outpost's controller it reserves (ADR 0042). Taken
                // across the border the intersection would name a tile
                // nobody ever upgrades from, and that tile would be a Post:
                // an Anchor place and an income share for an outpost source
                // with no container standing under it, which is exactly the
                // switch the container is supposed to be.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source; "ctrl-out", Controller ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (plainLine [ { X = 20; Y = 20 } ])
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ { X = 10; Y = 10 }; { X = 10; Y = 11 } ])
                        TargetPositions =
                            Map.ofList
                                [ "src-out", { X = 10; Y = 10 }; "ctrl-out", { X = 10; Y = 12 } ]
                    }

                let atlas = home |> withNeighbour "W2N1" outpost |> snapshotWith [] |> ofView

                Expect.isTrue
                    (Set.contains { X = 10; Y = 11 } (seatTilesOf atlas "src-out" |> tilesIn "W2N1"))
                    "the premise: (10,11) is a Seat of the outpost rock"

                Expect.isTrue
                    (Set.contains
                        { X = 10; Y = 11 }
                        (workArea atlas (Upgrade "ctrl-out") |> tilesIn "W2N1"))
                    "and the outpost controller's Upgrade area covers it"

                Expect.isEmpty
                    (postsOf atlas "src-out" |> tilesIn "W2N1")
                    "yet the rock has no Post: nothing is built on that Seat"

                Expect.equal (postCount atlas) 0 "so the Anchor row hires nobody for it"
            }

            test "droppedEnergyIn answers each room's own piles on one coordinate" {
                // The pickup reflex's geometry since #166: it measures a
                // bare pile `Pos` against a bare creep `Pos`, so the two
                // have to come out of one layer or a pile at home and a
                // creep in the outpost on the same coordinate read as range
                // 0 (ADR 0041). The kind census stays flat and world-unique
                // — both ids are Dropped here — and it is the join to a
                // *named* room's positions that separates them.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "pile-home", Dropped; "pile-out", Dropped ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = seamColumn
                            TargetPositions = Map.ofList [ "pile-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = seamColumn
                        TargetPositions = Map.ofList [ "pile-out", { X = 10; Y = 10 } ]
                    }

                let atlas = home |> withNeighbour "W2N1" outpost |> snapshotWith [] |> ofView

                Expect.equal
                    (droppedEnergyIn atlas "W1N1")
                    [ "pile-home", at "W1N1" { X = 10; Y = 10 } ]
                    "the home room's pile and no other, though both share the tile"

                Expect.equal
                    (droppedEnergyIn atlas "W2N1")
                    [ "pile-out", at "W2N1" { X = 10; Y = 10 } ]
                    "and the outpost's own, off the layer it is filed in"
            }

            test "placedCreeps files each creep under its own room, in ColonyView order" {
                // The Resolver's list since #145: arbitration runs once per
                // room, each over that room's creeps and tiles alone (ADR
                // 0041's Consequences), so the grouping is the seam that
                // keeps two rooms' coordinates from ever meeting in one
                // `Map<Pos, string>`. Within a group the order is the
                // ColonyView's, as every per-creep derivation's is; a creep
                // the projection places nowhere is in no group (ADR 0004).
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = seamColumn
                            CreepPositions =
                                Map.ofList
                                    [ "b-home", { X = 10; Y = 12 }; "a-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = seamColumn
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 10 } ]
                    }

                let atlas =
                    home
                    |> withNeighbour "W2N1" outpost
                    |> snapshotWith
                        [ worker "b-home"; worker "w-out"; worker "a-home"; worker "ghost" ]
                    |> ofView

                // One flat list in view creep order since #216 R3, each
                // tile carrying its own room (ADR 0052 decision 2) where
                // the answer used to be grouped by room name — the
                // grouping *was* the join.
                Expect.equal
                    (placedCreeps atlas)
                    [
                        "b-home", at "W1N1" { X = 10; Y = 12 }
                        "w-out", at "W2N1" { X = 10; Y = 10 }
                        "a-home", at "W1N1" { X = 10; Y = 10 }
                    ]
                    "each creep under its own room, ColonyView order, the unplaced in none"

                Expect.equal
                    (adjacentWalkableIn atlas "W2N1" { X = 10; Y = 10 })
                    [ { X = 10; Y = 11 } ]
                    "and the standing tiles beside an outpost creep are read off its own room's ground"

                Expect.isEmpty
                    (adjacentWalkableIn atlas "W3N1" { X = 10; Y = 10 })
                    "a room the projection does not carry has no ground beside anything"
            }
        ]

[<Tests>]
let heavyPinJoinTests =
    testList
        "atlas heavy pin joins"
        [
            test "standsAtSource is the engine's harvest range, and it is one tile" {
                // What the empty-window reprieve asks (ADR 0048): not
                // whether the tile catches overflow — `catchesOverflow`
                // answers that — but whether the body could dig the tick
                // the energy lands, which the engine measures at range 1.
                let atlas =
                    pinnedTwoRooms [ "beside", { X = 10; Y = 9 }; "two-out", { X = 10; Y = 8 } ] []
                    |> snapshotWith [ worker "beside"; worker "two-out"; worker "ghost" ]
                    |> ofView

                Expect.isTrue (standsAtSource atlas "beside" "src") "range 1 is in position"

                Expect.isFalse
                    (standsAtSource atlas "two-out" "src")
                    "range 2 is a step short, and a step is a walk"

                Expect.isFalse
                    (standsAtSource atlas "ghost" "src")
                    "and a creep the projection places nowhere stands nowhere (ADR 0004)"

                Expect.isFalse
                    (standsAtSource atlas "beside" "no-such-source")
                    "as does a source it cannot place"
            }

            test "standsAtSource measures range and never Seat membership" {
                // Why the range and not `seatTilesOf` (ADR 0048's third
                // rejected option, and ADR 0004's totality): the Seats are
                // read off the projection's ground, and a creep the engine
                // has put on ground the projection carries none for is in
                // position all the same. (11,10) is such a tile here.
                let atlas =
                    pinnedTwoRooms [ "off-grid", { X = 11; Y = 10 } ] []
                    |> snapshotWith [ worker "off-grid" ]
                    |> ofView

                Expect.isFalse
                    (Set.contains { X = 11; Y = 10 } (seatTilesOf atlas "src" |> tilesHome atlas))
                    "the premise: no ground there, so no Seat there"

                Expect.isTrue
                    (standsAtSource atlas "off-grid" "src")
                    "but the engine will let it dig, so the reprieve holds it"
            }

            test "standsAtSource never joins two rooms on one coordinate" {
                // ADR 0041, the same guard `catchesOverflow` carries: a
                // `Pos` names no room, so a creep at home on the
                // coordinate an outpost source seats is a border away from
                // it and in position for nothing.
                let atlas =
                    pinnedTwoRooms
                        [ "home-body", { X = 10; Y = 9 } ]
                        [ "out-body", { X = 10; Y = 9 } ]
                    |> snapshotWith [ worker "home-body"; worker "out-body" ]
                    |> ofView

                Expect.isTrue
                    (standsAtSource atlas "out-body" "src-out")
                    "the premise: the outpost body is beside the outpost rock"

                Expect.isFalse
                    (standsAtSource atlas "home-body" "src-out")
                    "and the home body on that same coordinate is beside nothing of the sort"
            }

            test "standsOnDualSeat answers for the colony's own room alone" {
                // `postsIn`'s reason (ADR 0042): the colony upgrades one
                // controller, so a Seat beside an outpost's is a tile
                // nobody ever upgrades from — and reading it as a Dual Seat
                // would subtract the outpost Anchor from ADR 0048's
                // reprieve and leave it holding nothing at all.
                let atlas =
                    pinnedTwoRooms
                        [ "home-dual", { X = 10; Y = 11 }; "home-plain", { X = 10; Y = 9 } ]
                        [ "out-dual", { X = 10; Y = 11 } ]
                    |> snapshotWith [ worker "home-dual"; worker "home-plain"; worker "out-dual" ]
                    |> ofView

                Expect.isTrue
                    (Set.contains { X = 10; Y = 11 } (dualSeatsIn atlas (atlasHome atlas)))
                    "the premise: the Seat south of the rock is inside the controller's area"

                Expect.isTrue
                    (standsOnDualSeat atlas "home-dual")
                    "and the body on it stands on one"

                Expect.isFalse
                    (standsOnDualSeat atlas "home-plain")
                    "an ordinary Seat two ranks north is not one"

                Expect.isFalse
                    (standsOnDualSeat atlas "out-dual")
                    "and the outpost's own geometry makes none, however it is shaped"

                Expect.isFalse
                    (standsOnDualSeat atlas "ghost")
                    "a creep the projection places nowhere stands on nothing (ADR 0004)"
            }
        ]

[<Tests>]
let routeTests =
    testList
        "room routes"
        [
            test "the name grid steps four ways, in the order that breaks a tie" {
                // The route search's whole tie-break, so it is pinned before
                // anything reads it: north, east, south, west, off the world
                // coordinates the names spell (W12S28 is (-13, 28)).
                Expect.equal
                    (RoomName.adjacent "W12S28")
                    [ "W12S27"; "W11S28"; "W12S29"; "W13S28" ]
                    "north, east, south, west"

                Expect.isEmpty
                    (RoomName.adjacent "nowhere")
                    "a name outside the grammar steps nowhere"
            }

            test "hopsBetween is the grid distance and never the walk" {
                Expect.equal (RoomName.hopsBetween "W13S28" "W15S29") (Some 3) "two west, one south"
                Expect.equal (RoomName.hopsBetween "W13S28" "W13S28") (Some 0) "a room and itself"

                Expect.equal
                    (RoomName.hopsBetween "W13S28" "W14S29")
                    (Some 2)
                    "a diagonal is two hops"

                Expect.equal (RoomName.hopsBetween "W13S28" "nowhere") None "outside the grammar"
            }

            test "transitBetween is the rectangle's interior, and it is empty for a neighbour" {
                // What decides which rooms are projected as transit layers:
                // every room a shortest chain could pass through, both ends
                // left out.
                Expect.equal
                    (RoomName.transitBetween "W13S28" "W15S29" |> List.sort)
                    [ "W13S29"; "W14S28"; "W14S29"; "W15S28" ]
                    "the four interior rooms of the 3x2 rectangle"

                Expect.isEmpty
                    (RoomName.transitBetween "W13S28" "W13S29")
                    "a one-hop outpost projects no transit room at all, which is today's world"

                Expect.equal
                    (RoomName.transitBetween "W13S28" "W14S29" |> List.sort)
                    [ "W13S29"; "W14S28" ]
                    "a diagonal's two corners"
            }

            test "routeBy crosses the fewest borders the links allow" {
                // `linked` is total here: every grid neighbour is joined, so
                // the search is measuring its own breadth and its tie-break.
                let anywhere _ _ = true

                Expect.equal
                    (RoomName.routeBy anywhere 3 "W13S28" "W15S29")
                    (Some [ "W13S28"; "W13S29"; "W14S29"; "W15S29" ])
                    "three hops, the tie falling south before west by adjacent's order"

                Expect.equal
                    (RoomName.routeBy anywhere 3 "W13S28" "W13S28")
                    (Some [ "W13S28" ])
                    "a room and itself is a chain of one and crosses nothing"

                Expect.equal
                    (RoomName.routeBy anywhere 3 "W13S28" "W13S29")
                    (Some [ "W13S28"; "W13S29" ])
                    "a neighbour is the one-hop chain the Seam model already priced"
            }

            test "the hop budget is a wall and an unlinked room is not walked through" {
                let anywhere _ _ = true

                Expect.equal
                    (RoomName.routeBy anywhere 2 "W13S28" "W15S29")
                    None
                    "three hops under a budget of two is no route at all (ADR 0004)"

                // W13S29's border is walled end to end, so the only chain to
                // W14S29 is the one through W14S28.
                let notThrough room = fun a b -> a <> room && b <> room

                Expect.equal
                    (RoomName.routeBy (notThrough "W13S29") 3 "W13S28" "W14S29")
                    (Some [ "W13S28"; "W14S28"; "W14S29" ])
                    "the search takes the other corner"

                Expect.equal
                    (RoomName.routeBy (fun _ _ -> false) 3 "W13S28" "W13S29")
                    None
                    "a room joined to nothing reaches nothing, neighbour or not"
            }

            test "routesBy answers every chain of the fewest hops, and routeBy is its first" {
                // #288: at one hop the chain is unique, and at two it is
                // not — an L-shaped target is reached round either corner
                // for the same number of borders and not for the same
                // number of ticks. The search hands every one of them out
                // and the price picks (`Atlas.routes`); what `adjacent`'s
                // order still decides is only the order they come in, which
                // is what keeps `routeBy` the answer it always was.
                let anywhere _ _ = true

                Expect.equal
                    (RoomName.routesBy anywhere 3 "W13S28" "W14S29")
                    [ [ "W13S28"; "W13S29"; "W14S29" ]; [ "W13S28"; "W14S28"; "W14S29" ] ]
                    "both corners of the diagonal, south before west by adjacent's order"

                Expect.equal
                    (RoomName.routeBy anywhere 3 "W13S28" "W14S29")
                    (Some [ "W13S28"; "W13S29"; "W14S29" ])
                    "and the one chain the compass used to answer alone is still the first"

                Expect.equal
                    (RoomName.routesBy anywhere 3 "W13S28" "W15S29" |> List.length)
                    3
                    "two west and one south is three chains of three hops, and no fourth"

                Expect.equal
                    (RoomName.routesBy anywhere 3 "W13S28" "W13S29")
                    [ [ "W13S28"; "W13S29" ] ]
                    "a neighbour is one chain: at one hop there is nothing to choose between"

                Expect.equal
                    (RoomName.routesBy anywhere 3 "W13S28" "W13S28")
                    [ [ "W13S28" ] ]
                    "a room and itself crosses nothing"

                Expect.isEmpty
                    (RoomName.routesBy anywhere 2 "W13S28" "W15S29")
                    "past the hop budget there is no chain to choose from at all (ADR 0004)"

                // W13S29's border is walled end to end, so the L has one
                // corner left and the chain is unique again.
                let notThrough room = fun a b -> a <> room && b <> room

                Expect.equal
                    (RoomName.routesBy (notThrough "W13S29") 3 "W13S28" "W14S29")
                    [ [ "W13S28"; "W14S28"; "W14S29" ] ]
                    "a corner the terrain shuts is a chain the search never offers"

                Expect.isEmpty
                    (RoomName.routesBy (fun _ _ -> false) 3 "W13S28" "W13S29")
                    "and a room joined to nothing is reached by no chain"
            }
        ]
