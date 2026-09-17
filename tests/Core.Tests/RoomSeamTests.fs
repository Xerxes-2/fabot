/// Seams and cross-room walks on real terrain (ADR 0041).
module Fabot.Core.Tests.RoomSeamTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

[<Tests>]
let seamTests =
    testList
        "seams on real terrain"
        [
            test "every captured pair of neighbours has a band, on their shared border" {
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let band = seams (acrossFrom near far) near.RoomName far.RoomName
                    let edge = $"{border.From} -> {border.To}"

                    let ground (ring: Map<Pos, Terrain>) tile =
                        match Map.tryFind tile ring with
                        | Some Plain
                        | Some Swamp -> true
                        | Some Wall
                        | None -> false

                    Expect.isNonEmpty band $"{edge}: rooms the engine joins have exits"

                    Expect.all
                        band
                        (fun (here, _) -> border.Near here)
                        $"{edge}: every near tile on this room's own exit row"

                    Expect.all
                        band
                        (fun (_, there) -> border.Far there)
                        $"{edge}: every far tile on the neighbour's opposite row"

                    Expect.all
                        band
                        (fun (here, there) -> border.Along here = border.Along there)
                        $"{edge}: each pair joins one coordinate to itself"

                    Expect.all
                        band
                        (fun (here, there) -> ground near.Border here && ground far.Border there)
                        $"{edge}: no wall on either side of a crossing"

                    Expect.equal
                        band
                        (band |> List.distinct |> List.sortBy (fun (here, _) -> here.X, here.Y))
                        $"{edge}: each pair once, in (X, Y) order"
            }

            test "the bands are as wide as ADR 0041 sizes the cross-room walk on" {
                // 36 north and 19 west are the numbers the cross-room walk
                // is costed against — "a minimum over 36 additions, not 36
                // floods" — so they are asserted of the query, not only of
                // one room's ring. The test below recomputes the band from
                // the same two rings and would follow a narrower recapture
                // of either room down without a word; this is the line that
                // would go red instead.
                for border in borders do
                    let atlas = acrossFrom (load border.From) (load border.To)

                    Expect.hasLength
                        (seams atlas border.From border.To)
                        border.Exits
                        $"{border.From} -> {border.To}: the band the two captures hold"
            }

            test "the band is every exit the two captures agree on, and no other" {
                // The width is pinned above; what is pinned here is which
                // tiles, recomputed off the committed captures rather than
                // written down — the query drops nothing a creep could
                // cross and admits nothing it could not.
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let edge = $"{border.From} -> {border.To}"

                    // A corner is on two borders at once and is a crossing
                    // on neither, so it is not one of the exits the two
                    // rooms agree on. Every capture walls its corners, so
                    // this line changes nothing today; it is here so a
                    // capture that did not would fail the width test above
                    // rather than this one.
                    let corner tile =
                        border.Along tile = 0 || border.Along tile = 49

                    let crossable =
                        near.Border
                        |> Map.toList
                        |> List.filter (fun (tile, terrain) ->
                            border.Near tile
                            && not (corner tile)
                            && terrain <> Wall
                            && far.Border
                               |> Map.exists (fun landing landingTerrain ->
                                   border.Far landing
                                   && border.Along landing = border.Along tile
                                   && landingTerrain <> Wall))
                        |> List.map (fst >> border.Along)

                    Expect.equal
                        (seams (acrossFrom near far) near.RoomName far.RoomName
                         |> List.map (fst >> border.Along))
                        crossable
                        $"{edge}: the band is the exits both rooms hold"
            }

            test "the band reads the same from the neighbour's side, every pair swapped" {
                // Adjacency has no preferred end **on these captures**, and
                // since ADR 0062 that is the captures' fact and not the model's
                // rule. A band asks the *far* room's ground behind the landing,
                // so `seams A B` and `seams B A` are two questions about two
                // different rooms and are free to answer differently — which on
                // W15S26's east border they do, and `RoomSeamTests`' keeper
                // list is where that is pinned. Not one of the pairs in
                // `borders` is a keeper room's, and over raw terrain no capture
                // in this repo orphans a landing at all (ADR 0062's own
                // measurement), so every one of them is symmetric and this says
                // so. What would go red if a re-capture ever orphaned one is
                // this line, and the right answer then is to name the direction
                // rather than to relax the equality.
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let atlas = acrossFrom near far

                    Expect.equal
                        (seams atlas far.RoomName near.RoomName)
                        (seams atlas near.RoomName far.RoomName
                         |> List.map (fun (here, there) -> there, here)
                         |> List.sortBy (fun (here, _) -> here.X, here.Y))
                        $"{border.To} -> {border.From}: the same band, swapped"
            }

            test "a room that borders none of them has no band with any of them" {
                // W15S25 is four rooms away, so nothing joins it — and that
                // is an empty answer, never a failure or a block (ADR 0004).
                let stranger = load "W15S25"

                for name in [ "W12S28"; "W12S27"; "W13S28" ] do
                    let other = load name
                    let atlas = acrossFrom stranger other

                    Expect.isEmpty
                        (seams atlas stranger.RoomName other.RoomName)
                        $"W15S25 -> {name}: no shared border, no band"

                    Expect.isEmpty
                        (seams atlas other.RoomName stranger.RoomName)
                        $"{name} -> W15S25: and none the other way"
            }
        ]

[<Tests>]
let crossRoomWalkTests =
    testList
        "cross-room walks on real terrain"
        [
            test
                "no cross-room walk undercuts the rooms' own distance, but for the border's free tile" {
                // The lower bound ADR 0041's join has to respect, on real
                // terrain and naming no tile: a creep crosses at most one
                // tile of Chebyshev distance per tick, so a walk cannot come
                // in under the distance between where it starts and the
                // nearest tile it may work from — measured on the world
                // grid, with the neighbour's coordinates shifted a room's
                // width into this room's frame.
                //
                // Less exactly one tile, and the one is the crossing itself:
                // the creep pays for stepping onto the exit tile, and the
                // engine then relocates it onto the landing tile in the
                // neighbouring room at the end of that tick, for no tick at
                // all. That free tile is the whole of the slack, it is the
                // engine's rule and not this join's, and every other tile of
                // the journey still costs a tick at least (ADR 0029).
                let mutable priced = 0

                for border in borders do
                    for from, into, offset in
                        [
                            border.From, border.To, border.Offset
                            border.To,
                            border.From,
                            {
                                X = -border.Offset.X
                                Y = -border.Offset.Y
                            }
                        ] do
                        let near = load from
                        let far = load into

                        let shift (tile: Pos) =
                            {
                                X = tile.X + offset.X
                                Y = tile.Y + offset.Y
                            }

                        for stand in standingSample near do
                            let atlas = walkingAcross near far stand

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId

                                Expect.equal
                                    (Option.isSome (travelCost atlas "w" task))
                                    (Option.isSome (walkTicks atlas "w" task))
                                    $"{from} -> {into} from {stand.X},{stand.Y}: one join answers both prices"

                                match walkTicks atlas "w" task with
                                | None -> ()
                                | Some walk ->
                                    priced <- priced + 1

                                    let apart =
                                        workArea atlas task
                                        |> RoomPos.inRoom into
                                        |> Set.toList
                                        |> List.map (shift >> range stand)
                                        |> List.min

                                    Expect.isTrue
                                        (walk + 1 >= apart)
                                        $"{from} -> {into}: a walk of {walk} from {stand.X},{stand.Y} to {sourceId}, {apart} tiles off"

                Expect.isGreaterThan
                    priced
                    0
                    "and the rooms the engine joins really do price across: an empty sweep proves nothing"
            }

            test "a crossing that has a price has a step, and the step stays in this room" {
                // #142's invariant, on real terrain and naming no tile. A
                // Task the Matcher can price is a Task the Matcher will
                // hand out, so a price without a step is a creep parked on
                // an assignment for life — which is the defect this test
                // exists to keep out, stated as an equivalence rather than
                // as a route anybody checked by hand.
                //
                // And the step is always one of this room's own tiles: its
                // ground, or an exit of the band toward the target's room.
                // Never the neighbour's, which a bare `Pos` could not tell
                // apart, and never further than one tile away, because a
                // step is a step (ADR 0001, ADR 0041).
                let mutable stepped = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        // The band is read off the two rings and nothing
                        // else (ADR 0041), so the tile this Atlas stands its
                        // creep on is never looked at.
                        let crossings =
                            seams (walkingAcross near far { X = 0; Y = 0 }) from into
                            |> List.map fst
                            |> Set.ofList

                        for stand in standingSample near do
                            let atlas = walkingAcross near far stand

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId

                                let step =
                                    firstStep atlas "w" task (workAreaFor atlas "w" task)
                                    |> Option.map RoomPos.pos

                                Expect.equal
                                    (Option.isSome step)
                                    (Option.isSome (travelCost atlas "w" task))
                                    $"{from} -> {into} from {stand.X},{stand.Y}: priced and walkable answer alike for {sourceId}"

                                match step with
                                | None -> ()
                                | Some tile ->
                                    stepped <- stepped + 1

                                    Expect.equal
                                        (range stand tile)
                                        1
                                        $"{from} -> {into}: the step from {stand.X},{stand.Y} to {tile.X},{tile.Y} is one tile"

                                    let onGround =
                                        match TerrainGrid.tryFind tile near.Terrain with
                                        | Some terrain -> terrain <> Wall
                                        | None -> false

                                    Expect.isTrue
                                        (onGround || Set.contains tile crossings)
                                        $"{from} -> {into}: {tile.X},{tile.Y} is this room's ground or its exit, nothing else"

                Expect.isGreaterThan
                    stepped
                    0
                    "and the sweep really does walk somebody across: an empty one proves nothing"
            }

            test "a cross-Seam lead prices every goal tile exactly as the Matcher's walk does" {
                // The pin #169's far-leg memo goes in under: by ADR 0030 the
                // lead's cross-room clock and the Matcher's are two readers
                // of one join, so on real terrain and with one near flood
                // between them they agree tile by tile — whatever the lead
                // computes the answer from. A lead priced per goal tile and
                // a lead read out of a table filled once per census are the
                // same number here, or this test is the alarm.
                //
                // Two readings, because the Matcher only ever answers a
                // minimum over a Work Area. Over a real source's Seats that
                // is a cluster, so the lead's per-tile answers are minimised
                // to meet it; over a probe source fenced down to one Seat
                // (`probeBeside`) it is one named tile, and that is where a
                // seeding that is right at a room's cheapest tile and wrong
                // at a dearer one would show — `leadOf` reads the tile its
                // creep happens to stand on, never the room's minimum.
                // Absence has to line up on both: an area whose every tile
                // is unreachable leads nobody, exactly as it walks nobody
                // (ADR 0004).
                let mutable led = 0
                let mutable pinned = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        for birth in standingSample near do
                            // A neighbour of the birth tile, inside the
                            // grid: which one is nobody's decision worth
                            // making, since the spawner's own tile is fenced
                            // off and never walked.
                            let spawn =
                                if birth.Y < 48 then
                                    { birth with Y = birth.Y + 1 }
                                else
                                    { birth with Y = birth.Y - 1 }

                            let atlas = leadingAcross near far spawn birth [] Set.empty

                            Expect.equal
                                (adjacentWalkableIn atlas from spawn)
                                [ birth ]
                                $"the premise: at {spawn.X},{spawn.Y} a body is born on {birth.X},{birth.Y} and nowhere else"

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId

                                // The body-blind area, because that is the
                                // set the far leg of a cross-room price
                                // floods into: `workAreaFor` hands a creep
                                // only its *own* room's tiles (ADR 0041),
                                // and this light body narrows nothing
                                // anyway (ADR 0020).
                                let perTile =
                                    workArea atlas task
                                    |> Set.toList
                                    |> List.choose (castWalkTicks atlas leadBody spawn)

                                let leadWalk =
                                    match perTile with
                                    | [] -> None
                                    | ticks ->
                                        led <- led + 1
                                        Some(List.min ticks)

                                Expect.equal
                                    leadWalk
                                    (walkTicks atlas "w" task)
                                    $"{from} -> {into}: the lead out of {spawn.X},{spawn.Y} and the walk from {birth.X},{birth.Y} price {sourceId} alike"

                            // Unreachable, tile by tile, and on the same
                            // Atlas the reachable ones were read off: the far
                            // room's own wall is no ground, and its ring is
                            // no ground either — both absent, never a zero
                            // that would leave a creep counted living for
                            // ever.
                            for wall in wallSample far do
                                Expect.equal
                                    (castWalkTicks atlas leadBody spawn (RoomPos.at into wall))
                                    None
                                    $"{from} -> {into}: the wall at {wall.X},{wall.Y} leads nobody"

                            for exit in seams atlas into from |> List.map fst do
                                Expect.equal
                                    (castWalkTicks atlas leadBody spawn (RoomPos.at into exit))
                                    None
                                    $"{from} -> {into}: the exit at {exit.X},{exit.Y} is the ring, and no room's ground"

                        // The per-tile half, one stand per direction because
                        // each probe is an Atlas and the floods on it. The
                        // goals are the far room's own strided sample, so
                        // they are scattered over the whole room rather than
                        // gathered where a source happens to sit.
                        for birth in standingSample near |> List.truncate 1 do
                            let spawn =
                                if birth.Y < 48 then
                                    { birth with Y = birth.Y + 1 }
                                else
                                    { birth with Y = birth.Y - 1 }

                            for goal in standingSample far do
                                let source, fence = probeBeside goal

                                let atlas =
                                    leadingAcross near far spawn birth [ "probe", source ] fence

                                Expect.equal
                                    (workArea atlas (Harvest "probe")
                                     |> RoomPos.inRoom into
                                     |> Set.toList)
                                    [ goal ]
                                    $"the premise: the probe beside {goal.X},{goal.Y} is worked from that tile and no other"

                                let led = castWalkTicks atlas leadBody spawn (RoomPos.at into goal)

                                if Option.isSome led then
                                    pinned <- pinned + 1

                                Expect.equal
                                    led
                                    (walkTicks atlas "w" (Harvest "probe"))
                                    $"{from} -> {into}: the lead out of {spawn.X},{spawn.Y} and the walk from {birth.X},{birth.Y} price the tile {goal.X},{goal.Y} alike"

                Expect.isGreaterThan
                    led
                    0
                    "and the sweep really does lead somebody across: an empty one proves nothing"

                Expect.isGreaterThan
                    pinned
                    0
                    "and some far tile was pinned at a number rather than at absence, or the per-tile half proves nothing either"
            }
        ]

/// A projection over a chain of captures: the first room is the colony's own
/// with one body standing in it, the last carries its own sources, and every
/// room in the list brings its terrain and its border ring — which is what the
/// shell lays for a declared room and for the transit rooms between (ADR 0058).
/// Which rooms are in the list is the whole of what the cases below vary: a
/// corner the projection does not carry has no ring, so no chain turns in it.
let private chainedProjection (rooms: string list) (stand: Pos) =
    let captures = rooms |> List.map load
    let home = List.head captures
    let far = List.last captures

    { SpatialInfo.empty with
        RoomName = Some home.RoomName
        Rooms =
            captures
            |> List.map (fun capture ->
                capture.RoomName,
                { RoomLayer.empty with
                    Terrain = capture.Terrain
                    CreepPositions =
                        if capture.RoomName = home.RoomName then
                            Map.ofList [ "w", stand ]
                        else
                            Map.empty
                    TargetPositions =
                        if capture.RoomName = far.RoomName then
                            Map.ofList far.Sources
                        else
                            Map.empty
                })
            |> Map.ofList
        Borders =
            captures
            |> List.map (fun capture -> capture.RoomName, capture.Border)
            |> Map.ofList
        TargetKinds = far.Sources |> List.map (fun (id, _) -> id, Source) |> Map.ofList
    }
    |> AtlasFixtures.snapshotWith [ AtlasFixtures.worker "w" ]
    |> ofView

[<Tests>]
let cornerChainTests =
    testList
        "multi-hop corners on real terrain"
        [
            test "the L to W14S29 is priced round whichever corner is cheaper" {
                // #288 on the captures, and on the very pair
                // `docs/research/multihop-outposts.md` measured (tick
                // 302,850): W14S29 is two hops from W13S28 and both corners
                // are real rooms — W13S29 to the south, which `adjacent`
                // names first, and W14S28 to the west, which that survey
                // priced the cheaper. Nothing is written down here but the
                // room names: the captures decide the numbers, and what this
                // pins is that the price is the smaller of the two and never
                // the compass's by default.
                let far = load "W14S29"
                let stand = standingSample (load "W13S28") |> List.head

                let both = chainedProjection [ "W13S28"; "W13S29"; "W14S28"; "W14S29" ] stand
                let southCorner = chainedProjection [ "W13S28"; "W13S29"; "W14S29" ] stand
                let westCorner = chainedProjection [ "W13S28"; "W14S28"; "W14S29" ] stand

                Expect.equal
                    (routes both "W13S28" "W14S29")
                    [ [ "W13S28"; "W13S29"; "W14S29" ]; [ "W13S28"; "W14S28"; "W14S29" ] ]
                    "the premise: two chains of two hops, the compass's south one first"

                let mutable cheaper = 0

                for sourceId, _ in far.Sources do
                    let task = Harvest sourceId

                    match
                        walkTicks both "w" task,
                        walkTicks southCorner "w" task,
                        walkTicks westCorner "w" task
                    with
                    | Some priced, Some south, Some west ->
                        Expect.equal
                            priced
                            (min south west)
                            $"{sourceId}: the walk with both corners carried is the cheaper corner's"

                        if west < south then
                            cheaper <- cheaper + 1
                    | answers ->
                        failtest
                            $"{sourceId}: every chain of this L prices on real terrain, got {answers}"

                Expect.isGreaterThan
                    cheaper
                    0
                    "and the corner the compass names really is the dearer one on these captures, or the case proves nothing"
            }
        ]

[<Tests>]
let keeperMaskTests =
    testList
        "the keeper mask on real terrain"
        [
            // The three rooms of the chain from W15S28 to the sector Reactor's
            // room, as the server has them (ADR 0036): `AtlasSeamTests`' own
            // keeper cases run over invented plain, because the mask is keyed
            // by room name and terrain-blind, and these are the half that only
            // the capture can say. W15S26's mineral and its three sources are
            // the capture's own rows, so the declaration in `Keepers` is
            // checked against the engine here rather than trusted (#317).
            let chain = [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ]
            let keeperRoom = load "W15S26"

            // The margin rides on `ReachMargin`, which is the whole point of
            // deriving it: one plus three plus this. Five, six and seven are
            // therefore `ReachMargin` of one, two and three, and the numbers
            // this bot ships with are the middle one.
            let atlasAt reachMargin =
                let captures = chain |> List.map load

                let view =
                    { SpatialInfo.empty with
                        RoomName = Some "W15S28"
                        Rooms =
                            captures
                            |> List.map (fun capture ->
                                capture.RoomName,
                                { RoomLayer.empty with
                                    Terrain = capture.Terrain
                                })
                            |> Map.ofList
                        Borders =
                            captures
                            |> List.map (fun capture -> capture.RoomName, capture.Border)
                            |> Map.ofList
                    }
                    |> AtlasFixtures.snapshotWith []

                { view with
                    Tuning =
                        { Tuning.defaults with
                            ReachMargin = reachMargin
                        }
                }
                |> ofView

            let marginOf reachMargin =
                Tuning.keeperMargin
                    { Tuning.defaults with
                        ReachMargin = reachMargin
                    }

            // W15S26's own exit tiles on one side, as `World.linked` reads
            // them: a ring tile the capture carries whose terrain is not wall
            // and which no declared rock masks. No neighbour is needed — which
            // of this room's exits survive the mask is a fact about this room.
            let survivingExits margin (onSide: Pos -> bool) =
                keeperRoom.Border
                |> Map.toList
                |> List.filter (fun (tile, terrain) ->
                    onSide tile && terrain <> Wall && not (Keepers.masked margin "W15S26" tile))
                |> List.map fst

            test "the declaration's rocks are the engine's own, tile for tile" {
                // The one datum in `Keepers.centres` that came off a live read
                // rather than out of a committed file, made checkable: three
                // sources and a mineral, which is four of the eight centres.
                // The four lairs are not here because `capture-room.mjs` keeps
                // sources, controllers and minerals alone — widening it is
                // #316's, and until then the lairs stay a hand-read fact.
                let declared = Keepers.centresIn "W15S26" |> Set.ofList

                for _, tile in keeperRoom.Rocks do
                    Expect.isTrue
                        (Set.contains tile declared)
                        $"the rock at ({tile.X},{tile.Y}) is a declared keeper centre"

                Expect.equal
                    keeperRoom.Rocks.Length
                    4
                    "and the capture holds no rock the declaration has not got: three sources and the mineral"

                Expect.isTrue
                    (Set.contains { X = 38; Y = 7 } declared)
                    "the mineral at (38,7) above all, which is the tile every orphaned north crossing traces to"
            }

            test "the mask takes a third of W15S26's ground and the chain still crosses it" {
                // ADR 0060 decision 2's first and third acceptance criteria,
                // over the terrain the courier will actually walk. The chain is
                // three crossings, which is `Tuning.MaxHops` exactly, so there
                // is no slack for a detour round a room the mask closed.
                let raw =
                    keeperRoom.Terrain
                    |> TerrainGrid.toList
                    |> List.filter (fun (_, terrain) -> terrain <> Wall)
                    |> List.length

                Expect.equal raw 1568 "the room's walkable ground before the mask"

                Expect.equal
                    (walkableTilesIn (atlasAt 2) "W15S26" |> Set.count)
                    1005
                    "and 563 of those tiles are inside six of a rock"

                Expect.equal
                    (routes (atlasAt 2) "W15S28" "W15S25")
                    [ [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ] ]
                    "the chain to the Reactor's room, straight up the column, over the masked layer"
            }

            test "six costs the crossing nothing that five did not, and eight closes the room" {
                // The acceptance criterion ADR 0060 left `Unverified`: only
                // five was measured, and only against one lair. The bands are
                // the measurement — the north one is where the mask first
                // bites, and it does not bite at six.
                let northBand margin =
                    survivingExits margin (fun tile -> tile.Y = 0)

                let southBand margin =
                    survivingExits margin (fun tile -> tile.Y = Seam.exitEdge)

                Expect.equal
                    (northBand 0).Length
                    20
                    "the north border carries twenty exits over raw terrain"

                Expect.equal (southBand 0).Length 20 "and the south border twenty"

                for reachMargin in 1..2 do
                    Expect.equal
                        (northBand (marginOf reachMargin)).Length
                        20
                        $"the mask at {marginOf reachMargin} takes no north crossing"

                    Expect.equal
                        (southBand (marginOf reachMargin)).Length
                        20
                        $"nor any south one, so a crossing at {marginOf reachMargin} costs what one at five costs"

                Expect.equal
                    (northBand (marginOf 3)).Length
                    11
                    "seven is where it starts to cost: nine of the twenty go"

                // The count at eight is still *not* asserted here — that number
                // is #327's table's, beside the decision it belongs to — but
                // what eight does is, because ADR 0062 is read as having closed
                // it and it has not. The note this comment used to carry said
                // the chain at eight survives "because it reads the rings",
                // and that reason has moved: the band asks the ground behind
                // the landing now, and at eight the north border orphans
                // **nothing**. The room's *interior* is what is cut in two —
                // the two bands are open and no walk joins them — and no
                // landing-neighbour predicate can see a severed transit room.
                // So this is a second silent failure wearing #326's clothes,
                // and it is the one ADR 0062 lists under what it does not
                // decide.
                Expect.equal
                    (southBand (marginOf 3)).Length
                    20
                    "the south border is clear even at seven, the nearest rock being far from that row"
            }

            test
                "a margin that severs the room's middle leaves the chain standing and the price gone" {
                // The hole ADR 0062 does **not** close, pinned so that a
                // reading of that ADR cannot mistake it for closed. Eight is no
                // knob this bot ships — `Tuning.defaults` is six and
                // `RoomSeamTests`' own case above is why — so this asserts a
                // shape and no count: at a margin that cuts W15S26 across the
                // middle, every crossing the north border keeps has ground
                // beside it, `Atlas.routes` answers the full chain, and the
                // walk over it prices `None`.
                //
                // That is #243/#259's silent failure again, one layer in from
                // the landing: ADR 0062 made the band ask whether a body can
                // step **off** its landing, and nothing yet asks whether the
                // two bands of a transit room are joined to each other.
                let atlas = atlasAt 4
                let margin = marginOf 4

                let north = survivingExits margin (fun tile -> tile.Y = 0)

                Expect.isNonEmpty
                    north
                    "the premise: the ring still carries crossings at this margin"

                Expect.isEmpty
                    (north
                     |> List.filter (fun tile ->
                         List.isEmpty (adjacentWalkableIn atlas "W15S26" tile)))
                    "and not one of them is an orphan, so ADR 0062's predicate takes none of them"

                Expect.equal
                    (seams atlas "W15S25" "W15S26" |> List.length)
                    north.Length
                    "so the band is the ring, crossing for crossing"

                Expect.equal
                    (routes atlas "W15S28" "W15S25")
                    [ [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ] ]
                    "and the route search walks that band and answers the chain it always answered"

                Expect.isNone
                    (castWalkTicks
                        atlas
                        [ BodyPart.Claim; Move ]
                        { X = 29; Y = 12 }
                        (RoomPos.at "W15S25" { X = 44; Y = 6 }))
                    "while the price over that very chain is None: the room's own middle is where the walk stops"
            }


            test "a rock behind a border leaves the crossing and takes the ground it lands on" {
                // #317's stranding, on the terrain it was found over rather
                // than on the fixture's plain. The mineral at (38,7) is seven
                // from the y = 0 ring and six from the y = 1 ground, so seven
                // of the twenty north crossings survive the mask with nothing
                // behind them; the lair at (42,39) does the same to the east
                // border and orphans **every** exit it leaves.
                //
                // What is read here is the **ring** and the ground beside it,
                // one beside the other, and that is deliberate: the two cases
                // below take the band's own answer, and this one takes the
                // facts the band is built out of, so a fixture that stopped
                // exhibiting the orphan would red here rather than leave them
                // green having checked nothing. Until ADR 0062 these two
                // readings were the whole disagreement — the band saw the ring
                // and only the movers asked the ground (#317). The band asks it
                // now.
                let atlas = atlasAt 2
                let margin = marginOf 2

                let orphans onSide =
                    survivingExits margin onSide
                    |> List.filter (fun tile ->
                        List.isEmpty (adjacentWalkableIn atlas "W15S26" tile))

                Expect.equal
                    (orphans (fun tile -> tile.Y = 0) |> List.map (fun tile -> tile.X))
                    [ 37; 38; 39; 40; 41; 42; 43 ]
                    "north: seven of the twenty land a body where it can never step again"

                Expect.isEmpty
                    (orphans (fun tile -> tile.Y = Seam.exitEdge))
                    "south: none, the nearest rock being further than the margin from that row"

                let east = survivingExits margin (fun tile -> tile.X = Seam.exitEdge)

                Expect.equal east.Length 7 "east: seven exits survive the mask"

                Expect.equal
                    (orphans (fun tile -> tile.X = Seam.exitEdge) |> List.length)
                    7
                    "and all seven are orphans: the whole of that border lands a body where it can never step again"

                Expect.isEmpty (orphans (fun tile -> tile.X = 0)) "west: none"
            }

            test
                "an orphaned crossing is no crossing: the north band drops the run and keeps the rest" {
                // ADR 0062, over the terrain that generated it. The **ring**
                // answer and the **band** answer are both taken here, because
                // the whole of what that ADR changes is that they may now
                // differ: the row of twenty exits the mask leaves open carries
                // seven landings the mask has taken the ground from, and a
                // landing with no ground beside it is a tile a body arrives on
                // and never leaves.
                //
                // The direction is the one the landings belong to: W15S26's
                // y = 0 row is what a crossing **out of W15S25** lands on.
                let atlas = atlasAt 2
                let margin = marginOf 2

                let landings = seams atlas "W15S25" "W15S26" |> List.map (fun (_, there) -> there.X)

                Expect.equal
                    (survivingExits margin (fun tile -> tile.Y = 0) |> List.length)
                    20
                    "the ring still carries twenty: the mineral at (38,7) is seven from that row"

                Expect.equal
                    landings
                    [ 20; 21; 22; 23; 24; 25; 26; 27; 28; 44; 45; 46; 47 ]
                    "and the band carries thirteen: the seven at x = 37..43 are orphans and are gone"

                // The converse, said in the same breath, or the clause would
                // be indistinguishable from one that dropped the whole band:
                // every crossing left lands a body on ground it can step onto,
                // and every crossing dropped was one with none.
                for x in landings do
                    Expect.isNonEmpty
                        (adjacentWalkableIn atlas "W15S26" { X = x; Y = 0 })
                        $"the crossing at x = {x} lands on ground with a step off it"

                for x in 37..43 do
                    Expect.isTrue
                        (survivingExits margin (fun tile -> tile.Y = 0)
                         |> List.contains { X = x; Y = 0 })
                        $"the ring keeps ({x},0)"

                    Expect.isEmpty
                        (adjacentWalkableIn atlas "W15S26" { X = x; Y = 0 })
                        $"and nothing of the room's own ground lies beside it, which is why the band does not"
            }

            test "the east band is orphaned end to end, so the model answers no band at all" {
                // The border where the whole band goes. The lair at (42,39)
                // masks x = 48 for y = 33..45 and stops one tile short of
                // x = 49, so seven exits survive on the ring and not one of
                // them has ground behind it.
                //
                // The neighbour across W15S26's x = 49 column is **W14S26**
                // (`RoomName.offsetOf`; #336 corrects the name this file used
                // to print), and no capture exists for it — so its side is
                // given as open as a ring can be. Nothing on the near side is
                // therefore what closes the band, and the two readings below
                // differ in the one predicate ADR 0062 added.
                let margin = marginOf 2
                let capture = load "W15S26"

                let ringOf tile =
                    match Map.tryFind tile capture.Border with
                    | Some terrain -> terrain <> Wall && not (Keepers.masked margin "W15S26" tile)
                    | None -> false

                let groundOf tile =
                    match TerrainGrid.tryFind tile capture.Terrain with
                    | Some terrain -> terrain <> Wall && not (Keepers.masked margin "W15S26" tile)
                    | None -> false

                let ringOnly = Seam.bandBy (fun _ -> true) ringOf (fun _ -> true) "W14S26" "W15S26"

                Expect.equal
                    (ringOnly |> List.map (fun (_, there) -> there.Y))
                    [ 36; 37; 38; 39; 40; 41; 42 ]
                    "the rings alone answer seven crossings, which is what the band said before ADR 0062"

                Expect.isEmpty
                    (Seam.bandBy (fun _ -> true) ringOf groundOf "W14S26" "W15S26")
                    "and with the far room's ground asked for, none of the seven is a crossing"

                // The ground each of them would land on, read out, so the
                // emptiness above is the mask's doing and not a mis-built
                // predicate.
                for _, landing in ringOnly do
                    Expect.isEmpty
                        (tilesWithin 1 landing |> List.filter groundOf)
                        $"({landing.X},{landing.Y}) has no ground of W15S26's beside it"
            }

            test "the price's reader and the scan set's answer alike at that border, each way round" {
                // ADR 0058's invariant — the scan set and the price cannot
                // disagree about which rooms are joined — asked in **both**
                // directions, which is what ADR 0062 made a second question.
                // The case above takes `Seam.bandBy` by hand; this one takes
                // the two shipped readers, `Atlas.seams`/`Atlas.routes` off the
                // grids and `World.linked` off the border maps, and puts their
                // answers beside each other. Without it the Atlas's half of
                // ADR 0062 is pinned nowhere: the third predicate can be struck
                // out of `Atlas.routes` and every other case in this repo stays
                // green.
                //
                // The same geometry as above: W15S26's capture with its mask,
                // and W14S26 across its x = 49 column invented as open as a
                // room can be, so nothing on that side is what closes a band.
                let margin = marginOf 2
                let capture = load "W15S26"

                let openRing =
                    Map.ofList
                        [
                            for x in 0 .. Seam.exitEdge do
                                for y in 0 .. Seam.exitEdge do
                                    if x = 0 || x = Seam.exitEdge || y = 0 || y = Seam.exitEdge then
                                        { X = x; Y = y }, Plain
                        ]

                let openGround =
                    TerrainGrid.ofList
                        [
                            for x in 1 .. Seam.exitEdge - 1 do
                                for y in 1 .. Seam.exitEdge - 1 -> { X = x; Y = y }, Plain
                        ]

                let rooms =
                    [
                        "W14S26", (openRing, openGround)
                        "W15S26", (capture.Border, capture.Terrain)
                        "W15S27", ((load "W15S27").Border, (load "W15S27").Terrain)
                    ]

                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W15S27"
                        Rooms =
                            rooms
                            |> List.map (fun (room, (_, terrain)) ->
                                room,
                                { RoomLayer.empty with
                                    Terrain = terrain
                                })
                            |> Map.ofList
                        Borders =
                            rooms |> List.map (fun (room, (ring, _)) -> room, ring) |> Map.ofList
                    }
                    |> AtlasFixtures.snapshotWith []
                    |> ofView

                let world =
                    { World.empty with
                        Rooms =
                            rooms
                            |> List.map (fun (room, (ring, terrain)) ->
                                room,
                                { RoomFacts.empty with
                                    Border = ring
                                    Layer =
                                        { RoomLayer.empty with
                                            Terrain = terrain
                                        }
                                })
                            |> Map.ofList
                    }

                let linked = World.linked margin world

                // Out of the keeper room the crossing stands: the landings are
                // W14S26's invented plain. Into it not one of the seven does.
                Expect.isNonEmpty
                    (seams atlas "W15S26" "W14S26")
                    "out of the keeper room the Atlas answers a band"

                Expect.isTrue
                    (linked "W15S26" "W14S26")
                    "and the world says the same rooms are joined"

                Expect.isEmpty
                    (seams atlas "W14S26" "W15S26")
                    "into it the Atlas answers none: every landing the mask leaves is an orphan"

                Expect.isFalse (linked "W14S26" "W15S26") "and the world says the same"

                // The table in front of it answers identically, and this pair
                // is the case worth checking it on: `linkedBy` keys the
                // **ordered** pair (`docs/research/cpu-headroom.md`'s candidate
                // 2), and a table keyed on an unordered one would answer `true`
                // backwards over the one border in this repo where the two
                // directions genuinely differ — the mask's orphaned east band
                // (#326, ADR 0062).
                let reaches = World.linkedBy margin world

                Expect.isTrue (reaches "W15S26" "W14S26") "the table agrees out of the keeper room"

                Expect.isFalse
                    (reaches "W14S26" "W15S26")
                    "and does not reuse that answer backwards"

                // The shell's own table (`JoinTable`, `World.linkedRecalling`):
                // one entry per ordered pair under the margin, read on the
                // second ask, and filed only for a pair the world holds both
                // rooms of — an unheld room is joined to nothing and files
                // nothing, because whether a room is held is this tick's fact
                // and the table outlives the tick.
                let table = JoinTable()
                let tabled = World.linkedRecalling table margin world

                Expect.isTrue (tabled "W15S26" "W14S26") "the table's first answer is the terrain's"
                Expect.isFalse (tabled "W14S26" "W15S26") "and so is the backward one"
                Expect.equal table.Count 2 "one entry per ordered pair asked"
                Expect.isTrue (tabled "W15S26" "W14S26") "asked again, it reads the entry"
                Expect.equal table.Count 2 "and files nothing new"

                Expect.isFalse
                    (tabled "W15S26" "W16S26")
                    "a room the world does not hold is joined to nothing"

                Expect.equal table.Count 2 "and is never filed"

                // Asked twice, because a table that answered once and drifted
                // would be worse than no table: the second reading is the
                // stored row and it has to be the same row.
                Expect.isTrue
                    (reaches "W15S26" "W14S26")
                    "and a second reading of a stored pair is the same"

                Expect.isFalse (reaches "W14S26" "W15S26") "in both directions"

                // And the chain the search builds off that relation, which is
                // the reader ADR 0062's predicate reaches through and the one
                // nothing else in this repo covers.
                Expect.equal
                    (routes atlas "W15S27" "W14S26")
                    [ [ "W15S27"; "W15S26"; "W14S26" ] ]
                    "a chain runs out of W15S27 into W14S26 by way of the keeper room"

                Expect.equal
                    (routes atlas "W14S26" "W15S27")
                    []
                    "and none runs back, which is the whole of what a directed band is"

                // What a colony would have done with that. `routesBy` expands
                // away from **home** and nowhere else, so every reader that
                // narrows a declaration was asking the half that says yes: a
                // colony at W15S27 declaring anything in W14S26 would have been
                // admitted, its reserver and its miners hired and walked out,
                // while `haulRoundTripTicks` asked `routes` in the direction
                // above that answers `[]` and priced `None`. That is #243's
                // silent failure in the direction ADR 0062 had just taught the
                // model to see, and `Declaration.routable` is where it is
                // closed: a declaration buys a **round trip**.
                Expect.isTrue
                    (RoomName.routesBy linked Tuning.defaults.MaxHops "W15S27" "W14S26"
                     |> List.isEmpty
                     |> not)
                    "the premise: the outbound half, which admission asked alone, says yes"

                Expect.isEmpty
                    (RoomName.routesBy linked Tuning.defaults.MaxHops "W14S26" "W15S27")
                    "and the way home, which it did not ask, has no chain at all"

                Expect.isFalse
                    (Declaration.routable linked Tuning.defaults.MaxHops "W15S27" "W14S26")
                    "so W15S27 may not declare W14S26: the way out is open and the way home is not"
            }
        ]

[<Tests>]
let reclaimerRelayTests =
    testList
        "the re-claimer's cadence, over the terrain it will actually walk"
        [
            test
                "the walk to the Reactor's ring and the overlapping cadence fall out of the terrain" {
                // ADR 0060 decision 3's own number, re-derived here off the
                // committed captures instead of being written into `Tuning`
                // (ADR 0036: real terrain is a counterexample generator, and a
                // constant measured once is a constant nothing re-checks).
                //
                // The origin is the ADR's own: W15S28's Thorium seat at
                // (29,12), which is where it measured **154 steps and three
                // room transitions** from. That is the ADR's measurement and
                // not a [[lead]]'s: a lead is priced from beside the spawn, and
                // these captures are terrain only — no spawn of ours stands in
                // any of them — so what is pinned here is the *walk* the live
                // lead will be built on, from the tile the ADR used, to a tile
                // adjacent to the reactor at (44,6).
                //
                // The arithmetic the answer feeds, which is the whole of the
                // cadence and is why no interval is written down:
                //
                //   lead      = 3 ticks a part × 2 parts + the walk
                //   relief at = lead + Tuning.ReclaimerOverlap (25) of life
                //   cadence   = CREEP_CLAIM_LIFE_TIME (600) − relief at
                //
                // `QuotaReserverTests` pins the cast threshold and
                // `ErrandTests` pins admission at arrival; this real-terrain
                // case ensures both are fed the same overlap.
                //
                // ADR 0057's 300 was the six-hop home's; this is W15S28's.
                let chain = [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ]
                let captures = chain |> List.map load

                let flatten tile = if tile = Wall then Wall else Plain

                let asPlainGrid grid =
                    grid |> TerrainGrid.map (fun _ tile -> flatten tile)

                let asPlainRing ring =
                    ring |> Map.map (fun _ tile -> flatten tile)

                let courier =
                    { AtlasFixtures.creepWith
                          "courier-route"
                          0
                          (List.replicate 20 Carry @ List.replicate 10 Move) with
                        Thorium = Tuning.defaults.ReactorLoad
                        FreeCapacity = 1
                    }

                let atlasWith terrainOf borderOf =
                    { SpatialInfo.empty with
                        RoomName = Some "W15S28"
                        Rooms =
                            captures
                            |> List.map (fun capture ->
                                capture.RoomName,
                                { RoomLayer.empty with
                                    Terrain = terrainOf capture
                                    CreepPositions =
                                        if capture.RoomName = "W15S28" then
                                            Map.ofList [ courier.Name, { X = 28; Y = 11 } ]
                                        else
                                            Map.empty
                                    TargetPositions =
                                        if capture.RoomName = "W15S25" then
                                            Map.ofList [ "reactor", { X = 44; Y = 6 } ]
                                        else
                                            Map.empty
                                })
                            |> Map.ofList
                        Borders =
                            captures
                            |> List.map (fun capture -> capture.RoomName, borderOf capture)
                            |> Map.ofList
                        TargetKinds = Map.ofList [ "reactor", Structure BuiltKind.Other ]
                    }
                    |> AtlasFixtures.snapshotWith [ courier ]
                    |> ofView

                let atlas =
                    atlasWith (fun capture -> capture.Terrain) (fun capture -> capture.Border)

                let stepAtlas =
                    atlasWith (fun capture -> asPlainGrid capture.Terrain) (fun capture ->
                        asPlainRing capture.Border)

                let body = [ BodyPart.Claim; Move ]

                let walk =
                    castWalkTicks
                        atlas
                        body
                        { X = 29; Y = 12 }
                        (RoomPos.at "W15S25" { X = 44; Y = 6 })

                match walk with
                | None -> failtest "the chain the Atlas answers with has to price this walk"
                | Some ticks ->
                    Expect.equal
                        (walkTicks stepAtlas courier.Name (Refill("reactor", Thorium)))
                        (Some 151)
                        "151 loaded movement steps, one tick each: a 500-unit load (#354) is half of this body's carry, so the walk out is not fatigued"

                    let loadedWalk = walkTicks atlas courier.Name (Refill("reactor", Thorium))

                    // 159 and not 318: at `Tuning.ReactorLoad` 500 (#354) the
                    // 20-Carry courier is half empty, and half a carry is no
                    // fatigue over 10 Move parts — the loaded leg costs one
                    // tick a step, as the empty one does. The load was chosen
                    // for the Reactor's 1,000-unit store; this is the second
                    // thing it bought.
                    Expect.equal loadedWalk (Some 159) "the loaded body clock over the real terrain"

                    // A `[Claim; Move]` body is one fatigue part against one
                    // Move, so it walks a plain tile in one tick and pays
                    // extra for a swamp. **160 ticks** against the ADR's 154
                    // *steps* — two different units, and the gap is not six
                    // extra steps: it is what the swamp on the way and the
                    // [[keeper margin]]'s detour charge over the masked layer,
                    // which is the reason to price the walk rather than to
                    // write the ADR's number down.
                    Expect.equal
                        ticks
                        160
                        "the walk from W15S28's Thorium seat to the Reactor's ring"

                    let lead = Engine.spawnTicksPerPart * List.length body + ticks
                    let relief = lead + Tuning.defaults.ReclaimerOverlap

                    Expect.equal lead 166 "the [[lead]]: six ticks of oven and the walk"

                    Expect.equal
                        relief
                        191
                        "the incumbent leaves the row at its lead plus the handover window"

                    Expect.equal
                        (relief - lead)
                        25
                        "the relief reaches the ring with 25 predecessor ticks left"

                    Expect.equal
                        (Engine.claimLifetime - relief)
                        409
                        "so the cast cadence is 409, derived rather than configured"

                    // The courier's separate clock (#319, re-derived by
                    // #354). What #354 took out of this block is the idea that
                    // a cadence meters the delivery: the Reactor burns exactly
                    // 1 T a tick against a 1,000-unit store, so supply is
                    // metered by the draw gate on that store
                    // (`Facts.reactorTakesALoad`) and `DeliveryInterval` only
                    // keeps a body in the row. The old arithmetic here read
                    // `ReactorLoad - DeliveryInterval` as "buffer", which is
                    // two units subtracted from each other — Thorium less
                    // ticks — and it is what let 999 T leave home every 636
                    // ticks against a 1 T/tick burn.
                    //
                    // The three clocks that do hold, over this room's real
                    // terrain:
                    let cycle = Tuning.defaults.ReactorLoad

                    Expect.equal
                        cycle
                        500
                        "one load is 500 Reactor-ticks, so the gate reopens 500 ticks after it closes"

                    Expect.equal
                        (Engine.reactorCapacity - Tuning.defaults.ReactorLoad - 159)
                        341
                        "the store at the courier's arrival: what the gate admits, less the loaded leg it drains through"

                    Expect.equal
                        (159 * Tuning.defaults.MineContactAgeing)
                        477
                        "the loaded leg's TTL cost: below the 1,000 cliff each elapsed tick spends three of life"
            }
        ]
