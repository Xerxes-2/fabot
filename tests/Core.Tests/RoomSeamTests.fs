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
                // Adjacency has no preferred end: the same crossing, asked
                // from the other room, is the same tiles the other way round.
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
                                        match Map.tryFind tile near.Terrain with
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
                    |> Map.toList
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

                // What eight does is *not* asserted here, and the reason is
                // worth the line: measured over this capture the room's ground
                // is cut in two at eight, and `routes` goes on answering with a
                // chain — because it reads the rings, where ten crossings
                // survive. That is #326's silent failure and not a fact about
                // the margin, so the number lives in #327's table beside the
                // decision it belongs to.
                Expect.equal
                    (southBand (marginOf 3)).Length
                    20
                    "the south border is clear even at seven, the nearest rock being far from that row"
            }

            test "a rock behind a border leaves the crossing and takes the ground it lands on" {
                // #317's stranding, on the terrain it was found over rather
                // than on the fixture's plain. The mineral at (38,7) is seven
                // from the y = 0 ring and six from the y = 1 ground, so seven
                // of the twenty north crossings survive the mask with nothing
                // behind them; the lair at (42,39) does the same to the east
                // border and orphans **every** exit it leaves. The band is a
                // fact about two rings and cannot see this, which is why the
                // movers ask the far side's ground themselves and why what the
                // band itself should say is #326's.
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
                    "and all seven are orphans, so W15S26 -> W16S26 is a join no body can use"

                Expect.isEmpty (orphans (fun tile -> tile.X = 0)) "west: none"
            }
        ]

[<Tests>]
let reclaimerRelayTests =
    testList
        "the re-claimer's cadence, over the terrain it will actually walk"
        [
            test
                "the walk to the Reactor's ring is the 154 ADR 0060 measured, and the cadence falls out of it" {
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
                //   lead    = 3 ticks a part × 2 parts + the walk
                //   cast at = the incumbent's life falling to that lead
                //   cadence = CREEP_CLAIM_LIFE_TIME (600) − the lead
                //
                // And **no overlap term**, because there is no overlap to have
                // (#318): `Reclaim` admits one holder, counted at the
                // candidate's arrival (ADR 0026), so the relief takes the seat
                // only once the incumbent can no longer outlive its walk — the
                // relay hands over at death and the seat gaps about a tick.
                // `ErrandTests` pins that half at `decide` level.
                //
                // ADR 0057's 300 was the six-hop home's; this is W15S28's.
                let chain = [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ]
                let captures = chain |> List.map load

                let atlas =
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
                    |> ofView

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

                    Expect.equal lead 166 "the [[lead]]: six ticks of oven and the walk"

                    // The cadence, which is the ticket's number and is nobody's
                    // constant: the incumbent leaves the living census at its
                    // own lead, so one cast follows another by a life less that
                    // lead.
                    Expect.equal
                        (Engine.claimLifetime - lead)
                        434
                        "so the cadence is 434 — ADR 0060 decision 3's ~420, derived off this Atlas's walk rather than asserted"
            }
        ]
