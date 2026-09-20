/// Seams and cross-room walks on real terrain.
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
                // The test below recomputes the band from the same two
                // rings and would follow a narrower recapture down without
                // a word; this is the line that would go red instead.
                for border in borders do
                    let atlas = acrossFrom (load border.From) (load border.To)

                    Expect.hasLength
                        (seams atlas border.From border.To)
                        border.Exits
                        $"{border.From} -> {border.To}: the band the two captures hold"
            }

            test "the band is every exit the two captures agree on, and no other" {
                // Which tiles, recomputed off the committed captures.
                for border in borders do
                    let near = load border.From
                    let far = load border.To
                    let edge = $"{border.From} -> {border.To}"

                    // Every capture walls its corners, so this line changes
                    // nothing today; a capture that did not would fail the
                    // width test above rather than this one.
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
                // Symmetry is these captures' fact, not the model's rule:
                // `seams A B` and `seams B A` ask two rooms' ground and are
                // free to differ (W15S26's east border does, in the keeper
                // list). No pair in `borders` orphans a landing over raw
                // terrain. If a re-capture ever did, name the direction
                // rather than relax the equality.
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
                // W15S25 is four rooms away.
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
                // Chebyshev distance on the world grid, the neighbour's
                // coordinates shifted a room's width into this room's
                // frame. Less exactly one tile: the creep pays for stepping
                // onto the exit tile, and the engine relocates it onto the
                // landing tile at the end of that tick for no tick at all.
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
                // A price without a step is a creep parked on an assignment
                // for life (#142). The step is one of this room's own
                // tiles: its ground, or an exit of the band toward the
                // target's room.
                let mutable stepped = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        // The band is read off the two rings, so the tile
                        // this Atlas stands its creep on is never looked at.
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
                // The lead's cross-room clock and the Matcher's are two
                // readers of one join, whatever the lead computes from
                // (#169's far-leg memo). Two readings: over a real source's
                // Seats the Matcher answers a minimum, so the per-tile leads
                // are minimised to meet it; over a probe source fenced to
                // one Seat (`probeBeside`) it is one named tile, where a
                // seeding right at the cheapest tile and wrong at a dearer
                // one would show.
                let mutable led = 0
                let mutable pinned = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        for birth in standingSample near do
                            // A neighbour of the birth tile, inside the grid;
                            // the spawner's own tile is fenced off and never walked.
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

                                // The body-blind area is the set the far leg
                                // floods into; `workAreaFor` hands a creep
                                // only its own room's tiles.
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

                            // A wall and a ring tile are both absent, never a
                            // zero that would leave a creep counted living for ever.
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
                        // each probe is an Atlas and the floods on it.
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
/// with one body standing in it, the last carries its own sources, every
/// room brings its terrain and its ring. A corner the projection does not
/// carry has no ring, so no chain turns in it.
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
                // The pair `docs/research/multihop-outposts.md` measured
                // (tick 302,850): W13S29 to the south, which `adjacent`
                // names first, and W14S28 to the west, which the survey
                // priced the cheaper. The captures decide the numbers.
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
            // The chain from W15S28 to the sector Reactor's room as the
            // server has it; `AtlasSeamTests`' keeper cases run over
            // invented plain. W15S26's mineral and sources are the
            // capture's own rows, so `Keepers` is checked against the
            // engine here rather than trusted (#317).
            let chain = [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ]
            let keeperRoom = load "W15S26"

            // The margin is one plus three plus `ReachMargin`: five, six and
            // seven are `ReachMargin` of one, two and three, and the bot
            // ships the middle one.
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
            // them: not wall and not masked. No neighbour is needed.
            let survivingExits margin (onSide: Pos -> bool) =
                keeperRoom.Border
                |> Map.toList
                |> List.filter (fun (tile, terrain) ->
                    onSide tile && terrain <> Wall && not (Keepers.masked margin "W15S26" tile))
                |> List.map fst

            test "the declaration's rocks are the engine's own, tile for tile" {
                // Three sources and a mineral, four of the eight centres.
                // The four lairs are not here because `capture-room.mjs`
                // keeps sources, controllers and minerals alone (widening it
                // is #316's), so the lairs stay a hand-read fact.
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
                // Three crossings is `Tuning.MaxHops` exactly: no slack for
                // a detour round a room the mask closed.
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
                // Only five was measured live, and only against one lair.
                // The north band is where the mask first bites.
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

                // The count at eight is #327's table's. At eight the north
                // border orphans nothing: the room's *interior* is cut in
                // two, and no landing-neighbour predicate can see a severed
                // transit room (the case below).
                Expect.equal
                    (southBand (marginOf 3)).Length
                    20
                    "the south border is clear even at seven, the nearest rock being far from that row"
            }

            test
                "a margin that severs the room's middle leaves the chain standing and the price gone" {
                // Eight is no knob this bot ships, so this asserts a shape
                // and no count: every crossing the north border keeps has
                // ground beside it, `Atlas.routes` answers the full chain,
                // and the walk over it prices `None`. Nothing yet asks
                // whether the two bands of a transit room are joined to
                // each other (#243/#259's silent failure, one layer in).
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
                // #317's stranding on the terrain it was found over. The
                // mineral at (38,7) is seven from the y = 0 ring and six
                // from the y = 1 ground; the lair at (42,39) orphans every
                // east exit it leaves. This case reads the ring and the
                // ground beside it, the facts the band is built from, so a
                // fixture that stopped exhibiting the orphan would red here.
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
                // The ring answer and the band answer both, since they may
                // differ. W15S26's y = 0 row is what a crossing out of
                // W15S25 lands on.
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

                // The converse, or the clause would be indistinguishable
                // from one that dropped the whole band.
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
                // The lair at (42,39) masks x = 48 for y = 33..45 and stops
                // one tile short of x = 49. The neighbour across that column
                // is W14S26 (#336), uncaptured, so its side is given as open
                // as a ring can be.
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

                // The ground each would land on, so the emptiness above is
                // the mask's doing and not a mis-built predicate.
                for _, landing in ringOnly do
                    Expect.isEmpty
                        (tilesWithin 1 landing |> List.filter groundOf)
                        $"({landing.X},{landing.Y}) has no ground of W15S26's beside it"
            }

            test "the price's reader and the scan set's answer alike at that border, each way round" {
                // The two shipped readers, `Atlas.seams`/`Atlas.routes` off
                // the grids and `World.linked` off the border maps, side by
                // side in both directions. Without it the third predicate
                // can be struck out of `Atlas.routes` and every other case
                // stays green. Same geometry as above.
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

                // Out of the keeper room the landings are W14S26's invented
                // plain; into it not one of the seven stands.
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

                // `linkedBy` keys the **ordered** pair; a table keyed on an
                // unordered one would answer `true` backwards over the one
                // border in this repo where the two directions differ.
                let reaches = World.linkedBy margin world

                Expect.isTrue (reaches "W15S26" "W14S26") "the table agrees out of the keeper room"

                Expect.isFalse
                    (reaches "W14S26" "W15S26")
                    "and does not reuse that answer backwards"

                // `JoinTable`: one entry per ordered pair, filed only for a
                // pair the world holds both rooms of, because whether a room
                // is held is this tick's fact and the table outlives the tick.
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

                // Asked twice: the second reading is the stored row.
                Expect.isTrue
                    (reaches "W15S26" "W14S26")
                    "and a second reading of a stored pair is the same"

                Expect.isFalse (reaches "W14S26" "W15S26") "in both directions"

                // The chain the search builds off that relation.
                Expect.equal
                    (routes atlas "W15S27" "W14S26")
                    [ [ "W15S27"; "W15S26"; "W14S26" ] ]
                    "a chain runs out of W15S27 into W14S26 by way of the keeper room"

                Expect.equal
                    (routes atlas "W14S26" "W15S27")
                    []
                    "and none runs back, which is the whole of what a directed band is"

                // `routesBy` expands away from home only, so admission asked
                // the half that says yes while `haulRoundTripTicks` asked
                // the other and priced `None`; `Declaration.routable` asks both.
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
                // The cadence re-derived off the committed captures rather
                // than written into `Tuning`. The origin is W15S28's Thorium
                // seat at (29,12), where 154 steps and three room
                // transitions were measured; these captures hold no spawn of
                // ours, so what is pinned is the walk a lead is built on.
                //
                //   lead      = 3 ticks a part × 2 parts + the walk
                //   relief at = lead + Tuning.ReclaimerOverlap (25) of life
                //   cadence   = CREEP_CLAIM_LIFE_TIME (600) − relief at
                //
                // `QuotaReserverTests` pins the cast threshold and
                // `ErrandTests` admission at arrival; both must be fed the
                // same overlap.
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

                    // 159 and not 318: at `Tuning.ReactorLoad` 500 the
                    // 20-Carry courier is half empty, and half a carry is no
                    // fatigue over 10 Move parts.
                    Expect.equal loadedWalk (Some 159) "the loaded body clock over the real terrain"

                    // The leg as the delivery draw's gate prices it (#373):
                    // from beside W15S28's Storage at (17,29), carrying
                    // `Tuning.ReactorLoad`, for the courier and for the
                    // `11W 12C 12M` worker that took two loads live. The
                    // courier is at fatigue parity under 500; the worker is
                    // 21 against 12, two ticks a plain tile.
                    let storage = RoomPos.at "W15S28" { X = 17; Y = 29 }
                    let target = RoomPos.at "W15S25" { X = 44; Y = 6 }

                    let worker =
                        AtlasFixtures.creepWith
                            "worker-route"
                            0
                            (List.replicate 11 Work
                             @ List.replicate 12 Carry
                             @ List.replicate 12 Move)

                    let legOf (creep: CreepInfo) =
                        walkTicksFrom
                            atlas
                            (Fabot.Core.Grid.factorCarrying creep Tuning.defaults.ReactorLoad)
                            storage
                            target

                    Expect.equal
                        (legOf courier)
                        (Some 196)
                        "the courier's loaded leg from the Storage: 588 ticks of life at the contact rate, inside a fresh 1,500"

                    Expect.equal
                        (legOf worker)
                        (Some 385)
                        "the worker's loaded leg from the same Storage: 1,155 of life at the contact rate — the dead worker drew with 920 left"

                    Expect.equal
                        (walkTicksFrom
                            atlas
                            (Fabot.Core.Grid.factorCarrying worker 0)
                            storage
                            target)
                        (Some 196)
                        "the worker's empty walk, which is what the gate read for it before #373: half the leg it went on to make"

                    // 160 ticks against the measured 154 *steps*: the gap
                    // is the swamp on the way and the keeper margin's detour.
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

                    // The courier's clock: supply is metered by the draw gate
                    // on the Reactor's store (`Facts.reactorTakesALoad`), not
                    // by a cadence. The old `ReactorLoad - DeliveryInterval`
                    // "buffer" subtracted ticks from Thorium and let 999 T
                    // leave home every 636 ticks against a 1 T/tick burn (#354).
                    // The three clocks that hold over real terrain:
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
