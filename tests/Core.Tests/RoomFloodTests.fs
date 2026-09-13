/// The Atlas flood settled on demand, and the band read bounded by the
/// best sum (ADR 0029).
module Fabot.Core.Tests.RoomFloodTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

[<Tests>]
let onDemandFloodTests =
    testList
        "atlas flood settled on demand"
        [
            test "a walk read off the resumable memo is the whole flood's, tile for tile" {
                // The oracle, and the only case here that does not compare
                // the memo against itself. `haulRoundTripTicks` runs its own
                // floods and settles them whole (ADR 0012, and #174 left it
                // that way deliberately), while `walkTicks` reads the tick's
                // per-creep memo, which #174 made resumable. Point them at
                // one room, one origin and one goal set and they must
                // answer the same number: same weights, same Walk pricing
                // (ADR 0029), both traffic-blind.
                //
                // The two are made to line up by the fixture and not by a
                // special case: a Carry-less body is priced identically
                // loaded and empty, so the round trip is exactly twice the
                // one-way walk; and a spawn structure is an obstacle, so
                // the Refill Work Area at range 1 is exactly the sink's
                // adjacent walkable tiles. Real terrain is what makes it
                // worth asserting — the walk detours around walls and pays
                // swamp, and a flood stopped a pop too early would answer a
                // route it had not finished finding (ADR 0036).
                let body = [ Work; Move ]

                for roomName, spawn, _ in floodRooms do
                    let capture = load roomName
                    let task = Refill("spawn-1", Energy)

                    let stands =
                        capture.Terrain
                        |> Map.toList
                        |> List.choose (fun (tile, terrain) ->
                            if terrain <> Wall && (tile.X + tile.Y) % 7 = 0 then
                                Some tile
                            else
                                None)

                    Expect.isGreaterThan
                        (List.length stands)
                        100
                        $"{roomName}: tiles worth sweeping"

                    let mutable walked = 0

                    for stand in stands do
                        let creep = AtlasFixtures.creepWith "w" 0 body
                        let atlas = standingIn capture spawn creep stand

                        let onDemand = walkTicks atlas creep.Name task

                        let whole =
                            haulRoundTripTicks
                                atlas
                                body
                                (RoomPos.at roomName stand)
                                (RoomPos.at roomName spawn)

                        match onDemand, whole with
                        | Some one, Some round ->
                            walked <- walked + 1

                            Expect.equal
                                (one * 2)
                                round
                                $"{roomName} from {stand}: the memo's walk is the whole flood's"
                        | None, None -> () // no route: absent from both, as ADR 0004 has it
                        | one, round ->
                            failtest
                                $"{roomName} from {stand}: memo {one} and whole flood {round} disagree on whether there is a walk"

                    Expect.isGreaterThan walked 50 $"{roomName}: walks actually compared"
            }

            test "a tile prices the same whichever order the room is read in" {
                // The memo is shared: every query pricing a creep this tick
                // reads one flood, so what it answers must not depend on
                // what was asked of it before (`Floods`). `travelCostWithin`
                // takes the tiles the caller names, so one call is one
                // tile's distance out of that flood, and a flood pushed out
                // tile by tile, a flood asked from the far end in, and a
                // flood asked for one tile and nothing else must agree
                // everywhere. Dijkstra settles a tile for good when it
                // leaves the heap, so this is a property and not a
                // coincidence.
                for roomName, spawn, stand in floodRooms do
                    let capture = load roomName

                    for bodyName, creep in floodBodies do
                        let where = $"{roomName}/{bodyName}"
                        let tiles = nearestFirst stand capture
                        let atlasOf () = standingIn capture spawn creep stand

                        let priced atlas tile =
                            travelCostWithin
                                atlas
                                creep.Name
                                (Set.singleton (RoomPos.at roomName tile))

                        Expect.isGreaterThan
                            (List.length tiles)
                            2000
                            $"{where}: a room worth sweeping"

                        let onDemand =
                            let atlas = atlasOf ()
                            tiles |> List.map (priced atlas)

                        let farEndFirst =
                            let atlas = atlasOf ()
                            tiles |> List.rev |> List.map (priced atlas) |> List.rev

                        let cold =
                            tiles
                            |> List.indexed
                            |> List.filter (fun (index, _) -> index % coldStride = 0)
                            |> List.map (fun (_, tile) -> priced (atlasOf ()) tile)

                        // The fourth order, and the one that makes this a
                        // comparison against the whole flood rather than
                        // against another resumable one: a tile nothing
                        // reaches settles the heap to exhaustion, so every
                        // read after it comes off exactly the flood the
                        // pre-#174 code laid in one go. Asking it first is
                        // the ticket's "whole flood, then on demand".
                        let wholeFirst =
                            let atlas = atlasOf ()

                            Expect.isNone
                                (priced atlas offTheGround)
                                $"{where}: the border ring is reached by nothing, so asking drains the flood"

                            tiles |> List.map (priced atlas)

                        Expect.isGreaterThan
                            (onDemand |> List.choose id |> List.length)
                            1500
                            $"{where}: a room the creep can price at all"

                        Expect.equal
                            onDemand
                            wholeFirst
                            $"{where}: a flood pushed out tile by tile prices what one settled whole does"

                        Expect.equal
                            onDemand
                            farEndFirst
                            $"{where}: the read order does not move a single tile's price"

                        Expect.equal
                            cold
                            (onDemand
                             |> List.indexed
                             |> List.filter (fun (index, _) -> index % coldStride = 0)
                             |> List.map snd)
                            $"{where}: a flood asked one tile answers what a flood asked every tile does"
            }

            test "the step toward a tile is the one the whole flood leaves, read early or late" {
                // The other half of a flood is its predecessor chain, and it
                // is read *after* the goal settles: every tile of a cheapest
                // path is strictly cheaper than the goal, so all of them
                // left the heap first. `firstStep` is the reader, and a
                // stale parent would move its answer without moving a single
                // price — a creep walking one way while ranked another
                // (ADR 0008, #142). Both routing pricings are swept: the
                // Resolver compares them and blames the difference on
                // traffic (ADR 0018, ADR 0030).
                for roomName, spawn, stand in floodRooms do
                    let capture = load roomName
                    let loaded = project capture spawn None
                    let task = Harvest(List.head loaded.SourceIds)
                    let creep = AtlasFixtures.worker "w"

                    let goals =
                        nearestFirst stand capture
                        |> List.indexed
                        |> List.filter (fun (index, _) -> index % 5 = 0)
                        |> List.map snd

                    let atlasOf () = standingIn capture spawn creep stand

                    let steps step =
                        let onDemand =
                            let atlas = atlasOf ()
                            goals |> List.map (step atlas)

                        let farEndFirst =
                            let atlas = atlasOf ()
                            goals |> List.rev |> List.map (step atlas) |> List.rev

                        let cold = goals |> List.map (fun goal -> step (atlasOf ()) goal)

                        // The chain against a flood settled whole, which is
                        // the only order here that is not the memo compared
                        // with itself: a goal nothing reaches drains the
                        // heap through this very reader, so the steps that
                        // follow are walked back down the predecessor grid
                        // the pre-#174 flood left. The drain has to answer
                        // absent, or it has not drained.
                        let wholeFirst =
                            let atlas = atlasOf ()

                            Expect.isNone
                                (step atlas offTheGround)
                                $"{roomName}: no step toward a tile the flood never reaches"

                            goals |> List.map (step atlas)

                        onDemand, farEndFirst, cold, wholeFirst

                    let routes =
                        [
                            "priced",
                            steps (fun atlas goal ->
                                firstStep
                                    atlas
                                    creep.Name
                                    task
                                    (Set.singleton (RoomPos.at roomName goal))
                                |> Option.map RoomPos.pos)
                            "traffic-blind",
                            steps (fun atlas goal ->
                                firstStepIgnoringTraffic
                                    atlas
                                    creep.Name
                                    task
                                    (Set.singleton (RoomPos.at roomName goal))
                                |> Option.map RoomPos.pos)
                        ]

                    for pricing, (onDemand, farEndFirst, cold, wholeFirst) in routes do
                        let where = $"{roomName}/{pricing}"

                        Expect.isGreaterThan
                            (onDemand |> List.choose id |> List.length)
                            100
                            $"{where}: a room the creep can step into at all"

                        Expect.equal
                            onDemand
                            wholeFirst
                            $"{where}: a chain read off a resumable flood is the chain a whole one leaves"

                        Expect.equal
                            onDemand
                            farEndFirst
                            $"{where}: the read order does not move a single step"

                        Expect.equal
                            onDemand
                            cold
                            $"{where}: a chain read off a resumed flood is the chain a fresh one leaves"
            }

            test "the walk resumes across the Tasks that share one flood" {
                // The third pricing (ADR 0029): the clock, whose reader takes
                // a Task rather than a tile, so it is the Tasks of a room
                // that ask one flood for one creep in one tick. Asked in
                // either order, or each on a flood of its own, every Task
                // must answer the same walk — the memo's own promise
                // (`Floods`), which #174 made depend on where the flood
                // stopped.
                for roomName, spawn, stand in floodRooms do
                    let capture = load roomName
                    let loaded = project capture spawn None
                    let creep = AtlasFixtures.worker "w"

                    let tasks =
                        [
                            for id in loaded.SourceIds -> Harvest id
                            for id in Option.toList loaded.ControllerId -> Upgrade id
                            yield Refill("spawn-1", Energy)
                        ]

                    let atlasOf () = standingIn capture spawn creep stand
                    let walked atlas task = walkTicks atlas creep.Name task

                    Expect.isGreaterThan (List.length tasks) 2 $"{roomName}: Tasks worth sweeping"

                    let apiece = tasks |> List.map (fun task -> walked (atlasOf ()) task)

                    // Three lists of absences would agree with each other
                    // too, so the walks have to be walks: a flood that
                    // answered nothing at all would satisfy every equality
                    // below and pin nothing.
                    Expect.isGreaterThan
                        (apiece |> List.choose id |> List.length)
                        2
                        $"{roomName}: Tasks the creep can actually walk to"

                    let inOrder =
                        let atlas = atlasOf ()
                        tasks |> List.map (walked atlas)

                    let reversed =
                        let atlas = atlasOf ()
                        tasks |> List.rev |> List.map (walked atlas) |> List.rev

                    Expect.equal
                        inOrder
                        apiece
                        $"{roomName}: a Task asked second walks what it walks asked alone"

                    Expect.equal
                        reversed
                        apiece
                        $"{roomName}: nor does asking them the other way round"
            }

            test "a tile nothing reaches is unreachable and not merely unsettled" {
                // The trap #174 introduces and no type can catch: `unreached`
                // means "nothing gets here" in a whole flood and "nobody has
                // asked yet" in a resumed one, so a reader that answered off
                // the grid before settling would call a reachable tile
                // unreachable — and an unpriceable Task is a creep that never
                // works (ADR 0004). Every wall of a real room is a case, and
                // so is the ring around it.
                let capture = load "W12S28"
                let creep = AtlasFixtures.worker "w"
                let stand = { X = 24; Y = 24 }
                let atlas = standingIn capture { X = 12; Y = 40 } creep stand
                let here tile = RoomPos.at capture.RoomName tile

                let walls =
                    capture.Terrain
                    |> Map.toList
                    |> List.choose (fun (tile, terrain) ->
                        if terrain = Wall then Some tile else None)

                Expect.isNonEmpty walls "a captured room has walls"

                Expect.isNone
                    (travelCostWithin atlas creep.Name (Set.singleton (here offTheGround)))
                    "the border ring is no tile of the projection's ground"

                Expect.isNone
                    (travelCostWithin atlas creep.Name (Set.singleton (here { X = 60; Y = 3 })))
                    "a tile off the fifty-by-fifty grid is unpriceable, not an exception (ADR 0004)"

                for wall in walls do
                    Expect.isNone
                        (travelCostWithin atlas creep.Name (Set.singleton (here wall)))
                        $"a wall at {wall} is reachable by nothing"

                // After all of that: an absent answer drains the flood, and
                // the reads that follow one must still answer — the failure
                // a shortcut that gave up on absence would hide. The tile
                // the creep stands on is left out of both, because
                // `pricedPathTo` answers that one before it asks the flood
                // anything (`Set.contains pos area`), so a set holding it
                // would be green with no flood at all.
                let elsewhere =
                    nearestFirst stand capture |> List.filter (fun tile -> tile <> stand)

                Expect.equal
                    (travelCostWithin atlas creep.Name (Set.singleton (here stand)))
                    (Some 0)
                    "the creep's own tile prices at nothing, asked before any flood is"

                Expect.isSome
                    (travelCostWithin atlas creep.Name (elsewhere |> List.map here |> Set.ofList))
                    "and the room around it is still reachable"

                Expect.isGreaterThan
                    (elsewhere
                     |> List.filter (fun tile ->
                         travelCostWithin atlas creep.Name (Set.singleton (here tile))
                         |> Option.isSome)
                     |> List.length)
                    1500
                    "and so is each of its tiles, asked one at a time after the drain"
            }
        ]

// ---- the band read bounded by the best sum (#176) -----------------------

[<Tests>]
let boundedBandTests =
    testList
        "atlas band read bounded by the best sum"
        [
            test "a cross-room price is the one the whole band answers, crossing for crossing" {
                // #176's whole promise. The near leg is now pushed out only
                // as far as the crossing that wins, so the crossings behind
                // the bound are never settled for at all — and the answer
                // has to be the one the band gave when every crossing was.
                //
                // The comparison is against the same memo read after it has
                // been drained, which is the pre-#176 reading exactly: with
                // the heap empty the frontier bounds nothing, every tile
                // holds its final distance, and no crossing is skipped. So
                // one side prunes and the other cannot, over one band, one
                // creep, one body and one room's terrain (ADR 0036).
                //
                // Both routes ride along, because a price and a step that
                // disagree are a creep walked to one crossing and ranked at
                // another (#142): the winning exit comes out of this very
                // minimum, so a wrongly pruned crossing moves the step
                // whether or not it moves the number.
                let mutable priced = 0
                let mutable stepped = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into
                        let drain = Set.singleton (RoomPos.at near.RoomName (drainTile near))

                        for stand in standsAcross near far from into do
                            let bounded = walkingAcross near far stand
                            let whole = walkingAcross near far stand

                            Expect.isNone
                                (travelCostWithin whole "w" drain)
                                $"{from}: a wall is reached by nothing, so asking drains the flood"

                            for sourceId, _ in far.Sources do
                                let task = Harvest sourceId
                                let cost = travelCost bounded "w" task

                                Expect.equal
                                    cost
                                    (travelCost whole "w" task)
                                    $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} prices what the whole band prices"

                                if Option.isSome cost then
                                    priced <- priced + 1

                                // The drained side's goals are the drain
                                // itself: the tiles a cross-room Task hands
                                // its mover are empty either way (`workAreaFor`
                                // answers a creep a room away with nothing),
                                // so both sides fall through to the Seam, and
                                // passing the unreachable tile is what settles
                                // the traffic-blind flood before it does.
                                let step = firstStep bounded "w" task (workAreaFor bounded "w" task)

                                Expect.equal
                                    step
                                    (firstStep whole "w" task drain)
                                    $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} steps where the whole band steps"

                                if Option.isSome step then
                                    stepped <- stepped + 1

                                Expect.equal
                                    (firstStepIgnoringTraffic
                                        bounded
                                        "w"
                                        task
                                        (workAreaFor bounded "w" task))
                                    (firstStepIgnoringTraffic whole "w" task drain)
                                    $"{from} -> {into} from {stand.X},{stand.Y}: and so does the traffic-blind route"

                Expect.isGreaterThan priced 20 "the sweep really did price across"
                Expect.isGreaterThan stepped 20 "and really did step across"
            }

            test "the walk across is the one a flood settled whole answers" {
                // The oracle from outside the memo, and the only case here
                // that does not compare a flood against itself. The hauler
                // quota's round trip runs its own floods and settles them
                // whole (ADR 0012), joins the same band by the same rule
                // (`joinedAcross`), and prices in the same whole ticks
                // (ADR 0029) — so over a Carry-less body its two legs are
                // one journey twice and it must answer exactly twice the
                // Matcher's walk, which reads the tick's resumable memo and
                // prunes the band against its best sum.
                //
                // Real terrain is what makes it worth asserting: the walk
                // detours around walls and pays swamp on both sides of the
                // border, and the two rooms' cheapest crossings are not the
                // same tile from every stand (ADR 0036).
                let mutable walked = 0

                for border in borders do
                    for from, into in [ border.From, border.To; border.To, border.From ] do
                        let near = load from
                        let far = load into

                        for stand in standsAcross near far from into do
                            let atlas = haulingAcross near far stand

                            for sourceId, source in far.Sources do
                                let one = walkTicks atlas "w" (Harvest sourceId)

                                let round =
                                    haulRoundTripTicks
                                        atlas
                                        haulBody
                                        (RoomPos.at from stand)
                                        (RoomPos.at into source)

                                match one, round with
                                | Some out, Some trip ->
                                    walked <- walked + 1

                                    Expect.equal
                                        (out * 2)
                                        trip
                                        $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId}'s bounded walk is the whole flood's"
                                | None, None -> () // unreachable from both, as ADR 0004 has it
                                | out, trip ->
                                    failtest
                                        $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} walks {out} on the memo and {trip} on the whole flood"

                Expect.isGreaterThan walked 20 "the sweep really did walk across"
            }
        ]

/// The road level gate on real terrain (#209 amending ADR 0011). Stated
/// pairwise on one fixture and one spawn, a level at a time: what the gate
/// changes is *when* the plan reaches the ground, so nothing but the level
/// may differ between the two runs it is read off. The child colony the
/// gate was written for is W13S28 — the room that placed 64 road sites at
/// RCL1 with 8 energy a tick coming in — and the mother is W12S28, which
/// is above the line and must not move.
///
/// No tile is named: the room says which tiles the trunks want and this
/// says only that the level decides whether they are asked for.
[<Tests>]
let roadGateTests =
    /// One captured room planned from one fixed spawn at a level, as the
    /// sweep plans it — the same `colonyOf` and the same projection, so the
    /// only thing that varies across a pair below is the controller level.
    ///
    /// The spawn is the tile the room's colony actually stands on where
    /// there is one: `AlsoSweep` carries W12S28's live spawn for exactly
    /// that reason (its own doc above), and the stride's first tile is
    /// (6,6), a corner of the room no colony has ever stood in. A pair
    /// read off `List.head` would be a real pair about an imaginary
    /// mother. A room the capture cannot answer for is planned from the
    /// stride's first tile, which is a premise and not an answer: what the
    /// gate is read off is the level, and the spawn only has to be the
    /// same in both runs.
    let placedAt roomName level =
        let room = rooms |> List.find (fun room -> room.Name = roomName)
        let capture = load roomName

        let spawn =
            match room.AlsoSweep with
            | live :: _ -> live
            | [] -> spawnTiles room capture |> List.head

        let loaded = project capture spawn room.FallbackController

        decide (colonyOf loaded level) Map.empty Set.empty None
        |> fun result -> placementsOf result.Intents

    testList
        "the road level gate on real terrain"
        [
            test "W13S28 places no road site below RCL3 and its whole trunk set at RCL3" {
                let rcl1 = placedAt "W13S28" 1
                let rcl2 = placedAt "W13S28" 2
                let rcl3 = placedAt "W13S28" 3

                Expect.isEmpty
                    (tilesOfKind Road rcl1)
                    "RCL1: the bootstrapping colony pours no income into pavement"

                Expect.isEmpty (tilesOfKind Road rcl2) "RCL2: still under the road level"

                Expect.isNonEmpty
                    (tilesOfKind Road rcl3)
                    "RCL3: the road gate opens and the whole gap drops at once"
            }

            test "the road gap is all the level withheld, and it drops whole" {
                // The gate is a filter on the placement and never on the
                // plan (ADR 0011's "computed whole"), so the set that
                // arrives at RCL3 is the set the room wanted all along —
                // pinned as level-invariance above the line rather than as
                // a tile list, since the plan is the room's answer and not
                // the test's.
                Expect.equal
                    (tilesOfKind Road (placedAt "W13S28" 4) |> Set.ofList)
                    (tilesOfKind Road (placedAt "W13S28" 3) |> Set.ofList)
                    "nothing above the line moves: RCL4 places what RCL3 places"
            }

            test "a bootstrapping room still gets its containers — they wait on no road" {
                // The tile clause (ADR 0040) defers a container to a road
                // *site* on its tile, and below the gate there is none. A
                // source container is the [[post]] that hires the [[anchor]]
                // whose income the gate exists to protect, so holding it
                // back until RCL3 would spend the gate's own saving.
                let rcl1 = placedAt "W13S28" 1

                Expect.isNonEmpty
                    (tilesOfKind Container rcl1)
                    "the containers are planned at RCL1, with no road owed under them"
            }

            test "W13S28's trunks cross the swamp field instead of looping the west edge" {
                // #211: priced at the walk's swamp 10 the router paved a
                // ~28-tile loop along the room's north and west edges to
                // reach the spawn from the north source; priced as a road
                // (swamp 3) it crosses the swamp field between them. Pinned
                // as "some placed road site stands on swamp, and the whole
                // set is well under the loop's size" rather than as a tile
                // list (`RoomFixtures`: real terrain is a counterexample
                // generator, not a source of expected values). The loop's
                // set was 68 sites; the crossing's is 30.
                let capture = load "W13S28"
                let roads = tilesOfKind Road (placedAt "W13S28" 3)

                Expect.isTrue
                    (roads
                     |> List.exists (fun tile -> Map.tryFind tile capture.Terrain = Some Swamp))
                    "at least one trunk tile is paved over swamp"

                Expect.isLessThan
                    (List.length roads)
                    45
                    "and the trunk set is the crossing's, not the loop's"
            }

            test "the mother is above the line and does not move" {
                // W12S28 from the tile its colony stands on, at the live
                // RCL5: the gate is inert from the bootstrap line up, so the road
                // sites there are the same set RCL3 places. What this pins
                // is the gate's upper edge and not byte-identity with the
                // revision before it — one revision cannot compare itself
                // to another, and a stored tile list is the expected value
                // this suite refuses to hold (`RoomFixtures`: real terrain
                // is a counterexample generator, not a source of expected
                // values). What judges the set itself is the RCL4 sweep's
                // own road invariants, which run over every swept spawn of
                // this room, this tile among them.
                let rcl5 = tilesOfKind Road (placedAt "W12S28" 5) |> Set.ofList

                Expect.isNonEmpty rcl5 "the mother still paves"

                Expect.equal
                    rcl5
                    (tilesOfKind Road (placedAt "W12S28" 3) |> Set.ofList)
                    "and paves exactly what it pays for at every level the gate lets through"
            }

            test "the gate withholds one kind and does not stop the Layout" {
                // What a level gate on roads is not: a Layout that waits for
                // RCL3. Every other kind is judged at RCL2 by its own rule
                // and still reaches the ground — the extensions the level
                // does unlock, the containers no level gates at all, the
                // ramparts from the bootstrap line up — so the room goes on
                // growing into exactly the spend the gate is protecting.
                let rcl2 = placedAt "W13S28" 2

                Expect.isEmpty (tilesOfKind Road rcl2) "the premise: RCL2 places no road"

                Expect.isNonEmpty
                    (tilesOfKind Extension rcl2)
                    "the extensions the level unlocks still drop"

                Expect.isNonEmpty (tilesOfKind Container rcl2) "and the containers with them"
            }
        ]
