/// The Atlas flood settled on demand, and the band read bounded by the
/// best sum.
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
                // the memo against itself: `haulRoundTripTicks` settles its
                // own floods whole, `walkTicks` reads the resumable per-creep
                // memo, and both are traffic-blind, so they must agree.
                //
                // The fixture makes them line up: a Carry-less body is priced
                // identically loaded and empty, so the round trip is exactly
                // twice the one-way walk; and a spawn is an obstacle, so the
                // Refill Work Area at range 1 is exactly its adjacent
                // walkable tiles. On real terrain a flood stopped a pop too
                // early would answer a route it had not finished finding.
                let body = [ Work; Move ]

                for roomName, spawn, _ in floodRooms do
                    let capture = load roomName
                    let task = Refill("spawn-1", Energy)

                    let stands =
                        capture.Terrain
                        |> TerrainGrid.toList
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
                        | None, None -> () // no route: absent from both
                        | one, round ->
                            failtest
                                $"{roomName} from {stand}: memo {one} and whole flood {round} disagree on whether there is a walk"

                    Expect.isGreaterThan walked 50 $"{roomName}: walks actually compared"
            }

            test "a tile prices the same whichever order the room is read in" {
                // The memo is shared, so what it answers must not depend on
                // what was asked of it before: a flood pushed out tile by
                // tile, one asked from the far end in, and one asked for a
                // single tile must agree everywhere. Dijkstra settles a tile
                // for good when it leaves the heap, so this is a property.
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

                        // The fourth order compares against the whole flood:
                        // a tile nothing reaches settles the heap to
                        // exhaustion, so every read after it comes off the
                        // flood laid in one go.
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
                // The other half of a flood is its predecessor chain, read
                // after the goal settles: every tile of a cheapest path is
                // strictly cheaper than the goal, so all of them left the
                // heap first. A stale parent would move `firstStep`'s answer
                // without moving a single price. Both routing pricings are
                // swept, since the Resolver compares them.
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

                        // The chain against a flood settled whole: a goal
                        // nothing reaches drains the heap through this very
                        // reader. The drain has to answer absent, or it has
                        // not drained.
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
                // The clock's reader takes a Task rather than a tile, so it
                // is the Tasks of a room that ask one flood for one creep in
                // one tick. Asked in either order, or each on a flood of its
                // own, every Task must answer the same walk.
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
                // The trap a resumable flood sets and no type can catch:
                // `unreached` means "nothing gets here" in a whole flood and
                // "nobody has asked yet" in a resumed one, so a reader that
                // answered off the grid before settling would call a
                // reachable tile unreachable. Every wall of a real room is a
                // case, and so is the ring around it.
                let capture = load "W12S28"
                let creep = AtlasFixtures.worker "w"
                let stand = { X = 24; Y = 24 }
                let atlas = standingIn capture { X = 12; Y = 40 } creep stand
                let here tile = RoomPos.at capture.RoomName tile

                let walls =
                    capture.Terrain
                    |> TerrainGrid.toList
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

                // An absent answer drains the flood, and the reads that
                // follow must still answer. The creep's own tile is left out:
                // `pricedPathTo` answers it before asking the flood anything,
                // so a set holding it would be green with no flood at all.
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
                // The near leg is pushed out only as far as the crossing
                // that wins, so the crossings behind the bound are never
                // settled — and the answer has to be the one the band gave
                // when every crossing was. The comparison is against the
                // same memo after it has been drained: with the heap empty
                // the frontier bounds nothing and no crossing is skipped.
                //
                // Both routes ride along: the winning exit comes out of this
                // very minimum, so a wrongly pruned crossing moves the step
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

                                // `workAreaFor` answers a creep a room away
                                // with nothing, so both sides fall through to
                                // the Seam; passing the unreachable tile is
                                // what settles the traffic-blind flood first.
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
                // The oracle from outside the memo: the hauler quota's round
                // trip settles its own floods whole and joins the same band
                // by the same rule (`joinedAcross`), so over a Carry-less
                // body it must answer exactly twice the Matcher's walk, which
                // reads the resumable memo and prunes the band against its
                // best sum. On real terrain the two rooms' cheapest crossings
                // are not the same tile from every stand.
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
                                | None, None -> () // unreachable from both
                                | out, trip ->
                                    failtest
                                        $"{from} -> {into} from {stand.X},{stand.Y}: {sourceId} walks {out} on the memo and {trip} on the whole flood"

                Expect.isGreaterThan walked 20 "the sweep really did walk across"
            }
        ]

/// The road level gate on real terrain, stated pairwise on one fixture and
/// one spawn, a level at a time: nothing but the level may differ between
/// the two runs it is read off. W13S28 is the child the gate was written
/// for (64 road sites at RCL1 on 8 energy a tick); W12S28 is the mother,
/// above the line, and must not move. No tile is named.
[<Tests>]
let roadGateTests =
    /// One captured room planned from one fixed spawn at a level, through
    /// the sweep's own `colonyOf` and projection. The spawn is the live one
    /// where `AlsoSweep` carries it — a pair read off the stride's first
    /// tile would be a real pair about an imaginary mother — and the
    /// stride's first tile otherwise, which only has to be the same in both
    /// runs.
    let colonyAt roomName level =
        let room = rooms |> List.find (fun room -> room.Name = roomName)
        let capture = load roomName

        let spawn =
            match room.AlsoSweep with
            | live :: _ -> live
            | [] -> spawnTiles room capture |> List.head

        colonyOf (project capture spawn room.FallbackController) level

    let placedAt roomName level =
        decide (colonyAt roomName level) Map.empty Set.empty None
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
                // plan: the tick it opens leaves nothing owed. Pave the set
                // RCL3 asks for and the same room at the same level asks for
                // no road at all; a gate that paced would hand out the rest
                // on the tick after. Level-invariance above the line is
                // pinned with it: the road reservation reads no level.
                let placed = tilesOfKind Road (placedAt "W13S28" 3)

                Expect.isNonEmpty placed "RCL3 asks for the trunk set"

                Expect.equal
                    (tilesOfKind Road (placedAt "W13S28" 4) |> Set.ofList)
                    (Set.ofList placed)
                    "and RCL4 asks for exactly what RCL3 asks for: the road plan reads no level"

                // The same spawn `placedAt` chose, through the same builder:
                // a second derivation would stop being the same room the day
                // W13S28 gains an `AlsoSweep` tile.
                let colony = colonyAt "W13S28" 3

                let paved =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.union layer.Roads (Set.ofList placed)
                                })
                    }

                Expect.isEmpty
                    (decide paved Map.empty Set.empty None
                     |> fun result -> placementsOf result.Intents |> tilesOfKind Road)
                    "and once they stand nothing is owed: the gap dropped whole, it was not paced"
            }

            test "a bootstrapping room still gets its containers — they wait on no road" {
                // A container defers to a road site on its tile, and below
                // the gate there is none; holding the Post back until RCL3
                // would spend the gate's own saving.
                let rcl1 = placedAt "W13S28" 1

                Expect.isNonEmpty
                    (tilesOfKind Container rcl1)
                    "the containers are planned at RCL1, with no road owed under them"
            }

            test "W13S28's trunks cross the swamp field instead of looping the west edge" {
                // Priced at the walk's swamp 10 the router paved a ~28-tile
                // loop along the north and west edges; priced as a road
                // (swamp 3) it crosses the swamp field. Pinned as "some road
                // stands on swamp, and the set is well under the loop's
                // size" rather than as a tile list: the loop was 68 sites,
                // the crossing 30.
                let capture = load "W13S28"
                let roads = tilesOfKind Road (placedAt "W13S28" 3)

                Expect.isTrue
                    (roads
                     |> List.exists (fun tile ->
                         TerrainGrid.tryFind tile capture.Terrain = Some Swamp))
                    "at least one trunk tile is paved over swamp"

                Expect.isLessThan
                    (List.length roads)
                    45
                    "and the trunk set is the crossing's, not the loop's"
            }

            test "the mother is above the line and does not move" {
                // The gate is inert from the bootstrap line up, so RCL5's
                // road sites are the set RCL3 places. This pins the gate's
                // upper edge, not a stored tile list; the sweep's own road
                // invariants judge the set itself.
                let rcl5 = tilesOfKind Road (placedAt "W12S28" 5) |> Set.ofList

                Expect.isNonEmpty rcl5 "the mother still paves"

                Expect.equal
                    rcl5
                    (tilesOfKind Road (placedAt "W12S28" 3) |> Set.ofList)
                    "and paves exactly what it pays for at every level the gate lets through"
            }

            test "the gate withholds one kind and does not stop the Layout" {
                // A level gate on roads is not a Layout that waits for RCL3:
                // every other kind is judged at RCL2 by its own rule.
                let rcl2 = placedAt "W13S28" 2

                Expect.isEmpty (tilesOfKind Road rcl2) "the premise: RCL2 places no road"

                Expect.isNonEmpty
                    (tilesOfKind Extension rcl2)
                    "the extensions the level unlocks still drop"

                Expect.isNonEmpty (tilesOfKind Container rcl2) "and the containers with them"
            }
        ]
