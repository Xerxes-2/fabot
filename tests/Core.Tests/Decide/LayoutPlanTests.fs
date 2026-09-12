/// The Layout itself: the Storage, the Link footings (ADR 0038) and the
/// trunks that carry a source (ADR 0011).
module Fabot.Core.Tests.Decide.LayoutPlanTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.LayoutFixtures

/// The premise the two collision cases below perturb (#246, #248): the RCL4
/// trunk colony with its roads standing, and the one tile that tick's plan asks
/// for the source container on. Asserted here rather than in each case, because
/// a case that blocks a tile the plan never wanted would pass for the wrong
/// reason.
let private sourceContainerPick () =
    let colony = withRoadsBuilt (trunkColony 4)

    let pick =
        sitesOfKind Container (decideOn colony).Intents
        |> List.filter (fun tile -> chebyshev tile { X = 15; Y = 25 } <= 1)

    Expect.hasLength pick 1 "the premise: with the tile free the source container is asked for"

    colony, pick.Head

[<Tests>]
let layoutTests =
    testList
        "layout"
        [
            test "RCL2 places the extension gap and no tower — and not one road" {
                let { Intents = intents } = decideOn (trunkColony 2)

                Expect.isEmpty (sitesOfKind Tower intents) "no tower below RCL3"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    3
                    "only the gap against the two built extensions is placed"

                // The gate on road sites (#209 amending ADR 0011): the
                // trunks are planned whole at every level and placed only
                // for a colony past the bootstrap line (`placesRoads`, ADR
                // 0052 decision 3). Below it the pavement costs the level
                // that would make the body bigger, which is worth more than
                // the half tick a loaded step it buys (ADR 0010, narrowed
                // by that gate). Pairwise against the test below, same
                // fixture one level on.
                Expect.isEmpty (sitesOfKind Road intents) "no road site below the road level"
            }

            test "the same fixture at RCL3 places every trunk road" {
                let { Intents = intents } = decideOn (trunkColony 3)
                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 15; Y = 25 } = 1))
                    "a trunk starts beside the source"

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 25; Y = 25 } = 1))
                    "a trunk ends beside the spawn"

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 35; Y = 25 } <= 3))
                    "a trunk reaches the controller's Work Area"

                Expect.contains roads { X = 33; Y = 27 } "a Work Area swamp is paved"
                Expect.contains roads { X = 34; Y = 24 } "the other Work Area swamp is paved"

                Expect.isFalse
                    (Set.contains { X = 20; Y = 20 } roads)
                    "a swamp off every trunk line is not paved"
            }

            test "the same fixture at RCL3 adds the tower and extensions 6-10 at once" {
                let { Intents = intents } = decideOn (trunkColony 3)

                // (24,24) is the ordering's first free tile and the Storage's
                // reservation (ADR 0022); the tower takes the one after it,
                // and the fixture's two built extensions hold (24,26)/(26,24).
                Expect.equal
                    (sitesOfKind Tower intents)
                    [ { X = 26; Y = 26 } ]
                    "the tower takes the ordering's first free tile after the Storage's"

                let extensions = sitesOfKind Extension intents
                Expect.hasLength extensions 8 "the RCL3 allowance fills against the two built"

                for tile in extensions do
                    Expect.isLessThan
                        (orderKey { X = 26; Y = 26 })
                        (orderKey tile)
                        "the tower's pick comes before every extension in the one ordering"
            }

            test "the same ColonyView recomputes to the identical site set" {
                let first = decideOn (trunkColony 2)
                let second = decideOn (trunkColony 2)

                Expect.equal
                    (placementIntents first.Intents)
                    (placementIntents second.Intents)
                    "the Layout is deterministic — sites never jitter between computations"
            }

            test "trunks route around every horizon reservation" {
                // Read from the road gate up, where the sites are placed
                // (#209): below it the plan is still routed whole but
                // nothing of it reaches the ground, so the road *sites* are
                // the same at every level the gate lets through and this
                // pair is RCL3 against the horizon's own RCL4.
                let rcl3 = decideOn (trunkColony 3)
                let rcl4 = decideOn (trunkColony 4)
                let roads = sitesOfKind Road rcl3.Intents |> Set.ofList

                // Read off the horizon's own level, where the whole
                // reservation is on the ground — the second tower and the
                // twenty extensions RCL5 and RCL6 add included (ADR 0039, ADR
                // 0055). Below it the check only ever saw the part the level
                // had placed, so this level moves with the horizon.
                let cluster = clusterTiles (decideOn (trunkColony 6)).Intents

                Expect.equal
                    (sitesOfKind Road rcl4.Intents |> Set.ofList)
                    roads
                    "the road plan is the same at every level — the horizon never moves"

                Expect.isEmpty
                    (Set.intersect roads cluster)
                    "no trunk tile coincides with a reserved structure tile"
            }

            test "a Seat beside the spawn is working ground: no tower, no extension" {
                // The source stands two tiles north of the spawn, so four of
                // its Seats are the cluster's own nearest same-colour tiles.
                let sourcePos = { X = 25; Y = 23 }

                let colony = atLevel 3 (openRoom 6 |> withTargets [ "src-a", sourcePos, Source ])

                let { Intents = intents } = decideOn colony

                let seats =
                    Set.ofList
                        [
                            for x in sourcePos.X - 1 .. sourcePos.X + 1 do
                                for y in sourcePos.Y - 1 .. sourcePos.Y + 1 do
                                    if { X = x; Y = y } <> sourcePos then
                                        { X = x; Y = y }
                        ]

                let cluster = clusterTiles intents

                Expect.isNonEmpty cluster "the cluster still fills, one ring out"

                Expect.isEmpty
                    (Set.intersect cluster seats)
                    "no clustered structure eats a Seat the Anchors stand on"
            }

            test "the Upgrade Work Area is working ground: no tower, no extension" {
                // The controller stands four tiles north of the spawn, so its
                // Upgrade Work Area covers the cluster's nearest same-colour
                // tiles without covering the spawn itself.
                let controllerPos = { X = 25; Y = 21 }

                let colony =
                    atLevel 3 (openRoom 6 |> withTargets [ "ctrl-1", controllerPos, Controller ])

                let { Intents = intents } = decideOn colony

                let upgradeArea =
                    Set.ofList
                        [
                            for x in controllerPos.X - 3 .. controllerPos.X + 3 do
                                for y in controllerPos.Y - 3 .. controllerPos.Y + 3 do
                                    { X = x; Y = y }
                        ]

                let cluster = clusterTiles intents

                Expect.isNonEmpty cluster "the cluster still fills, one ring out"

                Expect.isEmpty
                    (Set.intersect cluster upgradeArea)
                    "no clustered structure eats a tile an upgrader stands on"
            }

            test "without a source only the Work Area swamps are paved, never plain" {
                // At the road gate, the stage the sites reach the ground from
                // (#209): below it the answer is empty whatever the plan is.
                let { Intents = intents } = decideOn (noSourceColony 3)

                Expect.equal
                    (sitesOfKind Road intents |> Set.ofList)
                    (Set.ofList [ { X = 33; Y = 27 }; { X = 34; Y = 24 } ])
                    "exactly the Work Area's swamp tiles get roads"
            }

            test "built roads and pending road sites are never placed again" {
                // At the road gate too: below it the gate would answer empty
                // for its own reason and the census rule would go untested.
                let colony = noSourceColony 3

                let snapshot =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withRoads [ { X = 33; Y = 27 } ]
                            |> withTargets
                                [ "road-site-1", { X = 34; Y = 24 }, Site BuiltKind.Road ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty
                    (sitesOfKind Road intents)
                    "the gap reads the projection's road census: both tiles are claimed"
            }

            test "each source gets one container on the Seat where its trunk starts" {
                // Roads stand from the road gate up (#209), so the level that
                // has a trunk to seat the container beside is RCL3.
                let colony = withRoadsBuilt (trunkColony 3)
                let { Intents = intents } = decideOn colony

                let sourceContainers =
                    sitesOfKind Container intents
                    |> List.filter (fun tile -> chebyshev tile { X = 15; Y = 25 } = 1)

                Expect.hasLength sourceContainers 1 "one container per source"

                Expect.contains
                    (homeLayer colony.Spatial).Roads
                    sourceContainers.Head
                    "the Seat nearest the trunk is the trunk's own first tile"
            }

            test "a container never shares a tile with a planned road site" {
                // One construction site per tile (engine rule): on a fresh
                // plan the source container defers to the trunk road site
                // under it and drops only once that road stands. At
                // the road gate, where there is a road site to collide with —
                // below it none is placed and the clause has nothing to say
                // (#209).
                let { Intents = intents } = decideOn (trunkColony 3)
                let roads = sitesOfKind Road intents |> Set.ofList

                for tile in sitesOfKind Container intents do
                    Expect.isFalse
                        (Set.contains tile roads)
                        "the container waits for the road on its tile"
            }

            test "a site of another kind on the pick is waited on, not asked for again" {
                // #246: the tile clause subtracted the road sites alone, so a
                // Tower, Extension or Rampart site a human had put on the
                // container's own pick was invisible to it — the plan
                // re-issued `PlaceConstructionSite` onto that tile every tick
                // and the Executor answered ERR_INVALID_TARGET until somebody
                // built it. Only a hand can make the collision: the Layout's
                // own picks are pairwise disjoint. The container kind stays
                // out of the census this reads, a container site on the pick
                // being the target clause's business one rule above (ADR
                // 0040).
                let colony, pick = sourceContainerPick ()

                let blocked =
                    { colony with
                        Spatial =
                            colony.Spatial |> withTargets [ "tow-site", pick, Site BuiltKind.Tower ]
                    }

                Expect.isFalse
                    (List.contains pick (sitesOfKind Container (decideOn blocked).Intents))
                    "the pick waits for the tile the engine has already given another site"
            }

            test "a rival's site on the pick is waited on the same way" {
                // #248, the other half of #246's hole: the tile clause read a
                // census built from `FIND_MY_CONSTRUCTION_SITES`, so the only
                // sites it could collide with were our own. One site per tile
                // is the engine's rule whoever placed the site, so a rival's
                // refuses the pick exactly as a hand-placed Tower of ours does,
                // and the plan waits rather than asking again.
                //
                // The window at home is **narrow** and not zero, which is why
                // this half is worth having and why it is not urgent:
                // `createConstructionSite` answers ERR_RCL_NOT_ENOUGH in a room
                // another player owns, so nobody starts one here — but a site
                // placed while the room was still *neutral* survives into the
                // room we then claim, so a freshly claimed [[nursery]] is the
                // window. Inside it the container pick is the only one that
                // waits: the road, clustered, tower and rampart gaps still pick
                // blind onto a colliding tile, which is #291 and not this
                // ticket — widening them changes what those plans consider
                // owed.
                let colony, pick = sourceContainerPick ()

                let blocked =
                    { colony with
                        Spatial = colony.Spatial |> withRivalSites [ pick ]
                    }

                Expect.isFalse
                    (List.contains pick (sitesOfKind Container (decideOn blocked).Intents))
                    "the pick waits for the tile a rival's site already holds"
            }

            test "the controller container lands in the Work Area beside a trunk" {
                // At the road gate: the trunk it is judged against is a road
                // site, and those are placed from RCL3 up (#209).
                let { Intents = intents } = decideOn (trunkColony 3)
                let controllerPos = { X = 35; Y = 25 }

                let controllerContainers =
                    sitesOfKind Container intents
                    |> List.filter (fun tile -> chebyshev tile controllerPos <= 3)

                Expect.hasLength controllerContainers 1 "exactly one controller container"

                let tile = controllerContainers.Head
                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isTrue
                    (roads |> Set.exists (fun road -> chebyshev road tile = 1))
                    "the container sits adjacent to a trunk tile"

                Expect.isFalse (Set.contains tile roads) "the container stays off the road itself"
            }

            test "containers have no RCL gate — level 1 already places both kinds, on no road" {
                // The tile clause (ADR 0040) defers a container to a road
                // *site* on its tile, because two sites cannot share one.
                // Below the road gate no road site is placed, so there is
                // nothing to collide with and nothing to wait for (#209):
                // read off the whole road *plan* instead, this fixture's
                // source container would sit on the trunk's own first tile
                // and be held back until RCL3, and a container site on a
                // Seat is the [[post]] that hires the [[anchor]] (#205)
                // whose income the gate exists to protect. Nothing stands
                // in this fixture, so what the containers coexist with is
                // the empty placement and not a built road — the standing
                // road's own case is the RCL3 tests below.
                let { Intents = intents } = decideOn (trunkColony 1)

                Expect.isEmpty (sitesOfKind Road intents) "the premise: RCL1 places no road"

                Expect.hasLength
                    (sitesOfKind Container intents)
                    2
                    "one source container and one controller container"
            }

            test "the road the level withheld is never asked for onto a pending container" {
                // The mirror of the tile clause (ADR 0040), in the
                // direction the level gate opened (#209): the source
                // container drops on the trunk's first tile at RCL1
                // because no road site is placed there to collide with,
                // and it is still a *site* — 5,000 energy of progress —
                // when the colony reaches the road gate. That tile is still in
                // the road gap, so a road site would be asked for on top
                // of it and the engine would refuse it every tick until
                // the container finished.
                let rcl1 = decideOn (trunkColony 1)

                let trunkRoads =
                    decideOn (trunkColony 3)
                    |> fun result -> sitesOfKind Road result.Intents |> Set.ofList

                let onTrunk =
                    sitesOfKind Container rcl1.Intents
                    |> List.filter (fun tile -> Set.contains tile trunkRoads)

                Expect.isNonEmpty
                    onTrunk
                    "the premise: RCL1 seats a container on a tile the trunk wants"

                let pending =
                    onTrunk
                    |> List.mapi (fun index tile ->
                        $"can-site-%d{index}", tile, Site BuiltKind.Container)

                let colony = trunkColony 3

                let { Intents = intents } =
                    decideOn
                        { colony with
                            Spatial = colony.Spatial |> withTargets pending
                        }

                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isEmpty
                    (onTrunk |> List.filter (fun tile -> Set.contains tile roads))
                    "one construction site per tile: the road waits for the container to stand"

                Expect.isNonEmpty roads "and the rest of the trunk set still drops"
            }

            test "a one-Seat source gets its container on that Seat" {
                // From the road gate up, where `withRoadsBuilt` has a road
                // set to stand (#209): below it the plan places none and
                // the fixture would carry an empty premise.
                let { Intents = intents } = decideOn (withRoadsBuilt (pocketColony 3))

                Expect.contains
                    (sitesOfKind Container intents)
                    { X = 21; Y = 30 }
                    "the single Seat is the nearest Seat to the pocket source's trunk"
            }

            test "built containers and pending container sites are never placed again" {
                // From the road gate up, so the roads `withRoadsBuilt`
                // stands are the plan's own and the containers are read
                // beside a real road census (#209).
                let colony = withRoadsBuilt (trunkColony 3)
                let planned = decideOn colony

                let standing =
                    match sitesOfKind Container planned.Intents with
                    | [ a; b ] ->
                        [
                            "can-1", a, Structure BuiltKind.Container
                            "can-site-1", b, Site BuiltKind.Container
                        ]
                    | other -> failtest $"expected two planned container sites, got %A{other}"

                let snapshot =
                    { colony with
                        Spatial = colony.Spatial |> withTargets standing
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty
                    (sitesOfKind Container intents)
                    "the census claims both tiles: nothing re-drops"

                Expect.equal
                    (sitesOfKind Road intents)
                    (sitesOfKind Road planned.Intents)
                    "standing containers never perturb the road plan"
            }

            test "a container off its source's pick serves that source (#74)" {
                // The pick moves when the trunk moves, and the container
                // standing on the old pick is still the only container this
                // source has (ADR 0040). The target is served wherever the
                // thing serving it sits, so no second site drops beside it.
                let srcPos = { X = 15; Y = 25 }
                let colony = withRoadsBuilt (trunkColony 4)
                let planned = decideOn colony

                let pick =
                    sitesOfKind Container planned.Intents
                    |> List.find (fun tile -> chebyshev tile srcPos <= 1)

                // (14,24) is a Seat of the same source and not the pick —
                // the container a previous plan left standing.
                let orphan = { X = 14; Y = 24 }

                let offPick =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "can-old", orphan, Structure BuiltKind.Container ]
                    }

                let after = decideOn offPick

                Expect.isEmpty
                    (sitesOfKind Container after.Intents
                     |> List.filter (fun tile -> chebyshev tile srcPos <= 1))
                    "the standing container serves the source: its own pick is not placed"

                Expect.equal
                    after.Memo.DeferredContainers
                    [
                        {
                            Target = ContainerTarget.Source "src-a"
                            Pick = RoomPos.at (SpatialInfo.homeName colony.Spatial) pick
                            Serving = RoomPos.at (SpatialInfo.homeName colony.Spatial) orphan
                        }
                    ]
                    "the record names the target, the tile the plan picked and the tile serving it"

                // The tick after the plan runs: everything it placed now
                // stands. This is where the defect was paid for — a second
                // container on the pick is a second Post and a second term
                // in the hauler quota, forever.
                let built =
                    { offPick with
                        Spatial =
                            offPick.Spatial
                            |> withTargets
                                [
                                    for i, tile in
                                        List.indexed (sitesOfKind Container after.Intents) ->
                                        $"can-new-{i}", tile, Structure BuiltKind.Container
                                ]
                    }

                Expect.equal
                    (Atlas.postsIn (Atlas.ofView built) (SpatialInfo.homeName built.Spatial))
                    (Set.singleton orphan)
                    "one Post, on the Seat the container actually stands on"

                Expect.equal
                    (decideOn built).Memo.HaulerQuota
                    1
                    "the hauler row is sized for one source container — the orphan's own term"
            }

            test "a pending container site off the pick serves the source too (#74)" {
                // A site is already going up: judging the target from
                // standing containers alone would drop a second site beside
                // a site, the same defect one tick earlier (ADR 0040).
                let srcPos = { X = 15; Y = 25 }
                let colony = withRoadsBuilt (trunkColony 4)
                let orphan = { X = 14; Y = 24 }

                let offPick =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "can-site-old", orphan, Site BuiltKind.Container ]
                    }

                let after = decideOn offPick

                Expect.isEmpty
                    (sitesOfKind Container after.Intents
                     |> List.filter (fun tile -> chebyshev tile srcPos <= 1))
                    "the pending site serves the source: its own pick is not placed"

                Expect.equal
                    (after.Memo.DeferredContainers
                     |> List.map (fun d -> d.Target, RoomPos.pos d.Serving))
                    [ ContainerTarget.Source "src-a", orphan ]
                    "the deferral is recorded for a pending site as for a standing container"
            }

            test "a container anywhere in the Work Area serves the controller (#74)" {
                let controllerPos = { X = 35; Y = 25 }
                let colony = withRoadsBuilt (trunkColony 4)
                let planned = decideOn colony

                let pick =
                    sitesOfKind Container planned.Intents
                    |> List.find (fun tile -> chebyshev tile controllerPos <= 3)

                // An Upgrade Work Area tile that is not the pick: the
                // controller's container, left where an older plan put it.
                let orphan =
                    [
                        for x in controllerPos.X - 3 .. controllerPos.X + 3 do
                            for y in controllerPos.Y - 3 .. controllerPos.Y + 3 do
                                { X = x; Y = y }
                    ]
                    |> List.find (fun tile -> tile <> pick && tile <> controllerPos)

                let offPick =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "can-ctrl", orphan, Structure BuiltKind.Container ]
                    }

                let after = decideOn offPick

                Expect.isEmpty
                    (sitesOfKind Container after.Intents
                     |> List.filter (fun tile -> chebyshev tile controllerPos <= 3))
                    "the standing container serves the controller: its own pick is not placed"

                Expect.equal
                    after.Memo.DeferredContainers
                    [
                        {
                            Target = ContainerTarget.Controller
                            Pick = RoomPos.at (SpatialInfo.homeName colony.Spatial) pick
                            Serving = RoomPos.at (SpatialInfo.homeName colony.Spatial) orphan
                        }
                    ]
                    "the controller's deferral is recorded beside the sources'"
            }

            test "a container on its own pick is served, not deferred (#74)" {
                // The coinciding case: the plan wants exactly the tile the
                // container stands on, so nothing is lost and the record
                // stays empty (ADR 0040).
                let colony = withRoadsBuilt (trunkColony 4)
                let planned = decideOn colony

                let standing =
                    sitesOfKind Container planned.Intents
                    |> List.mapi (fun i tile -> $"can-{i}", tile, Structure BuiltKind.Container)

                let after =
                    decideOn
                        { colony with
                            Spatial = colony.Spatial |> withTargets standing
                        }

                Expect.isEmpty (sitesOfKind Container after.Intents) "nothing re-drops"

                Expect.isEmpty
                    after.Memo.DeferredContainers
                    "a pick that never moved lost nothing and records nothing"
            }

            test "a spawn the projection files in another room plans nothing here (#191)" {
                // The accident ADR 0052 decision 2 is written against, and
                // the one the Layout was carrying until #216 R3: `Spawns`
                // is a list, and the second entry's tile used to be read
                // onto the *home* grid whatever room the projection filed
                // it under. On the live colony that was Spawn2 standing in
                // the child room, setting this room's cluster parity, its
                // ordering distance and a trunk goal out of a coordinate
                // fifty tiles and a border away.
                //
                // Pairwise on the room and on nothing else: the same
                // spawn, the same id, the same coordinate, filed once in
                // the neighbour and once at home. The neighbour's changes
                // no site; home's changes several — which is what says the
                // fixture could have shown a difference, so the first
                // assertion is a rule holding rather than a coordinate
                // that happened not to matter.
                //
                // The neighbour half is hand-built and has to be:
                // `ColonyView.ofWorld` cuts `Spawns` from the home room's
                // facts alone since R2a, so no view the shell can cut puts
                // a spawn of this colony's in another room's layer. The
                // guard closes the shape at the site that reads the tile;
                // the narrowing that closes the live path is upstream.
                let colony = trunkColony 4
                let stray = { X = 12; Y = 34 }

                let secondSpawn =
                    {
                        Name = "Spawn2"
                        Id = "spawn-2"
                        RoomName = "W1N1"
                        IsSpawning = false
                    }

                let casting (view: ColonyView) =
                    { view with
                        Spawns = view.Spawns @ [ secondSpawn ]
                    }

                // The neighbour's layer, laid by hand: `withOutpost` is
                // defined below this list, and the whole of what this case
                // needs is the id filed under another room's name.
                let elsewhere =
                    { casting colony with
                        Spatial =
                            { colony.Spatial with
                                Rooms =
                                    Map.add
                                        "W2N1"
                                        { RoomLayer.empty with
                                            TargetPositions = Map.ofList [ "spawn-2", stray ]
                                        }
                                        colony.Spatial.Rooms
                                TargetKinds =
                                    Map.add
                                        "spawn-2"
                                        (Structure BuiltKind.Spawn)
                                        colony.Spatial.TargetKinds
                            }
                    }

                let here =
                    { casting colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "spawn-2", stray, Structure BuiltKind.Spawn ]
                    }

                let plan (view: ColonyView) =
                    placementIntents (decideOn view).Intents

                Expect.equal
                    (plan elsewhere)
                    (plan colony)
                    "a spawn standing in the neighbour draws no trunk and moves no site here"

                Expect.notEqual
                    (plan here)
                    (plan colony)
                    "while the same coordinate in this room does: the room is the whole difference"
            }

            test "a neighbour's extension site is no charge against this room's allowance (#140)" {
                // The gap rule is `allowed at RCL - built - pending`, and
                // the allowance is *this* controller's — so the census
                // subtracted from it has to be this room's. The six kind
                // counts read the flat, id-keyed census until #216 R3 and
                // answered for every room the projection carried, and ADR
                // 0052 decision 7's borrowing is what made that reachable:
                // a mother carries a bootstrapping child's construction
                // sites so her workers may build them
                // (`ColonyView.borrowed` keeps every `Site _`), so the
                // child's extension sites came off her own allowance and
                // she placed that many fewer, for the whole bootstrap
                // window.
                //
                // Pairwise on the room the site is filed under: the same
                // id at the same coordinate, once in the neighbour's layer
                // and once in this room's. The neighbour's takes nothing;
                // this room's takes exactly one slot — which is what says
                // the fixture could have shown a difference.
                let colony = atLevel 2 (openRoom 3)
                let stray = { X = 30; Y = 30 }

                let joined =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { RoomLayer.empty with
                                            TargetPositions = Map.ofList [ "ext-out", stray ]
                                        }
                                        colony.Spatial.Rooms
                                TargetKinds =
                                    Map.add
                                        "ext-out"
                                        (Site BuiltKind.Extension)
                                        colony.Spatial.TargetKinds
                            }
                    }

                let here =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "ext-out", stray, Site BuiltKind.Extension ]
                    }

                let extensions (view: ColonyView) =
                    sitesOfKind Extension (decideOn view).Intents

                Expect.hasLength (extensions colony) 5 "the premise: RCL2's whole allowance"

                Expect.equal
                    (extensions joined)
                    (extensions colony)
                    "a site standing in the neighbour is no charge against this room's five"

                Expect.hasLength
                    (extensions here)
                    4
                    "while the same site in this room is: one slot of the five is taken"
            }
        ]

[<Tests>]
let storageTests =
    testList
        "storage"
        [
            test "RCL4 places one Storage on the ordering's first pick, the tower next" {
                // The cluster's nearest same-colour tile is the Storage's at
                // every level (ADR 0022) — the tower and the extensions take
                // the picks after it.
                let { Intents = intents } = decideOn (atLevel 4 (openRoom 3))

                Expect.equal
                    (sitesOfKind Storage intents)
                    [ { X = 24; Y = 24 } ]
                    "one Storage, on the ordering's first still-open pick"

                Expect.equal
                    (sitesOfKind Tower intents)
                    [ { X = 24; Y = 26 } ]
                    "the tower's pick is the one after the Storage's"

                for tile in sitesOfKind Extension intents do
                    Expect.isLessThan
                        (orderKey { X = 24; Y = 24 })
                        (orderKey tile)
                        "the Storage's pick comes before every extension in the one ordering"
            }

            test "RCL3 places no Storage yet still holds its tile against the cluster" {
                // The reservation is level-blind (ADR 0022): once an extension
                // takes that tile it never comes back, so it is held from the
                // first tick, levels before the engine allows the Storage.
                let { Intents = intents } = decideOn (atLevel 3 (openRoom 3))

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the engine allows no Storage below RCL4"

                Expect.isNonEmpty (sitesOfKind Extension intents) "the cluster still fills"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "nothing at all is placed on the reserved first pick"
            }

            test "a standing Storage places none, and its tile leaves the ordering" {
                let standing =
                    openRoom 3
                    |> withTargets [ "sto-1", { X = 24; Y = 24 }, Structure BuiltKind.Storage ]

                let { Intents = intents } = decideOn (atLevel 4 standing)

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the standing census fills the allowance"

                Expect.isFalse
                    (Set.contains { X = 24; Y = 24 } (clusterTiles intents))
                    "a standing structure's tile is not buildable: no cluster pick lands on it"

                // The one thing that is planned onto it: its rampart. A
                // rampart is no footprint, so the Storage's own tile is where
                // it belongs (ADR 0034).
                Expect.contains
                    (sitesOfKind Rampart intents)
                    { X = 24; Y = 24 }
                    "a standing Storage is a Keep structure and gets its cover"
            }

            test "a pending Storage site places none" {
                let pending =
                    openRoom 3
                    |> withTargets [ "sto-site", { X = 24; Y = 24 }, Site BuiltKind.Storage ]

                let { Intents = intents } = decideOn (atLevel 4 pending)

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the pending census fills the allowance too: nothing re-drops"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a site is a target too: nothing else is planned onto its tile"
            }

            test "the trunks and both container picks keep off the Storage tile" {
                // (24,24) is this fixture's cheapest last step from the
                // source into the spawn: unreserved, the trunk takes it. The
                // reservation is impassable before the trunks are priced, so
                // the lane ends on (24,25) instead. The container picks miss
                // it by construction (ADR 0022): the Storage comes from the
                // clustered ordering, which excludes the working ground,
                // while both container picks draw only from working ground —
                // the Seats and the Upgrade Work Area.
                let colony = withRoadsBuilt (trunkColony 4)
                let { Intents = intents } = decideOn colony

                let storage = sitesOfKind Storage intents |> Set.ofList
                let containers = sitesOfKind Container intents |> Set.ofList

                Expect.isNonEmpty storage "the Storage is planned at RCL4"
                Expect.isNonEmpty containers "the containers drop once their roads stand"

                Expect.isEmpty
                    (Set.intersect storage (homeLayer colony.Spatial).Roads)
                    "no trunk road crosses the Storage's tile"

                Expect.isEmpty
                    (Set.intersect storage containers)
                    "the Storage's tile is never a container's"
            }

            test "the cluster keeps its tiles as the Storage goes from reserved to standing" {
                // The recomputation that can move something: a standing
                // Storage leaves the ordering and frees its slot at once, so
                // the tower and every extension keep the picks they had
                // while the tile was only reserved (ADR 0022).
                let planned = decideOn (atLevel 4 (openRoom 3))

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let built =
                    decideOn (
                        atLevel
                            4
                            (openRoom 3
                             |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ])
                    )

                Expect.equal
                    (sitesOfKind Tower built.Intents)
                    (sitesOfKind Tower planned.Intents)
                    "the tower's pick is the same tile either way"

                Expect.equal
                    (sitesOfKind Extension built.Intents)
                    (sitesOfKind Extension planned.Intents)
                    "and every extension's, in the same order"
            }

            test "a standing Storage changes neither the hauler quota nor the trunk plan" {
                // The spawn stays the trunk hub (ADR 0022): the Storage sits
                // beside it by construction and hires no haul capacity of its
                // own — the quota counts source containers (ADR 0012). One
                // stands on the source's trunk Seat, so the quota is a real
                // number on both sides of the comparison.
                let colony =
                    { trunkColony 4 with
                        Spatial =
                            trunkRoom
                            |> withTargets
                                [ "can-a", { X = 16; Y = 24 }, Structure BuiltKind.Container ]
                    }

                let planned = decideOn colony

                Expect.isGreaterThan
                    planned.Memo.HaulerQuota
                    0
                    "the standing source container hires the haulers the Storage must not"

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let standing =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add storageTile layer.Obstacles
                                })
                            |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ]
                    }

                let built = decideOn standing

                Expect.equal
                    built.Memo.HaulerQuota
                    planned.Memo.HaulerQuota
                    "a Storage hires nothing: the quota reads source containers alone"

                Expect.equal
                    (sitesOfKind Road built.Intents)
                    (sitesOfKind Road planned.Intents)
                    "the trunks keep their endpoints — the spawn stays the hub"
            }
        ]

[<Tests>]
let linkFootingTests =
    testList
        "link footing"
        [
            test "every target served records nothing: the empty list is the guarantee holding" {
                // Both of `footingRoom`'s targets get their tile, so the
                // record is empty — and empty is an answer rather than an
                // absence: it is what the Layout channel says while ADR
                // 0022 and ADR 0027's one-footing-per-target still holds
                // (#77, ADR 0035).
                let { Memo = memo } = decideOn (atLevel 4 footingRoom)

                Expect.isEmpty memo.UnservedFootings "both footings stand; nothing is lost"
            }

            test "a served target names the tile the fold reserved for it" {
                // The other half of the record (#106): the fold holds the
                // target, its kind and the tile in scope at the instant it
                // reserves one, and hands all three back. A bare set of
                // tiles would leave the target-to-tile pairing to be
                // rederived by hand — the second derivation the record
                // exists to remove (ADR 0035).
                let { Memo = memo } = decideOn (atLevel 4 footingRoom)

                Expect.equal
                    memo.ServedFootings
                    [
                        {
                            Target = plannedTile { X = 24; Y = 23 }
                            Kind = FootingKind.SourceContainer
                            Tile = plannedTile { X = 23; Y = 23 }
                        }
                        {
                            Target = plannedTile { X = 24; Y = 24 }
                            Kind = FootingKind.Storage
                            Tile = plannedTile { X = 24; Y = 25 }
                        }
                    ]
                    "both targets, each beside the tile held for its link"

                Expect.equal
                    (memo.ServedFootings
                     |> List.map (fun footing -> RoomPos.pos footing.Tile)
                     |> Set.ofList)
                    footingTiles
                    "and the tiles are the room's own footings, which no site may take"
            }

            test "the sealed room's four targets split three served to one unserved" {
                // `sealedPocketColony`'s four targets split three to one:
                // the sealed source container is the loss (#77) and the
                // other three stand. Neither list is the whole story alone
                // — the shortfall says which guarantee went and the served
                // record says which tiles the rest hold — and no target is
                // in both, because the fold visits each exactly once.
                let { Memo = memo } = decideOn (sealedPocketColony 4)

                let served = memo.ServedFootings |> List.map (fun footing -> footing.Target)
                let unserved = memo.UnservedFootings |> List.map (fun footing -> footing.Target)

                Expect.hasLength served 3 "the three targets the sealed room can still serve"
                Expect.hasLength unserved 1 "and the one it cannot"

                Expect.isEmpty
                    (served |> List.filter (fun target -> List.contains target unserved))
                    "no target is both served and unserved"
            }

            test "a target with no candidate is recorded by tile and kind, never dropped" {
                // W12S28's `10,43`, synthesised: the pocket source's
                // container pick has wall on five sides, its own source on
                // the sixth, the trunk road out on the seventh and a
                // standing extension on the last, so the fold has nothing
                // to reserve for it. That used to fall through to `taken`
                // and leave the room three footings where the ADRs promise
                // four, with no signal anywhere (#77). The room's other
                // three targets are absent from the list, which is the
                // other half of the claim: the fold still reserves
                // everything it can, and only what it cannot is recorded.
                let { Memo = memo } = decideOn (sealedPocketColony 4)

                Expect.equal
                    memo.UnservedFootings
                    [
                        {
                            Target = plannedTile { X = 21; Y = 30 }
                            Kind = FootingKind.SourceContainer
                        }
                    ]
                    "one entry: the sealed source container's pick, and nothing else"

                // And the seal is the whole cause. The same room with its
                // pocket open serves all four targets and records nothing,
                // so sealing one pick costs exactly that one footing: the
                // fold reserves everything it still can, which is the other
                // half of the claim above and cannot be read off a list
                // that only ever names losses.
                let { Memo = control } = decideOn (pocketColony 4)

                Expect.isEmpty
                    control.UnservedFootings
                    "unsealed, the same four targets all get their tile"
            }

            test "no site lands on a Link footing, at any level" {
                // The footings are held from level 0, levels before the
                // engine unlocks links, because the tile never comes back
                // once an extension takes it — and past RCL4, where links
                // would be allowed, nothing is placed on them either: Link
                // is a built kind with no placeable counterpart, so the
                // Layout emits no site for one at any level (ADR 0022).
                for level in 1..8 do
                    let { Intents = intents } = decideOn (atLevel level footingRoom)

                    Expect.isEmpty
                        (placedTiles intents
                         |> List.filter (fun tile -> Set.contains tile footingTiles))
                        $"RCL{level}: no extension, tower, road or container site sits on a footing"
            }

            test "a footing outranks the extensions: the cluster fills one tile further out" {
                // (23,23) is the ordering's third free pick and the footing
                // beside the source container. The footing wins it, so the
                // cluster takes the next tile instead — the allowance still
                // fills, it just reaches one ring wider.
                let { Intents = intents } = decideOn (atLevel 3 footingRoom)

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    10
                    "the RCL3 allowance still fills whole"

                Expect.isFalse
                    (Set.contains { X = 23; Y = 23 } (clusterTiles intents))
                    "the footing's tile is out of the clustered picks"

                Expect.contains
                    (clusterTiles intents)
                    { X = 22; Y = 24 }
                    "the pick behind the footing is drawn in: nothing is lost, the cluster moves out"
            }

            test
                "a footing may sit on a Seat: the working ground is off-limits to the cluster alone" {
                // This source's trunk leaves by the Seat at (24,22), so that
                // Seat is the container pick and the footing beside it wants
                // (24,23) — another Seat. A footing is the one structure
                // footing allowed on working ground (ADR 0022); were it to
                // dodge Seats the way the ordering does, it would fall
                // through to (25,23) and cost the cluster that tile.
                let colony =
                    atLevel
                        4
                        (openRoom 6
                         |> withTargets
                             [
                                 "src-a", { X = 23; Y = 23 }, Source
                                 "ctrl-1", { X = 30; Y = 25 }, Controller
                             ])

                let { Intents = intents } = decideOn colony

                Expect.isFalse
                    (List.contains { X = 24; Y = 23 } (placedTiles intents))
                    "the Seat beside the container is held, not built on"

                Expect.contains
                    (clusterTiles intents)
                    { X = 25; Y = 23 }
                    "the cluster keeps the tile a working-ground dodge would have cost it"
            }

            test "the footings hold their tiles as the container and the Storage are built" {
                // The reservation becomes a structure: the source container
                // on (24,23), the Storage on (24,24). Both picks are judged
                // from geometry that does not move when they are built, so
                // the footings beside them do not move either — (23,23) is
                // buildable again on this tick and still nothing takes it.
                let built =
                    footingRoom
                    |> withTargets [ "can-a", { X = 24; Y = 23 }, Structure BuiltKind.Container ]
                    |> withStanding "sto-1" { X = 24; Y = 24 } (Structure BuiltKind.Storage)

                let { Intents = intents } = decideOn (atLevel 4 built)

                Expect.isNonEmpty (sitesOfKind Extension intents) "the cluster still fills"

                Expect.isEmpty
                    (placedTiles intents |> List.filter (fun tile -> Set.contains tile footingTiles))
                    "both footings are still held, target built or reserved"
            }

            test "a standing Storage keeps the footing beside it, and a pending one too" {
                // (23,23) is the footing beside the Storage's pick at
                // (24,24). The tick the Storage is placed its reservation
                // leaves the clustered ordering, so the footing reads the
                // site's — and then the structure's — own tile instead, or
                // the tile it was holding falls to the next extension.
                let room =
                    openRoom 6
                    |> withTargets
                        [
                            "src-a", { X = 22; Y = 26 }, Source
                            "ctrl-1", { X = 30; Y = 25 }, Controller
                        ]

                let planOf colony =
                    let { Intents = intents } = decideOn (atLevel 4 colony)
                    intents

                let planned = planOf room

                let pending =
                    planOf (
                        room |> withTargets [ "sto-1", { X = 24; Y = 24 }, Site BuiltKind.Storage ]
                    )

                let built =
                    planOf (
                        room
                        |> withStanding "sto-1" { X = 24; Y = 24 } (Structure BuiltKind.Storage)
                    )

                Expect.equal
                    (sitesOfKind Storage planned)
                    [ { X = 24; Y = 24 } ]
                    "the Storage is planned on the ordering's first pick"

                for (label, intents) in
                    [ "reserved", planned; "pending", pending; "standing", built ] do
                    Expect.isFalse
                        (List.contains { X = 23; Y = 23 } (placedTiles intents))
                        $"{label}: the footing beside the Storage keeps its tile"
            }

            test "a standing link keeps its own footing: the plan is the tick-before plan" {
                // A link is a target, so the tick it goes up its tile stops
                // being buildable — the footing reads standing links back
                // into its candidates rather than jumping to the next tile
                // and costing the cluster a second one. This room's Storage
                // already stands off in the corner, so the source
                // container's footing is the only one bidding near the
                // cluster and the jump would be visible.
                let room =
                    openRoom 6
                    |> withTargets
                        [
                            "src-a", { X = 25; Y = 21 }, Source
                            "ctrl-1", { X = 26; Y = 28 }, Controller
                        ]
                    |> withStanding "sto-1" { X = 29; Y = 29 } (Structure BuiltKind.Storage)

                let linked =
                    room |> withStanding "link-1" { X = 23; Y = 23 } (Structure BuiltKind.Link)

                let before = decideOn (atLevel 4 room)
                let after = decideOn (atLevel 4 linked)

                Expect.isFalse
                    (List.contains { X = 23; Y = 23 } (placedTiles after.Intents))
                    "the link's tile leaves the ordering: nothing is planned onto it"

                Expect.equal
                    (placementIntents after.Intents)
                    (placementIntents before.Intents)
                    "the link standing on its footing moves nothing else in the plan"
            }

            test "no clustered structure and no trunk want the same tile" {
                // A footing takes one of the cluster's own picks, so the
                // cluster draws one more tile in behind it — and the
                // reservation the trunk flood was routed around is widened
                // by the footing count for exactly that (ADR 0027), so the
                // drawn-in tile is still ground no trunk was allowed to
                // cross. ADR 0011's precedence survives the push: a road
                // never sits where a structure will.
                let { Intents = intents } = decideOn (atLevel 4 crossedRoom)

                Expect.isNonEmpty (sitesOfKind Road intents) "the trunks are paved"

                Expect.isEmpty
                    (Set.intersect (clusterTiles intents) (sitesOfKind Road intents |> Set.ofList))
                    "the cluster the footings pushed out is still inside the reservation"
            }

            test "the footings survive the placement burst: the next tick asks for nothing" {
                // RCL4 places everything its own level unlocks in one
                // burst, so the tick after it every gap at that level is
                // zero and the tiles it took are carried by the sites
                // themselves — except the footings, which no site ever
                // stands on. The widened window is what still holds
                // them (ADR 0027); without it the trunk flood is free to
                // take a footing the moment the cluster is placed, and the
                // Layout emits a road on the tile it had been reserving
                // since level 0, orphaning the roads it just moved off.
                let { Intents = intents } = decideOn (withPlanPending (atLevel 4 crossedRoom))

                Expect.isEmpty
                    (placementIntents intents)
                    "the whole plan stands where it was asked for: nothing moved, nothing is re-sited"
            }
        ]

[<Tests>]
let unroutedTrunkTests =
    testList
        "unrouted trunk"
        [
            test "every trunk routed records nothing: the empty list is the guarantee holding" {
                // The trunk fixture's one source reaches both goals, so the
                // record is empty — and empty is an answer rather than an
                // absence, exactly as it is for the footing shortfall it
                // rides beside (#107, ADR 0035).
                let { Memo = memo } = decideOn (trunkColony 4)

                Expect.isEmpty memo.UnroutedTrunks "both trunks route; nothing is lost"
            }

            test "a source that loses one goal and keeps the other records exactly one entry" {
                // The detail a careless record gets wrong. The goals are
                // collected per source, so the loss is per (source, goal):
                // with the controller walled off, `src-a` loses its line to
                // the Upgrade Work Area and keeps the one to the spawn.
                // W12S27 from 6,18 is the live counterexample the other way
                // round (#105), and a record keyed on the source alone
                // would be false in both.
                let colony = severedControllerColony 4
                let { Memo = memo; Intents = intents } = decideOn colony

                Expect.equal
                    memo.UnroutedTrunks
                    [
                        {
                            Source = "src-a"
                            Goal = TrunkGoal.UpgradeArea
                        }
                    ]
                    "one entry: the goal that was lost, named beside the source that lost it"

                // And the trunk it kept is paved, which is what makes the
                // entry a loss of one line rather than of the source: a
                // room that paved nothing would be a different claim.
                Expect.isNonEmpty
                    (sitesOfKind Road intents)
                    "the line to the spawn is still routed and still paved"
            }

            test "a source no goal is reachable from records both its goals" {
                // `src-b` walled in on all eight sides: the router hands
                // back the empty path for each goal in turn, and each is an
                // entry of its own. The spawn carries its id because the
                // spawn list is a list (RCL7 adds a second one), where the
                // Upgrade Work Area is the controller's alone.
                let { Memo = memo } = decideOn (enclosedSourceColony 4)

                Expect.equal
                    memo.UnroutedTrunks
                    [
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.UpgradeArea
                        }
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.Spawn "spawn-1"
                        }
                    ]
                    "both goals, and only the enclosed source's"

                // The open source is the control: sealing one source costs
                // exactly that source's trunks, and the same room with the
                // pocket's Seat open loses nothing at all.
                let { Memo = control } = decideOn (pocketColony 4)

                Expect.isEmpty control.UnroutedTrunks "unsealed, every source reaches every goal"
            }
        ]
