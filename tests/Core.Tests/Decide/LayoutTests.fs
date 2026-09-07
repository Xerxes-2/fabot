/// The Layout: what the Planner places and where — the clustered ordering, the
/// trunks, the Storage, the Link footings, the room layer a site is filed under
/// — and the plan memo the census signature keys, which is the Layout's own
/// cache and moves with every input the plan reads (ADR 0017, ADR 0044).
module Fabot.Core.Tests.Decide.LayoutTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

[<Tests>]
let builtKindTests =
    testList
        "built kinds"
        [
            test "matches the engine's spelling, one name per kind" {
                // These strings come back in `structureType` on structures and
                // sites and leave the program in createConstructionSite; the
                // table is the engine's spec, restated so a swapped or
                // misspelt case fails. Other is not among them: it is what an
                // unmatched string classifies to, not a kind the engine names.
                Expect.equal
                    (allBuiltKinds |> List.map builtKindName)
                    [
                        "spawn"
                        "extension"
                        "tower"
                        "road"
                        "container"
                        "storage"
                        "link"
                        "rampart"
                    ]
                    "each BuiltKind maps to its Screeps string"
            }

            test "Refill keeps the spawn, the extensions and the towers fed" {
                // The rank layer's own kinds (ADR 0010). The controller
                // container and the Storage are Refill targets too, but
                // pooled off the projection's stores (ADR 0012, ADR 0023) —
                // they never enter the Refillables list.
                Expect.equal
                    (allBuiltKinds |> List.filter isRefillable)
                    [ BuiltKind.Spawn; BuiltKind.Extension; BuiltKind.Tower ]
                    "the energy-hungry kinds alone are Refillables"

                Expect.isFalse
                    (isRefillable BuiltKind.Other)
                    "an unmodelled kind is no Refillable: the projection reads no free capacity off it"
            }

            test "each kind is whole at its own line: half of max, a floor, or full" {
                // The whole line per kind (ADR 0034), which is also the list
                // of kinds whose hits the projection carries at all: the
                // decaying roads and containers sit at a fraction of max
                // (ADR 0010), a rampart at its floor, and the Keep at full —
                // it does not decay, so below max means damaged. The numbers
                // themselves are the Repair pool's tunables and are not here.
                Expect.equal
                    (allBuiltKinds |> List.map (fun kind -> kind, wholeLine kind))
                    [
                        BuiltKind.Spawn, Some WholeLine.Full
                        BuiltKind.Extension, None
                        BuiltKind.Tower, Some WholeLine.Full
                        BuiltKind.Road, Some WholeLine.Fraction
                        BuiltKind.Container, Some WholeLine.Fraction
                        BuiltKind.Storage, Some WholeLine.Full
                        BuiltKind.Link, None
                        BuiltKind.Rampart, Some WholeLine.Floor
                    ]
                    "one line per kind, and none for the kinds Repair never touches"

                // The Keep is the list the other two rules hang off (the
                // rampart covering and, from #102, safe mode), so it must be
                // exactly the kinds repaired to full: a Keep kind repaired to
                // half would leave the safe-mode trigger armed for every
                // hostile that wandered through afterwards.
                Expect.equal
                    (allBuiltKinds |> List.filter isKeep)
                    (allBuiltKinds |> List.filter (fun kind -> wholeLine kind = Some WholeLine.Full))
                    "the Keep is exactly the kinds whose whole line is full hits"

                Expect.equal
                    (allBuiltKinds |> List.filter isKeep)
                    [ BuiltKind.Spawn; BuiltKind.Tower; BuiltKind.Storage ]
                    "the spawn, the tower and the Storage are the Keep"

                Expect.equal
                    (allBuiltKinds |> List.filter isStored)
                    [ BuiltKind.Container; BuiltKind.Storage ]
                    "the containers and the Storage alone put a store in the projection"

                // `allBuiltKinds` leaves Other out, so no filter above can
                // say anything about it — and Other is the arm with the worst
                // reach: the projection reads hits and a store off every kind
                // these admit, and an unmodelled structure carries neither.
                Expect.isNone
                    (wholeLine BuiltKind.Other)
                    "an unmodelled kind has no whole line and never enters the Repair pool"

                Expect.isFalse (isKeep BuiltKind.Other) "an unmodelled kind is no Keep structure"

                // The Raid log charges damage on the Keep and its cover
                // (ADR 0034) and on nothing else: a chewed road is the
                // colony's ordinary decay, not a raid's cost.
                Expect.equal
                    (allBuiltKinds |> List.filter isDefence)
                    [ BuiltKind.Spawn; BuiltKind.Tower; BuiltKind.Storage; BuiltKind.Rampart ]
                    "the Keep and the ramparts over it are what a raid's damage is read on"

                // Ownership is asked of every kind that has an owner and a
                // whole line: what stands in a room we took is not
                // automatically ours. The decaying kinds have no owner in
                // the engine, so asking would drop every road and container.
                Expect.equal
                    (allBuiltKinds |> List.filter needsOwner)
                    (allBuiltKinds |> List.filter isDefence)
                    "the ownable repairable kinds are exactly the Keep and the ramparts"

                Expect.isFalse
                    (needsOwner BuiltKind.Road)
                    "a road has no owner to ask about: it would vanish from the projection"

                Expect.isFalse (isDefence BuiltKind.Other) "an unmodelled kind is charged no damage"

                Expect.isFalse
                    (isStored BuiltKind.Other)
                    "an unmodelled kind puts no store in the projection"
            }

            test "a creep stands on a road, a container or a rampart, and on nothing else" {
                // Screeps OBSTACLE_OBJECT_TYPES, as the projection reads it:
                // every kind that is not walkable blocks its tile.
                Expect.equal
                    (allBuiltKinds |> List.filter isWalkable)
                    [ BuiltKind.Road; BuiltKind.Container; BuiltKind.Rampart ]
                    "the three kinds a creep may share a tile with"

                Expect.isFalse
                    (isWalkable BuiltKind.Other)
                    "a kind the decision layer does not model blocks its tile: Other never walks"
            }

            test "a placement Intent's kind widens to the built kind of the same name" {
                // The one crossing between the two vocabularies (#75). The
                // Executor spells a site through it and a projection rebuilt
                // on the .NET side classifies its pending sites through it,
                // so a transposed case would place one kind and describe
                // another with nothing in either layer to catch it.
                Expect.equal
                    ([ Extension; Tower; Road; Container; Storage ] |> List.map builtKindOfPlaceable)
                    [
                        BuiltKind.Extension
                        BuiltKind.Tower
                        BuiltKind.Road
                        BuiltKind.Container
                        BuiltKind.Storage
                    ]
                    "each placeable kind widens to its own built kind"
            }
        ]

[<Tests>]
let plannerTests =
    testList
        "planner"
        [
            test "one Harvest task per source" {
                let tasks = planTasks bareRespawn noThreats

                let harvests =
                    tasks
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal
                    harvests
                    [ "src-a"; "src-b" ]
                    "each source gets exactly one Harvest task"
            }

            test "a drained source pools its Harvest task all the same" {
                // ADR 0013's gate, inverted by ADR 0025: the task no longer
                // flickers with the source's stock, because whether a dry
                // rock is worth walking to depends on the walker's body and
                // position — the Matcher's knowledge, not the Planner's.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a"; drained "src-b" 120 ]
                    }

                let harvests =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal
                    harvests
                    [ "src-a"; "src-b" ]
                    "the empty window is a wait to be judged at arrival, not a missing Task"
            }

            test "a controller yields an Upgrade task" {
                let upgrades =
                    planTasks bareRespawn noThreats
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.equal upgrades [ "ctrl-1" ] "the controller gets exactly one Upgrade task"
            }

            test "no Upgrade task without a controller" {
                let tasks = planTasks { bareRespawn with Controller = None } noThreats

                let upgrades =
                    tasks
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.isEmpty upgrades "nothing to upgrade"
            }

            test "each construction site yields a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" }; { Id = "site-2" } ]
                    }

                let builds =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Build siteId -> Some siteId
                        | _ -> None)

                Expect.equal builds [ "site-1"; "site-2" ] "one Build task per construction site"
            }

            test "the spawn and its extensions are one Refill, under the spawn's id" {
                // ADR 0054: the ring is one place a body walks to once, so
                // the extensions do not each carry a Task of their own —
                // which is what let whoever filled one first evaporate a
                // walker's Task every tick or two.
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" 50 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "ext-2" 50 BuiltKind.Extension
                            ]
                    }

                let refills =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal refills [ "spawn-1" ] "the cluster is pooled once, under its spawn"
            }

            test "a full cluster is pooled at all only while some member has room" {
                // The other end of ADR 0054's whole point: `task-gone`
                // fires when the *ring* is full, not when the extension a
                // body happened to be aimed at is. A full spawn beside an
                // empty extension keeps the Task standing.
                let cluster free =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" (fst free) BuiltKind.Spawn
                                refillable "ext-1" (snd free) BuiltKind.Extension
                            ]
                    }

                let refills snapshot =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    (refills (cluster (0, 50)))
                    [ "spawn-1" ]
                    "a full spawn with a hungry extension beside it is still a Refill"

                Expect.equal
                    (refills (cluster (0, 0)))
                    []
                    "the whole ring full is what takes the Refill out of the pool"
            }

            test "extensions with no spawn to key them stay one Task apiece" {
                // The `None` arm of `RefillCluster.ofRefillables` (ADR
                // 0054): a room whose spawn has been destroyed has no
                // cluster, and its extensions are pooled the way every
                // Refillable was before there was one.
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "ext-1" 50 BuiltKind.Extension
                                refillable "ext-2" 0 BuiltKind.Extension
                                refillable "ext-3" 50 BuiltKind.Extension
                            ]
                    }

                let refills =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "ext-1"; "ext-3" ]
                    "only the extensions with free capacity need a Refill"
            }

            test "a tower missing energy gets a Refill task; a full tower gets none" {
                // Same generalized Task, same free-capacity filter (ADR 0010) —
                // a tower is just one more energy-hungry structure to the Planner.
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "tower-2" 0 BuiltKind.Tower
                            ]
                    }

                let refills =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "tower-1" ]
                    "only the tower with free capacity needs a Refill"
            }
        ]

[<Tests>]
let placementTests =
    testList
        "placement"
        [
            test "RCL2 on open terrain places 5 extensions checkerboard, nearest first" {
                let { Intents = intents } = decide (atLevel 2 (openRoom 3)) Map.empty Set.empty None

                // The nearest checkerboard tile (24,24) is the Storage's pick
                // and (24,26) and (26,24) are the two towers' — reservations,
                // not sites: RCL2 allows no tower, but their picks still come
                // first in the one ordering — so the extensions start three
                // tiles in. A golden value of the horizon, not an assertion
                // about the ordering: the rule is unchanged, the list moved.
                // It survived the move to RCL6 (ADR 0055) because that move
                // widens the window's *tail* — RCL6 allows no third tower, so
                // the picks ahead of the extensions are the same two.
                Expect.equal
                    (sitesOfKind Extension intents)
                    [
                        { X = 26; Y = 26 }
                        { X = 23; Y = 23 }
                        { X = 23; Y = 25 }
                        { X = 23; Y = 27 }
                        { X = 25; Y = 23 }
                    ]
                    "the last diagonal neighbour, then rank-2 checkerboard tiles"

                for (room, _, kind) in placementIntents intents do
                    Expect.equal room "W1N1" "sites go in the spawn's room"

                    Expect.isTrue
                        (kind = Extension || kind = Rampart)
                        "the extensions the level unlocks, and the spawn's own rampart"
            }

            test "RCL5 on open terrain plans the whole level: 30 extensions, two towers" {
                // The current level's own filter, with the horizon a level
                // ahead of it (ADR 0055): a room at RCL5 places what RCL5
                // unlocks and no more, however far the reservation reaches.
                // The room is a ring wider than the fixtures beside it because
                // thirty extensions, two towers, the Storage and the footings
                // want more same-colour tiles than `openRoom 3` has.
                let { Intents = intents } = decide (atLevel 5 (openRoom 5)) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    30
                    "RCL5's whole extension allowance, the ten that level adds included"

                Expect.hasLength
                    (sitesOfKind Tower intents)
                    2
                    "both towers RCL5 allows, the second one the horizon held a tile for"
            }

            test "RCL6 on open terrain plans the whole level: 40 extensions, two towers" {
                // The clustered kinds are sized at the horizon and filtered at
                // the current level, so a room standing at the horizon's own
                // level plans everything the engine unlocked there (ADR 0039,
                // ADR 0055). A horizon left behind computes a gap of zero here
                // — the thirty of RCL5 already standing — and asks for none of
                // the ten RCL6 adds, which is why the constant moves before
                // the room reaches the level and not after. A ring wider again
                // than the RCL5 fixture: forty extensions, two towers, the
                // Storage and the footings want the tiles.
                let { Intents = intents } = decide (atLevel 6 (openRoom 6)) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    40
                    "RCL6's whole extension allowance, the ten that level adds included"

                Expect.hasLength
                    (sitesOfKind Tower intents)
                    2
                    "RCL6 allows no third tower, so the horizon holds two tiles and not three"

                // What the ordering owes the level below it, and what a
                // knob's own test cannot say: the filter only ever *adds*.
                // The same room a level down places thirty of these forty
                // tiles and no fortieth of its own, so the ten RCL6 unlocks
                // are picks the ordering had not reached rather than a
                // reshuffle of the thirty already standing.
                let { Intents = below } = decide (atLevel 5 (openRoom 6)) Map.empty Set.empty None

                Expect.isTrue
                    (Set.isSubset
                        (sitesOfKind Extension below |> Set.ofList)
                        (sitesOfKind Extension intents |> Set.ofList))
                    "RCL5's thirty are thirty of RCL6's forty, on the same tiles"

                Expect.hasLength
                    (sitesOfKind Extension below)
                    30
                    "and the level below places what its own level unlocks, not what the horizon reserved"
            }

            test "below RCL2 no placement Intents are emitted" {
                let { Intents = intents } = decide (atLevel 1 (openRoom 3)) Map.empty Set.empty None

                Expect.isEmpty
                    (placementIntents intents)
                    "no extensions allowed at RCL1, and no rampart either: the engine allows neither"
            }

            test "unwalkable tiles are skipped" {
                let room = openRoom 3

                let holed =
                    room
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 24; Y = 24 } Wall layer.Terrain
                        })

                let { Intents = intents } = decide (atLevel 2 holed) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "wall tile is never chosen"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    5
                    "the cap is still reached elsewhere"
            }

            test "occupied tiles are skipped" {
                let blocked =
                    openRoom 3
                    |> withTargets [ "rock-1", { X = 24; Y = 24 }, Structure BuiltKind.Other ]

                let { Intents = intents } = decide (atLevel 2 blocked) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "occupied tile is never chosen"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    5
                    "the cap is still reached elsewhere"
            }

            test "built extensions and pending sites count against the cap" {
                let room =
                    openRoom 3
                    |> withTargets
                        [
                            "ext-1", { X = 24; Y = 24 }, Structure BuiltKind.Extension
                            "ext-2", { X = 24; Y = 26 }, Structure BuiltKind.Extension
                            "site-1", { X = 26; Y = 24 }, Site BuiltKind.Extension
                            "site-2", { X = 26; Y = 26 }, Site BuiltKind.Extension
                        ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None

                Expect.hasLength (sitesOfKind Extension intents) 1 "only the shortfall is placed"
            }

            test "no placement Intents once the allowance is exhausted" {
                let room =
                    openRoom 3
                    |> withTargets
                        [
                            for i in 1..5 ->
                                $"ext-{i}", { X = 22 + i; Y = 22 }, Structure BuiltKind.Extension
                        ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None
                Expect.isEmpty (sitesOfKind Extension intents) "allowance already used up"
            }

            test "the controller's tile is never chosen" {
                // The controller stands on a free same-colour tile the old
                // Placement projection would have offered to a site.
                let room = openRoom 3 |> withTargets [ "ctrl-1", { X = 24; Y = 24 }, Controller ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a target's tile is never chosen"

                // One short of RCL2's cap, and that is the horizon's price
                // paid at today's level (ADR 0039): the controller's own
                // Upgrade Work Area is working ground, so this room offers
                // seven tiles, and the second tower's reservation sits ahead
                // of the extensions in the one ordering.
                Expect.hasLength
                    (sitesOfKind Extension intents)
                    4
                    "the cap is not reached: no tile is spare for the second tower's reservation"
            }

            test "no placement Intents without a projected room" {
                let snapshot =
                    { bareRespawn with
                        Controller = Some(controllerAt 2)
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (placementIntents intents) "nothing to plan around"
            }
        ]

/// The trunk fixture without its source: nothing to pave a trunk from.
let noSourceColony level =
    { trunkColony level with
        Sources = []
        Spatial =
            { trunkRoom with
                TargetKinds = Map.remove "src-a" trunkRoom.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.remove "src-a" layer.TargetPositions
                })
    }

/// The trunk fixture with a second source walled into a pocket: every
/// neighbour of (20,30) is wall terrain except the single Seat east of
/// it — the W12S28-source-B shape (ADR 0012).
let pocketColony level =
    let srcB = { X = 20; Y = 30 }
    let seat = { X = 21; Y = 30 }

    let walled =
        [
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    { X = srcB.X + dx; Y = srcB.Y + dy }
        ]
        |> List.filter (fun tile -> tile <> seat)

    let room =
        trunkRoom
        |> withHome (fun layer ->
            { layer with
                Terrain = (layer.Terrain, walled) ||> List.fold (fun acc t -> Map.add t Wall acc)
            })
        |> withTargets [ "src-b", srcB, Source ]

    { trunkColony level with
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = room
    }

/// The colony with its own road plan already standing: the state the
/// source containers drop in — a container defers to a road site on its
/// tile (one construction site per tile) and coexists with the built road.
///
/// It stands the roads the plan *places*, not the roads it plans, so
/// below the road gate it stands none (#209) and the premise is empty:
/// every caller that needs a road under its container asks from RCL3 up.
let withRoadsBuilt colony =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Roads = sitesOfKind Road intents |> Set.ofList
                })
    }

let chebyshev a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// The clustered ordering's sort key for a fixture whose spawn stands at
/// (25,25): nearest-to-spawn first, ties by x then y (ADR 0011).
let orderKey tile =
    chebyshev tile { X = 25; Y = 25 }, tile.X, tile.Y

[<Tests>]
let layoutTests =
    testList
        "layout"
        [
            test "RCL2 places the extension gap and no tower — and not one road" {
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None

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
                let { Intents = intents } = decide (trunkColony 3) Map.empty Set.empty None
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
                let { Intents = intents } = decide (trunkColony 3) Map.empty Set.empty None

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
                let first = decide (trunkColony 2) Map.empty Set.empty None
                let second = decide (trunkColony 2) Map.empty Set.empty None

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
                let rcl3 = decide (trunkColony 3) Map.empty Set.empty None
                let rcl4 = decide (trunkColony 4) Map.empty Set.empty None
                let roads = sitesOfKind Road rcl3.Intents |> Set.ofList

                // Read off the horizon's own level, where the whole
                // reservation is on the ground — the second tower and the
                // twenty extensions RCL5 and RCL6 add included (ADR 0039, ADR
                // 0055). Below it the check only ever saw the part the level
                // had placed, so this level moves with the horizon.
                let cluster = clusterTiles (decide (trunkColony 6) Map.empty Set.empty None).Intents

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

                let { Intents = intents } = decide colony Map.empty Set.empty None

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

                let { Intents = intents } = decide colony Map.empty Set.empty None

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
                let { Intents = intents } = decide (noSourceColony 3) Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.singleton { X = 33; Y = 27 }
                                })
                            |> withTargets
                                [ "road-site-1", { X = 34; Y = 24 }, Site BuiltKind.Road ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Road intents)
                    "the gap reads the projection's road census: both tiles are claimed"
            }

            test "each source gets one container on the Seat where its trunk starts" {
                // Roads stand from the road gate up (#209), so the level that
                // has a trunk to seat the container beside is RCL3.
                let colony = withRoadsBuilt (trunkColony 3)
                let { Intents = intents } = decide colony Map.empty Set.empty None

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
                let { Intents = intents } = decide (trunkColony 3) Map.empty Set.empty None
                let roads = sitesOfKind Road intents |> Set.ofList

                for tile in sitesOfKind Container intents do
                    Expect.isFalse
                        (Set.contains tile roads)
                        "the container waits for the road on its tile"
            }

            test "the controller container lands in the Work Area beside a trunk" {
                // At the road gate: the trunk it is judged against is a road
                // site, and those are placed from RCL3 up (#209).
                let { Intents = intents } = decide (trunkColony 3) Map.empty Set.empty None
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
                let { Intents = intents } = decide (trunkColony 1) Map.empty Set.empty None

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
                let rcl1 = decide (trunkColony 1) Map.empty Set.empty None

                let trunkRoads =
                    decide (trunkColony 3) Map.empty Set.empty None
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
                    decide
                        { colony with
                            Spatial = colony.Spatial |> withTargets pending
                        }
                        Map.empty
                        Set.empty
                        None

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
                let { Intents = intents } =
                    decide (withRoadsBuilt (pocketColony 3)) Map.empty Set.empty None

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
                let planned = decide colony Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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
                let planned = decide colony Map.empty Set.empty None

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

                let after = decide offPick Map.empty Set.empty None

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
                    (decide built Map.empty Set.empty None).Memo.HaulerQuota
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

                let after = decide offPick Map.empty Set.empty None

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
                let planned = decide colony Map.empty Set.empty None

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

                let after = decide offPick Map.empty Set.empty None

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
                let planned = decide colony Map.empty Set.empty None

                let standing =
                    sitesOfKind Container planned.Intents
                    |> List.mapi (fun i tile -> $"can-{i}", tile, Structure BuiltKind.Container)

                let after =
                    decide
                        { colony with
                            Spatial = colony.Spatial |> withTargets standing
                        }
                        Map.empty
                        Set.empty
                        None

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
                    placementIntents (decide view Map.empty Set.empty None).Intents

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
                    sitesOfKind Extension (decide view Map.empty Set.empty None).Intents

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
                let { Intents = intents } = decide (atLevel 4 (openRoom 3)) Map.empty Set.empty None

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
                let { Intents = intents } = decide (atLevel 3 (openRoom 3)) Map.empty Set.empty None

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

                let { Intents = intents } = decide (atLevel 4 standing) Map.empty Set.empty None

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

                let { Intents = intents } = decide (atLevel 4 pending) Map.empty Set.empty None

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
                let { Intents = intents } = decide colony Map.empty Set.empty None

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
                let planned = decide (atLevel 4 (openRoom 3)) Map.empty Set.empty None

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let built =
                    decide
                        (atLevel
                            4
                            (openRoom 3
                             |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ]))
                        Map.empty
                        Set.empty
                        None

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

                let planned = decide colony Map.empty Set.empty None

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

                let built = decide standing Map.empty Set.empty None

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

/// A tile of the room the Layout fixtures below plan. `openRoom` names
/// its projection "W1N1", and every tile the Layout records carries the
/// room it planned since #216 R3 (ADR 0052 decision 2) — so an
/// expectation written as a grid coordinate joins it back here, and a
/// footing recorded in any other room fails it.
let plannedTile (tile: Pos) : RoomPos = RoomPos.at "W1N1" tile

/// Synthetic footing fixture: the source stands three tiles north of the
/// spawn, so its trunk leaves by (24,23) and the source container is
/// planned there, while the Storage takes the ordering's first pick at
/// (24,24). The tile the container's Link footing wants, (23,23), is one
/// of the cluster's own same-colour tiles — the collision the reservation
/// exists to settle (ADR 0022).
let footingRoom = openRoom 6 |> withTargets [ "src-a", { X = 25; Y = 22 }, Source ]

/// The two tiles `footingRoom` holds as Link footings: one beside the
/// planned source container at (24,23), one beside the Storage at (24,24).
/// Two, not four — the count is one per planned source container plus the
/// controller container and the Storage, and this room projects no
/// controller position.
let footingTiles = Set.ofList [ { X = 23; Y = 23 }; { X = 24; Y = 25 } ]

/// The room with a target standing on a tile and blocking it, the way the
/// projection carries a built Storage or link: a target and an obstacle.
let withStanding id pos kind room =
    withTargets [ id, pos, kind ] room
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.add pos layer.Obstacles
        })

/// A footing fixture whose trunks run through the clustered ring: the
/// source stands against the room's east edge and the controller against
/// its west edge, so the source→controller trunk crosses the whole
/// cluster and the tiles a footing pushes the cluster onto are the ones
/// the trunk would otherwise want (ADR 0022, ADR 0027).
let crossedRoom =
    openRoom 6
    |> withTargets [ "src-a", { X = 30; Y = 26 }, Source ]
    |> withStanding "ctrl-1" { X = 19; Y = 25 } Controller

/// The colony one tick on: every site the Layout just asked for now
/// standing in the projection as a construction site, the obstacle kinds
/// blocking their tile exactly as the engine's own sites do — the state
/// the next tick's plan is computed against. Both halves go through the
/// Core's own tables (#75): a .NET-side projection builder that restated
/// them would drift from the one `buildSpatial` really builds, and the
/// tests would stay green describing a room the bot never sees.
let withPlanPending colony =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    let sites =
        placementIntents intents
        |> List.mapi (fun i (_, pos, kind) -> $"site-{i}", pos, builtKindOfPlaceable kind)

    { colony with
        Spatial =
            withTargets [ for id, pos, kind in sites -> id, pos, Site kind ] colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Obstacles =
                        (layer.Obstacles, sites)
                        ||> List.fold (fun acc (_, pos, kind) ->
                            if isWalkable kind then acc else Set.add pos acc)
                })
    }

/// W12S28's `10,43` shape, synthesised (#77): the pocket source's only
/// Seat is its container's pick, and every one of the eight tiles beside
/// that pick is spoken for — four wall, the source itself, the one trunk
/// road out, and two standing extensions, which is the live room's own
/// tile table in proportion. The extensions are the live loss: `11,43`
/// took one in the RCL4 burst, planned by a bundle that did not yet hold
/// footings back. Nothing is left for the fold to reserve, so this room's
/// guarantee is short by one — and `pocketColony`, the same room without
/// the seal, is the control that serves all four.
let sealedPocketColony level =
    let colony = pocketColony level
    let sealedTiles = [ { X = 21; Y = 29 }; { X = 21; Y = 31 } ]

    let standingExtensions =
        [ "ext-3", { X = 22; Y = 29 }; "ext-4", { X = 22; Y = 31 } ]

    let walled =
        colony.Spatial
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, sealedTiles)
                    ||> List.fold (fun acc tile -> Map.add tile Wall acc)
            })

    { colony with
        Spatial =
            (walled, standingExtensions)
            ||> List.fold (fun room (id, tile) ->
                withStanding id tile (Structure BuiltKind.Extension) room)
    }

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
                let { Memo = memo } = decide (atLevel 4 footingRoom) Map.empty Set.empty None

                Expect.isEmpty memo.UnservedFootings "both footings stand; nothing is lost"
            }

            test "a served target names the tile the fold reserved for it" {
                // The other half of the record (#106): the fold holds the
                // target, its kind and the tile in scope at the instant it
                // reserves one, and hands all three back. A bare set of
                // tiles would leave the target-to-tile pairing to be
                // rederived by hand — the second derivation the record
                // exists to remove (ADR 0035).
                let { Memo = memo } = decide (atLevel 4 footingRoom) Map.empty Set.empty None

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
                let { Memo = memo } = decide (sealedPocketColony 4) Map.empty Set.empty None

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
                let { Memo = memo } = decide (sealedPocketColony 4) Map.empty Set.empty None

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
                let { Memo = control } = decide (pocketColony 4) Map.empty Set.empty None

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
                    let { Intents = intents } =
                        decide (atLevel level footingRoom) Map.empty Set.empty None

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
                let { Intents = intents } = decide (atLevel 3 footingRoom) Map.empty Set.empty None

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

                let { Intents = intents } = decide colony Map.empty Set.empty None

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

                let { Intents = intents } = decide (atLevel 4 built) Map.empty Set.empty None

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
                    let { Intents = intents } = decide (atLevel 4 colony) Map.empty Set.empty None
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

                let before = decide (atLevel 4 room) Map.empty Set.empty None
                let after = decide (atLevel 4 linked) Map.empty Set.empty None

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
                let { Intents = intents } = decide (atLevel 4 crossedRoom) Map.empty Set.empty None

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
                let { Intents = intents } =
                    decide (withPlanPending (atLevel 4 crossedRoom)) Map.empty Set.empty None

                Expect.isEmpty
                    (placementIntents intents)
                    "the whole plan stands where it was asked for: nothing moved, nothing is re-sited"
            }
        ]

/// The trunk fixture cut in two: a wall ridge down x=30 severs the
/// controller and its whole Upgrade Work Area from the rest of the room,
/// leaving the source and the spawn together on the west side. `src-a`
/// still routes its trunk to the spawn and can route none to the Work
/// Area — the per-goal shape #107 records, and the one a record keyed on
/// the source alone would get wrong.
let severedControllerColony level =
    let colony = trunkColony level

    let ridge = [ for y in 15..35 -> { X = 30; Y = y } ]

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, ridge)
                        ||> List.fold (fun acc tile -> Map.add tile Wall acc)
                })
    }

/// The pocket fixture with its one Seat walled shut: every one of `src-b`'s
/// eight neighbours is wall, so no goal is reachable from it at all and
/// both its trunks are dropped. `src-a`, in the open, keeps both — the
/// record names the source as well as the goal.
let enclosedSourceColony level =
    let colony = pocketColony level

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.add { X = 21; Y = 30 } Wall layer.Terrain
                })
    }

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
                let { Memo = memo } = decide (trunkColony 4) Map.empty Set.empty None

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
                let { Memo = memo; Intents = intents } = decide colony Map.empty Set.empty None

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
                let { Memo = memo } = decide (enclosedSourceColony 4) Map.empty Set.empty None

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
                let { Memo = control } = decide (pocketColony 4) Map.empty Set.empty None

                Expect.isEmpty control.UnroutedTrunks "unsealed, every source reaches every goal"
            }
        ]

/// The step-weight grid ADR 0032's guard compares, for the room the caller
/// names. The room is the caller's rather than the fixture's home since
/// #169: the walk table now holds an entry per *goal* room and the far
/// leg's entry is a pure function of that room's grid, so a guard that
/// could only ask about home would pin the pairing in one room while the
/// memo reads every projected one — which is the asymmetry
/// `Atlas.stepWeights` was already given a room parameter for. Read off
/// the projection rather than retyped as a literal: `stepWeights` answers
/// every tile impassable for a room the projection does not carry (ADR
/// 0004, ADR 0041), so a literal that drifted from its fixture would leave
/// the one `sequenceEqual` in the group comparing two empty grids and
/// passing whatever the census did.
let private stepGridOf (room: string) (snapshot: ColonyView) =
    Atlas.stepWeights (Atlas.ofView snapshot) room

/// The same grid for the colony's own room — the reading every home-room
/// perturbation in the guard group is compared through.
let private homeGridOf (snapshot: ColonyView) =
    stepGridOf (SpatialInfo.homeName snapshot.Spatial) snapshot

[<Tests>]
let censusSignatureTests =
    testList
        "census signature"
        [
            // Every census input, perturbed alone, moves the signature —
            // the test surface ADR 0017 demands: a missed input would stall
            // the Layout until a reset instead of failing here.
            test "a structure appearing moves the signature" {
                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the standing census is a signature input"
            }

            test "a standing Storage is its own kind in the signature" {
                let standing kind =
                    trunkColony 2 |> withTarget "sto-1" { X = 24; Y = 24 } (Structure kind)

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (trunkColony 2))
                    "a Storage is a Structure: the standing census carries it (ADR 0022)"

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (standing BuiltKind.Other))
                    "and it is a kind of its own, not the unmodelled kind it used to project as"
            }

            test "a structure moving moves the signature" {
                let colony = trunkColony 2

                let moved =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "ext-1" { X = 24; Y = 27 } layer.TargetPositions
                                })
                    }

                Expect.notEqual
                    (censusSignature moved)
                    (censusSignature colony)
                    "the census is (kind, position), not a count"
            }

            test "a pending site appearing moves the signature" {
                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the pending census is a signature input"
            }

            test "a structure and a site of the same kind on the same tile differ" {
                let standing =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Structure BuiltKind.Container)

                let pending =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Site BuiltKind.Container)

                Expect.notEqual
                    (censusSignature standing)
                    (censusSignature pending)
                    "a site becoming a structure is a census change"
            }

            test "the controller level moves the signature" {
                Expect.notEqual
                    (censusSignature (trunkColony 3))
                    (censusSignature (trunkColony 2))
                    "the level gates allowances, so it is a signature input"
            }

            test "a second room's standing container joins the signature under its own name" {
                // The widening #116's forward note booked and #149 spent
                // (ADR 0042): the hauler quota folds the containers of
                // every projected room and prices each at the rate that
                // room is held at, so both of those are memo inputs now
                // and the signature that gates the memo has to carry them.
                // #121's narrowing to the home layer was right while the
                // memo held nothing but home's; this is the tick it stops
                // being.
                let colony = trunkColony 2

                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let joined =
                    colony
                    |> withOutpost
                        "W1N2"
                        [
                            "src-out", { X = 24; Y = 24 }, Source
                            "can-out", { X = 24; Y = 25 }, Structure BuiltKind.Container
                        ]
                        ground

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature colony)
                    "an outpost's standing container is a census entry: it hires haulers"

                // The room is *in* the entry and not merely implied by the
                // room list, because two rooms hold the same coordinates.
                // Pairwise, one rival at a time: both sides below project
                // W1N2 with the same ground, the same source and the same
                // control, and carry the same container id at the same
                // (24,25) — the only thing that moves is which room's
                // layer places it.
                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                // The widening itself, with nothing else moving: the same
                // room list, the same held rates, the same home layer —
                // the container standing in W1N2 is the only difference.
                // Joined against the home layer alone (#121's rule) these
                // two sign the same string, and the memo hands back the
                // quota from before the container stood.
                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature bare)
                    "a standing structure in a second room is a census entry of its own"

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature (
                        bare
                        |> withTarget "can-out" { X = 24; Y = 25 } (Structure BuiltKind.Container)
                    ))
                    "the same container at the same coordinates in the other room is another census"

                // And not the container kind alone. The quota prices that
                // container by a round trip flooded over the outpost's
                // step-weight grid, and `World.seenFacts` lays a
                // room's `Roads` and `Obstacles` out of the same
                // every-owner structure array the kind census comes from —
                // so a road paved along the haul lane, or a hostile core
                // standing on it, moves a number the memo holds. A
                // standing census filtered down to `Container` outside
                // home would be ADR 0017's signature gap.
                let paved =
                    colony
                    |> withOutpost
                        "W1N2"
                        [
                            "src-out", { X = 24; Y = 24 }, Source
                            "road-out", { X = 24; Y = 26 }, Structure BuiltKind.Road
                        ]
                        ground

                Expect.notEqual
                    (censusSignature paved)
                    (censusSignature bare)
                    "a second room's road prices its haul, so it is a signature input too"

                // And the room list itself, because the rate is signed per
                // projected room: a room that joins carrying nothing is a
                // room the quota can fold a container out of the tick one
                // stands there, and its held rate is what that container
                // would be priced at.
                Expect.notEqual
                    (censusSignature (colony |> withOutpost "W1N2" [] ground))
                    (censusSignature colony)
                    "a room joining the projection brings its own held rate into the signature"

                Expect.notEqual
                    (censusSignature (
                        colony
                        |> withTarget "can-out" { X = 24; Y = 25 } (Structure BuiltKind.Container)
                    ))
                    (censusSignature colony)
                    "the home room's own container still moves it"
            }

            test "the room name moves the signature" {
                let colony = trunkColony 2

                // The same geometry, carried under the new name: the layer
                // is keyed by room (ADR 0041), so a rename that left the
                // tiles filed under the old key would move the signature by
                // emptying the room rather than by naming it — and the
                // input under test would go unmeasured.
                let renamed =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RoomName = Some "W2N2"
                                Rooms = Map.ofList [ "W2N2", homeLayer colony.Spatial ]
                            }
                    }

                Expect.notEqual
                    (censusSignature renamed)
                    (censusSignature colony)
                    "terrain is keyed by the room, so the name is a signature input"
            }

            test "who holds the home room moves the signature" {
                // The hauler quota's second load-bearing input since ADR
                // 0042: it prices each container at its source's own
                // output, and that output is read off `RoomControl`. A
                // vision fact riding a census memo has to be signed, or
                // the memo hands back a quota sized for the held rate on
                // the tick the room stops being held — the signature gap
                // ADR 0017 names as its failure mode.
                let colony = trunkColony 2

                let holding control =
                    { colony with
                        RoomControl = homeControl |> Map.map (fun _ _ -> control)
                    }

                Expect.notEqual
                    (censusSignature (holding neutralRoom))
                    (censusSignature (holding ownedRoom))
                    "a room that stopped being held prices its sources at half: a memo input"

                Expect.equal
                    (censusSignature (holding (reservedRoom true 4000)))
                    (censusSignature (holding ownedRoom))
                    "owned or reserved by us is one rate, so the two sign the same"

                Expect.notEqual
                    (censusSignature { colony with RoomControl = Map.empty })
                    (censusSignature (holding neutralRoom))
                    "and no vision at all is a third answer, not the neutral one (ADR 0004)"
            }

            test "the ticks left on a reservation leave the signature alone" {
                // The half of the reservation the quota does *not* read.
                // `TicksToEnd` decays by one every tick, so signing it
                // would throw the Layout and the walk table away on every
                // tick the colony holds an outpost — the memo would never
                // survive its own input.
                let holding control =
                    { trunkColony 2 with
                        RoomControl = homeControl |> Map.map (fun _ _ -> control)
                    }

                Expect.equal
                    (censusSignature (holding (reservedRoom true 4000)))
                    (censusSignature (holding (reservedRoom true 3999)))
                    "the rate is the input, and the countdown under it is not"
            }

            test "everything outside the census leaves the signature alone" {
                let colony = trunkColony 2

                // The bank is perturbed in its Available alone: the
                // Capacity beside it is a function of the standing
                // spawn/extension census and the controller level, so it is
                // covered rather than absent (ADR 0017) — which is what
                // lets the successor body a lead is priced for ride this
                // signature too (ADR 0032).
                let perturbed =
                    { colony with
                        Time = colony.Time + 100
                        Bank = bank 0 300
                        Sources = [ drained "src-a" 120 ]
                        Creeps = [ worker "w1" 25 25 ]
                        Hostiles =
                            [
                                {
                                    Id = "h1"
                                    Owner = "raider"
                                    Pos = RoomPos.at "W1N1" { X = 30; Y = 25 }
                                    Body = [ Attack; Move ]
                                    TicksToLive = Engine.creepLifetime
                                }
                            ]
                        ConstructionSites = [ { Id = "site-9" } ]
                        Spatial =
                            { colony.Spatial with
                                Hits = Map.ofList [ "ext-1", { Hits = 1; HitsMax = 3000 } ]
                                Stores = Map.ofList [ "ext-1", 50 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 25 } ]
                                })
                            |> withTargets [ "pile-1", { X = 22; Y = 25 }, Dropped ]
                    }

                Expect.equal
                    (censusSignature perturbed)
                    (censusSignature colony)
                    "creeps, stores, hits, drops, hostiles, bank and tick are not census"

                // ADR 0032's guard, the inverse of every test above: the
                // spawn walks behind the leads are recalled on this
                // signature alone, so two views it calls equal have to
                // lay the same weight grid. A weights input the signature
                // missed would price leads off a stale grid until a global
                // reset, and would fail here rather than in the colony.
                Expect.sequenceEqual
                    (homeGridOf perturbed)
                    (homeGridOf colony)
                    "and the grid the walks flood over is bitwise the same"

                // The same pairing in the room the walk table only started
                // reading with #169: the far leg's entry is a pure function
                // of the *outpost's* grid, so a ColonyView the signature calls
                // equal has to lay that grid bitwise too. Perturbed out
                // there and not at home, or the assertion would be about the
                // home layer twice over: a creep standing in the outpost and
                // a raider beside it are vision facts, and a grid is
                // terrain, roads and obstacles alone.
                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let held =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                let seen =
                    { held with
                        Hostiles =
                            [
                                {
                                    Id = "h2"
                                    Owner = "raider"
                                    Pos = RoomPos.at "W1N2" { X = 26; Y = 26 }
                                    Body = [ Attack; Move ]
                                    TicksToLive = Engine.creepLifetime
                                }
                            ]
                        Spatial =
                            { held.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { SpatialInfo.layerOf held.Spatial "W1N2" with
                                            CreepPositions = Map.ofList [ "w1", { X = 25; Y = 25 } ]
                                        }
                                        held.Spatial.Rooms
                            }
                    }

                Expect.equal
                    (censusSignature seen)
                    (censusSignature held)
                    "a creep and a raider in the outpost are no census of that room either"

                Expect.sequenceEqual
                    (stepGridOf "W1N2" seen)
                    (stepGridOf "W1N2" held)
                    "and the grid the far leg floods over is bitwise the same"
            }

            // The three weights inputs beside the terrain, each perturbed
            // alone (ADR 0032). Each test asserts the pairing rather than
            // the signature alone: the perturbation moves the grid the
            // recalled walks flood over, and it moves the signature they
            // are recalled on. A census that held still through one of them
            // would price leads off a grid the room has left.
            test "a built road moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let paved =
                    let placed = colony |> withTarget "road-1" tile (Structure BuiltKind.Road)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.add tile layer.Roads
                                })
                    }

                Expect.notEqual
                    (homeGridOf paved)
                    (homeGridOf colony)
                    "a road discounts the ground under it"

                Expect.notEqual
                    (censusSignature paved)
                    (censusSignature colony)
                    "and the standing census carries it, so the signature moves with it (ADR 0010)"
            }

            test "an obstacle structure moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let blocked =
                    let placed = colony |> withTarget "twr-1" tile (Structure BuiltKind.Tower)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add tile layer.Obstacles
                                })
                    }

                Expect.notEqual
                    (homeGridOf blocked)
                    (homeGridOf colony)
                    "an obstacle closes its tile to every flood"

                Expect.notEqual
                    (censusSignature blocked)
                    (censusSignature colony)
                    "and the standing census carries it, so the signature moves with it"
            }

            test "an obstacle site moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let pending =
                    let placed = colony |> withTarget "twr-site" tile (Site BuiltKind.Tower)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add tile layer.Obstacles
                                })
                    }

                Expect.notEqual
                    (homeGridOf pending)
                    (homeGridOf colony)
                    "the engine refuses a creep its own obstacle site, so it blocks like the structure"

                Expect.notEqual
                    (censusSignature pending)
                    (censusSignature colony)
                    "and the pending census carries it, so the signature moves with it"
            }

            test "an obstacle site in an outpost moves the signature" {
                // The fourth weights input, and the one #169 made
                // load-bearing: the walk table's far-leg entry is a pure
                // function of the *goal* room's grid, so every input of
                // that grid has to be in the signature exactly as the home
                // room's are (ADR 0032). `World.seenFacts` folds
                // every scanned room's obstacle-kind construction sites
                // into that room's `Obstacles` — the engine refuses a creep
                // its own site wherever it stands — so a pending census
                // read in the home layer alone would leave an outpost's
                // closed tile unsigned, and a lead priced through ground
                // the successor cannot cross would be recalled for the life
                // of the census: ADR 0017's signature gap, in the room the
                // memo has just started reading.
                let colony = trunkColony 2
                let tile = { X = 24; Y = 26 }

                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                let sited =
                    let placed =
                        colony
                        |> withOutpost
                            "W1N2"
                            [
                                "src-out", { X = 24; Y = 24 }, Source
                                "twr-site-out", tile, Site BuiltKind.Tower
                            ]
                            ground

                    { placed with
                        Spatial =
                            { placed.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { SpatialInfo.layerOf placed.Spatial "W1N2" with
                                            Obstacles = Set.singleton tile
                                        }
                                        placed.Spatial.Rooms
                            }
                    }

                Expect.notEqual
                    (stepGridOf "W1N2" sited)
                    (stepGridOf "W1N2" bare)
                    "the site closes its tile in the outpost's grid, which the far leg floods"

                Expect.notEqual
                    (censusSignature sited)
                    (censusSignature bare)
                    "so the pending census reaches every projected room, not the home layer alone"

                Expect.notEqual
                    (stepGridOf "W1N2" bare)
                    (stepGridOf "W1N2" colony)
                    "the premise: a room the projection carries no layer for is all impassable (ADR 0004), so neither grid compared above is an empty one passing whatever the census did"
            }
        ]

/// The colony with the given creeps standing on the given tiles. A placed
/// creep beside a placed spawn is all it takes to price a lead, and pricing
/// one floods out of the spawner (ADR 0026).
let staffedColony creeps positions colony =
    { colony with
        Creeps = creeps
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

/// A memo whose site Intents are a sentinel no computation would produce:
/// reuse is then observable verbatim at the decide seam.
let sentinelMemo snapshot =
    {
        Signature = censusSignature snapshot
        SiteIntents = [ PlaceConstructionSite(RoomPos.at "W1N1" { X = 1; Y = 1 }, Tower) ]
        UnservedFootings = []
        ServedFootings = []
        UnroutedTrunks = []
        DeferredContainers = []
        HaulerQuota = 0
        HaulerDemand = []
        HaulerLoad = 0
        Walks = WalkTable()
    }

[<Tests>]
let planMemoTests =
    testList
        "plan memo"
        [
            test "a memo with the matching signature is reused verbatim" {
                let snapshot = trunkColony 2
                let memo = sentinelMemo snapshot
                let decision = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (placementIntents decision.Intents)
                    [ "W1N1", { X = 1; Y = 1 }, Tower ]
                    "the memo's site Intents pass through, nothing recomputes"

                Expect.equal decision.Memo memo "the memo rides out unchanged for next tick"
            }

            test "an added structure invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "an added site invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test
                "the Layout's records are census-derived: recalled with the plan, recomputed with it" {
                // #77's record, #106's and #107's join the site Intents and
                // the hauler quota under ADR 0017's standing invitation —
                // same census, same losses — so none is rederived per tick.
                // The sentinel says so in both directions: a memo whose
                // signature holds hands its own empty records back for a
                // room that has in fact lost a footing and reserved three,
                // and a stale one recomputes to what the room really has.
                let colony = sealedPocketColony 4

                let lost =
                    [
                        {
                            Target = plannedTile { X = 21; Y = 30 }
                            Kind = FootingKind.SourceContainer
                        }
                    ]

                let matching = decide colony Map.empty Set.empty (Some(sentinelMemo colony))

                Expect.isEmpty
                    matching.Memo.UnservedFootings
                    "a matching signature reuses the memo's record; nothing recomputes"

                Expect.isEmpty
                    matching.Memo.ServedFootings
                    "and reuses the served record with it, for a room that reserved three"

                let stale = decide colony Map.empty Set.empty (Some(sentinelMemo (trunkColony 2)))
                let memoless = decide colony Map.empty Set.empty None

                Expect.equal
                    stale.Memo.UnservedFootings
                    lost
                    "a moved signature recomputes the record with the rest of the plan"

                Expect.equal
                    memoless.Memo.UnservedFootings
                    lost
                    "and a memoless tick derives the same loss"

                Expect.hasLength
                    stale.Memo.ServedFootings
                    3
                    "the served record is recomputed too, not left at the stale memo's empty"

                Expect.equal
                    stale.Memo.ServedFootings
                    memoless.Memo.ServedFootings
                    "tile for tile what a memoless tick reserves"
            }

            test "the unrouted trunks ride the memo with the rest of the plan" {
                // #107's record on the same seam, over a room that has in
                // fact lost a trunk: a matching signature hands back the
                // memo's own empty list rather than rederiving the loss,
                // and a moved one recomputes to exactly what a memoless
                // tick finds. ADR 0017's guarantee is that a recalled plan
                // reports what it reported when it was computed, and a
                // record that quietly recomputed itself would break it.
                let colony = enclosedSourceColony 4

                let dropped =
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

                let matching = decide colony Map.empty Set.empty (Some(sentinelMemo colony))

                Expect.isEmpty
                    matching.Memo.UnroutedTrunks
                    "a matching signature reuses the memo's record; nothing recomputes"

                let stale = decide colony Map.empty Set.empty (Some(sentinelMemo (trunkColony 2)))
                let memoless = decide colony Map.empty Set.empty None

                Expect.equal
                    stale.Memo.UnroutedTrunks
                    dropped
                    "a moved signature recomputes the record with the rest of the plan"

                Expect.equal
                    memoless.Memo.UnroutedTrunks
                    dropped
                    "and a memoless tick derives the same loss"
            }

            test "a level-up invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)
                let decision = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "the memo's hauler quota feeds spawn planning; a stale one is discarded" {
                // The trunk colony has no source containers, so a fresh
                // quota is 0 and the sentinel's 3 is observable: with a
                // matching memo the hauler gap wins the casting order,
                // with a stale one the fresh quota casts a worker again.
                let snapshot =
                    { trunkColony 2 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let memo =
                    { sentinelMemo snapshot with
                        HaulerQuota = 3
                        HaulerDemand = []
                        HaulerLoad = 0
                    }

                let castNames decision =
                    spawnIntents decision.Intents |> List.map (fun (_, _, name) -> name)

                let reused = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (castNames reused)
                    [ "hauler-42-Spawn1" ]
                    "the memo's quota opens a hauler gap the casting order fills first"

                let stale = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (castNames stale)
                    (castNames fresh)
                    "a stale memo's quota is recomputed, never reused"
            }

            test "decide without a memo emits one keyed to this census" {
                let snapshot = trunkColony 2
                let decision = decide snapshot Map.empty Set.empty None

                Expect.equal
                    decision.Memo.Signature
                    (censusSignature snapshot)
                    "the memo carries the census it was computed from"

                let next = decide snapshot Map.empty Set.empty (Some decision.Memo)

                Expect.equal
                    next.Intents
                    decision.Intents
                    "feeding the memo back reproduces the tick verbatim"
            }

            test "the spawn walks ride the memo while the census holds" {
                // ADR 0032. The flood a lead is priced off reads nothing
                // but the census, so the next tick under the same signature
                // fills the table it was handed rather than one of its own:
                // a row the first tick never priced lands beside the first
                // tick's entry instead of in a table nobody keeps.
                let heavy name =
                    creepWith name 0 50 [ Work; Work; Work; Work; Carry; Move ]

                let lone =
                    trunkColony 2 |> staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let joined =
                    trunkColony 2
                    |> staffedColony
                        [ worker "w1" 0 50; heavy "a1" ]
                        [ "w1", { X = 22; Y = 25 }; "a1", { X = 23; Y = 25 } ]

                Expect.equal
                    (censusSignature joined)
                    (censusSignature lone)
                    "a creep arriving is not a census change"

                let first = decide lone Map.empty Set.empty None

                Expect.equal
                    first.Memo.Walks.Count
                    1
                    "the tick flooded once out of the spawn, into the table the memo carries"

                let workerFlood =
                    first.Memo.Walks |> Seq.map (fun entry -> entry.Value) |> Seq.exactlyOne

                let second = decide joined Map.empty Set.empty (Some first.Memo)

                Expect.isTrue
                    (obj.ReferenceEquals(second.Memo.Walks, first.Memo.Walks))
                    "an unchanged census hands the same table on"

                Expect.equal
                    second.Memo.Walks.Count
                    2
                    "and the second row's flood lands in it, beside the first tick's"

                Expect.isTrue
                    (second.Memo.Walks
                     |> Seq.exists (fun entry -> obj.ReferenceEquals(entry.Value, workerFlood)))
                    "the row the first tick priced was recalled, not flooded again"

                Expect.equal
                    second.Intents
                    (decide joined Map.empty Set.empty None).Intents
                    "a recalled walk decides exactly what a fresh flood decides"
            }

            test "a moved census drops the whole walk table" {
                // The Layout's own granularity (ADR 0032): a moved
                // signature may have moved the weights or the body the walk
                // is priced for, and telling which is a dependency tracker
                // the memo does not have.
                let staffed = staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let first = decide (staffed (trunkColony 2)) Map.empty Set.empty None

                let levelled =
                    decide (staffed (trunkColony 3)) Map.empty Set.empty (Some first.Memo)

                Expect.isFalse
                    (obj.ReferenceEquals(levelled.Memo.Walks, first.Memo.Walks))
                    "a level-up gets a table of its own"

                // The same moved census over a colony with no creep to lead
                // shows what "dropped whole" means: nothing at all rides
                // across, not merely a stale entry priced again.
                let emptied = decide (trunkColony 3) Map.empty Set.empty (Some first.Memo)

                Expect.equal emptied.Memo.Walks.Count 0 "the memo's table went with its signature"
            }
        ]

/// Two rooms whose coordinates collide on purpose (ADR 0041). At home:
/// the controller at (25,22), the buffer container "can-home" two tiles
/// off it, and a source far away at (20,30) — so the buffer is the
/// controller's and no source's. In the outpost: a source at (25,25),
/// range 1 from the home buffer's coordinates, and a container at
/// (25,23), range 1 from the home controller's. Nothing here is nearer
/// than a room boundary to anything it collides with.
let collidingRooms =
    { atLevel
          2
          (openRoom 8
           |> withTargets
               [
                   "ctrl-1", { X = 25; Y = 22 }, Controller
                   "can-home", { X = 25; Y = 24 }, Structure BuiltKind.Container
                   "src-a", { X = 20; Y = 30 }, Source
               ]) with
        Sources = [ source "src-a"; source "src-out" ]
    }
    |> withOutpost
        "W1N2"
        [
            "src-out", { X = 25; Y = 25 }, Source
            "can-out", { X = 25; Y = 23 }, Structure BuiltKind.Container
        ]
        [
            for x in 20..30 do
                for y in 20..30 -> { X = x; Y = y }, Plain
        ]

/// A home container stranded six tiles from the spawn and serving no home
/// source, with an outpost source one tile from its coordinates — the
/// hauler quota's half of the same collision. Six tiles because the quota
/// is a round trip: a container beside the spawn prices at zero and would
/// hire nobody whichever room the source stood in.
let strandedContainer sourceRoom =
    let home =
        openRoom 8
        |> withTargets
            [
                "can-far", { X = 25; Y = 31 }, Structure BuiltKind.Container
                "src-a", { X = 19; Y = 19 }, Source
            ]

    let colony =
        { bareRespawn with
            Sources = [ source "src-a"; source "src-out" ]
            Spatial = home
        }

    let strayed = "src-out", { X = 25; Y = 32 }, Source

    if Some sourceRoom = home.RoomName then
        { colony with
            Spatial = colony.Spatial |> withTargets [ strayed ]
        }
    else
        colony |> withOutpost sourceRoom [ strayed ] []

/// The mirror of the collision: a home source at (25,31) and an outpost
/// container one tile off its coordinates, in the outpost. The quota is
/// flooded over the home room's grid, so a container it does not place
/// must never reach the arithmetic at all.
let outpostContainerColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = openRoom 8 |> withTargets [ "src-a", { X = 25; Y = 31 }, Source ]
    }
    |> withOutpost "W1N2" [ "can-out", { X = 25; Y = 32 }, Structure BuiltKind.Container ] []

[<Tests>]
let roomLayerTests =
    testList
        "room layer"
        [
            test "a container belongs to the source and the controller of its own room" {
                let refills =
                    planTasks collidingRooms noThreats
                    |> List.choose (function
                        | Refill id -> Some id
                        | _ -> None)

                // Room-blind, both judgements invert: the home buffer reads
                // as the outpost source's container and drops out of the
                // pool, and the outpost's container reads as the home
                // controller's buffer and enters it.
                Expect.equal
                    refills
                    [ "can-home" ]
                    "the upgrade buffer is the container in the controller's own room"
            }

            test "an outpost source's coordinates hire no haulers at home" {
                // Pairwise, one rival at a time: the same container, the
                // same source, the same coordinates — only the room the
                // source stands in moves.
                Expect.equal
                    (quotaOf (strandedContainer "W1N2"))
                    0
                    "a source across a room boundary makes no container a source container"

                Expect.isGreaterThan
                    (quotaOf (strandedContainer "W1N1"))
                    0
                    "the same source at home does, and hires for the haul"

                // And the mirror, because the quota picks a room twice
                // over: since #149 it folds the containers of every
                // projected room, but each is judged against the sources
                // of *its own* room — so an outpost container beside a
                // home source's coordinates serves no rock and is priced
                // by nothing. The failure this guards is the container
                // being paired with the home rock and then flooded over
                // home terrain, hiring a fleet for a haul nobody makes.
                Expect.equal
                    (quotaOf outpostContainerColony)
                    0
                    "a container whose own room places no rock it serves hires nobody"
            }

            test "the home room keeps its own targets after a second one has joined" {
                // A target added to the home room after an outpost layer is
                // already in the projection lands beside that layer, never
                // over it — `Rooms` is a map keyed by room name and every
                // funnel here merges into the entry it names. Worth pinning
                // because the failure is silent in the direction a fixture
                // cannot see: a home container the projection dropped
                // produces no Refill and no quota, and reads as "the room
                // rule rejected it" when in fact no reader was ever shown
                // it.
                let late =
                    collidingRooms
                    |> withTarget "can-late" { X = 26; Y = 22 } (Structure BuiltKind.Container)

                let refills =
                    planTasks late noThreats
                    |> List.choose (function
                        | Refill id -> Some id
                        | _ -> None)
                    |> List.sort

                Expect.equal
                    refills
                    [ "can-home"; "can-late" ]
                    "a container added after the outpost joined is still the controller's"
            }

            test "a projection that names no room files and reads under the empty name" {
                // The convention `SpatialInfo.homeName` spells, and the one
                // every fixture here that never sets `RoomName` rests on:
                // tiles and no room name is this colony's own room written
                // without saying so, and the empty name is both where its
                // geometry is filed and where every home query looks for
                // it. Its only pin used to be a test of the bridge, so it
                // went when the bridge did; the convention did not go with
                // it. A site that spelled the unnamed room differently
                // would file the home room under one name and read it under
                // another, and ADR 0004 would answer every home query with
                // the empty set rather than throwing — silent in the one
                // direction a fixture cannot see.
                let unnamed = spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                Expect.equal (SpatialInfo.homeName unnamed) "" "the unnamed room's own name"

                Expect.equal
                    (unnamed.Rooms |> Map.toList |> List.map fst)
                    [ "" ]
                    "and the one room it carries is filed under it"

                Expect.equal
                    (homeLayer unnamed).TargetPositions
                    (Map.ofList [ "src-a", { X = 10; Y = 10 } ])
                    "so a home query reads that geometry back, not an empty layer"

                Expect.stringStarts
                    (censusSignature { bareRespawn with Spatial = unnamed })
                    "|"
                    "and the census signature spells that room the same empty name"
            }
        ]

[<Tests>]
let squareRingTests =
    testList
        "the square ring"
        [
            test "two bodies on the move meeting on a road ring pass each other (#225)" {
                // #225 (user's geometry): a road ring around one obstacle,
                // two bodies bound opposite ways meet on one edge, both
                // switch to the other edge together, meet again, and so on.
                let ring =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            // A 3×3 block of extensions with a road ring around it and
                            // a plain approach row on each side: the two lanes round
                            // the block are equal, so the occupancy surcharge alone
                            // decides which lane a body takes.
                            // Walls everywhere but the ring and the two approach rows, so
                            // the two lanes round the block are the only ways past it.
                            Terrain =
                                Map.ofList (
                                    [
                                        for x in 6..18 do
                                            for y in 9..15 -> { X = x; Y = y }, Wall
                                    ]
                                    @ [
                                        for x in 10..14 do
                                            for y in 10..14 do
                                                if x = 10 || x = 14 || y = 10 || y = 14 then
                                                    { X = x; Y = y }, Plain
                                    ]
                                    @ [ for x in 6..9 -> { X = x; Y = 12 }, Plain ]
                                    @ [ for x in 15..18 -> { X = x; Y = 12 }, Plain ]
                                    @ [ { X = 5; Y = 12 }, Wall; { X = 19; Y = 12 }, Wall ]
                                )
                            Roads =
                                Set.ofList
                                    [
                                        for x in 10..14 do
                                            for y in 10..14 do
                                                if x = 10 || x = 14 || y = 10 || y = 14 then
                                                    { X = x; Y = y }
                                    ]
                            Obstacles =
                                Set.ofList
                                    [
                                        for x in 11..13 do
                                            for y in 11..13 -> { X = x; Y = y }
                                    ]
                            TargetPositions =
                                Map.ofList (
                                    [ "src-w", { X = 5; Y = 12 }; "src-e", { X = 19; Y = 12 } ]
                                    @ [
                                        for x in 11..13 do
                                            for y in 11..13 -> $"ext-{x}-{y}", { X = x; Y = y }
                                    ]
                                )
                        })
                    |> fun s ->
                        { s with
                            TargetKinds =
                                (s.TargetKinds,
                                 [
                                     for x in 11..13 do
                                         for y in 11..13 -> $"ext-{x}-{y}"
                                 ])
                                ||> List.fold (fun kinds id ->
                                    Map.add id (Structure BuiltKind.Extension) kinds)
                        }

                let assigned = [ "eb", Harvest "src-e"; "wb", Harvest "src-w" ]

                let fatigueOf variant name tick =
                    match variant with
                    | 1 -> if (name = "eb") = (tick % 2 = 1) then 4 else 0
                    | _ -> 0

                let tickOf variant tick (positions: Map<string, Pos>) =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Refillables = []
                        Creeps =
                            [
                                { worker "eb" 0 50 with
                                    Fatigue = fatigueOf variant "eb" tick
                                    Moved = variant <> 2 && tick > 0
                                }
                                { worker "wb" 0 50 with
                                    Fatigue = fatigueOf variant "wb" tick
                                    Moved = variant <> 2 && tick > 0
                                }
                            ]
                        Spatial =
                            ring
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = positions
                                })
                    }

                let start = Map.ofList [ "eb", { X = 7; Y = 12 }; "wb", { X = 17; Y = 12 } ]

                let arrived (positions: Map<string, Pos>) =
                    positions["eb"].X >= 18 && positions["wb"].X <= 6

                let rec drive variant tick positions trace =
                    if arrived positions then
                        Some tick, List.rev trace
                    elif tick > 30 then
                        None, List.rev trace
                    else
                        let moves =
                            resolveOn (tickOf variant tick positions) assigned |> moveIntents

                        let stepped =
                            (positions, moves)
                            ||> List.fold (fun acc (name, direction) ->
                                Map.add name (stepFrom acc[name] direction) acc)

                        let eb = positions["eb"]
                        let wb = positions["wb"]

                        let line =
                            sprintf
                                "t%d eb=(%d,%d) wb=(%d,%d) moves=%A"
                                tick
                                eb.X
                                eb.Y
                                wb.X
                                wb.Y
                                moves

                        drive variant (tick + 1) stepped (line :: trace)

                let outcome variant =
                    let ticks, trace = drive variant 0 start []
                    ticks, String.concat "\n" trace

                let moving, movingTrace = outcome 0

                Expect.isSome
                    moving
                    (sprintf "two bodies on the move pass each other:\n%s" movingTrace)

                let tired, tiredTrace = outcome 1
                Expect.isSome tired (sprintf "and with alternating fatigue too:\n%s" tiredTrace)

                // The pairwise control, and the loop the user filmed: read as
                // standing traffic (the pre-#225 surcharge), the two bodies
                // price each other's tile, switch lanes together every tick
                // and never pass.
                let standing, _ = outcome 2

                Expect.isNone
                    standing
                    "read as standing traffic they switch lanes together forever — the livelock"
            }
        ]
