/// The built kinds, the Planner, and where a placement lands.
module Fabot.Core.Tests.Decide.LayoutPlacementTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.LayoutFixtures

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
