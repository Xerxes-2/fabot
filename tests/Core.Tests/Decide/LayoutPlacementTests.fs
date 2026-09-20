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
                        "extractor"
                        "terminal"
                    ]
                    "each BuiltKind maps to its Screeps string"
            }

            test "Refill keeps the spawn, the extensions and the towers fed" {
                // The controller container and the Storage are Refill targets
                // too, but pooled off the projection's stores — they never
                // enter the Refillables list.
                Expect.equal
                    (allBuiltKinds |> List.filter isRefillable)
                    [ BuiltKind.Spawn; BuiltKind.Extension; BuiltKind.Tower ]
                    "the energy-hungry kinds alone are Refillables"

                Expect.isFalse
                    (isRefillable BuiltKind.Other)
                    "an unmodelled kind is no Refillable: the projection reads no free capacity off it"
            }

            test "each kind is whole at its own line: half of max, a floor, or full" {
                // The whole line per kind, which is also the list of kinds
                // whose hits the projection carries at all. The numbers
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
                        // Neither decays, and neither's hits reach the
                        // projection.
                        BuiltKind.Extractor, None
                        BuiltKind.Terminal, None
                    ]
                    "one line per kind, and none for the kinds Repair never touches"

                // The Keep is the list the rampart covering and safe mode
                // hang off, so it must be exactly the kinds repaired to full:
                // a Keep kind repaired to half would leave the safe-mode
                // trigger armed for every hostile that wandered through.
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
                    [ BuiltKind.Container; BuiltKind.Storage; BuiltKind.Terminal ]
                    "the containers, the Storage and — since ore ships (#349) — the terminal put a store in the projection"

                // `allBuiltKinds` leaves Other out, so no filter above can
                // say anything about it — and Other is the arm with the worst
                // reach: the projection reads hits and a store off every kind
                // these admit, and an unmodelled structure carries neither.
                Expect.isNone
                    (wholeLine BuiltKind.Other)
                    "an unmodelled kind has no whole line and never enters the Repair pool"

                Expect.isFalse (isKeep BuiltKind.Other) "an unmodelled kind is no Keep structure"

                // The Raid log charges damage on the Keep and its cover and
                // on nothing else: a chewed road is ordinary decay.
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

            test
                "a creep stands on a road, a container, a rampart or an extractor, and on nothing else" {
                // Screeps OBSTACLE_OBJECT_TYPES, as the projection reads it:
                // every kind that is not walkable blocks its tile.
                Expect.equal
                    (allBuiltKinds |> List.filter isWalkable)
                    [
                        BuiltKind.Road
                        BuiltKind.Container
                        BuiltKind.Rampart
                        // The extractor is not one of OBSTACLE_OBJECT_TYPES,
                        // so a body walks over it as it walks over a road.
                        BuiltKind.Extractor
                    ]
                    "the four kinds a creep may share a tile with"

                Expect.isFalse
                    (isWalkable BuiltKind.Other)
                    "a kind the decision layer does not model blocks its tile: Other never walks"
            }

            test "a placement Intent's kind widens to the built kind of the same name" {
                // The one crossing between the two vocabularies: a transposed
                // case would place one kind and describe another with nothing
                // in either layer to catch it.
                Expect.equal
                    ([
                        Extension
                        Tower
                        Road
                        Container
                        Storage
                        Rampart
                        StructureKind.Extractor
                     ]
                     |> List.map builtKindOfPlaceable)
                    [
                        BuiltKind.Extension
                        BuiltKind.Tower
                        BuiltKind.Road
                        BuiltKind.Container
                        BuiltKind.Storage
                        BuiltKind.Rampart
                        BuiltKind.Extractor
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
                let tasks = planTasksOn bareRespawn noThreats

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
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a"; drained "src-b" 120 ]
                    }

                let harvests =
                    planTasksOn snapshot noThreats
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
                    planTasksOn bareRespawn noThreats
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.equal upgrades [ "ctrl-1" ] "the controller gets exactly one Upgrade task"
            }

            test "no Upgrade task without a controller" {
                let tasks = planTasksOn { bareRespawn with Controller = None } noThreats

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
                        ConstructionSites =
                            [
                                { Id = "site-1"; Left = siteOwes }
                                { Id = "site-2"; Left = siteOwes }
                            ]
                    }

                let builds =
                    planTasksOn snapshot noThreats
                    |> List.choose (function
                        | Build siteId -> Some siteId
                        | _ -> None)

                Expect.equal builds [ "site-1"; "site-2" ] "one Build task per construction site"
            }

            test "the spawn and its extensions are one Refill, under the spawn's id" {
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
                    planTasksOn snapshot noThreats
                    |> List.choose (function
                        | Refill(structureId, _) -> Some structureId
                        | _ -> None)

                Expect.equal refills [ "spawn-1" ] "the cluster is pooled once, under its spawn"
            }

            test "a full cluster is pooled at all only while some member has room" {
                // `task-gone` fires when the ring is full, not when the
                // extension a body happened to be aimed at is.
                let cluster free =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" (fst free) BuiltKind.Spawn
                                refillable "ext-1" (snd free) BuiltKind.Extension
                            ]
                    }

                let refills snapshot =
                    planTasksOn snapshot noThreats
                    |> List.choose (function
                        | Refill(structureId, _) -> Some structureId
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
                // The `None` arm of `RefillCluster.ofRefillables`: a room
                // whose spawn has been destroyed has no cluster.
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
                    planTasksOn snapshot noThreats
                    |> List.choose (function
                        | Refill(structureId, _) -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "ext-1"; "ext-3" ]
                    "only the extensions with free capacity need a Refill"
            }

            test "a tower missing energy gets a Refill task; a full tower gets none" {
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "tower-2" 0 BuiltKind.Tower
                            ]
                    }

                let refills =
                    planTasksOn snapshot noThreats
                    |> List.choose (function
                        | Refill(structureId, _) -> Some structureId
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
                let { Intents = intents } = decideOn (atLevel 2 (openRoom 3))

                // (24,24) is the Storage's pick and (24,26) the one tower's
                // — a pick, not a site: RCL2 allows no tower, but its pick
                // still comes first in the one ordering — so the extensions
                // start two tiles in. A golden value of the horizon, not an
                // assertion about the ordering.
                Expect.equal
                    (sitesOfKind Extension intents)
                    [
                        { X = 26; Y = 24 }
                        { X = 26; Y = 26 }
                        { X = 23; Y = 23 }
                        { X = 23; Y = 25 }
                        { X = 23; Y = 27 }
                    ]
                    "the last two diagonal neighbours, then rank-2 checkerboard tiles"

                for (room, _, kind) in placementIntents intents do
                    Expect.equal room "W1N1" "sites go in the spawn's room"

                    Expect.isTrue
                        (kind = Extension || kind = Rampart)
                        "the extensions the level unlocks, and the spawn's own rampart"
            }

            test "RCL5 on open terrain plans the whole level: 30 extensions, two towers" {
                // A ring wider than the fixtures beside it because thirty
                // extensions, two towers, the Storage and the footings want
                // more same-colour tiles than `openRoom 3` has.
                let { Intents = intents } = decideOn (atLevel 5 (openRoom 5))

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
                // A horizon left behind the room computes a gap of zero here
                // and asks for none of the ten RCL6 adds (#341).
                let { Intents = intents } = decideOn (atLevel 6 (openRoom 6))

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    40
                    "RCL6's whole extension allowance, the ten that level adds included"

                // The horizon is 7 and draws three tower tiles; the placement
                // filter is the room's own level, so the third is drawn and
                // not placed.
                Expect.hasLength
                    (sitesOfKind Tower intents)
                    2
                    "RCL6 places two towers, though its horizon draws a tile for a third"

                // And the level below places what its own level unlocks, not
                // what the horizon drew.
                let { Intents = below } = decideOn (atLevel 5 (openRoom 6))

                Expect.hasLength (sitesOfKind Extension below) 30 "RCL5 places its own thirty"
            }

            // The plan a room *built out* under one level's horizon only ever
            // grows when the level moves. Built out is the load-bearing word:
            // two bare rooms at neighbouring levels do not nest, since the
            // extra tower pick shifts the extension list by one. Both
            // clustered kinds at every rung, because the tower's allowance
            // steps on rungs the extensions' does not and jumps 3→6 at RCL8;
            // a ladder that read extensions alone would let that rung swallow
            // three towers silently.
            for level, adds, towers in [ 2, 5, 1; 3, 10, 0; 4, 10, 1; 5, 10, 0; 6, 10, 1; 7, 10, 3 ] do
                test $"a room built out at RCL{level} asks for exactly what RCL{level + 1} adds" {
                    let room = openRoom 8
                    let { Intents = before } = decideOn (atLevel level room)
                    let standing = sitesOfKind Extension before @ sitesOfKind Tower before

                    let built =
                        room
                        |> withTargets (
                            (sitesOfKind Extension before
                             |> List.mapi (fun i tile ->
                                 $"ext-{i}", tile, Structure BuiltKind.Extension))
                            @ (sitesOfKind Tower before
                               |> List.mapi (fun i tile ->
                                   $"tower-{i}", tile, Structure BuiltKind.Tower))
                        )
                        |> withHome (fun layer ->
                            { layer with
                                Obstacles = Set.union layer.Obstacles (Set.ofList standing)
                            })

                    let { Intents = after } = decideOn (atLevel (level + 1) built)

                    Expect.hasLength
                        (sitesOfKind Extension after)
                        adds
                        $"RCL{level + 1} adds {adds} extensions and the room asks for all of them"

                    Expect.hasLength
                        (sitesOfKind Tower after)
                        towers
                        $"and the {towers} tower(s) RCL{level + 1} adds, on the same ladder"

                    // A rampart is the one kind that may share a standing
                    // structure's tile; every other kind sharing one would be
                    // the plan eating the colony's own buildings.
                    Expect.isEmpty
                        (placementIntents after
                         |> List.filter (fun (_, tile, kind) ->
                             kind <> Rampart && List.contains tile standing))
                        "and no tile a structure already stands on is planned for anything else"
                }

            test "below RCL2 no placement Intents are emitted" {
                let { Intents = intents } = decideOn (atLevel 1 (openRoom 3))

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
                            Terrain = TerrainGrid.add { X = 24; Y = 24 } Wall layer.Terrain
                        })

                let { Intents = intents } = decideOn (atLevel 2 holed)

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

                let { Intents = intents } = decideOn (atLevel 2 blocked)

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

                let { Intents = intents } = decideOn (atLevel 2 room)

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

                let { Intents = intents } = decideOn (atLevel 2 room)
                Expect.isEmpty (sitesOfKind Extension intents) "allowance already used up"
            }

            test "the controller's tile is never chosen" {
                // The controller stands on a free same-colour tile the old
                // Placement projection would have offered to a site.
                let room = openRoom 3 |> withTargets [ "ctrl-1", { X = 24; Y = 24 }, Controller ]

                let { Intents = intents } = decideOn (atLevel 2 room)

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a target's tile is never chosen"

                // The controller's Upgrade Work Area is working ground, so the
                // room offers seven tiles; one tower's pick sits ahead of the
                // extensions and five are left.
                Expect.hasLength
                    (sitesOfKind Extension intents)
                    5
                    "the cap is reached: one tower a level away is all this room reserves for"
            }

            test "no placement Intents without a projected room" {
                let snapshot =
                    { bareRespawn with
                        Controller = Some(controllerAt 2)
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (placementIntents intents) "nothing to plan around"
            }
        ]
