/// The invader core the view carries, and decide across a border.
module Fabot.Core.Tests.Decide.OutpostBorderTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// An invader core of this level in a room, at (25,42): `raidedOutpost`'s
/// field, a tile below its Post, where `beside` is on its ring.
let private coreIn room level collapse : InvaderCoreInfo =
    {
        Id = $"core-{room}"
        RoomName = room
        Tile = { X = 25; Y = 42 }
        CollapseTick = collapse
        Level = level
    }

[<Tests>]
let invaderCoreTests =
    testList
        "the invader core the ColonyView carries"
        [
            test "a stronghold's core standing in an outpost moves nothing the colony decides" {
                // The stand-down is its answer, read a tick later off the raid log
                // (`Observe.deadlines`): a stronghold's core in the projection
                // leaves every Task, quota, cast and Verdict where it found them, reach
                // and flee included, so the comparison is over the whole decision.
                //
                // The colony under it is the posted outpost at its reserved target. A
                // quiet fixture would make the equality vacuous, so the premise is
                // asserted first.
                let colony = postedOutpostColony 19 [ "W1N2", reservedRoom true 4000 ]
                let untroubled = decideOn colony

                Expect.isNonEmpty
                    untroubled.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.isNonEmpty untroubled.Intents "and emits something for a core to disturb"

                // The whole of `Decision`, not three of its four fields: a reader folded
                // into `censusSignature` moves `Memo.Signature` alone, and the next
                // tick's `recalled` misses and refloods, which leaves Intents,
                // Assignments and Verdicts identical on this fixture.
                //
                // `Walks`, `SeamWalks` and `FarFields` are mutable `Dictionary`s the
                // Atlas fills through the tick and a Dictionary compares by reference,
                // so their references are swapped in and their contents compared beside.
                let walkRows (memo: PlanMemo) =
                    memo.Walks
                    |> Seq.map (fun entry -> entry.Key, List.ofArray entry.Value)
                    |> List.ofSeq
                    |> List.sortBy fst

                let seamRows (memo: PlanMemo) =
                    memo.SeamWalks
                    |> Seq.map (fun entry -> entry.Key, List.ofArray entry.Value)
                    |> List.ofSeq
                    |> List.sortBy fst

                let farRows (memo: PlanMemo) =
                    memo.FarFields
                    |> Seq.map (fun entry -> entry.Key, List.ofArray entry.Value)
                    |> List.ofSeq
                    |> List.sortBy fst

                let unchangedWith label cores =
                    let threatened = decideOn { colony with InvaderCores = cores }

                    Expect.equal
                        { threatened with
                            Memo =
                                { threatened.Memo with
                                    Walks = untroubled.Memo.Walks
                                    SeamWalks = untroubled.Memo.SeamWalks
                                    FarFields = untroubled.Memo.FarFields
                                    Narrowed = untroubled.Memo.Narrowed
                                }
                        }
                        untroubled
                        $"{label}: the same decision, memo and census signature and all"

                    Expect.equal
                        (walkRows threatened.Memo)
                        (walkRows untroubled.Memo)
                        $"{label}: the same spawn walks flooded under it"

                    Expect.equal
                        (seamRows threatened.Memo)
                        (seamRows untroubled.Memo)
                        $"{label}: the same Seam walks beside them"

                    Expect.equal
                        (farRows threatened.Memo)
                        (farRows untroubled.Memo)
                        $"{label}: and the same far fields"

                unchangedWith
                    "a core whose collapse timer is readable"
                    [ coreIn "W1N2" 1 (Some(colony.Time + 64000)) ]

                unchangedWith "a core carrying no deadline at all" [ coreIn "W1N2" 1 None ]

                // And one at home: the list is swept over every room the colony looks
                // into, and the reflexes that read the spawn room read hostile *creeps*.
                // A level-0 one too: home is no outpost, and no Guard is pooled for it.
                unchangedWith
                    "a core standing in the colony's own room"
                    [ coreIn "W1N1" 1 (Some colony.Time); coreIn "W1N1" 0 (Some colony.Time) ]
            }

            // #487: live t925,486, W15S27 — W15S28's outpost — stood down for
            // 47,240 ticks behind a level-0 core: 100,000 hits, no tower, no
            // rampart, nothing spawned. A guard kills it in ~134 ticks.
            test "a level-0 core in an outpost pools that room's Guard" {
                let withCore cores =
                    { declaredRaid [] with
                        InvaderCores = cores
                    }

                Expect.isSome
                    (entryFor (Guard "W1N2") (pooledOf (withCore [ coreIn "W1N2" 0 (Some 47_000) ])))
                    "the outpost holding a level-0 core is a guarded room"

                for label, cores in
                    [ "no core", []; "a stronghold's core", [ coreIn "W1N2" 1 (Some 47_000) ] ] do
                    Expect.isNone
                        (entryFor (Guard "W1N2") (pooledOf (withCore cores)))
                        $"{label}: no Guard"
            }

            test "a guard beside a level-0 core swings at it; a guard walking there does not" {
                let intentsFrom tile =
                    (decide
                        ({ declaredRaid [] with
                            InvaderCores = [ coreIn "W1N2" 0 None ]
                         }
                         |> withGuards [ guard "g-1", tile ])
                        Map.empty
                        Set.empty
                        None)

                let inSwing = intentsFrom beside
                let walking = intentsFrom { X = 28; Y = 48 }

                Expect.isOk
                    (Fabot.Core.IntentPlan.create inSwing.Intents)
                    "the guard's turn is executable"

                Expect.contains
                    inSwing.Intents
                    (AttackStructure("g-1", "core-W1N2"))
                    "on the core's ring, the guard swings at it"

                Expect.isFalse
                    (walking.Intents
                     |> List.exists (function
                         | AttackStructure _ -> true
                         | _ -> false))
                    "four tiles off, nothing"

                Expect.equal
                    (Map.tryFind "g-1" walking.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the body holds the room's Guard"

                Expect.isNonEmpty (moveIntentsFor "g-1" walking.Intents) "and walks to the core"
            }
        ]

[<Tests>]
let neighbouringRoomTests =
    testList
        "decide across a border"
        [
            test "a source in the neighbouring room is no Task this creep can be given" {
                // The Matcher prices through the Work Area, not the Task-shaped wrapper,
                // so a guard on the wrapper alone leaves the neighbour's source priced
                // off this room's flood at cost 0, cheaper than every home rival, and
                // the creep walks inside its own room toward ground it will never stand
                // on. Until #123 sums the legs over the Seam band the Task does not apply.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = TerrainGrid.ofList (corridor 10 10 17)
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = TerrainGrid.ofList (corridor 10 10 17)
                        TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 18 } ]
                    }

                let snapshot =
                    { bareRespawn with
                        Spawns = []
                        Sources = [ source "src-out" ]
                        Controller = None
                        Refillables = []
                        Creeps = [ worker "w-home" 0 50 ]
                        Spatial = home |> withNeighbour "W2N1" outpost
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn snapshot

                Expect.equal
                    (Map.tryFind "w-home" assignments)
                    None
                    "the neighbour's Harvest is inapplicable, so nothing is assigned"

                Expect.isEmpty
                    (moveIntents intents)
                    "and nobody is walked toward a border they cannot cross"
            }

            test "a grounded creep in the neighbouring room grounds nobody here" {
                // Arbitrated movement and the occupancy surcharge are single-room. The
                // Resolver pre-claims a fatigued creep's tile through a `Set<Pos>` with
                // no room dimension, so a creep on the same coordinate of another room
                // would deny a step here on evidence from fifty tiles away.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = TerrainGrid.ofList (corridor 10 10 18)
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let colony creeps layer =
                    { bareRespawn with
                        Spawns = []
                        Sources = [ source "src-home" ]
                        Controller = None
                        Refillables = []
                        Creeps = creeps
                        Spatial = home |> withNeighbour "W2N1" layer
                    }

                let assigned = Map.ofList [ "w-home", "harvest:src-home" ]

                let { Intents = alone } =
                    decideFrom assigned (colony [ worker "w-home" 0 50 ] RoomLayer.empty)

                let neighbour =
                    { RoomLayer.empty with
                        Terrain = TerrainGrid.ofList (corridor 10 10 18)
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 11 } ]
                    }

                let { Intents = crowded } =
                    decide
                        (colony
                            [ worker "w-home" 0 50; { worker "w-out" 0 50 with Fatigue = 5 } ]
                            neighbour)
                        assigned
                        Set.empty
                        None

                Expect.equal
                    (moveIntents alone)
                    [ "w-home", Bottom ]
                    "the premise: with the neighbour empty the home creep steps down its corridor"

                Expect.equal
                    (moveIntents crowded)
                    (moveIntents alone)
                    "and a creep paying off fatigue in another room changes nothing here"
            }
        ]
