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
                                    Lines = untroubled.Memo.Lines
                                    RoadFloods = untroubled.Memo.RoadFloods
                                    CreepFloods = untroubled.Memo.CreepFloods
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

            // Characterisation (the Brief's S0): the core's Guard whole. Its swing
            // and its row are pinned above and by `the guard row`, its stand-down
            // by `raid fold: outpost episodes`. Moved by #558: a ranger standing
            // in the room is seated beside the guard's seat, never in it.
            test
                "a level-0 core's Guard is the guard row's body on the core's ring, on the tier, for one, and a seat for a ranger in the room" {
                let colony =
                    { declaredRaid [] with
                        InvaderCores = [ coreIn "W1N2" 0 None ]
                    }
                    |> withGuards
                        [
                            guard "g-1", beside
                            creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 800),
                            { X = 21; Y = 47 }
                        ]

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony)
                    (Some(
                        safetyRank OnTheTier,
                        Capacity.fighters 2
                        |> Capacity.capping (CapScope.Row CastRow.Guard) 1
                        |> Capacity.capping (CapScope.Row CastRow.Ranger) 1
                    ))
                    "one Guard on the Safety tier, for one guard and the ranger standing in the room"

                Expect.equal
                    (Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2")
                    (Some(
                        set
                            [
                                for x in 24..26 do
                                    for y in 41..43 do
                                        if (x, y) <> (25, 42) then
                                            RoomPos.at "W1N2" { X = x; Y = y }
                            ]
                    ))
                    "its ground is the core's ring"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes it"

                let alone =
                    { declaredRaid [] with
                        InvaderCores = [ coreIn "W1N2" 0 None ]
                    }
                    |> withGuards
                        [
                            creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 800),
                            { X = 21; Y = 47 }
                        ]

                Expect.equal
                    ((decideOn alone).Assignments |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N2")))
                    "a ranger standing in the room takes it, the only body there"

                let outside =
                    { declaredRaid [] with
                        InvaderCores = [ coreIn "W1N2" 0 None ]
                    }
                    |> withBodyAtHome
                        (creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 800))
                        atSpawn

                Expect.notEqual
                    ((decideOn outside).Assignments |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N2")))
                    "and one out of the room is refused it: nothing casts or calls a ranger to a core"
            }

            // #558: a ranger holding a core's Guard strikes it from the range-3
            // ring, out of the swing of a melee body beside the core; never
            // from the guard's ring.
            test
                "a ranger holding a level-0 core's Guard is seated on its range-3 ring and shoots it from there" {
                let colony atTile =
                    { declaredRaid [] with
                        InvaderCores = [ coreIn "W1N2" 0 None ]
                    }
                    |> withGuards [ creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 800), atTile ]

                let held = Map.ofList [ "r-1", taskId (Guard "W1N2") ]
                let seated = colony { X = 21; Y = 47 }
                let atlas = Atlas.ofView seated
                let threats = threatsOfHeld seated atlas (Facts.heldTaskFacts seated held)
                let brief = Threats.briefOf threats "W1N2"

                let seat = Map.tryFind "r-1" brief.Seats.Value |> Option.defaultValue Set.empty
                Expect.isNonEmpty seat "it has a seat"

                Expect.isTrue
                    (seat
                     |> Set.forall (fun tile ->
                         range (RoomPos.pos tile) { X = 25; Y = 42 } = Engine.rangedRange))
                    "every tile of it at three from the core"

                Expect.contains
                    (decide (colony { X = 22; Y = 42 }) held Set.empty None).Intents
                    (RangedAttackStructure("r-1", "core-W1N2"))
                    "at three, it shoots the core"

                let away =
                    { declaredRaid [] with
                        InvaderCores = [ coreIn "W1N2" 0 None ]
                    }
                    |> withBodyAtHome
                        (creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 800))
                        atSpawn

                let awayBrief =
                    Threats.briefOf
                        (threatsOfHeld away (Atlas.ofView away) (Facts.heldTaskFacts away held))
                        "W1N2"

                Expect.equal
                    (Map.tryFind "r-1" awayBrief.Seats.Value)
                    (Some Set.empty)
                    "one with no walk to the ring from inside the room has no seat, never the ring beside the core"
            }

            // #558: the swap of the trespass ahead of the core must not stand a
            // guard-only colony beside a hauler it cannot catch while the core
            // stands for ever.
            test
                "where the guard alone answers trespassers it catches none of, the core is the room's and the guard breaks it" {
                let hauler =
                    { hostileIn
                          "W1N2"
                          { X = 30; Y = 44 }
                          (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Id = "a-haul"
                        Owner = "Trepidimous"
                    }

                // On its seat beside the outpost's rock at (25,40).
                let digger =
                    { hostileIn
                          "W1N2"
                          { X = 24; Y = 41 }
                          (List.replicate 5 Work @ List.replicate 3 Move) with
                        Id = "d-1"
                        Owner = "Trepidimous"
                    }

                let colony hostiles =
                    { declaredRaid hostiles with
                        InvaderCores = [ coreIn "W1N2" 0 None ]
                        Bank = bank 550 550
                    }
                    |> withGuards [ guard "g-1", beside ]

                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                let kindOf view =
                    (Threats.briefOf
                        (threatsOfHeld view (Atlas.ofView view) (Facts.heldTaskFacts view held))
                        "W1N2")
                        .Duty

                Expect.equal
                    (kindOf (colony [ hauler ]))
                    (Duty.Guard BriefKind.Core)
                    "the hauler at its pace: the core's"

                Expect.contains
                    (decide (colony [ hauler ]) held Set.empty None).Intents
                    (AttackStructure("g-1", "core-W1N2"))
                    "the guard beside it swings at the core"

                Expect.equal
                    (kindOf (colony [ hauler; digger ]))
                    (Duty.Guard BriefKind.Trespass)
                    "a digger it catches is still struck first"
            }
        ]

[<Tests>]
let neighbouringRoomTests =
    testList
        "decide across a border"
        [
            test "a neighbour's source with no crossing to it is no Task this creep can be given" {
                // The Matcher prices through the Work Area, not the Task-shaped wrapper,
                // so a guard on the wrapper alone leaves the neighbour's source priced
                // off this room's flood at cost 0, cheaper than every home rival, and
                // the creep walks inside its own room toward ground it will never stand
                // on. This view has no Borders and no Seam, so no leg crosses to it; with
                // a Seam the outpost's Harvest is assigned (OutpostDeclarationTests).
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
        ]
