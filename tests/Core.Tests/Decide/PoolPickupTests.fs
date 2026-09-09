/// The pickup reflex and the logistics layer behind it (ADR 0012).
module Fabot.Core.Tests.Decide.PoolPickupTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.PoolFixtures

[<Tests>]
let pickupReflexTests =
    testList
        "pickup reflex"
        [
            test "an adjacent creep with free capacity picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decideOn snapshot
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "in reach and hungry: pick up"
            }

            test "a creep standing on the pile picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 10 } ]
                let { Intents = intents } = decideOn snapshot
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "range 0 is within reach"
            }

            test "a full creep leaves the pile alone" {
                let snapshot = pileColony [ worker "w1" 50 0 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (pickups intents) "no free capacity, nothing to gain"
            }

            test "a pile out of reach draws nobody — the reflex never moves a creep" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 13 } ]
                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (pickups intents) "range 3: recapture only what is in reach"
            }

            test "every adjacent creep picks — the engine settles duplicates" {
                let snapshot =
                    pileColony
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 11 }; "w2", { X = 9; Y = 10 } ]

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "w1", "pile-1"; "w2", "pile-1" ]
                    "zero coordination: both reach, both ask"
            }

            test "the pickup rides beside the task's own action" {
                // The creep sits on a Seat of src-a with the pile also in
                // reach: pickup conflicts with no other action, so both
                // Intents are emitted for the same tick.
                let snapshot =
                    { pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ] with
                        Sources = [ source "src-a" ]
                    }

                let withSource =
                    { snapshot with
                        Spatial =
                            { snapshot.Spatial with
                                TargetKinds = Map.add "src-a" Source snapshot.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain = Map.add { X = 11; Y = 11 } Wall layer.Terrain
                                    TargetPositions =
                                        Map.add "src-a" { X = 11; Y = 11 } layer.TargetPositions
                                })
                    }

                let { Intents = intents } = decideOn withSource

                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the reflex fires"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-a"))
                    "the assigned task's action still goes out"
            }

            test "several reachable piles produce one pickup per creep" {
                let colony = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]

                let colony =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.add "pile-2" Dropped colony.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "pile-2" { X = 11; Y = 11 } layer.TargetPositions
                                })
                    }

                let decision = decideOn colony

                Expect.equal
                    (pickups decision.Intents)
                    [ "w1", "pile-2" ]
                    "the reflex keeps the former last-write target without issuing two calls"

                Expect.isOk
                    (Fabot.Core.IntentPlan.create decision.Intents)
                    "the entire turn is compatible"

                let tasked =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.ofList [ "pile-1", 150 ]
                            }
                    }

                let decision = decideOn tasked

                Expect.equal
                    (Map.tryFind "w1" decision.Assignments)
                    (Some(taskId (Pickup "pile-1")))
                    "the stocked first pile is the assigned task"

                Expect.equal
                    (pickups decision.Intents)
                    [ "w1", "pile-1" ]
                    "a task's target owns the channel even when the reflex would choose another pile"

                Expect.isOk
                    (Fabot.Core.IntentPlan.create decision.Intents)
                    "task and reflex compose without overwrites"
            }

            test "a pile keeps no construction site off its tile" {
                // Layout determinism (ADR 0011): a transient pile must not
                // perturb the ordering, so placement with and without the
                // pile is identical.
                let bare = atLevel 2 (openRoom 3)

                let strewn =
                    atLevel 2 (openRoom 3 |> withTargets [ "pile-1", { X = 24; Y = 24 }, Dropped ])

                let placedWith = decideOn strewn
                let placedWithout = decideOn bare

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see piles"
            }

            test "an outpost creep picks up the pile at its own feet" {
                // The live gap (#166): an outpost's Anchor stands on its
                // container, overflows onto the tile it stands on, and the
                // pile is at range 0 for the hauler that comes for the
                // container — 3,000 energy decaying on the ground at
                // t140,810 because both sides of the pairing answered home.
                // The home pile shares the coordinate and stays untouched.
                let snapshot =
                    pileColony [ worker "w-out" 0 50 ] []
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "w-out", { X = 10; Y = 10 } ]

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (pickups intents)
                    [ "w-out", "pile-out" ]
                    "its own room's pile, and only that one"
            }

            test "a pile at home draws no creep standing in the outpost" {
                // The pairing never crosses a border (ADR 0041): the pile
                // and the creep are bare `Pos`es on one coordinate of two
                // rooms, which is range 0 to `range` and out of the world
                // to the engine.
                let snapshot =
                    pileColony [ worker "w-out" 0 50 ] []
                    |> withPileRoom "W1N2" [] [ "w-out", { X = 10; Y = 10 } ]

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (pickups intents) "same coordinate, different room, no reach"
            }

            test "two rooms each pair their own pile with their own creep" {
                let snapshot =
                    pileColony
                        [ worker "w1" 0 50; worker "w-out" 0 50 ]
                        [ "w1", { X = 10; Y = 11 } ]
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "w-out", { X = 9; Y = 10 } ]

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "w-out", "pile-out"; "w1", "pile-1" ]
                    "one Intent a room, each creep on the pile of the room it stands in"
            }

            test "a creep the projection places nowhere reaches no pile" {
                // ADR 0004's absence, unchanged by the pairing going per
                // room: a creep in the fleet and in no layer is in no
                // group, so it is measured against nothing rather than
                // against every room's piles at once.
                let snapshot =
                    { pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ] with
                        Creeps = [ worker "w1" 0 50; worker "ghost" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the unplaced creep picks nothing"
            }
        ]

[<Tests>]
let logisticsTests =
    testList
        "logistics"
        [
            test "a stocked container yields a Withdraw Task; an empty one yields none" {
                Expect.equal
                    (withdrawTasks (planTasks haulColony noThreats))
                    [ "can-ctrl" ]
                    "the stocked buffer enters the pool; the empty source container does not"
            }

            test
                "the controller container with room is a Refill target; source containers never are" {
                let snapshot =
                    { haulColony with
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                    }

                let tasks = planTasks snapshot noThreats

                Expect.equal
                    (refillTasks tasks)
                    [ "can-ctrl" ]
                    "only the buffer is a Refill target, however stocked the source container"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "can-src" ]
                    "both stocked containers stay Withdraw Tasks"
            }

            test "a full controller container is no Refill target, but stays a Withdraw" {
                let snapshot =
                    { haulColony with
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-ctrl", 2000 ]
                            }
                    }

                let tasks = planTasks snapshot noThreats
                Expect.isEmpty (refillTasks tasks) "no room left to refill"
                Expect.equal (withdrawTasks tasks) [ "can-ctrl" ] "still stocked to draw from"
            }

            test "an empty creep between source and stocked container is matched by travel cost" {
                // At (15,10) the buffer's Work Area is two steps away, the
                // nearest Seat four: collect beats dig. At (12,10) the Seat
                // is one step away: dig beats collect. Same rule both ways.
                //
                // The source is read unposted here (ADR 0051): with the
                // container standing, the Seat a light body may dig from is
                // the one across the wall at (9,10) and the near one at
                // (11,10) is the garrison's, so the dig would lose on
                // reachability and not on price, which is not what this
                // test is about.
                let colonyAt pos =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds = haulRoom.TargetKinds |> Map.remove "can-src"
                            }
                            |> withCreepsAt [ "w1", pos ]
                    }

                let near = decideOn (colonyAt { X = 15; Y = 10 })

                Expect.equal
                    (Map.tryFind "w1" near.Assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the cheaper-to-reach buffer wins the feeding-tier tie"

                Expect.contains
                    near.Verdicts
                    (Verdict.Matched("w1", taskId (Withdraw "can-ctrl"), MatchFactor.TravelCost))
                    "the match speaks its Verdict: travel cost decided"

                let far = decideOn (colonyAt { X = 12; Y = 10 })

                Expect.equal
                    (Map.tryFind "w1" far.Assignments)
                    (Some(taskId (Harvest "src-a")))
                    "nearer the source, digging wins the same tie"
            }

            test "a heavy-Work body never collects: the far Post's Harvest beats the near buffer" {
                // Same geometry where the worker above picks Withdraw — at
                // (15,10) the buffer is two steps, the nearest Seat four.
                // Work > Move makes Withdraw inapplicable (ADR 0016), so
                // the anchor's only feeding-tier candidate is Harvest and
                // the unmanned Post wins regardless of distance.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 15; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the stocked buffer never outbids the Post for a Work-heavy body"
            }

            test "a kept Withdraw on a heavy-Work body releases as inapplicable and digs" {
                // Deployment heals the live colony without a death: the
                // remembered orbit breaks the first tick the gate lands.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 15; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Withdraw "can-ctrl"),
                        ReleaseReason.Inapplicable
                    ))
                    "the gate releases the remembered collection"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the rematch walks the anchor home"
            }

            test "alternation is emergent: a filled-up creep's Withdraw releases and rematches" {
                // The creep filled up inside the buffer's Work Area — which
                // is also the controller's. Withdraw loses applicability;
                // the rematch sinks the load into Upgrade, never back into
                // the container it just drew from.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "w1", { X = 17; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "w1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Withdraw "can-ctrl"),
                        ReleaseReason.Inapplicable
                    ))
                    "the full store releases Withdraw"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the rematch flips to Upgrade, like the Anchor's harvest↔upgrade"
            }

            test "the alternation's other half: an emptied creep's Upgrade releases into Withdraw" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = haulRoom |> withCreepsAt [ "w1", { X = 17; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "w1", taskId (Upgrade "ctrl-1") ]
                let { Assignments = assignments } = decideFrom remembered snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the empty store tops up from the buffer one tile away"
            }

            test "spawn-feeding Refill still outranks the buffer Refill" {
                // The spawn stands mid-corridor, two steps from the loaded
                // creep; the buffer's Work Area costs nothing at all. Rank
                // dominates: reproduction is fed before the buffer.
                let snapshot =
                    { haulColony with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles =
                                        Set.ofList [ { X = 14; Y = 10 }; { X = 20; Y = 10 } ]
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                            |> withTargets
                                [ "spawn-1", { X = 14; Y = 10 }, Structure BuiltKind.Spawn ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the buffer never outbids feeding the spawn"
            }

            test "a loaded Carry-only body is the buffer's Refill worker" {
                // The buffer's tier sits below every surplus Task, so
                // Work-bodied creeps pass it by — but a full hauler-shaped
                // body has no surplus work of its own, and the outflow
                // lands on it.
                let snapshot =
                    { haulColony with
                        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
                        Spatial = haulRoom |> withCreepsAt [ "h1", { X = 15; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the buffer Refill is live work for a body that can do nothing better"
            }

            test "a seated Withdraw emits the engine withdraw call and speaks 📥" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = haulRoom |> withCreepsAt [ "w1", { X = 17; Y = 10 } ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("w1", "can-ctrl"))
                    "in range at tick start: the Executor-bound Intent fires"

                Expect.contains intents (SayCreep("w1", "📥")) "the Task's own chat bubble"
            }
        ]
