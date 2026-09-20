/// The container Posts, their body-aware capacity, and the restock
/// dispatch judged at arrival.
module Fabot.Core.Tests.Decide.PoolPostTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.PoolFixtures

[<Tests>]
let containerPostTests =
    testList
        "container post garrison"
        [
            // The garrison rule: a full creep on a built source container
            // keeps Harvest — the engine drops the overflow into the
            // container underfoot.
            test "a full Anchor on a built source container keeps its Harvest across ticks" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the overflow falls into the container: the Post stays garrisoned"

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "kept, never released as Inapplicable"

                Expect.contains
                    intents
                    (HarvestSource("a1", "src-a"))
                    "the dig keeps firing past a full store"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no drift off the Post"
            }

            test "a full Anchor on the container matches Harvest fresh, not just from memory" {
                // Both gates — the remembered-assignment release and the
                // fresh judge — must read the same widened rule, or the
                // Anchor would be matched and released in alternate ticks.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the feeding-tier dig at cost 0 wins the fresh match too"
            }

            test "a full worker on the container releases Harvest: the garrison is body-aware" {
                // Body-blind, a light body that filled up on the Post kept
                // Harvest for life while the Anchor cast for it read
                // `none-free`.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "w1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "a light body's full store ends its dig, container or no container"
            }

            test "a full Anchor on a bare Seat digs nothing there" {
                // (9,10) is a Seat with no container: the overflow would
                // spill on the ground. The release here is the reachability
                // gate's: the room is a one-tile corridor with the source
                // walled into it, so the Post at (11,10) is on the far side
                // of the rock and no walk reaches it.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 9; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Unreachable
                    ))
                    "no container underfoot and no walk to one either"

                Expect.isEmpty
                    (digIntentsFor "a1" intents)
                    "and nothing is dug onto the ground where the overflow would spill"
            }

            test "a full creep beside the built container digs nothing: adjacency is not the tile" {
                // (12,10) touches the container but stands off it — the tile
                // a hauler drawing the container swaps the Anchor onto.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 12; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "one tile off the container is one step from it"

                Expect.isEmpty
                    (digIntentsFor "a1" intents)
                    "the widening reads the creep's own tile, never a neighbour"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Left) ]
                    "so the step it takes is back onto the container"
            }

            test "a container construction site catches no overflow: Harvest still releases" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds =
                                    haulRoom.TargetKinds
                                    |> Map.add "can-src" (Site BuiltKind.Container)
                            }
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "a pending container is not yet a container"
            }

            test "a built container off the Seats widens nothing: no dig, only the walk" {
                // The controller container's tile is no Seat of src-a: eight
                // tiles from its Post it is a body with a walk ahead of it.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 18; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "its Post is still its work, however far the walk"

                Expect.isEmpty
                    (digIntentsFor "a1" intents)
                    "only that source's own container Seat catches its overflow"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Left) ]
                    "and the walk is toward that Seat"
            }
        ]

[<Tests>]
let postCapacityTests =
    testList
        "post capacity"
        [
            // `haulRoom`'s src-a has two Seats and one Post.
            test "a source's Posts cap its heavy harvesters, however many Seats it has" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0; anchor "a2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                    }

                let remembered =
                    Map.ofList [ "a1", taskId (Harvest "src-a"); "a2", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a2",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.CapacityFull
                    ))
                    "one Post seats one garrison: the second Anchor is released, not left to crowd it"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the Post's holder keeps the dig"
            }

            test "a fresh heavy body is not matched to a source whose Post is taken" {
                // Both gates read the same cap, or the second Anchor would be
                // released and rematched in alternate ticks.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the fresh match stops at the Post count too"
            }

            test "a Post's Seat is the garrison's: the light crowd gets the Seats beyond the Posts" {
                // `haulRoom`'s src-a has two Seats, (9,10) and (11,10), and
                // the container stands on (11,10): one Post, one bare Seat.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withCreepsAt [ "w1", { X = 11; Y = 10 }; "w2", { X = 9; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a" |> List.length)
                    1
                    "two Seats less one Post admits one light body"

                // Pairwise on the Post alone: with the container gone both
                // Seats fill.
                let unposted =
                    { snapshot with
                        Spatial =
                            { snapshot.Spatial with
                                TargetKinds = snapshot.Spatial.TargetKinds |> Map.remove "can-src"
                            }
                    }

                let { Assignments = both } = decideOn unposted

                Expect.equal
                    (harvesters both "src-a")
                    [ "w1"; "w2" ]
                    "with no Post every Seat is a light body's"
            }

            test "the crowd a Post's Seat is kept from is every body but the garrison" {
                // The cap is over a group (`CapScope.Commuters`), not one
                // row: the Seats beyond the Posts are a count of tiles.
                // Counted per class, both bodies would be admitted to one.
                let snapshot =
                    { haulColony with
                        Creeps =
                            [ worker "w1" 0 50; creepWith "u1" 0 50 (bodyFor upgraderPattern 1800) ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 20; Y = 10 }
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 9; Y = 10 }; "u1", { X = 12; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "two Seats less one Post is one tile, whichever two rows want it"

                Expect.equal
                    (Map.tryFind "u1" assignments)
                    (Some(taskId (Withdraw("can-ctrl", Energy))))
                    "and the body the cap turned away drinks at the buffer it was cast for"
            }

            test "an Anchor and a light body share a source on disjoint tiles" {
                // The live case: the Anchor cast for a Post found the Seat
                // cap full of light bodies.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50; anchor "a1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withCreepsAt [ "w1", { X = 9; Y = 10 }; "a1", { X = 12; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "w1" ]
                    "the Anchor is admitted to the Post and the worker to the bare Seat"
            }

            test "a light body kept on a Post's Seat is released, not grandfathered" {
                // The capacity gate reads memory too: a remembered
                // assignment cannot outlive the rule.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withCreepsAt [ "w1", { X = 9; Y = 10 }; "w2", { X = 11; Y = 10 } ]
                    }

                let remembered =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w2",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.CapacityFull
                    ))
                    "the second light holder is released over the light cap"
            }

            test "a source with no Post caps heavy harvesters at its Seats" {
                // With nothing built a heavy body harvests from any Seat.
                // A Seat carrying a container site is a Post of its own, so
                // "unposted" is a rock with neither.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds = haulRoom.TargetKinds |> Map.remove "can-src"
                            }
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 }; "a2", { X = 9; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "a2" ]
                    "no Post derives no Post cap: both Seats are open"
            }
        ]

[<Tests>]
let restockTests =
    testList
        "restock dispatch"
        [
            test "a drained source's Harvest is applicable the tick the walk covers the wait" {
                // Four ticks of walking against four of waiting: it reaches
                // the Seat as the energy lands.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 4

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the walk covers the wait: the dry rock is worth setting out for"

                Expect.isNonEmpty
                    (moveIntentsFor "w1" intents)
                    "dispatched, not idled: the window is spent on the road"
            }

            test "one tick short of covering the wait, the creep stays where it stands" {
                // Zero slack, and the rule is self-correcting: the wait
                // shrinks by one each tick while the walk stays put, so this
                // creep departs next tick and still arrives as the energy
                // does.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 5

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn snapshot

                Expect.equal (Map.tryFind "w1" assignments) None "four ticks do not cover five"

                Expect.isEmpty
                    (moveIntentsFor "w1" intents)
                    "nothing to walk toward yet: it holds its ground for a tick"
            }

            test "paving one tile of the approach does not shorten the walk" {
                // The floor is per step: a road on (14,10) drops the
                // four-step approach from 8 cost units to 7 while the walk
                // stays four ticks. Halving the total would make it three.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 4

                let snapshot =
                    { snapshot with
                        Spatial = snapshot.Spatial |> withRoads [ { X = 14; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "four tiles are four ticks, paved or not"
            }

            test "an unreachable drained source rejects as Unreachable, not as too early" {
                // The arrival gate stands behind the reachability gate; the
                // walk and the travel cost reach the same tiles.
                let snapshot = restockAt "w1" { X = 17; Y = 10 } 60

                let snapshot =
                    { snapshot with
                        Spatial = snapshot.Spatial |> withObstacles [ { X = 16; Y = 10 } ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Unreachable
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneReachable)
                    ]
                    "the first gate it fails names the rejection, and the idle reason follows it"
            }

            test "a verbose Scoring names the wait: the drained Harvest is rejected TooEarly" {
                // The reason carries the two numbers the gate compared,
                // four ticks of walk against sixty of wait, not a bare word.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.TooEarly(4, 60)
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneInTime)
                    ]
                    "a dry source with no garrison shows up as a number of ticks, not a missing row"
            }

            test "the always-on Verdict names the wait even off the verbose list" {
                // The transition log is always on, and none-applicable there
                // would blame the body or the energy state.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneInTime) ]
                    "waiting on a restock, not rejected by its body"
            }

            test "a creep on the Seat beside a dry rock is released, walk or no walk" {
                // Standing in the Work Area there is no walk left to cover
                // the wait with.
                let snapshot = restockAt "w1" { X = 11; Y = 10 } 60
                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(0, 60))
                    ))
                    "an arrival of now covers no wait at all"

                Expect.equal (Map.tryFind "w1" assignments) None "and it is free to work elsewhere"
            }

            test "a release mid-trip carries the same two numbers the rejection does" {
                // A release on the road owes the same two numbers as a
                // rejection at the gate.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60
                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(4, 60))
                    ))
                    "why the creep is not on its way is a walk and a wait, not a bare word"
            }

            test "a Work-heavy garrison on a source container keeps Harvest through the window" {
                // The one exemption: the garrison's tile is its job whatever
                // the store or the source holds.
                let snapshot =
                    { haulColony with
                        Sources = [ drained "src-a" 60 ]
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "kept through the empty window, never released as too early"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the Post's capacity is held whether the source is drained or not"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no wobble off the Post"
            }

            test "the garrison holding residual energy keeps its Post too" {
                // With room left in the store this Anchor clears
                // applicability on free capacity alone, so only the arrival
                // gate's exemption keeps it.
                let snapshot =
                    { haulColony with
                        Sources = [ drained "src-a" 60 ]
                        Creeps = [ anchor "a1" 20 30 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "the wobble every cycle began here: it is kept, not released"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "still the Post's holder through the window"

                Expect.isEmpty (moveIntentsFor "a1" intents) "and it walks nowhere with its load"
            }

            test "the garrison digs nothing while the source is drained" {
                // The engine's ERR_NOT_ENOUGH_RESOURCES spam must stay
                // impossible: kept and silent until the energy lands.
                let snapshot =
                    { haulColony with
                        Sources = [ drained "src-a" 1 ]
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = haulRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Intents = intents } = decideFrom remembered snapshot

                Expect.isEmpty
                    (actionIntents intents)
                    "no dig Intent until the energy is there to dig"

                let restocked =
                    { snapshot with
                        Sources = [ source "src-a" ]
                    }

                let { Intents = intents } = decideFrom remembered restocked

                Expect.contains
                    (actionIntents intents)
                    (HarvestSource("a1", "src-a"))
                    "the tick the energy lands, the same garrison digs"
            }

            test "a Dual Seat Anchor gets no reprieve: it upgrades in place through the window" {
                // On a Dual Seat Upgrade is in place: the Anchor upgrades
                // through the window and rematches Harvest once spent.
                let snapshot =
                    { dualSeatColony with
                        Sources = [ drained "src-a" 60 ]
                        Creeps = [ anchor "a1" 50 10 ]
                        Spatial = dualSeatRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(0, 60))
                    ))
                    "no container underfoot, so no garrison exemption"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the Dual Seat's other half is work it can do standing still"

                Expect.isEmpty
                    (moveIntentsFor "a1" intents)
                    "it upgrades in place, it does not walk"
            }

            test "eight road tiles are eight ticks of waiting, not four" {
                // The lane is paved, so an empty worker unit pays one cost
                // unit a step: eight steps price at 8, which halved read as
                // a four-tick arrival. The walk floors each tile at a tick.
                let pavedAt pos ticks =
                    let snapshot = restockAt "w1" pos ticks

                    { snapshot with
                        Spatial =
                            snapshot.Spatial |> withRoads [ for x in 11..21 -> { X = x; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn (pavedAt { X = 19; Y = 10 } 8)

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the walk equals the wait: it leaves now and arrives as the energy does"

                let { Assignments = assignments } = decideOn (pavedAt { X = 19; Y = 10 } 9)

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "one tick short, and eight ticks of road do not cover nine of waiting"
            }

            test "a bystander in the lane does not change the dispatch" {
                // The lane is one tile wide, so a bystander cannot be walked
                // around: the occupancy surcharge once added 10 cost units,
                // five ticks of phantom arrival. The walk is blind to
                // today's traffic.
                let decides crowded ticks =
                    let snapshot = restockAt "w1" { X = 15; Y = 10 } ticks

                    let snapshot =
                        if crowded then
                            { snapshot with
                                Creeps =
                                    snapshot.Creeps
                                    @ [ creepWith "b1" 0 100 [ Carry; Carry; Move ] ]
                                Spatial =
                                    snapshot.Spatial
                                    |> withHome (fun layer ->
                                        { layer with
                                            CreepPositions =
                                                layer.CreepPositions
                                                |> Map.add "b1" { X = 14; Y = 10 }
                                        })
                            }
                        else
                            snapshot

                    let { Assignments = assignments } = decideOn snapshot
                    Map.tryFind "w1" assignments

                for ticks in [ 3; 4; 5; 6; 9; 12 ] do
                    Expect.equal
                        (decides true ticks)
                        (decides false ticks)
                        $"a bystander cannot decide a %d{ticks}-tick wait either way"

                Expect.equal
                    (decides true 4)
                    (Some(taskId (Harvest "src-a")))
                    "four ticks of walking still cover four of waiting, crowd or no crowd"

                Expect.equal
                    (decides true 9)
                    None
                    "and the crowd no longer buys the phantom five that dispatched it"
            }
        ]
