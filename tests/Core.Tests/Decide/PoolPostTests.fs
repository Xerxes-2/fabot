/// The container Posts, their body-aware capacity (ADR 0024), and the
/// restock dispatch judged at arrival (ADR 0025).
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
            // The garrison rule (#47, ADR 0012): a full creep standing on a
            // built source container keeps Harvest — the engine drops the
            // overflow into the container underfoot, so the creep
            // effectively has capacity. Everywhere else the ordinary
            // full-store rule stands.
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
                // The squat of #67 (ADR 0024): body-blind, this widening let
                // a light body that filled up on the Post keep Harvest for
                // the rest of its life — never Inapplicable, so anti-thrash
                // never let the tile go — while the Anchor cast for that
                // Post read `none-free`. Only a garrisoning body's overflow
                // keeps the dig past a full store.
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
                // (9,10) is a Seat of src-a with no container: harvesting
                // past a full store there spills onto the ground, so the
                // overflow reprieve does not reach it and the Emitter
                // issues no dig — which is ADR 0024's claim, and it is
                // untouched by ADR 0048's walk home.
                //
                // The release here is the reachability gate's and not the
                // store's: this room is a one-tile corridor with the
                // source walled into it, so the Post at (11,10) is on the
                // far side of the rock and no walk reaches it. The
                // neighbouring cases below stand on tiles that can walk.
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
                // (12,10) touches the container at (11,10) but stands off
                // it: adjacency catches nothing — only the tile itself.
                // This is the tile a hauler drawing the container swaps
                // the Anchor onto (#193), so it is the case ADR 0048's
                // walk home is written for: the reprieve is still the
                // container's, and the one step back onto it is the Task's.
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
                // The controller container's tile is no Seat of src-a — a
                // full creep standing on it is nowhere the overflow rule
                // helps, however built the container underfoot. Eight
                // tiles from its Post it is simply a body with a walk
                // ahead of it (ADR 0048).
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
            // The over-admission half of #67 (ADR 0024): a Work-heavy body's
            // Harvest Work Area is that source's Posts (ADR 0020), so the
            // Seat count admits garrisons to standing room that does not
            // exist. `haulRoom`'s src-a has two Seats and one Post.
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
                // ADR 0051 (#212). `haulRoom`'s src-a has two Seats, (9,10)
                // and (11,10), and the container stands on (11,10): one
                // Post, one bare Seat. Two light bodies want it; one is
                // admitted, to the bare Seat, and the second reads
                // none-free — where before both were admitted and an Anchor
                // arriving after them found no standing room at all.
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

                // Pairwise on the Post alone: the same two bodies with the
                // container gone from the census fill both Seats, which is
                // ADR 0045's bare-Seat bootstrap unchanged.
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
                // ADR 0051's cap is over a **group** and not over one row
                // (ADR 0052 decision 6, `CapScope.Commuters`): the Seats
                // beyond the Posts are a count of tiles, and any body but a
                // garrison may stand on one. The two non-garrison classes
                // the colony casts are the generalist and the [[standing
                // body]], so they are what this reads — one bare Seat, one
                // of the two on it, and the other takes the buffer it can
                // reach instead. Counted per class rather than over the
                // group, both would be admitted to the one tile.
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
                    (Some(taskId (Withdraw "can-ctrl")))
                    "and the body the cap turned away drinks at the buffer it was cast for"
            }

            test "an Anchor and a light body share a source on disjoint tiles" {
                // The live case (#212): the Anchor cast for a Post found the
                // Seat cap full of light bodies. Now the light body's Work
                // Area is the bare Seat alone and its cap the Seats beyond
                // the Posts, so the Post is the garrison's by geometry and
                // by cap together — both harvest, each on its own tile.
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
                // ADR 0024's squatter sat on a Post since t69135 because a
                // remembered assignment outlived the rule; the capacity gate
                // reads memory too, so two light bodies remembered on a
                // one-bare-Seat source lose one of them.
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
                // The pre-container fallback (ADR 0020): with nothing built,
                // a heavy body harvests from any Seat, so a Post cap of zero
                // would strand the colony instead of ordering it.
                //
                // The source container leaves the census outright since
                // #205. This fixture used to demote it to a construction
                // site to reach "no Post", and a Seat carrying a container
                // site is now a Post of its own — the garrison that raises
                // it — so nothing pending on a Seat says "unposted" any
                // more. What does is a rock with neither.
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
                // ADR 0025: the Task is judged at arrival, not at this tick.
                // Four ticks of walking against four ticks of waiting — the
                // creep leaves now and reaches the Seat as the energy lands.
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
                // The floor is per step, not on the total (ADR 0029): a road
                // on (14,10) drops the four-step approach from 8 cost units
                // to 7 — travel cost still ranks a paved route ahead — while
                // the walk stays four ticks, because four tiles are four
                // tiles however they are surfaced. Halving the total would
                // have made it three and sat the creep out of a tick it
                // could have spent walking.
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
                // The arrival gate stands behind the reachability gate:
                // geometry the creep cannot cross is reported as such,
                // whatever the source holds. The walk and the travel cost
                // reach the same tiles, so neither gate can shadow the
                // other's answer (ADR 0029).
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
                // The body and the energy state fit and the Seat is reachable
                // — only the arrival doesn't (ADR 0025), so the row carries
                // its own reason rather than lying as Inapplicable, and the
                // always-on Verdict beside it says the same. The reason is
                // not a bare word (#88): it carries the two numbers the gate
                // compared, four ticks of walk against sixty of wait, so the
                // operator reads the answer off the row instead of halving a
                // cost that no longer means ticks.
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
                // ADR 0025, CONTEXT's Verdict entry: the transition log is
                // always on, and none-applicable there would claim the body
                // or the energy state was the problem. Neither is: the creep
                // is simply too far from a source that is not ready yet.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneInTime) ]
                    "waiting on a restock, not rejected by its body"
            }

            test "a creep on the Seat beside a dry rock is released, walk or no walk" {
                // Issue #48's rule under ADR 0025's gate, on real geometry:
                // standing in the Work Area there is no walk left to cover
                // the wait with, so anti-thrash does not pin the creep to a
                // source that will not feed it for another sixty ticks.
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
                // #88: a creep released on the road owes the same
                // explanation as one rejected at the gate, so both reasons
                // carry the pair the gate compared. Four tiles out with
                // sixty ticks to go, the release says four and sixty —
                // distinct numbers, neither of them the other, and neither
                // recoverable from a scored row that is not written for a
                // rejected candidate at all.
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
                // The one exemption, on ADR 0024's condition and no other:
                // that tile is the garrison's job whatever the store or the
                // source holds, so the container-Post wobble is gone.
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
                // ADR 0025's motivating symptom, with room left in the store:
                // this Anchor clears the applicability gate on free capacity
                // alone, so only the arrival gate's exemption can keep it —
                // the reprieve is pinned here without ADR 0012's overflow
                // widening standing in for it.
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
                // The Emitter gate (ADR 0025): the occupancy surcharge can
                // land a creep a tick or two early, and the engine's
                // ERR_NOT_ENOUGH_RESOURCES spam must stay impossible. The
                // garrison stays kept and silent, and digs the tick the
                // energy lands.
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
                // The exemption is ADR 0024's condition and no other. On a
                // Dual Seat Upgrade is in place, so the Anchor keeps
                // upgrading as ADR 0013 described and rematches Harvest once
                // its Carry is spent.
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
                // #79's report, at the gate that made it visible. The lane
                // is paved, so an empty worker unit pays one cost unit a
                // step: eight steps price at 8, which halved read as a
                // four-tick arrival and sent this creep out to cover a wait
                // it could not reach in time. The walk floors each tile at
                // a whole tick — eight tiles, eight ticks — so the gate
                // now covers an eight-tick wait and no more.
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
                // #78 inverted. The lane is one tile wide, so a creep
                // standing in it has nowhere to be walked around: the
                // occupancy surcharge added 10 cost units to this walk,
                // five ticks of phantom arrival, and the gate dispatched a
                // creep on a crowd that had moved on by the next tick —
                // then released it TooEarly. The walk is blind to today's
                // traffic, so the same ColonyView decides the same way with
                // the bystander and without it, at every wait either side
                // of the boundary.
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
