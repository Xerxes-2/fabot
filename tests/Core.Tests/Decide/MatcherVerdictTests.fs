/// The Verdicts a match, a release and a rejection are returned under
/// (ADR 0009), and the verbose list an operator reads (ADR 0018).
module Fabot.Core.Tests.Decide.MatcherVerdictTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

[<Tests>]
let resolverVerdictTests =
    testList
        "resolver verdicts"
        [
            test "a grounded creep gets a grounded Verdict; the creep behind it yields to it" {
                // The one-lane corridor with a fatigued seated harvester: har
                // sits arbitration out with its tile blocked, and bob — whose
                // only path runs through that tile — stands down for the tick.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "bob", Build "site-1" ])
                    [ Verdict.Grounded "har"; Verdict.Yielded("bob", "har") ]
                    "har is grounded; bob's blocked step names the tired creep holding the tile"
            }

            test "a lone fatigued traveller is grounded, nothing more" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    [ Verdict.Grounded "w1" ]
                    "grounding is the whole story: no move was asked, none was denied"
            }

            test "a displaced squatter's Verdict names its displacer" {
                // The squatting regression's geometry: the upgrader on the
                // sole Seat is displaced by the inbound harvester.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 14 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "har", { X = 10; Y = 12 }
                                                "upg", { X = 10; Y = 11 }
                                            ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("upg", "har") ]
                    "the displaced upgrader yields to the harvester; the harvester says nothing"
            }

            test "losing a contested tile to a higher rank is a yield naming the winner" {
                // The contested-gap geometry: Harvest outranks Upgrade, so
                // the upgrader waits in place while the harvester takes the
                // gap it also wanted.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 8 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("u", "h") ]
                    "the outranked upgrader's wait is attributed to the harvester"
            }

            test "the reroute Verdict is manufactured only for a creep on the verbose list" {
                // The two-lane corridor: the builder's straight path runs
                // through the seated harvester's tile, and the surcharge
                // sends it into the parallel lane instead. Nobody yields —
                // the detour is a pricing event, not an arbitration one.
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveVerdictsOn snapshot assigned)
                    "a quiet colony pays for no second flood, so it records no reroute"

                Expect.isEmpty
                    (resolveVerdictsVerboseOn snapshot assigned [ "har" ])
                    "the list is read per creep: the detourer is not the one being watched"

                Expect.equal
                    (resolveVerdictsVerboseOn snapshot assigned [ "bob" ])
                    [ Verdict.Rerouted "bob" ]
                    "the lane sidestep is attributed to traffic; the seated harvester says nothing"
            }

            test "a creep simply stepping toward its Work Area produces no movement noise" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.isEmpty
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    "conclusion level means events, not every step"
            }

            test "a clean head-on swap is silent: both creeps settle where they asked" {
                Expect.isEmpty
                    (resolveVerdictsOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ])
                    "each traveller got exactly its preferred tile; nothing became of either move"
            }

            test "movement Verdicts ride behind the Matcher's in decide's output" {
                // A fatigued lone traveller at the decide seam: the Matcher
                // speaks first (the fresh match), the Resolver after (the
                // grounding) — one additive list, interleaved downstream.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Grounded "w1"
                    ]
                    "matcher verdicts first, then the Resolver's, in one list"
            }
        ]

[<Tests>]
let sayTests =
    testList
        "chat bubbles"
        [
            test "an assigned harvester says the Harvest glyph" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (SayCreep("w1", "⛏"))
                    "the bubble shows the creep's current Task"
            }

            test "each Task has its own glyph: Refill, Build, Upgrade" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0; worker "w2" 50 0; worker "w3" 50 0 ]
                    }

                let sticky =
                    Map.ofList
                        [
                            "w1", (taskId (Refill "spawn-1"))
                            "w2", (taskId (Build "site-1"))
                            "w3", (taskId (Upgrade "ctrl-1"))
                        ]

                let { Intents = intents } = decide snapshot sticky Set.empty None

                Expect.equal
                    (sayIntents intents)
                    [ "w1", "🔋"; "w2", "🔨"; "w3", "⚡" ]
                    "one bubble per assigned creep, glyph matched to its Task"
            }

            test "an unassigned creep says nothing" {
                // Nothing applicable for a full creep: no refill need, no
                // sites, no controller.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (sayIntents intents) "no Task, no bubble"
            }

            test "a creep still walking toward its target says its glyph anyway" {
                // Out of action range: no action Intent this tick, but the
                // assignment holds — the bubble reports it every tick.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
                Expect.equal (sayIntents intents) [ "w1", "⛏" ] "the bubble still shows the Task"
            }
        ]

[<Tests>]
let verdictTests =
    testList
        "matcher verdicts"
        [
            test "a lone applicable Task wins as the only candidate" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate) ]
                    "one creep, one candidate: the Verdict names the Task and the walkover"
            }

            test "rank decides: Refill outbids Upgrade for a loaded creep" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the feeding tier beat the surplus tier: rank decided"
            }

            test "rank layers by target: feeding the spawn outbids feeding the tower" {
                // The tower sits first in the pool, so only the target-layered
                // rank (ADR 0010) — not pool order — can hand the spawn the win.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "spawn-1" 50 BuiltKind.Spawn
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction before its guns: rank decided"
            }

            test "travel cost decides: the near source wins the rank tie" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-near"), MatchFactor.TravelCost) ]
                    "same rank, cheaper path: travel cost decided"
            }

            test "load decides: the second creep spreads to the emptier source" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.PoolOrder)
                        Verdict.Matched("w2", taskId (Harvest "src-b"), MatchFactor.Load)
                    ]
                    "w1's tie fell to pool order; w2 avoided the loaded source"
            }

            test "a remembered assignment kept is distinguishable from a fresh match" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-far") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Kept("w1", taskId (Harvest "src-far")) ]
                    "anti-thrash speaks as Kept, never as a fresh Matched"
            }

            test "a Task that left the pool releases with TaskGone" {
                // The remembered Refill target has no free capacity this
                // tick, so the Planner never generates the Task.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Refill "spawn-1") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Refill "spawn-1"), ReleaseReason.TaskGone))
                    "the release names the vanished Task"
            }

            test "a Task that left a pool we can see releases; the vision grace is about looking" {
                // #151's line, drawn from the other side. The grace holds an
                // assignment whose target left the pool **with the vision
                // that carried it**, and the sighting a room stamps while we
                // are looking at it must not be mistaken for that: a Refill
                // that filled, a store that emptied, a pile that decayed all
                // leave the pool while their target still stands in the
                // room's census, in full view. Holding those for 150 ticks
                // would stall the haul cycle every time an extension filled.
                // So what separates the two is the sighting's own tick, and
                // this is the pair that pins it — one field of one sighting
                // moves and nothing else.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let held = taskId (Refill "spawn-1")
                let sticky = Map.ofList [ "w1", held ]

                // The home room's own name under `SpatialInfo.empty`, which
                // is the room every fixture here files its facts under.
                let seenAt tick =
                    { snapshot with
                        Sightings =
                            Map.ofList
                                [
                                    "",
                                    {
                                        Tick = tick
                                        Targets = Set.singleton "spawn-1"
                                    }
                                ]
                    }

                Expect.contains
                    (decide (seenAt snapshot.Time) sticky Set.empty None).Verdicts
                    (Verdict.Released("w1", held, ReleaseReason.TaskGone))
                    "seen this tick, the target stands and the Task is gone all the same: released, as it always was"

                Expect.contains
                    (decide (seenAt (snapshot.Time - 1)) sticky Set.empty None).Verdicts
                    (Verdict.Kept("w1", held))
                    "and one tick of blindness later, the same disappearance is a room we cannot see and the holder is kept"
            }

            test "a drained source releases its harvester with TooEarly" {
                // Issue #48: anti-thrash must not pin a creep to a dry
                // rock. The Task stays pooled since ADR 0025, so the
                // release is the arrival gate's rather than TaskGone's, and
                // Inapplicable would make the transition log lie. No
                // projection here, so the walk prices at 0 the way ADR 0004
                // prices unplaced geometry — and the reason says so, beside
                // the wait it was compared against (#88); the same release
                // on real ground is pinned under "restock dispatch".
                let snapshot =
                    { bareRespawn with
                        Sources = [ drained "src-a" 120 ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.TooEarly(0, 120)
                    ))
                    "an arrival that covers no wait leaves the rock, exactly as ADR 0013 did"
            }

            test "a creep that fills up releases Harvest as Inapplicable and matches fresh" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable)
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "the handover carries both halves: why released, what won next"
            }

            test "a body that cannot do its remembered Task releases as Inapplicable" {
                // Part-based, not energy-state: the hauler has room to
                // harvest into but no Work part to harvest with.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let sticky = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "hauler",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Inapplicable
                    ))
                    "the missing Work part releases the assignment as Inapplicable"
            }

            test "a walled-off Work Area releases with Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 10; Y = 16 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
                    "no Seat can be reached: the release says so"
            }

            test "a remembered oversell releases with OverCapacity, the loser idles as NoneFree" {
                // One Seat at the source, two creeps remembered on it — an
                // oversell memory can carry across a redeploy. The
                // alphabetically first keeps; nothing else fits the loser.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                                })
                    }

                let sticky =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity)
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap releases the oversell and explains the loser's idleness"
            }

            test "an empty pool idles a creep with NoTasks" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoTasks) ]
                    "the Planner generated nothing at all"
            }

            test "a full creep with only Harvest on offer idles as NoneApplicable" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneApplicable) ]
                    "no Task fit the creep's body or energy state"
            }

            test "an applicable Task with an unreachable Work Area idles as NoneReachable" {
                // The source's one Seat is walled off; nothing else exists.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneReachable) ]
                    "the Task fit and had room, but no path reaches its Work Area"
            }

            test "a dead creep's dropped assignment speaks no Verdict" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = []
                    }

                let sticky = Map.ofList [ "ghost", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot sticky Set.empty None

                Expect.isEmpty (Map.toList assignments) "the dead creep's assignment is dropped"
                Expect.isEmpty verdicts "Verdicts attribute to living creeps only"
            }
        ]

[<Tests>]
let rankTierTests =
    testList
        "rank tiers"
        [
            test "the tier order is one sequence: feeding, then surplus, then the buffer" {
                // The Refill target layering (ADR 0010, ADR 0012) read top to
                // bottom by one body, one step at a time: the spawn six steps
                // away outbids the tower three away, and the tower outbids the
                // buffer underfoot. The buffer loses the second step, which is
                // what puts it below the surplus tier rather than beside it —
                // a tie there would hand the win to the container it stands on.
                let hungrySpawn = refillable "spawn-1" 50 BuiltKind.Spawn
                let fullSpawn = refillable "spawn-1" 0 BuiltKind.Spawn
                let hungryTower = refillable "tower-1" 500 BuiltKind.Tower

                let feeding =
                    decide (tierColony [ hungrySpawn; hungryTower ]) Map.empty Set.empty None

                Expect.equal
                    feeding.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction first: rank decided"

                let surplus =
                    decide (tierColony [ fullSpawn; hungryTower ]) Map.empty Set.empty None

                Expect.equal
                    surplus.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill "tower-1"), MatchFactor.Rank) ]
                    "reproduction fed, the guns outrank the buffer: rank decided"
            }

            test "tower Refill, Repair and Upgrade are one surplus rung; Build stands above" {
                // Pairwise, because the deciding factor is read off the winner
                // and its cheapest rival alone: pool all four at once and the
                // three-way tie hides whichever one left the tier. So each
                // surplus Task meets the tower Refill by itself, and pool order
                // — not rank — has to be what breaks the ties that remain.
                //
                // Build makes none of them any more (#234): it is the tier's
                // own top rung, so it outranks the tower's Refill on a fixture
                // where nothing is priceable and rank is the only thing that
                // can separate anything. That is the one comparison the rung
                // moves which is not Build-against-Upgrade, and it moves it for
                // the generalists alone — the row that feeds a tower is Carry
                // with no Work part, and no such body is applicable to a Build.
                //
                // The rung reads the site's room (`isHomeSite`) and this
                // fixture places nothing, which is the total resolving toward
                // home exactly as `isOutpostSite`'s does: absence
                // never counts against a Task (ADR 0004). The rung's *room*
                // is pinned where a room exists to pin it, in `OutpostTests`.
                let verdictsFor colony =
                    (decide colony Map.empty Set.empty None).Verdicts

                let tied =
                    [ Verdict.Matched("w1", taskId (Refill "tower-1"), MatchFactor.PoolOrder) ]

                Expect.equal
                    (verdictsFor
                        { surplusColony with
                            ConstructionSites = [ { Id = "site-1" } ]
                        })
                    [ Verdict.Matched("w1", taskId (Build "site-1"), MatchFactor.Rank) ]
                    "Build outranks the tower Refill: rank broke it, not pool order"

                // An *ordinary* Repair, deliberately: a road below the rescue
                // line has a rung of its own (#284) and would break this tie by
                // rank, which is the one thing this test is here to say Repair
                // does not do.
                Expect.equal
                    (verdictsFor (surplusColony |> withHits "road-1" BuiltKind.Road 2400 5000))
                    tied
                    "Repair ties the tower Refill: pool order broke it, not rank"

                Expect.equal
                    (verdictsFor
                        { surplusColony with
                            Controller = Some(controllerAt 1)
                        })
                    tied
                    "Upgrade ties the tower Refill: pool order broke it, not rank"
            }

            // The lane a loaded generalist really stands in (#234, live
            // t195,8xx): it fills at the [[buffer]] and is left standing in
            // the controller's Upgrade Work Area, where the Upgrade costs it
            // one step, applies to any load and never goes task-gone — so two
            // colonies holding fifty construction sites between them upgraded
            // with every worker they had. Pairwise on the pool alone: the same
            // lane, the same worker on the same tile, the site added and taken
            // away. The site is the **farther** of the two targets, so a win
            // on travel cost is not a win this case would accept.
            let siteDownTheLane sites =
                bufferLaneColony
                    [ "site-1", { X = 20; Y = 10 }, Site BuiltKind.Extension ]
                    sites
                    (creepWith "w" 100 0 (bodyFor workerPattern 300))

            test "a site down the lane outbids the controller beside the buffer" {
                Expect.equal
                    (matchOf (siteDownTheLane [ { Id = "site-1" } ]))
                    (Some(taskId (Build "site-1"), MatchFactor.Rank))
                    "three steps out against the controller's one, and the site wins on rank"

                Expect.equal
                    (matchOf (siteDownTheLane []))
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.Rank))
                    "with nothing standing to build, the same load goes into the controller"
            }

            test "the downgrade deadline still outranks a site" {
                // The rung is one step inside the surplus tier and the
                // deadline is a whole tier above the shallowest work there is
                // (ADR 0007, `deadlineRank`), so #234 does not reach it: a
                // controller about to lose a level takes back the load the
                // site had off it a tick before.
                let expiring =
                    { siteDownTheLane [ { Id = "site-1" } ] with
                        Controller =
                            Some
                                { controllerAt 2 with
                                    TicksToDowngrade = 4000
                                }
                    }

                Expect.equal
                    (matchOf expiring)
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.Rank))
                    "inside the deadline the controller outranks the site outright"
            }
        ]

[<Tests>]
let verboseScoringTests =
    testList
        "verbose scoring"
        [
            test "a verbose creep's Scoring covers the whole pool, scores and rejections both" {
                // Loaded and full: Harvest cannot fit the energy state, while
                // Refill and Upgrade score on the full key — no projection, so
                // every travel cost prices at 0.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
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
                                    RejectReason.Inapplicable
                                )
                                Candidate.Scored(taskId (Refill "spawn-1"), 0, 0, 0)
                                // Two tiers below the flow's zero, ten rungs
                                // apiece since #216 R5: the ladder gained
                                // room for a Task to be ordered inside its
                                // own tier, and `weightOfRank` divides the
                                // rungs back out (ADR 0052 decision 6).
                                Candidate.Scored(taskId (Upgrade "ctrl-1"), 20, 0, 0)
                            ]
                        )
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "every pool Task appears once: scored on the key or rejected at its gate"
            }

            test "a full Task rejects as CapacityFull; only the listed creep gets a Scoring" {
                // One Seat at the source, claimed by w1's match before w2's
                // turn: w2's scoring shows the cap, and its upgrade row shows
                // the empty carry. w1 is off the list and speaks no Scoring.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w2" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Scoring(
                            "w2",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.CapacityFull
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap that idled w2 is named per Task; the unlisted creep stays terse"
            }

            test "a kept creep's own single-Seat Task scores as held, never capacity-full" {
                // The creep's own claim is set aside for its scoring: the
                // Task it holds must read as the winning row, not as
                // rejected against its holder's own seat.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Scored(taskId (Harvest "src-a"), 0, 0, 0)
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                    ]
                    "the held Task is the scoring's winning row"
            }

            test "a walled-off Work Area rejects as Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
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
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneReachable)
                    ]
                    "the scoring pinpoints the gate the idle reason summarises"
            }
        ]
