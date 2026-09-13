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
                let snapshot = blockedLane [ 12 ] { worker "har" 0 50 with Fatigue = 4 }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "bob", Build "site-1" ])
                    [ Verdict.Grounded "har"; Verdict.Yielded("bob", "har") ]
                    "har is grounded; bob's blocked step names the tired creep holding the tile"
            }

            test "a lone fatigued traveller is grounded, nothing more" {
                let snapshot =
                    corridorColony
                        [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        [ "w1", { X = 10; Y = 14 } ]

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
                            |> withCreepsAt [ "har", { X = 10; Y = 12 }; "upg", { X = 10; Y = 11 } ]
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
                            |> withCreepsAt [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
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
                let snapshot = blockedLane [ 12; 13 ] (worker "har" 0 50)

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
                let snapshot = corridorColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 14 } ]

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
                let snapshot =
                    corridorColony
                        [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        [ "w1", { X = 10; Y = 14 } ]

                let { Verdicts = verdicts } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot

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
                            "w1", (taskId (Refill("spawn-1", Energy)))
                            "w2", (taskId (Build "site-1"))
                            "w3", (taskId (Upgrade "ctrl-1"))
                        ]

                let { Intents = intents } = decideFrom sticky snapshot

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

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (sayIntents intents) "no Task, no bubble"
            }

            test "a creep still walking toward its target says its glyph anyway" {
                // Out of action range: no action Intent this tick, but the
                // assignment holds — the bubble reports it every tick.
                let snapshot = corridorColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 14 } ]

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
                Expect.equal (sayIntents intents) [ "w1", "⛏" ] "the bubble still shows the Task"
            }

            test "a miner says nothing on the ticks the extractor's clock holds it" {
                // ADR 0057 decision 2 writes the bubble out: ⛏ on the ticks it
                // digs and **nothing on the ticks it waits**, so the one-in-six
                // rhythm `EXTRACTOR_COOLDOWN` imposes is legible in the viewer
                // rather than hidden behind a glyph that claims a dig every
                // tick. Read off the same gate the act is withheld by, so the
                // bubble and the Intent can never come to disagree. Pairwise,
                // one number on one clock apart.
                let sayingAt ticks =
                    { mineColony with
                        Creeps = [ miner "m1" ]
                        Spatial = mineColony.Spatial |> withCreepsAt [ "m1", minePost ]
                    }
                    |> onCooldown ticks
                    |> decideOn
                    |> fun decision -> sayIntents decision.Intents

                Expect.equal (sayingAt 0) [ "m1", "⛏" ] "a zero clock is a dig, and it says so"

                Expect.isEmpty (sayingAt 3) "and a clock still running says nothing"
            }
        ]

[<Tests>]
let extractorCooldownTests =
    testList
        "the extractor's cooldown"
        [
            // The miner standing on its own mine [[post]], which is where the
            // whole of this group is read: nothing here is about a walk.
            let standing colony =
                { colony with
                    Creeps = [ miner "m1" ]
                    Spatial = colony.Spatial |> withCreepsAt [ "m1", minePost ]
                }

            let held = taskId (Harvest "min-a")

            test "the harvest is issued on a zero clock and withheld on every other" {
                // `EXTRACTOR_COOLDOWN` is 5 and the engine runs the intent pass
                // before the object pass — `extractors/tick.js` writes the 5 at
                // the end of the harvest tick and decrements it once a tick
                // after — so successive harvests land **six** ticks apart and
                // the other five are refused outright. Issuing one anyway is
                // an `ERR_TIRED` a tick for five ticks in six.
                let digsAt ticks =
                    standing mineColony
                    |> onCooldown ticks
                    |> decideOn
                    |> fun decision -> digIntentsFor "m1" decision.Intents

                Expect.equal
                    (digsAt 0)
                    [ HarvestSource("m1", "min-a") ]
                    "a clock reading zero is this tick, and the dig goes out"

                for ticks in 1..5 do
                    Expect.isEmpty (digsAt ticks) $"and a clock reading {ticks} withholds it"
            }

            test "a miner keeps its Task through the ticks it cannot dig" {
                // The gate is in the **Emitter** and never in applicability,
                // which is the distinction ADR 0013 and ADR 0025 spent two
                // decisions on: a cooldown is five ticks long and a re-match is
                // a flood, so a Task that vanished and returned every sixth
                // tick would churn the pool for a body that has nowhere else to
                // be and no way to get there. The Task exists exactly while the
                // deposit does; what the cooldown decides is whether this
                // tick's act is issued.
                let sticky = Map.ofList [ "m1", held ]

                let verdictsAt ticks =
                    standing mineColony
                    |> onCooldown ticks
                    |> decideFrom sticky
                    |> fun decision -> decision.Verdicts

                Expect.contains
                    (verdictsAt 0)
                    (Verdict.Kept("m1", held))
                    "the premise: a digging tick keeps the body on its Task"

                Expect.contains
                    (verdictsAt 4)
                    (Verdict.Kept("m1", held))
                    "and so does a waiting one — the body stands, and the Verdict does not claim it dug"
            }

            test "a deposit with no extractor standing is dug on no tick at all" {
                // Held on the same footing and not by a different rule:
                // `harvest.js` refuses a mineral with no extractor on its tile,
                // so the act is as impossible as it is on a cooldown tick, and
                // issuing it would be one `ERR_NOT_FOUND` a tick for as long as
                // the site takes to build. Pairwise against the case above, one
                // target kind apart.
                Expect.isEmpty
                    (digIntentsFor
                        "m1"
                        (decideFrom
                            (Map.ofList [ "m1", held ])
                            (standing (mineColony |> withExtractorSite)))
                            .Intents)
                    "a site extracts nothing, whatever its clock would have read"
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

                let { Verdicts = verdicts } = decideOn snapshot

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

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank) ]
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

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank) ]
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

                let { Verdicts = verdicts } = decideOn snapshot

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

                let { Verdicts = verdicts } = decideOn snapshot

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
                let { Verdicts = verdicts } = decideFrom sticky snapshot

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

                let sticky = Map.ofList [ "w1", taskId (Refill("spawn-1", Energy)) ]
                let { Verdicts = verdicts } = decideFrom sticky snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Refill("spawn-1", Energy)),
                        ReleaseReason.TaskGone
                    ))
                    "the release names the vanished Task"
            }

            test "a repair runs to the whole line and then releases TaskGone, once per job" {
                // ADR 0061: the rule does not stop a structure crossing its
                // whole line, it stops it crossing in one tick. What a holder
                // buys is the band between the two lines — released nowhere in
                // it, released `task-gone` past it — so the count that falls is
                // one release per repair **job** instead of one per repair
                // **tick**. The pairwise is the two hits values and nothing
                // else.
                let repairing hits =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road hits 5000

                let sticky = Map.ofList [ "w1", taskId (Repair "road-1") ]

                Expect.equal
                    (decideFrom sticky (repairing 3900)).Verdicts
                    [ Verdict.Kept("w1", taskId (Repair "road-1")) ]
                    "inside the band the holder is kept: the repair is a job, not a reflex"

                Expect.contains
                    (decideFrom sticky (repairing 4100)).Verdicts
                    (Verdict.Released("w1", taskId (Repair "road-1"), ReleaseReason.TaskGone))
                    "past the whole line the Task is gone and `task-gone` is still the reason"
            }

            test "a holder that empties inside the band is released Inapplicable, not TaskGone" {
                // The other release of the two lines, and on the dearest
                // decaying kind it is the **normal** one: `applicable` for a
                // Repair is `spending && not standing`, so a body that runs dry
                // mid-band goes `inapplicable` while its target is still
                // pooled. The container's band is 75,000 hits — 750 energy at a
                // hundred hits an energy — against the 600 the live worker
                // carries, so a container entered at the hungry line ends its
                // job around 0.74 and this is the reason its release names.
                // Pinned because ADR 0061's own landing check reads the
                // transition log for `task-gone` alone, and on a container it
                // will not find one (#323, where the number is re-derived).
                let emptied =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 185_000 250_000

                let sticky = Map.ofList [ "w1", taskId (Repair "cont-1") ]

                Expect.contains
                    (decideFrom sticky emptied).Verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Repair "cont-1"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "an empty store inside the band is inapplicable: the Task is still there"
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

                let held = taskId (Refill("spawn-1", Energy))
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
                    (decideFrom sticky (seenAt snapshot.Time)).Verdicts
                    (Verdict.Released("w1", held, ReleaseReason.TaskGone))
                    "seen this tick, the target stands and the Task is gone all the same: released, as it always was"

                Expect.contains
                    (decideFrom sticky (seenAt (snapshot.Time - 1))).Verdicts
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
                let { Verdicts = verdicts } = decideFrom sticky snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(0, 120))
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
                let { Verdicts = verdicts } = decideFrom sticky snapshot

                Expect.equal
                    verdicts
                    [
                        Verdict.Released(
                            "w1",
                            taskId (Harvest "src-a"),
                            ReleaseReason.Rejected RejectReason.Inapplicable
                        )
                        Verdict.Matched("w1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank)
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
                let { Verdicts = verdicts } = decideFrom sticky snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "hauler",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
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
                            |> withCreepsAt [ "w1", { X = 10; Y = 14 } ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decideFrom sticky snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Unreachable
                    ))
                    "no Seat can be reached: the release says so"
            }

            test "a remembered oversell releases with OverCapacity, the loser idles as NoneFree" {
                // One Seat at the source, two creeps remembered on it — an
                // oversell memory can carry across a redeploy. The nearer of
                // the two keeps (#230), which here is also the
                // alphabetically first; nothing else fits the loser.
                let snapshot =
                    shortCorridorColony
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]

                let sticky =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decideFrom sticky snapshot

                Expect.equal
                    verdicts
                    [
                        Verdict.Released(
                            "w2",
                            taskId (Harvest "src-a"),
                            ReleaseReason.Rejected RejectReason.CapacityFull
                        )
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap releases the oversell and explains the loser's idleness"
            }

            test "an oversold cap releases the furthest holder, whatever the names are" {
                // #230: the release fold judges each remembered assignment
                // against the ones it has already kept, so the order it walks
                // a Task's holders in *is* the rule for who keeps the slot.
                // In memory order that was the creep-name order, so the body
                // already standing on the one Seat could lose it to one two
                // tiles further down the corridor — ADR 0054 records the same
                // price on the [[refill cluster]] and defers the fix to here,
                // because it is the Matcher's release order for every capped
                // Task and not that Task's rule.
                //
                // Pairwise on the one thing that may decide it: the two
                // bodies swap names between the halves and stand where they
                // stood, so a half that changes is name order deciding.
                let releasedFrom near far =
                    let snapshot =
                        shortCorridorColony
                            [ worker near 0 50; worker far 0 50 ]
                            [ near, { X = 10; Y = 12 }; far, { X = 10; Y = 14 } ]

                    let sticky =
                        Map.ofList [ near, taskId (Harvest "src-a"); far, taskId (Harvest "src-a") ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decideFrom sticky snapshot

                    Map.tryFind near assignments,
                    verdicts
                    |> List.choose (function
                        | Verdict.Released(name, _, ReleaseReason.Rejected RejectReason.CapacityFull) ->
                            Some name
                        | _ -> None)

                Expect.equal
                    (releasedFrom "w1" "w2")
                    (Some(taskId (Harvest "src-a")), [ "w2" ])
                    "the body on the Seat keeps it and the far one pays for the cap"

                Expect.equal
                    (releasedFrom "w2" "w1")
                    (Some(taskId (Harvest "src-a")), [ "w1" ])
                    "names swapped and nothing moves: it is proximity deciding, not the fold's order"
            }

            test "an unplaced holder does not outrank the body standing on the Seat" {
                // #230, the other half of the order: `Atlas.walkTicks` answers
                // `Some 0` for a body the projection cannot place (ADR 0004's
                // escape — unpriceable geometry never counts against a Task),
                // which is the same number it answers for a body standing in
                // the Work Area. Ranked on that number alone the ghost sorts
                // level with — and by name ahead of — the body we can watch
                // standing on the one Seat, and the cap gives the slot to the
                // one nobody can find. A holder with no tile has no distance
                // from anything, so it sorts behind every holder that has one.
                let releasedFrom placed ghost =
                    let snapshot =
                        shortCorridorColony
                            [ worker placed 0 50; worker ghost 0 50 ]
                            [ placed, { X = 10; Y = 11 } ]

                    let sticky =
                        Map.ofList
                            [ placed, taskId (Harvest "src-a"); ghost, taskId (Harvest "src-a") ]

                    let { Assignments = assignments } = decideFrom sticky snapshot

                    Map.tryFind placed assignments, Map.tryFind ghost assignments

                Expect.equal
                    (releasedFrom "w1" "w2")
                    (Some(taskId (Harvest "src-a")), None)
                    "the body on the Seat keeps it; the one with no tile is the one released"

                Expect.equal
                    (releasedFrom "w2" "w1")
                    (Some(taskId (Harvest "src-a")), None)
                    "and names swapped, so it is placement deciding and not the fold's order"
            }

            test "a holder the geometry disconnects sorts last under either name order" {
                // #230's tie-break has no distance to read for a walled-off
                // body, so it sorts behind the one it can price — the same
                // place an unplaced body sorts, and for the same reason. What
                // that settles is the Verdict: the cap is full by the time the
                // walled-off body is judged, and the gate cascade gives a pair
                // over a full cap `CapacityFull` whichever way it is read
                // (fresh candidate or remembered assignment). Judging it
                // *first* instead would earn it `Unreachable` and make the
                // reason a function of where the fold sorted it, which is the
                // kind of answer #230 exists to remove; the gate order is
                // where reachability's precedence lives, and it is deliberate.
                let reasonsFrom islanded seated =
                    let terrain =
                        [
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Plain
                            { X = 20; Y = 20 }, Plain
                        ]

                    let snapshot =
                        { bareRespawn with
                            Sources = [ source "src-a" ]
                            Creeps = [ worker islanded 0 50; worker seated 0 50 ]
                            Spatial =
                                spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                                |> withCreepsAt
                                    [ islanded, { X = 20; Y = 20 }; seated, { X = 10; Y = 11 } ]
                        }

                    let sticky =
                        Map.ofList
                            [ islanded, taskId (Harvest "src-a"); seated, taskId (Harvest "src-a") ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decideFrom sticky snapshot

                    Map.tryFind seated assignments,
                    verdicts
                    |> List.choose (function
                        | Verdict.Released(name, _, ReleaseReason.Rejected reason) when
                            name = islanded
                            ->
                            Some reason
                        | _ -> None)

                Expect.equal
                    (reasonsFrom "w1" "w2")
                    (Some(taskId (Harvest "src-a")), [ RejectReason.CapacityFull ])
                    "the Seat keeps the body that can reach it and the cap answers the other"

                Expect.equal
                    (reasonsFrom "w2" "w1")
                    (Some(taskId (Harvest "src-a")), [ RejectReason.CapacityFull ])
                    "and the same with the names swapped, where memory order used to give either"
            }

            test "an empty pool idles a creep with NoTasks" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decideOn snapshot

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

                let { Verdicts = verdicts } = decideOn snapshot

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
                            |> withCreepsAt [ "w1", { X = 10; Y = 14 } ]
                    }

                let { Verdicts = verdicts } = decideOn snapshot

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
                    decideFrom sticky snapshot

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

                let feeding = decideOn (tierColony [ hungrySpawn; hungryTower ])

                Expect.equal
                    feeding.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction first: rank decided"

                let surplus = decideOn (tierColony [ fullSpawn; hungryTower ])

                Expect.equal
                    surplus.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill("tower-1", Energy)), MatchFactor.Rank) ]
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
                let verdictsFor colony = (decideOn colony).Verdicts

                let tied =
                    [
                        Verdict.Matched(
                            "w1",
                            taskId (Refill("tower-1", Energy)),
                            MatchFactor.PoolOrder
                        )
                    ]

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

            test "a bleeding mineral container outbids the bank the body is standing on" {
                // #306, live at t402,520 and the whole of the ticket: W13S28 had
                // banked 485,916 energy and **no** Thorium with its mineral
                // container standing full at 2,000 and a ground pile growing
                // under the [[miner]], while W12S28 — the same code, an empty
                // Storage — had banked 3,600. Both draws rank at `StockDraw`
                // (ADR 0023, ADR 0057 decision 3), so travel cost decided, and
                // the bank is always the nearer of the two: a healthy colony
                // never drew its own mine, and the healthier it was the worse it
                // got.
                //
                // Pairwise on the mine's stock alone, with the body put on the
                // **Storage's** own Seat so the ore has to win on rank and can
                // never win on distance.
                let banked stock =
                    let stocked = mineHaulColony |> withMineStock stock

                    let colony =
                        { stocked with
                            Creeps = [ hauler "h1" 0 200 ]
                            // A mouth for the bank: the Storage's own Withdraw
                            // is pooled only where the colony has somewhere to
                            // put the energy (ADR 0023). The spawn is unplaced
                            // and its Refill unreachable, which is deliberate —
                            // an **empty** body is applicable to neither Refill
                            // nor cluster, so the pair this case is about is the
                            // only pair there is.
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.add "sto-1" 485_916 colony.Spatial.Stores
                            }
                            |> withCreepsAt [ "h1", { X = 13; Y = 10 } ]
                    }

                let haulerTakes colony =
                    (decideOn colony).Verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (haulerTakes (banked 999))
                    (Some(taskId (Withdraw("sto-1", Energy)), MatchFactor.TravelCost))
                    "the premise: tied on rank, the bank underfoot takes the body every time"

                Expect.equal
                    (haulerTakes (banked Tuning.defaults.MineContactCliff))
                    (Some(taskId (Withdraw("can-min", Thorium)), MatchFactor.Rank))
                    "past the contact cliff the ore outranks the bank and the mine is drawn"
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
                                Candidate.Scored(taskId (Refill("spawn-1", Energy)), 0, 0, 0)
                                // Two tiers below the flow's zero, ten rungs
                                // apiece since #216 R5: the ladder gained
                                // room for a Task to be ordered inside its
                                // own tier, and `weightOfRank` divides the
                                // rungs back out (ADR 0052 decision 6).
                                Candidate.Scored(taskId (Upgrade "ctrl-1"), 20, 0, 0)
                            ]
                        )
                        Verdict.Matched("w1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank)
                    ]
                    "every pool Task appears once: scored on the key or rejected at its gate"
            }

            test "a full Task rejects as CapacityFull; only the listed creep gets a Scoring" {
                // One Seat at the source, claimed by w1's match before w2's
                // turn: w2's scoring shows the cap, and its upgrade row shows
                // the empty carry. w1 is off the list and speaks no Scoring.
                let snapshot =
                    shortCorridorColony
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]

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
                let snapshot = shortCorridorColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]

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
                            |> withCreepsAt [ "w1", { X = 10; Y = 14 } ]
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

[<Tests>]
let resourceIdVerdictTests =
    testList
        "a Task id that names a resource"
        [
            test "the vision grace reads the store out of a Thorium Withdraw's id" {
                // A Task id has two colons since ADR 0057 decision 3 put a
                // resource on the [[withdraw]] and the [[refill]], and the
                // vision grace (#151) holds an *id* and no Task — the Task it
                // named has left the pool — so what it has to find in that id is
                // the target the room's census files, between the first colon
                // and the next. Neither an object id nor a room name has ever
                // held a colon, which is what makes the reading total.
                //
                // Pairwise on the resource alone: the same store, the same dark
                // room, the same grace, held under each of the two ids. Read
                // whole, the Thorium id's target would be "can-min:Thorium",
                // which the sighting does not hold, and the grace would quietly
                // stop covering exactly the Tasks this ticket added.
                let home = SpatialInfo.homeName mineHaulColony.Spatial

                // The store gone from the projection, so the Task really has
                // left the pool and the grace is the only thing that can keep
                // its holder.
                let gone = mineHaulColony |> withoutMineContainer

                let blind =
                    { gone with
                        Time = 1000
                        Creeps = [ hauler "h1" 0 200 ]
                    }

                let verdictsUnder sightings held =
                    (decideFrom
                        (Map.ofList [ "h1", taskId held ])
                        { blind with Sightings = sightings })
                        .Verdicts

                let seen =
                    Map.ofList
                        [
                            home,
                            {
                                Tick = 999
                                Targets = Set.singleton "can-min"
                            }
                        ]

                for held in [ Withdraw("can-min", Energy); Withdraw("can-min", Thorium) ] do
                    Expect.contains
                        (verdictsUnder seen held)
                        (Verdict.Kept("h1", taskId held))
                        $"the grace finds the store in %s{taskId held}"

                    Expect.contains
                        (verdictsUnder Map.empty held)
                        (Verdict.Released("h1", taskId held, ReleaseReason.TaskGone))
                        $"and with nothing seen there is no grace to keep %s{taskId held}"
            }
        ]
