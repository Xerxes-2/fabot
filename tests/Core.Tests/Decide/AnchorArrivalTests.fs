/// Expiring creeps and capacity judged at arrival (ADR 0026).
module Fabot.Core.Tests.Decide.AnchorArrivalTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.AnchorFixtures

[<Tests>]
let expiringTests =
    testList
        "expiring creeps"
        [
            test "an expiring creep leaves the count: the colony casts its replacement now" {
                // ADR 0026: spawning fills the gap between the target and
                // the creeps that will still be alive when a replacement
                // could arrive. The W12S28 fleet is whole, but its last
                // worker stands eight steps from the spawn at (20,10) —
                // seven from the tile a replacement is born on. That body
                // is five parts, so 15 ticks in the spawner, and its two
                // Move parts carry it over a plain tile in the walk's
                // one-tick floor: 7 ticks, one a tile (ADR 0029). A lead of
                // 22, so at 22 ticks left the worker is out of the count
                // and its successor is cast while it still works.
                let fleetWithLastWorker life =
                    { incomeColony with
                        Creeps =
                            List.truncate (List.length incomeFleet - 1) incomeFleet
                            @ [ worker "w19" 0 50 |> withLife life ]
                        Spatial =
                            incomeRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w19", { X = 12; Y = 10 } ]
                                })
                    }

                let casts life =
                    let { Intents = intents } =
                        decide (fleetWithLastWorker life) Map.empty Set.empty None

                    spawnIntents intents

                Expect.isEmpty (casts 23) "one tick outside its lead, the worker still counts"

                match casts 22 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "at its lead it is counted out"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an expiring Anchor leaves its row's gap, not just the count" {
                // The lane's one Post is garrisoned, so the colony's next
                // body is a hauler's. Once the garrison is expiring the
                // Anchor row is short again and its successor is cast first
                // — the whole point of counting a row's gap at arrival.
                let casts life =
                    let snapshot =
                        { successionColony with
                            // The generalist keeps the supply floor
                            // disarmed (ADR 0050): an Anchor alone can
                            // refill no extension.
                            Creeps = [ anchor "a1" 0 50 |> withLife life; worker "w1" 0 50 ]
                            Spatial =
                                successionRoom
                                |> withHome (fun layer ->
                                    { layer with
                                        CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                    })
                        }

                    let { Intents = intents } = decide snapshot Map.empty Set.empty None
                    spawnIntents intents

                match casts 1500 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "hauler-" "a living Anchor fills the row's gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts 5 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "anchor-" "an expiring one leaves it open"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the lead is the replacement's own body: long for an Anchor, short for a hauler" {
                // Nine plain steps from the spawn at (20,10) — eight from
                // the tile a replacement is born on — for two rows of the
                // same colony (ADR 0026). A fresh Anchor is empty and slow
                // — 4 cost units a step, 2 ticks of walk apiece, so 16
                // ticks of walking against 12 in the spawner: a lead of 28.
                // A hauler unit rides the walk's one-tick floor empty: 8
                // ticks of walking against 18 in the spawner, a lead of 26.
                // With 27 ticks left each, only the Anchor is inside its
                // own lead.
                let fleetAtPosts life =
                    { incomeColony with
                        Creeps =
                            incomeFleet
                            |> List.map (fun creep ->
                                if creep.Name = "a1" || creep.Name = "h1" then
                                    withLife life creep
                                else
                                    creep)
                        Spatial =
                            incomeRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "h1", { X = 29; Y = 10 } ]
                                })
                    }

                let casts life =
                    let { Intents = intents } = decide (fleetAtPosts life) Map.empty Set.empty None

                    spawnIntents intents |> List.map (fun (_, _, creepName) -> creepName)

                match casts 27 with
                | [ anchorCast ] ->
                    Expect.stringStarts anchorCast "anchor-" "the Anchor's lead outlasts 27 ticks"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts 26 with
                | [ anchorCast; haulerCast ] ->
                    Expect.stringStarts anchorCast "anchor-" "under both leads both rows are short"
                    Expect.stringStarts haulerCast "hauler-" "the hauler's lead is the shorter one"
                | other -> failtest $"expected exactly two SpawnCreep intents, got %A{other}"
            }

            test "the lead is the walk out of the spawner, not the step onto its tile" {
                // The engine places a finished creep on a free neighbour,
                // which for this lane is (20,10): the replacement walks
                // nine steps, not ten. At a 600 bank the Anchor row is five
                // Work over one Move — 10 cost units a plain step, 21 ticks
                // in the spawner — so the lead is 21 + 45 = 66. Charging
                // the step out of the spawner's own tile would make it 71
                // and cast the successor five ticks early, into a Post its
                // predecessor still reads as full.
                let casts life =
                    let snapshot =
                        { rcl3Succession "a1" "a2" life with
                            Creeps =
                                [
                                    creepWith
                                        "a1"
                                        0
                                        50
                                        [ Work; Work; Work; Work; Work; Carry; Move ]
                                    |> withLife life
                                    // The supply floor's premise (ADR
                                    // 0050) and not this case's: a lone
                                    // Anchor can refill no extension.
                                    worker "w1" 0 50
                                ]
                        }

                    let { Intents = intents } = decide snapshot Map.empty Set.empty None
                    spawnIntents intents |> List.map (fun (_, _, creepName) -> creepName)

                match casts 67 with
                | [ creepName ] ->
                    Expect.stringStarts
                        creepName
                        "hauler-"
                        "one tick outside its lead the Anchor still counts"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts 66 with
                | [ creepName ] ->
                    Expect.stringStarts creepName "anchor-" "at its lead the row is short again"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an expiring creep keeps its Task, whichever name the release fold reaches first" {
                // ADR 0026: an expiring creep is not released — anti-thrash
                // keeps it working to the last tick. The release fold walks
                // creep names in order, so the successor can be judged
                // first, take the slot its predecessor's arrival-priced
                // death frees, and leave the incumbent reading its own Post
                // as full. Both orders keep both creeps.
                let bothKept incumbent successor =
                    let remembered =
                        Map.ofList
                            [
                                incumbent, taskId (Harvest "src-a")
                                successor, taskId (Harvest "src-a")
                            ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (succession incumbent successor 5) remembered Set.empty None

                    Expect.isEmpty
                        (releases verdicts)
                        "the succession is the cap agreeing with the gap, not an oversell"

                    Expect.equal
                        (harvesters assignments "src-a" |> List.sort)
                        (List.sort [ incumbent; successor ])
                        "the incumbent digs to the last tick and the successor walks"

                bothKept "a-old" "z-new"
                bothKept "z-old" "a-new"
            }
        ]

[<Tests>]
let arrivalCapacityTests =
    testList
        "capacity at arrival"
        [
            test "a holder dead before the candidate arrives holds none of the Post" {
                // ADR 0026: the lane's one Post admits one garrison, and
                // the incumbent has 5 ticks left against a successor nine
                // steps — 41 ticks — up the lane. It will be gone before
                // the successor gets there, so it holds none of the cap and
                // the successor leaves now instead of after the death.
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let { Assignments = assignments } =
                    decide (succession "a1" "a2" 5) remembered Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "a2" ]
                    "the Post carries the succession, not two standing garrisons"
            }

            test "a holder that outlives the walk still fills the Post" {
                // The other half of the same gate: a garrison that will
                // still be standing there when the candidate arrives holds
                // the cap exactly as ADR 0024 has it, and the candidate is
                // turned away with nothing free.
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide (succession "a1" "a2" 1500) remembered Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "one Post, one garrison, for as long as the garrison lives"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a2", IdleReason.NoneFree))
                    "the second Anchor is idle for want of standing room, not for want of time"
            }

            test "the succession's margin is spent: the walk and the lead price the same ground" {
                // ADR 0026 read this margin as the occupancy surcharge on
                // the incumbent's own tile — the lead was traffic-blind and
                // the arrival was not, which bought the successor five
                // ticks. ADR 0029 makes the arrival traffic-blind too, and
                // the five ticks are gone: nine steps at five ticks a step
                // is a walk of 45, and the lead over the same lane is 66 —
                // 21 in the spawner and the same 45 of walking. So the
                // incumbent has exactly 45 ticks left the tick its
                // successor stands on the birth tile, and the window is
                // read at equality: 44 admits it, 45 does not. The margin
                // ADR 0026 named is no longer there to spend, and a
                // successor born on the boundary idles the tick before the
                // window opens.
                let admits life =
                    let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (rcl3Succession "a1" "a2" life) remembered Set.empty None

                    harvesters assignments "src-a", releases verdicts, verdicts

                let harvesting, released, _ = admits 44

                Expect.equal
                    harvesting
                    [ "a1"; "a2" ]
                    "a predecessor gone before the walk ends holds none of the Post"

                Expect.isEmpty released "and the predecessor keeps digging"

                let harvesting, released, verdicts = admits 45

                Expect.equal
                    harvesting
                    [ "a1" ]
                    "still standing there when the successor arrives, the Post reads full"

                Expect.isEmpty released "the incumbent is still never released for it"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a2", IdleReason.NoneFree))
                    "and the successor idles until the window opens a tick later"
            }

            test "a holder still walking when the candidate dies holds none of it either" {
                // The window is read from both ends (ADR 0026). This
                // garrison has 35 ticks left against a lead of 30 — it is
                // not expiring, and nothing is being cast to replace it —
                // while the Anchor nine steps up the lane is 41 ticks
                // away. Neither is standing on the tile while the other
                // is, so neither counts against the other, and the release
                // fold reaches the pair in creep-name order without that
                // order deciding anything: a window read only from the
                // candidate's end released whichever of the two the fold
                // came to second.
                let bothKept post far =
                    let remembered =
                        Map.ofList [ post, taskId (Harvest "src-a"); far, taskId (Harvest "src-a") ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (succession post far 35) remembered Set.empty None

                    Expect.isEmpty
                        (releases verdicts)
                        "a garrison nowhere near its own lead is not evicted by a distant candidate"

                    Expect.equal
                        (harvesters assignments "src-a" |> List.sort)
                        (List.sort [ post; far ])
                        "both keep what they were remembered on"

                bothKept "z-post" "a-far"
                bothKept "a-post" "z-far"
            }
        ]
