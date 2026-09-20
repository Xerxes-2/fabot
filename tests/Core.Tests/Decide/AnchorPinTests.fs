/// The Anchor and the heavy pin that ties it to its own rock. ADR-0048
module Fabot.Core.Tests.Decide.AnchorPinTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.AnchorFixtures

[<Tests>]
let anchorTests =
    testList
        "anchor"
        [
            test "an Anchor on a plain Seat walks to the Post instead of digging there" {
                // Controller far from every Seat, a built container on the
                // Seat at (9,10), and the Anchor on the plain Seat (10,11).
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { spatial
                                  [
                                      "src-a", { X = 10; Y = 10 }
                                      "ctrl-1", { X = 40; Y = 40 }
                                      "cont-1", { X = 9; Y = 10 }
                                  ]
                                  (openSeats { X = 10; Y = 10 }) with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "src-a", Source
                                            "ctrl-1", Controller
                                            "cont-1", Structure BuiltKind.Container
                                        ]
                            }
                            |> withCreepsAt [ "a1", { X = 10; Y = 11 } ]
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn snapshot

                Expect.isEmpty
                    (actionIntents intents
                     |> List.filter (function
                         | HarvestSource _ -> true
                         | _ -> false))
                    "off the Post a heavy body does not dig, so it never fills"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopLeft) ]
                    "it steps toward the container Seat"

                Expect.contains
                    verdicts
                    (Verdict.Matched("a1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "and stays matched to Harvest the whole walk"
            }

            test "an Anchor that is already full off-Post commutes nowhere: it walks to its Post" {
                // The controller four tiles away is not a candidate: a
                // Work-heavy body takes Upgrade only where it can already
                // act on it, and a full store off the Post is a walk, not a
                // release.
                let room =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 14; Y = 11 }
                              "cont-1", { X = 9; Y = 10 }
                          ]
                          (openSeats { X = 10; Y = 10 }
                           @ [ for x in 11..14 -> { X = x; Y = 11 }, Plain ]) with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "cont-1", Structure BuiltKind.Container
                                ]
                    }
                    |> withCreepsAt [ "a1", { X = 10; Y = 11 } ]

                let full =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = room
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn full

                Expect.contains
                    verdicts
                    (Verdict.Matched("a1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "the far controller is not a candidate at all; the Post it came off is the only one"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopLeft) ]
                    "so it walks the one tile back rather than four to spend one Carry"

                Expect.isEmpty
                    (digIntentsFor "a1" intents)
                    "and digs nothing on the way: the overflow reprieve is still the container's alone"

                // The tick after the walk, on the Post it was walking to.
                let arrived =
                    { full with
                        Spatial = full.Spatial |> withCreepsAt [ "a1", { X = 9; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered arrived

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "arrived, ADR 0024's own condition holds it there"

                Expect.isEmpty (moveIntentsFor "a1" intents) "and it stays"
            }

            test "a Dual Seat and banked capacity plan an Anchor body" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "the Anchor row sized to the 300 bank"

                    Expect.stringStarts creepName "anchor-" "the name carries the anchor row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "without a Dual Seat only generalists are planned" {
                // Same Seats, controller placed far away: no Seat falls in
                // its Upgrade Work Area, so there is no Dual Seat to cast for.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "src-a", { X = 10; Y = 10 }
                                                "ctrl-1", { X = 40; Y = 40 }
                                            ]
                                })
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body (workerBodyFor 300) "the worker row sized to the bank"
                    Expect.stringStarts creepName "worker-" "the name carries the worker row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a source-container Post with no Dual Seat casts an Anchor at full bank" {
                // The W12S28 shape: controller far from every Seat, but a
                // built container stands on the Seat at (9,10) — a Post,
                // so the Anchor row comes alive without any Dual Seat.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetKinds =
                                    dualSeatRoom.TargetKinds
                                    |> Map.add "cont-1" (Structure BuiltKind.Container)
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "src-a", { X = 10; Y = 10 }
                                                "ctrl-1", { X = 40; Y = 40 }
                                                "cont-1", { X = 9; Y = 10 }
                                            ]
                                })
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "the Anchor row sized to the 300 bank"

                    Expect.stringStarts creepName "anchor-" "the container Seat is a Post"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a capped Anchor is cast the tick its bank holds the body's cost, not a full bank" {
                // At RCL4 the bank caps at 1,300 but the Anchor row prices
                // at 700 (6W1C1M).
                let snapshot =
                    { dualSeatColony with
                        Bank = bank 700 1300
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "the capped Anchor row, priced at exactly the bank's holding"

                    Expect.stringStarts creepName "anchor-" "the name carries the anchor row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a bank short of the capped Anchor's cost still waits" {
                let snapshot =
                    { dualSeatColony with
                        Bank = bank 650 1300
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty (spawnIntents intents) "650 does not buy 6W1C1M"
            }

            test "the Anchor quota counts Posts: a Dual Seat plus a container Seat want two" {
                // One living Anchor covers the Dual Seat; the built
                // container on the other Seat is a second Post. The
                // generalist keeps the supply floor disarmed.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50; worker "w1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetKinds =
                                    dualSeatRoom.TargetKinds
                                    |> Map.add "cont-1" (Structure BuiltKind.Container)
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        layer.TargetPositions |> Map.add "cont-1" { X = 9; Y = 10 }
                                })
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "anchor-" "the second Post's gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a living Anchor fills the quota: the remaining gap goes generalist" {
                // The generalist keeps the supply floor disarmed.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50; worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the one Dual Seat is already worked"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            // Three Seats — (11,10) the Dual Seat, (9,10) and (9,9) ordinary —
            // and a second idle spawn drawing from the same bank.
            let threeSeatRoom =
                dualSeatRoom
                |> withHome (fun layer ->
                    { layer with
                        Terrain =
                            TerrainGrid.ofList
                                [
                                    { X = 9; Y = 10 }, Plain
                                    { X = 11; Y = 10 }, Plain
                                    { X = 9; Y = 9 }, Plain
                                ]
                    })

            let secondSpawn =
                { spawn with
                    Name = "Spawn2"
                    Id = "spawn-2"
                }

            test "the Anchor gap is filled before generalist gaps" {
                let snapshot =
                    { dualSeatColony with
                        Spawns = [ spawn; secondSpawn ]
                        Bank = bank 600 300
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeatRoom
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, firstBody, firstName); (_, _, secondName) ] ->
                    Expect.stringStarts firstName "anchor-" "the Anchor gap is filled first"
                    Expect.equal firstBody [ Work; Work; Carry; Move ] "the Anchor row's body"
                    Expect.stringStarts secondName "worker-" "the generalist fills the remainder"
                | other -> failtest $"expected exactly two SpawnCreep intents, got %A{other}"
            }

            test "planned creeps never exceed the workforce target" {
                // One Post and four income workers make a target of five:
                // the Post's 6,000 of lifetime income (four a tick, what the
                // `2W/1C/1M` this 300 bank casts digs) less the Anchor's 300
                // of amortization over 1500 is 3.8, rounded up. Four living
                // leave one gap; the second idle spawn must stay quiet.
                let snapshot =
                    { dualSeatColony with
                        Spawns = [ spawn; secondSpawn ]
                        Bank = bank 600 300
                        Creeps = anchor "a1" 0 50 :: [ for i in 1..3 -> worker $"w{i}" 0 50 ]
                        Spatial = threeSeatRoom
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "the Anchor quota lives inside the target, never on top of it"
            }

            test "an empty Anchor on its Dual Seat is assigned Harvest without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial = dualSeatRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "an empty store calls for Harvest"

                Expect.contains intents (HarvestSource("a1", "src-a")) "the action fires in place"
                Expect.isEmpty (moveIntentsFor "a1" intents) "no movement step is emitted"
            }

            test "a full Anchor on its Dual Seat is assigned Upgrade without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = dualSeatRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a full store calls for Upgrade"

                Expect.contains
                    intents
                    (UpgradeController("a1", "ctrl-1"))
                    "the action fires in place"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no movement step is emitted"
            }

            test
                "alternation is emergent: a filled-up Anchor's Harvest releases and rematches to Upgrade" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = dualSeatRoom |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decideFrom remembered snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "ordinary applicability release + rematch flips the assignment"
            }

            // The Dual Seat room extended east: a plain corridor from
            // (12,10) to (30,10) carrying distant mobile work at its end.
            let corridorEast extraTargets =
                dualSeatRoom
                |> withHome (fun layer ->
                    { layer with
                        TargetPositions =
                            (Map.toList layer.TargetPositions @ extraTargets) |> Map.ofList
                        Terrain =
                            (TerrainGrid.toList layer.Terrain
                             @ [ for x in 12..30 -> { X = x; Y = 10 }, Plain ])
                            |> TerrainGrid.ofList
                    })

            test "a distant Build flows to the generalist; the Anchor upgrades in place" {
                // What holds the Anchor here is the body gate, not the
                // distance: the case below takes the distance away.
                let snapshot =
                    { dualSeatColony with
                        ConstructionSites = [ { Id = "site-1"; Left = siteOwes } ]
                        Creeps = [ anchor "a1" 50 0; worker "g1" 50 0 ]
                        Spatial =
                            corridorEast [ "site-1", { X = 31; Y = 10 } ]
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 }; "g1", { X = 29; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Build "site-1")))
                    "the mobile body takes the distant site"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the slow heavy body stays where it is valuable"
            }

            test "a site one step off the rock is still not the Anchor's" {
                // The site is one step away, so nothing in the answer can
                // be the distance. A container site on the body's own Post
                // is the exception, and has its own cases.
                let snapshot =
                    { dualSeatColony with
                        ConstructionSites = [ { Id = "site-1"; Left = siteOwes } ]
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            corridorEast [ "site-1", { X = 12; Y = 10 } ]
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the site is a step away and the Anchor still spends into the controller"
            }

            test "a distant Refill flows to the generalist; the empty Anchor harvests" {
                let snapshot =
                    { dualSeatColony with
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ anchor "a1" 0 50; worker "g1" 50 0 ]
                        Spatial =
                            corridorEast [ "spawn-1", { X = 31; Y = 10 } ]
                            |> withCreepsAt [ "a1", { X = 11; Y = 10 }; "g1", { X = 30; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "the loaded mobile body delivers"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the empty Anchor works its Seat instead"
            }

            test "the disaster fallback still spawns bare worker units beside a Dual Seat" {
                let snapshot = { dualSeatColony with Creeps = [] }
                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | (_, body, creepName) :: _ ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "time-to-first-creep outranks specialisation"

                    Expect.stringStarts creepName "worker-" "the fallback casts the worker row"
                | [] -> failtest "expected the fallback to spawn"
            }
        ]

[<Tests>]
let heavyPinTests =
    testList
        "heavy pin"
        [
            test "bumped one tile off the Post, a heavy body keeps its drained source" {
                // A hauler taking the container's load swaps the Anchor
                // onto the Seat beside it. The tile is still inside the
                // source's digging range: it digs the tick the energy lands.
                let colony = pinnedColony 50 (anchor "a1" 0 50) { X = 11; Y = 11 }
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "one tile off the container is still in position to dig"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "and the Post's capacity is held through the window"
            }

            test "the same tile releases a light body: the exemption reads the body" {
                // The pairwise half: a light body beside a dry rock has
                // somewhere else worth being.
                let colony = pinnedColony 50 (worker "w1" 0 50) { X = 11; Y = 11 }
                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(0, 50))
                    ))
                    "an arrival of now covers no wait at all, whatever tile it is on"

                Expect.equal (Map.tryFind "w1" assignments) None "and the Seat is free again"
            }

            test "a heavy body two tiles out is no longer in position, and is released" {
                // The line is the engine's harvest range, not a distance
                // from the Post: a body that must step before it can dig
                // has a walk, and four ticks of walk cover no part of fifty.
                let colony = pinnedColony 50 (anchor "a1" 0 50) { X = 12; Y = 10 }
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(4, 50))
                    ))
                    "out of digging range the ordinary arrival gate judges it"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneInTime))
                    "and nothing else in the pool is a heavy body's work"

                // It takes the one step every idle body takes: (12,10) is
                // inside the Post container's range-1 ring, where the hauler
                // drawing it has to stand. Off the ring, not toward the
                // controller.
                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Right) ]
                    "so it holds its ground, stepping only off the store's ring"
            }

            test "a distant heavy body is dispatched when its walk covers the wait" {
                // An Anchor pays four ticks a plain step, so twenty-four
                // tiles of lane are a walk of ninety-six, and a rock twenty
                // ticks from restock has refilled long before it arrives.
                let colony = pinnedColony 20 (anchor "a1" 0 50) { X = 35; Y = 10 }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "ninety-six ticks of walking cover twenty ticks of waiting"

                Expect.isNonEmpty
                    (moveIntentsFor "a1" intents)
                    "and the window is spent walking, not standing"
            }

            test "what refuses the same walk onto a held Post is the cap, not the restock" {
                // The same walk with one garrison added: what refuses it is
                // the Post count, counted against a holder whose stay
                // overlaps this body's arrival, so the Verdict names
                // crowding and never earliness.
                let colony =
                    pinnedCrowd
                        50
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                        ]

                let remembered = Map.ofList [ "g1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneFree))
                    "the source's one Post is spoken for, which is a capacity and not a wait"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "g1" ]
                    "and the garrison standing on it keeps it"

                Expect.isEmpty
                    (moveIntentsFor "a1" intents)
                    "so nothing walks the width of the room for a tile that is taken"
            }

            test "the drained rock is not taken off a heavy body half way there" {
                // The live case (2026-09-08): an Anchor ninety-odd ticks
                // from an outpost Post had its rock dug out mid-walk, was
                // released `too-early: walk 90, wait 50`, and next tick
                // matched a home source another Anchor stood on. The walk
                // covers the wait by nearly two to one.
                let colony = pinnedColony 50 (anchor "a1" 0 50) { X = 35; Y = 10 }
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "a walk of ninety-six against a window of fifty is not earliness"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "and it keeps the Post it is walking to"
            }

            test "a distant light body is dispatched exactly as ADR 0025 has it" {
                // The pairwise rival: one dispatch rule for both bodies, and
                // a worker crosses the lane at a tick a tile.
                let colony = pinnedColony 20 (worker "w1" 0 50) { X = 35; Y = 10 }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "twenty-four tiles of walking cover twenty ticks of waiting"

                Expect.isNonEmpty
                    (moveIntentsFor "w1" intents)
                    "and the window is spent walking, not standing"
            }

            test "the bumped body is full, and that is the state the report was filed on" {
                // A garrison digs twelve a tick into a fifty store, so a
                // body on its Post is full nearly every tick, and the hauler
                // drawing the container bumps a full body off it.
                let colony = pinnedColony 50 (anchor "a1" 50 0) { X = 11; Y = 11 }
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "a full store off the container is a walk, not a reason to take the Post away"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "and the Post's capacity is held through the window"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Top) ]
                    "the one step it takes is back onto the container"

                Expect.isEmpty
                    (digIntentsFor "a1" intents)
                    "and it digs nothing off the container, drained or not"
            }

            test "a full heavy body off its Post walks back to it, never to the controller" {
                // A Work-heavy body never empties, so if a full store also
                // ended its Harvest it would hold no Task and stand beside
                // a stocked rock for the rest of its life.
                let colony = pinnedColony 0 (anchor "a1" 50 0) { X = 11; Y = 11 }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the far controller is not its work; the Post one tile away is"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Top) ]
                    "so it steps onto the Post instead of standing still"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | UpgradeController(creep, _) -> creep = "a1"
                         | _ -> false))
                    "and spends nothing into a controller a room away"
            }

            test "arrived on the Post, the full body stays and digs" {
                // The tick the step above lands, driven from the state it
                // produces.
                let colony = pinnedColony 0 (anchor "a1" 50 0) { X = 11; Y = 10 }
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "on the container a full store keeps Harvest, as it always did"

                Expect.isEmpty (moveIntentsFor "a1" intents) "the walk is over"

                Expect.contains
                    intents
                    (HarvestSource("a1", "src-a"))
                    "and this is the tile the dig was waiting for"
            }

            test "the walk home is refused onto a Post another garrison is standing on" {
                // The Post cap is counted at arrival, so a walk long enough
                // to outlast the incumbent reads every garrisoned Post as
                // free; live at 204,966 an Anchor crossed a border on that
                // reading and stood beside one with hundreds of ticks left.
                // The offer is the Post standing empty this tick.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 50 0, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                        ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn colony

                Expect.equal (Map.tryFind "a1" assignments) None "the manned Post is not its work"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "and it is the standing room that says so, not a restock or a cap read at arrival"

                Expect.isEmpty
                    (moveIntentsFor "a1" intents)
                    "so nothing walks it the width of the room for a tile it cannot have"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "g1" ]
                    "the garrison keeps its own rock throughout"
            }

            test "a light body on the same tile leaves the Post vacant, and the walk is offered" {
                // A light body standing on a Post is squatting it, not
                // holding it.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 50 0, { X = 35; Y = 10 }
                            worker "w1" 0 50, { X = 11; Y = 10 }
                        ]

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "a Post no garrison stands on is still the full body's walk home"

                Expect.isNonEmpty (moveIntentsFor "a1" intents) "and it sets out for it"
            }

            test "with no Harvest in the pool, a heavy body outside the Work Area has nothing" {
                // With the source out of the pool, the Upgrade gate is the
                // one comparison left.
                let colony =
                    { pinnedColony 0 (anchor "a1" 50 0) { X = 11; Y = 11 } with
                        Sources = []
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn colony

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    None
                    "the far controller is not its work"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "and it is its body that says so, not a restock or a distance"

                // The one step it takes is off the Seat and onto the
                // corridor tile beside it: a body with no Task parks off
                // idle ground, one tile and never a commute.
                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopRight) ]
                    "it steps off the Seat, and nothing walks it the width of the room"

                // (12,10) is inside the Post container's range-1 ring,
                // which the mover vacates too.
                Expect.equal
                    (moveIntentsFor
                        "a1"
                        (decide
                            { colony with
                                Spatial =
                                    colony.Spatial |> withCreepsAt [ "a1", { X = 12; Y = 10 } ]
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ MoveCreep("a1", Right) ]
                    "and there it steps once more, clear of the container's ring, still not a commute"

                // (13,10) is off every Seat and the container's ring, so it
                // stops: the assertion that tells a one-tile step from a
                // commute.
                Expect.isEmpty
                    (moveIntentsFor
                        "a1"
                        (decide
                            { colony with
                                Spatial =
                                    colony.Spatial |> withCreepsAt [ "a1", { X = 13; Y = 10 } ]
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "and standing clear of both, it stays"
            }

            test "the same body inside the Upgrade Work Area still upgrades in place" {
                // A heavy body standing where it can already spend upgrades
                // from the tile it is on: same colony, same body, one tile
                // apart.
                let colony =
                    { pinnedColony 0 (anchor "a1" 50 0) { X = 38; Y = 10 } with
                        Sources = []
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "two tiles from the controller it is already in place"

                Expect.contains
                    intents
                    (UpgradeController("a1", "ctrl-1"))
                    "and it spends its load without taking a step"
            }

            test "a light body still walks the whole corridor to Upgrade" {
                // The pairwise rival: the generalist's full store really
                // does end its dig, and it walks the corridor.
                let colony = pinnedColony 0 (worker "w1" 50 0) { X = 11; Y = 11 }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a mobile body's Upgrade is unchanged at any distance"

                Expect.isNonEmpty (moveIntentsFor "w1" intents) "and it sets out"
            }
        ]
