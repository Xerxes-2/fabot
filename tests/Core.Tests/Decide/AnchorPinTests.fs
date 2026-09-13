/// The Anchor and the heavy pin that ties it to its own rock (ADR 0048).
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
                // The W12S28 bug (ADR 0020): controller far from every Seat,
                // a built container on the Seat at (9,10) — the source's one
                // Post — and the Anchor standing on the plain Seat (10,11).
                // Harvesting there would fill its single Carry in four ticks
                // and hand it to Upgrade, five tiles away; instead it holds
                // its dig and walks.
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
                // The deployment path ADR 0020 names, with ADR 0016's
                // accepted detour closed (ADR 0048). The controller four
                // tiles away is no longer a candidate: a Work-heavy body
                // takes Upgrade only where it can already act on it. ADR
                // 0016 called this one commute; it was one *per release*,
                // and the room paid the walk out and the walk home every
                // time a hauler bumped the Anchor or its source ran dry.
                // What is left is the walk that was always the point — a
                // full store off the Post catches no overflow, but the
                // body is not digging there, it is walking, so Harvest
                // holds it and travel cost puts it back on the tile that
                // does catch it. A heavy body never empties (ADR 0016, ADR
                // 0046, ADR 0048): full is its ordinary condition, and it
                // is the Post and not the store that ends the walk.
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

                // The tick after the walk, on the Post it was walking to —
                // the state the step above produces, and the one the empty
                // window's reprieve is written for (ADR 0024).
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
                // ADR 0021: at RCL4 the bank caps at 1,300 but the Anchor
                // row prices at 700 (6W1C1M); waiting for a full bank would
                // hold every Anchor replacement past RCL3 for nothing.
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
                // container on the other Seat is a second Post, so the
                // remaining gap is cast from the anchor row, not generalist.
                //
                // The generalist beside it is what keeps the supply floor
                // disarmed (ADR 0050): an Anchor holds a Carry and can
                // still put nothing into an extension, so a fleet of
                // Anchors alone is a colony hiring a carrier before every
                // row and this case would read that row instead of the
                // Anchor's.
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
                // The second body is the supply floor's premise and not
                // the case's: a fleet of Anchors alone can refill no
                // extension, and the floor would answer before the Anchor
                // row (ADR 0050).
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
                            Map.ofList
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
                // One Post and four income workers make a target of five
                // (ADR 0012, the worker row rounded up by ADR 0037: the
                // Post's 6,000 of lifetime income — four a tick, which is
                // what the `2W/1C/1M` Anchor this 300 bank casts digs out
                // of it, #208 — less the Anchor's 300 of amortization over
                // 1 × 1500 is 3.8); four living leave one gap — the second
                // idle spawn must stay quiet even with energy banked for
                // it.
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
                            (Map.toList layer.Terrain
                             @ [ for x in 12..30 -> { X = x; Y = 10 }, Plain ])
                            |> Map.ofList
                    })

            test "a distant Build flows to the generalist; the Anchor upgrades in place" {
                // What holds the Anchor here is the body gate and no longer
                // the distance (#234): a site outranks the Upgrade beside it
                // by a rung now, so a heavy body offered one would walk to it
                // at any price. The case below is the same claim with the
                // distance taken away.
                let snapshot =
                    { dualSeatColony with
                        ConstructionSites = [ { Id = "site-1" } ]
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
                // #234 closed the Build gate over the whole tier, and this
                // is the case that says why it had to. Travel cost was what
                // pinned a heavy body on its rock while a home site was
                // ordinary surplus work; a rung above the Upgrade beside it,
                // no distance decides between the two any more, and the
                // Anchor would walk off its Post for fifty carried energy at
                // four to seven ticks a step. What refuses it is the
                // prohibition ADR 0020 and ADR 0048 already wrote for the
                // feeding-tier site, now asked of every one: a heavy body's
                // work is its Post and never a delivery, however short the
                // delivery is. The site is one step away here, so nothing in
                // the answer can be the distance.
                //
                // #205's exception is untouched and is the pair: a container
                // site on the body's **own** Post is built where it stands,
                // and its own cases are below.
                let snapshot =
                    { dualSeatColony with
                        ConstructionSites = [ { Id = "site-1" } ]
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
                // ADR 0048's widening of ADR 0025's exemption. A hauler
                // taking the container's load swaps the Anchor onto the
                // Seat beside it; on ADR 0024's container-only condition
                // that one step ended the garrison, released the Anchor
                // TooEarly, and freed the Post for whatever was released
                // elsewhere in the colony. The tile is still inside the
                // source's digging range, which is the whole of what the
                // window asks of it: it digs the tick the energy lands.
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
                // The pairwise half of the rule (ADR 0016's shape, ADR
                // 0048's clause): what the window forgives is a body that
                // has nowhere else worth being. A worker beside a dry rock
                // is released exactly as ADR 0013 released it and goes and
                // does something else with the fifty ticks.
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
                // Where ADR 0048 draws the line, and it is the engine's own
                // harvest range and not a distance from the Post: a body
                // that would have to take a step before it could dig has a
                // walk, and a walk is what ADR 0025 judges — on the same
                // arithmetic and the same numbers a light body is judged
                // on since #258. Four ticks of walk cover no part of fifty
                // ticks of wait, so it waits the window out where it
                // stands.
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

                // It holds its *place in the queue* and takes no walk — but
                // it does take the one step #268 asks of every idle body:
                // (12,10) is the corridor mouth, inside the Post container's
                // range-1 ring, so a body waiting a restock out there is
                // standing where the hauler drawing that container has to
                // stand. The step is off the ring and not toward the
                // controller, which is the distinction this case is about.
                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Right) ]
                    "so it holds its ground, stepping only off the store's ring"
            }

            test "a distant heavy body is dispatched when its walk covers the wait" {
                // #258 retires ADR 0048's heavy arm and #193's refusal
                // with it. What that arm cured was an Anchor walking half
                // a room onto a Post another Anchor was standing on, and
                // that is a **capacity** question — closed by the Post
                // count since ADR 0024 and ADR 0051, and closed a second
                // time for a body still walking by the pair below. What it
                // cost was the dispatch rule itself: an Anchor pays four
                // ticks a plain step, so twenty-four tiles of lane are a
                // walk of ninety-six, and a rock twenty ticks from its
                // restock has long since refilled by the time this body
                // arrives. There is nothing to wait for.
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
                // The other half of the case above, and the one #193's
                // test used to carry: with the heavy arm gone, the rule
                // that keeps an Anchor off a Post another Anchor is
                // standing on is the **Post count** (ADR 0024 as ADR 0051
                // sharpened it), counted against a holder whose stay
                // overlaps this body's arrival (ADR 0026). The same drained
                // rock, the same ninety-six ticks of lane, one garrison
                // added: the pair is offered and the cap refuses it, so
                // the Verdict names crowding and never earliness.
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
                // The live case (user, 2026-09-08), which is the same rule
                // read as a release rather than as a dispatch. An Anchor
                // ninety-odd ticks from an outpost Post had its rock dug
                // out from under it in mid-walk; the heavy arm released it
                // `too-early: walk 90, wait 50`, it went `none-in-time`,
                // and the next tick it matched a home source another
                // Anchor was standing on and walked the border back. The
                // walk covers the wait by nearly two to one, so there is
                // no release to start that chain.
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
                // The pairwise rival of the case above, one body apart:
                // one dispatch rule for both bodies since #258, and how
                // many ticks a tile costs each of them is already in the
                // walk the rule reads. A worker crosses the same lane at a
                // tick a tile and still spends the window on the road.
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
                // The pairwise store half of the case above, and the one
                // the colony actually reaches: a garrison digs twelve a
                // tick into a fifty store and the overflow falls into the
                // container, so a body standing on its Post is full nearly
                // every tick of its life (ADR 0012, ADR 0024) — and a
                // hauler drawing that container bumps a *full* body onto
                // the Seat beside it. The empty-window reprieve has to
                // reach that body or it reaches nothing the report
                // describes.
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
                // ADR 0016's last accepted detour, closed (ADR 0048). It
                // was written as one commute — a full Anchor spends its
                // load at the controller once and converges — but every
                // release put the same body back at the same gate, so the
                // colony paid the walk out and the walk home every time a
                // hauler bumped it or its source ran dry. Its Carry is
                // fifty energy against four Work: there is nothing at the
                // far end worth the trip.
                //
                // And closing that walk leaves the walk that was always
                // the point. A Work-heavy body never empties — nothing in
                // the pipeline spends its store any more — so if a full
                // store also ended its Harvest it would hold no Task, take
                // no step, and stand there for the rest of its life beside
                // a stocked rock: #193's own symptom, made by the cure.
                // ADR 0048's Consequence says it "stands where it is until
                // it can dig again", and this is the walk that gets it
                // there.
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
                // produces rather than from a hand-built creep: ADR 0024's
                // own condition takes over, the overflow falls into the
                // container underfoot, and the walk home is over.
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
                // #258's second half. ADR 0048 offers a full Work-heavy
                // body the walk wherever the source has a Post, on the
                // argument that a Post is a tile the arriving body has
                // something to do on — and the Post cap that would refuse
                // the pair is counted at arrival (ADR 0026), so a walk
                // long enough to outlast the incumbent reads every
                // garrisoned Post in the colony as free. Live at 204,966
                // an Anchor that had just lost its own rock crossed a
                // border home on exactly that reading and stood beside an
                // Anchor with hundreds of ticks left. The offer is now the
                // Post standing empty *this* tick.
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
                // The pairwise half, one body apart on the same tile: what
                // holds a Post is a garrison, and ADR 0051 keeps every
                // light body off it — one standing there is squatting the
                // Post, not working it — so the gate reads the body
                // exactly as `hasSpareRate` reads it (#235). The Anchor
                // still has somewhere to go.
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
                // The Upgrade gate on its own (ADR 0048's third clause),
                // with the source taken out of the pool so what is left is
                // the one comparison: a Work-heavy body one room's width
                // from the controller is not a candidate for Upgrade at
                // all, and there is nothing else its body can take.
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

                // The one step it does take is off the Seat it has no work
                // on and onto the corridor tile beside it (#241): a body
                // with no Task parks off the idle ground, and this body's
                // tile is a Seat of the source the pool no longer carries.
                // What the gate refuses is the *walk* — the width of the
                // room, east down the corridor — so the step is one tile
                // and never a commute.
                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopRight) ]
                    "it steps off the Seat, and nothing walks it the width of the room"

                // One tile further east than #241 left it: (12,10) is inside
                // the Post container's range-1 ring, which is store ground
                // the mover now vacates too (#268), so the tile it settles
                // on is the first one past that ring.
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

                // And there it stops: (13,10) is off every Seat, off the
                // container's ring and still a room from the controller, so
                // the widened set costs the body one tile and not a walk.
                // This is the assertion that tells a one-tile step from a
                // commute — without it a ground that receded a tile a tick
                // would pass the two above.
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
                // The half the gate must not take away (ADR 0046, ADR
                // 0020): a heavy body standing where it can already spend
                // — a Dual Seat Anchor, an upgrader beside the buffer —
                // upgrades from the tile it is on. What ADR 0048 refuses
                // is the walk, so the gate is Work-Area membership and not
                // a body-shaped ban on the Task. Same colony, same body,
                // same empty pool as the case above: one tile apart.
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
                // The pairwise rival again: the gate reads part arithmetic
                // and nothing else (ADR 0006). On the very tile where the
                // full Anchor takes the one step back onto its Post, the
                // generalist — whose full store really does end its dig
                // (ADR 0024) — takes the far Upgrade and walks the
                // corridor for it.
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
