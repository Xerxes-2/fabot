/// The Anchor: the work-heavy body pinned to its own rock (ADR 0048) — the Post
/// it harvests from and raises (ADR 0020, ADR 0051, ADR 0053), the Work ceiling
/// its source saturates at (ADR 0021), the standing body's own Refill, and the
/// lead that hands a Post on before its holder expires (ADR 0026).
module Fabot.Core.Tests.Decide.AnchorTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 10; Y = 11 } ]
                                })
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot Map.empty Set.empty None

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
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "a1", { X = 10; Y = 11 } ]
                        })

                let full =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = room
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide full Map.empty Set.empty None

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
                        Spatial =
                            full.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 9; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide arrived remembered Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "the Anchor quota lives inside the target, never on top of it"
            }

            test "an empty Anchor on its Dual Seat is assigned Harvest without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "g1", { X = 29; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "g1", { X = 30; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the loaded mobile body delivers"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the empty Anchor works its Seat instead"
            }

            test "the disaster fallback still spawns bare worker units beside a Dual Seat" {
                let snapshot = { dualSeatColony with Creeps = [] }
                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

/// The heavy-pin fixture (ADR 0048): the source embedded in wall at
/// (10,10) with its eight neighbours open, the built container "cont-1"
/// standing on the Seat (11,10) — the source's one Post — and a plain
/// corridor running east from that Post to the controller at (40,10),
/// whose Upgrade Work Area is a room's width from the source. No Dual
/// Seat, so nothing a heavy body does here it can do in two places at
/// once, and the controller is the only rival Harvest ever has.
let pinnedRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "cont-1", { X = 11; Y = 10 }
              "ctrl-1", { X = 40; Y = 10 }
          ]
          (openSeats { X = 10; Y = 10 } @ [ for x in 11..39 -> { X = x; Y = 10 }, Plain ]) with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "cont-1", Structure BuiltKind.Container
                    "ctrl-1", Controller
                ]
    }

/// The heavy-pin colony: the creeps of the test's choosing standing where
/// the test puts them, the source the given number of ticks from its
/// restock, and no spawn to cast anything that would crowd the pool.
let pinnedCrowd ticks (placed: (CreepInfo * Pos) list) =
    { bareRespawn with
        Spawns = []
        Refillables = []
        Sources = [ drained "src-a" ticks ]
        Controller = Some(controllerAt 2)
        Creeps = placed |> List.map fst
        Spatial =
            pinnedRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                })
    }

/// The same colony holding one body: the shape most of these cases take.
let pinnedColony ticks (creep: CreepInfo) pos = pinnedCrowd ticks [ creep, pos ]

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
                    decide colony remembered Set.empty None

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
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 50)))
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
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(4, 50)))
                    "out of digging range the ordinary arrival gate judges it"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneInTime))
                    "and nothing else in the pool is a heavy body's work"

                Expect.isEmpty (moveIntentsFor "a1" intents) "so it holds its ground"
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
                    decide colony Map.empty Set.empty None

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
                    decide colony remembered Set.empty None

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
                    decide colony remembered Set.empty None

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
                    decide colony Map.empty Set.empty None

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
                    decide colony remembered Set.empty None

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
                    decide colony Map.empty Set.empty None

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
                    decide colony remembered Set.empty None

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
                    decide colony Map.empty Set.empty None

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
                    decide colony Map.empty Set.empty None

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
                    decide colony Map.empty Set.empty None

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
                // with no Task parks off the [[working ground]], and this
                // body's tile is a Seat of the source the pool no longer
                // carries. What the gate refuses is the *walk* — the width
                // of the room, east down the corridor — and the corridor
                // tile it steps to is where it stays.
                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopRight) ]
                    "it steps off the Seat, and nothing walks it the width of the room"

                Expect.isEmpty
                    (moveIntentsFor
                        "a1"
                        (decide
                            { colony with
                                Spatial =
                                    colony.Spatial
                                    |> withHome (fun layer ->
                                        { layer with
                                            CreepPositions =
                                                Map.ofList [ "a1", { X = 12; Y = 10 } ]
                                        })
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "and standing there, off every Seat and still a room from the controller, it stays"
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
                    decide colony Map.empty Set.empty None

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
                    decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a mobile body's Upgrade is unchanged at any distance"

                Expect.isNonEmpty (moveIntentsFor "w1" intents) "and it sets out"
            }
        ]

/// The heavy-pin room with a second container: "cont-2" on the Seat (9,10)
/// beside "cont-1" on (11,10), so the rock carries **two** Posts and the cap
/// admits two garrisons. One Post cannot tell a count of holders from a count
/// of tiles apart — one body standing on its own Post satisfies both readings —
/// so the union the Post cap takes of the two (#269) is only visible on a rock
/// with a Post to spare.
let twoPostRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "cont-1", { X = 11; Y = 10 }
              "cont-2", { X = 9; Y = 10 }
              "ctrl-1", { X = 40; Y = 10 }
          ]
          (openSeats { X = 10; Y = 10 } @ [ for x in 11..39 -> { X = x; Y = 10 }, Plain ]) with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "cont-1", Structure BuiltKind.Container
                    "cont-2", Structure BuiltKind.Container
                    "ctrl-1", Controller
                ]
    }

let twoPostCrowd (placed: (CreepInfo * Pos) list) =
    { bareRespawn with
        Spawns = []
        Refillables = []
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt 2)
        Creeps = placed |> List.map fst
        Spatial =
            twoPostRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                })
    }

/// The Dual Seat room with a lane out of it. `dualSeatRoom`'s source sits at
/// (10,10) with two Seats, (11,10) inside the controller's Upgrade Work Area
/// and so a bare [[dual seat]] — the colony's one Post with no container under
/// it. The lane is laid along y = 9 from x = 12 to x = 31 and deliberately not
/// along y = 10: the controller stands at (13,10), and a row through it would
/// either wall the lane or, laid one tile lower, add a second Seat inside the
/// controller's range and give the rock a second Post.
let dualSeatLaneColony ticks (placed: (CreepInfo * Pos) list) =
    { dualSeatColony with
        Spawns = []
        Refillables = []
        Sources = [ drained "src-a" ticks ]
        Creeps = placed |> List.map fst
        Spatial =
            dualSeatRoom
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 12..31 -> { X = x; Y = 9 } ])
                        ||> List.fold (fun acc tile -> Map.add tile Plain acc)
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                })
    }

[<Tests>]
let postGarrisonTests =
    testList
        "a manned Post is never vacant"
        [
            test "a heavy body standing on the one Post holds it while holding nothing" {
                // #269, and the older half of it — the mechanism predates
                // #258's widening. `Capacity.Garrisons` counted the Post's
                // *holders*, so a rock whose garrison happened to hold no
                // Task this tick read as an empty Post to every heavy body
                // in the colony, and the Matcher walks its candidates in
                // view order: the body ninety-six ticks of lane away is
                // offered the Post first and takes it, and the body already
                // standing on it is told `none-free` and moves off. The cap
                // reads the tiles now, so the census answers where a body
                // *is* rather than what it was assigned last tick.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                        ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide colony Map.empty Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneFree))
                    "the Post is manned, and a body standing on one is what mans it"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "g1" ]
                    "so the rock goes to the body already on its Post"

                Expect.isEmpty
                    (moveIntentsFor "a1" intents)
                    "and nothing crosses the room for a tile that is taken"
            }

            test "one tile off the Post it holds nothing, and the walk is offered" {
                // The pairwise rival, one tile apart: what the census reads
                // is the Post itself and not the ground around it (ADR
                // 0024). The same body on (11,11) is beside the Post rather
                // than on it, the rock reads vacant, and the distant Anchor
                // is dispatched exactly as it was before #269 — which is
                // also what keeps the bumped-garrison window of ADR 0048
                // from locking the rock against its own successor.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 11 }
                        ]

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide colony Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "a Post with nobody standing on it is a Post the cap admits"

                Expect.isNonEmpty (moveIntentsFor "a1" intents) "and the body offered it sets out"
            }

            test "the garrison of a bare Dual Seat holds its Post through an Upgrade" {
                // The live shape #269 was filed on, and the window ADR 0025's
                // gate names and declines to cure. A drained rock releases
                // its Dual Seat Anchor `too-early` — the empty-window
                // reprieve subtracts a bare Dual Seat (ADR 0048) — and the
                // controller is two tiles away, so the released body spends
                // the window upgrading from the very tile it will dig from
                // in sixty ticks. Counting Harvest's holders alone, the Post
                // read vacant for those sixty ticks and a second Anchor
                // twenty tiles down the lane was dispatched onto it.
                let colony =
                    dualSeatLaneColony
                        60
                        [
                            anchor "a1" 50 10, { X = 11; Y = 10 }
                            anchor "a2" 0 50, { X = 31; Y = 9 }
                        ]

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 60)))
                    "the bare Dual Seat carries no empty-window reprieve"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "so it spends the window on the controller two tiles away"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a2", IdleReason.NoneFree))
                    "and the Post it is standing on is not vacant for holding something else"

                Expect.isEmpty (harvesters assignments "src-a") "the drained rock waits"

                Expect.isEmpty
                    (moveIntentsFor "a2" intents)
                    "and nothing walks twenty tiles onto an occupied tile"
            }

            test "an expiring garrison still hands its Post on" {
                // The half of ADR 0026 the widened census must not eat. The
                // discount is for an incumbent that will be **dead** when
                // the candidate arrives, and the tile census takes it at
                // arrival like every other holder: a garrison with ten
                // ticks left against a walk of ninety-six is not standing
                // there when the successor lands, so the Post reads vacant
                // and the succession the row cast for goes through. Pairwise
                // against the first case above, one field apart.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50 |> withLife 10, { X = 11; Y = 10 }
                        ]

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a" |> List.sort)
                    [ "a1"; "g1" ]
                    "two Anchors against one Post for the lead's duration is the succession"
            }

            test "a rock with a Post to spare admits a second garrison" {
                // The union, and the reason the widened census is not a sum
                // (#269). On a standing container the garrison holds the
                // Harvest it is standing on — ADR 0024's overflow reprieve
                // keeps it applicable through a full store — so the holder
                // list and the tile census name the same body. Added, that
                // body would spend both of this rock's Posts and the second
                // would read full while it stands empty; unioned, it counts
                // once and the second Post hires.
                let colony =
                    twoPostCrowd
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                        ]

                let remembered = Map.ofList [ "g1", taskId (Harvest "src-a") ]

                let { Assignments = assignments } = decide colony remembered Set.empty None

                Expect.equal
                    (harvesters assignments "src-a" |> List.sort)
                    [ "a1"; "g1" ]
                    "one body on one of two Posts is one garrison, not two"
            }

            test "both Posts manned, the third heavy body is refused" {
                // The pairwise rival of the case above, one body apart: the
                // widened census still counts, and a rock whose every Post
                // carries a standing heavy body is full whatever those
                // bodies hold.
                let colony =
                    twoPostCrowd
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                            anchor "g2" 0 50, { X = 9; Y = 10 }
                        ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony Map.empty Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneFree))
                    "two Posts, two garrisons standing on them, and no third slot"

                Expect.equal
                    (harvesters assignments "src-a" |> List.sort)
                    [ "g1"; "g2" ]
                    "and the rock is worked by the bodies already on it"
            }
        ]

[<Tests>]
let anchorDigTests =
    testList
        "a Post is worth what its garrison digs"
        [
            test "at a 300 bank the rock's rate is a ceiling nothing reaches" {
                // #208's live defect, pinned at the fixture that always
                // held it. `heldRateOf` prices this owned room's rocks at
                // ten a tick, and the Anchor row's cast at a 300 bank is
                // `2W/1C/1M`, which digs four: a Post yields what the body
                // garrisoning it takes out of the rock, and the rate is
                // only the ceiling on that. Read at the rate the colony
                // counted 20 a tick, hired 19 workers and 3 haulers, and
                // stood 12 of them idle beside a spawn already full.
                //
                // Two readings of one number, and both move together
                // (ADR 0042): the income base is 8 a tick, so the worker
                // row is ceil((8 × 1500 − 900) / 1500) = 8, and the hauler
                // row ships 8 rather than 20 — ceil((24 + 24) × 4 / 200) =
                // one body where the rate hired three.
                Expect.equal
                    (quotaOf incomeColony)
                    1
                    "the two Posts ship what two 2W bodies dig, not what the room would pay"

                Expect.isEmpty
                    (spawnIntents
                        (decide
                            { incomeColony with
                                Creeps = incomeFleet
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "and eight workers, not eighteen, are the whole of the row"

                match
                    spawnIntents
                        (decide
                            { incomeColony with
                                Creeps = List.truncate (List.length incomeFleet - 1) incomeFleet
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents
                with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "and it is tight: one body short and the colony casts a worker"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "under that ceiling the room's own rate moves nothing" {
                // The cap, pinned as a cap: one input moves — who holds
                // the spawn room — and at a bank whose Anchor digs four
                // the answer does not, because four is under the neutral
                // five as well as under the held ten. A rule that took the
                // room's rate, or the smaller of the two only sometimes,
                // would part these two colonies here.
                let neutralised =
                    { incomeColony with
                        Creeps = incomeFleet
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.equal
                    (quotaOf neutralised)
                    (quotaOf incomeColony)
                    "the same haul: what the garrison digs is what either room's rock ships"

                Expect.isEmpty
                    (spawnIntents (decide neutralised Map.empty Set.empty None).Intents)
                    "and the same fleet is the whole target, held or not"
            }

            test "at an 1800 bank the cast outruns the rock and the rate is the answer again" {
                // The other half of the pair, and the reason the rule is
                // `min` and not a discount: the same geometry at a bank
                // whose Anchor row casts six Work digs twelve a tick, over
                // the ten an owned rock pays and over the five a neutral
                // one does — so the ceiling binds, the rate is the answer,
                // and neutralising the room moves the target by a body
                // where at 300 it moved nothing.
                //
                // Sized to the neutral target — 2 Anchors of three Work
                // each, 1 hauler, and ceil((10 × 1500 − 1,400 − 1,800) /
                // (9 × 1500)) = 1 worker — so the neutral colony has no
                // gap and the owned one does.
                let neutralised =
                    { richestIncomeColony with
                        Creeps = richestIncomeFleet 1
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.isEmpty
                    (spawnIntents (decide neutralised Map.empty Set.empty None).Intents)
                    "the premise: at five a tick these four are the whole target"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { neutralised with
                                RoomControl = homeControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned, the same rocks are worth ten each and the fleet is a body short"
            }
        ]

/// The W12S28 colony with its own two source containers taken away: the
/// same two rocks, the same eight Seats apiece, and no Post on either. The
/// only Post left in a projection is whatever an outpost carries — which
/// is the one arrangement where a neutral rate is the *richest* rate the
/// Anchor row hires for, and so the only one where the row's ceiling can
/// be read off a cast body at all.
let private withoutHomePosts (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-a" |> Map.remove "can-b"
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions =
                        layer.TargetPositions |> Map.remove "can-a" |> Map.remove "can-b"
                })
    }

/// The W12S28 colony at a 1,300 bank with the posted outpost source of
/// `postedOutpostColony` standing beside it: the same rock in the same
/// three-Seat field, its container built, and a fleet of one worker so
/// every Post in the projection is an unfilled Anchor gap. The bank alone
/// would buy twelve Work, so the body the row casts is decided by its
/// ceiling and by nothing else, and 700 of the 1,300 goes on that body —
/// leaving too little for a second, so the tick casts exactly one Anchor
/// whatever the gap.
///
/// Two dials and no others: whether the colony's own room keeps its Posts,
/// and who holds W1N2. Everything the target is built from moves with
/// them, but the *body* reads only the ceiling.
let private anchorCapColony homePosts (control: (string * RoomControlInfo) list) =
    let rock = { X = 40; Y = 40 }

    let colony =
        { incomeColony with
            Bank = bank 1300 1300
            Sources = incomeColony.Sources @ [ source "src-out" ]
            Creeps = [ worker "w1" 0 50 ]
        }
        |> (if homePosts then id else withoutHomePosts)
        |> withOutpost
            "W1N2"
            [
                "src-out", rock, Source
                "can-out", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
            ]
            (threeSeatField rock)

    { colony with
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

/// The one Anchor body the tick casts, for the fixtures whose bank buys
/// exactly one.
let private anchorCastBy colony =
    match anchorCastsBy colony with
    | [ body ] -> body
    | other -> failtest $"expected exactly one Anchor SpawnCreep intent, got %A{other}"

/// The colony the anchor row's **charge** is legible in, which the cast's
/// own fixture is not: the same W12S28 without its two Posts, at a 1,400
/// bank, with three neutral rocks a room away, each with its container
/// standing — three Posts, three Anchors hired, and every one of them
/// under the neutral ceiling. Its fleet is whole but for the workers, so
/// the one thing a spawn Intent can be here is the income base's own
/// answer.
///
/// Why those two numbers and not the 1,300 of the cast's fixture. The
/// amortization is deducted from income before the surplus is divided into
/// worker places, and the division rounds up over a whole body's Work
/// drain across a lifetime (ADR 0037) — 10,500 energy at this bank — so a
/// charge that moves by 350 an Anchor is invisible unless the surplus
/// straddles a boundary. Three Posts move it by 1,050, and 15 energy a
/// tick over the lifetime leaves 21,450 charged at the cast body against
/// 20,400 charged at the held one: three worker places and two. One
/// Post at 1,300 moves it by 350 against a 9,000-energy place and could
/// not move the target at all.
/// `homePosts` keeps the colony's own two Posts in the projection, which
/// is the arrangement ADR 0053 is about and the one the aggregate charge
/// could not tell from any other: five Posts over two rates, charged
/// 2 × 700 + 3 × 400 Post by Post where a quota times one ceiling charges
/// 5 × 700.
let private anchorChargeColony homePosts workers =
    let rocks = [ { X = 10; Y = 40 }; { X = 20; Y = 40 }; { X = 30; Y = 40 } ]

    let outpost =
        rocks
        |> List.mapi (fun i rock ->
            [
                $"src-out{i}", rock, Source
                $"can-out{i}", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
            ])
        |> List.concat

    let colony =
        { incomeColony with
            Bank = bank 1400 1400
            Sources = incomeColony.Sources @ [ for i in 0..2 -> source $"src-out{i}" ]
            Creeps =
                [ for i in 1..3 -> anchor $"a{i}" 0 50 ]
                @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]
        }
        |> (if homePosts then id else withoutHomePosts)
        |> withOutpost "W1N2" outpost (rocks |> List.collect threeSeatField)

    { colony with
        RoomControl = Map.add "W1N2" neutralRoom colony.RoomControl
    }

[<Tests>]
let anchorWorkCapTests =
    testList
        "the Anchor row's Work ceiling"
        [
            test "the same rock caps the Anchor row at six Work reserved and three unreserved" {
                // ADR 0021's rule, ADR 0042's number: the ceiling is a
                // source's saturation plus one spare, and a source under no
                // reservation regenerates 1,500 over 300 ticks instead of
                // 3,000. Five Work saturate the held rock and two the
                // neutral one, so the ceilings are six and three — and the
                // 1,300 bank standing behind both would buy twelve.
                //
                // One rock, one field, one fleet: only who holds W1N2 moves
                // between the two calls.
                Expect.equal
                    (anchorCastBy (anchorCapColony false [ "W1N2", reservedRoom true 4000 ]))
                    sixWork
                    "reserved, the rock gives ten a tick and the row buys the six Work that dig it"

                Expect.equal
                    (anchorCastBy (anchorCapColony false [ "W1N2", neutralRoom ]))
                    threeWork
                    "unreserved it gives five, and three Work drain it as fast as it fills"
            }

            test "a neutral outpost Post does not shrink the ceiling the home room asks for" {
                // The direction the pairing is wrong in, pinned pairwise
                // against the case above: the same neutral W1N2, the same
                // rock, the same field — the colony's own two Posts are the
                // only thing added, and the fleet is one worker, so all
                // three Posts stand empty and the row is three bodies short.
                //
                // Every cast this tick is bought under the **dearest
                // vacancy's** rock (ADR 0053), because a cast is a body and
                // not a posting: travel cost pins the finished body on
                // whichever Post is nearest once it is alive (ADR 0021's own
                // rejection of sizing by the Post), so with several
                // vacancies open the colony cannot steer any of these
                // bodies and buys every one of them for the dearer rock.
                // Under-sizing an Anchor for a held rock loses four energy a
                // tick for the body's whole life; over-sizing one for a
                // neutral rock wastes 300 energy once in 1,500 ticks and
                // still digs everything the rock has.
                //
                // Two spawns and a 1,300 bank buy exactly one of the three:
                // 700 for a home Post's six Work, and the 600 left cannot
                // pay for a second six-Work body — so the second spawn
                // yields the seat (ADR 0050) rather than spending 400 on the
                // neutral Post's `3W/1C/1M`. Which is the whole of why the
                // ceiling is the dearest vacancy's and not each vacancy's
                // own: both of this colony's held Posts are a few tiles from
                // the spawns and the neutral one is a Seam away, so a body
                // bought for the outpost's hole lands on a held rock and
                // digs six where the rock gives ten.
                Expect.equal
                    (anchorCastsBy (anchorCapColony true [ "W1N2", neutralRoom ]))
                    [ sixWork ]
                    "the home room's held rock keeps its own replacement at six Work, and the neutral rock beside it buys nothing"
            }

            test "the colony's own room is capped exactly where it always was" {
                // The regression ADR 0042 promises: "unchanged as a rule and
                // changed as a number", and the colony's own number does not
                // move. Owned, with no outpost in the projection at all —
                // the case every existing Anchor test is written on, read
                // here for the ceiling alone.
                Expect.equal
                    (anchorCastBy
                        { incomeColony with
                            Bank = bank 1300 1300
                            Creeps = [ worker "w1" 0 50 ]
                        })
                    sixWork
                    "two held Posts and a 1,300 bank: the six-Work Anchor of ADR 0021"
            }

            test "a Post the colony cannot price this tick leaves the ceiling where it was" {
                // ADR 0004, entry by entry, and the same separation the
                // source rate keeps: unpriceable is not half. W1N2 carries
                // no control entry here, so nobody knows who holds it —
                // the rock contributes no saturation to the fold rather
                // than the neutral one, and a fold with nothing priceable
                // in it answers the held ceiling, which is the largest the
                // rule gives and the safe direction to be wrong in.
                //
                // Pinned strictly against the neutral case above: seen and
                // held by nobody the same rock casts three Work.
                Expect.equal
                    (anchorCastBy (anchorCapColony false []))
                    sixWork
                    "a rock nobody can price caps nothing, and the row keeps the held ceiling"
            }

            test "the row is charged the body it would cast, not the held one" {
                // The other half of #132's landing note — "the price the row
                // is charged must be the body the row is cast at" — and the
                // half no cast body can show: `workforceTarget` deducts the
                // Anchor row's replacement cost from the income before the
                // surplus is divided into worker places (ADR 0012, ADR
                // 0042), so charging six Work for a row that casts three
                // hires an upgrade mouth fewer than the income really feeds.
                //
                // Read as the income base's cases are read, pairwise across
                // one fleet: three Anchors and nineteen workers is the whole
                // of what this colony's 15 energy a tick pays for, so the
                // tick casts nothing; one worker short of it, the row that
                // is short is the worker row and the tick says so. Charged
                // at the held ceiling the target is 21 instead of 22, and
                // the fleet of 21 below has no gap at all.
                let casts workers =
                    spawnIntents
                        (decide (anchorChargeColony false workers) Map.empty Set.empty None).Intents
                    |> List.map (fun (_, _, name) -> name)

                Expect.isEmpty
                    (casts 19)
                    "three Anchors and nineteen workers: the income base is spent and the tick casts nothing"

                match casts 18 with
                | [ creepName ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "one short of it the worker row is short, which the held charge would not have hired"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the charge is one body a Post and not the quota times one ceiling" {
                // **ADR 0053's other half.** The test above is written on a
                // colony whose Posts agree — three neutral rocks, one
                // ceiling between them — where a quota times that ceiling
                // and a sum over the Posts are the same number. This is the
                // colony they part on: the same three neutral rocks with
                // the colony's own two held Posts kept beside them, five
                // Posts over two rates. Post by Post the row is charged
                // 2 x 700 + 3 x 400 = 2,600; the quota times its richest
                // ceiling charges 5 x 700 = 3,500, and the 900 between them
                // is an upgrade mouth the income really feeds.
                //
                // At a 1,600 bank because the target is an integer: the
                // surplus is divided into worker places of a whole body's
                // Work drain over a lifetime (ADR 0037), which is 12,000
                // energy here, and 900 moves the target only where it
                // straddles one. It does here — 11 against the 10 the
                // aggregate charge answers — and at 1,400, the bank the
                // test above is written at, it does not.
                //
                // Pairwise on the outpost room's reservation alone, which
                // is what makes the reading a pairing and not a number:
                // held, all five Posts saturate at six Work, the two
                // readings are the same sum by construction, and the target
                // is 12. The arms differ by more than the charge — a held
                // rock also pays twice the income — and it is the neutral
                // arm that carries the discrimination.
                let target held =
                    let colony = anchorChargeColony true 3

                    { colony with
                        Bank = bank 1600 1600
                        RoomControl =
                            Map.add
                                "W1N2"
                                (if held then reservedRoom true 5000 else neutralRoom)
                                colony.RoomControl
                    }
                    |> fun colony -> (decide colony Map.empty Set.empty None).Quotas.Target

                Expect.equal
                    (target false)
                    11
                    "three neutral Posts charged at their own three Work, beside two held ones charged at six"

                Expect.equal
                    (target true)
                    12
                    "and where every Post saturates alike the sum over them is the quota times the one ceiling"
            }
        ]

/// A lane with one Post at one end and the spawn at the other: the source
/// in wall at (10,10), its built container on the Seat (11,10) — the only
/// tile a Work-heavy body may dig that source from (ADR 0020) — and the
/// spawn structure standing at (21,10), ten plain steps up the lane. Its
/// one free neighbour is (20,10), so that is where a replacement is born
/// and the walk it is led by is nine steps, not ten. Far enough that a
/// replacement's own body, not just its cast time, prices the lead.
let successionRoom =
    { spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
        Stores = Map.ofList [ "can-src", 0 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 21; Y = 10 }
        })
    |> withTargets
        [
            "src-a", { X = 10; Y = 10 }, Source
            "can-src", { X = 11; Y = 10 }, Structure BuiltKind.Container
            "spawn-1", { X = 21; Y = 10 }, Structure BuiltKind.Spawn
        ]

/// The lane's colony. Its controller is unplaced and every creep below is
/// empty, so the one Task any of them can hold is the lane's Harvest.
let successionColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = successionRoom
    }

/// A succession in the lane: the incumbent Anchor on the Post with the
/// given ticks left to live, its successor nine steps away at (20,10).
let succession incumbent successor life =
    { successionColony with
        Creeps = [ anchor incumbent 0 50 |> withLife life; anchor successor 0 50 ]
        Spatial =
            successionRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ incumbent, { X = 11; Y = 10 }; successor, { X = 20; Y = 10 } ]
                })
    }

/// The same lane at an RCL3 bank, where the Anchor row's body is five
/// Work beside its Carry and Move (ADR 0021) — and where both creeps below
/// are that body, so the lead prices exactly the body it leads, as a real
/// succession does. Ten cost units a plain step, 21 ticks in the spawner.
let rcl3Succession incumbent successor life =
    let rcl3Anchor name =
        creepWith name 0 50 [ Work; Work; Work; Work; Work; Carry; Move ]

    { successionColony with
        Bank = bank 600 600
        Creeps = [ rcl3Anchor incumbent |> withLife life; rcl3Anchor successor ]
        Spatial =
            successionRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ incumbent, { X = 11; Y = 10 }; successor, { X = 20; Y = 10 } ]
                })
    }

/// The creeps a tick released and why — the release fold's own output,
/// read without the Task it dropped.
let releases verdicts =
    verdicts
    |> List.choose (function
        | Verdict.Released(creep, _, reason) -> Some(creep, reason)
        | _ -> None)

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


/// A body of the given row at the live RCL5 bank, full: energy on board
/// and no free capacity, which is the state every delivery Task asks for
/// and the state that ends a Withdraw. The name is the row's, so a failure
/// message says which body it was.
let private castFull pattern =
    let body = bodyFor pattern 1800

    creepWith pattern.Name (50 * (body |> List.filter ((=) Carry) |> List.length)) 0 body

/// The upgrader row's own body at that bank: `11W/1C/11M`, ADR 0046's.
let private upgraderBody = castFull upgraderPattern

/// The generalist at the same bank: `9W/9C/9M` — one Carry per Work where
/// the gate's line is one per four, so it is the row the gate must leave
/// alone, its whole design being that it walks its energy somewhere.
let private workerBody = castFull workerPattern

/// The Anchor row's live body: six Work, one Carry, one Move (ADR 0021's
/// held ceiling). A standing body by the same arithmetic as the upgrader's
/// — `1 * 4 < 6` — which is ADR 0046 saying the rule is about bodies and
/// not about rows.
let private anchorBody = castFull anchorPattern

/// The assignment one body takes in the lane, with the given furniture at
/// (15,10).
let private laneAssignment furniture sites creep =
    let { Assignments = assignments } =
        decide (bufferLaneColony furniture sites creep) Map.empty Set.empty None

    Map.tryFind (creep: CreepInfo).Name assignments

[<Tests>]
let standingBodyTests =
    testList
        "the standing body"
        [
            test "beside the buffer a standing body upgrades where the generalist builds" {
                // ADR 0046's whole claim, pairwise on the body and nothing
                // else: the same tile, the same pool, the same tier. The
                // site is the cheaper of the two Surplus Tasks for anyone
                // allowed to take it, so the generalist takes it; for the
                // upgrader row's body Build is inapplicable and Upgrade is
                // what is left. A standing body's Carry holds fifty energy
                // against eleven Work, so the trip would spend one tick
                // delivering for every tick of the walk out and back, with
                // eleven Work idle beside the buffer meanwhile.
                let site = [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                let sites = [ { Id = "site-1" } ]

                Expect.equal
                    (laneAssignment site sites upgraderBody)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the standing body spends its load where it stands"

                Expect.equal
                    (laneAssignment site sites workerBody)
                    (Some(taskId (Build "site-1")))
                    "the generalist at the same tile takes the nearer Task, as it always has"
            }

            test "the Anchor row's body is a standing body too" {
                // ADR 0046 says the gate covers `6W/1C/1M`, and says the
                // rule is read off parts and never off a row name (ADR
                // 0006). Pinned against the generalist one rival at a time
                // — the same lane, the same site, one body swapped — so it
                // is the arithmetic `1 * 4 < 6` that is on trial and not
                // some Anchor-shaped exception.
                //
                // What the ADR claims about the *colony* is that this
                // changes nothing for the Anchor row, because a rank-0
                // Harvest holds it at its Post long before travel cost gets
                // a say — which is the next case. This one is what the rule
                // says when Harvest is not in the pool at all.
                //
                // And ADR 0046's own "or holds no Task at all" is what it
                // says here since ADR 0048: this creep stands one step
                // outside the controller's Upgrade Work Area, and a
                // Work-heavy body takes Upgrade only where it can already
                // act on it. The gate on trial is unchanged — Build is
                // still inapplicable to `6W/1C/1M` by `1 * 4 < 6` — and
                // the generalist beside it still takes that Build, which
                // is the pairwise the case above spells out.
                let site = [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]

                Expect.equal
                    (laneAssignment site [ { Id = "site-1" } ] anchorBody)
                    None
                    "one Carry against six Work is a commute, whichever row cast it and whichever way it walks"
            }

            test "a standing body does not repair either" {
                // The third of ADR 0046's three deliveries. A road at half
                // hits is the ordinary Repair (ADR 0034), one step from the
                // creep and three from the controller, so the pairwise
                // reads exactly as the Build one does.
                let road = [ "road-1", { X = 15; Y = 10 }, Structure BuiltKind.Road ]

                let dented (colony: ColonyView) =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Hits =
                                    Map.add
                                        "road-1"
                                        { Hits = 100; HitsMax = 5000 }
                                        colony.Spatial.Hits
                            }
                    }

                let assignedFor creep =
                    let { Assignments = assignments } =
                        decide (dented (bufferLaneColony road [] creep)) Map.empty Set.empty None

                    Map.tryFind (creep: CreepInfo).Name assignments

                Expect.equal
                    (assignedFor upgraderBody)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a repair is a delivery, and this body delivers fifty at a time"

                Expect.equal
                    (assignedFor workerBody)
                    (Some(taskId (Repair "road-1")))
                    "the generalist at the same tile still repairs the road beside it"
            }

            test "the buffer's own Withdraw stays open to a standing body" {
                // ADR 0016's gate is `Work > Move` and ADR 0019's is a Work
                // part, and ADR 0046 opens no third one (the ticket's own
                // trap): the upgrader row is at `Work = Move` with a Work
                // part, so the buffer it stands beside is exactly what it
                // draws from. Empty, so the two Surplus Tasks are
                // inapplicable for want of energy and the Withdraw is the
                // whole of what is left.
                let empty = creepWith "upgrader" 0 50 (bodyFor upgraderPattern 1800)

                Expect.equal
                    (laneAssignment [] [] empty)
                    (Some(taskId (Withdraw "can-buf")))
                    "the row drinks from the buffer at its feet, which is why it stands there"
            }

            test "a buffer under the worth-the-trip line is still this row's drink" {
                // #232 gates a Withdraw on the store holding half the
                // asking body's free room — a line that prices a *trip*,
                // and this row makes none: it lives at that store, and ADR
                // 0046 opens no new gate on its Withdraw (the same
                // exception #205 makes of a site under a creep's own feet).
                // Without the exemption a buffer holding twenty-four left
                // an `11W/1C/11M` upgrader with no applicable Task at all —
                // Build, Repair and Refill are shut to it, and Upgrade
                // needs energy it could not go and get.
                //
                // Pairwise on the body alone: the same twenty-four in the
                // same buffer, and the generalist that would have to walk
                // there is refused by the line as any walking body is.
                let thin creep =
                    let colony = bufferLaneColony [] [] creep

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.add "can-buf" 24 colony.Spatial.Stores
                            }
                    }

                let assignedIn creep =
                    let { Assignments = assignments } = decide (thin creep) Map.empty Set.empty None

                    Map.tryFind (creep: CreepInfo).Name assignments

                let emptyOf pattern =
                    let body = bodyFor pattern 1800

                    creepWith
                        pattern.Name
                        0
                        (50 * (body |> List.filter ((=) Carry) |> List.length))
                        body

                Expect.equal
                    (assignedIn (emptyOf upgraderPattern))
                    (Some(taskId (Withdraw "can-buf")))
                    "twenty-four under its feet is a trip's worth for the row that never leaves"

                Expect.equal
                    (assignedIn (emptyOf workerPattern))
                    None
                    "the generalist walks to that buffer, so the line prices the walk and refuses"
            }

            test "under RCL3 the colony's own extension site joins the flow" {
                // A bootstrapping room builds its bank before its
                // controller: the site is feeding-tier while the controller
                // is under `Tuning.BootstrapLevel` with a spawn standing, and
                // surplus like any home site from RCL3 up. Pairwise on the
                // level alone: the same lane, the same worker, the same
                // site, and the match factor says which tier decided.
                //
                // Measured against the **flow** and no longer against the
                // Upgrade beside it (#234): a surplus Build outranks that
                // Upgrade by a rung now, so the site wins on rank at either
                // level and the controller has stopped being an instrument.
                // The hungry spawn `bufferLaneFlow` stands at the lane's far
                // end is one: on the feeding tier the site ties it and takes
                // the worker on price, and a rung below it the site is
                // outranked by it however near it stands.
                let lane level =
                    bufferLaneFlow
                        [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                        [ { Id = "site-1" } ]
                        (creepWith "w" 100 0 (bodyFor workerPattern 300))
                    |> withLevel level

                let matched level =
                    let { Verdicts = verdicts } = decide (lane level) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("w", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched 2)
                    (Some(taskId (Build "site-1"), MatchFactor.TravelCost))
                    "at RCL2 the extension site is feeding-tier: it ties the flow and stands nearer"

                Expect.equal
                    (matched 3)
                    (Some(taskId (Refill "spawn-1"), MatchFactor.Rank))
                    "at RCL3 it is surplus, and the flow five steps off outranks it"
            }

            test "a standing body fetches from the buffer alone: dry, it waits for the haulers" {
                // #206. The buffer empty, a stocked Storage and a pile of
                // 400 both standing five tiles down the lane: the generalist
                // walks to one of them, the standing body stands. Its one
                // Carry is a trip's worth, and the trip is the commute ADR
                // 0046 refuses — live it was fifty tiles across a Seam for
                // fifty energy. Pairwise on the body alone.
                let dryLane =
                    { bufferLane with
                        Stores = Map.ofList [ "can-buf", 0; "sto-1", 5000; "pile-1", 400 ]
                    }

                let colony creep =
                    { bufferLaneColony [] [] creep with
                        Spatial =
                            dryLane
                            |> withTargets
                                [
                                    "sto-1", { X = 18; Y = 10 }, Structure BuiltKind.Storage
                                    "pile-1", { X = 18; Y = 11 }, Dropped
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList [ (creep: CreepInfo).Name, { X = 14; Y = 10 } ]
                                })
                    }

                let empty pattern =
                    let body = bodyFor pattern 1800

                    creepWith
                        pattern.Name
                        0
                        (50 * (body |> List.filter ((=) Carry) |> List.length))
                        body

                let outcome creep =
                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (colony creep) Map.empty Set.empty None

                    Map.tryFind creep.Name assignments, verdicts

                let standingAssigned, standingVerdicts = outcome (empty upgraderPattern)

                Expect.equal
                    standingAssigned
                    None
                    "the standing body takes neither the Storage nor the pile"

                Expect.contains
                    standingVerdicts
                    (Verdict.Unassigned("upgrader", IdleReason.NoneApplicable))
                    "and it is inapplicability, not price: nothing in the pool is its to walk to"

                let workerAssigned, _ = outcome (empty workerPattern)

                Expect.isTrue
                    (workerAssigned = Some(taskId (Withdraw "sto-1"))
                     || workerAssigned = Some(taskId (Pickup "pile-1")))
                    "the generalist, one Carry per Work, walks to whichever intake is cheaper"
            }

            test "an Anchor with a site beside it still only Harvests" {
                // ADR 0046's claim about the Anchor row, at the seam it is
                // made about: with Harvest in the pool the rank settles it
                // (feeding above surplus) and the gate never gets a say, so
                // the row's behaviour is unchanged. Pairwise on the pool
                // rather than on the body — the same Anchor, once with the
                // site and once without — because it is the site that is on
                // trial here.
                let seatRoom =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          (openSeats { X = 10; Y = 10 }
                           @ [ { X = 12; Y = 10 }, Plain; { X = 13; Y = 10 }, Plain ]) with
                        TargetKinds = Map.ofList [ "src-a", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "anchor", { X = 11; Y = 10 } ]
                        })

                let colony sites targets =
                    { bareRespawn with
                        Bank = bank 1800 1800
                        Sources = [ source "src-a" ]
                        Refillables = []
                        Controller = None
                        ConstructionSites = sites
                        Creeps = [ creepWith "anchor" 25 25 (bodyFor anchorPattern 1800) ]
                        Spatial = seatRoom |> withTargets targets
                    }

                let assignedWith sites targets =
                    let { Assignments = assignments } =
                        decide (colony sites targets) Map.empty Set.empty None

                    Map.tryFind "anchor" assignments

                Expect.equal
                    (assignedWith [] [])
                    (Some(taskId (Harvest "src-a")))
                    "with nothing else in the pool the Anchor digs"

                Expect.equal
                    (assignedWith
                        [ { Id = "site-1" } ]
                        [ "site-1", { X = 12; Y = 10 }, Site BuiltKind.Extension ])
                    (Some(taskId (Harvest "src-a")))
                    "and a site one step away does not move it — the rule only writes down what the rank already did"
            }
        ]

/// The delivery lane: the same corridor with one hungry spawn standing at
/// (15,10) and nothing else at all — no controller, no source, no site —
/// so the Refill of that spawn is the only Task in the pool and an empty
/// assignment map means the gate and nothing else.
let private deliveryLaneColony creep =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Controller = None
        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
        Creeps = [ creep ]
        Spatial =
            spatial [] bufferLaneField
            |> withTargets [ "spawn-1", { X = 15; Y = 10 }, Structure BuiltKind.Spawn ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.singleton { X = 15; Y = 10 }
                    CreepPositions = Map.ofList [ (creep: CreepInfo).Name, { X = 14; Y = 10 } ]
                })
    }

let private deliveryAssignment creep =
    let { Assignments = assignments } =
        decide (deliveryLaneColony creep) Map.empty Set.empty None

    Map.tryFind (creep: CreepInfo).Name assignments

[<Tests>]
let standingRefillTests =
    testList
        "the standing body and the Refill"
        [
            test "the hauler row is outside the gate by arithmetic, not by exception" {
                // The trap ADR 0046's rule is written to walk into and
                // not be caught by: a hauler has no Work at all, so
                // `Carry * 4 < Work` reads `8 * 4 < 0` — false — and the
                // row whose entire life is delivery keeps every delivery
                // it ever had. Pinned because the gate's arithmetic could
                // so easily have been written the other way round.
                Expect.equal
                    (deliveryAssignment (hauler "hauler" 100 0))
                    (Some(taskId (Refill "spawn-1")))
                    "no Work part means no standing body, whatever the Carry"
            }

            test "a standing body does not refill the spawn beside it" {
                Expect.equal
                    (deliveryAssignment upgraderBody)
                    None
                    "one Carry against eleven Work delivers fifty energy a round trip"
            }

            test "a full Anchor off its Post keeps no delivery either" {
                // The state ADR 0046's Anchor consequence is qualified
                // for, and the one the Build arm's own #157 comment
                // records as observed: an Anchor whose Post has no
                // standing container to catch its overflow fills up, and
                // with no Harvest left in its pool the rank that used to
                // settle everything has nothing to settle. Before the gate
                // this body took the spawn's feeding-tier Refill two steps
                // away; now it holds no Task at all here, and takes a
                // pooled Upgrade at any distance where one exists.
                //
                // Pinned rather than left to be discovered: it is the
                // whole live cost of writing the gate as a prohibition,
                // and the glossary's "the rule only writes down what a
                // rank-0 Harvest was already doing" is true only where a
                // Harvest is in the pool.
                Expect.equal
                    (deliveryAssignment anchorBody)
                    None
                    "one Carry against six Work is the same commute, and the same prohibition"
            }

            test "the row's own small cast is outside the gate with the rest of the band" {
                // `3W/1C/3M`, the upgrader row's body at the RCL2 bank of
                // 550: the row, and not a standing body — `1 * 4 < 3` is
                // false — so it delivers like the generalist it is read
                // back to. The band under an 800 bank pinned at the seam
                // rather than only in the arithmetic above, because it is
                // the gate and not the sizing rule that the band is
                // interesting for (ADR 0046, Consequences; #187 settled it
                // the other way, the quota hiring none at a bank whose cast
                // this gate reads back to the generalist — a body the row
                // could never count).
                Expect.equal
                    (deliveryAssignment (creepWith "upgrader" 50 0 (bodyFor upgraderPattern 550)))
                    (Some(taskId (Refill "spawn-1")))
                    "three Work against a fifty-energy load is not yet a commute"
            }

            test "the generalist beside it refills as it always has" {
                // The near miss, and the reason the ratio is four rather
                // than something tighter: `9W/9C/9M` is one Carry per
                // Work where the gate's line is one per four, so the whole
                // worker row is outside it at every bank (ADR 0003's
                // parity keeps it there).
                Expect.equal
                    (deliveryAssignment workerBody)
                    (Some(taskId (Refill "spawn-1")))
                    "the row that carries its energy to work keeps its deliveries"
            }
        ]

/// A live Anchor's shape, `6W/1C/1M`: Work-heavy by ADR 0016's ratio
/// (`6 > 1`) and a standing body by ADR 0046's (`1 × 4 < 6`), so before
/// #205 every one of Build, Repair, Refill and Withdraw was shut to it and
/// Harvest at its Post was the whole of its working life.
let private postBody name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Work; Work; Work; Work; Work; Carry; Move ]

/// #205's colony: the outpost rock at (10,46) with its container gone and
/// the plan's site back on the Seat at (10,45) — the live shape after an
/// invader demolished three of them (W12S27 15,44 and W13S28 15,8 / 18,3)
/// — and one body of the caller's shape standing where the caller puts it.
///
/// No controller and no refillable, as the fixtures above have it, so the
/// pool is the two rocks and the site and a Matched factor names one
/// comparison rather than reporting on some third candidate. The home
/// creep the base fixture stands at (10,2) is taken out with it: the
/// caller's bodies are the whole colony, and every Verdict is about one of
/// them. Two rosters because the cap cases need both ends of the Seam: the
/// bodies standing in the outpost, and the ones standing at home.
let private raisingCrowd kind (outpostCreeps: (CreepInfo * Pos) list) homeCreeps =
    let colony =
        northBorderColony { X = 10; Y = 38 }
        |> withNorthOutpost (Some { X = 10; Y = 46 })
        |> withOutpostSiteOf kind { X = 10; Y = 45 }

    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let placed (creeps: (CreepInfo * Pos) list) =
        creeps |> List.map (fun (c, at) -> c.Name, at) |> Map.ofList

    { colony with
        Creeps = outpostCreeps @ homeCreeps |> List.map fst
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = placed homeCreeps
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = placed outpostCreeps
                }
    }

/// The one-body case the cases below are mostly written on.
let private raisingColony kind (body: CreepInfo) (at: Pos) = raisingCrowd kind [ body, at ] []

/// The same colony at home: a rock at (10,10) walled in but for its two
/// Seats, a container site on one of them, and one body standing on it.
/// #205's rule reads no room — the tick an RCL2 colony's own source
/// container is planned, the body that will garrison it raises it — and
/// this is that case with the Seam taken out of the picture.
let private homeRaisingColony kind (body: CreepInfo) (at: Pos) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-a" ]
        ConstructionSites = [ { Id = "can-a" } ]
        Creeps = [ body ]
        Spatial =
            spatial
                []
                [
                    { X = 9; Y = 10 }, Plain
                    { X = 10; Y = 10 }, Wall
                    { X = 11; Y = 10 }, Plain
                ]
            |> withTargets
                [ "src-a", { X = 10; Y = 10 }, Source; "can-a", { X = 9; Y = 10 }, Site kind ]
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ body.Name, at ]
                })
    }

[<Tests>]
let postSiteTests =
    testList
        "the Anchor raises its own Post"
        [
            test "empty on its container site the Anchor digs; full it builds" {
                // #205's whole mechanism, one body and one tile. Two rules
                // had closed the door between them: ADR 0045 empties an
                // unposted outpost source's Work Area, so no heavy body
                // walks there at all, and ADR 0046 shuts Build to a
                // standing body. What was left was the worker row
                // commuting a Seam and fifty tiles at fifty energy a trip
                // against 5,000 progress — thousands of ticks of lost
                // income every time an invader demolishes a container.
                //
                // The site is a Post, so the rock has a Work Area again;
                // the site is under the body's feet, so Build is not the
                // commute ADR 0046 forbids. The pair then alternates on
                // the store alone: `FreeCapacity = 0` ends the dig (a site
                // catches no overflow, ADR 0024) and carried energy is
                // what Build asks for.
                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (postBody "w" 0 50) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "empty, it digs the rock beside it — the home rock is a Seam and thirty tiles away"

                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (postBody "w" 50 0) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "full, the dig is over and the site under its feet is what is left"

                Expect.equal
                    (actionIntents
                        (decide
                            (raisingColony
                                BuiltKind.Container
                                (postBody "w" 50 0)
                                { X = 10; Y = 45 })
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ BuildSite("w", "site-out") ]
                    "and it spends what it dug into the progress without moving a tile"
            }

            test "swap the kind and both doors shut again" {
                // Pairwise, one rival at a time: the same body on the same
                // tile in the same colony, with a road site there instead
                // of a container site. Nothing on that Seat is a Post, so
                // the outpost rock's Work Area is empty for a heavy body
                // (ADR 0045) and Harvest is inapplicable; the site is no
                // Post either, so ADR 0046's gate stands and Build is shut
                // to a standing body. Which leaves the outpost rock
                // offering this body nothing at all: empty it is thrown
                // back on the home room's own bare-Seat fallback thirty
                // tiles and a Seam away — the theft ADR 0045 records and
                // leaves standing — and full it holds no Task, which is
                // #197's shape and stays #197's.
                let road energy free =
                    raisingColony BuiltKind.Road (postBody "w" energy free) { X = 10; Y = 45 }

                Expect.equal
                    (matchOf (road 0 50))
                    (Some(taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "empty, the outpost rock it is standing on offers it no tile to work from"

                Expect.isNone
                    (matchOf (road 50 0))
                    "full, the site beneath it is somebody else's work"
            }

            test "the site hires the garrison that raises it" {
                // The Anchor quota's own input (`Atlas.postCount`, read by
                // `planSpawns`): a Post is a garrison place, and the tile a
                // container is going up on is one — otherwise the body the
                // rule is written for is never cast and never walks there.
                // Pairwise on the site and nothing else.
                let count kind =
                    Atlas.postCount (
                        Atlas.ofView (raisingColony kind (postBody "w" 0 50) { X = 10; Y = 45 })
                    )

                Expect.equal (count BuiltKind.Container) 1 "the container site is a Post"
                Expect.equal (count BuiltKind.Road) 0 "and a site of any other kind is none"
            }

            test "the Work Area is the site tile and not the Seat beside it" {
                // ADR 0020's narrowing, over the Post #205 adds: the other
                // Seat of the same rock is standing room for a light body
                // and not for this one, so a heavy body that lands there is
                // walked onto the site rather than left digging beside it.
                // Which is the whole reason the site is a Post and not a
                // second bare-Seat fallback — a body that dug from (10,47)
                // would put its twelve a tick on the floor.
                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide
                        (raisingColony BuiltKind.Container (postBody "w" 0 50) { X = 10; Y = 47 })
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (verdicts
                     |> List.tryPick (function
                         | Verdict.Matched("w", task, _) -> Some task
                         | _ -> None))
                    (Some(taskId (Harvest "src-out")))
                    "the rock is its Task from the Seat next door"

                Expect.isEmpty (digIntentsFor "w" intents) "but it may not dig from where it stands"

                Expect.isNonEmpty
                    (moveIntentsFor "w" intents)
                    "so it walks the one tile onto its Post"
            }

            test "the rule reads the tile and never the row" {
                // ADR 0006, and #157 unmoved with it: the exception is a
                // fact about where the body is standing, so the generalist
                // standing on the same tile takes the same Build it always
                // took. Pairwise on the body alone.
                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (worker "w" 50 0) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the worker row's own Build across the Seam is untouched"
            }

            test "a home rock's container site is the same Post" {
                // #205's rule is not an outpost rule: the Seat under a
                // container site is a garrison place in every room, so an
                // RCL2 colony raises its own first source container the
                // same way. The room is left out of the rule deliberately
                // — what the two halves of ADR 0045 differ on is the bare
                // Seat *fallback*, and a rock with a site on a Seat has a
                // Post and never reaches it.
                Expect.equal
                    (matchOf (
                        homeRaisingColony BuiltKind.Container (postBody "w" 0 50) { X = 9; Y = 10 }
                    ))
                    (Some(taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "empty, it digs"

                Expect.equal
                    (matchOf (
                        homeRaisingColony BuiltKind.Container (postBody "w" 50 0) { X = 9; Y = 10 }
                    ))
                    (Some(taskId (Build "can-a"), MatchFactor.OnlyCandidate))
                    "full, it builds the site under its feet"

                // Pairwise, the same body and tile with a road site there:
                // at home the bare-Seat fallback is still ADR 0020's, so
                // the empty body keeps its dig — and the full one is back
                // inside ADR 0046's gate with nothing to do.
                Expect.equal
                    (matchOf (
                        homeRaisingColony BuiltKind.Road (postBody "w" 0 50) { X = 9; Y = 10 }
                    ))
                    (Some(taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "the home fallback stands: an empty body digs from any Seat"

                Expect.isNone
                    (matchOf (
                        homeRaisingColony BuiltKind.Road (postBody "w" 50 0) { X = 9; Y = 10 }
                    ))
                    "and a full standing body has no delivery it may walk to"
            }

            test "a site is a Post and still no income: the quotas wait for the container" {
                // The split #205 draws, at the seam ADR 0042 draws it on. A
                // source whose energy goes into 5,000 progress pays no haul
                // term and feeds no mouth at home, so `Decide.isPosted`
                // reads the standing census while the garrison reads the
                // whole one. Pairwise on the structure: the same Seat,
                // pending against built.
                let atlasOf kinds =
                    let colony = raisingColony kinds (postBody "w" 0 50) { X = 10; Y = 45 }

                    Atlas.ofView colony

                let pending = atlasOf BuiltKind.Container

                Expect.equal
                    (Atlas.postsOf pending "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.singleton { X = 10; Y = 45 })
                    "the site is the rock's Post"

                Expect.isEmpty
                    (Atlas.standingPostsOf pending "src-out")
                    "and nothing stands on it, so the rock is in no quota yet"
            }

            test "full beside its site, the reprieve walks it on rather than stranding it" {
                // Criterion 1's own wording — a body *beside* the site,
                // full — and the case the whole mechanism rests on: the
                // Build exception asks for the exact tile, so what saves a
                // full body one step off it is ADR 0048's walk-home
                // reprieve, which reads `Atlas.postsOf`. Had that call site
                // been left on the standing census with `isPosted`, this
                // body would hold no Task at all: Harvest shut by a full
                // store on a site that catches no overflow, Build shut by
                // ADR 0046, Withdraw and Repair and Refill shut with it —
                // #197's shape in the one case this ticket says it has
                // given an exit to.
                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide
                        (raisingColony BuiltKind.Container (postBody "w" 50 0) { X = 10; Y = 47 })
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (verdicts
                     |> List.tryPick (function
                         | Verdict.Matched("w", task, _) -> Some task
                         | _ -> None))
                    (Some(taskId (Harvest "src-out")))
                    "full or empty, the rock a step away is what it holds"

                Expect.isEmpty
                    (actionIntents intents)
                    "it builds nothing from where it stands — the exception is the tile and not the range"

                Expect.isNonEmpty
                    (moveIntentsFor "w" intents)
                    "so the walk onto its Post is the whole of this tick"
            }

            test "a garrison on the rock's other Post does not strand the body raising the site" {
                // The same reprieve with #258's unmanned-Post condition on
                // it, on the one shape where the two censuses part: a rock
                // with a **standing** container Post and a container site
                // Post beside it — the shape "the Anchor quota counts
                // Posts" above declares first-class, for the whole of the
                // window #205 exists for. Read against the standing census
                // the rock would be occupied by the garrison on the built
                // container, and the full body one step off its own site
                // would lose Harvest with Build already asking for the
                // exact tile (#205, #234) and everything else shut (ADR
                // 0016, ADR 0046, ADR 0048) — no Task at all, for the rest
                // of its life, with the site left unraised. The condition
                // reads every Post, so the standing room it is walking to
                // is its own.
                let room =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "cont-1", { X = 11; Y = 10 }
                              "can-a", { X = 9; Y = 10 }
                          ]
                          [
                              { X = 8; Y = 10 }, Plain
                              { X = 9; Y = 10 }, Plain
                              { X = 10; Y = 10 }, Wall
                              { X = 11; Y = 10 }, Plain
                          ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "cont-1", Structure BuiltKind.Container
                                    "can-a", Site BuiltKind.Container
                                ]
                    }

                let colony =
                    { bareRespawn with
                        Spawns = []
                        Refillables = []
                        Controller = None
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "can-a" } ]
                        Creeps = [ postBody "a1" 50 0; postBody "g1" 0 50 ]
                        Spatial =
                            room
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 8; Y = 10 }; "g1", { X = 11; Y = 10 } ]
                                })
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the site Post is standing room the garrison next door is not on"

                Expect.isNonEmpty
                    (moveIntentsFor "a1" intents)
                    "so the full body takes the step back onto the site it was cast to raise"

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "and the garrison on the built container keeps its own Post"
            }

            test "the builder budget prices a commute, and the body on the site pays none" {
                // #157's budget is `Tuning.OutpostBuilders` spread over
                // the sites it has lifted, and every word of its argument is about the
                // home room's surplus work stopping "for the fifty ticks
                // each of them spends crossing". A body standing on the
                // site spends none of those, so it is outside what the
                // number prices — counted inside it, the two loaded
                // workers holding the Build through their whole commute
                // leave the garrison full on the progress with Harvest
                // shut behind it and no Task at all, which is exactly the
                // stuck-full-Anchor shape #205 exists to end.
                let crowd extra =
                    raisingCrowd
                        BuiltKind.Container
                        [ postBody "a" 50 0, { X = 10; Y = 45 } ]
                        ([
                            worker "w1" 50 0, { X = 10; Y = 2 }
                            worker "w2" 50 0, { X = 10; Y = 3 }
                         ]
                         @ extra)

                let held =
                    Map.ofList [ "w1", taskId (Build "site-out"); "w2", taskId (Build "site-out") ]

                let matchedOf name (verdicts: Verdict list) =
                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched(who, task, _) when who = name -> Some task
                        | _ -> None)

                let { Verdicts = verdicts } = decide (crowd []) held Set.empty None

                Expect.equal
                    (matchedOf "a" verdicts)
                    (Some(taskId (Build "site-out")))
                    "the garrison builds what is under its feet whoever else is walking to it"

                // Pairwise on the body and nothing else: the row the
                // budget was written for is capped exactly as #157 had it,
                // so a third loaded worker still waits at home.
                let { Verdicts = withThird } =
                    decide (crowd [ worker "w3" 50 0, { X = 10; Y = 4 } ]) held Set.empty None

                Expect.isNone
                    (matchedOf "w3" withThird)
                    "and the third commuter is still refused: #157's number is unmoved"
            }

            test "the Post is held by the body standing on it, digging or building" {
                // ADR 0024's Post cap is "one Anchor per Post", and on a
                // standing container the garrison never lets it go: the
                // overflow reprieve keeps its Harvest applicable through a
                // full store. On a site there is no overflow, so the pair
                // alternates and the slot would read as free on every
                // build tick — admitting a second heavy body onto the one
                // tile the first is standing on, and releasing the
                // incumbent from Build onto a Seam-crossing walk to the
                // home rock the tick it empties. So the slot is held by
                // where the body stands and not by what it holds.
                let pair (aEnergy: int) (aFree: int) =
                    raisingCrowd
                        BuiltKind.Container
                        [
                            postBody "a" aEnergy aFree, { X = 10; Y = 45 }
                            postBody "b" 0 50, { X = 10; Y = 47 }
                        ]
                        []

                let { Assignments = building } = decide (pair 50 0) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a" building)
                    (Some(taskId (Build "site-out")))
                    "the garrison spends the load it dug"

                Expect.notEqual
                    (Map.tryFind "b" building)
                    (Some(taskId (Harvest "src-out")))
                    "and the Post it is standing on admits no second heavy body while it does"

                // Pairwise on the incumbent's store alone: empty, it holds
                // the Harvest itself and the cap answers as it always did.
                let { Assignments = digging } = decide (pair 0 50) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a" digging)
                    (Some(taskId (Harvest "src-out")))
                    "empty, the same body holds the same Post through the dig"

                Expect.notEqual
                    (Map.tryFind "b" digging)
                    (Some(taskId (Harvest "src-out")))
                    "and the second body is refused by the cap it always was"
            }
        ]
