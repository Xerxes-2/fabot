/// The Task pool: which Tasks a colony offers and what caps each one — the
/// Seats, the Refill cluster that is one Task (ADR 0054), the stores a body may
/// draw from (ADR 0019, ADR 0023), the container Posts and their body-aware
/// capacity (ADR 0024), the piles and tombstones a Pickup names, the Repairs,
/// and the Restock dispatch that judges a drained source at arrival (ADR 0025).
module Fabot.Core.Tests.Decide.PoolTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

[<Tests>]
let seatTests =
    testList
        "seat capacity"
        [
            test "a single-Seat source gets exactly one of three empty creeps" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    1
                    "one Seat supports exactly one harvester"

                let harvestIntents =
                    intents
                    |> List.filter (function
                        | HarvestSource _ -> true
                        | _ -> false)

                Expect.hasLength harvestIntents 1 "surplus creeps emit no Harvest intent"
            }

            test "creeps overflowing a single-Seat source are matched elsewhere" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 20; Y = 20 } ]
                                ([ { X = 9; Y = 10 }, Plain ] @ openSeats { X = 20; Y = 20 })

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.hasLength
                    (harvesters assignments "src-b")
                    2
                    "overflow lands on the source with free Seats"
            }

            test "a creep denied a Seat falls through to a lower-rank task" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 25 25 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.contains
                    (assignments |> Map.toList |> List.map snd)
                    (taskId (Upgrade "ctrl-1"))
                    "the denied creep sinks its energy into the controller instead"
            }

            test "Seats are counted from terrain: swamp is a Seat, wall and absent are not" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 } ]
                                [
                                    { X = 9; Y = 10 }, Plain
                                    { X = 11; Y = 10 }, Swamp
                                    { X = 10; Y = 9 }, Wall
                                ]

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    2
                    "plain and swamp neighbours are Seats; wall and off-map are not"
            }

            test "oversold remembered assignments are trimmed back to the Seat count" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let stale =
                    Map.ofList
                        [ "w1", (taskId (Harvest "src-a")); "w2", (taskId (Harvest "src-a")) ]

                let { Assignments = assignments } = decide snapshot stale Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the cap holds even against remembered oversell"
            }

            test "without a spatial projection Harvest stays uncapped" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    3
                    "no terrain data means no cap — today's room behaviour"
            }
        ]

/// The [[refill cluster]] on open ground (ADR 0054): the spawn at (10,10)
/// and two extensions south of it in the column x = 10, every structure
/// tile an obstacle as the engine has it and a 3-wide plain band around
/// them. The caller says how much room each of the three has left and where
/// the bodies stand, which is the whole of what this Task's [[capacity]],
/// its [[work area]] and the [[emitter]]'s pick turn on.
let clusterColony (spawnFree, ext1Free, ext2Free) creeps positions =
    let structures =
        [
            "spawn-1", { X = 10; Y = 10 }
            "ext-1", { X = 10; Y = 12 }
            "ext-2", { X = 10; Y = 14 }
        ]

    { bareRespawn with
        Refillables =
            [
                refillable "spawn-1" spawnFree BuiltKind.Spawn
                refillable "ext-1" ext1Free BuiltKind.Extension
                refillable "ext-2" ext2Free BuiltKind.Extension
            ]
        Creeps = creeps
        Spatial =
            spatial
                structures
                [
                    for x in 9..11 do
                        for y in 9..18 -> { X = x; Y = y }, Plain
                ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = structures |> List.map snd |> Set.ofList
                    CreepPositions = Map.ofList positions
                })
    }

/// The creeps holding one Task this tick, by name.
let holdersOf task assignments =
    assignments
    |> Map.toList
    |> List.filter (fun (_, tid) -> tid = taskId task)
    |> List.map fst

[<Tests>]
let refillClusterTests =
    testList
        "refill cluster"
        [
            test "the cluster admits as many bodies as its free energy divides into loads" {
                // ADR 0054's bound, pinned pairwise at the one line it can
                // be wrong on: the 300 bank casts a `4C/2M` hauler, so one
                // load is 200 — a hundred of room draws one body and three
                // hundred draws two. Two loaded carriers standing on either
                // side of the spawn, so nothing but the cap separates them.
                let colony free =
                    clusterColony
                        (free, 0, 0)
                        [
                            creepWith "h1" 50 0 [ Carry; Carry; Move ]
                            creepWith "h2" 50 0 [ Carry; Carry; Move ]
                        ]
                        [ "h1", { X = 9; Y = 10 }; "h2", { X = 11; Y = 10 } ]

                let holders free =
                    let { Assignments = assignments } =
                        decide (colony free) Map.empty Set.empty None

                    holdersOf (Refill "spawn-1") assignments

                Expect.hasLength (holders 100) 1 "one load of room admits one body"
                Expect.hasLength (holders 300) 2 "and two loads' worth admits the second"
            }

            test "an extension filled while a body walks costs it a neighbour, not its Task" {
                // The churn this ADR was written against, inverted (#226):
                // the body is aimed at the ring, not at the extension that
                // happened to be nearest, so somebody else topping that
                // extension up leaves its assignment exactly where it was.
                let walking free =
                    clusterColony
                        free
                        [ creepWith "h1" 50 0 [ Carry; Carry; Move ] ]
                        [ "h1", { X = 10; Y = 18 } ]

                let sticky = Map.ofList [ "h1", taskId (Refill "spawn-1") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide (walking (0, 50, 0)) sticky Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the near extension is full and the far one is not: the Task stands"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Released(_, _, ReleaseReason.TaskGone) -> true
                         | _ -> false))
                    "nothing went away, so nothing is released"
            }

            test "the whole ring full is what takes the Task away" {
                // The other half of the same sentence: `task-gone` still
                // fires, once, when there is nowhere in the cluster left to
                // pour — which is once a fill instead of once an extension.
                let full =
                    clusterColony
                        (0, 0, 0)
                        [ creepWith "h1" 50 0 [ Carry; Carry; Move ] ]
                        [ "h1", { X = 10; Y = 18 } ]

                let sticky = Map.ofList [ "h1", taskId (Refill "spawn-1") ]

                let { Verdicts = verdicts } = decide full sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("h1", taskId (Refill "spawn-1"), ReleaseReason.TaskGone))
                    "a cluster with no room left is no Task"
            }

            test "the arriving body pours into the member beside it that has room" {
                // h1 at (10,13) touches ext-1 (10,12) and ext-2 (10,14)
                // alike, so the pair moves only which of them is hungry —
                // the [[emitter]]'s pick, made at arrival off the tile the
                // body is standing on rather than at matching time.
                let arrived free =
                    clusterColony
                        free
                        [ creepWith "h1" 50 0 [ Carry; Carry; Move ] ]
                        [ "h1", { X = 10; Y = 13 } ]

                let { Intents = northIntents } =
                    decide (arrived (0, 50, 0)) Map.empty Set.empty None

                let { Intents = southIntents } =
                    decide (arrived (0, 0, 50)) Map.empty Set.empty None

                Expect.contains
                    northIntents
                    (TransferEnergyToStructure("h1", "ext-1"))
                    "ext-2 is full, so the load goes into the extension that is not"

                Expect.contains
                    southIntents
                    (TransferEnergyToStructure("h1", "ext-2"))
                    "and the other way round, so it is room and not id order deciding"
            }

            test "a load the ring no longer has room for is released over-capacity" {
                // The price ADR 0054 records rather than removes. The cap
                // is `ceil(free / one load)` and the ring's free energy
                // only falls, so on the tick it crosses a load boundary one
                // of the bodies aimed at the ring is released — a body that
                // may well be the one already standing beside a member,
                // because the release fold walks the assignments in
                // creep-name order and not by proximity.
                //
                // It is once per load *poured*, where a Task per extension
                // paid a `task-gone` per extension filled, so the churn is
                // bounded far below what #226 removed — but it is not zero,
                // and this is where it is written down.
                let colony free =
                    clusterColony
                        free
                        [
                            creepWith "h1" 50 0 [ Carry; Carry; Move ]
                            creepWith "h2" 50 0 [ Carry; Carry; Move ]
                        ]
                        [ "h1", { X = 10; Y = 18 }; "h2", { X = 10; Y = 11 } ]

                let sticky =
                    Map.ofList [ "h1", taskId (Refill "spawn-1"); "h2", taskId (Refill "spawn-1") ]

                let outcome free =
                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (colony free) sticky Set.empty None

                    holdersOf (Refill "spawn-1") assignments, verdicts

                // Four hundred of room is two of the 300 bank's 200-energy
                // loads, so both bodies keep what they hold.
                Expect.equal
                    (fst (outcome (300, 100, 0)))
                    [ "h1"; "h2" ]
                    "two loads' worth of room holds two bodies"

                // One load poured into the ring, and the second body's load
                // is one too many for what is left.
                let holders, verdicts = outcome (200, 0, 0)

                Expect.equal holders [ "h1" ] "one load's worth of room holds one"

                Expect.contains
                    verdicts
                    (Verdict.Released("h2", taskId (Refill "spawn-1"), ReleaseReason.OverCapacity))
                    "and the other is released over-capacity, though it is the one that had arrived"
            }
        ]

[<Tests>]
let unreachableTests =
    testList
        "unreachable targets"
        [
            test
                "a remembered assignment to an unreachable source is released and its Seat refilled" {
                // src-a's one Seat connects only to w2; w1 sits on a walkable
                // island with no path anywhere, remembering the source from
                // before the wall closed in.
                let terrain =
                    [
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 20; Y = 20 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 20; Y = 20 }; "w2", { X = 10; Y = 12 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w2" ]
                    "the freed Seat goes to the creep that can reach it"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the walled-off creep falls through to the next applicable task"
            }

            test "a creep with no reachable applicable task is left unassigned and emits nothing" {
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "the dead-end assignment is released"

                Expect.isEmpty (actionIntents intents) "no action fires at an unreachable target"
                Expect.isEmpty (moveIntents intents) "and no move Intent marches at the wall"
            }

            test "an empty Work Area releases a remembered assignment" {
                // The controller is placed but every tile within upgrade
                // range lies outside the projection: nowhere to stand at all.
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial [ "ctrl-1", { X = 10; Y = 10 } ] [ { X = 20; Y = 20 }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal (Map.tryFind "w1" assignments) None "no Work Area means no assignment"
            }

            test
                "an unplaced creep keeps its assignment: no reachability filtering without geometry" {
                // Same walled-off source, but the projection does not place
                // the creep — nothing can be proven, so nothing is released.
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "geometry the projection cannot price never releases an assignment"
            }
        ]

[<Tests>]
let repairTests =
    testList
        "repair"
        [
            test "a road below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "road-1" BuiltKind.Road 2499 5000
                let half = bareRespawn |> withHits "road-1" BuiltKind.Road 2500 5000

                Expect.equal
                    (repairTasks (planTasks low noThreats))
                    [ "road-1" ]
                    "below the trigger: one Repair per ailing road"

                Expect.isEmpty
                    (repairTasks (planTasks half noThreats))
                    "at half hits the road is left alone"
            }

            test "a repaired-whole road leaves the pool" {
                let whole = bareRespawn |> withHits "road-1" BuiltKind.Road 5000 5000

                Expect.isEmpty
                    (repairTasks (planTasks whole noThreats))
                    "a whole road needs nothing"
            }

            test "kinds with no whole line never enter the pool on low hits" {
                // The ColonyView projects hits on repairable kinds only, but the
                // kind gate holds in the Planner regardless of what arrives.
                // The extensions are deliberately outside the Keep (ADR
                // 0034): cheap, twenty of them, and no creep lives on one.
                let snapshot =
                    bareRespawn
                    |> withHits "ext-1" BuiltKind.Extension 1 5000
                    |> withHits "link-1" BuiltKind.Link 1 5000
                    |> withHits "rock-1" BuiltKind.Other 1 5000

                Expect.isEmpty
                    (repairTasks (planTasks snapshot noThreats))
                    "an extension, a link and an unmodelled structure are nobody's Repair"
            }

            test "a dented Keep structure enters the pool; a whole one does not" {
                // The Keep is repaired to full (ADR 0034): it does not decay,
                // so below max means it was damaged — the same fact the
                // safe-mode arm reads, which is why a dented Keep is never
                // left standing. This revises ADR 0023's "nothing repairs the
                // Storage".
                let dented =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 4999 5000
                    |> withHits "tower-1" BuiltKind.Tower 4999 5000
                    |> withHits "sto-1" BuiltKind.Storage 4999 5000

                Expect.equal
                    (repairTasks (planTasks dented noThreats))
                    [ "spawn-1"; "sto-1"; "tower-1" ]
                    "one hit off max is hungry, on every Keep structure"

                let whole =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
                    |> withHits "tower-1" BuiltKind.Tower 5000 5000
                    |> withHits "sto-1" BuiltKind.Storage 5000 5000

                Expect.isEmpty
                    (repairTasks (planTasks whole noThreats))
                    "a Keep at full hits asks for nothing"
            }

            test "a rampart is hungry below its floor and whole at it" {
                // The floor, not half of max (ADR 0034): a rampart's max is
                // three million at RCL4, so the decaying kinds' fraction
                // would leave it hungry forever. The number restates the
                // tunable, exactly as the road tests restate the half. At
                // the level the colony keeps ramparts from (#214): below it
                // the floor is not read at all — the pairwise test beside
                // this one.
                let floor = 100_000
                let max = 3_000_000

                let keeping = bareRespawn |> withLevel 3

                let below = keeping |> withHits "ram-1" BuiltKind.Rampart (floor - 1) max
                let at = keeping |> withHits "ram-1" BuiltKind.Rampart floor max
                let fresh = keeping |> withHits "ram-1" BuiltKind.Rampart 1 max
                let over = keeping |> withHits "ram-1" BuiltKind.Rampart (max / 2) max

                Expect.equal
                    (repairTasks (planTasks below noThreats))
                    [ "ram-1" ]
                    "one hit under the floor is hungry"

                Expect.isEmpty
                    (repairTasks (planTasks at noThreats))
                    "at the floor the rampart is whole"

                Expect.equal
                    (repairTasks (planTasks fresh noThreats))
                    [ "ram-1" ]
                    "a rampart just built stands at 1 hit and is the pool's business at once"

                Expect.isEmpty
                    (repairTasks (planTasks over noThreats))
                    "half of a rampart's max is far over the floor: nothing to do"
            }

            test "below the bootstrap level a rampart has no floor: it decays away unrepaired" {
                // #214: a child at RCL2 raised three ramparts the tick the
                // engine allowed them and then held four of its five loaded
                // workers repairing them toward a floor derived for the
                // home. Below the stage the colony keeps ramparts from
                // (`keepsRamparts`) a standing rampart is not the pool's
                // business; the decaying kinds and the Keep are.
                let floor = 100_000
                let max = 300_000

                let young = bareRespawn |> withLevel 2 |> withHits "ram-1" BuiltKind.Rampart 1 max

                Expect.isEmpty
                    (repairTasks (planTasks young noThreats))
                    "a rampart at 1 hit in an RCL2 room is left to decay"

                let youngRoad = young |> withHits "road-1" BuiltKind.Road 1000 5000

                Expect.equal
                    (repairTasks (planTasks youngRoad noThreats))
                    [ "road-1" ]
                    "the decaying kinds keep their trigger in the same room"

                let grown =
                    bareRespawn |> withLevel 3 |> withHits "ram-1" BuiltKind.Rampart (floor - 1) max

                Expect.equal
                    (repairTasks (planTasks grown noThreats))
                    [ "ram-1" ]
                    "one level up the same rampart is hungry under the same floor"
            }

            test "a surplus creep is sent to repair: assignment, intent and bubble" {
                // Feeding satisfied — the spawn is full, the creep can carry no
                // more — so the surplus tier is all that is left, and the
                // half-hit road is its only member.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Repair "road-1")))
                    "the surplus creep is assigned to the Repair"

                Expect.contains
                    intents
                    (RepairStructure("w1", "road-1"))
                    "the assignment emits the repair intent"

                Expect.equal (sayIntents intents) [ "w1", "🔧" ] "a repairing creep says 🔧"
            }

            test "Repair never poaches from the feeding tier" {
                // A hungry spawn and an ailing road bid for the same loaded
                // creep: the feeding tier wins on rank, not pool order.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds itself before it patches roads: rank decided"
            }

            test "Repair never poaches from Harvest either" {
                // A half-loaded creep fits both tiers — room to harvest,
                // energy to spend — and the feeding tier wins on rank.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 25 25 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.Rank) ]
                    "the economy is fed before roads are patched: rank decided"
            }

            test "a container below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "cont-1" BuiltKind.Container 124999 250000
                let half = bareRespawn |> withHits "cont-1" BuiltKind.Container 125000 250000

                Expect.equal
                    (repairTasks (planTasks low noThreats))
                    [ "cont-1" ]
                    "below the trigger: one Repair per ailing container"

                Expect.isEmpty
                    (repairTasks (planTasks half noThreats))
                    "at half hits the container is left alone"
            }

            test "a whole container produces no Repair" {
                let whole = bareRespawn |> withHits "cont-1" BuiltKind.Container 250000 250000

                Expect.isEmpty
                    (repairTasks (planTasks whole noThreats))
                    "a whole container needs nothing"
            }

            test "container Repair is surplus-tier: feeding still wins the creep" {
                // The same duel the road fights: a hungry spawn and an ailing
                // container bid for one loaded creep, and feeding wins on rank.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 100 250000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds itself before it mends containers: rank decided"
            }

            test "a surplus creep mends the container: assignment, intent and bubble" {
                // Feeding satisfied — spawn full, creep full — so the ailing
                // container is the only work left, exactly like a road.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 100 250000

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Repair "cont-1")))
                    "the surplus creep is assigned to the container Repair"

                Expect.contains
                    intents
                    (RepairStructure("w1", "cont-1"))
                    "the assignment emits the repair intent"

                Expect.equal (sayIntents intents) [ "w1", "🔧" ] "a repairing creep says 🔧"
            }

            test "an empty creep is inapplicable to Repair" {
                // Nothing to spend: no energy makes Repair unworkable, and the
                // remembered assignment is released rather than kept.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let remembered = Map.ofList [ "w1", taskId (Repair "road-1") ]

                let {
                        Verdicts = verdicts
                        Assignments = assignments
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Repair "road-1"), ReleaseReason.Inapplicable))
                    "the empty creep's remembered Repair is released"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "nothing else fits an empty creep here"
            }
        ]

let pickups intents =
    intents
    |> List.choose (function
        | PickupEnergy(creep, pile) -> Some(creep, pile)
        | _ -> None)

/// A colony around a dropped energy pile at (10, 10) on open ground, with
/// the given creeps standing on the given tiles.
let pileColony creeps positions =
    { bareRespawn with
        Sources = []
        Creeps = creeps
        Spatial =
            { spatial
                  [ "pile-1", { X = 10; Y = 10 } ]
                  [
                      for x in 8..12 do
                          for y in 8..12 -> { X = x; Y = y }, Plain
                  ] with
                TargetKinds = Map.ofList [ "pile-1", Dropped ]
            }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

/// The same colony with a second room's layer beside its own (ADR 0041):
/// that room's ground, the piles it names, and the creeps standing on its
/// tiles — and its coordinates deliberately collide with `pileColony`'s.
/// A `Pos` carries no room, so a reflex that unioned the two rooms' piles
/// or the two rooms' creeps would pair across the border at range 0 and
/// emit a pickup the engine answers ERR_NOT_IN_RANGE (#166). The creeps
/// still enter `Creeps`, which is the colony's fleet and no room's.
let private withPileRoom room piles positions (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain =
                                Map.ofList
                                    [
                                        for x in 8..12 do
                                            for y in 8..12 -> { X = x; Y = y }, Plain
                                    ]
                            TargetPositions = Map.ofList piles
                            CreepPositions = Map.ofList positions
                        }
                        colony.Spatial.Rooms
                TargetKinds =
                    (colony.Spatial.TargetKinds, piles)
                    ||> List.fold (fun kinds (id, _) -> Map.add id Dropped kinds)
            }
    }

[<Tests>]
let pickupReflexTests =
    testList
        "pickup reflex"
        [
            test "an adjacent creep with free capacity picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "in reach and hungry: pick up"
            }

            test "a creep standing on the pile picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 10 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "range 0 is within reach"
            }

            test "a full creep leaves the pile alone" {
                let snapshot = pileColony [ worker "w1" 50 0 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "no free capacity, nothing to gain"
            }

            test "a pile out of reach draws nobody — the reflex never moves a creep" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 13 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "range 3: recapture only what is in reach"
            }

            test "every adjacent creep picks — the engine settles duplicates" {
                let snapshot =
                    pileColony
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 11 }; "w2", { X = 9; Y = 10 } ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide withSource Map.empty Set.empty None

                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the reflex fires"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-a"))
                    "the assigned task's action still goes out"
            }

            test "a pile keeps no construction site off its tile" {
                // Layout determinism (ADR 0011): a transient pile must not
                // perturb the ordering, so placement with and without the
                // pile is identical.
                let bare = atLevel 2 (openRoom 3)

                let strewn =
                    atLevel 2 (openRoom 3 |> withTargets [ "pile-1", { X = 24; Y = 24 }, Dropped ])

                let placedWith = decide strewn Map.empty Set.empty None
                let placedWithout = decide bare Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the unplaced creep picks nothing"
            }
        ]

let withdrawTasks tasks =
    tasks
    |> List.choose (function
        | Withdraw storeId -> Some storeId
        | _ -> None)

let refillTasks tasks =
    tasks
    |> List.choose (function
        | Refill structureId -> Some structureId
        | _ -> None)

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", pos ]
                                })
                    }

                let near = decide (colonyAt { X = 15; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" near.Assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the cheaper-to-reach buffer wins the feeding-tier tie"

                Expect.contains
                    near.Verdicts
                    (Verdict.Matched("w1", taskId (Withdraw "can-ctrl"), MatchFactor.TravelCost))
                    "the match speaks its Verdict: travel cost decided"

                let far = decide (colonyAt { X = 12; Y = 10 }) Map.empty Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 15; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 15; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "w1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "w1", taskId (Upgrade "ctrl-1") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

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

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 15; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the buffer Refill is live work for a body that can do nothing better"
            }

            test "a seated Withdraw emits the engine withdraw call and speaks 📥" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("w1", "can-ctrl"))
                    "in range at tick start: the Executor-bound Intent fires"

                Expect.contains intents (SayCreep("w1", "📥")) "the Task's own chat bubble"
            }
        ]

/// The stock fixture: the tier corridor with the Storage standing at
/// (16,11), between the tower and the buffer and clear of the working
/// ground the clustered ordering excludes (ADR 0022) — the controller's
/// Upgrade Work Area reaches back only to x = 17. It blocks its own tile
/// the way the projection carries a built one, and nothing but the
/// projection's kind says which structure it is.
let stockRoom =
    tierRoom
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.add { X = 16; Y = 11 } layer.Obstacles
        })
    |> withTargets [ "sto-1", { X = 16; Y = 11 }, Structure BuiltKind.Storage ]

/// The stock colony with the given hunger and stores: one loaded
/// Carry-only body standing beside the Storage, so the deepest tier of all
/// costs it nothing to reach and every shallower one — the tower one step
/// west, the buffer one step east — costs more. Whatever outbids the
/// stock outbids it against travel cost, and only rank can do that. Each
/// caller leaves the stock exactly one rival, so the Verdict's factor is
/// evidence about that pair alone.
let stockColony refillables stores =
    { bareRespawn with
        Sources = []
        Refillables = refillables
        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
        Spatial =
            { stockRoom with Stores = stores }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "h1", { X = 16; Y = 10 } ]
                })
    }

[<Tests>]
let stockTests =
    testList
        "storage stock"
        [
            test "a Storage with room is a Refill target; a full one is not" {
                // Judged from the projection's kind, as the buffer's tier is
                // (ADR 0023). The buffer is brimming in both colonies, so the
                // stock is the only thing the pool can be reporting on.
                let hungry = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ])
                let full = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (refillTasks (planTasks hungry noThreats))
                    [ "sto-1" ]
                    "the stock with room pools the deepest Refill of all"

                Expect.isEmpty
                    (refillTasks (planTasks full noThreats))
                    "a full stock pools no Refill: there is nowhere left to put a load"
            }

            test "the upgrade buffer outbids the stock, however close the stock stands" {
                // The hauler stands beside the Storage and a step short of
                // the buffer's Work Area, so travel cost points at the stock
                // and only rank can overrule it: surplus reaches the colony's
                // stock once the upgrade buffer is full and not before (ADR
                // 0023). The tier above the buffer is already pinned by the
                // rank-tier tests, so this one step completes the sequence.
                let { Verdicts = verdicts } =
                    decide
                        (stockColony [] (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "can-ctrl"), MatchFactor.Rank) ]
                    "the buffer is filled before the stock: rank decided"
            }

            test "a hungry tower outbids the stock, however close the stock stands" {
                // The buffer is brimming, so the tower is the stock's one
                // rival and the factor is evidence about that pair alone.
                let { Verdicts = verdicts } =
                    decide
                        (stockColony
                            [ refillable "tower-1" 500 BuiltKind.Tower ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "tower-1"), MatchFactor.Rank) ]
                    "the guns are fed before the stock: rank decided"
            }

            test "with every other sink full the stock takes the load" {
                // Spawn and tower full, buffer brimming: the deepest tier of
                // all is the one live Refill, and it is served by the same
                // transfer Intent, the same bubble and the same Verdict
                // vocabulary as every other Refill (ADR 0023).
                let colony =
                    stockColony
                        [
                            refillable "spawn-1" 0 BuiltKind.Spawn
                            refillable "tower-1" 0 BuiltKind.Tower
                        ]
                        (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ])

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "sto-1")))
                    "the load the colony has nowhere else to put sinks into the stock"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("h1", "sto-1"))
                    "the ordinary transfer Intent serves the Storage"

                Expect.contains
                    intents
                    (SayCreep("h1", "🔋"))
                    "the ordinary battery bubble shows it"

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a stock deposit speaks the Verdicts every other Refill speaks"
            }
        ]

[<Tests>]
let stockGateTests =
    testList
        "storage draw gate"
        [
            test "with every other sink full the stock pools no Withdraw" {
                // The gate (ADR 0023): the stock is an intake only while the
                // pool holds a Refill that is not its own. Here the spawn is
                // full and the buffer brimming, so the stock's own Refill —
                // pooled, because the stock has room — is the only one there
                // is. Counting it would gate the Storage open against itself
                // forever, and a hauler beside it would cycle energy in and
                // out of one store.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]))
                        noThreats

                Expect.equal (refillTasks tasks) [ "sto-1" ] "the stock's own Refill is pooled"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl" ]
                    "and it is not a sink that opens the stock's own Withdraw"
            }

            test "one hungry extension opens it: exactly one Storage Withdraw" {
                // The Planner reads the refillable census, so a hungry
                // extension anywhere in the colony is the sink the stock is
                // drawn for — one Withdraw for the one Storage, never one
                // per hungry sink.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "ext-1" 50 BuiltKind.Extension ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]))
                        noThreats

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "the stocked buffer's intake, and one draw on the stock"
            }

            test "the upgrade buffer counts as a sink: the stock feeds it" {
                // Every refillable full and only the buffer with room, so the
                // buffer's Refill is the whole reason the stock opens —
                // stock flows to the upgrade buffer when the sources cannot
                // keep it full (ADR 0023).
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]))
                        noThreats

                Expect.equal
                    (refillTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "the buffer is the one sink other than the stock"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "and it opens the draw on the stock"
            }

            test "an empty Storage pools no Withdraw, however hungry the colony" {
                // The stock half of ADR 0012's rule, unchanged: a store with
                // nothing in it is nobody's intake.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "ext-1" 50 BuiltKind.Extension ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))
                        noThreats

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl" ]
                    "an open gate draws nothing out of an empty stock"
            }
        ]

/// The draw fixture: a two-row plain corridor, y = 10..11, x = 8..22, with
/// the source walled in at (8,10) and its container on the Seat at (9,10),
/// the Storage off the lane at (17,11), and the upgrade buffer at (21,10)
/// beside the controller at (22,10). The stock and the controller stand as
/// obstacles; the lane runs past both. A creep on the lane at (13,10)
/// stands three plain steps from either store's Work Area — (10,10) beside
/// the source container, (16,10) beside the stock — so travel cost ties
/// the two intakes and nothing but rank can separate them; a creep further
/// east stands inside the stock's Work Area and six steps from the
/// container's, so travel cost points the other way and only rank can
/// override it.
let drawRoom =
    let lane =
        [
            for x in 8..22 do
                for y in 10..11 -> { X = x; Y = y }, (if x = 8 && y = 10 then Wall else Plain)
        ]

    spatial [] lane
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.ofList [ { X = 17; Y = 11 }; { X = 22; Y = 10 } ]
        })
    |> withTargets
        [
            "src-a", { X = 8; Y = 10 }, Source
            "can-src", { X = 9; Y = 10 }, Structure BuiltKind.Container
            "sto-1", { X = 17; Y = 11 }, Structure BuiltKind.Storage
            "can-ctrl", { X = 21; Y = 10 }, Structure BuiltKind.Container
            "ctrl-1", { X = 22; Y = 10 }, Controller
        ]

/// The draw colony: the draw room with the given stores, one creep on the
/// tile the caller puts it on, and every refillable full — so whatever
/// opens the stock's Withdraw is something the test itself put there.
let drawColony stores (creep: CreepInfo) pos =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Refillables = [ refillable "spawn-1" 0 BuiltKind.Spawn ]
        Creeps = [ creep ]
        Spatial =
            { drawRoom with Stores = stores }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ creep.Name, pos ]
                })
    }

[<Tests>]
let stockDrawTests =
    testList
        "storage draw"
        [
            test "the source container outbids the stock, however near the stock stands" {
                // The tier (ADR 0023): the stock sits one tier below the
                // source containers, so an empty hauler empties the flow's
                // own containers first and draws on the stock only when
                // they are dry. The buffer's own hunger is what opened the
                // stock's Withdraw at all. Twice, because rank beating a
                // tie and rank beating a cheaper rival are two claims: from
                // the lane's middle it is three steps to either Work Area,
                // and from inside the stock's the stock costs nothing at
                // all while the container costs six — ADR 0023's own
                // motivating case, a stock that wins every travel-cost
                // contest and must still lose.
                let drawFrom pos =
                    decide
                        (drawColony
                            (Map.ofList [ "can-src", 500; "can-ctrl", 800; "sto-1", 500 ])
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            pos)
                        Map.empty
                        Set.empty
                        None

                let equidistant = drawFrom { X = 13; Y = 10 }

                Expect.equal
                    equidistant.Verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-src"), MatchFactor.Rank) ]
                    "the flow is emptied before the stock: rank decided"

                let underfoot = drawFrom { X = 16; Y = 10 }

                Expect.equal
                    underfoot.Verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-src"), MatchFactor.Rank) ]
                    "and it is emptied first from the stock's own doorstep too"
            }

            test "topping up from the stock outbids surplus work" {
                // The tier's other neighbour: the stock is drawn on above
                // everything the colony merely spends energy on, so a
                // half-loaded creep fills up before it spends. The worker
                // stands inside the controller's Work Area and one step from
                // the stock's, so Upgrade is the cheapest rival of the three
                // and the Verdict's factor is evidence about that pair
                // alone.
                let colony =
                    { drawColony
                          (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ])
                          (worker "w1" 50 50)
                          { X = 19; Y = 10 } with
                        Sources = []
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw "sto-1"), MatchFactor.Rank) ]
                    "a load worth carrying is worth completing first: rank decided"
            }

            test "the flow's own Refill outbids the stock's draw" {
                // The tier's shallow neighbour, and the price of ordering
                // the stock under the flow (ADR 0023): there is no rank
                // between a container's Withdraw and the spawn Refill it
                // feeds, so a stock one tier below the containers is a tier
                // below the spawn too. The hauler stands in the stock's own
                // Work Area with half a load and the hungry spawn is four
                // steps west — it carries what it has rather than topping
                // up first.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the spawn is fed before the stock is drawn on: rank decided"
            }

            test "both halves of the cycle pool on one tick; the tier gap closes it" {
                // What the Planner's gate does not do (ADR 0023): with a
                // sink other than the stock still hungry, a stocked Storage
                // with room pools its Withdraw and its Refill on the same
                // tick, and a part-loaded hauler beside it is applicable to
                // both. What keeps it out of the in-and-out cycle there is
                // the tier gap — the draw at the stock's shallow end, the
                // Refill at the deepest end of all — so it tops up and
                // carries the load away instead of putting it back.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let tasks = planTasks colony noThreats

                Expect.contains (withdrawTasks tasks) "sto-1" "the stock is an intake this tick"
                Expect.contains (refillTasks tasks) "sto-1" "and a sink on the very same tick"

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "sto-1"), MatchFactor.Rank) ]
                    "the draw outranks the load's way back in: rank decided"
            }

            test "the containers dry, the hauler draws on the stock for the spawn" {
                // What the stock is for (ADR 0023): the sources cannot
                // feed the spawn, so the stock does. The hauler already
                // stands beside it, and the ordinary withdraw Intent and the
                // ordinary bubble serve the draw — no Intent of the stock's
                // own, no glyph of its own.
                let colony =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 16; Y = 10 }

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        { colony with
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "sto-1")))
                    "with nothing in the containers the stock is the intake"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "sto-1"))
                    "the ordinary withdraw Intent serves the Storage"

                Expect.contains intents (SayCreep("h1", "📥")) "the ordinary inbox bubble shows it"

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a stock draw speaks the Verdicts every other Withdraw speaks"
            }

            test "with only the buffer hungry, the stock flows to it and never back" {
                // The other half of the ADR 0019 question, with the stock
                // standing where the buffer stood: the hauler draws on the
                // stock because the buffer has room, and the tick it fills
                // up the buffer outranks the store it just emptied — so the
                // pair alternates instead of cycling, exactly as the source
                // containers and the buffer do.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 800; "sto-1", 500 ]
                let beside = { X = 16; Y = 10 }

                let empty =
                    decide
                        (drawColony stores (creepWith "h1" 0 100 [ Carry; Carry; Move ]) beside)
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" empty.Assignments)
                    (Some(taskId (Withdraw "sto-1")))
                    "the buffer's own hunger is what opens the stock"

                let filled =
                    decide
                        (drawColony stores (creepWith "h1" 100 0 [ Carry; Carry; Move ]) beside)
                        (Map.ofList [ "h1", taskId (Withdraw "sto-1") ])
                        Set.empty
                        None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released("h1", taskId (Withdraw "sto-1"), ReleaseReason.Inapplicable))
                    "the full store ends the draw, as it ends every other one"

                Expect.contains
                    filled.Verdicts
                    (Verdict.Matched("h1", taskId (Refill "can-ctrl"), MatchFactor.Rank))
                    "and the load goes on to the buffer, not back into the stock"
            }

            test "beside a stock that is both its intake and its sink, a hauler idles" {
                // The ADR 0019 loop in the shape no body gate could cure —
                // the bodies that feed the spawn from the stock are the ones
                // with no Work part — and the gate that closes it: with
                // every other sink full the stock's Withdraw is not pooled
                // at all, so the hauler that would have emptied and refilled
                // one store tick after tick sits still instead. Idling is
                // the honest state; the stock holds energy the colony has
                // nowhere to put.
                let idleOn stores =
                    decide
                        (drawColony
                            stores
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            { X = 16; Y = 10 })
                        Map.empty
                        Set.empty
                        None

                let withRoom = idleOn (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ])

                Expect.equal
                    (Map.tryFind "h1" withRoom.Assignments)
                    None
                    "a stock that is its own only sink offers no intake"

                Expect.contains
                    withRoom.Verdicts
                    (Verdict.Unassigned("h1", IdleReason.NoneApplicable))
                    "the idle Verdict is the one ADR 0019 left behind"

                let brimming =
                    idleOn (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (Map.tryFind "h1" brimming.Assignments)
                    None
                    "a stock with no room left is no different: still nowhere to carry to"
            }

            test "a Work body draws on the same terms; a Work-heavy body never does" {
                // Nothing about the stock is body-specific (ADR 0023): the
                // ordinary Withdraw gate is the whole rule, so a worker
                // takes the stock exactly as a hauler does, and ADR 0016's
                // comparative gate keeps the Anchor row out of it. The
                // empty buffer is the sink that opens the draw, and holds
                // nothing either body could prefer to it.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ]

                let colonyFor creep =
                    { drawColony stores creep { X = 16; Y = 10 } with
                        Sources = []
                    }

                let worked = decide (colonyFor (worker "w1" 0 50)) Map.empty Set.empty None

                Expect.equal
                    worked.Verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a Work part is neither a bar to the stock nor a ticket to it"

                let heavy = decide (colonyFor (anchor "a1" 0 50)) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" heavy.Assignments)
                    None
                    "a Work-heavy body's intake is digging, whatever the stock holds"

                Expect.contains
                    heavy.Verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "the idle Verdict names the body gate, as ADR 0016 left it"
            }

            test "the tick the last other sink fills, the holder is released task-gone" {
                // The ADR 0013 shape (ADR 0023): the Task exists while the
                // condition holds and is gone otherwise, so a hauler
                // mid-trip is released through the path every vanishing Task
                // already uses — the stock needs no release reason of its
                // own.
                let colonyWithBuffer buffer =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", buffer; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 13; Y = 10 }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "sto-1") ]

                let hungry = decide (colonyWithBuffer 800) remembered Set.empty None

                Expect.contains
                    hungry.Verdicts
                    (Verdict.Kept("h1", taskId (Withdraw "sto-1")))
                    "while one sink still has room the trip stands"

                let filled = decide (colonyWithBuffer 2000) remembered Set.empty None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released("h1", taskId (Withdraw "sto-1"), ReleaseReason.TaskGone))
                    "the tick it fills, the walk it was on is over"
            }

            test "the accepted churn: a load the buffer will not take goes back to the stock" {
                // ADR 0023 accepts one load of this rather than remembering
                // where a load was drawn from. The hauler filled from the
                // stock while the buffer was hungry and the buffer filled
                // while it walked: its Refill is gone, the stock is the only
                // sink left, and the remainder goes back where it came from
                // rather than nowhere at all.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ]
                let loaded = creepWith "h1" 100 0 [ Carry; Carry; Move ]

                let arrived =
                    decide
                        (drawColony stores loaded { X = 20; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill "can-ctrl") ])
                        Set.empty
                        None

                Expect.contains
                    arrived.Verdicts
                    (Verdict.Released("h1", taskId (Refill "can-ctrl"), ReleaseReason.TaskGone))
                    "the buffer filled while the hauler walked to it"

                Expect.equal
                    (Map.tryFind "h1" arrived.Assignments)
                    (Some(taskId (Refill "sto-1")))
                    "the stock is the one sink left: the load turns around"

                let back =
                    decide
                        (drawColony stores loaded { X = 16; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill "sto-1") ])
                        Set.empty
                        None

                Expect.contains
                    back.Intents
                    (TransferEnergyToStructure("h1", "sto-1"))
                    "the ordinary transfer puts the remainder back: nothing is dropped"
            }
        ]

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
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
                        Spatial =
                            haulRoom
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
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 12; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 18; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                                })
                    }

                let remembered =
                    Map.ofList [ "a1", taskId (Harvest "src-a"); "a2", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity))
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 11; Y = 10 }; "w2", { X = 9; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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

                let { Assignments = both } = decide unposted Map.empty Set.empty None

                Expect.equal
                    (harvesters both "src-a")
                    [ "w1"; "w2" ]
                    "with no Post every Seat is a light body's"
            }

            test "the crowd a Post's Seat is kept from is every body but the garrison" {
                // ADR 0051's cap is over a **group** and not over one row
                // (ADR 0052 decision 6, `Capacity.Commuters`): the Seats
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

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 9; Y = 10 }; "a1", { X = 12; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 9; Y = 10 }; "w2", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity))
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "a2", { X = 9; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "a2" ]
                    "no Post derives no Post cap: both Seats are open"
            }
        ]

/// The restock dispatch corridor: a one-tile lane y = 10 from x = 9 to
/// x = 21 with the source embedded in wall at (10,10), so its Seats are
/// (9,10) and (11,10) and the lane east of the source is the only approach
/// to either. An empty worker-unit body pays a whole tick per plain step,
/// so a creep at (15,10) is four steps and a walk of four ticks from the
/// Seat it can reach (ADR 0029).
let restockRoom =
    spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ]
    |> withTargets [ "src-a", { X = 10; Y = 10 }, Source ]

/// The corridor with its one source the given number of ticks from its
/// restock, and one empty creep standing in the lane. The controller is
/// unplaced and its Upgrade is inapplicable to an empty body, so Harvest
/// is the only Task a creep in the lane can hold.
let restockAt name pos ticks =
    { bareRespawn with
        Sources = [ drained "src-a" ticks ]
        Creeps = [ worker name 0 50 ]
        Spatial =
            restockRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ name, pos ]
                })
    }

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
                    decide snapshot Map.empty Set.empty None

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
                    decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            snapshot.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.singleton { X = 14; Y = 10 }
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                        Spatial =
                            snapshot.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 16; Y = 10 }
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

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

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
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 60)))
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

                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(4, 60)))
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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

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
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Intents = intents } = decide snapshot remembered Set.empty None

                Expect.isEmpty
                    (actionIntents intents)
                    "no dig Intent until the energy is there to dig"

                let restocked =
                    { snapshot with
                        Sources = [ source "src-a" ]
                    }

                let { Intents = intents } = decide restocked remembered Set.empty None

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
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 60)))
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
                            snapshot.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.ofList [ for x in 11..21 -> { X = x; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } =
                    decide (pavedAt { X = 19; Y = 10 } 8) Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the walk equals the wait: it leaves now and arrives as the energy does"

                let { Assignments = assignments } =
                    decide (pavedAt { X = 19; Y = 10 } 9) Map.empty Set.empty None

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

                    let { Assignments = assignments } = decide snapshot Map.empty Set.empty None
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

let crowdRoom nearStock farStock =
    { spatial [] crowdField with
        Stores = Map.ofList [ "can-near", nearStock; "can-far", farStock ]
    }
    |> withTargets
        [
            "can-near", { X = 10; Y = 10 }, Structure BuiltKind.Container
            "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
        ]

/// The crowding colony: a 600-capacity bank, where the hauler row casts
/// `[8 Carry; 4 Move]` and one trip is therefore exactly 400 energy — the
/// number every stock below is written against. Its creeps are empty
/// hauler bodies on the tiles given, and it has no source and no placed
/// controller, so the only Tasks a Carry-only body is applicable to are the
/// two Withdraws.
let crowdColony nearStock farStock (creeps: (string * Pos) list) =
    { bareRespawn with
        Bank = bank 600 600
        Sources = []
        Creeps = [ for name, _ in creeps -> hauler name 0 100 ]
        Spatial =
            crowdRoom nearStock farStock
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList creeps
                })
    }

/// Three empty haulers abreast, one step from the near store's Work Area
/// and equally far from it, so nothing but the Matcher's own order can
/// separate them.
let crowdOfThree =
    [ "h1", { X = 12; Y = 9 }; "h2", { X = 12; Y = 10 }; "h3", { X = 12; Y = 11 } ]

/// The names drawing on one store, in name order.
let drawersOf assignments storeId =
    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> if tid = taskId (Withdraw storeId) then Some name else None)

/// The stock-crowding fixture: the same field with one Storage standing at
/// (13,10) — an obstacle, as the projection carries a built one — and the
/// same three haulers abreast, all three inside its Work Area. The colony keeps one hungry spawn
/// so ADR 0023's gate stands open; the haulers are empty, so that Refill is
/// inapplicable to every one of them and the stock's Withdraw is the only
/// Task in the pool they can take.
let stockCrowdColony stock =
    { bareRespawn with
        Bank = bank 600 600
        Sources = []
        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
        Creeps = [ for name, _ in crowdOfThree -> hauler name 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "sto-c", stock ]
            }
            |> withTargets [ "sto-c", { X = 13; Y = 10 }, Structure BuiltKind.Storage ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.singleton { X = 13; Y = 10 }
                    CreepPositions = Map.ofList [ for name, pos in crowdOfThree -> name, pos ]
                })
    }

/// The upgrade buffer's crowd (#161 under ADR 0019): the same three-row
/// field, the controller standing at (10,10) — an obstacle, as a
/// projected one is — with its buffer container "can-buf" at (12,10),
/// inside the Upgrade Work Area and on no source's Seat, and an ordinary
/// container "can-far" holding the same 900 at (30,10), far outside it.
///
/// The bank is 1,800 — the live RCL5 one, and where the two rows part: the
/// cast hauler carries 1,200 a trip and the cast worker 450. Only a Work
/// body may draw from the buffer (ADR 0019), so the three creeps on its
/// doorstep are cast worker bodies, and they are empty, which leaves the
/// Upgrade beside them and the buffer's own Refill inapplicable and the
/// two Withdraws the whole of the pool they can take.
let bufferCrowd =
    [ "w1", { X = 13; Y = 9 }; "w2", { X = 13; Y = 10 }; "w3", { X = 13; Y = 11 } ]

let bufferCrowdColony bufferStock =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Creeps = [ for name, _ in bufferCrowd -> creepWith name 0 450 (workerBodyFor 1800) ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "can-buf", bufferStock; "can-far", 900 ]
            }
            |> withTargets
                [
                    "ctrl-1", { X = 10; Y = 10 }, Controller
                    "can-buf", { X = 12; Y = 10 }, Structure BuiltKind.Container
                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.singleton { X = 10; Y = 10 }
                    CreepPositions = Map.ofList bufferCrowd
                })
    }

[<Tests>]
let withdrawCapacityTests =
    testList
        "withdraw capacity"
        [
            test "a container that fills one hauler takes one; the rest walk to the full one" {
                // The defect (#161): the matching key puts cost ahead of
                // `load` (ADR 0002), so without a capacity every empty
                // hauler picks the *nearest* stocked container whatever is
                // in it — three bodies onto 400 energy, two of them home
                // empty, while 1,800 stands unvisited seventeen tiles away.
                // The stock is the cap: `ceil(400 / 400)` is one seat. The
                // far store is 1,800 and not a full 2,000: a full source
                // container is lifted a rung of its own (its overflow is
                // going to the ground), and this test is about the cap.
                let { Assignments = split } =
                    decide (crowdColony 400 1800 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (drawersOf split "can-near")
                    [ "h1" ]
                    "one hauler's worth of stock admits one hauler"

                Expect.equal
                    (drawersOf split "can-far")
                    [ "h2"; "h3" ]
                    "and the crowd it turns away walks to the store that can fill it"

                // The pairwise control: the same three creeps on the same
                // tiles, with nothing changed but the near store's stock.
                // Travel cost still says near for all three, and now the
                // capacity lets it — so the split above is the stock's
                // doing and not the geometry's.
                let { Assignments = whole } =
                    decide (crowdColony 2000 2000 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (drawersOf whole "can-near")
                    [ "h1"; "h2"; "h3" ]
                    "stocked for five trips, the near container keeps the whole crowd"
            }

            test "the cap rounds up: one load exactly is one seat, one energy more is two" {
                // The `ceil` (#161), pinned at the boundary the arithmetic
                // turns on: 400 is exactly the cast hauler's load and admits
                // one body, and 401 — a fraction of a second trip — admits
                // the second, because the fraction a floor would drop is
                // energy nobody would be sent for.
                let seatsAt stock =
                    let { Assignments = assignments } =
                        decide
                            (crowdColony stock 1800 (List.truncate 2 crowdOfThree))
                            Map.empty
                            Set.empty
                            None

                    drawersOf assignments "can-near"

                Expect.equal (seatsAt 400) [ "h1" ] "one whole load is one seat"

                Expect.equal
                    (seatsAt 401)
                    [ "h1"; "h2" ]
                    "one energy past it is two: the cap rounds up"

                Expect.equal
                    (seatsAt 800)
                    [ "h1"; "h2" ]
                    "and two whole loads are two, with no third body to prove it wider"
            }

            test "a hauler still walking holds its seat: the second is turned away" {
                // Counted at arrival like every other cap (ADR 0026): the
                // holder is fourteen steps out and has not touched the
                // store, and the candidate is standing on its doorstep. A
                // cap counting only the creeps already on the tile would let
                // the near one in and land both on 400 energy — which is the
                // defect with an extra tick in it.
                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        (crowdColony 400 2000 [ "h1", { X = 25; Y = 10 }; "h2", { X = 11; Y = 10 } ])
                        (Map.ofList [ "h1", taskId (Withdraw "can-near") ])
                        (Set.singleton "h2")
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-near")))
                    "the walking holder keeps the store it was already sent to"

                Expect.equal
                    (drawersOf assignments "can-far")
                    [ "h2" ]
                    "and the creep on the doorstep is sent to the far store instead"

                let rejections =
                    verdicts
                    |> List.tryPick (function
                        | Verdict.Scoring("h2", rows) ->
                            rows
                            |> List.filter (function
                                | Candidate.Rejected _ -> true
                                | Candidate.Scored _ -> false)
                            |> Some
                        | _ -> None)

                Expect.equal
                    rejections
                    (Some
                        [
                            Candidate.Rejected(
                                taskId (Withdraw "can-near"),
                                RejectReason.CapacityFull
                            )
                            Candidate.Rejected(taskId (Upgrade "ctrl-1"), RejectReason.Inapplicable)
                        ])
                    "the near store names the cap and no gate before it — not the body, not the price; the Upgrade it has no Work for is the pool's only other loss"
            }

            test "the Storage is not special-cased: the same formula, and at 130k no cap" {
                // ADR 0023's stock is one more store and gets one more
                // reading of the same rule (#161) — a Storage down to one
                // trip's worth admits one drawer, exactly as a container
                // does. What keeps that from starving the haul cycle is the
                // number and not an exemption: a real stock divides into
                // hundreds of trips, so the cap is there and is never the
                // thing that binds.
                let { Assignments = thin } = decide (stockCrowdColony 400) Map.empty Set.empty None

                Expect.equal
                    (drawersOf thin "sto-c")
                    [ "h1" ]
                    "a stock holding one trip's worth admits one hauler"

                let { Assignments = full } =
                    decide (stockCrowdColony 130000) Map.empty Set.empty None

                Expect.equal
                    (drawersOf full "sto-c")
                    [ "h1"; "h2"; "h3" ]
                    "and a colony's real stock caps at 325 trips, which is no cap at all"
            }

            test "the upgrade buffer divides by the worker row that draws from it" {
                // Which row draws is a fact about the store (ADR 0019): no
                // body without a Work part may take the buffer, so its
                // drawers are the worker row and its 900 is two cast
                // workers' loads at this bank. Priced by the hauler the
                // colony would cast instead — 1,200 a trip — the same 900
                // reads `ceil(900 / 1200)` = one seat and sends the second
                // upgrader back to a rock while the energy it came to
                // spend stands beside it (#161).
                let { Assignments = split } =
                    decide (bufferCrowdColony 900) Map.empty Set.empty None

                Expect.equal
                    (drawersOf split "can-buf")
                    [ "w1"; "w2" ]
                    "two worker loads standing in the buffer admit two workers"

                // The same 900 in a store the haul cycle owns, judged for
                // the same three bodies: the divisor is the store's and
                // never the candidate's, so the ordinary container admits
                // one and takes the worker the buffer turned away.
                Expect.equal
                    (drawersOf split "can-far")
                    [ "w3" ]
                    "and an ordinary container's 900 is one hauler load, however the body that walks to it is built"

                // The pairwise control: nothing changed but the buffer's
                // stock, three loads instead of two.
                let { Assignments = whole } =
                    decide (bufferCrowdColony 1350) Map.empty Set.empty None

                Expect.equal
                    (drawersOf whole "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "three loads keep the whole crowd upgrading standing still, which is what a buffer is for"
            }

            test "the buffer's two rows are capped apart, each by its own load" {
                // #196, landed as ADR 0052 decision 6's per-[[body class]]
                // capacity. One store, two rows, two loads: the generalist
                // the colony casts at this bank carries 450 and the
                // [[standing body]] beside the buffer carries fifty, so a
                // 400-energy buffer is one trip for the first and eight for
                // the second. Divided by the generalist's load alone — the
                // one number the store used to answer — the row #187 hired
                // to *live* at that store took one seat between the three
                // of them and the rest walked to a rock they are the worst
                // body in the colony at digging.
                //
                // Pairwise on the class of the three bodies and nothing
                // else: the same tiles, the same 400, the same pool.
                let standingCrowd =
                    { bufferCrowdColony 400 with
                        Creeps =
                            [
                                for name, _ in bufferCrowd ->
                                    creepWith name 0 50 (bodyFor upgraderPattern 1800)
                            ]
                    }

                let { Assignments = standing } = decide standingCrowd Map.empty Set.empty None

                Expect.equal
                    (drawersOf standing "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "fifty energy a trip divides 400 into eight seats, so the row that lives there all drinks"

                let { Assignments = generalists } =
                    decide (bufferCrowdColony 400) Map.empty Set.empty None

                Expect.equal
                    (drawersOf generalists "can-buf")
                    [ "w1" ]
                    "and the generalists' own share is unchanged: one load standing is one of them"
            }
        ]

/// The names assigned to one pile, in name order — `drawersOf`'s twin for
/// the Pickup Task (#167).
let pickersOf assignments pileId =
    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> if tid = taskId (Pickup pileId) then Some name else None)

/// The same field with a tombstone at (10,10) holding the given energy and
/// nothing else standing anywhere (#167). Deliberately not in `Obstacles`:
/// a tombstone lies on the tile a creep died on and the engine lets
/// another walk over it, so its Work Area includes its own tile.
let tombColony energy (creeps: (string * Pos) list) =
    { bareRespawn with
        Bank = bank 150 150
        Sources = []
        Creeps = [ for name, _ in creeps -> hauler name 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "tomb-1", energy ]
            }
            |> withTargets [ "tomb-1", { X = 10; Y = 10 }, Tombstone ]
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList creeps
                })
    }

/// A stocked container at (10,10) with a dropped pile the case places —
/// on the container's own tile or ten tiles down the lane — and one empty
/// hauler on the tile beside the container (#216 R5). The bank is 150, so
/// a trip is 100 energy and both stores are stocked for several of them:
/// what separates the two Tasks here is neither capacity nor travel cost,
/// both of which tie, but the pool's own [[priority]].
let sameTilePileColony pilePos =
    { bareRespawn with
        Bank = bank 150 150
        Sources = []
        Creeps = [ hauler "h1" 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "can-a", 400; "pile-a", 150 ]
            }
            |> withTargets
                [
                    "can-a", { X = 10; Y = 10 }, Structure BuiltKind.Container
                    "pile-a", pilePos, Dropped
                ]
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "h1", { X = 10; Y = 11 } ]
                })
    }

[<Tests>]
let pickupTaskTests =
    testList
        "the pile and the tombstone"
        [
            test "a pile on a container's own tile is taken before the container" {
                // The live shape (user, 2026-09-07): a hauler standing at a
                // full container ignored the 1,859 energy lying on it. The
                // two Tasks share the feeding tier, the tile and therefore
                // the travel cost, so pool order decided — and the pool
                // order had the container first. What separates them is
                // decay: the pile loses `ceil(amount / 1000)` a tick and the
                // container loses nothing, so the copy that is going away is
                // the one to take (ADR 0052 decision 6, the [[priority]]
                // ladder's own rung of slack).
                let { Assignments = together } =
                    decide (sameTilePileColony { X = 10; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" together)
                    (Some(taskId (Pickup "pile-a")))
                    "one tile, two stores: the decaying one first"

                // The pairwise control: the same hauler, the same container,
                // the same pile, ten tiles down the lane. The Withdraw is on
                // its own tier again — the step is a claim about *this*
                // store and never about piles in general — and travel cost
                // says what it always said.
                let { Assignments = apart } =
                    decide (sameTilePileColony { X = 20; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" apart)
                    (Some(taskId (Withdraw "can-a")))
                    "a pile ten tiles off moves nothing: the container underfoot is still the flow"

                // And a *full* container outranks the pile on its own tile
                // (live, W12S28 2026-09-07): the garrison is overflowing, the
                // 2,000 is the flow, and the pickup reflex takes the pile
                // off the same tile for free while the body draws. Pairwise
                // on the stock alone.
                let full =
                    let colony = sameTilePileColony { X = 10; Y = 10 }

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores =
                                    Map.add "can-a" Engine.containerCapacity colony.Spatial.Stores
                            }
                    }

                let { Assignments = brimming } = decide full Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" brimming)
                    (Some(taskId (Withdraw "can-a")))
                    "full, the container is drawn first and the reflex takes the pile beside it"
            }

            test "the piled container keeps its place against every other store" {
                // Which of the pair carries the rung is not a matter of
                // taste (#216 R5 review). A [[priority]] is a scalar the
                // whole tier is ordered by and travel cost never overturns
                // it (ADR 0002), so stepping the *Withdraw* down put that
                // container behind every other store in the colony at any
                // distance — and the engine drops an [[anchor]]'s overflow
                // onto a container's tile only once the container is
                // **full**, so the demotion switched on exactly when the
                // store most needed emptying. Two haulers, a piled near
                // container and an unpiled far one: the first takes the
                // decaying copy, and the second must still take the four
                // hundred at its feet rather than walk twenty tiles past
                // it.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Creeps = [ hauler "h1" 0 100; hauler "h2" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores =
                                    Map.ofList [ "can-near", 400; "can-far", 400; "pile-a", 100 ]
                            }
                            |> withTargets
                                [
                                    "can-near", { X = 10; Y = 10 }, Structure BuiltKind.Container
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                    "pile-a", { X = 10; Y = 10 }, Dropped
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h1", { X = 10; Y = 11 }; "h2", { X = 14; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup "pile-a")))
                    "the pile's own hauler still takes the decaying copy first"

                // The pile's capacity is its amount over one load — one
                // body — so the second hauler is over capacity there and
                // has the two containers to choose between. Only travel
                // cost may decide that, which is the whole of the fix.
                Expect.equal
                    (Map.tryFind "h2" assignments)
                    (Some(taskId (Withdraw "can-near")))
                    "and the rest of the row is not sent past the full store beside it"
            }

            test "a pile past the threshold hires a hauler ten tiles off; one under it hires nobody" {
                // The live gap's second half (#167): 193 energy of death
                // drop at W13S28 36,21 with nobody near enough for the
                // reflex ever to reach it. A pile at or over the threshold
                // is a Task and gets walked to.
                let walk =
                    decide
                        (pileTaskColony 150 [ "h1", { X = 20; Y = 10 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" walk.Assignments)
                    (Some(taskId (Pickup "pile-a")))
                    "150 on the ground is worth ten tiles of walking"

                Expect.isEmpty
                    (pickups walk.Intents)
                    "and out of reach it is walking, not picking: the act waits for arrival"

                Expect.isNonEmpty
                    (moveIntentsFor "h1" walk.Intents)
                    "what a Task buys over the reflex is exactly this step"

                // The pairwise control: the same creep on the same tile
                // with the same everything, and 80 energy on the ground.
                let small =
                    decide (pileTaskColony 80 [ "h1", { X = 20; Y = 10 } ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" small.Assignments)
                    None
                    "under the threshold the pile is the reflex's business and nobody walks"
            }

            test "the threshold is inclusive: a hundred exactly is worth the trip" {
                // Where the tunable turns, pinned on both sides of it: two
                // CARRY parts' worth is the smallest load that pays for a
                // walk made for the pile alone.
                let assignmentAt amount =
                    let { Assignments = assignments } =
                        decide
                            (pileTaskColony amount [ "h1", { X = 20; Y = 10 } ])
                            Map.empty
                            Set.empty
                            None

                    Map.tryFind "h1" assignments

                Expect.equal
                    (assignmentAt 100)
                    (Some(taskId (Pickup "pile-a")))
                    "at the line, pooled"

                Expect.equal (assignmentAt 99) None "one energy short of it, not"
            }

            test "the pile that arrives is picked up once, and the bubble says so" {
                // The Task's own action Intent, at range 1 where the Atlas
                // permits it. The reflex asks for the same act on this
                // tick — its rule is the same range and the same free
                // capacity — so the count is the assertion and not the
                // membership: an arriving picker satisfies both producers,
                // and one creep's one pickup spelt twice would over-report
                // the CPU line's accepted-intent column tick after tick
                // (#167). Two *adjacent creeps* both reaching for one pile
                // stay two asks; this is one creep asking twice.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide
                        (pileTaskColony 150 [ "h1", { X = 10; Y = 11 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup "pile-a")))
                    "standing on its doorstep it still holds the Task"

                Expect.equal
                    (pickups intents)
                    [ "h1", "pile-a" ]
                    "it asks the engine for it, exactly once between the Task and the reflex"

                Expect.contains
                    intents
                    (SayCreep("h1", "🧲"))
                    "one glyph per Task, and this Task has its own"
            }

            test
                "the creep beside a hired picker still asks: the pair is deduplicated, not the pile" {
                // The other side of the count above (#167): what the
                // deduplication drops is one creep's own Intent spelt
                // twice, and nothing else. Two haulers stand at one pile
                // of exactly a hundred, so its capacity is one body: h1 is
                // hired and h2 is not, and h2's reflex pickup is energy
                // the colony recovers for free. A filter written over the
                // pile rather than over the (creep, pile) pair would drop
                // it — which is why the assertion is both names and not a
                // count.
                let { Intents = intents } =
                    decide
                        (pileTaskColony 100 [ "h1", { X = 10; Y = 11 }; "h2", { X = 11; Y = 10 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "h1", "pile-a"; "h2", "pile-a" ]
                    "one ask apiece: the hired picker's own, and the bystander's reflex"
            }

            test "a pile decaying under the threshold releases the hauler still walking to it" {
                // The accepted loss, pinned so it stays a decision
                // (`Tuning.PickupThreshold`, #167). The threshold gates
                // persistence as well as entry, because the pool is
                // rebuilt creep-blind every tick: a pile at 100 holds its
                // holder, and the same pile one energy lighter — a
                // hundredth of the decay a pile spends on its own, or the
                // first of two hired haulers arriving — is gone, and the
                // walk already spent bought nothing.
                let held = Map.ofList [ "h1", taskId (Pickup "pile-a") ]

                let standing =
                    decide (pileTaskColony 100 [ "h1", { X = 20; Y = 10 } ]) held Set.empty None

                Expect.contains
                    standing.Verdicts
                    (Verdict.Kept("h1", taskId (Pickup "pile-a")))
                    "at the line the walk stands"

                let decayed =
                    decide (pileTaskColony 99 [ "h1", { X = 20; Y = 10 } ]) held Set.empty None

                Expect.contains
                    decayed.Verdicts
                    (Verdict.Released("h1", taskId (Pickup "pile-a"), ReleaseReason.TaskGone))
                    "one energy under it, ten tiles from home, and the trip is over"
            }

            test "the pile's amount is its capacity: 150 admits two of the three haulers" {
                // The Withdraw rule over a pile (#161 read by #167):
                // `ceil(150 / 100)` is two bodies, and travel cost cannot
                // thin the crowd because all three stand one step from the
                // pile's Work Area.
                let { Assignments = split } =
                    decide (pileTaskColony 150 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (pickersOf split "pile-a")
                    [ "h1"; "h2" ]
                    "one and a half loads on the ground hire two haulers"

                // The pairwise control: the same three creeps on the same
                // tiles, nothing changed but the amount.
                let { Assignments = whole } =
                    decide (pileTaskColony 300 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (pickersOf whole "pile-a")
                    [ "h1"; "h2"; "h3" ]
                    "three loads take the whole crowd"
            }

            test "a Work-heavy body never picks a pile up (ADR 0016)" {
                // The gate that keeps an Anchor at its rock, read over the
                // ground as well as over a container: a heavy body's
                // intake is digging, and a pile is not a dig. Pairwise on
                // the body alone — the same parts, one Move more.
                let bodied body =
                    let colony = pileTaskColony 150 [ "a1", { X = 12; Y = 10 } ]

                    { colony with
                        Creeps = [ creepWith "a1" 0 50 body ]
                    }

                let { Assignments = heavy } =
                    decide (bodied [ Work; Work; Carry; Move ]) Map.empty Set.empty None

                Expect.equal (Map.tryFind "a1" heavy) None "more Work than Move: no Pickup"

                let { Assignments = balanced } =
                    decide (bodied [ Work; Work; Carry; Move; Move ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" balanced)
                    (Some(taskId (Pickup "pile-a")))
                    "the same body at Work <= Move picks it up"
            }

            test "a tombstone is a store: its 408 is withdrawn, and an empty one pools nothing" {
                // The live gap's first half (#167): 408 energy standing in
                // a tombstone in the home room while the colony dug. The
                // Intent is the container's own — the engine's `withdraw`
                // is one method over every store.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide (tombColony 408 [ "h1", { X = 11; Y = 10 } ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "tomb-1")))
                    "a store with a clock on it is drawn like any other"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "tomb-1"))
                    "and the act is withdraw, never pickup"

                // The pairwise control: the same tombstone on the same
                // tile, drawn dry.
                let { Assignments = spent } =
                    decide (tombColony 0 [ "h1", { X = 11; Y = 10 } ]) Map.empty Set.empty None

                Expect.equal (Map.tryFind "h1" spent) None "an empty store is no Task"
            }

            test "a tombstone keeps no construction site off its tile" {
                // Layout determinism (ADR 0011), the rule the piles already
                // had (#167): a tombstone stands wherever a creep happened
                // to die, and a plan that moved with it would be a function
                // of that accident.
                let bare = atLevel 2 (openRoom 3)

                let littered =
                    atLevel
                        2
                        (openRoom 3 |> withTargets [ "tomb-1", { X = 24; Y = 24 }, Tombstone ])

                let placedWith = decide littered Map.empty Set.empty None
                let placedWithout = decide bare Map.empty Set.empty None

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see tombstones"
            }

            test "a pile ties a container: one tier, and only cost between them" {
                // The tier (#167): a pile is the haul cycle's own energy
                // lying where it fell, so it feeds on the containers' tier
                // and the choice between the two is travel cost's. Equal
                // cost is the way to read that off one match — a rank
                // either way would have decided it before the price was
                // asked, and the factor says which happened.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 150; "can-far", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 10; Y = 10 }, Dropped
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 20; Y = 10 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-far"), MatchFactor.PoolOrder) ]
                    "ten tiles either way: pool order broke the tie, not rank"
            }

            test "a pile outranks the stock underfoot" {
                // The other half of the tier, and the one that has a rank
                // in it (ADR 0023): the Storage is drawn a tier below the
                // flow, so a pile sixteen tiles away beats a stock the
                // creep is standing beside. A pile decays at a thousandth
                // a tick and a stock does not, which is the reason the
                // ordering is right as well as inherited.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 300; "sto-c", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 30; Y = 10 }, Dropped
                                    "sto-c", { X = 13; Y = 10 }, Structure BuiltKind.Storage
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 13; Y = 10 }
                                    CreepPositions = Map.ofList [ "h1", { X = 14; Y = 10 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Pickup "pile-a"), MatchFactor.Rank) ]
                    "the feeding tier beats the stock draw whatever the distance"
            }

            test "an outpost's pile pools by the rule the home room's does" {
                // The declared outpost is a room of the projection like any
                // other (ADR 0041, ADR 0042): the pool is read off the kind
                // census and the amount, neither of which knows a border.
                // The home pile keeps its own coordinate and no amount, so
                // it stays the reflex's and proves the pairing is not
                // crossing (#166).
                let colony =
                    pileColony [ hauler "h-out" 0 100 ] []
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "h-out", { X = 12; Y = 10 } ]

                let snapshot =
                    { colony with
                        Bank = bank 150 150
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.ofList [ "pile-out", 150 ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (pickersOf assignments "pile-out")
                    [ "h-out" ]
                    "the outpost's pile hires the hauler standing in the outpost"

                Expect.contains
                    intents
                    (SayCreep("h-out", "🧲"))
                    "and it walks under the Pickup glyph"

                Expect.isEmpty
                    (pickups intents)
                    "two tiles out: no reflex, and no action Intent until it arrives"
            }
        ]

[<Tests>]
let fullContainerTests =
    testList
        "a full source container"
        [
            test "a full source container is drawn before a half-full one nearer to hand" {
                // One lane, two posted sources: can-a three tiles from the
                // hauler at 1,000, can-b twelve tiles away at 2,000 (full,
                // so its garrison's overflow is going to the ground). The
                // full one wins by rank; pairwise on can-b's stock alone,
                // at 1,000 the near one wins by travel cost, which is the
                // nearest-first dispatch that let W13S28's north container
                // overflow for hours (#198).
                let lane stockB =
                    let room =
                        { spatial
                              []
                              [
                                  for x in 9..28 ->
                                      { X = x; Y = 10 }, (if x = 10 || x = 27 then Wall else Plain)
                              ] with
                            Stores = Map.ofList [ "can-a", 1000; "can-b", stockB ]
                        }
                        |> withTargets
                            [
                                "src-a", { X = 10; Y = 10 }, Source
                                "can-a", { X = 11; Y = 10 }, Structure BuiltKind.Container
                                "src-b", { X = 27; Y = 10 }, Source
                                "can-b", { X = 26; Y = 10 }, Structure BuiltKind.Container
                            ]
                        |> withHome (fun layer ->
                            { layer with
                                CreepPositions = Map.ofList [ "h", { X = 14; Y = 10 } ]
                            })

                    { bareRespawn with
                        Sources = [ source "src-a"; source "src-b" ]
                        Refillables = []
                        Controller = None
                        Creeps = [ creepWith "h" 0 200 [ Carry; Carry; Carry; Carry; Move; Move ] ]
                        Spatial = room
                    }

                let matched stockB =
                    let { Verdicts = verdicts } = decide (lane stockB) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched 2000)
                    (Some(taskId (Withdraw "can-b"), MatchFactor.Rank))
                    "the full container outranks the near one"

                Expect.equal
                    (matched 1000)
                    (Some(taskId (Withdraw "can-a"), MatchFactor.TravelCost))
                    "both half-full: the near one, by travel cost"
            }
        ]
