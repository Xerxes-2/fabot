/// Seat capacity, the Refill cluster that is one Task (ADR 0054), the
/// targets nothing reaches, and the Repairs.
module Fabot.Core.Tests.Decide.PoolSeatTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.PoolFixtures

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
                    decideOn snapshot

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

                let { Assignments = assignments } = decideOn snapshot

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

                let { Assignments = assignments } = decideOn snapshot

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

                let { Assignments = assignments } = decideOn snapshot

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

                let { Assignments = assignments } = decideFrom stale snapshot

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

                let { Assignments = assignments } = decideOn snapshot

                Expect.hasLength
                    (harvesters assignments "src-a")
                    3
                    "no terrain data means no cap — today's room behaviour"
            }
        ]

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
                    let { Assignments = assignments } = decideOn (colony free)

                    holdersOf (Refill("spawn-1", Energy)) assignments

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

                let sticky = Map.ofList [ "h1", taskId (Refill("spawn-1", Energy)) ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom sticky (walking (0, 50, 0))

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill("spawn-1", Energy))))
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

                let sticky = Map.ofList [ "h1", taskId (Refill("spawn-1", Energy)) ]

                let { Verdicts = verdicts } = decideFrom sticky full

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Refill("spawn-1", Energy)),
                        ReleaseReason.TaskGone
                    ))
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

                let { Intents = northIntents } = decideOn (arrived (0, 50, 0))

                let { Intents = southIntents } = decideOn (arrived (0, 0, 50))

                Expect.contains
                    northIntents
                    (TransferEnergyToStructure("h1", "ext-1", Energy))
                    "ext-2 is full, so the load goes into the extension that is not"

                Expect.contains
                    southIntents
                    (TransferEnergyToStructure("h1", "ext-2", Energy))
                    "and the other way round, so it is room and not id order deciding"
            }

            test "a load the ring no longer has room for is released capacity-full" {
                // The price ADR 0054 records rather than removes. The cap
                // is `ceil(free / one load)` and the ring's free energy
                // only falls, so on the tick it crosses a load boundary one
                // of the bodies aimed at the ring is released — and since
                // #230 it is the body **furthest** from the ring, not the
                // one whose name sorts later. `h2` is standing beside the
                // spawn with a full store and pours this tick; `h1` is seven
                // tiles down the column and keeps nothing.
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
                    Map.ofList
                        [
                            "h1", taskId (Refill("spawn-1", Energy))
                            "h2", taskId (Refill("spawn-1", Energy))
                        ]

                let outcome free =
                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decideFrom sticky (colony free)

                    holdersOf (Refill("spawn-1", Energy)) assignments, verdicts

                // Four hundred of room is two of the 300 bank's 200-energy
                // loads, so both bodies keep what they hold.
                Expect.equal
                    (fst (outcome (300, 100, 0)))
                    [ "h1"; "h2" ]
                    "two loads' worth of room holds two bodies"

                // One load poured into the ring, and the second body's load
                // is one too many for what is left.
                let holders, verdicts = outcome (200, 0, 0)

                Expect.equal holders [ "h2" ] "one load's worth of room holds the body that arrived"

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Refill("spawn-1", Energy)),
                        ReleaseReason.Rejected RejectReason.CapacityFull
                    ))
                    "and the walker is released capacity-full, being the one furthest from the ring"
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
                            |> withCreepsAt [ "w1", { X = 20; Y = 20 }; "w2", { X = 10; Y = 12 } ]
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decideFrom sticky snapshot

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
                            |> withCreepsAt [ "w1", { X = 20; Y = 20 } ]
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideFrom sticky snapshot

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
                            |> withCreepsAt [ "w1", { X = 20; Y = 20 } ]
                    }

                let sticky = Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ]
                let { Assignments = assignments } = decideFrom sticky snapshot

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
                let { Assignments = assignments } = decideFrom sticky snapshot

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
                    (repairTasks (planTasksOn low noThreats))
                    [ "road-1" ]
                    "below the trigger: one Repair per ailing road"

                Expect.isEmpty
                    (repairTasks (planTasksOn half noThreats))
                    "at half hits the road is left alone"
            }

            test "a road over the hungry line stays pooled while a creep holds its Repair" {
                // The two lines (ADR 0061): a repair tick is `Work × 100` hits
                // whatever the structure's max, so one line makes every repair
                // a one-tick top-up that goes `task-gone` the tick after it
                // started and leaves the paving pinned at the line. The held
                // fact — one boolean per candidate, off the assignment table —
                // picks which line this structure is judged by.
                let at hits =
                    bareRespawn |> withHits "road-1" BuiltKind.Road hits 5000

                Expect.equal
                    (repairTasks (planTasksOn (at 2400) noThreats))
                    [ "road-1" ]
                    "under the hungry line, nobody holding: pooled, as it always was"

                Expect.isEmpty
                    (repairTasks (planTasksOn (at 2600) noThreats))
                    "over the hungry line, nobody holding: no Task — the entry is unchanged"

                Expect.equal
                    (repairTasks (planTasksHolding [ Repair "road-1" ] (at 2600)))
                    [ "road-1" ]
                    "the same road with a holder is judged at the whole line and stands"

                Expect.isEmpty
                    (repairTasks (planTasksHolding [ Repair "road-1" ] (at 4100)))
                    "and past the whole line even a held road is done: four fifths of max"
            }

            test "the held fact is spelled forward: a Withdraw on a container is no Repair on it" {
                // The seam's own property (ADR 0061 part 3): the set is read as
                // `Set.contains (taskId (Repair id))`, written from the
                // candidate id in hand and never parsed out of a string, so
                // "held" can never come to mean "somebody is drawing from it".
                let cont = bareRespawn |> withHits "cont-1" BuiltKind.Container 150000 250000

                Expect.isEmpty
                    (repairTasks (planTasksHolding [ Withdraw("cont-1", Energy) ] cont))
                    "a body drawing energy out of the container is not repairing it"

                Expect.equal
                    (repairTasks (planTasksHolding [ Repair "cont-1" ] cont))
                    [ "cont-1" ]
                    "the Repair on the same id is the key that holds it"
            }

            test "the two lines reach the fraction-judged kinds and no others" {
                // ADR 0061 part 2: a rampart is judged against a floor and a
                // Keep structure against full hits, and neither has a second
                // number to make. Both would move if the fraction rule reached
                // them — four fifths of a rampart's three-million max is far
                // over its floor — so the pairwise is the whole test.
                let colony =
                    bareRespawn
                    |> withLevel 5
                    |> withHits "ram-1" BuiltKind.Rampart 150_000 3_000_000
                    |> withHits "sto-1" BuiltKind.Storage 1_000_000 1_000_000

                Expect.isEmpty
                    (repairTasks (planTasksOn colony noThreats))
                    "a rampart over its floor and a whole Storage ask for nothing"

                Expect.isEmpty
                    (repairTasks (planTasksHolding [ Repair "ram-1"; Repair "sto-1" ] colony))
                    "and holding either changes neither: the floor and full hits are one number each"

                // And the same pair from under their lines, where the held set
                // must not take a Task away either: a rampart under its floor
                // and a dented Keep structure are pooled identically with and
                // without a holder.
                let ailing =
                    bareRespawn
                    |> withLevel 5
                    |> withHits "ram-1" BuiltKind.Rampart 99_999 3_000_000
                    |> withHits "sto-1" BuiltKind.Storage 999_999 1_000_000

                Expect.equal
                    (repairTasks (planTasksHolding [ Repair "ram-1"; Repair "sto-1" ] ailing))
                    (repairTasks (planTasksOn ailing noThreats))
                    "one hit under the floor and one hit off full: the same pool either way"
            }

            test "the two-line rule is monotone: holding never empties the pool" {
                // ADR 0061 part 4. The held line only ever keeps a Task pooled
                // that would otherwise be gone, so no structure can leave the
                // pool *earlier* because somebody is repairing it — over every
                // hits value a road, a container, a rampart and a Keep
                // structure can carry, in hundredths of their own max.
                let kinds =
                    [
                        "road-1", BuiltKind.Road, 5000
                        "cont-1", BuiltKind.Container, 250000
                        "ram-1", BuiltKind.Rampart, 3_000_000
                        "sto-1", BuiltKind.Storage, 1_000_000
                    ]

                let holding = kinds |> List.map (fun (id, _, _) -> Repair id)

                let mutable widened = 0

                for step in 0..100 do
                    let colony =
                        kinds
                        |> List.fold
                            (fun snapshot (id, kind, max) ->
                                snapshot |> withHits id kind (max * step / 100) max)
                            (bareRespawn |> withLevel 5)

                    let unheld = repairTasks (planTasksOn colony noThreats) |> Set.ofList
                    let held = repairTasks (planTasksHolding holding colony) |> Set.ofList

                    Expect.isTrue
                        (Set.isSubset unheld held)
                        $"at {step} hundredths of max the unheld pool is a subset of the held one"

                    if held <> unheld then
                        widened <- widened + 1

                // A subset test alone passes a rule that does nothing at all,
                // so the sweep also says the two sets **differ** somewhere: the
                // band is thirty hundredths of the two fraction-judged kinds.
                Expect.equal
                    widened
                    30
                    "and the held pool is strictly wider across the band, not everywhere and not nowhere"
            }

            test "an assignment naming a dead creep holds nothing" {
                // The join is over the **living** (ADR 0061 part 3):
                // `Assignments` arrives from Memory and may name a creep that
                // died last tick. The Matcher drops those silently, but
                // `planTasksOn` runs first, and a colony must not hold a Task
                // open on the strength of a body that is not there.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 2600 5000

                let ghost = Map.ofList [ "ghost", taskId (Repair "road-1") ]

                Expect.equal
                    (Map.tryFind "w1" (decideFrom ghost snapshot).Assignments)
                    None
                    "the dead creep's Repair pools nothing, so the loaded worker has no work"

                let living = Map.ofList [ "w1", taskId (Repair "road-1") ]

                Expect.equal
                    (Map.tryFind "w1" (decideFrom living snapshot).Assignments)
                    (Some(taskId (Repair "road-1")))
                    "the same table read off a living body keeps the road pooled and its holder on it"
            }

            test
                "the ratchet is the assignment: a released holder leaves the road judged by its hits" {
                // ADR 0061 part 4, and the correction to `Pool.fs`'s `rescued`
                // comment: nothing in the Matcher holds a Repair to the whole
                // line. `applicable` is `spending && not standing`, so a body
                // that empties mid-repair is released `inapplicable` and its
                // target is unheld the next tick — judged at the hungry line
                // again, wherever the load ran out, with no memory of the
                // half-finished job anywhere.
                let emptied hits =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }
                    |> withHits "road-1" BuiltKind.Road hits 5000

                let remembered = Map.ofList [ "w1", taskId (Repair "road-1") ]

                Expect.equal
                    (Map.tryFind "w1" (decideFrom remembered (emptied 3100)).Assignments)
                    None
                    "the empty body is released and the next tick's table names it nowhere"

                Expect.isEmpty
                    (repairTasks (planTasksOn (emptied 3100) noThreats))
                    "and at 62% of max, unheld, the road is over the hungry line and out of the pool"

                Expect.equal
                    (repairTasks (planTasksOn (emptied 2000) noThreats))
                    [ "road-1" ]
                    "released under the hungry line it is back in the pool, uncapped, for anybody"

                Expect.equal
                    (poolOn (emptied 2000)
                     |> List.tryPick (fun entry ->
                         if entry.Task = Repair "road-1" then
                             Some(Capacity.capOf CapScope.Everyone entry.Capacity)
                         else
                             None))
                    (Some None)
                    "at the ordinary surplus rung and uncapped: nothing here needs a cap"
            }

            test "a road a quarter from destruction is a rescue: a rung of its own, one body" {
                // The failure #284 was filed on: the surplus tier is ordered by
                // travel cost, and the cluster always holds a road just under
                // the trigger, so the one out on the trunk never wins. The
                // rescue is the lift that ends the comparison.
                let colony =
                    bareRespawn
                    |> withHits "road-near" BuiltKind.Road 2400 5000
                    |> withHits "road-far" BuiltKind.Road 200 5000

                let entryFor id =
                    poolOn colony |> List.tryFind (fun entry -> entry.Task = Repair id)

                let priorityOf id =
                    entryFor id |> Option.map (fun e -> e.Priority) |> Option.defaultValue 0

                Expect.isLessThan
                    (priorityOf "road-far")
                    (priorityOf "road-near")
                    "the road a quarter from destruction outranks the one under the spawn"

                Expect.equal
                    (entryFor "road-far"
                     |> Option.map (fun e -> e.Capacity |> Capacity.capOf CapScope.Everyone))
                    (Some(Some 1))
                    "a rescue is one body's trip"

                Expect.equal
                    (entryFor "road-near"
                     |> Option.map (fun e -> e.Capacity |> Capacity.capOf CapScope.Everyone))
                    (Some None)
                    "an ordinary Repair is uncapped, as it always was"
            }

            test "the rescue budget lifts the worst and leaves the rest in the surplus" {
                // The outpost builders' budget one Task over (#157, #266):
                // `Tuning.RepairRescues` at a time, the most damaged first, so
                // a colony that has let a whole trunk rot still spends most of
                // its surplus at home.
                let colony =
                    bareRespawn
                    |> withHits "road-a" BuiltKind.Road 100 5000
                    |> withHits "road-b" BuiltKind.Road 200 5000
                    |> withHits "road-c" BuiltKind.Road 300 5000

                let lifted =
                    poolOn colony
                    |> List.choose (fun entry ->
                        match entry.Task with
                        | Repair id when Capacity.capOf CapScope.Everyone entry.Capacity = Some 1 ->
                            Some id
                        | _ -> None)

                Expect.equal
                    (List.length lifted)
                    Tuning.defaults.RepairRescues
                    "the budget bounds the crowd that walks out"

                Expect.equal lifted [ "road-a"; "road-b" ] "the worst first, and the third waits"
            }

            test "a damaged Keep structure is no rescue" {
                // The lift reaches the decaying kinds alone (#284): the Keep is
                // judged against full hits and a rampart against a floor, and
                // neither is a thing the colony is letting rot — a Storage at
                // one hit was shot at, and the safe-mode reflex is what answers
                // that (ADR 0034).
                let colony = bareRespawn |> withHits "sto-1" BuiltKind.Storage 1 1_000_000

                Expect.equal
                    (poolOn colony
                     |> List.tryPick (fun entry ->
                         if entry.Task = Repair "sto-1" then
                             Some(Capacity.capOf CapScope.Everyone entry.Capacity)
                         else
                             None))
                    (Some None)
                    "the Keep's Repair is the uncapped, unlifted one"
            }

            test "a repaired-whole road leaves the pool" {
                let whole = bareRespawn |> withHits "road-1" BuiltKind.Road 5000 5000

                Expect.isEmpty
                    (repairTasks (planTasksOn whole noThreats))
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
                    (repairTasks (planTasksOn snapshot noThreats))
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
                    (repairTasks (planTasksOn dented noThreats))
                    [ "spawn-1"; "sto-1"; "tower-1" ]
                    "one hit off max is hungry, on every Keep structure"

                let whole =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
                    |> withHits "tower-1" BuiltKind.Tower 5000 5000
                    |> withHits "sto-1" BuiltKind.Storage 5000 5000

                Expect.isEmpty
                    (repairTasks (planTasksOn whole noThreats))
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
                    (repairTasks (planTasksOn below noThreats))
                    [ "ram-1" ]
                    "one hit under the floor is hungry"

                Expect.isEmpty
                    (repairTasks (planTasksOn at noThreats))
                    "at the floor the rampart is whole"

                Expect.equal
                    (repairTasks (planTasksOn fresh noThreats))
                    [ "ram-1" ]
                    "a rampart just built stands at 1 hit and is the pool's business at once"

                Expect.isEmpty
                    (repairTasks (planTasksOn over noThreats))
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
                    (repairTasks (planTasksOn young noThreats))
                    "a rampart at 1 hit in an RCL2 room is left to decay"

                let youngRoad = young |> withHits "road-1" BuiltKind.Road 1000 5000

                Expect.equal
                    (repairTasks (planTasksOn youngRoad noThreats))
                    [ "road-1" ]
                    "the decaying kinds keep their trigger in the same room"

                let grown =
                    bareRespawn |> withLevel 3 |> withHits "ram-1" BuiltKind.Rampart (floor - 1) max

                Expect.equal
                    (repairTasks (planTasksOn grown noThreats))
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
                    decideOn snapshot

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

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank) ]
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

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.Rank) ]
                    "the economy is fed before roads are patched: rank decided"
            }

            test "a container below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "cont-1" BuiltKind.Container 124999 250000
                let half = bareRespawn |> withHits "cont-1" BuiltKind.Container 125000 250000

                Expect.equal
                    (repairTasks (planTasksOn low noThreats))
                    [ "cont-1" ]
                    "below the trigger: one Repair per ailing container"

                Expect.isEmpty
                    (repairTasks (planTasksOn half noThreats))
                    "at half hits the container is left alone"
            }

            test "a whole container produces no Repair" {
                let whole = bareRespawn |> withHits "cont-1" BuiltKind.Container 250000 250000

                Expect.isEmpty
                    (repairTasks (planTasksOn whole noThreats))
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

                let { Verdicts = verdicts } = decideOn snapshot

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill("spawn-1", Energy)), MatchFactor.Rank) ]
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
                    decideOn snapshot

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
                    decideFrom remembered snapshot

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Repair "road-1"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "the empty creep's remembered Repair is released"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "nothing else fits an empty creep here"
            }
        ]
