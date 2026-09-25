/// The Tuning knobs that price the rows, and the Quotas record they are
/// reported in.
module Fabot.Core.Tests.Decide.QuotaTuningTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

[<Tests>]
let tuningTests =
    testList
        "the tunables"
        [
            test "MinWorkforce is the floor no colony plans below" {
                // A colony with nothing to hire for — no Post, no
                // container, no site — is at its floor and nothing else, so
                // the floor is the whole of its target and the fleet reads
                // it back one body at a time.
                let colony =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                Expect.isEmpty
                    (castRows (decideOn colony).Intents)
                    "two bodies is the shipped floor, and a colony at its floor casts nothing"

                Expect.equal
                    (castRows
                        (decide
                            (colony |> tunedBy (fun t -> { t with MinWorkforce = 3 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ "worker" ]
                    "raise the floor by one and the same colony is one body short"
            }

            test "RepairTrigger is the fraction a decaying kind enters the pool below" {
                let road = bareRespawn |> withHits "road-1" BuiltKind.Road 3000 5000

                Expect.isEmpty
                    (repairTasks (planTasksOn road noThreats))
                    "three fifths of max is above the shipped half, so the road is left alone"

                Expect.equal
                    (repairTasks (
                        planTasksOn
                            (road |> tunedBy (fun t -> { t with RepairTrigger = 0.7 }))
                            noThreats
                    ))
                    [ "road-1" ]
                    "a trigger of seven tenths and the same road is hungry"
            }

            test "RepairWholeLine is the fraction a held decaying kind leaves the pool at" {
                // Read only where a creep holds the Repair, so the pairwise
                // moves the number with the holder standing, and the unheld
                // case beside it shows the hungry line is the one that did
                // not move.
                let road = bareRespawn |> withHits "road-1" BuiltKind.Road 4500 5000

                Expect.isEmpty
                    (repairTasks (planTasksHolding [ Repair "road-1" ] road))
                    "nine tenths of max is over the shipped whole line, holder or no holder"

                Expect.equal
                    (repairTasks (
                        planTasksHolding
                            [ Repair "road-1" ]
                            (road |> tunedBy (fun t -> { t with RepairWholeLine = 0.95 }))
                    ))
                    [ "road-1" ]
                    "a whole line of 0.95 and the same held road is still the holder's job"

                Expect.isEmpty
                    (repairTasks (
                        planTasksOn
                            (road |> tunedBy (fun t -> { t with RepairWholeLine = 0.95 }))
                            noThreats
                    ))
                    "and with nobody holding it the whole line is not read at all: the trigger is"
            }

            test "the hungry line sits under the whole line, and the whole line at or under max" {
                // A whole line at or under the trigger leaves the held case
                // dead, and one over full hits leaves a structure pooled it
                // can never reach.
                Expect.isLessThan
                    Tuning.defaults.RepairTrigger
                    Tuning.defaults.RepairWholeLine
                    "the hungry line is the lower of the two"

                Expect.isLessThanOrEqual
                    Tuning.defaults.RepairWholeLine
                    1.0
                    "and the whole line is a fraction of max that max itself reaches"

                Expect.isGreaterThan
                    Tuning.defaults.RepairTrigger
                    Tuning.defaults.RepairRescueLine
                    "with the rescue line under both: a rescue is a structure past its own trigger"
            }

            test "RampartFloor is the hits a rampart is whole at" {
                // Read at `Independent`, the only stage that keeps a rampart
                // at all: below it the covering rule and this floor are both
                // switched off.
                let keep =
                    bareRespawn |> withLevel 5 |> withHits "ram-1" BuiltKind.Rampart 150_000 300_000

                Expect.isEmpty
                    (repairTasks (planTasksOn keep noThreats))
                    "a hundred and fifty thousand is over the shipped floor"

                Expect.equal
                    (repairTasks (
                        planTasksOn
                            (keep |> tunedBy (fun t -> { t with RampartFloor = 200_000 }))
                            noThreats
                    ))
                    [ "ram-1" ]
                    "raise the floor past it and the same rampart is hungry"

                Expect.isEmpty
                    (repairTasks (
                        planTasksOn
                            (bareRespawn
                             |> withLevel 2
                             |> withHits "ram-1" BuiltKind.Rampart 1 300_000
                             |> tunedBy (fun t -> { t with RampartFloor = 200_000 }))
                            noThreats
                    ))
                    "and under `Independent` no floor is read at all: the rampart decays away (#214)"
            }

            test "PickupThreshold is the pile a Pickup is worth walking for" {
                let pile = pileTaskColony 80 []

                Expect.isEmpty
                    (planTasksOn pile noThreats
                     |> List.filter (function
                         | Pickup _ -> true
                         | _ -> false))
                    "eighty is under the shipped hundred, so the pile is left to decay"

                Expect.equal
                    (planTasksOn
                        (pile |> tunedBy (fun t -> { t with PickupThreshold = 50 }))
                        noThreats
                     |> List.filter (function
                         | Pickup _ -> true
                         | _ -> false))
                    [ Pickup("pile-a", Energy) ]
                    "a threshold of fifty and the same pile is worth the walk"
            }

            test "ReachMargin is the tiles a weapon's range is widened by" {
                let melee = facingBody { X = 25; Y = 29 } [ Attack; Move ]

                Expect.isFalse
                    (Set.contains { X = 25; Y = 25 } (reachIn melee))
                    "range 1 plus the shipped two is three tiles, and four is clear"

                Expect.isTrue
                    (Set.contains
                        { X = 25; Y = 25 }
                        (reachIn (melee |> tunedBy (fun t -> { t with ReachMargin = 3 }))))
                    "one more tile of margin and the same tile is inside the Reach"
            }

            test "the keeper margin is derived from the Reach margin and never written down" {
                // What is pinned is the relation and not the digit: a human
                // who moved `ReachMargin` and not the margin would put a
                // crossing courier back inside a keeper's Reach in silence.
                let reachOf (tuning: Tuning) = Engine.rangedRange + tuning.ReachMargin

                for margin in 0..5 do
                    let tuning =
                        { Tuning.defaults with
                            ReachMargin = margin
                        }

                    Expect.isGreaterThan
                        (Tuning.keeperMargin tuning)
                        (reachOf tuning)
                        $"a tile at the margin is outside the Reach of a keeper pinned within one of its rock (ReachMargin = {margin})"

                    Expect.equal
                        (Tuning.keeperMargin tuning)
                        (Engine.keeperPin + reachOf tuning)
                        "the keeper's pin, its longest weapon, and the Reach margin"

                Expect.equal
                    (Tuning.keeperMargin Tuning.defaults)
                    6
                    "1 + 3 + 2 at the numbers this bot ships with"
            }

            test "StandingCarryPerWork is the line a delivery stops being work at" {
                // Read through the supply floor: a body over the line is a
                // standing body and cannot refill an extension, so the colony
                // hires a hauler in front of every row.
                let colony =
                    { bareRespawn with
                        Creeps = [ creepWith "b1" 0 50 [ Work; Work; Carry; Move; Move ] ]
                    }

                Expect.equal
                    (castRows (decideOn colony).Intents)
                    [ "worker" ]
                    "one Carry per two Work is under the shipped four, so the body can refill and the floor is quiet"

                Expect.equal
                    (castRows
                        (decide
                            (colony |> tunedBy (fun t -> { t with StandingCarryPerWork = 1 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ "hauler" ]
                    "move the line under it and the same body is standing: nothing here can fill an extension"
            }

            test "PioneerCount is the crowd a mother lends a child" {
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let nursery = asNursery switchHome

                Expect.isEmpty
                    (casts nursery (switchHomeFleet @ pioneers))
                    "three is the shipped addend, and thirteen plus three casts nothing"

                Expect.hasLength
                    (casts
                        (nursery |> tunedBy (fun t -> { t with PioneerCount = 4 }))
                        (switchHomeFleet @ pioneers))
                    1
                    "raise the crowd by one and the same fleet is a body short"
            }

            test "SafeModeDeadline is the claimer range the stock is spent at" {
                let claimer =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ]
                    }

                Expect.isEmpty
                    (activations (decideOn claimer).Intents)
                    "range four is outside the shipped deadline of three: the towers get their window"

                Expect.equal
                    (activations
                        (decide
                            (claimer |> tunedBy (fun t -> { t with SafeModeDeadline = 4 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ "ctrl-1" ]
                    "move the deadline out one tile and the same claimer fires it"
            }

            test "StorageLevel is the level the Storage's tile is reserved from" {
                let colony = atLevel 5 (openRoom 6)

                Expect.hasLength
                    (sitesOfKind Storage (decideOn colony).Intents)
                    1
                    "the shipped four is at or under RCL5, so the Storage's pick is held and placed"

                Expect.isEmpty
                    (sitesOfKind
                        Storage
                        (decide
                            (colony |> tunedBy (fun t -> { t with StorageLevel = 3 }))
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "read at a level the engine allows none, the reservation is empty and nothing is placed"
            }

            test "HorizonLookahead is how far above its own level a room is sized at" {
                // The field sizes the clustered placement alone, and the
                // placement filter is the current level's allowance either
                // way, so a positive lookahead is invisible in a count. Its
                // arithmetic is read where it can bite: at zero, and below.
                let colony = atLevel 6 (openRoom 6)

                let placed tuned =
                    let { Intents = intents } = decideOn tuned

                    List.length (sitesOfKind Tower intents),
                    List.length (sitesOfKind Extension intents)

                let towers, extensions = placed colony

                Expect.equal
                    towers
                    2
                    "RCL6 allows two towers and the placement filter is the level's"

                Expect.equal extensions 40 "and forty extensions, which RCL6 unlocks in full"

                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLookahead = 0 })))
                    (2, 40)
                    "no lookahead sizes the room at its own level, which RCL6 places in full — the horizon buying nothing"

                // The shipped value is read where it does move: the tiles.
                // Every lookahead from 0 upwards answers `(2, 40)` above.
                let extensionTiles tuned =
                    sitesOfKind Extension (decideOn tuned).Intents

                Expect.notEqual
                    (extensionTiles (colony |> tunedBy (fun t -> { t with HorizonLookahead = 0 })))
                    (extensionTiles colony)
                    "and one level of lookahead moves the tiles the forty land on, which is what it buys"

                // A negative lookahead is a stale absolute horizon written
                // relatively, and reproduces #341: only a human setting this
                // field below zero can produce it now.
                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLookahead = -1 })))
                    (2, 30)
                    "sized a level behind, an RCL6 room plans none of the ten the engine unlocked — #341's shape"

                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLookahead = -4 })))
                    (0, 5)
                    "four levels behind reserves an RCL2 room's cluster, and the room may place no more than it planned"

                // `allowanceOf`'s catch-all answers a negative level what it
                // answers RCL8, so the clamp at zero is what keeps a lookahead
                // reaching past zero from handing the youngest room the
                // widest window.
                Expect.equal
                    (placed (
                        atLevel 2 (openRoom 6)
                        |> tunedBy (fun t -> { t with HorizonLookahead = -8 })
                    ))
                    (0, 0)
                    "a lookahead reaching below zero sizes the cluster at nothing, rather than at RCL8's sixty"
            }

            test "OutpostBuilders is the crowd the outpost may take" {
                // One number, two rations: how many bodies may be across the
                // Seam at once, and how many of the outpost's sites are worth
                // crossing for.
                let crowd = crowdAtOutpostSite (northBorderColony { X = 10; Y = 38 })

                let tally colony =
                    (decideOn colony).Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.countBy id
                    |> List.sort

                Expect.equal
                    (tally crowd)
                    [ taskId (Build "site-out"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "two is the shipped budget, and the third worker falls to the Upgrade"

                Expect.equal
                    (tally (crowd |> tunedBy (fun t -> { t with OutpostBuilders = 1 })))
                    [ taskId (Build "site-out"), 1; taskId (Upgrade "ctrl-1"), 2 ]
                    "a budget of one and two of the three stay home"

                // The other half, where the queue is longer than the budget:
                // the sites it names are the nearest the Seam, so what moves
                // between the two readings is which site, not only how many.
                let trunk =
                    crowd
                    |> withOutpostTrunk
                        [
                            "site-r1", BuiltKind.Road, { X = 10; Y = 47 }
                            "site-r2", BuiltKind.Road, { X = 10; Y = 46 }
                            "site-r3", BuiltKind.Road, { X = 10; Y = 45 }
                            "site-r4", BuiltKind.Road, { X = 10; Y = 44 }
                        ]

                Expect.equal
                    (tally trunk)
                    [
                        taskId (Build "site-out"), 1
                        taskId (Build "site-r1"), 1
                        taskId (Upgrade "ctrl-1"), 1
                    ]
                    "two lifts the container and the road beside the crossing; the other three roads wait"

                Expect.equal
                    (tally (trunk |> tunedBy (fun t -> { t with OutpostBuilders = 1 })))
                    [ taskId (Build "site-out"), 1; taskId (Upgrade "ctrl-1"), 2 ]
                    "one lifts the container alone — the switch is never queued behind a road"
            }

            test "BootstrapLevel is the line a stage is cut at, and the one place it is read" {
                // `Colony.stageOf`'s own pairwise: the same three facts about
                // a room, read under two lines.
                Expect.equal
                    (Colony.stageOf Tuning.defaults true true (Some 3))
                    (Some Independent)
                    "at the shipped three, an RCL3 colony has outgrown its mother"

                Expect.equal
                    (Colony.stageOf
                        { Tuning.defaults with
                            BootstrapLevel = 5
                        }
                        true
                        true
                        (Some 3))
                    (Some Bootstrapping)
                    "move the line to five and the same room is still being raised"
            }

            test "VisionGrace is how long a held Task outlives the vision that carried it" {
                // Pinned on the field and not on the shipped number: the
                // boundary at the shipped 150 is `OutpostTests`' own case.
                let dark =
                    { bareRespawn with
                        Time = 1000
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                        Sightings =
                            Map.ofList
                                [
                                    "",
                                    {
                                        Tick = 900
                                        Targets = lazy (Set.singleton "spawn-1")
                                    }
                                ]
                    }

                let held = taskId (Refill("spawn-1", Energy))
                let sticky = Map.ofList [ "w1", held ]

                let verdictsOf colony = (decideFrom sticky colony).Verdicts

                Expect.contains
                    (verdictsOf (dark |> tunedBy (fun t -> { t with VisionGrace = 100 })))
                    (Verdict.Kept("w1", held))
                    "a grace of a hundred covers a hundred dark ticks, and the holder waits for the vision"

                Expect.contains
                    (verdictsOf (dark |> tunedBy (fun t -> { t with VisionGrace = 99 })))
                    (Verdict.Released("w1", held, ReleaseReason.TaskGone))
                    "one shorter and the same darkness is a Task given up on"
            }
        ]

[<Tests>]
let quotasRecordTests =
    testList
        "the quotas record"
        [
            test "the cascade writes down its rows, and they sum to the target" {
                // Observability only: the record the `observe.mjs quotas`
                // view prints. The supply floor is a floor and not a row and
                // so has no line here; the worker row is what the target
                // leaves after the specialists.
                let { Quotas = quotas } = decideOn bareRespawn

                Expect.equal
                    (quotas.Rows |> List.map (fun r -> r.Row))
                    [
                        "guard"
                        "ranger"
                        "reserver"
                        "anchor"
                        "hauler"
                        "miner"
                        "courier"
                        "upgrader"
                        "worker"
                    ]
                    "one row per casting row, in the cascade's order"

                Expect.equal
                    (quotas.Rows |> List.sumBy (fun r -> r.Quota))
                    quotas.Target
                    "the rows' quotas are the target"

                Expect.equal
                    (quotas.Rows |> List.sumBy (fun r -> r.Living))
                    quotas.Living
                    "the rows' living counts partition the fleet"
            }
        ]
