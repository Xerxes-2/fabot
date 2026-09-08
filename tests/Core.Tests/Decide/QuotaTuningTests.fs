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
                    (castRows (decide colony Map.empty Set.empty None).Intents)
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
                    (repairTasks (planTasks road noThreats))
                    "three fifths of max is above the shipped half, so the road is left alone"

                Expect.equal
                    (repairTasks (
                        planTasks
                            (road |> tunedBy (fun t -> { t with RepairTrigger = 0.7 }))
                            noThreats
                    ))
                    [ "road-1" ]
                    "a trigger of seven tenths and the same road is hungry"
            }

            test "RampartFloor is the hits a rampart is whole at" {
                // Read at `Independent`, which is the only [[stage]] that
                // keeps a rampart at all (#214): below it the covering rule
                // and this floor are both switched off, so the number is
                // never asked for at a 300 bank rather than answered wrongly
                // there.
                let keep =
                    bareRespawn |> withLevel 5 |> withHits "ram-1" BuiltKind.Rampart 150_000 300_000

                Expect.isEmpty
                    (repairTasks (planTasks keep noThreats))
                    "a hundred and fifty thousand is over the shipped floor"

                Expect.equal
                    (repairTasks (
                        planTasks
                            (keep |> tunedBy (fun t -> { t with RampartFloor = 200_000 }))
                            noThreats
                    ))
                    [ "ram-1" ]
                    "raise the floor past it and the same rampart is hungry"

                Expect.isEmpty
                    (repairTasks (
                        planTasks
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
                    (planTasks pile noThreats
                     |> List.filter (function
                         | Pickup _ -> true
                         | _ -> false))
                    "eighty is under the shipped hundred, so the pile is left to decay"

                Expect.equal
                    (planTasks
                        (pile |> tunedBy (fun t -> { t with PickupThreshold = 50 }))
                        noThreats
                     |> List.filter (function
                         | Pickup _ -> true
                         | _ -> false))
                    [ Pickup "pile-a" ]
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

            test "StandingCarryPerWork is the line a delivery stops being work at" {
                // Read through the supply floor (ADR 0050), which is the
                // rule that asks whether anything the colony holds can put
                // energy into an extension: a body over the line is a
                // [[standing body]] and may not, so the colony hires a
                // hauler in front of every row.
                let colony =
                    { bareRespawn with
                        Creeps = [ creepWith "b1" 0 50 [ Work; Work; Carry; Move; Move ] ]
                    }

                Expect.equal
                    (castRows (decide colony Map.empty Set.empty None).Intents)
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
                    spawnIntents
                        (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

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
                    (activations (decide claimer Map.empty Set.empty None).Intents)
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
                    (sitesOfKind Storage (decide colony Map.empty Set.empty None).Intents)
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

            test "HorizonLevel is the level the clustered kinds are sized at" {
                // Read at the horizon's own level, where the sizing is the
                // whole answer: the placement filter is wide open at RCL6, so
                // what the room asks for is what the reservation held.
                let colony = atLevel 6 (openRoom 6)

                let placed tuned =
                    let { Intents = intents } = decide tuned Map.empty Set.empty None

                    List.length (sitesOfKind Tower intents),
                    List.length (sitesOfKind Extension intents)

                let towers, extensions = placed colony

                Expect.equal towers 2 "the shipped horizon of six sizes two towers"
                Expect.equal extensions 40 "and forty extensions, which RCL6 unlocks in full"

                // The horizon left behind, one field moved (ADR 0055): the
                // same RCL6 room under the shipped-yesterday five sizes thirty
                // and plans none of the ten the engine unlocked. That is the
                // failure this constant exists to prevent, and it is why the
                // move lands before the room does.
                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLevel = 5 })))
                    (2, 30)
                    "a horizon of five sizes the RCL5 cluster, and an RCL6 room may place no more than it planned"

                Expect.equal
                    (placed (colony |> tunedBy (fun t -> { t with HorizonLevel = 2 })))
                    (0, 5)
                    "a horizon of two reserves an RCL2 room's cluster, and the room may place no more than it planned"
            }

            test "OutpostBuilders is the crowd the outpost may take" {
                // One number, two rations since #266: how many bodies may be
                // across the Seam at once, and how many of the outpost's sites
                // are worth crossing for — the head of the queue is exactly as
                // long as the crowd that could work it, so a trunk is paved
                // outward from the crossing instead of all at once.
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Creeps = [ for name in [ "w1"; "w2"; "w3" ] -> worker name 50 0 ]
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "w1", { X = 10; Y = 2 }
                                                "w2", { X = 10; Y = 3 }
                                                "w3", { X = 10; Y = 4 }
                                            ]
                                })
                    }

                let tally colony =
                    (decide colony Map.empty Set.empty None).Assignments
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

                // The other half of the same number, pinned where the queue is
                // longer than it: four hand-laid road sites down one corridor,
                // and the budget says how many of them are feeding-tier at all.
                // The ones it names are the nearest the Seam, so what moves
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
                // `Colony.stageOf`'s own pairwise (ADR 0052 decision 3):
                // the same three facts about a room, read under two lines.
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
                // #151's knob, pinned on the field and not on the shipped
                // number: one colony, one dark room, one dark tick short of
                // a hundred, read under two graces. The boundary at the
                // shipped 150 is `OutpostTests`' own case; what this owns is
                // that the number is read at all.
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
                                        Targets = Set.singleton "spawn-1"
                                    }
                                ]
                    }

                let held = taskId (Refill "spawn-1")
                let sticky = Map.ofList [ "w1", held ]

                let verdictsOf colony =
                    (decide colony sticky Set.empty None).Verdicts

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
                // Observability only (ADR 0009): the record the `observe.mjs
                // quotas` view prints. Six rows in cascade order — the guard
                // at the head of them since ADR 0056, behind only the supply
                // floor, which is a floor and not a row and so has no line
                // here; the worker row is what the target leaves after the
                // specialists, so the quotas sum to the target; the living
                // counts partition the fleet.
                let { Quotas = quotas } = decide bareRespawn Map.empty Set.empty None

                Expect.equal
                    (quotas.Rows |> List.map (fun r -> r.Row))
                    [ "guard"; "reserver"; "anchor"; "hauler"; "upgrader"; "worker" ]
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
