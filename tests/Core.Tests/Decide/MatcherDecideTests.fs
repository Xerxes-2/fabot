/// The whole decision: what `decide` emits, and the intake lines that
/// judge whether a trip is worth making.
module Fabot.Core.Tests.Decide.MatcherDecideTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

[<Tests>]
let tests =
    testList
        "decide"
        [
            test "an empty creep is matched to a Harvest task and remembered" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn snapshot

                Expect.contains intents (HarvestSource("w1", "src-a")) "empty creep goes harvesting"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "assignment is remembered"
            }

            test "bare respawn yields exactly one spawn Intent" {
                let { Intents = intents } = decideOn bareRespawn

                match spawnIntents intents with
                | [ (spawnName, body, creepName) ] ->
                    Expect.equal spawnName "Spawn1" "spawns from the only spawn"
                    Expect.isNonEmpty body "body must not be empty"
                    Expect.isNotEmpty creepName "creep needs a name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawn Intent body is affordable at bare-respawn energy" {
                let { Intents = intents } = decideOn bareRespawn

                for (_, body, _) in spawnIntents intents do
                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        300
                        "body cost within bare-respawn energy"
            }

            test "no spawn Intent when energy is below a worker body cost" {
                let snapshot = { bareRespawn with Bank = bank 100 300 }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
            }

            test "no spawn Intent while the spawn is already spawning" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ { spawn with IsSpawning = true } ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (spawnIntents intents) "spawn is busy"
            }

            // Three Seats around src-a: a target of three, so one worker
            // leaves a deficit of two — enough demand for both spawns.
            let threeSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                    ]

            test "two idle spawns in one room spend the shared bank once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (spawnName, _, _) ] ->
                    Expect.equal spawnName "Spawn1" "the first spawn in list order takes the budget"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            // One colony, one bank, whatever room a spawn record names: 300 buys one
            // body, and the second spawn waits.
            test "a spawn filed under another room still draws the colony's one bank" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                    RoomName = "W2N2"
                                }
                            ]
                        Bank = bank 300 300
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, _, _) -> name))
                    [ "Spawn1" ]
                    "one bank funds one body, and the second spawn waits"
            }

            test "with zero creeps one bank funds two minimal bodies at once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        Bank = bank 550 550
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, body, _) -> name, body))
                    [ "Spawn1", [ Work; Carry; Move ]; "Spawn2", [ Work; Carry; Move ] ]
                    "the fallback debits the bank per body instead of waiting on the engine"
            }

            test "at 550 capacity the whole capacity is spent" {
                let snapshot =
                    { bareRespawn with
                        Bank = bank 550 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                        "two units plus the 150 remainder as Carry/Carry/Move"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at 300 capacity the remainder pads the single unit" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Carry; Move; Move ]
                        "one unit plus the 100 remainder as a Carry/Move pair"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "below minimum workforce, spawning waits for full capacity" {
                let snapshot =
                    { bareRespawn with
                        Bank = bank 400 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty
                    (spawnIntents intents)
                    "a living workforce can bank up to a bigger body"
            }

            test "with zero creeps a minimal body is spawned from available energy" {
                let snapshot = { bareRespawn with Bank = bank 250 550 }

                let { Intents = intents } = decideOn snapshot

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "an empty colony cannot wait for extensions it cannot refill"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "with zero creeps and unaffordable minimal body, no spawn Intent" {
                let snapshot = { bareRespawn with Bank = bank 150 550 }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (spawnIntents intents) "even the fallback needs its unit cost"
            }

            test "one worker is below minimum: a second is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.hasLength (spawnIntents intents) 1 "a lone worker cannot keep the loop going"
            }

            test "no spawn Intent when workforce is at minimum" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50; worker "worker-2" 0 50 ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
            }

            test "empty creeps spread across sources instead of piling on one" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } = decideOn snapshot
                let assigned = assignments |> Map.toList |> List.map snd |> List.sort

                Expect.equal
                    assigned
                    [ (taskId (Harvest "src-a")); (taskId (Harvest "src-b")) ]
                    "greedy matching balances load per task"
            }

            test "greedy matching counts kept assignments as load" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } =
                    decideFrom (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "w1 keeps its source"

                Expect.equal
                    (Map.tryFind "w2" assignments)
                    (Some(taskId (Harvest "src-b")))
                    "w2 avoids the occupied source"
            }

            test "assignments pass through unchanged when no creeps died" {
                let assignments = Map.ofList [ "worker-1", (taskId (Harvest "src-a")) ]

                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Assignments = kept } = decideFrom assignments snapshot
                Expect.equal kept assignments "assignments survive the tick"
            }

            test "an assignment sticks across ticks even when greedy would rebalance" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30 ]
                    }

                let assignments = Map.ofList [ "w1", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideFrom assignments snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Harvest "src-b")))
                    "no thrash: creep stays on its source"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-b"))
                    "intent follows the sticky assignment"
            }

            test "a creep that fills up is reassigned from Harvest to Refill" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideFrom (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1", Energy))
                    "delivery intent emitted"
            }

            test "a loaded creep feeds a hungry tower once spawn and extensions are full" {
                // Full feeders leave the pool, so the tower Refill is the one delivery
                // on offer — the same transfer to the creep.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "spawn-1" 0 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "tower-1" 500 BuiltKind.Tower
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("tower-1", Energy))))
                    "the tower is the delivery that remains"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "tower-1", Energy))
                    "the same transfer intent feeds a tower"
            }

            test "a creep that empties is reassigned from Refill back to Harvest" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Refill("spawn-1", Energy))) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes back to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "surplus: a full creep with a full spawn switches to upgrading" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideFrom (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "nothing to refill, so surplus goes to the controller"

                Expect.contains intents (UpgradeController("w1", "ctrl-1")) "upgrade intent emitted"
            }

            test "a hungry structure beats the controller for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "refill outranks upgrade while a structure is missing energy"
            }

            test "an upgrading creep that empties goes back to harvest" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "spent creep returns to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test
                "a full creep with a full spawn and no controller is left unassigned with no intent" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideFrom (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) snapshot

                Expect.isEmpty (Map.toList kept) "no applicable task"

                let creepIntents =
                    intents
                    |> List.filter (function
                        | SpawnCreep _ -> false
                        | _ -> true)

                Expect.isEmpty creepIntents "idle creep emits nothing"
            }

            test "a full creep with a construction site and a full spawn goes building" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites =
                            [
                                {
                                    Id = "site-1"
                                    Left = siteOwes
                                    Begun = false
                                }
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideFrom (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Build "site-1")))
                    "surplus energy goes into construction"

                Expect.contains intents (BuildSite("w1", "site-1")) "build intent emitted"
            }

            test "an empty creep is never matched to a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites =
                            [
                                {
                                    Id = "site-1"
                                    Left = siteOwes
                                    Begun = false
                                }
                            ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decideFrom (Map.ofList [ "w1", (taskId (Build "site-1")) ]) snapshot

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes harvesting instead"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "a hungry structure beats a construction site for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites =
                            [
                                {
                                    Id = "site-1"
                                    Left = siteOwes
                                    Begun = false
                                }
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "refill outranks build while a structure is missing energy"
            }

            test "assignments of dead creeps are dropped" {
                let assignments = Map.ofList [ "ghost", "task-a" ]
                let { Assignments = kept } = decideFrom assignments bareRespawn
                Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
            }
        ]

[<Tests>]
let intakeRoomTests =
    testList
        "an intake needs room"
        [
            test "a nearly full hauler delivers before it picks up, and an emptier one picks up" {
                // Live, W12S28 2026-09-07: a hauler holding 1,150 of 1,200 walked forty
                // tiles into the north room to pick fifty off a pile while the spawn stood
                // at eighteen energy. An intake is for a body at least half empty;
                // pairwise on the store alone, same tile, same pool.
                // empty; pairwise on the store alone, same tile, same pool.
                let lane energy =
                    let body = List.replicate 6 Carry @ List.replicate 3 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Creeps = [ creepWith "h" energy (300 - energy) body ]
                        Spatial =
                            { spatial [] [ for x in 8..18 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "pile-1", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-1", { X = 12; Y = 10 }, (Dropped Energy)
                                    "ext-1", { X = 16; Y = 10 }, Structure BuiltKind.Extension
                                ]
                            |> withCreepsAt [ "h", { X = 13; Y = 10 } ]
                    }

                let matched energy =
                    let { Verdicts = verdicts } = decideOn (lane energy)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.equal
                    (matched 280)
                    (Some(taskId (Refill("ext-1", Energy))))
                    "twenty free of three hundred: a delivery, not an intake"

                Expect.equal
                    (matched 100)
                    (Some(taskId (Pickup("pile-1", Energy))))
                    "two hundred free: the pile is taken first"
            }
        ]

[<Tests>]
let intakeWorthTests =
    testList
        "an intake is worth the trip"
        [
            test "a container that cannot half fill the hauler is left for the stock" {
                // Live, W12S28 2026-09-07 (#232): a 24C/12M hauler matched a source
                // container holding ~200, was released `inapplicable` forty-two ticks
                // later having drained the Anchor's trickle up to half a load, while the
                // Storage held 263,803 and the spawn stood at twenty-eight energy. The
                // tier keeps the stock behind every container that applies, so the only
                // thing that reaches the stock is a container that does not.
                //
                // Pairwise on the store's stock alone: one hauler, two Withdraws, the
                // container the nearer at every reading.
                let lane stock =
                    let body = List.replicate 24 Carry @ List.replicate 12 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Creeps = [ creepWith "h" 0 1200 body ]
                        Spatial =
                            { spatial [] [ for x in 8..24 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "can-src", stock; "stock-1", 263_803 ]
                            }
                            |> withTargets
                                [
                                    "ext-1", { X = 8; Y = 10 }, Structure BuiltKind.Extension
                                    "can-src", { X = 12; Y = 10 }, Structure BuiltKind.Container
                                    "stock-1", { X = 22; Y = 10 }, Structure BuiltKind.Storage
                                ]
                            |> withCreepsAt [ "h", { X = 13; Y = 10 } ]
                    }

                let matched stock =
                    let { Verdicts = verdicts } = decideOn (lane stock)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.equal
                    (matched 216)
                    (Some(taskId (Withdraw("stock-1", Energy))))
                    "the live reading: two hundred at its feet is not worth a twelve-hundred body's trip"

                Expect.equal
                    (matched 599)
                    (Some(taskId (Withdraw("stock-1", Energy))))
                    "one under half a load is still the stock's"

                Expect.equal
                    (matched 600)
                    (Some(taskId (Withdraw("can-src", Energy))))
                    "half the body's free capacity standing in the store is worth the trip"
            }

            test "the line is the asking body's free capacity and not one row's load" {
                // The gate reads the pair and not the Task: the same store that is too
                // thin for a twelve-hundred hauler is worth a 450-carry generalist's trip.
                let lane stock =
                    let body =
                        List.replicate 9 Work @ List.replicate 9 Carry @ List.replicate 9 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = []
                        Creeps = [ creepWith "w" 0 450 body ]
                        Spatial =
                            { spatial [] [ for x in 8..24 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "can-src", stock ]
                            }
                            |> withTargets
                                [ "can-src", { X = 12; Y = 10 }, Structure BuiltKind.Container ]
                            |> withCreepsAt [ "w", { X = 13; Y = 10 } ]
                    }

                let matched stock =
                    let { Verdicts = verdicts } = decideOn (lane stock)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("w", task, _) -> Some task
                        | _ -> None)

                Expect.isNone (matched 224) "one under half of 450: the walk is not paid for"

                Expect.equal
                    (matched 225)
                    (Some(taskId (Withdraw("can-src", Energy))))
                    "half of 450 standing in the store is worth the trip"
            }
        ]

[<Tests>]
let intakeDecayTests =
    testList
        "the worth-the-trip line and the stores it is off"
        [
            test "a store whose energy is going away is taken by whatever body is asking" {
                // The [[pickup]] is outside the line because a pile decays — and a
                // tombstone and a ruin decay too, which is the only thing CONTEXT says
                // separates them from a container. So the exemption follows the decay and
                // not the Task's name.
                //
                // Pairwise on the target's kind alone: one store, one body, a hundred and
                // fifty in it at every reading.
                let lane kind =
                    let body = List.replicate 24 Carry @ List.replicate 12 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = []
                        Creeps = [ creepWith "h" 0 1200 body ]
                        Spatial =
                            { spatial [] [ for x in 8..18 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "store-1", 150 ]
                            }
                            |> withTargets [ "store-1", { X = 12; Y = 10 }, kind ]
                            |> withCreepsAt [ "h", { X = 16; Y = 10 } ]
                    }

                let matched kind =
                    let { Verdicts = verdicts } = decideOn (lane kind)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.isNone
                    (matched (Structure BuiltKind.Container))
                    "a container holding a hundred and fifty waits for a body it can half fill"

                Expect.equal
                    (matched Tombstone)
                    (Some(taskId (Withdraw("store-1", Energy))))
                    "a tombstone ends, so its hundred and fifty is drawn by the body that is asking"

                Expect.equal
                    (matched (Dropped Energy))
                    (Some(taskId (Pickup("store-1", Energy))))
                    "and the pile the line was never carried to is picked up by the same body"
            }

            test "the stock is the fall-through, so it is never the thing that refuses" {
                // What the line buys is the fall to the tier below, and there is no tier
                // below the stock's own Withdraw: a Storage drawn down by a build holding
                // four hundred against a 1,200-carry hauler is the colony's last intake.
                //
                // Pairwise on the store's kind alone: the same four hundred in a source
                // container is exactly the refusal above.
                let lane kind =
                    let body = List.replicate 24 Carry @ List.replicate 12 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Creeps = [ creepWith "h" 0 1200 body ]
                        Spatial =
                            { spatial [] [ for x in 8..24 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "store-1", 400 ]
                            }
                            |> withTargets
                                [
                                    "ext-1", { X = 8; Y = 10 }, Structure BuiltKind.Extension
                                    "store-1", { X = 12; Y = 10 }, kind
                                ]
                            |> withCreepsAt [ "h", { X = 16; Y = 10 } ]
                    }

                let matched kind =
                    let { Verdicts = verdicts } = decideOn (lane kind)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.equal
                    (matched (Structure BuiltKind.Storage))
                    (Some(taskId (Withdraw("store-1", Energy))))
                    "the deepest intake in the colony draws whatever body asks it"

                Expect.isNone
                    (matched (Structure BuiltKind.Container))
                    "the same four hundred in a container is left for the tier below it"
            }
        ]

/// The live pioneer's body (#501): W15S28's worker, `16W 17C 17M`, light and
/// not standing, empty.
let private pioneer name =
    creepWith name 0 850 (List.replicate 16 Work @ List.replicate 17 Carry @ List.replicate 17 Move)

/// A mother and her [[nursery]] one room north (#501): the mother's full
/// source container at W1N1 (10,46), at the far end of her corridor from the
/// border, and the nursery's own source walled in at W1N2 (10,3), its one
/// Seat (10,4) at the head of a corridor running down to the border. The
/// pioneer stands on that Seat. Nothing else is work: no controller, no
/// refillable, no site, so the pool is the far Withdraw and the local Harvest
/// and the Matched Verdict names one comparison.
let private nurseryIntake (rock: SourceInfo) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ rock ]
        Creeps = [ pioneer "p" ]
        RoomControl = Map.ofList [ "W1N1", ownedRoom; "W1N2", ownedRoom ]
        Declared = [ "W1N1"; "W1N2" ]
        Stages = Map.ofList [ "W1N1", Independent; "W1N2", Nursery ]
        Borrowed =
            {
                Rooms = [ "W1N2" ]
                Defended = []
                Garrisoned = []
            }
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList [ "src-nur", Source; "can-home", Structure BuiltKind.Container ]
                Stores = Map.ofList [ "can-home", Engine.containerCapacity ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (corridor 10 1 48)
                    TargetPositions = Map.ofList [ "can-home", { X = 10; Y = 46 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 10 4 48)
                    TargetPositions = Map.ofList [ "src-nur", { X = 10; Y = 3 } ]
                    CreepPositions = Map.ofList [ "p", { X = 10; Y = 4 } ]
                }
    }

/// One body "b" and two source containers on one plain row (#501): the
/// half-full one two tiles west of the body, the full one at (24,10).
/// Pairwise: no source, no controller, no refillable, so the two Withdraws are
/// the whole pool.
let private twoContainers (body: CreepInfo) =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = []
        Creeps = [ body ]
        Spatial =
            { spatial [] [ for x in 5..40 -> { X = x; Y = 10 }, Plain ] with
                Stores = Map.ofList [ "can-half", 1_000; "can-full", Engine.containerCapacity ]
            }
            |> withTargets
                [
                    "can-half", { X = 10; Y = 10 }, Structure BuiltKind.Container
                    "can-full", { X = 24; Y = 10 }, Structure BuiltKind.Container
                ]
            |> withCreepsAt [ body.Name, { X = 12; Y = 10 } ]
    }

/// A home container holding 500 four tiles from body "b" at W1N1 (10,40), and
/// a full container in the outpost one room north at W1N2 (10,10): the haul
/// the user's ruling keeps on rank (#501).
let private outpostHaul (body: CreepInfo) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = []
        Creeps = [ body ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "can-home", Structure BuiltKind.Container
                            "can-out", Structure BuiltKind.Container
                        ]
                Stores = Map.ofList [ "can-home", 500; "can-out", Engine.containerCapacity ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (corridor 10 1 48)
                    TargetPositions = Map.ofList [ "can-home", { X = 10; Y = 44 } ]
                    CreepPositions = Map.ofList [ body.Name, { X = 10; Y = 40 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 10 1 48)
                    TargetPositions = Map.ofList [ "can-out", { X = 10; Y = 10 } ]
                }
    }

/// A full energy container and a full mineral container on one row, a worker
/// between them at (30,10), swamp on both sides of it (#501): the energy
/// store at (44,10) costs more than one `IntakeTravelPerRung` to reach, and the
/// mine's container at (11,10), beside the deposit walled in at (10,10), less
/// than one further again. Both stand two rungs up on the Feeding tier.
let private oreBesideEnergy =
    let terrain x =
        if x = 10 then Wall
        elif (x >= 13 && x <= 28) || (x >= 32 && x <= 42) then Swamp
        else Plain

    { bareRespawn with
        Sources = []
        Refillables = []
        Controller = None
        Creeps = [ lightWorker "b" 0 450 ]
        Spatial =
            { spatial [] [ for x in 8..48 -> { X = x; Y = 10 }, terrain x ] with
                Thorium = Map.ofList [ "min-a", 22_000; "can-min", Engine.containerCapacity ]
                Stores = Map.ofList [ "can-e", Engine.containerCapacity ]
                Cooldowns = Map.ofList [ "ext-a", 0 ]
            }
            |> withTargets
                [
                    "min-a", minePos, Mineral
                    "ext-a", minePos, Structure BuiltKind.Extractor
                    "can-min", minePost, Structure BuiltKind.Container
                    "can-e", { X = 44; Y = 10 }, Structure BuiltKind.Container
                ]
            |> withCreepsAt [ "b", { X = 30; Y = 10 } ]
    }

let private matchedFor creep colony =
    let { Verdicts = verdicts } = decideOn colony

    verdicts
    |> List.tryPick (function
        | Verdict.Matched(name, task, factor) when name = creep -> Some(task, factor)
        | _ -> None)

let private costOf creep task (colony: ColonyView) =
    Atlas.travelCost (Atlas.ofView colony) creep task |> Option.defaultValue -1

/// How much further body "b" walks to the first Task than to the second.
let private extraTravel far near colony =
    costOf "b" far colony - costOf "b" near colony

[<Tests>]
let intakeTravelTests =
    testList
        "an intake pays for its travel in rungs"
        [
            test "a pioneer beside the nursery's source digs it over a full container a room away" {
                // Live, W17S25 at t930,881 (#501): the two pioneers walked ~220 ticks
                // each way to W15S27's full container while the nursery's own source
                // sat at 1,500. A full container's two rungs are spent on the walk.
                let colony = nurseryIntake (source "src-nur")
                let far = costOf "p" (Withdraw("can-home", Energy)) colony

                Expect.isGreaterThanOrEqual
                    far
                    (2 * colony.Tuning.IntakeTravelPerRung)
                    "premise: the container's two rungs are spent on the walk"

                Expect.equal
                    (matchedFor "p" colony)
                    (Some(taskId (Harvest "src-nur"), MatchFactor.TravelCost))
                    "the source under its feet, and travel is what turned the rank"
            }

            test "the same pioneer with the source drained past its walk still fetches" {
                // Pairwise on the rock alone: the Harvest is rejected by the restock gate
                // (`tooEarly`), so the far container is all there is.
                let colony = nurseryIntake (drained "src-nur" 300)

                Expect.equal
                    (matchedFor "p" colony |> Option.map fst)
                    (Some(taskId (Withdraw("can-home", Energy))))
                    "nothing local to dig: the full container"
            }

            test
                "a full container under one IntakeTravelPerRung further still outbids a nearer half-full one" {
                // #242, #306: the full container's two rungs keep their purpose for a
                // body near it.
                let colony = twoContainers (lightWorker "b" 0 450)

                Expect.isLessThan
                    (extraTravel
                        (Withdraw("can-full", Energy))
                        (Withdraw("can-half", Energy))
                        colony)
                    colony.Tuning.IntakeTravelPerRung
                    "premise: the full container is less than one IntakeTravelPerRung further"

                Expect.equal
                    (matchedFor "b" colony)
                    (Some(taskId (Withdraw("can-full", Energy)), MatchFactor.Rank))
                    "the full container, on rank"
            }

            test "the same pair at twice IntakeTravelPerRung further goes to the nearer" {
                // Two rungs are given back by twice the extra travel, and no more: the
                // full container then ties the half-full one and cost decides.
                let colony = twoContainers (lightWorker "b" 0 450)

                let extra =
                    extraTravel (Withdraw("can-full", Energy)) (Withdraw("can-half", Energy)) colony

                let tuned =
                    { colony with
                        Tuning =
                            { colony.Tuning with
                                IntakeTravelPerRung = extra / 3
                            }
                    }

                Expect.equal
                    (matchedFor "b" tuned)
                    (Some(taskId (Withdraw("can-half", Energy)), MatchFactor.TravelCost))
                    "both rungs spent on the walk: the nearer store"
            }

            test "a body with no Work keeps the full outpost container on rank" {
                // User, 2026-10-04: a hauler has no dig to choose, and charging it left
                // an outpost's full container to overflow. Pairwise on the body: the
                // worker standing on the same tile goes home.
                // Fifty aboard puts the hauler at fatigue parity, the worker's pace,
                // and still half empty.
                let haul = outpostHaul (hauler "b" 50 50)
                let dig = outpostHaul (lightWorker "b" 0 450)

                for colony in [ haul; dig ] do
                    Expect.isGreaterThanOrEqual
                        (extraTravel
                            (Withdraw("can-out", Energy))
                            (Withdraw("can-home", Energy))
                            colony)
                        (2 * colony.Tuning.IntakeTravelPerRung)
                        "premise: the outpost lies twice IntakeTravelPerRung further"

                Expect.equal
                    (matchedFor "b" haul)
                    (Some(taskId (Withdraw("can-out", Energy)), MatchFactor.Rank))
                    "the hauler drains the full outpost container"

                Expect.equal
                    (matchedFor "b" dig |> Option.map fst)
                    (Some(taskId (Withdraw("can-home", Energy))))
                    "the worker on the same tile draws the home container"
            }

            test
                "a Work body near a full energy container does not walk to a farther full ore container" {
                // The whole intake family is charged against the nearest intake: an
                // absolute charge on energy alone discounted the energy container for
                // its own walk and sent the body past it to the ore.
                let colony = oreBesideEnergy
                let energy = Withdraw("can-e", Energy)

                Expect.isGreaterThanOrEqual
                    (costOf "b" energy colony)
                    colony.Tuning.IntakeTravelPerRung
                    "premise: the energy container is itself a walk"

                Expect.isGreaterThan
                    (extraTravel (Withdraw("can-min", Thorium)) energy colony)
                    0
                    "premise: the ore container is further still"

                Expect.equal
                    (matchedFor "b" colony |> Option.map fst)
                    (Some(taskId energy))
                    "the nearer full container"
            }

            test "the lift is the Matcher's alone: the full container's push weight is its tier's" {
                // The Resolver's push weight reads the pooled rank, which the travel lift
                // never touches.
                let colony = nurseryIntake (source "src-nur")

                let rank =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Withdraw("can-home", Energy) then
                            Some pooled.Priority
                        else
                            None)

                Expect.equal
                    rank
                    (Some(priorityOfTier Feeding + rankOfRung TwoRungsUp))
                    "the pooled rank is the full container's two rungs, travel or not"
            }
        ]

[<Tests>]
let selfHealTests =
    let healer =
        { creepWith "patient" 0 0 [ Move; Heal ] with
            Hits = { Hits = 199; HitsMax = 200 }
        }

    let run creep actions =
        let colony = { bareRespawn with Creeps = [ creep ] }

        match IntentPlan.create actions with
        | Error conflict -> failtestf "invalid fixture: %A" conflict
        | Ok plan -> healReflex colony noThreats plan |> IntentPlan.intents

    testList
        "self-heal reflex"
        [
            test "injured idle bodies heal themselves with no task or energy" {
                let colony = { bareRespawn with Creeps = [ healer ] }
                let result = decideOn colony

                Expect.contains
                    result.Intents
                    (HealCreep("patient", "patient"))
                    "one point of damage is enough"

                Expect.isOk
                    (IntentPlan.create result.Intents)
                    "the complete decision stays executable"

                Expect.isFalse
                    (Map.containsKey "patient" result.Assignments)
                    "healing needs no assignment"
            }
            test "healthy bodies and bodies without active HEAL do nothing" {
                for body in [ healer.Body; Map.empty; Map.ofList [ Heal, 0 ] ] do
                    let healthy =
                        { healer with
                            Hits = { Hits = 200; HitsMax = 200 }
                            Body = body
                        }

                    Expect.isEmpty (run healthy []) "full life needs no healing"

                for body in [ Map.empty; Map.ofList [ Heal, 0 ]; Map.ofList [ Move, 1 ] ] do
                    Expect.isEmpty
                        (run { healer with Body = body } [])
                        "destroyed or absent HEAL cannot heal"
            }
            test "every engine-conflicting action takes precedence over the reflex" {
                for action in
                    [
                        HarvestSource("patient", "source")
                        AttackCreep("patient", "hostile")
                        BuildSite("patient", "site")
                        RepairStructure("patient", "road")
                        HealCreep("patient", "other")
                    ] do
                    Expect.equal
                        (run healer [ action ])
                        [ action ]
                        "the reflex cannot replace or suppress a chosen act"
            }
            test "independent actions coexist and another creep's attack does not block healing" {
                let selected =
                    [
                        UpgradeController("patient", "controller")
                        TransferEnergyToStructure("patient", "store", Energy)
                        WithdrawFromStore("patient", "store", Energy, None)
                        PickupPile("patient", "pile")
                        ClaimController("patient", "controller")
                        ReserveController("patient", "controller")
                        MoveCreep("patient", Top)
                        SayCreep("patient", "task")
                        AttackCreep("other", "hostile")
                    ]

                Expect.equal
                    (run healer selected)
                    (selected @ [ HealCreep("patient", "patient") ])
                    "read the shared compatibility rules"
            }
            test "a whole healer heals the most-hurt neighbour, adjacent before ranged" {
                let hurt name missing =
                    { creepWith name 0 0 [ Move ] with
                        Hits = { Hits = 200 - missing; HitsMax = 200 }
                    }

                let medic = creepWith "medic" 0 0 [ Heal; Heal; Move ]

                let heals placed =
                    let colony =
                        { bareRespawn with
                            Creeps = placed |> List.map fst
                            Spatial =
                                bareRespawn.Spatial
                                |> withCreepsAt (
                                    placed |> List.map (fun (c: CreepInfo, pos) -> c.Name, pos)
                                )
                        }

                    match IntentPlan.create [] with
                    | Error conflict -> failtestf "invalid fixture: %A" conflict
                    | Ok plan -> healReflex colony noThreats plan |> IntentPlan.intents

                let at x = { X = x; Y = 10 }

                Expect.equal
                    (heals [ medic, at 10; hurt "near" 50, at 11; hurt "far" 150, at 13 ])
                    [ HealCreep("medic", "near") ]
                    "adjacent first, at three times the ranged rate, whoever bleeds more"

                Expect.equal
                    (heals [ medic, at 10; hurt "far" 150, at 13 ])
                    [ RangedHealCreep("medic", "far") ]
                    "a wound three tiles off takes the ranged heal"

                Expect.isEmpty
                    (heals [ medic, at 10; hurt "farther" 150, at 14 ])
                    "and four tiles off is out of reach"
            }
            test "a fighter pre-heals an adjacent fighter standing in a Reach, at full hits" {
                // #451: the engine credits a heal before its death check, so the
                // heal that saves a body is the one cast before it is hurt.
                let ranger name =
                    creepWith name 0 0 Bodies.rangerPattern.Block

                let heals hostiles =
                    let colony =
                        { bareRespawn with
                            Creeps = [ ranger "a"; ranger "b" ]
                            Spatial =
                                bareRespawn.Spatial
                                |> withCreepsAt [ "a", { X = 10; Y = 10 }; "b", { X = 11; Y = 10 } ]
                        }
                        |> facing hostiles

                    match IntentPlan.create [] with
                    | Error conflict -> failtestf "invalid fixture: %A" conflict
                    | Ok plan ->
                        healReflex colony (threatsOf colony (Atlas.ofView colony)) plan
                        |> IntentPlan.intents

                Expect.isEmpty (heals []) "the premise: whole and out of any Reach, nobody heals"

                Expect.equal
                    (heals [ hostileAt "bow" { X = 14; Y = 10 } [ RangedAttack; Move ] ])
                    [ HealCreep("a", "b"); HealCreep("b", "a") ]
                    "each heals the other before the shot lands"
            }

            test "a wounded fighter heals itself before it pre-heals a whole one beside it" {
                // #451: a shield is spent by a body that owes nothing; one that
                // is bleeding closes its own wound first.
                let block = Bodies.rangerPattern.Block
                let full = Engine.partHits * List.length block

                let ranger =
                    { creepWith "ranger" 0 0 block with
                        Hits = { Hits = full * 2 / 5; HitsMax = full }
                    }

                let guard = creepWith "guard" 0 0 Bodies.guardPattern.Block

                let colony =
                    { bareRespawn with
                        Creeps = [ ranger; guard ]
                        Spatial =
                            bareRespawn.Spatial
                            |> withCreepsAt
                                [ "ranger", { X = 10; Y = 10 }; "guard", { X = 11; Y = 10 } ]
                    }
                    |> facing [ hostileAt "bow" { X = 14; Y = 10 } [ RangedAttack; Move ] ]

                let heals =
                    match IntentPlan.create [] with
                    | Error conflict -> failtestf "invalid fixture: %A" conflict
                    | Ok plan ->
                        healReflex colony (threatsOf colony (Atlas.ofView colony)) plan
                        |> IntentPlan.intents

                Expect.contains
                    heals
                    (HealCreep("ranger", "ranger"))
                    "the ranger closes its own wound"

                Expect.isFalse
                    (List.contains (HealCreep("ranger", "guard")) heals)
                    "and does not spend the heal on the whole guard"
            }

            test "a second healer does not pour into a wound the first one closes" {
                let patient missing =
                    { creepWith "patient" 0 0 [ Move ] with
                        Hits = { Hits = 200 - missing; HitsMax = 200 }
                    }

                let heals missing =
                    let one = creepWith "one" 0 0 [ Heal; Heal; Move ]
                    let two = creepWith "two" 0 0 [ Heal; Heal; Move ]

                    let colony =
                        { bareRespawn with
                            Creeps = [ one; two; patient missing ]
                            Spatial =
                                bareRespawn.Spatial
                                |> withCreepsAt
                                    [
                                        "one", { X = 10; Y = 10 }
                                        "two", { X = 10; Y = 11 }
                                        "patient", { X = 11; Y = 10 }
                                    ]
                        }

                    match IntentPlan.create [] with
                    | Error conflict -> failtestf "invalid fixture: %A" conflict
                    | Ok plan -> healReflex colony noThreats plan |> IntentPlan.intents

                Expect.equal
                    (heals 10)
                    [ HealCreep("one", "patient") ]
                    "24 closes 10; the second waits"

                Expect.equal
                    (heals 100)
                    [ HealCreep("one", "patient"); HealCreep("two", "patient") ]
                    "a wound deeper than one healer's 24 takes both"
            }
            test "the reflex is idempotent and still acts when fatigued or almost dead" {
                let exhausted =
                    { healer with
                        Fatigue = 10
                        Hits = { Hits = 1; HitsMax = 200 }
                    }

                let once = run exhausted []

                Expect.equal
                    once
                    [ HealCreep("patient", "patient") ]
                    "one active HEAL is enough regardless of fatigue"

                Expect.equal (run exhausted once) once "never duplicate an existing self-heal"
            }
        ]
