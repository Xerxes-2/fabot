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

            // One colony, one bank, whatever room a spawn record names
            // (ADR 0052 decision 1). This pinned the opposite until R2a:
            // the projection carried a bank per room and a spawn filed
            // under a second room drew a second full one — a colony with
            // two homes, which is the shape #191 split into two colonies
            // and ADR 0047 gave one `decide` each. The spawn below is the
            // same shape it was and the answer is now the one the shared
            // bank gives above: 300 buys one body, and the second spawn
            // waits.
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
                    (Some(taskId (Refill "spawn-1")))
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1"))
                    "delivery intent emitted"
            }

            test "a loaded creep feeds a hungry tower once spawn and extensions are full" {
                // Full feeders leave the pool, so the tower Refill is the one
                // delivery on offer — the same transfer to the creep (ADR 0010).
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
                    (Some(taskId (Refill "tower-1")))
                    "the tower is the delivery that remains"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "tower-1"))
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
                        (Map.ofList [ "w1", (taskId (Refill "spawn-1")) ])
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
                    (Some(taskId (Refill "spawn-1")))
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
                        ConstructionSites = [ { Id = "site-1" } ]
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
                        ConstructionSites = [ { Id = "site-1" } ]
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
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
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
                // Live, W12S28 2026-09-07: a hauler holding 1,150 of 1,200
                // walked forty tiles into the north room to pick fifty off
                // a pile while the spawn stood at eighteen energy — the
                // pile's lifted rung beat every Refill and one free slot
                // made it applicable. An intake is for a body at least half
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
                                    "pile-1", { X = 12; Y = 10 }, Dropped
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
                    (Some(taskId (Refill "ext-1")))
                    "twenty free of three hundred: a delivery, not an intake"

                Expect.equal
                    (matched 100)
                    (Some(taskId (Pickup "pile-1")))
                    "two hundred free: the pile is taken first"
            }
        ]

[<Tests>]
let intakeWorthTests =
    testList
        "an intake is worth the trip"
        [
            test "a container that cannot half fill the hauler is left for the stock" {
                // Live, W12S28 2026-09-07 (#232): a 24C/12M hauler matched a
                // source container holding ~200 at t194,906 and was released
                // `inapplicable` forty-two ticks later, having drained the
                // Anchor's trickle up to half a load, while the Storage held
                // 263,803 and the spawn stood at twenty-eight energy. The
                // Withdraw's own [[capacity]] admits a drawer to any store
                // with one energy in it, and the tier (ADR 0023) keeps the
                // stock behind every container that applies — so the only
                // thing that reaches the stock is a container that does not.
                //
                // Pairwise on the store's stock alone: one hauler, two
                // Withdraws, and the container is the nearer of the two at
                // every reading, so nothing but this gate can move the match.
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
                    (Some(taskId (Withdraw "stock-1")))
                    "the live reading: two hundred at its feet is not worth a twelve-hundred body's trip"

                Expect.equal
                    (matched 599)
                    (Some(taskId (Withdraw "stock-1")))
                    "one under half a load is still the stock's"

                Expect.equal
                    (matched 600)
                    (Some(taskId (Withdraw "can-src")))
                    "half the body's free capacity standing in the store is worth the trip"
            }

            test "the line is the asking body's free capacity and not one row's load" {
                // The gate reads the pair and not the Task (#161, #196): the
                // same store that is too thin for a twelve-hundred hauler is
                // worth a 450-carry generalist's trip at a quarter of the
                // stock. One store and one body here, so what the readings
                // separate is the line itself and nothing else.
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
                    (Some(taskId (Withdraw "can-src")))
                    "half of 450 standing in the store is worth the trip"
            }
        ]

[<Tests>]
let intakeDecayTests =
    testList
        "the worth-the-trip line and the stores it is off"
        [
            test "a store whose energy is going away is taken by whatever body is asking" {
                // The [[pickup]] is outside #232's line because a pile
                // decays (#167, #216 R5) — and a tombstone and a ruin decay
                // too, which is the only thing CONTEXT says separates them
                // from a container. So the exemption follows the decay and
                // not the Task's name: a hundred and fifty is not worth a
                // 1,200-carry hauler's trip to a *container*, because the
                // container will still be there when a smaller body asks,
                // and it is taken off either transient store by that same
                // hauler, because nothing will.
                //
                // Pairwise on the target's kind alone: one store, one body,
                // a hundred and fifty in it at every reading.
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
                    (Some(taskId (Withdraw "store-1")))
                    "a tombstone ends, so its hundred and fifty is drawn by the body that is asking"

                Expect.equal
                    (matched Dropped)
                    (Some(taskId (Pickup "store-1")))
                    "and the pile the line was never carried to is picked up by the same body"
            }

            test "the stock is the fall-through, so it is never the thing that refuses" {
                // What the line buys is the fall to the tier below (ADR
                // 0023), and there is no tier below the stock's own
                // Withdraw. A Storage drawn down by a build — or a young
                // RCL4 one — holding four hundred against a 1,200-carry
                // hauler is the colony's last intake, and refusing it
                // leaves the row idle with the spawn hungry and the energy
                // in reach of nobody.
                //
                // Pairwise on the store's kind alone: the same four hundred
                // in a source container is exactly the refusal #232 asked
                // for.
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
                    (Some(taskId (Withdraw "store-1")))
                    "the deepest intake in the colony draws whatever body asks it"

                Expect.isNone
                    (matched (Structure BuiltKind.Container))
                    "the same four hundred in a container is left for the tier below it"
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
        | Ok plan -> selfHeal colony plan |> IntentPlan.intents

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
                        TransferEnergyToStructure("patient", "store")
                        WithdrawFromStore("patient", "store")
                        PickupEnergy("patient", "pile")
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
