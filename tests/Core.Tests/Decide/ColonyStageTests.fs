/// The colony's Stage — ownership, a spawn, a tower stood full and a level —
/// and what each rule reads off it, then the little of a raised child's room
/// its mother borrows: the builders' and ferry budgets, the child's rock,
/// and the ferry's drop before the child's buffer stands.
module Fabot.Core.Tests.Decide.ColonyStageTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ColonyFixtures

/// One colony's stage forced to an answer, with its controller level left
/// where the fixture put it. The shell can never build such a ColonyView
/// (`World.stages` derives one from the other), which is what makes it
/// the instrument: a rule still reading `Controller.Level` would answer
/// the same either way.
let private atStage stage (colony: ColonyView) =
    { colony with
        Stages = Map.add (SpatialInfo.homeName colony.Spatial) stage colony.Stages
    }

/// A mother "W1N1" and the child "W1N2" she raises, both declared.
let private raisingPair: Colony list =
    [
        {
            Home = "W1N1"
            Outposts = []
            Errands = []
            Salvage = []
            Mother = None
            Consignee = None
            Perimeter = []
            RampartFloors = Map.empty
            FirstSpawn = None
            Assaults = []
        }
        {
            Home = "W1N2"
            Outposts = []
            Errands = []
            Salvage = []
            Mother = Some "W1N1"
            Consignee = None
            Perimeter = []
            RampartFloors = Map.empty
            FirstSpawn = None
            Assaults = []
        }
    ]

/// The pair as the shell reads it: both rooms ours with a spawn standing at
/// `level`, the mother with no tower and the child with one tower per entry
/// of `towers`, holding that much energy. Only the facts a stage is read off.
let private raisingWorld level (towers: int list) : World =
    let colonyRoom name refillables =
        name,
        { RoomFacts.empty with
            Control =
                Some
                    {
                        Owner = Ownership.Ours
                        Reservation = None
                        SafeMode = false
                        SafeModeCooldownUntil = 0
                        Sign = None
                    }
            Controller =
                Some
                    {
                        Id = $"ctrl-{name}"
                        Level = level
                        TicksToDowngrade = 20000
                        SafeModeAvailable = 1
                        SafeModeActive = false
                        SafeModeCooldownUntil = 0
                    }
            Spawns =
                [
                    {
                        Name = $"Spawn-{name}"
                        Id = $"spawn-{name}"
                        RoomName = name
                        IsSpawning = false
                        Remaining = 0
                    }
                ]
            Refillables = refillables
        }

    { World.empty with
        Rooms =
            Map.ofList
                [
                    colonyRoom "W1N1" []
                    colonyRoom
                        "W1N2"
                        (towers
                         |> List.mapi (fun i energy ->
                             {
                                 Id = $"tower-{i}"
                                 FreeCapacity = Engine.towerCapacity - energy
                                 Kind = BuiltKind.Tower
                             }))
                ]
    }

[<Tests>]
let colonyStageTests =
    testList
        "the colony stage"
        [
            test
                "a stage is ownership, a spawn, a tower stood full and a level, and each of the four moves it alone" {
                // `Colony.stageOf`, the one place `Tuning.BootstrapLevel` is read.
                // Pairwise on each input in turn, the other three held.
                Expect.equal
                    (Colony.stageOf Tuning.defaults false false true (Some 1))
                    None
                    "a room we do not own is no colony of ours: a candidate, whose one rule is the Claim pool"

                Expect.equal
                    (Colony.stageOf Tuning.defaults false true true (Some 5))
                    None
                    "and ownership is asked first: nothing standing in it makes an unowned room a stage"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true false true (Some 1))
                    (Some Nursery)
                    "claimed with no spawn of ours standing in it is a nursery"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true false true (Some 8))
                    (Some Nursery)
                    "and a nursery at any level: what ends it is a spawn, not a controller"

                Expect.equal
                    (Colony.stageOf
                        Tuning.defaults
                        true
                        true
                        true
                        (Some(Tuning.defaults.BootstrapLevel - 1)))
                    (Some Bootstrapping)
                    "its own spawn standing and one level short of the line is the bootstrap window"

                Expect.equal
                    (Colony.stageOf
                        Tuning.defaults
                        true
                        true
                        true
                        (Some Tuning.defaults.BootstrapLevel))
                    (Some Independent)
                    "and at the line, a tower stood full, it is independent — the one comparison this constant is read in"

                Expect.equal
                    (Colony.stageOf
                        Tuning.defaults
                        true
                        true
                        false
                        (Some Tuning.defaults.BootstrapLevel))
                    (Some Weaning)
                    "at the line with no tower stood full it is weaning: its own rules run as an independent colony's, and its mother still raises it (#445)"

                Expect.equal
                    (Colony.stageOf
                        Tuning.defaults
                        true
                        true
                        false
                        (Some(Tuning.defaults.BootstrapLevel + 1)))
                    (Some Independent)
                    "and past the line the tower is not asked: a mature colony is independent whatever its towers"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true true false (Some 8))
                    (Some Independent)
                    "at RCL8 as at RCL4"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true true true None)
                    None
                    "a colony whose controller nothing can place has no stage, and every reader answers it as it always did"
            }

            test "a child at the line is weaning until a tower of its own stands full" {
                // #445's acceptance, at the seam the shell hands every view its
                // stages through: the World.
                let childAt level towers =
                    World.stages Tuning.defaults raisingPair (raisingWorld level towers)
                    |> Map.tryFind "W1N2"

                Expect.equal (childAt 3 []) (Some Weaning) "RCL3 and no tower: still being raised"

                Expect.equal
                    (childAt 3 [ Engine.towerCapacity / 2 ])
                    (Some Weaning)
                    "RCL3 and a tower half full: still being raised"

                Expect.equal
                    (childAt 3 [ 0; Engine.towerCapacity ])
                    (Some Independent)
                    "RCL3 and one tower of its own full: independent"

                Expect.equal
                    (childAt 2 [ Engine.towerCapacity ])
                    (Some Bootstrapping)
                    "and a full tower below the line moves nothing"
            }

            test "a tower that fires and drains never takes an independent child back" {
                // The latch: once a tower of the child's has stood full, the room is
                // in `World.Towered` for as long as it stays ours, whatever its
                // towers hold or whether any stands.
                let stood =
                    World.latchTowers
                        Tuning.defaults
                        Set.empty
                        (raisingWorld 3 [ Engine.towerCapacity ])

                let next level towers =
                    World.latchTowers Tuning.defaults stood.Towered (raisingWorld level towers)

                let childOf world =
                    World.stages Tuning.defaults raisingPair world |> Map.tryFind "W1N2"

                Expect.equal
                    (childOf stood)
                    (Some Independent)
                    "the premise: full at RCL3, independent"

                Expect.equal
                    (childOf (next 3 [ 300 ]))
                    (Some Independent)
                    "the tower fired down to 300 the tick after: still independent"

                Expect.equal
                    (childOf (next 4 []))
                    (Some Independent)
                    "RCL4 and its tower destroyed after independence: still independent"

                Expect.equal
                    (childOf (raisingWorld 3 [ 300 ]))
                    (Some Weaning)
                    "and the latch is the only thing holding it: a world that never saw the tower full reads the tower now"
            }

            test
                "a weaning child whose tower fills during a raid stays weaning until the raid has gone" {
                // #468: Independent drops the mother's resident garrison
                // (#447), so the stage read holds back while an armed raid
                // stands in the home; the latch still records the tower.
                let raided (world: World) =
                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.change
                                "W1N2"
                                (Option.map (fun facts ->
                                    { facts with
                                        Hostiles =
                                            [
                                                { hostileAt
                                                      "h-1"
                                                      { X = 10; Y = 10 }
                                                      [ Attack; Move ] with
                                                    Pos = RoomPos.at "W1N2" { X = 10; Y = 10 }
                                                }
                                            ]
                                    }))
                    }

                let filled =
                    World.latchTowers
                        Tuning.defaults
                        Set.empty
                        (raided (raisingWorld 3 [ Engine.towerCapacity ]))

                let childOf world =
                    World.stages Tuning.defaults raisingPair world |> Map.tryFind "W1N2"

                Expect.isTrue
                    (Set.contains "W1N2" filled.Towered)
                    "the latch records the full tower"

                Expect.equal (childOf filled) (Some Weaning) "the raid in its home: still weaning"

                let hold =
                    ExitWatch.Held
                        {
                            Run = [ { X = 0; Y = 10 } ]
                            Exit = { X = 0; Y = 10 }
                            Until = filled.Time + 50
                        }

                Expect.equal
                    (childOf (
                        World.latchTowers
                            Tuning.defaults
                            filled.Towered
                            { raisingWorld 3 [ 300 ] with
                                ExitWatches = Map.ofList [ "W1N2", hold ]
                            }
                    ))
                    (Some Weaning)
                    "and while the Guard holds the exit it left by (#450)"

                Expect.equal
                    (childOf (
                        World.latchTowers Tuning.defaults filled.Towered (raisingWorld 3 [ 300 ])
                    ))
                    (Some Independent)
                    "the raid gone: independent off the latch, the tower drained or not"
            }

            test "a child past the line is independent whatever its towers, with the latch empty" {
                // A global reset empties the heap; a raid drains or destroys the
                // towers. Neither may hand a mature colony back to its mother.
                let childAt level towers =
                    World.stages Tuning.defaults raisingPair (raisingWorld level towers)
                    |> Map.tryFind "W1N2"

                for level in [ Tuning.defaults.BootstrapLevel + 1; 7 ] do
                    Expect.equal
                        (childAt level [])
                        (Some Independent)
                        $"RCL{level}, no tower standing, nothing latched: independent"

                    Expect.equal
                        (childAt level [ 300 ])
                        (Some Independent)
                        $"RCL{level}, its tower fired down to 300, nothing latched: independent"
            }

            test "the latch forgets a room that stops being ours" {
                let stood =
                    World.latchTowers
                        Tuning.defaults
                        Set.empty
                        (raisingWorld 3 [ Engine.towerCapacity ])

                let lost =
                    { stood with
                        Rooms =
                            stood.Rooms
                            |> Map.change
                                "W1N2"
                                (Option.map (fun facts ->
                                    { facts with
                                        Control =
                                            facts.Control
                                            |> Option.map (fun c ->
                                                { c with Owner = Ownership.Unowned })
                                    }))
                    }

                Expect.isTrue
                    (Set.contains "W1N2" stood.Towered)
                    "the premise: the child's tower stood full"

                Expect.isFalse
                    (Set.contains
                        "W1N2"
                        (World.latchTowers Tuning.defaults stood.Towered lost).Towered)
                    "a room we lost and claim again starts its window over"
            }

            test "a colony nobody raises is independent at the line, tower or none" {
                // The tower is the mother's reason to keep raising; a colony with
                // no mother has nobody to wait for it.
                Expect.equal
                    (World.stages Tuning.defaults raisingPair (raisingWorld 3 [])
                     |> Map.tryFind "W1N1")
                    (Some Independent)
                    "the mother at RCL3 with no tower is independent as she always was"
            }

            test "the road gate reads the stage and not the level" {
                // #209's gate, migrated: the same RCL5 fixture the layout tests plan,
                // with the stage moved under it.
                let roads colony =
                    let { Intents = intents } = decideOn colony
                    sitesOfKind Road intents

                Expect.isNonEmpty
                    (roads (trunkColony 5 |> atStage Independent))
                    "the premise: an independent colony places its trunk"

                Expect.isEmpty
                    (roads (trunkColony 5 |> atStage Bootstrapping))
                    "the same RCL5 room bootstrapping places none — the gate is the stage, and the level says nothing"

                Expect.isEmpty
                    (roads (trunkColony 5 |> atStage Nursery))
                    "and a nursery none either: every stage under the line answers alike"
            }

            test
                "a child weaning on the tower clause places roads and keeps ramparts as before #445" {
                // The tower holds the mother's borrowed work and nothing of the
                // child's own: at the line its roads and its rampart floor are an
                // independent colony's, whatever a raid has drained its tower to.
                Expect.equal
                    (Colony.stageOf
                        Tuning.defaults
                        true
                        true
                        false
                        (Some Tuning.defaults.BootstrapLevel))
                    (Some Weaning)
                    "the premise: RCL3 with no tower stood full is the stage under test"

                let roads colony =
                    let { Intents = intents } = decideOn colony
                    sitesOfKind Road intents

                let level = Tuning.defaults.BootstrapLevel

                Expect.isNonEmpty
                    (roads (trunkColony level |> atStage Independent))
                    "the premise: an independent RCL3 colony places its trunk"

                Expect.equal
                    (roads (trunkColony level |> atStage Weaning))
                    (roads (trunkColony level |> atStage Independent))
                    "and a weaning one places the very same roads"

                let hungry colony =
                    repairTasks (planTasksOn colony noThreats)

                let ramparted stage =
                    bareRespawn
                    |> withLevel level
                    |> atStage stage
                    |> withHits "ram-1" BuiltKind.Rampart 1 300_000

                Expect.equal
                    (hungry (ramparted Weaning))
                    [ "ram-1" ]
                    "and holds its rampart to the floor, as an independent colony does"
            }

            test
                "a weaning child is raised as a bootstrapping one: the mother's borrowed work waits for the tower" {
                let pool colony =
                    planTasksOn colony noThreats |> List.map taskId

                let weaning =
                    { raisingMother with
                        Stages = Map.add "W1N2" Weaning raisingMother.Stages
                    }

                Expect.contains
                    (pool weaning)
                    (taskId (Upgrade "ctrl-child"))
                    "she still upgrades its controller"

                Expect.equal
                    (Colony.bootstrapping
                        (Map.ofList [ "W1N2", Weaning ])
                        raisingPair
                        raisingPair.[0])
                    [ "W1N2" ]
                    "and keeps it in her scan set"

                Expect.isEmpty
                    (Colony.bootstrapping
                        (Map.ofList [ "W1N2", Independent ])
                        raisingPair
                        raisingPair.[0])
                    "until it is independent"
            }

            test "the rampart line reads the stage and not the level" {
                // #214's floor, migrated with it: the level is held at RCL2 through
                // both, where the level-reading rule would have answered "no floor"
                // whatever the stage.
                let hungry colony =
                    repairTasks (planTasksOn colony noThreats)

                let ramparted stage =
                    bareRespawn
                    |> withLevel 2
                    |> atStage stage
                    |> withHits "ram-1" BuiltKind.Rampart 1 300_000

                Expect.equal
                    (hungry (ramparted Independent))
                    [ "ram-1" ]
                    "an independent colony holds its rampart to the floor"

                Expect.isEmpty
                    (hungry (ramparted Bootstrapping))
                    "and the same room bootstrapping leaves it to decay, at the very same level"
            }

            test "a home site's tier reads the stage and not the level" {
                // The child's own reading of `isBootstrappingRoom`, which used to be a
                // spawn standing plus `Controller.Level`. Only the stage moves, and
                // the match factor says which tier decided.
                //
                // Against the flow and not the controller (#234): an independent
                // colony's site outranks its own Upgrade by a rung now, so only the
                // hungry spawn at the lane's far end tells the two tiers apart.
                let lane stage =
                    bufferLaneFlow
                        [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                        [
                            {
                                Id = "site-1"
                                Left = siteOwes
                                Begun = false
                            }
                        ]
                        (creepWith "w" 100 0 (bodyFor workerPattern 300))
                    |> atStage stage

                let matched stage =
                    let { Verdicts = verdicts } = decideOn (lane stage)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("w", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched Bootstrapping)
                    (Some(taskId (Build "site-1"), MatchFactor.TravelCost))
                    "a bootstrapping colony builds its bank before its controller: the site ties the flow and is nearer"

                Expect.equal
                    (matched Independent)
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.Rank))
                    "and an independent one leaves it surplus, under the flow like any home site"

                Expect.equal
                    (matched Weaning)
                    (matched Independent)
                    "and so does a weaning one: the tower holds its mother's work, not its own tiers"
            }

            test "the nursery and the bootstrap predicates read the stage and not the census" {
                // The only instrument that can show it: a fixture whose kind census
                // and whose stage **disagree**. `raisingMother` holds a spawn structure
                // in the child's layer, the fact both predicates used to be written
                // on, and here the stage is forced back to `Nursery`.
                let pool colony =
                    planTasksOn colony noThreats |> List.map taskId

                let stagedAs stage colony =
                    { colony with
                        Stages = Map.add "W1N2" stage colony.Stages
                    }

                Expect.contains
                    (pool raisingMother)
                    (taskId (Upgrade "ctrl-child"))
                    "the premise: a child she projects at `Bootstrapping` is one whose controller she upgrades"

                Expect.isFalse
                    (List.contains
                        (taskId (Upgrade "ctrl-child"))
                        (pool (stagedAs Nursery raisingMother)))
                    "the same room at `Nursery` is not, though the spawn the old rule counted still stands in her census"

                Expect.contains
                    (pool (stagedAs Nursery raisingMother))
                    (taskId (Build "site-spawn"))
                    "and what she works there instead is the site, pooled off the projection as a nursery's always was"

                // The other direction, on the fixture whose census holds
                // no spawn at all: the stage says one stands, and the
                // borrowing follows the stage rather than the structure.
                Expect.isFalse
                    (List.contains (taskId (Upgrade "ctrl-child")) (pool claimedChild))
                    "the premise: a nursery's own controller is nobody's business yet"

                Expect.contains
                    (pool (stagedAs Bootstrapping claimedChild))
                    (taskId (Upgrade "ctrl-child"))
                    "and the same census at `Bootstrapping` pools it: no structure moved, only the stage"
            }

            test "the pioneer addend is flat across the stage, and reads the stage to be" {
                // The addend is flat over both stages before independence, so the
                // fleet must not move on the tick a nursery becomes a bootstrapping
                // child. Pinned against the *stage* alone: the census keeps the spawn
                // structure through the flip, so a `raising` still reading the census
                // would drop her target by three on the `Nursery` reading.
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents

                let short fleet =
                    List.truncate (List.length fleet - 1) fleet

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let raised = withNorthSpawn (asNursery switchHome)

                let stillANursery =
                    { raised with
                        Stages = Map.add "W1N2" Nursery raised.Stages
                    }

                Expect.isEmpty
                    (casts raised (switchHomeFleet @ pioneers))
                    "the premise: raising a child at `Bootstrapping` puts her target at sixteen"

                Expect.isEmpty
                    (casts stillANursery (switchHomeFleet @ pioneers))
                    "and the very same room at `Nursery` leaves it at sixteen: one addend across both stages"

                match casts stillANursery (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "and tight: one short of sixteen she still casts the addend's own row for the nursery"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "Decide reads the mother's scan set, not the child's stage, to close her window" {
                // The stage is the shell's to read: it decides the projection
                // (`Colony.bootstrapping` lets an `Independent` child go), and Decide
                // reads only the projection it is handed. Pinned on a ColonyView the
                // shell no longer builds — an `Independent` child still in her scan
                // set — so that a predicate in Decide reading the stage a second time
                // would show here.
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents

                let short fleet =
                    List.truncate (List.length fleet - 1) fleet

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let raised = withNorthSpawn (asNursery switchHome)

                let grownUp =
                    { raised with
                        Stages = Map.add "W1N2" Independent raised.Stages
                    }

                Expect.isEmpty
                    (casts raised (switchHomeFleet @ pioneers))
                    "the premise: a bootstrapping child in her projection puts her target at sixteen"

                Expect.isEmpty
                    (casts grownUp (switchHomeFleet @ pioneers))
                    "the same room, the same projection, the child independent — and her target is still sixteen"

                match casts grownUp (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "and tight: one short of sixteen she still casts the addend's own row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                // The Upgrade half of the same borrowing: it stays in her pool while
                // the room is in her scan set, and leaves with the room.
                let pool colony =
                    planTasksOn colony noThreats |> List.map taskId

                let outgrownInPlace =
                    { raisingMother with
                        Stages = Map.add "W1N2" Independent raisingMother.Stages
                    }

                Expect.contains
                    (pool outgrownInPlace)
                    (taskId (Upgrade "ctrl-child"))
                    "an independent child she still projects is still hers to upgrade"

                Expect.isFalse
                    (List.contains
                        (taskId (Upgrade "ctrl-child"))
                        (pool (withoutNorthRoom outgrownInPlace)))
                    "and what takes it away is the room leaving her projection, which is the only thing that ever did"
            }
        ]

[<Tests>]
let borrowedRoomBudgetTests =
    testList
        "a borrowed room's sites and the budgets"
        [
            test "one FerryLoads budget is spread over a child room's buffers in id order" {
                // #224 (user decision 2026-09-07): the hauler row hires FerryLoads
                // bodies per child, so the pool admits that many per child, the first
                // buffer by id taking the budget's share. Pairwise on the budget.
                let twoBuffers loads =
                    let mother = ferryMother Bootstrapping
                    let north = SpatialInfo.layerOf mother.Spatial "W1N2"

                    { mother with
                        Tuning =
                            { mother.Tuning with
                                FerryLoads = loads
                            }
                        Spatial =
                            { mother.Spatial with
                                TargetKinds =
                                    Map.add
                                        "can-child2"
                                        (Structure BuiltKind.Container)
                                        mother.Spatial.TargetKinds
                                Stores = Map.add "can-child2" 500 mother.Spatial.Stores
                            }
                            |> withNeighbour
                                "W1N2"
                                { north with
                                    TargetPositions =
                                        Map.add
                                            "can-child2"
                                            { X = 10; Y = 47 }
                                            north.TargetPositions
                                }
                    }

                let capOf loads id =
                    let view = twoBuffers loads

                    let atlas = Atlas.ofView view

                    planPool view atlas (Threats.quietOn view atlas) [] (planTasksOn view noThreats)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Refill(id, Energy) then
                            Capacity.capOf CapScope.Everyone pooled.Capacity
                        else
                            None)

                Expect.equal
                    (capOf 1 "can-child")
                    (Some 1)
                    "one load: the first buffer by id takes it"

                Expect.equal (capOf 1 "can-child2") (Some 0) "and the second takes none"
                Expect.equal (capOf 2 "can-child") (Some 1) "two loads: one apiece"

                Expect.equal
                    (capOf 2 "can-child2")
                    (Some 1)
                    "so the pool admits exactly what the row hires"
            }
        ]

/// `ferryMother` raising W1N2 past its nursery, with no controller of her own
/// to upgrade, the child's source at (11,48) and a container on its Seat at
/// (10,47), and one empty generalist "w" of hers standing at `at`.
let private raisedChildRock (at: RoomPos) =
    let mother = ferryMother Bootstrapping
    let child = SpatialInfo.layerOf mother.Spatial "W1N2"

    { mother with
        Controller = None
        BorrowedSources = [ source "src-child" ]
        Creeps = [ worker "w" 0 50 ]
        Spatial =
            { mother.Spatial with
                TargetKinds =
                    mother.Spatial.TargetKinds
                    |> Map.add "src-child" Source
                    |> Map.add "can-src" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { child with
                    TargetPositions =
                        child.TargetPositions
                        |> Map.add "src-child" { X = 11; Y = 48 }
                        |> Map.add "can-src" { X = 10; Y = 47 }
                    CreepPositions =
                        if at.Room = "W1N2" then
                            Map.ofList [ "w", RoomPos.pos at ]
                        else
                            Map.empty
                }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        if at.Room = "W1N1" then
                            Map.ofList [ "w", RoomPos.pos at ]
                        else
                            Map.empty
                })
    }

[<Tests>]
let raisedChildRockTests =
    testList
        "a mother's pioneer digs a raised child's rock"
        [
            test "a pioneer standing in the child digs its source" {
                // Live 2026-10-04: W17S25's spawn stood, the child's rock left the
                // mother's pool with the nursery, and her pioneer walked two rooms
                // for energy while the child's own first bodies were too small to dig.
                Expect.equal
                    (matchOf (raisedChildRock (RoomPos.at "W1N2" { X = 9; Y = 44 }))
                     |> Option.map fst)
                    (Some(taskId (Harvest "src-child")))
                    "the rock under its feet"
            }

            test "a body at the mother's home is never sent to the child's rock" {
                Expect.isNone
                    (matchOf (raisedChildRock (RoomPos.at "W1N1" { X = 10; Y = 5 })))
                    "the child's rock is for the pioneer already there"
            }

            test "the child's Post hires no Anchor of the mother's" {
                let rock = raisedChildRock (RoomPos.at "W1N2" { X = 9; Y = 44 })

                let anchors colony =
                    rowOf "anchor" colony
                    |> Option.map (fun row -> row.Quota)
                    |> Option.defaultValue 0

                Expect.equal
                    (anchors { rock with BorrowedSources = [] } - anchors rock)
                    1
                    "the child's Post is out of the mother's count (#192)"
            }
        ]

/// `ferryMother` raising W1N2 past its nursery before its buffer stands: no
/// container of the child's, and these bodies of hers in the child.
let private unbufferedChild (bodies: (CreepInfo * Pos) list) =
    let mother = ferryMother Bootstrapping
    let child = SpatialInfo.layerOf mother.Spatial "W1N2"

    { mother with
        Creeps = bodies |> List.map fst
        Spatial =
            { mother.Spatial with
                TargetKinds = mother.Spatial.TargetKinds |> Map.remove "can-child"
                Stores = Map.empty
            }
            |> withNeighbour
                "W1N2"
                { child with
                    TargetPositions = child.TargetPositions |> Map.remove "can-child"
                    CreepPositions = bodies |> List.map (fun (c, at) -> c.Name, at) |> Map.ofList
                }
    }

[<Tests>]
let unbufferedChildTests =
    testList
        "a raised child with no buffer yet takes the ferry's load beside its controller"
        [
            test "the child's controller is the ferry's sink while a Work body of ours stands there" {
                // User, 2026-10-04: W17S25 stood its spawn, its buffer was a site,
                // and no ferry crossed at all.
                let drop = Refill("ctrl-child", Energy)

                Expect.contains
                    (planTasksOn
                        (unbufferedChild [ worker "p1" 0 50, { X = 10; Y = 44 } ])
                        noThreats)
                    drop
                    "a pioneer there to pick it up: dropped beside the controller"

                Expect.isFalse
                    (List.contains drop (planTasksOn (unbufferedChild []) noThreats))
                    "nobody there: nothing is dropped to decay"

                let dropped =
                    (decideFrom
                        (Map.ofList [ "h1", taskId drop ])
                        (unbufferedChild
                            [
                                hauler "h1" 100 0, { X = 10; Y = 45 }
                                worker "p1" 0 50, { X = 10; Y = 44 }
                            ]))
                        .Intents

                Expect.contains
                    dropped
                    (DropEnergy "h1")
                    "beside the controller the hauler drops its load"
            }
        ]

[<Tests>]
let unbufferedChildMatchTests =
    testList
        "a loaded hauler at home is matched to the unbuffered child's controller"
        [
            test "the ferry crosses for the controller's drop" {
                let colony =
                    let child = unbufferedChild [ worker "p1" 0 50, { X = 10; Y = 44 } ]

                    // The mother's stock over her reserve (#505): under it her
                    // income banks before it crosses.
                    { child with
                        Creeps = child.Creeps @ [ hauler "w" 1600 0 ]
                        Spatial =
                            { child.Spatial with
                                Stores = Map.add "storage-1" 50_000 child.Spatial.Stores
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 5 } ]
                                })
                    }

                let mine =
                    (decideOn colony).Verdicts
                    |> List.filter (fun v -> (sprintf "%A" v).Contains "\"w\"")

                Expect.equal
                    (matchOf colony |> Option.map fst)
                    (Some(taskId (Refill("ctrl-child", Energy))))
                    $"%A{mine}"
            }
        ]
