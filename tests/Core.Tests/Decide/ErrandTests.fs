/// The errand as `decide` sees it: the one Task a declared controller-less
/// room offers, who may hold it, what caps it, and the act it fires. The
/// projection half — what a colony may carry of that room — is `ViewTests`'.
module Fabot.Core.Tests.Decide.ErrandTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

/// The errand and tiles are `Fixtures`' own, shared with the reserver row's
/// suite. The room neighbours the fixtures' home, so the chain is one
/// crossing and nothing here is about the walk: `RoomSeamTests` prices the
/// live three-crossing errand.
let private errandRoom = reactorErrand.RoomName
let private reactor = reactorId
let private ringTile = reactorRing

/// A CLAIM body of ours: `[Claim; Move]`, the one block the reserver row
/// casts and so the one the re-claimer is.
let private claimer name =
    creepWith name 0 0 [ BodyPart.Claim; Move ]

let private courier name =
    creepWith name 0 1000 (List.replicate 20 Carry @ List.replicate 10 Move)

/// The complete delivery programme on the one-hop decision fixture. The live
/// three-hop price is pinned by `RoomSeamTests`; here each test moves one fact
/// that opens the row or one end of its Withdraw→Refill cycle.
let private courierRow colony =
    (decideOn colony).Quotas.Rows |> List.find (fun row -> row.Row = "courier")

let private withHomeCreep pos creep colony =
    { colony with
        Creeps = creep :: colony.Creeps
        Spatial = colony.Spatial |> withCreepsAt [ creep.Name, pos ]
    }

let private withHomeCreeps creeps colony =
    { colony with
        Creeps = (creeps |> List.map fst) @ colony.Creeps
        Spatial =
            colony.Spatial
            |> withCreepsAt (creeps |> List.map (fun (creep: CreepInfo, pos) -> creep.Name, pos))
    }

let private withErrandCreep pos creep colony =
    let layer = SpatialInfo.layerOf colony.Spatial errandRoom

    { colony with
        Creeps = creep :: colony.Creeps
        Spatial =
            colony.Spatial
            |> withNeighbour
                errandRoom
                { layer with
                    CreepPositions = Map.add creep.Name pos layer.CreepPositions
                }
    }

/// The shared declaration with the given bodies standing on the given tiles of
/// the errand room, and the owner entry the act is gated on: `None` leaves it
/// out altogether, which is what a gapped relay reads.
let private errandColony owner creeps (colony: ColonyView) =
    colony |> withReactorErrand |> withReactorOwner owner |> standingInErrand creeps

/// The fixtures' home room with nothing of its own to offer, so the pool a
/// case reads is the errand's and the comparison is pairwise.
let private bareHome =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = []
        Spatial = openRoom 6
        RoomControl = homeControl
    }

/// The Reclaims this tick's pool offers.
let private reclaimsOf (colony: ColonyView) =
    planTasksOn colony noThreats
    |> List.filter (function
        | Reclaim _ -> true
        | _ -> false)

/// The Task the Matcher settles on for one creep, None for one it leaves idle.
let private assignedTask name (colony: ColonyView) =
    let { Assignments = assignments } = decideOn colony
    Map.tryFind name assignments

let private holds name task (colony: ColonyView) =
    assignedTask name colony = Some(taskId task)

/// The life the delivery draw asks of a body for a leg of this length, read
/// through the knobs so the cases pin the rule and not the numbers.
let private lifeNeededFor leg =
    leg
    * Tuning.defaults.MineContactAgeing
    * (100 + Tuning.defaults.DeliveryLifeMargin)
    / 100

/// The delivery's loaded leg as the gate prices it: from the Storage's free
/// neighbours to the Reactor's ring, for this body carrying
/// `Tuning.ReactorLoad`. Read off the Atlas, not asserted as a number.
let private loadedLegOf (creep: CreepInfo, colony: ColonyView) =
    let store =
        match SpatialInfo.placementOf colony.Spatial "sto-1" with
        | Some store -> store
        | None -> failtest "the fixture places its Storage"

    let loaded = Grid.factorCarrying creep Tuning.defaults.ReactorLoad

    match Atlas.walkTicksFrom (Atlas.ofView colony) loaded store (snd reactorErrand.Target) with
    | Some ticks -> ticks
    | None -> failtest "the fixture prices this leg since #379, or this case shows nothing"

/// The `ClaimReactor` Intents one tick emits.
let private reclaimIntents (colony: ColonyView) =
    let { Intents = intents } = decideOn colony

    intents
    |> List.choose (function
        | ClaimReactor(name, id) -> Some(name, id)
        | _ -> None)

/// Shibdib's live W15S25 defender at t444,287: enough ranged damage to kill
/// the 200-hit re-claimer, and enough healing that the guard arithmetic wants
/// two blocks rather than one — which distinguishes it from an overwhelming
/// raid the stand-down already handles.
let private liveDefender =
    List.replicate 5 BodyPart.RangedAttack @ List.replicate 6 Move @ [ Heal ]

[<Tests>]
let errandStandDownTests =
    testList
        "an armed player in the errand room is a withdrawal, not a fight no guard can join"
        [
            test "the guard-cap comparison cannot excuse a room the guard row does not serve" {
                let tick = 100

                let defender =
                    { hostileIn errandRoom { X = 29; Y = 28 } liveDefender with
                        Owner = "Shibdib"
                        TicksToLive = 600
                    }

                let keeper =
                    { hostileIn errandRoom { X = 20; Y = 20 } [ RangedAttack; Move ] with
                        Owner = "Source Keeper"
                        TicksToLive = 1500
                    }

                let colony =
                    { (bareHome |> errandColony (Some Ownership.Ours) []) with
                        Time = tick
                        Hostiles = [ defender; keeper ]
                    }

                Expect.isTrue
                    (guardBlocksBeat colony errandRoom Engine.guardCap)
                    "the premise: two guard blocks beat this body, which is why the old deadline opened nothing"

                let log =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        colony
                        (Planner.outpostFactsOf colony)

                Expect.contains
                    (Observe.standDown Tuning.defaults (tick + 1) log).Shut
                    errandRoom
                    "an errand has no guard row, so the hostile's own remaining life is its withdrawal clock"

                Expect.contains
                    (Observe.standDown Tuning.defaults (tick + defender.TicksToLive - 1) log).Shut
                    errandRoom
                    "one tick short of the player's deadline the withdrawal still holds, not extended by the keeper"

                Expect.isFalse
                    (Set.contains
                        errandRoom
                        (Observe.standDown Tuning.defaults (tick + defender.TicksToLive) log).Shut)
                    "on the deadline the unchanged declaration may return"
            }

            test
                "the owner and declaration kind are the asymmetry: a Source Keeper is expected, and an Outpost can fight" {
                let tick = 100

                let defender owner =
                    { hostileIn errandRoom { X = 29; Y = 28 } liveDefender with
                        Owner = owner
                        TicksToLive = 600
                    }

                let folded (colony: ColonyView) =
                    let seen = { colony with Time = tick }

                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        seen
                        (Planner.outpostFactsOf seen)
                    |> Observe.standDown Tuning.defaults (tick + 1)
                    |> fun gate -> gate.Shut

                let errandRaid =
                    { (bareHome |> errandColony (Some Ownership.Ours) []) with
                        Hostiles = [ defender "Shibdib" ]
                    }

                let expectedKeeper =
                    { errandRaid with
                        Hostiles = [ defender "Source Keeper" ]
                    }

                let guardServedRoom =
                    { errandRaid with
                        Errands = []
                        RoomControl = Map.add errandRoom neutralRoom errandRaid.RoomControl
                    }
                    |> withOutpostRoom errandRoom { X = 25; Y = 25 } false

                Expect.isFalse
                    (Set.contains errandRoom (folded expectedKeeper))
                    "a Source Keeper is the expected transit hazard, not a player raid on the target"

                Expect.isTrue
                    (guardBlocksBeat guardServedRoom errandRoom Engine.guardCap)
                    "the premise: the declared Outpost can buy the two blocks that win this exchange"

                Expect.isFalse
                    (Set.contains errandRoom (folded guardServedRoom))
                    "so the same player body in an Outpost keeps the existing fight answer"
            }

            test "the target-room withdrawal removes the errand from the scan until its clock ends" {
                let ground =
                    TerrainGrid.ofList
                        [
                            for x in 1 .. Seam.exitEdge - 1 do
                                for y in 1 .. Seam.exitEdge - 1 -> { X = x; Y = y }, Plain
                        ]

                let ring =
                    Map.ofList
                        [
                            for i in 0 .. Seam.exitEdge do
                                yield { X = i; Y = 0 }, Plain
                                yield { X = i; Y = Seam.exitEdge }, Plain
                                yield { X = 0; Y = i }, Plain
                                yield { X = Seam.exitEdge; Y = i }, Plain
                        ]

                let room =
                    { RoomFacts.empty with
                        Layer =
                            { RoomLayer.empty with
                                Terrain = ground
                            }
                        Border = ring
                    }

                let home =
                    { room with
                        Layer =
                            { room.Layer with
                                TargetPositions =
                                    Map.ofList
                                        [ spawn.Id, { X = 25; Y = 25 }; "w1", { X = 24; Y = 25 } ]
                            }
                        TargetKinds = Map.ofList [ spawn.Id, Structure BuiltKind.Spawn ]
                        Control = Some ownedRoom
                        Controller = Some(controllerAt 5)
                        Spawns = [ spawn ]
                        Energy = bank 650 650
                    }

                let colony =
                    {
                        Home = "W1N1"
                        Outposts = []
                        Errands = [ reactorErrand ]
                        Mother = None
                        Consignee = None
                    }

                let world =
                    {
                        Time = 101
                        Rooms = Map.ofList [ colony.Home, home; errandRoom, room ]
                        Creeps =
                            [
                                {
                                    Room = colony.Home
                                    Info = worker "w1" 0 50
                                }
                            ]
                        Sightings = Map.empty
                    }

                let view shut =
                    ColonyView.ofWorld
                        Tuning.defaults
                        [ colony ]
                        { StandDown.none with Shut = shut }
                        (Map.ofList [ "w1", colony.Home ])
                        world
                        colony

                let admitted = view Set.empty

                let defender =
                    { hostileIn errandRoom { X = 29; Y = 28 } liveDefender with
                        Owner = "Shibdib"
                        TicksToLive = 600
                    }

                let seen =
                    { admitted with
                        Time = 100
                        Hostiles = [ defender ]
                    }

                let log =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        seen
                        (Planner.outpostFactsOf seen)

                let gateAt tick =
                    (Observe.standDown Tuning.defaults tick log).Shut

                let withdrawn = view (gateAt 101)
                let restored = view (gateAt 700)

                Expect.equal
                    (reclaimsOf admitted)
                    [ Reclaim reactor ]
                    "admitted, the declaration pools its Reclaim"

                Expect.equal
                    (reserverCasts (decideOn admitted).Intents)
                    [ oneBlock ]
                    "so its open reserver-row seat really casts the replacement the incident observed"

                Expect.isEmpty (reclaimsOf withdrawn) "withdrawn, no Reclaim remains in the pool"

                Expect.isEmpty
                    (reserverCasts (decideOn withdrawn).Intents)
                    "and no errand seat remains, so Spawn3 does not replace the body the defender killed"

                Expect.equal
                    (reclaimsOf restored)
                    [ Reclaim reactor ]
                    "on the hostile's exact deadline the unchanged declaration pools its Reclaim again"

                let reclaimerName = "reserver-100-Spawn1"

                let occupiedWorld =
                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.add
                                errandRoom
                                { room with
                                    Layer =
                                        { room.Layer with
                                            CreepPositions =
                                                Map.ofList [ reclaimerName, { X = 29; Y = 29 } ]
                                        }
                                }
                        Creeps =
                            world.Creeps
                            @ [
                                {
                                    Room = errandRoom
                                    Info = claimer reclaimerName
                                }
                            ]
                    }

                let withdrawnWithHolder =
                    ColonyView.ofWorld
                        Tuning.defaults
                        [ colony ]
                        { StandDown.none with
                            Shut = gateAt 101
                        }
                        (Map.ofList [ "w1", colony.Home; reclaimerName, colony.Home ])
                        occupiedWorld
                        colony

                let held = Map.ofList [ reclaimerName, taskId (Reclaim reactor) ]
                let decision = decideFrom held withdrawnWithHolder

                Expect.isFalse
                    (Map.containsKey reclaimerName decision.Assignments)
                    "anti-thrash cannot retain the Reclaim after the declaration leaves the view"

                Expect.isEmpty
                    (moveIntents decision.Intents |> List.filter (fst >> (=) reclaimerName))
                    "and the released re-claimer is not moved toward the shut target"
            }
        ]

[<Tests>]
let errandTaskTests =
    testList
        "the errand's own Task: one per declaration, held by a CLAIM body, one at a time"
        [
            test "one Reclaim per declared errand, named for the target the declaration names" {
                // The Task is pooled off the declaration and never off a kind
                // census: `Errand.place`'s target is kind-less, so no pool
                // that sweeps a kind can find it.
                let colony = bareHome |> errandColony (Some Ownership.Rival) []

                Expect.equal
                    (reclaimsOf colony)
                    [ Reclaim reactor ]
                    "the declared reactor's Reclaim, and one of it"

                Expect.isFalse
                    (Map.containsKey reactor colony.Spatial.TargetKinds)
                    "the premise: the target is classified by nothing, so no kind sweep found it"
            }

            test "a colony that declares no errand pools no Reclaim" {
                let colony =
                    bareHome
                    |> errandColony (Some Ownership.Rival) []
                    |> fun c -> { c with Errands = [] }

                Expect.isEmpty
                    (reclaimsOf colony)
                    "no declaration, no Task — the room's vision buys nothing"
            }

            test "a CLAIM body may hold it and a worker may not" {
                // Part arithmetic and nothing else: `claimReactor` is a CLAIM
                // part's act, and a re-claimer asks for no energy state.
                let withClaimer =
                    bareHome |> errandColony (Some Ownership.Rival) [ claimer "rc", ringTile ]

                let withWorker =
                    bareHome |> errandColony (Some Ownership.Rival) [ worker "wk" 0 50, ringTile ]

                Expect.isTrue
                    (withClaimer |> holds "rc" (Reclaim reactor))
                    "the CLAIM body takes it"

                Expect.isFalse
                    (withWorker |> holds "wk" (Reclaim reactor))
                    "and the worker standing on the very same tile does not"
            }

            test "one body at a time: the relay is never a garrison of two" {
                // The flag is taken by one touch of one CLAIM part, so a
                // second body beside it buys nothing.
                let colony =
                    bareHome
                    |> errandColony
                        (Some Ownership.Rival)
                        [ claimer "rc", ringTile; claimer "rc2", { X = 24; Y = 43 } ]

                let held =
                    [ "rc"; "rc2" ]
                    |> List.filter (fun name -> colony |> holds name (Reclaim reactor))

                Expect.equal
                    (List.length held)
                    1
                    "one holder, whichever of the two travel cost picks"
            }

            test "the relief is admitted in time to land with the handover window left" {
                // Three plain tiles between the relief and the ring, so the
                // walk is three ticks: twenty-nine ticks of incumbent life
                // leave 26 at arrival and still block; twenty-eight leaves the
                // 25-tick overlap and admits the relief.
                let relayAt life =
                    let colony =
                        bareHome
                        |> errandColony
                            (Some Ownership.Rival)
                            [
                                claimer "rc" |> withLife life, ringTile
                                claimer "relief", { X = 25; Y = 40 }
                            ]

                    [ "rc"; "relief" ]
                    |> List.filter (fun name -> colony |> holds name (Reclaim reactor))

                Expect.equal
                    (relayAt 29)
                    [ "rc" ]
                    "one tick outside the handover window, the incumbent is still the one resident"

                Expect.equal
                    (relayAt 28)
                    [ "rc"; "relief" ]
                    "at the boundary the relief departs and will land with 25 ticks of overlap"
            }
        ]

[<Tests>]
let errandActTests =
    testList
        "the act: claimReactor on a tick the reactor is not ours, and on no other"
        [
            test "a rival holds it, so the body standing on the ring takes it back" {
                // `claimReactor` has no cooldown and no ownership
                // precondition, so the claim is the first act of the
                // programme and fires against a standing owner.
                let colony =
                    bareHome |> errandColony (Some Ownership.Rival) [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "the act is issued against the rival's ownership"
            }

            test "it is ours already, so the body stands there and says nothing" {
                // The act is withheld and the Task is not: the body keeps its
                // Reclaim, holds the tile and stays the colony's only eye on
                // the room.
                let colony =
                    bareHome |> errandColony (Some Ownership.Ours) [ claimer "rc", ringTile ]

                Expect.isEmpty (reclaimIntents colony) "no act on a tick the flag is already ours"

                Expect.isTrue
                    (colony |> holds "rc" (Reclaim reactor))
                    "and the Task is still held, which is the whole of standing guard"
            }

            test "nobody holds it, which is not ours either" {
                // An unowned reactor consumes nothing and scores for nobody,
                // and it is still a flag we do not have: the engine's
                // `claimReactor` checks no ownership at all, so taking an empty
                // one costs the same act as taking a rival's.
                let colony =
                    bareHome |> errandColony (Some Ownership.Unowned) [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "an unowned reactor is claimed on the same terms"
            }

            test "no owner entry at all is not ours: the act fires the tick vision arrives" {
                // Absence read the safe way round: the relay is the colony's
                // only vision of the room, so the tick a body lands is the
                // first tick there is an answer, and a withheld act on a
                // missing fact would leave the flag with whoever planted it
                // until the next tick.
                let colony = bareHome |> errandColony None [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "absence reads as not-ours, and the act is issued"
            }

            test "two tiles off is not adjacent, so nothing is issued" {
                // `claimReactor` is a Chebyshev-1 act (`creep.claimReactor.js`,
                // verified against `mod-season5` da59118), which is the range
                // the Work Area is the ring for: a body still walking in emits
                // no act it would be refused for.
                let colony =
                    bareHome
                    |> errandColony (Some Ownership.Rival) [ claimer "rc", { X = 25; Y = 42 } ]

                Expect.isEmpty (reclaimIntents colony) "the act waits for the ring"
            }
        ]

[<Tests>]
let courierTests =
    testList
        "the courier: one 500-unit trip over the priced errand"
        [
            test
                "the row opens behind a full load and the resident re-claimer, and not behind a mine" {
                let ready = deliveryColony (Some Ownership.Ours)

                Expect.equal
                    (courierRow ready).Quota
                    1
                    "all three current facts open one cadence seat"

                let incomeRocks =
                    [
                        for i in 1..5 ->
                            $"income-{i}",
                            {
                                X = 21 + (i - 1) % 5 * 3
                                Y = 20 + (i - 1) / 5 * 3
                            }
                    ]

                let incomeTargets =
                    incomeRocks
                    |> List.collect (fun (id, rock) ->
                        [
                            id, rock, Source
                            $"can-{id}",
                            { rock with X = rock.X + 1 },
                            Structure BuiltKind.Container
                        ])

                let earning =
                    { ready with
                        Sources = incomeRocks |> List.map (fst >> source)
                        Tuning = { ready.Tuning with MinWorkforce = 0 }
                        Spatial =
                            ready.Spatial
                            |> withTargets incomeTargets
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        incomeRocks
                                        |> List.fold
                                            (fun terrain (_, rock) ->
                                                terrain
                                                |> TerrainGrid.add rock Wall
                                                |> TerrainGrid.add
                                                    { rock with X = rock.X + 1 }
                                                    Plain)
                                            layer.Terrain
                                })
                    }

                let workerQuota colony =
                    (decideOn colony).Quotas.Rows
                    |> List.find (fun row -> row.Row = "worker")
                    |> fun row -> row.Quota

                let everyTick =
                    { earning with
                        Tuning =
                            { earning.Tuning with
                                DeliveryInterval = 1
                            }
                    }

                // At the 636-tick cadence this income still hires four
                // generalists; a 1,500-energy courier every tick costs
                // 2,250,000 over a worker life and leaves only the Task floor.
                Expect.equal
                    (workerQuota earning, workerQuota everyTick)
                    (4, 1)
                    "courier replacement is amortized at its own cadence before the surplus is divided"

                let poor = { ready with Bank = bank 1499 1499 }

                let short =
                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium = Map.add "sto-1" 499 ready.Spatial.Thorium
                            }
                    }

                let exhausted =
                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium = Map.add "min-a" 0 ready.Spatial.Thorium
                            }
                    }

                let noResident =
                    { ready with
                        Creeps = ready.Creeps |> List.filter (fun creep -> creep.Name <> "relay")
                        Spatial =
                            ready.Spatial
                            |> withNeighbour
                                errandRoom
                                { SpatialInfo.layerOf ready.Spatial errandRoom with
                                    CreepPositions = Map.empty
                                }
                    }

                for colony, reason in
                    [
                        poor, "a bank below the fixed 1,500 body yields"
                        short, "499 Thorium is not one delivery load"
                        noResident, "the delivery waits behind the re-claimer"
                    ] do
                    Expect.equal (courierRow colony).Quota 0 reason

                // The mine is not one of the facts: Thorium never regenerates,
                // so every deposit ends mined out with its ore in a Storage,
                // and ore in the bank scores what ore in the ground scores.
                for colony, reason in
                    [
                        exhausted,
                        "an exhausted deposit does not close the row: the bank still holds a load"
                        ready |> withExtractorSite,
                        "nor does a mine with no extractor standing — the carrier does not dig"
                    ] do
                    Expect.equal (courierRow colony).Quota 1 reason
            }

            test "636 ticks is the cadence, and the fixed body is cast at its boundary" {
                let fixedBody = List.replicate 20 Carry @ List.replicate 10 Move

                let staffed =
                    deliveryColony (Some Ownership.Ours)
                    |> withHomeCreeps
                        [
                            miner "m", { X = 12; Y = 10 }
                            creepWith "hauler-h" 0 1000 fixedBody, { X = 8; Y = 10 }
                            worker "w" 0 50, { X = 9; Y = 10 }
                        ]

                let young = courier "courier-young" |> withLife 865
                let old = courier "courier-old" |> withLife 864

                let courierCasts colony =
                    (decideOn colony).Intents
                    |> spawnIntents
                    |> List.filter (fun (_, _, name) -> name.StartsWith "courier-")

                Expect.hasLength
                    (courierCasts staffed)
                    1
                    "an identical 1,500-capacity hauler does not fill the courier row's gap"

                Expect.isEmpty
                    (staffed |> withHomeCreep { X = 13; Y = 10 } young |> courierCasts)
                    "a courier younger than 636 ticks still owns this delivery slot"

                Expect.equal
                    (staffed
                     |> withHomeCreep { X = 13; Y = 10 } old
                     |> courierCasts
                     |> List.map (fun (_, body, _) -> body))
                    [ fixedBody ]
                    "at 864 TTL the next fixed body is owed"
            }

            // The mod spends `floor(log10 store.T)` extra life a tick on a
            // creep whose tile carries ore, and a loaded courier's own load
            // is on its tile: a body that dies on the leg loses the ore.
            test "the delivery draw refuses a body that could not outlive the loaded leg" {
                let atStorage life =
                    let aged =
                        { courier "courier-aged" with
                            TicksToLive = life
                        }

                    aged,
                    deliveryColony (Some Ownership.Ours) |> withHomeCreep { X = 13; Y = 10 } aged

                let draws (creep: CreepInfo, colony) =
                    Map.tryFind creep.Name (decideOn colony).Assignments = Some(
                        taskId (Withdraw("sto-1", Thorium))
                    )

                let needed = lifeNeededFor (loadedLegOf (atStorage Engine.creepLifetime))

                Expect.isTrue
                    (draws (atStorage needed))
                    "exactly the loaded leg's life, at three ticks a tick, is enough"

                Expect.isFalse
                    (draws (atStorage (needed - 1)))
                    "one tick short of it is refused: that load would be dropped short of the Reactor"
            }

            // The gate reads parts and never a row: a Work part is dead
            // weight on a leg that is all carrying, and puts the body above
            // fatigue parity at exactly the load the programme carries.
            test
                "the delivery draw refuses a body with a Work part, and the pure carrier beside it draws" {
                let worker =
                    creepWith
                        "worker-fresh"
                        0
                        600
                        (List.replicate 11 Work @ List.replicate 12 Carry @ List.replicate 12 Move)

                let alone =
                    deliveryColony (Some Ownership.Ours) |> withHomeCreep { X = 13; Y = 10 } worker

                Expect.isFalse
                    (alone |> holds worker.Name (Withdraw("sto-1", Thorium)))
                    "a fresh, empty, 600-carry worker beside the Storage is refused the delivery draw: it has a Work part"

                let carrier = courier "courier-fresh"
                let both = alone |> withHomeCreep { X = 13; Y = 11 } carrier

                Expect.isTrue
                    (both |> holds carrier.Name (Withdraw("sto-1", Thorium)))
                    "the pure carrier one tile further off draws it"

                Expect.isFalse
                    (both |> holds worker.Name (Withdraw("sto-1", Thorium)))
                    "and the worker still does not"
            }

            // The body asking is empty (it has to be, to draw), and an empty
            // body is not the one that walks the leg: a `10 Carry; 5 Move`
            // body is weightless empty and two ticks a plain tile under a
            // 500-unit load. The leg is priced from the Storage's own
            // neighbours for the body carrying `Tuning.ReactorLoad`.
            test "the loaded leg is priced for the body as loaded, not as it stands empty" {
                let atStorage life =
                    let slow =
                        { creepWith
                              "carrier-slow"
                              0
                              500
                              (List.replicate 10 Carry @ List.replicate 5 Move) with
                            TicksToLive = life
                        }

                    slow,
                    deliveryColony (Some Ownership.Ours) |> withHomeCreep { X = 13; Y = 10 } slow

                let draws (creep: CreepInfo, colony) =
                    colony |> holds creep.Name (Withdraw("sto-1", Thorium))

                let emptyLeg =
                    let creep, colony = atStorage Engine.creepLifetime

                    match
                        Atlas.walkTicks (Atlas.ofView colony) creep.Name (Refill(reactor, Thorium))
                    with
                    | Some ticks -> ticks
                    | None -> failtest "the fixture prices the empty walk too"

                let loadedLeg = loadedLegOf (atStorage Engine.creepLifetime)

                Expect.isGreaterThan
                    loadedLeg
                    emptyLeg
                    "the premise: loaded, this body is slower than the empty walk the gate used to read"

                Expect.isFalse
                    (draws (atStorage (lifeNeededFor emptyLeg)))
                    "the life that covered the empty walk is refused: it does not cover the loaded one"

                Expect.isTrue
                    (draws (atStorage (lifeNeededFor loadedLeg)))
                    "the life that covers the loaded leg at the contact rate, with the margin, draws"

                Expect.isFalse
                    (draws (atStorage (lifeNeededFor loadedLeg - 1)))
                    "one tick short of it is refused"

                // The margin: the walk the body makes is not the walk the
                // Atlas prices — a flee, a keeper detour, a swamp step.
                Expect.isFalse
                    (draws (atStorage (loadedLeg * Tuning.defaults.MineContactAgeing)))
                    "exactly the loaded leg's own arithmetic, with no slack over it, is refused"
            }

            // Live at t559,469 the Reactor ran down toward zero with 376 T in
            // W15S28's Storage and every deposit mined out: there was never
            // going to be a fuller load.
            test
                "the last load is drawn whole once no more ore is coming, and the full-load gate stands while it is" {
                let banked amount digging =
                    let ready = deliveryColony (Some Ownership.Ours)

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium =
                                    ready.Spatial.Thorium
                                    |> Map.add "sto-1" amount
                                    |> Map.add "can-min" 0
                                    |> Map.add "min-a" (if digging then 22_000 else 0)
                            }
                    }

                let partial = Tuning.defaults.ReactorLoad - 124

                Expect.isFalse
                    (planTasksOn (banked partial true) noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "while the mine still feeds the bank, a part load buys no courier: the remainder is the front of a queue"

                Expect.contains
                    (planTasksOn (banked partial false) noThreats)
                    (Withdraw("sto-1", Thorium))
                    "with nothing left to dig, the remainder is all there will be and the draw opens on it"

                let empty = courier "courier-last"

                let atStorage = banked partial false |> withHomeCreep { X = 13; Y = 10 } empty

                Expect.contains
                    (emitOn atStorage [ empty.Name, Withdraw("sto-1", Thorium) ])
                    (WithdrawFromStore(empty.Name, "sto-1", Thorium, Some partial))
                    "and the Intent names the remainder, where it names the whole load when there is one"

                // A pooled Task nobody may hold is not a delivery: the
                // remainder is under half this body's carry, so the
                // worth-the-trip line refuses it unless the delivery draw is
                // exempt, and the exemption was once carried by a rank
                // comparison a tier lift silently falsified.
                Expect.isTrue
                    (atStorage |> holds empty.Name (Withdraw("sto-1", Thorium)))
                    "and a body may actually hold it: the store is worth the trip because the ore has nowhere deeper to fall"

                Expect.contains
                    (emitOn
                        (banked Tuning.defaults.ReactorLoad false
                         |> withHomeCreep { X = 13; Y = 10 } empty)
                        [ empty.Name, Withdraw("sto-1", Thorium) ])
                    (WithdrawFromStore(
                        empty.Name,
                        "sto-1",
                        Thorium,
                        Some Tuning.defaults.ReactorLoad
                    ))
                    "a whole load is still a whole load"
            }

            // A remainder is not the exact `ReactorLoad` that marks a
            // delivery, so `Facts.carryingADelivery` reads the terminal state
            // beside it. The Reactor is preferred by the tier gap, not a
            // refusal: refusing the Storage outright reached the last mine
            // haul and an arriving consignment's carrier.
            test "a body holding the last load takes it to the Reactor, and may still bank it" {
                let carrier = courier "courier-holding" |> carrying 376

                let colony =
                    let ready = deliveryColony (Some Ownership.Ours)

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium =
                                    ready.Spatial.Thorium
                                    |> Map.add "sto-1" 0
                                    |> Map.add "can-min" 0
                                    |> Map.add "min-a" 0
                            }
                    }
                    |> withHomeCreep { X = 13; Y = 10 } carrier

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Refill(reactor, Thorium))
                    "the load is in flight by the only reading left: nothing is mining, so ore aboard is a delivery"

                Expect.isTrue
                    (colony |> holds carrier.Name (Refill(reactor, Thorium)))
                    "and the body prefers it to the store it was drawn from, on the tier gap alone"

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Refill("sto-1", Thorium))
                    "the Storage is still a sink: a body that cannot reach the Reactor banks the ore rather than standing on it"
            }

            // `oreStillComing` is false for every colony with nothing left to
            // dig, so a rule keyed on it alone reaches the mine's last load,
            // a crossed room's pile and an arriving consignment's carrier.
            // All three bank into the Storage, and `Planner.mineRefills` is
            // written unconditionally so that they can.
            test "a mined-out colony still banks ore too heavy for the Reactor" {
                // A carrier holding more than one load, which the Reactor's
                // own Refill turns away on `creep.Thorium <= ReactorLoad`:
                // its only sink is the Storage.
                let hauler =
                    creepWith
                        "hauler-arrival"
                        0
                        900
                        (List.replicate 30 Carry @ List.replicate 15 Move)
                    |> carrying (Tuning.defaults.ReactorLoad + 100)

                let colony =
                    let ready = deliveryColony (Some Ownership.Ours)

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium =
                                    ready.Spatial.Thorium
                                    |> Map.add "sto-1" 0
                                    |> Map.add "can-min" 0
                                    |> Map.add "min-a" 0
                            }
                    }
                    |> withHomeCreep { X = 13; Y = 10 } hauler

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Refill("sto-1", Thorium))
                    "the sink the mine haul and the arrival haul both depend on is pooled whatever the mine has become"

                Expect.isTrue
                    (colony |> holds hauler.Name (Refill("sto-1", Thorium)))
                    "and the body holds it: refusing this leaves an over-full carrier applicable to nothing (#262)"
            }

            // A tombstone beside the Reactor is season score at the far end of
            // the delivery's own walk; ranked `StockDraw` it lost every
            // travel-cost tie to the energy work at home.
            test
                "ore lying in the Reactor's room ranks with the delivery and its sink is the Reactor" {
                let tombTile = { X = 26; Y = 45 }

                let colony =
                    let ready = deliveryColony (Some Ownership.Ours)
                    let layer = SpatialInfo.layerOf ready.Spatial errandRoom

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                TargetKinds =
                                    Map.add "tomb-reactor" Tombstone ready.Spatial.TargetKinds
                                Thorium =
                                    ready.Spatial.Thorium
                                    |> Map.add "tomb-reactor" Tuning.defaults.ReactorLoad
                                    |> Map.add "sto-1" 0
                                    |> Map.add "can-min" 0
                                    |> Map.add "min-a" 0
                            }
                            |> withNeighbour
                                errandRoom
                                { layer with
                                    TargetPositions =
                                        Map.add "tomb-reactor" tombTile layer.TargetPositions
                                }
                    }

                let draw = taskId (Withdraw("tomb-reactor", Thorium))

                let pooled = poolOn colony |> List.tryFind (fun entry -> taskId entry.Task = draw)

                match pooled with
                | None ->
                    failtest
                        "the tombstone's ore is pooled, which is the premise every part of this rests on"
                | Some entry ->
                    Expect.isLessThan
                        entry.Priority
                        (priorityOfTier StockDraw)
                        "it does not rank as stock: three rooms out, stock loses every tie to the energy at home"

                    Expect.isLessThanOrEqual
                        entry.Priority
                        (priorityOfTier Feeding)
                        "it ranks with the feeding work, which is where the delivery's own draw ranks (#367)"

                    Expect.equal
                        (Capacity.capOf CapScope.Everyone entry.Capacity)
                        (Some 1)
                        "and one body goes for it: the ore is a finite remainder three crossings away"

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Refill(reactor, Thorium))
                    "the sink is the Reactor five tiles away, not the Storage three crossings back"
            }

            // The TTL clause's permissive end: an unpriceable leg refuses
            // nobody. `bareDeliveryColony` is one crossing with no ground
            // behind either landing, so every cross-room price out of it is
            // `None`. `RoomSeamTests` holds the arithmetic over the real
            // captures.
            test "an unpriceable delivery leg refuses nobody, however old the body" {
                let aged =
                    { courier "courier-aged" with
                        TicksToLive = 1
                    }

                let colony =
                    bareDeliveryColony (Some Ownership.Ours)
                    |> withHomeCreep { X = 13; Y = 10 } aged

                Expect.isNone
                    (Atlas.walkTicks (Atlas.ofView colony) aged.Name (Refill(reactor, Thorium)))
                    "the fixture's premise: this leg has no price, which is what `bareDeliveryColony` is for (#379)"

                Expect.equal
                    (Map.tryFind aged.Name (decideOn colony).Assignments)
                    (Some(taskId (Withdraw("sto-1", Thorium))))
                    "a walk the Atlas cannot price is no reason to refuse a body its work"
            }

            // The Reactor burns 1 T a tick against a 1,000-unit store, so no
            // cadence can meter the delivery; the draw is gated on the store's
            // own room, read at the draw and so strictly conservative: the
            // store drains for the whole loaded walk.
            test "the draw waits for the Reactor to have room for a whole load" {
                let withStore held =
                    deliveryColony (Some Ownership.Ours) |> withReactorStore held

                let room = Engine.reactorCapacity - Tuning.defaults.ReactorLoad

                Expect.contains
                    (planTasksOn (withStore room) noThreats)
                    (Withdraw("sto-1", Thorium))
                    "exactly one load of room opens the draw"

                Expect.isFalse
                    (planTasksOn (withStore (room + 1)) noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "one unit short of a load's room closes it: a load drawn now could not be put down"

                Expect.isFalse
                    (planTasksOn (withStore Engine.reactorCapacity) noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "and a full Reactor is the case that stranded a loaded courier on its own hot tile"

                // The sink is not gated with the draw: a load already drawn
                // must have somewhere to go while any of it fits. Only a
                // Reactor at its cap closes both, since an engine `transfer`
                // into a full store is an error, not a wait.
                let loaded = courier "courier-loaded" |> carrying 500

                let nearlyFull =
                    withStore (Engine.reactorCapacity - 1) |> withErrandCreep ringTile loaded

                let tasks = planTasksOn nearlyFull noThreats

                Expect.contains
                    tasks
                    (Refill(reactor, Thorium))
                    "the Reactor is still the sink for ore already aboard"

                Expect.isFalse
                    (tasks |> List.contains (Withdraw("sto-1", Thorium)))
                    "and the draw behind it stays shut"
            }

            // "A load admitted here has strictly more room when it lands" is
            // true of one carrier and false of two: live a hauler drew 204 T,
            // a courier drew a whole 500 behind it and landed first, and the
            // hauler reached a store of 999 with 196 T it could not put down.
            test "the draw counts the ore already walking, not only the ore already burnt" {
                let room = Engine.reactorCapacity - Tuning.defaults.ReactorLoad

                let withStore held =
                    deliveryColony (Some Ownership.Ours) |> withReactorStore held

                // One unit afloat is one unit of the room already spoken for.
                let afloat aboard held =
                    withStore held
                    |> withErrandCreep ringTile (courier "courier-walking" |> carrying aboard)

                Expect.isFalse
                    (planTasksOn (afloat 1 room) noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "exactly one load of room and one unit walking: the second draw is the one that strands"

                Expect.contains
                    (planTasksOn (afloat 1 (room - 1)) noThreats)
                    (Withdraw("sto-1", Thorium))
                    "and a unit of room to spare over what is afloat opens it again"

                // The live shape: 204 aboard against a store that leaves room
                // for a load and no more.
                Expect.isFalse
                    (planTasksOn (afloat 204 room) noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "the incident's own arithmetic: 204 walking is 204 of the store's room already claimed"

                // Counting every unit afloat counts a mine hauler's load too,
                // which is not inbound to the Reactor at all: naming which
                // body is inbound means reading the assignments the Planner is
                // blind to, and the cost of the reading is one haul cycle of
                // cadence.
                let mineHaul =
                    { withStore room with
                        Creeps =
                            (hauler "hauler-homebound" 0 100 |> carrying 400)
                            :: (withStore room).Creeps
                    }

                Expect.isFalse
                    (planTasksOn mineHaul noThreats |> List.contains (Withdraw("sto-1", Thorium)))
                    "ore walking home to the Storage defers the draw as well, deliberately"
            }

            // `SpatialInfo.Thorium` deliberately does not carry the Reactor's
            // store, and a gate that read it there once answered 0 for a
            // Reactor holding 999 while the fixture agreed with it. This case
            // writes the store into that map on purpose.
            test "the draw reads the Reactor's own row, and no Thorium map beside it" {
                let ready = deliveryColony (Some Ownership.Ours)

                let inTheWrongMap =
                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium =
                                    Map.add reactor Engine.reactorCapacity ready.Spatial.Thorium
                            }
                    }

                Expect.contains
                    (planTasksOn inTheWrongMap noThreats)
                    (Withdraw("sto-1", Thorium))
                    "a full store written where the projection never writes one changes nothing"

                Expect.isFalse
                    (planTasksOn (ready |> withReactorStore Engine.reactorCapacity) noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "the same number on the Reactor's own row closes the draw"

                // The row's absence is a closed draw, not an open one: a load
                // is better banked at home than walked towards a level nobody
                // read.
                Expect.isFalse
                    (planTasksOn { ready with Reactors = [] } noThreats
                     |> List.contains (Withdraw("sto-1", Thorium)))
                    "no vision, no row, no draw"
            }

            // `Facts.ourThoriumPiles` once filtered "a room we own", and the
            // Reactor's room has no controller at all, so its floor was
            // invisible while a CLAIM body of ours stood two tiles away.
            test "a Thorium pile on the declared Reactor's floor is ours to pick up" {
                let pileTile = { X = 26; Y = 43 }

                let withPile room amount =
                    let ready = deliveryColony (Some Ownership.Ours)
                    let layer = SpatialInfo.layerOf ready.Spatial room

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                TargetKinds =
                                    Map.add
                                        "pile-reactor"
                                        (Dropped Thorium)
                                        ready.Spatial.TargetKinds
                                Thorium = Map.add "pile-reactor" amount ready.Spatial.Thorium
                            }
                            |> withNeighbour
                                room
                                { layer with
                                    TargetPositions =
                                        Map.add "pile-reactor" pileTile layer.TargetPositions
                                }
                    }

                Expect.contains
                    (planTasksOn (withPile errandRoom 915) noThreats)
                    (Pickup("pile-reactor", Thorium))
                    "the errand room's floor is the one floor of ours that is in nobody's room"

                Expect.isFalse
                    (planTasksOn (withPile errandRoom 99) noThreats
                     |> List.contains (Pickup("pile-reactor", Thorium)))
                    "the pickup threshold is unchanged by where the pile lies"

                Expect.isFalse
                    (planTasksOn (withPile "W9S9" 915) noThreats
                     |> List.contains (Pickup("pile-reactor", Thorium)))
                    "a room we neither own nor declared is still none of ours"
            }

            // A courier that dies on the ring leaves its ore in a tombstone,
            // not on the floor; the floor check catches it only after the
            // tombstone decays into piles that bleed. The shape hand-written
            // here is one the projection has to be able to build, and
            // `ViewTests`' tombstone case is this one's other half.
            test "a tombstone of ours on that floor is ours to draw, ore and all" {
                let tombTile = { X = 26; Y = 45 }

                let withTomb room amount =
                    let ready = deliveryColony (Some Ownership.Ours)
                    let layer = SpatialInfo.layerOf ready.Spatial room

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                TargetKinds =
                                    Map.add "tomb-reactor" Tombstone ready.Spatial.TargetKinds
                                Thorium = Map.add "tomb-reactor" amount ready.Spatial.Thorium
                            }
                            |> withNeighbour
                                room
                                { layer with
                                    TargetPositions =
                                        Map.add "tomb-reactor" tombTile layer.TargetPositions
                                }
                    }

                Expect.contains
                    (planTasksOn (withTomb errandRoom 175) noThreats)
                    (Withdraw("tomb-reactor", Thorium))
                    "the errand room's tombstone is drawn on the errand room's own argument"

                // No threshold, where the pile above carries one: there is no
                // reflex that empties a store, so the alternative is the decay.
                Expect.contains
                    (planTasksOn (withTomb errandRoom 1) noThreats)
                    (Withdraw("tomb-reactor", Thorium))
                    "a store that ends is worth the trip at any holding (`worthTheTrip`, #232)"

                Expect.isFalse
                    (planTasksOn (withTomb "W9S9" 175) noThreats
                     |> List.contains (Withdraw("tomb-reactor", Thorium)))
                    "a room we neither own nor declared is still none of ours"
            }

            // A tombstone draw is the opposite errand from the Storage's in
            // every term: the ore is already in the room, the walk is over,
            // and it is decaying under a body standing next to it. Refusing
            // it would leave the colony watching the ore go rather than
            // saving a body that is going anyway.
            test "the TTL clause refuses the Storage's load and not a tombstone's in that room" {
                let tombTile = { X = 26; Y = 45 }

                let atStorage life =
                    let aged =
                        { courier "courier-aged" with
                            TicksToLive = life
                        }

                    let ready = deliveryColony (Some Ownership.Ours)
                    let layer = SpatialInfo.layerOf ready.Spatial errandRoom

                    aged,
                    { ready with
                        Spatial =
                            { ready.Spatial with
                                TargetKinds =
                                    Map.add "tomb-reactor" Tombstone ready.Spatial.TargetKinds
                                Thorium =
                                    ready.Spatial.Thorium
                                    |> Map.add "tomb-reactor" 175
                                    // The mine emptied, so the ore intakes are
                                    // the two under test: the container is
                                    // nearer than either.
                                    |> Map.add "can-min" 0
                            }
                            |> withNeighbour
                                errandRoom
                                { layer with
                                    TargetPositions =
                                        Map.add "tomb-reactor" tombTile layer.TargetPositions
                                }
                    }
                    |> withHomeCreep { X = 13; Y = 10 } aged

                let leg = loadedLegOf (atStorage Engine.creepLifetime)
                let creep, colony = atStorage (lifeNeededFor leg - 1)

                Expect.isFalse
                    (colony |> holds creep.Name (Withdraw("sto-1", Thorium)))
                    "the premise: one tick short of the loaded leg, the Storage's 500 is refused"

                Expect.isTrue
                    (colony |> holds creep.Name (Withdraw("tomb-reactor", Thorium)))
                    "and the very same body draws the tombstone: a short local errand over ore that is bleeding"
            }

            // A tombstone's 175 is under `Tuning.ReactorLoad`, so this pins
            // that no rung of the delivery's arithmetic shuts on a sub-load:
            // the 500-unit gate is on the draw from Storage, and the Reactor's
            // own Refill admits any load at or under one.
            test "a sub-load of ore has a sink: the Reactor beside it, or the Storage at home" {
                let carried = 175
                let loaded = courier "courier-part" |> carrying carried

                // The Storage half of the pair is a walk home, so it needs a
                // crossing that prices: an unpriceable sink reads as "no sink"
                // for the fixture's reason and not the rule's.
                let open' = deliveryColony (Some Ownership.Ours) |> withErrandCreep ringTile loaded

                Expect.isTrue
                    (open' |> holds loaded.Name (Refill(reactor, Thorium)))
                    "with the programme open the Reactor is the nearer sink and takes a part load"

                // The pairwise control: the bank drained under one delivery,
                // which is the one fact `courierProgrammeOpen` reads here.
                let shut =
                    { open' with
                        Spatial =
                            { open'.Spatial with
                                Thorium = Map.add "sto-1" 0 open'.Spatial.Thorium
                            }
                    }

                Expect.isFalse
                    (planTasksOn shut noThreats |> List.contains (Refill(reactor, Thorium)))
                    "the premise: a shut programme pools no Reactor Refill"

                Expect.isTrue
                    (shut |> holds loaded.Name (Refill("sto-1", Thorium)))
                    "so the load goes to the warehouse, which takes any amount that is not the delivery's own"
            }

            test "the priced pair draws exactly 500 from Storage and pours it into our Reactor" {
                let empty = courier "courier-empty"

                let atStorage =
                    deliveryColony (Some Ownership.Ours) |> withHomeCreep { X = 13; Y = 10 } empty

                Expect.contains
                    (emitOn atStorage [ empty.Name, Withdraw("sto-1", Thorium) ])
                    (WithdrawFromStore(empty.Name, "sto-1", Thorium, Some 500))
                    "the delivery is the Withdraw amount option's first bounded caller"

                let loaded = courier "courier-loaded" |> carrying 500

                let atReactor =
                    deliveryColony (Some Ownership.Ours) |> withErrandCreep ringTile loaded

                Expect.contains
                    (emitOn atReactor [ loaded.Name, Refill(reactor, Thorium) ])
                    (TransferEnergyToStructure(loaded.Name, reactor, Thorium))
                    "the multi-room leg ends as the ordinary resource-aware Refill"

                let afterMine =
                    { atReactor with
                        Spatial =
                            { atReactor.Spatial with
                                Thorium =
                                    atReactor.Spatial.Thorium
                                    |> Map.add "min-a" 0
                                    |> Map.add "sto-1" 0
                            }
                    }

                Expect.contains
                    (planTasksOn afterMine noThreats)
                    (Refill(reactor, Thorium))
                    "a drawn load keeps its sink after the start facts close"

                let partial =
                    { loaded with
                        Thorium = 998
                        FreeCapacity = 2
                    }

                let afterPartial =
                    { afterMine with
                        Creeps =
                            partial
                            :: (afterMine.Creeps
                                |> List.filter (fun creep -> creep.Name <> loaded.Name))
                    }

                Expect.contains
                    (planTasksHoldingThorium [ Refill(reactor, Thorium) ] afterPartial)
                    (Refill(reactor, Thorium))
                    "a partially poured load keeps the delivery it already holds after the start facts close"

                Expect.isFalse
                    (planTasksOn afterPartial noThreats |> List.contains (Refill(reactor, Thorium)))
                    "an arbitrary partial mine load does not open a Reactor delivery"

                let emptied =
                    { partial with
                        Thorium = 0
                        FreeCapacity = 1000
                    }

                let otherLoad = courier "mine-load" |> carrying 499

                let staleHolder =
                    { afterPartial with
                        Creeps =
                            emptied
                            :: (afterPartial.Creeps
                                |> List.filter (fun creep -> creep.Name <> loaded.Name))
                    }
                    |> withErrandCreep { ringTile with X = ringTile.X + 1 } otherLoad

                let reassigned =
                    (decideFrom
                        (Map.ofList [ emptied.Name, taskId (Refill(reactor, Thorium)) ])
                        staleHolder)
                        .Assignments
                    |> Map.tryFind otherLoad.Name

                Expect.notEqual
                    reassigned
                    (Some(taskId (Refill(reactor, Thorium))))
                    "an empty stale holder cannot hand its delivery to another partial mine load"
            }

            test "a rival-held Reactor is no sink, while an unowned one may stage the load" {
                let tasks owner =
                    deliveryColony (Some owner) |> fun colony -> planTasksOn colony noThreats

                Expect.isFalse
                    (tasks Ownership.Rival |> List.contains (Refill(reactor, Thorium)))
                    "transferring would score for the rival"

                Expect.contains
                    (tasks Ownership.Unowned)
                    (Refill(reactor, Thorium))
                    "an unowned Reactor keeps the staged load"

                let loaded = courier "courier-waiting" |> carrying 500

                let waiting =
                    deliveryColony (Some Ownership.Rival) |> withErrandCreep ringTile loaded

                Expect.isNone
                    ((decideOn waiting).Assignments |> Map.tryFind loaded.Name)
                    "the exact delivery load waits instead of returning to Storage while the rival owns the sink"
            }

            test
                "a rival's claimer beside our Reactor closes the draw and the sink though the flag is ours" {
                let rivalClaimer =
                    { hostileIn
                          errandRoom
                          { X = ringTile.X + 1; Y = ringTile.Y }
                          [ BodyPart.Claim; Move; Move ] with
                        Owner = "Shibdib"
                    }

                let clear = deliveryColony (Some Ownership.Ours)

                let contested =
                    { clear with
                        Hostiles = [ rivalClaimer ]
                    }

                Expect.equal
                    ((courierRow clear).Quota, (courierRow contested).Quota)
                    (1, 0)
                    "the flag is ours this tick and theirs the next: no load is drawn into a flag war"

                Expect.isFalse
                    (planTasksOn contested noThreats |> List.contains (Refill(reactor, Thorium)))
                    "and nothing is poured, since the Reactor burns for whoever holds it"

                Expect.isFalse
                    (planTasksHoldingThorium [ Refill(reactor, Thorium) ] contested
                     |> List.contains (Refill(reactor, Thorium)))
                    "not even a load already drawn and held"

                let loaded = courier "courier-contested" |> carrying 500

                let assigned colony =
                    (decideOn (colony |> withErrandCreep ringTile loaded)).Assignments

                Expect.equal
                    (Map.tryFind loaded.Name (assigned contested))
                    (Some(taskId (Refill("sto-1", Thorium))))
                    "the loaded courier banks it: a flag war is not a flag the re-claimer takes back next tick"

                Expect.equal
                    (Map.tryFind loaded.Name (assigned clear))
                    (Some(taskId (Refill(reactor, Thorium))))
                    "the premise: uncontested, the same body pours"
            }

            test "a drawn load banks when no errand is worked, held or shut" {
                let loaded = courier "courier-held" |> carrying 500

                let banked colony =
                    (decideOn (colony |> withHomeCreep { X = 13; Y = 10 } loaded)).Assignments
                    |> Map.tryFind loaded.Name

                let worked = deliveryColony (Some Ownership.Ours)

                Expect.notEqual
                    (banked worked)
                    (Some(taskId (Refill("sto-1", Thorium))))
                    "the premise: while the errand is worked, the drawn load is the Reactor's"

                Expect.equal
                    (banked { worked with Errands = [] })
                    (Some(taskId (Refill("sto-1", Thorium))))
                    "with no errand in the view the Storage takes it back, or it ages its body to death"
            }

            test "visible Keeper Reach pre-empts a loaded courier, which re-prices after it clears" {
                let loaded = courier "courier-fleeing" |> carrying 500

                let quiet = deliveryColony (Some Ownership.Ours) |> withErrandCreep ringTile loaded

                let keeper =
                    { hostileAt
                          "keeper"
                          { X = ringTile.X; Y = ringTile.Y + 4 }
                          [ RangedAttack; Move ] with
                        Owner = "Source Keeper"
                        Pos = RoomPos.at errandRoom { X = ringTile.X; Y = ringTile.Y + 4 }
                    }

                let threatened = { quiet with Hostiles = [ keeper ] }

                Expect.equal
                    ((decideOn threatened).Assignments |> Map.tryFind loaded.Name)
                    (Some(taskId Flee))
                    "Safety-tier Flee interrupts the delivery inside visible Reach"

                Expect.equal
                    ((decideOn quiet).Assignments |> Map.tryFind loaded.Name)
                    (Some(taskId (Refill(reactor, Thorium))))
                    "when Reach clears the same loaded body re-prices the Reactor leg"
            }
        ]

/// Ore in a room a chain merely crosses: the pool-side counterpart of
/// `ViewTests`' "only the ore comes through".
[<Tests>]
let crossedSweepTests =
    testList
        "ore in a room the chain crosses"
        [
            test "a pile and a tombstone on a crossed floor are pooled, and a stranger's are not" {
                let far = "W1N9"

                let bleeding colony =
                    colony
                    |> withPileIn far "pile-crossing" 419
                    |> withTombstoneIn far "tomb-crossing" 175

                let stranger = bleeding (deliveryColony (Some Ownership.Ours))

                let onTheWay =
                    { stranger with
                        Crossed = Set.singleton far
                    }

                Expect.isFalse
                    (planTasksOn stranger noThreats
                     |> List.contains (Pickup("pile-crossing", Thorium)))
                    "a floor we neither own, declared nor cross is not ours to walk onto"

                Expect.contains
                    (planTasksOn onTheWay noThreats)
                    (Pickup("pile-crossing", Thorium))
                    "and the same floor, once the projection says the chain crosses it, is ours to sweep"

                Expect.contains
                    (planTasksOn onTheWay noThreats)
                    (Withdraw("tomb-crossing", Thorium))
                    "the tombstone beside it too, which is what a courier that dies on the loaded leg leaves (#359)"
            }
        ]

/// The consignment: W12S28 and W13S28 sit five and six crossings from the
/// Reactor, outside `Tuning.MaxHops`, so the ore moves by terminal or not at
/// all. Three rules haul it (`Planner`) and one ships it (`Layout`).
[<Tests>]
let consignmentTests =
    testList
        "the consignment"
        [
            // A consignor's Storage is drawn for its own terminal, a leg of a
            // few tiles, and what tells it from the delivery draw is the
            // errand a consignor does not declare. Read off the verbose
            // Scoring: that the draw is scored for the worker, not rejected at
            // applicability, is the claim; which Task wins is not.
            test
                "a consignor's Storage draw keeps the worker: the Work-part clause is the delivery draw's alone" {
                let worker =
                    creepWith
                        "worker-consign"
                        0
                        600
                        (List.replicate 11 Work @ List.replicate 12 Carry @ List.replicate 12 Move)

                let scoredFor (colony: ColonyView) =
                    let { Verdicts = verdicts } =
                        decide colony Map.empty (Set.singleton worker.Name) None

                    verdicts
                    |> List.exists (function
                        | Verdict.Scoring(name, rows) when name = worker.Name ->
                            rows
                            |> List.exists (function
                                | Candidate.Scored(task, _, _, _) ->
                                    task = taskId (Withdraw("sto-1", Thorium))
                                | Candidate.Rejected _ -> false)
                        | _ -> false)

                Expect.isTrue
                    (scoredFor (
                        consigningColony 0 10_000 |> withHomeCreep { X = 13; Y = 10 } worker
                    ))
                    "in the consignor, the Storage's Thorium draw is scored for the worker"

                Expect.isFalse
                    (scoredFor (
                        deliveryColony (Some Ownership.Ours)
                        |> withHomeCreep { X = 13; Y = 10 } worker
                    ))
                    "in the colony that declared the errand, the same draw is rejected for the same body"
            }

            test
                "a declared consignee draws its bank towards the terminal, and the terminal takes it" {
                let tasks = planTasksOn (consigningColony 0 10_000) noThreats

                Expect.contains
                    tasks
                    (Withdraw("sto-1", Thorium))
                    "the banked ore is drawn out of the Storage"

                Expect.contains
                    tasks
                    (Refill("term-1", Thorium))
                    "and the terminal is where it goes"

                // `send` is paid out of the sending terminal's own energy.
                Expect.contains
                    (planTasksOn (consigningColony 0 0) noThreats)
                    (Refill("term-1", Energy))
                    "and energy follows it, because a terminal with no energy ships nothing"

                Expect.isFalse
                    (planTasksOn (consigningColony 0 Tuning.defaults.TerminalEnergy) noThreats
                     |> List.contains (Refill("term-1", Energy)))
                    "stocked to the tuned figure it takes no more: energy in a terminal buys no body"
            }

            test
                "a colony that declares no consignee walks arriving ore out of its terminal instead" {
                let arriving = planTasksOn (receivingColony 5_000) noThreats

                Expect.contains
                    arriving
                    (Withdraw("term-1", Thorium))
                    "the far end draws what landed in the terminal"

                Expect.contains
                    arriving
                    (Refill("sto-1", Thorium))
                    "and the Storage takes it, the same sink the mine's own ore uses"

                // Only a sender pays a fee, and a terminal stocked for a send
                // it will never make has taken 4,000 out of the spawn economy
                // to hold forever.
                Expect.isFalse
                    (arriving |> List.contains (Refill("term-1", Energy)))
                    "and no fee is stocked for a send this colony never makes"

                // The ore sink is gated too: the Storage's unconditional
                // Thorium sink is where a laden body puts its load down, and
                // an ungated terminal sink in a room that also draws out of
                // that terminal fed itself live.
                Expect.isFalse
                    (arriving |> List.contains (Refill("term-1", Thorium)))
                    "and the receiving end never offers its terminal as a sink: that is the loop, and the Storage is the sink a laden body needs"

                Expect.contains
                    (planTasksOn (receivingColony 5_000) noThreats)
                    (Refill("sto-1", Thorium))
                    "which the Storage's own unconditional Thorium sink already answers, as it does for the mine's ore"

                // The two directions must never both be pooled in one colony:
                // a room that ships out and draws in would cycle its ore
                // between two stores forever.
                Expect.isFalse
                    (planTasksOn (consigningColony 5_000 10_000) noThreats
                     |> List.contains (Withdraw("term-1", Thorium)))
                    "a shipping colony never draws out of its own terminal"
            }

            test "the send ships what the terminal holds, priced by the engine's own fee" {
                // W1N1 to W1N4 is three rooms, which is `getRoomLinearDistance`
                // and not the hop count: the fee is `ceil(amount · (1 −
                // e^(−3/30)))`, about 95 energy a thousand.
                let fee = Engine.sendFee 3 10_000

                Expect.equal fee 952 "the engine's arithmetic, read off `calcTerminalEnergyCost`"



                Expect.equal
                    (sendsOn (consigningColony 10_000 fee))
                    [ "term-1", Thorium, 10_000, "W1N4" ]
                    "energy exactly covering the fee ships the whole store"

                Expect.equal
                    (sendsOn (consigningColony 10_000 0))
                    []
                    "and no energy ships nothing at all, which is W12S28's live state"
            }

            test "a terminal that cannot pay for all of it ships what it can" {
                // The amount is scaled by the ratio the fee overshoots by. The
                // fee is linear in the amount — the exponential is in the range
                // alone — so one correction lands, and the assertion is that
                // what is shipped is affordable rather than that it is maximal.
                let shipped = sendsOn (consigningColony 10_000 500)

                match shipped with
                | [ _, _, amount, _ ] ->
                    Expect.isLessThan
                        amount
                        10_000
                        "less than the store, because the energy does not cover it"

                    Expect.isLessThanOrEqual
                        (Engine.sendFee 3 amount)
                        500
                        "and the fee on what is shipped is paid by the energy that is there"
                | other -> failtestf "expected one send, got %A" other
            }

            test "the floors: below the engine's minimum, or with nothing declared, nothing ships" {
                Expect.equal
                    (sendsOn (consigningColony (Engine.terminalMinSend - 1) 10_000))
                    []
                    "under 100 units the engine refuses a send outright, so the tail waits to go in one lot"

                Expect.equal
                    (sendsOn (consigningColony Engine.terminalMinSend 10_000))
                    [ "term-1", Thorium, Engine.terminalMinSend, "W1N4" ]
                    "at the minimum it goes"

                Expect.equal
                    (sendsOn (receivingColony 10_000))
                    []
                    "and a colony with no consignee declared ships nothing, whatever its terminal holds"
            }
        ]

/// The one Thorium draw whose sink is the Reactor, and the tier it ranks on:
/// a programme at the far end turns the load into score, and a store there
/// burns 1 T a tick whether or not the load arrives.
[<Tests>]
let deliveryRankTests =
    testList
        "the delivery's rank"
        [
            test "the delivery's own draw outranks the energy hauling beside it" {
                // With source containers, the arrival haul and the mine haul
                // there is always energy work, so "the work a body does when
                // it has no better" became work nobody ever did.
                let delivering = deliveryColony (Some Ownership.Ours)

                let rankOf colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                // Stated as a tier and not against a neighbour task, because
                // it competes with every Feeding-tier intake and this fixture
                // carries only some of them.
                Expect.isLessThan
                    (rankOf delivering (Withdraw("sto-1", Thorium)))
                    (Some(priorityOfTier StockDraw))
                    "the score's own load is no longer stock work: it beats every StockDraw rung outright"

                Expect.isLessThanOrEqual
                    (rankOf delivering (Withdraw("sto-1", Thorium)))
                    (Some(priorityOfTier Feeding))
                    "and it sits inside the Feeding tier, which is the tier the energy hauling it lost 465 ticks to is on"

                Expect.equal
                    (rankOf delivering (Withdraw("sto-1", Thorium))
                     |> Option.map (fun rank -> rank - priorityOfTier Feeding))
                    (Some -2)
                    "two rungs up inside it — the rungs are the Storage's own and this change does not touch them"

                Expect.isGreaterThanOrEqual
                    (rankOf (mineHaulColony |> withMineStock 600) (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw - tierRungs / 2))
                    "and the mine's own haul stays on the Storage's tier: ore *into* the Storage is exactly the stock work decision 3 ranked"
            }

            test "and it admits one body, not one per banked load" {
                // The ordinary Withdraw capacity divides the store by the
                // load, which for a bank of 34,876 T is 69 holders — at the
                // top of Feeding, every idle Carrier walking 500 T three rooms
                // out. One, because the programme is one body by construction.
                let capOf colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Capacity else None)

                let delivering = deliveryColony (Some Ownership.Ours)

                Expect.equal
                    (capOf delivering (Withdraw("sto-1", Thorium))
                     |> Option.bind (Capacity.capOf CapScope.Everyone))
                    (Some 1)
                    "one courier's worth of the bank, whatever the bank holds"

                // Pairwise against the mine's own draw, which keeps its
                // load-divided cap: 1,200 of ore over this fixture's 200 load
                // is six bodies, right for a haul of a few tiles in one room.
                Expect.equal
                    (capOf (mineHaulColony |> withMineStock 1200) (Withdraw("can-min", Thorium))
                     |> Option.bind (Capacity.capOf CapScope.Everyone))
                    (Some 6)
                    "while the mine's draw still answers the number its own stock divides into loads"
            }
        ]
