/// The [[errand]] as `decide` sees it (ADR 0060 decision 1, ADR 0057 decision
/// 5): the one Task a declared controller-less room offers, who may hold it,
/// what caps it, and the act it fires — which is fired on a tick the target is
/// **not ours** and on no other. The projection half of an errand — what a
/// colony may carry of that room at all — is `ViewTests`' and stays there; this
/// is the work the declaration buys, so it lives in `Decide`'s own suite beside
/// the outposts' rather than in the view's.
module Fabot.Core.Tests.Decide.ErrandTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

/// The errand these cases run and the tiles they name are `Fixtures`' own
/// (#318) — one spelling for this suite and the reserver row's, which reads the
/// same declaration for what it costs. The room is a neighbour of the fixtures'
/// home, so the chain is one crossing and nothing here is about the walk: the
/// live errand is three crossings out and `RoomSeamTests` prices it.
let private errandRoom = reactorErrand.RoomName
let private reactor = reactorId
let private ringTile = reactorRing

/// A CLAIM body of ours: `[Claim; Move]`, 650 energy, the one block the
/// reserver row casts and so the one the re-claimer is (ADR 0057 decision 5,
/// ADR 0006 — a second pattern row would be the same block under a second
/// name).
let private claimer name =
    creepWith name 0 0 [ BodyPart.Claim; Move ]

let private courier name =
    creepWith name 0 1000 (List.replicate 20 Carry @ List.replicate 10 Move)

/// The complete delivery programme on the one-hop decision fixture. The live
/// three-hop price is pinned by `RoomSeamTests`; here each test moves one fact
/// that opens the row or one end of its Withdraw→Refill cycle.
let private deliveryColony owner =
    let resident = claimer "relay"

    let colony =
        { mineHaulColony with
            Bank = bank 2300 2300
            Spatial =
                { mineHaulColony.Spatial with
                    Thorium =
                        mineHaulColony.Spatial.Thorium
                        |> Map.add "sto-1" 2997
                        |> Map.add reactorId 0
                }
        }

    colony
    |> withReactorErrand
    |> withReactorOwner owner
    |> standingInErrand [ resident, reactorRing ]

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
/// out altogether, which is what a gapped relay reads and what the act treats
/// as *not ours* (ADR 0004).
let private errandColony owner creeps (colony: ColonyView) =
    colony |> withReactorErrand |> withReactorOwner owner |> standingInErrand creeps

/// The fixtures' home room with nothing of its own to offer: no controller, no
/// refillable with room, no source placed — so the pool a case reads is the
/// errand's and the comparison is pairwise (the orchestration note: a pool
/// holding three rivals proves nothing about the two that lost).
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

/// The Task **id** one named body holds this tick, which is what the assignment
/// table is keyed in. `holds` below is what a case should reach for: it takes
/// the Task itself, so a case names the Task and never spells the string.
let private assignedTask name (colony: ColonyView) =
    let { Assignments = assignments } = decideOn colony
    Map.tryFind name assignments

/// The Task id of one named body's assignment, for the two cases that compare
/// against a Task rather than reading one back.
let private holds name task (colony: ColonyView) =
    assignedTask name colony = Some(taskId task)

/// The `ClaimReactor` Intents one tick emits.
let private reclaimIntents (colony: ColonyView) =
    let { Intents = intents } = decideOn colony

    intents
    |> List.choose (function
        | ClaimReactor(name, id) -> Some(name, id)
        | _ -> None)

/// Shibdib's live W15S25 defender at t444,287: enough ranged damage to kill
/// the 200-hit re-claimer, and enough healing that the guard arithmetic wants
/// two blocks rather than one. The exact body is the independent premise that
/// distinguishes this incident from an overwhelming raid the existing
/// stand-down already handles.
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
                    |> Observe.foldRaids Observe.capEpisodes Set.empty colony

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

                let folded colony =
                    Observe.RaidState.empty
                    |> Observe.foldRaids Observe.capEpisodes Set.empty { colony with Time = tick }
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

                let log =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { admitted with
                            Time = 100
                            Hostiles = [ defender ]
                        }

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
                // ADR 0060 decision 1's narrowing said in the pool: the errand
                // room's one target is work and nothing else in that room is,
                // so the Task is pooled off the **declaration** and never off a
                // kind census — which is what keeps `Errand.place`'s kind-less
                // target enumerable by no pool that sweeps a kind.
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
                // The other half of the same rule: the Task is the
                // declaration's, so a colony without one offers none however
                // much of that room it can see.
                let colony =
                    bareHome
                    |> errandColony (Some Ownership.Rival) []
                    |> fun c -> { c with Errands = [] }

                Expect.isEmpty
                    (reclaimsOf colony)
                    "no declaration, no Task — the room's vision buys nothing"
            }

            test "a CLAIM body may hold it and a worker may not" {
                // Part arithmetic and nothing else (ADR 0006, ADR 0057
                // decision 5): `claimReactor` is a CLAIM part's act, and a
                // re-claimer carries nothing and asks for no energy state.
                // Pairwise: one rival at a time, and the pool holds one Task.
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
                // ADR 0057 decision 5's capacity, and the Reserve's own
                // argument one room further out: the flag is taken by one
                // touch of one CLAIM part, so a second body beside it buys
                // nothing at all.
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
                // The one task-specific exception to ADR 0026 (#329): a
                // Reclaim incumbent stops consuming the handover seat when a
                // candidate can land with `ReclaimerOverlap` ticks of its life
                // left. Ordinary capped Tasks continue to count a holder that
                // survives through the candidate's arrival.
                //
                // Three plain tiles between the relief and the ring, so the
                // walk is three ticks for a one-fatigue-part body. Twenty-nine
                // ticks of incumbent life would leave 26 at arrival and still
                // blocks; twenty-eight leaves the chosen 25-tick overlap and
                // admits the relief. The permanent cap remains one: the case
                // above puts two fresh bodies on the ring and admits only one.
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
                // The board this row was cut for (ADR 0060 decision 3): W15S25
                // is `Odiodin`'s, any Thorium delivered scores for him, and
                // `claimReactor` has no cooldown and no ownership precondition
                // — so the claim is the **first** act of the programme and
                // fires against a standing owner.
                let colony =
                    bareHome |> errandColony (Some Ownership.Rival) [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "the act is issued against the rival's ownership"
            }

            test "it is ours already, so the body stands there and says nothing" {
                // "Every other tick the body stands there and says nothing,
                // which is what resident means" (ADR 0057 decision 5). The act
                // is withheld and the Task is not: the body keeps its Reclaim,
                // holds the tile and goes on being the colony's only eye on the
                // room.
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
                // ADR 0004's per-entry absence, read the safe way round. The
                // relay is the colony's only vision of the room, so the tick a
                // body lands is the first tick there is an answer — and a
                // withheld act on a missing fact would leave the flag with
                // whoever planted it until the *next* tick.
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
                "the row opens only behind a diggable mine, a full load and the resident re-claimer" {
                let ready = deliveryColony (Some Ownership.Ours)

                Expect.equal
                    (courierRow ready).Quota
                    1
                    "all four current facts open one cadence seat"

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

                // At the shipped 636-tick cadence this income still hires four
                // generalists. Buying the same 1,500-energy courier every tick
                // costs 2,250,000 over a worker life and leaves only the Task
                // floor: a fixed one-cast charge, or no charge, would leave the
                // row at four.
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
                        ready |> withExtractorSite, "an extractor site is not a diggable deposit"
                        exhausted, "an exhausted deposit closes the row"
                        short, "499 Thorium is not one delivery load"
                        noResident, "the delivery waits behind the re-claimer"
                    ] do
                    Expect.equal (courierRow colony).Quota 0 reason
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

            // #354's third clause, at the one end this fixture can show. The
            // clause: a delivery draw is refused a body that cannot outlive the
            // loaded leg, priced at three ticks of life per tick walked
            // (`Tuning.MineContactAgeing`) — a body that dies loaded does not
            // lose a body, it loses the ore, through a tombstone that cooks its
            // own tile and drops a pile that bleeds at 1 T a tick.
            //
            // What is pinned here is its **permissive** end (ADR 0004): an
            // unpriceable leg refuses nobody. This fixture is one crossing with
            // no ground behind either landing (ADR 0062) and a nameless home
            // layer, so every cross-room price out of it is `None` — which is
            // why the refusal itself is pinned nowhere yet and #354 carries the
            // ticket for it. `RoomSeamTests` holds the arithmetic over the real
            // captures: 159 loaded ticks, 477 of life at the contact rate.
            test "an unpriceable delivery leg refuses nobody, however old the body" {
                let aged =
                    { courier "courier-aged" with
                        TicksToLive = 1
                    }

                let colony =
                    deliveryColony (Some Ownership.Ours) |> withHomeCreep { X = 13; Y = 10 } aged

                Expect.isNone
                    (Atlas.walkTicks (Atlas.ofView colony) aged.Name (Refill(reactor, Thorium)))
                    "the fixture's premise: this leg has no price"

                Expect.equal
                    (Map.tryFind aged.Name (decideOn colony).Assignments)
                    (Some(taskId (Withdraw("sto-1", Thorium))))
                    "a walk the Atlas cannot price is no reason to refuse a body its work"
            }

            // #354. The Reactor burns exactly 1 T a tick against a
            // 1,000-unit store, so nothing about a *cadence* can meter this
            // delivery: 999 T every 636 ticks is 1.57 T a tick, and the
            // surplus has nowhere to be but a courier's store or the floor —
            // which is where 915 T of it went. The draw is gated on the
            // store's own room instead, read at the draw and so strictly
            // conservative: the store drains for the whole loaded walk, so a
            // load admitted here has more room when it lands than when it left.
            test "the draw waits for the Reactor to have room for a whole load" {
                let withStore held =
                    let ready = deliveryColony (Some Ownership.Ours)

                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium = Map.add reactor held ready.Spatial.Thorium
                            }
                    }

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
                // must have somewhere to go while any of it fits, which is the
                // same reason the start facts closing does not strand one. A
                // store with room for one unit and not for one load closes the
                // draw and keeps the sink — and only a Reactor at its cap
                // closes both, which is the `stored < reactorCapacity` rule
                // this leaves alone: an engine `transfer` into a full store is
                // an error, not a wait.
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

            // The other half of #354: the ore that reached the floor could be
            // named by nobody. `Facts.ourThoriumPiles` filtered "a room we
            // own", and the Reactor's room has no controller at all, so it is
            // owned by nobody and its floor was invisible — while a CLAIM body
            // of ours stood two tiles away and the pile decayed at 1 T a tick.
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
