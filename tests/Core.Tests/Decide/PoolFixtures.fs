/// The pool suite's fixtures: the colonies, stores and piles the Tasks
/// are pooled from.
module Fabot.Core.Tests.Decide.PoolFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The refill cluster on open ground: the spawn at (10,10) and two
/// extensions south of it in the column x = 10, every structure tile an
/// obstacle and a 3-wide plain band around them.
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

let pickups intents =
    intents
    |> List.choose (function
        | PickupPile(creep, pile) -> Some(creep, pile)
        | _ -> None)

/// A dropped energy pile at (10,10) on open ground, with the given creeps
/// on the given tiles.
let pileColony creeps positions =
    reflexColony "pile-1" (Dropped Energy) creeps positions

/// The same colony with a second room's layer beside its own, whose
/// coordinates deliberately collide with `pileColony`'s. A `Pos` carries no
/// room, so a reflex that unioned the two rooms' piles or creeps would pair
/// across the border at range 0 and emit a pickup the engine answers
/// ERR_NOT_IN_RANGE (#166).
let internal withPileRoom room piles positions (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain =
                                TerrainGrid.ofList
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
                    ||> List.fold (fun kinds (id, _) -> Map.add id (Dropped Energy) kinds)
            }
    }

/// The stores the pool draws and the structures it fills, in energy. The
/// Thorium pair has its own cases, which name it rather than counting ids.
let withdrawTasks tasks =
    tasks
    |> List.choose (function
        | Withdraw(storeId, Energy) -> Some storeId
        | _ -> None)

let refillTasks tasks =
    tasks
    |> List.choose (function
        | Refill(structureId, Energy) -> Some structureId
        | _ -> None)

/// The stock fixture: the tier corridor with the Storage standing at
/// (16,11), between the tower and the buffer and clear of the controller's
/// Upgrade Work Area, which reaches back only to x = 17.
let stockRoom =
    tierRoom
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.add { X = 16; Y = 11 } layer.Obstacles
        })
    |> withTargets [ "sto-1", { X = 16; Y = 11 }, Structure BuiltKind.Storage ]

/// The stock colony with the given hunger and stores: one loaded
/// Carry-only body beside the Storage, so the deepest tier costs it
/// nothing to reach and every shallower one costs more. Only rank can
/// outbid the stock here, and each caller leaves it exactly one rival.
let stockColony refillables stores =
    { bareRespawn with
        Sources = []
        Refillables = refillables
        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
        Spatial = { stockRoom with Stores = stores } |> withCreepsAt [ "h1", { X = 16; Y = 10 } ]
    }

/// The draw fixture: a two-row plain corridor, y = 10..11, x = 8..22, the
/// source walled in at (8,10) with its container on (9,10), the Storage off
/// the lane at (17,11), the upgrade buffer at (21,10) beside the controller
/// at (22,10). A creep at (13,10) is three plain steps from either store's
/// Work Area, so travel cost ties the two intakes; one further east stands
/// inside the stock's Work Area and six steps from the container's.
let drawRoom =
    let lane =
        [
            for x in 8..22 do
                for y in 10..11 -> { X = x; Y = y }, (if x = 8 && y = 10 then Wall else Plain)
        ]

    spatial [] lane
    |> withObstacles [ { X = 17; Y = 11 }; { X = 22; Y = 10 } ]
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
        Spatial = { drawRoom with Stores = stores } |> withCreepsAt [ creep.Name, pos ]
    }

/// The restock dispatch corridor: a one-tile lane y = 10 from x = 9 to
/// x = 21 with the source in wall at (10,10). An empty worker unit pays a
/// whole tick per plain step, so a creep at (15,10) is a walk of four ticks
/// from the Seat it can reach.
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
        Spatial = restockRoom |> withCreepsAt [ name, pos ]
    }

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
        Spatial = crowdRoom nearStock farStock |> withCreepsAt creeps
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
    |> List.choose (fun (name, tid) ->
        if tid = taskId (Withdraw(storeId, Energy)) then
            Some name
        else
            None)

/// The stock-crowding fixture: one Storage at (13,10) and the same three
/// haulers abreast inside its Work Area. One hungry spawn keeps the stock's
/// draw gate open; the haulers are empty, so its Withdraw is the only Task
/// they can take.
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

/// The upgrade buffer's crowd: the controller at (10,10) with its buffer
/// "can-buf" at (12,10), inside the Upgrade Work Area and on no Seat, and
/// an ordinary container "can-far" holding the same 900 at (30,10). The
/// bank is 1,800, where the cast hauler carries 1,200 a trip and the cast
/// worker 450. The three creeps are empty cast worker bodies, so the two
/// Withdraws are the whole of the pool they can take.
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

/// The names assigned to one pile, in name order.
let pickersOf assignments pileId =
    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) ->
        if tid = taskId (Pickup(pileId, Energy)) then
            Some name
        else
            None)

/// The same field with a tombstone at (10,10) holding the given energy.
/// Deliberately not in `Obstacles`: a tombstone lies on the tile a creep
/// died on and the engine lets another walk over it, so its Work Area
/// includes its own tile.
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
            |> withCreepsAt creeps
    }

/// A stocked container at (10,10) with a dropped pile the case places, and
/// one empty hauler beside the container. The bank is 1,800, so the hauler
/// row's cast carries 1,200 and the 150 on the ground is a fraction of a
/// trip: at a bank whose row hauls 100 there is no pooled pile that the
/// tile rule under test would be the only lift for.
let sameTilePileColony pilePos =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Creeps = [ hauler "h1" 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "can-a", 400; "pile-a", 150 ]
            }
            |> withTargets
                [
                    "can-a", { X = 10; Y = 10 }, Structure BuiltKind.Container
                    "pile-a", pilePos, (Dropped Energy)
                ]
            |> withCreepsAt [ "h1", { X = 10; Y = 11 } ]
    }

/// A stocked container at (10,10) with the hauler beside it and a dropped
/// pile five tiles down the lane, each stocked by the case. At the 1,800
/// bank half a load is six hundred.
let internal pileDownTheLane containerStock pileAmount =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Creeps = [ hauler "h1" 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "can-a", containerStock; "pile-a", pileAmount ]
            }
            |> withTargets
                [
                    "can-a", { X = 10; Y = 10 }, Structure BuiltKind.Container
                    "pile-a", { X = 16; Y = 10 }, (Dropped Energy)
                ]
            |> withCreepsAt [ "h1", { X = 11; Y = 10 } ]
    }

/// A hungry spawn at (12,10), a half-loaded hauler beside it at (11,10),
/// and a pile nineteen tiles down the lane. The bank is the case's, because
/// half the row's cast is what the pile is measured against.
let internal pileAgainstAHungrySpawn bankEnergy pileAmount =
    { bareRespawn with
        Bank = bank bankEnergy bankEnergy
        Sources = []
        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
        Creeps = [ hauler "h1" 600 600 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "pile-a", pileAmount ]
            }
            |> withTargets
                [
                    "spawn-1", { X = 12; Y = 10 }, Structure BuiltKind.Spawn
                    "pile-a", { X = 30; Y = 10 }, (Dropped Energy)
                ]
            |> withCreepsAt [ "h1", { X = 11; Y = 10 } ]
    }

/// A tombstone holding 1,500 at (12,10), an empty hauler beside it and a
/// pile nineteen tiles down the lane, at the 1,800 bank.
let internal pileAgainstATombstone pileAmount =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Creeps = [ hauler "h1" 0 1200 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "tomb-a", 1500; "pile-a", pileAmount ]
            }
            |> withTargets
                [
                    "tomb-a", { X = 12; Y = 10 }, Tombstone
                    "pile-a", { X = 30; Y = 10 }, (Dropped Energy)
                ]
            |> withCreepsAt [ "h1", { X = 11; Y = 10 } ]
    }
