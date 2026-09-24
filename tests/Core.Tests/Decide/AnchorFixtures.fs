/// The Anchor suite's fixtures: the Posts, garrisons and successions the
/// cases are staged on.
module Fabot.Core.Tests.Decide.AnchorFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The heavy-pin fixture: the source in wall at (10,10) with eight open
/// neighbours, the built container "cont-1" on the Seat (11,10) — the one
/// Post — and a plain corridor east to the controller at (40,10), so the
/// controller is the only rival Harvest has.
let pinnedRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "cont-1", { X = 11; Y = 10 }
              "ctrl-1", { X = 40; Y = 10 }
          ]
          (openSeats { X = 10; Y = 10 } @ [ for x in 11..39 -> { X = x; Y = 10 }, Plain ]) with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "cont-1", Structure BuiltKind.Container
                    "ctrl-1", Controller
                ]
    }

/// The heavy-pin colony: the creeps of the test's choosing standing where
/// the test puts them, the source the given number of ticks from its
/// restock, and no spawn to cast anything that would crowd the pool.
let pinnedCrowd ticks (placed: (CreepInfo * Pos) list) =
    { bareRespawn with
        Spawns = []
        Refillables = []
        Sources = [ drained "src-a" ticks ]
        Controller = Some(controllerAt 2)
        Creeps = placed |> List.map fst
        Spatial =
            pinnedRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                })
    }

/// The same colony holding one body: the shape most of these cases take.
let pinnedColony ticks (creep: CreepInfo) pos = pinnedCrowd ticks [ creep, pos ]

/// The W12S28 colony with its two source containers taken away, so the
/// only Post in the projection is whatever an outpost carries — the one
/// arrangement where the Anchor row's ceiling can be read off a cast body.
let internal withoutHomePosts (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-a" |> Map.remove "can-b"
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions =
                        layer.TargetPositions |> Map.remove "can-a" |> Map.remove "can-b"
                })
    }

/// The W12S28 colony at a 1,300 bank with `postedOutpostColony`'s posted
/// outpost source beside it and a fleet of one worker, so every Post is an
/// unfilled Anchor gap. 700 of the 1,300 goes on the first body, so the
/// tick casts exactly one Anchor whatever the gap. Two dials: whether the
/// home room keeps its Posts, and who holds W1N2.
let internal anchorCapColony homePosts (control: (string * RoomControlInfo) list) =
    let rock = { X = 40; Y = 40 }

    let colony =
        { incomeColony with
            Bank = bank 1300 1300
            Sources = incomeColony.Sources @ [ source "src-out" ]
            Creeps = [ worker "w1" 0 50 ]
        }
        |> (if homePosts then id else withoutHomePosts)
        |> withOutpost
            "W1N2"
            [
                "src-out", rock, Source
                "can-out", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
            ]
            (threeSeatField rock)

    { colony with
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

/// The one Anchor body the tick casts, for the fixtures whose bank buys
/// exactly one.
let internal anchorCastBy colony =
    match anchorCastsBy colony with
    | [ body ] -> body
    | other -> failtest $"expected exactly one Anchor SpawnCreep intent, got %A{other}"

/// The colony the Anchor row's charge is legible in: W12S28 without its two
/// Posts, at a 1,400 bank, with three neutral rocks a room away, each with
/// its container standing — three Posts, three Anchors, all under the
/// neutral ceiling. Its fleet is whole but for the workers.
///
/// Why 1,400 and not the cast fixture's 1,300: the amortization is deducted
/// from income before the surplus is divided into worker places, and a
/// place is a whole body's lifetime Work drain — 10,500 energy at this bank
/// — so a charge that moves by 350 an Anchor is invisible unless the
/// surplus straddles a boundary. Three Posts move it by 1,050: 21,450
/// charged at the cast body against 20,400 at the held one, three places
/// and two. `homePosts` keeps the home Posts too: five Posts over two
/// rates, charged 2 × 700 + 3 × 400 Post by Post where a quota times one
/// ceiling charges 5 × 700.
let internal anchorChargeColony homePosts workers =
    let rocks = [ { X = 10; Y = 40 }; { X = 20; Y = 40 }; { X = 30; Y = 40 } ]

    let outpost =
        rocks
        |> List.mapi (fun i rock ->
            [
                $"src-out{i}", rock, Source
                $"can-out{i}", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
            ])
        |> List.concat

    let colony =
        { incomeColony with
            Bank = bank 1400 1400
            Sources = incomeColony.Sources @ [ for i in 0..2 -> source $"src-out{i}" ]
            Creeps =
                [ for i in 1..3 -> anchor $"a{i}" 0 50 ]
                @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]
        }
        |> (if homePosts then id else withoutHomePosts)
        |> withOutpost "W1N2" outpost (rocks |> List.collect threeSeatField)

    { colony with
        RoomControl = Map.add "W1N2" neutralRoom colony.RoomControl
    }

/// A lane with one Post at one end and the spawn at the other: the source
/// in wall at (10,10), its container on the Seat (11,10), the spawn at
/// (21,10). Its one free neighbour is (20,10), so a replacement is born
/// there and walks nine steps, not ten.
let successionRoom =
    { spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
        Stores = Map.ofList [ "can-src", 0 ]
    }
    |> withObstacles [ { X = 21; Y = 10 } ]
    |> withTargets
        [
            "src-a", { X = 10; Y = 10 }, Source
            "can-src", { X = 11; Y = 10 }, Structure BuiltKind.Container
            "spawn-1", { X = 21; Y = 10 }, Structure BuiltKind.Spawn
        ]

/// The lane's colony. Its controller is unplaced and every creep below is
/// empty, so the one Task any of them can hold is the lane's Harvest.
let successionColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = successionRoom
    }

/// A succession in the lane: the incumbent Anchor on the Post with the
/// given ticks left to live, its successor nine steps away at (20,10).
let succession incumbent successor life =
    { successionColony with
        Creeps = [ anchor incumbent 0 50 |> withLife life; anchor successor 0 50 ]
        Spatial =
            successionRoom
            |> withCreepsAt [ incumbent, { X = 11; Y = 10 }; successor, { X = 20; Y = 10 } ]
    }

/// The same lane at an RCL3 bank, where the Anchor row's body is 5W/1C/1M
/// and both creeps are that body: ten cost units a plain step, 21 ticks in
/// the spawner.
let rcl3Succession incumbent successor life =
    let rcl3Anchor name =
        creepWith name 0 50 [ Work; Work; Work; Work; Work; Carry; Move ]

    { successionColony with
        Bank = bank 600 600
        Creeps = [ rcl3Anchor incumbent |> withLife life; rcl3Anchor successor ]
        Spatial =
            successionRoom
            |> withCreepsAt [ incumbent, { X = 11; Y = 10 }; successor, { X = 20; Y = 10 } ]
    }

/// The creeps a tick released and why — the release fold's own output,
/// read without the Task it dropped.
let releases verdicts =
    verdicts
    |> List.choose (function
        | Verdict.Released(creep, _, reason) -> Some(creep, reason)
        | _ -> None)

/// A body of the given row at the live RCL5 bank, full: energy on board
/// and no free capacity, which is the state every delivery Task asks for
/// and the state that ends a Withdraw. The name is the row's, so a failure
/// message says which body it was.
let internal castFull pattern =
    let body = bodyFor pattern 1800

    creepWith pattern.Name (50 * (body |> List.filter ((=) Carry) |> List.length)) 0 body

/// The upgrader row's body at that bank: `11W/1C/11M`.
let internal upgraderBody = castFull upgraderPattern

/// The generalist at the same bank: `9W/9C/9M`, one Carry per Work where
/// the gate's line is one per four.
let internal workerBody = castFull workerPattern

/// The Anchor row's live body: `6W/1C/1M`, a standing body by the same
/// arithmetic as the upgrader's (`1 * 4 < 6`).
let internal anchorBody = castFull anchorPattern

/// The assignment one body takes in the lane, with the given furniture at
/// (15,10).
let internal laneAssignment furniture sites creep =
    let { Assignments = assignments } =
        decide (bufferLaneColony furniture sites creep) Map.empty Set.empty None

    Map.tryFind (creep: CreepInfo).Name assignments

/// The delivery lane: the same corridor with one hungry spawn standing at
/// (15,10) and nothing else at all — no controller, no source, no site —
/// so the Refill of that spawn is the only Task in the pool and an empty
/// assignment map means the gate and nothing else.
let internal deliveryLaneColony creep =
    { bareRespawn with
        Bank = bank 1800 1800
        Sources = []
        Controller = None
        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
        Creeps = [ creep ]
        Spatial =
            spatial [] bufferLaneField
            |> withTargets [ "spawn-1", { X = 15; Y = 10 }, Structure BuiltKind.Spawn ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.singleton { X = 15; Y = 10 }
                    CreepPositions = Map.ofList [ (creep: CreepInfo).Name, { X = 14; Y = 10 } ]
                })
    }

let internal deliveryAssignment creep =
    let { Assignments = assignments } =
        decide (deliveryLaneColony creep) Map.empty Set.empty None

    Map.tryFind (creep: CreepInfo).Name assignments

/// A live Anchor's shape, `6W/1C/1M`: Work-heavy (`6 > 1`) and a standing
/// body (`1 × 4 < 6`).
let internal postBody name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Work; Work; Work; Work; Work; Carry; Move ]

/// The outpost rock at (10,46) with its container gone and the plan's site
/// back on the Seat at (10,45) — the live shape after an invader demolished
/// three of them — and one body of the caller's shape where the caller
/// puts it. No controller, no refillable, no home creep: the caller's
/// bodies are the whole colony. Two rosters because the cap cases need
/// both ends of the Seam.
let internal raisingCrowd kind (outpostCreeps: (CreepInfo * Pos) list) homeCreeps =
    let colony =
        northBorderColony { X = 10; Y = 38 }
        |> withNorthOutpost (Some { X = 10; Y = 46 })
        |> withOutpostSiteOf kind { X = 10; Y = 45 }

    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let placed (creeps: (CreepInfo * Pos) list) =
        creeps |> List.map (fun (c, at) -> c.Name, at) |> Map.ofList

    { colony with
        Creeps = outpostCreeps @ homeCreeps |> List.map fst
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = placed homeCreeps
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = placed outpostCreeps
                }
    }

/// The one-body case the cases below are mostly written on.
let internal raisingColony kind (body: CreepInfo) (at: Pos) = raisingCrowd kind [ body, at ] []

/// The same colony at home: a rock at (10,10) walled in but for its two
/// Seats, a container site on one of them, and one body standing on it.
let internal homeRaisingColony kind (body: CreepInfo) (at: Pos) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-a" ]
        ConstructionSites = [ { Id = "can-a"; Left = siteOwes } ]
        Creeps = [ body ]
        Spatial =
            spatial
                []
                [
                    { X = 9; Y = 10 }, Plain
                    { X = 10; Y = 10 }, Wall
                    { X = 11; Y = 10 }, Plain
                ]
            |> withTargets
                [ "src-a", { X = 10; Y = 10 }, Source; "can-a", { X = 9; Y = 10 }, Site kind ]
            |> withCreepsAt [ body.Name, at ]
    }
