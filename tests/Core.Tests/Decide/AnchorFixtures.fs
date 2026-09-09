/// The Anchor: the work-heavy body pinned to its own rock (ADR 0048) — the Post
/// it harvests from and raises (ADR 0020, ADR 0051, ADR 0053), the Work ceiling
/// its source saturates at (ADR 0021), the standing body's own Refill, and the
/// lead that hands a Post on before its holder expires (ADR 0026).
/// The Anchor suite's fixtures: the Posts, garrisons and successions the
/// cases below are staged on.
module Fabot.Core.Tests.Decide.AnchorFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The heavy-pin fixture (ADR 0048): the source embedded in wall at
/// (10,10) with its eight neighbours open, the built container "cont-1"
/// standing on the Seat (11,10) — the source's one Post — and a plain
/// corridor running east from that Post to the controller at (40,10),
/// whose Upgrade Work Area is a room's width from the source. No Dual
/// Seat, so nothing a heavy body does here it can do in two places at
/// once, and the controller is the only rival Harvest ever has.
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

/// The heavy-pin room with a second container: "cont-2" on the Seat (9,10)
/// beside "cont-1" on (11,10), so the rock carries **two** Posts and the cap
/// admits two garrisons. One Post cannot tell a count of holders from a count
/// of tiles apart — one body standing on its own Post satisfies both readings —
/// so the union the Post cap takes of the two (#269) is only visible on a rock
/// with a Post to spare.
let twoPostRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "cont-1", { X = 11; Y = 10 }
              "cont-2", { X = 9; Y = 10 }
              "ctrl-1", { X = 40; Y = 10 }
          ]
          (openSeats { X = 10; Y = 10 } @ [ for x in 11..39 -> { X = x; Y = 10 }, Plain ]) with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "cont-1", Structure BuiltKind.Container
                    "cont-2", Structure BuiltKind.Container
                    "ctrl-1", Controller
                ]
    }

let twoPostCrowd (placed: (CreepInfo * Pos) list) =
    { bareRespawn with
        Spawns = []
        Refillables = []
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt 2)
        Creeps = placed |> List.map fst
        Spatial =
            twoPostRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                })
    }

/// The Dual Seat room with a lane out of it. `dualSeatRoom`'s source sits at
/// (10,10) with two Seats, (11,10) inside the controller's Upgrade Work Area
/// and so a bare [[dual seat]] — the colony's one Post with no container under
/// it. The lane is laid along y = 9 from x = 12 to x = 31 and deliberately not
/// along y = 10: the controller stands at (13,10), and a row through it would
/// either wall the lane or, laid one tile lower, add a second Seat inside the
/// controller's range and give the rock a second Post.
let dualSeatLaneColony ticks (placed: (CreepInfo * Pos) list) =
    { dualSeatColony with
        Spawns = []
        Refillables = []
        Sources = [ drained "src-a" ticks ]
        Creeps = placed |> List.map fst
        Spatial =
            dualSeatRoom
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 12..31 -> { X = x; Y = 9 } ])
                        ||> List.fold (fun acc tile -> Map.add tile Plain acc)
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                })
    }

/// The W12S28 colony with its own two source containers taken away: the
/// same two rocks, the same eight Seats apiece, and no Post on either. The
/// only Post left in a projection is whatever an outpost carries — which
/// is the one arrangement where a neutral rate is the *richest* rate the
/// Anchor row hires for, and so the only one where the row's ceiling can
/// be read off a cast body at all.
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

/// The W12S28 colony at a 1,300 bank with the posted outpost source of
/// `postedOutpostColony` standing beside it: the same rock in the same
/// three-Seat field, its container built, and a fleet of one worker so
/// every Post in the projection is an unfilled Anchor gap. The bank alone
/// would buy twelve Work, so the body the row casts is decided by its
/// ceiling and by nothing else, and 700 of the 1,300 goes on that body —
/// leaving too little for a second, so the tick casts exactly one Anchor
/// whatever the gap.
///
/// Two dials and no others: whether the colony's own room keeps its Posts,
/// and who holds W1N2. Everything the target is built from moves with
/// them, but the *body* reads only the ceiling.
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

/// The colony the anchor row's **charge** is legible in, which the cast's
/// own fixture is not: the same W12S28 without its two Posts, at a 1,400
/// bank, with three neutral rocks a room away, each with its container
/// standing — three Posts, three Anchors hired, and every one of them
/// under the neutral ceiling. Its fleet is whole but for the workers, so
/// the one thing a spawn Intent can be here is the income base's own
/// answer.
///
/// Why those two numbers and not the 1,300 of the cast's fixture. The
/// amortization is deducted from income before the surplus is divided into
/// worker places, and the division rounds up over a whole body's Work
/// drain across a lifetime (ADR 0037) — 10,500 energy at this bank — so a
/// charge that moves by 350 an Anchor is invisible unless the surplus
/// straddles a boundary. Three Posts move it by 1,050, and 15 energy a
/// tick over the lifetime leaves 21,450 charged at the cast body against
/// 20,400 charged at the held one: three worker places and two. One
/// Post at 1,300 moves it by 350 against a 9,000-energy place and could
/// not move the target at all.
/// `homePosts` keeps the colony's own two Posts in the projection, which
/// is the arrangement ADR 0053 is about and the one the aggregate charge
/// could not tell from any other: five Posts over two rates, charged
/// 2 × 700 + 3 × 400 Post by Post where a quota times one ceiling charges
/// 5 × 700.
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
/// in wall at (10,10), its built container on the Seat (11,10) — the only
/// tile a Work-heavy body may dig that source from (ADR 0020) — and the
/// spawn structure standing at (21,10), ten plain steps up the lane. Its
/// one free neighbour is (20,10), so that is where a replacement is born
/// and the walk it is led by is nine steps, not ten. Far enough that a
/// replacement's own body, not just its cast time, prices the lead.
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

/// The same lane at an RCL3 bank, where the Anchor row's body is five
/// Work beside its Carry and Move (ADR 0021) — and where both creeps below
/// are that body, so the lead prices exactly the body it leads, as a real
/// succession does. Ten cost units a plain step, 21 ticks in the spawner.
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

/// The upgrader row's own body at that bank: `11W/1C/11M`, ADR 0046's.
let internal upgraderBody = castFull upgraderPattern

/// The generalist at the same bank: `9W/9C/9M` — one Carry per Work where
/// the gate's line is one per four, so it is the row the gate must leave
/// alone, its whole design being that it walks its energy somewhere.
let internal workerBody = castFull workerPattern

/// The Anchor row's live body: six Work, one Carry, one Move (ADR 0021's
/// held ceiling). A standing body by the same arithmetic as the upgrader's
/// — `1 * 4 < 6` — which is ADR 0046 saying the rule is about bodies and
/// not about rows.
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

/// A live Anchor's shape, `6W/1C/1M`: Work-heavy by ADR 0016's ratio
/// (`6 > 1`) and a standing body by ADR 0046's (`1 × 4 < 6`), so before
/// #205 every one of Build, Repair, Refill and Withdraw was shut to it and
/// Harvest at its Post was the whole of its working life.
let internal postBody name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Work; Work; Work; Work; Work; Carry; Move ]

/// #205's colony: the outpost rock at (10,46) with its container gone and
/// the plan's site back on the Seat at (10,45) — the live shape after an
/// invader demolished three of them (W12S27 15,44 and W13S28 15,8 / 18,3)
/// — and one body of the caller's shape standing where the caller puts it.
///
/// No controller and no refillable, as the fixtures above have it, so the
/// pool is the two rocks and the site and a Matched factor names one
/// comparison rather than reporting on some third candidate. The home
/// creep the base fixture stands at (10,2) is taken out with it: the
/// caller's bodies are the whole colony, and every Verdict is about one of
/// them. Two rosters because the cap cases need both ends of the Seam: the
/// bodies standing in the outpost, and the ones standing at home.
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
/// #205's rule reads no room — the tick an RCL2 colony's own source
/// container is planned, the body that will garrison it raises it — and
/// this is that case with the Seam taken out of the picture.
let internal homeRaisingColony kind (body: CreepInfo) (at: Pos) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-a" ]
        ConstructionSites = [ { Id = "can-a" } ]
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
