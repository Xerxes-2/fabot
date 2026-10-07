/// The threat suites' shared surface: the hostile, the towered colony, the
/// resident ranger, the defended perimeter home and the tower readers more
/// than one threat file reads. A helper one file alone reads stays private there.
module Fabot.Core.Tests.Decide.ThreatFixtures

open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// A hostile creep of the given body, position immaterial.
let internal hostile body = hostileAt "h-1" { X = 25; Y = 25 } body

/// The same colony with its tower placed, so the undefended arm (#217) is
/// out of the question and only the Keep arm can fire.
let internal towered (snapshot: ColonyView) =
    { snapshot with
        Spatial =
            snapshot.Spatial
            |> withTargets [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
    }

/// One resident ranger of seven whole blocks (#447's garrison body).
let internal resident name =
    creepWith name 0 0 (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

/// The declared perimeter at (20, 33..35): it seals the corridor from the
/// west exits (x 1..19, y 33..35) off the home's inside (x 21..40, y
/// 20..38), where the spawn stands at (25,25).
let internal lineTiles = [ for y in 33..35 -> { X = 20; Y = y } ]

/// Ramparts of ours on each perimeter tile.
let internal defendedLine =
    [
        "ram-a", { X = 20; Y = 33 }
        "ram-b", { X = 20; Y = 34 }
        "ram-c", { X = 20; Y = 35 }
    ]

/// Trepidimous' 18M17A melee, a player's, at a tile of the home.
let internal raidMeleeAt id (pos: Pos) =
    { hostileIn "W1N1" pos (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ]) with
        Id = id
        Owner = "Trepidimous"
    }

/// An Independent home at RCL4 on an 800 bank, `lineTiles` its perimeter
/// with `ramparts` standing, a worker at home so the supply floor keeps off
/// the spawn, these hostiles in the room and these creeps of ours on these
/// tiles.
let internal defenderHome ramparts hostiles (creeps: (CreepInfo * Pos) list) =
    let spawnPos = { X = 25; Y = 25 }

    let room =
        { SpatialInfo.empty with
            RoomName = Some "W1N1"
            TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            Borders =
                Map.ofList [ "W1N1", Map.ofList [ for y in 33..35 -> { X = 0; Y = y }, Plain ] ]
        }
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    TerrainGrid.ofList
                        [
                            for x in 21..40 do
                                for y in 20..38 -> { X = x; Y = y }, Plain
                            for x in 1..20 do
                                for y in 33..35 -> { X = x; Y = y }, Plain
                        ]
                TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
                Obstacles = Set.singleton spawnPos
            })
        |> withTargets [ for id, pos in ramparts -> id, pos, Structure BuiltKind.Rampart ]

    let colony =
        { atLevel 4 room with
            Bank = bank 800 800
            Hostiles = hostiles
            Perimeter = lineTiles
            Creeps = worker "w" 0 50 :: List.map fst creeps
            Spatial =
                room
                |> withCreepsAt (
                    ("w", { X = 25; Y = 27 }) :: [ for creep, pos in creeps -> creep.Name, pos ]
                )
        }

    (colony, ramparts |> List.mapi (fun i (id, _) -> id, 90_000 + 5_000 * i))
    ||> List.fold (fun colony (id, hits) -> colony |> withHits id BuiltKind.Rampart hits 1_000_000)

let internal defender name =
    creepWith name 0 0 (List.replicate 6 Move @ List.replicate 6 Attack)

let internal shots intents =
    intents
    |> List.choose (function
        | FireTower(tower, target) -> Some(tower, target)
        | _ -> None)

/// The colony with this tower of its holding `energy`.
let internal holdingTower (id: string) (energy: int) (colony: ColonyView) =
    { colony with
        Refillables =
            {
                Id = id
                FreeCapacity = Engine.towerCapacity - energy
                Kind = BuiltKind.Tower
            }
            :: colony.Refillables
    }
