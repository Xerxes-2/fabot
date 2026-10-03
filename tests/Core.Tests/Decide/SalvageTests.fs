/// The salvage room as `decide` sees it: the `Dismantle` Task each of its
/// targets offers, who may hold it, the act it fires, and the row that casts
/// the body. The projection half — which structures are targets, and what a
/// colony may carry of the room — is `ViewTests`'.
module Fabot.Core.Tests.Decide.SalvageTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The room next door: one crossing out, so nothing here is about the walk.
let private salvageRoom = "W1N2"

/// What is left standing in it: a spawn and an extension, which block the
/// tile they stand on, and a rampart, which does not.
let private targets =
    [
        "spawn-old", { X = 20; Y = 20 }
        "ext-old", { X = 22; Y = 20 }
        "ramp-old", { X = 24; Y = 20 }
    ]

let private targetIds = targets |> List.map fst

/// A tile within range 1 of the spawn and the extension, and one within range
/// 1 of nothing.
let private besideTheSpawn = { X = 21; Y = 21 }
let private twoOff = { X = 22; Y = 23 }

let private floor =
    [
        for x in 1..48 do
            for y in 1..48 -> { X = x; Y = y }, Plain
    ]

/// The home with nothing of its own to offer, so the pool a case reads is the
/// salvage room's.
let private bareHome =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = []
        Spatial = openRoom 6
        RoomControl = homeControl
    }

/// The colony with that room declared and seen, laid the way
/// `ColonyView.ofWorld` lays one: ids and tiles, and no kind, hit or store.
let private salvaging (colony: ColonyView) =
    { colony with
        Dismantles = targetIds
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add salvageRoom plainRing
            }
            |> withNeighbour
                salvageRoom
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList floor
                    TargetPositions = Map.ofList targets
                    Obstacles = Set.ofList [ { X = 20; Y = 20 }; { X = 22; Y = 20 } ]
                }
    }

/// Our own bodies standing in the salvage room.
let private standingThere (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial salvageRoom

    { colony with
        Creeps = colony.Creeps @ (ours |> List.map fst)
        Spatial =
            colony.Spatial
            |> withNeighbour
                salvageRoom
                { layer with
                    CreepPositions =
                        ours |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// The row's body, two blocks of it.
let private dismantler name =
    creepWith name 0 0 [ Work; Work; Move; Move ]

let private dismantlerRow (colony: ColonyView) =
    (decideOn colony).Quotas.Rows
    |> List.tryFind (fun row -> row.Row = "dismantler")

let private heldBy name (colony: ColonyView) =
    let { Assignments = assignments } = decideOn colony
    Map.tryFind name assignments

[<Tests>]
let salvagePoolTests =
    testList
        "a salvage room's pool"
        [
            test "a salvage room pools one Dismantle per target, and nothing else of it" {
                let colony = bareHome |> salvaging

                Expect.equal
                    (planTasksOn colony noThreats
                     |> List.filter (fun task ->
                         targetIds |> List.exists (fun id -> (taskId task).EndsWith $":{id}")))
                    (targetIds |> List.map Dismantle)
                    "a Dismantle of each, and no Repair, Refill or Withdraw of any"
            }

            test "only the dismantler's body is applicable to a Dismantle" {
                // Every body stands beside the spawn, so no walk separates them:
                // a generalist carrying energy, a hauler, the Work-heavy miner
                // with no Carry, and the dismantler.
                let colony =
                    bareHome
                    |> salvaging
                    |> standingThere
                        [
                            worker "w" 50 0, besideTheSpawn
                            creepWith "h" 0 100 [ Carry; Carry; Move ], { X = 20; Y = 21 }
                            miner "m", { X = 19; Y = 21 }
                            dismantler "d", { X = 21; Y = 19 }
                            // An assault's sapper: the dismantler's parts, its
                            // own name (#490).
                            creepWith "sapper-1-Spawn1" 0 0 sapperPattern.Block, { X = 22; Y = 21 }
                        ]

                let dismantling name =
                    heldBy name colony |> Option.exists (fun held -> held.StartsWith "dismantle:")

                Expect.isTrue (dismantling "d") "the dismantler takes one"

                for name in [ "w"; "h"; "m"; "sapper-1-Spawn1" ] do
                    Expect.isFalse
                        (dismantling name)
                        $"{name} is never sent to take a structure down"
            }
        ]

[<Tests>]
let salvageActTests =
    testList
        "the dismantle act"
        [
            test "a dismantler within range 1 of its target dismantles it" {
                let colony =
                    bareHome |> salvaging |> standingThere [ dismantler "d", besideTheSpawn ]

                let { Intents = intents } = decideOn colony

                let acts =
                    intents
                    |> List.choose (function
                        | DismantleStructure(name, id) -> Some(name, id)
                        | _ -> None)

                match acts with
                | [ "d", id ] ->
                    let tile = targets |> List.find (fun (target, _) -> target = id) |> snd

                    Expect.isLessThanOrEqual
                        (range tile besideTheSpawn)
                        1
                        "the act names a target the body stands beside"
                | other -> failtest $"expected one DismantleStructure by d, got %A{other}"
            }

            test "a dismantler two tiles off walks and does not act" {
                let colony = bareHome |> salvaging |> standingThere [ dismantler "d", twoOff ]

                let { Intents = intents } = decideOn colony

                Expect.isFalse
                    (intents
                     |> List.exists (function
                         | DismantleStructure _ -> true
                         | _ -> false))
                    "range 1 is the act's reach, as the engine's `dismantle` checks it"

                Expect.isNonEmpty (moveIntentsFor "d" intents) "it walks instead"
            }
        ]

[<Tests>]
let salvageRowTests =
    testList
        "the dismantler row"
        [
            test
                "the row wants one body while anything stands to take down, and none once it has fallen" {
                let standing = bareHome |> salvaging

                let fallen = { standing with Dismantles = [] }

                Expect.equal
                    (dismantlerRow standing |> Option.map (fun row -> row.Quota))
                    (Some 1)
                    "one body takes the room down"

                Expect.equal
                    (dismantlerRow fallen |> Option.map (fun row -> row.Quota))
                    (Some 0)
                    "and the row closes when the room goes dark"
            }

            test "the row casts a Work/Move body, not a generalist" {
                // A hauler already stands, so the supply floor casts nothing and the
                // one seat open is the dismantler's.
                let colony =
                    { (bareHome |> salvaging) with
                        Bank = bank 800 800
                        Creeps = [ creepWith "h" 0 100 [ Carry; Carry; Move ] ]
                        Tuning =
                            { Tuning.defaults with
                                MinWorkforce = 0
                            }
                    }

                let casts = spawnIntents (decideOn colony).Intents

                Expect.contains
                    (casts |> List.map (fun (_, body, _) -> body))
                    (bodyFor dismantlerPattern 800)
                    "the row's own body is cast"
            }
        ]
