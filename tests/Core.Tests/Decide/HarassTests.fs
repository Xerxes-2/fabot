/// The harassment room as `decide` sees it: the ranger's Guard there, the
/// enemy's unarmed creeps as targets beside its armed ones, the ranger's ground,
/// the row's floor, and the Dismantle of the enemy's containers. Which colony
/// casts the room and what it may carry of it are `ViewTests`'.
module Fabot.Core.Tests.Decide.HarassTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

/// The room next door: one crossing out, so nothing here is about the walk.
let private harassRoom = "W1N2"

let private enemy = "Trepidimous"

/// The enemy's source tile, the ranger's ground while nothing stands there.
let private standTile = { X = 20; Y = 10 }

let private declaration =
    {
        RoomName = harassRoom
        Enemy = enemy
        Stand = RoomPos.at harassRoom standTile
    }

/// The enemy's container, beside its source.
let private container = "can-enemy", { X = 21; Y = 11 }

let private floor =
    [
        for x in 1..48 do
            for y in 1..48 -> { X = x; Y = y }, Plain
    ]

/// The home with nothing of its own to offer, so the pool a case reads is the
/// harassment room's.
let private bareHome =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = []
        Spatial = openRoom 6
        RoomControl = homeControl
    }

/// The colony casting that room, laid the way `ColonyView.ofWorld` lays one:
/// the ground, and the container's id and tile with no kind, hit or store.
let private harassing (colony: ColonyView) =
    { casting declaration colony with
        Dismantles = [ fst container ]
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add harassRoom plainRing
            }
            |> withNeighbour
                harassRoom
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList floor
                    TargetPositions = Map.ofList [ container ]
                }
    }

/// Our own bodies standing in the harassment room.
let private standingThere (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial harassRoom

    { colony with
        Creeps = colony.Creeps @ (ours |> List.map fst)
        Spatial =
            colony.Spatial
            |> withNeighbour
                harassRoom
                { layer with
                    CreepPositions =
                        ours |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// A creep standing in the harassment room, owned by whoever is named.
let private theirs id owner pos body =
    { hostileIn harassRoom pos body with
        Id = id
        Owner = owner
    }

/// The enemy's remote miner: Work and Move, no weapon.
let private miner id pos =
    theirs id enemy pos [ Work; Work; Work; Move ]

let private ranger name =
    creepWith name 0 0 Bodies.rangerPattern.Block

[<Tests>]
let harassGuardTests =
    testList
        "a harassment room's Guard: the ranger's, and the enemy's creeps its targets"
        [
            test "a quiet harassment room still pools its Guard, and only the ranger holds it" {
                let colony =
                    { (bareHome |> harassing) with
                        Bank = bank 2100 2100
                    }
                    |> standingThere
                        [
                            ranger "ranger-r", standTile
                            creepWith "guard-m" 0 0 Bodies.guardPattern.Block,
                            { standTile with X = standTile.X + 2 }
                        ]

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Guard harassRoom)
                    "the room is worked target or none, as an errand room is"

                let assignments = (decideOn colony).Assignments

                Expect.equal
                    (Map.tryFind "ranger-r" assignments, Map.tryFind "guard-m" assignments)
                    (Some(taskId (Guard harassRoom)), None)
                    "the ranged body holds it; a melee one is an outpost's"
            }

            test
                "the ranger shoots the enemy's unarmed miner, and never a third player's or a keeper's" {
                let here = { X = 20; Y = 20 }
                let shooter = ranger "ranger-r"

                let target hostiles =
                    let colony =
                        { (bareHome |> harassing) with
                            Hostiles = hostiles
                        }
                        |> standingThere [ shooter, here ]

                    emitOn colony [ shooter.Name, Guard harassRoom ]
                    |> List.choose (function
                        | RangedAttackCreep(name, id) when name = shooter.Name -> Some id
                        | _ -> None)

                // Nearer than the miner, both of them, and both unarmed.
                let bystanders =
                    [
                        theirs "passer" "Somebody" { X = 21; Y = 20 } [ Work; Carry; Move ]
                        theirs "keeper" "Source Keeper" { X = 19; Y = 20 } [ Move ]
                    ]

                Expect.equal
                    (target (bystanders @ [ miner "miner" { X = 22; Y = 20 } ]))
                    [ "miner" ]
                    "the enemy's miner, two tiles off"

                Expect.isEmpty (target bystanders) "nobody else's unarmed creep is a target"
            }

            test "the enemy's armed creep is shot before its unarmed one" {
                let here = { X = 20; Y = 20 }
                let shooter = ranger "ranger-r"

                let escort =
                    theirs
                        "escort"
                        enemy
                        { X = 23; Y = 20 }
                        [ Move; Move; Move; RangedAttack; Heal ]

                let colony =
                    { (bareHome |> harassing) with
                        Hostiles = [ miner "miner" { X = 21; Y = 20 }; escort ]
                    }
                    |> standingThere [ shooter, here ]

                Expect.contains
                    (emitOn colony [ shooter.Name, Guard harassRoom ])
                    (RangedAttackCreep(shooter.Name, "escort"))
                    "what shoots back first, the miner after"
            }

            test
                "the ranger's ground is beside the enemy's creeps, and beside the Stand while none stands" {
                let quiet = bareHome |> harassing
                let atlas = Atlas.ofView quiet

                Expect.equal
                    (Threats.harassRingIn (threatsOf quiet atlas) harassRoom)
                    (Some(
                        Atlas.adjacentWalkableIn atlas harassRoom standTile
                        |> Set.ofList
                        |> RoomPos.setAt harassRoom
                    ))
                    "an empty room: the enemy's source, where its miner comes back to"

                let minerTile = { X = 30; Y = 30 }

                let lit =
                    { quiet with
                        Hostiles = [ miner "miner" minerTile ]
                    }

                let ring = Threats.harassRingIn (threatsOf lit (Atlas.ofView lit)) harassRoom

                Expect.equal
                    ring
                    (Some(
                        Atlas.adjacentWalkableIn atlas harassRoom minerTile
                        |> Set.ofList
                        |> RoomPos.setAt harassRoom
                    ))
                    "an unarmed miner has no Reach and no ring of its own; the harassment ring is its"
            }
        ]

[<Tests>]
let harassRowTests =
    testList
        "the ranger row for a harassment room"
        [
            test "one ranger, and never under three blocks" {
                // A hauler already stands, so the supply floor casts nothing and
                // the seat open is the ranger's. The bank buys eight blocks.
                let colony =
                    { (bareHome |> harassing) with
                        Bank = bank 5600 5600
                        Creeps = [ creepWith "h" 0 100 [ Carry; Carry; Move ] ]
                        Tuning =
                            { Tuning.defaults with
                                MinWorkforce = 0
                            }
                    }

                let decision = decideOn colony

                Expect.equal
                    (decision.Quotas.Rows
                     |> List.tryFind (fun row -> row.Row = "ranger")
                     |> Option.map (fun row -> row.Quota))
                    (Some 1)
                    "one ranger works the room"

                let rangedParts =
                    spawnIntents decision.Intents
                    |> List.map (fun (_, body, _) ->
                        body |> List.filter ((=) RangedAttack) |> List.length)
                    |> List.filter (fun parts -> parts > 0)

                Expect.equal
                    rangedParts
                    [ 2 * Tuning.defaults.HarassBlocks ]
                    "cast at the harassment floor, three blocks, not the errand garrison's seven"
            }
        ]

[<Tests>]
let harassContainerTests =
    testList
        "the enemy's containers"
        [
            test "the enemy's container is pooled as a Dismantle, and as nothing else" {
                let colony = bareHome |> harassing
                let id = fst container

                let naming =
                    planTasksOn colony noThreats
                    |> List.filter (fun task -> (taskId task).EndsWith $":{id}")

                Expect.equal naming [ Dismantle id ] "no Withdraw, Refill or Repair of it"
            }
        ]
