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

/// The room's controller tile, where the enemy's reserver stands.
let private controllerTile = { X = 30; Y = 40 }

let private declarationOf room =
    {
        RoomName = room
        Enemy = enemy
        Stand = RoomPos.at room standTile
        Controller = RoomPos.at room controllerTile
        Via = []
    }

let private declaration = declarationOf harassRoom

/// A second harassment room, west of home where the first is north.
let private westRoom = "W2N1"

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

/// The harassment room's tiles within `radius` of `centre`, less the tiles
/// hostiles stand on: its whole floor is plain.
let private within radius (centre: Pos) (occupied: Set<Pos>) =
    [
        for x in centre.X - radius .. centre.X + radius do
            for y in centre.Y - radius .. centre.Y + radius do
                let tile = { X = x; Y = y }

                if not (Set.contains tile occupied) then
                    RoomPos.at harassRoom tile
    ]
    |> Set.ofList

/// A colony casting these harassment rooms off a home whose whole floor is
/// plain, so a body at home walks to any of them: the north room is three
/// tiles from the north edge, the west one twenty-five from the west.
let private castingFrom (declarations: Harass list) (ours: (CreepInfo * RoomPos) list) =
    let colony = bareHome
    let home = SpatialInfo.homeName colony.Spatial

    let spatial =
        declarations
        |> List.fold
            (fun spatial (h: Harass) ->
                { spatial with
                    Borders = spatial.Borders |> Map.add h.RoomName plainRing
                }
                |> withNeighbour
                    h.RoomName
                    { RoomLayer.empty with
                        Terrain = TerrainGrid.ofList floor
                    })
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add home plainRing
            }
        |> withHome (fun layer ->
            { layer with
                Terrain = TerrainGrid.ofList floor
            })

    let placed =
        (spatial, ours |> List.groupBy (fun (_, at) -> at.Room))
        ||> List.fold (fun spatial (room, standing) ->
            let layer = SpatialInfo.layerOf spatial room

            spatial
            |> withNeighbour
                room
                { layer with
                    CreepPositions =
                        standing
                        |> List.map (fun (creep, at) -> creep.Name, RoomPos.pos at)
                        |> Map.ofList
                })

    { colony with
        Harass = declarations
        HarassCast = declarations |> List.map (fun h -> h.RoomName) |> Set.ofList
        Creeps = ours |> List.map fst
        Spatial = placed
    }

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

            test "with no enemy on a work spot, the ranger's ground is the Stand's seats" {
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                Expect.equal
                    (ground quiet)
                    (Some(within 1 standTile Set.empty))
                    "an empty room: beside the enemy's source, within reach of whatever works it"

                // A claimer walking to the controller, five tiles short of it,
                // as fast as the ranger: chased, it is never caught.
                let passing =
                    { quiet with
                        Hostiles =
                            [ theirs "claimer" enemy { X = 30; Y = 35 } [ BodyPart.Claim; Move ] ]
                    }

                Expect.equal
                    (ground passing)
                    (ground quiet)
                    "a moving enemy creep does not pull the ground; it is shot if it comes into reach"
            }

            test
                "an enemy creep on a work spot pulls the ranger's ground to within ranged reach of it" {
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                // Beside the source, and a walker the ground ignores.
                let minerTile = { X = 19; Y = 11 }
                let walker = theirs "walker" enemy { X = 40; Y = 20 } [ Carry; Move ]

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles = [ miner "miner" minerTile; walker ]
                        })
                    (Some(within Engine.rangedRange minerTile (Set.ofList [ minerTile ])))
                    "the miner beside the source: the ranger stands within three of it"

                // Beside the controller: the reserver stops there for its whole
                // reservation.
                let reserverTile = { X = 31; Y = 39 }

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                                    walker
                                ]
                        })
                    (Some(within Engine.rangedRange reserverTile (Set.ofList [ reserverTile ])))
                    "the reserver at the controller: the ranger stands within three of it"

                // Two from the source: where a hauler stands to draw from the
                // miner's pile or container (W18S27's Oni986 at (25,7)).
                let haulerTile = { X = 18; Y = 10 }

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles = [ theirs "hauler" enemy haulerTile [ Carry; Move ]; walker ]
                        })
                    (Some(within Engine.rangedRange haulerTile (Set.ofList [ haulerTile ])))
                    "the hauler two from the source: the ranger stands within three of it"

                // A reserver shuffles around the controller rather than
                // standing beside it (W18S27's Infinity271: 26,31 → 25,30 →
                // 24,29), and ground that dropped it at two flipped every step.
                let claimerAt tile =
                    ground
                        { quiet with
                            Hostiles = [ theirs "claimer" enemy tile [ BodyPart.Claim; Move ] ]
                        }

                let threeOff = { X = 33; Y = 43 }

                Expect.equal
                    (claimerAt threeOff)
                    (Some(within Engine.rangedRange threeOff (Set.ofList [ threeOff ])))
                    "three from the controller is still on it"

                Expect.equal (claimerAt { X = 34; Y = 40 }) (ground quiet) "four from it is not"

                // Both at once: the reserver kept its distance from the ranger
                // while the miner harvested on, and ground round both turned the
                // ranger between them (W18S27, t840,7xx).
                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    miner "miner" minerTile
                                    theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                                ]
                        })
                    (Some(within Engine.rangedRange minerTile (Set.ofList [ minerTile ])))
                    "a miner at the source and a reserver at the controller: the ground is the miner's alone"

                // The miner stepped just out of reach, off the source, and
                // back: ground that followed it off the source turned the
                // ranger to the reserver and back (W18S27, t840,8xx).
                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    miner "miner" { X = 15; Y = 7 }
                                    theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                                ]
                        })
                    (ground quiet)
                    "a miner off the source and a reserver at the controller: the ranger holds the source's seats"
            }
        ]

[<Tests>]
let harassSeatTests =
    testList
        "one ranger per harassment room, and its relief beside it only inside its lead"
        [
            test "two fresh rangers and two harassment rooms: one each" {
                // Both stand nearer the north room, so the nearer seat alone
                // would take both.
                let colony =
                    castingFrom
                        [ declaration; declarationOf westRoom ]
                        [
                            ranger "ranger-a", RoomPos.at "W1N1" { X = 25; Y = 3 }
                            ranger "ranger-b", RoomPos.at "W1N1" { X = 26; Y = 3 }
                        ]

                let assignments = (decideOn colony).Assignments

                Expect.equal
                    ([ "ranger-a"; "ranger-b" ]
                     |> List.choose (fun name -> Map.tryFind name assignments)
                     |> Set.ofList)
                    (Set.ofList [ taskId (Guard harassRoom); taskId (Guard westRoom) ])
                    "each room holds one ranger"
            }

            test
                "a fresh ranger is admitted beside an incumbent inside its lead, and not beside one outside it" {
                let relief = ranger "ranger-new", RoomPos.at "W1N1" { X = 25; Y = 3 }

                let decided ticksToLive =
                    let incumbent =
                        { ranger "ranger-old" with
                            TicksToLive = ticksToLive
                        }

                    castingFrom
                        [ declaration ]
                        [ incumbent, RoomPos.at harassRoom standTile; relief ]
                    |> decideFrom (Map.ofList [ "ranger-old", taskId (Guard harassRoom) ])
                    |> fun decision -> decision.Assignments

                let dying = decided 5

                Expect.equal
                    (Map.tryFind "ranger-old" dying, Map.tryFind "ranger-new" dying)
                    (Some(taskId (Guard harassRoom)), Some(taskId (Guard harassRoom)))
                    "inside the lead: the relief takes the seat beside the incumbent"

                let fresh = decided 1500

                Expect.equal
                    (Map.tryFind "ranger-old" fresh, Map.tryFind "ranger-new" fresh)
                    (Some(taskId (Guard harassRoom)), None)
                    "outside it: a second body buys nothing"
            }

            test
                "a held relief is kept beside its incumbent once the lead no longer covers the incumbent, and a third body is still refused" {
                let relief = ranger "ranger-new", RoomPos.at "W1N1" { X = 25; Y = 3 }
                let third = ranger "ranger-third", RoomPos.at "W1N1" { X = 26; Y = 3 }

                // Both held, as after the tick the relief was admitted; the
                // lead the incumbent was inside has since shrunk below it.
                let decided ticksToLive =
                    let incumbent =
                        { ranger "ranger-old" with
                            TicksToLive = ticksToLive
                        }

                    castingFrom
                        [ declaration ]
                        [ incumbent, RoomPos.at harassRoom standTile; relief; third ]
                    |> decideFrom (
                        Map.ofList
                            [
                                "ranger-old", taskId (Guard harassRoom)
                                "ranger-new", taskId (Guard harassRoom)
                            ]
                    )
                    |> fun decision ->
                        [ "ranger-old"; "ranger-new"; "ranger-third" ]
                        |> List.map (fun name -> Map.tryFind name decision.Assignments)

                let guard = Some(taskId (Guard harassRoom))

                Expect.equal
                    (decided 600)
                    [ guard; guard; None ]
                    "a generation apart: the relief stays, and the third body buys nothing"

                // Whichever of the two the fold keeps: a Guard's holders tie on
                // the walk, so its order is their names'.
                Expect.equal
                    (decided 1400 |> List.choose id)
                    [ taskId (Guard harassRoom) ]
                    "two bodies of one generation are no relief: one is released, and the third buys nothing"
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
