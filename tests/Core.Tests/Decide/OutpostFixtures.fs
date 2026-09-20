/// The outpost suite's fixtures: the declared neighbours, their sources and
/// containers, and the raids and stand-downs staged against them.
module Fabot.Core.Tests.Decide.OutpostFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The colony with **two** outposts and one Anchor standing beside the
/// wrong one (#159).
///
/// Home is the north corridor with a west arm along row 26, and the Anchor
/// stands two tiles from the west border and thirty-three from the north
/// one. W1N2 across the north border carries a rock with a container on
/// one of its Seats; W2N1 across the west carries a rock with nothing on
/// it, the shape the live colony had.
///
/// `northBorderColony`'s own rock is not a third one: `Sources` is
/// replaced, `src-home` is dropped from the kind census and the home
/// layer's `TargetPositions` is emptied.
///
/// With the west rock bare the Post count is one, so the north container
/// hires exactly this one body, and nothing in the Matcher knows which
/// Post it was hired for. Nothing here casts a second, the fixture having
/// no spawn. The pool is the two Harvests and nothing else, so a Matched
/// Verdict names this pair.
let internal twoRockColony (westContainer: (string * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 }

    { colony with
        Sources = [ source "src-north"; source "src-west" ]
        Creeps = [ creepWith "anchor" 0 50 [ Work; Work; Carry; Move ] ]
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders |> Map.add "W1N2" plainRing |> Map.add "W2N1" plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, westContainer)
                    ||> List.fold (fun kinds (id, _) ->
                        Map.add id (Structure BuiltKind.Container) kinds)
                    |> Map.remove "src-home"
                    |> Map.add "src-north" Source
                    |> Map.add "src-west" Source
                    |> Map.add "cont-north" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                        ||> List.fold (fun terrain pos -> TerrainGrid.add pos Plain terrain)
                    TargetPositions = Map.empty
                    CreepPositions = Map.ofList [ "anchor", { X = 2; Y = 26 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                    TargetPositions =
                        Map.ofList
                            [ "src-north", { X = 10; Y = 46 }; "cont-north", { X = 10; Y = 45 } ]
                }
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
                    TargetPositions = Map.ofList (("src-west", { X = 45; Y = 26 }) :: westContainer)
                }
    }

/// Which Task won the one Anchor, and what separated it from its closest
/// rival.
let internal anchorMatch (colony: ColonyView) =
    let { Verdicts = verdicts } = decideOn colony

    verdicts
    |> List.tryPick (function
        | Verdict.Matched("anchor", task, factor) -> Some(task, factor)
        | _ -> None)

/// The two-room fixture #193 was filed on: a Post in each room, both rocks
/// inside their empty window, and one body beside the outpost's Post, the
/// tile a hauler drawing that container swaps an Anchor onto.
///
/// The crossing is a corridor, a Seam and thirty-six tiles, which an Anchor
/// walks at four ticks a step. The pool is the two Harvests and nothing
/// else: `northBorderColony` carries no controller, no spawn and no
/// refillable.
let internal twoPostWindowColony ticksHome ticksOut (creep: CreepInfo) =
    let colony =
        northBorderColony { X = 10; Y = 38 }
        |> withNorthOutpost (Some { X = 10; Y = 46 })

    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Sources = [ drained "src-home" ticksHome; drained "src-out" ticksOut ]
        Creeps = [ creep ]
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "can-home" (Structure BuiltKind.Container)
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "can-home" { X = 10; Y = 37 } layer.TargetPositions
                    CreepPositions = Map.empty
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "can-out" { X = 10; Y = 45 } outpost.TargetPositions
                    CreepPositions = Map.ofList [ creep.Name, { X = 10; Y = 47 } ]
                }
    }

/// A colony with an outpost beside it whose ground and furniture are the
/// case's own. The container pick is a choice *between* Seats, so a
/// corridor with one Seat at each end proves nothing; these cases lay a
/// floor that makes the Seats differ.
///
/// Both rooms get a plain border ring, because the pick is measured to
/// the Seam and a projection carrying no ring answers an empty band.
///
/// The outpost room gets a `RoomControl` entry, held by nobody: that map
/// is one entry per *seen* room, so an entry is how a fixture says the
/// colony is looking into the room this tick. Neutral because nothing
/// here reads the rate.
let internal withOutpostGround room terrain placed (colony: ColonyView) =
    { colony with
        Sources = colony.Sources @ [ source "src-out" ]
        RoomControl = Map.add room neutralRoom colony.RoomControl
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add room plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, placed)
                    ||> List.fold (fun kinds (id, _, kind) -> Map.add id kind kinds)
            }
            |> withNeighbour
                room
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList terrain
                    TargetPositions = placed |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                }
    }

/// Every container site the tick asks for, room beside tile, in the order
/// the colony emits them.
let internal containerSites (colony: ColonyView) =
    let { Intents = intents } = decideOn colony

    intents
    |> List.choose (function
        | PlaceConstructionSite(tile, Container) -> Some(tile.Room, RoomPos.pos tile)
        | _ -> None)

/// `src-out` sits at (10,44), which no case lays ground on, so its Seats
/// are whichever of its eight neighbours the case does.
let internal outpostSource = { X = 10; Y = 44 }

/// One named room's layer, changed in place, starting from an empty layer
/// for a room nothing has.
let internal withOutpostLayer room (change: RoomLayer -> RoomLayer) (colony: ColonyView) =
    let layer =
        Map.tryFind room colony.Spatial.Rooms |> Option.defaultValue RoomLayer.empty

    { colony with
        Spatial = colony.Spatial |> withNeighbour room (change layer)
    }

/// A road that **stands** on one of the outpost's tiles, in the two pieces
/// `World.factsOf` hands one in: the id-keyed kind census and the layer's
/// own `Roads`, the half the walk prices. One piece alone would be a road
/// the projection half believes in.
let internal paved room tiles (colony: ColonyView) =
    colony
    |> withOutpostLayer room (fun layer ->
        { layer with
            Roads = Set.union layer.Roads (Set.ofList tiles)
        })

/// A construction site **somebody else** put on one of the outpost's tiles
/// (#248): the layer's `RivalSites` and nothing beside it, no id, no kind.
let internal rivalSites room tiles (colony: ColonyView) =
    colony
    |> withOutpostLayer room (fun layer ->
        { layer with
            RivalSites = Set.union layer.RivalSites (Set.ofList tiles)
        })

/// Two Seats and two ways out. `(10,45)` is a row nearer the border and
/// its only run to it is three tiles of swamp; `(11,43)` is a row farther
/// and its run is five of plain. Walk and proximity therefore disagree,
/// which is the whole point of the floor: 6 ticks against 16.
let internal detourGround =
    [
        { X = 10; Y = 45 }, Plain
        { X = 10; Y = 46 }, Swamp
        { X = 10; Y = 47 }, Swamp
        { X = 10; Y = 48 }, Swamp
        { X = 11; Y = 43 }, Plain
        for y in 44..48 do
            { X = 12; Y = y }, Plain
    ]

/// The colony's own room for the haul below: its spawn eleven tiles down a
/// one-wide corridor from the north border, so the only tile a transfer
/// reaches it from on the haul's side is (25,9). No controller, no
/// refillable with room and no home source, so the hauler quota folds the
/// outpost's one container and nothing beside it.
///
/// **A 600 bank and not the default 300, because the rate is what these
/// cases read** (#208). At 300 the Anchor row casts `2W/1C/1M` and digs
/// four, under the neutral five as well as the held ten, so the two rates
/// would price alike. At 600 the row casts five Work against a held rock
/// and three against a neutral one (`sourceOutputOf`), which digs ten and
/// six. The bank's other effect is the divisor, 400 here rather than 200;
/// the round trips are unmoved, both bodies at road parity.
let internal haulHome =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-out" ]
        Bank = bank 600 600
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (corridor 25 1 48)
                    TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
                    Obstacles = Set.singleton { X = 25; Y = 10 }
                })
    }

/// The same colony with its outpost one room north: the rock at (25,40)
/// and the container on the Seat below it. Who holds W1N2 is the
/// caller's; `None` is the room the colony sees nobody in, a third answer
/// and not the neutral one.
let internal withHaulOutpost (control: RoomControlInfo option) (colony: ColonyView) =
    { colony with
        RoomControl =
            match control with
            | Some held -> Map.add "W1N2" held colony.RoomControl
            | None -> colony.RoomControl
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out" Source
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 25 41 48)
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 25; Y = 40 }; "can-out", { X = 25; Y = 41 } ]
                }
    }

/// The same outpost the tick before its container stands: `can-out` simply
/// absent, so the standing census is the only census input that moves.
let internal beforeHaulContainer (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-out"
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = outpost.TargetPositions |> Map.remove "can-out"
                }
    }

/// The same outpost with a **second** rock and container down a side
/// branch of its corridor, the container's tile the caller's: more than
/// one Seam-crossing term in the same sum. The branch runs east along
/// y = 44, so the tile lengthens this container's haul and nothing else's.
let internal withSecondHaulContainer (tile: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Sources = source "src-out2" :: colony.Sources
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out2" Source
                    |> Map.add "can-out2" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        (outpost.Terrain, [ for x in 26..40 -> { X = x; Y = 44 } ])
                        ||> List.fold (fun acc branch -> TerrainGrid.add branch Plain acc)
                    TargetPositions =
                        outpost.TargetPositions
                        |> Map.add "src-out2" { tile with Y = 43 }
                        |> Map.add "can-out2" tile
                }
    }

/// The same haul fixture with an Anchor garrisoning the outpost's container
/// (#153), its ticks to live the caller's.
///
/// A generalist stands at home beside it, the supply floor's premise: an
/// Anchor holds one Carry, so a fleet of Anchors alone can put nothing
/// into an extension and the colony would hire a carrier in front of
/// every row, which is the row these cases would then read.
let internal withOutpostGarrison life (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ [ anchor "a-out" 0 50 |> withLife life; worker "w-home" 0 50 ]
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.add "w-home" { X = 25; Y = 12 } layer.CreepPositions
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = Map.add "a-out" { X = 25; Y = 41 } outpost.CreepPositions
                }
    }

/// The haul fixture's two rooms, the outpost held and its container
/// standing, and one Anchor on that container with the given life left.
///
/// Banked back down to 300, the bank the leads below are counted at:
/// `haulHome` banks 600 for the *quota's* reason (#208). Spelled here so
/// a case that reads a tick count says which body it counted.
let internal outpostSuccession life =
    { haulHome with Bank = bank 300 300 }
    |> withHaulOutpost (Some(reservedRoom true 4000))
    |> withOutpostGarrison life

/// The posted outpost with the hostiles the caller names standing in it:
/// `haulHome`'s two rooms, one anchor garrisoning the Post and two haulers
/// beside it.
///
/// A **new** builder and never a Threat added to a fixture above: every
/// pin those carry is a quiet colony's. Handed the hostiles so the same
/// geometry answers the quiet tick and the raided one pairwise.
///
/// Its outpost is a field eleven tiles wide, x 20..30 and y 41..48, where
/// `withHaulOutpost` lays one corridor, because a raid needs somewhere to
/// run *to*: a `smallMelee` at (25,42) reaches five tiles around it, so
/// every row but the last is inside the Reach and the y = 48 row is the
/// safe set. Down a one-tile corridor Flee would price as unreachable.
///
/// Held rather than neutral: a rate moves quotas and these cases are
/// about the Tasks.
let internal raidedOutpost (hostiles: HostileInfo list) =
    let colony = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = [ anchor "a-out" 0 50; hauler "h-out1" 0 100; hauler "h-out2" 0 100 ]
        Hostiles = hostiles
        Spatial =
            { colony.Spatial with
                Stores = Map.add "can-out" 1000 colony.Spatial.Stores
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 20..30 do
                                    for y in 41..48 -> { X = x; Y = y }, Plain
                            ]
                    CreepPositions =
                        Map.ofList
                            [
                                "a-out", { X = 25; Y = 41 }
                                "h-out1", { X = 24; Y = 42 }
                                "h-out2", { X = 26; Y = 42 }
                            ]
                }
    }

/// The raid itself: one invader a tile below the Post. Filed under the
/// outpost's own room, because a hostile carrying another room's name
/// would take no tile here at all (#138).
let internal raiders = [ hostileIn "W1N2" { X = 25; Y = 42 } smallMelee ]

/// The raided outpost as a **declared** one: its controller projected at a
/// corner of the same field, which is what makes W1N2 a declared outpost
/// the guard row hires for. It arrives here rather than in the builders
/// above because a controller in the projection pools a Reserve, and
/// every quiet pin above would re-baseline on it.
let internal declaredRaid hostiles =
    let colony = raidedOutpost hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctl-out" Controller colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "ctl-out" { X = 30; Y = 48 } outpost.TargetPositions
                }
    }

/// The same raided outpost with guards of ours standing in it, each on the
/// tile the case names. Built **on top of** `declaredRaid`: every pin
/// above is a raid nobody answers, and this is the one that is answered.
let internal withGuards (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ List.map fst ours
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions =
                        (outpost.CreepPositions, ours)
                        ||> List.fold (fun tiles (creep, pos) -> Map.add creep.Name pos tiles)
                }
    }

/// The tile a body the guard row casts stands on: `haulHome`'s corridor is
/// one wide and its spawn plugs it at (25,10), so (25,9) is the one tile
/// beside the spawn on the outpost's side.
let internal atSpawn = { X = 25; Y = 9 }

/// The same raided outpost with one body of ours standing **at home**, on
/// the tile the caller names. The guard row hires at the spawn, so this
/// is where every guard the colony buys begins its life, and where the
/// body #147 watched cross the Seam begins its too.
let internal withBodyAtHome (creep: CreepInfo) (pos: Pos) (colony: ColonyView) =
    { colony with
        Creeps = colony.Creeps @ [ creep ]
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.add creep.Name pos layer.CreepPositions
                })
    }

/// A `smallHealer` of the raid's, well off the fight at (22,47): no
/// threat, so no ground taken and no ring tile, only the count rule's
/// arithmetic. Each carries an id of its own, a raid being a roster.
let internal healers count =
    [
        for i in 1..count ->
            { hostileIn "W1N2" { X = 22; Y = 47 } smallHealer with
                Id = $"heal-{i}"
            }
    ]

/// This tick's pool for a colony, the two Planner halves in the order
/// `decide` runs them.
let internal pooledOf colony =
    let atlas = Atlas.ofView colony
    planPool colony atlas (planTasksOn colony (threatsOf colony atlas))

let internal entryFor task pool =
    pool |> List.tryFind (fun (entry: PooledTask) -> entry.Task = task)

/// The guard's two acts, read off the tick's Intents.
let internal attacksOf intents =
    intents
    |> List.choose (function
        | AttackCreep(name, hostile) -> Some(name, hostile)
        | _ -> None)

let internal healsOf intents =
    intents
    |> List.choose (function
        | HealCreep(name, target) -> Some(name, target)
        | _ -> None)

/// The rows a verbose scoring rejected for one creep, in pool order.
let internal rejectionsFor name verdicts =
    verdicts
    |> List.tryPick (function
        | Verdict.Scoring(creep, rows) when creep = name ->
            rows
            |> List.choose (function
                | Candidate.Rejected(task, reason) -> Some(task, reason)
                | Candidate.Scored _ -> None)
            |> Some
        | _ -> None)

/// The ring tile the cases stand a guard on: south-west of the invader at
/// (25,42), inside its range-1 ring and **free**, which the tiles due
/// west and east are not. The engine puts no two bodies on one tile and
/// the Atlas's occupancy grid cannot say that it did.
let internal beside = { X = 24; Y = 43 }

/// A second free ring tile of the same invader, south-east where `beside`
/// is south-west.
let internal besideToo = { X = 26; Y = 43 }

/// The second outpost, across the *west* border: one room is not enough to
/// tell "one reserver per outpost" apart from "every reserver on the
/// nearest controller". The two tiles left beside its controller are the
/// declared shape W12S27's `37,43` really has.
let internal westReserveDeclaration =
    {
        RoomName = "W2N1"
        Sources = []
        Controller = "ctrl-west", { Room = "W2N1"; X = 41; Y = 25 }
    }

/// The colony with both outposts declared and a west arm of home leading
/// to the second: home's `1,26` opens onto W2N1's `48,26`. The creeps
/// stand in that arm, a dozen steps from the west controller and some
/// thirty-five from the north one, so travel cost alone prefers the
/// *same* controller for every one of them.
let internal twoOutpostColony (creeps: (CreepInfo * Pos) list) =
    let colony = reserveColony []

    let spatial =
        { colony.Spatial with
            Borders = Map.add "W2N1" plainRing colony.Spatial.Borders
        }
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                    ||> List.fold (fun terrain pos -> TerrainGrid.add pos Plain terrain)
                CreepPositions =
                    creeps |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
            })
        |> withNeighbour
            "W2N1"
            { RoomLayer.empty with
                Terrain = TerrainGrid.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
            }
        |> Outpost.place [ westReserveDeclaration ]

    { colony with
        Creeps = creeps |> List.map fst
        Spatial = spatial
    }

/// One outpost declared off the tuple the reserver row's fixtures are
/// written in, spelling the same ids and tiles `withOutpostRoom`
/// furnishes: the declaration is what a stand-down subtracts and the
/// furniture is what vision pays for, and the two have to name one room.
/// The `posted` flag rides along unread: `gatedColony` posts every room
/// it works, because an unposted outpost contributes nothing to three of
/// the four rows and would make the gate's subtraction unreadable.
let internal gatedOutpost (room, rock: Pos, _posted) : Outpost =
    {
        RoomName = room
        Sources = [ $"src-{room}", RoomPos.at room rock ]
        Controller = $"ctrl-{room}", RoomPos.at room { rock with Y = rock.Y + 2 }
    }

/// The colony the shell assembles for a declaration under a shut set, the
/// way `ColonyView.ofWorld` builds one: the declarations less what the
/// gate withholds (`Outpost.worked`), then every fact of a worked room
/// and *none* of a withheld one. The furniture is laid only into rooms
/// the scan set carries (`Outpost.place`), the rocks pooled only for
/// those rooms (`Outpost.pooledSources`), and every entry vision pays
/// for is collected over `seen`.
///
/// The reservation stands at its 5,000 cap on every worked room, so the
/// reserver row's casts are a count of rooms and never a deficit.
///
/// Assembled by `reserverColony` so the gate reads over the same colony
/// the reserver row is pinned on.
let internal gatedColony declarations shut creeps =
    let worked = Outpost.worked shut declarations

    reserverColony
        (worked
         |> List.map (fun (outpost: Outpost) ->
             outpost.RoomName, outpost.Sources |> List.head |> snd |> RoomPos.pos, true))
        creeps
        (worked |> List.map (fun outpost -> outpost.RoomName, reservedRoom true 5000))

/// The two outposts the gate is read over, diagonal to each other as
/// W12S27 and W13S28 are: one is not enough to tell "this room is
/// withheld" from "outposts are withheld".
let internal northGated = gatedOutpost (northOutpost true)

let internal westGated = gatedOutpost (westOutpost true)

/// A creep standing in a room, placed the way the shell places one: in
/// the layer of the room it stands in, and nowhere when that room is not
/// projected. The shell reads the rooms it scans and no others, so a
/// stood-down room's creep is unplaced.
let internal standingIn room (name, pos) (colony: ColonyView) =
    match Map.tryFind room colony.Spatial.Rooms with
    | None -> colony
    | Some layer ->
        { colony with
            Spatial =
                colony.Spatial
                |> withNeighbour
                    room
                    { layer with
                        CreepPositions = Map.add name pos layer.CreepPositions
                    }
        }

/// Every Task in the pool that names a room's furniture — the rock, the
/// controller and the container `withOutpostRoom` gives it, whose ids all
/// carry the room's name.
let internal tasksNaming room colony =
    planTasksOn colony noThreats
    |> List.map taskId
    |> List.filter (fun id -> (id: string).Contains(room: string))
