/// The Atlas suite's fixtures: the minimal views, creeps and rooms every
/// group below is built from, and the helpers that take a room name back off
/// an answer so an expectation can be written as one room's geometry.
module Fabot.Core.Tests.AtlasFixtures

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas

/// Minimal ColonyView around a spatial projection: the Atlas reads only the
/// projection and the creep list's order.
let snapshotWith creeps spatial =
    {
        Time = 1
        Spawns = []
        Bank = { Available = 0; Capacity = 0 }
        Refillables = []
        Sources = []
        Controller = None
        // Everything below the projection and the creeps is empty: the
        // Atlas reads geometry and nothing else. An invader core reaches
        // it as an ordinary structure's obstacle or not at all.
        RoomControl = Map.empty
        HeldOutposts = Set.empty
        ThreatenedOutposts = Set.empty
        ConstructionSites = []
        Creeps = creeps
        Hostiles = []
        InvaderCores = []
        Spatial = spatial
        Declared = []
        Stages = Map.empty
        Foreign = Set.empty
        Borrowed = { Rooms = [] }
        Refused = []
        Errands = []
        Consignee = None
        Crossed = Set.empty
        Reactors = []
        Sightings = Map.empty
        Tuning = Tuning.defaults
        Casting = []
    }

let worker name =
    {
        Name = name
        TicksToLive = 1500
        Hits = { Hits = 300; HitsMax = 300 }
        Fatigue = 0
        Energy = 0
        Thorium = 0
        FreeCapacity = 50
        Moved = false
        Body = Map.ofList [ Work, 1; Carry, 1; Move, 1 ]
    }

/// A creep with the given carried energy and body's part counts.
let creepWith name energy body =
    {
        Name = name
        TicksToLive = 1500
        Hits =
            {
                Hits = Engine.partHits * List.length body
                HitsMax = Engine.partHits * List.length body
            }
        Fatigue = 0
        Energy = energy
        Thorium = 0
        FreeCapacity = 50
        Moved = false
        Body = body |> List.countBy id |> Map.ofList
    }

/// The room a projection calls its own, defaulting the way
/// `SpatialInfo.homeName` does.
let atlasHome (atlas: Atlas) : string =
    homeRoom atlas |> Option.defaultValue ""

/// The tiles of that room, back as grid coordinates. An assertion, not a
/// cast: a tile answered in another room is dropped and the expectation fails.
let tilesHome (atlas: Atlas) (tiles: Set<RoomPos>) : Set<Pos> =
    RoomPos.inRoom (atlasHome atlas) tiles

/// The same for a room the caller names.
let tilesIn (room: string) (tiles: Set<RoomPos>) : Set<Pos> = RoomPos.inRoom room tiles

/// The same for the queries that answer at most one tile.
let tileHome (atlas: Atlas) (tile: RoomPos option) : Pos option =
    tile
    |> Option.filter (fun t -> t.Room = atlasHome atlas)
    |> Option.map RoomPos.pos

/// A step back as a grid coordinate of the stepping creep's own room; the
/// filter asserts that rather than assumes it.
let stepOf (atlas: Atlas) (creep: string) (step: RoomPos option) : Pos option =
    step
    |> Option.filter (fun tile -> creepRoom atlas creep = Some tile.Room)
    |> Option.map RoomPos.pos

/// `trunkPath` over the atlas's own room, tiles in and out as that room's
/// grid coordinates; the join is pinned in the Layout's cross-room test.
let trunkPathHome (atlas: Atlas) (avoid: Set<Pos>) (origin: Pos) (goals: Set<Pos>) : Pos list =
    let room = atlasHome atlas

    trunkPath atlas (RoomPos.setAt room avoid) (RoomPos.at room origin) (RoomPos.setAt room goals)
    |> List.map RoomPos.pos

/// A tile of a named room.
let at room (tile: Pos) = RoomPos.at room tile

/// `mayAct` over a Task's own Work Area, nothing taken out of it.
let mayActFor atlas creep task =
    mayAct atlas creep task (workAreaFor atlas creep task)

/// `firstStep` over a Task's own Work Area. The Task rides beside the tiles
/// because a target in the neighbouring room leaves the creep-aware area
/// empty and the step is then the Seam's near side.
let firstStepFor atlas creep task =
    firstStep atlas creep task (workAreaFor atlas creep task) |> stepOf atlas creep

/// The traffic-blind route over the same area.
let firstStepBlindFor atlas creep task =
    firstStepIgnoringTraffic atlas creep task (workAreaFor atlas creep task)
    |> stepOf atlas creep

/// A row of the refill cluster on open ground: spawn-1 at (10,10) with two
/// extensions east of it, every structure tile an obstacle, plain
/// everywhere else in the band. The caller says how much room each has left.
let clusterAtlas creeps (spawnFree, ext1Free, ext2Free) =
    let structures =
        [
            "spawn-1", { X = 10; Y = 10 }
            "ext-1", { X = 14; Y = 10 }
            "ext-2", { X = 12; Y = 10 }
        ]

    let view =
        spatial
            structures
            [
                for x in 8..16 do
                    for y in 9..11 -> { X = x; Y = y }, Plain
            ]
        |> withHome (fun layer ->
            { layer with
                Obstacles = structures |> List.map snd |> Set.ofList
                CreepPositions = Map.ofList creeps
            })
        |> snapshotWith (
            creeps |> List.map (fun (name, _) -> creepWith name 50 [ Carry; Carry; Move ])
        )

    { view with
        Refillables =
            [
                {
                    Id = "spawn-1"
                    FreeCapacity = spawnFree
                    Kind = BuiltKind.Spawn
                }
                {
                    Id = "ext-1"
                    FreeCapacity = ext1Free
                    Kind = BuiltKind.Extension
                }
                {
                    Id = "ext-2"
                    FreeCapacity = ext2Free
                    Kind = BuiltKind.Extension
                }
            ]
    }
    |> ofView

/// Source at (10,12) on a wall; its Work Area is a swamp Seat at (10,13)
/// and a plain Seat at (11,13). A plain lane runs down to (10,15).
let corridor creeps =
    spatial
        [ "src-a", { X = 10; Y = 12 } ]
        [
            { X = 10; Y = 12 }, Wall
            { X = 10; Y = 13 }, Swamp
            { X = 11; Y = 13 }, Plain
            { X = 10; Y = 14 }, Plain
            { X = 11; Y = 14 }, Plain
            { X = 10; Y = 15 }, Plain
        ]
    |> withCreepsAt creeps

/// Source at (10,10) whose only Seat is at (10,11) with the given terrain
/// and roads; the creep "w" stands one step below on plain — the cost is
/// exactly one step onto that Seat.
let seatPriced terrain roads =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, terrain
            { X = 10; Y = 12 }, Plain
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ "w", { X = 10; Y = 12 } ]
            Roads = roads
        })

/// Source at (10,10) behind a Seat at (10,11); the creep "w" at (10,13)
/// has its only route through (10,12), the tile each caller dresses. One
/// tile wide, so no diagonal skirts it.
let corridorThrough middle roads obstacles =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        ([
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 13 }, Plain
         ]
         @ middle)
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ "w", { X = 10; Y = 13 } ]
            Roads = roads
            Obstacles = obstacles
        })

/// Two lanes to one Seat: source at (9,9) in wall with (10,10) its only
/// Seat, the creep "w" at (10,12). One swamp tile at (10,11) joins them in
/// two steps; a five-tile paved ring joins them in six. No diagonal links
/// the lanes.
let forkedLanes creeps =
    let ring =
        [
            { X = 11; Y = 13 }
            { X = 12; Y = 12 }
            { X = 12; Y = 11 }
            { X = 12; Y = 10 }
            { X = 11; Y = 9 }
        ]

    spatial
        [ "src-a", { X = 9; Y = 9 } ]
        ([
            { X = 9; Y = 9 }, Wall
            { X = 10; Y = 10 }, Plain
            { X = 10; Y = 11 }, Swamp
            { X = 10; Y = 12 }, Plain
         ]
         @ [ for tile in ring -> tile, Plain ])
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList ring
            CreepPositions = Map.ofList creeps
        })

/// A paved lane east from the source's only Seat (11,10) out to (19,10):
/// a creep there is eight road steps from the Seat (#79's corridor).
let roadCorridor creeps =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [ for x in 10..19 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ]
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]
            CreepPositions = Map.ofList creeps
        })

/// The same lane unpaved: six plain steps from (17,10) to the Seat.
let plainCorridor creeps =
    spatial
        [ "src-a", { X = 10; Y = 10 } ]
        [ for x in 10..17 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ]
    |> withCreepsAt creeps

/// 15 × 15 of mixed terrain with single walls spaced four apart, so the
/// passable tiles stay one connected component.
let mixedRoom creeps =
    let tiles =
        [
            for x in 0..14 do
                for y in 0..14 do
                    if x % 4 = 1 && y % 4 = 1 then { X = x; Y = y }, Wall
                    elif (x + y) % 3 = 0 then { X = x; Y = y }, Swamp
                    else { X = x; Y = y }, Plain
        ]

    spatial
        [
            for i, tile in List.indexed [ { X = 3; Y = 3 }; { X = 11; Y = 4 }; { X = 6; Y = 12 } ] ->
                $"src-%d{i}", tile
        ]
        tiles
    |> withHome (fun layer ->
        { layer with
            Roads =
                Set.ofList
                    [
                        for x in 0..14 do
                            for y in 0..14 do
                                if (2 * x + y) % 5 = 0 then
                                    { X = x; Y = y }
                    ]
            CreepPositions = Map.ofList creeps
        })

/// The store-ring room (#268): the source at (10,10) with its eight
/// neighbours open, "can-a" on the Seat (11,10), a corridor east along
/// y = 10 with the Storage at (20,10) and the spawn at (30,10), both
/// obstacles. No controller, so the working ground is the Seats alone.
let internal storeRingRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "can-a", { X = 11; Y = 10 }
              "sto-1", { X = 20; Y = 10 }
              "spawn-1", { X = 30; Y = 10 }
          ]
          ([
              for dx in -1 .. 1 do
                  for dy in -1 .. 1 do
                      if (dx, dy) <> (0, 0) then
                          { X = 10 + dx; Y = 10 + dy }, Plain
           ]
           @ [ { X = 10; Y = 10 }, Wall ]
           @ [ for x in 12..32 -> { X = x; Y = 10 }, Plain ]) with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "can-a", Structure BuiltKind.Container
                    "sto-1", Structure BuiltKind.Storage
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withObstacles [ { X = 20; Y = 10 }; { X = 30; Y = 10 } ]

/// The same room seen by a colony whose spawn is a refill cluster member:
/// the cluster is read off the view's Refillables.
let internal storeRingView =
    { snapshotWith [] storeRingRoom with
        Refillables =
            [
                {
                    Id = "spawn-1"
                    FreeCapacity = 0
                    Kind = BuiltKind.Spawn
                }
            ]
    }

/// A projection carrying border rings under room names, each room's interior
/// plain. A tile a ring leaves out is impassable. The plain interior is
/// what lets the cases vary the ring alone: a landing with no ground
/// beside it is no crossing.
let bordered rings =
    let plain =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { SpatialInfo.empty with
        Borders = rings |> List.map (fun (room, tiles) -> room, Map.ofList tiles) |> Map.ofList
        Rooms =
            rings
            |> List.map (fun (room, _) -> room, { RoomLayer.empty with Terrain = plain })
            |> Map.ofList
    }

/// The one tile of ground a far room is given behind each ring tile, set
/// diagonally behind it (`landsOnGround` counts diagonals). A case that
/// wants a groundless far room builds its own projection.
///
/// `None` at the low end of an edge (`x = 1` on a y-row, `y = 1` on a
/// column), where the diagonal would leave the 1..48 ground: such an exit
/// drops out of every band silently. No case places one today.
let private diagonallyBehind (tile: Pos) : Pos option =
    let inside (pos: Pos) =
        if pos.X >= 1 && pos.X <= 48 && pos.Y >= 1 && pos.Y <= 48 then
            Some pos
        else
            None

    if tile.Y = 0 then
        inside { X = tile.X - 1; Y = 1 }
    elif tile.Y = Seam.exitEdge then
        inside
            {
                X = tile.X - 1
                Y = Seam.exitEdge - 1
            }
    elif tile.X = 0 then
        inside { X = 1; Y = tile.Y - 1 }
    elif tile.X = Seam.exitEdge then
        inside
            {
                X = Seam.exitEdge - 1
                Y = tile.Y - 1
            }
    else
        None

/// One room's ground (W12S28's) and any number of rooms' border rings;
/// every other room gets `diagonallyBehind`'s single ground tile per exit.
let internal seamGround ground rings =
    { SpatialInfo.empty with
        RoomName = Some "W12S28"
        Rooms =
            rings
            |> List.map (fun (room, tiles) ->
                room,
                { RoomLayer.empty with
                    Terrain =
                        tiles
                        |> List.choose (fun (tile, _) -> diagonallyBehind tile)
                        |> List.map (fun tile -> tile, Plain)
                        |> TerrainGrid.ofList
                })
            |> Map.ofList
            |> Map.add
                "W12S28"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList ground
                }
        Borders = rings |> List.map (fun (room, tiles) -> room, Map.ofList tiles) |> Map.ofList
    }
    |> snapshotWith []
    |> ofView

/// A plain three-tile column up to the room's north exit: the smallest
/// room that has a walk out to a Seam at all.
let internal toNorthExit =
    [ { X = 10; Y = 1 }, Plain; { X = 10; Y = 2 }, Plain; { X = 10; Y = 3 }, Plain ]

let internal northExit terrain =
    [
        "W12S28", [ { X = 10; Y = 0 }, terrain ]
        "W12S27", [ { X = 10; Y = 49 }, Plain ]
    ]

/// A straight line of Plain ground.
let internal plainLine tiles =
    tiles |> List.map (fun tile -> tile, Plain)

/// A room whose source sits at (10,10) and whose controller is four tiles
/// south, so the Seats on the source's south rank lie in its Upgrade area and
/// the rest do not; an outpost carries the very same coordinates beside it.
let internal pinnedLayer =
    { RoomLayer.empty with
        Terrain =
            TerrainGrid.ofList
                [
                    for x in 8..12 do
                        for y in 8..15 do
                            // (11,10) is a Seat by range and no Seat by ground.
                            if (x, y) <> (11, 10) then
                                { X = x; Y = y }, Plain
                ]
        TargetPositions = Map.ofList [ "src", { X = 10; Y = 10 }; "ctrl", { X = 10; Y = 14 } ]
    }

let internal pinnedTwoRooms homeCreeps outCreeps =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds =
            Map.ofList
                [ "src", Source; "ctrl", Controller; "src-out", Source; "ctrl-out", Controller ]
    }
    |> withHome (fun _ ->
        { pinnedLayer with
            CreepPositions = Map.ofList homeCreeps
        })
    |> withNeighbour
        "W2N1"
        { pinnedLayer with
            TargetPositions =
                Map.ofList [ "src-out", { X = 10; Y = 10 }; "ctrl-out", { X = 10; Y = 14 } ]
            CreepPositions = Map.ofList outCreeps
        }

/// The colony's own room and one outpost across its north border, rings
/// and all. W1N1 is world (-2,-2) and W1N2 (-2,-3), so stepping onto y=0
/// at home lands the creep on y=49 there. The ColonyView is handed out
/// because one case prices a lead over a walk table it supplies itself.
let internal northOfSnapshot
    (home: RoomLayer)
    homeRing
    (outpost: RoomLayer)
    outpostRing
    kinds
    creeps
    =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Borders = Map.ofList [ "W1N1", Map.ofList homeRing; "W1N2", Map.ofList outpostRing ]
        TargetKinds = Map.ofList kinds
    }
    |> withHome (fun _ -> home)
    |> withNeighbour "W1N2" outpost
    |> snapshotWith creeps

let internal northOf (home: RoomLayer) homeRing (outpost: RoomLayer) outpostRing kinds creeps =
    northOfSnapshot home homeRing outpost outpostRing kinds creeps |> ofView

/// One plain corridor down column 25 to the exit row, with the creeps in it.
let internal corridorHome creeps =
    { RoomLayer.empty with
        Terrain = TerrainGrid.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
        CreepPositions = Map.ofList creeps
    }

/// The same corridor with a source on ground the projection does not
/// carry, so its Work Area is the one tile below it.
let internal corridorOutpost =
    { RoomLayer.empty with
        Terrain =
            TerrainGrid.ofList (
                plainLine
                    [
                        for y in 1..48 do
                            if y <> 40 then
                                { X = 25; Y = y }
                    ]
            )
        TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
    }

/// The cross-room corridor with a rock at (25,20) of the colony's own room
/// and one at (25,40) of the outpost, neither with a container, and a
/// Work-heavy body and a light one in each room: `workAreaFor` answers a
/// creep only in its target's own room, so the tiles are read from
/// inside each room and the price from the home pair. The caller places
/// what stands in the outpost.
let internal twoRockRooms placed kinds =
    northOf
        { corridorHome [ "a", { X = 25; Y = 10 }; "w", { X = 25; Y = 11 } ] with
            TargetPositions = Map.ofList [ "src-home", { X = 25; Y = 20 } ]
        }
        [ { X = 25; Y = 0 }, Plain ]
        { corridorOutpost with
            TargetPositions =
                (corridorOutpost.TargetPositions, placed)
                ||> List.fold (fun targets (id, pos) -> Map.add id pos targets)
            CreepPositions = Map.ofList [ "a-out", { X = 25; Y = 44 }; "w-out", { X = 25; Y = 45 } ]
        }
        [ { X = 25; Y = 49 }, Plain ]
        (("src-home", Source) :: ("src-out", Source) :: kinds)
        [
            creepWith "a" 0 [ Work; Work; Carry; Move ]
            worker "w"
            creepWith "a-out" 0 [ Work; Work; Carry; Move ]
            worker "w-out"
        ]

/// The Seats of the outpost's rock: the corridor tile above it and the one below.
let internal outpostSeats = Set.ofList [ { X = 25; Y = 39 }; { X = 25; Y = 41 } ]

/// The spawn structure of every lead, at (25,10) of the home corridor: an
/// obstacle, so a finished body is born on (25,9) or (25,11).
let internal leadSpawn = { X = 25; Y = 10 }

/// The tile in the outpost a lead is priced to: the Seat under that room's rock.
let internal outpostSeat = { X = 25; Y = 41 }

/// The cross-room corridors with the spawn structure in the home one and
/// the rings the caller shapes. The ColonyView, because one case hands its
/// own walk table in.
let internal leadAcrossSnapshot homeRing outpostRing placed creeps =
    northOfSnapshot
        { corridorHome placed with
            TargetPositions = Map.ofList [ "spawn-1", leadSpawn ]
            Obstacles = Set.singleton leadSpawn
        }
        homeRing
        corridorOutpost
        outpostRing
        [ "spawn-1", Structure BuiltKind.Spawn; "src-out", Source ]
        creeps

let internal leadAcross homeRing outpostRing placed creeps =
    leadAcrossSnapshot homeRing outpostRing placed creeps |> ofView

/// The hauler the haul is priced for, and the body of the two creeps on
/// its container: `fatigueFactorOf` reads a living creep's load, so a full
/// one carries the loaded factor and an empty one the empty factor.
let internal haulerBody = [ Carry; Carry; Move ]

/// The container at (25,41) of the outpost's corridor, the spawn structure
/// at (25,10) of the home room's: an obstacle, so the haul reaches it from
/// (25,9). Two haulers stand on the container, one full and one empty.
/// The rings are the caller's.
let internal haulAcross homeRing outpostRing =
    northOf
        { RoomLayer.empty with
            Terrain = TerrainGrid.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
            TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
            Obstacles = Set.singleton { X = 25; Y = 10 }
        }
        homeRing
        { RoomLayer.empty with
            Terrain = TerrainGrid.ofList (plainLine [ for y in 41..48 -> { X = 25; Y = y } ])
            TargetPositions = Map.ofList [ "can-out", { X = 25; Y = 41 } ]
            CreepPositions =
                Map.ofList [ "loaded", { X = 25; Y = 41 }; "empty", { X = 25; Y = 41 } ]
        }
        outpostRing
        [
            "spawn-1", Structure BuiltKind.Spawn
            "can-out", Structure BuiltKind.Container
        ]
        [ creepWith "loaded" 100 haulerBody; creepWith "empty" 0 haulerBody ]

/// The outpost's container to the home room's spawn.
let internal roundTripOf atlas =
    haulRoundTripTicks
        atlas
        haulerBody
        (at "W1N2" { X = 25; Y = 41 })
        (at "W1N1" { X = 25; Y = 10 })

/// Three rooms in a north-south line, rings and all: W1N1 (the colony's
/// own) at world (-2,-2), W1N2 at (-2,-3) and W1N3 at (-2,-4), each
/// crossing pairing y=0 here with y=49 there. The middle room is a transit
/// room: terrain and a ring and nothing else. The ColonyView, because one
/// case hands its own far-field tables in.
let internal chainOfThreeSnapshot
    (home: RoomLayer)
    homeRing
    middleRing
    (middle: RoomLayer)
    farRing
    (far: RoomLayer)
    kinds
    creeps
    =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Borders =
            Map.ofList
                [
                    "W1N1", Map.ofList homeRing
                    "W1N2", Map.ofList middleRing
                    "W1N3", Map.ofList farRing
                ]
        TargetKinds = Map.ofList kinds
    }
    |> withHome (fun _ -> home)
    |> withNeighbour "W1N2" middle
    |> withNeighbour "W1N3" far
    |> snapshotWith creeps

let internal chainOfThree home homeRing middleRing middle farRing far kinds creeps =
    chainOfThreeSnapshot home homeRing middleRing middle farRing far kinds creeps
    |> ofView

/// Four rooms in the same line, W1N1 through W1N4: a chain three crossings
/// long, at `Tuning.MaxHops`' own budget.
let internal chainOfFour
    (home: RoomLayer)
    homeRing
    firstRing
    (first: RoomLayer)
    secondRing
    (second: RoomLayer)
    farRing
    (far: RoomLayer)
    kinds
    creeps
    =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Borders =
            Map.ofList
                [
                    "W1N1", Map.ofList homeRing
                    "W1N2", Map.ofList firstRing
                    "W1N3", Map.ofList secondRing
                    "W1N4", Map.ofList farRing
                ]
        TargetKinds = Map.ofList kinds
    }
    |> withHome (fun _ -> home)
    |> withNeighbour "W1N2" first
    |> withNeighbour "W1N3" second
    |> withNeighbour "W1N4" far
    |> snapshotWith creeps
    |> ofView

/// The same plain corridor down column 25 with no creep and no target in it.
let internal corridorTransit =
    { RoomLayer.empty with
        Terrain = TerrainGrid.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
    }

/// The Source Keeper room on the chain to the sector Reactor, under its own
/// declared name and over invented ground: every tile of the window plain,
/// ring included. The keeper margin is declared by room name and is
/// terrain-blind, so an invented ground under a real name says something
/// true about the real declaration; a case that needs the server's terrain
/// belongs in `RoomSeamTests`.
let internal keeperRoom =
    let plain window =
        TerrainGrid.ofList
            [
                for x in window do
                    for y in window -> { X = x; Y = y }, Plain
            ]

    let ring =
        Map.ofList
            [
                for x in 0..49 do
                    for y in 0..49 do
                        if x = 0 || x = 49 || y = 0 || y = 49 then
                            { X = x; Y = y }, Plain
            ]

    { SpatialInfo.empty with
        RoomName = Some "W15S26"
        Rooms =
            Map.ofList
                [
                    "W15S26",
                    { RoomLayer.empty with
                        Terrain = plain [ 1..48 ]
                        // The room's three sources at the declared tiles: a
                        // Seat count says the mask reached the terrain grid.
                        TargetPositions =
                            Map.ofList
                                [
                                    "sk-src-0", { X = 11; Y = 16 }
                                    "sk-src-1", { X = 4; Y = 33 }
                                    "sk-src-2", { X = 39; Y = 34 }
                                ]
                    }
                    // The two rooms the chain joins it to, on the same ground.
                    "W15S27",
                    { RoomLayer.empty with
                        Terrain = plain [ 1..48 ]
                    }
                    "W15S25",
                    { RoomLayer.empty with
                        Terrain = plain [ 1..48 ]
                    }
                    // The two rooms at the ends, with ground: `World.ofGame`
                    // reads a room's terrain wherever it reads its ring.
                    "W15S28",
                    { RoomLayer.empty with
                        Terrain = plain [ 1..48 ]
                    }
                    "W16S26",
                    { RoomLayer.empty with
                        Terrain = plain [ 1..48 ]
                    }
                ]
        // W15S28 for the whole three-hop chain, W16S26 for its west border
        // alone: the mask reaches that ring where it reaches neither of the chain's.
        Borders =
            Map.ofList
                [
                    "W15S28", ring
                    "W15S27", ring
                    "W15S26", ring
                    "W15S25", ring
                    "W16S26", ring
                ]
        TargetKinds = Map.ofList [ "sk-src-0", Source; "sk-src-1", Source; "sk-src-2", Source ]
    }

/// The same projection with one body standing in one of its rooms: what
/// `Atlas.stepTowardRoom` needs beyond the geometry (#317).
let internal keeperRoomStanding (room: string) (tile: Pos) =
    { keeperRoom with
        RoomName = Some room
        Rooms =
            keeperRoom.Rooms
            |> Map.add
                room
                { SpatialInfo.layerOf keeperRoom room with
                    CreepPositions = Map.ofList [ "w", tile ]
                }
    }
