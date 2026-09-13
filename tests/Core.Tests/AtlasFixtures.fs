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
        // Who holds a room prices its sources (ADR 0042) and the Atlas
        // prices nothing: geometry is all it reads.
        RoomControl = Map.empty
        ConstructionSites = []
        Creeps = creeps
        Hostiles = []
        // A threat prices nothing and blocks nothing either: the Atlas
        // reads geometry, and an invader core reaches it as an ordinary
        // structure's obstacle or not at all (ADR 0043).
        InvaderCores = []
        Spatial = spatial
        // A declaration prices nothing here either: the Atlas reads
        // geometry, and which rooms a human means to own is the Planner's
        // question (ADR 0047).
        Declared = []
        // And neither does a [[stage]]: where a colony stands in its life
        // decides what it builds and who it raises (ADR 0052 decision 3),
        // and the Atlas builds nothing.
        Stages = Map.empty
        // Nor another colony's bodies: the Atlas places the creeps a view
        // holds, and a body it does not hold is on no tile of its layers
        // (ADR 0052 decision 1).
        Foreign = Set.empty
        // And nothing is borrowed: what one colony may take of a child's
        // room decides Tasks, and the Atlas decides none (ADR 0047
        // decision 4).
        Borrowed = { Rooms = [] }
        // And nothing refused: the outposts these fixtures name border
        // their home, which is what makes a Seam band to price over (#243).
        Refused = []
        // And nothing remembered of a room it cannot see: the sighting map
        // is the Matcher's alone and the Atlas prices nothing off it (#151).
        Sightings = Map.empty
        // The numbers this bot ships with (ADR 0052 decision 5): a
        // fixture starts from them and the tests that are *about* a
        // tunable move the one field they are about.
        Tuning = Tuning.defaults
        // Nothing in the oven: a fixture's rows count what is alive, and
        // the casting cascade's own tests are the ones that put a body
        // here (#156).
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
/// `SpatialInfo.homeName` does — the fixtures below leave it empty unless
/// they name one. The room the Atlas's per-room queries are asked for
/// here, written once instead of at every call.
let atlasHome (atlas: Atlas) : string =
    homeRoom atlas |> Option.defaultValue ""

/// The tiles of that room, back as grid coordinates. Every Atlas query
/// hands tiles out joined to their room since #216 R3 (ADR 0052 decision
/// 2), and the expectations below are written as one room's geometry — so
/// taking the room off here is an assertion and not a cast: a tile
/// answered in another room is dropped, and the expectation it was meant
/// for fails.
let tilesHome (atlas: Atlas) (tiles: Set<RoomPos>) : Set<Pos> =
    RoomPos.inRoom (atlasHome atlas) tiles

/// The same for a room the caller names — an outpost's, where the query
/// is about the neighbour's geometry rather than the colony's own.
let tilesIn (room: string) (tiles: Set<RoomPos>) : Set<Pos> = RoomPos.inRoom room tiles

/// The same for the queries that answer at most one tile.
let tileHome (atlas: Atlas) (tile: RoomPos option) : Pos option =
    tile
    |> Option.filter (fun t -> t.Room = atlasHome atlas)
    |> Option.map RoomPos.pos

/// A step back as a grid coordinate of the **stepping creep's own room** —
/// the room a step is always a tile of (ADR 0041), which is what the
/// filter asserts rather than assumes.
let stepOf (atlas: Atlas) (creep: string) (step: RoomPos option) : Pos option =
    step
    |> Option.filter (fun tile -> creepRoom atlas creep = Some tile.Room)
    |> Option.map RoomPos.pos

/// `trunkPathHome` over the atlas's own room: the room the Layout plans and
/// the room these fixtures build in (ADR 0011). Tiles go in and come back
/// as that room's grid coordinates, which is how the expectations below
/// are written — the join itself is the query's own and is pinned where
/// it bites, in the Layout's cross-room test.
let trunkPathHome (atlas: Atlas) (avoid: Set<Pos>) (origin: Pos) (goals: Set<Pos>) : Pos list =
    let room = atlasHome atlas

    trunkPath atlas (RoomPos.setAt room avoid) (RoomPos.at room origin) (RoomPos.setAt room goals)
    |> List.map RoomPos.pos

/// A tile of a named room — the shape every Atlas query hands a tile back
/// in since #216 R3 (ADR 0052 decision 2).
let at room (tile: Pos) = RoomPos.at room tile

/// `mayAct` over a Task's own Work Area — the tiles the decision layer
/// hands it on a tick with nothing taken out of one (ADR 0033).
let mayActFor atlas creep task =
    mayAct atlas creep task (workAreaFor atlas creep task)

/// `firstStep` over a Task's own Work Area, the same tick the mover sees:
/// the Task rides beside the tiles since #142, because a target in the
/// neighbouring room leaves the creep-aware area empty and the step is then
/// the Seam's near side.
let firstStepFor atlas creep task =
    firstStep atlas creep task (workAreaFor atlas creep task) |> stepOf atlas creep

/// The traffic-blind route over the same area — the half the reroute
/// attribution compares against (ADR 0008, ADR 0018).
let firstStepBlindFor atlas creep task =
    firstStepIgnoringTraffic atlas creep task (workAreaFor atlas creep task)
    |> stepOf atlas creep

/// A row of the [[refill cluster]] standing on open ground (ADR 0054):
/// spawn-1 at (10,10) with two extensions east of it, every structure tile
/// an obstacle as the engine has it, and plain everywhere else in the band.
/// Each caller says how much room each of the three has left, which is what
/// decides the Work Area and the Emitter's pick.
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

/// Source at (10,10) behind a Seat at (10,11); the creep "w" stands two
/// steps below it at (10,13), so its only route runs through (10,12) —
/// the tile each corridor test dresses. Nothing else is projected, so the
/// corridor is one tile wide and no diagonal skirts the dressed tile.
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

/// Two lanes to one Seat, so a route can be read off the weights alone.
/// The source at (9,9) sits in wall with (10,10) its only Seat; the creep
/// "w" stands at (10,12). One swamp tile at (10,11) joins the two in two
/// steps; a five-tile paved ring — (11,13), (12,12), (12,11), (12,10),
/// (11,9) — joins them in six. Nothing else is projected, so no diagonal
/// links the lanes and the only choice is which one to take.
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

/// A paved lane running east from the source's only Seat: source at
/// (10,10) in wall, Seat at (11,10), road tiles from there out to
/// (19,10). A creep at (19,10) is eight road steps from the Seat —
/// #79's corridor.
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

/// A varied room for the walk's floor property: 15 × 15 of mixed terrain
/// with scattered single walls — spaced four apart, so no two touch and
/// the passable tiles stay one connected component, which makes every
/// Work Area tile reachable from every creep. Sources sit at four corners
/// of the interior.
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

/// The store-ring room (#268): the source at (10,10) walled in with its eight
/// neighbours open, the source container "can-a" standing on the Seat (11,10),
/// a corridor running east along y = 10 with the [[storage]] "sto-1" standing
/// in it at (20,10) and the spawn at (30,10). Both of those are obstacles, as
/// the engine has them, so neither is a tile anything stands on and each
/// contributes its two corridor neighbours and nothing else. No controller: the
/// working ground here is the Seats alone, which keeps the two sets far enough
/// apart to be read off each other.
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

/// The same room seen by a colony whose spawn is a [[refill cluster]] member:
/// the cluster is read off the view's Refillables (ADR 0054), so the spawn's
/// ring is in the idle ground only because this view says the spawn is one of
/// the structures the colony fills.
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

/// A projection carrying border rings under room names — the Seam query's
/// whole input, and nothing else, so a test that names three exit tiles
/// documents the rule the way `spatial`'s three ground tiles do. A tile a
/// ring leaves out is impassable, exactly as a tile missing from the
/// ground is.
let bordered rings =
    { SpatialInfo.empty with
        Borders = rings |> List.map (fun (room, tiles) -> room, Map.ofList tiles) |> Map.ofList
    }

/// A projection carrying one room's ground and any number of rooms' border
/// rings — the whole input a walk out to a Seam reads. The ground is
/// W12S28's, because the walk runs inside one room and stops at its
/// border; the far room needs a ring and nothing else, exactly as `seams`
/// needs of it.
let internal seamGround ground rings =
    { SpatialInfo.empty with
        RoomName = Some "W12S28"
        Rooms =
            Map.ofList
                [
                    "W12S28",
                    { RoomLayer.empty with
                        Terrain = Map.ofList ground
                    }
                ]
        Borders = rings |> List.map (fun (room, tiles) -> room, Map.ofList tiles) |> Map.ofList
    }
    |> snapshotWith []
    |> ofView

/// A plain three-tile column running up to the room's north exit, and the
/// exit's plain landing across it — the smallest room that has a walk out
/// to a Seam at all.
let internal toNorthExit =
    [ { X = 10; Y = 1 }, Plain; { X = 10; Y = 2 }, Plain; { X = 10; Y = 3 }, Plain ]

let internal northExit terrain =
    [
        "W12S28", [ { X = 10; Y = 0 }, terrain ]
        "W12S27", [ { X = 10; Y = 49 }, Plain ]
    ]

/// A straight line of Plain ground, for geometry that has to differ
/// between two rooms in a way a reader can count.
let internal plainLine tiles =
    tiles |> List.map (fun tile -> tile, Plain)

/// The two joins ADR 0048 added, in the geometry that tells them apart: a
/// room whose source sits at (10,10) and whose controller is four tiles
/// south, so the Seats on the source's south rank are Dual Seats and the
/// rest are ordinary, and an outpost carrying the very same coordinates
/// beside it (ADR 0041, ADR 0042).
let internal pinnedLayer =
    { RoomLayer.empty with
        Terrain =
            Map.ofList
                [
                    for x in 8..12 do
                        for y in 8..15 do
                            // (11,10) is a Seat by range and no Seat by
                            // ground: the projection carries no terrain for
                            // it at all.
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

/// A projection carrying the colony's own room and one outpost across its
/// north border, rings and all. W1N1 is world (-2,-2) and W1N2 (-2,-3), so
/// stepping onto y=0 at home lands the creep on y=49 there — the pairing
/// `seams` answers and the join `pricedAcross` sums over. The rings ride
/// beside the ground and never inside it (ADR 0041), which is what makes a
/// crossing priceable without any exit tile becoming a tile to stand on.
///
/// The ColonyView is handed out beside the Atlas because one case below
/// prices a lead over a walk table it supplies itself
/// (`ofViewRecalling`, ADR 0032); every other case wants the Atlas and
/// takes the shorthand.
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

/// The colony's own room as every cross-room case below shapes it: one
/// plain corridor down column 25 to the exit row, with the creeps standing
/// in it.
let internal corridorHome creeps =
    { RoomLayer.empty with
        Terrain = Map.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
        CreepPositions = Map.ofList creeps
    }

/// The outpost as the worked example shapes it: the same corridor, with a
/// source standing on ground the projection does not carry, so its Work
/// Area is the one tile below it and the arithmetic has one route to count.
let internal corridorOutpost =
    { RoomLayer.empty with
        Terrain =
            Map.ofList (
                plainLine
                    [
                        for y in 1..48 do
                            if y <> 40 then
                                { X = 25; Y = y }
                    ]
            )
        TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
    }

/// The two rooms the narrowing's own room rule is read in: the cross-room
/// corridor, with a rock at (25,20) of the colony's own room and a rock at
/// (25,40) of the outpost — neither with a container on it — and a
/// Work-heavy body and a light one standing in each room.
///
/// Both rooms, because the room is the whole of what the rule turns on and
/// one room cannot show it. Creeps on both sides, because the two halves
/// are read at different queries: `workAreaFor` answers a creep only in
/// its target's own room (ADR 0041), so the tiles are read from inside
/// each room, and the price is read from the home pair, which is where a
/// fresh Anchor really stands when the Matcher asks.
///
/// The caller places what stands in the outpost, so the same geometry
/// serves the unposted case and the posted one and nothing but the
/// container moves between them.
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

/// The Seats of the outpost's rock, which sits on ground the projection
/// does not carry: the corridor tile above it and the one below.
let internal outpostSeats = Set.ofList [ { X = 25; Y = 39 }; { X = 25; Y = 41 } ]

/// The spawn structure of every lead below, standing at (25,10) of the home
/// corridor. An obstacle, as a spawn is, so a finished body is born on
/// (25,9) or (25,11) and the walk it is led by starts there.
let internal leadSpawn = { X = 25; Y = 10 }

/// The tile in the outpost a lead below is priced to: the Seat under that
/// room's rock, which is where an outpost's Anchor garrisons (ADR 0042).
let internal outpostSeat = { X = 25; Y = 41 }

/// The two rooms every lead below is priced over: the cross-room fixture's
/// own corridors, with the spawn structure standing in the home one and the
/// rings the caller shapes. The ColonyView, because one case hands its own
/// walk table in (ADR 0032).
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

/// The hauler the haul below is priced for, and the body the two creeps
/// standing on its container are cast from: `fatigueFactorOf` reads a
/// living creep's load, so a full one carries the round trip's loaded
/// factor and an empty one its empty factor, exactly — which is what lets
/// the quota's legs be pinned against the Matcher's own walk.
let internal haulerBody = [ Carry; Carry; Move ]

/// The haul the outpost's container makes: the container standing at
/// (25,41) of the outpost's corridor, the spawn structure eleven tiles
/// down the home room's at (25,10). The spawn is an obstacle, so the only
/// tile a transfer reaches it from on the side the haul arrives on is
/// (25,9); the tile behind it is ground the corridor never opens onto from
/// the north. The two haulers stand on the container, one full and one
/// empty; standing creeps price nothing here, the round trip and the walk
/// alike being traffic-blind (ADR 0029). The rings are the caller's, so a
/// case can wall a crossing off or lay swamp on it.
let internal haulAcross homeRing outpostRing =
    northOf
        { RoomLayer.empty with
            Terrain = Map.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
            TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
            Obstacles = Set.singleton { X = 25; Y = 10 }
        }
        homeRing
        { RoomLayer.empty with
            Terrain = Map.ofList (plainLine [ for y in 41..48 -> { X = 25; Y = y } ])
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

/// The one haul every case below prices: the outpost's container to the
/// home room's spawn, each end carrying the room it is a tile of (ADR
/// 0052 decision 2).
let internal roundTripOf atlas =
    haulRoundTripTicks
        atlas
        haulerBody
        (at "W1N2" { X = 25; Y = 41 })
        (at "W1N1" { X = 25; Y = 10 })

/// A projection carrying three rooms in a north-south line, rings and all:
/// W1N1 (the colony's own) at world (-2,-2), W1N2 at (-2,-3) and W1N3 at
/// (-2,-4), so the chain runs north a room at a time and each crossing pairs
/// y=0 here with y=49 there. The middle room is a **transit** room — it holds
/// terrain and a border ring and nothing else, which is exactly what ADR 0058
/// projects one for.
let internal chainOfThree
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
    |> ofView

/// A projection carrying four rooms in the same north-south line — W1N1 through
/// W1N4 — so a chain three crossings long can be priced at `Tuning.MaxHops`'
/// own budget and not one hop inside it.
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

/// The same plain corridor down column 25 the cross-room cases use, with no
/// creep and no target in it: the shape a room a walk only passes through has.
let internal corridorTransit =
    { RoomLayer.empty with
        Terrain = Map.ofList (plainLine [ for y in 1..48 -> { X = 25; Y = y } ])
    }
