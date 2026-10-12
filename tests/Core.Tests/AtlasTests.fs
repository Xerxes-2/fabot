/// Invariants of the Atlas over generated rooms, and the tile and pace
/// arithmetic it rests on.
module Fabot.Core.Tests.AtlasTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid

let private roomName = "W1N1"

let private structure (kind: Kind) (walkable: bool) (x: int) (y: int) : Structure =
    {
        Id = $"s{x},{y}"
        Kind = kind
        Pos = { Room = roomName; X = x; Y = y }
        Owner = Nobody
        Hits = 1
        HitsMax = 1
        Store = Store.empty
        Cooldown = 0
        Collapse = 0
        Walkable = walkable
    }

/// One generated room: raw terrain, roads and walls built on it.
type Ground =
    {
        Terrain: byte[]
        Roads: (int * int) list
        Walls: (int * int) list
    }

    /// Whether nothing may stand on a tile: terrain wall without a road, or a
    /// built wall.
    member this.Blocked(x: int, y: int) =
        List.contains (x, y) this.Walls
        || (this.Terrain.[Tiles.index x y] &&& Terrain.wall <> 0uy
            && not (List.contains (x, y) this.Roads))

let private worldOf (ground: Ground) : World =
    let structures =
        [|
            for x, y in ground.Roads -> structure Kind.Road true x y
            for x, y in ground.Walls -> structure Kind.Wall false x y
        |]

    {
        Time = 1
        Me = ""
        CpuLimit = 20.0
        Bucket = 10_000
        Rooms =
            [|
                {
                    Name = roomName
                    Terrain = ground.Terrain
                    Controller = None
                    EnergyAvailable = 0
                    EnergyCapacity = 0
                    Sources = [||]
                    Minerals = [||]
                    Structures = structures
                    Sites = [||]
                    Piles = [||]
                    Remains = [||]
                    Hostiles = [||]
                }
            |]
        Spawns = [||]
        Creeps = [||]
        OurSites = 0
    }

/// A walk to price: the ground, a mover, a goal tile with its range, a start.
type Walk =
    {
        Ground: Ground
        Mover: Mover
        Goal: int
        Range: int
        Start: int
    }

let private tileGen = Gen.choose (0, Tiles.count - 1)

let private coordGen = Gen.choose (0, Tiles.side - 1)

let private groundGen =
    gen {
        // Rooms from open to a quarter wall, so a 2×2 body finds a way often.
        let! walls = Gen.choose (0, 3)

        let! cells =
            Gen.arrayOfLength
                Tiles.count
                (Gen.frequency
                    [
                        7, Gen.constant 0uy
                        2, Gen.constant Terrain.swamp
                        walls, Gen.constant Terrain.wall
                    ])

        let! roads = Gen.listOf (Gen.zip coordGen coordGen)
        let! built = Gen.listOf (Gen.zip coordGen coordGen)

        return
            {
                Terrain = cells
                Roads = roads
                Walls = built
            }
    }

let private bodyGen =
    gen {
        let! n = Gen.choose (1, Engine.maxBodyParts)
        let! parts = Gen.listOfLength n (Gen.elements allBodyParts)
        let! loaded = Gen.elements [ true; false ]
        return if loaded then loadedFactor parts else emptyFactor parts
    }

let private moverGen =
    gen {
        let! factor = bodyGen
        let! footprint = Gen.elements [ One; Quad ]

        return
            {
                Footprint = footprint
                Factor = factor
            }
    }

type Gens =
    static member Walk() =
        gen {
            let! ground = groundGen
            let! mover = moverGen
            let! goal = tileGen
            let! range = Gen.choose (0, 3)
            let! start = tileGen

            return
                {
                    Ground = ground
                    Mover = mover
                    Goal = goal
                    Range = range
                    Start = start
                }
        }
        |> Arb.fromGen

let private config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Gens> ]
    }

let private targetOf (room: string) (walk: Walk) : Target =
    {
        Room = room
        Tiles = [| walk.Goal |]
        Range = walk.Range
        Avoid = Set.empty
        MaxHops = 4
    }

/// Chebyshev range from what a footprint anchored at `anchor` covers to `tile`.
let private reach (footprint: Footprint) (anchor: int) (tile: int) =
    let span =
        match footprint with
        | One -> 0
        | Quad -> 1

    let gap a t = max 0 (max (a - t) (t - a - span))
    max (gap (Tiles.xOf anchor) (Tiles.xOf tile)) (gap (Tiles.yOf anchor) (Tiles.yOf tile))

let private covered (footprint: Footprint) (anchor: int) =
    let x = Tiles.xOf anchor
    let y = Tiles.yOf anchor

    match footprint with
    | One -> [ x, y ]
    | Quad -> [ x, y; x + 1, y; x, y + 1; x + 1, y + 1 ]

/// The walk followed by `firstStep` from the start: each tile and its ticks,
/// until a goal or no step.
let private followed (atlas: Atlas) (walk: Walk) (target: Target) =
    let rec go (tile: int) (seen: int) acc =
        let pos = Tiles.toPos roomName tile

        match Atlas.travelTicks atlas walk.Mover target pos with
        | None -> List.rev acc
        | Some ticks ->
            let acc = (tile, ticks) :: acc

            match Atlas.firstStep atlas walk.Mover target pos with
            | Some direction when seen < 2 * Tiles.count ->
                go (Tiles.neighbour tile direction) (seen + 1) acc
            | _ -> List.rev acc

    go walk.Start 0 []

[<Tests>]
let tests =
    testList
        "Atlas"
        [
            test "a tile's index round-trips through its column and row in terrain order" {
                for tile in 0 .. Tiles.count - 1 do
                    Expect.equal (Tiles.index (Tiles.xOf tile) (Tiles.yOf tile)) tile $"tile {tile}"

                Expect.equal (Tiles.index 3 1) (1 * 50 + 3) "y * 50 + x"
            }

            testProperty "a room name round-trips through world coordinates"
            <| fun (x: int) (y: int) ->
                let x = x % 300
                let y = y % 300
                Tiles.roomCoords (Tiles.roomName x y) = Some(x, y)

            test "names read as the engine spells them" {
                Expect.equal (Tiles.roomCoords "W12S28") (Some(-13, 28)) "W12S28"
                Expect.equal (Tiles.roomCoords "E0N0") (Some(0, -1)) "E0N0"
                Expect.equal (Tiles.roomCoords "sim") None "outside the grammar"
                Expect.equal (Tiles.roomCoords "W1") None "half a name"
            }

            test "a step and the direction back to it agree, and stay within range one" {
                for tile in 0 .. Tiles.count - 1 do
                    for direction in 1..8 do
                        let next = Tiles.neighbour tile direction

                        if next >= 0 then
                            Expect.equal (Tiles.direction tile next) direction "direction"
                            Expect.equal (Tiles.range tile next) 1 "adjacent"
            }

            test "an exit's landing is on the opposite exit, and lands back" {
                for exit in [ Tiles.top; Tiles.right; Tiles.bottom; Tiles.left ] do
                    for tile in Tiles.exitTiles exit do
                        let landing = Tiles.landing tile
                        Expect.equal (Tiles.exitOf tile) exit "on its exit"
                        Expect.equal (Tiles.exitOf landing) ((exit + 3) % 8 + 1) "opposite"
                        Expect.equal (Tiles.landing landing) tile "and back"

                    let room = Tiles.roomToward roomName exit |> Option.get
                    Expect.equal (Tiles.exitToward roomName room) (Some exit) "the room across"
            }

            test "a step takes the fatigue it makes over twice the MOVE, at least one tick" {
                let pace weight moves : FatigueFactor = { Weight = weight; Moves = moves }

                for weight, moves, ground, ticks in
                    [
                        1, 1, Engine.plainWeight, 1
                        1, 1, Engine.swampWeight, 5
                        2, 1, Engine.plainWeight, 2
                        2, 1, Engine.roadWeight, 1
                        49, 1, Engine.swampWeight, 245
                        0, 1, Engine.swampWeight, 1
                        5, 0, Engine.plainWeight, -1
                    ] do
                    Expect.equal
                        (stepTicks (pace weight moves) ground)
                        ticks
                        $"{weight}W {moves}M on {ground}"
            }

            testProperty "a step's ticks are the ceiling of its fatigue over the MOVE's pay"
            <| fun (weight: byte) (moves: byte) ->
                let factor =
                    {
                        Weight = int weight % 50
                        Moves = 1 + int moves % 50
                    }

                [ Engine.roadWeight; Engine.plainWeight; Engine.swampWeight ]
                |> List.forall (fun ground ->
                    let fatigue = ground * factor.Weight

                    stepTicks factor ground = max
                        1
                        ((fatigue + 2 * factor.Moves - 1) / (2 * factor.Moves)))

            testProperty
                "a loaded body weighs at least its empty self, and a creep reads as its parts"
            <| fun (kinds: BodyPart list) ->
                let empty = emptyFactor kinds
                let loaded = loadedFactor kinds

                let creep: Creep =
                    {
                        Id = "c"
                        Name = "c"
                        Owner = Ours
                        Pos = { Room = roomName; X = 1; Y = 1 }
                        Body =
                            [|
                                for kind in kinds ->
                                    {
                                        Kind = kind
                                        Hits = 100
                                        Boost = Unboosted
                                    }
                            |]
                        Hits = 100
                        HitsMax = 100
                        TicksToLive = 1
                        Fatigue = 0
                        Store =
                            {
                                Held = [| Energy, Engine.carryPartCapacity * 50 |]
                                Capacity = 0
                            }
                    }

                loaded.Weight >= empty.Weight
                && loaded.Moves = empty.Moves
                && factorOf { creep with Store = Store.empty } = empty
                && factorOf creep = loaded

            testPropertyWithConfig
                config
                "a walk takes at least a tick a tile of range it must close"
            <| fun (walk: Walk) ->
                let atlas = Atlas.ofWorld (worldOf walk.Ground)
                let target = targetOf roomName walk

                match
                    Atlas.travelTicks atlas walk.Mover target (Tiles.toPos roomName walk.Start)
                with
                | Some ticks ->
                    ticks >= reach walk.Mover.Footprint walk.Start walk.Goal - walk.Range
                | None -> true

            testPropertyWithConfig
                config
                "each first step lands where the walk is shorter, on open ground, to a seat"
            <| fun (walk: Walk) ->
                let atlas = Atlas.ofWorld (worldOf walk.Ground)
                let target = targetOf roomName walk
                let path = followed atlas walk target

                let open' (tile: int) =
                    covered walk.Mover.Footprint tile
                    |> List.forall (fun (x, y) ->
                        Tiles.inRoom x y && not (walk.Ground.Blocked(x, y)))

                let seats =
                    Atlas.seats
                        atlas
                        walk.Mover.Footprint
                        walk.Range
                        (Tiles.toPos roomName walk.Goal)
                    |> List.map Tiles.ofPos

                let rec descending steps =
                    match steps with
                    | (_, a) :: ((tile, b) :: _ as rest) -> b < a && open' tile && descending rest
                    | _ -> true

                descending path
                && (match List.tryLast path with
                    | Some(tile, 0) -> List.contains tile seats
                    | Some _ -> false
                    | None -> true)

            testPropertyWithConfig config "a seat is open ground off the ring, within range"
            <| fun (walk: Walk) ->
                let atlas = Atlas.ofWorld (worldOf walk.Ground)
                let footprint = walk.Mover.Footprint

                Atlas.seats atlas footprint walk.Range (Tiles.toPos roomName walk.Goal)
                |> List.forall (fun pos ->
                    let tile = Tiles.ofPos pos

                    reach footprint tile walk.Goal <= walk.Range
                    && not (Grid.onEdge footprint tile)
                    && covered footprint tile
                       |> List.forall (fun (x, y) -> not (walk.Ground.Blocked(x, y))))

            testPropertyWithConfig
                config
                "no seat is a source's, a mineral's or a controller's tile"
            <| fun (walk: Walk) (x: int) (y: int) ->
                let at =
                    {
                        Room = roomName
                        X = abs x % 48 + 1
                        Y = abs y % 48 + 1
                    }

                let world = worldOf walk.Ground
                let room = world.Rooms.[0]

                let rock =
                    { room with
                        Sources =
                            [|
                                {
                                    Id = "src"
                                    Pos = at
                                    Energy = 0
                                    EnergyCapacity = 0
                                    TicksToRegeneration = 0
                                }
                            |]
                        Minerals =
                            [|
                                {
                                    Id = "min"
                                    Pos = { at with X = 49 - at.X }
                                    Resource = Vocabulary.Energy
                                    Amount = 0
                                }
                            |]
                    }

                let atlas = Atlas.ofWorld { world with Rooms = [| rock |] }

                [ at; { at with X = 49 - at.X } ]
                |> List.forall (fun obstacle ->
                    Atlas.seats atlas One 1 obstacle |> List.contains obstacle |> not)

            testPropertyWithConfig config "a memoised answer is a fresh Atlas's answer"
            <| fun (walk: Walk) (before: Walk) (kept: bool) ->
                let world = worldOf walk.Ground
                let target = targetOf roomName walk
                let start = Tiles.toPos roomName walk.Start
                let stale = Atlas.ofWorld (worldOf (if kept then walk.Ground else before.Ground))
                Atlas.travelTicks stale walk.Mover target start |> ignore
                let warm = Atlas.following stale world
                let first = Atlas.travelTicks warm walk.Mover target start
                let again = Atlas.travelTicks warm walk.Mover target start
                let step = Atlas.firstStep warm walk.Mover target start
                let fresh = Atlas.ofWorld world
                let misses = Atlas.misses warm

                first = again
                && first = Atlas.travelTicks fresh walk.Mover target start
                && step = Atlas.firstStep fresh walk.Mover target start
                && (walk.Mover.Factor.Moves = 0 || misses = (if kept then 0 else 1))

            testPropertyWithConfig config "toward another room, the walk ends on an exit into it"
            <| fun (walk: Walk) (exitPick: byte) ->
                let toward =
                    [| Tiles.top; Tiles.right; Tiles.bottom; Tiles.left |].[int exitPick % 4]

                let onto = Tiles.roomToward roomName toward |> Option.get
                let atlas = Atlas.ofWorld (worldOf walk.Ground)
                let target = targetOf onto walk

                match
                    List.tryLast (followed atlas walk target),
                    Atlas.route atlas Set.empty target.MaxHops roomName onto
                with
                | Some(tile, 0), Some(_ :: next :: _) ->
                    let exit = Tiles.exitToward roomName next |> Option.get

                    let edge =
                        covered walk.Mover.Footprint tile
                        |> List.exists (fun (x, y) -> Tiles.exitOf (Tiles.index x y) = exit)

                    edge
                | Some(_, ticks), _ -> ticks = 0
                | None, _ -> true

            testProperty "a route crosses shared borders, around the avoided, by fewest crossings"
            <| fun (dx: sbyte) (dy: sbyte) (avoided: (sbyte * sbyte) list) ->
                let atlas =
                    Atlas.ofWorld
                        { worldOf
                              {
                                  Terrain = [||]
                                  Roads = []
                                  Walls = []
                              } with
                            Rooms = [||]
                        }

                let dx, dy = int dx % 4, int dy % 4
                let onto = Tiles.roomName dx dy
                let from = Tiles.roomName 0 0

                let avoid =
                    avoided
                    |> List.map (fun (x, y) -> Tiles.roomName (int x % 4) (int y % 4))
                    |> Set.ofList

                let hops = abs dx + abs dy

                let shares (a: string) (b: string) = (Tiles.exitToward a b).IsSome

                match Atlas.route atlas avoid 12 from onto with
                | Some chain ->
                    List.head chain = from
                    && List.last chain = onto
                    && List.pairwise chain |> List.forall (fun (a, b) -> shares a b)
                    && chain
                       |> List.forall (fun room ->
                           room = from || room = onto || not (avoid.Contains room))
                    && List.length chain - 1 >= hops
                    && (Set.isEmpty avoid |> not || List.length chain - 1 = hops)
                | None -> not (Set.isEmpty avoid)
        ]
