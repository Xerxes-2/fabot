// The tick's geometry, built once from `World`: a walk grid per visible room,
// the room graph between rooms, and the one tick cache there is, the floods
// (`ARCHITECTURE.md` §1). Every path query takes a footprint. Built twice
// from one `World`, an Atlas answers identically: nothing in it is static
// or survives the tick.
namespace Fabot.Core

open System.Collections.Generic
open Fabot.Core.World
open Fabot.Core.Grid

/// How a body walks: the tiles it covers and its pace.
type Mover =
    {
        Footprint: Footprint
        Factor: FatigueFactor
    }

/// Where a walk ends: a footprint within `Range` of any of `Tiles` (tile
/// indices) in `Room`. A walk from another room follows the room graph,
/// around `Avoid` (never its two ends) and within `MaxHops` crossings.
type Target =
    {
        Room: string
        Tiles: int[]
        Range: int
        Avoid: Set<string>
        MaxHops: int
    }

/// The tick cache's reads so far: a hit is a flood answered from the table, a
/// miss a flood run.
type Counts = { Hits: int; Misses: int }

/// One visible room's ground this tick: a fatigue weight per tile (-1 where
/// nothing may stand) and whether a creep stands there. A creep is no wall: who yields is `Move`'s call.
[<NoEquality; NoComparison>]
type internal RoomGrid = { Weights: int[]; Occupied: bool[] }

/// A room priced for one mover toward one target: the ticks from each tile.
[<NoEquality; NoComparison>]
type Field =
    private
        {
            Footprint: Footprint
            Weights: int[]
            Steps: int[]
            Dist: int[]
        }

/// This tick's grids and flood table; built by `Atlas.ofWorld`, dropped with
/// the tick.
[<NoEquality; NoComparison>]
type Atlas =
    private
        {
            Rooms: Dictionary<string, RoomGrid>
            Floods: Dictionary<string, int[]>
            mutable Hits: int
            mutable Misses: int
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Field =
    // What a step onto `next` and the walk on from it cost, `unreached` when it
    // leads nowhere: blocked, unreached, or a ring tile short of the goal.
    let private via (field: Field) (next: int) =
        let weight = weightOf field.Footprint field.Weights next

        if weight < 0 then
            unreached
        else
            let d = at next field.Dist

            if d = unreached || (d > 0 && onEdge field.Footprint next) then
                unreached
            else
                d + at weight field.Steps

    // The cheapest next tile and the ticks through it; -1 for none.
    let private best (field: Field) (tile: int) : struct (int * int) =
        let mutable next = -1
        let mutable ticks = unreached

        for direction in 1..8 do
            let n = Tiles.neighbour tile direction

            if n >= 0 then
                let t = via field n

                if t < ticks then
                    next <- n
                    ticks <- t

        struct (next, ticks)

    /// The ticks from a tile to the nearest goal; None where none is reached.
    /// A tile the footprint cannot stand on is priced by its cheapest step off.
    let ticks (field: Field) (tile: int) : int option =
        let d =
            if weightOf field.Footprint field.Weights tile >= 0 then
                at tile field.Dist
            else
                let struct (_, t) = best field tile
                t

        if d = unreached then None else Some d

    /// The direction of a cheapest step from a tile toward the goals, ties to
    /// the lowest direction; None at a goal or where none is reached.
    let step (field: Field) (tile: int) : int option =
        match ticks field tile with
        | Some d when d > 0 ->
            let struct (next, _) = best field tile
            if next < 0 then None else Some(Tiles.direction tile next)
        | _ -> None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Atlas =
    let private beside (range: int) (a: Pos) (b: Pos) =
        a.Room = b.Room && max (abs (a.X - b.X)) (abs (a.Y - b.Y)) <= range

    /// Whether a tile is a room's controller buffer: in its controller's
    /// upgrade range, with no source beside it.
    let isBuffer (room: Room) (pos: Pos) : bool =
        room.Controller |> Option.exists (fun c -> beside Engine.upgradeRange c.Pos pos)
        && not (room.Sources |> Array.exists (fun s -> beside 1 s.Pos pos))

    /// The tiles a working body holds for its life, read off the structures
    /// alone: a container or a container site of ours beside a source (its
    /// harvester's post), and the controller's buffer.
    let workSeats (room: Room) : int[] =
        let posts =
            Array.append
                (room.Structures
                 |> Array.filter (fun s -> s.Kind = Kind.Container)
                 |> Array.map (fun s -> s.Pos))
                (room.Sites
                 |> Array.filter (fun s -> s.Kind = Kind.Container && s.Owner = Ours)
                 |> Array.map (fun s -> s.Pos))

        posts
        |> Array.filter (fun p ->
            isBuffer room p || room.Sources |> Array.exists (fun s -> beside 1 s.Pos p))
        |> Array.map Tiles.ofPos

    /// The walk grid and occupancy of every visible room. A road makes its
    /// tile 1 (a tunnel too); a structure no creep of ours may stand on, of any
    /// owner, a site of ours that the engine treats as an obstacle, and a
    /// source, mineral or controller make it -1. A room whose terrain buffer is
    /// not a whole room has no grid: routed by name, never walked.
    let ofWorld (world: World) : Atlas =
        let rooms = Dictionary<string, RoomGrid>()

        for room in world.Rooms |> Array.filter (fun r -> r.Terrain.Length = Tiles.count) do
            let weights = Array.zeroCreate Tiles.count
            let occupied = Array.zeroCreate Tiles.count
            let terrain = room.Terrain

            for i in 0 .. Tiles.count - 1 do
                let t = terrain.[i]

                weights.[i] <-
                    if t &&& Terrain.wall <> 0uy then -1
                    elif t &&& Terrain.swamp <> 0uy then Engine.swampWeight
                    else Engine.plainWeight

            for s in room.Structures do
                match s.Kind with
                | Kind.Road -> weights.[Tiles.ofPos s.Pos] <- Engine.roadWeight
                | _ -> ()

            for tile in workSeats room do
                weights.[tile] <- seatWeight

            for s in room.Structures do
                if not s.Walkable then
                    weights.[Tiles.ofPos s.Pos] <- -1

            for site in room.Sites do
                match site.Owner with
                | Ours when not (Kind.walkable site.Kind) -> weights.[Tiles.ofPos site.Pos] <- -1
                | _ -> ()

            for source in room.Sources do
                weights.[Tiles.ofPos source.Pos] <- -1

            for mineral in room.Minerals do
                weights.[Tiles.ofPos mineral.Pos] <- -1

            room.Controller |> Option.iter (fun c -> weights.[Tiles.ofPos c.Pos] <- -1)

            for creep in room.Hostiles do
                occupied.[Tiles.ofPos creep.Pos] <- true

            rooms.[room.Name] <-
                {
                    Weights = weights
                    Occupied = occupied
                }

        for creep in world.Creeps do
            match rooms.TryGetValue creep.Pos.Room with
            | true, grid -> grid.Occupied.[Tiles.ofPos creep.Pos] <- true
            | _ -> ()

        {
            Rooms = rooms
            Floods = Dictionary()
            Hits = 0
            Misses = 0
        }

    let counts (atlas: Atlas) : Counts =
        {
            Hits = atlas.Hits
            Misses = atlas.Misses
        }

    /// Whether a creep stands on a tile this tick; false in a room not visible.
    let occupied (atlas: Atlas) (pos: Pos) : bool =
        match atlas.Rooms.TryGetValue pos.Room with
        | true, grid -> grid.Occupied.[Tiles.ofPos pos]
        | _ -> false

    /// Whether a body of one tile may stand on a tile of a room this tick, a
    /// creep's tile included; false in a room not visible.
    let passable (atlas: Atlas) (room: string) (tile: int) : bool =
        match atlas.Rooms.TryGetValue room with
        | true, grid -> tile >= 0 && tile < Tiles.count && grid.Weights.[tile] >= 0
        | _ -> false

    // A tile a footprint may stand on and stay: passable, off the ring.
    let private seat (footprint: Footprint) (weights: int[]) (tile: int) =
        weightOf footprint weights tile >= 0 && not (onEdge footprint tile)

    // Chebyshev range from what a footprint anchored at `anchor` covers to `tile`.
    let private reach (footprint: Footprint) (anchor: int) (tile: int) =
        match footprint with
        | One -> Tiles.range anchor tile
        | Quad ->
            let gap a t = max 0 (max (a - t) (t - a - 1))

            max (gap (Tiles.xOf anchor) (Tiles.xOf tile)) (gap (Tiles.yOf anchor) (Tiles.yOf tile))

    let private seatsNear
        (footprint: Footprint)
        (weights: int[])
        (range: int)
        (tile: int)
        (into: ResizeArray<int>)
        =
        let spread =
            match footprint with
            | One -> range
            | Quad -> range + 1

        for anchor in Tiles.within spread tile do
            if reach footprint anchor tile <= range && seat footprint weights anchor then
                into.Add anchor

    /// The tiles a footprint may stand on with something it covers within
    /// `range` of `target`, in tile order: passable, off the border ring.
    let seats (atlas: Atlas) (footprint: Footprint) (range: int) (target: Pos) : Pos list =
        match atlas.Rooms.TryGetValue target.Room with
        | true, grid ->
            let found = ResizeArray()
            seatsNear footprint grid.Weights range (Tiles.ofPos target) found
            [ for tile in found -> Tiles.toPos target.Room tile ]
        | _ -> []

    // Whether a crossing joins two rooms that share a border: some exit tile
    // and its landing both passable, a room not visible counted open.
    let private linked (atlas: Atlas) (from: string) (onto: string) (exit: int) =
        let near =
            match atlas.Rooms.TryGetValue from with
            | true, grid -> Some grid.Weights
            | _ -> None

        let far =
            match atlas.Rooms.TryGetValue onto with
            | true, grid -> Some grid.Weights
            | _ -> None

        Tiles.exitTiles exit
        |> Array.exists (fun tile ->
            (match near with
             | Some weights -> weights.[tile] >= 0
             | None -> true)
            && (match far with
                | Some weights -> weights.[Tiles.landing tile] >= 0
                | None -> true))

    /// The rooms a walk crosses from one room to another, both ends included,
    /// by fewest crossings: a breadth-first search over the room graph within
    /// `maxHops`, around `avoid` (the ends excepted), exits tried top, right,
    /// bottom, left. None when no such chain exists.
    let route
        (atlas: Atlas)
        (avoid: Set<string>)
        (maxHops: int)
        (from: string)
        (onto: string)
        : string list option =
        if from = onto then
            Some [ from ]
        elif (Tiles.roomCoords from).IsNone || (Tiles.roomCoords onto).IsNone then
            None
        else
            let parent = Dictionary<string, string>()
            parent.[from] <- from
            let mutable frontier = ResizeArray [ from ]
            let mutable hops = 0

            while frontier.Count > 0 && hops < maxHops && not (parent.ContainsKey onto) do
                let next = ResizeArray()

                for room in frontier do
                    for exit in [ Tiles.top; Tiles.right; Tiles.bottom; Tiles.left ] do
                        match Tiles.roomToward room exit with
                        | Some there when
                            not (parent.ContainsKey there)
                            && (there = onto || not (Set.contains there avoid))
                            && linked atlas room there exit
                            ->
                            parent.[there] <- room
                            next.Add there
                        | _ -> ()

                frontier <- next
                hops <- hops + 1

            if parent.ContainsKey onto then
                let mutable chain = [ onto ]

                while List.head chain <> from do
                    chain <- parent.[List.head chain] :: chain

                Some chain
            else
                None

    // The anchors a footprint leaves a room by through one exit edge.
    let private exitGoals (footprint: Footprint) (weights: int[]) (exit: int) : int[] =
        match footprint with
        | One -> Tiles.exitTiles exit |> Array.filter (fun tile -> weights.[tile] >= 0)
        | Quad ->
            let inner = Tiles.side - 2

            [|
                for i in 1 .. inner - 1 do
                    let anchor =
                        match exit with
                        | 1 -> Tiles.index i 0
                        | 3 -> Tiles.index inner i
                        | 5 -> Tiles.index i inner
                        | _ -> Tiles.index 0 i

                    if weightOf Quad weights anchor >= 0 then
                        yield anchor
            |]

    /// A room priced for a mover toward a target: the goals are the seats
    /// within range of the target's tiles when the room is the target's, else
    /// the exit toward the next room of `route`. Flooded once a tick per room,
    /// goals, footprint and pace; None for a room not visible, a body that
    /// never steps, or no route.
    let field (atlas: Atlas) (mover: Mover) (target: Target) (room: string) : Field option =
        match atlas.Rooms.TryGetValue room with
        | true, grid when mover.Factor.Moves > 0 ->
            let footprint = mover.Footprint

            let goals =
                if room = target.Room then
                    let tiles = Array.sort (Array.distinct target.Tiles)

                    let build () =
                        let found = ResizeArray()

                        for tile in tiles do
                            if tile >= 0 && tile < Tiles.count then
                                seatsNear footprint grid.Weights target.Range tile found

                        found.ToArray()

                    Some("r" + string target.Range + ":" + System.String.Join(",", tiles), build)
                else
                    match route atlas target.Avoid target.MaxHops room target.Room with
                    | Some(_ :: next :: _) ->
                        Tiles.exitToward room next
                        |> Option.map (fun exit ->
                            "x" + string exit, (fun () -> exitGoals footprint grid.Weights exit))
                    | _ -> None

            match goals with
            | None -> None
            | Some(goalKey, build) ->
                let steps = stepTable mover.Factor

                let key =
                    System.String.Join(
                        "|",
                        room,
                        (match footprint with
                         | One -> "1"
                         | Quad -> "4"),
                        string steps.[Engine.roadWeight],
                        string steps.[Engine.plainWeight],
                        string steps.[Engine.swampWeight],
                        string steps.[seatWeight],
                        goalKey
                    )

                let dist =
                    match atlas.Floods.TryGetValue key with
                    | true, dist ->
                        atlas.Hits <- atlas.Hits + 1
                        dist
                    | _ ->
                        atlas.Misses <- atlas.Misses + 1
                        let dist = flood footprint grid.Weights steps (build ())
                        atlas.Floods.[key] <- dist
                        dist

                Some
                    {
                        Footprint = footprint
                        Weights = grid.Weights
                        Steps = steps
                        Dist = dist
                    }
        | _ -> None

    /// Ticks from a tile to the target (in another room: to leaving this one
    /// toward it); None where it is not reached.
    let travelTicks (atlas: Atlas) (mover: Mover) (target: Target) (from: Pos) : int option =
        field atlas mover target from.Room
        |> Option.bind (fun field -> Field.ticks field (Tiles.ofPos from))

    /// The direction of a cheapest step from a tile toward the target; None at
    /// the target or where it is not reached.
    let firstStep (atlas: Atlas) (mover: Mover) (target: Target) (from: Pos) : int option =
        field atlas mover target from.Room
        |> Option.bind (fun field -> Field.step field (Tiles.ofPos from))
