// The floods are the one tick cache (`ARCHITECTURE.md` §1). Built twice from
// one `World`, an Atlas answers identically: nothing in it survives the tick.
namespace Fabot.Core

open System.Collections.Generic
open Fabot.Core.World
open Fabot.Core.Grid

type Mover =
    {
        Footprint: Footprint
        Factor: FatigueFactor
    }

/// `Tiles` are tile indices; `Avoid` never holds a walk back from its ends.
type Target =
    {
        Room: string
        Tiles: int[]
        Range: int
        Avoid: Set<string>
        MaxHops: int
    }

type Counts = { Hits: int; Misses: int }

/// Our creeps are no wall (who yields is `Move`'s call); a creep not ours
/// yields to nothing we decide, so nothing may stand on its tile this tick.
[<NoEquality; NoComparison>]
type internal RoomGrid = { Weights: int[] }

[<NoEquality; NoComparison>]
type Field =
    private
        {
            Footprint: Footprint
            Weights: int[]
            Steps: int[]
            Dist: int[]
        }

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

    let ticks (field: Field) (tile: int) : int option =
        let d =
            if weightOf field.Footprint field.Weights tile >= 0 then
                at tile field.Dist
            else
                let struct (_, t) = best field tile
                t

        if d = unreached then None else Some d

    let step (field: Field) (tile: int) : int option =
        match ticks field tile with
        | Some d when d > 0 ->
            let struct (next, _) = best field tile
            if next < 0 then None else Some(Tiles.direction tile next)
        | _ -> None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Atlas =
    let isBuffer (room: Room) (pos: Pos) : bool =
        room.Controller
        |> Option.exists (fun c -> Pos.within Engine.upgradeRange c.Pos pos)
        && not (room.Sources |> Array.exists (fun s -> Pos.within 1 s.Pos pos))

    /// Within 2: beside the source's container.
    let isSourceLink (room: Room) (pos: Pos) : bool =
        room.Sources |> Array.exists (fun s -> Pos.within 2 s.Pos pos)

    let controllerLink (room: Room) : Structure option =
        room.Controller
        |> Option.bind (fun c ->
            room.Structures
            |> Array.filter (fun s ->
                s.Kind = Kind.Link
                && s.Owner = Ours
                && Pos.within Engine.upgradeRange c.Pos s.Pos
                && not (isSourceLink room s.Pos))
            |> Array.sortBy (fun s -> Pos.range c.Pos s.Pos, Tiles.ofPos s.Pos)
            |> Array.tryHead)

    let buffer (room: Room) : Structure option =
        controllerLink room
        |> Option.orElseWith (fun () ->
            room.Structures
            |> Array.tryFind (fun s -> s.Kind = Kind.Container && isBuffer room s.Pos))

    /// Farthest from the controller first, so a controller container on a seat
    /// is no post.
    let postOf (room: Room) (source: Pos) : (Pos * string option) option =
        let fromController (p: Pos) =
            room.Controller
            |> Option.bind (fun c -> Pos.range c.Pos p)
            |> Option.defaultValue 0

        Array.append
            (room.Structures
             |> Array.filter (fun s -> s.Kind = Kind.Container)
             |> Array.map (fun s -> s.Pos, Some s.Id))
            (room.Sites
             |> Array.filter (fun s -> s.Kind = Kind.Container && s.Owner = Ours)
             |> Array.map (fun s -> s.Pos, None))
        |> Array.filter (fun (p, _) -> Pos.within 1 source p && p <> source)
        |> Array.sortBy (fun (p, built) ->
            -(fromController p), Option.isNone built, p.Y * Tiles.side + p.X)
        |> Array.tryHead

    /// A link buffer's upgrader tiles stay open: held, they priced every walk
    /// past the controller dearer.
    let workSeats (room: Room) : int[] =
        Array.append
            (room.Sources |> Array.choose (fun s -> postOf room s.Pos |> Option.map fst))
            (buffer room
             |> Option.filter (fun b -> b.Kind = Kind.Container)
             |> Option.map (fun b -> b.Pos)
             |> Option.toArray)
        |> Array.map Tiles.ofPos

    /// A room whose terrain buffer is not a whole room has no grid: routed by
    /// name, never walked.
    let ofWorld (world: World) : Atlas =
        let rooms = Dictionary<string, RoomGrid>()

        for room in world.Rooms |> Array.filter (fun r -> r.Terrain.Length = Tiles.count) do
            let weights = Array.zeroCreate Tiles.count
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
                weights.[Tiles.ofPos creep.Pos] <- -1

            rooms.[room.Name] <- { Weights = weights }

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

    let passable (atlas: Atlas) (room: string) (tile: int) : bool =
        match atlas.Rooms.TryGetValue room with
        | true, grid -> tile >= 0 && tile < Tiles.count && grid.Weights.[tile] >= 0
        | _ -> false

    let private seat (footprint: Footprint) (weights: int[]) (tile: int) =
        weightOf footprint weights tile >= 0 && not (onEdge footprint tile)

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

    let seats (atlas: Atlas) (footprint: Footprint) (range: int) (target: Pos) : Pos list =
        match atlas.Rooms.TryGetValue target.Room with
        | true, grid ->
            let found = ResizeArray()
            seatsNear footprint grid.Weights range (Tiles.ofPos target) found
            [ for tile in found -> Tiles.toPos target.Room tile ]
        | _ -> []

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

    /// In another room: the ticks to leaving this one toward it.
    let travelTicks (atlas: Atlas) (mover: Mover) (target: Target) (from: Pos) : int option =
        field atlas mover target from.Room
        |> Option.bind (fun field -> Field.ticks field (Tiles.ofPos from))

    let firstStep (atlas: Atlas) (mover: Mover) (target: Target) (from: Pos) : int option =
        field atlas mover target from.Room
        |> Option.bind (fun field -> Field.step field (Tiles.ofPos from))
