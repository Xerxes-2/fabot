/// Built from the room alone, so a plan drawn on it moves only when the
/// ground does.
module Fabot.Core.Layout.Ground

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid

[<NoEquality; NoComparison>]
type Ground =
    {
        Room: string
        /// -1 where no creep crosses. Our own structures are not read: the
        /// plan is drawn on the ground, never on itself.
        Weights: int[]
        Free: bool[]
        Sources: int[]
        Minerals: int[]
        Controller: int option
    }

/// Upgraders work within range 3 of the controller; this leaves them the two
/// rings inside that.
let private controllerKeep = 2

/// The ring inside the border where the engine refuses a structure beside an
/// exit; roads and containers are allowed there but nothing here plans one.
let private exitKeep = 1

/// The only part of a room's ground that can change.
let obstacles (room: Room) : int[] =
    room.Structures
    |> Array.choose (fun s ->
        match s.Owner with
        | Ours -> None
        | _ when s.Walkable -> None
        | _ -> Some(Tiles.ofPos s.Pos))
    |> Array.sort

let ofRoom (room: Room) : Ground option =
    if room.Terrain.Length <> Tiles.count then
        None
    else
        let weights = Array.zeroCreate Tiles.count

        for i in 0 .. Tiles.count - 1 do
            let t = at i room.Terrain

            setAt
                i
                weights
                (if t &&& Terrain.wall <> 0uy then -1
                 elif t &&& Terrain.swamp <> 0uy then Engine.swampWeight
                 else Engine.plainWeight)

        for t in obstacles room do
            setAt t weights -1

        let sources = room.Sources |> Array.map (fun s -> Tiles.ofPos s.Pos) |> Array.sort
        let minerals = room.Minerals |> Array.map (fun m -> Tiles.ofPos m.Pos) |> Array.sort

        let controller = room.Controller |> Option.map (fun c -> Tiles.ofPos c.Pos)

        for t in sources do
            setAt t weights -1

        for t in minerals do
            setAt t weights -1

        controller |> Option.iter (fun c -> setAt c weights -1)

        let free = Array.zeroCreate Tiles.count
        let edge = exitKeep + 1

        for i in 0 .. Tiles.count - 1 do
            let x = Tiles.xOf i
            let y = Tiles.yOf i

            setAt
                i
                free
                (at i weights >= 0
                 && x >= edge
                 && y >= edge
                 && x < Tiles.side - edge
                 && y < Tiles.side - edge)

        for rock in Array.append sources minerals do
            for t in Tiles.within 1 rock do
                setAt t free false

        controller
        |> Option.iter (fun c ->
            for t in Tiles.within controllerKeep c do
                setAt t free false)

        Some
            {
                Room = room.Name
                Weights = weights
                Free = free
                Sources = sources
                Minerals = minerals
                Controller = controller
            }

/// Holds the plan's core of hub, spawns, towers and near extensions: a design
/// constant of the lattice (`Plan`), not an engine fact.
let coreRadius = 4

/// The centres of the four lattice cells beside an anchor (`Plan`).
let spawnOffsets = [| 2, 0; -2, 0; 0, 2; 0, -2 |]

let spawnSlots (anchor: int) : int[] =
    spawnOffsets
    |> Array.map (fun (dx, dy) ->
        let x = Tiles.xOf anchor + dx
        let y = Tiles.yOf anchor + dy
        if Tiles.inRoom x y then Tiles.index x y else -1)

// Range, not path cost: the cheapest measure of the walk the colony pays
// from its core.
let private reachSum (ground: Ground) (tile: int) =
    let mutable sum = 0

    for s in ground.Sources do
        sum <- sum + Tiles.range tile s

    sum
    + (ground.Controller |> Option.map (Tiles.range tile) |> Option.defaultValue 0)

let private openness (ground: Ground) : int[] =
    let n = Tiles.side + 1
    let sums = Array.zeroCreate (n * n)

    for y in 0 .. Tiles.side - 1 do
        for x in 0 .. Tiles.side - 1 do
            let here = if at (Tiles.index x y) ground.Free then 1 else 0

            setAt
                ((y + 1) * n + x + 1)
                sums
                (here + at (y * n + x + 1) sums + at ((y + 1) * n + x) sums - at (y * n + x) sums)

    Array.init Tiles.count (fun t ->
        let x0 = max 0 (Tiles.xOf t - coreRadius)
        let y0 = max 0 (Tiles.yOf t - coreRadius)
        let x1 = min (Tiles.side - 1) (Tiles.xOf t + coreRadius) + 1
        let y1 = min (Tiles.side - 1) (Tiles.yOf t + coreRadius) + 1

        at (y1 * n + x1) sums - at (y0 * n + x1) sums - at (y1 * n + x0) sums
        + at (y0 * n + x0) sums)

let private valid (ground: Ground) (anchor: int) =
    let x = Tiles.xOf anchor
    let y = Tiles.yOf anchor

    x >= 2
    && y >= 2
    && x < Tiles.side - 2
    && y < Tiles.side - 2
    && at anchor ground.Weights >= 0
    && spawnSlots anchor |> Array.exists (fun s -> s >= 0 && at s ground.Free)

let anchors (ground: Ground) (spawns: int[]) : int[] =
    let opens = openness ground

    let candidates =
        if spawns.Length = 0 then
            [| 0 .. Tiles.count - 1 |]
        else
            [|
                for s in spawns do
                    for dx, dy in spawnOffsets do
                        let x = Tiles.xOf s - dx
                        let y = Tiles.yOf s - dy

                        if Tiles.inRoom x y then
                            yield Tiles.index x y
            |]
            |> Array.distinct

    candidates
    |> Array.filter (valid ground)
    |> Array.sortBy (fun a -> struct (-(at a opens), reachSum ground a, a))
