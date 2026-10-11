/// One room's fixed ground as the Layout plans it: terrain and the objects
/// that never move (sources, minerals, the controller, a structure that is
/// not ours), the tiles a structure may be planned on, and the anchor the
/// plan is drawn around. Built from the room alone, so a plan drawn on it
/// moves only when the ground does.
module Fabot.Core.Layout.Ground

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid

/// A room's ground for planning.
[<NoEquality; NoComparison>]
type Ground =
    {
        Room: string
        /// Per tile: -1 for a wall or a fixed obstacle (a source, a mineral,
        /// the controller, a structure not ours that no creep crosses), else
        /// the terrain's fatigue weight (plain or swamp). Our own structures
        /// are not read: the plan is drawn on the ground, never on itself.
        Weights: int[]
        /// Per tile: whether a structure may be planned here. Passable, off
        /// the border ring and the ring inside it (the engine refuses most
        /// structures beside an exit), and off the working ground: a seat of
        /// a source or a mineral, and the controller's near tiles.
        Free: bool[]
        /// Tiles, ascending.
        Sources: int[]
        Minerals: int[]
        /// The controller's tile; -1 for none.
        Controller: int
    }

/// The controller's near tiles kept off the plan's slots: upgraders stand
/// within range 3 of it, and this leaves them the two rings inside that.
let private controllerKeep = 2

/// The ring inside the border where the engine refuses a structure beside an
/// exit; roads and containers are allowed there but nothing here plans one.
let private exitKeep = 1

/// The tiles of the structures not ours that no creep crosses, ascending:
/// the only part of a room's ground that can change.
let obstacles (room: Room) : int[] =
    room.Structures
    |> Array.choose (fun s ->
        match s.Owner with
        | Ours -> None
        | _ when s.Walkable -> None
        | _ -> Some(Tiles.ofPos s.Pos))
    |> Array.sort

/// The ground of one room. None for a room whose terrain is not a whole room.
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

        let controller =
            match room.Controller with
            | Some c -> Tiles.ofPos c.Pos
            | None -> -1

        for t in sources do
            setAt t weights -1

        for t in minerals do
            setAt t weights -1

        if controller >= 0 then
            setAt controller weights -1

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

        if controller >= 0 then
            for t in Tiles.within controllerKeep controller do
                setAt t free false

        Some
            {
                Room = room.Name
                Weights = weights
                Free = free
                Sources = sources
                Minerals = minerals
                Controller = controller
            }

/// The square an anchor is judged by: its free tiles within this range hold
/// the plan's core of hub, spawns, towers and near extensions. A design
/// constant of the lattice (`Plan`), not an engine fact.
let coreRadius = 4

/// The spawn slots relative to an anchor, in the order they are taken: the
/// centres of the four lattice cells beside it (`Plan`).
let spawnOffsets = [| 2, 0; -2, 0; 0, 2; 0, -2 |]

/// The spawn slots of an anchor, as tiles; -1 for one off the room.
let spawnSlots (anchor: int) : int[] =
    spawnOffsets
    |> Array.map (fun (dx, dy) ->
        let x = Tiles.xOf anchor + dx
        let y = Tiles.yOf anchor + dy
        if Tiles.inRoom x y then Tiles.index x y else -1)

// Summed range from a tile to every source and the controller: the walk the
// colony pays from its core, by the cheapest measure there is.
let private reachSum (ground: Ground) (tile: int) =
    let mutable sum = 0

    for s in ground.Sources do
        sum <- sum + Tiles.range tile s

    if ground.Controller >= 0 then
        sum <- sum + Tiles.range tile ground.Controller

    sum

// Each tile's free tiles within `coreRadius`, by a summed-area table.
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

// An anchor may stand where it is passable, two tiles inside the border, with
// a free spawn slot.
let private valid (ground: Ground) (anchor: int) =
    let x = Tiles.xOf anchor
    let y = Tiles.yOf anchor

    x >= 2
    && y >= 2
    && x < Tiles.side - 2
    && y < Tiles.side - 2
    && at anchor ground.Weights >= 0
    && spawnSlots anchor |> Array.exists (fun s -> s >= 0 && at s ground.Free)

/// The valid anchors, best first: the most of our standing spawns on its
/// spawn slots, then the most free tiles within `coreRadius`, then the least
/// summed range to the sources and the controller, then the lowest tile.
/// With spawns standing only an anchor with one on its slots is a candidate;
/// without, every tile is.
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
    |> Array.sortBy (fun a ->
        let held =
            spawnSlots a |> Array.sumBy (fun s -> if Array.contains s spawns then 1 else 0)

        struct (-held, -(at a opens), reachSum ground a, a))
