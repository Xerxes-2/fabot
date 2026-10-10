/// A room's tiles as one integer each, the engine's directions between them,
/// its exit edges, and room names as world coordinates. A tile is `y * 50 + x`,
/// the index of the terrain's raw buffer, so every per-room array in the Atlas
/// reads the terrain's own order.
module Fabot.Core.Tiles

open Fabot.Core.World

/// A room's side in tiles.
let side = Engine.roomSide

/// Tiles in a room.
let count = side * side

/// The tile at `x`, `y`.
let inline index (x: int) (y: int) = y * side + x

let inline xOf (tile: int) = tile % side

let inline yOf (tile: int) = tile / side

/// Whether `x`, `y` is one of a room's own tiles.
let inline inRoom (x: int) (y: int) =
    x >= 0 && x < side && y >= 0 && y < side

/// A position's tile in its own room.
let ofPos (pos: Pos) = index pos.X pos.Y

/// A tile joined to its room.
let toPos (room: string) (tile: int) : Pos =
    {
        Room = room
        X = xOf tile
        Y = yOf tile
    }

/// Chebyshev range between two tiles of one room: the engine's `getRangeTo`.
let range (a: int) (b: int) =
    max (abs (xOf a - xOf b)) (abs (yOf a - yOf b))

/// The engine's directions, TOP = 1 clockwise to TOP_LEFT = 8.
let top = 1
let topRight = 2
let right = 3
let bottomRight = 4
let bottom = 5
let bottomLeft = 6
let left = 7
let topLeft = 8

/// The column a step in `direction` moves by.
let dx (direction: int) =
    match direction with
    | 2
    | 3
    | 4 -> 1
    | 6
    | 7
    | 8 -> -1
    | _ -> 0

/// The row a step in `direction` moves by; rows grow downward.
let dy (direction: int) =
    match direction with
    | 8
    | 1
    | 2 -> -1
    | 4
    | 5
    | 6 -> 1
    | _ -> 0

/// The tile one step from `tile` in `direction`, or -1 off the room.
let neighbour (tile: int) (direction: int) =
    let x = xOf tile + dx direction
    let y = yOf tile + dy direction
    if inRoom x y then index x y else -1

/// The direction from one tile to an adjacent one; 0 when they are not
/// adjacent or are one tile.
let direction (from: int) (onto: int) =
    let ddx = xOf onto - xOf from
    let ddy = yOf onto - yOf from

    match ddx, ddy with
    | 0, -1 -> top
    | 1, -1 -> topRight
    | 1, 0 -> right
    | 1, 1 -> bottomRight
    | 0, 1 -> bottom
    | -1, 1 -> bottomLeft
    | -1, 0 -> left
    | -1, -1 -> topLeft
    | _ -> 0

/// The tiles of a room within `radius` of `tile`, clamped to the room, in
/// tile order.
let within (radius: int) (tile: int) : int[] =
    let x = xOf tile
    let y = yOf tile
    let tiles = ResizeArray()

    for ty in max 0 (y - radius) .. min (side - 1) (y + radius) do
        for tx in max 0 (x - radius) .. min (side - 1) (x + radius) do
            tiles.Add(index tx ty)

    tiles.ToArray()

let private last = side - 1

/// Whether a tile is on the room's border ring, corners included.
let onEdge (tile: int) =
    let x = xOf tile
    let y = yOf tile
    x = 0 || y = 0 || x = last || y = last

/// The exit a tile stands on, as the direction it leaves by (`top`, `right`,
/// `bottom` or `left`, the engine's FIND_EXIT_* values); 0 off the ring and on
/// a corner, which lies on two borders and lands nowhere.
let exitOf (tile: int) =
    let x = xOf tile
    let y = yOf tile
    let onX = x = 0 || x = last
    let onY = y = 0 || y = last

    if onX = onY then 0
    elif y = 0 then top
    elif x = last then right
    elif y = last then bottom
    else left

/// The tiles of one exit edge, corners excluded, in tile order; empty for a
/// direction that names no edge.
let exitTiles (exit: int) : int[] =
    match exit with
    | 1 -> Array.init (side - 2) (fun i -> index (i + 1) 0)
    | 3 -> Array.init (side - 2) (fun i -> index last (i + 1))
    | 5 -> Array.init (side - 2) (fun i -> index (i + 1) last)
    | 7 -> Array.init (side - 2) (fun i -> index 0 (i + 1))
    | _ -> [||]

/// The tile of the next room a body on this exit tile lands on; -1 off an
/// exit.
let landing (tile: int) =
    let x = xOf tile
    let y = yOf tile

    match exitOf tile with
    | 1 -> index x last
    | 3 -> index 0 y
    | 5 -> index x 0
    | 7 -> index last y
    | _ -> -1

/// A room's place on the world grid, read off its name: `W0` is x = -1 and
/// `E0` x = 0, `N0` y = -1 and `S0` y = 0, so `W12S28` is (-13, 28). None for
/// a name outside the engine's grammar.
let roomCoords (name: string) : (int * int) option =
    let digit i =
        i < name.Length && name.[i] >= '0' && name.[i] <= '9'

    let mutable xEnd = 1

    while digit xEnd do
        xEnd <- xEnd + 1

    let mutable yEnd = xEnd + 1

    while digit yEnd do
        yEnd <- yEnd + 1

    let number start stop =
        let mutable value = 0

        for i in start .. stop - 1 do
            value <- value * 10 + (int name.[i] - int '0')

        value

    let axis (letter: char) outward inward value =
        if letter = outward then Some(-value - 1)
        elif letter = inward then Some value
        else None

    if name.Length < 4 || xEnd = 1 || yEnd = xEnd + 1 || yEnd <> name.Length then
        None
    else
        match
            axis name.[0] 'W' 'E' (number 1 xEnd), axis name.[xEnd] 'N' 'S' (number (xEnd + 1) yEnd)
        with
        | Some x, Some y -> Some(x, y)
        | _ -> None

/// The name of the room at world coordinates: `roomCoords` backwards.
let roomName (x: int) (y: int) : string =
    let axis outward inward value =
        if value < 0 then
            outward + string (-value - 1)
        else
            inward + string value

    axis "W" "E" x + axis "N" "S" y

/// The room across an exit of this one; None for a name outside the grammar
/// or a direction that names no exit.
let roomToward (name: string) (exit: int) : string option =
    match exit with
    | 1
    | 3
    | 5
    | 7 ->
        roomCoords name
        |> Option.map (fun (x, y) -> roomName (x + dx exit) (y + dy exit))
    | _ -> None

/// The exit of `from` that leads into `onto`; None unless the two share a
/// border.
let exitToward (from: string) (onto: string) : int option =
    match roomCoords from, roomCoords onto with
    | Some(fx, fy), Some(tx, ty) ->
        match tx - fx, ty - fy with
        | 0, -1 -> Some top
        | 1, 0 -> Some right
        | 0, 1 -> Some bottom
        | -1, 0 -> Some left
        | _ -> None
    | _ -> None
