/// A tile is `y * 50 + x`, the terrain raw buffer's index, so every per-room
/// array in the Atlas reads the terrain's own order.
module Fabot.Core.Tiles

open Fabot.Core.World

let side = Engine.roomSide

let count = side * side

let inline index (x: int) (y: int) = y * side + x

let inline xOf (tile: int) = tile % side

let inline yOf (tile: int) = tile / side

let inline inRoom (x: int) (y: int) =
    x >= 0 && x < side && y >= 0 && y < side

let ofPos (pos: Pos) = index pos.X pos.Y

let toPos (room: string) (tile: int) : Pos =
    {
        Room = room
        X = xOf tile
        Y = yOf tile
    }

/// The engine's `getRangeTo`.
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

let dx (direction: int) =
    match direction with
    | 2
    | 3
    | 4 -> 1
    | 6
    | 7
    | 8 -> -1
    | _ -> 0

/// Rows grow downward.
let dy (direction: int) =
    match direction with
    | 8
    | 1
    | 2 -> -1
    | 4
    | 5
    | 6 -> 1
    | _ -> 0

let neighbour (tile: int) (direction: int) =
    let x = xOf tile + dx direction
    let y = yOf tile + dy direction
    if inRoom x y then index x y else -1

/// A bare JS index under Fable, whose indexer (behind `for x in array` too)
/// is a bounds-checked call: a per-tile or per-pair loop reads through these.
#if FABLE_COMPILER_JAVASCRIPT
[<Fable.Core.Emit("$1[$0]")>]
#endif
let at (_index: int) (_array: 'T[]) : 'T = _array.[_index]

#if FABLE_COMPILER_JAVASCRIPT
[<Fable.Core.Emit("$1[$0] = $2")>]
#endif
let setAt (_index: int) (_array: 'T[]) (_value: 'T) : unit = _array.[_index] <- _value

let private aroundTable: int[][] =
    Array.init count (fun t ->
        let ns = ResizeArray<int>()

        for d in 1..8 do
            let n = neighbour t d

            if n >= 0 then
                ns.Add n

        ns.ToArray())

/// The in-room neighbours, in direction order.
let around (tile: int) : int[] = at tile aroundTable

/// The neighbour costing least under `bound`, the first in direction order
/// on a tie; -1 when none does.
let inline leastAround (tile: int) (bound: int) ([<InlineIfLambda>] cost: int -> int) : int =
    let ns = around tile
    let mutable best = -1
    let mutable least = bound

    for i in 0 .. ns.Length - 1 do
        let n = at i ns
        let c = cost n

        if c < least then
            best <- n
            least <- c

    best

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

let within (radius: int) (tile: int) : int[] =
    let x = xOf tile
    let y = yOf tile
    let tiles = ResizeArray()

    for ty in max 0 (y - radius) .. min (side - 1) (y + radius) do
        for tx in max 0 (x - radius) .. min (side - 1) (x + radius) do
            tiles.Add(index tx ty)

    tiles.ToArray()

let private last = side - 1

let onEdge (tile: int) =
    let x = xOf tile
    let y = yOf tile
    x = 0 || y = 0 || x = last || y = last

/// The engine's FIND_EXIT_* values; a corner lies on two borders and lands
/// nowhere.
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

let private exitBands =
    Array.init 9 (fun exit ->
        match exit with
        | 1 -> Array.init (side - 2) (fun i -> index (i + 1) 0)
        | 3 -> Array.init (side - 2) (fun i -> index last (i + 1))
        | 5 -> Array.init (side - 2) (fun i -> index (i + 1) last)
        | 7 -> Array.init (side - 2) (fun i -> index 0 (i + 1))
        | _ -> [||])

/// Shared: never written.
let exitTiles (exit: int) : int[] =
    if exit >= 0 && exit < exitBands.Length then
        exitBands.[exit]
    else
        [||]

let landing (tile: int) =
    let x = xOf tile
    let y = yOf tile

    match exitOf tile with
    | 1 -> index x last
    | 3 -> index 0 y
    | 5 -> index x 0
    | 7 -> index last y
    | _ -> -1

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

/// Across rooms, where adjacent rooms' edge tiles are one tile.
let seamRange (a: Pos) (b: Pos) : int option =
    if a.Room = b.Room then
        Pos.range a b
    else
        match roomCoords a.Room, roomCoords b.Room with
        | Some(ax, ay), Some(bx, by) ->
            Some(
                max
                    (abs ((ax - bx) * (side - 1) + a.X - b.X))
                    (abs ((ay - by) * (side - 1) + a.Y - b.Y))
            )
        | _ -> None

let roomName (x: int) (y: int) : string =
    let axis outward inward value =
        if value < 0 then
            outward + string (-value - 1)
        else
            inward + string value

    axis "W" "E" x + axis "N" "S" y

let roomToward (name: string) (exit: int) : string option =
    match exit with
    | 1
    | 3
    | 5
    | 7 ->
        roomCoords name
        |> Option.map (fun (x, y) -> roomName (x + dx exit) (y + dy exit))
    | _ -> None

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
