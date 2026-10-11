/// Knows nothing of the Atlas: it prices raw weight arrays (-1 where nothing
/// may stand), so the Atlas keeps its own representation private.
module Fabot.Core.Grid

open Fable.Core
open Fabot.Core.World

let unreached = System.Int32.MaxValue

[<Struct>]
type Footprint =
    | One
    | Quad

/// MOVE pays off two fatigue a tick, at its boost's multiple.
[<Struct>]
type FatigueFactor = { Weight: int; Moves: int }

/// The engine's rule: a destroyed part still weighs; only a live one carries
/// or moves.
let factorOf (creep: Creep) : FatigueFactor =
    let mutable weight = 0
    let mutable carry = 0
    let mutable moves = 0

    for part in creep.Body do
        match part.Kind with
        | Move ->
            if part.Hits > 0 then
                moves <- moves + Boost.multiple Move part.Boost
        | Carry ->
            if part.Hits > 0 then
                carry <- carry + 1
        | _ -> weight <- weight + 1

    let load = Store.used creep.Store

    {
        Weight =
            weight
            + min carry ((load + Engine.carryPartCapacity - 1) / Engine.carryPartCapacity)
        Moves = moves
    }

let emptyFactor (body: BodyPart list) : FatigueFactor =
    let mutable weight = 0
    let mutable moves = 0

    for part in body do
        match part with
        | Move -> moves <- moves + 1
        | Carry -> ()
        | _ -> weight <- weight + 1

    { Weight = weight; Moves = moves }

let loadedFactor (body: BodyPart list) : FatigueFactor =
    let moves = partCountIn body Move

    {
        Weight = List.length body - moves
        Moves = moves
    }

/// In half-ticks: at least one, since no step is free.
let stepUnits (factor: FatigueFactor) (weight: int) =
    if factor.Moves = 0 then
        -1
    else
        max 1 ((weight * factor.Weight + factor.Moves - 1) / factor.Moves)

/// Exact per step: a body cannot step again until its fatigue is zero, so a
/// path's ticks are the sum of its steps'.
let stepTicks (factor: FatigueFactor) (weight: int) =
    let units = stepUnits factor weight
    if units < 0 then -1 else (units + 1) / 2

/// The detour a walk takes before it waits on a seated body; finite, so the
/// seat's own walk and the only way through still reach it.
let seatWeight = 5 * Engine.swampWeight

/// The flood reads a table, not a closure.
let stepTable (factor: FatigueFactor) : int[] =
    Array.init (seatWeight + 1) (stepTicks factor)

/// The flood's array accessors: checked on .NET, a bare JS index under Fable,
/// whose own indexer re-tests indices the loop has already kept in range.
#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$1[$0]")>]
#endif
let at (_index: int) (_array: 'T[]) : 'T = _array.[_index]

#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$1[$0] = $2")>]
#endif
let setAt (_index: int) (_array: 'T[]) (_value: 'T) : unit = _array.[_index] <- _value

#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$1[$0]")>]
#endif
let private heapAt (_index: int) (_heap: ResizeArray<int>) : int = _heap.[_index]

#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$1[$0] = $2")>]
#endif
let private setHeapAt (_index: int) (_heap: ResizeArray<int>) (_value: int) : unit =
    _heap.[_index] <- _value

let weightOf (footprint: Footprint) (weights: int[]) (tile: int) : int =
    match footprint with
    | One -> at tile weights
    | Quad ->
        let x = Tiles.xOf tile
        let y = Tiles.yOf tile

        if x = Tiles.side - 1 || y = Tiles.side - 1 then
            -1
        else
            let a = at tile weights
            let b = at (tile + 1) weights
            let c = at (tile + Tiles.side) weights
            let d = at (tile + Tiles.side + 1) weights

            if a < 0 || b < 0 || c < 0 || d < 0 then
                -1
            else
                max (max a b) (max c d)

/// A step onto the border ring carries the body into the next room.
let onEdge (footprint: Footprint) (tile: int) : bool =
    match footprint with
    | One -> Tiles.onEdge tile
    | Quad ->
        let x = Tiles.xOf tile
        let y = Tiles.yOf tile
        x = 0 || y = 0 || x >= Tiles.side - 2 || y >= Tiles.side - 2

[<NoEquality; NoComparison>]
type private Heap =
    {
        Keys: ResizeArray<int>
        mutable Size: int
    }

let private push (heap: Heap) (key: int) =
    let keys = heap.Keys

    if heap.Size >= keys.Count then
        keys.Add 0

    let mutable hole = heap.Size
    heap.Size <- heap.Size + 1
    let mutable climbing = hole > 0

    while climbing do
        let parent = (hole - 1) / 2
        let parentKey = heapAt parent keys

        if parentKey > key then
            setHeapAt hole keys parentKey
            hole <- parent
            climbing <- hole > 0
        else
            climbing <- false

    setHeapAt hole keys key

let private pop (heap: Heap) =
    let keys = heap.Keys
    let top = heapAt 0 keys
    heap.Size <- heap.Size - 1
    let size = heap.Size

    if size > 0 then
        let key = heapAt size keys
        let mutable hole = 0
        let mutable sinking = true

        while sinking do
            let left = 2 * hole + 1

            if left >= size then
                sinking <- false
            else
                let right = left + 1
                let mutable child = left
                let mutable childKey = heapAt left keys

                if right < size then
                    let rightKey = heapAt right keys

                    if rightKey < childKey then
                        child <- right
                        childKey <- rightKey

                if childKey < key then
                    setHeapAt hole keys childKey
                    hole <- child
                else
                    sinking <- false

        setHeapAt hole keys key

    top

/// The dearest step is 245 ticks (49 weighted parts on one MOVE, onto swamp),
/// so no path's distance × tiles + tile overflows an int key.
let flood (footprint: Footprint) (weights: int[]) (steps: int[]) (goals: int[]) : int[] =
    let dist = Array.create Tiles.count unreached
    let heap = { Keys = ResizeArray<int>(); Size = 0 }

    for goal in goals do
        if
            goal >= 0
            && goal < Tiles.count
            && weightOf footprint weights goal >= 0
            && at goal dist > 0
        then
            setAt goal dist 0
            push heap goal

    while heap.Size > 0 do
        let key = pop heap
        let tile = key % Tiles.count
        let d = key / Tiles.count

        // A non-goal border tile leads nowhere: a body stepping onto it leaves.
        // Only a tile the footprint stands on is pushed, so its weight indexes
        // the table.
        if at tile dist = d && (d = 0 || not (onEdge footprint tile)) then
            let price = at (weightOf footprint weights tile) steps
            let x = Tiles.xOf tile
            let y = Tiles.yOf tile

            for direction in 1..8 do
                let nx = x + Tiles.dx direction
                let ny = y + Tiles.dy direction

                if price >= 0 && Tiles.inRoom nx ny then
                    let next = Tiles.index nx ny

                    if weightOf footprint weights next >= 0 then
                        let candidate = d + price

                        if candidate < at next dist then
                            setAt next dist candidate
                            push heap (candidate * Tiles.count + next)

    dist
