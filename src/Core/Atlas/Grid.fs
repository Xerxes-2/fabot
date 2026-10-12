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

let inline at (index: int) (array: 'T[]) : 'T = Tiles.at index array

#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$1[$0] = $2")>]
#endif
let setAt (_index: int) (_array: 'T[]) (_value: 'T) : unit = _array.[_index] <- _value

/// The bench's counts (`ARCHITECTURE.md` §9.3), read by `scripts/bench.mjs
/// --counts`; nothing in the bot reads them.
#if FABLE_COMPILER_JAVASCRIPT
[<Emit("globalThis.__fabotCount?.($0, $1)")>]
#endif
let tally (_what: string) (_n: int) : unit = ()

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

/// An inner tile's neighbours, as offsets: cheaper than its `Tiles.around` row.
let private offsets =
    [|
        -Tiles.side
        1
        Tiles.side
        -1
        -Tiles.side - 1
        -Tiles.side + 1
        Tiles.side - 1
        Tiles.side + 1
    |]

/// Dial's queue: no step costs more than the table's dearest, so a ring of one
/// bucket more holds every queued distance apart.
let flood (footprint: Footprint) (weights: int[]) (steps: int[]) (goals: int[]) : int[] =
    let dist = Array.create Tiles.count unreached
    // The step onto a tile; -1 where no walk leaves it.
    let price = Array.zeroCreate<int> Tiles.count
    let next = Array.zeroCreate<int> Tiles.count
    let prev = Array.zeroCreate<int> Tiles.count
    let mutable dearest = 0

    for i in 0 .. steps.Length - 1 do
        dearest <- max dearest (at i steps)

    let ring = dearest + 1
    let head = Array.create ring -1
    let mutable queued = 0
    let mutable relaxed = 0

    let stepOn (weight: int) =
        if weight < steps.Length then at weight steps else -1

    let link (tile: int) (d: int) =
        let b = d % ring
        let h = at b head
        setAt tile next h
        setAt tile prev -1

        if h >= 0 then
            setAt h prev tile

        setAt b head tile
        queued <- queued + 1

    let unlink (tile: int) =
        let p = at tile prev
        let n = at tile next

        if p >= 0 then
            setAt p next n
        else
            setAt (at tile dist % ring) head n

        if n >= 0 then
            setAt n prev p

        queued <- queued - 1

    // A blocked tile reads -1 until the end, so no candidate beats it. A
    // non-goal border tile leads nowhere: a body stepping onto it leaves.
    for t in 0 .. Tiles.count - 1 do
        let w = weightOf footprint weights t

        if w < 0 then setAt t dist -1
        elif onEdge footprint t then setAt t price -1
        else setAt t price (stepOn w)

    for i in 0 .. goals.Length - 1 do
        let goal = at i goals

        if goal >= 0 && goal < Tiles.count && at goal dist > 0 then
            setAt goal dist 0
            setAt goal price (stepOn (weightOf footprint weights goal))
            link goal 0

    let mutable d = 0
    let mutable b = 0

    while queued > 0 do
        let tile = at b head

        if tile < 0 then
            d <- d + 1
            b <- if b + 1 = ring then 0 else b + 1
        else
            unlink tile
            let p = at tile price

            if p >= 0 then
                relaxed <- relaxed + 1
                let candidate = d + p
                let x = Tiles.xOf tile
                let y = Tiles.yOf tile
                let inner = x > 0 && y > 0 && x < Tiles.side - 1 && y < Tiles.side - 1
                let around = if inner then offsets else Tiles.around tile
                let origin = if inner then tile else 0

                for i in 0 .. around.Length - 1 do
                    let n = origin + at i around
                    let old = at n dist

                    if candidate < old then
                        if old <> unreached then
                            unlink n

                        setAt n dist candidate
                        link n candidate

    for t in 0 .. Tiles.count - 1 do
        if at t dist < 0 then
            setAt t dist unreached

    tally "flood" relaxed
    dist
