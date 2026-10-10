/// A room's set of in-grid tiles as a bitset (ADR-0097): the `Set<Pos>` of a
/// room's tiles, membership and the set algebra a word at a time. A tile is
/// its grid index (`indexOf`), and the index ascends X then Y, `Pos`'s own
/// order, so a `TileSet` enumerates what the `Set<Pos>` it replaces did, in
/// the order it did. Immutable to its readers: every operation that changes
/// it answers a new one.
[<AutoOpen>]
module Fabot.Core.Types.TileSets

open Fable.Core

// The grid's tiles, 32 to a word.
let private wordCount = (tileCount + 31) / 32

// A word read and written bare under Fable, as `Grid.at` reads a grid: every
// index here is already below `wordCount`, and Fable's own indexer would test
// it again on every word.
[<Emit("$1[$0]")>]
let private word (index: int) (words: int[]) : int = words.[index]

[<Emit("$1[$0] = $2")>]
let private setWord (index: int) (words: int[]) (value: int) : unit = words.[index] <- value

let inline private wordOf (index: int) = index >>> 5
let inline private bitOf (index: int) = 1 <<< (index &&& 31)

// The index of a word's lowest set bit, the word not zero: the count of
// trailing zeros, which JS reaches through `clz32` of the bit alone. The .NET
// body is the tests' (netstandard2.0 has no `BitOperations`).
[<Emit("(31 - Math.clz32($0 & -$0))")>]
let private lowestBit (w: int) : int =
    let mutable low = w &&& -w
    let mutable b = 0

    while low <> 1 do
        // A logical shift: bit 31 alone reads negative, and `>>>` on an int
        // keeps its sign.
        low <- (low >>> 1) &&& 0x7FFFFFFF
        b <- b + 1

    b

let private popCount (w: int) =
    let mutable rest = w
    let mutable n = 0

    while rest <> 0 do
        rest <- rest &&& (rest - 1)
        n <- n + 1

    n

/// A room's tiles, by grid index: in-grid alone.
[<CustomEquality; CustomComparison>]
type TileSet =
    private
        {
            Words: int[]
        }

    override this.Equals other =
        match other with
        | :? TileSet as that ->
            let mutable same = true
            let mutable i = 0

            while same && i < wordCount do
                same <- word i this.Words = word i that.Words
                i <- i + 1

            same
        | _ -> false

    override this.GetHashCode() =
        let mutable h = 17

        for i in 0 .. wordCount - 1 do
            h <- h * 31 + word i this.Words

        h

    /// `Set<Pos>`'s order: the two as sorted sequences, element by element,
    /// a prefix the lesser. At the first word they differ in, the lowest bit
    /// they differ in is the first tile one holds and the other does not; the
    /// one holding it is the lesser, unless the other has no tile left past
    /// it, which makes the other a prefix.
    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? TileSet as that ->
                let mutable i = 0

                while i < wordCount && word i this.Words = word i that.Words do
                    i <- i + 1

                if i = wordCount then
                    0
                else
                    let a = word i this.Words
                    let b = word i that.Words
                    let bit = lowestBit (a ^^^ b)
                    let mine = a &&& (1 <<< bit) <> 0
                    let mask = if bit = 31 then 0 else -1 <<< (bit + 1)

                    // Whether a set holds any tile past the differing one.
                    let past (words: int[]) (w: int) =
                        w &&& mask <> 0
                        || (let mutable j = i + 1
                            let mutable any = false

                            while not any && j < wordCount do
                                any <- word j words <> 0
                                j <- j + 1

                            any)

                    if mine then
                        if past that.Words b then -1 else 1
                    elif past this.Words a then
                        1
                    else
                        -1
            | _ -> 1

[<RequireQualifiedAccess>]
module TileSet =

    let private indexIn (tile: Pos) =
        if not (inGrid tile) then
            invalidArg
                "tile"
                $"{tile.X},{tile.Y} is off the grid: a TileSet holds a room's own tiles"

        indexOf tile

    let empty: TileSet = { Words = Array.zeroCreate wordCount }

    let isEmpty (set: TileSet) =
        let mutable empty = true
        let mutable i = 0

        while empty && i < wordCount do
            empty <- word i set.Words = 0
            i <- i + 1

        empty

    let count (set: TileSet) =
        let mutable n = 0

        for i in 0 .. wordCount - 1 do
            n <- n + popCount (word i set.Words)

        n

    /// Off the grid is in no TileSet, as no such tile is in its room.
    let contains (tile: Pos) (set: TileSet) =
        inGrid tile
        && (let index = indexOf tile
            word (wordOf index) set.Words &&& bitOf index <> 0)

    let add (tile: Pos) (set: TileSet) : TileSet =
        let index = indexIn tile

        if word (wordOf index) set.Words &&& bitOf index <> 0 then
            set
        else
            let words = Array.copy set.Words
            setWord (wordOf index) words (word (wordOf index) words ||| bitOf index)
            { Words = words }

    let remove (tile: Pos) (set: TileSet) : TileSet =
        if not (contains tile set) then
            set
        else
            let index = indexOf tile
            let words = Array.copy set.Words
            setWord (wordOf index) words (word (wordOf index) words &&& ~~~(bitOf index))
            { Words = words }

    let singleton (tile: Pos) = add tile empty

    let ofSeq (tiles: seq<Pos>) : TileSet =
        let words = Array.zeroCreate wordCount

        for tile in tiles do
            let index = indexIn tile
            setWord (wordOf index) words (word (wordOf index) words ||| bitOf index)

        { Words = words }

    let ofList (tiles: Pos list) = ofSeq tiles
    let ofArray (tiles: Pos[]) = ofSeq tiles

    /// Every tile, in `Pos` order, to `f`.
    let iter (f: Pos -> unit) (set: TileSet) =
        for i in 0 .. wordCount - 1 do
            let mutable w = word i set.Words

            while w <> 0 do
                f (posAt ((i <<< 5) + lowestBit w))
                w <- w &&& (w - 1)

    let fold (folder: 'state -> Pos -> 'state) (state: 'state) (set: TileSet) : 'state =
        let mutable acc = state

        for i in 0 .. wordCount - 1 do
            let mutable w = word i set.Words

            while w <> 0 do
                acc <- folder acc (posAt ((i <<< 5) + lowestBit w))
                w <- w &&& (w - 1)

        acc

    let toArray (set: TileSet) : Pos[] =
        let tiles = Array.zeroCreate (count set)
        let mutable n = 0

        for i in 0 .. wordCount - 1 do
            let mutable w = word i set.Words

            while w <> 0 do
                tiles.[n] <- posAt ((i <<< 5) + lowestBit w)
                n <- n + 1
                w <- w &&& (w - 1)

        tiles

    let toList (set: TileSet) : Pos list = List.ofArray (toArray set)

    let toSeq (set: TileSet) : seq<Pos> = toArray set :> seq<Pos>

    let exists (predicate: Pos -> bool) (set: TileSet) =
        let mutable found = false
        let mutable i = 0

        while not found && i < wordCount do
            let mutable w = word i set.Words

            while not found && w <> 0 do
                found <- predicate (posAt ((i <<< 5) + lowestBit w))
                w <- w &&& (w - 1)

            i <- i + 1

        found

    let forall (predicate: Pos -> bool) (set: TileSet) =
        let mutable all = true
        let mutable i = 0

        while all && i < wordCount do
            let mutable w = word i set.Words

            while all && w <> 0 do
                all <- predicate (posAt ((i <<< 5) + lowestBit w))
                w <- w &&& (w - 1)

            i <- i + 1

        all

    let filter (predicate: Pos -> bool) (set: TileSet) : TileSet =
        let words = Array.zeroCreate wordCount

        for i in 0 .. wordCount - 1 do
            let mutable w = word i set.Words
            let mutable kept = 0

            while w <> 0 do
                let low = w &&& -w

                if predicate (posAt ((i <<< 5) + lowestBit w)) then
                    kept <- kept ||| low

                w <- w &&& (w - 1)

            setWord i words kept

        { Words = words }

    let union (a: TileSet) (b: TileSet) : TileSet =
        let words = Array.zeroCreate wordCount

        for i in 0 .. wordCount - 1 do
            setWord i words (word i a.Words ||| word i b.Words)

        { Words = words }

    let unionMany (sets: seq<TileSet>) : TileSet =
        let words = Array.zeroCreate wordCount

        for set in sets do
            for i in 0 .. wordCount - 1 do
                setWord i words (word i words ||| word i set.Words)

        { Words = words }

    let intersect (a: TileSet) (b: TileSet) : TileSet =
        let words = Array.zeroCreate wordCount

        for i in 0 .. wordCount - 1 do
            setWord i words (word i a.Words &&& word i b.Words)

        { Words = words }

    /// `a` less `b`.
    let difference (a: TileSet) (b: TileSet) : TileSet =
        let words = Array.zeroCreate wordCount

        for i in 0 .. wordCount - 1 do
            setWord i words (word i a.Words &&& ~~~(word i b.Words))

        { Words = words }

    let isSubset (a: TileSet) (b: TileSet) =
        let mutable subset = true
        let mutable i = 0

        while subset && i < wordCount do
            subset <- word i a.Words &&& ~~~(word i b.Words) = 0
            i <- i + 1

        subset

    /// The least tile in `Pos` order; throws on an empty set, as `Set.minElement`.
    let minElement (set: TileSet) : Pos =
        let mutable i = 0

        while i < wordCount && word i set.Words = 0 do
            i <- i + 1

        if i = wordCount then
            invalidArg "set" "The input set was empty."

        posAt ((i <<< 5) + lowestBit (word i set.Words))

    /// The `Set<Pos>` a seam not yet moved reads, built to outlive the tick.
    let toSet (set: TileSet) : Set<Pos> = Fresh.setOfSeq (toArray set)

    /// A `Set<Pos>` of in-grid tiles, as a `TileSet`.
    let ofSet (tiles: Set<Pos>) : TileSet = ofSeq tiles
