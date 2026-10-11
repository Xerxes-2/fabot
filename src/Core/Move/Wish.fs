// Tiles are indices of the room the creep stands in; no wish names a tile of
// another room.
namespace Fabot.Core.Move

open Fabot.Core.Grid

/// A wish whose `Head` is `At` stays, and `Tail` lists where it may be shoved.
[<NoEquality; NoComparison>]
type Wish =
    {
        Creep: string
        /// A `Quad` is held where it stands: formation stepping is not
        /// arbitrated.
        Footprint: Footprint
        At: int
        Head: int
        /// -1 for none. A body it shoves off its head is put there last.
        Onward: int
        /// Best first.
        Tail: int[]
        /// Its work area when it stands inside it, else empty; holds `At` when
        /// not empty.
        Working: int[]
        /// What its head is worth over standing still.
        Weight: int
        Moves: bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Wish =
    let stays (wish: Wish) = wish.Head = wish.At

    let movable (wish: Wish) =
        wish.Moves
        && (match wish.Footprint with
            | One -> true
            | Quad -> false)

    /// A body is pushed off its work only by a chain at least two weights
    /// heavier: one weight heavier scores nothing.
    let value (wish: Wish) (tile: int) : int =
        let weight = max 1 wish.Weight

        let asked =
            if stays wish then 0
            elif tile = wish.Head then weight
            elif Array.contains tile wish.Tail then 1
            else 0

        if wish.Working.Length > 0 && not (Array.contains tile wish.Working) then
            asked - weight - 1
        else
            asked
