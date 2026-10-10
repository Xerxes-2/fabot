// What a creep asks of its room's movement arbitration, and what standing on
// a tile is worth to it. Tiles are indices (`Tiles.index`) of the room the
// creep stands in; no wish names a tile of another room.
namespace Fabot.Core.Move

open Fabot.Core.Grid

/// One creep's ask for next tick. A wish whose `Head` is the tile it stands
/// on **stays**: it asked for nothing, and `Tail` lists where it may be shoved
/// to. Any other wish **moves**: `Head` is the step it wants, `Tail` the tiles
/// it takes instead, best first.
[<NoEquality; NoComparison>]
type Wish =
    {
        Creep: string
        /// One tile today. A `Quad` is held where it stands: formation
        /// stepping is not arbitrated yet.
        Footprint: Footprint
        /// The tile it stands on.
        At: int
        /// The tile it wants next; `At` to stay.
        Head: int
        /// The tile its walk takes after its head; -1 for none. A body it
        /// shoves off its head is put there last.
        Onward: int
        /// The tiles it accepts instead of its head, best first.
        Tail: int[]
        /// The tiles that count as still working for it: its work area when
        /// it stands inside it; empty for a body standing anywhere it does not
        /// work from (travelling, parked, idle). Never without `At` when not
        /// empty.
        Working: int[]
        /// Its push, at least 1: what its head is worth over standing still.
        Weight: int
        /// Whether it can step this tick: rested and with a live MOVE part.
        Moves: bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Wish =
    /// Whether it asked for nothing but the tile it stands on.
    let stays (wish: Wish) = wish.Head = wish.At

    /// Whether the arbitration may place it anywhere but where it stands.
    let movable (wish: Wish) =
        wish.Moves
        && (match wish.Footprint with
            | One -> true
            | Quad -> false)

    /// What standing on `tile` is worth to it, the score the arbitration
    /// raises: for a wish that moves, its weight on its head and 1 (a
    /// sidestep) on a tail tile; nothing for a wish that stays; and less its
    /// weight and one off its working tiles, when it has any. So a body is
    /// pushed off its work only by a chain at least two weights heavier: one
    /// weight heavier scores nothing.
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
