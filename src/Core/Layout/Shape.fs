/// The Layout's plan as a value: what `Plan` draws, and the standing copy
/// of it `State` carries on the heap between ticks (`ARCHITECTURE.md` §4,
/// `standing.*`). Ahead of `State` so `State` can hold it.
module Fabot.Core.Layout.Shape

open Fabot.Core.World

/// One planned structure: its kind, its tile, and the level from which the
/// engine allows it (its rank among its kind against CONTROLLER_STRUCTURES,
/// or the level a rule gives a road or container).
[<Struct; NoEquality; NoComparison>]
type Item = { Kind: Kind; Tile: int; Level: int }

/// A room's plan: its anchor, its items in the order they are placed, and
/// what did not fit (empty for a room that holds the whole set).
[<NoEquality; NoComparison>]
type Plan =
    {
        Room: string
        Anchor: int
        Items: Item[]
        Unfit: string list
    }

/// A room's plan as last drawn, and the key it was drawn on: everything the
/// plan reads that can change (`Derive.keyOf`). Redrawn only when the key
/// moves; `Plan` None for a room that offered no anchor.
[<NoEquality; NoComparison>]
type Standing = { Key: string; Plan: Plan option }
