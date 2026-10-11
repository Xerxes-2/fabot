/// Ahead of `State` so `State` can hold the standing plan.
module Fabot.Core.Layout.Shape

open Fabot.Core.World

/// `Level`: from which the engine allows it (its rank among its kind against
/// CONTROLLER_STRUCTURES), or the level a rule gives a road or container.
[<Struct; NoEquality; NoComparison>]
type Item = { Kind: Kind; Tile: int; Level: int }

/// `Items` in the order they are placed.
[<NoEquality; NoComparison>]
type Plan =
    {
        Room: string
        Anchor: int
        Items: Item[]
        Unfit: string list
    }

[<NoEquality; NoComparison>]
type Standing = { Key: string; Plan: Plan option }
