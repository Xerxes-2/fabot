/// Ahead of `State` so `State` can hold the fight's stores (`ARCHITECTURE.md` §4).
module Fabot.Core.Fight.Shape

open Fabot.Core.World

type Group =
    {
        Id: int
        Members: Set<string>
        Room: string
        LastSeen: int
        Centre: Pos
    }
