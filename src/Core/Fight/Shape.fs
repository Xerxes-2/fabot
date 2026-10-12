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

[<RequireQualifiedAccess>]
type Phase =
    | Forming
    | Approaching
    | Fighting
    | Pursuing
    | Disengaging

/// `Group` is the target; `Room` the room held or worked it threatened.
type Engagement =
    {
        Id: int
        Group: int
        Room: string
        Ours: Set<string>
        Phase: Phase
        Since: int
        Lost: int
        /// The bodies it may lose before it disengages.
        Budget: int
        Priority: float
        /// Starved: towers and ramparts answer it alone.
        Unanswered: bool
    }
