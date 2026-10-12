/// Ahead of `State` so `State` can hold the fight's stores (`ARCHITECTURE.md` §4).
module Fabot.Core.Fight.Shape

open Fabot.Core.Vocabulary
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

/// How the plan fights while it approaches and fights; a phase's own tactic
/// (pursue, retreat) is the phase's.
[<RequireQualifiedAccess>]
type Tactic =
    | Kite
    | Close
    /// On our ramparts: a home is a post the raid comes to.
    | Hold

/// Set when a side is priced, never between: `Order` is the hostiles' ids,
/// first struck first.
type Plan = { Tactic: Tactic; Order: string list }

/// `Group` is the target; `Room` the room held or worked it threatened.
type Engagement =
    {
        Id: int
        Group: int
        Room: string
        Ours: Set<string>
        Plan: Plan
        Phase: Phase
        Since: int
        Lost: int
        /// The bodies it may lose before it disengages.
        Budget: int
        Priority: float
        /// Starved: towers and ramparts answer it alone.
        Unanswered: bool
        /// The bank it was last priced against; a larger one is an event.
        Capacity: int
    }

/// A body asked of `Room`'s spawns under `Name`, until it stands.
type Cast =
    {
        Engagement: int
        Name: string
        Room: string
        Body: BodyPart list
    }
