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
    /// A ranged set that approaches as one, then fights by `fights`.
    | Blob of fights: Tactic

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
        Opened: int
        Since: int
        Lost: int
        /// The bodies it may lose before it disengages.
        Budget: int
        Priority: float
        /// Starved: towers and ramparts answer it alone.
        Unanswered: bool
        /// The bank it was last priced against; a larger one is an event.
        Capacity: int
        /// What `Exchange` said each time it was answered after standing
        /// unanswered (at open, or a given-up home's re-answered), set against
        /// what happened (§10.1); None while it never was.
        Predicted: Fabot.Core.Exchange.Exchange.Outcome option
    }

/// A body asked of `Room`'s spawns under `Name`, until it stands.
type Cast =
    {
        Engagement: int
        Name: string
        Room: string
        Body: BodyPart list
    }
