/// `Siege` (`ARCHITECTURE.md` §6.2); `step` closes it with None once the way in stands open.
module Fabot.Core.Fight.Kinds.Siege

open Fabot.Core.Fight.Shape

[<NoEquality; NoComparison>]
type Facts =
    {
        Time: int
        Standing: Set<string>
        AgingOut: Set<string>
        Asked: bool
        Home: bool
        /// One of ours stands in the rival's room, all of it in its towers' reach.
        UnderFire: bool
        Open: bool
    }

let private enter (phase: Phase) (time: int) (e: Engagement) : Engagement =
    { e with Phase = phase; Since = time }

let step (facts: Facts) (e: Engagement) : Engagement option =
    let alive = Set.intersect e.Ours facts.Standing

    let e =
        { e with
            Ours = Set.difference alive facts.AgingOut
            Lost = e.Lost + e.Ours.Count - alive.Count
        }

    match e.Phase with
    | _ when facts.Open -> None
    | Phase.Disengaging -> if facts.Home then None else Some e
    | _ when e.Lost > e.Budget -> Some(enter Phase.Disengaging facts.Time e)
    | Phase.Forming when not facts.Asked && not e.Ours.IsEmpty ->
        Some(enter Phase.Approaching facts.Time e)
    | Phase.Approaching when facts.UnderFire -> Some(enter Phase.Fighting facts.Time e)
    | _ -> Some e

/// A plan re-priced while fighting is fought from now; none disengages.
let replanned (time: int) (held: bool) (e: Engagement) : Engagement =
    match e.Phase with
    | _ when not held -> enter Phase.Disengaging time e
    | Phase.Fighting -> enter Phase.Fighting time e
    | _ -> e
