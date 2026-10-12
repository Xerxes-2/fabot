/// `Defend` (`ARCHITECTURE.md` §6.2): a phase moves on an event, never a clock;
/// `step` closes it with None.
module Fabot.Core.Fight.Kinds.Defend

open Fabot.Core.Fight.Shape

[<NoEquality; NoComparison>]
type Facts =
    {
        Time: int
        Group: Group option
        Standing: Set<string>
        InReach: bool
        Home: bool
        Asked: bool
        /// The group stands in a room we hold or work.
        Threatens: bool
    }

let private enter (phase: Phase) (time: int) (e: Engagement) : Engagement =
    { e with Phase = phase; Since = time }

let disengage (time: int) (e: Engagement) = enter Phase.Disengaging time e

let standsAlone (time: int) (e: Engagement) =
    if e.Phase.IsForming && not e.Ours.IsEmpty then
        enter Phase.Approaching time e
    else
        e

let step (facts: Facts) (e: Engagement) : Engagement option =
    let alive = Set.intersect e.Ours facts.Standing

    let e =
        { e with
            Ours = alive
            Lost = e.Lost + e.Ours.Count - alive.Count
        }

    let toward (phase: Phase) = Some(enter phase facts.Time e)

    match facts.Group with
    | None -> None
    | Some _ when e.Phase.IsDisengaging -> if facts.Home then None else Some e
    | Some _ when e.Lost > e.Budget -> toward Phase.Disengaging
    | Some g ->
        let left = g.Room <> e.Room

        match e.Phase with
        | Phase.Forming when left && not facts.Threatens -> toward Phase.Disengaging
        | Phase.Forming when not facts.Asked && not e.Ours.IsEmpty -> toward Phase.Approaching
        | Phase.Approaching
        | Phase.Fighting when left && facts.Threatens -> toward Phase.Pursuing
        | Phase.Approaching
        | Phase.Fighting when left -> toward Phase.Disengaging
        | Phase.Approaching when facts.InReach -> toward Phase.Fighting
        | Phase.Pursuing when not facts.Threatens -> toward Phase.Disengaging
        | _ -> Some e

let homePriority = 2.0
let outpostPriority = 1.0

/// Strategy's stand-in (§7.3): the groups a Defend answers, a home's before an
/// outpost's. A group in a room an engagement targets is that engagement's.
let opens
    (homes: Set<string>)
    (worked: Set<string>)
    (armed: string -> bool)
    (time: int)
    (targeted: Set<string>)
    (groups: Group list)
    : (Group * float) list =
    groups
    |> List.filter (fun g ->
        g.LastSeen = time
        && (homes.Contains g.Room || worked.Contains g.Room)
        && g.Members |> Set.exists armed)
    |> List.map (fun g ->
        g,
        (if homes.Contains g.Room then
             homePriority
         else
             outpostPriority))
    |> List.sortBy (fun (g, priority) -> -priority, g.Id)
    |> List.fold
        (fun (opened, targeted: Set<string>) (g, priority) ->
            if targeted.Contains g.Room then
                opened, targeted
            else
                (g, priority) :: opened, targeted.Add g.Room)
        ([], targeted)
    |> fst
    |> List.rev
