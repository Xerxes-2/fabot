/// `Defend` (`ARCHITECTURE.md` §6.2): a phase moves on an event, never a clock;
/// `step` closes it with None.
module Fabot.Core.Fight.Kinds.Defend

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Fight.Shape

[<NoEquality; NoComparison>]
type Facts =
    {
        Time: int
        Group: Group option
        Standing: Set<string>
        AgingOut: Set<string>
        InReach: bool
        Home: bool
        /// It defends a home, which it never leaves to its towers alone.
        Held: bool
        Asked: bool
        /// The group stands in a room we hold or work.
        Threatens: bool
    }

let private enter (phase: Phase) (time: int) (e: Engagement) : Engagement =
    { e with Phase = phase; Since = time }

/// A home's Defend never disengages: it stops asking, holds and is unanswered.
let giveUp (held: bool) (time: int) (e: Engagement) =
    if held then
        { e with Unanswered = true }
    else
        enter Phase.Disengaging time e

let standsAlone (time: int) (e: Engagement) =
    if e.Phase.IsForming && not e.Ours.IsEmpty then
        enter Phase.Approaching time e
    else
        e

let step (facts: Facts) (e: Engagement) : Engagement option =
    let alive = Set.intersect e.Ours facts.Standing

    let e =
        { e with
            Ours = Set.difference alive facts.AgingOut
            Lost = e.Lost + e.Ours.Count - alive.Count
        }

    let toward (phase: Phase) = Some(enter phase facts.Time e)
    let quit () = Some(giveUp facts.Held facts.Time e)

    match facts.Group with
    | None -> None
    | Some _ when e.Phase.IsDisengaging -> if facts.Home then None else Some e
    | Some _ when e.Lost > e.Budget -> quit ()
    | Some g ->
        let left = g.Room <> e.Room

        match e.Phase with
        | Phase.Forming when left && not facts.Threatens -> quit ()
        | Phase.Forming when not facts.Asked && not e.Ours.IsEmpty -> toward Phase.Approaching
        | Phase.Approaching
        | Phase.Fighting when left && facts.Threatens -> toward Phase.Pursuing
        | Phase.Approaching
        | Phase.Fighting when left -> quit ()
        | Phase.Approaching when facts.InReach -> toward Phase.Fighting
        | Phase.Pursuing when not facts.Threatens -> quit ()
        | _ -> Some e

[<NoEquality; NoComparison>]
type Home =
    {
        Controller: Controller
        Towered: bool
        /// The unanswered Defend's group standing in the home; empty while answered.
        Raid: Creep[]
        Hostiles: Creep[]
        Ours: Structure[]
        Bodies: Creep[]
    }

/// A tap is range 1 from where the claimer stood at the tick's start and it
/// steps one tile a tick; one tile more covers a skipped tick.
let private claimerReach = Engine.meleeRange + 2

/// Two 16-CARRY haulers: two cheap bodies under it the home casts again itself.
let private refillingFloor = 32

let private live (kind: BodyPart) (c: Creep) =
    c.Body |> Array.exists (fun p -> p.Kind = kind && p.Hits > 0)

let private reaches (structure: bool) (raid: Creep[]) (at: Pos) =
    raid
    |> Array.exists (fun h ->
        let range =
            if live RangedAttack h then
                Engine.rangedRange
            elif live Attack h || structure && live Work h then
                Engine.meleeRange
            else
                -1

        Pos.within range h.Pos at)

let private half (lost: int) (whole: int) = whole > 0 && 2 * lost >= whole

let private razed (home: Home) =
    let raid = home.Raid |> Array.filter (fun h -> h.Owner <> Theirs Engine.invaderUser)

    let covered =
        home.Ours
        |> Array.filter (fun s -> s.Kind.IsRampart)
        |> Array.map (fun s -> s.Pos)
        |> Set.ofArray

    let share (kind: Kind) =
        let all = home.Ours |> Array.filter (fun s -> s.Kind = kind)

        let lost =
            all
            |> Array.filter (fun s -> not (covered.Contains s.Pos) && reaches true raid s.Pos)

        lost.Length, all.Length

    let carry (c: Creep) =
        c.Body |> Array.sumBy (fun p -> if p.Kind.IsCarry then 1 else 0)

    let hands =
        home.Bodies
        |> Array.filter (fun c -> carry c > 0 && not (covered.Contains c.Pos))

    let struck = hands |> Array.filter (fun c -> reaches false raid c.Pos)
    let spawns, banks = share Kind.Spawn, share Kind.Extension

    half (fst spawns) (snd spawns)
    || fst banks >= 2 && half (fst banks) (snd banks)
    || struck.Length >= 2
       && Array.sumBy carry struck >= refillingFloor
       && half (Array.sumBy carry struck) (Array.sumBy carry hands)

let private keep = set [ Kind.Spawn; Kind.Tower; Kind.Storage; Kind.Terminal ]

/// An unanswered raid is one the towers and ramparts are left to; the stock is
/// spent where they hold nothing, or what cannot move or regrow is falling.
let private lastResort (home: Home) =
    not (Array.isEmpty home.Raid)
    && (not home.Towered
        || home.Ours
           |> Array.exists (fun s ->
               keep.Contains s.Kind && s.Hits < s.HitsMax && reaches true home.Raid s.Pos)
        || razed home)

let private fireable (c: Controller) =
    c.SafeModeAvailable > 0
    && c.SafeModeCooldown = 0
    && c.UpgradeBlocked = 0
    && c.TicksToDowngrade >= Engine.safeModeDowngradeLine c.Level

let safeMode (running: bool) (homes: Home list) : string option =
    let claimer (c: Controller) (h: Creep) =
        live Claim h && Pos.within claimerReach h.Pos c.Pos

    if running then
        None
    else
        homes
        |> List.tryFind (fun h ->
            fireable h.Controller
            && (h.Hostiles |> Array.exists (claimer h.Controller) || lastResort h))
        |> Option.map (fun h -> h.Controller.Id)

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
