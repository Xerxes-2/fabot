/// A body as the exchange reads it, on either side.
module Fabot.Core.Exchange.Fighter

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Grid

/// Weapon and heal fields are live parts at their boosts' multiples.
[<NoEquality; NoComparison>]
type Fighter =
    {
        /// Head first, as the engine destroys it.
        Body: BodyPart list
        /// One a part, head first, or empty.
        Boosts: Boost list
        Hits: int
        /// None for a body not yet cast.
        Pos: Pos option
        Attack: int
        Ranged: int
        Heal: int
        Work: int
        Reach: int
        Gait: FatigueFactor
        /// Zero on a rampart, where a swing at it lands on the rampart.
        StrikesBack: int
        Invader: bool
    }

let ofParts (body: BodyPart list) (boosts: Boost list) (hits: int) (pos: Pos option) : Fighter =
    let power = Engine.powerAt body boosts hits
    let attack = power Attack
    let ranged = power RangedAttack

    {
        Body = body
        Boosts = boosts
        Hits = hits
        Pos = pos
        Attack = attack
        Ranged = ranged
        Heal = power Heal
        Work = power Work
        Reach =
            if ranged > 0 then Engine.rangedRange
            elif attack > 0 then Engine.meleeRange
            else 0
        Gait =
            {
                Weight = List.length body - partCountIn body Move - partCountIn body Carry
                Moves = power Move
            }
        StrikesBack = Engine.attackPower * attack
        Invader = false
    }

let cast (body: BodyPart list) : Fighter =
    ofParts body [] (Engine.partHits * List.length body) None

/// Unlike `Hostiles.armed`, a healer or a dismantler beside a striker does not count: it deals no damage.
let strikes (f: Fighter) = f.Reach > 0

let swings (f: Fighter) (range: int) =
    f.Attack > 0 && range <= Engine.meleeRange

/// The engine's ATTACK and RANGED_ATTACK act in separate intent chains.
let damageAt (f: Fighter) (range: int) : int =
    (if swings f range then Engine.attackPower * f.Attack else 0)
    + (if range <= Engine.rangedRange then
           Engine.rangedAttackPower * f.Ranged
       else
           0)

/// A `heal` beside an `attack` drops the attack, so a body that swings heals nothing.
let healAt (f: Fighter) (range: int) : int =
    if swings f range then 0 else Engine.healPower * f.Heal

let healing (f: Fighter) = Engine.healPower * f.Heal

let damageFor (f: Fighter) (hits: int) =
    Engine.damageFor f.Body f.Boosts f.Hits hits

let effectiveHits (f: Fighter) = damageFor f f.Hits

let toughHits (f: Fighter) = Engine.toughHits f.Body f.Boosts f.Hits

let pace (weight: int) (gait: FatigueFactor) : int option =
    match stepTicks gait weight with
    | -1 -> None
    | ticks -> Some ticks

/// A body with no MOVE steps nowhere of itself: a puller's pace is the puller's.
let fallsBehind (weight: int) (ours: FatigueFactor) (theirs: FatigueFactor) : bool =
    match pace weight theirs, pace weight ours with
    | None, Some _ -> true
    | Some t, Some o -> t > o
    | _, None -> false

/// Read on plain, as the ground a kite runs on: a swamp tile's pace at a MOVE a part.
let stationary (f: Fighter) =
    pace Engine.plainWeight f.Gait |> Option.exists (fun ticks -> ticks >= 5)

let bestRange (f: Fighter) : int =
    if damageAt f f.Reach >= damageAt f Engine.meleeRange then
        max Engine.meleeRange f.Reach
    else
        Engine.meleeRange
