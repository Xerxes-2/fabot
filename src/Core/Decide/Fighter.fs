/// One body of ours as a fight reads it (#615): what it strikes from how
/// far, what it heals, how it walks and what it takes to kill, read off its
/// parts alone and never off the row that cast it — Arrival's
/// `HostileClass` on our side of the fight.
[<AutoOpen>]
module Fabot.Core.Decide.Fighters

open Fabot.Core
open Fabot.Core.Types

/// A body's fight, by its live parts at their boosts.
type Fighter =
    {
        /// The farthest range it strikes from: 3 with a live RANGED_ATTACK, 1
        /// with a live ATTACK alone, 0 with neither (a medic).
        Reach: int
        /// The hits it takes off one creep at this range a tick: its ATTACK
        /// at range 1 beside its RANGED_ATTACK out to 3, the two acts being
        /// in different chains of the engine's intent priorities. A mass
        /// attack (`Engine.massAttackPowerAt`) never takes more off one
        /// target than the single shot, so it is not priced here.
        DamageAt: int -> int
        /// What it heals itself a tick it fights at this range: nothing where
        /// it swings (range 1 with a live ATTACK), because the engine drops
        /// an `attack` beside a `heal`, so a body that heals has not swung;
        /// all its HEAL elsewhere, a `heal` standing beside a `rangedAttack`.
        HealAt: int -> int
        /// What it puts back on another body at range 2..3 a tick
        /// (`rangedHeal`), which the engine drops beside a `heal` and which
        /// drops a `rangedAttack` beside it.
        RangedHeal: int
        /// What the engine strikes back a tick at each body that swings at
        /// it (`_damage.js`): its live ATTACK at their boosts, whatever it
        /// does that tick. A shot is never struck back.
        StrikesBack: int
        /// Its weight, every part but MOVE and shot-off ones too, against its
        /// live MOVE at their boosts.
        Gait: Gait
        Hits: int
        /// The share of `Hits` in boosted TOUGH still standing
        /// (`Engine.toughHits`), which a strike pays more for.
        Tough: int
        /// The creep of ours it is (`atHits`), which a view places to read
        /// the ground it kites on (`Facts.keepsOff`); None for a priced body
        /// no creep is yet.
        Creep: string option
    }

[<RequireQualifiedAccess>]
module Fighter =
    /// `Reach` of a body acting at `power`.
    let private reachOfPower (power: BodyPart -> int) =
        if power RangedAttack > 0 then Engine.rangedRange
        elif power Attack > 0 then Engine.meleeRange
        else 0

    /// The fight of a body acting at `power` (a part's count at its boost).
    let private ofPower (power: BodyPart -> int) (gait: Gait) (hits: int) (tough: int) : Fighter =
        let attack = power Attack
        let ranged = power RangedAttack
        let heal = power Heal

        let swings range =
            attack > 0 && range <= Engine.meleeRange

        {
            Reach = reachOfPower power
            DamageAt =
                fun range ->
                    (if swings range then Engine.attackPower * attack else 0)
                    + (if range <= Engine.rangedRange then
                           Engine.rangedAttackPower * ranged
                       else
                           0)
            HealAt = fun range -> if swings range then 0 else Engine.healPower * heal
            RangedHeal = Engine.rangedHealPower * heal
            StrikesBack = Engine.attackPower * attack
            Gait = gait
            Hits = hits
            Tough = tough
            Creep = None
        }

    /// A body's fight at these hits: `body` head first as the engine holds
    /// it, so the parts still acting are the tail its hits cover
    /// (`Engine.powerAt`), and `boosts` one a part head first, or empty.
    let ofParts (body: BodyPart list) (boosts: Boost list) (hits: int) : Fighter =
        let power = Engine.powerAt body boosts hits

        ofPower
            power
            {
                Weight = Gait.weightOf (partCountIn body)
                Moves = power Move
            }
            hits
            (Engine.toughHits body boosts hits)

    /// The fight of a body of these live part counts, unboosted, at these
    /// hits: a block count's or a creep's counts as the pricing reads them,
    /// its gait off the counts alone (`Gait.ofParts`).
    let ofCounts (counts: BodyPart -> int) (hits: int) : Fighter =
        ofPower counts (Gait.ofParts counts) hits 0

    /// The order a body of ours carries its parts in, head first, as our rows
    /// cast them (`Bodies`, a block's parts grouped in its own order): TOUGH,
    /// then MOVE, the weapons, HEAL last. The engine destroys the head first.
    let private headFirst =
        [ Tough; Move; Work; Carry; BodyPart.Claim; Attack; RangedAttack; Heal ]

    /// A living creep of ours' fight worn to `hits` (#615 U3): its parts laid
    /// head first (`headFirst`), so the parts still acting are the tail those
    /// hits cover (`Engine.powerAt`), unboosted (no body of ours is boosted at
    /// spawn yet); its gait `Arrival.gaitOf`'s weight (`movement.js` weighs
    /// every part but MOVE and CARRY, shot off or not) on the MOVE still
    /// live (`tick.js` pays two a live MOVE). At the creep's own hits every part it counts is
    /// live, and a count stating more than its hits cover prices none of the
    /// rest.
    let atHits (creep: CreepInfo) (hits: int) : Fighter =
        let hits = max 0 (min hits creep.Hits.Hits)
        let counts = partCount creep.Body
        let parts = creep.Body |> Map.fold (fun total _ count -> total + count) 0

        // Every part counted still acts (the live tick's case): no laying out.
        let power =
            if parts * Engine.partHits < hits + Engine.partHits then
                counts
            else
                Engine.powerAt
                    (headFirst |> List.collect (fun part -> List.replicate (counts part) part))
                    []
                    hits

        // The engine weighs every part but MOVE, shot off or not, and each
        // live MOVE pays two off: a MOVE shot off since only stops paying.
        let gait = Arrival.gaitOf creep

        { ofPower
              power
              {
                  Weight = gait.Weight
                  Moves = min gait.Moves (power Move)
              }
              hits
              0 with
            Creep = Some creep.Name
        }

    /// A living creep of ours' fight at its hits (`atHits`).
    let ofCreep (creep: CreepInfo) : Fighter = atHits creep creep.Hits.Hits

    /// A living creep of ours' `Reach` alone, for the readers asking nothing
    /// else of its fight every tick.
    let reachOf (creep: CreepInfo) : int = reachOfPower (partCount creep.Body)

    /// Whether some weapon strikes shorter than this body reaches: a swing is
    /// the shortest, so only a body that shoots outranges anything.
    let outranges (fighter: Fighter) : bool = fighter.Reach > Engine.meleeRange

    /// Whether a body stands off a hostile class on pace alone (#615 U1): it
    /// holds at its own reach outside the class's strike, where the class
    /// strikes shorter than it reaches and falls behind it on plain
    /// (`Gait.fallsBehind`, #613's ground). A class as quick is stood off
    /// only by a body's damage and its ground (`Facts.keepsOff`, #625),
    /// which this floor is the first half of.
    let standsOff (fighter: Fighter) (cls: HostileClass) : bool =
        cls.Strike < fighter.Reach
        && (match cls.Pace, Gait.ticks Gait.plain fighter.Gait with
            | None, Some _ -> true
            | Some pace, Some ours -> pace.Plain > ours
            | _, None -> false)

    /// The range a body picks where the pick is its own (#615 U2): the
    /// farthest it deals its most from, so a ranger holds three and a body
    /// that swings closes to one: what it deals falls with range, so its
    /// reach is the farthest where its reach deals what a swing's range does.
    let bestRange (fighter: Fighter) : int =
        if fighter.DamageAt fighter.Reach >= fighter.DamageAt Engine.meleeRange then
            max Engine.meleeRange fighter.Reach
        else
            Engine.meleeRange

    /// Whether a body's standoff is its target's tile (#615 U1): its reach is
    /// a swing, so it closes to one and steps after the target onto the tile
    /// it leaves (`Emitter.glue`), its chase weighing that step as any other.
    let glues (fighter: Fighter) : bool = fighter.Reach = Engine.meleeRange
