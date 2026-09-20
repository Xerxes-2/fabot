/// The two vocabularies that share the engine's names, in the one file that
/// can keep them apart. `BodyPart.Claim` names a CLAIM part and `Task.Claim`
/// the act of taking a controller; a bare `Claim` means the Task, and it means
/// that because `Task` is declared second **here**. F# resolves the collision
/// by declaration order, and order only holds within one file.
[<AutoOpen>]
module Fabot.Core.Types.Vocabulary

/// A creep body part, the engine's full vocabulary. Our own bodies use only
/// Work/Carry/Move; the rest arrive on hostile creeps.
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

/// A body's parts counted by kind — the shape a living creep already carries
/// in `CreepInfo.Body`, so one rule reads a body still in the oven and a
/// living creep alike.
///
/// Counted in one pass over eight counters rather than through
/// `List.countBy`, which hashes each part into a table and then builds the
/// map anyway: the shell counts every living body this way every tick, and
/// that was 3% of a `pair --level 7` tick by inclusive samples (2026-09-18,
/// #370). The map is the same map — a kind occurs in it with its count, or
/// not at all.
let partsOf (body: BodyPart list) : Map<BodyPart, int> =
    let mutable work = 0
    let mutable carry = 0
    let mutable move = 0
    let mutable attack = 0
    let mutable ranged = 0
    let mutable heal = 0
    let mutable claim = 0
    let mutable tough = 0

    for part in body do
        match part with
        | Work -> work <- work + 1
        | Carry -> carry <- carry + 1
        | Move -> move <- move + 1
        | Attack -> attack <- attack + 1
        | RangedAttack -> ranged <- ranged + 1
        | Heal -> heal <- heal + 1
        | Claim -> claim <- claim + 1
        | Tough -> tough <- tough + 1

    [
        Work, work
        Carry, carry
        Move, move
        Attack, attack
        RangedAttack, ranged
        Heal, heal
        Claim, claim
        Tough, tough
    ]
    |> List.filter (fun (_, count) -> count > 0)
    |> Map.ofList

/// How many of one part a counted body holds — 0 for one it has none of.
let partCount (parts: Map<BodyPart, int>) part =
    parts |> Map.tryFind part |> Option.defaultValue 0

/// How many of one part a body still in the oven holds, counted off the list
/// itself: for the rules that ask about a single part, where building a map
/// to read one key out of it is the dearer spelling.
let partCountIn (body: BodyPart list) part =
    body |> List.filter ((=) part) |> List.length

/// What a store holds, as this colony reads it: the energy every Task in the
/// pool is about, and the season's Thorium beside it. ADR-0057
///
/// Two cases and not the engine's whole `RESOURCE_*` table: a resource belongs
/// here when a decision of ours names it. The room's ordinary ore is never
/// extracted and never reaches the projection (`World` filters
/// `FIND_MINERALS` to `mineralType = "T"`). Declared beside the body parts
/// because it is the engine's vocabulary: `withdraw` and `transfer` have taken
/// one of these strings all along.
type Resource =
    | Energy
    | Thorium

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    /// Dig a **rock**: an energy source or a [[thorium]] deposit — same act,
    /// same Intent, same Task kind. `rockId` and not `sourceId` because half of
    /// what it can name is not a source.
    | Harvest of rockId: string
    /// Take a resource out of a stocked container, or out of the Storage a
    /// tier below them — the haul cycle's intake, judged over stores rather
    /// than energy's name.
    | Withdraw of storeId: string * resource: Resource
    /// Walk to a dropped pile and take it: what sends a creep to a pile the
    /// [[pickup reflex]] (range 1, in passing) will never reach. Pooled on the
    /// pile's amount alone, from `Tuning.PickupThreshold`. Ranked down the
    /// same column Withdraw is: the `Energy` arm is Feeding-tier intake; the
    /// `Thorium` arm is drawn on the [[storage]]'s own tier, gated on an
    /// **empty** body, so no travel cost ever sets it against an energy
    /// intake.
    ///
    /// The **resource** is the Withdraw's argument on the one target that
    /// keeps no store to read it off (#311): a dropped pile holds its amount
    /// in `object[resourceType]` and not in a `store`, and the engine's own
    /// `pickup` takes no argument, so the Task has to say which.
    | Pickup of pileId: string * resource: Resource
    /// Deliver energy into an energy-hungry structure: a tower, the upgrade
    /// [[buffer]], the [[storage]], a [[ferry]]'s sink — and the flow's own
    /// sink, which is not a structure but a **place**: the id is the [[refill
    /// cluster]]'s spawn, and that spawn and every extension of the colony are
    /// one Task with one [[capacity]]. The [[storage]] takes a `Thorium` one
    /// beside its own `Energy` Refill.
    | Refill of structureId: string * resource: Resource
    | Build of siteId: string
    | Repair of structureId: string
    | Upgrade of controllerId: string
    /// Holding a neutral controller with CLAIM parts: work that is never
    /// finished, one per projected controller that is not the colony's own.
    | Reserve of controllerId: string
    /// Taking a **candidate colony**'s controller for our own with CLAIM
    /// parts: one per declared home this colony does not own yet, and never
    /// for a plain [[outpost]], whose controller is [[reserve]]d instead.
    | Claim of controllerId: string
    /// Taking the sector **Reactor** for this player with CLAIM parts: the
    /// [[errand]]'s own Task, one per declared errand.
    ///
    /// **Not `Claim` above, and the difference is the engine's.**
    /// `claimController` spends a GCL level, needs a takeable controller and is
    /// finished the tick it succeeds; `claimReactor` spends nothing, has no
    /// cooldown, no ownership precondition, and leaves `launchTime` untouched.
    /// The act fires only on a tick the reactor is not ours; every other tick
    /// the body holds the Task and stands on the ring.
    | Reclaim of reactorId: string
    /// Getting out of a Threat's Reach. The one Task with no target and no
    /// action: its Work Area is the tiles no Threat can hurt, and the Emitter
    /// issues movement for it and nothing else.
    | Flee
    /// Killing what stands in a declared [[outpost]]: one Task per outpost a
    /// [[threat]] stands in, keyed on the **room** and never on the hostile —
    /// keyed on the hostile, a multi-creep raid would pool one Task per
    /// creep and re-match the [[guard]] between them every time one moved.
    /// Its Work Area is the walkable range-1 ring of every Threat in that
    /// room, a colony fact derived off `Threats` as [[flee]]'s safe set is.
    /// ADR-0056
    | Guard of roomName: string
