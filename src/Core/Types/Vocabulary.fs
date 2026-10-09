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

/// Whether two parts are one kind, matched case by case: `=` on a union is
/// Fable's generic `equals`, and the body scans below ask it of every part of
/// every hostile, many times a tick.
let samePart (a: BodyPart) (b: BodyPart) : bool =
    match a, b with
    | Work, Work
    | Carry, Carry
    | Move, Move
    | Attack, Attack
    | RangedAttack, RangedAttack
    | Heal, Heal
    | Claim, Claim
    | Tough, Tough -> true
    | _ -> false

/// The boost one part carries, as far as a fight reads it: the tier of a
/// compound on the part's own combat act (`BOOSTS`) — ATTACK's swing,
/// RANGED_ATTACK's shot and mass attack, HEAL's heals, TOUGH's damage taken,
/// MOVE's fatigue, WORK's dismantle — or none. A WORK boost for harvest,
/// build or upgrade and a CARRY boost are `Unboosted`: no fight reads them.
type Boost =
    | Unboosted
    | Tier1
    | Tier2
    | Tier3

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Boost =
    /// The boost a compound gives a part, as the engine spells the compound
    /// (`BOOSTS`): Unboosted for none, or one the part's fight act ignores.
    let ofCompound (part: BodyPart) (compound: string) : Boost =
        match part, compound with
        | Attack, "UH"
        | RangedAttack, "KO"
        | Heal, "LO"
        | Tough, "GO"
        | Move, "ZO"
        | Work, "ZH" -> Tier1
        | Attack, "UH2O"
        | RangedAttack, "KHO2"
        | Heal, "LHO2"
        | Tough, "GHO2"
        | Move, "ZHO2"
        | Work, "ZH2O" -> Tier2
        | Attack, "XUH2O"
        | RangedAttack, "XKHO2"
        | Heal, "XLHO2"
        | Tough, "XGHO2"
        | Move, "XZHO2"
        | Work, "XZH2O" -> Tier3
        | _ -> Unboosted

    /// How many unboosted parts of its kind one part acts as: ×2/×3/×4 a tier
    /// on ATTACK, RANGED_ATTACK, HEAL, MOVE and (dismantling) WORK, as
    /// `calcBodyEffectiveness` multiplies them; 1 for the rest, TOUGH
    /// included, whose boost is on the damage it takes (`damageRatio`).
    let multiple (part: BodyPart) (boost: Boost) : int =
        match part, boost with
        | (Attack | RangedAttack | Heal | Move | Work), Tier1 -> 2
        | (Attack | RangedAttack | Heal | Move | Work), Tier2 -> 3
        | (Attack | RangedAttack | Heal | Move | Work), Tier3 -> 4
        | _ -> 1

    /// The share of a strike's damage a part takes in its own hits: 0.7, 0.5
    /// and 0.3 on a boosted TOUGH, 1 on any other part.
    let damageRatio (part: BodyPart) (boost: Boost) : float =
        match part, boost with
        | Tough, Tier1 -> 0.7
        | Tough, Tier2 -> 0.5
        | Tough, Tier3 -> 0.3
        | _ -> 1.0

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

/// The guard cut, over parts: an ATTACK part, the one cut no other row of
/// ours makes. One predicate for a living body and one still in an oven, and
/// for the defender reading which of its bodies still stand on a defended
/// chain (`World.defends`), so none of them can drift.
let isGuardParts (parts: Map<BodyPart, int>) = partCount parts Attack > 0

/// How many of one part a body still in the oven holds, counted off the list
/// itself: for the rules that ask about a single part, where building a map
/// to read one key out of it is the dearer spelling.
let partCountIn (body: BodyPart list) part =
    let mutable count = 0

    for each in body do
        if samePart each part then
            count <- count + 1

    count

/// A room's ordinary **[[ore]]**: the seven base minerals a `FIND_MINERALS`
/// deposit can hold beside the season's Thorium. The compounds a lab makes
/// out of them are not ore and are not here.
type Ore =
    | Hydrogen
    | Oxygen
    | Utrium
    | Keanium
    | Lemergium
    | Zynthium
    | Catalyst

/// Every Ore — the closed set, closed by `Core.Tests` as `allBodyParts` is.
let allOres = [ Hydrogen; Oxygen; Utrium; Keanium; Lemergium; Zynthium; Catalyst ]

/// A **[[compound]]**: what the [[lab cluster]] makes out of [[ore]]. The UH
/// line alone, the one boost line our own ore reaches; a compound joins the
/// day a decision of ours names it.
type Compound =
    /// OH, hydroxide: H and O. It boosts nothing; UH2O's second reagent.
    | Hydroxide
    /// UH, utrium hydride: U and H. ATTACK ×2.
    | UtriumHydride
    /// UH2O, utrium acid: UH and OH. ATTACK ×3.
    | UtriumAcid

/// Every Compound — the closed set, closed by `Core.Tests` as `allOres` is.
let allCompounds = [ Hydroxide; UtriumHydride; UtriumAcid ]

/// What a store holds, as this colony reads it: the energy every Task in the
/// pool is about, the season's Thorium beside it (ADR-0057), the room's own
/// ore, dug into the terminal to a target, and the compounds the lab cluster
/// makes of it.
///
/// Not the engine's whole `RESOURCE_*` table: a resource belongs here when a
/// decision of ours names it. Declared beside the body parts because it is the
/// engine's vocabulary: `withdraw` and `transfer` have taken one of these
/// strings all along.
type Resource =
    | Energy
    | Thorium
    | Ore of Ore
    | Compound of Compound

/// Whether a resource is a **mineral** in a store's sense — an ore or a
/// compound — the resources a lab holds and `SpatialInfo.Minerals` files.
let isMineralResource =
    function
    | Ore _
    | Compound _ -> true
    | Energy
    | Thorium -> false

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
    /// Taking a structure down with WORK parts: one per ownable structure
    /// standing in a declared [[salvage]] room. Named for the act and not for
    /// the room, so a later pool may name a structure of another kind.
    | Dismantle of structureId: string
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
    /// Holding the home's perimeter with its own [[defender]]s (#504): one
    /// Task per home a raid stands outside the line of, keyed on the room for
    /// the Guard's reason. Its Work Area is the free perimeter ramparts beside
    /// the raid, and it is held by defender casts alone; for the raiders their
    /// swing does not reach, by rangers too, within three of those (#546).
    | Defend of roomName: string
    /// Breaking into a rival's room with a [[squad]] (#490): one Task per
    /// declared [[assault]] a human has switched on, held by the role bodies
    /// its capacity counts. Its Work Area is the rally ground until the squad
    /// launches, then beside the structure its sappers take down.
    | Assault of roomName: string
    /// Working an [[operation]]'s room (#663 S7c): one Task per operation,
    /// keyed on the operation and never on the room — its id is `op:<id>`,
    /// so a body holds it across a move of the room the operation is
    /// committed to. The room rides beside the key: the room this tick's
    /// fight is in, its Work Area its Brief's, as a Guard's is.
    | Operate of opId: string * roomName: string
