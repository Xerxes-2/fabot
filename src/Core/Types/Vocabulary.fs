/// The two vocabularies that share the engine's names, in the one file that
/// can keep them apart. `BodyPart.Claim` names a CLAIM part and `Task.Claim`
/// (ADR 0047) the act of taking a controller; a bare `Claim` means the Task,
/// and it means that because `Task` is declared second **here**. Split the two
/// across files and that guarantee goes with them — F# resolves the collision
/// by declaration order, and order only holds within one file.
[<AutoOpen>]
module Fabot.Core.Types.Vocabulary

/// A creep body part, the engine's full vocabulary. Our own bodies use only
/// Work/Carry/Move; the rest arrive on hostile creeps. `BodyPart.Claim` is
/// qualified wherever it means a part, because `Task.Claim` (ADR 0047) shares
/// the engine's name and is declared just below it.
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

/// The engine's own numbers (ADR 0052 decision 5), each named for the server
/// constant it spells. A number belongs here when changing it would be a **lie
/// about the server**, and in `Tuning` below when changing it would be a
/// **different colony**.
/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    /// Take stored energy out of a stocked container (ADR 0012), or out of the
    /// Storage a tier below them (ADR 0023) — the haul cycle's intake, judged
    /// over stores rather than energy's name.
    | Withdraw of storeId: string
    /// Walk to a dropped energy pile and take it. The Task half of what the
    /// [[pickup reflex]] does by hand: the reflex takes what is already within
    /// range 1 of a creep standing there for its own reasons, and this is what
    /// sends a creep to a pile no reflex will ever reach. Pooled on the pile's
    /// amount alone and only from a threshold (`Tuning.PickupThreshold`).
    /// Feeding tier and hauler-shaped, the same as the Withdraw beside it:
    /// which of the two an empty carrier goes for is travel cost's call.
    | Pickup of pileId: string
    /// Deliver energy into an energy-hungry structure (ADR 0010, widened by ADR
    /// 0012 and ADR 0023): a tower, the upgrade [[buffer]], the [[storage]], a
    /// [[ferry]]'s sink — and the flow's own sink, which since ADR 0054 is not
    /// a structure but a **place**: the id is the [[refill cluster]]'s spawn,
    /// and that spawn and every extension of the colony are one Task with one
    /// [[capacity]].
    | Refill of structureId: string
    | Build of siteId: string
    | Repair of structureId: string
    | Upgrade of controllerId: string
    /// Holding a neutral controller with CLAIM parts (ADR 0042): a reservation
    /// is what makes that room's sources worth the held ten a tick rather than
    /// the neutral five, and it decays by one a tick, so this is work that is
    /// never finished. One per projected controller that is not the colony's
    /// own.
    | Reserve of controllerId: string
    /// Taking a **candidate colony**'s controller for our own with CLAIM parts
    /// (ADR 0047): the act that turns a declared home room into an owned one,
    /// and so the first tick of a second colony. One per candidate colony — a
    /// declared home this colony does not own yet — and never for a plain
    /// [[outpost]], whose controller is [[reserve]]d instead: claiming costs a
    /// GCL level and asks the colony to run the room, which is a human's
    /// decision written in `Colony.declared`.
    | Claim of controllerId: string
    /// Getting out of a Threat's Reach (ADR 0033). The one Task with no
    /// target and no action: its Work Area is the tiles no Threat can
    /// hurt, and the Emitter issues movement for it and nothing else.
    | Flee
    /// Killing what stands in a declared [[outpost]] (ADR 0056): one Task per
    /// outpost a [[threat]] stands in, keyed on the **room** and never on the
    /// hostile, which is ADR 0054's split applied where it was learnt — the
    /// Planner names a place and the [[emitter]] names the target at arrival.
    /// Keyed on the hostile, the 2% multi-creep raid would pool five Tasks and
    /// re-match the [[guard]] between them every time one moved. Its Work Area
    /// is the walkable range-1 ring of every Threat standing in that room — a
    /// colony fact derived off `Threats` as [[flee]]'s safe set is, and no
    /// target's surroundings — so it is the second Task the projection places
    /// nothing for, and the one that acts anyway.
    | Guard of roomName: string
