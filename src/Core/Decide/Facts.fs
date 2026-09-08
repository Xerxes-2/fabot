/// A Task's identity across ticks, and the colony-level predicates read all over
/// the pipeline: the stage a room stands at, whether the colony is independent,
/// and which of its structures are hungry.
[<AutoOpen>]
module Fabot.Core.Decide.Facts

open Fabot.Core
open Fabot.Core.Types

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw storeId -> $"withdraw:{storeId}"
    | Refill structureId -> $"refill:{structureId}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"
    | Reserve controllerId -> $"reserve:{controllerId}"
    | Claim controllerId -> $"claim:{controllerId}"
    | Pickup pileId -> $"pickup:{pileId}"
    // One Flee for the whole colony: it has no target to be identified by,
    // and every creep inside a Reach is running from the same thing.
    | Flee -> "flee"
    // One Guard per raided room, identified by the room and never by what
    // stands in it (ADR 0056): a raid that loses a creep, or moves one a tile,
    // is the same fight and must be the same identity, or anti-thrash would
    // re-match the [[guard]] mid-swing.
    | Guard room -> $"guard:{room}"

/// The **target** inside a Task id: the engine's own object id, which is
/// everything `taskId` writes after its one colon. `None` for Flee, the one id
/// naming no target. Read by the vision grace, which holds an id and no Task —
/// the Task it named has left the pool — and stays blind to Task kinds doing
/// it (ADR 0052 decision 6): what it needs is the id the room's census files
/// that target under, and every kind spells that the same way.
let private taskTarget (tid: string) : string option =
    match tid.IndexOf ':' with
    | -1 -> None
    | colon -> Some(tid.Substring(colon + 1))

/// The vision grace's one question (#151): the room a held Task's target was
/// last seen standing in, when that room has gone **dark** inside
/// `Tuning.VisionGrace` and not longer. `None` is every other answer — the room
/// is in view this tick, it was never seen, the darkness has outlasted the
/// grace, or the id names no target at all.
///
/// A Task leaves the pool for two opposite reasons and its id alone cannot tell
/// them apart: the target was destroyed, or the room carrying it went dark. The
/// pool stays gated on vision and rightly so (ADR 0004) — a blind room hands
/// over an empty site list and no rule may price what nobody can see — so what
/// this asks is about **looking** and never about the target. Two readers, and
/// they are the two halves of one rule: the Matcher keeps the assignment, and
/// the mover walks its holder at the room the answer names (`Atlas.stepTowardRoom`).
/// Written once because a second spelling is free to disagree about which of
/// them is holding a creep still.
let internal lastSeenIn (view: ColonyView) (tid: string) : string option =
    match taskTarget tid with
    | None -> None
    | Some target ->
        view.Sightings
        |> Map.tryPick (fun room sighting ->
            if
                sighting.Tick < view.Time
                && view.Time - sighting.Tick <= view.Tuning.VisionGrace
                && Set.contains target sighting.Targets
            then
                Some room
            else
                None)

/// The [[stage]] of the colony whose home is the named room, off the one
/// derivation the shell ran for the tick (ADR 0052 decision 3). `None` for a
/// room no colony of ours lives in — undeclared, unclaimed, or one nothing
/// could see — which is the answer every reader here already gives such a room.
let internal roomStage (view: ColonyView) room = Map.tryFind room view.Stages

/// This colony's own stage: its home room's entry. Always present for a living
/// colony (ADR 0047 decision 1), so `None` is the projection that cannot place
/// its own controller, and every reader gives that colony the answer it gives
/// one standing under the line.
let internal homeStage (view: ColonyView) =
    roomStage view (SpatialInfo.homeName view.Spatial)

/// Whether this colony has outgrown its bootstrap window: `Independent`,
/// at `Tuning.BootstrapLevel` or past it (ADR 0052 decision 3). The one
/// question the Layout's two gates and the Repair pool's rampart line ask
/// — a colony below it is still buying the economy those spends serve.
let internal isIndependent (view: ColonyView) = homeStage view = Some Independent

/// Whether this colony keeps ramparts this tick (ADR 0034 as #214 amends it):
/// it is `Independent`, past the bootstrap line and one stage past the engine's
/// own unlock. The covering rule's one gate and the floor's one gate, one
/// spelling for both: a colony below it places no rampart and counts none of
/// its standing ramparts hungry, so the three a child raised at RCL2 decay away
/// instead of holding four workers to a floor derived for a home with a tower
/// and a Storage behind it.
let internal keepsRamparts (view: ColonyView) = isIndependent view

/// Whether a structure of this kind, carrying these hits, is hungry: its own
/// whole line, read off the kind (ADR 0034). The decaying kinds sit below a
/// fraction of max (ADR 0010), a rampart below the floor, the Keep below full —
/// it does not decay, so below max means damaged. A kind with no line is never
/// hungry. The floor is capped at the structure's own max so a rampart whose
/// max is somehow under it can still be whole.
let private isHungry (tuning: Tuning) kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction -> float hits.Hits < tuning.RepairTrigger * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min tuning.RampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// Every structure the projection carries hits for that stands below its kind's
/// whole line, with its kind, in id order. The one walk over the hits and the
/// kinds, shared by its two readers: the Repair pool takes all of them, the
/// safe-mode reflex's Keep arm asks only whether one is of the Keep (ADR 0034).
let internal hungryStructures (view: ColonyView) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view

    view.Spatial.Hits
    |> Map.toList
    |> List.choose (fun (id, hits) ->
        match Map.tryFind id view.Spatial.TargetKinds with
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | Some(Structure BuiltKind.Rampart) when not ramparts -> None
        | Some(Structure kind) when isHungry view.Tuning kind hits -> Some(id, kind)
        | _ -> None)

/// The range a hostile can hurt a creep from, or None for one that cannot (ADR
/// 0033).
let internal weaponRange (hostile: HostileInfo) : int option =
    [
        if List.contains Attack hostile.Body then
            Engine.meleeRange
        if List.contains RangedAttack hostile.Body then
            Engine.rangedRange
    ]
    |> function
        | [] -> None
        | ranges -> Some(List.max ranges)
