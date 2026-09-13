/// A Task's identity across ticks, and the colony-level predicates read all over
/// the pipeline: the stage a room stands at, whether the colony is independent,
/// and which of its structures are hungry.
[<AutoOpen>]
module Fabot.Core.Decide.Facts

open Fabot.Core
open Fabot.Core.Types

/// The resource a Task id names, and **nothing at all for energy** (ADR 0057
/// decision 3). A Task id is a key carried across ticks — in Memory, in the
/// Assignments the anti-thrash reads back, and in every `observe` transition
/// line — so the spelling of the colony's energy work is frozen where it was:
/// widening every `withdraw:` id would orphan every standing assignment on the
/// tick the bundle carrying it is deployed, for a resource no store in this
/// colony held until the extractor stood. What a second resource needs is a
/// *distinct* id at the same store, which a suffix gives it.
let private resourceSuffix =
    function
    | Energy -> ""
    | Thorium -> ":Thorium"

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw(storeId, resource) -> $"withdraw:{storeId}{resourceSuffix resource}"
    | Refill(structureId, resource) -> $"refill:{structureId}{resourceSuffix resource}"
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

/// The **target** inside a Task id: the engine's own object id, which is what
/// `taskId` writes between its first colon and the next one — a Task id has two
/// colons since ADR 0057 decision 3 put a resource on the [[withdraw]] and the
/// [[refill]], and neither an object id nor a room name has ever held one.
/// `None` for Flee, the one id naming no target. Read by the vision grace,
/// which holds an id and no Task —
/// the Task it named has left the pool — and stays blind to Task kinds doing
/// it (ADR 0052 decision 6): what it needs is the id the room's census files
/// that target under, and every kind spells that the same way.
let private taskTarget (tid: string) : string option =
    match tid.IndexOf ':' with
    | -1 -> None
    | colon ->
        let rest = tid.Substring(colon + 1)

        match rest.IndexOf ':' with
        | -1 -> Some rest
        | next -> Some(rest.Substring(0, next))

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

    SpatialInfo.structureHits view.Spatial
    |> List.choose (fun (id, kind, hits) ->
        match kind with
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | BuiltKind.Rampart when not ramparts -> None
        | _ when isHungry view.Tuning kind hits -> Some(id, kind)
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

/// Whether a hostile can hurt anything at all — `weaponRange` asked as a
/// yes/no (ADR 0033), and the one cut between a raider and a scout or a lone
/// healer. Written here because four rules turn on it and each used to own its
/// own opinion of what a weapon is: whose hits the guard beat counts, whether
/// a room stands down, whether safe mode fires, and whether a Reach is derived
/// at all.
let internal isArmed (hostile: HostileInfo) : bool = weaponRange hostile |> Option.isSome

/// The [[thorium]] deposits standing in a room this colony **owns** (ADR 0057
/// decision 2), in id order — the only deposits any rule of this colony may
/// answer for, and the list both the Task pool and the [[miner]] row's quota are
/// read off so the two can never disagree about which rock is ours.
///
/// **Whose the deposit is, is whose the room is** (#261). `FIND_MINERALS` and
/// `FIND_STRUCTURES` both carry every owner's, so a scanned neighbour arrives in
/// the projection with its own deposit, its own extractor and its own container,
/// and nothing in the shape of those three facts says who built them. The engine
/// does say: `harvest` refuses a mineral whose extractor belongs to somebody
/// else, one `ERR_NOT_OWNER` a tick for the whole of a body's life. The room is
/// the honest join and not a second fact — an extractor needs an **owned** RCL6
/// room (`checkControllerAvailability` derives `rcl = 0` from a reservation), so
/// an extractor in a room we own is ours and one in a room we do not is not.
/// A room the colony cannot see owns nothing here (ADR 0004).
let internal ourDeposits (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Mineral
    |> List.filter (fun id ->
        SpatialInfo.roomOf view.Spatial id
        |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
        |> Option.exists (fun control -> control.Owner = Ownership.Ours))

/// The **mineral [[container]]s** of this colony's own deposits, in deposit
/// order (ADR 0057 decision 3): the built container standing on a deposit's
/// [[seat]] — the mine [[post]] the store-less [[miner]] digs from, whose drop
/// the engine lands *in* the container it is standing on. The one Thorium store
/// this colony ever draws, and the term the [[hauler unit]]'s demand sum grows
/// by, read here once so the Task pool and the quota cannot come to disagree
/// about which container is a mine's.
///
/// Range 1 of the deposit **in the deposit's own room**, which is the mine Post
/// said without the [[atlas]]: a Seat is a walkable neighbour and a container
/// stands only on one, so the two censuses name the same tile. The room join is
/// load-bearing for the reason ADR 0041 gives — a `Pos` names no room, and a
/// container on the same coordinate of an [[outpost]] is not this deposit's. A
/// **built** container alone and never a site: a site holds nothing, and the
/// Post does not exist until the container stands (#261).
///
/// Off `ourDeposits` and never off the container census, for that list's own
/// reason: `FIND_STRUCTURES` carries every owner's containers, and a scanned
/// neighbour's mine is exactly as visible as ours.
///
/// The **deposit beside the container**, because the two readers ask different
/// questions of the same join (#262): the Task pool wants the store, and the
/// [[hauler unit]]'s demand term wants to know whether the rock behind it can be
/// dug at all. `ourMineralContainers` is this list with the deposit dropped.
let internal ourMineralContainerPairs (view: ColonyView) : (string * string) list =
    let containers =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Container)
        |> List.choose (fun id ->
            SpatialInfo.placementOf view.Spatial id |> Option.map (fun tile -> id, tile))

    ourDeposits view
    |> List.choose (fun depositId ->
        match SpatialInfo.placementOf view.Spatial depositId with
        | None -> None
        | Some deposit ->
            containers
            |> List.filter (fun (_, tile) ->
                tile.Room = deposit.Room && range (RoomPos.pos tile) (RoomPos.pos deposit) <= 1)
            // Ties by id, the way every other tie in this colony falls: a
            // deposit the Layout ever seated two containers beside answers with
            // one of them and not with both.
            |> List.map fst
            |> List.sort
            |> List.tryHead
            |> Option.map (fun containerId -> depositId, containerId))

let internal ourMineralContainers (view: ColonyView) : string list =
    ourMineralContainerPairs view |> List.map snd

/// Whether a deposit of ours is one the colony can **actually dig this tick**
/// (ADR 0057 decision 2): it still holds Thorium, and its extractor **stands**.
/// The two conditions the [[miner]] row's own quota puts a body at 0 for, written
/// once so the row that hires the digger and the term that hires the carrier
/// cannot disagree about whether there is a mine (#262). The row adds the third,
/// the mine [[post]], which the container census the haul term reads has already
/// answered. A site is not an extractor: `harvest.js` refuses a mineral with
/// none on it, so a deposit under one produces nothing and the haul priced
/// against it is a hauler hired for a trip nobody ever makes.
let internal depositIsDiggable (view: ColonyView) atlas (depositId: string) =
    Map.tryFind depositId view.Spatial.Thorium |> Option.defaultValue 0 > 0
    && (Atlas.extractorOn atlas depositId).IsSome
