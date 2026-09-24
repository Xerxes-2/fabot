/// A Task's identity across ticks, and the colony-level predicates read all over
/// the pipeline: the stage a room stands at, whether the colony is independent,
/// and which of its structures are hungry.
[<AutoOpen>]
module Fabot.Core.Decide.Facts

open Fabot.Core
open Fabot.Core.Types

/// The resource a Task id names, and nothing at all for energy: a Task id is a
/// key carried across ticks (Memory, Assignments, every `observe` transition
/// line), so widening every `withdraw:` id would orphan every standing
/// assignment on the tick the bundle is deployed. A second resource needs a
/// distinct id at the same store, which a suffix gives it.
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
    // Identified by the object the declaration names — the engine's own id,
    // so the Memory key survives every tick the room is dark.
    | Reclaim reactorId -> $"reclaim:{reactorId}"
    | Pickup(pileId, resource) -> $"pickup:{pileId}{resourceSuffix resource}"
    // One Flee for the whole colony: every creep inside a Reach is running
    // from the same thing.
    | Flee -> "flee"
    // One Guard per raided room, identified by the room and never by what
    // stands in it: a raid that loses a creep is the same fight and must be
    // the same identity, or anti-thrash would re-match the guard mid-swing.
    | Guard room -> $"guard:{room}"

/// The target inside a Task id: what `taskId` writes between its first colon
/// and the next one (neither an object id nor a room name holds a colon).
/// `None` for Flee. Read by the vision grace, which holds an id and no Task
/// and stays blind to Task kinds.
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
/// pool stays gated on vision, so this asks about looking and never about the
/// target. Two readers, the two halves of one rule: the Matcher keeps the
/// assignment, and the mover walks its holder at the room the answer names
/// (`Atlas.stepTowardRoom`).
let internal lastSeenIn (view: ColonyView) (tid: string) : string option =
    match taskTarget tid with
    | None -> None
    | Some target ->
        view.Sightings
        |> Map.tryPick (fun room sighting ->
            if
                sighting.Tick < view.Time
                && view.Time - sighting.Tick <= view.Tuning.VisionGrace
                && Set.contains target sighting.Targets.Value
            then
                Some room
            else
                None)

/// The stage of the colony whose home is the named room, off the one
/// derivation the shell ran for the tick. `None` for a room no colony of ours
/// lives in.
let internal roomStage (view: ColonyView) room = Map.tryFind room view.Stages

/// This colony's own stage: its home room's entry. `None` is the projection
/// that cannot place its own controller, and every reader gives that colony
/// the answer it gives one standing under the line.
let internal homeStage (view: ColonyView) =
    roomStage view (SpatialInfo.homeName view.Spatial)

/// Whether this colony has outgrown its bootstrap window: `Independent`,
/// at `Tuning.BootstrapLevel` or past it.
let internal isIndependent (view: ColonyView) = homeStage view = Some Independent

/// ADR-0034. Whether this colony keeps ramparts this tick: the covering rule's
/// one gate and the floor's one gate, one spelling for both, so a colony below
/// it places no rampart and counts none of its standing ramparts hungry.
let internal keepsRamparts (view: ColonyView) = isIndependent view

/// ADR-0061. The facts the Planner reads about the assignment table, derived
/// once in `Entry` and handed down to the pool. `All` is read as
/// `Set.contains (taskId t) held.All`; `WithThorium` binds delivery persistence
/// to the living holder that still carries the paid-for load.
type HeldTaskFacts =
    {
        All: Set<string>
        WithThorium: Set<string>
    }

[<RequireQualifiedAccess>]
module HeldTaskFacts =
    let empty =
        {
            All = Set.empty
            WithThorium = Set.empty
        }

/// The Planner's narrow view of the assignment table, filtered to the living:
/// `Assignments` arrives from Memory and may name a creep that died last tick,
/// the Matcher drops those silently, but `planTasks` runs first. Not filtered
/// to task kinds: a reader that wants one kind writes the key it wants and
/// asks, as `isHungry` does.
let internal heldTaskFacts (view: ColonyView) (assignments: Assignments) : HeldTaskFacts =
    let living = view.Creeps |> List.map (fun creep -> creep.Name, creep) |> Map.ofList

    assignments
    |> Map.fold
        (fun facts name tid ->
            match Map.tryFind name living with
            | None -> facts
            | Some creep ->
                {
                    All = Set.add tid facts.All
                    WithThorium =
                        if creep.Thorium > 0 then
                            Set.add tid facts.WithThorium
                        else
                            facts.WithThorium
                })
        HeldTaskFacts.empty

/// Whether a structure of this kind, carrying these hits, is hungry: its
/// kind's line, and for the decaying kinds the held fact picks which of the
/// two. A kind with no line is never hungry. The floor is capped at the
/// structure's own max so a rampart whose max is somehow under it can still
/// be whole.
let private isHungry (tuning: Tuning) (held: Set<string>) id kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction ->
        let line =
            if Set.contains (taskId (Repair id)) held then
                tuning.RepairWholeLine
            else
                tuning.RepairTrigger

        float hits.Hits < line * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min tuning.RampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// Every structure the projection carries hits for that stands below its kind's
/// line, with its kind, in id order — the Repair pool's own walk. The
/// safe-mode reflex's Keep arm asks `keepDamaged` instead.
let internal hungryStructures (view: ColonyView) (held: Set<string>) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view

    SpatialInfo.structureHits view.Spatial
    |> List.choose (fun (id, kind, hits) ->
        match kind with
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | BuiltKind.Rampart when not ramparts -> None
        | _ when isHungry view.Tuning held id kind hits -> Some(id, kind)
        | _ -> None)

/// Whether any Keep structure of this colony stands below full hits: the
/// safe-mode reflex's own question. Its own predicate and not a filter over
/// `hungryStructures`, because this arm needs no held set. A second walk over
/// a hundred-odd structure hits, which is not a flood (#171).
let internal keepDamaged (view: ColonyView) : bool =
    SpatialInfo.structureHits view.Spatial
    |> List.exists (fun (_, kind, hits) -> isKeep kind && hits.Hits < hits.HitsMax)

/// ADR-0033. The range a hostile can hurt a creep from, or None for one that
/// cannot.
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

/// Whether a hostile can hurt anything at all: `weaponRange` asked as a yes/no.
/// Written once because four rules turn on it: whose hits the guard beat
/// counts, whether a room stands down, whether safe mode fires, and whether a
/// Reach is derived at all.
let internal isArmed (hostile: HostileInfo) : bool = weaponRange hostile |> Option.isSome

/// Whether a projected target stands in a room this player owns, which is how
/// every rule of the season's ore answers "is this ours?" (#261, #311).
/// `FIND_MINERALS`, `FIND_STRUCTURES` and `FIND_DROPPED_RESOURCES` all carry
/// every owner's, and nothing in the shape of a deposit, an extractor, a
/// container or a pile says who put it there; the engine says it once, about
/// the room (an extractor needs an owned RCL6 room). For a pile it says only
/// that the floor under it is ours, which is the only question a Pickup asks.
/// A room the colony cannot see owns nothing here.
let internal inARoomWeOwn (view: ColonyView) (id: string) : bool =
    SpatialInfo.roomOf view.Spatial id
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// The Thorium deposits standing in a room this colony owns, in id order — the
/// list both the Task pool and the miner row's quota are read off. A scanned
/// neighbour arrives in the projection with its own deposit, extractor and
/// container, and `harvest` refuses a mineral whose extractor belongs to
/// somebody else: one `ERR_NOT_OWNER` a tick for the whole of a body's life.
let internal ourDeposits (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Mineral |> List.filter (inARoomWeOwn view)

/// The rooms this colony has declared an errand in, as a set — the join four
/// rules of the season's ore make, written once (#378).
let internal errandRooms (view: ColonyView) : Set<string> =
    view.Errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

/// The errand rooms a rival's CLAIM body stands in this tick (#406). The flag
/// may be ours this tick, but `claimReactor` has no precondition, so a load
/// put in now burns for whoever holds the flag next. An armed rival is the
/// stand-down's (`Observe.raidDeadlines`); this is the unarmed half. Off
/// vision alone: no load is drawn without a re-claimer resident to see.
let internal errandRoomsContested (view: ColonyView) : Set<string> =
    errandRooms view
    |> Set.filter (fun room ->
        view.Hostiles
        |> List.exists (fun hostile ->
            hostile.Pos.Room = room && List.contains BodyPart.Claim hostile.Body))

/// The errand rooms a delivery may be drawn for and poured into this tick:
/// the declared ones less the contested.
let internal errandRoomsDeliverable (view: ColonyView) : Set<string> =
    Set.difference (errandRooms view) (errandRoomsContested view)

/// Whether decaying ore standing at this target is this colony's to sweep: a
/// room we own (#311), a room we declared an errand in (#354: the Reactor's
/// room has no controller, so "a room we own" made its floor belong to nobody
/// while 915 T bled on it), or a room a chain of ours crosses (#360). Written
/// once for the piles, the tombstones and the breach channel that alarms on
/// both.
let private oursToSweep (view: ColonyView) (id: string) : bool =
    let errandRooms = errandRooms view

    inARoomWeOwn view id
    || SpatialInfo.roomOf view.Spatial id
       |> Option.exists (fun room ->
           Set.contains room errandRooms
           // `view.Crossed` and not "any room in the projection": a stranger's
           // room can be in a scan set without being on a chain, and its floor
           // is not ours to walk onto. `ColonyView.transiting` admits exactly
           // two kinds out of a crossed room — a pile and a tombstone.
           || Set.contains room view.Crossed)

/// The Thorium on the ground in a room this colony sweeps, in id order (#311):
/// the dig that landed on the floor because the container was at its 2,000
/// cap. Off the projection's kind census and not off the deposits: a mine
/// whose container is destroyed mid-haul goes on dropping onto a tile the
/// deposit census would still name but the container census no longer does.
let internal ourThoriumPiles (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial (Dropped Thorium)
    |> List.filter (oursToSweep view)

/// The Thorium in a store with a clock on it — a tombstone or a ruin —
/// standing in a room this colony sweeps, in id order (#359). The engine's
/// `withdraw` takes a tombstone or a ruin for any resource (`@screeps/engine`
/// `src/game/creeps.js`), and a tombstone that decays drops its whole store
/// as piles (`processor/intents/tombstones/tick.js`), where `ourThoriumPiles`
/// picks the story up. The kind carries no resource, so the holding selects.
let internal ourThoriumTombstones (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Tombstone
    |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0 && oursToSweep view id)

/// Whether a Reactor this colony has declared has room for a whole load
/// (#354): what gates a new draw at the Storage, and the programme's only
/// regulator of supply, since the Reactor burns exactly 1 T a tick and a
/// cadence cannot meter that.
///
/// Read on the store as it stands plus every unit of ore already aboard a body
/// of ours (#362): the store alone is right for one carrier and wrong for two
/// (live at t501,501 a hauler landed 196 T it could not put down behind a
/// courier's 500, and stood on the Reactor's tile for 152 ticks). Counting all
/// ore afloat is deliberately conservative: a mine hauler walking ore home is
/// counted too, so a colony that mines and delivers sometimes defers a draw by
/// one haul cycle. Naming which body is inbound would mean reading the
/// `Assignments` map the Planner is blind to.
///
/// A Reactor we cannot see answers `false`, which leaves the load banked at
/// home. Read off `view.Reactors` and not off `SpatialInfo.Thorium`, which
/// deliberately does not carry this store: the first version asked the wrong
/// map, answered 0 for a Reactor holding 999, and the unit test agreed because
/// the fixture wrote the store where the gate looked.
let internal reactorTakesALoad (view: ColonyView) (load: int) : bool =
    let afloat = view.Creeps |> List.sumBy (fun creep -> creep.Thorium)

    view.Errands
    |> List.exists (fun errand ->
        let reactorId = fst errand.Target

        view.Reactors
        |> List.exists (fun reactor ->
            reactor.Id = reactorId
            && reactor.Thorium + afloat + load <= Engine.reactorCapacity))

/// The mineral containers of this colony's own deposits, in deposit order: the
/// built container within range 1 of a deposit in the deposit's own room (a
/// `Pos` names no room, and a container on the same coordinate of an outpost
/// is not this deposit's). Built alone and never a site: a site holds nothing
/// (#261). Off `ourDeposits` and never off the container census, which
/// carries every owner's. Paired with the deposit because the two readers ask
/// different questions of the same join (#262).
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

/// Whether a deposit of ours is one the colony can actually dig this tick: it
/// still holds Thorium, and its extractor stands. Only the digger reads this
/// (#361): the carrier's load comes out of a Storage, which outlives the
/// deposit that filled it. A site is not an extractor: `harvest.js` refuses a
/// mineral with none on it.
let internal depositIsDiggable (view: ColonyView) atlas (depositId: string) =
    Map.tryFind depositId view.Spatial.Thorium |> Option.defaultValue 0 > 0
    && (Atlas.extractorOn atlas depositId).IsSome

/// The energy standing in this colony's Storages — its stock, as against
/// `ColonyView.Bank`'s spawn account. Read by the worker row's backlog term
/// (#364), which is paid out of the stock and not out of income. Folded over
/// every Storage the projection holds rather than the home room's alone: a
/// rule that assumed one would be silently wrong in the colony that first has
/// two.
let internal stockedEnergy (view: ColonyView) : int =
    view.Spatial.TargetKinds
    |> Map.fold
        (fun total id kind ->
            if kind = Structure BuiltKind.Storage then
                total + SpatialInfo.storedIn view.Spatial id
            else
                total)
        0

/// Whether more ore is still on its way to the Storage: a diggable deposit of
/// ours, or ore already standing in a mineral container. The question the last
/// load turns on.
let internal oreStillComing (view: ColonyView) atlas : bool =
    ourDeposits view |> List.exists (depositIsDiggable view atlas)
    || ourMineralContainers view
       |> List.exists (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)

/// The most ore any one Storage of this colony banks — a maximum and not a
/// sum, because the draw this feeds is one body at one store.
let internal mostOreInAStorage (view: ColonyView) : int =
    view.Spatial.TargetKinds
    |> Map.fold
        (fun most id kind ->
            if kind = Structure BuiltKind.Storage then
                max most (SpatialInfo.heldIn view.Spatial Thorium id)
            else
                most)
        0

/// ADR-0073. What the next delivery draw takes, and 0 when there is none to
/// take: a whole `Tuning.ReactorLoad` while the mine still feeds the bank, and
/// the remainder once it does not.
let internal deliveryLoad (view: ColonyView) atlas : int =
    let banked = mostOreInAStorage view

    if banked >= view.Tuning.ReactorLoad then
        view.Tuning.ReactorLoad
    elif banked > 0 && not (oreStillComing view atlas) then
        banked
    else
        0

/// Whether the ore a body holds is a delivery's — drawn for the Reactor and
/// not hauled out of a mine. Three ways to be one: the exact `ReactorLoad`,
/// this tick's own load, or, once nothing is left to dig or haul, any ore
/// aboard — a remainder taken on one tick is measured against a load of 0 on
/// the next. Read by the Planner, which pools a sink, and by nothing that
/// refuses one.
let internal carryingADelivery
    (view: ColonyView)
    (load: int)
    (stillComing: bool)
    (creep: CreepInfo)
    =
    creep.Thorium > 0
    && (creep.Thorium = view.Tuning.ReactorLoad
        || creep.Thorium = load
        || (not stillComing && not (List.isEmpty view.Errands)))

/// Whether ore is lying in a declared errand room — a pile or a tombstone
/// beside the Reactor itself.
let internal oreBesideTheReactor (view: ColonyView) : bool =
    ourThoriumTombstones view @ ourThoriumPiles view
    |> List.exists (fun id ->
        SpatialInfo.roomOf view.Spatial id
        |> Option.exists (fun room -> Set.contains room (errandRooms view)))

/// Whether the season's delivery programme has all of its current ground
/// facts (#319, narrowed by #361): a Storage holding a load, and a resident
/// CLAIM body at a declared Reactor. No remembered switch — each fact closes
/// the row when it disappears. A diggable mine is deliberately not a
/// condition: Thorium never regenerates, so every deposit ends mined out with
/// its ore in a Storage, and the tick the mine ran dry this row closed and the
/// Reactor burned down with 7,226 T banked and a 7,989-tick streak standing
/// (#361).
let internal courierProgrammeOpen (view: ColonyView) atlas =
    let hasLoad = deliveryLoad view atlas > 0
    let deliverable = errandRoomsDeliverable view

    let hasResidentReclaimer =
        view.Creeps
        |> List.exists (fun creep ->
            partCount creep.Body BodyPart.Claim > 0
            && (Atlas.creepTile atlas creep.Name
                |> Option.exists (fun tile -> Set.contains tile.Room deliverable)))

    view.Bank.Capacity >= bodyCost courierPattern.Block
    && hasLoad
    && hasResidentReclaimer
