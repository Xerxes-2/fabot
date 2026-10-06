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
    | Ore _
    | Compound _ as mineral -> $":{resourceName mineral}"

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
    | Dismantle structureId -> $"dismantle:{structureId}"
    | Pickup(pileId, resource) -> $"pickup:{pileId}{resourceSuffix resource}"
    // One Flee for the whole colony: every creep inside a Reach is running
    // from the same thing.
    | Flee -> "flee"
    // One Guard per raided room, identified by the room and never by what
    // stands in it: a raid that loses a creep is the same fight and must be
    // the same identity, or anti-thrash would re-match the guard mid-swing.
    | Guard room -> $"guard:{room}"
    | Defend room -> $"defend:{room}"
    | Assault room -> $"assault:{room}"

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

/// The store a creep's energy came from (#530): the store of the energy
/// Withdraw it held last tick, once it has any aboard, else the one Memory
/// recorded, and nothing once it carries none. Read off the assignment and not
/// the act, so a draw a light tick made is recorded on the next full one.
let internal drawnFrom
    (view: ColonyView)
    (assignments: Assignments)
    (creep: CreepInfo)
    : DrawRecord option =
    // Beside the store it holds a Withdraw of: a body still walking there has
    // drawn nothing from it yet.
    let beside store =
        match
            SpatialInfo.creepPlacementOf view.Spatial creep.Name,
            SpatialInfo.placementOf view.Spatial store
        with
        | Some at, Some tile -> RoomPos.range at tile |> Option.exists (fun r -> r <= 1)
        | _ -> false

    // What last tick's Task says about the load: a draw beside a store records
    // it, any other intake mixes the load and clears the record, and anything
    // else leaves the record as it was.
    let held =
        Map.tryFind creep.Name assignments
        |> Option.map (fun tid ->
            match taskTarget tid with
            | Some store when tid = taskId (Withdraw(store, Energy)) ->
                if beside store then
                    Some(Some { Store = store; Tick = view.Time })
                else
                    None
            | _ when tid.StartsWith "pickup:" || tid.StartsWith "harvest:" -> Some None
            | _ -> None)
        |> Option.flatten

    if creep.Energy = 0 then
        None
    else
        match held with
        | Some record -> record
        // The record expires: it breaks the flip, and must never strand a body
        // whose only sink is the store it drew from.
        | None ->
            creep.DrawnFrom
            |> Option.filter (fun record -> view.Time - record.Tick <= view.Tuning.DrawnHold)

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

/// Whether a body's life covers a walk and `stay` ticks of work beyond it —
/// the one in-time sum, for the pool's child-room gate and the anchor row's
/// census alike. A walk the Atlas cannot price refuses nobody.
let internal outlivesWalk (creep: CreepInfo) stay (walk: int option) =
    walk |> Option.forall (fun ticks -> creep.TicksToLive >= ticks + stay)

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
/// at `Tuning.BootstrapLevel` or past it — or `Weaning`, whose tower holds
/// only its mother's lend and none of its own rules (#445).
let internal isIndependent (view: ColonyView) =
    match homeStage view with
    | Some Independent
    | Some Weaning -> true
    | Some Nursery
    | Some Bootstrapping
    | None -> false

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
        /// Each held Task id's living holders, by name.
        Holders: Map<string, string list>
    }

[<RequireQualifiedAccess>]
module HeldTaskFacts =
    let empty =
        {
            All = Set.empty
            WithThorium = Set.empty
            Holders = Map.empty
        }

    /// One Task id's living holders.
    let holdersOf (held: HeldTaskFacts) (tid: string) : string list =
        Map.tryFind tid held.Holders |> Option.defaultValue []

/// Whether a structure of this kind, carrying these hits, is hungry: its
/// kind's line, and for the decaying kinds the held fact picks which of the
/// two. A kind with no line is never hungry. The floor is the one handed in
/// (`floorOf`), capped at the structure's own max so a rampart whose max is
/// somehow under it can still be whole.
let private isHungry (tuning: Tuning) (floor: int) (held: Set<string>) id kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction ->
        let line =
            if Set.contains (taskId (Repair id)) held then
                tuning.RepairWholeLine
            else
                tuning.RepairTrigger

        float hits.Hits < line * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min floor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// The home room's tile of one of its structures, or None for one placed
/// nowhere or in another room.
let private homeTile (view: ColonyView) (id: string) : RoomPos option =
    let home = SpatialInfo.homeName view.Spatial

    Map.tryFind id (SpatialInfo.layerOf view.Spatial home).TargetPositions
    |> Option.map (RoomPos.at home)

/// The home's ramparts of ours under attack this tick (#467): a hostile's
/// weapon reaches the tile, or a WORK body stands beside it to dismantle.
/// Read off where the hostiles stand, because the projection carries no
/// hits from the tick before to see them falling.
let internal rampartsUnderAttack (view: ColonyView) : Set<string> =
    match view.Hostiles with
    | [] -> Set.empty
    | hostiles ->
        let strikes (tile: RoomPos) (hostile: HostileInfo) =
            match RoomPos.range hostile.Pos tile with
            | Some r ->
                HostileInfo.weaponRange hostiles hostile
                |> Option.exists (fun reach -> r <= reach)
                || r <= Engine.meleeRange && HostileInfo.works hostiles hostile
            | None -> false

        SpatialInfo.structureHits view.Spatial
        |> List.choose (fun (id, kind, _) ->
            match kind with
            | BuiltKind.Rampart ->
                homeTile view id
                |> Option.filter (fun tile -> hostiles |> List.exists (strikes tile))
                |> Option.map (fun _ -> id)
            | _ -> None)
        |> Set.ofList

/// Whether a structure is a dying rampart: under `Tuning.RampartDying` hits,
/// built at 1 and decaying away within a hundred ticks.
let internal isDyingRampart (view: ColonyView) (id: string) =
    match Map.tryFind id view.Spatial.TargetKinds, Map.tryFind id view.Spatial.Hits with
    | Some(Structure BuiltKind.Rampart), Some hits -> hits.Hits < view.Tuning.RampartDying
    | _ -> false

/// The construction sites standing in the home, an unplaced one reading as
/// home as the surplus rung's does.
let private homeSites (view: ColonyView) =
    let home = SpatialInfo.homeName view.Spatial

    view.ConstructionSites
    |> List.filter (fun site ->
        SpatialInfo.roomOf view.Spatial site.Id
        |> Option.forall (fun room -> room = home))

/// The progress the home's sites still owe.
let internal homeSitesOwed (view: ColonyView) =
    homeSites view |> List.sumBy (fun site -> site.Left)

/// Whether a site of this kind stands in the home.
let internal homeSiteStands (view: ColonyView) (kind: BuiltKind) =
    homeSites view
    |> List.exists (fun site -> Map.tryFind site.Id view.Spatial.TargetKinds = Some(Site kind))

/// Whether the home is in its build phase (#526): an extension, Storage or
/// tower site stands there. An outpost road waits on it (#548).
let internal homeBuilding (view: ColonyView) =
    homeSiteStands view BuiltKind.Extension
    || homeSiteStands view BuiltKind.Storage
    || homeSiteStands view BuiltKind.Tower

/// Whether a raider stands in a room (`HostileInfo.raider`).
let internal raided (view: ColonyView) (room: string) : bool =
    view.Hostiles
    |> List.exists (fun h -> h.Pos.Room = room && HostileInfo.raider view.Hostiles h)

/// The **build floor** (#526): `Tuning.BuildRampartFloor`, capping every
/// rampart floor while an extension or Storage site stands in a quiet home;
/// None, the full floors, while a raider stands at home or its healers are
/// remembered there, and for `Tuning.BuildFloorRaidTicks` after a raider
/// last stood there (`ColonyView.HomeRaided`).
let internal buildFloor (view: ColonyView) : int option =
    let home = SpatialInfo.homeName view.Spatial

    let building =
        homeSiteStands view BuiltKind.Extension || homeSiteStands view BuiltKind.Storage

    let raidedLately =
        raided view home
        || view.RecalledHealers |> List.exists (fun h -> h.Pos.Room = home)
        || view.HomeRaided
           |> Option.exists (fun tick -> view.Time - tick < view.Tuning.BuildFloorRaidTicks)

    if building && not raidedLately then
        Some view.Tuning.BuildRampartFloor
    else
        None

/// A floor under the build floor's cap, when there is one.
let private capped (cap: int option) floor =
    cap |> Option.fold (fun floor cap -> min floor cap) floor

/// The floor a structure of the home is whole at: its tile's declared one
/// (`ColonyView.RampartFloors`), else `Tuning.RampartFloor`, under `cap`.
let private floorOf (view: ColonyView) (cap: int option) (id: string) =
    if Map.isEmpty view.RampartFloors then
        capped cap view.Tuning.RampartFloor
    else
        homeTile view id
        |> Option.bind (fun tile -> Map.tryFind (RoomPos.pos tile) view.RampartFloors)
        |> Option.defaultValue view.Tuning.RampartFloor
        |> capped cap

/// Every structure the projection carries hits for that stands below its kind's
/// line, with its kind, in id order — the Repair pool's own walk. The
/// safe-mode reflex's Keep arm asks `keepDamaged` instead.
let internal hungryStructures (view: ColonyView) (held: Set<string>) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view
    let attacked = rampartsUnderAttack view
    let cap = buildFloor view

    SpatialInfo.structureHits view.Spatial
    |> List.choose (fun (id, kind, hits) ->
        match kind with
        // A rampart under attack is hungry to its max, kept or not: it is
        // what stands between the raid and the room.
        | BuiltKind.Rampart when Set.contains id attacked ->
            if hits.Hits < hits.HitsMax then Some(id, kind) else None
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | BuiltKind.Rampart when not ramparts -> None
        // Nor is a child's: the mother carries it as ground, not work.
        | _ when
            isHungry view.Tuning (floorOf view cap id) held id kind hits
            && not (kind = BuiltKind.Rampart && SpatialInfo.placedAway view.Spatial id)
            ->
            Some(id, kind)
        | _ -> None)

/// The standing ramparts of ours on the home's declared perimeter, by id,
/// tile and hits: never a Keep's or a Post container's.
let internal perimeterRamparts (view: ColonyView) : (string * RoomPos * int) list =
    match view.Perimeter with
    | [] -> []
    | perimeter ->
        let line = Set.ofList perimeter

        SpatialInfo.structureHits view.Spatial
        |> List.choose (fun (id, kind, hits) ->
            if kind = BuiltKind.Rampart then
                homeTile view id
                |> Option.filter (fun tile -> Set.contains (RoomPos.pos tile) line)
                |> Option.map (fun tile -> id, tile, hits.Hits)
            else
                None)

/// The declared line a raid breaks: its ramparts under nine tenths of their
/// floor, and the rampart sites on its tiles, by id. Empty in a colony that
/// keeps no ramparts or declared no line. Read by the pool's line rung and by
/// `Planner.busyReasons`, so the two name one line.
let internal brokenLine (view: ColonyView) : Set<string> * Set<string> =
    if not (keepsRamparts view) || List.isEmpty view.Perimeter then
        Set.empty, Set.empty
    else
        let home = SpatialInfo.homeName view.Spatial
        let line = Set.ofList view.Perimeter

        let cap = buildFloor view

        let floorAt tile =
            Map.tryFind tile view.RampartFloors
            |> Option.defaultValue view.Tuning.RampartFloor
            |> capped cap

        // Nine tenths: decay and the repair that stops at the floor keep a
        // maintained line just under it (live W12S28: 99,401-99,901).
        let weak =
            perimeterRamparts view
            |> List.choose (fun (id, tile, hits) ->
                let floor = floorAt (RoomPos.pos tile)
                if hits < floor - floor / 10 then Some id else None)
            |> Set.ofList

        let sites =
            view.ConstructionSites
            |> List.choose (fun site ->
                if Map.tryFind site.Id view.Spatial.TargetKinds = Some(Site BuiltKind.Rampart) then
                    SpatialInfo.placementOf view.Spatial site.Id
                    |> Option.filter (fun tile ->
                        tile.Room = home && Set.contains (RoomPos.pos tile) line)
                    |> Option.map (fun _ -> site.Id)
                else
                    None)
            |> Set.ofList

        weak, sites

/// The hostiles in a room, the bounce on its border included (#552): each on
/// an exit tile of a room beside it at the tile it lands on in this one, and
/// each healer remembered off an exit tile of this one
/// (`ColonyView.RecalledHealers`). The engine carries a body on an exit tile
/// across every tick, so one bouncing on the border stands in the room every
/// other tick and answers for it on both, seen across or not.
let internal bouncingInto (view: ColonyView) (room: string) : HostileInfo list =
    let seen =
        view.Hostiles
        |> List.choose (fun hostile ->
            if hostile.Pos.Room = room then
                Some hostile
            else
                RoomName.offsetOf hostile.Pos.Room room
                |> Option.bind (fun offset -> Seam.landingOf offset (RoomPos.pos hostile.Pos))
                |> Option.map (fun landing ->
                    { hostile with
                        Pos = RoomPos.at room landing
                    }))

    seen
    @ (view.RecalledHealers
       |> List.filter (fun healer ->
           healer.Pos.Room = room
           && Seam.isExit (RoomPos.pos healer.Pos)
           && not (seen |> List.exists (fun hostile -> hostile.Id = healer.Id))))

/// The raid bodies at the home's line this tick (#504): a hostile, no
/// keeper's, standing outside the line (`Atlas.outsideLine`, the exit tiles
/// included) that fights with an ATTACK or RANGED_ATTACK, heals the raid,
/// or works with a WORK beside a perimeter
/// rampart (`HostileInfo.effectiveParts`); one bouncing on the home's exit
/// across the border at the exit tile (`bouncingInto`). A body inside
/// the line — a breach, or at the Keep — is the towers' and the relief's,
/// never a [[defender]]'s.
///
/// Beside it the tiles outside the line, which the defender's walk reads;
/// both empty on a tick no hostile stands in the home.
let internal raidAtLine (view: ColonyView) atlas : Set<Pos> * HostileInfo list =
    let home = SpatialInfo.homeName view.Spatial

    match
        bouncingInto view home
        |> List.filter (fun hostile -> not (HostileInfo.isKeeper hostile))
    with
    | [] -> Set.empty, []
    | _ when List.isEmpty view.Perimeter -> Set.empty, []
    | hostiles ->
        let outside = Atlas.outsideLine atlas view.Perimeter
        let ramparts = perimeterRamparts view |> List.map (fun (_, tile, _) -> tile)

        let besideLine (hostile: HostileInfo) =
            ramparts
            |> List.exists (fun tile ->
                RoomPos.range tile hostile.Pos
                |> Option.exists (fun r -> r <= Engine.meleeRange))

        outside,
        hostiles
        |> List.filter (fun hostile ->
            Set.contains (RoomPos.pos hostile.Pos) outside
            && (HostileInfo.armed view.Hostiles hostile
                || HostileInfo.heals view.Hostiles hostile
                || HostileInfo.works view.Hostiles hostile && besideLine hostile))

/// Whether any Keep structure of this colony stands below full hits: the
/// safe-mode reflex's own question. Its own predicate and not a filter over
/// `hungryStructures`, because this arm needs no held set. A second walk over
/// a hundred-odd structure hits, which is not a flood (#171).
let internal keepDamaged (view: ColonyView) : bool =
    SpatialInfo.structureHits view.Spatial
    |> List.exists (fun (_, kind, hits) -> isKeep kind && hits.Hits < hits.HitsMax)

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

/// The minerals our extractors are for, in the rooms this colony owns — each
/// room's Thorium deposits, or once none stands its ore
/// (`SpatialInfo.dugMineralsIn`) — the list both the Task pool and the miner
/// row's quota are read off. A scanned neighbour arrives in the projection
/// with its own deposit, extractor and container, and `harvest` refuses a
/// mineral whose extractor belongs to somebody else: one `ERR_NOT_OWNER` a
/// tick for the whole of a body's life.
///
/// And the ore of the rooms past home the colony digs into its terminal
/// (`ColonyView.oreRooms`, the errands it works): the Reactor's room has no
/// controller to own, and its neutral extractor is anybody's to dig through.
let internal ourDeposits (view: ColonyView) : string list =
    let oreRooms = ColonyView.oreRooms view

    view.Spatial.Rooms
    |> Map.toList
    |> List.filter (fun (room, _) ->
        Set.contains room oreRooms
        || Map.tryFind room view.RoomControl
           |> Option.exists (fun control -> control.Owner = Ownership.Ours))
    |> List.collect (fun (room, _) ->
        SpatialInfo.dugMineralsIn view.Spatial oreRooms room |> List.map fst)

/// What a mineral holds, and None for a target that is no mineral.
let internal mineralResourceOf (view: ColonyView) (id: string) : Resource option =
    match Map.tryFind id view.Spatial.TargetKinds with
    | Some(Mineral resource) -> Some resource
    | _ -> None

/// What a mineral has left to give, read down its own resource's column.
let internal mineralLeft (view: ColonyView) (id: string) : int =
    mineralResourceOf view id
    |> Option.map (fun resource -> SpatialInfo.heldIn view.Spatial resource id)
    |> Option.defaultValue 0

/// The rooms this colony has declared an errand in, as a set — the join four
/// rules of the season's ore make, written once (#378).
let internal errandRooms (view: ColonyView) : Set<string> =
    view.Errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

/// The [[harassment room]]s this colony casts this tick, as a set.
let internal harassRooms (view: ColonyView) : Set<string> =
    view.Harass |> List.map (fun h -> h.RoomName) |> Set.ofList

/// Whether a hostile is the declared enemy's, standing in the harassment room
/// that names it: armed or not, a target there. Nobody else's creep is.
let internal harassTarget (view: ColonyView) (hostile: HostileInfo) : bool =
    view.Harass
    |> List.exists (fun h -> h.RoomName = hostile.Pos.Room && h.Enemy = hostile.Owner)

/// Whether a hostile is a rival's CLAIM body standing in one of these
/// resident rooms: unarmed, and the thing that takes the flag (#406, #414) or
/// attacks a raised child's controller (#447).
let internal claimsAFlag
    (side: HostileInfo list)
    (residentRooms: Set<string>)
    (hostile: HostileInfo)
    =
    Set.contains hostile.Pos.Room residentRooms && HostileInfo.claims side hostile

/// The children's homes this colony is raising (#447): its borrowed rooms at
/// `Nursery`, `Bootstrapping` or `Weaning`, `Colony.bootstrapping`'s half of
/// them. A lost child's home, the other half, has no stage. And a contested
/// `Independent` child's short of its towers (`BorrowedWork.Garrisoned`, #479).
let internal raisedHomes (view: ColonyView) : Set<string> =
    view.Borrowed.Rooms
    |> List.filter (fun room ->
        match roomStage view room with
        | Some Nursery
        | Some Bootstrapping
        | Some Weaning -> true
        | Some Independent
        | None -> false)
    |> List.append view.Borrowed.Garrisoned
    |> Set.ofList

/// The rooms the ranger row keeps its resident garrison in, raid or none
/// (#419): the errand rooms, and the children's homes this colony raises
/// (#447). One rule for both, read by the row, the pool's cap, the ground and
/// the target.
let internal residentRooms (view: ColonyView) : Set<string> =
    Set.union (errandRooms view) (raisedHomes view)

/// The rooms whose Guard is the ranger's: the resident rooms and the
/// harassment rooms (#411, #432). Every other guarded room is the guard's.
let internal rangerRooms (view: ColonyView) : Set<string> =
    Set.union (residentRooms view) (harassRooms view)

/// The level-0 invader cores this colony sees (#487): each is a guard's to
/// kill, where a stronghold's is the stand-down's.
let internal expansionCores (view: ColonyView) : InvaderCoreInfo list =
    view.InvaderCores |> List.filter (InvaderCoreInfo.isStronghold >> not)

/// Whether two tiles stand within `reach` of each other; never across rooms.
let private within (reach: int) (a: RoomPos) (b: RoomPos) =
    RoomPos.range a b |> Option.exists (fun r -> r <= reach)

/// Whether a hostile is a raid's healer (#451): unarmed, healing, and
/// standing within three of an armed hostile — part of the raid, which it
/// keeps alive.
let internal raidHealer (view: ColonyView) (hostile: HostileInfo) : bool =
    not (HostileInfo.armed view.Hostiles hostile)
    && HostileInfo.heals view.Hostiles hostile
    && view.Hostiles
       |> List.exists (fun other ->
           HostileInfo.armed view.Hostiles other
           && within Engine.rangedRange other.Pos hostile.Pos)

/// Whether a hostile is a healer in a room we own (#552), whatever stands
/// beside it, as at the home's line (`raidAtLine`): one bouncing on the exit
/// while its melee waits across the border, or one tanking the towers
/// alone. Whose it may be is `mayStrike`'s.
let internal healerAtHome (view: ColonyView) (hostile: HostileInfo) : bool =
    not (HostileInfo.armed view.Hostiles hostile)
    && HostileInfo.heals view.Hostiles hostile
    && Map.tryFind hostile.Pos.Room view.RoomControl
       |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// Whether a hostile's body trespasses wherever it stands (#516), among
/// `side`: unarmed in effect, and shot disarmed — a weapon on paper and none
/// in effect (#528, any owner's since #535), so it strikes nothing back — or
/// a declared enemy's (`Colony.enemies`) working or carrying with a WORK,
/// CARRY or CLAIM. A hauler carries our dug energy off; a bare scout takes
/// nothing. Whose it may be is `mayStrike`'s.
let internal trespassingBody (side: HostileInfo list) (hostile: HostileInfo) =
    let takes part =
        part = Work || part = Carry || part = BodyPart.Claim

    not (HostileInfo.armed side hostile)
    && (Option.isSome (HostileInfo.weaponRangeOf hostile.Body)
        || Colony.isEnemy hostile.Owner
           && List.exists takes hostile.Body
           && List.exists takes (HostileInfo.effectiveParts side hostile))

/// Whether this colony mines a room: its home, or a room of its sources.
let internal minesRoom (view: ColonyView) (room: string) =
    room = SpatialInfo.homeName view.Spatial
    || view.Sources
       |> List.exists (fun source -> SpatialInfo.roomOf view.Spatial source.Id = Some room)

/// Whether a trespasser in a room is this colony's to kill: a room it mines
/// (#516), or a child's outpost it watches for the residents next door
/// (`BorrowedWork.ChildOutposts`, #517).
let internal trespassRooms (view: ColonyView) (room: string) =
    minesRoom view room || List.contains room view.Borrowed.ChildOutposts

/// Whether a neutral player's creep struck one of ours first, which would
/// make it ours to strike back anywhere: deferred, no hit memory yet (#535).
let internal struckUsFirst (_hostile: HostileInfo) = false

/// The one diplomacy rule every strike reads (#535, the user 2026-10-06):
/// never a Source Keeper (not provoked) nor an ally; a declared enemy's and
/// an NPC Invader's creep anywhere; any other armed one — a weapon on paper,
/// so a cripple shot disarmed counts — only where it stands in a room we
/// work or hold (`trespassRooms`, our home, a room whose fight we hold, in
/// `engaged`); and a neutral that struck us first (`struckUsFirst`).
let internal mayStrike (view: ColonyView) (engaged: Set<string>) (hostile: HostileInfo) =
    not (HostileInfo.isKeeper hostile)
    && not (Colony.isAlly hostile.Owner)
    && (Colony.strikesAnywhere hostile.Owner
        || Option.isSome (HostileInfo.weaponRangeOf hostile.Body)
           && (trespassRooms view hostile.Pos.Room || Set.contains hostile.Pos.Room engaged)
        || struckUsFirst hostile)

/// Whether a hostile is a [[trespasser]] (#516): its body trespasses, we may
/// strike it (`mayStrike`), and it stands in a room this colony kills
/// trespassers in, or one whose fight it holds (`engaged`, #535): a cripple
/// left there is the fight's to finish. The body is asked first, so a tick
/// with no such body walks no source.
let internal trespasser (view: ColonyView) (engaged: Set<string>) (hostile: HostileInfo) =
    trespassingBody view.Hostiles hostile
    && mayStrike view engaged hostile
    && (trespassRooms view hostile.Pos.Room || Set.contains hostile.Pos.Room engaged)

/// What a Guard in its room may shoot, every target reader's one predicate
/// (`Emitter.strikeOrder`, the kite's head): an armed
/// hostile we may strike (`mayStrike`, #535: in a harassment room a keeper or
/// a third party's is no target), a rival's claimer or a raid's healer (#451)
/// in a resident room, and the declared enemy's creep in a harassment room. In
/// a room whose fight we hold (`fighting`: a held Guard, #535), any healer we may strike: one whose raid is dead is still the
/// raid's. And a trespasser (#516), last in the kill order. Whether a hostile
/// breaks a room's quiet is `disturbs`, not this.
let internal guardShoots
    (view: ColonyView)
    (residentRooms: Set<string>)
    (fighting: Set<string>)
    (hostile: HostileInfo)
    =
    HostileInfo.armed view.Hostiles hostile
    && mayStrike view (Set.union residentRooms fighting) hostile
    || claimsAFlag view.Hostiles residentRooms hostile
    || harassTarget view hostile
    || Set.contains hostile.Pos.Room residentRooms
       && (raidHealer view hostile
           || healerAtHome view hostile && mayStrike view residentRooms hostile)
    || Set.contains hostile.Pos.Room fighting
       && HostileInfo.heals view.Hostiles hostile
       && mayStrike view fighting hostile
    || trespasser view fighting hostile

/// Whether a hostile breaks the quiet of the room it stands in (#516): any
/// armed one, whoever's — an armed neutral is still a threat to a home its
/// garrison would leave —, a rival's claimer or a raid's healer in a resident
/// room, the declared enemy's creep in a harassment room, or a trespasser.
/// The threat question, apart from the target one (`guardShoots`, #535).
let internal disturbs (view: ColonyView) (residentRooms: Set<string>) (hostile: HostileInfo) =
    HostileInfo.armed view.Hostiles hostile
    || claimsAFlag view.Hostiles residentRooms hostile
    || harassTarget view hostile
    || Set.contains hostile.Pos.Room residentRooms
       && (raidHealer view hostile
           || healerAtHome view hostile && mayStrike view residentRooms hostile)
    || trespasser view Set.empty hostile

/// Whether a ranged body of these part counts holds the range on a hostile
/// for as long as it likes (#510): the hostile shoots nothing, and falls
/// behind ours on road and plain alike (`Gait.fallsBehind`), ours read off
/// its live parts, the hostile's among `side` (`HostileInfo.effectiveParts`).
let internal cannotClose (side: HostileInfo list) (ours: BodyPart -> int) (hostile: HostileInfo) =
    not (HostileInfo.shoots side hostile)
    && Gait.fallsBehind (Gait.ofParts ours) (HostileInfo.gait side hostile)

/// The tiles of one projected room the strike rules ask after, read off its
/// layer once: its sources', its controller's, and its standing ramparts not
/// ours (`underForeignRampart`), these by tile index (`indexOf`) where the
/// tile is on the grid.
type private StrikeTiles =
    {
        Sources: Pos list
        Controllers: Pos list
        ForeignRamparts: System.Collections.Generic.HashSet<int>
        ForeignRampartsOffGrid: Pos list
    }

let private strikeTilesIn (spatial: SpatialInfo, room: string) : StrikeTiles =
    let ramparts = System.Collections.Generic.HashSet<int>()
    let mutable offGrid = []
    let mutable sources = []
    let mutable controllers = []

    for KeyValue(id, tile) in (SpatialInfo.layerOf spatial room).TargetPositions do
        match Map.tryFind id spatial.TargetKinds with
        | Some Source -> sources <- tile :: sources
        | Some Controller -> controllers <- tile :: controllers
        | Some(Structure BuiltKind.Rampart) when not (Map.containsKey id spatial.Hits) ->
            if inGrid tile then
                ramparts.Add(indexOf tile) |> ignore
            else
                offGrid <- tile :: offGrid
        | _ -> ()

    {
        Sources = sources
        Controllers = controllers
        ForeignRamparts = ramparts
        ForeignRampartsOffGrid = offGrid
    }

/// One projection's rooms' `StrikeTiles`, a room's derived once it has been
/// asked `strikeTilesAsks` times a tick: `canStrike` asks per fighter,
/// candidate and hostile, and a room's derivation costs about that many
/// direct asks.
let private strikeTilesAsks = 4

let private strikeTilesMemo: TickMemo.Memo<SpatialInfo * string, StrikeTiles> =
    TickMemo.create 32 (fun (spatial, room) (spatial', room') ->
        obj.ReferenceEquals(spatial, spatial') && room = room')

let private strikeTilesOf (view: ColonyView) (room: string) : StrikeTiles option =
    TickMemo.recallAfter strikeTilesAsks strikeTilesMemo (view.Spatial, room) strikeTilesIn

/// Whether a hostile stands within a step of a target of this kind in its room.
let private besideTargetOf (view: ColonyView) (kind: TargetKind) (hostile: HostileInfo) =
    let beside (tiles: Pos list) =
        tiles |> List.exists (fun tile -> range tile (RoomPos.pos hostile.Pos) <= 1)

    match kind, strikeTilesOf view hostile.Pos.Room with
    | Source, Some tiles -> beside tiles.Sources
    | Controller, Some tiles -> beside tiles.Controllers
    | _ ->
        (SpatialInfo.layerOf view.Spatial hostile.Pos.Room).TargetPositions
        |> Map.exists (fun id tile ->
            Map.tryFind id view.Spatial.TargetKinds = Some kind
            && range tile (RoomPos.pos hostile.Pos) <= 1)

/// Whether an unarmed worker stands on a seat it digs from: WORK in effect,
/// no weapon, within a step of a source of its room.
let private onDigSeat (view: ColonyView) (hostile: HostileInfo) =
    HostileInfo.works view.Hostiles hostile
    && not (HostileInfo.armed view.Hostiles hostile)
    && besideTargetOf view Source hostile

/// Whether a [[trespasser]] stands rooted at its work (#550): a digger on its
/// seat (`onDigSeat`), or a CLAIM body beside its room's controller, which it
/// reserves or attacks from there; one crossing the room is not.
let internal rootedAtWork (view: ColonyView) (hostile: HostileInfo) =
    onDigSeat view hostile
    || HostileInfo.claims view.Hostiles hostile
       && besideTargetOf view Controller hostile

/// Whether a hostile stands on a standing rampart that is not ours (#535): the
/// projection carries hits for an ownable structure only when it is ours.
let internal underForeignRampart (view: ColonyView) (hostile: HostileInfo) =
    let at = RoomPos.pos hostile.Pos

    match strikeTilesOf view hostile.Pos.Room with
    | Some tiles when inGrid at -> tiles.ForeignRamparts.Contains(indexOf at)
    | Some tiles -> List.contains at tiles.ForeignRampartsOffGrid
    | None ->
        (SpatialInfo.layerOf view.Spatial hostile.Pos.Room).TargetPositions
        |> Map.exists (fun id tile ->
            tile = at
            && Map.tryFind id view.Spatial.TargetKinds = Some(Structure BuiltKind.Rampart)
            && not (Map.containsKey id view.Spatial.Hits))

/// Whether a body of these live part counts can strike a hostile to kill it
/// (#531): the one rule every kill's admission and its verdict read. A body
/// with a weapon strikes any target that does not outrun it
/// (`Gait.outruns`), and one that does but is rooted (`HostileInfo.stationary`,
/// or an unarmed worker on its dig seat) or penned in its room (an Invader).
/// A target that outruns it is held off our work instead (`Brief`'s hold
/// ground), never chased. Never one standing under a rampart not ours while
/// it stands (`underForeignRampart`, #535): a strike lands on the rampart, so
/// a fight there is held for ever; the general no-progress give-up is the
/// movement epic's.
let internal canStrike (view: ColonyView) (ours: BodyPart -> int) (target: HostileInfo) =
    let side = view.Hostiles

    (ours RangedAttack > 0 || ours Attack > 0)
    && not (underForeignRampart view target)
    && (not (Gait.outruns (HostileInfo.gait side target) (Gait.ofParts ours))
        || not (HostileInfo.crossesExits target)
        || HostileInfo.stationary side target
        || onDigSeat view target)

/// Whether a body strikes every target a kill counts in a room (`canStrike`):
/// what the kill admits it by, and what the verdict prices it by — a chase's
/// raiders, a trespass's trespassers. Every, not any: a body that kills one
/// and is kited by another neither wins the room nor ends the kill.
let internal strikesEvery
    (view: ColonyView)
    (ours: BodyPart -> int)
    (counts: HostileInfo -> bool)
    (room: string)
    =
    view.Hostiles
    |> List.forall (fun h -> h.Pos.Room <> room || not (counts h) || canStrike view ours h)

/// What a body deals a tick, off its part counts: melee and ranged, a boosted
/// part counted at its multiple (`HostileInfo.powerWithin`).
let private damageOf (parts: BodyPart -> int) =
    Engine.attackPower * parts Attack
    + Engine.rangedAttackPower * parts RangedAttack

/// Whether a hostile is slower than a body of these part counts on plain, the
/// ground a fight crosses: it cannot keep off that body's swing.
let private slowerOnPlain (side: HostileInfo list) (ours: BodyPart -> int) (hostile: HostileInfo) =
    match
        Gait.ticks Gait.plain (HostileInfo.gait side hostile),
        Gait.ticks Gait.plain (Gait.ofParts ours)
    with
    | None, _ -> true
    | Some theirs, Some mine -> theirs > mine
    | Some _, None -> false

/// Whether a melee swing reaches a raider (#535): one whose gun is not its
/// weapon — no RANGED_ATTACK in effect, or a swing that outdeals its shot
/// (the engine's smallMelee, one gun beside its ATTACK, closes) — so it must
/// close to hurt our work; or one that cannot keep away, rooted, an Invader
/// carrying ATTACK or slower than ours on plain, the ground a fight crosses.
/// A body fighting with its gun, as quick as ours, keeps its range for as
/// long as it likes, and its healers behind it: one token ATTACK among its
/// guns does not bring it in. Penned in its room is no reach (#568): an
/// Invader with no ATTACK flees every hostile inside three
/// (`findAttack.js`), and kites a melee guard to death inside its room.
let internal swingReaches (side: HostileInfo list) (ours: BodyPart -> int) (raider: HostileInfo) =
    let parts = HostileInfo.effectivePower side raider

    Engine.attackPower * parts Attack
    >= Engine.rangedAttackPower * parts RangedAttack
    || HostileInfo.stationary side raider
    // An Invader with a live ATTACK walks onto its target whatever else it
    // carries: `findAttack.js` reads the active parts, never healed-back ones.
    || not (HostileInfo.crossesExits raider)
       && List.contains Attack (Engine.liveParts raider.Body raider.Hits)
    || slowerOnPlain side ours raider

/// Whether a body of these part counts brings its swing to bear on one
/// hostile (#535): one that is no raider, or a raider its swing reaches
/// (`swingReaches`).
let internal swingBorneOn (side: HostileInfo list) (ours: BodyPart -> int) (hostile: HostileInfo) =
    not (HostileInfo.raider side hostile) || swingReaches side ours hostile

/// What a body of these part counts brings to bear a tick on a raid (#535):
/// its shot in full, and its swing only where it reaches every raider of the
/// raid (`swingReaches`). A melee body that kills the ones that close and is
/// kept off by a shooter clears nothing, so against a raid holding one its
/// swing is priced at nothing: the named stand-in for a reach-aware exchange.
let private borneDamage (side: HostileInfo list) (raid: HostileInfo list) (ours: BodyPart -> int) =
    let swing =
        if ours Attack > 0 && raid |> List.forall (swingBorneOn side ours) then
            Engine.attackPower * ours Attack
        else
            0

    swing + Engine.rangedAttackPower * ours RangedAttack

/// Whether the [[defender]] meets one body of the raid at the home's line
/// (#546, #552): a raider its swing reaches (`swingReaches`), which must
/// close to hurt our work and so comes to the defender's rampart, a body
/// working with a WORK beside it, or a healer, which comes with its melee. A
/// shooter keeping its range never comes.
let internal defenderMeets (side: HostileInfo list) (hostile: HostileInfo) : bool =
    if HostileInfo.raider side hostile then
        swingReaches side (partCountIn defenderPattern.Block) hostile
    else
        HostileInfo.works side hostile || HostileInfo.heals side hostile

/// Whether the guard's swing catches a [[trespasser]] (#550): one rooted at
/// its work (`rootedAtWork`), or one slower than the guard on plain. A hauler
/// at the guard's pace keeps off a swing and never off a shot, so the guard
/// row answers the ones it catches and the ranger row the rest.
let internal guardCatches (view: ColonyView) (trespasser: HostileInfo) : bool =
    rootedAtWork view trespasser
    || slowerOnPlain view.Hostiles (partCountIn guardPattern.Block) trespasser

/// What a body of ours heals itself a tick while it fights: a ranged body's
/// heal acts beside its shot (#411), and a melee body's is suppressed by its
/// own attack, so it heals nothing.
let private selfHealOf (parts: BodyPart -> int) =
    if parts RangedAttack > 0 then
        Engine.healPower * parts Heal
    else
        0

/// Which band of the kill order a target falls in (#451), first band first.
type KillTier =
    /// A rival's CLAIM body in a resident room, anywhere in it:
    /// `claimReactor` has no cooldown and a controller attack lands the tick
    /// it stands beside, so the whole approach is the only window.
    | Claimer
    /// A raid's healer whose heal our damage reaching it outpaces.
    | BrokenHealer
    /// The rest of the raid: an armed body, or a healer we do not break.
    | Raid
    /// A CLAIM body outside a ranger's room (#550): a reservation against
    /// ours shuts the room's sources, so it goes first of the bodies that
    /// strike nothing back.
    | Reserver
    /// Anything else the Guard may shoot.
    | Bystander

/// One target's place in the kill order, compared field by field.
[<Struct>]
type KillRank =
    {
        Tier: KillTier
        /// Its hits, plus the heal that reaches it this tick, less our damage
        /// that does, all priced in damage through its TOUGH. Without a lock
        /// this all but locks itself: the target we hurt stays the lowest.
        EffectiveHits: int
    }

/// What one target's band in the kill order is read off: what it is, and the
/// heal and our damage that reach it this tick.
[<Struct>]
type private KillFacts =
    {
        ClaimsAFlag: bool
        RaidHealer: bool
        Armed: bool
        HealOn: int
        DamageOn: int
    }

/// One target's band in the kill order (`killRank`).
let private killTier (target: KillFacts) =
    if target.ClaimsAFlag then
        Claimer
    elif target.RaidHealer && target.DamageOn > target.HealOn then
        BrokenHealer
    elif target.Armed || target.RaidHealer then
        Raid
    else
        Bystander

/// The heal that reaches a hostile in `room` this tick, its own included: a
/// heal reaches one tile, a ranged heal three. A closure over the room's
/// healers, found once.
let internal healReaching (view: ColonyView) (room: string) : HostileInfo -> int =
    // A healer remembered from the last few ticks counts where it was last
    // seen (#480). One bouncing on the exit, standing on it or remembered off
    // it, is in the room every other tick and heals at half its rate over the
    // two: two bouncing out of step are one healer's worth, never two
    // (arena, 2026-10-04: W17S25's east line, where they tipped the tower
    // into repairing when a shot out-damaged the one healer there). An NPC
    // on the exit is never carried off it, and heals every tick.
    let present =
        view.Hostiles
        |> List.filter (fun h -> h.Pos.Room = room)
        |> List.map (fun h ->
            HostileInfo.healing view.Hostiles h,
            h,
            Seam.onRing (RoomPos.pos h.Pos) && HostileInfo.crossesExits h)

    let remembered =
        view.RecalledHealers
        |> List.filter (fun h -> h.Pos.Room = room)
        |> List.map (fun h -> HostileInfo.healing view.RecalledHealers h, h, true)

    let healers =
        present @ remembered |> List.filter (fun (healing, _, _) -> healing > 0)

    fun hostile ->
        healers
        |> List.sumBy (fun (healing, healer, bouncing) ->
            let rate =
                match RoomPos.range healer.Pos hostile.Pos with
                | Some r when r <= Engine.meleeRange -> healing
                | Some r when r <= Engine.rangedRange ->
                    healing * Engine.rangedHealPower / Engine.healPower
                | _ -> 0

            if bouncing then rate / 2 else rate)

/// What a body of these part counts deals one target at this range a tick: a
/// swing one tile, a shot three.
let internal damageAt (parts: BodyPart -> int) (range: int) =
    if range <= Engine.meleeRange then
        damageOf parts
    elif range <= Engine.rangedRange then
        Engine.rangedAttackPower * parts RangedAttack
    else
        0

/// Our creeps' damage that reaches a hostile this tick (`damageAt`). A
/// closure over our placed creeps, found once.
let internal damageReaching (view: ColonyView) : HostileInfo -> int =
    let fighters =
        view.Creeps
        |> List.choose (fun creep ->
            SpatialInfo.creepPlacementOf view.Spatial creep.Name
            |> Option.map (fun at -> at, partCount creep.Body))

    fun hostile ->
        fighters
        |> List.sumBy (fun (at, parts) ->
            RoomPos.range at hostile.Pos
            |> Option.map (damageAt parts)
            |> Option.defaultValue 0)

/// The kill order's leading key for one room's targets (#451), smallest
/// first; the caller breaks ties. Outside a ranger's room (an outpost's
/// melee guard) the raid first, then a claimer, and nothing more.
let internal killRank (view: ColonyView) (room: string) : HostileInfo -> KillRank =
    if not (Set.contains room (rangerRooms view)) then
        fun hostile ->
            {
                Tier =
                    (if HostileInfo.armed view.Hostiles hostile then Raid
                     elif HostileInfo.claims view.Hostiles hostile then Reserver
                     else Bystander)
                EffectiveHits = 0
            }
    else
        let resident = residentRooms view
        let healOn = healReaching view room
        let damageOn = damageReaching view

        fun hostile ->
            // The heal reaching it in the damage it costs to undo, through
            // its TOUGH, as its hits are.
            let heal = HostileInfo.damageFor hostile (healOn hostile)
            let damage = damageOn hostile

            {
                Tier =
                    killTier
                        {
                            ClaimsAFlag = claimsAFlag view.Hostiles resident hostile
                            RaidHealer = raidHealer view hostile
                            Armed = HostileInfo.armed view.Hostiles hostile
                            HealOn = heal
                            DamageOn = damage
                        }
                EffectiveHits = HostileInfo.effectiveHits hostile + heal - damage
            }

/// A target's band of the kill order (`killRank`'s tier), first band
/// first: what a mass attack is weighed against a shot by (#563).
let internal killBandOf (rank: KillRank) : int =
    match rank.Tier with
    | Claimer -> 0
    | BrokenHealer -> 1
    | Raid -> 2
    | Reserver -> 3
    | Bystander -> 4

/// A raid's price at a window (`raidPrice`). Its armed bodies' hits come in
/// two phases: `Tough`, the damage that breaks their boosted TOUGH still
/// standing, while a hit healed costs `ToughHealing` of it; then `Hits`, the
/// rest, struck and healed one for one at `Healing`.
type private RaidPrice =
    {
        Damage: int
        Healing: int
        ToughHealing: int
        Tough: int
        Hits: int
    }

/// A raid's price at a window: the damage it deals a tick, each raider with
/// what it fights with at its boosts (`HostileInfo.powerWithin`); the healing
/// it does, priced as a rate; and the hits of its armed bodies as they stand.
/// Healers are priced in the healing and never in the hits. Its boosted TOUGH
/// is charged once, in the damage that breaks it (`HostileInfo.damageFor`),
/// the healing meanwhile on the armed body it costs most: a T3 TOUGH in front
/// makes each hit healed on it cost three of our damage and a third.
let private raidPrice (view: ColonyView) (window: int) (raid: HostileInfo list) : RaidPrice =
    let armed =
        raid
        |> List.filter (fun h ->
            Option.isSome (
                HostileInfo.weaponRangeOf (HostileInfo.partsWithin window view.Hostiles h)
            ))

    let healing = raid |> List.sumBy (HostileInfo.healing view.Hostiles)
    let tough = armed |> List.filter (fun h -> HostileInfo.toughHits h > 0)

    {
        Damage =
            raid
            |> List.sumBy (fun h -> damageOf (HostileInfo.powerWithin window view.Hostiles h))
        Healing = healing
        ToughHealing =
            match tough with
            | [] -> healing
            | _ -> tough |> List.map (fun h -> HostileInfo.damageFor h healing) |> List.max
        Tough = tough |> List.sumBy (fun h -> HostileInfo.damageFor h (HostileInfo.toughHits h))
        Hits = armed |> List.sumBy (fun h -> h.Hits - HostileInfo.toughHits h)
    }

/// Whether our side — this damage, healing and hits — wins against a priced
/// raid: two clocks compared, cross-multiplied to stay in whole numbers. A
/// raid that out-heals our damage is never killed, nor one whose TOUGH it
/// never breaks; past its TOUGH our clock runs in two phases.
let private wonAgainst (price: RaidPrice) ourDamage ourHealing ourHits =
    if price.Damage = 0 then
        true
    elif
        ourDamage <= price.Healing
        || (price.Tough > 0 && ourDamage <= price.ToughHealing)
    then
        false
    elif price.Damage <= ourHealing then
        true
    elif price.Tough = 0 then
        price.Hits * (price.Damage - ourHealing) < ourHits * (ourDamage - price.Healing)
    else
        let ours =
            float price.Tough / float (ourDamage - price.ToughHealing)
            + float price.Hits / float (ourDamage - price.Healing)

        ours < float ourHits / float (price.Damage - ourHealing)

/// Whether our side wins the exchange against a raid (`wonAgainst` its
/// `raidPrice`): what stands beside our bodies (`beside` damage and
/// `besideHits`, a home's towers) and the bodies themselves, each dealing
/// what it brings to bear on this raid (`borneDamage`, #535) and healing
/// itself as it fights (`selfHealOf`).
let private exchangeAgainst
    (view: ColonyView)
    (window: int)
    (raid: HostileInfo list)
    (beside: int)
    (besideHits: int)
    (bodies: ((BodyPart -> int) * int) list)
    : bool =
    wonAgainst
        (raidPrice view window raid)
        (beside
         + List.sumBy (fun (ours, _) -> borneDamage view.Hostiles raid ours) bodies)
        (List.sumBy (fst >> selfHealOf) bodies)
        (besideHits + List.sumBy snd bodies)

/// `exchangeAgainst` the raid standing in one room, read at the window to
/// close and strike (`Reaction.strike`): our bodies, by part counts and hits,
/// beside `beside` damage a tick that is no body's.
let internal exchangeWon
    (view: ColonyView)
    (room: string)
    (beside: int)
    (bodies: ((BodyPart -> int) * int) list)
    =
    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
    exchangeAgainst view Reaction.strike raid beside 0 bodies

/// Whether these creeps of ours, together, win the raid standing in a room
/// (`exchangeWon`): what a muster waits for (#535), and one body alone is
/// what it weighs a body standing in the room by (`Quota.homeHolds`).
let bodiesWin (view: ColonyView) (room: string) (creeps: CreepInfo list) =
    exchangeWon
        view
        room
        0
        (creeps |> List.map (fun creep -> partCount creep.Body, creep.Hits.Hits))

/// `exchangeWon` for one body of these active part counts and hits.
let internal bodyWins (view: ColonyView) (room: string) (parts: BodyPart -> int) (hits: int) =
    exchangeWon view room 0 [ parts, hits ]

/// Whether `blocks` whole blocks of a fighting row win `exchangeWon`, one
/// body of the blocks' parts and hits, beside `besideDamage` a tick that is
/// not a body's. Here and not in `Quota` because the ranger's ground reads it
/// too (`outmatched`).
///
/// Worked example: a lone smallMelee needs one guard block; backed by a
/// smallHealer, its 40 damage kills our 1,000 hits in 25 ticks, before our 30
/// net damage kills its 1,000 hits, so that raid needs the second block.
///
/// Partly applied to its room it prices the raid and one block's damage once,
/// and every block count off them: a body of whole blocks walks at one
/// block's pace, so its swing reaches what one block's does. `borne` prices
/// the damage the block brings to bear (`borneDamage`, #535); else in full,
/// the guard holding the post (#531).
let private blocksBeat
    (block: BodyPart list)
    (borne: bool)
    (besideDamage: int)
    (view: ColonyView)
    (room: string)
    =
    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
    let price = raidPrice view Reaction.strike raid
    let parts = partCountIn block

    let damage =
        if borne then
            borneDamage view.Hostiles raid parts
        else
            damageOf parts

    let heal = selfHealOf parts
    let hits = Engine.partHits * List.length block

    fun (blocks: int) ->
        wonAgainst price (besideDamage + blocks * damage) (blocks * heal) (blocks * hits)

/// `blocksBeat` for the guard's melee block, its swing priced in full: the
/// guard holds the post whatever it can reach (#531). Two readers: the guard
/// row asks it of one block to size the crowd, and the stand-down asks it of
/// the biggest body the bank buys (`guardBlocksReach`) to decide whether an
/// outpost is a fight or a withdrawal (`Observe.raidDeadlines`).
let guardBlocksBeat (view: ColonyView) (room: string) : int -> bool =
    blocksBeat guardPattern.Block false 0 view room

/// `guardBlocksBeat` with the swing priced as brought to bear (#535): what
/// picks the ranger row over the guard's.
let private guardBorneBeat (view: ColonyView) (room: string) : int -> bool =
    blocksBeat guardPattern.Block true 0 view room

/// `blocksBeat` for the ranger's block (#411), which the errand room's
/// stand-down and the ranger row read as the guard's do an outpost's.
let rangerBlocksBeat (view: ColonyView) (room: string) : int -> bool =
    blocksBeat rangerPattern.Block true 0 view room

/// The biggest guard body the row can cast (#417): at most `guardBlocksMost`
/// blocks, and as many as the bank's capacity buys (`Bodies.guardBodyWithin`).
/// What the stand-down weighs a raid against. One body and never the row's two
/// summed: `guardBlocksBeat` grows about as the square of the blocks, and a
/// raid focuses one body at a time while the second may arrive late, so the
/// sum would call a fight won that each body loses alone — and each body is
/// sized to win alone (`guardBlocksFor`).
let guardBlocksReach (view: ColonyView) : int =
    min guardBlocksMost (view.Bank.Capacity / bodyCost guardPattern.Block)

/// Whether the bank casts one block of a row's pattern (#546): a row whose
/// block it cannot buy is never asked for an answer, which falls to the next
/// row that can — a ranger's 700 on a 550 bank waits for ever, holding the
/// economy that would raise the bank.
let castable (view: ColonyView) (pattern: BodyPattern) =
    bodyCost pattern.Block <= view.Bank.Capacity

/// The ranger's twin of `guardBlocksReach` (#411): the biggest ranger body the
/// bank buys, what an errand room's raid is weighed against.
let rangerBlocksReach (view: ColonyView) : int =
    min rangerBlocksMost (view.Bank.Capacity / bodyCost rangerPattern.Block)

/// Whether a ranged body of these live parts kites the raid standing in a
/// room down (#510): every raider there is one it holds the range on
/// (`cannotClose`), and its damage out-runs every heal acting in the room,
/// so the kite ends in a kill.
let internal kitedDown (view: ColonyView) (parts: BodyPart -> int) (room: string) : bool =
    let inRoom = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    let healing = inRoom |> List.sumBy (HostileInfo.healing view.Hostiles)

    match inRoom |> List.filter (HostileInfo.raider view.Hostiles) with
    | [] -> false
    | raid ->
        raid |> List.forall (cannotClose view.Hostiles parts)
        // The heal priced in damage on the raider it costs least
        // (`HostileInfo.damageFor`), as `raidPrice` prices it.
        && damageOf parts > (raid |> List.map (fun h -> HostileInfo.damageFor h healing) |> List.min)

/// `kitedDown` for the biggest ranger body the bank buys: the room-level
/// answer the stand-down, `outmatched` and the chase read.
let internal kitedByRanger (view: ColonyView) (room: string) : bool =
    let blocks = max 1 (rangerBlocksReach view)
    kitedDown view (fun part -> blocks * partCountIn rangerPattern.Block part) room

/// What a room's loaded towers land a tick on one target, priced at the
/// falloff range: a raised home's, beside whatever body of ours fights there.
let private towerDamageIn (view: ColonyView) (room: string) =
    let towers = Map.tryFind room view.LoadedTowers |> Option.defaultValue 0
    towers * Engine.towerAttackAt Engine.towerFalloffRange

/// A home's middle: what a raid on it strikes, its spawn, store and
/// controller standing about there.
let private homeMiddle = Engine.roomSide / 2

/// Whether a hostile standing outside `home` is a raider that reaches
/// striking range of its middle inside the window a lent body is away
/// (#531): armed, paced and ranged all read at that one window
/// (`Reaction.lend`), its walk the world tiles there less its weapon's range,
/// at its pace on road, the faster ground. One that never leaves its room
/// (an Invader) or steps of itself arrives nowhere.
let private arrivesWithin (view: ColonyView) (home: string) (hostile: HostileInfo) =
    let parts = HostileInfo.partsWithin Reaction.lend view.Hostiles hostile

    HostileInfo.crossesExits hostile
    && (match
            RoomName.tileRange hostile.Pos (RoomPos.at home { X = homeMiddle; Y = homeMiddle }),
            HostileInfo.weaponRangeOf parts
        with
        | Some tiles, Some reach ->
            Gait.ticks Gait.road (HostileInfo.gaitWithin Reaction.lend view.Hostiles hostile)
            |> Option.exists (fun pace -> max 0 (tiles - reach) * pace <= Reaction.lend)
        | _ -> false)

/// Whether what a home keeps while its garrison is lent beats the raiders
/// arriving inside the lend's window (#531), priced together by the exchange
/// at that window: its loaded towers, standing at their hits, and the bodies
/// of ours standing there that no lend takes (anything but the ranger row's).
let private homeHolds (view: ColonyView) (home: string) (arriving: HostileInfo list) =
    let staying =
        view.Creeps
        |> List.filter (fun creep ->
            not (isRangerRowBody creep)
            && SpatialInfo.creepPlacementOf view.Spatial creep.Name
               |> Option.exists (fun at -> at.Room = home))
        |> List.map (fun creep -> partCount creep.Body, creep.Hits.Hits)

    let towers = Map.tryFind home view.LoadedTowers |> Option.defaultValue 0

    exchangeAgainst
        view
        Reaction.lend
        arriving
        (towerDamageIn view home)
        (towers * Engine.towerHits)
        staying

/// Whether a resident room is quiet enough to lend its garrison to a kill
/// in `kill` (#516): nothing its Guard shoots stands in it, and what it
/// keeps beats every raider but the kill's own that reaches it before the
/// garrison is back (`arrivesWithin`, `homeHolds`).
let private residentQuiet (view: ColonyView) (resident: Set<string>) (kill: string) (home: string) =
    // One bouncing on the home's exit is in the home (#552), though it
    // stands in the kill's room this tick.
    let inHome = bouncingInto view home

    let arriving =
        view.Hostiles |> List.filter (fun h -> h.Pos.Room <> kill && h.Pos.Room <> home)

    inHome |> List.forall (disturbs view resident >> not)
    && (match arriving |> List.filter (arrivesWithin view home) with
        | [] -> true
        | raid -> homeHolds view home raid)

/// Whether a ranger of this colony standing in a room may be lent to a kill
/// in `kill`, a chase's or a trespass's (#510, #516): its own colony's rule,
/// asked in its own colony's view whoever prices the room (#518, the world
/// layer's `judge`). Nothing a ranger carries says whose resident it is, so
/// every resident room beside the kill is quiet (`residentQuiet`), or no
/// ranger may — a harassment ranger or a fresh cast as much as the
/// garrison: a raid arriving there calls its garrison back off the kill at
/// once. And one standing in a resident room is its garrison, lent only to
/// a room beside it.
let internal rangerMayKill (view: ColonyView) (kill: string) (standing: string option) =
    let resident = residentRooms view

    resident
    |> Set.forall (fun home ->
        not (RoomName.neighbouring home kill) || residentQuiet view resident kill home)
    && standing
       |> Option.forall (fun here ->
           not (Set.contains here resident) || RoomName.neighbouring here kill)

/// Whether the holder of a room's Guard may be called off it for a kill in
/// another (#510, #516), the bodies the kill admits being the gate's to say.
/// A resident only as it may be lent (`rangerMayKill`); any other holder
/// while nothing its Guard shoots stands in its room, and never off another
/// kill.
let internal mayBeCalled (view: ColonyView) (kills: Set<string>) (held: string) (kill: string) =
    let resident = residentRooms view

    if Set.contains held resident then
        rangerMayKill view kill (Some held)
    else
        not (Set.contains held kills)
        && view.Hostiles
           |> List.forall (fun h -> h.Pos.Room <> held || not (disturbs view resident h))

/// Whether the biggest ranger the bank buys, at least one block as for the
/// kite, wins a room's raid alone beside a raised home's loaded towers.
let private rangerRowWins (view: ColonyView) (room: string) =
    blocksBeat
        rangerPattern.Block
        true
        (towerDamageIn view room)
        view
        room
        (max 1 (rangerBlocksReach view))

/// A room's own row and its answer, the one reading `rangerAnswers` and the
/// verdict share. A ranger room and one the colony harasses are the ranger's
/// (#411). Any other is the melee guard's where the guard's borne damage wins
/// (`guardBorneBeat`); else the ranger's where the biggest ranger wins; each
/// only where the bank casts its block (`castable`, #546); else
/// still the guard's, which holds the post (#531) and wins while it outlasts
/// the raid, its swing priced in full (`guardBlocksBeat`): no new stand-down
/// for a raid of quick shooters no ranger wins either. Never a win in a
/// child's outpost, which no row of this colony casts for (#517).
let rowAnswer (view: ColonyView) (room: string) : RowAnswer =
    if List.contains room view.Borrowed.ChildOutposts then
        { Ranger = false; Wins = false }
    elif Set.contains room (rangerRooms view) || Set.contains room view.HarassCast then
        {
            Ranger = true
            Wins = rangerRowWins view room
        }
    elif castable view guardPattern && guardBorneBeat view room (guardBlocksReach view) then
        { Ranger = false; Wins = true }
    elif castable view rangerPattern && rangerRowWins view room then
        { Ranger = true; Wins = true }
    else
        {
            Ranger = false
            Wins = guardBlocksBeat view room (guardBlocksReach view)
        }

/// Whether a room's own Guard is the ranger row's (`rowAnswer`).
let rangerAnswers (view: ColonyView) (room: string) : bool = (rowAnswer view room).Ranger

/// One colony's rangers that may answer the raid in `room`, each with the
/// room it stands in: its own standing in the room, the garrison among them,
/// and its own standing beside it, lent by its own colony's rule
/// (`rangerMayKill`) out of a room where nothing its Guard would shoot
/// stands; each a body that strikes every raider there and that its colony
/// sends this tick (`sends`). Asked in the lender's own view, so its quiet is
/// read on its side.
let private lendableTo
    (view: ColonyView)
    (sends: CreepInfo -> bool)
    (room: string)
    : (CreepInfo * string) list =
    let resident = residentRooms view

    let quiet (here: string) =
        view.Hostiles
        |> List.forall (fun h -> h.Pos.Room <> here || not (disturbs view resident h))

    view.Creeps
    |> List.choose (fun creep ->
        SpatialInfo.creepPlacementOf view.Spatial creep.Name
        |> Option.map (fun at -> creep, at.Room))
    |> List.filter (fun (creep, here) ->
        (here = room || RoomName.neighbouring here room)
        && isRangerRowBody creep
        && sends creep
        && strikesEvery view (partCount creep.Body) (HostileInfo.raider view.Hostiles) room)
    |> List.filter (fun (_, here) ->
        here = room || quiet here && rangerMayKill view room (Some here))

/// The fewest rangers that win the raid in `room` together, among every
/// lender's (`lendableTo`), each by name with its colony's home; None where
/// all of them lose. Taken standing in the room first, then beside it, by
/// name within each, and priced in `pricing`, a view that sees the raid. One
/// count over every colony's bodies, so a raid only two colonies' rangers
/// win together is won (#518).
let private rangersAmong
    (pricing: Lazy<ColonyView>)
    (lenders: (string * ColonyView * (CreepInfo -> bool)) list)
    (room: string)
    : (string * string) list option =
    let rec wins taken named rest =
        match rest with
        | [] -> None
        | ((creep: CreepInfo), (home: string)) :: rest ->
            let taken = creep :: taken
            let named = (creep.Name, home) :: named

            if bodiesWin pricing.Value room taken then
                Some(List.rev named)
            else
                wins taken named rest

    lenders
    |> List.collect (fun (home, view, sends) ->
        lendableTo view sends room
        |> List.map (fun (creep, here) -> (here <> room, creep.Name), (creep, home)))
    |> List.sortBy fst
    |> List.map snd
    |> wins [] []

/// One colony as the world's verdict prices a room: its home; its view, with
/// the world's look into the stand-down rows it prices out of sight laid in,
/// the rooms it can answer at all; the rooms its own rows cast for
/// (`castsFor`); and which of its bodies it sends to a room this tick.
type private Pricer =
    {
        Home: string
        View: ColonyView
        Casts: Lazy<Set<string>>
        Sends: string -> CreepInfo -> bool
    }

/// The one colony that answers among these: the fewest crossings from the
/// room (`hops`), then the largest bank, then by home name, as
/// `World.harassCasters` ranks a harassment room's casters.
let private answererAmong
    (hops: string -> string -> int option)
    (room: string)
    (among: Pricer list)
    =
    match among with
    | [] -> None
    | [ one ] -> Some one.Home
    | _ ->
        among
        |> List.sortBy (fun p ->
            hops p.Home room |> Option.defaultValue System.Int32.MaxValue,
            -p.View.Bank.Capacity,
            p.Home)
        |> List.tryHead
        |> Option.map (fun p -> p.Home)

/// One room's verdict over these colonies (the world layer's S3), among
/// those that see its raid — the colonies that can answer it at all. The
/// room's own answer is its casters': of the colonies whose rows cast for it
/// (`castsFor`, a home its own colony's), or, where none does and it is no
/// home of ours, of every colony that sees it, the biggest row cast first,
/// then the biggest ranger kiting it. Then the rangers standing in it or lent
/// to it, of every colony that sees it, together (`rangersAmong`). The raid's
/// strength is read off the world's vision of the room and the rooms beside
/// it, whichever colony prices it. A room no colony sees a raider in is a
/// Win every row gives.
let private priceAmong
    (hops: string -> string -> int option)
    (pricers: Pricer list)
    (room: string)
    =
    let seeing = pricers |> List.filter (fun p -> raided p.View room)

    // A view with the world's vision of the room and its neighbours laid in.
    let beside = lazy (room :: RoomName.adjacent room)

    let around (view: ColonyView) =
        ColonyView.lookingInto beside.Value view

    let rangers =
        lazy
            (match seeing with
             | [] -> None
             | first :: _ ->
                 rangersAmong
                     (lazy (around first.View))
                     (seeing |> List.map (fun p -> p.Home, p.View, p.Sends room))
                     room)

    let verdict answer rows own bodies (answering: Pricer list) =
        {
            Answer = answer
            Rows = rows
            Own = own
            Rangers = lazy (Option.map List.length rangers.Value)
            Bodies = bodies
            Answerer = answererAmong hops room answering
        }

    match seeing with
    | [] -> verdict Answer.Win Map.empty (pricers |> List.map (fun p -> p.Home) |> Set.ofList) [] []
    | first :: _ ->
        let casters =
            match seeing |> List.filter (fun p -> p.Casts.Value.Contains room) with
            | [] when Map.containsKey room first.View.Stages -> []
            | [] -> seeing
            | owners ->
                // A home another colony's row casts for (a raised child's,
                // the mother's residents; a defended one, her guard) is
                // theirs: no row of the home's own casts there.
                match owners |> List.filter (fun p -> p.Home <> room) with
                | [] -> owners
                | others -> others
            |> List.map (fun p -> p, around p.View)

        let rows =
            casters
            |> List.map (fun (p, priced) -> p.Home, rowAnswer priced room)
            |> Map.ofList

        // A row never answers a room its colony shut on a raid
        // (`ColonyView.RaidedOutposts`): the stand-down withholds that row's
        // Guard, so its Win would bind the room to a body nobody casts and
        // keep the rangers that kite it down from pooling the chase (#573).
        let rowed =
            casters
            |> List.filter (fun (p, _) ->
                rows.[p.Home].Wins && not (p.View.RaidedOutposts.Contains room))
            |> List.map fst

        if not (List.isEmpty rowed) then
            verdict Answer.Win rows (rowed |> List.map (fun p -> p.Home) |> Set.ofList) [] rowed
        else
            match
                casters
                |> List.filter (fun (_, priced) -> kitedByRanger priced room)
                |> List.map fst
            with
            | _ :: _ as kiters -> verdict Answer.Kite rows Set.empty [] kiters
            | [] ->
                match rangers.Value with
                | Some taken ->
                    let homes = taken |> List.map snd |> Set.ofList
                    let among = pricers |> List.filter (fun p -> homes.Contains p.Home)

                    // A colony whose row casts for the room answers it ahead
                    // of one whose rangers only join it (the world layer's
                    // S4): it casts what the room still wants, as for a
                    // child's outpost the child, not the mother crossing it.
                    let answering =
                        match
                            among
                            |> List.filter (fun p ->
                                casters |> List.exists (fun (c, _) -> c.Home = p.Home))
                        with
                        | [] -> among
                        | casting -> casting

                    verdict Answer.Win rows Set.empty taken answering
                | None -> verdict Answer.Lose rows Set.empty [] []

/// One room's verdict as this view reads it: the world's, priced once a tick
/// over every colony's bodies (`judge`), or, where the world priced none — a
/// room no colony saw a raider in, or a fixture of one colony — over this
/// view alone.
let verdictOf (view: ColonyView) (room: string) : RoomVerdict =
    match Map.tryFind room view.World.Answers with
    | Some verdict -> verdict.Value
    | None ->
        priceAmong
            (fun _ _ -> None)
            [
                {
                    Home = SpatialInfo.homeName view.Spatial
                    View = view
                    Casts = lazy (Set.singleton room)
                    Sends = fun _ _ -> true
                }
            ]
            room

/// One room's verdict and whether the room's own row gives it, whichever
/// colony's (`verdictOf`).
let internal answered (view: ColonyView) (room: string) : Answer * bool =
    let verdict = verdictOf view room
    verdict.Answer, not (Set.isEmpty verdict.Own)

/// One room's verdict (`verdictOf`).
let answer (view: ColonyView) (room: string) : Answer = (verdictOf view room).Answer

/// The fewest rangers that win the raid in `room` together, every colony's
/// (`verdictOf`); None where all of them lose.
let internal rangersWin (view: ColonyView) (room: string) : int option =
    (verdictOf view room).Rangers.Value

/// Whether a room no ranger row serves is a ranger's chase (#510): its raid
/// in sight is answered, by rangers kiting it or called to it and not by its
/// own row, whichever colony's; or, where a ranger already holds the room's
/// Guard (`kept`), by rangers at all, so a raid worn down to what the row's
/// cast would win is not handed from the ranger at it to a body still to be
/// cast. In a room shut on a raid (`ColonyView.RaidedOutposts`) the row's own
/// answer is withheld with its Guard (#569), so rangers that kite it or win it
/// chase it there, held or not: the stand-down shuts the economy, not the
/// kill. Read once a tick into the chase's `Brief`, off the
/// room's `verdict` (`answered`).
let internal chased (view: ColonyView) (verdict: Answer * bool) (kept: bool) (room: string) : bool =
    room <> SpatialInfo.homeName view.Spatial
    && not (Set.contains room (rangerRooms view))
    && raided view room
    && (match verdict with
        | Answer.Lose, _ -> false
        | _, false -> true
        | _, true ->
            (kept || Set.contains room view.RaidedOutposts)
            && (kitedByRanger view room || Option.isSome (rangersWin view room)))

/// Whether nothing of ours wins the raid standing in a room (`Answer.Lose`):
/// in a ranger room, the raid its row casts nobody into, and the residents
/// already there hold the room's safe ground against.
let internal outmatched (view: ColonyView) (room: string) : bool = answer view room = Answer.Lose

/// Whether the resident garrison loses the raid standing in a room beside its
/// loaded towers: what opens its fight record. Both the garrison's size and the
/// rangers actually standing there, priced together, lose.
let internal residentsLose (view: ColonyView) (room: string) : bool =
    let towers = towerDamageIn view room

    let standing =
        view.Creeps
        |> List.filter (fun creep ->
            isRangerRowBody creep
            && SpatialInfo.creepPlacementOf view.Spatial creep.Name
               |> Option.exists (fun at -> at.Room = room))

    let standingWin =
        not (List.isEmpty standing)
        && exchangeWon
            view
            room
            towers
            (standing |> List.map (fun creep -> partCount creep.Body, creep.Hits.Hits))

    not standingWin
    && not (blocksBeat rangerPattern.Block true towers view room view.Tuning.RangerResidentBlocks)

/// The Planner's narrow view of the assignment table, filtered to the living:
/// `Assignments` arrives from Memory and may name a creep that died last tick,
/// the Matcher drops those silently, but `planTasks` runs first. Not filtered
/// to task kinds: a reader that wants one kind writes the key it wants and
/// asks, as `isHungry` does.
let heldTaskFacts (view: ColonyView) (assignments: Assignments) : HeldTaskFacts =
    let living = view.Creeps |> List.map (fun creep -> creep.Name, creep) |> Map.ofList

    assignments
    |> Map.fold
        (fun facts name tid ->
            match Map.tryFind name living with
            | None -> facts
            | Some creep ->
                { facts with
                    All = Set.add tid facts.All
                    WithThorium =
                        if creep.Thorium > 0 then
                            Set.add tid facts.WithThorium
                        else
                            facts.WithThorium
                    Holders = Map.add tid (name :: HeldTaskFacts.holdersOf facts tid) facts.Holders
                })
        HeldTaskFacts.empty

/// The errand rooms a raid stands in this tick (#414): an armed hostile that is
/// not a Source Keeper, or a rival's CLAIM body. What the guard is kept there
/// to meet, and what the re-claimer's seat waits on while the guard is short.
let internal errandRoomsRaided (view: ColonyView) : Set<string> =
    let rooms = errandRooms view

    view.Hostiles
    |> List.filter (fun hostile ->
        Set.contains hostile.Pos.Room rooms
        && (HostileInfo.raider view.Hostiles hostile
            || claimsAFlag view.Hostiles rooms hostile))
    |> List.map (fun hostile -> hostile.Pos.Room)
    |> Set.ofList

/// The errand rooms a rival's CLAIM body stands in this tick (#406). The flag
/// may be ours this tick, but `claimReactor` has no precondition, so a load
/// put in now burns for whoever holds the flag next. Off vision alone: no load
/// is drawn without a re-claimer resident to see.
let internal errandRoomsContested (view: ColonyView) : Set<string> =
    let rooms = errandRooms view

    view.Hostiles
    |> List.filter (claimsAFlag view.Hostiles rooms)
    |> List.map (fun hostile -> hostile.Pos.Room)
    |> Set.ofList

/// Whether an ally is burning in this Reactor (#413): their flag on it and
/// Thorium in its store, by the Reactor's own row. No row, no vision, and
/// absence is not an ally.
let internal reactorAllyBurning (view: ColonyView) (reactorId: string) : bool =
    view.Reactors
    |> List.exists (fun reactor ->
        reactor.Id = reactorId
        && reactor.Thorium > 0
        && (match reactor.Owner with
            | ReactorOwner.Rival username -> Colony.isAlly username
            | ReactorOwner.Ours
            | ReactorOwner.Unowned -> false))

/// The errand rooms whose Reactor an ally is burning in.
let internal errandRoomsAllyBurning (view: ColonyView) : Set<string> =
    view.Errands
    |> List.filter (fun errand -> reactorAllyBurning view (fst errand.Target))
    |> List.map (fun errand -> errand.RoomName)
    |> Set.ofList

/// The errand rooms a delivery may be drawn for and poured into this tick:
/// the declared ones less the contested, and less those an ally is burning in.
let internal errandRoomsDeliverable (view: ColonyView) : Set<string> =
    Set.difference
        (errandRooms view)
        (Set.union (errandRoomsContested view) (errandRoomsAllyBurning view))

/// Whether decaying ore standing at this target is this colony's to sweep: a
/// room we own (#311), a room we declared an errand in (#354: the Reactor's
/// room has no controller, so "a room we own" made its floor belong to nobody
/// while 915 T bled on it), or a room a chain of ours crosses (#360). Written
/// once for the piles, the tombstones and the breach channel that alarms on
/// both.
let private oursToSweep (view: ColonyView) (id: string) : bool =
    let errandRooms = errandRooms view

    // A held errand's walk is crossed for the miner's walking alone: the
    // Reactor's work is paused, and the ore on that floor (an ally's courier
    // dies there too) is nobody's errand of ours. A room on an unheld
    // errand's walk as well keeps its sweep.
    let heldWalks =
        let home = SpatialInfo.homeName view.Spatial

        let walks rooms =
            rooms |> Set.toList |> List.collect (RoomName.transitBetween home) |> Set.ofList

        Set.difference
            (walks (Set.difference (ColonyView.oreRooms view) errandRooms))
            (walks errandRooms)

    inARoomWeOwn view id
    || SpatialInfo.roomOf view.Spatial id
       |> Option.exists (fun room ->
           Set.contains room errandRooms
           // `view.Crossed` and not "any room in the projection": a stranger's
           // room can be in a scan set without being on a chain, and its floor
           // is not ours to walk onto. `ColonyView.transiting` admits exactly
           // two kinds out of a crossed room — a pile and a tombstone.
           || Set.contains room view.Crossed
              // Never in an evacuating room: no sweep may walk a body in (#512).
              && not (Set.contains room view.Evacuating)
              // Nor in a child's outpost: its floor is the child's (#517).
              && not (List.contains room view.Borrowed.ChildOutposts)
              && not (Set.contains room heldWalks))

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

/// The mineral containers of this colony's own mines, each keyed by what marks
/// the mine: the built container within range 1 of the mark in the mark's own
/// room (a `Pos` names no room, and a container on the same coordinate of an
/// outpost is not this mine's). Built alone and never a site: a site holds
/// nothing (#261). Off our deposits and extractors and never off the container
/// census, which carries every owner's. Paired with the mark because the two
/// readers ask different questions of the same join (#262).
///
/// A live mine is marked by its deposit. One whose deposit is gone is marked by
/// the extractor still standing on that tile (#421): the mod deletes an
/// exhausted deposit, and the miner's last digs stay in the container.
/// `depositIsDiggable` reads no for an extractor's id, so only the
/// Thorium-held readers see these.
let internal ourMineralContainerPairs (view: ColonyView) : (string * string) list =
    let containers =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Container)
        |> List.choose (fun id ->
            SpatialInfo.placementOf view.Spatial id |> Option.map (fun tile -> id, tile))

    let deposits = ourDeposits view

    let depositTiles =
        deposits |> List.choose (SpatialInfo.placementOf view.Spatial) |> Set.ofList

    let spentExtractors =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Extractor)
        |> List.filter (inARoomWeOwn view)
        |> List.filter (fun id ->
            SpatialInfo.placementOf view.Spatial id
            |> Option.exists (fun tile -> not (Set.contains tile depositTiles)))

    deposits @ spentExtractors
    |> List.choose (fun markId ->
        match SpatialInfo.placementOf view.Spatial markId with
        | None -> None
        | Some mark ->
            containers
            |> List.filter (fun (_, tile) ->
                tile.Room = mark.Room && range (RoomPos.pos tile) (RoomPos.pos mark) <= 1)
            // Ties by id, the way every other tie in this colony falls: a
            // mine the Layout ever seated two containers beside answers with
            // one of them and not with both.
            |> List.map fst
            |> List.sort
            |> List.tryHead
            |> Option.map (fun containerId -> markId, containerId))

let internal ourMineralContainers (view: ColonyView) : string list =
    ourMineralContainerPairs view |> List.map snd

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

/// This colony's [[emergency reserve]] (#505): `Tuning.energyReserve` at its
/// own controller's level, and nothing for a colony with no controller.
let internal energyReserve (view: ColonyView) : int =
    view.Controller
    |> Option.map (fun controller -> Tuning.energyReserve view.Tuning controller.Level)
    |> Option.defaultValue 0

/// The energy in the home room's own Storage, or None with no Storage
/// standing there: a colony whose Storage is gone has no stock to keep.
let private homeStock (view: ColonyView) : int option =
    match
        SpatialInfo.idsOfKindIn view.Spatial.TargetKinds (Structure BuiltKind.Storage)
        |> List.filter (SpatialInfo.placedAway view.Spatial >> not)
    with
    | [] -> None
    | ids -> Some(ids |> List.sumBy (SpatialInfo.storedIn view.Spatial))

/// Whether the stock stands under the [[emergency reserve]]: the one
/// question every reader of the reserve asks (#505). Below it the Storage
/// feeds the refill cluster, the towers and a struck or dying rampart's
/// Repair, and nothing else. Latched (`ColonyView.ReserveShort`): a colony
/// under it stays under until it banks one hauler load past the line, so a
/// single draw at the line does not flip every gate that reads it.
let internal belowReserve (view: ColonyView) : bool =
    let reserve = energyReserve view

    match homeStock view with
    | Some stock when reserve > 0 ->
        let band =
            if view.ReserveShort then
                carryCapacityOf (bodyFor haulerPattern view.Bank.Capacity)
            else
                0

        stock < reserve + band
    | _ -> false

/// Whether this colony gives energy aid this tick: somebody is under their
/// reserve, and its own stock stands `Tuning.AidDonorMargin` over its own.
let internal givesAid (view: ColonyView) : bool =
    not (List.isEmpty view.AidRooms)
    && homeStock view
       |> Option.exists (fun stock -> stock >= energyReserve view + view.Tuning.AidDonorMargin)

/// Whether the colony wants an [[ore]] dug this tick: its home terminal holds
/// under `Tuning.OreTarget` of it, and its home Storage stands
/// `Tuning.OreEnergyMargin` over the [[emergency reserve]] — the dig is paid
/// for in bodies. No terminal at home, no ore wanted: there is nowhere to put
/// it.
///
/// Latched (`ColonyView.DigOpen`): a dig the last decided tick left shut
/// opens only once the terminal stands `Tuning.OreTargetBand` under the
/// target and the Storage `Tuning.OreEnergyBand` over its margin, so a
/// stock hovering at either line casts the miner row once and not on each
/// crossing.
let internal oreWanted (view: ColonyView) (ore: Ore) : bool =
    let terminals =
        SpatialInfo.idsOfKindIn view.Spatial.TargetKinds (Structure BuiltKind.Terminal)
        |> List.filter (SpatialInfo.placedAway view.Spatial >> not)

    let stockBand, energyBand =
        // A latch never written reads open: a deploy shuts no running dig.
        if view.DigOpen |> Option.forall (Set.contains ore) then
            0, 0
        else
            view.Tuning.OreTargetBand, view.Tuning.OreEnergyBand

    not (List.isEmpty terminals)
    && terminals |> List.sumBy (SpatialInfo.heldIn view.Spatial (Ore ore)) < view.Tuning.OreTarget
                                                                             - stockBand
    && homeStock view
       |> Option.exists (fun stock ->
           stock > energyReserve view + view.Tuning.OreEnergyMargin + energyBand)

/// Whether the colony wants a mineral of ours dug at all this tick: Thorium
/// always — it keeps priority, and nothing but the extractor rations it — and
/// an ore while it has some left to give and `oreWanted` holds. The Harvest
/// pool's gate, beneath the extractor and container a dig also needs.
let internal digWanted (view: ColonyView) (depositId: string) : bool =
    match mineralResourceOf view depositId with
    | Some Thorium -> true
    | Some(Ore ore) -> mineralLeft view depositId > 0 && oreWanted view ore
    | _ -> false

/// Whether a deposit of ours is one the colony can actually dig this tick: it
/// is wanted, it still holds something, and its extractor stands. Only the
/// digger reads this (#361): the carrier's load comes out of a Storage, which
/// outlives the deposit that filled it. A site is not an extractor:
/// `harvest.js` refuses a mineral with none on it.
let internal depositIsDiggable (view: ColonyView) atlas (depositId: string) =
    digWanted view depositId
    && mineralLeft view depositId > 0
    && (Atlas.extractorOn atlas depositId).IsSome

/// The ore deposits of ours the colony digs this tick, in id order — what the
/// census signature reads, so the memoised hauler quota hears the dig gate.
let internal oreDug (view: ColonyView) : string list =
    ourDeposits view
    |> List.filter (fun id -> mineralResourceOf view id <> Some Thorium && digWanted view id)

/// The ores whose dig is open this tick — what the raid log writes back as
/// next tick's latch (`ColonyView.DigOpen`). Off the deposits dug, so an ore
/// no rock of ours holds is never latched.
let internal oresDigOpen (view: ColonyView) : Set<Ore> =
    oreDug view
    |> List.choose (fun id ->
        match mineralResourceOf view id with
        | Some(Ore ore) -> Some ore
        | _ -> None)
    |> Set.ofList

/// The [[lab cluster]] this colony runs: its own home's, and None at every
/// other home.
let internal ownCluster (view: ColonyView) : LabFacts option =
    view.Labs
    |> Option.filter (fun labs -> Some labs.Cluster.Home = view.Spatial.RoomName)

/// The labs standing in the home room, each beside its tile, in id order.
let internal clusterLabs (view: ColonyView) : (string * Pos) list =
    match view.Spatial.RoomName with
    | None -> []
    | Some home ->
        let layer = SpatialInfo.layerOf view.Spatial home

        SpatialInfo.idsOfKindIn view.Spatial.TargetKinds (Structure BuiltKind.Lab)
        |> List.choose (fun id ->
            Map.tryFind id layer.TargetPositions |> Option.map (fun at -> id, at))
        |> List.sortBy fst

/// The one mineral a lab holds, and how much, or None for an empty one.
let internal labHolds (view: ColonyView) (labId: string) : (Resource * int) option =
    SpatialInfo.mineralsIn view.Spatial labId |> List.tryHead

/// Whether the cluster's home runs it this tick: the home is the cluster's,
/// and its Storage stands at `Tuning.LabEnergyFloor`, or within
/// `Tuning.LabEnergyBand` under it while a lab still holds a mineral — a
/// cluster at work runs on down to the band's foot, and an idle one opens
/// only at the floor, so a Storage hovering at the line neither flips the
/// plan nor sends the reagents back and forth. Read off the labs, not
/// remembered. Under it no lab is placed and no lab works.
let internal labsFunded (view: ColonyView) : bool =
    match ownCluster view, homeStock view with
    | Some _, Some stock ->
        stock >= view.Tuning.LabEnergyFloor
        || (stock >= view.Tuning.LabEnergyFloor - view.Tuning.LabEnergyBand
            && clusterLabs view |> List.exists (fst >> labHolds view >> Option.isSome))
    | _ -> false

/// The cluster's plan this tick (`Labs.planFor`), idle at a home without the
/// cluster and under the floor.
let internal labPlan (view: ColonyView) : Labs.LabPlan =
    match ownCluster view with
    | Some labs when labsFunded view ->
        Labs.planFor labs.Cluster.Targets labs.Stock (clusterLabs view) (labHolds view)
    | _ -> Labs.idle

/// The cluster's home and the ore it lacks (`Labs.oreShort`), for a colony
/// that ships it ore: None at the cluster's own home and where none is
/// declared, and an empty map while the cluster wants for nothing.
let internal oreForCluster (view: ColonyView) : (string * Map<Ore, int>) option =
    match view.Labs with
    | Some labs when Some labs.Cluster.Home <> view.Spatial.RoomName ->
        Some(labs.Cluster.Home, Labs.oreShort labs.Cluster.Targets labs.Stock)
    | _ -> None

/// The colony's standing terminals, in id order.
let internal ourTerminals (view: ColonyView) : string list =
    SpatialInfo.idsOfKindIn view.Spatial.TargetKinds (Structure BuiltKind.Terminal)
    |> List.filter (SpatialInfo.placedAway view.Spatial >> not)

/// The rooms a `send` between two homes is priced over: Screeps prices it
/// over `Game.map.getRoomLinearDistance`, which is Chebyshev — the diagonal of
/// a room-name grid costs one room, not two — and every other distance in this
/// tree is the hop count (`RoomName.hopsBetween`). Read the engine's metric
/// here and not the walk's: the fee is the engine's to charge.
let internal sendRange (home: string) (destination: string) : int option =
    RoomName.offsetOf home destination
    |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

/// The fee this colony's terminals pay to ship the cluster every unit of ore
/// they hold that it lacks (`Engine.sendFee`), and 0 for a colony shipping it
/// none.
let internal oreShipFee (view: ColonyView) : int =
    match oreForCluster view, view.Spatial.RoomName with
    | Some(destination, short), Some home ->
        let shipped =
            short
            |> Map.toList
            |> List.sumBy (fun (ore, wanted) ->
                min
                    wanted
                    (ourTerminals view |> List.sumBy (SpatialInfo.heldIn view.Spatial (Ore ore))))

        match sendRange home destination with
        | Some range when shipped > 0 -> Engine.sendFee range shipped
        | _ -> 0
    | _ -> 0

/// The energy a terminal of this colony keeps for `send`'s fee: the
/// consignment's `Tuning.TerminalEnergy` for a colony with a consignee, and
/// for the ore the cluster lacks that ore's own fee and a margin over it —
/// 3,000 U two rooms out is about 200 — whichever is more; none otherwise.
let internal feeReserve (view: ColonyView) : int =
    let consignment =
        if Option.isSome view.Consignee then
            view.Tuning.TerminalEnergy
        else
            0

    // A hundred over: the fee is rounded up per send, and a top-up arriving
    // between the Refill and the send still pays.
    let ore =
        match oreShipFee view with
        | 0 -> 0
        | fee -> fee + 100

    max consignment ore

/// Whether more ore is still on its way to the Storage: a diggable Thorium
/// deposit of ours, or ore already standing in a mineral container. The
/// question the last load turns on.
let internal oreStillComing (view: ColonyView) atlas : bool =
    ourDeposits view
    |> List.exists (fun id ->
        mineralResourceOf view id = Some Thorium && depositIsDiggable view atlas id)
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

/// The errands there is ore for (#420): ore stands somewhere it could still
/// reach the Reactor — a Storage or Terminal of ours, a diggable deposit, a
/// mineral container, a body's hold, the floor beside the Reactor, or the
/// Reactor's own store while it burns for us or an ally. What the
/// garrison and the re-claimer are kept for; the declaration outlives it, so
/// the ore's return reopens the errand, and a flag taken off an empty Reactor
/// costs nothing (`claimReactor` has no precondition). Colony-wide rather than
/// per errand: the ore is the colony's, and any Reactor may take it.
let internal fuelledErrands (view: ColonyView) : Errand list =
    let held kinds =
        view.Spatial.TargetKinds
        |> Map.exists (fun id kind ->
            List.exists (sameKind kind) kinds
            && SpatialInfo.heldIn view.Spatial Thorium id > 0)

    let extractorTiles =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Extractor)
        |> List.choose (SpatialInfo.placementOf view.Spatial)
        |> Set.ofList

    // `depositIsDiggable` without the Atlas the Planner does not hold: the
    // extractor's tile is the deposit's.
    let diggable depositId =
        Map.tryFind depositId view.Spatial.Thorium |> Option.defaultValue 0 > 0
        && SpatialInfo.placementOf view.Spatial depositId
           |> Option.exists (fun tile -> Set.contains tile extractorTiles)

    let fuelled =
        held [ Structure BuiltKind.Storage; Structure BuiltKind.Terminal ]
        || ourDeposits view |> List.exists diggable
        || ourMineralContainers view
           |> List.exists (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)
        || view.Creeps |> List.exists (fun creep -> creep.Thorium > 0)
        || oreBesideTheReactor view
        || view.Reactors
           |> List.exists (fun reactor ->
               reactor.Thorium > 0
               && (match reactor.Owner with
                   | ReactorOwner.Ours -> true
                   | ReactorOwner.Rival username -> Colony.isAlly username
                   | ReactorOwner.Unowned -> false))

    if fuelled then view.Errands else []

/// Whether the named room carries an owner at all — one spelling for the two
/// rules of the reserver's that read it. `reserveController` answers
/// ERR_INVALID_TARGET on a controller with an owner, *whoever* it is, so the
/// Reserve pool must not carry its controller and the row must not hire against
/// it. A rival's counts too, because the stand-down reads the *previous* tick's
/// raid log and the one tick between first sight and the withdrawal cast the
/// bank's whole reserver body at a room the engine refuses.
let private roomHasOwner (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner <> Ownership.Unowned)

/// The candidate colonies' controllers, each with the room it stands in. A
/// candidate colony is a declared home this colony does not own yet, and both
/// halves are needed: the declaration, because no projected fact distinguishes
/// a room we mean to own from a neighbour we merely mine; and the ownership,
/// because the tick the claim lands this pool empties itself, with no state
/// kept.
let internal candidateControllers (view: ColonyView) : (string * string) list =
    let takeable room =
        match Map.tryFind room view.RoomControl with
        | Some control ->
            control.Owner = Ownership.Unowned
            && RoomControlInfo.heldByOther control |> Option.isNone
        | None -> false

    let candidate room =
        List.contains room view.Declared && takeable room

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.choose (fun id ->
        match SpatialInfo.placementOf view.Spatial id with
        | Some tile when candidate tile.Room -> Some(id, tile.Room)
        | _ -> None)

/// The controllers of the rooms this colony works as outposts: every controller
/// the projection carries that is not this colony's home, stands in no room
/// somebody owns, and is no candidate colony's. The engine refuses
/// `reserveController` on an owned room, so an owned room's controller is one
/// this colony is withdrawing from rather than mining.
///
/// Ids and not rooms, because a controller the projection does not place names
/// no room and is still ours to reserve — it is the Reserve pool that reads it
/// that way, while `declaredOutposts` takes the rooms and loses the unplaced
/// one. One scan for all three readers, or the rows and the pool could
/// disagree about which rooms are ours to work.
let internal outpostControllers (view: ColonyView) : string list =
    let home = view.Controller |> Option.map (fun c -> c.Id)
    let claimed = candidateControllers view |> List.map fst |> Set.ofList

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.filter (fun id ->
        Some id <> home
        && not (Set.contains id claimed)
        && not (SpatialInfo.roomOf view.Spatial id |> Option.exists (roomHasOwner view)))

/// The rooms of `outpostControllers`' answer, each once.
let internal declaredOf (view: ColonyView) (controllers: string list) : string list =
    controllers |> List.choose (SpatialInfo.roomOf view.Spatial) |> List.distinct

/// The declared outposts this colony works this tick — the rooms the guard row
/// is hired per and the rooms the reserver row starts from, written once so
/// the two cannot disagree. A room qualifies by carrying a controller of its
/// own in the projection that is not this colony's home, unowned, and not a
/// candidate colony's.
///
/// Declared and not posted (#131): gating on a standing container deadlocks
/// the outpost chain, since the container needs vision, vision needs a creep,
/// and the reserver is the only creep with a reason to go. The scan set is the
/// gate that remains, narrowed inside `World.scanOf` by the stand-down and by
/// `Outpost.withinHopBudget`; what *says* so is `Outpost.refused` on the
/// layout record.
///
/// Off the projection itself (`SpatialInfo.placementOf`) and not off the
/// Atlas's join of it: the Planner's first half is handed the view and no
/// Atlas. The derivation itself is `outpostControllers`: this takes its rooms,
/// the Reserve pool takes its ids once `reservableControllers` has dropped the
/// controllers somebody else holds, and the reserver row takes the rooms of
/// *that*.
let internal declaredOutposts (view: ColonyView) : string list =
    declaredOf view (outpostControllers view)

/// The declared outposts a threat stands in this tick. A Threat and never "a
/// hostile".
///
/// Vision is not the whole of what it reads (#366): the vision in an
/// *unguarded* outpost is the anchor, the hauler and the reserver, which are
/// precisely what a raid kills, and read off vision alone the Guard left the
/// pool while the cast body stood idle at home. So a tick with vision decides
/// the room either way, and a tick without reads the last look's record
/// (`view.ThreatenedOutposts`), which ends by its own clock
/// (`Tuning.ThreatMemory`) because nothing in the engine counts a raid down. A
/// room never looked into is in neither half: this rule buys a body, not a look.
let private guardedOutpostsOf (view: ColonyView) (declared: string list) : string list =
    if List.isEmpty view.Hostiles && Set.isEmpty view.ThreatenedOutposts then
        []
    else
        declared
        |> List.filter (fun room ->
            let seenArmed =
                view.Hostiles
                |> List.exists (fun h -> h.Pos.Room = room && HostileInfo.armed view.Hostiles h)

            // The union of the two halves and not a two-armed test on vision,
            // so the blind half can never take a room the tick's own hostiles
            // put in: the record is the previous tick's, and this tick's raid
            // outranks it wherever both answer.
            seenArmed
            || (Set.contains room view.ThreatenedOutposts
                && not (Map.containsKey room view.RoomControl)))

/// The declared outposts a level-0 invader core stands in (#487): it holds the
/// controller against the reserver for as long as it stands, and a guard
/// kills it. Vision alone: a core never moves, so the room's own bodies see it.
let private coredOutpostsOf (view: ColonyView) (declared: string list) : string list =
    match expansionCores view with
    | [] -> []
    | cores ->
        declared
        |> List.filter (fun room -> cores |> List.exists (fun core -> core.RoomName = room))

/// ADR-0077
/// The errand rooms the ranger row keeps a body in: every one this colony works
/// this tick, raid or none — the ranger stands on the Reactor's ring and meets
/// what comes (`Laid.Resident`), while there is ore to burn in it
/// (`fuelledErrands`, #420). A held errand and a withdrawn one are out of
/// `view.Errands` before this reads it.
let private guardedErrandsOf (fuelled: Errand list) : string list =
    fuelled
    |> List.map (fun errand -> errand.RoomName)
    |> List.distinct
    |> List.sort

/// The children's homes this colony defends (`BorrowedWork.Defended`) that an
/// armed hostile stands in this tick. The scan set already asked the harder
/// question; this is the vision half every other guarded room is read by.
let private defendedHomesOf (view: ColonyView) : string list =
    view.Borrowed.Defended
    |> List.filter (fun room ->
        view.Hostiles
        |> List.exists (fun h -> h.Pos.Room = room && HostileInfo.armed view.Hostiles h))

/// Whether the home stands raided by bodies nothing of ours answers but its
/// own row's cast (#568): an armed body we may strike, out of safe mode,
/// that no Defend meets — one inside the line, or any where the home holds
/// no line a defender stands on (`raidAtLine`, `perimeterRamparts`) —
/// priced with the healers beside it against the home's towers, which lose
/// it; and the home's own row, where the verdict weighs it, wins it. Never a
/// child's home (`ColonyView.Mother`): its mother guards it while its towers
/// cannot hold it (`defendedHomesOf`, #428), and its own bank buys a body
/// that dies before it matters. A row that loses casts nothing either.
let private homeUnheld (view: ColonyView) atlas : bool =
    let home = SpatialInfo.homeName view.Spatial

    let shielded =
        Map.tryFind home view.RoomControl
        |> Option.exists (fun control -> control.SafeMode)

    let inHome =
        view.Hostiles
        |> List.filter (fun h -> h.Pos.Room = home && not (HostileInfo.isKeeper h))

    if
        Option.isSome view.Mother
        || shielded
        || inHome |> List.forall (HostileInfo.armed view.Hostiles >> not)
    then
        false
    else
        let lined = keepsRamparts view && not (List.isEmpty (perimeterRamparts view))

        let atLine =
            if lined then
                snd (raidAtLine view atlas) |> List.map (fun h -> h.Id) |> Set.ofList
            else
                Set.empty

        let raid =
            inHome
            |> List.filter (fun h ->
                not (Set.contains h.Id atLine)
                && (HostileInfo.armed view.Hostiles h && mayStrike view Set.empty h
                    || HostileInfo.heals view.Hostiles h))

        // The home's own towers (`LoadedTowers` carries only the raised
        // children's homes): each holding a shot's energy, and each dry one
        // the stock refills whole, so a tick's refill gap casts nothing; only
        // towers the stock cannot feed are lost to the raid.
        let loaded, dry =
            view.Refillables
            |> List.filter (fun r -> r.Kind = BuiltKind.Tower)
            |> List.partition (fun r ->
                Engine.towerCapacity - r.FreeCapacity >= Engine.towerEnergyCost)

        let towers =
            List.length loaded
            + min (List.length dry) (stockedEnergy view / Engine.towerCapacity)

        raid |> List.exists (HostileInfo.armed view.Hostiles)
        && not (
            exchangeAgainst
                view
                Reaction.strike
                raid
                (towers * Engine.towerAttackAt Engine.towerFalloffRange)
                (towers * Engine.towerHits)
                []
        )
        && (verdictOf view home).Rows
           |> Map.tryFind home
           |> Option.exists (fun row -> row.Wins)

/// The rooms whose Guard `planTasks` pools whatever kind claims them, and the
/// rooms the fighting rows cast for (`OutpostFacts.Guarded`), or a Guard is
/// pooled with no body bought or a body cast with no Task: the declared
/// outposts a threat or a level-0 core (#487) stands in, the errand rooms the
/// ranger row keeps a body in (#414), the children's homes this colony raises
/// (#447) or defends (#428), the harassment rooms it casts (#432), and its
/// own home while a raid there is unheld (`homeUnheld`, #568). Each
/// room once: a raised home that is beaten is one Guard and one row's count,
/// not two. The Brief reads it too (#542): a kind claims a room only where its
/// claim pools something.
let internal guardedRooms (view: ColonyView) atlas (declared: string list) (fuelled: Errand list) =
    guardedOutpostsOf view declared
    @ coredOutpostsOf view declared
    @ guardedErrandsOf fuelled
    // Raid or none, as an errand room is kept.
    @ (raisedHomes view |> Set.toList)
    @ defendedHomesOf view
    // Target or none, as an errand room is kept.
    @ (harassRooms view |> Set.toList)
    @ (if homeUnheld view atlas then
           [ SpatialInfo.homeName view.Spatial ]
       else
           [])
    |> List.distinct

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

/// The rooms a colony answers a raid in by its own rows and towers: its home,
/// its declared outposts, its ranger rooms (`rangerRooms`, a raised child's
/// home among them), the harassment rooms it casts and the children's homes
/// it defends. Whose row the world's verdict weighs (`priceAmong`), a home's
/// own only where no other colony's row casts for it.
let private castsFor (view: ColonyView) : Set<string> =
    Set.unionMany
        [
            Set.singleton (SpatialInfo.homeName view.Spatial)
            Set.ofList (declaredOutposts view)
            rangerRooms view
            view.HarassCast
            Set.ofList view.Borrowed.Defended
        ]

/// The rooms a colony prices a raid in, seen or not: those it declares
/// (`Colony.declaredRooms`), its declared outposts, its ranger rooms and the
/// harassment rooms it casts. The one set the world's verdicts (`judge`) and
/// the danger log's fold (`Observe.Danger.foldColony`) price its stand-down
/// rows over.
let pricedRooms (declared: Set<string>) (view: ColonyView) : Set<string> =
    Set.unionMany
        [
            declared
            Set.ofList (declaredOutposts view)
            rangerRooms view
            view.HarassCast
        ]

/// One colony as `judge` is handed it.
type Judged =
    {
        /// Its view, as cut.
        Cut: ColonyView
        /// The rooms of the stand-down rows it prices out of sight
        /// (`Observe.pricedRaidRows`), which the world's look is laid into.
        OutOfSight: string list
        /// Whether it decides this tick: one that replays its plan sends a
        /// room only the bodies already holding the room's Guard.
        Decides: bool
    }

/// The world layer's verdicts this tick (its S3): each room a raider stands in, in
/// any colony's view or in a stand-down row of its own the world looks into
/// (`ColonyView.lookingInto`), priced once and on first read over every
/// colony's view (`priceAmong`) and laid in every view, so one room has one
/// verdict whoever reads it. Each view comes with the stand-down rows its
/// colony prices out of sight and whether it decides this tick (`Judged`);
/// `held` is the shard's assignments, what a replaying colony's bodies hold;
/// `hops` is the crossings from a home to a room, the answerer's rank.
let judge
    (hops: string -> string -> int option)
    (held: Assignments)
    (colonies: Judged list)
    : ColonyView list =
    let pricers =
        colonies
        |> List.map (fun colony ->
            {
                Home = SpatialInfo.homeName colony.Cut.Spatial
                View = ColonyView.lookingInto colony.OutOfSight colony.Cut
                Casts = lazy (castsFor colony.Cut)
                Sends =
                    if colony.Decides then
                        fun _ _ -> true
                    else
                        fun room creep -> Map.tryFind creep.Name held = Some(taskId (Guard room))
            })

    let verdicts =
        pricers
        |> List.collect (fun p ->
            p.View.Hostiles
            |> List.filter (HostileInfo.raider p.View.Hostiles)
            |> List.map (fun hostile -> hostile.Pos.Room))
        |> List.distinct
        |> List.map (fun room -> room, lazy (priceAmong hops pricers room))
        |> Map.ofList

    colonies
    |> List.map (fun colony ->
        { colony.Cut with
            World =
                { colony.Cut.World with
                    Answers = verdicts
                }
        })
