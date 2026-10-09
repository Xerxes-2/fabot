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

/// Whether a room is one of ours: the home, or one whose controller this
/// colony sees ours.
let internal ourRoom (view: ColonyView) (room: string) : bool =
    room = SpatialInfo.homeName view.Spatial
    || Map.tryFind room view.RoomControl
       |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// Whether a hostile is dismantling ours (#429): it works with a WORK in the
/// home, beside a Keep structure or rampart there that lost hits since the
/// last decided tick (`ColonyView.KeepHitsBefore`) — never a rampart's loss
/// of whole decays, nor a loss under an armed hostile's reach, which is
/// that one's. The evidence a neutral's body is read by: one crossing the
/// home, a pioneer or a miner, dismantles nothing.
let dismantling (view: ColonyView) (hostile: HostileInfo) : bool =
    let home = SpatialInfo.homeName view.Spatial

    not (Map.isEmpty view.KeepHitsBefore)
    && hostile.Pos.Room = home
    && HostileInfo.works view.Hostiles hostile
    && (let tiles = (SpatialInfo.layerOf view.Spatial home).TargetPositions
        let at = RoomPos.pos hostile.Pos

        // Whoever's weapon reaches the tile takes the blame for its loss.
        let armedNear (tile: Pos) =
            view.Hostiles
            |> List.exists (fun h ->
                h.Pos.Room = home
                && HostileInfo.armed view.Hostiles h
                && range tile (RoomPos.pos h.Pos) <= Engine.rangedRange)

        SpatialInfo.structureHits view.Spatial
        |> List.exists (fun (id, kind, hits) ->
            match Map.tryFind id view.KeepHitsBefore, Map.tryFind id tiles with
            | Some before, Some tile ->
                hits.Hits < before
                // A baseline a light colony left some ticks old has seen
                // whole decays: any whole number of them is no dismantle.
                && not (
                    kind = BuiltKind.Rampart
                    && (before - hits.Hits) % Engine.rampartDecayAmount = 0
                )
                && range tile at <= Engine.meleeRange
                && not (armedNear tile)
            | _ -> false))

/// Whether a hostile endangers the room of ours it stands in (#429):
/// `HostileInfo.endangers` (armed, or an enemy's or an Invader's WORK body),
/// or any other's but an ally's caught dismantling ours (`dismantling`).
let internal endangersOurs (view: ColonyView) (hostile: HostileInfo) : bool =
    HostileInfo.endangers view.Hostiles hostile
    || not (Colony.isAlly hostile.Owner)
       && not (HostileInfo.isKeeper hostile)
       && dismantling view hostile

/// Whether a hostile threatens the room it stands in (#429): an armed one
/// anywhere, and in a room of ours one that endangers it (`endangersOurs`).
let internal threatens (view: ColonyView) (hostile: HostileInfo) : bool =
    HostileInfo.armed view.Hostiles hostile
    || ourRoom view hostile.Pos.Room && endangersOurs view hostile

/// Whether a raider stands in a room (`HostileInfo.raider`), or in a room of
/// ours a body that endangers it (`endangersOurs`, #429).
let internal raided (view: ColonyView) (room: string) : bool =
    let owned = ourRoom view room

    view.Hostiles
    |> List.exists (fun h ->
        h.Pos.Room = room
        && (HostileInfo.raider view.Hostiles h || owned && endangersOurs view h))

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

/// What the broken line (`brokenLine`) is short of its floor, by id, in hits:
/// a weak rampart its floor less its hits, a site on the line its whole floor.
/// The hits a raid meets fewer of there, which the pool prices under the
/// home's raid hazard.
let internal lineShortfall (view: ColonyView) : Map<string, int> =
    let weak, sites = brokenLine view

    if Set.isEmpty weak && Set.isEmpty sites then
        Map.empty
    else
        let cap = buildFloor view

        let floorAt tile =
            Map.tryFind tile view.RampartFloors
            |> Option.defaultValue view.Tuning.RampartFloor
            |> capped cap

        let ramparts =
            perimeterRamparts view
            |> List.choose (fun (id, tile, hits) ->
                if Set.contains id weak then
                    Some(id, max 0 (floorAt (RoomPos.pos tile) - hits))
                else
                    None)

        let siteFloors =
            sites
            |> Set.toList
            |> List.choose (fun id ->
                SpatialInfo.placementOf view.Spatial id
                |> Option.map (fun tile -> id, floorAt (RoomPos.pos tile)))

        Map.ofList (ramparts @ siteFloors)

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

/// Every home of ours this colony knows, any colony's (#602): its own, each
/// room whose controller it sees ours, and the children it raises. A home is
/// fought for to the death, never broken off.
let internal homesOfOurs (view: ColonyView) : Set<string> =
    view.RoomControl
    |> Map.filter (fun _ control -> control.Owner = Ownership.Ours)
    |> Map.keys
    |> Set.ofSeq
    |> Set.add (SpatialInfo.homeName view.Spatial)
    |> Set.union (raisedHomes view)

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
    && ourRoom view hostile.Pos.Room

/// Whether a hostile is a declared enemy's economy body (`Colony.enemies`):
/// working or carrying with a WORK, CARRY or CLAIM, on paper and in effect.
/// The trespasser a priced cast is bought against (#652); a body shot
/// disarmed or a healer left bare is the fight's to finish, never a cast's.
let internal enemyEconomy (side: HostileInfo list) (hostile: HostileInfo) =
    let takes part =
        part = Work || part = Carry || part = BodyPart.Claim

    Colony.isEnemy hostile.Owner
    && List.exists takes hostile.Body
    && List.exists takes (HostileInfo.effectiveParts side hostile)

/// Whether a hostile's body trespasses wherever it stands (#516), among
/// `side`, unarmed in effect and one of three bodies:
/// - shot disarmed: a weapon on paper and none in effect (#528, any owner's
///   since #535), so it strikes nothing back;
/// - a healer shot bare (#598): a HEAL on paper and none live, beside its
///   raid or not. Its raiders still fighting leave it nobody's healer, and
///   it is struck where it stands, as a cripple shot disarmed is;
/// - a raid's healer whose raiders are gone (#597): a HEAL live, with no
///   raider in its room or the rooms beside it. It heals its own parts
///   back, or the next raid's; beside a raider, or one just across the
///   exit, it is that raid's;
/// - a declared enemy's (`Colony.enemies`) working or carrying with a WORK,
///   CARRY or CLAIM. A hauler carries our dug energy off; a bare scout takes
///   nothing.
/// Whose it may be is `mayStrike`'s.
let internal trespassingBody (side: HostileInfo list) (hostile: HostileInfo) =
    let raidGone () =
        let room = hostile.Pos.Room
        let beside = RoomName.adjacent room

        not (
            side
            |> List.exists (fun other ->
                (other.Pos.Room = room || List.contains other.Pos.Room beside)
                && HostileInfo.raider side other)
        )

    not (HostileInfo.armed side hostile)
    && (Option.isSome (HostileInfo.weaponRangeOf hostile.Body)
        || List.contains Heal hostile.Body
           && (not (HostileInfo.heals side hostile) || raidGone ())
        || enemyEconomy side hostile)

/// Whether this colony mines a room: its home, or a room of its sources.
let internal minesRoom (view: ColonyView) (room: string) =
    room = SpatialInfo.homeName view.Spatial
    || view.Sources
       |> List.exists (fun source -> SpatialInfo.roomOf view.Spatial source.Id = Some room)

/// Whether a trespasser in a room is this colony's to kill: a room it mines
/// (#516), or a child's outpost it watches for the residents next door
/// (`BorrowedWork.ChildOutposts`, #517), or an outpost of ours a raid has
/// shut once no raider stands there in sight or recalled (#585): the
/// stand-down withholds the economy's work there, never the kill of a body
/// taking it, and while the raid stands the room is its fight's — a chase
/// where the verdict does not lose it (`chased`), nobody's where it does —
/// so no body is cast or seated at the prey under the raid. Read off the
/// raid and not the verdict, which prices trespassers itself.
let internal trespassRooms (view: ColonyView) (room: string) =
    minesRoom view room
    || List.contains room view.Borrowed.ChildOutposts
    || view.RaidedOutposts.Contains room
       && view.MinedRooms.Contains room
       && not (
           view.Hostiles
           |> List.exists (fun h -> h.Pos.Room = room && HostileInfo.raider view.Hostiles h)
       )

/// Whether a neutral player's creep struck one of ours first, which would
/// make it ours to strike back anywhere: deferred, no hit memory yet (#535).
let internal struckUsFirst (_hostile: HostileInfo) = false

/// The share of `mayStrike` its owner alone settles, wherever the body
/// stands: a declared enemy's or an Invader's, never a keeper's nor an
/// ally's. What a light tick's glance, which holds no view, strikes of a body
/// no full tick named (#671); a neutral's waits for a full tick's rooms and
/// keep hits.
let internal mayStrikeAnywhere (owner: string) =
    not (HostileInfo.isKeeperOwner owner)
    && not (Colony.isAlly owner)
    && Colony.strikesAnywhere owner

/// The one diplomacy rule every strike reads (#535, the user 2026-10-06):
/// never a Source Keeper (not provoked) nor an ally; a declared enemy's and
/// an NPC Invader's creep anywhere; any other armed one — a weapon on paper,
/// so a cripple shot disarmed counts — only where it stands in a room we
/// work or hold (`trespassRooms`, our home, a room whose fight we hold, in
/// `engaged`); a neutral's WORK body only caught dismantling ours
/// (`dismantling`, #429: the narrow rule until the user rules on a wider
/// one), never one crossing the home; and a neutral that struck us first
/// (`struckUsFirst`).
let internal mayStrike (view: ColonyView) (engaged: Set<string>) (hostile: HostileInfo) =
    mayStrikeAnywhere hostile.Owner
    || not (HostileInfo.isKeeper hostile)
       && not (Colony.isAlly hostile.Owner)
       && (Option.isSome (HostileInfo.weaponRangeOf hostile.Body)
           && (trespassRooms view hostile.Pos.Room || Set.contains hostile.Pos.Room engaged)
           || dismantling view hostile
           || struckUsFirst hostile)

/// Whether a hostile is a [[trespasser]] (#516): its body trespasses, we may
/// strike it (`mayStrike`), and it stands in a room this colony kills
/// trespassers in, or one whose fight it holds (`engaged`, #535): a cripple
/// left there is the fight's to finish; or it is the declared enemy's in the
/// harassment room that names it (`harassTarget`): one economic war over a
/// room, ours or theirs (#656). The body is asked first, so a tick with no
/// such body walks no source.
let internal trespasser (view: ColonyView) (engaged: Set<string>) (hostile: HostileInfo) =
    trespassingBody view.Hostiles hostile
    && mayStrike view engaged hostile
    && (trespassRooms view hostile.Pos.Room
        || Set.contains hostile.Pos.Room engaged
        || harassTarget view hostile)
    // In the sweep's room every unarmed body of the enemy's is prey, a
    // cripple shot disarmed or bare among them: whether it is worth its kill
    // against moving on is its kill score's and the sweep's price (#648,
    // #658), not its parts' category.
    || harassTarget view hostile && not (HostileInfo.armed view.Hostiles hostile)

/// What a Guard in its room may shoot, every target reader's one predicate
/// (`Emitter.strikeOrder`, the kite's head): a threat (`threatens`, a
/// dismantler in a room of ours too) we may strike (`mayStrike`, #535: in a
/// harassment room a keeper or a third party's is no target), a rival's
/// claimer or a raid's healer (#451)
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
    threatens view hostile
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
/// threat (`threatens`), whoever's — an armed neutral is still a threat to a
/// home its garrison would leave —, a rival's claimer or a raid's healer in a
/// resident room, the declared enemy's creep in a harassment room, or a
/// trespasser. The threat question, apart from the target one
/// (`guardShoots`, #535).
let internal disturbs (view: ColonyView) (residentRooms: Set<string>) (hostile: HostileInfo) =
    threatens view hostile
    || claimsAFlag view.Hostiles residentRooms hostile
    || harassTarget view hostile
    || Set.contains hostile.Pos.Room residentRooms
       && (raidHealer view hostile
           || healerAtHome view hostile && mayStrike view residentRooms hostile)
    || trespasser view Set.empty hostile

/// The tiles of one projected room the strike rules ask after, read off its
/// layer once: its sources', its controller's, its standing ramparts not
/// ours (`underForeignRampart`), these by tile index (`indexOf`) where the
/// tile is on the grid, and ours at their hits by tile (`rampartUnder`).
type private StrikeTiles =
    {
        Sources: Pos list
        Controllers: Pos list
        ForeignRamparts: System.Collections.Generic.HashSet<int>
        ForeignRampartsOffGrid: Pos list
        OurRamparts: Map<Pos, int>
    }

let private strikeTilesIn (spatial: SpatialInfo, room: string) : StrikeTiles =
    let ramparts = System.Collections.Generic.HashSet<int>()
    let mutable offGrid = []
    let mutable sources = []
    let mutable controllers = []
    let mutable ours = Map.empty

    for KeyValue(id, tile) in (SpatialInfo.layerOf spatial room).TargetPositions do
        match Map.tryFind id spatial.TargetKinds with
        | Some Source -> sources <- tile :: sources
        | Some Controller -> controllers <- tile :: controllers
        | Some(Structure BuiltKind.Rampart) ->
            match Map.tryFind id spatial.Hits with
            | Some hits -> ours <- Map.add tile hits.Hits ours
            | None when inGrid tile -> ramparts.Add(indexOf tile) |> ignore
            | None -> offGrid <- tile :: offGrid
        | _ -> ()

    {
        Sources = sources
        Controllers = controllers
        ForeignRamparts = ramparts
        ForeignRampartsOffGrid = offGrid
        OurRamparts = ours
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

/// How many sources a room holds, off its projection: the memo's where it is
/// derived (`strikeTilesOf`), else the direct count, so a room's first asks
/// a tick read its sources as its later ones do (#656: they read none).
let private sourcesIn (view: ColonyView) (room: string) : int =
    match strikeTilesOf view room with
    | Some tiles -> List.length tiles.Sources
    | None ->
        (SpatialInfo.layerOf view.Spatial room).TargetPositions
        |> Map.filter (fun id _ -> Map.tryFind id view.Spatial.TargetKinds = Some Source)
        |> Map.count

/// Whether a hostile stands within a step of a target of this kind in its room.
let private besideTargetOf (view: ColonyView) (kind: TargetKind) (hostile: HostileInfo) =
    let beside (tiles: Pos list) =
        tiles |> List.exists (fun tile -> range tile (RoomPos.pos hostile.Pos) <= 1)

    match kind, strikeTilesOf view hostile.Pos.Room with
    | Source, Some tiles -> beside tiles.Sources
    | Controller, Some tiles -> beside tiles.Controllers
    | _ ->
        Quarry.besideTarget
            (SpatialInfo.layerOf view.Spatial hostile.Pos.Room).TargetPositions
            view.Spatial.TargetKinds
            kind
            (RoomPos.pos hostile.Pos)

/// Whether an unarmed worker stands on a seat it digs from: WORK in effect,
/// no weapon, within a step of a source of its room.
let private onDigSeat (view: ColonyView) (hostile: HostileInfo) =
    HostileInfo.works view.Hostiles hostile
    && not (HostileInfo.armed view.Hostiles hostile)
    && besideTargetOf view Source hostile

/// Whether a [[trespasser]] stands rooted at its work (`Quarry.rooted`,
/// #550): a digger on its seat, or a CLAIM body beside its room's controller,
/// which it reserves or attacks from there; one crossing the room is not.
let internal rootedAtWork (view: ColonyView) (hostile: HostileInfo) =
    Quarry.rooted (fun kind -> besideTargetOf view kind hostile) view.Hostiles hostile

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

/// Whether a hostile closes onto a body of ours where the range is its own to
/// pick (#535): it carries an ATTACK in effect and closing costs it nothing
/// it keeps off at its shot's range — the body deals as much from three as
/// from one, so the hostile closes to swing beside its shot —, or its swing
/// is its weapon, matching or outdealing its shot (the engine's smallMelee,
/// one gun beside its ATTACK, closes on a swing too); or it is an Invader
/// carrying a live ATTACK, which walks onto its target whatever else it
/// carries (#568: `findAttack.js` reads the active parts, never healed-back
/// ones). Anything else keeps its shot's range: a body fighting with its gun
/// against a swing, one token ATTACK among its guns not bringing it in, and
/// its healers behind it; an Invader with no ATTACK, which flees every
/// hostile inside three; and a body with no weapon, which keeps off a swing
/// and never off a shot.
let private closesIn (side: HostileInfo list) (fighter: Fighter) (hostile: HostileInfo) : bool =
    let parts = HostileInfo.effectivePower side hostile

    parts Attack > 0
    && (fighter.DamageAt Engine.rangedRange >= fighter.DamageAt Engine.meleeRange
        || Engine.attackPower * parts Attack
           >= Engine.rangedAttackPower * parts RangedAttack)
    || not (HostileInfo.crossesExits hostile)
       && List.contains Attack (Engine.liveParts hostile.Body hostile.Hits)

/// Where a creep of ours stands: its colony's projection, else the world's
/// census of every body (`WorldView.Bodies`).
let internal placementOf (view: ColonyView) (name: string) : RoomPos option =
    SpatialInfo.creepPlacementOf view.Spatial name
    |> Option.orElseWith (fun () ->
        view.World.Bodies.Value
        |> Map.toSeq
        |> Seq.tryPick (fun (here, bodies) ->
            bodies
            |> List.tryPick (fun (body, pos) ->
                if body.Name = name then Some(RoomPos.at here pos) else None)))

/// The tiles between two positions: their range within one room, else across
/// the world grid (`RoomName.tileRange`).
let private tilesBetween (a: RoomPos) (b: RoomPos) : int option =
    RoomPos.range a b |> Option.orElseWith (fun () -> RoomName.tileRange a b)

/// One healing body as a fight reads it (#651): its live HEAL's rate
/// (`HostileInfo.healing`), whether it bounces on the exit (in the room every
/// other tick, so at half that over the two), and which of the engine's two
/// heals it casts beside what it fights with: `CastsHeal`, a `heal` beside
/// its patient or on itself, which drops its swing; `CastsRangedHeal`, a
/// `rangedHeal` at two or three, which drops its swing and its shot
/// (`intents.js` priorities).
type private Healer =
    {
        Mender: HostileInfo
        HealRate: int
        Bounces: bool
        CastsHeal: bool
        CastsRangedHeal: bool
    }

/// The heal one healer puts on `target` a tick (#651): its rate beside the
/// target or on itself (`heal`), a third of it at two or three
/// (`rangedHeal`), nothing farther, one target a tick. Read at the range it
/// closes to within `horizon` ticks, a tile each of its paces on plain, and
/// halved for one bouncing on the exit. A heal it does not cast heals nothing.
let private healOnto
    (side: HostileInfo list)
    (horizon: int)
    (healer: Healer)
    (target: HostileInfo)
    =
    match tilesBetween healer.Mender.Pos target.Pos with
    | None -> 0
    | Some range ->
        let closes =
            if horizon <= 0 || range <= Engine.meleeRange then
                0
            else
                match Gait.ticks Gait.plain (HostileInfo.gait side healer.Mender) with
                | Some pace when pace > 0 -> horizon / pace
                | _ -> 0

        let range = range - closes

        let rate =
            if range <= Engine.meleeRange && healer.CastsHeal then
                healer.HealRate
            elif range <= Engine.rangedRange && healer.CastsRangedHeal then
                healer.HealRate * Engine.rangedHealPower / Engine.healPower
            else
                0

        if healer.Bounces then rate / 2 else rate

/// Every body among `bodies` that heals, casting either heal: what nothing
/// it fights with stands against (`raidPrice` weighs the rest).
let private healersAmong
    (side: HostileInfo list)
    (bouncing: HostileInfo -> bool)
    (bodies: HostileInfo list)
    =
    bodies
    |> List.choose (fun h ->
        match HostileInfo.healing side h with
        | 0 -> None
        | healing ->
            Some
                {
                    Mender = h
                    HealRate = healing
                    Bounces = bouncing h
                    CastsHeal = true
                    CastsRangedHeal = true
                })

/// The heal `healers` put on `target` a tick (`healOnto`), together.
let private healFrom
    (side: HostileInfo list)
    (horizon: int)
    (healers: Healer list)
    (target: HostileInfo)
    =
    healers |> List.sumBy (fun healer -> healOnto side horizon healer target)

/// The hits of the rampart not ours a hostile stands on (#651,
/// `WorldRoom.RivalRamparts`): every strike at it lands there until it
/// falls, and none at it, nor any swing of its, is struck back. 0 on open
/// ground.
let private rampartOver (view: ColonyView) (hostile: HostileInfo) : int =
    Map.tryFind hostile.Pos.Room view.World.Rooms
    |> Option.bind (fun seen -> Map.tryFind (RoomPos.pos hostile.Pos) seen.RivalRamparts)
    |> Option.defaultValue 0

/// Our rampart a body of ours stands on and its hits (#651), ours by the
/// projection's hits: the raid's strikes land there until it falls, and no
/// swing from it nor at it is struck back. None for a priced body no creep
/// is yet, or one on open ground.
let private rampartUnder (view: ColonyView) (fighter: Fighter) : (RoomPos * int) option =
    match fighter.Creep |> Option.bind (placementOf view) with
    | None -> None
    | Some at ->
        let tile = RoomPos.pos at

        match strikeTilesOf view at.Room with
        | Some tiles -> Map.tryFind tile tiles.OurRamparts
        | None ->
            (SpatialInfo.layerOf view.Spatial at.Room).TargetPositions
            |> Map.tryPick (fun id placed ->
                if
                    placed = tile
                    && Map.tryFind id view.Spatial.TargetKinds = Some(Structure BuiltKind.Rampart)
                then
                    Map.tryFind id view.Spatial.Hits |> Option.map (fun hits -> hits.Hits)
                else
                    None)
        |> Option.map (fun hits -> at, hits)

/// A fixture's danger (`WorldDanger.read` off a view the world laid none
/// in), laid once for the same facts.
let private dangersMemo: TickMemo.Memo<ColonyView, Map<string, Lazy<RoomDanger option>>> =
    TickMemo.create 4 (fun a b ->
        obj.ReferenceEquals(a.World, b.World)
        && obj.ReferenceEquals(a.Spatial, b.Spatial)
        && obj.ReferenceEquals(a.Hostiles, b.Hostiles)
        && obj.ReferenceEquals(a.RoomControl, b.RoomControl)
        && obj.ReferenceEquals(a.Tuning, b.Tuning))

/// The world's danger as a view reads it (`WorldDanger.read`).
let private dangersOf (view: ColonyView) : Map<string, Lazy<RoomDanger option>> =
    match view.World.Dangers with
    | Some table -> table
    | None -> TickMemo.recall dangersMemo view WorldDanger.read

/// One kiter's run-off (`Arrival.runOff`) by the room's danger (its maps,
/// laid once a tick), the reach that picks the classes striking it first,
/// its pace and where it walks from, walked once a tick. Sized for a war
/// tick's kiters and the priced bodies' stands about each pursuer.
let private runOffMemo: TickMemo.Memo<RoomDanger * int * Pace * (int * int) list, int> =
    TickMemo.create 64 (fun (danger, reach, pace, seeds) (danger', reach', pace', seeds') ->
        obj.ReferenceEquals(danger, danger')
        && reach = reach'
        && pace = pace'
        && seeds = seeds')

/// The run-off ground a body of ours has against a hostile it out-reaches
/// (#625), in its own ticks (`Arrival.runOff`): against every class of the
/// room's maps that strikes shorter than the body reaches (each strikes it
/// before it strikes back, the hostile's own among them) and the risk an
/// unseen neighbour lands, walked from the tile the body stands on in the
/// hostile's room, its fatigue waited out; else from every tile at its
/// reach about the hostile, where a body coming to the fight takes its
/// stand. None where the room lays no map of the hostile.
let private runOffFor (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : int =
    let room = hostile.Pos.Room

    let danger =
        Map.tryFind room (dangersOf view) |> Option.bind (fun danger -> danger.Value)

    let shorter (map: ClassArrival) = map.Class.Strike < fighter.Reach

    let mapped =
        danger
        |> Option.exists (fun danger ->
            danger.HostileMaps.Classes
            |> List.exists (fun map -> shorter map && List.contains hostile.Id map.Members))

    match danger, Arrival.paceOf fighter.Gait with
    | Some danger, Some pace when mapped ->
        let walking = danger.Walking

        let seeds =
            match
                fighter.Creep
                |> Option.bind (fun name ->
                    placementOf view name |> Option.map (fun at -> name, at))
            with
            | Some(name, at) when at.Room = room ->
                let wait =
                    ColonyView.bodies view
                    |> List.tryFind (fun creep -> creep.Name = name)
                    |> Option.map (fun creep -> Arrival.waitOf creep.Fatigue fighter.Gait)
                    |> Option.defaultValue 0

                [ indexOf (RoomPos.pos at), wait ]
            | _ ->
                let centre = RoomPos.pos hostile.Pos
                let reach = fighter.Reach
                let edge = Engine.roomSide - 1

                [
                    for dx in -reach .. reach do
                        for dy in -reach .. reach do
                            let x = centre.X + dx
                            let y = centre.Y + dy

                            if
                                max (abs dx) (abs dy) = reach
                                && x > 0
                                && y > 0
                                && x < edge
                                && y < edge
                            then
                                let index = indexOf { X = x; Y = y }

                                if walking.[index] > 0 then
                                    yield index, 0
                ]

        TickMemo.recall
            runOffMemo
            (danger, fighter.Reach, pace, seeds)
            (fun (danger, _, pace, seeds) ->
                Arrival.runOff
                    danger.Walking
                    (danger.HostileMaps.Classes |> List.filter shorter)
                    danger.HostileMaps.Unseen
                    danger.Ramparts
                    pace
                    seeds
                    Engine.creepLifetime)
    | _ -> 0

/// The damage that lames or disarms a hostile body against one gait of
/// ours (`lamedIn`), by its parts, boosts and hits: worked once a tick.
/// Sized for a war tick's pursuers against each gait of ours, worn ones
/// (`Fighter.atHits`) among them.
let private lamedMemo: TickMemo.Memo<BodyPart list * Boost list * int * Gait, int> =
    TickMemo.create 128 (fun a b -> a = b)

/// The ticks a hostile struck alone at `damage` a tick takes to stop
/// pressing a body of ours that out-reaches it (#625): net of the heal its
/// side puts on it (every healer of its owner in its room that reaches it
/// within that window, `healOnto`, #651), until
/// it falls behind the body on plain
/// (`Gait.fallsBehind`), its last ATTACK falls, or it dies. Read off its
/// live body head first, as the engine destroys it (`Engine.powerAt`): a
/// body whose MOVE comes first is lamed in a few shots, one whose ATTACK
/// does is disarmed before its legs go. None where the damage never
/// out-heals it.
let private lamedIn
    (view: ColonyView)
    (ours: Gait)
    (damage: int)
    (hostile: HostileInfo)
    : int option =
    let healers =
        view.Hostiles
        |> List.filter (fun h -> h.Owner = hostile.Owner && h.Pos.Room = hostile.Pos.Room)
        |> healersAmong view.Hostiles (fun _ -> false)

    // The net damage a tick while the healers reaching it within `ticks`
    // heal it.
    let netWithin ticks =
        damage
        - HostileInfo.damageFor hostile (healFrom view.Hostiles ticks healers hostile)

    let needed =
        lazy
            (TickMemo.recall lamedMemo (hostile.Body, hostile.Boosts, hostile.Hits, ours) (fun _ ->
                let n = List.length hostile.Body
                let weight = Gait.weightOf (partCountIn hostile.Body)

                // Its head destroyed a part at a time (`Engine.powerAt`): the
                // hits it stands at once the next part is gone, and whether
                // its MOVE and ATTACK then still press the body.
                let rec lamedFrom live =
                    let hits = Engine.partHits * live
                    let power = Engine.powerAt hostile.Body hostile.Boosts hits

                    if
                        live <= 0
                        || power Attack <= 0
                        || Gait.fallsBehind ours { Weight = weight; Moves = power Move }
                    then
                        hits
                    else
                        lamedFrom (live - 1)

                let live = min n ((hostile.Hits + Engine.partHits - 1) / Engine.partHits)
                HostileInfo.damageFor hostile (max 0 (hostile.Hits - lamedFrom (live - 1)))))

    // The lame window and the heal within it read each other (#651): every
    // healer that reaches it before it is lamed heals it. Widened until the
    // window holds, the heal only growing with it; never lamed where the
    // damage stops out-healing it.
    let rec within ticks tries =
        let net = netWithin ticks

        if net <= 0 then
            None
        else
            let lamed = (needed.Value + net - 1) / net

            if lamed <= ticks || tries = 0 then
                Some lamed
            else
                within lamed (tries - 1)

    within view.Tuning.ReachHorizon 4

/// The ticks a hostile presses a body of ours that out-reaches it (#625),
/// struck by the body's own damage at its reach: one body shoots one target
/// a tick, so every hostile in the room pressing it alike (armed shorter
/// than it reaches, and not behind it on plain) is lamed in turn
/// (`lamedIn`), the soonest first, and this one presses it until its own
/// turn ends. None where its damage never out-heals this one.
let private pressesFor (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : int option =
    let side = view.Hostiles
    let damage = fighter.DamageAt fighter.Reach

    lamedIn view fighter.Gait damage hostile
    |> Option.map (fun own ->
        side
        |> List.sumBy (fun h ->
            if h.Id = hostile.Id || h.Pos.Room <> hostile.Pos.Room then
                0
            else
                match HostileInfo.weaponRange side h with
                | Some strike when
                    strike < fighter.Reach
                    && not (Gait.fallsBehind fighter.Gait (HostileInfo.gait side h))
                    ->
                    match lamedIn view fighter.Gait damage h with
                    | Some ticks when ticks < own || ticks = own && h.Id < hostile.Id -> ticks
                    | _ -> 0
                | _ -> 0)
        |> (+) own)

/// Whether a body of ours keeps a hostile off at its own reach (#615 U1,
/// #625), given the hostile's gait and the range it strikes from: the
/// hostile strikes shorter than the body reaches, and falls behind it on
/// plain now (`Fighter.standsOff`'s floor), or, armed and as quick, presses
/// it (`pressesFor`, the body's own damage at its reach) no longer than the
/// run-off ground the body has (`runOffFor`): with both stepping at once, a
/// pursuer at its pace never closes while the body has ground to step
/// onto. One quicker presses it no longer than it takes to close the
/// ground between their reaches, too. A body of ours to be lamed meanwhile
/// is priced at its worn gait by its reader (`winsBeside`'s `atHits`).
let private keepsOffAt
    (view: ColonyView)
    (fighter: Fighter)
    (hostile: HostileInfo)
    (gait: Gait)
    (strike: int)
    : bool =
    strike < fighter.Reach
    && (Gait.fallsBehind fighter.Gait gait
        || strike > 0
           && (match Gait.ticks Gait.plain fighter.Gait, Gait.ticks Gait.plain gait with
               | Some ours, Some theirs ->
                   pressesFor view fighter hostile
                   |> Option.exists (fun presses ->
                       (theirs = ours
                        || presses < (fighter.Reach - strike) * ours * theirs / (ours - theirs))
                       && presses <= runOffFor view fighter hostile)
               | _ -> false))

/// Whether a body of ours keeps a hostile off at its own reach
/// (`keepsOffAt`): the one reading the exchange, the admission, the hold
/// and the kite walk take of who picks the range.
let internal keepsOff (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : bool =
    let side = view.Hostiles

    keepsOffAt
        view
        fighter
        hostile
        (HostileInfo.gait side hostile)
        (HostileInfo.weaponRange side hostile |> Option.defaultValue 0)

/// Who picks the range a body of ours and a hostile fight at (`standoffPick`).
[<RequireQualifiedAccess>]
type private Picker =
    | Kites
    | Closes
    | Theirs

/// The range a body of ours and one hostile fight at (#615 U2), the faster
/// side's pick on plain, the ground a fight crosses. Ours where it keeps the
/// hostile off (`keepsOffAt`): its reach, the standoff a kite holds (#510).
/// Ours too where the hostile falls behind it or cannot keep away, rooted on
/// its legs (`HostileInfo.stationary`) or at its work (`rootedAtWork`):
/// where it deals most (`Fighter.bestRange`). Else the hostile's
/// (`closesIn`): onto us, or its shot's range, which keeps it off a swing.
/// Beside the range, who picks it: `Kites` (ours, walking off it),
/// `Closes` (ours, walking to it), or `Theirs` (the hostile comes to it).
let private standoffPick (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) =
    let side = view.Hostiles
    let gait = HostileInfo.gait side hostile
    let strike = HostileInfo.weaponRange side hostile |> Option.defaultValue 0

    if keepsOffAt view fighter hostile gait strike then
        fighter.Reach, Picker.Kites
    elif
        Gait.fallsBehind fighter.Gait gait
        || Gait.stationary gait
        // Only a body with no weapon is rooted at work (`onDigSeat`), or
        // brought to a swing by its claim (#550).
        || strike = 0 && rootedAtWork view hostile
    then
        Fighter.bestRange fighter, Picker.Closes
    elif closesIn side fighter hostile then
        Engine.meleeRange, Picker.Theirs
    else
        Engine.rangedRange, Picker.Theirs

/// The range a body of ours and one hostile fight at (`standoffPick`).
let internal standoff (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : int =
    fst (standoffPick view fighter hostile)

/// Whether a body of ours strikes a hostile at their standoff (`standoff`):
/// what the guard's swing catches of a [[trespasser]] (#550) — a hauler at its
/// pace keeps off a swing and never off a shot — and the [[defender]] meets of
/// a raid at the home's line (#546), where a raider that must close to hurt
/// our work comes to its rampart and a shooter keeping its range never does.
let internal strikesAtStandoff (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : bool =
    fighter.DamageAt(standoff view fighter hostile) > 0

/// A row's block counts `blocks` times over.
let internal blocksOf (block: BodyPart list) (blocks: int) : BodyPart -> int =
    let counts = partCountIn block
    fun part -> blocks * counts part

/// One body of one block of a row's pattern, as the pricing reads it: a body
/// of whole blocks walks at one block's pace, and fights at its ranges.
let private oneBlockOf (pattern: BodyPattern) : Fighter =
    Fighter.ofCounts (blocksOf pattern.Block 1) (Engine.partHits * List.length pattern.Block)

let private guardBlock = oneBlockOf guardPattern
let private rangerBlock = oneBlockOf rangerPattern
let private defenderBlock = oneBlockOf defenderPattern

/// `oneBlockOf`, the fighting rows' read once.
let internal blockFighter (pattern: BodyPattern) : Fighter =
    if pattern.Name = guardPattern.Name then guardBlock
    elif pattern.Name = rangerPattern.Name then rangerBlock
    elif pattern.Name = defenderPattern.Name then defenderBlock
    else oneBlockOf pattern

/// One body of `blocks` whole blocks of a row's pattern.
let internal blocksFighter (pattern: BodyPattern) (blocks: int) : Fighter =
    Fighter.ofCounts (blocksOf pattern.Block blocks) (blocks * (blockFighter pattern).Hits)

/// Whether towers not ours fire in a room (`WorldRoom.Towers`: a
/// stronghold's, a rival's, never an ally's), every tile of it under them.
/// No body of ours wins or kites anything there (#576): the towers strike
/// the kite ground and the ring alike, up to 600 a tick each, refilled,
/// over raiders on ramparts, and the exchange prices none of it. Their
/// damage priced at the falloff still called a stronghold's L2 defender
/// won by two rangers, who died walking in (spar).
let private underRivalTowers (view: ColonyView) (room: string) =
    Map.tryFind room view.World.Rooms
    |> Option.exists (fun seen -> not (List.isEmpty seen.Towers))

/// The tiles of our loaded towers in a room (`WorldRoom.OurTowers`).
let private ourTowersIn (view: ColonyView) (room: string) : Pos list =
    Map.tryFind room view.World.Rooms
    |> Option.map (fun seen -> seen.OurTowers)
    |> Option.defaultValue []

/// What our loaded towers in a room (`WorldRoom.OurTowers`) land a tick on
/// the armed raid there, each at its range from the raider they land least
/// on, and at the falloff on one outside the room. Every fight priced in a
/// room of our towers counts it beside our bodies (#590): a ranger idled
/// beside a healer pair its 60 and the towers' 300 out-damaged together.
let private ourTowersOn (view: ColonyView) (room: string) (raid: HostileInfo list) : int =
    let towers = ourTowersIn view room

    let on (h: HostileInfo) =
        towers
        |> List.sumBy (fun tower ->
            RoomPos.range (RoomPos.at room tower) h.Pos
            |> Option.defaultValue Engine.towerFalloffRange
            |> Engine.towerAttackAt)

    match towers, raid |> List.filter (HostileInfo.armed view.Hostiles) with
    | [], _ -> 0
    | _, [] -> List.length towers * Engine.towerAttackAt Engine.towerFalloffRange
    | _, armed -> armed |> List.map on |> List.min

/// Which band of the kill order a target falls in (#451), first band first.
type KillTier =
    /// A rival's CLAIM body in a resident room, anywhere in it:
    /// `claimReactor` has no cooldown and a controller attack lands the tick
    /// it stands beside, so the whole approach is the only window.
    | Claimer
    /// A raid's healer whose heal our damage reaching it outpaces.
    | BrokenHealer
    /// The rest of the raid: a threat (`threatens`), or a healer we do not
    /// break.
    | Raid
    /// Anything else the Guard may shoot, a body that strikes nothing back:
    /// ordered by its kill score (`killScoresIn`, #621), a CLAIM body's
    /// reservation, a digger's dig and a hauler's load priced in it.
    | Prey

/// One target's place in the kill order, compared field by field.
[<Struct>]
type KillRank =
    {
        Tier: KillTier
        /// A body of the `Prey` band's kill score, negated so the highest
        /// leads (#621); 0 in every other band.
        Worth: float
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
        Threatens: bool
        HealOn: int
        DamageOn: int
    }

/// One target's band in the kill order (`killRank`).
let private killTier (target: KillFacts) =
    if target.ClaimsAFlag then
        Claimer
    elif target.RaidHealer && target.DamageOn > target.HealOn then
        BrokenHealer
    elif target.Threatens || target.RaidHealer then
        Raid
    else
        Prey

/// The heal that reaches a hostile in `room` a tick, its own included, from
/// where each healer stands and steps to within `horizon` ticks (`healOnto`,
/// #651; 0 for this tick's). A closure over the room's healers, found once.
let internal healReachingWithin
    (view: ColonyView)
    (room: string)
    (horizon: int)
    : HostileInfo -> int =
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
        |> healersAmong view.Hostiles (fun h ->
            Seam.onRing (RoomPos.pos h.Pos) && HostileInfo.crossesExits h)

    let remembered =
        view.RecalledHealers
        |> List.filter (fun h -> h.Pos.Room = room)
        |> healersAmong view.RecalledHealers (fun _ -> true)

    let healers = present @ remembered
    let side = view.Hostiles

    fun hostile -> healFrom side horizon healers hostile

/// The heal that reaches a hostile in `room` this tick (`healReachingWithin`
/// no tick on): a heal reaches one tile, a ranged heal three.
let internal healReaching (view: ColonyView) (room: string) : HostileInfo -> int =
    healReachingWithin view room 0

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
        ColonyView.bodies view
        |> List.choose (fun creep ->
            SpatialInfo.creepPlacementOf view.Spatial creep.Name
            |> Option.map (fun at -> at, partCount creep.Body))

    fun hostile ->
        fighters
        |> List.sumBy (fun (at, parts) ->
            RoomPos.range at hostile.Pos
            |> Option.map (damageAt parts)
            |> Option.defaultValue 0)

/// What a hostile carries, every resource counted one for one
/// (`WorldRoom.HostileCargo`, #621): a kill drops it in its tombstone.
let internal cargoOf (view: ColonyView) (hostile: HostileInfo) : int =
    Map.tryFind hostile.Pos.Room view.World.Rooms
    |> Option.bind (fun seen -> Map.tryFind hostile.Id seen.HostileCargo)
    |> Option.defaultValue 0

/// The tiles a rival's body walks from its spawn to the room it works: the
/// spawn stands in a room we do not see, a room's side off at least (#621).
let private replacementWalk = Engine.roomSide

/// The energy a tick a hostile's work takes now (#621), off its parts in
/// effect: a digger's dig while it stands on a seat (one with no free seat
/// takes nothing), at most its source's output; a carrier's load once a
/// round trip of its walk (`walk` ticks each way); a CLAIM body's
/// reservation, every source of its room held — against ours where we mine
/// it, else the output it doubles from a source nobody holds.
let private harmRate (view: ColonyView) (hostile: HostileInfo) (walk: int) : float =
    let power = HostileInfo.effectivePower view.Hostiles hostile
    let room = hostile.Pos.Room

    let digs =
        if onDigSeat view hostile then
            min (power Work * Engine.harvestPerWork) Engine.heldOutputPerTick
        else
            0

    let reserves =
        if power BodyPart.Claim > 0 then
            sourcesIn view room
            * (if minesRoom view room then
                   Engine.heldOutputPerTick
               else
                   Engine.heldOutputPerTick - Engine.neutralOutputPerTick)
        else
            0

    float (digs + reserves)
    + float (power Carry * Engine.carryPartCapacity) / float (2 * max 1 walk)

/// A hostile's gait as the engine walks it loaded (#621, `Gait.walked`): its
/// gait (`HostileInfo.gait`, CARRY read empty) and a live CARRY's weight for
/// each 50 it carries (`cargoOf`). The score's alone: every other reader keeps
/// the empty reading, the safe side for a threat's pace.
let private loadedGait (view: ColonyView) (hostile: HostileInfo) : Gait =
    HostileInfo.loadedGait view.Hostiles (cargoOf view hostile) hostile

/// What killing a hostile is worth, in energy (#621, by its remaining life,
/// #663): the share of its body (every part, destroyed ones too, and each
/// boost a lab spent on it) its life has left to spend — its ticks to live
/// over a lifetime, a CLAIM body's shorter one —, what it carries, whole,
/// and its work lost until its owner replaces it — its spawn time and its
/// walk back (`replacementWalk` at its loaded pace) at its rate (`harmRate`),
/// never past its life, where its work was ending anyway.
let internal killValue (view: ColonyView) (hostile: HostileInfo) : float =
    let boosted =
        hostile.Boosts |> List.filter (fun boost -> boost <> Unboosted) |> List.length

    let pace = Gait.ticks Gait.plain (loadedGait view hostile) |> Option.defaultValue 1

    let walk = replacementWalk * pace
    let replaced = Engine.spawnTicksPerPart * List.length hostile.Body + walk

    let lifetime =
        if List.contains BodyPart.Claim hostile.Body then
            Engine.claimLifetime
        else
            Engine.creepLifetime

    // A spawning body reads no life (the engine leaves it undefined, which
    // compares false either way): it has its whole lifetime ahead of it. A
    // life read at none or under is spent.
    let life =
        if hostile.TicksToLive >= 1 then
            min lifetime hostile.TicksToLive
        elif hostile.TicksToLive <= 0 then
            0
        else
            lifetime

    float (
        bodyCost hostile.Body
        + boosted * (Engine.boostEnergyPerPart + Engine.boostMineralPerPart)
    )
    * float life
    / float lifetime
    + float (cargoOf view hostile)
    + harmRate view hostile walk * float (min life replaced)

/// The rooms an observer in `from` looks across to `room`, Chebyshev over
/// the room grid, while within `Engine.observerRange`; None past it. The one
/// observer-range rule: every reader of an observer's reach reads it.
let internal observerReach (from: string) (room: string) : int option =
    RoomName.offsetOf from room
    |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))
    |> Option.filter (fun distance -> distance <= Engine.observerRange)

/// Whether an observer of ours reaches a room (`observerReach`).
let internal observerReaches (view: ColonyView) (room: string) : bool =
    view.World.Observers
    |> List.exists (fun observer -> Option.isSome (observerReach observer room))

/// The odds a body still stands where the view lays it (#663): certain in
/// sight, or where the world keeps no track of it (a fixture's); out of
/// sight, the world's one still-there rule (`Quarry.odds`) over its track,
/// rooted asked where it lies (`rootedAtWork`). Read for its value to a
/// fight already standing, at those odds. Read for a cast
/// (`Quarry.Reading.Cast`), a body out of sight is never certain: nothing
/// where an observer of ours reaches its room (`observerReaches`: the look
/// that confirms it comes in a tick or a few of its take, where a cast spent
/// on a body gone is the whole body), else its odds with no trip kept (one
/// that stepped out may have left for good), a body rooted at its work still
/// certain, no look being able to tell.
let internal odds (view: ColonyView) (reading: Quarry.Reading) (hostile: HostileInfo) : float =
    match Map.tryFind hostile.Id view.World.Quarries with
    | Some track when track.Seen < view.Time ->
        let rooted = rootedAtWork view hostile

        match reading with
        | Quarry.Reading.Value -> Quarry.odds view.Hostiles rooted view.Time track
        | Quarry.Reading.Cast when observerReaches view hostile.Pos.Room -> 0.0
        | Quarry.Reading.Cast ->
            Quarry.odds view.Hostiles rooted view.Time { track with Trail = [] }
    | _ -> 1.0

/// What one body of ours' blows on the hostiles of a room are priced by this
/// tick (#651, #621), by id: each one's kill value a hit (`killValue` over
/// its effective hits) beside the damage our other bodies deal it
/// (`damageReaching`, less this body's own); and each healer there's heal on
/// each (`healOnto`, this tick's, the healers `healReaching` reads), in the
/// damage that undoes it through the target's TOUGH.
let internal blowTerms
    (view: ColonyView)
    (room: string)
    (striker: CreepInfo)
    : Map<string, float * int> * Map<string, int> list =
    let damageOn = damageReaching view
    let here = SpatialInfo.creepPlacementOf view.Spatial striker.Name
    let parts = partCount striker.Body
    let targets = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    let healers =
        (targets
         |> healersAmong view.Hostiles (fun h ->
             Seam.onRing (RoomPos.pos h.Pos) && HostileInfo.crossesExits h))
        @ (view.RecalledHealers
           |> List.filter (fun h -> h.Pos.Room = room)
           |> healersAmong view.RecalledHealers (fun _ -> true))

    let terms =
        targets
        |> List.map (fun hostile ->
            let own =
                here
                |> Option.bind (fun at -> RoomPos.range at hostile.Pos)
                |> Option.map (damageAt parts)
                |> Option.defaultValue 0

            hostile.Id,
            (killValue view hostile / float (max 1 (HostileInfo.effectiveHits hostile)),
             max 0 (damageOn hostile - own)))
        |> Fresh.mapOfList

    let heals =
        healers
        |> List.map (fun healer ->
            targets
            |> List.choose (fun target ->
                match healOnto view.Hostiles 0 healer target with
                | 0 -> None
                | heal -> Some(target.Id, HostileInfo.damageFor target heal))
            |> Fresh.mapOfList)

    terms, heals

/// What blows landing together this tick are worth by their terms
/// (`blowTerms`, #651): the value a hit of what each leaves standing on its
/// target beside our other bodies' damage, once each healer has spent its
/// heal on the one target where it undoes the most (a heal acts on one
/// target a tick; the raid's best answer, our worst case), less what our
/// other bodies leave standing without them. A blow a heal undoes is worth
/// nothing; blows spread past what the healers can cover stand. A target the
/// terms do not price (one come since they were laid) is priced at their
/// mean value a hit, with no partner and no heal on it; with no terms at
/// all, a blow at its damage.
let blowsWorth
    (terms: Map<string, float * int>)
    (heals: Map<string, int> list)
    (blows: (string * int) list)
    =
    let unpriced =
        if Map.isEmpty terms then
            1.0
        else
            (terms |> Map.fold (fun total _ (perHit, _) -> total + perHit) 0.0)
            / float (Map.count terms)

    let termsOf id =
        Map.tryFind id terms |> Option.defaultValue (unpriced, 0)

    let healOf (heal: Map<string, int>) id =
        Map.tryFind id heal |> Option.defaultValue 0

    // The healers, the most heal on these targets first.
    let healers =
        heals
        |> List.sortByDescending (fun heal -> blows |> List.sumBy (fun (id, _) -> healOf heal id))

    let standing (ours: bool) =
        let left =
            blows
            |> List.map (fun (id, damage) -> id, snd (termsOf id) + (if ours then damage else 0))
            |> Map.ofList

        let worth id hits = fst (termsOf id) * float hits

        healers
        |> List.fold
            (fun (left: Map<string, int>) heal ->
                match
                    left
                    |> Map.toList
                    |> List.map (fun (id, hits) -> id, min hits (healOf heal id))
                    |> List.filter (fun (_, undone) -> undone > 0)
                with
                | [] -> left
                | undone ->
                    let id, hits = undone |> List.maxBy (fun (id, hits) -> worth id hits)
                    Map.add id (left.[id] - hits) left)
            left
        |> Map.fold (fun total id hits -> total + worth id hits) 0.0

    standing true - standing false

/// Whether a hostile dances on its room's border, by its trajectory (#658):
/// within the depth our reach and its step out cover, it crossed the border
/// lately (`ColonyView.Bounces`: out and back, either way, a few steps over
/// or on the exit tile alike), or it mills in place (`Quarry.inPlace`), or
/// it is placed back on the tile it crossed out from (`Quarry.placed`). A
/// body that cannot walk off dances on nothing.
let internal dancingOn (view: ColonyView) (hostile: HostileInfo) : bool =
    HostileInfo.crossesExits hostile
    && not (HostileInfo.stationary view.Hostiles hostile)
    && not (rootedAtWork view hostile)
    && Dance.byBorder hostile.Pos
    && (Map.containsKey hostile.Id view.Bounces
        || Quarry.inPlace
            hostile.Pos
            (Map.tryFind hostile.Id view.Stepped)
            (Map.tryFind hostile.Id view.SteppedBack))

/// The tiles of its room a dancer steps in and out on (#658,
/// `Crossing.Tiles`), each as often as it used it, and its tile now where it
/// stands in from elsewhere.
let private dancerEntries (view: ColonyView) (hostile: HostileInfo) : RoomPos list =
    let room = hostile.Pos.Room
    let crossed = Map.tryFind hostile.Id view.Bounces

    let here =
        if crossed |> Option.forall (fun c -> c.Came.Room <> room) then
            [ hostile.Pos ]
        else
            []

    (crossed |> Option.map _.Tiles |> Option.defaultValue []) @ here
    |> List.filter (fun tile -> tile.Room = room)

/// Whether a hostile is a dancer a camp is laid for (#663 M4): unarmed,
/// dancing on its border (`dancingOn`), in a room a camp is laid in (the
/// [[sweep]]'s, `harassRooms`). Any other dancer is met where it stands.
let private campable (view: ColonyView) (hostile: HostileInfo) : bool =
    view.Harass |> List.exists (fun h -> h.RoomName = hostile.Pos.Room)
    && not (HostileInfo.armed view.Hostiles hostile)
    && dancingOn view hostile

/// Whether a body of ours may stand on a tile of a room: ground, no wall,
/// no obstacle built on it, off the border and terrain this view projects
/// or the world holds (`exitSteps`' reading); none where neither holds the
/// room.
let private standable (view: ColonyView) (room: string) : Pos -> bool =
    let layer =
        match Map.tryFind room view.Spatial.Borders with
        | Some border when not border.IsEmpty -> Some(SpatialInfo.layerOf view.Spatial room)
        | _ -> Map.tryFind room view.World.Facts |> Option.map _.Layer

    match layer with
    | None -> fun _ -> false
    | Some layer ->
        fun tile ->
            inGrid tile
            && not (layer.Obstacles.Contains tile)
            && (match TerrainGrid.tryFind tile layer.Terrain with
                | Some Plain
                | Some Swamp -> true
                | Some Wall
                | None -> false)

/// The ground a body of `reach` camps a dancer from (#658, #663 M4), each
/// tile with the share of the dancer's steps in it covers: its entries
/// (`dancerEntries`, by use; its tile now where none is known) within the
/// reach of the tile. A tile off the border ring (`Dance.campDepth`), one a
/// body may stand on (`standable`), and out of every armed body's reach and
/// the step it closes. One ground for the camp's price and the camp laid
/// (`Threat`'s camp job): a swing covers only the entries a step in from
/// the exit, and a dancer whose way in has no such ground is no camp.
let private campGroundOf (view: ColonyView, reach: int, hostile: HostileInfo) : (Pos * float) list =
    let room = hostile.Pos.Room

    let entries =
        match dancerEntries view hostile with
        | [] -> [ hostile.Pos ]
        | entries -> entries
        |> List.map RoomPos.pos
        |> List.countBy id

    let total = entries |> List.sumBy snd
    let stands = standable view room

    let struck (tile: Pos) =
        view.Hostiles
        |> List.exists (fun h ->
            h.Pos.Room = room
            && HostileInfo.armed view.Hostiles h
            && (HostileInfo.weaponRange view.Hostiles h
                |> Option.exists (fun weapon -> range tile (RoomPos.pos h.Pos) <= weapon + 1)))

    entries
    |> List.collect (fun (entry, _) ->
        [
            for x in entry.X - reach .. entry.X + reach do
                for y in entry.Y - reach .. entry.Y + reach do
                    yield { X = x; Y = y }
        ])
    |> List.distinct
    |> List.filter (fun tile ->
        Dance.toEdge tile >= Dance.campDepth && stands tile && not (struck tile))
    |> List.map (fun tile ->
        tile,
        float (
            entries
            |> List.sumBy (fun (entry, uses) -> if range tile entry <= reach then uses else 0)
        )
        / float total)

let private campGroundMemo: TickMemo.Memo<ColonyView * int * HostileInfo, (Pos * float) list> =
    TickMemo.create 32 (fun (view, reach, hostile) (view', reach', hostile') ->
        reach = reach'
        && hostile.Id = hostile'.Id
        && hostile.Pos = hostile'.Pos
        && view.Time = view'.Time
        && obj.ReferenceEquals(view.Bounces, view'.Bounces)
        && obj.ReferenceEquals(view.Hostiles, view'.Hostiles)
        && obj.ReferenceEquals(view.Spatial, view'.Spatial)
        && obj.ReferenceEquals(view.World.Facts, view'.World.Facts))

/// `campGroundOf`, once a tick per view, reach and dancer.
let campGround (view: ColonyView) (reach: int) (hostile: HostileInfo) : (Pos * float) list =
    TickMemo.recall campGroundMemo (view, reach, hostile) campGroundOf

/// The share of a dancer's steps in a body of ours camped on its best tile
/// of the camp's ground strikes it on (#663 M4, `campGround`); none where
/// no ground covers any.
let private campCover (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : float =
    campGround view fighter.Reach hostile
    |> List.fold (fun best (_, share) -> max best share) 0.0

/// Whether a body of ours camps a hostile rather than meets it at their
/// standoff (#663 M4): a dancer a camp is laid for (`campable`), some of
/// whose steps in the body's reach covers from the camp's ground
/// (`campCover`). It walks into the body, which strikes it at its own range.
let private camped (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : bool =
    fighter.Reach > 0
    && campable view hostile
    && campCover view fighter hostile > 0.0

/// The range a body of ours strikes a hostile at (#663 M4): its own best
/// range where it camps it (`camped`), else their standoff (`standoff`).
let private strikeRange (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : int =
    if camped view fighter hostile then
        Fighter.bestRange fighter
    else
        standoff view fighter hostile

/// The ticks our side takes to kill a hostile once it stands at their
/// standoff, or in the camp (`strikeRange`), every body of `team` at its own: its hits through
/// its TOUGH against the side's damage there together — force concentrated
/// for the kill, which one body alone a healer out-heals may never make —
/// net of the heal reaching it (`healOn`, its partners' and its own), after
/// the rampart it stands on, which no heal puts back (`rampartOver`, #651).
/// None where the damage never out-heals it.
let private killTicksOf
    (view: ColonyView)
    (healOn: HostileInfo -> int)
    (team: Fighter list)
    (hostile: HostileInfo)
    : int option =
    let damage =
        team
        |> List.sumBy (fun fighter -> fighter.DamageAt(strikeRange view fighter hostile))

    let net = damage - HostileInfo.damageFor hostile (healOn hostile)

    if net <= 0 then
        None
    else
        let shield = rampartOver view hostile

        Some(
            (shield + damage - 1) / damage
            + max 1 ((HostileInfo.effectiveHits hostile + net - 1) / net)
        )

/// The ticks a hostile a body of ours keeps off (`keepsOff`) presses it,
/// the run-off its walk holds wherever it stands (#615): one as quick or
/// quicker until the body's shots lame or disarm it (`pressesFor`); one that
/// falls behind on plain, which closes on it only where its ground runs out,
/// until the body alone kills it (`killTicksOf`). `Int32.MaxValue` where its
/// damage never out-heals it.
let internal pressedFor (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : int =
    if Gait.fallsBehind fighter.Gait (HostileInfo.gait view.Hostiles hostile) then
        let healOn = healReachingWithin view hostile.Pos.Room view.Tuning.ReachHorizon

        killTicksOf view healOn [ fighter ] hostile
    else
        pressesFor view fighter hostile
    |> Option.defaultValue System.Int32.MaxValue

/// The share of the ticks a dancer was watched that it stood in the room the
/// view lays it in (`Quarry.inShare`, #658), off its crossings
/// (`ColonyView.Bounces`).
let private dancerInShare (view: ColonyView) (hostile: HostileInfo) : float =
    Quarry.inShare
        hostile.Pos.Room
        view.Time
        (RoomPos.pos hostile.Pos)
        (Map.tryFind hostile.Id view.Bounces)

/// The heal a dancer takes a tick while it stands out of its room (#658):
/// each healer of its owner across the border within a ranged heal's reach
/// of the tiles it crosses on (`Crossing.Tiles`), beside it its whole heal,
/// farther its ranged heal. A heal never crosses a border, so it lands only
/// while the dancer is out.
let private healAcross (view: ColonyView) (hostile: HostileInfo) : int =
    let room = hostile.Pos.Room

    let tiles =
        Map.tryFind hostile.Id view.Bounces
        |> Option.map (fun crossed -> crossed.Tiles |> List.filter (fun tile -> tile.Room <> room))
        |> Option.defaultValue []

    match tiles with
    | [] -> 0
    | tiles ->
        view.Hostiles
        |> List.filter (fun h ->
            h.Id <> hostile.Id
            && h.Owner = hostile.Owner
            && h.Pos.Room <> room
            && RoomName.neighbouring h.Pos.Room room
            && HostileInfo.heals view.Hostiles h)
        |> List.sumBy (fun healer ->
            let power = HostileInfo.effectivePower view.Hostiles healer Heal

            match
                tiles
                |> List.choose (fun tile -> RoomName.tileRange healer.Pos tile)
                |> List.sort
            with
            | near :: _ when near <= Engine.meleeRange -> power * Engine.healPower
            | near :: _ when near <= Engine.rangedRange -> power * Engine.rangedHealPower
            | _ -> 0)

/// The share of a dancer's watched time it stood in reach of a body of ours
/// (#658, #663 M4), and what the body deals it there. A dancer a camp is
/// laid for (`campable`): the share it stood in (`dancerInShare`) on the
/// steps in the body covers from the camp's ground (`campCover`), struck at
/// the body's best range. Any other dancer is struck only from off its
/// border's dance, by a shot: the share it stood in, at the ranged range —
/// a swing there is struck back, which no camp prices.
let private campShare (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : float * int =
    if campable view hostile then
        dancerInShare view hostile * campCover view fighter hostile,
        fighter.DamageAt(Fighter.bestRange fighter)
    else
        dancerInShare view hostile, fighter.DamageAt Engine.rangedRange

/// Whether the heal a dancer takes while out (`healAcross`) undoes what a
/// body of ours lands while it is in its reach (#658, `campShare`): it stays
/// on the border and our camp never wears it down.
let private outHealsCamp (view: ColonyView) (fighter: Fighter) (hostile: HostileInfo) : bool =
    let share, damage = campShare view fighter hostile

    float (healAcross view hostile) * (1.0 - share) >= float damage * share

/// The odds a dancer's kill lands from a camp on a tile covering its way in
/// (#658): the share of its watched time it stood in the body's reach
/// (`campShare`),
/// times the odds it keeps coming back over the camp's ticks, 1 − e^(−λ·T),
/// λ its returns a tick (`Quarry.returnRate`) and T twice the kill (it is in
/// reach about half its time); nothing where the heal it takes while out
/// (`healAcross`) undoes what lands while it is in. A body that never left
/// the room comes back for certain.
let private campOdds
    (view: ColonyView)
    (fighter: Fighter)
    (kill: int)
    (hostile: HostileInfo)
    : float =
    let share, _ = campShare view fighter hostile

    let returning =
        match Map.tryFind hostile.Id view.Bounces with
        | None -> 1.0
        | Some crossed -> 1.0 - exp (-(Quarry.returnRate view.Time crossed) * 2.0 * float kill)

    if outHealsCamp view fighter hostile then
        0.0
    else
        share * returning

/// Each tile's steps through a room's ground to its nearest walkable exit
/// tile (#659 S1), by tile index: the border's exit tiles that are no wall,
/// and every tile of ground a step at a time from them; -1 for a tile no
/// step reaches.
let private exitStepsIn (border: Map<Pos, Terrain>, terrain: TerrainGrid) : int[] =
    let steps = Array.create tileCount -1
    let queue = Array.zeroCreate tileCount
    let mutable head = 0
    let mutable tail = 0

    for KeyValue(tile, ground) in border do
        if Seam.isExit tile && ground <> Wall && inGrid tile then
            steps.[indexOf tile] <- 0
            queue.[tail] <- indexOf tile
            tail <- tail + 1

    while head < tail do
        let index = queue.[head]
        head <- head + 1

        for next in neighbours (posAt index) do
            if inGrid next && steps.[indexOf next] < 0 then
                match TerrainGrid.tryFind next terrain with
                | Some Plain
                | Some Swamp ->
                    steps.[indexOf next] <- steps.[index] + 1
                    queue.[tail] <- indexOf next
                    tail <- tail + 1
                | Some Wall
                | None -> ()

    steps

/// One room's exit steps as derived, beside the border and terrain read.
type private ExitStepsKept =
    {
        Border: Map<Pos, Terrain>
        Terrain: TerrainGrid
        Steps: int[]
    }

/// Each room's exit steps across ticks: the terrain never changes, and the
/// shell reads each room's once a heap (`World.terrainOf`), so one entry a
/// room holds until a reset. An entry is kept only for the very border and
/// terrain it was derived off (a fixture's room of the same name lays its
/// own); built walls and ramparts are no terrain and stay out of it. Swapped
/// whole, never mutated, so a reader on another thread sees an entry whole.
let mutable private exitStepsKept: Map<string, ExitStepsKept> = Fresh.mapOfList []

let private exitStepsOf (room: string) (border: Map<Pos, Terrain>) (terrain: TerrainGrid) : int[] =
    match Map.tryFind room exitStepsKept with
    | Some kept when
        obj.ReferenceEquals(kept.Border, border)
        && obj.ReferenceEquals(kept.Terrain, terrain)
        ->
        kept.Steps
    | _ ->
        let steps = exitStepsIn (border, terrain)

        exitStepsKept <-
            Map.add
                room
                {
                    Border = border
                    Terrain = terrain
                    Steps = steps
                }
                exitStepsKept

        steps

/// A tile's steps to the way out of its room a body walks (#659 S1): through
/// the room's ground to its nearest walkable exit (`exitStepsIn`), off the
/// border and terrain this view projects or the world holds, so a body
/// backed into a dead end or behind a walled edge is as far out as its walk.
/// The straight steps to the nearest edge (`Dance.toEdge`) where neither
/// holds the room's border, or no step of its ground reaches the tile.
let internal exitSteps (view: ColonyView) (tile: RoomPos) : int =
    let room = tile.Room
    let at = RoomPos.pos tile

    let ground =
        match Map.tryFind room view.Spatial.Borders with
        | Some border when not border.IsEmpty ->
            Some(border, (SpatialInfo.layerOf view.Spatial room).Terrain)
        | _ ->
            Map.tryFind room view.World.Facts
            |> Option.filter (fun facts -> not facts.Border.IsEmpty)
            |> Option.map (fun facts -> facts.Border, facts.Layer.Terrain)

    match ground with
    | Some(border, terrain) when inGrid at ->
        match (exitStepsOf room border terrain).[indexOf at] with
        | -1 -> Dance.toEdge at
        | steps -> steps
    | _ -> Dance.toEdge at

/// The odds a body of ours lands the whole of a kill (#621), as the share of
/// its `kill` ticks the hostile stays in reach. Certain where it cannot walk
/// off: penned in its room (an Invader), stationary or rooted at its work,
/// behind the body on plain (#613), or kept off. Else it walks for the
/// room's way out at its pace, its steps to the nearest walkable exit
/// through the room's ground (`exitSteps`, #659): one
/// quicker on plain stays in reach only until it opens the gap past the
/// body's reach, from where they stand or, coming in, from the reach; one at
/// the body's pace is caught at its work; walking elsewhere in the room, only
/// until its ticks to the edge, from when the body is in reach (#658); and
/// once it walks off (its last
/// step took it nearer that way out, `ColonyView.Stepped`, and the one
/// before not farther, `ColonyView.SteppedBack`, `Quarry.leaving`) only from
/// in reach, and then until it is off.
let private catchOdds
    (view: ColonyView)
    (fighter: Fighter)
    (from: RoomPos option)
    (arrival: int)
    (kill: int)
    (hostile: HostileInfo)
    : float =
    let side = view.Hostiles
    let theirs = loadedGait view hostile

    if
        not (HostileInfo.crossesExits hostile)
        || HostileInfo.stationary side hostile
        || rootedAtWork view hostile
        || Gait.fallsBehind fighter.Gait theirs
        || keepsOff view fighter hostile
    then
        1.0
    else
        let pace = Gait.ticks Gait.plain theirs |> Option.defaultValue 1
        let out = exitSteps view hostile.Pos
        let off = out * pace

        let left = Map.tryFind hostile.Id view.Stepped
        let back = Map.tryFind hostile.Id view.SteppedBack

        // A body stepping back and forth in place is caught at its work
        // (live t1,030,4xx, W19S25: two haulers on one four-step dance,
        // each read leaving off one step every other tick, turned the
        // ranger between them for 100+ ticks).
        let leaving = Quarry.leaving (exitSteps view) hostile.Pos left back
        let walking = Quarry.walking hostile.Pos left back

        let dancing = dancingOn view hostile

        let window =
            match Gait.ticks Gait.plain fighter.Gait with
            | _ when dancing -> kill
            | Some ours when ours > pace ->
                if leaving && arrival > 0 then
                    0
                else
                    let range =
                        from
                        |> Option.bind (fun tile -> RoomPos.range tile hostile.Pos)
                        |> Option.defaultValue fighter.Reach
                        |> min fighter.Reach

                    min off ((fighter.Reach - range + 1) * ours * pace / (ours - pace))
            // One walking, not at its work and not yet toward the edge (a
            // hauler on its way in), is caught only while it is still in the
            // room once the body is in reach: it turns for the edge before a
            // long kill lands (live t1,038,8xx, W19S26: two rangers trailed
            // a 23M23C hauler priced certain beside two seated miners).
            // One dancing on the border (`dancingOn`) is camped, at its
            // camp's odds (`campOdds`), and never chased across, whatever
            // its pace. One on or beside an exit walking out
            // leaves in a step: caught only for what lands before it, from
            // in reach.
            | _ when out <= 1 -> off + 1 - max 0 arrival
            | _ when not leaving && walking -> off - max 0 arrival
            | _ when not leaving -> kill
            | _ when arrival > 0 -> 0
            | _ -> off

        // A dancer's odds are its camp's, as a share and never rounded to a
        // tick: a one-tick kill at a third's odds is a third, not none.
        if dancing then
            min 1.0 (campOdds view fighter kill hostile)
        else
            min 1.0 (float (max 0 window) / float kill)

/// One room's kill scores (#621), the heal reaching each hostile found once:
/// what killing a hostile is worth (`killValue`) per tick of a body's effort,
/// its own `arrival` within reach and the kill it lands beside its partners
/// in the fight (`side`, every one at its standoff, `killTicksOf`), times the
/// odds it lands the kill (`catchOdds`). Zero where the side never kills it.
/// One score every kill order, ground and seat reads, so partners agree on
/// the head and focus it. Whether the side kills it at all is `killableIn`'s,
/// the heal it takes `healUndoingIn`'s.
let internal killScoresIn
    (view: ColonyView)
    (room: string)
    (side: Fighter list)
    : Fighter -> RoomPos option -> int -> HostileInfo -> float =
    // The heal over the kill, as every exchange reads it (#651).
    let healOn = healReachingWithin view room view.Tuning.ReachHorizon

    fun fighter from arrival hostile ->
        let team =
            fighter
            :: (side
                |> List.filter (fun partner ->
                    Option.isNone partner.Creep || partner.Creep <> fighter.Creep))

        match killTicksOf view healOn team hostile with
        | None -> 0.0
        | Some kill ->
            catchOdds view fighter from arrival kill hostile * killValue view hostile
            / float (max 0 arrival + kill)

/// Whether a side of ours kills a hostile of `room` at all (`killTicksOf`,
/// the heal over the kill as `killScoresIn` reads it): a nought kill score
/// of one it kills is its arrival's or its catch odds', of one it never
/// kills its heal's.
let internal killableIn
    (view: ColonyView)
    (room: string)
    (side: Fighter list)
    : HostileInfo -> bool =
    let healOn = healReachingWithin view room view.Tuning.ReachHorizon
    fun hostile -> Option.isSome (killTicksOf view healOn side hostile)

/// The heal reaching a hostile of `room` a tick (`healReaching`), in the
/// damage that undoes it through its TOUGH: what of our blows on it does not
/// stick.
let internal healUndoingIn (view: ColonyView) (room: string) : HostileInfo -> int =
    let healOn = healReaching view room
    fun hostile -> HostileInfo.damageFor hostile (healOn hostile)

/// ADR-0042
/// What one source of a room held this way is worth per tick. Owned **or**
/// reserved, never reserved alone: the colony's own room is owned while
/// nothing reserves it, so "reserved, or half" would price the two home
/// sources at five each.
let internal heldRateOf (control: RoomControlInfo) =
    if
        control.Owner = Ownership.Ours
        || RoomControlInfo.heldBy ReservationHolder.Ours control |> Option.isSome
    then
        Engine.heldOutputPerTick
    else
        Engine.neutralOutputPerTick

/// What one tick of a colony's spawn is worth (#652): the energy its sources
/// yield a tick, each at the rate its room is held at (`heldRateOf`), shared
/// over its spawns. What a cast's spawn time costs the economy the spawn
/// would otherwise be casting for.
let internal spawnTickWorth (view: ColonyView) : float =
    let income =
        view.Sources
        |> List.sumBy (fun source ->
            SpatialInfo.roomOf view.Spatial source.Id
            |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
            |> Option.map heldRateOf
            |> Option.defaultValue 0)

    float income / float (max 1 (List.length view.Spawns))

/// The spawn ticks one body of a template's `blocks` takes (#654).
let internal castSpawnTicks (template: BodyTemplate) (blocks: int) : int =
    blocks
    * List.length (patternOfTemplate template).Block
    * Engine.spawnTicksPerPart

/// What one body of a template's `blocks` costs a colony (#652, #654): its
/// energy, and its spawn time at what a tick of the colony's spawn is worth
/// (`spawnTickWorth`). The one price of a cast body, the chooser's
/// (`fighterCast`) and the Caster's (`castBodiesPaid`) alike.
let internal castCost (view: ColonyView) (template: BodyTemplate) (blocks: int) : float =
    float (blocks * bodyCost (patternOfTemplate template).Block)
    + float (castSpawnTicks template blocks) * spawnTickWorth view

/// A colony's view a kill's cast is priced in (#652): the world's vision of
/// the room and the rooms beside it laid in, read as though no prey were
/// walking off — a cast lands long after one step, and an ask that flipped
/// on each step would cast and cancel.
let private killPricing (view: ColonyView) (room: string) : ColonyView =
    { ColonyView.lookingInto (room :: RoomName.adjacent room) view with
        Stepped = Map.empty
        SteppedBack = Map.empty
    }

/// The kills a team of ours lands in a room (#652, #654), landing in
/// `landing` ticks: each hostile's value in `values` times the odds its
/// first body catches it (`catchOdds`), killed by the team together at its
/// standoff (`killTicksOf`), nothing of one that dies of age first.
let private killsLanded
    (priced: ColonyView)
    (room: string)
    (team: Fighter list)
    (landing: int)
    (values: Map<string, float>)
    : float =
    match team with
    | [] -> 0.0
    | body :: _ ->
        let healOn = healReachingWithin priced room priced.Tuning.ReachHorizon

        priced.Hostiles
        |> List.filter (fun hostile -> hostile.Pos.Room = room)
        |> List.sumBy (fun hostile ->
            match Map.tryFind hostile.Id values with
            | Some value ->
                match killTicksOf priced healOn team hostile with
                | Some kill when hostile.TicksToLive > landing + kill ->
                    catchOdds priced body None landing kill hostile * value
                | _ -> 0.0
            | None -> 0.0)

/// Whether the holders of a held exit (#450) kill a trespasser in its room
/// and walk back to the exit before the raid that left by it is back
/// (#656). The raid's return: the soonest armed body seen across the border
/// reaches the exit tile in its tiles there at its plain pace; with none in
/// sight (the rooms across are mostly dark), the raid that left is expected
/// back no sooner than it has been gone (the hold's ticks since it left,
/// `ExitHold.Left`): a raid bouncing on the border
/// is back at once, one long gone is not — the stand-in until the exit watch
/// learns a raid's bounce. The holders reach a trespasser in their straight
/// tiles to their reach at the slowest one's pace, kill it together at their
/// standoff (`killTicksOf`) and walk back its tiles to the exit. With no
/// holder, nothing leaves the exit.
let preyBeforeReturn
    (view: ColonyView)
    (room: string)
    (hold: ExitHold)
    (holders: CreepInfo list)
    : bool =
    let exit = hold.Exit
    let exitTile = RoomPos.at room exit
    let beside = RoomName.adjacent room

    let paceOf gait =
        Gait.ticks Gait.plain gait |> Option.defaultValue System.Int32.MaxValue

    // The world's vision of the rooms across, which a colony's cut may not
    // project.
    let across = (ColonyView.lookingInto (room :: beside) view).Hostiles

    let seen =
        across
        |> List.filter (fun h -> List.contains h.Pos.Room beside && HostileInfo.armed across h)
        |> List.choose (fun h ->
            RoomName.tileRange h.Pos exitTile
            |> Option.map (fun tiles ->
                let pace = paceOf (HostileInfo.gait across h)

                if pace = System.Int32.MaxValue then
                    System.Int32.MaxValue
                else
                    tiles * pace))

    let due =
        match seen with
        | [] -> [ max 0 (view.Time - hold.Left) ]
        | seen -> seen

    match holders with
    | [] -> true
    | holders ->
        let deadline = List.min due
        let team = holders |> List.map Fighter.ofCreep
        let pace = team |> List.map (fun f -> paceOf f.Gait) |> List.max
        let reach = team |> List.map _.Reach |> List.max
        let healOn = healReachingWithin view room view.Tuning.ReachHorizon

        let from =
            holders
            |> List.choose (fun creep -> SpatialInfo.creepPlacementOf view.Spatial creep.Name)

        let tilesTo (target: RoomPos) =
            from
            |> List.choose (fun at -> RoomName.tileRange at target)
            |> function
                | [] -> None
                | tiles -> Some(List.min tiles)

        pace < System.Int32.MaxValue
        && view.Hostiles
           |> List.exists (fun h ->
               h.Pos.Room = room
               && trespasser view (Set.singleton room) h
               && (match
                       tilesTo h.Pos,
                       killTicksOf view healOn team h,
                       RoomName.tileRange h.Pos exitTile
                   with
                   | Some walk, Some kill, Some back ->
                       int64 (max 0 (walk - reach)) * int64 pace
                       + int64 kill
                       + int64 back * int64 pace
                       <= int64 deadline
                   | _ -> false))

/// What one body of a row cast for a trespass's kill returns on its cost
/// (#652): the kills it lands over what it costs. The kills: each prey's
/// kill value (`prey`, priced once by the colony asking, so every colony
/// weighing the cast values it alike) times the odds one body, landing in
/// `landing` ticks (its spawn time, its spawns' queue and its walk, as the
/// Caster bids it), catches it (`catchOdds`, read as though no prey were
/// walking off: a cast lands long after one step, and an ask that flipped on
/// each step would cast and cancel), killed by the body alone at its
/// standoff (`killTicksOf`), nothing of one that dies of age first. A body's
/// cost: its `blocks` blocks of the row and its spawn time at what a tick of
/// this colony's spawn is worth (`spawnTickWorth`). Priced with the world's
/// vision of the room laid in, as `castPrice` is. The one return the Caster
/// casts a kill by (`castBodiesPaid`) and the spawn cascade orders its seat
/// by (`Spawns.planSpawns`). None for a row no kill prices.
let internal castReturn
    (view: ColonyView)
    (row: CastRow)
    (blocks: int)
    (room: string)
    (landing: int)
    (prey: (string * float) list)
    : float option =
    match row with
    | CastRow.Fighter template ->
        let body = blocksFighter (patternOfTemplate template) blocks

        let landed =
            killsLanded (killPricing view room) room [ body ] landing (Map.ofList prey)

        Some(landed / castCost view template blocks)
    | CastRow.Defender
    | CastRow.Squad _ -> None

/// How many bodies of a row cast for a trespass's kill pay for themselves
/// (#652): the most `k` whose cost the kills they land cover, off one body's
/// return (`castReturn`). Any other row pays for every body.
let internal castBodiesPaid
    (view: ColonyView)
    (row: CastRow)
    (blocks: int)
    (room: string)
    (landing: int)
    (prey: (string * float) list)
    : int =
    match castReturn view row blocks room landing prey with
    | Some paid -> int paid
    | None -> System.Int32.MaxValue

/// The kill order's leading key for one room's targets (#451), smallest
/// first; the caller breaks ties. Outside a ranger's room (an outpost's
/// melee guard) the raid first, then every other body by its kill score
/// (`score`, #621), highest first. An equal score is broken toward the body
/// our damage already wears down most (its effective hits net of the heal and
/// our damage reaching it), so partners lock onto one.
let internal killRank
    (view: ColonyView)
    (room: string)
    (score: HostileInfo -> float)
    : HostileInfo -> KillRank =
    if not (Set.contains room (rangerRooms view)) then
        let healOn = lazy (healReaching view room)
        let damageOn = lazy (damageReaching view)

        fun hostile ->
            if threatens view hostile then
                {
                    Tier = Raid
                    Worth = 0.0
                    EffectiveHits = 0
                }
            else
                {
                    Tier = Prey
                    Worth = -(score hostile)
                    EffectiveHits =
                        HostileInfo.effectiveHits hostile
                        + HostileInfo.damageFor hostile (healOn.Value hostile)
                        - damageOn.Value hostile
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

            let tier =
                killTier
                    {
                        ClaimsAFlag = claimsAFlag view.Hostiles resident hostile
                        RaidHealer = raidHealer view hostile
                        Threatens = threatens view hostile
                        HealOn = heal
                        DamageOn = damage
                    }

            {
                Tier = tier
                Worth = if tier = Prey then -(score hostile) else 0.0
                EffectiveHits = HostileInfo.effectiveHits hostile + heal - damage
            }

/// A target's band of the kill order (`killRank`'s tier), first band
/// first: what a mass attack is weighed against a shot by (#563).
let internal killBandOf (rank: KillRank) : int =
    match rank.Tier with
    | Claimer -> 0
    | BrokenHealer -> 1
    | Raid -> 2
    | Prey -> 3

/// A raid's price at a window (`raidPrice`). Its armed bodies' hits come in
/// three phases: `Shield`, the ramparts they stand on, which no heal puts
/// back; `Tough`, the damage that breaks their boosted TOUGH still
/// standing, while a hit healed costs `ToughHealing` of it; then `Hits`, the
/// rest, struck and healed one for one at `Healing`.
type private RaidPrice =
    {
        Damage: int
        Healing: int
        ToughHealing: int
        /// The hits of the ramparts its armed bodies stand on (#651,
        /// `rampartOver`).
        Shield: int
        Tough: int
        Hits: int
        /// Whether it endangers a room of ours it stands in
        /// (`endangersOurs`, #429): a dismantler there deals our creeps
        /// nothing and razes the room all the same.
        Breaches: bool
        /// Each body of it with a WORK, beside what it dismantles a tick
        /// (`Engine.dismantlePower` a WORK at its boost, #651): what breaks
        /// our ramparts beside its damage, where it reaches them
        /// (`exchangeAgainst`).
        Dismantlers: (HostileInfo * int) list
        /// The hits, through their TOUGH, of its unarmed bodies razing a room
        /// of ours (`endangersOurs`): what our kill ends where no armed body
        /// stands (`fightTicks`).
        Razing: int
    }

/// A raid priced both ways its fighting healers may act (#651): `Weapons`,
/// each fighting with all it carries and healing only beside its weapon;
/// `Heals`, each healing where its heal reaches, the weapon its heal drops
/// dropped. One object where no healer fights. Every exchange settles both
/// and takes the outcome worse for us (`worseOf`); a clock of ours reads
/// `Heals`, a clock of the raid's `Weapons`.
type private RaidPrices =
    { Weapons: RaidPrice; Heals: RaidPrice }

/// A raid's price at a window: the damage it deals a tick, each raider with
/// what it fights with at its boosts (`HostileInfo.powerWithin`); the healing
/// it does, priced as a rate; and the hits of its armed bodies as they stand,
/// behind the ramparts they stand on. Healers are priced in the healing and
/// never in the hits. Its boosted TOUGH is charged once, in the damage that
/// breaks it (`HostileInfo.damageFor`), the healing meanwhile on the armed
/// body it costs most: a T3 TOUGH in front makes each hit healed on it cost
/// three of our damage and a third. Each armed body deals what reaches from
/// the range it fights ours at (`rangeOf`, `damageAt`, #615 U2): a swing's
/// range for all of it.
///
/// The healing is the engine's (#651): what its healers put on the armed
/// body they heal most (with none armed, the dismantler razing a room of
/// ours, never the healer's own self-heal; else any body), each
/// from where it stands and steps to within `horizon` ticks (`healOnto`:
/// our body's arrival, so every heal that reaches the raid before it lands
/// counts), one target a tick. A healer that also fights heals beside its
/// weapon alone where `heals` is false; where it is true it heals wherever
/// its heal reaches, a heal dropping its swing, a ranged heal its swing and
/// its shot, and what it drops to heal that body leaves the damage.
let private raidPricedAs
    (view: ColonyView)
    (window: int)
    (horizon: int)
    (heals: bool)
    (rangeOf: HostileInfo -> int)
    (raid: HostileInfo list)
    : RaidPrice =
    let side = view.Hostiles

    let armed =
        raid
        |> List.filter (fun h ->
            Option.isSome (HostileInfo.weaponRangeOf (HostileInfo.partsWithin window side h)))

    // A body's swing and its shot at the range it fights ours at.
    let weapons (h: HostileInfo) =
        let power = HostileInfo.powerWithin window side h
        let range = rangeOf h

        (if range <= Engine.meleeRange then
             Engine.attackPower * power Attack
         else
             0),
        (if range <= Engine.rangedRange then
             Engine.rangedAttackPower * power RangedAttack
         else
             0)

    // Each healer's heals, the ones it casts beside what it fights with.
    let healers =
        healersAmong side (fun _ -> false) raid
        |> List.map (fun healer ->
            let swing, shot = weapons healer.Mender

            { healer with
                CastsHeal = heals || swing = 0
                CastsRangedHeal = heals || swing + shot = 0
            })

    let healOn = healFrom side horizon healers

    // The bodies our kill must end: its armed ones, else those razing a room
    // of ours (a dismantler, not the healer keeping it up), else any.
    let breachers =
        raid |> List.filter (fun h -> ourRoom view h.Pos.Room && endangersOurs view h)

    let focus =
        match
            (if not (List.isEmpty armed) then armed
             elif not (List.isEmpty breachers) then breachers
             else raid)
        with
        | [] -> None
        | patients -> Some(List.maxBy healOn patients)

    // What the healers drop to heal the focus: a heal beside it its swing, a
    // ranged heal its swing and its shot.
    let dropped =
        match focus with
        | None -> 0
        | Some focus ->
            healers
            |> List.sumBy (fun healer ->
                let swing, shot = weapons healer.Mender

                if healOnto side horizon { healer with CastsRangedHeal = false } focus > 0 then
                    swing
                elif healOnto side horizon healer focus > 0 then
                    swing + shot
                else
                    0)

    let healing = focus |> Option.map healOn |> Option.defaultValue 0
    let tough = armed |> List.filter (fun h -> HostileInfo.toughHits h > 0)

    {
        Breaches = raid |> List.exists (fun h -> ourRoom view h.Pos.Room && endangersOurs view h)
        Damage = (armed |> List.sumBy (fun h -> weapons h ||> (+))) - dropped
        Healing = healing
        ToughHealing =
            match tough with
            | [] -> healing
            | _ -> tough |> List.map (fun h -> HostileInfo.damageFor h (healOn h)) |> List.max
        Shield = armed |> List.sumBy (rampartOver view)
        Tough = tough |> List.sumBy (fun h -> HostileInfo.damageFor h (HostileInfo.toughHits h))
        Hits = armed |> List.sumBy (fun h -> h.Hits - HostileInfo.toughHits h)
        Dismantlers =
            raid
            |> List.choose (fun h ->
                match HostileInfo.powerWithin window side h Work with
                | 0 -> None
                | work -> Some(h, work * Engine.dismantlePower))
        Razing =
            breachers
            |> List.filter (fun h -> not (List.contains h armed))
            |> List.sumBy HostileInfo.effectiveHits
    }

/// `raidPricedAs` both ways (`RaidPrices`), one where no healer of the raid
/// carries a weapon that fights at its range.
let private raidPrices
    (view: ColonyView)
    (window: int)
    (horizon: int)
    (rangeOf: HostileInfo -> int)
    (raid: HostileInfo list)
    : RaidPrices =
    let weapons = raidPricedAs view window horizon false rangeOf raid

    let fights (h: HostileInfo) =
        HostileInfo.heals view.Hostiles h
        && Option.isSome (
            HostileInfo.weaponRangeOf (HostileInfo.partsWithin window view.Hostiles h)
        )

    if raid |> List.exists fights then
        {
            Weapons = weapons
            Heals = raidPricedAs view window horizon true rangeOf raid
        }
    else
        { Weapons = weapons; Heals = weapons }

/// The outcome worse for us of a raid priced both ways (`RaidPrices`):
/// lost either way is lost, and a raid only kited one way is only kited.
let private settleBoth
    (worse: 'outcome -> 'outcome -> 'outcome)
    (prices: RaidPrices)
    (settleOne: RaidPrice -> 'outcome)
    =
    if obj.ReferenceEquals(prices.Weapons, prices.Heals) then
        settleOne prices.Weapons
    else
        worse (settleOne prices.Weapons) (settleOne prices.Heals)

/// Whether this damage outpaces a priced raid's heal, through its TOUGH.
let private outHeals (price: RaidPrice) (ourDamage: int) =
    ourDamage > price.Healing && (price.Tough = 0 || ourDamage > price.ToughHealing)

/// Whether our side — this damage, healing, ramparts' hits (`ourShield`,
/// #651) and hits — wins against a priced raid: two clocks compared,
/// cross-multiplied to stay in whole numbers where no rampart or TOUGH
/// stands. A raid that out-heals our damage is never killed, nor one whose
/// TOUGH it never breaks. Each clock breaks its ramparts first, at the whole
/// damage, no heal putting them back, ours at the raid's damage and what its
/// dismantlers reaching them take a tick (`broken`); past its TOUGH ours
/// runs in phases. A
/// raid that deals our creeps nothing is won, but where it breaches a room of
/// ours (`RaidPrice.Breaches`, #429) only by damage that outpaces its heal: a
/// dismantler left standing razes the room.
let private wonAgainst (price: RaidPrice) ourDamage ourHealing (ourShield, broken) ourHits =
    if price.Damage = 0 then
        not price.Breaches || outHeals price ourDamage
    elif
        ourDamage <= price.Healing
        || (price.Tough > 0 && ourDamage <= price.ToughHealing)
    then
        false
    elif price.Damage <= ourHealing then
        true
    elif price.Tough = 0 && price.Shield = 0 && ourShield = 0 then
        price.Hits * (price.Damage - ourHealing) < ourHits * (ourDamage - price.Healing)
    else
        let ours =
            float price.Shield / float ourDamage
            + (if price.Tough = 0 then
                   0.0
               else
                   float price.Tough / float (ourDamage - price.ToughHealing))
            + float price.Hits / float (ourDamage - price.Healing)

        ours < float ourShield / float (price.Damage + broken)
               + float ourHits / float (price.Damage - ourHealing)

/// `wonAgainst` for `bodies` bodies alike, each dealing `damage`, healing
/// itself `heal`, struck back `back` a tick by its own swing (`hitBack`) and
/// standing `hits`, beside `beside` damage, the raid focusing one at a time
/// (#589): body k fights on while the k − 1 before it are dead, worn
/// meanwhile by its own swing's hit back past its heal, so the raid's clock
/// is each body's in turn and ours slows as they fall, breaking the raid's
/// ramparts first (`RaidPrice.Shield`). One body is `wonAgainst` exactly.
let private focusedAgainst (price: RaidPrice) beside (bodies: int) damage heal back hits =
    let dealt alive = float (beside + alive * damage)
    let struck = price.Damage + back

    if price.Damage = 0 then
        not price.Breaches || outHeals price (beside + bodies * damage)
    elif struck <= heal then
        // The raid never kills one: ours wins where it out-damages the heal.
        dealt bodies > float price.Healing
        && (price.Tough = 0 || dealt bodies > float price.ToughHealing)
    else
        let wear = float (max 0 (back - heal))

        // How long the raid takes to kill the next body, `elapsed` in.
        let phase elapsed =
            max 0.0 (float hits - wear * elapsed) / float (struck - heal)

        let rec fight alive elapsed (shield: float) (tough: float) (left: float) =
            alive > 0
            && (let whole = phase elapsed
                let shieldRate = dealt alive
                let toughRate = dealt alive - float price.ToughHealing
                let hitRate = dealt alive - float price.Healing

                let shieldTime =
                    if shield <= 0.0 then 0.0
                    elif shieldRate <= 0.0 then infinity
                    else shield / shieldRate

                if shieldTime > whole then
                    fight (alive - 1) (elapsed + whole) (shield - whole * shieldRate) tough left
                else
                    let phase = whole - shieldTime

                    let toughTime =
                        if tough <= 0.0 then 0.0
                        elif toughRate <= 0.0 then infinity
                        else tough / toughRate

                    if toughTime > phase then
                        fight
                            (alive - 1)
                            (elapsed + whole)
                            0.0
                            (tough - phase * max 0.0 toughRate)
                            left
                    else
                        let rest = phase - toughTime

                        if hitRate > 0.0 && left / hitRate < rest then
                            true
                        else
                            fight
                                (alive - 1)
                                (elapsed + whole)
                                0.0
                                0.0
                                (left - rest * max 0.0 hitRate))

        fight bodies 0.0 (float price.Shield) (float price.Tough) (float price.Hits)

/// How a body of ours meets the raid it is priced against (#615 U2).
[<RequireQualifiedAccess>]
type Stance =
    /// It holds its post, which the raid must come to to hurt our work: both
    /// sides at a swing's range, every weapon in full (#531: the guard holds
    /// the post whatever it can reach).
    | Hold
    /// The faster side picks the range (`standoff`): a body kept outside its
    /// reach deals nothing, and a raid it stands off deals it nothing.
    | Standoff

/// Whether a body of ours on a rampart stays on it through the fight
/// (#651): it holds a post (`Stance.Hold`), or against every raider the
/// raider comes to the range (`standoffPick`), or the range ours picks is
/// already reached from the rampart; a body that kites walks off it.
let private staysOn
    (view: ColonyView)
    (stance: Stance)
    (raid: HostileInfo list)
    (fighter: Fighter)
    (tile: RoomPos)
    =
    stance = Stance.Hold
    || raid
       |> List.forall (fun h ->
           not (HostileInfo.raider view.Hostiles h)
           || (match standoffPick view fighter h with
               | _, Picker.Theirs -> true
               | range, Picker.Closes ->
                   tilesBetween tile h.Pos |> Option.exists (fun r -> r <= range)
               | _, Picker.Kites -> false))

/// Our fighters as they stand on our ramparts they stay on (`rampartUnder`,
/// `staysOn`): each strikes nothing back and is struck back by nothing
/// (`StrikesBack` none); and those ramparts, which the raid breaks before
/// any of them and our heal never puts back.
let private onRamparts
    (view: ColonyView)
    (stance: Stance)
    (raid: HostileInfo list)
    (fighters: Fighter list)
    : Fighter list * (RoomPos * int) list =
    fighters
    |> List.mapFold
        (fun held fighter ->
            match rampartUnder view fighter with
            | Some(tile, hits) when staysOn view stance raid fighter tile ->
                { fighter with StrikesBack = 0 }, (tile, hits) :: held
            | _ -> fighter, held)
        []

/// What the raid's dismantlers reaching our ramparts take off them a tick
/// (#651, `RaidPrice.Dismantlers`): each one whose walk to a tile beside one
/// of them, at its plain pace, lands within `horizon`.
let private brokenBy
    (view: ColonyView)
    (horizon: int)
    (price: RaidPrice)
    (ramparts: (RoomPos * int) list)
    =
    match ramparts with
    | [] -> 0
    | _ ->
        price.Dismantlers
        |> List.sumBy (fun (h, rate) ->
            let pace =
                Gait.ticks Gait.plain (HostileInfo.gait view.Hostiles h)
                |> Option.defaultValue System.Int32.MaxValue

            let reaches (tile: RoomPos) =
                tilesBetween tile h.Pos
                |> Option.exists (fun r ->
                    r <= Engine.meleeRange || int64 (r - 1) * int64 pace <= int64 horizon)

            if ramparts |> List.exists (fst >> reaches) then rate else 0)

/// Our ramparts' hits and what breaks them a tick beside the raid's damage
/// (`wonAgainst`'s shield).
let private shieldOf
    (view: ColonyView)
    (horizon: int)
    (price: RaidPrice)
    (ramparts: (RoomPos * int) list)
    =
    List.sumBy snd ramparts, brokenBy view horizon price ramparts

/// The ticks our body takes to arrive at the raid (#651), the heal it meets
/// priced from then: the slowest of `fighters`' walks, its straight tiles to
/// within its reach of the nearest body of the raid (a dismantler's or a
/// healer's as much as a raider's: where the fight is) at its plain pace,
/// where its creep is placed; else, a body not yet standing anywhere, the
/// window our answering body needs (`Reaction.strike`), the worst case. At
/// least the reach horizon. The exchange widens it to the fight's own length
/// (`exchangeAgainst`).
let private arrivalOf (view: ColonyView) (fighters: Fighter list) (raid: HostileInfo list) : int =
    fighters
    |> List.fold
        (fun most (fighter: Fighter) ->
            let walk =
                match fighter.Creep |> Option.bind (placementOf view) with
                | None -> Reaction.strike
                | Some at ->
                    match raid |> List.choose (fun h -> tilesBetween at h.Pos) with
                    | [] -> 0
                    | tiles ->
                        max 0 (List.min tiles - fighter.Reach)
                        * (Gait.ticks Gait.plain fighter.Gait
                           |> Option.defaultValue Reaction.strike)

            max most walk)
        view.Tuning.ReachHorizon

/// The range a body of ours fights a raid at (#535): the farthest any of its
/// raiders holds it at (`standoff`), so a swing kept off by one shooter
/// clears nothing, the ones that close dying to the rest of our side.
let private rangeOn (view: ColonyView) (raid: HostileInfo list) (fighter: Fighter) : int =
    raid
    |> List.fold
        (fun range h ->
            if HostileInfo.raider view.Hostiles h then
                max range (standoff view fighter h)
            else
                range)
        Engine.meleeRange

/// The range a hostile fights our side at: the nearest any of our fighters
/// stands to it (`standoff`), a swing's with none of them there.
let private rangeFrom (view: ColonyView) (fighters: Fighter list) (hostile: HostileInfo) : int =
    match fighters with
    | [] -> Engine.meleeRange
    | _ -> fighters |> List.map (fun fighter -> standoff view fighter hostile) |> List.min

/// For each of our fighters, aligned with them, the range it deals from
/// (`rangeOn`) and the range it heals at: the nearest any raider stands to
/// it, so a guard a shooter keeps off while a melee closes on it swings at
/// that melee and heals nothing. And `rangeFrom` for each armed body of the
/// raid, and each armed body beside its range to each fighter, aligned with
/// them: every pair's `standoff` read once.
let private rangesBetween
    (view: ColonyView)
    (fighters: Fighter list)
    (raid: HostileInfo list)
    : (int * int) list * (HostileInfo -> int) * (HostileInfo * int list) list =
    let rows =
        raid
        |> List.filter (HostileInfo.armed view.Hostiles)
        |> List.map (fun h ->
            h,
            HostileInfo.raider view.Hostiles h,
            fighters |> List.map (fun fighter -> standoff view fighter h))

    let ours =
        fighters
        |> List.mapi (fun i _ ->
            let raiders =
                rows
                |> List.choose (fun (_, raider, ranges) ->
                    if raider then Some(List.item i ranges) else None)

            match raiders with
            | [] -> Engine.meleeRange, Engine.meleeRange
            | _ -> max Engine.meleeRange (List.max raiders), List.min raiders)

    let theirs (hostile: HostileInfo) =
        match rows |> List.tryFind (fun (h, _, _) -> obj.ReferenceEquals(h, hostile)) with
        | Some(_, _, (_ :: _ as ranges)) -> List.min ranges
        | Some _ -> Engine.meleeRange
        | None -> rangeFrom view fighters hostile

    ours, theirs, rows |> List.map (fun (h, _, ranges) -> h, ranges)

/// The hit back between our fighters and a raid a tick (`hitBack`).
type private HitBack =
    {
        /// What each fighter's own swing takes back, aligned with them.
        Taken: int list
        /// What the raiders that swing at ours take back, all of them.
        Struck: int
        /// Those raiders' hits, through their TOUGH: the most hit back
        /// strikes before they fall.
        Swingers: int
        /// The hits, through their TOUGH, of the raiders that strike back a
        /// swing of ours (`Taken`): it is taken only while they stand
        /// (`takenWhile`, #651).
        Strikers: int
    }

/// The engine's hit back between our fighters and a raid (#624,
/// `_damage.js`): a swing on a creep carrying a live ATTACK strikes the
/// swinger back by that ATTACK at its boosts, whatever the target does that
/// tick, and a shot never is. A body of ours that swings (it deals from a
/// swing's range, `deals`, with a live ATTACK) is struck back by the
/// strongest swing among the raiders at a swing's range of it, and a raider
/// that swings by the weakest `StrikesBack` among our bodies at a swing's
/// range of it (`rows`, each armed raider's range to each fighter): who
/// swings at whom is either side's pick, so neither is priced on its best.
/// A raider no body of ours strikes back is no swinger. Each side's swing
/// lands twice a tick where both swing at range 1 (spar
/// `outpost-melee-healer`: the guard's 90 comes back off the smallMelee's
/// swing at it). Nothing is struck back either way where either side stands
/// on a rampart (#651): a body of ours on ours strikes back nothing
/// (`onRamparts`), and a raider on one is no swinger nor striker
/// (`rampartOver`): a swing at it lands on the rampart.
let private hitBack
    (view: ColonyView)
    (window: int)
    (fighters: Fighter list)
    (deals: int list)
    (rows: (HostileInfo * int list) list)
    : HitBack =
    let swings =
        rows
        |> List.filter (fun (h, _) -> rampartOver view h = 0)
        |> List.map (fun (h, ranges) ->
            h, Engine.attackPower * HostileInfo.powerWithin window view.Hostiles h Attack, ranges)
        |> List.filter (fun (_, swing, _) -> swing > 0)

    // Whether fighter `i` swings, and so is struck back.
    let swinging =
        List.zip fighters deals
        |> List.map (fun (fighter: Fighter, range) ->
            fighter.StrikesBack > 0 && range <= Engine.meleeRange)

    let taken =
        swinging
        |> List.mapi (fun i swings' ->
            if not swings' then
                0
            else
                swings
                |> List.fold
                    (fun most (_, swing, ranges) ->
                        if List.item i ranges <= Engine.meleeRange then
                            max most swing
                        else
                            most)
                    0)

    let strikers =
        swings
        |> List.filter (fun (_, _, ranges) ->
            List.zip swinging ranges
            |> List.exists (fun (swings', range) -> swings' && range <= Engine.meleeRange))
        |> List.sumBy (fun (h, _, _) -> HostileInfo.effectiveHits h)

    let struck =
        swings
        |> List.choose (fun (h, _, ranges) ->
            List.zip fighters ranges
            |> List.fold
                (fun least (fighter: Fighter, range) ->
                    if range <= Engine.meleeRange then
                        Some(
                            min
                                fighter.StrikesBack
                                (Option.defaultValue fighter.StrikesBack least)
                        )
                    else
                        least)
                None
            |> Option.filter (fun back -> back > 0)
            |> Option.map (fun back -> h, back))

    {
        Taken = taken
        Struck = List.sumBy snd struck
        Swingers = struck |> List.sumBy (fst >> HostileInfo.effectiveHits)
        Strikers = strikers
    }

/// What our swings' hit back (`HitBack.Taken`, `taken` a tick) adds to a
/// priced raid's damage while our side kills it at `damage` a tick (#651):
/// taken only while the raiders striking back stand (`strikers`, their
/// hits), so of our kill's ticks only the share they stand, the strikers
/// falling first as `struckBack` has them, at our damage net of the heal,
/// against the whole kill: the ramparts broken at the whole damage, the
/// rest at the net. The bound on the strongest of them leaves nothing once
/// they are gone. All of it where they are the whole raid, or `damage`
/// never out-heals it.
let private takenWhile (price: RaidPrice) (damage: int) (taken: int) (strikers: int) : int =
    let net = damage - price.Healing

    if taken = 0 || net <= 0 || strikers >= price.Tough + price.Hits then
        taken
    else
        // Strikers / net over Shield / damage + (Tough + Hits) / net.
        let kill =
            float price.Shield * float net / float damage + float (price.Tough + price.Hits)

        int (ceil (float taken * float strikers / kill))

/// What a raid's swings struck back (`HitBack.Struck`, `struck` a tick) add
/// to `damage` a tick against a priced raid: struck back only while its
/// swingers stand, the hit back ends once their hits (`swingers`) are spent,
/// so it adds only the rate that kills the raid as soon as `damage` and it
/// do, the swingers' hits spent at most. All of it where the swingers are
/// the whole raid; none where `damage` never out-heals what is left of it.
let private struckBack (price: RaidPrice) (damage: int) (struck: int) (swingers: int) : int =
    let hits = int64 (price.Shield + price.Tough + price.Hits)
    let net = int64 (damage - price.Healing)
    let all = net + int64 struck

    if struck = 0 || all <= 0L then
        0
    elif hits <= int64 swingers || int64 struck * hits <= int64 swingers * all then
        struck
    elif net <= 0L then
        0
    else
        // The swingers fall before the rest: the kill takes the rest's hits
        // at `net`, so the rate that kills the raid as soon is
        // `hits / ((hits - swingers) / net)`.
        int (hits * net / (hits - int64 swingers) - net)

/// `hitBack` at the standoff (`rangesBetween`), or holding a post
/// (`Stance.Hold`), where every pair stands at a swing's range.
let private hitBackUnder
    (view: ColonyView)
    (window: int)
    (stance: Stance)
    (fighters: Fighter list)
    (raid: HostileInfo list)
    : HitBack =
    match stance with
    | Stance.Hold ->
        let ones = List.map (fun _ -> Engine.meleeRange) fighters

        hitBack
            view
            window
            fighters
            ones
            (raid
             |> List.filter (HostileInfo.armed view.Hostiles)
             |> List.map (fun h -> h, ones))
    | Stance.Standoff ->
        let ours, _, rows = rangesBetween view fighters raid
        hitBack view window fighters (List.map fst ours) rows

/// What a fighter deals a raid a tick at the range it fights it at.
let private dealtOn (view: ColonyView) (raid: HostileInfo list) (fighter: Fighter) : int =
    fighter.DamageAt(rangeOn view raid fighter)

/// Whether damage ends a raid that strikes nothing back (#510): it outruns
/// the heal acting there priced on the raider it costs least
/// (`HostileInfo.damageFor`), the one a kite kills first.
let private kitesDown (price: RaidPrice) (raiders: HostileInfo list) (damage: int) =
    damage > (raiders |> List.map (fun h -> HostileInfo.damageFor h price.Healing) |> List.min)

/// How an exchange at the standoff comes out (#615 U2).
type internal Outcome =
    | Lost
    /// Its raid strikes nothing at the range (every raider kept off,
    /// `keepsOff`) and our damage, every body's together, outruns
    /// its heal (`kitesDown`).
    | Kites
    /// Its raid strikes back and our clock beats its.
    | Trades

/// Whether a set of bodies ends the raid, trading or kiting it down: what
/// every win, count and size reads. Only a row's own answer tells the two
/// apart (`rowAnswer`), a raid its row only kites being the verdict's Kite.
let internal won (outcome: Outcome) = outcome <> Lost

/// The worse for us of a raid's two outcomes (`settleBoth`), its weapons'
/// first: lost either way is lost, and else the weapons' reading says how it
/// is won — a raid that strikes nothing while it heals is not kited.
let private worseOf (weapons: Outcome) (heals: Outcome) : Outcome =
    match heals with
    | Lost -> Lost
    | _ -> weapons

/// Settles an exchange whose raid is priced at the ranges it fights at:
/// kited where the raiders a fighter of ours stands off (`kited`, none
/// holding the post) strike nothing there, else traded by `trade`.
let private settle
    (price: RaidPrice)
    (kited: HostileInfo list)
    (damage: int)
    (trade: RaidPrice -> bool)
    : Outcome =
    if price.Damage = 0 && not (List.isEmpty kited) then
        if kitesDown price kited damage then Kites else Lost
    elif trade price then
        Trades
    else
        Lost

/// The ticks an exchange lasts (#651), `wonAgainst`'s two clocks: the
/// sooner of our kill of the raid (its ramparts at our whole damage, its
/// TOUGH and hits net of its heal; with no armed body, the unarmed ones
/// razing a room of ours, `Razing`) and its kill of our side (our ramparts
/// at its damage and what breaks them, our hits net of our heal). None where
/// neither side ever ends the other.
let private fightTicks
    (price: RaidPrice)
    ourDamage
    ourHealing
    (ourShield, broken)
    ourHits
    : float option =
    let price =
        if price.Shield + price.Tough + price.Hits = 0 then
            { price with Hits = price.Razing }
        else
            price

    let ours =
        if
            ourDamage <= price.Healing
            || (price.Tough > 0 && ourDamage <= price.ToughHealing)
        then
            None
        else
            Some(
                float price.Shield / float ourDamage
                + (if price.Tough = 0 then
                       0.0
                   else
                       float price.Tough / float (ourDamage - price.ToughHealing))
                + float price.Hits / float (ourDamage - price.Healing)
            )

    let theirs =
        if price.Damage <= ourHealing then
            None
        else
            Some(
                float ourShield / float (price.Damage + broken)
                + float ourHits / float (price.Damage - ourHealing)
            )

    match ours, theirs with
    | Some a, Some b -> Some(min a b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

/// One exchange against a raid (#615 U2): what stands beside our bodies
/// (`beside` damage and `besideHits`, a home's towers) and the bodies
/// themselves, each at the range it fights at under `stance`, dealing what
/// reaches from there and healing itself as it fights there
/// (`Fighter.HealAt`), the raid dealing what reaches from its ranges, each
/// side's swings struck back (`hitBack`) while the bodies striking back stand
/// (`struckBack`, `takenWhile`), and each side's ramparts broken before the
/// bodies on them (`onRamparts`, `RaidPrice.Shield`, #651).
let private exchangeAgainst
    (view: ColonyView)
    (window: int)
    (horizon: int)
    (raid: HostileInfo list)
    (stance: Stance)
    (beside: int)
    (besideHits: int)
    (fighters: Fighter list)
    : Outcome =
    let fighters, ramparts = onRamparts view stance raid fighters

    let ours, theirs, kited, back =
        match stance, fighters with
        | Stance.Hold, _
        | _, [] ->
            List.map (fun _ -> Engine.meleeRange, Engine.meleeRange) fighters,
            (fun _ -> Engine.meleeRange),
            [],
            hitBackUnder view window Stance.Hold fighters raid
        | Stance.Standoff, _ ->
            let ours, theirs, rows = rangesBetween view fighters raid

            ours,
            theirs,
            raid |> List.filter (HostileInfo.raider view.Hostiles),
            hitBack view window fighters (List.map fst ours) rows

    let ranges = List.zip fighters ours

    let dealt =
        beside
        + List.sumBy (fun (fighter: Fighter, (range, _)) -> fighter.DamageAt range) ranges

    let healing =
        List.sumBy (fun (fighter: Fighter, (_, healing)) -> fighter.HealAt healing) ranges

    let hits =
        besideHits + List.sumBy (fun (fighter: Fighter, _) -> fighter.Hits) ranges

    // One reading of the raid at a horizon: its price with what our swings
    // take back, and our damage with what its swings take back.
    let terms horizon (price: RaidPrice) =
        let damage = dealt + struckBack price dealt back.Struck back.Swingers

        { price with
            Damage = price.Damage + takenWhile price damage (List.sum back.Taken) back.Strikers
        },
        damage,
        shieldOf view horizon price ramparts

    // The exchange at a horizon, and how long it lasts read both ways.
    let at horizon =
        let prices = raidPrices view window horizon theirs raid

        let outcome =
            settleBoth worseOf prices (fun price ->
                let priced, damage, shield = terms horizon price

                settle priced kited damage (fun priced ->
                    wonAgainst priced damage healing shield hits))

        let lasts =
            [ prices.Weapons; prices.Heals ]
            |> List.map (fun price ->
                let priced, damage, shield = terms horizon price
                fightTicks priced damage healing shield hits)
            |> List.fold
                (fun most ticks ->
                    match ticks with
                    | Some ticks -> max most (int (ceil ticks))
                    | None -> max most Reaction.lend)
                0

        outcome, lasts

    // Every heal and dismantle that reaches the fight before it ends counts
    // (#651): from our arrival, widened to the fight's own length, a few
    // rounds, the heal only growing with it; a fight neither side ends reads
    // two rooms' walk, everything in reach.
    let rec widen horizon rounds =
        let outcome, lasts = at horizon

        if lasts <= horizon || rounds = 0 then
            outcome
        else
            widen (min Reaction.lend lasts) (rounds - 1)

    widen horizon 3

/// `exchangeAgainst` the raid standing in one room at the standoff, read at
/// the window to close and strike (`Reaction.strike`): our fighters beside
/// our towers there (`ourTowersOn`); Lost under towers not ours
/// (`underRivalTowers`).
let private exchangeIn (view: ColonyView) (room: string) (fighters: Fighter list) : Outcome =
    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    if underRivalTowers view room then
        Lost
    else
        exchangeAgainst
            view
            Reaction.strike
            (arrivalOf view fighters raid)
            raid
            Stance.Standoff
            (ourTowersOn view room raid)
            0
            fighters

/// Whether our bodies, by part counts and hits, win `exchangeIn`.
let internal exchangeWon
    (view: ColonyView)
    (room: string)
    (bodies: ((BodyPart -> int) * int) list)
    =
    exchangeIn view room (bodies |> List.map (fun (parts, hits) -> Fighter.ofCounts parts hits))
    |> won

/// Whether these creeps of ours, together, win the raid standing in a room
/// (`exchangeWon`): what a muster waits for (#535), and one body alone is
/// what it weighs a body standing in the room by (`Quota.homeHolds`). Each
/// at its live gait (`Fighter.ofCreep`), as the kite walk reads it.
let bodiesWin (view: ColonyView) (room: string) (creeps: CreepInfo list) =
    exchangeIn view room (List.map Fighter.ofCreep creeps) |> won

/// A row's block against the raid in a room (#615 U2), priced once: the
/// raid at the ranges it fights one block's body at under `stance`
/// (`exchangeAgainst`) — a body of whole blocks walks at one block's pace and
/// so fights at one block's ranges —, the raiders it may kite (`settle`), our
/// towers' damage there (`ourTowersOn`), and what one block deals, heals and
/// stands at its range; and the hit back (`hitBack`): what a body of the row
/// takes back a tick whatever its blocks, and what one block strikes back.
type private BlockPrice =
    {
        Price: RaidPrices
        Kited: HostileInfo list
        Beside: int
        Damage: int
        Heal: int
        Hits: int
        Back: HitBack
    }

/// The hits a body not holding a fight brings to it (#647): what it keeps by
/// its next decision once in it, `soon` ticks struck by the raid's own weapons
/// (`struck` a tick, past its heal). The one price of a fresh body, the
/// Matcher's (`fits`) and the row's that sizes one body (`blocksOutcome`)
/// alike, so a row never casts a body the gate refuses alone. A home of ours
/// keeps its whole hits: it is fought for to the death, and the gate prices
/// no fit there.
let private freshHits (view: ColonyView) (room: string) (soon: int) (hits: int) (struck: int) =
    if (homesOfOurs view).Contains room then
        hits
    else
        max 1 (hits - soon * max 0 struck)

/// `BlockPrice` of a row's pattern in a room; None under towers not ours
/// (`underRivalTowers`).
let private blockPrice (pattern: BodyPattern) (stance: Stance) (view: ColonyView) (room: string) =
    if underRivalTowers view room then
        None
    else
        let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
        let one = blockFighter pattern

        let (range, healing), theirs, kited, back =
            match stance with
            | Stance.Hold ->
                (Engine.meleeRange, Engine.meleeRange),
                (fun _ -> Engine.meleeRange),
                [],
                hitBackUnder view Reaction.strike Stance.Hold [ one ] raid
            | Stance.Standoff ->
                let ours, theirs, rows = rangesBetween view [ one ] raid

                List.head ours,
                theirs,
                raid |> List.filter (HostileInfo.raider view.Hostiles),
                hitBack view Reaction.strike [ one ] (List.map fst ours) rows

        Some
            {
                // A body yet to be cast lands no sooner than the answering
                // window: every heal that reaches the raid by then counts.
                Price = raidPrices view Reaction.strike Reaction.strike theirs raid
                Kited = kited
                Beside = ourTowersOn view room raid
                Damage = one.DamageAt range
                Heal = one.HealAt healing
                Hits = one.Hits
                Back = back
            }

/// A body of `blocks` blocks of a priced row: what it deals beside our
/// towers, what its swings struck back add to that (`struckBack`), and the
/// raid's price with what its own swing takes back (`hitBack`) while its
/// strikers stand (`takenWhile`).
let private blocksDealt
    (block: BlockPrice)
    (raid: RaidPrice)
    (bodies: int)
    (blocks: int)
    : int * int * RaidPrice =
    let damage = block.Beside + bodies * blocks * block.Damage

    let struck = struckBack raid damage (blocks * block.Back.Struck) block.Back.Swingers

    let price =
        { raid with
            Damage =
                raid.Damage
                + takenWhile raid (damage + struck) (List.head block.Back.Taken) block.Back.Strikers
        }

    damage, struck, price

/// Whether `blocks` whole blocks of a fighting row, one body, win the raid in
/// a room under `stance` (`blockPrice`): what sizes a row and decides whether
/// a room is a fight or a withdrawal (`Observe.raidDeadlines`). Here and not
/// in `Quota` because the ranger's ground reads it too (`outmatched`). A
/// guard row sized for its post is priced holding it (`Stance.Hold`, #531),
/// the rest at the standoff. Partly applied to its room it prices the raid
/// once and every block count off it.
///
/// Worked example: a lone smallMelee needs one guard block; backed by a
/// smallHealer, its 40 damage kills our 1,000 hits in 25 ticks, before our 30
/// net damage kills its 1,000 hits, so that raid needs the second block.
let private blocksOutcome
    (pattern: BodyPattern)
    (stance: Stance)
    (view: ColonyView)
    (room: string)
    : int -> Outcome =
    match blockPrice pattern stance view room with
    | None -> fun _ -> Lost
    | Some block ->
        fun (blocks: int) ->
            settleBoth worseOf block.Price (fun raid ->
                let damage, struck, price = blocksDealt block raid 1 blocks
                let damage = damage + struck

                let hits =
                    freshHits
                        view
                        room
                        view.Tuning.ReachHorizon
                        (blocks * block.Hits)
                        (raid.Damage - blocks * block.Heal)

                settle price block.Kited damage (fun price ->
                    wonAgainst price damage (blocks * block.Heal) (0, 0) hits))

/// `blocksOutcome` that ends the raid (`won`): what sizes and counts a row.
let blocksBeat
    (pattern: BodyPattern)
    (stance: Stance)
    (view: ColonyView)
    (room: string)
    : int -> bool =
    blocksOutcome pattern stance view room >> won

/// How `bodies` bodies of `blocks` whole blocks each come out against the
/// raid in a room at the standoff beside our towers there (#589,
/// `blockPrice`), the raid killing them one at a time (`focusedAgainst`),
/// never summed into one body; their damage together kites it down.
let private bodiesOutcome
    (pattern: BodyPattern)
    (view: ColonyView)
    (room: string)
    (bodies: int)
    (blocks: int)
    : Outcome =
    match blockPrice pattern Stance.Standoff view room with
    | Some block when blocks > 0 ->
        // Every raider that swings strikes the body it focuses, which
        // strikes it back while any body stands; each body's own swing is
        // struck back whether focused or not (`focusedAgainst`).
        settleBoth worseOf block.Price (fun raid ->
            let damage, struck, _ = blocksDealt block raid bodies blocks
            let beside = block.Beside + struck

            settle raid block.Kited (damage + struck) (fun price ->
                focusedAgainst
                    price
                    beside
                    bodies
                    (blocks * block.Damage)
                    (blocks * block.Heal)
                    (takenWhile
                        price
                        (damage + struck)
                        (List.head block.Back.Taken)
                        block.Back.Strikers)
                    (blocks * block.Hits)))
    | _ -> Lost

/// The biggest guard body the row can cast (#417): at most `guardBlocksMost`
/// blocks, and as many as the bank's capacity buys (`Bodies.guardBodyWithin`).
/// What the stand-down weighs a raid against. One body and never the row's two
/// summed: `blocksBeat` grows about as the square of the blocks, and a
/// raid focuses one body at a time while the second may arrive late, so the
/// sum would call a fight won that each body loses alone — and a melee body
/// is sized to win alone (`fighterCast`).
let guardBlocksReach (view: ColonyView) : int =
    min guardBlocksMost (view.Bank.Capacity / bodyCost guardPattern.Block)

/// Whether the bank casts one block of a row's pattern (#546): a row whose
/// block it cannot buy is never asked for an answer, which falls to the next
/// row that can — a ranger's 700 on a 550 bank waits for ever, holding the
/// economy that would raise the bank.
let castable (view: ColonyView) (pattern: BodyPattern) =
    bodyCost pattern.Block <= view.Bank.Capacity

/// Whether one body kites the raid standing in a room down alone (#510): its
/// exchange there at the standoff (`exchangeIn`) is `Kites`, every raider
/// struck from outside its reach and the heal acting in the room outrun, our
/// towers there beside it.
let internal kitedDown (view: ColonyView) (fighter: Fighter) (room: string) : bool =
    // Every raider stood off first, the exchange's own reading, so a raid
    // that can close is never priced.
    let stoodOff (h: HostileInfo) =
        h.Pos.Room <> room
        || not (HostileInfo.raider view.Hostiles h)
        || keepsOff view fighter h

    Fighter.outranges fighter
    && List.forall stoodOff view.Hostiles
    && exchangeIn view room [ fighter ] = Kites

/// The ticks these fighters take, together and beside our towers there, to
/// kill the raid standing in a room once they stand at it (#596): its armed
/// bodies' hits, TOUGH first, against their damage net of the raid's heal
/// (`raidPrice`), its swings struck back by them (`hitBack`, none by a body
/// on our rampart), the ramparts its bodies stand on broken first (#651).
/// Our clock alone: what the raid deals them is the exchange's question, not
/// this one. `Some 0` where no armed body stands there; None where the
/// damage never out-heals the raid.
let private killTicks (view: ColonyView) (room: string) (fighters: Fighter list) : int option =
    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    let back =
        hitBackUnder
            view
            Reaction.strike
            Stance.Standoff
            (fst (onRamparts view Stance.Standoff raid fighters))
            raid

    let towersAndOurs =
        ourTowersOn view room raid + List.sumBy (dealtOn view raid) fighters

    // Our clock at a horizon: the raid healing all it reaches by then.
    let killAt horizon =
        let price =
            (raidPrices view Reaction.strike horizon (fun _ -> Engine.meleeRange) raid).Heals

        let dealt = towersAndOurs + struckBack price towersAndOurs back.Struck back.Swingers

        if price.Tough = 0 && price.Hits = 0 then
            Some 0
        elif not (outHeals price dealt) then
            None
        else
            let tough =
                if price.Tough = 0 then
                    0.0
                else
                    float price.Tough / float (dealt - price.ToughHealing)

            Some(
                int (
                    ceil (
                        float price.Shield / float dealt
                        + tough
                        + float price.Hits / float (dealt - price.Healing)
                    )
                )
            )

    // From our arrival, widened to the kill's own length (#651): every heal
    // that reaches the raid before it falls counts.
    let rec widen horizon rounds =
        match killAt horizon with
        | Some ticks when ticks > horizon && rounds > 0 && horizon < Reaction.lend ->
            widen (min Reaction.lend ticks) (rounds - 1)
        | kill -> kill

    widen (arrivalOf view fighters raid) 3

/// The raiders standing in a room.
let private raidersIn (view: ColonyView) (room: string) =
    view.Hostiles
    |> List.filter (fun h -> h.Pos.Room = room && HostileInfo.raider view.Hostiles h)

/// A body's walk to a raid, a tile a tick from where it stands — this view's
/// placement, else where the fleet stands it — to its weapon's range of the
/// nearest raider. None where neither places it.
let private walkToRaid (view: ColonyView) (raid: HostileInfo list) (creep: CreepInfo) =
    let reach =
        if partCount creep.Body RangedAttack > 0 then
            Engine.rangedRange
        else
            1

    placementOf view creep.Name
    |> Option.bind (fun at ->
        match raid |> List.choose (fun h -> RoomName.tileRange at h.Pos) with
        | [] -> None
        | tiles -> Some(max 0 (List.min tiles - reach)))

/// One fight's cast as its price chose it (`fighterCast`, #654): `Bodies`
/// bodies of `Blocks` blocks of one template.
type FighterCast =
    {
        Template: BodyTemplate
        Blocks: int
        Bodies: int
        /// Whether the set ends the fight: the raid by the exchange, a kill
        /// by every trespasser it kills.
        Ends: bool
        /// Whether some set priced trades the raid down rather than kiting
        /// it (`Trades`): the row's own win (`rowAnswer`).
        Trades: bool
        /// Whether the melee body is sized holding its post (`Stance.Hold`,
        /// #531): the stance the gate prices it at, or no set ending the raid
        /// at the standoff.
        Held: bool
    }

/// The block counts of a template one body of which the bank buys
/// (`castable`): one up to the most whole blocks under the part cap and the
/// capacity; none where it buys no block.
let private blocksBought (view: ColonyView) (template: BodyTemplate) : int list =
    let pattern = patternOfTemplate template

    if castable view pattern then
        [
            1 .. min
                (Engine.maxBodyParts / List.length pattern.Block)
                (view.Bank.Capacity / bodyCost pattern.Block)
        ]
    else
        []

/// The most whole blocks of a template one body carries.
let private blocksMostOf (template: BodyTemplate) : int =
    Engine.maxBodyParts / List.length (patternOfTemplate template).Block

/// Whether our economy works a room (#656): one we mine, a child's outpost
/// we watch (`trespassRooms`), or an outpost of ours a raid has shut or
/// threatens. Any other room a fight is had in is a rival's.
let private economyOurs (view: ColonyView) (room: string) : bool =
    trespassRooms view room
    || view.RaidedOutposts.Contains room
    || view.ThreatenedOutposts.Contains room

/// Whether a contested room is ours to keep (#656): every room but a
/// rival's remote we harass that no colony of ours mines (`MinedRooms`) and
/// our economy does not work. A home, an outpost, another colony's home we
/// defend: a fight there keeps what is ours; in a rival's remote it only
/// denies.
let contestOurs (view: ColonyView) (room: string) : bool =
    economyOurs view room
    || view.MinedRooms.Contains room
    || not (Set.contains room (harassRooms view))

/// What a tick of a declared outpost open is worth (#585): its sources'
/// yield, reserved, off the declaration (`Colony.declared`), whether or not
/// a stand-down projects the room this tick. Nothing for a room no colony
/// declares an outpost.
let internal outpostTickWorth (room: string) : int =
    Colony.declared
    |> List.tryPick (fun colony ->
        colony.Outposts |> List.tryFind (fun outpost -> outpost.RoomName = room))
    |> Option.map (fun outpost -> List.length outpost.Sources * Engine.heldOutputPerTick)
    |> Option.defaultValue 0

/// What a reopen sends back into an outpost (#585): the crew its declaration
/// asks for, priced off the body patterns — per source an anchor that drains
/// it and the haul of its yield over a round trip home, and a two-CLAIM
/// reserver — and the room's income lost while they are cast and walk back
/// (`outpostTickWorth`). Nothing for a room no colony declares an outpost.
let internal outpostCrewWorth (room: string) : float =
    let declared =
        Colony.declared
        |> List.tryPick (fun colony ->
            colony.Outposts
            |> List.tryFind (fun outpost -> outpost.RoomName = room)
            |> Option.map (fun outpost -> colony.Home, outpost))

    match declared with
    | None -> 0.0
    | Some(home, outpost) ->
        let walk =
            Engine.roomSide
            * (RoomName.hopsBetween home room |> Option.defaultValue 1 |> max 1)

        // The pattern repeated until it holds `needed` of `part`.
        let sized (pattern: BodyPattern) (part: BodyPart) (needed: int) =
            let per = pattern.Block |> List.filter ((=) part) |> List.length |> max 1
            List.replicate ((needed + per - 1) / per) pattern.Block |> List.concat

        let drain = Engine.heldOutputPerTick

        let anchor =
            sized anchorPattern Work ((drain + Engine.harvestPerWork - 1) / Engine.harvestPerWork)

        let hauler =
            sized
                haulerPattern
                Carry
                ((drain * 2 * walk + Engine.carryPartCapacity - 1) / Engine.carryPartCapacity)

        let reserver = sized reserverPattern BodyPart.Claim 2

        let crew =
            (List.replicate (List.length outpost.Sources) (anchor @ hauler) |> List.concat)
            @ reserver

        let respawn = Engine.spawnTicksPerPart * List.length crew + walk

        float (bodyCost crew) + float (outpostTickWorth room * respawn)

/// How many ticks a hold runs past a raid's last sighting (#585), off what it
/// weighs: the odds the raid still stands there or is back fall in a line
/// from certain to nothing over its round trip, `trip`; a reopen at that age
/// risks the crew it sends back, `crew`, at those odds; and a look that would
/// end the hold comes within `evidence` ticks, each such wait costing the
/// room's `income` a tick. The room reopens once the risk falls under one
/// wait's income, never before the evidence can come: longer for a slower
/// raid or a dearer crew, shorter for a richer room.
let holdSpan (income: float) (crew: float) (trip: int) (evidence: int) : int =
    if crew <= 0.0 then
        evidence
    else
        max evidence (int (ceil (float trip * (1.0 - income * float evidence / crew))))

/// How often a room that wants eyes is looked at again (#534): one look in
/// five re-reads a raid row's clock and chase well inside a melee's walk
/// across a room (~50 ticks), and leaves an observer four ticks in five for
/// the rest. What a stand-down's evidence waits on where an observer reaches.
let lookStride = 5

/// A room as its worth prices it (#658, #663): the kill's pricing
/// view (`killPricing`), the bodies out of sight the world remembers among
/// its hostiles (`Quarry.ghosts`), with each body's last steps kept: the
/// group is there or close by, so a carrier walking off is priced at the
/// share of its kill landed before it is gone (`catchOdds`), never as
/// certain as a digger on its seat (a cast, landing long after one step,
/// reads none, #652). Each body is priced on the tile it is the room's
/// quarry on (`Quarry.placed`): a dancer standing across the border on the
/// tile it crossed out from, so the room's price holds through the tick it
/// is out (its camp, `catchOdds`), and the group is not pulled from the room
/// and back. Built once a tick per view and room (`pricingMemo`): the
/// worth, the rate, the muster and the camp read one.
let private pricingOf (view: ColonyView, room: string) : ColonyView =
    let priced = killPricing view room

    { priced with
        Hostiles =
            priced.Hostiles
            |> List.map (fun h ->
                match Quarry.placed room h.Pos (Map.tryFind h.Id view.Bounces) with
                | Some tile when tile <> h.Pos -> { h with Pos = tile }
                | _ -> h)
            // A dancer in sight across the border is laid back by the view
            // too (`Quarry.ghosts`) and by the look beside: one body.
            |> List.distinctBy (fun h -> h.Id, h.Pos)
        Stepped = view.Stepped
        SteppedBack = view.SteppedBack
    }

/// Whether two views price a room alike (`pricingOf`): the same hostiles,
/// projection, candidates, steps and world rooms, whatever else a copy of
/// the view changed (the worths or the group laid in, `sweepAll`). The
/// priced view keeps the first asker's world: a reader takes the world's
/// readings past these (`Worths`, `Responses`, `Flees`) off its own view.
/// It also reads RecalledHealers, Sources, RaidedOutposts, MinedRooms,
/// Borrowed, World.Facts and our own bodies, fixed at cut time: a copy that
/// changes one of them must join the key.
let private pricesAlike (view: ColonyView, room: string) (view': ColonyView, room': string) =
    room = room'
    && (obj.ReferenceEquals(view, view')
        || obj.ReferenceEquals(view.Hostiles, view'.Hostiles)
           && obj.ReferenceEquals(view.Spatial, view'.Spatial)
           && obj.ReferenceEquals(view.Harass, view'.Harass)
           && obj.ReferenceEquals(view.Bounces, view'.Bounces)
           && obj.ReferenceEquals(view.Stepped, view'.Stepped)
           && obj.ReferenceEquals(view.SteppedBack, view'.SteppedBack)
           && obj.ReferenceEquals(view.RoomControl, view'.RoomControl)
           && obj.ReferenceEquals(view.Tuning, view'.Tuning)
           && obj.ReferenceEquals(view.World.Rooms, view'.World.Rooms)
           && obj.ReferenceEquals(view.World.Quarries, view'.World.Quarries)
           && obj.ReferenceEquals(view.World.Observers, view'.World.Observers)
           && obj.ReferenceEquals(view.World.Answers, view'.World.Answers)
           && obj.ReferenceEquals(view.World.Dangers, view'.World.Dangers)
           && view.Time = view'.Time)

let private pricingMemo: TickMemo.Memo<ColonyView * string, ColonyView> =
    TickMemo.create 16 pricesAlike

let private sweepPricing (view: ColonyView) (room: string) : ColonyView =
    TickMemo.recall pricingMemo (view, room) pricingOf

/// The defence response a room's worth prices where the rival's dossier
/// holds none for it: it answers at once, the worst case.
let private instantResponse = 0

/// What the world's worths are read over (#663 M2), of every colony alike:
/// the rooms our economy works — every home of ours and every room a colony
/// mines, raises, defends or watches for a child, every declared outpost —,
/// the [[sweep]] candidates by the enemy each names, and the rival
/// structures the dismantlers are handed.
type private WorthScope =
    {
        Ours: Set<string>
        Candidates: Map<string, string>
        Dismantles: string list
    }

let private scopeOf (views: ColonyView list) : WorthScope =
    {
        Ours =
            views
            |> List.collect (fun view ->
                (view.Sources
                 |> List.choose (fun source -> SpatialInfo.roomOf view.Spatial source.Id))
                @ Set.toList (homesOfOurs view)
                @ view.Borrowed.Rooms
                @ view.Borrowed.Defended
                @ view.Borrowed.ChildOutposts
                @ Set.toList view.MinedRooms)
            |> Set.ofList
        Candidates =
            views
            |> List.collect (fun view -> view.Harass |> List.map (fun h -> h.RoomName, h.Enemy))
            |> Map.ofList
        Dismantles = views |> List.collect _.Dismantles |> List.distinct
    }

/// A room's prey, of the world's (#663 M2): in a room our economy works or a
/// candidate, a body that trespasses wherever it stands (`trespassingBody`)
/// we may strike there (`mayStrike`, the room's fight ours); and in a
/// candidate every unarmed body of the enemy it names, a cripple shot
/// disarmed or bare among them — whether it is worth its kill is its price's
/// (#648).
let private preyOf (scope: WorthScope) (priced: ColonyView) (room: string) (hostile: HostileInfo) =
    hostile.Pos.Room = room
    && ((scope.Ours.Contains room || scope.Candidates.ContainsKey room)
        && trespassingBody priced.Hostiles hostile
        && mayStrike priced (Set.singleton room) hostile
        || Map.tryFind room scope.Candidates = Some hostile.Owner
           && not (HostileInfo.armed priced.Hostiles hostile))

/// Our income a tick a fight keeps in a room (#663 M2): what we earn there
/// while we hold it, each of its sources at its held yield (reserved or
/// owned) — the projection's sources, or where it projects none (a
/// stand-down withholds the room), the declared outpost's
/// (`outpostTickWorth`) —; nothing in a room our economy does not work.
let private keptIn (scope: WorthScope) (view: ColonyView) (room: string) : float =
    if not (scope.Ours.Contains room) then
        0.0
    else
        match sourcesIn view room with
        | 0 -> float (outpostTickWorth room)
        | sources -> float (sources * Engine.heldOutputPerTick)

/// One room's worth (#663 M2, `RoomWorth`), over the world's scope
/// (`WorthScope`) and read off one view of the room as its worth prices it
/// (`sweepPricing`): its prey (`preyOf`), each at its take (`harmRate`) and
/// its kill value (`killValue`, #621), and the candidate's enemy's escort at
/// its kill — an Invader's or a third player's escort only gates the
/// exchange (we do not defend his remote for him) —, each at the odds it
/// still stands there, read for its value and for a cast (`odds`); our
/// income kept there (`keptIn`); the rival containers the dismantlers are
/// handed there at their rebuild over the hits they stand at; its raiders
/// as the escort; and its owner's answer (`WorldView.Responses`, else
/// `instantResponse`).
let private worthOf (scope: WorthScope) (view: ColonyView) (room: string) : RoomWorth =
    let priced = sweepPricing view room

    let prey = priced.Hostiles |> List.filter (preyOf scope priced room)

    let escort = raidersIn priced room

    let valued =
        prey
        @ (escort
           |> List.filter (fun h -> Map.tryFind room scope.Candidates = Some h.Owner))
        |> List.distinctBy _.Id

    // A container's hits as the world or the projection read them, else
    // whole.
    let hitsOf (id: string) =
        Map.tryFind room view.World.Facts
        |> Option.bind (fun facts -> Map.tryFind id facts.Hits)
        |> Option.orElse (Map.tryFind id view.Spatial.Hits)
        |> Option.map _.Hits
        |> Option.defaultValue Engine.containerHits

    {
        Kept = keptIn scope view room
        Takes =
            prey
            |> List.map (fun h -> h.Id, harmRate priced h replacementWalk)
            |> Map.ofList
        Values = valued |> List.map (fun h -> h.Id, killValue priced h) |> Map.ofList
        Odds =
            valued
            |> List.map (fun h ->
                h.Id, (odds priced Quarry.Reading.Value h, odds priced Quarry.Reading.Cast h))
            |> Map.ofList
        Dark =
            valued
            |> List.filter (fun h ->
                Map.tryFind h.Id view.World.Quarries
                |> Option.exists (fun track -> track.Seen < view.Time))
            |> List.map _.Id
            |> Set.ofList
        Structures =
            scope.Dismantles
            |> List.filter (fun id ->
                SpatialInfo.placementOf view.Spatial id
                |> Option.exists (fun at -> at.Room = room))
            |> List.map (fun id ->
                id,
                {
                    Rebuild = Engine.containerBuildCost
                    Hits = hitsOf id
                })
            |> Map.ofList
        Exits = 0.0
        Escort = escort
        Response = Map.tryFind room view.World.Responses |> Option.defaultValue instantResponse
    }

let private worthMemo: TickMemo.Memo<ColonyView * string, RoomWorth> =
    TickMemo.create 16 (fun (view, room) (view', room') ->
        obj.ReferenceEquals(view, view') && room = room')

/// A room's worth this tick (#663 M2): the world's, laid once for every
/// colony (`WorldView.Worths`, `sweepAll`), else priced off this view once a
/// tick (`worthOf`).
let roomWorth (view: ColonyView) (room: string) : RoomWorth =
    match view.World.Worths |> Option.bind (Map.tryFind room) with
    | Some worth -> worth.Value
    | None ->
        TickMemo.recall worthMemo (view, room) (fun (view, room) ->
            worthOf (scopeOf [ view ]) view room)

/// The view a room is priced off (`sweepPricing`), of the views laid
/// together, whatever their order: the colony that mines it, else one that
/// works it as a [[sweep]] candidate, else one that projects it; of equals,
/// the first home by name. Its worth's scope is the world's either way, so
/// only the bodies its cut lays and the tiles it projects follow the choice.
let private worthOwner (views: ColonyView list) (room: string) : ColonyView option =
    let first (fits: ColonyView -> bool) =
        views
        |> List.filter fits
        |> List.sortBy (fun view -> SpatialInfo.homeName view.Spatial)
        |> List.tryHead

    first (fun view -> minesRoom view room)
    |> Option.orElse (first (fun view -> view.Harass |> List.exists (fun h -> h.RoomName = room)))
    |> Option.orElse (first (fun view -> Map.containsKey room view.Spatial.Rooms))

/// Every room any of the views projects or works as a candidate, its worth
/// priced on first read over the world's scope (`scopeOf`) off its owner's
/// view (`worthOwner`) as it stands when read (`current`: the views with the
/// table laid in), once for all.
let private worthTable
    (views: ColonyView list)
    (current: unit -> ColonyView list)
    : Map<string, Lazy<RoomWorth>> =
    let scope = scopeOf views

    let ownedBy (home: string) =
        current ()
        |> List.tryFind (fun view -> SpatialInfo.homeName view.Spatial = home)

    views
    |> List.collect (fun view ->
        (view.Spatial.Rooms |> Map.keys |> List.ofSeq)
        @ (view.Harass |> List.map _.RoomName)
        @ Set.toList view.HarassCast)
    |> List.distinct
    |> List.choose (fun room ->
        worthOwner views room
        |> Option.map (fun owner ->
            room,
            lazy
                (worthOf
                    scope
                    (ownedBy (SpatialInfo.homeName owner.Spatial) |> Option.defaultValue owner)
                    room)))
    |> Map.ofList

/// How long a player's raid holds the outpost it was last seen standing in
/// shut past that sighting (#585), where no look has found it gone
/// (`holdSpan`): over its slowest body's round trip (`WorldView.raidTrip`),
/// the outpost's crew (`outpostCrewWorth`) against the income a fight there
/// keeps (`RoomWorth.Kept`), the evidence a look every `lookStride` ticks
/// where an observer reaches the room, else none before the trip is out.
let internal raidHoldSpan (view: ColonyView) (room: string) (raid: HostileInfo list) : int =
    let trip = WorldView.raidTrip raid

    holdSpan
        (roomWorth view room).Kept
        (outpostCrewWorth room)
        trip
        (if observerReaches view room then
             min lookStride trip
         else
             trip)

/// The dancer the [[sweep]]'s group camps in its room (#658), and the tiles
/// of the room it steps in and out on (`dancerEntries`), the most used
/// first: where the body its
/// kill score leads, the group's (`team`) over the room's prey as the sweep
/// prices them (`sweepPricing`: a dancer across the border laid back), is a
/// dancer (`dancingOn`) whose camp pays (`campOdds`). None where the head is
/// a body caught where it stands, or nothing pays.
let sweepDancer
    (view: ColonyView)
    (room: string)
    (team: Fighter list)
    : (HostileInfo * RoomPos list) option =
    match team with
    | [] -> None
    | fighter :: _ ->
        let priced = sweepPricing view room
        let score = killScoresIn priced room team

        priced.Hostiles
        |> List.filter (fun h -> h.Pos.Room = room && trespasser priced (Set.singleton room) h)
        |> List.map (fun h -> score fighter None 0 h, h)
        |> List.filter (fun (worth, _) -> worth > 0.0)
        |> List.sortBy (fun (worth, h) -> -worth, h.Id)
        |> List.tryHead
        |> Option.map snd
        |> Option.filter (dancingOn priced)
        |> Option.map (fun dancer ->
            let entries =
                dancerEntries priced dancer
                |> List.countBy id
                |> List.sortBy (fun (tile, uses) -> -uses, tile.Y, tile.X)
                |> List.map fst

            dancer, entries)

/// The odds a camp on a border dancer lands (#663 M3) and the ticks the
/// team's kill of it takes (`killTicksOf`): its best body's odds
/// (`campOdds`: the camp lands while any body of the team strikes it),
/// priced as the [[sweep]] prices the room (`sweepPricing`); none for a kill
/// the team never lands, or no team. `dancer` is the body as `sweepDancer`
/// names it.
let private campChance
    (view: ColonyView)
    (room: string)
    (team: Fighter list)
    (dancer: HostileInfo)
    : (float * int) option =
    let priced = sweepPricing view room
    let healOn = healReachingWithin priced room priced.Tuning.ReachHorizon

    match team, killTicksOf priced healOn team dancer with
    | [], _
    | _, None -> None
    | team, Some kill ->
        let odds =
            team
            |> List.fold (fun best fighter -> max best (campOdds priced fighter kill dancer)) 0.0

        Some(min 1.0 odds, kill)

/// What camping a border dancer earns a tick (#663 M3), and the camp's odds
/// it is priced at (`campChance`): its kill value at those odds over the
/// ticks its kill takes; nothing for a kill the team never lands, or no
/// team, at no odds.
let campPrice
    (view: ColonyView)
    (room: string)
    (team: Fighter list)
    (dancer: HostileInfo)
    : float * float option =
    match campChance view room team dancer with
    | None -> 0.0, None
    | Some(odds, kill) ->
        odds * killValue (sweepPricing view room) dancer / float (max 1 kill), Some odds

/// `campPrice`'s value.
let campValue (view: ColonyView) (room: string) (team: Fighter list) (dancer: HostileInfo) : float =
    campPrice view room team dancer |> fst

/// The ticks a body standing at `from` walks to a room at its plain `pace`
/// (#658): none in the room or on its border's doorstep (the tile beside it,
/// where a crossing body stands one tick in and one out), else its tiles to
/// the room's nearest edge off the names (`RoomName.tileRange` to the centre
/// less half a room), a floor on the walk, never the route.
/// `Int32.MaxValue` off the grid.
let private doorTicks (from: RoomPos) (pace: int) (room: string) : int =
    if from.Room = room then
        0
    else
        match RoomName.tileRange from (RoomPos.at room { X = 25; Y = 25 }) with
        | Some tiles ->
            match max 0 (tiles - Engine.roomSide / 2) with
            | edge when edge <= 1 -> 0
            | edge -> edge * pace
        | None -> System.Int32.MaxValue

/// The rival structures of a room's worth a crew takes down where it pays
/// (#661), against the rate its fight earns without them (`gained` over
/// `ticks`): each at its rebuild, over the ticks its hits take at the crew's
/// `dismantle` a tick, taken the best first while it raises the rate. The
/// rebuild taken and the ticks it costs; nothing for a crew that dismantles
/// nothing.
let private structuresTaken
    (worth: RoomWorth)
    (dismantle: int)
    (gained: float)
    (ticks: int)
    : float * int =
    if dismantle <= 0 then
        0.0, 0
    else
        worth.Structures
        |> Map.toList
        |> List.map (fun (_, standing) ->
            float standing.Rebuild, (standing.Hits + dismantle - 1) / dismantle)
        |> List.sortByDescending (fun (rebuild, cost) -> rebuild / float (max 1 cost))
        |> List.fold
            (fun (taken, spent) (rebuild, cost) ->
                if
                    rebuild * float (max 1 (ticks + spent)) > (gained + taken) * float (max 1 cost)
                then
                    taken + rebuild, spent + cost
                else
                    taken, spent)
            (0.0, 0)

/// The odds a body's work stops for our holding its room (#663 M2): none for
/// one that stays and out-heals the crew (never killed, or a dancer whose
/// heal across the border undoes the camp, `outHealsCamp`); else the odds it
/// slips the crew (one caught is priced by its kill instead), times its
/// owner's flee share as its dossier saw it (`WorldView.Flees`), if any.
let private stopsWorking
    (view: ColonyView)
    (priced: ColonyView)
    (body: Fighter)
    (hostile: HostileInfo)
    (kill: int option)
    (catch: float)
    =
    match kill with
    | None -> 0.0
    | Some _ when dancingOn priced hostile && outHealsCamp priced body hostile -> 0.0
    | Some _ ->
        Map.tryFind hostile.Owner view.World.Flees
        |> Option.defaultValue 1.0
        |> (*) (1.0 - catch)

/// The one rate a crew of ours earns in a room a tick of its effort (#663
/// M2), off the room's worth (`roomWorth`). What it earns: the kills it
/// lands there (each body's kill value at its odds read as `reading`, a
/// cast's or a standing group's, times the odds the crew catches it,
/// `catchOdds`, nothing of one that dies of age first); and over the ticks
/// it holds the room clear, its kills past the escort's, our income kept
/// there (`RoomWorth.Kept`) and the rival income that stops for our holding
/// it — each economy body's take at its odds (`RoomWorth.takes`) times the
/// odds its work stops while we hold (`stopsWorking`): a body that keeps
/// working until its kill lands is denied nothing before it, its kill value
/// carrying the loss after; and the structures it takes down where they pay
/// (`structuresTaken`, its `dismantle` a tick). Over the ticks they cost: its
/// `arrival`, the escort's kill (`killTicks`) and each economy body's
/// together (`killTicksOf`), concentrated, one at a time, and the
/// structures'. Nothing where the escort beats the crew (exposure: it moves
/// on rather than stands off), and nothing for no crew.
let roomRate
    (reading: Quarry.Reading)
    (view: ColonyView)
    (team: Fighter list)
    (dismantle: int)
    (arrival: int)
    (room: string)
    : float =
    match team with
    | [] -> 0.0
    | body :: _ ->
        let worth = roomWorth view room
        let priced = sweepPricing view room

        let escort =
            match worth.Escort with
            | [] -> Some 0
            | _ when won (exchangeIn priced room team) -> killTicks priced room team
            | _ -> None

        match escort with
        | None -> 0.0
        | Some escortTicks ->
            let healOn = healReachingWithin priced room priced.Tuning.ReachHorizon
            let landing = max 0 arrival + escortTicks
            let values = RoomWorth.values reading worth
            let takes = RoomWorth.takes reading worth

            // Each body the worth values, its kill and the odds the crew
            // lands it, read once.
            let bodies =
                priced.Hostiles
                |> List.filter (fun h -> h.Pos.Room = room && values.ContainsKey h.Id)
                |> List.map (fun h ->
                    let kill = killTicksOf priced healOn team h

                    let catch =
                        match kill with
                        | Some kill when h.TicksToLive > landing + kill ->
                            catchOdds priced body None landing kill h
                        | _ -> 0.0

                    h, kill, catch)

            let kills =
                bodies
                |> List.sumBy (fun (h, kill, _) ->
                    if takes.ContainsKey h.Id then
                        Option.defaultValue 0 kill
                    else
                        0)

            let landed = bodies |> List.sumBy (fun (h, _, catch) -> catch * values.[h.Id])

            let denied =
                bodies
                |> List.sumBy (fun (h, kill, catch) ->
                    match Map.tryFind h.Id takes with
                    | Some take -> take * stopsWorking view priced body h kill catch
                    | None -> 0.0)

            let gained = landed + (worth.Kept + denied) * float kills

            let taken, broken = structuresTaken worth dismantle gained (landing + kills)

            (gained + taken) / float (max 1 (landing + kills + broken))

/// What a crew of these creeps dismantles a tick: its live WORK at the
/// engine's dismantle power.
let private dismantleOf (creeps: CreepInfo list) : int =
    creeps
    |> List.sumBy (fun creep -> partCount creep.Body Work * Engine.dismantlePower)

/// A [[sweep]]'s group (#658): the bodies of this colony holding the Guard of
/// one of its candidates, by last tick's Tasks (`held`), wherever they stand,
/// and every armed one standing in a candidate.
let sweepGroup (view: ColonyView) (held: Assignments) : CreepInfo list =
    let rooms = view.Harass |> List.map _.RoomName |> Set.ofList
    let guards = rooms |> Set.map (fun room -> taskId (Guard room))

    ColonyView.bodies view
    |> List.filter (fun creep ->
        Map.tryFind creep.Name held |> Option.exists guards.Contains
        || (Fighter.ofCreep creep).Reach > 0
           && SpatialInfo.creepPlacementOf view.Spatial creep.Name
              |> Option.exists (fun at -> rooms.Contains at.Room))

/// A fighter's plain pace, a tile a tick where it has none.
let private paceOf (fighter: Fighter) =
    Gait.ticks Gait.plain fighter.Gait |> Option.defaultValue 1

/// The [[sweep]]'s whole group over every colony's view (#658, `sweepGroup`),
/// each body once, with where it stands.
let private sweepBodies (held: Assignments) (views: ColonyView list) =
    views
    |> List.collect (fun view ->
        sweepGroup view held
        |> List.choose (fun creep ->
            placementOf view creep.Name |> Option.map (fun at -> creep, at)))
    |> List.distinctBy (fun (creep, _) -> creep.Name)

/// How many bodies a contested room's job seats for its group (#658, #663
/// M3): the whole group, every body on it with where it stands (`group`),
/// and one more where a body like its weakest, landing with its last, raises
/// what the group earns there a tick (`roomRate`, read as a cast: the seat
/// is what a body is cast for) by more than a tick of that body's life costs
/// (its energy over `Engine.creepLifetime`) and more than a body of the
/// group earns there a tick already (its share of the rate): a free fighter
/// joins the one group while concentrating pays, never a second room, and
/// the group stops where the next body would earn less than its share. No
/// group seats none: the job's own count does.
let private seatsPaidOf
    (view: ColonyView)
    (room: string)
    (group: (CreepInfo * RoomPos) list)
    : int =
    let placed = group |> List.map (fun (creep, at) -> at, Fighter.ofCreep creep)

    let dismantle = dismantleOf (List.map fst group)

    let rateOf (team: (RoomPos * Fighter) list) =
        let arrival =
            team
            |> List.map (fun (at, fighter) -> doorTicks at (paceOf fighter) room)
            |> List.max

        if arrival = System.Int32.MaxValue then
            0.0
        else
            roomRate Quarry.Reading.Cast view (List.map snd team) dismantle arrival room

    match placed with
    | [] -> 0
    | placed ->
        let strength (fighter: Fighter) =
            fighter.DamageAt Engine.rangedRange + fighter.DamageAt 1

        let weakestCreep, _ =
            group |> List.minBy (fun (creep, _) -> strength (Fighter.ofCreep creep))

        let weakest = Fighter.ofCreep weakestCreep

        let last =
            placed
            |> List.maxBy (fun (at, fighter) -> doorTicks at (paceOf fighter) room)
            |> fst

        // The seat pays where what the body adds a tick beats both what a
        // tick of its life costs (its energy over its lifetime) and what a
        // body of the group earns there a tick already: a body that would
        // earn less than its share is worth more anywhere else, so the
        // group stops growing where concentrating stops paying, never a
        // body at a time for ever.
        let lifeTick =
            weakestCreep.Body
            |> Map.toList
            |> List.collect (fun (part, n) -> List.replicate n part)
            |> bodyCost
            |> float
            |> fun energy -> energy / float Engine.creepLifetime

        let now = rateOf placed
        let share = now / float (List.length placed)

        List.length placed
        + (if rateOf ((last, weakest) :: placed) - now > max lifeTick share then
               1
           else
               0)

let private seatsPaidMemo: TickMemo.Memo<ColonyView * string * (string * RoomPos) list, int> =
    TickMemo.create 8 (fun (view, room, group) (view', room', group') ->
        obj.ReferenceEquals(view, view') && room = room' && group = group')

/// `seatsPaidOf`, once a tick per view, room and group — its bodies and
/// where they stand, whatever order they come in: the first reading of a
/// group is every later one's, so the operation's shadow reads the seats the
/// room's Brief priced rather than pricing them again (#663 S7b).
let seatsPaid (view: ColonyView) (room: string) (group: (CreepInfo * RoomPos) list) : int =
    let key = group |> List.map (fun (creep, at) -> creep.Name, at) |> List.sortBy fst

    TickMemo.recall seatsPaidMemo (view, room, key) (fun _ -> seatsPaidOf view room group)

/// One body a muster weighs (#663 M3): its fight, the ticks it lands in the
/// room (none for one already waiting), and its energy.
type Mustering =
    {
        Fighter: Fighter
        Lands: int
        Energy: float
    }

/// The ticks a body standing at `at` lands in a room at its plain pace
/// (`doorTicks`): none in it or on its doorstep.
let landsIn (at: RoomPos) (fighter: Fighter) (room: string) : int =
    doorTicks at (paceOf fighter) room

/// The one muster rule (#535, #658, #663 M3): whether a fight's group waits
/// at its last crossing for the rest of it, priced off the room's worth
/// (`roomWorth`) whatever the room's phase, and its price a tick where it
/// does — what the first come would lose over the waiting it saves, over
/// the spread. `group`: every body coming (`Mustering`), an armed one
/// standing by or one still to be cast alike. Its vanguard is the bodies
/// landing first, together; the spread, the ticks until the last lands. No
/// muster with no spread (a group all waiting goes in: a muster never holds
/// bodies that only wait for each other), nor where the whole group loses
/// the room's escort (`RoomWorth.Escort`): nothing coming wins it, so nobody
/// waits, and the room's verdict stands it down. The vanguard is caught
/// where the escort beats it alone, or, in a room not ours to keep
/// (`contestOurs`), where its owner's defence answers (`RoomWorth.Response`)
/// before the rest land; caught, it loses its bodies' energy. Waiting costs
/// nothing where the escort beats the vanguard (it would keep and kill
/// nothing before it fell); else the room's economic delta over the spread
/// (`RoomWorth.delta`: our income kept there and the rival's denied) and the
/// prey's kills it lets walk off or age out (`killsLanded` at once against
/// after the spread), each body at the odds it is still there. It waits
/// where the loss exceeds the waiting.
let muster (view: ColonyView) (room: string) (group: Mustering list) : float option =
    let group = group |> List.filter (fun body -> body.Lands < System.Int32.MaxValue)

    match group with
    | []
    | [ _ ] -> None
    | group ->
        let landing = group |> List.map _.Lands |> List.min
        let spread = (group |> List.map _.Lands |> List.max) - landing

        if spread <= 0 then
            None
        else
            let worth = roomWorth view room
            let priced = sweepPricing view room
            let vanguard = group |> List.filter (fun body -> body.Lands = landing)
            let lone = vanguard |> List.map _.Fighter

            let beats (team: Fighter list) =
                not (List.isEmpty worth.Escort) && not (won (exchangeIn priced room team))

            if beats (group |> List.map _.Fighter) then
                None
            else
                let beaten = beats lone

                let caught = beaten || not (contestOurs view room) && spread > worth.Response

                let loss = if caught then vanguard |> List.sumBy _.Energy else 0.0

                // The prey's kills: the escort's is what the vanguard risks.
                let values =
                    RoomWorth.values Quarry.Reading.Value worth
                    |> Map.filter (fun id _ -> worth.Takes.ContainsKey id)

                let waiting =
                    if beaten then
                        0.0
                    else
                        RoomWorth.delta Quarry.Reading.Value worth * float spread
                        + killsLanded priced room lone landing values
                        - killsLanded priced room lone (landing + spread) values

                if loss > waiting then
                    Some((loss - waiting) / float spread)
                else
                    None

/// The share of one body's energy a fight that holds it `ticks` spends of
/// its life (`Engine.creepLifetime`, #654): a body that ends a fight
/// sooner is free for the next one sooner.
let private lifeSpent (template: BodyTemplate) (blocks: int) (ticks: int) =
    float (blocks * bodyCost (patternOfTemplate template).Block)
    * float (min ticks Engine.creepLifetime)
    / float Engine.creepLifetime

/// The ticks a colony's spawns take to cast `n` bodies of a template's
/// `blocks` (#654): a round at a time, every spawn one body a round.
let private spawnRounds (view: ColonyView) (template: BodyTemplate) (blocks: int) (n: int) =
    let spawns = max 1 (List.length view.Spawns)
    castSpawnTicks template blocks * ((n + spawns - 1) / spawns)

/// What `n` bodies of a template's `blocks` yet to be cast cost for a fight
/// that holds them `ticks` from their birth (#654): the life it spends of
/// each (`lifeSpent`), and their spawn time at what a tick of the colony's
/// spawn is worth (`spawnTickWorth`).
let private castSpent
    (view: ColonyView)
    (template: BodyTemplate)
    (blocks: int)
    (n: int)
    (ticks: int)
    =
    float n
    * (lifeSpent template blocks ticks
       + float (castSpawnTicks template blocks) * spawnTickWorth view)

/// The raid a fight's cast is priced against (#654): the raiders in the
/// room, and in the home every body that endangers it (#429: a dismantler
/// the exchange prices beside its healers).
let private fightRaid (view: ColonyView) (room: string) : HostileInfo list =
    raidersIn view room
    @ (if room = SpatialInfo.homeName view.Spatial then
           view.Hostiles
           |> List.filter (fun h -> h.Pos.Room = room && endangersOurs view h)
       else
           [])
    |> List.distinctBy _.Id

/// The sets of bodies that end the fight in a room (#654), cheapest first,
/// the last the answer where none does (`fighterCast` takes the first the
/// gate seats): of the `templates` the colony's bank buys at its capacity,
/// each set at most `bodies` bodies of one template and one block count.
/// Priced as their
/// spawn time and the share of their energy their walk and kill spend of
/// their lives (`castSpent`), and every tick from the cast to the kill at
/// what the fight costs a tick.
///
/// Against a raid: the sets whose exchange ends it, trading or kiting, at
/// the stance the gate prices each body at — a melee body holding its post
/// where a raider keeps off its swing, every other at the standoff
/// (`blocksOutcome` alone, `bodiesOutcome` for more, the raid
/// focusing one at a time), each priced its spent life, and its spawn time,
/// the wait for its last body (its spawns cast a round at a time), its walk
/// and kill (`killTicks`, a lifetime where it never kills) at the room's
/// economic delta a tick (`RoomWorth.delta`, #656: our yield there kept and
/// the take of the rival's economy bodies working beside the raid denied);
/// by least price, then the fewest parts,
/// the fewest bodies, the template's order. Last, the melee body holding
/// its post (`Stance.Hold`, #531): one body of the fewest blocks that hold
/// it, ending it where the bank buys that body, `bodies` of them where the
/// bank buys only smaller ones; else the most, `bodies` of them. A home of
/// ours keeps its row's melee pair (#654's home exception): pairs of melee bodies are
/// priced there, a melee body of more than one block is cast twice, and the
/// last answer is the pair of the biggest body the bank buys where that
/// pair ends the raid.
///
/// Against a kill, no raider standing there (nor, in the home, a body that
/// endangers it): the sets that kill the most of
/// its trespassers — a melee body those its swing catches
/// (`strikesAtStandoff`) and the dancers its camp covers (`camped`, #663
/// M4: a swing may win a camp by price), a ranged one any it strikes
/// (`canStrike`), each
/// killed (`killTicksOf`) before it ages out, landing after the body's
/// spawn time and the walk —, then the most the kills land
/// (`killsLanded`, each trespasser's `killValue`) less the work each does
/// until its kill lands (`harmRate`) and the set's spent life over the walk
/// and every kill in turn, its bodies already cast priced as a raid's set
/// prices them (no spawn, no energy), then as a raid's: one set. A room where nothing
/// is to be fought casts one melee block.
let private castsByPrice
    (view: ColonyView)
    (room: string)
    (templates: BodyTemplate list)
    (bodies: int)
    : FighterCast list =
    let order (template: BodyTemplate) =
        List.findIndex ((=) template) bodyTemplates

    let quiet =
        {
            Template = List.head templates
            Blocks = 1
            Bodies = 1
            Ends = true
            Trades = true
            Held = false
        }

    let parts (template: BodyTemplate) blocks n =
        n * blocks * List.length (patternOfTemplate template).Block

    let raid = fightRaid view room

    let trespassers =
        view.Hostiles
        |> List.filter (fun hostile ->
            hostile.Pos.Room = room && trespasser view (Set.singleton room) hostile)

    // The room's worth (#656, #663): what each tick of the fight costs and
    // what each kill lands, one price for a raid and a kill alike, read for
    // a cast.
    let worth = lazy (roomWorth view room)

    let walk =
        CastBid.walk view.Tuning (RoomName.hopsBetween (SpatialInfo.homeName view.Spatial) room)

    // The colony's bodies of a template of at least `blocks` blocks already
    // cast, worn or not, in an oven or standing past `life` ticks: a body's
    // blocks are its weapon and heal parts over one block's, the fewest of
    // any. A standing body that ages out before the set's fight is over is
    // no body of it, so its relief is priced cast, as an expiring body leaves
    // its row's count (`Spawns.expiring`). Of the standing ones only those
    // `on` admits.
    let castAmong (on: CreepInfo -> bool) (template: BodyTemplate) (blocks: int) (life: int) =
        let pattern = patternOfTemplate template

        let block =
            partsOf pattern.Block
            |> Map.filter (fun part _ -> part <> Move && part <> Tough)

        let cut =
            match template with
            | BodyTemplate.Melee -> isGuardRowCut
            | BodyTemplate.Ranged -> isRangerRowCut

        let blocksIn (parts: Map<BodyPart, int>) =
            block
            |> Map.fold
                (fun least part count -> min least (partCount parts part / count))
                System.Int32.MaxValue

        let standing =
            ColonyView.bodies view
            |> List.filter (fun creep ->
                cut creep.Name creep.Body
                && blocksIn creep.Body >= blocks
                && creep.TicksToLive > life
                && on creep)

        let oven =
            view.Casting
            |> List.filter (fun cast ->
                let parts = partsOf cast.Body
                cut cast.Name parts && blocksIn parts >= blocks)

        List.length standing + List.length oven

    // A raid's set counts every standing body of the row: the raid is the
    // row's to answer wherever its bodies stand.
    let castAlready = castAmong (fun _ -> true)

    // The ticks the set's last body is spawned after its first: the first
    // waits for the last.
    let waits (template: BodyTemplate) blocks n =
        spawnRounds view template blocks n - castSpawnTicks template blocks

    if not (List.isEmpty raid) then
        let tick = RoomWorth.delta Quarry.Reading.Cast worth.Value
        let home = (homesOfOurs view).Contains room

        // The stance a body meets the raid at, as the gate prices it
        // (`Brief.stanceOf`): a swing holds its post (`Stance.Hold`, #531)
        // where a raider keeps off it at their standoff; every other body
        // fights at the standoff.
        let stanceOf (template: BodyTemplate) =
            let fighter = blockFighter (patternOfTemplate template)

            if
                Fighter.glues fighter
                && view.Hostiles
                   |> List.exists (fun h ->
                       h.Pos.Room = room
                       && HostileInfo.raider view.Hostiles h
                       && not (strikesAtStandoff view fighter h))
            then
                Stance.Hold
            else
                Stance.Standoff

        let sets =
            templates
            |> List.collect (fun template ->
                let pattern = patternOfTemplate template
                let alone = blocksOutcome pattern (stanceOf template) view room

                // More than one body only of a template that outranges a
                // swing: `bodiesOutcome` is the shooters' pair (#609), and
                // prices no swing struck back on the bodies the raid does not
                // focus. A home of ours is the exception (`fighterCast`): its
                // defence keeps the melee pair until pair coordination is
                // priced.
                let most =
                    if Fighter.outranges (blockFighter pattern) || home then
                        bodies
                    else
                        1

                blocksBought view template
                |> List.collect (fun blocks ->
                    [ 1..most ]
                    |> List.map (fun n ->
                        let outcome =
                            if n = 1 then
                                alone blocks
                            else
                                bodiesOutcome pattern view room n blocks

                        template, blocks, n, outcome)))

        let trades = sets |> List.exists (fun (_, _, _, outcome) -> outcome = Trades)

        let priceOf (template: BodyTemplate, blocks, n, _) =
            let team = List.replicate n (blocksFighter (patternOfTemplate template) blocks)

            let kill = killTicks view room team |> Option.defaultValue Engine.creepLifetime

            // The bodies of it already standing or in an oven are cast: they
            // spend only the life the walk and the kill take, and the set is
            // spawned no tick longer for them. A standing one counts only
            // while it outlives the set's cast, walk and kill.
            let cast =
                min n (castAlready template blocks (castSpawnTicks template blocks + walk + kill))

            let fresh = n - cast
            let wait = if fresh = 0 then 0 else waits template blocks fresh
            let spawn = if fresh = 0 then 0 else castSpawnTicks template blocks

            castSpent view template blocks fresh (wait + walk + kill)
            + float cast * lifeSpent template blocks (walk + kill)
            + float (spawn + wait + walk + kill) * tick,
            parts template blocks n,
            n,
            order template

        // More bodies of a template only where no one body of it ends it:
        // the bodies of a set land one at a time, and the gate seats one
        // only where it fits beside those landed (`fits`), so the first of a
        // pair one body would have ended waits idle on the second; and a
        // pair priced a hair under one body flips the ask on the raid's
        // every step, stranding the body already cast.
        let ending =
            let ending = sets |> List.filter (fun (_, _, _, outcome) -> won outcome)

            let alone (template: BodyTemplate) =
                ending |> List.exists (fun (t, _, n, _) -> t = template && n = 1)

            ending |> List.filter (fun (t, _, n, _) -> n = 1 || not (alone t))

        let held =
            let template =
                if List.contains BodyTemplate.Melee templates then
                    BodyTemplate.Melee
                else
                    List.head templates

            let most = blocksMostOf template

            let holds =
                if template = BodyTemplate.Melee then
                    [ 1..most ] |> List.tryFind (blocksBeat guardPattern Stance.Hold view room)
                else
                    None

            // It ends the raid only where the bank buys the body that holds;
            // where it buys only smaller ones, `bodies` of them hold together
            // (the gate seats each beside the others); where it buys none,
            // the one body waits for the bank.
            let buys = blocksBought view template

            let bought = holds |> Option.exists (fun blocks -> List.contains blocks buys)

            // A home of ours (the home exception): the pair of the biggest body the
            // bank buys, focused one at a time, as the home's row cast it.
            let homePair =
                home
                && template = BodyTemplate.Melee
                && not bought
                && not (List.isEmpty buys)
                && won (bodiesOutcome guardPattern view room bodies (List.last buys))

            if homePair then
                {
                    Template = template
                    Blocks = List.last buys
                    Bodies = bodies
                    Ends = true
                    Trades = trades
                    Held = true
                }
            else
                {
                    Template = template
                    Blocks = holds |> Option.defaultValue most
                    Bodies =
                        if bought || List.isEmpty buys && Option.isSome holds then
                            1
                        else
                            bodies
                    Ends = bought
                    Trades = trades
                    Held = true
                }

        (ending
         |> List.sortBy priceOf
         |> List.map (fun (template, blocks, n, _) ->
             {
                 Template = template
                 Blocks = blocks
                 // A home of ours keeps the redundancy its row cast (ADR
                 // 0092): where one block loses, two bodies of the blocks
                 // that win, as one may land late or fall first.
                 Bodies =
                     if home && template = BodyTemplate.Melee && n = 1 && blocks > 1 then
                         bodies
                     else
                         n
                 Ends = true
                 Trades = trades
                 Held = stanceOf template = Stance.Hold
             }))
        @ [ held ]
    elif not (List.isEmpty trespassers) then
        let priced = killPricing view room
        let healOn = healReachingWithin priced room priced.Tuning.ReachHorizon

        let values = RoomWorth.values Quarry.Reading.Cast worth.Value
        let takes = RoomWorth.takes Quarry.Reading.Cast worth.Value

        let sets =
            templates
            |> List.collect (fun template ->
                let pattern = patternOfTemplate template

                blocksBought view template
                |> List.collect (fun blocks ->
                    let body = blocksFighter pattern blocks

                    let strikes (hostile: HostileInfo) =
                        match template with
                        | BodyTemplate.Melee ->
                            strikesAtStandoff view body hostile || camped priced body hostile
                        | BodyTemplate.Ranged ->
                            canStrike view (blocksOf pattern.Block blocks) hostile

                    [ 1..bodies ]
                    |> List.map (fun n ->
                        let team = List.replicate n body

                        // The bodies of it already on the kill, standing in
                        // the room or in an oven, are cast, as a raid's set
                        // prices them: no spawn and no energy for them, only
                        // the life the walk and the kills take. A standing one counts only while it outlives a
                        // fresh body's cast and walk; one standing elsewhere
                        // holds another job.
                        let inRoom (creep: CreepInfo) =
                            placementOf view creep.Name
                            |> Option.exists (fun at -> at.Room = room)

                        let cast =
                            min
                                n
                                (castAmong
                                    inRoom
                                    template
                                    blocks
                                    (castSpawnTicks template blocks + walk))

                        let fresh = n - cast

                        let spawning =
                            if fresh = 0 then
                                0
                            else
                                castSpawnTicks template blocks + waits template blocks fresh

                        let landing = spawning + walk

                        let kills =
                            trespassers
                            |> List.choose (fun hostile ->
                                if strikes hostile then
                                    killTicksOf priced healOn team hostile
                                    |> Option.filter (fun kill ->
                                        hostile.TicksToLive > landing + kill)
                                    |> Option.map (fun kill -> hostile, kill)
                                else
                                    None)

                        let killed =
                            kills |> List.map (fun (hostile, _) -> hostile.Id) |> Set.ofList

                        let landed =
                            killsLanded
                                priced
                                room
                                team
                                landing
                                (values |> Map.filter (fun id _ -> killed.Contains id))

                        // Its work goes on until the kill lands: every tick
                        // from the cast at what it takes a tick (`Takes`).
                        let meanwhile =
                            kills
                            |> List.sumBy (fun (hostile, kill) ->
                                (Map.tryFind hostile.Id takes |> Option.defaultValue 0.0)
                                * float (landing + kill))

                        // The set held the walk and every kill, one after
                        // another.
                        let held = walk + List.sumBy snd kills

                        let spent =
                            (if fresh = 0 then
                                 0.0
                             else
                                 castSpent
                                     view
                                     template
                                     blocks
                                     fresh
                                     (waits template blocks fresh + held))
                            + float cast * lifeSpent template blocks held

                        (-killed.Count,
                         -(landed - meanwhile - spent),
                         parts template blocks n,
                         n,
                         order template),
                        (template, blocks, n, killed.Count))))

        match sets with
        | [] ->
            [
                { quiet with
                    Template = List.head templates
                    Ends = false
                }
            ]
        | _ ->
            let template, blocks, n, killed = sets |> List.minBy fst |> snd

            [
                {
                    Template = template
                    Blocks = blocks
                    Bodies = n
                    Ends = killed > 0
                    Trades = killed > 0
                    Held = false
                }
            ]
    else
        [ quiet ]

/// Whether a body this walk (`walkToRaid`) off its raid is at the fight: a
/// kiter's dance about its range, three steps either way.
let internal atFight (walk: int) = walk <= Engine.rangedRange

/// Whether a body of ours wins the fight in a room alone or beside its side
/// there (#602): the exchange (`exchangeHeld`) by it and those of `beside` at the
/// fight (`atFight`), and of those holding the fight (the flag, beside its
/// walk's ticks there where its colony's Atlas prices one), every one that lands within
/// `soon` ticks, or (#609) by its walk's ticks while this body still stands
/// above its floor: a quarter of its hits, or the raid's damage over twice
/// `soon`, whichever is more — the slack for a landing a few ticks late.
/// Meanwhile the raid's whole damage strikes it, net of what it heals
/// itself at its own range with the parts it still has (`Fighter.atHits`,
/// its head lost first), and the exchange is priced on what is left of it
/// when the last of them lands. A holder this view places no
/// walk for counts only within `soon`, its straight tiles. One that lands
/// later strikes nothing beside it, whatever a verdict counted it for.
/// Before #609 a friend farther than `soon` was never counted, so a ranger
/// that outlasts a 3RA4H kiter by two hundred ticks ran from it, and the
/// pair never met. A partner counts only while it lives to land and then
/// through the side's kill (#615 U3).
let private winsBeside
    (view: ColonyView)
    (stance: Stance)
    (room: string)
    (soon: int)
    (holding: bool)
    (creep: CreepInfo)
    (beside: (CreepInfo * bool * int option) list)
    =
    let raid = raidersIn view room
    let inRoom = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    // The raid priced from this body's arrival (#651): its damage on us at
    // its weapons (`Weapons`), its heal against our clock (`Heals`).
    let horizon = arrivalOf view [ Fighter.ofCreep creep ] inRoom

    let prices =
        raidPrices view Reaction.strike horizon (fun _ -> Engine.meleeRange) inRoom

    let price = prices.Weapons
    let healed = prices.Heals

    // The hit back (`hitBack`) between this body alone and the raid, none
    // from or at it on a rampart of ours it stays on (`onRamparts`, #651).
    let backOf (me: Fighter) =
        hitBackUnder
            view
            Reaction.strike
            Stance.Standoff
            (fst (onRamparts view stance inRoom [ me ]))
            inRoom

    // What this body's own damage alone deals the raid a tick, its swings
    // struck back included (`struckBack`).
    let dealtAlone (me: Fighter) (back: HitBack) =
        let damage = me.DamageAt(rangeOn view inRoom me)
        damage + struckBack healed damage back.Struck back.Swingers

    // What its own swing takes back a tick, while the raiders striking it
    // back stand under its own damage (`takenWhile`, #651).
    let takenBy (me: Fighter) =
        let back = backOf me
        takenWhile healed (dealtAlone me back) (List.sum back.Taken) back.Strikers

    // What the raid takes off this body a tick, struck alone (#589's focus)
    // at their standoff, its own swing struck back, past what it heals
    // itself there.
    let struck (me: Fighter) =
        (raidPrices view Reaction.strike horizon (standoff view me) inRoom).Weapons.Damage
        + takenBy me
        - me.HealAt(rangeOn view inRoom me)

    // The rampart of ours this body stays on and what breaks it a tick
    // beside the raid's damage (`shieldOf`, #651).
    let shieldUnder (me: Fighter) =
        match rampartUnder view me with
        | Some(tile, hits) when staysOn view stance inRoom me tile ->
            shieldOf view horizon price [ tile, hits ]
        | _ -> 0, 0

    // The ticks the raid alone takes to kill this body, struck as `struck`
    // has it, the rampart it stays on broken first at the whole strike and
    // its dismantlers' and no heal of its own putting it back (#651). None
    // where it never does.
    let diesIn (me: Fighter) =
        let lost = struck me

        if lost <= 0 then
            None
        else
            let raw = lost + me.HealAt(rangeOn view inRoom me)
            let shield, broken = shieldUnder me
            Some(float shield / float (raw + broken) + float me.Hits / float lost)

    // A body not holding the fight is priced at the hits it keeps by its
    // next decision once in it (`soon` ticks struck alone): the holder lets
    // go only of a fight it loses, and one let go comes back only to a fight
    // it still fits once struck, so the answer does not flip on its wounds.
    // Struck by the raid's own weapons only: the band wears our body and
    // never the raid, and its own swing's hit back is paid only while that
    // swing lands, which the band does not price (#624: a 1-block guard
    // fresh to a 4-ATTACK raider was a loser it beats in three ticks).
    let start =
        if holding then
            creep.Hits.Hits
        else
            let me = Fighter.ofCreep creep
            freshHits view room soon creep.Hits.Hits (struck me - takenBy me)

    let floor = max (creep.Hits.HitsMax / 4) (price.Damage * 2 * soon)

    // What its own swing takes back a tick while it waits (`backOf`), and
    // the ticks the raiders striking it back stand under its own damage: the
    // hit back ends with them (#651).
    let back =
        lazy
            (let me = Fighter.ofCreep creep
             let back = backOf me
             let net = dealtAlone me back - healed.Healing

             List.sum back.Taken,
             if net <= 0 then
                 System.Int32.MaxValue
             else
                 (back.Strikers + net - 1) / net)

    // This body's hits after `ticks` of the raid alone, its swing struck
    // back while its strikers stand, the rampart it stands on broken first
    // (#651), and its heal worn with it, priced once a live part count.
    let after ticks =
        let taken, falls = back.Value
        let heals = System.Collections.Generic.Dictionary<int, int>()

        let healAt hits =
            let live = (hits + Engine.partHits - 1) / Engine.partHits

            match heals.TryGetValue live with
            | true, heal -> heal
            | _ ->
                let worn = Fighter.atHits creep hits
                let heal = worn.HealAt(Fighter.bestRange worn)
                heals.[live] <- heal
                heal

        let shield, broken = shieldUnder (Fighter.ofCreep creep)

        let rec go t shield hits =
            if t >= ticks || hits <= 0 then
                hits
            else
                let struck = price.Damage + (if t < falls then taken else 0)
                let through = max 0 (struck - shield)

                go
                    (t + 1)
                    (max 0 (shield - struck - broken))
                    (if through > 0 then
                         hits - max 0 (through - healAt hits)
                     else
                         hits)

        go 0 shield start

    let landing =
        beside
        |> List.choose (fun (other, holds, ticks) ->
            match walkToRaid view raid other, ticks with
            | Some walk, _ when atFight walk -> Some(other, 0)
            | _, Some ticks when
                holds && other.TicksToLive > ticks && (ticks <= soon || after ticks > floor)
                ->
                Some(other, ticks)
            | Some walk, None when holds && other.TicksToLive > walk && atFight (walk - soon) ->
                Some(other, walk - Engine.rangedRange)
            | _ -> None)

    // A partner counts only while it lives through the kill it is counted
    // for: the side's kill (`killTicks`), struck once the last of it lands
    // (the exchange prices the side whole from then), ends before it ages
    // out. One that ages out first deals damage the raid heals back
    // once it is gone, so the side is priced without it, and its kill again
    // (live t1,038,641: ranger-1037139, 33 ticks left, beside ranger-1037937
    // against Wraith7582's 1,248 hits, out-healed 48 a tick, a kill of ~39
    // ticks; it aged out with the Wraith at 512 and the Wraith healed back).
    let rec livesThrough (side: (CreepInfo * int) list) =
        let waits = side |> List.map snd |> List.fold max 0
        let me = Fighter.atHits creep (after waits)

        match killTicks view room (me :: List.map (fst >> Fighter.ofCreep) side) with
        | Some kill ->
            match side |> List.filter (fun (other, _) -> other.TicksToLive > waits + kill) with
            | kept when List.length kept = List.length side -> side
            | kept -> livesThrough kept
        | None -> side

    let counted =
        if List.isEmpty landing then
            landing
        else
            livesThrough landing

    let waits = counted |> List.map snd |> List.fold max 0

    let wins fighters = exchangeIn view room fighters |> won

    // Beside a side that wins without it, it fits only if it outlives the
    // fight: the raid may strike it alone until the side's damage kills the
    // raid (`killTicks`). One the raid falls on sooner adds nothing the side
    // needs and is fed to it, whatever the side's hits summed say; one the
    // side needs fights on.
    let outlives (me: Fighter) (side: Fighter list) =
        match diesIn me with
        | None -> true
        | Some dies ->
            killTicks view room (me :: side)
            |> Option.exists (fun ticks -> dies > float ticks)

    let alone = Fighter.atHits creep start

    // A body lost anyway spends itself (#615 U3): what leaving saves is the
    // life it would keep past the raid's kill of it, at its live parts'
    // cost (none for one that ages out first, or before it walks home);
    // what staying buys is the raid it takes off meanwhile that a kill
    // keeps: its net damage alone at their standoff, its hit back
    // included, at the raid's cost a hit, where it kills the raid itself
    // while it stands (`killTicks`) or the side finishes it (`finish`),
    // less what the raid heals back before they land. Damage no kill keeps
    // is healed back and buys nothing. A body whose life is nearly spent
    // fights on while it still deals damage a kill keeps.
    let spends () =
        let walk = walkToRaid view raid creep |> Option.defaultValue 0
        let left = creep.TicksToLive - walk

        let dies =
            match diesIn alone with
            | None -> System.Int32.MaxValue
            | Some dies -> int (min dies (float System.Int32.MaxValue))

        let fought = max 0 (min left dies)

        let finishers =
            beside
            |> List.choose (fun (other, holds, ticks) ->
                match walkToRaid view raid other, ticks with
                | Some walk, _ when atFight walk -> Some(other, 0)
                | _, Some ticks when holds && other.TicksToLive > ticks -> Some(other, ticks)
                | _ -> None)

        let dealt =
            int64 (max 0 (dealtAlone alone (backOf alone) - healed.Healing)) * int64 fought

        // The raid as this body leaves it: `kept` of its damage off the armed
        // raiders, the least to kill first, as a focus takes them.
        let wornBy (kept: int64) =
            let armed =
                inRoom
                |> List.filter (HostileInfo.armed view.Hostiles)
                |> List.sortBy (fun h -> HostileInfo.effectiveHits h, h.Id)

            let rec wear left (raiders: HostileInfo list) =
                match raiders with
                | [] -> Map.empty
                | h :: rest ->
                    let whole = int64 (HostileInfo.effectiveHits h)

                    if left >= whole then
                        Map.add h.Id None (wear (left - whole) rest)
                    elif left > 0L then
                        Map.add h.Id (Some(h.Hits - HostileInfo.damageTaken h (int left))) Map.empty
                    else
                        Map.empty

            let worn = wear kept armed

            { view with
                Hostiles =
                    view.Hostiles
                    |> List.choose (fun h ->
                        match Map.tryFind h.Id worn with
                        | None -> Some h
                        | Some None -> None
                        | Some(Some hits) -> Some { h with Hits = hits })
            }

        // When the raid is finished once this body falls: by our towers
        // there alone, at once; else by the finishers, as the first lands,
        // where they win their exchange against what is left of the raid:
        // its hits less what this body keeps of its damage, healed back
        // meanwhile until they land. One the raid kills first finishes
        // nothing.
        let finish =
            let side = finishers |> List.map (fst >> Fighter.ofCreep)

            if killTicks view room [] |> Option.isSome then
                Some 0
            elif List.isEmpty side then
                None
            else
                let lands = finishers |> List.map snd |> List.min
                let kept = max 0L (dealt - int64 healed.Healing * int64 (max 0 (lands - fought)))

                if exchangeIn (wornBy kept) room side |> won then
                    Some lands
                else
                    None

        let net =
            if killTicks view room [ alone ] |> Option.exists (fun ticks -> ticks <= fought) then
                dealt
            else
                match finish with
                | Some lands ->
                    let gap = max 0 (lands - fought)
                    max 0L (dealt - int64 healed.Healing * int64 gap)
                | None -> 0L

        let raidValue = raid |> List.sumBy (fun h -> bodyCost h.Body) |> int64
        let raidHits = int64 (healed.Shield + healed.Tough + healed.Hits)

        let mine =
            creep.Body
            |> Map.toList
            |> List.collect (fun (part, count) -> List.replicate count part)
            |> bodyCost
            |> int64

        let gain =
            if raidHits <= 0L then
                0L
            else
                min raidValue (net * raidValue / raidHits)

        gain > mine * int64 (max 0 (left - dies)) / int64 Engine.creepLifetime

    // A post it holds (`Stance.Hold`, #531) is held while it stands: the raid
    // must come to it to hurt our work, and one keeping off takes nothing
    // off it its heal does not put back.
    stance = Stance.Hold && struck alone <= 0
    // Alone first: a partner the raid strikes can turn a kite into a trade.
    || wins [ alone ]
    || not (List.isEmpty counted)
       && (let me = Fighter.atHits creep (after waits)
           let side = List.map (fst >> Fighter.ofCreep) counted
           wins (me :: side) && (outlives me side || not (wins side)))
    || spends ()

/// Whether a body fits a room's fight (#615 U3): the one answer taking a
/// Guard and keeping it read, for every body, whatever its row or the room's
/// kind. A room no raider stands in has no fight to lose. Else the body ends
/// the raid at the standoff beside its side (`winsBeside`): those at the
/// fight and those holding it or arriving at it in time, trading or kiting
/// (`won`); or, holding a post (`Stance.Hold`, #531), it stands there
/// whatever keeps off it; or, its life nearly spent, it buys more of the
/// raid than leaving saves of it. The room's verdict is what a cast would
/// come to, and loses nothing a body standing by wins. One that does not
/// fit is let go, and never walks in; one not `holding` the fight is
/// priced once struck to its next decision.
let fits
    (view: ColonyView)
    (stance: Stance)
    (room: string)
    (soon: int)
    (holding: bool)
    (creep: CreepInfo)
    (beside: (CreepInfo * bool * int option) list)
    : bool =
    List.isEmpty (raidersIn view room)
    || winsBeside view stance room soon holding creep beside

/// A body in an oven of ours as it will stand once born: its parts at full
/// hits and a whole life.
let internal ovenBody (cast: CastingInfo) : CreepInfo =
    let hits = Engine.partHits * List.length cast.Body

    {
        Name = cast.Name
        TicksToLive = Engine.creepLifetime
        Fatigue = 0
        Hits = { Hits = hits; HitsMax = hits }
        Energy = 0
        Thorium = 0
        Minerals = Map.empty
        FreeCapacity = 0
        Body = partsOf cast.Body
        ShotOff = 0
        Walking = false
        DrawnFrom = None
    }

/// The fresh bodies a cast is, at full hits and life, named for its
/// template's row (`Bodies`' patterns, laid as the row casts them).
let private castBodies (cast: FighterCast) : CreepInfo list =
    let pattern = patternOfTemplate cast.Template

    let body =
        pattern.Block
        |> List.countBy id
        |> List.map (fun (part, count) -> part, count * cast.Blocks)
        |> Map.ofList

    let hits = Engine.partHits * cast.Blocks * List.length pattern.Block

    [
        for i in 1 .. cast.Bodies ->
            {
                Name = $"{pattern.Name}-cast-{i}"
                TicksToLive = Engine.creepLifetime
                Fatigue = 0
                Hits = { Hits = hits; HitsMax = hits }
                Energy = 0
                Thorium = 0
                Minerals = Map.empty
                FreeCapacity = 0
                Body = body
                ShotOff = 0
                Walking = false
                DrawnFrom = None
            }
    ]

/// ADR-0092
/// The cheapest set of bodies that ends the fight in a room (#654), of the
/// `templates` the colony's bank buys at its capacity (`castsByPrice`), at
/// most `bodies` bodies: the first set ending it whose every body the gate
/// seats fresh beside the rest of its set (`fits`, at the stance it prices
/// it at) — the row and the gate price alike (#647) —, else the answer
/// where none does, which ends nothing.
let private fighterCastOf
    (view: ColonyView, room: string, templates: BodyTemplate list, bodies: int)
    : FighterCast =
    let casts = castsByPrice view room templates bodies

    let seated (cast: FighterCast) =
        let stance = if cast.Held then Stance.Hold else Stance.Standoff
        let set = castBodies cast

        set
        |> List.forall (fun body ->
            fits
                view
                stance
                room
                view.Tuning.ReachHorizon
                false
                body
                (set
                 |> List.filter (fun other -> other.Name <> body.Name)
                 |> List.map (fun other -> other, true, Some 0)))

    // The last is cast unseated, as before (#589's held guard): it ends
    // nothing, but at a home of ours, where the melee pair it may be ends
    // the raid (the home exception), and the home is fought for whatever the gate.
    let last = List.last casts

    casts
    |> List.tryFind (fun cast -> cast.Ends && seated cast)
    |> Option.defaultValue (
        if last.Ends && (homesOfOurs view).Contains room then
            last
        else
            { last with Ends = false }
    )

let private fighterCastMemo
    : TickMemo.Memo<ColonyView * string * BodyTemplate list * int, FighterCast> =
    TickMemo.create 32 (fun (view, room, templates, bodies) (view', room', templates', bodies') ->
        obj.ReferenceEquals(view, view')
        && room = room'
        && templates = templates'
        && bodies = bodies')

/// `fighterCastOf`, once a tick per view, room, templates and bodies: one
/// view asks it of a room from its ask, its camp and its verdict alike.
let fighterCast
    (view: ColonyView)
    (room: string)
    (templates: BodyTemplate list)
    (bodies: int)
    : FighterCast =
    TickMemo.recall fighterCastMemo (view, room, templates, bodies) fighterCastOf

/// The bodies a cast is, as the pricing reads them (`blocksFighter`).
let internal castFighters (cast: FighterCast) : Fighter list =
    List.replicate cast.Bodies (blocksFighter (patternOfTemplate cast.Template) cast.Blocks)

/// What the chooser's cast for a room earns there a tick (`fighterCast`,
/// #663 M4), read as a cast (`Quarry.Reading.Cast`), landed after its spawns
/// (`spawnRounds`) and its slowest body's walk from the colony's home;
/// nothing for a cast that ends no fight there or casts no body.
let castRateIn (view: ColonyView) (room: string) : float =
    let cast = fighterCast view room bodyTemplates Engine.guardCap
    let team = castFighters cast
    let home = RoomPos.at (SpatialInfo.homeName view.Spatial) { X = 25; Y = 25 }

    match team with
    | _ when not cast.Ends -> 0.0
    | [] -> 0.0
    | team ->
        match doorTicks home (team |> List.map paceOf |> List.max) room with
        | System.Int32.MaxValue -> 0.0
        | walk ->
            roomRate
                Quarry.Reading.Cast
                view
                team
                0
                (spawnRounds view cast.Template cast.Blocks cast.Bodies + walk)
                room

/// What a group standing where it stands earns in a room a tick
/// (`roomRate`, each body at its odds), its arrival its slowest body's walk
/// (`doorTicks`); nothing off the grid.
let private groupRateIn
    (view: ColonyView)
    (placed: (RoomPos * Fighter) list)
    (dismantle: int)
    (room: string)
    : float =
    match placed |> List.map (fun (at, fighter) -> doorTicks at (paceOf fighter) room) with
    | [] -> 0.0
    | arrivals ->
        match List.max arrivals with
        | System.Int32.MaxValue -> 0.0
        | arrival -> roomRate Quarry.Reading.Value view (List.map snd placed) dismantle arrival room

/// Each candidate the views work with the rate its group earns there
/// (`sweepChoice`), in the order the views name them.
let private sweepRates (held: Assignments) (views: ColonyView list) : (string * float) list =
    match views |> List.filter (fun view -> not (List.isEmpty view.Harass)) with
    | [] -> []
    | working ->
        let group = sweepBodies held working
        let placed = group |> List.map (fun (creep, at) -> at, Fighter.ofCreep creep)
        let dismantle = dismantleOf (List.map fst group)

        let rateIn (room: string) =
            let workers =
                working
                |> List.filter (fun view -> view.Harass |> List.exists (fun h -> h.RoomName = room))

            match placed with
            | [] -> workers |> List.map (fun view -> castRateIn view room) |> List.max
            | placed ->
                groupRateIn
                    (worthOwner views room |> Option.defaultValue (List.head workers))
                    placed
                    dismantle
                    room

        working
        |> List.collect (fun view -> view.Harass |> List.map _.RoomName)
        |> List.distinct
        |> List.map (fun room -> room, rateIn room)

/// ADR-0093
/// The [[sweep]]'s room (#658), one for every colony: of the candidates the
/// views work (`ColonyView.Harass`), the one its group's effort earns the
/// most in a tick (`roomRate`, priced once per room off its worth's owner,
/// `worthOwner`, the room's worth the world's), then by name; None where none
/// pays. The group is every colony's (`sweepGroup`), its arrival its slowest
/// body's walk to the room (`doorTicks`: none in it or on its doorstep);
/// with no group, each colony's cast for the room as the chooser prices it
/// (`fighterCast`, #663 M4), landed after its spawns (`spawnRounds`) and its
/// slowest body's walk from that home, the best colony's price the room's,
/// read as a cast (`Quarry.Reading.Cast`: no spawn for a body a look is
/// about to confirm or not); a group standing reads each body at its odds.
/// A room no cast of a colony ends the fight in is none of its: a room
/// chosen for a body no cast buys would hold the group for ever. An escort
/// the group loses is no room for it, whatever a bigger cast would win: the
/// cast is the fight's to price. The room the group stands in costs no walk, so
/// it keeps the group until what it earns there runs out or another room's
/// net of the walk pays more: no stored room, no hysteresis band.
let private sweepPick (rates: (string * float) list) : (string * float) option =
    rates
    |> List.filter (fun (_, rate) -> rate > 0.0)
    |> List.sortBy (fun (room, rate) -> -rate, room)
    |> List.tryHead

/// `sweepPick`'s room.
let sweepChoice (held: Assignments) (views: ColonyView list) : string option =
    sweepPick (sweepRates held views) |> Option.map fst

/// `sweepChoice` over one colony's view alone.
let sweepRoom (view: ColonyView) (held: Assignments) : string option = sweepChoice held [ view ]

/// The [[sweep]] laid in every colony's view (#658), before the world's
/// verdicts are priced (`Entry.judgeColonies`): each view that works the
/// sweep's one room (`sweepChoice`) keeps it in `Harass`, and no other
/// candidate; the rest stay cast (`HarassCast`: projected, their stand-down
/// read) but pool nothing. Every reader past this one — the verdicts, the
/// Guard, its kill and its cast, the world's fight — reads the one room.
/// The dismantler's targets (`Dismantles`) are narrowed to the sweep's room
/// and the salvage rooms. `held` is last tick's Tasks, whose
/// holders are the group. Every room's worth is laid first, one table for
/// every view (`WorldView.Worths`, `worthTable`): the choice and every reader
/// after it price a room once.
let sweepAll (held: Assignments) (views: ColonyView list) : ColonyView list =
    // The table prices each room off its owner's view as laid, the one the
    // choice reads too: one pricing a candidate.
    let laid = ref views
    let worths = Some(worthTable views (fun () -> laid.Value))

    let views =
        views
        |> List.map (fun view ->
            { view with
                World = { view.World with Worths = worths }
            })

    laid.Value <- views

    let rates = sweepRates held views
    let pick = sweepPick rates
    let room = pick |> Option.map fst
    let rate = pick |> Option.map snd |> Option.defaultValue 0.0
    let group = lazy (sweepBodies held views)

    views
    |> List.map (fun view ->
        match view.Harass with
        | [] -> view
        | candidates ->
            // The dismantler's targets follow the sweep's room: an enemy
            // container in a candidate it does not work is nobody's target
            // this tick; a salvage room's, no candidate, stays.
            let elsewhere =
                candidates
                |> List.map _.RoomName
                |> List.filter (fun candidate -> Some candidate <> room)
                |> Set.ofList

            { view with
                Harass = candidates |> List.filter (fun h -> Some h.RoomName = room)
                Dismantles =
                    view.Dismantles
                    |> List.filter (fun id ->
                        SpatialInfo.placementOf view.Spatial id
                        |> Option.forall (fun at -> not (elsewhere.Contains at.Room)))
                World =
                    { view.World with
                        SweepGroup = group.Value
                        SweepRate = rate
                        SweepRates = rates
                    }
            })

/// `sweepAll` over one colony's view alone: the shape a decide of one
/// colony asks in.
let sweep (held: Assignments) (view: ColonyView) : ColonyView = sweepAll held [ view ] |> List.head

/// How a body of ours ends the raid standing in a room alone (#596): its
/// walk there (`walkToRaid`) and then its kill (`killTicks`), the kill
/// `Engine.creepLifetime` where it never kills the raid alone or ages out
/// first. None where no raider stands there, or nothing places the body.
let endsIn (view: ColonyView) (room: string) (creep: CreepInfo) : (int * int) option =
    match raidersIn view room with
    | [] -> None
    | raid ->
        walkToRaid view raid creep
        |> Option.map (fun walk ->
            walk,
            killTicks view room [ Fighter.ofCreep creep ]
            |> Option.filter (fun kill -> walk + kill <= creep.TicksToLive)
            |> Option.defaultValue Engine.creepLifetime)

/// How many bodies a kite in a room seats (#596): of the
/// fleet's rangers standing in it or beside it that kite its raid down
/// (`kitedDown`), soonest done first (`endsIn`), the fewest that kill it
/// before its first raider ages out — every body's damage from its walk's
/// end while it lives, beside our towers, against the raid's hits and heal
/// — or all of them where none so does; and beside them every one already
/// at the fight (`atFight`), whose shots end it sooner at no walk's cost,
/// and every one `holding` the kite within twice that: a seat taken at the
/// fight stays while its holder dances out past it. A kited raid strikes no kiter, so
/// none falls and no focus is priced.
let kiteSeats (view: ColonyView) (room: string) (holding: Set<string>) : int =
    let inRoom = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    match raidersIn view room with
    | [] -> 1
    | raid ->
        // Kiters on their way: every heal that reaches the raid in the
        // answering window counts (#651).
        let price =
            (raidPrices view Reaction.strike Reaction.strike (fun _ -> Engine.meleeRange) inRoom)
                .Heals

        let towers = ourTowersOn view room inRoom
        let heal = max price.Healing price.ToughHealing
        let deadline = raid |> List.map _.TicksToLive |> List.min
        let near = Set.ofList (room :: RoomName.adjacent room)

        let kiters =
            view.World.Bodies.Value
            |> Map.toList
            |> List.filter (fst >> near.Contains)
            |> List.collect snd
            |> List.map fst
            |> List.filter (fun creep ->
                isRangerRowBody creep && kitedDown view (Fighter.ofCreep creep) room)
            |> List.sortBy (fun creep ->
                endsIn view room creep
                |> Option.map (fun (walk, kill) -> walk + kill)
                |> Option.defaultValue System.Int32.MaxValue,
                creep.Name)
            |> List.map (fun creep ->
                let walk = walkToRaid view raid creep |> Option.defaultValue deadline

                (atFight walk
                 || Set.contains creep.Name holding && atFight (walk - Engine.rangedRange)),
                (walk, creep.TicksToLive, dealtOn view inRoom (Fighter.ofCreep creep)))

        // The tick these bodies end the raid on, its ramparts broken first,
        // no heal putting them back (#651).
        let endOf chosen =
            let rec go tick shield left =
                if tick >= deadline then
                    deadline
                else
                    let dealt =
                        towers
                        + List.sumBy
                            (fun (walk, life, damage) ->
                                if walk <= tick && tick < life then damage else 0)
                            chosen

                    let through = max 0 (dealt - shield)

                    let left = if through > 0 then left - max 0 (through - heal) else left

                    if left <= 0 then
                        tick
                    else
                        go (tick + 1) (max 0 (shield - dealt)) left

            go 0 price.Shield (price.Tough + price.Hits)

        let enough =
            seq { 1 .. List.length kiters }
            |> Seq.tryFind (fun n -> endOf (kiters |> List.truncate n |> List.map snd) < deadline)
            |> Option.defaultValue (List.length kiters)

        let standing = kiters |> List.skip enough |> List.filter fst |> List.length
        max 1 (enough + standing)

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

/// HomeHold: whether a garrison body may stand away from its homes for a
/// fight in `kill` (#510, #516, #531), over the fleet's bodies left. A
/// colony's resident rooms are its garrisoned homes and its
/// ranger-row bodies their garrison, nothing a ranger carries saying which
/// home it holds. Each garrisoned home beside the kill is quiet (no
/// disturber in it or bouncing into it) and holds without it: its loaded
/// towers, standing at their hits, and every body of the fleet standing
/// there that stays — whoever's, less the ones `leaving` names for that
/// home — beat every raider but the kill's own that reaches its middle
/// inside a lend's window (`arrivesWithin`), priced together at that
/// window. And one standing in a garrisoned home leaves only for a room
/// beside it, which is all the window prices. Any other body leaves no
/// home.
let homeHoldReleases
    (view: ColonyView)
    (kill: string)
    (standing: string option)
    (leaving: CreepInfo -> string -> bool)
    (maps: string -> ArrivalMaps option)
    (creep: CreepInfo)
    : bool =
    not (isRangerRowBody creep)
    || (let resident = residentRooms view

        let staying (home: string) =
            let fleet =
                Map.tryFind home view.World.Bodies.Value
                |> Option.defaultValue []
                |> List.map fst

            let own =
                ColonyView.bodies view
                |> List.filter (fun body ->
                    SpatialInfo.creepPlacementOf view.Spatial body.Name
                    |> Option.exists (fun at -> at.Room = home))

            own @ fleet
            |> List.distinctBy (fun body -> body.Name)
            |> List.filter (fun body -> body.Name <> creep.Name && not (leaving body home))
            |> List.map Fighter.ofCreep

        let holds (home: string) =
            let quiet = bouncingInto view home |> List.forall (disturbs view resident >> not)

            // And every member of a class whose arrival map reaches the
            // home's middle inside the window, where the home's maps are laid.
            let mapped =
                match maps home with
                | None -> Set.empty
                | Some maps ->
                    maps.Classes
                    |> List.filter (fun map ->
                        Arrival.ticksAt map.Ticks { X = homeMiddle; Y = homeMiddle }
                        |> Option.exists (fun ticks -> ticks <= Reaction.lend))
                    |> List.collect (fun map -> map.Members)
                    |> Set.ofList

            let arriving =
                view.Hostiles
                |> List.filter (fun h ->
                    h.Pos.Room <> kill
                    && h.Pos.Room <> home
                    && (arrivesWithin view home h || Set.contains h.Id mapped))

            quiet
            && (List.isEmpty arriving
                || exchangeAgainst
                    view
                    Reaction.lend
                    Reaction.lend
                    arriving
                    Stance.Standoff
                    (ourTowersOn view home arriving)
                    (List.length (ourTowersIn view home) * Engine.towerHits)
                    (staying home)
                   |> won)

        resident
        |> Set.forall (fun home -> not (RoomName.neighbouring home kill) || holds home)
        && standing
           |> Option.forall (fun here ->
               not (Set.contains here resident)
               || here = kill
               || RoomName.neighbouring here kill))

/// Whether a body standing in a room may leave it for a fight in `kill`:
/// HomeHold, the body itself the one leaving (`homeHoldReleases`; the
/// FightMatch counts the rest it matches out).
let internal mayLeaveFor
    (view: ColonyView)
    (kill: string)
    (standing: string option)
    (creep: CreepInfo)
    =
    homeHoldReleases view kill standing (fun _ _ -> false) (fun _ -> None) creep

/// Whether the holder of a room's Guard may be called off it for a kill in
/// another (#510, #516), the bodies the kill admits being the gate's to say.
/// A resident only as it may leave (`mayLeaveFor`); any other holder
/// while nothing its Guard shoots stands in its room, and never off another
/// kill.
let internal mayBeCalled
    (view: ColonyView)
    (kills: Set<string>)
    (held: string)
    (kill: string)
    (creep: CreepInfo)
    =
    let resident = residentRooms view

    if Set.contains held resident then
        mayLeaveFor view kill (Some held) creep
    else
        not (Set.contains held kills)
        && view.Hostiles
           |> List.forall (fun h -> h.Pos.Room <> held || not (disturbs view resident h))

/// Whether a ranger this colony's bank buys kites the raid in a room down
/// alone (#510, #663 M4): some one ranged body of the block counts the
/// chooser weighs (`blocksBought`), the biggest asked first, that kites it
/// (`kitedDown`), one exchange deciding both the end and the kite. A
/// verdict's probe, never a seat's: no gate (`fits`) is asked, so a full
/// seat fails no verdict. None where the bank buys no ranger.
let private castKites (view: ColonyView) (room: string) : bool =
    blocksBought view BodyTemplate.Ranged
    |> List.rev
    |> List.exists (fun blocks -> kitedDown view (blocksFighter rangerPattern blocks) room)

/// A room's own row and its answer, the one reading `rangerAnswers` and the
/// verdict share. A ranger room and one the colony harasses are the ranger's
/// (#411), won where some ranger the bank buys trades its raid down (the
/// chooser's `Trades`, `fighterCast` of one ranged body, #663 M4), and where
/// no raider stands there nor rival towers fire. Any other is the template of its fight's cheapest cast
/// (`fighterCast`, #654) where that cast ends the raid at the standoff and
/// some set the bank buys trades it down (`Trades`): a raid the rangers only
/// kite down is the verdict's Kite, answered after the rows (`priceAmong`).
/// Else the guard's, which holds the post (`Stance.Hold`, #531) and wins
/// while it outlasts the raid: no new stand-down for a raid of quick
/// shooters no ranger wins either. Never a win in a child's outpost, which
/// no row of this colony casts for (#517).
let rowAnswer (view: ColonyView) (room: string) : RowAnswer =
    let rangerWins () =
        if List.isEmpty (fightRaid view room) then
            not (underRivalTowers view room)
        else
            (fighterCast view room [ BodyTemplate.Ranged ] 1).Trades

    let cast = lazy (fighterCast view room bodyTemplates Engine.guardCap)

    if List.contains room view.Borrowed.ChildOutposts then
        { Ranger = false; Wins = false }
    elif Set.contains room (rangerRooms view) || Set.contains room view.HarassCast then
        { Ranger = true; Wins = rangerWins () }
    elif List.isEmpty (fightRaid view room) then
        { Ranger = false; Wins = true }
    elif not cast.Value.Held && cast.Value.Ends && cast.Value.Trades then
        {
            Ranger = cast.Value.Template = BodyTemplate.Ranged
            Wins = true
        }
    else
        {
            Ranger = false
            Wins = blocksBeat guardPattern Stance.Hold view room (guardBlocksReach view)
        }

/// Whether a room's own Guard is the ranger row's (`rowAnswer`).
let rangerAnswers (view: ColonyView) (room: string) : bool = (rowAnswer view room).Ranger

/// One colony's rangers that may answer the raid in `room`, each with the
/// room it stands in (`placed`, the world's fleet): its own standing in the
/// room, the garrison among them, and its own standing beside it, let go by
/// HomeHold (`mayLeaveFor`) out of a room where nothing its Guard
/// would shoot stands; each a body that strikes every raider there and that
/// its colony sends this tick (`sends`). Asked in the lender's own view with
/// the world's vision of the room laid in, so its quiet is read on its side.
let private lendableTo
    (view: ColonyView)
    (placed: string -> string option)
    (sends: CreepInfo -> bool)
    (room: string)
    : (CreepInfo * string) list =
    let resident = residentRooms view

    let quiet (here: string) =
        view.Hostiles
        |> List.forall (fun h -> h.Pos.Room <> here || not (disturbs view resident h))

    ColonyView.bodies view
    |> List.choose (fun creep -> placed creep.Name |> Option.map (fun here -> creep, here))
    |> List.filter (fun (creep, here) ->
        (here = room || RoomName.neighbouring here room)
        && isRangerRowBody creep
        && sends creep
        && strikesEvery view (partCount creep.Body) (HostileInfo.raider view.Hostiles) room)
    |> List.filter (fun (creep, here) ->
        here = room || quiet here && mayLeaveFor view room (Some here) creep)

/// Whether every body of a set lives through the set's kill of the raid in
/// `room` (`killTicks`), as a partner the gate counts does (`winsBeside`): a
/// body that ages out first answers nothing.
let private livesThrough (view: ColonyView) (room: string) (taken: CreepInfo list) =
    killTicks view room (List.map Fighter.ofCreep taken)
    |> Option.forall (fun kill -> taken |> List.forall (fun creep -> creep.TicksToLive > kill))

/// The bodies of ours among `creeps` whose fight with the raid in `room`
/// converges (#673): the longest-lived of them that win it together
/// (`bodiesWin`) and live through their kill (`livesThrough`), its net
/// damage past the raid's heal over the ticks they have left covering its
/// hits; the shortest-lived let go one at a time until the rest do, none
/// where no such set is left. The rest are no answer to the raid, whatever
/// they stand on, and a fight's ask counts them as none.
let converging (view: ColonyView) (room: string) (creeps: CreepInfo list) : CreepInfo list =
    let rec trim (taken: CreepInfo list) =
        match taken with
        | [] -> []
        | _ when bodiesWin view room taken && livesThrough view room taken -> taken
        | _ :: longer -> trim longer

    creeps |> List.sortBy (fun creep -> creep.TicksToLive, creep.Name) |> trim

/// The bodies of a cast's row a room's raid wants counted against its ask
/// (#673), its `holders` of the row standing by: the cast's own where the
/// holders converge alone (`converging`) or nothing does; else every holder
/// and the fewest fresh bodies of the cast that converge beside them, a
/// holder that ages out first counted as none. A swing holding its post
/// (`FighterCast.Held`, #531) converges on nothing, and wants the cast
/// alone.
let internal castConverging
    (view: ColonyView)
    (room: string)
    (cast: FighterCast)
    (holders: CreepInfo list)
    : int =
    if cast.Held || List.isEmpty holders then
        cast.Bodies
    else
        let fresh = castBodies { cast with Bodies = Engine.guardCap }

        match
            [ 0 .. cast.Bodies ]
            |> List.tryFind (fun k ->
                not (List.isEmpty (converging view room (holders @ List.truncate k fresh))))
        with
        | Some 0 -> cast.Bodies
        | Some joined -> max cast.Bodies (List.length holders + joined)
        | None -> cast.Bodies

/// The fewest rangers that win the raid in `room` together, among every
/// lender's (`lendableTo`), each by name with its colony's home; None where
/// all of them lose. Taken standing in the room first, then beside it, by
/// name within each, and priced in `pricing`, a view that sees the raid. One
/// count over the fleet's bodies HomeHold lets go, so a raid only two
/// colonies' rangers win together is won (#518). Each lives through the
/// set's kill (`livesThrough`): a ranger that ages out first answers
/// nothing, so a fight it holds is no chase.
let private rangersAmong
    (pricing: Lazy<ColonyView>)
    (placed: string -> string option)
    (lenders: (string * ColonyView * (CreepInfo -> bool)) list)
    (room: string)
    : (string * string) list option =
    let rec wins taken named rest =
        match rest with
        | [] -> None
        | ((creep: CreepInfo), (home: string)) :: rest ->
            let taken = creep :: taken
            let named = (creep.Name, home) :: named

            if bodiesWin pricing.Value room taken && livesThrough pricing.Value room taken then
                Some(List.rev named)
            else
                wins taken named rest

    lenders
    |> List.collect (fun (home, view, sends) ->
        lendableTo view placed sends room
        |> List.map (fun (creep, here) -> (here <> room, creep.Name), (creep, home)))
    |> List.sortBy fst
    |> List.map snd
    |> wins [] []

/// One colony as the world's verdict prices a room: its home; its view, as
/// cut, which the world's vision of the room is laid into wherever it prices
/// (`priceAmong`); whether it may cast for a room (`castsInto`, the
/// Caster's eligibility); and which of its bodies it sends to a room this
/// tick.
type private Pricer =
    {
        Home: string
        View: ColonyView
        Casts: string -> bool
        Sends: string -> CreepInfo -> bool
        /// Whether a body of its already holds a room's Guard.
        Holds: string -> bool
    }

/// What one colony's cast of a fighter's template for a room comes to, as
/// the Caster weighs it (`Caster.assign`, #589): whether the cheapest set of
/// the template its bank buys ends the fight there (`fighterCast`, #654), a
/// template whose block the bank buys not winning anything, and one body's
/// spawn time at that set's blocks, at most the biggest the bank buys.
/// Priced in the colony's view with the world's vision of the room and the
/// rooms beside it laid in. Any other row wins, and spawns in no time: it is
/// never weighed between colonies.
let castPrice (view: ColonyView) (row: CastRow) (room: string) : bool * int =
    match row with
    | CastRow.Fighter template ->
        let priced = ColonyView.lookingInto (room :: RoomName.adjacent room) view
        let pattern = patternOfTemplate template
        let cast = fighterCast priced room [ template ] Engine.guardCap

        let most =
            max 1 (min (blocksMostOf template) (priced.Bank.Capacity / bodyCost pattern.Block))

        castable priced pattern && cast.Ends, castSpawnTicks template (min most cast.Blocks)
    | CastRow.Defender
    | CastRow.Squad _ -> true, 0

/// One room's verdict over these colonies, each reading the
/// world's vision of the room and the rooms beside it laid into its view, and
/// our bodies where the world's fleet places them (`placed`), so no colony's
/// cut decides it. A colony takes part where it may cast for the room or a
/// fighter of its stands in it or beside it, and sees the raid there. The room's
/// own answer is its casters': of the colonies that may cast for it
/// (`castsInto`, the Caster's eligibility), or, where none may and it is no
/// home of ours, of every colony taking part, the biggest row cast first,
/// then the biggest ranger kiting it. Then the rangers standing in it or lent
/// to it, of every colony taking part, together (`rangersAmong`). A room no
/// colony sees a raider in is a Win every row gives.
let private priceAmong (placed: string -> string option) (pricers: Pricer list) (room: string) =
    let beside = room :: RoomName.adjacent room
    let near = Set.ofList beside

    // Each colony's view with the world's vision of the room and its
    // neighbours laid in.
    let around =
        pricers
        |> List.map (fun p -> p.Home, lazy (ColonyView.lookingInto beside p.View))
        |> Map.ofList

    let aroundOf (p: Pricer) = around.[p.Home].Value

    // Each colony's fighters standing in the room or beside it, each with
    // the room it stands in.
    let fighters (p: Pricer) =
        ColonyView.bodies p.View
        |> List.filter (fun creep -> isGuardRowBody creep || isRangerRowBody creep)
        |> List.choose (fun creep ->
            placed creep.Name
            |> Option.filter near.Contains
            |> Option.map (fun here -> creep, here))

    let present (p: Pricer) =
        p.Casts room || not (List.isEmpty (fighters p))

    // A fighter of this colony that may take the room's raid now: one it
    // sends, lent by its colony's rule, that strikes every raider; and
    // whether it wins alone, or kites it down.
    let fit (p: Pricer) (wins: ColonyView -> CreepInfo -> bool) =
        let priced = aroundOf p

        fighters p
        |> List.exists (fun (creep, here) ->
            p.Sends room creep
            && mayLeaveFor priced room (Some here) creep
            && strikesEvery priced (partCount creep.Body) (HostileInfo.raider priced.Hostiles) room
            && wins priced creep)

    let kites (p: Pricer) =
        fit p (fun priced creep -> kitedDown priced (Fighter.ofCreep creep) room)

    // Whether a fighter of this colony takes the raid now — it ends it alone,
    // trading or kiting (`bodiesWin`), whatever its row (#615 U3) —, or
    // already holds the room's Guard: the verdict starts a fight and does
    // not end it on a tick a body's live parts dip under the price.
    let takes (p: Pricer) =
        p.Holds room || fit p (fun priced creep -> bodiesWin priced room [ creep ])

    let seeing = pricers |> List.filter (fun p -> present p && raided (aroundOf p) room)

    let rangers =
        lazy
            (match seeing with
             | [] -> None
             | first :: _ ->
                 rangersAmong
                     (lazy (aroundOf first))
                     placed
                     (seeing |> List.map (fun p -> p.Home, aroundOf p, p.Sends room))
                     room)

    let verdict answer rows own bodies =
        {
            Answer = answer
            Rows = rows
            Own = own
            Casting = Set.empty
            Rangers = lazy (Option.map List.length rangers.Value)
            Bodies = bodies
        }

    match seeing with
    | [] -> verdict Answer.Win Map.empty (pricers |> List.map (fun p -> p.Home) |> Set.ofList) []
    | first :: _ ->
        let casters =
            match seeing |> List.filter (fun p -> p.Casts room) with
            | [] when Map.containsKey room first.View.Stages -> []
            | [] -> seeing
            | owners ->
                // A home another colony raises or garrisons is its
                // residents' (`residentRooms`): no row of the home's own
                // casts there. One other colonies only defend
                // (`BorrowedWork.Defended`) is the home's row's as much as
                // theirs (#589).
                if
                    owners
                    |> List.exists (fun p ->
                        p.Home <> room && Set.contains room (residentRooms p.View))
                then
                    owners |> List.filter (fun p -> p.Home <> room)
                else
                    owners
            |> List.map (fun p -> p, aroundOf p)

        let rows =
            casters
            |> List.map (fun (p, priced) -> p.Home, rowAnswer priced room)
            |> Map.ofList

        // A row's Win binds its colony only where that row casts for the
        // room or one of its fighters takes the raid; a colony that only
        // sees it binds nobody, and the room falls to the kite or the
        // rangers that win it (#573).
        let rowed =
            casters
            |> List.filter (fun (p, _) -> rows.[p.Home].Wins && (p.Casts room || takes p))
            |> List.map fst

        if not (List.isEmpty rowed) then
            { verdict Answer.Win rows (rowed |> List.map (fun p -> p.Home) |> Set.ofList) [] with
                Casting =
                    rowed
                    |> List.filter (fun p -> p.Casts room)
                    |> List.map (fun p -> p.Home)
                    |> Set.ofList
            }
        else
            // A kite: a caster's ranger as the chooser prices it, or any
            // colony's ranger standing by that kites it down with its live
            // parts. A verdict that counts a body it does not have casts it
            // (#655): each caster whose ranger kites the raid down
            // (`castKites`: only a ranger its bank buys), and that may cast
            // for the room, is the Kite's `Casting`.
            let castKiting =
                casters
                |> List.filter (fun (_, priced) -> castKites priced room)
                |> List.map fst

            let standing = seeing |> List.filter kites

            match castKiting @ standing |> List.distinctBy (fun p -> p.Home) with
            | _ :: _ ->
                { verdict Answer.Kite rows Set.empty [] with
                    // Cast only where no ranger already standing by kites it:
                    // that one is the body counted.
                    Casting =
                        if List.isEmpty standing then
                            castKiting
                            |> List.filter (fun p -> p.Casts room)
                            |> List.map (fun p -> p.Home)
                            |> Set.ofList
                        else
                            Set.empty
                }
            | [] ->
                match rangers.Value with
                | Some taken -> verdict Answer.Win rows Set.empty taken
                | None -> verdict Answer.Lose rows Set.empty []

/// One room's verdict as this view reads it: the world's, priced once a tick
/// over the fleet (`judge`), or, where the world priced none — a room nothing
/// sees a raider in, or a fixture of one colony — over this view alone, its
/// bodies where it places them.
let verdictOf (view: ColonyView) (room: string) : RoomVerdict =
    match Map.tryFind room view.World.Answers with
    | Some verdict -> verdict.Value
    | None ->
        priceAmong
            (SpatialInfo.creepRoomOf view.Spatial)
            [
                {
                    Home = SpatialInfo.homeName view.Spatial
                    View = view
                    Casts = (=) room
                    Sends = fun _ _ -> true
                    Holds = fun _ -> false
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
/// cast. A room shut on a raid (`ColonyView.RaidedOutposts`) pools no row's
/// Guard, its economy being out, so any raid there the verdict does not lose
/// is the chase, whichever row or rangers win it (#569, #571): the stand-down
/// shuts the economy, never the kill. In sight the room is a contested room
/// pursued (`Contention.Pursued`, #663): its raid and its prey priced
/// together; the stand-down's row latches it (`OutpostEpisode.Chased`) for
/// the blind ticks after, the chase proper. Read off the room's `verdict`
/// (`answered`). Never a [[sweep]] candidate (#658): the sweep fights in the
/// one it works and leaves the rest.
let internal chased (view: ColonyView) (verdict: Answer * bool) (kept: bool) (room: string) : bool =
    room <> SpatialInfo.homeName view.Spatial
    && not (Set.contains room (rangerRooms view))
    && not (Set.contains room view.HarassCast)
    && raided view room
    && (match verdict with
        | Answer.Lose, _ -> false
        | _, false -> true
        | _, true ->
            Set.contains room view.RaidedOutposts
            || kept && (castKites view room || Option.isSome (rangersWin view room)))

/// Whether nothing of ours wins the raid standing in a room (`Answer.Lose`):
/// in a ranger room, the raid its row casts nobody into, and the residents
/// already there hold the room's safe ground against.
let internal outmatched (view: ColonyView) (room: string) : bool = answer view room = Answer.Lose

/// Whether the resident garrison loses the raid standing in a room beside its
/// loaded towers: what opens its fight record. Both the garrison's size and the
/// rangers actually standing there, priced together, lose.
let internal residentsLose (view: ColonyView) (room: string) : bool =
    let standing =
        ColonyView.bodies view
        |> List.filter (fun creep ->
            isRangerRowBody creep
            && SpatialInfo.creepPlacementOf view.Spatial creep.Name
               |> Option.exists (fun at -> at.Room = room))

    let standingWin = not (List.isEmpty standing) && bodiesWin view room standing

    not standingWin
    && not (blocksBeat rangerPattern Stance.Standoff view room view.Tuning.RangerResidentBlocks)

/// Whether an economy body may walk into a room (#605): a room of ours
/// always; any other while no raid stands in it, or while the verdict does
/// not lose the raid and the bodies of ours holding the room's Guard or
/// Defend (`holds`) win it, those seated at the fight (`atFight`) together
/// (`bodiesWin`) or one kiting it down (`kitedDown`). The verdict says whether
/// to fight, never whether the economy walks in. A body already holding work
/// there (`kept`) keeps it while any holder stands in the room or beside it,
/// so a kite dancing out past the fight turns nobody back at the border. The
/// raid is the raiders we strike anywhere (`Colony.strikesAnywhere`), seen,
/// or recalled while `WorldView.recallShuts`; one that can barely move (#510)
/// is the Reach's alone.
let internal openToEconomy
    (view: ColonyView)
    (holds: CreepInfo -> bool)
    (kept: bool)
    (room: string)
    : bool =
    let recalled = view.RecalledRaiders |> List.map _.Id |> Set.ofList

    let seen = Map.tryFind room view.World.RaidSeen |> Option.defaultValue view.Time

    let raid =
        let raiders =
            raidersIn view room
            |> List.filter (fun h ->
                Colony.strikesAnywhere h.Owner && not (HostileInfo.stationary view.Hostiles h))

        let span =
            lazy
                (raidHoldSpan
                    view
                    room
                    (raiders |> List.filter (fun h -> Set.contains h.Id recalled)))

        raiders
        |> List.filter (fun h ->
            not (Set.contains h.Id recalled)
            || WorldView.recallShuts span.Value view.Time seen h)

    let near = Set.ofList (room :: RoomName.adjacent room)

    // The holders standing in the room or beside it, whichever colony's: the
    // world's fleet (`WorldView.Bodies`), the colony's own beside it for a
    // fixture with no world laid in.
    let holders () =
        let placed =
            ColonyView.bodies view
            |> List.filter (fun creep ->
                SpatialInfo.creepPlacementOf view.Spatial creep.Name
                |> Option.exists (fun at -> near.Contains at.Room))

        near
        |> Seq.collect (fun room ->
            Map.tryFind room view.World.Bodies.Value
            |> Option.defaultValue []
            |> Seq.map fst)
        |> Seq.append placed
        |> Seq.distinctBy _.Name
        |> Seq.filter holds
        |> List.ofSeq

    let held () =
        match holders () with
        | [] -> false
        | _ when kept -> true
        | holding ->
            let seated =
                holding
                |> List.filter (fun creep -> walkToRaid view raid creep |> Option.exists atFight)

            not (List.isEmpty seated)
            && (bodiesWin view room seated
                || seated |> List.exists (fun creep -> kitedDown view (Fighter.ofCreep creep) room))

    ourRoom view room
    || List.isEmpty raid
    || answer view room <> Answer.Lose && held ()

/// The Planner's narrow view of the assignment table, filtered to the living:
/// `Assignments` arrives from Memory and may name a creep that died last tick,
/// the Matcher drops those silently, but `planTasks` runs first. Not filtered
/// to task kinds: a reader that wants one kind writes the key it wants and
/// asks, as `isHungry` does.
let heldTaskFacts (view: ColonyView) (assignments: Assignments) : HeldTaskFacts =
    let living =
        ColonyView.bodies view
        |> List.map (fun creep -> creep.Name, creep)
        |> Map.ofList

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
/// what comes (its Guard's `Resident` job), while there is ore to burn in it
/// (`fuelledErrands`, #420). A held errand and a withdrawn one are out of
/// `view.Errands` before this reads it.
let private guardedErrandsOf (fuelled: Errand list) : string list =
    fuelled
    |> List.map (fun errand -> errand.RoomName)
    |> List.distinct
    |> List.sort

/// The other colonies' homes this colony defends (`BorrowedWork.Defended`)
/// that a hostile endangers this tick (`endangersOurs`, #429): it pools
/// their Guard and asks for it; which colony casts is the Caster's
/// (`Caster.assign`, #589).
let private defendedHomesOf (view: ColonyView) : string list =
    view.Borrowed.Defended
    |> List.filter (fun room ->
        view.Hostiles
        |> List.exists (fun h -> h.Pos.Room = room && endangersOurs view h))

/// Whether the home stands raided by bodies nothing of ours answers but its
/// own row's cast (#568): a body we may strike that endangers it
/// (`endangersOurs`, #429), out of safe mode, that no Defend meets — one
/// inside the line, or any where the home holds no line a defender stands
/// on (`raidAtLine`, `perimeterRamparts`) — priced with the healers beside
/// it against the home's towers, which lose it — a dismantler's raid where
/// their damage does not outpace its heal (`RaidPrice.Breaches`); and the
/// verdict weighs the home's own row (`RoomVerdict.Rows`; a home another
/// colony raises or garrisons is its residents'), whose cast wins
/// (`castPrice`) or beside which another colony's row is weighed. It pools
/// the home's Guard and asks for it; which colony casts is the Caster's
/// (`Caster.assign`, #589), and the home's own row casts no losing fight.
let private homeUnheld (view: ColonyView) atlas : bool =
    let home = SpatialInfo.homeName view.Spatial

    let shielded =
        Map.tryFind home view.RoomControl
        |> Option.exists (fun control -> control.SafeMode)

    let inHome =
        view.Hostiles
        |> List.filter (fun h -> h.Pos.Room = home && not (HostileInfo.isKeeper h))

    if shielded || inHome |> List.forall (endangersOurs view >> not) then
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
                && (endangersOurs view h && mayStrike view Set.empty h
                    || HostileInfo.heals view.Hostiles h))

        // The home's own towers, more than `WorldRoom.OurTowers`' loaded
        // ones: each holding a shot's energy, and each dry one
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

        raid |> List.exists (endangersOurs view)
        && not (
            settleBoth
                (&&)
                (raidPrices view Reaction.strike Reaction.strike (fun _ -> Engine.meleeRange) raid)
                (fun price ->
                    wonAgainst
                        price
                        (towers * Engine.towerAttackAt Engine.towerFalloffRange)
                        0
                        (0, 0)
                        (towers * Engine.towerHits))
        )
        && (let rows = (verdictOf view home).Rows

            match Map.tryFind home rows with
            | Some row ->
                Map.count rows > 1
                || fst (
                    castPrice
                        view
                        (if row.Ranger then
                             CastRow.Fighter BodyTemplate.Ranged
                         else
                             CastRow.Fighter BodyTemplate.Melee)
                        home
                )
            | None -> false)

/// The rooms whose Guard `planTasks` pools whatever kind claims them, and the
/// rooms the fighting rows cast for (`OutpostFacts.Guarded`), or a Guard is
/// pooled with no body bought or a body cast with no Task: the declared
/// outposts a threat or a level-0 core (#487) stands in, the errand rooms the
/// ranger row keeps a body in (#414), the children's homes this colony raises
/// (#447), the other homes it defends (#589), the [[sweep]]'s one room where
/// it works it (#658), and its own home while a raid there is unheld
/// (`homeUnheld`, #568). Each
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
    // The sweep's room, which it works only while it holds a kill (#658).
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

/// Whether a colony may cast for an ask not its own: a spawn, and a bank and
/// Storage stock that pay it (`CastBid.paysOthers`). The Caster's floor.
let castsForOthers (view: ColonyView) : bool =
    not (List.isEmpty view.Spawns)
    && CastBid.paysOthers view.Tuning view.Bank.Capacity (stockedEnergy view)

/// Whether a room is another colony's home this one defends
/// (`BorrowedWork.Defended`) and neither raises nor garrisons
/// (`raisedHomes`, whose residents are its own): of the rooms a colony asks
/// for, the one it casts for only past the Caster's floor
/// (`castsForOthers`).
let defendsOthers (view: ColonyView) (room: string) : bool =
    List.contains room view.Borrowed.Defended
    && not (Set.contains room (raisedHomes view))

/// Whether a colony's row is weighed for a room, by the Caster's
/// eligibility for the asks it makes: a room of its own past no floor — its
/// home, its declared outposts, its ranger rooms (`rangerRooms`, a raised or
/// garrisoned home among them), and the rooms of its own (`owns`) the
/// stand-down shuts on a raid, whose row the stand-down, an economy gate,
/// never withholds from the verdict —; another colony's home it defends
/// (`defendsOthers`) and a [[sweep]] candidate it projects, no colony's own
/// (#658), only past the floor (`castsForOthers`). A colony that only could cast for a room, asking
/// nothing there, is no row of its verdict: a cast on paper is no answer
/// beside a body standing there (#573). Whose row the world's verdict weighs
/// (`priceAmong`), a home's own only where no other colony raises or
/// garrisons it.
let private castsInto (owns: Set<string>) (view: ColonyView) : string -> bool =
    let own =
        lazy
            (Set.unionMany
                [
                    Set.singleton (SpatialInfo.homeName view.Spatial)
                    Set.ofList (declaredOutposts view)
                    rangerRooms view
                    Set.intersect owns view.RaidedOutposts
                ])

    let defended =
        match view.Borrowed.Defended with
        | [] -> Set.empty
        | rooms -> Set.difference (Set.ofList rooms) (raisedHomes view)

    let floored = lazy (castsForOthers view)

    fun room ->
        if defended.Contains room || view.HarassCast.Contains room then
            floored.Value
        else
            own.Value.Contains room

/// The rooms a colony prices a raid in, seen or not: those it declares
/// (`Colony.declaredRooms`), its declared outposts, its ranger rooms and the
/// harassment rooms it casts. The set the danger log's fold
/// (`Observe.Danger.foldColony`) prices its stand-down rows over.
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
        /// The rooms it declares (`Colony.declaredRooms`), whatever its
        /// stand-down shuts: the gate's raid rows are the world's, every
        /// colony's alike, and only its own are its rows' to cast for.
        Owns: Set<string>
        /// Whether it decides this tick: one that replays its plan sends a
        /// room only the bodies already holding the room's Guard.
        Decides: bool
    }

/// The world's verdicts this tick: each room a raider stands
/// in, in the world's vision or in any colony's view (a raid it recalls
/// through a blind tick), priced once and on first read over the fleet and
/// the world's vision of the room (`priceAmong`) and laid in every view, so
/// one room has one verdict whoever reads it and whatever any colony's cut
/// or stand-down holds. Each view comes with whether it decides this tick
/// (`Judged`); `held` is the shard's assignments, what a replaying colony's
/// bodies hold.
let judge (held: Assignments) (colonies: Judged list) : ColonyView list =
    let pricers =
        colonies
        |> List.map (fun colony ->
            {
                Home = SpatialInfo.homeName colony.Cut.Spatial
                View = colony.Cut
                Casts = castsInto colony.Owns colony.Cut
                Sends =
                    if colony.Decides then
                        fun _ _ -> true
                    else
                        fun room creep -> Map.tryFind creep.Name held = Some(taskId (Guard room))
                Holds =
                    fun room ->
                        ColonyView.bodies colony.Cut
                        |> List.exists (fun creep ->
                            Map.tryFind creep.Name held = Some(taskId (Guard room)))
            })

    let world =
        match colonies with
        | [] -> WorldView.empty
        | first :: _ -> first.Cut.World

    // Where the fleet stands, by creep name, whichever colony holds it.
    let fleet =
        lazy
            (world.Bodies.Value
             |> Map.toSeq
             |> Seq.collect (fun (room, bodies) ->
                 bodies |> Seq.map (fun ((creep: CreepInfo), _) -> creep.Name, room))
             |> Fresh.mapOfSeq)

    let placed name = Map.tryFind name fleet.Value

    let raidedIn (hostiles: HostileInfo list) =
        hostiles
        |> List.filter (HostileInfo.raider hostiles)
        |> List.map (fun hostile -> hostile.Pos.Room)

    let verdicts =
        (world.Rooms
         |> Map.toList
         |> List.collect (fun (_, seen) ->
             seen.Hostiles
             |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner))
             |> raidedIn))
        @ (pricers |> List.collect (fun p -> raidedIn p.View.Hostiles))
        |> List.distinct
        |> List.map (fun room -> room, lazy (priceAmong placed pricers room))
        |> Map.ofList

    colonies
    |> List.map (fun colony ->
        { colony.Cut with
            World =
                { colony.Cut.World with
                    Answers = verdicts
                }
        })
