/// The colony reflexes that stand beside the pipeline — safe mode and tower
/// fire — and the Layout: the RCL-gated allowances and the declarative
/// structure plan, plus outpost containers and pickups.
[<AutoOpen>]
module Fabot.Core.Decide.Layout

open Fabot.Core
open Fabot.Core.Types

/// The Layout's one pick rule: cheapest by the caller's price, ties to the
/// lowest (X, Y), and None where there is nothing to pick from. Every tile the
/// Layout settles on goes through it, because a pick that left the coordinates
/// out of its key would be decided by set ordering instead — and set ordering
/// is not the same under Fable as under .NET, so the plan would differ between
/// the bot and its own tests.
let private cheapest (price: 'a -> int) (tileOf: 'a -> Pos) (candidates: 'a list) : 'a option =
    match candidates with
    | [] -> None
    | _ ->
        candidates
        |> List.minBy (fun candidate ->
            let tile = tileOf candidate
            price candidate, tile.X, tile.Y)
        |> Some

/// The hostiles standing in the colony's own room, which is the whole of what
/// the two reflexes below may read: safe mode protects a controller of ours and
/// an outpost has none, and a tower's shot is a range act inside its own room.
/// The home name and not the controller's or a tower's room, because both arms
/// need an answer on a tick the projection places neither.
let private hostilesAtHome (view: ColonyView) : HostileInfo list =
    let home = SpatialInfo.homeName view.Spatial
    view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

/// Colony reflex beside the pipeline (ADR-0007, ADR-0015), two arms and one
/// pair of gates — stock remaining, safe mode not already running. The
/// CLAIM arm holds until a claimer stands within reach of the range-1 tap; an
/// unplaceable controller falls back to firing on sight. The Keep arm: any Keep
/// structure below full hits while any hostile stands in the home room — any
/// hostile and not only a Threat, a WORK-only dismantler hurting a structure
/// without ever qualifying as one. Stateless on purpose: one tick's hits, never
/// a comparison against the last tick's.
let internal planSafeMode (view: ColonyView) atlas : Intent list =
    match view.Controller with
    | Some controller when controller.SafeModeAvailable > 0 && not controller.SafeModeActive ->
        // The colony's own room and no other (#201): a claimer in an outpost is
        // tapping a controller safe mode does not cover.
        let here = hostilesAtHome view

        let withinReach (h: HostileInfo) =
            List.contains BodyPart.Claim h.Body
            && match Atlas.positionOf atlas controller.Id with
               // Across a border there is no range to measure, so None here is
               // "not in reach", where an unplaced controller below is still
               // "fire on sight".
               | Some tile ->
                   RoomPos.range h.Pos tile
                   |> Option.exists (fun r -> r <= view.Tuning.SafeModeDeadline)
               | None -> true

        let claimerInReach = here |> List.exists withinReach

        // The structure half is `keepDamaged`'s, off the same projected hits the
        // Repair pool walks. The Posts and the ramparts are hungry on their own
        // lines and are not of the Keep.
        let dentedKeepUnderHostiles = not (List.isEmpty here) && keepDamaged view

        // The undefended arm (#217): a colony with no tower standing fires on
        // the first armed hostile in its room.
        let undefended =
            List.isEmpty (Atlas.placedTowers atlas) && here |> List.exists isArmed

        if claimerInReach || dentedKeepUnderHostiles || undefended then
            [ ActivateSafeMode controller.Id ]
        else
            []
    | _ -> []

/// The consignment's one intent (#349): a terminal shipping this colony's
/// banked Thorium to the consignee's terminal — the only place the ore crosses
/// the map without a body; the hauling either side is the `Planner`'s. Four
/// facts and no memory: a declared consignee, the terminal's Thorium, its
/// energy, and the engine's fee. Not read: the cooldown (`RoomFacts` carries
/// none, so about a tenth of the asks are refused `ERR_TIRED`, a logged failure
/// and a tick's call), and anything about the far end, which is outside every
/// scan set this colony holds — the engine moves the ore or refuses the intent.
/// The amount is the smaller of what the terminal holds and what its energy
/// pays the fee on, floored at `Engine.terminalMinSend`.
let internal planConsignment (view: ColonyView) : Intent list =
    match view.Consignee, view.Spatial.RoomName with
    | Some destination, Some home ->
        let range =
            // Screeps prices a send over `Game.map.getRoomLinearDistance`,
            // which is Chebyshev — the diagonal of a room-name grid costs one
            // room, not two — and every other distance in this tree is the hop
            // count (`RoomName.hopsBetween`). Read the engine's metric here and
            // not the walk's: the fee is the engine's to charge.
            RoomName.offsetOf home destination
            |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

        let ship terminalId =
            let banked = SpatialInfo.heldIn view.Spatial Thorium terminalId
            let energy = SpatialInfo.storedIn view.Spatial terminalId

            match range with
            | Some range when banked >= Engine.terminalMinSend ->
                // The largest amount this terminal's own energy pays for,
                // found by the fee rather than by an inverse of it: the
                // engine's formula is an exponential and the amount is an
                // integer, so the affordable amount is read off the fee of
                // what is there.
                let affordable =
                    if Engine.sendFee range banked <= energy then
                        banked
                    else
                        // Scale down by the ratio the fee overshoots by, then
                        // step back to a figure the energy covers. One
                        // correction is enough because the fee is linear in the
                        // amount — the exponential is in the range alone.
                        let ratio = float energy / float (Engine.sendFee range banked)
                        float banked * ratio |> floor |> int

                if
                    affordable >= Engine.terminalMinSend
                    && Engine.sendFee range affordable <= energy
                then
                    Some(SendFromTerminal(terminalId, Thorium, affordable, destination))
                else
                    None
            | _ -> None

        view.Spatial.TargetKinds
        |> Map.toList
        |> List.filter (fun (_, kind) -> kind = Structure BuiltKind.Terminal)
        |> List.choose (fst >> ship)
    | _ -> []

/// Colony reflex beside the pipeline (ADR-0014): every tower shoots the hostile
/// nearest to itself, every tick one stands in the room. No energy gate: unlike
/// safe mode there is no stock to protect, so a dry tower's Intent fails
/// harmlessly. Equal ranges tie-break by hostile id. `placedTowers` has always
/// answered home alone, and the hostiles are narrowed to match;
/// `RoomPos.range` answers None across a border.
let internal planFire (view: ColonyView) atlas : Intent list =
    match hostilesAtHome view with
    | [] -> []
    | hostiles ->
        Atlas.placedTowers atlas
        |> List.choose (fun (towerId, tile) ->
            hostiles
            |> List.choose (fun h -> RoomPos.range tile h.Pos |> Option.map (fun r -> r, h))
            |> function
                | [] -> None
                | reachable ->
                    let _, target = reachable |> List.minBy (fun (r, h) -> r, h.Id)
                    Some(FireTower(towerId, target.Id)))

/// The fire reflex's quiet twin (#410): with no hostile at home to shoot, each
/// tower heals the creep of ours at home with the most hits still owed, the
/// heal it will land (`Engine.towerHealAt` its range) counted off as it is
/// planned so two towers do not both pour into a wound the first one closes.
/// Never on a tick `planFire` fires: the engine runs a tower's heal before its
/// attack and drops the attack. Ties by name. No energy gate, as `planFire`
/// has none: a dry tower's heal fails harmlessly.
let internal planTowerHeal (view: ColonyView) atlas : Intent list =
    let home = SpatialInfo.homeName view.Spatial

    if not (List.isEmpty (hostilesAtHome view)) then
        []
    else
        let wounded =
            view.Creeps
            |> List.filter (fun creep -> creep.Hits.Hits < creep.Hits.HitsMax)
            |> List.choose (fun creep ->
                SpatialInfo.creepPlacementOf view.Spatial creep.Name
                |> Option.filter (fun tile -> tile.Room = home)
                |> Option.map (fun tile ->
                    creep.Name, (tile, creep.Hits.HitsMax - creep.Hits.Hits)))
            |> Map.ofList

        ((wounded, []), Atlas.placedTowers atlas)
        ||> List.fold (fun (owed, heals) (towerId, towerTile) ->
            owed
            |> Map.toList
            |> List.filter (fun (_, (_, left)) -> left > 0)
            |> List.sortBy (fun (name, (_, left)) -> -left, name)
            |> List.tryHead
            |> function
                | None -> owed, heals
                | Some(name, (tile, left)) ->
                    let landed =
                        RoomPos.range towerTile tile
                        |> Option.map Engine.towerHealAt
                        |> Option.defaultValue 0

                    Map.add name (tile, left - landed) owed, HealWithTower(towerId, name) :: heals)
        |> snd
        |> List.rev

/// What a controller level allows the room, by kind (Screeps
/// CONTROLLER_STRUCTURES). One table over the kind and not one per kind,
/// because the gap rule below subtracts a census keyed by that same kind: a
/// pair carried separately is a pair that can be handed to each other's
/// allowance. Only the sized kinds are in it; every other kind the Layout
/// places is sized by its own rule.
let private allowanceOf kind level =
    match kind, level with
    | BuiltKind.Extension, (0 | 1) -> 0
    | BuiltKind.Extension, 2 -> 5
    | BuiltKind.Extension, 3 -> 10
    | BuiltKind.Extension, 4 -> 20
    | BuiltKind.Extension, 5 -> 30
    | BuiltKind.Extension, 6 -> 40
    | BuiltKind.Extension, 7 -> 50
    | BuiltKind.Extension, _ -> 60
    | BuiltKind.Tower, (0 | 1 | 2) -> 0
    | BuiltKind.Tower, (3 | 4) -> 1
    | BuiltKind.Tower, (5 | 6) -> 2
    | BuiltKind.Tower, 7 -> 3
    | BuiltKind.Tower, _ -> 6
    | BuiltKind.Storage, (0 | 1 | 2 | 3) -> 0
    | BuiltKind.Storage, _ -> 1
    | BuiltKind.Terminal, (0 | 1 | 2 | 3 | 4 | 5) -> 0
    | BuiltKind.Terminal, _ -> 1
    | _ -> 0

/// The kinds the clustered horizon sizes, and the ones the ceiling below is
/// read over. The Storage is not one of them: it reads no horizon at all and
/// holds its whole allowance from level 0.
let private clusteredKinds =
    [ BuiltKind.Extension; BuiltKind.Tower; BuiltKind.Terminal ]

/// The level past which `allowanceOf` stops growing (ADR-0064): the smallest
/// level at which every clustered kind already answers its catch-all row,
/// jointly, because it is the joint window the reservation is sized at. Read
/// off the table and never written down, so the table is what moves the day
/// the engine adds a level.
///
/// Two premises the table has to keep, unchecked here because a check that
/// fails at module load is a bot that never boots: every clustered kind has a
/// wildcard row of its own (the trailing `| _ -> 0` does not count, and an
/// unbounded row makes the climb diverge), and every clustered kind's row is
/// non-decreasing in the level, which is what makes the reservation never
/// narrower than the placement.
///
/// `terminal` asks for `Int32.MaxValue` and not an arithmetic expression on
/// it: a `when` guard doing arithmetic on `level` is the one place .NET's
/// wrapping int and Fable's JS number would answer differently.
let private allowanceCeiling =
    let terminal kind = allowanceOf kind System.Int32.MaxValue

    let rec climb level =
        if
            clusteredKinds
            |> List.forall (fun kind -> allowanceOf kind level = terminal kind)
        then
            level
        else
            climb (level + 1)

    climb 0

/// Whether the Layout places road sites at all this tick (#209): only for an
/// `Independent` colony. Not an engine unlock — the engine allows a road at
/// RCL1 — but the stage below which a road is the wrong spend: a bootstrapping
/// room's trunk set is thousands of energy of income placed in one tick, on the
/// same surplus tier as the Upgrade and nearer to hand, so every worker builds
/// roads and nobody upgrades.
let private placesRoads (view: ColonyView) = isIndependent view

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR-0011), computed whole from the Atlas every tick
/// and placed all at once — no persisted plan, no pacing. One ordering rule
/// seats every clustered structure, the working ground excluded (ADR-0022);
/// trunk roads route around the reservation, which the Link footing count
/// widens (ADR-0027); a rampart covers every standing Keep structure and Post
/// container (ADR-0034), a rampart being no footprint at all.
let internal planLayout
    (view: ColonyView)
    atlas
    : Intent list *
      ServedFooting list *
      UnservedFooting list *
      UnroutedTrunk list *
      DeferredContainer list
    =
    let home = Atlas.homeRoom atlas

    // The tile the whole plan is oriented on, and it is a tile of the room
    // being planned.
    let inHome (tile: RoomPos) = Some tile.Room = home

    let anchor =
        view.Spawns
        |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id |> Option.filter inHome)

    match home, anchor, view.Controller with
    | Some room, Some anchorTile, Some controller ->
        let spawnPos = RoomPos.pos anchorTile
        // Same checkerboard colour as the spawn: clustered structures sit on
        // the spawn's colour, leaving the other colour free for movement.
        let parity = (spawnPos.X + spawnPos.Y) % 2

        // The sources this plan is for: the home room's alone.
        let homeSources =
            view.Sources |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some room)

        // The working ground — every source's Seats and the controller's
        // Upgrade Work Area — is off-limits to the cluster.
        let working = Atlas.workingGroundIn atlas room

        let buildable = Atlas.buildableTilesIn atlas room

        let ordering =
            buildable
            |> List.filter (fun tile ->
                (tile.X + tile.Y) % 2 = parity && not (Set.contains tile working))
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)

        // A kind's still-open gap at a level: its allowance there minus the
        // projection's censuses of standing and pending structures of that
        // kind. Judged at the level the kind is reserved for it sizes the
        // reservation; at the current level it sizes the placement.
        //
        // The room being planned, and no other (#140): the allowance is this
        // controller's, so what is subtracted from it is this room's census —
        // a neighbour's site counted here is a site this room never places.
        let gapAt kind level =
            allowanceOf kind level
            - Atlas.builtIn atlas room kind
            - Atlas.pendingIn atlas room kind
            |> max 0

        // The still-unclaimed slots, Storage first and tower next: a built or
        // pending structure keeps its tile out of the ordering (it is a target)
        // and its slot off the plan. The clustered kinds are placed at the
        // horizon; the Storage reads none — its whole allowance is held from
        // level 0, because once an extension takes that tile it never comes
        // back.
        //
        // The horizon is this room's own level plus the lookahead (ADR-0063),
        // read here beside the level the placement filters at rather than off
        // a constant. It sizes the placement alone; the reservation below reads
        // the ceiling instead and no level at all.
        let horizon = Tuning.horizonOf view.Tuning controller.Level

        let storageSlots = gapAt BuiltKind.Storage view.Tuning.StorageLevel

        // The terminal's slot, sized at the horizon and not from level 0
        // (#349): holding a tile from level 0 for a kind unlocked at RCL6 cost
        // a cramped room its fifth extension at RCL2. What the horizon risks
        // is one tile of distance.
        let terminalSlots = gapAt BuiltKind.Terminal horizon

        // ... and at the ceiling for the reservation the trunks dodge: a road
        // planned across the terminal's tile at RCL2 is a road orphaned at
        // RCL6.
        let reservedTerminalSlots = gapAt BuiltKind.Terminal allowanceCeiling
        let towerSlots = gapAt BuiltKind.Tower horizon
        let extensionSlots = gapAt BuiltKind.Extension horizon

        // The same two kinds sized at the ceiling instead, which is what the
        // trunk router dodges. Sized there the reservation is a function of
        // the terrain and of this room's own census, and of no level at all,
        // so a bare room's road plan is identical at every level by
        // construction. It is never narrower than the placement's:
        // `allowanceOf` never decreases and the ceiling is where it stops.
        let reservedTowerSlots = gapAt BuiltKind.Tower allowanceCeiling
        let reservedExtensionSlots = gapAt BuiltKind.Extension allowanceCeiling

        // The Link footings cannot be named here — their targets are the
        // container picks, which are derived from the trunks the reservation is
        // for — but their count can: one per source, one for the controller
        // container, one for the Storage.
        let footingSlots = List.length homeSources + 2

        let clustered =
            ordering
            |> List.truncate (
                storageSlots
                + reservedTerminalSlots
                + reservedTowerSlots
                + reservedExtensionSlots
                + footingSlots
            )

        let storagePick = ordering |> List.truncate storageSlots

        // The terminal behind the Storage in the same ordering (#349): the
        // ordering is the cluster sorted by range from the spawn, so the tile
        // after the Storage's is the nearest tile to it the cluster has, and
        // the ore's walk from the one store to the other is a hauler's
        // shortest leg.
        let terminalPick =
            ordering
            |> List.skip (min storageSlots (List.length ordering))
            |> List.truncate terminalSlots

        // Reserved before trunks: a trunk never crosses a tile a reserved
        // structure will claim, and the widened window holds the footings as
        // well. The footings' own tiles are settled below.
        let reserved = Set.ofList clustered

        // The reservation as the router reads it, joined once: the trunks ask
        // for it per source per goal, and a room name added to every tile of
        // it at each ask is a census tick's worth of rebuilding for an answer
        // that does not move (#216 R3).
        let reservedTiles = RoomPos.setAt room reserved

        // This room's share of the controller's Upgrade Work Area: a
        // controller the projection files under another room contributes
        // nothing here, rather than its coordinates.
        let upgradeArea =
            Atlas.workArea atlas (Upgrade controller.Id) |> RoomPos.inRoom room

        // Each goal beside the name it is recorded under when a source cannot
        // reach it (#107). The Upgrade Work Area first and the spawns after,
        // which is the order the routes are collected in and therefore the
        // order a loss reads in.
        let trunkGoals =
            (TrunkGoal.UpgradeArea, RoomPos.setAt room upgradeArea)
            :: (view.Spawns
                |> List.choose (fun s ->
                    Atlas.positionOf atlas s.Id
                    |> Option.filter inHome
                    |> Option.map (fun spawn ->
                        TrunkGoal.Spawn s.Id,
                        Atlas.adjacentWalkableIn atlas room (RoomPos.pos spawn)
                        |> List.map (RoomPos.at room)
                        |> Set.ofList)))

        // Every route the Layout asks for, kept per source and per goal: the
        // union paves the roads and each source's own trunk anchors its
        // container, while the goals stay apart because the loss below is per
        // goal.
        let sourceRoutes =
            homeSources
            |> List.sortBy (fun s -> s.Id)
            |> List.choose (fun s ->
                Atlas.positionOf atlas s.Id
                |> Option.filter inHome
                |> Option.map (fun sourcePos ->
                    s.Id,
                    trunkGoals
                    |> List.map (fun (goal, area) ->
                        goal,
                        Atlas.trunkPath atlas reservedTiles sourcePos area
                        |> List.map RoomPos.pos)))

        let sourceTrunks =
            sourceRoutes
            |> List.map (fun (id, routes) -> id, routes |> List.collect snd |> Set.ofList)

        // The empty path is the router's answer for a goal it paved nothing
        // for, and it unions into the road plan contributing nothing. Recorded
        // here, where the source and the goal are both still in scope:
        // downstream there is only a set of tiles, and a trunk that was dropped
        // whole looks exactly like one that was never asked for.
        let unroutedTrunks =
            sourceRoutes
            |> List.collect (fun (id, routes) ->
                routes
                |> List.choose (fun (goal, path) ->
                    if List.isEmpty path then
                        Some { Source = id; Goal = goal }
                    else
                        None))

        let trunkTiles = sourceTrunks |> List.map snd |> List.fold Set.union Set.empty

        // The spawn-bound half of those routes, kept apart from the union: the
        // mineral container is seated against the Storage's trunk, and the
        // Storage stands on the cluster's first pick beside the spawn, so the
        // paved line a deposit's load is carried down is the `Spawn` half. The
        // `UpgradeArea` half leads the other way, and a Seat priced against the
        // union would, at a deposit beyond the spawn, be the Seat furthest from
        // the haul.
        let spawnTrunkTiles =
            sourceRoutes
            |> List.collect (fun (_, routes) ->
                routes
                |> List.collect (fun (goal, path) ->
                    match goal with
                    | TrunkGoal.Spawn _ -> path
                    | TrunkGoal.UpgradeArea -> []))
            |> Set.ofList

        // The controller's Work Area paves its swamps and only its swamps —
        // upgraders shuttle within it, so the dear ground gets a road and the
        // plain ground does not. No reservation can stand here: the Work Area
        // is working ground, which the ordering never offered.
        let workAreaSwamps = upgradeArea |> Set.filter (Atlas.isSwampIn atlas room)

        // Every tile the Layout paves: the trunks plus the Work Area's
        // swamps. The road gap measures this against the projection's road
        // census, and a Link footing is chosen off it.
        let roadPlan = Set.union trunkTiles workAreaSwamps

        // A built road or a pending road site already claims its tile.
        let roadGap =
            Set.difference roadPlan (Atlas.roadTilesIn atlas room)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTilesIn atlas room)

        // The road sites this tick: the whole gap once the colony is
        // `Independent`, none before it (#209). The stage gate is a filter on
        // the placement and not on the plan — `roadPlan` and `roadGap` are
        // computed at every stage, so the trunks still route around the
        // reservation — and it is a gate rather than pacing.
        let placedRoads =
            if placesRoads view then
                Set.difference roadGap (Atlas.pendingContainerTilesIn atlas room)
            else
                Set.empty

        // Containers, computed whole like everything else and RCL-gated by
        // nothing — the engine allows them from level 0. Each source's
        // container sits on the Seat nearest that source's trunk; the trunk's
        // first tile is itself a Seat, so in practice the container lands where
        // the trunk leaves the source and harvest overflow falls straight in.
        // Seats are terrain geometry and trunks avoid only the reservations, so
        // the pick never shifts as the container gets built.
        let sourceContainerPicks =
            sourceTrunks
            |> List.choose (fun (sourceId, trunk) ->
                let seats = Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room

                // The trunk guard is not the empty-list one `cheapest` makes:
                // the price below is a `List.min` over the trunk's own tiles.
                if Set.isEmpty trunk then
                    None
                else
                    seats
                    |> Set.toList
                    |> cheapest
                        (fun seat -> trunk |> Set.toList |> List.map (range seat) |> List.min)
                        id
                    |> Option.map (fun seat -> sourceId, seat))

        let sourceContainerTiles = sourceContainerPicks |> List.map snd

        // The controller container: preferably an Upgrade-Work-Area tile beside
        // a trunk and off the road itself; only when that strict set is empty
        // may the buffer share the paving (ADR-0068). The footing fold below
        // records the resulting shortfall rather than deleting the buffer.
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.filter inHome
            |> Option.map RoomPos.pos
            |> Option.bind (fun controllerPos ->
                let besideTrunk tile =
                    trunkTiles |> Set.exists (fun t -> range tile t = 1)

                let candidates =
                    upgradeArea
                    |> Set.filter (fun tile ->
                        not (Set.contains tile trunkTiles) && besideTrunk tile)

                let pick tiles =
                    tiles |> Set.toList |> cheapest (fun tile -> range tile controllerPos) id

                candidates
                |> Set.filter (fun tile -> not (Set.contains tile workAreaSwamps))
                |> pick
                |> Option.orElseWith (fun () -> pick candidates))

        // The room's Thorium deposits and the level that unlocks them
        // (ADR-0057 decision 1). `CONTROLLER_STRUCTURES.extractor` is 1 at
        // RCL6, 7 and 8 and 0 below. The current level and not the horizon:
        // the deposit sits on a wall tile off the clustered checkerboard, so
        // there is no window an extension can take and nothing to hold open.
        // The container is gated with it: a container beside a deposit no body
        // can dig is a site the surplus tier builds for nothing.
        let minerals =
            if controller.Level >= view.Tuning.ExtractorLevel then
                Atlas.mineralsIn atlas room
            else
                []

        // The extractor, on the mineral's own tile. One per room ever, so the
        // census is the whole of the gap. The tile clause is still owed below
        // (`extractorGap`): a road site is the one kind Screeps allows on a
        // natural wall, and the extractor is the one kind whose tile can never
        // move to dodge one.
        let extractorTiles =
            let census = Atlas.extractorCensusIn atlas room

            minerals
            |> List.map snd
            |> List.filter (fun tile -> not (Set.contains tile census))

        // The mineral container, on the deposit's Seat nearest the Storage's
        // trunk: the source container's rule with the source swapped out. A
        // mineral has no trunk of its own — nothing paves one to a deposit — so
        // it is seated against the line the load is carried down to the
        // Storage, which is `spawnTrunkTiles` and not the whole network. At a
        // wall mouth this is a choice between one and three tiles and never a
        // search.
        let mineralContainerPicks =
            minerals
            |> List.choose (fun (mineralId, _) ->
                let seats = Atlas.seatTilesOf atlas mineralId |> RoomPos.inRoom room

                // The same trunk guard the source picks carry: the price below
                // is a `List.min` over the trunk's own tiles, which has no
                // answer for a room that paved none — and a room that reached
                // no spawn has no haul to the Storage to price against either.
                if Set.isEmpty spawnTrunkTiles then
                    None
                else
                    seats
                    |> Set.toList
                    |> cheapest
                        (fun seat ->
                            spawnTrunkTiles |> Set.toList |> List.map (range seat) |> List.min)
                        id
                    |> Option.map (fun seat -> mineralId, seat))

        // The Link footings: one tile held beside every target a link will
        // ever serve — each planned source container, the controller container,
        // and the Storage. Not the mineral container: a link carries energy and
        // nothing else. Planned, not built: a Post needs a standing container,
        // so a Post-anchored rule would reserve nothing at level 0. The tiles
        // are settled here rather than with the reservation because a
        // footing's targets are the container picks, derived from the trunks
        // the reservation is for; re-flooding the trunks to name the tiles
        // first would pay the tick's dearest step twice.
        let footingTargets =
            [
                for tile in sourceContainerTiles -> tile, FootingKind.SourceContainer
                for tile in Option.toList controllerContainerTile ->
                    tile, FootingKind.ControllerContainer
                for tile in storagePick -> tile, FootingKind.Storage
                for tile in
                    Set.union
                        (Atlas.storageTilesIn atlas room)
                        (Atlas.pendingStorageTilesIn atlas room)
                    |> Set.toList -> tile, FootingKind.Storage
            ]
            |> List.distinctBy fst

        // The tiles no footing may be reserved on: every footing target, since
        // a link beside one container may not stand on another's tile, and the
        // mineral container's and the terminal's picks with them (#349), which
        // are the targets of no footing and reach this list through no other
        // route. A link and a container cannot share a tile, and the loss is
        // silent in both directions: the footing fold would record the tile as
        // served while the container site took it.
        let footingBlockedTiles =
            (footingTargets |> List.map fst)
            @ (mineralContainerPicks |> List.map snd)
            @ terminalPick

        // A standing link is a target, so its own footing has stopped being
        // buildable: added back, or the footing would jump the tick the link
        // went up. The working ground is deliberately not subtracted — a
        // footing is the one structure footing allowed there, because a link
        // on a Seat or an Upgrade tile is exactly what buys the Anchor and the
        // upgraders a transfer without leaving their tile.
        let footingCandidates =
            Set.union (Set.ofList buildable) (Atlas.linkTilesIn atlas room)

        // A target with no candidate at all leaves the tiles alone and is
        // recorded: the fold reserves what it can, and the shortfall rides out
        // beside the plan instead of falling through in silence. What it does
        // reserve rides out too, each tile beside the target and the kind it
        // was reserved for — both in scope here and nowhere else, since no
        // Intent ever names a link.
        let footingTiles, servedFootings, unservedFootings =
            ((Set.empty, [], []), footingTargets)
            ||> List.fold (fun (taken, served, unserved) (target, kind) ->
                footingCandidates
                |> Set.filter (fun tile ->
                    range tile target = 1
                    && not (Set.contains tile roadPlan)
                    && not (List.contains tile footingBlockedTiles)
                    && not (Set.contains tile taken))
                |> Set.toList
                |> cheapest (fun tile -> range tile spawnPos) id
                |> function
                    | None ->
                        taken,
                        served,
                        {
                            Target = RoomPos.at room target
                            Kind = kind
                        }
                        :: unserved
                    | Some tile ->
                        Set.add tile taken,
                        {
                            Target = RoomPos.at room target
                            Kind = kind
                            Tile = RoomPos.at room tile
                        }
                        :: served,
                        unserved)

        // The tower and the extensions take the ordering again with the
        // footings held out — a footing outranks both — and the Storage's and
        // the terminal's picks held out with them (#349). `towerSlots` and
        // `extensionSlots`, not the `reserved…` pair: this is the placement,
        // sized at the horizon, inside the reservation the trunks already
        // dodged.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick)
                && not (List.contains tile terminalPick)
                && not (Set.contains tile footingTiles))
            |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clusterPicks |> List.splitAt (min towerSlots clusterPicks.Length)

        // The container census the target clause is judged against: a
        // container standing, or a site already going up.
        let containerCensus = Atlas.containerCensusIn atlas room

        // The target clause (ADR-0040): a source is served when a container
        // stands or is pending within range 1 of it, the controller when one
        // stands or is pending in its Upgrade Work Area. A pick the clause
        // defers because something else serves its target is a loss the room
        // keeps, so it rides out beside the footings and the trunks. Named for
        // the geometry and not for the source: two kinds of rock are judged by
        // it and the rule is one rule.
        let servingRock rockId =
            Atlas.positionOf atlas rockId
            |> Option.filter inHome
            |> Option.map (fun rockPos ->
                Set.filter (servesSource (RoomPos.pos rockPos)) containerCensus)
            |> Option.defaultValue Set.empty

        // Every target beside its pick and the containers already serving it.
        // Both answers below are read off this one list, so each target is
        // judged once and the same judgement decides whether it is planned for
        // and whether it lost its pick.
        let targets =
            [
                for sourceId, pick in sourceContainerPicks ->
                    ContainerTarget.Source sourceId, pick, servingRock sourceId
                for pick in Option.toList controllerContainerTile ->
                    ContainerTarget.Controller, pick, Set.intersect containerCensus upgradeArea
                for mineralId, pick in mineralContainerPicks ->
                    ContainerTarget.Mineral mineralId, pick, servingRock mineralId
            ]

        let unservedPicks =
            targets
            |> List.choose (fun (_, pick, serving) ->
                if Set.isEmpty serving then Some pick else None)

        let deferredContainers =
            targets
            |> List.choose (fun (target, pick, serving) ->
                if Set.isEmpty serving || Set.contains pick serving then
                    None
                else
                    Some
                        {
                            Target = target
                            Pick = RoomPos.at room pick
                            Serving = RoomPos.at room (Set.minElement serving)
                        })

        // The tile clause, and only it: a pick whose tile another site already
        // holds waits, because the engine takes one construction site per tile.
        // `Atlas.collidingSiteTilesIn` is every pending site in the room but a
        // container's, ours (#246) or a rival's (#248: a site placed while the
        // room was neutral survives into the room we claim). Beside it, the
        // roads placed this tick, which no census carries yet — the placed
        // roads and not the whole gap, since below the road gate none is
        // placed at all.
        let takenTiles = Set.union placedRoads (Atlas.collidingSiteTilesIn atlas room)

        // Distinct, because the picks are made per target and the targets are
        // judged independently: a deposit two tiles from a rock, or from the
        // controller's Work Area, can be seated on the very tile that rock's
        // container was picked for. Two `PlaceConstructionSite(tile,
        // Container)` in one tick is one site and one `ERR_INVALID_TARGET`,
        // and one container within range 1 of both serves both.
        let containerGap =
            unservedPicks
            |> List.filter (fun tile -> not (Set.contains tile takenTiles))
            |> List.distinct

        // The extractor's tile clause, subtracted off the same census the
        // containers are: the one kind that can stand on the wall the deposit
        // occupies is a road, which the engine allows there as a tunnel, so a
        // site on that tile is a placement the engine refuses and the plan
        // cannot route around.
        let extractorGap =
            extractorTiles |> List.filter (fun tile -> not (Set.contains tile takenTiles))

        // The ramparts: one over every standing Keep structure and every
        // standing Post container, the tick the thing it covers stands — a site
        // is not covered until it is a structure. No allowance to size against:
        // the gap is the covering census alone, standing ramparts and pending
        // sites subtracted the way the roads' is. The one gate is the colony's
        // stage (`keepsRamparts`).
        let covered =
            if keepsRamparts view then
                Set.union (Atlas.keepTilesIn atlas room) (Atlas.postContainerTilesIn atlas room)
            else
                Set.empty

        let rampartGap =
            Set.difference
                covered
                (Set.union
                    (Atlas.rampartTilesIn atlas room)
                    (Atlas.pendingRampartTilesIn atlas room))

        let place kind tiles =
            tiles
            |> List.map (fun tile -> PlaceConstructionSite(RoomPos.at room tile, kind))

        place Storage (storagePick |> List.truncate (gapAt BuiltKind.Storage controller.Level))
        @ place Terminal (terminalPick |> List.truncate (gapAt BuiltKind.Terminal controller.Level))
        @ place Tower (towerTiles |> List.truncate (gapAt BuiltKind.Tower controller.Level))
        @ place
            Extension
            (extensionTiles |> List.truncate (gapAt BuiltKind.Extension controller.Level))
        @ place Road (Set.toList placedRoads)
        @ place Container containerGap
        @ place Extractor extractorGap
        @ place Rampart (Set.toList rampartGap),
        List.rev servedFootings,
        List.rev unservedFootings,
        unroutedTrunks,
        deferredContainers
    // A room the Layout cannot even orient itself in plans nothing and loses
    // nothing: there are no targets to serve and no trunk was ever asked for,
    // so every record is empty rather than any of them being a shortfall.
    | _ -> [], [], [], [], []

/// The outpost's source containers — the colony's one placement rule that is
/// not the Layout's: one container per outpost source, on the Seat whose walk
/// out to the Seam toward home is shortest, the Seam being the only fixed
/// thing in a room with no spawn to pave a trunk to. The room is the outpost's
/// own: the Layout stamps the single room it plans onto every site it emits,
/// so a pick routed through it would land on the home room's coordinates.
/// Served by target, with the census read in the source's own room.
///
/// And a tile clause (#244): a human lays road sites across Seats, and a
/// container asked for on an occupied tile is `ERR_INVALID_TARGET` once a
/// tick for ever — no container, no Post, no income. A Seat holding another
/// site, ours or anyone's (`Atlas.collidingSiteTilesIn`), is no candidate, and
/// a source whose every Seat is taken waits; our own sites clear once the
/// builders' queue reaches them (#266), a rival's never, and no record says so
/// — an outpost shortfall entry is its own ticket. A standing road is not
/// subtracted: a container site goes down on a built road.
///
/// Only into a room the colony can see: planning off a blind room's empty
/// census would hand the Executor an `ActorMissing` a tick per rock. Recomputed
/// every tick and not ridden on the plan memo: signing the pending census it
/// reads would throw the whole Layout away the tick an outpost site appears,
/// and what the memo would buy is one flood a room a tick — 4.84 ms without
/// this rule against 5.35 with it.
let internal planOutpostContainers (view: ColonyView) atlas : Intent list =
    let home = SpatialInfo.homeName view.Spatial

    // Every rock the projection places in a room that is not home and that
    // the colony is looking into this tick — `RoomControl` carries one
    // entry per seen room, and vision is what both the census below and
    // the Executor's own `Game.rooms` lookup are paid for with.
    view.Sources
    |> List.choose (fun s ->
        match Atlas.positionOf atlas s.Id with
        | Some tile when tile.Room <> home && Map.containsKey tile.Room view.RoomControl ->
            Some(s.Id, tile)
        | _ -> None)
    |> List.choose (fun (sourceId, source) ->
        let room = source.Room

        let served =
            Atlas.containerCensusIn atlas room
            |> Set.exists (servesSource (RoomPos.pos source))

        if served then
            None
        else
            // Three Seats all of swamp can price identically, so the lowest
            // (X, Y) answers. Over the Seats a site of another kind has not
            // already taken (#244).
            Set.difference
                (Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room)
                (Atlas.collidingSiteTilesIn atlas room)
            |> Set.toList
            |> List.choose (fun seat ->
                Atlas.seamWalkTicks atlas room home seat |> Option.map (fun walk -> walk, seat))
            |> cheapest fst snd
            |> Option.map (fun (_, seat) -> PlaceConstructionSite(RoomPos.at room seat, Container)))

/// The colony's signature, written by whoever is standing there (#381). A
/// reflex and not a Task: it sends nobody anywhere, and the engine keeps a sign
/// until it is overwritten. It reaches what somebody already has business
/// beside — an outpost's controller by its reserver, a claimed room by its
/// claimer — and not a home controller, which the upgrader row works from
/// range 3 (four were signed by hand once). Silent when the text standing
/// there is ours; absence is not a match, which is the case this exists for.
/// One creep per controller per tick, the lowest name beside it. Geometry
/// through the Atlas and not off the projection, so the keeper margin's mask
/// reaches this rule too.
let internal planSignatures (view: ColonyView) atlas (text: string) : Intent list =
    let signed room =
        Map.tryFind room view.RoomControl
        |> Option.bind (fun control -> control.Sign)
        |> Option.contains text

    let placed = Atlas.placedCreeps atlas

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.choose (fun id ->
        match Atlas.positionOf atlas id with
        | Some at when not (signed at.Room) ->
            placed
            |> List.filter (fun (_, tile) ->
                RoomPos.range tile at |> Option.exists (fun range -> range <= 1))
            |> List.map fst
            |> List.sort
            |> List.tryHead
            |> Option.map (fun name -> SignController(name, id, text))
        | _ -> None)

/// Colony reflex beside the pipeline: every creep with free carry capacity
/// standing within pickup range of a dropped energy pile asks to pick it up —
/// beside its assigned Task's action, since the engine's pickup conflicts with
/// no other action. No movement, no matching, no threshold. One target per
/// creep, the last reachable pile in Atlas order. Paired once per room and
/// never across two: a pile and a creep on the same coordinate of two rooms
/// would draw a pickup the engine answers ERR_NOT_IN_RANGE. A body already
/// carrying Thorium is not hungry (#262): the reflex asks nothing of
/// `applicable`, so free capacity alone let it scoop energy into a mixed load.
let internal planPickups (view: ColonyView) atlas : Intent list =
    let hungry =
        view.Creeps
        |> List.filter (fun c -> c.FreeCapacity > 0 && c.Thorium = 0)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    Atlas.placedCreeps atlas
    |> List.groupBy (fun (_, tile) -> tile.Room)
    |> List.collect (fun (room, placed) ->
        match Atlas.droppedEnergyIn atlas room with
        | [] -> []
        | piles ->
            placed
            |> List.collect (fun (name, pos) ->
                if Set.contains name hungry then
                    piles
                    |> List.choose (fun (pile, tile) ->
                        if RoomPos.range pos tile |> Option.exists (fun r -> r <= 1) then
                            Some(PickupPile(name, pile))
                        else
                            None)
                    |> List.tryLast
                    |> Option.toList
                else
                    []))
