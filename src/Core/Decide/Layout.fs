/// The colony reflexes that stand beside the pipeline — safe mode (ADR 0007) and
/// tower fire (ADR 0014) — and the Layout: the RCL-gated allowances and the
/// declarative structure plan (ADR 0011), plus outpost containers and pickups.
[<AutoOpen>]
module Fabot.Core.Decide.Layout

open Fabot.Core
open Fabot.Core.Types

/// The hostiles standing in the colony's own room, which is the whole of what
/// the two reflexes below may read. Since `ColonyView.Hostiles` stopped being
/// the spawn rooms' alone, "a hostile" and "a hostile here" are two different
/// questions, and both reflexes ask the second: safe mode protects a controller
/// of ours and an outpost has none (ADR 0007), and a tower's shot is a range act
/// inside its own room (ADR 0014). Everything above them — Reach, Flee, the
/// spawn hold — reads the list whole and files each hostile under its own room
/// (ADR 0033). The home name and not the controller's or a tower's room, because
/// both arms need an answer on a tick the projection places neither: ADR 0004's
/// absence would otherwise widen the reflex back to every room.
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

let private hostilesAtHome (view: ColonyView) : HostileInfo list =
    let home = SpatialInfo.homeName view.Spatial
    view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

/// Colony reflex beside the pipeline, two arms and one pair of gates — stock
/// remaining, safe mode not already running. The CLAIM arm: a CLAIM-part
/// hostile is the one threat that can disarm safe mode itself,
/// `attackController` blocking activation for 1,000 ticks. But the tap is a
/// range-1 act, so the activation holds until a claimer stands within reach of
/// landing it (ADR 0015) — free, and it buys the towers their window. An
/// unplaceable controller falls back to firing on sight. The Keep arm (ADR
/// 0034): any Keep structure below full hits while any hostile stands in the
/// home room — the same shape, hold until the harm is certain, over the other
/// half of the exposure. Any hostile and not only a Threat, a WORK-only
/// dismantler hurting a structure without ever qualifying as one. Stateless on
/// purpose: one tick's hits, never a comparison against the last tick's.
let internal planSafeMode (view: ColonyView) atlas : Intent list =
    match view.Controller with
    | Some controller when controller.SafeModeAvailable > 0 && not controller.SafeModeActive ->
        // The colony's own room and no other (`hostilesAtHome`, #201): a
        // claimer in an outpost is tapping a controller safe mode does not
        // cover, and the Keep it could be denting is not in that room.
        let here = hostilesAtHome view

        let withinReach (h: HostileInfo) =
            List.contains BodyPart.Claim h.Body
            && match Atlas.positionOf atlas controller.Id with
               // Across a border there is no range to measure (ADR 0052
               // decision 2), and a claimer in another room is tapping a
               // controller this safe mode does not cover — so None here is
               // "not in reach", where an unplaced controller below is still
               // "fire on sight".
               | Some tile ->
                   RoomPos.range h.Pos tile
                   |> Option.exists (fun r -> r <= view.Tuning.SafeModeDeadline)
               | None -> true

        let claimerInReach = here |> List.exists withinReach

        // A Keep structure below full hits **and** a hostile standing here,
        // which is this arm's whole condition and why the name says both. The
        // structure half is `keepDamaged`'s, off the same projected hits the
        // Repair pool walks and no longer a filter over its pool (ADR 0061 —
        // the reason is written there, once). The Posts and the ramparts are
        // hungry on their own lines and are not of the Keep.
        let dentedKeepUnderHostiles = not (List.isEmpty here) && keepDamaged view

        // The undefended arm (ADR 0034 as #217 amends it): a colony with no
        // tower standing fires on the first armed hostile in its room.
        let undefended =
            List.isEmpty (Atlas.placedTowers atlas) && here |> List.exists isArmed

        if claimerInReach || dentedKeepUnderHostiles || undefended then
            [ ActivateSafeMode controller.Id ]
        else
            []
    | _ -> []

/// Colony reflex beside the pipeline (ADR 0014): every tower shoots the hostile
/// nearest to itself, every tick one stands in the room. Attack only — no tower
/// repair or heal — per-tower nearest with no focus fire or anti-drain gate,
/// and no energy gate: unlike safe mode there is no stock to protect, so a dry
/// tower's Intent fails harmlessly. Equal ranges tie-break by hostile id. Both
/// halves of the pairing are the colony's own room's: `placedTowers` has always
/// answered home alone — a tower stands only in a room we own — and the
/// hostiles are narrowed to match. That narrowing is the reflex's own rule and
/// not a repair for a missing join: a tower shoots inside its own room (ADR
/// 0014), and `RoomPos.range` answers None across a border.
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

/// What a controller level allows the room, by kind (Screeps
/// CONTROLLER_STRUCTURES). One table over the kind and not one per kind,
/// because the gap rule below subtracts a census keyed by that same kind: a
/// pair carried separately is a pair that can be handed to each other's
/// allowance. Nothing but the three sized kinds is in it — every other kind
/// the Layout places is sized by its own rule (a road by the trunk, a
/// container by ADR 0040's pick, a rampart by ADR 0034's cover), so an
/// allowance is not the question asked of them and none is answered.
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
    | _ -> 0

/// Whether the Layout places **road sites** at all this tick (ADR 0011 as #209
/// amends it): only for an `Independent` colony. Not an engine unlock — the
/// engine allows a road at RCL1 — but the stage below which a road is the wrong
/// spend: the trunk set a bootstrapping room plans is thousands of energy of
/// income placed in one tick, on the same surplus tier as the Upgrade and
/// nearer to hand, so every worker builds roads and nobody upgrades. One colony
/// at RCL1 planned some 19,000 energy of it against 8 a tick, ahead of the 200
/// progress that unlocks five extensions and doubles the body. This narrows ADR
/// 0010 and does not contradict it: what #209 says is that half a tick a loaded
/// step is not worth 2,400 ticks of income when the same energy buys the level
/// that doubles the body.
let private placesRoads (view: ColonyView) = isIndependent view

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR 0011), computed whole from the Atlas every tick
/// and placed all at once — no persisted plan, no pacing. One ordering rule
/// eats every clustered structure: buildable tiles on the spawn's checkerboard
/// colour, nearest-to-spawn first, the working ground excluded, the Storage's
/// pick before the tower's and both before the extensions' (ADR 0022). Trunk
/// roads pave each source to the controller and to each spawn plus the swamps
/// of the controller's Work Area, priced on raw terrain and routed around every
/// reserved tile, reservations coming first. One tile beside each container
/// pick and beside the Storage is held as a Link footing (ADR 0022) and
/// outranks the tower and the extensions, the reservation being widened by the
/// footing count (ADR 0027); no link is ever placed on one (ADR 0038). Beside
/// all of that runs one rule that reads no tile of the ordering: a rampart
/// covers every standing Keep structure and every standing Post container (ADR
/// 0034), a rampart being no footprint at all.
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
    // being planned (ADR 0052 decision 2).
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

        // The sources this plan is for: the home room's alone (ADR 0041).
        let homeSources =
            view.Sources |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some room)

        // The working ground — every source's Seats and the controller's
        // Upgrade Work Area — is off-limits (ADR 0022): a clustered structure
        // there eats a tile an Anchor or an upgrader stands on, so a colony
        // whose nearest same-colour tiles are working ground clusters one ring
        // out instead of eating them.
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
        // and its slot off the plan. The clustered kinds are sized at the
        // horizon; the Storage is not one of them and reads none (ADR 0022) —
        // its whole allowance is held from level 0, because once an extension
        // takes that tile it never comes back.
        let storageSlots = gapAt BuiltKind.Storage view.Tuning.StorageLevel
        let towerSlots = gapAt BuiltKind.Tower view.Tuning.HorizonLevel
        let extensionSlots = gapAt BuiltKind.Extension view.Tuning.HorizonLevel

        // The Link footings cannot be named here — their targets are the
        // container picks, which are derived from the trunks the reservation is
        // for — but their count can: one per source, one for the controller
        // container, one for the Storage. The window is widened by that many, so
        // the tiles the cluster is pushed onto when a footing takes one of its
        // picks are inside the reservation too (ADR 0027).
        let footingSlots = List.length homeSources + 2

        let clustered =
            ordering
            |> List.truncate (storageSlots + towerSlots + extensionSlots + footingSlots)

        let storagePick = ordering |> List.truncate storageSlots

        // Reserved before trunks: a trunk never crosses a tile a reserved
        // structure will claim, and the widened window holds the footings as
        // well — so the precedence runs one way for every kind the Layout places
        // (ADR 0011). The footings' own tiles are settled below.
        let reserved = Set.ofList clustered

        // The reservation as the router reads it, joined once: the trunks
        // ask for it per source per goal, and a room name added to every
        // tile of it at each of those asks is a census tick's worth of
        // rebuilding for an answer that does not move (#216 R3).
        let reservedTiles = RoomPos.setAt room reserved

        // This room's share of the controller's Upgrade Work Area: a
        // controller the projection files under another room contributes
        // nothing here, rather than its coordinates (ADR 0052 decision 2).
        let upgradeArea =
            Atlas.workArea atlas (Upgrade controller.Id) |> RoomPos.inRoom room

        // Each goal beside the name it is recorded under when a source
        // cannot reach it (#107). The Upgrade Work Area first and the
        // spawns after, which is the order the routes are collected in and
        // therefore the order a loss reads in.
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

        // Every route the Layout asks for, kept per source and per goal:
        // the union paves the roads and each source's own trunk anchors
        // its container (ADR 0012), while the goals stay apart for the
        // reason `TrunkGoal` is a type — the loss below is per goal.
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

        // The spawn-bound half of those routes, kept apart from the union (ADR
        // 0057 decision 1). The mineral container is seated against **the
        // [[storage]]'s trunk**, and the Storage is never the trunk hub: it
        // stands on the cluster's first pick, beside the spawn by construction
        // (ADR 0023), so the paved line a deposit's load is carried down is the
        // `Spawn` half of `sourceRoutes`. The `UpgradeArea` half leads the
        // other way — past the spawn and out to the controller — and a Seat
        // priced against the union would, at a deposit beyond the spawn, be the
        // Seat *furthest* from the haul that is the whole reason the container
        // is there. The goals are kept apart in `sourceRoutes` precisely so a
        // rule can ask for one of them.
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
        // plain ground does not. No reservation can stand here: the Work Area is
        // working ground, which the ordering never offered (ADR 0022).
        let workAreaSwamps = upgradeArea |> Set.filter (Atlas.isSwampIn atlas room)

        // Every tile the Layout paves: the trunks plus the Work Area's
        // swamps. The road gap measures this against the projection's road
        // census, and a Link footing is chosen off it.
        let roadPlan = Set.union trunkTiles workAreaSwamps

        // The road gap reads the projection's road census: a built road or a
        // pending road site already claims its tile (ADR 0010).
        let roadGap =
            Set.difference roadPlan (Atlas.roadTilesIn atlas room)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTilesIn atlas room)

        // The road sites this tick: the whole gap once the colony is
        // `Independent`, none before it (#209). The stage gate is a filter on
        // the placement and not on the plan — `roadPlan` and `roadGap` are
        // computed at every stage, so the trunks still route around the
        // reservation — and it is a gate rather than pacing, which ADR 0011
        // rejected and still rejects. It is the same shape the clustered kinds
        // already have, one question coarser.
        let placedRoads =
            if placesRoads view then
                Set.difference roadGap (Atlas.pendingContainerTilesIn atlas room)
            else
                Set.empty

        // Containers (ADR 0012), computed whole like everything else and
        // RCL-gated by nothing — the engine allows them from level 0. Each
        // source's container sits on the Seat nearest that source's trunk; the
        // trunk's first tile is itself a Seat, so in practice the container
        // lands where the trunk leaves the source and harvest overflow falls
        // straight in. Seats are terrain geometry and trunks avoid only the
        // reservations, so the pick never shifts as the container gets built,
        // and Seats need no reservation dodge (ADR 0022).
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

        // The controller container: an Upgrade-Work-Area tile beside a trunk
        // and off the road itself — the buffer upgraders work from standing
        // still, one tile from where the haulers drive. No reservation to dodge
        // either: the Work Area is working ground (ADR 0022).
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.filter inHome
            |> Option.map RoomPos.pos
            |> Option.bind (fun controllerPos ->
                upgradeArea
                |> Set.filter (fun tile ->
                    not (Set.contains tile trunkTiles)
                    && not (Set.contains tile workAreaSwamps)
                    && trunkTiles |> Set.exists (fun t -> range tile t = 1))
                |> Set.toList
                |> cheapest (fun tile -> range tile controllerPos) id)

        // The room's Thorium deposits and the level that unlocks them (ADR
        // 0057 decision 1). `CONTROLLER_STRUCTURES.extractor` is 1 at RCL6, 7
        // and 8 and 0 below, so nothing here is planned until the room stands
        // at `Tuning.ExtractorLevel` — and it is the **current** level and not
        // the horizon, which is the other half of that decision: the deposit
        // sits on a wall tile at the mouth of a wall, off the clustered
        // checkerboard and unbuildable for every other kind, so unlike the
        // Storage and the Link footings there is no window an extension can
        // take and nothing to hold open (ADR 0022). The container is gated
        // with it rather than planned from level 0 the way a source's is: a
        // container beside a deposit no body can dig is a site the surplus
        // tier builds for nothing.
        let minerals =
            if controller.Level >= view.Tuning.ExtractorLevel then
                Atlas.mineralsIn atlas room
            else
                []

        // The extractor, on the mineral's own tile (ADR 0057 decision 1). One
        // per room ever, so the census is the whole of the gap: a standing
        // extractor or a site going up on that tile is the plan already made,
        // and asking again would be `ERR_INVALID_TARGET` once a tick for ever.
        // No allowance and no ordering — the tile is the target's, and the
        // clustered ring never offers a wall. The tile clause is still owed
        // below (`extractorGap`) and not asserted away here: the engine takes
        // one construction site per tile whoever placed it, a **road** site is
        // the one kind Screeps allows on a natural wall, and a rival's site
        // survives into a room we claim (#248) — and the extractor is the one
        // kind whose tile can never move to dodge one, so an unsubtracted
        // collision is `ERR_INVALID_TARGET` once a tick for ever.
        let extractorTiles =
            let census = Atlas.extractorCensusIn atlas room

            minerals
            |> List.map snd
            |> List.filter (fun tile -> not (Set.contains tile census))

        // The mineral container, on the deposit's Seat nearest the **Storage's**
        // trunk (ADR 0057 decision 1): the source container's rule with the
        // source swapped out. A source seats its container on its **own**
        // trunk, which is the paved line its haul leaves by; a mineral has no
        // trunk of its own — nothing paves one to a deposit — so what it is
        // seated against is the line the load is carried down to the
        // [[storage]], which is the same walk read from the other end and is
        // `spawnTrunkTiles` and not the whole network. Seats are terrain
        // geometry (ADR 0001), so at a wall mouth this is a choice between one
        // and three tiles and never a search.
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

        // The Link footings (ADR 0022): one tile held for a link beside every
        // target a link will ever serve — each planned source container, the
        // controller container, and the Storage. **Not** the mineral
        // container: a link carries energy and nothing else, so there is no
        // link a deposit's container will ever be served by and no footing to
        // hold for one (ADR 0057 decision 1 names none). Planned, not built: a Post
        // needs a standing container, so a Post-anchored rule would reserve
        // nothing at level 0 and the tiles would be gone by the time links
        // arrive. The count is the rule's, never a constant (ADR 0027). The
        // tiles are settled here rather than with the reservation because a
        // footing's targets are the container picks, derived from the trunks
        // the reservation is for; re-flooding the trunks to name the tiles
        // first would pay the tick's dearest step twice (ADR 0017).
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
        // a link beside one container may not stand on another's tile — and
        // the **mineral container's** pick with them (ADR 0057 decision 1),
        // which is a container the plan is about to ask for and is the target
        // of no footing, so it reaches this list through no other route. A link
        // and a container cannot share a tile, and the loss is silent in both
        // directions: the footing fold would record the tile as *served* while
        // the container site took it, and the Storage would go without the link
        // ADR 0022 reserved one for.
        let footingBlockedTiles =
            (footingTargets |> List.map fst) @ (mineralContainerPicks |> List.map snd)

        // A standing link is a target, so its own footing has stopped being
        // buildable: added back, or the footing would jump the tick the link
        // went up. The working ground is deliberately not subtracted — a footing
        // is the one structure footing allowed there (ADR 0022), because a link
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
        // footings held out — a footing outranks both — and the Storage's pick
        // held out with them: it outranks the footings, which are anchored on
        // it.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick) && not (Set.contains tile footingTiles))
            |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clusterPicks |> List.splitAt (min towerSlots clusterPicks.Length)

        // The container census the target clause is judged against (ADR 0040):
        // a container standing, or a site already going up.
        let containerCensus = Atlas.containerCensusIn atlas room

        // The target clause (ADR 0040): a source is served when a container
        // stands or is pending within range 1 of it, the controller when one
        // stands or is pending in its Upgrade Work Area — the geometry each
        // rule already reads a container by, not the tile this plan happens to
        // have picked. A served target is planned for no further container. A
        // pick the clause defers because something else serves its target is a
        // loss the room keeps — nothing demolishes the orphan — so it rides out
        // beside the footings and the trunks.
        // Named for the geometry and not for the source, because since ADR
        // 0057 two kinds of rock are judged by it and the rule is one rule:
        // served is a container standing or pending within range 1, wherever
        // it sits.
        let servingRock rockId =
            Atlas.positionOf atlas rockId
            |> Option.filter inHome
            |> Option.map (fun rockPos ->
                Set.filter (servesSource (RoomPos.pos rockPos)) containerCensus)
            |> Option.defaultValue Set.empty

        // Every target beside its pick and the containers already serving
        // it. Both answers below are read off this one list, so each
        // target is judged once and the same judgement decides whether it
        // is planned for and whether it lost its pick.
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

        // The tile clause (ADR 0040), and only it: a pick whose tile another
        // site already holds waits, because the engine takes one construction
        // site per tile. This is about the tile and moves with no target. One
        // census answers it — `Atlas.collidingSiteTilesIn`: every pending site
        // of ours in the room but a container's, the road the trunk owes among
        // them and the hand-placed tower, extension or rampart beside it (#246,
        // widening what #209 wrote as the road sites alone: against one of the
        // others the plan re-issued `PlaceConstructionSite` every tick for
        // `ERR_INVALID_TARGET` until somebody built it), and every site another
        // player has placed in the room whatever its kind (#248 — the same
        // refusal from another hand, which the census could not see while it
        // read our own sites alone). That rival half is **narrow** here and
        // wide out in an outpost: the engine refuses a site in a room another
        // player owns, so nobody starts one in this room — but a site placed
        // while the room was still neutral survives into the room we claim, so
        // a freshly claimed nursery is the window it covers. Narrow is not the
        // same as defensive-only, which is why it is subtracted and not merely
        // asserted against. The container kind stays out of *our*
        // half by its own rule, a container site on the pick being the target
        // clause's business above and not a collision. Beside the
        // census, the roads placed **this** tick, which no census carries yet
        // — and the placed roads and not the whole gap (#209): below the road
        // gate none is placed at all, so there is nothing to collide with and
        // nothing to wait for.
        let takenTiles = Set.union placedRoads (Atlas.collidingSiteTilesIn atlas room)

        // Distinct, because the picks are made per **target** and the targets
        // are judged independently (ADR 0040): a deposit two tiles from a rock,
        // or from the controller's Work Area, can be seated on the very tile
        // that rock's container was picked for, and both targets are unserved
        // on the tick before either site stands. Two
        // `PlaceConstructionSite(tile, Container)` in one tick is one site and
        // one `ERR_INVALID_TARGET`, and one container within range 1 of both is
        // exactly what ADR 0040's target clause says serves both.
        let containerGap =
            unservedPicks
            |> List.filter (fun tile -> not (Set.contains tile takenTiles))
            |> List.distinct

        // The extractor's tile clause, subtracted off the same census the
        // containers are (ADR 0057 decision 1, and #248's rule that narrow is
        // not the same as defensive-only): the one kind that can stand on the
        // wall the deposit occupies is a road, which the engine allows there as
        // a tunnel, so a site of ours or of a rival's on that tile is a
        // placement the engine refuses and the plan cannot route around.
        let extractorGap =
            extractorTiles |> List.filter (fun tile -> not (Set.contains tile takenTiles))

        // The ramparts (ADR 0034): one over every standing Keep structure and
        // every standing Post container, the tick the thing it covers stands —
        // a site is not covered until it is a structure. No allowance to size
        // against: the rule is the whole plan, so the gap is the covering
        // census alone, standing ramparts and pending sites subtracted the way
        // the roads' is. The one gate is the colony's [[stage]]
        // (`keepsRamparts`).
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
    // A room the Layout cannot even orient itself in plans nothing and
    // loses nothing: there are no targets to serve and no trunk was ever
    // asked for, so every record is empty rather than any of them being a
    // shortfall (#77, #106, #107).
    | _ -> [], [], [], [], []

/// The outpost's source containers (ADR 0042) — the colony's one placement rule
/// that is not the Layout's, and a rule beside it rather than a branch inside
/// it: one container per outpost source, on the Seat whose walk out to the Seam
/// toward home is shortest. **Why it is not the Layout's.** The Layout seats a
/// source container on the Seat nearest that source's trunk, and a trunk is a
/// paved line to a spawn; an outpost has no spawn, so the pick needs another
/// anchor, and the Seam is the only fixed thing in that room home lies beyond.
/// Nothing here orders a clustered pick, reserves a Link footing, paves a trunk
/// or enters the layout record's three lists: every one of those is a fact
/// about the home room's plan, which ADR 0042 leaves untouched. **The room is
/// the outpost's own**, stamped from the projection's id-to-room join (ADR
/// 0041): the Layout stamps the single room it plans onto every site it emits,
/// so an outpost pick routed through that path would drop a container site on
/// the *home* room's tile of the same coordinates. **ADR 0040 holds here, by
/// target rather than by tile**: a source with a container standing or pending
/// within range 1 is served wherever the thing serving it sits, and the census
/// is read in that source's own room, or a home container on its coordinates
/// would defer the plan forever. **And a tile clause after all** (#244). ADR
/// 0040 keeps the two questions apart and this rule was written with only the
/// target one, on the premise that nothing paves an outpost — but a *human*
/// does: live in W13S29 the user laid road sites across the Seats both picks
/// answered, and since the engine takes one construction site per tile the
/// Executor asked for a container on an occupied tile and was answered
/// ERR_INVALID_TARGET once a tick, for ever — no container, so no Post, no
/// Anchor and no income, behind a road the surplus tier gives two workers
/// hundreds of ticks to finish. So a Seat holding a site of another kind of
/// ours — or a site of **anyone else's**, of any kind at all, which out here in
/// a room nobody owns is the likelier hand and was invisible to the census
/// until #248 — is no candidate
/// (`Atlas.collidingSiteTilesIn`), the pick is the shortest walk
/// over the Seats that are left, and a source whose every Seat is taken plans
/// nothing and waits: asking the engine for a refusal once a tick is not a
/// plan, and this colony has no vocabulary for cancelling a human's site.
/// **That wait clears no faster than the builders' budget reaches the site**
/// (#266), and until #266 it did not clear at all: the Seat frees when the
/// site on it is *built*, and nothing built it — a non-container site in an
/// outpost was a plain Surplus Build with no home rung (#234) and outside the
/// builders' budget (#157, which was keyed on a container site), so travel
/// cost kept every loaded worker at the home controller. The same budget now
/// queues those sites, the [[seam]] nearest first, so the Seat frees of itself
/// once its site reaches the head of that queue — which is a wait on a queue
/// and no longer a wait on the human. It is still a wait, and can be a long
/// one: the queue is `Tuning.OutpostBuilders` long against a trunk 45 sites
/// long, and a Seat is wherever in it the human happened to pave. What keeps
/// its travel cost meanwhile is everything behind the head ("an ordinary
/// outpost site keeps its travel cost"), and the outage is silent either way,
/// the `-7` line having been the only thing that ever said so.
/// **That argument is about our own sites, and does not carry to a rival's**
/// (#248). #266's queue is the Build pool, which this colony keeps ours-only
/// on purpose — we never raise another player's site — so no budget ever
/// reaches one, and the only other way a site leaves a tile by itself is a
/// creep of ours stepping onto an **obstacle**-kind one, which a road or a
/// container is not. So a rival's road or container site on a Seat is a wait
/// with no end this colony can name: both Seats held that way and the rock has
/// no container, no [[post]], no [[anchor]] and no income, indefinitely and
/// with nothing in the timeline saying so. It is still the better of the two
/// answers — asking the engine for a refusal once a tick was never a plan, and
/// the `-7` was a symptom and not a report — but it is a wait on the rival's
/// hand and not on a queue, and the record that would say so out loud is not
/// this rule's to keep: the [[layout record]]'s three lists are the home
/// Layout's, and an outpost shortfall entry beside them is its own ticket. A
/// **standing** road is not subtracted and must not be: a container site goes
/// down on a built road, and out here that is the best tile there is. It is a
/// collision rule and not a target one, so ADR 0040's "by target, not by tile"
/// is untouched. **Only into a room the colony can see.**
/// Both halves of the rule are paid for by vision, and a blind room's empty
/// census is a missing entry and not a "no container" (ADR 0004): planning off
/// it would hand the Executor an Intent it can only report as `ActorMissing`,
/// once a tick per rock for ever. Nothing is lost by waiting — a Harvest names
/// an outpost rock with no vision at all. **Recomputed every tick, and
/// deliberately not ridden on the plan memo.** The signature does not sign this
/// rule's other inputs — the outpost's terrain, its declared source tiles, its
/// Seam band, and the *pending* census its own site lands in — and signing the
/// pending half would throw the whole Layout and spawn-walk table (ADR 0032)
/// away the tick an outpost site appears. What it would buy is one flood a room
/// a tick, measured at 4.84 ms a tick without this rule and 5.35 with it,
/// against ADR 0041's revisit trigger of a 50 ms mean. Total (ADR 0004): a
/// source the projection does not place, a source in a room the colony cannot
/// see, a room with no Seam band to home, and a source no Seat of which can
/// reach one all plan nothing.
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
            // The pick, and with it the tie-break — the same trap the Layout's
            // own pick has: three Seats all of swamp can price identically, so
            // the lowest (X, Y) answers, exactly as every other tie in the
            // colony answers. Over the Seats a site of another kind has not
            // already taken (#244): one construction site per tile is the
            // engine's rule, and a pick onto a taken tile is refused every
            // tick until that site is built.
            Set.difference
                (Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room)
                (Atlas.collidingSiteTilesIn atlas room)
            |> Set.toList
            |> List.choose (fun seat ->
                Atlas.seamWalkTicks atlas room home seat |> Option.map (fun walk -> walk, seat))
            |> cheapest fst snd
            |> Option.map (fun (_, seat) -> PlaceConstructionSite(RoomPos.at room seat, Container)))

/// Colony reflex beside the pipeline, the second after safe mode: every creep
/// with free carry capacity standing within pickup range of a dropped energy
/// pile asks to pick it up — beside its assigned Task's action, since the
/// engine's pickup conflicts with no other action. No movement, no matching, no
/// threshold: the reflex only recaptures what is already in reach. One target
/// per creep, retaining the last reachable pile in Atlas order; different
/// creeps asking for one pile are still the engine's to settle.
///
/// Paired once per room the projection places a creep in, and never across two:
/// a pickup is a range-1 act inside one room, and a pile in one room and a creep
/// in another on the same coordinate would draw a pickup the engine answers
/// ERR_NOT_IN_RANGE. Both sides carry that room in the tile, so the range is
/// measured or it is not measured at all. The room that made it necessary is the
/// outpost (ADR 0042): its hauler runs one container, so the Anchor's overflow
/// lands on the container's own tile, a full container turns that overflow into
/// a pile, and the hauler then stands *on* the pile and walked away from it.
///
/// **A body already carrying the season's ore is not hungry** (#262, ADR 0057
/// decision 3). The reflex runs beside the pipeline and asks nothing of
/// `applicable`, so free capacity alone let a hauler holding 150 of 200 Thorium
/// scoop energy into the same store — the mixed load decision 3 forbids, made by
/// the one act in the colony that never consulted the gate forbidding it. It is
/// survivable today, the Feeding-tier energy Refill outranking the Stock-tier
/// Thorium one so the body pours the energy first, but two claims the Thorium
/// arm rests on are false while it stands: that `Refill(_, Energy)` is
/// unreachable for a laden body, and that a body holding Thorium has no energy.
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
