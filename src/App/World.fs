// Reads the engine, once, and files what it answered under the room names
// it answered for: this tick's World (ADR 0052 decision 1). The only code
// that reads the game's *objects*; what one colony makes of them is
// `ColonyView.ofWorld`'s, in Core, where a test can hand it a world.
module Fabot.World

open Fabot.Bindings
open Fabot.Core.Types

/// Classify one tile of engine terrain into the Core's three states.
let private terrainAt (terrain: ITerrain) x y =
    let mask = terrain.get (x, y)

    if mask &&& terrainMaskWall <> 0 then Wall
    elif mask &&& terrainMaskSwamp <> 0 then Swamp
    else Plain

let private posOf (p: IRoomPosition) : Pos = { X = p.x; Y = p.y }

/// The tile a creep stands on, room and all — the one reading of an engine
/// creep's position, since a `Pos` carries no room (ADR 0041) and a creep's own
/// room is the only room its coordinates mean anything in.
let private tileOf (c: ICreep) : RoomPos = RoomPos.at c.room.name (posOf c.pos)

/// Classify an engine part-type string into the Core's body vocabulary:
/// the reverse of the Core's one part-name table. The engine's part set is
/// closed, so Tough is an unreachable fallback that keeps it total.
let private bodyPartOf =
    reverseOf partName allBodyParts >> Option.defaultValue Tough

/// Classify an engine STRUCTURE_* string into the Core's built kinds. A
/// string the table lacks is a kind the decision layer has no rules for,
/// which is what Other says. Classified once here so every filter below
/// reads the kind and the rules over it stay in Core (#75).
let private builtKindOf =
    reverseOf builtKindName allBuiltKinds >> Option.defaultValue BuiltKind.Other

/// One room's terrain as the engine spells it — the whole fifty-by-fifty grid,
/// in the two windows the projection assembles from it, off one engine read so
/// there is one terrain truth per room (ADR 0041).
type private RoomTerrain =
    {
        /// x,y in 1..48: the ground the projection stands on.
        Ground: Map<Pos, Terrain>
        /// The border ring, x or y of 0 or 49: the Seam's terrain, never
        /// ground.
        Border: Map<Pos, Terrain>
    }

/// The projection's terrain, memoised per room name (ADR 0031). Room
/// terrain is fixed for the life of the server, so the key can never go
/// stale. Heap state only — nothing here reaches Memory, and a global
/// reset empties the table.
let private terrainMemo =
    System.Collections.Generic.Dictionary<string, RoomTerrain>()

let private terrainOf (roomName: string) : RoomTerrain =
    match terrainMemo.TryGetValue roomName with
    | true, tiles -> tiles
    | _ ->
        let terrain = Game.map.getRoomTerrain roomName

        let tiles =
            {
                // Rows and columns 0/49 are exit tiles — stepping on one
                // teleports the creep into the next room. They stay out
                // of the projection's ground: an absent tile is
                // impassable, so no path, Seat or standing candidate ever
                // uses an exit (ADR 0041). Do not "fix" this trim.
                Ground =
                    Map.ofList
                        [
                            for x in 1..48 do
                                for y in 1..48 do
                                    { X = x; Y = y }, terrainAt terrain x y
                        ]
                // The same read's other window: the ring the trim drops,
                // kept beside the ground because a Seam is a pair of rooms
                // joined at a tile and never a tile to stand on (ADR 0036,
                // ADR 0041).
                Border =
                    Map.ofList
                        [
                            for x in 0..49 do
                                for y in 0..49 do
                                    if x = 0 || x = 49 || y = 0 || y = 49 then
                                        { X = x; Y = y }, terrainAt terrain x y
                        ]
            }

        terrainMemo.[roomName] <- tiles
        tiles

/// The absolute tick a structure's collapse timer runs out at, or None where it
/// carries none (ADR 0043). `effects` is undefined on an object nothing is
/// applied to, and a deployed core carries other effects beside this one, so
/// the array is searched by id rather than read at an index. `Game.time +` is
/// load-bearing: the engine's `ticksRemaining` is a **relative** count, while
/// ADR 0043's prose is written in the read-only API's absolute `endTime`.
/// Storing either raw puts the deadline about a hundred thousand ticks out.
let private collapseTickOf (structure: IStructure) : int option =
    if isNull (box structure.effects) then
        None
    else
        structure.effects
        |> Array.tryFind (fun effect -> effect.effect = effectCollapseTimer)
        |> Option.map (fun effect -> Game.time + effect.ticksRemaining)

/// One room we can see, read whole: everything vision pays for, filed under
/// this room's name and narrowed by nothing (ADR 0052 decision 1). What the
/// engine does not scope to the room — our own creeps, out of the world-wide
/// `Game.creeps` — is scoped by hand here; terrain and spawns are handed in,
/// the first needing no vision and the second swept once for the tick. It holds
/// **every creep of ours standing here**, not one colony's: which of them a
/// colony holds is that colony's own cut (`ColonyView.ofWorld`, which files the
/// rest under `Foreign`). `ours` is the name the engine spells this player,
/// read in `ofGame`: whose a reservation is, is a comparison against it, so
/// Core is handed the answer rather than the two names (ADR 0042).
let private seenFacts
    (ours: string option)
    (terrain: RoomTerrain)
    (spawns: SpawnInfo list)
    (standing: ICreep list)
    (casting: ICreep list)
    (room: IRoom)
    : RoomFacts =
    // Each structure and site is classified once here and carried beside
    // its kind, so the engine string is interpreted in one place (#75).
    let structures =
        room.find findStructures
        |> Array.map (fun o ->
            let st = o :?> IStructure
            st, builtKindOf st.structureType)

    // The structures whose store enters the projection (ADR 0012, ADR 0023),
    // swept once: the energy table below and the Thorium table beside it ask
    // the same question of the same array, and two sweeps are two answers free
    // to disagree the day the predicate moves.
    let storedStructures = structures |> Array.filter (fun (_, kind) -> isStored kind)

    let sites =
        room.find findMyConstructionSites
        |> Array.map (fun o ->
            let site = o :?> IConstructionSite
            site, builtKindOf site.structureType)

    // Everybody else's sites standing here, as tiles and nothing more (#248).
    // No kind is classified and no id is kept: the engine takes one
    // construction site per tile whoever placed it, and refusing to ask for a
    // site under one is the only thing a decision can do about it
    // (`RoomLayer.RivalSites`). Every other reader of a site — the Build pool,
    // the gap rule's allowance, the Post a container site raises, the rampart
    // that covers it — asks a question that presumes the site is ours, and the
    // census above is what answers those.
    let rivalSites =
        room.find findHostileConstructionSites
        |> Array.map (fun o -> posOf (o :?> IConstructionSite).pos)

    // The structures we own here, classified once. Their **ids** are what
    // the kinds that ask for an owner are checked against (`needsOwner`,
    // ADR 0034) — FIND_STRUCTURES carries every owner's — and the
    // energy-hungry ones among them are the room's Refillables.
    let mine =
        room.find findMyStructures
        |> Array.map (fun o ->
            let st = o :?> IStructure
            st, builtKindOf st.structureType)

    let ourIds = mine |> Array.map (fun (st, _) -> st.id) |> Set.ofArray

    let sources = room.find findSources |> Array.map (fun o -> o :?> ISource)

    // Dropped energy piles: position, kind and amount, which is what the
    // Pickup Task's threshold and its capacity are read off (#167).
    let dropped =
        room.find findDroppedResources
        |> Array.map (fun o -> o :?> IResource)
        |> Array.filter (fun r -> r.resourceType = "energy")

    // The stores with a clock on them (#167): a dead creep's tombstone and a
    // destroyed structure's ruin, projected as one kind because a Withdraw
    // reads the same three facts off either. Energy only, and only while there
    // is some: an empty tombstone is a target no rule can answer for, and
    // projecting one is a hundred ticks of churn in every id-keyed table.
    let tombstones =
        Array.append (room.find findTombstones) (room.find findRuins)
        |> Array.map (fun o -> o :?> ITombstone)
        |> Array.filter (fun r -> r.store.getUsedCapacity "energy" > 0)

    // The season's Thorium deposits, and only those (ADR 0057 decision 1).
    // The mod stands an ordinary-ore mineral in the same room and the colony
    // never extracts it — there is no market this season — so it is filtered
    // out here, where every other engine string is classified, and the Core's
    // `TargetKind.Mineral` carries no resource because only one kind ever
    // reaches it. An exhausted deposit is deleted by the mod outright, so a
    // mineral that leaves this array is a deposit that is gone and not one at
    // zero.
    let minerals =
        room.find findMinerals
        |> Array.map (fun o -> o :?> IMineral)
        |> Array.filter (fun m -> m.mineralType = resourceName Thorium)

    // The controller travels through FIND_STRUCTURES on live servers, but
    // is projected explicitly so nothing depends on that detail.
    let controllers =
        if isNull (box room.controller) then
            [||]
        else
            [| room.controller |]

    let controller =
        if Array.isEmpty controllers then
            None
        else
            Some controllers.[0]

    {
        Layer =
            {
                Terrain = terrain.Ground
                TargetPositions =
                    Map.ofArray (
                        Array.concat
                            [
                                sources |> Array.map (fun s -> s.id, posOf s.pos)
                                structures |> Array.map (fun (st, _) -> st.id, posOf st.pos)
                                sites |> Array.map (fun (site, _) -> site.id, posOf site.pos)
                                controllers |> Array.map (fun c -> c.id, posOf c.pos)
                                dropped |> Array.map (fun r -> r.id, posOf r.pos)
                                tombstones |> Array.map (fun r -> r.id, posOf r.pos)
                                minerals |> Array.map (fun m -> m.id, posOf m.pos)
                            ]
                    )
                // This room's creeps, not the world's — the scope rides on the
                // argument, `ofGame` having grouped the one sweep by room. A
                // layer keyed by room name may hold only the tiles of the room
                // it is filed under (ADR 0041): a creep standing elsewhere
                // filed here under that room's coordinates is a phantom
                // occupant the Resolver arbitrates against (ADR 0001). A creep
                // the projection cannot place is ADR 0004's absence, which is
                // what `Atlas.placedCreeps` already answers.
                CreepPositions = standing |> List.map (fun c -> c.name, posOf c.pos) |> Map.ofList
                // Structures a creep cannot stand on block their tile; the
                // kinds it can are the Core's own predicate (Screeps
                // OBSTACLE_OBJECT_TYPES).
                Obstacles =
                    Set.ofArray (
                        Array.concat
                            [
                                structures
                                |> Array.filter (fun (_, kind) -> not (isWalkable kind))
                                |> Array.map (fun (st, _) -> posOf st.pos)
                                // The engine refuses to move a creep onto
                                // its own obstacle-type construction site,
                                // so those tiles block exactly like the
                                // finished structure would.
                                sites
                                |> Array.filter (fun (_, kind) -> not (isWalkable kind))
                                |> Array.map (fun (site, _) -> posOf site.pos)
                                controllers |> Array.map (fun c -> posOf c.pos)
                                // A mineral is one of Screeps'
                                // OBSTACLE_OBJECT_TYPES, exactly as the
                                // controller beside it is. The season's
                                // deposits stand on wall tiles, so this
                                // subtracts nothing today; it is here because
                                // it is what the engine does.
                                minerals |> Array.map (fun m -> posOf m.pos)
                            ]
                    )
                // Built roads only: a road construction site is not yet a
                // road, so it never enters the pricing (ADR 0010).
                Roads =
                    structures
                    |> Array.filter (fun (_, kind) -> kind = BuiltKind.Road)
                    |> Array.map (fun (st, _) -> posOf st.pos)
                    |> Set.ofArray
                // Tiles and no kind, and deliberately not in `Obstacles`
                // above: the engine blocks a creep on an obstacle-type site
                // its own owner placed, and a hostile creep that walks onto
                // one destroys it, so a rival's site prices nothing (#248).
                RivalSites = Set.ofArray rivalSites
            }
        // The border ring of the room, under its own name: the Atlas
        // answers a Seam from these and from nothing else (ADR 0041).
        Border = terrain.Border
        // Same array order as the layer's TargetPositions, so a controller
        // that also travels through FIND_STRUCTURES resolves to Controller
        // both times.
        TargetKinds =
            Map.ofArray (
                Array.concat
                    [
                        sources |> Array.map (fun s -> s.id, Source)
                        structures |> Array.map (fun (st, kind) -> st.id, Structure kind)
                        sites |> Array.map (fun (site, kind) -> site.id, Site kind)
                        controllers |> Array.map (fun c -> c.id, Controller)
                        dropped |> Array.map (fun r -> r.id, Dropped)
                        // A tombstone stands on the tile its creep died on and
                        // a ruin where its structure stood.
                        tombstones |> Array.map (fun r -> r.id, Tombstone)
                        minerals |> Array.map (fun m -> m.id, Mineral)
                    ]
            )
        // Hits on the repairable kinds only — the decaying roads and containers
        // (ADR 0010, ADR 0012), the Keep and our own ramparts (ADR 0034):
        // fields nobody decides on stay out.
        Hits =
            structures
            |> Array.filter (fun (st, kind) ->
                (wholeLine kind).IsSome && (not (needsOwner kind) || Set.contains st.id ourIds))
            |> Array.map (fun (st, _) -> st.id, { Hits = st.hits; HitsMax = st.hitsMax })
            |> Map.ofArray
        // Stored energy on the containers, the stock the logistics Tasks judge
        // one by (ADR 0012), and on the Storage, which the Planner reads the
        // same way (ADR 0023). The two transient stores ride the same table
        // (#167): a tombstone's or a ruin's energy, which a Withdraw draws
        // exactly as it draws a container's, and a pile's amount, which decides
        // whether the pile is worth a Task at all.
        Stores =
            Array.concat
                [
                    storedStructures
                    |> Array.map (fun (st, _) -> st.id, st.store.getUsedCapacity "energy")
                    tombstones |> Array.map (fun r -> r.id, r.store.getUsedCapacity "energy")
                    dropped |> Array.map (fun r -> r.id, r.amount)
                ]
            |> Map.ofArray
        // The Thorium beside it (ADR 0057 decision 3): what each store holds of
        // it, and the deposit's own remaining amount, which is the fact the
        // miner row's quota reads. A second map and never a resource key inside
        // `Stores`, for the reason the field's own comment gives. Absent per
        // entry (ADR 0004): a store holding none of it has no entry, so a
        // colony with no deposit carries an empty map rather than a table of
        // zeroes.
        Thorium =
            Array.concat
                [
                    storedStructures
                    |> Array.map (fun (st, _) ->
                        st.id, st.store.getUsedCapacity (resourceName Thorium))
                    minerals |> Array.map (fun m -> m.id, m.mineralAmount)
                ]
            |> Array.filter (fun (_, held) -> held > 0)
            |> Map.ofArray
        // The extractor's cooldown (ADR 0057 decision 2): `EXTRACTOR_COOLDOWN`
        // is 5 and the intent pass runs before the object pass, so successive
        // harvests land six ticks apart and the other five are refused. Read
        // off the one kind that carries one — `cooldown` is undefined on every
        // other structure the colony builds — and 0 is a real answer here,
        // meaning "this tick", which is why the map is not filtered the way the
        // Thorium above is.
        Cooldowns =
            structures
            |> Array.filter (fun (_, kind) -> kind = BuiltKind.Extractor)
            |> Array.map (fun (st, _) -> st.id, st.cooldown)
            |> Map.ofArray
        // Who holds the room, home included (ADR 0042). A seen room with
        // no controller at all gets a truthful entry: nobody owns or
        // reserves it, which is the neutral rate and not an unknown.
        Control =
            Some(
                match controller with
                | None ->
                    {
                        Owner = Ownership.Unowned
                        Reservation = None
                        SafeMode = false
                    }
                | Some c ->
                    {
                        // `safeMode` is the tick count remaining and
                        // undefined otherwise.
                        SafeMode = not (isNull (box c.safeMode))
                        // `my` is undefined and not false on a controller
                        // nobody owns, so ours is asked first and off
                        // `my`. `owner` separates the other two: an owner
                        // that is not us is a rival's, and none at all is
                        // unowned and reservable, which is every outpost a
                        // colony works (ADR 0042, ADR 0043).
                        Owner =
                            if not (isNull (box c.my)) && c.my then Ownership.Ours
                            elif isNull (box c.owner) then Ownership.Unowned
                            else Ownership.Rival
                        Reservation =
                            if isNull (box c.reservation) then
                                None
                            else
                                // Three holders and not two: ADR 0043
                                // reads different answers off the two that
                                // are not ours — the NPC's reservation is
                                // the clock a core's stand-down runs to
                                // under a floor, a player's is a stand-down
                                // clocked to the hold itself (#165).
                                let holder =
                                    if Some c.reservation.username = ours then
                                        ReservationHolder.Ours
                                    elif c.reservation.username = invaderUsername then
                                        ReservationHolder.Invader
                                    else
                                        ReservationHolder.Rival

                                Some
                                    {
                                        Holder = holder
                                        TicksToEnd = c.reservation.ticksToEnd
                                    }
                    }
            )
        // The controller **while it is ours**: the downgrade clock and the
        // banked safe modes are undefined on a controller we do not own, so a
        // room a rival holds carries its ownership in `Control` and no
        // controller here (ADR 0004).
        Controller =
            controller
            |> Option.filter (fun c -> not (isNull (box c.my)) && c.my)
            |> Option.map (fun c ->
                {
                    Id = c.id
                    Level = c.level
                    TicksToDowngrade = c.ticksToDowngrade
                    SafeModeAvailable = c.safeModeAvailable
                    // `safeMode` is the tick count remaining, undefined
                    // when safe mode is off.
                    SafeModeActive = not (isNull (box c.safeMode))
                }
                : ControllerInfo)
        Energy =
            {
                Available = room.energyAvailable
                Capacity = room.energyCapacityAvailable
            }
        Spawns = spawns
        // The bodies still gestating in this room's ovens (#156).
        Casting =
            casting
            |> List.map (fun c ->
                c.body |> Array.map (fun p -> bodyPartOf p.``type``) |> Array.toList)
        Refillables =
            mine
            |> Array.filter (fun (_, kind) -> isRefillable kind)
            |> Array.map (fun (st, kind) ->
                {
                    Id = st.id
                    FreeCapacity = st.store.getFreeCapacity "energy"
                    Kind = kind
                }
                : RefillableInfo)
            |> Array.toList
        // The room's rocks as vision answered for them. A source holding
        // energy restocks in zero ticks (ADR 0025) whatever its
        // regeneration timer reads; the timer is read only for a drained
        // source, and is undefined until the engine starts it.
        Sources =
            sources
            |> Array.map (fun s ->
                {
                    Id = s.id
                    TicksToRestock =
                        if s.energy > 0 || isNull (box s.ticksToRegeneration) then
                            0
                        else
                            s.ticksToRegeneration
                }
                : SourceInfo)
            |> Array.toList
        // Our sites standing here (#150): the Build pool is a colony's share of
        // these one to one (`Decide.planTasks`), so a site missing from it is a
        // site no creep is ever sent to — and ADR 0042 makes a standing
        // container the switch that admits an outpost into the economy.
        ConstructionSites =
            sites
            |> Array.map (fun (site, _) -> ({ Id = site.id }: ConstructionSiteInfo))
            |> Array.toList
        // The hostiles standing here (ADR 0033, #201). Read for every room the
        // world can see and not the spawn rooms' alone: a Threat's Reach gates
        // the Tasks whose Work Area lies in it, a creep standing in one is
        // matched to Flee, and a spawn whose doorstep is in one holds.
        Hostiles =
            room.find findHostileCreeps
            |> Array.map (fun o ->
                let c = o :?> ICreep

                {
                    Id = c.id
                    Owner = c.owner.username
                    // The room being scanned and not the creep's own field: a
                    // hostile is found *in* this room, which is what places it.
                    Pos = RoomPos.at room.name (posOf c.pos)
                    Body = c.body |> Array.map (fun p -> bodyPartOf p.``type``) |> Array.toList
                    TicksToLive = c.ticksToLive
                }
                : HostileInfo)
            |> Array.toList
        // The invader cores standing here (ADR 0043). Not folded into
        // `Hostiles`: a core is a *structure*, so `FIND_HOSTILE_CREEPS` cannot
        // answer with one, and the two lists answer different questions — a
        // raider is something a creep runs from this tick, a core is something
        // a whole room is withheld from for thousands.
        // `FIND_HOSTILE_STRUCTURES` answers with every structure a rival owns,
        // so the kind is checked here.
        InvaderCores =
            room.find findHostileStructures
            |> Array.map (fun o -> o :?> IStructure)
            |> Array.filter (fun st -> st.structureType = structureInvaderCore)
            |> Array.map (fun st ->
                ({
                    RoomName = room.name
                    CollapseTick = collapseTickOf st
                }
                : InvaderCoreInfo))
            |> Array.toList
    }

/// One room of the world as the engine hands it back this tick, or None
/// where we have no vision in it. `Game.rooms` holds only the rooms we can
/// see, so a missing key is exactly "no vision" — and this is the one place
/// that says so.
let private roomSeen (roomName: string) : IRoom option =
    let room = objectItem<IRoom> Game.rooms roomName

    if isNull (box room) then None else Some room

/// One room's facts. Terrain comes off the memo whether or not we can see the
/// room: `Game.map.getRoomTerrain` answers for any room, needs no vision and
/// never goes stale (ADR 0031, ADR 0041), so the terrain layer's marginal cost
/// across rooms is zero. Everything else comes off `Game.rooms`, which holds
/// only the rooms we have vision in — so the half vision pays for is absent
/// entry by entry until vision returns (ADR 0004) rather than a "blind" state
/// anything models: unplaced geometry is unpriceable, enters no Task and blocks
/// no action.
let private factsOf
    (ours: string option)
    (spawns: SpawnInfo list)
    (standing: ICreep list)
    (casting: ICreep list)
    (roomName: string)
    : RoomFacts =
    let terrain = terrainOf roomName

    match roomSeen roomName with
    | None ->
        { RoomFacts.empty with
            Layer =
                { RoomLayer.empty with
                    Terrain = terrain.Ground
                }
            Border = terrain.Border
            // A spawn of ours stands in a room we can see, so this list is
            // empty here in every world the engine can build; it is filed
            // from the same sweep as the seen half.
            Spawns = spawns
        }
    | Some room -> seenFacts ours terrain spawns standing casting room

/// The rooms the world holds facts for this tick: every room the engine
/// answered `Game.rooms` with — which is every room we can see — and, beside
/// them, the rooms a **standing** colony's declaration names, whose terrain and
/// furniture need no vision at all (ADR 0041). The union and not one colony's
/// scan set, which is the whole difference between a world and a projection
/// (ADR 0052 decision 1): the [[stand-down]] gate (ADR 0043) and the bootstrap
/// rule narrow what a *colony* works (`ColonyView.ofWorld`), and narrowing the
/// world by them would put the shell in the business of deciding which rooms
/// matter. The price is that a room we can **see** and no colony works — a
/// [[stand-down]]'s withheld outpost with one of our creeps still walking out
/// of it — costs the full `seenFacts` sweep; it is bounded by the rooms our own
/// bodies stand in, since vision is what `Game.rooms` is.
let private worldRooms (maxHops: int) (colonies: Colony list) (seen: string list) : string list =
    let declared =
        colonies
        |> List.filter (fun colony -> List.contains colony.Home seen)
        // The colony's own projection set and not the declaration read a
        // second time: it is home, the outposts and the transit rooms a chain
        // to one of them crosses (ADR 0058), and a room the view will project
        // is a room the world has to hold terrain for. Terrain is what a
        // transit room is for and terrain is free of vision (`terrainOf`), so
        // the marginal cost of one here is a memo read.
        //
        // Narrowed by the hop budget exactly as `World.scanOf` narrows it, and
        // for a reason the budget's own rule gives: a declaration past it is
        // refused, so no view projects that room — and reading the transit
        // rectangle of one anyway would drag every room between here and a
        // mis-declaration into the world for nobody to use.
        |> List.collect (fun colony ->
            let outposts =
                colony.Outposts |> List.filter (Outpost.withinHopBudget maxHops colony.Home)

            // The rooms a child of this colony's would be projected through,
            // off the names alone: which of them the view actually borrows
            // turns on a [[stage]] this function cannot read — the stages are
            // derived from the world it is choosing the rooms for — so the
            // declaration's shape is what is read here, and a room the view
            // does not borrow costs the memo read a transit room costs
            // (`Colony.roomsProjected`, ADR 0058). Without it a claimed
            // nursery two hops out is projected with no chain to it, which is
            // the state W15S28 was found in on 2026-09-10.
            let children =
                colonies
                |> List.filter (fun child ->
                    child.Mother = Some colony.Home
                    && child.Home <> colony.Home
                    && RoomName.hopsBetween colony.Home child.Home
                       |> Option.exists (fun hops -> hops <= maxHops))
                |> List.collect (fun child ->
                    child.Home :: RoomName.transitBetween colony.Home child.Home)

            Outpost.roomsProjected outposts colony.Home @ children)

    seen @ declared |> List.distinct

/// This tick's World (ADR 0052 decision 1): every room we declared or can see,
/// under its own name, and every creep we own beside them. The one place the
/// bot reads `Game`. The declaration is handed in rather than read off the
/// constant (ADR 0041), so a harness or a test can hand this function a world
/// of its own.
let ofGame (maxHops: int) (colonies: Colony list) (lastPositions: Map<string, RoomPos>) : World =
    let spawns = objectValues<ISpawn> Game.spawns

    // The name the engine spells us, off the controller of a room one of
    // our spawns stands in — a spawn cannot stand in a room we do not own,
    // so that owner is us. Read once for the world: there is one of us.
    let ours =
        spawns
        |> Array.tryPick (fun s ->
            let c = s.room.controller

            if isNull (box c) || isNull (box c.owner) then
                None
            else
                Some c.owner.username)

    // Our spawns grouped by the room they stand in, swept once: which of
    // them a colony casts from is its own cut (`ColonyView.ofWorld`).
    let spawnsByRoom =
        spawns
        |> Array.map (fun s ->
            s.room.name,
            {
                Name = s.name
                Id = s.id
                RoomName = s.room.name
                IsSpawning = not (isNull s.spawning)
            })
        |> Array.toList
        |> List.groupBy fst
        |> List.map (fun (room, entries) -> room, entries |> List.map snd)
        |> Map.ofList

    // Every creep we own, swept once for the tick and grouped by the room it
    // stands in, the standing apart from the still-gestating (#156) — two facts
    // a room is asked for and one traversal for both. The scope is ADR 0041's:
    // a layer keyed by room name may hold only the tiles of the room it is
    // filed under, and an argument states that where a predicate at each
    // reader would only promise it. `List.groupBy` keeps the engine's own
    // order within a room, which `World.creepColonies` reads (ADR 0047
    // decision 2).
    let standing, casting =
        objectValues<ICreep> Game.creeps
        |> Array.toList
        |> List.partition (fun c -> not c.spawning)

    let byRoom (creeps: ICreep list) =
        creeps |> List.groupBy (fun c -> c.room.name) |> Map.ofList

    let standingByRoom = byRoom standing
    let castingByRoom = byRoom casting

    let inRoom (grouped: Map<string, ICreep list>) roomName =
        Map.tryFind roomName grouped |> Option.defaultValue []

    // The rooms vision answered for this tick — `Game.rooms` is exactly that
    // (`roomSeen`) — read once and used twice: it decides which rooms the
    // world holds facts for, and which of them this tick may stamp a sighting
    // for (#151).
    let seen = objectEntries Game.rooms |> Array.map fst |> Array.toList

    let rooms =
        worldRooms maxHops colonies seen
        |> List.map (fun roomName ->
            roomName,
            factsOf
                ours
                (Map.tryFind roomName spawnsByRoom |> Option.defaultValue [])
                (inRoom standingByRoom roomName)
                (inRoom castingByRoom roomName)
                roomName)

    {
        Time = Game.time
        Rooms = Map.ofList rooms
        // This tick's sighting for every room this tick could see, and none
        // for the rest (#151): the ids of the room's own kind census and not
        // the kinds, which is all the grace asks and so all the sighting
        // carries (ADR 0007). The rooms it does *not* cover are the ones
        // `World.recalling` fills from the previous tick's map — the merge is
        // Core's, so the only thing this reads out of `Game` is which rooms
        // answered.
        Sightings =
            rooms
            |> List.filter (fun (roomName, _) -> List.contains roomName seen)
            |> List.map (fun (roomName, facts) ->
                roomName,
                ({
                    Tick = Game.time
                    Targets = facts.TargetKinds |> Map.toList |> List.map fst |> Set.ofList
                }
                : RoomSighting))
            |> Map.ofList
        // Every creep we own that is not still gestating, in the engine's
        // own order — whose each of these is this tick is
        // `World.creepColonies`' answer (ADR 0047 decision 2).
        Creeps =
            standing
            |> List.map (fun c ->
                {
                    Room = c.room.name
                    Info =
                        {
                            Name = c.name
                            TicksToLive = c.ticksToLive
                            Fatigue = c.fatigue
                            Hits = { Hits = c.hits; HitsMax = c.hitsMax }
                            Energy = c.store.getUsedCapacity "energy"
                            FreeCapacity = c.store.getFreeCapacity "energy"
                            Body =
                                // The parts still standing, and never the parts
                                // it was cast with (#270): the engine destroys
                                // them from the head of the body and leaves
                                // them in the array reading zero hits. Live, a
                                // guard whose three Attack parts were gone went
                                // on being counted as three, so it read as a
                                // Fighter, kept the Guard its body could not
                                // perform, and answered ERR_NO_BODYPART every
                                // tick while the raid it was hired for went on
                                // untouched.
                                c.body
                                |> Array.toList
                                |> List.filter (fun p -> p.hits > 0)
                                |> List.map (fun p -> bodyPartOf p.``type``)
                                |> partsOf
                            Moved =
                                match Map.tryFind c.name lastPositions with
                                | Some last -> last <> tileOf c
                                | None -> false
                        }
                }
                : WorldCreep)
    }

/// Where every creep of ours stands this tick, for next tick's
/// `CreepInfo.Moved` (#225). Here and not at the Memory boundary that writes it
/// because this module is the only code that reads the game's objects: the same
/// sweep and the same gestating filter `ofGame`'s own `Creeps` uses, so the two
/// ends of that loop cannot part.
let positions () : (string * RoomPos) list =
    objectValues<ICreep> Game.creeps
    |> Array.filter (fun c -> not c.spawning)
    |> Array.map (fun c -> c.name, tileOf c)
    |> Array.toList
