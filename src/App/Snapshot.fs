// Builds the tick's Snapshot projection from the live game objects.
module Fabot.Snapshot

open Fabot.Bindings
open Fabot.Core.Types

/// Classify one tile of engine terrain into the Core's three states.
let private terrainAt (terrain: ITerrain) x y =
    let mask = terrain.get (x, y)

    if mask &&& terrainMaskWall <> 0 then Wall
    elif mask &&& terrainMaskSwamp <> 0 then Swamp
    else Plain

let private posOf (p: IRoomPosition) : Pos = { X = p.x; Y = p.y }

/// Classify an engine part-type string into the Core's body vocabulary:
/// the reverse of the Core's one part-name table. The engine's part set
/// is closed, so the fallback for an unmatched string is unreachable;
/// Tough keeps the classification total without inventing a case.
let private bodyPartOf =
    let byName = allBodyParts |> List.map (fun p -> partName p, p) |> Map.ofList
    fun partType -> byName |> Map.tryFind partType |> Option.defaultValue Tough

/// Classify an engine STRUCTURE_* string into the Core's built kinds: the
/// reverse of the Core's one kind-name table, exactly as `bodyPartOf` is
/// the reverse of `partName`. A string the table lacks is a kind the
/// decision layer has no rules for, which is what Other says. Classify
/// once here; every filter below reads the kind, never the string, so the
/// rules over it stay in Core where a test can pin them (#75).
let private builtKindOf =
    let byName = allBuiltKinds |> List.map (fun k -> builtKindName k, k) |> Map.ofList
    fun structureType -> byName |> Map.tryFind structureType |> Option.defaultValue BuiltKind.Other

/// One room's terrain as the engine spells it — the whole fifty-by-fifty
/// grid, in the two views the projection assembles from it. One engine
/// read behind both, so there is one terrain truth per room and not two
/// (ADR 0041): what differs is which window a reader is entitled to, and
/// that is the projection's rule rather than the engine's.
///
/// The split happens here, at memo fill, rather than where `SpatialInfo`
/// is assembled. Assembly runs every tick, and cutting a fifty-by-fifty
/// map into these two there would rebuild 2304 `Pos`-keyed entries per
/// room per tick — precisely the structural comparison ADR 0031 measured
/// at about a quarter of an 8 ms tick and exists to delete, which is why
/// ADR 0041's own Consequences say the terrain memo needs no change at
/// all. Both windows come off one `getRoomTerrain` read and are disjoint,
/// so this is one terrain truth cut once, not two kept in step.
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
/// stale: the first tick that projects a room reads the engine's terrain
/// once and every later tick recalls the same maps. Heap state only, like
/// the plan memo (ADR 0017) — nothing here reaches Memory, and a global
/// reset empties the table so the next tick rebuilds it.
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
                // teleports the creep into the next room. They stay out of
                // the projection's ground: an absent tile is impassable, so
                // no path or Work Area ever uses an exit. Across rooms the
                // trim is more right than it was single-room, not less
                // (ADR 0041): the Matcher now ranks Tasks in a neighbouring
                // room, and an exit admitted as ordinary ground would be a
                // Seat or standing candidate the engine empties the tick a
                // creep reaches it. Do not "fix" this trim.
                Ground =
                    Map.ofList
                        [
                            for x in 1..48 do
                                for y in 1..48 do
                                    { X = x; Y = y }, terrainAt terrain x y
                        ]
                // The same read's other window: the ring the trim drops,
                // kept beside the ground rather than inside it, because a
                // Seam is a pair of rooms joined at a tile and never a tile
                // to stand on (ADR 0036, ADR 0041).
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

/// What one scanned room puts into the projection: its geometry and its
/// border ring, both filed under its own name, beside its share of the
/// three id-keyed tables, which stay unlayered because an object id is
/// already unique across the world (ADR 0041). A record rather than a
/// whole `SpatialInfo` per room, because merging projections would have to
/// decide which of them names the home room and there is only one answer:
/// the spawn's.
type private RoomProjection =
    {
        Layer: RoomLayer
        Border: Map<Pos, Terrain>
        TargetKinds: Map<string, TargetKind>
        Hits: Map<string, HitsInfo>
        Stores: Map<string, int>
    }

/// One room we can see, projected: the half of a room's projection that
/// vision pays for. Most of it comes off the `room.find` families, and
/// what does not — the controller, a structure's store, our own creeps
/// out of the world-wide `Game.creeps` — is scoped to this room by hand
/// where the engine does not scope it, which is what the creep filter
/// below is for and why it is not redundant. Its terrain is handed in
/// rather than read here, because the half that needs no vision is the
/// same read either way.
let private projectVisible (terrain: RoomTerrain) (room: IRoom) : RoomProjection =
    // Each structure and site is classified once, here, and carried beside
    // its kind: every filter below reads that kind, and the engine string
    // is interpreted in one place (#75).
    let structures =
        room.find findStructures
        |> Array.map (fun o ->
            let st = o :?> IStructure
            st, builtKindOf st.structureType)

    let sites =
        room.find findMyConstructionSites
        |> Array.map (fun o ->
            let site = o :?> IConstructionSite
            site, builtKindOf site.structureType)

    // The ids of the structures we own — what the kinds that ask for an
    // owner are checked against (`needsOwner`, ADR 0034): FIND_STRUCTURES
    // carries every owner's, and a rampart is the one projected kind whose
    // ownership changes the answer.
    let ours =
        room.find findMyStructures
        |> Array.map (fun o -> (o :?> IStructure).id)
        |> Set.ofArray

    let sources = room.find findSources |> Array.map (fun o -> o :?> ISource)

    // Dropped energy piles: position, kind, and — since #167 — the amount,
    // which is what the Pickup Task's threshold and its capacity are read
    // off. The reflex still reads none of it (ADR 0007's rule that the
    // field list grows the tick a decision reads one: this is that tick).
    let dropped =
        room.find findDroppedResources
        |> Array.map (fun o -> o :?> IResource)
        |> Array.filter (fun r -> r.resourceType = "energy")

    // The stores with a clock on them (#167): a dead creep's tombstone and
    // a destroyed structure's ruin, projected as one kind because a
    // Withdraw reads the same three facts off either.
    //
    // Energy only, as the piles above are — the colony holds no other
    // resource and has no Task that would spend one — and only while there
    // is some: an empty tombstone is a store that says nothing, and
    // projecting one would put a target in the census that no rule can
    // ever answer for. The Core pool gates on stock as well (`planTasks`
    // pools only stocked stores), so this filter buys quiet rather than
    // correctness: a hundred ticks of a spent tombstone in `TargetKinds`
    // is churn in every id-keyed table for a target nothing will ever
    // take.
    let tombstones =
        Array.append (room.find findTombstones) (room.find findRuins)
        |> Array.map (fun o -> o :?> ITombstone)
        |> Array.filter (fun r -> r.store.getUsedCapacity "energy" > 0)

    // The controller travels through FIND_STRUCTURES on live servers, but
    // is projected explicitly so nothing depends on that detail.
    let controllers =
        if isNull (box room.controller) then
            [||]
        else
            [| room.controller |]

    // This room's geometry, under this room's name: the one shape the
    // projection has since ADR 0041's contract step, so what the shell
    // assembles and what every reader reads are the same record and not a
    // pair kept in step.
    let layer: RoomLayer =
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
                        ]
                )
            // This room's creeps, not the world's: `Game.creeps` is every
            // creep we own wherever it stands, and a layer keyed by room
            // name may only hold the tiles of the room it is filed under
            // (ADR 0041). Every other field here is already room-scoped
            // through `room.find`; without the same scope on this one, a
            // creep standing in another room would be filed at home under
            // that room's coordinates — a phantom occupant the Resolver
            // arbitrates against (ADR 0001) and a tile the Raid log
            // measures a raider's closest approach to. A creep the
            // projection cannot place is ADR 0004's absence, which is the
            // answer `Atlas.placedCreepsByRoom` already gives it: it is in
            // no group, so no room's geometry is measured against it.
            // Load-bearing rather than defensive since #142: the mover now
            // aims a creep matched across a border at an exit tile, and the
            // engine puts it down on the neighbour's border row for the
            // next tick to read, so `Game.creeps` really does report one of
            // ours outside this room and this filter is what files it under
            // the room it stands in. #126 puts three rooms in the scan set
            // and this call still projects one of them, so the filter
            // answers for every other room whether or not the colony has
            // anyone out there this tick — do not simplify it away as a
            // dead branch.
            CreepPositions =
                objectValues<ICreep> Game.creeps
                |> Array.filter (fun c -> not c.spawning && c.room.name = room.name)
                |> Array.map (fun c -> c.name, posOf c.pos)
                |> Map.ofArray
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
                            // The engine refuses to move a creep onto its
                            // own obstacle-type construction site, so those
                            // tiles block exactly like the finished
                            // structure would.
                            sites
                            |> Array.filter (fun (_, kind) -> not (isWalkable kind))
                            |> Array.map (fun (site, _) -> posOf site.pos)
                            controllers |> Array.map (fun c -> posOf c.pos)
                        ]
                )
            // Built roads only: a road construction site is not yet a road,
            // so it never enters the pricing (ADR 0010).
            Roads =
                structures
                |> Array.filter (fun (_, kind) -> kind = BuiltKind.Road)
                |> Array.map (fun (st, _) -> posOf st.pos)
                |> Set.ofArray
        }

    {
        Layer = layer
        // The border ring of the room, under its own name once the caller
        // files it: the Atlas answers a Seam from these and from nothing
        // else (ADR 0041). Since #126 declared W12S27 and W13S28 the
        // projection carries a ring for each of the three scanned rooms,
        // so the home room's two declared joins are answerable; a room
        // outside the scan set has no ring here and answers no Seam at
        // all, which is ADR 0004's per-entry absence and not a special
        // case.
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
                        // A tombstone stands on the tile its creep died
                        // on, which may well be a Seat or a Post, and a
                        // ruin on the tile its structure stood on. Neither
                        // joins `Obstacles`: the engine lets a creep walk
                        // over both, and a transient tile-blocker would
                        // move every price the room answers for a hundred
                        // ticks (#167).
                        tombstones |> Array.map (fun r -> r.id, Tombstone)
                    ]
            )
        // Hits on the repairable kinds only — the decaying roads and
        // containers (ADR 0010, ADR 0012), the Keep and our own ramparts
        // (ADR 0034): fields nobody decides on stay out, and the Repair
        // pool and the Raid log's damage decide on these (the safe-mode
        // arm is the third reader, #102). Which line each kind is whole at
        // and which kind wants an owner are both Core's; asking the engine
        // who owns one is the shell's.
        Hits =
            structures
            |> Array.filter (fun (st, kind) ->
                (wholeLine kind).IsSome && (not (needsOwner kind) || Set.contains st.id ours))
            |> Array.map (fun (st, _) -> st.id, { Hits = st.hits; HitsMax = st.hitsMax })
            |> Map.ofArray
        // Stored energy on the containers, the stock the logistics Tasks
        // judge one by (ADR 0012), and on the Storage, which the Planner
        // reads the same way (ADR 0023): its Refill tier pools a Storage
        // with room and drops a full one, and its Withdraw tier drops an
        // empty one and pools a stocked one only while some sink other
        // than the stock still has room to take the load.
        //
        // The two transient stores ride the same table (#167): a
        // tombstone's or a ruin's energy, which a Withdraw draws exactly
        // as it draws a container's, and a pile's amount, which is what
        // decides whether the pile is worth a Task at all. A pile carries
        // a bare `amount` rather than a store, so it is the one entry read
        // off a field instead of a `getUsedCapacity` call.
        Stores =
            Array.concat
                [
                    structures
                    |> Array.filter (fun (_, kind) -> isStored kind)
                    |> Array.map (fun (st, _) -> st.id, st.store.getUsedCapacity "energy")
                    tombstones |> Array.map (fun r -> r.id, r.store.getUsedCapacity "energy")
                    dropped |> Array.map (fun r -> r.id, r.amount)
                ]
            |> Map.ofArray
    }

/// The absolute tick a structure's collapse timer runs out at, or None
/// where it carries none (ADR 0043). Two engine shapes to guard: `effects`
/// is `undefined` on an object nothing is applied to, exactly as
/// `safeMode` and `reservation` are, and a deployed core carries other
/// effects beside this one, so the array is searched by id rather than
/// read at an index.
///
/// `Game.time +` is the whole point of this function and not a
/// convenience. The engine's `ticksRemaining` is a **relative** count —
/// the official documentation's "how many ticks will the effect last",
/// confirmed against docs.screeps.com for #133 — while the read-only HTTP
/// API's raw documents carry an absolute `endTime`, and it is the raw
/// API's numbers (`endTime = 170,283` for W15S24) that
/// `docs/research/remote-mining.md` and ADR 0043's prose are written in.
/// Storing the relative count as if it were the absolute tick would put
/// the deadline about a hundred thousand ticks early — a stand-down that
/// expired before it began — and storing the raw API's number would put
/// it a hundred thousand late.
let private collapseTickOf (structure: IStructure) : int option =
    if isNull (box structure.effects) then
        None
    else
        structure.effects
        |> Array.tryFind (fun effect -> effect.effect = effectCollapseTimer)
        |> Option.map (fun effect -> Game.time + effect.ticksRemaining)

/// One scanned room as the engine hands it back this tick, or None where
/// the colony has no vision in it. `Game.rooms` holds only the rooms we
/// can see, so a missing key is exactly "no vision" — and this is the one
/// place that says so, because both halves of the scan read it: the
/// geometry the projection files, and the entity lists the Task pool is
/// built from.
let private roomSeen (roomName: string) : IRoom option =
    let room = objectItem<IRoom> Game.rooms roomName

    if isNull (box room) then None else Some room

/// One room of the scan set, projected. Terrain comes off the memo whether
/// or not we can see the room: `Game.map.getRoomTerrain` answers for any
/// room in the world, needs no vision and never goes stale (ADR 0031, ADR
/// 0041), which is why the terrain layer's marginal cost across rooms is
/// zero. Everything else comes off `Game.rooms`, which holds only the
/// rooms we have vision in this tick.
///
/// So the half a room we cannot see contributes is its terrain, and the
/// half vision pays for — stores, hits, sites, creeps, hostiles, and every
/// structure standing — is absent entry by entry until vision returns (ADR
/// 0004), rather than a "blind" state anything has to model: unplaced
/// geometry is unpriceable, enters no Task and blocks no action.
///
/// It is not the whole of what a declared outpost puts in the projection.
/// The other half is declared rather than seen — the sources' and the
/// controller's ids and tiles — and `Outpost.place` lays it in over the
/// whole assembled projection, once, after this runs (ADR 0041, #148). It
/// is not laid here because the rule is Core's: this function knows only
/// what the engine answered for one room name.
let private projectRoom (roomName: string) : RoomProjection =
    let terrain = terrainOf roomName

    match roomSeen roomName with
    | None ->
        {
            Layer =
                { RoomLayer.empty with
                    Terrain = terrain.Ground
                }
            Border = terrain.Border
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
        }
    | Some room -> projectVisible terrain room

/// The tick's projection: the rooms the colony works, each under its own
/// name, in one projection and never two (ADR 0005, layered by ADR 0041).
/// The scan set is handed in rather than derived here: it is Core's rule
/// (`Outpost.roomsProjected`) and the projection is not its only reader,
/// so `build` takes the union once for the whole tick — the shell decides
/// nothing about which rooms the colony works, it only reads them.
///
/// The three id-keyed tables are merged flat across the scanned rooms,
/// because an object id is already unique across the world and layering it
/// would key a unique thing twice (ADR 0041). Deterministic under a
/// collision that cannot happen: the fold walks the scan set in order, so
/// the last room to name an id would win — and one object id stands in one
/// room, so no id is ever merged twice.
let private buildSpatial (home: string) (scanned: string list) : SpatialInfo =
    let projected = scanned |> List.map (fun roomName -> roomName, projectRoom roomName)

    let mergedBy (select: RoomProjection -> Map<string, 'v>) =
        (Map.empty, projected)
        ||> List.fold (fun acc (_, room) ->
            (acc, select room) ||> Map.fold (fun acc id value -> Map.add id value acc))

    {
        RoomName = Some home
        Rooms = projected |> List.map (fun (name, room) -> name, room.Layer) |> Map.ofList
        Borders = projected |> List.map (fun (name, room) -> name, room.Border) |> Map.ofList
        TargetKinds = mergedBy (fun room -> room.TargetKinds)
        Hits = mergedBy (fun room -> room.Hits)
        Stores = mergedBy (fun room -> room.Stores)
    }

/// The tick's Snapshot, and the one argument it takes: the rooms the
/// [[stand-down]] gate is withholding (ADR 0043), derived by Core from the
/// previous tick's [[raid log]] (`Observe.standDown`) and handed in the
/// way the scan set is — the shell reads which rooms the colony works, it
/// decides none of it. Empty is the ordinary case and the only one this
/// colony has ever run in: no outpost has yet held a core.
let build (shut: Set<string>) : Snapshot =
    let spawns = objectValues<ISpawn> Game.spawns

    let spawnRooms =
        spawns |> Array.map (fun s -> s.room) |> Array.distinctBy (fun r -> r.name)

    // The home room: the first spawn's, the single-colony assumption this
    // shell has always made and ADR 0041 does not touch.
    let home = spawns |> Array.tryHead |> Option.map (fun spawn -> spawn.room.name)

    // The declarations this tick works from, read once: the scan set, the
    // furniture laid into the projection and the rocks pooled for Harvest
    // are three readings of one constant, and a second read is a second
    // constant that can disagree — which is exactly what the stand-down
    // gate (ADR 0043) would narrow one of and not the others. The scan
    // set below is taken from this list and then gates the other two, so
    // the three narrow together or not at all.
    //
    // And this is where the gate lands, on the one read: a room being
    // stood down leaves the declarations here, so it is not scanned, not
    // furnished, and its rocks are not pooled — three consequences of one
    // subtraction (`Outpost.worked`). Everything downstream sees a room
    // nobody declared, which is the semantics ADR 0004 already paid for.
    //
    // The constant is the colonies' now and no longer the outposts'
    // alone (`Colony.declared`, ADR 0047): this colony's own entry is the
    // one whose home is the room the first spawn stands in, and a home
    // nobody declared works no outposts at all — the behaviour the empty
    // declaration shipped with (#124) and the one downstream has a rule
    // for.
    let outposts =
        home
        |> Option.map (Colony.outpostsOf Colony.declared)
        |> Option.defaultValue []
        |> Outpost.worked shut

    // The rooms the colony works this tick — the home room and every
    // declared outpost beside it (ADR 0041). Core owns the union
    // (`Outpost.roomsProjected`) and the shell reads it once here, because
    // the projection is not the only thing built off a room scan: an
    // outpost's Tasks join the *same* pool as the home room's, so the
    // entity lists the Planner pools from are scanned over the same set.
    // Three rooms since #126 filled the declaration: the home room,
    // W12S27 and W13S28.
    let scanned =
        home |> Option.map (Outpost.roomsProjected outposts) |> Option.defaultValue []

    // The scanned rooms we can actually see. What vision pays for is read
    // off these and is absent entry by entry where there is none (ADR
    // 0004); what a declaration carries is read off `outposts` instead and
    // needs no vision at all (ADR 0041).
    let seen = scanned |> List.choose roomSeen

    // The name the engine spells this colony, off the controller of the
    // room its spawns stand in — a spawn cannot stand in a room this
    // colony does not own, so that owner is us. Read here and nowhere
    // else: whose a reservation is, is a comparison against this name,
    // and Core is handed the answer rather than the two names, because
    // asking the engine who anybody is is the shell's half of ADR 0042
    // and the economics is Core's.
    let colonyOwner =
        spawns
        |> Array.tryPick (fun s ->
            let c = s.room.controller

            if isNull (box c) || isNull (box c.owner) then
                None
            else
                Some c.owner.username)

    {
        Time = Game.time
        Spawns =
            spawns
            |> Array.map (fun s ->
                {
                    Name = s.name
                    Id = s.id
                    RoomName = s.room.name
                    IsSpawning = not (isNull s.spawning)
                })
            |> Array.toList
        RoomEnergy =
            spawnRooms
            |> Array.map (fun r ->
                r.name,
                {
                    Available = r.energyAvailable
                    Capacity = r.energyCapacityAvailable
                })
            |> Map.ofArray
        Refillables =
            spawns
            |> Array.collect (fun s -> s.room.find findMyStructures)
            |> Array.map (fun o ->
                let st = o :?> IStructure
                st, builtKindOf st.structureType)
            |> Array.filter (fun (_, kind) -> isRefillable kind)
            |> Array.distinctBy (fun (st, _) -> st.id)
            |> Array.map (fun (st, kind) ->
                {
                    Id = st.id
                    FreeCapacity = st.store.getFreeCapacity "energy"
                    Kind = kind
                }
                : RefillableInfo)
            |> Array.toList
        // Every scanned room's sources, not the home room's: the Harvest
        // pool is built from this list, and ADR 0041 puts an outpost's
        // Harvest in the *same* pool ranked by the *same* order. Vision
        // answers for the rooms it covers and the declaration answers for
        // the rest, joined by `Outpost.pooledSources`, which keeps the
        // engine's own answer where both speak. Until #148 this read off
        // `seen` alone — #124's fourth acceptance criterion, and wrong: a
        // declared source's id and tile are exactly the facts ADR 0041
        // refuses to make vision wait for, and pooling them only where
        // there is vision is the deadlock that ADR's third paragraph
        // exists to break — no Harvest names the outpost, so nothing walks
        // there, so vision never comes. Gated on `scanned`, the same set
        // the projection is built over: a rock pooled for a room the scan
        // left out would be a Task over geometry nothing places — and an
        // unplaced target is not inert, it prices at 0 and wins. What an
        // unposted outpost source is worth to the quotas is not this
        // list's question: it is answered once, in
        // `Decide.workforceTarget`, and the answer is nothing (ADR 0042).
        //
        // The lists beside it stay home-only on purpose. `Refillables`,
        // `Controller` and `RoomEnergy` are about rooms we own, and an
        // outpost is by definition one we do not. Two lists have left that
        // family since: `ConstructionSites` until #150 — a site of ours in
        // an outpost is one nobody would ever build, which is where ADR
        // 0042's switch stuck — and `Hostiles` until #201, a raider in an
        // outpost being exactly the thing ADR 0033's rules were written
        // for. Both read `seen` now, and each says below why that set and
        // not this one's.
        Sources =
            seen
            |> List.collect (fun room -> room.find findSources |> Array.toList)
            |> List.map (fun o ->
                let s = o :?> ISource

                // A source holding energy restocks in zero ticks (ADR
                // 0025), whatever its regeneration timer reads — that
                // guard, not the timer, is what makes the projection
                // right. The timer is read only for a drained source, and
                // is undefined until the engine starts it.
                ({
                    Id = s.id
                    TicksToRestock =
                        if s.energy > 0 || isNull (box s.ticksToRegeneration) then
                            0
                        else
                            s.ticksToRegeneration
                }
                : SourceInfo))
            |> Outpost.pooledSources scanned outposts
        Controller =
            spawns
            |> Array.tryPick (fun s ->
                let c = s.room.controller

                if not (isNull (box c)) && c.my then
                    Some(
                        {
                            Id = c.id
                            Level = c.level
                            TicksToDowngrade = c.ticksToDowngrade
                            SafeModeAvailable = c.safeModeAvailable
                            // `safeMode` is the tick count remaining,
                            // undefined when safe mode is off.
                            SafeModeActive = not (isNull (box c.safeMode))
                        }
                        : ControllerInfo
                    )
                else
                    None)
        // Who holds each room the colony can see this tick, home included
        // (ADR 0042). One entry per *seen* room and never per scanned one:
        // a reservation is exactly the changing half of an outpost that
        // vision pays for, so a room we cannot look into contributes no
        // entry and its sources are unpriceable rather than half-rate (ADR
        // 0004). A seen room with no controller at all — a highway, a
        // sector centre — does get an entry, and a truthful one: nobody
        // owns or reserves it, which is the neutral rate and not an
        // unknown.
        RoomControl =
            seen
            |> List.map (fun room ->
                let c = room.controller

                let control: RoomControlInfo =
                    if isNull (box c) then
                        {
                            Owner = Ownership.Unowned
                            Reservation = None
                        }
                    else
                        {
                            // `my` is undefined and not false on a
                            // controller nobody owns, the shape `safeMode`
                            // and `ticksToRegeneration` also arrive in —
                            // so ours is asked first and off `my`, exactly
                            // as it was before this grew a third answer.
                            // `owner` is the undefined-when-absent half
                            // that separates the other two: a controller
                            // with an owner that is not us is a rival's,
                            // and one with none at all is unowned and
                            // reservable, which is every outpost the
                            // colony works (ADR 0042, ADR 0043).
                            Owner =
                                if not (isNull (box c.my)) && c.my then Ownership.Ours
                                elif isNull (box c.owner) then Ownership.Unowned
                                else Ownership.Rival
                            Reservation =
                                if isNull (box c.reservation) then
                                    None
                                else
                                    // Three holders and not two, because
                                    // ADR 0043 reads opposite answers off
                                    // the two that are not ours: the NPC's
                                    // reservation is the clock a
                                    // stand-down runs to where the core
                                    // carries no collapse timer, and a
                                    // player's is the clockless
                                    // withdrawal. The username separating
                                    // them is the shell's to know, exactly
                                    // as the colony's own name is, so the
                                    // comparison happens here and Core is
                                    // handed the answer.
                                    let holder =
                                        if Some c.reservation.username = colonyOwner then
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

                room.name, control)
            |> Map.ofList
        // Our sites in every room the colony is looking into, not the spawn
        // rooms' alone (#150). The Build pool is this list mapped one to
        // one (`Decide.planTasks`), so a site missing from it is a site no
        // Task ever names and no creep is ever sent to — which is exactly
        // where the outpost stalled: the container rule placed a site,
        // saw it standing there on the next tick and correctly declined to
        // place a second, and nothing ever built the first. ADR 0042 makes
        // a standing container the switch that admits an outpost into the
        // economy, and a site nobody builds is a switch that cannot close.
        //
        // Gated on `seen` and not on `scanned`, the rule `RoomControl`
        // follows and `Sources` above deliberately does not: a site is a
        // thing vision pays for and no declaration carries, so a room we
        // cannot look into contributes none of them (ADR 0004) rather than
        // a guess. The dedupe the spawn-keyed read needed goes with it —
        // `seen` is `scanned` filtered by vision (`List.choose`), and
        // `scanned` names each room once (`Outpost.roomsProjected`), so no
        // room's sites can be collected twice.
        //
        // This is not cross-room paving, which stays out of scope (ADR
        // 0042). The container rule is the only thing that places outside
        // the home room, so a container's is the only site that can appear
        // in an outpost; roads there would drag in the Layout's
        // spawn-anchored trunk and ADR 0010's whole-line Repair, and they
        // are their own ticket.
        ConstructionSites =
            seen
            |> List.collect (fun room -> room.find findMyConstructionSites |> Array.toList)
            |> List.map (fun o -> ({ Id = (o :?> IConstructionSite).id }: ConstructionSiteInfo))
        Creeps =
            objectValues<ICreep> Game.creeps
            // A creep still inside the spawn cannot act; keep it out of the pool.
            |> Array.filter (fun c -> not c.spawning)
            |> Array.map (fun c ->
                {
                    Name = c.name
                    TicksToLive = c.ticksToLive
                    Fatigue = c.fatigue
                    Energy = c.store.getUsedCapacity "energy"
                    FreeCapacity = c.store.getFreeCapacity "energy"
                    Body = c.body |> Array.countBy (fun p -> bodyPartOf p.``type``) |> Map.ofArray
                })
            |> Array.toList
        // Every room the colony is looking into, and no longer the spawn
        // rooms alone (#201). ADR 0033 was written before there was a
        // second room to write it about, and its three rules read this
        // list: a Threat's Reach gates the Tasks whose Work Area lies in
        // it, a creep standing in one is matched to Flee, and a spawn
        // whose doorstep is in one holds. Swept over the spawn rooms, a
        // hostile in an outpost was not merely unhandled but *absent* —
        // no Reach, so a freshly cast reserver walked into one; no Flee,
        // so a creep already there stood on its Seat and was killed; and
        // no episode in the Raid log, which reads this same list. All
        // three came true at once in W13S28 at t161,9xx.
        //
        // Gated on `seen` and not on `scanned`, the rule `RoomControl`
        // and `ConstructionSites` already follow: a hostile is a thing
        // vision pays for and no declaration carries, so a room nothing
        // is looking into contributes none of them rather than a guess
        // (ADR 0004) — and a room the colony has no creep in is one no
        // Reach could gate anything of ours in anyway. `seen` names each
        // room once (`Outpost.roomsProjected` filtered by vision), so no
        // room's hostiles are collected twice, and the home room is in it
        // by construction: a spawn cannot stand in a room with no vision.
        //
        // What the reflexes make of the widened list is Core's business
        // and stated there: safe mode and the towers read the colony's
        // own room out of it (ADR 0007, ADR 0014), because an outpost has
        // no controller of ours to protect and a tower shoots nothing
        // across a border.
        Hostiles =
            seen
            |> List.collect (fun r ->
                r.find findHostileCreeps
                |> Array.toList
                |> List.map (fun o ->
                    let c = o :?> ICreep

                    {
                        Id = c.id
                        Owner = c.owner.username
                        RoomName = r.name
                        Pos = posOf c.pos
                        Body =
                            c.body |> Array.map (fun p -> bodyPartOf p.``type``) |> Array.toList
                    }
                    : HostileInfo))
        // Every room the colony is looking into, the same set the list
        // above it is swept over since #201 (ADR 0043). An invader core is
        // what an outpost is stood down from, and an outpost is by
        // definition a room with no spawn in it, so the sweep that answers
        // for one always had to reach past the home room.
        //
        // Deliberately not folded into `Hostiles`, and the room set is no
        // longer any part of why. A core is a *structure*, so
        // `FIND_HOSTILE_CREEPS` cannot answer with one whatever set it is
        // swept over; and the two lists answer different questions — a
        // raider is something a creep runs from this tick (ADR 0033), a
        // core is something a whole room is withheld from for thousands
        // (ADR 0043), so one drives Reach and Flee and the other a clock.
        // `FIND_HOSTILE_STRUCTURES` answers with every structure a rival
        // owns, so the kind is checked here: this is the only question the
        // shell asks of that spelling.
        //
        // Gated on `seen`, the rule every fact vision pays for follows: a
        // room nothing is looking into contributes no entry at all rather
        // than "no core" (ADR 0004). That absence is exactly why the
        // deadline is read here and now: the creeps paying for the vision
        // are the ones a stand-down withdraws, and after they leave there
        // is nothing left to read it off.
        InvaderCores =
            seen
            |> List.collect (fun room ->
                room.find findHostileStructures
                |> Array.toList
                |> List.map (fun o -> o :?> IStructure)
                |> List.filter (fun st -> st.structureType = structureInvaderCore)
                |> List.map (fun st ->
                    ({
                        RoomName = room.name
                        CollapseTick = collapseTickOf st
                    }
                    : InvaderCoreInfo)))
        // Single-colony assumption, unchanged by ADR 0041: the first
        // spawn's room is the home room, the one `RoomName` names and the
        // Layout and the census signature read. What layering adds is the
        // rooms beside it — the declared outposts — over the same scan set
        // the Sources above are collected from.
        //
        // The declared furniture goes in last, over the whole assembled
        // projection rather than room by room inside it (`Outpost.place`,
        // ADR 0041): the rule that a source's and a controller's id and
        // tile do not wait for vision is Core's, and the shell's share of
        // it is this one splice. It lays nothing into a room the scan set
        // left out, and neither does the pool above, so the union stays
        // the single gate on which rooms the colony works.
        Spatial =
            home
            |> Option.map (fun name -> buildSpatial name scanned |> Outpost.place outposts)
            |> Option.defaultValue SpatialInfo.empty
        // The declaration's other half, handed over whole (ADR 0047): every
        // home room a human has declared a colony for, this one's included
        // and unfiltered. Which of them are **candidate colonies** — the
        // ones nobody owns yet — is read off `RoomControl` in Core, so the
        // shell passes on what was declared and judges none of it, exactly
        // as it does for the scan set above.
        ColonyHomes = Colony.homes Colony.declared
    }
