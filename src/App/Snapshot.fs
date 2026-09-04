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

let private buildSpatial (spawn: ISpawn) : SpatialInfo =
    let room = spawn.room

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

    // Dropped energy piles, for the pickup reflex: position and kind only —
    // no decision reads an amount, so none is projected.
    let dropped =
        room.find findDroppedResources
        |> Array.map (fun o -> o :?> IResource)
        |> Array.filter (fun r -> r.resourceType = "energy")

    // The controller travels through FIND_STRUCTURES on live servers, but
    // is projected explicitly so nothing depends on that detail.
    let controllers =
        if isNull (box room.controller) then
            [||]
        else
            [| room.controller |]

    let terrain = terrainOf room.name

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
            // answer `Atlas.placedCreeps` already gives it. Unreachable
            // today (one spawn room, and no Task ever stands a creep on an
            // exit tile), so this holds the invariant rather than fixing a
            // live symptom.
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
        RoomName = Some room.name
        Rooms = Map.ofList [ room.name, layer ]
        // The border ring of every room the projection covers, under its
        // own name: the Atlas answers a Seam from these and from nothing
        // else (ADR 0041). One room today, so every Seam query answers
        // empty — the neighbour is not projected — which is ADR 0004's
        // per-entry absence and not a special case.
        Borders = Map.ofList [ room.name, terrain.Border ]
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
        Stores =
            structures
            |> Array.filter (fun (_, kind) -> isStored kind)
            |> Array.map (fun (st, _) -> st.id, st.store.getUsedCapacity "energy")
            |> Map.ofArray
    }

let build () : Snapshot =
    let spawns = objectValues<ISpawn> Game.spawns

    let spawnRooms =
        spawns |> Array.map (fun s -> s.room) |> Array.distinctBy (fun r -> r.name)

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
        Sources =
            spawns
            |> Array.collect (fun s -> s.room.find findSources)
            |> Array.map (fun o ->
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
            |> Array.distinctBy (fun s -> s.Id)
            |> Array.toList
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
        ConstructionSites =
            spawns
            |> Array.collect (fun s -> s.room.find findMyConstructionSites)
            |> Array.map (fun o -> ({ Id = (o :?> IConstructionSite).id }: ConstructionSiteInfo))
            |> Array.distinctBy (fun site -> site.Id)
            |> Array.toList
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
        // The spawn rooms and no others, unchanged by ADR 0041: what the
        // layering adds is that each hostile now says which of them it
        // stands in, so the Raid log measures it against that room's
        // tiles rather than against every room's unioned (ADR 0028).
        Hostiles =
            spawnRooms
            |> Array.collect (fun r ->
                r.find findHostileCreeps
                |> Array.map (fun o ->
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
            |> Array.toList
        // Single-colony assumption: only the first spawn's room is projected.
        Spatial =
            spawns
            |> Array.tryHead
            |> Option.map buildSpatial
            |> Option.defaultValue SpatialInfo.empty
    }
