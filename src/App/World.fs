// Reads the engine, once, and files what it answered under the room names it
// answered for: this tick's World. The only code that reads the game's
// objects; what one colony makes of them is `ColonyView.ofWorld`'s, in Core.
module Fabot.World

open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types

/// Classify one tile of engine terrain into the Core's three states.
let private terrainAt (terrain: ITerrain) x y =
    let mask = terrain.get (x, y)

    if mask &&& terrainMaskWall <> 0 then Wall
    elif mask &&& terrainMaskSwamp <> 0 then Swamp
    else Plain

let private posOf (p: IRoomPosition) : Pos = { X = p.x; Y = p.y }

/// The tile a creep stands on, room and all: the one reading of an engine
/// creep's position.
let private tileOf (c: ICreep) : RoomPos = RoomPos.at c.room.name (posOf c.pos)

/// A creep's name as a string that owns nothing (#396): the engine hands
/// `name` over sliced, a slice pins its ~4.5 KB parent, and the Transition
/// log holds names across ticks (measured 2026-09-21: 6.3 MB over 1,500
/// entries). A JSON round trip is a fresh sequential string.
let private nameOf (c: ICreep) : string =
    emitJsExpr c.name "JSON.parse(JSON.stringify($0))"

/// Classify an engine part-type string into the Core's body vocabulary:
/// the reverse of the Core's one part-name table. The engine's part set is
/// closed, so Tough is an unreachable fallback that keeps it total.
let private bodyPartOf =
    reverseOf partName allBodyParts >> Option.defaultValue Tough

/// Classify an engine STRUCTURE_* string into the Core's built kinds; a
/// string the table lacks is Other. Classified once so the rules stay in Core.
let private builtKindOf =
    reverseOf builtKindName allBuiltKinds >> Option.defaultValue BuiltKind.Other

/// One visible Reactor's owner, read once for both the decision's ownership
/// and the observe row.
let private reactorOwnerOf (reactor: IReactor) =
    if not (isNull (box reactor.my)) && reactor.my then
        ReactorOwner.Ours
    elif isNull (box reactor.owner) then
        ReactorOwner.Unowned
    else
        ReactorOwner.Rival reactor.owner.username

/// One room's terrain, in the two windows the projection assembles from it,
/// off one engine read.
type private RoomTerrain =
    {
        /// x,y in 1..48: the ground the projection stands on.
        Ground: TerrainGrid
        /// The border ring, x or y of 0 or 49: the Seam's terrain, never
        /// ground.
        Border: Map<Pos, Terrain>
    }

/// ADR-0031
let private terrainMemo =
    System.Collections.Generic.Dictionary<string, RoomTerrain>()

let private terrainOf (roomName: string) : RoomTerrain =
    match terrainMemo.TryGetValue roomName with
    | true, tiles -> tiles
    | _ ->
        let terrain = Game.map.getRoomTerrain roomName

        let tiles =
            {
                // Rows and columns 0/49 are exit tiles: stepping on one
                // teleports the creep into the next room. They stay out of
                // the ground, so nothing ever stands on one. Do not "fix"
                // this trim.
                Ground =
                    TerrainGrid.ofList
                        [
                            for x in 1..48 do
                                for y in 1..48 do
                                    { X = x; Y = y }, terrainAt terrain x y
                        ]
                // The same read's other window: the ring the trim drops.
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

/// The absolute tick a structure's collapse timer runs out at, or None.
/// `effects` is undefined on an object nothing is applied to, and a deployed
/// core carries other effects, so the array is searched by id. `Game.time +`
/// is load-bearing: the engine's `ticksRemaining` is relative, unlike the
/// HTTP API's absolute `endTime`.
let private collapseTickOf (structure: IStructure) : int option =
    if isNull (box structure.effects) then
        None
    else
        structure.effects
        |> Array.tryFind (fun effect -> effect.effect = effectCollapseTimer)
        |> Option.map (fun effect -> Game.time + effect.ticksRemaining)

/// The census's stable half, memoised per room name (#384). Building
/// `TargetKinds` is 6.5% of a profiled tick (`--scenario reactor --level 7`,
/// four interleaved pairs), and the cost is the `Map.ofArray` build, not the
/// reading. The fixtures are the same objects tick after tick; only the floor
/// (piles, tombstones) moves, and it is a handful of `Map.add`s per tick.
///
/// The key is a checksum and not a count: a count alone would miss one
/// structure destroyed and another built on the same tick, and a census still
/// naming a destroyed target is a Task pointed at nothing. Sum and xor of the
/// ids' hashes are order-insensitive, so a `find` sweep answering in another
/// order is a hit. Heap only, like `terrainMemo`.
type private StableCensus =
    {
        Sum: int
        Xor: int
        Count: int
        Kinds: Map<string, TargetKind>
        Positions: Map<string, Pos>
    }

let private censusMemo =
    System.Collections.Generic.Dictionary<string, StableCensus>()

let private seenFacts
    (ours: string option)
    (terrain: RoomTerrain)
    (spawns: SpawnInfo list)
    (standing: ICreep list)
    (casting: ICreep list)
    (room: IRoom)
    : RoomFacts =
    // Each structure and site is classified once and carried beside its kind.
    let structures =
        room.find findStructures
        |> Array.map (fun o ->
            let st = o :?> IStructure
            st, builtKindOf st.structureType)

    // Swept once for the energy table and the Thorium table both.
    let storedStructures = structures |> Array.filter (fun (_, kind) -> isStored kind)

    let sites =
        room.find findMyConstructionSites
        |> Array.map (fun o ->
            let site = o :?> IConstructionSite
            site, builtKindOf site.structureType)

    // Everybody else's sites, as tiles and nothing more: the engine takes one
    // site per tile whoever placed it, and every other reader of a site
    // presumes the site is ours.
    let rivalSites =
        room.find findHostileConstructionSites
        |> Array.map (fun o -> posOf (o :?> IConstructionSite).pos)

    // The structures we own here: `needsOwner` kinds are checked against
    // their ids (FIND_STRUCTURES carries every owner's), and the
    // energy-hungry ones are the Refillables.
    let mine =
        room.find findMyStructures
        |> Array.map (fun o ->
            let st = o :?> IStructure
            st, builtKindOf st.structureType)

    let ourIds = mine |> Array.map (fun (st, _) -> st.id) |> Set.ofArray

    let sources = room.find findSources |> Array.map (fun o -> o :?> ISource)

    // Dropped piles of either of the colony's resources, classified rather
    // than filtered on "energy" (#311 was a Thorium pile the shell did not
    // carry at all). A pile of anything else is dropped here.
    let dropped =
        room.find findDroppedResources
        |> Array.map (fun o -> o :?> IResource)
        |> Array.choose (fun r ->
            if r.resourceType = resourceName Energy then
                Some(r, Energy)
            elif r.resourceType = resourceName Thorium then
                Some(r, Thorium)
            else
                None)

    // Tombstones and ruins, one kind, only while something is in them: an
    // empty one is a target no rule can answer for, and projecting it is a
    // hundred ticks of churn in every id-keyed table. Either resource, not
    // energy alone (#359): the engine's `withdraw` admits `Tombstone` and
    // `Ruin` with any of `RESOURCES_ALL`, and a decaying tombstone drops its
    // whole store as piles (`intents/tombstones/tick.js`), so ore left in one
    // bleeds on the floor at `ceil(amount / 1000)` a tick.
    let tombstones =
        Array.append (room.find findTombstones) (room.find findRuins)
        |> Array.map (fun o -> o :?> ITombstone)
        |> Array.filter (fun r ->
            r.store.getUsedCapacity (resourceName Energy) > 0
            || r.store.getUsedCapacity (resourceName Thorium) > 0)

    // The season's Thorium deposits only; the ordinary ore beside them is
    // never extracted, so `TargetKind.Mineral` carries no resource. The mod
    // deletes an exhausted deposit outright, so a mineral that leaves this
    // array is gone, not at zero.
    let minerals =
        room.find findMinerals
        |> Array.map (fun o -> o :?> IMineral)
        |> Array.filter (fun m -> m.mineralType = resourceName Thorium)

    // The only sweep that can answer with a reactor (`findReactors`). Normally
    // empty: one room in a sector holds one, seen only while a body of ours
    // stands in it.
    let reactors = room.find findReactors |> Array.map (fun o -> o :?> IReactor)

    let reactorFacts =
        reactors |> Array.map (fun reactor -> reactor, reactorOwnerOf reactor)

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

    // The stable half of the census, recalled or rebuilt: the checksum is over
    // exactly the fixtures that enter it, and the floor is added per tick.
    let stable =
        let mutable sum = 0
        let mutable bits = 0
        let mutable count = 0

        let note (id: string) =
            let h = hash id
            sum <- sum + h
            bits <- bits ^^^ h
            count <- count + 1

        for s in sources do
            note s.id

        for (st, _) in structures do
            note st.id

        for (site, _) in sites do
            note site.id

        for c in controllers do
            note c.id

        for m in minerals do
            note m.id

        for r in reactors do
            note r.id

        match censusMemo.TryGetValue room.name with
        | true, held when held.Sum = sum && held.Xor = bits && held.Count = count -> held
        | _ ->
            let built =
                {
                    Sum = sum
                    Xor = bits
                    Count = count
                    // The same order the flat build had, so a controller that
                    // also travels through `FIND_STRUCTURES` still resolves to
                    // `Controller` and not to `Structure Other`: later entries
                    // win in `Map.ofArray`, and the floor added afterwards can
                    // never collide with a fixture's id.
                    Kinds =
                        Map.ofArray (
                            Array.concat
                                [
                                    sources |> Array.map (fun s -> s.id, Source)
                                    structures
                                    |> Array.map (fun (st, kind) -> st.id, Structure kind)
                                    sites |> Array.map (fun (site, kind) -> site.id, Site kind)
                                    controllers |> Array.map (fun c -> c.id, Controller)
                                    minerals |> Array.map (fun m -> m.id, Mineral)
                                ]
                        )
                    Positions =
                        Map.ofArray (
                            Array.concat
                                [
                                    sources |> Array.map (fun s -> s.id, posOf s.pos)
                                    structures |> Array.map (fun (st, _) -> st.id, posOf st.pos)
                                    sites |> Array.map (fun (site, _) -> site.id, posOf site.pos)
                                    controllers |> Array.map (fun c -> c.id, posOf c.pos)
                                    minerals |> Array.map (fun m -> m.id, posOf m.pos)
                                ]
                        )
                }

            censusMemo.[room.name] <- built
            built

    {
        Layer =
            {
                Terrain = terrain.Ground
                // The fixtures recalled, the floor added.
                TargetPositions =
                    (stable.Positions,
                     Array.append
                         (dropped |> Array.map (fun (r, _) -> r.id, posOf r.pos))
                         (tombstones |> Array.map (fun r -> r.id, posOf r.pos)))
                    ||> Array.fold (fun places (id, tile) -> Map.add id tile places)
                // This room's creeps, not the world's: a creep standing
                // elsewhere filed here under that room's coordinates is a
                // phantom occupant the Resolver arbitrates against.
                CreepPositions = standing |> List.map (fun c -> nameOf c, posOf c.pos) |> Map.ofList
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
                // Built roads only: a road site is not yet a road.
                Roads =
                    structures
                    |> Array.filter (fun (_, kind) -> kind = BuiltKind.Road)
                    |> Array.map (fun (st, _) -> posOf st.pos)
                    |> Set.ofArray
                // Deliberately not in `Obstacles`: the engine blocks only the
                // owner's own creep on an obstacle-type site, and a hostile
                // that walks onto one destroys it, so a rival's site prices
                // nothing.
                RivalSites = Set.ofArray rivalSites
            }
        Border = terrain.Border
        // Same array order as the layer's TargetPositions, so a controller
        // that also travels through FIND_STRUCTURES resolves to Controller
        // both times. The fixtures recalled, the floor added.
        TargetKinds =
            (stable.Kinds,
             Array.append
                 (dropped |> Array.map (fun (r, resource) -> r.id, Dropped resource))
                 (tombstones |> Array.map (fun r -> r.id, Tombstone)))
            ||> Array.fold (fun kinds (id, kind) -> Map.add id kind kinds)
        // Hits on the repairable kinds only; fields nobody decides on stay out.
        Hits =
            structures
            |> Array.filter (fun (st, kind) ->
                (wholeLine kind).IsSome && (not (needsOwner kind) || Set.contains st.id ourIds))
            |> Array.map (fun (st, _) -> st.id, { Hits = st.hits; HitsMax = st.hitsMax })
            |> Map.ofArray
        // Stored energy, with the transient stores (tombstones, piles) in the
        // same table.
        Stores =
            Array.concat
                [
                    storedStructures
                    |> Array.map (fun (st, _) ->
                        st.id, st.store.getUsedCapacity (resourceName Energy))
                    tombstones
                    |> Array.map (fun r -> r.id, r.store.getUsedCapacity (resourceName Energy))
                    dropped
                    |> Array.choose (fun (r, resource) ->
                        if resource = Energy then Some(r.id, r.amount) else None)
                ]
            |> Map.ofArray
        // The Thorium beside it, the deposit's own remaining amount included.
        // A store holding none has no entry.
        Thorium =
            Array.concat
                [
                    storedStructures
                    |> Array.map (fun (st, _) ->
                        st.id, st.store.getUsedCapacity (resourceName Thorium))
                    minerals |> Array.map (fun m -> m.id, m.mineralAmount)
                    // A dropped pile holds its amount in `object[resourceType]`
                    // rather than a `store`, which is why the mod's contact
                    // penalty skips it.
                    dropped
                    |> Array.choose (fun (r, resource) ->
                        if resource = Thorium then Some(r.id, r.amount) else None)
                    // A tombstone or a ruin holds its ore in a real store, so
                    // the mod's contact penalty counts it and `withdraw`
                    // empties it. Without this column one reached the pool
                    // with no amount (#359).
                    tombstones
                    |> Array.map (fun r -> r.id, r.store.getUsedCapacity (resourceName Thorium))
                ]
            |> Array.filter (fun (_, held) -> held > 0)
            |> Map.ofArray
        // The extractor's cooldown: `cooldown` is undefined on every other
        // kind we build, and 0 is a real answer ("this tick"), so the map is
        // not filtered the way the Thorium above is.
        Cooldowns =
            structures
            |> Array.filter (fun (_, kind) -> kind = BuiltKind.Extractor)
            |> Array.map (fun (st, _) -> st.id, st.cooldown)
            |> Map.ofArray
        // The reactors' owners, read off `my` and `owner` as the controller's
        // are (the mod's `my` is undefined, not false, on one nobody owns).
        // Neither the reactor's tile nor a kind is filed, and both omissions
        // are load-bearing: the tile is the declaration's (`Errand.place`),
        // and an id classified by nothing is priceable by a Task that names it
        // and enumerable by no pool that sweeps a kind. A `TargetKind` here
        // would undo that from the far side of the view's `erranding` cut.
        Owners =
            reactorFacts
            |> Array.map (fun (reactor, owner) ->
                reactor.id,
                match owner with
                | ReactorOwner.Ours -> Ownership.Ours
                | ReactorOwner.Unowned -> Ownership.Unowned
                | ReactorOwner.Rival _ -> Ownership.Rival)
            |> Map.ofArray
        Reactors =
            reactorFacts
            |> Array.map (fun (reactor, owner) ->
                {
                    Id = reactor.id
                    Owner = owner
                    Thorium = reactor.store.getUsedCapacity (resourceName Thorium)
                    ContinuousWork = reactor.continuousWork
                })
            |> Array.toList
        // Who holds the room. A seen room with no controller gets a truthful
        // entry: nobody owns or reserves it.
        Control =
            Some(
                match controller with
                | None ->
                    {
                        Owner = Ownership.Unowned
                        Reservation = None
                        SafeMode = false
                        // A room with no controller has nothing to sign.
                        Sign = None
                    }
                | Some c ->
                    {
                        // `safeMode` is the tick count remaining and
                        // undefined otherwise.
                        SafeMode = not (isNull (box c.safeMode))
                        // `sign` is undefined until somebody writes one; the
                        // text is what the rule compares, not who wrote it.
                        Sign =
                            if isNull (box c.sign) then
                                None
                            else
                                Some(string c.sign.text)
                        // `my` is undefined and not false on a controller
                        // nobody owns, so ours is asked first and off `my`;
                        // `owner` separates the other two.
                        Owner =
                            if not (isNull (box c.my)) && c.my then Ownership.Ours
                            elif isNull (box c.owner) then Ownership.Unowned
                            else Ownership.Rival
                        Reservation =
                            if isNull (box c.reservation) then
                                None
                            else
                                // Three holders, not two: the stand-down
                                // reads different clocks off the NPC's and a
                                // player's.
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
        // The controller while it is ours: the downgrade clock and the banked
        // safe modes are undefined on one we do not own.
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
        // The bodies still gestating in this room's spawns.
        Casting =
            casting
            |> List.map (fun c ->
                {
                    Name = nameOf c
                    Body = c.body |> Array.map (fun p -> bodyPartOf p.``type``) |> Array.toList
                })
        Refillables =
            mine
            |> Array.filter (fun (_, kind) -> isRefillable kind)
            |> Array.map (fun (st, kind) ->
                {
                    Id = st.id
                    FreeCapacity = st.store.getFreeCapacity (resourceName Energy)
                    Kind = kind
                }
                : RefillableInfo)
            |> Array.toList
        // A source holding energy restocks in zero ticks whatever its timer
        // reads; the timer is read only for a drained source, and is undefined
        // until the engine starts it.
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
        // Our sites standing here: a site missing from this list is a site no
        // creep is ever sent to.
        ConstructionSites =
            sites
            |> Array.map (fun (site, _) ->
                ({
                    Id = site.id
                    Left = site.progressTotal - site.progress
                }
                : ConstructionSiteInfo))
            |> Array.toList
        // The hostiles standing here, read for every room the world can see.
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
        // The invader cores standing here. A core is a structure, so
        // `FIND_HOSTILE_CREEPS` cannot answer with one; `FIND_HOSTILE_STRUCTURES`
        // answers with every structure a rival owns, so the kind is checked.
        InvaderCores =
            room.find findHostileStructures
            |> Array.map (fun o -> o :?> IStructure)
            |> Array.filter (fun st -> st.structureType = structureInvaderCore)
            |> Array.map (fun st ->
                ({
                    RoomName = room.name
                    CollapseTick = collapseTickOf st
                    // Every core carries a level, so the fallback is
                    // unreachable, and it reads the wrong direction (safe to
                    // cross) if it ever fires: the filter above is what the
                    // rule rests on.
                    Level = if isNull (box st.level) then 0 else st.level
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
/// room (`Game.map.getRoomTerrain` needs no vision); everything else comes off
/// `Game.rooms`, and is absent entry by entry until vision returns.
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
            // Empty in every world the engine can build (a spawn of ours is
            // in a room we can see); filed from the same sweep as the seen
            // half.
            Spawns = spawns
        }
    | Some room -> seenFacts ours terrain spawns standing casting room

/// The rooms the world holds facts for this tick: every room we can see and
/// every room a standing colony's declaration names. The union and not one
/// colony's scan set: narrowing the world by the gates would put the shell in
/// the business of deciding which rooms matter. A seen room no colony works
/// costs the full `seenFacts` sweep; that is bounded by where our bodies stand.
let private worldRooms (maxHops: int) (colonies: Colony list) (seen: string list) : string list =
    let declared =
        colonies
        |> List.filter (fun colony -> List.contains colony.Home seen)
        // The colony's own projection set, transit rooms included: a room the
        // view will project is a room the world has to hold terrain for, and a
        // transit room costs a memo read. Narrowed by the hop budget as
        // `World.scanOf` is: a declaration past it is refused, and reading its
        // transit rectangle anyway would drag every room between here and a
        // mis-declaration into the world for nobody to use.
        |> List.collect (fun colony ->
            let outposts =
                colony.Outposts |> List.filter (Outpost.withinHopBudget maxHops colony.Home)

            // The rooms a child would be projected through, off the names
            // alone: whether the view borrows them turns on a stage derived
            // from the world this is choosing rooms for, so the declaration's
            // shape is read and an unborrowed room costs a memo read. Without
            // it a nursery two hops out is projected with no chain to it
            // (W15S28, 2026-09-10).
            let children =
                colonies
                |> List.filter (fun child ->
                    child.Mother = Some colony.Home
                    && child.Home <> colony.Home
                    && RoomName.hopsBetween colony.Home child.Home
                       |> Option.exists (fun hops -> hops <= maxHops))
                |> List.collect (fun child ->
                    child.Home :: RoomName.transitBetween colony.Home child.Home)

            // The colony's errands and their chains, narrowed by the same
            // budget for the same reason.
            let errands =
                colony.Errands |> List.filter (Errand.withinHopBudget maxHops colony.Home)

            Outpost.roomsProjected outposts colony.Home
            @ Errand.roomsProjected errands colony.Home
            @ children)

    seen @ declared |> List.distinct

/// The CPU counter as each room's facts finished, in sweep order, for the
/// tick's readings to difference. Heap-only and overwritten every tick. Held
/// here rather than on the `World` because a measurement of the shell is not
/// a fact about the game. It exists because `snapshot` is 21% of the live
/// tick and the harness's stub rooms cannot measure any of it (#370); a
/// per-room split counts our own calls, so it is comparable between the two.
let mutable roomCosts: (string * float) list = []

/// The counter as the room sweep began, which the first room is differenced
/// against. Not `AtEntry`: the phase enumerates `Game.rooms`, groups the
/// creeps and reads the declarations before the first room, and charging that
/// to the first room swept made a one-rock outpost read 2.35 ms against the
/// four-spawn home's 1.23 (#370).
let mutable roomsBegan: float = 0.0

let ofGame (maxHops: int) (colonies: Colony list) (lastPositions: Map<string, RoomPos>) : World =
    let spawns = objectValues<ISpawn> Game.spawns

    // The name the engine spells us, off the controller of a room one of our
    // spawns stands in: a spawn cannot stand in a room we do not own.
    let ours =
        spawns
        |> Array.tryPick (fun s ->
            let c = s.room.controller

            if isNull (box c) || isNull (box c.owner) then
                None
            else
                Some c.owner.username)

    // Our spawns grouped by the room they stand in, swept once.
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

    // Every creep we own, swept once and grouped by the room it stands in,
    // the standing apart from the gestating. `List.groupBy` keeps the engine's
    // own order within a room, which `World.creepColonies` reads.
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

    // The rooms vision answered for, read once and used twice: which rooms
    // the world holds facts for, and which may stamp a sighting.
    let seen = objectEntries Game.rooms |> Array.map fst |> Array.toList

    let mutable costs = []
    roomsBegan <- Game.cpu.getUsed ()

    let rooms =
        worldRooms maxHops colonies seen
        |> List.map (fun roomName ->
            let facts =
                factsOf
                    ours
                    (Map.tryFind roomName spawnsByRoom |> Option.defaultValue [])
                    (inRoom standingByRoom roomName)
                    (inRoom castingByRoom roomName)
                    roomName

            // Read after the room's facts and not before, so the list is the
            // same cumulative shape the phase boundaries are: one counter per
            // room in sweep order, differenced by `foldCpu`.
            costs <- (roomName, Game.cpu.getUsed ()) :: costs
            roomName, facts)

    roomCosts <- List.rev costs

    {
        Time = Game.time
        Rooms = Map.ofList rooms
        // A sighting for every room seen this tick, carrying the census's ids
        // and not the kinds; the rest `World.recalling` fills from last tick.
        Sightings =
            rooms
            |> List.filter (fun (roomName, _) -> List.contains roomName seen)
            |> List.map (fun (roomName, facts) ->
                roomName,
                ({
                    Tick = Game.time
                    // Deferred, and the census captured rather than copied:
                    // the ids are read the tick this room goes dark, not the
                    // tick it is seen (#371).
                    Targets = lazy (facts.TargetKinds |> Map.keys |> Set.ofSeq)
                }
                : RoomSighting))
            |> Map.ofList
        // Every creep we own that is not still gestating, in the engine's own
        // order.
        Creeps =
            standing
            |> List.map (fun c ->
                {
                    Room = c.room.name
                    Info =
                        {
                            Name = nameOf c
                            TicksToLive = c.ticksToLive
                            Fatigue = c.fatigue
                            Hits = { Hits = c.hits; HitsMax = c.hitsMax }
                            Energy = c.store.getUsedCapacity (resourceName Energy)
                            Thorium = c.store.getUsedCapacity (resourceName Thorium)
                            // The whole store's free room: a creep's store is
                            // general, so this is the capacity less everything
                            // aboard and not the energy's own share.
                            FreeCapacity = c.store.getFreeCapacity (resourceName Energy)
                            Body =
                                // The parts still standing, never the parts it
                                // was cast with: a destroyed part stays in the
                                // array reading zero hits (#270).
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
/// `CreepInfo.Moved`. Here so it shares `ofGame`'s gestating filter.
let positions () : (string * RoomPos) list =
    objectValues<ICreep> Game.creeps
    |> Array.filter (fun c -> not c.spawning)
    |> Array.map (fun c -> nameOf c, tileOf c)
    |> Array.toList
