// Reads the engine, once, and files what it answered under the room names
// it answered for: this tick's World (ADR 0052 decision 1). The only code
// that reads the game's *objects* — the rooms, the structures, the creeps
// — so what one colony makes of them is `ColonyView.ofWorld`'s, in Core,
// where a test can hand it a world. What the loop still reads for itself
// is the engine's clock, its Memory and the names of the creeps that exist
// (`Main.loop`), none of which is a fact a decision is made from.
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
/// The split happens here, at memo fill, rather than where the room's
/// facts are assembled. Assembly runs every tick, and cutting a
/// fifty-by-fifty map into these two there would rebuild 2304 `Pos`-keyed
/// entries per room per tick — precisely the structural comparison ADR
/// 0031 measured at about a quarter of an 8 ms tick and exists to delete,
/// which is why ADR 0041's own Consequences say the terrain memo needs no
/// change at all. Both windows come off one `getRoomTerrain` read and are
/// disjoint, so this is one terrain truth cut once, not two kept in step.
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

/// One room we can see, read whole: everything vision pays for, filed
/// under this room's name and narrowed by nothing (ADR 0052 decision 1).
/// Most of it comes off the `room.find` families, and what does not — the
/// controller, a structure's store, our own creeps out of the world-wide
/// `Game.creeps` — is scoped to this room by hand where the engine does
/// not scope it, which is what the creep filter below is for and why it is
/// not redundant. Its terrain and its spawns are handed in: the first
/// needs no vision, and the second is a world-wide sweep grouped once for
/// the tick rather than re-swept per room.
///
/// **Every creep of ours standing here**, and no longer one colony's: the
/// world reads a room once for everybody, and which of these bodies a
/// given colony holds is that colony's own cut (`ColonyView.ofWorld`,
/// which files the rest under `Foreign`). Reading it here was what made
/// another colony's creep an invisible occupant of this room (#220).
///
/// `ours` is the name the engine spells this player, taken off a spawn
/// room's controller in `ofGame`: whose a reservation is, is a comparison
/// against that name, and Core is handed the answer rather than the two
/// names, because asking the engine who anybody is is the shell's half of
/// ADR 0042 and the economics is Core's.
let private seenFacts
    (ours: string option)
    (terrain: RoomTerrain)
    (spawns: SpawnInfo list)
    (room: IRoom)
    : RoomFacts =
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

    // The structures we own here, classified once: two questions read
    // them, so the sweep runs once. Their **ids** are what the kinds that
    // ask for an owner are checked against (`needsOwner`, ADR 0034) —
    // FIND_STRUCTURES carries every owner's, and a rampart is the one
    // projected kind whose ownership changes the answer — and the
    // energy-hungry ones among them are the room's Refillables.
    let mine =
        room.find findMyStructures
        |> Array.map (fun o ->
            let st = o :?> IStructure
            st, builtKindOf st.structureType)

    let ourIds = mine |> Array.map (fun (st, _) -> st.id) |> Set.ofArray

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
                            ]
                    )
                // This room's creeps, not the world's: `Game.creeps` is
                // every creep we own wherever it stands, and a layer keyed
                // by room name may only hold the tiles of the room it is
                // filed under (ADR 0041). Every other field here is
                // already room-scoped through `room.find`; without the
                // same scope on this one, a creep standing in another room
                // would be filed here under that room's coordinates — a
                // phantom occupant the Resolver arbitrates against (ADR
                // 0001) and a tile the Raid log measures a raider's
                // closest approach to. A creep the projection cannot place
                // is ADR 0004's absence, which is the answer
                // `Atlas.placedCreepsByRoom` already gives it: it is in no
                // group, so no room's geometry is measured against it.
                // Load-bearing rather than defensive since #142: the mover
                // aims a creep matched across a border at an exit tile, and
                // the engine puts it down on the neighbour's border row for
                // the next tick to read, so `Game.creeps` really does
                // report one of ours outside the room being read.
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
                                // The engine refuses to move a creep onto
                                // its own obstacle-type construction site,
                                // so those tiles block exactly like the
                                // finished structure would.
                                sites
                                |> Array.filter (fun (_, kind) -> not (isWalkable kind))
                                |> Array.map (fun (site, _) -> posOf site.pos)
                                controllers |> Array.map (fun c -> posOf c.pos)
                            ]
                    )
                // Built roads only: a road construction site is not yet a
                // road, so it never enters the pricing (ADR 0010).
                Roads =
                    structures
                    |> Array.filter (fun (_, kind) -> kind = BuiltKind.Road)
                    |> Array.map (fun (st, _) -> posOf st.pos)
                    |> Set.ofArray
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
                (wholeLine kind).IsSome && (not (needsOwner kind) || Set.contains st.id ourIds))
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
        // Who holds the room, home included (ADR 0042). A seen room with
        // no controller at all — a highway, a sector centre — gets an
        // entry, and a truthful one: nobody owns or reserves it, which is
        // the neutral rate and not an unknown.
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
                        // nobody owns, the shape `safeMode` and
                        // `ticksToRegeneration` also arrive in — so ours
                        // is asked first and off `my`. `owner` is the
                        // undefined-when-absent half that separates the
                        // other two: a controller with an owner that is
                        // not us is a rival's, and one with none at all is
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
                                // Three holders and not two, because ADR
                                // 0043 reads opposite answers off the two
                                // that are not ours: the NPC's reservation
                                // is the clock a stand-down runs to where
                                // the core carries no collapse timer, and
                                // a player's is the clockless withdrawal.
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
        // banked safe modes are undefined on a controller we do not own,
        // so a room a rival holds carries its ownership in `Control` and no
        // controller here (ADR 0004). The level rides this record, and it
        // is what a [[stage]] is derived from (`World.stages`).
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
        // energy restocks in zero ticks (ADR 0025), whatever its
        // regeneration timer reads — that guard, not the timer, is what
        // makes the projection right. The timer is read only for a drained
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
        // Our sites standing here (#150): the Build pool is a colony's
        // share of these one to one (`Decide.planTasks`), so a site
        // missing from it is a site no Task ever names and no creep is
        // ever sent to — which is exactly where the outpost stalled, ADR
        // 0042 making a standing container the switch that admits an
        // outpost into the economy and a site nobody builds a switch that
        // cannot close.
        ConstructionSites =
            sites
            |> Array.map (fun (site, _) -> ({ Id = site.id }: ConstructionSiteInfo))
            |> Array.toList
        // The hostiles standing here (ADR 0033, #201). Read for every room
        // the world can see and not the spawn rooms' alone: a Threat's
        // Reach gates the Tasks whose Work Area lies in it, a creep
        // standing in one is matched to Flee, and a spawn whose doorstep is
        // in one holds — swept over the spawn rooms, a hostile in an
        // outpost was not merely unhandled but *absent*, and all three came
        // true at once in W13S28 at t161,9xx.
        Hostiles =
            room.find findHostileCreeps
            |> Array.map (fun o ->
                let c = o :?> ICreep

                {
                    Id = c.id
                    Owner = c.owner.username
                    RoomName = room.name
                    Pos = posOf c.pos
                    Body = c.body |> Array.map (fun p -> bodyPartOf p.``type``) |> Array.toList
                }
                : HostileInfo)
            |> Array.toList
        // The invader cores standing here (ADR 0043). Deliberately not
        // folded into `Hostiles`, and the room set is no part of why: a
        // core is a *structure*, so `FIND_HOSTILE_CREEPS` cannot answer
        // with one whatever set it is swept over, and the two lists answer
        // different questions — a raider is something a creep runs from
        // this tick, a core is something a whole room is withheld from for
        // thousands. `FIND_HOSTILE_STRUCTURES` answers with every structure
        // a rival owns, so the kind is checked here: this is the only
        // question the shell asks of that spelling.
        //
        // Read while there is still vision, because that is the only time
        // it is readable: the creeps paying for the vision in an outpost
        // are exactly the ones a stand-down withdraws.
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

/// One room's facts. Terrain comes off the memo whether or not we can see
/// the room: `Game.map.getRoomTerrain` answers for any room in the world,
/// needs no vision and never goes stale (ADR 0031, ADR 0041), which is why
/// the terrain layer's marginal cost across rooms is zero. Everything else
/// comes off `Game.rooms`, which holds only the rooms we have vision in.
///
/// So the half a room we cannot see contributes is its terrain, and the
/// half vision pays for — stores, hits, sites, creeps, hostiles, and every
/// structure standing — is absent entry by entry until vision returns (ADR
/// 0004), rather than a "blind" state anything has to model: unplaced
/// geometry is unpriceable, enters no Task and blocks no action.
///
/// It is not the whole of what a declared outpost puts in a colony's
/// projection. The other half is declared rather than seen — the sources'
/// and the controller's ids and tiles — and `Outpost.place` lays it over
/// the assembled view (ADR 0041, #148). It is not laid here because the
/// rule is a colony's: this function knows only what the engine answered
/// for one room name.
let private factsOf (ours: string option) (spawns: SpawnInfo list) (roomName: string) : RoomFacts =
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
            // from the same sweep as the seen half so that "where our
            // spawns are" has one answer and not two.
            Spawns = spawns
        }
    | Some room -> seenFacts ours terrain spawns room

/// The rooms the world holds facts for this tick: every room the engine
/// answered `Game.rooms` with — which is every room we can see — and,
/// beside them, the rooms a **standing** colony's declaration names, whose
/// terrain and furniture need no vision at all (ADR 0041).
///
/// The union and not one colony's scan set, which is the whole difference
/// between a world and a projection (ADR 0052 decision 1): the
/// [[stand-down]] gate (ADR 0043) and the [[bootstrap]] rule narrow what a
/// *colony* works (`ColonyView.ofWorld`), and narrowing the world by them
/// would put the shell in the business of deciding which rooms matter —
/// the answer this file is not allowed to have.
///
/// What it costs is two different prices. A **declared** room outside
/// every scan set costs one terrain read, once per heap (ADR 0031), and
/// nothing else — there is no vision to sweep. A room we can **see** and
/// no colony works costs the full `seenFacts` sweep, which the old shell
/// did not pay: a [[stand-down]]'s withheld outpost with one of our creeps
/// still walking out of it (ADR 0043) is that room, for as long as the
/// creep is there. Every view then drops it, because the gate that
/// withheld it is inside the cut. That is the price of the shell not
/// deciding — the gate's answer is Memory's and arrives after this read —
/// and it is bounded by the rooms our own bodies stand in, since vision is
/// what `Game.rooms` is.
///
/// A declaration is read **only where its home room is one we can see**,
/// and that is the one narrowing here. A colony we are not standing in is
/// a room a human means to have and nothing more — its outposts are rooms
/// nobody is mining, and reading their terrain would be the whole map of a
/// sector the bot has never been to, charged to a tick that has no use for
/// it. A home we can see is a home a spawn of ours may stand in, which is
/// what `Colony.living` asks next.
let private worldRooms (colonies: Colony list) : string list =
    let seen = objectEntries Game.rooms |> Array.map fst |> Array.toList

    let declared =
        colonies
        |> List.filter (fun colony -> List.contains colony.Home seen)
        |> List.collect (fun colony ->
            colony.Home :: (colony.Outposts |> List.map (fun outpost -> outpost.RoomName)))

    seen @ declared |> List.distinct

/// This tick's World (ADR 0052 decision 1): every room we declared or can
/// see, under its own name, and every creep we own beside them. The one
/// place the bot reads `Game`.
///
/// The declaration is handed in rather than read off the constant, which
/// is the rule every other declared fact travels under (`Outpost.place`,
/// ADR 0041): it decides which rooms are read here, so a harness or a test
/// can hand this function a world of its own.
let ofGame (colonies: Colony list) : World =
    let spawns = objectValues<ISpawn> Game.spawns

    // The name the engine spells us, off the controller of a room one of
    // our spawns stands in — a spawn cannot stand in a room we do not own,
    // so that owner is us. Read once for the world and not once per
    // colony: whose a reservation is, is a comparison against this name,
    // and there is one of us.
    let ours =
        spawns
        |> Array.tryPick (fun s ->
            let c = s.room.controller

            if isNull (box c) || isNull (box c.owner) then
                None
            else
                Some c.owner.username)

    // Our spawns grouped by the room they stand in, swept once: the world
    // holds every spawn we have, and which of them a colony casts from is
    // its own cut (`ColonyView.ofWorld` takes its home room's).
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

    {
        Time = Game.time
        Rooms =
            worldRooms colonies
            |> List.map (fun roomName ->
                roomName,
                factsOf ours (Map.tryFind roomName spawnsByRoom |> Option.defaultValue []) roomName)
            |> Map.ofList
        // Every creep we own that is not still gestating, in the engine's
        // own order — a creep still inside the spawn cannot act, and whose
        // each of these is this tick is `World.creepColonies`' answer over
        // the living colonies' scan sets (ADR 0047 decision 2).
        Creeps =
            objectValues<ICreep> Game.creeps
            |> Array.filter (fun c -> not c.spawning)
            |> Array.map (fun c ->
                {
                    Room = c.room.name
                    Info =
                        {
                            Name = c.name
                            TicksToLive = c.ticksToLive
                            Fatigue = c.fatigue
                            Energy = c.store.getUsedCapacity "energy"
                            FreeCapacity = c.store.getFreeCapacity "energy"
                            Body =
                                c.body
                                |> Array.countBy (fun p -> bodyPartOf p.``type``)
                                |> Map.ofArray
                        }
                }
                : WorldCreep)
            |> Array.toList
    }
