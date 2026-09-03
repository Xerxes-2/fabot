module Fabot.Core.Decide

open Fabot.Core.Types

/// The Workforce target's floor: the colony never plans below this many
/// living creeps. Two keep the harvest/refill loop running while one is in
/// transit or being replaced.
let private minWorkforce = 2

/// A Body pattern: the repeating part block a body is generated from.
/// Which pattern a spawn casts is a colony decision; the pattern shapes
/// what a creep is good at, never what it is assigned (ADR 0006).
type BodyPattern = { Name: string; Block: BodyPart list }

/// The generalist pattern: 200 energy, full speed empty, half speed loaded.
let workerPattern =
    {
        Name = "worker"
        Block = [ Work; Carry; Move ]
    }

/// The Anchor pattern: the heavy-WORK body cast for a Dual Seat (ADR
/// 0006). The block is its minimal cast — two Work keep the body readable
/// as an Anchor (Work > Move, which fatigue parity forbids a worker body)
/// beside the single Carry and the single Move that is the one-time price
/// of walking to the seat.
let anchorPattern =
    {
        Name = "anchor"
        Block = [ Work; Work; Carry; Move ]
    }

/// The hauler unit (ADR 0012): 150 energy, full speed loaded on roads —
/// the row carries its own parity declaration (road parity, not the
/// worker row's plain parity), because a hauler's whole life is the
/// trunk. No Work part, so Harvest, Build, Upgrade and Repair are
/// inapplicable by body; it lives in the Withdraw→Refill cycle.
let haulerPattern =
    {
        Name = "hauler"
        Block = [ Carry; Carry; Move ]
    }

/// The pattern table: every body the colony casts is a row here, sized by
/// energy under the row's own sizing rule. A future pattern is one more
/// data row plus its own quota rule — a colony fact deciding when it is
/// cast — never a new code path (ADR 0006).
let patternTable = [ workerPattern; anchorPattern; haulerPattern ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50
        | Attack -> 80
        | RangedAttack -> 150
        | Heal -> 250
        | Claim -> 600
        | Tough -> 10)

// Screeps MAX_CREEP_SIZE: the engine rejects bodies over 50 parts.
let private maxBodyParts = 50

/// The worker row's sizing rule: the largest affordable repetition of the
/// block (never below one repeat), with the remainder spent on Carry/Move
/// at fatigue parity — the padded body is never slower than the pure-block
/// body, empty or loaded, and within that buys as much Carry as possible
/// (ADR 0003, narrowed to the worker pattern by ADR 0006). Parts are
/// grouped Work, Carry, Move so damage strips Work first and mobility last.
let private parityBodyFor block capacity =
    let blockSize = List.length block
    let carryCost = bodyCost [ Carry ]
    let moveCost = bodyCost [ Move ]

    let blockCount part =
        block |> List.filter ((=) part) |> List.length

    let repeats = capacity / bodyCost block |> max 1 |> min (maxBodyParts / blockSize)

    // Loaded parity is work + carry <= 2 * move: a lone Carry is added
    // only under that bound, a Carry+Move pair preserves it, and a lone
    // Move (the trailing 50) only widens it.
    let rec pad work carry move budget slots =
        if slots >= 1 && budget >= carryCost && work + carry + 1 <= 2 * move then
            pad work (carry + 1) move (budget - carryCost) (slots - 1)
        elif slots >= 2 && budget >= carryCost + moveCost then
            pad work (carry + 1) (move + 1) (budget - carryCost - moveCost) (slots - 2)
        elif slots >= 1 && budget >= moveCost then
            pad work carry (move + 1) (budget - moveCost) (slots - 1)
        else
            work, carry, move

    let work, carry, move =
        pad
            (repeats * blockCount Work)
            (repeats * blockCount Carry)
            (repeats * blockCount Move)
            (capacity - repeats * bodyCost block)
            (maxBodyParts - repeats * blockSize)

    List.replicate work Work @ List.replicate carry Carry @ List.replicate move Move

/// The anchor row's sizing rule: one Carry, one Move, and every part slot
/// the remaining energy affords on Work — nearly all spawn energy buys
/// output rather than mobility the Dual Seat never uses. Exempt from
/// fatigue parity (ADR 0006); never below the row's two-Work block.
let private anchorBodyFor capacity =
    let work =
        (capacity - bodyCost [ Carry; Move ]) / bodyCost [ Work ]
        |> max 2
        |> min (maxBodyParts - 2)

    List.replicate work Work @ [ Carry; Move ]

/// The hauler row's sizing rule (ADR 0012): as many whole [Carry; Carry;
/// Move] blocks as capacity buys (never below one), and nothing else. The
/// row's parity declaration is road parity — two loaded Carry generate
/// two fatigue on a road tile, the one Move pays off two a tick — which
/// the whole block meets and a padded lone Carry would break; the
/// remainder stays banked. Parts are grouped Carry then Move so damage
/// strips capacity first and mobility last.
let private haulerBodyFor capacity =
    let repeats =
        capacity / bodyCost haulerPattern.Block
        |> max 1
        |> min (maxBodyParts / List.length haulerPattern.Block)

    List.replicate (2 * repeats) Carry @ List.replicate repeats Move

/// Body for a pattern at an energy capacity, under the row's own sizing
/// rule (ADR 0006): the anchor row spends on Work beside its fixed
/// Carry/Move pair, the hauler row buys whole blocks at its own road
/// parity, and every other block-replicating row pads its remainder at
/// plain fatigue parity.
let bodyFor pattern capacity =
    if pattern.Name = anchorPattern.Name then
        anchorBodyFor capacity
    elif pattern.Name = haulerPattern.Name then
        haulerBodyFor capacity
    else
        parityBodyFor pattern.Block capacity

/// The generalist body: the worker row of the pattern table, sized to
/// capacity.
let workerBodyFor capacity = bodyFor workerPattern capacity

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw containerId -> $"withdraw:{containerId}"
    | Refill structureId -> $"refill:{structureId}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"

/// The built kinds Repair keeps whole (ADR 0010, ADR 0012): roads and
/// containers. Non-repairable kinds (spawn, extension, tower) never
/// enter the pool on low hits, whatever the projection carries.
let private repairableKinds = [ BuiltKind.Road; BuiltKind.Container ]

/// The Repair trigger: a repairable structure enters the pool when its
/// hits sink strictly below this fraction of max, and leaves it once
/// repaired back over the line. A tunable, not part of ADR 0010.
let private repairTrigger = 0.5

/// Screeps range: Chebyshev distance between two tiles.
let private range a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// Screeps CONTAINER_CAPACITY: what a container's store can hold — the
/// line past which the buffer needs no Refill.
let private containerCapacity = 2000

/// Whether a placed tile is a source container's (ADR 0012): within range
/// 1 of a placed source — the Seat-standing kind the Layout places, which
/// harvest overflow fills. The one geometry judgement behind both rules
/// that care: the Planner keeps source containers out of Refill, the
/// hauler quota counts them. Unplaced geometry classifies nothing.
let private isSourceContainerTile (snapshot: Snapshot) pos =
    snapshot.Sources
    |> List.choose (fun s -> Map.tryFind s.Id snapshot.Spatial.TargetPositions)
    |> List.exists (fun s -> range pos s <= 1)

/// Planner: rebuild this tick's full Task pool from the Snapshot. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (snapshot: Snapshot) : Task list =
    let harvests = snapshot.Sources |> List.map (fun s -> Harvest s.Id)

    let refills =
        snapshot.Refillables
        |> List.filter (fun r -> r.FreeCapacity > 0)
        |> List.map (fun r -> Refill r.Id)

    let builds = snapshot.ConstructionSites |> List.map (fun site -> Build site.Id)

    // A Repair per repairable structure below the trigger, in id order.
    // The projection carries hits on repairable kinds only, but the kind
    // gate is judged here — the Planner owns what enters the pool.
    let repairs =
        snapshot.Spatial.Hits
        |> Map.toList
        |> List.filter (fun (id, hits) ->
            match Map.tryFind id snapshot.Spatial.TargetKinds with
            | Some(Structure kind) when List.contains kind repairableKinds ->
                float hits.Hits < repairTrigger * float hits.HitsMax
            | _ -> false)
        |> List.map (fst >> Repair)

    let upgrades =
        snapshot.Controller |> Option.toList |> List.map (fun c -> Upgrade c.Id)

    // The haul cycle's intake (ADR 0012), shaped over the projection's
    // stores rather than energy's name: every stocked container yields a
    // Withdraw, at feeding tier beside Harvest — whether to dig or to
    // collect is travel cost's call, never a rule's.
    let stored id =
        snapshot.Spatial.Stores |> Map.tryFind id |> Option.defaultValue 0

    let containers =
        snapshot.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            if kind = Structure BuiltKind.Container then
                Some id
            else
                None)

    let withdraws =
        containers |> List.filter (fun id -> stored id > 0) |> List.map Withdraw

    // The haul cycle's outflow: the controller container is one more
    // Refill target (ADR 0010's target layering, widened by ADR 0012).
    // Which container is the controller's is judged by geometry — it
    // stands inside the Upgrade Work Area (range 3) the Layout picked it
    // from, while a source container's tile (the Seat-standing kind) is
    // never a Refill target.
    let containerRefills =
        snapshot.Controller
        |> Option.bind (fun c -> Map.tryFind c.Id snapshot.Spatial.TargetPositions)
        |> Option.map (fun controllerPos ->
            containers
            |> List.filter (fun id ->
                match Map.tryFind id snapshot.Spatial.TargetPositions with
                | Some pos ->
                    range pos controllerPos <= 3
                    && not (isSourceContainerTile snapshot pos)
                    && stored id < containerCapacity
                | None -> false)
            |> List.map Refill)
        |> Option.defaultValue []

    harvests @ withdraws @ refills @ builds @ repairs @ upgrades @ containerRefills

/// Screeps source regen: 3000 energy per 300 ticks — the output per tick
/// a continuously drained source yields, and what its container's hauler
/// share must ship.
let private sourceOutputPerTick = 10

/// Screeps CARRY_CAPACITY: energy one Carry part holds.
let private carryPartCapacity = 50

/// The hauler row's quota rule (ADR 0012) — the row's colony fact, per
/// ADR 0006's law that a row arrives with its quota or not at all: per
/// source container, ceil(round-trip travel ticks to the spawn × source
/// output ÷ the cast body's carry capacity), so a farther container hires
/// proportionally more haul capacity and never quietly overflows. The
/// spawn is the canonical sink because the trunks radiate from it; of
/// several spawns the cheapest wins. No source containers, no placed
/// spawns, or unreachable geometry hire nothing.
let private haulerQuota (snapshot: Snapshot) atlas : int =
    let sourceContainerTiles =
        snapshot.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            if kind = Structure BuiltKind.Container then
                Map.tryFind id snapshot.Spatial.TargetPositions
            else
                None)
        |> List.filter (isSourceContainerTile snapshot)

    let spawns =
        snapshot.Spawns
        |> List.choose (fun s ->
            Atlas.positionOf atlas s.Id |> Option.map (fun pos -> s.RoomName, pos))

    sourceContainerTiles
    |> List.sumBy (fun tile ->
        spawns
        |> List.choose (fun (roomName, spawnPos) ->
            let bank =
                snapshot.RoomEnergy
                |> Map.tryFind roomName
                |> Option.defaultValue { Available = 0; Capacity = 0 }

            let body = bodyFor haulerPattern bank.Capacity

            let capacity = (body |> List.filter ((=) Carry) |> List.length) * carryPartCapacity

            Atlas.haulRoundTripTicks atlas body tile spawnPos
            |> Option.map (fun ticks -> (ticks * sourceOutputPerTick + capacity - 1) / capacity))
        |> function
            | [] -> 0
            | quotas -> List.min quotas)

/// Screeps CREEP_LIFE_TIME: the ticks a spawned creep lives — the horizon
/// a body's replacement cost is amortized over.
let private creepLifetime = 1500

/// Screeps UPGRADE_CONTROLLER_POWER's energy cost: what one Work part
/// drains per upgrade tick — the rate an upgrade mouth eats income at.
let private upgradeDrainPerWork = 1

/// Workforce target (ADR 0012): three addends, each a pattern row's own
/// colony fact — Anchors one per Post, haulers the throughput quota,
/// workers the income arithmetic — floored at minWorkforce and derived
/// fresh each tick. A source whose Post is provided for retires its other
/// Seats: one heavy body drains it alone, so counting seats after that is
/// hiring for jobs that no longer exist. An unposted source still
/// contributes its Seat count as today — its output is spoken for by the
/// seat crews that walk it, so only the posted sources' output is income.
/// From that income the anchor and hauler rows' replacement amortization
/// (body cost spread over a creep's lifetime) is deducted; every energy
/// per tick left feeds upgrade mouths at one worker body's Work drain —
/// exactly as many workers as the surplus feeds, bodies priced as the
/// richest bank would cast them. The arithmetic runs scaled by the
/// lifetime so the amortization never rounds away.
let private workforceTarget (snapshot: Snapshot) atlas anchorQuota haulerQuota =
    let posts = Atlas.posts atlas

    let posted, unposted =
        snapshot.Sources
        |> List.partition (fun s ->
            Atlas.seatTilesOf atlas s.Id |> Set.exists (fun seat -> Set.contains seat posts))

    let unpostedSeats =
        unposted
        |> List.sumBy (fun s -> Atlas.seats atlas s.Id |> Option.defaultValue 0)

    let capacity =
        snapshot.RoomEnergy
        |> Map.toList
        |> List.map (fun (_, bank) -> bank.Capacity)
        |> function
            | [] -> 0
            | caps -> List.max caps

    let amortization =
        anchorQuota * bodyCost (bodyFor anchorPattern capacity)
        + haulerQuota * bodyCost (bodyFor haulerPattern capacity)

    let workerDrain =
        bodyFor workerPattern capacity
        |> List.sumBy (function
            | Work -> upgradeDrainPerWork
            | _ -> 0)

    let incomeWorkers =
        (List.length posted * sourceOutputPerTick * creepLifetime - amortization)
        / (workerDrain * creepLifetime)
        |> max 0

    anchorQuota + haulerQuota + unpostedSeats + incomeWorkers |> max minWorkforce

/// Whether a living body was cast from the anchor row: more Work than
/// Move. Fatigue parity keeps every worker body at Work <= Move (ADR
/// 0003) and the anchor row's floor of two Work over one Move clears it,
/// so the casting pattern is readable off the body itself — what a creep
/// is is decided from what it is made of; the row name in a creep's name
/// is observability only, never read back (ADR 0006).
let private isAnchorBody (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    count Work > count Move

/// Whether a living body was cast from the hauler row: Carry parts but no
/// Work. The worker and anchor rows both keep at least one Work, and only
/// the hauler row casts none (ADR 0012) — so, like the anchor's
/// Work > Move, the casting pattern is readable off the body itself; the
/// row name in a creep's name stays observability only.
let private isHaulerBody (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    count Work = 0 && count Carry > 0

/// Pre-Task bootstrap step: spawn Intents needed to keep the workforce at
/// the Workforce target. Spawning is a colony-level need, not a Task creeps
/// get matched to, so it sits beside the Planner/Matcher pipeline rather
/// than inside it.
let private planSpawns (snapshot: Snapshot) atlas : Intent list =
    // The specialist rows' quota rules (ADR 0006, ADR 0012): one Anchor
    // per Post, haulers per the throughput arithmetic. Both are addends of
    // the target itself — inside it by construction, never on top of it.
    let anchorQuota = Atlas.posts atlas |> Set.count
    let haulerQuota = haulerQuota snapshot atlas
    let target = workforceTarget snapshot atlas anchorQuota haulerQuota
    let deficit = target - List.length snapshot.Creeps

    // Disaster fallback: an empty colony can never refill extensions, so
    // waiting for full capacity would wait forever — spawn a minimal
    // worker unit from whatever energy is banked right now.
    // Time-to-first-creep outranks specialisation, so the anchor gap
    // waits (ADR 0006).
    let castFromBank pattern (bank: RoomEnergy) =
        if List.isEmpty snapshot.Creeps then
            if bank.Available >= bodyCost workerPattern.Block then
                Some(workerPattern, workerPattern.Block)
            else
                None
        elif bank.Available >= bank.Capacity then
            Some(pattern, bodyFor pattern bank.Capacity)
        else
            None

    if deficit <= 0 then
        []
    else
        // Anchor gaps are filled before hauler gaps, hauler gaps before
        // generalist gaps — the casting order runs Anchor, hauler, worker
        // — and the worker row's quota is whatever the target has left.
        let anchorGap =
            anchorQuota - (snapshot.Creeps |> List.filter isAnchorBody |> List.length)
            |> max 0

        let haulerGap =
            haulerQuota - (snapshot.Creeps |> List.filter isHaulerBody |> List.length)
            |> max 0

        // Idle spawns draw from their room's one bank in list order — each
        // body debits the budget the next spawn sees, so the same energy is
        // never committed twice.
        let intents, _ =
            snapshot.Spawns
            |> List.filter (fun s -> not s.IsSpawning)
            |> List.fold
                (fun (intents, banks: Map<string, RoomEnergy>) s ->
                    let bank =
                        banks
                        |> Map.tryFind s.RoomName
                        |> Option.defaultValue { Available = 0; Capacity = 0 }

                    let planned = List.length intents

                    let wanted =
                        if planned < anchorGap then anchorPattern
                        elif planned < anchorGap + haulerGap then haulerPattern
                        else workerPattern

                    match castFromBank wanted bank with
                    | Some(pattern, body) when planned < deficit ->
                        SpawnCreep(s.Name, body, $"{pattern.Name}-{snapshot.Time}-{s.Name}")
                        :: intents,
                        banks
                        |> Map.add
                            s.RoomName
                            { bank with
                                Available = bank.Available - bodyCost body
                            }
                    | _ -> intents, banks)
                ([], snapshot.RoomEnergy)

        List.rev intents

/// Colony reflex beside the pipeline: a CLAIM-part hostile is the one
/// threat that can disarm safe mode itself — attackController blocks
/// activation for 1,000 ticks — so the activation fires the tick such a
/// hostile is seen, while firing is still possible. Fighters without
/// CLAIM cannot touch the controller and never spend the stock: at RCL2
/// safe mode outlasts any invader raid 13×, so it keeps for when the
/// room is actually being taken (ADR 0007).
let private planSafeMode (snapshot: Snapshot) : Intent list =
    match snapshot.Controller with
    | Some controller when
        controller.SafeModeAvailable > 0
        && not controller.SafeModeActive
        && snapshot.Hostiles |> List.exists (fun h -> List.contains Claim h.Body)
        ->
        [ ActivateSafeMode controller.Id ]
    | _ -> []

/// Extensions the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "extension").
let private extensionAllowance level =
    match level with
    | 0
    | 1 -> 0
    | 2 -> 5
    | 3 -> 10
    | 4 -> 20
    | 5 -> 30
    | 6 -> 40
    | 7 -> 50
    | _ -> 60

/// Towers the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "tower").
let private towerAllowance level =
    match level with
    | 0
    | 1
    | 2 -> 0
    | 3
    | 4 -> 1
    | 5
    | 6 -> 2
    | 7 -> 3
    | _ -> 6

/// The Layout horizon (ADR 0011): the whole plan is computed up to this
/// level regardless of the current one, so today's roads route around
/// tomorrow's structures. Deliberately not RCL8 — a wider reservation
/// would tax today's trunks with detours for structures five levels away.
let private horizonLevel = 4

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR 0011), computed whole from the Atlas every
/// tick and placed all at once — no persisted plan, no pacing. One
/// ordering rule eats every clustered structure: buildable tiles on the
/// spawn's checkerboard colour, nearest-to-spawn first, the tower taking
/// its pick before the extensions. Trunk roads pave each source to the
/// controller and to each spawn plus the swamps of the controller's Work
/// Area, priced on raw terrain and routed around every reserved tile —
/// reservations come first, so a road never sits where a structure will.
/// Placement filters the Layout to what the current level unlocks and
/// what the projection's censuses say is missing. Sites are not creep
/// work, so this emits Intents directly rather than Tasks.
let private planLayout (snapshot: Snapshot) atlas : Intent list =
    let anchor = snapshot.Spawns |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id)

    match Atlas.roomName atlas, anchor, snapshot.Controller with
    | Some room, Some spawnPos, Some controller ->
        // Same checkerboard colour as the spawn: clustered structures sit on
        // the spawn's colour, leaving the other colour free for movement.
        let parity = (spawnPos.X + spawnPos.Y) % 2

        let ordering =
            Atlas.buildableTiles atlas
            |> List.filter (fun tile -> (tile.X + tile.Y) % 2 = parity)
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)

        // A kind's still-open gap at a level: its allowance there minus the
        // projection's censuses of standing and pending structures. Judged
        // at the horizon it sizes the reservation; at the current level it
        // sizes the placement.
        let gapAt allowanceOf built pending level =
            allowanceOf level - built - pending |> max 0

        let towerGap =
            gapAt towerAllowance (Atlas.builtTowers atlas) (Atlas.pendingTowers atlas)

        let extensionGap =
            gapAt extensionAllowance (Atlas.builtExtensions atlas) (Atlas.pendingExtensions atlas)

        // The horizon's still-unclaimed slots, tower first: a built or
        // pending structure keeps its tile out of the ordering (it is a
        // target) and its slot off the plan.
        let towerSlots = towerGap horizonLevel
        let extensionSlots = extensionGap horizonLevel

        let clustered = ordering |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clustered |> List.splitAt (min towerSlots clustered.Length)

        // Reserved before trunks: a trunk never crosses a tile any horizon
        // structure will claim.
        let reserved = Set.ofList clustered

        let upgradeArea = Atlas.workArea atlas (Upgrade controller.Id)

        let spawnAreas =
            snapshot.Spawns
            |> List.choose (fun s -> Atlas.positionOf atlas s.Id)
            |> List.map (Atlas.adjacentWalkable atlas >> Set.ofList)

        // Trunks kept per source: the union paves the roads, and each
        // source's own trunk anchors its container (ADR 0012).
        let sourceTrunks =
            snapshot.Sources
            |> List.sortBy (fun s -> s.Id)
            |> List.choose (fun s ->
                Atlas.positionOf atlas s.Id
                |> Option.map (fun sourcePos ->
                    s.Id,
                    upgradeArea :: spawnAreas
                    |> List.collect (Atlas.trunkPath atlas reserved sourcePos)
                    |> Set.ofList))

        let trunkTiles = sourceTrunks |> List.map snd |> List.fold Set.union Set.empty

        // The controller's Work Area paves its swamps and only its swamps —
        // upgraders shuttle within it, so the dear ground gets a road and
        // the plain ground does not. A reserved tile is a structure's, not
        // a road's.
        let workAreaSwamps =
            upgradeArea
            |> Set.filter (Atlas.isSwamp atlas)
            |> fun s -> Set.difference s reserved

        // The road gap reads the projection's road census: a built road or a
        // pending road site already claims its tile (ADR 0010).
        let roadGap =
            Set.union trunkTiles workAreaSwamps
            |> fun wanted -> Set.difference wanted (Atlas.roadTiles atlas)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTiles atlas)

        // Containers (ADR 0012), computed whole like everything else and
        // RCL-gated by nothing — the engine allows them from level 0. Each
        // source's container sits on the Seat nearest that source's trunk;
        // the trunk's first tile is itself a Seat, so in practice the
        // container lands where the trunk leaves the source and harvest
        // overflow falls straight in. Seats are terrain geometry and
        // trunks avoid only the reservations, so the pick never shifts as
        // the container itself gets built.
        let sourceContainerTiles =
            sourceTrunks
            |> List.choose (fun (sourceId, trunk) ->
                let seats =
                    Atlas.seatTilesOf atlas sourceId
                    |> Set.filter (fun seat -> not (Set.contains seat reserved))

                if Set.isEmpty trunk || Set.isEmpty seats then
                    None
                else
                    seats
                    |> Set.toList
                    |> List.minBy (fun seat ->
                        trunk |> Set.toList |> List.map (range seat) |> List.min, seat.X, seat.Y)
                    |> Some)

        // The controller container: an Upgrade-Work-Area tile beside a
        // trunk, off the road itself and off every reservation — the
        // buffer upgraders work from standing still, one tile from where
        // the haulers drive. Judged from the same stable geometry as the
        // Seat pick, so a standing container recomputes to its own tile.
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.bind (fun controllerPos ->
                upgradeArea
                |> Set.filter (fun tile ->
                    not (Set.contains tile reserved)
                    && not (Set.contains tile trunkTiles)
                    && not (Set.contains tile workAreaSwamps)
                    && trunkTiles |> Set.exists (fun t -> range tile t = 1))
                |> Set.toList
                |> function
                    | [] -> None
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile controllerPos, tile.X, tile.Y)
                        |> Some)

        // The container gap reads the projection's container census, tile
        // for tile (ADR 0012): a built or pending container already
        // claims its spot. A tile still owed its road defers the
        // container too — the engine takes one construction site per
        // tile, so the source container (planned onto the trunk's first
        // tile) waits for the road to stand and then coexists with it.
        let claimed =
            [
                Atlas.containerTiles atlas
                Atlas.pendingContainerTiles atlas
                Atlas.pendingRoadTiles atlas
                roadGap
            ]
            |> List.fold Set.union Set.empty

        let containerGap =
            sourceContainerTiles @ Option.toList controllerContainerTile
            |> List.filter (fun tile -> not (Set.contains tile claimed))

        let place kind tiles =
            tiles |> List.map (fun tile -> PlaceConstructionSite(room, tile, kind))

        place Tower (towerTiles |> List.truncate (towerGap controller.Level))
        @ place Extension (extensionTiles |> List.truncate (extensionGap controller.Level))
        @ place Road (Set.toList roadGap)
        @ place Container containerGap
    | _ -> []

/// Colony reflex beside the pipeline, the second after safe mode: every
/// creep with free carry capacity standing within pickup range of a
/// dropped energy pile asks to pick it up — beside its assigned Task's
/// action, since the engine's pickup conflicts with no other action. No
/// movement, no matching, no threshold: the reflex only recaptures what
/// is already in reach (death drops, harvest overflow), and duplicate
/// pickups on one pile are the engine's to settle.
let private planPickups (snapshot: Snapshot) atlas : Intent list =
    match Atlas.droppedEnergy atlas with
    | [] -> []
    | piles ->
        let hungry =
            snapshot.Creeps
            |> List.filter (fun c -> c.FreeCapacity > 0)
            |> List.map (fun c -> c.Name)
            |> Set.ofList

        Atlas.placedCreeps atlas
        |> List.collect (fun (name, pos) ->
            if Set.contains name hungry then
                piles
                |> List.choose (fun (pile, tile) ->
                    if range pos tile <= 1 then
                        Some(PickupEnergy(name, pile))
                    else
                        None)
            else
                [])

/// Whether a creep can usefully work this Task right now. The body must
/// physically be able to do it — Work-part tasks need a Work part, energy
/// delivery needs a Carry part — and the energy state must call for it: a
/// full creep is done harvesting; an empty creep has nothing to deliver.
/// One geometric widening (ADR 0012): a full creep standing on a built
/// source container keeps Harvest — the engine drops the overflow into
/// the container underfoot, so the creep effectively has capacity and the
/// Post stays garrisoned. Body-blind like every gate here (ADR 0006):
/// any full creep on the tile qualifies; travel-cost pinning and the
/// workforce quotas are what keep the tile an Anchor's home.
let private applicable atlas (creep: CreepInfo) task =
    let has part =
        creep.Body |> Map.tryFind part |> Option.exists (fun n -> n > 0)

    match task with
    | Harvest sourceId ->
        has Work
        && (creep.FreeCapacity > 0 || Atlas.catchesOverflow atlas creep.Name sourceId)
    | Withdraw _ -> has Carry && creep.FreeCapacity > 0
    | Refill _ -> has Carry && creep.Energy > 0
    | Build _
    | Repair _
    | Upgrade _ -> has Work && creep.Energy > 0

let private intentFor (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> HarvestSource(creep.Name, sourceId)
    | Withdraw containerId -> WithdrawEnergyFromStructure(creep.Name, containerId)
    | Refill structureId -> TransferEnergyToStructure(creep.Name, structureId)
    | Build siteId -> BuildSite(creep.Name, siteId)
    | Repair structureId -> RepairStructure(creep.Name, structureId)
    | Upgrade controllerId -> UpgradeController(creep.Name, controllerId)

/// Chat-bubble glyph of a Task: the whole colony's current matching is
/// legible in the viewer at one glyph per creep.
let private glyphFor =
    function
    | Harvest _ -> "⛏"
    | Withdraw _ -> "📥"
    | Refill _ -> "🔋"
    | Build _ -> "🔨"
    | Repair _ -> "🔧"
    | Upgrade _ -> "⚡"

/// The full downgrade timer per controller level (Screeps
/// CONTROLLER_DOWNGRADE).
let private fullDowngradeTimer level =
    match level with
    | 1 -> 20000
    | 2 -> 10000
    | 3 -> 20000
    | 4 -> 40000
    | 5 -> 80000
    | 6 -> 120000
    | 7 -> 150000
    | _ -> 200000

/// The hard deadline on the controller's downgrade timer: half the
/// level's full timer. The engine refuses activateSafeMode once the
/// timer sinks below half minus 5,000 (its
/// CONTROLLER_DOWNGRADE_SAFEMODE_THRESHOLD grace), so escalating at half
/// keeps the safe-mode reflex fireable with the whole grace still banked
/// — a downgrade costs a level and zeroes the stock, so neither line is
/// ever approached (ADR 0007).
let private downgradeDeadline level = fullDowngradeTimer level / 2

/// Matching tier between applicable tasks (lower wins): feeding the economy
/// (Harvest, Withdraw, spawn-feeding Refill) outranks sinking surplus into
/// construction (Build), upkeep (Repair), the controller (Upgrade), or
/// the guns — Refill is the one Task whose rank layers by target (ADR
/// 0010): a tower Refill is
/// surplus-tier, because the colony feeds its own reproduction before its
/// guns, and a controller-container Refill (ADR 0012) sits one tier
/// deeper still — below Upgrade, so a full creep beside the buffer sinks
/// its load into the controller rather than dumping it back into the
/// container it just drew from and orbiting in place; the buffer is
/// filled by bodies with no surplus work of their own. One exception: a
/// controller inside the downgrade deadline makes Upgrade the colony's
/// most urgent work, outranking even the feeding tier (ADR 0007).
let private rank (snapshot: Snapshot) task =
    match task with
    | Harvest _
    | Withdraw _ -> 0
    | Refill structureId ->
        let isTower =
            snapshot.Refillables
            |> List.exists (fun r -> r.Id = structureId && r.Kind = BuiltKind.Tower)

        let isContainer =
            Map.tryFind structureId snapshot.Spatial.TargetKinds = Some(
                Structure BuiltKind.Container
            )

        if isContainer then 2
        elif isTower then 1
        else 0
    | Build _
    | Repair _ -> 1
    | Upgrade _ ->
        let urgent =
            snapshot.Controller
            |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

        if urgent then -1 else 1

/// Concurrent-worker cap per task id; tasks absent from the map are
/// unbounded. Harvest is capped by its source's Seat count — a source the
/// projection does not place derives no cap, so behaviour without terrain
/// data is unchanged.
let private taskCapacities (snapshot: Snapshot) atlas : Map<string, int> =
    snapshot.Sources
    |> List.choose (fun s ->
        Atlas.seats atlas s.Id |> Option.map (fun count -> taskId (Harvest s.Id), count))
    |> Map.ofList

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position.
let private actionIntents atlas (creep: CreepInfo) (task: Task) : Intent list =
    if Atlas.mayAct atlas creep.Name task then
        [ intentFor creep task ]
    else
        []

/// Emitter: each assigned creep's action Intent, then every assigned
/// creep's chat bubble, both in Snapshot creep order. Judges actions from
/// tick-start geometry — it must run against the same Atlas the Matcher
/// used, never against resolved positions.
let emit (snapshot: Snapshot) atlas (assigned: Map<string, Task>) : Intent list =
    let actions =
        snapshot.Creeps
        |> List.collect (fun creep ->
            match Map.tryFind creep.Name assigned with
            | Some task -> actionIntents atlas creep task
            | None -> [])

    // Every assigned creep says its Task's glyph every tick; unassigned
    // creeps say nothing.
    let says =
        snapshot.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name assigned
            |> Option.map (fun task -> SayCreep(creep.Name, glyphFor task)))

    actions @ says

/// A creep's Move Intent: candidate standing tiles for next tick in
/// preference order, plus a priority (the task rank). Input to the
/// Resolver — not an Intent; the Resolver's output is what becomes one.
type private MoveIntent =
    {
        Creep: string
        Pos: Pos
        Rank: int
        Candidates: Pos list
    }

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// Register one creep's Move Intent — every creep gets one (ADR 0001).
/// A creep travelling toward its Work Area wants exactly its next path
/// step; one already inside is force-registered "stay put, displaceable
/// within the Work Area"; one with no Task — or no way to reach its
/// area, which is just as immobilising — is parked: stay put,
/// displaceable to any adjacent walkable tile.
let private moveIntentFor
    (rankOf: Task -> int)
    atlas
    (creep: string)
    (pos: Pos)
    (task: Task option)
    : MoveIntent =
    let parked rank =
        {
            Creep = creep
            Pos = pos
            Rank = rank
            Candidates = pos :: Atlas.adjacentWalkable atlas pos
        }

    match task with
    | None -> parked idleRank
    | Some task ->
        let area = Atlas.workArea atlas task

        if Set.contains pos area then
            {
                Creep = creep
                Pos = pos
                Rank = rankOf task
                Candidates =
                    pos
                    :: (Atlas.adjacentWalkable atlas pos
                        |> List.filter (fun tile -> Set.contains tile area))
            }
        else
            match Atlas.firstStep atlas creep task with
            | Some step ->
                {
                    Creep = creep
                    Pos = pos
                    Rank = rankOf task
                    Candidates = [ step ]
                }
            | None -> parked (rankOf task)

/// Resolver core (per screeps-cartographer): movers claim before creeps
/// already standing where they want to be — a stay-put claim never walls
/// off a traveller's path; the stayer is displaced instead and shuffles
/// or swaps. Within each class claims go priority descending,
/// most-constrained first within a priority. Claiming a tile somebody
/// stands on displaces that occupant: the claimed tile leaves the
/// occupant's candidates and the claimant's vacated tile joins them as a
/// last resort, so an occupant that cannot stand elsewhere swaps with its
/// displacer. An occupant left with fewer than two open candidates
/// resolves immediately, ahead of every rank, locking the exchange in
/// before the vacated tile is claimed by anyone else. Tiles in `blocked`
/// arrive pre-claimed: they belong to fatigued creeps, which are not in
/// arbitration at all — a creep that cannot step this tick can neither be
/// displaced nor asked to move, so nobody claims its tile and no Intent
/// is ever issued to it.
let private arbitrate
    (occupants: Map<Pos, string>)
    (blocked: Set<Pos>)
    (moveIntents: MoveIntent list)
    : Map<string, Pos> =
    let openCandidates (claimed: Set<Pos>) (intent: MoveIntent) =
        intent.Candidates |> List.filter (fun tile -> not (Set.contains tile claimed))

    // A creep whose preferred tile is the one it stands on. Travellers
    // settle first: a stayer's claim can always be honoured by shuffling
    // or swapping it later, but a stayer settling first would wall off a
    // traveller's only path for the tick.
    let staying (intent: MoveIntent) =
        List.tryHead intent.Candidates = Some intent.Pos

    let rec settle (pending: Map<string, MoveIntent>) urgent claimed resolved =
        let next =
            match urgent |> List.filter (fun name -> Map.containsKey name pending) with
            | name :: rest -> Some(Map.find name pending, rest)
            | [] ->
                if Map.isEmpty pending then
                    None
                else
                    pending
                    |> Map.toList
                    |> List.map snd
                    |> List.minBy (fun i ->
                        staying i, i.Rank, List.length (openCandidates claimed i), i.Creep)
                    |> fun intent -> Some(intent, [])

        match next with
        | None -> resolved
        | Some(intent, urgent) ->
            let pending = Map.remove intent.Creep pending

            let chosen =
                match openCandidates claimed intent with
                | tile :: _ -> tile
                // Nowhere left to stand: stay put and let the engine fail
                // whichever move contests this tile.
                | [] -> intent.Pos

            let claimed = Set.add chosen claimed
            let resolved = Map.add intent.Creep chosen resolved

            match Map.tryFind chosen occupants with
            | Some other when Map.containsKey other pending ->
                let occupant = Map.find other pending

                let displaced =
                    { occupant with
                        Candidates =
                            (occupant.Candidates |> List.filter ((<>) chosen))
                            @ (if List.contains intent.Pos occupant.Candidates then
                                   []
                               else
                                   [ intent.Pos ])
                    }

                let pending = Map.add other displaced pending

                let urgent =
                    if List.length (openCandidates claimed displaced) < 2 then
                        other :: urgent
                    else
                        urgent

                settle pending urgent claimed resolved
            | _ -> settle pending urgent claimed resolved

    let pending = moveIntents |> List.map (fun i -> i.Creep, i) |> Map.ofList
    settle pending [] blocked Map.empty

/// Direction of a single step between adjacent tiles.
let private directionTo (from: Pos) (dest: Pos) : Direction option =
    match sign (dest.X - from.X), sign (dest.Y - from.Y) with
    | 0, -1 -> Some Top
    | 1, -1 -> Some TopRight
    | 1, 0 -> Some Right
    | 1, 1 -> Some BottomRight
    | 0, 1 -> Some Bottom
    | -1, 1 -> Some BottomLeft
    | -1, 0 -> Some Left
    | -1, -1 -> Some TopLeft
    | _ -> None

/// Resolver, room pass: every rested creep the Atlas places registers a
/// Move Intent, arbitration settles them into at most one single-step move
/// per creep, and the settled standing tiles become move Intents in
/// Snapshot creep order. Takes the tick's assigned Task per creep as data;
/// a creep absent from the map is idle. A fatigued creep sits arbitration
/// out — the engine would answer its move with ERR_TIRED — and its tile is
/// blocked for the tick, so nobody plans a step through it.
///
/// Beside the moves ride the movement Verdicts (ADR 0009), in Snapshot
/// creep order: grounded for each creep whose tile is blocked by fatigue;
/// rerouted for a traveller whose step differs from its traffic-blind one
/// (the occupancy surcharge is the only pricing the two floods do not
/// share); yielded — naming the counterpart holding the tile — for a creep
/// settled off its preferred candidate. A creep that simply steps toward
/// its Work Area, a clean swap included, says nothing: both sides of a
/// swap settle on the tile they asked for.
let resolve (snapshot: Snapshot) atlas (assigned: Map<string, Task>) : Intent list * Verdict list =
    let placed = Atlas.placedCreeps atlas

    let tired =
        snapshot.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let moveIntents =
        placed
        |> List.filter (fun (name, _) -> not (Set.contains name tired))
        |> List.map (fun (name, pos) ->
            moveIntentFor (rank snapshot) atlas name pos (Map.tryFind name assigned))

    let blocked =
        placed
        |> List.choose (fun (name, pos) -> if Set.contains name tired then Some pos else None)
        |> Set.ofList

    let occupants = placed |> List.map (fun (name, pos) -> pos, name) |> Map.ofList
    let standing = arbitrate occupants blocked moveIntents

    let intents =
        placed
        |> List.choose (fun (name, pos) ->
            Map.tryFind name standing
            |> Option.bind (directionTo pos)
            |> Option.map (fun direction -> MoveCreep(name, direction)))

    // Each rested creep's preferred standing tile: the head of its
    // candidate list — a Move Intent's candidates are never empty.
    let preferences =
        moveIntents |> List.map (fun i -> i.Creep, List.head i.Candidates) |> Map.ofList

    // Who holds a tile this creep did not get: the creep settled on it, or
    // the fatigued occupant whose blocked tile pre-claimed it.
    let counterpartAt tile self =
        standing
        |> Map.tryPick (fun name settled ->
            if settled = tile && name <> self then Some name else None)
        |> Option.orElse (
            if Set.contains tile blocked then
                Map.tryFind tile occupants
            else
                None
        )

    let rerouted name task =
        match Atlas.firstStep atlas name task, Atlas.firstStepIgnoringTraffic atlas name task with
        | Some priced, Some blind -> priced <> blind
        | _ -> false

    let verdicts =
        placed
        |> List.collect (fun (name, _) ->
            if Set.contains name tired then
                [ Verdict.Grounded name ]
            else
                let reroute =
                    match Map.tryFind name assigned with
                    | Some task when rerouted name task -> [ Verdict.Rerouted name ]
                    | _ -> []

                let yielded =
                    match Map.tryFind name preferences, Map.tryFind name standing with
                    | Some preferred, Some settled when settled <> preferred ->
                        counterpartAt preferred name
                        |> Option.map (fun other -> Verdict.Yielded(name, other))
                        |> Option.toList
                    | _ -> []

                reroute @ yielded)

    intents, verdicts

/// Matcher: keep still-valid assignments (anti-thrash) and greedily assign
/// the rest. Assignments in, Assignments and the Verdicts explaining them
/// out (ADR 0009): releases first in memory order, then one status Verdict
/// per living creep in Snapshot order — each preceded, for a creep on the
/// verbose list, by its Scoring Verdict: the whole pool judged against the
/// same state its status was decided from. Emission belongs to the
/// Emitter, movement to the Resolver.
let matchCreeps
    (snapshot: Snapshot)
    atlas
    (tasks: Task list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : Assignments * Verdict list =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList
    let capacities = taskCapacities snapshot atlas

    let load (acc: Assignments) tid =
        acc |> Map.filter (fun _ assigned -> assigned = tid) |> Map.count

    let hasCapacity acc tid =
        match Map.tryFind tid capacities with
        | Some cap -> load acc tid < cap
        | None -> true

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed (e.g. across a redeploy). So does
    // reachability: a Work Area the Atlas can no longer reach releases the
    // assignment, freeing its capacity for creeps that can get there —
    // deliberately with no range-based fallback (ADR 0002). Each failed
    // gate names the release; a dead creep's assignment drops silently —
    // Verdicts attribute to living creeps only.
    let kept, released =
        ((Map.empty, []), assignments)
        ||> Map.fold (fun (acc, released) name tid ->
            let release reason =
                acc, Verdict.Released(name, tid, reason) :: released

            match snapshot.Creeps |> List.tryFind (fun c -> c.Name = name) with
            | None -> acc, released
            | Some creep ->
                match Map.tryFind tid byId with
                | None -> release ReleaseReason.TaskGone
                | Some task when not (applicable atlas creep task) ->
                    release ReleaseReason.Inapplicable
                | Some _ when not (hasCapacity acc tid) -> release ReleaseReason.OverCapacity
                | Some task when (Atlas.travelCost atlas creep.Name task).IsNone ->
                    release ReleaseReason.Unreachable
                | Some _ -> Map.add name tid acc, released)

    // One gate cascade judges every (creep, Task) pair — rejected at the
    // first matching gate it fails (applicable, capacity, reachable) or
    // scored on the full key when none does. The Matcher's candidates and a
    // verbose Scoring both read from here, so the narration can never
    // drift from what actually decided the match.
    let judge acc (creep: CreepInfo) task =
        let tid = taskId task

        if not (applicable atlas creep task) then
            Candidate.Rejected(tid, RejectReason.Inapplicable)
        elif not (hasCapacity acc tid) then
            Candidate.Rejected(tid, RejectReason.CapacityFull)
        else
            match Atlas.travelCost atlas creep.Name task with
            | None -> Candidate.Rejected(tid, RejectReason.Unreachable)
            | Some cost -> Candidate.Scored(tid, rank snapshot task, cost, load acc tid)

    let assignOne (acc, verdicts) (creep: CreepInfo) =
        let verdicts =
            if Set.contains creep.Name verbose then
                // The creep's own claim is set aside for its scoring: a held
                // single-Seat Task must read as the winning row, never as
                // capacity-full against its own holder's seat.
                let rows = tasks |> List.map (judge (Map.remove creep.Name acc) creep)
                Verdict.Scoring(creep.Name, rows) :: verdicts
            else
                verdicts

        match Map.tryFind creep.Name acc with
        | Some tid -> acc, Verdict.Kept(creep.Name, tid) :: verdicts
        | None ->
            let judged = tasks |> List.map (fun t -> t, judge acc creep t)

            let keyed =
                judged
                |> List.choose (function
                    | t, Candidate.Scored(_, rank, cost, load) -> Some((rank, cost, load), t)
                    | _ -> None)

            match keyed with
            | [] ->
                // How far the best Task got through the gates — applicable,
                // capacity, reachable — is why the creep sits idle.
                let rejectedWith wanted =
                    judged
                    |> List.exists (function
                        | _, Candidate.Rejected(_, reason) -> reason = wanted
                        | _ -> false)

                let reason =
                    if List.isEmpty tasks then
                        IdleReason.NoTasks
                    elif rejectedWith RejectReason.Unreachable then
                        IdleReason.NoneReachable
                    elif rejectedWith RejectReason.CapacityFull then
                        IdleReason.NoneFree
                    else
                        IdleReason.NoneApplicable

                acc, Verdict.Unassigned(creep.Name, reason) :: verdicts
            | keyed ->
                let bestKey, task = keyed |> List.minBy fst

                // The deciding factor: the first component separating the
                // winner from its closest rival, or the pool-order tie-break
                // when the whole key ties.
                let factor =
                    match keyed |> List.filter (fun (_, t) -> t <> task) with
                    | [] -> MatchFactor.OnlyCandidate
                    | rivals ->
                        let bestRank, bestCost, bestLoad = bestKey
                        let rivalRank, rivalCost, rivalLoad = rivals |> List.map fst |> List.min

                        if rivalRank <> bestRank then MatchFactor.Rank
                        elif rivalCost <> bestCost then MatchFactor.TravelCost
                        elif rivalLoad <> bestLoad then MatchFactor.Load
                        else MatchFactor.PoolOrder

                Map.add creep.Name (taskId task) acc,
                Verdict.Matched(creep.Name, taskId task, factor) :: verdicts

    let next, statuses = snapshot.Creeps |> List.fold assignOne (kept, [])
    next, List.rev released @ List.rev statuses

/// Join the Matcher's Assignments back onto the Planner's pool: the tick's
/// assigned Task per creep, as data for the Emitter and the Resolver.
let private assignedTasks (tasks: Task list) (assignments: Assignments) : Map<string, Task> =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> Map.tryFind tid byId |> Option.map (fun t -> name, t))
    |> Map.ofList

/// The decision seam: Snapshot in — with the verbose list of creep names
/// owed full candidate scoring — Decision out. The tick's pipeline is visible here — plan, match, emit, resolve —
/// beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same
/// flood (ADR 0004).
let decide (snapshot: Snapshot) (assignments: Assignments) (verbose: Set<string>) : Decision =
    let atlas = Atlas.ofSnapshot snapshot
    let defenseIntents = planSafeMode snapshot
    let spawnIntents = planSpawns snapshot atlas
    let siteIntents = planLayout snapshot atlas
    let pickupIntents = planPickups snapshot atlas
    let tasks = planTasks snapshot
    let next, verdicts = matchCreeps snapshot atlas tasks assignments verbose
    let assigned = assignedTasks tasks next
    let moveIntents, moveVerdicts = resolve snapshot atlas assigned

    {
        Intents =
            defenseIntents
            @ spawnIntents
            @ siteIntents
            @ pickupIntents
            @ emit snapshot atlas assigned
            @ moveIntents
        Assignments = next
        Verdicts = verdicts @ moveVerdicts
    }
