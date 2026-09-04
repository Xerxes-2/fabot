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

/// Screeps source regen: 3000 energy per 300 ticks — the output per tick
/// a continuously drained source yields, and what its container's hauler
/// share must ship.
let private sourceOutputPerTick = 10

/// Screeps HARVEST_POWER: energy one Work part digs from a source a tick.
let private harvestPerWork = 2

/// The Anchor row's Work ceiling (ADR 0021): the Work that saturate one
/// source — dig its whole regeneration in the regeneration time — plus
/// one spare. Past saturation a further Work only drains the source
/// sooner and idles until it regenerates; the spare drains it 50 ticks
/// early, and those ticks absorb an unmanned Post's gap (death, recast,
/// the walk back) at no cost in output.
let private anchorWorkCap = sourceOutputPerTick / harvestPerWork + 1

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
/// the remaining energy affords on Work up to the row's ceiling — spawn
/// energy buys output rather than mobility the Post never uses, and
/// stops where the source has no more to give (ADR 0021). Exempt from
/// fatigue parity (ADR 0006); never below the row's two-Work block.
let private anchorBodyFor capacity =
    let work =
        (capacity - bodyCost [ Carry; Move ]) / bodyCost [ Work ]
        |> max 2
        |> min anchorWorkCap

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
    | Withdraw storeId -> $"withdraw:{storeId}"
    | Refill structureId -> $"refill:{structureId}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"

/// The Repair trigger: a repairable structure enters the pool when its
/// hits sink strictly below this fraction of max, and leaves it once
/// repaired back over the line. A tunable, not part of ADR 0010.
let private repairTrigger = 0.5

/// Screeps CONTAINER_CAPACITY: what a container's store can hold — the
/// line past which the buffer needs no Refill.
let private containerCapacity = 2000

/// Screeps STORAGE_CAPACITY: what the Storage's store can hold — the line
/// past which the stock needs no Refill. Read against stored *energy*, as
/// the container line is, because energy is the only resource this colony
/// ever holds; the day it holds another, the Storage's free capacity has
/// to be projected rather than inferred from one resource.
let private storageCapacity = 1000000

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
    // Harvest exists for every source, drained or not (ADR 0013, revised
    // by ADR 0025): the task no longer flickers with the source's stock,
    // because whether a dry rock is worth walking to depends on the
    // walker's body and position — the Matcher's knowledge, not the
    // creep-blind Planner's.
    let harvests = snapshot.Sources |> List.map (fun s -> Harvest s.Id)

    let refills =
        snapshot.Refillables
        |> List.filter (fun r -> r.FreeCapacity > 0)
        |> List.map (fun r -> Refill r.Id)

    let builds = snapshot.ConstructionSites |> List.map (fun site -> Build site.Id)

    // A Repair per repairable structure below the trigger, in id order.
    // The projection carries hits on repairable kinds only, but the kind
    // gate is judged here — the Planner owns what enters the pool, off the
    // same predicate the projection filtered by (ADR 0010, ADR 0012).
    let repairs =
        snapshot.Spatial.Hits
        |> Map.toList
        |> List.filter (fun (id, hits) ->
            match Map.tryFind id snapshot.Spatial.TargetKinds with
            | Some(Structure kind) when isRepairable kind ->
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

    // The standing structures of one built kind, in id order. Both the
    // containers and the Storage are pooled by the projection's kind —
    // never by position, never by name — so the rule is written once.
    let targetsOfKind kind =
        snapshot.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, k) -> if k = Structure kind then Some id else None)

    let containers = targetsOfKind BuiltKind.Container
    let storages = targetsOfKind BuiltKind.Storage

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

    // The colony's stock is the outflow's last stop (ADR 0023): a standing
    // Storage with room is one more Refill target, on the deepest tier of
    // all. Recognised by the projection's kind, as the tier is — the
    // Layout puts the one Storage on the cluster's first pick, so no
    // position rule could name it.
    let storageRefills =
        storages
        |> List.filter (fun id -> stored id < storageCapacity)
        |> List.map Refill

    // The stock's other half (ADR 0023): a stocked Storage is a Withdraw
    // source too, but only while the pool holds a Refill whose target is
    // not the stock itself — some sink other than it has room. Its own
    // Refill is deliberately no such sink: counting it would gate the
    // Storage open against itself for as long as it had room, and a hauler
    // beside a store that is both its only intake and its only sink cycles
    // energy in and out of it tick after tick — the ADR 0019 loop in the
    // one shape that gate cannot cure, since the bodies that must feed the
    // spawn from the stock are the ones with no Work part. The ADR 0013
    // shape: the Task exists while the condition holds, so a holder
    // mid-trip is released through task-gone the tick the last other sink
    // fills. What the gate closes is the cycle with nowhere else to go —
    // both halves can still be pooled on one tick, and a part-loaded
    // hauler is applicable to both; there the tier gap is what carries the
    // load away instead of putting it back, the draw shallower than every
    // sink but the flow's own and the stock's Refill deeper than all of
    // them.
    let storageWithdraws =
        if List.isEmpty refills && List.isEmpty containerRefills then
            []
        else
            storages |> List.filter (fun id -> stored id > 0) |> List.map Withdraw

    harvests
    @ withdraws
    @ refills
    @ builds
    @ repairs
    @ upgrades
    @ containerRefills
    @ storageRefills
    @ storageWithdraws

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

/// Screeps CREEP_SPAWN_TIME: the ticks a spawner spends per body part —
/// the half of a lead that is paid before the replacement takes its first
/// step.
let private spawnTicksPerPart = 3

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

/// Whether a living body was cast from the hauler row: Carry parts but no
/// Work. The worker and anchor rows both keep at least one Work, and only
/// the hauler row casts none (ADR 0012) — so, like the anchor's
/// Work > Move, the casting pattern is readable off the body itself; the
/// row name in a creep's name stays observability only.
let private isHaulerBody (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    count Work = 0 && count Carry > 0

/// The pattern row a living body was cast from, read off the parts alone
/// (ADR 0006): more Work than Move is the anchor row, no Work beside a
/// Carry is the hauler row, and every other body is the generalist. The
/// row is what sizes the replacement a lead prices (ADR 0026), so the one
/// rule serves every row and none of them needs a constant of its own.
let private patternOf atlas (creep: CreepInfo) =
    if Atlas.workHeavy atlas creep.Name then anchorPattern
    elif isHaulerBody creep then haulerPattern
    else workerPattern

/// A creep's lead (ADR 0026): the ticks its replacement needs to stand
/// where it stands — the successor body's cast time plus that body's walk
/// out of the spawn, priced for the successor's own fatigue factor and not
/// the incumbent's. The body is the creep's own row at the bank's
/// capacity, so a slow Anchor earns a long lead and a hauler on a trunk a
/// short one. The walk starts beside the spawner rather than on it, where
/// the engine actually places the finished creep: a lead that charged the
/// step out of the spawner's tile would cast the successor that much too
/// early and leave it reading the incumbent's Post as full for the first
/// ticks of its life — the mispricing ADR 0026 names NoneFree as the
/// symptom of. Several spawns resolve as the hauler quota resolves them,
/// at the cheapest: the shortest lead is the optimistic bound on when a
/// replacement could stand there, and the row's quota is already read that
/// way — which spawn the colony actually casts from is the spawn fold's
/// own business. Geometry that prices nothing leads nobody (ADR 0004) — a
/// creep the projection cannot place, a colony whose spawns it cannot
/// place, and a tile no spawn can reach each answer 0, and a lead of 0
/// leaves every living creep counted.
let private leadOf (snapshot: Snapshot) atlas (creep: CreepInfo) : int =
    let pattern = patternOf atlas creep

    let tile =
        Atlas.placedCreeps atlas
        |> List.tryPick (fun (name, pos) -> if name = creep.Name then Some pos else None)

    match tile with
    | None -> 0
    | Some tile ->
        snapshot.Spawns
        |> List.choose (fun s ->
            match Atlas.positionOf atlas s.Id with
            | None -> None
            | Some spawnPos ->
                let bank =
                    snapshot.RoomEnergy
                    |> Map.tryFind s.RoomName
                    |> Option.defaultValue { Available = 0; Capacity = 0 }

                let body = bodyFor pattern bank.Capacity

                Atlas.castWalkTicks atlas body spawnPos tile
                |> Option.map (fun walk -> spawnTicksPerPart * List.length body + walk))
        |> function
            | [] -> 0
            | leads -> List.min leads

/// Whether a creep is expiring (ADR 0026): its remaining life is at or
/// under its lead, so it will be dead before a replacement cast now could
/// stand where it stands. It leaves the workforce's living count and its
/// row's gap, which is what casts the successor while it still works. It
/// is never released for it — anti-thrash keeps it on its Task to the last
/// tick, and the Post the two share for the lead's duration is the
/// succession, not an oversell.
let private expiring (snapshot: Snapshot) atlas (creep: CreepInfo) =
    creep.TicksToLive <= leadOf snapshot atlas creep

/// Pre-Task bootstrap step: spawn Intents needed to keep the workforce at
/// the Workforce target. Spawning is a colony-level need, not a Task creeps
/// get matched to, so it sits beside the Planner/Matcher pipeline rather
/// than inside it.
let private planSpawns (snapshot: Snapshot) atlas (haulerQuota: int) : Intent list =
    // The specialist rows' quota rules (ADR 0006, ADR 0012): one Anchor
    // per Post, haulers per the throughput arithmetic — the hauler quota
    // arrives memoised on the census signature (ADR 0017), its input set
    // a subset of the Layout's. Both are addends of the target itself —
    // inside it by construction, never on top of it.
    let anchorQuota = Atlas.posts atlas |> Set.count
    let target = workforceTarget snapshot atlas anchorQuota haulerQuota

    // The deficit and both row gaps count the creeps that will still be
    // alive when a replacement could arrive: an expiring creep is already
    // outside the count (ADR 0026), so its successor is cast while it
    // still works rather than after it dies. The disaster fallback below
    // still reads the creep list itself — an expiring creep can refill an
    // extension, and a colony holding one is not the empty one.
    let living =
        snapshot.Creeps
        |> List.filter (fun creep -> not (expiring snapshot atlas creep))

    let deficit = target - List.length living

    // A body is sized to the bank's capacity and cast the tick the bank
    // holds its cost (ADR 0021) — a full bank for rows priced at
    // capacity, sooner for the capped Anchor row. Disaster fallback: an
    // empty colony can never refill extensions, so a capacity-sized body
    // would wait forever — spawn a minimal worker unit from whatever is
    // banked right now; time-to-first-creep outranks specialisation, so
    // the anchor gap waits (ADR 0006).
    let castFromBank pattern (bank: RoomEnergy) =
        if List.isEmpty snapshot.Creeps then
            if bank.Available >= bodyCost workerPattern.Block then
                Some(workerPattern, workerPattern.Block)
            else
                None
        else
            let body = bodyFor pattern bank.Capacity

            if bank.Available >= bodyCost body then
                Some(pattern, body)
            else
                None

    if deficit <= 0 then
        []
    else
        // Anchor gaps are filled before hauler gaps, hauler gaps before
        // generalist gaps — the casting order runs Anchor, hauler, worker
        // — and the worker row's quota is whatever the target has left.
        let anchorGap =
            anchorQuota
            - (living
               |> List.filter (fun creep -> Atlas.workHeavy atlas creep.Name)
               |> List.length)
            |> max 0

        let haulerGap =
            haulerQuota - (living |> List.filter isHaulerBody |> List.length) |> max 0

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

/// The claimer range at which safe mode fires (ADR 0015): the precise
/// deadline is 2 — attackController is range 1 and judged from
/// tick-start position, and a creep steps at most one tile a tick, so
/// activating at 2 always lands before the tap — plus one tile of
/// margin for a skipped tick.
let private safeModeDeadline = 3

/// Colony reflex beside the pipeline: a CLAIM-part hostile is the one
/// threat that can disarm safe mode itself — attackController blocks
/// activation for 1,000 ticks. But the tap is a range-1 act, so the
/// activation holds until a claimer stands within reach of landing it
/// (ADR 0015) — the hold is free (activation still wins the race) and
/// buys the towers their window to kill the claimer en route. A
/// controller the projection cannot place has no deadline to measure
/// and falls back to firing on sight. Fighters without CLAIM cannot
/// touch the controller and never spend the stock: at RCL2 safe mode
/// outlasts any invader raid 13×, so it keeps for when the room is
/// actually being taken (ADR 0007).
let private planSafeMode (snapshot: Snapshot) atlas : Intent list =
    match snapshot.Controller with
    | Some controller when controller.SafeModeAvailable > 0 && not controller.SafeModeActive ->
        let withinReach (h: HostileInfo) =
            List.contains Claim h.Body
            && match Atlas.positionOf atlas controller.Id with
               | Some pos -> range h.Pos pos <= safeModeDeadline
               | None -> true

        if snapshot.Hostiles |> List.exists withinReach then
            [ ActivateSafeMode controller.Id ]
        else
            []
    | _ -> []

/// Colony reflex beside the pipeline (ADR 0014): every tower shoots the
/// hostile nearest to itself, every tick one stands in the room. Attack
/// only — no tower repair or heal — per-tower nearest with no focus fire
/// or anti-drain gate, and no energy gate: unlike safe mode there is no
/// stock to protect, so a dry tower's Intent fails harmlessly. Equal
/// ranges tie-break by hostile id, so the pick is deterministic.
let private planFire (snapshot: Snapshot) atlas : Intent list =
    match snapshot.Hostiles with
    | [] -> []
    | hostiles ->
        Atlas.placedTowers atlas
        |> List.map (fun (towerId, pos) ->
            let target = hostiles |> List.minBy (fun h -> range pos h.Pos, h.Id)
            FireTower(towerId, target.Id))

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

/// Storages the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "storage").
let private storageAllowance level =
    match level with
    | 0
    | 1
    | 2
    | 3 -> 0
    | _ -> 1

/// The level the engine unlocks the Storage at. The Layout reserves the
/// Storage's whole allowance here rather than at the horizon (ADR 0022):
/// the Storage is not a clustered kind, and its tile never comes back
/// once an extension takes it, so the reservation must outlive any
/// revisit of the horizon.
let private storageLevel = 4

/// The Layout horizon (ADR 0011): the whole plan is computed up to this
/// level regardless of the current one, so today's roads route around
/// tomorrow's structures. Deliberately not RCL8 — a wider reservation
/// would tax today's trunks with detours for structures five levels away.
let private horizonLevel = 4

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR 0011), computed whole from the Atlas every
/// tick and placed all at once — no persisted plan, no pacing. One
/// ordering rule eats every clustered structure: buildable tiles on the
/// spawn's checkerboard colour, nearest-to-spawn first, the working
/// ground excluded, the Storage's pick coming before the tower's and both
/// before the extensions' (ADR 0022). Trunk roads pave each source to the
/// controller and to each spawn plus the swamps of the controller's Work
/// Area, priced on raw terrain and routed around every reserved tile —
/// reservations come first, so a road never sits where a structure will.
/// One tile beside each container pick and beside the Storage is held as
/// a Link footing (ADR 0022) and outranks the tower and the extensions;
/// the reservation is widened by the footing count so the tiles they push
/// the cluster onto are reserved too (ADR 0027). No link is ever placed
/// on one — Link is a built kind only, and which footings are filled is
/// RCL5's decision.
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

        // The working ground — every source's Seats and the controller's
        // Upgrade Work Area — is off-limits (ADR 0022): a clustered
        // structure there eats a tile an Anchor or an upgrader stands on,
        // so a colony whose nearest same-colour tiles are working ground
        // clusters one ring out instead of eating them.
        let working = Atlas.workingGround atlas

        let buildable = Atlas.buildableTiles atlas

        let ordering =
            buildable
            |> List.filter (fun tile ->
                (tile.X + tile.Y) % 2 = parity && not (Set.contains tile working))
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)

        // A kind's still-open gap at a level: its allowance there minus the
        // projection's censuses of standing and pending structures. Judged
        // at the level the kind is reserved for it sizes the reservation;
        // at the current level it sizes the placement.
        let gapAt allowanceOf built pending level =
            allowanceOf level - built - pending |> max 0

        let storageGap =
            gapAt storageAllowance (Atlas.builtStorages atlas) (Atlas.pendingStorages atlas)

        let towerGap =
            gapAt towerAllowance (Atlas.builtTowers atlas) (Atlas.pendingTowers atlas)

        let extensionGap =
            gapAt extensionAllowance (Atlas.builtExtensions atlas) (Atlas.pendingExtensions atlas)

        // The still-unclaimed slots, Storage first and tower next: a built
        // or pending structure keeps its tile out of the ordering (it is a
        // target) and its slot off the plan. The clustered kinds are sized
        // at the horizon; the Storage is not one of them and reads none
        // (ADR 0022) — its whole allowance is held from level 0, because
        // once an extension takes that tile it never comes back.
        let storageSlots = storageGap storageLevel
        let towerSlots = towerGap horizonLevel
        let extensionSlots = extensionGap horizonLevel

        // The Link footings cannot be named here — their targets are the
        // container picks, which are derived from the trunks the
        // reservation is for — but their count can: one per source, one
        // for the controller container, one for the Storage. The window is
        // widened by that many, so the tiles the cluster is pushed onto
        // when a footing takes one of its picks are inside the reservation
        // too (ADR 0027). Without the widening a pick lands past the
        // window on ground the flood never dodged, and the tick its site
        // stands the trunk reroutes around it.
        let footingSlots = List.length snapshot.Sources + 2

        let clustered =
            ordering
            |> List.truncate (storageSlots + towerSlots + extensionSlots + footingSlots)

        let storagePick = ordering |> List.truncate storageSlots

        // Reserved before trunks: a trunk never crosses a tile a reserved
        // structure will claim, and the widened window holds the footings
        // as well — so the precedence runs one way for every kind the
        // Layout places (ADR 0011). The footings' own tiles are settled
        // below, once the trunks are known.
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
        // the plain ground does not. No reservation can stand here: the
        // Work Area is working ground, which the ordering never offered
        // (ADR 0022).
        let workAreaSwamps = upgradeArea |> Set.filter (Atlas.isSwamp atlas)

        // Every tile the Layout paves: the trunks plus the Work Area's
        // swamps. The road gap measures this against the projection's road
        // census, and a Link footing is chosen off it.
        let roadPlan = Set.union trunkTiles workAreaSwamps

        // The road gap reads the projection's road census: a built road or a
        // pending road site already claims its tile (ADR 0010).
        let roadGap =
            Set.difference roadPlan (Atlas.roadTiles atlas)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTiles atlas)

        // Containers (ADR 0012), computed whole like everything else and
        // RCL-gated by nothing — the engine allows them from level 0. Each
        // source's container sits on the Seat nearest that source's trunk;
        // the trunk's first tile is itself a Seat, so in practice the
        // container lands where the trunk leaves the source and harvest
        // overflow falls straight in. Seats are terrain geometry and
        // trunks avoid only the reservations, so the pick never shifts as
        // the container itself gets built. Seats need no reservation dodge:
        // they are working ground, which the clustered ordering never
        // offered (ADR 0022).
        let sourceContainerTiles =
            sourceTrunks
            |> List.choose (fun (sourceId, trunk) ->
                let seats = Atlas.seatTilesOf atlas sourceId

                if Set.isEmpty trunk || Set.isEmpty seats then
                    None
                else
                    seats
                    |> Set.toList
                    |> List.minBy (fun seat ->
                        trunk |> Set.toList |> List.map (range seat) |> List.min, seat.X, seat.Y)
                    |> Some)

        // The controller container: an Upgrade-Work-Area tile beside a
        // trunk and off the road itself — the buffer upgraders work from
        // standing still, one tile from where the haulers drive. No
        // reservation to dodge either: the Work Area is working ground
        // (ADR 0022). Judged from the same stable geometry as the
        // Seat pick, so a standing container recomputes to its own tile.
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.bind (fun controllerPos ->
                upgradeArea
                |> Set.filter (fun tile ->
                    not (Set.contains tile trunkTiles)
                    && not (Set.contains tile workAreaSwamps)
                    && trunkTiles |> Set.exists (fun t -> range tile t = 1))
                |> Set.toList
                |> function
                    | [] -> None
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile controllerPos, tile.X, tile.Y)
                        |> Some)

        // The Link footings (ADR 0022): one tile held for a link beside
        // every target a link will ever serve — each planned source
        // container, the controller container, and the Storage. Planned,
        // not built: a Post needs a standing container, so a Post-anchored
        // rule would reserve nothing at level 0 and the tiles would be
        // gone by the time links arrive. A room with three sources gets
        // five footings; the count is the rule's, never a constant
        // (ADR 0027).
        //
        // The tiles are settled here rather than with the reservation
        // because a footing's targets are the container picks, and those
        // are derived from the trunks the reservation is for. Only the
        // count could be held ahead of the trunks, and it was: a footing
        // still yields to them by being chosen off the finished trunk
        // plan, but the tiles it pushes the cluster onto were reserved.
        // Re-flooding the trunks to name the tiles first would pay the
        // tick's dearest step twice for the same answer (ADR 0017).
        //
        // One footing per target, and a tile is only ever one target: a
        // Seat that is also the controller container's pick would
        // otherwise be served twice and hold two tiles for one link.
        let footingTargets =
            sourceContainerTiles
            @ Option.toList controllerContainerTile
            @ storagePick
            @ (Set.union (Atlas.storageTiles atlas) (Atlas.pendingStorageTiles atlas)
               |> Set.toList)
            |> List.distinct

        // A standing link is a target, so its own footing has stopped
        // being buildable: added back, or the footing would jump the tick
        // the link went up. The working ground is deliberately not
        // subtracted — a footing is the one structure footing allowed
        // there (ADR 0022), because a link on a Seat or an Upgrade tile is
        // exactly what buys the Anchor and the upgraders a transfer
        // without leaving their tile.
        let footingCandidates = Set.union (Set.ofList buildable) (Atlas.linkTiles atlas)

        let footings =
            (Set.empty, footingTargets)
            ||> List.fold (fun taken target ->
                footingCandidates
                |> Set.filter (fun tile ->
                    range tile target = 1
                    && not (Set.contains tile roadPlan)
                    && not (List.contains tile footingTargets)
                    && not (Set.contains tile taken))
                |> Set.toList
                |> function
                    | [] -> taken
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile spawnPos, tile.X, tile.Y)
                        |> fun tile -> Set.add tile taken)

        // The tower and the extensions take the ordering again with the
        // footings held out — a footing outranks both — and the Storage's
        // pick held out with them: it outranks the footings, which are
        // anchored on it. Each footing draws one more tile in behind it,
        // and the reservation was widened by exactly that many, so no pick
        // reaches past the window the trunks dodged.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick) && not (Set.contains tile footings))
            |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clusterPicks |> List.splitAt (min towerSlots clusterPicks.Length)

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

        place Storage (storagePick |> List.truncate (storageGap controller.Level))
        @ place Tower (towerTiles |> List.truncate (towerGap controller.Level))
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

/// Ticks until a source restocks (ADR 0025), 0 while it holds energy —
/// and 0 for a source the Snapshot does not carry at all, so a source
/// nothing projects never holds a decision up.
let private ticksToRestock (snapshot: Snapshot) sourceId =
    snapshot.Sources
    |> List.tryFind (fun s -> s.Id = sourceId)
    |> Option.map (fun s -> s.TicksToRestock)
    |> Option.defaultValue 0

/// Whether a creep garrisons a source's container Post: ADR 0024's
/// condition — a Work-heavy body standing on that source's built
/// container. The full-store reprieve and the empty-window reprieve are
/// one judgement (ADR 0025), so both gates ask it here rather than
/// spelling the pair out twice: that tile is the garrison's job whatever
/// its store or the source holds.
let private garrisons atlas (creep: CreepInfo) sourceId =
    Atlas.workHeavy atlas creep.Name
    && Atlas.catchesOverflow atlas creep.Name sourceId

/// Whether a Task's time has not come for this creep (ADR 0025, repriced
/// by ADR 0029): a drained source's Harvest is applicable only when the
/// creep's walk covers the restock wait — walk ≥ ticks to restock, with no
/// slack, because the wait shrinks by one each tick while the walk stays
/// put, so a creep one tick short departs one tick later and arrives as
/// the energy does. The walk is the Atlas's own query and nothing here
/// converts anything: it is already whole ticks, floored at one a tile and
/// blind to today's traffic, so a bystander in the lane cannot dispatch a
/// creep this tick and recall it the next. A creep already beside a dry
/// rock has no walk to cover anything and is released, exactly as ADR 0013
/// released it. One exemption, on ADR 0024's condition and no other: the
/// garrison keeps its Post through the empty window. A Dual Seat Anchor
/// gets none: Upgrade is in place there, so it upgrades through the window
/// and rematches Harvest once its Carry is spent. Every other Task is
/// judged at the current tick.
/// Two consequences worth naming, both ADR 0004's totality. The walk
/// answers 0 for an unplaced creep or target, and this gate reads that as
/// an arrival of now — the one place unpriceable geometry holds a Task up,
/// waiting out a drained source exactly as a creep on its Seat does.
/// Exempting it would assign a creep to waiting instead, the stranding ADR
/// 0013 fixed, and the outcome is 0013's own: no Task at all for a drained
/// source. And an unreachable Work Area has no walk at all, which is not
/// earliness: the reachability gate stands ahead of this one and names
/// that rejection itself.
/// The walk arrives deferred because only this arm and the capacity gate
/// spend it, and pricing one for every Task in the pool measured at a
/// third of the tick (ADR 0029).
let private tooEarly (snapshot: Snapshot) atlas (creep: CreepInfo) task (walk: Lazy<int option>) =
    match task with
    | Harvest sourceId ->
        match walk.Value with
        // No walk at all is unreachable geometry, which is not earliness:
        // the reachability gate stands ahead of this one in both cascades
        // and names that rejection itself (ADR 0002, ADR 0029).
        | None -> false
        | Some ticks ->
            ticks < ticksToRestock snapshot sourceId && not (garrisons atlas creep sourceId)
    | Withdraw _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _ -> false

/// Whether a creep can usefully work this Task right now. The body must
/// physically be able to do it — Work-part tasks need a Work part, energy
/// delivery needs a Carry part — and the energy state must call for it: a
/// full creep is done harvesting; an empty creep has nothing to deliver.
/// One geometric widening (ADR 0012), body-aware since ADR 0024: a full
/// Work-heavy creep standing on a built source container keeps Harvest —
/// the engine drops the overflow into the container underfoot, so the
/// creep effectively has capacity and the Post stays garrisoned. A light
/// body gets no such reprieve: its full store ends its dig wherever it
/// stands, or it would hold the Post for the rest of its life — never
/// Inapplicable, so never released — and lock the garrison out of the one
/// tile it can work from. Gates read part arithmetic, never names or
/// roles (ADR 0006) — including one comparative gate (ADR 0016): a body
/// with more Work than Move never Withdraws, so its only feeding-tier
/// candidate is Harvest and an unmanned Post wins it regardless of
/// distance. Travel cost pins an Anchor that is at its Post; this gate
/// is what walks one home past a nearer stocked container.
/// A second gate stands at the other end of the haul cycle, this one
/// reading the target's kind beside the body (ADR 0019): only a creep
/// with a Work part draws from the controller's upgrade buffer. A body without one can spend nothing at the
/// controller, so its Withdraw there is energy flowing back the way it
/// came — and with every other sink full, the buffer is also its only
/// Refill target, which cycled a hauler in and out of one container tick
/// after tick. Source containers stay open to every carrier, and so does
/// the Storage: the gate is scoped to the buffer by id, and the stock's own
/// in-and-out cycle is closed in the Planner instead (ADR 0023), because
/// the bodies that must feed the spawn from it are the ones with no Work.
let private applicable atlas (creep: CreepInfo) task =
    let has part =
        creep.Body |> Map.tryFind part |> Option.exists (fun n -> n > 0)

    match task with
    | Harvest sourceId -> has Work && (creep.FreeCapacity > 0 || garrisons atlas creep sourceId)
    | Withdraw storeId ->
        has Carry
        && creep.FreeCapacity > 0
        && not (Atlas.workHeavy atlas creep.Name)
        && (has Work || not (Set.contains storeId (Atlas.controllerContainers atlas)))
    | Refill _ -> has Carry && creep.Energy > 0
    | Build _
    | Repair _
    | Upgrade _ -> has Work && creep.Energy > 0

let private intentFor (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> HarvestSource(creep.Name, sourceId)
    | Withdraw storeId -> WithdrawEnergyFromStructure(creep.Name, storeId)
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

/// The tier of work a Task belongs to, once its target is taken into
/// account (ADR 0010, ADR 0012, ADR 0023) — what the matcher ranks by.
type private Tier =
    /// Feeding the economy: Harvest, a container's Withdraw, and the
    /// Refill of a spawn or an extension — the flow the colony's
    /// reproduction runs on.
    | Feeding
    /// The Storage's Withdraw (ADR 0023): the colony's stock as an
    /// intake, one tier below the source containers the flow fills. An
    /// empty creep empties those first and draws on the stock only when
    /// they are dry, so a stock standing beside the spawn never wins the
    /// travel-cost tie the containers have to win. Below the whole
    /// feeding tier, not just its Withdraws: there is no rank between a
    /// container's Withdraw and the spawn Refill it feeds, so a hungry
    /// spawn outbids a stock underfoot too and a part-loaded hauler
    /// delivers what it has rather than topping up first — the price of
    /// ordering the stock under the flow it must never outbid.
    | StockDraw
    /// Surplus work: a tower Refill (ADR 0010), Build, Repair and
    /// Upgrade. The colony feeds its own reproduction before its guns,
    /// and everything it merely spends energy on waits behind the flow.
    | Surplus
    /// The controller container's Refill (ADR 0012): a full creep beside
    /// the buffer sinks its load into the controller rather than dumping
    /// it back into the container it just drew from and orbiting in
    /// place, so the buffer is filled by bodies with no surplus work of
    /// their own.
    | UpgradeBuffer
    /// The Storage's Refill (ADR 0023): the colony's stock, deeper than
    /// every sink that spends. A load reaches it only when there is
    /// nowhere else at all to put it, the upgrade buffer included, so the
    /// stock never outbids the flow, however close beside the spawn it
    /// stands (ADR 0023's own motivating case).
    | Stock

/// The matcher's whole tier order, shallowest first — the one place the
/// ordering lives (ADR 0010, ADR 0012, ADR 0023): the flow is fed, then
/// the stock is drawn on, then surplus is spent, then whatever is left
/// sinks into the upgrade buffer, and what even the buffer cannot hold is
/// stocked. The stock's two roles sit on either side of the surplus work
/// the colony does between them. Exhaustive over Tier on purpose — a tier
/// this sequence forgets is a build error, not a Task that silently ranks
/// below every other. The downgrade deadline (ADR 0007) is the one thing
/// above the sequence rather than in it; `rank` carries it.
let private rankOfTier =
    function
    | Feeding -> 0
    | StockDraw -> 1
    | Surplus -> 2
    | UpgradeBuffer -> 3
    | Stock -> 4

/// The tier a Task sits in. Refill and Withdraw are the two Tasks whose
/// tier layers by target (ADR 0010, ADR 0023), and both read the layer off
/// the projection's kind — the stock is recognised for what it is, never
/// for where it stands. On Refill: the Storage and the container are each
/// one projected kind, and the projection holds one kind per id, so those
/// two answers exclude each other by construction; a tower is read off the
/// Refillables census instead, which can overlap either. So the kind is
/// asked first — deepest answer first — and the census only of what the
/// kind leaves. Any container answers UpgradeBuffer, not the controller's
/// alone; it is the Planner that pools only the controller's (range 3 of
/// the controller, never a source container's tile), so no other container
/// reaches here. Everything the three tests miss is a spawn or an
/// extension: the flow. On Withdraw the layering is the one line: the
/// stock is drawn on a tier below every container, source and controller
/// alike.
let private tierOf (snapshot: Snapshot) task =
    match task with
    | Harvest _ -> Feeding
    | Withdraw storeId ->
        let kind = Map.tryFind storeId snapshot.Spatial.TargetKinds

        if kind = Some(Structure BuiltKind.Storage) then
            StockDraw
        else
            Feeding
    | Refill structureId ->
        let isTower =
            snapshot.Refillables
            |> List.exists (fun r -> r.Id = structureId && r.Kind = BuiltKind.Tower)

        let kind = Map.tryFind structureId snapshot.Spatial.TargetKinds

        if kind = Some(Structure BuiltKind.Storage) then
            Stock
        elif kind = Some(Structure BuiltKind.Container) then
            UpgradeBuffer
        elif isTower then
            Surplus
        else
            Feeding
    | Build _
    | Repair _
    | Upgrade _ -> Surplus

/// Whether the controller stands inside its downgrade deadline (ADR 0007).
let private insideDowngradeDeadline (snapshot: Snapshot) =
    snapshot.Controller
    |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

/// One rank above the shallowest tier: where the downgrade deadline puts
/// Upgrade (ADR 0007). Not a tier of its own — "never let it downgrade"
/// is an ordering imposed on the sequence, not a tier of work.
let private deadlineRank = -1

/// Matching tier between applicable tasks (lower wins): the rank of the
/// Task's tier. One exception: a controller inside the downgrade deadline
/// makes Upgrade the colony's most urgent work, outranking even the
/// feeding tier (ADR 0007).
let private rank (snapshot: Snapshot) task =
    match task with
    | Upgrade _ when insideDowngradeDeadline snapshot -> deadlineRank
    | _ -> tierOf snapshot task |> rankOfTier

/// Concurrent-worker cap per task id; tasks absent from the map are
/// unbounded. Harvest is capped by its source's Seat count — a source the
/// projection does not place derives no cap, so behaviour without terrain
/// data is unchanged.
let private taskCapacities (snapshot: Snapshot) atlas : Map<string, int> =
    snapshot.Sources
    |> List.choose (fun s ->
        Atlas.seats atlas s.Id |> Option.map (fun count -> taskId (Harvest s.Id), count))
    |> Map.ofList

/// Concurrent Work-heavy-harvester cap per Harvest task id (ADR 0024): the
/// source's Post count, the standing room a heavy body actually has — its
/// Harvest Work Area is that source's Posts alone (ADR 0020), so the Seat
/// cap would admit garrisons to tiles they may not work from and pile two
/// Anchors onto one Post. A source with no Post derives no cap: nothing
/// narrows a heavy body's area there (the pre-container fallback), and the
/// Seat cap is the only one. Rides beside the Seat cap rather than
/// replacing it — a Post is a capacity unit of its own, and both must hold.
let private postCapacities (snapshot: Snapshot) atlas : Map<string, int> =
    snapshot.Sources
    |> List.choose (fun s ->
        match Atlas.postsOf atlas s.Id |> Set.count with
        | 0 -> None
        | count -> Some(taskId (Harvest s.Id), count))
    |> Map.ofList

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position, and — for Harvest alone
/// — only while the source holds energy (ADR 0025). Anticipatory dispatch
/// and the occupancy surcharge (ADR 0008) both price a walk high enough to
/// land a creep a tick or two early, so the gate is what keeps the
/// engine's ERR_NOT_ENOUGH_RESOURCES spam structurally impossible. The
/// garrison gets no exemption here: it stays kept on its Post through the
/// window, silent, and digs the tick the energy lands.
let private actionIntents (snapshot: Snapshot) atlas (creep: CreepInfo) (task: Task) : Intent list =
    let drained =
        match task with
        | Harvest sourceId -> ticksToRestock snapshot sourceId > 0
        | Withdraw _
        | Refill _
        | Build _
        | Repair _
        | Upgrade _ -> false

    if Atlas.mayAct atlas creep.Name task && not drained then
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
            | Some task -> actionIntents snapshot atlas creep task
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
        let area = Atlas.workAreaFor atlas creep task

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
///
/// Rerouted alone is evidence that must be manufactured — a second,
/// traffic-blind flood per traveller, unmemoisable because each creep's
/// tile is its own key — so it is computed only for creeps on the verbose
/// list (ADR 0018). Grounded and yielded fall out of work the arbitration
/// already did and stay always-on.
let resolve
    (snapshot: Snapshot)
    atlas
    (assigned: Map<string, Task>)
    (verbose: Set<string>)
    : Intent list * Verdict list =
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
                    if Set.contains name verbose then
                        match Map.tryFind name assigned with
                        | Some task when rerouted name task -> [ Verdict.Rerouted name ]
                        | _ -> []
                    else
                        []

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
    let postCaps = postCapacities snapshot atlas

    // Each living creep's remaining life, hoisted for the tick as the two
    // cap tables are: the capacity gate asks it once per holder per judged
    // pair, and the answer is a Snapshot fact.
    let lives =
        snapshot.Creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    // The crowding component of the matching key (ADR 0002): every holder,
    // counted at this tick. Arrival discounts what a Task's cap counts
    // (ADR 0026), never what the key does — spreading creeps over Tasks is
    // a judgement about now, and nothing in 0026 revises the key.
    let load (acc: Assignments) tid =
        acc |> Map.filter (fun _ assigned -> assigned = tid) |> Map.count

    // The holders a candidate actually competes with, counted at arrival
    // (ADR 0026): two creeps hold the same standing room against each
    // other only while both are standing on it, so a holder counts against
    // a candidate exactly when their two stays overlap. A holder dead
    // before the candidate arrives has left the tile — which is what lets
    // a successor leave the spawn while its predecessor still digs — and a
    // holder still walking when the candidate dies never reaches it. The
    // window is read from both ends, so the fold's creep-name order cannot
    // decide which of a pair keeps the Task: ADR 0026 promises that for a
    // succession, and a one-ended window would have kept it only there,
    // letting a candidate a whole spawn-and-walk away evict a garrison
    // that is nowhere near its own lead. A walk the Atlas cannot price has
    // no arrival, and counts from now.
    let holdersAt (acc: Assignments) (candidate: CreepInfo) task arrival =
        let tid = taskId task

        let overlaps name =
            let alive =
                match arrival with
                | None -> true
                | Some ticks -> Map.tryFind name lives |> Option.forall (fun life -> life >= ticks)

            let arrived =
                match Atlas.walkTicks atlas name task with
                | None -> true
                | Some ticks -> ticks <= candidate.TicksToLive

            alive && arrived

        acc
        |> Map.toList
        |> List.choose (fun (name, assigned) ->
            if assigned = tid && overlaps name then Some name else None)

    // A heavy body is judged against both caps (ADR 0024): the Seat count
    // it shares with every other harvester, and the Post count only its own
    // kind competes for. Only Harvest is capped at all, so the holders are
    // gathered inside the capped arms — the Refills, Withdraws and surplus
    // work the pool is mostly made of never walk the assignment map.
    let hasCapacity (creep: CreepInfo) acc task (arrival: Lazy<int option>) =
        let tid = taskId task
        let seatCap = Map.tryFind tid capacities

        let postCap =
            if Atlas.workHeavy atlas creep.Name then
                Map.tryFind tid postCaps
            else
                None

        match seatCap, postCap with
        // Only a capped Task forces the walk: the Refills, Withdraws and
        // surplus work the pool is mostly made of neither walk the
        // assignment map nor pay for an arrival (ADR 0029).
        | None, None -> true
        | _ ->
            let holders = holdersAt acc creep task arrival.Value

            let withinSeats =
                match seatCap with
                | Some cap -> List.length holders < cap
                | None -> true

            let withinPosts =
                match postCap with
                | Some cap -> (holders |> List.filter (Atlas.workHeavy atlas) |> List.length) < cap
                | None -> true

            withinSeats && withinPosts

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed (e.g. across a redeploy). So does
    // reachability: a Work Area the Atlas can no longer reach releases the
    // assignment, freeing its capacity for creeps that can get there —
    // deliberately with no range-based fallback (ADR 0002). So does the
    // arrival gate: a drained source's Harvest whose wait the holder's
    // walk no longer covers releases it (ADR 0025) — the same behaviour
    // ADR 0013 got from the Task vanishing, now under its own reason. Each
    // failed gate names the release; a dead creep's assignment drops
    // silently — Verdicts attribute to living creeps only.
    // One exemption, ADR 0026's own: an expiring creep is never released
    // over capacity. Its successor is cast and matched while it still
    // works, and where the two do overlap on the tile — a lead longer than
    // the successor's walk — the exemption is what keeps the fold's
    // creep-name order from deciding which of them holds the Post.
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
                | Some task ->
                    // The walk is bound one gate before it is spent, exactly
                    // as the fresh cascade below binds it: capacity counts
                    // holders at this holder's own arrival (ADR 0026), and
                    // the arrival gate spends the very same number — priced
                    // at most once, and only if one of the two asks.
                    // Reachability is still travel cost's answer — the two
                    // floods reach the same tiles, and ADR 0002's release
                    // has no other price.
                    let cost = Atlas.travelCost atlas creep.Name task
                    let arrival = lazy (Atlas.walkTicks atlas creep.Name task)

                    if
                        not (hasCapacity creep acc task arrival)
                        && not (expiring snapshot atlas creep)
                    then
                        release ReleaseReason.OverCapacity
                    else
                        match cost with
                        | None -> release ReleaseReason.Unreachable
                        | Some _ when tooEarly snapshot atlas creep task arrival ->
                            release ReleaseReason.TooEarly
                        | Some _ -> Map.add name tid acc, released)

    // One gate cascade judges every (creep, Task) pair — rejected at the
    // first matching gate it fails (applicable, capacity, reachable, in
    // time) or scored on the full key when none does. Two numbers are bound
    // above the capacity gate: the travel cost, which the reachability gate
    // and the scored key both read, and the walk, which capacity counts
    // holders at (ADR 0026) and the arrival gate spends after it — one
    // number for both, priced at most once. The walk is bound rather than
    // priced because most of the pool asks for neither gate, and pricing
    // one per pair regardless measured at a third of the tick (ADR 0029
    // split the pair; the cascade did not move). The order of the gates is
    // unchanged by that: a candidate the Atlas cannot price has no arrival
    // to count holders at, every holder counts against it, and it reports
    // the rejection it always did. The Matcher's candidates and a verbose
    // Scoring both read from here, so the narration can never drift from
    // what actually decided the match.
    let judge acc (creep: CreepInfo) task =
        let tid = taskId task

        if not (applicable atlas creep task) then
            Candidate.Rejected(tid, RejectReason.Inapplicable)
        else
            let cost = Atlas.travelCost atlas creep.Name task
            let arrival = lazy (Atlas.walkTicks atlas creep.Name task)

            if not (hasCapacity creep acc task arrival) then
                Candidate.Rejected(tid, RejectReason.CapacityFull)
            else
                match cost with
                | None -> Candidate.Rejected(tid, RejectReason.Unreachable)
                | Some _ when tooEarly snapshot atlas creep task arrival ->
                    Candidate.Rejected(tid, RejectReason.TooEarly)
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
                // capacity, reachable, in time — is why the creep sits
                // idle, deepest gate first: a creep whose only rejection is
                // the arrival gate is waiting out a restock (ADR 0025), and
                // saying nothing fit its body would be the same lie the
                // rejection reason refuses.
                let rejectedWith wanted =
                    judged
                    |> List.exists (function
                        | _, Candidate.Rejected(_, reason) -> reason = wanted
                        | _ -> false)

                let reason =
                    if List.isEmpty tasks then
                        IdleReason.NoTasks
                    elif rejectedWith RejectReason.TooEarly then
                        IdleReason.NoneInTime
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

/// The census signature (ADR 0017): a string over exactly the inputs the
/// census-derived plans read — the (kind, position) census of standing
/// structures, the (kind, position) census of pending sites, the
/// controller level, and the room name. Any one input moving moves the
/// signature; everything else a Snapshot carries — creeps, stores, hits,
/// dropped piles, hostiles, banked energy, the tick — is invisible to it.
/// The hauler quota rides the same signature on one load-bearing
/// derivation: the RoomEnergy Capacity it sizes bodies from is the
/// engine's energyCapacityAvailable — a function of the standing
/// spawn/extension census and the controller level, both covered here. A
/// colony spanning a second spawn room would outgrow the single RoomName
/// and must widen the signature before anything census-derived differs
/// between its rooms.
let censusSignature (snapshot: Snapshot) : string =
    let census select =
        snapshot.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            select kind
            |> Option.bind (fun (built: BuiltKind) ->
                Map.tryFind id snapshot.Spatial.TargetPositions
                |> Option.map (fun pos -> $"{built}@{pos.X},{pos.Y}")))
        |> List.sort
        |> String.concat ";"

    let standing =
        census (function
            | Structure kind -> Some kind
            | _ -> None)

    let pending =
        census (function
            | Site kind -> Some kind
            | _ -> None)

    let level =
        snapshot.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    let room = snapshot.Spatial.RoomName |> Option.defaultValue ""
    $"{room}|{level}|{standing}|{pending}"

/// The decision seam: Snapshot in — with the verbose list of creep names
/// owed the manufactured-evidence Verdicts (full candidate scoring, reroute
/// attribution) and the previous tick's plan memo — Decision out. The tick's pipeline is visible here — plan, match, emit, resolve —
/// beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same
/// flood (ADR 0004). The census-derived plans — the Layout's site Intents
/// and the hauler quota — are reused verbatim from a memo whose signature
/// matches this tick's census, and recomputed otherwise (ADR 0017).
let decide
    (snapshot: Snapshot)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    let atlas = Atlas.ofSnapshot snapshot
    let signature = censusSignature snapshot

    let plan =
        match memo with
        | Some m when m.Signature = signature -> m
        | _ ->
            {
                Signature = signature
                SiteIntents = planLayout snapshot atlas
                HaulerQuota = haulerQuota snapshot atlas
            }

    let defenseIntents = planSafeMode snapshot atlas @ planFire snapshot atlas
    let spawnIntents = planSpawns snapshot atlas plan.HaulerQuota
    let pickupIntents = planPickups snapshot atlas
    let tasks = planTasks snapshot
    let next, verdicts = matchCreeps snapshot atlas tasks assignments verbose
    let assigned = assignedTasks tasks next
    let moveIntents, moveVerdicts = resolve snapshot atlas assigned verbose

    {
        Intents =
            defenseIntents
            @ spawnIntents
            @ plan.SiteIntents
            @ pickupIntents
            @ emit snapshot atlas assigned
            @ moveIntents
        Assignments = next
        Memo = plan
        Verdicts = verdicts @ moveVerdicts
    }
