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
    // One Flee for the whole colony: it has no target to be identified by,
    // and every creep inside a Reach is running from the same thing.
    | Flee -> "flee"

/// The Repair trigger of the decaying kinds: a road or a container enters
/// the pool when its hits sink strictly below this fraction of max, and
/// leaves it once repaired back over the line. A tunable, not part of
/// ADR 0010.
let private repairTrigger = 0.5

/// The rampart floor (ADR 0034): a rampart is hungry below this many hits
/// and whole at it. A tunable, and a derived one — the ticks the room must
/// hold times the damage per tick it must hold against. Against the squad
/// of #66, 180 hits a tick, 100,000 hits is 555 ticks, two and a half
/// times the raid that was seen. That costs 1,000 energy to raise and, at
/// 300 hits of decay per 100 ticks, one Repair visit per rampart every 200
/// ticks to hold — so no hysteresis is needed: one visit at 600 hits a
/// tick puts a rampart that just dipped back over the line.
let private rampartFloor = 100_000

/// Whether a structure of this kind, carrying these hits, is hungry: its
/// own whole line, read off the kind (ADR 0034). The decaying kinds sit
/// below a fraction of max (ADR 0010), a rampart below the floor, the Keep
/// below full — it does not decay, so below max means damaged. A kind with
/// no line is never hungry. The floor is capped at the structure's own max
/// so a rampart whose max is somehow under it can still be whole; today's
/// engine puts a rampart's max at 300,000 from RCL2, well clear.
let private isHungry kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction -> float hits.Hits < repairTrigger * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min rampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// Every structure the projection carries hits for that stands below its
/// kind's whole line, with its kind, in id order. The one walk over the
/// hits and the kinds that judges them, and the two readers that ask it
/// share it: the Repair pool takes all of them, the safe-mode reflex's
/// Keep arm asks only whether one of them is of the Keep (ADR 0034). The
/// projection carries hits on repairable kinds only, but the kind gate is
/// judged here — the decision layer owns what it reads, off the same table
/// the projection filtered by.
let private hungryStructures (snapshot: Snapshot) : (string * BuiltKind) list =
    snapshot.Spatial.Hits
    |> Map.toList
    |> List.choose (fun (id, hits) ->
        match Map.tryFind id snapshot.Spatial.TargetKinds with
        | Some(Structure kind) when isHungry kind hits -> Some(id, kind)
        | _ -> None)

/// Screeps CONTAINER_CAPACITY: what a container's store can hold — the
/// line past which the buffer needs no Refill.
let private containerCapacity = 2000

/// Screeps STORAGE_CAPACITY: what the Storage's store can hold — the line
/// past which the stock needs no Refill. Read against stored *energy*, as
/// the container line is, because energy is the only resource this colony
/// ever holds; the day it holds another, the Storage's free capacity has
/// to be projected rather than inferred from one resource.
let private storageCapacity = 1000000

/// The Reach margin (ADR 0033): the tiles a Threat's weapon range is
/// widened by — one for the hostile's next step, one for our own tick of
/// lag. A tunable beside `repairTrigger`, not a term of the decision.
let private reachMargin = 2

/// Screeps weapon ranges: ATTACK strikes at range 1, RANGED_ATTACK at 3.
let private meleeRange = 1
let private rangedRange = 3

/// The range a hostile can hurt a creep from, or None for one that cannot
/// (ADR 0033). A Threat is read off the parts and never off the owner — an
/// NPC invader and a player's raider do the same damage per part — and
/// nothing but ATTACK and RANGED_ATTACK hurts a creep at all: a healer, a
/// scout or a claimer is a hostile the fire reflex shoots and the Raid log
/// records, and it gates no Task. A body carrying both weapons reaches the
/// farther of them.
let private weaponRange (hostile: HostileInfo) : int option =
    [
        if List.contains Attack hostile.Body then
            meleeRange
        if List.contains RangedAttack hostile.Body then
            rangedRange
    ]
    |> function
        | [] -> None
        | ranges -> Some(List.max ranges)

/// The tick's colony-level threat facts (ADR 0033): the tiles a Threat can
/// hurt, and the walkable tiles no Threat can. Derived once a tick and
/// shared by the three readers — the applicability gate that takes the
/// Reach out of every Work Area, Flee's own Work Area, and the spawn hold.
/// Colony facts, never a change to the spatial projection: hostiles still
/// block no tiles and price no paths, and nothing in the Atlas reads one.
///
/// Layered by room name, as the projection they are derived from is (ADR
/// 0041, #138): a Reach is a set of one room's tiles, and a `Set<Pos>`
/// cannot say which room's, so the room rides on the outer key — the
/// room the hostile stands in, which `HostileInfo` carries for exactly
/// this join. Without the layer a hostile in one room dug the same hole
/// on the same coordinate of every projected room. Each reader picks its
/// own room's share through `Threats.reachIn` and `Threats.safeIn`, and
/// a room with no entry answers the empty set, which is ADR 0004's
/// absence: it blocks no action and pools no Flee. So the single-room
/// colony's answers are unchanged, and an outpost this tick projects no
/// hostile in — `Snapshot.Hostiles` still sweeps the spawn rooms alone —
/// is quiet by absence rather than by a second rule.
type Threats =
    {
        /// Per room, the tiles a Threat standing in it can hurt. Never an
        /// empty set under a room: a room whose whole Reach our ramparts
        /// took back has no entry, so `Map.isEmpty` is "no Reach
        /// anywhere" — the one question the pool asks of it.
        Reach: Map<string, Set<Pos>>
        /// Per room, the walkable tiles no Threat reaches — Flee's Work
        /// Area for a creep standing there. Derived only for the rooms
        /// with a Reach, where every other room's absence stands for "not
        /// derived" rather than "nowhere is safe": nothing reads it there,
        /// because a creep with no Reach around it is matched to no Flee.
        Safe: Map<string, Set<Pos>>
    }

/// The tick with nothing to run from: every Work Area stands whole and no
/// creep flees. What the pipeline is handed for a quiet colony.
let noThreats = { Reach = Map.empty; Safe = Map.empty }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// One room's Reach; empty for a room no Threat stands in (ADR 0004).
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// One room's safe set; empty for a room no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Safe |> Option.defaultValue Set.empty

/// This tick's Threats, off the Snapshot's hostiles and the rampart
/// census, room by room. Each Threat reaches its weapon range plus the
/// margin, in the Chebyshev tiles every range in the colony is measured
/// in — less every tile under one of our standing ramparts in that same
/// room, which is in no Reach at all: a creep on its own rampart cannot be
/// attacked, and that exemption is what lets an Anchor keep digging on a
/// ramparted Post (ADR 0034). A room's safe set is that room's walkable
/// ground less that room's Reach, derived only where something is unsafe,
/// so a quiet tick pays for no walk over any room, and a raid in one room
/// walks that room alone. Derived once here and handed down; the layering
/// does not make it once per creep.
let threatsOf (snapshot: Snapshot) atlas : Threats =
    // The hostiles are asked first, so a quiet colony walks nothing:
    // neither the rampart census nor any room's own tiles are read on a
    // tick with nothing in it to run from.
    match
        snapshot.Hostiles
        |> List.choose (fun hostile ->
            weaponRange hostile |> Option.map (fun r -> hostile.RoomName, hostile.Pos, r))
    with
    | [] -> noThreats
    | threats ->
        let reach =
            threats
            |> List.groupBy (fun (room, _, _) -> room)
            |> List.choose (fun (room, inRoom) ->
                let ramparts = Atlas.ourRampartTilesIn atlas room

                let tiles =
                    inRoom
                    |> List.collect (fun (_, pos, weapon) ->
                        let r = weapon + reachMargin

                        [
                            for x in pos.X - r .. pos.X + r do
                                for y in pos.Y - r .. pos.Y + r do
                                    let tile = { X = x; Y = y }

                                    if not (Set.contains tile ramparts) then
                                        tile
                        ])
                    |> Set.ofList

                // Nothing left to run from once our own ramparts have taken
                // the whole Reach back: no Reach, no entry, no safe set to
                // derive.
                if Set.isEmpty tiles then None else Some(room, tiles))
            |> Map.ofList

        {
            Reach = reach
            Safe =
                reach
                |> Map.map (fun room tiles ->
                    Set.difference (Atlas.walkableTilesIn atlas room) tiles)
        }

/// The source container geometry (ADR 0012): a tile within range 1 of the
/// given source is that source's container tile — the Seat-standing kind
/// the Layout places, which harvest overflow fills. The one range this
/// colony calls a source container, asked of one source: the Layout asks
/// it per target to know whether that source is served (ADR 0040), and
/// the two rules below ask it of every source of one room at once.
let private servesSource (sourcePos: Pos) (tile: Pos) = range tile sourcePos <= 1

/// Whether a tile of the named room is a source container's: within range
/// 1 of a placed source **standing in that same room**. The one geometry
/// judgement behind both rules that care about the kind and not the source
/// — the Planner keeps source containers out of Refill, the hauler quota
/// counts them. Unplaced geometry classifies nothing (ADR 0004).
///
/// The room is matched before the range, and it has to be (ADR 0041): a
/// `Pos` carries no room, so a fold over every source compares a home
/// container's coordinates against an outpost source's and answers yes on
/// a collision that is fifty tiles and a room boundary away. That one
/// wrong answer costs twice over — the container enters the hauler quota
/// as a source's, and drops out of the Refill pool as one — so the two
/// rules below hand in the room the tile came out of rather than the tile
/// alone.
let private isSourceContainerTile (snapshot: Snapshot) (room: string) (pos: Pos) =
    snapshot.Sources
    |> List.choose (fun s -> SpatialInfo.placementOf snapshot.Spatial s.Id)
    |> List.exists (fun (sourceRoom, sourcePos) -> sourceRoom = room && servesSource sourcePos pos)

/// Planner: rebuild this tick's full Task pool from the Snapshot. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (snapshot: Snapshot) (threats: Threats) : Task list =
    // Flee exists while a Reach does (ADR 0033): one Task for the whole
    // colony, at the head of the pool as its Safety tier is at the head of
    // the ranking. No Reach, no Flee — a quiet tick's pool is the pool it
    // always was.
    let flees = if Map.isEmpty threats.Reach then [] else [ Flee ]

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

    // A Repair per repairable structure below its kind's whole line, in id
    // order (ADR 0010, ADR 0034).
    let repairs = hungryStructures snapshot |> List.map (fst >> Repair)

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
    //
    // The controller fixes the room, and the containers are then read out
    // of that room's layer rather than out of the kind census's whole id
    // space (ADR 0041). Range 3 is a distance inside one room: a container
    // in another room is not the buffer this controller's upgraders draw
    // from however near its coordinates fall, and pooling it as one would
    // send a hauler to fill a store nobody upgrades out of. A container
    // the controller's room does not place drops out, exactly as an
    // unplaced one always has (ADR 0004).
    let containerRefills =
        snapshot.Controller
        |> Option.bind (fun c -> SpatialInfo.placementOf snapshot.Spatial c.Id)
        |> Option.map (fun (controllerRoom, controllerPos) ->
            let placed = (SpatialInfo.layerOf snapshot.Spatial controllerRoom).TargetPositions

            containers
            |> List.filter (fun id ->
                match Map.tryFind id placed with
                | Some pos ->
                    range pos controllerPos <= 3
                    && not (isSourceContainerTile snapshot controllerRoom pos)
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

    flees
    @ harvests
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

/// Ceiling division over the quota rows' arithmetic: a quota that came
/// out a fraction of a body hires the whole body (ADR 0012 for the hauler
/// row, ADR 0037 for the worker row), because the fraction a floor drops
/// is demand nobody is hired for. A numerator at or below zero lands at
/// or below zero — F# divides toward zero — and each row's own floor
/// answers for it.
let private ceilDiv numerator divisor = (numerator + divisor - 1) / divisor

/// The hauler row's quota rule (ADR 0012) — the row's colony fact, per
/// ADR 0006's law that a row arrives with its quota or not at all: per
/// source container, ceil(round-trip travel ticks to the spawn × source
/// output ÷ the cast body's carry capacity), so a farther container hires
/// proportionally more haul capacity and never quietly overflows. The
/// spawn is the canonical sink because the trunks radiate from it; of
/// several spawns the cheapest wins. No source containers, no placed
/// spawns, or unreachable geometry hire nothing.
///
/// The colony's own room and no other (ADR 0041). The kind census spans
/// every room the projection carries, so the containers are read out of
/// the home layer to pick the room, and the source judgement is then made
/// inside it. Both halves have to be home: the round trip is priced by
/// `Atlas.haulRoundTripTicks`, which floods the home room's grid, so an
/// outpost container handed to it would be walked over home terrain and
/// hire a fleet for a haul nobody makes. The honest cross-room price is a
/// minimum over the Seam band (#123), and the outpost's own quota is the
/// economics ADR 0042 owns; until both land, a container the home room
/// does not place hires nobody, which is ADR 0004's answer for geometry
/// this query cannot price.
let private haulerQuota (snapshot: Snapshot) atlas : int =
    let home = SpatialInfo.homeName snapshot.Spatial
    let placed = (SpatialInfo.layerOf snapshot.Spatial home).TargetPositions

    let sourceContainerTiles =
        snapshot.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            if kind = Structure BuiltKind.Container then
                Map.tryFind id placed
            else
                None)
        |> List.filter (isSourceContainerTile snapshot home)

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
            |> Option.map (fun ticks -> ceilDiv (ticks * sourceOutputPerTick) capacity))
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
/// hiring for jobs that no longer exist. An unposted source of the spawn
/// room still contributes its Seat count — its output is spoken for by the
/// seat crews that walk it — so only the posted sources' output is income.
///
/// An unposted source of an outpost contributes nothing at all (ADR 0042).
/// The seat-crew justification presumes the walk is cheap, and across a
/// border it is not: the three declared outpost sources carry six Seats
/// between them, five of them swamp, and counted here they would hire six
/// generalists to commute forty-seven to fifty-six tiles to dig them. The
/// useful half of that exclusion is that a standing container is the
/// switch admitting an outpost into the economy — until one stands the
/// room is invisible to every quota, and the tick it stands the source
/// becomes a Post and enters the income base at its own output. Which room
/// a source stands in is the Atlas's own id-to-room join — the layer that
/// places its id, precomputed for every reader holding an Atlas (ADR
/// 0041) — never the constant: the projection is what the quota is derived
/// from, and a source the projection does not place is unpriceable and
/// counts nothing wherever it was declared (ADR 0004).
///
/// Being posted is judged in the source's own room, by `Atlas.postsOf` and
/// not by testing its Seats against the home room's Posts: a `Pos` carries
/// no room, so a home Post standing on an outpost Seat's coordinates would
/// read that outpost source as posted with no container under it, and put
/// a phantom ten energy a tick into the income base below.
///
/// From that income the anchor and hauler rows' replacement amortization
/// (body cost spread over a creep's lifetime) is deducted; every energy
/// per tick left feeds upgrade mouths at one worker body's Work drain,
/// rounded up so the mouths cover the surplus rather than fall a body
/// short of it (ADR 0037), bodies priced as the richest bank would cast
/// them. The arithmetic runs scaled by the lifetime so the amortization
/// never rounds away.
let private workforceTarget (snapshot: Snapshot) atlas anchorQuota haulerQuota =
    let home = SpatialInfo.homeName snapshot.Spatial

    let posted, unposted =
        snapshot.Sources
        |> List.partition (fun s -> Atlas.postsOf atlas s.Id |> Set.isEmpty |> not)

    let unpostedSeats =
        unposted
        |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some home)
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

    // Rounded up through the same ceilDiv as the hauler row (ADR 0037):
    // the granularity a floor would drop is a whole worker body's Work,
    // which grows with RCL, and the income it drops leaks every tick
    // while the body it oversells is paid for out of stock. An
    // amortization above income leaves the surplus negative, and max 0 is
    // the row's floor for it.
    let incomeWorkers =
        let surplusOverLifetime =
            List.length posted * sourceOutputPerTick * creepLifetime - amortization

        ceilDiv surplusOverLifetime (workerDrain * creepLifetime) |> max 0

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
///
/// Priced in the colony's own room (ADR 0041): the tile comes from
/// `Atlas.placedCreeps`, which answers home, and the walk from
/// `Atlas.castWalkTicks`, which floods the home grid and rides a memo ADR
/// 0032 keys on the census signature — a key with no room on it, because
/// no flood leaves the room it started in. So a creep the home room does
/// not place answers 0 and is never expiring: its successor is not cast
/// early, which is the safe direction of the two while a spawn cannot
/// walk to it at all. The honest cross-room lead is the minimum over the
/// Seam band (#123).
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
let private planSpawns
    (snapshot: Snapshot)
    atlas
    (threats: Threats)
    (haulerQuota: int)
    : Intent list =
    // The spawn holds while its doorstep is hot (ADR 0033): a creep born
    // into a Reach is a kill delivered, so no spawn casts anything while
    // any tile beside any spawn lies in one — the disaster fallback below
    // included, since an empty colony's first creep is the one that can
    // least afford to be born under fire. The doorstep is read against the
    // Reach of the spawn's own room (#138): a Threat on the neighbouring
    // coordinate of another room is a room away from the birth tile.
    let doorstepInReach (s: SpawnInfo) =
        match Atlas.targetRoom atlas s.Id, Atlas.positionOf atlas s.Id with
        | Some room, Some pos ->
            let reach = Threats.reachIn threats room

            [
                for x in pos.X - 1 .. pos.X + 1 do
                    for y in pos.Y - 1 .. pos.Y + 1 -> { X = x; Y = y }
            ]
            |> List.exists (fun tile -> Set.contains tile reach)
        | _ -> false

    // Asked before anything is priced, the way the reflexes ask their
    // hostiles first: a held tick derives no Workforce target and floods
    // no lead.
    if snapshot.Spawns |> List.exists doorstepInReach then
        []
    else

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

/// Colony reflex beside the pipeline, two arms and one pair of gates —
/// stock remaining, safe mode not already running.
///
/// The CLAIM arm: a CLAIM-part hostile is the one threat that can disarm
/// safe mode itself — attackController blocks activation for 1,000 ticks.
/// But the tap is a range-1 act, so the activation holds until a claimer
/// stands within reach of landing it (ADR 0015) — the hold is free
/// (activation still wins the race) and buys the towers their window to
/// kill the claimer en route. A controller the projection cannot place has
/// no deadline to measure and falls back to firing on sight.
///
/// The Keep arm (ADR 0034): any Keep structure below full hits while any
/// hostile stands in the spawn room. The same shape — hold until the harm
/// is certain — over the other half of the exposure. Any hostile, not only
/// a Threat: a WORK-only dismantler hurts a structure without ever
/// qualifying as one, and "the Keep is losing hits with someone here" is
/// the honest reading whoever is doing it. Stateless on purpose: one
/// tick's hits, never a comparison against the last tick's (ADR 0012,
/// 0017), which is what makes Repair's full-hits line on the Keep part of
/// this reflex — a Keep left dented would keep the arm armed for every
/// hostile that wandered through afterwards.
///
/// A hostile that neither claims nor damages spends nothing: at RCL2 safe
/// mode outlasts any invader raid 13×, so the stock keeps for when the
/// room is actually being taken (ADR 0007).
let private planSafeMode (snapshot: Snapshot) atlas : Intent list =
    match snapshot.Controller with
    | Some controller when controller.SafeModeAvailable > 0 && not controller.SafeModeActive ->
        let withinReach (h: HostileInfo) =
            List.contains Claim h.Body
            && match Atlas.positionOf atlas controller.Id with
               | Some pos -> range h.Pos pos <= safeModeDeadline
               | None -> true

        let claimerInReach = snapshot.Hostiles |> List.exists withinReach

        // Below full hits, off the walk the Repair pool reads: the Keep's
        // whole line is Full, so "hungry" and "damaged" are one fact and
        // the two readers cannot drift apart. The Posts and the ramparts
        // are hungry on their own lines and are not of the Keep — neither
        // a container's hits nor a rampart's ever spend the stock. The
        // hostiles are asked first, so a quiet room walks nothing.
        let keepDamaged =
            not (List.isEmpty snapshot.Hostiles)
            && hungryStructures snapshot |> List.exists (snd >> isKeep)

        if claimerInReach || keepDamaged then
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

/// The level the engine unlocks ramparts at (Screeps CONTROLLER_STRUCTURES
/// for "rampart": none at RCL1, 2,500 from RCL2 up). The covering rule's
/// one gate, and a level rather than an allowance because the count is
/// never what constrains it — the Keep and the Posts are a handful of
/// tiles against thousands (ADR 0034).
let private rampartLevel = 2

/// The Layout horizon (ADR 0011, moved to RCL5 by ADR 0039): the whole
/// plan is computed up to this level regardless of the current one, so
/// today's roads route around tomorrow's structures. One level of
/// lookahead is the standing bargain — RCL8 would tax today's trunks
/// with detours for structures four levels away, and a horizon the room
/// has already passed sizes every clustered gap at zero, so the room
/// stops growing without saying why. Declared and not computed from the
/// current level, which is what keeps it stepping once, in a commit
/// (ADR 0039).
let private horizonLevel = 5

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
/// on one — Link is a built kind only, and this room fills none of them
/// (ADR 0038).
/// Placement filters the Layout to what the current level unlocks and
/// what the projection's censuses say is missing. Sites are not creep
/// work, so this emits Intents directly rather than Tasks.
/// Beside all of that runs one rule that reads no tile of the ordering: a
/// rampart covers every standing Keep structure and every standing Post
/// container (ADR 0034), from the level the engine allows one, because a
/// rampart is no footprint and takes no tile from anything.
/// Returned beside the Intents: the footings, served and unserved. A
/// target with no candidate reserves nothing, and the guarantee of ADR
/// 0022 and ADR 0027 — one footing per target — would otherwise degrade
/// with nothing anywhere to say so (#77). Not an Intent, because there is
/// nothing to place; not a Verdict, because a footing has no creep (ADR
/// 0035). The tiles it did reserve ride out for the same reason read the
/// other way (#106): a link is placed by no Intent, so a footing the fold
/// dropped from the accumulator would be a reservation no reader could
/// see — and the rule it was chosen by, off the trunks and off every
/// target and every other footing, could be asserted nowhere (ADR 0036).
/// Beside them, the container picks the plan deferred to a container that
/// already serves their target (ADR 0040): the room keeps the container it
/// has, and what the plan wanted instead is a colony fact no other channel
/// carries — nothing demolishes the orphan, so the difference is permanent.
let private planLayout
    (snapshot: Snapshot)
    atlas
    : Intent list *
      ServedFooting list *
      UnservedFooting list *
      UnroutedTrunk list *
      DeferredContainer list
    =
    let anchor = snapshot.Spawns |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id)

    match Atlas.homeRoom atlas, anchor, snapshot.Controller with
    | Some room, Some spawnPos, Some controller ->
        // Same checkerboard colour as the spawn: clustered structures sit on
        // the spawn's colour, leaving the other colour free for movement.
        let parity = (spawnPos.X + spawnPos.Y) % 2

        // The sources this plan is for: the home room's alone (ADR 0041).
        // `snapshot.Sources` is every scanned room's since #124 — the
        // Harvest pool is one pool — while every use of a source below
        // joins a bare `Pos` to the *home* grid: a footing slot widens a
        // home reservation, a trunk is a home flood started from the
        // source's coordinate, and a container pick lands on a home tile.
        // An outpost source read here draws a trunk and plants a container
        // site out of another room's coordinates onto home ground, which
        // is the Layout ADR 0042 promises the outpost will never get ("The
        // outpost gets a container and nothing else. No roads, and no
        // Layout"). The room is resolved through the Atlas's own id join
        // (ADR 0041), as every other reader holding one resolves it.
        let homeSources =
            snapshot.Sources
            |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some room)

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
        let footingSlots = List.length homeSources + 2

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

        // Each goal beside the name it is recorded under when a source
        // cannot reach it (#107). The Upgrade Work Area first and the
        // spawns after, which is the order the routes are collected in and
        // therefore the order a loss reads in.
        let trunkGoals =
            (TrunkGoal.UpgradeArea, upgradeArea)
            :: (snapshot.Spawns
                |> List.choose (fun s ->
                    Atlas.positionOf atlas s.Id
                    |> Option.map (fun spawn ->
                        TrunkGoal.Spawn s.Id, Atlas.adjacentWalkable atlas spawn |> Set.ofList)))

        // Every route the Layout asks for, kept per source and per goal:
        // the union paves the roads and each source's own trunk anchors
        // its container (ADR 0012), while the goals stay apart for the
        // reason `TrunkGoal` is a type — the loss below is per goal.
        let sourceRoutes =
            homeSources
            |> List.sortBy (fun s -> s.Id)
            |> List.choose (fun s ->
                Atlas.positionOf atlas s.Id
                |> Option.map (fun sourcePos ->
                    s.Id,
                    trunkGoals
                    |> List.map (fun (goal, area) ->
                        goal, Atlas.trunkPath atlas reserved sourcePos area)))

        let sourceTrunks =
            sourceRoutes
            |> List.map (fun (id, routes) -> id, routes |> List.collect snd |> Set.ofList)

        // The empty path is the router's answer for a goal it paved nothing
        // for, and it unions into the road plan contributing nothing
        // (#107). Recorded here, where the source and the goal are both
        // still in scope: downstream there is only a set of tiles, and a
        // trunk that was dropped whole looks exactly like one that was
        // never asked for. The predicate is the paving and not the flood
        // on purpose — what the colony lost is the line, however the
        // geometry failed to draw it.
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
        //
        // The pick rides beside the source it is for: the target clause
        // below asks whether *that* source is served, and downstream there
        // is only a tile (ADR 0040).
        let sourceContainerPicks =
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
                    |> fun seat -> Some(sourceId, seat))

        let sourceContainerTiles = sourceContainerPicks |> List.map snd

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
        // Each target carries the kind it is, which the fold knows by
        // construction — the list is assembled from exactly the three the
        // guarantee names — so an unserved one below says which guarantee
        // was lost rather than only which tile (#77). A tile named twice
        // keeps the first kind that named it, the way `List.distinct`
        // always kept the first tile.
        let footingTargets =
            [
                for tile in sourceContainerTiles -> tile, FootingKind.SourceContainer
                for tile in Option.toList controllerContainerTile ->
                    tile, FootingKind.ControllerContainer
                for tile in storagePick -> tile, FootingKind.Storage
                for tile in
                    Set.union (Atlas.storageTiles atlas) (Atlas.pendingStorageTiles atlas)
                    |> Set.toList -> tile, FootingKind.Storage
            ]
            |> List.distinctBy fst

        let footingTargetTiles = footingTargets |> List.map fst

        // A standing link is a target, so its own footing has stopped
        // being buildable: added back, or the footing would jump the tick
        // the link went up. The working ground is deliberately not
        // subtracted — a footing is the one structure footing allowed
        // there (ADR 0022), because a link on a Seat or an Upgrade tile is
        // exactly what buys the Anchor and the upgraders a transfer
        // without leaving their tile.
        let footingCandidates = Set.union (Set.ofList buildable) (Atlas.linkTiles atlas)

        // A target with no candidate at all leaves the tiles alone and is
        // recorded (#77): the fold reserves what it can, exactly as
        // before, and the shortfall rides out beside the plan instead of
        // falling through in silence. What it does reserve rides out too
        // (#106), each tile beside the target and the kind it was
        // reserved for — both are in scope here, and nowhere else is,
        // since no Intent ever names a link. Accumulated head-first and
        // reversed once, so both records read in the targets' own order.
        // The bare set of reserved tiles rides the accumulator beside the
        // served entries because the cluster ordering below needs exactly
        // that, and the filter here reads it per candidate tile.
        let footingTiles, servedFootings, unservedFootings =
            ((Set.empty, [], []), footingTargets)
            ||> List.fold (fun (taken, served, unserved) (target, kind) ->
                footingCandidates
                |> Set.filter (fun tile ->
                    range tile target = 1
                    && not (Set.contains tile roadPlan)
                    && not (List.contains tile footingTargetTiles)
                    && not (Set.contains tile taken))
                |> Set.toList
                |> function
                    | [] -> taken, served, { Target = target; Kind = kind } :: unserved
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile spawnPos, tile.X, tile.Y)
                        |> fun tile ->
                            Set.add tile taken,
                            {
                                Target = target
                                Kind = kind
                                Tile = tile
                            }
                            :: served,
                            unserved)

        // The tower and the extensions take the ordering again with the
        // footings held out — a footing outranks both — and the Storage's
        // pick held out with them: it outranks the footings, which are
        // anchored on it. Each footing draws one more tile in behind it,
        // and the reservation was widened by exactly that many, so no pick
        // reaches past the window the trunks dodged.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick) && not (Set.contains tile footingTiles))
            |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clusterPicks |> List.splitAt (min towerSlots clusterPicks.Length)

        // The container census the target clause is judged against: a
        // container standing, or a site already going up. The asymmetry
        // with `Atlas.posts`, which counts standing containers alone, is
        // deliberate (ADR 0040) — the plan asks whether another one must
        // be built, and a site answers that; a Post asks what is catching
        // overflow on a Seat right now, and a site catches nothing.
        let containerCensus =
            Set.union (Atlas.containerTiles atlas) (Atlas.pendingContainerTiles atlas)

        // The target clause (ADR 0040): a source is served when a
        // container stands or is pending within range 1 of it, the
        // controller when one stands or is pending in its Upgrade Work
        // Area — the geometry each rule already reads a container by, not
        // the tile this plan happens to have picked. A served target is
        // planned for no further container, wherever the thing serving it
        // sits; an unserved one is planned onto its pick as before.
        //
        // A pick the clause defers because something else serves its
        // target is a loss the room keeps — nothing demolishes the
        // orphan — so it rides out beside the footings and the trunks.
        // The tile it names is the lowest of a served target's
        // containers, which is one tile in every room we hold; a target
        // with two already had the defect this closes. A container on the
        // pick itself is the coinciding case and no loss at all: the plan
        // wanted exactly what stands there.
        let servingSource sourceId =
            Atlas.positionOf atlas sourceId
            |> Option.map (fun sourcePos -> Set.filter (servesSource sourcePos) containerCensus)
            |> Option.defaultValue Set.empty

        // Every target beside its pick and the containers already serving
        // it. Both answers below are read off this one list, so each
        // target is judged once and the same judgement decides whether it
        // is planned for and whether it lost its pick.
        let targets =
            [
                for sourceId, pick in sourceContainerPicks ->
                    ContainerTarget.Source sourceId, pick, servingSource sourceId
                for pick in Option.toList controllerContainerTile ->
                    ContainerTarget.Controller, pick, Set.intersect containerCensus upgradeArea
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
                            Pick = pick
                            Serving = Set.minElement serving
                        })

        // The tile clause (ADR 0040), and only it: a pick whose tile is
        // still owed a road waits, for the reason it always did — the
        // engine takes one construction site per tile, so the source
        // container (planned onto the trunk's first tile) waits for the
        // road to stand and then coexists with it. This is about the tile
        // and moves with no target.
        let owedRoad = Set.union (Atlas.pendingRoadTiles atlas) roadGap

        let containerGap =
            unservedPicks |> List.filter (fun tile -> not (Set.contains tile owedRoad))

        // The ramparts (ADR 0034): one over every standing Keep structure
        // and every standing Post container, the tick the thing it covers
        // stands — a site is not covered until it is a structure. No
        // allowance to size against: the rule is the whole plan, so the
        // gap is the covering census alone, standing ramparts and pending
        // sites subtracted the way the roads' is. The one gate is the
        // level the engine allows a rampart at, which is the level after
        // the first: below it every site would be refused, every tick. The
        // working-ground exclusion does not apply — a rampart is no
        // footprint, walkable, blocking nothing, taking no tile from the
        // Post it covers (ADR 0022 as ADR 0034 revises it) — which is why
        // these tiles are read off the census and not off the ordering.
        let covered =
            if controller.Level >= rampartLevel then
                Set.union (Atlas.keepTiles atlas) (Atlas.postContainerTiles atlas)
            else
                Set.empty

        let rampartGap =
            Set.difference
                covered
                (Set.union (Atlas.rampartTiles atlas) (Atlas.pendingRampartTiles atlas))

        let place kind tiles =
            tiles |> List.map (fun tile -> PlaceConstructionSite(room, tile, kind))

        place Storage (storagePick |> List.truncate (storageGap controller.Level))
        @ place Tower (towerTiles |> List.truncate (towerGap controller.Level))
        @ place Extension (extensionTiles |> List.truncate (extensionGap controller.Level))
        @ place Road (Set.toList roadGap)
        @ place Container containerGap
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

/// Colony reflex beside the pipeline, the second after safe mode: every
/// creep with free carry capacity standing within pickup range of a
/// dropped energy pile asks to pick it up — beside its assigned Task's
/// action, since the engine's pickup conflicts with no other action. No
/// movement, no matching, no threshold: the reflex only recaptures what
/// is already in reach (death drops, harvest overflow), and duplicate
/// pickups on one pile are the engine's to settle.
///
/// Both sides come out of the colony's own room, and they have to come out
/// of the same one (ADR 0041): the piles and the creeps are bare `Pos`es,
/// so a pile in one room and a creep in another at the same coordinate
/// would read as range 0 and emit a pickup the engine answers
/// ERR_NOT_IN_RANGE. `Atlas.droppedEnergy` and `Atlas.placedCreeps` both
/// answer home, so the pairing never crosses a border; an outpost's
/// overflow is left where it lies until a cross-room reflex has a walk to
/// price (#123).
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

/// The walk and the wait that hold a Task up for this creep, or None when
/// its time has come (ADR 0025, repriced by ADR 0029): a drained source's
/// Harvest is applicable only when the creep's walk covers the restock
/// wait — walk ≥ ticks to restock, with no slack, because the wait shrinks
/// by one each tick while the walk stays put, so a creep one tick short
/// departs one tick later and arrives as the energy does. The walk is the
/// Atlas's own query and nothing here converts anything: it is already
/// whole ticks, floored at one a tile and
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
/// The answer is that pair rather than a yes because the reason the
/// rejection and the release carry is exactly what was compared here
/// (#88): read off the gate, never re-derived by a caller from a price
/// that no longer converts to ticks.
let private tooEarly (snapshot: Snapshot) atlas (creep: CreepInfo) task (walk: Lazy<int option>) =
    match task with
    | Harvest sourceId ->
        match walk.Value with
        // No walk at all is unreachable geometry, which is not earliness:
        // the reachability gate stands ahead of this one in both cascades
        // and names that rejection itself (ADR 0002, ADR 0029).
        | None -> None
        | Some ticks ->
            let wait = ticksToRestock snapshot sourceId

            if ticks < wait && not (garrisons atlas creep sourceId) then
                Some(ticks, wait)
            else
                None
    | Withdraw _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    | Flee -> None

/// The room a Task's Work Area lies in: its target's, since the area is
/// that target's surroundings and empty across a border (ADR 0020, ADR
/// 0041) — so the Reach taken out of it is that room's share (#138). None
/// for Flee, whose area is the creep's own room's, and for a target the
/// projection does not place, whose area is empty and takes nothing.
let private roomOfWork atlas task =
    match task with
    | Harvest id
    | Withdraw id
    | Refill id
    | Build id
    | Repair id
    | Upgrade id -> Atlas.targetRoom atlas id
    | Flee -> None

/// The tiles a creep may work a Task from this tick (ADR 0033): its Work
/// Area less its room's Reach — and for Flee, the safe set of the room the
/// creep stands in, an area of the colony's own rather than some target's
/// surroundings. Each is the share of one room (#138): a hostile a room
/// away on the same coordinate takes no tile here. The Atlas's memoised
/// Work Areas are never modified; this is a filter at the point of
/// judgement, and a tick with no Reach anywhere hands the memo back
/// verbatim.
let private areaFor (threats: Threats) atlas creep task =
    match task with
    | Flee ->
        Atlas.creepRoom atlas creep
        |> Option.map (Threats.safeIn threats)
        |> Option.defaultValue Set.empty
    | _ when Map.isEmpty threats.Reach -> Atlas.workAreaFor atlas creep task
    | _ ->
        let reach =
            roomOfWork atlas task
            |> Option.map (Threats.reachIn threats)
            |> Option.defaultValue Set.empty

        Atlas.workAreaFor atlas creep task
        |> Set.filter (fun tile -> not (Set.contains tile reach))

/// The travel cost of a Task for a creep, priced over the tiles it may
/// actually work from this tick (ADR 0033): the safe set for Flee, which
/// has no target, and every other Task's own Work Area less the Reach —
/// so the reachability gate judges the tiles that are left rather than a
/// tile the creep may not stand on, and a candidate whose cold remainder
/// is walled off is rejected as unreachable instead of being held and
/// never worked. The pricing itself is untouched: same weights, same
/// surcharge, same flood — only the goals are this tick's.
/// An area that is empty here was never taken by the Reach — the threat
/// gate stands ahead of this one in both cascades — so it falls back to
/// the Task's own price, which carries ADR 0004's escape for a target the
/// projection cannot place, and, since #123, the Seam join for a target
/// in another room. The Work Area a creep is handed is empty across a
/// border by construction (ADR 0041): the tiles are the other room's and a
/// `Set<Pos>` cannot say so, while the price is a minimum over the Seam
/// band and knows both rooms. So an outpost's Task reaches the Matcher
/// priced and ranks in the one pool, and it does so through this fallback
/// rather than through a case of its own.
let private travelCostOf (threats: Threats) atlas (creep: string) task =
    match task with
    | Flee -> Atlas.travelCostWithin atlas creep (areaFor threats atlas creep task)
    | _ ->
        match areaFor threats atlas creep task with
        | area when Set.isEmpty area -> Atlas.travelCost atlas creep task
        | area -> Atlas.travelCostWithin atlas creep area

/// Whether the Reach has taken a creep's whole Work Area for a Task (ADR
/// 0033): it had somewhere to stand and has nowhere left. That makes the
/// Task inapplicable to that creep — a Harvest whose only Seat is hot is
/// no Harvest — and releases a holder under a reason of its own, so the
/// transition log tells a raid's release from a Task that vanished. An area
/// that was empty to begin with is not threatened: unplaceable or blocked
/// geometry is the reachability gate's answer (ADR 0002, ADR 0020) and a
/// raid must not be blamed for it. Flee is never threatened either — its
/// area is the safe set, which the Reach has already been taken out of.
let private threatened (threats: Threats) atlas (creep: CreepInfo) task =
    not (Set.isEmpty (Atlas.workAreaFor atlas creep.Name task))
    && Set.isEmpty (areaFor threats atlas creep.Name task)

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
let private applicable (threats: Threats) atlas (creep: CreepInfo) task =
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
    // Flee asks for no part and no energy state, only for a creep that is
    // being shot at and can run (ADR 0033). A Work-heavy body is exempt: at
    // four to seven ticks a step an Anchor leaving its Post neither escapes
    // nor digs, and the answer for the Post is a rampart (ADR 0034) — which
    // is also why the tile under one is in no Reach. The Reach it stands
    // in is its own room's (#138): a Threat on its coordinate a room away
    // is not shooting at it.
    | Flee ->
        not (Atlas.workHeavy atlas creep.Name)
        && (match Atlas.creepRoom atlas creep.Name, Atlas.creepTile atlas creep.Name with
            | Some room, Some tile -> Set.contains tile (Threats.reachIn threats room)
            | _ -> false)

/// The action Intent a Task asks of a creep, or None for a Task with no
/// action: Flee is movement and nothing else (ADR 0033), and the Emitter
/// issues it none.
let private intentFor (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> Some(HarvestSource(creep.Name, sourceId))
    | Withdraw storeId -> Some(WithdrawEnergyFromStructure(creep.Name, storeId))
    | Refill structureId -> Some(TransferEnergyToStructure(creep.Name, structureId))
    | Build siteId -> Some(BuildSite(creep.Name, siteId))
    | Repair structureId -> Some(RepairStructure(creep.Name, structureId))
    | Upgrade controllerId -> Some(UpgradeController(creep.Name, controllerId))
    | Flee -> None

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
    | Flee -> "🏃"

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
    /// Getting out of a Reach (ADR 0033): the one Task in it is Flee, and
    /// it sits above every other tier and above the downgrade deadline
    /// too, because no other work matters while a creep is being killed.
    | Safety
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
    // One rank beneath `deadlineRank`'s -1, which is itself one beneath the
    // shallowest tier of work: a fleeing creep outbids even a controller
    // about to downgrade (ADR 0033).
    | Safety -> -2
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
    | Flee -> Safety
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
/// window, silent, and digs the tick the energy lands. The tiles it may
/// act from are the ones it was judged applicable over — the Work Area
/// less this tick's Reach (ADR 0033) — so a creep no more works from a
/// tile a Threat took than it walks to one.
let private actionIntents
    (snapshot: Snapshot)
    atlas
    (threats: Threats)
    (creep: CreepInfo)
    (task: Task)
    : Intent list =
    let drained =
        match task with
        | Harvest sourceId -> ticksToRestock snapshot sourceId > 0
        | Withdraw _
        | Refill _
        | Build _
        | Repair _
        | Upgrade _
        | Flee -> false

    if
        Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task)
        && not drained
    then
        intentFor creep task |> Option.toList
    else
        []

/// Emitter: each assigned creep's action Intent, then every assigned
/// creep's chat bubble, both in Snapshot creep order. Judges actions from
/// tick-start geometry — it must run against the same Atlas the Matcher
/// used, never against resolved positions.
let emit (snapshot: Snapshot) atlas (threats: Threats) (assigned: Map<string, Task>) : Intent list =
    let actions =
        snapshot.Creeps
        |> List.collect (fun creep ->
            match Map.tryFind creep.Name assigned with
            | Some task -> actionIntents snapshot atlas threats creep task
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
///
/// Standing tiles but one (#142): a creep walked at a Seam is given the
/// exit tile itself as its last step, and that is a destination rather
/// than a place to stand — the engine moves a creep off a border tile at
/// the end of the tick, which is why no Seat, Work Area or standing
/// candidate query will ever name one. It is a tile of this room, so the
/// arbitration below settles it exactly as it settles ground.
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
///
/// The displacement tiles — the parked creep's, and the ones a creep
/// inside its area may shuffle to — are its own room's
/// (`Atlas.adjacentWalkableIn`): the Resolver arbitrates each projected
/// room by itself (#145, ADR 0041's Consequences), so the room a creep is
/// filed under rides in beside its tile, and a creep in an outpost is
/// offered that room's ground and never home's.
///
/// One tile is never a candidate: the creep's own, when that tile is a
/// Seam (`Atlas.standsOnSeam`) — the border ring the engine put it down
/// on the tick it crossed (#142). The ring is no room's ground (ADR
/// 0036), and a creep that ends its tick on it is moved out of the room
/// by the engine again, so "stay put" there is not a wait but a bounce
/// across the border every other tick. A ring creep therefore walks
/// inward first (#145): parked, its candidates are the ground tiles
/// beside it alone; travelling, its step comes first and the other ground
/// tiles beside it after, so a contested step becomes a step off the ring
/// rather than a stay on it. Only when no ground lies beside it at all is
/// its own tile offered, so a Move Intent's candidates stay non-empty and
/// arbitration's own fallback — stay, and let the engine fail the move —
/// is the answer it always was.
///
/// The Task goes to `Atlas.firstStep` beside the area, and that is what
/// gives a creep matched across a border somewhere to walk (#142): its
/// Work Area is empty here by construction — the standing tiles are the
/// neighbour's and a `Set<Pos>` cannot say so — so without the Task this
/// creep would park on a Task it was priced for and never move, holding it
/// against anti-thrash for the rest of its life. The step it gets back is
/// the near side of the Seam the price won at, which is a tile of this
/// creep's own room, so arbitration is handed nothing it could not already
/// arbitrate.
let private moveIntentFor
    (rankOf: Task -> int)
    (threats: Threats)
    atlas
    (room: string)
    (creep: string)
    (pos: Pos)
    (task: Task option)
    : MoveIntent =
    let beside = Atlas.adjacentWalkableIn atlas room pos
    let onSeam = Atlas.standsOnSeam atlas creep && not (List.isEmpty beside)

    // Where this creep may stay: its own tile, unless that tile is a Seam
    // with ground beside it to walk onto.
    let staying = if onSeam then [] else [ pos ]

    let parked rank =
        {
            Creep = creep
            Pos = pos
            Rank = rank
            Candidates = staying @ beside
        }

    match task with
    | None -> parked idleRank
    | Some task ->
        // The area less this tick's Reach (ADR 0033): a creep works from
        // the safe half of its Work Area rather than abandoning the Task
        // because one corner is hot, and its steps go nowhere else.
        let area = areaFor threats atlas creep task

        if Set.contains pos area then
            {
                Creep = creep
                Pos = pos
                Rank = rankOf task
                Candidates = pos :: (beside |> List.filter (fun tile -> Set.contains tile area))
            }
        else
            match Atlas.firstStep atlas creep task area with
            | Some step ->
                {
                    Creep = creep
                    Pos = pos
                    Rank = rankOf task
                    Candidates =
                        if onSeam then
                            step :: (beside |> List.filter ((<>) step))
                        else
                            [ step ]
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

/// One room's arbitration, settled: what each of its rested creeps was
/// settled on and what it asked for first, and the fatigued creeps' tiles
/// and the occupants the settlement was made against — the four things
/// the Verdicts read back, all keyed on that room's tiles alone (#145).
type private RoomPass =
    {
        /// Each rested creep's settled standing tile.
        Standing: Map<string, Pos>
        /// Each rested creep's preferred standing tile: the head of its
        /// candidate list — a Move Intent's candidates are never empty.
        Preferences: Map<string, Pos>
        /// The fatigued creeps' tiles, pre-claimed for the tick (ADR 0008).
        Blocked: Set<Pos>
        /// Who stands where at tick start.
        Occupants: Map<Pos, string>
    }

/// Resolver: every rested creep the Atlas places registers a Move Intent,
/// arbitration settles them into at most one single-step move per creep,
/// and the settled standing tiles become move Intents in Snapshot creep
/// order. Takes the tick's assigned Task per creep as data; a creep absent
/// from the map is idle. A fatigued creep sits arbitration out — the
/// engine would answer its move with ERR_TIRED — and its tile is blocked
/// for the tick, so nobody plans a step through it.
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
/// traffic-blind flood per traveller — so it is computed only for creeps
/// on the verbose list (ADR 0018), whose decision is about log noise and
/// stands now that the flood comes off the Atlas's shared memo (ADR 0030).
/// Grounded and yielded fall out of work the arbitration already did and
/// stay always-on.
///
/// Once per projected room, and never across two (#145): arbitrated
/// movement is a room's (ADR 0001, ADR 0008), and ADR 0041's Consequences
/// keep it so — geometry crosses the Seam and arbitration does not,
/// decomposed strictly per room as screeps-cartographer decomposes
/// `reconcileTraffic`. Each room's `occupants`, `blocked` and Move Intents
/// are built from `Atlas.placedCreepsByRoom`'s group for that room and
/// settled by `arbitrate` over that room's tiles alone — a `Map<Pos,
/// string>` and a `Set<Pos>` carry no room on their key, so a union across
/// rooms would collapse two creeps standing on one coordinate of two rooms
/// into one occupant and let a fatigued outpost creep pre-claim a home
/// tile, deleting a home creep's `MoveCreep` outright. `arbitrate` itself
/// is unchanged: it solves one room's Move Intents, and is simply called
/// per room. The Verdicts' attribution is each room's own for the same
/// reason. What is *not* arbitrated is the border tile: two creeps
/// aiming at one exit from its two sides are never checked against each
/// other, which ADR 0041 accepts in as many words.
///
/// What crosses the Seam is the *destination* (#142): a creep standing at
/// home and matched to an outpost's Task is arbitrated at home over a
/// step that is a home tile — the near side of the Seam it was priced at
/// — and the engine puts it down in the neighbour at the end of that
/// tick. The next tick the projection files it under the neighbour's
/// name, and that room's pass walks it on from its landing tile: the ring
/// is not ground, but a flood seeds its start tile regardless, so
/// `Atlas.firstStep` steps it off the ring onto the room's own floor
/// exactly as it stepped the near side onto the exit. Before #145 the
/// far side was deferred and the creep stood where it landed for the rest
/// of its life; that gap was what #126 waited on.
let resolve
    (snapshot: Snapshot)
    atlas
    (threats: Threats)
    (assigned: Map<string, Task>)
    (verbose: Set<string>)
    : Intent list * Verdict list =
    let byRoom = Atlas.placedCreepsByRoom atlas

    let tired =
        snapshot.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let settleRoom (room: string) (placed: (string * Pos) list) : RoomPass =
        let moveIntents =
            placed
            |> List.filter (fun (name, _) -> not (Set.contains name tired))
            |> List.map (fun (name, pos) ->
                moveIntentFor
                    (rank snapshot)
                    threats
                    atlas
                    room
                    name
                    pos
                    (Map.tryFind name assigned))

        let blocked =
            placed
            |> List.choose (fun (name, pos) -> if Set.contains name tired then Some pos else None)
            |> Set.ofList

        let occupants = placed |> List.map (fun (name, pos) -> pos, name) |> Map.ofList

        {
            Standing = arbitrate occupants blocked moveIntents
            Preferences =
                moveIntents |> List.map (fun i -> i.Creep, List.head i.Candidates) |> Map.ofList
            Blocked = blocked
            Occupants = occupants
        }

    // Every placed creep beside its room's pass, in Snapshot creep order
    // across the rooms: the order the Intents and Verdicts leave in.
    let placed =
        byRoom
        |> List.map (fun (room, placed) -> room, placed, settleRoom room placed)
        |> List.collect (fun (_, placed, pass) ->
            placed |> List.map (fun (name, pos) -> name, pos, pass))
        |> List.sortBy (fun (name, _, _) ->
            snapshot.Creeps |> List.findIndex (fun c -> c.Name = name))

    let intents =
        placed
        |> List.choose (fun (name, pos, pass) ->
            Map.tryFind name pass.Standing
            |> Option.bind (directionTo pos)
            |> Option.map (fun direction -> MoveCreep(name, direction)))

    // Who holds a tile this creep did not get, in this creep's room: the
    // creep settled on it, or the fatigued occupant whose blocked tile
    // pre-claimed it.
    let counterpartAt (pass: RoomPass) tile self =
        pass.Standing
        |> Map.tryPick (fun name settled ->
            if settled = tile && name <> self then Some name else None)
        |> Option.orElse (
            if Set.contains tile pass.Blocked then
                Map.tryFind tile pass.Occupants
            else
                None
        )

    let rerouted name task =
        let area = areaFor threats atlas name task

        match
            Atlas.firstStep atlas name task area,
            Atlas.firstStepIgnoringTraffic atlas name task area
        with
        | Some priced, Some blind -> priced <> blind
        | _ -> false

    let verdicts =
        placed
        |> List.collect (fun (name, _, pass) ->
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
                    match Map.tryFind name pass.Preferences, Map.tryFind name pass.Standing with
                    | Some preferred, Some settled when settled <> preferred ->
                        counterpartAt pass preferred name
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
    (threats: Threats)
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
    // a judgement about now, and nothing in 0026 revises the key. The
    // count is a fold: the question is how many, and a Map built only to
    // be measured and dropped costs an allocation and a structural insert
    // per holder, once per scored pair.
    let load (acc: Assignments) tid =
        acc |> Map.fold (fun n _ assigned -> if assigned = tid then n + 1 else n) 0

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
                // The raid's release stands ahead of the ordinary one: a
                // Task whose whole Work Area is in a Reach is gone for this
                // creep however well its body fits (ADR 0033).
                | Some task when threatened threats atlas creep task ->
                    release ReleaseReason.Threatened
                | Some task when not (applicable threats atlas creep task) ->
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
                    let cost = travelCostOf threats atlas creep.Name task
                    let arrival = lazy (Atlas.walkTicks atlas creep.Name task)

                    if
                        not (hasCapacity creep acc task arrival)
                        && not (expiring snapshot atlas creep)
                    then
                        release ReleaseReason.OverCapacity
                    else
                        match cost with
                        | None -> release ReleaseReason.Unreachable
                        | Some _ ->
                            match tooEarly snapshot atlas creep task arrival with
                            | Some(walk, wait) -> release (ReleaseReason.TooEarly(walk, wait))
                            | None -> Map.add name tid acc, released)

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

        if threatened threats atlas creep task then
            Candidate.Rejected(tid, RejectReason.Threatened)
        elif not (applicable threats atlas creep task) then
            Candidate.Rejected(tid, RejectReason.Inapplicable)
        else
            let cost = travelCostOf threats atlas creep.Name task
            let arrival = lazy (Atlas.walkTicks atlas creep.Name task)

            if not (hasCapacity creep acc task arrival) then
                Candidate.Rejected(tid, RejectReason.CapacityFull)
            else
                match cost with
                | None -> Candidate.Rejected(tid, RejectReason.Unreachable)
                | Some cost ->
                    match tooEarly snapshot atlas creep task arrival with
                    | Some(walk, wait) -> Candidate.Rejected(tid, RejectReason.TooEarly(walk, wait))
                    | None -> Candidate.Scored(tid, rank snapshot task, cost, load acc tid)

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
                        | _, Candidate.Rejected(_, reason) -> wanted reason
                        | _ -> false)

                // The arrival gate's reason carries the numbers it compared
                // (#88), so the depth question asks after the case rather
                // than after a value it would have to invent to compare to.
                let isTooEarly =
                    function
                    | RejectReason.TooEarly _ -> true
                    | _ -> false

                let reason =
                    if List.isEmpty tasks then
                        IdleReason.NoTasks
                    elif rejectedWith isTooEarly then
                        IdleReason.NoneInTime
                    elif rejectedWith ((=) RejectReason.Unreachable) then
                        IdleReason.NoneReachable
                    elif rejectedWith ((=) RejectReason.CapacityFull) then
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
/// spawn/extension census and the controller level, both covered here.
///
/// It signs **one room**, and since ADR 0041 it says which: the home
/// entry of the layer, the room `RoomName` names and `SpatialInfo.homeName`
/// spells the empty string when it names none. The single `RoomName` no
/// longer has a colony to outgrow, because everything the memo carries is
/// that one room's — the Layout is anchored in it and stamps it onto every
/// site, the spawn walks flood its grid alone (ADR 0032), and the hauler
/// quota prices its containers alone. A second room's structures entering
/// the kind census must therefore leave this string alone: they move
/// nothing the memo holds, and a signature that flinched at them would
/// throw the whole Layout and the whole walk table away for geometry no
/// entry of the memo reads. What makes them *not* enter is the position
/// join — the census is (kind, position), and the position is read out of
/// the home room's layer, so an id the home room does not place carries no
/// entry. The tick that a memo entry does read a second room — the outpost
/// container plan and its quota (ADR 0042) — is the tick this has to
/// widen, and widening it means naming the room in the entry, because two
/// rooms hold the same coordinates and `Container@16,44` in either would
/// otherwise be the same census.
let censusSignature (snapshot: Snapshot) : string =
    let spatial = snapshot.Spatial
    let home = SpatialInfo.homeName spatial
    let placed = (SpatialInfo.layerOf spatial home).TargetPositions

    let census select =
        spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            select kind
            |> Option.bind (fun (built: BuiltKind) ->
                Map.tryFind id placed |> Option.map (fun pos -> $"{built}@{pos.X},{pos.Y}")))
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

    $"{home}|{level}|{standing}|{pending}"

/// The decision seam: Snapshot in — with the verbose list of creep names
/// owed the manufactured-evidence Verdicts (full candidate scoring, reroute
/// attribution) and the previous tick's plan memo — Decision out. The tick's pipeline is visible here — plan, match, emit, resolve —
/// beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same
/// flood (ADR 0004). The census-derived plans — the Layout's site Intents
/// and the hauler quota — are reused verbatim from a memo whose signature
/// matches this tick's census, and recomputed otherwise (ADR 0017); the
/// same memo hands the Atlas the spawn walks behind the leads, recalled
/// under an unchanged signature and dropped whole under a moved one
/// (ADR 0032).
let decide
    (snapshot: Snapshot)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    let signature = censusSignature snapshot
    // The signature is read before the Atlas is built, because the Atlas
    // is one of the things it decides: a memo whose census still stands
    // hands over its spawn walk table, and a memo that has gone stale —
    // or none at all — leaves the Atlas a fresh one (ADR 0032).
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    let walks =
        match recalled with
        | Some m -> m.Walks
        | None -> WalkTable()

    let atlas = Atlas.ofSnapshotRecalling walks snapshot

    let plan =
        match recalled with
        | Some m -> m
        | None ->
            let siteIntents, servedFootings, unservedFootings, unroutedTrunks, deferredContainers =
                planLayout snapshot atlas

            {
                Signature = signature
                SiteIntents = siteIntents
                UnservedFootings = unservedFootings
                ServedFootings = servedFootings
                UnroutedTrunks = unroutedTrunks
                DeferredContainers = deferredContainers
                HaulerQuota = haulerQuota snapshot atlas
                Walks = walks
            }

    // The tick's Threats, derived once off the Snapshot's hostiles and the
    // rampart census, and shared by every reader of them (ADR 0033).
    let threats = threatsOf snapshot atlas

    let defenseIntents = planSafeMode snapshot atlas @ planFire snapshot atlas
    let spawnIntents = planSpawns snapshot atlas threats plan.HaulerQuota
    let pickupIntents = planPickups snapshot atlas
    let tasks = planTasks snapshot threats
    let next, verdicts = matchCreeps snapshot atlas threats tasks assignments verbose
    let assigned = assignedTasks tasks next
    let moveIntents, moveVerdicts = resolve snapshot atlas threats assigned verbose

    {
        Intents =
            defenseIntents
            @ spawnIntents
            @ plan.SiteIntents
            @ pickupIntents
            @ emit snapshot atlas threats assigned
            @ moveIntents
        Assignments = next
        Memo = plan
        Verdicts = verdicts @ moveVerdicts
    }
