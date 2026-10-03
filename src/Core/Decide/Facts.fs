/// A Task's identity across ticks, and the colony-level predicates read all over
/// the pipeline: the stage a room stands at, whether the colony is independent,
/// and which of its structures are hungry.
[<AutoOpen>]
module Fabot.Core.Decide.Facts

open Fabot.Core
open Fabot.Core.Types

/// The resource a Task id names, and nothing at all for energy: a Task id is a
/// key carried across ticks (Memory, Assignments, every `observe` transition
/// line), so widening every `withdraw:` id would orphan every standing
/// assignment on the tick the bundle is deployed. A second resource needs a
/// distinct id at the same store, which a suffix gives it.
let private resourceSuffix =
    function
    | Energy -> ""
    | Thorium -> ":Thorium"

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw(storeId, resource) -> $"withdraw:{storeId}{resourceSuffix resource}"
    | Refill(structureId, resource) -> $"refill:{structureId}{resourceSuffix resource}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"
    | Reserve controllerId -> $"reserve:{controllerId}"
    | Claim controllerId -> $"claim:{controllerId}"
    // Identified by the object the declaration names — the engine's own id,
    // so the Memory key survives every tick the room is dark.
    | Reclaim reactorId -> $"reclaim:{reactorId}"
    | Dismantle structureId -> $"dismantle:{structureId}"
    | Pickup(pileId, resource) -> $"pickup:{pileId}{resourceSuffix resource}"
    // One Flee for the whole colony: every creep inside a Reach is running
    // from the same thing.
    | Flee -> "flee"
    // One Guard per raided room, identified by the room and never by what
    // stands in it: a raid that loses a creep is the same fight and must be
    // the same identity, or anti-thrash would re-match the guard mid-swing.
    | Guard room -> $"guard:{room}"
    // Keyed on the room for the Guard's reason: a squad that loses a member
    // is the same fight.
    | Fight room -> $"fight:{room}"

/// The target inside a Task id: what `taskId` writes between its first colon
/// and the next one (neither an object id nor a room name holds a colon).
/// `None` for Flee. Read by the vision grace, which holds an id and no Task
/// and stays blind to Task kinds.
let private taskTarget (tid: string) : string option =
    match tid.IndexOf ':' with
    | -1 -> None
    | colon ->
        let rest = tid.Substring(colon + 1)

        match rest.IndexOf ':' with
        | -1 -> Some rest
        | next -> Some(rest.Substring(0, next))

/// The vision grace's one question (#151): the room a held Task's target was
/// last seen standing in, when that room has gone **dark** inside
/// `Tuning.VisionGrace` and not longer. `None` is every other answer — the room
/// is in view this tick, it was never seen, the darkness has outlasted the
/// grace, or the id names no target at all.
///
/// A Task leaves the pool for two opposite reasons and its id alone cannot tell
/// them apart: the target was destroyed, or the room carrying it went dark. The
/// pool stays gated on vision, so this asks about looking and never about the
/// target. Two readers, the two halves of one rule: the Matcher keeps the
/// assignment, and the mover walks its holder at the room the answer names
/// (`Atlas.stepTowardRoom`).
let internal lastSeenIn (view: ColonyView) (tid: string) : string option =
    match taskTarget tid with
    | None -> None
    | Some target ->
        view.Sightings
        |> Map.tryPick (fun room sighting ->
            if
                sighting.Tick < view.Time
                && view.Time - sighting.Tick <= view.Tuning.VisionGrace
                && Set.contains target sighting.Targets.Value
            then
                Some room
            else
                None)

/// The stage of the colony whose home is the named room, off the one
/// derivation the shell ran for the tick. `None` for a room no colony of ours
/// lives in.
let internal roomStage (view: ColonyView) room = Map.tryFind room view.Stages

/// This colony's own stage: its home room's entry. `None` is the projection
/// that cannot place its own controller, and every reader gives that colony
/// the answer it gives one standing under the line.
let internal homeStage (view: ColonyView) =
    roomStage view (SpatialInfo.homeName view.Spatial)

/// Whether this colony has outgrown its bootstrap window: `Independent`,
/// at `Tuning.BootstrapLevel` or past it — or `Weaning`, whose tower holds
/// only its mother's lend and none of its own rules (#445).
let internal isIndependent (view: ColonyView) =
    match homeStage view with
    | Some Independent
    | Some Weaning -> true
    | Some Nursery
    | Some Bootstrapping
    | None -> false

/// ADR-0034. Whether this colony keeps ramparts this tick: the covering rule's
/// one gate and the floor's one gate, one spelling for both, so a colony below
/// it places no rampart and counts none of its standing ramparts hungry.
let internal keepsRamparts (view: ColonyView) = isIndependent view

/// ADR-0061. The facts the Planner reads about the assignment table, derived
/// once in `Entry` and handed down to the pool. `All` is read as
/// `Set.contains (taskId t) held.All`; `WithThorium` binds delivery persistence
/// to the living holder that still carries the paid-for load.
type HeldTaskFacts =
    {
        All: Set<string>
        WithThorium: Set<string>
        /// Each held Task id's living holders, by name.
        Holders: Map<string, string list>
        /// Every resident room a Fight is pooled for this tick, with the squad
        /// priced for it (`fights`): derived once off the holders, and read by
        /// the ground, the pool, the caps and the rows alike.
        Fights: Map<string, Squad>
    }

[<RequireQualifiedAccess>]
module HeldTaskFacts =
    let empty =
        {
            All = Set.empty
            WithThorium = Set.empty
            Holders = Map.empty
            Fights = Map.empty
        }

    /// One Task id's living holders.
    let holdersOf (held: HeldTaskFacts) (tid: string) : string list =
        Map.tryFind tid held.Holders |> Option.defaultValue []

/// Whether a structure of this kind, carrying these hits, is hungry: its
/// kind's line, and for the decaying kinds the held fact picks which of the
/// two. A kind with no line is never hungry. The floor is capped at the
/// structure's own max so a rampart whose max is somehow under it can still
/// be whole.
let private isHungry (tuning: Tuning) (held: Set<string>) id kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction ->
        let line =
            if Set.contains (taskId (Repair id)) held then
                tuning.RepairWholeLine
            else
                tuning.RepairTrigger

        float hits.Hits < line * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min tuning.RampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// The home room's tile of one of its structures, or None for one placed
/// nowhere or in another room.
let private homeTile (view: ColonyView) (id: string) : RoomPos option =
    let home = SpatialInfo.homeName view.Spatial

    Map.tryFind id (SpatialInfo.layerOf view.Spatial home).TargetPositions
    |> Option.map (RoomPos.at home)

/// The home's ramparts of ours under attack this tick (#467): a hostile's
/// weapon reaches the tile, or a WORK body stands beside it to dismantle.
/// Read off where the hostiles stand, because the projection carries no
/// hits from the tick before to see them falling.
let internal rampartsUnderAttack (view: ColonyView) : Set<string> =
    match view.Hostiles with
    | [] -> Set.empty
    | hostiles ->
        let strikes (tile: RoomPos) (hostile: HostileInfo) =
            match RoomPos.range hostile.Pos tile with
            | Some r ->
                HostileInfo.weaponRange hostile |> Option.exists (fun reach -> r <= reach)
                || r <= Engine.meleeRange && List.contains Work hostile.Body
            | None -> false

        SpatialInfo.structureHits view.Spatial
        |> List.choose (fun (id, kind, _) ->
            match kind with
            | BuiltKind.Rampart ->
                homeTile view id
                |> Option.filter (fun tile -> hostiles |> List.exists (strikes tile))
                |> Option.map (fun _ -> id)
            | _ -> None)
        |> Set.ofList

/// Every structure the projection carries hits for that stands below its kind's
/// line, with its kind, in id order — the Repair pool's own walk. The
/// safe-mode reflex's Keep arm asks `keepDamaged` instead.
let internal hungryStructures (view: ColonyView) (held: Set<string>) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view
    let attacked = rampartsUnderAttack view

    SpatialInfo.structureHits view.Spatial
    |> List.choose (fun (id, kind, hits) ->
        match kind with
        // A rampart under attack is hungry to its max, kept or not: it is
        // what stands between the raid and the room.
        | BuiltKind.Rampart when Set.contains id attacked ->
            if hits.Hits < hits.HitsMax then Some(id, kind) else None
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | BuiltKind.Rampart when not ramparts -> None
        // Nor is a child's: the mother carries it as ground, not work.
        | _ when
            isHungry view.Tuning held id kind hits
            && not (kind = BuiltKind.Rampart && SpatialInfo.placedAway view.Spatial id)
            ->
            Some(id, kind)
        | _ -> None)

/// Whether any Keep structure of this colony stands below full hits: the
/// safe-mode reflex's own question. Its own predicate and not a filter over
/// `hungryStructures`, because this arm needs no held set. A second walk over
/// a hundred-odd structure hits, which is not a flood (#171).
let internal keepDamaged (view: ColonyView) : bool =
    SpatialInfo.structureHits view.Spatial
    |> List.exists (fun (_, kind, hits) -> isKeep kind && hits.Hits < hits.HitsMax)

/// `HostileInfo.weaponRange`, under the name every rule here reads it by.
let internal weaponRange (hostile: HostileInfo) : int option = HostileInfo.weaponRange hostile

/// `HostileInfo.isArmed`, likewise.
let internal isArmed (hostile: HostileInfo) : bool = HostileInfo.isArmed hostile

/// Whether a hostile is a Source Keeper: the room's own NPC, never a raid.
let internal isKeeper (hostile: HostileInfo) : bool = HostileInfo.isKeeper hostile

/// Whether a hostile raids a resident room: armed, and not a Source Keeper.
let internal isRaider (hostile: HostileInfo) : bool =
    isArmed hostile && not (isKeeper hostile)

/// Whether a projected target stands in a room this player owns, which is how
/// every rule of the season's ore answers "is this ours?" (#261, #311).
/// `FIND_MINERALS`, `FIND_STRUCTURES` and `FIND_DROPPED_RESOURCES` all carry
/// every owner's, and nothing in the shape of a deposit, an extractor, a
/// container or a pile says who put it there; the engine says it once, about
/// the room (an extractor needs an owned RCL6 room). For a pile it says only
/// that the floor under it is ours, which is the only question a Pickup asks.
/// A room the colony cannot see owns nothing here.
let internal inARoomWeOwn (view: ColonyView) (id: string) : bool =
    SpatialInfo.roomOf view.Spatial id
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// The Thorium deposits standing in a room this colony owns, in id order — the
/// list both the Task pool and the miner row's quota are read off. A scanned
/// neighbour arrives in the projection with its own deposit, extractor and
/// container, and `harvest` refuses a mineral whose extractor belongs to
/// somebody else: one `ERR_NOT_OWNER` a tick for the whole of a body's life.
let internal ourDeposits (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Mineral |> List.filter (inARoomWeOwn view)

/// The rooms this colony has declared an errand in, as a set — the join four
/// rules of the season's ore make, written once (#378).
let internal errandRooms (view: ColonyView) : Set<string> =
    view.Errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

/// The [[harassment room]]s this colony casts this tick, as a set.
let internal harassRooms (view: ColonyView) : Set<string> =
    view.Harass |> List.map (fun h -> h.RoomName) |> Set.ofList

/// Whether a hostile is the declared enemy's, standing in the harassment room
/// that names it: armed or not, a target there. Nobody else's creep is.
let internal harassTarget (view: ColonyView) (hostile: HostileInfo) : bool =
    view.Harass
    |> List.exists (fun h -> h.RoomName = hostile.Pos.Room && h.Enemy = hostile.Owner)

/// Whether a hostile is a rival's CLAIM body standing in one of these
/// resident rooms: unarmed, and the thing that takes the flag (#406, #414) or
/// attacks a raised child's controller (#447).
let internal claimsAFlag (residentRooms: Set<string>) (hostile: HostileInfo) =
    Set.contains hostile.Pos.Room residentRooms
    && List.contains BodyPart.Claim hostile.Body

/// The children's homes this colony is raising (#447): its borrowed rooms at
/// `Nursery`, `Bootstrapping` or `Weaning`, `Colony.bootstrapping`'s half of
/// them. A lost child's home, the other half, has no stage. And a contested
/// `Independent` child's short of its towers (`BorrowedWork.Garrisoned`, #479).
let internal raisedHomes (view: ColonyView) : Set<string> =
    view.Borrowed.Rooms
    |> List.filter (fun room ->
        match roomStage view room with
        | Some Nursery
        | Some Bootstrapping
        | Some Weaning -> true
        | Some Independent
        | None -> false)
    |> List.append view.Borrowed.Garrisoned
    |> Set.ofList

/// The rooms the ranger row keeps its resident garrison in, raid or none
/// (#419): the errand rooms, and the children's homes this colony raises
/// (#447). One rule for both, read by the row, the pool's cap, the ground and
/// the target.
let internal residentRooms (view: ColonyView) : Set<string> =
    Set.union (errandRooms view) (raisedHomes view)

/// The rooms whose Guard is the ranger's: the resident rooms and the
/// harassment rooms (#411, #432). Every other guarded room is the guard's.
let internal rangerRooms (view: ColonyView) : Set<string> =
    Set.union (residentRooms view) (harassRooms view)

/// Whether two tiles stand within `reach` of each other; never across rooms.
let private within (reach: int) (a: RoomPos) (b: RoomPos) =
    RoomPos.range a b |> Option.exists (fun r -> r <= reach)

/// Whether a hostile is a raid's healer (#451): unarmed, with a HEAL part
/// still acting, and standing within three of an armed hostile — part of the
/// raid, which it keeps alive.
let internal raidHealer (view: ColonyView) (hostile: HostileInfo) : bool =
    not (isArmed hostile)
    && HostileInfo.activeCount hostile Heal > 0
    && view.Hostiles
       |> List.exists (fun other ->
           isArmed other && within Engine.rangedRange other.Pos hostile.Pos)

/// What a Guard in its room may shoot: an armed hostile, a rival's claimer or
/// a raid's healer (#451) in a resident room, and the declared enemy's creep
/// in a harassment room. In a room a Fight is pooled for, any healer: one
/// whose raid is dead is still the raid's. `Emitter.guardTarget` and the
/// harassment ring read this one predicate.
let internal guardShoots
    (view: ColonyView)
    (residentRooms: Set<string>)
    (fighting: Set<string>)
    (hostile: HostileInfo)
    =
    isArmed hostile
    || claimsAFlag residentRooms hostile
    || harassTarget view hostile
    || Set.contains hostile.Pos.Room residentRooms && raidHealer view hostile
    || Set.contains hostile.Pos.Room fighting
       && HostileInfo.activeCount hostile Heal > 0

/// What a body deals a tick, off its part counts: melee and ranged, unboosted.
let private damageOf (parts: BodyPart -> int) =
    Engine.attackPower * parts Attack
    + Engine.rangedAttackPower * parts RangedAttack

/// What a body of ours heals itself a tick while it fights: a ranged body's
/// heal acts beside its shot (#411), and a melee body's is suppressed by its
/// own attack, so it heals nothing.
let private selfHealOf (parts: BodyPart -> int) =
    if parts RangedAttack > 0 then
        Engine.healPower * parts Heal
    else
        0

/// Which band of the kill order a target falls in (#451), first band first.
type KillTier =
    /// A rival's CLAIM body in a resident room, anywhere in it:
    /// `claimReactor` has no cooldown and a controller attack lands the tick
    /// it stands beside, so the whole approach is the only window.
    | Claimer
    /// A raid's healer whose heal our damage reaching it outpaces.
    | BrokenHealer
    /// The rest of the raid: an armed body, or a healer we do not break.
    | Raid
    /// Anything else the Guard may shoot.
    | Bystander

/// One target's place in the kill order, compared field by field.
[<Struct>]
type KillRank =
    {
        Tier: KillTier
        /// Its hits, plus the heal that reaches it this tick, less our damage
        /// that does. Without a lock this all but locks itself: the target we
        /// hurt stays the lowest.
        EffectiveHits: int
    }

/// What one target's band in the kill order is read off: what it is, and the
/// heal and our damage that reach it this tick.
[<Struct>]
type private KillFacts =
    {
        ClaimsAFlag: bool
        RaidHealer: bool
        Armed: bool
        HealOn: int
        DamageOn: int
    }

/// One target's band in the kill order: the one rule the Emitter's live order
/// (`killRank`) and `squadFight`'s simulated one share.
let private killTier (target: KillFacts) =
    if target.ClaimsAFlag then
        Claimer
    elif target.RaidHealer && target.DamageOn > target.HealOn then
        BrokenHealer
    elif target.Armed || target.RaidHealer then
        Raid
    else
        Bystander

/// The heal that reaches a hostile in `room` this tick, its own included: a
/// heal reaches one tile, a ranged heal three. A closure over the room's
/// healers, found once.
let internal healReaching (view: ColonyView) (room: string) : HostileInfo -> int =
    // A healer remembered from the last few ticks counts where it was last
    // seen (#480).
    let healers =
        view.Hostiles @ view.RecalledHealers
        |> List.filter (fun h -> h.Pos.Room = room && HostileInfo.healing h > 0)

    fun hostile ->
        healers
        |> List.sumBy (fun healer ->
            match RoomPos.range healer.Pos hostile.Pos with
            | Some r when r <= Engine.meleeRange -> HostileInfo.healing healer
            | Some r when r <= Engine.rangedRange ->
                HostileInfo.healing healer * Engine.rangedHealPower / Engine.healPower
            | _ -> 0)

/// Our creeps' damage that reaches a hostile this tick: a swing one tile, a
/// shot three. A closure over our placed creeps, found once.
let internal damageReaching (view: ColonyView) : HostileInfo -> int =
    let fighters =
        view.Creeps
        |> List.choose (fun creep ->
            SpatialInfo.creepPlacementOf view.Spatial creep.Name
            |> Option.map (fun at -> at, partCount creep.Body))

    fun hostile ->
        fighters
        |> List.sumBy (fun (at, parts) ->
            match RoomPos.range at hostile.Pos with
            | Some r when r <= Engine.meleeRange -> damageOf parts
            | Some r when r <= Engine.rangedRange ->
                damageOf (fun part -> if part = Attack then 0 else parts part)
            | _ -> 0)

/// The kill order's leading key for one room's targets (#451), smallest
/// first; the caller breaks ties. Outside a ranger's room (an outpost's
/// melee guard) the raid first and nothing more.
let internal killRank (view: ColonyView) (room: string) : HostileInfo -> KillRank =
    if not (Set.contains room (rangerRooms view)) then
        fun hostile ->
            {
                Tier = (if isArmed hostile then Raid else Bystander)
                EffectiveHits = 0
            }
    else
        let resident = residentRooms view
        let healOn = healReaching view room
        let damageOn = damageReaching view

        fun hostile ->
            let heal = healOn hostile
            let damage = damageOn hostile

            {
                Tier =
                    killTier
                        {
                            ClaimsAFlag = claimsAFlag resident hostile
                            RaidHealer = raidHealer view hostile
                            Armed = isArmed hostile
                            HealOn = heal
                            DamageOn = damage
                        }
                EffectiveHits = hostile.Hits + heal - damage
            }

/// Whether our side — this damage, healing and hits a tick — wins the exchange
/// against the raid standing in one room: two clocks compared,
/// cross-multiplied to stay in whole numbers. A raid that out-heals our damage
/// is never killed. Healers are priced in the healing and never in the hits.
/// The raid's durability is priced at full off its parts; over-stating what
/// it can take is the safe direction.
let internal exchangeWon
    (view: ColonyView)
    (room: string)
    (ourDamage: int)
    (ourHealing: int)
    (ourHits: int)
    : bool =
    let raid = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)

    let raidDamage = raid |> List.sumBy (fun h -> damageOf (partCountIn h.Body))

    let raidHealing = raid |> List.sumBy HostileInfo.healing

    let raidHits =
        raid
        |> List.filter isArmed
        |> List.sumBy (fun h -> Engine.partHits * List.length h.Body)

    if raidDamage = 0 then
        true
    elif ourDamage <= raidHealing then
        false
    elif raidDamage <= ourHealing then
        true
    else
        raidHits * (raidDamage - ourHealing) < ourHits * (ourDamage - raidHealing)

/// `exchangeWon` for one body of these active part counts and hits: what a
/// body standing in the room is weighed by (`Quota.homeHolds`).
let internal bodyWins (view: ColonyView) (room: string) (parts: BodyPart -> int) (hits: int) =
    exchangeWon view room (damageOf parts) (selfHealOf parts) hits

/// Whether `blocks` whole blocks of a fighting row win `exchangeWon`, each
/// block's damage and self-heal (`selfHealOf`) times the count, beside
/// `besideDamage` a tick that is not a body's. Here and not in `Quota`
/// because the ranger's ground reads it too (`outmatched`).
///
/// Worked example: a lone smallMelee needs one guard block; backed by a
/// smallHealer, its 40 damage kills our 1,000 hits in 25 ticks, before our 30
/// net damage kills its 1,000 hits, so that raid needs the second block.
let private blocksBeat
    (block: BodyPart list)
    (besideDamage: int)
    (view: ColonyView)
    (room: string)
    (blocks: int)
    : bool =
    let parts = partCountIn block

    exchangeWon
        view
        room
        (blocks * damageOf parts + besideDamage)
        (blocks * selfHealOf parts)
        (blocks * Engine.partHits * List.length block)

/// `blocksBeat` for the guard's melee block. Two readers: the guard row asks
/// it of one block to size the crowd, and the stand-down asks it of the
/// biggest body the bank buys (`guardBlocksReach`) to decide whether an
/// outpost is a fight or a withdrawal (`Observe.raidDeadlines`).
let guardBlocksBeat (view: ColonyView) (room: string) (blocks: int) : bool =
    blocksBeat guardPattern.Block 0 view room blocks

/// `blocksBeat` for the ranger's block (#411), which the errand room's
/// stand-down and the ranger row read as the guard's do an outpost's.
let rangerBlocksBeat (view: ColonyView) (room: string) (blocks: int) : bool =
    blocksBeat rangerPattern.Block 0 view room blocks

/// What a room's loaded towers land a tick on one target, priced at the
/// falloff range: a raised home's, beside whatever body of ours fights there.
let private towerDamageIn (view: ColonyView) (room: string) =
    let towers = Map.tryFind room view.LoadedTowers |> Option.defaultValue 0
    towers * Engine.towerAttackAt Engine.towerFalloffRange

/// Whether no ranger body of any size wins the raid standing in a room alone
/// (#451): a raid the ranger row casts nobody into, and the residents already
/// there hold the room's safe ground against. A raised home's loaded towers
/// fight beside it, priced at the falloff range.
let internal outmatched (view: ColonyView) (room: string) : bool =
    not (blocksBeat rangerPattern.Block (towerDamageIn view room) view room rangerBlocksMost)

/// One body in `squadFight`: its parts, head first, and the hits it has
/// left, which is the engine's whole account of which parts still act.
[<Struct>]
type private Combatant =
    {
        Parts: BodyPart list
        Left: int
        /// Its hits whole: the cap on its heal.
        Full: int
        /// A rival's CLAIM body in a resident room: the kill order's first band.
        Claims: bool
    }

/// The longest fight `squadFight` plays out: one not won in sixty ticks is
/// not won.
let private squadTicksMost = 60

/// The most bodies, both sides together, `squadFight` prices; a bigger field
/// is priced lost.
let private squadBodiesMost = 10

/// Whom our side shoots in `squadFight`: the kill order the Emitter plays
/// (`killTier`), or the raid's armed bodies ahead of its healers — the order
/// the research priced the duo losing with, kept to show the order matters.
type SquadAim =
    | KillOrder
    | MeleeFirst

/// How a squad fight ends: whether we hold a weapon when the raid holds
/// none, and the tick it is decided on.
type SquadFight = { Won: bool; Ticks: int }

/// What one combatant's live parts do this tick, counted once: the ATTACK,
/// RANGED_ATTACK and HEAL parts its hits still cover.
[<Struct>]
type private Strength = { Swings: int; Shots: int; Heals: int }

let private strengthOf (c: Combatant) : Strength =
    let none = { Swings = 0; Shots = 0; Heals = 0 }

    if c.Left <= 0 then
        none
    else
        Engine.liveParts c.Parts c.Left
        |> List.fold
            (fun s part ->
                match part with
                | Attack -> { s with Swings = s.Swings + 1 }
                | RangedAttack -> { s with Shots = s.Shots + 1 }
                | Heal -> { s with Heals = s.Heals + 1 }
                | _ -> s)
            none

let private armedWith (s: Strength) = s.Swings > 0 || s.Shots > 0

/// The kill order's bands as numbers, first band smallest.
let private bandOf =
    function
    | Claimer -> 0
    | BrokenHealer -> 1
    | Raid -> 2
    | Bystander -> 3

/// One side's heal this tick, given the damage each of its bodies takes:
/// each healer in turn pre-heals the body most threatened — the damage
/// coming plus the hits already missing, less the heal already given it —
/// all heal adjacent. A body that swings this tick heals nothing, its attack
/// suppressing its heal.
let private healsOf
    (side: Combatant array)
    (strength: Strength array)
    (incoming: int array)
    (swung: bool array)
    =
    let n = Array.length side
    let given = Array.zeroCreate n

    let need j =
        incoming[j] + side[j].Full - side[j].Left - given[j]

    for i in 0 .. n - 1 do
        let power = Engine.healPower * strength[i].Heals

        if side[i].Left > 0 && power > 0 && not swung[i] then
            let mutable best = -1

            for j in 0 .. n - 1 do
                if side[j].Left > 0 && (best < 0 || need j > need best) then
                    best <- j

            if best >= 0 && need best > 0 then
                given[best] <- given[best] + power

    given

/// ADR-0083
/// How this squad, cast whole and arriving together, fights the raid standing
/// in one room, beside the room's loaded towers: the bounded simulation the
/// ADR describes, on start-of-tick parts. Our fire re-picks the kill order's
/// head each tick as the Emitter does, with no lock: every body reaching
/// every body, effective hits order as hits do, so the target we hurt stays
/// the head unless a band above it opens. Source Keepers are no part of it.
let squadFight
    (view: ColonyView)
    (room: string)
    (members: BodyPart list list)
    (kite: bool)
    (aim: SquadAim)
    : SquadFight =
    let resident = residentRooms view

    let combatant parts hits claims =
        {
            Parts = parts
            Left = hits
            Full = Engine.partHits * List.length parts
            Claims = claims
        }

    let raid =
        view.Hostiles
        |> List.filter (fun h -> h.Pos.Room = room && not (isKeeper h))
        |> List.map (fun h -> combatant h.Body h.Hits (claimsAFlag resident h))
        |> Array.ofList

    let ours =
        members
        |> List.map (fun body -> combatant body (Engine.partHits * List.length body) false)
        |> Array.ofList

    let towers = towerDamageIn view room

    let melee (s: Strength) =
        if kite then 0 else Engine.attackPower * s.Swings

    let shot (s: Strength) =
        melee s + Engine.rangedAttackPower * s.Shots

    let bandFor (c: Combatant) (s: Strength) raidHeal ourDamage =
        let armed = armedWith s

        match aim with
        | KillOrder ->
            bandOf (
                killTier
                    {
                        ClaimsAFlag = c.Claims
                        RaidHealer = s.Heals > 0 && not armed
                        Armed = armed
                        HealOn = raidHeal
                        DamageOn = ourDamage
                    }
            )
        | MeleeFirst ->
            if c.Claims then 0
            elif armed then 1
            else 2

    // The kill order's head: band, then hits, then the first standing.
    let targetOf (raid: Combatant array) (raidS: Strength array) raidHeal ourDamage =
        let mutable best = -1
        let mutable bestBand = 0

        for i in 0 .. Array.length raid - 1 do
            if raid[i].Left > 0 then
                let band = bandFor raid[i] raidS[i] raidHeal ourDamage

                if
                    best < 0 || band < bestBand || band = bestBand && raid[i].Left < raid[best].Left
                then
                    best <- i
                    bestBand <- band

        best

    // The raid's front: our first standing body with ATTACK, else our lowest.
    let frontOf (ours: Combatant array) (oursS: Strength array) =
        let mutable brawler = -1
        let mutable lowest = -1

        for i in 0 .. Array.length ours - 1 do
            if ours[i].Left > 0 then
                if brawler < 0 && oursS[i].Swings > 0 then
                    brawler <- i

                if lowest < 0 || ours[i].Left < ours[lowest].Left then
                    lowest <- i

        if brawler >= 0 then brawler else lowest

    let settle (side: Combatant array) (strength: Strength array) (damage: int array) =
        let swung = strength |> Array.map (fun s -> melee s > 0)
        let heal = healsOf side strength damage swung

        side
        |> Array.mapi (fun i c ->
            if c.Left <= 0 then
                c
            else
                { c with
                    Left = min c.Full (c.Left - damage[i] + heal[i]) |> max 0
                })

    let tick (ours: Combatant array) oursS (raid: Combatant array) raidS =
        let toOurs = Array.zeroCreate (Array.length ours)
        let toRaid = Array.zeroCreate (Array.length raid)
        let raidHeal = raidS |> Array.sumBy (fun s -> Engine.healPower * s.Heals)
        let ourDamage = towers + (oursS |> Array.sumBy shot)
        let target = targetOf raid raidS raidHeal ourDamage
        toRaid[target] <- toRaid[target] + ourDamage

        if not kite then
            for i in 0 .. Array.length ours - 1 do
                if oursS[i].Swings > 0 then
                    toOurs[i] <- toOurs[i] + Engine.attackPower * raidS[target].Swings

        let front = frontOf ours oursS
        toOurs[front] <- toOurs[front] + (raidS |> Array.sumBy shot)

        if not kite then
            for i in 0 .. Array.length raid - 1 do
                if raidS[i].Swings > 0 then
                    toRaid[i] <- toRaid[i] + Engine.attackPower * oursS[front].Swings

        settle ours oursS toOurs, settle raid raidS toRaid

    // Ours asked first: a trade that disarms both sides on one tick is lost.
    let rec fight t ours raid =
        let oursS = Array.map strengthOf ours
        let raidS = Array.map strengthOf raid

        if not (Array.exists armedWith oursS) || t >= squadTicksMost then
            { Won = false; Ticks = t }
        elif not (Array.exists armedWith raidS) then
            { Won = true; Ticks = t }
        else
            let ours, raid = tick ours oursS raid raidS
            fight (t + 1) ours raid

    if Array.length ours + Array.length raid <= squadBodiesMost then
        fight 0 ours raid
    else
        { Won = false; Ticks = 0 }

/// Whether this squad wins the raid in one room (`squadFight`, the kill
/// order's aim): what prices a Fight's squad (`fightSquadIn`).
let squadWins (view: ColonyView) (room: string) (members: BodyPart list list) (kite: bool) : bool =
    (squadFight view room members kite KillOrder).Won

/// Whether this squad can keep out of the raid's melee reach for the whole
/// fight (`docs/research/squads.md` §4.8): no member carries ATTACK, the
/// slowest walks a plain tile at least as fast as the raid's fastest melee,
/// and `safe`, the room's ground no Threat reaches, is not empty.
let kiteHolds
    (view: ColonyView)
    (safe: Set<RoomPos>)
    (room: string)
    (members: BodyPart list list)
    : bool =
    // Whole ticks a plain tile takes: a MOVE pays off two fatigue a tick, and
    // every other part makes two.
    let plainTicks (body: BodyPart list) =
        let factor = Grid.emptyFactorOf body

        if factor.MoveParts = 0 then
            System.Int32.MaxValue
        else
            max 1 ((factor.FatigueParts + factor.MoveParts - 1) / factor.MoveParts)

    let raidMelee =
        view.Hostiles
        |> List.filter (fun h -> h.Pos.Room = room && not (isKeeper h))
        |> List.map (fun h -> Engine.liveParts h.Body h.Hits)
        |> List.filter (List.contains Attack)

    not (Set.isEmpty safe)
    && not (List.isEmpty members)
    && members |> List.forall (List.contains Attack >> not)
    && (List.isEmpty raidMelee
        || (members |> List.map plainTicks |> List.max)
           <= (raidMelee |> List.map plainTicks |> List.min))

/// Whether the resident garrison's size loses the raid standing in a room
/// beside its loaded towers: what pools a Fight there.
let internal residentsLose (view: ColonyView) (room: string) : bool =
    not (
        blocksBeat
            rangerPattern.Block
            (towerDamageIn view room)
            view
            room
            view.Tuning.RangerResidentBlocks
    )

/// The squad a room's fight record latched (`FightLatch.Squad`), or None.
let private latchedSquad (view: ColonyView) (room: string) : Squad option =
    Map.tryFind room view.Fought
    |> Option.bind (fun latch -> latch.Squad)
    |> Option.bind (fun name -> squadCatalogue |> List.tryFind (fun squad -> squad.Name = name))

/// Ticks since a room's latched fight record last saw its raid, or None for a
/// room with no record, or one that latched no squad.
let private sinceFightSeen (view: ColonyView) (room: string) : int option =
    match Map.tryFind room view.Fought, latchedSquad view room with
    | Some latch, Some _ -> Some(view.Time - latch.Seen)
    | _ -> None

/// Whether the raid record keeps a room's Fight pooled: its squad latched and
/// its raid seen within `Tuning.FightHoldTicks`.
let internal fightHeld (view: ColonyView) (room: string) : bool =
    sinceFightSeen view room
    |> Option.exists (fun since -> since <= view.Tuning.FightHoldTicks)

/// Whether a room's Fight dropped less than `Tuning.FightHoldTicks` ago, so no
/// squad is cast for it: a raid that comes back as each squad lands is not
/// paid 8,650 a time.
let internal fightCooling (view: ColonyView) (room: string) : bool =
    sinceFightSeen view room
    |> Option.exists (fun since ->
        since > view.Tuning.FightHoldTicks && since < 2 * view.Tuning.FightHoldTicks)

/// Whether a raid the residents lose stands in a room for a second time inside
/// `Tuning.FightConfirmTicks`, its first on the record: what first pools its
/// Fight, and what the record latches its squad on.
let private fightConfirmed (view: ColonyView) (room: string) : bool =
    view.Hostiles |> List.exists (fun h -> h.Pos.Room = room && isRaider h)
    && Map.tryFind room view.Fought
       |> Option.exists (fun latch -> view.Time - latch.Seen <= view.Tuning.FightConfirmTicks)
    && residentsLose view room

/// The cheapest catalogue squad that wins a room's raid standing
/// (`squadWins`) and whose every body this colony's bank holds, or None.
/// Standing and never kiting: a launched squad meets the raid in contact.
let internal pricedSquad (view: ColonyView) (room: string) : Squad option =
    let castable (squad: Squad) =
        not (List.isEmpty view.Spawns)
        && squad.Members |> List.forall (fun body -> bodyCost body <= view.Bank.Capacity)

    squadCatalogue
    |> List.filter castable
    |> List.sortBy (fun squad -> List.sumBy bodyCost squad.Members)
    |> List.tryFind (fun squad -> squadWins view room squad.Members false)

/// The squad a resident room's raid is fought with, or None: the one its
/// record latched, else the one `pricedSquad` names. Pooled once its raid is
/// confirmed (`fightConfirmed`), and kept while the latched record holds it,
/// or while a holder stands by it with a raider or the tapper still there.
let private fightSquadIn (view: ColonyView) (holders: Map<string, string list>) (room: string) =
    let resident = residentRooms view
    let inRoom = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
    let raided = inRoom |> List.exists isRaider

    let held =
        Map.tryFind (taskId (Fight room)) holders |> Option.exists (List.isEmpty >> not)

    let latched = latchedSquad view room

    let pooled =
        raided && residentsLose view room && Option.isSome latched
        || fightConfirmed view room
        || fightHeld view room
        || held && (raided || inRoom |> List.exists (claimsAFlag resident))

    if Set.contains room resident && pooled then
        latched |> Option.orElse (pricedSquad view room)
    else
        None

/// Every resident room a Fight is pooled for this tick, with its squad. A
/// quiet colony with no record and no holder asks no room.
let private fights (view: ColonyView) (holders: Map<string, string list>) : Map<string, Squad> =
    let resident = residentRooms view

    let asked =
        (view.Hostiles |> List.map (fun h -> h.Pos.Room))
        @ (view.Fought |> Map.keys |> List.ofSeq)
        @ (resident
           |> Set.toList
           |> List.filter (fun room -> Map.containsKey (taskId (Fight room)) holders))
        |> List.distinct
        |> List.filter (fun room -> Set.contains room resident)

    asked
    |> List.choose (fun room ->
        fightSquadIn view holders room |> Option.map (fun squad -> room, squad))
    |> Map.ofList

/// The Planner's narrow view of the assignment table, filtered to the living:
/// `Assignments` arrives from Memory and may name a creep that died last tick,
/// the Matcher drops those silently, but `planTasks` runs first. Not filtered
/// to task kinds: a reader that wants one kind writes the key it wants and
/// asks, as `isHungry` does.
let heldTaskFacts (view: ColonyView) (assignments: Assignments) : HeldTaskFacts =
    let living = view.Creeps |> List.map (fun creep -> creep.Name, creep) |> Map.ofList

    let held =
        assignments
        |> Map.fold
            (fun facts name tid ->
                match Map.tryFind name living with
                | None -> facts
                | Some creep ->
                    { facts with
                        All = Set.add tid facts.All
                        WithThorium =
                            if creep.Thorium > 0 then
                                Set.add tid facts.WithThorium
                            else
                                facts.WithThorium
                        Holders =
                            Map.add tid (name :: HeldTaskFacts.holdersOf facts tid) facts.Holders
                    })
            HeldTaskFacts.empty

    { held with
        Fights = fights view held.Holders
    }

/// The roles one squad's bodies fill, in its cast order.
let internal squadRoles (squad: Squad) : SquadRole list =
    squad.Members |> List.choose (partsOf >> squadRoleOfParts)

/// The errand rooms a raid stands in this tick (#414): an armed hostile that is
/// not a Source Keeper, or a rival's CLAIM body. What the guard is kept there
/// to meet, and what the re-claimer's seat waits on while the guard is short.
let internal errandRoomsRaided (view: ColonyView) : Set<string> =
    let rooms = errandRooms view

    view.Hostiles
    |> List.filter (fun hostile ->
        Set.contains hostile.Pos.Room rooms
        && (isRaider hostile || claimsAFlag rooms hostile))
    |> List.map (fun hostile -> hostile.Pos.Room)
    |> Set.ofList

/// The errand rooms a rival's CLAIM body stands in this tick (#406). The flag
/// may be ours this tick, but `claimReactor` has no precondition, so a load
/// put in now burns for whoever holds the flag next. Off vision alone: no load
/// is drawn without a re-claimer resident to see.
let internal errandRoomsContested (view: ColonyView) : Set<string> =
    let rooms = errandRooms view

    view.Hostiles
    |> List.filter (claimsAFlag rooms)
    |> List.map (fun hostile -> hostile.Pos.Room)
    |> Set.ofList

/// Whether an ally is burning in this Reactor (#413): their flag on it and
/// Thorium in its store, by the Reactor's own row. No row, no vision, and
/// absence is not an ally.
let internal reactorAllyBurning (view: ColonyView) (reactorId: string) : bool =
    view.Reactors
    |> List.exists (fun reactor ->
        reactor.Id = reactorId
        && reactor.Thorium > 0
        && (match reactor.Owner with
            | ReactorOwner.Rival username -> Colony.isAlly username
            | ReactorOwner.Ours
            | ReactorOwner.Unowned -> false))

/// The errand rooms whose Reactor an ally is burning in.
let internal errandRoomsAllyBurning (view: ColonyView) : Set<string> =
    view.Errands
    |> List.filter (fun errand -> reactorAllyBurning view (fst errand.Target))
    |> List.map (fun errand -> errand.RoomName)
    |> Set.ofList

/// The errand rooms a delivery may be drawn for and poured into this tick:
/// the declared ones less the contested, and less those an ally is burning in.
let internal errandRoomsDeliverable (view: ColonyView) : Set<string> =
    Set.difference
        (errandRooms view)
        (Set.union (errandRoomsContested view) (errandRoomsAllyBurning view))

/// Whether decaying ore standing at this target is this colony's to sweep: a
/// room we own (#311), a room we declared an errand in (#354: the Reactor's
/// room has no controller, so "a room we own" made its floor belong to nobody
/// while 915 T bled on it), or a room a chain of ours crosses (#360). Written
/// once for the piles, the tombstones and the breach channel that alarms on
/// both.
let private oursToSweep (view: ColonyView) (id: string) : bool =
    let errandRooms = errandRooms view

    inARoomWeOwn view id
    || SpatialInfo.roomOf view.Spatial id
       |> Option.exists (fun room ->
           Set.contains room errandRooms
           // `view.Crossed` and not "any room in the projection": a stranger's
           // room can be in a scan set without being on a chain, and its floor
           // is not ours to walk onto. `ColonyView.transiting` admits exactly
           // two kinds out of a crossed room — a pile and a tombstone.
           || Set.contains room view.Crossed)

/// The Thorium on the ground in a room this colony sweeps, in id order (#311):
/// the dig that landed on the floor because the container was at its 2,000
/// cap. Off the projection's kind census and not off the deposits: a mine
/// whose container is destroyed mid-haul goes on dropping onto a tile the
/// deposit census would still name but the container census no longer does.
let internal ourThoriumPiles (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial (Dropped Thorium)
    |> List.filter (oursToSweep view)

/// The Thorium in a store with a clock on it — a tombstone or a ruin —
/// standing in a room this colony sweeps, in id order (#359). The engine's
/// `withdraw` takes a tombstone or a ruin for any resource (`@screeps/engine`
/// `src/game/creeps.js`), and a tombstone that decays drops its whole store
/// as piles (`processor/intents/tombstones/tick.js`), where `ourThoriumPiles`
/// picks the story up. The kind carries no resource, so the holding selects.
let internal ourThoriumTombstones (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Tombstone
    |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0 && oursToSweep view id)

/// Whether a Reactor this colony has declared has room for a whole load
/// (#354): what gates a new draw at the Storage, and the programme's only
/// regulator of supply, since the Reactor burns exactly 1 T a tick and a
/// cadence cannot meter that.
///
/// Read on the store as it stands plus every unit of ore already aboard a body
/// of ours (#362): the store alone is right for one carrier and wrong for two
/// (live at t501,501 a hauler landed 196 T it could not put down behind a
/// courier's 500, and stood on the Reactor's tile for 152 ticks). Counting all
/// ore afloat is deliberately conservative: a mine hauler walking ore home is
/// counted too, so a colony that mines and delivers sometimes defers a draw by
/// one haul cycle. Naming which body is inbound would mean reading the
/// `Assignments` map the Planner is blind to.
///
/// A Reactor we cannot see answers `false`, which leaves the load banked at
/// home. Read off `view.Reactors` and not off `SpatialInfo.Thorium`, which
/// deliberately does not carry this store: the first version asked the wrong
/// map, answered 0 for a Reactor holding 999, and the unit test agreed because
/// the fixture wrote the store where the gate looked.
let internal reactorTakesALoad (view: ColonyView) (load: int) : bool =
    let afloat = view.Creeps |> List.sumBy (fun creep -> creep.Thorium)

    view.Errands
    |> List.exists (fun errand ->
        let reactorId = fst errand.Target

        view.Reactors
        |> List.exists (fun reactor ->
            reactor.Id = reactorId
            && reactor.Thorium + afloat + load <= Engine.reactorCapacity))

/// The mineral containers of this colony's own mines, each keyed by what marks
/// the mine: the built container within range 1 of the mark in the mark's own
/// room (a `Pos` names no room, and a container on the same coordinate of an
/// outpost is not this mine's). Built alone and never a site: a site holds
/// nothing (#261). Off our deposits and extractors and never off the container
/// census, which carries every owner's. Paired with the mark because the two
/// readers ask different questions of the same join (#262).
///
/// A live mine is marked by its deposit. One whose deposit is gone is marked by
/// the extractor still standing on that tile (#421): the mod deletes an
/// exhausted deposit, and the miner's last digs stay in the container.
/// `depositIsDiggable` reads no for an extractor's id, so only the
/// Thorium-held readers see these.
let internal ourMineralContainerPairs (view: ColonyView) : (string * string) list =
    let containers =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Container)
        |> List.choose (fun id ->
            SpatialInfo.placementOf view.Spatial id |> Option.map (fun tile -> id, tile))

    let deposits = ourDeposits view

    let depositTiles =
        deposits |> List.choose (SpatialInfo.placementOf view.Spatial) |> Set.ofList

    let spentExtractors =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Extractor)
        |> List.filter (inARoomWeOwn view)
        |> List.filter (fun id ->
            SpatialInfo.placementOf view.Spatial id
            |> Option.exists (fun tile -> not (Set.contains tile depositTiles)))

    deposits @ spentExtractors
    |> List.choose (fun markId ->
        match SpatialInfo.placementOf view.Spatial markId with
        | None -> None
        | Some mark ->
            containers
            |> List.filter (fun (_, tile) ->
                tile.Room = mark.Room && range (RoomPos.pos tile) (RoomPos.pos mark) <= 1)
            // Ties by id, the way every other tie in this colony falls: a
            // mine the Layout ever seated two containers beside answers with
            // one of them and not with both.
            |> List.map fst
            |> List.sort
            |> List.tryHead
            |> Option.map (fun containerId -> markId, containerId))

let internal ourMineralContainers (view: ColonyView) : string list =
    ourMineralContainerPairs view |> List.map snd

/// Whether a deposit of ours is one the colony can actually dig this tick: it
/// still holds Thorium, and its extractor stands. Only the digger reads this
/// (#361): the carrier's load comes out of a Storage, which outlives the
/// deposit that filled it. A site is not an extractor: `harvest.js` refuses a
/// mineral with none on it.
let internal depositIsDiggable (view: ColonyView) atlas (depositId: string) =
    Map.tryFind depositId view.Spatial.Thorium |> Option.defaultValue 0 > 0
    && (Atlas.extractorOn atlas depositId).IsSome

/// The energy standing in this colony's Storages — its stock, as against
/// `ColonyView.Bank`'s spawn account. Read by the worker row's backlog term
/// (#364), which is paid out of the stock and not out of income. Folded over
/// every Storage the projection holds rather than the home room's alone: a
/// rule that assumed one would be silently wrong in the colony that first has
/// two.
let internal stockedEnergy (view: ColonyView) : int =
    view.Spatial.TargetKinds
    |> Map.fold
        (fun total id kind ->
            if kind = Structure BuiltKind.Storage then
                total + SpatialInfo.storedIn view.Spatial id
            else
                total)
        0

/// Whether more ore is still on its way to the Storage: a diggable deposit of
/// ours, or ore already standing in a mineral container. The question the last
/// load turns on.
let internal oreStillComing (view: ColonyView) atlas : bool =
    ourDeposits view |> List.exists (depositIsDiggable view atlas)
    || ourMineralContainers view
       |> List.exists (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)

/// The most ore any one Storage of this colony banks — a maximum and not a
/// sum, because the draw this feeds is one body at one store.
let internal mostOreInAStorage (view: ColonyView) : int =
    view.Spatial.TargetKinds
    |> Map.fold
        (fun most id kind ->
            if kind = Structure BuiltKind.Storage then
                max most (SpatialInfo.heldIn view.Spatial Thorium id)
            else
                most)
        0

/// ADR-0073. What the next delivery draw takes, and 0 when there is none to
/// take: a whole `Tuning.ReactorLoad` while the mine still feeds the bank, and
/// the remainder once it does not.
let internal deliveryLoad (view: ColonyView) atlas : int =
    let banked = mostOreInAStorage view

    if banked >= view.Tuning.ReactorLoad then
        view.Tuning.ReactorLoad
    elif banked > 0 && not (oreStillComing view atlas) then
        banked
    else
        0

/// Whether the ore a body holds is a delivery's — drawn for the Reactor and
/// not hauled out of a mine. Three ways to be one: the exact `ReactorLoad`,
/// this tick's own load, or, once nothing is left to dig or haul, any ore
/// aboard — a remainder taken on one tick is measured against a load of 0 on
/// the next. Read by the Planner, which pools a sink, and by nothing that
/// refuses one.
let internal carryingADelivery
    (view: ColonyView)
    (load: int)
    (stillComing: bool)
    (creep: CreepInfo)
    =
    creep.Thorium > 0
    && (creep.Thorium = view.Tuning.ReactorLoad
        || creep.Thorium = load
        || (not stillComing && not (List.isEmpty view.Errands)))

/// Whether ore is lying in a declared errand room — a pile or a tombstone
/// beside the Reactor itself.
let internal oreBesideTheReactor (view: ColonyView) : bool =
    ourThoriumTombstones view @ ourThoriumPiles view
    |> List.exists (fun id ->
        SpatialInfo.roomOf view.Spatial id
        |> Option.exists (fun room -> Set.contains room (errandRooms view)))

/// The errands there is ore for (#420): ore stands somewhere it could still
/// reach the Reactor — a Storage or Terminal of ours, a diggable deposit, a
/// mineral container, a body's hold, the floor beside the Reactor, or the
/// Reactor's own store while it burns for us or an ally. What the
/// garrison and the re-claimer are kept for; the declaration outlives it, so
/// the ore's return reopens the errand, and a flag taken off an empty Reactor
/// costs nothing (`claimReactor` has no precondition). Colony-wide rather than
/// per errand: the ore is the colony's, and any Reactor may take it.
let internal fuelledErrands (view: ColonyView) : Errand list =
    let held kinds =
        view.Spatial.TargetKinds
        |> Map.exists (fun id kind ->
            List.exists (sameKind kind) kinds
            && SpatialInfo.heldIn view.Spatial Thorium id > 0)

    let extractorTiles =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Extractor)
        |> List.choose (SpatialInfo.placementOf view.Spatial)
        |> Set.ofList

    // `depositIsDiggable` without the Atlas the Planner does not hold: the
    // extractor's tile is the deposit's.
    let diggable depositId =
        Map.tryFind depositId view.Spatial.Thorium |> Option.defaultValue 0 > 0
        && SpatialInfo.placementOf view.Spatial depositId
           |> Option.exists (fun tile -> Set.contains tile extractorTiles)

    let fuelled =
        held [ Structure BuiltKind.Storage; Structure BuiltKind.Terminal ]
        || ourDeposits view |> List.exists diggable
        || ourMineralContainers view
           |> List.exists (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)
        || view.Creeps |> List.exists (fun creep -> creep.Thorium > 0)
        || oreBesideTheReactor view
        || view.Reactors
           |> List.exists (fun reactor ->
               reactor.Thorium > 0
               && (match reactor.Owner with
                   | ReactorOwner.Ours -> true
                   | ReactorOwner.Rival username -> Colony.isAlly username
                   | ReactorOwner.Unowned -> false))

    if fuelled then view.Errands else []

/// Whether the season's delivery programme has all of its current ground
/// facts (#319, narrowed by #361): a Storage holding a load, and a resident
/// CLAIM body at a declared Reactor. No remembered switch — each fact closes
/// the row when it disappears. A diggable mine is deliberately not a
/// condition: Thorium never regenerates, so every deposit ends mined out with
/// its ore in a Storage, and the tick the mine ran dry this row closed and the
/// Reactor burned down with 7,226 T banked and a 7,989-tick streak standing
/// (#361).
let internal courierProgrammeOpen (view: ColonyView) atlas =
    let hasLoad = deliveryLoad view atlas > 0
    let deliverable = errandRoomsDeliverable view

    let hasResidentReclaimer =
        view.Creeps
        |> List.exists (fun creep ->
            partCount creep.Body BodyPart.Claim > 0
            && (Atlas.creepTile atlas creep.Name
                |> Option.exists (fun tile -> Set.contains tile.Room deliverable)))

    view.Bank.Capacity >= bodyCost courierPattern.Block
    && hasLoad
    && hasResidentReclaimer
