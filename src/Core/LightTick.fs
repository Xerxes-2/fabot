/// The light tick: a quiet tick that replays the last full tick's decision
/// instead of deciding. Pure: what the last full tick left, and what this
/// tick sees of `Game` without building a World, in; the Intents, or the
/// reason a full tick has to run instead, out. `LightTick` and not `Light`,
/// which `BodyClass` already spells.
module Fabot.Core.LightTick

open Fabot.Core.Types

/// One creep of ours as the glance reads it: where it stands, its hits, and —
/// for a creep on its room's border ring — the step off it (`inward`).
type GlanceCreep =
    {
        Tile: RoomPos
        Hits: int
        Inward: Direction option
        /// Its HEAL parts with hits left: what it heals a tick.
        Heals: int
        /// Its ATTACK and RANGED_ATTACK parts with hits left: a fighter's
        /// weapons (#552).
        Strikes: int
    }

/// One hostile creep in a visible room, asked `HostileInfo`'s questions
/// (`glanceOf`).
type GlanceHostile =
    {
        /// What a replayed aim names (`LastFull.Aims`).
        Id: string
        Tile: RoomPos
        Owner: string
        Armed: bool
        Weapon: int option
        Breaks: bool
        /// Whether it heals: a raid's healer at home is a fight too.
        Heals: bool
        Gait: Gait
        /// Hits left: what a mass attack's damage is capped by (#537).
        Hits: int
    }

/// A hostile as the glance reads it, beside its room's other hostiles
/// (`side`), the full tick's questions asked the full tick's way: `Armed` is
/// `HostileInfo.armed`, which a healer is not; `Weapon`, `weaponRange`;
/// `Breaks`, `breaks`; `Gait`, `gait`. A wrong "armed" costs a full tick, a
/// wrong "unarmed" a shot undecided.
let glanceOf (side: HostileInfo list) (hostile: HostileInfo) : GlanceHostile =
    {
        Id = hostile.Id
        Tile = hostile.Pos
        Owner = hostile.Owner
        Armed = HostileInfo.armed side hostile
        Weapon = HostileInfo.weaponRange side hostile
        Breaks = HostileInfo.breaks side hostile
        Heals = HostileInfo.heals side hostile
        Gait = HostileInfo.gait side hostile
        Hits = hostile.Hits
    }

/// What a tick reads off `Game` directly, before any World is built: our
/// creeps, every hostile in sight, our structures' tiles (for proximity), and
/// our controllers by room as (level, safe mode on).
type Glance =
    {
        Creeps: Map<string, GlanceCreep>
        Hostiles: GlanceHostile list
        Structures: RoomPos list
        Controllers: Map<string, int * bool>
        /// The CPU bucket is low: a second light tick may follow the first
        /// (`Main.loop`).
        Lean: bool
        /// The power creeps not ours to strike anywhere
        /// (`Colony.strikesAnywhere`) standing in the rooms of
        /// the last full tick's volleys (`LastFull.Volleys`, #537): shields a
        /// mass attack may not reach, which move. Read in no other room.
        PowerCreeps: RoomPos list
        /// Of the ids the last full tick's acts name (`lookups`), the ones
        /// still standing: a store, a pile, a site, a structure, a source, a
        /// controller, the Reactor, an observer. Looked up only on a tick
        /// that may be light.
        Objects: Set<string>
    }

/// Something of ours a hostile can strike from where it stands (`now`) or
/// after one step: a body stepping into its weapon's range is a new fact.
[<RequireQualifiedAccess>]
type Reached =
    /// A creep of ours, by name.
    | Creep of name: string * now: bool
    /// A structure of ours, by tile.
    | Structure of tile: RoomPos * now: bool

/// One colony's part of the last plan (#549): what its last decide issued and
/// laid, kept apart from the other colonies' so a forced tick that decides
/// some colonies carries the rest whole.
type Plan =
    {
        /// The bodies the colony held when it decided: whom the plan speaks
        /// for.
        Creeps: Set<string>
        /// Each of them's tile at the start of that tick: where a drop, the
        /// one act with no target, was decided to land.
        Standing: Map<string, RoomPos>
        /// Each walking creep's step plan.
        Steps: Map<string, RoomPos * RoomPos>
        /// The acts a light tick issues again (`replayed`), in the decide's
        /// own order, its planned next acts (`Decision.Next`) among them.
        Acts: Intent list
        /// Whether a structure of the colony's fought (`fights`): an act no
        /// replay repeats.
        Fought: bool
        /// Each fighter's strike at its target, in reach or not
        /// (`Emitter.aims`), which a light tick issues again while the target
        /// is in sight (#528): a swing lands the tick a body steps beside it.
        Aims: Intent list
        /// Each aiming ranger's guns and shields (`Emitter.volleys`, #537).
        Volleys: Map<string, Volley>
    }

/// What the last full tick left for the ticks after it: every colony's plan,
/// and the facts the plans were made against.
type LastFull =
    {
        /// By home: a colony's plan from the last tick it decided on.
        Plans: Map<string, Plan>
        Hits: Map<string, int>
        Controllers: Map<string, int * bool>
        Creeps: Set<string>
        /// Who could hurt what of ours as the plans were made (`reached`):
        /// a threat the plans already saw, by hostile id.
        Hurting: Set<string * Reached>
        /// The colonies a cold reset left undecided (`resetSplit`): the tick
        /// after it is full and decides them.
        Deferred: Set<string>
    }

/// Why a light tick hands over to a full one, and whose replay it breaks
/// (`deciders`).
[<RequireQualifiedAccess>]
type LightForce =
    /// A raider (a live weapon or heal, no keeper) in a room we own: a home
    /// is decided every tick it stands there, by its own colony and by each
    /// colony with a fighter standing there (#552: the mother's residents).
    | Home of room: string * owner: string
    /// A body that can strike something of ours, now or after a step, that
    /// it could not as the plans were made, and that is not fighting a creep
    /// of ours that strikes it.
    | Hostile of room: string * owner: string
    /// A colony's structure fought: a tower's act no replay repeats.
    | Fought of home: string
    /// Creeps of ours born or gone since the plans.
    | CreepsChanged of creeps: string list
    /// A creep of ours on its room's border ring with no ground to step onto
    /// (`inward`): the engine carries a body that ends a tick there into the
    /// neighbour, so one that crossed on the full tick and stood still on the
    /// light one would be carried straight back.
    | OnBorder of creep: string
    /// A creep of ours that lost hits to a body no plan strikes, or to
    /// nothing in sight.
    | HitsLost of creep: string
    | ControllerChanged of room: string
    /// The reset tick before left colonies undecided (`resetSplit`).
    | Deferred

/// The reason on the CPU line: the rule's word first, then where it fired —
/// the room and the hostile's owner, the colony, or the creep — since a reason
/// read back after the hostile has left has nothing else to be traced by.
let tag (reason: LightForce) : string =
    match reason with
    | LightForce.Home(room, owner) -> $"home {room} {owner}"
    | LightForce.Hostile(room, owner) -> $"hostile {room} {owner}"
    | LightForce.Fought home -> $"fought {home}"
    | LightForce.CreepsChanged _ -> "creeps"
    | LightForce.OnBorder creep -> $"border {creep}"
    | LightForce.HitsLost creep -> $"hurt {creep}"
    | LightForce.ControllerChanged room -> $"controller {room}"
    | LightForce.Deferred -> "deferred"

/// The step off the border ring into the room, for a creep standing on it: a
/// body on the ring at the start of a tick has always just crossed in (the
/// engine carries one that ends a tick there), so the room it stands in is the
/// one it is walking into. Straight in, else either inward diagonal, onto
/// ground the terrain lets it stand on; None off the ring, or with all three
/// walled. A body on the tile it steps to costs the crossing one tick: the
/// engine carries it back, and the next full tick sends it again.
let inward (walkable: Pos -> bool) (tile: Pos) : Direction option =
    let last = Engine.roomSide - 1

    let candidates =
        if tile.X = 0 then [ Right; TopRight; BottomRight ]
        elif tile.X = last then [ Left; TopLeft; BottomLeft ]
        elif tile.Y = 0 then [ Bottom; BottomRight; BottomLeft ]
        elif tile.Y = last then [ Top; TopRight; TopLeft ]
        else []

    let stepTo direction =
        let dx, dy =
            match direction with
            | Top -> 0, -1
            | TopRight -> 1, -1
            | Right -> 1, 0
            | BottomRight -> 1, 1
            | Bottom -> 0, 1
            | BottomLeft -> -1, 1
            | Left -> -1, 0
            | TopLeft -> -1, -1

        { X = tile.X + dx; Y = tile.Y + dy }

    candidates
    |> List.tryFind (fun direction ->
        let next = stepTo direction
        not (Seam.onRing next) && walkable next)

/// What a replayed act needs still standing in the glance besides its actor.
[<RequireQualifiedAccess>]
type private Stands =
    /// An object by id (`Glance.Objects`).
    | Object of id: string
    /// A hostile creep by id, while the glance still sees it (`Glance.Hostiles`).
    | Hostile of id: string
    /// A creep of ours, by name.
    | Ours of name: string
    /// No target but the ground: the tile the act was decided on.
    | Tile

/// The acts a light tick issues again (#529), by the creep that acts — none
/// for a look, whose actor is the observer it names — and what must still
/// stand for it. Range, store and cooldown are the engine's to check, and a
/// refused act costs no CPU. Every case named, so an Intent added later is a
/// compile error here and not a silent "not replayed".
let private replayed (intent: Intent) : (string option * Stands) option =
    match intent with
    | HarvestSource(creep, target)
    | UpgradeController(creep, target)
    | ReserveController(creep, target)
    | AttackController(creep, target)
    | BuildSite(creep, target)
    | RepairStructure(creep, target)
    | DismantleStructure(creep, target)
    | TransferEnergyToStructure(creep, target, _)
    | WithdrawFromStore(creep, target, _, None)
    | PickupPile(creep, target)
    | AttackStructure(creep, target)
    | ClaimReactor(creep, target) -> Some(Some creep, Stands.Object target)
    | AttackCreep(creep, target)
    | RangedAttackCreep(creep, target) -> Some(Some creep, Stands.Hostile target)
    | HealCreep(creep, target)
    | RangedHealCreep(creep, target) -> Some(Some creep, Stands.Ours target)
    | DropEnergy creep -> Some(Some creep, Stands.Tile)
    // Vision lands the tick after the look: one issued on the full tick
    // lands on the light tick, which reads nothing.
    | ObserveRoom(observer, _) -> Some(None, Stands.Object observer)
    // It hits whoever stands near now, an ally walked in among them: an
    // aiming ranger's is chosen again off the glance (`intents`).
    | RangedMassAttack _
    // A named amount: a draw that landed would be drawn twice — under the
    // terminal's fee reserve, or past the courier's marked load (#378).
    | WithdrawFromStore(_, _, _, Some _)
    // As a terminal's send names one.
    | SendFromTerminal _
    // The step plan's, one tile on.
    | MoveCreep _
    // A glyph priced as an intent, for nothing.
    | SayCreep _
    // Written once.
    | SignController _
    // It takes the room, which forces the next tick full, or the engine
    // refuses it for a GCL that another try does not change.
    | ClaimController _
    // The planner's, off a bank or a site count the first one moved.
    | SpawnCreep _
    | PlaceConstructionSite _
    | PlaceSpawnSite _
    // A state, already set.
    | SetRampartPublic _
    // A structure's fight: the next tick is full (`fights`).
    | ActivateSafeMode _
    | FireTower _
    | HealWithTower _
    | RepairWithTower _ -> None

/// The full tick's acts worth replaying, its planned next acts after them,
/// each superseding a full-tick act of its creep on its channel: a refiller's
/// next pour replaces the pour it made.
let private actsOf (next: Intent list) (intents: Intent list) : Intent list =
    let kept = intents |> List.filter (replayed >> Option.isSome)
    let planned = next |> List.filter (replayed >> Option.isSome)

    (kept
     |> List.filter (fun act -> planned |> List.forall (IntentPlan.compatible act)))
    @ planned

/// Whether an intent is a structure's fight — a tower's shot, heal or repair
/// of a struck rampart, or safe mode raised: its colony decides the next
/// tick, never replays it. A creep's own shot or heal is not: it is replayed
/// while its target stands (live 2026-09-30, 40 of 83 full ticks were forced
/// by a harassing ranger's shot at a target already gone). Every case named,
/// so a new shot is a compile error here and not a silent "no".
let private fights (intent: Intent) : bool =
    match intent with
    | FireTower _
    | HealWithTower _
    | RepairWithTower _
    | ActivateSafeMode _ -> true
    | AttackCreep _
    | AttackStructure _
    | RangedAttackCreep _
    | RangedMassAttack _
    | HealCreep _
    | RangedHealCreep _
    | SpawnCreep _
    | PlaceConstructionSite _
    | PlaceSpawnSite _
    | HarvestSource _
    | TransferEnergyToStructure _
    | WithdrawFromStore _
    | BuildSite _
    | RepairStructure _
    | DismantleStructure _
    | UpgradeController _
    | ReserveController _
    | AttackController _
    | ClaimController _
    | ClaimReactor _
    | PickupPile _
    | DropEnergy _
    | SignController _
    | MoveCreep _
    | SayCreep _
    | SetRampartPublic _
    | ObserveRoom _
    | SendFromTerminal _ -> false

/// One colony's plan, off the glance taken at the start of the tick it
/// decided on: the bodies it held, their step plans, every intent it issued
/// and its planned **next** acts (`Decision.Next`) — a refiller's pour into
/// the next hungry member it stands beside, a hauler's act on the tile it
/// steps onto, which a replay issues and the full tick never — and its aims
/// and volleys. It outlives the tick, so every collection in it is built by
/// `Fresh` (#401).
let plan
    (glance: Glance)
    (creeps: Set<string>)
    (steps: Map<string, RoomPos * RoomPos>)
    (next: Intent list)
    (intents: Intent list)
    (aims: Intent list)
    (volleys: Map<string, Volley>)
    : Plan =
    {
        Creeps = Fresh.setOfSeq creeps
        Standing =
            glance.Creeps
            |> Map.toSeq
            |> Seq.filter (fun (name, _) -> Set.contains name creeps)
            |> Seq.map (fun (name, creep) -> name, creep.Tile)
            |> Fresh.mapOfSeq
        Steps = steps |> Map.toSeq |> Fresh.mapOfSeq
        Acts = actsOf next intents
        Fought = intents |> List.exists fights
        Aims = aims
        Volleys = volleys |> Map.toSeq |> Fresh.mapOfSeq
    }

/// Whether a hostile strikes a tile at `range` now (`Some true`), after one
/// step (`Some false`), or not by then (`None`): its weapon's range, one step
/// more unless it is as good as rooted (`Gait.stationary`, #521).
let private strikes (hostile: GlanceHostile) (range: int) (tile: RoomPos) : bool option =
    match RoomPos.range hostile.Tile tile with
    | Some r when r <= range -> Some true
    | Some r when r <= range + 1 && not (Gait.stationary hostile.Gait) -> Some false
    | _ -> None

/// Whether a hostile's weapon strikes a tile now or after one step.
let private reaches (hostile: GlanceHostile) (tile: RoomPos) : bool =
    hostile.Weapon
    |> Option.exists (fun range -> Option.isSome (strikes hostile range tile))

/// What of ours a hostile can strike now or after one step, each creep and
/// structure apart: a creep or a structure for a body with a weapon, a
/// structure beside one that dismantles or claims. A scout, a hauler or a
/// healer near our creeps reaches nothing (#462; user, 2026-10-04: a
/// Trepidimous worker beside a harassing ranger held every tick full).
let private reached (now: Glance) (hostile: GlanceHostile) : Reached list =
    let onStructures =
        match hostile.Weapon with
        | Some range -> Some range
        | None when hostile.Breaks -> Some Engine.meleeRange
        | None -> None

    [
        match hostile.Weapon with
        | Some range ->
            for KeyValue(name, creep) in now.Creeps do
                match strikes hostile range creep.Tile with
                | Some soon -> yield Reached.Creep(name, soon)
                | None -> ()
        | None -> ()

        match onStructures with
        | Some range ->
            for tile in now.Structures do
                match strikes hostile range tile with
                | Some soon -> yield Reached.Structure(tile, soon)
                | None -> ()
        | None -> ()
    ]

let private within (now: Glance) (hostile: GlanceHostile) : bool =
    not (List.isEmpty (reached now hostile))

/// Whether a body can act on nothing of ours: an ally fights beside us, and a
/// body in a room under our safe mode is held off (live t931,466, W17S25's
/// safe mode held 100 full ticks of 95 ms).
let private harmless (now: Glance) (hostile: GlanceHostile) =
    Colony.isAlly hostile.Owner
    || Map.tryFind hostile.Tile.Room now.Controllers |> Option.exists snd

/// Who can hurt us this tick, read once off the glance: every hostile that is
/// not `harmless` with something of ours it can hurt in its reach (`within`),
/// its live parts read (`glanceOf`), so a disarmed cripple hurts nothing.
let hurting (now: Glance) : GlanceHostile list =
    now.Hostiles
    |> List.filter (fun hostile -> not (harmless now hostile) && within now hostile)

/// A plan less the bodies `claimed` by a plan before it.
let private without (claimed: Set<string>) (plan: Plan) : Plan =
    if Set.isEmpty (Set.intersect claimed plan.Creeps) then
        plan
    else
        let mine name = not (Set.contains name claimed)

        let acting act =
            match replayed act with
            | Some(Some name, _) -> mine name
            | _ -> true

        let striking aim =
            match aim with
            | AttackCreep(name, _)
            | RangedAttackCreep(name, _)
            | RangedMassAttack name -> mine name
            | _ -> true

        {
            Creeps = Fresh.setOfSeq (plan.Creeps - claimed)
            Standing = plan.Standing |> Map.toSeq |> Seq.filter (fst >> mine) |> Fresh.mapOfSeq
            Steps = plan.Steps |> Map.toSeq |> Seq.filter (fst >> mine) |> Fresh.mapOfSeq
            Acts = plan.Acts |> List.filter acting
            Fought = plan.Fought
            Aims = plan.Aims |> List.filter striking
            Volleys = plan.Volleys |> Map.toSeq |> Seq.filter (fst >> mine) |> Fresh.mapOfSeq
        }

/// The record of the plans by home, each body claimed by the first plan that
/// holds it — a forced tick's fresh plans before the ones it carried, so a body
/// a deciding colony adopted is that colony's alone (#549) — and the facts
/// they were made against, off this tick's glance. `deferred`: the colonies a
/// cold reset left undecided.
let ofPlans (glance: Glance) (deferred: Set<string>) (plans: (string * Plan) list) : LastFull =
    let kept, _ =
        ((Fresh.mapOfSeq Seq.empty, Set.empty), plans)
        ||> List.fold (fun (kept, claimed) (home, plan) ->
            Map.add home (without claimed plan) kept, Set.union claimed plan.Creeps)

    {
        Plans = kept |> Map.toSeq |> Fresh.mapOfSeq
        Hits =
            glance.Creeps
            |> Map.toSeq
            |> Seq.map (fun (name, creep) -> name, creep.Hits)
            |> Fresh.mapOfSeq
        Controllers = glance.Controllers
        Creeps = glance.Creeps |> Map.keys |> Fresh.setOfSeq
        Hurting =
            glance.Hostiles
            |> Seq.filter (harmless glance >> not)
            |> Seq.collect (fun hostile ->
                reached glance hostile |> Seq.map (fun r -> hostile.Id, r))
            |> Fresh.setOfSeq
        Deferred = Fresh.setOfSeq deferred
    }

/// One colony's full tick, holding every creep the glance sees, as a
/// single-colony shard reads it: its step plans and every intent it issued.
let rec lastFull
    (glance: Glance)
    (steps: Map<string, RoomPos * RoomPos>)
    (intents: Intent list)
    : LastFull =
    lastFullWith glance steps [] intents

/// `lastFull` with its planned next acts (`Decision.Next`, `plan`).
and lastFullWith
    (glance: Glance)
    (steps: Map<string, RoomPos * RoomPos>)
    (next: Intent list)
    (intents: Intent list)
    : LastFull =
    let creeps = glance.Creeps |> Map.keys |> Set.ofSeq

    ofPlans glance Set.empty [ "", plan glance creeps steps next intents [] Map.empty ]

/// The record with only these colonies' plans.
let only (homes: Set<string>) (last: LastFull) : LastFull =
    { last with
        Plans = last.Plans |> Map.filter (fun home _ -> Set.contains home homes)
    }

/// Every plan as one: the bodies are disjoint (`ofPlans`), so nothing is
/// overwritten. Read within the tick.
let private merged (last: LastFull) : Plan =
    match Map.toList last.Plans with
    | [ _, plan ] -> plan
    | plans ->
        let plans = List.map snd plans

        let mapOf part =
            plans |> Seq.collect (part >> Map.toSeq) |> Map.ofSeq

        {
            Creeps = plans |> List.map _.Creeps |> Set.unionMany
            Standing = mapOf _.Standing
            Steps = mapOf _.Steps
            Acts = plans |> List.collect _.Acts
            Fought = plans |> List.exists _.Fought
            Aims = plans |> List.collect _.Aims
            Volleys = mapOf _.Volleys
        }

/// The rooms the plans' volleys stand in (`Glance.PowerCreeps`).
let volleyRooms (last: LastFull) : string list =
    last.Plans
    |> Map.values
    |> Seq.collect (fun plan -> plan.Volleys |> Map.values |> Seq.map (fun volley -> volley.Room))
    |> Seq.distinct
    |> List.ofSeq

/// The hostiles the plans strike at, each with the creeps of ours that aim or
/// shoot at it and the range they strike from.
let private strikers (last: LastFull) : Map<string, (string * int) list> =
    last.Plans
    |> Map.values
    |> Seq.collect (fun plan -> Seq.append plan.Aims plan.Acts)
    |> Seq.choose (function
        | AttackCreep(creep, target) -> Some(target, (creep, Engine.meleeRange))
        | RangedAttackCreep(creep, target) -> Some(target, (creep, Engine.rangedRange))
        | _ -> None)
    |> Seq.groupBy fst
    |> Seq.map (fun (target, pairs) -> target, pairs |> Seq.map snd |> Seq.distinct |> List.ofSeq)
    |> Map.ofSeq

/// What the plans heal each creep of ours with a tick: every heal aimed at
/// it whose healer still stands, at its live HEAL parts.
let private healing (last: LastFull) (now: Glance) : Map<string, int> =
    let power healer perPart =
        Map.tryFind healer now.Creeps
        |> Option.map (fun creep -> creep.Heals * perPart)
        |> Option.defaultValue 0

    last.Plans
    |> Map.values
    |> Seq.collect _.Acts
    |> Seq.choose (function
        | HealCreep(healer, target) -> Some(target, power healer Engine.healPower)
        | RangedHealCreep(healer, target) -> Some(target, power healer Engine.rangedHealPower)
        | _ -> None)
    |> Seq.groupBy fst
    |> Seq.map (fun (target, heals) -> target, heals |> Seq.sumBy snd)
    |> Map.ofSeq

/// The colonies a cold reset decides on its own tick and the ones it leaves
/// to the next (#488), each in the order given: every colony with a threat in
/// a room it projects — an armed body of neither a keeper nor an ally, or one
/// the near rule reads within reach of ours — or with a room of its own the
/// world danger log holds danger in (`danger`, `Observe.Danger.standing`),
/// seen or not: a shut room watched by vision alone is still its colony's;
/// then the rest in order until `share` of them is decided. Never none: a
/// lone colony decides.
let resetSplit
    (share: float)
    (now: Glance)
    (danger: Set<string>)
    (colonies: (string * Set<string>) list)
    : string list * string list =
    let threats =
        now.Hostiles
        |> List.filter (fun hostile ->
            not (HostileInfo.isKeeperOwner hostile.Owner)
            && not (Colony.isAlly hostile.Owner)
            && (hostile.Armed || within now hostile))
        |> List.map (fun hostile -> hostile.Tile.Room)
        |> Set.ofList
        |> Set.union danger

    let threatened, quiet =
        colonies
        |> List.partition (fun (_, rooms) -> not (Set.isEmpty (Set.intersect rooms threats)))

    let half = max 1 (int (ceil (float (List.length colonies) * share)))
    let spare = min (List.length quiet) (max 0 (half - List.length threatened))

    threatened @ List.take spare quiet |> List.map fst, List.skip spare quiet |> List.map fst

/// Every fact the plans did not account for, in the ADR's order (#549): a
/// replay is wrong only where the world moved off what a plan was made
/// against — a raider in a home room, every tick (a home is never a tick
/// late); a body that can strike something of ours now or after one step
/// that it could not as the plans were made, unless that is a creep of ours
/// that strikes it (a cripple whose live parts hurt nothing, or one out of
/// reach, reaches nothing); a creep of ours that lost hits, unless every body
/// that could have struck it is one a striker of ours aims at and reaches,
/// and the loss is within the healing the plans give it; a creep born or
/// gone; one on the border ring with no step in; a controller changed — or
/// where a plan holds an act no replay repeats (a structure's fight), or a
/// colony has no plan (a cold reset's deferred).
let forcing (last: LastFull) (now: Glance) : LightForce list =
    let strikers = strikers last

    // The creeps of ours striking the hostile from where they stand: in its
    // room, and with `reach`, within their own range of it.
    let fighting (hostile: GlanceHostile) (reach: bool) (by: string -> bool) =
        Map.tryFind hostile.Id strikers
        |> Option.exists (
            List.exists (fun (name, range) ->
                by name
                && Map.tryFind name now.Creeps
                   |> Option.exists (fun creep ->
                       match RoomPos.range creep.Tile hostile.Tile with
                       | Some r -> not reach || r <= range
                       | None -> false))
        )

    let hostiles = now.Hostiles |> List.filter (harmless now >> not)

    let deferred =
        if Set.isEmpty last.Deferred then
            []
        else
            [ LightForce.Deferred ]

    // A healer counts: one alone at home between its pair's swaps on the exit
    // healed back what the light ticks' silent towers let it (live t981,05x,
    // W17S25). So does a dismantler: one working a structure while the towers
    // are drained fights nothing that would force a tick.
    let home =
        hostiles
        |> List.filter (fun hostile ->
            (hostile.Armed || hostile.Heals || hostile.Breaks)
            && not (HostileInfo.isKeeperOwner hostile.Owner)
            && Map.containsKey hostile.Tile.Room now.Controllers)
        |> List.distinctBy (fun hostile -> hostile.Tile.Room)
        |> List.map (fun hostile -> LightForce.Home(hostile.Tile.Room, hostile.Owner))

    let unforeseen =
        hostiles
        |> List.filter (fun hostile ->
            reached now hostile
            |> List.exists (fun r ->
                not (Set.contains (hostile.Id, r) last.Hurting)
                && match r with
                   | Reached.Creep(name, _) -> not (fighting hostile false ((=) name))
                   | Reached.Structure _ -> not (fighting hostile false (fun _ -> true))))
        |> List.distinctBy (fun hostile -> hostile.Tile.Room)
        |> List.map (fun hostile -> LightForce.Hostile(hostile.Tile.Room, hostile.Owner))

    let fought =
        last.Plans
        |> Map.toList
        |> List.filter (fun (_, plan) -> plan.Fought)
        |> List.map (fst >> LightForce.Fought)

    let creepsChanged =
        let standing = now.Creeps |> Map.keys |> Set.ofSeq
        let changed = (standing - last.Creeps) + (last.Creeps - standing)

        if Set.isEmpty changed then
            []
        else
            [ LightForce.CreepsChanged(Set.toList changed) ]

    let onBorder =
        now.Creeps
        |> Map.toList
        |> List.filter (fun (_, creep) ->
            Seam.onRing (RoomPos.pos creep.Tile) && Option.isNone creep.Inward)
        |> List.map (fst >> LightForce.OnBorder)

    // Who could have struck the creep where it stands: a body whose weapon
    // reaches it now or after a step. None means a cause out of sight (a
    // tower, a body gone). Excused only where the plans fight every one of
    // them from in reach and heal what it lost (#533's out-damaged reading).
    let hitsLost =
        let healed = healing last now

        now.Creeps
        |> Map.toList
        |> List.filter (fun (name, creep) ->
            match Map.tryFind name last.Hits with
            | Some before when creep.Hits < before ->
                match hostiles |> List.filter (fun hostile -> reaches hostile creep.Tile) with
                | [] -> true
                | near ->
                    near
                    |> List.exists (fun hostile -> not (fighting hostile true (fun _ -> true)))
                    || before - creep.Hits > (Map.tryFind name healed |> Option.defaultValue 0)
            | _ -> false)
        |> List.map (fst >> LightForce.HitsLost)

    let controllerChanged =
        Seq.append (Map.keys last.Controllers) (Map.keys now.Controllers)
        |> Seq.distinct
        |> Seq.sort
        |> Seq.filter (fun room ->
            Map.tryFind room last.Controllers <> Map.tryFind room now.Controllers)
        |> Seq.map LightForce.ControllerChanged
        |> List.ofSeq

    List.concat
        [
            deferred
            home
            unforeseen
            fought
            creepsChanged
            onBorder
            hitsLost
            controllerChanged
        ]

/// The first fact that makes replaying the plans wrong (`forcing`); `None`
/// when the light tick may run.
let forced (last: LastFull) (now: Glance) : LightForce option = forcing last now |> List.tryHead

/// The colonies a forced tick decides (#549), of `colonies` (each home with
/// the rooms it answers for): each fact's own — a raided home's own colony,
/// a hostile's or a controller's room's colonies and the holders of our
/// bodies standing there, a creep's holder, a fighting structure's colony,
/// the deferred — and every colony with no plan. A fact no colony owns is
/// everyone's. `holders`: each creep's colony this tick; a body gone is read
/// off the plan that held it.
let deciders
    (colonies: (string * Set<string>) list)
    (holders: Map<string, string>)
    (last: LastFull)
    (now: Glance)
    (reasons: LightForce list)
    : Set<string> =
    let homes = colonies |> List.map fst |> Set.ofList

    let answering room =
        colonies
        |> List.filter (fun (_, rooms) -> Set.contains room rooms)
        |> List.map fst
        |> Set.ofList

    let standingWhere (stands: GlanceCreep -> bool) room =
        now.Creeps
        |> Map.toSeq
        |> Seq.filter (fun (_, creep) -> creep.Tile.Room = room && stands creep)
        |> Seq.choose (fun (name, _) -> Map.tryFind name holders)
        |> Set.ofSeq

    let standingIn = standingWhere (fun _ -> true)

    // A body with a live weapon or HEAL: a resident, never a worker passing.
    let fightingIn = standingWhere (fun creep -> creep.Strikes > 0 || creep.Heals > 0)

    let holderOf name =
        match Map.tryFind name holders with
        | Some home -> Set.singleton home
        | None ->
            last.Plans
            |> Map.tryFindKey (fun _ plan -> Set.contains name plan.Creeps)
            |> Option.toList
            |> Set.ofList

    let scope reason =
        let own =
            match reason with
            | LightForce.Home(room, _) when Set.contains room homes ->
                Set.add room (fightingIn room)
            | LightForce.Home(room, _) -> answering room
            | LightForce.Hostile(room, _) -> answering room + standingIn room
            | LightForce.Fought home -> Set.singleton home
            | LightForce.CreepsChanged names -> names |> List.map holderOf |> Set.unionMany
            | LightForce.OnBorder name -> holderOf name
            | LightForce.HitsLost name -> holderOf name
            | LightForce.ControllerChanged room -> answering room
            | LightForce.Deferred -> last.Deferred

        if Set.isEmpty own then homes else own

    let planless =
        homes |> Set.filter (fun home -> not (Map.containsKey home last.Plans))

    reasons
    |> List.map scope
    |> Set.unionMany
    |> Set.union planless
    |> Set.intersect homes

/// The ids the last full tick's acts name, for the glance to look up
/// (`Glance.Objects`): each act's object and each look's observer. A
/// creep's or a hostile's stands in the glance already.
let lookups (last: LastFull) : string list =
    last.Plans
    |> Map.values
    |> Seq.collect _.Acts
    |> Seq.choose (fun act ->
        match replayed act with
        | Some(_, Stands.Object id) -> Some id
        | _ -> None)
    |> Seq.distinct
    |> List.ofSeq

/// The light tick's intents (#529), in creep-name order and the looks after
/// them: every act the plans issued or planned (`Plan.Acts`) whose
/// actor and target still stand in the glance, wherever the body stands —
/// the engine checks range, store and cooldown, and a refused act costs no
/// CPU; a drop only from the tile it was decided on. A fighter strikes at
/// its aim again the same way (#528), on a channel no act of its holds. A
/// creep standing on its step plan's first tile steps toward the second.
let intents (record: LastFull) (now: Glance) : Intent list =
    let last = merged record
    let seen = now.Hostiles |> List.map (fun hostile -> hostile.Id) |> Set.ofList

    let stands (actor: string option) (target: Stands) =
        let body = actor |> Option.bind (fun name -> Map.tryFind name now.Creeps)

        (Option.isNone actor || Option.isSome body)
        && match target with
           | Stands.Object id -> Set.contains id now.Objects
           | Stands.Hostile id -> Set.contains id seen
           | Stands.Ours name -> Map.containsKey name now.Creeps
           | Stands.Tile ->
               match actor, body with
               | Some name, Some body -> Map.tryFind name last.Standing = Some body.Tile
               | _ -> false

    let standing =
        last.Acts
        |> List.choose (fun act ->
            match replayed act with
            | Some(actor, target) when stands actor target -> Some(actor, act)
            | _ -> None)

    let acts =
        standing
        |> List.choose (fun (actor, act) -> actor |> Option.map (fun creep -> creep, act))
        |> List.groupBy fst
        |> List.map (fun (creep, pairs) -> creep, List.map snd pairs)
        |> Map.ofList

    let aims =
        last.Aims
        |> List.choose (fun aim ->
            match aim with
            | AttackCreep(creep, target)
            | RangedAttackCreep(creep, target) when Set.contains target seen -> Some(creep, aim)
            | _ -> None)
        |> List.groupBy fst
        |> List.map (fun (creep, pairs) -> creep, List.map snd pairs)
        |> Map.ofList

    let tiles =
        now.Hostiles |> List.map (fun hostile -> hostile.Id, hostile.Tile) |> Map.ofList

    let hits =
        now.Hostiles |> List.map (fun hostile -> hostile.Id, hostile.Hits) |> Map.ofList

    // The hostile an aim names.
    let aimedAt (aim: Intent) =
        match aim with
        | AttackCreep(_, target)
        | RangedAttackCreep(_, target) -> Some target
        | _ -> None

    // The aim's target's tile and the body's reach, for an aim it holds.
    let aimed (aim: Intent) =
        match aim with
        | AttackCreep(_, target) ->
            Map.tryFind target tiles |> Option.map (fun tile -> tile, Engine.meleeRange)
        | RangedAttackCreep(_, target) ->
            Map.tryFind target tiles |> Option.map (fun tile -> tile, Engine.rangedRange)
        | _ -> None

    let strikes (act: Intent) =
        match act with
        | AttackCreep _
        | RangedAttackCreep _
        | RangedMassAttack _ -> true
        | _ -> false

    [
        for KeyValue(name, creep) in now.Creeps do
            let inward =
                match creep.Inward with
                | Some direction when Seam.onRing (RoomPos.pos creep.Tile) -> Some direction
                | _ -> None

            match inward with
            | Some direction -> yield MoveCreep(name, direction)
            | None -> ()

            // The full tick's strike choice again off the glance (#535, #537):
            // the first aim in reach, else the head, or the mass attack where
            // it deals more (`Emitter.pickStrike`) — never with a body within
            // three that is none of its aims. It replaces the full tick's own
            // strike, which named the target in reach where the bodies stood
            // then.
            let volley =
                Map.tryFind name last.Volleys
                |> Option.filter (fun volley -> volley.Room = creep.Tile.Room)

            let guns = volley |> Option.map (fun volley -> volley.Guns) |> Option.defaultValue 0

            let pick =
                Map.tryFind name aims
                |> Option.bind (fun held ->
                    let targets = held |> List.choose aimedAt |> Set.ofList

                    let mass =
                        volley
                        |> Option.bind (fun volley ->
                            now.Hostiles
                            |> List.map (fun h -> h.Tile, h.Hits, Set.contains h.Id targets)
                            |> Decide.Emitter.massDamage
                                volley.Guns
                                creep.Tile
                                (Set.union
                                    volley.Shields
                                    (RoomPos.inRoom volley.Room (Set.ofList now.PowerCreeps))))

                    let dealt aim =
                        aimedAt aim
                        |> Option.bind (fun target -> Map.tryFind target hits)
                        |> Option.map (fun left -> min left (guns * Engine.rangedAttackPower))
                        |> Option.defaultValue 0

                    held
                    |> Decide.Emitter.pickStrike
                        (fun aim ->
                            aimed aim
                            |> Option.bind (fun (tile, reach) ->
                                RoomPos.range creep.Tile tile |> Option.map (fun r -> r <= reach))
                            |> Option.defaultValue false)
                        dealt
                        mass
                    |> Option.map (function
                        | Decide.Emitter.At(aim, reaches) -> aim, reaches
                        | Decide.Emitter.Mass -> RangedMassAttack name, true))

            // The full tick's own priority (`IntentPlan.arbitrate`): its
            // chosen acts, the aim on a channel none of them holds, then a
            // replayed heal on what is left — a swing beats a pre-heal.
            let reflexes, chosen =
                Map.tryFind name acts
                |> Option.defaultValue []
                |> List.filter (fun act -> Option.isNone pick || not (strikes act))
                |> List.partition IntentPlan.isReflex

            let issued =
                match pick with
                | Some(aim, reaches) when chosen |> List.forall (IntentPlan.compatible aim) ->
                    Some(aim, reaches)
                | _ -> None

            yield!
                IntentPlan.arbitrate (chosen @ (issued |> Option.map fst |> Option.toList)) reflexes

            // A melee body steps onto the target it swings at, as on the full
            // tick (`Emitter.glueStep`), in place of its step plan's step:
            // never a squad's cast, whose movement is its Formation's, nor
            // off our structure's tile (a rampart), and never onto a tile
            // another melee body of theirs swings at.
            let glued =
                match issued with
                | Some(AttackCreep _ as aim, true) when
                    Option.isNone (Decide.Bodies.squadRoleByName name)
                    ->
                    let melee =
                        now.Hostiles
                        |> List.filter (fun h -> h.Armed && h.Weapon = Some Engine.meleeRange)
                        |> List.map (fun h -> h.Tile)

                    let onRampart = List.contains creep.Tile now.Structures

                    aimed aim
                    |> Option.bind (fun (tile, _) ->
                        Decide.Emitter.glueStep creep.Tile tile onRampart melee)
                | _ -> None

            match inward, glued with
            | None, Some direction -> yield MoveCreep(name, direction)
            | _ ->
                match Map.tryFind name last.Steps with
                | Some(first, second) when first = creep.Tile ->
                    match directionTo (RoomPos.pos first) (RoomPos.pos second) with
                    | Some direction -> yield MoveCreep(name, direction)
                    | None -> ()
                | _ -> ()

        yield!
            standing
            |> List.choose (fun (actor, act) -> if Option.isNone actor then Some act else None)
    ]

/// A forced tick's replay of the colonies it does not decide (#549): their
/// plans exactly as a light tick replays them (`intents`), for the bodies
/// each holds this tick (`holders`) — a body a deciding colony holds is that
/// colony's to decide, and one whose plan another colony made stands.
let replay
    (homes: Set<string>)
    (holders: Map<string, string>)
    (last: LastFull)
    (now: Glance)
    : Intent list =
    let held name =
        Map.tryFind name holders |> Option.exists (fun home -> Set.contains home homes)

    intents
        (only homes last)
        { now with
            Creeps = now.Creeps |> Map.filter (fun name _ -> held name)
        }
