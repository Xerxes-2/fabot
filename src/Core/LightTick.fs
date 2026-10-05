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
        Gait: Gait
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
        Gait = HostileInfo.gait side hostile
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
        /// The CPU bucket is low (2026-10-05: twice emptied by full ticks a
        /// raid forced): the near rule then reads only the rooms we own a
        /// structure in, so a hostile loitering in an outpost no longer
        /// holds every tick full while the bucket drains.
        Lean: bool
        /// Of the ids the last full tick's acts name (`lookups`), the ones
        /// still standing: a store, a pile, a site, a structure, a source, a
        /// controller, the Reactor, an observer. Looked up only on a tick
        /// that may be light.
        Objects: Set<string>
    }

/// What the last full tick left for the light tick after it.
type LastFull =
    {
        /// Each creep's tile at the start of the full tick: where a drop, the
        /// one act with no target, was decided to land.
        Standing: Map<string, RoomPos>
        /// Each walking creep's step plan, every colony's merged.
        Steps: Map<string, RoomPos * RoomPos>
        /// The full tick's acts a light tick issues again (`replayed`), in its
        /// own order, its planned next acts (`Decision.Next`) among them.
        Acts: Intent list
        /// Whether a structure of ours fought on the full tick (`fights`).
        Fought: bool
        /// The creeps of ours that swung or shot on the full tick: a hostile
        /// still near one of them is the near rule's whatever it carries, a
        /// swing being one-shot and the fight not over.
        Shooters: Set<string>
        Hits: Map<string, int>
        Controllers: Map<string, int * bool>
        Creeps: Set<string>
        /// The colonies a cold reset left undecided (`resetSplit`): the tick
        /// after it is full and decides them.
        Deferred: Set<string>
        /// The harassment rooms no colony works for anything else (`loose`):
        /// a light tick passes the near rule over in one while every body of
        /// ours standing there is one of `Rangers`.
        Loose: Set<string>
        /// The harass rangers (`harassRangers`).
        Rangers: Set<string>
        /// Each fighter's strike at its target, in reach or not
        /// (`Emitter.aims`), which a light tick issues again while the target
        /// is in sight (#528): a swing lands the tick a body steps beside it.
        Aims: Intent list
    }

/// One colony's rooms as `loose` reads them: the rooms it works (its scan set
/// less the ones it only crosses), every room any colony declares as an
/// outpost, the harassment rooms it casts, its evacuating rooms, and its
/// bodies' Tasks.
type RoomRoles =
    {
        Worked: Set<string>
        Mined: Set<string>
        Harassed: Set<string>
        Evacuating: Set<string>
        Assignments: Assignments
    }

/// The harassment rooms no colony works for anything else (#520): not a home,
/// a declared outpost (shut or not, and so a child's outpost a mother watches,
/// #517), a resident or assault room, nor an evacuating room. Only a harass
/// ranger is meant to stand there, beside enemy bodies by its job; `forced`
/// checks that no other body of ours does.
let loose (colonies: RoomRoles list) : Set<string> =
    let kept =
        colonies
        |> List.map (fun roles ->
            Set.unionMany [ roles.Worked - roles.Harassed; roles.Mined; roles.Evacuating ])
        |> Set.unionMany

    // Kept on `LastFull`, so built by `Fresh` (#401).
    (colonies |> List.map (fun roles -> roles.Harassed) |> Set.unionMany) - kept
    |> Fresh.setOfSeq

/// The bodies holding a harassment room's Guard: the harass rangers.
let harassRangers (colonies: RoomRoles list) : Set<string> =
    colonies
    |> Seq.collect (fun roles ->
        let guards = roles.Harassed |> Set.map (Guard >> Decide.Facts.taskId)

        roles.Assignments
        |> Map.toSeq
        |> Seq.filter (fun (_, task) -> Set.contains task guards)
        |> Seq.map fst)
    |> Fresh.setOfSeq

/// The loose rooms that still hold no body of ours but a harass ranger.
let private looseNow (last: LastFull) (now: Glance) : Set<string> =
    if Set.isEmpty last.Loose then
        last.Loose
    else
        let mixed =
            now.Creeps
            |> Map.toSeq
            |> Seq.filter (fun (name, _) -> not (Set.contains name last.Rangers))
            |> Seq.map (fun (_, creep) -> creep.Tile.Room)
            |> Set.ofSeq

        last.Loose - mixed

/// The first reason a light tick hands over to a full one.
[<RequireQualifiedAccess>]
type LightForce =
    | ArmedHostile of room: string * owner: string
    | HostileNear of room: string * owner: string
    | Fought
    | CreepsChanged
    /// A creep of ours on its room's border ring with no ground to step onto
    /// (`inward`): the engine carries a body that ends a tick there into the
    /// neighbour, so one that crossed on the full tick and stood still on the
    /// light one would be carried straight back.
    | OnBorder of creep: string
    | HitsLost of creep: string
    | ControllerChanged of room: string
    /// The reset tick before left colonies undecided (`resetSplit`).
    | Deferred

/// The reason on the CPU line: the rule's word first (`armed` is a home room's
/// armed hostile, `near` one within reach anywhere but a `loose` room), then where it fired —
/// the room and the hostile's owner, or the creep — since a reason read back
/// after the hostile has left has nothing else to be traced by.
let tag (reason: LightForce) : string =
    match reason with
    | LightForce.ArmedHostile(room, owner) -> $"armed {room} {owner}"
    | LightForce.HostileNear(room, owner) -> $"near {room} {owner}"
    | LightForce.Fought -> "fought"
    | LightForce.CreepsChanged -> "creeps"
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
/// of a struck rampart, or safe mode raised: a tick that holds one is followed by a full tick, never a
/// light one. A creep's own shot or heal is not: a body it could still reach
/// is within `nearRange` and forces the tick full on that rule, and one that
/// died or ran leaves nothing to decide (live 2026-09-30, 40 of 83 full ticks
/// were forced by a harassing ranger's shot at a target already gone). Every
/// case named, so a new shot is a compile error here and not a silent "no".
let private fights (intent: Intent) : bool =
    match intent with
    | FireTower _
    | HealWithTower _
    | RepairWithTower _
    | ActivateSafeMode _ -> true
    | AttackCreep _
    | AttackStructure _
    | RangedAttackCreep _
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

/// The full tick's record, off the glance taken at its start, its step plans
/// and every intent it issued. It outlives the tick, so every collection in
/// it is built by `Fresh` (#401).
let rec lastFull
    (glance: Glance)
    (steps: Map<string, RoomPos * RoomPos>)
    (intents: Intent list)
    : LastFull =
    lastFullWith glance steps [] intents

/// `lastFull` with the full tick's planned **next** acts beside its own
/// (`Decision.Next`): a refiller's pour into the next hungry member it stands
/// beside, and a hauler's act on the tile it steps onto. The light tick
/// issues them, the full tick never.
and lastFullWith
    (glance: Glance)
    (steps: Map<string, RoomPos * RoomPos>)
    (next: Intent list)
    (intents: Intent list)
    : LastFull =
    {
        Standing =
            glance.Creeps
            |> Map.toSeq
            |> Seq.map (fun (name, creep) -> name, creep.Tile)
            |> Fresh.mapOfSeq
        Steps = steps
        Acts = actsOf next intents
        Fought = intents |> List.exists fights
        Shooters =
            intents
            |> List.choose (function
                | AttackCreep(creep, _)
                | RangedAttackCreep(creep, _) -> Some creep
                | _ -> None)
            |> Fresh.setOfSeq
        Hits =
            glance.Creeps
            |> Map.toSeq
            |> Seq.map (fun (name, creep) -> name, creep.Hits)
            |> Fresh.mapOfSeq
        Controllers = glance.Controllers
        Creeps = glance.Creeps |> Map.keys |> Fresh.setOfSeq
        Deferred = Fresh.setOfSeq Seq.empty
        Loose = Fresh.setOfSeq Seq.empty
        Rangers = Fresh.setOfSeq Seq.empty
        Aims = []
    }

/// How close a hostile of any kind may come to a creep or structure of ours
/// before a tick is decided.
let private nearRange = Engine.rangedRange + 2

/// How close a hostile may come before a tick is decided: a rooted one
/// (`Gait.stationary`) reaches a step past its weapon's range, a swing being
/// adjacent for a body that only dismantles or claims (#521), as Threat's
/// Reach reads it; any other `nearRange`.
let private reachOf (hostile: GlanceHostile) : int =
    if Gait.stationary hostile.Gait then
        defaultArg hostile.Weapon Engine.meleeRange + Engine.meleeRange
    else
        nearRange

/// The tiles of ours the near rule measures from: our creeps' and our
/// structures', apart, for a body may threaten one and not the other.
type private Ours =
    {
        CreepTiles: RoomPos list
        StructureTiles: RoomPos list
    }

let private oursOf (now: Glance) : Ours =
    {
        CreepTiles = now.Creeps |> Map.values |> Seq.map (fun creep -> creep.Tile) |> List.ofSeq
        StructureTiles = now.Structures
    }

/// The near rule: a hostile within its reach (`reachOf`) of something of ours
/// it can hurt — a creep or a structure for a body that hurts creeps, a
/// structure for one that can dismantle or claim. A scout, a hauler or a
/// healer near our creeps changes no decision (#462; user, 2026-10-04: a
/// Trepidimous worker beside a harassing ranger held every tick full).
let private within (ours: Ours) (hostile: GlanceHostile) : bool =
    let reach = reachOf hostile

    let near (tiles: RoomPos list) =
        tiles
        |> List.exists (fun tile ->
            match RoomPos.range hostile.Tile tile with
            | Some r -> r <= reach
            | None -> false)

    (Option.isSome hostile.Weapon
     && (near ours.CreepTiles || near ours.StructureTiles))
    || (hostile.Breaks && near ours.StructureTiles)

/// The colonies a cold reset decides on its own tick and the ones it leaves
/// to the next (#488), each in the order given: every colony with a threat in
/// a room it projects — an armed body of neither a keeper nor an ally, or one
/// the near rule reads within reach of ours — then the rest in order until
/// `share` of them is decided. Never none: a lone colony decides.
let resetSplit
    (share: float)
    (now: Glance)
    (colonies: (string * Set<string>) list)
    : string list * string list =
    let ours = oursOf now

    let threats =
        now.Hostiles
        |> List.filter (fun hostile ->
            not (HostileInfo.isKeeperOwner hostile.Owner)
            && not (Colony.isAlly hostile.Owner)
            && (hostile.Armed || within ours hostile))
        |> List.map (fun hostile -> hostile.Tile.Room)
        |> Set.ofList

    let threatened, quiet =
        colonies
        |> List.partition (fun (_, rooms) -> not (Set.isEmpty (Set.intersect rooms threats)))

    let half = max 1 (int (ceil (float (List.length colonies) * share)))
    let spare = min (List.length quiet) (max 0 (half - List.length threatened))

    threatened @ List.take spare quiet |> List.map fst, List.skip spare quiet |> List.map fst

/// The first fact that makes replaying the last full tick wrong, in the
/// ADR's order; `None` when the light tick may run.
let forced (last: LastFull) (now: Glance) : LightForce option =
    // An ally fights beside us, and a body in a room under our safe mode can
    // act on nothing of ours: neither is a reason to decide (live t931,466,
    // W17S25's safe mode held 100 full ticks of 95 ms).
    let hostiles =
        now.Hostiles
        |> List.filter (fun hostile ->
            not (Colony.isAlly hostile.Owner)
            && not (Map.tryFind hostile.Tile.Room now.Controllers |> Option.exists snd))

    // Only in a room where we own a structure: an armed body standing off in
    // a remote or harassment room is the near rule's to answer (#461), and in
    // a `loose` room the lost-hits rule's.
    let armed =
        let homes = now.Structures |> List.map (fun tile -> tile.Room) |> Set.ofList

        hostiles
        |> List.tryFind (fun hostile ->
            hostile.Armed
            && not (HostileInfo.isKeeperOwner hostile.Owner)
            && Set.contains hostile.Tile.Room homes)
        |> Option.map (fun hostile -> LightForce.ArmedHostile(hostile.Tile.Room, hostile.Owner))

    let near () =
        let ours = oursOf now

        // The full tick's shooters, where they stand now: any hostile near
        // one is the fight going on.
        let shooting =
            now.Creeps
            |> Map.toList
            |> List.filter (fun (name, _) -> Set.contains name last.Shooters)
            |> List.map (fun (_, creep) -> creep.Tile)

        // Our shooters' reach, not the hostile's: a rooted body is still
        // within their shot.
        let besideAShooter (hostile: GlanceHostile) =
            shooting
            |> List.exists (fun tile ->
                match RoomPos.range hostile.Tile tile with
                | Some r -> r <= nearRange
                | None -> false)

        let loose = looseNow last now
        let homes = now.Structures |> List.map (fun tile -> tile.Room) |> Set.ofList

        hostiles
        |> List.tryFind (fun hostile ->
            not (Set.contains hostile.Tile.Room loose)
            && (not now.Lean || Set.contains hostile.Tile.Room homes)
            && (within ours hostile || besideAShooter hostile))
        |> Option.map (fun hostile -> LightForce.HostileNear(hostile.Tile.Room, hostile.Owner))

    let fought () =
        if last.Fought then Some LightForce.Fought else None

    let creepsChanged () =
        if (now.Creeps |> Map.keys |> Set.ofSeq) <> last.Creeps then
            Some LightForce.CreepsChanged
        else
            None

    let onBorder () =
        now.Creeps
        |> Map.tryPick (fun name creep ->
            if Seam.onRing (RoomPos.pos creep.Tile) && Option.isNone creep.Inward then
                Some(LightForce.OnBorder name)
            else
                None)

    let hitsLost () =
        now.Creeps
        |> Map.tryPick (fun name creep ->
            match Map.tryFind name last.Hits with
            | Some before when creep.Hits < before -> Some(LightForce.HitsLost name)
            | _ -> None)

    let controllerChanged () =
        Seq.append (Map.keys last.Controllers) (Map.keys now.Controllers)
        |> Seq.distinct
        |> Seq.sort
        |> Seq.tryFind (fun room ->
            Map.tryFind room last.Controllers <> Map.tryFind room now.Controllers)
        |> Option.map LightForce.ControllerChanged

    let deferred =
        if Set.isEmpty last.Deferred then
            None
        else
            Some LightForce.Deferred

    deferred
    |> Option.orElse armed
    |> Option.orElseWith near
    |> Option.orElseWith fought
    |> Option.orElseWith creepsChanged
    |> Option.orElseWith onBorder
    |> Option.orElseWith hitsLost
    |> Option.orElseWith controllerChanged

/// The ids the last full tick's acts name, for the glance to look up
/// (`Glance.Objects`): each act's object and each look's observer. A
/// creep's or a hostile's stands in the glance already.
let lookups (last: LastFull) : string list =
    last.Acts
    |> List.choose (fun act ->
        match replayed act with
        | Some(_, Stands.Object id) -> Some id
        | _ -> None)
    |> List.distinct

/// The light tick's intents (#529), in creep-name order and the looks after
/// them: every act the full tick issued or planned (`LastFull.Acts`) whose
/// actor and target still stand in the glance, wherever the body stands —
/// the engine checks range, store and cooldown, and a refused act costs no
/// CPU; a drop only from the tile it was decided on. A fighter strikes at
/// its aim again the same way (#528), on a channel no act of its holds. A
/// creep standing on its step plan's first tile steps toward the second.
let intents (last: LastFull) (now: Glance) : Intent list =
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
        | RangedAttackCreep _ -> true
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

            // The full tick's target choice again off the glance (#535): the
            // first aim in reach, else the head (`Emitter.pickStrike`). It
            // replaces the full tick's own strike, which named the target in
            // reach where the bodies stood then.
            let pick =
                Map.tryFind name aims
                |> Option.bind (
                    Decide.Emitter.pickStrike (fun aim ->
                        aimed aim
                        |> Option.bind (fun (tile, reach) ->
                            RoomPos.range creep.Tile tile |> Option.map (fun r -> r <= reach))
                        |> Option.defaultValue false)
                )

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
