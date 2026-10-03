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

/// One hostile creep in a visible room. `Armed` is any Attack, RangedAttack
/// or Heal part in its body; `Hurts`, an Attack or RangedAttack part with hits
/// left (what can hurt a creep); `Breaks`, a Work or Claim part with hits left
/// (what can take down a structure or a controller).
type GlanceHostile =
    {
        Tile: RoomPos
        Owner: string
        Armed: bool
        Hurts: bool
        Breaks: bool
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
    }

/// What the last full tick left for the light tick after it.
type LastFull =
    {
        /// Each creep's tile at the start of the full tick: where its work
        /// intents were decided from.
        Standing: Map<string, RoomPos>
        /// Each walking creep's step plan, every colony's merged.
        Steps: Map<string, RoomPos * RoomPos>
        /// The full tick's repeatable intents and standing draws (`workOf`),
        /// in its own order.
        Work: Intent list
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
    }

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
/// armed hostile, `near` one within reach anywhere), then where it fired —
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

/// The repeatable work intents, by the creep that acts: a light tick issues
/// them again from the tile they were decided on. Every case named, so an
/// Intent added later is a compile error here and not a silent "not work".
let private repeatableActor (intent: Intent) : string option =
    match intent with
    | HarvestSource(creep, _)
    | UpgradeController(creep, _)
    | ReserveController(creep, _)
    | AttackController(creep, _)
    | BuildSite(creep, _)
    | RepairStructure(creep, _)
    | DismantleStructure(creep, _) -> Some creep
    | SpawnCreep _
    | PlaceConstructionSite _
    | PlaceSpawnSite _
    | TransferEnergyToStructure _
    | WithdrawFromStore _
    | ClaimController _
    | ClaimReactor _
    | PickupPile _
    | DropEnergy _
    | SignController _
    | AttackCreep _
    // One-shot (#487), as a swing at a creep is.
    | AttackStructure _
    | HealCreep _
    | RangedHealCreep _
    | RangedAttackCreep _
    | MoveCreep _
    | SayCreep _
    | ActivateSafeMode _
    | FireTower _
    | HealWithTower _
    | RepairWithTower _
    | SetRampartPublic _
    // One-shot (#484): a light tick looks nowhere, and the next full tick
    // aims again.
    | ObserveRoom _
    | SendFromTerminal _ -> None

/// The actor of a kept work intent: a repeatable one's, or a standing
/// upgrader's draw's. `lastFull` keeps no other withdraw.
let private workActor (intent: Intent) : string option =
    match intent with
    | WithdrawFromStore(creep, _, _, _) -> Some creep
    // The full tick's next pour into the refill cluster (`lastFull`'s `next`).
    | TransferEnergyToStructure(creep, _, _) -> Some creep
    | _ -> repeatableActor intent

/// The full tick's work worth replaying: the repeatable intents, and a
/// withdraw issued beside an upgrade by the same creep — the Emitter pairs
/// the two for a standing body beside its buffer alone (#498), and the
/// same-tile rule keeps it within the buffer's reach.
let private workOf (intents: Intent list) : Intent list =
    let upgrading =
        intents
        |> List.choose (function
            | UpgradeController(creep, _) -> Some creep
            | _ -> None)
        |> Set.ofList

    intents
    |> List.filter (fun intent ->
        match intent with
        | WithdrawFromStore(creep, _, _, _) -> Set.contains creep upgrading
        | _ -> Option.isSome (repeatableActor intent))

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

/// `lastFull` with the full tick's planned **next** acts beside its own: a
/// refiller's pour into the next hungry member it stands beside, which the
/// light tick issues once (`Decision.Next`), the full tick never.
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
        Work = workOf intents @ next
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
    }

/// A Source Keeper's owner, as the engine spells it.
let private keeper = "Source Keeper"

/// How close a hostile of any kind may come to a creep or structure of ours
/// before a tick is decided.
let private nearRange = Engine.rangedRange + 2

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

/// The near rule: a hostile within `nearRange` of something of ours it can
/// hurt — a creep or a structure for a body that hurts creeps, a structure
/// for one that can dismantle or claim. A scout, a hauler or a healer near
/// our creeps changes no decision (#462; user, 2026-10-04: a Trepidimous
/// worker beside a harassing ranger held every tick full).
let private within (ours: Ours) (hostile: GlanceHostile) : bool =
    let near (tiles: RoomPos list) =
        tiles
        |> List.exists (fun tile ->
            match RoomPos.range hostile.Tile tile with
            | Some r -> r <= nearRange
            | None -> false)

    (hostile.Hurts && (near ours.CreepTiles || near ours.StructureTiles))
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
            hostile.Owner <> keeper
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
    // a remote or harassment room is the near rule's to answer (#461).
    let armed =
        let homes = now.Structures |> List.map (fun tile -> tile.Room) |> Set.ofList

        hostiles
        |> List.tryFind (fun hostile ->
            hostile.Armed && hostile.Owner <> keeper && Set.contains hostile.Tile.Room homes)
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

        let besideAShooter (hostile: GlanceHostile) =
            shooting
            |> List.exists (fun tile ->
                match RoomPos.range hostile.Tile tile with
                | Some r -> r <= nearRange
                | None -> false)

        hostiles
        |> List.tryFind (fun hostile -> within ours hostile || besideAShooter hostile)
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

/// The light tick's intents, in creep-name order: a creep standing where it
/// stood at the full tick re-issues that tick's repeatable work, and a creep
/// standing on its step plan's first tile steps toward the second. Nothing
/// else, ever: a one-shot act waits for the next full tick.
let intents (last: LastFull) (now: Glance) : Intent list =
    let work =
        last.Work
        |> List.choose (fun intent -> workActor intent |> Option.map (fun creep -> creep, intent))
        |> List.groupBy fst
        |> List.map (fun (creep, pairs) -> creep, List.map snd pairs)
        |> Map.ofList

    [
        for KeyValue(name, creep) in now.Creeps do
            match creep.Inward with
            | Some direction when Seam.onRing (RoomPos.pos creep.Tile) ->
                yield MoveCreep(name, direction)
            | _ -> ()

            if Map.tryFind name last.Standing = Some creep.Tile then
                yield! Map.tryFind name work |> Option.defaultValue []

            match Map.tryFind name last.Steps with
            | Some(first, second) when first = creep.Tile ->
                match directionTo (RoomPos.pos first) (RoomPos.pos second) with
                | Some direction -> yield MoveCreep(name, direction)
                | None -> ()
            | _ -> ()
    ]
