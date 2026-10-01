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
/// or Heal part in its body.
type GlanceHostile =
    {
        Tile: RoomPos
        Owner: string
        Armed: bool
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
        /// The full tick's repeatable intents, in its own order.
        Work: Intent list
        /// Whether a structure of ours fought on the full tick (`fights`).
        Fought: bool
        Hits: Map<string, int>
        Controllers: Map<string, int * bool>
        Creeps: Set<string>
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

/// The reason on the CPU line: the rule's word first, then where it fired —
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
    | BuildSite(creep, _)
    | RepairStructure(creep, _)
    | DismantleStructure(creep, _) -> Some creep
    | SpawnCreep _
    | PlaceConstructionSite _
    | TransferEnergyToStructure _
    | WithdrawFromStore _
    | ClaimController _
    | ClaimReactor _
    | PickupPile _
    | SignController _
    | AttackCreep _
    | HealCreep _
    | RangedHealCreep _
    | RangedAttackCreep _
    | MoveCreep _
    | SayCreep _
    | ActivateSafeMode _
    | FireTower _
    | HealWithTower _
    | SendFromTerminal _ -> None

/// Whether an intent is a structure's fight — a tower's shot or heal, or safe
/// mode raised: a tick that holds one is followed by a full tick, never a
/// light one. A creep's own shot or heal is not: a body it could still reach
/// is within `nearRange` and forces the tick full on that rule, and one that
/// died or ran leaves nothing to decide (live 2026-09-30, 40 of 83 full ticks
/// were forced by a harassing ranger's shot at a target already gone). Every
/// case named, so a new shot is a compile error here and not a silent "no".
let private fights (intent: Intent) : bool =
    match intent with
    | FireTower _
    | HealWithTower _
    | ActivateSafeMode _ -> true
    | AttackCreep _
    | RangedAttackCreep _
    | HealCreep _
    | RangedHealCreep _
    | SpawnCreep _
    | PlaceConstructionSite _
    | HarvestSource _
    | TransferEnergyToStructure _
    | WithdrawFromStore _
    | BuildSite _
    | RepairStructure _
    | DismantleStructure _
    | UpgradeController _
    | ReserveController _
    | ClaimController _
    | ClaimReactor _
    | PickupPile _
    | SignController _
    | MoveCreep _
    | SayCreep _
    | SendFromTerminal _ -> false

/// The full tick's record, off the glance taken at its start, its step plans
/// and every intent it issued. It outlives the tick, so every collection in
/// it is built by `Fresh` (#401).
let lastFull
    (glance: Glance)
    (steps: Map<string, RoomPos * RoomPos>)
    (intents: Intent list)
    : LastFull =
    {
        Standing =
            glance.Creeps
            |> Map.toSeq
            |> Seq.map (fun (name, creep) -> name, creep.Tile)
            |> Fresh.mapOfSeq
        Steps = steps
        Work = intents |> List.filter (repeatableActor >> Option.isSome)
        Fought = intents |> List.exists fights
        Hits =
            glance.Creeps
            |> Map.toSeq
            |> Seq.map (fun (name, creep) -> name, creep.Hits)
            |> Fresh.mapOfSeq
        Controllers = glance.Controllers
        Creeps = glance.Creeps |> Map.keys |> Fresh.setOfSeq
    }

/// A Source Keeper's owner, as the engine spells it.
let private keeper = "Source Keeper"

/// How close a hostile of any kind may come to a creep or structure of ours
/// before a tick is decided.
let private nearRange = Engine.rangedRange + 2

/// The first fact that makes replaying the last full tick wrong, in the
/// ADR's order; `None` when the light tick may run.
let forced (last: LastFull) (now: Glance) : LightForce option =
    let armed =
        now.Hostiles
        |> List.tryFind (fun hostile -> hostile.Armed && hostile.Owner <> keeper)
        |> Option.map (fun hostile -> LightForce.ArmedHostile(hostile.Tile.Room, hostile.Owner))

    let near () =
        let ours =
            Seq.append
                (now.Creeps |> Map.values |> Seq.map (fun creep -> creep.Tile))
                now.Structures
            |> List.ofSeq

        now.Hostiles
        |> List.tryFind (fun hostile ->
            ours
            |> List.exists (fun tile ->
                match RoomPos.range hostile.Tile tile with
                | Some r -> r <= nearRange
                | None -> false))
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

    armed
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
        |> List.choose (fun intent ->
            repeatableActor intent |> Option.map (fun creep -> creep, intent))
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
