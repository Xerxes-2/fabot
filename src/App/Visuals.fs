/// The per-creep debug overlay (#638): what the heap already holds of each
/// drawn creep — its Task, its newest Verdict, the acts the last full tick
/// planned for it and a fresh refusal — written above it, with a line to
/// each act's target and its step plan, ground and chase track. Drawn only
/// for the creeps the draw list names, by name or by room; nothing is read
/// or decided for it.
module Fabot.Visuals

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core
open Fabot.Core.Types

// The harness's fake `Game` has no `RoomVisual`.
[<Emit("typeof RoomVisual === 'function'")>]
let private canDraw: bool = jsNative

[<Emit("new RoomVisual($0)")>]
let private visualOf (_room: string) : obj = jsNative

/// What an act names: an object by id, or a creep of ours by name.
type private Target =
    | Id of string
    | Ours of string

/// A creep's act as the overlay words it, and what it names; none for an
/// act no body issues, a step or a say.
let private actOf (intent: Intent) : (string * string * Target option) option =
    match intent with
    | HarvestSource(creep, target) -> Some(creep, "dig", Some(Id target))
    | TransferEnergyToStructure(creep, target, _) -> Some(creep, "fill", Some(Id target))
    | WithdrawFromStore(creep, target, _, _) -> Some(creep, "draw", Some(Id target))
    | BuildSite(creep, target) -> Some(creep, "build", Some(Id target))
    | RepairStructure(creep, target) -> Some(creep, "repair", Some(Id target))
    | DismantleStructure(creep, target) -> Some(creep, "dismantle", Some(Id target))
    | UpgradeController(creep, target) -> Some(creep, "upgrade", Some(Id target))
    | ReserveController(creep, target) -> Some(creep, "reserve", Some(Id target))
    | AttackController(creep, target) -> Some(creep, "unreserve", Some(Id target))
    | ClaimController(creep, target)
    | ClaimReactor(creep, target) -> Some(creep, "claim", Some(Id target))
    | PickupPile(creep, target) -> Some(creep, "pickup", Some(Id target))
    | AttackCreep(creep, target)
    | AttackStructure(creep, target) -> Some(creep, "hit", Some(Id target))
    | RangedAttackCreep(creep, target)
    | RangedAttackStructure(creep, target) -> Some(creep, "shoot", Some(Id target))
    | HealCreep(creep, target)
    | RangedHealCreep(creep, target) -> Some(creep, "heal", Some(Ours target))
    | RangedMassAttack creep -> Some(creep, "mass", None)
    | DropEnergy creep -> Some(creep, "drop", None)
    | SignController(creep, target, _) -> Some(creep, "sign", Some(Id target))
    | SpawnCreep _
    | PlaceConstructionSite _
    | PlaceSpawnSite _
    | MoveCreep _
    | SayCreep _
    | ActivateSafeMode _
    | FireTower _
    | HealWithTower _
    | RepairWithTower _
    | DestroyStructure _
    | RemoveSite _
    | SetRampartPublic _
    | ObserveRoom _
    | RunReaction _
    | SendFromTerminal _ -> None

/// An id cut to its last four characters: enough to tell two apart on
/// screen.
let private shortId (id: string) =
    if id.Length > 8 then
        "…" + id.Substring(id.Length - 4)
    else
        id

/// A Task id with its object id cut short (`Facts.taskId`'s
/// `kind:target[:resource]`); a room name stays whole.
let private shortTask (task: string) =
    match task.Split [| ':' |] |> List.ofArray with
    | kind :: target :: rest -> String.concat ":" (kind :: shortId target :: rest)
    | _ -> task

/// What an act's target is, off the object itself, or its id cut short
/// when it is gone or out of sight.
let private labelOf (target: Target) (seen: obj) =
    if isNull seen then
        match target with
        | Id id -> shortId id
        | Ours name -> name
    elif not (isNull seen?structureType) then
        string seen?structureType
    elif not (isNull seen?body) then
        if unbox<bool> seen?my then
            string seen?name
        else
            string seen?owner?username
    elif not (isNull seen?resourceType) then
        "pile"
    elif not (isNull seen?deathTime) then
        "tomb"
    elif not (isNull seen?ticksToRegeneration) then
        "source"
    else
        "object"

let private resolve (target: Target) : obj =
    match target with
    | Id id -> Game.getObjectById id
    | Ours name -> objectItem<obj> Game.creeps name

let private isTaskChannel (verdict: Verdict) =
    match verdict with
    | Verdict.Grounded _
    | Verdict.Yielded _
    | Verdict.Rerouted _
    | Verdict.Stalled _
    | Verdict.Scoring _ -> false
    | _ -> true

let private textStyle =
    createObj
        [
            "font" ==> 0.35
            "color" ==> "#ffffff"
            "stroke" ==> "#000000"
            "strokeWidth" ==> 0.06
            "opacity" ==> 0.9
        ]

let private targetStyle =
    createObj
        [
            "color" ==> "#ffee58"
            "width" ==> 0.06
            "opacity" ==> 0.6
            "lineStyle" ==> "dashed"
        ]

let private stepStyle =
    createObj
        [
            "stroke" ==> "#4fc3f7"
            "strokeWidth" ==> 0.08
            "opacity" ==> 0.5
            "fill" ==> "transparent"
        ]

let private trackStyle =
    createObj
        [
            "stroke" ==> "#ffa726"
            "strokeWidth" ==> 0.06
            "opacity" ==> 0.5
            "lineStyle" ==> "dotted"
            "fill" ==> "transparent"
        ]

let private groundStyle =
    createObj
        [
            "stroke" ==> "#ef5350"
            "strokeWidth" ==> 0.04
            "opacity" ==> 0.4
            "fill" ==> "transparent"
        ]

/// A path from the creep through the tiles of its own room.
let private path (visual: obj) (from: IRoomPosition) (tiles: RoomPos list) (style: obj) =
    let points =
        tiles
        |> List.filter (fun tile -> tile.Room = from.roomName)
        // Boxed, so each point is a plain array: the engine serialises a
        // typed one as an object.
        |> List.map (fun tile -> [| box tile.X; box tile.Y |])

    if not (List.isEmpty points) then
        visual?poly (Array.ofList ([| box from.x; box from.y |] :: points), style)
        |> ignore

/// One creep's overlay, off the plan that holds it.
let private drawCreep
    (plans: Map<string, LightTick.Plan>)
    (tasks: Assignments)
    (log: Observe.ObserveState)
    (refusals: Refusal list)
    (creep: ICreep)
    =
    let name = creep.name
    let at = creep.pos
    let visual = visualOf at.roomName

    let plan =
        plans
        |> Map.tryPick (fun _ plan -> if Set.contains name plan.Creeps then Some plan else None)

    let acts =
        plan
        |> Option.map (fun plan -> plan.Aims @ plan.Acts)
        |> Option.defaultValue []
        |> List.choose actOf
        |> List.filter (fun (actor, _, _) -> actor = name)
        |> List.distinct
        |> List.truncate 2

    let worded =
        acts
        |> List.map (fun (_, verb, target) ->
            match target with
            | None -> verb
            | Some target ->
                let seen = resolve target

                if not (isNull seen) then
                    let pos: IRoomPosition = seen?pos

                    if pos.roomName = at.roomName then
                        visual?line (at.x, at.y, pos.x, pos.y, targetStyle) |> ignore

                $"{verb} {labelOf target seen}")

    let task =
        Map.tryFind name tasks |> Option.map shortTask |> Option.defaultValue "no task"

    let story =
        Map.tryFind name log
        |> Option.map (fun creepLog ->
            let newest =
                creepLog.Entries
                |> List.tryFindBack (fun entry -> isTaskChannel entry.Verdict)
                |> Option.map (fun entry ->
                    $"{Observe.verdictText entry.Verdict} {Game.time - entry.Tick}t ago")

            let moves = creepLog.LastMove |> List.map Observe.verdictText
            Option.toList newest @ moves |> String.concat " · ")
        |> Option.filter (fun line -> line <> "")

    let refused =
        refusals
        |> List.tryFind (fun row -> row.Body && row.Actor = name && row.Fresh > 0)
        |> Option.map (fun row -> $"refused {row.Code} x{row.Fresh}")

    // Bottom line nearest the body, each one above the last.
    let lines =
        [ Some(String.concat " · " (task :: worded)); story; refused ] |> List.choose id

    lines
    |> List.rev
    |> List.iteri (fun i line ->
        visual?text (line, at.x, float at.y - 0.6 - 0.4 * float i, textStyle) |> ignore)

    match plan with
    | Some plan ->
        plan.Steps
        |> Map.tryFind name
        |> Option.iter (fun tiles -> path visual at tiles stepStyle)

        plan.Tracks
        |> Map.tryFind name
        |> Option.iter (fun (_, tiles) -> path visual at tiles trackStyle)

        plan.Mired
        |> Map.tryFind name
        |> Option.iter (
            Set.iter (fun tile ->
                if tile.Room = at.roomName then
                    visual?rect (float tile.X - 0.45, float tile.Y - 0.45, 0.9, 0.9, groundStyle)
                    |> ignore)
        )
    | None -> ()

/// Every creep of ours the draw list names, by its name or its room's.
let creeps
    (drawn: Set<string>)
    (plans: Map<string, LightTick.Plan>)
    (tasks: Assignments)
    (log: Observe.ObserveState)
    (refusals: Refusal list)
    =
    if canDraw && not (Set.isEmpty drawn) then
        for creep in objectValues<ICreep> Game.creeps do
            if
                not creep.spawning
                && (Set.contains creep.name drawn || Set.contains creep.pos.roomName drawn)
            then
                drawCreep plans tasks log refusals creep
