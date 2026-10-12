/// `Memory.v2` is ours; v1's keys are never read or written, so a rollback to
/// v1 finds its Memory as it left it.
module Fabot.Wire

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.State
open Fabot.Core.Fight.Shape

/// Read by `npm run observe` and `scripts/capture`.
let observeSegment = 1
let recorderFirst = 10
let recorderSlots = 5

[<Emit("RawMemory.get()")>]
let private rawMemory () : string = jsNative

[<Emit("(typeof $0 === 'object' && $0 !== null)")>]
let private isObject (_value: obj) : bool = jsNative

[<Emit("(Array.isArray($0) ? $0 : null)")>]
let private asArray (_value: obj) : obj[] = jsNative

let private root () : obj =
    let v2 = Memory?v2

    if isObject v2 then
        v2
    else
        let fresh = createObj [ "switches" ==> createObj [] ]
        Memory?v2 <- fresh
        fresh

let private switches () : obj =
    let v2 = root ()

    if isObject v2?switches then
        v2?switches
    else
        let fresh = createObj []
        v2?switches <- fresh
        fresh

let private field (o: obj) (name: string) : obj = if isObject o then o?(name) else null

let private intOf (o: obj) : int option =
    match o with
    | :? float as n -> Some(int n)
    | _ -> None

let private stringOf (o: obj) : string option =
    match o with
    | :? string as s -> Some s
    | _ -> None

let private listOf (parse: obj -> 'a option) (o: obj) : 'a list =
    match asArray o with
    | null -> []
    | items -> items |> Array.choose parse |> List.ofArray

let private posOf (o: obj) : Fabot.Core.World.Pos option =
    match field o "room", intOf (field o "x"), intOf (field o "y") with
    | (:? string as room), Some x, Some y -> Some { Room = room; X = x; Y = y }
    | _ -> None

let private posObj (p: Fabot.Core.World.Pos) =
    createObj [ "room" ==> p.Room; "x" ==> p.X; "y" ==> p.Y ]

let private groupOf (o: obj) : Group option =
    match intOf (field o "id"), intOf (field o "seen"), posOf o with
    | Some id, Some seen, Some centre ->
        Some
            {
                Id = id
                Members = listOf stringOf (field o "members") |> Set.ofList
                Room = centre.Room
                LastSeen = seen
                Centre = centre
            }
    | _ -> None

let private groupObj (g: Group) =
    let o = posObj g.Centre
    o?id <- g.Id
    o?seen <- g.LastSeen
    o?members <- Array.ofSeq g.Members
    o

let private phases =
    [
        "forming", Phase.Forming
        "approaching", Phase.Approaching
        "fighting", Phase.Fighting
        "pursuing", Phase.Pursuing
        "disengaging", Phase.Disengaging
    ]

let private tactics =
    [ "kite", Tactic.Kite; "close", Tactic.Close; "hold", Tactic.Hold ]

let private engagementOf (o: obj) : Engagement option =
    let phase =
        phases
        |> List.tryFind (fun (name, _) -> box name = field o "phase")
        |> Option.map snd

    match intOf (field o "id"), intOf (field o "group"), field o "room", phase with
    | Some id, Some group, (:? string as room), Some phase ->
        Some
            {
                Id = id
                Group = group
                Room = room
                Ours = listOf stringOf (field o "ours") |> Set.ofList
                Plan =
                    {
                        Tactic =
                            tactics
                            |> List.tryFind (fun (name, _) -> box name = field o "tactic")
                            |> Option.map snd
                            |> Option.defaultValue Tactic.Close
                        Order = listOf stringOf (field o "order")
                    }
                Phase = phase
                Since = intOf (field o "since") |> Option.defaultValue 0
                Lost = intOf (field o "lost") |> Option.defaultValue 0
                Budget = intOf (field o "budget") |> Option.defaultValue 0
                Priority =
                    match field o "priority" with
                    | :? float as p -> p
                    | _ -> 0.0
                Unanswered = field o "unanswered" = box true
                Capacity = intOf (field o "capacity") |> Option.defaultValue 0
            }
    | _ -> None

let private engagementObj (e: Engagement) =
    createObj
        [
            "id" ==> e.Id
            "group" ==> e.Group
            "room" ==> e.Room
            "ours" ==> Array.ofSeq e.Ours
            "tactic" ==> (tactics |> List.find (fun (_, t) -> t = e.Plan.Tactic) |> fst)
            "order" ==> Array.ofList e.Plan.Order
            "phase" ==> (phases |> List.find (fun (_, p) -> p = e.Phase) |> fst)
            "since" ==> e.Since
            "lost" ==> e.Lost
            "budget" ==> e.Budget
            "priority" ==> e.Priority
            "unanswered" ==> e.Unanswered
            "capacity" ==> e.Capacity
        ]

let private castOf (o: obj) : Cast option =
    let part (letter: char) =
        Fabot.Core.Vocabulary.allBodyParts
        |> List.tryFind (fun p -> Fabot.Core.Vocabulary.partLetter p = letter)

    match intOf (field o "engagement"), field o "name", field o "room", field o "body" with
    | Some engagement, (:? string as name), (:? string as room), (:? string as body) ->
        let parts = body |> Seq.choose part |> List.ofSeq

        if parts.Length = body.Length then
            Some
                {
                    Engagement = engagement
                    Name = name
                    Room = room
                    Body = parts
                }
        else
            None
    | _ -> None

let private castObj (c: Cast) =
    createObj
        [
            "engagement" ==> c.Engagement
            "name" ==> c.Name
            "room" ==> c.Room
            "body"
            ==> (c.Body |> List.map Fabot.Core.Vocabulary.partLetter |> System.String.Concat)
        ]

/// The first touch of `Memory` is the engine's parse, so this is the read the
/// tick's "memory" phase times.
let readState () : State =
    let s = switches ()
    let held = (root ())?assignments
    let fight = (root ())?fight

    {
        Off =
            objectEntries s
            |> Array.choose (fun (name, value) -> if value = box false then Some name else None)
            |> Set.ofArray
        Assignments =
            if isObject held then
                objectEntries held
                |> Array.choose (fun (name, value) ->
                    match value with
                    | :? string as id -> Some(name, id)
                    | _ -> None)
                |> Map.ofArray
            else
                Map.empty
        Layout = Map.empty
        Groups = listOf groupOf (field fight "groups")
        Engagements = listOf engagementOf (field fight "engagements")
        Casts = listOf castOf (field fight "casts")
    }

/// The switches are the operator's and are left as they are.
let writeState (state: State) : unit =
    (root ())?assignments <-
        createObj (state.Assignments |> Map.toList |> List.map (fun (name, id) -> name ==> id))

    (root ())?fight <-
        createObj
            [
                "groups" ==> (state.Groups |> List.map groupObj |> Array.ofList)
                "engagements" ==> (state.Engagements |> List.map engagementObj |> Array.ofList)
                "casts" ==> (state.Casts |> List.map castObj |> Array.ofList)
            ]

let readDeclarations () : Fabot.Core.Tick.Declarations =
    let declared = (root ())?declarations

    let declaredAt (room: string) (o: obj) : Fabot.Core.Tick.Declared option =
        match field o "id", field o "x", field o "y" with
        | (:? string as id), (:? float as x), (:? float as y) ->
            Some
                {
                    Id = id
                    Pos = { Room = room; X = int x; Y = int y }
                }
        | _ -> None

    let outpostOf (o: obj) : Fabot.Core.Tick.Outpost option =
        match field o "room", asArray (field o "sources") with
        | (:? string as room), sources when not (isNull sources) ->
            let parsed = sources |> Array.choose (declaredAt room)

            declaredAt room (field o "controller")
            |> Option.filter (fun _ -> parsed.Length = sources.Length)
            |> Option.map (fun controller ->
                {
                    Room = room
                    Controller = controller
                    Sources = List.ofArray parsed
                })
        | _ -> None

    {
        ReserveLifetimes =
            match field declared "reserve" with
            | :? float as n when n >= 0.0 -> n
            | _ -> Fabot.Core.Tick.undeclared.ReserveLifetimes
        Outposts =
            match asArray (field declared "outposts") with
            | null -> []
            | outposts -> outposts |> Array.choose outpostOf |> List.ofArray
    }

let recording () : bool = (switches ())?recorder = box true

type Fault =
    {
        Tick: int
        Where: string
        Message: string
    }

type Observed =
    {
        Tick: int
        Version: string
        Bucket: int
        Total: float
        Phases: (string * float) list
        Off: string list
        Faults: Fault list
        Refusals: Executor.Refusal list
    }

let writeObserve (o: Observed) : unit =
    let text =
        jsonStringify (
            createObj
                [
                    "tick" ==> o.Tick
                    "version" ==> o.Version
                    "bucket" ==> o.Bucket
                    "cpu" ==> o.Total
                    "phases"
                    ==> (o.Phases
                         |> List.map (fun (name, ms) -> [| box name; box ms |])
                         |> Array.ofList)
                    "off" ==> Array.ofList o.Off
                    "faults"
                    ==> (o.Faults
                         |> List.map (fun f ->
                             createObj
                                 [
                                     "tick" ==> f.Tick
                                     "where" ==> f.Where
                                     "message" ==> f.Message
                                 ])
                         |> Array.ofList)
                    "refusals"
                    ==> (o.Refusals
                         |> List.map (fun r ->
                             createObj [ "actor" ==> r.Actor; "act" ==> r.Act; "code" ==> r.Code ])
                         |> Array.ofList)
                ]
        )

    writeSegment observeSegment text

/// A terrain buffer would otherwise stringify as an object keyed by index.
[<Emit("JSON.stringify($0, (_k, v) => v instanceof Uint8Array ? Array.from(v).join('') : v)")>]
let private stringifyWorld (_value: obj) : string = jsNative

let record (time: int) (world: Fabot.Core.World.World) : string option =
    let text =
        stringifyWorld (
            createObj
                [ "tick" ==> time; "memoryBytes" ==> (rawMemory ()).Length; "world" ==> world ]
        )

    if text.Length > 100 * 1024 then
        Some $"recorder: tick {time} is {text.Length} bytes, over a segment"
    else
        writeSegment (recorderFirst + time % recorderSlots) text
        None
