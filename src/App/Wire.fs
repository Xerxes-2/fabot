/// `Memory.v2` is ours; v1's keys are never read or written, so a rollback to
/// v1 finds its Memory as it left it.
module Fabot.Wire

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.State

/// Read by `npm run observe` and `scripts/capture`.
let observeSegment = 1
let recorderFirst = 10
let recorderSlots = 5

[<Emit("RawMemory.get()")>]
let private rawMemory () : string = jsNative

[<Emit("(typeof $0 === 'object' && $0 !== null)")>]
let private isObject (_value: obj) : bool = jsNative

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

/// The first touch of `Memory` is the engine's parse, so this is the read the
/// tick's "memory" phase times.
let readState () : State =
    let s = switches ()
    let held = (root ())?assignments

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
    }

/// The switches are the operator's and are left as they are.
let writeState (state: State) : unit =
    (root ())?assignments <-
        createObj (state.Assignments |> Map.toList |> List.map (fun (name, id) -> name ==> id))

let readDeclarations () : Fabot.Core.Tick.Declarations =
    let declared = (root ())?declarations

    match (if isObject declared then box declared?reserve else null) with
    | :? float as n when n >= 0.0 -> { ReserveLifetimes = n }
    | _ -> Fabot.Core.Tick.undeclared

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
