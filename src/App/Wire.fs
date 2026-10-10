/// Memory and segments: the only code that touches either
/// (`ARCHITECTURE.md` §4). `Memory.v2` is ours; v1's keys are never read or
/// written, so a rollback to v1 finds its Memory as it left it.
module Fabot.Wire

open Fable.Core
open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.State

/// Segment ids. Observe is read by `npm run observe`; the recorder's ring is
/// pulled by `scripts/capture` (§9.1).
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

/// `Memory.v2.switches.<module> = false` turns a module off; anything else
/// leaves it on. The first touch of `Memory` is the engine's parse, so this
/// is the read the tick's "memory" phase times.
let readState () : State =
    let s = switches ()

    {
        Off =
            objectEntries s
            |> Array.choose (fun (name, value) -> if value = box false then Some name else None)
            |> Set.ofArray
    }

/// The recorder is off unless `Memory.v2.switches.recorder = true`.
let recording () : bool = (switches ())?recorder = box true

/// One fault, kept on the heap in a short ring and written with observe.
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
        /// Shell phases and modules, in order, with their CPU.
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

/// Write this tick's world and the Memory string's size to the recorder's
/// ring. A tick whose record would not fit a segment is skipped and says so.
let record (time: int) (world: Fabot.Core.World.World) : string option =
    let text =
        jsonStringify (
            createObj
                [ "tick" ==> time; "memoryBytes" ==> (rawMemory ()).Length; "world" ==> world ]
        )

    if text.Length > 100 * 1024 then
        Some $"recorder: tick {time} is {text.Length} bytes, over a segment"
    else
        writeSegment (recorderFirst + time % recorderSlots) text
        None
