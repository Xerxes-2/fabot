module Fabot.Main

open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core.Types
open Fabot.Core.Decide

// Assignments persist in Memory.fabot.assignments as a plain {creepName: taskId} hash.
let private loadAssignments () : Assignments =
    // Memory.fabot is absent on a bare respawn / after a memory wipe.
    let fabot = Memory?fabot
    let raw = if isNull fabot then null else fabot?assignments

    if isNull raw then
        Map.empty
    else
        objectEntries raw
        |> Array.map (fun (name, taskId) -> name, string taskId)
        |> Map.ofArray

let private saveAssignments (assignments: Assignments) =
    let hash = createEmpty<obj>

    for KeyValue(name, taskId) in assignments do
        hash?(name) <- taskId

    if isNull Memory?fabot then
        Memory?fabot <- createEmpty<obj>

    Memory?fabot?assignments <- hash

// The engine writes per-creep scratch (moveTo path cache) into Memory.creeps;
// drop entries of dead creeps so nothing of ours outlives its creep.
let private pruneDeadCreepMemory (snapshot: Snapshot) =
    let creepsMemory = Memory?creeps

    if not (isNull creepsMemory) then
        let alive = snapshot.Creeps |> List.map (fun c -> c.Name) |> Set.ofList

        for (name, _) in objectEntries creepsMemory do
            if not (Set.contains name alive) then
                emitJsStatement (creepsMemory, name) "delete $0[$1]"

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    let snapshot = Snapshot.build ()
    let intents, assignments = decide snapshot (loadAssignments ())
    Executor.run intents
    saveAssignments assignments
    pruneDeadCreepMemory snapshot
