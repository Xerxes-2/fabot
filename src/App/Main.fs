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

// Path caches from the moveTo era (or anything else) may linger in
// Memory.creeps; drop entries of dead creeps so nothing outlives its creep.
// Alive means present in Game.creeps: unlike the Snapshot, that includes
// gestating creeps, whose memory must survive the spawn.
let private pruneDeadCreepMemory () =
    let creepsMemory = Memory?creeps

    if not (isNull creepsMemory) then
        for (name, _) in objectEntries creepsMemory do
            if isNull (Game.creeps?(name)) then
                emitJsStatement (creepsMemory, name) "delete $0[$1]"

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    let snapshot = Snapshot.build ()
    let decision = decide snapshot (loadAssignments ()) Set.empty
    // Memory writes land before the engine calls: a throw inside Executor.run
    // must not discard the tick's anti-thrash state.
    saveAssignments decision.Assignments
    pruneDeadCreepMemory ()
    // Outcomes go unread here; failures are already logged by the Executor.
    Executor.run decision.Intents |> ignore
