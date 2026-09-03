module Fabot.Main

open Fable.Core.JsInterop
open Fabot.Bindings
open Fabot.Core
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

// The one aliveness rule for pruning: present in Game.creeps — unlike the
// Snapshot, that includes gestating creeps, whose memory and timeline must
// survive the spawn.
let private livingCreeps () =
    objectEntries Game.creeps |> Array.map fst |> Set.ofArray

// Path caches from the moveTo era (or anything else) may linger in
// Memory.creeps; drop entries of dead creeps so nothing outlives its creep.
let private pruneDeadCreepMemory (living: Set<string>) =
    let creepsMemory = Memory?creeps

    if not (isNull creepsMemory) then
        for (name, _) in objectEntries creepsMemory do
            if not (Set.contains name living) then
                emitJsStatement (creepsMemory, name) "delete $0[$1]"

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    let snapshot = Snapshot.build ()
    // The verbose list is read fresh from Memory each tick, so a flip from
    // the terminal changes what the very next tick records.
    let decision = decide snapshot (loadAssignments ()) (ObserveMemory.loadVerbose ())
    // Memory writes land before the engine calls: a throw inside Executor.run
    // must not discard the tick's anti-thrash state.
    saveAssignments decision.Assignments
    // Dead creeps' timelines are pruned by the fold under the same
    // aliveness rule as the memory pruning below.
    let living = livingCreeps ()

    ObserveMemory.load ()
    |> Observe.fold Observe.capPerCreep snapshot.Time living decision.Verdicts
    |> ObserveMemory.save

    pruneDeadCreepMemory living
    // Outcomes go unread here; failures are already logged by the Executor.
    Executor.run decision.Intents |> ignore
