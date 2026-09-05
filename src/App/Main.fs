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

// The census-keyed plan memo (ADR 0017): heap state only, carried across
// ticks in this binding and never written to Memory — a global reset
// starts the next tick at None and decide recomputes from scratch.
let mutable private planMemo: PlanMemo option = None

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    // The engine's counter is already running when `loop` is entered, and
    // this reads how far (#170). Nothing of the bot's has run yet, so this
    // number is the engine's prelude alone — the tick's own bookkeeping
    // before the bot is called — and it is the first candidate for the
    // ~1.6× that separates the live line from the local ruler, which has no
    // engine in front of it at all.
    //
    // What is *not* in it is the Memory parse, the second candidate.
    // `Memory` deserializes on the first touch of it, and the first touch
    // of this loop is `loadRaids` below — so the parse is charged to the
    // `snapshot` phase, fused there with the `find` sweep, and a reader
    // attributing the gap must not strike it off against this column.
    //
    // Every reading below is taken unconditionally, at every boundary, on
    // every tick. No "the phases are only worth it when a tick is slow"
    // guard: ADR 0041 is measured, not budgeted, and a measurement that
    // switches itself off is one whose absences a reader has to explain.
    let atEntry = Game.cpu.getUsed ()

    // The Raid log is read *before* the Snapshot is built, alone among the
    // observe channels, because ADR 0043's gate is a condition on which
    // rooms the shell scans at all: a stood-down outpost never enters the
    // projection, so the log has to be in hand before there is one. The
    // tick it is read at is this one and the conclusion is the previous
    // tick's — the last one that had the vision to read a deadline with,
    // which is the whole mechanism, since the gate's own effect is to
    // withdraw the creeps that pay for that vision.
    //
    // The same value is folded and written back at the bottom of the loop.
    // One read and not two: a second `loadRaids` after `decide` could
    // answer differently — a hand-edit through the Memory HTTP API lands
    // between them — and the tick would then be decided against one log
    // and recorded against another.
    let raids = ObserveMemory.loadRaids ()
    let snapshot = Snapshot.build (Observe.standDown Game.time raids)
    // The Snapshot boundary, and the Raid log's read rides in this phase
    // rather than in the prelude: the two are one act — the gate decides
    // which rooms are swept — and splitting them would price a `find` sweep
    // against a Memory read.
    let atSnapshot = Game.cpu.getUsed ()
    // The verbose list is read fresh from Memory each tick, so a flip from
    // the terminal changes what the very next tick records.
    let decision =
        decide snapshot (loadAssignments ()) (ObserveMemory.loadVerbose ()) planMemo

    // The decision boundary. The two Memory reads above are inside it: they
    // are `decide`'s arguments and are evaluated as it is called, which
    // costs the phase a hash walk and buys the reading a place the code
    // cannot drift away from.
    let atDecide = Game.cpu.getUsed ()
    planMemo <- Some decision.Memo
    // Memory writes land before the engine calls: a throw inside Executor.run
    // must not discard the tick's anti-thrash state.
    saveAssignments decision.Assignments
    // Dead creeps' timelines are pruned by the fold under the same
    // aliveness rule as the memory pruning below.
    let living = livingCreeps ()

    ObserveMemory.load ()
    |> Observe.fold Observe.capPerCreep snapshot.Time living decision.Verdicts
    |> ObserveMemory.save

    // The Raid log's own channel (ADR 0028): colony-level and episodic,
    // because the fold above prunes a creep's whole timeline the tick it
    // dies — the one event a raid record has to keep. Written every tick
    // whether or not the fold changed anything, so the leaf's presence is
    // itself the signal that this bundle is live — which is what lets
    // `observe.mjs raids` tell "no channel" from "no raids" instead of
    // reporting a confident false negative against a stale deploy.
    //
    // Folded here and read at the top of the loop: this tick's sightings
    // are what the *next* tick's gate stands on (ADR 0043).
    raids
    |> Observe.foldRaids Observe.capEpisodes Observe.quietGap snapshot
    |> ObserveMemory.saveRaids

    // The Layout's own channel (ADR 0035): the footing targets this tick's
    // plan could not serve, the trunks it could not route (#107), and the
    // container picks it deferred to a container already serving their
    // target (ADR 0040).
    // Colony-level for the Raid log's structural reason — neither a footing
    // nor a trunk has a creep to key a Verdict on — and off the memo, so a
    // recalled plan reports exactly what it reported when it was computed.
    // Written every tick, empty or not, for the same reason the Raid log's
    // leaf is.
    ObserveMemory.saveLayout
        decision.Memo.UnservedFootings
        decision.Memo.UnroutedTrunks
        decision.Memo.DeferredContainers

    pruneDeadCreepMemory living
    // The Memory boundary: the assignments, all three observe channels and
    // the dead creeps' pruning, which is everything this tick persists
    // except the CPU line's own leaf — that one is written after the last
    // reading is taken, so it is the single write the line never prices.
    // A boundary and not a noun's price: the phase holds the observe folds
    // (`Observe.fold`, `foldRaids`) and the `Game.creeps` sweep that feeds
    // them as well as the writes themselves, so a reader cannot conclude
    // "persisting costs this much" off the column alone.
    // This phase sits between `decide` and the intents rather than after
    // them because that is where the writes are, and they are there
    // deliberately: a throw inside `Executor.run` must not discard the
    // tick's anti-thrash state.
    let atSave = Game.cpu.getUsed ()
    // Failures are already logged by the Executor; what is read off the
    // outcomes here is how many intents the engine took (#170). The engine
    // charges 0.2 CPU per intent it *executes*, so a call it answered with
    // an error code, and one whose actor the Snapshot promised but the
    // engine does not hold, are both counted out: this number times 0.2 is
    // an estimate a reader can subtract, and only the accepted calls belong
    // in it.
    let outcomes = Executor.run decision.Intents

    let accepted =
        outcomes
        |> List.sumBy (fun (_, outcome) ->
            match outcome with
            | Executor.Ok -> 1
            | Executor.Failed _
            | Executor.ActorMissing -> 0)

    // The CPU line (ADR 0041): one row per tick, so the condition that
    // sends the layered projection back to the drawing board — a mean tick
    // above 50 ms, or any single tick above 80 — is a number somebody can
    // read rather than a feeling. Measured, never budgeted: nothing in the
    // bot reads this back, and the thresholds live with the readers
    // (`scripts/cpu-trigger.mjs`).
    //
    // The tick's total is deliberately the last of the five readings, taken
    // after the Executor, because the intents are most of what a tick costs
    // and a measurement taken before them would flatter every tick. What it
    // therefore excludes is this channel's own read and write and the
    // engine's serialization of Memory once `loop` returns — all outside
    // what the bot can move — and any tick that throws before reaching
    // here, which writes no row at all: the gap in the tick numbers is the
    // record of it.
    let readings: Observe.CpuReadings =
        {
            AtEntry = atEntry
            AtSnapshot = atSnapshot
            AtDecide = atDecide
            AtSave = atSave
            AtExecute = Game.cpu.getUsed ()
            Intents = accepted
        }

    ObserveMemory.loadCpu ()
    |> Observe.foldCpu Observe.capCpuTicks snapshot.Time readings
    |> ObserveMemory.saveCpu
