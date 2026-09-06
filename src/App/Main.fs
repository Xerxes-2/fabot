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

// The census-keyed plan memo (ADR 0017), one per colony and keyed by its
// home room (ADR 0047): heap state only, carried across ticks in this
// binding and never written to Memory — a global reset empties the table
// and the next tick recomputes every colony's plan from scratch.
//
// Keyed and not a list, because a colony that stops running — its home
// lost, or a human's edit to the declaration — must not leave its memo
// where another colony's lookup can find it: a plan recalled under the
// wrong census would place another room's sites. A key nobody asks for
// costs one stale entry until the reset, and the signature would refuse
// it anyway.
let mutable private planMemos: Map<string, PlanMemo> = Map.empty

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

    // Our spawns, for the two facts the *loop* reads off them: which
    // colonies are living, and which colony cast a given creep (ADR 0047).
    // Not the colonies' own casting lists — `Snapshot.build` sweeps
    // `Game.spawns` itself and filters to its home room, so a two-colony
    // tick enumerates the spawns three times and not once. Handing this
    // array down instead would be a fourth argument to a function whose
    // whole shape is three handed-in facts it decides none of, and the
    // sweep is a `Game` object walk over a colony's handful of spawns.
    let spawns = objectValues<ISpawn> Game.spawns

    // The colonies that run this tick: a declared home that is ours and
    // holds a spawn of ours (`Colony.living`, ADR 0047 decision 1). Both
    // facts come off the spawns — a home with no spawn in it fails the
    // second whatever the first says, so a controller read for a room no
    // spawn stands in would be an engine call whose answer changes
    // nothing.
    let ourRooms =
        spawns
        |> Array.filter (fun s ->
            let c = s.room.controller
            not (isNull (box c)) && not (isNull (box c.my)) && c.my)
        |> Array.map (fun s -> s.room.name)
        |> Set.ofArray

    let colonies =
        Colony.living
            ourRooms
            (spawns |> Array.map (fun s -> s.room.name) |> Array.distinct |> Array.toList)
            Colony.declared

    // Each colony's Raid log is read *before* any Snapshot is built, alone
    // among the observe channels, because ADR 0043's gate is a condition on
    // which rooms the shell scans at all: a stood-down outpost never enters
    // the projection, so the log has to be in hand before there is one. The
    // tick it is read at is this one and the conclusion is the previous
    // tick's — the last one that had the vision to read a deadline with,
    // which is the whole mechanism, since the gate's own effect is to
    // withdraw the creeps that pay for that vision.
    //
    // The same values are folded and written back at the bottom of the
    // loop. One read and not two: a second `loadRaids` after `decide` could
    // answer differently — a hand-edit through the Memory HTTP API lands
    // between them — and the tick would then be decided against one log
    // and recorded against another.
    //
    // A log per colony (ADR 0047): the gate withholds a room from the
    // colony that works it, and one shared ring would let one colony's
    // twenty home-room raids evict another colony's stand-down.
    let raids =
        colonies
        |> List.map (fun colony -> colony.Home, ObserveMemory.loadRaids colony.Home)
        |> Map.ofList

    // The gate's answer for each colony, derived once from that colony's
    // log: three readers ask for it — the scan set, the furniture and the
    // pooled rocks all narrow through it, inside `Snapshot.build` and
    // `Snapshot.projectedRooms` alike — and a second derivation is a second
    // answer free to disagree.
    let shut = raids |> Map.map (fun _ log -> Observe.standDown Game.time log)

    let shutOf home =
        shut |> Map.tryFind home |> Option.defaultValue Set.empty

    // Which rooms each colony projects, before any of them is built:
    // adoption is decided over the whole table at once (ADR 0047 decision
    // 2), so a creep's colony cannot be known from inside one Snapshot.
    let projections =
        colonies
        |> List.map (fun colony -> colony.Home, Snapshot.projectedRooms colony (shutOf colony.Home))

    // Every creep this bot owns, filed under the colony that holds it this
    // tick: the one it was cast by, or the one that has adopted it. Cut
    // once here and handed to each Snapshot, so the same creep cannot be
    // two colonies' business — two decisions over one body would write two
    // Tasks into the one flat `assignments` leaf and move it twice.
    let membership =
        Colony.creepColonies
            projections
            (spawns |> Array.map (fun s -> s.name, s.room.name) |> Array.toList)
            (objectValues<ICreep> Game.creeps
             |> Array.map (fun c -> c.name, Some c.room.name)
             |> Array.toList)

    let snapshots =
        colonies
        |> List.map (fun colony ->
            let mine =
                membership
                |> Map.toList
                |> List.choose (fun (creep, home) ->
                    if home = colony.Home then Some creep else None)
                |> Set.ofList

            colony, Snapshot.build colony (shutOf colony.Home) mine)

    // The Snapshot boundary, and the Raid logs' reads ride in this phase
    // rather than in the prelude: the two are one act — the gate decides
    // which rooms are swept — and splitting them would price a `find` sweep
    // against a Memory read. Every colony's sweep is in this one phase, so
    // the column is the tick's whole projection cost and not one colony's
    // (ADR 0047).
    let atSnapshot = Game.cpu.getUsed ()
    // The verbose list and the assignments are read once and handed to
    // every colony: both are flat, keyed by creep name, and a creep is one
    // colony's business for the tick — so what a colony is handed for a
    // creep it does not hold is dropped by the Matcher's own fold, which
    // keeps an assignment only for a creep in its Snapshot.
    let assignments = loadAssignments ()
    let verbose = ObserveMemory.loadVerbose ()

    let decisions =
        snapshots
        |> List.map (fun (colony, snapshot) ->
            colony,
            snapshot,
            decide snapshot assignments verbose (Map.tryFind colony.Home planMemos))

    // The decision boundary, and every colony's `decide` is inside it: the
    // column is what the tick spent deciding and not what one colony did
    // (ADR 0047). The two Memory reads above are inside it too: they are
    // `decide`'s arguments, which costs the phase a hash walk and buys the
    // reading a place the code cannot drift away from.
    let atDecide = Game.cpu.getUsed ()

    planMemos <-
        decisions
        |> List.map (fun (colony, _, decision) -> colony.Home, decision.Memo)
        |> Map.ofList

    // Memory writes land before the engine calls: a throw inside Executor.run
    // must not discard the tick's anti-thrash state.
    //
    // The assignments stay one flat leaf keyed by creep name (ADR 0047):
    // a creep is one colony's business for the tick, so the colonies'
    // answers are disjoint and the union is the whole map. Union and not
    // the last colony's answer, because each colony's Matcher drops what
    // it was handed for creeps outside its own Snapshot — writing one
    // colony's map alone would release every other colony's fleet.
    saveAssignments (
        (Map.empty, decisions)
        ||> List.fold (fun acc (_, _, decision) ->
            (acc, decision.Assignments)
            ||> Map.fold (fun acc creep task -> Map.add creep task acc))
    )

    // Dead creeps' timelines are pruned by the fold under the same
    // aliveness rule as the memory pruning below — and the Raid logs read
    // their losses against this one world-wide set too (ADR 0047): a
    // colony's `Creeps` is its own fleet since #191, so a name that left it
    // may merely have been adopted, and only `Game.creeps` can say which
    // names actually stopped existing.
    let living = livingCreeps ()

    // The Transition log stays flat too, and for the same reason: it is
    // keyed by creep name, and the tick's Verdicts are every colony's in
    // colony order — one fold over the union, so a creep adopted this tick
    // continues the timeline its caster started rather than beginning a
    // second one.
    ObserveMemory.load ()
    |> Observe.fold
        Observe.capPerCreep
        Game.time
        living
        (decisions |> List.collect (fun (_, _, decision) -> decision.Verdicts))
    |> ObserveMemory.save

    for colony, snapshot, decision in decisions do
        // The Raid log's own channel (ADR 0028): colony-level and episodic,
        // because the fold above prunes a creep's whole timeline the tick
        // it dies — the one event a raid record has to keep. Written every
        // tick whether or not the fold changed anything, so the leaf's
        // presence is itself the signal that this bundle is live — which is
        // what lets `observe.mjs raids` tell "no channel" from "no raids"
        // instead of reporting a confident false negative against a stale
        // deploy.
        //
        // Folded here and read at the top of the loop: this tick's
        // sightings are what the *next* tick's gate stands on (ADR 0043).
        // Under this colony's own key, from the log read under it: the
        // rooms a colony can see are the rooms it works, so an episode is
        // one colony's record and the gate that reads it back is that
        // colony's (ADR 0047). The losses are read against the world's
        // living names and not this colony's Snapshot, so a creep another
        // colony adopted this tick is not written down as this raid's
        // casualty.
        raids
        |> Map.tryFind colony.Home
        |> Option.defaultValue Observe.RaidState.empty
        |> Observe.foldRaids Observe.capEpisodes Observe.quietGap living snapshot
        |> ObserveMemory.saveRaids colony.Home

        // The Layout's own channel (ADR 0035): the footing targets this
        // tick's plan could not serve, the trunks it could not route
        // (#107), and the container picks it deferred to a container
        // already serving their target (ADR 0040).
        // Colony-level for the Raid log's structural reason — neither a
        // footing nor a trunk has a creep to key a Verdict on — and off the
        // memo, so a recalled plan reports exactly what it reported when it
        // was computed. Written every tick, empty or not, for the same
        // reason the Raid log's leaf is, and under the home room whose
        // Layout it is: one Layout per colony (ADR 0047).
        ObserveMemory.saveLayout
            colony.Home
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
    // Every colony's Intents in colony order, executed in one pass: the
    // engine is one world and the phase is the tick's whole execution cost
    // (ADR 0047). Nothing here has to be merged or deduplicated — an Intent
    // names an actor, and no actor is two colonies' this tick.
    let outcomes =
        Executor.run (decisions |> List.collect (fun (_, _, decision) -> decision.Intents))

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

    // The CPU line stays one flat leaf keyed by tick: it records the whole
    // loop, every colony's phase inside every column, so there is nothing
    // here for two colonies to collide over (ADR 0047).
    ObserveMemory.loadCpu ()
    |> Observe.foldCpu Observe.capCpuTicks Game.time readings
    |> ObserveMemory.saveCpu
