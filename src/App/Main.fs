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
// World, that includes gestating creeps, whose memory and timeline must
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

// The census-keyed plan memo (ADR 0017), one per colony and keyed by its home
// room (ADR 0047): heap state only, carried across ticks in this binding and
// never written to Memory.
let mutable private planMemos: Map<string, PlanMemo> = Map.empty

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    // The engine's counter is already running when `loop` is entered, and this
    // reads how far (#170). Nothing of the bot's has run yet, so this number is
    // the engine's prelude alone, and it is the first candidate for the ~1.6x
    // that separates the live line from the local ruler. What is *not* in it is
    // the Memory parse, the second candidate: `Memory` deserializes on the
    // first touch of it, which is `loadRaids` below, so the parse is charged to
    // the `snapshot` phase and a reader attributing the gap must not strike it
    // off against this column. Every reading below is taken unconditionally, on
    // every tick: ADR 0041 is measured, not budgeted, and a measurement that
    // switches itself off is one whose absences a reader has to explain.
    let atEntry = Game.cpu.getUsed ()

    // The tick's World: every room we declared or can see and every creep we
    // own, read out of the engine once (ADR 0052 decision 1). Every other line
    // in this loop works off this record or off Memory.
    let world = World.ofGame Colony.declared (ObserveMemory.loadPositions ())

    // The colonies that run this tick: a declared home that is ours and holds
    // a spawn of ours (`Colony.living`, ADR 0047 decision 1), both facts read
    // off the world's rooms rather than a second sweep of `Game.spawns`.
    let colonies = World.living Colony.declared world

    // Each colony's Raid log is read *before* any view is cut, alone among the
    // observe channels, because ADR 0043's gate is a condition on which rooms a
    // colony works at all: a stood-down outpost never enters its projection.
    // The conclusion is the previous tick's — the last one with the vision to
    // read a deadline — which is the whole mechanism, the gate's own effect
    // being to withdraw the creeps that pay for that vision. One read and not
    // two: a second `loadRaids` after `decide` could answer differently — a
    // hand-edit through the Memory HTTP API lands between them — and the tick
    // would be decided against one log and recorded against another.
    let raids =
        colonies
        |> List.map (fun colony -> colony.Home, ObserveMemory.loadRaids colony.Home)
        |> Map.ofList

    // The gate's answer for each colony, derived once from that colony's log:
    // the scan set, the furniture and the pooled rocks all narrow through it
    // inside `ColonyView.ofWorld`, and a second derivation would be a second
    // answer free to disagree.
    let shut = raids |> Map.map (fun _ log -> Observe.standDown world.Time log)

    let shutOf home =
        shut |> Map.tryFind home |> Option.defaultValue Set.empty

    // Every creep this bot owns, filed under the colony that holds it this
    // tick: the one it was cast by, or the one that has adopted it (ADR 0047
    // decision 2). Cut once here over every living colony's scan set and handed
    // to each view — a creep cannot be two colonies' business, or two decisions
    // would write two Tasks into the one flat `assignments` leaf. An argument
    // to the view and not a field of the World, because the rule needs the
    // stand-down gate above, which is Memory's answer and not the world's. The
    // numbers every colony decides under (ADR 0052 decision 5).
    let holders =
        World.creepColonies Tuning.defaults Colony.declared colonies shut world

    // One view per living colony (ADR 0052 decision 1), each cut from the one
    // world by a pure function in Core: the rooms this colony works, the bodies
    // it holds, its own bank and controller, and the explicit little it may
    // borrow of a child's. `ColonyView.ofWorld` owns every rule, and that half
    // of the shell boundary is under test (`ViewTests`, ADR 0052 decision 8).
    let views =
        colonies
        |> List.map (fun colony ->
            colony,
            ColonyView.ofWorld
                Tuning.defaults
                Colony.declared
                (shutOf colony.Home)
                holders
                world
                colony)

    // The projection boundary, and the Raid logs' reads ride in this phase
    // rather than the prelude: the two are one act — the gate decides which
    // rooms a colony works — and splitting them would price a `find` sweep
    // against a Memory read. The world's one sweep and every colony's cut of
    // it are both inside this column (ADR 0047).
    let atSnapshot = Game.cpu.getUsed ()
    // The verbose list and the assignments are read once and handed to every
    // colony: both are flat, keyed by creep name, and a creep is one colony's
    // business for the tick — so what a colony is handed for a creep it does
    // not hold is dropped by the Matcher's own fold.
    let assignments = loadAssignments ()
    let verbose = ObserveMemory.loadVerbose ()

    // Each colony decides its own tick, with its movement left unarbitrated
    // (#216 R2b): a room two colonies both work is one room, and half its
    // traffic arbitrated against the other half read as empty is how a
    // mother's [[pioneer]] came to claim the child's [[anchor]]'s tile (#220).
    let decisions =
        views
        |> List.map (fun (colony, view) ->
            colony,
            view,
            decideUnarbitrated view assignments verbose (Map.tryFind colony.Home planMemos))

    // The one movement pass of the tick: every colony's Move Intents folded
    // together and arbitrated once per room, over every creep of ours standing
    // in it, each moving on the intent its own colony registered (ADR 0001 —
    // this is that pure Resolver taking the whole room as its argument).
    let moveIntents, moveVerdicts =
        resolveRooms (decisions |> List.map (fun (_, _, decision) -> decision.Movement))

    // The decision boundary, and every colony's `decide` is inside it: the
    // column is what the tick spent deciding and not what one colony did (ADR
    // 0047). The two Memory reads above are inside it too, being `decide`'s
    // arguments, which buys the reading a place the code cannot drift away
    // from.
    let atDecide = Game.cpu.getUsed ()

    planMemos <-
        decisions
        |> List.map (fun (colony, _, decision) -> colony.Home, decision.Memo)
        |> Map.ofList

    // Memory writes land before the engine calls: a throw inside Executor.run
    // must not discard the tick's anti-thrash state. The assignments stay one
    // flat leaf keyed by creep name (ADR 0047): a creep is one colony's
    // business for the tick, so the colonies' answers are disjoint and the
    // union is the whole map.
    saveAssignments (
        (Map.empty, decisions)
        ||> List.fold (fun acc (_, _, decision) ->
            (acc, decision.Assignments)
            ||> Map.fold (fun acc creep task -> Map.add creep task acc))
    )

    // Dead creeps' timelines are pruned by the fold under the same aliveness
    // rule as the memory pruning below — and the Raid logs read their losses
    // against this one world-wide set too (ADR 0047): a colony's `Creeps` is
    // its own fleet, so a name that left it may merely have been
    // adopted, and only `Game.creeps` can say which names stopped existing.
    let living = livingCreeps ()

    // The Transition log stays flat too, and for the same reason: it is keyed
    // by creep name, and the tick's Verdicts are every colony's in colony
    // order — one fold over the union, so a creep adopted this tick continues
    // the timeline its caster started.
    ObserveMemory.load ()
    |> Observe.fold
        Observe.capPerCreep
        Game.time
        living
        ((decisions |> List.collect (fun (_, _, decision) -> decision.Verdicts))
         @ moveVerdicts)
    |> ObserveMemory.save

    for colony, view, decision in decisions do
        // The Raid log's own channel (ADR 0028): colony-level and episodic,
        // because the fold above prunes a creep's whole timeline the tick it
        // dies — the one event a raid record has to keep. Written every tick
        // whether or not the fold changed anything, so the leaf's presence is
        // itself the signal that this bundle is live, which is what lets
        // `observe.mjs raids` tell "no channel" from "no raids". Folded here
        // and read at the top of the loop: this tick's sightings are what the
        // *next* tick's gate stands on (ADR 0043). Under this colony's own key,
        // an episode being one colony's record and the gate that reads it back
        // that colony's (ADR 0047).
        raids
        |> Map.tryFind colony.Home
        |> Option.defaultValue Observe.RaidState.empty
        |> Observe.foldRaids Observe.capEpisodes living view
        |> ObserveMemory.saveRaids colony.Home

        // The Layout's own channel (ADR 0035): the footing targets this tick's
        // plan could not serve, the trunks it could not route (#107), and the
        // container picks it deferred to a container already serving their
        // target (ADR 0040). Written every tick, empty or not, and under the
        // home room whose Layout it is (ADR 0047). Beside them the colony's
        // other loss of this tick, taken off the **view** and not the memo
        // because it is the declaration's rather than the plan's: the declared
        // outposts this home shares no border with, which the view refused
        // (#243).
        ObserveMemory.saveLayout
            colony.Home
            decision.Memo.UnservedFootings
            decision.Memo.UnroutedTrunks
            decision.Memo.DeferredContainers
            view.Refused

        // The cascade's own numbers, for `observe.mjs quotas` (ADR 0009).
        ObserveMemory.saveQuotas colony.Home decision.Quotas

    pruneDeadCreepMemory living

    // Where every creep of ours stood this tick, for next tick's
    // `CreepInfo.Moved` (#225).
    objectValues<ICreep> Game.creeps
    |> Array.filter (fun c -> not c.spawning)
    |> Array.map (fun c ->
        c.name,
        ({
            Room = c.room.name
            X = c.pos.x
            Y = c.pos.y
        }
        : RoomPos))
    |> Array.toList
    |> ObserveMemory.savePositions
    // The Memory boundary: the assignments, all three observe channels and the
    // dead creeps' pruning, which is everything this tick persists except the
    // CPU line's own leaf — written after the last reading, so it is the single
    // write the line never prices. A boundary and not a noun's price: the phase
    // holds the observe folds and the `Game.creeps` sweep that feeds them as
    // well as the writes.
    let atSave = Game.cpu.getUsed ()
    // Failures are already logged by the Executor; what is read off the
    // outcomes here is how many intents the engine took (#170). The engine
    // charges 0.2 CPU per intent it *executes*, so a call answered with an
    // error code, and one whose actor the view promised but the engine does not
    // hold, are both counted out. Every colony's Intents in colony order,
    // executed in one pass: the engine is one world and the phase is the tick's
    // whole execution cost (ADR 0047).
    let outcomes =
        Executor.run (
            (decisions |> List.collect (fun (_, _, decision) -> decision.Intents))
            @ moveIntents
        )

    let accepted =
        outcomes
        |> List.sumBy (fun (_, outcome) ->
            match outcome with
            | Executor.Ok -> 1
            | Executor.Failed _
            | Executor.ActorMissing -> 0)

    // The CPU line (ADR 0041): one row per tick, so the condition that sends
    // the layered projection back to the drawing board — a mean tick above 50
    // ms, or any single tick above 80 — is a number somebody can read rather
    // than a feeling. Measured, never budgeted: nothing in the bot reads this
    // back, and the thresholds live with the readers. The tick's total is
    // deliberately the last of the five readings, taken after the Executor,
    // because the intents are most of what a tick costs.
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
