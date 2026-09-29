module Fabot.Main

open Fable.Core
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

// The plan memo per colony, keyed by home: heap state only, never in Memory.
let mutable private planMemos: Map<string, PlanMemo> = Map.empty

// What the world last saw standing in each room, on the heap and not in
// Memory: the grace it feeds is worth 150 ticks of one creep's patience, not
// a leaf every tick's `JSON.stringify` pays for. A reset empties it and the
// colony decides as it did before the grace existed.
let mutable private sightings: Map<string, RoomSighting> = Map.empty

// The CPU line on the heap: read off Memory only when the heap holds none (a
// global reset), dropped when the leaf is gone (discarded on purpose). The
// leaf is still written every tick; the heap saves decoding and re-encoding
// the hundred standing rows to add one (`ObserveMemory.appendCpu`).
let mutable private cpuLine: Observe.CpuState option = None

// Which ordered room pairs a Seam band joins, on the heap and emptied only
// by a reset or the console (#395):
// every answer is the terrain's, and the one moving input (whether the world
// holds both rooms) is read ahead of the table (`World.linkedRecalling`).
let private joins = JoinTable()

// The Transition log on the heap, on the CPU line's terms: folded on the
// heap and written back one changed creep at a time
// (`ObserveMemory.saveChanged`). It was the largest leaf in Memory.
let mutable private observeLog: Observe.ObserveState option = None

[<Emit("globalThis")>]
let private globalThis: obj = jsNative

// The five process-lifetime slots, on the global for the console (#395):
// `__fabot.sizes()` and `__fabot.drop.<slot>()`, so a heap that climbs with
// every count flat can be attributed by dropping one slot, `gc()`, and
// reading `Game.cpu.getHeapStatistics()` — each drop is a state the loop
// already recovers from. Read by nothing in the loop.
let private sizes () =
    let memos =
        planMemos
        |> Map.toList
        |> List.map (fun (home, memo) ->
            home,
            createObj
                [
                    "walks" ==> memo.Walks.Count
                    "seams" ==> memo.SeamWalks.Count
                    "far" ==> memo.FarFields.Count
                    "narrowed" ==> memo.Narrowed.Count
                    "sites" ==> List.length memo.SiteIntents
                    "demand" ==> List.length memo.HaulerDemand
                ])

    let log = observeLog |> Option.defaultValue Map.empty

    createObj
        [
            "memos" ==> createObj memos
            "sightings" ==> Map.count sightings
            "cpuTicks"
            ==> (cpuLine |> Option.map (fun l -> List.length l.Ticks) |> Option.defaultValue 0)
            "cpuSpans"
            ==> (cpuLine |> Option.map (fun l -> List.length l.Spans) |> Option.defaultValue 0)
            "logCreeps" ==> Map.count log
            "logEntries"
            ==> (log |> Map.toList |> List.sumBy (fun (_, c) -> List.length c.Entries))
            "joins" ==> joins.Count
            "hops" ==> joins.Hops.Count
        ]

do
    globalThis?__fabot <-
        createObj
            [
                "sizes" ==> sizes
                "drop"
                ==> createObj
                        [
                            "planMemos" ==> (fun () -> planMemos <- Map.empty)
                            "sightings" ==> (fun () -> sightings <- Map.empty)
                            "cpuLine" ==> (fun () -> cpuLine <- None)
                            "observeLog" ==> (fun () -> observeLog <- None)
                            "joins"
                            ==> (fun () ->
                                joins.Joins.Clear()
                                joins.Hops.Clear())
                        ]
            ]

// The last full tick's record and the tick it was taken on, on the heap: a
// reset empties it and the next tick is full.
let mutable private lastFull: (int * LightTick.LastFull) option = None

// Full ticks run since the reset: the replan turn counts these, not
// `Game.time`, which with light ticks between would hand an even count of
// colonies half the turns and the other half none.
let mutable private fullTicks = 0

/// A part of the three that make a hostile armed, destroyed or not, as
/// `World` reads a hostile's body: a wrong "armed" costs a full tick, a wrong
/// "unarmed" a shot undecided.
let private armedPart (part: IBodyPartDef) =
    part.``type`` = "attack"
    || part.``type`` = "ranged_attack"
    || part.``type`` = "heal"

/// What this tick sees of `Game` without a World: our creeps, every hostile
/// in a visible room, our structures' tiles and our controllers. Built with
/// `Fresh`, because the full tick keeps it as the last full tick's record.
let private glance () : LightTick.Glance =
    let tileOf (room: string) (pos: IRoomPosition) =
        RoomPos.at room { X = pos.x; Y = pos.y }

    let rooms = objectValues<IRoom> Game.rooms

    {
        Creeps =
            objectValues<ICreep> Game.creeps
            // A body still in its spawn is not yet one the decision holds, as
            // `World` reads it: it is born, for `CreepsChanged`, when it
            // emerges.
            |> Array.filter (fun c -> not c.spawning)
            |> Array.map (fun c ->
                c.name,
                ({
                    Tile = tileOf c.room.name c.pos
                    Hits = c.hits
                }
                : LightTick.GlanceCreep))
            |> Fresh.mapOfArray
        Hostiles =
            [
                for room in rooms do
                    for hostile in room.find findHostileCreeps |> Array.map unbox<ICreep> do
                        yield
                            ({
                                Tile = tileOf room.name hostile.pos
                                Owner =
                                    if isNull (box hostile.owner) then
                                        ""
                                    else
                                        hostile.owner.username
                                Armed = hostile.body |> Array.exists armedPart
                            }
                            : LightTick.GlanceHostile)
            ]
        Structures =
            objectValues<IStructure> Game.structures
            |> Array.map (fun s -> tileOf s.pos.roomName s.pos)
            |> Array.toList
        Controllers =
            rooms
            |> Array.choose (fun room ->
                let controller = room.controller

                if not (isNull (box controller)) && controller.my then
                    Some(room.name, (controller.level, controller.safeMode > 0))
                else
                    None)
            |> Fresh.mapOfArray
    }

/// The CPU line's write, the same on both kinds of tick: nothing in the bot
/// reads it back.
let private writeCpuLine (readings: Observe.CpuReadings) =
    // One flat leaf keyed by tick: nothing for two colonies to collide over.
    let prior =
        if not (ObserveMemory.cpuLineStands ()) then
            Observe.CpuState.empty
        else
            match cpuLine with
            | Some line -> line
            | None -> ObserveMemory.loadCpu ()

    let line = Observe.foldCpu Observe.capCpuTicks Game.time readings prior

    // The tick after a global reset writes the line whole, and every tick
    // after it appends: only a re-encode normalises a phase group this bundle
    // no longer reads whole (`decodeCpuPhases` answers `None` for a row short
    // a key, and `observe.mjs cpu` refuses such a row).
    match cpuLine with
    | Some _ -> ObserveMemory.appendCpu line
    | None -> ObserveMemory.saveCpu line

    cpuLine <- Some line

/// Absent on the sim room and the shared-VM runtimes
/// (`docs/research/engine-testing.md`); 0 there reads as unmeasured. One
/// call: each crosses the isolate boundary.
let private heapReading () =
    if isNull (box Game.cpu?getHeapStatistics) then
        0.0, 0.0
    else
        let h = Game.cpu.getHeapStatistics ()
        h.used_heap_size / 1048576.0, h.externally_allocated_size / 1048576.0

let private memoRows () =
    planMemos
    |> Map.toList
    |> List.sumBy (fun (_, memo) ->
        memo.Walks.Count
        + memo.SeamWalks.Count
        + memo.FarFields.Count
        + memo.Narrowed.Count)

/// How many intents the engine took: it charges 0.2 CPU per intent it
/// executes, so an error code and a missing actor are both counted out.
let private acceptedOf (outcomes: (Intent * Executor.Outcome) list) =
    outcomes
    |> List.sumBy (fun (_, outcome) ->
        match outcome with
        | Executor.Ok -> 1
        | Executor.Failed _
        | Executor.ActorMissing -> 0)

/// The light tick: the last full tick's work and step plans replayed off the
/// glance, the positions leaf, the CPU line, and nothing else.
let private lightTick (atEntry: float) (last: LightTick.LastFull) (seen: LightTick.Glance) =
    let plan =
        LightTick.intents last seen
        |> Fabot.Core.IntentPlan.create
        |> function
            | Ok plan -> plan
            | Error conflict -> invalidOp $"Conflicting creep intents: %A{conflict}"

    // The glance and the replay are this tick's decision; nothing was swept
    // or projected, so those phases are zero-width.
    let atDecide = Game.cpu.getUsed ()
    // `Moved` keeps meaning "moved since the last tick".
    World.positions () |> ObserveMemory.savePositions
    let atSave = Game.cpu.getUsed ()
    let accepted = Executor.run plan |> acceptedOf
    let heap, external = heapReading ()

    writeCpuLine
        {
            AtEntry = atEntry
            AtSnapshot = atEntry
            AtDecide = atDecide
            AtSave = atSave
            AtExecute = Game.cpu.getUsed ()
            Intents = accepted
            Bucket = Game.cpu.bucket
            Replans = 0
            ColonyDecides = []
            ColonyFloods = []
            HeapMb = heap
            ExternalMb = external
            MemoRows = memoRows ()
            RoomSnapshots = []
            AtRooms = atEntry
            AtProjects = atEntry
            ColonyProjects = []
            Light = true
            Forced = None
        }

/// The global harassment list, priced once: the declarations and the bank a
/// colony needs to cast one.
let private harassment: Harassment =
    {
        Rooms = Colony.harass
        Floor = Bodies.harassFloor Tuning.defaults
    }

/// The full tick: the World, every colony's view and decision, the observe
/// folds and every Memory leaf, and the record the light tick after it
/// replays.
let private fullTick
    (atEntry: float)
    (seen: LightTick.Glance option)
    (forced: LightTick.LightForce option)
    =
    // The tick's World, read out of the engine once, with the previous tick's
    // sightings laid under it.
    let world =
        World.ofGame
            Tuning.defaults.MaxHops
            Colony.declared
            harassment
            (ObserveMemory.loadPositions ())
        |> World.recalling sightings

    sightings <- world.Sightings

    let colonies = World.living Colony.declared world

    // Each colony's Raid log is read before any view is cut, because the
    // stand-down gate decides which rooms a colony works at all. One read and
    // not two: a second `loadRaids` after `decide` could answer differently (a
    // hand-edit through the Memory HTTP API lands between them) and the tick
    // would be decided against one log and recorded against another.
    let raids =
        colonies
        |> List.map (fun colony -> colony.Home, ObserveMemory.loadRaids colony.Home)
        |> Map.ofList

    // The gate's answer per colony, derived once: a second derivation would
    // be a second answer free to disagree.
    let gates =
        raids |> Map.map (fun _ log -> Observe.standDown Tuning.defaults world.Time log)

    let gateOf home =
        gates |> Map.tryFind home |> Option.defaultValue StandDown.none

    // Which colony casts each harassment room, decided once for every
    // colony's reading below (#434).
    let casting =
        World.harassCasters joins Tuning.defaults Colony.declared harassment world

    // Every creep filed under the colony that holds it this tick, cut once
    // and handed to each view: two colonies holding one creep would write two
    // Tasks into the one flat `assignments` leaf. An argument to the view and
    // not a field of the World because the rule needs the stand-down gate,
    // which is Memory's answer. Keyed on the rooms withdrawn from, not the
    // rooms looked into: a latched room is projected by nobody this tick.
    let holders =
        World.creepColoniesRecalling
            joins
            Tuning.defaults
            Colony.declared
            casting
            colonies
            (gates |> Map.map (fun _ gate -> gate.Shut))
            world

    // One view per living colony, cut from the one world by `ColonyView.ofWorld`
    // (under test in `ViewTests`). The reading here is the boundary the first
    // projection is differenced against; what stands between the last room
    // and it is the world's own tail (sightings, creep list, Raid logs).
    let atProjects = Game.cpu.getUsed ()
    let mutable projectedAt = []

    let views =
        colonies
        |> List.map (fun colony ->
            let view =
                ColonyView.ofWorldRecalling
                    joins
                    Tuning.defaults
                    Colony.declared
                    casting
                    (gateOf colony.Home)
                    holders
                    world
                    colony

            // The counter after each colony's projection, as `decide` reads it
            // after each decision: the `snapshot` column turned out to be
            // mostly this (7.63 ms of it stood after the last room was swept,
            // #370), and it is ours to cut, unlike the engine's `find` sweeps.
            projectedAt <- (colony.Home, Game.cpu.getUsed ()) :: projectedAt
            colony, view)

    // The projection boundary. The Raid logs' reads ride in this phase, not
    // the prelude: the gate and the cut are one act, and splitting them would
    // price a `find` sweep against a Memory read.
    let atSnapshot = Game.cpu.getUsed ()
    // Both flat and keyed by creep name, read once and handed to every colony;
    // what a colony is handed for a creep it does not hold, the Matcher drops.
    let assignments = loadAssignments ()
    let verbose = ObserveMemory.loadVerbose ()

    // Each colony decides its own tick with its movement left unarbitrated: a
    // room two colonies both work is one room, and half its traffic arbitrated
    // against the other half read as empty is how a mother's pioneer came to
    // claim the child's anchor's tile (#220).
    //
    // One colony re-plans per tick, round-robin (#357): the tick that re-planned
    // all four at once cost 487 ms of the engine's 500, a re-planning tick
    // averages 209 ms against a mean of 84, and they arrive together because a
    // global reset empties every memo in the same tick. The turn is
    // `Game.time % count` and not "whoever is stalest" because the alternative
    // asks this shell for a census signature that is the decision layer's own.
    // A colony that needs no re-plan passes its turn; one colony alone is
    // always its own turn, so a one-colony world is unchanged. The count is
    // of full ticks (`fullTicks`): a light tick decides nothing.
    //
    // The reset tick itself is nobody's turn (#442): every memo is empty, every
    // colony prices from empty tables under a cold bundle, and the one
    // re-plan on top of that read 496 ms. Each colony serves
    // `PlanMemo.deferred` for that tick, and the turns start on the next.
    let resetTick = Map.isEmpty planMemos
    let counted = fullTicks
    fullTicks <- fullTicks + 1

    let turn =
        if List.isEmpty views || resetTick then
            -1
        else
            counted % List.length views

    // The decision is bound and then tupled, and the turn is a DU
    // (`ReplanTurn`), because the first shape shipped broken: as an expression
    // inside the tuple with a `bool` last argument, the turn arrived as
    // JavaScript `undefined`, `not undefined` is `true`, and no layout was
    // planned at all (#357). Only `npm run profile` caught it.
    // The counter is read after each colony's decision, cumulative, so the CPU
    // line can say which colony a spike came out of; `foldCpu` differences.
    let decisions =
        views
        |> List.mapi (fun index (colony, view) ->
            let memo = Map.tryFind colony.Home planMemos

            let whose = if index = turn then ReplanTurn.Now else ReplanTurn.Waiting

            let decision = decideUnarbitrated view assignments verbose memo whose

            // The flood counters at the same boundary, cumulative like the
            // clock beside them.
            let flooded: Observe.FloodCounts =
                {
                    Floods = Grid.Counters.floods
                    Free = Grid.Counters.free
                    Pops = Grid.Counters.pops
                }

            colony, view, decision, Game.cpu.getUsed (), flooded)

    // The one movement pass of the tick: every colony's Move Intents,
    // arbitrated once per room.
    let moveIntents, moveVerdicts =
        resolveRooms (decisions |> List.map (fun (_, _, decision, _, _) -> decision.Movement))

    // The decision boundary; the two Memory reads above are inside it, being
    // `decide`'s arguments.
    let atDecide = Game.cpu.getUsed ()

    // How many colonies paid for a plan this tick, counted against the memo
    // each was handed before the table is overwritten: a `decide` six times
    // its mean is either a replan or a pricing storm, and the line has to tell
    // them apart. Paid, not dropped: the two differ only on the tick after a
    // global reset, the tick most read.
    //
    // Whose turn it was, by the same index the loop above handed `ReplanTurn`
    // on, read here rather than widening the decision tuple further.
    let payingHome =
        views
        |> List.tryItem turn
        |> Option.map (fun (colony: Colony, _) -> colony.Home)

    let replans =
        decisions
        |> List.filter (fun (colony, _, decision, _, _) ->
            match Map.tryFind colony.Home planMemos with
            // A deferred plan keeps the stale signature on purpose, so it
            // compares equal and is not a replan.
            | Some prior -> prior.Signature <> decision.Memo.Signature
            // No prior at all is a global reset, and only the colony whose
            // turn it was paid; the rest were handed `PlanMemo.deferred`.
            // Counting all four here reported the same four re-plans twice.
            | None -> Some colony.Home = payingHome)
        |> List.length

    planMemos <-
        decisions
        |> List.map (fun (colony, _, decision, _, _) -> colony.Home, decision.Memo)
        // Not `Map.ofList` here (#401): its comparer would keep this tick's
        // `decisions`, views and Atlases alive until the next tick's memos.
        |> Fresh.mapOfList

    // The Reactor programme's observation. The room facts answer only while
    // vision does, so a blind tick hands `None` and the last sample stands.
    let reactorReading =
        decisions
        |> List.tryPick (fun (colony, _, decision, _, _) ->
            colony.Errands
            |> List.tryPick (fun errand ->
                let reactorId = fst errand.Target

                (World.roomOf world errand.RoomName).Reactors
                |> List.tryFind (fun reactor -> reactor.Id = reactorId)
                |> Option.map (fun reactor ->
                    let home = World.roomOf world colony.Home

                    let banked =
                        home.TargetKinds
                        |> Map.toSeq
                        |> Seq.tryPick (fun (id, kind) ->
                            if kind = Structure BuiltKind.Storage then
                                Some(Map.tryFind id home.Thorium |> Option.defaultValue 0)
                            else
                                None)
                        |> Option.defaultValue 0

                    let issued =
                        decision.Intents
                        |> List.exists (function
                            | TransferEnergyToStructure(_, target, Thorium) -> target = reactorId
                            | _ -> false)

                    ({
                        Owner = reactor.Owner
                        StoreT = reactor.Thorium
                        ContinuousWork = reactor.ContinuousWork
                        BankedT = banked
                        DeliveryIssued = issued
                    }
                    : Observe.ReactorReading))))

    ObserveMemory.loadReactor ()
    |> Observe.foldReactor Game.time reactorReading
    |> ObserveMemory.saveReactor

    // Memory writes land before the engine calls: a throw inside Executor.run
    // must not discard the tick's anti-thrash state. The colonies' answers are
    // disjoint, so the union is the whole map.
    saveAssignments (
        (Map.empty, decisions)
        ||> List.fold (fun acc (_, _, decision, _, _) ->
            (acc, decision.Assignments)
            ||> Map.fold (fun acc creep task -> Map.add creep task acc))
    )

    // One world-wide aliveness set for the fold, the Raid logs and the memory
    // pruning: a name that left a colony's `Creeps` may merely have been
    // adopted, and only `Game.creeps` says which names stopped existing.
    let living = livingCreeps ()

    // One fold over every colony's Verdicts in colony order, so a creep
    // adopted this tick continues the timeline its caster started.
    let priorLog =
        if not (ObserveMemory.observeLogStands ()) then
            Map.empty
        else
            match observeLog with
            | Some log -> log
            | None -> ObserveMemory.load ()

    let log =
        Observe.fold
            Observe.capPerCreep
            Game.time
            living
            ((decisions |> List.collect (fun (_, _, decision, _, _) -> decision.Verdicts))
             @ moveVerdicts)
            priorLog

    // The tick after a global reset writes the log whole: the rows standing
    // are another bundle's, and only a whole write normalises the leaf to
    // what this bundle reads. Every tick after it writes the changed creeps.
    match observeLog with
    | Some _ -> ObserveMemory.saveChanged priorLog log
    | None -> ObserveMemory.save log

    observeLog <- Some log

    for colony, view, decision, _, _ in decisions do
        // The Raid log, written every tick whether or not the fold changed
        // anything, so the leaf's presence lets `observe.mjs raids` tell "no
        // channel" from "no raids". Folded here and read at the top of the
        // loop: this tick's sightings are what the next tick's gate stands on.
        raids
        |> Map.tryFind colony.Home
        |> Option.defaultValue Observe.RaidState.empty
        |> Observe.foldRaids Observe.capEpisodes living view decision.OutpostRooms
        |> ObserveMemory.saveRaids colony.Home

        // The Layout's channel, written every tick, empty or not. The refused
        // declarations come off the view and not the memo: they are the
        // declaration's loss, not the plan's.
        ObserveMemory.saveLayout
            colony.Home
            decision.Memo.UnservedFootings
            decision.Memo.UnroutedTrunks
            decision.Memo.DeferredContainers
            view.Refused

        // The cascade's own numbers, for `observe.mjs quotas`.
        ObserveMemory.saveQuotas colony.Home decision.Quotas

        // The breach log: the live invariant checks, off the same view this
        // tick decided from. The suite runs on fixtures this repo authors, so
        // it confirms the code's belief about the projection; an assertion
        // read off the real projection every tick is the feedback loop that
        // could have caught the `World.fs` incidents (#278). Written every
        // tick, empty or not.
        ObserveMemory.loadBreaches colony.Home
        |> Observe.foldBreaches Observe.capBreaches Game.time view
        |> ObserveMemory.saveBreaches colony.Home

    pruneDeadCreepMemory living

    // Where every creep of ours stood this tick, for next tick's
    // `CreepInfo.Moved`.
    World.positions () |> ObserveMemory.savePositions
    // The Memory boundary: everything this tick persists except the CPU
    // line's own leaf, which is written after the last reading and so is the
    // one write the line never prices. The phase holds the observe folds and
    // the `Game.creeps` sweep as well as the writes.
    let atSave = Game.cpu.getUsed ()

    let executionPlan =
        (decisions |> List.collect (fun (_, _, decision, _, _) -> decision.Intents))
        @ moveIntents
        |> Fabot.Core.IntentPlan.create
        |> function
            | Ok plan -> plan
            | Error conflict -> invalidOp $"Conflicting creep intents: %A{conflict}"

    let accepted = Executor.run executionPlan |> acceptedOf

    // The record the next tick replays if it is light: where each creep stood
    // as this tick began, what every colony issued, and the step plans.
    // None with the cadence off (`Tuning.LightTicks`): no glance was taken.
    lastFull <-
        seen
        |> Option.map (fun seen ->
            Game.time,
            LightTick.lastFull
                seen
                (decisions
                 |> Seq.collect (fun (_, _, decision, _, _) -> Map.toSeq decision.Steps)
                 |> Fresh.mapOfSeq)
                (executionPlan |> Fabot.Core.IntentPlan.intents))

    // The CPU line: measured, never budgeted; nothing in the bot reads it
    // back. The tick's total is the last reading, after the Executor, because
    // the intents are most of what a tick costs.
    let heap, external = heapReading ()

    writeCpuLine
        {
            AtEntry = atEntry
            AtSnapshot = atSnapshot
            AtDecide = atDecide
            AtSave = atSave
            AtExecute = Game.cpu.getUsed ()
            Intents = accepted
            Bucket = Game.cpu.bucket
            Replans = replans
            ColonyDecides = decisions |> List.map (fun (colony, _, _, at, _) -> colony.Home, at)
            ColonyFloods =
                decisions |> List.map (fun (colony, _, _, _, flooded) -> colony.Home, flooded)
            HeapMb = heap
            ExternalMb = external
            MemoRows = memoRows ()
            // Off `World`'s own heap slot, not the world record: a measurement
            // of the shell is not a fact about the game.
            RoomSnapshots = World.roomCosts
            AtRooms = World.roomsBegan
            AtProjects = atProjects
            ColonyProjects = List.rev projectedAt
            Light = false
            Forced = forced |> Option.map LightTick.tag
        }

// Exported as `loop` on the bundled `main` module; the engine calls it every tick.
let loop () =
    // The engine's counter is already running when `loop` is entered: this is
    // the engine's prelude alone. The Memory parse is not in it: `Memory`
    // deserializes on first touch, which is `loadRaids` on a full tick, so the
    // parse is charged to the `snapshot` phase. Every reading is taken every
    // tick; a measurement that switches itself off has absences to explain.
    let atEntry = Game.cpu.getUsed ()
    // The flood counters start the tick at zero, so each colony's reading
    // below is cumulative from here and `foldCpu` can difference it.
    Grid.Counters.reset ()

    // ADR-0082
    // Light only straight after a full tick, so never two in a row and never
    // on the tick after a reset, and only when nothing the glance reads says
    // the replay would be wrong. With the cadence off no glance is taken at
    // all, so the flag gives back today's tick and its cost exactly.
    if not Tuning.defaults.LightTicks then
        fullTick atEntry None None
    else
        let seen = glance ()

        match lastFull with
        | Some(tick, last) when tick = Game.time - 1 ->
            match LightTick.forced last seen with
            | None -> lightTick atEntry last seen
            | forced -> fullTick atEntry (Some seen) forced
        | _ -> fullTick atEntry (Some seen) None
