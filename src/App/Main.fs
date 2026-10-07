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
// colony decides as it did before the grace existed, but for each rival
// room's owner, which `roomLatches` below brings back.
let mutable private sightings: Map<string, RoomSighting> = Map.empty

// The homes a tower of ours has stood full in (`World.Towered`, #445): heap
// state for the same reason.
let mutable private towered: Set<string> = Set.empty

// Each room's armed Threats as last seen, or the exit they left by
// (`World.ExitWatches`, #450): heap state written into the world danger log
// every full tick and seeded off it on a cold heap (#542).
let mutable private exitWatches: Map<string, ExitWatch> = Map.empty

// Each room's raid healers of the last few ticks (`World.Healers`, #480):
// heap only, a reset forgetting a handful of ticks.
let mutable private healers: Map<string, HealerSeen list> = Map.empty

// Each hostile in sight and the tile it left (`World.HostileSteps`, #551):
// heap only, a reset forgetting one step.
let mutable private hostileSteps: Map<string, HostileStep> = Map.empty

// Each room's raid as last seen (`World.Raiders`, #569): heap only, a reset
// falling back on the danger log's threat memory.
let mutable private raiders: Map<string, RaidSeen> = Map.empty

// The last tick an armed rival stood in each room (`World.ArmedSeen`, #485):
// heap state seeded off the `rooms` leaf on a cold heap, so the deploy before
// a walk does not forget the room the walk goes round.
let mutable private armedSeen: Map<string, int> = Map.empty

// Every rival controller as last seen (`World.RivalControllers`, #489): heap
// state seeded off the `rooms` leaf on a cold heap, so a safe mode seen
// before a deploy still refuses its owner's other rooms after it.
let mutable private rivalControllers: Map<string, RivalController> = Map.empty

// The two of those a reset must not forget, as last written to the `rooms`
// leaf: who owns each rival room (#444) and the tower latch. None on a cold
// heap, which seeds `sightings` and `towered` off the leaf; written back
// only on the tick either moves.
let mutable private roomLatches: ObserveMemory.RoomLatches option = None

// The CPU line on the heap: restored off its segments only when the heap
// holds none (a global reset), and None until they are read. The heap saves
// decoding and re-encoding the hundred standing rows to add one
// (`ObserveMemory.appendCpu`).
let mutable private cpuLine: Observe.CpuState option = None

// Whether this heap has written the reset tick's `version` leaf and dropped
// the probe's sums.
let mutable private announced = false

// Which ordered room pairs a Seam band joins, on the heap and emptied only
// by a reset or the console (#395):
// every answer is the terrain's, and the one moving input (whether the world
// holds both rooms) is read ahead of the table (`World.linkedRecalling`).
let private joins = JoinTable()

// The Transition log on the heap, on the CPU line's terms: folded on the
// heap and encoded one changed creep at a time (`ObserveMemory.saveChanged`).
let mutable private observeLog: Observe.ObserveState option = None

[<Emit("globalThis")>]
let private globalThis: obj = jsNative

// The change this bundle was built from, off the banner `scripts/bundle.mjs`
// writes; a bundle built any other way (the tests, the harness off
// `build/fable`) has none.
[<Emit("(typeof __FABOT_VERSION__ === 'string' ? __FABOT_VERSION__ : 'unbuilt')")>]
let private bannerVersion: string = jsNative

// Read once, at module load: once per global reset.
let private version = bannerVersion

// The bundle's fable-library line ranges, [first, last], off the banner's
// second line (`scripts/bundle.mjs`, #628); none in a bundle built any
// other way.
[<Emit("(typeof __FABOT_LIBRARY_LINES__ === 'object' ? __FABOT_LIBRARY_LINES__ : [])")>]
let private libraryLines: int[][] = jsNative

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
                    "floods" ==> memo.CreepFloods.Floods.Count
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
                "version" ==> version
                "sizes" ==> sizes
                "wire" ==> ObserveMemory.wires
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

// The plans' record and the tick every colony last decided on together (the
// cadence's), on the heap: a reset empties it and the next tick is full.
let mutable private lastFull: (int * LightTick.LastFull) option = None

// What each colony's last whole decide left its fight-only ticks of its
// economy (`Decision.Economy`): heap only, a reset deciding whole.
let mutable private economyCarries: Map<string, EconomyCarry> = Map.empty

// Ticks since the cadence's full one that replayed a plan (0 on it): light
// ticks, and forced ticks that carried a colony (#549) or a colony's economy.
let mutable private lightRun = 0

// The cadence the bucket asked for last tick (`LightTick.bucketCadence`),
// and the tuning the colonies decide under with this tick's cadence in it,
// which lays their later acts (`Decision.Later`).
let mutable private bucketHeld = Tuning.defaults.Cadence
let mutable private tuning = Tuning.defaults

// The fight-only switch in effect, the tuned one or its live override, and
// which (`ObserveMemory.saveSwitches`).
let private fightOnlyNow () =
    LightTick.switchFrom Tuning.defaults.FightOnly (ObserveMemory.loadFightOnly ())

// Full ticks run since the reset: the replan turn counts these, not
// `Game.time`, which with light ticks between would hand an even count of
// colonies half the turns and the other half none.
let mutable private fullTicks = 0

// Each colony's run of consecutive faulted decides (`LightTick.faultRuns`,
// #628): only the first fault of a run forces the next tick full.
let mutable private faultRuns: Map<string, int> = Map.empty

// The tick this heap first ran: the bundle's load, written as
// `observe.version.reset` and the line the `Fault` breaches read the fault
// ring from (#630), as `observe health` does.
let mutable private loadedAt = -1

// The breaches open as the last full tick left them, every colony's and the
// world's: what the room visuals draw on every tick between.
let mutable private openBreaches: Observe.Breach list = []

// The engine's refusals of our acts (#629), folded off every tick's
// outcomes, light and full, and laid into the next full tick's World. Heap
// only: a reset forgets what the next refused act teaches again.
let mutable private refusals: Refusal list = []

// The Tasks the last full tick saved: what a light tick's refused body was
// holding, read without the Memory leaf.
let mutable private heldTasks: Assignments = Map.empty

// The spawns whose last cast the engine refused (#629): a spawn leaves it on
// the next cast the engine takes. Only a full tick casts.
let mutable private castRefused: Set<string> = Set.empty

/// One replay's refusals folded into the heap's (`Refusal.record`), each
/// body's under the Task it holds and each structure's under the colony
/// whose intents (`issued`, by home, read only on a refusal) hold its act;
/// an act none holds is filed under no colony and decays unread.
let private recordRefusals
    (issued: Lazy<(string * Intent list) list>)
    (outcomes: (Intent * Executor.Outcome) list)
    =
    let issuedBy intent =
        issued.Value
        |> List.tryPick (fun (home, intents) ->
            if List.contains intent intents then Some home else None)
        |> Option.defaultValue ""

    match
        Executor.refusals
            (fun name -> Map.tryFind name heldTasks |> Option.defaultValue "")
            issuedBy
            Game.time
            outcomes
    with
    | [] -> ()
    | refused -> refusals <- Refusal.record tuning.RefusalHalfLife Game.time refused refusals

/// Each colony's replayable acts in the last full tick's record, by home:
/// what issued a replayed act.
let private planned (last: LightTick.LastFull) : (string * Intent list) list =
    last.Plans
    |> Map.toList
    |> List.map (fun (home, plan) -> home, plan.Acts @ List.concat plan.Later)

/// A creep's gait off `Game` (`Gait.walked`).
let private gaitOf (c: ICreep) : Gait =
    let mutable weighed = 0
    let mutable moves = 0
    let mutable carry = 0

    for part in c.body do
        match part.``type`` with
        | "move" ->
            if part.hits > 0 then
                moves <- moves + 1
        | "carry" ->
            if part.hits > 0 then
                carry <- carry + 1
        | _ -> weighed <- weighed + 1

    Gait.walked weighed carry moves (if carry = 0 then 0 else storeUsedTotal c.store)

/// What this tick sees of `Game` without a World: our creeps, every hostile
/// in a visible room, our structures' tiles and our controllers. Built with
/// `Fresh`, because the full tick keeps it as the last full tick's record.
let private glance () : LightTick.Glance =
    let tileOf (room: string) (pos: IRoomPosition) =
        RoomPos.at room { X = pos.x; Y = pos.y }

    let rooms = objectValues<IRoom> Game.rooms

    // A chase's pace is asked of a body the last plans aim (`LightTick.aimers`).
    let aimers =
        match lastFull with
        | Some(_, last) -> LightTick.aimers last
        | None -> Set.empty

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
                    Inward =
                        LightTick.inward
                            (World.groundWalkableAt c.room.name)
                            { X = c.pos.x; Y = c.pos.y }
                    Heals =
                        c.body
                        |> Array.sumBy (fun part ->
                            if part.``type`` = "heal" && part.hits > 0 then 1 else 0)
                    Strikes =
                        c.body
                        |> Array.sumBy (fun part ->
                            if
                                (part.``type`` = "attack" || part.``type`` = "ranged_attack")
                                && part.hits > 0
                            then
                                1
                            else
                                0)
                    Gait = if Set.contains c.name aimers then Some(gaitOf c) else None
                }
                : LightTick.GlanceCreep))
            |> Fresh.mapOfArray
        Hostiles =
            [
                for room in rooms do
                    // Each read beside the room's other hostiles
                    // (`LightTick.glanceOf`).
                    let side =
                        room.find findHostileCreeps
                        |> Array.map (fun o ->
                            let hostile = unbox<ICreep> o

                            ({
                                Id = hostile.id
                                Owner =
                                    if isNull (box hostile.owner) then
                                        ""
                                    else
                                        hostile.owner.username
                                Pos = tileOf room.name hostile.pos
                                Body =
                                    hostile.body
                                    |> Array.map (fun part -> World.bodyPartOf part.``type``)
                                    |> List.ofArray
                                Boosts = World.boostsOf hostile.body
                                Hits = hostile.hits
                                TicksToLive = hostile.ticksToLive
                            }
                            : HostileInfo))
                        |> List.ofArray

                    for hostile in side do
                        yield LightTick.glanceOf side hostile
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
        Time = Game.time
        // Asked only in a room a volley stands in, mostly none (#537).
        PowerCreeps =
            match lastFull with
            | None -> []
            | Some(_, last) ->
                [
                    for room in LightTick.volleyRooms last do
                        let seen = objectItem<IRoom> Game.rooms room

                        if not (isNull (box seen)) then
                            for o in seen.find findHostilePowerCreeps do
                                let c = unbox<ICreep> o

                                if not (Colony.strikesAnywhere c.owner.username) then
                                    yield tileOf room c.pos
                ]
        // A light tick's alone (`standingOf`): a forced tick never asks.
        Objects = Fresh.setOfSeq Seq.empty
    }

/// Of the objects the last full tick's acts name (`LightTick.lookups`), the
/// ones `Game` still holds: the glance's `Objects` on a light tick.
let private standingOf (last: LightTick.LastFull) : Set<string> =
    LightTick.lookups last
    |> Seq.filter (fun id -> not (isNull (Game.getObjectById id)))
    |> Fresh.setOfSeq

// The harness's fake `Game` has no `RoomVisual`.
[<Emit("typeof RoomVisual === 'function' && new RoomVisual($0).text($1, 1, $2, { align: 'left', font: 0.6, color: '#ffffff', stroke: '#000000', strokeWidth: 0.12 })")>]
let private visualText (_room: string) (_text: string) (_y: float) : unit = jsNative

/// The bucket and the tick's cost in the top-left corner of every room we
/// own (#638): the season client shows no bucket, and a room visual reaches
/// only our own client.
let private drawCpu (line: Observe.CpuState) =
    let mean =
        if List.isEmpty line.Ticks then
            0.0
        else
            line.Ticks |> List.averageBy (fun s -> s.Ms)

    let bucket = $"bucket %d{Game.cpu.bucket}"

    let cost =
        match line.Ticks |> List.tryFind (fun s -> s.Tick = Game.time) with
        | Some s ->
            let kind =
                if s.Light then "light"
                elif s.Forced.IsSome then "forced"
                else "full"

            $"cpu %.1f{s.Ms} ms %s{kind}, mean %.1f{mean} of %d{int Game.cpu?limit}"
        | None -> $"cpu mean %.1f{mean} of %d{int Game.cpu?limit}"

    // The open breaches (#630), a line each: the world's under the bucket in
    // every room we own, and each colony's in the room it names.
    let world, placed = openBreaches |> List.partition (fun b -> b.Room = "")

    let draw room (top: float) (rows: Observe.Breach list) =
        rows
        |> List.iteri (fun i b ->
            visualText room $"breach {b.Kind} {b.Subject} {b.Amount}" (top + 0.8 * float i))

    let worldTop = 2.6
    let mutable owned = Set.empty

    for room in objectValues<IRoom> Game.rooms do
        let c = room.controller

        if not (isNull (box c)) && c.my then
            owned <- Set.add room.name owned
            visualText room.name bucket 1.0
            visualText room.name cost 1.8
            draw room.name worldTop world

    for room, rows in List.groupBy (fun (b: Observe.Breach) -> b.Room) placed do
        let top =
            if Set.contains room owned then
                worldTop + 0.8 * float (List.length world)
            else
                1.0

        draw room top rows

/// The CPU line's write, the same on both kinds of tick: nothing in the bot
/// reads it back.
let private writeCpuLine (readings: Observe.CpuReadings) =
    // One flat leaf keyed by tick: nothing for two colonies to collide over.
    let prior =
        match cpuLine with
        | Some line -> Some line
        | None -> ObserveMemory.restoreCpu ()

    let line =
        Observe.foldCpu
            Observe.capCpuTicks
            Game.time
            readings
            (prior |> Option.defaultValue Observe.CpuState.empty)

    // The tick after a global reset encodes the line whole, and every tick
    // after it appends: only a re-encode normalises a phase group this bundle
    // no longer reads whole (`decodeCpuPhases` answers `None` for a row short
    // a key, and `observe.mjs cpu` refuses such a row). A line folded onto
    // empty while the segments are not read yet is drawn and not kept.
    match cpuLine, prior with
    | Some _, _ -> ObserveMemory.appendCpu line
    | None, Some _ -> ObserveMemory.saveCpu line
    | None, None -> ()

    if prior.IsSome then
        cpuLine <- Some line

    if not announced then
        ObserveMemory.dropProbe ()
        ObserveMemory.saveVersion version loadedAt
        announced <- true

    drawCpu line

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
        | Executor.ActorMissing
        | Executor.Threw _ -> 0)

/// How much of a fault's message the ring keeps: a `%A` of a conflict can
/// run to kilobytes.
let private faultMessageCap = 300

/// Whether a stack frame's line (`…:line:column`) is fable-library's: the
/// frames worth keeping are the bot's own, which name the step that threw.
let private inLibrary (frame: string) =
    // The bundle's own lines alone: the module is `main` live, `main.js` in
    // the harnesses.
    let at =
        System.Text.RegularExpressions.Regex.Match(frame, @"\bmain(?:\.js)?:(\d+):\d+\)?$")

    at.Success
    && (let line = int at.Groups.[1].Value

        libraryLines
        |> Array.exists (fun range -> range.[0] <= line && line <= range.[1]))

/// A caught throw as the fault ring records it (#628): its message and the
/// top three of the bot's own stack frames. The bundle is not minified, so a
/// frame names the Fable function it threw in and no source map is needed.
/// A JavaScript throw need not be an Error: a thrown value with no message
/// is recorded as its text.
let private faultRow (colony: string) (phase: string) (error: exn) : ObserveMemory.FaultRow =
    let thrown = box error
    let stack: obj = if isNull thrown then null else thrown?stack
    let message: obj = if isNull thrown then null else thrown?message

    {
        Tick = Game.time
        Colony = colony
        Phase = phase
        Message =
            let text =
                if jsTypeof message = "string" then
                    unbox<string> message
                else
                    string thrown

            if text.Length > faultMessageCap then
                text.Substring(0, faultMessageCap) + "…"
            else
                text
        Frames =
            if jsTypeof stack = "string" then
                (unbox<string> stack).Split('\n')
                |> Array.map _.Trim()
                |> Array.filter (fun frame -> frame.StartsWith "at " && not (inLibrary frame))
                |> Array.truncate 3
                |> Array.toList
            else
                []
    }

/// Faults written to the ring and to the console, which the engine no
/// longer prints for a throw the bot caught.
let private recordFaults (rows: ObserveMemory.FaultRow list) =
    for row in rows do
        let frames = String.concat " < " row.Frames
        JS.console.log $"fault {row.Phase} {row.Colony}: {row.Message} {frames}"

    ObserveMemory.recordFaults rows

/// Each Intent whose engine call threw, as a fault of the execute.
let private thrownOf (outcomes: (Intent * Executor.Outcome) list) =
    outcomes
    |> List.choose (fun (intent, outcome) ->
        match outcome with
        | Executor.Threw error ->
            let row = faultRow "" "execute" error

            Some
                { row with
                    Message = $"%A{intent}: {row.Message}"
                }
        | Executor.Ok
        | Executor.Failed _
        | Executor.ActorMissing -> None)

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
    // `CreepInfo.Walking` keeps reading "moved since the last tick".
    World.positions () |> ObserveMemory.savePositions
    let atSave = Game.cpu.getUsed ()
    let outcomes = Executor.runQuiet plan
    recordFaults (thrownOf outcomes)
    recordRefusals (lazy (planned last)) outcomes
    let accepted = acceptedOf outcomes
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
            Moved = []
        }

/// The global harassment list, priced once: the declarations and the ranger
/// block a room's floor is counted in.
let private harassment: Harassment =
    {
        Rooms = Colony.harass
        BlockCost = Bodies.rangerBlockCost
    }

/// The full tick: the World, every colony's view and decision, the observe
/// folds and every Memory leaf, and the record the light tick after it
/// replays. `forcedBy`, a would-be light tick's cadence tick, record and the
/// facts that forced it: only their colonies decide, the rest replay
/// (`LightTick.deciders`). True when a colony was replayed.
let private fullTick
    (atEntry: float)
    (seen: LightTick.Glance option)
    (forcedBy: (int * LightTick.LastFull * LightTick.LightForce list) option)
    : bool =
    // The world danger log's rooms, read once a tick off Memory as each
    // colony's own log is: every colony's gate stands on them.
    let dangerRooms = ObserveMemory.loadDanger ()

    // A cold heap reads back what the last one wrote, so a rival's dark room
    // stays unentered, a weaned child stays weaned and a held exit stays held
    // across the reset.
    let written =
        match roomLatches with
        | Some latches -> latches
        | None ->
            let loaded = ObserveMemory.loadRoomLatches ()
            sightings <- World.seedRivals loaded.Rivals sightings
            towered <- Set.union towered loaded.Towered
            armedSeen <- loaded.Armed
            rivalControllers <- loaded.Controllers
            exitWatches <- Observe.Danger.watches dangerRooms
            loaded

    let danger =
        { dangerRooms with
            Rivals = written.Latched
        }

    // The tick's World, read out of the engine once, with the previous tick's
    // sightings, tower latch, exit watch and healer memory laid under it.
    let world =
        World.ofGame
            Tuning.defaults
            Colony.declared
            harassment
            (ObserveMemory.loadPositions ())
            (match lastFull with
             | Some(_, last) -> LightTick.walkers last
             | None -> Map.empty)
            (ObserveMemory.loadDrawn ())
        |> World.recalling sightings
        |> World.latchTowers Tuning.defaults towered
        |> World.watchExits Tuning.defaults exitWatches
        |> World.recallHealers Tuning.defaults healers
        |> World.recallHostiles hostileSteps
        |> World.recallRaiders raiders
        |> World.recallArmed Tuning.defaults armedSeen
        |> World.recallRivalControllers rivalControllers
        |> fun world -> { world with Refusals = refusals }

    sightings <- world.Sightings
    towered <- world.Towered
    exitWatches <- world.ExitWatches
    healers <- world.Healers
    hostileSteps <- world.HostileSteps
    raiders <- world.Raiders
    armedSeen <- world.ArmedSeen
    rivalControllers <- world.RivalControllers

    // Written at the tick's end, once the Raid-log folds have moved the
    // latches.
    let latches: ObserveMemory.RoomLatches =
        {
            Rivals = World.rivalOwners world
            Latched = written.Latched
            Towered = world.Towered
            Armed = world.ArmedSeen
            Controllers = world.RivalControllers
        }

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
    // be a second answer free to disagree. Each the colony's projection of
    // the one world danger log.
    let laid = raids |> Map.map (fun _ log -> Observe.Danger.into danger log)

    let gates =
        laid |> Map.map (fun _ log -> Observe.standDown Tuning.defaults world.Time log)

    let gateOf home =
        gates |> Map.tryFind home |> Option.defaultValue StandDown.none

    // Both flat and keyed by creep name, read once and handed to every colony;
    // what a colony is handed for a creep it does not hold, the Matcher drops.
    let assignments = loadAssignments ()

    // Which colony casts each harassment room, decided once for every
    // colony's reading below (#434); a fighter held to a room's Guard keeps
    // its caster.
    let casting =
        World.harassCasters joins Tuning.defaults Colony.declared harassment assignments world

    // Every creep filed under the colony that holds it this tick, cut once
    // and handed to each view: two colonies holding one creep would write two
    // Tasks into the one flat `assignments` leaf. An argument to the view and
    // not a field of the World because the rule needs the stand-down gate,
    // which is Memory's answer. Keyed on the rooms withdrawn from, not the
    // rooms looked into: a latched room is projected by nobody this tick.
    let shut = gates |> Map.map (fun _ gate -> gate.Shut)

    let holders =
        World.creepColoniesRecalling
            joins
            Tuning.defaults
            Colony.declared
            casting
            colonies
            shut
            world

    // One view per living colony, cut from the one world by `ColonyView.ofWorld`
    // (under test in `ViewTests`). The reading here is the boundary the first
    // projection is differenced against; what stands between the last room
    // and it is the world's own tail (sightings, creep list, Raid logs).
    let atProjects = Game.cpu.getUsed ()

    // What each colony's cut and projection spent, in colony order: the
    // `snapshot` column turned out to be mostly this (7.63 ms of it stood
    // after the last room was swept, #370), and it is ours to cut, unlike
    // the engine's `find` sweeps.
    let mutable projectSpent: Map<string, float> = Map.empty

    let timed (home: string) (step: unit -> 'a) : 'a =
        let at = Game.cpu.getUsed ()
        let result = step ()

        projectSpent <-
            Map.add
                home
                ((Map.tryFind home projectSpent |> Option.defaultValue 0.0)
                 + (Game.cpu.getUsed () - at))
                projectSpent

        result

    // The world layer's read, once for every colony (`WorldView`): fresh each
    // tick, and its danger laid in once the views are cut (`WorldDanger`).
    let watch =
        WorldView.ofWorld Colony.declared (gates |> Map.toList |> List.map snd) world

    // Each colony's cut (`ColonyView.cutOf`): the rooms it projects, which
    // the forced tick's deciders and the reset split read before any view is
    // projected.
    let cuts =
        colonies
        |> List.map (fun colony ->
            colony,
            timed colony.Home (fun () ->
                ColonyView.cutOf
                    joins
                    tuning
                    Colony.declared
                    casting
                    (gateOf colony.Home)
                    holders
                    isFighterBody
                    watch
                    world
                    colony))

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
    // of full ticks the turn's colony decided on (`fullTicks`): a light tick
    // decides nothing, and a colony a forced tick replays keeps its turn.
    //
    // The reset tick itself is nobody's turn (#442): every memo is empty, every
    // colony prices from empty tables under a cold bundle, and the one
    // re-plan on top of that read 496 ms. Each colony serves
    // `PlanMemo.deferred` for that tick, and the turns start on the next.
    let resetTick = Map.isEmpty planMemos

    // Each colony with the rooms it answers for, as the reset split and the
    // forced tick's deciders read them.
    let answering () =
        cuts
        |> List.map (fun (colony, cut) ->
            colony.Home,
            ColonyView.cutRooms cut |> Set.ofList |> Set.union (Colony.declaredRooms colony))

    // A forced tick decides the colonies its facts are theirs (#549); the
    // others replay their plans as a light tick would, issue nothing else and
    // keep every Memory leaf they own as written. The cadence bounds them: a
    // colony is replayed only on a tick the light cadence would replay it.
    //
    // And of the deciding colonies, each whose every fact is a fight's
    // decides its fight alone and replays its economy
    // (`LightTick.fightOnly`), while the switch stands (`Tuning.FightOnly`, or
    // its live override) and it has a memo and a whole decide's carry to
    // read: `Entry.decideColonies` opens its fight alone, and hands it back
    // to a whole decide when the economy would decide otherwise.
    let replaying, fightCandidates =
        match forcedBy, seen with
        | Some(_, last, reasons), Some seen when not resetTick ->
            let colonies = answering ()
            let deciding = LightTick.deciders colonies holders last seen reasons

            let fighters = cuts |> List.collect (snd >> ColonyView.cutFighters) |> Set.ofList

            let alone =
                if fst (fightOnlyNow ()) then
                    LightTick.fightOnly
                        (fun name -> Set.contains name fighters)
                        colonies
                        holders
                        last
                        seen
                        reasons
                    |> Set.filter (fun home ->
                        Map.containsKey home planMemos && Map.containsKey home economyCarries)
                else
                    Set.empty

            cuts
            |> List.map (fun (colony, _) -> colony.Home)
            |> List.filter (fun home -> not (Set.contains home deciding))
            |> Set.ofList,
            alone
        | _ -> Set.empty, Set.empty

    // Nor does the reset tick decide every colony (#488): the threatened ones
    // and the rest up to `ResetFirstShare` decide, and the others issue
    // nothing, keep every Memory leaf they own as written, and decide on the
    // next tick, which `LastFull.Deferred` makes full.
    let waiting =
        if resetTick then
            answering ()
            |> LightTick.resetSplit
                Tuning.defaults.ResetFirstShare
                (seen |> Option.defaultWith glance)
                (Observe.Danger.standing world.Time danger)
            |> snd
            |> Set.ofList
        else
            Set.empty

    // Every colony that does not decide this tick.
    let idle = Set.union waiting replaying

    // One view per living colony (under test in `ViewTests`): whole for a
    // colony that decides, and for one that does not, its cut of the lane
    // alone (`ColonyView.lane`): the rooms the deciding colonies project and
    // the rooms beside them, all a decide reads of another colony's cut
    // (`ArenaLaneTests`, "arena lane cut").
    let lane =
        lazy
            (cuts
             |> List.filter (fun (colony, _) -> not (Set.contains colony.Home idle))
             |> List.map snd
             |> ColonyView.lane)

    let views =
        cuts
        |> List.map (fun (colony, cut) ->
            let within =
                if Set.contains colony.Home idle then
                    Some lane.Value
                else
                    None

            colony, timed colony.Home (fun () -> ColonyView.project within cut))

    // The projection boundary. The Raid logs' reads ride in this phase, not
    // the prelude: the gate and the cut are one act, and splitting them would
    // price a `find` sweep against a Memory read. So do the deciders, which
    // read the cuts.
    let atSnapshot = Game.cpu.getUsed ()
    let verbose = ObserveMemory.loadVerbose ()

    // Each colony's cut and projection summed as one reading, in colony
    // order, cumulative off the boundary before the first: built, not read
    // off the counter, since the deciders run between the cuts and the
    // projections and fall to the phase's remainder.
    let projectedAt =
        colonies
        |> List.scan
            (fun (_, at) colony ->
                colony.Home, at + (Map.tryFind colony.Home projectSpent |> Option.defaultValue 0.0))
            ("", atProjects)
        |> List.tail

    // The crossings a cast walks (`Caster.assign`) and an undeclared Task's
    // owner is ranked by, as a harassment room's caster is: the table built
    // only where it is read.
    let hops =
        lazy (World.reachHops StandDown.none joins Tuning.defaults world Tuning.defaults.MaxHops)

    // The flood counters after a decision, cumulative like the clock beside
    // them.
    let floodedNow () : Observe.FloodCounts =
        {
            Floods = Grid.Counters.floods
            Free = Grid.Counters.free
            Pops = Grid.Counters.pops
        }

    // Whose turn it is to re-plan, by the full ticks counted so far.
    let turn =
        if List.isEmpty views || resetTick then
            -1
        else
            fullTicks % List.length views

    // Every colony's decision (`Entry.decideColonies`). The counter is read
    // after each colony's decision, cumulative, so the CPU line can say which
    // colony a spike came out of; `foldCpu` differences.
    //
    // A colony whose own step throws decides nothing this tick (#628): it
    // replays its last plan as a replayed colony does (none, where it has
    // none), keeps its memo, carry, Tasks and draw record as an idle colony
    // does, and on the first fault of a run the record defers it, so the
    // next tick decides it again as a reset tick's deferred colony is
    // decided (`LightTick.faultRuns`). A throw every colony shares is the
    // tick's, and still spends the turn.
    let colonyDecisions =
        try
            views
            |> Decide.Entry.decideColonies
                {
                    Hops = fun home room -> hops.Value home room
                    Holding = assignments
                    Verbose = verbose
                    PlanMemos = planMemos
                    Carries = economyCarries
                    Idle = idle
                    FightAlone = fightCandidates
                    TurnIndex = Some turn
                }
                (fun () -> Game.cpu.getUsed (), floodedNow ())
        with error ->
            fullTicks <- fullTicks + 1
            raise error

    let fightOnly = colonyDecisions.FoughtAlone

    let faulted = colonyDecisions.Faulted |> List.map _.Colony |> Set.ofList

    colonyDecisions.Faulted
    |> List.map (fun fault -> faultRow fault.Colony fault.Phase fault.Error)
    |> recordFaults

    let firstFaults =
        let runs, first =
            LightTick.faultRuns
                faultRuns
                faulted
                (colonyDecisions.Decided
                 |> List.map (fun (colony, _, _, _) -> colony.Home)
                 |> Set.ofList)

        faultRuns <- runs
        first

    if not colonyDecisions.TurnWaits then
        fullTicks <- fullTicks + 1

    let turn = colonyDecisions.Turn
    let views = colonyDecisions.Views

    // Every decision in the order it was made, as the clock reads them.
    let decidedInOrder =
        colonyDecisions.Decided
        |> List.map (fun (colony, view, decision, (at, flooded)) ->
            colony, view, decision, at, flooded)

    // And in colony order, as every fold after reads them.
    let decisions =
        views
        |> List.choose (fun (colony, _) ->
            decidedInOrder
            |> List.tryFind (fun (decided, _, _, _, _) -> decided.Home = colony.Home))

    let replayedHomes = Set.union replaying fightOnly

    // The record this tick replays from: a forced tick's, and on any other
    // full tick the last one's, which only a faulted colony replays.
    let prior = lastFull |> Option.map snd

    // Each faulted colony with a plan to replay.
    let faultReplay =
        match prior with
        | Some last -> faulted |> Set.filter (fun home -> Map.containsKey home last.Plans)
        | None -> Set.empty

    let replayHomes = Set.union replayedHomes faultReplay

    // The decides' end and the replay's start, as a forced tick's row splits
    // its `decide` phase (`Forcing.ReplayMs`).
    let atReplay = Game.cpu.getUsed ()

    // The replayed acts and steps, off the glance as a light tick reads it,
    // the objects their acts name looked up, and the tiles they claim
    // (`LightTick.forcedReplay`): a body a deciding colony holds is that
    // colony's to decide, and one whose plan another colony made stands.
    let replayed, claims =
        match prior, seen with
        | Some last, Some seen when not (Set.isEmpty replayHomes) ->
            let economy =
                views
                |> List.filter (fun (colony, _) -> Set.contains colony.Home fightOnly)
                |> List.collect (fun (_, view: ColonyView) -> view.Creeps |> List.map _.Name)
                |> Set.ofList

            let tired =
                views
                |> List.collect (fun (_, view: ColonyView) ->
                    ColonyView.bodies view
                    |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None))
                |> Set.ofList

            LightTick.forcedReplay
                (Set.union replaying faultReplay)
                fightOnly
                holders
                economy
                tired
                last
                { seen with
                    Objects = standingOf (LightTick.only replayHomes last)
                }
        | _ -> [], Set.empty

    // The one movement pass of the tick: every deciding colony's Move
    // Intents, arbitrated once per room around the replayed bodies' steps.
    let moveIntents, moveVerdicts =
        resolveRooms
            claims
            (decisions |> List.map (fun (_, _, decision, _, _) -> decision.Movement))

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

    // Each replanning colony with what moved its signature (#463), read only
    // on the replan: `signatureMoves` parses both strings.
    let moved =
        decisions
        |> List.choose (fun (colony, _, decision, _, _) ->
            match Map.tryFind colony.Home planMemos with
            // A deferred plan keeps the stale signature on purpose, so it
            // compares equal and is not a replan.
            | Some prior when prior.Signature <> decision.Memo.Signature ->
                // `PlanMemo.deferred`'s empty signature: the first plan since
                // a reset, which moved everything at once.
                if prior.Signature = "" then
                    Some(colony.Home, [ "reset" ])
                else
                    Some(colony.Home, signatureMoves prior.Signature decision.Memo.Signature)
            | Some _ -> None
            // No prior at all is a global reset, and only the colony whose
            // turn it was paid; the rest were handed `PlanMemo.deferred`.
            // Counting all four here reported the same four re-plans twice.
            | None when Some colony.Home = payingHome -> Some(colony.Home, [ "reset" ])
            | None -> None)

    let replans = List.length moved

    // A replayed colony keeps its memo for the tick it decides on, and a
    // faulted one for the tick it decides again on.
    let keeping = Set.union replaying faulted

    planMemos <-
        (planMemos |> Map.filter (fun home _ -> Set.contains home keeping) |> Map.toList)
        @ (decisions
           |> List.map (fun (colony, _, decision, _, _) -> colony.Home, decision.Memo))
        // Not `Map.ofList` here (#401): its comparer would keep this tick's
        // `decisions`, views and Atlases alive until the next tick's memos.
        |> Fresh.mapOfList

    // And its economy's carry; a fight-only decide hands back the one it read.
    economyCarries <-
        (economyCarries
         |> Map.filter (fun home _ -> Set.contains home keeping)
         |> Map.toList)
        @ (decisions
           |> List.map (fun (colony, _, decision, _, _) -> colony.Home, decision.Economy))
        |> Fresh.mapOfList

    // The Reactor programme's observation. The room facts answer only while
    // vision does, so a blind tick hands `None` and the last sample stands.
    // A fight-only colony's delivery is its replay's, and it decided none.
    let economyDecisions =
        decisions
        |> List.filter (fun (colony, _, _, _, _) -> not (Set.contains colony.Home fightOnly))

    let reactorReading =
        economyDecisions
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
    // disjoint, so the union is the whole map; an idle or a faulted colony's
    // creeps keep the Tasks they were read with.
    let undecided = Set.union idle faulted

    let kept =
        views
        |> List.filter (fun (colony, _) -> Set.contains colony.Home undecided)
        |> List.collect (fun (_, view) ->
            ColonyView.bodies view
            |> List.choose (fun creep ->
                Map.tryFind creep.Name assignments |> Option.map (fun task -> creep.Name, task)))
        |> Map.ofList

    let saved =
        (kept, decisions)
        ||> List.fold (fun acc (_, _, decision, _, _) ->
            (acc, decision.Assignments)
            ||> Map.fold (fun acc creep task -> Map.add creep task acc))

    saveAssignments saved
    // Kept for the light ticks after, so rebuilt where nothing is captured
    // (#401).
    heldTasks <- Fresh.mapOfSeq (Map.toSeq saved)

    // The draw record (#530) on the same terms: an idle colony's creeps keep
    // what they were read with.
    let keptDrawn =
        views
        |> List.filter (fun (colony, _) -> Set.contains colony.Home undecided)
        |> List.collect (fun (_, view) ->
            ColonyView.bodies view
            |> List.choose (fun creep ->
                creep.DrawnFrom |> Option.map (fun store -> creep.Name, store)))
        |> Map.ofList

    ObserveMemory.saveDrawn (
        (keptDrawn, decisions)
        ||> List.fold (fun acc (_, _, decision, _, _) ->
            (acc, decision.Drawn)
            ||> Map.fold (fun acc creep store -> Map.add creep store acc))
    )

    // One world-wide aliveness set for the fold, the Raid logs and the memory
    // pruning: a name that left a colony's `Creeps` may merely have been
    // adopted, and only `Game.creeps` says which names stopped existing.
    let living = livingCreeps ()

    // One fold over every colony's Verdicts in colony order, so a creep
    // adopted this tick continues the timeline its caster started.
    let restored =
        match observeLog with
        | Some log -> Some log
        | None -> ObserveMemory.restoreLog ()

    let priorLog = restored |> Option.defaultValue Map.empty

    // The refusals this tick's decides read as news (#629): each deciding
    // colony's bodies the match judged — a fight-only decide's fighters —
    // and its home's structures where its economy decided. Their news is
    // settled once the breach log has read it.
    let decidedBodies =
        decisions
        |> List.collect (fun (colony, view, decision, _, _) ->
            if Set.contains colony.Home fightOnly then
                Set.toList decision.Fight.Fighters
            else
                ColonyView.bodies view |> List.map _.Name)
        |> Set.ofList

    let decidedHomes =
        decisions
        |> List.choose (fun (colony, _, _, _, _) ->
            if Set.contains colony.Home fightOnly then
                None
            else
                Some colony.Home)
        |> Set.ofList

    let decidedOn (row: Refusal) =
        if row.Body then
            Set.contains row.Actor decidedBodies
        else
            Set.contains row.Home decidedHomes

    // Each body's refusals since its colony last decided, ahead of the
    // decide's own Verdicts: they answer acts made before it.
    let refused =
        world.Refusals
        |> List.filter (fun row -> row.Body && row.Fresh > 0 && decidedOn row)
        |> List.map (fun row -> Verdict.Refused(row.Actor, row.Task, row.Code, row.Fresh))

    let verdicts =
        refused
        @ (decisions |> List.collect (fun (_, _, decision, _, _) -> decision.Verdicts))
        @ moveVerdicts

    let log =
        Observe.fold Observe.capPerCreep Game.time living verdicts priorLog
        |> Observe.forgetScorings verbose

    // The tick after a global reset encodes the log whole: the rows standing
    // are another bundle's, and only a whole write normalises the wire to
    // what this bundle reads. Every tick after it encodes the changed creeps.
    // A log folded onto empty while the segments are not read yet is dropped.
    match observeLog, restored with
    | Some _, _ -> ObserveMemory.saveChanged priorLog log
    | None, Some _ -> ObserveMemory.save log
    | None, None -> ()

    if restored.IsSome then
        observeLog <- Some log

    // The world danger log, folded through every deciding colony in order
    // (`Danger.foldColony`): each reads what the one before wrote.
    let mutable folded = danger
    // Each colony's raid log as this tick leaves it: the episodes the world
    // roster is pruned against.
    let mutable logs = raids

    // Each colony's open breaches as its fold leaves them, for the visuals.
    let colonyOpen = ResizeArray<Observe.StandingBreach>()

    // The fault ring's rows since the bundle's load, as `observe health`
    // lists them: the `Fault` breaches' fact, by colony ("" for a throw no
    // colony owns).
    let sinceLoad =
        ObserveMemory.loadFaults () |> List.filter (fun row -> row.Tick >= loadedAt)

    let faultsOf (colony: string) =
        sinceLoad
        |> List.filter (fun row -> row.Colony = colony)
        |> List.map (fun row -> row.Phase, row.Tick)

    // One colony's breach log folded off its view and the facts beside it.
    let foldColonyBreaches (home: string) (view: ColonyView) (facts: Observe.ColonyBreachFacts) =
        let prior = ObserveMemory.loadBreaches home

        let state =
            Observe.foldBreaches
                Observe.capBreaches
                Game.time
                (Observe.colonyBreaches facts view prior)
                prior

        ObserveMemory.saveBreaches home state
        colonyOpen.AddRange(Observe.openRows Game.time state)

    for colony, view, decision, _, _ in decisions do
        let economic = not (Set.contains colony.Home fightOnly)

        // The Raid log, written every tick whether or not the fold changed
        // anything, so the leaf's presence lets `observe.mjs raids` tell "no
        // channel" from "no raids". Folded here and read at the top of the
        // loop: this tick's sightings are what the next tick's gate stands on.
        let state, after =
            Observe.Danger.foldColony
                Observe.capEpisodes
                living
                (Colony.declaredRooms colony)
                view
                decision.OutpostRooms
                (raids |> Map.tryFind colony.Home |> Option.defaultValue Observe.RaidState.empty)
                folded

        let state, after = Observe.Danger.probing view decision.Intents (state, after)
        folded <- after

        // The haul overflow reading the decision smoothed (#508); a
        // fight-only colony's economy smoothed none, and keeps the last.
        let state =
            if economic then
                { state with
                    HaulOverflow = decision.HaulOverflow
                }
            else
                state

        logs <- Map.add colony.Home state logs
        ObserveMemory.saveRaids colony.Home (colony.Outposts |> List.map _.RoomName) state

        // A fight-only colony keeps its Layout and quota leaves as written,
        // as a replayed one does: its economy did not decide.
        if economic then
            // The Layout's channel, written every tick, empty or not. The
            // refused declarations come off the view and not the memo: they
            // are the declaration's loss, not the plan's.
            ObserveMemory.saveLayout
                colony.Home
                decision.Memo.UnservedFootings
                decision.Memo.UnroutedTrunks
                decision.Memo.DeferredContainers
                view.Refused
                decision.WaitingContainers
                decision.Memo.OutpostRoads

            // The cascade's own numbers, for `observe.mjs quotas`.
            ObserveMemory.saveQuotas colony.Home decision.Quotas

        // The breach log: the live invariant checks, off the same view this
        // tick decided from. The suite runs on fixtures this repo authors, so
        // it confirms the code's belief about the projection; an assertion
        // read off the real projection every tick is the feedback loop that
        // could have caught the `World.fs` incidents (#278). Written every
        // tick, empty or not. A fight-only decide's quotas are silent, so
        // the seats owed and the holds are unknown there.
        foldColonyBreaches
            colony.Home
            view
            {
                Quotas = (if economic then Some decision.Quotas else None)
                Casting =
                    decision.Intents
                    |> List.choose (function
                        | SpawnCreep(spawn, _, _) -> Some spawn
                        | _ -> None)
                    |> Set.ofList
                CastRefused = castRefused
                StoodDown =
                    (state.Outposts
                     |> List.filter (Observe.standingDown Game.time)
                     |> List.map _.RoomName)
                    @ (colony.Outposts
                       |> List.map _.RoomName
                       |> List.filter (fun room ->
                           Map.tryFind room after.Rooms
                           |> Option.bind _.Down
                           |> Option.exists (Observe.standingDown Game.time)))
                    |> Set.ofList
                    |> Set.union view.HeldOutposts
                Log = log
                Faults = faultsOf colony.Home
            }

    // A faulted colony's breaches read its view alone, and are folded; its
    // Raid log and the world danger fold read its decision's outposts and
    // intents, which it has none of, and stand as read, as an idle colony's
    // do.
    for colony, view in views do
        if Set.contains colony.Home faulted then
            foldColonyBreaches
                colony.Home
                view
                { Observe.ColonyBreachFacts.none with
                    Log = log
                    Faults = faultsOf colony.Home
                }

    // The world's breach log: the CPU line's and the throws no colony owns.
    // The line ends at the tick before this one, which writes its row last.
    let world' =
        cpuLine
        |> Option.map (fun line ->
            ObserveMemory.loadWorldBreaches ()
            |> Observe.foldBreaches
                Observe.capBreaches
                Game.time
                (Observe.worldBreaches
                    tuning
                    {
                        Time = Game.time
                        Line = line
                        Limit = float Game.cpu?limit
                        Cadence =
                            (if Tuning.defaults.LightTicks then
                                 Some tuning.Cadence
                             else
                                 None)
                        Faults = faultsOf ""
                    }))

    world' |> Option.iter ObserveMemory.saveWorldBreaches

    openBreaches <-
        (world' |> Option.map (Observe.openRows Game.time) |> Option.defaultValue [])
        @ List.ofSeq colonyOpen
        |> List.map _.Breach

    // The world danger log, written every tick with this tick's exit watches,
    // and its latches with the room latches when either moved.
    let folded =
        folded
        |> Observe.Danger.trim Observe.capEpisodes world.Time
        |> Observe.Danger.watching world.ExitWatches

    ObserveMemory.saveDanger folded

    // The world raid roster: this tick's sightings and tombstones, every
    // room's, kept while some colony's episode reads them. Written every full
    // tick.
    ObserveMemory.loadRoster ()
    |> Observe.Roster.fold world.Time watch
    |> Observe.Roster.prune
        world.Time
        Tuning.defaults.QuietGap
        (logs |> Map.toList |> List.collect (fun (_, log) -> log.Episodes))
    |> ObserveMemory.saveRoster

    // A given-up colony's leaf and the flat legacy log, which nothing writes:
    // every home neither declared nor living (an undeclared spawn room, when
    // nothing declared lives, is a colony of its own).
    ObserveMemory.pruneColonies ((Colony.declared @ colonies) |> List.map _.Home)

    let latches = { latches with Latched = folded.Rivals }

    if latches <> written then
        ObserveMemory.saveRoomLatches latches

    roomLatches <- Some latches

    pruneDeadCreepMemory living

    // Where every creep of ours stood this tick, for next tick's
    // `CreepInfo.Walking`.
    World.positions () |> ObserveMemory.savePositions
    // The Memory boundary: everything this tick persists except the CPU
    // line's own leaf, which is written after the last reading and so is the
    // one write the line never prices. The phase holds the observe folds and
    // the `Game.creeps` sweep as well as the writes.
    let atSave = Game.cpu.getUsed ()

    let planOf intents =
        intents
        |> Fabot.Core.IntentPlan.create
        |> function
            | Ok plan -> plan
            | Error conflict -> invalidOp $"Conflicting creep intents: %A{conflict}"

    // Each bubble's glyph with its creep's Verdicts this tick after it
    // (#638): what the creep did, in the characters the say leaves.
    let codes = Observe.sayCodes verdicts

    let denser intent =
        match intent with
        | SayCreep(name, glyph) ->
            match Map.tryFind name codes with
            | Some code -> SayCreep(name, Observe.denser glyph code)
            | None -> intent
        | _ -> intent

    // A faulted colony's towers and safe mode ride with the decided acts: no
    // replay repeats them (#628).
    let decided =
        (decisions
         |> List.collect (fun (_, _, decision, _, _) -> decision.Intents)
         |> List.map denser)
        @ moveIntents
        @ (colonyDecisions.Towers |> List.collect snd)
        |> firstActivationOnly

    // The replayed colonies' acts on the channels the deciding ones leave
    // free, run as a light tick runs them: a refusal the replay expects is
    // not news.
    let outcomes =
        Executor.run (planOf decided)
        @ Executor.runQuiet (planOf (Fabot.Core.IntentPlan.beside decided replayed))

    recordFaults (thrownOf outcomes)

    // The decided colonies' news read, this tick's refusals laid on, and the
    // rows priced at nothing and the dead bodies' gone.
    refusals <-
        refusals
        |> Refusal.settle decidedOn
        |> Refusal.forget tuning.RefusalHalfLife Game.time living

    // The decided acts by the colony that decided them, a faulted colony's
    // towers by its home, then the plans the replayed acts came off.
    recordRefusals
        (lazy
            ((decisions
              |> List.map (fun (colony, _, decision, _, _) -> colony.Home, decision.Intents))
             @ colonyDecisions.Towers
             @ (prior |> Option.map planned |> Option.defaultValue [])))
        outcomes

    for spawn, refused in Executor.casts outcomes do
        castRefused <-
            if refused then
                Set.add spawn castRefused
            else
                Set.remove spawn castRefused

    let accepted = acceptedOf outcomes

    // The record the next tick replays: each deciding colony's plan off the
    // glance this tick began with — a fight-only colony's, its fighters'
    // beside the economy's carried (`LightTick.recorded`) — then each
    // replayed colony's carried whole. None with the cadence off
    // (`Tuning.LightTicks`): no glance was taken.
    lastFull <-
        seen
        |> Option.map (fun seen ->
            let fresh =
                decisions
                |> List.map (fun (colony, view, decision, _, _) ->
                    let decided =
                        if Set.contains colony.Home fightOnly then
                            decision.Fight.Fighters
                        else
                            ColonyView.bodies view |> List.map _.Name |> Set.ofList

                    let plan =
                        LightTick.plan
                            seen
                            decided
                            decision.Steps
                            decision.Next
                            decision.Later
                            decision.Intents
                            decision.Aims
                            decision.Volleys
                            decision.Mired
                        |> LightTick.tracking seen decision.Tracks

                    colony.Home,
                    LightTick.recorded
                        (if Set.contains colony.Home fightOnly then
                             Some decided
                         else
                             None)
                        plan
                        (forcedBy
                         |> Option.bind (fun (_, last, _) -> Map.tryFind colony.Home last.Plans)))

            // A forced tick that replayed colonies or a colony's economy
            // leaves the cadence where it was: the next full tick on it
            // decides them.
            let cadence, carried =
                match forcedBy with
                | Some(tick, last, _) when not (Set.isEmpty replayedHomes) ->
                    tick,
                    replaying
                    |> Set.toList
                    |> List.choose (fun home ->
                        Map.tryFind home last.Plans |> Option.map (fun plan -> home, plan))
                | _ -> Game.time, []

            // Each faulted colony's plan carried, the one it replayed or an
            // empty one; on the first fault of its run it is deferred as the
            // reset tick's waiting colonies are: the next tick is full and
            // decides it.
            let faultCarried =
                faulted
                |> Set.toList
                |> List.map (fun home ->
                    home,
                    LightTick.faultedPlan
                        seen
                        (prior |> Option.bind (fun last -> Map.tryFind home last.Plans)))

            cadence,
            LightTick.ofPlans seen (Set.union waiting firstFaults) (fresh @ carried @ faultCarried))

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
            // In the order decided: the readings are cumulative.
            ColonyDecides =
                decidedInOrder |> List.map (fun (colony, _, _, at, _) -> colony.Home, at)
            ColonyFloods =
                decidedInOrder
                |> List.map (fun (colony, _, _, _, flooded) -> colony.Home, flooded)
            HeapMb = heap
            ExternalMb = external
            MemoRows = memoRows ()
            // Off `World`'s own heap slot, not the world record: a measurement
            // of the shell is not a fact about the game.
            RoomSnapshots = World.roomCosts
            AtRooms = World.roomsBegan
            AtProjects = atProjects
            ColonyProjects = projectedAt
            Light = false
            Forced =
                forcedBy
                |> Option.bind (fun (_, _, reasons) ->
                    List.tryHead reasons
                    |> Option.map (fun first ->
                        ({
                            Reason = LightTick.tag first
                            Facts = List.length reasons
                            Replayed = Set.count replaying
                            FightOnly = Set.count fightOnly
                            // Only where something replayed: on a forced
                            // tick every colony decided, the span is the
                            // movement pass alone.
                            ReplayMs =
                                if Set.isEmpty replayHomes then 0.0 else atDecide - atReplay
                        }
                        : Observe.Forcing)))
            Moved = moved
        }

    not (Set.isEmpty replayedHomes)

/// One tick, light or full.
let private runTick () =
    // The engine's counter is already running when `loop` is entered: this is
    // the engine's prelude alone. The Memory parse is not in it: `Memory`
    // deserializes on first touch, which is `loadRaids` on a full tick, so the
    // parse is charged to the `snapshot` phase. Every reading is taken every
    // tick; a measurement that switches itself off has absences to explain.
    let atEntry = Game.cpu.getUsed ()

    if loadedAt < 0 then
        loadedAt <- Game.time

    // The flood counters start the tick at zero, so each colony's reading
    // below is cumulative from here and `foldCpu` can difference it; and the
    // last tick's hostile sides go (`HostileInfo.forgetSides`), and with them
    // its projections' indexes (`TickMemo.forgetAll`).
    Grid.Counters.reset ()
    HostileInfo.forgetSides ()
    TickMemo.forgetAll ()

    // ADR-0082
    // Light only within the cadence after a full tick (`cadence` ticks a
    // decide: the tuned one, a short bucket's, or the live override in
    // Memory), never on the tick after a reset, and only when nothing the
    // glance reads says the replay would be wrong. With the cadence off no
    // glance is taken at all, so the flag gives back today's tick and its
    // cost exactly.
    if not Tuning.defaults.LightTicks then
        fullTick atEntry None None |> ignore
    else
        bucketHeld <- LightTick.bucketCadence Tuning.defaults bucketHeld Game.cpu.bucket

        let cadence, cadenceFrom =
            LightTick.cadenceFrom Tuning.defaults bucketHeld (ObserveMemory.loadCadence ())

        ObserveMemory.saveSwitches (cadence, cadenceFrom) (fightOnlyNow ())

        if tuning.Cadence <> cadence then
            tuning <-
                { Tuning.defaults with
                    Cadence = cadence
                }

        let seen = glance ()

        // A forced tick that replayed colonies counts toward the cadence as a
        // light one: the full tick after it decides every colony (#549).
        match lastFull with
        | Some(tick, last) when LightTick.replays cadence (Game.time - tick) lightRun ->
            match LightTick.forcing last seen with
            | [] ->
                lightRun <- lightRun + 1
                lightTick atEntry last { seen with Objects = standingOf last }
            | reasons ->
                lightRun <-
                    if fullTick atEntry (Some seen) (Some(tick, last, reasons)) then
                        lightRun + 1
                    else
                        0
        | _ ->
            lightRun <- 0
            fullTick atEntry (Some seen) None |> ignore

    // The creep overlay (#638), off what the tick left on the heap: one
    // Memory read when the draw list is empty.
    Visuals.creeps
        (ObserveMemory.loadVisuals ())
        (lastFull
         |> Option.map (fun (_, last) -> last.Plans)
         |> Option.defaultValue Map.empty)
        heldTasks
        (observeLog |> Option.defaultValue Map.empty)
        refusals

// Exported as `loop` on the bundled `main` module; the engine calls it every
// tick. A throw no colony owns is still the tick's and still reaches the
// engine, written to the fault ring first (#628).
let loop () =
    try
        runTick ()
        // The operator-only logs out to their segments, when owed (#639).
        ObserveMemory.flushSegments Game.time |> recordFaults
    with error ->
        // A ring that will not write must not hide the throw it records.
        try
            recordFaults [ faultRow "" "tick" error ]
            ObserveMemory.flushSegments Game.time |> recordFaults
        with _ ->
            ()

        raise error
