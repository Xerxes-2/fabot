/// Loads the committed room captures (ADR 0036). Real terrain, taken off
/// the season server once by `scripts/capture-room.mjs`, reviewed as text
/// and committed — so the suite can test whole-room geometry without ever
/// touching the API. Real terrain is a counterexample generator here, not
/// a source of expected values: nothing in this module knows a tile the
/// Layout is supposed to pick.
module Fabot.Core.Tests.RoomFixtures

open System
open System.Collections.Generic
open System.IO
open Fabot.Core
open Fabot.Core.Types

/// One captured room, as its committed fixture spells it.
type RoomCapture =
    {
        RoomName: string
        /// The shard and tick the capture was taken at — provenance, so a
        /// fixture can be re-captured later with confidence. Terrain never
        /// changes; the furniture is what the tick pins.
        Shard: string
        Tick: int
        /// Terrain per tile over x,y in 1..48 — the same window
        /// `World.terrainOf` projects as ground, with the exit rows
        /// dropped.
        Terrain: Map<Pos, Terrain>
        /// Terrain on the border ring, x or y of 0 or 49 — the rows the
        /// window above drops, delivered beside it and never inside it,
        /// exactly as the shell delivers them (ADR 0041): the Seam's own
        /// terrain, the engine's verbatim, and never a tile to stand on.
        Border: Map<Pos, Terrain>
        /// The room's sources in the capture's order, each under the
        /// readable id the projection knows it by.
        Sources: (string * Pos) list
        /// The controller, when the room has one. A three-source room —
        /// sector centre or Source Keeper room — has none, and cannot be
        /// owned.
        Controller: (string * Pos) option
        /// The same sources, in the same order, under the ids the *engine*
        /// gave them — what the capture actually recorded, before the
        /// rename above made it readable. Beside the readable ids rather
        /// than instead of them, because the two answer different
        /// questions: a test that names a tile reads better with `src-0`,
        /// and a test that has to match an outpost declaration has no
        /// choice at all. An outpost is declared in the engine's ids
        /// (`Outpost`), because that is what a live projection keys every
        /// target by, so these are the ids a ColonyView built to meet one
        /// has to carry (ADR 0041, ADR 0042).
        RealSources: (string * Pos) list
        /// The controller under the engine's own id, as `RealSources` is.
        RealController: (string * Pos) option
    }

/// A captured room projected as a `SpatialInfo`, beside the ids the
/// loader placed on it: a test that hand-wrote its own source list would
/// be green and wrong the day it met a room with a different count.
type LoadedRoom =
    {
        Spatial: SpatialInfo
        SourceIds: string list
        ControllerId: string option
    }

let private roomSide = 50

/// Fixtures are copied beside the test assembly, so they are found the
/// same way whatever the runner's working directory happens to be.
let private roomsDirectory = Path.Combine(AppContext.BaseDirectory, "rooms")

/// The engine's own terrain mask, classified into the Core's three states
/// exactly as `World.terrainAt` classifies it — wall bit first, then
/// swamp. These two must agree or the fixture describes a room the bot
/// never sees, which is #75's argument one level up.
let private terrainOfMask mask =
    if mask &&& 1 <> 0 then Wall
    elif mask &&& 2 <> 0 then Swamp
    else Plain

/// The committed capture of one room, by name.
let load (roomName: string) : RoomCapture =
    let path = Path.Combine(roomsDirectory, roomName + ".room")

    if not (File.Exists path) then
        failwithf "no room capture at %s — run `npm run capture-room -- %s`" path roomName

    let lines = File.ReadAllLines path

    let sectionAt marker =
        match Array.tryFindIndex ((=) marker) lines with
        | Some index -> index
        | None -> failwithf "%s has no %s section" path marker

    let terrainSection = sectionAt "[terrain]"
    let objectSection = sectionAt "[objects]"

    let header =
        lines.[.. terrainSection - 1]
        |> Array.filter (fun line -> not (line.StartsWith "#") && line.Trim() <> "")
        |> Array.map (fun line ->
            match line.Split '\t' with
            | [| key; value |] -> key, value
            | _ -> failwithf "%s: header line %s is not key<TAB>value" path line)
        |> Map.ofArray

    let field name =
        match Map.tryFind name header with
        | Some value -> value
        | None -> failwithf "%s: header carries no %s" path name

    let rows = lines.[terrainSection + 1 .. terrainSection + roomSide]

    if
        rows.Length <> roomSide
        || rows |> Array.exists (fun row -> row.Length <> roomSide)
    then
        failwithf "%s: terrain is not %d rows of %d characters" path roomSide roomSide

    // The file holds the room verbatim so a re-capture diffs cleanly, and
    // these are the lines that split it the way the shell splits it: the
    // window the projection stands on, and the border ring beside it that
    // only the Seam reads (ADR 0036, ADR 0041). Rows are the capture's own
    // row-major order — `rows.[y].[x]`.
    let terrainAt x y =
        terrainOfMask (int rows.[y].[x] - int '0')

    let terrain =
        Map.ofList
            [
                for y in 1 .. roomSide - 2 do
                    for x in 1 .. roomSide - 2 -> { X = x; Y = y }, terrainAt x y
            ]

    let border =
        Map.ofList
            [
                for y in 0 .. roomSide - 1 do
                    for x in 0 .. roomSide - 1 do
                        if x = 0 || x = roomSide - 1 || y = 0 || y = roomSide - 1 then
                            { X = x; Y = y }, terrainAt x y
            ]

    let objectRows =
        lines.[objectSection + 1 ..] |> Array.filter (fun line -> line.Trim() <> "")

    match Array.tryHead objectRows with
    | Some "id\ttype\tx\ty" -> ()
    | _ -> failwithf "%s: the objects section has no id/type/x/y column header" path

    // The capture keeps the API's real ids, which is what makes it
    // traceable; the projection's keys are the test's own vocabulary — and
    // since an outpost is declared in the engine's ids, both are carried
    // out rather than one being thrown away here.
    // A mineral row is read and ignored: a mineral has no TargetKind to
    // enter the projection through.
    let objects =
        objectRows
        |> Array.skip 1
        |> Array.map (fun line ->
            match line.Split '\t' with
            | [| objectId; kind; x; y |] -> kind, (objectId, { X = int x; Y = int y })
            | _ -> failwithf "%s: object row %s is not id/type/x/y" path line)

    let ofKind kind =
        objects |> Array.filter (fst >> (=) kind) |> Array.map snd |> List.ofArray

    let sources = ofKind "source"
    let controller = ofKind "controller" |> List.tryHead

    {
        RoomName = field "room"
        Shard = field "shard"
        Tick = int (field "tick")
        Terrain = terrain
        Border = border
        Sources = sources |> List.mapi (fun index (_, pos) -> $"src-{index}", pos)
        Controller = controller |> Option.map (fun (_, pos) -> "ctrl", pos)
        RealSources = sources
        RealController = controller
    }

/// The captured room with a spawn standing on it. The spawn is always the
/// test's — a real room's spawn is somebody else's base — and
/// `fallbackController` supplies a tile only for a room the capture found
/// none in, which is the only way a three-source room plans anything:
/// without a projected controller position the Upgrade Work Area is empty,
/// the trunks route to the spawn alone, and the footing count is wrong for
/// reasons that are not the rule's.
let project (capture: RoomCapture) (spawn: Pos) (fallbackController: Pos option) : LoadedRoom =
    let controllerTarget =
        capture.Controller
        |> Option.orElse (fallbackController |> Option.map (fun pos -> "ctrl", pos))

    let targets =
        [
            yield "spawn-1", spawn, Structure BuiltKind.Spawn

            for id, pos in capture.Sources do
                yield id, pos, Source

            for id, pos in Option.toList controllerTarget do
                yield id, pos, Controller
        ]

    {
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some capture.RoomName
                // The captured room's geometry under the captured room's
                // name, which is the shape `buildSpatial` produces and the
                // only shape there is (ADR 0041).
                Rooms =
                    Map.ofList
                        [
                            capture.RoomName,
                            { RoomLayer.empty with
                                Terrain = capture.Terrain
                                TargetPositions =
                                    targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                                // The obstacle rule is the shell's, read off
                                // the Core's own table rather than restated:
                                // a structure a creep cannot stand on blocks
                                // its tile, and so does the controller.
                                Obstacles =
                                    targets
                                    |> List.choose (fun (_, pos, kind) ->
                                        match kind with
                                        | Structure built when not (isWalkable built) -> Some pos
                                        | Controller -> Some pos
                                        | _ -> None)
                                    |> Set.ofList
                            }
                        ]
                // Beside the ground, never inside it, exactly as the shell
                // delivers it (ADR 0041): one room's ring under its own
                // name, which is the shape `buildSpatial` produces. One
                // capture is one room, so this projection answers no band
                // by itself — a Seam joins two rooms, and a projection that
                // can answer one is composed by merging two captures'
                // rings, as `RoomInvariantTests.acrossFrom` does.
                Borders = Map.ofList [ capture.RoomName, capture.Border ]
                TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
            }
        SourceIds = capture.Sources |> List.map fst
        ControllerId = controllerTarget |> Option.map fst
    }

/// The engine's CONTROLLER_STRUCTURES for the three kinds a level moves in
/// this fixture, indexed by RCL. The same numbers Core spells in
/// `Decide.extensionAllowance` and its kin — copied rather than shared
/// because those are `private` to Core, and copied *whole* as a table so a
/// tier asks the level for a count instead of a reader hand-checking one
/// (`scripts/profile.mjs` carries the same table for the same reason).
let private extensionAllowance = [| 0; 0; 5; 10; 20; 30; 40; 50; 60 |]
let private towerAllowance = [| 0; 0; 0; 1; 1; 2; 2; 3; 6 |]
let private storageAllowance = [| 0; 0; 0; 0; 1; 1; 1; 1; 1 |]

/// Screeps CARRY_CAPACITY: what one Carry part holds.
let private carryCapacity = 50

/// Screeps SPAWN_ENERGY_CAPACITY and EXTENSION_ENERGY_CAPACITY at RCL7 and
/// below: the two stores a room's bank is the sum of.
let private spawnCapacity = 300
let private extensionCapacity = 50

/// Whether a tile is ground a creep can stand on: inside the projected
/// window (x,y in 1..48, which is the only ground the capture carries) and
/// not wall. Swamp is ground — a container sits on one and W12S28's own
/// controller is served off one.
let private isGround (capture: RoomCapture) (pos: Pos) =
    match Map.tryFind pos capture.Terrain with
    | Some Wall
    | None -> false
    | Some _ -> true

let private neighboursOf (pos: Pos) =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if dx <> 0 || dy <> 0 then
                    { X = pos.X + dx; Y = pos.Y + dy }
    ]

/// The nearest tile to `origin` that is ground, unclaimed, and reachable
/// from it across ground. Reachable rather than merely near: a container
/// placed across a wall would be a target nothing can serve, and the
/// fixture would describe a colony that cannot work. Real terrain is a
/// counterexample generator (ADR 0036), so this fails loudly rather than
/// guessing when the room has no such tile.
let private nearestFree (capture: RoomCapture) (taken: HashSet<Pos>) (origin: Pos) : Pos =
    let seen = HashSet<Pos>([ origin ])
    let queue = Queue<Pos>([ origin ])
    let mutable found = None

    while found.IsNone && queue.Count > 0 do
        let tile = queue.Dequeue()

        for step in neighboursOf tile do
            if found.IsNone && seen.Add step && isGround capture step then
                if taken.Contains step then
                    queue.Enqueue step
                else
                    found <- Some step

    match found with
    | Some tile -> tile
    | None -> failwithf "%s: no free tile reachable from %d,%d" capture.RoomName origin.X origin.Y

/// The shortest walkable route between two tiles, endpoints included — the
/// line a trunk road is paved along. A search over ground rather than a
/// straight line, because on real terrain a straight line runs through
/// walls and a road on a wall is a road nothing walks; `blocked` carries
/// the cluster's obstacle tiles, which the lane lattice weaves around.
let private routeBetween (capture: RoomCapture) (blocked: HashSet<Pos>) (from: Pos) (goal: Pos) =
    let cameFrom = Dictionary<Pos, Pos option>()
    cameFrom[from] <- None
    let queue = Queue<Pos>([ from ])
    let mutable arrived = false

    while not arrived && queue.Count > 0 do
        let tile = queue.Dequeue()

        if tile = goal then
            arrived <- true
        else
            for step in neighboursOf tile do
                if
                    not (cameFrom.ContainsKey step)
                    && isGround capture step
                    && not (blocked.Contains step)
                then
                    cameFrom[step] <- Some tile
                    queue.Enqueue step

    if not arrived then
        failwithf
            "%s: no walkable route from %d,%d to %d,%d"
            capture.RoomName
            from.X
            from.Y
            goal.X
            goal.Y

    let rec unwind tile acc =
        match cameFrom[tile] with
        | Some parent -> unwind parent (tile :: acc)
        | None -> tile :: acc

    unwind goal []

/// The room's working ground (ADR 0022): every source's Seats plus the
/// controller's Upgrade Work Area. The Layout keeps its clustered
/// structures off it — a structure there eats a tile an Anchor or an
/// upgrader stands on — so the fixture's cluster steps over it too, or it
/// furnishes a room this colony's own Layout would never have built.
let private workingGround (capture: RoomCapture) (controller: Pos) =
    let ground = HashSet<Pos>()

    let reserve tile =
        if isGround capture tile then
            ground.Add tile |> ignore

    for _, source in capture.Sources do
        for tile in neighboursOf source do
            reserve tile

    for dx in -3 .. 3 do
        for dy in -3 .. 3 do
            reserve
                {
                    X = controller.X + dx
                    Y = controller.Y + dy
                }

    ground

/// The tiles the room's cluster of extensions, towers and Storage stands
/// on: ground spiralling out from the spawn, taking only the tiles whose
/// x+y parity matches the spawn's and never the working ground above. The
/// parity is load-bearing and not tidiness — those kinds are all obstacles,
/// so thirty of them packed nearest-first would wall the spawn in, and one
/// parity leaves the lane lattice a real clustered plan leaves (ADR 0039).
let private clusterTiles (capture: RoomCapture) (spawn: Pos) count (taken: HashSet<Pos>) reserved =
    let parity = (spawn.X + spawn.Y) % 2
    let tiles = ResizeArray<Pos>()
    let seen = HashSet<Pos>([ spawn ])
    let queue = Queue<Pos>([ spawn ])

    while tiles.Count < count && queue.Count > 0 do
        let tile = queue.Dequeue()

        for step in neighboursOf tile do
            if seen.Add step && isGround capture step then
                queue.Enqueue step

                if
                    tiles.Count < count
                    && (step.X + step.Y) % 2 = parity
                    && not (taken.Contains step)
                    && not ((reserved: HashSet<Pos>).Contains step)
                then
                    tiles.Add step

    if tiles.Count < count then
        failwithf
            "%s: only %d of the %d cluster tiles this level needs are reachable from the spawn at %d,%d"
            capture.RoomName
            tiles.Count
            count
            spawn.X
            spawn.Y

    List.ofSeq tiles

/// Where a fleet row's body stands against the tile its work is at:
/// `Exactly` on that tile, which is what a row holding a *place* takes —
/// the Anchor on its Post, the only footing a work-heavy body's Harvest
/// offers it (ADR 0020, ADR 0051) — or `Nearby`, the nearest free ground
/// outward from it, which is what a row pooling over a place takes.
type private Station =
    | Exactly
    | Nearby

/// A creep of the given body, freshly cast and filled to the given
/// fraction of its carry. A full Screeps CREEP_LIFE_TIME to live, so no
/// fixture creep is expiring and no lead has to be priced to read a test
/// (ADR 0026).
let private castCreep name (body: BodyPart list) fill : CreepInfo =
    let capacity = (body |> List.filter ((=) Carry) |> List.length) * carryCapacity
    let carried = int (round (float capacity * fill))

    {
        Name = name
        TicksToLive = 1500
        Fatigue = 0
        Energy = carried
        FreeCapacity = capacity - carried
        Body = body |> List.countBy id |> Map.ofList
    }

/// A whole colony's ColonyView, built on a captured room at one rung of its
/// life: the room furnished as its Layout would have left it by `level`,
/// the bank that level's extensions add up to, and a fleet cast from
/// `Decide`'s own body rules at that bank. The three rungs the suite runs
/// are RCL1 with a 300 bank, RCL3 with 800 and RCL5 with 1,800 — a young
/// colony, one that has just crossed `Colony.bootstrapLevel`, and the
/// mother this bot grew up on (ADR 0052).
///
/// What the level moves, and all it moves: the extension, tower and
/// Storage counts come off the engine's own allowance table, and the roads
/// and ramparts appear only from `Colony.bootstrapLevel` up, because a
/// room under it places neither (#209, #214). Containers stand at every
/// rung — one on each source's Seat, which is what makes it a Post, and
/// the upgrade buffer beside the controller (ADR 0046). The buffer is a
/// fact about the room and not about the level: the container plan is
/// "RCL-gated by nothing" (`Decide.planLayout`), so a Layout run to
/// completion has one at RCL1 too. That is where this fixture and
/// `scripts/profile.mjs`'s `young` scenario deliberately part company —
/// `young` models the live W13S28, which has built its two source
/// containers and no buffer yet, and this models the room its Layout
/// would have finished — so a test that reads the upgrader gate off one
/// of them is not reading the other's room. Nothing here names a tile:
/// every position is derived from the captured terrain, so this fixture
/// is a counterexample generator like the rest of the file and never a
/// source of expected values.
///
/// Two things it is deliberately **not**. It is the colony's **home room
/// alone**: a declared outpost's terrain layer, which `World.ofGame`
/// reads with or without vision (ADR 0041, ADR 0042), is not here, so
/// the RCL5 rung prices W12S28 as the one-room colony it is not — a test
/// that needs the outpost has to add it. And the fleet pins **body
/// sizing** at this bank and never row counts: the bodies are
/// `Decide.bodyFor`'s own at `bank`, the counts are this fixture's (see
/// `fleetRows`), so nothing here reproduces the [[workforce target]].
///
/// `bank` is checked against the level rather than believed: extensions
/// stand full here, so a bank that is not what this level's extensions
/// add up to describes a room the engine cannot report.
let colonyAt (capture: RoomCapture) (level: int) (bank: int) : ColonyView =
    let levelBank = spawnCapacity + extensionAllowance[level] * extensionCapacity

    if bank <> levelBank then
        failwithf
            "%s at RCL%d holds %d extensions standing full, so its bank is %d and not %d"
            capture.RoomName
            level
            extensionAllowance[level]
            levelBank
            bank

    let controllerId, controllerPos =
        match capture.Controller with
        | Some entry -> entry
        | None ->
            failwithf "%s has no controller, so no colony can be declared in it" capture.RoomName

    let taken = HashSet<Pos>()

    let claim pos =
        taken.Add pos |> ignore
        pos

    // The spawn stands on the ground nearest the room's own furniture —
    // the mean of its sources and its controller — which is roughly where
    // a Layout wants the Keep and, more to the point here, is derived from
    // the capture rather than written down per room.
    let anchors = controllerPos :: (capture.Sources |> List.map snd)

    let centroid =
        {
            X = (anchors |> List.sumBy (fun p -> p.X)) / anchors.Length
            Y = (anchors |> List.sumBy (fun p -> p.Y)) / anchors.Length
        }

    for pos in anchors do
        claim pos |> ignore

    let spawnPos = claim (nearestFree capture taken centroid)

    let sourceContainers =
        capture.Sources
        |> List.map (fun (sourceId, pos) ->
            $"cont-{sourceId}", claim (nearestFree capture taken pos))

    let bufferPos = claim (nearestFree capture taken controllerPos)
    let containers = sourceContainers @ [ "cont-ctrl", bufferPos ]

    // The level's cluster, on one run over the room's checkerboard so the
    // three kinds share it.
    let extensions = extensionAllowance[level]
    let towers = towerAllowance[level]
    let storages = storageAllowance[level]
    let reserved = workingGround capture controllerPos

    let clustered =
        clusterTiles capture spawnPos (extensions + towers + storages) taken reserved

    for pos in clustered do
        claim pos |> ignore

    let extensionTiles = clustered |> List.truncate extensions
    let towerTiles = clustered |> List.skip extensions |> List.truncate towers
    let storageTiles = clustered |> List.skip (extensions + towers)

    // Roads and ramparts from `Colony.bootstrapLevel` up and never below
    // it: a room earning eight a tick places no road site (#209) and keeps
    // no rampart (#214).
    let furnishesDefence = level >= Colony.bootstrapLevel

    let roadTiles =
        if not furnishesDefence then
            []
        else
            let blocked = HashSet<Pos>(clustered)

            containers
            |> List.collect (fun (_, goal) -> routeBetween capture blocked spawnPos goal)
            |> List.filter (fun tile -> not (taken.Contains tile))
            |> List.distinct
            |> List.map claim

    // The ramparts the Layout places, which is a set and not a structure:
    // over every Keep structure — the spawn, every tower and the Storage
    // (ADR 0034) — and over every Post a container stands on, which are
    // ramparted with the Keep without being of it. Walkable, so each
    // shares the tile of the thing it covers the way the engine lets it,
    // and a three-source room ramparts three Posts because the set is the
    // rule's.
    let ramparts =
        if not furnishesDefence then
            []
        else
            [
                yield "rampart-spawn", spawnPos
                for index, pos in List.indexed towerTiles -> $"rampart-tower-{index}", pos
                for index, pos in List.indexed storageTiles -> $"rampart-storage-{index}", pos
                for index, (_, pos) in List.indexed sourceContainers -> $"rampart-post-{index}", pos
            ]

    let targets =
        [
            yield "spawn-1", spawnPos, Structure BuiltKind.Spawn
            yield controllerId, controllerPos, Controller

            for sourceId, pos in capture.Sources do
                yield sourceId, pos, Source

            for id, pos in containers do
                yield id, pos, Structure BuiltKind.Container

            for index, pos in List.indexed extensionTiles do
                yield $"ext-{index}", pos, Structure BuiltKind.Extension

            for index, pos in List.indexed towerTiles do
                yield $"tower-{index}", pos, Structure BuiltKind.Tower

            for index, pos in List.indexed storageTiles do
                yield $"storage-{index}", pos, Structure BuiltKind.Storage

            for index, pos in List.indexed roadTiles do
                yield $"road-{index}", pos, Structure BuiltKind.Road

            for id, pos in ramparts do
                yield id, pos, Structure BuiltKind.Rampart
        ]

    // The fleet, cast from Decide's own rules at this bank and stood where
    // its row works: an Anchor **on** each Post, a hauler at the spawn it
    // shuttles from, two workers at the controller, and — only where the
    // bank buys a standing body (ADR 0046, #187) — an upgrader at the
    // buffer it drinks from. The counts are the fixture's and deliberately
    // small: what this fleet is for is that every row of the pattern table
    // is in the pool, not that the Workforce target is reproduced here.
    //
    // The Anchor's tile is the source container's own and is taken
    // `Exactly`, not resolved outward: a container is walkable, standing on
    // one is what garrisoning a Post *is* (ADR 0020, ADR 0051), and Harvest
    // offers a work-heavy body no other footing — so an Anchor placed on
    // the nearest free tile beside its Post would be a body on a walk in
    // every rung of the fixture. On W13S28's `16,7` the point is forced:
    // its one Seat is the container's, so "beside" is range 2 from the
    // rock and out of digging range altogether.
    let fleetRows =
        [
            for index, (_, pos) in List.indexed sourceContainers do
                yield $"anchor-{index}", Decide.bodyFor Decide.anchorPattern bank, pos, Exactly

            for index, _ in List.indexed sourceContainers do
                yield $"hauler-{index}", Decide.bodyFor Decide.haulerPattern bank, spawnPos, Nearby

            for index in 0..1 do
                yield
                    $"worker-{index}",
                    Decide.bodyFor Decide.workerPattern bank,
                    controllerPos,
                    Nearby

            if bank >= 800 then
                yield "upgrader-0", Decide.bodyFor Decide.upgraderPattern bank, bufferPos, Nearby
        ]

    // Both halves of the logistics loop, cycled over the fleet: an empty
    // creep pools Harvest and Withdraw, a full one Refill, Build and
    // Upgrade.
    let fills = [| 0.0; 0.5; 1.0 |]

    let fleet =
        fleetRows
        |> List.mapi (fun index (name, body, at, station) ->
            let pos =
                match station with
                | Exactly -> at
                | Nearby -> claim (nearestFree capture taken at)

            castCreep name body fills[index % fills.Length], pos)

    let hitsOf (id: string) =
        if id.StartsWith "road-" then
            // A road in every eighth tile below half hits, so the Repair
            // family is in the pool rather than empty (ADR 0010).
            let index = int (id.Substring 5)

            if index % 8 = 3 then
                { Hits = 2100; HitsMax = 5000 }
            else
                { Hits = 4000; HitsMax = 5000 }
        elif id.StartsWith "cont-" then
            { Hits = 4000; HitsMax = 5000 }
        elif id.StartsWith "rampart-" then
            // Whole at the floor ADR 0034 derives, so the fixture's one
            // rampart is not a Repair task at every rung it stands in.
            { Hits = 100_000; HitsMax = 300_000 }
        elif id = "spawn-1" then
            { Hits = 5000; HitsMax = 5000 }
        else
            { Hits = 4000; HitsMax = 5000 }

    {
        Time = 1000
        Spawns =
            [
                {
                    Name = "Spawn1"
                    Id = "spawn-1"
                    RoomName = capture.RoomName
                    IsSpawning = false
                }
            ]
        Bank = { Available = bank; Capacity = bank }
        Refillables =
            [
                yield
                    {
                        Id = "spawn-1"
                        FreeCapacity = 0
                        Kind = BuiltKind.Spawn
                    }
                for index in 0 .. extensions - 1 do
                    yield
                        {
                            Id = $"ext-{index}"
                            FreeCapacity = 0
                            Kind = BuiltKind.Extension
                        }
                // The towers are the refillable kind whose store is not
                // the bank, so they are half drained: the Refill family
                // stays in the pool without lying about the bank above.
                for index in 0 .. towers - 1 do
                    yield
                        {
                            Id = $"tower-{index}"
                            FreeCapacity = 500
                            Kind = BuiltKind.Tower
                        }
            ]
        Sources = capture.Sources |> List.map (fun (id, _) -> { Id = id; TicksToRestock = 0 })
        Controller =
            Some
                {
                    Id = controllerId
                    Level = level
                    TicksToDowngrade = 20000
                    SafeModeAvailable = 1
                    SafeModeActive = false
                }
        RoomControl =
            Map.ofList
                [
                    capture.RoomName,
                    {
                        Owner = Ownership.Ours
                        Reservation = None
                        SafeMode = false
                    }
                ]
        ConstructionSites = []
        Creeps = fleet |> List.map fst
        Hostiles = []
        InvaderCores = []
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some capture.RoomName
                Rooms =
                    Map.ofList
                        [
                            capture.RoomName,
                            { RoomLayer.empty with
                                Terrain = capture.Terrain
                                TargetPositions =
                                    targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                                CreepPositions =
                                    fleet
                                    |> List.map (fun (creep, pos) -> creep.Name, pos)
                                    |> Map.ofList
                                Obstacles =
                                    targets
                                    |> List.choose (fun (_, pos, kind) ->
                                        match kind with
                                        | Structure built when not (isWalkable built) -> Some pos
                                        | Controller -> Some pos
                                        | _ -> None)
                                    |> Set.ofList
                                Roads = roadTiles |> Set.ofList
                            }
                        ]
                Borders = Map.ofList [ capture.RoomName, capture.Border ]
                TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
                Hits =
                    targets
                    |> List.choose (fun (id, _, kind) ->
                        match kind with
                        | Structure _ -> Some(id, hitsOf id)
                        | _ -> None)
                    |> Map.ofList
                Stores =
                    [
                        for id, _ in sourceContainers -> id, 1500
                        yield "cont-ctrl", 800
                        for index in 0 .. storages - 1 -> $"storage-{index}", 200_000
                    ]
                    |> Map.ofList
            }
        Declared = [ capture.RoomName ]
        // The rung as a [[stage]] (ADR 0052 decision 3): owned, a spawn
        // standing, and the level above — so RCL1 and RCL3 are one stage
        // apart and the road, the rampart and the feeding-tier rules read
        // that and not the number. Derived and never written down beside
        // the level, so a rung cannot be furnished as one colony and
        // decided as another.
        Stages =
            match Colony.stageOf true true (Some level) with
            | Some stage -> Map.ofList [ capture.RoomName, stage ]
            | None -> Map.empty
        // One colony over one captured room: every body in it is this
        // colony's, so there is nobody else's to carry (ADR 0052 decision
        // 1), and it raises no child, so it borrows nothing.
        Foreign = Map.empty
        Borrowed = { Rooms = [] }
    }
