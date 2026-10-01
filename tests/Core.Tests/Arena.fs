/// The combat arena (#458): a deterministic, pure-.NET world of captured rooms
/// joined by their real seams, where our side runs the **shipped** decision
/// pipeline every tick and the other side runs a script, all under the
/// engine's own physics.
///
/// **The entry point is the shell's own full tick**, not a narrower seam: the
/// arena reads its state into a `World` the way `World.ofGame` does (vision
/// per room: a room is seen while a body of ours or an owned structure stands
/// in it), lays the carried memory under it (`World.recalling`,
/// `World.latchTowers`, `World.watchExits`), cuts every living colony's view
/// (`World.creepColoniesRecalling`, `ColonyView.ofWorldRecalling`), decides
/// each (`decideUnarbitrated`) and arbitrates every room's moves once
/// (`resolveRooms`) — `Main.fullTick` less Memory. What the arena leaves out,
/// and says so: the [[stand-down]] gate (the Raid log is Memory's, so every
/// colony decides under `StandDown.none`, its fight record alone carried, as
/// `Observe.foldFights` folds it), harassment casting
/// (`HarassCasting.none`), the round-robin replan (every colony is
/// `ReplanTurn.Now`), and light ticks (`LightTick`): every tick is a full one.
///
/// **The physics is the engine's** (`@screeps/engine/src/processor`), cited
/// per rule below, with `Engine`'s constants and `Engine.liveParts` shared
/// with `Facts.squadFight` rather than re-spelt. Not modelled: spawning,
/// economy, roads, ramparts, boosts, pulling, portals, power creeps.
module Fabot.Core.Tests.Arena

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures

/// Who a body answers to. A player's creep crosses borders; an NPC's never
/// does (`creeps/tick.js`: the exit transfer skips users '2' and '3').
[<RequireQualifiedAccess>]
type Side =
    | Ours
    | Player of username: string
    | Npc of username: string

/// Whom a scripted body picks among the enemies it can see.
type Focus =
    /// The nearest, then the lowest hits.
    | Nearest
    /// The lowest hits, then the nearest.
    | LowestHits
    /// Unarmed healers first (Odiodin's order, `docs/research/squads.md`
    /// §1.4–§1.5), then the lowest hits.
    | HealersFirst

/// One engine intent of one body, as the arena performs it.
[<RequireQualifiedAccess>]
type Act =
    | Move of Direction
    | Attack of target: string
    | RangedAttack of target: string
    | RangedMassAttack
    | Heal of target: string
    | RangedHeal of target: string
    | AttackController of controllerRoom: string

/// A scripted body's plan, re-read every tick from the start-of-tick state.
/// Every behaviour but `Do` also fights what stands in reach of it
/// (`fightInReach`).
type Behaviour =
    /// Exactly these acts every tick and nothing else: the physics tests'
    /// hand on the engine.
    | Do of Act list
    /// Stand still.
    | Hold
    /// Walk to range 1 of the focused enemy; with a leash, only enemies
    /// within that range of the anchor are chased, and the body walks back
    /// to the anchor when there are none.
    | Chase of Focus * leash: (RoomPos * int) option
    /// Stay beside the leader, stepping into the tile it vacates (the snake,
    /// §1.2), healing the most hurt beside it and pre-healing the leader.
    | Follow of leader: string
    /// Walk to the controller and `attackController` it.
    | Tap of controller: RoomPos
    /// Keep every enemy melee at range 2 or more and the focused enemy at
    /// `keep`, shooting it.
    | Kite of Focus * keep: int
    /// The NPC: walk at the nearest enemy and hit it with whatever it has.
    | Invade
    /// Walk to a tile, across rooms if it is in another one.
    | GoTo of RoomPos
    /// Each behaviour until the arena tick beside it (exclusive); the last
    /// runs on.
    | Phases of (int * Behaviour) list
    /// Below `below` hits fall back to `refuge` and heal; back to `inner` at
    /// `resume` hits (#455's enemy: the hurt pair that leaves and returns).
    | Retreat of below: int * refuge: RoomPos * resume: int * inner: Behaviour

/// One creep in the arena: its body head first, the hits it has left (which
/// say which parts still act, `Engine.liveParts`), where it stands, its
/// fatigue and its store.
type Body =
    {
        Id: string
        Side: Side
        Parts: BodyPart list
        Hits: int
        At: RoomPos
        Fatigue: int
        Energy: int
        TicksToLive: int
        /// None for a body of ours, which the decision pipeline drives.
        Script: Behaviour option
        /// Whether a `Retreat` is falling back; the one piece of script state.
        Retreating: bool
    }

/// A controller in an arena room. `Owner` is the room's ownership, and
/// `Username` the rival's name when it is one.
type ArenaController =
    {
        Id: string
        At: Pos
        Owner: Ownership
        Username: string option
        Level: int
        TicksToDowngrade: int
        SafeModeAvailable: int
        /// The first tick safe mode no longer runs; 0 for none.
        SafeModeUntil: int
        /// The first tick upgrades are allowed again
        /// (`CONTROLLER_ATTACK_BLOCKED_UPGRADE` after a landed
        /// `attackController`); 0 for never blocked.
        UpgradeBlockedUntil: int
    }

/// A tower of ours and the energy it holds.
type ArenaTower = { Id: string; At: Pos; Energy: int }

/// One captured room in the arena, with what stands on it beyond terrain.
type ArenaRoom =
    {
        Capture: RoomCapture
        Controller: ArenaController option
        /// Our spawns here, by spawn name: a creep named after one is that
        /// colony's (`Colony.castBy`).
        Spawns: (string * Pos) list
        Towers: ArenaTower list
        /// The home's bank, for a room with a spawn.
        Bank: int
    }

/// What the decision layer remembers from one tick to the next, carried
/// exactly as `Main` carries it in heap.
type Carried =
    {
        Assignments: Assignments
        Memos: Map<string, PlanMemo>
        Sightings: Map<string, RoomSighting>
        Towered: Set<string>
        ExitWatches: Map<string, ExitWatch>
        LastPositions: Map<string, RoomPos>
        /// Each colony's fight record (`Observe.foldFights`), by home: the one
        /// part of the Raid log the arena carries.
        Fought: Map<string, Map<string, FightLatch>>
    }

/// The arena's whole state between ticks.
type Arena =
    {
        /// The game tick the next step runs as.
        Time: int
        /// The next step's index, from 0: what `Phases` counts in.
        Tick: int
        Rooms: Map<string, ArenaRoom>
        Bodies: Body list
        Colonies: Colony list
        Carried: Carried
    }

/// What happened in a tick beyond positions and hits.
type ArenaEvent =
    | Died of id: string
    /// A body ended the tick on an exit tile and was moved to the landing
    /// tile of the neighbour room (`creeps/tick.js`). The move is
    /// `global.js`' bare position write: nothing checks the landing, so two
    /// bodies can share it until one steps off.
    | Crossed of id: string * from: RoomPos * landing: RoomPos
    /// The same, into a room the arena does not hold: the body leaves.
    | Exited of id: string * from: RoomPos
    | ControllerAttacked of room: string * by: string
    | SafeModeActivated of room: string

/// One body at the end of a tick.
type Snapshot =
    {
        Id: string
        Side: Side
        At: RoomPos
        Hits: int
        Fatigue: int
    }

/// One tick of the trace.
type TickTrace =
    {
        Tick: int
        Time: int
        /// Every Intent our pipeline emitted this tick, whether the arena
        /// performs it or not.
        Ours: Intent list
        /// Every scripted body's acts.
        Theirs: (string * Act) list
        Events: ArenaEvent list
        /// The living bodies at the end of the tick.
        Bodies: Snapshot list
    }

let private partHitsOf (parts: BodyPart list) = Engine.partHits * List.length parts

/// A body of the given parts at full hits, standing still, a full life left.
let body
    (id: string)
    (side: Side)
    (parts: BodyPart list)
    (at: RoomPos)
    (script: Behaviour option)
    : Body =
    {
        Id = id
        Side = side
        Parts = parts
        Hits = partHitsOf parts
        At = at
        Fatigue = 0
        Energy = 0
        TicksToLive = Engine.creepLifetime
        Script = script
        Retreating = false
    }

/// A body spelt as part counts in order, head first: `parts [ Move, 21; RangedAttack, 14 ]`.
let parts (counts: (BodyPart * int) list) : BodyPart list =
    counts |> List.collect (fun (part, n) -> List.replicate n part)

/// A captured room with nothing of anybody's standing on it.
let room (name: string) : ArenaRoom =
    {
        Capture = load name
        Controller = None
        Spawns = []
        Towers = []
        Bank = 0
    }

/// The room's captured controller, owned as given.
let withController
    (owner: Ownership)
    (username: string option)
    (level: int)
    (safeModes: int)
    (r: ArenaRoom)
    : ArenaRoom =
    match r.Capture.RealController with
    | Some(id, at) ->
        { r with
            Controller =
                Some
                    {
                        Id = id
                        At = at
                        Owner = owner
                        Username = username
                        Level = level
                        TicksToDowngrade = 20_000
                        SafeModeAvailable = safeModes
                        SafeModeUntil = 0
                        UpgradeBlockedUntil = 0
                    }
        }
    | None -> failwithf "%s has no controller" r.Capture.RoomName

/// A spawn of ours standing on a tile of the room, which must be ground.
let withSpawn (name: string) (at: Pos) (bank: int) (r: ArenaRoom) : ArenaRoom =
    match TerrainGrid.tryFind at r.Capture.Terrain with
    | Some Wall
    | None -> failwithf "%s: the spawn tile %d,%d is not ground" r.Capture.RoomName at.X at.Y
    | Some _ ->
        { r with
            Spawns = r.Spawns @ [ name, at ]
            Bank = bank
        }

/// A colony declared over a home, with nothing else.
let colony (home: string) : Colony =
    {
        Home = home
        Outposts = []
        Errands = []
        Salvage = []
        Mother = None
        Consignee = None
        Perimeter = []
    }

/// An outpost declared off a capture's own engine ids.
let outpostOf (capture: RoomCapture) : Outpost =
    match capture.RealController with
    | Some(id, at) ->
        {
            RoomName = capture.RoomName
            Sources =
                capture.RealSources
                |> List.map (fun (sid, pos) -> sid, RoomPos.at capture.RoomName pos)
            Controller = id, RoomPos.at capture.RoomName at
        }
    | None -> failwithf "%s has no controller to declare an outpost by" capture.RoomName

/// An arena over these rooms, colonies and bodies, starting at `time`.
let arena (time: int) (rooms: ArenaRoom list) (colonies: Colony list) (bodies: Body list) : Arena =
    {
        Time = time
        Tick = 0
        Rooms = rooms |> List.map (fun r -> r.Capture.RoomName, r) |> Map.ofList
        Bodies = bodies
        Colonies = colonies
        Carried =
            {
                Assignments = Map.empty
                Memos = Map.empty
                Sightings = Map.empty
                Towered = Set.empty
                ExitWatches = Map.empty
                LastPositions = Map.empty
                Fought = Map.empty
            }
    }

// ---------------------------------------------------------------------------
// Geometry and the engine's tile rules
// ---------------------------------------------------------------------------

/// Terrain at any tile of a room, the border ring included.
let terrainAt (r: ArenaRoom) (tile: Pos) : Terrain =
    if Seam.onRing tile then
        Map.tryFind tile r.Capture.Border |> Option.defaultValue Wall
    else
        TerrainGrid.tryFind tile r.Capture.Terrain |> Option.defaultValue Wall

/// The tiles the engine's OBSTACLE_OBJECT_TYPES hold in a room: sources,
/// minerals (every rock), the controller, spawns and towers
/// (`movement.js` `checkObstacleAtXY`).
let private structureTiles (r: ArenaRoom) : Set<Pos> =
    Set.ofList
        [
            for _, pos in r.Capture.Rocks -> pos
            for _, pos in Option.toList r.Capture.RealController -> pos
            for _, pos in r.Spawns -> pos
            for t in r.Towers -> t.At
        ]

let private isEdge (tile: Pos) = Seam.onRing tile

let private stepOf (d: Direction) : int * int =
    match d with
    | Top -> 0, -1
    | TopRight -> 1, -1
    | Right -> 1, 0
    | BottomRight -> 1, 1
    | Bottom -> 0, 1
    | BottomLeft -> -1, 1
    | Left -> -1, 0
    | TopLeft -> -1, -1

/// Where a step lands, clamped to the room as `movement.js` `add` clamps it.
let private stepTo (at: RoomPos) (d: Direction) : RoomPos =
    let dx, dy = stepOf d
    let clamp v = max 0 (min Seam.exitEdge v)

    { at with
        X = clamp (at.X + dx)
        Y = clamp (at.Y + dy)
    }

/// The neighbour room and landing tile a body on this exit tile is moved to
/// at the end of the tick (`creeps/tick.js`: x = 0 first, then y = 0, x = 49,
/// y = 49).
let landingOf (at: RoomPos) : (string * Pos) option =
    let adjacent = RoomName.adjacent at.Room // north, east, south, west

    let pick index (landing: Pos) =
        List.tryItem index adjacent |> Option.map (fun name -> name, landing)

    if at.X = 0 then pick 3 { X = Seam.exitEdge; Y = at.Y }
    elif at.Y = 0 then pick 0 { X = at.X; Y = Seam.exitEdge }
    elif at.X = Seam.exitEdge then pick 1 { X = 0; Y = at.Y }
    elif at.Y = Seam.exitEdge then pick 2 { X = at.X; Y = 0 }
    else None

// ---------------------------------------------------------------------------
// What a body can do this tick
// ---------------------------------------------------------------------------

/// The parts that act at these hits (`calcBodyEffectiveness` over
/// `_recalc-body`'s head-first loss), shared with `squadFight`.
let live (b: Body) = Engine.liveParts b.Parts b.Hits

let private activeCount (b: Body) (part: BodyPart) = partCountIn (live b) part

let private hitsMax (b: Body) = partHitsOf b.Parts

/// Whether two bodies fight each other: different sides, and not us beside
/// an ally of ours (`Colony.isAlly`).
let hostileTo (a: Body) (b: Body) : bool =
    let allied (x: Side) (y: Side) =
        match x, y with
        | Side.Ours, Side.Player name -> Colony.isAlly name
        | _ -> false

    a.Side <> b.Side && not (allied a.Side b.Side) && not (allied b.Side a.Side)

let private username (side: Side) =
    match side with
    | Side.Ours -> "fabot"
    | Side.Player name
    | Side.Npc name -> name

let private range (a: RoomPos) (b: RoomPos) = RoomPos.range a b

let private within (reach: int) (a: RoomPos) (b: RoomPos) =
    range a b |> Option.exists (fun r -> r <= reach)

// ---------------------------------------------------------------------------
// The World the shell would read off this state
// ---------------------------------------------------------------------------

/// Whether vision answers for a room this tick: a body of ours or an owned
/// structure stands in it.
let private seen (a: Arena) (name: string) (r: ArenaRoom) =
    a.Bodies |> List.exists (fun b -> b.Side = Side.Ours && b.At.Room = name)
    || not (List.isEmpty r.Spawns)
    || not (List.isEmpty r.Towers)
    || r.Controller |> Option.exists (fun c -> c.Owner = Ownership.Ours)

let private creepInfo (a: Arena) (b: Body) : CreepInfo =
    let active = live b
    let carry = partCountIn active Carry * Engine.carryPartCapacity

    {
        Name = b.Id
        TicksToLive = b.TicksToLive
        Fatigue = b.Fatigue
        Hits = { Hits = b.Hits; HitsMax = hitsMax b }
        Energy = b.Energy
        Thorium = 0
        FreeCapacity = max 0 (carry - b.Energy)
        Body = active |> List.countBy id |> Map.ofList
        Moved =
            match Map.tryFind b.Id a.Carried.LastPositions with
            | Some last -> last <> b.At
            | None -> false
    }

/// One room's facts, seen or blind, as `World.factsOf` builds them.
let private factsOf (a: Arena) (name: string) (r: ArenaRoom) : RoomFacts =
    let spawns =
        r.Spawns
        |> List.map (fun (spawn, _) ->
            {
                Name = spawn
                Id = $"spawn-{spawn}"
                RoomName = name
                IsSpawning = false
            })

    if not (seen a name r) then
        { RoomFacts.empty with
            Layer =
                { RoomLayer.empty with
                    Terrain = r.Capture.Terrain
                }
            Border = r.Capture.Border
            Spawns = spawns
        }
    else
        let controller = r.Capture.RealController

        let targets =
            [
                for id, pos in r.Capture.RealSources -> id, pos, Source
                for id, pos in Option.toList controller -> id, pos, Controller
                for id, pos in r.Capture.RealMinerals -> id, pos, Mineral
                for spawn, pos in r.Spawns -> $"spawn-{spawn}", pos, Structure BuiltKind.Spawn
                for t in r.Towers -> t.Id, t.At, Structure BuiltKind.Tower
            ]

        let here = a.Bodies |> List.filter (fun b -> b.At.Room = name)

        { RoomFacts.empty with
            Layer =
                {
                    Terrain = r.Capture.Terrain
                    TargetPositions =
                        targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                    CreepPositions =
                        here
                        |> List.filter (fun b -> b.Side = Side.Ours)
                        |> List.map (fun b -> b.Id, RoomPos.pos b.At)
                        |> Map.ofList
                    // The shell's obstacle census: obstacle structures, the
                    // controller and the minerals (`World.seenFacts`).
                    Obstacles =
                        [
                            for _, pos in Option.toList controller -> pos
                            for _, pos in r.Capture.RealMinerals -> pos
                            for _, pos in r.Spawns -> pos
                            for t in r.Towers -> t.At
                        ]
                        |> Set.ofList
                    Roads = Set.empty
                    RivalSites = Set.empty
                }
            Border = r.Capture.Border
            TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
            Thorium = r.Capture.RealMinerals |> List.map (fun (id, _) -> id, 20_000) |> Map.ofList
            Control =
                Some
                    {
                        Owner =
                            r.Controller
                            |> Option.map (fun c -> c.Owner)
                            |> Option.defaultValue Ownership.Unowned
                        Reservation = None
                        SafeMode = r.Controller |> Option.exists (fun c -> c.SafeModeUntil > a.Time)
                        Sign = None
                    }
            Controller =
                r.Controller
                |> Option.filter (fun c -> c.Owner = Ownership.Ours)
                |> Option.map (fun c ->
                    {
                        Id = c.Id
                        Level = c.Level
                        TicksToDowngrade = c.TicksToDowngrade
                        SafeModeAvailable = c.SafeModeAvailable
                        SafeModeActive = c.SafeModeUntil > a.Time
                    })
            Energy =
                if List.isEmpty r.Spawns then
                    { Available = 0; Capacity = 0 }
                else
                    {
                        Available = r.Bank
                        Capacity = r.Bank
                    }
            Spawns = spawns
            Refillables =
                [
                    for spawn, _ in r.Spawns ->
                        {
                            Id = $"spawn-{spawn}"
                            FreeCapacity = 0
                            Kind = BuiltKind.Spawn
                        }
                    for t in r.Towers ->
                        {
                            Id = t.Id
                            FreeCapacity = Engine.towerCapacity - t.Energy
                            Kind = BuiltKind.Tower
                        }
                ]
            Sources =
                r.Capture.RealSources
                |> List.map (fun (id, _) -> { Id = id; TicksToRestock = 0 })
            Hostiles =
                here
                |> List.filter (fun b -> b.Side <> Side.Ours)
                |> List.map (fun b ->
                    {
                        Id = b.Id
                        Owner = username b.Side
                        Pos = b.At
                        // The whole body, dead parts included, as
                        // `World.seenFacts` reads `c.body`.
                        Body = b.Parts
                        Hits = b.Hits
                        TicksToLive = b.TicksToLive
                    })
        }

/// This tick's World, read off the arena as `World.ofGame` reads the engine,
/// with last tick's memory laid under it as `Main.fullTick` lays it.
let worldOf (a: Arena) : World =
    let tuning = Tuning.defaults
    let rooms = a.Rooms |> Map.map (fun name r -> factsOf a name r)

    let sightings =
        a.Rooms
        |> Map.filter (fun name r -> seen a name r)
        |> Map.map (fun name r ->
            let facts = Map.find name rooms

            {
                Tick = a.Time
                Targets = lazy (facts.TargetKinds |> Map.keys |> Set.ofSeq)
                Rival =
                    r.Controller
                    |> Option.filter (fun c -> c.Owner = Ownership.Rival)
                    |> Option.bind (fun c -> c.Username)
            })

    {
        Time = a.Time
        Rooms = rooms
        Creeps =
            a.Bodies
            |> List.filter (fun b -> b.Side = Side.Ours)
            |> List.map (fun b ->
                {
                    Room = b.At.Room
                    Info = creepInfo a b
                })
        Sightings = sightings
        Towered = Set.empty
        ExitWatches = Map.empty
    }
    |> World.recalling a.Carried.Sightings
    |> World.latchTowers tuning a.Carried.Towered
    |> World.watchExits tuning a.Carried.ExitWatches

/// Our side's tick: every living colony's view cut from the world and decided,
/// every room's moves arbitrated once. The Intents, the next assignments and
/// memos, and the world's carried memory.
let private decideOurs (a: Arena) : Intent list * Carried =
    let world = worldOf a
    let tuning = Tuning.defaults
    let living = World.living a.Colonies world
    let joins = JoinTable()
    let casting = HarassCasting.none

    let holders =
        World.creepColoniesRecalling joins tuning a.Colonies casting living Map.empty world

    let fought home =
        Map.tryFind home a.Carried.Fought |> Option.defaultValue Map.empty

    let decided =
        living
        |> List.map (fun c ->
            let gate =
                { StandDown.none with
                    Fought = fought c.Home
                }

            let view =
                ColonyView.ofWorldRecalling joins tuning a.Colonies casting gate holders world c

            (c.Home, Observe.foldFights view (fought c.Home)),
            (c.Home,
             decideUnarbitrated
                 view
                 a.Carried.Assignments
                 Set.empty
                 (Map.tryFind c.Home a.Carried.Memos)
                 ReplanTurn.Now))

    let decisions = decided |> List.map snd

    let moves, _ = resolveRooms (decisions |> List.map (fun (_, d) -> d.Movement))

    let intents = (decisions |> List.collect (fun (_, d) -> d.Intents)) @ moves

    // The assignments one flat table, as the shell writes them.
    let assignments =
        (Map.empty, decisions)
        ||> List.fold (fun acc (_, d) -> Map.fold (fun m k v -> Map.add k v m) acc d.Assignments)

    intents,
    { a.Carried with
        Assignments = assignments
        Memos =
            (a.Carried.Memos, decisions)
            ||> List.fold (fun memos (home, d) -> Map.add home d.Memo memos)
        Sightings = world.Sightings
        Towered = world.Towered
        ExitWatches = world.ExitWatches
        Fought = decided |> List.map fst |> Map.ofList
    }

// ---------------------------------------------------------------------------
// The scripts
// ---------------------------------------------------------------------------

/// The walk graph across every arena room, one node per tile per room: a
/// tile is passable when its terrain is not wall and no obstacle structure
/// stands on it; an exit tile leads only to its landing (a player's body;
/// an NPC never steps onto one). Bodies block every tile but a goal.
let private stepToward (a: Arena) (mover: Body) (goals: Set<RoomPos>) : Direction option =
    if Set.isEmpty goals || Set.contains mover.At goals then
        None
    else
        let names = a.Rooms |> Map.toList |> List.map fst |> Array.ofList
        let indexOf = names |> Array.mapi (fun i n -> n, i) |> Map.ofArray
        let side = Engine.roomSide
        let total = names.Length * side * side

        let blocked = names |> Array.map (fun n -> structureTiles (Map.find n a.Rooms))

        let occupied =
            a.Bodies
            |> List.filter (fun b -> b.Id <> mover.Id)
            |> List.map (fun b -> b.At)
            |> Set.ofList

        let npc =
            match mover.Side with
            | Side.Npc _ -> true
            | _ -> false

        let node (at: RoomPos) =
            Map.find at.Room indexOf * side * side + at.X * side + at.Y

        let posOf (n: int) =
            let r = n / (side * side)
            let rest = n % (side * side)

            {
                Room = names[r]
                X = rest / side
                Y = rest % side
            }

        let passable (at: RoomPos) =
            Map.containsKey at.Room indexOf
            && at.X >= 0
            && at.X <= Seam.exitEdge
            && at.Y >= 0
            && at.Y <= Seam.exitEdge
            && terrainAt (Map.find at.Room a.Rooms) (RoomPos.pos at) <> Wall
            && not (Set.contains (RoomPos.pos at) blocked[Map.find at.Room indexOf])
            && not (npc && Seam.onRing (RoomPos.pos at))
            && (Set.contains at goals || not (Set.contains at occupied))

        // A tile the walk was carried onto (a landing, or where the mover
        // stands) is left like any other; an exit tile stepped onto leads to
        // its landing and nowhere else.
        let carried = Array.create total false

        let successors (n: int) (at: RoomPos) =
            let tile = RoomPos.pos at

            if Seam.isExit tile && not carried[n] then
                match landingOf at with
                | Some(roomName, landing) when Map.containsKey roomName indexOf ->
                    [ RoomPos.at roomName landing ]
                | _ -> []
            else
                [ Top; TopRight; Right; BottomRight; Bottom; BottomLeft; Left; TopLeft ]
                |> List.map (stepTo at)
                |> List.filter (fun next -> next <> at && passable next)

        let parent = Array.create total -1
        let start = node mover.At
        parent[start] <- start
        carried[start] <- true
        let queue = Queue<int>()
        queue.Enqueue start
        let mutable found = -1

        while found < 0 && queue.Count > 0 do
            let n = queue.Dequeue()
            let here = posOf n

            for next in successors n here do
                let m = node next

                if found < 0 && parent[m] < 0 then
                    parent[m] <- n
                    carried[m] <- next.Room <> here.Room

                    if Set.contains next goals then
                        found <- m
                    else
                        queue.Enqueue m

        if found < 0 then
            None
        else
            let mutable n = found

            while parent[n] <> start do
                n <- parent[n]

            let first = posOf n

            if first.Room <> mover.At.Room then
                None
            else
                directionTo (RoomPos.pos mover.At) (RoomPos.pos first)

/// The tiles of a room within `reach` of a tile, the tile itself included.
let private ringAround (reach: int) (at: RoomPos) : Set<RoomPos> =
    tilesWithin reach (RoomPos.pos at)
    |> List.filter (fun p -> p.X >= 0 && p.Y >= 0 && p.X <= Seam.exitEdge && p.Y <= Seam.exitEdge)
    |> List.map (RoomPos.at at.Room)
    |> Set.ofList

let private isHealer (b: Body) =
    let active = live b

    List.contains Heal active
    && not (List.contains Attack active)
    && not (List.contains RangedAttack active)

/// The focused enemy among candidates, deterministic to the id.
let private focusOf (focus: Focus) (from: Body) (candidates: Body list) : Body option =
    let distance (b: Body) =
        range from.At b.At |> Option.defaultValue System.Int32.MaxValue

    match focus with
    | Nearest -> candidates |> List.sortBy (fun b -> distance b, b.Hits, b.Id)
    | LowestHits -> candidates |> List.sortBy (fun b -> b.Hits, distance b, b.Id)
    | HealersFirst ->
        candidates
        |> List.sortBy (fun b -> (if isHealer b then 0 else 1), b.Hits, distance b, b.Id)
    |> List.tryHead

let private enemiesOf (a: Arena) (b: Body) =
    a.Bodies |> List.filter (fun o -> o.At.Room = b.At.Room && hostileTo b o)

let private friendsOf (a: Arena) (b: Body) =
    a.Bodies |> List.filter (fun o -> o.At.Room = b.At.Room && o.Side = b.Side)

/// What a body fights with this tick, wherever its legs take it: its
/// ATTACK on the focused adjacent enemy, its RANGED_ATTACK on the focused
/// enemy in three, and its HEAL on the most hurt friend in reach when no
/// melee act claims the tick (`creeps/intents.js`: heal suppresses attack).
let private fightInReach (a: Arena) (focus: Focus) (b: Body) : Act list =
    let enemies = enemiesOf a b
    let active = live b

    let melee =
        if List.contains Attack active then
            enemies
            |> List.filter (fun e -> within Engine.meleeRange b.At e.At)
            |> focusOf focus b
            |> Option.map (fun e -> Act.Attack e.Id)
        else
            None

    let shot =
        if List.contains RangedAttack active then
            enemies
            |> List.filter (fun e -> within Engine.rangedRange b.At e.At)
            |> focusOf focus b
            |> Option.map (fun e -> Act.RangedAttack e.Id)
        else
            None

    let heal =
        if melee.IsNone && List.contains Heal active then
            let hurt =
                friendsOf a b
                |> List.filter (fun f -> f.Hits < hitsMax f)
                |> List.sortBy (fun f -> -(hitsMax f - f.Hits), f.Id)

            match hurt |> List.tryFind (fun f -> within Engine.meleeRange b.At f.At) with
            | Some f -> Some(Act.Heal f.Id)
            | None ->
                hurt
                |> List.tryFind (fun f -> within Engine.rangedRange b.At f.At)
                |> Option.map (fun f -> Act.RangedHeal f.Id)
        else
            None

    List.choose id [ melee; shot; heal ]

/// The tile a body steps to under a kite: away from every enemy melee
/// within two, else toward `keep` of the target, else nowhere.
let private kiteStep (a: Arena) (b: Body) (target: Body) (keep: int) : Direction option =
    let threats = enemiesOf a b |> List.filter (fun e -> List.contains Attack (live e))

    let nearestThreat (at: RoomPos) =
        threats
        |> List.choose (fun e -> range at e.At)
        |> function
            | [] -> System.Int32.MaxValue
            | rs -> List.min rs

    if nearestThreat b.At <= 2 then
        let r = Map.find b.At.Room a.Rooms
        let blocked = structureTiles r

        let occupied =
            a.Bodies
            |> List.filter (fun o -> o.Id <> b.Id)
            |> List.map (fun o -> o.At)
            |> Set.ofList

        [ Top; TopRight; Right; BottomRight; Bottom; BottomLeft; Left; TopLeft ]
        |> List.map (fun d -> d, stepTo b.At d)
        |> List.filter (fun (_, at) ->
            let tile = RoomPos.pos at

            not (Seam.onRing tile)
            && terrainAt r tile <> Wall
            && not (Set.contains tile blocked)
            && not (Set.contains at occupied))
        |> List.sortBy (fun (d, at) ->
            -(nearestThreat at),
            abs ((range at target.At |> Option.defaultValue 99) - keep),
            (if terrainAt r (RoomPos.pos at) = Swamp then 1 else 0),
            d)
        |> List.tryHead
        |> Option.filter (fun (_, at) -> nearestThreat at > nearestThreat b.At)
        |> Option.map fst
    elif not (within keep b.At target.At) then
        stepToward a b (ringAround keep target.At)
    else
        None

/// The behaviour in force this tick, `Phases` and `Retreat` resolved.
let rec private current (tick: int) (b: Body) (behaviour: Behaviour) : Behaviour =
    match behaviour with
    | Phases phases ->
        match phases |> List.tryFind (fun (until, _) -> tick < until) with
        | Some(_, inner) -> current tick b inner
        | None ->
            match List.tryLast phases with
            | Some(_, inner) -> current tick b inner
            | None -> Hold
    | Retreat(_, refuge, _, inner) -> if b.Retreating then GoTo refuge else current tick b inner
    | other -> other

/// The retreat latch, read before the behaviour: below the line it falls
/// back, at the resume line it returns.
let rec private retreatLatch (tick: int) (b: Body) (behaviour: Behaviour) : bool =
    match behaviour with
    | Retreat(below, _, resume, _) ->
        if b.Hits < below then true
        elif b.Hits >= resume then false
        else b.Retreating
    | Phases phases ->
        match
            phases
            |> List.tryFind (fun (until, _) -> tick < until)
            |> Option.orElse (List.tryLast phases)
        with
        | Some(_, inner) -> retreatLatch tick b inner
        | None -> false
    | _ -> b.Retreating

/// One scripted body's acts this tick. `planned` holds the moves of the
/// bodies scripted before it, which a follower reads to step into the tile
/// its leader is leaving.
let private scriptActs (a: Arena) (planned: Map<string, RoomPos>) (b: Body) : Act list =
    let behaviour = b.Script |> Option.defaultValue Hold |> current a.Tick b

    let move (d: Direction option) =
        d |> Option.map Act.Move |> Option.toList

    let enemies = enemiesOf a b

    match behaviour with
    | Do acts -> acts
    | Hold -> fightInReach a Nearest b
    | GoTo goal -> fightInReach a Nearest b @ move (stepToward a b (Set.singleton goal))
    | Chase(focus, leash) ->
        let candidates =
            match leash with
            | Some(anchor, reach) -> enemies |> List.filter (fun e -> within reach anchor e.At)
            | None -> enemies

        let goal =
            match focusOf focus b candidates, leash with
            | Some target, _ -> ringAround Engine.meleeRange target.At |> Set.remove target.At
            | None, Some(anchor, _) -> Set.singleton anchor
            | None, None -> Set.empty

        let fights = fightInReach a focus b

        let adjacent =
            fights
            |> List.exists (function
                | Act.Attack _ -> true
                | _ -> false)

        fights @ (if adjacent then [] else move (stepToward a b goal))
    | Follow leaderId ->
        let leader = a.Bodies |> List.tryFind (fun o -> o.Id = leaderId)

        let healing =
            if List.contains Heal (live b) then
                let friends = friendsOf a b

                let hurt =
                    friends
                    |> List.filter (fun f ->
                        f.Hits < hitsMax f && within Engine.meleeRange b.At f.At)
                    |> List.sortBy (fun f -> -(hitsMax f - f.Hits), f.Id)

                match hurt, leader with
                | f :: _, _ -> [ Act.Heal f.Id ]
                | [], Some l when within Engine.meleeRange b.At l.At -> [ Act.Heal l.Id ]
                | [], Some l when within Engine.rangedRange b.At l.At -> [ Act.RangedHeal l.Id ]
                | [], _ -> [ Act.Heal b.Id ]
            else
                []

        let step =
            match leader with
            | None -> None
            | Some l ->
                let next = Map.tryFind l.Id planned |> Option.defaultValue l.At

                if next <> l.At && within 1 b.At l.At && next.Room = l.At.Room then
                    directionTo (RoomPos.pos b.At) (RoomPos.pos l.At)
                elif within 1 b.At next then
                    None
                else
                    stepToward a b (ringAround 1 next |> Set.remove next)

        healing @ move step
    | Tap controller ->
        if within 1 b.At controller then
            [ Act.AttackController controller.Room ]
        else
            move (stepToward a b (ringAround 1 controller |> Set.remove controller))
    | Kite(focus, keep) ->
        match focusOf focus b enemies with
        | None -> fightInReach a focus b
        | Some target ->
            let shoot =
                if
                    within Engine.rangedRange b.At target.At && List.contains RangedAttack (live b)
                then
                    [ Act.RangedAttack target.Id ]
                else
                    fightInReach a focus b
                    |> List.filter (function
                        | Act.RangedAttack _ -> true
                        | _ -> false)

            let heal =
                if List.contains Heal (live b) && b.Hits < hitsMax b then
                    [ Act.Heal b.Id ]
                else
                    []

            shoot @ heal @ move (kiteStep a b target keep)
    | Invade ->
        let fights = fightInReach a Nearest b

        let goal =
            match focusOf Nearest b enemies with
            | Some target ->
                if List.contains Attack (live b) || not (List.contains RangedAttack (live b)) then
                    ringAround 1 target.At |> Set.remove target.At
                else
                    ringAround Engine.rangedRange target.At
            | None -> Set.empty

        let engaged =
            fights
            |> List.exists (function
                | Act.Attack _ -> true
                | _ -> false)

        fights @ (if engaged then [] else move (stepToward a b goal))
    | Phases _
    | Retreat _ -> []

/// Every scripted body's acts, leaders before followers.
let private theirActs (a: Arena) : (string * Act) list =
    let scripted =
        a.Bodies
        |> List.filter (fun b -> b.Script.IsSome)
        |> List.sortBy (fun b -> b.Id)

    let isFollower (b: Body) =
        match b.Script |> Option.map (current a.Tick b) with
        | Some(Follow _) -> true
        | _ -> false

    let leaders, followers = scripted |> List.partition (isFollower >> not)

    let acted = leaders |> List.map (fun b -> b, scriptActs a Map.empty b)

    let planned =
        acted
        |> List.choose (fun (b, acts) ->
            acts
            |> List.tryPick (function
                | Act.Move d -> Some(b.Id, stepTo b.At d)
                | _ -> None))
        |> Map.ofList

    acted @ (followers |> List.map (fun b -> b, scriptActs a planned b))
    |> List.collect (fun (b, acts) -> acts |> List.map (fun act -> b.Id, act))

// ---------------------------------------------------------------------------
// The engine
// ---------------------------------------------------------------------------

/// The name of an act in `creeps/intents.js`' table: one intent per name per
/// creep, the last written winning.
let private actName (act: Act) =
    match act with
    | Act.Move _ -> "move"
    | Act.Attack _ -> "attack"
    | Act.RangedAttack _ -> "rangedAttack"
    | Act.RangedMassAttack -> "rangedMassAttack"
    | Act.Heal _ -> "heal"
    | Act.RangedHeal _ -> "rangedHeal"
    | Act.AttackController _ -> "attackController"

/// `creeps/intents.js` `priorities`: the acts that suppress each act.
let private suppressedBy (name: string) : string list =
    match name with
    | "rangedHeal" -> [ "heal" ]
    | "attackController" -> [ "rangedHeal"; "heal" ]
    | "attack" -> [ "attackController"; "rangedHeal"; "heal" ]
    | "rangedMassAttack" -> [ "rangedHeal" ]
    | "rangedAttack" -> [ "rangedMassAttack"; "rangedHeal" ]
    | _ -> []

/// The acts the engine performs of one body's intents.
let private admitted (acts: Act list) : Act list =
    let byName =
        (Map.empty, acts) ||> List.fold (fun m act -> Map.add (actName act) act m)

    byName
    |> Map.toList
    |> List.filter (fun (name, _) ->
        suppressedBy name |> List.forall (fun s -> not (Map.containsKey s byName)))
    |> List.map snd

/// Whether safe mode in a body's room stops it acting: a room whose
/// controller runs safe mode for somebody else (`attack.js`, `heal.js`).
let private safeModeStops (a: Arena) (b: Body) =
    match Map.tryFind b.At.Room a.Rooms |> Option.bind (fun r -> r.Controller) with
    | Some c when c.SafeModeUntil > a.Time ->
        match c.Owner, b.Side with
        | Ownership.Ours, Side.Ours -> false
        | Ownership.Ours, _ -> true
        | _, Side.Ours -> true
        | _ -> false
    | _ -> false

/// One tick of damage and heal, accumulated as `_damageToApply` and
/// `_healToApply` off start-of-tick positions and parts.
type private Ledger =
    {
        Damage: Dictionary<string, int>
        Healing: Dictionary<string, int>
    }

let private credit (table: Dictionary<string, int>) (id: string) (amount: int) =
    match table.TryGetValue id with
    | true, held -> table[id] <- held + amount
    | _ -> table[id] <- amount

/// Perform one body's combat acts (`attack.js`, `rangedAttack.js`,
/// `rangedMassAttack.js`, `heal.js`, `rangedHeal.js`, `_damage.js`). Ranges
/// are Chebyshev within one room and never across a border.
let private perform (a: Arena) (ledger: Ledger) (b: Body) (act: Act) =
    let byId = a.Bodies |> List.map (fun o -> o.Id, o) |> Map.ofList
    let power part each = activeCount b part * each
    let stopped = safeModeStops a b

    // `attack.js` and `rangedAttack.js` refuse the body itself as a target.
    let target id reach =
        Map.tryFind id byId
        |> Option.filter (fun t -> t.Id <> b.Id && within reach b.At t.At)

    match act with
    | Act.Attack id when not stopped ->
        match target id Engine.meleeRange with
        | Some t ->
            credit ledger.Damage t.Id (power Attack Engine.attackPower)
            // Strike-back: the target's own ATTACK lands on the attacker
            // (`_damage.js`, no rampart under the attacker here).
            let back = activeCount t Attack * Engine.attackPower

            if back > 0 then
                credit ledger.Damage b.Id back
        | None -> ()
    | Act.RangedAttack id when not stopped ->
        match target id Engine.rangedRange with
        | Some t -> credit ledger.Damage t.Id (power RangedAttack Engine.rangedAttackPower)
        | None -> ()
    | Act.RangedMassAttack when not stopped ->
        let each = power RangedAttack Engine.rangedAttackPower

        for t in a.Bodies do
            match range b.At t.At with
            | Some r when r <= Engine.rangedRange && t.Side <> b.Side ->
                let rate =
                    match r with
                    | 0
                    | 1 -> 1.0
                    | 2 -> 0.4
                    | _ -> 0.1

                credit ledger.Damage t.Id (int (System.Math.Round(float each * rate)))
            | _ -> ()
    | Act.Heal id when not stopped ->
        match
            Map.tryFind id byId
            |> Option.filter (fun t -> within Engine.meleeRange b.At t.At)
        with
        | Some t -> credit ledger.Healing t.Id (power Heal Engine.healPower)
        | None -> ()
    | Act.RangedHeal id when not stopped ->
        match
            Map.tryFind id byId
            |> Option.filter (fun t -> within Engine.rangedRange b.At t.At)
        with
        | Some t -> credit ledger.Healing t.Id (power Heal Engine.rangedHealPower)
        | None -> ()
    | _ -> ()

/// `attackController.js` on an owned controller: CONTROLLER_CLAIM_DOWNGRADE
/// (300) a CLAIM part off the downgrade timer and upgrades blocked for
/// CONTROLLER_ATTACK_BLOCKED_UPGRADE (1,000); refused while that block runs.
let private tapController (a: Arena) (b: Body) (roomName: string) : ArenaRoom option =
    match Map.tryFind roomName a.Rooms with
    | Some r ->
        match r.Controller with
        | Some c when
            b.At.Room = roomName
            && within 1 b.At (RoomPos.at roomName c.At)
            && c.Owner <> Ownership.Unowned
            && c.UpgradeBlockedUntil <= a.Time
            && not (safeModeStops a b)
            ->
            let claims = activeCount b BodyPart.Claim

            if claims = 0 then
                None
            else
                Some
                    { r with
                        Controller =
                            Some
                                { c with
                                    TicksToDowngrade = c.TicksToDowngrade - 300 * claims
                                    UpgradeBlockedUntil = a.Time + 1000
                                }
                    }
        | _ -> None
    | None -> None

/// `movement.js` `check`: every mover's target tile, the contested ones won
/// by the mover whose own tile is most wanted (a swap counting 100), then by
/// MOVE per weight; then every mover that cannot move or whose tile holds an
/// obstacle — a wall, a structure, a body not itself moving — is struck out,
/// and with it whoever wanted its tile. Returns each successful mover's tile.
let private resolveMoves (a: Arena) (moves: (Body * RoomPos) list) : Map<string, RoomPos> =
    let wanting = Dictionary<RoomPos, ResizeArray<Body * RoomPos>>()
    let order = ResizeArray<RoomPos>()

    for b, dest in moves do
        match wanting.TryGetValue dest with
        | true, list -> list.Add((b, dest))
        | _ ->
            wanting[dest] <- ResizeArray [ (b, dest) ]
            order.Add dest

    let affected (tile: RoomPos) =
        match wanting.TryGetValue tile with
        | true, list -> list.Count
        | _ -> 0

    let weightOf (b: Body) =
        b.Parts
        |> List.filter (fun p -> p <> Move && p <> Carry)
        |> List.length
        |> max 1

    // The winner of each wanted tile.
    let matrix = Dictionary<RoomPos, Body>()

    for tile in order do
        let candidates = wanting[tile] |> List.ofSeq

        let winner =
            match candidates with
            | [ (b, _) ] -> b
            | _ ->
                candidates
                |> List.mapi (fun i (b, _) ->
                    let swap =
                        match wanting.TryGetValue b.At with
                        | true, list -> list |> Seq.exists (fun (o, _) -> o.At = tile)
                        | _ -> false

                    let rate1 = if swap then 100 else affected b.At
                    let rate4 = float (activeCount b Move) / float (weightOf b)
                    b, rate1, rate4, i)
                |> List.sortBy (fun (_, r1, r4, i) -> -r1, -r4, i)
                |> List.head
                |> fun (b, _, _, _) -> b

        matrix[tile] <- winner

    let moving = HashSet<string>(matrix.Values |> Seq.map (fun b -> b.Id))

    let rec strike (tile: RoomPos) =
        match matrix.TryGetValue tile with
        | true, b ->
            matrix.Remove tile |> ignore
            moving.Remove b.Id |> ignore

            if matrix.ContainsKey b.At then
                strike b.At
        | _ -> ()

    let canMove (b: Body) = b.Fatigue = 0 && activeCount b Move > 0

    let obstacle (tile: RoomPos) =
        let r = Map.find tile.Room a.Rooms
        let p = RoomPos.pos tile

        terrainAt r p = Wall
        || Set.contains p (structureTiles r)
        || a.Bodies |> List.exists (fun o -> o.At = tile && not (moving.Contains o.Id))

    for tile in List.ofSeq order do
        match matrix.TryGetValue tile with
        | true, b when not (canMove b) || obstacle tile -> strike tile
        | _ -> ()

    matrix |> Seq.map (fun kv -> kv.Value.Id, kv.Key) |> Map.ofSeq

/// Fatigue a step onto this tile adds (`movement.js` `execute`): every part
/// but MOVE and CARRY, dead ones included, times 2 on plain and 10 on swamp;
/// none at all for a step onto an exit tile from inside the room.
let private stepFatigue (a: Arena) (b: Body) (dest: RoomPos) =
    if isEdge (RoomPos.pos dest) && not (isEdge (RoomPos.pos b.At)) then
        None
    else
        let rate =
            match terrainAt (Map.find dest.Room a.Rooms) (RoomPos.pos dest) with
            | Swamp -> Engine.swampWeight
            | _ -> 2

        let heavy = b.Parts |> List.filter (fun p -> p <> Move && p <> Carry) |> List.length
        let carried = (b.Energy + Engine.carryPartCapacity - 1) / Engine.carryPartCapacity
        Some((heavy + carried) * rate)

/// One tick of the arena: our pipeline and the scripts decide off the
/// start-of-tick state, the engine performs both, and the tick's trace.
let step (a: Arena) : Arena * TickTrace =
    let ours, carried =
        if List.isEmpty a.Colonies then
            [], a.Carried
        else
            decideOurs a

    let theirs = theirActs a
    let events = ResizeArray<ArenaEvent>()
    let byId = a.Bodies |> List.map (fun b -> b.Id, b) |> Map.ofList

    // Our Intents as engine acts, by the body they name; the rest are
    // structure verbs or economy the arena does not perform.
    let ourActs =
        ours
        |> List.choose (function
            | AttackCreep(n, h) -> Some(n, Act.Attack h)
            | RangedAttackCreep(n, h) -> Some(n, Act.RangedAttack h)
            | HealCreep(n, t) -> Some(n, Act.Heal t)
            | RangedHealCreep(n, t) -> Some(n, Act.RangedHeal t)
            | MoveCreep(n, d) -> Some(n, Act.Move d)
            | _ -> None)

    let actsOf =
        ourActs @ theirs
        |> List.filter (fun (id, _) -> Map.containsKey id byId)
        |> List.groupBy fst
        |> List.map (fun (id, acts) -> id, admitted (List.map snd acts))
        |> Map.ofList

    let ledger =
        {
            Damage = Dictionary<string, int>()
            Healing = Dictionary<string, int>()
        }

    let mutable rooms = a.Rooms

    // Every body's acts in id order, on start-of-tick positions and parts.
    for b in a.Bodies |> List.sortBy (fun b -> b.Id) do
        for act in Map.tryFind b.Id actsOf |> Option.defaultValue [] do
            match act with
            | Act.AttackController roomName ->
                match tapController { a with Rooms = rooms } b roomName with
                | Some r ->
                    rooms <- Map.add roomName r rooms
                    events.Add(ControllerAttacked(roomName, b.Id))
                | None -> ()
            | Act.Move _ -> ()
            | other -> perform a ledger b other

    // The towers (`towers/attack.js`, `towers/heal.js`): heal before attack,
    // either at the falloff curve `Engine.towerAttackAt` spells.
    for intent in ours do
        match intent with
        | FireTower(towerId, hostileId)
        | HealWithTower(towerId, hostileId) ->
            let heal =
                match intent with
                | HealWithTower _ -> true
                | _ -> false

            let placed =
                rooms
                |> Map.toList
                |> List.tryPick (fun (name, r) ->
                    r.Towers
                    |> List.tryFind (fun t -> t.Id = towerId)
                    |> Option.map (fun t -> name, r, t))

            match placed, Map.tryFind hostileId byId with
            | Some(name, r, t), Some target when
                t.Energy >= Engine.towerEnergyCost && target.At.Room = name
                ->
                let reach = RoomPos.range (RoomPos.at name t.At) target.At |> Option.defaultValue 0

                if heal then
                    credit ledger.Healing target.Id (Engine.towerHealAt reach)
                else
                    credit ledger.Damage target.Id (Engine.towerAttackAt reach)

                rooms <-
                    Map.add
                        name
                        { r with
                            Towers =
                                r.Towers
                                |> List.map (fun x ->
                                    if x.Id = t.Id then
                                        { x with
                                            Energy = x.Energy - Engine.towerEnergyCost
                                        }
                                    else
                                        x)
                        }
                        rooms
            | _ -> ()
        | ActivateSafeMode controllerId ->
            match
                rooms
                |> Map.tryFindKey (fun _ r ->
                    r.Controller |> Option.exists (fun c -> c.Id = controllerId))
            with
            | Some name ->
                let r = Map.find name rooms

                match r.Controller with
                | Some c when c.SafeModeAvailable > 0 && c.SafeModeUntil <= a.Time ->
                    rooms <-
                        Map.add
                            name
                            { r with
                                Controller =
                                    Some
                                        { c with
                                            SafeModeAvailable = c.SafeModeAvailable - 1
                                            SafeModeUntil = a.Time + 20_000
                                        }
                            }
                            rooms

                    events.Add(SafeModeActivated name)
                | _ -> ()
            | None -> ()
        | _ -> ()

    // Movement, resolved together.
    let moves =
        a.Bodies
        |> List.sortBy (fun b -> b.Id)
        |> List.choose (fun b ->
            Map.tryFind b.Id actsOf
            |> Option.defaultValue []
            |> List.tryPick (function
                | Act.Move d -> Some(b, stepTo b.At d)
                | _ -> None))

    let moved = resolveMoves a moves

    // `creeps/tick.js`, per body: the step and its fatigue, the exit
    // transfer, MOVE paying fatigue off, then damage and heal applied
    // together before the death check.
    let settled =
        a.Bodies
        |> List.choose (fun b ->
            let at, fatigue =
                match Map.tryFind b.Id moved with
                | Some dest ->
                    match stepFatigue a b dest with
                    | None -> dest, 0
                    | Some added -> dest, b.Fatigue + added
                | None -> b.At, b.Fatigue

            let fatigue = max 0 (fatigue - 2 * activeCount b Move)

            let damage =
                match ledger.Damage.TryGetValue b.Id with
                | true, d -> d
                | _ -> 0

            let healing =
                match ledger.Healing.TryGetValue b.Id with
                | true, h -> h
                | _ -> 0

            let hits = min (hitsMax b) (b.Hits - damage + healing)

            if hits <= 0 || b.TicksToLive <= 1 then
                events.Add(Died b.Id)
                None
            else
                let npc =
                    match b.Side with
                    | Side.Npc _ -> true
                    | _ -> false

                let landed =
                    if npc || not (Seam.isExit (RoomPos.pos at)) then
                        Some at
                    else
                        match landingOf at with
                        | Some(name, landing) when Map.containsKey name rooms ->
                            let there = RoomPos.at name landing
                            events.Add(Crossed(b.Id, at, there))
                            Some there
                        | _ ->
                            events.Add(Exited(b.Id, at))
                            None

                landed
                |> Option.map (fun at ->
                    let script = b.Script |> Option.defaultValue Hold

                    { b with
                        At = at
                        Fatigue = fatigue
                        Hits = hits
                        TicksToLive = b.TicksToLive - 1
                        Retreating = retreatLatch a.Tick { b with Hits = hits } script
                    }))

    let next =
        { a with
            Time = a.Time + 1
            Tick = a.Tick + 1
            Rooms = rooms
            Bodies = settled
            Carried =
                { carried with
                    LastPositions = a.Bodies |> List.map (fun b -> b.Id, b.At) |> Map.ofList
                }
        }

    next,
    {
        Tick = a.Tick
        Time = a.Time
        Ours = ours
        Theirs = theirs
        Events = List.ofSeq events
        Bodies =
            settled
            |> List.map (fun b ->
                {
                    Id = b.Id
                    Side = b.Side
                    At = b.At
                    Hits = b.Hits
                    Fatigue = b.Fatigue
                })
    }

/// Run `ticks` steps, or until `stop` holds of the arena, returning the final
/// arena and the trace in tick order.
let runUntil (stop: Arena -> bool) (ticks: int) (a: Arena) : Arena * TickTrace list =
    let rec go n (a: Arena) acc =
        if n = 0 || stop a then
            a, List.rev acc
        else
            let next, trace = step a
            go (n - 1) next (trace :: acc)

    go ticks a []

/// Run exactly `ticks` steps.
let run (ticks: int) (a: Arena) : Arena * TickTrace list = runUntil (fun _ -> false) ticks a

// ---------------------------------------------------------------------------
// Reading a trace
// ---------------------------------------------------------------------------

/// The tick a body died on, if it did.
let diedOn (id: string) (trace: TickTrace list) : int option =
    trace
    |> List.tryFind (fun t -> t.Events |> List.contains (Died id))
    |> Option.map (fun t -> t.Tick)

/// One body's snapshot at the end of every tick it lived through.
let pathOf (id: string) (trace: TickTrace list) : (int * Snapshot) list =
    trace
    |> List.choose (fun t ->
        t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map (fun b -> t.Tick, b))

/// The bodies standing at the end of a tick.
let standingAt (tick: int) (trace: TickTrace list) : Snapshot list =
    trace
    |> List.tryFind (fun t -> t.Tick = tick)
    |> Option.map (fun t -> t.Bodies)
    |> Option.defaultValue []

/// A short line per tick for a failing assertion's message.
let describe (trace: TickTrace list) : string =
    trace
    |> List.map (fun t ->
        let bodies =
            t.Bodies
            |> List.map (fun b -> $"{b.Id}@{b.At.Room}:{b.At.X},{b.At.Y}/{b.Hits}")
            |> String.concat " "

        let events = t.Events |> List.map string |> String.concat "; "
        $"t{t.Tick} [{bodies}] {events}")
    |> String.concat "\n"
