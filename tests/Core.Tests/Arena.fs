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
/// (`resolveRooms`) — `Main.fullTick` less Memory. Each colony's Raid log is
/// carried in heap and not Memory: folded after the tick
/// (`Observe.foldRaids`) and read into the next tick's [[stand-down]] gate
/// (`Observe.standDown`), as `Main` does (#469). What the arena leaves out,
/// and says so: harassment casting (`HarassCasting.none`), the round-robin
/// replan (every colony is `ReplanTurn.Now`), and light ticks (`LightTick`):
/// every tick is a full one.
///
/// **The physics is the engine's** (`@screeps/engine/src/processor`), cited
/// per rule below, with `Engine`'s constants and `Engine.liveParts` shared
/// with `Facts.squadFight` rather than re-spelt. Structures of either side
/// stand in a room (#465): ramparts, walls, towers, spawns and the rest,
/// placed or loaded off a capture's `[structures]`. A body spends the
/// energy it carries on a repair or a transfer, and nothing refills it. The
/// season's sector Reactor stands in a room (#469), with `screeps/mod-season5`
/// `da59118`'s rules: `claimReactor`, a Thorium transfer in, and the burn
/// that scores. A spawn of ours casts the rows an arena names (`withCasts`),
/// and only those: the live colony's economy is staffed, and the arena holds
/// none to cast for; the bank is never debited, as the economy the arena
/// leaves out refills it. A controller is claimed, upgraded to its next
/// level and tapped (#470), and safe mode is refused as the engine refuses
/// it: on cooldown, under an upgrade block, or past the downgrade line.
/// Construction sites stand, are built with carried energy, and are removed
/// by a body of another side stepping on them; placing one is a test's, not
/// the pipeline's. A body withdraws energy from a store. Not modelled: the
/// rest of the economy (harvest, pickup, a store's refill), a downgrade,
/// roads, boosts, pulling, portals, power creeps, nukes, and safe mode for
/// any side but ours.
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
    | Dismantle of target: string
    | Repair of target: string
    /// Energy into a structure's store, all it holds or all there is room for.
    | Transfer of target: string
    /// Thorium into the Reactor, all the body holds or all there is room for.
    | TransferThorium of reactor: string
    /// `claimReactor` (`mod-season5/src/creep.claimReactor.js`).
    | ClaimReactor of reactor: string
    /// `claimController` on the room's controller (`creeps/claimController.js`).
    | ClaimController of controllerRoom: string
    /// `upgradeController` on the room's controller (`creeps/upgradeController.js`).
    | UpgradeController of controllerRoom: string
    /// `build` on a construction site (`creeps/build.js`).
    | Build of site: string
    /// Energy out of a structure's store, as much as the body has room for
    /// or `amount` (`creeps/withdraw.js`).
    | Withdraw of store: string * amount: int option

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
    /// Walk to a ground tile, across rooms, breaking through what bars the
    /// way (`firstBarrier`): the first barrier on the cheapest route is struck
    /// with every weapon the body carries — dismantle, else attack, and a
    /// ranged shot beside either — until it falls (#465).
    | Breach of goal: RoomPos
    /// SlothBot's `reactorClaimer`
    /// (`docs/research/shibdib-reactor-steal.md` §3): walk to the Reactor on
    /// this tile, `claimReactor` it every tick it is not its side's, and park
    /// two off it once it is.
    | TakeReactor of reactor: RoomPos
    /// A squad body on its way to a tile: in a room where an enemy stands it
    /// kites (`Kite`), and walks on (`GoTo`) where none does.
    | Sweep of goal: RoomPos * Focus * keep: int

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
        Thorium: int
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
        /// Points toward the next level (`progress`).
        Progress: int
        /// The engine's `safeModeCooldown`: no activation while the game
        /// tick is at or below it. An unclaim or a downgrade to 0 sets it
        /// SAFE_MODE_COOLDOWN on, and a claim leaves it standing
        /// (`controllers/unclaim.js`, `creeps/claimController.js`).
        SafeModeCooldown: int
    }

/// A construction site in an arena room, of any owner.
type ArenaSite =
    {
        Id: string
        /// The engine's `structureType` it becomes.
        Kind: string
        At: Pos
        Owner: Side
        Progress: int
        /// `progressTotal`: CONSTRUCTION_COST of the kind.
        Total: int
        /// A spawn site's name, which the spawn takes.
        Name: string option
    }

/// One structure in an arena room, of any owner.
type ArenaStructure =
    {
        Id: string
        /// The engine's `structureType`: "rampart", "constructedWall",
        /// "spawn", "tower", "extension", ...
        Kind: string
        At: Pos
        /// None for a wall nobody owns.
        Owner: Side option
        Hits: int
        HitsMax: int
        /// The energy it holds: a tower's shots, a store's stock.
        Energy: int
        /// The Thorium it holds: a Storage's banked ore.
        Thorium: int
        /// A rampart any body may step onto (`ramparts/set-public.js`).
        IsPublic: bool
        /// A spawn's name, which names the colony its creeps are
        /// (`Colony.castBy`); None for every other kind.
        Name: string option
        /// A rampart's `nextDecayTime`. One placed with none is taken to
        /// have just decayed: `arena` sets it a RAMPART_DECAY_TIME off.
        NextDecay: int
    }

/// What a tower not ours does, in the order it asks: the first duty with a
/// target is the tick's act.
type TowerDuty =
    /// Heal the most hurt body of the tower's side in its room.
    | HealHurt
    /// Shoot the focused enemy body in its room.
    | Shoot of Focus
    /// Repair the weakest rampart or wall of its side below these hits.
    | Mend of below: int

/// A sector centre's Reactor (`mod-season5/src/reactor.roomObject.js`): no
/// hits, walkable, a T store of `Engine.reactorCapacity`.
type ArenaReactor =
    {
        Id: string
        At: Pos
        /// Its `user`; None for one nobody has claimed.
        Owner: Side option
        Thorium: int
        /// The tick its streak began; `continuousWork` is the time since.
        LaunchTime: int option
    }

/// One captured room in the arena, with what stands on it beyond terrain.
type ArenaRoom =
    {
        Capture: RoomCapture
        Controller: ArenaController option
        /// Every structure standing here, ours and everybody else's.
        Structures: ArenaStructure list
        /// Every construction site here, ours and everybody else's.
        Sites: ArenaSite list
        /// The script the towers not ours run; ours are our pipeline's.
        Duties: TowerDuty list
        /// The home's bank, for a room with a spawn.
        Bank: int
        Reactor: ArenaReactor option
    }

/// A body in a spawn of ours (`spawns/create-creep.js`): born at `Done`.
type Oven =
    {
        Spawn: string
        Room: string
        Name: string
        Parts: BodyPart list
        Done: int
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
        /// Each colony's Raid log (`Observe.foldRaids`), by home.
        Raids: Map<string, Observe.RaidState>
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
        /// The season score each player's Reactors have earned, by username.
        Scores: Map<string, int>
        /// The bodies our spawns are casting.
        Ovens: Oven list
        /// The rows whose casts the arena performs, by pattern name.
        Casts: Set<string>
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
    /// A structure's hits ran out (`structures/_destroy.js`), or a
    /// rampart's decay did (`ramparts/tick.js`).
    | Destroyed of id: string
    /// A `claimReactor` landed: the Reactor's flag is the body's side's.
    | ReactorClaimed of room: string * by: string
    /// The Reactor burnt one T for its owner, who scored this much.
    | Burned of room: string * owner: string * score: int
    /// A spawn of ours finished a body, standing beside it.
    | Born of id: string
    /// A `claimController` landed: the room is the body's side's at level 1.
    | ControllerClaimed of room: string * by: string
    /// An upgrade took the room's controller to this level, banking a safe
    /// mode.
    | LeveledUp of room: string * level: int
    /// A body of another side stepped onto a site and removed it
    /// (`movement.js` `execute`).
    | SiteStomped of site: string * by: string
    /// A build finished a site: the structure stands under the site's id's
    /// kind and tile.
    | Built of site: string

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
        /// Every standing structure's hits at the end of the tick, by id.
        Structures: Map<string, int>
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
        Thorium = 0
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
        Structures = []
        Sites = []
        Duties = []
        Bank = 0
        Reactor = None
    }

/// The sector Reactor standing on a tile of the room, holding `thorium`,
/// flagged `owner`'s, its streak not yet begun.
let withReactor (id: string) (at: Pos) (owner: Side option) (thorium: int) (r: ArenaRoom) =
    { r with
        Reactor =
            Some
                {
                    Id = id
                    At = at
                    Owner = owner
                    Thorium = thorium
                    LaunchTime = None
                }
    }

/// RAMPART_DECAY_AMOUNT and RAMPART_DECAY_TIME (`ramparts/tick.js`).
let private rampartDecayAmount = 300
let private rampartDecayTime = 100

/// DISMANTLE_POWER (`creeps/dismantle.js`), and REPAIR_POWER at REPAIR_COST
/// 0.01 an energy a hit (`creeps/repair.js`): 100 hits an energy.
let private dismantlePower = 50
let private repairPower = 100
let private hitsPerEnergy = 100

/// RAMPART_HITS_MAX by controller level; a room with no level of two or more
/// takes RCL2's.
let private rampartHitsMax (level: int) =
    match level with
    | 3 -> 1_000_000
    | 4 -> 3_000_000
    | 5 -> 10_000_000
    | 6 -> 30_000_000
    | 7 -> 100_000_000
    | 8 -> 300_000_000
    | _ -> 300_000

/// CONTROLLER_DOWNGRADE (`constants.js`): the full downgrade timer a level.
let private fullDowngrade (level: int) =
    match level with
    | 1 -> 20_000
    | 2 -> 10_000
    | 3 -> 20_000
    | 4 -> 40_000
    | 5 -> 80_000
    | 6 -> 120_000
    | 7 -> 150_000
    | _ -> 200_000

/// CONTROLLER_LEVELS (`constants.js`): the points from a level to the next.
let private levelPoints (level: int) =
    match level with
    | 1 -> 200
    | 2 -> 45_000
    | 3 -> 135_000
    | 4 -> 405_000
    | 5 -> 1_215_000
    | 6 -> 3_645_000
    | _ -> 10_935_000

/// A structure of the engine's `kind`, under the id `kind-x-y`.
let structureOf
    (kind: string)
    (owner: Side option)
    (hits: int)
    (hitsMax: int)
    (at: Pos)
    : ArenaStructure =
    {
        Id = $"{kind}-{at.X}-{at.Y}"
        Kind = kind
        At = at
        Owner = owner
        Hits = hits
        HitsMax = hitsMax
        Energy = 0
        Thorium = 0
        IsPublic = false
        Name = None
        NextDecay = 0
    }

/// A rampart of `owner`'s, not public, at RCL2's RAMPART_HITS_MAX.
let rampart (owner: Side) (hits: int) (at: Pos) : ArenaStructure =
    structureOf "rampart" (Some owner) hits (rampartHitsMax 2) at

/// A tower of `owner`'s at TOWER_HITS, holding `energy`.
let towerOf (owner: Side) (at: Pos) (energy: int) : ArenaStructure =
    { structureOf "tower" (Some owner) 3000 3000 at with
        Energy = energy
    }

/// These structures standing in the room as well.
let withStructures (structures: ArenaStructure list) (r: ArenaRoom) : ArenaRoom =
    { r with
        Structures = r.Structures @ structures
    }

/// Ramparts of the controller's holder on these tiles at these hits, at its
/// level's RAMPART_HITS_MAX (`ramparts/tick.js`): placed after
/// `withController`.
let withRamparts (owner: Side) (hits: int) (tiles: Pos list) (r: ArenaRoom) : ArenaRoom =
    let level = r.Controller |> Option.map (fun c -> c.Level) |> Option.defaultValue 0

    r
    |> withStructures (
        tiles
        |> List.map (fun at ->
            { rampart owner hits at with
                HitsMax = rampartHitsMax level
            })
    )

/// The towers not ours run these duties.
let withTowerDuties (duties: TowerDuty list) (r: ArenaRoom) : ArenaRoom = { r with Duties = duties }

/// The room's captured controller, owned as given, its downgrade timer full.
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
                        TicksToDowngrade = fullDowngrade level
                        SafeModeAvailable = safeModes
                        SafeModeUntil = 0
                        UpgradeBlockedUntil = 0
                        Progress = 0
                        SafeModeCooldown = 0
                    }
        }
    | None -> failwithf "%s has no controller" r.Capture.RoomName

/// The room's controller with its `safeModeCooldown` standing to this tick:
/// placed after `withController`.
let withSafeModeCooldown (until: int) (r: ArenaRoom) : ArenaRoom =
    { r with
        Controller = r.Controller |> Option.map (fun c -> { c with SafeModeCooldown = until })
    }

/// CONSTRUCTION_COST (`constants.js`) of the kinds a site is placed for here.
let private constructionCost (kind: string) =
    match kind with
    | "spawn" -> 15_000
    | "tower" -> 5_000
    | "extension" -> 3_000
    | "rampart" -> 1
    | "road" -> 300
    | "container" -> 5_000
    | "storage" -> 30_000
    | other -> failwithf "no CONSTRUCTION_COST for %s here" other

/// A site of `owner`'s on a tile of the room, `progress` already built into
/// it, under the id `site-kind-x-y`; a spawn's carries its name.
let withSite
    (owner: Side)
    (kind: string)
    (name: string option)
    (progress: int)
    (at: Pos)
    (r: ArenaRoom)
    : ArenaRoom =
    { r with
        Sites =
            r.Sites
            @ [
                {
                    Id = $"site-{kind}-{at.X}-{at.Y}"
                    Kind = kind
                    At = at
                    Owner = owner
                    Progress = progress
                    Total = constructionCost kind
                    Name = name
                }
            ]
    }

/// A spawn of ours standing on a tile of the room, which must be ground, at
/// SPAWN_HITS and full.
let withSpawn (name: string) (at: Pos) (bank: int) (r: ArenaRoom) : ArenaRoom =
    match TerrainGrid.tryFind at r.Capture.Terrain with
    | Some Wall
    | None -> failwithf "%s: the spawn tile %d,%d is not ground" r.Capture.RoomName at.X at.Y
    | Some _ ->
        let spawn =
            { structureOf "spawn" (Some Side.Ours) 5000 5000 at with
                Id = $"spawn-{name}"
                Energy = 300
                Name = Some name
            }

        { r with
            Structures = r.Structures @ [ spawn ]
            Bank = bank
        }

/// The base the capture was taken with (`--structures`), its controller held
/// as it was at the level and the safe modes it banked, its towers running
/// `duties`. A structure's owner is a player of that username.
let withBase (duties: TowerDuty list) (r: ArenaRoom) : ArenaRoom =
    let held =
        match r.Capture.Holder with
        | Some holder ->
            withController Ownership.Rival (Some holder.Username) holder.Level holder.SafeModes r
        | None -> failwithf "%s was captured without its structures" r.Capture.RoomName

    let structures =
        r.Capture.Structures
        |> List.map (fun s ->
            {
                Id = s.Id
                Kind = s.Type
                At = s.At
                Owner = s.Owner |> Option.map Side.Player
                Hits = s.Hits
                HitsMax = s.HitsMax
                Energy = s.Energy
                Thorium = 0
                IsPublic = s.IsPublic
                Name = None
                NextDecay = s.NextDecay
            })

    { held with
        Structures = held.Structures @ structures
        Duties = duties
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
    let decaying (r: ArenaRoom) =
        { r with
            Structures =
                r.Structures
                |> List.map (fun s ->
                    if s.Kind = "rampart" && s.NextDecay = 0 then
                        { s with
                            NextDecay = time + rampartDecayTime
                        }
                    else
                        s)
        }

    {
        Time = time
        Tick = 0
        Rooms = rooms |> List.map (fun r -> r.Capture.RoomName, decaying r) |> Map.ofList
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
                Raids = Map.empty
            }
        Scores = Map.empty
        Ovens = []
        Casts = Set.empty
    }

/// Our spawns cast these rows, by pattern name, and only these.
let withCasts (rows: string list) (a: Arena) : Arena = { a with Casts = Set.ofList rows }

// ---------------------------------------------------------------------------
// Geometry and the engine's tile rules
// ---------------------------------------------------------------------------

/// Terrain at any tile of a room, the border ring included.
let terrainAt (r: ArenaRoom) (tile: Pos) : Terrain =
    if Seam.onRing tile then
        Map.tryFind tile r.Capture.Border |> Option.defaultValue Wall
    else
        TerrainGrid.tryFind tile r.Capture.Terrain |> Option.defaultValue Wall

/// OBSTACLE_OBJECT_TYPES' structures (`constants.js`).
let private obstacleKinds =
    set
        [
            "spawn"
            "constructedWall"
            "extension"
            "link"
            "storage"
            "tower"
            "observer"
            "powerSpawn"
            "lab"
            "terminal"
            "nuker"
            "factory"
            "invaderCore"
        ]

/// Whether a structure bars a body of this side from its tile (`movement.js`
/// `checkObstacleAtXY`): an obstacle kind, or a rampart neither public nor
/// that side's own.
let private bars (side: Side) (s: ArenaStructure) =
    Set.contains s.Kind obstacleKinds
    || s.Kind = "rampart" && not s.IsPublic && s.Owner <> Some side

/// The tiles barred to a body of this side in a room: every rock, the
/// controller, the structures that bar it, and its own side's sites of an
/// obstacle kind (`checkObstacleAtXY`).
let private structureTiles (side: Side) (r: ArenaRoom) : Set<Pos> =
    Set.ofList
        [
            for _, pos in r.Capture.Rocks -> pos
            for _, pos in Option.toList r.Capture.RealController -> pos
            for s in r.Structures do
                if bars side s then
                    yield s.At
            for site in r.Sites do
                if site.Owner = side && Set.contains site.Kind obstacleKinds then
                    yield site.At
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

/// Whether two sides fight each other: different, and not us beside an ally
/// of ours (`Colony.isAlly`).
let private sidesHostile (x: Side) (y: Side) : bool =
    let allied (x: Side) (y: Side) =
        match x, y with
        | Side.Ours, Side.Player name -> Colony.isAlly name
        | _ -> false

    x <> y && not (allied x y) && not (allied y x)

/// Whether two bodies fight each other (`sidesHostile`).
let hostileTo (a: Body) (b: Body) : bool = sidesHostile a.Side b.Side

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
    || r.Structures |> List.exists (fun s -> s.Owner = Some Side.Ours)
    || r.Controller |> Option.exists (fun c -> c.Owner = Ownership.Ours)

/// A structure's kind as the shell classifies its `structureType`
/// (`World.builtKindOf`): one the table lacks is Other.
let private builtKindOf =
    reverseOf builtKindName allBuiltKinds >> Option.defaultValue BuiltKind.Other

/// SPAWN_ENERGY_CAPACITY and EXTENSION_ENERGY_CAPACITY below RCL7, and the
/// tower's: the free room a Refillable reports.
let private capacityOf (kind: BuiltKind) =
    match kind with
    | BuiltKind.Spawn -> 300
    | BuiltKind.Tower -> Engine.towerCapacity
    | _ -> 50

let private creepInfo (a: Arena) (b: Body) : CreepInfo =
    let active = live b
    let carry = partCountIn active Carry * Engine.carryPartCapacity

    {
        Name = b.Id
        TicksToLive = b.TicksToLive
        Fatigue = b.Fatigue
        Hits = { Hits = b.Hits; HitsMax = hitsMax b }
        Energy = b.Energy
        Thorium = b.Thorium
        FreeCapacity = max 0 (carry - b.Energy - b.Thorium)
        Body = active |> List.countBy id |> Map.ofList
        Moved =
            match Map.tryFind b.Id a.Carried.LastPositions with
            | Some last -> last <> b.At
            | None -> false
    }

/// One room's facts, seen or blind, as `World.factsOf` builds them.
let private factsOf (a: Arena) (name: string) (r: ArenaRoom) : RoomFacts =
    let structures = r.Structures |> List.map (fun s -> s, builtKindOf s.Kind)

    let ours = structures |> List.filter (fun (s, _) -> s.Owner = Some Side.Ours)

    let spawns =
        ours
        |> List.choose (fun (s, _) ->
            s.Name
            |> Option.map (fun spawn ->
                {
                    Name = spawn
                    Id = s.Id
                    RoomName = name
                    IsSpawning = a.Ovens |> List.exists (fun oven -> oven.Spawn = s.Id)
                }))

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

        // `FIND_MY_CONSTRUCTION_SITES`: every reader presumes a site is ours,
        // and the others are tiles alone (`World.seenFacts`).
        let sites, rivalSites =
            r.Sites |> List.partition (fun site -> site.Owner = Side.Ours)

        let targets =
            [
                for id, pos in r.Capture.RealSources -> id, pos, Source
                for s, kind in structures -> s.Id, s.At, Structure kind
                for site in sites -> site.Id, site.At, Site(builtKindOf site.Kind)
                for id, pos in Option.toList controller -> id, pos, Controller
                for id, pos in r.Capture.RealMinerals -> id, pos, Mineral
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
                    // The shell's obstacle census: every owner's structures
                    // a creep cannot stand on, the controller and the
                    // minerals (`World.seenFacts`). A rampart is walkable
                    // there whoever owns it.
                    Obstacles =
                        [
                            for _, pos in Option.toList controller -> pos
                            for _, pos in r.Capture.RealMinerals -> pos
                            for s, kind in structures do
                                if not (isWalkable kind) then
                                    yield s.At
                            for site in sites do
                                if not (isWalkable (builtKindOf site.Kind)) then
                                    yield site.At
                        ]
                        |> Set.ofList
                    Roads = Set.empty
                    RivalSites = rivalSites |> List.map (fun site -> site.At) |> Set.ofList
                }
            Border = r.Capture.Border
            TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
            // Hits on the repairable kinds, an ownable one only when ours
            // (`World.seenFacts`).
            Hits =
                structures
                |> List.filter (fun (s, kind) ->
                    (wholeLine kind).IsSome && (not (needsOwner kind) || s.Owner = Some Side.Ours))
                |> List.map (fun (s, _) -> s.Id, { Hits = s.Hits; HitsMax = s.HitsMax })
                |> Map.ofList
            Stores =
                structures
                |> List.filter (fun (_, kind) -> isStored kind)
                |> List.map (fun (s, _) -> s.Id, s.Energy)
                |> Map.ofList
            Thorium =
                (r.Capture.RealMinerals |> List.map (fun (id, _) -> id, 20_000))
                @ (structures
                   |> List.filter (fun (s, _) -> s.Thorium > 0)
                   |> List.map (fun (s, _) -> s.Id, s.Thorium))
                |> Map.ofList
            // The Reactor's two rows, as `World.seenFacts` files them: its
            // tile and kind are the declaration's, not vision's.
            Owners =
                r.Reactor
                |> Option.map (fun reactor ->
                    reactor.Id,
                    match reactor.Owner with
                    | Some Side.Ours -> Ownership.Ours
                    | Some _ -> Ownership.Rival
                    | None -> Ownership.Unowned)
                |> Option.toList
                |> Map.ofList
            Reactors =
                r.Reactor
                |> Option.map (fun reactor ->
                    {
                        Id = reactor.Id
                        Owner =
                            match reactor.Owner with
                            | Some Side.Ours -> ReactorOwner.Ours
                            | Some side -> ReactorOwner.Rival(username side)
                            | None -> ReactorOwner.Unowned
                        Thorium = reactor.Thorium
                        // `continuousWork`: `time - launchTime`, 0 when idle.
                        ContinuousWork =
                            reactor.LaunchTime
                            |> Option.map (fun t -> a.Time - t)
                            |> Option.defaultValue 0
                    })
                |> Option.toList
            Casting =
                a.Ovens
                |> List.filter (fun oven -> oven.Room = name)
                |> List.map (fun oven -> { Name = oven.Name; Body = oven.Parts })
            Control =
                Some
                    {
                        Owner =
                            r.Controller
                            |> Option.map (fun c -> c.Owner)
                            |> Option.defaultValue Ownership.Unowned
                        Reservation = None
                        SafeMode = r.Controller |> Option.exists (fun c -> c.SafeModeUntil > a.Time)
                        SafeModeCooldownUntil =
                            r.Controller
                            |> Option.map (fun c -> c.SafeModeCooldown)
                            |> Option.defaultValue 0
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
                        SafeModeCooldownUntil = c.SafeModeCooldown
                    })
            Energy =
                if List.isEmpty spawns then
                    { Available = 0; Capacity = 0 }
                else
                    {
                        Available = r.Bank
                        Capacity = r.Bank
                    }
            Spawns = spawns
            Refillables =
                ours
                |> List.filter (fun (_, kind) -> isRefillable kind)
                |> List.map (fun (s, kind) ->
                    {
                        Id = s.Id
                        FreeCapacity = max 0 (capacityOf kind - s.Energy)
                        Kind = kind
                    })
            Sources =
                r.Capture.RealSources
                |> List.map (fun (id, _) -> { Id = id; TicksToRestock = 0 })
            ConstructionSites =
                sites
                |> List.map (fun site ->
                    {
                        Id = site.Id
                        Left = site.Total - site.Progress
                        Begun = site.Progress > 0
                    })
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

/// One colony's Raid log as the arena carries it.
let private raidsOf (a: Arena) (home: string) =
    Map.tryFind home a.Carried.Raids |> Option.defaultValue Observe.RaidState.empty

/// Every living colony's view cut from this tick's world, as
/// `Main.fullTick` cuts them, beside the world.
let private cut (a: Arena) : World * (Colony * ColonyView) list =
    let world = worldOf a
    let tuning = Tuning.defaults
    let living = World.living a.Colonies world
    let joins = JoinTable()
    let casting = HarassCasting.none

    let holders =
        World.creepColoniesRecalling joins tuning a.Colonies casting living Map.empty world

    world,
    living
    |> List.map (fun c ->
        let gate = Observe.standDown tuning a.Time (raidsOf a c.Home)

        c, ColonyView.ofWorldRecalling joins tuning a.Colonies casting gate holders world c)

/// The view one living colony decides this tick off.
let viewOf (a: Arena) (home: string) : ColonyView =
    cut a
    |> snd
    |> List.tryPick (fun (c, view) -> if c.Home = home then Some view else None)
    |> Option.defaultWith (fun () -> failwithf "%s is no living colony" home)

/// Our side's tick: every living colony's view cut from the world and decided,
/// every room's moves arbitrated once. The Intents, the next assignments and
/// memos, and the world's carried memory.
let private decideOurs (a: Arena) : Intent list * Carried =
    let world, views = cut a

    let alive =
        a.Bodies
        |> List.filter (fun b -> b.Side = Side.Ours)
        |> List.map (fun b -> b.Id)
        |> Set.ofList

    let decided =
        views
        |> List.map (fun (c, view) ->
            let decision =
                decideUnarbitrated
                    view
                    a.Carried.Assignments
                    Set.empty
                    (Map.tryFind c.Home a.Carried.Memos)
                    ReplanTurn.Now

            (c.Home,
             Observe.foldRaids
                 Observe.capEpisodes
                 alive
                 view
                 decision.OutpostRooms
                 (raidsOf a c.Home)),
            (c.Home, decision))

    let decisions = decided |> List.map snd

    let moves, _ = resolveRooms (decisions |> List.map (fun (_, d) -> d.Movement))

    // One activation a shard, as `Main` sends them.
    let intents =
        (decisions |> List.collect (fun (_, d) -> d.Intents)) @ moves
        |> Layout.firstActivationOnly

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
        Raids = decided |> List.map fst |> Map.ofList
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

        let blocked =
            names |> Array.map (fun n -> structureTiles mover.Side (Map.find n a.Rooms))

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

/// What a body takes off a structure a tick with everything it carries:
/// dismantle, else attack — the engine admits one (`creeps/intents.js`) —
/// beside a ranged shot.
let private structureStrike (b: Body) =
    let work = activeCount b Work * dismantlePower

    let melee =
        if work > 0 then
            work
        else
            activeCount b Attack * Engine.attackPower

    melee + activeCount b RangedAttack * Engine.rangedAttackPower

/// The structures that bar a body of this side and are not its own, by tile:
/// what it has to break to pass. A rampart over a structure stands first.
let private barriersFor (side: Side) (r: ArenaRoom) : Map<Pos, ArenaStructure list> =
    r.Structures
    |> List.filter (fun s -> bars side s && s.Owner <> Some side)
    |> List.groupBy (fun s -> s.At)
    |> List.map (fun (at, stack) ->
        at, stack |> List.sortBy (fun s -> (if s.Kind = "rampart" then 0 else 1), s.Id))
    |> Map.ofList

/// Each captured room's terrain walls, row by column, read once: a capture
/// never changes, and the breach search asks every tile of every room.
let private wallsOf =
    let held = System.Collections.Concurrent.ConcurrentDictionary<string, bool[]>()

    fun (r: ArenaRoom) ->
        held.GetOrAdd(
            r.Capture.RoomName,
            fun _ ->
                Array.init (Engine.roomSide * Engine.roomSide) (fun n ->
                    terrainAt
                        r
                        {
                            X = n / Engine.roomSide
                            Y = n % Engine.roomSide
                        } = Wall)
        )

/// The first barrier (`barriersFor`) on the cheapest walk for a body to a
/// ground tile across the arena's rooms, beside the tile it stands on; None
/// when that walk meets none, or there is no walk. A tile costs one, a
/// barrier the ticks the body takes to break it. Its own side's structures,
/// every rock, the controller and the terrain's walls stay impassable;
/// bodies are not asked, the step is (`stepToward`). An exit tile leads to
/// its landing alone, as there.
let private firstBarrier
    (a: Arena)
    (mover: Body)
    (goal: RoomPos)
    : (RoomPos * ArenaStructure) option =
    let strike = max 1 (structureStrike mover)

    let npc =
        match mover.Side with
        | Side.Npc _ -> true
        | _ -> false

    let names = a.Rooms |> Map.keys |> Array.ofSeq
    let side = Engine.roomSide
    let area = side * side
    let indexOf = names |> Array.mapi (fun i n -> n, i) |> Map.ofArray
    let rooms = names |> Array.map (fun n -> Map.find n a.Rooms)
    let barriers = rooms |> Array.map (barriersFor mover.Side)
    let tileOf (p: Pos) = p.X * side + p.Y

    // Per room, the tiles nothing passes — terrain walls, every rock, the
    // controller, the side's own obstacles, and the ring for an NPC — and
    // the hits a barrier stack holds per tile.
    let impassable, barrierHits =
        rooms
        |> Array.map (fun r ->
            let blocked = Array.copy (wallsOf r)
            let hits = Array.zeroCreate area

            for _, p in r.Capture.Rocks @ Option.toList r.Capture.RealController do
                blocked[tileOf p] <- true

            for s in r.Structures do
                if bars mover.Side s then
                    if s.Owner = Some mover.Side then
                        blocked[tileOf s.At] <- true
                    else
                        hits[tileOf s.At] <- hits[tileOf s.At] + s.Hits

            if npc then
                for t in 0 .. area - 1 do
                    if Seam.onRing { X = t / side; Y = t % side } then
                        blocked[t] <- true

            blocked, hits)
        |> Array.unzip

    let posOf (n: int) =
        {
            Room = names[n / area]
            X = n % area / side
            Y = n % side
        }

    let node (at: RoomPos) =
        indexOf[at.Room] * area + at.X * side + at.Y

    // Each tile's cost, read the first time it is asked: -1 impassable.
    let known = Array.create (names.Length * area) -2

    let cost (n: int) =
        if known[n] = -2 then
            let i = n / area
            let t = n % area

            known[n] <-
                if impassable[i][t] then
                    -1
                elif barrierHits[i][t] > 0 then
                    1 + barrierHits[i][t] / strike
                else
                    1

        known[n]

    let dist = Array.create (names.Length * area) System.Int32.MaxValue
    let parent = Array.create (names.Length * area) -1
    let carried = Array.create (names.Length * area) false
    let queue = PriorityQueue<int, int>()
    let start = node mover.At
    let target = if Map.containsKey goal.Room indexOf then node goal else -1
    dist[start] <- 0
    carried[start] <- true
    queue.Enqueue(start, 0)
    let mutable found = false

    while not found && queue.Count > 0 do
        let mutable n = start
        let mutable d = 0
        queue.TryDequeue(&n, &d) |> ignore

        if n = target then
            found <- true
        elif d <= dist[n] then
            let i = n / area
            let x = n % area / side
            let y = n % side

            let relax (m: int) =
                let c = cost m

                if c >= 0 && d + c < dist[m] then
                    dist[m] <- d + c
                    parent[m] <- n
                    carried[m] <- m / area <> i
                    queue.Enqueue(m, d + c)

            if Seam.onRing { X = x; Y = y } && Seam.isExit { X = x; Y = y } && not carried[n] then
                match landingOf (posOf n) with
                | Some(roomName, landing) when Map.containsKey roomName indexOf ->
                    relax (node (RoomPos.at roomName landing))
                | _ -> ()
            else
                // The eight steps, clamped to the room as `stepTo` clamps.
                for dx in -1 .. 1 do
                    for dy in -1 .. 1 do
                        let nx = max 0 (min Seam.exitEdge (x + dx))
                        let ny = max 0 (min Seam.exitEdge (y + dy))

                        if nx <> x || ny <> y then
                            relax (i * area + nx * side + ny)

    if not found then
        None
    else
        let rec unwind (m: int) acc =
            if m = start then acc else unwind parent[m] (m :: acc)

        unwind target []
        |> List.tryPick (fun m ->
            Map.tryFind (RoomPos.pos (posOf m)) barriers[m / area]
            |> Option.bind List.tryHead
            |> Option.map (fun s -> posOf m, s))

let private isHealer (b: Body) =
    let active = live b

    List.contains Heal active
    && not (List.contains Attack active)
    && not (List.contains RangedAttack active)

/// The focused enemy among candidates, seen from a tile, deterministic to
/// the id.
let private focusFrom (focus: Focus) (from: RoomPos) (candidates: Body list) : Body option =
    let distance (b: Body) =
        range from b.At |> Option.defaultValue System.Int32.MaxValue

    match focus with
    | Nearest -> candidates |> List.sortBy (fun b -> distance b, b.Hits, b.Id)
    | LowestHits -> candidates |> List.sortBy (fun b -> b.Hits, distance b, b.Id)
    | HealersFirst ->
        candidates
        |> List.sortBy (fun b -> (if isHealer b then 0 else 1), b.Hits, distance b, b.Id)
    |> List.tryHead

/// The focused enemy among candidates, seen from a body.
let private focusOf (focus: Focus) (from: Body) (candidates: Body list) : Body option =
    focusFrom focus from.At candidates

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
        let blocked = structureTiles b.Side r

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
        // A free tile in reach: one another body stands on is no goal.
        let taken =
            a.Bodies
            |> List.filter (fun o -> o.Id <> b.Id)
            |> List.map (fun o -> o.At)
            |> Set.ofList

        stepToward a b (Set.difference (ringAround keep target.At) taken)
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
let rec private scriptActs (a: Arena) (planned: Map<string, RoomPos>) (b: Body) : Act list =
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

        // Never onto an exit tile beside the leader, nor waiting on one: the
        // tick's end would carry it across the border (`creeps/tick.js`).
        let onEdge (at: RoomPos) = Seam.onRing (RoomPos.pos at)

        let step =
            match leader with
            | None -> None
            | Some l ->
                let next = Map.tryFind l.Id planned |> Option.defaultValue l.At

                if next <> l.At && within 1 b.At l.At && next.Room = l.At.Room then
                    directionTo (RoomPos.pos b.At) (RoomPos.pos l.At)
                elif within 1 b.At next && not (onEdge b.At) then
                    None
                else
                    stepToward
                        a
                        b
                        (ringAround 1 next |> Set.remove next |> Set.filter (onEdge >> not))

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
    | Breach goal ->
        match firstBarrier a b goal with
        | Some(target, s) ->
            let works = activeCount b Work > 0
            let swings = activeCount b Attack > 0

            let reach =
                if works || swings then
                    Engine.meleeRange
                else
                    Engine.rangedRange

            let melee =
                if works && within Engine.meleeRange b.At target then
                    [ Act.Dismantle s.Id ]
                elif swings && within Engine.meleeRange b.At target then
                    [ Act.Attack s.Id ]
                else
                    []

            let shot =
                if activeCount b RangedAttack > 0 && within Engine.rangedRange b.At target then
                    [ Act.RangedAttack s.Id ]
                else
                    []

            // Heal suppresses a dismantle and an attack, never a shot.
            let heal =
                if List.isEmpty melee && activeCount b Heal > 0 && b.Hits < hitsMax b then
                    [ Act.Heal b.Id ]
                else
                    []

            let step =
                if within reach b.At target then
                    None
                else
                    stepToward a b (ringAround reach target |> Set.remove target)

            melee @ shot @ heal @ move step
        | None -> fightInReach a Nearest b @ move (stepToward a b (Set.singleton goal))
    | TakeReactor at ->
        match Map.tryFind at.Room a.Rooms |> Option.bind (fun r -> r.Reactor) with
        | None -> move (stepToward a b (Set.singleton at))
        | Some reactor when reactor.Owner = Some b.Side ->
            let parking =
                ringAround 2 at
                |> Set.filter (fun tile -> range tile at = Some 2)
                |> Set.filter (fun tile ->
                    terrainAt (Map.find tile.Room a.Rooms) (RoomPos.pos tile) <> Wall)

            if Set.contains b.At parking then
                []
            else
                move (stepToward a b parking)
        | Some reactor ->
            if within 1 b.At at then
                [ Act.ClaimReactor reactor.Id ]
            else
                // A free tile of the ring, as a `moveTo` that paths around
                // creeps would find one.
                let taken = a.Bodies |> List.map (fun o -> o.At) |> Set.ofList
                let ring = ringAround 1 at |> Set.remove at
                let free = Set.difference ring taken
                move (stepToward a b (if Set.isEmpty free then ring else free))
    | Sweep(goal, focus, keep) ->
        if List.isEmpty enemies then
            fightInReach a Nearest b @ move (stepToward a b (Set.singleton goal))
        else
            scriptActs
                a
                planned
                { b with
                    Script = Some(Kite(focus, keep))
                }
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
    | Act.Dismantle _ -> "dismantle"
    | Act.Repair _ -> "repair"
    | Act.Transfer _
    | Act.TransferThorium _ -> "transfer"
    | Act.ClaimReactor _ -> "claimReactor"
    | Act.ClaimController _ -> "claimController"
    | Act.UpgradeController _ -> "upgradeController"
    | Act.Build _ -> "build"
    | Act.Withdraw _ -> "withdraw"

/// `creeps/intents.js` `priorities`: the acts that suppress each act.
let private suppressedBy (name: string) : string list =
    match name with
    | "rangedHeal" -> [ "heal" ]
    | "attackController" -> [ "rangedHeal"; "heal" ]
    | "dismantle" -> [ "attackController"; "rangedHeal"; "heal" ]
    | "repair" -> [ "dismantle"; "attackController"; "rangedHeal"; "heal" ]
    | "build" -> [ "repair"; "dismantle"; "attackController"; "rangedHeal"; "heal" ]
    | "attack" -> [ "build"; "repair"; "dismantle"; "attackController"; "rangedHeal"; "heal" ]
    | "rangedMassAttack" -> [ "build"; "repair"; "rangedHeal" ]
    | "rangedAttack" -> [ "rangedMassAttack"; "build"; "repair"; "rangedHeal" ]
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
/// `_healToApply` off start-of-tick positions and parts; and the structures,
/// which take their damage and repair the moment it lands (`_damage.js`,
/// `creeps/repair.js`).
type private Ledger =
    {
        Damage: Dictionary<string, int>
        Healing: Dictionary<string, int>
        /// Every structure still standing, by id, beside its room.
        Standing: Dictionary<string, string * ArenaStructure>
        /// The energy each body has spent on repair this tick.
        Spent: Dictionary<string, int>
        Events: ResizeArray<ArenaEvent>
    }

let private credit (table: Dictionary<string, int>) (id: string) (amount: int) =
    match table.TryGetValue id with
    | true, held -> table[id] <- held + amount
    | _ -> table[id] <- amount

/// Where a standing structure is.
let private placeOf (room: string, s: ArenaStructure) = RoomPos.at room s.At

/// The rampart standing on a tile, which takes every hit aimed at what
/// stands under it (`attack.js`, `rangedAttack.js`, `dismantle.js`,
/// `towers/attack.js`).
let private rampartOn (ledger: Ledger) (tile: RoomPos) : ArenaStructure option =
    ledger.Standing.Values
    |> Seq.tryPick (fun (room, s) ->
        if s.Kind = "rampart" && RoomPos.at room s.At = tile then
            Some s
        else
            None)

/// Damage landed on a structure: off its hits, and gone at none
/// (`_damage.js`, `structures/_destroy.js`).
let private strike (ledger: Ledger) (id: string) (amount: int) =
    match ledger.Standing.TryGetValue id with
    | true, (room, s) when amount > 0 ->
        let hits = s.Hits - amount

        if hits <= 0 then
            ledger.Standing.Remove id |> ignore
            ledger.Events.Add(Destroyed id)
        else
            ledger.Standing[id] <- (room, { s with Hits = hits })
    | _ -> ()

/// Repair landed on a structure, capped at its max.
let private mend (ledger: Ledger) (id: string) (amount: int) =
    match ledger.Standing.TryGetValue id with
    | true, (room, s) ->
        ledger.Standing[id] <-
            (room,
             { s with
                 Hits = min s.HitsMax (s.Hits + amount)
             })
    | _ -> ()

/// Perform one body's acts (`attack.js`, `rangedAttack.js`,
/// `rangedMassAttack.js`, `heal.js`, `rangedHeal.js`, `dismantle.js`,
/// `repair.js`, `transfer.js`, `_damage.js`). Ranges are Chebyshev within
/// one room and never across a border.
let private perform (a: Arena) (ledger: Ledger) (b: Body) (act: Act) =
    let byId = a.Bodies |> List.map (fun o -> o.Id, o) |> Map.ofList
    let power part each = activeCount b part * each
    let stopped = safeModeStops a b

    // `attack.js` and `rangedAttack.js` refuse the body itself as a target.
    let target id reach =
        Map.tryFind id byId
        |> Option.filter (fun t -> t.Id <> b.Id && within reach b.At t.At)

    // A structure in reach, and the rampart over it if one stands there.
    let structureIn id reach =
        match ledger.Standing.TryGetValue id with
        | true, placed when within reach b.At (placeOf placed) ->
            Some(rampartOn ledger (placeOf placed) |> Option.defaultValue (snd placed))
        | _ -> None

    // A hit on a body lands on the rampart it stands on, if one does.
    let hit (t: Body) amount =
        match rampartOn ledger t.At with
        | Some cover -> strike ledger cover.Id amount
        | None -> credit ledger.Damage t.Id amount

    match act with
    | Act.Attack id when not stopped ->
        match target id Engine.meleeRange with
        | Some t ->
            let covered = (rampartOn ledger t.At).IsSome
            hit t (power Attack Engine.attackPower)
            // Strike-back: the target's own ATTACK lands on the attacker,
            // unless the hit landed on a rampart or the attacker stands on
            // one (`_damage.js`).
            let back = activeCount t Attack * Engine.attackPower

            if back > 0 && not covered && (rampartOn ledger b.At).IsNone then
                credit ledger.Damage b.Id back
        | None ->
            structureIn id Engine.meleeRange
            |> Option.iter (fun s -> strike ledger s.Id (power Attack Engine.attackPower))
    | Act.RangedAttack id when not stopped ->
        match target id Engine.rangedRange with
        | Some t -> hit t (power RangedAttack Engine.rangedAttackPower)
        | None ->
            structureIn id Engine.rangedRange
            |> Option.iter (fun s ->
                strike ledger s.Id (power RangedAttack Engine.rangedAttackPower))
    | Act.Dismantle id when not stopped ->
        structureIn id Engine.meleeRange
        |> Option.iter (fun s -> strike ledger s.Id (power Work dismantlePower))
    | Act.Repair id ->
        let spent =
            match ledger.Spent.TryGetValue b.Id with
            | true, n -> n
            | _ -> 0

        let energy = b.Energy - spent

        match ledger.Standing.TryGetValue id with
        | true, ((_, s) as placed) when
            energy > 0
            && s.Hits < s.HitsMax
            && within Engine.rangedRange b.At (placeOf placed)
            ->
            let effect =
                min (power Work repairPower) (min (energy * hitsPerEnergy) (s.HitsMax - s.Hits))

            if effect > 0 then
                mend ledger id effect
                credit ledger.Spent b.Id (min energy ((effect + hitsPerEnergy - 1) / hitsPerEnergy))
        | _ -> ()
    | Act.Transfer id ->
        let spent =
            match ledger.Spent.TryGetValue b.Id with
            | true, n -> n
            | _ -> 0

        match ledger.Standing.TryGetValue id with
        | true, ((room, s) as placed) when within Engine.meleeRange b.At (placeOf placed) ->
            let amount = min (b.Energy - spent) (capacityOf (builtKindOf s.Kind) - s.Energy)

            if amount > 0 then
                ledger.Standing[id] <- (room, { s with Energy = s.Energy + amount })
                credit ledger.Spent b.Id amount
        | _ -> ()
    // `withdraw.js`, energy only: beside the store, as much as the body has
    // room for, the store holds and was asked. Booked as negative spending,
    // so a later act this tick spends it.
    | Act.Withdraw(id, asked) when not stopped ->
        let spent =
            match ledger.Spent.TryGetValue b.Id with
            | true, n -> n
            | _ -> 0

        let room =
            activeCount b Carry * Engine.carryPartCapacity - (b.Energy - spent) - b.Thorium

        match ledger.Standing.TryGetValue id with
        | true, ((r, s) as placed) when within Engine.meleeRange b.At (placeOf placed) ->
            let amount =
                min room s.Energy
                |> fun n -> asked |> Option.map (min n) |> Option.defaultValue n

            if amount > 0 then
                ledger.Standing[id] <- (r, { s with Energy = s.Energy - amount })
                credit ledger.Spent b.Id -amount
        | _ -> ()
    | Act.RangedMassAttack when not stopped ->
        let each = power RangedAttack Engine.rangedAttackPower

        let rate r =
            match r with
            | 0
            | 1 -> 1.0
            | 2 -> 0.4
            | _ -> 0.1

        let landed r =
            int (System.Math.Round(float each * rate r))

        // Bodies of another side, skipping one under a rampart, which is
        // struck as a structure below.
        for t in a.Bodies do
            match range b.At t.At with
            | Some r when
                r <= Engine.rangedRange && t.Side <> b.Side && (rampartOn ledger t.At).IsNone
                ->
                credit ledger.Damage t.Id (landed r)
            | _ -> ()

        // Every owned structure of another side in reach, a rampart over
        // anything; a wall has no owner and is never struck.
        for room, s in List.ofSeq ledger.Standing.Values do
            let tile = RoomPos.at room s.At

            match s.Owner, range b.At tile with
            | Some owner, Some r when
                owner <> b.Side
                && r <= Engine.rangedRange
                && (s.Kind = "rampart" || (rampartOn ledger tile).IsNone)
                ->
                strike ledger s.Id (landed r)
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
/// CONTROLLER_ATTACK_BLOCKED_UPGRADE (1,000); refused while a block stands
/// from an earlier tick. `blocked` is that start-of-tick block: the new one
/// is written in `controllers/tick.js`, so a second tap the same tick lands.
let private tapController
    (a: Arena)
    (blocked: bool)
    (b: Body)
    (roomName: string)
    : ArenaRoom option =
    match Map.tryFind roomName a.Rooms with
    | Some r ->
        match r.Controller with
        | Some c when
            b.At.Room = roomName
            && within 1 b.At (RoomPos.at roomName c.At)
            && c.Owner <> Ownership.Unowned
            && not blocked
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

/// `claimController.js`: beside a controller nobody holds, with a live CLAIM
/// part, the room is the body's at level 1, `progress` 0; the downgrade
/// timer starts full (`controllers/tick.js`). The safe-mode stock and its
/// cooldown are left as they stand. The GCL is taken to allow it.
let private claimBy (a: Arena) (b: Body) (roomName: string) : ArenaRoom option =
    match Map.tryFind roomName a.Rooms with
    | Some({ Controller = Some c } as r) when
        b.At.Room = roomName
        && within 1 b.At (RoomPos.at roomName c.At)
        && c.Owner = Ownership.Unowned
        && activeCount b BodyPart.Claim > 0
        ->
        Some
            { r with
                Controller =
                    Some
                        { c with
                            Owner =
                                (if b.Side = Side.Ours then
                                     Ownership.Ours
                                 else
                                     Ownership.Rival)
                            Username = (if b.Side = Side.Ours then None else Some(username b.Side))
                            Level = 1
                            Progress = 0
                            TicksToDowngrade = fullDowngrade 1
                        }
            }
    | _ -> None

/// `upgradeController.js`: in range 3 of a controller of the body's own,
/// carrying energy, upgrades not `blocked` at the start of the tick (a tap
/// this tick blocks the next): UPGRADE_CONTROLLER_POWER (1) a live WORK,
/// capped by the energy. At CONTROLLER_LEVELS the level rises — only while
/// the downgrade timer stands within CONTROLLER_DOWNGRADE_RESTORE (100) of
/// full — with `downgradeTime` reset to half the new level's and one safe
/// mode banked. The energy spent and whether it leveled.
let private upgradeBy (a: Arena) (blocked: bool) (b: Body) (energy: int) (roomName: string) =
    match Map.tryFind roomName a.Rooms with
    | Some({ Controller = Some c } as r) when
        b.At.Room = roomName
        && energy > 0
        && within 3 b.At (RoomPos.at roomName c.At)
        && c.Level > 0
        && c.Owner = Ownership.Ours
        && b.Side = Side.Ours
        && not blocked
        && activeCount b Work > 0
        ->
        let effect = min (activeCount b Work) energy
        let next = levelPoints c.Level

        let leveled =
            c.Level < 8
            && c.Progress + effect >= next
            && c.TicksToDowngrade + 100 >= fullDowngrade c.Level

        let controller =
            if leveled then
                { c with
                    Level = c.Level + 1
                    Progress = c.Progress + effect - next
                    TicksToDowngrade = fullDowngrade (c.Level + 1) / 2
                    SafeModeAvailable = c.SafeModeAvailable + 1
                }
            else
                { c with
                    Progress = c.Progress + effect
                }

        Some({ r with Controller = Some controller }, effect, leveled)
    | _ -> None

/// `build.js`: in range 3 of a site, carrying energy: BUILD_POWER (5) a live
/// WORK, capped by the energy and what is left. A site of an obstacle kind
/// takes nothing while a body stands on it — under its owner's safe mode,
/// only a body of the owner's. Finished, the structure stands: a spawn
/// named, empty, at SPAWN_HITS; a rampart at RAMPART_HITS (1) under the
/// level's max; a tower at TOWER_HITS, empty. The site, its room, the energy
/// spent, and the structure if it finished.
let private buildBy (a: Arena) (b: Body) (energy: int) (siteId: string) =
    let found =
        a.Rooms
        |> Map.toSeq
        |> Seq.tryPick (fun (name, r) ->
            r.Sites
            |> List.tryFind (fun site -> site.Id = siteId)
            |> Option.map (fun site -> name, r, site))

    match found with
    | Some(name, r, site) when
        b.At.Room = name && energy > 0 && within 3 b.At (RoomPos.at name site.At)
        ->
        let tile = RoomPos.at name site.At

        // `mySafeMode`: the builder's own safe mode runs here.
        let mySafeMode =
            b.Side = Side.Ours
            && r.Controller
               |> Option.exists (fun c -> c.SafeModeUntil > a.Time && c.Owner = Ownership.Ours)

        let standing =
            a.Bodies
            |> List.exists (fun o -> o.At = tile && (not mySafeMode || o.Side = b.Side))

        if Set.contains site.Kind obstacleKinds && standing then
            None
        else
            let effect = min (activeCount b Work * 5) (min energy (site.Total - site.Progress))

            if effect <= 0 then
                None
            else
                let progress = site.Progress + effect

                let structure =
                    if progress < site.Total then
                        None
                    else
                        let level =
                            r.Controller |> Option.map (fun c -> c.Level) |> Option.defaultValue 0

                        let owner = Some site.Owner

                        Some(
                            match site.Kind with
                            | "spawn" ->
                                { structureOf "spawn" owner 5000 5000 site.At with
                                    Id = $"spawn-{site.Name |> Option.defaultValue site.Id}"
                                    Name = site.Name
                                }
                            | "rampart" ->
                                { structureOf "rampart" owner 1 (rampartHitsMax level) site.At with
                                    NextDecay = a.Time + rampartDecayTime
                                }
                            | "tower" -> towerOf site.Owner site.At 0
                            | kind -> structureOf kind owner 1000 1000 site.At
                        )

                Some(name, { site with Progress = progress }, effect, structure)
    | _ -> None

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

    let obstacle (b: Body) (tile: RoomPos) =
        let r = Map.find tile.Room a.Rooms
        let p = RoomPos.pos tile

        terrainAt r p = Wall
        || Set.contains p (structureTiles b.Side r)
        || a.Bodies |> List.exists (fun o -> o.At = tile && not (moving.Contains o.Id))

    for tile in List.ofSeq order do
        match matrix.TryGetValue tile with
        | true, b when not (canMove b) || obstacle b tile -> strike tile
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

/// What one tower does this tick.
type private TowerAct =
    | Fire of body: string
    | HealBody of body: string
    | Fix of structure: string

/// One tower act (`towers/attack.js`, `towers/heal.js`, `towers/repair.js`):
/// nothing below TOWER_ENERGY_COST or across a border, else that energy
/// spent and the falloff curve's power landed — a shot on the rampart over
/// its target if one stands there.
let private towerDoes (a: Arena) (ledger: Ledger) (towerId: string) (act: TowerAct) =
    match ledger.Standing.TryGetValue towerId with
    | true, ((room, tower) as placed) when
        tower.Kind = "tower" && tower.Energy >= Engine.towerEnergyCost
        ->
        let from = placeOf placed

        let reach (at: RoomPos) =
            RoomPos.range from at |> Option.filter (fun _ -> at.Room = room)

        let landed =
            match act with
            | Fire id ->
                a.Bodies
                |> List.tryFind (fun t -> t.Id = id)
                |> Option.bind (fun t -> reach t.At |> Option.map (fun r -> t, r))
                |> Option.map (fun (t, r) ->
                    match rampartOn ledger t.At with
                    | Some cover -> strike ledger cover.Id (Engine.towerAttackAt r)
                    | None -> credit ledger.Damage t.Id (Engine.towerAttackAt r))
            | HealBody id ->
                a.Bodies
                |> List.tryFind (fun t -> t.Id = id)
                |> Option.bind (fun t -> reach t.At |> Option.map (fun r -> t, r))
                |> Option.map (fun (t, r) -> credit ledger.Healing t.Id (Engine.towerHealAt r))
            | Fix id ->
                match ledger.Standing.TryGetValue id with
                | true, ((_, s) as target) when s.Hits < s.HitsMax ->
                    reach (placeOf target)
                    |> Option.map (fun r -> mend ledger id (Engine.towerRepairAt r))
                | _ -> None

        if landed.IsSome then
            // Re-read: a tower can stand under its own rampart's repair.
            let room, tower = ledger.Standing[towerId]

            ledger.Standing[towerId] <-
                (room,
                 { tower with
                     Energy = tower.Energy - Engine.towerEnergyCost
                 })
    | _ -> ()

/// The towers not ours, each running its room's duties off the start-of-tick
/// bodies: the first duty with a target is its act.
let private theirTowers (a: Arena) (ledger: Ledger) : (string * TowerAct) list =
    a.Rooms
    |> Map.toList
    |> List.collect (fun (name, r) ->
        r.Structures
        |> List.filter (fun s -> s.Kind = "tower" && s.Owner.IsSome && s.Owner <> Some Side.Ours)
        |> List.choose (fun tower ->
            let side = tower.Owner.Value
            let here = a.Bodies |> List.filter (fun b -> b.At.Room = name)

            let duty d =
                match d with
                | HealHurt ->
                    here
                    |> List.filter (fun b -> b.Side = side && b.Hits < hitsMax b)
                    |> List.sortBy (fun b -> -(hitsMax b - b.Hits), b.Id)
                    |> List.tryHead
                    |> Option.map (fun b -> HealBody b.Id)
                | Shoot focus ->
                    here
                    |> List.filter (fun b -> sidesHostile side b.Side)
                    |> focusFrom focus (RoomPos.at name tower.At)
                    |> Option.map (fun b -> Fire b.Id)
                | Mend below ->
                    ledger.Standing.Values
                    |> Seq.filter (fun (room, s) ->
                        room = name
                        && (s.Kind = "rampart" && s.Owner = Some side
                            || s.Kind = "constructedWall")
                        && s.Hits < below
                        && s.Hits < s.HitsMax)
                    |> Seq.sortBy (fun (_, s) -> s.Hits, s.Id)
                    |> Seq.tryHead
                    |> Option.map (fun (_, s) -> Fix s.Id)

            r.Duties |> List.tryPick duty |> Option.map (fun act -> tower.Id, act)))

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

    // The room a controller stands in, by its id.
    let controllerRoom (id: string) =
        a.Rooms
        |> Map.tryFindKey (fun _ r -> r.Controller |> Option.exists (fun c -> c.Id = id))

    // Our Intents as engine acts, by the body they name; the rest are the
    // towers' and the controller's, below, or economy the arena does not
    // perform.
    let ourActs =
        ours
        |> List.choose (function
            | ClaimController(n, c) ->
                controllerRoom c |> Option.map (fun r -> n, Act.ClaimController r)
            | UpgradeController(n, c) ->
                controllerRoom c |> Option.map (fun r -> n, Act.UpgradeController r)
            | BuildSite(n, s) -> Some(n, Act.Build s)
            | WithdrawFromStore(n, s, Energy, amount) -> Some(n, Act.Withdraw(s, amount))
            | AttackCreep(n, h) -> Some(n, Act.Attack h)
            | RangedAttackCreep(n, h) -> Some(n, Act.RangedAttack h)
            | HealCreep(n, t) -> Some(n, Act.Heal t)
            | RangedHealCreep(n, t) -> Some(n, Act.RangedHeal t)
            | MoveCreep(n, d) -> Some(n, Act.Move d)
            | RepairStructure(n, s) -> Some(n, Act.Repair s)
            | TransferEnergyToStructure(n, s, Energy) -> Some(n, Act.Transfer s)
            | TransferEnergyToStructure(n, s, Thorium) -> Some(n, Act.TransferThorium s)
            | ClaimReactor(n, r) -> Some(n, Act.ClaimReactor r)
            | DismantleStructure(n, s) -> Some(n, Act.Dismantle s)
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
            Standing =
                Dictionary<string, string * ArenaStructure>(
                    a.Rooms
                    |> Map.toSeq
                    |> Seq.collect (fun (name, r) ->
                        r.Structures |> Seq.map (fun s -> KeyValuePair(s.Id, (name, s))))
                )
            Spent = Dictionary<string, int>()
            Events = events
        }

    let mutable rooms = a.Rooms
    let poured = Dictionary<string, int>()

    // The Reactor beside a body, by id, and the room it stands in.
    let reactorBeside (b: Body) (id: string) =
        rooms
        |> Map.tryFind b.At.Room
        |> Option.bind (fun r -> r.Reactor |> Option.map (fun reactor -> r, reactor))
        |> Option.filter (fun (_, reactor) ->
            reactor.Id = id && within 1 b.At (RoomPos.at b.At.Room reactor.At))

    // The rooms whose controller was upgraded, and the ones claimed, this
    // tick: what `controllers/tick.js` reads for the downgrade timer.
    let upgraded = HashSet<string>()
    let claimed = HashSet<string>()

    let energyLeft (b: Body) =
        b.Energy
        - (match ledger.Spent.TryGetValue b.Id with
           | true, n -> n
           | _ -> 0)

    // The upgrade block standing at the start of the tick: what the intents
    // read, `upgradeBlocked` being written only in `controllers/tick.js`.
    let blockedAtStart (roomName: string) =
        Map.tryFind roomName a.Rooms
        |> Option.bind (fun r -> r.Controller)
        |> Option.exists (fun c -> c.UpgradeBlockedUntil > a.Time)

    // Every body's acts in id order, on start-of-tick positions and parts.
    for b in a.Bodies |> List.sortBy (fun b -> b.Id) do
        for act in Map.tryFind b.Id actsOf |> Option.defaultValue [] do
            match act with
            | Act.ClaimController roomName ->
                match claimBy { a with Rooms = rooms } b roomName with
                | Some r ->
                    rooms <- Map.add roomName r rooms
                    claimed.Add roomName |> ignore
                    events.Add(ControllerClaimed(roomName, b.Id))
                | None -> ()
            | Act.UpgradeController roomName ->
                match
                    upgradeBy
                        { a with Rooms = rooms }
                        (blockedAtStart roomName)
                        b
                        (energyLeft b)
                        roomName
                with
                | Some(r, spent, leveled) ->
                    rooms <- Map.add roomName r rooms
                    upgraded.Add roomName |> ignore
                    credit ledger.Spent b.Id spent

                    if leveled then
                        events.Add(LeveledUp(roomName, r.Controller.Value.Level))
                | None -> ()
            | Act.Build siteId ->
                match buildBy { a with Rooms = rooms } b (energyLeft b) siteId with
                | Some(roomName, site, spent, built) ->
                    let r = Map.find roomName rooms
                    credit ledger.Spent b.Id spent

                    match built with
                    | Some structure ->
                        ledger.Standing[structure.Id] <- (roomName, structure)

                        rooms <-
                            Map.add
                                roomName
                                { r with
                                    Sites = r.Sites |> List.filter (fun s -> s.Id <> site.Id)
                                    Structures = r.Structures @ [ structure ]
                                }
                                rooms

                        events.Add(Built site.Id)
                    | None ->
                        rooms <-
                            Map.add
                                roomName
                                { r with
                                    Sites =
                                        r.Sites
                                        |> List.map (fun s -> if s.Id = site.Id then site else s)
                                }
                                rooms
                | None -> ()
            | Act.AttackController roomName ->
                match
                    tapController { a with Rooms = rooms } (blockedAtStart roomName) b roomName
                with
                | Some r ->
                    rooms <- Map.add roomName r rooms
                    events.Add(ControllerAttacked(roomName, b.Id))
                | None -> ()
            // `creep.claimReactor.js`: adjacent and a live CLAIM part, then
            // only `user` is written (`bulk.update` merges it into the
            // object, so this tick's burn is already the new owner's).
            | Act.ClaimReactor id ->
                match reactorBeside b id with
                | Some(r, reactor) when activeCount b BodyPart.Claim > 0 ->
                    rooms <-
                        Map.add
                            b.At.Room
                            { r with
                                Reactor = Some { reactor with Owner = Some b.Side }
                            }
                            rooms

                    events.Add(ReactorClaimed(b.At.Room, b.Id))
                | _ -> ()
            // `transfer.js`, generic: the Reactor takes T up to its
            // `storeCapacityResource` (`reactor.roomObject.js`).
            | Act.TransferThorium id ->
                match reactorBeside b id with
                | Some(r, reactor) ->
                    let amount = min b.Thorium (Engine.reactorCapacity - reactor.Thorium)

                    if amount > 0 then
                        rooms <-
                            Map.add
                                b.At.Room
                                { r with
                                    Reactor =
                                        Some
                                            { reactor with
                                                Thorium = reactor.Thorium + amount
                                            }
                                }
                                rooms

                        credit poured b.Id amount
                | None -> ()
            | Act.Move _ -> ()
            | other -> perform a ledger b other

    // The towers: ours by our Intents, the rest by their rooms' duties; one
    // act a tower, heal before repair before attack (`towers/intents.js`).
    let ourTowers =
        ours
        |> List.choose (function
            | FireTower(tower, hostile) -> Some(tower, Fire hostile)
            | HealWithTower(tower, creep) -> Some(tower, HealBody creep)
            | _ -> None)
        |> List.filter (fun (tower, _) ->
            match ledger.Standing.TryGetValue tower with
            | true, (_, s) -> s.Owner = Some Side.Ours
            | _ -> false)

    let rank act =
        match act with
        | HealBody _ -> 0
        | Fix _ -> 1
        | Fire _ -> 2

    for tower, act in
        ourTowers @ theirTowers a ledger
        |> List.groupBy fst
        |> List.map (fun (tower, acts) -> tower, acts |> List.map snd |> List.minBy rank) do
        towerDoes a ledger tower act

    for intent in ours do
        match intent with
        | ActivateSafeMode controllerId ->
            match
                rooms
                |> Map.tryFindKey (fun _ r ->
                    r.Controller |> Option.exists (fun c -> c.Id = controllerId))
            with
            | Some name ->
                let r = Map.find name rooms

                // `controllers/activateSafeMode.js`: stock, no cooldown, no
                // upgrade block, the downgrade timer above half the level's
                // less CONTROLLER_DOWNGRADE_SAFEMODE_THRESHOLD (5,000); and
                // `controllers/tick.js` refuses it under a block a tap landed
                // this very tick, which `rooms` already holds. The cooldown
                // runs SAFE_MODE_COOLDOWN (50,000) from it.
                match r.Controller with
                | Some c when
                    c.SafeModeAvailable > 0
                    && c.SafeModeUntil <= a.Time
                    && c.SafeModeCooldown < a.Time
                    && c.UpgradeBlockedUntil <= a.Time
                    && c.TicksToDowngrade >= fullDowngrade c.Level / 2 - 5_000
                    ->
                    rooms <-
                        Map.add
                            name
                            { r with
                                Controller =
                                    Some
                                        { c with
                                            SafeModeAvailable = c.SafeModeAvailable - 1
                                            SafeModeUntil = a.Time + 20_000
                                            SafeModeCooldown = a.Time + 50_000
                                        }
                            }
                            rooms

                    events.Add(SafeModeActivated name)
                | _ -> ()
            | None -> ()
        | _ -> ()

    // `controllers/tick.js`, per owned controller: a claim starts the
    // downgrade timer full; an upgrade not under a block restores
    // CONTROLLER_DOWNGRADE_RESTORE (100), capped at full; else it runs down
    // one. A downgrade at zero is not modelled.
    for name, r in Map.toList rooms do
        match r.Controller with
        | Some c when c.Owner <> Ownership.Unowned && not (claimed.Contains name) ->
            let ticks =
                if upgraded.Contains name && c.UpgradeBlockedUntil <= a.Time then
                    min (c.TicksToDowngrade + 100) (fullDowngrade c.Level)
                else
                    c.TicksToDowngrade - 1

            rooms <-
                Map.add
                    name
                    { r with
                        Controller = Some { c with TicksToDowngrade = ticks }
                    }
                    rooms
        | _ -> ()

    // The Reactor's `postProcessObject` (`reactor.roomObject.js`), after
    // every intent: a dry store clears the streak and does nothing else; an
    // owned stocked one launches the streak if it has none, burns one T, and
    // scores its owner `1 + floor(log10(1 + gameTime - launchTime))`. An
    // owner change touches neither the store nor the streak.
    let mutable scores = a.Scores

    for name, r in Map.toList rooms do
        match r.Reactor with
        | Some reactor when reactor.Thorium = 0 && reactor.LaunchTime.IsSome ->
            rooms <-
                Map.add
                    name
                    { r with
                        Reactor = Some { reactor with LaunchTime = None }
                    }
                    rooms
        | Some({ Owner = Some owner } as reactor) when reactor.Thorium > 0 ->
            let launch = reactor.LaunchTime |> Option.defaultValue a.Time
            let score = 1 + int (floor (log10 (float (1 + a.Time - launch))))
            let who = username owner

            scores <- Map.add who (score + (Map.tryFind who scores |> Option.defaultValue 0)) scores

            rooms <-
                Map.add
                    name
                    { r with
                        Reactor =
                            Some
                                { reactor with
                                    Thorium = reactor.Thorium - 1
                                    LaunchTime = Some launch
                                }
                    }
                    rooms

            events.Add(Burned(name, who, score))
        | _ -> ()

    // A rampart's decay (`ramparts/tick.js`): RAMPART_DECAY_AMOUNT off at
    // `gameTime >= nextDecayTime - 1`, the next one RAMPART_DECAY_TIME on.
    for room, s in List.ofSeq ledger.Standing.Values do
        if s.Kind = "rampart" && a.Time >= s.NextDecay - 1 then
            strike ledger s.Id rampartDecayAmount

            match ledger.Standing.TryGetValue s.Id with
            | true, (_, left) ->
                ledger.Standing[s.Id] <-
                    (room,
                     { left with
                         NextDecay = a.Time + rampartDecayTime
                     })
            | _ -> ()

    // What stands at the end of the acts, in each room's own order.
    rooms <-
        rooms
        |> Map.map (fun _ r ->
            { r with
                Structures =
                    r.Structures
                    |> List.choose (fun s ->
                        match ledger.Standing.TryGetValue s.Id with
                        | true, (_, now) -> Some now
                        | _ -> None)
            })

    // Movement, resolved together, past whatever fell this tick.
    let moves =
        a.Bodies
        |> List.sortBy (fun b -> b.Id)
        |> List.choose (fun b ->
            Map.tryFind b.Id actsOf
            |> Option.defaultValue []
            |> List.tryPick (function
                | Act.Move d -> Some(b, stepTo b.At d)
                | _ -> None))

    let moved = resolveMoves { a with Rooms = rooms } moves

    // `movement.js` `execute`: a body stepping onto another side's site
    // removes it, unless safe mode runs here for somebody else than the
    // mover. The refund a begun site drops is economy, and not modelled.
    for b in a.Bodies |> List.sortBy (fun b -> b.Id) do
        match Map.tryFind b.Id moved with
        | Some dest ->
            let r = Map.find dest.Room rooms

            let shielded =
                r.Controller
                |> Option.exists (fun c ->
                    let mine =
                        match c.Owner, b.Side with
                        | Ownership.Ours, Side.Ours -> true
                        | Ownership.Rival, side ->
                            c.Username = Some(username side) && side <> Side.Ours
                        | _ -> false

                    c.SafeModeUntil > a.Time && not mine)

            match
                r.Sites
                |> List.tryFind (fun site -> site.At = RoomPos.pos dest && site.Owner <> b.Side)
            with
            | Some site when not shielded ->
                rooms <-
                    Map.add
                        dest.Room
                        { r with
                            Sites = r.Sites |> List.filter (fun s -> s.Id <> site.Id)
                        }
                        rooms

                events.Add(SiteStomped(site.Id, b.Id))
            | _ -> ()
        | None -> ()

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

                let spent =
                    match ledger.Spent.TryGetValue b.Id with
                    | true, n -> n
                    | _ -> 0

                landed
                |> Option.map (fun at ->
                    let script = b.Script |> Option.defaultValue Hold

                    { b with
                        At = at
                        Fatigue = fatigue
                        Hits = hits
                        Energy = b.Energy - spent
                        Thorium =
                            b.Thorium
                            - (match poured.TryGetValue b.Id with
                               | true, n -> n
                               | _ -> 0)
                        TicksToLive = b.TicksToLive - 1
                        Retreating = retreatLatch a.Tick { b with Hits = hits } script
                    }))

    // `spawns/create-creep.js`: an idle spawn of ours takes a cast of a row
    // the arena performs, for CREEP_SPAWN_TIME a part, the bank paying.
    let started =
        ours
        |> List.choose (function
            | SpawnCreep(spawnName, bodyParts, name) ->
                let role = name.Split('-')[0]

                rooms
                |> Map.toList
                |> List.tryPick (fun (roomName, r) ->
                    r.Structures
                    |> List.tryFind (fun s -> s.Name = Some spawnName && s.Owner = Some Side.Ours)
                    |> Option.map (fun s -> roomName, r, s))
                |> Option.filter (fun (_, r, s) ->
                    Set.contains role a.Casts
                    && r.Bank >= bodyCost bodyParts
                    && not (a.Ovens |> List.exists (fun oven -> oven.Spawn = s.Id)))
                |> Option.map (fun (roomName, _, s) ->
                    {
                        Spawn = s.Id
                        Room = roomName
                        Name = name
                        Parts = bodyParts
                        Done = a.Time + Engine.spawnTicksPerPart * List.length bodyParts
                    })
            | _ -> None)

    // `spawns/tick.js` `_born`: a finished body steps out onto the first free
    // tile around its spawn, a CLAIM body with CREEP_CLAIM_LIFE_TIME.
    let born, ovens =
        (([], []), a.Ovens @ started)
        ||> List.fold (fun (born: Body list, waiting) oven ->
            let spawnAt =
                rooms[oven.Room].Structures
                |> List.tryFind (fun s -> s.Id = oven.Spawn)
                |> Option.map (fun s -> RoomPos.at oven.Room s.At)

            let taken = (settled @ born) |> List.map (fun b -> b.At) |> Set.ofList
            let r = rooms[oven.Room]
            let blocked = structureTiles Side.Ours r

            let free =
                spawnAt
                |> Option.bind (fun at ->
                    [ Top; TopRight; Right; BottomRight; Bottom; BottomLeft; Left; TopLeft ]
                    |> List.map (stepTo at)
                    |> List.tryFind (fun tile ->
                        let p = RoomPos.pos tile

                        not (Seam.onRing p)
                        && terrainAt r p <> Wall
                        && not (Set.contains p blocked)
                        && not (Set.contains tile taken)))

            match free with
            | Some tile when oven.Done <= a.Time + 1 ->
                events.Add(Born oven.Name)

                let life =
                    if List.contains BodyPart.Claim oven.Parts then
                        Engine.claimLifetime
                    else
                        Engine.creepLifetime

                ({ body oven.Name Side.Ours oven.Parts tile None with
                    TicksToLive = life
                 }
                 :: born),
                waiting
            | _ -> born, oven :: waiting)

    let next =
        { a with
            Time = a.Time + 1
            Tick = a.Tick + 1
            Rooms = rooms
            Bodies = settled @ List.rev born
            Scores = scores
            Ovens = List.rev ovens
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
            next.Bodies
            |> List.map (fun b ->
                {
                    Id = b.Id
                    Side = b.Side
                    At = b.At
                    Hits = b.Hits
                    Fatigue = b.Fatigue
                })
        Structures =
            rooms
            |> Map.toSeq
            |> Seq.collect (fun (_, r) -> r.Structures |> Seq.map (fun s -> s.Id, s.Hits))
            |> Map.ofSeq
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
