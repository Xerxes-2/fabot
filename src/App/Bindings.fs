// Minimal hand-written bindings: only the API surface the bot uses.
module Fabot.Bindings

open Fable.Core

/// In bytes.
type IHeapStatistics =
    abstract used_heap_size: float
    /// Outside the V8 heap but counted against the isolate's limit: an
    /// ArrayBuffer over a certain size (a 10 KB flood field) lands here.
    abstract externally_allocated_size: float

type ICpu =
    abstract getUsed: unit -> float
    /// The account's CPU allotment a tick.
    abstract limit: float
    abstract getHeapStatistics: unit -> IHeapStatistics
    /// The tick ceiling stays 500 ms while the bucket is non-empty, not
    /// `min(limit + bucket, 500)`.
    abstract bucket: int

let findSources = 105

let findMyStructures = 108

let findStructures = 107

let findMyConstructionSites = 114

/// Read because the engine takes one site per tile, whoever owns it.
let findHostileConstructionSites = 115

let findHostileCreeps = 103

/// Includes the NPC Invader. The only sweep that names an invader core:
/// `FIND_STRUCTURES` sees it as `BuiltKind.Other`.
let findHostileStructures = 109

let findHostilePowerCreeps = 121

let findDroppedResources = 106

/// The season mod puts a Thorium deposit beside the ordinary ore in most rooms.
let findMinerals = 116

/// The season mod's (`reactor.roomObject.js`, mod 1.0.3): a reactor reaches no
/// `FIND_STRUCTURES` pass and has no `structureType`. The number, not the global.
let findReactors = 10051

let findTombstones = 118

/// Projected as `TargetKind.Tombstone`: one store with a clock on it.
let findRuins = 123

let findNukes = 117

let findMyCreeps = 102

let terrainMaskWall = 1

let terrainMaskSwamp = 2

// The STRUCTURE_* spellings live in Core (`builtKindName`); Core reads the
// invader core as a threat fact, never as a built kind.
let structureInvaderCore = "invaderCore"

/// When it runs out the engine removes the stronghold.
let effectCollapseTimer = 1002

type IStore =
    abstract getFreeCapacity: resource: string -> int
    abstract getUsedCapacity: resource: string -> int

type IRoomPosition =
    abstract x: int
    abstract y: int
    /// The light tick reads it off `Game.structures`, whose structures carry
    /// no room of their own on this binding.
    abstract roomName: string

/// `pos` is the landing tile.
type INuke =
    abstract id: string
    abstract pos: IRoomPosition
    abstract timeToLand: int
    abstract launchRoomName: string

type ISource =
    abstract id: string
    abstract pos: IRoomPosition
    abstract energy: int
    /// Undefined until the engine starts the timer.
    abstract ticksToRegeneration: int

type IEffect =
    /// An EFFECT_* or a Power id.
    abstract effect: int
    /// Relative to now; the HTTP API's raw documents carry an absolute `endTime`.
    abstract ticksRemaining: int

type IOwner =
    abstract username: string

/// Density and `ticksToRegeneration` are not bound: Thorium never
/// regenerates, and ore at zero is not dug.
type IMineral =
    abstract id: string
    abstract pos: IRoomPosition
    /// "T" is the season's Thorium.
    abstract mineralType: string
    /// The mod deletes a Thorium deposit the tick this hits zero; ore reads 0
    /// while it regenerates.
    abstract mineralAmount: int

type IStructure =
    abstract id: string
    abstract structureType: string
    /// Undefined on an unowned one (a road, a container, a wall).
    abstract owner: IOwner
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract hits: int
    abstract hitsMax: int
    /// Undefined when none, as `safeMode` and `reservation` are.
    abstract effects: IEffect[]
    /// Invader core only: 0 is a stronghold's expansion core, 1 and up the bunker.
    abstract level: int
    /// Defined on the extractor (`EXTRACTOR_COOLDOWN` 5) and the link
    /// (`LINK_COOLDOWN` times the last send's range).
    abstract cooldown: int
    /// Undefined on every kind but a rampart.
    abstract isPublic: bool

type IResource =
    abstract id: string
    abstract resourceType: string
    abstract pos: IRoomPosition
    abstract amount: int

type IFallenCreep =
    abstract id: string
    abstract my: bool
    abstract owner: IOwner

/// One binding for tombstone and ruin: the projection reads the same three
/// fields off both, and `creep.withdraw` takes any store.
type ITombstone =
    abstract id: string
    abstract pos: IRoomPosition
    abstract store: IStore
    /// Undefined on a ruin.
    abstract creep: IFallenCreep

type IConstructionSite =
    abstract id: string
    abstract structureType: string
    abstract pos: IRoomPosition
    abstract progress: int
    abstract progressTotal: int


/// Outlives every body in the room.
type ISign =
    /// At most a hundred characters.
    abstract text: string

/// Undefined on a controller nothing reserves and on every owned one: a
/// reservation and an owner are exclusive.
type IReservation =
    abstract username: string
    /// Decays by one a tick; caps at 5,000.
    abstract ticksToEnd: int

type IController =
    abstract id: string
    /// Undefined on an unowned controller, as `safeMode` is.
    abstract my: bool
    /// Undefined on an unowned one.
    abstract owner: IOwner
    /// Undefined when none.
    abstract reservation: IReservation
    abstract level: int
    /// Undefined on an unowned controller.
    abstract ticksToDowngrade: int
    abstract safeModeAvailable: int
    /// Undefined until somebody writes one.
    abstract sign: ISign
    /// Undefined when safe mode is off.
    abstract safeMode: int
    /// Undefined when no cooldown runs; defined on an unowned controller too.
    abstract safeModeCooldown: int
    /// Blocks safe mode and upgrading after an `attackController`; undefined
    /// when none runs.
    abstract upgradeBlocked: int
    abstract pos: IRoomPosition
    abstract activateSafeMode: unit -> int

/// `mod-season5/src/reactor.roomObject.js`: indestructible (no `hits`) and
/// walkable. Its tile is the declaration's (`Errand.place`), not read here.
type IReactor =
    abstract id: string
    /// Undefined on one nobody owns (the mod's own accessor).
    abstract my: bool
    /// Undefined on an unowned one.
    abstract owner: IOwner
    abstract store: IStore
    /// Ticks consumed without the store running dry; zero while idle.
    abstract continuousWork: int

type ITower =
    abstract attack: target: obj -> int
    /// Falls off with range; 10 energy an act.
    abstract heal: target: obj -> int
    /// Falls off with range; 10 energy an act.
    abstract repair: target: obj -> int

/// The target is credited `amount` less LINK_LOSS_RATIO, rounded down.
type ILink =
    abstract transferEnergy: target: obj * amount: int -> int

/// `mod-season5/src/terminal-restriction.js` nulls `send` only toward another
/// user's room. The optional `description` is omitted, never passed as null.
type ITerminal =
    abstract send: resourceType: string * amount: int * destination: string -> int

type IRoom =
    abstract name: string
    abstract energyAvailable: int
    abstract energyCapacityAvailable: int
    abstract find: findType: int -> obj[]
    /// Null in rooms without a controller.
    abstract controller: IController
    abstract createConstructionSite: x: int * y: int * structureType: string -> int

[<AllowNullLiteral>]
type ISpawning =
    abstract name: string
    abstract remainingTime: int

type ISpawn =
    abstract name: string
    abstract id: string
    /// Null when the spawn is idle.
    abstract spawning: ISpawning
    abstract owner: IOwner
    abstract room: IRoom
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract spawnCreep: body: string[] * name: string -> int

type IBodyPartDef =
    abstract ``type``: string
    /// A destroyed part stays in the array reading zero, so a count that does
    /// not read this counts weapons the creep no longer has.
    abstract hits: int
    /// Undefined unboosted.
    abstract boost: string

type ICreep =
    abstract id: string
    abstract name: string
    /// `Game.creeps` is world-wide; the projection is one room's.
    abstract room: IRoom
    abstract owner: IOwner
    abstract spawning: bool
    /// Undefined while spawning.
    abstract ticksToLive: int
    /// The creep cannot move while > 0.
    abstract fatigue: int
    abstract hits: int
    abstract hitsMax: int
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract body: IBodyPartDef[]
    abstract harvest: target: obj -> int
    abstract transfer: target: obj * resource: string -> int
    abstract withdraw: target: obj * resource: string -> int
    abstract build: target: obj -> int
    abstract repair: target: obj -> int
    /// Range 1, DISMANTLE_POWER per WORK part.
    abstract dismantle: target: obj -> int
    abstract upgradeController: target: obj -> int
    /// Range 1, one tick per CLAIM part; refused on a controller anybody owns.
    abstract reserveController: target: obj -> int
    /// Range 1, one tick per CLAIM part off any reservation, the Invader's too.
    abstract attackController: target: obj -> int
    /// Range 1, one CLAIM part; ERR_GCL_NOT_ENOUGH while every GCL level is spent.
    abstract claimController: target: obj -> int
    /// Range 1, no body part, a hundred characters; lasts until overwritten.
    abstract signController: target: obj * text: string -> int
    /// The season mod's intent: range 1, one live CLAIM part, no cooldown, no
    /// ownership precondition; leaves `launchTime` alone.
    abstract claimReactor: target: obj -> int
    abstract pickup: target: obj -> int
    /// The whole of one resource, onto the body's own tile.
    abstract drop: resource: string -> int
    /// Range 1, `ATTACK_POWER` per ATTACK part.
    abstract attack: target: obj -> int
    /// Range 1, itself included. A different act from `attack` in the engine,
    /// so a body carrying both parts does both in one tick.
    abstract heal: target: obj -> int
    /// Range 1..3, RANGED_HEAL_POWER per HEAL part.
    abstract rangedHeal: target: obj -> int
    /// Range 3, RANGED_ATTACK_POWER per RANGED_ATTACK part.
    abstract rangedAttack: target: obj -> int
    /// Every creep and structure not ours within range 3.
    abstract rangedMassAttack: unit -> int
    /// TOP = 1, clockwise. The only movement API bound; moveTo is not.
    abstract move: direction: int -> int
    /// The omitted `public` argument keeps bubbles private to our own viewer.
    abstract say: message: string -> int

type ITerrain =
    /// 0 plain, TERRAIN_MASK_WALL wall, 2 swamp.
    abstract get: x: int * y: int -> int

type IGameMap =
    abstract getRoomTerrain: roomName: string -> ITerrain

type IGame =
    abstract time: int
    abstract cpu: ICpu
    abstract map: IGameMap
    /// Visible rooms only.
    abstract rooms: obj
    abstract spawns: obj
    abstract creeps: obj
    /// Ours only, in every room.
    abstract structures: obj
    /// Ours only, in every room, seen or not.
    abstract constructionSites: obj
    /// Null when no object with that id exists or it is out of sight.
    abstract getObjectById: id: string -> obj

[<Global("Game")>]
let Game: IGame = jsNative

[<Global("Memory")>]
let Memory: obj = jsNative

/// Read and written by `Wire` alone.
[<Global("RawMemory")>]
let RawMemory: obj = jsNative

/// Write one memory segment (0..99, at most 100 KB, at most ten a tick). A
/// write needs no `setActiveSegments`; a read does, a tick ahead.
[<Emit("RawMemory.segments[$0] = $1")>]
let writeSegment (_id: int) (_text: string) : unit = jsNative

[<Emit("JSON.stringify($0)")>]
let jsonStringify (_value: obj) : string = jsNative

[<Emit("Object.values($0)")>]
let objectValues<'T> (_o: obj) : 'T[] = jsNative

[<Emit("Object.entries($0)")>]
let objectEntries (_o: obj) : (string * obj)[] = jsNative

[<Emit("Object.keys($0)")>]
let storeKeys (_store: IStore) : string[] = jsNative

[<Emit("$0.getUsedCapacity()")>]
let storeUsedTotal (_store: IStore) : int = jsNative

/// Against `Game.rooms` a null is exactly "no vision".
[<Emit("$0[$1]")>]
let objectItem<'T> (_o: obj) (_key: string) : 'T = jsNative

/// A stub of its own: F# has no optional argument on an abstract member, and
/// the two-argument call is not the same call as one passing `undefined`.
[<Emit("$0.withdraw($1, $2, $3)")>]
let withdrawAmount (_creep: ICreep) (_target: obj) (_resource: string) (_amount: int) : int =
    jsNative

/// A stub of its own, for `withdrawAmount`'s reason.
[<Emit("$0.createConstructionSite($1, $2, 'spawn', $3)")>]
let createSpawnSite (_room: IRoom) (_x: int) (_y: int) (_name: string) : int = jsNative

/// `game/structures.js`: ERR_NOT_OWNER on one not ours.
[<Emit("$0.setPublic($1)")>]
let setPublic (_rampart: obj) (_isPublic: bool) : int = jsNative

/// `game/structures.js`: ERR_NOT_OWNER on one not ours, ERR_BUSY while a
/// hostile creep stands in the room.
[<Emit("$0.destroy()")>]
let destroyStructure (_structure: obj) : int = jsNative

/// `game/construction-sites.js`: ERR_NOT_OWNER on one not ours.
[<Emit("$0.remove()")>]
let removeSite (_site: obj) : int = jsNative

/// `game/structures.js`: ERR_NOT_IN_RANGE past OBSERVER_RANGE,
/// ERR_RCL_NOT_ENOUGH below RCL8.
[<Emit("$0.observeRoom($1)")>]
let observeRoom (_observer: obj) (_room: string) : int = jsNative

/// `game/rooms.js`: the tick before's events, parsed once a room a tick.
[<Emit("$0.getEventLog()")>]
let eventLog (_room: IRoom) : obj[] = jsNative

/// EVENT_ATTACK: `data` holds `targetId`, `damage` and `attackType`; a
/// tower's shot is one, under the tower's id.
let eventAttack = 1

/// `game/structures.js`: ERR_TIRED on cooldown, ERR_NOT_IN_RANGE past range 2
/// of a reagent lab, ERR_NOT_ENOUGH_RESOURCES or ERR_INVALID_ARGS.
[<Emit("$0.runReaction($1, $2)")>]
let runReaction (_lab: obj) (_reagentLab: obj) (_otherReagentLab: obj) : int = jsNative
