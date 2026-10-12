// A field the engine may leave absent is typed as present; `World` reads it
// through `orZero` or `absent`.
module Fabot.Bindings

open Fable.Core

type IHeapStatistics =
    abstract used_heap_size: float
    /// Outside the V8 heap but counted against the isolate's limit: an
    /// ArrayBuffer over a certain size (a 10 KB flood field) lands here.
    abstract externally_allocated_size: float

type ICpu =
    abstract getUsed: unit -> float
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

let findMinerals = 116

/// The season mod's (`reactor.roomObject.js`, mod 1.0.3): a reactor reaches no
/// `FIND_STRUCTURES` pass and has no `structureType`. The number, not the global.
let findReactors = 10051

let findTombstones = 118

let findRuins = 123

let findNukes = 117

let findMyCreeps = 102

let terrainMaskWall = 1

let terrainMaskSwamp = 2

let structureInvaderCore = "invaderCore"

/// When it runs out the engine removes the stronghold.
let effectCollapseTimer = 1002

type IStore =
    abstract getFreeCapacity: resource: string -> int
    abstract getUsedCapacity: resource: string -> int

type IRoomPosition =
    abstract x: int
    abstract y: int
    abstract roomName: string

type INuke =
    abstract id: string
    abstract pos: IRoomPosition
    abstract timeToLand: int
    abstract launchRoomName: string

type ISource =
    abstract id: string
    abstract pos: IRoomPosition
    abstract energy: int
    abstract ticksToRegeneration: int

type IEffect =
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
    abstract mineralType: string
    /// The mod deletes a Thorium deposit the tick this hits zero; ore reads 0
    /// while it regenerates.
    abstract mineralAmount: int

type IStructure =
    abstract id: string
    abstract structureType: string
    abstract owner: IOwner
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract hits: int
    abstract hitsMax: int
    abstract effects: IEffect[]
    /// Invader core only: 0 is a stronghold's expansion core, 1 and up the bunker.
    abstract level: int
    /// Invader core only: absent once deployed.
    abstract ticksToDeploy: int
    abstract cooldown: int
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
    abstract creep: IFallenCreep

type IConstructionSite =
    abstract id: string
    abstract structureType: string
    abstract pos: IRoomPosition
    abstract progress: int
    abstract progressTotal: int

type ISign =
    abstract text: string

/// A reservation and an owner are exclusive.
type IReservation =
    abstract username: string
    abstract ticksToEnd: int

type IController =
    abstract id: string
    abstract my: bool
    abstract owner: IOwner
    abstract reservation: IReservation
    abstract level: int
    abstract ticksToDowngrade: int
    abstract safeModeAvailable: int
    abstract sign: ISign
    abstract safeMode: int
    abstract safeModeCooldown: int
    abstract upgradeBlocked: int
    abstract pos: IRoomPosition
    abstract activateSafeMode: unit -> int

/// `mod-season5/src/reactor.roomObject.js`: indestructible (no `hits`) and
/// walkable. Its tile is the declaration's (`Errand.place`), not read here.
type IReactor =
    abstract id: string
    abstract my: bool
    abstract owner: IOwner
    abstract store: IStore
    /// Ticks consumed without the store running dry; zero while idle.
    abstract continuousWork: int

type ITower =
    abstract attack: target: obj -> int
    abstract heal: target: obj -> int
    abstract repair: target: obj -> int

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
    abstract controller: IController
    abstract createConstructionSite: x: int * y: int * structureType: string -> int

[<AllowNullLiteral>]
type ISpawning =
    abstract name: string
    abstract remainingTime: int

type ISpawn =
    abstract name: string
    abstract id: string
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
    abstract boost: string

type ICreep =
    abstract id: string
    abstract name: string
    abstract room: IRoom
    abstract owner: IOwner
    abstract spawning: bool
    abstract ticksToLive: int
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
    abstract dismantle: target: obj -> int
    abstract upgradeController: target: obj -> int
    abstract reserveController: target: obj -> int
    abstract attackController: target: obj -> int
    abstract claimController: target: obj -> int
    abstract signController: target: obj * text: string -> int
    /// The season mod's intent: one live CLAIM part, no ownership precondition.
    abstract claimReactor: target: obj -> int
    abstract pickup: target: obj -> int
    abstract drop: resource: string -> int
    abstract attack: target: obj -> int
    /// A different act from `attack`: a body carrying both does both in one tick.
    abstract heal: target: obj -> int
    abstract rangedHeal: target: obj -> int
    abstract rangedAttack: target: obj -> int
    abstract rangedMassAttack: unit -> int
    abstract move: direction: int -> int
    /// The omitted `public` argument keeps bubbles private to our own viewer.
    abstract say: message: string -> int

type ITerrain =
    abstract get: x: int * y: int -> int

type IGameMap =
    abstract getRoomTerrain: roomName: string -> ITerrain

type IGame =
    abstract time: int
    abstract cpu: ICpu
    abstract map: IGameMap
    abstract rooms: obj
    abstract spawns: obj
    abstract creeps: obj
    abstract structures: obj
    abstract constructionSites: obj
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

[<Emit("$0.setPublic($1)")>]
let setPublic (_rampart: obj) (_isPublic: bool) : int = jsNative

/// ERR_BUSY while a hostile creep stands in the room.
[<Emit("$0.destroy()")>]
let destroyStructure (_structure: obj) : int = jsNative

[<Emit("$0.remove()")>]
let removeSite (_site: obj) : int = jsNative

[<Emit("$0.observeRoom($1)")>]
let observeRoom (_observer: obj) (_room: string) : int = jsNative

/// The tick before's events, parsed once a room a tick.
[<Emit("$0.getEventLog()")>]
let eventLog (_room: IRoom) : obj[] = jsNative

/// EVENT_ATTACK: `data` holds `targetId`, `damage` and `attackType`; a
/// tower's shot is one, under the tower's id.
let eventAttack = 1

/// ERR_NOT_IN_RANGE past range 2 of a reagent lab.
[<Emit("$0.runReaction($1, $2)")>]
let runReaction (_lab: obj) (_reagentLab: obj) (_otherReagentLab: obj) : int = jsNative
