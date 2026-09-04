// Minimal hand-written bindings: only the API surface the bot uses.
module Fabot.Bindings

open Fable.Core

type ICpu =
    abstract getUsed: unit -> float

/// Screeps `FIND_SOURCES` constant.
let findSources = 105

/// Screeps `FIND_MY_STRUCTURES` constant.
let findMyStructures = 108

/// Screeps `FIND_STRUCTURES` constant (any owner, includes neutral).
let findStructures = 107

/// Screeps `FIND_MY_CONSTRUCTION_SITES` constant.
let findMyConstructionSites = 114

/// Screeps `FIND_HOSTILE_CREEPS` constant.
let findHostileCreeps = 103

/// Screeps `FIND_DROPPED_RESOURCES` constant.
let findDroppedResources = 106

/// Screeps `TERRAIN_MASK_WALL` constant.
let terrainMaskWall = 1

/// Screeps `TERRAIN_MASK_SWAMP` constant.
let terrainMaskSwamp = 2

// The STRUCTURE_* spellings live in Core (`builtKindName`, #75): the kind
// predicates over them are Core rules, so the table has to be readable
// there — as `partName`'s is.

type IStore =
    abstract getFreeCapacity: resource: string -> int
    abstract getUsedCapacity: resource: string -> int

type IRoomPosition =
    abstract x: int
    abstract y: int

type ISource =
    abstract id: string
    abstract pos: IRoomPosition
    /// Energy remaining in the source this regen cycle.
    abstract energy: int
    /// Ticks until the source regenerates; undefined until the engine
    /// starts the timer.
    abstract ticksToRegeneration: int

type IStructure =
    abstract id: string
    /// Screeps STRUCTURE_* string, e.g. "spawn" or "extension".
    abstract structureType: string
    abstract store: IStore
    abstract pos: IRoomPosition
    /// Current hit points.
    abstract hits: int
    /// Maximum hit points.
    abstract hitsMax: int

/// A dropped resource pile lying on the ground.
type IResource =
    abstract id: string
    /// Screeps RESOURCE_* string, e.g. "energy".
    abstract resourceType: string
    abstract pos: IRoomPosition

type IConstructionSite =
    abstract id: string
    /// Screeps STRUCTURE_* string of what is being built.
    abstract structureType: string
    abstract pos: IRoomPosition

type IController =
    abstract id: string
    /// True when this controller is owned by us.
    abstract my: bool
    /// Controller level (RCL).
    abstract level: int
    /// Ticks left on the downgrade timer; undefined on unowned controllers.
    abstract ticksToDowngrade: int
    /// Safe-mode activations banked.
    abstract safeModeAvailable: int
    /// Ticks of safe mode remaining; undefined when safe mode is off.
    abstract safeMode: int
    abstract pos: IRoomPosition
    abstract activateSafeMode: unit -> int

type ITower =
    abstract attack: target: obj -> int

type IRoom =
    abstract name: string
    /// Energy available for spawning in this room (spawn + extensions).
    abstract energyAvailable: int
    /// Spawn-energy capacity of this room (spawn + built extensions).
    abstract energyCapacityAvailable: int
    abstract find: findType: int -> obj[]
    /// Null in rooms without a controller.
    abstract controller: IController
    abstract createConstructionSite: x: int * y: int * structureType: string -> int

type ISpawn =
    abstract name: string
    abstract id: string
    /// Null when the spawn is idle.
    abstract spawning: obj
    abstract room: IRoom
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract spawnCreep: body: string[] * name: string -> int

/// One entry of a creep's `body` array.
type IBodyPartDef =
    /// Part-type string, e.g. "work" or "claim".
    abstract ``type``: string

/// The `owner` sub-object every owned game object carries.
type IOwner =
    abstract username: string

type ICreep =
    abstract id: string
    abstract name: string
    /// The room the creep is standing in this tick. Read to keep the
    /// projection's creep table inside the room it is filed under
    /// (ADR 0041): `Game.creeps` is world-wide and the projection is one
    /// room's.
    abstract room: IRoom
    /// Whose creep this is. Read only off hostiles, for the Raid log's
    /// roster (ADR 0028); our own creeps' ownership is never in question.
    abstract owner: IOwner
    /// True while the creep is still being built inside the spawn.
    abstract spawning: bool
    /// Ticks the creep has left to live; undefined only while it is still
    /// spawning, which the projection filters out.
    abstract ticksToLive: int
    /// Fatigue points outstanding; the creep cannot move while > 0.
    abstract fatigue: int
    abstract store: IStore
    abstract pos: IRoomPosition
    abstract body: IBodyPartDef[]
    abstract harvest: target: obj -> int
    abstract transfer: target: obj * resource: string -> int
    abstract withdraw: target: obj * resource: string -> int
    abstract build: target: obj -> int
    abstract repair: target: obj -> int
    abstract upgradeController: target: obj -> int
    abstract pickup: target: obj -> int
    /// Single-step move by direction constant (TOP = 1, clockwise). The
    /// only movement API the bot uses — moveTo is forbidden (ADR 0001).
    abstract move: direction: int -> int
    /// Chat bubble above the creep. The omitted `public` argument defaults
    /// to false: bubbles stay private to our own viewer.
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
    /// Hash of room name -> room (visible rooms only).
    abstract rooms: obj
    /// Hash of spawn name -> spawn.
    abstract spawns: obj
    /// Hash of creep name -> creep.
    abstract creeps: obj
    /// Null when no object with that id exists (or it is out of sight).
    abstract getObjectById: id: string -> obj

[<Global("Game")>]
let Game: IGame = jsNative

[<Global("Memory")>]
let Memory: obj = jsNative

[<Emit("Object.values($0)")>]
let objectValues<'T> (_o: obj) : 'T[] = jsNative

[<Emit("Object.entries($0)")>]
let objectEntries (_o: obj) : (string * obj)[] = jsNative

/// One value out of a JS hash by key, or null when the hash holds no such
/// entry. Read against `Game.rooms`, which is keyed by room name and holds
/// only the rooms we have vision in this tick — so a null here is exactly
/// "no vision", which the projection expresses as absence entry by entry
/// (ADR 0004) rather than as a state of its own.
[<Emit("$0[$1]")>]
let objectItem<'T> (_o: obj) (_key: string) : 'T = jsNative
