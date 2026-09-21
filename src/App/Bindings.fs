// Minimal hand-written bindings: only the API surface the bot uses.
module Fabot.Bindings

open Fable.Core

/// `Game.cpu.getHeapStatistics()`: the V8 heap of our isolate, in bytes.
type IHeapStatistics =
    abstract used_heap_size: float
    /// Memory "not included in the v8 heap but counts against this isolate's
    /// memory limit"; the docs name ArrayBuffers "over a certain size", so a
    /// 10 KB flood field lands here. Other external accounting rides too.
    abstract externally_allocated_size: float

type ICpu =
    abstract getUsed: unit -> float
    abstract getHeapStatistics: unit -> IHeapStatistics
    /// The CPU the engine has banked for us. The tick ceiling is 500 ms for
    /// the whole life of a non-empty bucket, not `min(limit + bucket, 500)`:
    /// the engine's `tickLimit` "equals 500" and "will start decreasing only
    /// after the accumulation is depleted".
    abstract bucket: int

/// Screeps `FIND_SOURCES` constant.
let findSources = 105

/// Screeps `FIND_MY_STRUCTURES` constant.
let findMyStructures = 108

/// Screeps `FIND_STRUCTURES` constant (any owner, includes neutral).
let findStructures = 107

/// Screeps `FIND_MY_CONSTRUCTION_SITES` constant.
let findMyConstructionSites = 114

/// Screeps `FIND_HOSTILE_CONSTRUCTION_SITES` constant: every site a user who
/// is not us placed (`my === false`). Read because the engine takes one site
/// per tile whoever owns it.
let findHostileConstructionSites = 115

/// Screeps `FIND_HOSTILE_CREEPS` constant.
let findHostileCreeps = 103

/// Screeps `FIND_HOSTILE_STRUCTURES` constant: every structure a user who is
/// not us owns, the NPC Invader included. The only sweep that can name an
/// invader core: `FIND_STRUCTURES` sees it as `BuiltKind.Other`.
let findHostileStructures = 109

/// Screeps `FIND_DROPPED_RESOURCES` constant.
let findDroppedResources = 106

/// Screeps `FIND_MINERALS` constant. The season mod puts a Thorium deposit in
/// most rooms beside the ordinary ore; `World` filters on `mineralType`.
let findMinerals = 116

/// The season mod's `FIND_REACTORS` constant (`reactor.roomObject.js`, mod
/// 1.0.3). The only sweep that can answer with a reactor: it is registered
/// through `registerCustomObjectPrototype`, reaches no `FIND_STRUCTURES` pass
/// and carries no `structureType`. The number, not the runtime global.
let findReactors = 10051

/// Screeps `FIND_TOMBSTONES` constant.
let findTombstones = 118

/// Screeps `FIND_RUINS` constant. Projected as `TargetKind.Tombstone`: one
/// store with a clock on it.
let findRuins = 123

/// Screeps `TERRAIN_MASK_WALL` constant.
let terrainMaskWall = 1

/// Screeps `TERRAIN_MASK_SWAMP` constant.
let terrainMaskSwamp = 2

// The STRUCTURE_* spellings live in Core (`builtKindName`); the one exception
// is the invader core, which Core reads as a threat fact (`InvaderCoreInfo`)
// and never as a built kind.
/// Screeps `STRUCTURE_INVADER_CORE` constant.
let structureInvaderCore = "invaderCore"

/// The NPC Invader's username, as the engine spells it on every object that
/// user holds: a core, a raider, and the reservation a level-0 core takes.
let invaderUsername = "Invader"

/// Screeps `EFFECT_COLLAPSE_TIMER` constant: the effect a deployed stronghold's
/// structures carry; when it runs out the engine removes the stronghold.
let effectCollapseTimer = 1002

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

/// One entry of `RoomObject.effects`: an effect standing on a game object.
type IEffect =
    /// Effect id: a natural effect (EFFECT_*) or a Power id.
    abstract effect: int
    /// Ticks the effect will last, relative to now (the HTTP API's raw
    /// documents carry an absolute `endTime` instead, and
    /// `docs/research/remote-mining.md` speaks in that vocabulary).
    abstract ticksRemaining: int

/// A mineral deposit standing in a room. Density and `ticksToRegeneration`
/// are not bound: a Thorium deposit never regenerates.
type IMineral =
    abstract id: string
    abstract pos: IRoomPosition
    /// Screeps RESOURCE_* string; "T" for the season's Thorium.
    abstract mineralType: string
    /// How much is left in the deposit. The mod deletes a Thorium deposit
    /// outright the tick this hits zero, so it is never read as 0 here.
    abstract mineralAmount: int

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
    /// The effects standing on this structure; undefined when none does,
    /// the shape `safeMode` and `reservation` also arrive in.
    abstract effects: IEffect[]
    /// An invader core's level, `undefined` on every other structure: 0 is
    /// the expansion core a stronghold plants next door, 1 and up the bunker.
    abstract level: int
    /// Ticks before this structure may act again. Among the kinds we build,
    /// defined on the extractor alone (`EXTRACTOR_COOLDOWN` is 5).
    abstract cooldown: int

/// A dropped resource pile lying on the ground.
type IResource =
    abstract id: string
    /// Screeps RESOURCE_* string, e.g. "energy".
    abstract resourceType: string
    abstract pos: IRoomPosition
    /// How much the pile holds: a bare amount, not a store.
    abstract amount: int

/// A tombstone or a ruin: one binding, since the projection reads the same
/// three fields off both and `creep.withdraw` takes any store.
type ITombstone =
    abstract id: string
    abstract pos: IRoomPosition
    abstract store: IStore

type IConstructionSite =
    abstract id: string
    /// Screeps STRUCTURE_* string of what is being built.
    abstract structureType: string
    abstract pos: IRoomPosition
    /// The energy already built into it, and what it needs in all.
    abstract progress: int
    abstract progressTotal: int

/// The `owner` sub-object every owned game object carries.
type IOwner =
    abstract username: string

/// The line somebody has written onto a controller. Undefined until one is,
/// and it outlives every body in the room.
type ISign =
    /// The text itself, at most a hundred characters.
    abstract text: string

/// The reservation standing on a neutral controller: who holds it, and
/// how long it has left. Undefined on a controller nothing reserves, and
/// on every owned one — a reservation and an owner are exclusive.
type IReservation =
    /// The username holding it.
    abstract username: string
    /// Ticks left before it lapses; decays by one a tick, caps at 5,000.
    abstract ticksToEnd: int

type IController =
    abstract id: string
    /// True when this controller is owned by us; undefined on a
    /// controller nobody owns, the shape `safeMode` also arrives in.
    abstract my: bool
    /// Whose controller this is; undefined on an unowned one.
    abstract owner: IOwner
    /// The reservation standing on this controller; undefined when none
    /// does.
    abstract reservation: IReservation
    /// Controller level (RCL).
    abstract level: int
    /// Ticks left on the downgrade timer; undefined on unowned controllers.
    abstract ticksToDowngrade: int
    /// Safe-mode activations banked.
    abstract safeModeAvailable: int
    /// The sign standing on this controller, `undefined` until somebody
    /// writes one.
    abstract sign: ISign
    /// Ticks of safe mode remaining; undefined when safe mode is off.
    abstract safeMode: int
    abstract pos: IRoomPosition
    abstract activateSafeMode: unit -> int

/// The sector Reactor (`mod-season5/src/reactor.roomObject.js`): one per
/// sector centre, indestructible (no `hits` at all) and walkable. `my` is the
/// mod's own accessor, `o.user ? o.user == runtimeData.user._id : undefined`,
/// so it is undefined on one nobody owns, as a controller's is. The tile is
/// not read here; it is the declaration's (`Errand.place`).
type IReactor =
    abstract id: string
    /// True when this reactor is ours; undefined on one nobody owns.
    abstract my: bool
    /// Whose it is; undefined on an unowned one.
    abstract owner: IOwner
    abstract store: IStore
    /// Ticks consumed without the store running dry; zero while idle.
    abstract continuousWork: int

type ITower =
    abstract attack: target: obj -> int

/// `send` moves a resource to another room's terminal, paying a fee out of
/// this terminal's energy; `mod-season5/src/terminal-restriction.js` nulls it
/// only when the destination belongs to another user. The optional
/// `description` argument is omitted rather than passed as null.
type ITerminal =
    abstract send: resourceType: string * amount: int * destination: string -> int

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
    /// Hits left in this one part. The engine destroys parts from the head of
    /// the body, and a destroyed part stays in the array reading zero — so a
    /// count that does not read this counts weapons the creep no longer has
    /// (#270).
    abstract hits: int

type ICreep =
    abstract id: string
    abstract name: string
    /// The room the creep is standing in this tick (`Game.creeps` is
    /// world-wide; the projection is one room's).
    abstract room: IRoom
    /// Whose creep this is; read only off hostiles.
    abstract owner: IOwner
    /// True while the creep is still being built inside the spawn.
    abstract spawning: bool
    /// Ticks the creep has left to live; undefined only while it is still
    /// spawning, which the projection filters out.
    abstract ticksToLive: int
    /// Fatigue points outstanding; the creep cannot move while > 0.
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
    abstract upgradeController: target: obj -> int
    /// Push a neutral controller's reservation up by one tick per CLAIM part.
    /// Range 1; refused on a controller anybody owns.
    abstract reserveController: target: obj -> int
    /// Take a neutral controller's room. Range 1, one CLAIM part, and
    /// ERR_GCL_NOT_ENOUGH while every GCL level is already spent.
    abstract claimController: target: obj -> int
    /// Write a line onto a controller at range 1: no body part, a hundred
    /// characters, lasts until overwritten.
    abstract signController: target: obj * text: string -> int
    /// Take the sector Reactor: the season mod's own intent. Range 1, one live
    /// CLAIM part, no cooldown and no ownership precondition, and it leaves
    /// `launchTime` alone.
    abstract claimReactor: target: obj -> int
    abstract pickup: target: obj -> int
    /// Hit a creep at range 1 for `ATTACK_POWER` per ATTACK part.
    abstract attack: target: obj -> int
    /// Restore `HEAL_POWER` per HEAL part to a creep of ours at range 1,
    /// itself included. A different act from `attack` in the engine, so a body
    /// carrying both parts does both in one tick.
    abstract heal: target: obj -> int
    /// Single-step move by direction constant (TOP = 1, clockwise). The only
    /// movement API the bot uses; moveTo is not bound.
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
/// entry. Against `Game.rooms` a null is exactly "no vision".
[<Emit("$0[$1]")>]
let objectItem<'T> (_o: obj) (_key: string) : 'T = jsNative

/// `creep.withdraw` with the engine's optional third argument, the amount. A
/// stub of its own because F# has no optional argument on an abstract member,
/// and the two-argument call is not the same call as one passing `undefined`.
[<Emit("$0.withdraw($1, $2, $3)")>]
let withdrawAmount (_creep: ICreep) (_target: obj) (_resource: string) (_amount: int) : int =
    jsNative
