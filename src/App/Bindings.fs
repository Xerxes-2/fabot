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

/// Screeps `FIND_HOSTILE_CONSTRUCTION_SITES` constant: every construction site
/// in the room a user who is not us placed — "hostile" being the engine's word
/// for `my === false` and this bot having no ally vocabulary. Swept beside the
/// sweep above rather than partitioning one `FIND_CONSTRUCTION_SITES` pass
/// (#248), so each array is already the side it belongs to: ours are the
/// projection's id-keyed sites and the colony's Build pool, and these are tiles
/// alone. The engine takes one site per tile whoever owns it, which is the
/// whole of why they are read.
let findHostileConstructionSites = 115

/// Screeps `FIND_HOSTILE_CREEPS` constant.
let findHostileCreeps = 103

/// Screeps `FIND_HOSTILE_STRUCTURES` constant: every structure in the room a
/// user who is not us owns — the NPC Invader included, which is what an invader
/// core belongs to. The only sweep that can *name* a core: a core is a
/// structure, so `FIND_HOSTILE_CREEPS` has never answered with one, and the
/// projection's own `FIND_STRUCTURES` pass sees it as `BuiltKind.Other` (ADR
/// 0043).
let findHostileStructures = 109

/// Screeps `FIND_DROPPED_RESOURCES` constant.
let findDroppedResources = 106

/// Screeps `FIND_MINERALS` constant: the mineral deposits standing in the room
/// (ADR 0057 decision 1). The season mod puts a Thorium deposit in most rooms
/// beside the ordinary ore, and only the Thorium one is projected — the filter
/// is on `mineralType` in `World`, where every other engine string is
/// classified.
let findMinerals = 116

/// Screeps `FIND_TOMBSTONES` constant: what a creep leaves behind when it
/// dies, holding whatever it carried (#167).
let findTombstones = 118

/// Screeps `FIND_RUINS` constant: what a destroyed structure leaves behind,
/// holding whatever stood in it. Projected as the same kind a tombstone is
/// (`TargetKind.Tombstone`) — one store with a clock on it — because that is
/// the whole of what a decision reads off either.
let findRuins = 123

/// Screeps `TERRAIN_MASK_WALL` constant.
let terrainMaskWall = 1

/// Screeps `TERRAIN_MASK_SWAMP` constant.
let terrainMaskSwamp = 2

// The STRUCTURE_* spellings live in Core (`builtKindName`, #75): the kind
// predicates over them are Core rules, so the table has to be readable there.
// One exception, and the sentence above is the reason for it: an invader core
// has no kind predicate in Core at all. Nothing repairs it, refills it, stores
// in it or is charged damage on it, and Core's answer to every one of those is
// already `BuiltKind.Other`'s. What Core reads off a core is a threat fact
// under its own name (`InvaderCoreInfo`, ADR 0043) and never a built kind, so
// admitting it to the modelled vocabulary would add eight predicate arms nobody
// asks — and buy the census nothing, a core already signing as
// `Other@room:x,y`.
/// Screeps `STRUCTURE_INVADER_CORE` constant.
let structureInvaderCore = "invaderCore"

/// The NPC Invader's username, as the engine spells it on every object that
/// user holds — an invader core, a raider, and the reservation a level-0 core
/// takes with `attackController` (ADR 0043).
let invaderUsername = "Invader"

/// Screeps `EFFECT_COLLAPSE_TIMER` constant. The effect an NPC stronghold's
/// structures carry once deployed; when it runs out the engine removes the
/// stronghold, and with it that sector's invasion switch (ADR 0043).
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
    /// Effect id — a natural effect (EFFECT_*) or a Power id. The `level`
    /// beside it in the engine is a Power effect's alone and no rule here
    /// reads one, so it is not bound.
    abstract effect: int
    /// How many ticks the effect will last: a count **relative** to now, which
    /// is the engine runtime's shape and not the read-only HTTP API's. That
    /// API's raw documents carry an absolute `endTime` instead, and
    /// `docs/research/remote-mining.md` is written in its vocabulary — so an
    /// expiry read off this has the current tick added to it (ADR 0043, #133).
    abstract ticksRemaining: int

/// A mineral deposit standing in a room (ADR 0057 decision 1). Read only for
/// the season's Thorium: `mineralType` is the filter, and `mineralAmount` is
/// what the projection carries as the deposit's own remaining Thorium. Density
/// and `ticksToRegeneration` are not bound — a Thorium deposit never
/// regenerates, and nothing decides on the density.
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
    /// Ticks before this structure may act again. Defined on the extractor
    /// alone among the kinds we build (`EXTRACTOR_COOLDOWN` is 5), and read
    /// only there — the shell classifies the kind first, so no other structure
    /// is ever asked (ADR 0057 decision 2).
    abstract cooldown: int

/// A dropped resource pile lying on the ground.
type IResource =
    abstract id: string
    /// Screeps RESOURCE_* string, e.g. "energy".
    abstract resourceType: string
    abstract pos: IRoomPosition
    /// How much of that resource the pile holds — the field the Pickup Task's
    /// threshold and its capacity are both read off (#167). A pile is a bare
    /// amount and not a store, which is why this is a number here and a
    /// `getUsedCapacity` call on everything else.
    abstract amount: int

/// A tombstone or a ruin: the two engine objects that are a store with a clock
/// on it. One binding for both (#167), because the three fields the projection
/// reads are the same three and Core models the pair as one kind. What draws
/// from them is the ordinary `creep.withdraw`, which takes any store.
type ITombstone =
    abstract id: string
    abstract pos: IRoomPosition
    abstract store: IStore

type IConstructionSite =
    abstract id: string
    /// Screeps STRUCTURE_* string of what is being built.
    abstract structureType: string
    abstract pos: IRoomPosition

/// The `owner` sub-object every owned game object carries.
type IOwner =
    abstract username: string

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
    /// Whose controller this is; undefined on an unowned one. Read off every
    /// room the colony can see, and twice for two questions: once off the room
    /// its spawns stand in, for the one name a reservation and a hostile are
    /// compared against (ADR 0042), and once per seen room for the third
    /// answer `Ownership` carries — the clockless half of ADR 0043.
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
    /// Hits left in this one part. The engine destroys parts from the head of
    /// the body, and a destroyed part stays in the array reading zero — so a
    /// count that does not read this counts weapons the creep no longer has
    /// (#270).
    abstract hits: int

type ICreep =
    abstract id: string
    abstract name: string
    /// The room the creep is standing in this tick. Read to keep the
    /// projection's creep table inside the room it is filed under (ADR 0041):
    /// `Game.creeps` is world-wide and the projection is one room's.
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
    /// Push a neutral controller's reservation up by one tick per CLAIM
    /// part (ADR 0042). Range 1, and refused on a controller anybody owns
    /// — including ours, which is Upgraded instead.
    abstract reserveController: target: obj -> int
    /// Take a neutral controller's room for this player (ADR 0047). Range
    /// 1, one CLAIM part, and refused — ERR_GCL_NOT_ENOUGH — while every
    /// GCL level this account has is already spent on a room.
    abstract claimController: target: obj -> int
    abstract pickup: target: obj -> int
    /// Hit a creep at range 1 for `ATTACK_POWER` per ATTACK part (ADR 0056).
    /// The [[guard]]'s act, and the only one this colony aims at a body it
    /// does not own.
    abstract attack: target: obj -> int
    /// Restore `HEAL_POWER` per HEAL part to a creep of ours at range 1,
    /// itself included (ADR 0056). A different act from `attack` in the
    /// engine, so a body carrying both parts does both in one tick.
    abstract heal: target: obj -> int
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
/// entry. Read against `Game.rooms`, which holds only the rooms we have vision
/// in this tick — so a null here is exactly "no vision", which the projection
/// expresses as absence entry by entry (ADR 0004).
[<Emit("$0[$1]")>]
let objectItem<'T> (_o: obj) (_key: string) : 'T = jsNative
