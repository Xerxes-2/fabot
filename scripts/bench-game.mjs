// `Game` stood from one captured tick for `scripts/bench.mjs`, in the cost
// shape of the official engine (@screeps/engine src/game), which live runs:
// objects built before the clock starts, as the runtime builds them before
// `loop`; fields read through per-class prototype getters that memoise a
// truthy value; packed positions; a fresh Store; `find` a copy; Memory parsed
// to a null prototype; 0.2 an accepted intent, once per object and verb,
// `say` free.

import { existsSync, readFileSync, readdirSync } from "node:fs";
import Module, { createRequire } from "node:module";

// The engine's binary module `fabot_bg` (`scripts/rust.mjs`), for every bundle this process loads.
const nodeLoad = Module._load;
Module._load = function (request, parent, ...rest) {
  return request === "fabot_bg" ? readFileSync(parent.filename.replace(/\.js$/, ".wasm")) : nodeLoad.call(this, request, parent, ...rest);
};

const VERBS = ["move", "say", "harvest", "transfer", "withdraw", "pickup", "build", "repair", "dismantle", "upgradeController", "reserveController",
  "attackController", "claimController", "signController", "claimReactor", "drop", "attack", "heal", "rangedHeal", "rangedAttack", "rangedMassAttack",
  "spawnCreep", "transferEnergy", "setPublic", "destroy", "remove", "observeRoom", "runReaction", "send", "activateSafeMode"];
const FREE = new Set(["say"]);

let tick;
let data;
let intents;
const idOf = (a) => (a !== null && typeof a === "object" && !Array.isArray(a) ? (a.id ?? a.name) : a);

const roomNames = [];
const toXY = (name) => {
  const v = name.search(/[NS]/);
  const x = Number(name.slice(1, v));
  const y = Number(name.slice(v + 1));
  return [(name[0] === "W" ? -x - 1 : x) + 128, (name[v] === "N" ? -y - 1 : y) + 128];
};
function RoomPosition(x, y, roomName) {
  const [rx, ry] = toXY(roomName);
  roomNames[(rx << 8) | ry] ??= roomName;
  Object.defineProperty(this, "__packedPos", { enumerable: false, value: (rx << 24) | (ry << 16) | (x << 8) | y, writable: true });
}
Object.defineProperties(RoomPosition.prototype, {
  x: { enumerable: true, get() { return (this.__packedPos >> 8) & 0xff; } },
  y: { enumerable: true, get() { return this.__packedPos & 0xff; } },
  roomName: { enumerable: true, get() { return roomNames[this.__packedPos >>> 16]; } },
});

function Store(object) {
  const held = object.store.held;
  Object.entries(held).forEach(([resource, amount]) => {
    if (amount) this[resource] = amount;
  });
  Object.defineProperties(this, {
    getCapacity: { value: () => object.store.capacity },
    getUsedCapacity: {
      value: (r) => {
        if (r !== undefined) return held[r] ?? 0;
        if (this._sum === undefined) Object.defineProperty(this, "_sum", { value: Object.values(held).reduce((s, n) => s + n, 0) });
        return this._sum;
      },
    },
    getFreeCapacity: { value: (r) => object.store.capacity - this.getUsedCapacity(r) },
  });
}

const special = {
  owner: (o) => (o.owner ? new Object({ username: o.owner.username }) : undefined),
  reservation: (o) => (o.reservation ? { username: o.reservation.username, ticksToEnd: o.reservation.ticksToEnd } : undefined),
  spawning: (o) => (o.spawning && typeof o.spawning === "object" ? { name: o.spawning.name, remainingTime: o.spawning.remainingTime } : o.spawning),
  store: (o) => (o.store ? new Store(o) : undefined),
  creep: (o) => (o.creep ? { ...o.creep, store: new Store(o.creep) } : undefined),
};
// The kinds the engine gives an `energy` and `energyCapacity` read off the data, not the Store.
const ENERGY_ONLY = new Set(["s:spawn", "s:extension", "s:tower", "s:link"]);
const energyOnly = {
  energy: (o) => (o.store ? (o.store.held.energy ?? 0) : 0),
  energyCapacity: (o) => o.store?.capacity ?? 0,
};
function getters(proto, keys, readers = special) {
  for (const k of keys) {
    const read = readers[k] ?? ((o) => o[k]);
    const memo = `_${k}`;
    Object.defineProperty(proto, k, {
      configurable: true,
      enumerable: true,
      get() {
        if (!this[memo]) this[memo] = read(data[this.id]);
        return this[memo];
      },
    });
  }
}

// The checks the engine reads before it accepts a creep's intent. A refusal
// goes uncharged but still returns OK and is still recorded: the old
// harness's fingerprints, and a capture holds no refusal codes to replay.
const PART = { move: "move", harvest: "work", build: "work", repair: "work", dismantle: "work", upgradeController: "work", attack: "attack",
  rangedAttack: "ranged_attack", rangedMassAttack: "ranged_attack", heal: "heal", rangedHeal: "heal", claimController: "claim",
  reserveController: "claim", attackController: "claim" };
function accepts(creep, verb, target, resource) {
  if (!creep.my || creep.spawning || (verb === "move" && creep.fatigue > 0)) return false;
  if (PART[verb] && !creep.body.some((p) => p.type === PART[verb] && p.hits > 0)) return false;
  const far = (a, b) => a.roomName !== b.roomName || Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) > 3;
  if (target?.pos && far(target.pos, creep.pos)) return false;
  return typeof resource !== "string" || creep.store.getUsedCapacity(resource) + (target.store?.getFreeCapacity(resource) ?? 0) > 0;
}

const RoomObject = {};
for (const verb of VERBS)
  RoomObject[verb] = function (...args) {
    const accepted = this.body === undefined || accepts(this, verb, args[0], args[1]);
    const actor = this.id ?? this.name;
    if (accepted && !FREE.has(verb)) intents.charged.add(`${actor} ${verb}`);
    intents.list.push([actor, verb, ...args.map(idOf)]);
    return 0;
  };
const Structure = Object.create(RoomObject);
getters(Structure, ["hits", "hitsMax", "structureType"]);
const OwnedStructure = Object.create(Structure);
getters(OwnedStructure, ["owner", "my"]);
// A class per kind (a find type, or a structure type), its fields the ones
// its records carry.
const classes = new Map();
function classOf(kind, s) {
  let proto = classes.get(kind);
  if (proto === undefined) {
    const base = kind.startsWith("s:") ? ("owner" in s || "my" in s ? OwnedStructure : Structure) : RoomObject;
    proto = Object.create(base);
    classes.set(kind, proto);
  }
  getters(proto, Object.keys(s).filter((k) => k !== "id" && k !== "pos" && !(k in proto)));
  if (ENERGY_ONLY.has(kind) && !("energy" in proto)) getters(proto, ["energy", "energyCapacity"], energyOnly);
  return proto;
}
const KIND = { 105: "source", 116: "mineral", 111: "site", 106: "resource", 118: "tombstone", 123: "ruin", 103: "creep" };

function Room(r, objects) {
  Object.defineProperty(this, "name", { value: r.name, enumerable: true });
  this.energyAvailable = r.energyAvailable;
  this.energyCapacityAvailable = r.energyCapacityAvailable;
  this.controller = r.controller === undefined ? undefined : objects[r.controller];
  this._events = r.events ?? [];
  this._found = Object.fromEntries(Object.entries(r.find).map(([type, ids]) => [type, ids.map((id) => objects[id])]));
}
// Only the attacks and the destructions were captured: what `read.js` keeps.
globalThis.EVENT_ATTACK ??= 1;
globalThis.EVENT_OBJECT_DESTROYED ??= 2;
// Only the metrics price a lost structure, and no decision reads them.
globalThis.CONSTRUCTION_COST ??= {};
// The engine's, for a capture whose controllers lack ticksToDowngrade.
globalThis.CONTROLLER_DOWNGRADE ??= { 1: 20000, 2: 10000, 3: 20000, 4: 40000, 5: 80000, 6: 120000, 7: 150000, 8: 200000 };
// The vanilla engine's, for a capture from before captures carried the table.
const from = (level, n) => Array.from({ length: 9 }, (_, l) => (l >= level ? n : 0));
const VANILLA = {
  spawn: [0, 1, 1, 1, 1, 1, 1, 2, 3],
  extension: [0, 0, 5, 10, 20, 30, 40, 50, 60],
  link: [0, 0, 0, 0, 0, 2, 3, 4, 6],
  road: from(0, 2500),
  constructedWall: from(2, 2500),
  rampart: from(2, 2500),
  storage: from(4, 1),
  tower: [0, 0, 0, 1, 1, 2, 2, 3, 6],
  observer: from(8, 1),
  powerSpawn: from(8, 1),
  extractor: from(6, 1),
  terminal: from(6, 1),
  lab: [0, 0, 0, 0, 0, 0, 3, 6, 10],
  container: from(0, 5),
  nuker: from(8, 1),
  factory: from(7, 1),
};
globalThis.CONTROLLER_STRUCTURES ??= VANILLA;
Room.prototype.getEventLog = function () {
  return this._events;
};
Room.prototype.find = function (type) {
  const found = this._found[type] ?? [];
  const copy = new Array(found.length);
  for (let i = 0; i < found.length; i++) copy[i] = found[i];
  return copy;
};
Room.prototype.createConstructionSite = function (...args) {
  intents.charged.add(`${this.name} site ${intents.list.length}`);
  intents.list.push([this.name, "createConstructionSite", ...args]);
  return 0;
};

// Out of sight, the terrain `observe capture` cached, read before any clock
// runs; a room it lacks throws, as one off the map does live.
const cache = new URL("../captures/terrain/", import.meta.url);
const cached = new Map(
  (existsSync(cache) ? readdirSync(cache) : []).map((name) => [name, Uint8Array.from(readFileSync(new URL(name, cache), "utf8").trim(), Number)]),
);
function Terrain(name) {
  const array = tick.rooms.find((r) => r.name === name)?.terrain ?? cached.get(name);
  if (array === undefined) throw new Error(`no terrain for ${name}`);
  this.get = (x, y) => array[y * 50 + x];
  this.getRawBuffer = (into = new Uint8Array(2500)) => (into.set(array), into);
}

const require = createRequire(import.meta.url);
// Each bundle installs its own world reader on the global; a tick runs on its own.
export const load = (path) => {
  delete require.cache[path];
  const bot = require(path);
  return { loop: () => bot.loop(), read: globalThis.__fabotRead };
};

// Stands `captured` as globalThis's Game, Memory and RawMemory, the intents
// it accepts pushed onto `list`. The clock is the caller's.
export function stand(captured, list, getHeapStatistics, clock) {
  tick = captured;
  globalThis.CONTROLLER_STRUCTURES = tick.controllerStructures ?? VANILLA;
  intents = { list, charged: new Set() };
  data = {};
  const objects = {};
  const kindOf = {};
  for (const r of tick.rooms) for (const [type, ids] of Object.entries(r.find)) for (const id of ids) kindOf[id] = KIND[type];
  for (const id of Object.values(tick.creeps)) kindOf[id] = "creep";
  for (const [key, s] of Object.entries(tick.objects)) {
    data[key] = s;
    const kind = s.structureType && kindOf[key] !== "site" ? `s:${s.structureType}` : (kindOf[key] ?? "other");
    const o = Object.create(classOf(kind, s));
    o.id = key;
    o.pos = new RoomPosition(s.pos.x, s.pos.y, s.pos.roomName);
    objects[key] = o;
    // The runtime's find cache reads these before `loop`.
    if ("my" in o && o.my === false) o.owner;
    if (kind === "source") o.energy;
  }
  const named = (ids) => Object.fromEntries(Object.entries(ids).map(([name, key]) => [name, objects[key]]));
  const rooms = Object.fromEntries(tick.rooms.map((r) => [r.name, new Room(r, objects)]));
  // The intel rows as the capture's tick read them, for a window's cold start.
  const segments = tick.intel === undefined ? {} : { 2: tick.intel };
  let parsed;
  Object.defineProperty(globalThis, "Memory", {
    configurable: true,
    get() {
      parsed = JSON.parse(tick.memory || "{}");
      parsed.__proto__ = null;
      Object.defineProperty(globalThis, "Memory", { configurable: true, value: parsed });
      return parsed;
    },
  });
  // As the engine flushes: a `set` drops what `Memory` parsed.
  let written;
  const set = (text) => ((written = text), (parsed = undefined));
  globalThis.RawMemory = { segments, get: () => written ?? tick.memory, set, setActiveSegments: () => {} };
  globalThis.Game = {
    time: tick.time,
    cpu: { ...tick.cpu, getUsed: () => clock() + 0.2 * intents.charged.size, getHeapStatistics },
    gcl: tick.gcl ?? { level: 0 },
    map: { getRoomTerrain: (name) => new Terrain(name) },
    rooms,
    creeps: named(tick.creeps),
    spawns: named(tick.spawns),
    constructionSites: Object.fromEntries(tick.constructionSites.map((key) => [key, objects[key]])),
    getObjectById: (id) => objects[id] || null,
  };
  return { segments, memory: () => (parsed === undefined ? (written ?? tick.memory) : JSON.stringify(parsed)) };
}
