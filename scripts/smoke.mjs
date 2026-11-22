// Run the built bundle for a few ticks against a stub game: the cheapest check
// that the bundle loads, reads, writes and does not throw. Not a test of
// behaviour; that is xxscreeps' (ARCHITECTURE.md §10).
//
// Usage: node scripts/smoke.mjs [bundle]   (after `npm run build`)
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { getHeapStatistics } from "node:v8";
import { createContext, runInContext } from "node:vm";
import { stand } from "./bench-game.mjs";
import { RECORDER, unpack } from "./recorder.mjs";

const fail = (msg) => {
  console.error(`smoke: ${msg}`);
  process.exit(1);
};

const segments = {};
const active = [];
let used = 0;
const at = (x, y) => ({ x, y, roomName: "W1N1" });
const store = (held, capacity) => Object.defineProperty({ ...held }, "getCapacity", { value: (r) => (r === undefined || r === "energy" ? capacity : null) });
const spawn = {
  id: "a1",
  name: "Spawn1",
  structureType: "spawn",
  my: true,
  owner: { username: "me" },
  pos: at(25, 25),
  hits: 5000,
  hitsMax: 5000,
  store: store({ energy: 300 }, 300),
  spawning: null,
  spawnCreep: () => 0,
};
const road = { id: "a2", structureType: "road", pos: at(26, 25), hits: 5000, hitsMax: 5000 };
const creep = {
  id: "a3",
  name: "a",
  my: true,
  owner: { username: "me" },
  spawning: false,
  pos: at(10, 10),
  body: [
    { type: "work", hits: 100 },
    { type: "carry", hits: 100 },
    { type: "move", hits: 100, boost: "ZO" },
  ],
  hits: 300,
  hitsMax: 300,
  ticksToLive: 1400,
  fatigue: 0,
  store: store({ energy: 20 }, 50),
  say: () => 0,
  move: () => 0,
  harvest: () => 0,
  transfer: () => 0,
  withdraw: () => 0,
  pickup: () => 0,
  build: () => 0,
  repair: () => 0,
  upgradeController: () => 0,
};
const found = {
  105: [{ id: "a4", pos: at(5, 5), energy: 3000, energyCapacity: 3000, ticksToRegeneration: undefined }],
  116: [{ id: "a5", pos: at(40, 40), mineralType: "H", mineralAmount: 70000 }],
  107: [spawn, road],
  111: [{ id: "a6", structureType: "extension", my: true, owner: { username: "me" }, pos: at(27, 27), progress: 0, progressTotal: 3000 }],
  106: [{ id: "a7", resourceType: "energy", pos: at(11, 11), amount: 50 }],
  118: [true, false].map((my, i) => ({ id: `b${i}`, pos: at(12, 12 + i), store: store({}, 0), deathTime: 99, creep: { id: `e${i}`, my, pos: at(12, 12 + i), body: [{ type: "carry", hits: 0 }], ticksToLive: 900 } })),
  123: [],
  103: [],
};
const room = {
  name: "W1N1",
  energyAvailable: 300,
  energyCapacityAvailable: 300,
  controller: { id: "c1", my: true, owner: { username: "me" }, pos: at(30, 30), level: 1, progress: 10, progressTotal: 200, ticksToDowngrade: 20000, safeModeAvailable: 1 },
  find: (type) => found[type] ?? [],
  getEventLog: () => "[]",
  createConstructionSite: () => 0,
};
spawn.room = room;
creep.room = room;
const terrain = new Uint8Array(2500);
terrain[0] = 1;
terrain[51] = 2;

let memory = JSON.stringify({ fabot: { v1: "left alone" }, creeps: { dead: {} } });
// The engine parses and stringifies `Memory` on a tick that touches it.
Object.defineProperty(globalThis, "Memory", { configurable: true, get: () => fail("the bot touched Memory") });
globalThis.RawMemory = { segments, get: () => memory, set: (text) => (memory = text), setActiveSegments: (ids) => active.push(ids) };
globalThis.Game = {
  time: 100,
  cpu: { limit: 20, bucket: 10000, getUsed: () => (used += 0.25) },
  rooms: { W1N1: room },
  map: { getRoomTerrain: () => ({ getRawBuffer: (into = new Uint8Array(2500)) => (into.set(terrain), into) }) },
  spawns: { Spawn1: spawn },
  creeps: { a: creep },
  gcl: { level: 1 },
  constructionSites: { a6: found[111][0] },
  getObjectById: (id) => ({ a1: spawn, a4: found[105][0], c1: room.controller, a6: found[111][0], a7: found[106][0] })[id] ?? null,
};

// The sandbox's text polyfill, run where no native one exists, matches Node's byte for byte.
const sandbox = createContext({});
runInContext(readFileSync(new URL("../src/app/text.js", import.meta.url), "utf8"), sandbox);
const { TextEncoder: Encoder, TextDecoder: Decoder } = sandbox;
const long = "x".repeat(20000) + "\u{1F600}" + "中".repeat(9000) + "é";
for (const text of ["", "ascii", "é", "中文", "a\u{1F600}b", "\ud800", "\udc00x", "\ud800a", long]) {
  const want = new TextEncoder().encode(text);
  const got = new Encoder().encode(text);
  if (got.join() !== want.join()) fail(`encode(${JSON.stringify(text.slice(0, 20))}) is ${got.slice(0, 20)}, not ${want.slice(0, 20)}`);
  if (new Decoder().decode(want) !== new TextDecoder().decode(want)) fail(`decode(${JSON.stringify(text.slice(0, 20))})`);
  for (const size of [0, 1, 2, 3, 4, 5, text.length * 3]) {
    const [mine, theirs] = [new Uint8Array(size), new Uint8Array(size)];
    const [a, b] = [new Encoder().encodeInto(text, mine), new TextEncoder().encodeInto(text, theirs)];
    if (a.read !== b.read || a.written !== b.written || mine.join() !== theirs.join()) fail(`encodeInto(${JSON.stringify(text.slice(0, 20))}, ${size})`);
  }
}
if (new Decoder().decode() !== "") fail("decode() is not empty");

const require = createRequire(import.meta.url);
const bundle = process.argv[2] ?? "../dist/main.js";
const main = require(bundle);
if (typeof main.loop !== "function") fail(`${bundle} exports no loop`);

for (let t = 0; t < 3; t++) {
  Game.time = 100 + t;
  main.loop();
}
memory = JSON.stringify({ v2: { ...JSON.parse(memory).v2, switches: { recorder: true } } });
const before = memory;
Game.time = 103;
main.loop();

const observe = JSON.parse(segments[1] ?? fail("no observe segment written"));
if (observe.tick !== 103) fail(`observe tick ${observe.tick}, not 103`);
if (observe.faults.length !== 0) fail(`faults: ${JSON.stringify(observe.faults)}`);
const phases = observe.phases.map(([name]) => name);
for (const want of ["memory", "read.rooms", "read.structures.find", "read.structures", "read.others", "read.events", "read.own", "read.pack", "read.copy", "decode", "map", "world", "atlas", "economy", "execute", "write", "metrics", "record"])
  if (!phases.includes(want)) fail(`phase ${want} not reported: ${phases}`);
if (!observe.counts.some(([name, n]) => name === "read.structures" && n > 0)) fail(`read.structures walked nothing: ${JSON.stringify(observe.counts)}`);
const pass = globalThis.__fabotRead().slice(0, 2).map((each) => (typeof each === "string" ? each : Array.from(each)));
const [recorded] = unpack(RECORDER.map((id) => segments[id]));
if (recorded?.time !== 103) fail(`recorder wrote no tick 103: ${recorded?.time}`);
if (!recorded.intel.startsWith("W1N1 103 ")) fail(`recorder's intel lost the room: ${recorded.intel}`);
if (recorded.memory !== before) fail("recorder's Memory is not the tick's");
if (!recorded.objects.b0.creep || recorded.objects.b1.creep) fail(`a tombstone's creep is read unless ours: ${JSON.stringify([recorded.objects.b0, recorded.objects.b1])}`);
const stored = JSON.parse(memory);
if (stored.fabot !== undefined) fail(`v1's Memory was kept: ${JSON.stringify(stored.fabot)}`);
if (stored.creeps !== undefined) fail(`a dead creep's Memory was kept: ${JSON.stringify(stored.creeps)}`);
const held = stored.v2.assignments?.a;
if (typeof held !== "string") fail(`the creep holds no task: ${JSON.stringify(stored.v2.assignments)}`);
if (observe.refusals.some((r) => r.code === -100)) fail(`an intent threw: ${JSON.stringify(observe.refusals)}`);

// A read too short for its decoder panics, a wasm trap; the tick after runs
// on a fresh instance.
const read = globalThis.__fabotRead;
const error = console.error;
globalThis.__fabotRead = () => [new Int32Array(0), ""];
console.error = () => {};
let trapped = false;
try {
  Game.time = 104;
  main.loop();
} catch (thrown) {
  trapped = thrown instanceof WebAssembly.RuntimeError;
}
globalThis.__fabotRead = read;
console.error = error;
if (!trapped) fail("a short read did not trap");
// A cold heap replaces the active list and still reads the intel already loaded.
segments[2] += "W9N9 50 - 0 - - - -\n";
segments[3] = "names stock\n@0 100\nW1N1 5\n";
active.length = 0;
Game.time = 105;
main.loop();
const after = JSON.parse(segments[1]);
if (after.tick !== 105 || after.faults.length !== 0) fail(`the tick after a trap: ${JSON.stringify(after).slice(0, 200)}`);
if (JSON.stringify(active) !== "[[2,3]]") fail(`a cold heap set the active segments to ${JSON.stringify(active)}`);
Game.time = 106;
main.loop();
if (!segments[2].includes("W9N9 50 ")) fail(`intel was not read after a reset: ${segments[2]}`);
// The metrics' ring outlives a reset; the window it was open on is lost.
Game.time = 200;
main.loop();
if (!/^@0 100\nW1N1 5$/m.test(segments[3]) || !/^@100 2$/m.test(segments[3])) fail(`the metrics' ring after a reset: ${segments[3]}`);

// A vm timeout terminates as the engine's CPU limit does, past any catch or finally.
globalThis.__fabotRead = () => {
  for (;;);
};
let aborted = false;
try {
  Game.time = 201;
  runInContext("loop()", createContext({ loop: main.loop }), { timeout: 50 });
} catch (thrown) {
  aborted = thrown.code === "ERR_SCRIPT_EXECUTION_TIMEOUT";
}
globalThis.__fabotRead = read;
if (!aborted) fail("a spinning read did not time out");
active.length = 0;
Game.time = 202;
main.loop();
if (JSON.stringify(active) !== "[[2,3]]") fail(`the tick after a timeout ran on a warm heap: ${JSON.stringify(active)}`);

// Refusals as the executor names them, the world changed between read and execute.
const exec = globalThis.__fabotExec;
const patch = (o, k, v) => {
  const was = o[k];
  o[k] = v;
  return () => (o[k] = was);
};
const refusals = (time, ...patches) => {
  globalThis.__fabotExec = (ints, text) => {
    const undo = patches.map(([o, k, v]) => patch(o, k, v));
    try {
      return exec(ints, text);
    } finally {
      undo.forEach((u) => u());
    }
  };
  Game.time = time;
  main.loop();
  globalThis.__fabotExec = exec;
  return JSON.stringify(JSON.parse(segments[1]).refusals.map(({ actor, act, code }) => [actor, act, code]));
};
const seen = Game.getObjectById;
let spawned;
const thrown = refusals(
  203,
  [spawn, "spawnCreep", () => { throw new Error("refused"); }],
  [creep, "move", () => -11],
  [Game, "getObjectById", (id) => (id === "a7" ? null : seen(id))],
);
if (thrown !== '[["a1","threw",-100],["a","pickup",-5],["a","move",-11]]') fail(`a throw, a missing target and a code refused as ${thrown}`);
const gone = refusals(204, [Game, "creeps", {}], [spawn, "spawnCreep", (...args) => ((spawned = args), 0)]);
if (gone !== '[["a","creep",-5],["a","creep",-5]]') fail(`a gone creep's intents refused as ${gone}`);
if (!/^\[\["work","work","carry","move"\],"harvester-204-Spawn1",\{/.test(JSON.stringify(spawned))) fail(`spawnCreep got ${JSON.stringify(spawned)}`);
// The actors smoke's world has none of, through the JS half alone: a tower whose
// lookup throws reads gone, and the site after it is still made.
const undo = [
  patch(room, "createConstructionSite", (x, y, kind) => (x === 3 && y === 4 && kind === "road" ? -8 : 0)),
  patch(Game, "getObjectById", (id) => {
    if (id === "t9") throw new Error("lookup");
    return seen(id);
  }),
];
const direct = Array.from(exec(Int32Array.of(2, 0, 1, 1, 2, 2, 1, 3, 4, 3, 0, 3, 0, 4, 1, 5), "t9\0attack\0a3\0W1N1\0createConstructionSite\0road\0"));
undo.forEach((u) => u());
if (direct.join() !== "0,-5,1,1,-8,0") fail(`a tower whose lookup threw and a refused site came back ${direct}`);

// What the recorder kept, stood as the bench's Game, reads as the tick did.
stand(recorded, [], getHeapStatistics, () => 0);
const again = globalThis.__fabotRead().slice(0, 2).map((each) => (typeof each === "string" ? each : Array.from(each)));
if (JSON.stringify(again) !== JSON.stringify(pass)) fail(`the recorded tick reads otherwise: ${JSON.stringify(again).slice(0, 200)}`);

const version = readFileSync(new URL(bundle, import.meta.url), "utf8").split("\n")[0];
console.log(`smoke: 4 ticks clean, and after a trap and a timeout (${version.match(/"(.*)"/)?.[1] ?? "unnamed"})`);
