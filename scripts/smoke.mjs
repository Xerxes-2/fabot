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
  id: "s1",
  name: "Spawn1",
  structureType: "spawn",
  my: true,
  owner: { username: "me" },
  pos: at(25, 25),
  hits: 5000,
  hitsMax: 5000,
  store: store({ energy: 300 }, 300),
  spawning: null,
  spawnCreep: (body, name) => (((Memory.creeps ??= {})[name] = {}), 0),
};
const road = { id: "r1", structureType: "road", pos: at(26, 25), hits: 5000, hitsMax: 5000 };
const creep = {
  id: "k1",
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
  105: [{ id: "src1", pos: at(5, 5), energy: 3000, energyCapacity: 3000, ticksToRegeneration: undefined }],
  116: [{ id: "m1", pos: at(40, 40), mineralType: "H", mineralAmount: 70000 }],
  107: [spawn, road],
  111: [{ id: "cs1", structureType: "extension", my: true, owner: { username: "me" }, pos: at(27, 27), progress: 0, progressTotal: 3000 }],
  106: [{ id: "p1", resourceType: "energy", pos: at(11, 11), amount: 50 }],
  118: [],
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

globalThis.Memory = { fabot: { v1: "left alone" }, creeps: { dead: {} } };
globalThis.RawMemory = { segments, get: () => JSON.stringify(globalThis.Memory), setActiveSegments: (ids) => active.push(ids) };
globalThis.Game = {
  time: 100,
  cpu: { limit: 20, bucket: 10000, getUsed: () => (used += 0.25) },
  rooms: { W1N1: room },
  map: { getRoomTerrain: () => ({ getRawBuffer: () => terrain }) },
  spawns: { Spawn1: spawn },
  creeps: { a: creep },
  gcl: { level: 1 },
  constructionSites: { cs1: found[111][0] },
  getObjectById: (id) => ({ s1: spawn, src1: found[105][0], c1: room.controller, cs1: found[111][0], p1: found[106][0] })[id] ?? null,
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
Memory.v2.switches.recorder = true;
Game.time = 103;
main.loop();

const observe = JSON.parse(segments[1] ?? fail("no observe segment written"));
if (observe.tick !== 103) fail(`observe tick ${observe.tick}, not 103`);
if (observe.faults.length !== 0) fail(`faults: ${JSON.stringify(observe.faults)}`);
const phases = observe.phases.map(([name]) => name);
for (const want of ["memory", "world", "atlas", "economy", "execute", "write", "metrics", "record"])
  if (!phases.includes(want)) fail(`phase ${want} not reported: ${phases}`);
const pass = globalThis.__fabotRead().map((each) => (typeof each === "string" ? each : Array.from(each)));
const [recorded] = unpack(RECORDER.map((id) => segments[id]));
if (recorded?.time !== 103) fail(`recorder wrote no tick 103: ${recorded?.time}`);
if (!recorded.intel.startsWith("W1N1 103 ")) fail(`recorder's intel lost the room: ${recorded.intel}`);
if (recorded.memory !== RawMemory.get()) fail("recorder's Memory is not the tick's");
if (Memory.fabot !== undefined) fail(`v1's Memory was kept: ${JSON.stringify(Memory.fabot)}`);
if (Memory.creeps !== undefined) fail(`a dead creep's Memory was kept: ${JSON.stringify(Memory.creeps)}`);
const held = Memory.v2.assignments?.a;
if (typeof held !== "string") fail(`the creep holds no task: ${JSON.stringify(Memory.v2.assignments)}`);
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
// A cold heap replaces the active list even when intel is loaded, and reads it the tick after.
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
if (!/^@0 100\nW1N1 5$/m.test(segments[3]) || !/^@100 1$/m.test(segments[3])) fail(`the metrics' ring after a reset: ${segments[3]}`);

// What the recorder kept, stood as the bench's Game, reads as the tick did.
stand(recorded, [], getHeapStatistics, () => 0);
const again = globalThis.__fabotRead().map((each) => (typeof each === "string" ? each : Array.from(each)));
if (JSON.stringify(again) !== JSON.stringify(pass)) fail(`the recorded tick reads otherwise: ${JSON.stringify(again).slice(0, 200)}`);

const version = readFileSync(new URL(bundle, import.meta.url), "utf8").split("\n")[0];
console.log(`smoke: 4 ticks clean, and two after a trap (${version.match(/"(.*)"/)?.[1] ?? "unnamed"})`);
