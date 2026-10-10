// Run the built bundle for a few ticks against a stub game: the cheapest check
// that the Fable output loads, reads, writes and does not throw. Not a test of
// behaviour; that is xxscreeps' (ARCHITECTURE.md §10).
//
// Usage: node scripts/smoke.mjs   (after `npm run build`)
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";

const fail = (msg) => {
  console.error(`smoke: ${msg}`);
  process.exit(1);
};

const segments = {};
let used = 0;
const at = (x, y) => ({ x, y, roomName: "W1N1" });
const store = (held, capacity) => ({ ...held, getCapacity: (r) => (r === undefined || r === "energy" ? capacity : null) });
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
  spawnCreep: () => 0,
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
};
spawn.room = room;
creep.room = room;
const terrain = new Uint8Array(2500);
terrain[0] = 1;
terrain[51] = 2;

globalThis.Memory = { fabot: { v1: "left alone" } };
globalThis.RawMemory = { segments, get: () => JSON.stringify(globalThis.Memory) };
globalThis.Game = {
  time: 100,
  cpu: { limit: 20, bucket: 10000, getUsed: () => (used += 0.25) },
  rooms: { W1N1: room },
  map: { getRoomTerrain: () => ({ getRawBuffer: () => terrain }) },
  spawns: { Spawn1: spawn },
  creeps: { a: creep },
  getObjectById: (id) => ({ s1: spawn, src1: found[105][0], c1: room.controller, cs1: found[111][0], p1: found[106][0] })[id] ?? null,
};

const require = createRequire(import.meta.url);
const main = require("../dist/main.js");
if (typeof main.loop !== "function") fail("dist/main.js exports no loop");

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
for (const want of ["memory", "world", "atlas", "economy", "execute", "write", "record"])
  if (!phases.includes(want)) fail(`phase ${want} not reported: ${phases}`);
const recorded = JSON.parse(segments[10 + (103 % 5)] ?? fail("recorder wrote nothing"));
const w = recorded.world;
if (w.Creeps.length !== 1) fail("recorder's world lost the creep");
const [r] = w.Rooms;
if (typeof r.Terrain !== "string" || r.Terrain.length !== 2500 || r.Terrain.slice(0, 2) !== "10")
  fail(`terrain not recorded as 2,500 digits: ${String(r.Terrain).slice(0, 40)}`);
if (r.Sources.length !== 1 || r.Structures.length !== 2 || r.Sites.length !== 1 || r.Piles.length !== 1)
  fail(`room facts lost: ${JSON.stringify({ s: r.Sources.length, st: r.Structures.length, si: r.Sites.length, p: r.Piles.length })}`);
const [k] = w.Creeps;
if (k.Body.length !== 3 || k.Store.Capacity !== 50) fail(`creep facts lost: ${JSON.stringify(k).slice(0, 200)}`);
if (Memory.fabot.v1 !== "left alone") fail("v1's Memory was touched");
const held = Memory.v2.assignments?.a;
if (typeof held !== "string") fail(`the creep holds no task: ${JSON.stringify(Memory.v2.assignments)}`);
if (observe.refusals.some((r) => r.code === -100)) fail(`an intent threw: ${JSON.stringify(observe.refusals)}`);

const version = readFileSync(new URL("../dist/main.js", import.meta.url), "utf8").split("\n")[0];
console.log(`smoke: 4 ticks clean (${version.match(/"(.*)"/)?.[1] ?? "unnamed"})`);
