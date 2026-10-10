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
const room = {
  name: "W1N1",
  energyAvailable: 300,
  energyCapacityAvailable: 300,
  controller: { id: "c1", my: true, level: 1, ticksToDowngrade: 20000 },
};
const spawn = { id: "s1", name: "Spawn1", room, spawning: null };
const creep = {
  name: "a",
  spawning: false,
  pos: { x: 10, y: 10, roomName: "W1N1" },
  hits: 100,
  hitsMax: 100,
  ticksToLive: 1400,
  say: () => 0,
  move: () => 0,
};

globalThis.Memory = { fabot: { v1: "left alone" } };
globalThis.RawMemory = { segments, get: () => JSON.stringify(globalThis.Memory) };
globalThis.Game = {
  time: 100,
  cpu: { limit: 20, bucket: 10000, getUsed: () => (used += 0.25) },
  rooms: { W1N1: room },
  spawns: { Spawn1: spawn },
  creeps: { a: creep },
  getObjectById: (id) => (id === "s1" ? spawn : null),
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
for (const want of ["memory", "world", "execute", "record"])
  if (!phases.includes(want)) fail(`phase ${want} not reported: ${phases}`);
const recorded = JSON.parse(segments[10 + (103 % 5)] ?? fail("recorder wrote nothing"));
if (recorded.world.Creeps.length !== 1) fail("recorder's world lost the creep");
if (Memory.fabot.v1 !== "left alone") fail("v1's Memory was touched");

const version = readFileSync(new URL("../dist/main.js", import.meta.url), "utf8").split("\n")[0];
console.log(`smoke: 4 ticks clean (${version.match(/"(.*)"/)?.[1] ?? "unnamed"})`);
