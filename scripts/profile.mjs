// npm run profile — drive the compiled loop() against a stub colony under
// the V8 sampling profiler and print ms/tick plus a per-function hotspot
// table. See README "Profiling" for the stub ↔ live mapping and limits.
//
// The stub touches only the API surface declared in src/App/Bindings.fs and
// never the Screeps network API. The world is frozen: intents are accepted
// (return code 0) but nothing mutates between ticks except Game.time and
// whatever the bot writes into Memory.

import { createRequire } from "node:module";
import { Session } from "node:inspector/promises";
import { performance } from "node:perf_hooks";
import { writeFileSync, mkdirSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const TICKS = Number(process.argv[2] ?? 100);
const TOP = Number(process.argv[3] ?? 30);
if (!Number.isInteger(TICKS) || TICKS < 1 || !Number.isInteger(TOP) || TOP < 1) {
  console.error("usage: npm run profile -- [ticks] [top-N]  (positive integers)");
  process.exit(1);
}
const WARMUP = 3; // unprofiled JIT warm-up ticks
const SAMPLE_INTERVAL_US = 100;

// ---------------------------------------------------------------------------
// Stub colony: a deterministic in-memory room shaped like the live colony
// (2 sources, spawn, controller, trunk roads, source + controller
// containers, 3 extension sites, 8 creeps).
// ---------------------------------------------------------------------------

const ROOM = "W1N1";
const WALL = 1;
const SWAMP = 2;

// Deterministic PRNG so every run profiles the identical world.
function lcg(seed) {
  let s = seed >>> 0;
  return () => {
    s = (Math.imul(s, 1664525) + 1013904223) >>> 0;
    return s / 2 ** 32;
  };
}

// Chebyshev line from a to b, endpoints included.
function line(a, b) {
  const tiles = [];
  let { x, y } = a;
  tiles.push({ x, y });
  while (x !== b.x || y !== b.y) {
    x += Math.sign(b.x - x);
    y += Math.sign(b.y - y);
    tiles.push({ x, y });
  }
  return tiles;
}

const SPAWN_POS = { x: 25, y: 25 };
const SOURCE_A = { x: 11, y: 14 };
const SOURCE_B = { x: 38, y: 39 };
const CONTROLLER = { x: 8, y: 33 };
const CONTAINER_A = { x: 12, y: 15 }; // source container, beside source A
const CONTAINER_B = { x: 9, y: 32 }; // controller container
const SITES = [
  { x: 23, y: 23 },
  { x: 23, y: 27 },
  { x: 27, y: 23 },
];
// The paved trunks; buildTerrain also carves an unpaved lane to source B so
// the room is connected everywhere the bot expects to reach.
const TRUNKS = [
  [SPAWN_POS, CONTAINER_A],
  [SPAWN_POS, CONTAINER_B],
];

function buildTerrain() {
  const data = new Uint8Array(50 * 50);
  const rand = lcg(0xfab07);

  // Border walls (rows/cols 0 and 49 are outside the projection anyway).
  for (let i = 0; i < 50; i++) {
    data[i] = data[49 * 50 + i] = data[i * 50] = data[i * 50 + 49] = WALL;
  }

  // Scattered wall blobs and swamp patches, grown by short random walks.
  const blob = (kind, count, steps) => {
    for (let b = 0; b < count; b++) {
      let x = 3 + Math.floor(rand() * 44);
      let y = 3 + Math.floor(rand() * 44);
      for (let s = 0; s < steps; s++) {
        if (x > 0 && x < 49 && y > 0 && y < 49) data[y * 50 + x] = kind;
        x += Math.floor(rand() * 3) - 1;
        y += Math.floor(rand() * 3) - 1;
      }
    }
  };
  blob(WALL, 14, 7);
  blob(SWAMP, 10, 12);

  // Carve every colony tile (plus a working halo) back to plain so the
  // layout is guaranteed connected and harvest/upgrade spots exist.
  const carve = ({ x, y }) => {
    for (let dx = -1; dx <= 1; dx++) {
      for (let dy = -1; dy <= 1; dy++) {
        const cx = x + dx;
        const cy = y + dy;
        if (cx > 0 && cx < 49 && cy > 0 && cy < 49) data[cy * 50 + cx] = 0;
      }
    }
  };
  for (const p of [SPAWN_POS, SOURCE_A, SOURCE_B, CONTROLLER, CONTAINER_A, CONTAINER_B, ...SITES]) {
    carve(p);
  }
  for (const [a, b] of [...TRUNKS, [SPAWN_POS, SOURCE_B]]) {
    for (const p of line(a, b)) carve(p);
  }

  return { get: (x, y) => data[y * 50 + x] };
}

function store({ used = 0, capacity = 0 } = {}) {
  return {
    getUsedCapacity: () => used,
    getFreeCapacity: () => capacity - used,
  };
}

function buildWorld() {
  const byId = new Map();
  const register = (obj) => {
    byId.set(obj.id, obj);
    return obj;
  };
  const ok = () => 0;

  const sources = [SOURCE_A, SOURCE_B].map((pos, i) =>
    register({ id: `src-${i}`, pos, energy: 3000, ticksToRegeneration: undefined })
  );

  const controller = register({
    id: "ctrl",
    my: true,
    level: 3,
    ticksToDowngrade: 9000,
    safeModeAvailable: 1,
    safeMode: undefined,
    pos: CONTROLLER,
    activateSafeMode: ok,
  });

  const structure = (id, structureType, pos, extra = {}) =>
    register({
      id,
      structureType,
      pos,
      hits: 4000,
      hitsMax: 5000,
      store: store(),
      ...extra,
    });

  // Trunk roads: spawn → source container and spawn → controller container,
  // skipping tiles already holding a structure, site, or endpoint.
  const occupied = new Set(
    [SPAWN_POS, SOURCE_A, SOURCE_B, CONTROLLER, CONTAINER_A, CONTAINER_B, ...SITES].map(
      (p) => `${p.x},${p.y}`
    )
  );
  const roadTiles = [];
  for (const [a, b] of TRUNKS) {
    for (const p of line(a, b)) {
      const key = `${p.x},${p.y}`;
      if (!occupied.has(key)) {
        occupied.add(key);
        roadTiles.push(p);
      }
    }
  }
  // A couple of roads below half hits, so the Repair family is in the
  // measurement instead of pooling zero tasks.
  const roads = roadTiles.map((pos, i) =>
    structure(`road-${i}`, "road", pos, i % 8 === 3 ? { hits: 2100 } : {})
  );

  const containers = [
    structure("cont-src", "container", CONTAINER_A, { store: store({ used: 1500, capacity: 2000 }) }),
    structure("cont-ctrl", "container", CONTAINER_B, { store: store({ used: 800, capacity: 2000 }) }),
  ];

  // One object serves as structure (find tables), spawn (Game.spawns), and
  // getObjectById target; the spawn-specific fields are attached below once
  // the room exists.
  const spawn = structure("spawn-1", "spawn", SPAWN_POS, {
    store: store({ used: 250, capacity: 300 }),
    hits: 5000,
    hitsMax: 5000,
  });

  const sites = SITES.map((pos, i) =>
    register({ id: `site-${i}`, structureType: "extension", pos })
  );

  // 8 creeps in the live colony's mix of body patterns: 2 Anchors parked at
  // the sources, 3 hauler units on the trunk, 3 worker units at the
  // controller and the sites.
  const body = (spec) =>
    Object.entries(spec).flatMap(([type, n]) => Array(n).fill({ type }));
  const creepDefs = [
    { name: "anchor-a", pos: CONTAINER_A, spec: { work: 3, carry: 1, move: 1 }, used: 30 },
    { name: "anchor-b", pos: { x: 37, y: 38 }, spec: { work: 3, carry: 1, move: 1 }, used: 50 },
    { name: "haul-1", pos: { x: 18, y: 20 }, spec: { carry: 4, move: 4 }, used: 200 },
    { name: "haul-2", pos: { x: 15, y: 28 }, spec: { carry: 4, move: 4 }, used: 0 },
    { name: "haul-3", pos: { x: 26, y: 26 }, spec: { carry: 4, move: 4 }, used: 100 },
    { name: "work-1", pos: { x: 9, y: 33 }, spec: { work: 2, carry: 1, move: 2 }, used: 50 },
    { name: "work-2", pos: { x: 10, y: 32 }, spec: { work: 2, carry: 1, move: 2 }, used: 0 },
    { name: "work-3", pos: { x: 24, y: 23 }, spec: { work: 2, carry: 2, move: 2 }, used: 60 },
  ];
  const creeps = creepDefs.map(({ name, pos, spec, used }) =>
    register({
      id: `creep-${name}`,
      name,
      spawning: false,
      ticksToLive: 1500,
      fatigue: 0,
      pos,
      body: body(spec),
      store: store({ used, capacity: (spec.carry ?? 0) * 50 }),
      harvest: ok,
      transfer: ok,
      withdraw: ok,
      build: ok,
      repair: ok,
      upgradeController: ok,
      pickup: ok,
      move: ok,
      say: ok,
    })
  );

  const findTables = {
    105: sources, // FIND_SOURCES
    108: [spawn], // FIND_MY_STRUCTURES (refillables)
    107: [spawn, ...roads, ...containers], // FIND_STRUCTURES
    114: sites, // FIND_MY_CONSTRUCTION_SITES
    103: [], // FIND_HOSTILE_CREEPS
    106: [], // FIND_DROPPED_RESOURCES
  };

  const room = {
    name: ROOM,
    energyAvailable: 250,
    energyCapacityAvailable: 300,
    controller,
    find: (type) => findTables[type] ?? [],
    createConstructionSite: ok,
  };

  Object.assign(spawn, { name: "Spawn1", spawning: null, room, spawnCreep: ok });

  const terrain = buildTerrain();
  const game = {
    time: 1000,
    cpu: { getUsed: () => performance.now() },
    map: { getRoomTerrain: () => terrain },
    rooms: { [ROOM]: room },
    spawns: { Spawn1: spawn },
    creeps: Object.fromEntries(creeps.map((c) => [c.name, c])),
    getObjectById: (id) => byId.get(id) ?? null,
  };
  return { game, roadCount: roads.length };
}

// ---------------------------------------------------------------------------
// .cpuprofile aggregation: self and inclusive sampled time per call frame.
// ---------------------------------------------------------------------------

const META = new Set(["(root)", "(program)", "(idle)"]);

function summarize(profile) {
  const nodes = new Map(profile.nodes.map((n) => [n.id, n]));
  const parent = new Map();
  for (const n of profile.nodes) {
    for (const childId of n.children ?? []) parent.set(childId, n.id);
  }

  const keyOf = (node) => {
    const f = node.callFrame;
    const name = f.functionName || "(anonymous)";
    const file = f.url ? path.basename(f.url) : "";
    return file ? `${name}\u0000${file}:${f.lineNumber + 1}` : `${name}\u0000`;
  };

  // Distinct frame keys on each node's stack, for inclusive attribution
  // (a Set so recursion counts a frame once per sample).
  const stackKeys = new Map();
  const stackOf = (id) => {
    let keys = stackKeys.get(id);
    if (!keys) {
      const parentId = parent.get(id);
      keys = new Set(parentId === undefined ? [] : stackOf(parentId));
      keys.add(keyOf(nodes.get(id)));
      stackKeys.set(id, keys);
    }
    return keys;
  };

  const self = new Map();
  const inclusive = new Map();
  let active = 0;
  const { samples, timeDeltas } = profile;
  for (let i = 0; i < samples.length; i++) {
    // µs; clamp V8's occasional negatives. The delta is really the gap
    // before this sample — attributing it here is off by one sample, noise
    // at a 100µs interval.
    const dt = Math.max(0, timeDeltas[i] ?? 0);
    const node = nodes.get(samples[i]);
    if (!node) continue;
    const key = keyOf(node);
    if (node.callFrame.functionName === "(idle)") continue;
    active += dt;
    self.set(key, (self.get(key) ?? 0) + dt);
    for (const k of stackOf(samples[i])) {
      inclusive.set(k, (inclusive.get(k) ?? 0) + dt);
    }
  }

  // Keys come from the inclusive map — a superset of the self map, so
  // frames that only ever appear mid-stack (planLayout, decide) get rows.
  const rows = [...inclusive.entries()]
    .filter(([key]) => !META.has(key.split("\u0000")[0]))
    .map(([key, inclusiveUs]) => {
      const [name, location] = key.split("\u0000");
      return { name, location, selfUs: self.get(key) ?? 0, inclusiveUs };
    });

  return { rows, activeUs: active };
}

function printReport(tickMs, summary, roadCount) {
  const sorted = [...tickMs].sort((a, b) => a - b);
  const mean = tickMs.reduce((a, b) => a + b, 0) / tickMs.length;
  const median = sorted[Math.floor(sorted.length / 2)];

  console.log(
    `fabot profile — ${tickMs.length} ticks, stub colony ` +
      `(2 sources, spawn, controller, ${roadCount} roads, 2 containers, ${SITES.length} sites, 8 creeps)`
  );
  console.log(
    `ms/tick: mean ${mean.toFixed(2)}  median ${median.toFixed(2)}  ` +
      `min ${sorted[0].toFixed(2)}  max ${sorted[sorted.length - 1].toFixed(2)}`
  );
  console.log(`sampled ${(summary.activeUs / 1000).toFixed(0)} ms at ${SAMPLE_INTERVAL_US}µs\n`);

  const pct = (us) => ((100 * us) / summary.activeUs).toFixed(1).padStart(5);
  const table = (title, rows) => {
    console.log(`${title}\n  self%  incl%   self ms  function`);
    for (const row of rows.slice(0, TOP)) {
      const ms = (row.selfUs / 1000).toFixed(1).padStart(9);
      const loc = row.location ? `  [${row.location}]` : "";
      console.log(`  ${pct(row.selfUs)}  ${pct(row.inclusiveUs)} ${ms}  ${row.name}${loc}`);
    }
    console.log("");
  };

  // Self time names where samples land (runtime primitives, floods); the
  // inclusive view names the phases paying for them (decide, planLayout, …).
  table("hot by self time", [...summary.rows].sort((a, b) => b.selfUs - a.selfUs));
  table("hot by inclusive time", [...summary.rows].sort((a, b) => b.inclusiveUs - a.inclusiveUs));
}

// ---------------------------------------------------------------------------
// Drive it.
// ---------------------------------------------------------------------------

const here = path.dirname(fileURLToPath(import.meta.url));
const bundle = path.join(here, "..", "dist", "main.js");
if (!existsSync(bundle)) {
  console.error("dist/main.js not found — run `npm run build` first.");
  process.exit(1);
}

const { game, roadCount } = buildWorld();
globalThis.Game = game;
globalThis.Memory = {};

const { loop } = createRequire(import.meta.url)(bundle);

for (let i = 0; i < WARMUP; i++) {
  loop();
  game.time++;
}

const session = new Session();
session.connect();
await session.post("Profiler.enable");
await session.post("Profiler.setSamplingInterval", { interval: SAMPLE_INTERVAL_US });
await session.post("Profiler.start");

const tickMs = [];
for (let i = 0; i < TICKS; i++) {
  const start = performance.now();
  loop();
  tickMs.push(performance.now() - start);
  game.time++;
}

const { profile } = await session.post("Profiler.stop");
session.disconnect();

mkdirSync(path.join(here, "..", "build"), { recursive: true });
const profilePath = path.join(here, "..", "build", "fabot.cpuprofile");
writeFileSync(profilePath, JSON.stringify(profile));

printReport(tickMs, summarize(profile), roadCount);
console.log(`\nraw profile: ${path.relative(process.cwd(), profilePath)} (open in Chrome DevTools / speedscope)`);
