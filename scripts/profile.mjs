// npm run profile — drive the compiled loop() against a stub colony under
// the V8 sampling profiler and print ms/tick plus a per-function hotspot
// table. See README "Profiling" for the stub ↔ live mapping and limits.
//
// The stub touches only the API surface declared in src/App/Bindings.fs and
// never the Screeps network API. The world is frozen: intents are accepted
// (return code 0) but nothing mutates between ticks except Game.time and
// whatever the bot writes into Memory — unless --census-every N is given,
// which moves the structure census every Nth tick so the census-keyed
// memos (ADR 0017, ADR 0032) are made to pay their recompute.

import { createRequire } from "node:module";
import { Session } from "node:inspector/promises";
import { performance } from "node:perf_hooks";
import { writeFileSync, mkdirSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const USAGE = "usage: npm run profile -- [ticks] [top-N] [--census-every N]  (positive integers)";

// Positional [ticks] [top-N] as before, with --census-every N pulled out
// from anywhere in the line.
const positional = [];
let censusEvery = 0; // absent: the frozen world, and no perturbation report
for (let i = 2; i < process.argv.length; i++) {
  if (process.argv[i] === "--census-every") censusEvery = Number(process.argv[++i]);
  else positional.push(process.argv[i]);
}

const TICKS = Number(positional[0] ?? 100);
const TOP = Number(positional[1] ?? 30);
const CENSUS_EVERY = censusEvery;
const notPositive = (n) => !Number.isInteger(n) || n < 1;
if (notPositive(TICKS) || notPositive(TOP) || (CENSUS_EVERY !== 0 && notPositive(CENSUS_EVERY))) {
  console.error(USAGE);
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

  // Border walls. Rows/cols 0 and 49 are projected — they are the Seam's
  // layer (ADR 0041), not nothing — and they are walled here because this
  // scenario is one room with no neighbour to cross to, so every Seam band
  // is empty by construction. A scenario with an outpost has to carve its
  // exits, or the cross-room walk it profiles costs nothing.
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

  // --- census perturbation (--census-every) ------------------------------
  // The lane to source B is carved but unpaved — the room's spare line.
  // Each perturbation paves the next tile of it, and once the lane is
  // fully paved lifts the tiles again one at a time, so a run of any
  // length keeps moving the census by exactly one standing structure —
  // enough to move the signature (ADR 0017) and drop the walk table (ADR
  // 0032), while the world stays the same size and shape. The paved tile
  // stands at the default 4000/5000 hits, above the repair trigger, so what
  // a perturbed tick pays for is the recompute and not a new Repair task.
  const spare = line(SPAWN_POS, SOURCE_B).filter((p) => !occupied.has(`${p.x},${p.y}`));
  const structures = findTables[107];
  const spareRoads = new Map();
  let spareNext = 0;
  const perturb = () => {
    if (!spare.length) throw new Error("--census-every: no unpaved tile left to move the census");
    const pos = spare[spareNext++ % spare.length];
    const key = `${pos.x},${pos.y}`;
    const standing = spareRoads.get(key);
    if (standing) {
      spareRoads.delete(key);
      byId.delete(standing.id);
      structures.splice(structures.indexOf(standing), 1);
      return;
    }
    const road = structure(`road-spare-${key}`, "road", pos);
    spareRoads.set(key, road);
    structures.push(road);
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

  // One terrain object per room, handed back unchanged on every call, as
  // the engine does — and counted, so the per-room terrain memo (ADR
  // 0031) is checkable: the bot should read it once per heap lifetime.
  const terrain = buildTerrain();
  let terrainReads = 0;
  const game = {
    time: 1000,
    cpu: { getUsed: () => performance.now() },
    map: {
      getRoomTerrain: () => {
        terrainReads++;
        return terrain;
      },
    },
    rooms: { [ROOM]: room },
    spawns: { Spawn1: spawn },
    creeps: Object.fromEntries(creeps.map((c) => [c.name, c])),
    getObjectById: (id) => byId.get(id) ?? null,
  };
  return {
    game,
    roadCount: roads.length,
    terrainReads: () => terrainReads,
    perturb,
    spareTiles: spare.length,
  };
}

// The frames the census-keyed memos exist to skip: the Layout and the
// hauler quota behind the plan memo (ADR 0017), and the spawn walks behind
// the walk table (ADR 0032). Named rather than ranked, so the perturbation
// report always shows them however far down the hotspot tables they sit.
const CENSUS_KEYED = ["planLayout", "haulerQuota", "trunkPath", "castWalkTicks"];

// ---------------------------------------------------------------------------
// .cpuprofile aggregation: self and inclusive sampled time per call frame.
// ---------------------------------------------------------------------------

const META = new Set(["(root)", "(program)", "(idle)"]);

function summarize(profile, keepSample = () => true) {
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
    if (!node || !keepSample(samples[i])) continue;
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

// Which class of tick each call node sits under, read off the marker frame
// the profiled loop() is called through (see perturbedTick below). Nodes
// outside the tick — the profiler's own start/stop — belong to neither.
function tickClasses(profile) {
  const marks = { perturbedTick: "perturbed", quietTick: "quiet" };
  const nodes = new Map(profile.nodes.map((n) => [n.id, n]));
  const parented = new Set();
  for (const n of profile.nodes) for (const c of n.children ?? []) parented.add(c);

  const cls = new Map();
  const walk = (id, inherited) => {
    const node = nodes.get(id);
    if (!node) return;
    const here = marks[node.callFrame.functionName] ?? inherited;
    cls.set(id, here);
    for (const child of node.children ?? []) walk(child, here);
  };
  for (const n of profile.nodes) if (!parented.has(n.id)) walk(n.id, undefined);
  return cls;
}

function stats(msList) {
  const sorted = [...msList].sort((a, b) => a - b);
  return {
    n: msList.length,
    mean: msList.reduce((a, b) => a + b, 0) / msList.length,
    median: sorted[Math.floor(sorted.length / 2)],
    min: sorted[0],
    max: sorted[sorted.length - 1],
  };
}

// What the memos are worth: the sampled inclusive cost of each census-keyed
// frame on a tick that has to recompute it, against a tick that recalls it.
// The rows nest — trunkPath's ms are inside planLayout's — so read them one
// at a time; the column is not a sum and does not add up to the tick.
function printCensusKeyed(classes) {
  const msPerTick = ({ summary, ms }, name) =>
    summary.rows
      .filter((row) => row.name === name)
      .reduce((total, row) => total + row.inclusiveUs, 0) /
    1000 /
    ms.length;

  console.log("\ncensus-keyed frames — inclusive ms per tick of each class");
  console.log(`  ${classes.map((c) => c.label.padStart(9)).join("  ")}  function`);
  for (const name of CENSUS_KEYED) {
    console.log(
      `  ${classes.map((c) => msPerTick(c, name).toFixed(2).padStart(9)).join("  ")}  ${name}`
    );
  }
}

function printReport(classes, pooled, stub) {
  console.log(
    `fabot profile — ${TICKS} ticks, stub colony ` +
      `(2 sources, spawn, controller, ${stub.roadCount} roads, 2 containers, ` +
      `${SITES.length} sites, 8 creeps)` +
      (CENSUS_EVERY
        ? `, census moved every ${CENSUS_EVERY} ticks over a ${stub.spareTiles}-tile lane`
        : "")
  );

  // Under perturbation the two classes of tick are different workloads — one
  // pays the census-keyed recompute, the other recalls it — so ms/tick, the
  // hotspot tables and the census-keyed frames are all reported per class.
  // One mean over the two would hide both.
  if (CENSUS_EVERY) {
    for (const { label, ms } of classes) {
      const s = stats(ms);
      console.log(
        `ms/tick ${label.padEnd(9)} (${String(s.n).padStart(4)} ticks): ` +
          `mean ${s.mean.toFixed(2)}  median ${s.median.toFixed(2)}  ` +
          `min ${s.min.toFixed(2)}  max ${s.max.toFixed(2)}`
      );
    }
    printCensusKeyed(classes);
  } else {
    const s = stats(classes[0].ms);
    console.log(
      `ms/tick: mean ${s.mean.toFixed(2)}  median ${s.median.toFixed(2)}  ` +
        `min ${s.min.toFixed(2)}  max ${s.max.toFixed(2)}`
    );
  }

  // Samples V8 parents at the root — the garbage collector, and the
  // profiler's own start and stop — sit under no tick marker, so they are in
  // neither class and each class's percentages are on its own base.
  const outside = pooled.activeUs - classes.reduce((total, c) => total + c.summary.activeUs, 0);
  if (outside > 0) {
    console.log(
      `\n${(outside / 1000).toFixed(0)} ms sampled outside both classes ` +
        "(root-parented GC, and the profiler's own start and stop)"
    );
  }
  if (CENSUS_EVERY) console.log("");

  for (const { label, summary } of classes) {
    const head = CENSUS_EVERY ? `${label} ticks — sampled` : "sampled";
    console.log(`${head} ${(summary.activeUs / 1000).toFixed(0)} ms at ${SAMPLE_INTERVAL_US}µs\n`);

    const pct = (us) => ((100 * us) / summary.activeUs).toFixed(1).padStart(5);
    const line = (row) => {
      const ms = (row.selfUs / 1000).toFixed(1).padStart(9);
      const loc = row.location ? `  [${row.location}]` : "";
      return `  ${pct(row.selfUs)}  ${pct(row.inclusiveUs)} ${ms}  ${row.name}${loc}`;
    };
    // `always` names rows to print even when they rank below the cut: the
    // census-keyed frames are the point of a perturbed run and are easily
    // outranked by the F# runtime plumbing they pass through.
    const table = (title, rows, always = []) => {
      console.log(`${title}\n  self%  incl%   self ms  function`);
      const shown = rows.slice(0, TOP);
      for (const row of shown) console.log(line(row));
      const below = rows.filter((row) => always.includes(row.name) && !shown.includes(row));
      if (below.length) {
        console.log("  census-keyed frames below the cut:");
        for (const row of below) console.log(line(row));
      }
      console.log("");
    };

    // Self time names where samples land (runtime primitives, floods); the
    // inclusive view names the phases paying for them (decide, planLayout, …).
    table("hot by self time", [...summary.rows].sort((a, b) => b.selfUs - a.selfUs));
    table(
      "hot by inclusive time",
      [...summary.rows].sort((a, b) => b.inclusiveUs - a.inclusiveUs),
      CENSUS_EVERY ? CENSUS_KEYED : []
    );
  }
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

const { game, terrainReads, perturb, ...stub } = buildWorld();
globalThis.Game = game;
globalThis.Memory = {};

const { loop } = createRequire(import.meta.url)(bundle);

// One counter over warm-up and profiled ticks alike, so the recompute path
// is JIT-warm before it is measured.
let tick = 0;
const movesCensus = () => CENSUS_EVERY > 0 && tick % CENSUS_EVERY === 0;

// Tick-class markers. Under --census-every the tick is run through one of
// these, so every sample's stack names the class of tick it was taken in
// and the hotspot tables can be split. The marker is picked before the
// call, never around it: an unflagged run calls loop() itself, so its
// stacks — and its numbers — are exactly what they were.
function perturbedTick() {
  loop();
}

function quietTick() {
  loop();
}

const tickThrough = (moved) => (!CENSUS_EVERY ? loop : moved ? perturbedTick : quietTick);

for (let i = 0; i < WARMUP; i++) {
  const moved = movesCensus();
  if (moved) perturb();
  tickThrough(moved)();
  game.time++;
  tick++;
}

const session = new Session();
session.connect();
await session.post("Profiler.enable");
await session.post("Profiler.setSamplingInterval", { interval: SAMPLE_INTERVAL_US });
await session.post("Profiler.start");

const ticks = { all: [], perturbed: [], quiet: [] };
for (let i = 0; i < TICKS; i++) {
  const moved = movesCensus();
  if (moved) perturb();
  const run = tickThrough(moved);
  const start = performance.now();
  run();
  const ms = performance.now() - start;
  ticks.all.push(ms);
  (moved ? ticks.perturbed : ticks.quiet).push(ms);
  game.time++;
  tick++;
}

const { profile } = await session.post("Profiler.stop");
session.disconnect();

mkdirSync(path.join(here, "..", "build"), { recursive: true });
const profilePath = path.join(here, "..", "build", "fabot.cpuprofile");
writeFileSync(profilePath, JSON.stringify(profile));

// One entry per class of tick worth reporting apart: the whole run when the
// world is frozen, the perturbed and the quiet ticks when it is not.
const pooled = summarize(profile);
const classOf = CENSUS_EVERY ? tickClasses(profile) : null;
const classes = CENSUS_EVERY
  ? [
      { label: "perturbed", ms: ticks.perturbed },
      { label: "quiet", ms: ticks.quiet },
    ]
      .filter((c) => c.ms.length)
      .map((c) => ({ ...c, summary: summarize(profile, (id) => classOf.get(id) === c.label) }))
  : [{ label: "all", ms: ticks.all, summary: pooled }];

printReport(classes, pooled, stub);
console.log(`engine terrain reads: ${terrainReads()} over ${WARMUP + TICKS} ticks (Game.map.getRoomTerrain)`);
console.log(`\nraw profile: ${path.relative(process.cwd(), profilePath)} (open in Chrome DevTools / speedscope)`);
