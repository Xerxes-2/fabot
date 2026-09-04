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
//
// Two scenarios, chosen with --scenario:
//   stub     one synthetic room (the default), the shape this harness has
//            always measured, kept byte-for-byte so its numbers stay
//            comparable across commits
//   outpost  the colony's own room and its declared neighbours, on the
//            committed real terrain (ADR 0036) — the world ADR 0041's
//            layered projection is sized against

import { createRequire } from "node:module";
import { Session } from "node:inspector/promises";
import { performance } from "node:perf_hooks";
import { writeFileSync, readFileSync, mkdirSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";
import { report as cpuReport } from "./cpu-trigger.mjs";

const SCENARIOS = ["stub", "outpost"];
const USAGE =
  "usage: npm run profile -- [ticks] [top-N] [--census-every N] [--scenario stub|outpost]" +
  "  (positive integers)";

// Positional [ticks] [top-N] as before, with --census-every N and
// --scenario NAME pulled out from anywhere in the line.
const positional = [];
let censusEvery = 0; // absent: the frozen world, and no perturbation report
let scenario = "stub";
for (let i = 2; i < process.argv.length; i++) {
  if (process.argv[i] === "--census-every") censusEvery = Number(process.argv[++i]);
  else if (process.argv[i] === "--scenario") scenario = process.argv[++i];
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
if (!SCENARIOS.includes(scenario)) {
  console.error(`unknown scenario "${scenario}"; one of: ${SCENARIOS.join(", ")}\n${USAGE}`);
  process.exit(1);
}
const WARMUP = 3; // unprofiled JIT warm-up ticks
const SAMPLE_INTERVAL_US = 100;

const WALL = 1;
const SWAMP = 2;

// ---------------------------------------------------------------------------
// Stub engine surface shared by every scenario.
// ---------------------------------------------------------------------------

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

function store({ used = 0, capacity = 0 } = {}) {
  return {
    getUsedCapacity: () => used,
    getFreeCapacity: () => capacity - used,
  };
}

const ok = () => 0;
const body = (spec) => Object.entries(spec).flatMap(([type, n]) => Array(n).fill({ type }));

// The tick the profiled loop is running inside. `Game.cpu.getUsed()` is
// the engine's "milliseconds spent so far this tick", and the bot now
// reads it to write the CPU line (ADR 0041) — so a stub answering a clock
// that never resets would record the process's whole age as the tick's
// cost. Set immediately before every loop() call, warm-up included.
let tickStart = performance.now();

// The world the bundle runs against: the rooms, the objects in them, and
// the slice of the engine Bindings.fs declares over both.
//
// `unmodelled` is the scenario's answer for a room it did not build. The
// engine has terrain for every room in the world, so a scenario that can
// name one — the `stub` world's synthetic neighbours — hands back a grid
// rather than throwing; a scenario whose rooms are the property under test
// leaves it out, and a name it does not hold is the harness lying.
function buildGame({ terrains, rooms, spawns, creeps, byId, unmodelled }) {
  // One terrain object per room, handed back unchanged on every call, as
  // the engine does — and counted per room, so the per-room terrain memo
  // (ADR 0031, layered by ADR 0041) is checkable: the bot should read each
  // projected room once per heap lifetime, and read no room it does not
  // project.
  const terrainReads = new Map([...terrains.keys()].map((name) => [name, 0]));
  const game = {
    time: 1000,
    cpu: { getUsed: () => performance.now() - tickStart },
    map: {
      // Answered by room name. The engine answers for every room in the
      // world with no vision and never goes stale, which is the whole
      // reason ADR 0041's terrain layer costs nothing across rooms; a stub
      // that ignored the argument would hand the outpost the home room's
      // walls and quietly measure one room twice.
      getRoomTerrain: (roomName) => {
        let terrain = terrains.get(roomName);
        if (!terrain) {
          if (!unmodelled) {
            throw new Error(
              `the stub world holds no terrain for ${roomName} — ` +
                `it knows ${[...terrains.keys()].join(", ")}`,
            );
          }
          // Memoised under its name like every modelled room, so the
          // engine's "the same object every call" holds for it too.
          terrain = unmodelled(roomName);
          terrains.set(roomName, terrain);
          terrainReads.set(roomName, 0);
        }
        terrainReads.set(roomName, terrainReads.get(roomName) + 1);
        return terrain;
      },
    },
    // Visible rooms only, as the engine's hash is.
    rooms: Object.fromEntries(rooms.map((room) => [room.name, room])),
    spawns: Object.fromEntries(spawns.map((spawn) => [spawn.name, spawn])),
    creeps: Object.fromEntries(creeps.map((creep) => [creep.name, creep])),
    getObjectById: (id) => byId.get(id) ?? null,
  };
  return { game, terrainReads };
}

// A stub room, with the find tables the projection sweeps and the creeps
// standing in it wired to it. Every creep carries the `room` back-reference
// the engine gives it: `Game.creeps` is world-wide and `Snapshot.fs` scopes
// it to the room being projected by reading `creep.room.name` (ADR 0041).
function stubRoom({ name, controller, findTables, energy = { available: 0, capacity: 0 } }) {
  return {
    name,
    energyAvailable: energy.available,
    energyCapacityAvailable: energy.capacity,
    controller,
    find: (type) => findTables[type] ?? [],
    createConstructionSite: ok,
  };
}

// ---------------------------------------------------------------------------
// Scenario `stub`: a deterministic in-memory room shaped like the live
// colony (2 sources, spawn, controller, trunk roads, source + controller
// containers, 3 extension sites, 8 creeps).
// ---------------------------------------------------------------------------

const ROOM = "W1N1";

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
  // is empty by construction. The `outpost` scenario is the one that
  // carries real exits.
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

function buildStubWorld() {
  const byId = new Map();
  const register = (obj) => {
    byId.set(obj.id, obj);
    return obj;
  };

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
  const creeps = creepDefs.map((def) => register(stubCreep(def)));

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

  const room = stubRoom({
    name: ROOM,
    controller,
    findTables,
    energy: { available: 250, capacity: 300 },
  });

  Object.assign(spawn, { name: "Spawn1", spawning: null, room, spawnCreep: ok });
  for (const creep of creeps) creep.room = room;

  return {
    terrains: new Map([[ROOM, buildTerrain()]]),
    // A room this scenario does not model, answered as solid rock. The
    // scan set is the spawn room plus every declared outpost (ADR 0041),
    // and `Snapshot.projectRoom` reads terrain for all of them whether or
    // not there is vision — so the tick `Outpost.declared` stops being
    // empty (ADR 0042, #126) is the tick a one-room stub is asked for a
    // room it never built. Throwing there would rot this harness the way
    // #141 rotted it at #122, and on the default scenario at that. Solid
    // rock is the fiction this world already tells: it walls its own
    // border ring so every Seam band is empty by construction, and a
    // neighbour with no exits keeps it that way — the stub stays the #50
    // baseline's shape rather than growing a cross-room walk it was never
    // built to measure. The `outpost` scenario passes none of this, because
    // there a room the world does not hold really is the harness lying.
    unmodelled: () => ({ get: () => WALL }),
    rooms: [room],
    spawns: [spawn],
    creeps,
    byId,
    perturb: pavingPerturbation({ spare, structures: findTables[107], byId, structure }),
    description: [
      `stub colony in ${ROOM} (2 sources, spawn, controller, ${roads.length} roads, ` +
        `2 containers, ${SITES.length} sites, ${creeps.length} creeps)`,
    ],
    spareTiles: spare.length,
  };
}

// One stub creep. Its `room` is attached by the caller once the room object
// exists, exactly as the spawn's is.
function stubCreep({ name, pos, spec, used, ticksToLive = 1500 }) {
  return {
    id: `creep-${name}`,
    name,
    spawning: false,
    ticksToLive,
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
  };
}

// The census perturbation, one paved tile at a time over a spare lane, and
// the same in both scenarios: pave the next tile, and once the lane is full
// lift them again one by one.
function pavingPerturbation({ spare, structures, byId, structure }) {
  const standing = new Map();
  let next = 0;
  return () => {
    if (!spare.length) throw new Error("--census-every: no unpaved tile left to move the census");
    const pos = spare[next++ % spare.length];
    const key = `${pos.x},${pos.y}`;
    const road = standing.get(key);
    if (road) {
      standing.delete(key);
      byId.delete(road.id);
      structures.splice(structures.indexOf(road), 1);
      return;
    }
    const paved = structure(`road-spare-${key}`, "road", pos);
    standing.set(key, paved);
    structures.push(paved);
  };
}

// ---------------------------------------------------------------------------
// Scenario `outpost`: the colony's own room and its declared neighbours, on
// the committed real terrain.
// ---------------------------------------------------------------------------

// The rooms ADR 0041 names: W12S28 is the colony's, W12S27 lies across its
// north edge and W13S28 across its west edge, and both are the outposts ADR
// 0042 declares. Real terrain rather than synthetic for ADR 0036's reason,
// which is stronger here than it was single-room: a Seam band is a fact
// about two rooms' border rows, and a synthetic room would let the harness
// invent the one number the layering's cost turns on (36 exits north, 19
// west).
const HOME_ROOM = "W12S28";
const OUTPOST_ROOMS = ["W12S27", "W13S28"];
// The tile the live colony's spawn actually stands on — the same one
// RoomInvariantTests sweeps W12S28 over, so the plan this scenario profiles
// is the plan the suite already reasons about.
const HOME_SPAWN = { x: 12, y: 40 };

const capturesDirectory = () =>
  path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "tests", "Core.Tests", "rooms");

// One committed room capture (ADR 0036), read the way RoomFixtures.fs reads
// it: a header, fifty rows of fifty terrain masks, then the furniture. The
// border rows are kept — they are the Seam's own terrain and the projection
// drops them itself (ADR 0041) — so what the engine would answer and what
// this hands back are the same fifty-by-fifty grid.
function loadCapture(roomName) {
  const file = path.join(capturesDirectory(), `${roomName}.room`);
  const lines = readFileSync(file, "utf8").split("\n");

  const sectionAt = (marker) => {
    const at = lines.indexOf(marker);
    if (at < 0) throw new Error(`${file} has no ${marker} section`);
    return at;
  };
  const terrainSection = sectionAt("[terrain]");
  const objectSection = sectionAt("[objects]");

  const rows = lines.slice(terrainSection + 1, terrainSection + 51);
  if (rows.length !== 50 || rows.some((row) => row.length !== 50)) {
    throw new Error(`${file}: terrain is not 50 rows of 50 characters`);
  }
  const data = new Uint8Array(50 * 50);
  for (let y = 0; y < 50; y++) {
    for (let x = 0; x < 50; x++) data[y * 50 + x] = rows[y].charCodeAt(x) - 0x30;
  }

  // The capture's own ids are the engine's, and that is the point: a
  // declaration written in RoomFixtures' readable short names would match
  // nothing a live projection keys by (ADR 0041), so the scenario carries
  // the ids `Outpost.declared` will name.
  const objects = lines
    .slice(objectSection + 2)
    .filter((row) => row.trim() !== "")
    .map((row) => {
      const [id, type, x, y] = row.split("\t");
      return { id, type, pos: { x: Number(x), y: Number(y) } };
    });

  return {
    name: roomName,
    mask: (x, y) => data[y * 50 + x],
    terrain: { get: (x, y) => data[y * 50 + x] },
    sources: objects.filter((o) => o.type === "source"),
    controller: objects.find((o) => o.type === "controller"),
  };
}

const keyOf = (p) => `${p.x},${p.y}`;

function* neighbours(p) {
  for (let dx = -1; dx <= 1; dx++) {
    for (let dy = -1; dy <= 1; dy++) {
      if (dx === 0 && dy === 0) continue;
      const x = p.x + dx;
      const y = p.y + dy;
      // The projection's own window (ADR 0036): the border ring is never
      // ground, so nothing this scenario places may stand on it.
      if (x >= 1 && x <= 48 && y >= 1 && y <= 48) yield { x, y };
    }
  }
}

// The nearest tile to `origin` that is walkable, free, and reachable from
// it across non-wall ground. Walkable and not plain: swamp is ground a
// creep stands on and a container sits on, and W12S28's own controller is
// served off one — excluding it would make the placement fail on real
// terrain for no gain. Reachable rather than merely near: a container
// placed across a wall would be a target nothing can serve, and the
// scenario would measure a colony that cannot work rather than one that
// can. Real terrain is a counterexample generator (ADR 0036), so this
// throws rather than guessing when the room has no such tile.
function nearestFree(capture, origin, taken) {
  const seen = new Set([keyOf(origin)]);
  let frontier = [origin];
  while (frontier.length) {
    const next = [];
    for (const tile of frontier) {
      for (const step of neighbours(tile)) {
        const key = keyOf(step);
        if (seen.has(key)) continue;
        seen.add(key);
        if ((capture.mask(step.x, step.y) & WALL) !== 0) continue;
        if (!taken.has(key)) return step;
        next.push(step);
      }
    }
    frontier = next;
  }
  throw new Error(`${capture.name}: no free tile reachable from ${keyOf(origin)}`);
}

// The shortest walkable route between two tiles, endpoints included — the
// line a trunk road is paved along. A breadth-first search over non-wall
// ground rather than a Chebyshev line, because on real terrain a straight
// line runs through walls and a road on a wall is a road nothing walks.
function route(capture, from, to) {
  const cameFrom = new Map([[keyOf(from), null]]);
  let frontier = [from];
  while (frontier.length) {
    const next = [];
    for (const tile of frontier) {
      if (tile.x === to.x && tile.y === to.y) {
        const path = [];
        for (let at = tile; at; at = cameFrom.get(keyOf(at))) path.unshift(at);
        return path;
      }
      for (const step of neighbours(tile)) {
        const key = keyOf(step);
        if (cameFrom.has(key)) continue;
        if ((capture.mask(step.x, step.y) & WALL) !== 0) continue;
        cameFrom.set(key, tile);
        next.push(step);
      }
    }
    frontier = next;
  }
  throw new Error(`${capture.name}: no walkable route from ${keyOf(from)} to ${keyOf(to)}`);
}

function buildOutpostWorld() {
  const byId = new Map();
  const register = (obj) => {
    byId.set(obj.id, obj);
    return obj;
  };
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

  const home = loadCapture(HOME_ROOM);
  const outposts = OUTPOST_ROOMS.map(loadCapture);

  // --- the home room, furnished on its own terrain -----------------------
  const taken = new Set([keyOf(HOME_SPAWN)]);
  const claim = (pos) => {
    taken.add(keyOf(pos));
    return pos;
  };
  for (const source of home.sources) claim(source.pos);
  claim(home.controller.pos);

  const homeSources = home.sources.map((source) =>
    register({
      id: source.id,
      pos: source.pos,
      energy: 3000,
      ticksToRegeneration: undefined,
    })
  );
  const homeController = register({
    id: home.controller.id,
    my: true,
    level: 3,
    ticksToDowngrade: 9000,
    safeModeAvailable: 1,
    safeMode: undefined,
    pos: home.controller.pos,
    activateSafeMode: ok,
  });

  // Containers where the plan would want them: one beside each source, one
  // beside the controller. Placed by the terrain rather than by hand, so
  // the scenario does not smuggle in a tile the room does not have.
  const containerTargets = [...home.sources.map((s) => s.pos), home.controller.pos];
  const containers = containerTargets.map((target, i) =>
    structure(`cont-${i}`, "container", claim(nearestFree(home, target, taken)), {
      store: store({ used: i === containerTargets.length - 1 ? 800 : 1500, capacity: 2000 }),
    })
  );

  const spawn = structure("spawn-1", "spawn", HOME_SPAWN, {
    store: store({ used: 250, capacity: 300 }),
    hits: 5000,
    hitsMax: 5000,
  });

  // Three extension sites within the cluster the Layout would grow, taken
  // off the ground nearest the spawn that nothing else holds.
  const sites = [0, 1, 2].map((i) =>
    register({
      id: `site-${i}`,
      structureType: "extension",
      pos: claim(nearestFree(home, HOME_SPAWN, taken)),
    })
  );

  // The paved trunks: spawn to every container, along walkable ground.
  const roadTiles = [];
  for (const container of containers) {
    for (const tile of route(home, HOME_SPAWN, container.pos)) {
      if (taken.has(keyOf(tile))) continue;
      claim(tile);
      roadTiles.push(tile);
    }
  }
  // A couple of roads below half hits, so the Repair family is in the
  // measurement instead of pooling zero tasks — the stub scenario's rule.
  const roads = roadTiles.map((pos, i) =>
    structure(`road-${i}`, "road", pos, i % 8 === 3 ? { hits: 2100 } : {})
  );

  const homeFinds = {
    105: homeSources,
    108: [spawn],
    107: [spawn, ...roads, ...containers],
    114: sites,
    103: [],
    106: [],
  };
  const homeRoom = stubRoom({
    name: home.name,
    controller: homeController,
    findTables: homeFinds,
    energy: { available: 250, capacity: 300 },
  });

  // --- the outposts ------------------------------------------------------
  // Vision in both, which is the expensive half: a room we can see is
  // projected entry by entry, and a room we cannot contributes terrain and
  // nothing else (ADR 0004). The worst case is the one worth measuring.
  // Nothing of ours stands in them — an outpost is a room we do not own —
  // beyond the creeps working it, and their containers and roads arrive
  // with ADR 0042 rather than here.
  const outpostRooms = outposts.map((capture) => {
    const sources = capture.sources.map((source) =>
      register({
        id: source.id,
        pos: source.pos,
        energy: 3000,
        ticksToRegeneration: undefined,
      })
    );
    const controller = register({
      id: capture.controller.id,
      my: false,
      level: 0,
      ticksToDowngrade: undefined,
      safeModeAvailable: 0,
      safeMode: undefined,
      pos: capture.controller.pos,
      activateSafeMode: ok,
    });
    return {
      capture,
      sources,
      room: stubRoom({
        name: capture.name,
        controller,
        findTables: { 105: sources, 108: [], 107: [], 114: [], 103: [], 106: [] },
      }),
    };
  });

  // --- the fleet ---------------------------------------------------------
  // The home room's eight, in the live colony's mix of body patterns, plus
  // an Anchor on every outpost source and a hauler per outpost room —
  // thirteen over the three rooms, and the run prints the count. Three
  // short of the roughly sixteen ADR 0041 sizes the layered projection
  // against, and the three missing are the outpost haulers a round trip
  // fifty tiles long will need: how many is ADR 0042's arithmetic, not a
  // number to guess here, so the scenario measures the fleet it can
  // justify and says which one that is.
  const homeCreepDefs = [
    { at: containers[0].pos, spec: { work: 3, carry: 1, move: 1 }, used: 30 },
    { at: containers[1].pos, spec: { work: 3, carry: 1, move: 1 }, used: 50 },
    { at: HOME_SPAWN, spec: { carry: 4, move: 4 }, used: 200 },
    { at: HOME_SPAWN, spec: { carry: 4, move: 4 }, used: 0 },
    { at: HOME_SPAWN, spec: { carry: 4, move: 4 }, used: 100 },
    { at: home.controller.pos, spec: { work: 2, carry: 1, move: 2 }, used: 50 },
    { at: home.controller.pos, spec: { work: 2, carry: 1, move: 2 }, used: 0 },
    { at: sites[0].pos, spec: { work: 2, carry: 2, move: 2 }, used: 60 },
  ];
  const standing = new Set(taken);
  const creeps = [];
  const place = (capture, room, prefix, defs, occupied) => {
    for (const [i, def] of defs.entries()) {
      const pos = nearestFree(capture, def.at, occupied);
      occupied.add(keyOf(pos));
      const creep = register(stubCreep({ ...def, name: `${prefix}-${i}`, pos }));
      creep.room = room;
      creeps.push(creep);
    }
  };
  place(home, homeRoom, "home", homeCreepDefs, standing);
  for (const outpost of outpostRooms) {
    const occupied = new Set([
      ...outpost.sources.map((s) => keyOf(s.pos)),
      keyOf(outpost.room.controller.pos),
    ]);
    const defs = [
      ...outpost.sources.map((source) => ({
        at: source.pos,
        spec: { work: 3, carry: 1, move: 1 },
        used: 40,
      })),
      { at: outpost.sources[0].pos, spec: { carry: 4, move: 4 }, used: 120 },
    ];
    place(outpost.capture, outpost.room, outpost.capture.name.toLowerCase(), defs, occupied);
  }

  Object.assign(spawn, { name: "Spawn1", spawning: null, room: homeRoom, spawnCreep: ok });

  // The spare lane the census perturbation walks: the walkable ground
  // between one container and the next, which no trunk paves — every trunk
  // runs from the spawn. Paving one of its tiles moves the census by
  // exactly one standing structure while the world keeps its size and
  // shape, which is what a perturbed tick is supposed to cost (ADR 0017,
  // ADR 0032). Container to container rather than spawn to source, because
  // a source stands on wall terrain often enough that routing *to* one is a
  // coin toss on real ground — W12S28's own (17,40) is such a tile.
  const paved = new Set(taken);
  const spare = [];
  for (let i = 0; i + 1 < containers.length; i++) {
    for (const tile of route(home, containers[i].pos, containers[i + 1].pos)) {
      const key = keyOf(tile);
      if (paved.has(key)) continue;
      paved.add(key);
      spare.push(tile);
    }
  }
  if (spare.length === 0) {
    throw new Error(`${home.name}: every tile between the containers is already paved`);
  }

  const rooms = [homeRoom, ...outpostRooms.map((o) => o.room)];
  return {
    terrains: new Map([
      [home.name, home.terrain],
      ...outposts.map((capture) => [capture.name, capture.terrain]),
    ]),
    rooms,
    spawns: [spawn],
    creeps,
    byId,
    perturb: pavingPerturbation({ spare, structures: homeFinds[107], byId, structure }),
    description: [
      `outpost colony on real terrain (ADR 0036), ${creeps.length} creeps over ` +
        `${rooms.length} rooms`,
      `  ${home.name} home     ${homeSources.length} sources, controller, ${roads.length} roads, ` +
        `${containers.length} containers, ${sites.length} sites, spawn at ` +
        `${HOME_SPAWN.x},${HOME_SPAWN.y}`,
      ...outpostRooms.map(
        (outpost) =>
          `  ${outpost.capture.name} outpost  ${outpost.sources.length} source` +
          `${outpost.sources.length === 1 ? "" : "s"}, controller, vision`
      ),
    ],
    spareTiles: spare.length,
  };
}

const buildWorld = () => (scenario === "outpost" ? buildOutpostWorld() : buildStubWorld());

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

function printReport(classes, pooled, world, allTicks) {
  console.log(
    `fabot profile — ${scenario} scenario, ${TICKS} ticks, ` +
      world.description[0] +
      (CENSUS_EVERY
        ? `, census moved every ${CENSUS_EVERY} ticks over a ${world.spareTiles}-tile lane`
        : "")
  );
  for (const detail of world.description.slice(1)) console.log(detail);

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

  // ADR 0041's condition to revisit the layered projection, over every tick
  // of the run whatever class it fell in: the engine charges one budget and
  // does not care which of our ticks recomputed the plan. Printed on every
  // run, tripped or not — a trigger only visible when it fires is a feeling
  // again, which is the thing the ADR replaced.
  //
  // Read against this harness's clock, which is a floor and not the
  // server's: the same caveat the ms/tick above carries (engine-side costs
  // unsimulated, developer hardware) applies to the verdict read off them,
  // and #81 measured the live colony at ~21 ms against this scenario's
  // handful. So a "not triggered" here is the harness failing to trip the
  // trigger, never the colony clearing it. The reading that decides is
  // `npm run observe cpu` over the deployed bundle's own
  // `Game.cpu.getUsed()`; both are judged off the same two thresholds in
  // `scripts/cpu-trigger.mjs`, and only one of them is authoritative.
  console.log(`\n${cpuReport(allTicks)}`);
  console.log(
    "  (this harness's clock, a floor: engine-side costs are not simulated — " +
      "`npm run observe cpu` reads the trigger off the deployed bundle)"
  );

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
  console.log("");

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

const world = buildWorld();
const { game, terrainReads } = buildGame(world);
globalThis.Game = game;
globalThis.Memory = {};

// Harness self-check, before a tick is ever run: the terrain query answers
// by room name. Each room is counted by its own wall tiles read back
// through `Game.map.getRoomTerrain`, and two rooms answering the same count
// would mean the argument is being ignored — the defect this scenario
// exists to rule out. Validate the harness before trusting it: a
// single-terrain stub would have measured the outpost scenario as the home
// room three times over and said nothing.
const wallsOf = (roomName) => {
  const terrain = game.map.getRoomTerrain(roomName);
  let walls = 0;
  for (let x = 0; x < 50; x++) {
    for (let y = 0; y < 50; y++) if ((terrain.get(x, y) & WALL) !== 0) walls++;
  }
  return walls;
};
const worldRooms = [...terrainReads.keys()];
const wallCounts = worldRooms.map((name) => [name, wallsOf(name)]);
if (worldRooms.length > 1 && new Set(wallCounts.map(([, n]) => n)).size !== worldRooms.length) {
  console.error(
    "the stub's terrain query answers the same grid for two rooms — it is ignoring its argument:\n" +
      wallCounts.map(([name, walls]) => `  ${name}  ${walls} wall tiles`).join("\n")
  );
  process.exit(1);
}
// The self-check's own reads are not the bot's, so the counters start the
// run at zero and still answer "how many times did the bundle read each
// room's terrain" (ADR 0031).
for (const name of worldRooms) terrainReads.set(name, 0);

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
  if (moved) world.perturb();
  tickStart = performance.now();
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
  if (moved) world.perturb();
  const run = tickThrough(moved);
  const start = performance.now();
  tickStart = start;
  run();
  const ms = performance.now() - start;
  ticks.all.push({ t: game.time, ms });
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
  : [{ label: "all", ms: ticks.all.map((row) => row.ms), summary: pooled }];

printReport(classes, pooled, world, ticks.all);

// Per room, because ADR 0041 layered the memo by room name: the number to
// read is one read per room the bundle projected, over the whole run. Read
// off the counters rather than off the modelled rooms, so a room the
// scenario did not build and the bundle asked for anyway — a declared
// outpost the `stub` world answers as solid rock — is counted where a
// reader can see it.
console.log(
  `engine terrain reads over ${WARMUP + TICKS} ticks (Game.map.getRoomTerrain): ` +
    [...terrainReads]
      .map(([name, reads]) => `${name} ${reads}${worldRooms.includes(name) ? "" : " (unmodelled)"}`)
      .join(", ")
);

// The self-check's evidence, printed rather than only asserted: a wall
// count is a cheap fingerprint of a fifty-by-fifty grid, so distinct counts
// are the harness saying out loud that the query read its argument.
console.log(
  "terrain query answers by room name: " +
    wallCounts.map(([name, walls]) => `${name} ${walls} wall tiles`).join(", ") +
    (worldRooms.length > 1 ? " (all distinct)" : "")
);

// The observe channel's CPU line as the bundle itself wrote it (ADR 0041),
// beside the harness's own clock above — the only place the channel can be
// seen without a deploy. The two measure a tick from opposite sides,
// `Game.cpu.getUsed()` inside the loop against `performance.now()` around
// it, so the comparison is worth something only over the same ticks: the
// ring carries the unprofiled warm-up rows too, and the bundle caps it, so
// the line is cut to the profiled window before its mean is taken and the
// rows outside are counted rather than judged. What is then the channel
// broken is a line that is absent, one that misses profiled ticks it
// should hold, or one whose mean is wildly apart from the ms/tick above —
// none of which a healthy run can produce by arithmetic.
const cpuLine = globalThis.Memory?.fabot?.observe?.cpu?.ticks;
const firstProfiled = ticks.all[0].t;
const lastProfiled = ticks.all[ticks.all.length - 1].t;
const inWindow = Array.isArray(cpuLine)
  ? cpuLine.filter((row) => row.t >= firstProfiled && row.t <= lastProfiled)
  : [];
if (!Array.isArray(cpuLine) || cpuLine.length === 0) {
  console.log(
    "observe CPU line (Memory.fabot.observe.cpu): absent — this bundle does not write it"
  );
} else if (inWindow.length === 0) {
  console.log(
    `observe CPU line (Memory.fabot.observe.cpu): ${cpuLine.length} rows and not one of them a ` +
      `profiled tick (t${firstProfiled}-t${lastProfiled}) — the channel is broken`
  );
} else {
  const mean = inWindow.reduce((total, row) => total + row.ms, 0) / inWindow.length;
  const outside = cpuLine.length - inWindow.length;
  console.log(
    `observe CPU line (Memory.fabot.observe.cpu): ${inWindow.length} of the run's ${TICKS} ` +
      `profiled ticks, t${inWindow[0].t}-t${inWindow[inWindow.length - 1].t}, ` +
      `mean ${mean.toFixed(2)} ms` +
      (inWindow.length < TICKS
        ? ` — short because the bundle's ring holds ${cpuLine.length} rows in all, so the run's ` +
          "oldest ticks have already fallen off the front of it"
        : "") +
      (outside ? `; ${outside} more rows in the ring, outside the window, not compared` : "")
  );
}

// What the bundle actually projected, against what the world holds. With
// `Outpost.declared` empty (#124 landed it so, ADR 0042/#126 fills it) the
// scan set is the spawn room alone, so an outpost run measures the world's
// rooms and the home room's projection — the harness is ready for the
// declaration, and the numbers are not two rooms' yet. Said out loud rather
// than left to be inferred from a read count nobody compares.
const projected = wallCounts.filter(([name]) => terrainReads.get(name) > 0).map(([name]) => name);
const unprojected = worldRooms.filter((name) => !projected.includes(name));
if (unprojected.length) {
  console.log(
    `projection: the bundle read terrain for ${projected.join(", ") || "no room"} and never for ` +
      `${unprojected.join(", ")} — those rooms are in the world but outside the scan set, ` +
      "which is `Outpost.declared` standing empty (ADR 0041 ships the capability, ADR 0042 " +
      "fills the constant). These ms are one room's projection, not the layered one's."
  );

  // And the creeps standing in those rooms are worse than unmeasured, so
  // the number above says how many. `Snapshot.Creeps` is taken from
  // `Game.creeps` world-wide while `CreepPositions` is filtered per
  // projected room (ADR 0041), so a creep in an unprojected room joins the
  // Task pool with no tile anywhere in the projection — and the Matcher
  // assigns it a home-room Task and the Executor emits the work against a
  // target fifty tiles away in another room, with no move. That is a
  // decision the colony never makes, priced into the ms above. Said out
  // loud rather than worked around: what the decision layer owes a creep
  // it cannot place is ADR 0004's question and Core's to answer, and this
  // ticket adds observation and changes no Core.
  const stray = world.creeps.filter((creep) => unprojected.includes(creep.room.name));
  if (stray.length) {
    console.log(
      `  and ${stray.length} of the world's ${world.creeps.length} creeps stand in those rooms ` +
        `(${stray.map((creep) => creep.name).join(", ")}): matched off a position the projection ` +
        "does not hold, so their share of these ms is a match the colony would not make."
    );
  }
}

console.log(`\nraw profile: ${path.relative(process.cwd(), profilePath)} (open in Chrome DevTools / speedscope)`);
