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
// Four scenarios, chosen with --scenario:
//   stub     one synthetic room (the default), the shape this harness has
//            always measured
//   outpost  the colony's own room and its declared neighbours, on the
//            committed real terrain (ADR 0036) — the world ADR 0041's
//            layered projection is sized against
//   young    one colony at the other end of its life (ADR 0052): W13S28's
//            captured terrain, RCL1, a 300 bank, a container on each of
//            its two sources and nothing else standing — no road and no
//            rampart, because a room under `Colony.bootstrapLevel` places
//            neither (#209, #214). Every body in it is what
//            `Decide.bodyFor` casts at 300.
//   pair     two colonies in one tick (ADR 0047, ADR 0052): the mother
//            W12S28 at RCL5 with her W12S27 outpost — the `outpost`
//            scenario's shape — and beside her the child W13S28,
//            bootstrapping with its own Spawn2 standing at 16,12, which
//            the mother still projects while the child is under
//            `bootstrapLevel` (#192). `decide` runs once per living
//            colony, exactly as `Main.loop` runs it, and the report
//            prints one CPU row per colony's `decide` beside the total.
//
// Every scenario is built at a controller level, `--level N`, and
// everything that hangs off the level is derived from it rather than
// written down twice: the extension, tower and Storage counts off the
// engine's CONTROLLER_STRUCTURES table, the energy bank off the extensions
// that table allows, and the fleet off the bundle's own SpawnCreep
// intents. So moving a colony a level is one number on the command line,
// and the scenarios follow the live rooms instead of ageing behind them.
//
// Which colony `--level` moves is the scenario's own answer, because a
// scenario with two colonies in it has two levels and only one flag:
//   stub, outpost  the one colony, default RCL5 — where the live mother
//                  stands
//   young          the one young colony, default RCL1 — the level the
//                  live child was claimed at
//   pair           the **child** colony, default RCL2 — where the live
//                  W13S28 stands. The mother is pinned at RCL5 by
//                  `MOTHER_LEVEL`, because "an RCL5 mother" is half of
//                  what this scenario is. `--level 3` and up is a
//                  deliberate reading rather than a mistake: at
//                  `Colony.bootstrapLevel` the bootstrap window closes,
//                  the mother stops projecting the child's room and the
//                  two colonies run side by side with nothing shared.

import { createRequire } from "node:module";
import { Session } from "node:inspector/promises";
import { performance } from "node:perf_hooks";
import { writeFileSync, readFileSync, mkdirSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";
import { report as cpuReport } from "./cpu-trigger.mjs";

const SCENARIOS = ["stub", "outpost", "young", "pair"];
const USAGE =
  "usage: npm run profile -- [ticks] [top-N] [--census-every N]" +
  " [--scenario stub|outpost|young|pair] [--level 1..8]  (positive integers)";

// The controller level a scenario's colony is built at, and the one number
// to move when it climbs: every count that follows from it — the
// extension, tower and Storage allowances, the energy bank they add up to,
// and through the bank the bodies and the Workforce target the fleet is
// hired against — is derived below, never written down a second time. The
// default tracks the live room the scenario is about (#144: the mother at
// RCL5; ADR 0052: the child where it stands today); `--level N` builds any
// other, which is what a comparison against an older run costs now.
//
// Which colony the flag moves is the scenario's, and the header comment
// spells it: for `pair` it is the child, and the mother stays at
// MOTHER_LEVEL below.
const DEFAULT_LEVEL = { stub: 5, outpost: 5, young: 1, pair: 2 };

// The mother's level in the `pair` scenario. Pinned rather than flagged:
// ADR 0052's scenario is "an RCL5 mother with a bootstrapping child", and
// a run that moved both ends at once would answer neither question.
const MOTHER_LEVEL = 5;

// Positional [ticks] [top-N] as before, with --census-every N,
// --scenario NAME and --level N pulled out from anywhere in the line.
const positional = [];
let censusEvery = 0; // absent: the frozen world, and no perturbation report
let scenario = "stub";
// Absent until the line says otherwise, because the default is the
// scenario's and the scenario may be named after the flag.
let level = null;
// The raw token beside the parsed number, so a rejection quotes what was
// typed rather than the `NaN` the conversion made of it — the answer the
// sibling `--scenario` check already gives.
let levelArg = null;
for (let i = 2; i < process.argv.length; i++) {
  if (process.argv[i] === "--census-every") censusEvery = Number(process.argv[++i]);
  else if (process.argv[i] === "--scenario") scenario = process.argv[++i];
  else if (process.argv[i] === "--level") {
    levelArg = process.argv[++i];
    level = Number(levelArg);
  }
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
// The scenario's own default, resolved after the line is read: a young
// colony's level is not a mother's, and neither is a bootstrapping
// child's.
const LEVEL = level ?? DEFAULT_LEVEL[scenario];
if (levelArg === null) levelArg = String(LEVEL);
if (notPositive(LEVEL) || LEVEL > 8) {
  console.error(`--level must be a controller level, 1 to 8; got "${levelArg}"\n${USAGE}`);
  process.exit(1);
}
const WARMUP = 3; // unprofiled JIT warm-up ticks
const SAMPLE_INTERVAL_US = 100;

const WALL = 1;
const SWAMP = 2;

// ---------------------------------------------------------------------------
// The engine's level table: what a controller level lets a room hold.
// ---------------------------------------------------------------------------

// Screeps CONTROLLER_STRUCTURES, indexed by RCL, for the three kinds a
// level actually moves in this harness. The same numbers Core spells in
// `Decide.extensionAllowance` / `towerAllowance` / `storageAllowance` —
// copied rather than shared because those are `private` to Core and this
// script drives the compiled bundle, not the F# assembly. Copied *whole*,
// as a table, so a scenario asks the level for a count instead of a reader
// hand-checking one: the RCL3 numbers the two scenarios used to carry as
// literals were three levels stale before anyone noticed (#144).
const CONTROLLER_STRUCTURES = {
  extension: [0, 0, 5, 10, 20, 30, 40, 50, 60],
  tower: [0, 0, 0, 1, 1, 2, 2, 3, 6],
  storage: [0, 0, 0, 0, 1, 1, 1, 1, 1],
};

// Screeps EXTENSION_ENERGY_CAPACITY by RCL, and SPAWN_ENERGY_CAPACITY: the
// bank is the spawn plus the extensions that stand, which is what the
// engine's `energyCapacityAvailable` reports and what every body in ADR
// 0006's pattern table is sized against.
const EXTENSION_ENERGY_CAPACITY = [50, 50, 50, 50, 50, 50, 50, 100, 200];
const SPAWN_ENERGY_CAPACITY = 300;

// The stores the harness fills: TOWER_CAPACITY, STORAGE_CAPACITY and
// CONTAINER_CAPACITY as the engine spells them.
const TOWER_CAPACITY = 1000;
const STORAGE_CAPACITY = 1000000;
const CONTAINER_CAPACITY = 2000;

// Extensions left as construction sites rather than built, in both
// scenarios, so the Build family is in the measurement instead of pooling
// zero tasks — the same reason a few roads stand below half hits. Held
// back out of the level's allowance rather than added on top of it, so the
// room holds exactly what the level allows however the level moves — the
// engine counts a site against CONTROLLER_STRUCTURES too, so 30 built plus
// 3 pending is a room RCL5 cannot hold. The bank pays for it: 27 standing
// extensions report 1650, not the 1800 a finished RCL5 would, and every
// body ADR 0006 casts is sized against that (#144, recorded in the README).
const EXTENSION_SITES = 3;

// What a level lets the home room hold, and the bank that follows from it.
// Every count a scenario furnishes comes from here.
function furnitureFor(rcl) {
  const allowed = CONTROLLER_STRUCTURES.extension[rcl];
  const pending = Math.min(EXTENSION_SITES, allowed);
  const built = allowed - pending;
  return {
    extensions: built,
    extensionSites: pending,
    towers: CONTROLLER_STRUCTURES.tower[rcl],
    storages: CONTROLLER_STRUCTURES.storage[rcl],
    bank: SPAWN_ENERGY_CAPACITY + built * EXTENSION_ENERGY_CAPACITY[rcl],
  };
}

const FURNITURE = furnitureFor(LEVEL);

const plural = (n, word) => `${n} ${word}${n === 1 ? "" : "s"}`;

// The level's furniture in one phrase, printed by every scenario, so the
// rooms are read against the same list and a level that furnished them
// differently would be visible in the report rather than only in the ms.
// It takes the furniture rather than reading the flag, because since ADR
// 0052's `pair` scenario one run can furnish two rooms at two levels.
const furnitureLine = (furniture = FURNITURE) =>
  `${plural(furniture.extensions, "extension")}, ${plural(furniture.towers, "tower")}, ` +
  `${furniture.storages} storage, ${furniture.bank} energy bank`;

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

// The username this colony holds its rooms under. One name for the whole
// harness, because `Snapshot` reads it once off the spawn room's
// controller and then decides whose every reservation is by comparing
// against it (ADR 0042): a scenario that spelled the owner and the
// reserver's username differently would hand the bundle two outposts held
// by a rival, and a rival's hold is a room the colony withdraws from
// rather than mines.
const COLONY_OWNER = "fabot";

// Screeps CARRY_CAPACITY: what one Carry part holds.
const CARRY_CAPACITY = 50;

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
// Room geometry, shared by both scenarios. A `grid` here is the least a
// placement needs: a room name to blame in an error, and a terrain mask
// read by coordinate. Both scenarios hand one over — the synthetic room
// and the committed capture alike — so the furnishing rules below are
// written once and neither room gets a placement rule the other does not.
// ---------------------------------------------------------------------------

const keyOf = (p) => `${p.x},${p.y}`;

// A row's stations: where a hired creep of that row is stood, each tile
// paired with the room it lies in and that room's grid. A station is a
// room and a tile and not a bare coordinate, because two of ADR 0042's
// rows are cast by the home spawn and work a room away — the reserver
// walks to an outpost's controller and holds the reservation there, and
// an outpost Post's Anchor stands on its container — so "where this row
// stands" stopped being answerable inside one room the tick #131 landed.
// Every other row still hands its own room over, and reads the same.
const stationsIn = (room, grid, positions) => positions.map((pos) => ({ room, grid, pos }));

// The same, for a row that holds a *place* rather than pooling near one:
// the body stands on the tile itself and is never resolved outward from
// it. The Anchor row is the whole of it today — Harvest's Work Area for a
// work-heavy body is its Posts and nothing else (ADR 0020, ADR 0048), so
// the tile beside a Post is a tile that row cannot work from — and a Post
// is walkable ground it may stand on, which is what makes standing there
// expressible at all (ADR 0051). `hireFleet` still refuses to put two
// bodies on one tile.
const stationsOn = (room, grid, positions) =>
  positions.map((pos) => ({ room, grid, pos, onTile: true }));

// The tiles already claimed in one room, by name. One set per room and
// never one flat set over the world: every room has the same 2,500
// coordinates, so a reserver beside W12S27's controller at 36,44 and a
// worker on W12S28's 36,44 are two creeps on one key, and the second
// would be pushed off a tile nothing stands on. The scenario builds the
// sets — it is the half that knows what its rooms already hold — and a
// room with none is the harness stationing a creep in a room it never
// furnished, which throws rather than guesses (ADR 0027).
function claimsIn(world, roomName) {
  const claimed = world.claimed.get(roomName);
  if (!claimed) {
    throw new Error(
      `the ${scenario} scenario holds no claimed tiles for ${roomName} ` +
        `(it knows: ${[...world.claimed.keys()].join(", ")}), so a creep stationed there ` +
        "would be stood on a tile the world may already be using"
    );
  }
  return claimed;
}

// The station table one spawn's casts are stood out of. A scenario with
// one colony in it has one table and hands it over as `stations`; a
// scenario with two — ADR 0047's world, which is now every live tick —
// files a table per spawn under `stationsBySpawn`, because "where the
// worker row stands" is a different tile in the mother's room and in the
// child's. A spawn the scenario furnished no table for throws rather than
// falling back to another colony's tiles (ADR 0027).
function stationsFor(world, spawnName) {
  if (!world.stationsBySpawn) return world.stations;
  const table = world.stationsBySpawn[spawnName];
  if (!table) {
    throw new Error(
      `the ${scenario} scenario stations no row for ${spawnName} (it knows: ` +
        `${Object.keys(world.stationsBySpawn).join(", ")}), so a body cast there would be stood ` +
        "in another colony's room"
    );
  }
  return table;
}

// One row of the hired fleet read back as `room x,y` per body — where the
// row actually ended up, which is `stations` resolved through
// `nearestFree` and not the tile that was asked for. Both scenarios print
// two of these (see their `describe`), because the fleet line above counts
// a row's bodies and never says which tile they hold, and for two rows
// that difference is the whole point of the seat: a reserver parked on the
// spawn's doorstep and one holding an outpost's controller are the same
// number in every other count the report prints, and an upgrader beside
// the buffer and one anywhere else are a body that works standing still
// against a body on a walk it never finishes in a frozen world (ADR 0046).
// `spawnName` narrows it to one colony's cast, which a two-colony
// scenario needs: both colonies hire a worker row, and a line that read
// the row world-wide would report the mother's tiles and the child's as
// one row of one colony.
const stationsOf = (creeps, row, spawnName = null) =>
  creeps
    .filter((creep) => creep.name.split("-")[0] === row)
    .filter((creep) => spawnName === null || creep.name.split("-")[2] === spawnName)
    .map((creep) => `${creep.room.name} ${keyOf(creep.pos)}`);

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

const isWall = (grid, p) => (grid.mask(p.x, p.y) & WALL) !== 0;

// The room's working ground (ADR 0022): every source's Seats — the
// walkable neighbours of its tile — plus the controller's Upgrade Work
// Area, the walkable tiles within Chebyshev range 3 of it. The same two
// sets `Atlas.workingGround` unions, and the one exclusion the clustered
// ordering carries: "a clustered structure there eats a tile an Anchor or
// an upgrader stands on, and nothing a tower or extension does is worth
// that". Without it the harness furnishes a room its own Layout would
// never build, and the higher the level the further out of true it goes:
// on W12S28 the RCL5 cluster stood on 8,38 / 8,40 / 8,42 — three of the
// Upgrade tiles that ADR 0022 was decided on — and one level higher it
// took the east source's Seat at 17,39 and pushed the Anchor off it.
function workingGround(grid, sourcePositions, controllerPos) {
  const ground = new Set();
  const reserve = (tile) => {
    if (tile.x < 1 || tile.x > 48 || tile.y < 1 || tile.y > 48) return;
    if (!isWall(grid, tile)) ground.add(keyOf(tile));
  };
  for (const source of sourcePositions) for (const tile of neighbours(source)) reserve(tile);
  for (let dx = -3; dx <= 3; dx++) {
    for (let dy = -3; dy <= 3; dy++) {
      reserve({ x: controllerPos.x + dx, y: controllerPos.y + dy });
    }
  }
  return ground;
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
function nearestFree(grid, origin, taken) {
  const seen = new Set([keyOf(origin)]);
  let frontier = [origin];
  while (frontier.length) {
    const next = [];
    for (const tile of frontier) {
      for (const step of neighbours(tile)) {
        const key = keyOf(step);
        if (seen.has(key)) continue;
        seen.add(key);
        if (isWall(grid, step)) continue;
        if (!taken.has(key)) return step;
        next.push(step);
      }
    }
    frontier = next;
  }
  throw new Error(`${grid.name}: no free tile reachable from ${keyOf(origin)}`);
}

// The tiles the room's cluster of extensions, towers and Storage stands
// on: ground spiralling out from the spawn, taking only the tiles whose
// x+y parity matches the spawn's.
//
// The parity is load-bearing, not tidiness. Extensions, towers and the
// Storage are all in the engine's OBSTACLE_OBJECT_TYPES, so thirty of them
// packed nearest-first around a spawn would wall the spawn in, and this
// harness would profile a colony whose every creep is fenced off from its
// own room — a decision the colony never makes, timed as if it did. Taking
// one parity leaves the complementary tiles as a lane lattice that is
// connected diagonally and touches every structure orthogonally, which is
// the checkerboard a real clustered plan leaves (ADR 0039) and the shape
// this room's own Layout would grow. `reserved` is the room's working
// ground, which the ordering steps over rather than builds on (ADR 0022).
// Throws rather than shrinking the cluster: a room that cannot hold its
// level's furniture is the harness lying about which level it profiled.
function clusterTiles(grid, spawnPos, count, taken, reserved, rcl) {
  const parity = (spawnPos.x + spawnPos.y) % 2;
  const tiles = [];
  const seen = new Set([keyOf(spawnPos)]);
  let frontier = [spawnPos];
  while (frontier.length && tiles.length < count) {
    const next = [];
    for (const tile of frontier) {
      for (const step of neighbours(tile)) {
        const key = keyOf(step);
        if (seen.has(key)) continue;
        seen.add(key);
        if (isWall(grid, step)) continue;
        next.push(step);
        if (reserved.has(key)) continue;
        if ((step.x + step.y) % 2 === parity && !taken.has(key) && tiles.length < count) {
          tiles.push(step);
        }
      }
    }
    frontier = next;
  }
  if (tiles.length < count) {
    throw new Error(
      `${grid.name}: only ${tiles.length} of the ${count} cluster tiles RCL${rcl} needs are ` +
        `reachable from the spawn at ${keyOf(spawnPos)}`
    );
  }
  return tiles;
}

// The shortest walkable route between two tiles, endpoints included — the
// line a trunk road is paved along. A breadth-first search over non-wall
// ground rather than a Chebyshev line, because on real terrain a straight
// line runs through walls and a road on a wall is a road nothing walks.
// `blocked` carries the cluster's obstacle tiles: a trunk that ran through
// a standing extension would be a road no creep can walk either, and the
// lane lattice the cluster leaves is what it weaves along instead.
function route(grid, from, to, blocked) {
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
        if (isWall(grid, step) || blocked.has(key)) continue;
        cameFrom.set(key, tile);
        next.push(step);
      }
    }
    frontier = next;
  }
  throw new Error(`${grid.name}: no walkable route from ${keyOf(from)} to ${keyOf(to)}`);
}

// The level's cluster, placed: the built extensions, the towers, the
// Storage and the extension sites the level allows, on one run of
// `clusterTiles` so the four kinds share the room's one checkerboard.
// Stores are the state a colony at rest sits in — the bank full, because
// a bank below a body's cost is a colony that cannot cast one and the
// fleet below is hired from what the bundle asks for; the towers part
// drained, because they are the refillable kind whose store is not the
// bank, so the Refill family stays in the measurement without lying about
// `energyCapacityAvailable`; the Storage stocked, so ADR 0023's Withdraw
// tier pools it.
//
// `taken` is the one set of claimed tiles: the cluster reads it for what
// the room already holds and writes its own picks back into it, so a
// caller cannot hand it a claim over some other set and get two
// structures on one tile.
function placeCluster({
  grid,
  spawnPos,
  sourcePositions,
  controllerPos,
  taken,
  structure,
  register,
  // The room's own level, because one run can furnish two rooms at two
  // levels since ADR 0052's `pair` scenario: the flag's level is the
  // default and the mother passes her own.
  rcl = LEVEL,
  // What every id this cluster registers is prefixed with. One world can
  // now furnish two rooms, and an object id is unique across the world —
  // a second room's `ext-0` would land on the first room's in
  // `getObjectById` and the projection would key one structure twice.
  prefix = "",
}) {
  const furniture = furnitureFor(rcl);
  const { extensions, extensionSites, towers, storages } = furniture;
  const wanted = extensions + towers + storages + extensionSites;
  const reserved = workingGround(grid, sourcePositions, controllerPos);
  const tiles = clusterTiles(grid, spawnPos, wanted, taken, reserved, rcl);
  let at = 0;
  const take = () => {
    const pos = tiles[at++];
    taken.add(keyOf(pos));
    return pos;
  };

  const built = [];
  for (let i = 0; i < extensions; i++) {
    built.push(
      structure(`${prefix}ext-${i}`, "extension", take(), {
        store: store({
          used: EXTENSION_ENERGY_CAPACITY[rcl],
          capacity: EXTENSION_ENERGY_CAPACITY[rcl],
        }),
      })
    );
  }
  for (let i = 0; i < towers; i++) {
    built.push(
      structure(`${prefix}tower-${i}`, "tower", take(), {
        store: store({ used: TOWER_CAPACITY / 2, capacity: TOWER_CAPACITY }),
        hits: 3000,
        hitsMax: 3000,
      })
    );
  }
  for (let i = 0; i < storages; i++) {
    built.push(
      structure(`${prefix}storage-${i}`, "storage", take(), {
        store: store({ used: STORAGE_CAPACITY / 5, capacity: STORAGE_CAPACITY }),
        hits: 10000,
        hitsMax: 10000,
      })
    );
  }

  const sites = [];
  for (let i = 0; i < extensionSites; i++) {
    sites.push(register({ id: `${prefix}site-${i}`, structureType: "extension", pos: take() }));
  }

  return { built, sites, furniture };
}

// ---------------------------------------------------------------------------
// Scenario `stub`: a deterministic in-memory room shaped like the live
// colony — 2 sources, spawn, controller, trunk roads, source + controller
// containers, and the level's own cluster and fleet.
// ---------------------------------------------------------------------------

const ROOM = "W1N1";

const SPAWN_POS = { x: 25, y: 25 };
const SOURCE_A = { x: 11, y: 14 };
const SOURCE_B = { x: 38, y: 39 };
const CONTROLLER = { x: 8, y: 33 };
const CONTAINER_A = { x: 12, y: 15 }; // source container, beside source A
const CONTAINER_B = { x: 9, y: 32 }; // controller container
// The paved trunks; buildStubGrid also carves an unpaved lane to source B
// so the room is connected everywhere the bot expects to reach.
const TRUNKS = [
  [SPAWN_POS, CONTAINER_A],
  [SPAWN_POS, CONTAINER_B],
];

function buildStubGrid() {
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
  for (const p of [SPAWN_POS, SOURCE_A, SOURCE_B, CONTROLLER, CONTAINER_A, CONTAINER_B]) {
    carve(p);
  }
  for (const [a, b] of [...TRUNKS, [SPAWN_POS, SOURCE_B]]) {
    for (const p of line(a, b)) carve(p);
  }

  // Shaped like `loadCapture`'s room below — a name, a mask read by
  // coordinate, and the terrain object the engine hands back — so the
  // placement rules in the geometry section above serve both scenarios
  // off one shape rather than one each.
  return {
    name: ROOM,
    mask: (x, y) => data[y * 50 + x],
    terrain: { get: (x, y) => data[y * 50 + x] },
  };
}

function buildStubWorld() {
  const byId = new Map();
  const register = (obj) => {
    byId.set(obj.id, obj);
    return obj;
  };

  const grid = buildStubGrid();

  const sources = [SOURCE_A, SOURCE_B].map((pos, i) =>
    register({ id: `src-${i}`, pos, energy: 3000, ticksToRegeneration: undefined })
  );

  const controller = register({
    id: "ctrl",
    my: true,
    // The name the engine spells this colony, which `Snapshot` reads off
    // the spawn room's controller and off nothing else: it is the name
    // every reservation is compared against (ADR 0042), so a home
    // controller with no owner leaves the colony nameless and a
    // reservation of our own reading as a rival's. Nothing in this
    // one-room world is reserved, but the two rooms of the `outpost`
    // scenario are, and both rooms answer the same shape.
    owner: { username: COLONY_OWNER },
    level: LEVEL,
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

  // Every tile something of the colony's already stands on. Claimed in the
  // order the room is furnished — the fixed points, then the level's
  // cluster, then the trunks that weave through what the cluster left.
  const taken = new Set(
    [SPAWN_POS, SOURCE_A, SOURCE_B, CONTROLLER, CONTAINER_A, CONTAINER_B].map(keyOf)
  );
  const claim = (pos) => {
    taken.add(keyOf(pos));
    return pos;
  };

  const containers = [
    structure("cont-src", "container", CONTAINER_A, {
      store: store({ used: 1500, capacity: CONTAINER_CAPACITY }),
    }),
    structure("cont-ctrl", "container", CONTAINER_B, {
      store: store({ used: 800, capacity: CONTAINER_CAPACITY }),
    }),
  ];

  // One object serves as structure (find tables), spawn (Game.spawns), and
  // getObjectById target; the spawn-specific fields are attached below once
  // the room exists. Its store is full for the same reason the extensions'
  // are: the bank is what the fleet below is cast from.
  const spawn = structure("spawn-1", "spawn", SPAWN_POS, {
    store: store({ used: SPAWN_ENERGY_CAPACITY, capacity: SPAWN_ENERGY_CAPACITY }),
    hits: 5000,
    hitsMax: 5000,
  });

  const cluster = placeCluster({
    grid,
    spawnPos: SPAWN_POS,
    sourcePositions: [SOURCE_A, SOURCE_B],
    controllerPos: CONTROLLER,
    taken,
    structure,
    register,
  });

  // Trunk roads: spawn → source container and spawn → controller container,
  // walked around the cluster's obstacles rather than straight through
  // them, and skipping tiles already holding a structure, site, or endpoint.
  const blocked = new Set(cluster.built.concat(cluster.sites).map((s) => keyOf(s.pos)));
  const roadTiles = [];
  for (const [a, b] of TRUNKS) {
    for (const p of route(grid, a, b, blocked)) {
      if (taken.has(keyOf(p))) continue;
      claim(p);
      roadTiles.push(p);
    }
  }
  // A couple of roads below half hits, so the Repair family is in the
  // measurement instead of pooling zero tasks.
  const roads = roadTiles.map((pos, i) =>
    structure(`road-${i}`, "road", pos, i % 8 === 3 ? { hits: 2100 } : {})
  );

  const findTables = {
    105: sources, // FIND_SOURCES
    108: [spawn, ...cluster.built], // FIND_MY_STRUCTURES (ours: refillables, the Keep)
    107: [spawn, ...cluster.built, ...roads, ...containers], // FIND_STRUCTURES
    114: cluster.sites, // FIND_MY_CONSTRUCTION_SITES
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
  const spare = route(grid, SPAWN_POS, SOURCE_B, blocked).filter((p) => !taken.has(keyOf(p)));

  const room = stubRoom({
    name: ROOM,
    controller,
    findTables,
    energy: { available: FURNITURE.bank, capacity: FURNITURE.bank },
  });

  Object.assign(spawn, { name: "Spawn1", spawning: null, room, spawnCreep: ok });

  const creeps = [];

  return {
    terrains: new Map([[ROOM, grid.terrain]]),
    // A room this scenario does not model, answered as solid rock. The
    // scan set is the spawn room plus every declared outpost (ADR 0041),
    // and `Snapshot.projectRoom` reads terrain for all of them whether or
    // not there is vision — so the tick the colony's declared outposts
    // (`Colony.declared`) stop being empty (ADR 0042, #126) is the tick a
    // one-room stub is asked for a room it never built. Throwing there would rot this harness the way
    // #141 rotted it at #122, and on the default scenario at that. Solid
    // rock is the fiction this world already tells: it walls its own
    // border ring so every Seam band is empty by construction, and a
    // neighbour with no exits keeps it that way — the stub stays one room
    // rather than growing a cross-room walk it was never built to measure.
    // The `outpost` scenario passes none of this, because there a room the
    // world does not hold really is the harness lying.
    unmodelled: () => ({ get: () => WALL }),
    rooms: [room],
    spawns: [spawn],
    creeps,
    byId,
    perturb: pavingPerturbation({ spare, structures: findTables[107], byId, structure }),
    // Where a hired creep is stationed, by the row the bundle cast it from
    // (see `hireFleet`): an Anchor at a source, a hauler at the spawn it
    // shuttles from, a worker at the controller or a site. A fleet born on
    // the spawn's doorstep would spend the run walking and the profile
    // would time a colony in transit rather than one at work.
    stations: {
      // The reserver row stands at the spawn — beside it, in fact, since
      // `nearestFree` resolves a station to a walkable tile the world is
      // not already using — and this scenario cannot do better: the rooms
      // it was cast to reserve are the declared outposts, which this world
      // does not model and answers as solid rock (above), so its Reserve
      // target is across a border with no exits. Standing it here is the
      // honest version of that limit — a creep that cannot reach its work,
      // priced every tick as one — and
      // the `outpost` scenario is where the walk and the hold are
      // measured. Building the two rooms for it here would make this the
      // outpost scenario twice.
      reserver: stationsIn(room, grid, [SPAWN_POS]),
      anchor: stationsIn(room, grid, [SOURCE_A, SOURCE_B]),
      hauler: stationsIn(room, grid, [SPAWN_POS]),
      // The upgrader row stands at the controller container — the upgrade
      // buffer (ADR 0046): it draws from the store at its feet and spends
      // it into the controller from where it stands, so its tile is the
      // buffer's Upgrade Work Area and nothing else in the room. Written
      // as the container's own tile and left to `nearestFree` like every
      // other station: the container is claimed ground, so what a body
      // gets is the nearest free ground outward from it — a walkable
      // neighbour of the buffer while that ring has room, and a tile
      // further out once it is full. Either way it is ground the row can
      // act from, since the buffer stands at range 1 of the controller
      // and the Upgrade Work Area reaches 3: the widest row either
      // scenario hires at any level the flag accepts is five bodies, and
      // all five stand inside range 3. Standing this row anywhere else
      // would time a body walking to work that is defined as work done in
      // place.
      upgrader: stationsIn(room, grid, [CONTAINER_B]),
      worker: stationsIn(room, grid, [CONTROLLER, ...cluster.sites.map((site) => site.pos)]),
    },
    // One room, so one claimed-tile set: everything the colony already
    // stands on, which `taken` has collected as the room was furnished.
    claimed: new Map([[ROOM, taken]]),
    // The colonies this world expects `Colony.living` to answer with, in
    // the order `Colony.declared` files them — checked against the
    // bundle's own `decide` calls every profiled tick.
    colonies: [ROOM],
    homeRooms: [room],
    // Each furnished home room's geometry and its clustered tiles, for
    // the ADR 0022 self-check below.
    furnished: [
      {
        grid,
        sourcePositions: [SOURCE_A, SOURCE_B],
        controllerPos: CONTROLLER,
        clustered: cluster.built.concat(cluster.sites).map((s) => s.pos),
      },
    ],
    describe: () => [
      `stub colony in ${ROOM} at RCL${LEVEL} (2 sources, spawn, controller, ` +
        `${furnitureLine()}, ${plural(roads.length, "road")}, ` +
        `${plural(containers.length, "container")}, ${plural(cluster.sites.length, "site")}, ` +
        `${plural(creeps.length, "creep")})`,
      `  ${plural(stationsOf(creeps, "reserver").length, "reserver")} at ` +
        `${stationsOf(creeps, "reserver").join(", ") || "no station"} — this world models no ` +
        "declared outpost and answers every one of them as solid rock, so their Reserve target " +
        "is unreachable and they stand where they were cast; the walk is the outpost scenario's " +
        "to measure",
      `  ${plural(stationsOf(creeps, "upgrader").length, "upgrader")} at ` +
        `${stationsOf(creeps, "upgrader").join(", ") || "no station"} — beside the controller ` +
        `container at ${keyOf(CONTAINER_B)}, in the Upgrade Work Area it buffers (ADR 0046). ` +
        "Two gates divide this scenario's levels between them: under an 800 bank the row's own " +
        "cast is no standing body and none is hired at all (ADR 0046, #187), and above it the " +
        "quota is the surplus divided by one body's upgrade drain, rounded down (#195) — over " +
        "an income base of one posted source, since one of this room's two sources stands a " +
        "container and the other counts zero (ADR 0042). So the seat stands empty at every " +
        "level, which is the row costing nothing here, not the row missing",
    ],
    spareTiles: spare.length,
  };
}

// One stub creep, from the part list the bundle asked the spawn for. Its
// `room` is attached by the caller once the room object exists, exactly as
// the spawn's is.
function stubCreep({ name, pos, parts, used, ticksToLive = 1500 }) {
  return {
    id: `creep-${name}`,
    name,
    spawning: false,
    ticksToLive,
    fatigue: 0,
    pos,
    body: parts.map((type) => ({ type })),
    store: store({
      used,
      capacity: parts.filter((part) => part === "carry").length * CARRY_CAPACITY,
    }),
    harvest: ok,
    transfer: ok,
    withdraw: ok,
    build: ok,
    repair: ok,
    upgradeController: ok,
    // The reserver row's own verb (ADR 0042, #130). Here for the same
    // reason every other one is: the stub implements exactly the surface
    // `Bindings.fs` declares, and a creep missing a method the Executor
    // reaches for takes the whole run down on the tick that row is first
    // matched — which is how a stub answers "this row does not exist" (#163).
    reserveController: ok,
    // The claim verb (ADR 0047): a **candidate colony** is a declared home
    // this colony does not own yet, and the creep sent to take it calls
    // this. Here for the reason `reserveController` above is, and it is the
    // same bill coming due a third time — the `outpost` scenario has thrown
    // here since W13S28 was declared (#186) and its rooms became a claim
    // target the harness had no verb for. A stub that implements exactly
    // the surface `Bindings.fs` declares is the only stub that cannot
    // rot this way.
    claimController: ok,
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
// The rows whose stations are places rather than a pool: one Anchor per
// Post (ADR 0020, ADR 0042), so the row's station count is the world's
// Post count and a body past it has nowhere of its own to stand. The
// reserver row is *not* one of these, though its quota is one per declared
// outpost: the stub scenario deliberately stands both of its reservers at
// the one station it can offer, because it models neither outpost, and
// says so in its report. Neither is the upgrader row, and for a different
// reason again: its quota is the surplus divided by one body's upgrade
// drain (ADR 0046), a number with no places in it at all, so however many
// the colony hires they all belong at the one buffer and the cursor is
// meant to wrap them around it.
const ONE_PER_STATION = new Set(["anchor"]);

// The fleet, hired by the bundle rather than written down.
// ---------------------------------------------------------------------------

// How full a hired creep's store is, cycled over the hires so both halves
// of the logistics loop are in the measurement: an empty creep pools
// Harvest and Withdraw, a full one pools Refill, Build and Upgrade.
const FILLS = [0, 0.5, 1];

// The most creeps a run will hire before it gives up. A Workforce target
// is an arithmetic of quotas and income (ADR 0012), so it converges; a run
// that walks past this is one where it did not, and a fleet still growing
// is not a colony to profile.
//
// Raised from 60 by #163, and by the outposts entering the economy rather
// than by the two reservers: a standing container makes an outpost source
// a Post (ADR 0042), so the `outpost` scenario now hires against five
// held sources instead of two, and the count is largest where the bank is
// smallest — a 300-energy body is a single Work part, so the income buys
// a great many of them. Re-measured at #199 across every level the flag
// accepts, now that ADR 0049 rounds the hauler quota once for the colony
// (#194) and ADR 0046's upgrader row takes a whole body's drink out of
// the surplus before the worker row divides the remainder (#187, #195):
// `outpost` converges at 73 hires at RCL1, 51 at RCL2, 33 at RCL3, 21 at
// RCL4, **17 at the RCL5 default** (2 reservers, 5 Anchors, 5 haulers, 3
// upgraders, 2 workers; the live colony held 22 creeps at t140,810: 15 at
// home, 3 in W12S27, 4 in W13S28), 15 at RCL6 and 13 at RCL7 and RCL8;
// `stub` never passes 21. So the ceiling clears the worst of them with
// room to spare and still catches a fleet that is genuinely running away.
const HIRE_CAP = 120;

// Fill the home room's fleet the way the colony would: run the bundle,
// honour every SpawnCreep intent it emits, and stop the tick it stops
// asking. The count is the bundle's own Workforce target at this level's
// bank — the anchor quota, the hauler quota and the income workers of ADR
// 0012, cast from the bodies ADR 0006's pattern table sizes against that
// bank — so it moves with `--level` without a number here being edited,
// and the old hard-coded 8 (which had gone three levels stale, #144)
// cannot come back.
//
// The first cast is the disaster fallback's minimal worker (ADR 0006):
// an empty colony can never refill its extensions, so its first creep is
// cast from what is banked rather than sized to capacity — exactly the
// creep a real colony starts with.
//
// Nothing here reads a quota rule; the harness only stations what it is
// handed. The row is read off the name the intent carries, which is
// observability (ADR 0006 keeps the row out of what a creep is assigned),
// and it decides where the creep stands and nothing else.
//
// Every spawn in the world is intercepted and not the first alone: since
// ADR 0047 a tick can hold two colonies, each casting from its own spawn
// into its own rooms, and a harness that listened to one of them would
// hire half a world and station it out of the other's stations. Which
// colony a cast belongs to is read off the name the intent carries —
// `{row}-{tick}-{spawn}` (`Decide.planSpawns`) — the same string the row
// is read off, and the same one `Colony.creepColonies` files a creep by.
function hireFleet(world, game, loop) {
  const requests = [];
  for (const spawn of world.spawns) {
    spawn.spawnCreep = (parts, name) => {
      requests.push({ parts: Array.from(parts), name, spawn: spawn.name });
      return 0;
    };
  }

  for (const creep of world.creeps) claimsIn(world, creep.room.name).add(keyOf(creep.pos));
  const cursors = new Map();
  const bodies = new Map();
  let hired = 0;
  let hireTicks = 0;

  for (;;) {
    requests.length = 0;
    tickStart = performance.now();
    loop();
    game.time++;
    hireTicks++;
    if (requests.length === 0) break;
    if (hired + requests.length > HIRE_CAP) {
      throw new Error(
        `the bundle is still hiring past ${HIRE_CAP} creeps at RCL${LEVEL}: the Workforce ` +
          "target is not converging, so this run would profile a colony that does not exist"
      );
    }
    for (const request of requests) {
      const row = request.name.split("-")[0];
      const stationTable = stationsFor(world, request.spawn);
      const bodyKey = `${request.spawn} ${row}`;
      if (!bodies.has(bodyKey)) bodies.set(bodyKey, request.parts);
      // A row this scenario stations nowhere is not a row to guess at:
      // standing it among the workers would have the report say "hired N
      // creeps by the bundle's own intents" over a fleet in the wrong
      // places. Falling back would be the shape ADR 0027 names and
      // refuses: code that hides a broken invariant instead of failing on
      // it. This is the throw ADR 0042's reserver row tripped the tick
      // #131 cast one (#163), and ADR 0046's upgrader row tripped again
      // the tick #187 cast one (#199) — a real row arriving, correctly
      // refused a seat it had not been given, and given one here rather
      // than a fallback. Twice is a pattern, so the message now names the
      // edit that closes it: the reader of this throw is whoever just
      // landed the row.
      if (!Object.hasOwn(stationTable, row)) {
        throw new Error(
          `${request.spawn} cast a "${row}" body and the ${scenario} scenario stations no such ` +
            `row for it (it knows: ${Object.keys(stationTable).join(", ")}), so this run would ` +
            "profile a fleet standing where the colony would not have put it. A row added to " +
            "Decide's patternTable owes this harness a station in *every* scenario's `stations` " +
            "— the tile that row does its work from — or every profile run throws here"
        );
      }
      const stations = stationTable[row];
      // The cursor is one colony's, not the world's: two colonies each
      // hire a worker row of their own, and a shared cursor would walk
      // the second colony's first body onto the first colony's second
      // station.
      const cursorKey = `${request.spawn} ${row}`;
      const cursor = cursors.get(cursorKey) ?? 0;
      cursors.set(cursorKey, cursor + 1);
      // A row whose quota is one body per place must not run out of
      // places. The hauler and worker rows pool over theirs and the cursor
      // is meant to wrap, but the Anchor row's quota is one per Post (ADR
      // 0020, ADR 0042), so an Anchor wrapping onto a station another
      // Anchor already holds is two on one Post — and, when the Posts it
      // ran out of are an outpost's, a wrap that quietly stands the whole
      // outpost's Anchors in the spawn room. That is what a scenario
      // standing an outpost container without stationing its Anchor does,
      // and nothing else in the report would show it: the fleet line
      // counts bodies, not tiles. Refused for the same reason the missing
      // row above is (ADR 0027).
      if (ONE_PER_STATION.has(row) && cursor >= stations.length) {
        throw new Error(
          `${request.spawn} cast ${cursor + 1} "${row}" bodies and the ${scenario} scenario ` +
            `stations ${stations.length} for it — that row's quota is one body per place, so ` +
            "the world it hired against holds places this scenario has not stationed"
        );
      }
      // The row's next station, which carries the room as well as the
      // tile: a reserver is cast by the home spawn and stationed beside a
      // declared outpost's controller (ADR 0042), so the room a hired
      // creep ends up in is the station's and no longer the spawn's.
      const station = stations[cursor % stations.length];
      const claimed = claimsIn(world, station.room.name);
      // A `stationsOn` row stands on its own tile; every other row is
      // resolved outward from the tile its work is at. A place-holding row
      // whose tile is already taken is a bug in the scenario and not a
      // body to re-seat, so it throws rather than walking off the place it
      // was given.
      if (station.onTile && claimed.has(keyOf(station.pos))) {
        throw new Error(
          `${request.spawn}'s "${row}" row holds the tile ${station.room.name} ` +
            `${keyOf(station.pos)} and the ${scenario} scenario has already put something ` +
            "there, so this body would be stood off the place its row exists to hold"
        );
      }
      const pos = station.onTile
        ? station.pos
        : nearestFree(station.grid, station.pos, claimed);
      claimed.add(keyOf(pos));
      const capacity = request.parts.filter((part) => part === "carry").length * CARRY_CAPACITY;
      const creep = stubCreep({
        name: request.name,
        pos,
        parts: request.parts,
        used: Math.round(capacity * FILLS[hired % FILLS.length]),
      });
      creep.room = station.room;
      world.byId.set(creep.id, creep);
      world.creeps.push(creep);
      game.creeps[creep.name] = creep;
      hired++;
    }
  }

  for (const spawn of world.spawns) spawn.spawnCreep = ok;
  // The bodies are read back by spawn and row, because in a two-colony
  // world "the hauler body" is two bodies at two banks and a crew copying
  // the wrong one would stand a 300-energy body where an 1,800-energy one
  // works.
  const bodyOf = (spawnName, row) => bodies.get(`${spawnName} ${row}`);
  return { hired, hireTicks, bodyOf };
}

// The body a crew the bundle does not hire is cast from: one the colony
// itself cast, read back off `hireFleet`'s record by the spawn that cast
// it and the row it was cast for. Standing in for a missing one with the
// worker row would be worse than unreachable: `hireFleet` records the
// *first* body per row, and the first cast of any run is ADR 0006's
// disaster-fallback minimal worker, so the crew would silently become
// three-part workers while the report still called them haulers (ADR
// 0027).
function crewBody(bodyOf, spawnName, row, rcl) {
  const parts = bodyOf(spawnName, row);
  if (!parts) {
    throw new Error(
      `${spawnName} cast no "${row}" body at RCL${rcl}, so the crew standing beside its colony ` +
        "has none to copy"
    );
  }
  return parts;
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

// The containers standing in the colony's neighbouring rooms, as the live
// server holds them (read off the read-only API at t140,810): one beside
// each source, on the Seat #128 placed the site on. ADR 0042 makes a
// standing container the switch that admits an outpost into the economy —
// until one stands, the room is invisible to every quota but the
// reserver's — so a scenario without them profiles rooms whose sources the
// bundle prices at nothing, which is not the colony that exists. W13S28's
// two are the same tiles whether the room is read as an outpost (the
// `outpost` scenario, which predates its spawn) or as the young colony it
// has since become (`young`, `pair`): a container is where it was built,
// and what changed is who owns the room.
//
// Written down as tiles rather than derived, for the reason the spawn tile
// above is: which Seat the container landed on is a decision the colony
// has already made and the server has already built, and re-deriving it
// here would have the harness profile the room it thinks the colony should
// have rather than the one it has. Each is range 1 of its own source
// (`16,45`; `18,4`; `16,7`) and walkable — 18,3 plain, the other two swamp.
const LIVE_CONTAINERS = {
  W12S27: [{ x: 15, y: 44 }],
  W13S28: [
    { x: 18, y: 3 },
    { x: 15, y: 8 },
  ],
};

// How long the colony's reservation on each outpost controller has left.
// A hold rather than none, because the live colony holds both (ADR 0042's
// reserver row, #131): an unreserved source is worth five a tick instead
// of ten, so a scenario with no reservation prices three sources at half
// and sizes every quota that reads them off the wrong number. Long rather
// than nearly spent, because the row's body is
// `ceil((5000 − ticks held) / 600)` CLAIM parts and the world is frozen:
// a hold about to lapse would have every tick of the run cast the largest
// body the bank affords, which is a colony in an emergency and not one at
// work.
const OUTPOST_RESERVATION_TICKS = 4000;

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
  // the ids `Colony.declared` will name.
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

// The level a colony starts paving at: `Colony.bootstrapLevel`, which is
// where ADR 0034's ramparts and #209's road sites both begin. A room under
// it earns eight energy a tick and its Layout places neither (#209, #214),
// so a scenario that paved a young room would profile a colony whose
// hauler quota, Repair pool and walk costs are all a level's worth of
// furniture ahead of the room it is standing in.
const BOOTSTRAP_LEVEL = 3;

// One captured room furnished as a colony's home: its sources and
// controller, the containers its Layout would have built, the level's own
// cluster, and — only from `BOOTSTRAP_LEVEL` up — the trunk roads. Written
// once and called by every scenario that stands a home on real terrain, so
// a mother's room at RCL5 and a child's at RCL1 are furnished by the same
// rules at two levels rather than by two copies free to drift apart.
//
// Every id it registers carries the caller's `prefix`, because an object
// id is unique across the world and a two-colony scenario furnishes two
// rooms through this one function.
function furnishHome({
  capture,
  spawnPos,
  spawnName,
  rcl,
  prefix,
  register,
  structure,
  // Where the source containers stand: "derive" places each on the
  // nearest free tile to its source, which is what a scenario with no
  // live room to copy has to do, or an array of tiles for a room whose
  // containers the server already holds (`LIVE_CONTAINERS`).
  sourceContainers = "derive",
  // Whether the controller carries an upgrade buffer (ADR 0046). A colony
  // that has only just started building has none — its Layout puts the
  // Posts up first — and its upgrader row is not hired at that bank
  // anyway (#187).
  buffer = true,
}) {
  const furniture = furnitureFor(rcl);
  const taken = new Set([keyOf(spawnPos)]);
  const claim = (pos) => {
    taken.add(keyOf(pos));
    return pos;
  };
  for (const source of capture.sources) claim(source.pos);
  claim(capture.controller.pos);

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
    my: true,
    // The colony's own name, which every reservation is judged against —
    // see the stub scenario's controller for why it is here.
    owner: { username: COLONY_OWNER },
    level: rcl,
    ticksToDowngrade: 9000,
    safeModeAvailable: 1,
    safeMode: undefined,
    pos: capture.controller.pos,
    activateSafeMode: ok,
  });

  // Containers where the plan would want them: one on each source's Seat,
  // and the upgrade buffer beside the controller where the room has one.
  // Placed by the terrain rather than by hand unless the caller names the
  // tiles the live server holds, so the scenario does not smuggle in a
  // tile the room does not have.
  const seats =
    sourceContainers === "derive"
      ? capture.sources.map((source) => nearestFree(capture, source.pos, taken))
      : sourceContainers;
  // A source container off its source's Seat ring is no Post at all (ADR
  // 0012, ADR 0020), and everything below stations the Anchor row on these
  // tiles — so a tile the caller wrote down wrong would stand a work-heavy
  // body where it can never dig, and the report would still call it a
  // Post. Checked here rather than trusted, the way every other placement
  // in this file fails loudly (ADR 0027).
  // Which seat serves which source is read off the terrain and never off
  // the order the tiles were written in: `LIVE_CONTAINERS` lists W13S28's
  // two in the reverse of the capture's source order, deliberately, and a
  // check that paired them by index would call the room's own furniture a
  // mistake.
  const posted = new Set();
  for (const seat of seats) {
    const source = capture.sources.find(
      (candidate) =>
        !posted.has(candidate.id) &&
        Math.max(Math.abs(seat.x - candidate.pos.x), Math.abs(seat.y - candidate.pos.y)) === 1
    );
    if (!source) {
      throw new Error(
        `${capture.name}: the container at ${keyOf(seat)} is on no unposted source's Seat, so ` +
          "it makes no Post — a Post is the Seat a source container stands on (ADR 0012)"
      );
    }
    posted.add(source.id);
  }
  const containers = seats.map((pos, i) =>
    structure(`${prefix}cont-${i}`, "container", claim(pos), {
      store: store({ used: 1500, capacity: CONTAINER_CAPACITY }),
    })
  );
  // The buffer is a different thing from the rest and is named rather than
  // left as an index expression, because two rules read it: it is stocked
  // at 800 where a source container holds 1500, and it is where the
  // upgrader row stands.
  const bufferContainer = buffer
    ? structure(
        `${prefix}cont-buffer`,
        "container",
        claim(nearestFree(capture, capture.controller.pos, taken)),
        { store: store({ used: 800, capacity: CONTAINER_CAPACITY }) }
      )
    : null;
  if (bufferContainer) containers.push(bufferContainer);

  const spawn = structure(`${prefix}spawn-1`, "spawn", spawnPos, {
    store: store({ used: SPAWN_ENERGY_CAPACITY, capacity: SPAWN_ENERGY_CAPACITY }),
    hits: 5000,
    hitsMax: 5000,
  });

  // The level's cluster on the room's own ground: the extensions, towers
  // and Storage this level allows, and the extension sites held back out
  // of that same allowance.
  const cluster = placeCluster({
    grid: capture,
    spawnPos,
    sourcePositions: capture.sources.map((source) => source.pos),
    controllerPos: capture.controller.pos,
    taken,
    structure,
    register,
    rcl,
    prefix,
  });

  // The paved trunks: spawn to every container, along walkable ground and
  // around the cluster rather than through it — and none at all under
  // `BOOTSTRAP_LEVEL`, where the colony places no road site.
  const blocked = new Set(cluster.built.concat(cluster.sites).map((s) => keyOf(s.pos)));
  const roadTiles = [];
  if (rcl >= BOOTSTRAP_LEVEL) {
    for (const container of containers) {
      for (const tile of route(capture, spawnPos, container.pos, blocked)) {
        if (taken.has(keyOf(tile))) continue;
        claim(tile);
        roadTiles.push(tile);
      }
    }
  }
  // A couple of roads below half hits, so the Repair family is in the
  // measurement instead of pooling zero tasks — the stub scenario's rule.
  const roads = roadTiles.map((pos, i) =>
    structure(`${prefix}road-${i}`, "road", pos, i % 8 === 3 ? { hits: 2100 } : {})
  );

  const finds = {
    105: sources,
    108: [spawn, ...cluster.built],
    107: [spawn, ...cluster.built, ...roads, ...containers],
    114: cluster.sites,
    103: [],
    106: [],
  };
  const room = stubRoom({
    name: capture.name,
    controller,
    findTables: finds,
    energy: { available: furniture.bank, capacity: furniture.bank },
  });
  Object.assign(spawn, { name: spawnName, spawning: null, room, spawnCreep: ok });

  const postKeys = new Set(seats.map(keyOf));
  return {
    capture,
    rcl,
    furniture,
    taken,
    // The Posts this room's Layout made: the tile each source container
    // stands on (ADR 0012, ADR 0051), in the sources' own order. Beside
    // `containers` rather than read back out of it, because the buffer is
    // in that list and is never a Post (ADR 0046 against ADR 0012's
    // generalization).
    posts: seats,
    // What a creep may not be stood on in this room: `taken` is every tile
    // the furnishing *placed* something on, and this is that set less the
    // Posts — `furnishOutpost`'s rule, and for its reason. A container is
    // walkable, standing on one is exactly what garrisoning a Post means
    // (ADR 0020), and Harvest offers a work-heavy body no other footing
    // (ADR 0048), so an Anchor resolved against `taken` would be pushed
    // off the one tile it can work from and the run would time a creep in
    // transit.
    occupied: new Set([...taken].filter((key) => !postKeys.has(key))),
    blocked,
    sources,
    controller,
    containers,
    buffer: bufferContainer,
    spawn,
    spawnPos,
    cluster,
    roads,
    finds,
    room,
  };
}

// One captured room furnished as an outpost: the rocks, the reservation
// the colony holds on the controller, and the containers the live server
// stands there. Nothing owned — an outpost is a room we do not own, so no
// spawn, no extension and no tower — and no road either: ADR 0042 declines
// to pave one, so the colony has none to model.
function furnishOutpost(capture, register, structure) {
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
    reservation: { username: COLONY_OWNER, ticksToEnd: OUTPOST_RESERVATION_TICKS },
    pos: capture.controller.pos,
    activateSafeMode: ok,
  });
  const containers = (LIVE_CONTAINERS[capture.name] ?? []).map((pos, i) =>
    structure(`${capture.name.toLowerCase()}-cont-${i}`, "container", pos, {
      store: store({ used: 1500, capacity: CONTAINER_CAPACITY }),
    })
  );
  return {
    capture,
    sources,
    containers,
    // What a creep may not be stood on here: a source and a controller
    // are obstacles the engine will not let one share, and nothing else
    // in the room is — a container is walkable, and standing an Anchor
    // on one is exactly what a Post is (ADR 0020). Handed to `hireFleet`
    // and to the crew below as one set per room, so the reserver and the
    // Anchor the bundle stations here and the hauler crew stood beside
    // them cannot land on one tile.
    occupied: new Set([
      ...sources.map((source) => keyOf(source.pos)),
      keyOf(capture.controller.pos),
    ]),
    room: stubRoom({
      name: capture.name,
      controller,
      findTables: { 105: sources, 108: [], 107: containers, 114: [], 103: [], 106: [] },
    }),
  };
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
  const furnished = furnishHome({
    capture: home,
    spawnPos: HOME_SPAWN,
    spawnName: "Spawn1",
    rcl: LEVEL,
    prefix: "",
    register,
    structure,
  });
  const {
    taken,
    blocked,
    sources: homeSources,
    containers,
    buffer,
    spawn,
    cluster,
    roads,
    finds: homeFinds,
    room: homeRoom,
  } = furnished;

  // --- the outposts ------------------------------------------------------
  // Vision in both, which is the expensive half: a room we can see is
  // projected entry by entry, and a room we cannot contributes terrain and
  // nothing else (ADR 0004). The worst case is the one worth measuring.
  // Nothing *owned* stands in them — an outpost is a room we do not own,
  // so no spawn, no extension and no tower — but the three source
  // containers do, on the tiles the live server holds them
  // (`LIVE_CONTAINERS`), because a container is nobody's and is the one
  // structure ADR 0042 puts in an outpost. Roads are still out: ADR 0042
  // declines to pave an outpost, so the colony has none to model.
  //
  // Their controllers carry the colony's own reservation
  // (`OUTPOST_RESERVATION_TICKS`), which is what doubles those sources and
  // is the reserver row's whole reason to exist.
  const outpostRooms = outposts.map((capture) => furnishOutpost(capture, register, structure));

  // --- the fleet ---------------------------------------------------------
  // Hired by the bundle itself (`hireFleet`), so its size is this level's
  // Workforce target and not a number written here — and since
  // `Colony.declared`'s outpost list was filled (#126) that target is the
  // three-room colony's. Three of its rows leave the spawn room: the reserver stands
  // beside each outpost's controller and, because the containers below
  // make those sources Posts, an Anchor stands on each of their containers
  // (ADR 0042 — one Anchor per Post, wherever the Post lies). `hireFleet`
  // stations them there rather than at home, so the run times creeps at
  // work instead of creeps that would spend a frozen world in transit.
  //
  // Beside them one crew the bundle does not hire: a hauler per outpost
  // *container*. The hauler row itself is hired and stood at the home
  // spawn, which is the storage end of every round trip; these three are
  // the far end, which nothing else in a frozen world stands at. It is a
  // floor and not a quota — the row's own quota already prices these three
  // round trips into the home hires, one rounding for the whole colony
  // (ADR 0049, which is where ADR 0042's "two haulers per container"
  // reading of an unpaved outpost went) — so the count is deliberately
  // under what the colony runs, and what the bundle would hire *instead
  // of* it is ADR 0012's arithmetic and not this harness's to guess.
  //
  // They are crewed *after* the fleet is hired, and cast from the bodies
  // the bundle itself cast at home — an outpost hauler is the home
  // hauler's body a room over. After, because the world-wide `Game.creeps`
  // is what the Workforce target counts against, so crewing first would
  // have the bundle hire that much less and the run would measure a fleet
  // nobody chose the size of.
  const creeps = [];
  const crewOutposts = (bodyOf, game) => {
    const haulerParts = crewBody(bodyOf, "Spawn1", "hauler", LEVEL);
    for (const outpost of outpostRooms) {
      // The room's own claimed tiles, the same set `hireFleet` stood the
      // reserver and the outpost Anchors out of, so the crew cannot be put
      // on top of one.
      const occupied = outpost.occupied;
      // One hauler per standing container and not per room: the haul is a
      // round trip per source container, and W13S28 holds two of the
      // three. One is ADR 0042's *paved* number — the unpaved outpost this
      // world models sizes two — and the floor is deliberate, because the
      // hired hauler row above already prices these same round trips.
      const defs = outpost.containers.map((container) => ({
        at: container.pos,
        parts: haulerParts,
      }));
      for (const [i, def] of defs.entries()) {
        const pos = nearestFree(outpost.capture, def.at, occupied);
        occupied.add(keyOf(pos));
        const capacity = def.parts.filter((part) => part === "carry").length * CARRY_CAPACITY;
        const creep = register(
          stubCreep({
            name: `${outpost.capture.name.toLowerCase()}-${i}`,
            pos,
            parts: def.parts,
            used: Math.round(capacity * FILLS[i % FILLS.length]),
          })
        );
        creep.room = outpost.room;
        creeps.push(creep);
        game.creeps[creep.name] = creep;
      }
    }
  };

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
    for (const tile of route(home, containers[i].pos, containers[i + 1].pos, blocked)) {
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
    // Same rule as the stub scenario's for the two rows that never leave
    // the home room: a hauler at the spawn — the storage end of every
    // round trip, wherever the far end lies — and a worker at the
    // controller or a site.
    //
    // The other two rows are cast at home and work a room away, so their
    // stations carry the room as well as the tile. The reserver's is one
    // per declared outpost, at that outpost's controller — `nearestFree`
    // resolves it to a walkable tile at range 1, which on W12S27 is one of
    // exactly two, both swamp — and taken in `Colony.declared`'s own
    // order, which `OUTPOST_ROOMS` above spells, so the first body cast
    // holds W12S27 and the second W13S28. The Anchor row's is one station
    // per source, in every room the projection carries and not the spawn
    // room's alone (ADR 0042): the tick an outpost's container stands, its
    // source becomes a Post and gains an Anchor of its own, and the row's
    // quota is one per Post wherever the Post lies. Written as the source
    // and left to `nearestFree` like the home room's, which on all three
    // outpost sources resolves to the container's own tile — the Seat the
    // colony built on, and what standing on a Post means (ADR 0020).
    //
    // Standing either row at home instead would put it on a fifty-tile
    // walk it never finishes in a frozen world, and the run would time
    // creeps in transit as if they were creeps at work — which is the
    // world `hireFleet`'s own throw refuses to profile.
    stations: {
      reserver: outpostRooms.flatMap((outpost) =>
        stationsIn(outpost.room, outpost.capture, [outpost.room.controller.pos])
      ),
      anchor: [
        ...stationsIn(
          homeRoom,
          home,
          home.sources.map((source) => source.pos)
        ),
        ...outpostRooms.flatMap((outpost) =>
          stationsIn(
            outpost.room,
            outpost.capture,
            outpost.sources.map((source) => source.pos)
          )
        ),
      ],
      hauler: stationsIn(homeRoom, home, [HOME_SPAWN]),
      // The upgrader row at the buffer it drinks from (ADR 0046) — this
      // room's real controller container, wherever the terrain put it,
      // rather than a tile written down here. `nearestFree` resolves each
      // body to the nearest free ground outward from it — a walkable
      // neighbour of the buffer while that ring has room, and a tile
      // further out once it is full, which the widest row this scenario
      // hires (five, at RCL4) is: its last two bodies stand at range 2 of
      // the buffer. All of them are still ground the row can act from,
      // which is what the seat is for: the buffer stands at range 1 of the
      // controller while the Upgrade Work Area reaches 3, and measured
      // across every level the flag accepts no body of this row lands
      // outside it. It pools over the one tile the way the hauler row
      // pools over the spawn,
      // rather than holding places the way the Anchor row does: its quota
      // is a division of the surplus (ADR 0046) and has no count of places
      // in it, so how many stand here is the colony's arithmetic and they
      // all belong at the one buffer.
      upgrader: stationsIn(homeRoom, home, [buffer.pos]),
      worker: stationsIn(homeRoom, home, [
        home.controller.pos,
        ...cluster.sites.map((site) => site.pos),
      ]),
    },
    // One claimed-tile set per room of the world: the home room's is what
    // furnishing it collected, each outpost's is the obstacles it holds.
    claimed: new Map([
      [home.name, taken],
      ...outpostRooms.map((outpost) => [outpost.room.name, outpost.occupied]),
    ]),
    // One living colony: the neighbours hold no spawn of ours, so
    // `Colony.living` answers with the home room alone however many rooms
    // this world models.
    colonies: [home.name],
    homeRooms: [homeRoom],
    furnished: [
      {
        grid: home,
        sourcePositions: home.sources.map((source) => source.pos),
        controllerPos: home.controller.pos,
        clustered: cluster.built.concat(cluster.sites).map((s) => s.pos),
      },
    ],
    crew: crewOutposts,
    describe: () => [
      `outpost colony on real terrain (ADR 0036) at RCL${LEVEL}, ${creeps.length} creeps over ` +
        `${rooms.length} rooms`,
      `  ${home.name} home     ${plural(homeSources.length, "source")}, controller, ` +
        `${furnitureLine()}, ${plural(roads.length, "road")}, ` +
        `${plural(containers.length, "container")}, ${plural(cluster.sites.length, "site")}, ` +
        `spawn at ${HOME_SPAWN.x},${HOME_SPAWN.y}`,
      ...outpostRooms.map(
        (outpost) =>
          `  ${outpost.capture.name} outpost  ${outpost.sources.length} source` +
          `${outpost.sources.length === 1 ? "" : "s"}, controller reserved ` +
          `${OUTPOST_RESERVATION_TICKS} ticks, ` +
          `${plural(outpost.containers.length, "container")}, vision`
      ),
      `  ${plural(stationsOf(creeps, "reserver").length, "reserver")} beside the outpost ` +
        `controllers at ${stationsOf(creeps, "reserver").join(", ") || "no station"}, one per ` +
        "declared outpost in Colony.declared's own order",
      `  ${plural(stationsOf(creeps, "upgrader").length, "upgrader")} at ` +
        `${stationsOf(creeps, "upgrader").join(", ") || "no station"}, around the controller ` +
        `container at ${keyOf(buffer.pos)} — the upgrade buffer, whose Upgrade Work Area is ` +
        "this row's working ground (ADR 0046); the count is the three-room surplus divided by " +
        "one body's upgrade drain, rounded down (#195), and not a number in this file",
    ],
    spareTiles: spare.length,
  };
}

// ---------------------------------------------------------------------------
// Scenarios `young` and `pair`: the colony at the start of its life, and
// the two colonies a live tick actually holds (ADR 0052).
// ---------------------------------------------------------------------------

// The second colony, on its own committed terrain: W13S28, claimed on
// 2026-09-06 and spawning from (16,12) — the tile the live Spawn2 stands
// on, written down for the reason `HOME_SPAWN` is. Its sources are 16,7
// and 18,4 and its controller 24,17, all read off the capture and none of
// them written here.
const CHILD_ROOM = "W13S28";
const CHILD_SPAWN = { x: 16, y: 12 };
const CHILD_SPAWN_NAME = "Spawn2";

// The world-unique id prefix the child's furniture carries. An object id
// is unique across the world (ADR 0041) and the `pair` scenario furnishes
// two home rooms through one `furnishHome`, so the child's extensions,
// containers and roads cannot be `ext-0` twice.
const CHILD_PREFIX = "w13-";

// `Decide.pioneerCount`: how many bodies over her income workers a mother
// hires for a child she is bootstrapping (ADR 0047 decision 4). Copied
// rather than shared, like `CONTROLLER_STRUCTURES` above and for the same
// reason — the constant is `private` to Core and this script drives the
// compiled bundle — and read by nothing but the station table: the count
// the bundle actually hires is its own, and this only decides how many of
// that row stand in the child's room rather than the mother's.
const PIONEER_COUNT = 3;

// The rows every scenario has to station, filled for a colony that works
// only its own room: the Anchor row on its Posts, the hauler at its spawn,
// the worker at its controller and its sites, the upgrader on the ground
// it upgrades from, and the reserver — which such a colony never casts,
// its quota being one per *declared* outpost and it declaring none — at
// its spawn, so that the day a human declares one this harness reports a
// creep in the wrong room rather than throwing on the first cast.
function homeStations(furnished) {
  const { room, capture, cluster } = furnished;
  const at = (positions) => stationsIn(room, capture, positions);
  return {
    reserver: at([furnished.spawnPos]),
    // The Anchor row stands **on** its Posts — the tiles the source
    // containers stand on — and not beside them: Harvest's Work Area for a
    // work-heavy body is that room's Posts alone (ADR 0020, ADR 0048), and
    // a Post's Seat is the garrison's (ADR 0051). The tiles are handed
    // over rather than left to `nearestFree` from the rock, because
    // "nearest free" is the wrong answer on real terrain twice over: it
    // walks past the Post itself, which the furnishing has claimed, and on
    // W13S28's `16,7` — whose only Seat is the container's — it lands two
    // tiles from the rock, where the body can neither dig nor fill. That
    // room's own claimed set (`furnished.occupied`) leaves the Posts out
    // for exactly this row, the way an outpost's does.
    anchor: stationsOn(room, capture, furnished.posts),
    hauler: at([furnished.spawnPos]),
    // The buffer where the room has one, and the controller where it does
    // not: a colony this young has built no upgrade buffer yet, and at its
    // bank the row is not hired at all (ADR 0046, #187) — so the seat is
    // ground inside the Upgrade Work Area, and it stands empty.
    upgrader: at([furnished.buffer ? furnished.buffer.pos : capture.controller.pos]),
    worker: at([capture.controller.pos, ...cluster.sites.map((site) => site.pos)]),
  };
}

// One colony's world, on the child's captured terrain: the room, its
// furniture and the fleet the bundle hires against a 300 bank. What makes
// it the `young` scenario and not a small `outpost` one is what is *not*
// here: no road and no rampart, because a colony under
// `Colony.bootstrapLevel` places neither (#209, #214); no extension the
// level does not allow; and no upgrade buffer — which is a fact about
// *this room* and not about its level, the container plan being "RCL-gated
// by nothing" (`Decide.planLayout`), so the live W13S28 has simply built
// its two Posts first. Every quota that reads a walk, a Repair or a bank
// reads it off exactly that. (`RoomFixtures.colonyAt` models the other
// half of the same rung — the room its Layout has *finished*, buffer and
// all — and the two are meant to differ there.)
function buildYoungWorld() {
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

  const capture = loadCapture(CHILD_ROOM);
  const home = furnishHome({
    capture,
    spawnPos: CHILD_SPAWN,
    spawnName: CHILD_SPAWN_NAME,
    rcl: LEVEL,
    prefix: "",
    register,
    structure,
    // The two containers the live server holds in this room, which are
    // what make both its sources Posts (ADR 0042) — the two posted rocks
    // ADR 0052's scenario is about.
    sourceContainers: LIVE_CONTAINERS[CHILD_ROOM],
    buffer: false,
  });

  // The spare lane the census perturbation walks: the ground between the
  // two Posts, which nothing paves here — this room is under the level
  // that places a road at all, so every tile of it is spare.
  const spare = route(capture, home.containers[0].pos, home.containers[1].pos, home.blocked).filter(
    (tile) => !home.taken.has(keyOf(tile))
  );
  if (spare.length === 0) {
    throw new Error(`${capture.name}: no unpaved tile between the two Posts to move the census`);
  }

  const creeps = [];
  const stations = homeStations(home);

  return {
    terrains: new Map([[capture.name, capture.terrain]]),
    rooms: [home.room],
    spawns: [home.spawn],
    creeps,
    byId,
    perturb: pavingPerturbation({ spare, structures: home.finds[107], byId, structure }),
    stations,
    // The room's own claimed tiles less its Posts (`furnished.occupied`),
    // so `hireFleet` can stand the Anchor row on the containers it
    // garrisons rather than being pushed off them.
    claimed: new Map([[capture.name, home.occupied]]),
    colonies: [capture.name],
    homeRooms: [home.room],
    furnished: [
      {
        grid: capture,
        sourcePositions: capture.sources.map((source) => source.pos),
        controllerPos: capture.controller.pos,
        clustered: home.cluster.built.concat(home.cluster.sites).map((s) => s.pos),
      },
    ],
    describe: () => [
      `young colony in ${capture.name} on real terrain (ADR 0036) at RCL${LEVEL}, ` +
        `${creeps.length} creeps in one room`,
      `  ${capture.name} home     ${plural(home.sources.length, "source")}, controller, ` +
        `${furnitureLine(home.furniture)}, ${plural(home.roads.length, "road")}, ` +
        `${plural(home.containers.length, "container")} (both of them Posts), ` +
        `${plural(home.cluster.sites.length, "site")}, ` +
        `${CHILD_SPAWN_NAME} at ${keyOf(CHILD_SPAWN)}`,
      LEVEL < BOOTSTRAP_LEVEL
        ? `  no road and no rampart: this room is under Colony.bootstrapLevel, where the Layout ` +
          "places neither (#209, #214) — so the Repair pool is the containers' alone and every " +
          "walk is priced on bare ground"
        : `  at or past Colony.bootstrapLevel, so this room paves and ramparts like any other ` +
          "(#209, #214) — the flag has moved it off the rung the scenario is named for, and " +
          "its Repair pool and walk costs are a paved colony's",
      `  ${plural(stationsOf(creeps, "anchor").length, "anchor")} at ` +
        `${stationsOf(creeps, "anchor").join(", ") || "no station"} — one per Post, on the ` +
        "container the live server holds, which is what garrisoning a Post means (ADR 0020)",
      `  ${plural(stationsOf(creeps, "upgrader").length, "upgrader")} at ` +
        `${stationsOf(creeps, "upgrader").join(", ") || "no station"}: this room has built no ` +
        "upgrade buffer, so the row has no working ground and hires nobody (ADR 0046) — a fact " +
        "about the room and not about its level, the container plan being RCL-gated by " +
        "nothing" +
        (home.furniture.bank < 800
          ? ", and at this bank the row's own cast is no standing body either (#187)"
          : "") +
        " — so the seat by the controller stands empty",
    ],
    spareTiles: spare.length,
  };
}

// Two colonies in one tick, which is what every live tick has been since
// W13S28's spawn stood (ADR 0047, #191): the mother W12S28 with her one
// declared outpost, and the child W13S28 bootstrapping beside her. What
// this scenario measures that no other does is the shape of that tick —
// `Main.loop` builds one Snapshot per living colony and runs `decide` once
// over each, the mother projecting the child's room as a bootstrap layer
// while it is under `Colony.bootstrapLevel` (#192), and the report prices
// each colony's `decide` on its own row.
function buildPairWorld() {
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

  const motherCapture = loadCapture(HOME_ROOM);
  const childCapture = loadCapture(CHILD_ROOM);
  // The mother's outposts as `Colony.declared` spells them today: W12S27
  // alone, W13S28 having left the list the day it stood its own spawn.
  const outposts = OUTPOST_ROOMS.filter((name) => name !== CHILD_ROOM).map(loadCapture);

  const mother = furnishHome({
    capture: motherCapture,
    spawnPos: HOME_SPAWN,
    spawnName: "Spawn1",
    rcl: MOTHER_LEVEL,
    prefix: "",
    register,
    structure,
  });
  const child = furnishHome({
    capture: childCapture,
    spawnPos: CHILD_SPAWN,
    spawnName: CHILD_SPAWN_NAME,
    rcl: LEVEL,
    prefix: CHILD_PREFIX,
    register,
    structure,
    sourceContainers: LIVE_CONTAINERS[CHILD_ROOM],
    buffer: false,
  });
  const outpostRooms = outposts.map((capture) => furnishOutpost(capture, register, structure));

  // The crew the bundle does not hire, on the mother's side alone: one
  // hauler per outpost container, standing the far end of a round trip
  // whose near end is her own hired hauler row. The `outpost` scenario's
  // rule and its reasons, and none of it is the child's — nothing of the
  // child's is a room away from its spawn.
  const creeps = [];
  const crewOutposts = (bodyOf, game) => {
    const haulerParts = crewBody(bodyOf, "Spawn1", "hauler", MOTHER_LEVEL);
    for (const outpost of outpostRooms) {
      for (const [i, container] of outpost.containers.entries()) {
        const pos = nearestFree(outpost.capture, container.pos, outpost.occupied);
        outpost.occupied.add(keyOf(pos));
        const capacity = haulerParts.filter((part) => part === "carry").length * CARRY_CAPACITY;
        const creep = register(
          stubCreep({
            name: `${outpost.capture.name.toLowerCase()}-${i}`,
            pos,
            parts: haulerParts,
            used: Math.round(capacity * FILLS[i % FILLS.length]),
          })
        );
        creep.room = outpost.room;
        creeps.push(creep);
        game.creeps[creep.name] = creep;
      }
    }
  };

  const motherStations = homeStations(mother);
  const childStations = homeStations(child);

  // The mother's rows, widened by the two rooms she works that are not her
  // own. The reserver's station is her one declared outpost's controller,
  // and the Anchor row gains that outpost's Post beside her own two rocks
  // (ADR 0042: one Anchor per Post, wherever the Post lies).
  motherStations.reserver = outpostRooms.flatMap((outpost) =>
    stationsIn(outpost.room, outpost.capture, [outpost.room.controller.pos])
  );
  motherStations.anchor = [
    ...motherStations.anchor,
    ...outpostRooms.flatMap((outpost) =>
      stationsIn(
        outpost.room,
        outpost.capture,
        outpost.sources.map((source) => source.pos)
      )
    ),
  ];
  // The pioneers (ADR 0047 decision 4, #213): the mother's worker row
  // hires `pioneerCount` bodies over its income workers for the child's
  // Upgrade and Build, and their work is in the child's room. Nothing in
  // the cast tells the two apart — a pioneer is a worker, cast from the
  // worker row and named like one — so the harness stands the row's first
  // three bodies at the child's controller and pools the rest over her own
  // room. It is an approximation of *which* bodies, and it is exact in
  // what it is for: a colony that hires pioneers has that many bodies
  // standing in its child's room, and a run that stood them all at home
  // would time a walk across a Seam that a frozen world never finishes.
  //
  // Only while the window is open, which is the whole of the rule: the
  // mother hires pioneers until the child's controller reaches
  // `Colony.bootstrapLevel` and none after (ADR 0047 decision 4,
  // `Colony.bootstrapping`). Past it she no longer projects the child's
  // room at all, so a body of hers standing there would be adopted by the
  // child for the tick (`Colony.creepColonies`) — and the `decide by
  // colony` table this scenario exists to print would charge three of the
  // mother's workers to the child while her own row came up three short.
  const pioneering = LEVEL < BOOTSTRAP_LEVEL;
  if (pioneering) {
    motherStations.worker = [
      ...stationsIn(
        child.room,
        childCapture,
        Array(PIONEER_COUNT).fill(childCapture.controller.pos)
      ),
      ...motherStations.worker,
    ];
  }

  // The spare lane, on the mother's side: the `outpost` scenario's own —
  // the ground between one container and the next, which no trunk paves.
  // Hers and not the child's because the census that matters here is the
  // bigger room's, and because the child's room is paved by nothing at all
  // (it is under `BOOTSTRAP_LEVEL`), so a tile paved there would be the
  // first road in a room whose Layout places none.
  const paved = new Set(mother.taken);
  const spare = [];
  for (let i = 0; i + 1 < mother.containers.length; i++) {
    for (const tile of route(
      motherCapture,
      mother.containers[i].pos,
      mother.containers[i + 1].pos,
      mother.blocked
    )) {
      const key = keyOf(tile);
      if (paved.has(key)) continue;
      paved.add(key);
      spare.push(tile);
    }
  }
  if (spare.length === 0) {
    throw new Error(`${motherCapture.name}: every tile between the containers is already paved`);
  }

  const rooms = [mother.room, child.room, ...outpostRooms.map((o) => o.room)];
  return {
    terrains: new Map([
      [motherCapture.name, motherCapture.terrain],
      [childCapture.name, childCapture.terrain],
      ...outposts.map((capture) => [capture.name, capture.terrain]),
    ]),
    rooms,
    spawns: [mother.spawn, child.spawn],
    creeps,
    byId,
    perturb: pavingPerturbation({ spare, structures: mother.finds[107], byId, structure }),
    // A station table per spawn, because the two colonies' rows are the
    // same five names over different rooms: the mother's worker stands at
    // her controller or her child's, the child's at its own.
    stationsBySpawn: { Spawn1: motherStations, [CHILD_SPAWN_NAME]: childStations },
    claimed: new Map([
      [motherCapture.name, mother.occupied],
      [childCapture.name, child.occupied],
      ...outpostRooms.map((outpost) => [outpost.room.name, outpost.occupied]),
    ]),
    // Both, in `Colony.declared`'s order — and the run throws on the first
    // profiled tick if the bundle decides for anything else.
    colonies: [motherCapture.name, childCapture.name],
    homeRooms: [mother.room, child.room],
    furnished: [
      {
        grid: motherCapture,
        sourcePositions: motherCapture.sources.map((source) => source.pos),
        controllerPos: motherCapture.controller.pos,
        clustered: mother.cluster.built.concat(mother.cluster.sites).map((s) => s.pos),
      },
      {
        grid: childCapture,
        sourcePositions: childCapture.sources.map((source) => source.pos),
        controllerPos: childCapture.controller.pos,
        clustered: child.cluster.built.concat(child.cluster.sites).map((s) => s.pos),
      },
    ],
    crew: crewOutposts,
    describe: () => [
      `mother and bootstrapping child on real terrain (ADR 0036, ADR 0052): ${motherCapture.name}` +
        ` at RCL${MOTHER_LEVEL} and ${childCapture.name} at RCL${LEVEL}, ${creeps.length} creeps ` +
        `over ${rooms.length} rooms`,
      `  ${motherCapture.name} mother   ${plural(mother.sources.length, "source")}, controller, ` +
        `${furnitureLine(mother.furniture)}, ${plural(mother.roads.length, "road")}, ` +
        `${plural(mother.containers.length, "container")}, ` +
        `${plural(mother.cluster.sites.length, "site")}, Spawn1 at ${keyOf(HOME_SPAWN)}`,
      ...outpostRooms.map(
        (outpost) =>
          `  ${outpost.capture.name} outpost  ${plural(outpost.sources.length, "source")}, ` +
          `controller reserved ${OUTPOST_RESERVATION_TICKS} ticks, ` +
          `${plural(outpost.containers.length, "container")}, vision`
      ),
      `  ${childCapture.name} child    ${plural(child.sources.length, "source")}, controller, ` +
        `${furnitureLine(child.furniture)}, ${plural(child.roads.length, "road")}, ` +
        `${plural(child.containers.length, "container")} (both Posts), ` +
        `${plural(child.cluster.sites.length, "site")}, ${CHILD_SPAWN_NAME} at ` +
        `${keyOf(CHILD_SPAWN)}` +
        (LEVEL < BOOTSTRAP_LEVEL
          ? " — under Colony.bootstrapLevel, so the mother projects this room as a bootstrap " +
            "layer and her workers may cross for its Upgrade and Build (#192, #213)"
          : " — at or past Colony.bootstrapLevel, so the bootstrap window is shut: the mother " +
            "no longer projects this room and the two colonies share nothing"),
      `  ${plural(stationsOf(creeps, "worker", "Spawn1").length, "mother's worker")} at ` +
        `${stationsOf(creeps, "worker", "Spawn1").join(", ") || "no station"}` +
        (pioneering
          ? ` — the first ${PIONEER_COUNT} of them at the child's controller, which is what a ` +
            "pioneer is"
          : " — all of them in her own room: the window is shut, so she hires no pioneer and " +
            "stands nobody in a room she no longer projects"),
      `  ${plural(stationsOf(creeps, "anchor", CHILD_SPAWN_NAME).length, "child's anchor")} at ` +
        `${stationsOf(creeps, "anchor", CHILD_SPAWN_NAME).join(", ") || "no station"}, one per ` +
        "Post of its own",
    ],
    spareTiles: spare.length,
  };
}

const WORLDS = {
  stub: buildStubWorld,
  outpost: buildOutpostWorld,
  young: buildYoungWorld,
  pair: buildPairWorld,
};

const buildWorld = () => WORLDS[scenario]();

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

// The CPU line ADR 0052 asks for: one row per colony's `decide` and one
// for the sum of them, per class of tick. It is the harness's clock and
// not the bot's — `Main.loop`'s decide phase is one reading over every
// colony (ADR 0047), which is the whole reason this table is taken from
// outside — so read it against the ms/tick above and never against the
// observe channel's own `decide` column, which prices the same work plus
// the fold around it.
//
// The total is the sum of the colonies' means and not a fourth
// measurement: what a reader compares it against is the tick, and the gap
// between the two is the projection, the Memory writes and the intents —
// everything `decide` is not.
function printDecideByColony(classes, decideMs, ticks, stages) {
  const mean = (rows) => (rows.length ? rows.reduce((a, b) => a + b, 0) / rows.length : 0);
  const tickMs = {
    all: ticks.all.map((row) => row.ms),
    perturbed: ticks.perturbed,
    quiet: ticks.quiet,
  };
  console.log("\ndecide by colony — ms per tick of each class (this harness's clock)");
  console.log(`  ${classes.map((c) => c.label.padStart(9)).join("  ")}  colony`);
  const homes = [...decideMs.all.keys()];
  const column = (label, home) =>
    mean(decideMs[label].get(home) ?? [])
      .toFixed(2)
      .padStart(9);
  for (const home of homes) {
    // The colony's [[stage]] beside its row (ADR 0052 decision 3), read
    // off the Snapshot the bundle was handed: which of the rules that turn
    // on it — the road gate, the rampart gate, the tier a young room's
    // sites are built on — this row's ms were paid under.
    const stage = stages.get(home);
    console.log(
      `  ${classes.map((c) => column(c.label, home)).join("  ")}  ${home}` +
        (stage ? `  (${stage})` : "  (no stage)")
    );
  }
  const totalOf = (label) =>
    homes.reduce((total, home) => total + mean(decideMs[label].get(home) ?? []), 0);
  console.log(
    `  ${classes.map((c) => totalOf(c.label).toFixed(2).padStart(9)).join("  ")}  ` +
      `all ${homes.length} colon${homes.length === 1 ? "y" : "ies"}`
  );
  console.log(
    `  ${classes.map((c) => mean(tickMs[c.label] ?? []).toFixed(2).padStart(9)).join("  ")}  ` +
      "the whole tick, for comparison (projection, Memory and intents included)"
  );
}

function printReport(classes, pooled, world, allTicks) {
  // The level is printed on the first line of every run, tripped trigger
  // or not: the ms below are a colony's only at the level it was built at,
  // and a report that did not say which one is how two scenarios came to
  // be judged three levels below the live room (#144).
  const description = world.describe();
  console.log(
    `fabot profile — ${scenario} scenario, RCL${LEVEL}, ${TICKS} ticks, ` +
      description[0] +
      (CENSUS_EVERY
        ? `, census moved every ${CENSUS_EVERY} ticks over a ${world.spareTiles}-tile lane`
        : "")
  );
  for (const detail of description.slice(1)) console.log(detail);

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
// The bundle, with one probe in it: what each colony's `decide` costs.
// ---------------------------------------------------------------------------

// `Main.loop` runs `decide` once per living colony (ADR 0047) inside one
// pair of `Game.cpu.getUsed()` readings, so the bot's own CPU line prices
// the tick's whole decide phase and never one colony's share of it. ADR
// 0052 asks for the share, and R0 changes no runtime line to get it — so
// the harness takes the reading on the outside, by wrapping the bundle's
// own `decide` before the bundle is ever called.
//
// A textual patch on a compiled artifact, which is exactly as brittle as
// it sounds, so it fails loudly rather than silently measuring nothing
// (ADR 0027): the name has to appear exactly once as a top-level
// declaration, or the run stops and names the edit that closes it. The
// wrapper is two clock reads per colony per tick — under a microsecond
// against a tick's milliseconds — and it is in every scenario's numbers
// rather than only the two-colony one, so the four scenarios stay
// comparable with each other.
//
// The row's label is the colony's home room, read off the Snapshot the
// call was handed (`SpatialInfo.RoomName`, which `Snapshot.build` sets to
// the colony's home): the same name `Colony.living` files the colony
// under, so the table and the declaration cannot drift apart.
//
// Beside it the colony's [[stage]] (ADR 0052 decision 3), read off the
// same Snapshot's `Stages` under that same home name — the bundle's own
// answer and never this harness's arithmetic off `--level`, which is the
// whole point of printing it: `young --level 3` and `pair` are worlds
// whose rules turn on the stage, and a scenario that furnished one colony
// and decided another would say so here. A stage the map does not carry
// prints as none, which is what a colony whose controller nothing can
// place is.
const DECIDE_PROBE = `
// ---- appended by scripts/profile.mjs: one timing per decide call --------
globalThis.__fabotDecideCalls = [];
{
  const inner = decide;
  const stageOf = (snapshot, home) => {
    const stages = snapshot && snapshot.Stages;
    if (!home || !stages || typeof stages[Symbol.iterator] !== "function") return null;
    for (const [room, stage] of stages) if (room === home) return String(stage);
    return null;
  };
  decide = function decideProbe(snapshot, assignments, verbose, memo) {
    const started = globalThis.__fabotClock();
    const home = (snapshot && snapshot.Spatial && snapshot.Spatial.RoomName) || "(unnamed)";
    try {
      return inner(snapshot, assignments, verbose, memo);
    } finally {
      globalThis.__fabotDecideCalls.push({
        home,
        stage: stageOf(snapshot, home),
        ms: globalThis.__fabotClock() - started,
      });
    }
  };
}
`;

// The bundle as the engine would load it, plus the probe above. Written
// beside the original rather than over it — under the same basename, so
// the hotspot tables' `[main.js:NNNN]` locations still name the lines a
// reader can open, and appended rather than spliced, so every line number
// in them is the bundle's own.
function loadBundle(file) {
  const source = readFileSync(file, "utf8");
  const declarations = source.match(/^function decide\(/gm) ?? [];
  if (declarations.length !== 1) {
    throw new Error(
      `${path.relative(process.cwd(), file)} holds ${declarations.length} top-level ` +
        "`function decide(` declarations and this harness needs exactly one to time each " +
        "colony's decide (ADR 0052's CPU row per colony). Whatever renamed or inlined it — a " +
        "Fable or esbuild upgrade, a rename in Decide.fs — is what this probe has to be " +
        "re-pointed at"
    );
  }
  globalThis.__fabotClock = () => performance.now();
  const probed = path.join(here, "..", "build", "probe");
  mkdirSync(probed, { recursive: true });
  const probedFile = path.join(probed, path.basename(file));
  writeFileSync(probedFile, source + DECIDE_PROBE);
  const { loop } = createRequire(import.meta.url)(probedFile);
  return { loop, decideCalls: () => globalThis.__fabotDecideCalls };
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
// Second self-check, the same discipline on the other axis the level
// moves: no clustered structure stands on the home room's working ground
// (ADR 0022). Derived here from the room's own sources and controller
// rather than from the ordering that placed the cluster, so dropping the
// exclusion in `clusterTiles` fails here instead of quietly furnishing a
// room the colony's Layout would never build — which is what an untested
// level bump did at RCL6, taking the east source's Seat at 17,39 (#144).
// Once per furnished home room and not once per world: a `pair` run
// furnishes a mother's room and a child's, at two levels, and a check that
// read the first would pass a cluster standing on the other's Seats.
for (const home of world.furnished) {
  const reservedGround = workingGround(home.grid, home.sourcePositions, home.controllerPos);
  const onWorkingGround = home.clustered.filter((pos) => reservedGround.has(keyOf(pos)));
  if (onWorkingGround.length > 0) {
    console.error(
      `${home.grid.name}: ${onWorkingGround.length} clustered structure(s) stand on the working ` +
        `ground ADR 0022 keeps the Layout off — ${onWorkingGround.map(keyOf).join(", ")}`
    );
    process.exit(1);
  }
}

// The self-check's own reads are not the bot's, so the counters start the
// run at zero and still answer "how many times did the bundle read each
// room's terrain" (ADR 0031). Zeroed here rather than after the hiring
// below, because the terrain memo is filled once per heap and the tick
// that fills it is the first tick the bundle runs — hiring included. A
// counter zeroed after that reads "never projected" for a room the bundle
// projects every tick.
for (const name of worldRooms) terrainReads.set(name, 0);

const { loop, decideCalls } = loadBundle(bundle);

// The fleet, before a tick is either warmed or measured: the bundle hires
// it against this level's bank, and the outpost crews follow it. Neither
// count is written down anywhere in this file (#144).
const { hired, hireTicks, bodyOf } = hireFleet(world, game, loop);
if (world.crew) world.crew(bodyOf, game);
// Where the hired fleet stands, which since #163 is no longer the home
// room for every body: the reserver row and the outposts' own Anchors are
// cast by the home spawn and stationed a room away (ADR 0042), so a line
// that read every hire as the home room's would over-count it by exactly
// the rows that left. Counted per home room since ADR 0052's `pair`, where
// there are two of them and "at home" is a different room for each colony.
// The crew stands outside the count entirely — it is not hired — and is
// named after it.
const homeNames = world.homeRooms.map((room) => room.name);
const atHome = (name) => world.creeps.filter((creep) => creep.room.name === name).length;
const homeHires = homeNames.reduce((total, name) => total + atHome(name), 0);
const outpostHires = hired - homeHires;
console.log(
  `hired ${hired} creep${hired === 1 ? "" : "s"} for ` +
    `${homeNames.map((name) => `${name} (${atHome(name)} standing there)`).join(" and ")} ` +
    `over ${hireTicks} ticks, by the bundle's own SpawnCreep intents` +
    (outpostHires ? `, ${outpostHires} of them stationed outside a home room` : "") +
    (world.creeps.length > hired ? `, plus ${world.creeps.length - hired} outpost crew` : "")
);

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

// Every colony's `decide`, tick by tick and class by class: the probe
// pushes one row per call and this reads the tick's rows back off it, so a
// tick that ran a colony fewer than the scenario declares is caught here
// rather than showing up as a mean over the wrong divisor.
const decideMs = { all: new Map(), perturbed: new Map(), quiet: new Map() };
// And the [[stage]] each colony was decided at, off the same rows: one
// name per home, overwritten every tick because a stage is a fact of the
// tick and this world never moves one.
const stages = new Map();
const recordDecide = (label, calls) => {
  const table = decideMs[label];
  for (const call of calls) {
    const rows = table.get(call.home) ?? [];
    rows.push(call.ms);
    table.set(call.home, rows);
  }
};

const ticks = { all: [], perturbed: [], quiet: [] };
for (let i = 0; i < TICKS; i++) {
  const moved = movesCensus();
  if (moved) world.perturb();
  const run = tickThrough(moved);
  decideCalls().length = 0;
  const start = performance.now();
  tickStart = start;
  run();
  const ms = performance.now() - start;
  const calls = decideCalls();
  // The colonies the scenario built against the colonies the bundle
  // actually decided for: a child whose spawn stands in a room the
  // controller does not answer `my` for is simply not living
  // (`Colony.living`), and nothing else in this report would say so — the
  // ms would just be one colony's under a two-colony heading.
  // Compared by name and not by count: a count alone passes any change
  // that keeps the number and moves the identity — a `Colony.declared`
  // edit that made the mother and her *outpost* the two living colonies of
  // the `pair` world would print a `decide by colony` table headed by a
  // room this scenario furnished as an outpost and never as a colony, and
  // the two-colony mean under it would be over the wrong two colonies.
  const decided = calls.map((call) => call.home).sort();
  const declared = [...world.colonies].sort();
  if (decided.length !== declared.length || decided.some((home, i) => home !== declared[i])) {
    throw new Error(
      `the ${scenario} scenario declares ${world.colonies.length} living colon` +
        `${world.colonies.length === 1 ? "y" : "ies"} (${world.colonies.join(", ")}) and the ` +
        `bundle ran decide ${calls.length} time${calls.length === 1 ? "" : "s"} this tick ` +
        `(${calls.map((call) => call.home).join(", ") || "none"}): ` +
        "`Colony.living` is not reading this world the way the scenario describes it"
    );
  }
  for (const call of calls) stages.set(call.home, call.stage);
  recordDecide("all", calls);
  recordDecide(moved ? "perturbed" : "quiet", calls);
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
printDecideByColony(classes, decideMs, ticks, stages);

// Per room, because ADR 0041 layered the memo by room name: the number to
// read is one read per room the bundle projected, over the whole run. Read
// off the counters rather than off the modelled rooms, so a room the
// scenario did not build and the bundle asked for anyway — a declared
// outpost the `stub` world answers as solid rock — is counted where a
// reader can see it.
console.log(
  `engine terrain reads over ${hireTicks + WARMUP + TICKS} ticks (Game.map.getRoomTerrain): ` +
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

// What the bundle actually projected, against what the world holds. The
// scan set is the spawn room plus every declared outpost the stand-down
// gate leaves standing (ADR 0041, ADR 0043), so with the constant filled
// (#126) an outpost run reads all three of its rooms and this says
// nothing. It speaks when a room the world holds is left out of the scan
// — a declaration removed, or a stand-down shutting one — because those
// ms are then fewer rooms' projection than the world in front of it, and
// nothing else in the report would say so.
const projected = wallCounts.filter(([name]) => terrainReads.get(name) > 0).map(([name]) => name);
const unprojected = worldRooms.filter((name) => !projected.includes(name));
if (unprojected.length) {
  console.log(
    `projection: the bundle read terrain for ${projected.join(", ") || "no room"} and never for ` +
      `${unprojected.join(", ")} — those rooms are in the world but outside the scan set, which ` +
      "is the colony's declared outposts less whatever ADR 0043's stand-down is withholding. " +
      "These ms are the projected rooms' and not the whole world's."
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
