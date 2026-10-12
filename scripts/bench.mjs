// The benchmark (ARCHITECTURE.md §9.1) over a soak's capture (`npm run soak
// -- --capture a-b`). The cost model is live's: Memory parsed from the string
// on first touch and stringified after the loop, 0.2 a call on `getUsed`
// (xxscreeps charges neither, so its `memory` and `execute` read lower).
// A window starts from a fresh load, as after a global reset: its first
// --cold ticks are apart. A claim is the min-of-rounds B/A with identical
// fingerprints (each tick's intents and Memory).
//
// Usage: npm run bench -- --a A.js [--b B.js] [--rounds R] [--cold K] capture.jsonl…
//   node --cpu-prof --max-old-space-size=256 scripts/bench.mjs --a A.js …   one bundle's profile
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { resolve } from "node:path";
import { getHeapStatistics } from "node:v8";

const usage = "usage: npm run bench -- --a A.js [--b B.js] [--rounds R] [--cold K] capture.jsonl…";
const fail = (m) => {
  console.error(m);
  process.exit(1);
};
const flags = {};
const files = [];
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++)
  if (argv[i].startsWith("--")) {
    if (!["--a", "--b", "--rounds", "--cold"].includes(argv[i]) || argv[i + 1] === undefined) fail(usage);
    flags[argv[i]] = argv[++i];
  } else files.push(argv[i]);
if (!flags["--a"] || files.length === 0) fail(usage);
const rounds = Number(flags["--rounds"] ?? 5);
const cold = Number(flags["--cold"] ?? 3);
const bundles = { A: resolve(flags["--a"]), ...(flags["--b"] ? { B: resolve(flags["--b"]) } : {}) };
const sides = Object.keys(bundles);

const windows = [];
for (const f of files)
  for (const line of readFileSync(f, "utf8").split("\n").filter(Boolean)) {
    const tick = JSON.parse(line);
    for (const r of tick.rooms) r.terrain = Uint8Array.from(r.terrain, Number);
    const w = windows.at(-1);
    if (w && w.at(-1).time === tick.time - 1) w.push(tick);
    else windows.push([tick]);
  }

const VERBS = ["move", "say", "harvest", "transfer", "withdraw", "pickup", "build", "repair", "dismantle", "upgradeController", "reserveController",
  "attackController", "claimController", "signController", "claimReactor", "drop", "attack", "heal", "rangedHeal", "rangedAttack", "rangedMassAttack",
  "spawnCreep", "transferEnergy", "setPublic", "destroy", "remove", "observeRoom", "runReaction", "send", "activateSafeMode"];
const STORE = {
  getCapacity() {
    return this.capacity;
  },
  getUsedCapacity(r) {
    return r === undefined ? Object.values(this).reduce((s, n) => s + n, 0) : (this[r] ?? 0);
  },
  getFreeCapacity(r) {
    return this.capacity - this.getUsedCapacity(r);
  },
};
const idOf = (a) => (a !== null && typeof a === "object" && !Array.isArray(a) ? (a.id ?? a.name) : a);

function stand(tick, intents, clock) {
  const actor = {};
  for (const verb of VERBS)
    actor[verb] = function (...args) {
      intents.push([this.id ?? this.name, verb, ...args.map(idOf)]);
      return 0;
    };
  const byId = new Map();
  for (const [key, s] of Object.entries(tick.objects)) {
    const o = Object.assign(Object.create(actor), s);
    if (s.store) o.store = Object.defineProperty(Object.assign(Object.create(STORE), s.store.held), "capacity", { value: s.store.capacity });
    byId.set(key, o);
  }
  const named = (ids) => Object.fromEntries(Object.entries(ids).map(([name, id]) => [name, byId.get(id)]));
  const rooms = {};
  const terrain = {};
  for (const r of tick.rooms) {
    const found = Object.fromEntries(Object.entries(r.find).map(([type, ids]) => [type, ids.map((id) => byId.get(id))]));
    terrain[r.name] = r.terrain;
    rooms[r.name] = {
      name: r.name,
      energyAvailable: r.energyAvailable,
      energyCapacityAvailable: r.energyCapacityAvailable,
      controller: r.controller === undefined ? undefined : byId.get(r.controller),
      find: (type) => found[type] ?? [],
      createConstructionSite: (...args) => (intents.push([r.name, "createConstructionSite", ...args]), 0),
    };
  }
  let parsed;
  const segments = {};
  Object.defineProperty(globalThis, "Memory", { configurable: true, get: () => (parsed ??= JSON.parse(tick.memory)) });
  globalThis.RawMemory = { segments, get: () => tick.memory };
  globalThis.Game = {
    time: tick.time,
    cpu: { ...tick.cpu, getUsed: clock, getHeapStatistics },
    map: { getRoomTerrain: (name) => ({ getRawBuffer: () => terrain[name].slice() }) },
    rooms,
    creeps: named(tick.creeps),
    spawns: named(tick.spawns),
    constructionSites: Object.fromEntries(tick.constructionSites.map((id) => [id, byId.get(id)])),
    getObjectById: (id) => byId.get(id) ?? null,
  };
  return { segments, memory: () => (parsed === undefined ? tick.memory : JSON.stringify(parsed)) };
}

const require = createRequire(import.meta.url);
const load = (path) => {
  delete require.cache[path];
  return require(path);
};

function runTick(bot, tick) {
  const intents = [];
  let start;
  const game = stand(tick, intents, () => performance.now() - start + 0.2 * intents.length);
  start = performance.now();
  bot.loop();
  const flushed = performance.now();
  const memory = game.memory();
  const stringify = performance.now() - flushed;
  const observed = JSON.parse(game.segments[1]);
  const ms = Object.fromEntries(observed.phases);
  Object.assign(ms, { bot: observed.cpu, stringify, tick: observed.cpu + stringify });
  const print = createHash("sha256").update(JSON.stringify(intents)).update(memory).digest("hex").slice(0, 12);
  return { ms, print, faults: observed.faults.filter((f) => f.tick === tick.time) };
}

// runs[side][round][i]: the i-th tick of all windows in order.
const runs = Object.fromEntries(sides.map((s) => [s, []]));
const coldAt = windows.flatMap((w) => w.map((_, i) => i < cold));
for (let r = 0; r < rounds; r++) {
  for (const s of sides) runs[s].push([]);
  for (const w of windows) {
    const bots = Object.fromEntries(sides.map((s) => [s, load(bundles[s])]));
    w.forEach((tick, i) => {
      const order = (r + i) % 2 === 0 ? sides : [...sides].reverse();
      for (const s of order) runs[s][r].push(runTick(bots[s], tick));
    });
  }
  console.error(`round ${r + 1}/${rounds}`);
}

const ticks = coldAt.length;
const faults = sides.flatMap((s) => runs[s][0].flatMap((t) => t.faults.map((f) => `${s} t${f.tick} ${f.where}: ${f.message}`)));
for (const f of faults.slice(0, 5)) console.log(`fault ${f}`);
const unstable = sides.filter((s) => runs[s].some((round) => round.some((t, i) => t.print !== runs[s][0][i].print)));
const differ = sides.length === 2 ? runs.A[0].filter((t, i) => t.print !== runs.B[0][i].print).length : 0;
console.log(
  `${windows.length} windows, ${ticks} ticks (${coldAt.filter(Boolean).length} cold), ${rounds} rounds; ` +
    `fingerprints ${unstable.length ? `UNSTABLE in ${unstable}` : "stable"}${sides.length === 2 ? `, A/B ${differ ? `DIFFER on ${differ} ticks` : "identical"}` : ""}`,
);

const phases = [...new Set(runs.A[0].flatMap((t) => Object.keys(t.ms)))];
const table = (pick) => {
  const n = coldAt.filter(pick).length;
  const at = (s, p) => {
    const perRound = runs[s].map((round) => round.reduce((sum, t, i) => sum + (pick(coldAt[i]) ? (t.ms[p] ?? 0) : 0), 0) / n);
    const min = runs[s][0].reduce((sum, _, i) => sum + (pick(coldAt[i]) ? Math.min(...runs[s].map((round) => round[i].ms[p] ?? 0)) : 0), 0) / n;
    return { perRound, min };
  };
  const mean = (xs) => xs.reduce((a, b) => a + b, 0) / xs.length;
  const se = (xs) => Math.sqrt(xs.reduce((a, x) => a + (x - mean(xs)) ** 2, 0) / Math.max(1, xs.length - 1) / xs.length);
  const pct = (x) => `${x >= 0 ? "+" : ""}${(100 * x).toFixed(1)}%`;
  const head = sides.length === 2 ? ["phase", "A min", "B min", "min B/A", "mean B/A", "±2se"] : ["phase", "min ms", "mean ms", "±2se", "share"];
  console.log(head.map((h) => h.padStart(10)).join(""));
  const total = at("A", "tick").min;
  for (const p of phases) {
    const a = at("A", p);
    let row;
    if (sides.length === 2) {
      const b = at("B", p);
      const ratios = a.perRound.map((x, k) => (x > 0 ? b.perRound[k] / x - 1 : 0));
      row = [p, a.min.toFixed(3), b.min.toFixed(3), pct(b.min / a.min - 1), pct(mean(ratios)), `${(200 * se(ratios)).toFixed(1)}%`];
    } else row = [p, a.min.toFixed(3), mean(a.perRound).toFixed(3), (2 * se(a.perRound)).toFixed(3), `${((100 * a.min) / total).toFixed(1)}%`];
    console.log(row.map((c) => String(c).padStart(10)).join(""));
  }
};
console.log("warm, ms a tick");
table((c) => !c);
console.log("cold, ms a tick");
table((c) => c);
if (faults.length || unstable.length || differ) process.exitCode = 1;
