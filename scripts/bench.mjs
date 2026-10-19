// The benchmark (ARCHITECTURE.md §9.1) over a soak's capture (`npm run soak
// -- --capture a-b`) through the official engine's `Game` (`bench-game.mjs`),
// Memory stringified after the loop. xxscreeps reuses last tick's Memory
// object and charges no intent, so its `memory`, `write` and `execute` read
// lower. A window starts from a fresh load, as after a global reset: its first
// --cold ticks are apart, the JIT still settling (the atlas reads 5× its
// steady cost over the first 40). A round streams the capture a tick at a
// time: the heap is the live limit, and a takeover capture outgrows it. A
// claim is the min-of-rounds B/A with identical fingerprints (each tick's
// intents and Memory).
//
// --counts replays A once and prints §9.3's counts a tick per window instead:
// floods, flood-table hits, tiles relaxed, by the caller's named frames.
//
// Usage: npm run bench -- --a A.js [--b B.js] [--rounds R] [--cold K] capture.jsonl…
//        npm run bench -- --a A.js --counts capture.jsonl…
//   node --cpu-prof --max-old-space-size=256 scripts/bench.mjs --a A.js …   one bundle's profile
import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { resolve } from "node:path";
import { createInterface } from "node:readline";
import { getHeapStatistics } from "node:v8";
import { load, stand } from "./bench-game.mjs";

const usage = "usage: npm run bench -- --a A.js [--b B.js] [--rounds R] [--cold K | --counts] capture.jsonl…";
const fail = (m) => {
  console.error(m);
  process.exit(1);
};
const flags = {};
const files = [];
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++)
  if (argv[i] === "--counts") flags.counts = true;
  else if (argv[i].startsWith("--")) {
    if (!["--a", "--b", "--rounds", "--cold"].includes(argv[i]) || argv[i + 1] === undefined) fail(usage);
    flags[argv[i]] = argv[++i];
  } else files.push(argv[i]);
if (!flags["--a"] || files.length === 0) fail(usage);
const rounds = Number(flags["--rounds"] ?? 5);
const cold = Number(flags["--cold"] ?? 50);
const bundles = { A: resolve(flags["--a"]), ...(flags["--b"] ? { B: resolve(flags["--b"]) } : {}) };
const sides = Object.keys(bundles);

async function* replay() {
  let last;
  for (const f of files)
    for await (const line of createInterface({ input: createReadStream(f), crlfDelay: Infinity })) {
      if (!line) continue;
      const tick = JSON.parse(line);
      for (const r of tick.rooms) r.terrain = Uint8Array.from(r.terrain, Number);
      const i = last && last.time === tick.time - 1 ? last.i + 1 : 0;
      last = { time: tick.time, i };
      yield [tick, i];
    }
}

function runTick(bot, tick) {
  const intents = [];
  let start;
  const game = stand(tick, intents, getHeapStatistics, () => performance.now() - start);
  globalThis.__fabotRead = bot.read;
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

if (flags.counts) {
  Error.stackTraceLimit = 40;
  const skip = new Set(["flood", "AtlasModule_field", "AtlasModule_travelTicks", "ticksTo", "Object.loop", "loop", "runTick", "run"]);
  let tally;
  globalThis.__fabotCount = (what, n) => {
    const who = [...new Error().stack.matchAll(/at (?:Object\.)?([\w$]+) \(/g)].map((m) => m[1]).filter((f) => !skip.has(f) && !f.startsWith("__")).slice(0, 2).join("<");
    const row = (tally[who] ??= { floods: 0, hits: 0, tiles: 0 });
    if (what === "hit") row.hits += n;
    else (row.floods++, (row.tiles += n));
  };
  const windows = [];
  let bot;
  for await (const [tick, i] of replay()) {
    if (i === 0) (windows.push((tally = {})), (bot = load(bundles.A)));
    runTick(bot, tick);
    tally.length = i + 1;
  }
  for (const [wi, { length, ...w }] of windows.entries()) {
    const rows = Object.entries(w).sort((a, b) => b[1].tiles - a[1].tiles);
    const sum = (k) => rows.reduce((s, [, r]) => s + r[k], 0);
    const per = (x) => (x / length).toFixed(1);
    console.log(`window ${wi} (${length} ticks): ${per(sum("floods"))} floods a tick, hit rate ${((100 * sum("hits")) / (sum("hits") + sum("floods"))).toFixed(1)}%, ${(sum("tiles") / sum("floods")).toFixed(0)} tiles a flood`);
    for (const [who, r] of rows) console.log(`  ${who.padEnd(48)} floods ${per(r.floods).padStart(7)} hits ${per(r.hits).padStart(7)} tiles ${per(r.tiles).padStart(9)}`);
  }
  process.exit(0);
}

// runs[side][round][i]: the i-th tick of all windows in order.
const runs = Object.fromEntries(sides.map((s) => [s, []]));
const coldAt = [];
let windows = 0;
for (let r = 0; r < rounds; r++) {
  for (const s of sides) runs[s].push([]);
  let bots;
  for await (const [tick, i] of replay()) {
    if (i === 0) bots = Object.fromEntries(sides.map((s) => [s, load(bundles[s])]));
    if (r === 0) (coldAt.push(i < cold), (windows += i === 0));
    const order = (r + i) % 2 === 0 ? sides : [...sides].reverse();
    for (const s of order) runs[s][r].push(runTick(bots[s], tick));
  }
  console.error(`round ${r + 1}/${rounds}`);
}

const ticks = coldAt.length;
const faults = sides.flatMap((s) => runs[s][0].flatMap((t) => t.faults.map((f) => `${s} t${f.tick} ${f.where}: ${f.message}`)));
for (const f of faults.slice(0, 5)) console.log(`fault ${f}`);
const unstable = sides.filter((s) => runs[s].some((round) => round.some((t, i) => t.print !== runs[s][0][i].print)));
const differ = sides.length === 2 ? runs.A[0].filter((t, i) => t.print !== runs.B[0][i].print).length : 0;
console.log(
  `${windows} windows, ${ticks} ticks (${coldAt.filter(Boolean).length} cold), ${rounds} rounds; ` +
    `fingerprints ${unstable.length ? `UNSTABLE in ${unstable}` : "stable"}${sides.length === 2 ? `, A/B ${differ ? `DIFFER on ${differ} ticks` : "identical"}` : ""}`,
);

const phases = [...new Set(runs.A[0].flatMap((t) => Object.keys(t.ms)))];
const table = (pick) => {
  const n = coldAt.filter(pick).length;
  if (n === 0) return console.log("  none: no window is longer than --cold");
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
