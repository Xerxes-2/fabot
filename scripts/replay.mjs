// Two bundles' decisions over the same recorded ticks (a soak's capture.jsonl,
// `npm run scenario -- --capture`): each tick is stood as bench.mjs stands it
// (`bench-game.mjs`), from the Memory and world bundle A was handed then, and
// both bundles decide it. A refactor claims identical intents and Memory on
// every tick; a change of behaviour shows the first tick it decides apart,
// and what it decided. Ticks after the first difference still compare (each
// is stood from A's recorded world), so the count is how often B decides
// otherwise, not how far it drifts.
//
// Usage: npm run replay -- --a A.js [--b B.js] [--jobs J] [--show N] [--at T] [capture.jsonl | run dir | scenario tag]…
//   no --b      A against itself: whether the replay repeats
//   --at        also print tick T's intents and Memory as A decided them
//               (ARCHITECTURE.md §10.4: an incident replayed)
//   a tag       every soak-runs/scenario-<tag>-*/capture.jsonl
//   --show      the differing ticks detailed per capture (default 1)
//   --check     read.js reads again each fact it keeps across ticks, the
//               Atlas computes afresh each tree and walk it carries, and
//               stops on the first one apart
import { spawn } from "node:child_process";
import { existsSync, readdirSync, statSync } from "node:fs";
import { availableParallelism } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { load, stand } from "./bench-game.mjs";
import { ticksOf } from "./capture-read.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const RUNS = join(HERE, "..", "soak-runs");
const usage = "usage: npm run replay -- --a A.js [--b B.js] [--jobs J] [--show N] [--check] [capture.jsonl | run dir | scenario tag]…";
const fail = (m) => {
  console.error(m);
  process.exit(2);
};
const flags = {};
const targets = [];
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++)
  if (argv[i] === "--check") globalThis.__fabotCheck = true;
  else if (argv[i].startsWith("--")) {
    if (!["--a", "--b", "--jobs", "--show", "--worker", "--at"].includes(argv[i]) || argv[i + 1] === undefined) fail(usage);
    flags[argv[i]] = argv[++i];
  } else targets.push(argv[i]);
if (!flags["--a"]) fail(usage);
const show = Number(flags["--show"] ?? 1);

// One capture, in this process: a JSON line of what differed.
async function replayOne(file) {
  const bundles = [resolve(flags["--a"]), resolve(flags["--b"] ?? flags["--a"])];
  let bots;
  let last;
  let ticks = 0;
  const differ = [];
  let at;
  for await (const tick of ticksOf([file])) {
    // A window starts from a fresh load, as the capture's bot did after a gap.
    if (!last || last !== tick.time - 1) bots = bundles.map(load);
    last = tick.time;
    const [a, b] = [decide(bots[0], tick), decide(bots[1], tick)];
    ticks++;
    if (tick.time === Number(flags["--at"])) at = { t: tick.time, intents: a.intents, memory: a.memory };
    if (a.intents.join("\n") === b.intents.join("\n") && a.memory === b.memory) continue;
    differ.push(differ.length < show ? { t: tick.time, ...explain(a, b) } : { t: tick.time });
  }
  return { file, ticks, differ, at };
}

function decide(bot, tick) {
  const list = [];
  const game = stand(tick, list, () => ({}), () => 0);
  bot.loop();
  // An id reads as its creep's or spawn's name where it has one.
  const names = Object.fromEntries([...Object.entries(tick.creeps), ...Object.entries(tick.spawns)].map(([name, id]) => [id, name]));
  return { intents: list.map(([id, ...rest]) => JSON.stringify([names[id] ?? id, ...rest])), memory: game.memory() };
}

// The intents one side made and the other did not, and the Memory paths apart.
function explain(a, b) {
  const minus = (xs, ys) => {
    const left = new Map();
    for (const y of ys) left.set(y, (left.get(y) ?? 0) + 1);
    return xs.filter((x) => (left.get(x) ? (left.set(x, left.get(x) - 1), false) : true));
  };
  const paths = [];
  const walk = (x, y, at) => {
    if (paths.length >= 12 || JSON.stringify(x) === JSON.stringify(y)) return;
    if (x && y && typeof x === "object" && typeof y === "object" && Array.isArray(x) === Array.isArray(y))
      for (const k of new Set([...Object.keys(x), ...Object.keys(y)])) walk(x[k], y[k], `${at}.${k}`);
    else paths.push(`${at}: ${clip(x)} → ${clip(y)}`);
  };
  walk(JSON.parse(a.memory || "{}"), JSON.parse(b.memory || "{}"), "Memory");
  const onlyA = minus(a.intents, b.intents);
  const onlyB = minus(b.intents, a.intents);
  return { onlyA, onlyB, reordered: !onlyA.length && !onlyB.length && a.intents.join() !== b.intents.join(), memory: paths };
}
const clip = (v) => {
  const s = JSON.stringify(v) ?? "absent";
  return s.length > 120 ? `${s.slice(0, 117)}...` : s;
};

if (flags["--worker"]) {
  process.stdout.write(JSON.stringify(await replayOne(flags["--worker"])));
  process.exit(0);
}

// A target is a capture, a run directory, or a scenario tag's runs.
const files = targets.flatMap((t) => {
  if (existsSync(t) && statSync(t).isFile()) return [resolve(t)];
  if (existsSync(join(t, "capture.jsonl"))) return [resolve(t, "capture.jsonl")];
  const runs = readdirSync(RUNS).filter((d) => d.startsWith(`scenario-${t}-`) && existsSync(join(RUNS, d, "capture.jsonl")));
  if (!runs.length) fail(`${t}: no capture.jsonl, run directory, or soak-runs/scenario-${t}-* with a capture`);
  return runs.map((d) => join(RUNS, d, "capture.jsonl"));
});
if (!files.length) fail(usage);

const jobs = Number(flags["--jobs"] ?? Math.max(1, availableParallelism() >> 1));
const began = Date.now();
const queue = [...files].sort((x, y) => statSync(y).size - statSync(x).size);
const results = [];
const worker = (file) =>
  new Promise((done) => {
    const child = spawn(process.execPath, [fileURLToPath(import.meta.url), "--worker", file, "--a", flags["--a"], ...(flags["--b"] ? ["--b", flags["--b"]] : []), "--show", String(show), ...(globalThis.__fabotCheck ? ["--check"] : []), ...(flags["--at"] ? ["--at", flags["--at"]] : [])], {
      stdio: ["ignore", "pipe", "inherit"],
    });
    let out = "";
    child.stdout.on("data", (d) => (out += d));
    child.on("exit", (code) => done(code === 0 ? JSON.parse(out) : { file, ticks: 0, differ: [], crashed: code }));
  });
await Promise.all(
  Array.from({ length: jobs }, async () => {
    for (let file; (file = queue.shift()); ) results.push(await worker(file));
  }),
);

const name = (file) => basename(dirname(file));
let apart = 0;
for (const r of results.sort((x, y) => name(x.file).localeCompare(name(y.file)))) {
  if (r.crashed !== undefined) {
    apart++;
    console.log(`CRASH ${name(r.file)} (exit ${r.crashed})`);
    continue;
  }
  if (r.at) {
    console.log(`${name(r.file)} t${r.at.t}: ${r.at.intents.length} intents`);
    for (const i of r.at.intents) console.log(`  ${i}`);
    console.log(`  Memory ${r.at.memory}`);
  }
  if (!r.differ.length) continue;
  apart++;
  console.log(`DIFFER ${name(r.file)}: ${r.differ.length}/${r.ticks} ticks, first t${r.differ[0].t}`);
  for (const d of r.differ.filter((d) => d.onlyA)) {
    console.log(`  t${d.t}${d.reordered ? " (same intents, another order)" : ""}`);
    for (const i of d.onlyA.slice(0, 15)) console.log(`    A ${i}`);
    for (const i of d.onlyB.slice(0, 15)) console.log(`    B ${i}`);
    for (const p of d.memory) console.log(`    ${p}`);
  }
}
const ticks = results.reduce((n, r) => n + r.ticks, 0);
console.log(`${results.length} captures, ${ticks} ticks: ${apart ? `${apart} apart` : "identical"} in ${((Date.now() - began) / 1000).toFixed(1)} s`);
process.exit(apart ? 1 : 0);
