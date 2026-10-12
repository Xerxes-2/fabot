// The scenario library (ARCHITECTURE.md §10.2): each scenario in scenarios/
// is a start, a number of ticks and bounds on what the run measured. Starts
// are recipes (room, seed, late, node), so no world is checked in; scenarios
// with one start share one soak, run to the longest of their ticks.
//
// Usage: npm run scenario -- [name...] [--bot main.js] [--jobs J]
//   name    the scenarios to run (default all), by file name
//   --bot   the bundle (default dist/main.js)
//   --jobs  soaks at once (default half the cores)
//
// A scenario file: { "why": the invariant, "start": { "room", "seed",
// "late"?, "node"? }, "ticks", "assert": [{ "measure", "from"?, "to"?,
// "max" }] }; a measure is one of `measures` below, read off the soak's
// output over ticks [from, to). Exits non-zero when a bound fails or a soak does.
import { spawn, spawnSync } from "node:child_process";
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { availableParallelism } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const LIBRARY = join(HERE, "scenarios");
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const names = [];
const flags = {};
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++)
  if (rest[i] === "--bot" || rest[i] === "--jobs") flags[rest[i]] = rest[++i];
  else if (rest[i].startsWith("--")) fail("usage: npm run scenario -- [name...] [--bot main.js] [--jobs J]");
  else names.push(rest[i]);
const bot = resolve(flags["--bot"] ?? join(REPO, "dist/main.js"));
if (!existsSync(bot)) fail(`no bundle at ${bot}: npm run build first`);
const jobs = Number(flags["--jobs"] ?? Math.max(1, Math.floor(availableParallelism() / 2)));

const all = readdirSync(LIBRARY).filter((f) => f.endsWith(".json")).map((f) => basename(f, ".json"));
for (const n of names) if (!all.includes(n)) fail(`no scenario ${n}; the library: ${all.join(", ")}`);
const scenarios = (names.length ? names : all).map((name) => ({ name, ...JSON.parse(readFileSync(join(LIBRARY, `${name}.json`), "utf8")) }));

// Ticks are counted from the bot's first, so a window means the same on every start.
const samplesOf = (dir) => readFileSync(join(dir, "samples.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
const at = (samples, t) => samples.find((s) => s.t >= t) ?? samples.at(-1);
const within = (samples, a) => samples.filter((s) => s.t >= (a.from ?? 0) && s.t < (a.to ?? Infinity));
const measures = {
  // Energy lost to pile decay.
  decayed: (samples, a) => at(samples, a.to ?? Infinity).ledger.decayed - at(samples, a.from ?? 0).ledger.decayed,
  // Harvesters seen digging from more than one tile.
  harvestersOnTwoPosts: (samples, a) => {
    const tiles = new Map();
    for (const s of within(samples, a))
      for (const b of s.bodies) {
        const m = /^(harvester-\S+)@(\S+) \S+ \S+ .*harvest/.exec(b);
        if (m) tiles.set(m[1], (tiles.get(m[1]) ?? new Set()).add(m[2]));
      }
    return [...tiles.values()].filter((t) => t.size > 1).length;
  },
  // The first tick a container stands beside a source (never: Infinity).
  firstSourceContainer: (samples) => samples.find((s) => s.sourceContainers > 0)?.t ?? Infinity,
  faults: (_, __, dir) => readFileSync(join(dir, "faults.jsonl"), "utf8").split("\n").filter(Boolean).length,
};
for (const s of scenarios) for (const a of s.assert) if (!measures[a.measure]) fail(`${s.name}: no measure ${a.measure}`);

const keyOf = ({ room, seed, late = 0, node }) => `scenario-${basename(bot, ".js")}-${room}-s${seed}${late ? `l${late}` : ""}${node ? `-${node}` : ""}`;
const runs = new Map();
for (const s of scenarios) {
  const run = runs.get(keyOf(s.start)) ?? { name: keyOf(s.start), start: s.start, ticks: 0 };
  run.ticks = Math.max(run.ticks, s.ticks);
  runs.set(run.name, run);
}

// Built once here: parallel builds of one project collide.
if ([...runs.values()].some((r) => r.start.node) && spawnSync("dotnet", ["build", "-c", "Release", "--nologo", "-v", "q", join(REPO, "scripts/plan")], { stdio: "inherit" }).status !== 0)
  fail("scripts/plan did not build");
const started = Date.now();
const soak = ({ name, start, ticks }) =>
  new Promise((done) => {
    const args = ["--room", start.room, "--seed", String(start.seed), "--late", String(start.late ?? 0), "--ticks", String(ticks), "--run", name, "--bot", bot];
    if (start.node) args.push("--node", start.node);
    const child = spawn("node", [join(HERE, "soak.mjs"), ...args], { stdio: "ignore", env: { ...process.env, FABOT_PLAN_BUILT: "1" } });
    child.on("exit", (code) => done(code));
  });
const queue = [...runs.values()];
console.error(`${scenarios.length} scenarios in ${queue.length} soaks, ${jobs} at a time`);
await Promise.all(
  Array.from({ length: jobs }, async () => {
    for (let run; (run = queue.shift()); ) run.code = await soak(run);
  }),
);

let failed = 0;
for (const s of scenarios) {
  const run = runs.get(keyOf(s.start));
  const dir = join(REPO, "soak-runs", run.name);
  if (run.code !== 0) {
    failed++;
    console.log(`FAIL ${s.name}: the soak failed (exit ${run.code}); see ${dir}/server.log`);
    continue;
  }
  const samples = samplesOf(dir);
  for (const a of s.assert) {
    const value = measures[a.measure](samples, a, dir);
    const ok = value <= a.max;
    if (!ok) failed++;
    const window = a.from !== undefined || a.to !== undefined ? ` t${a.from ?? 0}-${a.to ?? s.ticks}` : "";
    console.log(`${ok ? "pass" : "FAIL"} ${s.name}: ${a.measure}${window} ${value} (≤ ${a.max})`);
  }
}
console.log(`${failed ? `${failed} failed` : "all passed"} in ${((Date.now() - started) / 1000).toFixed(0)} s`);
process.exit(failed ? 1 : 0);
