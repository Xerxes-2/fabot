// The soak over several rooms, seeds and bundles at once, so an economy change
// is judged against its base on more than one room's terrain and more than
// one seed (ARCHITECTURE.md §10.2): `sim` (krebs, official rules) once per
// (binary, seed, room), many at a time, then one table.
//
// Usage: npm run soak:many -- [--bot [name=]a,[name=]b] [--seed 1,2,1+7] [--room A,B,...]
//                             [--ticks T] [--jobs J] [--tag name] [--node N] [--fresh]
//   --bot    the sim binaries to compare (default this tree's, `npm run build:sim`);
//            the first is the base the others are read against. A name
//            defaults to the file's.
//   --seed   the starts for every room (default 1,2,1+7): an engine seed, or seed+K
//            for a bot that sits out its first K ticks (soak.mjs --late), a
//            nudge the seed alone does not give a room with few tie-breaks
//   --room   these starts instead of the default: the extremes of harvest
//            seats, both sources with one seat (a small harvester cannot fill
//            a source) and both with four or more (bodies can crowd in
//            instead), one source (E23S9 on Season 11's map) and the late
//            game. A start is [<world>:]<room>[@<node>], <world> one of
//            worlds/ with no home of ours (soak.mjs --world, --node). On a
//            world from one spawn, as the library's scenarios there, our safe
//            mode is kept to activate (--ended)
//
// Every run holds `measures.mjs`'s invariants; a run that breaks one is named
// under the table, and the exit is non-zero.
//   --ticks  per run (default 30000; a node's 10000)
//   --node   start every run without its own node at this one
//   --jobs   runs at once (default every allowed core)
//   --tag    output names: soak-runs/<tag>-<bundle>-s<seed>-<room>/ (default many)
//   --fresh  run even where the cache holds this bundle, room, seed and ticks
//   --reset-every N  beside each bundle, its twin `<name>-reset` reset (heap lost) every N ticks
//   --tuning {"name": {key: value}, ...}  beside each bundle, its twin `<bundle>-<name>` playing that
//            Memory.v2.tuning (core/base/tuning/mod.rs): a parameter sweep
// A start sim cannot stand is named under the table.
//
// A finished run is cached under soak-runs/.cache/ by its binary, start, seed, ticks and the soak scripts (with the
// plan a node is built from, and the world), so a base that
// has not changed is not run again.
import { createHash } from "node:crypto";
import { cpSync, existsSync, readFileSync, readdirSync, rmSync } from "node:fs";
import { availableParallelism } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { sweep } from "./prune.mjs";
import { exposureOf, invariants } from "./measures.mjs";
import { unstood } from "../sim/node.mjs";
import { REPO, TEMPLATE as SIM_TEMPLATE, buildPlan, jsonLines, runSim, simBinary, soakDir } from "../sim/run.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const RUNS = soakDir();
const CACHE = join(RUNS, ".cache");
const usage =
  "usage: npm run soak:many -- [--bot [name=]a,...] [--seed 1,2,1+7] [--room [world:]A[@N],...] [--ticks T] [--jobs J] [--tag name] [--node N] [--fresh] [--reset-every N] [--tuning <{name: tuning}>]";
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const flags = {};
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++) {
  if (rest[i] === "--fresh") {
    flags[rest[i]] = true;
    continue;
  }
  if (!["--room", "--ticks", "--seed", "--jobs", "--tag", "--bot", "--node", "--reset-every", "--tuning"].includes(rest[i])) fail(usage);
  const value = rest[i + 1];
  if (value === undefined || value.startsWith("--")) fail(`${rest[i]} needs a value\n${usage}`);
  flags[rest[i++]] = value;
}
const whole = (text, flag, min) => {
  const n = Number(text);
  if (!(Number.isInteger(n) && n >= min)) fail(`${flag} needs whole numbers ≥ ${min}`);
  return n;
};
const node = flags["--node"] ?? "";
if (!/^[A-Za-z0-9]*$/.test(node)) fail("--node: a node's name, as N3");
const parseSeeds = (text) =>
  text.split(",").map((spec) => {
    const [seed, late = "0"] = spec.split("+");
    return { seed: whole(seed, "--seed", 0), late: whole(late, "--seed", 0), label: spec };
  });
const allStarts = (flags["--room"] ?? "W9N8,W8N3,season-outpost-ring:E23S9,season-late-seat:W12S28@N7").split(",").map((spec) => {
  const m = /^(?:([\w-]+):)?([WE]\d+[NS]\d+)(?:@([A-Za-z0-9]+))?$/.exec(spec);
  if (!m) fail(`--room: "${spec}" is not [world:]room[@node]`);
  const world = m[1] && join(HERE, "worlds", `${m[1]}.json`);
  if (world && !existsSync(world)) fail(`--room: no world ${world}`);
  const at = m[3] ?? node;
  return { label: spec, room: m[2], world, node: at, ended: Boolean(world && !at), seeds: parseSeeds(flags["--seed"] ?? "1,2,1+7"), ticks: whole(flags["--ticks"] ?? (at ? 10000 : 30000), "--ticks", 1) };
});
const starts = allStarts.filter((s) => !(s.node && unstood(s.node)));
const unrun = allStarts.filter((s) => !starts.includes(s)).map((s) => `not run: ${s.label} (${unstood(s.node)})`);
unrun.forEach((u) => console.error(u));
if (!starts.length) fail("no start to run");
const jobs = whole(flags["--jobs"] ?? availableParallelism(), "--jobs", 1);
const tag = flags["--tag"] ?? "many";
if (!/^[A-Za-z0-9-]+$/.test(tag)) fail("--tag: letters, digits, dashes");

const resetEvery = whole(flags["--reset-every"] ?? 0, "--reset-every", 0);
const bundles = (flags["--bot"] ?? simBinary() ?? fail("sim did not build")).split(",").map((spec) => {
  const [name, path] = spec.includes("=") ? spec.split("=") : [basename(spec, ".js"), spec];
  if (!/^[A-Za-z0-9-]+$/.test(name)) fail(`--bot: "${name}" is not a name (letters, digits, dashes)`);
  const file = resolve(path);
  if (!existsSync(file)) fail(`--bot: no file at ${file}`);
  return { name, file, resetEvery: 0 };
});
let tunings = {};
try {
  tunings = JSON.parse(flags["--tuning"] ?? "{}");
} catch {
  fail("--tuning: a JSON object of name → tuning");
}
if (Object.keys(tunings).some((n) => !/^[A-Za-z0-9-]+$/.test(n))) fail("--tuning: names are letters, digits, dashes");
const tuned = bundles.flatMap((b) => [b, ...Object.entries(tunings).map(([n, t]) => ({ ...b, name: `${b.name}-${n}`, tuning: JSON.stringify(t) }))]);
const bots = tuned.flatMap((b) => (resetEvery ? [b, { ...b, name: `${b.name}-reset`, resetEvery }] : [b]));
if (new Set(bots.map((b) => b.name)).size < bots.length) fail("--bot: two bundles share a name");

// The cache key: the bundle's code without its version stamp, so the same
// code built at another change hits; and the soak's own scripts, so a changed
// sampler misses.
const hash = (...parts) => {
  const h = createHash("sha256");
  for (const p of parts) h.update(p);
  return h.digest("hex").slice(0, 16);
};
const scriptsHash = hash(
  ...[join(HERE, "../sim/node.mjs"), join(HERE, "../sim/run.mjs"), join(HERE, "engaged.mjs"), join(HERE, "measures.mjs"), join(HERE, "seat-tile.mjs")].map((f) => readFileSync(f)),
);
// A node, and a fresh colony's first spawn, stand on the plan of this tree's Layout (and the crates it builds on), whatever the bundle.
const crates = ["base", "atlas", "layout"].flatMap((c) => readdirSync(join(REPO, "core", c), { recursive: true }).filter((f) => f.endsWith(".rs") || f === "Cargo.toml").sort().map((f) => join("core", c, f)));
const planHash = hash(...["src/bin/plan.rs", ...crates].map((f) => readFileSync(join(REPO, f))));
for (const b of bots) b.code = hash(readFileSync(b.file));

const runs = bots.flatMap((bot) =>
  starts.flatMap((start) =>
    start.seeds.map((seed) => ({
      bot,
      seed,
      start,
      name: `${tag}-${bot.name}-s${seed.seed}${seed.late ? `l${seed.late}` : ""}-${start.label.replace(/[:@]/g, "-")}`,
      key: hash(bot.code, start.room, planHash + start.node, readFileSync(start.world ?? SIM_TEMPLATE), String(seed.seed), String(seed.late), String(start.ticks), scriptsHash, String(bot.resetEvery), bot.tuning ?? ""),
    })),
  ),
);

sweep(RUNS);
const cached = (run) => !flags["--fresh"] && existsSync(join(CACHE, run.key, "levels.json"));
// Two bundles of the same code share their runs.
const todo = runs.filter((r, i) => !cached(r) && runs.findIndex((o) => o.key === r.key) === i);
console.error(
  `${bots.map((b) => b.name).join(" vs ")}; ${starts.map((s) => `${s.label} ${s.ticks} ticks, seeds ${s.seeds.map((x) => x.label).join(" ")}`).join("; ")}; ` +
    `${runs.length - todo.length} cached, ${todo.length} to run, ${jobs} at a time`,
);

function soak(run) {
  const { room, world, node, ticks, ended } = run.start;
  return runSim(run.bot.file, { world, room, node, ticks, seed: run.seed.seed, late: run.seed.late, ended, resetEvery: run.bot.resetEvery, tuning: run.bot.tuning, out: join(RUNS, run.name) });
}

// Built once here: parallel builds of one project collide.
if (todo.some((r) => r.start.node) && !buildPlan()) fail("the plan tool did not build");
const codes = {};
const queue = [...todo];
await Promise.all(
  Array.from({ length: jobs }, async () => {
    while (queue.length) {
      const run = queue.shift();
      codes[run.name] = await soak(run);
      if (codes[run.name] === 0) {
        rmSync(join(CACHE, run.key), { recursive: true, force: true });
        cpSync(join(RUNS, run.name), join(CACHE, run.key), { recursive: true });
      }
      console.error(`${run.name}: ${codes[run.name] === 0 ? "done" : `failed (exit ${codes[run.name]})`}`);
    }
  }),
);

const ranAs = (run) => todo.find((r) => r.key === run.key);
const failed = (run) => ranAs(run) !== undefined && codes[ranAs(run).name] !== 0;
const dirOf = (run) => (failed(run) ? join(RUNS, ranAs(run).name) : join(CACHE, run.key));
const read = (run, file) => {
  const path = join(dirOf(run), file);
  return existsSync(path) ? readFileSync(path, "utf8") : null;
};

const rowOf = (run) => {
  const samples = jsonLines(join(dirOf(run), "samples.jsonl"));
  const levels = JSON.parse(read(run, "levels.json") ?? "[]");
  const faults = jsonLines(join(dirOf(run), "faults.jsonl")).length;
  const refusals = Object.values(JSON.parse(read(run, "refusals.json") ?? "{}")).reduce((a, b) => a + b, 0);
  const last = samples.at(-1) ?? {};
  const at = (rcl) => levels.find((l) => l.rcl >= rcl)?.t;
  // What the economy ends with, in energy, as the room stands at the last
  // sample (the sampler's `worth`): every economy shows up here; a
  // diagnostic belongs in the run's own samples, not in this table.
  const w = last.worth;
  const { exposure, lost, held, open } = exposureOf(dirOf(run), last);
  const k = (n) => (w ? `${Math.round(n / 1000)}k` : "-");
  return {
    ok: !failed(run),
    rcl: last.rcl,
    rcl3: at(3),
    rcl4: at(4),
    controller: k(w?.controller ?? 0),
    structures: k((w?.structures ?? 0) + (w?.sites ?? 0)),
    fortified: k(w?.fortification ?? 0),
    stock: k(w?.stock ?? 0),
    worth: k(w ? w.controller + w.structures + w.sites + w.fortification + w.stock : 0),
    score: k(last.score ? last.score.controller + last.score.structures + last.score.fortification + last.score.stock : 0),
    faults: faults + refusals,
    exposure,
    lostHeld: `${(lost / 1000).toFixed(1)}k/${(held / 1000).toFixed(1)}k`,
    open,
  };
};

const cell = (v, w = 9) => String(v ?? "-").padStart(w);
console.log(["room", "seed", "bundle", "ok", "rcl", "rcl3", "rcl4", "control", "structs", "fortify", "stock", "worth", "score", "faults", "exposure", "lost/held"].map((h, i) => cell(h, i === 0 || i === 2 ? 12 : i === 15 ? 14 : 9)).join(""));
for (const start of starts)
  for (const seed of start.seeds)
    for (const bot of bots) {
      const run = runs.find((r) => r.start === start && r.seed === seed && r.bot === bot);
      const r = rowOf(run);
      console.log(
        [start.label.split(":").at(-1), seed.label, bot.name, r.ok ? "yes" : "NO", r.rcl, r.rcl3, r.rcl4, r.controller, r.structures, r.fortified, r.stock, r.open ? `${r.worth}?` : r.worth, r.open ? `${r.score}?` : r.score, r.faults, r.exposure, r.lostHeld]
          .map((v, i) => cell(v, i === 0 || i === 2 ? 12 : i === 15 ? 14 : 9))
          .join(""),
      );
    }
const broken = runs.filter((r) => !failed(r)).flatMap((r) => invariants(dirOf(r)).map((b) => `${r.name}: ${b}`));
for (const b of broken) console.log(`invariant broken: ${b}`);
unrun.forEach((u) => console.log(u));
if (broken.length || Object.values(codes).some((c) => c !== 0)) process.exit(1);
