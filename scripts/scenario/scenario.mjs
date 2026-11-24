// The scenario library (ARCHITECTURE.md §10.2): each scenario in scenarios/
// is a start, a number of ticks and bounds on what the run measured, played
// on `sim`. Starts are recipes (room, seed, late, node), or a small crafted
// world checked in for what no recipe stands (a road through swamp);
// scenarios with one start and one length share one soak, since a measure
// reads its whole run. A world a soak left (a deploy's, Memory kept) is a
// crafted world too, its intel rows its segment 2.
//
// Usage: npm run scenario -- [name...] [--modules m,n] [--list] [--bot sim] [--jobs J] [--nightly] [--capture] [--base name] [--seeds N]
//        npm run scenario -- <name> --watch [--tick-ms N] [--port N]
//   name    the scenarios to run (default all but the nightly ones), by file name
//   --modules  also the scenarios naming any of these modules (directories under core)
//   --list  print the selection and run nothing
//   --nightly  all of them, with those marked "nightly", kept out of `jj ship`
//   --bot   a sim binary to play (default this tree's, `npm run build:sim`),
//           its runs named after its file
//   --base  a binary name whose runs stand in soak-runs (an earlier --bot):
//           each measure is printed beside the base's, marked better or
//           worse by its bound's side, and each soak's score
//   --seeds each start also on the N - 1 seeds after its own
//   --capture  every tick each soak decides, to its capture.jsonl, which
//           `npm run replay` decides again and `npm run bench` times
//   --jobs  soaks at once (default every hardware thread)
//   --watch the one scenario's soak served to lazycreep (`sim --watch`), in this terminal
//
// A scenario file: { "why": the invariant, "modules": the core modules it tests, "nightly"?, "start": { "gen"? (sim --gen's map, which picks the room) or "room", "seed",
// "late"?, "node"?, "invaders"? (room:every[:first],…), "wave"? (role[:size],…), "core"? (room:tick,…), "stronghold"? ("room:tick:level[:deploy],…"), "resetEvery"? (soak:many's --reset-every), "unsafe"?, "ended"?,
// "opponents"? (sim's scripted rivals, `sim/src/rival`), "players"? and "sectors"? (soak.mjs's: our bot in every seat; an assert's "seat" names the one it reads, unset the first) } or { "world": a capture-shaped file
// (crafted ones under worlds/), "seed", "room"?, "node"?, "late"? (a world with no home of ours, as a recipe's), "ended"?, "invaders"?, "wave"?, "opponents"? (the world's own users), "declarations"? (in place of the world's), "memory"? (keys of Memory.v2 beside the world's), "gcl"? (in place of the world's), "level"? ({ room: its controller level in place of the world's }) }, "ticks", "assert": [{ "measure", "from"?, "to"?, "max"?, "min"?, "room"?, "kind"?, "name"?, "act"? }] };
// a measure is one of `measures` below, read off the soak's output over ticks
// [from, to), counted from the run's first tick; { "metric": a name the bot's
// metrics carry (`core/observe/metrics.rs`), "room"?, "total"? } bounds its worst
// full window, or with "total" their sum, and fails when none was read;
// { "each": an occurrence `events` lists, "where"?: { field: a bound or a value }, "least"? (default 1),
// "assert": { field: { "min"?, "max"? } } } holds each bound on every occurrence matching "where".
// Exits non-zero when a bound fails or a soak does.
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { availableParallelism } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { breaches, check, engagementsOf, events, measures, occurrences, samplesOf, withAlways } from "./measures.mjs";
import { report } from "../sim/prediction.mjs";
import { sweep } from "./prune.mjs";
import { unstood } from "../sim/node.mjs";
import { SIM_MEASURES, SIM_START, buildPlan, runSim, simBinary, unseated } from "../sim/run.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
// A count ceiling holds for every scenario (`ARCHITECTURE.md` §9.3), so an assert
// naming one takes its bound from there rather than repeating the number.
const countCeilings = JSON.parse(readFileSync(join(REPO, "scripts/ceilings.json"), "utf8")).counts;
const LIBRARY = join(HERE, "scenarios");
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const names = [];
const flags = {};
const usage = "usage: npm run scenario -- [name...] [--modules m,n] [--list] [--bot sim] [--jobs J] [--nightly] [--capture] [--base name] [--seeds N] | <name> --watch [--tick-ms N] [--port N]";
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++)
  if (["--bot", "--jobs", "--modules", "--base", "--seeds", "--tick-ms", "--port"].includes(rest[i])) flags[rest[i]] = rest[++i] ?? fail(`${rest[i - 1]} takes a value`);
  else if (["--list", "--nightly", "--capture", "--watch"].includes(rest[i])) flags[rest[i]] = true;
  else if (rest[i].startsWith("--")) fail(usage);
  else names.push(rest[i]);
if (!flags["--watch"] && (flags["--tick-ms"] || flags["--port"])) fail(`--tick-ms and --port need --watch\n${usage}`);

const CORE = join(REPO, "core");
const modules = readdirSync(CORE).filter((m) => m !== "tick" && m !== "base" && statSync(join(CORE, m)).isDirectory());
const library = readdirSync(LIBRARY)
  .filter((f) => f.endsWith(".json"))
  .map((f) => ({ name: basename(f, ".json"), ...JSON.parse(readFileSync(join(LIBRARY, f), "utf8")) }));
for (const s of library) s.assert = withAlways(s.assert);
for (const s of library) for (const a of s.assert) if (a.each) Object.assign(a, { measure: "each", max: 0 });
for (const s of library)
  if (!s.modules?.length || s.modules.some((m) => !modules.includes(m))) fail(`${s.name}: "modules" must name some of ${modules.join(", ")}`);
for (const s of library.filter((s) => s.name.startsWith("defend-")))
  if (!s.assert.some((a) => (a.measure === "bodiesLost" && a.max !== undefined) || a.assert?.pastBudget?.max !== undefined)) fail(`${s.name}: a fight scenario bounds bodiesLost, or each engagement's pastBudget, by a "max"`);
// What sim cannot stand or read (`scripts/sim/run.mjs`), or null.
const unplayable = (s) =>
  Object.keys(s.start).filter((k) => !SIM_START.includes(k)).map((k) => `its start's ${k}`)[0] ||
  (s.start.node && unstood(s.start.node)) ||
  (s.start.opponents ?? []).map(unseated).find(Boolean) ||
  (s.start.world && Object.keys(JSON.parse(readFileSync(resolve(REPO, s.start.world), "utf8")).segments ?? {}).some((k) => k !== "2") && "a capture's segments beside intel's") ||
  (s.assert.some((a) => a.who) && "a measure of an opponent's side (`who`), which sim samples nowhere") ||
  s.assert.filter((a) => !SIM_MEASURES.has(a.metric !== undefined ? "metric" : a.measure)).map((a) => `${a.measure} is not read off sim`)[0] ||
  null;
for (const s of library) if (unplayable(s)) fail(`${s.name}: ${unplayable(s)}`);

// name@sK names a scenario on seed K alone, as --seeds names its runs.
const seeded = (s, seed) => (seed === (s.start.seed ?? 1) ? s : { ...s, name: `${s.name}@s${seed}`, start: { ...s.start, seed } });
const pinned = names.filter((n) => /@s\d+$/.test(n)).map((n) => [n.replace(/@s\d+$/, ""), Number(n.match(/@s(\d+)$/)[1])]);
names.splice(0, names.length, ...names.filter((n) => !/@s\d+$/.test(n)));
for (const n of [...names, ...pinned.map(([n]) => n)]) if (!library.some((s) => s.name === n)) fail(`no scenario ${n}; the library: ${library.map((s) => s.name).join(", ")}`);
const touched = flags["--modules"]?.split(",").filter(Boolean) ?? [];
for (const m of touched) if (!modules.includes(m)) fail(`no module ${m}; the modules: ${modules.join(", ")}`);
const meets = (s) => (touched.length ? s.modules.some((m) => touched.includes(m)) : !names.length);
// --seeds N plays each start on its seed and the N - 1 after it: one seed is one
// game, and a change of behaviour is read over several.
const seeds = Number(flags["--seeds"] ?? 1);
if (!Number.isInteger(seeds) || seeds < 1) fail("--seeds takes a count of seeds, 1 or more");
const scenarios = library
  .filter((s) => names.includes(s.name) || (!pinned.length && meets(s) && (flags["--nightly"] || !s.nightly)))
  .flatMap((s) => Array.from({ length: seeds }, (_, k) => seeded(s, (s.start.seed ?? 1) + k)))
  .concat(pinned.map(([n, seed]) => seeded(library.find((s) => s.name === n), seed)));
if (flags["--list"]) {
  for (const s of scenarios) console.log(`${s.name}  ${s.modules.join(",")}`);
  console.log(`${scenarios.length} of ${library.length} scenarios`);
  process.exit(0);
}
if (flags["--watch"] && names.length !== 1) fail("--watch runs exactly one scenario: npm run scenario -- <name> --watch");
// The Ctrl-C that ends a watched soak reaches this process too; its asserts still print.
if (flags["--watch"]) process.on("SIGINT", () => {});
const jobs = Number(flags["--jobs"] ?? availableParallelism());

sweep(join(REPO, "soak-runs"));

for (const s of scenarios) for (const a of s.assert) if (a.metric !== undefined) a.measure = "metric";
for (const s of scenarios) for (const a of s.assert) if (!measures[a.measure]) fail(`${s.name}: no measure ${a.measure}`);
for (const s of scenarios) for (const a of s.assert) if (a.each && !events[a.each]) fail(`${s.name}: no occurrence ${a.each}; the occurrences: ${Object.keys(events).join(", ")}`);
for (const { name, start } of scenarios)
  if (start.world && Object.keys(start).some((k) => !["world", "seed", "room", "node", "late", "invaders", "wave", "opponents", "declarations", "follow", "ended", "memory", "gcl", "stock", "level"].includes(k))) fail(`${name}: a "world" start takes a "seed", "room", "node", "late", "invaders", "wave", "opponents", "declarations", "follow", "ended", "memory", "gcl", "stock", "level" and nothing else`);
for (const { name, start } of scenarios)
  if (!start.world && (start.declarations || start.memory || start.gcl)) fail(`${name}: "declarations", "memory" and "gcl" take a "world" start`);

const tag = (opponents = []) => (opponents.length ? `-o${createHash("sha256").update(JSON.stringify(opponents)).digest("hex").slice(0, 8)}` : "");
const recipe = ({ gen, sectors, players, world, room, seed, late = 0, node, invaders, wave, core, stronghold, unsafe, ended, declarations, memory, stock, gcl, level, follow, resetEvery }, opponents) =>
  `${[gen !== undefined && `gen${gen}`, sectors && `x${sectors}`, world && basename(world, ".json"), room].filter(Boolean).join("-")}${declarations ? `-d${createHash("sha256").update(JSON.stringify(declarations)).digest("hex").slice(0, 8)}` : ""}${memory ? `-m${createHash("sha256").update(JSON.stringify(memory)).digest("hex").slice(0, 8)}` : ""}${stock ? `-k${createHash("sha256").update(JSON.stringify(stock)).digest("hex").slice(0, 8)}` : ""}${gcl ? `-g${gcl}` : ""}${level ? `-r${Object.entries(level).flat().join("_")}` : ""}-s${seed}${late ? `l${late}` : ""}${node ? `-${node}` : ""}${invaders ? `-i${invaders.replace(/[:,]/g, "_")}` : ""}${wave ? `-w${wave.replace(/[:,]/g, "_")}` : ""}${core ? `-c${core.replace(/[:,]/g, "_")}` : ""}${stronghold ? `-h${stronghold.replace(/[:,]/g, "_")}` : ""}${unsafe ? "-u" : ""}${ended ? "-e" : ""}${follow ? `-f${follow.join("_")}` : ""}${resetEvery ? `-x${resetEvery}` : ""}${players ? `-p${createHash("sha256").update(JSON.stringify(players)).digest("hex").slice(0, 8)}` : ""}${tag(opponents)}`;
const sim = flags["--bot"] ? resolve(flags["--bot"]) : simBinary() ?? fail("sim did not build");
if (!existsSync(sim)) fail(`--bot: no sim at ${sim}`);
const simName = flags["--bot"] ? basename(sim) : `sim-${createHash("sha256").update(readFileSync(sim)).digest("hex").slice(0, 8)}`;
const keyOf = ({ start, ticks }, name = simName) => `scenario-${name}-${recipe(start, start.opponents)}-${ticks}`;
const runs = new Map();
for (const s of scenarios) {
  const run = runs.get(keyOf(s)) ?? { name: keyOf(s), label: `${recipe(s.start, s.start.opponents)}-${s.ticks}`, start: s.start, ticks: s.ticks, scenarios: [], secs: 0 };
  run.scenarios.push(s.name);
  runs.set(run.name, run);
}

// Built once here: parallel builds of one project collide.
if (runs.size && !buildPlan()) fail("the plan tool did not build");
const started = Date.now();
const play = async ({ name, start, ticks }) => {
  if (start.world && (start.declarations || start.memory || start.gcl || start.stock || start.level)) {
    const world = join(REPO, "soak-runs", `world-${name}.json`);
    mkdirSync(dirname(world), { recursive: true });
    const crafted = JSON.parse(readFileSync(resolve(REPO, start.world), "utf8"));
    const v2 = { ...crafted.memory?.v2, ...start.memory };
    // `stock`: amounts set in each named room's terminal.
    for (const [room, add] of Object.entries(start.stock ?? {})) {
      const terminal = crafted.rooms.find((r) => r.name === room)?.objects.find((o) => o.type === "terminal");
      if (!terminal) fail(`${name}: "stock" names ${room}, which holds no terminal in ${start.world}`);
      Object.assign(terminal.store, add);
    }
    for (const [room, level] of Object.entries(start.level ?? {})) {
      const controller = crafted.rooms.find((r) => r.name === room)?.objects.find((o) => o.type === "controller");
      if (!controller) fail(`${name}: "level" names ${room}, which holds no controller in ${start.world}`);
      Object.assign(controller, { level, progress: 0 });
    }
    writeFileSync(world, JSON.stringify({ ...crafted, gcl: start.gcl ?? crafted.gcl, memory: { ...crafted.memory, v2 }, declarations: start.declarations ?? crafted.declarations }));
    start = { ...start, world };
  }
  const { gen, sectors, players, world, room, node, seed, late, ended, unsafe, follow, opponents, invaders, wave, core, stronghold, resetEvery } = start;
  const watch = flags["--watch"] ? { port: Number(flags["--port"] ?? 21025), ms: Number(flags["--tick-ms"] ?? 50) } : undefined;
  const capture = flags["--capture"] ? `0-${ticks}` : undefined;
  return runSim(sim, { capture, gen, sectors, players: players && JSON.stringify(players), world: world && resolve(REPO, world), room, node, ticks, seed, late, ended, unsafe, follow, rivals: opponents, invaders, wave, core, stronghold, resetEvery, watch, out: join(REPO, "soak-runs", name) });
};
// A long soak started last is the whole run's tail.
const queue = [...runs.values()].sort((a, b) => b.ticks - a.ticks);
console.error(`${scenarios.length} scenarios in ${runs.size} soaks, ${jobs} at a time`);
await Promise.all(
  Array.from({ length: jobs }, async () => {
    for (let run; (run = queue.shift()); ) {
      const began = Date.now();
      run.code = await play(run);
      run.secs = (Date.now() - began) / 1000;
      console.error(`soak ${run.label} ${run.secs.toFixed(0)} s (scenarios: ${run.scenarios.join(", ")})`);
    }
  }),
);

let failed = 0;
const moved = { better: 0, worse: 0 };
for (const s of scenarios) {
  const run = runs.get(keyOf(s));
  const dir = join(REPO, "soak-runs", run.name);
  if (run.code !== 0) {
    failed++;
    console.log(`FAIL ${s.name}: the soak failed (exit ${run.code}); see ${dir}`);
    continue;
  }
  const read = new Map();
  const seat = (dir, a) => {
    const at = a.seat ? join(dir, a.seat) : dir;
    if (!read.has(at)) read.set(at, samplesOf(at));
    return [read.get(at), at];
  };
  const baseDir = flags["--base"] && join(REPO, "soak-runs", keyOf(s, flags["--base"]));
  const base = baseDir && existsSync(join(baseDir, "samples.jsonl"));
  for (const a of s.assert) {
    const [samples, at] = seat(dir, a);
    const { value, ok, bound } = check(samples, a, at, a.max ?? countCeilings[a.measure]);
    if (!ok) failed++;
    let against = "";
    if (base) {
      const [baseSamples, baseAt] = seat(baseDir, a);
      const was = measures[a.measure](baseSamples, a, baseAt);
      // A bound below says more is better; a bound above, less.
      const better = a.min !== undefined ? value > was : value < was;
      const mark = value === was ? "=" : better ? "better" : "WORSE";
      if (value !== was) moved[better ? "better" : "worse"]++;
      against = `; base ${was} ${mark}`;
    }
    const window = a.from !== undefined || a.to !== undefined ? ` t${a.from ?? 0}-${a.to ?? s.ticks}` : "";
    const named = a.each ? `each ${a.each}${a.where ? ` ${JSON.stringify(a.where)}` : ""} ${JSON.stringify(a.assert)} (${occurrences(samples, a, at).length} seen), breaches` : a.metric ?? a.measure;
    console.log(`${ok ? "pass" : "FAIL"} ${s.name}: ${named}${a.seat ? ` of ${a.seat}` : ""}${a.room ? ` in ${a.room}` : ""}${window} ${value} (${bound}${against})`);
    if (a.each) for (const line of breaches(samples, a, at).slice(0, 5)) console.log(`  ${line}`);
  }
  const rows = s.assert.some((a) => a.measure === "predictedWinsLost") ? engagementsOf(dir) : [];
  if (rows.length) for (const line of [`${rows.length} engagements`, ...report(rows)]) console.log(`  ${s.name}: ${line}`);
}
if (flags["--base"]) {
  // A soak's score at its last tick (the season's: controller, structures,
  // fortification, stock), against the base's on the same start.
  const scoreOf = (dir) => {
    const last = existsSync(join(dir, "samples.jsonl")) ? samplesOf(dir).at(-1) : undefined;
    return last?.score ? Object.values(last.score).reduce((a, b) => a + b, 0) : undefined;
  };
  const rows = [...runs.values()]
    .map((r) => ({ label: r.label, now: scoreOf(join(REPO, "soak-runs", r.name)), was: scoreOf(join(REPO, "soak-runs", keyOf(r, flags["--base"]))) }))
    .filter((r) => r.now !== undefined && r.was !== undefined && r.was > 0)
    .map((r) => ({ ...r, change: r.now / r.was - 1 }));
  for (const r of rows.filter((r) => Math.abs(r.change) >= 0.005).sort((a, b) => a.change - b.change))
    console.log(`score ${r.label}: ${r.now.toFixed(0)} against ${r.was.toFixed(0)} (${r.change >= 0 ? "+" : ""}${(100 * r.change).toFixed(1)}%)`);
  const mean = rows.reduce((a, r) => a + r.change, 0) / Math.max(1, rows.length);
  console.log(`against ${flags["--base"]}: ${moved.better} measures better, ${moved.worse} worse; score over ${rows.length} soaks ${mean >= 0 ? "+" : ""}${(100 * mean).toFixed(2)}% on mean`);
}
const slowest = [...runs.values()].sort((a, b) => b.secs - a.secs).slice(0, 5);
console.log(`slowest: ${slowest.map((r) => `${r.label} ${r.secs.toFixed(0)} s (${r.scenarios.join(", ")})`).join("; ")}`);
// Where the soaks' time went (each soak's timing.json, ms a tick), summed over ticks.
const timed = [...runs.values()]
  .map((r) => join(REPO, "soak-runs", r.name, "timing.json"))
  .filter(existsSync)
  .map((f) => JSON.parse(readFileSync(f, "utf8")));
if (timed.length) {
  const ticks = timed.reduce((a, t) => a + t.ticks, 0);
  const per = (k) => (timed.reduce((a, t) => a + t[k] * t.ticks, 0) / ticks).toFixed(2);
  console.log(`timing over ${timed.length} soaks, ${ticks} ticks, ms a tick: ${["engine", "adapter", "decide", "sampler"].map((k) => `${k} ${per(k)}`).join(", ")}`);
}
console.log(`${failed ? `${failed} failed` : "all passed"} in ${((Date.now() - started) / 1000).toFixed(0)} s`);
process.exit(failed ? 1 : 0);
