// The scenario library (ARCHITECTURE.md §10.2): each scenario in scenarios/
// is a start, a number of ticks and bounds on what the run measured. Starts
// are recipes (room, seed, late, node), or a small crafted world checked in
// for what no recipe stands (a road through swamp); scenarios
// with one start and one length share one soak, since a measure reads its
// whole run. A start
// `at` tick T resumes the world a soak of `bundle` left at T: that bundle is
// pinned to a commit so the world is played once and cached under
// soak-runs/worlds/, and the bundle under test runs on from it as a deploy
// would, Memory kept.
//
// Usage: npm run scenario -- [name...] [--modules m,n] [--list] [--bot main.js] [--jobs J] [--nightly] [--fetch] [--capture] [--base name] [--seeds N]
//        npm run scenario -- <name> --watch [--tick-ms N] [--port N]
//   name    the scenarios to run (default all but the nightly ones), by file name
//   --modules  also the scenarios naming any of these modules (directories under core)
//   --list  print the selection, each with its engine, and run nothing
//   --nightly  all of them, with those marked "nightly": the offence target,
//           kept out of `jj ship`
//   --fetch  fetch the tarball a rival's `fetch.json` names; without it a
//           scenario whose rival is not cached is SKIPped, never failed
//   --bot   the bundle (default dist/main.js)
//   --base  a bundle name whose runs stand in soak-runs (an earlier --bot
//           name.js): each measure is printed beside the base's, marked
//           better or worse by its bound's side, and each soak's score
//   --seeds each start also on the N - 1 seeds after its own
//   --capture  every tick each soak decides, to its capture.jsonl, which
//           `npm run replay` decides again with another bundle
//   --jobs  soaks at once (default a quarter of the hardware threads, as soak:many)
//   --watch the one scenario's soak watched (soak's --watch), in this terminal
//
// A scenario file: { "why": the invariant, "modules": the core modules it tests, "nightly"?, "xxscreeps"?: why it stays off sim, "start": { "room", "seed",
// "late"?, "node"?, "invaders"? (soak's --invaders), "wave"? (soak's --wave), "core"? (soak's --core), "unsafe"?, "ended"?, "at"?, "bundle"?,
// "opponents"? (soak's --opponents) } or { "world": a capture-shaped file
// (soak's --world; crafted ones under worlds/), "seed", "room"?, "node"?, "late"? (a world with no home of ours, as a recipe's), "ended"?, "invaders"?, "wave"?, "opponents"? (the world's own users), "declarations"? (in place of the world's), "memory"? (keys of Memory.v2 beside the world's), "gcl"? (in place of the world's), "withdrawn"? (rooms whose open siege the owner has just withdrawn), "off"? (modules switched off from the first tick, Memory.v2.switches; "economy" off, our spawns and extensions stand full) }, "ticks", "assert": [{ "measure", "from"?, "to"?, "max"?, "min"?, "who"?, "room"?, "kind"?, "name"?, "act"? }] };
// a measure is one of `measures` below, read off the soak's output over ticks
// [from, to), counted from the run's first tick; { "metric": a name the bot's
// metrics carry (`core/observe/metrics.rs`), "room"? } bounds its worst
// full window, and fails when none was read.
// Exits non-zero when a bound fails or a soak does, or loses ticks twice.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from "node:fs";
import { availableParallelism } from "node:os";
import { node26Env } from "./node26.mjs";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { check, engagementsOf, measures, samplesOf, withAlways } from "./measures.mjs";
import { report } from "./prediction.mjs";
import { sweep } from "./prune.mjs";
import { unstood } from "../sim/node.mjs";
import { SIM_MEASURES, SIM_START, buildPlan, exportTemplate, runSim, simBinary } from "../sim/run.mjs";

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
// Soak's watch flags, passed on as given; soak.mjs checks them.
const watch = [];
const WATCH_VALUES = ["--tick-ms", "--port"];
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++)
  if (["--bot", "--jobs", "--modules", "--base", "--seeds"].includes(rest[i])) flags[rest[i]] = rest[++i] ?? fail(`${rest[i - 1]} takes a value`);
  else if (rest[i] === "--list" || rest[i] === "--nightly" || rest[i] === "--fetch" || rest[i] === "--capture") flags[rest[i]] = true;
  else if (rest[i] === "--watch") watch.push(rest[i]);
  else if (WATCH_VALUES.includes(rest[i])) watch.push(rest[i], rest[++i]);
  else if (rest[i].startsWith("--")) fail("usage: npm run scenario -- [name...] [--modules m,n] [--list] [--bot main.js] [--jobs J] [--nightly] [--fetch] | <name> --watch [--tick-ms N] [--port N]");
  else names.push(rest[i]);

const CORE = join(REPO, "core");
const modules = readdirSync(CORE).filter((m) => m !== "tests" && statSync(join(CORE, m)).isDirectory());
const library = readdirSync(LIBRARY)
  .filter((f) => f.endsWith(".json"))
  .map((f) => ({ name: basename(f, ".json"), ...JSON.parse(readFileSync(join(LIBRARY, f), "utf8")) }));
for (const s of library) s.assert = withAlways(s.assert);
for (const s of library)
  if (!s.modules?.length || s.modules.some((m) => !modules.includes(m))) fail(`${s.name}: "modules" must name some of ${modules.join(", ")}`);
// A scripted rival shares our blind spots: each fight kind meets an open-source bot at least once.
const OPEN_SOURCE = ["scripts/xxscreeps/engine/hivemind", "scripts/xxscreeps/engine/tooangel"];
const FIGHT_KINDS = ["siege", "sweep", "defend"];
for (const kind of FIGHT_KINDS)
  if (!library.some((s) => s.name.startsWith(`${kind}-`) && s.start.opponents?.some((o) => OPEN_SOURCE.includes(o.bot)))) fail(`no ${kind}-* scenario seats an open-source bot (${OPEN_SOURCE.join(", ")})`);
// A siege opens only on a declared room, so a crafted world holding one open
// on another is a state no run reaches, unless the owner withdrew it (`withdrawn`).
for (const { name, start } of library.filter((s) => s.start.world)) {
  const world = JSON.parse(readFileSync(resolve(REPO, start.world), "utf8"));
  const declared = [...(start.declarations ?? world.declarations ?? {}).sieges ?? [], ...(start.withdrawn ?? [])];
  for (const e of world.memory?.v2?.fight?.engagements ?? [])
    if (e.kind === "siege" && !declared.includes(e.room)) fail(`${name}: ${start.world} holds a siege open on ${e.room}, which its declarations do not name`);
}
for (const s of library.filter((s) => FIGHT_KINDS.some((k) => s.name.startsWith(`${k}-`))))
  if (!s.assert.some((a) => a.measure === "bodiesLost" && a.max !== undefined)) fail(`${s.name}: a fight scenario bounds bodiesLost by a "max"`);
// A scenario runs on `sim` (ARCHITECTURE.md §10.2) unless its start or a
// measure needs what only xxscreeps stands or writes, or its "xxscreeps" says
// why it stays there; a bundle, base, capture or watch runs every one there.
const seatsUs = (file) => {
  const world = JSON.parse(readFileSync(resolve(REPO, file), "utf8"));
  return (world.rooms ?? []).some((r) => r.objects.some((o) => o.type === "spawn" && o.user === world.userId));
};
const xxscreepsWhy = (s) =>
  s.xxscreeps ??
  (Object.keys(s.start).filter((k) => !SIM_START.includes(k)).map((k) => `its start's ${k}`)[0] ||
    (s.start.world && seatsUs(s.start.world) && "a world with a home of ours") ||
    (s.start.node && unstood(s.start.node)) ||
    (s.assert.some((a) => a.who) && "a measure of an opponent's side") ||
    s.assert.filter((a) => !SIM_MEASURES.has(a.metric !== undefined ? "metric" : a.measure)).map((a) => `${a.measure} is not read off sim`)[0] ||
    null);
const forced = ["--bot", "--base", "--capture"].find((f) => flags[f]) ?? (watch.length ? "--watch" : undefined);
for (const s of library) s.engine = forced || xxscreepsWhy(s) ? "xxscreeps" : "sim";

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
  for (const s of scenarios) console.log(`${s.name}  ${s.modules.join(",")}  ${s.engine === "sim" ? "sim" : `xxscreeps: ${xxscreepsWhy(s) ?? forced}`}`);
  console.log(`${scenarios.length} of ${library.length} scenarios`);
  process.exit(0);
}
if (watch.length && names.length !== 1) fail("--watch runs exactly one scenario: npm run scenario -- <name> --watch");
// The Ctrl-C that ends a watched soak reaches this process too; its asserts still print.
if (watch.length) process.on("SIGINT", () => {});
const bot = resolve(flags["--bot"] ?? join(REPO, "dist/main.js"));
if (!existsSync(bot)) fail(`no bundle at ${bot}: npm run build first`);
const jobs = Number(flags["--jobs"] ?? Math.max(1, Math.floor(availableParallelism() / 4)));

sweep(join(REPO, "soak-runs"));

for (const s of scenarios) for (const a of s.assert) if (a.metric !== undefined) a.measure = "metric";
for (const s of scenarios) for (const a of s.assert) if (!measures[a.measure]) fail(`${s.name}: no measure ${a.measure}`);
for (const { name, start } of scenarios)
  if ((start.at === undefined) !== (start.bundle === undefined) || (start.at !== undefined && (start.node || !/^[0-9a-f]{8,40}$/.test(start.bundle))))
    fail(`${name}: a start "at" a tick names its "bundle" by commit id, and no node`);
for (const { name, start } of scenarios)
  if (start.world && Object.keys(start).some((k) => !["world", "seed", "room", "node", "late", "invaders", "wave", "opponents", "declarations", "withdrawn", "off", "follow", "ended", "memory", "gcl"].includes(k))) fail(`${name}: a "world" start takes a "seed", "room", "node", "late", "invaders", "wave", "opponents", "declarations", "withdrawn", "off", "follow", "ended", "memory", "gcl" and nothing else`);
  else if (!start.world && start.off) fail(`${name}: only a "world" start switches modules "off"`);

// A world holds its opponents' seats; their config is the scenario's.
const tag = (opponents = []) => (opponents.length ? `-o${createHash("sha256").update(JSON.stringify(opponents)).digest("hex").slice(0, 8)}` : "");
const seats = (opponents = []) => opponents.map(({ config, ...seat }) => seat);
const recipe = ({ world, room, seed, late = 0, node, invaders, wave, core, unsafe, ended, declarations, memory, gcl, off, follow }, opponents) =>
  `${[world && basename(world, ".json"), room].filter(Boolean).join("-")}${declarations ? `-d${createHash("sha256").update(JSON.stringify(declarations)).digest("hex").slice(0, 8)}` : ""}${memory ? `-m${createHash("sha256").update(JSON.stringify(memory)).digest("hex").slice(0, 8)}` : ""}${gcl ? `-g${gcl}` : ""}-s${seed}${late ? `l${late}` : ""}${node ? `-${node}` : ""}${invaders ? `-i${invaders.replace(/[:,]/g, "_")}` : ""}${wave ? `-w${wave.replace(/[:,]/g, "_")}` : ""}${core ? `-c${core.replace(/[:,]/g, "_")}` : ""}${unsafe ? "-u" : ""}${ended ? "-e" : ""}${off ? `-x${off.join("_")}` : ""}${follow ? `-f${follow.join("_")}` : ""}${tag(opponents)}`;
// A cached world is replayed only by the opponents' code that played it, and
// the engine mods and world scripts it was stood and played under.
const codeOf = (...paths) =>
  paths
    .map((p) => resolve(REPO, p))
    .flatMap((path) => (statSync(path).isDirectory() ? readdirSync(path).sort().map((f) => join(path, f)) : [path, path.replace(/\.js$/, ".wasm")].filter(existsSync)))
    .reduce((h, f) => h.update(f).update(readFileSync(f)), createHash("sha256"))
    .digest("hex")
    .slice(0, 8);
const harness = codeOf(...["seed", "seat.js", "waves", "core", "colony.mjs", "node.mjs", "live.mjs"].map((f) => join(HERE, "engine", f)));
const worldOf = (start) => `${recipe(start, seats(start.opponents).map((s) => ({ ...s, code: codeOf(s.bot === "@bot" ? bot : s.bot) })))}-t${start.at}-${start.bundle}-h${harness}`;
const worldDir = (start) => join(REPO, "soak-runs/worlds", worldOf(start));
const keyOf = ({ start, ticks, engine }, name = engine === "sim" ? `sim-${simCode}` : basename(bot, ".js")) => `scenario-${name}-${start.at ? `${worldOf(start)}${tag(start.opponents)}` : recipe(start, start.opponents)}-${ticks}`;
const sim = scenarios.some((s) => s.engine === "sim") ? (simBinary() ?? fail("sim did not build")) : null;
const simCode = sim && createHash("sha256").update(readFileSync(sim)).digest("hex").slice(0, 8);
if (forced && scenarios.some((s) => !xxscreepsWhy(s))) console.error(`${forced}: every scenario on xxscreeps, which runs bundles, bases, captures and the client`);
const runs = new Map();
for (const s of scenarios) {
  const run = runs.get(keyOf(s)) ?? { name: keyOf(s), label: `${recipe(s.start, s.start.opponents)}${s.start.at ? `-t${s.start.at}` : ""}-${s.ticks}${s.engine === "sim" ? " (sim)" : ""}`, start: s.start, ticks: s.ticks, engine: s.engine, scenarios: [], secs: 0 };
  run.scenarios.push(s.name);
  runs.set(run.name, run);
}

// Built once here: parallel builds of one project collide.
if (runs.size && !buildPlan()) fail("the plan tool did not build");
// Built in `.scratch/`, on disk (`/tmp` is RAM), with a `jj` that fails: the
// real one would name the build after the workspace above it, not "unknown".
const bundleAt = (commit) => {
  const out = join(REPO, "soak-runs/bundles", `${commit}.js`);
  if (existsSync(out)) return out;
  console.error(`building ${commit}`);
  mkdirSync(join(REPO, ".scratch"), { recursive: true });
  const src = mkdtempSync(join(REPO, ".scratch", "bundle-"));
  mkdirSync(join(src, ".bin"));
  writeFileSync(join(src, ".bin", "jj"), "#!/bin/sh\nexit 1\n", { mode: 0o755 });
  const env = { ...process.env, PATH: `${join(src, ".bin")}:${process.env.PATH}` };
  // A failed step answers its command, and the tree is removed before `fail` exits.
  const sh = (cmd) => (spawnSync("sh", ["-c", cmd], { cwd: src, env, stdio: ["ignore", "ignore", "inherit"] }).status === 0 ? null : cmd);
  const broke =
    sh(`git -C '${REPO}' archive ${commit} | tar -x && ln -s '${join(REPO, "node_modules")}' node_modules`) ??
    // A commit from before the port builds the F# bundle, with its own tools.
    sh(existsSync(join(src, "src/App")) ? "dotnet tool restore && npm run build --silent" : "npm run build --silent") ??
    sh(`mkdir -p '${dirname(out)}' && { [ ! -e dist/main.wasm ] || cp dist/main.wasm '${out.replace(/\.js$/, ".wasm")}'; } && cp dist/main.js '${out}'`);
  rmSync(src, { recursive: true, force: true });
  if (broke) fail(`${commit}: ${broke} failed`);
  return out;
};
for (const { start } of runs.values()) if (start.bundle) bundleAt(start.bundle);
if (scenarios.some((s) => s.engine === "sim" && !s.start.world) && !exportTemplate()) fail("the template did not export");
const started = Date.now();
// A soak that saves a world for a later start is a prelude, never the one watched.
const soak = (args, watched = watch.length > 0 && !args.includes("--save")) =>
  new Promise((done) => {
    const capture = flags["--capture"] && !args.includes("--save") ? ["--capture", `0-${args[args.indexOf("--ticks") + 1]}`] : [];
    const child = spawn("node", [join(HERE, "soak.mjs"), ...args, ...capture, ...(flags["--fetch"] ? ["--fetch"] : []), ...(watched ? watch : [])], { stdio: watched ? "inherit" : "ignore", env: node26Env({ ...process.env, FABOT_PLAN_BUILT: "1" }) });
    child.on("exit", (code) => done(code));
  });
// `bot`: a path from the repo, or "@bot", the bundle under test.
const opponentsOf = (name, opponents) => {
  if (!opponents) return [];
  const file = join(REPO, "soak-runs", `opponents-${name}.json`);
  mkdirSync(dirname(file), { recursive: true });
  writeFileSync(file, JSON.stringify(opponents.map((o) => ({ ...o, bot: o.bot === "@bot" ? bot : resolve(REPO, o.bot) }))));
  return ["--opponents", file];
};
const preludes = [];
// Two preludes of one world would save into one directory.
const preluding = new Map();
const play = async ({ name, start, ticks, engine }) => {
  if (engine === "sim") {
    const { world, room, node, seed, late, ended } = start;
    return runSim(sim, { world: world && resolve(REPO, world), room, node, ticks, seed, late, ended, out: join(REPO, "soak-runs", name) });
  }
  if (start.world && (start.declarations || start.off || start.memory || start.gcl)) {
    const world = join(REPO, "soak-runs", `world-${name}.json`);
    mkdirSync(dirname(world), { recursive: true });
    const crafted = JSON.parse(readFileSync(resolve(REPO, start.world), "utf8"));
    const v2 = { ...crafted.memory?.v2, ...start.memory, switches: { ...crafted.memory?.v2?.switches, ...Object.fromEntries((start.off ?? []).map((m) => [m, false])) } };
    writeFileSync(world, JSON.stringify({ ...crafted, gcl: start.gcl ?? crafted.gcl, memory: { ...crafted.memory, v2 }, declarations: start.declarations ?? crafted.declarations }));
    start = { ...start, world };
  }
  if (start.world)
    return soak(["--world", resolve(REPO, start.world), "--seed", String(start.seed), "--ticks", String(ticks), "--run", name, "--bot", bot, ...(start.room ? ["--room", start.room] : []), ...(start.node ? ["--node", start.node] : []), ...(start.late ? ["--late", String(start.late)] : []), ...(start.invaders ? ["--invaders", start.invaders] : []), ...(start.wave ? ["--wave", start.wave] : []), ...(start.follow ? ["--follow", start.follow.join(",")] : []), ...(start.ended ? ["--ended"] : []), ...opponentsOf(name, start.opponents)]);
  const recipe = ["--room", start.room, "--seed", String(start.seed), "--late", String(start.late ?? 0)];
  if (!start.at)
    return soak([
      ...recipe,
      "--ticks",
      String(ticks),
      "--run",
      name,
      "--bot",
      bot,
      ...(start.node ? ["--node", start.node] : []),
      ...(start.invaders ? ["--invaders", start.invaders] : []), ...(start.wave ? ["--wave", start.wave] : []),
      ...(start.core ? ["--core", start.core] : []),
      ...(start.unsafe ? ["--unsafe"] : []),
      ...(start.ended ? ["--ended"] : []),
      ...opponentsOf(name, start.opponents),
    ]);
  const world = worldDir(start);
  if (!existsSync(world) && !preluding.has(world))
    preluding.set(world, (async () => {
      const began = Date.now();
      if (!preludes.some((p) => p.dir === `world-${worldOf(start)}`)) preludes.push({ label: `world ${worldOf(start)}`, dir: `world-${worldOf(start)}` });
      const code = await soak([...recipe, "--ticks", String(start.at), "--run", `world-${worldOf(start)}`, "--bot", bundleAt(start.bundle), "--save", world, ...opponentsOf(`world-${name}`, start.opponents && seats(start.opponents))]);
      console.error(`world ${worldOf(start)} played in ${((Date.now() - began) / 1000).toFixed(0)} s, counted in the soaks that waited on it`);
      if (code !== 0) rmSync(world, { recursive: true, force: true });
      return code;
    })());
  const code = await preluding.get(world);
  if (code !== undefined && code !== 0) return code;
  return soak(["--room", start.room, "--seed", String(start.seed), "--ticks", String(ticks), "--run", name, "--bot", bot, "--from", world, ...opponentsOf(name, start.opponents)]);
};
// A long soak started last is the whole run's tail; ticks, with a world still to play, stand in for its cost.
const cost = ({ start, ticks }) => ticks + (start.at && !existsSync(worldDir(start)) ? start.at : 0);
const playAll = (list, jobs) => {
  const queue = [...list].sort((a, b) => cost(b) - cost(a));
  return Promise.all(
    Array.from({ length: jobs }, async () => {
      for (let run; (run = queue.shift()); ) {
        const began = Date.now();
        run.code = await play(run);
        const secs = (Date.now() - began) / 1000;
        run.secs += secs;
        console.error(`soak ${run.label} ${secs.toFixed(0)} s (scenarios: ${run.scenarios.join(", ")})`);
      }
    }),
  );
};
console.error(`${scenarios.length} scenarios in ${runs.size} soaks, ${jobs} at a time`);
await playAll(runs.values(), jobs);
// A soak that lost ticks (soak.mjs's exit 3) measured nothing: once more, fewer at a time.
const LOST = 3;
const invalid = [...runs.values()].filter((r) => r.code === LOST);
if (invalid.length) {
  console.error(`${invalid.length} soaks lost ticks; again, ${Math.max(1, jobs >> 1)} at a time`);
  preluding.clear();
  await playAll(invalid, Math.max(1, jobs >> 1));
}

let failed = 0;
const moved = { better: 0, worse: 0 };
// A soak that skipped (soak.mjs's exit 4) measured nothing: a rival's tarball is
// not in the cache, and the run may not reach the registry.
const SKIPPED = 4;
let skipped = 0;
for (const s of scenarios) {
  const run = runs.get(keyOf(s));
  const dir = join(REPO, "soak-runs", run.name);
  if (run.code === SKIPPED) {
    skipped++;
    console.log(`SKIP ${s.name}: its rival's tarball is not cached (soak exit ${SKIPPED}); \`npm run scenario -- --fetch\` fetches it`);
    continue;
  }
  if (run.code === LOST) {
    failed++;
    console.log(`INVALID ${s.name}: ticks lost on both tries (a script timeout or abandoned intents); see ${dir}/lost.json`);
    continue;
  }
  if (run.code !== 0) {
    failed++;
    console.log(`FAIL ${s.name}: the soak failed (exit ${run.code}); see ${dir}/server.log`);
    continue;
  }
  const samples = samplesOf(dir);
  const baseDir = flags["--base"] && join(REPO, "soak-runs", keyOf(s, flags["--base"]));
  const base = baseDir && existsSync(join(baseDir, "samples.jsonl")) ? samplesOf(baseDir) : null;
  for (const a of s.assert) {
    const { value, ok, bound } = check(samples, a, dir, a.max ?? countCeilings[a.measure]);
    if (!ok) failed++;
    let against = "";
    if (base) {
      const was = measures[a.measure](base, a, baseDir);
      // A bound below says more is better; a bound above, less.
      const better = a.min !== undefined ? value > was : value < was;
      const mark = value === was ? "=" : better ? "better" : "WORSE";
      if (value !== was) moved[better ? "better" : "worse"]++;
      against = `; base ${was} ${mark}`;
    }
    const window = a.from !== undefined || a.to !== undefined ? ` t${a.from ?? 0}-${a.to ?? s.ticks}` : "";
    console.log(`${ok ? "pass" : "FAIL"} ${s.name}: ${a.metric ?? a.measure}${a.who ? ` of ${a.who}` : ""}${a.room ? ` in ${a.room}` : ""}${window} ${value} (${bound}${against})`);
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
// Where the soaks' wall time went (each soak's timing.json), summed and the slowest ten.
const timed = [...[...runs.values()].map((r) => ({ label: r.label, dir: r.name })), ...preludes]
  .map((r) => ({ ...r, file: join(REPO, "soak-runs", r.dir, "timing.json") }))
  .filter((r) => existsSync(r.file))
  .map((r) => ({ label: r.label, ...JSON.parse(readFileSync(r.file, "utf8")) }));
if (timed.length) {
  const PHASES = ["wall", "setup", "node26", "boot", "stepperBoot", "step", "botCpu", "shutdown"];
  const sum = Object.fromEntries(["ticks", "node26Calls", ...PHASES].map((k) => [k, timed.reduce((a, t) => a + (t[k] ?? 0), 0)]));
  const s = (ms) => `${(ms / 1000).toFixed(0)} s`;
  console.log(`timing over ${timed.length} soaks, ${sum.ticks} ticks: ${PHASES.map((k) => `${k} ${s(sum[k])}`).join(", ")}; ${sum.node26Calls} node26 calls; step ${(sum.step / sum.ticks).toFixed(1)} ms/tick of which bot ${(sum.botCpu / sum.ticks).toFixed(1)}`);
  for (const t of timed.sort((a, b) => b.wall - a.wall).slice(0, 10))
    console.log(`  ${t.label} ${t.ticks} ticks: ${PHASES.map((k) => `${k} ${(t[k] / 1000).toFixed(1)}`).join(" ")} (${(t.step / t.ticks).toFixed(1)} ms/tick)`);
  writeFileSync(join(REPO, "soak-runs", "scenario-timing.json"), JSON.stringify(timed, null, 1) + "\n");
}
console.log(`${failed ? `${failed} failed` : "all passed"}${skipped ? `, ${skipped} skipped` : ""} in ${((Date.now() - started) / 1000).toFixed(0)} s`);
process.exit(failed ? 1 : 0);
