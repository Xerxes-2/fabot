// The scenario library (ARCHITECTURE.md §10.2): each scenario in scenarios/
// is a start, a number of ticks and bounds on what the run measured. Starts
// are recipes (room, seed, late, node), so no world is checked in; scenarios
// with one start share one soak, run to the longest of their ticks. A start
// `at` tick T resumes the world a soak of `bundle` left at T: that bundle is
// pinned to a commit so the world is played once and cached under
// soak-runs/worlds/, and the bundle under test runs on from it as a deploy
// would, Memory kept.
//
// Usage: npm run scenario -- [name...] [--bot main.js] [--jobs J] [--nightly]
//   name    the scenarios to run (default all but the nightly ones), by file name
//   --nightly  all of them, with those marked "nightly": a start that needs
//           the network or a long first run, kept out of `jj ship`
//   --bot   the bundle (default dist/main.js)
//   --jobs  soaks at once (default half the cores)
//
// A scenario file: { "why": the invariant, "nightly"?, "start": { "room", "seed",
// "late"?, "node"?, "invaders"? (soak's --invaders), "core"? (soak's --core), "unsafe"?, "ended"?, "at"?, "bundle"?,
// "opponents"? (soak's --opponents) }, "ticks", "assert": [{ "measure", "from"?, "to"?, "max"?, "min"?, "who"? }] };
// a measure is one of `measures` below, read off the soak's output over ticks
// [from, to), counted from the run's first tick.
// Exits non-zero when a bound fails or a soak does, or loses ticks twice.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { cpSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync, symlinkSync, writeFileSync } from "node:fs";
import { availableParallelism, tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { bugs, engagement, report } from "./prediction.mjs";

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
  else if (rest[i] === "--nightly") flags[rest[i]] = true;
  else if (rest[i].startsWith("--")) fail("usage: npm run scenario -- [name...] [--bot main.js] [--jobs J] [--nightly]");
  else names.push(rest[i]);
const bot = resolve(flags["--bot"] ?? join(REPO, "dist/main.js"));
if (!existsSync(bot)) fail(`no bundle at ${bot}: npm run build first`);
const jobs = Number(flags["--jobs"] ?? Math.max(1, Math.floor(availableParallelism() / 2)));

const all = readdirSync(LIBRARY).filter((f) => f.endsWith(".json")).map((f) => basename(f, ".json"));
for (const n of names) if (!all.includes(n)) fail(`no scenario ${n}; the library: ${all.join(", ")}`);
const scenarios = (names.length ? names : all)
  .map((name) => ({ name, ...JSON.parse(readFileSync(join(LIBRARY, `${name}.json`), "utf8")) }))
  .filter((s) => names.length || flags["--nightly"] || !s.nightly);

// Ticks are counted from the bot's first, so a window means the same on every start.
// Each engagement the run priced, as `prediction.mjs` reads a row; one never
// priced predicted nothing.
const rowsOf = (dir) =>
  readFileSync(join(dir, "engagements.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
const engagementsOf = (dir) =>
  rowsOf(dir)
    .filter((e) => e.predicted)
    .map((e) => ({ p: e.predicted, a: engagement(e) }));
const samplesOf = (dir) => readFileSync(join(dir, "samples.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
const at = (samples, t) => samples.find((s) => s.t >= t) ?? samples.at(-1);
const sideOf = (a) => (s) => (a.who ? s.others[a.who] : s);
const within = (samples, a) => samples.filter((s) => s.t >= (a.from ?? 0) && s.t < (a.to ?? Infinity));
const measures = {
  // Energy lost to pile decay.
  decayed: (samples, a) => at(samples, a.to ?? Infinity).ledger.decayed - at(samples, a.from ?? 0).ledger.decayed,
  // Roads and containers lost between samples, over every room the soak follows.
  decayedAway: (samples, a) => {
    const standing = (s) => Object.values(s.rooms).reduce((n, r) => n + r.roads + r.containers, 0);
    const seen = within(samples, a);
    return seen.slice(1).reduce((n, s, i) => n + Math.max(0, standing(seen[i]) - standing(s)), 0);
  },
  // Energy spent on construction.
  build: (samples, a) => at(samples, a.to ?? Infinity).ledger.build - at(samples, a.from ?? 0).ledger.build,
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
  sourceContainersAtEnd: (samples, a) => at(samples, a.to ?? Infinity).sourceContainers,
  // Links standing beside a source's container.
  postedLinksAtEnd: (samples, a) => at(samples, a.to ?? Infinity).postedLinks,
  // Energy lost to link transfers: none means no link ever sent.
  linked: (samples, a) => at(samples, a.to ?? Infinity).ledger.linked - at(samples, a.from ?? 0).ledger.linked,
  // The least any declared outpost netted after its bodies (`rooms`, the
  // colony's own first); none declared: -Infinity.
  poorestOutpost: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    return Math.min(...Object.keys(last.rooms).slice(1).map((r) => last.rooms[r].net - (first.rooms[r]?.net ?? 0)));
  },
  // Ticks an enemy stood in the colony's own room.
  homeHostileTicks: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    const home = Object.keys(last.rooms)[0];
    return last.rooms[home].hostile - (first.rooms[home]?.hostile ?? 0);
  },
  // Engagements whose predicted Win or Kite was lost (§10.1).
  predictedWinsLost: (_, __, dir) => bugs(engagementsOf(dir)).length,
  // Engagements opened: an attack the bot never answered tests nothing.
  engagements: (_, __, dir) => engagementsOf(dir).length,
  // Acts the engine refused, every kind (the soak's refusals.json).
  refusals: (_, __, dir) => Object.values(JSON.parse(readFileSync(join(dir, "refusals.json"), "utf8"))).reduce((n, c) => n + c, 0),
  // Raids an answered engagement saw die without disengaging.
  raidsKilled: (_, __, dir) => engagementsOf(dir).filter(({ a }) => a.won === true).length,
  // The most energy one engagement cast, of the bodies that stood in it.
  mostCast: (_, __, dir) => Math.max(0, ...rowsOf(dir).map((e) => e.cast ?? 0)),
  faults: (_, __, dir) => readFileSync(join(dir, "faults.jsonl"), "utf8").split("\n").filter(Boolean).length,
  // The most levels the room stood below the highest it held before (`who`:
  // an opponent's room, else ours); a room lost stands at level 0.
  levelsLost: (samples, a) => {
    let top = 0;
    let worst = 0;
    for (const s of within(samples, a).map(sideOf(a))) {
      top = Math.max(top, s.rcl);
      worst = Math.max(worst, top - s.rcl);
    }
    return worst;
  },
  spawnsLost: (samples, a) => {
    const spawns = within(samples, a).map((s) => sideOf(a)(s).spawns);
    return spawns[0] - Math.min(...spawns);
  },
  fewestCreeps: (samples, a) => Math.min(...within(samples, a).map((s) => sideOf(a)(s).creeps)),
  // The first tick our room stood in safe mode (never: Infinity).
  safeModeAt: (samples, a) => within(samples, a).find((s) => s.safeMode)?.t ?? Infinity,
  creepsAtEnd: (samples, a) => sideOf(a)(at(samples, a.to ?? Infinity)).creeps,
  // Invader cores standing in the declared outposts at the window's end.
  coresAtEnd: (samples, a) => Object.values(at(samples, a.to ?? Infinity).rooms).reduce((n, r) => n + (r.cores ?? 0), 0),
  // Enemies standing in our room at the window's end.
  hostilesAtEnd: (samples, a) => at(samples, a.to ?? Infinity).hostiles,
  // `Hostiles.armed` off part letters; WORK counts anywhere, as every sample is our room.
  attackersAtEnd: (samples, a) => {
    const bodies = at(samples, a.to ?? Infinity).foes.map((f) => f.split(" ")[1]);
    const strikes = (b) => /[ar]/.test(b);
    return bodies.filter((b) => strikes(b) || b.includes("w") || (b.includes("h") && bodies.some(strikes))).length;
  },
};
for (const s of scenarios) for (const a of s.assert) if (!measures[a.measure]) fail(`${s.name}: no measure ${a.measure}`);
for (const { name, start } of scenarios)
  if ((start.at === undefined) !== (start.bundle === undefined) || (start.at !== undefined && (start.node || !/^[0-9a-f]{8,40}$/.test(start.bundle))))
    fail(`${name}: a start "at" a tick names its "bundle" by commit id, and no node`);

// A world holds its opponents' seats; their config is the scenario's.
const tag = (opponents = []) => (opponents.length ? `-o${createHash("sha256").update(JSON.stringify(opponents)).digest("hex").slice(0, 8)}` : "");
const seats = (opponents = []) => opponents.map(({ config, ...seat }) => seat);
const recipe = ({ room, seed, late = 0, node, invaders, core, unsafe, ended }, opponents) =>
  `${room}-s${seed}${late ? `l${late}` : ""}${node ? `-${node}` : ""}${invaders ? `-i${invaders.replace(/[:,]/g, "_")}` : ""}${core ? `-c${core.replace(/[:,]/g, "_")}` : ""}${unsafe ? "-u" : ""}${ended ? "-e" : ""}${tag(opponents)}`;
// A cached world is replayed only by the opponents' code that played it, and
// the engine mods and world scripts it was stood and played under.
const codeOf = (...paths) =>
  paths
    .map((p) => resolve(REPO, p))
    .flatMap((path) => (statSync(path).isDirectory() ? readdirSync(path).sort().map((f) => join(path, f)) : [path]))
    .reduce((h, f) => h.update(f).update(readFileSync(f)), createHash("sha256"))
    .digest("hex")
    .slice(0, 8);
const harness = codeOf(...["seed", "waves", "core", "colony.mjs", "node.mjs", "live.mjs"].map((f) => join(HERE, "engine", f)));
const worldOf = (start) => `${recipe(start, seats(start.opponents).map((s) => ({ ...s, code: codeOf(s.bot === "@bot" ? bot : s.bot) })))}-t${start.at}-${start.bundle}-h${harness}`;
const keyOf = (start) => `scenario-${basename(bot, ".js")}-${start.at ? `${worldOf(start)}${tag(start.opponents)}` : recipe(start, start.opponents)}`;
const runs = new Map();
for (const s of scenarios) {
  const run = runs.get(keyOf(s.start)) ?? { name: keyOf(s.start), start: s.start, ticks: 0 };
  run.ticks = Math.max(run.ticks, s.ticks);
  runs.set(run.name, run);
}

// Built once here: parallel builds of one project collide.
if ([...runs.values()].some((r) => r.start.node) && spawnSync("dotnet", ["build", "-c", "Release", "--nologo", "-v", "q", join(REPO, "scripts/plan")], { stdio: "inherit" }).status !== 0)
  fail("scripts/plan did not build");
// Built outside the repo: inside it, jj would name the build after this workspace.
const bundleAt = (commit) => {
  const out = join(REPO, "soak-runs/bundles", `${commit}.js`);
  if (existsSync(out)) return out;
  console.error(`building ${commit}`);
  const src = mkdtempSync(join(tmpdir(), "fabot-"));
  const sh = (cmd) => spawnSync("sh", ["-c", cmd], { cwd: src, stdio: ["ignore", "ignore", "inherit"] }).status === 0 || fail(`${commit}: ${cmd} failed`);
  sh(`git -C '${REPO}' archive ${commit} | tar -x`);
  symlinkSync(join(REPO, "node_modules"), join(src, "node_modules"));
  sh("dotnet tool restore && npm run build --silent");
  mkdirSync(dirname(out), { recursive: true });
  cpSync(join(src, "dist/main.js"), out);
  rmSync(src, { recursive: true, force: true });
  return out;
};
for (const { start } of runs.values()) if (start.bundle) bundleAt(start.bundle);
const started = Date.now();
const soak = (args) =>
  new Promise((done) => {
    const child = spawn("node", [join(HERE, "soak.mjs"), ...args], { stdio: "ignore", env: { ...process.env, FABOT_PLAN_BUILT: "1" } });
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
const play = async ({ name, start, ticks }) => {
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
      ...(start.invaders ? ["--invaders", start.invaders] : []),
      ...(start.core ? ["--core", start.core] : []),
      ...(start.unsafe ? ["--unsafe"] : []),
      ...(start.ended ? ["--ended"] : []),
      ...opponentsOf(name, start.opponents),
    ]);
  const world = join(REPO, "soak-runs/worlds", worldOf(start));
  if (!existsSync(world)) {
    const code = await soak([...recipe, "--ticks", String(start.at), "--run", `world-${worldOf(start)}`, "--bot", bundleAt(start.bundle), "--save", world, ...opponentsOf(`world-${name}`, start.opponents && seats(start.opponents))]);
    if (code !== 0) {
      rmSync(world, { recursive: true, force: true });
      return code;
    }
  }
  return soak(["--room", start.room, "--seed", String(start.seed), "--ticks", String(ticks), "--run", name, "--bot", bot, "--from", world, ...opponentsOf(name, start.opponents)]);
};
const playAll = (list, jobs) => {
  const queue = [...list];
  return Promise.all(
    Array.from({ length: jobs }, async () => {
      for (let run; (run = queue.shift()); ) run.code = await play(run);
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
  await playAll(invalid, Math.max(1, jobs >> 1));
}

let failed = 0;
for (const s of scenarios) {
  const run = runs.get(keyOf(s.start));
  const dir = join(REPO, "soak-runs", run.name);
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
  for (const a of s.assert) {
    const value = measures[a.measure](samples, a, dir);
    const ok = value <= (a.max ?? Infinity) && value >= (a.min ?? -Infinity);
    if (!ok) failed++;
    const window = a.from !== undefined || a.to !== undefined ? ` t${a.from ?? 0}-${a.to ?? s.ticks}` : "";
    const bound = [a.min !== undefined && `≥ ${a.min}`, a.max !== undefined && `≤ ${a.max}`].filter(Boolean).join(", ");
    console.log(`${ok ? "pass" : "FAIL"} ${s.name}: ${a.measure}${a.who ? ` of ${a.who}` : ""}${window} ${value} (${bound})`);
  }
  const rows = s.assert.some((a) => a.measure === "predictedWinsLost") ? engagementsOf(dir) : [];
  if (rows.length) for (const line of [`${rows.length} engagements`, ...report(rows)]) console.log(`  ${s.name}: ${line}`);
}
console.log(`${failed ? `${failed} failed` : "all passed"} in ${((Date.now() - started) / 1000).toFixed(0)} s`);
process.exit(failed ? 1 : 0);
