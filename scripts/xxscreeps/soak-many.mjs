// The soak over several rooms, seeds and bundles at once, so an economy change
// is judged against its base on more than one room's terrain and more than
// one seed (ARCHITECTURE.md §10.2): `soak.mjs` once per (bundle, seed, room),
// many at a time, then one table.
//
// Usage: npm run soak:many -- [--bot [name=]a.js,[name=]b.js] [--seed 1,2,1+7] [--room A,B,...]
//                             [--ticks T] [--jobs J] [--tag name] [--node N] [--fresh]
//   --bot    the bundles to compare (default dist/main.js); the first is the
//            base the others are read against. A name defaults to the file's.
//   --seed   the starts for every room (default 1,2,1+7, the contested start
//            1,2,3,4,1+7,2+7: its spread is wider): an engine seed, or seed+K
//            for a bot that sits out its first K ticks (soak.mjs --late), a
//            nudge the seed alone does not give a room with few tie-breaks
//   --room   these starts instead of the default: the extremes of harvest
//            seats, both sources with one seat (a small harvester cannot fill
//            a source) and both with four or more (bodies can crowd in
//            instead), the late game, and a contested start: E1 on Season
//            11's map with Hivemind seated two rooms off, contesting our
//            outposts. A start is [<world>:]<room>[@<node>][+<bot>:<room>],
//            <world> one of worlds/ with no home of ours (soak.mjs --world,
//            --node), <bot> a rival under engine/ seated in <room>
//            (soak.mjs --opponents), our safe mode kept to activate (--ended)
//   --ticks  per run (default 30000; a node's 10000)
//   --node   start every run without its own node at this one
//   --jobs   runs at once (default a quarter of the hardware threads: a soak
//            keeps server, processor, runner and stepper threads busy, and
//            more jobs only slow each other)
//   --tag    output names: soak-runs/<tag>-<bundle>-s<seed>-<room>/ (default many)
//   --fresh  run even where the cache holds this bundle, room, seed and ticks
//
// A finished run is cached under soak-runs/.cache/ by its bundle's code (the
// version stamp aside), start, seed, ticks and the soak scripts (with the
// plan a node is built from, and the world), so a base that
// has not changed is not run again.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { cpSync, existsSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { availableParallelism } from "node:os";
import { node26Env } from "./node26.mjs";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { sweep } from "./prune.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const SOAK = join(HERE, "soak.mjs");
const RUNS = join(REPO, "soak-runs");
const CACHE = join(RUNS, ".cache");
const usage =
  "usage: npm run soak:many -- [--bot [name=]a.js,...] [--seed 1,2,1+7] [--room [world:]A[@N],...] [--ticks T] [--jobs J] [--tag name] [--node N] [--fresh]";
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const flags = {};
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++) {
  if (rest[i] === "--fresh") {
    flags["--fresh"] = true;
    continue;
  }
  if (!["--room", "--ticks", "--seed", "--jobs", "--tag", "--bot", "--node"].includes(rest[i])) fail(usage);
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
const CONTESTED = "season-outpost-ring:E25S9@E1+hivemind:E27S9";
const starts = (flags["--room"] ?? `W9N8,W8N3,season-late-seat:W12S28@N7,${CONTESTED}`).split(",").map((spec) => {
  const m = /^(?:([\w-]+):)?([WE]\d+[NS]\d+)(?:@([A-Za-z0-9]+))?(?:\+([a-z]+):([WE]\d+[NS]\d+))?$/.exec(spec);
  if (!m) fail(`--room: "${spec}" is not [world:]room[@node][+bot:room]`);
  const world = m[1] && join(HERE, "worlds", `${m[1]}.json`);
  if (world && !existsSync(world)) fail(`--room: no world ${world}`);
  const at = m[3] ?? node;
  const rival = m[4] && { name: m[4], bot: join(HERE, "engine", m[4]), room: m[5] };
  if (rival && !existsSync(rival.bot)) fail(`--room: no rival ${rival.bot}`);
  return { label: spec, room: m[2], world, node: at, rival, seeds: parseSeeds(flags["--seed"] ?? (spec === CONTESTED ? "1,2,3,4,1+7,2+7" : "1,2,1+7")), ticks: whole(flags["--ticks"] ?? (at ? 10000 : 30000), "--ticks", 1) };
});
const jobs = whole(flags["--jobs"] ?? Math.max(1, Math.floor(availableParallelism() / 4)), "--jobs", 1);
const tag = flags["--tag"] ?? "many";
if (!/^[A-Za-z0-9-]+$/.test(tag)) fail("--tag: letters, digits, dashes");

const bots = (flags["--bot"] ?? join(REPO, "dist/main.js")).split(",").map((spec) => {
  const [name, path] = spec.includes("=") ? spec.split("=") : [basename(spec, ".js"), spec];
  if (!/^[A-Za-z0-9-]+$/.test(name)) fail(`--bot: "${name}" is not a name (letters, digits, dashes)`);
  const file = resolve(path);
  if (!existsSync(file)) fail(`--bot: no file at ${file}`);
  return { name, file };
});
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
  ...[SOAK, ...readdirSync(join(HERE, "engine"), { recursive: true }).map((f) => join(HERE, "engine", f))]
    .filter((f) => f.endsWith(".mjs") || f.endsWith(".js"))
    .sort()
    .map((f) => readFileSync(f)),
);
// A node is built from the plan of this tree's Layout, whatever the bundle.
const planHash = hash(...[join(REPO, "src/bin/plan.rs"), ...readdirSync(join(REPO, "src/core/layout")).sort().map((f) => join(REPO, "src/core/layout", f))].map((f) => readFileSync(f)));
const wasmOf = (file) => [file.replace(/\.js$/, ".wasm")].filter(existsSync).map((f) => readFileSync(f));
for (const b of bots) b.code = hash(readFileSync(b.file, "utf8").replace(/^(?:var |globalThis\.)__FABOT_VERSION__ = .*$/m, ""), ...wasmOf(b.file));

const runs = bots.flatMap((bot) =>
  starts.flatMap((start) =>
    start.seeds.map((seed) => ({
      bot,
      seed,
      start,
      name: `${tag}-${bot.name}-s${seed.seed}${seed.late ? `l${seed.late}` : ""}-${start.label.replace(/[:@+]/g, "-")}`,
      key: hash(bot.code, start.room, start.rival ? `${start.rival.name}:${start.rival.room}` : "", start.node ? planHash + start.node : "", start.world ? readFileSync(start.world) : "", String(seed.seed), String(seed.late), String(start.ticks), scriptsHash),
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
  return new Promise((done) => {
    const { room, world, node, rival, ticks } = run.start;
    const opponents = join(RUNS, `${run.name}.opponents.json`);
    if (rival) writeFileSync(opponents, JSON.stringify([rival]));
    const args = [...(rival ? ["--opponents", opponents, "--ended"] : []), "--room", room, "--run", run.name, "--ticks", String(ticks), "--seed", String(run.seed.seed), "--late", String(run.seed.late), "--every", "100", "--bot", run.bot.file, ...(node ? ["--node", node] : []), ...(world ? ["--world", world] : [])];
    const child = spawn("node", [SOAK, ...args], { stdio: ["ignore", "ignore", "ignore"], env: node26Env({ ...process.env, FABOT_PLAN_BUILT: "1" }) });
    child.on("exit", (code) => {
      rmSync(opponents, { force: true });
      done(code);
    });
  });
}

// Built once here: parallel builds of one project collide.
if (todo.some((r) => r.start.node) && spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "plan", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: "inherit" }).status !== 0)
  fail("the plan tool did not build");
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
const read = (run, file) => {
  const path = join(failed(run) ? join(RUNS, ranAs(run).name) : join(CACHE, run.key), file);
  return existsSync(path) ? readFileSync(path, "utf8") : null;
};

const rowOf = (run) => {
  const samples = (read(run, "samples.jsonl") ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l));
  const levels = JSON.parse(read(run, "levels.json") ?? "[]");
  const faults = (read(run, "faults.jsonl") ?? "").split("\n").filter(Boolean).length;
  const refusals = Object.values(JSON.parse(read(run, "refusals.json") ?? "{}")).reduce((a, b) => a + b, 0);
  const last = samples.at(-1) ?? {};
  const sum = (named) =>
    (read(run, "metrics.jsonl") ?? "").split("\n").filter(Boolean).reduce((a, l) => {
      const { values } = JSON.parse(l);
      return a + Object.entries(values).reduce((b, [n, v]) => b + (named(n) ? v : 0), 0);
    }, 0);
  const at = (rcl) => levels.find((l) => l.rcl >= rcl)?.t;
  // What the economy ends with, in energy, as the room stands at the last
  // sample (the sampler's `worth`): every economy shows up here; a
  // diagnostic belongs in the run's own samples, not in this table.
  const ledger = last.ledger;
  const w = last.worth;
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
    exposure: sum((n) => n === "exposure"),
    lostHeld: `${(sum((n) => n.startsWith("loss.")) / 1000).toFixed(1)}k/${(sum((n) => n === "held") / 1000).toFixed(1)}k`,
    // The ledger closes within a percent of what was dug, or the worth is
    // not to be read.
    open: ledger && ledger.dug > 0 && Math.abs(ledger.residual) > ledger.dug / 100,
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
        [start.label.split("+")[0].split(":").at(-1), seed.label, bot.name, r.ok ? "yes" : "NO", r.rcl, r.rcl3, r.rcl4, r.controller, r.structures, r.fortified, r.stock, r.open ? `${r.worth}?` : r.worth, r.open ? `${r.score}?` : r.score, r.faults, r.exposure, r.lostHeld]
          .map((v, i) => cell(v, i === 0 || i === 2 ? 12 : i === 15 ? 14 : 9))
          .join(""),
      );
    }
if (Object.values(codes).some((c) => c !== 0)) process.exit(1);
