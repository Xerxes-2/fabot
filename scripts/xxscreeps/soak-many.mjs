// The soak over several rooms, seeds and bundles at once, so an economy change
// is judged against its base on more than one room's terrain and more than
// one seed (ARCHITECTURE.md §10.2): `soak.mjs` once per (bundle, seed, room),
// many at a time, then one table.
//
// Usage: npm run soak:many -- [--bot [name=]a.js,[name=]b.js] [--seed 1,2] [--room A,B,...]
//                             [--ticks T] [--jobs J] [--tag name] [--fresh]
//   --bot    the bundles to compare (default dist/main.js); the first is the
//            base the others are read against. A name defaults to the file's.
//   --seed   the seeds (default 1,2)
//   --room   these rooms instead of the two extremes of harvest seats: both
//            sources with one seat (a small harvester cannot fill a source)
//            and both with four or more (bodies can crowd in instead)
//   --ticks  per run (default 30000)
//   --jobs   runs at once (default half the cores: each holds a server and
//            our sandbox, about one core)
//   --tag    output names: soak-runs/<tag>-<bundle>-s<seed>-<room>/ (default many)
//   --fresh  run even where the cache holds this bundle, room, seed and ticks
//
// A finished run is cached under soak-runs/.cache/ by its bundle's code (the
// version stamp aside), room, seed, ticks and the soak scripts, so a base that
// has not changed is not run again.
import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { cpSync, existsSync, readFileSync, readdirSync, rmSync } from "node:fs";
import { availableParallelism } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const SOAK = join(HERE, "soak.mjs");
const RUNS = join(REPO, "soak-runs");
const CACHE = join(RUNS, ".cache");
const usage =
  "usage: npm run soak:many -- [--bot [name=]a.js,...] [--seed 1,2] [--room A,B,...] [--ticks T] [--jobs J] [--tag name] [--fresh]";
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
  if (!["--room", "--ticks", "--seed", "--jobs", "--tag", "--bot"].includes(rest[i])) fail(usage);
  const value = rest[i + 1];
  if (value === undefined || value.startsWith("--")) fail(`${rest[i]} needs a value\n${usage}`);
  flags[rest[i++]] = value;
}
const whole = (text, flag, min) => {
  const n = Number(text);
  if (!(Number.isInteger(n) && n >= min)) fail(`${flag} needs whole numbers ≥ ${min}`);
  return n;
};
const ticks = whole(flags["--ticks"] ?? 30000, "--ticks", 1);
const seeds = (flags["--seed"] ?? "1,2").split(",").map((s) => whole(s, "--seed", 0));
const jobs = whole(flags["--jobs"] ?? Math.max(1, Math.floor(availableParallelism() / 2)), "--jobs", 1);
const tag = flags["--tag"] ?? "many";
if (!/^[A-Za-z0-9-]+$/.test(tag)) fail("--tag: letters, digits, dashes");
const rooms = (flags["--room"] ?? "W8N7,W9N8,W3N1,W8N3").split(",");

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
for (const b of bots) b.code = hash(readFileSync(b.file, "utf8").replace(/^var __FABOT_VERSION__ = .*$/m, ""));

const runs = bots.flatMap((bot) =>
  seeds.flatMap((seed) =>
    rooms.map((room) => ({
      bot,
      seed,
      room,
      name: `${tag}-${bot.name}-s${seed}-${room}`,
      key: hash(bot.code, room, String(seed), String(ticks), scriptsHash),
    })),
  ),
);

const cached = (run) => !flags["--fresh"] && existsSync(join(CACHE, run.key, "levels.json"));
// Two bundles of the same code share their runs.
const todo = runs.filter((r, i) => !cached(r) && runs.findIndex((o) => o.key === r.key) === i);
console.error(
  `${bots.map((b) => b.name).join(" vs ")}; rooms ${rooms.join(", ")}; seeds ${seeds.join(", ")}; ${ticks} ticks; ` +
    `${runs.length - todo.length} cached, ${todo.length} to run, ${jobs} at a time`,
);

function soak(run) {
  return new Promise((done) => {
    const args = ["--room", run.room, "--run", run.name, "--ticks", String(ticks), "--seed", String(run.seed), "--every", "100", "--bot", run.bot.file];
    const child = spawn("node", [SOAK, ...args], { stdio: ["ignore", "ignore", "ignore"] });
    child.on("exit", (code) => done(code));
  });
}

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
  const at = (rcl) => levels.find((l) => l.rcl >= rcl)?.t;
  return {
    ok: !failed(run),
    rcl: last.rcl,
    progress: last.progress,
    rcl2: at(2),
    rcl3: at(3),
    rcl4: at(4),
    dropped: samples.find((s) => s.t >= 8000)?.dropped,
    faults,
    refusals,
  };
};

const cell = (v, w = 9) => String(v ?? "-").padStart(w);
console.log(["room", "seed", "bundle", "ok", "rcl", "progress", "rcl2", "rcl3", "rcl4", "drop@8k", "faults", "refusals"].map((h, i) => cell(h, i === 2 ? 12 : 9)).join(""));
for (const room of rooms)
  for (const seed of seeds)
    for (const bot of bots) {
      const run = runs.find((r) => r.room === room && r.seed === seed && r.bot === bot);
      const r = rowOf(run);
      console.log(
        [room, seed, bot.name, r.ok ? "yes" : "NO", r.rcl, r.progress, r.rcl2, r.rcl3, r.rcl4, r.dropped, r.faults, r.refusals]
          .map((v, i) => cell(v, i === 2 ? 12 : 9))
          .join(""),
      );
    }
if (Object.values(codes).some((c) => c !== 0)) process.exit(1);
