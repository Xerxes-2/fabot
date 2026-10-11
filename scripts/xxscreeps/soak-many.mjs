// The soak over several rooms, so an economy change is judged on more than one
// room's terrain (ARCHITECTURE.md §10.2): `soak.mjs` once per room, a few at a
// time, then one table and the medians.
//
// Usage: npm run soak:many -- [--rooms N | --room A,B,...] [--ticks T] [--seed S] [--jobs J] [--tag name]
//   --rooms  how many rooms, spread evenly over the ones a soak can stand in
//            (`soak.mjs --list`, by name); default 5
//   --room   these rooms instead
//   --ticks  per room (default 30000), --seed (default 1)
//   --jobs   soaks at once (default 2: each holds a server and our sandbox)
//   --tag    output names: soak-runs/<tag>-<room>/ (default many)
//   --bot    the bundle every room runs (default dist/main.js)
import { spawn, spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const SOAK = join(HERE, "soak.mjs");
const usage = "usage: npm run soak:many -- [--rooms N | --room A,B,...] [--ticks T] [--seed S] [--jobs J] [--tag name] [--bot main.js]";
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const flags = {};
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++) {
  if (!["--rooms", "--room", "--ticks", "--seed", "--jobs", "--tag", "--bot"].includes(rest[i])) fail(usage);
  const value = rest[i + 1];
  if (value === undefined || value.startsWith("--")) fail(`${rest[i]} needs a value\n${usage}`);
  flags[rest[i++]] = value;
}
const whole = (flag, fallback, min) => {
  const n = Number(flags[flag] ?? fallback);
  if (!(Number.isInteger(n) && n >= min)) fail(`${flag} needs a whole number ≥ ${min}`);
  return n;
};
const ticks = whole("--ticks", 30000, 1);
const seed = whole("--seed", 1, 0);
const jobs = whole("--jobs", 2, 1);
const tag = flags["--tag"] ?? "many";
if (!/^[A-Za-z0-9-]+$/.test(tag)) fail("--tag: letters, digits, dashes");

let rooms;
if (flags["--room"]) {
  rooms = flags["--room"].split(",");
} else {
  const listed = spawnSync("node", [SOAK, "--list"], { encoding: "utf8", stdio: ["ignore", "pipe", "inherit"] });
  if (listed.status !== 0) fail("soak.mjs --list failed");
  const all = JSON.parse(listed.stdout.trim().split("\n").at(-1));
  const n = Math.min(whole("--rooms", 5, 1), all.length);
  // Evenly spread, deterministic: the i-th of n strides through the list.
  rooms = Array.from({ length: n }, (_, i) => all[Math.floor((i * all.length) / n)]);
}
console.error(`rooms: ${rooms.join(", ")}; ${ticks} ticks each, seed ${seed}, ${jobs} at a time`);

const runOf = (room) => `${tag}-${room}`;
function soak(room) {
  return new Promise((done) => {
    const child = spawn("node", [SOAK, "--room", room, "--run", runOf(room), "--ticks", String(ticks), "--seed", String(seed), "--every", "100", ...(flags["--bot"] ? ["--bot", resolve(flags["--bot"])] : [])], {
      stdio: ["ignore", "ignore", "ignore"],
    });
    child.on("exit", (code) => done(code));
  });
}

const queue = [...rooms];
const codes = {};
await Promise.all(
  Array.from({ length: jobs }, async () => {
    while (queue.length) {
      const room = queue.shift();
      codes[room] = await soak(room);
      console.error(`${room}: ${codes[room] === 0 ? "done" : `failed (exit ${codes[room]})`}`);
    }
  }),
);

const read = (room, file) => {
  const path = join(REPO, "soak-runs", runOf(room), file);
  return existsSync(path) ? readFileSync(path, "utf8") : null;
};
const median = (xs) => {
  const s = xs.filter((x) => x !== undefined && x !== null).sort((a, b) => a - b);
  return s.length ? s[Math.floor((s.length - 1) / 2)] : undefined;
};

const rows = rooms.map((room) => {
  const samples = (read(room, "samples.jsonl") ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l));
  const levels = JSON.parse(read(room, "levels.json") ?? "[]");
  const faults = (read(room, "faults.jsonl") ?? "").split("\n").filter(Boolean).length;
  const refusals = Object.values(JSON.parse(read(room, "refusals.json") ?? "{}")).reduce((a, b) => a + b, 0);
  const last = samples.at(-1) ?? {};
  const at = (rcl) => levels.find((l) => l.rcl >= rcl)?.t;
  return { room, ok: codes[room] === 0, rcl: last.rcl, progress: last.progress, rcl2: at(2), rcl3: at(3), rcl4: at(4), faults, refusals };
});

const cell = (v) => String(v ?? "-").padStart(9);
console.log(["room", "ok", "rcl", "progress", "rcl2", "rcl3", "rcl4", "faults", "refusals"].map(cell).join(""));
for (const r of rows) console.log([r.room, r.ok ? "yes" : "NO", r.rcl, r.progress, r.rcl2, r.rcl3, r.rcl4, r.faults, r.refusals].map(cell).join(""));
const done = rows.filter((r) => r.ok);
console.log(
  ["median", "", median(done.map((r) => r.rcl)), median(done.map((r) => r.progress)), median(done.map((r) => r.rcl2)), median(done.map((r) => r.rcl3)), median(done.map((r) => r.rcl4)), done.reduce((a, r) => a + r.faults, 0), done.reduce((a, r) => a + r.refusals, 0)]
    .map(cell)
    .join(""),
);
if (done.length < rows.length) process.exit(1);
