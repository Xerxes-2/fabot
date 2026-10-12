// An interleaved A/B of two bundles' CPU (ARCHITECTURE.md §9.1) over the
// soak: each round runs A and B side by side on the same room, seed and
// ticks, so both see the same machine load; rounds alternate which starts
// first. A claim needs identical fingerprints (every sample, fault, refusal
// and level but the clocks) and a per-module ratio whose rounds all lie past
// the noise an unchanged bundle shows against itself (`--a x.js --b x.js`).
//
// Usage: node scripts/xxscreeps/ab.mjs --a base.js --b new.js [--rounds R] [--jobs J]
//          [--cold K] [--tag name] [-- <soak.mjs flags: --node, --ticks, --room, --seed>]
//   --rounds  A/B pairs (default 4)
//   --jobs    soaks at once (default 8, an even number: whole pairs)
//   --cold    first ticks left out of the warm figures (default 10)
//   --again   report on the tag's runs already on disk, running nothing
//
// Per module: the mean over rounds, each round's B/A, ±2 standard errors of
// that ratio, and B/A of the sums of each tick's fastest round (the least
// disturbed by the machine's other load).
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const usage = "usage: node scripts/xxscreeps/ab.mjs --a A.js --b B.js [--rounds R] [--jobs J] [--cold K] [--tag name] [-- soak flags]";
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const argv = process.argv.slice(2);
const cut = argv.indexOf("--");
const soakFlags = cut < 0 ? [] : argv.slice(cut + 1);
const own = cut < 0 ? argv : argv.slice(0, cut);
const flags = {};
for (let i = 0; i < own.length; i += 2) {
  if (own[i] === "--again") {
    flags["--again"] = true;
    i--;
    continue;
  }
  if (!["--a", "--b", "--rounds", "--jobs", "--cold", "--tag"].includes(own[i]) || own[i + 1] === undefined) fail(usage);
  flags[own[i]] = own[i + 1];
}
if (!flags["--a"] || !flags["--b"]) fail(usage);
if (soakFlags.some((f) => f === "--run" || f === "--bot")) fail("--run and --bot are the A/B's own");
const rounds = Number(flags["--rounds"] ?? 4);
const jobs = Math.max(2, Number(flags["--jobs"] ?? 8));
const cold = Number(flags["--cold"] ?? 10);
const tag = flags["--tag"] ?? "ab";
const bots = { A: resolve(flags["--a"]), B: resolve(flags["--b"]) };

// A node's plan is built once here, not by every run at once.
const node = soakFlags.includes("--node");
if (node && spawnSync("dotnet", ["build", "-c", "Release", "--nologo", "-v", "q", join(REPO, "scripts/plan")], { stdio: "inherit" }).status !== 0)
  fail("scripts/plan did not build");

const soak = (side, round) =>
  new Promise((done, failed) => {
    const run = `${tag}-${side}-r${round}`;
    const child = spawn("node", [join(HERE, "soak.mjs"), "--bot", bots[side], "--run", run, ...soakFlags], {
      env: { ...process.env, ...(node ? { FABOT_PLAN_BUILT: "1" } : {}) },
      stdio: ["ignore", "ignore", "ignore"],
    });
    child.on("exit", (code) => (code === 0 ? done(ran(side, round)) : failed(new Error(`${run} exited ${code}`))));
  });

const pairs = [];
const ran = (side, k) => join(REPO, "soak-runs", `${tag}-${side}-r${k}`);
if (flags["--again"]) for (let k = 0; k < rounds; k++) pairs.push([ran("A", k), ran("B", k)]);
for (let r = 0; r < (flags["--again"] ? 0 : rounds); r += jobs / 2) {
  const batch = [];
  for (let k = r; k < Math.min(rounds, r + jobs / 2); k++) {
    const order = k % 2 === 0 ? ["A", "B"] : ["B", "A"];
    const started = {};
    for (const side of order) started[side] = soak(side, k);
    batch.push(Promise.all([started.A, started.B]));
  }
  pairs.push(...(await Promise.all(batch)));
  console.error(`rounds ${Math.min(rounds, r + jobs / 2)}/${rounds} done`);
}

const lines = (dir, f) => readFileSync(join(dir, f), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
const fingerprint = (dir) => {
  const hash = createHash("sha256");
  for (const { cpu, botCpu, ms, ...rest } of lines(dir, "samples.jsonl")) hash.update(JSON.stringify(rest));
  for (const f of ["faults.jsonl", "refusals.json", "levels.json"]) hash.update(readFileSync(join(dir, f)));
  return hash.digest("hex").slice(0, 12);
};
// Warm ms by tick and phase, with the bot's and engine's totals.
const ticksOf = (dir) =>
  lines(dir, "cpu.jsonl")
    .filter((row) => row.t >= cold && row.phases !== null)
    .map((row) => Object.fromEntries([...row.phases, ["bot", row.observed], ["engine", row.cpu ?? 0]]));
const perTick = (ticks) => {
  const sum = {};
  for (const tick of ticks) for (const [name, ms] of Object.entries(tick)) sum[name] = (sum[name] ?? 0) + ms;
  return Object.fromEntries(Object.entries(sum).map(([k, v]) => [k, v / ticks.length]));
};
const fastest = (runs) => runs[0].map((_, t) => Object.fromEntries(Object.keys(runs[0][t]).map((k) => [k, Math.min(...runs.map((r) => r[t]?.[k] ?? Infinity))])));

const prints = pairs.map(([a, b]) => [fingerprint(a), fingerprint(b)]);
const same = new Set(prints.flat()).size === 1;
console.log(`fingerprints ${same ? "identical" : "DIFFER"}: ${prints.map(([a, b]) => `${a}/${b}`).join(" ")}`);

const runs = pairs.map(([a, b]) => [ticksOf(a), ticksOf(b)]);
const profiles = runs.map(([a, b]) => [perTick(a), perTick(b)]);
const [minA, minB] = [perTick(fastest(runs.map(([a]) => a))), perTick(fastest(runs.map(([, b]) => b)))];
const phases = Object.keys(profiles[0][0]);
const mean = (xs) => xs.reduce((s, x) => s + x, 0) / xs.length;
const pct = (x) => `${x >= 0 ? "+" : ""}${(100 * x).toFixed(1)}%`;
console.log(["phase", "A ms", "B ms", "B/A", "±2se", "min B/A"].map((h) => h.padStart(10)).join("") + "  rounds");
for (const p of phases) {
  const a = profiles.map(([x]) => x[p] ?? 0);
  const b = profiles.map(([, y]) => y[p] ?? 0);
  const ratios = a.map((x, i) => (x > 0 ? b[i] / x - 1 : 0));
  const m = mean(ratios);
  const se = Math.sqrt(ratios.reduce((s, r) => s + (r - m) ** 2, 0) / Math.max(1, ratios.length - 1) / ratios.length);
  const row = [p, mean(a).toFixed(4), mean(b).toFixed(4), pct(m), `${(200 * se).toFixed(1)}%`, pct(minB[p] / minA[p] - 1)];
  console.log(`${row.map((c) => String(c).padStart(10)).join("")}  ${ratios.map(pct).join(" ")}`);
}
if (!same) process.exitCode = 1;
