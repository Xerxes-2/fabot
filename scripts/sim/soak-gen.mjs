// Breadth for the economy: one soak per seed on the world map.js generates off
// it (`sim --gen`), each held to `measures.mjs`'s invariants. Seeds do not
// shrink: a broken seed is shrunk in time only, to the first tick it breaks at.
//
// Usage: npm run soak:gen -- --seeds A..B|A,B,... [--ticks N] [--jobs J]
//   --ticks  per seed (default 30000)
//   --jobs   seeds at once (default every allowed core)
// Prints a row per seed and, under the table, each broken seed's first broken
// tick and the commands that replay and watch it; the exit is non-zero then.
import { existsSync, readFileSync } from "node:fs";
import { availableParallelism } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { counted, failures, samplesOf } from "../scenario/measures.mjs";
import { runSim, simBinary } from "./run.mjs";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const usage = "usage: npm run soak:gen -- --seeds A..B|A,B,... [--ticks N] [--jobs J]";
const flags = {};
for (let i = 2; i < process.argv.length; i += 2) flags[process.argv[i]] = process.argv[i + 1];
const whole = (v, flag) => (/^\d+$/.test(String(v)) ? Number(v) : (console.error(`${flag}: ${usage}`), process.exit(2)));
const span = /^(\d+)\.\.(\d+)$/.exec(flags["--seeds"] ?? "");
const seeds = span
  ? Array.from({ length: Math.max(0, Number(span[2]) - Number(span[1]) + 1) }, (_, i) => Number(span[1]) + i)
  : (flags["--seeds"] ?? "").split(",").filter(Boolean).map((s) => whole(s, "--seeds"));
if (!seeds.length) (console.error(usage), process.exit(2));
const ticks = whole(flags["--ticks"] ?? 30000, "--ticks");
const jobs = whole(flags["--jobs"] ?? availableParallelism(), "--jobs");
const bin = simBinary();
if (!bin) process.exit(1);

const dirOf = (seed) => join(REPO, "soak-runs", `gen-s${seed}`);
const codes = {};
const queue = [...seeds];
await Promise.all(
  Array.from({ length: jobs }, async () => {
    for (let seed; (seed = queue.shift()) !== undefined; ) {
      codes[seed] = await runSim(bin, { gen: seed, ticks, out: dirOf(seed) });
      console.error(`seed ${seed}: ${codes[seed] === 0 ? "done" : `failed (exit ${codes[seed]})`}`);
    }
  }),
);

const lines = (dir, file) => (existsSync(join(dir, file)) ? readFileSync(join(dir, file), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l)) : []);
// The tick each broken invariant is first seen at: a fault's and a refusal's own
// (the whole run's files, not the samples, hold them), else the first sample
// whose prefix breaks it.
function firstBroken(dir, samples, broken) {
  const cpu = lines(dir, "cpu.jsonl");
  const tickOf = (time) => time - (cpu[0].time - cpu[0].t);
  const own = {
    faults: () => tickOf(lines(dir, "faults.jsonl")[0].tick),
    refusals: () => cpu.find((r) => Object.keys(r.refused ?? {}).some(counted))?.t,
  };
  const first = broken.filter((f) => own[f.measure]).map((f) => ({ t: own[f.measure](), what: f.text }));
  if (broken.some((f) => !own[f.measure]))
    for (let i = 1; i <= samples.length; i++) {
      const b = failures(dir, samples.slice(0, i)).filter((f) => !own[f.measure]);
      if (b.length) {
        first.push({ t: samples[i - 1].t, what: b.map((f) => f.text).join("; ") });
        break;
      }
    }
  return first.sort((a, b) => a.t - b.t);
}

const rows = seeds.map((seed) => {
  const dir = dirOf(seed);
  if (codes[seed] !== 0) return { seed, crashed: codes[seed] };
  const samples = samplesOf(dir);
  const last = samples.at(-1);
  const levels = JSON.parse(readFileSync(join(dir, "levels.json"), "utf8"));
  const s = last.score;
  const broken = failures(dir, samples);
  return {
    seed,
    room: Object.keys(last.rooms)[0],
    rcl: last.rcl,
    at: Object.fromEntries([2, 3, 4, 5, 6, 7, 8].map((l) => [l, levels.find((x) => x.rcl >= l)?.t])),
    score: s ? Math.round(s.controller + s.structures + s.fortification + s.stock) : 0,
    broken,
    first: broken.length ? firstBroken(dir, samples, broken) : [],
  };
});

const cell = (v, w = 8) => String(v ?? "-").padStart(w);
console.log(["seed", "room", "rcl", ...[2, 3, 4, 5, 6, 7, 8].map((l) => `rcl${l}`), "score", "ok"].map((h) => cell(h)).join(""));
for (const r of rows)
  console.log(
    (r.crashed !== undefined ? [r.seed, "-", "-", ...Array(7).fill("-"), "-", `exit ${r.crashed}`] : [r.seed, r.room, r.rcl, ...Object.values(r.at), `${Math.round(r.score / 1000)}k`, r.broken.length ? "NO" : "yes"])
      .map((v) => cell(v))
      .join(""),
  );
const ran = rows.filter((r) => r.crashed === undefined);
const spread = (vs) => {
  const v = vs.filter((x) => x !== undefined).sort((a, b) => a - b);
  return v.length ? `min ${v[0]} median ${v[v.length >> 1]} max ${v.at(-1)} (${v.length}/${ran.length})` : "none";
};
for (const l of [2, 3, 4, 5, 6, 7, 8]) console.log(`rcl${l} at: ${spread(ran.map((r) => r.at[l]))}`);
console.log(`score: ${spread(ran.map((r) => r.score))}`);
for (const r of rows.filter((r) => r.crashed !== undefined || r.broken.length)) {
  const first = r.first?.[0];
  console.log(`\nseed ${r.seed}${r.room ? ` (${r.room})` : ""}: ${r.crashed !== undefined ? `sim exited ${r.crashed}` : `first broken at t${first.t}: ${first.what}`}`);
  for (const f of r.first?.slice(1) ?? []) console.log(`  then t${f.t}: ${f.what}`);
  console.log(`  replay: ${bin} --gen ${r.seed} --ticks ${first ? first.t + 1 : ticks} --out .scratch/gen-s${r.seed}`);
  console.log(`  watch:  npm run watch:sim -- --gen ${r.seed}`);
}
if (rows.some((r) => r.crashed !== undefined || r.broken.length)) process.exit(1);
