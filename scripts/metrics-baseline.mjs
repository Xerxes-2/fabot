// The metrics' distribution over healthy soaks, and the windows each run's
// THRESHOLDS (core/observe/metrics.rs) would alert (ARCHITECTURE.md §10.5).
//
// Usage: npm run metrics:baseline -- <soak run dir>... [--skip T]
//   --skip  ticks from a run's first window left out, the colony forming
//           (default 2000; 0 for a live capture's world)
// A row is the world's (*), a colony's (a room with spawns) or another room's
// (outpost); a metric missing from a row reads 0, but for a share whose
// denominator is missing and a site's age where no site stands.
import { readFileSync } from "node:fs";
import { basename, join } from "node:path";
import { METRIC as consts, SOURCE as source, WINDOW } from "./metrics.mjs";

const args = process.argv.slice(2);
const at = args.indexOf("--skip");
const skip = at < 0 ? 2000 : Number(args.splice(at, 2)[1]);
if (!args.length || !Number.isInteger(skip)) {
  console.error("usage: npm run metrics:baseline -- <soak run dir>... [--skip T]");
  process.exit(1);
}

const named = (family, name) => family === name || (family.endsWith(".") && name.startsWith(family));
const slice = (from) => source.slice(source.indexOf(from), source.indexOf("];", source.indexOf(from)));
const table = slice("const THRESHOLDS");
const thresholds = [...table.matchAll(/\((?:"([^"]+)"|(\w+)), Bound::(Max|Min)\((-?\d+)\)\)/g)].map((m) => ({
  name: m[1] ?? consts[m[2]],
  max: m[3] === "Max",
  bound: Number(m[4]),
}));
if (thresholds.length !== table.split("Bound::").length - 1) throw new Error("a THRESHOLDS row this script cannot read");
const shares = [...slice("const METRICS").matchAll(/(\w+),\s*Fold::Share\((\w+)\)/g)].map((m) => ({ name: consts[m[1]], of: consts[m[2]] }));
const bound = (name) => thresholds.find((t) => named(t.name, name));
const past = (name, v) => {
  const t = bound(name);
  return t && (t.max ? v > t.bound : v < t.bound);
};
const defined = (name, values) => {
  if (name === consts.STALE) return name in values;
  const share = shares.find((s) => named(s.name, name));
  if (!share) return true;
  const of = share.of.endsWith(".") ? share.of + name.slice(name.indexOf(".") + 1) : share.of;
  return values[of] > 0;
};

const samples = {};
const worst = {};
console.log("run: windows alerting / complete windows past --skip; alerts by metric");
for (const dir of args) {
  const lines = readFileSync(join(dir, "metrics.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
  const first = lines.reduce((a, l) => Math.min(a, l.start), Infinity);
  const kept = lines.filter((l) => l.ticks === WINDOW && l.start - first >= skip);
  const colonies = new Set(kept.filter((l) => l.values[consts.SPAWN_TICKS]).map((l) => l.row));
  const kind = (row) => (row === "*" ? "world" : colonies.has(row) ? "colony" : "outpost");
  const names = {};
  for (const l of kept) for (const n of Object.keys(l.values)) (names[kind(l.row)] ??= new Set()).add(n);
  const alerting = new Set();
  const fired = {};
  for (const l of kept) {
    for (const name of names[kind(l.row)]) {
      if (!defined(name, l.values)) continue;
      const v = l.values[name] ?? 0;
      const key = `${name} ${kind(l.row)}`;
      (samples[key] ??= []).push(v);
      const low = bound(name)?.max === false;
      if (!worst[key] || (low ? v < worst[key].v : v > worst[key].v)) worst[key] = { v, at: `${basename(dir)} ${l.row} t${l.start}` };
      if (past(name, v)) {
        alerting.add(l.start);
        fired[name] = (fired[name] ?? 0) + 1;
      }
    }
  }
  const windows = new Set(kept.map((l) => l.start)).size;
  const list = Object.entries(fired).map(([n, k]) => `${n} ${k}`).join(", ");
  console.log(`${basename(dir)}: ${alerting.size}/${windows}${list && `; ${list}`}`);
}

// Nearest rank.
const pct = (sorted, p) => sorted[Math.max(0, Math.ceil((p / 100) * sorted.length) - 1)];
console.log("\nmetric row: n min p50 p95 p99 max (threshold) — the worst window's run, row and start");
for (const key of Object.keys(samples).sort()) {
  const s = samples[key].sort((a, b) => a - b);
  const t = bound(key.split(" ")[0]);
  const limit = t ? ` (${t.max ? "≤" : "≥"}${t.bound})` : "";
  console.log(`${key}: ${s.length} ${s[0]} ${pct(s, 50)} ${pct(s, 95)} ${pct(s, 99)} ${s.at(-1)}${limit} — ${worst[key].at}`);
}
