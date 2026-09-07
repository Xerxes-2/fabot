import { execSync } from "node:child_process";
const names = execSync("node --env-file=.env scripts/observe.mjs tasks", { encoding: "utf8" }).split("\n").map(l => l.trim().split(/\s+/)[0]).filter(Boolean);
const events = [];
for (const n of names) {
  let out = "";
  try { out = execSync(`node --env-file=.env scripts/observe.mjs timeline ${n}`, { encoding: "utf8" }); } catch { continue; }
  for (const line of out.split("\n")) {
    const m = line.match(/^(\d+)\s+yielded to (\S+)/);
    if (m) events.push({ t: +m[1], from: n, to: m[2] });
  }
}
events.sort((a, b) => a.t - b.t);
const pairs = new Map();
for (const e of events) {
  const key = [e.from, e.to].sort().join(" <-> ");
  const p = pairs.get(key) ?? { ab: 0, ba: 0, ticks: [] };
  if (e.from < e.to) p.ab++; else p.ba++;
  p.ticks.push(e.t);
  pairs.set(key, p);
}
for (const [k, p] of [...pairs].sort((x, y) => (y[1].ab + y[1].ba) - (x[1].ab + x[1].ba)).slice(0, 8)) {
  const span = p.ticks.length ? `${p.ticks[0]}..${p.ticks[p.ticks.length - 1]}` : "";
  console.log(`${k}: ${p.ab}/${p.ba} yields, ${p.ticks.length} events over ${span}${p.ab > 0 && p.ba > 0 ? "  <== mutual" : ""}`);
}
console.log(`${events.length} yield events over ${names.length} creeps`);
