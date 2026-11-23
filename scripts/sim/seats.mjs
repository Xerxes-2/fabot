// Each seat of a finished `sim --players` run side by side, off what sim wrote
// for it (`<out>` the first, `<out>/<name>` the rest): written to `<out>/seats.json`.
//
// Usage: node scripts/sim/seats.mjs <out>
import { existsSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { samplesOf } from "../scenario/measures.mjs";
import { engagement } from "./prediction.mjs";

const lines = (path) => (existsSync(path) ? readFileSync(path, "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l)) : []);
const sum = (xs) => xs.reduce((a, b) => a + b, 0);
const tally = (keys) => Object.fromEntries([...new Set(keys)].sort().map((k) => [k, keys.filter((x) => x === k).length]));
// A body that can strike or heal is a fight body (lives.rs's letters).
const fights = (body) => /[arh]/.test(body);

/** The seats' directories, the first `out`. */
const seatDirs = (out) => [out, ...readdirSync(out, { withFileTypes: true }).filter((d) => d.isDirectory() && existsSync(join(out, d.name, "seat.json"))).map((d) => join(out, d.name))];

export function seats(out) {
  // sim numbers its seats' users u1, u3, …: their order is the seats'.
  const all = seatDirs(out).map((dir) => ({ dir, ...JSON.parse(readFileSync(join(dir, "seat.json"), "utf8")) })).sort((a, b) => Number(a.uid.slice(1)) - Number(b.uid.slice(1)));
  const names = Object.fromEntries(all.map((s) => [s.uid, s.name]));
  const who = (by) => (by ? `${by[0] === "tower" ? "tower " : ""}${names[by[1]] ?? { 2: "Invader", 3: "Keeper" }[by[1]] ?? by[1]}` : "unseen");
  // Each seat sees deaths only around its own rooms: one seen by several is one death.
  const seen = new Map();
  for (const d of all.flatMap((s) => s.deaths)) if (!seen.get(`${d.t} ${d.user} ${d.name}`)?.by) seen.set(`${d.t} ${d.user} ${d.name}`, d);
  const deaths = [...seen.values()];
  return all.map(({ dir, uid, name, room, structuresLost }) => {
    const last = samplesOf(dir).at(-1);
    const rooms = Object.entries(last.rooms);
    const outposts = rooms.filter(([r, l]) => r !== room && l.reserved > 0);
    const bodies = lines(join(dir, "bodies.jsonl")).filter((b) => b.ours);
    const engaged = lines(join(dir, "engagements.jsonl")).map(engagement);
    const lost = deaths.filter((d) => d.user === uid);
    const killed = deaths.filter((d) => d.user !== uid && d.by?.[1] === uid);
    return {
      name, room, t: last.t, rcl: last.rcl, progress: last.progress, controller: last.worth.controller,
      levels: Object.fromEntries(JSON.parse(readFileSync(join(dir, "levels.json"), "utf8")).filter((l) => l.rcl > 0).map((l) => [l.rcl, l.t])),
      dugHome: sum(rooms.filter(([r]) => r === room).map(([, l]) => l.dug)), dugOutposts: sum(outposts.map(([, l]) => l.dug)),
      outposts: outposts.length, outpostTicks: sum(outposts.map(([, l]) => l.reserved)), outpostHostileTicks: sum(outposts.map(([, l]) => l.hostile)),
      creeps: last.creeps, fightSpend: sum(bodies.filter((b) => fights(b.body)).map((b) => b.cost)), spawnSpend: last.ledger.spawn,
      lost: lost.length, lostBy: tally(lost.map((d) => who(d.by))), killed: killed.length, killedOf: tally(killed.map((d) => who([null, d.user]))),
      engagements: engaged.length, won: engaged.filter((e) => e.won === true).length, lostEngagements: engaged.filter((e) => e.won === false).length,
      structuresLost,
    };
  });
}

const ROWS = [
  ["room", (s) => s.room], ["rcl (progress)", (s) => `${s.rcl} (${s.progress})`], ["controller total", (s) => s.controller],
  ["tick of rcl 2..8", (s) => [2, 3, 4, 5, 6, 7, 8].map((l) => s.levels[l] ?? "-").join(" ")],
  ["dug home / outposts", (s) => `${s.dugHome} / ${s.dugOutposts}`], ["outposts ever reserved (held ticks)", (s) => `${s.outposts} (${s.outpostTicks})`],
  ["hostile ticks in outposts", (s) => s.outpostHostileTicks], ["creeps at end", (s) => s.creeps], ["spawn / fight spend", (s) => `${s.spawnSpend} / ${s.fightSpend}`],
  ["bodies lost", (s) => `${s.lost} ${JSON.stringify(s.lostBy)}`], ["bodies killed", (s) => `${s.killed} ${JSON.stringify(s.killedOf)}`],
  ["engagements opened / won / lost", (s) => `${s.engagements} / ${s.won} / ${s.lostEngagements}`], ["structures lost", (s) => s.structuresLost],
];

/** The table, a row a measure and a column a seat. */
export function table(rows) {
  const cells = [["", ...rows.map((s) => `${s.name} t${s.t}`)], ...ROWS.map(([k, f]) => [k, ...rows.map((s) => String(f(s)))])];
  const width = cells[0].map((_, i) => Math.max(...cells.map((r) => r[i].length)));
  return cells.map((r) => r.map((c, i) => c.padEnd(width[i])).join("  ").trimEnd()).join("\n");
}

export function report(out) {
  if (!existsSync(join(out, "seat.json"))) return `${out}: no seat.json, the run did not finish`;
  const rows = seats(out);
  writeFileSync(join(out, "seats.json"), JSON.stringify(rows, null, 1) + "\n");
  return table(rows);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  if (!process.argv[2]) throw new Error("usage: node scripts/sim/seats.mjs <out>");
  console.log(report(process.argv[2]));
}
