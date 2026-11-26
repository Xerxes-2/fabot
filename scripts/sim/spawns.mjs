// Spawn time a soak used, per colony and window: the spawn-ticks it had and
// spent (the bot's `spawn.*` metrics), the spent ones split by the role cast
// (its name's prefix, off `bodies.jsonl`), and the idle ones by why its
// Economy queue cast nothing (`blocked.bank`, `blocked.fight`, `spawn.idleWant`). Then, by the
// colony's level, the median and p90 use, each role's share, and the windows
// at or past --full: which colonies, what filled them, and what waited.
//
// Usage: npm run soak:spawns -- <run dir>... [--window 1500] [--full 0.9] [--json out.json]
//   A run dir's seats under it (`<dir>/<name>/`) are read beside it. "mineral?" is
//   a guess off a harvester's digging: its name names no mineral.
//   The idle split counts colony-ticks, so with several spawns it is a floor;
//   a Fight ask the bank cannot pay shows only as an idle tick.
import { existsSync, readdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { METRIC } from "../metrics.mjs";
import { SPAWN_TIME_PER_PART } from "../scenario/measures.mjs";
import { jsonLines } from "./run.mjs";

const args = process.argv.slice(2);
const flag = (name, dflt) => (args.includes(name) ? args.splice(args.indexOf(name), 2)[1] : dflt);
const WINDOW = Number(flag("--window", 1500));
const FULL = Number(flag("--full", 0.9));
const out = flag("--json");
if (!args.length) throw new Error("usage: npm run soak:spawns -- <run dir>... [--window 1500] [--full 0.9] [--json out.json]");

// A mineral is dug one tick in EXTRACTOR_COOLDOWN + 1 at most, in its colony; a source's harvester digs most ticks.
const MINES_BELOW = 0.3;
// core/economy/bodies.rs's Pattern, and Fight's prefix.
const ROLES = new Set(["harvester", "hauler", "worker", "upgrader", "claimer", "scout", "dismantler", "fight"]);

function role(life) {
  const prefix = life.name.split("-")[0];
  if (prefix === "harvester") {
    const ticks = Object.values(life.rooms).reduce((a, b) => a + b, 0);
    const dug = life.acts.harvest ?? 0;
    const home = (life.tiles?.harvest ?? []).every((t) => t.startsWith(`${life.entered[0][0]} `));
    return dug > 0 && home && dug / ticks < MINES_BELOW ? "mineral?" : "harvester";
  }
  return ROLES.has(prefix) ? prefix : "other";
}

/** Each of our bodies' cast: its spawn's room, role, and the spawn-ticks it held. */
function casts(dir) {
  const first = new Map();
  for (const l of jsonLines(join(dir, "bodies.jsonl")).filter((l) => l.ours)) {
    const had = first.get(l.name);
    if (!had) first.set(l.name, { ...l, acts: { ...l.acts }, rooms: { ...l.rooms }, tiles: { harvest: [...(l.tiles?.harvest ?? [])] } });
    else {
      for (const [k, n] of Object.entries(l.acts)) had.acts[k] = (had.acts[k] ?? 0) + n;
      for (const [k, n] of Object.entries(l.rooms)) had.rooms[k] = (had.rooms[k] ?? 0) + n;
      had.tiles.harvest.push(...(l.tiles?.harvest ?? []));
      if (l.first < had.first) Object.assign(had, { first: l.first, entered: l.entered });
    }
  }
  return [...first.values()].map((l) => ({ room: l.entered[0][0], role: role(l), from: Number(l.name.split("-")[1]), ticks: SPAWN_TIME_PER_PART * l.body.length }));
}

function colonyWindows(dir, seat) {
  const metrics = jsonLines(join(dir, "metrics.jsonl"));
  const samples = jsonLines(join(dir, "samples.jsonl"));
  const rows = new Map();
  const row = (room, start) => {
    const key = `${room} ${start}`;
    if (!rows.has(key)) rows.set(key, { seat, room, start, ticks: 0, spawnTicks: 0, busy: 0, bank: 0, owedFull: 0, fightHeld: 0, onSpawns: 0, byRole: {}, short: {} });
    return rows.get(key);
  };
  const seen = new Set();
  for (const m of metrics) {
    const v = m.values;
    const spawnTicks = v[METRIC.SPAWN_TICKS];
    if (!spawnTicks || seen.has(`${m.row} ${m.start}`)) continue;
    seen.add(`${m.row} ${m.start}`);
    const r = row(m.row, m.start - (m.start % WINDOW));
    r.ticks += m.ticks;
    r.spawnTicks += spawnTicks;
    // A share of the window's spawn-ticks, in percent.
    r.busy += ((v[METRIC.SPAWNING] ?? 0) / 100) * spawnTicks;
    r.bank += v[`${METRIC.BLOCKED}bank`] ?? 0;
    r.owedFull += v[METRIC.IDLE_WANT] ?? 0;
    r.fightHeld += v[`${METRIC.BLOCKED}fight`] ?? 0;
    r.onSpawns += v[`${METRIC.BLOCKED}spawns`] ?? 0;
    const short = METRIC.SHORT;
    for (const [k, n] of Object.entries(v)) if (k.startsWith(short)) r.short[k.slice(short.length)] = (r.short[k.slice(short.length)] ?? 0) + n * m.ticks;
  }
  for (const c of casts(dir)) {
    for (let t = c.from; t < c.from + c.ticks; ) {
      const start = t - (t % WINDOW);
      const end = Math.min(c.from + c.ticks, start + WINDOW);
      const r = rows.get(`${c.room} ${start}`);
      if (r) r.byRole[c.role] = (r.byRole[c.role] ?? 0) + end - t;
      t = end;
    }
  }
  const at = samples.map((s) => s.time);
  const level = (room, time) => {
    let i = at.findLastIndex((t) => t <= time);
    const s = samples[Math.max(i, 0)];
    return s?.homes?.[room]?.rcl ?? s?.claimed?.[room]?.level ?? s?.rcl;
  };
  return [...rows.values()].map((r) => {
    const idle = r.spawnTicks - r.busy;
    const starved = Math.min(idle, r.bank);
    const fightHeld = Math.min(idle - starved, r.fightHeld);
    const owedFull = Math.min(idle - starved - fightHeld, r.owedFull);
    for (const k in r.short) r.short[k] = Math.round((10 * r.short[k]) / r.ticks) / 10;
    return { ...r, rcl: level(r.room, r.start + WINDOW - 1), util: r.busy / r.spawnTicks, idle, starved, fightHeld, owedFull, unasked: idle - starved - fightHeld - owedFull };
  });
}

function engagementsIn(dir) {
  return jsonLines(join(dir, "engagements.jsonl")).filter((e) => e.kind === "defend");
}

const quantile = (xs, q) => {
  const s = [...xs].sort((a, b) => a - b);
  return s.length ? s[Math.min(s.length - 1, Math.floor(q * s.length))] : NaN;
};
const pct = (x) => `${Math.round(100 * x)}%`;
const shares = (rows) => {
  const by = {};
  for (const r of rows) for (const [k, n] of Object.entries(r.byRole)) by[k] = (by[k] ?? 0) + n;
  const total = Object.values(by).reduce((a, b) => a + b, 0);
  return Object.entries(by).sort((a, b) => b[1] - a[1]).map(([k, n]) => `${k} ${pct(n / total)}`).join(", ");
};

const windows = [];
const defends = [];
for (const dir of args) {
  const seats = [dir, ...readdirSync(dir, { withFileTypes: true }).filter((d) => d.isDirectory() && existsSync(join(dir, d.name, "metrics.jsonl"))).map((d) => join(dir, d.name))];
  for (const s of seats) {
    const ws = colonyWindows(s, s);
    windows.push(...ws.filter((w) => w.spawnTicks >= WINDOW / 2));
    for (const e of engagementsIn(s)) defends.push({ ...e, seat: s, full: ws.some((w) => w.util >= FULL && e.opened >= w.start && e.opened < w.start + WINDOW) });
  }
}

console.log(`colony windows of ${WINDOW} ticks, full at ${pct(FULL)} busy`);
console.log("rcl  windows  util p50  p90  full  idle: starved fightHeld owedFull unasked  busy by role");
for (const rcl of [1, 2, 3, 4, 5, 6, 7, 8]) {
  const at = windows.filter((w) => w.rcl === rcl);
  if (!at.length) continue;
  const idle = at.reduce((a, w) => a + w.idle, 0);
  const of = (k) => pct(at.reduce((a, w) => a + w[k], 0) / Math.max(idle, 1));
  const util = at.map((w) => w.util);
  console.log(`${rcl}    ${String(at.length).padStart(6)}  ${pct(quantile(util, 0.5)).padStart(8)} ${pct(quantile(util, 0.9)).padStart(4)}  ${String(at.filter((w) => w.util >= FULL).length).padStart(4)}  ${of("starved").padStart(13)} ${of("fightHeld").padStart(9)} ${of("owedFull").padStart(8)} ${of("unasked").padStart(7)}  ${shares(at)}`);
}
const full = windows.filter((w) => w.util >= FULL);
const busyRest = windows.filter((w) => w.util < FULL);
const perWindow = (rows, k) => rows.reduce((a, w) => a + (typeof w[k] === "number" ? w[k] : Object.values(w[k]).reduce((x, y) => x + y, 0)), 0) / Math.max(rows.length, 1);
console.log(`\nfull windows: ${full.length} of ${windows.length}`);
for (const w of full) console.log(`  ${w.seat.split("/").slice(-1)[0]} ${w.room} t${w.start} rcl${w.rcl} ${pct(w.util)} ${shares([w])} | waited on spawns ${w.onSpawns} mean slots uncast ${JSON.stringify(w.short)}`);
console.log(`per window, full vs not: queue waited on spawns ${perWindow(full, "onSpawns").toFixed(0)} vs ${perWindow(busyRest, "onSpawns").toFixed(0)} ticks; mean open slots uncast ${perWindow(full, "short").toFixed(1)} vs ${perWindow(busyRest, "short").toFixed(1)}`);
const waited = (es) => `${es.length} opened, waited p50 ${quantile(es.map((e) => e.waited), 0.5) ?? "-"} p90 ${quantile(es.map((e) => e.waited), 0.9) ?? "-"} forming p50 ${quantile(es.map((e) => e.forming), 0.5) ?? "-"}`;
console.log(`defends in a full window: ${waited(defends.filter((e) => e.full))}; elsewhere: ${waited(defends.filter((e) => !e.full))}`);
if (out) writeFileSync(out, JSON.stringify({ windows, defends: defends.map(({ seat, room, opened, waited, forming, full }) => ({ seat, room, opened, waited, forming, full })) }));
