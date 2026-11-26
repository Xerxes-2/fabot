// The server's per-tick room history, replayed from its 100-tick diffs into one
// record per tick, and the measures `observe battle` reads off it. A measure is
// `(ticks, args) -> result` over those records and nothing else, so a scenario
// can read a report's numbers off its own bodies (ARCHITECTURE.md §10.2).
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { roomHistory } from "./screeps-api.mjs";

const CHUNK = 100;
const CACHE = new URL("../captures/history/", import.meta.url);
// A chunk the server may still be writing is fetched again: a half-written one
// holds half a fight.
const SETTLED = 1000;

/// `engine::tower_attack_at`.
const towerAt = (r) => (r > 20 ? 0 : r <= 5 ? 600 : 600 - 30 * (r - 5));
/// Ranged attack power per part at range 1, and ranged mass attack at range 2.
const RANGED = 10;
const MASS = 4;
const range = (a, b) => Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y));

const LETTER = { ranged_attack: "r", work: "w", heal: "h", move: "m", carry: "c", attack: "a", tough: "t", claim: "k" };
const short = (body) => {
  const parts = {};
  for (const p of body ?? []) if (p) parts[LETTER[p.type] ?? p.type[0]] = (parts[LETTER[p.type] ?? p.type[0]] ?? 0) + 1;
  return Object.entries(parts).map(([k, n]) => `${k}${n}`).join("");
};
const ACTS = ["dismantle", "attack", "rangedAttack", "rangedMassAttack", "rangedHeal", "heal"];
const acted = (log) => {
  for (const kind of ACTS) {
    const v = log?.[kind];
    if (v) return { kind, x: v.x, y: v.y };
  }
  return null;
};
const patch = (old, v, id) => {
  const o = { ...(old ?? {}), ...v, _id: id };
  if (v.body && !Array.isArray(v.body)) {
    const parts = Array.isArray(old?.body) ? old.body.slice() : [];
    for (const [i, part] of Object.entries(v.body)) parts[Number(i)] = part;
    o.body = parts;
  }
  if (v.store && old?.store) o.store = { ...old.store, ...v.store };
  return o;
};

/// The window's ticks, plus the one before `from` so the first tick has a delta.
export async function history({ url, token, shard, room, from, to, now }) {
  const objects = new Map();
  const ticks = [];
  for (let base = Math.floor((from - 1) / CHUNK) * CHUNK; base <= to; base += CHUNK) {
    const file = new URL(`${room}_${base}.json`, CACHE);
    const settled = base + CHUNK <= now - SETTLED;
    const kept = settled && existsSync(file) ? readFileSync(file, "utf8") : null;
    const text = kept ?? (await roomHistory({ url, token, shard, room, tick: base }));
    if (text === null) {
      throw new Error(`room history: the server holds no ${room} ${base}.json — its newest file lags the tick by ` +
        `~50–150, so a fight is only replayable that long after it ran`);
    }
    if (!kept) {
      mkdirSync(CACHE, { recursive: true });
      if (settled) writeFileSync(file, text);
    }
    const data = JSON.parse(text);
    for (const t of Object.keys(data.ticks).map(Number).sort((a, b) => a - b)) {
      const rec = { t, creeps: {}, towers: {}, ramparts: {}, died: [] };
      for (const [id, v] of Object.entries(data.ticks[String(t)])) {
        const old = objects.get(id);
        if (v === null) {
          objects.delete(id);
          if (old?.type === "creep") rec.died.push(old.name);
          continue;
        }
        const o = patch(old, v, id);
        if (v.actionLog !== undefined) o.logT = t;
        if (o.type === "rampart" && old?.hits !== undefined && o.hits < old.hits) {
          // The engine decays a rampart on the tick its own timer names, and a
          // strike can hide behind that drop.
          rec.ramparts[`${o.x},${o.y}`] = { drop: old.hits - o.hits, decay: old.nextDecayTime === t + 1 };
        }
        objects.set(id, o);
      }
      for (const o of objects.values()) {
        if (o.type === "creep" && o.name) {
          const fresh = o.logT === t;
          rec.creeps[o.name] = { u: o.user, x: o.x, y: o.y, hits: o.hits, hitsMax: o.hitsMax, body: short(o.body),
            acted: fresh ? acted(o.actionLog) : null, struck: fresh ? o.actionLog.attacked ?? null : null };
        } else if (o.type === "tower") rec.towers[o._id] = { x: o.x, y: o.y, e: o.store?.energy, log: o.logT === t ? o.actionLog : null };
      }
      ticks.push(rec);
    }
  }
  return { room, from, to, ticks: ticks.filter((r) => r.t >= from - 1 && r.t <= to) };
}

// ------------------------------------------------------------------ measures

/// Each tower's shots this tick: a shot costs 10 energy.
function shots(prev, rec) {
  const out = [];
  for (const [id, tw] of Object.entries(rec.towers)) {
    const e = prev.towers[id]?.e;
    const n = typeof e === "number" && typeof tw.e === "number" ? Math.floor((e - tw.e) / 10) : 0;
    if (n > 0) out.push({ id, x: tw.x, y: tw.y, n });
  }
  return out;
}

const OURS = (rec, ours) => Object.entries(rec.creeps).filter(([, c]) => c.u === ours);
const medics = (rec, ours) => OURS(rec, ours).filter(([, c]) => /h\d/.test(c.body) && !/[wra]\d/.test(c.body));
const dismantlers = (rec, ours) => OURS(rec, ours).filter(([, c]) => /w\d/.test(c.body) && !/h\d/.test(c.body));
const armed = (rec) => Object.entries(rec.creeps).filter(([, c]) => /[ar]\d/.test(c.body));

/// One body's tick: hits lost and healed, and the damage every tower that fired
/// could deal to the tile the body started the tick on.
function flow(prev, rec, name) {
  const p = prev.creeps[name], c = rec.creeps[name];
  if (!p) return { took: 0, healed: 0, tower: 0 };
  const fired = shots(prev, rec);
  return { took: Math.max(0, p.hits - c.hits), healed: Math.max(0, c.hits - p.hits),
    tower: fired.reduce((sum, t) => sum + towerAt(range(t, p)), 0), struck: c.struck };
}

/// Every body in the window: its blood, a medic's reach to it, and how it ended.
export function bodies({ ticks, ours }) {
  const out = new Map();
  for (const [i, rec] of ticks.entries()) {
    const prev = ticks[i - 1];
    for (const [name, c] of Object.entries(rec.creeps)) {
      const b = out.get(name) ?? { name, u: c.u, body: c.body, in: rec.t, out: rec.t, ticks: 0, damage: 0, heal: 0,
        tower: 0, by: {}, medic: {}, last: [] };
      out.set(name, b);
      b.out = rec.t;
      b.body = c.body;
      if (!prev?.creeps[name]) continue;
      const f = flow(prev, rec, name);
      // A tower only explains a tick it aimed at: more than it could deal is a
      // body's blows, less is a shot that went elsewhere.
      const tower = f.took === 0 ? 0 : f.took === f.tower ? f.took : Math.min(f.took, f.tower);
      b.ticks++;
      b.damage += f.took;
      b.heal += f.healed;
      b.tower += tower;
      const rest = f.took - tower;
      const at = rest > 0 && c.struck;
      const foe = at && [rec, prev].map((r) => Object.entries(r.creeps)
        .find(([n, o]) => n !== name && o.x === at.x && o.y === at.y)?.[0]).find(Boolean);
      if (rest > 0) b.by[foe ?? "unattributed"] = (b.by[foe ?? "unattributed"] ?? 0) + rest;
      const near = c.u === ours ? medics(rec, ours) : [];
      const d = near.length ? Math.min(...near.map(([, m]) => range(c, m))) : null;
      b.last.push({ t: rec.t, took: f.took, healed: f.healed, tower, foe: rest > 0 ? foe ?? "unattributed" : null, medic: d });
      if (b.last.length > 30) b.last.shift();
      if (d !== null) b.medic[d] = (b.medic[d] ?? 0) + 1;
    }
    for (const name of rec.died) {
      const b = out.get(name);
      if (b) b.died = rec.t;
    }
  }
  return out;
}

/// Each tower's shots and what they hit. The target is the tile its actionLog
/// names, else the one body whose damage this tick is exactly some of the firing
/// towers' power at its tile — a tower fires once, so an exact cover answers.
export function towers({ ticks, ours }) {
  const out = new Map();
  for (const [i, rec] of ticks.entries()) {
    const prev = ticks[i - 1];
    if (!prev) continue;
    const fired = shots(prev, rec);
    if (!fired.length) continue;
    const hurt = OURS(rec, ours).map(([name]) => ({ name, ...flow(prev, rec, name) }))
      .filter((f) => f.took > 0).map((f) => ({ ...f, x: prev.creeps[f.name].x, y: prev.creeps[f.name].y }));
    const named = new Map();
    const spent = new Set();
    for (const f of hurt.slice().sort((a, b) => b.took - a.took)) {
      const open = fired.filter((t) => !spent.has(t.id)).map((t) => ({ id: t.id, v: towerAt(range(t, f)) })).filter((t) => t.v > 0);
      const one = open.filter((t) => t.v === f.took);
      const pair = open.flatMap((a, j) => open.slice(j + 1).map((b) => [a, b])).find(([a, b]) => a.v + b.v === f.took);
      // The one body hurt this tick took every shot that hit nothing else: a
      // heal in the same tick hides the rest of its damage.
      const only = hurt.length === 1 ? open : [];
      for (const t of one.length === 1 ? one : pair ?? only) (spent.add(t.id), named.set(t.id, f.name));
    }
    for (const t of fired) {
      const tw = out.get(t.id) ?? { id: t.id, x: t.x, y: t.y, shots: 0, first: null, dry: null, curve: [], targets: {} };
      out.set(t.id, tw);
      tw.shots += t.n;
      tw.first ??= rec.t;
      tw.curve.push([rec.t, rec.towers[t.id].e]);
      if (rec.towers[t.id].e === 0 && tw.dry === null) tw.dry = rec.t;
      const logged = rec.towers[t.id].log?.attack;
      const at = logged && Object.entries(prev.creeps).find(([, c]) => c.x === logged.x && c.y === logged.y)?.[0];
      const target = at ?? named.get(t.id) ?? "unattributed";
      tw.targets[target] = (tw.targets[target] ?? 0) + 1;
    }
  }
  return out;
}

/// Dismantlers' distance to the nearest medic in the buckets a ranged heal
/// cares about (it reaches 3), and how far apart our bodies stood.
export function formation({ ticks, ours }) {
  const out = { bodies: new Map(), spread: { ticks: 0, apart: 0, most: 0 } };
  for (const rec of ticks) {
    const near = medics(rec, ours);
    for (const [name, c] of dismantlers(rec, ours)) {
      const b = out.bodies.get(name) ?? { name, ticks: 0, one: 0, reach: 0, far: 0 };
      out.bodies.set(name, b);
      b.ticks++;
      if (!near.length) b.far++;
      else {
        const d = Math.min(...near.map(([, m]) => range(c, m)));
        if (d <= 1) b.one++;
        else if (d <= 3) b.reach++;
        else b.far++;
      }
    }
    const mine = OURS(rec, ours);
    if (mine.length < 2) continue;
    const spread = Math.max(...mine.flatMap(([, a]) => mine.map(([, b]) => range(a, b))));
    out.spread.ticks++;
    out.spread.most = Math.max(out.spread.most, spread);
    if (spread > 6) out.spread.apart++;
  }
  return out;
}

/// What our fire cost him: the hits his ramparts absorbed (a wall takes the
/// hit in a body's place) against the hits his bodies took, split by the stripe
/// a body's ranged parts land.
export function strikes({ ticks, ours }) {
  const parts = (rec) => Math.max(0, ...OURS(rec, ours).filter(([, c]) => /r\d/.test(c.body))
    .map(([, c]) => Number(c.body.match(/r(\d+)/)[1])));
  const full = RANGED * Math.max(0, ...ticks.map(parts));
  const mass = (MASS * full) / RANGED;
  const out = { full, ramparts: { total: 0, decay: 0, stripe: 0, mass: 0, other: 0 }, bodies: { armed: 0, working: 0 } };
  for (const [i, rec] of ticks.entries()) {
    for (const drop of Object.values(rec.ramparts)) {
      out.ramparts.total += drop.drop;
      if (drop.decay) out.ramparts.decay += drop.drop;
      else if (drop.drop === full || drop.drop === 2 * full) out.ramparts.stripe += drop.drop;
      else if (drop.drop === mass || drop.drop === full + mass) out.ramparts.mass += drop.drop;
      else out.ramparts.other += drop.drop;
    }
    const prev = ticks[i - 1];
    if (!prev) continue;
    for (const [name, c] of Object.entries(rec.creeps)) {
      if (c.u === ours) continue;
      const f = flow(prev, rec, name);
      if (f.took > 0) out.bodies[/[ar]\d/.test(c.body) ? "armed" : "working"] += f.took;
    }
  }
  return out;
}

/// His garrison: the bodies that could hit back, where they stood, which of ours
/// stood closest, and what they swung at.
export function defenders({ ticks, ours }) {
  const out = new Map();
  for (const rec of ticks) {
    for (const [name, c] of armed(rec)) {
      if (c.u === ours) continue;
      const d = out.get(name) ?? { name, body: c.body, ticks: 0, tiles: {}, swings: [], nearest: [], closest: {}, in: rec.t, out: rec.t, hits: 0, hitsMax: 0 };
      out.set(name, d);
      d.ticks++;
      d.out = rec.t;
      d.hits = c.hits;
      d.hitsMax = c.hitsMax;
      d.tiles[`${c.x},${c.y}`] = (d.tiles[`${c.x},${c.y}`] ?? 0) + 1;
      if (c.acted && /attack/.test(c.acted.kind)) d.swings.push({ t: rec.t, x: c.acted.x, y: c.acted.y });
      const near = OURS(rec, ours).map(([n, o]) => ({ n, d: range(c, o) })).sort((a, b) => a.d - b.d);
      if (near.length) {
        d.nearest.push(near[0].d);
        d.closest[near[0].n] = (d.closest[near[0].n] ?? 0) + 1;
      }
    }
  }
  return out;
}
