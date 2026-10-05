// The two traces side by side (#543): an arena run's (`Arena.crossValidate`)
// and an xxscreeps run's (`engine/step.mjs`), both one JSON row per tick,
// `{ t, time, bodies: [{ id, room, x, y, hits }], acts }`, t0 the state after
// the first step. Pure: no I/O but the reading, so `cli-smoke.mjs` can run it.
import { readFileSync } from "node:fs";

export const readTrace = (path) =>
  readFileSync(path, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((line) => JSON.parse(line));

/// The arena's `describe` line: `t3 [id@Room:x,y/hits ...]`, a row's acts after.
export const describe = (rows) =>
  rows
    .map((r) => `t${r.t} [${r.bodies.map((b) => `${b.id}@${b.room}:${b.x},${b.y}/${b.hits}`).join(" ")}] ${(r.acts ?? []).join("; ")}`)
    .join("\n");

const chebyshev = (a, b) => (a.room === b.room ? Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) : Infinity);
const where = (b) => (b ? `${b.room}:${b.x},${b.y}/${b.hits}` : "dead");

/// The tick each body is first missing on, or undefined while it stands.
export const deaths = (rows) => {
  const seen = new Map();
  for (const r of rows) {
    const here = new Set(r.bodies.map((b) => b.id));
    for (const [id, died] of seen) if (died === undefined && !here.has(id)) seen.set(id, r.t);
    for (const id of here) if (!seen.has(id)) seen.set(id, undefined);
  }
  return seen;
};

/// The per-tick diff as text: a death table, each body's agreement, and
/// every tick on which some body stands elsewhere or at other hits.
export function diff(arena, xx, { label = ["arena", "xxscreeps"] } = {}) {
  const lines = [];
  const n = Math.min(arena.length, xx.length);
  const da = deaths(arena);
  const dx = deaths(xx);
  const ids = [...new Set([...da.keys(), ...dx.keys()])];
  const width = Math.max(...ids.map((id) => id.length), 4);
  const minHits = (rows, id) => Math.min(...rows.flatMap((r) => r.bodies.filter((b) => b.id === id).map((b) => b.hits)));
  lines.push(`ticks compared: ${n} (${label[0]} ${arena.length}, ${label[1]} ${xx.length})`);
  lines.push("");
  lines.push(`${"body".padEnd(width)}  ${`died (${label[0]})`.padStart(14)}  ${`died (${label[1]})`.padStart(18)}  ${"min hits".padStart(13)}  same tile  mean dist  first apart`);
  for (const id of ids) {
    let same = 0;
    let dist = 0;
    let both = 0;
    let firstApart;
    for (let t = 0; t < n; t++) {
      const a = arena[t].bodies.find((b) => b.id === id);
      const x = xx[t].bodies.find((b) => b.id === id);
      if (!a || !x) continue;
      both++;
      const d = chebyshev(a, x);
      if (d === 0) same++;
      else if (firstApart === undefined) firstApart = t;
      if (Number.isFinite(d)) dist += d;
    }
    const hits = `${minHits(arena, id)}/${minHits(xx, id)}`;
    lines.push(
      `${id.padEnd(width)}  ${String(da.get(id) ?? "-").padStart(14)}  ${String(dx.get(id) ?? "-").padStart(18)}  ${hits.padStart(13)}  ${`${same}/${both}`.padStart(9)}  ${(both ? dist / both : 0).toFixed(1).padStart(9)}  ${firstApart === undefined ? "-" : `t${firstApart}`}`,
    );
  }
  lines.push("");
  lines.push(`per tick, the bodies that differ (${label[0]} | ${label[1]}):`);
  for (let t = 0; t < n; t++) {
    const parts = [];
    for (const id of ids) {
      const a = arena[t].bodies.find((b) => b.id === id);
      const x = xx[t].bodies.find((b) => b.id === id);
      if (!a && !x) continue;
      if (a && x && chebyshev(a, x) === 0 && a.hits === x.hits) continue;
      parts.push(`${id} ${where(a)} | ${where(x)}`);
    }
    if (parts.length > 0) lines.push(`t${t}  ${parts.join("  ;  ")}`);
  }
  return lines.join("\n");
}
