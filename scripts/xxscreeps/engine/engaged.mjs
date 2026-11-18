// Each engagement Fight held, its row in `engagements.jsonl` written at its
// close: what `prediction.mjs` and `measures.mjs` read its outcome from. A
// tick's objects are plain, whichever engine read them:
// { id, name, room, x, y, hits, hitsMax, type, ours, ttl (a tombstone's life left) }.
const near = (a, b, r) => a.room === b.room && Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) <= r;

// `write` takes each row; `at(rooms)` resolves to the objects of `rooms`, of every room when none is named.
export function engagements(write, at) {
  const open = new Map();
  // `dead`: its own count, or our bodies' tombstones with life left in the rooms
  // it targeted, which catch a death that closed it before it counted one.
  async function close(rec, t) {
    const graves = (await at([...rec.rooms])).filter((o) => o.type === "tombstone" && o.ours && o.ttl > 1 && rec.ours.has(o.name));
    const { members, ours, rooms, swing, ...rest } = rec;
    const swung = Object.fromEntries(Object.entries(swing).map(([n, { ticks, swings }]) => [n, { ticks, swings }]));
    const outlived = t === null || (await at()).some((o) => members.has(o.id));
    write({ ...rest, dead: Math.max(rest.dead, graves.length), swung, closed: t, outlived });
  }
  // ibex's A-B-A oscillation. A swing that struck or healed on both its steps,
  // or stepped over a room's edge (out of the towers' reach), is a dance that
  // pays. Struck is read off the event log: the engine logs no dismantle action.
  function swinging(rec, bodies, t, struck) {
    for (const o of bodies) {
      const s = (rec.swing[o.name] ??= { ticks: 0, swings: 0, last: [] });
      const now = { t, room: o.room, tile: `${o.room} ${o.x},${o.y}`, struck: struck.has(o.id) };
      const [a, b] = s.last;
      if (a?.t === t - 2 && b?.t === t - 1) {
        s.ticks++;
        if (a.tile === now.tile && b.tile !== now.tile && b.room === now.room && !(b.struck && now.struck)) s.swings++;
      }
      s.last = [b, now];
    }
  }
  return {
    // `struck`: the ids the tick's event logs show attacking or healing.
    async tick(t, fight, objects, struck) {
      const now = new Set();
      for (const e of fight?.engagements ?? []) {
        now.add(e.id);
        const rec = open.get(e.id) ?? { id: e.id, kind: e.kind ?? "defend", room: e.room, opened: t, members: new Set(), ours: new Set(), rooms: new Set(), swing: {}, answered: false, gaveUp: false, disengaged: false, contact: null, seen: null, dead: 0 };
        open.set(e.id, rec);
        for (const m of (fight.groups ?? []).find((g) => g.id === e.group)?.members ?? []) rec.members.add(m);
        rec.predicted = e.predicted;
        rec.dead = Math.max(rec.dead, e.lost);
        rec.rooms.add(e.room);
        for (const n of e.ours) rec.ours.add(n);
        rec.answered ||= !e.unanswered;
        rec.gaveUp ||= rec.answered && e.unanswered;
        rec.disengaged ||= e.phase === "disengaging";
        const theirs = objects.filter((o) => rec.members.has(o.id));
        if (theirs.length) rec.seen = t;
        rec.core ||= theirs.some((o) => o.type === "invaderCore");
        const bodies = objects.filter((o) => o.type === "creep" && o.ours && e.ours.includes(o.name));
        const close = bodies.some((o) => theirs.some((h) => near(h, o, 3)));
        if (rec.contact === null && (close || theirs.some((h) => h.hits < h.hitsMax))) rec.contact = t;
        swinging(rec, bodies, t, struck);
      }
      for (const [id, rec] of open)
        if (!now.has(id)) {
          open.delete(id);
          await close(rec, t);
        }
    },
    async end() {
      for (const rec of open.values()) await close(rec, null);
    },
  };
}
