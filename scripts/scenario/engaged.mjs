// Each engagement Fight held, its row in `engagements.jsonl` written at its
// close: what `prediction.mjs` and `measures.mjs` read its outcome from. A
// tick's objects are plain, whichever engine read them:
// { id, name, room, x, y, hits, hitsMax, type, ours, ttl (a tombstone's life left) }.
// An engagement's bodies by slot; a bundle older than slots lists the names.
const namesOf = (ours) => (Array.isArray(ours) ? ours : Object.keys(ours));
const near = (a, b, r) => a.room === b.room && Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) <= r;

// `write` takes each row; `at(rooms)` resolves to the objects of `rooms`, of every room when none is named;
// `bodyOf(name)` is a rival body's part letters (`bodies.jsonl`).
export function engagements(write, at, bodyOf = () => "") {
  const open = new Map();
  let lastCreeps = new Set();
  // `dead`: its own count, or our bodies' tombstones with life left in the rooms
  // it targeted, which catch a death that closed it before it counted one.
  async function close(rec, t) {
    const graves = (await at([...rec.rooms])).filter((o) => o.type === "tombstone" && o.ours && o.ttl > 1 && rec.ours.has(o.name));
    const { members, ours, rooms, swing, blind, asking, reached, fought, formed, empty, tombs, was, off, ...rest } = rec;
    const swung = Object.fromEntries(Object.entries(swing).map(([n, { ticks, swings }]) => [n, { ticks, swings }]));
    // `apart`: the ticks between the first and the last of its bodies coming within three of its raid.
    const ticks = Object.values(reached);
    rest.apart = ticks.length ? Math.max(...ticks) - Math.min(...ticks) : 0;
    const outlived = t === null || (await at()).some((o) => members.has(o.id));
    write({ ...rest, dead: Math.max(rest.dead, graves.length), swung, closed: t, outlived, drew: [...ours], rooms: [...rooms] });
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
    // `strikes`: [by, target id] of each attack or heal the tick's event logs show.
    async tick(t, fight, objects, strikes) {
      const now = new Set();
      const asked = new Set((fight?.casts ?? []).map((c) => c.engagement));
      const struck = new Set(strikes.map(([by]) => by));
      const creeps = new Set(objects.filter((o) => o.type === "creep").map((o) => o.id));
      // `sweep::prey`: his bodies that work, carry or claim and strike back with nothing.
      const preyIn = new Set(objects.filter((o) => o.type === "creep" && !o.ours && /[wck]/.test(bodyOf(o.name)) && !/[ar]/.test(bodyOf(o.name))).map((o) => o.room));
      for (const e of fight?.engagements ?? []) {
        now.add(e.id);
        const rec = open.get(e.id) ?? { id: e.id, kind: e.kind ?? "defend", room: e.room, opened: t, members: new Set(), ours: new Set(), rooms: new Set(), swing: {}, reached: {}, answered: false, gaveUp: false, disengaged: false, contact: null, seen: null, dead: 0, blind: 0, idle: 0, asking: 0, waited: 0, budget: 0, formed: 0, forming: 0, empty: 0, bare: 0, askedMost: 0, plinked: 0, tombs: new Set() };
        open.set(e.id, rec);
        for (const m of (fight.groups ?? []).find((g) => g.id === e.group)?.members ?? []) rec.members.add(m);
        rec.predicted = e.predicted;
        // A sweep's `lost` is its room's; the dead of the rooms it left are its log's (`leftDead`).
        rec.dead = Math.max(rec.dead, e.lost + (e.actual?.leftDead ?? 0));
        rec.rooms.add(e.room);
        const ours = namesOf(e.ours);
        for (const n of ours) rec.ours.add(n);
        rec.held = Math.max(rec.held ?? 0, ours.length);
        if (rec.ours.size) rec.drafted ??= t;
        rec.answered ||= !e.unanswered;
        rec.gaveUp ||= rec.answered && e.unanswered;
        rec.disengaged ||= e.phase === "disengaging";
        // `repriced`: the ticks a Fighting engagement re-entered Fighting, the phase a re-price keeps.
        if (e.phase === "fighting" && rec.fought !== undefined && rec.fought !== e.since) rec.repriced = (rec.repriced ?? 0) + 1;
        rec.fought = e.phase === "fighting" ? e.since : undefined;
        // `idle`: the most ticks on end a Defend neither acted (held a body, asked one, saw its group) nor stood down.
        const sighted = (fight.groups ?? []).some((g) => g.id === e.group && g.seen === t);
        rec.blind = rec.kind !== "defend" || ours.length || asked.has(e.id) || sighted || e.phase === "disengaging" ? 0 : rec.blind + 1;
        rec.idle = Math.max(rec.idle, rec.blind);
        // `waited`: the most ticks on end a Defend stood answered on casts asked, none of its bodies standing.
        rec.asking = rec.kind === "defend" && !e.unanswered && !ours.length && asked.has(e.id) ? rec.asking + 1 : 0;
        rec.waited = Math.max(rec.waited, rec.asking);
        // `budget`: the bodies its price let it lose when it last Formed, where it commits; one moved later is no commitment.
        if (e.phase === "forming") rec.budget = e.budget ?? 0;
        // `forming`: the most ticks on end it formed answered.
        rec.formed = e.phase === "forming" && !e.unanswered ? rec.formed + 1 : 0;
        rec.forming = Math.max(rec.forming, rec.formed);
        const theirs = objects.filter((o) => rec.members.has(o.id));
        if (theirs.length) rec.seen = t;
        // `beside`: the ticks a Defend that disengaged as its raid stepped out stood so with its raid back in its room.
        const raid = (fight.groups ?? []).find((g) => g.id === e.group);
        if (e.phase === "disengaging" && rec.was !== "disengaging") rec.off = raid?.room !== e.room;
        rec.was = e.phase;
        if (rec.kind === "defend" && rec.off && e.phase === "disengaging" && raid?.room === e.room) rec.beside = (rec.beside ?? 0) + 1;
        rec.core ||= theirs.some((o) => o.type === "invaderCore");
        const bodies = objects.filter((o) => o.type === "creep" && o.ours && ours.includes(o.name));
        for (const o of bodies) if (theirs.some((h) => near(h, o, 3))) rec.reached[o.name] ??= t;
        const close = bodies.some((o) => theirs.some((h) => near(h, o, 3)));
        if (rec.contact === null && (close || theirs.some((h) => h.hits < h.hitsMax))) rec.contact = t;
        swinging(rec, bodies, t, struck);
        // A sweep with all its bodies in its room: `bare`, the most ticks on end no rival creep stood there.
        if (rec.kind !== "sweep") continue;
        const held = bodies.length > 0 && bodies.every((o) => o.room === e.room);
        const rivals = held ? objects.filter((o) => o.type === "creep" && !o.ours && o.room === e.room) : [];
        rec.empty = held && !rivals.length ? rec.empty + 1 : 0;
        rec.bare = Math.max(rec.bare, rec.empty);
        // `killed`: the rival bodies that are no fight's whose tombstones stood beside it.
        if (held) for (const o of objects) if (o.type === "tombstone" && !o.ours && o.room === e.room && !o.name.startsWith("fight-")) rec.tombs.add(o.id);
        rec.killed = rec.tombs.size;
        rec.askedMost = Math.max(rec.askedMost, (fight.casts ?? []).filter((c) => c.engagement === e.id).length);
        // `plinked`: its bodies' strikes on what was no creep then or a tick before, where prey stood (`Scene::ground`'s marks).
        const roomOf = new Map(bodies.map((o) => [o.id, o.room]));
        rec.plinked += strikes.filter(([by, hit]) => roomOf.has(by) && hit && !creeps.has(hit) && !lastCreeps.has(hit) && preyIn.has(roomOf.get(by))).length;
      }
      lastCreeps = creeps;
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
