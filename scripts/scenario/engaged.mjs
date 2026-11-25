// Each engagement Fight held, its row in `engagements.jsonl` written at its
// close: what `prediction.mjs` and `measures.mjs` read its outcome from. A
// tick's objects are plain, whichever engine read them:
// { id, name, room, x, y, type, ours, spent }.
// An engagement's bodies by slot; a bundle older than slots lists the names.
const namesOf = (ours) => (Array.isArray(ours) ? ours : Object.keys(ours));
const near = (a, b, r) => a.room === b.room && Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) <= r;

// `OUTPOST_LOSSES` in `core/fight/kinds/defend.rs`: an outpost's Defend loses this past its budget before it quits.
export const OUTPOST_LOSSES = 2;

// `write` takes each row; `bodyOf(name)` is a rival body's part letters (`bodies.jsonl`); `offset`, the
// bot's game tick less the run's; `closes`, the bot's `engagements.log` rows (segment 4) by `${id} ${closed}`;
// `killedAt(t)`, the rival bodies our seat killed at run tick `t` (seat.json's `deaths`).
export function engagements(write, bodyOf, offset, closes, killedAt) {
  const open = new Map();
  let lastCreeps = new Set();
  const run = (time) => (time == null ? null : time - offset);
  // `merged`: `run::merged` folded it into an older open Defend on its raid (the older one's group holds
  // its raid's bodies, or its `ours` its bodies), so it did not close and writes no row.
  function close(rec, t, merged) {
    const row = t === null ? undefined : closes.get(`${rec.id} ${t + offset}`);
    if (t !== null && !row && merged) return;
    if (t !== null && !row) throw new Error(`engagement ${rec.id} closed at t${t} with no engagements.log row`);
    if (row) Object.assign(rec, { contact: run(row.contact), seen: run(row.seen), gaveUp: Boolean(row.gaveUp), dead: row.dead, left: Boolean(row.left) });
    const { members, ours, standing, raid, rooms, swing, blind, asking, fought, formed, empty, kills, was, off, formedSince, formingSet, committed, ...rest } = rec;
    const swung = Object.fromEntries(Object.entries(swing).map(([n, { ticks, swings }]) => [n, { ticks, swings }]));
    write({ ...rest, swung, closed: t, drew: [...ours], rooms: [...rooms] });
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
    tick(t, fight, objects, strikes) {
      const now = new Set();
      const asked = new Set((fight?.casts ?? []).map((c) => c.engagement));
      const struck = new Set(strikes.map(([by]) => by));
      const creeps = new Set(objects.filter((o) => o.type === "creep").map((o) => o.id));
      // `sweep::prey`: his bodies that work, carry or claim and strike back with nothing.
      const preyIn = new Set(objects.filter((o) => o.type === "creep" && !o.ours && /[wck]/.test(bodyOf(o.name)) && !/[ar]/.test(bodyOf(o.name))).map((o) => o.room));
      for (const e of fight?.engagements ?? []) {
        now.add(e.id);
        const rec = open.get(e.id) ?? { id: e.id, kind: e.kind ?? "defend", room: e.room, opened: t, members: new Set(), ours: new Set(), rooms: new Set(), swing: {}, answered: false, disengaged: false, blind: 0, idle: 0, asking: 0, waited: 0, budget: 0, formed: 0, forming: 0, empty: 0, bare: 0, askedMost: 0, plinked: 0, kills: new Set() };
        open.set(e.id, rec);
        // Its raid's bodies and its own this tick: what `run::merged` hands the older Defend.
        const raiders = (fight.groups ?? []).find((g) => g.id === e.group)?.members ?? [];
        for (const m of raiders) rec.members.add(m);
        [rec.predicted, rec.group, rec.standing, rec.raid] = [e.predicted, e.group, namesOf(e.ours), raiders];
        // A sweep's `lost` is its room's; the dead of the rooms it left are its log's (`leftDead`).
        rec.dead = e.lost + (e.actual?.leftDead ?? 0);
        [rec.contact, rec.seen, rec.gaveUp] = [run(e.actual?.contact), run(e.actual?.seen), Boolean(e.actual?.gaveUp)];
        rec.rooms.add(e.room);
        const ours = namesOf(e.ours);
        for (const n of ours) rec.ours.add(n);
        rec.held = Math.max(rec.held ?? 0, ours.length);
        if (rec.ours.size) rec.drafted ??= t;
        rec.answered ||= !e.unanswered;
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
        // `budget`: the bodies its price let it lose when it last committed: Formed, or re-committed by a newcomer (§6.2).
        const recommitted = rec.committed !== undefined && e.budget !== rec.committed;
        if (e.phase === "forming" || recommitted) rec.budget = e.budget ?? 0;
        // `forming`: the most ticks on end it formed answered.
        rec.formed = e.phase === "forming" && !e.unanswered ? rec.formed + 1 : 0;
        rec.forming = Math.max(rec.forming, rec.formed);
        const theirs = objects.filter((o) => rec.members.has(o.id));
        // `beside`: the ticks a Defend that disengaged as its raid stepped out stood so with its raid back in its room.
        const raid = (fight.groups ?? []).find((g) => g.id === e.group);
        // Given up past its losses it is not one the raid's return re-answers.
        if (e.phase === "disengaging" && rec.was !== "disengaging") rec.off = raid?.room !== e.room && e.lost <= e.budget + OUTPOST_LOSSES;
        rec.was = e.phase;
        if (rec.kind === "defend" && rec.off && e.phase === "disengaging" && raid?.room === e.room) rec.beside = (rec.beside ?? 0) + 1;
        rec.core ||= theirs.some((o) => o.type === "invaderCore");
        const bodies = objects.filter((o) => o.type === "creep" && o.ours && ours.includes(o.name));
        // `strayed`: the ticks a body of a fighting blob stood in its raid's room out of a shot's range of it
        // and of every mate in one, as a ranged heal reaches.
        const fighting = bodies.filter((o) => theirs.some((h) => near(h, o, 3)));
        const apart = bodies.some((o) => o.room === e.room && !fighting.some((m) => near(m, o, 3)));
        if (apart && fighting.length && e.phase === "fighting" && e.tactic.startsWith("blob")) rec.strayed = (rec.strayed ?? 0) + 1;
        swinging(rec, bodies, t, struck);
        // `raised`: the re-prices a forming Defend raised its set on after the tick it began forming.
        const set = ours.length + (fight.casts ?? []).filter((c) => c.engagement === e.id).length;
        if (e.phase === "forming" && rec.formedSince === e.since && !recommitted && set > rec.formingSet) rec.raised = (rec.raised ?? 0) + 1;
        rec.committed = e.budget;
        [rec.formedSince, rec.formingSet] = [e.phase === "forming" ? e.since : null, set];
        // A sweep with all its bodies in its room: `bare`, the most ticks on end no rival creep stood there.
        if (rec.kind !== "sweep") continue;
        // `disarmed`: the member-ticks it held a body the bot reads spent (`Creep::spent`, sim's engaged.rs).
        if (bodies.some((o) => o.spent === undefined)) throw new Error("fight.jsonl carries no `spent`: a sim from before it");
        rec.disarmed = (rec.disarmed ?? 0) + bodies.filter((o) => o.spent).length;
        const held = bodies.length > 0 && bodies.every((o) => o.room === e.room);
        const rivals = held ? objects.filter((o) => o.type === "creep" && !o.ours && o.room === e.room) : [];
        rec.empty = held && !rivals.length ? rec.empty + 1 : 0;
        rec.bare = Math.max(rec.bare, rec.empty);
        // `killed`: the rival bodies our seat killed beside it.
        if (held) for (const d of killedAt(t)) if (d.room === e.room) rec.kills.add(`${d.user} ${d.name}`);
        rec.killed = rec.kills.size;
        rec.askedMost = Math.max(rec.askedMost, (fight.casts ?? []).filter((c) => c.engagement === e.id).length);
        // `plinked`: its bodies' strikes on what was no creep then or a tick before, where prey stood (`Scene::ground`'s marks).
        const roomOf = new Map(bodies.map((o) => [o.id, o.room]));
        rec.plinked += strikes.filter(([by, hit]) => roomOf.has(by) && hit && !creeps.has(hit) && !lastCreeps.has(hit) && preyIn.has(roomOf.get(by))).length;
      }
      lastCreeps = creeps;
      for (const [id, rec] of open)
        if (!now.has(id)) {
          open.delete(id);
          const raid = (e) => (fight?.groups ?? []).find((g) => g.id === e.group)?.members ?? [];
          const older = (fight?.engagements ?? []).filter((e) => (e.kind ?? "defend") === "defend" && e.id < rec.id);
          const merged = older.some((e) => raid(e).some((m) => rec.raid.includes(m)) || namesOf(e.ours).some((n) => rec.standing.includes(n)));
          close(rec, t, rec.kind === "defend" && merged);
        }
    },
    end() {
      for (const rec of open.values()) close(rec, null);
    },
  };
}
