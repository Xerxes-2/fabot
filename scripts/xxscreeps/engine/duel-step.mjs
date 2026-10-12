// Online, the server up: step the exchange duels (`exchange.mjs`) until every
// room has one side left or <ticks> pass. Runs under xxscreeps' own loader:
//   xxscreeps.js file://…/duel-step.mjs <ticks> <out.json>
// Writes per room what happened: `won` (true, false, or null where both
// stand), the ticks from first contact (else the first hit) to the end, our bodies dead and our
// hits lost (a dead body's whole), and each side's hits a tick.
import * as fs from "node:fs";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import { getServiceChannel } from "xxscreeps/engine/service/index.js";

const [ticksArg, out] = process.argv.slice(2);
const ticks = Number(ticksArg);
const { userIds, rooms } = JSON.parse(fs.readFileSync("world.json", "utf8"));

const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const svc = getServiceChannel(shard);
const sub = shard.channel.iterable()[Symbol.asyncIterator]();
const nextTick = async (ms) => {
  const timeout = new Promise((resolve) => setTimeout(() => resolve(null), ms));
  for (;;) {
    const res = await Promise.race([sub.next(), timeout]);
    if (res === null) return null;
    if (res.value?.type === "tick") return res.value.time;
  }
};

const spam = setInterval(() => svc.publish({ type: "pause" }), 20);
await svc.publish({ type: "pause" });
let time = await nextTick(120_000);
clearInterval(spam);
if (time === null) throw new Error("the engine ran no first tick in 120 s");

const duels = new Map(rooms.map((r) => [r, { contact: null, struck: null, end: null, won: null, first: null, last: null, trace: [] }]));
const t0 = Date.now();
for (let t = 0; t < ticks && [...duels.values()].some((d) => d.end === null); t++) {
  if (t > 0) {
    await svc.publish({ type: "pausedTick" });
    time = await nextTick(60_000);
    if (time === null) throw new Error(`no tick after a pausedTick at t${t}`);
  }
  for (const [name, duel] of duels) {
    if (duel.end !== null) continue;
    const room = await shard.loadRoom(name, time);
    const bodies = { ours: new Map(), theirs: new Map() };
    for (const o of room["#objects"]) {
      if (o.body === undefined) continue;
      bodies[o["#user"] === userIds.ours ? "ours" : "theirs"].set(o.name, { hits: o.hits, x: o.pos.x, y: o.pos.y });
    }
    duel.first ??= bodies;
    duel.last = bodies;
    duel.trace.push(["ours", "theirs"].map((side) => [...bodies[side].values()].reduce((sum, b) => sum + b.hits, 0)));
    const struck = ["ours", "theirs"].some((side) =>
      [...duel.first[side]].some(([id, b]) => (bodies[side].get(id)?.hits ?? 0) < b.hits),
    );
    const near = [...bodies.ours.values()].some((a) =>
      [...bodies.theirs.values()].some((b) => Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) <= 3),
    );
    if (near) duel.contact ??= t;
    if (struck) duel.struck ??= t;
    if (bodies.ours.size === 0 || bodies.theirs.size === 0) {
      duel.end = t;
      duel.won = bodies.ours.size > 0;
    }
  }
}

const results = {};
for (const [name, d] of duels) {
  const lost = [...d.first.ours].reduce((sum, [id, b]) => sum + b.hits - (d.last.ours.get(id)?.hits ?? 0), 0);
  results[name] = {
    won: d.won,
    ticks: d.end === null || (d.contact ?? d.struck) === null ? null : d.end - (d.contact ?? d.struck),
    dead: [...d.first.ours.keys()].filter((id) => !d.last.ours.has(id)).length,
    hits: lost,
    trace: d.trace,
  };
}
fs.writeFileSync(out, JSON.stringify(results));
console.log(`stepped the duels in ${Date.now() - t0} ms`);

await svc.publish({ type: "shutdown" });
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
process.exit(0);
