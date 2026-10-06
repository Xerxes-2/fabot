// Online, the server up (#543): pause the engine, step it one tick at a time
// and write every body in the scenario rooms after each tick, in the arena
// trace's row shape. Runs under xxscreeps' own loader, cwd the world:
//   xxscreeps.js file://…/step.mjs <ticks> <out.jsonl> [<heap tick> <heap file>]
// Stops early once no body but ours stands. The engine always runs its first
// tick on start; `pause` lands while it does (the runner's first tick builds
// the isolate), so that tick is the trace's t0, as the arena's first step is.
import * as fs from "node:fs";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import { getServiceChannel } from "xxscreeps/engine/service/index.js";

const [ticksArg, out, heapTickArg, heapOut] = process.argv.slice(2);
const ticks = Number(ticksArg);
const heapTick = heapTickArg === undefined ? undefined : Number(heapTickArg);
const { time: start, userIds, rooms } = JSON.parse(fs.readFileSync("world.json", "utf8"));
const ours = userIds.ours;

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

// Keep asking for the pause until the first tick is through.
const spam = setInterval(() => svc.publish({ type: "pause" }), 20);
await svc.publish({ type: "pause" });
let time = await nextTick(120_000);
clearInterval(spam);
if (time === null) throw new Error("the engine ran no first tick in 120 s");

const heap = async (tick) => {
  const req = { user: ours, out: heapOut };
  fs.rmSync(`${heapOut}.done`, { force: true });
  fs.writeFileSync("snap.req.tmp", JSON.stringify(req));
  fs.renameSync("snap.req.tmp", "snap.req");
  for (let i = 0; i < 600 && !fs.existsSync(`${heapOut}.done`); i++) await new Promise((r) => setTimeout(r, 100));
  const done = fs.existsSync(`${heapOut}.done`)
    ? JSON.parse(fs.readFileSync(`${heapOut}.done`, "utf8"))
    : { error: `no runner thread answered in 60 s: none holds a sandbox for ${ours}` };
  fs.rmSync("snap.req", { force: true });
  fs.rmSync(`${heapOut}.done`, { force: true });
  console.log(done.error ? `heap snapshot failed: ${done.error}` : `heap snapshot at t${tick}: ${heapOut} (${done.bytes} bytes, ${done.ms} ms)`);
};

const rows = [];
const t0 = Date.now();
for (let t = 0; t < ticks; t++) {
  if (t > 0) {
    await svc.publish({ type: "pausedTick" });
    time = await nextTick(30_000);
    if (time === null) {
      throw new Error(`no tick after a pausedTick at t${t}: the pause missed the first tick; run again`);
    }
  }
  const row = { t, time, bodies: [], acts: [], structures: {} };
  for (const name of rooms) {
    const room = await shard.loadRoom(name, time).catch(() => null);
    if (!room) continue;
    for (const o of room["#objects"]) {
      // The arena's `kind-x-y` ids for what a stronghold or a home stands behind.
      if (["invaderCore", "rampart", "tower"].includes(o.structureType)) {
        row.structures[`${o.structureType}-${o.pos.x}-${o.pos.y}`] = o.hits;
      }
      if (o.body === undefined) continue;
      row.bodies.push({
        id: o.name,
        room: name,
        x: o.pos.x,
        y: o.pos.y,
        hits: o.hits,
        fatigue: o.fatigue,
        side: o["#user"] === ours ? "ours" : o["#user"],
      });
      for (const a of o["#actionLog"] ?? []) if (a.time === time - 1) row.acts.push(`${o.name} ${a.type}@${a.x},${a.y}`);
    }
  }
  rows.push(row);
  if (heapTick === t) await heap(t);
  if (t > 0 && row.bodies.every((b) => b.side === "ours")) break;
}
fs.writeFileSync(out, rows.map((r) => JSON.stringify(r)).join("\n") + "\n");
console.log(`stepped ${rows.length} ticks (from engine tick ${start}) in ${Date.now() - t0} ms`);
if (heapTick !== undefined && heapTick >= rows.length) await heap(rows.length - 1);

await svc.publish({ type: "shutdown" });
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
process.exit(0);
