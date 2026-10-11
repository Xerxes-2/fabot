// Online, the server up: pause the engine, step it one tick at a time and
// watch one colony. Runs under xxscreeps' own loader, cwd the world:
//   xxscreeps.js file://…/soak-step.mjs <ticks> <every> <room> <username> <out dir>
// Every tick: our CPU (the runner's usage channel), our console, and the
// observe segment our bot writes (segment 1: faults, refusals). Every
// <every> ticks, and on the last: one sample of the room. Writes to <out dir>
// `samples.jsonl` (one sample a line), `faults.jsonl` (each fault once),
// `refusals.json` (counts by act and code), `levels.json` (the tick each
// controller level was first seen), `observe.json` (the last segment
// read), `cpu.jsonl` (tick, engine-reported
// CPU, the bot's own phases) and `console.log`.
import * as fs from "node:fs";
import { join } from "node:path";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import * as User from "xxscreeps/engine/db/user/index.js";
import { getServiceChannel } from "xxscreeps/engine/service/index.js";
import { getConsoleChannel, runnerUsageChannel } from "xxscreeps/engine/runner/model.js";
import { loadMemorySegmentBlob } from "xxscreeps/mods/meta/memory/model.js";
import { typedArrayToString } from "xxscreeps/utility/string.js";

const [ticksArg, everyArg, roomName, username, out] = process.argv.slice(2);
const ticks = Number(ticksArg);
const every = Number(everyArg);
const observeSegment = 1;

const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const ours = await User.findUserByName(db, username);
const svc = getServiceChannel(shard);
const sub = shard.channel.iterable()[Symbol.asyncIterator]();

// The runner publishes a tick's usage before the processor closes that tick,
// so the latest message on the channel is the tick just stepped.
let cpu = null;
(await runnerUsageChannel(shard, ours).subscribe()).listen((usage) => (cpu = usage?.cpu ?? null));
const consoleLog = fs.openSync(join(out, "console.log"), "w");
let consoleTick = null;
(await getConsoleChannel(shard, ours).subscribe()).listen((message) => {
  let lines;
  try {
    lines = JSON.parse(message).map((m) => `${m.fd === 2 ? "ERR " : ""}${m.data}`);
  } catch {
    lines = [String(message)];
  }
  for (const line of lines) fs.writeSync(consoleLog, `[${consoleTick ?? "?"}] ${line}\n`);
});

const nextTick = async (ms) => {
  const timeout = new Promise((resolve) => setTimeout(() => resolve(null), ms));
  for (;;) {
    const res = await Promise.race([sub.next(), timeout]);
    if (res === null) return null;
    if (res.value?.type === "tick") return res.value.time;
  }
};

async function observed() {
  const blob = await loadMemorySegmentBlob(shard, ours, observeSegment);
  if (blob === null) return null;
  const text = typedArrayToString(new Uint16Array(blob.buffer, blob.byteOffset, blob.length >>> 1));
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}

const energyIn = (o) => o.store?.getUsedCapacity?.("energy") ?? 0;

function sample(t, time, room) {
  const row = {
    t,
    time,
    rcl: room["#level"],
    progress: 0,
    creeps: 0,
    parts: 0,
    hostiles: 0,
    sources: 0,
    energy: 0,
    capacity: 0,
    stored: 0,
    dropped: 0,
    sites: 0,
    cpu: null,
    bodies: [],
  };
  for (const o of room["#objects"]) {
    const mine = o["#user"] === ours;
    // A site carries its structure's type and no store: read it first.
    if (o["#lookType"] === "constructionSite") {
      if (mine) row.sites++;
    } else if (o.structureType === "controller") row.progress = o["#progress"] ?? 0;
    else if (o.structureType === "spawn" || o.structureType === "extension") {
      if (!mine) continue;
      row.energy += energyIn(o);
      row.capacity += o.store?.getCapacity?.("energy") ?? 0;
    } else if (o.structureType === "container" || o.structureType === "storage") row.stored += energyIn(o);
    else if (o["#lookType"] === "resource" && o.resourceType === "energy") row.dropped += o.amount;
    else if (o["#lookType"] === "source") row.sources += o.energy;
    else if (o.body !== undefined) {
      if (mine) {
        row.creeps++;
        row.parts += o.body.length;
        // Where each body stands, what it carries and what it did this tick (the
        // processor stamps an action with the tick it leads to).
        const acts = (o["#actionLog"] ?? []).filter((a) => a.time === time).map((a) => a.type);
        row.bodies.push(`${o.name}@${o.pos.x},${o.pos.y} ${o.body.map((p) => p.type[0]).join("")} e${energyIn(o)} ${acts.join("+") || "-"}`);
      } else row.hostiles++;
    }
  }
  return row;
}

// Keep asking for the pause until the first tick is through (`step.mjs`).
const spam = setInterval(() => svc.publish({ type: "pause" }), 20);
await svc.publish({ type: "pause" });
let time = await nextTick(120_000);
clearInterval(spam);
if (time === null) throw new Error("the engine ran no first tick in 120 s");

const samplesOut = fs.openSync(join(out, "samples.jsonl"), "w");
const faultsOut = fs.openSync(join(out, "faults.jsonl"), "w");
const cpuOut = fs.openSync(join(out, "cpu.jsonl"), "w");
const seenFaults = new Set();
const levels = [];
const refusals = {};
let lastObserved = null;
let lastObs = null;
const started = Date.now();
// Steps that needed a second pausedTick: reported, never silent.
const slowTicks = [];
for (let t = 0; t < ticks; t++) {
  consoleTick = time;
  if (t > 0) {
    cpu = null; // a tick the runner skips reports no usage
    // The engine waits up to its intent-abandon timeout (120 s, set by
    // soak.mjs) for our bot's intents, so a tick may legitimately take that
    // long; wait past it, then ask once more before giving up.
    await svc.publish({ type: "pausedTick" });
    time = await nextTick(150_000);
    if (time === null) {
      slowTicks.push(t);
      await svc.publish({ type: "pausedTick" });
      time = await nextTick(150_000);
    }
    if (time === null) throw new Error(`no tick after two pausedTicks at t${t}; see the server log`);
  }
  const obs = await observed();
  // A segment the bot did not rewrite this tick is last tick's: count it once.
  const fresh = obs !== null && obs.tick !== lastObserved;
  if (fresh) {
    lastObserved = obs.tick;
    lastObs = obs;
    for (const f of obs.faults ?? []) {
      const key = `${f.tick}|${f.where}|${f.message}`;
      if (seenFaults.has(key)) continue;
      seenFaults.add(key);
      fs.writeSync(faultsOut, JSON.stringify(f) + "\n");
    }
    for (const r of obs.refusals ?? []) {
      const key = `${r.act} ${r.code}`;
      refusals[key] = (refusals[key] ?? 0) + 1;
    }
  }
  fs.writeSync(cpuOut, JSON.stringify({ t, time, cpu, observed: fresh ? obs.cpu : null, phases: fresh ? obs.phases : null }) + "\n");
  const room = await shard.loadRoom(roomName, time);
  if (room["#level"] !== levels.at(-1)?.rcl) levels.push({ t, time, rcl: room["#level"] });
  if (t % every === 0 || t === ticks - 1) {
    const row = sample(t, time, room);
    row.cpu = cpu;
    row.botCpu = fresh ? obs.cpu : null;
    row.faults = seenFaults.size;
    row.ms = Date.now() - started;
    fs.writeSync(samplesOut, JSON.stringify(row) + "\n");
    process.stderr.write(`t${t} rcl${row.rcl} ${row.progress} creeps ${row.creeps} (${row.ms} ms)\n`);
  }
}
fs.writeFileSync(join(out, "refusals.json"), JSON.stringify(refusals, null, 2) + "\n");
fs.writeFileSync(join(out, "observe.json"), JSON.stringify(lastObs, null, 1) + "\n");
fs.writeFileSync(join(out, "levels.json"), JSON.stringify(levels) + "\n");
fs.writeFileSync(join(out, "slow.json"), JSON.stringify(slowTicks) + "\n");
console.log(JSON.stringify({ ticks, ms: Date.now() - started }));

await svc.publish({ type: "shutdown" });
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
process.exit(0);
