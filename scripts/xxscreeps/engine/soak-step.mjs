// Online, the server up: pause the engine, step it one tick at a time and
// watch one colony. Runs under xxscreeps' own loader, cwd the world:
//   xxscreeps.js file://…/soak-step.mjs <ticks> <every> <room[,room…]> <username> <out dir> [<waves> [<opponents.json> [<cores> [<wave> [<watch ms>]]]]]
// Every tick: our CPU (the runner's usage channel), our console, and the
// observe segment our bot writes (segment 1: faults, refusals). Every
// <every> ticks, and on the last: one sample of the room. Writes to <out dir>
// `samples.jsonl` (one sample a line), `faults.jsonl` (each fault once),
// `refusals.json` (counts by act and code), `lost.json` (the ticks lost),
// `levels.json` (the tick each controller level was first seen),
// `observe.json` (the last segment read), `metrics.jsonl` (each 100-tick window
// segment 3 holds, a row a line, `t` the run tick it was read; the newest read carries the `ledger` as it stood then), `trace.jsonl` (the bot's `#trace` lines), `cpu.jsonl` (tick, engine-reported
// CPU, the bot's own phases and counts) and `console.log`. Each sample carries the
// room's energy ledger, cumulative from the first tick (`ledger`): dug,
// spawn regeneration, spent on bodies, building, upgrading, repairing and
// towers, lost to pile decay and to link sends, and the stock standing
// (stores, carried, piles); dug + regen = spent + decayed + linked + Δstock,
// less the residual it reports; what strangers drop in or carry off shared
// piles and containers stays in it. `decay.jsonl`: each energy pile that
// decayed, once it is gone or the run ends, with its tile's role and the
// tombstone or ruin it fell out of. `bodies.jsonl`: each body, ours or not,
// once it is gone from the colony's rooms, their neighbours and the opponents' rooms or the run ends: its parts, the ticks it
// was first and last seen, its ticks to live then, the damage struck on it and the last tick it was struck (`struckAt`), its ticks by room and
// by action, each room it entered with the tick, and the tiles it harvested and reserved from (`tiles`).
// The colony's rooms are <room> (several homes: each sample's `homes` holds
// one sample a home, the first's at its top), the outposts and claims its Memory
// declares at the start, and each room we later reserve or a body of ours
// harvests, builds or repairs in, from the tick it first does: the ledger and the worth
// sum over them, and each sample's `rooms` holds one line a room, cumulative:
// dug, decayed, built, repaired, `bodies` (each of our bodies' cost spread
// over its life, charged to the room it stands in each tick), `net` (dug less
// decayed, repairs and bodies), ticks reserved by us and ticks a hostile
// stood there, and as it stands: our creeps, hostiles, roads, containers,
// Invader cores.
// <waves>: room:every[:first],… — an Invader wave (`waves/`) pushed in room
// at t = first, first + every, …
// <opponents.json>: [{ name, room, raid? }], the players seated beside ours;
// each sample's `others` holds each one's room as it stands: level, spawns,
// creeps, and its worth and score over its room and the followed ones. A raid { first, every?, room?, inside?, bodies: [{ part: count, boosts?: {
// part: compound } }] } stands the opponent's bodies on the edge of `room`
// (default <room>) toward its room (`waves/`) at t = first, first + every, …
// <cores>: room:tick,… — a level-0 Invader core (`core/`) stood in room at t = tick.
// <follow>: soak's --follow, rooms the samples follow besides.
// <watch ms>: soak's --watch, each step held to it and the server left up after the last; 0 (default) off.
import * as fs from "node:fs";
import { join } from "node:path";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import * as User from "xxscreeps/engine/db/user/index.js";
import { getServiceChannel } from "xxscreeps/engine/service/index.js";
import { getConsoleChannel, runnerUsageChannel } from "xxscreeps/engine/runner/model.js";
import { loadMemorySegmentBlob, loadUserMemoryString } from "xxscreeps/mods/meta/memory/model.js";
import { loadTransactionBlob, loadTransactionEntries } from "xxscreeps/mods/classic/brokerage/model.js";
import { read as readTransaction } from "xxscreeps/mods/classic/brokerage/transaction.js";
import { getProcessorChannel, pushIntentsForRoomNextTick, userToIntentRoomsSetKey } from "xxscreeps/engine/processor/model.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { REACTIONS } from "xxscreeps/mods/classic/chemistry/constants.js";
import { typedArrayToString } from "xxscreeps/utility/string.js";
import { METRICS, windows } from "./metrics.mjs";
import { engagements } from "./engaged.mjs";
const LETTER = { move: "m", work: "w", carry: "c", attack: "a", ranged_attack: "r", heal: "h", claim: "k", tough: "t" };
const letters = (body) => body.map((p) => LETTER[p.type]).join("");
const COMPOUNDS = [...new Set(Object.values(REACTIONS).flatMap(Object.values))];
// Where a compound `made` counts stands.
const MADE_IN = ["storage", "terminal", "lab"];

const [ticksArg, everyArg, roomArg, username, out, wavesArg = "", opponentsPath, coresArg = "", waveArg = "melee,healer,ranged", watchArg = "0", followArg = ""] = process.argv.slice(2);
const homes = roomArg.split(",");
const roomName = homes[0];
const ticks = Number(ticksArg);
const every = Number(everyArg);
const observeSegment = 1;
const watchMs = Number(watchArg);

const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const ours = await User.findUserByName(db, username);
const svc = getServiceChannel(shard);
const memory = JSON.parse((await loadUserMemoryString(shard, ours)) || "{}");
const claimed = (memory.v2?.declarations?.claims ?? []).map((o) => o.room);
const roomNames = [...new Set([...homes, ...(memory.v2?.declarations?.outposts ?? []).map((o) => o.room), ...claimed, ...followArg.split(",").filter(Boolean)])];
const waves = wavesArg
  .split(",")
  .filter(Boolean)
  .map((w) => {
    const [room, every, first = every] = w.split(":");
    return { room, every: Number(every), first: Number(first) };
  });
const cores = coresArg
  .split(",")
  .filter(Boolean)
  .map((c) => {
    const [room, at] = c.split(":");
    return { room, at: Number(at) };
  });
const opponents = await Promise.all(
  (opponentsPath ? JSON.parse(fs.readFileSync(opponentsPath, "utf8")) : []).map(async (o) => ({ ...o, id: await User.findUserByName(db, o.name) })),
);
const sub = shard.channel.iterable()[Symbol.asyncIterator]();

// The runner publishes a tick's usage before the processor closes that tick,
// so the latest message on the channel is the tick just stepped.
let cpu = null;
(await runnerUsageChannel(shard, ours).subscribe()).listen((usage) => (cpu = usage?.cpu ?? null));
const consoleLog = fs.openSync(join(out, "console.log"), "w");
let consoleTick = null;
let captureLog;
let traceLog;
(await getConsoleChannel(shard, ours).subscribe()).listen((message) => {
  let lines;
  try {
    lines = JSON.parse(message).map((m) => `${m.fd === 2 ? "ERR " : ""}${m.data}`);
  } catch {
    lines = [String(message)];
  }
  for (const line of lines)
    if (line.startsWith("#capture ")) fs.writeSync((captureLog ??= fs.openSync(join(out, "capture.jsonl"), "w")), line.slice(9) + "\n");
    else if (line.startsWith("#trace ")) fs.writeSync((traceLog ??= fs.openSync(join(out, "trace.jsonl"), "w")), JSON.stringify({ t: consoleTick, ...JSON.parse(line.slice(7)) }) + "\n");
    else {
      if (/^ERR Script (timed out|was disposed)/.test(line)) lost.add(consoleTick);
      fs.writeSync(consoleLog, `[${consoleTick ?? "?"}] ${line}\n`);
    }
});
// Ticks lost: our bot's tick cut off by its time limit (its CPU or the
// host's load), or the engine abandoning intents in one of its rooms. A run
// holding one is no measurement. An opponent's room alone is left out: its
// bot's ticks are not ours to measure.
const lost = new Set();
const serverLog = fs.openSync(join(out, "server.log"), "r");
let serverRead = 0;
function abandoned() {
  const size = fs.fstatSync(serverLog).size;
  if (size <= serverRead) return;
  const text = Buffer.alloc(size - serverRead);
  serverRead += fs.readSync(serverLog, text, 0, text.length, serverRead);
  const lines = text.toString();
  for (const [, rooms, tick] of lines.matchAll(/Abandoning intents in rooms \[(.*)\] for tick (\d+)/g))
    if (rooms.split(", ").some((r) => roomNames.includes(r))) lost.add(Number(tick));
}
for (const o of opponents) {
  const log = fs.openSync(join(out, `console-${o.name}.log`), "w");
  (await getConsoleChannel(shard, o.id).subscribe()).listen((message) => fs.writeSync(log, `[${consoleTick ?? "?"}] ${message}\n`));
}

// The processor drops a wake-up that lands while its worker goes idle, and the tick waits out the abandon
// with every intent in; a resent one takes only rooms no runner still owes, so a stuck bot is still abandoned.
const processor = getProcessorChannel(shard);
const nextTick = async (ms) => {
  const timeout = new Promise((resolve) => setTimeout(() => resolve(null), ms));
  const wake = setInterval(() => shard.data.get("time").then((time) => processor.publish({ type: "process", time: Number(time) }), () => {}), 1000);
  try {
    for (;;) {
      const res = await Promise.race([sub.next(), timeout]);
      if (res === null) return null;
      if (res.value?.type === "tick") return res.value.time;
    }
  } finally {
    clearInterval(wake);
  }
};

async function segmentText(id) {
  const blob = await loadMemorySegmentBlob(shard, ours, id);
  return blob === null ? null : typedArrayToString(new Uint16Array(blob.buffer, blob.byteOffset, blob.length >>> 1));
}
let windowsRead = -Infinity;
async function readWindows(t) {
  const fresh = windows(await segmentText(METRICS)).filter((w) => w.start > windowsRead);
  windowsRead = Math.max(windowsRead, ...fresh.map((w) => w.start));
  const ledgered = ledgerRow();
  for (const w of fresh) fs.writeSync(metricsOut, JSON.stringify({ t, ...w, ...(w.start === windowsRead && { ledger: ledgered }) }) + "\n");
}
async function observed() {
  const text = await segmentText(observeSegment);
  if (text === null) return null;
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}

const energyIn = (o) => o.store?.getUsedCapacity?.("energy") ?? 0;

// Event ids (EVENT_* constants).
const HARVEST = 5;
const BUILD = 4;
const REPAIR = 7;
const UPGRADE = 9;
const ATTACK = 1;
const HEAL = 6;
const TRANSFER = 12;
// A send between links credits floor(amount × (1 − LINK_LOSS_RATIO)); a
// creep's withdraw from a link logs a transfer from it too.
const LINK_KEEPS = 1 - 0.03;
const CREEP_LIFE_TIME = 1500;
const CREEP_CLAIM_LIFE_TIME = 600;
const PARTS = ["tough", "move", "work", "carry", "attack", "ranged_attack", "heal"];
const PART_COST = { move: 50, work: 100, carry: 50, attack: 80, ranged_attack: 150, heal: 250, claim: 600, tough: 10 };
const ledger = { dug: 0, regen: 0, spawn: 0, build: 0, upgrade: 0, repair: 0, tower: 0, decayed: 0, linked: 0, stock0: null, stock: 0 };
// What a colony holds, in energy, as it stands: the controller's climb, every
// structure at its construction cost (a road at its terrain's), ramparts' and
// walls' hits at the repair rate, sites at their progress, and the stock.
// Read off the room each sample, never summed from history: a structure that
// decayed or was destroyed is worth nothing.
const CONTROLLER_LEVELS = { 1: 200, 2: 45000, 3: 135000, 4: 405000, 5: 1215000, 6: 3645000, 7: 10935000 };
const CONSTRUCTION_COST = {
  spawn: 15000, extension: 3000, road: 300, constructedWall: 1, rampart: 1, link: 5000, storage: 30000, tower: 5000,
  observer: 8000, powerSpawn: 100000, extractor: 5000, lab: 50000, terminal: 100000, container: 5000, nuker: 100000, factory: 100000,
};
const REPAIR_COST = 0.01;
// What a colony can use, not what it holds: a structure carries the level that
// unlocked it, stock saturates at the bot's reserve, a site is not yet anything.
const UNLOCK = {
  spawn: 1, extension: 1, container: 1, road: 1, rampart: 1, constructedWall: 1, tower: 3, storage: 4, link: 5,
  extractor: 6, lab: 6, terminal: 6, factory: 7, observer: 8, powerSpawn: 8, nuker: 8,
};
const TOWERS = [0, 0, 0, 1, 1, 2, 2, 3, 6];
// `reserveTarget` (core/economy/quota.rs): every tower firing through a
// creep's life, twice over — and nothing before storage exists to keep it in.
const reserveOf = (level) => (level >= 4 ? TOWERS[level] * 10 * 1500 * 2 : 0);

const creditOf = (stock, level) => {
  const reserve = reserveOf(level);
  return reserve === 0 ? stock : reserve * Math.tanh(stock / reserve);
};
const world = await shard.loadWorld();
const borderOf = () =>
  [...new Set([...roomNames.flatMap((r) => Object.values(world.map.describeExits(r) ?? {})), ...opponents.map((o) => o.room)])].filter(
    (r) => !roomNames.includes(r) && world.map.describeExits(r) !== null,
  );
let bordering = borderOf();
const roadCost = (room, pos) => {
  const t = world.map.getRoomTerrain(room).get(pos.x, pos.y);
  return t & 1 ? 150 * 300 : t & 2 ? 5 * 300 : 300;
};
// A seat's: its own objects, and the ownerless ones (roads, containers, walls)
// in the rooms it owns or reserves; ours also in the rooms no other seat holds.
function worthAndScore(rooms, stock, user = ours) {
  const worth = { controller: 0, structures: 0, fortification: 0, sites: 0, stock };
  const score = { controller: 0, structures: 0, fortification: 0, stock: 0 };
  const level = Math.max(0, ...rooms.filter((r) => r.controller?.["#user"] === user).map((r) => r["#level"] ?? 0));
  for (const room of rooms) for (const o of room["#objects"]) {
    const mine = o["#user"] === user;
    const held = room["#user"] === user || (user === ours && !opponents.some((s) => s.id === room["#user"]));
    const ownerless = held && (o.structureType === "road" || o.structureType === "container");
    if (o["#lookType"] === "constructionSite") {
      if (mine) worth.sites += o.progress ?? 0;
    } else if (o.structureType === "controller") {
      if (!mine) continue;
      for (let l = 1; l < room["#level"]; l++) worth.controller += CONTROLLER_LEVELS[l];
      worth.controller += o["#progress"] ?? 0;
    } else if (o.structureType === "rampart" || o.structureType === "constructedWall") {
      if (mine || (held && o.structureType === "constructedWall")) worth.fortification += Math.floor(o.hits * REPAIR_COST);
    } else if (o.structureType !== undefined && (mine || ownerless)) {
      const cost = o.structureType === "road" ? roadCost(room.name, o.pos) : CONSTRUCTION_COST[o.structureType] ?? 0;
      worth.structures += cost;
      score.structures += cost * (1 + Math.log(UNLOCK[o.structureType] ?? 1));
    }
  }
  score.controller = worth.controller;
  score.fortification = worth.fortification;
  score.stock = creditOf(stock, level);
  return { worth, score };
}

const born = new Set();
let lastPiles = new Map();
const pileLog = new Map();
const graves = new Map();
const lives = new Map();
// A body's key by id: the strike that kills it lands the tick it is gone from the room.
const byId = new Map();
const near = (a, b, r) => Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) <= r;
function roleOf(pos, objects) {
  const by = (pred) => objects.filter(pred);
  if (by((o) => o["#lookType"] === "constructionSite" && near(o.pos, pos, 0)).length) return "on a site";
  const sources = by((o) => o["#lookType"] === "source");
  const controller = by((o) => o.structureType === "controller")[0];
  const containers = by((o) => o.structureType === "container" && o["#lookType"] !== "constructionSite");
  const atSource = (c) => sources.some((s) => near(s.pos, c.pos, 1));
  const onTile = containers.find((c) => near(c.pos, pos, 0));
  if (onTile) return atSource(onTile) ? "on source container" : "on other container";
  if (sources.some((s) => near(s.pos, pos, 1))) return "beside source";
  if (controller && containers.some((c) => !atSource(c) && near(c.pos, controller.pos, 3) && near(c.pos, pos, 1))) return "beside controller buffer";
  if (by((o) => o.structureType === "spawn" || o.structureType === "extension").some((o) => near(o.pos, pos, 1))) return "beside spawn/extension";
  return "elsewhere";
}
const pileOut = (p) => fs.writeSync(decayOut, JSON.stringify(p) + "\n");
let regenNext = 0;

// One tick of the ledger, off the colony's rooms as the tick left them.
const newLine = () => ({ dug: 0, decayed: 0, build: 0, upgrade: 0, repair: 0, bodies: 0, reserved: 0, hostile: 0, waves: 0, carriedOut: 0, leastDowngrade: Infinity });
const lines = Object.fromEntries(roomNames.map((r) => [r, newLine()]));
// The energy objects hold as the ledger counts it: piles, ownerless stores,
// and the bodies and structures of `user`.
const stockOf = (objects, user = ours) =>
  objects.reduce((n, o) => n + (o["#lookType"] === "resource" ? (o.resourceType === "energy" ? o.amount : 0) : o["#user"] != null && o["#user"] !== user ? 0 : energyIn(o)), 0);
// Strategy picks remotes as it runs. A room joins with what it held the tick
// before as held from the start: the ledger saw none of it come in.
async function followWorked(time) {
  const before = roomNames.length;
  for (const name of (await shard.scratch.sMembers(userToIntentRoomsSetKey(ours))).sort()) {
    if (roomNames.includes(name)) continue;
    const room = await shard.loadRoom(name, time);
    const objects = [...room["#objects"]];
    const sources = new Set(objects.filter((o) => o["#lookType"] === "source").map((o) => o.id));
    const worked = (room["#eventLog"] ?? []).some((e) => {
      const kind = e.event ?? e._$Variant;
      return ((kind === HARVEST && sources.has(e.targetId)) || kind === BUILD || kind === REPAIR) && objects.some((o) => o.id === e.objectId && o.body !== undefined && o["#user"] === ours);
    });
    if (room["#user"] !== ours && !worked) continue;
    roomNames.push(name);
    lines[name] = newLine();
    if (ledger.stock0 !== null) ledger.stock0 += stockOf([...(await shard.loadRoom(name, time - 1))["#objects"]]);
  }
  if (roomNames.length > before) bordering = borderOf();
}
function account(rooms, time) {
  ledger.regen += regenNext;
  regenNext = 0;
  const piles = new Map();
  let stock = 0;
  for (const room of rooms) {
    const line = lines[room.name];
    const objects = [...room["#objects"]];
    stock += stockOf(objects);
    const sources = new Set(objects.filter((o) => o["#lookType"] === "source").map((o) => o.id));
    const towers = new Set(objects.filter((o) => o.structureType === "tower" && o["#user"] === ours).map((o) => o.id));
    const links = new Set(objects.filter((o) => o.structureType === "link" && o["#user"] === ours).map((o) => o.id));
    const strangers = new Set(objects.filter((o) => o["#user"] != null && o["#user"] !== ours).map((o) => o.id));
    for (const e of room["#eventLog"] ?? []) {
      if (strangers.has(e.objectId)) continue;
      const kind = e.event ?? e._$Variant;
      if (kind === HARVEST && sources.has(e.targetId)) {
        ledger.dug += e.amount;
        line.dug += e.amount;
      } else if (kind === BUILD) {
        ledger.build += e.energySpent ?? 0;
        line.build += e.energySpent ?? 0;
      } else if (kind === UPGRADE) {
        ledger.upgrade += e.energySpent ?? 0;
        line.upgrade += e.energySpent ?? 0;
      }
      else if (kind === REPAIR) {
        if (towers.has(e.objectId)) ledger.tower += e.energySpent ?? 10;
        else {
          ledger.repair += e.energySpent ?? 0;
          line.repair += e.energySpent ?? 0;
        }
      } else if (kind === TRANSFER && links.has(e.objectId) && links.has(e.targetId)) ledger.linked += e.amount - Math.floor(e.amount * LINK_KEEPS);
      else if ((kind === ATTACK || kind === HEAL) && towers.has(e.objectId)) ledger.tower += 10;
    }
    if (room["#user"] === ours && room.controller?.["#user"] !== ours && room.controller?.["#reservationEndTime"] > time) line.reserved++;
    if (room.controller?.["#user"] === ours) line.leastDowngrade = Math.min(line.leastDowngrade, room.controller["#downgradeTime"] - time);
    let bank = 0;
    let hostile = false;
    const spawns = [];
    for (const o of objects) {
      if (o["#lookType"] === "resource") {
        if (o.resourceType !== "energy") continue;
        piles.set(o.id, o.amount);
        const before = lastPiles.get(o.id);
        if (before !== undefined) {
          ledger.decayed += Math.ceil(before / 1000);
          line.decayed += Math.ceil(before / 1000);
        }
        let log = pileLog.get(o.id);
        if (log === undefined) {
          const g = graves.get(`${room.name} ${o.pos.x},${o.pos.y}`);
          const grave = g !== undefined && g.time >= time - 1 ? g.who : null;
          log = { room: room.name, x: o.pos.x, y: o.pos.y, role: roleOf(o.pos, objects), grave, first: time, last: time, peak: 0, decayed: 0 };
          pileLog.set(o.id, log);
        }
        log.last = time;
        log.peak = Math.max(log.peak, o.amount);
        if (before !== undefined) log.decayed += Math.ceil(before / 1000);
        continue;
      }
      if (o["#lookType"] === "tombstone" || o["#lookType"] === "ruin") {
        const who = o["#creep"] ? `${o["#creep"].name} ttl${o["#creep"].ticksToLive} e${energyIn(o)}` : "ruin";
        graves.set(`${room.name} ${o.pos.x},${o.pos.y}`, { time, who });
      }
      const cost = o.body?.reduce((sum, p) => sum + (PART_COST[p.type] ?? 0), 0);
      if (o.body !== undefined && o["#user"] !== ours) {
        hostile = true;
        continue;
      }
      if (o.body !== undefined) {
        if (!born.has(o.name)) {
          born.add(o.name);
          // A body standing at the first tick was paid for before the run.
          if (ledger.stock0 !== null) ledger.spawn += cost;
        }
        line.bodies += cost / (o.body.some((p) => p.type === "claim") ? CREEP_CLAIM_LIFE_TIME : CREEP_LIFE_TIME);
      }
      if ((o.structureType === "spawn" || o.structureType === "extension") && o["#user"] === ours) {
        bank += energyIn(o);
        if (o.structureType === "spawn") spawns.push(o);
      }
    }
    if (hostile) line.hostile++;
    // A spawn regains one a tick while it and the room's bank are under 300.
    if (bank < 300) regenNext += spawns.filter((sp) => energyIn(sp) < 300).length;
  }
  lastPiles = piles;
  for (const [id, log] of pileLog)
    if (!piles.has(id)) {
      if (log.decayed > 0) pileOut(log);
      pileLog.delete(id);
    }
  if (ledger.stock0 === null) ledger.stock0 = stock;
  ledger.stock = stock;
}

function live(rooms, time) {
  for (const room of rooms) {
    const struck = new Map();
    for (const e of room["#eventLog"] ?? [])
      if ((e.event ?? e._$Variant) === ATTACK) struck.set(e.targetId, (struck.get(e.targetId) ?? 0) + e.damage);
    for (const o of room["#objects"]) {
      if (o.body === undefined) continue;
      const key = `${o["#user"]} ${o.name}`;
      byId.set(o.id, key);
      const cost = o.body.reduce((sum, p) => sum + (PART_COST[p.type] ?? 0), 0);
      const life = lives.get(key) ?? { name: o.name, ours: o["#user"] === ours, body: letters(o.body), cost, first: time, rooms: {}, acts: {} };
      lives.set(key, life);
      // Ticks it stood on the tile it stood on the tick before.
      const at = `${room.name} ${o.pos.x},${o.pos.y}`;
      if (life.at === at && life.last === time - 1) life.still = (life.still ?? 0) + 1;
      if (life.at !== undefined && life.at !== at) life.left ??= time;
      if (!life.at?.startsWith(`${room.name} `)) (life.entered ??= []).push([room.name, time]);
      life.at = at;
      life.last = time;
      life.ttl = o["#ageTime"] - time;
      life.hits = o.hits;
      life.rooms[room.name] = (life.rooms[room.name] ?? 0) + 1;
      for (const a of (o["#actionLog"] ?? []).filter((a) => a.time === time)) {
        life.acts[a.type] = (life.acts[a.type] ?? 0) + 1;
        if ((a.type === "harvest" || a.type === "reserveController") && !life.tiles?.[a.type]?.includes(at)) ((life.tiles ??= {})[a.type] ??= []).push(at);
      }
    }
    for (const [id, damage] of struck) {
      const life = lives.get(byId.get(id));
      if (life) (life.struck = (life.struck ?? 0) + damage), (life.struckAt = time);
    }
  }
  for (const [key, life] of lives)
    if (life.last !== time) {
      fs.writeSync(livesOut, JSON.stringify(life) + "\n");
      lives.delete(key);
    }
}

function roomLines(rooms) {
  return Object.fromEntries(
    rooms.map((room) => {
      const line = lines[room.name];
      // `held`: what our storage, terminal, labs, factory and power spawn hold; `ores` the non-energy of it.
      const now = { creeps: 0, hostiles: 0, roads: 0, containers: 0, cores: 0, held: 0, ores: 0, made: {} };
      for (const o of room["#objects"]) {
        if (o["#user"] === ours && o.store && ["storage", "terminal", "lab", "factory", "powerSpawn"].includes(o.structureType)) {
          // A lab's store answers no total.
          for (const r of Object.keys(o.store)) {
            now.held += o.store.getUsedCapacity(r) ?? 0;
            if (r !== "energy") now.ores += o.store.getUsedCapacity(r) ?? 0;
            if (COMPOUNDS.includes(r) && MADE_IN.includes(o.structureType)) now.made[r] = (now.made[r] ?? 0) + (o.store.getUsedCapacity(r) ?? 0);
          }
        }
        if (o.body !== undefined) now[o["#user"] === ours ? "creeps" : "hostiles"]++;
        else if (o.structureType === "invaderCore" && o["#lookType"] !== "ruin") now.cores++;
        else if (o["#lookType"] !== "constructionSite" && (o.structureType === "road" || o.structureType === "container")) now[`${o.structureType}s`]++;
      }
      const bodies = Math.round(line.bodies);
      return [room.name, { ...line, bodies, net: line.dug - line.decayed - line.repair - bodies, ...now }];
    }),
  );
}

function ledgerRow() {
  const { stock0, stock, ...flows } = ledger;
  const spent = flows.spawn + flows.build + flows.upgrade + flows.repair + flows.tower;
  return { ...flows, stock, residual: flows.dug + flows.regen - spent - flows.decayed - flows.linked - (stock - stock0) };
}

// A site that pays back, by id: the tick a sample first saw it.
const PAYING = ["spawn", "extension", "container", "storage", "link", "tower"];
const placedAt = new Map();
const summary = ({ bodies, foes, ...s }) => s;
function sample(t, time, room, user = ours) {
  const row = {
    t,
    time,
    rcl: room["#level"],
    safeMode: room["#safeModeUntil"] > time,
    progress: 0,
    creeps: 0,
    parts: 0,
    spawns: 0,
    hostiles: 0,
    sources: 0,
    energy: 0,
    capacity: 0,
    stored: 0,
    dropped: 0,
    sites: 0,
    queued: {},
    oldestPaying: 0,
    built: {},
    cpu: null,
    bodies: [],
    foes: [],
  };
  for (const o of room["#objects"]) {
    const mine = o["#user"] === user;
    // Our structures, and the roads and containers nobody owns, in our room.
    const ownerless = o.structureType === "road" || o.structureType === "container";
    if (o.structureType !== undefined && o["#lookType"] !== "constructionSite" && (mine || ownerless) && o.structureType !== "controller")
      row.built[o.structureType] = (row.built[o.structureType] ?? 0) + 1;
    // A site carries its structure's type and no store: read it first.
    if (o["#lookType"] === "constructionSite") {
      if (mine) {
        row.sites++;
        row.queued[o.structureType] = (row.queued[o.structureType] ?? 0) + 1;
        if (PAYING.includes(o.structureType)) {
          if (!placedAt.has(o.id)) placedAt.set(o.id, t);
          row.oldestPaying = Math.max(row.oldestPaying, t - placedAt.get(o.id));
        }
      }
    } else if (o.structureType === "controller") row.progress = o["#progress"] ?? 0;
    else if (o.structureType === "spawn" || o.structureType === "extension") {
      if (!mine) continue;
      if (o.structureType === "spawn") row.spawns++;
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
        row.bodies.push(`${o.name}@${o.pos.x},${o.pos.y} ${letters(o.body)} e${energyIn(o)} ${acts.join("+") || "-"}`);
      } else {
        row.hostiles++;
        row.foes.push(`${o.name}@${o.pos.x},${o.pos.y} ${letters(o.body)}`);
      }
    }
  }
  const objects = [...room["#objects"]];
  const atSource = (c) => objects.some((s) => s["#lookType"] === "source" && near(s.pos, c.pos, 1));
  const sourceContainers = objects.filter((o) => o.structureType === "container" && o["#lookType"] !== "constructionSite" && atSource(o));
  row.sourceContainers = sourceContainers.length;
  const roads = objects.filter((o) => o.structureType === "road" && o["#lookType"] !== "constructionSite");
  const solid = objects.filter((o) => o["#lookType"] === "structure" && (o["#user"] === user || o.structureType === "constructedWall") && !["road", "container", "rampart", "controller"].includes(o.structureType));
  row.buried = roads.filter((r) => solid.some((o) => o.pos.x === r.pos.x && o.pos.y === r.pos.y)).length;
  // A harvest bay's extension is its harvester's, filled from the container
  // beside it; an extension bay's (Hivemind's) is filled from the open tile
  // five or more of them ring.
  const extensions = solid.filter((o) => o.structureType === "extension");
  const bayCentre = (o) =>
    [-1, 0, 1].some((dx) => [-1, 0, 1].some((dy) => {
      const [x, y] = [o.pos.x + dx, o.pos.y + dy];
      return (dx || dy) && !(world.map.getRoomTerrain(room.name).get(x, y) & 1) && !solid.some((s) => s.pos.x === x && s.pos.y === y) && extensions.filter((e) => Math.max(Math.abs(e.pos.x - x), Math.abs(e.pos.y - y)) === 1).length >= 5;
    }));
  row.unroaded = extensions.filter((o) => !sourceContainers.some((c) => near(c.pos, o.pos, 1)) && !bayCentre(o) && !roads.some((r) => near(r.pos, o.pos, 1) && !solid.some((s) => s.pos.x === r.pos.x && s.pos.y === r.pos.y))).length;
  row.postedLinks = objects.filter((o) => o.structureType === "link" && o["#lookType"] !== "constructionSite" && sourceContainers.some((c) => near(c.pos, o.pos, 1))).length;
  row.mineralAmount = objects.find((o) => o["#lookType"] === "mineral")?.mineralAmount ?? null;
  return row;
}

const onEdge = (p) => p.x === 0 || p.x === 49 || p.y === 0 || p.y === 49;
const engagementsOut = fs.openSync(join(out, "engagements.jsonl"), "w");
const allRooms = await shard.data.sMembers("rooms");
const plain = (o) => {
  const user = o["#user"] ?? o["#creep"]?.user;
  const type = o["#lookType"] === "tombstone" ? "tombstone" : o.body !== undefined ? "creep" : o.structureType;
  return { id: o.id, name: o.name ?? o["#creep"]?.name, room: o.pos.roomName, x: o.pos.x, y: o.pos.y, hits: o.hits, hitsMax: o.hitsMax, type, ours: user === ours, ttl: o["#creep"]?.ticksToLive };
};
const loaded = async (names) => (await Promise.all(names.map((n) => shard.loadRoom(n, time).catch(() => null)))).filter(Boolean);
const fights = engagements(
  (row) => fs.writeSync(engagementsOut, JSON.stringify(row) + "\n"),
  async (names = allRooms) => (await loaded(names)).flatMap((r) => [...r["#objects"]].map(plain)),
);
async function engaged(t, rooms) {
  const fight = JSON.parse((await loadUserMemoryString(shard, ours)) || "{}").v2?.fight;
  // An engagement's room may be one no sample follows.
  const seen = [...rooms, ...(await loaded([...new Set((fight?.engagements ?? []).map((e) => e.room))].filter((r) => !roomNames.includes(r))))];
  const struck = new Set(seen.flatMap((r) => (r["#eventLog"] ?? []).filter((v) => [ATTACK, HEAL].includes(v.event ?? v._$Variant)).map((v) => v.objectId)));
  await fights.tick(t, fight, seen.flatMap((r) => [...r["#objects"]].map(plain)), struck);
}

// The most ticks a body of ours stood on one tile carrying energy, acting on
// nothing and handed nothing.
let stillLoaded = 0;
const stills = new Map();
function stoodLoaded(rooms, time) {
  for (const o of rooms.flatMap((r) => [...r["#objects"]]))
    if (o.body !== undefined && o["#user"] === ours) {
      const tile = `${o.pos.roomName}:${o.pos.x},${o.pos.y}`;
      const energy = energyIn(o);
      const was = stills.get(o.name);
      const idle = energy > 0 && was?.tile === tile && was.energy === energy && !(o["#actionLog"] ?? []).some((a) => a.time === time);
      const ticks = idle ? was.ticks + 1 : 0;
      stills.set(o.name, { tile, energy, ticks });
      stillLoaded = Math.max(stillLoaded, ticks);
    }
}

// Crossings a body of ours made back across the border the tick after it
// landed, and the most consecutive ticks one carried a mineral it never
// delivered: a carrier at all, any body while it took energy aboard.
let bounced = 0;
let mineralHeld = 0;
let mineralAside = 0;
const lastTwo = new Map();
function wasted(rooms) {
  for (const o of rooms.flatMap((r) => [...r["#objects"]]))
    if (o.body !== undefined && o["#user"] === ours) {
      const [a, b] = lastTwo.get(o.name) ?? [];
      const energy = energyIn(o);
      const ore = (o.store?.getUsedCapacity() ?? 0) - energy;
      const kept = b && ore > 0 && ore >= b.ore;
      const now = { room: o.pos.roomName, edge: onEdge(o.pos), energy, ore, aside: kept ? b.aside + 1 : 0, took: kept && (b.took || energy > b.energy) };
      if (!o.body.some((p) => p.type === "work")) mineralHeld = Math.max(mineralHeld, now.aside);
      if (now.took) mineralAside = Math.max(mineralAside, now.aside);
      if (a && b && a.edge && b.edge && now.edge && a.room === now.room && b.room !== now.room) bounced++;
      if (b && b.room !== now.room && homes.includes(b.room)) lines[b.room].carriedOut += now.energy;
      lastTwo.set(o.name, [b, now]);
    }
}

// What our terminals sent, by resource then the room sent to. The engine
// lists a user's last 100 sends; a terminal sends once in 10 ticks, ten in a
// sample.
const sentOf = {};
const sentSeen = new Set();
async function sends() {
  for (const id of (await loadTransactionEntries(shard, ours)).outgoing.filter((id) => !sentSeen.has(id))) {
    sentSeen.add(id);
    const send = readTransaction(await loadTransactionBlob(shard, id));
    const of = (sentOf[send.resourceType] ??= {});
    of[send.to] = (of[send.to] ?? 0) + send.amount;
  }
}

// Keep asking for the pause until the first tick is through (`step.mjs`).
const spam = setInterval(() => svc.publish({ type: "pause" }), 20);
await svc.publish({ type: "pause" });
let time = await nextTick(120_000);
clearInterval(spam);
if (time === null) throw new Error("the engine ran no first tick in 120 s");
if (lost.delete(null)) lost.add(time);

const samplesOut = fs.openSync(join(out, "samples.jsonl"), "w");
const faultsOut = fs.openSync(join(out, "faults.jsonl"), "w");
const cpuOut = fs.openSync(join(out, "cpu.jsonl"), "w");
const decayOut = fs.openSync(join(out, "decay.jsonl"), "w");
const livesOut = fs.openSync(join(out, "bodies.jsonl"), "w");
const metricsOut = fs.openSync(join(out, "metrics.jsonl"), "w");
const seenFaults = new Set();
const levels = [];
const refusals = {};
let lastObserved = null;
let lastObs = null;
const started = Date.now();
// Steps that needed a second pausedTick: reported, never silent.
const slowTicks = [];
let stepAt = Date.now();
for (let t = 0; t < ticks; t++) {
  consoleTick = time;
  if (t > 0) {
    cpu = null; // a tick the runner skips reports no usage
    if (watchMs) await new Promise((r) => setTimeout(r, stepAt + watchMs - Date.now()));
    stepAt = Date.now();
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
  abandoned();
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
  if (fresh && obs.tick % 100 === 0) await readWindows(t);
  fs.writeSync(cpuOut, JSON.stringify({ t, time, cpu, observed: fresh ? obs.cpu : null, phases: fresh ? obs.phases : null, counts: fresh ? obs.counts : null }) + "\n");
  await followWorked(time);
  const rooms = await Promise.all(roomNames.map((name) => shard.loadRoom(name, time)));
  const room = rooms[0];
  account(rooms, time);
  const around = [...rooms, ...(await Promise.all(bordering.map((name) => shard.loadRoom(name, time))))];
  live(around, time);
  await engaged(t, rooms);
  stoodLoaded(rooms, time);
  wasted(around);
  for (const o of opponents.filter(({ raid: r }) => r && (t === r.first || (r.every && t > r.first && (t - r.first) % r.every === 0)))) {
    const into = o.raid.room ?? roomName;
    const toward = Object.entries(world.map.describeExits(into) ?? {}).filter(([, next]) => next === o.room).map(([dir]) => Number(dir));
    const bodies = o.raid.bodies.map(({ boosts = {}, ...counts }) => PARTS.flatMap((p) => Array(counts[p] ?? 0).fill({ type: p, boost: boosts[p] })));
    await pushIntentsForRoomNextTick(shard, into, o.id, { local: { soakRaid: [[o.id, bodies, toward.length ? toward : [1, 3, 5, 7], o.raid.inside ?? false]] }, internal: true });
    process.stderr.write(`t${t} raid of ${o.name} in ${into} from sides ${toward.join(",")}\n`);
  }
  for (const wave of waves.filter((w) => t >= w.first && (t - w.first) % w.every === 0)) {
    const exits = Object.entries(world.map.describeExits(wave.room) ?? {});
    const users = await Promise.all(exits.map(([, next]) => shard.loadRoom(next, time).then((r) => r["#user"], () => null)));
    const open = exits.filter((_, i) => users[i] == null).map(([dir]) => Number(dir));
    await pushIntentsForRoomNextTick(shard, wave.room, kInvaderUserId, { local: { soakWave: [[open, waveArg.split(",")]] }, internal: true });
    if (lines[wave.room]) lines[wave.room].waves++;
    process.stderr.write(`t${t} wave in ${wave.room} from sides ${open.join(",")}\n`);
  }
  for (const core of cores.filter((c) => t === c.at)) {
    await pushIntentsForRoomNextTick(shard, core.room, kInvaderUserId, { local: { soakCore: [[]] }, internal: true });
    process.stderr.write(`t${t} core in ${core.room}\n`);
  }
  if (room["#level"] !== levels.at(-1)?.rcl) levels.push({ t, time, rcl: room["#level"] });
  if (t % every === 0 || t === ticks - 1) {
    const row = sample(t, time, room);
    row.cpu = cpu;
    row.ledger = ledgerRow();
    const held = worthAndScore(rooms, ledger.stock);
    row.worth = held.worth;
    row.score = held.score;
    row.rooms = roomLines(rooms);
    row.others = {};
    for (const o of opponents) {
      const room = await shard.loadRoom(o.room, time);
      const objects = [...room["#objects"]].filter((x) => x["#user"] === o.id);
      const seat = [room, ...rooms.filter((r) => r.name !== o.room)];
      const { worth, score } = worthAndScore(seat, seat.filter((r) => r["#user"] === o.id).reduce((n, r) => n + stockOf([...r["#objects"]], o.id), 0), o.id);
      row.others[o.name] = {
        worth,
        score,
        rcl: room["#user"] === o.id ? room["#level"] : 0,
        spawns: objects.filter((x) => x.structureType === "spawn").length,
        creeps: objects.filter((x) => x.body !== undefined).length,
        home: room["#user"] === o.id ? summary(sample(t, time, room, o.id)) : null,
      };
    }
    if (homes.length > 1) row.homes = Object.fromEntries(homes.map((h, i) => [h, summary(sample(t, time, rooms[i]))]));
    row.botCpu = fresh ? obs.cpu : null;
    await sends();
    row.sent = { ...sentOf.energy };
    row.sentOf = structuredClone(sentOf);
    row.stillLoaded = stillLoaded;
    row.mineralAside = mineralAside;
    row.mineralHeld = mineralHeld;
    // Ore held in our storages and terminals: what the mines brought home.
    row.ore = rooms.flatMap((r) => [...r["#objects"]])
      .filter((o) => o["#user"] === ours && o.store && (o.structureType === "storage" || o.structureType === "terminal"))
      .reduce((n, o) => n + o.store.getUsedCapacity() - energyIn(o), 0);
    // Our bodies' parts carrying a boost, at most once each: what a lab's stock bought.
    row.boosted = rooms.flatMap((r) => [...r["#objects"]])
      .filter((o) => o.body !== undefined && o["#user"] === ours)
      .reduce((n, o) => n + o.body.filter((p) => p.boost !== undefined).length, 0);
    row.boostedWith = Math.max(0, ...rooms.flatMap((r) => [...r["#objects"]])
      .filter((o) => o.body !== undefined && o["#user"] === ours)
      .map((o) => new Set(o.body.flatMap((p) => p.boost ?? [])).size));
    // A declared claim's, or a room Strategy claimed, level, spawns, towers, the energy they hold and whether its safe mode runs: 0 until it is ours.
    const raised = roomNames.filter((name, i) => !homes.includes(name) && rooms[i]?.["#user"] === ours && rooms[i]["#level"] > 0);
    row.claimed = Object.fromEntries([...new Set([...claimed, ...raised])].map((name) => {
      const r = rooms[roomNames.indexOf(name)];
      const mine = r["#user"] === ours;
      const towers = mine ? [...r["#objects"]].filter((o) => o.structureType === "tower" && o["#user"] === ours && o.store) : [];
      return [name, { fire: towers.reduce((n, o) => n + energyIn(o), 0), towers: towers.length, safe: mine && r["#safeModeUntil"] > time, level: mine ? r["#level"] : 0, spawns: mine ? [...r["#objects"]].filter((o) => o.structureType === "spawn" && o["#lookType"] !== "constructionSite").length : 0 }];
    }));
    // Each compound our storages, terminals and labs hold.
    row.made = {};
    for (const o of rooms.flatMap((r) => [...r["#objects"]]))
      if (o["#user"] === ours && o.store && MADE_IN.includes(o.structureType))
        for (const r of COMPOUNDS) row.made[r] = (row.made[r] ?? 0) + (o.store.getUsedCapacity(r) ?? 0);
    row.bounced = bounced;
    row.faults = seenFaults.size;
    row.lost = lost.size;
    row.ms = Date.now() - started;
    fs.writeSync(samplesOut, JSON.stringify(row) + "\n");
    process.stderr.write(`t${t} rcl${row.rcl} ${row.progress} creeps ${row.creeps} (${row.ms} ms)\n`);
  }
}
for (const log of pileLog.values()) if (log.decayed > 0) pileOut(log);
for (const life of lives.values()) fs.writeSync(livesOut, JSON.stringify(life) + "\n");
await fights.end();
fs.writeFileSync(join(out, "refusals.json"), JSON.stringify(refusals, null, 2) + "\n");
fs.writeFileSync(join(out, "observe.json"), JSON.stringify(lastObs, null, 1) + "\n");
fs.writeFileSync(join(out, "levels.json"), JSON.stringify(levels) + "\n");
fs.writeFileSync(join(out, "slow.json"), JSON.stringify(slowTicks) + "\n");
abandoned();
fs.writeFileSync(join(out, "lost.json"), JSON.stringify([...lost].sort((a, b) => a - b)) + "\n");
// `engagements.log` as the bot left it, as sim writes it.
fs.writeFileSync(join(out, "segment4.jsonl"), (await segmentText(4)) ?? "");
console.log(JSON.stringify({ ticks, ms: Date.now() - started }));

if (!watchMs) await svc.publish({ type: "shutdown" });
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
process.exit(0);
