// Online, the server up: pause the engine, step it one tick at a time and
// watch one colony. Runs under xxscreeps' own loader, cwd the world:
//   xxscreeps.js file://…/soak-step.mjs <ticks> <every> <room[,room…]> <username> <out dir> [<waves> [<opponents.json> [<cores>]]]
// Every tick: our CPU (the runner's usage channel), our console, and the
// observe segment our bot writes (segment 1: faults, refusals). Every
// <every> ticks, and on the last: one sample of the room. Writes to <out dir>
// `samples.jsonl` (one sample a line), `faults.jsonl` (each fault once),
// `refusals.json` (counts by act and code), `lost.json` (the ticks lost),
// `levels.json` (the tick each controller level was first seen),
// `observe.json` (the last segment read), `cpu.jsonl` (tick, engine-reported
// CPU, the bot's own phases) and `console.log`. Each sample carries the
// room's energy ledger, cumulative from the first tick (`ledger`): dug,
// spawn regeneration, spent on bodies, building, upgrading, repairing and
// towers, lost to pile decay and to link sends, and the stock standing
// (stores, carried, piles); dug + regen = spent + decayed + linked + Δstock,
// less the residual it reports. `decay.jsonl`: each energy pile that
// decayed, once it is gone or the run ends, with its tile's role and the
// tombstone or ruin it fell out of. `bodies.jsonl`: each body, ours or not,
// once it is gone from the colony's rooms or the run ends: its parts, the ticks it
// was first and last seen, its ticks to live then, and its ticks by room and
// by action.
// The colony's rooms are <room> (several homes: each sample's `homes` holds
// one sample a home, the first's at its top) and the outposts its Memory declares
// (`v2.declarations.outposts`, read at the start): the ledger and the worth
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
// creeps. A raid { first, every?, bodies: [{ part: count, boosts?: { part:
// compound } }] } stands the opponent's bodies on <room>'s edge toward its
// room (`waves/`) at t = first, first + every, …
// <cores>: room:tick,… — a level-0 Invader core (`core/`) stood in room at t = tick.
import * as fs from "node:fs";
import { join } from "node:path";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import * as User from "xxscreeps/engine/db/user/index.js";
import { getServiceChannel } from "xxscreeps/engine/service/index.js";
import { getConsoleChannel, runnerUsageChannel } from "xxscreeps/engine/runner/model.js";
import { loadMemorySegmentBlob, loadUserMemoryString } from "xxscreeps/mods/meta/memory/model.js";
import { pushIntentsForRoomNextTick } from "xxscreeps/engine/processor/model.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { typedArrayToString } from "xxscreeps/utility/string.js";

const [ticksArg, everyArg, roomArg, username, out, wavesArg = "", opponentsPath, coresArg = ""] = process.argv.slice(2);
const homes = roomArg.split(",");
const roomName = homes[0];
const ticks = Number(ticksArg);
const every = Number(everyArg);
const observeSegment = 1;

const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const ours = await User.findUserByName(db, username);
const svc = getServiceChannel(shard);
const memory = JSON.parse((await loadUserMemoryString(shard, ours)) || "{}");
const roomNames = [...homes, ...(memory.v2?.declarations?.outposts ?? []).map((o) => o.room)];
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
(await getConsoleChannel(shard, ours).subscribe()).listen((message) => {
  let lines;
  try {
    lines = JSON.parse(message).map((m) => `${m.fd === 2 ? "ERR " : ""}${m.data}`);
  } catch {
    lines = [String(message)];
  }
  for (const line of lines)
    if (line.startsWith("#capture ")) fs.writeSync((captureLog ??= fs.openSync(join(out, "capture.jsonl"), "w")), line.slice(9) + "\n");
    else {
      if (/^ERR Script (timed out|was disposed)/.test(line)) lost.add(consoleTick);
      fs.writeSync(consoleLog, `[${consoleTick ?? "?"}] ${line}\n`);
    }
});
// Ticks lost: our bot's tick cut off by its time limit (its CPU or the
// host's load), or the engine abandoning intents in one of its rooms. A run
// holding one is no measurement. An opponent's room alone is left out: the
// engine abandons one now and then on a quiet host.
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
const world = await shard.loadWorld();
const roadCost = (room, pos) => {
  const t = world.map.getRoomTerrain(room).get(pos.x, pos.y);
  return t & 1 ? 150 * 300 : t & 2 ? 5 * 300 : 300;
};
function worthOf(rooms, stock) {
  const worth = { controller: 0, structures: 0, fortification: 0, sites: 0, stock };
  for (const room of rooms) for (const o of room["#objects"]) {
    const mine = o["#user"] === ours;
    const ownerless = o.structureType === "road" || o.structureType === "container";
    if (o["#lookType"] === "constructionSite") {
      if (mine) worth.sites += o.progress ?? 0;
    } else if (o.structureType === "controller") {
      if (!mine) continue;
      for (let l = 1; l < room["#level"]; l++) worth.controller += CONTROLLER_LEVELS[l];
      worth.controller += o["#progress"] ?? 0;
    } else if (o.structureType === "rampart" || o.structureType === "constructedWall") {
      if (mine || o.structureType === "constructedWall") worth.fortification += Math.floor(o.hits * REPAIR_COST);
    } else if (o.structureType !== undefined && (mine || ownerless)) {
      worth.structures += o.structureType === "road" ? roadCost(room.name, o.pos) : CONSTRUCTION_COST[o.structureType] ?? 0;
    }
  }
  return worth;
}

const born = new Set();
let lastPiles = new Map();
const pileLog = new Map();
const graves = new Map();
const lives = new Map();
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
const lines = Object.fromEntries(roomNames.map((r) => [r, { dug: 0, decayed: 0, build: 0, repair: 0, bodies: 0, reserved: 0, hostile: 0, waves: 0 }]));
function account(rooms, time) {
  ledger.regen += regenNext;
  regenNext = 0;
  const piles = new Map();
  let stock = 0;
  for (const room of rooms) {
    const line = lines[room.name];
    const objects = [...room["#objects"]];
    const sources = new Set(objects.filter((o) => o["#lookType"] === "source").map((o) => o.id));
    const towers = new Set(objects.filter((o) => o.structureType === "tower" && o["#user"] === ours).map((o) => o.id));
    const links = new Set(objects.filter((o) => o.structureType === "link" && o["#user"] === ours).map((o) => o.id));
    for (const e of room["#eventLog"] ?? []) {
      const kind = e.event ?? e._$Variant;
      if (kind === HARVEST && sources.has(e.targetId)) {
        ledger.dug += e.amount;
        line.dug += e.amount;
      } else if (kind === BUILD) {
        ledger.build += e.energySpent ?? 0;
        line.build += e.energySpent ?? 0;
      } else if (kind === UPGRADE) ledger.upgrade += e.energySpent ?? 0;
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
    let bank = 0;
    let hostile = false;
    const spawns = [];
    for (const o of objects) {
      if (o["#lookType"] === "resource") {
        if (o.resourceType !== "energy") continue;
        piles.set(o.id, o.amount);
        stock += o.amount;
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
      if (o.body !== undefined) {
        const key = `${o["#user"]} ${o.name}`;
        const life = lives.get(key) ?? { name: o.name, ours: o["#user"] === ours, body: o.body.map((p) => p.type[0]).join(""), cost, first: time, rooms: {}, acts: {} };
        lives.set(key, life);
        life.last = time;
        life.ttl = o["#ageTime"] - time;
        life.rooms[room.name] = (life.rooms[room.name] ?? 0) + 1;
        for (const a of (o["#actionLog"] ?? []).filter((a) => a.time === time)) life.acts[a.type] = (life.acts[a.type] ?? 0) + 1;
      }
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
      stock += energyIn(o);
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
  for (const [key, life] of lives)
    if (life.last !== time) {
      fs.writeSync(livesOut, JSON.stringify(life) + "\n");
      lives.delete(key);
    }
  for (const [id, log] of pileLog)
    if (!piles.has(id)) {
      if (log.decayed > 0) pileOut(log);
      pileLog.delete(id);
    }
  if (ledger.stock0 === null) ledger.stock0 = stock;
  ledger.stock = stock;
}

function roomLines(rooms) {
  return Object.fromEntries(
    rooms.map((room) => {
      const line = lines[room.name];
      const now = { creeps: 0, hostiles: 0, roads: 0, containers: 0, cores: 0 };
      for (const o of room["#objects"]) {
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

function sample(t, time, room) {
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
    built: {},
    cpu: null,
    bodies: [],
    foes: [],
  };
  for (const o of room["#objects"]) {
    const mine = o["#user"] === ours;
    // Our structures, and the roads and containers nobody owns, in our room.
    const ownerless = o.structureType === "road" || o.structureType === "container";
    if (o.structureType !== undefined && o["#lookType"] !== "constructionSite" && (mine || ownerless) && o.structureType !== "controller")
      row.built[o.structureType] = (row.built[o.structureType] ?? 0) + 1;
    // A site carries its structure's type and no store: read it first.
    if (o["#lookType"] === "constructionSite") {
      if (mine) row.sites++;
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
        row.bodies.push(`${o.name}@${o.pos.x},${o.pos.y} ${o.body.map((p) => p.type[0]).join("")} e${energyIn(o)} ${acts.join("+") || "-"}`);
      } else {
        row.hostiles++;
        row.foes.push(`${o.name}@${o.pos.x},${o.pos.y} ${o.body.map((p) => p.type[0]).join("")}`);
      }
    }
  }
  const objects = [...room["#objects"]];
  const atSource = (c) => objects.some((s) => s["#lookType"] === "source" && near(s.pos, c.pos, 1));
  const sourceContainers = objects.filter((o) => o.structureType === "container" && o["#lookType"] !== "constructionSite" && atSource(o));
  row.sourceContainers = sourceContainers.length;
  row.postedLinks = objects.filter((o) => o.structureType === "link" && o["#lookType"] !== "constructionSite" && sourceContainers.some((c) => near(c.pos, o.pos, 1))).length;
  return row;
}

// What `prediction.mjs` reads an engagement's outcome from; `cast`: the energy
// of the bodies it asked that stood in it; `core`: an Invader core was in its group.
const LETTER_COST = Object.fromEntries(Object.entries(PART_COST).map(([t, c]) => [t === "claim" ? "K" : t[0].toUpperCase(), c]));
const open = new Map();
const engagementsOut = fs.openSync(join(out, "engagements.jsonl"), "w");
const allRooms = await shard.data.sMembers("rooms");
async function standing(ids) {
  for (const name of allRooms) {
    const r = await shard.loadRoom(name, time).catch(() => null);
    if (r && [...r["#objects"]].some((o) => ids.has(o.id))) return true;
  }
  return false;
}
async function closed(e, t) {
  const { members, casts, ...rest } = e;
  fs.writeSync(engagementsOut, JSON.stringify({ ...rest, closed: t, outlived: t === null || (await standing(members)) }) + "\n");
}
async function engaged(t, time, rooms) {
  const fight = JSON.parse((await loadUserMemoryString(shard, ours)) || "{}").v2?.fight ?? {};
  const now = new Set();
  for (const e of fight.engagements ?? []) {
    now.add(e.id);
    const rec = open.get(e.id) ?? { id: e.id, room: e.room, opened: t, members: new Set(), answered: false, gaveUp: false, disengaged: false, contact: null, seen: null, casts: new Map(), cast: 0 };
    open.set(e.id, rec);
    for (const c of (fight.casts ?? []).filter((c) => c.engagement === e.id)) rec.casts.set(c.name, c.body);
    for (const n of e.ours.filter((n) => rec.casts.has(n))) {
      rec.cast += [...rec.casts.get(n)].reduce((sum, p) => sum + LETTER_COST[p], 0);
      rec.casts.delete(n);
    }
    for (const m of (fight.groups ?? []).find((g) => g.id === e.group)?.members ?? []) rec.members.add(m);
    rec.predicted = e.predicted;
    rec.dead = e.lost;
    rec.answered ||= !e.unanswered;
    rec.gaveUp ||= rec.answered && e.unanswered;
    rec.disengaged ||= e.phase === "disengaging";
    const objects = rooms.flatMap((r) => [...r["#objects"]]);
    const theirs = objects.filter((o) => rec.members.has(o.id));
    if (theirs.length) rec.seen = t;
    rec.core ||= theirs.some((o) => o.structureType === "invaderCore");
    const close = objects.some((o) => e.ours.includes(o.name) && o["#user"] === ours && theirs.some((h) => near(h.pos, o.pos, 3) && h.pos.roomName === o.pos.roomName));
    if (rec.contact === null && (close || theirs.some((h) => h.hits < h.hitsMax))) rec.contact = t;
  }
  for (const [id, rec] of open)
    if (!now.has(id)) {
      open.delete(id);
      await closed(rec, t);
    }
}

// Ticks our bodies lost beside a road: grounded past a step onto a tile off
// it but beside it (never a walk's own choice while the road is free), or a
// stop of at most 4 ticks on it ended by a step onto a tile a body held.
let roadLoss = 0;
const trails = new Map();
let onRoad = [];
function detours(rooms) {
  const objects = rooms.flatMap((r) => [...r["#objects"]]);
  const key = (p, dx = 0, dy = 0) => `${p.roomName}:${p.x + dx},${p.y + dy}`;
  const roads = new Set(objects.filter((o) => o.structureType === "road").map((o) => key(o.pos)));
  const besideRoad = (p) => [-1, 0, 1].some((dx) => [-1, 0, 1].some((dy) => roads.has(key(p, dx, dy))));
  for (const o of objects)
    if (o.body !== undefined && o["#user"] === ours) {
      const was = trails.get(o.name);
      const tile = key(o.pos);
      if (was?.tile === tile) {
        if (was.moved) was.wait = { tiles: roads.has(tile) ? onRoad.filter((b) => b.pos.roomName === o.pos.roomName && near(b.pos, o.pos, 1)).map((b) => key(b.pos)) : [], ticks: 1 };
        else if (was.wait) was.wait.ticks++;
        was.moved = false;
        continue;
      }
      if (was?.wait?.tiles.includes(tile) && was.wait.ticks <= 4) roadLoss += was.wait.ticks;
      const moves = o.body.filter((p) => p.type === "move" && p.hits > 0).length;
      if (!roads.has(tile) && besideRoad(o.pos)) roadLoss += Math.ceil(o.fatigue / (2 * Math.max(1, moves)));
      trails.set(o.name, { tile, moved: true, wait: null });
    }
  onRoad = objects.filter((o) => o.body !== undefined && roads.has(key(o.pos)));
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
  fs.writeSync(cpuOut, JSON.stringify({ t, time, cpu, observed: fresh ? obs.cpu : null, phases: fresh ? obs.phases : null }) + "\n");
  const rooms = await Promise.all(roomNames.map((name) => shard.loadRoom(name, time)));
  const room = rooms[0];
  account(rooms, time);
  await engaged(t, time, rooms);
  detours(rooms);
  for (const o of opponents.filter(({ raid: r }) => r && (t === r.first || (r.every && t > r.first && (t - r.first) % r.every === 0)))) {
    const toward = Object.entries(world.map.describeExits(roomName) ?? {}).filter(([, next]) => next === o.room).map(([dir]) => Number(dir));
    const bodies = o.raid.bodies.map(({ boosts = {}, ...counts }) => PARTS.flatMap((p) => Array(counts[p] ?? 0).fill({ type: p, boost: boosts[p] })));
    await pushIntentsForRoomNextTick(shard, roomName, o.id, { local: { soakRaid: [[o.id, bodies, toward.length ? toward : [1, 3, 5, 7]]] }, internal: true });
    process.stderr.write(`t${t} raid of ${o.name} in ${roomName} from sides ${toward.join(",")}\n`);
  }
  for (const wave of waves.filter((w) => t >= w.first && (t - w.first) % w.every === 0)) {
    const exits = Object.entries(world.map.describeExits(wave.room) ?? {});
    const users = await Promise.all(exits.map(([, next]) => shard.loadRoom(next, time).then((r) => r["#user"], () => null)));
    const open = exits.filter((_, i) => users[i] == null).map(([dir]) => Number(dir));
    await pushIntentsForRoomNextTick(shard, wave.room, kInvaderUserId, { local: { soakWave: [open] }, internal: true });
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
    row.worth = worthOf(rooms, ledger.stock);
    row.rooms = roomLines(rooms);
    row.others = {};
    for (const o of opponents) {
      const room = await shard.loadRoom(o.room, time);
      const objects = [...room["#objects"]].filter((x) => x["#user"] === o.id);
      row.others[o.name] = {
        rcl: room["#user"] === o.id ? room["#level"] : 0,
        spawns: objects.filter((x) => x.structureType === "spawn").length,
        creeps: objects.filter((x) => x.body !== undefined).length,
      };
    }
    if (homes.length > 1) row.homes = Object.fromEntries(homes.map((h, i) => [h, (({ bodies, foes, ...s }) => s)(sample(t, time, rooms[i]))]));
    row.botCpu = fresh ? obs.cpu : null;
    row.roadLoss = roadLoss;
    row.faults = seenFaults.size;
    row.lost = lost.size;
    row.ms = Date.now() - started;
    fs.writeSync(samplesOut, JSON.stringify(row) + "\n");
    process.stderr.write(`t${t} rcl${row.rcl} ${row.progress} creeps ${row.creeps} (${row.ms} ms)\n`);
  }
}
for (const log of pileLog.values()) if (log.decayed > 0) pileOut(log);
for (const life of lives.values()) fs.writeSync(livesOut, JSON.stringify(life) + "\n");
for (const rec of open.values()) await closed(rec, null);
fs.writeFileSync(join(out, "refusals.json"), JSON.stringify(refusals, null, 2) + "\n");
fs.writeFileSync(join(out, "observe.json"), JSON.stringify(lastObs, null, 1) + "\n");
fs.writeFileSync(join(out, "levels.json"), JSON.stringify(levels) + "\n");
fs.writeFileSync(join(out, "slow.json"), JSON.stringify(slowTicks) + "\n");
abandoned();
fs.writeFileSync(join(out, "lost.json"), JSON.stringify([...lost].sort((a, b) => a - b)) + "\n");
console.log(JSON.stringify({ ticks, ms: Date.now() - started }));

await svc.publish({ type: "shutdown" });
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
process.exit(0);
