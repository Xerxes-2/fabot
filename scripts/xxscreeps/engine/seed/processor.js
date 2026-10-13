// A seeded Math.random for the processor (#543): xxscreeps breaks a tie
// between two moves of equal power at random (`engine/processor/movement.js`),
// which made two runs of one scenario part by tick 2. The seed is
// `<world>/seed.txt`.
import * as fs from "node:fs";
import { RoomProcessor } from "xxscreeps/engine/processor/room.js";
import { Game } from "xxscreeps/game/index.js";

const seed = Number(fs.readFileSync("seed.txt", "utf8")) >>> 0;

// Rooms are processed, and each player's intents in a room applied, in the
// order their runners published them, which timing changes run to run: so a
// room draws from its own sequence each tick, and its players act in user-id order.
const sequences = new Map();
let tick = -1;
const sequenceOf = () => {
  const key = Game ? `${Object.keys(Game.rooms).join()}:${Game.time}` : "";
  if (Game && Game.time !== tick) {
    sequences.clear();
    tick = Game.time;
  }
  if (!sequences.has(key)) sequences.set(key, { state: [...key].reduce((h, c) => Math.imul(h ^ c.charCodeAt(0), 0x01000193) >>> 0, seed ^ 0x811c9dc5) });
  return sequences.get(key);
};

// mulberry32
Math.random = () => {
  const s = sequenceOf();
  s.state = (s.state + 0x6d2b79f5) >>> 0;
  let t = s.state;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

const processRoom = RoomProcessor.prototype.process;
RoomProcessor.prototype.process = function (...args) {
  this.intents = new Map([...this.intents].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)));
  return processRoom.apply(this, args);
};

// A body crossing into a room is an intent its old room sent, and the rooms
// sending to one room finish in the order timing gives: the new room takes
// them in their content's order.
const swap = (target, key, value) => new Proxy(target, { get: (t, k) => (k === key ? value : typeof t[k] === "function" ? t[k].bind(t) : t[k]) });
const finalizeRoom = RoomProcessor.prototype.finalize;
RoomProcessor.prototype.finalize = async function (...args) {
  const shard = this.shard;
  this.shard = swap(shard, "scratch", swap(shard.scratch, "lRange", async (...range) => (await shard.scratch.lRange(...range)).sort()));
  try {
    return await finalizeRoom.apply(this, args);
  } finally {
    this.shard = shard;
  }
};
