// A seeded Math.random for the processor (#543): xxscreeps breaks a tie
// between two moves of equal power at random (`engine/processor/movement.js`),
// which made two runs of one scenario part by tick 2. The seed is
// `<world>/seed.txt`.
import { RoomProcessor } from "xxscreeps/engine/processor/room.js";
import { Game } from "xxscreeps/game/index.js";
import { draw, sequence } from "./random.js";
import { Room } from "xxscreeps/game/room/index.js";
import { StructureController } from "xxscreeps/mods/classic/controller/controller.js";

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
  if (!sequences.has(key)) sequences.set(key, sequence(key));
  return sequences.get(key);
};
Math.random = () => draw(sequenceOf());

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

// The official engine carries a body across a border with its fatigue
// (`processor/intents/creeps/tick.js`), but a step onto the border from
// inside costs none (`movement.js`); xxscreeps zeroes it on the carry
// instead, which hides a body stepping along the border bouncing on it.
const edge = (p) => p.x === 0 || p.y === 0 || p.x === 49 || p.y === 49;
const steppedOn = new Set();
const moveObject = Room.prototype["#moveObject"];
Room.prototype["#moveObject"] = function (object, pos) {
  if (edge(pos) && !edge(object.pos)) steppedOn.add(object.id);
  else steppedOn.delete(object.id);
  return moveObject.call(this, object, pos);
};
const carried = new Map();
const removeObject = Room.prototype["#removeObject"];
Room.prototype["#removeObject"] = function (object) {
  if (object.fatigue > 0 && !steppedOn.has(object.id)) carried.set(object.id, object.fatigue);
  steppedOn.delete(object.id);
  return removeObject.call(this, object);
};
const insertObject = Room.prototype["#insertObject"];
Room.prototype["#insertObject"] = function (object, ...rest) {
  if (carried.has(object.id)) (object.fatigue = carried.get(object.id)), carried.delete(object.id);
  return insertObject.call(this, object, ...rest);
};

// A tap that runs a controller's timer out downgrades it in the official
// engine (`controllers/tick.js`); xxscreeps throws on the overdue time.
const ticksToDowngrade = Object.getOwnPropertyDescriptor(StructureController.prototype, "ticksToDowngrade").get;
Object.defineProperty(StructureController.prototype, "ticksToDowngrade", {
  get() {
    if (this["#downgradeTime"] > 0 && this["#downgradeTime"] <= Game.time) this["#downgradeTime"] = Game.time + 1;
    return ticksToDowngrade.call(this);
  },
});
