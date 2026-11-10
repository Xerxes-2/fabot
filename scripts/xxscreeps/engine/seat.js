// Read off the machine, a seated bot's CPU, bucket and heap throttle it by how
// loaded the machine is, and its Math.random is unseeded.
let config = {};
try {
  config = require("opponent");
} catch {}

// mulberry32, as `engine/seed` seeds the processor.
let state = (config.seed ?? 1) >>> 0;
Math.random = () => {
  state = (state + 0x6d2b79f5) >>> 0;
  let t = state;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};
// The engine's lodash took Math.random before this file ran.
// shortcut: integer `_.random(min, max)` and one-draw `_.sample` only, the forms both bots call.
_.random = (min = 0, max = 1) => min + Math.floor(Math.random() * (max - min + 1));
_.sample = (collection) => {
  const values = _.values(collection);
  return values[Math.floor(Math.random() * values.length)];
};
_.shuffle = (collection) => {
  const values = _.values(collection);
  for (let i = values.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [values[i], values[j]] = [values[j], values[i]];
  }
  return values;
};

// Hivemind spends some 0.06 ms between two readings (measured); a fixed
// reading would never end the loops it bounds by CPU spent.
const STEP = 0.1;
// Game.cpu is a new object each tick: pinned before the bot reads it.
const pin = () => {
  let used = 0;
  Object.assign(Game.cpu, {
    bucket: 10000,
    tickLimit: 500,
    getUsed: () => (used += STEP),
    getHeapStatistics: () => ({ heap_size_limit: 256 * 2 ** 20, used_heap_size: 0, externally_allocated_size: 0 }),
  });
};
pin();

module.exports = { config, pin };
