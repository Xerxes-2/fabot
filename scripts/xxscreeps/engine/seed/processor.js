// A seeded Math.random for the processor (#543): xxscreeps breaks a tie
// between two moves of equal power at random (`engine/processor/movement.js`),
// which made two runs of one scenario part by tick 2. The seed is
// `<world>/seed.txt`; with one processor thread the sequence is the run's.
import * as fs from "node:fs";

let state = Number(fs.readFileSync("seed.txt", "utf8")) >>> 0;
// mulberry32
Math.random = () => {
  state = (state + 0x6d2b79f5) >>> 0;
  let t = state;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};
