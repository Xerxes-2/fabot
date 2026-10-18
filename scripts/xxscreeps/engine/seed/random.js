import * as fs from "node:fs";

const seed = Number(fs.readFileSync("seed.txt", "utf8")) >>> 0;

// A sequence of `<world>/seed.txt` and `key` (FNV-1a), drawn by mulberry32.
export const sequence = (key) => ({ state: [...key].reduce((h, c) => Math.imul(h ^ c.charCodeAt(0), 0x01000193) >>> 0, seed ^ 0x811c9dc5) });
export const draw = (s) => {
  s.state = (s.state + 0x6d2b79f5) >>> 0;
  let t = s.state;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};
