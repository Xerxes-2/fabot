// Behaviour parity of two bundles on one capture: the same tick, in, must
// produce the same intents, the same Memory and the same recorder ring, out
// (`ARCHITECTURE.md` §10.3). A capture already carries the whole World of every
// tick, so this is the decision boundary itself, replayed tick for tick; the
// first tick the two disagree on is the one to diff. A fault on either side
// fails the run, so "the two agree" never hides a fault both make.
//
// Usage: npm run parity -- --a base.js --b new.js [--limit N] capture.jsonl…
import { createReadStream } from "node:fs";
import { createRequire } from "node:module";
import { resolve } from "node:path";
import { createInterface } from "node:readline";
import { getHeapStatistics } from "node:v8";
import { stand } from "../bench-game.mjs";

const usage = "usage: npm run parity -- --a A.js --b B.js [--limit N] capture.jsonl…";
const flags = {};
const files = [];
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++) {
  if (argv[i] === "--a" || argv[i] === "--b" || argv[i] === "--limit") flags[argv[i]] = argv[++i];
  else if (argv[i].startsWith("--")) {
    console.error(usage);
    process.exit(1);
  } else files.push(argv[i]);
}
if (!flags["--a"] || !flags["--b"] || files.length === 0) {
  console.error(usage);
  process.exit(1);
}
const limit = flags["--limit"] === undefined ? Infinity : Number(flags["--limit"]);

async function captures() {
  const ticks = [];
  for (const file of files)
    for await (const line of createInterface({ input: createReadStream(file), crlfDelay: Infinity })) {
      if (!line) continue;
      const tick = JSON.parse(line);
      for (const r of tick.rooms) r.terrain = Uint8Array.from(r.terrain, Number);
      ticks.push(tick);
      if (ticks.length >= limit) return ticks;
    }
  return ticks;
}

const RECORDER = [10, 11, 12, 13, 14];
const ticks = await captures();
const require = createRequire(import.meta.url);

function replay(path) {
  delete require.cache[path];
  const bot = require(path);
  const out = [];
  for (const tick of ticks) {
    const intents = [];
    let start;
    const game = stand(tick, intents, getHeapStatistics, () => performance.now() - start);
    start = performance.now();
    bot.loop();
    const segment = game.segments[1] === undefined ? "{}" : game.segments[1];
    const observed = JSON.parse(segment);
    out.push({
      intents,
      acts: JSON.stringify(intents),
      memory: game.memory(),
      recorded: RECORDER.map((id) => game.segments[id] ?? "").join("\u0000"),
      faults: (observed.faults ?? []).filter((f) => f.tick === tick.time),
      observe: { faults: (observed.faults ?? []).length, refusals: (observed.refusals ?? []).length },
    });
  }
  return out;
}

const sides = { A: replay(resolve(flags["--a"])), B: replay(resolve(flags["--b"])) };

const firstDifference = (a, b) => {
  let i = 0;
  while (
    i < a.length &&
    i < b.length &&
    a[i].acts === b[i].acts &&
    a[i].memory === b[i].memory &&
    a[i].recorded === b[i].recorded
  )
    i++;
  return i;
};

for (const [name, side] of Object.entries(sides)) {
  const faulted = side.findIndex((tick) => tick.faults.length);
  if (faulted < 0) continue;
  console.log(`parity: ${name} faulted at the tick ${ticks[faulted].time}`);
  console.log(`  ${JSON.stringify(side[faulted].faults)}`);
  process.exit(1);
}

const at = firstDifference(sides.A, sides.B);
if (at === Math.min(sides.A.length, sides.B.length)) {
  console.log(`parity: identical over ${ticks.length} ticks (${flags["--a"]} vs ${flags["--b"]})`);
  process.exit(0);
}

const tick = ticks[at];
const a = sides.A[at];
const b = sides.B[at];
console.log(`parity: the tick ${tick.time} (line ${at + 1} of ${ticks.length}) differs`);
if (a.acts !== b.acts) {
  const x = a.intents;
  const y = b.intents;
  let i = 0;
  while (i < x.length && i < y.length && JSON.stringify(x[i]) === JSON.stringify(y[i])) i++;
  console.log(`  intents: A ${x.length}, B ${y.length}; first difference at ${i}`);
  console.log(`    A ${JSON.stringify(x[i])}`);
  console.log(`    B ${JSON.stringify(y[i])}`);
} else {
  console.log("  intents: identical");
}
if (a.memory !== b.memory) {
  let i = 0;
  while (i < a.memory.length && i < b.memory.length && a.memory[i] === b.memory[i]) i++;
  console.log(`  memory: A ${a.memory.length} chars, B ${b.memory.length}; first difference at ${i}`);
  console.log(`    A …${a.memory.slice(Math.max(0, i - 60), i + 120)}`);
  console.log(`    B …${b.memory.slice(Math.max(0, i - 60), i + 120)}`);
} else {
  console.log("  memory: identical");
}
if (a.recorded !== b.recorded) {
  const x = a.recorded.split("\u0000");
  const y = b.recorded.split("\u0000");
  const ring = RECORDER.findIndex((_, i) => x[i] !== y[i]);
  console.log(`  recorder: segment ${RECORDER[ring]} differs`);
  let i = 0;
  while (i < x[ring].length && i < y[ring].length && x[ring][i] === y[ring][i]) i++;
  console.log(`    A …${x[ring].slice(Math.max(0, i - 60), i + 120)}`);
  console.log(`    B …${y[ring].slice(Math.max(0, i - 60), i + 120)}`);
}
for (const [name, side] of Object.entries({ A: a, B: b })) {
  if (side.faults.length) console.log(`  ${name} faults: ${JSON.stringify(side.faults)}`);
}
process.exit(1);
