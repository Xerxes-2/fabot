// Wrap named functions of a built bundle with `Game.cpu` timers, for a live
// probe the local ruler cannot take: the engine's own clock, on the real
// colonies. The wrapped bundle sums each function's ms and call count over
// a window of ticks after the global reset and writes the sums to
// `Memory.__probe` every tick of the window; `observe.mjs probe` reads them
// back as ms/tick and calls/tick per colony:function.
//
// Each named function is renamed `<fn>__orig` and a wrapper of the original
// name is appended, so every call site in the bundle goes through it. Time
// spent inside `decideUnarbitrated` is charged to the colony it decides for;
// time outside any colony to `-`. `loop` is always wrapped, as `-:tick`.
//
// A name the bundle does not declare exactly once at the top level is a
// failure, never a skip: a probe that silently measured fewer functions
// than asked would read as those functions costing nothing.
//
// This is a throwaway bundle. Upload it, read the window, then upload a
// clean build; never commit or deploy the output as a release.
//
// Usage: probe.mjs [in.js] [--out out.js] [--fns a,b,c] [--from N] [--ticks N]
//   in.js    the built bundle, default dist/main.js
//   --out    where the wrapped bundle goes, default dist/main.js
//   --fns    the functions to time, default the decide sub-phases below
//   --from   ticks after a reset before the window opens, default 20
//   --ticks  the window's length, default 300
import { readFileSync, writeFileSync } from "node:fs";

const usage = "usage: probe.mjs [in.js] [--out out.js] [--fns a,b,c] [--from N] [--ticks N]";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

// The decide sub-phases as of the 2026-09-30 CPU round.
const DEFAULT_FNS = `signaturesOf ofViewRecalling planLayout haulerDemandOf threatsOf planOutpostContainers
planSafeMode planFire planTowerHeal planConsignment outpostFactsOf rowSizingOf heldTaskFacts planTasks
planPool evictFarFieldsExcept planSpawns matchCreeps assignedTasks emit planSignatures movementOf
resolveRooms travelCostOf walkTicks areaFor evictRooms joinedAcross ColonyViewModule_ofWorldRecalling
WorldModule_scanRecalling WorldModule_harassCasters`.split(/\s+/);

const VALUED = ["--out", "--fns", "--from", "--ticks"];
const flags = {};
const positional = [];
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++) {
  if (VALUED.includes(argv[i])) {
    flags[argv[i]] = argv[++i];
    if (flags[argv[i - 1]] === undefined || flags[argv[i - 1]].startsWith("--")) fail(`${argv[i - 1]} needs a value\n${usage}`);
  } else if (argv[i].startsWith("--")) {
    fail(usage);
  } else {
    positional.push(argv[i]);
  }
}
if (positional.length > 1) fail(usage);
const input = positional[0] ?? "dist/main.js";
const output = flags["--out"] ?? "dist/main.js";
const count = (flag, fallback) => {
  if (flags[flag] === undefined) return fallback;
  const n = Number(flags[flag]);
  if (!(Number.isInteger(n) && n >= 0)) fail(`${flag} takes a whole number\n${usage}`);
  return n;
};
const from = count("--from", 20);
const window = count("--ticks", 300);
if (window === 0) fail(`--ticks must be at least 1\n${usage}`);
const fns = flags["--fns"] ? flags["--fns"].split(",").filter(Boolean) : DEFAULT_FNS;
for (const fn of fns) {
  if (!/^[A-Za-z_$][\w$]*$/.test(fn)) fail(`${fn} is not a function name`);
  if (fn === "loop" || fn === "decideUnarbitrated") fail(`${fn} is always wrapped; leave it out of --fns`);
}

let source;
try {
  source = readFileSync(input, "utf8");
} catch (err) {
  fail(`cannot read ${input}: ${err.message} — run npm run build first`);
}
if (source.includes("__orig(")) fail(`${input} is already wrapped; probe a clean build`);

// Every anchor is checked before any is renamed, so one run names every
// missing function rather than the first.
const declarations = (fn) => source.split(`\nfunction ${fn}(`).length - 1;
const anchors = [...fns, "decideUnarbitrated", "loop", "SpatialInfoModule_homeName"];
const missing = anchors.filter((fn) => declarations(fn) !== 1);
if (missing.length > 0) {
  fail(
    `${input} does not declare exactly once: ${missing.map((fn) => `${fn} (${declarations(fn)})`).join(", ")}. ` +
      "A function was renamed, inlined or split; pass --fns with the names the bundle has now.",
  );
}

let patched = source;
for (const fn of [...fns, "decideUnarbitrated", "loop"]) {
  patched = patched.replace(`\nfunction ${fn}(`, `\nfunction ${fn}__orig(`);
}

const wrappers = fns.map(
  (fn) =>
    `function ${fn}(...a) { const P = globalThis.__P; if (!P || !P.on) return ${fn}__orig(...a); ` +
    `const t0 = Game.cpu.getUsed(); try { return ${fn}__orig(...a); } ` +
    `finally { P.add(P.cur + ':${fn}', Game.cpu.getUsed() - t0); } }`,
);
wrappers.push(
  `function decideUnarbitrated(...a) { const P = globalThis.__P; if (!P || !P.on) return decideUnarbitrated__orig(...a); ` +
    `const prev = P.cur; P.cur = SpatialInfoModule_homeName(a[0].Spatial); const t0 = Game.cpu.getUsed(); ` +
    `try { return decideUnarbitrated__orig(...a); } finally { P.add(P.cur + ':decide', Game.cpu.getUsed() - t0); P.cur = prev; } }`,
);
wrappers.push(`function loop() {
  const P = globalThis.__P ??= { n: 0, acc: {}, cur: '-', on: false, add(k, v) { const e = this.acc[k] ??= [0, 0]; e[0] += v; e[1]++; } };
  P.n++; P.on = P.n > ${from} && P.n <= ${from + window}; P.cur = '-';
  const t0 = Game.cpu.getUsed();
  try { return loop__orig(); } finally {
    if (P.on) {
      P.add('-:tick', Game.cpu.getUsed() - t0);
      const out = {};
      for (const [k, [ms, c]] of Object.entries(P.acc)) out[k] = [+ms.toFixed(2), c];
      Memory.__probe = { ticks: P.n - ${from}, window: ${window}, to: Game.time, fns: out };
    }
  }
}`);

writeFileSync(output, `${patched}\n${wrappers.join("\n")}\n`);
console.log(
  `wrapped ${fns.length + 2} functions into ${output}: ticks ${from + 1}-${from + window} after a reset ` +
    "write Memory.__probe; read it with `npm run observe -- probe`",
);
