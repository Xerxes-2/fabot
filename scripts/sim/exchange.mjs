// The exchange's generated set (ARCHITECTURE.md §10.1): random hostile
// compositions (bodies, boosts, count, terrain, our towers) against fixed
// defending sides, each duel run on krebs (`sim duel`, a world a duel)
// and set against what `Exchange.outcome` predicted for it, on the arm of the
// tactic a home's Defend fields.
//
// Usage: npm run exchange -- [--count N] [--seed N] [--ticks N] [--real 1] [--rollout R --theirs 1]
//   --count  duels (default 200)      --seed   the draw and the engine's (default 1)
//   --ticks  the longest a duel runs (default 400)
//   --real   our side runs the bot's tactics, the one its Defend fields
//   --rollout R --theirs 1   their side searches its scripts (`sim duel`'s PGS)
//
// Writes build/exchange/seed<N>/: samples.json, predictions.json,
// results.json, report.txt. A predicted Win or Kite that lost is an Exchange
// bug, one that timed out a miss; the report's bound is their counts and the
// error spread. Ticks count from first contact (two bodies within three), as
// `outcome`'s clocks do.
import { spawnSync } from "node:child_process";
import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { bugs, misses, outcome, report, stood } from "./prediction.mjs";
import { simBinary } from "./run.mjs";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const usage = "usage: npm run exchange -- [--count N] [--seed N] [--ticks N] [--real 1] [--rollout R --theirs 1]";
const fail = (message) => {
  console.error(message);
  process.exit(1);
};

const flags = { "--count": 200, "--seed": 1, "--ticks": 400, "--real": 0, "--rollout": 0, "--theirs": 0 };
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i += 2) {
  if (!(argv[i] in flags) || !/^\d+$/.test(argv[i + 1] ?? "")) fail(usage);
  flags[argv[i]] = Number(argv[i + 1]);
}
const { "--count": count, "--seed": seed, "--ticks": ticks } = flags;
const out = join(REPO, "build/exchange", `seed${seed}`);
rmSync(out, { recursive: true, force: true });
mkdirSync(out, { recursive: true });

// ---- the draw --------------------------------------------------------------
let state = seed >>> 0;
const random = () => {
  state = (state + 0x6d2b79f5) >>> 0;
  let t = state;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};
const between = (a, b) => a + Math.floor(random() * (b - a + 1));
const pick = (xs) => xs[Math.floor(random() * xs.length)];
const repeat = (part, n) => Array(n).fill(part);

const COMPOUNDS = {
  tough: ["GO", "GHO2", "XGHO2"],
  attack: ["UH", "UH2O", "XUH2O"],
  ranged_attack: ["KO", "KHO2", "XKHO2"],
  heal: ["LO", "LHO2", "XLHO2"],
  move: ["ZO", "ZHO2", "XZHO2"],
  work: ["ZH", "ZH2O", "XZH2O"],
};
const KINDS = {
  melee: () => ({ attack: between(2, 8) }),
  ranged: () => ({ ranged_attack: between(1, 6) }),
  healer: () => ({ heal: between(1, 5) }),
  skirmisher: () => ({ ranged_attack: between(1, 4), heal: between(1, 3) }),
  tank: () => ({ tough: between(2, 6), attack: between(2, 6) }),
  dismantler: () => ({ work: between(2, 8) }),
  brawler: () => ({ attack: between(1, 2), ranged_attack: between(3, 6) }),
  kiter: () => ({ tough: between(1, 3), ranged_attack: between(2, 5), heal: between(1, 4) }),
};
const KITER = ["tough", "ranged_attack", "heal"];
// Head first as a body is cast: TOUGH, MOVE, the rest, HEAL last.
const ORDER = ["tough", "move", "work", "attack", "ranged_attack", "heal"];
// The Invader's AI drives a body with HEAL as a healer, which with no one
// hurt suicides: its raids strike.
const INVADERS = ["melee", "ranged", "tank", "brawler"];
function hostile(kinds) {
  const kind = pick(kinds);
  const counts = KINDS[kind]();
  const weight = Object.values(counts).reduce((a, b) => a + b, 0);
  counts.move = pick([weight, Math.ceil(weight / 2), Math.max(1, Math.floor(weight / 4))]);
  const parts = ORDER.flatMap((p) => repeat(p, counts[p] ?? 0));
  const tier = random() < 0.3 ? between(0, 2) : null;
  const tiers = kind === "kiter" ? Object.fromEntries(KITER.map((p) => [p, pick([null, 0, 1, 2])])) : {};
  const boosts = parts.map((p) => (tier !== null ? COMPOUNDS[p][tier] : tiers[p] != null ? COMPOUNDS[p][tiers[p]] : ""));
  return { parts, boosts, hits: 100 * parts.length };
}

// The defending side, fixed: a guard and a ranger, or two rangers, which
// alone can kite.
const GUARD = [...repeat("tough", 2), ...repeat("move", 7), ...repeat("attack", 5)];
const RANGER = [...repeat("move", 5), ...repeat("ranged_attack", 4), "heal"];
const OURS = [
  [GUARD, RANGER],
  [RANGER, RANGER],
];
const ROW = 30;
// Our side backs off from the raid toward y 48, or into the pocket: a
// channel x 20-30 from y 22, closed at y 37.
const TERRAINS = {
  plain: { cell: "0", pocket: false, weight: 2 },
  swamp: { cell: "2", pocket: false, weight: 10 },
  pocket: { cell: "0", pocket: true, weight: 2 },
};
function groundOf(kind) {
  const { cell, pocket } = TERRAINS[kind];
  const wall = (x, y) => x === 0 || y === 0 || x === 49 || y === 49 || (pocket && y >= 22 && (x < 20 || x > 30 || y >= 37));
  return Array.from({ length: 50 }, (_, y) => Array.from({ length: 50 }, (_, x) => (wall(x, y) ? "1" : cell)).join(""));
}
// A room is ours (our controller, so our towers fire) or, one in ten, theirs;
// its owner's towers stand and its bodies may start on its ramparts. A raid
// in our room is, one in four, the NPC Invader's, run by the engine's AI.
const sided = (owner, side, bodies, row) => ({
  bodies,
  towers: owner === side ? Array.from({ length: between(0, 2) }, (_, k) => ({ x: 20 + 8 * k, y: row, energy: 1000 })) : [],
  ramparts: owner === side ? bodies.filter(() => random() < 0.3).map((b) => ({ x: b.x, y: b.y, hits: 1000 * between(1, 30) })) : [],
});

const samples = Array.from({ length: count }, (_, i) => {
  const terrain = pick(Object.keys(TERRAINS));
  const owner = random() < 0.1 ? "theirs" : "ours";
  const invader = owner === "ours" && random() < 0.25;
  const kinds = invader ? INVADERS : Object.keys(KINDS);
  const theirs = Array.from({ length: between(1, 4) }, () => hostile(kinds)).map((b, k) => ({ ...b, invader, x: 21 + 2 * k, y: 15 }));
  const behind = i % 4 >= 2 ? (TERRAINS[terrain].pocket ? 6 : 14) : 0;
  const ours = OURS[i % 2].map((parts, k) => ({ parts, boosts: parts.map(() => ""), hits: 100 * parts.length, x: 24 + 2 * k, y: ROW + k * behind }));
  return {
    id: `s${i}`,
    room: "E21S21",
    kind: terrain,
    owner,
    ground: groundOf(terrain),
    controller: { x: 2, y: 2 },
    terrain: { weight: TERRAINS[terrain].weight, runOff: (TERRAINS[terrain].pocket ? 37 : 49) - 1 - ROW },
    ours: sided(owner, "ours", ours, 34),
    theirs: sided(owner, "theirs", theirs, 11),
  };
});
writeFileSync(join(out, "samples.json"), JSON.stringify(samples));

// ---- the predictions and the duels -----------------------------------------
const built = spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "exchange", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: ["ignore", "ignore", "inherit"] });
if (built.status !== 0) fail("the exchange tool did not build");
const run = (bin, args = [], input = samples) => {
  const res = spawnSync(bin, args, { input: JSON.stringify(input), encoding: "utf8", maxBuffer: 1 << 28 });
  if (res.status !== 0) fail(`${bin} ${args.join(" ")} failed: ${res.stderr}`);
  return JSON.parse(res.stdout);
};
const predictions = Object.fromEntries(run(join(REPO, "target/release/exchange")).map((p) => [p.id, p]));
writeFileSync(join(out, "predictions.json"), JSON.stringify(predictions));
const sim = simBinary();
const began = performance.now();
const forwarded = ["--real", "--rollout", "--theirs"].flatMap((f) => (flags[f] ? [f, String(flags[f])] : []));
const fielded = samples.map((s) => ({ ...s, tactic: predictions[s.id].tactic }));
const actual = run(sim, ["duel", "--ticks", String(ticks), "--seed", String(seed), ...forwarded], fielded);
const ms = performance.now() - began;
console.error(`${count} duels in ${Math.round(ms)} ms: ${(ms / count).toFixed(2)} ms a duel, ${Math.round((count * 60000) / ms)} a minute`);
writeFileSync(join(out, "results.json"), JSON.stringify(actual));

// ---- the report ------------------------------------------------------------
const rows = samples.map((s) => ({ s, p: predictions[s.id], a: actual[s.id] }));
const describe = ({ s, p, a }) =>
  `  ${s.id} ${s.kind}: ${s.theirs.bodies.map((b) => b.parts.length + (b.boosts.some(Boolean) ? "*" : "") + b.parts.filter((x) => x !== "move").map((x) => x[0]).join("")).join(" ")}` +
  ` ${s.owner === "ours" ? "" : "their room, "}${s.theirs.bodies[0].invader ? "Invader, " : ""}towers ${s[s.owner].towers.length}, ramparts ${s[s.owner].ramparts.length}; ${p.tactic}, predicted ${p.kind} ${p.ticks}t, ${outcome(a)}${a.ticks === null ? "" : ` ${a.ticks}t`}`;
const lines = [
  `exchange generated set: ${count} duels, seed ${seed}, ${ticks} ticks at most${forwarded.length ? `, ${forwarded.join(" ")}` : ""}`,
  ...report(rows),
  `predicted Lose by the tactic fielded: ${Object.entries(Object.groupBy(rows.filter(({ p }) => p.kind === "lose"), ({ p, a }) => `lose→${p.tactic} ${outcome(a)}`)).sort().map(([k, xs]) => `${k} ${xs.length}`).join(", ")}`,
  "bugs:",
  ...bugs(rows).map(describe),
  "predicted Win/Kite that timed out:",
  ...stood(rows).map(describe),
  "predicted Lose that won:",
  ...misses(rows).map(describe),
];
writeFileSync(join(out, "report.txt"), lines.join("\n") + "\n");
console.log(lines.join("\n"));
