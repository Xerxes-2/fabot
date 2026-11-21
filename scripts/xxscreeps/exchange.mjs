// The exchange's generated set (ARCHITECTURE.md §10.1): random hostile
// compositions (bodies, boosts, count, terrain, our towers) against fixed
// defending sides, each duel run on xxscreeps and set against what
// `Exchange.outcome` predicted for it. Both sides run `engine/duel/main.js`.
//
// Usage: npm run exchange -- [--count N] [--seed N] [--batch N] [--ticks N]
//   --count  duels (default 200)      --seed   the draw and the engine's (default 1)
//   --batch  duels a world (default 50) --ticks the longest a duel runs (default 400)
//
// Needs `npm run xx -- setup`. Writes build/exchange/seed<N>/: samples.json,
// predictions.json, results.json, report.txt. A predicted Win or Kite that
// lost is an Exchange bug, one that timed out a miss; the report's bound is
// their counts and the error spread. Ticks count from first contact (two
// bodies within three), as `outcome`'s clocks do.
import { spawn, spawnSync } from "node:child_process";
import { cpSync, existsSync, lstatSync, mkdirSync, openSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { bugs, misses, outcome, report, stood } from "./prediction.mjs";
import { sweep } from "./prune.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const CACHE = process.env.FABOT_XXSCREEPS_HOME ?? join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "fabot-xxscreeps");
const ENGINE = join(CACHE, "engine");
const TEMPLATE = join(ENGINE, "template");
const XXSCREEPS = join(ENGINE, "node_modules/xxscreeps/xxscreeps.js");
const usage = "usage: npm run exchange -- [--count N] [--seed N] [--batch N] [--ticks N]";
const fail = (message) => {
  console.error(message);
  process.exit(1);
};

const flags = { "--count": 200, "--seed": 1, "--batch": 50, "--ticks": 400 };
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i += 2) {
  if (!(argv[i] in flags) || !/^\d+$/.test(argv[i + 1] ?? "")) fail(usage);
  flags[argv[i]] = Number(argv[i + 1]);
}
const { "--count": count, "--seed": seed, "--batch": batch, "--ticks": ticks } = flags;
if (!existsSync(join(TEMPLATE, "screeps/shard0/data.json"))) fail(`no template world at ${TEMPLATE}: npm run xx -- setup first`);
const out = join(REPO, "build/exchange", `seed${seed}`);
rmSync(out, { recursive: true, force: true });
mkdirSync(out, { recursive: true });
sweep(join(REPO, "soak-runs"));

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
  const room = `E${20 + (i % batch) % 10}S${20 + Math.floor((i % batch) / 10)}`;
  const terrain = pick(Object.keys(TERRAINS));
  const owner = random() < 0.1 ? "theirs" : "ours";
  const invader = owner === "ours" && random() < 0.25;
  const kinds = invader ? INVADERS : Object.keys(KINDS);
  const theirs = Array.from({ length: between(1, 4) }, () => hostile(kinds)).map((b, k) => ({ ...b, invader, x: 21 + 2 * k, y: 15 }));
  const behind = i % 4 >= 2 ? (TERRAINS[terrain].pocket ? 6 : 14) : 0;
  const ours = OURS[i % 2].map((parts, k) => ({ parts, boosts: parts.map(() => ""), hits: 100 * parts.length, x: 24 + 2 * k, y: ROW + k * behind }));
  return {
    id: `s${i}`,
    batch: Math.floor(i / batch),
    room,
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

// ---- the predictions -------------------------------------------------------
const built = spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "exchange", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: ["ignore", "ignore", "inherit"] });
if (built.status !== 0) fail("the exchange tool did not build");
const predicted = spawnSync(join(REPO, "target/release/exchange"), { input: JSON.stringify(samples), encoding: "utf8", maxBuffer: 1 << 28 });
if (predicted.status !== 0) fail(`the exchange tool failed: ${predicted.stderr}`);
const predictions = Object.fromEntries(JSON.parse(predicted.stdout).map((p) => [p.id, p]));
writeFileSync(join(out, "predictions.json"), JSON.stringify(predictions));

// ---- the duels, a world a batch --------------------------------------------
function node26(args, cwd) {
  const res = spawnSync("nix", ["shell", "nixpkgs#nodejs_26", "-c", "node", ...args], { cwd, stdio: ["ignore", "ignore", "inherit"] });
  if (res.status !== 0) fail(`node ${args.slice(1).join(" ")} failed (exit ${res.status})`);
}
const screepsrc = ["mods:", "  - ./seed/index.js", "  - xxscreeps/mods/classic", "game:", "  tickSpeed: 60000", "  respawnTimeout: 0",
  "processor:", "  concurrency: 1", "  intentAbandonTimeout: 120000", "runner:", "  concurrency: 1", "  cpu:", "    tickLimit: 5000", ""].join("\n");
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const filesOnly = (src) => lstatSync(src).isFile() || lstatSync(src).isDirectory();

async function duel(index, members) {
  const world = join(ENGINE, "worlds", `exch-${seed}-${index}`);
  rmSync(world, { recursive: true, force: true });
  cpSync(TEMPLATE, world, { recursive: true, filter: filesOnly });
  for (const f of ["seed", "duel", "duel-world.mjs", "duel-step.mjs"]) cpSync(join(HERE, "engine", f), join(world, f), { recursive: true });
  writeFileSync(join(world, ".screepsrc.yaml"), screepsrc);
  writeFileSync(join(world, "package.json"), '{ "private": true }\n');
  writeFileSync(join(world, "seed.txt"), `${seed}\n`);
  writeFileSync(join(world, "samples.json"), JSON.stringify(members));
  for (const user of ["fabot", "rival"]) node26([XXSCREEPS, pathToFileURL(join(world, "seed/manage.js")).href, "bot", "add", user, "./duel"], world);
  node26([XXSCREEPS, pathToFileURL(join(world, "duel-world.mjs")).href, "samples.json", "fabot", "rival"], world);
  const log = join(out, `server-${index}.log`);
  const server = spawn("nix", ["shell", "nixpkgs#nodejs_26", "-c", "node", XXSCREEPS, "start", "--no-backend"], {
    cwd: world,
    detached: true,
    stdio: ["ignore", openSync(log, "w"), openSync(log, "a")],
  });
  let exited = false;
  server.on("exit", () => (exited = true));
  const sockets = [".db", ".db.pubsub", ".shard0.db", ".shard0.pubsub", ".shard0.scratch"].map((s) => join(world, "screeps", s));
  for (let i = 0; !sockets.every(existsSync); i++) {
    if (exited || i > 1200) fail(`the server did not come up; see ${log}`);
    await sleep(100);
  }
  const results = join(out, `results-${index}.json`);
  node26([XXSCREEPS, pathToFileURL(join(world, "duel-step.mjs")).href, String(ticks), results], world);
  for (let i = 0; !exited && i < 150; i++) await sleep(100);
  if (!exited) process.kill(-server.pid, "SIGKILL");
  rmSync(world, { recursive: true, force: true });
  const byRoom = JSON.parse(readFileSync(results, "utf8"));
  return members.map((s) => [s.id, byRoom[s.room]]);
}

const batches = [...new Set(samples.map((s) => s.batch))];
const actual = {};
for (const b of batches) {
  console.error(`batch ${b + 1}/${batches.length}`);
  for (const [id, r] of await duel(b, samples.filter((s) => s.batch === b))) actual[id] = r;
}
writeFileSync(join(out, "results.json"), JSON.stringify(actual));

// ---- the report ------------------------------------------------------------
const rows = samples.map((s) => ({ s, p: predictions[s.id], a: actual[s.id] }));
const describe = ({ s, p, a }) =>
  `  ${s.id} ${s.kind}: ${s.theirs.bodies.map((b) => b.parts.length + (b.boosts.some(Boolean) ? "*" : "") + b.parts.filter((x) => x !== "move").map((x) => x[0]).join("")).join(" ")}` +
  ` ${s.owner === "ours" ? "" : "their room, "}${s.theirs.bodies[0].invader ? "Invader, " : ""}towers ${s[s.owner].towers.length}, ramparts ${s[s.owner].ramparts.length}; predicted ${p.kind} ${p.ticks}t, ${outcome(a)}${a.ticks === null ? "" : ` ${a.ticks}t`}`;
const lines = [
  `exchange generated set: ${count} duels, seed ${seed}, ${ticks} ticks at most`,
  ...report(rows),
  "bugs:",
  ...bugs(rows).map(describe),
  "predicted Win/Kite that timed out:",
  ...stood(rows).map(describe),
  "predicted Lose that won:",
  ...misses(rows).map(describe),
];
writeFileSync(join(out, "report.txt"), lines.join("\n") + "\n");
console.log(lines.join("\n"));
