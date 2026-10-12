// The economy's soak (ARCHITECTURE.md §10.2): our built bot from one spawn and
// no creeps, as a respawn, for thousands of ticks of the vanilla engine.
//
// Usage: npm run soak -- [--ticks N] [--every K] [--seed N] [--room <name>] [--run <name>]
//        npm run soak -- --list      the rooms a soak can stand in, as JSON
//   --bot    the bundle to run (default dist/main.js): v1's or a baseline's,
//            to compare on the same room and seed
//   --ticks  ticks to step (default 3000)
//   --every  a sample every K ticks (default 100), and the last tick
//   --seed   the engine's move tie-break seed (default 1): one seed, one run
//   --room   the colony's room (default: the first by name with an unowned
//            controller and two sources)
//   --run    the output's name under soak-runs/ (default <room>-seed<N>)
//   --late   ticks the bot sits out at the start (default 0): a nudge to the
//            early trajectory, so a soak:many spread measures how far a run
//            drifts from a small change, which the seed alone does not
//   --node   a start from §10.2's nodes (default ticks 5000): N1 is the
//            respawn above; N2-N4 stand what Layout's plan (`scripts/plan`)
//            builds by the node's level, the first spawn on its planned
//            tile, and the node's stock in storage
//   --capture  ticks a-b[,c-d] (b excluded) to capture.jsonl, the input
//            `scripts/bench.mjs` replays
//
// Needs `npm run build` (dist/main.js) and `npm run xx -- setup` (the engine
// and its template world, under $FABOT_XXSCREEPS_HOME). Writes
// soak-runs/<run>/ (outside build/, which `npm run build` wipes):
// samples.jsonl, faults.jsonl, refusals.json, levels.json, slow.json, observe.json,
// cpu.jsonl, console.log and server.log; prints the samples as a table and a
// summary.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { cpSync, existsSync, lstatSync, mkdirSync, openSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const usage = "usage: npm run soak -- [--ticks N] [--every K] [--seed N] [--room <name>] [--run <name>] [--bot <main.js>] [--late K] [--node N1|N2|N3|N4] [--capture a-b,…]";
const fail = (message) => {
  console.error(message);
  process.exit(1);
};

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const CACHE = process.env.FABOT_XXSCREEPS_HOME ?? join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "fabot-xxscreeps");
const ENGINE = join(CACHE, "engine");
const TEMPLATE = join(ENGINE, "template");
const XXSCREEPS = join(ENGINE, "node_modules/xxscreeps/xxscreeps.js");
const OUR_NAME = "fabot";

// ---- arguments -------------------------------------------------------------
const flags = {};
const rest = process.argv.slice(2);
const listOnly = rest[0] === "--list";
if (listOnly) rest.shift();
for (let i = 0; i < rest.length; i++) {
  const a = rest[i];
  if (!["--ticks", "--every", "--seed", "--room", "--run", "--bot", "--late", "--node", "--capture"].includes(a)) fail(usage);
  const value = rest[++i];
  if (value === undefined || value.startsWith("--")) fail(`${a} needs a value\n${usage}`);
  flags[a] = value;
}
const whole = (flag, fallback, min) => {
  const n = Number(flags[flag] ?? fallback);
  if (!(Number.isInteger(n) && n >= min)) fail(`${flag} needs a whole number ≥ ${min}\n${usage}`);
  return n;
};
// Each node's level and storage stock; N4's links are what it judges, so
// they are left to the bot, and every node's ramparts, which a node would
// stand at full hits the bot never paid for.
const NODES = {
  N1: { level: 1, stored: 0, without: [] },
  N2: { level: 3, stored: 0, without: ["rampart"] },
  N3: { level: 4, stored: 5000, without: ["rampart"] },
  N4: { level: 5, stored: 50000, without: ["link", "rampart"] },
};
const node = flags["--node"] === undefined ? undefined : NODES[flags["--node"]];
if (flags["--node"] !== undefined && !node) fail(`--node: one of ${Object.keys(NODES).join(", ")}`);
const ticks = whole("--ticks", node ? 5000 : 3000, 1);
const every = whole("--every", 100, 1);
const seed = whole("--seed", 1, 0);
const late = whole("--late", 0, 0);
const windows = (flags["--capture"] ?? "").split(",").filter(Boolean).map((w) => w.split("-").map(Number));
if (windows.some((w) => w.length !== 2 || !w.every(Number.isInteger))) fail(`--capture: ticks a-b[,c-d]\n${usage}`);
if (flags["--room"] !== undefined && !/^[WE]\d+[NS]\d+$/.test(flags["--room"])) fail(`--room: "${flags["--room"]}" is not a room name`);
if (flags["--run"] !== undefined && !/^[\w.-]+$/.test(flags["--run"])) fail("--run: letters, digits, dots, dashes");
// The bundle that plays: ours as built, or another (v1's, a baseline's) to
// compare against on the same room and seed.
const bot = resolve(flags["--bot"] ?? join(REPO, "dist/main.js"));
if (!existsSync(bot)) fail(flags["--bot"] ? `--bot: no file at ${bot}` : "no dist/main.js: npm run build first");
if (!existsSync(join(TEMPLATE, "screeps/shard0/data.json"))) fail(`no template world at ${TEMPLATE}: npm run xx -- setup first`);

// ---- the engine, on Node 24 through nix (as xx.mjs) ------------------------
function node24(args, cwd, capture = false) {
  const res = spawnSync("nix", ["shell", "nixpkgs#nodejs_24", "-c", "node", ...args], {
    cwd,
    stdio: capture ? ["ignore", "pipe", "inherit"] : "inherit",
    encoding: "utf8",
  });
  if (res.error) fail(`nix: ${res.error.message}`);
  if (res.status !== 0) fail(`node ${args.slice(1).join(" ")} failed (exit ${res.status})`);
  return res.stdout;
}
const script = (name) => pathToFileURL(join(world, name)).href;

// One processor and one runner thread keep the seeded tie-breaks in one
// sequence; a tick a minute when not paused, so a missed pause times out.
// The first tick builds our isolate, which can outlast the engine's 5 s wait
// for intents: that tick's would be dropped some runs and not others.
const screepsrc = [
  "mods:",
  "  - ./seed/index.js",
  "  - xxscreeps/mods/classic",
  "game:",
  "  tickSpeed: 60000",
  "  respawnTimeout: 0",
  "processor:",
  "  concurrency: 1",
  "  intentAbandonTimeout: 120000",
  "runner:",
  "  concurrency: 1",
  "",
].join("\n");

// ---- the world -------------------------------------------------------------
// One world per run, so two soaks at once never share a server. The name is
// kept short: the engine's unix sockets live under it, and a socket path has
// a length limit. A long run name keeps its head and a hash of the whole, so
// two runs whose names share their first characters never share a world.
const runName = (flags["--run"] ?? "default").replace(/[^A-Za-z0-9-]/g, "");
const worldName = runName.length <= 24 ? runName : `${runName.slice(0, 15)}-${createHash("sha256").update(runName).digest("hex").slice(0, 8)}`;
const world = join(ENGINE, "worlds", `soak-${worldName}`);
rmSync(world, { recursive: true, force: true });
// A copied socket is a dead server's; only files and directories come over.
cpSync(TEMPLATE, world, { recursive: true, filter: (src) => lstatSync(src).isFile() || lstatSync(src).isDirectory() });
for (const f of ["seed", "colony.mjs", "node.mjs", "soak-step.mjs"]) cpSync(join(HERE, "engine", f), join(world, f), { recursive: true });
writeFileSync(join(world, ".screepsrc.yaml"), screepsrc);
// Mods resolve from the cwd only where a package.json stands.
writeFileSync(join(world, "package.json"), '{ "private": true }\n');
writeFileSync(join(world, "seed.txt"), `${seed}\n`);
mkdirSync(join(world, "bot"), { recursive: true });
if (late === 0 && windows.length === 0) cpSync(bot, join(world, "bot/main.js"));
else {
  // The bundle's exports are getters, so the wait and the capture wrap it
  // from outside; the capture runs after the loop, out of the bot's clock.
  cpSync(bot, join(world, "bot/fabot.js"));
  cpSync(join(HERE, "engine/capture.js"), join(world, "bot/capture.js"));
  writeFileSync(
    join(world, "bot/main.js"),
    `const bot = require("fabot");\nconst capture = require("capture");\nconst windows = ${JSON.stringify(windows)};\nlet first;\n` +
      `module.exports.loop = () => {\n  first ??= Game.time;\n  const t = Game.time - first;\n` +
      `  const memory = windows.some(([a, b]) => t >= a && t < b) ? RawMemory.get() : null;\n` +
      `  if (t >= ${late}) bot.loop();\n  if (memory !== null) console.log("#capture " + capture(memory));\n};\n`,
  );
}

if (listOnly) {
  // The rooms a soak can stand in, for `soak-many.mjs`; nothing is run.
  console.log(node24([XXSCREEPS, script("colony.mjs"), "--list"], world, true).trim().split("\n").at(-1));
  rmSync(world, { recursive: true, force: true });
  process.exit(0);
}
const colony = JSON.parse(node24([XXSCREEPS, script("colony.mjs"), ...(flags["--room"] ? [flags["--room"]] : [])], world, true).trim().split("\n").at(-1));
const run = flags["--run"] ?? `${colony.room}${flags["--node"] ? `-${flags["--node"]}` : ""}-seed${seed}`;
const out = join(REPO, "soak-runs", run);
rmSync(out, { recursive: true, force: true });
mkdirSync(out, { recursive: true });
let built = null;
if (node && node.level > 1) {
  // FABOT_PLAN_BUILT: the caller built it once (soak-many's parallel runs).
  const plan = join(REPO, "scripts/plan");
  if (!process.env.FABOT_PLAN_BUILT) {
    const res = spawnSync("dotnet", ["build", "-c", "Release", "--nologo", "-v", "q", plan], { stdio: ["ignore", "ignore", "inherit"] });
    if (res.status !== 0) fail("scripts/plan did not build");
  }
  const ground = node24([XXSCREEPS, script("node.mjs"), "ground", colony.room], world, true).trim().split("\n").at(-1);
  const res = spawnSync("dotnet", [join(plan, "bin/Release/net10.0/Plan.dll")], {
    input: JSON.stringify({ ...JSON.parse(ground), level: node.level }),
    encoding: "utf8",
  });
  if (res.status !== 0) fail(`scripts/plan failed: ${res.stderr}`);
  built = JSON.parse(res.stdout).filter((s) => !node.without.includes(s.kind));
  writeFileSync(join(world, "node.json"), JSON.stringify(built));
  [colony.x, colony.y] = [built[0].x, built[0].y];
}
console.error(`colony: ${colony.room}, spawn at ${colony.x},${colony.y}; output ${out}`);
node24([XXSCREEPS, "manage", "bot", "add", OUR_NAME, "./bot", "--spawn", colony.room, `${colony.x},${colony.y}`], world);
if (built) node24([XXSCREEPS, script("node.mjs"), "stand", colony.room, "node.json", String(node.level), String(node.stored), OUR_NAME], world);

// ---- the server, stepped ---------------------------------------------------
const log = join(out, "server.log");
const server = spawn("nix", ["shell", "nixpkgs#nodejs_24", "-c", "node", XXSCREEPS, "start", "--no-backend"], {
  cwd: world,
  detached: true,
  stdio: ["ignore", openSync(log, "w"), openSync(log, "a")],
});
let exited = false;
server.on("exit", () => (exited = true));
const stop = () => {
  if (!exited) {
    try {
      process.kill(-server.pid, "SIGKILL");
    } catch {}
  }
};
process.on("exit", stop);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const sockets = [".db", ".db.pubsub", ".shard0.db", ".shard0.pubsub", ".shard0.scratch"].map((s) => join(world, "screeps", s));
for (let i = 0; !sockets.every(existsSync); i++) {
  if (exited || i > 1200) fail(`the server did not come up; see ${log}`);
  await sleep(100);
}
const stepped = JSON.parse(node24([XXSCREEPS, script("soak-step.mjs"), String(ticks), String(every), colony.room, OUR_NAME, out], world, true).trim().split("\n").at(-1));
for (let i = 0; !exited && i < 150; i++) await sleep(100);
stop();

// ---- the report ------------------------------------------------------------
const lines = (f) => readFileSync(join(out, f), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
const samples = lines("samples.jsonl");
const faults = lines("faults.jsonl");
const refusals = JSON.parse(readFileSync(join(out, "refusals.json"), "utf8"));
// Steps the engine took past the step's wait (a slow or abandoned bot tick).
const slow = JSON.parse(readFileSync(join(out, "slow.json"), "utf8"));
const levels = JSON.parse(readFileSync(join(out, "levels.json"), "utf8"));

const columns = [
  ["t", (s) => s.t],
  ["rcl", (s) => s.rcl],
  ["progress", (s) => s.progress],
  ["creeps", (s) => s.creeps],
  ["parts", (s) => s.parts],
  ["sources", (s) => s.sources],
  ["energy", (s) => `${s.energy}/${s.capacity}`],
  ["stored", (s) => s.stored],
  ["dropped", (s) => s.dropped],
  ["sites", (s) => s.sites],
  ["hostile", (s) => s.hostiles],
  ["cpu", (s) => (s.cpu == null ? "-" : s.cpu.toFixed(2))],
  ["faults", (s) => s.faults],
];
const cells = samples.map((s) => columns.map(([, f]) => String(f(s))));
const widths = columns.map(([h], i) => Math.max(h.length, ...cells.map((c) => c[i].length)));
const row = (c) => c.map((v, i) => v.padStart(widths[i])).join("  ");
console.log(row(columns.map(([h]) => h)));
for (const c of cells) console.log(row(c));

const last = samples.at(-1);
const reached = (rcl) => levels.find((l) => l.rcl >= rcl)?.t;
const cpus = lines("cpu.jsonl").map((r) => r.cpu).filter((c) => typeof c === "number");
const top = Object.entries(refusals)
  .sort((a, b) => b[1] - a[1])
  .slice(0, 5)
  .map(([k, n]) => `${k} ×${n}`);
console.log("");
console.log(
  [
    `${run}: ${stepped.ticks} ticks in ${(stepped.ms / 1000).toFixed(1)} s (${(stepped.ms / stepped.ticks).toFixed(1)} ms/tick)`,
    `final RCL ${last.rcl} progress ${last.progress}`,
    `peak creeps ${Math.max(...samples.map((s) => s.creeps))}`,
    `RCL2 ${reached(2) === undefined ? "not reached" : `t${reached(2)}`}`,
    `RCL3 ${reached(3) === undefined ? "not reached" : `t${reached(3)}`}`,
    `cpu mean ${cpus.length ? (cpus.reduce((a, b) => a + b, 0) / cpus.length).toFixed(2) : "-"} max ${cpus.length ? Math.max(...cpus).toFixed(2) : "-"}`,
    `faults ${faults.length}`,
    `refusals ${top.length ? top.join(", ") : "none"}`,
    `slow steps ${slow.length ? slow.map((t) => `t${t}`).join(", ") : "none"}`,
  ].join("; "),
);
for (const f of faults.slice(0, 10)) console.log(`  fault t${f.tick} ${f.where}: ${f.message}`);
if (faults.length > 10) console.log(`  … ${faults.length - 10} more in ${join(out, "faults.jsonl")}`);
