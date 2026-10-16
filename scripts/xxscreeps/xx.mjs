// Cross-validate an arena scenario on xxscreeps, the vanilla engine's rules
// with the vanilla NPC AI, and take heap snapshots of our isolate (#543).
// When to use it and what it cannot tell: docs/agents/arena-capture.md.
//
// Usage:
//   xx.mjs setup
//       install xxscreeps (and build isolated-vm) in the cache, import a world
//   xx.mjs run <slug | scenario.json> [--ticks N] [--bot standin|replay|dist|<dir>]
//              [--seed N] [--heap [tick]] [--filter <test name part>] [--out dir]
//       <slug>    an arena test's `crossValidate "<slug>"`: the test is run
//                 with the dump on, writing its start and its trace
//       --seed    the engine's move tie-break seed (default 1): one seed is
//                 one run, every time; a few seeds give the spread
//       --bot     who drives our bodies: the #535 melee stand-in (default),
//                 a replay of the arena run's own (only the other side's AI
//                 can then part the traces), our `dist/main.js`, or a
//                 directory holding a main.js
//       --heap    a heap snapshot of our isolate after that tick (default the last)
//       --filter  the `dotnet test` filter part that reaches the test (default `arena`)
//   xx.mjs diff <arena.jsonl> <xxscreeps.jsonl>
//
// The engine runs on Node 24 through `nix shell nixpkgs#nodejs_24`, never the
// system Node; xxscreeps and its native builds live in
// $FABOT_XXSCREEPS_HOME (default ~/.cache/fabot-xxscreeps), never in the repo.
// Output goes to build/xxscreeps/<slug>/: the scenario, the arena trace, and
// per bot the xxscreeps trace, the diff, the server log and the snapshot.
import { spawn, spawnSync } from "node:child_process";
import { cpSync, existsSync, lstatSync, mkdirSync, readFileSync, rmSync, writeFileSync, openSync } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { describe, diff, readTrace } from "./trace.mjs";

const usage = `usage: xx.mjs setup
       xx.mjs run <slug | scenario.json> [--ticks N] [--bot standin|replay|dist|<dir>] [--seed N] [--heap [tick]] [--filter <test name part>] [--out dir]
       xx.mjs diff <arena.jsonl> <xxscreeps.jsonl>`;

function fail(message) {
  console.error(message);
  process.exit(1);
}

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const VERSION = "0.2.1";
const CACHE = process.env.FABOT_XXSCREEPS_HOME ?? join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "fabot-xxscreeps");
const ENGINE = join(CACHE, "engine");
const TEMPLATE = join(ENGINE, "template");
const XXSCREEPS = join(ENGINE, "node_modules/xxscreeps/xxscreeps.js");
const OUR_NAME = "fabot";
// A world's schema is its mods': a scenario with an invader core stands on a
// template imported with the modern stronghold mod beside the classic ones;
// a live capture (`soak --world`) on one with every structure kind.
const VARIANTS = {
  "": [],
  stronghold: ["xxscreeps/mods/modern/stronghold"],
  modern: ["xxscreeps/mods/modern/stronghold", "xxscreeps/mods/modern/observer", "xxscreeps/mods/modern/powerspawn"],
};

// ---- arguments: every refusal before any nix, dotnet or engine call ---------
const [command, ...rest] = process.argv.slice(2);
const flags = {};
const args = [];
for (let i = 0; i < rest.length; i++) {
  const a = rest[i];
  if (["--ticks", "--bot", "--seed", "--filter", "--out"].includes(a)) {
    const value = rest[++i];
    if (value === undefined || value.startsWith("--")) fail(`${a} needs a value\n${usage}`);
    flags[a] = value;
  } else if (a === "--heap") {
    flags[a] = rest[i + 1] !== undefined && /^\d+$/.test(rest[i + 1]) ? Number(rest[++i]) : "last";
  } else if (a.startsWith("--")) {
    fail(usage);
  } else {
    args.push(a);
  }
}

if (command === "diff") {
  if (args.length !== 2 || Object.keys(flags).length > 0) fail(usage);
  for (const f of args) if (!existsSync(f)) fail(`no trace at ${f}`);
  console.log(diff(readTrace(args[0]), readTrace(args[1])));
  process.exit(0);
}
if (command === "setup") {
  if (args.length > 0 || Object.keys(flags).length > 0) fail(usage);
  for (const variant of Object.keys(VARIANTS)) setup(variant);
  process.exit(0);
}
if (command !== "run" || args.length !== 1) fail(usage);

const target = args[0];
const fromFile = target.endsWith(".json");
if (fromFile && !existsSync(target)) fail(`no scenario at ${target}`);
if (!fromFile && !/^[a-z0-9][a-z0-9-]*$/.test(target)) fail(`"${target}" is neither a scenario .json nor a slug (lowercase letters, digits, dashes)\n${usage}`);
const slug = fromFile ? basename(target).replace(/\.scenario\.json$|\.json$/, "") : target;
const ticks = flags["--ticks"] === undefined ? undefined : Number(flags["--ticks"]);
if (ticks !== undefined && !(Number.isInteger(ticks) && ticks > 0)) fail(`--ticks needs a positive whole number\n${usage}`);
const seed = Number(flags["--seed"] ?? 1);
if (!(Number.isInteger(seed) && seed >= 0)) fail(`--seed needs a whole number\n${usage}`);
const bot = flags["--bot"] ?? "standin";
if (!["standin", "replay", "dist"].includes(bot) && !existsSync(join(bot, "main.js"))) {
  fail(`--bot: "${bot}" is not standin, replay, dist, or a directory holding a main.js`);
}
if (bot === "dist" && !existsSync(join(REPO, "dist/main.js"))) fail("--bot dist needs dist/main.js: npm run build first");
const out = resolve(flags["--out"] ?? join(REPO, "build/xxscreeps", slug));
// One set per bot, so a stand-in run and a replay sit side by side.
const tag = ["standin", "replay", "dist"].includes(bot) ? bot : "bot";

// ---- the engine ------------------------------------------------------------
function nix(packages, cmd, cmdArgs, options = {}) {
  const res = spawnSync("nix", ["shell", ...packages.map((p) => `nixpkgs#${p}`), "-c", cmd, ...cmdArgs], { stdio: "inherit", ...options });
  if (res.error) fail(`nix: ${res.error.message}`);
  return res.status;
}
function node24(cmdArgs, cwd) {
  return nix(["nodejs_24"], "node", cmdArgs, { cwd });
}
function must(status, what) {
  if (status !== 0) fail(`${what} failed (exit ${status})`);
}

function setup(variant = "") {
  mkdirSync(ENGINE, { recursive: true });
  const manifest = {
    name: "fabot-xxscreeps",
    private: true,
    dependencies: { xxscreeps: VERSION },
    allowScripts: { "isolated-vm@6.0.2": true, "ivm-inspect@5.0.0": true },
  };
  writeFileSync(join(ENGINE, "package.json"), JSON.stringify(manifest, null, 2) + "\n");
  const installed = existsSync(join(ENGINE, "node_modules/xxscreeps/package.json"))
    ? JSON.parse(readFileSync(join(ENGINE, "node_modules/xxscreeps/package.json"), "utf8")).version
    : null;
  const build = ["nodejs_24", "python3", "gcc", "gnumake"];
  if (installed !== VERSION) {
    console.error(`installing xxscreeps ${VERSION} in ${ENGINE}`);
    must(nix(build, "npm", ["install", "--no-audit", "--no-fund"], { cwd: ENGINE }), "npm install");
  }
  // The native builds must be Node 24's: the system Node's ABI is not.
  const probe = ["-e", "require('isolated-vm'); require('ivm-inspect')"];
  if (node24(probe, ENGINE) !== 0) {
    console.error("building isolated-vm and ivm-inspect against Node 24");
    must(nix(build, "npm", ["rebuild", "isolated-vm", "ivm-inspect", "--foreground-scripts"], { cwd: ENGINE }), "npm rebuild");
    must(node24(probe, ENGINE), "loading isolated-vm");
  }
  const template = templateOf(variant);
  if (!existsSync(join(template, "screeps/shard0/data.json"))) {
    console.error(`importing a world into ${template}`);
    rmSync(template, { recursive: true, force: true });
    mkdirSync(template, { recursive: true });
    writeFileSync(join(template, ".screepsrc.yaml"), screepsrc(false, variant));
    must(node24([XXSCREEPS, "import"], template), "xxscreeps import");
  }
}

function templateOf(variant) {
  return variant ? `${TEMPLATE}-${variant}` : TEMPLATE;
}

// One tick a minute when not paused: a pause that missed the first tick
// shows as a timeout, not as a run that ran on. One processor thread keeps
// the seeded tie-breaks in one sequence; one runner thread holds every
// sandbox the snapshot asks for.
function screepsrc(ours, variant) {
  return [
    "mods:",
    ...(ours ? ["  - ./heapsnap/index.js", "  - ./seed/index.js"] : []),
    "  - xxscreeps/mods/classic",
    ...VARIANTS[variant].map((m) => `  - ${m}`),
    "game:",
    "  tickSpeed: 60000",
    "  respawnTimeout: 0",
    "processor:",
    "  concurrency: 1",
    "runner:",
    "  concurrency: 1",
    "",
  ].join("\n");
}

// Our bodies' arena path and casts, tick by tick, for `engine/replay`: the
// pipeline's Intents as `crossValidate` wrote them (`AttackCreep ("a", "b")`).
const VERBS = { AttackCreep: "attack", RangedAttackCreep: "rangedAttack", HealCreep: "heal", RangedHealCreep: "rangedHeal", RangedMassAttack: "rangedMassAttack" };
// Our towers' shots and heals too (#567), by the tower's tile: xxscreeps
// gives every structure an id of its own.
const TOWER_VERBS = { FireTower: "attack", HealWithTower: "heal" };
// A script's acts (`Arena.Act`), as the arena trace prints them.
const SCRIPT_VERBS = { Attack: "attack", RangedAttack: "rangedAttack", Heal: "heal", RangedHeal: "rangedHeal", RangedMassAttack: "rangedMassAttack", Dismantle: "dismantle" };
function replayOf(scenario, arena) {
  const ours = new Set(scenario.bodies.filter((b) => b.side === "ours").map((b) => b.id));
  const towers = new Map(scenario.rooms.flatMap((r) => r.structures.filter((s) => s.kind === "tower" && s.owner === "ours").map((s) => [s.id, [r.name, s.x, s.y]])));
  const ticks = arena.map((row) => {
    const plan = { "#towers": [] };
    for (const b of row.bodies) if (ours.has(b.id)) plan[b.id] = { at: [b.room, b.x, b.y], acts: [] };
    for (const intent of row.ours) {
      const m = /^(\w+) \((.*)\)$/.exec(intent);
      if (!m) continue;
      const [actor, targetName] = [...m[2].matchAll(/"([^"]*)"/g)].map((q) => q[1]);
      if (VERBS[m[1]]) plan[actor]?.acts.push([VERBS[m[1]], targetName ?? null]);
      else if (TOWER_VERBS[m[1]] && towers.has(actor)) plan["#towers"].push([...towers.get(actor), TOWER_VERBS[m[1]], targetName]);
    }
    // A scripted body of ours (a spar squad, no pipeline) acts by its
    // script: `<id> RangedAttack "invaderCore-8-36"`. A structure target is
    // its arena id, `kind-x-y`, which the bot finds by tile.
    for (const act of row.acts ?? []) {
      const m = /^(\S+) (\w+)(?: "([^"]*)")?$/.exec(act);
      if (!m || !ours.has(m[1]) || !SCRIPT_VERBS[m[2]]) continue;
      plan[m[1]]?.acts.push([SCRIPT_VERBS[m[2]], m[3] ?? null]);
    }
    return plan;
  });
  return { start: JSON.parse(readFileSync(join(world, "world.json"), "utf8")).time, ticks };
}

// ---- run -------------------------------------------------------------------
mkdirSync(out, { recursive: true });
let scenarioPath;
let arenaPath;
if (fromFile) {
  scenarioPath = resolve(target);
  const sibling = scenarioPath.replace(/\.scenario\.json$|\.json$/, ".arena.jsonl");
  arenaPath = existsSync(sibling) ? sibling : undefined;
} else {
  scenarioPath = join(out, `${slug}.scenario.json`);
  arenaPath = join(out, `${slug}.arena.jsonl`);
  rmSync(scenarioPath, { force: true });
  console.error(`running the arena test that calls crossValidate "${slug}"`);
  const res = spawnSync("dotnet", ["test", join(REPO, "tests/Core.Tests"), "-c", "Release", "--filter", `FullyQualifiedName~${flags["--filter"] ?? "arena"}`], {
    stdio: ["ignore", "ignore", "inherit"],
    env: { ...process.env, FABOT_ARENA_DUMP: slug, FABOT_ARENA_DUMP_DIR: out },
  });
  if (res.error) fail(`dotnet: ${res.error.message}`);
  if (!existsSync(scenarioPath)) {
    fail(`no test under the filter called crossValidate "${slug}" (dotnet test exit ${res.status}); --filter reaches it`);
  }
}
const scenario = JSON.parse(readFileSync(scenarioPath, "utf8"));
const arena = arenaPath ? readTrace(arenaPath) : undefined;
if (bot === "replay" && !arena) fail("--bot replay needs the arena trace beside the scenario (<slug>.arena.jsonl)");

const variant = scenario.rooms.some((r) => r.structures.some((s) => s.kind === "invaderCore")) ? "stronghold" : "";
setup(variant);

const world = join(ENGINE, "worlds", slug);
rmSync(world, { recursive: true, force: true });
// A copied socket is a dead server's; only files and directories come over.
cpSync(templateOf(variant), world, { recursive: true, filter: (src) => lstatSync(src).isFile() || lstatSync(src).isDirectory() });
cpSync(join(HERE, "engine"), world, { recursive: true, filter: (src) => !/\/(standin|replay)$/.test(src) });
writeFileSync(join(world, ".screepsrc.yaml"), screepsrc(true, variant));
// Mods resolve from the cwd only where a package.json stands (`config/mods.js`).
writeFileSync(join(world, "package.json"), '{ "private": true }\n');
writeFileSync(join(world, "seed.txt"), `${seed}\n`);
const botDir = join(world, "bot");
mkdirSync(botDir, { recursive: true });
if (bot === "standin") {
  cpSync(join(HERE, "engine/standin/main.js"), join(botDir, "main.js"));
  const hostiles = scenario.bodies.filter((b) => b.side !== "ours").map((b) => ({ room: b.room, x: b.x, y: b.y }));
  writeFileSync(join(botDir, "targets.js"), `module.exports = ${JSON.stringify(hostiles.slice(0, 1))};\n`);
} else if (bot === "replay") {
  // replay.js needs the start tick, known once the scenario stands.
  cpSync(join(HERE, "engine/replay/main.js"), join(botDir, "main.js"));
  writeFileSync(join(botDir, "replay.js"), "module.exports = { start: 0, ticks: [] };\n");
} else {
  cpSync(bot === "dist" ? join(REPO, "dist/main.js") : join(bot, "main.js"), join(botDir, "main.js"));
}

must(node24([XXSCREEPS, "manage", "bot", "add", OUR_NAME, "./bot"], world), "manage bot add");
const players = new Set(
  [...scenario.bodies.map((b) => b.side), ...scenario.rooms.flatMap((r) => r.structures.map((s) => s.owner))].filter((s) => s?.startsWith("player:")),
);
for (const p of players) must(node24([XXSCREEPS, "manage", "user", "create", p.slice(7)], world), `manage user create ${p}`);
must(node24([XXSCREEPS, pathToFileURL(join(world, "world.mjs")).href, scenarioPath, join(REPO, "tests/Core.Tests/rooms"), OUR_NAME], world), "standing the scenario");
if (bot === "replay") {
  writeFileSync(join(botDir, "replay.js"), `module.exports = ${JSON.stringify(replayOf(scenario, arena))};\n`);
  must(node24([XXSCREEPS, "manage", "bot", "update", OUR_NAME, "./bot"], world), "manage bot update");
}

const log = join(out, `${slug}.${tag}.server.log`);
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

const xxPath = join(out, `${slug}.${tag}.xxscreeps.jsonl`);
const heapPath = join(out, `${slug}.${tag}.heapsnapshot`);
const steps = ticks ?? arena?.length ?? 300;
const heapArgs = flags["--heap"] === undefined ? [] : [String(flags["--heap"] === "last" ? steps - 1 : flags["--heap"]), heapPath];
const stepped = node24([XXSCREEPS, pathToFileURL(join(world, "step.mjs")).href, String(steps), xxPath, ...heapArgs], world);
for (let i = 0; !exited && i < 150; i++) await sleep(100);
stop();
must(stepped, `stepping (server log: ${log})`);

const xx = readTrace(xxPath);
writeFileSync(join(out, `${slug}.${tag}.xxscreeps.txt`), describe(xx) + "\n");
const { warnings } = JSON.parse(readFileSync(join(world, "world.json"), "utf8"));
const notes = warnings.length > 0 ? `notes from standing it:\n${warnings.map((w) => `  ${w}`).join("\n")}\n\n` : "";
if (arena) {
  const text = `${scenario.name}: arena (fabot) | xxscreeps (bot: ${bot})\n${notes}${diff(arena, xx)}\n`;
  writeFileSync(join(out, `${slug}.${tag}.diff.txt`), text);
  console.log(text.split("\nper tick")[0]);
  console.log(`\nper-tick diff: ${join(out, `${slug}.${tag}.diff.txt`)}`);
} else {
  console.log(`${notes}no arena trace beside the scenario: ${join(out, `${slug}.${tag}.xxscreeps.txt`)} alone`);
}
// The world is rebuilt from the template on every run; it is kept nowhere.
rmSync(world, { recursive: true, force: true });
