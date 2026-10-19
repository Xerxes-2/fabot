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
//            respawn above; N2-T7 stand what Layout's plan (`src/bin/plan.rs`)
//            builds by the node's level, the first spawn on its planned
//            tile, and the node's stock in storage; E1 is N4 with the two
//            neighbours `colony.mjs` picks declared as outposts
//            (Memory.v2.declarations.outposts) and an Invader wave in each;
//            E2 is N5 with the first of them declared a claim (`claims`),
//            C2 with both, G2 E2 with no GCL level spare for it;
//            L5 is N5 with H, O and U stocked and its labs the cluster (`labs`)
//   --invaders  room:every[:first][,…]: an Invader wave (`engine/waves`) in
//            room at t = first, first + every, … (first defaults to every);
//            E1's default: its outposts every 4000, from 1000 and 2500
//   --wave   role[:size],…: each wave's Invaders, a size small (the default) or
//            big (default melee,healer,ranged)
//   --core   room:tick[,…]: a level-0 Invader core (`engine/core`) stood in
//            room at t = tick, on the template imported with the stronghold
//            mod (`npm run xx -- setup`)
//   --capture  ticks a-b[,c-d] (b excluded) to capture.jsonl, the input
//            `scripts/bench.mjs` replays
//   --save   a directory to copy the world into after the last tick
//   --from   a world --save wrote (needs --room): the bot is swapped for
//            --bot, its Memory kept, as a deploy at that tick
//   --opponents  a JSON file of players seated beside ours, each
//            { name, bot, room, x?, y?, config?, raid?, ally? }: `bot` a bundle or a
//            bot directory, `config` written beside it as `opponent.js`, the
//            spawn on colony.mjs's tile unless x, y; `raid` its bodies stood
//            on our room's edge, or its `room`'s (`engine/soak-step.mjs`), `ally` its name in our
//            declared allies (not with --world or --from: their Memory declares). With --from they stand
//            in the world already and only their code is swapped. A bot
//            directory's `fetch.json` { tarball, integrity, from, main }
//            names code that is never in this tree: the tarball, fetched once
//            into the cache and checked against its integrity, gives its
//            `from` directory's files beside the directory's own, its
//            main.js as `main`.
//   --world  a live capture (`scripts/capture-world.mjs`), or a crafted world
//            of its shape (`worlds/`, terrain as 50 rows), as the world, at its
//            tick (`engine/live.mjs`): our user the capture's, every home of
//            ours a colony the samples follow, the capture's outposts declared;
//            stood on the template with every structure kind (`xx.mjs`); its
//            --opponents are its own users, their code seated on their rooms
//   --unsafe  our room without the engine's safe mode, which would decide
//            every fight in it for us; implied by --opponents
//   --ended   our room's starting safe mode ended, its stock kept to activate,
//            opponents or not
//   --keep   leave the run's world in the cache's worlds/ (default removed)
//   --watch  the server's backend up on 127.0.0.1:--port (default 21025) for
//            a client to follow, each tick held to --tick-ms (default 500),
//            the world left up after the last until Ctrl-C; prints the login
//
// Needs `npm run build` (dist/main.js) and `npm run xx -- setup` (the engine
// and its template world, under $FABOT_XXSCREEPS_HOME). Writes
// soak-runs/<run>/ (outside build/, which `npm run build` wipes):
// samples.jsonl, faults.jsonl, refusals.json, levels.json, slow.json, lost.json, observe.json,
// cpu.jsonl, bodies.jsonl, console.log and server.log; prints the samples as a table and a
// summary.
import { spawn, spawnSync } from "node:child_process";
import { createHash, randomBytes } from "node:crypto";
import { cpSync, existsSync, lstatSync, mkdirSync, openSync, readdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const usage = "usage: npm run soak -- [--ticks N] [--every K] [--seed N] [--room <name>] [--run <name>] [--bot <main.js>] [--late K] [--node N1|N2|N3|T3|N4|T4|N5|T7|E1|E2|C2|G2|L5] [--invaders room:every[:first],…] [--wave role[:size],…] [--core room:tick,…] [--capture a-b,…] [--save <dir>] [--from <dir> --room <name>] [--opponents <file.json>] [--world <capture.json>] [--unsafe] [--ended] [--keep] [--watch [--tick-ms N] [--port N]]";
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

// ---- arguments -------------------------------------------------------------
const flags = {};
const WATCH_VALUES = ["--tick-ms", "--port"];
const rest = process.argv.slice(2);
const listOnly = rest[0] === "--list";
if (listOnly) rest.shift();
for (let i = 0; i < rest.length; i++) {
  const a = rest[i];
  if (a === "--unsafe" || a === "--ended" || a === "--keep" || a === "--watch") {
    flags[a] = true;
    continue;
  }
  if (!["--ticks", "--every", "--seed", "--room", "--run", "--bot", "--late", "--node", "--capture", "--save", "--from", "--invaders", "--wave", "--core", "--opponents", "--world", ...WATCH_VALUES].includes(a)) fail(usage);
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
  // N3 as another bot built it: each container on another tile serving its source or controller.
  T3: { level: 4, stored: 5000, without: ["rampart"], moved: true },
  N4: { level: 5, stored: 50000, without: ["link", "rampart"] },
  T4: { level: 5, stored: 50000, without: ["link", "rampart"], moved: true },
  E1: { level: 5, stored: 50000, without: ["link", "rampart"], outposts: true },
  N5: { level: 6, stored: 100000, without: ["rampart"] },
  E2: { level: 6, stored: 100000, without: ["rampart"], claims: 1 },
  C2: { level: 6, stored: 100000, without: ["rampart"], claims: 2 },
  G2: { level: 6, stored: 100000, without: ["rampart"], claims: 1, short: true },
  // N5 with the ore for the UH line stocked and its labs declared the cluster.
  L5: { level: 6, stored: "100000,H:6000,O:6000,U:6000", without: ["rampart"], labs: { UH2O: 3000 } },
  // An RCL7 node as another bot left it: its roads and containers worn to
  // half, its links and ramparts sites to build.
  T7: { level: 7, stored: 100000, without: ["link", "rampart", "factory"], worn: 0.5 },
};
const node = flags["--node"] === undefined ? undefined : NODES[flags["--node"]];
if (flags["--node"] !== undefined && !node) fail(`--node: one of ${Object.keys(NODES).join(", ")}`);
const ticks = whole("--ticks", node ? 5000 : 3000, 1);
const every = whole("--every", 100, 1);
const seed = whole("--seed", 1, 0);
const late = whole("--late", 0, 0);
const watch = flags["--watch"] && { ms: whole("--tick-ms", 500, 1), port: whole("--port", 21025, 1), password: randomBytes(9).toString("base64url") };
if (!watch && WATCH_VALUES.some((f) => flags[f])) fail(`${WATCH_VALUES.join(" and ")} need --watch\n${usage}`);
// The engine saves the world only on the shutdown a watched run never sends.
if (watch && flags["--save"]) fail(`--watch takes no --save\n${usage}`);
const windows = (flags["--capture"] ?? "").split(",").filter(Boolean).map((w) => w.split("-").map(Number));
if (windows.some((w) => w.length !== 2 || !w.every(Number.isInteger))) fail(`--capture: ticks a-b[,c-d]\n${usage}`);
if (flags["--room"] !== undefined && !/^[WE]\d+[NS]\d+$/.test(flags["--room"])) fail(`--room: "${flags["--room"]}" is not a room name`);
const waves = (flags["--invaders"] ?? "").split(",").filter(Boolean);
if (!waves.every((w) => /^[WE]\d+[NS]\d+:[1-9]\d*(:\d+)?$/.test(w))) fail(`--invaders: room:every[:first][,…]\n${usage}`);
const wave = flags["--wave"] ?? "melee,healer,ranged";
if (!wave.split(",").every((w) => /^(melee|healer|ranged)(:(small|big))?$/.test(w))) fail(`--wave: role[:size][,…]\n${usage}`);
const cores = (flags["--core"] ?? "").split(",").filter(Boolean);
if (!cores.every((c) => /^[WE]\d+[NS]\d+:\d+$/.test(c))) fail(`--core: room:tick[,…]\n${usage}`);
if (flags["--run"] !== undefined && !/^[\w.-]+$/.test(flags["--run"])) fail("--run: letters, digits, dots, dashes");
// The bundle that plays: ours as built, or another (v1's, a baseline's) to
// compare against on the same room and seed.
const bot = resolve(flags["--bot"] ?? join(REPO, "dist/main.js"));
if (!existsSync(bot)) fail(flags["--bot"] ? `--bot: no file at ${bot}` : "no dist/main.js: npm run build first");
const live = flags["--world"] && JSON.parse(readFileSync(flags["--world"], "utf8"));
const OUR_NAME = live?.username ?? "fabot";
const from = flags["--from"] && resolve(flags["--from"]);
if (from && (!flags["--room"] || node || late || windows.length)) fail(`--from needs --room, and takes no --node, --late or --capture\n${usage}`);
const opponents = flags["--opponents"] ? JSON.parse(readFileSync(flags["--opponents"], "utf8")) : [];
for (const o of opponents)
  if (!/^[a-z][\w-]*$/.test(o.name ?? "") || o.name === OUR_NAME || !existsSync(o.bot ?? "") || !/^[WE]\d+[NS]\d+$/.test(o.room ?? ""))
    fail(`--opponents: ${JSON.stringify(o)} needs a name, a bot that exists and a room`);
if (live && (from || node || late || flags["--room"] || cores.length || flags["--unsafe"] || flags["--ended"]))
  fail(`--world takes no --from, --node, --late, --room, --core, --unsafe or --ended\n${usage}`);
if (live && opponents.some((o) => !Object.values(live.users).includes(o.name))) fail("--world: an opponent is one of the world's users, its rooms the world's");
// A world's schema is its mods' (`xx.mjs` templateOf).
const template = live ? `${TEMPLATE}-modern` : cores.length ? `${TEMPLATE}-stronghold` : TEMPLATE;
if (!existsSync(join(template, "screeps/shard0/data.json"))) fail(`no template world at ${template}: npm run xx -- setup first`);

// ---- the engine, on Node 26 through nix (as xx.mjs) ------------------------
function node26(args, cwd, capture = false) {
  const res = spawnSync("nix", ["shell", "nixpkgs#nodejs_26", "-c", "node", ...args], {
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
  ...(cores.length || live ? ["  - xxscreeps/mods/modern/stronghold"] : []),
  ...(cores.length ? ["  - ./core/index.js"] : []),
  ...(live ? ["  - xxscreeps/mods/modern/observer", "  - xxscreeps/mods/modern/powerspawn"] : []),
  "  - ./waves/index.js",
  ...(watch ? ["  - xxscreeps/mods/backend/password"] : []),
  "game:",
  "  tickSpeed: 60000",
  "  respawnTimeout: 0",
  "processor:",
  "  concurrency: 1",
  "  intentAbandonTimeout: 120000",
  "runner:",
  "  concurrency: 1",
  ...(watch ? ["backend:", `  bind: 127.0.0.1:${watch.port}`] : []),
  "",
].join("\n");

// ---- the world -------------------------------------------------------------
// One world per run, so two soaks at once never share a server. The name is
// kept short: the engine's unix sockets live under it, and a socket path has
// a length limit. The name keeps the run's head and a hash of the run and the
// checkout, so two runs of one name from two workspaces (every scenario run
// of a dist/main.js is named alike) never share a world.
const runName = (flags["--run"] ?? "default").replace(/[^A-Za-z0-9-]/g, "");
const worldName = `${runName.slice(0, 15)}-${createHash("sha256").update(REPO).update(runName).digest("hex").slice(0, 8)}`;
const world = join(ENGINE, "worlds", `soak-${worldName}`);
rmSync(world, { recursive: true, force: true });
// Node runs no exit handler on a signal, and the detached server and stepper
// never see the terminal's Ctrl-C: a signal ends the run through `exit`.
const children = [];
let samplesSoFar;
const interrupt = () => process.exit(130);
let onSignal = interrupt;
for (const s of ["SIGINT", "SIGTERM"]) process.on(s, () => onSignal());
const stop = () => {
  for (const c of children.filter((c) => c.exitCode === null && c.signalCode === null))
    try {
      process.kill(-c.pid, "SIGKILL");
    } catch {}
};
process.on("exit", (code) => {
  stop();
  // The world is derived from the template on every run, and its sockets and DB
  // are worth gigabytes over a session; --keep leaves it to be read back.
  if (!flags["--keep"]) rmSync(world, { recursive: true, force: true });
  if (code === 130 && samplesSoFar) console.error(`interrupted; the samples so far: ${samplesSoFar}`);
});
// A copied socket is a dead server's; only files and directories come over.
const filesOnly = (src) => lstatSync(src).isFile() || lstatSync(src).isDirectory();
cpSync(template, world, { recursive: true, filter: filesOnly });
if (from) {
  rmSync(join(world, "screeps"), { recursive: true });
  cpSync(from, join(world, "screeps"), { recursive: true });
}
for (const f of ["seed", "waves", "core", "colony.mjs", "node.mjs", "live.mjs", "soak-step.mjs"]) cpSync(join(HERE, "engine", f), join(world, f), { recursive: true });
writeFileSync(join(world, ".screepsrc.yaml"), screepsrc);
// Mods resolve from the cwd only where a package.json stands.
writeFileSync(join(world, "package.json"), '{ "private": true }\n');
writeFileSync(join(world, "seed.txt"), `${seed}\n`);
mkdirSync(join(world, "bot"), { recursive: true });
// `fabotTrace`: the bot logs its tactic trace (`#trace`), never live.
cpSync(bot, join(world, "bot/fabot.js"));
// xxscreeps' runtime map reads every room normal, so a world's `closed` rooms
// (an unopened area) stand in on `Game.map.getRoomStatus`.
const closed = live?.closed?.length
  ? `const closed = new Set(${JSON.stringify(live.closed)});\nconst status = Object.getPrototypeOf(Game.map).getRoomStatus;\n` +
    `Object.getPrototypeOf(Game.map).getRoomStatus = function (room, actually) {\n  return closed.has(room) ? { status: "closed", timestamp: null } : status.call(this, room, actually);\n};\n`
  : "";
if (late === 0 && windows.length === 0 && !closed) writeFileSync(join(world, "bot/main.js"), `globalThis.fabotTrace = true;\nmodule.exports = require("fabot");\n`);
else {
  // The bundle's exports are getters, so the wait and the capture wrap it
  // from outside; the capture runs after the loop, out of the bot's clock.
  cpSync(join(HERE, "engine/capture.js"), join(world, "bot/capture.js"));
  writeFileSync(
    join(world, "bot/main.js"),
    `globalThis.fabotTrace = true;\nconst bot = require("fabot");\nconst capture = require("capture");\nconst windows = ${JSON.stringify(windows)};\nlet first;\n` +
      `module.exports.loop = () => {\n  if (first === undefined) {\n${closed}  }\n  first ??= Game.time;\n  const t = Game.time - first;\n` +
      `  const memory = windows.some(([a, b]) => t >= a && t < b) ? RawMemory.get() : null;\n` +
      `  if (t >= ${late}) bot.loop();\n  if (memory !== null) console.log("#capture " + capture(memory));\n};\n`,
  );
}

if (listOnly) {
  // The rooms a soak can stand in, for `soak-many.mjs`; nothing is run.
  console.log(node26([XXSCREEPS, script("colony.mjs"), "--list"], world, true).trim().split("\n").at(-1));
  process.exit(0);
}
const homes = (live?.rooms ?? [])
  .filter((r) => r.objects.some((o) => o.type === "controller" && o.user === live.userId) && r.objects.some((o) => o.type === "spawn" && o.user === live.userId))
  .map((r) => r.name);
if (live && homes.length === 0) fail(`--world: no room of ${live.username}'s with a spawn`);
const colony = live ? { room: homes.join(",") } : from ? { room: flags["--room"] } : JSON.parse(node26([XXSCREEPS, script("colony.mjs"), ...(flags["--room"] ? [flags["--room"]] : [])], world, true).trim().split("\n").at(-1));
const run = flags["--run"] ?? `${live ? "world" : colony.room}${flags["--node"] ? `-${flags["--node"]}` : ""}-seed${seed}`;
const out = join(REPO, "soak-runs", run);
rmSync(out, { recursive: true, force: true });
mkdirSync(out, { recursive: true });
let built = null;
if (node && node.level > 1) {
  // FABOT_PLAN_BUILT: the caller built it once (soak-many's parallel runs).
  if (!process.env.FABOT_PLAN_BUILT) {
    const res = spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "plan", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: ["ignore", "ignore", "inherit"] });
    if (res.status !== 0) fail("the plan tool did not build");
  }
  const ground = node26([XXSCREEPS, script("node.mjs"), "ground", colony.room], world, true).trim().split("\n").at(-1);
  const res = spawnSync(join(REPO, "target/release/plan"), {
    input: JSON.stringify({ ...JSON.parse(ground), level: node.level }),
    encoding: "utf8",
  });
  if (res.status !== 0) fail(`the plan tool failed: ${res.stderr}`);
  built = JSON.parse(res.stdout).filter((s) => !node.without.includes(s.kind));
  if (node.moved) {
    const { terrain, sources, controller } = JSON.parse(ground);
    const range = (a, [x, y]) => Math.max(Math.abs(a.x - x), Math.abs(a.y - y));
    const used = new Set(built.map((s) => s.y * 50 + s.x));
    for (const c of built.filter((s) => s.kind === "container")) {
      const rock = sources.find((r) => range(c, r) === 1);
      const serves = (t) => (rock ? range(t, rock) === 1 : range(t, controller) <= 3 && !sources.some((r) => range(t, r) <= 1));
      const tiles = Array.from({ length: 2304 }, (_, i) => ({ x: (i % 48) + 1, y: Math.floor(i / 48) + 1 }));
      const seat = tiles.find((t) => serves(t) && terrain[t.y * 50 + t.x] !== "1" && !used.has(t.y * 50 + t.x) && range(t, rock ?? controller) > 0);
      if (seat) [c.x, c.y] = [seat.x, seat.y], used.add(seat.y * 50 + seat.x);
    }
  }
  writeFileSync(join(world, "node.json"), JSON.stringify(built));
  [colony.x, colony.y] = [built[0].x, built[0].y];
}
console.error(`colony: ${colony.room}${from ? ` from ${from}` : live ? ` from ${flags["--world"]}` : `, spawn at ${colony.x},${colony.y}`}; output ${out}`);
if (from) node26([XXSCREEPS, "manage", "bot", "update", OUR_NAME, "./bot"], world);
else if (live) {
  node26([XXSCREEPS, script("seed/manage.js"), "bot", "add", OUR_NAME, "./bot"], world);
  const flat = (rooms = []) => rooms.map((r) => ({ ...r, terrain: [r.terrain].flat().join("") }));
  writeFileSync(join(world, "capture.json"), JSON.stringify({ ...live, rooms: flat(live.rooms), terrain: flat(live.terrain) }));
  node26([XXSCREEPS, script("live.mjs"), "capture.json"], world);
}
else node26([XXSCREEPS, script("seed/manage.js"), "bot", "add", OUR_NAME, "./bot", "--spawn", colony.room, `${colony.x},${colony.y}`], world);
async function fetched(spec, dir) {
  const cache = join(CACHE, "opponents", createHash("sha256").update(spec.integrity).digest("hex").slice(0, 16));
  if (!existsSync(join(cache, ".ok"))) {
    const res = await fetch(spec.tarball);
    if (!res.ok) fail(`${spec.tarball}: ${res.status}`);
    const body = Buffer.from(await res.arrayBuffer());
    const cut = spec.integrity.indexOf("-");
    if (createHash(spec.integrity.slice(0, cut)).update(body).digest("base64") !== spec.integrity.slice(cut + 1)) fail(`${spec.tarball}: not the integrity fetch.json names`);
    rmSync(cache, { recursive: true, force: true });
    mkdirSync(cache, { recursive: true });
    writeFileSync(join(cache, "package.tgz"), body);
    if (spawnSync("tar", ["-xzf", "package.tgz"], { cwd: cache, stdio: "inherit" }).status !== 0) fail(`${spec.tarball}: tar failed`);
    writeFileSync(join(cache, ".ok"), "");
  }
  for (const f of readdirSync(join(cache, spec.from)).filter((f) => f.endsWith(".js"))) cpSync(join(cache, spec.from, f), join(dir, f === "main.js" ? `${spec.main}.js` : f));
}
for (const o of opponents) {
  const dir = join(world, `bot-${o.name}`);
  if (lstatSync(o.bot).isDirectory()) {
    cpSync(o.bot, dir, { recursive: true, filter: (f) => basename(f) !== "fetch.json" });
    if (existsSync(join(o.bot, "fetch.json"))) await fetched(JSON.parse(readFileSync(join(o.bot, "fetch.json"), "utf8")), dir);
  } else cpSync(o.bot, join(dir, "main.js"));
  if (o.config) writeFileSync(join(dir, "opponent.js"), `module.exports = ${JSON.stringify(o.config)};\n`);
  if (from || live) {
    node26([XXSCREEPS, "manage", "bot", "update", o.name, `./bot-${o.name}`], world);
    continue;
  }
  const seat = o.x === undefined ? JSON.parse(node26([XXSCREEPS, script("colony.mjs"), o.room], world, true).trim().split("\n").at(-1)) : o;
  node26([XXSCREEPS, script("seed/manage.js"), "bot", "add", o.name, `./bot-${o.name}`, "--spawn", o.room, `${seat.x},${seat.y}`], world);
}
writeFileSync(join(world, "opponents.json"), JSON.stringify(opponents.map(({ name, room, raid }) => ({ name, room, raid }))));
if (built) node26([XXSCREEPS, script("node.mjs"), "stand", colony.room, "node.json", String(node.level), String(node.stored), OUR_NAME, String(node.worn ?? 1)], world);
if (flags["--ended"]) node26([XXSCREEPS, script("node.mjs"), "ended", colony.room], world);
else if ((opponents.length && !live) || flags["--unsafe"]) node26([XXSCREEPS, script("node.mjs"), "unsafe", colony.room], world);
if ((node?.outposts || node?.claims) && colony.outposts.length === 0) fail(`${colony.room}: no neighbour fits an outpost`);
const allies = opponents.filter((o) => o.ally).map((o) => o.name);
if (node?.outposts || node?.claims || node?.labs || (allies.length && !live && !from)) {
  const claims = colony.outposts.slice(0, node?.claims ?? 0);
  const labs = node?.labs ? { labs: { home: colony.room, targets: node.labs } } : {};
  writeFileSync(join(world, "declarations.json"), JSON.stringify({ outposts: node?.outposts ? colony.outposts : [], claims, allies, ...labs }));
  node26([XXSCREEPS, script("node.mjs"), "declare", colony.room, OUR_NAME, "declarations.json", ...(node?.short ? ["short"] : [])], world);
}
if (node?.outposts && !flags["--invaders"]) waves.push(...colony.outposts.map((o, i) => `${o.room}:4000:${1000 + 1500 * i}`));
if (watch) node26([XXSCREEPS, "manage", "user", "password", OUR_NAME, "--", watch.password], world);

// ---- the server, stepped ---------------------------------------------------
const log = join(out, "server.log");
const server = spawn("nix", ["shell", "nixpkgs#nodejs_26", "-c", "node", XXSCREEPS, "start", ...(watch ? [] : ["--no-backend"])], {
  cwd: world,
  detached: true,
  stdio: ["ignore", openSync(log, "w"), openSync(log, "a")],
});
children.push(server);
let exited = false;
server.on("exit", () => (exited = true));
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const sockets = [".db", ".db.pubsub", ".shard0.db", ".shard0.pubsub", ".shard0.scratch"].map((s) => join(world, "screeps", s));
for (let i = 0; !sockets.every(existsSync); i++) {
  if (exited || i > 1200) fail(`the server did not come up; see ${log}`);
  await sleep(100);
}
if (watch) {
  const api = `http://127.0.0.1:${watch.port}`;
  for (let i = 0; !(await fetch(`${api}/api/version`).then((r) => r.ok, () => false)); i++) {
    if (exited || i > 600) fail(`the backend did not come up on ${api}; see ${log}`);
    await sleep(100);
  }
  const outposts = live?.declarations?.outposts ?? (node?.outposts || node?.claims ? colony.outposts : []);
  const rooms = [...new Set([colony.room, ...outposts.map((o) => o.room), ...opponents.map((o) => o.room)])].join(",");
  const socket = `${api.replace("http", "ws")}/socket/websocket`;
  console.error(
    [
      `\nwatch: ${api}, a tick every ${watch.ms} ms; user ${OUR_NAME} password ${watch.password}`,
      "  client, in another shell (MSC_PORT=… if 8080 is taken):",
      `    MSC_API_UPSTREAM=${api} nix run github:Xerxes-2/lazycreep#gateway`,
      "  then http://localhost:8080 → 设置 → 添加自定义 Server:",
      `    名称 fabot-watch · API 根路径 /api · WebSocket 地址 ${socket} · 分 Shard 不勾`,
      "  then 设置 → Auth token: guest",
      `  rooms (#!/<server id>/room/shard0/<room>): ${rooms}\n`,
    ].join("\n"),
  );
}
// Spawned, not run synchronously, so a signal is handled while it steps.
const stepper = spawn(
  "nix",
  ["shell", "nixpkgs#nodejs_26", "-c", "node", XXSCREEPS, script("soak-step.mjs"), String(ticks), String(every), colony.room, OUR_NAME, out, waves.join(","), "opponents.json", cores.join(","), wave, String(watch ? watch.ms : 0)],
  { cwd: world, detached: true, stdio: ["ignore", "pipe", "inherit"] },
);
children.push(stepper);
samplesSoFar = join(out, "samples.jsonl");
let steps = "";
stepper.stdout.on("data", (d) => (steps += d));
const code = await new Promise((r) => stepper.on("exit", r));
if (code !== 0) fail(`soak-step.mjs failed (exit ${code}); see ${log}`);
const stepped = JSON.parse(steps.trim().split("\n").at(-1));
if (watch) {
  console.error("watch: the last tick is stepped; the world stays up until Ctrl-C");
  await new Promise((r) => (onSignal = r));
  onSignal = interrupt;
}
for (let i = 0; !exited && !watch && i < 150; i++) await sleep(100);
if (flags["--save"]) {
  if (!exited) fail(`the server did not save on shutdown; no world in ${flags["--save"]}`);
  rmSync(flags["--save"], { recursive: true, force: true });
  cpSync(join(world, "screeps"), resolve(flags["--save"]), { recursive: true, filter: filesOnly });
}
stop();

// ---- the report ------------------------------------------------------------
const lines = (f) => readFileSync(join(out, f), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
const samples = lines("samples.jsonl");
const faults = lines("faults.jsonl");
const refusals = JSON.parse(readFileSync(join(out, "refusals.json"), "utf8"));
// Steps the engine took past the step's wait (a slow or abandoned bot tick).
const slow = JSON.parse(readFileSync(join(out, "slow.json"), "utf8"));
const levels = JSON.parse(readFileSync(join(out, "levels.json"), "utf8"));
const lost = JSON.parse(readFileSync(join(out, "lost.json"), "utf8"));

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
if (Object.keys(last.rooms).length > 1)
  for (const [name, r] of Object.entries(last.rooms))
    console.log(
      `  ${name}: dug ${r.dug} bodies ${r.bodies} net ${r.net}; reserved ${r.reserved} ticks; hostile ${r.hostile} ticks, ${r.waves} waves; ${r.creeps} creeps, ${r.roads} roads, ${r.containers} containers`,
    );
for (const f of faults.slice(0, 10)) console.log(`  fault t${f.tick} ${f.where}: ${f.message}`);
if (faults.length > 10) console.log(`  … ${faults.length - 10} more in ${join(out, "faults.jsonl")}`);
// Exit 3: no measurement, which `scenario.mjs` reruns.
if (lost.length) {
  console.error(`${run}: no measurement: ${lost.length} ticks lost to a script timeout or abandoned intents (${lost.slice(0, 10).join(", ")}${lost.length > 10 ? ", …" : ""}); see lost.json`);
  process.exit(3);
}
