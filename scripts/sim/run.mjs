// One soak on `sim`: what `soak.mjs`, `soak-many.mjs` and `scenario.mjs` share.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { BUILT, US, built, near, standNode } from "./node.mjs";
import { seatTile } from "../scenario/seat-tile.mjs";
import { engagements } from "../scenario/engaged.mjs";

const SAFE_MODE_DURATION = 20000;

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const PLAN = join(REPO, "target/release/plan");
/** The template world (every room its terrain and sources, minerals, controller, keeper lairs, nobody's), checked in. */
export const TEMPLATE = join(REPO, "scripts/scenario/worlds/template.json");

// What sim stands (`world.rs`, `node.mjs`) and what `scenario.mjs` reads off what it writes
// (`sample.rs`, `lives.rs`, `engaged.rs`: no opponent's side).
export const SIM_START = ["room", "seed", "late", "node", "ended", "unsafe", "world", "declarations", "memory", "gcl", "stock", "level", "follow", "opponents", "invaders", "wave", "core", "stronghold", "resetEvery"];
export const SIM_MEASURES = new Set([
  "metric", "spendError", "decayedAway", "roadsLost", "roadsAtEnd", "dug", "contactWait", "upgrade", "build", "linked", "builtAtEnd", "sitesAtEnd",
  "buriedAtEnd", "unroadedAtEnd", "oldestPayingSite", "extensionsBeforeContainer", "sourceContainersAtEnd", "postedLinksAtEnd",
  "levelsLost", "spawnsLost", "fewestCreeps", "safeModeAfterCull", "rising", "outpostDug", "upgradeWhileQueued",
  "upgradeStall", "leastDowngrade", "coresAtEnd", "hostilesAtEnd", "hostileTicks", "floodsMost", "atlasMissRate",
  "threatLookupsMost", "parkedTicks", "outpostFlips", "slotIdsShared", "refusals", "faults", "heldAtEnd", "oresAtEnd", "homeLevel", "claimSafeMode", "claimLevelOf", "claimsOwned",
  "rcl2ToTower", "claimSpawned", "claimersCast", "made", "produced", "seenAt", "extractorToOre", "boosted", "boostedWith", "bounced", "stillLoaded", "mineralAside", "mineralHeld", "carriedOut", "castsBelow",
  "spawnStuck", "scoutsCast", "scoutStill", "scoutTicksIn", "reserverOnPost", "castIn", "bodiesOut", "bodiesStruck", "struckOn",
  "deathsError", "attackersAtEnd", "creepsLostBeforeSafeMode", "predictedWinsLost", "engagements", "oscillation", "bodiesLost",
  "closedOnCore", "strongholdMisread", "strongholdDeploying", "boostedShort", "diedIn", "heldPastWave", "idleMost", "answeredMost", "sentTo", "sentBack", "draftedAfterOthers", "arrivedApart", "signsSent", "signedInRun",
  "liveEngagements", "liveAstray", "liveEnergy", "defendsOpened", "defendSpent", "rivalSpent", "answeredAfter", "recycledWounded", "culled",
]);

/** Whether a `worlds/`-shaped world holds a spawn of ours: a capture, not a map. */
export const seatsUs = (world) => (world.rooms ?? []).some((r) => (r.objects ?? []).some((o) => o.type === "spawn" && o.user === world.userId));

// What a capture stands besides the structures a site builds: no tombstone, ruin or portal.
const LIVE_KINDS = new Set([...Object.keys(BUILT), "controller", "source", "mineral", "constructionSite", "creep", "keeperLair", "invaderCore"]);

/**
 * Each room's border toward a room outside `group` walled (a capture's terrain-only ring is a group
 * of its own), as the official map closes its edge: an exit leads to a room or nowhere, and Layout plans by it.
 */
function seal(group) {
  const names = new Set(group.map((r) => r.name));
  for (const r of group) {
    const tiles = [...[r.terrain].flat().join("")];
    const [, north, , west, east, , south] = near(r.name, 1);
    const sides = [[north, (i) => i], [south, (i) => 2450 + i], [west, (i) => i * 50], [east, (i) => i * 50 + 49]];
    for (const [next, tile] of sides) if (!names.has(next)) for (let i = 0; i < 50; i++) tiles[tile(i)] = "1";
    r.terrain = tiles.join("");
  }
}

export function standCapture(world) {
  const edge = (o) => o.x === 0 || o.y === 0 || o.x === 49 || o.y === 49;
  const kept = new Map();
  for (const o of world.rooms.flatMap((r) => r.objects ?? [])) if (!kept.has(o._id) || (edge(kept.get(o._id)) && !edge(o))) kept.set(o._id, o);
  const npc = (user) => user === "2" || user === "3";
  const known = (o) => LIVE_KINDS.has(o.type) || Boolean(o.resourceType);
  const stands = (o) => kept.get(o._id) === o && known(o) && !(o.type === "creep" && (o.spawning || (o.user !== world.userId && !npc(o.user) && !o.stands)));
  const captured = world.rooms.map((r) => ({ ...r, objects: (r.objects ?? []).filter(stands) }));
  const ring = (world.terrain ?? []).map((r) => ({ ...r, objects: [] }));
  seal(captured);
  seal(ring);
  const rooms = [...captured, ...ring];
  // A crafted world leaves out what live.mjs's constructors fill in: a structure's are what its site
  // builds, a creep's its body's.
  const CARRY_BOOST = { KH: 2, KH2O: 3, XKH2O: 4 };
  for (const r of rooms) {
    const terrain = [r.terrain].flat().join("");
    const level = r.objects.find((o) => o.type === "controller")?.level ?? 0;
    for (const o of r.objects) {
      if (BUILT[o.type]) for (const [k, v] of Object.entries(built(o.type, terrain[o.y * 50 + o.x], world.time, level))) o[k] ??= v;
      if (o.type !== "creep") continue;
      o.storeCapacity ??= o.body.reduce((n, p) => n + (p.type === "carry" ? 50 * (CARRY_BOOST[p.boost] ?? 1) : 0), 0);
      o.hitsMax ??= 100 * o.body.length;
      o.hits ??= o.hitsMax;
      o.fatigue ??= 0;
    }
  }
  // A safe mode the capture holds is kept, never the respawn's.
  for (const o of rooms.flatMap((r) => r.objects)) if (o.type === "controller" && o.user === world.userId) o.safeMode ??= 0;
  const simMemory = { ...world.memory, v2: { ...world.memory?.v2, declarations: world.declarations } };
  // An id the bot reads is hex, as the engine writes one; a crafted world's need not be.
  const crafted = rooms.flatMap((r) => r.objects.map((o) => o._id)).filter((id) => !/^[0-9a-f]{1,24}$/.test(id));
  let text = JSON.stringify({ ...world, rooms, simMemory }).replaceAll(`"${world.userId}"`, `"${US}"`);
  for (const id of crafted) text = text.replaceAll(`"${id}"`, `"${createHash("sha256").update(id).digest("hex").slice(0, 24)}"`);
  return JSON.parse(text);
}

/** Why sim cannot seat `rival` (a scenario's opponent), or null (ARCHITECTURE.md §10.2). */
export const unseated = (rival) => (rival.bot === "rival" && rival.x === undefined ? null : `${rival.name} plays`);

const rivalId = (i) => (0xe00000 + i).toString(16);
const PARTS = ["tough", "move", "work", "carry", "attack", "ranged_attack", "heal"];
/** sim's `--rivals` (`sim/src/rival`): each rival's user, room, config and raid, a raid's bodies part by part. */
const seats = (rivals) =>
  rivals.map(({ room, config, raid }, i) => ({ id: rivalId(i), room, ...config, raid: raid && { ...raid,
    bodies: raid.bodies.map(({ boosts = {}, ...counts }) => PARTS.flatMap((type) => Array(counts[type] ?? 0).fill({ type, hits: 100, boost: boosts[type] }))) } }));

/** `rival`'s room as `manage bot add` seats it: its controller at level 1 in safe mode, a spawn on `seatTile`. */
function standRival(world, rival, i) {
  const id = rivalId(i);
  world.users = { ...world.users, [id]: rival.name };
  const home = world.rooms.find((r) => r.name === rival.room);
  const terrain = [home.terrain].flat().join("");
  const controller = home.objects.find((o) => o.type === "controller");
  const anchors = [controller, ...home.objects.filter((o) => o.type === "source")];
  const seat = seatTile((x, y) => Number(terrain[y * 50 + x]), new Set(home.objects.map((o) => `${o.x},${o.y}`)), anchors);
  const t = world.time ?? 0;
  Object.assign(controller, { user: id, level: 1, progress: 0, downgradeTime: t + 20000, safeMode: t + SAFE_MODE_DURATION, safeModeAvailable: 0 });
  home.objects.push({ _id: `${id}ffff`, type: "spawn", room: rival.room, x: seat.x, y: seat.y, name: "Spawn1", user: id,
    ...built("spawn", "0", t, 1), store: { energy: 300 }, spawning: null, notifyWhenAttacked: true });
}

// cronjobs.js createCreep's bodies, by `{size}{Role}`: part letters and counts.
const INVADER = { bigHealer: "m24h25m1", bigRanged: "t6m24r18w1m1", bigMelee: "t16m24r3w4a2m1", smallHealer: "m4h5m1", smallRanged: "t2m4r3m1", smallMelee: "t2m4r1w1a1m1" };
const PART = { m: "move", h: "heal", r: "ranged_attack", w: "work", a: "attack", t: "tough" };
/** `soak.mjs`'s `--wave role[:size],…` as the official engine's Invader bodies. */
const bodies = (wave = "melee,healer,ranged") =>
  wave.split(",").map((spec) => {
    const [role, size = "small"] = spec.split(":");
    return [...INVADER[`${size}${role[0].toUpperCase()}${role.slice(1)}`].matchAll(/([a-z])(\d+)/g)].flatMap(([, p, n]) => Array(Number(n)).fill(PART[p]));
  });

/**
 * `soak.mjs`'s `--invaders room:every[:first],…` and `--core room:tick,…`, and a scenario's `stronghold` `room:tick:level[:deploy],…`
 * (sim alone stands one), as sim's `--npc`: [tick, room, a wave's bodies, [a stronghold's level, deploy] or null for a core].
 */
function npc(invaders = "", wave, core = "", stronghold = "", ticks) {
  const waves = invaders.split(",").filter(Boolean).flatMap((w) => {
    const [room, every, first = every] = w.split(":").map((v, i) => (i ? Number(v) : v));
    return Array.from({ length: Math.max(0, Math.ceil((ticks - first) / every)) }, (_, i) => [first + i * every, room, wave]);
  });
  const strongholds = stronghold.split(",").filter(Boolean).map((c) => c.split(":")).map(([room, tick, level, deploy = 1]) => [Number(tick), room, [Number(level), Number(deploy)]]);
  return [...waves, ...core.split(",").filter(Boolean).map((c) => [Number(c.split(":")[1]), c.split(":")[0], null]), ...strongholds];
}

/** `engagements.jsonl` off the ticks sim wrote to `fight.jsonl`, and `segment4.jsonl` off `cpu.jsonl`. */
async function engaged(out) {
  const rows = [];
  // Every room's objects of the tick being fed: an engagement closes inside `fights.tick`.
  let tick = { objects: [] };
  const fights = engagements((row) => rows.push(JSON.stringify(row) + "\n"), async (rooms) => tick.objects.filter((o) => !rooms || rooms.includes(o.room)));
  for (const line of readFileSync(join(out, "fight.jsonl"), "utf8").split("\n").filter(Boolean)) {
    tick = JSON.parse(line);
    await fights.tick(tick.t, tick.fight, tick.objects, new Set(tick.struck));
  }
  await fights.end();
  writeFileSync(join(out, "engagements.jsonl"), rows.join(""));
  // The bot's own `engagements.log` rows (segment 4), as each tick closed them.
  const closed = readFileSync(join(out, "cpu.jsonl"), "utf8").split("\n").filter(Boolean).flatMap((l) => JSON.parse(l).closed ?? []);
  writeFileSync(join(out, "segment4.jsonl"), closed.map((r) => r + "\n").join(""));
}

/** This tree's sim (`npm run build:sim`), its path; null when it did not build. */
export function simBinary() {
  const res = spawnSync("node", [join(HERE, "build.mjs")], { encoding: "utf8", stdio: ["ignore", "pipe", "inherit"] });
  return res.status === 0 ? res.stdout.trim() : null;
}

/** The plan tool a node is stood from, built once before runs share it. */
export const buildPlan = () =>
  spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "plan", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: "inherit" }).status === 0;

/**
 * A soak into `out` on `world` (default the template), or on the world map.js generates off seed `gen`
 * (sim picks the room); resolves to sim's exit code, -1 when it never ran. `watch` ({port, ms, badge}) serves it to lazycreep;
 * `capture` ("a-b[,c-d]") writes those ticks to `out`/capture.jsonl for `scripts/bench.mjs`.
 */
export function runSim(bin, { world, gen, room, node, ticks, seed = 1, late = 0, ended = false, unsafe = false, follow = [], rivals = [], players, invaders, wave, core, stronghold, resetEvery = 0, watch, capture, out }) {
  return new Promise((done) => {
    rmSync(out, { recursive: true, force: true });
    const map = `${out}.world.json`;
    const finish = (code) => {
      rmSync(map, { force: true });
      done(code);
    };
    const play = (where) => {
      const args = [...where, "--ticks", String(ticks), "--seed", String(seed), "--late", String(late), "--every", "100", "--out", out, ...(follow.length ? ["--follow", follow.join(",")] : []), ...(ended ? ["--ended"] : []), "--reset-every", String(resetEvery), ...(capture ? ["--capture", capture] : []), "--rivals", JSON.stringify(seats(rivals)), ...(players ? ["--players", players] : []),
        "--npc", JSON.stringify(npc(invaders, bodies(wave), core, stronghold, ticks)), ...(watch ? ["--watch", "--port", String(watch.port), "--tick-ms", String(watch.ms), ...(watch.badge ? ["--badge", watch.badge] : [])] : [])];
      spawn(bin, args, { stdio: watch ? "inherit" : "ignore" }).on("exit", (code, signal) => (code === 0 || (watch && signal === "SIGINT" && existsSync(join(out, "timing.json"))) ? engaged(out).then(() => finish(0), () => finish(-1)) : finish(code))).on("error", () => finish(-1));
    };
    if (gen !== undefined) return play(["--gen", String(gen)]);
    const base = JSON.parse(readFileSync(world ?? TEMPLATE, "utf8"));
    const seated = seatsUs(base);
    const captured = seated || base.declarations || Object.keys(base.memory ?? {}).length;
    if (!captured) seal(base.rooms);
    if (seated) room = base.rooms.find((r) => r.objects.some((o) => o.type === "spawn" && o.user === base.userId)).name;
    try {
      const stood = node ? standNode(base, room, node, PLAN) : captured ? standCapture(base) : base;
      rivals.forEach((rival, i) => standRival(stood, rival, i));
      // Beside a rival or asked: no safe mode to decide the fight for us, none to activate.
      if ((rivals.length || unsafe) && !seated && !ended)
        for (const c of stood.rooms.find((r) => r.name === room).objects.filter((o) => o.type === "controller"))
          Object.assign(c, { safeMode: 0, safeModeAvailable: 0 });
      writeFileSync(map, JSON.stringify(stood));
    } catch (e) {
      console.error(`${out}: ${e.message}`);
      return finish(-1);
    }
    if (!existsSync(map)) return finish(-1);
    play(["--world", map, "--room", room]);
  });
}
