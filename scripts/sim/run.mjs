// One soak on `sim`, as `soak.mjs` runs one on xxscreeps: what `soak-many.mjs`
// and `scenario.mjs` share of the native engine.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { BUILT, US, built, standNode } from "./node.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const PLAN = join(REPO, "target/release/plan");
export const TEMPLATE = join(REPO, "soak-runs/.template.json");

// What sim stands (`world.rs`, `node.mjs`) and what `scenario.mjs` reads off what it writes
// (`sample.rs`: no bodies.jsonl, engagements.jsonl, opponent's side or per-home rows).
export const SIM_START = ["room", "seed", "late", "node", "ended", "world", "declarations", "memory", "gcl"];
export const SIM_MEASURES = new Set([
  "metric", "spendError", "decayedAway", "roadsLost", "dug", "upgrade", "build", "linked", "builtAtEnd", "sitesAtEnd",
  "buriedAtEnd", "unroadedAtEnd", "oldestPayingSite", "extensionsBeforeContainer", "sourceContainersAtEnd", "postedLinksAtEnd",
  "levelsLost", "spawnsLost", "fewestCreeps", "creepsAtEnd", "safeModeAt", "rising", "outpostDug", "upgradeWhileQueued",
  "upgradeStall", "leastDowngrade", "coresAtEnd", "hostilesAtEnd", "homeHostileTicks", "floodsMost", "atlasMissRate",
  "threatLookupsMost", "refusals", "faults", "heldAtEnd", "oresAtEnd",
]);

/** Whether a `worlds/`-shaped world holds a spawn of ours: a capture, not a map. */
export const seatsUs = (world) => (world.rooms ?? []).some((r) => (r.objects ?? []).some((o) => o.type === "spawn" && o.user === world.userId));

// What `engine/live.mjs` stands besides the structures a site builds: no tombstone, ruin or portal.
const LIVE_KINDS = new Set([...Object.keys(BUILT), "controller", "source", "mineral", "constructionSite", "creep", "keeperLair", "invaderCore"]);

/**
 * A capture as `engine/live.mjs` stands it on xxscreeps, so a scenario reads
 * the same start on either engine; its Memory carries its declarations.
 */
export function standCapture(world) {
  const edge = (o) => o.x === 0 || o.y === 0 || o.x === 49 || o.y === 49;
  const kept = new Map();
  for (const o of world.rooms.flatMap((r) => r.objects ?? [])) if (!kept.has(o._id) || (edge(kept.get(o._id)) && !edge(o))) kept.set(o._id, o);
  const npc = (user) => user === "2" || user === "3";
  const known = (o) => LIVE_KINDS.has(o.type) || Boolean(o.resourceType);
  const stands = (o) => kept.get(o._id) === o && known(o) && !(o.type === "creep" && (o.spawning || (o.user !== world.userId && !npc(o.user) && !o.stands)));
  const rooms = [...world.rooms.map((r) => ({ ...r, objects: (r.objects ?? []).filter(stands) })), ...(world.terrain ?? []).map((r) => ({ ...r, objects: [] }))];
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

/** This tree's sim (`npm run build:sim`), its path; null when it did not build. */
export function simBinary() {
  const res = spawnSync("node", [join(HERE, "build.mjs")], { encoding: "utf8", stdio: ["ignore", "pipe", "inherit"] });
  return res.status === 0 ? res.stdout.trim() : null;
}

/** The xxscreeps template as sim reads it; false when it did not export. */
export function exportTemplate() {
  mkdirSync(dirname(TEMPLATE), { recursive: true });
  return spawnSync("node", [join(HERE, "export.mjs"), TEMPLATE], { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
}

/** The plan tool a node is stood from, built once before runs share it. */
export const buildPlan = () =>
  spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "plan", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: "inherit" }).status === 0;

/** A soak into `out` on `world` (default the template); resolves to sim's exit code, -1 when it never ran. */
export function runSim(bin, { world, room, node, ticks, seed = 1, late = 0, ended = false, out }) {
  return new Promise((done) => {
    rmSync(out, { recursive: true, force: true });
    const base = node || world ? JSON.parse(readFileSync(world ?? TEMPLATE, "utf8")) : null;
    const seated = base && seatsUs(base);
    const captured = seated || base?.declarations || Object.keys(base?.memory ?? {}).length;
    const stood = node || captured;
    const map = stood ? `${out}.world.json` : (world ?? TEMPLATE);
    const finish = (code) => {
      if (stood) rmSync(map, { force: true });
      done(code);
    };
    room ??= seated && base.rooms.find((r) => r.objects.some((o) => o.type === "spawn" && o.user === base.userId)).name;
    try {
      if (stood) writeFileSync(map, JSON.stringify(node ? standNode(base, room, node, PLAN) : standCapture(base)));
    } catch (e) {
      console.error(`${out}: ${e.message}`);
      return finish(-1);
    }
    if (!existsSync(map)) return finish(-1);
    const args = ["--world", map, "--room", room, "--ticks", String(ticks), "--seed", String(seed), "--late", String(late), "--every", "100", "--out", out, ...(ended ? ["--ended"] : [])];
    spawn(bin, args, { stdio: "ignore" }).on("exit", finish).on("error", () => finish(-1));
  });
}
