// One soak on `sim`, as `soak.mjs` runs one on xxscreeps: what `soak-many.mjs`
// and `scenario.mjs` share of the native engine.
import { spawn, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { standNode } from "./node.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const PLAN = join(REPO, "target/release/plan");
export const TEMPLATE = join(REPO, "soak-runs/.template.json");

// What sim stands (`world.rs`, `node.mjs`) and what `scenario.mjs` reads off what it writes
// (`sample.rs`: no bodies.jsonl, engagements.jsonl or opponent's side).
export const SIM_START = ["room", "seed", "late", "node", "ended", "world"];
export const SIM_MEASURES = new Set([
  "metric", "spendError", "decayedAway", "roadsLost", "homeLevel", "dug", "upgrade", "build", "linked", "builtAtEnd", "sitesAtEnd",
  "buriedAtEnd", "unroadedAtEnd", "oldestPayingSite", "extensionsBeforeContainer", "sourceContainersAtEnd", "postedLinksAtEnd",
  "levelsLost", "spawnsLost", "fewestCreeps", "creepsAtEnd", "safeModeAt", "rising", "outpostDug", "upgradeWhileQueued",
  "upgradeStall", "leastDowngrade", "coresAtEnd", "hostilesAtEnd", "homeHostileTicks", "floodsMost", "atlasMissRate",
  "threatLookupsMost", "refusals", "faults",
]);

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
    const map = node ? `${out}.world.json` : (world ?? TEMPLATE);
    const finish = (code) => {
      if (node) rmSync(map, { force: true });
      done(code);
    };
    try {
      if (node) writeFileSync(map, JSON.stringify(standNode(JSON.parse(readFileSync(world ?? TEMPLATE, "utf8")), room, node, PLAN)));
    } catch (e) {
      console.error(`${out}: ${e.message}`);
      return finish(-1);
    }
    if (!existsSync(map)) return finish(-1);
    const args = ["--world", map, "--room", room, "--ticks", String(ticks), "--seed", String(seed), "--late", String(late), "--every", "100", "--out", out, ...(ended ? ["--ended"] : [])];
    spawn(bin, args, { stdio: "ignore" }).on("exit", finish).on("error", () => finish(-1));
  });
}
