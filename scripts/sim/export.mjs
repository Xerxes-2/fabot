// The xxscreeps template world (`npm run xx -- setup`) as a `worlds/`-shaped
// JSON, for the native engine (`sim/`): every room's terrain and its sources,
// minerals, controller, keeper lairs and extractors, nobody's.
//
// Usage: node scripts/sim/export.mjs <out.json>
import { spawnSync } from "node:child_process";
import { cpSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { node26, node26Env } from "../xxscreeps/node26.mjs";

const DUMP = `
import * as fs from "node:fs";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const world = await shard.loadWorld();
const rooms = [];
for (const name of (await shard.data.sMembers("rooms")).sort()) {
  const room = await shard.loadRoom(name, shard.time);
  const t = world.map.getRoomTerrain(name);
  const terrain = Array.from({ length: 50 }, (_, y) => Array.from({ length: 50 }, (_, x) => t.get(x, y)).join(""));
  const objects = [];
  for (const o of room["#objects"]) {
    const at = { _id: o.id, room: name, x: o.pos.x, y: o.pos.y };
    if (o.structureType === "controller") objects.push({ ...at, type: "controller" });
    else if (o.structureType === "keeperLair") objects.push({ ...at, type: "keeperLair" });
    else if (o["#lookType"] === "source") objects.push({ ...at, type: "source", energy: o.energyCapacity, energyCapacity: o.energyCapacity });
    else if (o["#lookType"] === "mineral") objects.push({ ...at, type: "mineral", mineralType: o.mineralType, density: o.density, mineralAmount: o.mineralAmount });
    else if (o.structureType === "extractor") objects.push({ ...at, type: "extractor" });
    else throw new Error(name + ": an object the export does not know: " + (o.structureType ?? o["#lookType"]));
  }
  rooms.push({ name, terrain, objects });
}
fs.writeFileSync(process.argv[2], JSON.stringify({ time: shard.time, rooms }) + "\\n");
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
`;

const out = process.argv[2];
if (!out) throw new Error("usage: export.mjs <out.json>");
const CACHE = process.env.FABOT_XXSCREEPS_HOME ?? join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "fabot-xxscreeps");
const ENGINE = join(CACHE, "engine");
const world = mkdtempSync(join(ENGINE, "worlds", "export-"));
try {
  cpSync(join(ENGINE, "template"), world, { recursive: true });
  writeFileSync(join(world, ".screepsrc.yaml"), "mods:\n  - xxscreeps/mods/classic\n");
  writeFileSync(join(world, "package.json"), '{ "private": true }\n');
  writeFileSync(join(world, "dump.mjs"), DUMP);
  const res = spawnSync(node26(), [join(ENGINE, "node_modules/xxscreeps/xxscreeps.js"), `file://${join(world, "dump.mjs")}`, resolve(out)], { cwd: world, env: node26Env(), stdio: "inherit" });
  if (res.status !== 0) throw new Error(`dump failed (exit ${res.status})`);
} finally {
  rmSync(world, { recursive: true, force: true });
}
