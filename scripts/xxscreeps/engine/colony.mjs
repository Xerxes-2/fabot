// Offline, server stopped: choose where the soak's colony stands. Runs under
// xxscreeps' own loader, cwd the world directory:
//   xxscreeps.js file://…/colony.mjs [<room>]
// The room is the given one, or the first by name with an unowned controller
// and two sources. The spawn's tile is the plain one at range ≥ 3 from every
// source, the controller and the edges, with no wall around it, nearest (sum
// of ranges) to the sources and the controller; ties go to the lower y, then x.
// Prints `{ "room": …, "x": …, "y": … }` and changes nothing: `manage bot add
// --spawn` stands it.
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import { Source } from "xxscreeps/mods/classic/source/source.js";

const [wanted] = process.argv.slice(2);
const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const world = await shard.loadWorld();
const accessible = await shard.data.sMembers("rooms");

const range = (a, b) => Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y));

async function survey(name) {
  const room = await shard.loadRoom(name, shard.time).catch(() => null);
  if (!room) return null;
  const objects = [...room["#objects"]];
  const controller = objects.find((o) => o.structureType === "controller");
  const sources = objects.filter((o) => o instanceof Source);
  return { objects, controller, sources };
}

function spawnTile(name, { objects, controller, sources }) {
  const terrain = world.map.getRoomTerrain(name);
  const wall = (x, y) => (terrain.get(x, y) & 1) === 1;
  const taken = new Set(objects.map((o) => `${o.pos.x},${o.pos.y}`));
  const anchors = [controller.pos, ...sources.map((s) => s.pos)];
  let best = null;
  for (let y = 3; y <= 46; y++) {
    for (let x = 3; x <= 46; x++) {
      if (terrain.get(x, y) !== 0 || taken.has(`${x},${y}`)) continue;
      if (anchors.some((a) => range(a, { x, y }) < 3)) continue;
      let open = true;
      for (let dy = -1; dy <= 1 && open; dy++) for (let dx = -1; dx <= 1; dx++) if (wall(x + dx, y + dy)) open = false;
      if (!open) continue;
      const score = anchors.reduce((sum, a) => sum + range(a, { x, y }), 0);
      if (best === null || score < best.score) best = { x, y, score };
    }
  }
  return best;
}

const candidates = wanted ? [wanted] : [...accessible].sort();
let chosen = null;
for (const name of candidates) {
  const s = await survey(name);
  const fits = s?.controller && s.sources.length === 2 && !s.controller["#user"];
  if (!fits) {
    if (wanted) throw new Error(`${name}: needs an unowned controller and two sources`);
    continue;
  }
  const tile = spawnTile(name, s);
  if (tile) {
    chosen = { room: name, x: tile.x, y: tile.y };
    break;
  }
  if (wanted) throw new Error(`${name}: no tile fits a spawn`);
}
if (!chosen) throw new Error("no room in this world has an unowned controller, two sources and a spawn tile");
console.log(JSON.stringify(chosen));
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
