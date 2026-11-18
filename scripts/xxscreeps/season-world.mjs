// A world of Season 11's real map for `npm run soak -- --world` (ARCHITECTURE.md
// §10.2): the rooms within <radius> of <room> and their sectors' centres,
// each with its terrain and its sources, minerals, controller and keeper lairs, nobody's, from a copy of the
// season's room history (its README). Season objects (Thorium, reactors) and
// what comes and goes (deposits, power banks, cores, portals) are left out.
//
// Usage: node scripts/xxscreeps/season-world.mjs <archive> <room> <radius> <out.json> [--tick T]
//   --tick  the history read for each room's objects (default the season's last)
import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const usage = "usage: season-world.mjs <archive> <room> <radius> <out.json> [--tick T]";
const args = process.argv.slice(2);
const at = args.indexOf("--tick");
const tick = at < 0 ? 1583699 : Number(args.splice(at, 2)[1]);
const [archive, center, radiusArg, out] = args;
const radius = Number(radiusArg);
const m = /^([WE])(\d+)([NS])(\d+)$/.exec(center ?? "");
if (!m || !out || !Number.isInteger(radius) || radius < 0 || !Number.isInteger(tick)) {
  console.error(usage);
  process.exit(1);
}
const cx = m[1] === "E" ? Number(m[2]) : -Number(m[2]) - 1;
const cy = m[3] === "S" ? Number(m[4]) : -Number(m[4]) - 1;
const nameOf = (x, y) => `${x >= 0 ? `E${x}` : `W${-x - 1}`}${y >= 0 ? `S${y}` : `N${-y - 1}`}`;

// 102 × 102 rooms from -51, row by row; a room's 2500 tiles, 2 bits each, low bits first.
const map = readFileSync(join(archive, "terrain.bin"));
const terrainOf = (x, y) => {
  const at = ((y + 51) * 102 + x + 51) * 625;
  const tiles = Array.from({ length: 2500 }, (_, i) => (map[at + (i >> 2)] >> ((i & 3) * 2)) & 3);
  return Array.from({ length: 50 }, (_, row) => tiles.slice(row * 50, row * 50 + 50).join(""));
};

const MINERAL_AMOUNT = { 1: 15000, 2: 35000, 3: 70000, 4: 100000 };
// Why each sector's centre 3x3 comes too: ARCHITECTURE.md §10.2.
const shown = (c) => (c >= 0 ? c : -c - 1);
const centre = (c) => (c >= 0 ? shown(c) - (shown(c) % 10) + 5 : -(shown(c) - (shown(c) % 10) + 5) - 1);
const box = [];
for (let y = cy - radius; y <= cy + radius; y++) for (let x = cx - radius; x <= cx + radius; x++) box.push([x, y]);
const sectors = box.filter(([x, y]) => shown(x) % 10 && shown(y) % 10).flatMap(([x, y]) => [-1, 0, 1].flatMap((dy) => [-1, 0, 1].map((dx) => [centre(x) + dx, centre(y) + dy])));
const rooms = [];
for (const [x, y] of new Map([...box, ...sectors].map(([x, y]) => [nameOf(x, y), [x, y]])).values()) {
    const name = nameOf(x, y);
    const res = spawnSync(process.execPath, [join(archive, "read.mjs"), "state", name, String(tick)], { encoding: "utf8", maxBuffer: 1 << 28 });
    const state = res.status === 0 && res.stdout.trim() ? Object.values(JSON.parse(res.stdout)) : [];
    if (!state.length) console.error(`${name}: no history by tick ${tick}; terrain only`);
    // A sector's keeper rooms and its centre hold 4000 a source.
    const keepers = state.some((o) => o.type === "keeperLair") || name.match(/\d+/g).every((n) => n % 10 === 5);
    const objects = [];
    for (const o of state) {
      const base = { _id: o._id, room: name, type: o.type, x: o.x, y: o.y };
      if (o.type === "controller") objects.push(base);
      else if (o.type === "keeperLair") objects.push(base);
      else if (o.type === "source") {
        const capacity = keepers ? 4000 : 1500;
        objects.push({ ...base, energy: capacity, energyCapacity: capacity });
      } else if (o.type === "mineral" && o.mineralType !== "T")
        objects.push({ ...base, mineralType: o.mineralType, density: o.density, mineralAmount: MINERAL_AMOUNT[o.density] });
    }
    rooms.push({ name, terrain: terrainOf(x, y), objects });
  }

const world = { time: 1, username: "fabot", userId: "f00000000000000000000001", gcl: 0, users: {}, memory: {}, rooms };
writeFileSync(out, JSON.stringify(world, null, 1) + "\n");
const sourced = rooms.filter((r) => r.objects.some((o) => o.type === "controller")).map((r) => `${r.name}:${r.objects.filter((o) => o.type === "source").length}`);
console.error(`${rooms.length} rooms at tick ${tick}; controllers and their sources: ${sourced.join(" ")}`);
