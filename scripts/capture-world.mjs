// Read a block of the live world for `npm run soak -- --world` to stand in
// xxscreeps: each room's terrain and objects as the API serves them, our
// username, the tick, our Memory, and the outposts named here as v2 declares
// them (`Memory.v2.declarations`). Read-only: every call is a GET.
//
// Usage: node --env-file-if-exists=.env scripts/capture-world.mjs <rooms> [--out <file>] [--outposts r1,r2,…] [--claims r1,…] [--terrain <rooms>]
//   <rooms>     room names and rectangles, comma-separated: W1N1:W3N3 is
//               every room with a corner at each name
//   --out       the capture (default captures/capture.json), its terrain
//               cached beside it; never where jj would snapshot it, as it
//               holds our Memory and the server's names and ids
//   --outposts  rooms (among <rooms>) declared as outposts, each with its
//               controller and sources as captured
//   --claims    rooms (among <rooms>) declared as claims, in the same shape
//   --terrain   rooms (as <rooms>) whose terrain alone is taken: a bot reads
//               the terrain of rooms it never enters, which the engine throws
//               on for a room it has none of
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { connect, fail } from "./screeps-api.mjs";

const usage = "usage: capture-world.mjs <rooms> [--out <file>] [--outposts r1,r2,…] [--claims r1,…] [--terrain <rooms>]";
const args = process.argv.slice(2);
const valueOf = (flag) => (args.includes(flag) ? (args[args.indexOf(flag) + 1] ?? "") : "");
const outposts = valueOf("--outposts").split(",").filter(Boolean);
const claims = valueOf("--claims").split(",").filter(Boolean);
const [roomsArg, ...extra] = args.filter((a, i) => !a.startsWith("--") && !["--out", "--outposts", "--claims", "--terrain"].includes(args[i - 1]));
if (!roomsArg || extra.length) fail(usage);

const parse = (name) => {
  const m = /^([WE])(\d+)([NS])(\d+)$/.exec(name);
  if (!m) fail(`"${name}" is not a room name\n${usage}`);
  return { x: m[1] === "E" ? Number(m[2]) : -Number(m[2]) - 1, y: m[3] === "S" ? Number(m[4]) : -Number(m[4]) - 1 };
};
const nameOf = ({ x, y }) => `${x >= 0 ? `E${x}` : `W${-x - 1}`}${y >= 0 ? `S${y}` : `N${-y - 1}`}`;
const roomsOf = (list) => [
  ...new Set(
    list
      .split(",")
      .filter(Boolean)
      .flatMap((part) => {
        const [a, b = a] = part.split(":").map(parse);
        const names = [];
        for (let y = Math.min(a.y, b.y); y <= Math.max(a.y, b.y); y++)
          for (let x = Math.min(a.x, b.x); x <= Math.max(a.x, b.x); x++) names.push(nameOf({ x, y }));
        return names;
      }),
  ),
];
const rooms = roomsOf(roomsArg);
const terrainOnly = roomsOf(valueOf("--terrain")).filter((n) => !rooms.includes(n));
for (const o of [...outposts, ...claims]) if (!rooms.includes(o)) fail(`--outposts, --claims: ${o} is not among the rooms captured`);

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const out = resolve(valueOf("--out") || join(REPO, "captures/capture.json"));
const cache = join(dirname(out), "terrain");
for (const path of [out, cache]) {
  const inRepo = !relative(REPO, path).startsWith("..");
  if (inRepo && spawnSync("git", ["-C", REPO, "check-ignore", "-q", path]).status !== 0) fail(`--out: ${path} is in the repo and not ignored`);
}
mkdirSync(cache, { recursive: true });
const { api, shard, url } = await connect();
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
// The API limits each endpoint per hour; a pause between calls keeps a
// capture of dozens of rooms well under it.
const get = async (path, query) => {
  await sleep(300);
  return api.req("GET", path, { ...query, shard });
};

// Terrain never changes, so it is fetched once into <out.json>'s directory
// (`terrain/<name>`); objects and Memory are read each capture, in one pass.
const terrainOf = async (name) => {
  const file = join(cache, name);
  if (!existsSync(file)) writeFileSync(file, (await get("/api/game/room-terrain", { room: name, encoded: 1 })).terrain?.[0]?.terrain ?? "");
  return readFileSync(file, "utf8") || null;
};
const me = await get("/api/auth/me", {});
const ring = [];
for (const name of terrainOnly) {
  const terrain = await terrainOf(name);
  if (terrain) ring.push({ name, terrain });
}
const captured = [];
for (const name of rooms) {
  const terrain = await terrainOf(name);
  if (terrain) captured.push({ name, terrain });
  else console.error(`${name}: no terrain (out of the world); left out`);
}
const { time } = await get("/api/game/time", {});
const users = {};
for (const room of captured) {
  const objects = await get("/api/game/room-objects", { room: room.name });
  room.objects = objects.objects ?? [];
  for (const [id, u] of Object.entries(objects.users ?? {})) users[id] = u.username;
}
const memory = (await get("/api/user/memory", {})).data ?? {};
const { time: after } = await get("/api/game/time", {});

const declared = (o) => ({ id: o._id, x: o.x, y: o.y });
const declaredRoom = (name) => {
  const objects = captured.find((r) => r.name === name)?.objects ?? [];
  const controller = objects.find((o) => o.type === "controller");
  if (!controller) fail(`--outposts, --claims: ${name} has no controller`);
  return { room: name, controller: declared(controller), sources: objects.filter((o) => o.type === "source").map(declared) };
};
const declarations = { outposts: outposts.map(declaredRoom), claims: claims.map(declaredRoom) };
writeFileSync(out, JSON.stringify({ url, shard, time, username: me.username, userId: me._id, gcl: me.gcl, users, declarations, memory, rooms: captured, terrain: ring }));
console.error(`${captured.length} rooms and ${ring.length} rooms' terrain over ticks ${time}-${after} into ${out}`);
