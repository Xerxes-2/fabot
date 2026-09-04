// One-shot capture of a room's fixed shape into a committed test fixture
// (ADR 0036): terrain plus the room's furniture — sources, controller,
// mineral — written as reviewable text under `tests/Core.Tests/rooms/`.
// The API is an authoring tool, never a test dependency: the suite loads
// the committed file and calls nothing. Config via .env (loaded by
// `node --env-file-if-exists=.env`), same as observe.mjs:
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard to capture from; when unset and the server
//                     has exactly one shard, that shard is used
//
// Usage:
//   capture-room.mjs <room>          capture into tests/Core.Tests/rooms/<room>.room
//   capture-room.mjs <room> --force  overwrite a fixture that already exists
//
// There is one correct destination and no flag to override it (ADR 0036):
// a fixture the suite cannot find is not a fixture. Structures are
// deliberately NOT captured either. What the Layout wants is the empty
// room; a live room's objects are somebody's half-built base, and a
// committed fixture carrying another player's structures rots in a way
// nobody can review.
import { writeFileSync, existsSync, mkdirSync } from "node:fs";
import { join } from "node:path";
import { ScreepsHttpClient } from "screeps-api";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

const usage = "usage: capture-room.mjs <room> [--force]";

const outDir = "tests/Core.Tests/rooms";
const rawArgs = process.argv.slice(2);
const force = rawArgs.includes("--force");
const [room, ...rest] = rawArgs.filter((arg) => arg !== "--force");
if (!room || rest.length > 0) fail(usage);
// A room name the server would reject is worth catching here rather than
// as an empty terrain response three requests later.
if (!/^[WE]\d+[NS]\d+$/.test(room)) fail(`"${room}" is not a room name (e.g. W12S28)`);

const token = process.env.SCREEPS_TOKEN;
if (!token) {
  fail("SCREEPS_TOKEN is not set. Copy .env.example to .env and fill in your token.");
}
const url = (process.env.SCREEPS_API_URL ?? "https://screeps.com/season").replace(/\/$/, "") + "/";
const api = new ScreepsHttpClient({ token, url });

let shard = process.env.SCREEPS_SHARD;
if (!shard) {
  const info = await api.req("GET", "/api/game/shards/info", {}).catch((err) => {
    fail(`shard lookup failed: ${err.message ?? err}`);
  });
  const shards = (info.shards ?? []).map((s) => s.name);
  if (shards.length !== 1) {
    fail(`server has shards [${shards.join(", ")}]; set SCREEPS_SHARD to pick one.`);
  }
  shard = shards[0];
}

const path = join(outDir, `${room}.room`);
if (existsSync(path) && !force) {
  fail(`${path} already exists; re-capturing is deliberate — pass --force to overwrite.`);
}

const time = await api.req("GET", "/api/game/time", { shard }).catch((err) => {
  fail(`tick read failed: ${err.message ?? err}`);
});
if (time.ok !== 1) fail(`tick read failed: ${JSON.stringify(time)}`);

const terrainRes = await api.gameRoomTerrain(room, shard, true).catch((err) => {
  fail(`terrain read failed: ${err.message ?? err}`);
});
if (terrainRes.ok !== 1) fail(`terrain read failed: ${JSON.stringify(terrainRes)}`);

// The encoded form is one 2500-character string, row-major: the engine's
// own terrain mask per tile, bit 1 wall and bit 2 swamp. It is written out
// verbatim in 50 rows of 50, border included, so a re-capture of unchanged
// terrain is byte-identical and the loader owns every interpretation.
const encoded = terrainRes.terrain?.[0]?.terrain;
if (typeof encoded !== "string" || encoded.length !== 2500) {
  fail(`terrain for ${room} came back as ${encoded?.length ?? "nothing"} characters, wanted 2500`);
}

const objectsRes = await api.gameRoomObjects(room, shard).catch((err) => {
  fail(`objects read failed: ${err.message ?? err}`);
});
if (objectsRes.ok !== 1) fail(`objects read failed: ${JSON.stringify(objectsRes)}`);

// The room's furniture and nothing else. Sorted so a re-capture diffs on
// what moved rather than on whatever order the server happened to answer in.
const furniture = ["source", "controller", "mineral"];
const objects = (objectsRes.objects ?? [])
  .filter((o) => furniture.includes(o.type))
  .map((o) => ({ id: o._id, type: o.type, x: o.x, y: o.y }))
  .sort((a, b) => a.type.localeCompare(b.type) || a.x - b.x || a.y - b.y);

const rows = [];
for (let y = 0; y < 50; y++) rows.push(encoded.slice(y * 50, y * 50 + 50));

const lines = [
  "# fabot room capture — ADR 0036. Terrain is the engine's own mask per",
  "# tile (bit 1 wall, bit 2 swamp), row-major, border rows included; the",
  "# loader owns the 1..48 trim and the classification. Furniture only —",
  "# no structures, by design.",
  `room\t${room}`,
  `shard\t${shard}`,
  `server\t${url}`,
  `tick\t${time.time}`,
  "",
  "[terrain]",
  ...rows,
  "",
  "[objects]",
  "id\ttype\tx\ty",
  ...objects.map((o) => `${o.id}\t${o.type}\t${o.x}\t${o.y}`),
  "",
].join("\n");

mkdirSync(outDir, { recursive: true });
writeFileSync(path, lines);

const counts = objects.reduce((acc, o) => ({ ...acc, [o.type]: (acc[o.type] ?? 0) + 1 }), {});
const summary = Object.entries(counts)
  .map(([type, n]) => `${n} ${type}${n === 1 ? "" : "s"}`)
  .join(", ");
console.log(`${path}: ${room} @ ${shard} tick ${time.time} — ${summary || "no furniture"}`);
