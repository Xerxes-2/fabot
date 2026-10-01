// One-shot capture of a room's fixed shape into a committed test fixture
// (ADR 0036): terrain plus the room's furniture — sources, controller,
// mineral — written as reviewable text under `tests/Core.Tests/rooms/`.
// The API is an authoring tool, never a test dependency: the suite loads
// the committed file and calls nothing. The server connection and its .env
// config are `screeps-api.mjs`'s.
//
// Usage:
//   capture-room.mjs <room>                capture into tests/Core.Tests/rooms/<room>.room
//   capture-room.mjs <room> --force        overwrite a fixture that already exists
//   capture-room.mjs <room> --structures   also write the room's structures (#465)
//
// There is one correct destination and no flag to override it (ADR 0036):
// a fixture the suite cannot find is not a fixture. Structures are NOT
// captured by default: what the Layout wants is the empty room, and a live
// room's objects are somebody's half-built base. `--structures` is the
// arena's (#465): a siege is played against the base as it stood, so the
// capture says which tick it stood at, and the Layout's loader ignores the
// section. Roads and containers are left out: neither owned nor an
// obstacle, they bar nothing a siege breaks.
import { writeFileSync, existsSync, mkdirSync } from "node:fs";
import { join } from "node:path";
import { connect, fail } from "./screeps-api.mjs";

const usage = "usage: capture-room.mjs <room> [--force] [--structures]";

const outDir = "tests/Core.Tests/rooms";
const rawArgs = process.argv.slice(2);
const force = rawArgs.includes("--force");
const withStructures = rawArgs.includes("--structures");
const [room, ...rest] = rawArgs.filter((arg) => arg !== "--force" && arg !== "--structures");
if (!room || rest.length > 0) fail(usage);
// A room name the server would reject is worth catching here rather than
// as an empty terrain response three requests later.
if (!/^[WE]\d+[NS]\d+$/.test(room)) fail(`"${room}" is not a room name (e.g. W12S28)`);

const { api, shard, url } = await connect();

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
//
// Each row carries the resource the object holds, because a mineral's type is
// the one thing about this furniture the coordinates do not say and a rule
// depends on: the season mod stands a Thorium deposit beside the room's
// ordinary ore, and only the Thorium one is ever projected (ADR 0057). A
// source holds energy and a controller holds nothing, spelt "-", so the table
// is one shape rather than a ragged one.
const furniture = ["source", "controller", "mineral"];
// A mineral with no `mineralType` is the server answering something this
// script does not understand, and a "?" written into the file would be
// swallowed by both readers as an ore that is simply not Thorium. Throwing is
// the only spelling that says so.
const resourceOf = (o) => {
  if (o.type !== "mineral") return o.type === "source" ? "energy" : "-";
  if (!o.mineralType)
    throw new Error(`mineral ${o._id} at ${o.x},${o.y} has no mineralType`);
  return o.mineralType;
};
const objects = (objectsRes.objects ?? [])
  .filter((o) => furniture.includes(o.type))
  .map((o) => ({ id: o._id, type: o.type, x: o.x, y: o.y, resource: resourceOf(o) }))
  .sort((a, b) => a.type.localeCompare(b.type) || a.x - b.x || a.y - b.y);

const rows = [];
for (let y = 0; y < 50; y++) rows.push(encoded.slice(y * 50, y * 50 + 50));

// The base as it stood (#465): every structure but the roads and the
// containers, with its owner's username ("-" for a wall nobody owns), its
// hits, the energy it holds, whether a rampart is public, and a rampart's
// next decay tick (0 for none). The controller's holder, level and banked
// safe modes ride in the header, since the controller is furniture above.
const usernameOf = (userId) => objectsRes.users?.[userId]?.username ?? "-";
const skipped = ["road", "container", "creep", "powerCreep", ...furniture];
const structures = withStructures
  ? (objectsRes.objects ?? [])
      .filter((o) => o.hitsMax !== undefined && !skipped.includes(o.type))
      .map((o) => ({
        id: o._id,
        type: o.type,
        x: o.x,
        y: o.y,
        owner: o.user ? usernameOf(o.user) : "-",
        hits: o.hits,
        hitsMax: o.hitsMax,
        energy: o.store?.energy ?? 0,
        public: o.isPublic ? 1 : 0,
        decay: o.nextDecayTime ?? 0,
      }))
      .sort((a, b) => a.type.localeCompare(b.type) || a.x - b.x || a.y - b.y)
  : [];
const controller = (objectsRes.objects ?? []).find((o) => o.type === "controller");
const holder =
  withStructures && controller?.user
    ? [
        `owner\t${usernameOf(controller.user)}`,
        `level\t${controller.level}`,
        `safeModes\t${controller.safeModeAvailable ?? 0}`,
      ]
    : [];

const lines = [
  "# fabot room capture — ADR 0036. Terrain is the engine's own mask per",
  "# tile (bit 1 wall, bit 2 swamp), row-major, border rows included; the",
  ...(withStructures
    ? [
        "# loader owns the 1..48 trim and the classification. The [structures]",
        "# section is the base as it stood at this tick, for the arena (#465).",
      ]
    : [
        "# loader owns the 1..48 trim and the classification. Furniture only —",
        "# no structures, by design.",
      ]),
  `room\t${room}`,
  `shard\t${shard}`,
  `server\t${url}`,
  `tick\t${time.time}`,
  ...holder,
  "",
  "[terrain]",
  ...rows,
  "",
  "[objects]",
  "id\ttype\tx\ty\tresource",
  ...objects.map((o) => `${o.id}\t${o.type}\t${o.x}\t${o.y}\t${o.resource}`),
  "",
  ...(withStructures
    ? [
        "[structures]",
        "id\ttype\tx\ty\towner\thits\thitsMax\tenergy\tpublic\tdecay",
        ...structures.map((s) =>
          [s.id, s.type, s.x, s.y, s.owner, s.hits, s.hitsMax, s.energy, s.public, s.decay].join("\t"),
        ),
        "",
      ]
    : []),
].join("\n");

mkdirSync(outDir, { recursive: true });
writeFileSync(path, lines);

const counts = objects.reduce((acc, o) => ({ ...acc, [o.type]: (acc[o.type] ?? 0) + 1 }), {});
const summary = Object.entries(counts)
  .map(([type, n]) => `${n} ${type}${n === 1 ? "" : "s"}`)
  .join(", ");
const built = withStructures ? `; ${structures.length} structures` : "";
console.log(`${path}: ${room} @ ${shard} tick ${time.time} — ${summary || "no furniture"}${built}`);
