// One live tick, read off the season's room-history replay, as an F# arena
// scenario skeleton (#532): a Trepidimous wave seen live becomes a
// regression test in minutes. The emitting is `arena-skeleton.mjs`'s; this
// reads. Reads only: the replay files, the owner names, the game time and
// the Transition log off its memory segments (#639; for each of our creeps'
// task at the tick), and, for a recent tick, the Memory leaves the replay
// cannot give.
// How to use what it writes: docs/agents/arena-capture.md.
//
// Usage:
//   capture-tick.mjs <room> <tick> [--rooms R1,R2] [--name slug] [--out path]
//     <room>        the room the incident is in; --rooms adds its neighbours
//     --name slug   names the fixture and the test (default <room>-t<tick>)
//     --out path    write the skeleton there instead of printing it
//     --live-memory / --no-live-memory
//                   seat the Raid log, ArmedSeen and Towered off live Memory;
//                   on by default when the tick is at most 300 behind live
import { existsSync, writeFileSync } from "node:fs";
import { connect, fail } from "./screeps-api.mjs";
import { readHistory, usernamesOf } from "./room-history.mjs";
import { assignmentsAt, skeleton } from "./arena-skeleton.mjs";
import { readLog } from "./observe-segments.mjs";

const usage =
  "usage: capture-tick.mjs <room> <tick> [--rooms R1,R2] [--name slug] [--out path] [--live-memory | --no-live-memory]";
// Past this, live Memory is another fight's: what it holds is not the tick's.
const RECENT = 300;
const ROOM = /^[WE]\d+[NS]\d+$/;

const rawArgs = process.argv.slice(2);
const VALUED = ["--rooms", "--name", "--out"];
const flags = {};
const args = [];
for (let i = 0; i < rawArgs.length; i++) {
  if (VALUED.includes(rawArgs[i])) {
    const value = rawArgs[++i];
    if (value === undefined || value.startsWith("--")) fail(`${rawArgs[i - 1]} needs a value\n${usage}`);
    flags[rawArgs[i - 1]] = value;
  } else if (rawArgs[i] === "--live-memory" || rawArgs[i] === "--no-live-memory") {
    flags[rawArgs[i]] = true;
  } else if (rawArgs[i].startsWith("--")) {
    fail(usage);
  } else {
    args.push(rawArgs[i]);
  }
}
const [primary, tickArg, ...extra] = args;
if (!primary || extra.length > 0) fail(usage);
if (!ROOM.test(primary)) fail(`"${primary}" is not a room name (e.g. W17S25)\n${usage}`);
const tick = Number(tickArg);
if (!(Number.isInteger(tick) && tick > 0)) fail(`capture needs a tick, e.g. capture W17S25 972544\n${usage}`);
const others = (flags["--rooms"] ?? "").split(",").filter(Boolean);
for (const r of others) if (!ROOM.test(r)) fail(`--rooms: "${r}" is not a room name`);
const roomNames = [...new Set([primary, ...others])];
if (flags["--live-memory"] && flags["--no-live-memory"]) fail(`--live-memory and --no-live-memory contradict\n${usage}`);
const slug = flags["--name"] ?? `${primary.toLowerCase()}-t${tick}`;
if (!/^[A-Za-z][A-Za-z0-9-]*$/.test(slug)) fail(`--name: "${slug}" is not a slug (letters, digits, dashes)`);

const { api, shard, url } = await connect();

// One room at a time: the replay server rate-limits a burst.
const rooms = [];
for (const name of roomNames) {
  const { href, objects } = await readHistory({ url, shard, room: name, tick }).catch((err) => fail(err.message));
  rooms.push({ name, href, objects, hasFixture: existsSync(`tests/Core.Tests/rooms/${name}.room`) });
}

const me = await api.me().catch((err) => fail(`who-am-I read failed: ${err.message ?? err}`));
const all = rooms.flatMap((r) => Object.values(r.objects));
const ids = all.flatMap((o) => [o.user, o.reservation?.user]);
const usernames = await usernamesOf(api, ids.filter((id) => id && id !== me._id && id !== "2" && id !== "3"));

const time = await api.req("GET", "/api/game/time", { shard }).catch(() => null);
const liveTick = time?.time;

// The Transition log is a ring: a creep's task at the tick is known only
// while the log still reaches back that far.
const ourCreeps = all.filter((o) => o.type === "creep" && o.user === me._id && o.hits > 0 && !o.spawning).map((o) => o.name);
let assignments = null;
if (ourCreeps.length > 0) {
  // Off its memory segments (#639), or the Memory leaf an older bundle writes.
  const creeps = await readLog(api, shard, "creeps").catch((err) => {
    console.error(`the Transition log read failed: ${err.message ?? err}`);
    return undefined;
  });
  if (creeps != null && typeof creeps === "object") {
    assignments = { ...assignmentsAt(creeps, ourCreeps, tick), readAt: liveTick };
  } else {
    console.error("no Transition log in Memory or its segments: Assignments left empty");
  }
}

// A leaf may come back gzip'd under a `gz:` prefix.
const memoryRead = async (path) => {
  const res = await api.userMemoryGet(path, shard).catch(() => null);
  let data = res?.ok === 1 ? res.data : null;
  if (typeof data === "string" && data.startsWith("gz:")) {
    const { gunzipSync } = await import("node:zlib");
    data = JSON.parse(gunzipSync(Buffer.from(data.slice(3), "base64")).toString());
  }
  return data && typeof data === "object" ? data : null;
};
const recent = liveTick !== undefined && liveTick - tick <= RECENT;
const wantLive = flags["--live-memory"] || (recent && !flags["--no-live-memory"]);
let live = null;
if (wantLive) {
  if (!recent) console.error(`t${tick} is ${liveTick - tick} ticks behind live: its live Memory is mostly another fight's`);
  const colonies = (await memoryRead("fabot.observe.colonies")) ?? {};
  // The stand-down rows, threat memories and fight records are the world
  // danger log's (ADR 0086 S2, S5), one record a room: laid into each captured
  // colony's log in its old shape, as `Danger.into` lays them into every
  // colony's, and the arena merges them back into one.
  const danger = (await memoryRead("fabot.observe.world.danger")) ?? {};
  const worldRows = { outposts: [], threatened: {}, fought: {} };
  for (const [room, entry] of Object.entries(danger)) {
    if (entry?.down != null) worldRows.outposts.push({ ...entry.down, room });
    if (entry?.threat != null) worldRows.threatened[room] = entry.threat;
    if (entry?.fight != null) worldRows.fought[room] = entry.fight;
  }
  const raids = Object.fromEntries(
    Object.entries(colonies)
      .filter(([home, leaf]) => roomNames.includes(home) && leaf?.raids)
      .map(([home, leaf]) => [
        home,
        {
          ...leaf.raids,
          outposts: [...(leaf.raids.outposts ?? []), ...worldRows.outposts],
          threatened: { ...leaf.raids.threatened, ...worldRows.threatened },
          fought: { ...leaf.raids.fought, ...worldRows.fought },
        },
      ]),
  );
  live = { readAt: liveTick, raids, rooms: (await memoryRead("fabot.observe.rooms")) ?? {} };
}

const command = `npm run capture -- ${process.argv.slice(2).filter((a, i, all) => a !== "--out" && all[i - 1] !== "--out").join(" ")}`;
const text = skeleton({ rooms, tick, ourId: me._id, usernames, assignments, live, slug, command, liveTick });

if (flags["--out"]) {
  writeFileSync(flags["--out"], text);
  console.error(`wrote ${flags["--out"]}`);
} else {
  process.stdout.write(text);
}
