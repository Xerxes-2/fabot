// The operator's view of the live bot: the observe segment, the switches, and
// the recorder's ring (ARCHITECTURE.md §4, §9.1, §11). Server and .env config
// are screeps-api.mjs's.
//
// Usage:
//   observe.mjs health             tick, version, bucket, CPU, faults, refusals, modules off
//   observe.mjs cpu                CPU by phase and module, this tick
//   observe.mjs faults             the fault ring, newest first
//   observe.mjs switches           Memory.v2.switches as stored
//   observe.mjs switch <name> on|off|clear
//                                  off: `false` (the module is skipped); on: `true`
//                                  (the recorder's opt-in); clear: remove the key
//   observe.mjs declare [reserve <n> | allies <name,…> | enemies <name,…> | sieges <room,…> | outposts <capture.json> | claims <capture.json> | labs <room> <compound=n,…>]
//                                  Memory.v2.declarations as stored; with a name, set it
//                                  (outposts: the capture's, `scripts/capture-world.mjs`)
//                                  (reserve: creep lifetimes of tower fire the reserve holds)
//   observe.mjs capture [out.jsonl]
//                                  the recorder's ring (segments 10..14) as `npm run bench`'s
//                                  capture, oldest first; terrain cached in captures/terrain
//   observe.mjs wait --ticks N     return after N game ticks
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { gunzipSync } from "node:zlib";
import { unpack } from "./recorder.mjs";
import { connect, fail } from "./screeps-api.mjs";

const OBSERVE = 1;
const RECORDER = [10, 11, 12, 13, 14];

const [cmd, ...args] = process.argv.slice(2);
const { api, shard } = await connect();

const raw = async (id) => (await api.req("GET", "/api/user/memory-segment", { segment: id, shard })).data || null;
const segment = async (id) => JSON.parse((await raw(id)) ?? "null");
const memory = async (path) => {
  const res = await api.req("GET", "/api/user/memory", { path, shard });
  // The server's answer for a path under a key Memory does not hold.
  if (typeof res.data !== "string" || res.data === "Incorrect memory path") return typeof res.data === "string" ? null : (res.data ?? null);
  return res.data.startsWith("gz:") ? JSON.parse(gunzipSync(Buffer.from(res.data.slice(3), "base64")).toString()) : JSON.parse(res.data);
};
// The server runs `Memory.v2.<key> = value`, which throws while Memory holds no v2.
const putV2 = async (key, value) =>
  (await memory("v2"))
    ? api.req("POST", "/api/user/memory", { path: `v2.${key}`, value, shard })
    : api.req("POST", "/api/user/memory", { path: "v2", value: { [key]: value }, shard });
const observed = async () => (await segment(OBSERVE)) ?? fail("observe segment empty: the bot has not written one yet");
const ms = (n) => n.toFixed(2).padStart(7);

switch (cmd) {
  case "health": {
    const o = await observed();
    const now = (await api.req("GET", "/api/game/time", { shard })).time;
    console.log(`tick ${o.tick} (server ${now}, ${now - o.tick} behind) · ${o.version} · bucket ${o.bucket} · cpu ${o.cpu.toFixed(2)}`);
    console.log(`off: ${o.off.length ? o.off.join(", ") : "none"}`);
    console.log(`faults: ${o.faults.length} in ring${o.faults[0] ? `, newest t${o.faults[0].tick} ${o.faults[0].where}: ${o.faults[0].message}` : ""}`);
    console.log(`refusals this tick: ${o.refusals.length}`);
    break;
  }
  case "cpu": {
    const o = await observed();
    for (const [name, spent] of o.phases) console.log(`${ms(spent)}  ${name}`);
    console.log(`${ms(o.cpu)}  total (tick ${o.tick})`);
    break;
  }
  case "faults": {
    const o = await observed();
    if (o.faults.length === 0) console.log("no faults in the ring");
    for (const f of o.faults) console.log(`t${f.tick}  ${f.where}  ${f.message}`);
    break;
  }
  case "switches":
    console.log(JSON.stringify((await memory("v2.switches")) ?? {}, null, 2));
    break;
  case "declare": {
    const [name, value, more] = args;
    const stored = await memory("v2.declarations");
    const declarations = stored ?? {};
    if (name === undefined) {
      console.log(JSON.stringify(declarations, null, 2));
      break;
    }
    const n = Number(value);
    if (name === "reserve" && n >= 0) declarations.reserve = n;
    else if ((name === "allies" || name === "enemies" || name === "sieges") && value !== undefined) declarations[name] = value.split(",").filter(Boolean);
    else if ((name === "outposts" || name === "claims") && value !== undefined) declarations[name] = JSON.parse(readFileSync(value, "utf8")).declarations[name] ?? [];
    else if (name === "labs" && value !== undefined && more !== undefined)
      declarations.labs = { home: value, targets: Object.fromEntries(more.split(",").map((p) => p.split("=")).map(([c, n]) => [c, Number(n)])) };
    else fail("usage: observe declare [reserve <n ≥ 0> | allies <name,…> | enemies <name,…> | sieges <room,…> | outposts <capture.json> | claims <capture.json> | labs <room> <compound=n,…>]");
    // A memory write is not read back at once: a whole-object write would
    // revert a key declared just before it.
    await (stored ? putV2(`declarations.${name}`, declarations[name]) : putV2("declarations", declarations));
    console.log(`v2.declarations = ${JSON.stringify(declarations)} (read next tick)`);
    break;
  }
  case "switch": {
    const [name, state] = args;
    if (!name || !["on", "off", "clear"].includes(state)) fail("usage: observe switch <name> on|off|clear");
    const switches = (await memory("v2.switches")) ?? {};
    if (state === "clear") delete switches[name];
    else switches[name] = state === "on";
    await putV2("switches", switches);
    console.log(`v2.switches = ${JSON.stringify(switches)} (read next tick)`);
    break;
  }
  case "capture": {
    const ticks = unpack(await Promise.all(RECORDER.map(raw)));
    if (ticks.length === 0) fail("recorder ring empty: observe switch recorder on, then wait a few ticks");
    const cache = new URL("../captures/terrain/", import.meta.url);
    mkdirSync(cache, { recursive: true });
    for (const room of ticks.flatMap((t) => t.rooms)) {
      const file = new URL(room.name, cache);
      if (!existsSync(file))
        writeFileSync(file, (await api.req("GET", "/api/game/room-terrain", { room: room.name, encoded: 1, shard })).terrain?.[0]?.terrain ?? "");
      room.terrain = readFileSync(file, "utf8");
    }
    const text = ticks.map((t) => JSON.stringify(t)).join("\n") + "\n";
    if (args[0]) writeFileSync(args[0], text);
    else process.stdout.write(text);
    console.error(`captured ticks ${ticks.map((t) => t.time).join(", ")}`);
    break;
  }
  case "wait": {
    const n = Number(args[args.indexOf("--ticks") + 1]);
    if (!Number.isInteger(n) || n < 1) fail("usage: observe wait --ticks N");
    const time = async () => (await api.req("GET", "/api/game/time", { shard })).time;
    const until = (await time()) + n;
    while ((await time()) < until) await new Promise((r) => setTimeout(r, 3000));
    console.log(`tick ${until} reached`);
    break;
  }
  default:
    fail("usage: observe health|cpu|faults|switches|switch <name> on|off|clear|declare [reserve <n>|allies <names>|enemies <names>|sieges <rooms>|outposts <capture>|claims <capture>|labs <room> <compound=n,…>]|capture [out]|wait --ticks N");
}
