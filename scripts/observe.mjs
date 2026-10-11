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
//   observe.mjs declare [reserve <n>]
//                                  Memory.v2.declarations as stored; with a name, set it
//                                  (reserve: creep lifetimes of tower fire the reserve holds)
//   observe.mjs capture [out.jsonl]
//                                  the recorder's ring (segments 10..14), oldest first
//   observe.mjs wait --ticks N     return after N game ticks
import { writeFileSync } from "node:fs";
import { gunzipSync } from "node:zlib";
import { connect, fail } from "./screeps-api.mjs";

const OBSERVE = 1;
const RECORDER = [10, 11, 12, 13, 14];

const [cmd, ...args] = process.argv.slice(2);
const { api, shard } = await connect();

const segment = async (id) => {
  const res = await api.req("GET", "/api/user/memory-segment", { segment: id, shard });
  return res.data ? JSON.parse(res.data) : null;
};
const memory = async (path) => {
  const res = await api.req("GET", "/api/user/memory", { path, shard });
  if (typeof res.data !== "string") return res.data ?? null;
  return res.data.startsWith("gz:") ? JSON.parse(gunzipSync(Buffer.from(res.data.slice(3), "base64")).toString()) : JSON.parse(res.data);
};
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
    const [name, value] = args;
    const declarations = (await memory("v2.declarations")) ?? {};
    if (name === undefined) {
      console.log(JSON.stringify(declarations, null, 2));
      break;
    }
    const n = Number(value);
    if (name !== "reserve" || value === undefined || !(n >= 0)) fail("usage: observe declare [reserve <n ≥ 0>]");
    declarations[name] = n;
    await api.req("POST", "/api/user/memory", { path: "v2.declarations", value: declarations, shard });
    console.log(`v2.declarations = ${JSON.stringify(declarations)} (read next tick)`);
    break;
  }
  case "switch": {
    const [name, state] = args;
    if (!name || !["on", "off", "clear"].includes(state)) fail("usage: observe switch <name> on|off|clear");
    const switches = (await memory("v2.switches")) ?? {};
    if (state === "clear") delete switches[name];
    else switches[name] = state === "on";
    await api.req("POST", "/api/user/memory", { path: "v2.switches", value: switches, shard });
    console.log(`v2.switches = ${JSON.stringify(switches)} (read next tick)`);
    break;
  }
  case "capture": {
    const rows = (await Promise.all(RECORDER.map(segment))).filter(Boolean).sort((a, b) => a.tick - b.tick);
    if (rows.length === 0) fail("recorder ring empty: observe switch recorder on, then wait a few ticks");
    const text = rows.map((r) => JSON.stringify(r)).join("\n") + "\n";
    if (args[0]) writeFileSync(args[0], text);
    else process.stdout.write(text);
    console.error(`captured ticks ${rows.map((r) => r.tick).join(", ")}`);
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
    fail("usage: observe health|cpu|faults|switches|switch <name> on|off|clear|declare [reserve <n>]|capture [out]|wait --ticks N");
}
