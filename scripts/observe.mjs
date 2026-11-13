// The operator's view of the live bot: the observe segment, the switches, and
// the recorder's ring (ARCHITECTURE.md §4, §9.1, §11). Server and .env config
// are screeps-api.mjs's.
//
// Usage: run with no args.
import { readFileSync } from "node:fs";
import { gunzipSync } from "node:zlib";
import { connect, fail } from "./screeps-api.mjs";
import { METRICS, windows } from "./metrics.mjs";
import { replay, views } from "./observe-views.mjs";

const OBSERVE = 1;
const USAGE = `usage: npm run observe -- <command>
  health             tick, version, bucket, CPU, faults, refusals, modules off, metrics past their threshold
  metrics [room] [--since <tick>]
                     the 100-tick windows of segment 3, a metric a line (\`*\`, the default: the world's row); without --since the last ten
  cpu                CPU by phase and module, this tick
  faults             the fault ring, newest first
  switches           Memory.v2.switches as stored
  switch <name> on|off|clear
                     off: \`false\` (the module is skipped); on: \`true\` (the recorder's opt-in); clear: remove the key
  declare [reserve <n> | allies <name,…> | enemies <name,…> | sieges <room,…> | avoid <room,…> | abandon <room,…> | outposts <capture.json> | claims <capture.json> | tactics <rules.json> | labs <room> <compound=n,…>]
                     Memory.v2.declarations as stored; with a name, set it
                     (outposts: the capture's, \`scripts/capture-world.mjs\`; reserve: creep lifetimes of tower fire the reserve holds)
  capture [out.jsonl]
                     the recorder's ring as \`npm run bench\`'s capture, oldest first; terrain cached in captures/terrain
  tap --out <file> [--from <tick> --to <tick>]
                     the ring recorded inside a window — an open engagement's, or from..to —
                     every tick the hour's 360 segment reads pay for, the rest gaps
  battle <room> <from> <to>
                     the room's history: every body's blood, each tower's shots, the razes, his garrison
  wait --ticks N     return after N game ticks
  engagements        each open engagement: members, plan order, offers, target, prediction; pending casts
  room <room>        controller, structures, creeps, tombstones, sites, as the server holds them
  creep <name>       one of our creeps and its assignment
  regions <room>     walkable regions past rival ramparts: exits per side, owned structures beside
  replay <capture.jsonl> [--trace <creep>]
                     dist/main.js run on the captured ticks, offline: intents, refusals, tile scores`;
const LIVE = ["metrics", "health", "cpu", "faults", "switches", "switch", "declare", "capture", "tap", "battle", "wait", "engagements", "room", "creep", "regions"];

const [cmd, ...args] = process.argv.slice(2);
if (cmd === "replay") {
  const traced = args.includes("--trace") ? args[args.indexOf("--trace") + 1] : undefined;
  if (!args[0] || (args.includes("--trace") && !traced)) fail("usage: observe replay <capture.jsonl> [--trace <creep>]");
  replay(args[0], traced);
  process.exit(0);
}
if (!LIVE.includes(cmd)) fail(USAGE);
const { api, shard, url, token } = await connect();

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
const view = views({ api, shard, url, token, memory, raw });

switch (cmd) {
  case "health": {
    const o = await observed();
    const now = (await api.req("GET", "/api/game/time", { shard })).time;
    console.log(`tick ${o.tick} (server ${now}, ${now - o.tick} behind) · ${o.version} · bucket ${o.bucket} · cpu ${o.cpu.toFixed(2)}`);
    console.log(`off: ${o.off.length ? o.off.join(", ") : "none"}`);
    console.log(`faults: ${o.faults.length} in ring${o.faults[0] ? `, newest t${o.faults[0].tick} ${o.faults[0].where}: ${o.faults[0].message}` : ""}`);
    console.log(`refusals this tick: ${o.refusals.length}`);
    for (const alert of o.alerts ?? []) console.log(`metric ${alert}`);
    for (const [key] of o.counts.filter(([k]) => k.startsWith("refused:"))) console.log(`refused ${key.slice("refused:".length)}`);
    for (const [key, left] of o.counts.filter(([k]) => k.startsWith("abandon:"))) {
      const room = key.slice("abandon:".length);
      console.log(`abandon ${room}: ${left} held${left > 200 ? "" : " — shipped; the bot razes and unclaims it"}`);
    }
    break;
  }
  case "metrics": {
    const since = args.includes("--since") ? Number(args[args.indexOf("--since") + 1]) : undefined;
    const row = args.find((a, i) => !a.startsWith("--") && args[i - 1] !== "--since") ?? "*";
    if (since !== undefined && !Number.isInteger(since)) fail("usage: observe metrics [room] [--since <tick>]");
    const all = windows(await raw(METRICS)).filter((w) => w.row === row);
    const shown = since === undefined ? all.slice(-10) : all.filter((w) => w.start >= since);
    if (!shown.length) fail(`no window of ${row} in segment ${METRICS}`);
    const names = [...new Set(shown.flatMap((w) => Object.keys(w.values)))].sort();
    const width = Math.max(...names.map((n) => n.length));
    console.log(`${"start".padEnd(width)} ${shown.map((w) => String(w.start).padStart(8)).join("")}`);
    for (const name of names) console.log(`${name.padEnd(width)} ${shown.map((w) => String(w.values[name] ?? 0).padStart(8)).join("")}`);
    const partial = shown.filter((w) => w.ticks < 100);
    if (partial.length) console.log(`partial (a reset): ${partial.map((w) => `t${w.start} ${w.ticks} ticks`).join(", ")}`);
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
    else if (["allies", "enemies", "sieges", "avoid", "abandon"].includes(name) && value !== undefined) declarations[name] = value.split(",").filter(Boolean);
    else if ((name === "outposts" || name === "claims") && value !== undefined) declarations[name] = JSON.parse(readFileSync(value, "utf8")).declarations[name] ?? [];
    else if (name === "tactics" && value !== undefined) {
      const file = JSON.parse(readFileSync(value, "utf8"));
      const rules = file.declarations?.tactics ?? file.tactics ?? file;
      if (!Array.isArray(rules)) fail("declare tactics: the file holds a list of { \"rival\"?, \"room\"?, \"pin\"?, \"forbid\"? }");
      declarations.tactics = rules;
    } else if (name === "labs" && value !== undefined && more !== undefined)
      declarations.labs = { home: value, targets: Object.fromEntries(more.split(",").map((p) => p.split("=")).map(([c, n]) => [c, Number(n)])) };
    else fail("usage: observe declare [reserve <n ≥ 0> | allies <name,…> | enemies <name,…> | sieges <room,…> | avoid <room,…> | abandon <room,…> | outposts <capture.json> | claims <capture.json> | tactics <rules.json> | labs <room> <compound=n,…>]");
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
  case "capture":
    await view.capture(args[0]);
    break;
  case "tap": {
    const flag = (name) => (args.includes(name) ? Number(args[args.indexOf(name) + 1]) : undefined);
    const out = args.includes("--out") ? args[args.indexOf("--out") + 1] : undefined;
    const from = flag("--from");
    const to = flag("--to");
    const window = from !== undefined && Number.isInteger(from) && Number.isInteger(to) && from < to ? { from, to } : null;
    if (!out || ((from !== undefined || to !== undefined) && window === null)) fail("usage: observe tap --out <file> [--from <tick> --to <tick>]");
    await view.tap(out, window);
    break;
  }
  case "battle": {
    const [room, from, to] = args;
    if (!room || !Number.isInteger(Number(from)) || !Number.isInteger(Number(to))) fail("usage: observe battle <room> <from> <to>");
    await view.battle(room, Number(from), Number(to));
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
  case "engagements":
    await view.engagements();
    break;
  case "room":
  case "creep":
  case "regions":
    if (!args[0]) fail(`usage: observe ${cmd} <${cmd === "creep" ? "name" : "room"}>`);
    await view[cmd](args[0]);
    break;
}
