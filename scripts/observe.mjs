// The operator's view of the live bot: the observe segment, the switches, and
// the recorder's ring (ARCHITECTURE.md §4, §9.1, §11). Server and .env config
// are screeps-api.mjs's.
//
// Usage: run with no args.
import { createReadStream, existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { createInterface } from "node:readline";
import { gunzipSync } from "node:zlib";
import { connect, fail } from "./screeps-api.mjs";
import { METRICS, windows } from "./metrics.mjs";
import { OBSERVE, replay, views } from "./observe-views.mjs";
import { engagement, outcome, report, tested } from "./sim/prediction.mjs";

const ENGAGEMENTS = 4;
const USAGE = `usage: npm run observe -- <command>
  health             tick, version, bucket, CPU, faults, refusals, modules off, metrics past their threshold
  metrics [room] [--since <tick>]
                     the 100-tick windows of segment 3, a metric a line (\`*\`, the default: the world's row); without --since the last ten
  cpu                CPU by phase and module, this tick
  faults             the fault ring, newest first
  switches           Memory.v2.switches as stored
  switch <name> on|off|clear
                     off: \`false\` (the module is skipped); on: \`true\` (the recorder's opt-in); clear: remove the key
  declare [allies <name,…> | enemies <name,…> | avoid <room,…> | abandon <room,…> | outposts <capture.json> | claims <capture.json> | tactics <rules.json>]
                     Memory.v2.declarations as stored; with a name, set it
                     (outposts: the capture's, \`scripts/capture-world.mjs\`)
  tuning [<key> <n> | <key> clear]
                     Memory.v2.tuning as stored (\`core/base/tuning/mod.rs\`: a key left out is its default, one unknown a fault); with a key, set or clear it
  capture [out.jsonl]
                     the recorder's ring as \`npm run bench\`'s capture, oldest first; terrain cached in captures/terrain
  tap --out <file> [--from <tick> --to <tick>]
                     the ring recorded inside a window — an open engagement's, or from..to —
                     every tick the hour's 360 segment reads pay for, the rest gaps
  battle <room> <from> <to>
                     the room's history: every body's blood, each tower's shots, his garrison
  wait --ticks N     return after N game ticks
  exchange [segment4.jsonl]
                     the closed engagements of segment 4 (or a run's copy, sim's \`segment4.jsonl\`): \`npm run exchange\`'s table, then each rival's outcomes by tactic
  grudges [segment4.jsonl]
                     per rival, off segment 4 (or sim's \`segment4.jsonl\`): fights by kind, our bodies dead, energy cast, won, first contact, last close
  engagements        each open engagement: members, plan order, offers, target, prediction; pending casts
  room <room>        controller, structures, creeps, tombstones, sites, as the server holds them
  creep <name>       one of our creeps and its assignment
  regions <room>     walkable regions past rival ramparts: exits per side, owned structures beside
  claims <run dir>   a soak's \`claim\` decisions off its cpu.jsonl (a later seat's: its subdirectory), on each due tick
                     the top candidate changed or a claim or give-up was decided: each room's score term by term
  replay <capture.jsonl> [--trace <creep>]
                     dist/main.js run on the captured ticks, offline: intents, refusals, tile scores`;
const LIVE = ["metrics", "health", "cpu", "faults", "switches", "switch", "declare", "tuning", "capture", "tap", "battle", "wait", "engagements", "exchange", "grudges", "room", "creep", "regions"];

// The rows of `engagements.log` as `report` reads them, then the outcomes per rival (`npc`: none named) and tactic.
function exchange(text) {
  const rows = tested((text ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l)));
  for (const line of [`${rows.length} engagements`, ...report(rows)]) console.log(line);
  const by = Map.groupBy(rows, (r) => `${r.rivals.join(",") || "npc"} ${r.tactic}`);
  for (const [key, of] of [...by].sort()) {
    const counts = Object.entries(Object.groupBy(of, (r) => `${r.p.kind}→${outcome(r.a)}`)).sort();
    console.log(`${key}: ${counts.map(([k, n]) => `${k} ${n.length}`).join(", ")}`);
  }
}

// Each rival's ledger, for the owner alone: each rival a row names is charged the whole row, so totals overlap;
// a row naming none (an Invader's, or closed before contact) is `npc`'s.
function grudges(text) {
  const by = new Map();
  for (const r of (text ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l))) {
    for (const rival of r.rivals.length ? r.rivals : ["npc"]) {
      const g = by.get(rival) ?? { fights: {}, dead: 0, energy: 0, won: 0, first: Infinity, last: 0 };
      by.set(rival, g);
      g.fights[r.kind] = (g.fights[r.kind] ?? 0) + 1;
      Object.assign(g, { dead: g.dead + r.dead, energy: g.energy + r.energy, last: Math.max(g.last, r.closed) });
      g.won += outcome(engagement(r)) === "won" ? 1 : 0;
      g.first = Math.min(g.first, r.contact ?? r.opened);
    }
  }
  const table = [["rival", "fights", "our dead", "cast", "won", "first", "last"]].concat(
    [...by].sort(([, a], [, b]) => b.energy - a.energy).map(([rival, g]) =>
      [rival, Object.entries(g.fights).map(([k, n]) => `${k} ${n}`).join(","), g.dead, g.energy, g.won, `t${g.first}`, `t${g.last}`]),
  );
  const widths = table[0].map((_, i) => Math.max(...table.map((row) => String(row[i]).length)));
  for (const row of table) console.log(row.map((v, i) => String(v)[i < 2 ? "padEnd" : "padStart"](widths[i])).join("  "));
}

// A claim is decided on a tick with a `best` and no colony `held` against it, a give-up on one with a `swap`.
async function claims(dir) {
  const COLUMNS = ["room", "why", "ore", "total", "source", "near", "crossing", "worth", "failure", "mineral", "now", "option", "rarity", "margin"];
  let top;
  if (!existsSync(join(dir, "cpu.jsonl"))) fail(`no cpu.jsonl in ${dir}`);
  for await (const line of createInterface({ input: createReadStream(join(dir, "cpu.jsonl")) })) {
    if (!line.includes('"claim"')) continue;
    const { t, decisions } = JSON.parse(line);
    const rows = decisions.filter(([kind]) => kind === "claim").map(([, fields]) => Object.fromEntries(fields));
    const why = (w) => rows.some((r) => r.why === w);
    const decided = why("swap") || (why("best") && !why("held"));
    if (rows[0]?.room === top && !decided) continue;
    top = rows[0]?.room;
    const table = [COLUMNS, ...rows.map((r) => COLUMNS.map((c) => r[c] ?? ""))];
    const widths = COLUMNS.map((_, i) => Math.max(...table.map((row) => String(row[i]).length)));
    console.log(`t${t}${decided ? " decided" : ""}`);
    for (const row of table) console.log(row.map((v, i) => String(v)[i < 3 ? "padEnd" : "padStart"](widths[i])).join("  "));
  }
}

const [cmd, ...args] = process.argv.slice(2);
if (cmd === "claims") {
  if (!args[0]) fail("usage: observe claims <run dir>");
  await claims(args[0]);
  process.exit(0);
}
if (cmd === "replay") {
  const traced = args.includes("--trace") ? args[args.indexOf("--trace") + 1] : undefined;
  if (!args[0] || (args.includes("--trace") && !traced)) fail("usage: observe replay <capture.jsonl> [--trace <creep>]");
  await replay(args[0], traced);
  process.exit(0);
}
if (cmd === "exchange" && args[0]) {
  exchange(readFileSync(args[0], "utf8"));
  process.exit(0);
}
if (cmd === "grudges" && args[0]) {
  grudges(readFileSync(args[0], "utf8"));
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
    const walked = Object.fromEntries(o.counts);
    const units = { "read.rooms": "rooms", "read.events": "logs", "read.pack": "ints" };
    const each = (name, spent) =>
      walked[name] > 0 ? `  ${walked[name]} ${units[name] ?? "objects"}, ${((1000 * spent) / walked[name]).toFixed(2)} us each` : "";
    const parts = (name) => (walked[`${name}.parts`] > 0 ? `, ${walked[`${name}.parts`]} body parts` : "");
    for (const [name, spent] of o.phases) console.log(`${ms(spent)}  ${name}${each(name, spent)}${parts(name)}`);
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
    const stored = await memory("v2.declarations");
    const declarations = stored ?? {};
    if (name === undefined) {
      console.log(JSON.stringify(declarations, null, 2));
      break;
    }
    if (["allies", "enemies", "avoid", "abandon"].includes(name) && value !== undefined) declarations[name] = value.split(",").filter(Boolean);
    else if ((name === "outposts" || name === "claims") && value !== undefined) declarations[name] = JSON.parse(readFileSync(value, "utf8")).declarations[name] ?? [];
    else if (name === "tactics" && value !== undefined) {
      const file = JSON.parse(readFileSync(value, "utf8"));
      const rules = file.declarations?.tactics ?? file.tactics ?? file;
      if (!Array.isArray(rules)) fail("declare tactics: the file holds a list of { \"rival\"?, \"room\"?, \"pin\"?, \"forbid\"? }");
      declarations.tactics = rules;
    } else fail("usage: observe declare [allies <name,…> | enemies <name,…> | avoid <room,…> | abandon <room,…> | outposts <capture.json> | claims <capture.json> | tactics <rules.json>]");
    // A memory write is not read back at once: a whole-object write would
    // revert a key declared just before it.
    await (stored ? putV2(`declarations.${name}`, declarations[name]) : putV2("declarations", declarations));
    console.log(`v2.declarations = ${JSON.stringify(declarations)} (read next tick)`);
    break;
  }
  case "tuning": {
    const [key, value] = args;
    const tuning = (await memory("v2.tuning")) ?? {};
    if (key !== undefined) {
      if (value === "clear") delete tuning[key];
      else if (value !== undefined && Number.isFinite(Number(value))) tuning[key] = Number(value);
      else fail("usage: observe tuning [<key> <n> | <key> clear]");
      await putV2("tuning", tuning);
    }
    console.log(`v2.tuning = ${JSON.stringify(tuning)}${key === undefined ? "" : " (read next tick)"}`);
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
  case "exchange":
    exchange(await raw(ENGAGEMENTS));
    break;
  case "grudges":
    grudges(await raw(ENGAGEMENTS));
    break;
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
