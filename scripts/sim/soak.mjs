// One soak on sim (ARCHITECTURE.md §10.2), into soak-runs/<run>/; with --watch
// served to lazycreep as it runs (`sim --watch`) until Ctrl-C.
// Usage: npm run soak -- [--room <name>] [--world <world.json> | --gen <seed>] [--node N] [--seed N] [--late K] [--ticks N]
//          [--ended] [--unsafe] [--invaders room:every[:first],…] [--wave role[:size],…] [--core room:tick,…]
//          [--capture a-b[,c-d]] [--run name] [--watch [--port N] [--tick-ms N] [--badge <json>]]
//        npm run watch:sim -- …   the same, --watch implied
//   --room  default the first room by name with a controller nobody owns and two sources
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { invariants, samplesOf } from "../scenario/measures.mjs";
import { TEMPLATE, buildPlan, runSim, seatsUs, simBinary } from "./run.mjs";

const usage = "usage: npm run soak -- [--room <name>] [--world <world.json> | --gen <seed>] [--node N] [--seed N] [--late K] [--ticks N] [--ended] [--unsafe] [--invaders …] [--wave …] [--core …] [--capture a-b] [--run name] [--watch [--port N] [--tick-ms N] [--badge <json>]]";
const SWITCHES = ["--ended", "--unsafe", "--watch"];
const VALUES = ["--room", "--world", "--gen", "--node", "--seed", "--late", "--ticks", "--invaders", "--wave", "--core", "--capture", "--run", "--port", "--tick-ms", "--badge"];
const fail = (m) => {
  console.error(`${m}\n${usage}`);
  process.exit(2);
};
const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (SWITCHES.includes(a)) flags[a] = true;
  else if (VALUES.includes(a) && process.argv[i + 1] !== undefined && !process.argv[i + 1].startsWith("--")) flags[a] = process.argv[++i];
  else fail(`${a}: not a flag soak takes, or it lacks its value`);
}
const WHOLE = ["--gen", "--seed", "--late", "--ticks", "--port", "--tick-ms"];
for (const f of WHOLE.filter((f) => flags[f] !== undefined)) if (!/^\d+$/.test(flags[f])) fail(`${f} takes a whole number`);
if (!flags["--watch"] && ["--port", "--tick-ms", "--badge"].some((f) => flags[f])) fail("--port, --tick-ms and --badge need --watch");
const { "--world": world, "--node": node, "--gen": gen } = flags;
const number = (flag, or) => Number(flags[flag] ?? or);
const rooms = gen === undefined ? JSON.parse(readFileSync(world ?? TEMPLATE, "utf8")) : null;
// A world that seats us plays from its own home, as runSim stands it.
const home = rooms && seatsUs(rooms) && rooms.rooms.find((r) => r.objects.some((o) => o.type === "spawn" && o.user === rooms.userId)).name;
if (gen !== undefined && (world || flags["--room"])) fail("--gen picks its own map and room: no --world or --room");
if (home && (flags["--room"] || node)) fail(`--world seats us in ${home}: no --room or --node`);
const room =
  home ||
  flags["--room"] ||
  rooms?.rooms
    .toSorted((a, b) => (a.name < b.name ? -1 : 1))
    .find((r) => r.objects.some((o) => o.type === "controller" && !o.user) && r.objects.filter((o) => o.type === "source").length === 2)?.name;
if (gen === undefined && !room) fail("no room in the world has a free controller and two sources: name one with --room");
if (node && !buildPlan()) process.exit(1);
const bin = simBinary();
if (!bin) process.exit(1);
const watch = flags["--watch"] && { port: number("--port", 21025), ms: number("--tick-ms", 50), badge: flags["--badge"] };
const out = join(process.cwd(), "soak-runs", flags["--run"] ?? `${room ?? `gen${gen}`}${node ? `-${node}` : ""}-seed${number("--seed", 1)}`);
console.error(`sim: output ${out}`);
const code = await runSim(bin, {
  world, gen, room, node, seed: number("--seed", 1), late: number("--late", 0), ticks: number("--ticks", watch ? 1e6 : node ? 5000 : 3000),
  ended: Boolean(flags["--ended"]), unsafe: Boolean(flags["--unsafe"]), invaders: flags["--invaders"], wave: flags["--wave"], core: flags["--core"],
  capture: flags["--capture"], watch, out,
});
if (code === 0) {
  const last = samplesOf(out).at(-1);
  console.log(`t${last.t} rcl${last.rcl} progress ${last.progress} creeps ${last.creeps}`);
  for (const broken of invariants(out)) console.log(`invariant broken: ${broken}`);
}
process.exit(code);
