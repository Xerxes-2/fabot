// One soak on sim served to lazycreep as it runs (`sim --watch`), as `soak.mjs --watch` serves one on xxscreeps.
// Usage: npm run watch:sim -- (--room <name> [--world <world.json>] | --gen <seed>) [--node N] [--seed N] [--ended] [--ticks N] [--port N] [--tick-ms N] [--badge <json>]
import { mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { buildPlan, runSim, simBinary } from "./run.mjs";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  flags[a] = a === "--ended" || process.argv[i + 1]?.startsWith("--") ? true : process.argv[++i];
}
const { "--room": room, "--world": world, "--node": node, "--gen": gen } = flags;
const number = (flag, or) => Number(flags[flag] ?? or);
if (!room && gen === undefined) {
  console.error("usage: npm run watch:sim -- (--room <name> [--world <world.json>] | --gen <seed>) [--node N] [--seed N] [--ended] [--ticks N] [--port N] [--tick-ms N] [--badge <json>]");
  process.exit(2);
}
if (node && !buildPlan()) process.exit(1);
const bin = simBinary();
if (!bin) process.exit(1);
const out = join(process.cwd(), ".scratch/sim-watch/run");
mkdirSync(dirname(out), { recursive: true });
console.error(`sim --watch: output ${out}`);
const code = await runSim(bin, { world, gen, room, node, ticks: number("--ticks", 1e6), seed: number("--seed", 1), ended: Boolean(flags["--ended"]),
  watch: { port: number("--port", 21025), ms: number("--tick-ms", 50), badge: flags["--badge"] }, out });
process.exit(code);
