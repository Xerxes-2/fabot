// The built bundle on the official server (Node 24, isolated-vm) for a fresh
// W8N3 colony: fails on anything our code throws or logs, a fault in the
// observe segment, a global reset (a re-upload halfway) that does not recover,
// or a heap past its limit. `npm run smoke` is the cheap stub-game check.
//
// Usage: npm run smoke:real -- [--ticks N] [bundle]   (after `npm run build`)
import { resolve } from "node:path";
import { OBSERVE } from "../observe-views.mjs";
import { boot, fail, pct, scratch } from "./server.mjs";

const argv = process.argv.slice(2);
const at = argv.indexOf("--ticks");
const ticks = at < 0 ? 300 : Number(argv.splice(at, 2)[1]);
const bundle = resolve(argv[0] ?? "dist/main.js");
const t0 = Date.now();
const server = await boot(scratch("smoke"));
await server.upload(bundle);
let reset;
const rows = await server.play(ticks, async (p) => {
  if (!reset && p.t >= ticks / 2) (reset = p.t), await server.upload(bundle);
});
const probes = rows.map((r) => r.probe).filter(Boolean);
const loads = probes.filter((p) => p.load !== undefined);
// The stock Invader AI has no `main` on the private server, every tick.
const INVADER = "Invader error Error: Unknown module 'main'";
const engine = ["engine_runner1.log", "engine_processor1.log", "engine_processor2.log"]
  .flatMap((f) => server.log(f).split("\n").filter((l) => /error/i.test(l) && l !== INVADER).map((l) => `${f}: ${l}`));
const errors = [
  ...probes.filter((p) => p.err).map((p) => `tick ${p.t} threw: ${p.err}`),
  ...rows.filter((r) => r.error || r.log).map((r) => `console: ${r.error ?? r.log}`),
  ...engine,
  ...probes.filter((p) => p.heap > p.limit).map((p) => `tick ${p.t} heap ${p.heap} past ${p.limit}`),
];
if (!loads.some((p) => p.t > reset)) errors.push(`no global reset after the re-upload at tick ${reset}`);
const observe = JSON.parse((await server.segment(OBSERVE).catch(() => null)) ?? "null");
if (observe?.faults?.length) errors.push(`observe faults: ${JSON.stringify(observe.faults)}`);
else if (!observe) errors.push("no observe segment");
server.stop();
const cpu = probes.filter((p) => p.load === undefined).map((p) => p.cpu);
const mib = (b) => +(b / 1048576).toFixed(1);
console.log(
  JSON.stringify({
    ticks: probes.length,
    seconds: Math.round((Date.now() - t0) / 1000),
    cpu: { p50: pct(cpu, 0.5), p90: pct(cpu, 0.9), p99: pct(cpu, 0.99), max: pct(cpu, 1) },
    resets: loads.map((p) => ({ t: p.t, load: +p.load.toFixed(2), tick: +p.cpu.toFixed(2) })),
    heapMiB: { max: mib(Math.max(...probes.map((p) => p.heap))), limit: mib(probes[0]?.limit ?? 0) },
  }),
);
if (errors.length) fail(`smoke:real: ${errors.length} error(s)\n${[...new Set(errors)].slice(0, 20).join("\n")}`);
