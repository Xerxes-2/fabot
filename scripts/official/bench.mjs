// CPU of two bundles on the official server, from one saved world, in
// interleaved rounds (ABBA): calibration beside `npm run bench`, which is
// fingerprint-exact where this is not (the server's ticks are its own).
// --save-at T plays A on a fresh W8N3 world for T ticks and saves it to --world.
//
// Usage: npm run bench:real -- --a A.js --b B.js --world db.json [--rounds R] [--ticks N] [--cold K]
//        npm run bench:real -- --a A.js --save-at T --world db.json
import { resolve } from "node:path";
import { boot, fail, pct, scratch } from "./server.mjs";

const usage = "usage: npm run bench:real -- --a A.js (--b B.js | --save-at T) --world db.json [--rounds R] [--ticks N] [--cold K]";
const flags = {};
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i += 2) {
  if (!["--a", "--b", "--world", "--rounds", "--ticks", "--cold", "--save-at"].includes(argv[i]) || argv[i + 1] === undefined) fail(usage);
  flags[argv[i].slice(2)] = argv[i + 1];
}
if (!flags.a || !flags.world || !flags.b === !flags["save-at"]) fail(usage);
const world = resolve(flags.world);

if (flags["save-at"]) {
  const server = await boot(scratch("bench-save"));
  await server.upload(resolve(flags.a));
  await server.play(Number(flags["save-at"]));
  const controller = await server.run("storage.db['rooms.objects'].findOne({type:'controller',room:'W8N3'})");
  await server.save(world);
  server.stop();
  console.log(`saved ${world} at RCL ${controller.match(/level: (\d+)/)?.[1]}`);
  process.exit(0);
}

const [rounds, ticks, cold] = [flags.rounds ?? 4, flags.ticks ?? 300, flags.cold ?? 50].map(Number);
const bundles = { A: resolve(flags.a), B: resolve(flags.b) };
const runs = { A: [], B: [] };
for (let r = 0; r < rounds; r++)
  for (const side of r % 2 ? ["B", "A"] : ["A", "B"]) {
    const server = await boot(scratch("bench"), { world });
    await server.upload(bundles[side]);
    const probes = (await server.play(cold + ticks)).map((row) => row.probe).filter(Boolean);
    server.stop();
    const bad = probes.find((p) => p.err);
    if (bad) fail(`${side} threw at tick ${bad.t}: ${bad.err}`);
    // A tick's billed intents arrive in the next tick's probe.
    const kept = probes.slice(cold);
    const billed = kept.slice(0, -1).flatMap((p, i) => (kept[i + 1].intents == null ? [] : [[p.cpu, 0.2 * kept[i + 1].intents]]));
    const stats = (xs) => ({ p50: pct(xs, 0.5), p90: pct(xs, 0.9), p99: pct(xs, 0.99), mean: xs.reduce((a, b) => a + b, 0) / xs.length });
    const [cpu, intents] = [billed.map(([c]) => c), billed.map(([, i]) => i)];
    runs[side].push({ ...stats(cpu), compute: stats(cpu.map((c, i) => c - intents[i])), intents: stats(intents) });
    console.error(`round ${r + 1}/${rounds} ${side}: ${JSON.stringify(runs[side].at(-1))}`);
  }
const median = (xs) => [...xs].sort((a, b) => a - b)[Math.floor(xs.length / 2)];
const summary = (side, of = (run) => run) => Object.fromEntries(["p50", "p90", "p99"].map((k) => [k, median(runs[side].map((run) => of(run)[k]))]));
// The median round, not the min: the server's ticks differ run to run (its
// invaders, its timing), so no round is the one free of noise that a min assumes.
const ratios = runs.A.map((a, i) => runs.B[i].mean / a.mean);
// Compute (CPU less 0.2 per billed intent) is the code-speed number; intents follow behaviour.
const computeRatios = runs.A.map((a, i) => runs.B[i].compute.mean / a.compute.mean);
const sides = Object.fromEntries(["A", "B"].map((s) => [s, { ...summary(s), compute: summary(s, (r) => r.compute), intents: summary(s, (r) => r.intents) }]));
console.log(JSON.stringify({ ...sides, ratio: +median(ratios).toFixed(3), ratios: ratios.map((x) => +x.toFixed(3)), computeRatio: +median(computeRatios).toFixed(3), computeRatios: computeRatios.map((x) => +x.toFixed(3)) }));
