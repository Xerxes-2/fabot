// The boost chain's soak (ARCHITECTURE.md §10.2): `young-among-colonies` (three RCL8
// colonies, an RCL7 and an RCL6, each with labs and a terminal) on sim, its RCL8
// homes digging O, U and K and the lower ones H, with the season's only X in one
// terminal, so every T3 is reacted from ore another colony dug and balanced to it.
// Then what each cluster produced and the ticks its labs stood idle and why
// (sim's sampler reads both), where each ore went, what the terminals sent and
// what it cost, and soak:many's exposure, lost/held and open-ledger flag.
//
// Usage: npm run soak:labs -- [--seed 1,2] [--ticks 30000] [--tag labs] [--bot <sim binary>]
//   --bot  a sim binary (default this tree's, `npm run build:sim`), to read a change against its base
// Each run goes to soak-runs/<tag>-s<seed>/ with its report.json beside the samples.
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { exposureOf, invariants, samplesOf } from "../xxscreeps/measures.mjs";
import { runSim, simBinary } from "./run.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const WORLD = join(REPO, "scripts/xxscreeps/worlds/young-among-colonies.json");
// Each home's mineral, where the world's is another (all three RCL8 homes dig O there).
const ORES = { W3S8: "U", W2S8: "K" };
const STOCK = { W2S8: { X: 20000 } };
// A T3 boost is a catalysed compound.
const t3 = (r) => r.length > 1 && r.startsWith("X");

const flags = { "--seed": "1,2", "--ticks": "30000", "--tag": "labs" };
const args = process.argv.slice(2);
for (let i = 0; i < args.length; i += 2) {
  if (!(args[i] in flags) && args[i] !== "--bot") throw new Error(`usage: npm run soak:labs -- [--seed 1,2] [--ticks 30000] [--tag labs] [--bot <sim>]`);
  flags[args[i]] = args[i + 1];
}
const bin = flags["--bot"] ? resolve(flags["--bot"]) : simBinary();
if (!bin) throw new Error("sim did not build");

const world = JSON.parse(readFileSync(WORLD, "utf8"));
for (const room of world.rooms) {
  for (const o of room.objects) {
    if (o.type === "mineral" && ORES[room.name]) o.mineralType = ORES[room.name];
    if (o.type === "terminal") Object.assign(o.store, STOCK[room.name]);
  }
}
const stood = join(REPO, "soak-runs", `${flags["--tag"]}-world.json`);
mkdirSync(dirname(stood), { recursive: true });
writeFileSync(stood, JSON.stringify(world));

const sum = (o, keep = () => true) => Object.entries(o ?? {}).reduce((n, [r, v]) => n + (keep(r) ? v : 0), 0);
const minus = (a = {}, b = {}) => Object.fromEntries([...new Set([...Object.keys(a), ...Object.keys(b)])].map((r) => [r, (a[r] ?? 0) - (b[r] ?? 0)]));

function report(dir) {
  const samples = samplesOf(dir);
  const [first, last] = [samples[0], samples.at(-1)];
  const homes = Object.keys(last.homes ?? {});
  const at = (t) => samples.findLast((s) => s.t <= t) ?? first;
  const rooms = {};
  for (const h of homes.filter((h) => sum(last.rooms[h]?.labTicks) > 0)) {
    const x = last.rooms[h];
    const t3At = (t) => sum(at(t).rooms[h]?.produced, t3);
    rooms[h] = {
      rcl: last.homes[h].rcl,
      labs: x.labs.length,
      mineral: world.rooms.find((r) => r.name === h).objects.filter((o) => o.type === "mineral").map((o) => o.mineralType).join(""),
      produced: x.produced,
      consumed: x.consumed,
      boosted: x.boosted,
      t3Per10k: [10000, 20000, 30000].filter((t) => t <= last.t + 1).map((t) => t3At(t) - t3At(t - 10000)),
      t3Stock: Object.fromEntries([0, 5000, 10000, 15000, 20000, 25000, 30000].filter((t) => t <= last.t + 1).map((t) => [t, sum(at(t).rooms[h]?.stockOf, t3)])),
      labTicks: x.labTicks,
      inputs: x.inputs,
    };
  }
  // By ore, from the first sample to the last: what was there and dug is held, sent away, boosted with, or lost (residual).
  const dug = minus(last.oreDug, first.oreDug);
  const away = minus(last.oreAway, first.oreAway);
  const boosted = minus(last.oreBoosted, first.oreBoosted);
  const ore = Object.fromEntries(
    Object.keys({ ...first.oreHeld, ...last.oreHeld, ...dug }).map((o) => {
      const flow = { held0: first.oreHeld[o] ?? 0, dug: dug[o] ?? 0, held: last.oreHeld[o] ?? 0, away: away[o] ?? 0, boosted: boosted[o] ?? 0 };
      return [o, { ...flow, residual: flow.held0 + flow.dug - flow.held - flow.away - flow.boosted }];
    }),
  );
  const stock = {};
  for (const h of homes) for (const [r, n] of Object.entries(last.rooms[h]?.stockOf ?? {})) stock[r] = (stock[r] ?? 0) + n;
  const sent = Object.fromEntries(Object.entries(last.sentOf ?? {}).map(([r, to]) => [r, sum(to)]));
  const { worth: w, score: s, ledger } = last;
  const economy = { ticks: last.t + 1, worth: w.controller + w.structures + w.sites + w.fortification + w.stock, score: s.controller + s.structures + s.fortification + s.stock, ...w, dug: ledger.dug, upgrade: ledger.upgrade, build: ledger.build, spawn: ledger.spawn, ...exposureOf(dir, last) };
  return { rooms, ore, stock, sent, fees: last.feeOf ?? {}, economy, broken: invariants(dir) };
}

const seeds = flags["--seed"].split(",").map(Number);
const ticks = Number(flags["--ticks"]);
const codes = await Promise.all(
  seeds.map((seed) => runSim(bin, { world: stood, room: "W5S8", ticks, seed, out: join(REPO, "soak-runs", `${flags["--tag"]}-s${seed}`) })),
);
const k = (n) => `${(n / 1000).toFixed(1)}k`;
for (const [i, seed] of seeds.entries()) {
  const dir = join(REPO, "soak-runs", `${flags["--tag"]}-s${seed}`);
  if (codes[i] !== 0 || !existsSync(join(dir, "samples.jsonl"))) {
    console.log(`seed ${seed}: sim failed (exit ${codes[i]})`);
    continue;
  }
  const r = report(dir);
  writeFileSync(join(dir, "report.json"), JSON.stringify(r, null, 1));
  console.log(`seed ${seed} (${dir}): worth ${k(r.economy.worth)} score ${k(r.economy.score)} dug ${k(r.economy.dug)} upgrade ${k(r.economy.upgrade)}`);
  const e = r.economy;
  console.log(`  exposure ${e.exposure} lost/held ${k(e.lost)}/${k(e.held)}${e.open ? " LEDGER OPEN: worth not to be read" : ""}`);
  for (const [h, x] of Object.entries(r.rooms)) {
    const made = Object.entries(x.produced ?? {}).sort((a, b) => b[1] - a[1]).map(([c, n]) => `${c} ${n}`).join(", ");
    const ticks = sum(x.labTicks);
    const why = Object.entries(x.labTicks).sort((a, b) => b[1] - a[1]).map(([c, n]) => `${c} ${n} (${Math.round((100 * n) / ticks)}%)`).join(" ");
    console.log(`  ${h} rcl${x.rcl} ${x.labs} labs, digs ${x.mineral}: T3/10k ${x.t3Per10k.join(" ")}; T3 held ${Object.values(x.t3Stock).join(" ")}`);
    console.log(`    produced ${made || "nothing"}; boosted ${JSON.stringify(x.boosted ?? {})}`);
    console.log(`    lab ticks ${why}`);
  }
  for (const [o, f] of Object.entries(r.ore)) console.log(`  ore ${o}: held ${f.held0} + dug ${f.dug} = held ${f.held} + away ${f.away} + boosted ${f.boosted} + residual ${f.residual}`);
  console.log(`  stock ${JSON.stringify(Object.fromEntries(Object.entries(r.stock).filter(([, n]) => n > 0)))}`);
  console.log(`  sent ${JSON.stringify(r.sent)}; fees ${JSON.stringify(r.fees)}`);
  for (const b of r.broken) console.log(`  invariant broken: ${b}`);
}
if (codes.some((c) => c !== 0)) process.exit(1);
