// Weaknesses, not winners: one two-seat self-play soak per seed on the world
// map.js generates off it (`sim --gen --players`), alpha and beta each the
// other's enemy, each seat read for the smells below.
//
// Usage: npm run soak:selfplay -- --seeds A..B|A,B,... [--ticks N] [--jobs J]
//        npm run soak:selfplay -- --read <out>   a finished two-seat run's smells, its files kept
//   --ticks  per seed (default 30000)
//   --jobs   seeds at once (default every allowed core)
// Prints a markdown row per smell (seed, seat, first tick, evidence) and each
// smelly seed's watch command; a seed's fight.jsonl is deleted once read.
import { createReadStream, existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { createInterface } from "node:readline";
import { join, resolve } from "node:path";
import { outpostsIn, samplesOf } from "../scenario/measures.mjs";
import { jsonLines, pool, runSim, seedFlags, simBinary, soakDir } from "./run.mjs";

const PLAYERS = JSON.stringify([
  { name: "alpha", declarations: { enemies: ["beta"] } },
  { name: "beta", declarations: { enemies: ["alpha"] } },
]);

const CREEP_LIFE = 1500;
const SPAWN_TIME_PER_PART = 3;
// `AGED` in `core/observe/metrics.rs`: fewer ticks to live, a body died of age.
const AGED = 5;
const COST = { W: 100, C: 50, M: 50, A: 80, R: 150, H: 250, K: 600, T: 10 };
// A cast unspawned this long past its own spawn time missed any landing a fight could plan on.
const CAST_LATE = 300;
// Forming answered this long is a third of a body's life spent before a shot.
const FORMING_MOST = 500;
// Open two-thirds of a life with nothing to fight (a defend's raid out of the world view, a sweep's room empty of rival bodies): its bodies age guarding nothing.
const TARGETLESS_MOST = 1000;
// A set whose spawn time over one life passes this share of the colony's leaves the economy too little to replace itself.
const SPAWN_SHARE = 0.5;
// The economy at its peak at least this many bodies, then at most `COLLAPSED_TO` this long, did not recover by itself.
const PEAK = 10;
const COLLAPSED_TO = 3;
const COLLAPSED_FOR = 2000;
// One unit in seven dug left on the floor: the outpost's carry does not keep up with its dig.
const DECAY_SHARE = 0.15;
// Below this dug, a share is noise.
const DUG_LEAST = 5000;
// Hostile this many ticks, a room's decay is mostly fight corpses and a rival's spill: ranked apart, not as a carry gap.
const CONTESTED = 1000;
// Dropped this often, an outpost's roads, containers and reserver are paid for again each time.
const FLAP_DROPS = 3;
// A weak heuristic: a raid all dead this soon after a safe mode began likely would have died without it. Samples are
// 100 ticks apart, so the activation is known only to its window.
const RAID_DIES = 100;
// Spawning blocked on the bank most of each window, the bank this flat this long, nothing refilled it.
const BANK_BLOCKED = 90;
const BANK_FLAT = 50;
const BANK_STUCK = 1000;

async function* stream(path) {
  if (existsSync(path)) for await (const line of createInterface({ input: createReadStream(path) })) if (line) yield JSON.parse(line);
}
const costOf = (letters) => [...letters.toUpperCase()].reduce((n, p) => n + (COST[p] ?? 0), 0);
const fights = (letters) => /[arh]/i.test(letters);

/** Each engagement's worst ticks off `fight.jsonl`: set, forming streak, targetless streak, late casts. */
async function engagementsOf(dir, costs) {
  const all = new Map();
  const casts = new Map();
  for await (const { t, fight, objects } of stream(join(dir, "fight.jsonl"))) {
    if (!fight) continue;
    for (const c of fight.casts ?? []) {
      const k = casts.get(c.name) ?? { first: t, engagement: c.engagement, body: c.body };
      casts.set(c.name, { ...k, last: t });
    }
    for (const e of fight.engagements ?? []) {
      const r = all.get(e.id) ?? { id: e.id, kind: e.kind ?? "defend", room: e.room, opened: e.opened, cost: 0, parts: 0, formed: 0, forming: 0, formingAt: 0, idle: 0, targetless: 0, targetlessAt: 0 };
      all.set(e.id, r);
      const asked = (fight.casts ?? []).filter((c) => c.engagement === e.id);
      const held = Object.keys(e.ours ?? {});
      const cost = held.reduce((n, b) => n + (costs.get(b)?.cost ?? 0), 0) + asked.reduce((n, c) => n + costOf(c.body), 0);
      if (cost > r.cost) Object.assign(r, { cost, parts: held.reduce((n, b) => n + (costs.get(b)?.body.length ?? 0), 0) + asked.reduce((n, c) => n + c.body.length, 0), costAt: t });
      r.formed = e.phase === "forming" && !e.unanswered ? r.formed + 1 : 0;
      if (r.formed > r.forming) Object.assign(r, { forming: r.formed, formingAt: t - r.formed + 1 });
      // Both read the world view (`engaged.rs` writes every room), not the bot's sight: a target missing from it is gone.
      for (const m of (fight.groups ?? []).find((g) => g.id === e.group)?.members ?? []) (r.members ??= new Set()).add(m);
      const target =
        r.kind === "sweep"
          ? objects.some((o) => o.type === "creep" && !o.ours && o.room === e.room && !/^(Keeper|Invader)/.test(o.name ?? ""))
          : objects.some((o) => r.members?.has(o.id));
      r.idle = target ? 0 : r.idle + 1;
      if (r.idle > r.targetless) Object.assign(r, { targetless: r.idle, targetlessAt: t - r.idle + 1 });
    }
  }
  return { engagements: all, casts };
}

/** The smells of the seat whose output is `dir`, each `{ smell, t, evidence, count }`. */
async function smellsOf(dir) {
  const seat = JSON.parse(readFileSync(join(dir, "seat.json"), "utf8"));
  const samples = samplesOf(dir);
  const at = (t) => samples.findLast((s) => s.t <= t) ?? samples[0];
  const colonies = new Set([seat.room, ...samples.flatMap((s) => Object.entries(s.claimed ?? {}).filter(([, c]) => c.level > 0).map(([r]) => r))]);
  const lives = jsonLines(join(dir, "bodies.jsonl")).filter((b) => b.ours);
  const costs = new Map(lives.map((b) => [b.name, b]));
  const { engagements, casts } = await engagementsOf(dir, costs);
  const found = [];
  const worst = (smell, hits, key, show) => {
    if (!hits.length) return;
    const top = hits.toSorted((a, b) => key(b) - key(a))[0];
    found.push({ smell, t: top.t, evidence: show(top), count: hits.length });
  };

  const visits = Map.groupBy(lives.filter((b) => fights(b.body) && b.name.startsWith("fight-")), (b) => b.name);
  const homebound = [...visits].flatMap(([name, rows]) => {
    const e = engagements.get(Number(name.split("-")[2]));
    const end = rows.at(-1);
    return end.ttl <= AGED && rows.every((r) => Object.keys(r.rooms).every((room) => colonies.has(room))) && e && !colonies.has(e.room)
      ? [{ t: end.last, name, body: end.body, e }]
      : [];
  });
  worst("aged at home", homebound, (h) => -h.t, (h) => `${h.name} (${h.body}) died of age at home, its ${h.e.kind} #${h.e.id} in ${h.e.room}`);
  const late = [...casts].map(([name, c]) => ({ name, ...c, t: c.first, late: c.last - c.first - SPAWN_TIME_PER_PART * c.body.length })).filter((c) => c.late > CAST_LATE);
  worst("cast unspawned", late, (c) => c.late, (c) => `${c.name} (${c.body}) for #${c.engagement} ${engagements.get(c.engagement)?.room} asked t${c.first}..t${c.last}, ${c.late} ticks past its spawn time`);
  const all = [...engagements.values()];
  worst("forming forever", all.filter((e) => e.forming > FORMING_MOST).map((e) => ({ ...e, t: e.formingAt })), (e) => e.forming, (e) => `${e.kind} #${e.id} ${e.room} formed ${e.forming} ticks from t${e.formingAt}`);
  const targetless = all.filter((e) => e.targetless > TARGETLESS_MOST).map((e) => ({ ...e, t: e.targetlessAt }));
  const idleFor = (what) => (e) => `${e.kind} #${e.id} ${e.room} ${what} ${e.targetless} ticks from t${e.targetlessAt}`;
  worst("defend raid gone", targetless.filter((e) => e.kind !== "sweep"), (e) => e.targetless, idleFor("open with every body of its raid gone from the world"));
  worst("sweep room empty", targetless.filter((e) => e.kind === "sweep"), (e) => e.targetless, idleFor("open with no rival body in its room"));

  const overset = all.flatMap((e) => {
    const s = at(e.opened);
    const before = at(e.opened - CREEP_LIFE);
    const rate = (s.ledger.dug - before.ledger.dug) / Math.max(1, s.t - before.t);
    const pays = Math.round(s.energy + s.stored + rate * CREEP_LIFE);
    const spawnTime = SPAWN_SHARE * s.spawns * CREEP_LIFE;
    return e.cost > pays || e.parts * SPAWN_TIME_PER_PART > spawnTime ? [{ ...e, t: e.costAt, pays, spawnTime, over: e.cost / pays }] : [];
  });
  worst("set over bank", overset, (e) => e.over, (e) => `${e.kind} #${e.id} ${e.room} set ${e.cost}e/${e.parts} parts; bank+dig pays ${e.pays}e, spawn share ${e.spawnTime} ticks`);

  // seat.json holds every room's deaths: a raid's are those in the rooms the seat follows.
  const followed = new Set(Object.keys(samples.at(-1).rooms));
  const theirDeaths = seat.deaths.filter((d) => d.user !== seat.uid && followed.has(d.room));
  // Economy bodies: no strike or heal, not a lone MOVE (a scout).
  const economy = samples.map((s) => ({ t: s.t, n: s.bodies.filter((b) => { const l = b.split(" ")[1] ?? ""; return !fights(l) && l !== "m"; }).length }));
  let peak = 0;
  for (let i = 0; i < economy.length; i++) {
    peak = Math.max(peak, economy[i].n);
    if (peak < PEAK || economy[i].n > COLLAPSED_TO) continue;
    let j = i;
    while (j + 1 < economy.length && economy[j + 1].n <= COLLAPSED_TO) j++;
    if (economy[j].t - economy[i].t >= COLLAPSED_FOR) {
      const killed = seat.deaths.filter((d) => d.user === seat.uid && d.by && d.t > economy[i].t - CREEP_LIFE && d.t <= economy[i].t).length;
      found.push({ smell: "economy collapse", t: economy[i].t, evidence: `${peak} economy bodies at peak, ≤${COLLAPSED_TO} t${economy[i].t}..t${economy[j].t}${j === economy.length - 1 ? " (to the end)" : ""}; ${killed} of ours killed in the life before`, count: 1 });
      break;
    }
    i = j;
  }

  const last = samples.at(-1);
  const decayed = Object.entries(last.rooms)
    .filter(([r, l]) => !colonies.has(r) && l.dug >= DUG_LEAST && l.decayed / l.dug > DECAY_SHARE)
    .map(([r, l]) => ({ t: last.t, r, l, share: l.decayed / l.dug }));
  const decay = (d) => `${d.r} decayed ${Math.round(d.l.decayed)} of ${Math.round(d.l.dug)} dug (${Math.round(100 * d.share)}%), hostile ${d.l.hostile} ticks`;
  worst("outpost decay", decayed.filter((d) => d.l.hostile < CONTESTED), (d) => d.share, decay);
  worst("contested decay", decayed.filter((d) => d.l.hostile >= CONTESTED), (d) => d.share, decay);

  const held = new Set();
  const drops = new Map();
  for await (const { t, counts } of stream(join(dir, "cpu.jsonl"))) {
    const now = new Set(outpostsIn(Object.fromEntries(counts)));
    for (const r of held) if (!now.has(r)) drops.set(r, [...(drops.get(r) ?? []), t]);
    held.clear();
    for (const r of now) held.add(r);
  }
  const flaps = [...drops].filter(([, ts]) => ts.length >= FLAP_DROPS).map(([r, ts]) => ({ r, ts, t: ts[0] }));
  worst("outpost flap", flaps, (f) => f.ts.length, (f) => `${f.r} dropped ${f.ts.length}× at t${f.ts.slice(0, 6).join(",t")}${f.ts.length > 6 ? ",…" : ""}`);

  const wasted = samples.flatMap((s, i) => {
    if (!i || !s.safeMode || samples[i - 1].safeMode) return [];
    const from = samples[i - 1].t;
    const raid = theirDeaths.filter((d) => d.t > from && d.t <= s.t + RAID_DIES);
    const after = samples.find((x) => x.t > s.t + RAID_DIES);
    return raid.length && after && !after.hostiles ? [{ t: from, to: s.t, raid, hostiles: samples[i - 1].hostiles }] : [];
  });
  worst("safe mode wasted?", wasted, (w) => -w.t, (w) => `on in t${w.t}..t${w.to}, ${w.hostiles} hostiles; ${w.raid.map((d) => `${d.body}@t${d.t}`).join(", ")} dead within ${RAID_DIES} ticks of t${w.to}`);

  const blocked = new Map(jsonLines(join(dir, "metrics.jsonl")).filter((m) => m.row === seat.room).map((m) => [m.t, m.values["blocked.bank"] ?? 0]));
  const stuck = [];
  for (let i = 0; i < samples.length; i++) {
    const s = samples[i];
    const holds = (x) => (blocked.get(x.t) ?? blocked.get(x.t + 1) ?? 0) >= BANK_BLOCKED && x.energy < x.capacity && Math.abs(x.energy - s.energy) <= BANK_FLAT;
    if (!holds(s)) continue;
    let j = i;
    while (j + 1 < samples.length && holds(samples[j + 1])) j++;
    if (samples[j].t - s.t >= BANK_STUCK) stuck.push({ t: s.t, to: samples[j].t, energy: s.energy, capacity: s.capacity });
    i = j;
  }
  worst("bank stuck", stuck, (b) => b.to - b.t, (b) => `${b.energy}/${b.capacity} blocked on the bank t${b.t}..t${b.to}`);
  return found;
}

const seatDirs = (out) => JSON.parse(PLAYERS).map(({ name }, i) => ({ name, dir: i ? join(out, name) : out }));
const seatsOf = async (out) => {
  const seats = [];
  for (const { name, dir } of seatDirs(out)) seats.push({ name, room: JSON.parse(readFileSync(join(dir, "seat.json"), "utf8")).room, smells: await smellsOf(dir) });
  return seats;
};
const usage = "usage: npm run soak:selfplay -- --seeds A..B|A,B,... [--ticks N] [--jobs J] | --read <out>";
if (process.argv[2] === "--read") {
  if (!process.argv[3]) (console.error(usage), process.exit(2));
  for (const s of await seatsOf(resolve(process.argv[3]))) for (const f of s.smells) console.log(`${s.name} ${s.room} ${f.smell} t${f.t} ×${f.count}: ${f.evidence}`);
  process.exit(0);
}
const { seeds, ticks, jobs } = seedFlags(process.argv.slice(2), usage);
const bin = simBinary();
if (!bin) process.exit(1);
const results = {};
await pool(seeds, jobs, async (seed) => {
  const out = soakDir(`selfplay-s${seed}`);
  try {
    const code = await runSim(bin, { gen: seed, ticks, players: PLAYERS, out });
    if (code !== 0) throw new Error(`sim exited ${code}`);
    const seats = await seatsOf(out);
    writeFileSync(join(out, "smells.json"), JSON.stringify(seats, null, 1) + "\n");
    results[seed] = { seats };
    console.error(`seed ${seed}: ${seats.map((s) => `${s.name} ${s.smells.length}`).join(", ")} smells`);
  } catch (e) {
    results[seed] = { failed: e.message };
    console.error(`seed ${seed}: ${e.message}`);
  } finally {
    for (const { dir } of seatDirs(out)) rmSync(join(dir, "fight.jsonl"), { force: true });
  }
});

console.log(`# soak:selfplay seeds ${seeds[0]}..${seeds.at(-1)}, ${ticks} ticks\n`);
console.log("| seed | seat | room | smell | tick | count | evidence |\n|---:|---|---|---|---:|---:|---|");
for (const seed of seeds) {
  const r = results[seed];
  if (r.failed !== undefined) console.log(`| ${seed} | - | - | failed: ${r.failed} | - | - | - |`);
  else for (const s of r.seats) for (const f of s.smells) console.log(`| ${seed} | ${s.name} | ${s.room} | ${f.smell} | ${f.t} | ${f.count} | ${f.evidence} |`);
}
const tally = Object.entries(Object.groupBy(seeds.flatMap((s) => results[s].seats?.flatMap((x) => x.smells) ?? []), (f) => f.smell));
console.log(`\n${tally.map(([k, v]) => `${k}: ${v.length} seats`).join("; ") || "no smells"}\n`);
for (const seed of seeds.filter((s) => results[s].failed !== undefined || results[s].seats.some((x) => x.smells.length)))
  console.log(`- seed ${seed}: \`npm run watch:sim -- --gen ${seed} --players '${PLAYERS}'\``);
