// The scenario library (ARCHITECTURE.md §10.2): each scenario in scenarios/
// is a start, a number of ticks and bounds on what the run measured. Starts
// are recipes (room, seed, late, node), or a small crafted world checked in
// for what no recipe stands (a road through swamp); scenarios
// with one start share one soak, run to the longest of their ticks. A start
// `at` tick T resumes the world a soak of `bundle` left at T: that bundle is
// pinned to a commit so the world is played once and cached under
// soak-runs/worlds/, and the bundle under test runs on from it as a deploy
// would, Memory kept.
//
// Usage: npm run scenario -- [name...] [--modules m,n] [--list] [--bot main.js] [--jobs J] [--nightly] [--fetch]
//        npm run scenario -- <name> --watch [--tick-ms N] [--port N]
//   name    the scenarios to run (default all but the nightly ones), by file name
//   --modules  also the scenarios naming any of these modules (directories under src/core)
//   --list  print the selection and run nothing
//   --nightly  all of them, with those marked "nightly": the offence target,
//           kept out of `jj ship`
//   --fetch  fetch the tarball a rival's `fetch.json` names; without it a
//           scenario whose rival is not cached is SKIPped, never failed
//   --bot   the bundle (default dist/main.js)
//   --jobs  soaks at once (default half the cores)
//   --watch the one scenario's soak watched (soak's --watch), in this terminal
//
// A scenario file: { "why": the invariant, "modules": the src/core modules it tests, "nightly"?, "start": { "room", "seed",
// "late"?, "node"?, "invaders"? (soak's --invaders), "wave"? (soak's --wave), "core"? (soak's --core), "unsafe"?, "ended"?, "at"?, "bundle"?,
// "opponents"? (soak's --opponents) } or { "world": a capture-shaped file
// (soak's --world; crafted ones under worlds/), "seed", "invaders"?, "wave"?, "opponents"? (the world's own users), "declarations"? (in place of the world's), "withdrawn"? (rooms whose open siege the owner has just withdrawn) }, "ticks", "assert": [{ "measure", "from"?, "to"?, "max"?, "min"?, "who"?, "room"?, "kind"?, "name"? }] };
// a measure is one of `measures` below, read off the soak's output over ticks
// [from, to), counted from the run's first tick.
// Exits non-zero when a bound fails or a soak does, or loses ticks twice.
import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from "node:fs";
import { availableParallelism } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { bugs, engagement, report } from "./prediction.mjs";
import { sweep } from "./prune.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
// A count ceiling holds for every scenario (`ARCHITECTURE.md` §9.3), so an assert
// naming one takes its bound from there rather than repeating the number.
const countCeilings = JSON.parse(readFileSync(join(REPO, "scripts/ceilings.json"), "utf8")).counts;
const LIBRARY = join(HERE, "scenarios");
const fail = (m) => {
  console.error(m);
  process.exit(1);
};

const names = [];
const flags = {};
// Soak's watch flags, passed on as given; soak.mjs checks them.
const watch = [];
const WATCH_VALUES = ["--tick-ms", "--port"];
const rest = process.argv.slice(2);
for (let i = 0; i < rest.length; i++)
  if (rest[i] === "--bot" || rest[i] === "--jobs" || rest[i] === "--modules") flags[rest[i]] = rest[++i] ?? fail(`${rest[i - 1]} takes a value`);
  else if (rest[i] === "--list" || rest[i] === "--nightly" || rest[i] === "--fetch") flags[rest[i]] = true;
  else if (rest[i] === "--watch") watch.push(rest[i]);
  else if (WATCH_VALUES.includes(rest[i])) watch.push(rest[i], rest[++i]);
  else if (rest[i].startsWith("--")) fail("usage: npm run scenario -- [name...] [--modules m,n] [--list] [--bot main.js] [--jobs J] [--nightly] [--fetch] | <name> --watch [--tick-ms N] [--port N]");
  else names.push(rest[i]);

const CORE = join(REPO, "src/core");
const modules = readdirSync(CORE).filter((m) => m !== "tests" && statSync(join(CORE, m)).isDirectory());
const library = readdirSync(LIBRARY)
  .filter((f) => f.endsWith(".json"))
  .map((f) => ({ name: basename(f, ".json"), ...JSON.parse(readFileSync(join(LIBRARY, f), "utf8")) }));
for (const s of library)
  if (!s.modules?.length || s.modules.some((m) => !modules.includes(m))) fail(`${s.name}: "modules" must name some of ${modules.join(", ")}`);
// A scripted rival shares our blind spots: each fight kind meets an open-source bot at least once.
const OPEN_SOURCE = ["scripts/xxscreeps/engine/hivemind", "scripts/xxscreeps/engine/tooangel"];
const FIGHT_KINDS = ["siege", "sweep", "defend"];
for (const kind of FIGHT_KINDS)
  if (!library.some((s) => s.name.startsWith(`${kind}-`) && s.start.opponents?.some((o) => OPEN_SOURCE.includes(o.bot)))) fail(`no ${kind}-* scenario seats an open-source bot (${OPEN_SOURCE.join(", ")})`);
// A siege opens only on a declared room, so a crafted world holding one open
// on another is a state no run reaches, unless the owner withdrew it (`withdrawn`).
for (const { name, start } of library.filter((s) => s.start.world)) {
  const world = JSON.parse(readFileSync(resolve(REPO, start.world), "utf8"));
  const declared = [...(start.declarations ?? world.declarations ?? {}).sieges ?? [], ...(start.withdrawn ?? [])];
  for (const e of world.memory?.v2?.fight?.engagements ?? [])
    if (e.kind === "siege" && !declared.includes(e.room)) fail(`${name}: ${start.world} holds a siege open on ${e.room}, which its declarations do not name`);
}
for (const s of library.filter((s) => FIGHT_KINDS.some((k) => s.name.startsWith(`${k}-`))))
  if (!s.assert.some((a) => a.measure === "bodiesLost" && a.max !== undefined)) fail(`${s.name}: a fight scenario bounds bodiesLost by a "max"`);
for (const n of names) if (!library.some((s) => s.name === n)) fail(`no scenario ${n}; the library: ${library.map((s) => s.name).join(", ")}`);
const touched = flags["--modules"]?.split(",").filter(Boolean) ?? [];
for (const m of touched) if (!modules.includes(m)) fail(`no module ${m}; the modules: ${modules.join(", ")}`);
const meets = (s) => (touched.length ? s.modules.some((m) => touched.includes(m)) : !names.length);
const scenarios = library.filter((s) => names.includes(s.name) || (meets(s) && (flags["--nightly"] || !s.nightly)));
if (flags["--list"]) {
  for (const s of scenarios) console.log(`${s.name}  ${s.modules.join(",")}`);
  console.log(`${scenarios.length} of ${library.length} scenarios`);
  process.exit(0);
}
if (watch.length && names.length !== 1) fail("--watch runs exactly one scenario: npm run scenario -- <name> --watch");
// The Ctrl-C that ends a watched soak reaches this process too; its asserts still print.
if (watch.length) process.on("SIGINT", () => {});
const bot = resolve(flags["--bot"] ?? join(REPO, "dist/main.js"));
if (!existsSync(bot)) fail(`no bundle at ${bot}: npm run build first`);
const jobs = Number(flags["--jobs"] ?? Math.max(1, Math.floor(availableParallelism() / 2)));

sweep(join(REPO, "soak-runs"));

// Ticks are counted from the bot's first, so a window means the same on every start.
// Each engagement the run priced, as `prediction.mjs` reads a row; one never
// priced predicted nothing.
const rowsOf = (dir) =>
  readFileSync(join(dir, "engagements.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
const engagementsOf = (dir) =>
  rowsOf(dir)
    .filter((e) => e.predicted)
    .map((e) => ({ p: e.predicted, a: engagement(e) }));
const livesOf = (dir) =>
  readFileSync(join(dir, "bodies.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
const fightLives = (dir) => livesOf(dir).filter((b) => b.ours && b.name.startsWith("fight-") && b.hits !== undefined);
const moves = (body) => [...body].filter((p) => p === "m").length;
const scoutsOf = (dir) => livesOf(dir).filter((b) => b.ours && b.name.startsWith("scout-"));
const walksOf = (dir) => rowsOf(dir).flatMap((e) => (e.blob ? [e.blob] : []));
const foughtOf = (dir) => rowsOf(dir).flatMap((e) => (e.fought ? [e.fought] : []));
const mean = (xs) => xs.reduce((a, b) => a + b, 0) / Math.max(1, xs.length);
const samplesOf = (dir) => readFileSync(join(dir, "samples.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
// The bot's per-tick counts (`ARCHITECTURE.md` §9.3), as `soak-step` reads them off
// the observe segment; a tick whose segment was not rewritten carries last tick's.
const countsOf = (dir, a) =>
  readFileSync(join(dir, "cpu.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l))
    .filter((r) => r.counts && r.t >= (a.from ?? 0) && r.t < (a.to ?? Infinity))
    .map((r) => Object.fromEntries(r.counts));
// A run that reported no counts fails its ceiling: Infinity is over any max.
const withCounts = (dir, a, f) => {
  const rows = countsOf(dir, a);
  return rows.length ? f(rows) : Infinity;
};
const at = (samples, t) => samples.find((s) => s.t >= t) ?? samples.at(-1);
const sideOf = (a) => (s) => (a.who ? s.others[a.who] : s);
const within = (samples, a) => samples.filter((s) => s.t >= (a.from ?? 0) && s.t < (a.to ?? Infinity));
const upgradedIn = (s, a) => (a.room ? (s.rooms[a.room]?.upgrade ?? 0) : s.ledger.upgrade);
const builtIn = (s, a) => (a.room ? (s.rooms[a.room]?.build ?? 0) : s.ledger.build);
const measures = {
  // Energy lost to pile decay.
  decayed: (samples, a) => at(samples, a.to ?? Infinity).ledger.decayed - at(samples, a.from ?? 0).ledger.decayed,
  // Roads and containers lost between samples, over every room the soak follows.
  decayedAway: (samples, a) => {
    const standing = (s) => Object.values(s.rooms).reduce((n, r) => n + r.roads + r.containers, 0);
    const seen = within(samples, a);
    return seen.slice(1).reduce((n, s, i) => n + Math.max(0, standing(seen[i]) - standing(s)), 0);
  },
  // Ticks bodies lost beside a road: grounded off it, or stopped for a body on it.
  roadLoss: (samples, a) => at(samples, a.to ?? Infinity).roadLoss - at(samples, a.from ?? 0).roadLoss,
  stillLoaded: (samples, a) => at(samples, a.to ?? Infinity).stillLoaded,
  // The lowest level a declared claim stands at, and the first tick every one has a spawn.
  claimLevel: (samples, a) => Math.min(...Object.values(at(samples, a.to ?? Infinity).claimed ?? {}).map((c) => c.level)),
  claimCharged: (samples) => samples.find((s) => Object.values(s.claimed ?? {}).every((c) => c.fire >= 1000) && Object.keys(s.claimed ?? {}).length)?.t ?? Infinity,
  // Scouts cast, the most ticks one stood still (a scout never idles), and
  // the ticks scouts stood in `room`.
  scoutsCast: (_, __, dir) => new Set(scoutsOf(dir).map((b) => b.name)).size,
  scoutStill: (_, __, dir) => Math.max(0, ...scoutsOf(dir).map((b) => b.still ?? 0)),
  scoutTicksIn: (_, a, dir) => scoutsOf(dir).reduce((n, b) => n + (b.rooms[a.room] ?? 0), 0),
  // The most ticks `room` went without one of our bodies in it.
  stalest: (_, a, dir) => {
    const lives = livesOf(dir).filter((b) => b.ours);
    const stays = lives.flatMap((b) => (b.entered ?? []).map(([r, t], i, e) => [r, t, e[i + 1]?.[1] ?? b.last + 1]).filter(([r]) => r === a.room));
    let [seen, worst] = [Math.min(...lives.map((b) => b.first)), 0];
    for (const [, t, until] of stays.sort((x, y) => x[1] - y[1])) [worst, seen] = [Math.max(worst, t - seen), Math.max(seen, until)];
    return Math.max(worst, Math.max(...lives.map((b) => b.last)) - seen);
  },
  claimersCast: (samples, a) => new Set(within(samples, a).flatMap((s) => s.bodies.filter((b) => b.startsWith("claimer-")).map((b) => b.split("@")[0]))).size,
  claimSpawned: (samples) => samples.find((s) => Object.values(s.claimed ?? {}).every((c) => c.spawns > 0) && Object.keys(s.claimed ?? {}).length)?.t ?? Infinity,
  // The most of a compound (`resource`) stood at once.
  made: (samples, a) => Math.max(0, ...samples.map((s) => s.made?.[a.resource] ?? 0)),
  // What `room`'s storage, terminal, labs, factory and power spawn hold at the window's end, and the non-energy of it.
  heldAtEnd: (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.held ?? Infinity,
  oresAtEnd: (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.ores ?? 0,
  ore: (samples, a) => at(samples, a.to ?? Infinity).ore - at(samples, a.from ?? 0).ore,
  bounced: (samples, a) => at(samples, a.to ?? Infinity).bounced - at(samples, a.from ?? 0).bounced,
  pouredBack: (samples, a) => at(samples, a.to ?? Infinity).pouredBack - at(samples, a.from ?? 0).pouredBack,
  // Energy spent on construction (`room`: in that room alone).
  build: (samples, a) => builtIn(at(samples, a.to ?? Infinity), a) - builtIn(at(samples, a.from ?? 0), a),
  // Energy a room's spawns put into bodies (`room`: the room).
  bodiesSpent: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.bodies ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.bodies ?? 0),
  // The fewest ticks `room`'s controller stood from its downgrade over the run, never ours: -Infinity.
  leastDowngrade: (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.leastDowngrade ?? -Infinity,
  // Energy our bodies carried out of a home across its border.
  carriedOut: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.carriedOut ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.carriedOut ?? 0),
  dug: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.dug ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.dug ?? 0),
  // The least energy the home's storage and containers held.
  leastStored: (samples, a) => Math.min(...within(samples, a).map((s) => s.stored)),
  // Percent of the colony's harvest the engagements cast into bodies.
  fightShare: (samples, _, dir) => Math.round((100 * rowsOf(dir).reduce((n, e) => n + (e.cast ?? 0), 0)) / Math.max(1, samples.at(-1).ledger.dug)),
  towerSpent: (samples, a) => at(samples, a.to ?? Infinity).ledger.tower - at(samples, a.from ?? 0).ledger.tower,
  upgrade: (samples, a) => upgradedIn(at(samples, a.to ?? Infinity), a) - upgradedIn(at(samples, a.from ?? 0), a),
  // Percent of samples whose buffer stood empty with an upgrader beside it.
  dryBuffer: (samples, a) => {
    const seen = within(samples, a);
    return Math.round((100 * seen.filter((s) => s.dry).length) / Math.max(1, seen.length));
  },
  // Harvesters seen digging from more than one tile.
  harvestersOnTwoPosts: (samples, a) => {
    const tiles = new Map();
    for (const s of within(samples, a))
      for (const b of s.bodies) {
        const m = /^(harvester-\S+)@(\S+) \S+ \S+ .*harvest/.exec(b);
        if (m) tiles.set(m[1], (tiles.get(m[1]) ?? new Set()).add(m[2]));
      }
    return [...tiles.values()].filter((t) => t.size > 1).length;
  },
  // The first tick a container stands beside a source (never: Infinity).
  firstSourceContainer: (samples) => samples.find((s) => s.sourceContainers > 0)?.t ?? Infinity,
  sourceContainersAtEnd: (samples, a) => at(samples, a.to ?? Infinity).sourceContainers,
  // Links standing beside a source's container.
  postedLinksAtEnd: (samples, a) => at(samples, a.to ?? Infinity).postedLinks,
  linksAtEnd: (samples, a) => at(samples, a.to ?? Infinity).built.link ?? 0,
  // Energy lost to link transfers: none means no link ever sent.
  linked: (samples, a) => at(samples, a.to ?? Infinity).ledger.linked - at(samples, a.from ?? 0).ledger.linked,
  // The least any declared outpost netted after its bodies (`rooms`, the
  // colony's own first); none declared: -Infinity.
  poorestOutpost: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    return Math.min(...Object.keys(last.rooms).slice(1).map((r) => last.rooms[r].net - (first.rooms[r]?.net ?? 0)));
  },
  // Ticks an enemy stood in the colony's own room.
  homeHostileTicks: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    const home = Object.keys(last.rooms)[0];
    return last.rooms[home].hostile - (first.rooms[home]?.hostile ?? 0);
  },
  // Engagements whose predicted Win or Kite was lost (§10.1).
  predictedWinsLost: (_, __, dir) => bugs(engagementsOf(dir)).length,
  // Engagements opened: an attack the bot never answered tests nothing.
  engagements: (_, __, dir) => engagementsOf(dir).length,
  // Priced or not: a sweep that finds nothing opens unanswered.
  engagementsOpened: (_, a, dir) => rowsOf(dir).filter((e) => (!a.kind || e.kind === a.kind) && (!a.room || e.room === a.room)).length,
  // Acts the engine refused, every kind (the soak's refusals.json).
  refusals: (_, __, dir) => Object.values(JSON.parse(readFileSync(join(dir, "refusals.json"), "utf8"))).reduce((n, c) => n + c, 0),
  // Raids an answered engagement saw die without disengaging.
  raidsKilled: (_, __, dir) => engagementsOf(dir).filter(({ a }) => a.won === true).length,
  // Over the sets of two or more that walked to their raid (soak-step's
  // `blob`), the worst: the percent of approaching ticks every member stood
  // within ⌊n/2⌋ + 1 of the median member (none walked: -Infinity), the
  // percent of member-ticks one stood still free of fatigue, and the median
  // member's ticks from the muster to within 3 of the raid (none: Infinity).
  blobAnchorRange: (_, __, dir) => Math.min(...walksOf(dir).map((w) => (100 * w.near) / w.ticks), ...(walksOf(dir).length ? [] : [-Infinity])),
  blobStallTicks: (_, __, dir) => Math.max(...walksOf(dir).map((w) => (100 * w.stalls) / Math.max(1, w.memberTicks)), ...(walksOf(dir).length ? [] : [Infinity])),
  blobArrival: (_, __, dir) => Math.max(...walksOf(dir).map((w) => w.arrival ?? Infinity), ...(walksOf(dir).length ? [] : [Infinity])),
  // Over the sets of two or more that fought (soak-step's `fought`), the
  // worst: the most hits one member lost over the mean of its set's, its
  // lowest share of its hits in percent, and the percent of member-ticks
  // under half hits stood out of the raid's reach (none: 100); and every
  // engagement's bodies lost.
  fightDamageSpread: (_, __, dir) => Math.max(1, ...foughtOf(dir).map((f) => Math.max(...Object.values(f.taken)) / Math.max(1, mean(Object.values(f.taken))))),
  fightLowestHits: (_, __, dir) => Math.min(100, ...foughtOf(dir).flatMap((f) => Object.values(f.low).map((l) => 100 * l))),
  fightWoundedOut: (_, __, dir) => Math.min(100, ...foughtOf(dir).filter((f) => f.wounded).map((f) => (100 * f.out) / f.wounded)),
  // The most percent of a member's ticks it swung back (soak-step's `swung`),
  // over the engagements of `kind` (none named: every one); none seen: 0.
  oscillation: (_, a, dir) =>
    Math.max(0, ...rowsOf(dir).filter((e) => !a.kind || e.kind === a.kind).flatMap((e) => Object.values(e.swung).map((s) => Math.round((100 * s.swings) / Math.max(1, s.ticks))))),
  // The most ticks a siege took from its open to its breach (one that never breached: Infinity).
  breachTicks: (_, __, dir) => Math.max(...rowsOf(dir).filter((e) => e.kind === "siege").map((e) => (e.breached ?? (e.disengaged || e.closed === null ? Infinity : e.closed)) - e.opened), ...(rowsOf(dir).some((e) => e.kind === "siege") ? [] : [Infinity])),
  // The rival's structures the last siege opened left standing in its room (none opened: Infinity).
  razeLeft: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "siege").sort((a, b) => a.opened - b.opened).at(-1)?.left ?? Infinity,
  // Over the first siege: the ticks from its open until its first target fell
  // (never: Infinity), the ramparts its room lost before, and its bodies that
  // landed in a region the target is walled off from.
  siegeFirstRaze: (_, __, dir) => {
    const e = rowsOf(dir).find((e) => e.kind === "siege");
    return e?.razed === undefined ? Infinity : e.razed - e.opened;
  },
  siegeRampartsLost: (_, __, dir) => rowsOf(dir).find((e) => e.kind === "siege")?.rampartsLost ?? Infinity,
  // 1 when the first siege's first target is of `kind`, else 0.
  siegeFirstTarget: (_, a, dir) => Number(rowsOf(dir).find((e) => e.kind === "siege")?.firstKind === a.kind),
  siegeWalledEntries: (_, __, dir) => Object.values(rowsOf(dir).find((e) => e.kind === "siege")?.entries ?? {}).filter((s) => s === "walled").length,
  // The least hits a siege's target (a spawn, a core) ever stood at (none seen: Infinity).
  siegeTargetLow: (_, __, dir) => Math.min(Infinity, ...rowsOf(dir).filter((e) => e.kind === "siege" && e.target).map((e) => e.target.low)),
  // Over the sweeps, the rival's bodies killed and harvest denied (soak-step's `killed`, `denied`), and those less what they cast.
  sweepKilled: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "sweep").reduce((n, e) => n + (e.killed ?? 0), 0),
  sweepDenied: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "sweep").reduce((n, e) => n + (e.denied ?? 0), 0),
  sweepNet: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "sweep").reduce((n, e) => n + (e.killed ?? 0) + (e.denied ?? 0) - e.cast, 0),
  sweepKept: (_, __, dir) => {
    const rows = rowsOf(dir).filter((e) => e.kind === "sweep");
    return Math.round((100 * rows.reduce((n, e) => n + (e.kept ?? 0), 0)) / Math.max(1, rows.reduce((n, e) => n + (e.held ?? 0), 0)));
  },
  bodiesLost: (_, __, dir) => rowsOf(dir).reduce((n, e) => n + (e.dead ?? 0), 0),
  // Of those, the ones that died in their engagement's room with no set-mate within 3 (soak-step's `alone`).
  diedAlone: (_, __, dir) => rowsOf(dir).reduce((n, e) => n + (e.alone ?? 0), 0),
  // Our bodies gone from the colony's rooms and their neighbours with life left: died, or walked
  // through a room the soak does not follow.
  bodiesOut: (_, __, dir) => {
    const lives = livesOf(dir);
    const end = Math.max(...lives.map((b) => b.last));
    return new Set(lives.filter((b) => b.ours && b.last < end && b.ttl > 1).map((b) => b.name)).size;
  },
  // Ticks our fight bodies stood in the rooms the soak follows once back from beyond them.
  fightTicksBack: (_, __, dir) => {
    const lives = livesOf(dir).filter((b) => b.ours && b.name.startsWith("fight-"));
    const first = new Map(lives.map((b) => [b.name, Infinity]));
    for (const b of lives) first.set(b.name, Math.min(first.get(b.name), b.first));
    return lives.filter((b) => b.first > first.get(b.name)).reduce((n, b) => n + Object.values(b.rooms).reduce((m, k) => m + k, 0), 0);
  },
  bodiesStruck: (_, __, dir) => livesOf(dir).reduce((n, b) => n + (b.ours ? (b.struck ?? 0) : 0), 0),
  // Ticks a fight body healed: its own damage or a mate's.
  fightHeals: (_, __, dir) => fightLives(dir).reduce((n, b) => n + (b.acts.heal ?? 0) + (b.acts.rangedHeal ?? 0), 0),
  // The fight bodies last seen with fewer MOVE parts live than they were cast
  // with: a part shot off is gone, and only a live MOVE gives power.
  legsLost: (_, __, dir) =>
    fightLives(dir).filter((b) => {
      const cast = b.body.length;
      return moves(b.body) > moves(b.body.slice(Math.max(0, cast - Math.ceil(b.hits / 100))));
    }).length,
  // The damage struck on the body called `name`.
  struckOn: (_, a, dir) => livesOf(dir).reduce((n, b) => n + (b.name === a.name ? (b.struck ?? 0) : 0), 0),
  // The most energy one engagement cast, of the bodies that stood in it.
  mostCast: (_, __, dir) => Math.max(0, ...rowsOf(dir).map((e) => e.cast ?? 0)),
  // The most energy a siege cast that stood before its breach (none breached: Infinity).
  breachCast: (_, __, dir) => Math.max(...rowsOf(dir).filter((e) => e.kind === "siege").map((e) => e.breachCast ?? Infinity)),
  faults: (_, __, dir) => readFileSync(join(dir, "faults.jsonl"), "utf8").split("\n").filter(Boolean).length,
  // The most levels the room stood below the highest it held before (`who`:
  // an opponent's room, else ours); a room lost stands at level 0.
  levelsLost: (samples, a) => {
    let top = 0;
    let worst = 0;
    for (const s of within(samples, a).map(sideOf(a))) {
      top = Math.max(top, s.rcl);
      worst = Math.max(worst, top - s.rcl);
    }
    return worst;
  },
  spawnsLost: (samples, a) => {
    const spawns = within(samples, a).map((s) => sideOf(a)(s).spawns);
    return spawns[0] - Math.min(...spawns);
  },
  fewestCreeps: (samples, a) => Math.min(...within(samples, a).map((s) => sideOf(a)(s).creeps)),
  // The first tick our room stood in safe mode (never: Infinity).
  safeModeAt: (samples, a) => within(samples, a).find((s) => s.safeMode)?.t ?? Infinity,
  creepsAtEnd: (samples, a) => sideOf(a)(at(samples, a.to ?? Infinity)).creeps,
  // A core engagement that loses sight must not drop its razer's asks.
  closedOnCore: (samples, _, dir) => rowsOf(dir).filter((e) => e.core && e.closed !== null && (at(samples, e.closed).rooms[e.room]?.cores ?? 0) > 0).length,
  // A wave's engagement left open holds its bodies and asks: the most ticks
  // one with no core stayed open after its group was last seen.
  heldPastWave: (samples, _, dir) => Math.max(0, ...rowsOf(dir).filter((e) => !e.core).map((e) => (e.closed ?? samples.at(-1).t) - (e.seen ?? e.opened))),
  // Invader cores standing in the declared outposts at the window's end.
  coresAtEnd: (samples, a) => Object.values(at(samples, a.to ?? Infinity).rooms).reduce((n, r) => n + (r.cores ?? 0), 0),
  // Enemies standing in our room at the window's end.
  hostilesAtEnd: (samples, a) => at(samples, a.to ?? Infinity).hostiles,
  // `Hostiles.armed` off part letters; WORK counts anywhere, as every sample is our room.
  attackersAtEnd: (samples, a) => {
    const bodies = at(samples, a.to ?? Infinity).foes.map((f) => f.split(" ")[1]);
    const strikes = (b) => /[ar]/.test(b);
    return bodies.filter((b) => strikes(b) || b.includes("w") || (b.includes("h") && bodies.some(strikes))).length;
  },
  // The most floods one tick ran (a miss is one flood), and the percent of
  // `fieldOn` lookups that missed: §1's "counting hits and misses".
  floodsMost: (_, a, dir) => withCounts(dir, a, (rows) => Math.max(0, ...rows.map((c) => c.atlasMisses ?? 0))),
  threatLookupsMost: (_, a, dir) => withCounts(dir, a, (rows) => Math.max(0, ...rows.map((c) => c.threatLookups ?? 0))),
  atlasMissRate: (_, a, dir) =>
    withCounts(dir, a, (rows) => {
      const hits = rows.reduce((n, c) => n + (c.atlasHits ?? 0), 0);
      const misses = rows.reduce((n, c) => n + (c.atlasMisses ?? 0), 0);
      return Number(((100 * misses) / Math.max(1, hits + misses)).toFixed(2));
    }),
};
for (const s of scenarios) for (const a of s.assert) if (!measures[a.measure]) fail(`${s.name}: no measure ${a.measure}`);
for (const { name, start } of scenarios)
  if ((start.at === undefined) !== (start.bundle === undefined) || (start.at !== undefined && (start.node || !/^[0-9a-f]{8,40}$/.test(start.bundle))))
    fail(`${name}: a start "at" a tick names its "bundle" by commit id, and no node`);
for (const { name, start } of scenarios)
  if (start.world && Object.keys(start).some((k) => !["world", "seed", "invaders", "wave", "opponents", "declarations", "withdrawn"].includes(k))) fail(`${name}: a "world" start takes a "seed", "invaders", "wave", "opponents", "declarations", "withdrawn" and nothing else`);

// A world holds its opponents' seats; their config is the scenario's.
const tag = (opponents = []) => (opponents.length ? `-o${createHash("sha256").update(JSON.stringify(opponents)).digest("hex").slice(0, 8)}` : "");
const seats = (opponents = []) => opponents.map(({ config, ...seat }) => seat);
const recipe = ({ world, room, seed, late = 0, node, invaders, wave, core, unsafe, ended, declarations }, opponents) =>
  `${world ? basename(world, ".json") : room}${declarations ? `-d${createHash("sha256").update(JSON.stringify(declarations)).digest("hex").slice(0, 8)}` : ""}-s${seed}${late ? `l${late}` : ""}${node ? `-${node}` : ""}${invaders ? `-i${invaders.replace(/[:,]/g, "_")}` : ""}${wave ? `-w${wave.replace(/[:,]/g, "_")}` : ""}${core ? `-c${core.replace(/[:,]/g, "_")}` : ""}${unsafe ? "-u" : ""}${ended ? "-e" : ""}${tag(opponents)}`;
// A cached world is replayed only by the opponents' code that played it, and
// the engine mods and world scripts it was stood and played under.
const codeOf = (...paths) =>
  paths
    .map((p) => resolve(REPO, p))
    .flatMap((path) => (statSync(path).isDirectory() ? readdirSync(path).sort().map((f) => join(path, f)) : [path]))
    .reduce((h, f) => h.update(f).update(readFileSync(f)), createHash("sha256"))
    .digest("hex")
    .slice(0, 8);
const harness = codeOf(...["seed", "waves", "core", "colony.mjs", "node.mjs", "live.mjs"].map((f) => join(HERE, "engine", f)));
const worldOf = (start) => `${recipe(start, seats(start.opponents).map((s) => ({ ...s, code: codeOf(s.bot === "@bot" ? bot : s.bot) })))}-t${start.at}-${start.bundle}-h${harness}`;
const worldDir = (start) => join(REPO, "soak-runs/worlds", worldOf(start));
const keyOf = (start) => `scenario-${basename(bot, ".js")}-${start.at ? `${worldOf(start)}${tag(start.opponents)}` : recipe(start, start.opponents)}`;
const runs = new Map();
for (const s of scenarios) {
  const run = runs.get(keyOf(s.start)) ?? { name: keyOf(s.start), label: `${recipe(s.start, s.start.opponents)}${s.start.at ? `-t${s.start.at}` : ""}`, start: s.start, ticks: 0, scenarios: [], secs: 0 };
  run.ticks = Math.max(run.ticks, s.ticks);
  run.scenarios.push(s.name);
  runs.set(run.name, run);
}

// Built once here: parallel builds of one project collide.
if ([...runs.values()].some((r) => r.start.node) && spawnSync("cargo", ["build", "--release", "--quiet", "--features", "tools", "--bin", "plan", "--manifest-path", join(REPO, "Cargo.toml")], { stdio: "inherit" }).status !== 0)
  fail("the plan tool did not build");
// Built in `.scratch/`, on disk (`/tmp` is RAM), with a `jj` that fails: the
// real one would name the build after the workspace above it, not "unknown".
const bundleAt = (commit) => {
  const out = join(REPO, "soak-runs/bundles", `${commit}.js`);
  if (existsSync(out)) return out;
  console.error(`building ${commit}`);
  mkdirSync(join(REPO, ".scratch"), { recursive: true });
  const src = mkdtempSync(join(REPO, ".scratch", "bundle-"));
  mkdirSync(join(src, ".bin"));
  writeFileSync(join(src, ".bin", "jj"), "#!/bin/sh\nexit 1\n", { mode: 0o755 });
  const env = { ...process.env, PATH: `${join(src, ".bin")}:${process.env.PATH}` };
  // A failed step answers its command, and the tree is removed before `fail` exits.
  const sh = (cmd) => (spawnSync("sh", ["-c", cmd], { cwd: src, env, stdio: ["ignore", "ignore", "inherit"] }).status === 0 ? null : cmd);
  const broke =
    sh(`git -C '${REPO}' archive ${commit} | tar -x && ln -s '${join(REPO, "node_modules")}' node_modules`) ??
    // A commit from before the port builds the F# bundle, with its own tools.
    sh(existsSync(join(src, "src/App")) ? "dotnet tool restore && npm run build --silent" : "npm run build --silent") ??
    sh(`mkdir -p '${dirname(out)}' && cp dist/main.js '${out}'`);
  rmSync(src, { recursive: true, force: true });
  if (broke) fail(`${commit}: ${broke} failed`);
  return out;
};
for (const { start } of runs.values()) if (start.bundle) bundleAt(start.bundle);
const started = Date.now();
// A soak that saves a world for a later start is a prelude, never the one watched.
const soak = (args, watched = watch.length > 0 && !args.includes("--save")) =>
  new Promise((done) => {
    const child = spawn("node", [join(HERE, "soak.mjs"), ...args, ...(flags["--fetch"] ? ["--fetch"] : []), ...(watched ? watch : [])], { stdio: watched ? "inherit" : "ignore", env: { ...process.env, FABOT_PLAN_BUILT: "1" } });
    child.on("exit", (code) => done(code));
  });
// `bot`: a path from the repo, or "@bot", the bundle under test.
const opponentsOf = (name, opponents) => {
  if (!opponents) return [];
  const file = join(REPO, "soak-runs", `opponents-${name}.json`);
  mkdirSync(dirname(file), { recursive: true });
  writeFileSync(file, JSON.stringify(opponents.map((o) => ({ ...o, bot: o.bot === "@bot" ? bot : resolve(REPO, o.bot) }))));
  return ["--opponents", file];
};
const play = async ({ name, start, ticks }) => {
  if (start.world && start.declarations) {
    const world = join(REPO, "soak-runs", `world-${name}.json`);
    writeFileSync(world, JSON.stringify({ ...JSON.parse(readFileSync(resolve(REPO, start.world), "utf8")), declarations: start.declarations }));
    start = { ...start, world };
  }
  if (start.world)
    return soak(["--world", resolve(REPO, start.world), "--seed", String(start.seed), "--ticks", String(ticks), "--run", name, "--bot", bot, ...(start.invaders ? ["--invaders", start.invaders] : []), ...(start.wave ? ["--wave", start.wave] : []), ...opponentsOf(name, start.opponents)]);
  const recipe = ["--room", start.room, "--seed", String(start.seed), "--late", String(start.late ?? 0)];
  if (!start.at)
    return soak([
      ...recipe,
      "--ticks",
      String(ticks),
      "--run",
      name,
      "--bot",
      bot,
      ...(start.node ? ["--node", start.node] : []),
      ...(start.invaders ? ["--invaders", start.invaders] : []), ...(start.wave ? ["--wave", start.wave] : []),
      ...(start.core ? ["--core", start.core] : []),
      ...(start.unsafe ? ["--unsafe"] : []),
      ...(start.ended ? ["--ended"] : []),
      ...opponentsOf(name, start.opponents),
    ]);
  const world = worldDir(start);
  if (!existsSync(world)) {
    const began = Date.now();
    const code = await soak([...recipe, "--ticks", String(start.at), "--run", `world-${worldOf(start)}`, "--bot", bundleAt(start.bundle), "--save", world, ...opponentsOf(`world-${name}`, start.opponents && seats(start.opponents))]);
    console.error(`world ${worldOf(start)} played in ${((Date.now() - began) / 1000).toFixed(0)} s, counted in its first soak`);
    if (code !== 0) {
      rmSync(world, { recursive: true, force: true });
      return code;
    }
  }
  return soak(["--room", start.room, "--seed", String(start.seed), "--ticks", String(ticks), "--run", name, "--bot", bot, "--from", world, ...opponentsOf(name, start.opponents)]);
};
// A long soak started last is the whole run's tail; ticks, with a world still to play, stand in for its cost.
const cost = ({ start, ticks }) => ticks + (start.at && !existsSync(worldDir(start)) ? start.at : 0);
const playAll = (list, jobs) => {
  const queue = [...list].sort((a, b) => cost(b) - cost(a));
  return Promise.all(
    Array.from({ length: jobs }, async () => {
      for (let run; (run = queue.shift()); ) {
        const began = Date.now();
        run.code = await play(run);
        const secs = (Date.now() - began) / 1000;
        run.secs += secs;
        console.error(`soak ${run.label} ${secs.toFixed(0)} s (scenarios: ${run.scenarios.join(", ")})`);
      }
    }),
  );
};
console.error(`${scenarios.length} scenarios in ${runs.size} soaks, ${jobs} at a time`);
await playAll(runs.values(), jobs);
// A soak that lost ticks (soak.mjs's exit 3) measured nothing: once more, fewer at a time.
const LOST = 3;
const invalid = [...runs.values()].filter((r) => r.code === LOST);
if (invalid.length) {
  console.error(`${invalid.length} soaks lost ticks; again, ${Math.max(1, jobs >> 1)} at a time`);
  await playAll(invalid, Math.max(1, jobs >> 1));
}

let failed = 0;
// A soak that skipped (soak.mjs's exit 4) measured nothing: a rival's tarball is
// not in the cache, and the run may not reach the registry.
const SKIPPED = 4;
let skipped = 0;
for (const s of scenarios) {
  const run = runs.get(keyOf(s.start));
  const dir = join(REPO, "soak-runs", run.name);
  if (run.code === SKIPPED) {
    skipped++;
    console.log(`SKIP ${s.name}: its rival's tarball is not cached (soak exit ${SKIPPED}); \`npm run scenario -- --fetch\` fetches it`);
    continue;
  }
  if (run.code === LOST) {
    failed++;
    console.log(`INVALID ${s.name}: ticks lost on both tries (a script timeout or abandoned intents); see ${dir}/lost.json`);
    continue;
  }
  if (run.code !== 0) {
    failed++;
    console.log(`FAIL ${s.name}: the soak failed (exit ${run.code}); see ${dir}/server.log`);
    continue;
  }
  const samples = samplesOf(dir);
  for (const a of s.assert) {
    const value = measures[a.measure](samples, a, dir);
    const max = a.max ?? countCeilings[a.measure] ?? Infinity;
    const ok = value <= max && value >= (a.min ?? -Infinity);
    if (!ok) failed++;
    const window = a.from !== undefined || a.to !== undefined ? ` t${a.from ?? 0}-${a.to ?? s.ticks}` : "";
    const bound = [a.min !== undefined && `≥ ${a.min}`, max !== Infinity && `≤ ${max}`].filter(Boolean).join(", ");
    console.log(`${ok ? "pass" : "FAIL"} ${s.name}: ${a.measure}${a.who ? ` of ${a.who}` : ""}${a.room ? ` in ${a.room}` : ""}${window} ${value} (${bound})`);
  }
  const rows = s.assert.some((a) => a.measure === "predictedWinsLost") ? engagementsOf(dir) : [];
  if (rows.length) for (const line of [`${rows.length} engagements`, ...report(rows)]) console.log(`  ${s.name}: ${line}`);
}
const slowest = [...runs.values()].sort((a, b) => b.secs - a.secs).slice(0, 5);
console.log(`slowest: ${slowest.map((r) => `${r.label} ${r.secs.toFixed(0)} s (${r.scenarios.join(", ")})`).join("; ")}`);
console.log(`${failed ? `${failed} failed` : "all passed"}${skipped ? `, ${skipped} skipped` : ""} in ${((Date.now() - started) / 1000).toFixed(0)} s`);
process.exit(failed ? 1 : 0);
