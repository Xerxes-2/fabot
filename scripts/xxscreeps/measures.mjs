// What a soak measured, read off its output directory: `scenario.mjs` bounds
// these per scenario, and `npm run soak` and soak:many hold `invariants`.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { bugs, engagement } from "./prediction.mjs";

// Ticks are counted from the bot's first, so a window means the same on every start.
// Each engagement the run priced, as `prediction.mjs` reads a row; one never
// priced predicted nothing.
const rowsOf = (dir) =>
  readFileSync(join(dir, "engagements.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
export const engagementsOf = (dir) =>
  rowsOf(dir)
    .filter((e) => e.predicted)
    .map((e) => ({ p: e.predicted, a: engagement(e) }));
const livesOf = (dir) =>
  readFileSync(join(dir, "bodies.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
const scoutsOf = (dir) => livesOf(dir).filter((b) => b.ours && b.name.startsWith("scout-"));
export const samplesOf = (dir) => readFileSync(join(dir, "samples.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
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
const windowsOf = (dir, a) =>
  readFileSync(join(dir, "metrics.jsonl"), "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l))
    .filter((w) => w.t >= (a.from ?? 0) && w.t < (a.to ?? Infinity));
// A spend category the ledger moved less than this in a run is too small to
// read a percent off.
const LEDGER_LEAST = 1000;
// A body seen this soon after the tick in its name was spawned in the run,
// not stood in the world it started from.
const SEEN_AFTER_CAST = 2;
// The engine's CREEP_SPAWN_TIME.
const SPAWN_TIME_PER_PART = 3;
// `AGED` in `core/observe/metrics.rs`: fewer ticks to live, a body died of age.
const AGED = 5;
const firstClaimed = (samples, met) => samples.find((s) => Object.keys(s.claimed ?? {}).length && Object.values(s.claimed).every(met))?.t ?? Infinity;
export const measures = {
  metric: (_, a, dir) => {
    const values = windowsOf(dir, a).filter((w) => w.ticks === 100 && w.row === (a.room ?? "*")).map((w) => w.values[a.metric] ?? 0);
    if (!values.length) return a.min !== undefined ? -Infinity : Infinity;
    return a.min !== undefined ? Math.min(...values) : Math.max(...values);
  },
  // A body cast before the stock fell is not one cast under it, nor one cast
  // between two samples while the stock climbed past the line.
  castsBelow: (samples, a, dir) =>
    new Set(
      livesOf(dir)
        .filter((b) => b.ours && b.name.startsWith(`${a.pattern}-`))
        .filter((b) => {
          const at = samples.findLastIndex((s) => s.time <= b.first);
          const [before, after] = [samples[at], samples[at + 1] ?? samples[at]];
          return before && before.t >= (a.from ?? 0) && before.stored < a.below && after.stored < a.below;
        })
        .map((b) => b.name),
    ).size,
  // The most percent the bot's spend split (`spend.*`, its metrics) is off the
  // engine's ledger by category, over the rooms the ledger follows, between
  // the first and the last full window read with the ledger beside it.
  spendError: (samples, a, dir) => {
    const rooms = new Set(Object.keys(samples.at(-1).rooms));
    const seen = windowsOf(dir, a);
    const marks = seen.filter((w) => w.ledger && w.ticks === 100);
    if (marks.length < 2) return Infinity;
    const [first, last] = [marks[0], marks.at(-1)];
    const inside = seen.filter((w) => rooms.has(w.row) && w.start > first.start && w.start <= last.start);
    const errors = ["build", "repair", "upgrade", "spawn", "tower"]
      .map((k) => [inside.reduce((n, w) => n + (w.values[`spend.${k}`] ?? 0), 0), last.ledger[k] - first.ledger[k]])
      .filter(([, ledger]) => ledger >= LEDGER_LEAST)
      .map(([bot, ledger]) => Math.abs((100 * (bot - ledger)) / ledger));
    return errors.length ? Number(Math.max(...errors).toFixed(1)) : Infinity;
  },
  // The bot's deaths (`death.*`) against the engine's, over the bot's full
  // windows: a body is counted dead the tick after it was last seen, and
  // `bodies.jsonl` also closes a body that only walked out of its rooms, so
  // one that comes back, or was never struck, did not die.
  deathsError: (_, a, dir) => {
    const full = windowsOf(dir, a).filter((w) => w.ticks === 100);
    if (!full.length) return Infinity;
    const [lo, hi] = [Math.min(...full.map((w) => w.start)), Math.max(...full.map((w) => w.start)) + 100];
    const bot = full.reduce((n, w) => n + Object.entries(w.values).reduce((m, [k, v]) => m + (k.startsWith("death.") ? v : 0), 0), 0);
    const lives = livesOf(dir);
    const back = (b) => lives.some((c) => c.name === b.name && c.first > b.last);
    const died = lives.filter((b) => b.ours && b.struck && b.ttl > AGED && b.last + 1 >= lo && b.last + 1 < hi && !back(b));
    return Math.abs(bot - died.length);
  },
  // Roads and containers lost between samples, over every room the soak follows.
  decayedAway: (samples, a) => {
    const standing = (s) => Object.values(s.rooms).reduce((n, r) => n + r.roads + r.containers, 0);
    const seen = within(samples, a);
    return seen.slice(1).reduce((n, s, i) => n + Math.max(0, standing(seen[i]) - standing(s)), 0);
  },
  // From the peak, so a road that decays and is rebuilt is not lost.
  roadsLost: (samples, a) => {
    const seen = within(samples, a).map((s) => s.rooms[a.room]?.roads ?? 0);
    return Math.max(0, ...seen) - (seen.at(-1) ?? 0);
  },
  stillLoaded: (samples, a) => at(samples, a.to ?? Infinity).stillLoaded,
  // The ticks from every claim at RCL2 to every one with a tower, whether a safe mode ran in one, and `room`'s level.
  rcl2ToTower: (samples) => firstClaimed(samples, (c) => c.towers > 0) - firstClaimed(samples, (c) => c.level >= 2),
  claimSafeMode: (samples, a) => (within(samples, a).some((s) => Object.values(s.claimed ?? {}).some((c) => c.safe)) ? 1 : 0),
  claimLevelOf: (samples, a) => at(samples, a.to ?? Infinity).claimed?.[a.room]?.level ?? 0,
  // A home's level at the window's end.
  homeLevel: (samples, a) => { const s = sideOf(a)(at(samples, a.to ?? Infinity)); return s.homes?.[a.room]?.rcl ?? s.rcl; },
  claimsOwned: (samples, a) => Object.values(at(samples, a.to ?? Infinity).claimed ?? {}).filter((c) => c.level > 0).length,
  // The most ticks a body of ours stood in its spawn past its spawn time: the
  // engine holds it there while every tile beside the spawn is taken.
  spawnStuck: (_, __, dir) =>
    Math.max(
      0,
      ...livesOf(dir)
        .filter((b) => b.ours && b.first <= Number(b.name.split("-")[1]) + SEEN_AFTER_CAST)
        .map((b) => (b.left ?? b.last + 1) - b.first - SPAWN_TIME_PER_PART * b.body.length),
    ),
  // Scouts cast, the most ticks one stood still (a scout never idles), and
  // the ticks scouts stood in `room`.
  scoutsCast: (_, __, dir) => new Set(scoutsOf(dir).map((b) => b.name)).size,
  scoutStill: (_, __, dir) => Math.max(0, ...scoutsOf(dir).map((b) => b.still ?? 0)),
  scoutTicksIn: (_, a, dir) => scoutsOf(dir).reduce((n, b) => n + (b.rooms[a.room] ?? 0), 0),
  // The most outposts dug at once with no container yet, over the samples.
  rising: (samples, a) => Math.max(0, ...within(samples, a).map((s) => Object.entries(s.rooms).slice(1).filter(([, r]) => r.dug > 0 && r.containers === 0).length)),
  reserverOnPost: (_, __, dir) => {
    const lives = livesOf(dir).filter((b) => b.ours);
    const posts = new Set(lives.flatMap((b) => b.tiles?.harvest ?? []));
    return new Set(lives.flatMap((b) => b.tiles?.reserveController ?? []).filter((t) => posts.has(t))).size;
  },
  // Our `name`-prefixed bodies cast in the run from a spawn in `room`.
  castIn: (samples, a, dir) =>
    new Set(livesOf(dir).filter((b) => b.ours && b.name.startsWith(a.name) && b.first > samples[0].time - samples[0].t && b.entered?.[0]?.[0] === a.room).map((b) => b.name)).size,
  claimersCast: (samples, a) => new Set(within(samples, a).flatMap((s) => s.bodies.filter((b) => b.startsWith("claimer-")).map((b) => b.split("@")[0]))).size,
  claimSpawned: (samples) => firstClaimed(samples, (c) => c.spawns > 0),
  // The most of a compound (`resource`) stood at once.
  made: (samples, a) => Math.max(0, ...samples.map((s) => s.made?.[a.resource] ?? 0)),
  // Our bodies' boosted parts at once, the most over the run.
  boosted: (samples) => Math.max(0, ...samples.map((s) => s.boosted ?? 0)),
  // What `room`'s storage, terminal, labs, factory and power spawn hold at the window's end, and the non-energy of it.
  heldAtEnd: (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.held ?? Infinity,
  oresAtEnd: (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.ores ?? 0,
  ore: (samples, a) => at(samples, a.to ?? Infinity).ore - at(samples, a.from ?? 0).ore,
  bounced: (samples, a) => at(samples, a.to ?? Infinity).bounced - at(samples, a.from ?? 0).bounced,
  sentTo: (samples, a) => (at(samples, a.to ?? Infinity).sent?.[a.room] ?? 0) - (at(samples, a.from ?? 0).sent?.[a.room] ?? 0),
  // Energy spent on construction (`room`: in that room alone).
  build: (samples, a) => builtIn(at(samples, a.to ?? Infinity), a) - builtIn(at(samples, a.from ?? 0), a),
  upgradeWhileQueued: (samples, a) => {
    const seen = within(samples, a);
    return seen.slice(1).reduce((n, s, i) => n + (seen[i].sites > 0 ? upgradedIn(s, a) - upgradedIn(seen[i], a) : 0), 0);
  },
  // The longest stretch nothing was upgraded once the storage stood; no storage reads as never.
  upgradeStall: (samples, a) => {
    const seen = within(samples, a).filter((s) => s.built.storage);
    let most = seen.length ? 0 : Infinity;
    for (let i = 1, from = seen[0]?.t; i < seen.length; i++) {
      if (upgradedIn(seen[i], a) > upgradedIn(seen[i - 1], a)) from = seen[i].t;
      most = Math.max(most, seen[i].t - from);
    }
    return most;
  },
  // The fewest ticks `room`'s controller stood from its downgrade over the run, never ours: -Infinity.
  leastDowngrade: (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.leastDowngrade ?? -Infinity,
  // Energy our bodies carried out of a home across its border.
  carriedOut: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.carriedOut ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.carriedOut ?? 0),
  // Energy dug in every room but the colony's own.
  outpostDug: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    return Object.keys(last.rooms).slice(1).reduce((n, r) => n + last.rooms[r].dug - (first.rooms[r]?.dug ?? 0), 0);
  },
  dug: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.dug ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.dug ?? 0),
  upgrade: (samples, a) => upgradedIn(at(samples, a.to ?? Infinity), a) - upgradedIn(at(samples, a.from ?? 0), a),
  // (none ever: Infinity)
  extensionsBeforeContainer: (samples) => {
    const first = samples.find((s) => s.sourceContainers > 0);
    return first ? (first.built.extension ?? 0) : Infinity;
  },
  sourceContainersAtEnd: (samples, a) => at(samples, a.to ?? Infinity).sourceContainers,
  // Links standing beside a source's container.
  postedLinksAtEnd: (samples, a) => at(samples, a.to ?? Infinity).postedLinks,
  builtAtEnd: (samples, a) => at(samples, a.to ?? Infinity).built[a.kind] ?? 0,
  sitesAtEnd: (samples, a) => at(samples, a.to ?? Infinity).queued?.[a.kind] ?? 0,
  buriedAtEnd: (samples, a) => at(samples, a.to ?? Infinity).buried ?? Infinity,
  unroadedAtEnd: (samples, a) => at(samples, a.to ?? Infinity).unroaded ?? Infinity,
  oldestPayingSite: (samples, a) => Math.max(0, ...within(samples, a).map((s) => s.oldestPaying ?? 0)),
  // Energy lost to link transfers: none means no link ever sent.
  linked: (samples, a) => at(samples, a.to ?? Infinity).ledger.linked - at(samples, a.from ?? 0).ledger.linked,
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
  // Acts the engine refused (the soak's refusals.json), but an observer's
  // look at a room xxscreeps' map does not hold (-10), which no live map refuses.
  refusals: (_, __, dir) =>
    Object.entries(JSON.parse(readFileSync(join(dir, "refusals.json"), "utf8")))
      .filter(([act]) => act !== "observeRoom -10")
      .reduce((n, [, c]) => n + c, 0),
  // The most percent of a member's ticks it swung back (soak-step's `swung`),
  // over the engagements of `kind` (none named: every one); none seen: 0.
  oscillation: (_, a, dir) =>
    Math.max(0, ...rowsOf(dir).filter((e) => !a.kind || e.kind === a.kind).flatMap((e) => Object.values(e.swung).map((s) => Math.round((100 * s.swings) / Math.max(1, s.ticks))))),
  bodiesLost: (_, __, dir) => rowsOf(dir).reduce((n, e) => n + (e.dead ?? 0), 0),
  // Our bodies gone from the colony's rooms and their neighbours with life left: died, or walked
  // through a room the soak does not follow.
  bodiesOut: (_, __, dir) => {
    const lives = livesOf(dir);
    const end = Math.max(...lives.map((b) => b.last));
    return new Set(lives.filter((b) => b.ours && b.last < end && b.ttl > 1).map((b) => b.name)).size;
  },
  bodiesStruck: (_, __, dir) => livesOf(dir).reduce((n, b) => n + (b.ours ? (b.struck ?? 0) : 0), 0),
  // The damage struck on the body called `name`.
  struckOn: (_, a, dir) => livesOf(dir).reduce((n, b) => n + (b.name === a.name ? (b.struck ?? 0) : 0), 0),
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
  creepsLostBeforeSafeMode: (samples, a) => {
    const seen = within(samples, a);
    const safe = seen.findIndex((s) => s.safeMode);
    return seen[0].creeps - Math.min(...seen.slice(0, safe < 0 ? seen.length : safe + 1).map((s) => s.creeps));
  },
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

// An act the engine refuses is a decision made on a wrong reading of the
// world, and a fault a module that threw.
const ALWAYS = [
  { measure: "refusals", max: 0 },
  { measure: "faults", max: 0 },
];
export const withAlways = (asserts) => [...asserts, ...ALWAYS.filter((d) => !asserts.some((a) => a.measure === d.measure))];
export const check = (samples, a, dir, max = a.max ?? Infinity) => {
  const value = measures[a.measure](samples, a, dir);
  const bound = [a.min !== undefined && `≥ ${a.min}`, max !== Infinity && `≤ ${max}`].filter(Boolean).join(", ");
  return { value, ok: value <= max && value >= (a.min ?? -Infinity), bound };
};

// main's longest wait from RCL4 to its third rampart over soak:many's starts:
// 5,900 ticks, on E1's node, which starts past RCL4 with no ring.
const RING_OWED_AFTER = 6000;
export const invariants = (dir) => {
  const samples = samplesOf(dir);
  const last = samples.at(-1);
  const rooms = Object.keys(last.rooms);
  const homes = Object.keys(last.homes ?? { [rooms[0]]: last });
  return [
    ...rooms.map((room) => [room, samples, { measure: "roadsLost", room, max: 0 }]),
    ...homes.flatMap((home) => {
      const of = samples.map((s) => s.homes?.[home] ?? s);
      const rcl4 = of.find((s) => s.rcl >= 4)?.t ?? Infinity;
      return [
        { measure: "levelsLost", max: 0 },
        ...(last.t - rcl4 >= RING_OWED_AFTER ? [{ measure: "builtAtEnd", kind: "rampart", min: 3 }] : []),
        { measure: "oldestPayingSite", max: 10000 },
      ].map((a) => [home, of, a]);
    }),
    ...ALWAYS.map((a) => [rooms[0], samples, a]),
  ].flatMap(([where, of, a]) => {
    const { value, ok, bound } = check(of, a, dir);
    return ok ? [] : [`${where}: ${a.measure}${a.kind ? ` ${a.kind}` : ""} ${value} (${bound})`];
  });
};
