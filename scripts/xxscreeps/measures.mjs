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
const fightLives = (dir) => livesOf(dir).filter((b) => b.ours && b.name.startsWith("fight-") && b.hits !== undefined);
// The first siege's set: the bodies of its ask, the one tick it cast the most
// of them in (its name's first token the tick it was cast, the second its
// engagement) — not a reinforcement, nor a cast left over from an earlier use
// of the id (`bodies.jsonl`).
const siegeSet = (dir) => {
  const first = rowsOf(dir).filter((e) => e.kind === "siege").sort((a, b) => a.opened - b.opened)[0];
  if (!first) return [];
  const of = livesOf(dir).filter((b) => b.ours && b.name.split("-")[2] === String(first.id));
  const castOf = (b) => Number(b.name.split("-")[1]);
  const most = new Map();
  for (const b of of) most.set(castOf(b), (most.get(castOf(b)) ?? 0) + 1);
  const ask = [...most].sort((a, b) => b[1] - a[1] || a[0] - b[0])[0]?.[0];
  return of.filter((b) => castOf(b) === ask);
};
const lastSiege = (dir) => rowsOf(dir).filter((e) => e.kind === "siege").sort((a, b) => a.opened - b.opened).at(-1);
const moves = (body) => [...body].filter((p) => p === "m").length;
const scoutsOf = (dir) => livesOf(dir).filter((b) => b.ours && b.name.startsWith("scout-"));
const walksOf = (dir) => rowsOf(dir).flatMap((e) => (e.blob ? [e.blob] : []));
const foughtOf = (dir) => rowsOf(dir).flatMap((e) => (e.fought ? [e.fought] : []));
const mean = (xs) => xs.reduce((a, b) => a + b, 0) / Math.max(1, xs.length);
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
const decayedIn = (s, a) => (a.room ? (s.rooms[a.room]?.decayed ?? 0) : s.ledger.decayed);
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
  // A body cast before the stock fell is not one cast under it.
  castsBelow: (samples, a, dir) =>
    new Set(
      livesOf(dir)
        .filter((b) => b.ours && b.name.startsWith(`${a.pattern}-`))
        .filter((b) => {
          const at = samples.filter((s) => s.time <= b.first).at(-1);
          return at && at.t >= (a.from ?? 0) && at.stored < a.below;
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
  // Energy lost to pile decay.
  decayed: (samples, a) => decayedIn(at(samples, a.to ?? Infinity), a) - decayedIn(at(samples, a.from ?? 0), a),
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
  // Ticks bodies lost beside a road: grounded off it, or stopped for a body on it.
  roadLoss: (samples, a) => at(samples, a.to ?? Infinity).roadLoss - at(samples, a.from ?? 0).roadLoss,
  stillLoaded: (samples, a) => at(samples, a.to ?? Infinity).stillLoaded,
  // The claims we own, the lowest level one stands at, the first tick every one has a spawn, and whether a safe mode ran in one.
  claimToRcl2: (samples) => firstClaimed(samples, (c) => c.level >= 2) - firstClaimed(samples, (c) => c.level >= 1),
  rcl2ToTower: (samples) => firstClaimed(samples, (c) => c.towers > 0) - firstClaimed(samples, (c) => c.level >= 2),
  claimSafeMode: (samples, a) => (within(samples, a).some((s) => Object.values(s.claimed ?? {}).some((c) => c.safe)) ? 1 : 0),
  claimLevelOf: (samples, a) => at(samples, a.to ?? Infinity).claimed?.[a.room]?.level ?? 0,
  // A home's level at the window's end.
  homeLevel: (samples, a) => { const s = sideOf(a)(at(samples, a.to ?? Infinity)); return s.homes?.[a.room]?.rcl ?? s.rcl; },
  claimsOwned: (samples, a) => Object.values(at(samples, a.to ?? Infinity).claimed ?? {}).filter((c) => c.level > 0).length,
  claimLevel: (samples, a) => Math.min(...Object.values(at(samples, a.to ?? Infinity).claimed ?? {}).map((c) => c.level)),
  claimCharged: (samples) => firstClaimed(samples, (c) => c.fire >= 1000),
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
  // Energy `room` netted a tick after its bodies, the least-squares slope from its first dig.
  netRate: (samples, a) => {
    const worked = within(samples, a).filter((s) => s.rooms[a.room]?.dug > 0);
    if (worked.length < 2) return -Infinity;
    const [mt, mn] = [mean(worked.map((s) => s.t)), mean(worked.map((s) => s.rooms[a.room].net))];
    const slope = worked.reduce((n, s) => n + (s.t - mt) * (s.rooms[a.room].net - mn), 0) / worked.reduce((n, s) => n + (s.t - mt) ** 2, 0);
    return Number(slope.toFixed(1));
  },
  // Percent of the dig that decays, the least-squares slopes of both from the first outpost dig.
  decayShare: (samples, a) => {
    const seen = within(samples, a).filter((s) => Object.values(s.rooms).slice(1).some((r) => r.dug > 0));
    if (seen.length < 2) return Infinity;
    const mt = mean(seen.map((s) => s.t));
    const slope = (y, my = mean(seen.map(y))) => seen.reduce((n, s) => n + (s.t - mt) * (y(s) - my), 0);
    return Math.round((100 * slope((s) => s.ledger.decayed)) / slope((s) => s.ledger.dug));
  },
  // The most outposts dug at once with no container yet, over the samples.
  rising: (samples, a) => Math.max(0, ...within(samples, a).map((s) => Object.entries(s.rooms).slice(1).filter(([, r]) => r.dug > 0 && r.containers === 0).length)),
  // The most ticks `room` went without one of our bodies in it.
  stalest: (_, a, dir) => {
    const lives = livesOf(dir).filter((b) => b.ours);
    const stays = lives.flatMap((b) => (b.entered ?? []).map(([r, t], i, e) => [r, t, e[i + 1]?.[1] ?? b.last + 1]).filter(([r]) => r === a.room));
    let [seen, worst] = [Math.min(...lives.map((b) => b.first)), 0];
    for (const [, t, until] of stays.sort((x, y) => x[1] - y[1])) [worst, seen] = [Math.max(worst, t - seen), Math.max(seen, until)];
    return Math.max(worst, Math.max(...lives.map((b) => b.last)) - seen);
  },
  firstIn: (_, a, dir) => Math.min(...livesOf(dir).filter((b) => b.ours).flatMap((b) => (b.entered ?? []).filter(([r]) => r === a.room).map(([, t]) => t))),
  // Percent of the ticks our `name`-prefixed bodies stood in `room` they did `act`, pooled over
  // the lives that stood longest there: a life's acts are not split by room.
  actShare: (_, a, dir) => {
    const most = (b) => Object.entries(b.rooms).sort((x, y) => y[1] - x[1])[0]?.[0];
    const of = livesOf(dir).filter((b) => b.ours && b.name.startsWith(a.name) && most(b) === a.room);
    const ticks = of.reduce((n, b) => n + b.rooms[a.room], 0);
    return ticks ? Math.round((100 * of.reduce((n, b) => n + (b.acts[a.act] ?? 0), 0)) / ticks) : -Infinity;
  },
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
  pouredBack: (samples, a) => at(samples, a.to ?? Infinity).pouredBack - at(samples, a.from ?? 0).pouredBack,
  // Energy spent on construction (`room`: in that room alone).
  build: (samples, a) => builtIn(at(samples, a.to ?? Infinity), a) - builtIn(at(samples, a.from ?? 0), a),
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
  // Energy a room's spawns put into bodies (`room`: the room).
  bodiesSpent: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.bodies ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.bodies ?? 0),
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
  // Energy dug in `room` a tick, the least-squares slope from the tick a rival first
  // entered it (none: its first dig): a cast order shifted by a hundred ticks moves a fit less than a window.
  digRate: (samples, a, dir) => {
    const raid = Math.min(...livesOf(dir).flatMap((b) => (b.ours ? [] : (b.entered ?? []).filter(([r]) => r === a.room).map(([, t]) => t))));
    const worked = within(samples, a).filter((s) => s.rooms[a.room]?.dug > 0 && (raid === Infinity || s.t >= raid));
    if (worked.length < 2) return -Infinity;
    const [mt, md] = [mean(worked.map((s) => s.t)), mean(worked.map((s) => s.rooms[a.room].dug))];
    const slope = worked.reduce((n, s) => n + (s.t - mt) * (s.rooms[a.room].dug - md), 0) / worked.reduce((n, s) => n + (s.t - mt) ** 2, 0);
    return Number(slope.toFixed(1));
  },
  // The least energy the home's storage and containers held, and the ticks it stood below `below`.
  leastStored: (samples, a) => Math.min(...within(samples, a).map((s) => s.stored)),
  ticksBelow: (samples, a) =>
    a.below === undefined ? Infinity : within(samples, a).reduce((n, s, i, seen) => n + (s.stored < a.below && seen[i + 1] ? seen[i + 1].t - s.t : 0), 0),
  mostStored: (samples, a) => Math.max(...within(samples, a).map((s) => s.stored)),
  // Percent of the colony's harvest the engagements cast into bodies.
  fightShare: (samples, _, dir) => Math.round((100 * rowsOf(dir).reduce((n, e) => n + (e.cast ?? 0), 0)) / Math.max(1, samples.at(-1).ledger.dug)),
  // Energy a side's towers spent: ours, or `who`'s an opponent's.
  towerSpent: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    return a.who ? (last.others[a.who]?.spent ?? 0) - (first.others[a.who]?.spent ?? 0) : last.ledger.tower - first.ledger.tower;
  },
  // The energy `who`'s towers hold at the window's end.
  towerLeft: (samples, a) => at(samples, a.to ?? Infinity).others[a.who]?.fire ?? Infinity,
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
  builtAtEnd: (samples, a) => at(samples, a.to ?? Infinity).built[a.kind] ?? 0,
  sitesAtEnd: (samples, a) => at(samples, a.to ?? Infinity).queued?.[a.kind] ?? 0,
  buriedAtEnd: (samples, a) => at(samples, a.to ?? Infinity).buried ?? Infinity,
  unroadedAtEnd: (samples, a) => at(samples, a.to ?? Infinity).unroaded ?? Infinity,
  overUpgrade: (samples, a) => {
    const CONTROLLER_MAX_UPGRADE_PER_TICK = 15;
    const seen = within(samples, a).filter((s) => s.rcl === 8);
    return Math.round((100 * seen.filter((s) => (s.upgradeWork ?? Infinity) > CONTROLLER_MAX_UPGRADE_PER_TICK).length) / Math.max(1, seen.length));
  },
  oldestPayingSite: (samples, a) => Math.max(0, ...within(samples, a).map((s) => s.oldestPaying ?? 0)),
  // A share: the amount alone tracks how much is upgraded.
  handFedShare: (samples, a) => {
    const [to, from] = [at(samples, a.to ?? Infinity).ledger, at(samples, a.from ?? 0).ledger];
    return (100 * ((to.handFed ?? 0) - (from.handFed ?? 0))) / Math.max(1, to.upgrade - from.upgrade);
  },
  // A bundle that never read `carry.ring` fails, rather than read as none carried.
  ringHauledShare: (_, a, dir) => {
    const seen = windowsOf(dir, a);
    const sum = (k) => seen.reduce((n, w) => n + (w.values[k] ?? 0), 0);
    if (!seen.some((w) => "carry.ring" in w.values)) return Infinity;
    return Math.round((100 * sum("carry.ring")) / Math.max(1, sum("spend.spawn")));
  },
  // Energy lost to link transfers: none means no link ever sent.
  linked: (samples, a) => at(samples, a.to ?? Infinity).ledger.linked - at(samples, a.from ?? 0).ledger.linked,
  // The least any declared outpost netted after its bodies (`rooms`, the
  // colony's own first); none declared: -Infinity.
  poorestOutpost: (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    return Math.min(...Object.keys(last.rooms).slice(1).map((r) => last.rooms[r].net - (first.rooms[r]?.net ?? 0)));
  },
  // The most percent of an outpost's dig that decayed over the sample steps no rival stood
  // in it (none dug: Infinity): haul fallen behind its dig, however long a raider dwells.
  quietRot: (samples, a) => {
    const seen = within(samples, a);
    const rot = (r) => {
      const quiet = seen.slice(1).map((s, i) => [seen[i].rooms[r], s.rooms[r]]).filter(([was, now]) => was && now && was.hostile === now.hostile);
      const dug = quiet.reduce((n, [was, now]) => n + now.dug - was.dug, 0);
      return dug ? (100 * quiet.reduce((n, [was, now]) => n + now.decayed - was.decayed, 0)) / dug : Infinity;
    };
    return Number(Math.max(...Object.keys(samples.at(-1).rooms).slice(1).map(rot)).toFixed(1));
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
  // Acts the engine refused (the soak's refusals.json), but an observer's
  // look at a room xxscreeps' map does not hold (-10), which no live map refuses.
  refusals: (_, __, dir) =>
    Object.entries(JSON.parse(readFileSync(join(dir, "refusals.json"), "utf8")))
      .filter(([act]) => act !== "observeRoom -10")
      .reduce((n, [, c]) => n + c, 0),
  // Raids an answered engagement saw die without disengaging.
  raidsKilled: (_, __, dir) => engagementsOf(dir).filter(({ a }) => a.won === true).length,
  // Over the sets of two or more that walked to their raid (soak-step's
  // `blob`), the worst: the percent of approaching ticks every member stood
  // within ⌊n/2⌋ + 1 of the median member (none walked: -Infinity), the tiles
  // a tick the set stood past that, the ticks a member stood still free of
  // fatigue (none walked: Infinity), and the median member's ticks from the
  // muster to within 3 of the raid (none: Infinity). A spread and a stall are
  // read as amounts, so one tile or one tick more is a small change, not a flip.
  blobAnchorRange: (_, __, dir) => Math.min(...walksOf(dir).map((w) => (100 * w.near) / w.ticks), ...(walksOf(dir).length ? [] : [-Infinity])),
  blobSpread: (_, __, dir) => Math.max(...walksOf(dir).map((w) => w.over / w.ticks), ...(walksOf(dir).length ? [] : [Infinity])),
  blobStallTicks: (_, __, dir) => Math.max(...walksOf(dir).map((w) => w.stalls / w.members), ...(walksOf(dir).length ? [] : [Infinity])),
  blobArrival: (_, __, dir) => Math.max(...walksOf(dir).map((w) => w.arrival ?? Infinity), ...(walksOf(dir).length ? [] : [Infinity])),
  // Over the sets of two or more that fought (soak-step's `fought`), the
  // worst: the most hits one member lost over the mean of its set's, its
  // lowest share of its hits in percent, and the percent of member-ticks
  // under half hits stood out of the raid's reach (none: 100); and every
  // engagement's bodies lost.
  fightDamageSpread: (_, __, dir) => Math.max(1, ...foughtOf(dir).map((f) => Math.max(...Object.values(f.taken)) / Math.max(1, mean(Object.values(f.taken))))),
  fightLowestHits: (_, __, dir) => Math.min(100, ...foughtOf(dir).flatMap((f) => Object.values(f.low).map((l) => 100 * l))),
  fightWoundedOut: (_, __, dir) => Math.min(100, ...foughtOf(dir).filter((f) => f.wounded).map((f) => (100 * f.out) / f.wounded)),
  // Over a siege's dismantler-ticks in its fighting phase, the percent with a
  // body of its set that heals within 1 tile of it (none seen: 100).
  dismantlerRing: (_, __, dir) => {
    const seen = rowsOf(dir).filter((e) => e.kind === "siege" && e.ring);
    const ticks = seen.reduce((n, e) => n + e.ring.ticks, 0);
    return ticks ? Math.round((100 * seen.reduce((n, e) => n + e.ring.covered, 0)) / ticks) : 100;
  },
  // The most percent of a member's ticks it swung back (soak-step's `swung`),
  // over the engagements of `kind` (none named: every one); none seen: 0.
  oscillation: (_, a, dir) =>
    Math.max(0, ...rowsOf(dir).filter((e) => !a.kind || e.kind === a.kind).flatMap((e) => Object.values(e.swung).map((s) => Math.round((100 * s.swings) / Math.max(1, s.ticks))))),
  // The most ticks a siege took from its muster to its breach (one that never breached: Infinity),
  // so its cast's wait on the economy's spawns is not read.
  breachTicks: (_, __, dir) => Math.max(...rowsOf(dir).filter((e) => e.kind === "siege").map((e) => (e.mustered === undefined ? Infinity : (e.breached ?? (e.disengaged || e.closed === null ? Infinity : e.closed)) - e.mustered)), ...(rowsOf(dir).some((e) => e.kind === "siege") ? [] : [Infinity])),
  // The rival's structures the last siege opened left standing in its room (none opened: Infinity),
  // and the ticks from its fight's start until it first stood none (never: Infinity), which a rebuild after does not move.
  razeLeft: (_, __, dir) => lastSiege(dir)?.left ?? Infinity,
  razeTicks: (_, __, dir) => (lastSiege(dir)?.bare ?? Infinity) - (lastSiege(dir)?.fighting ?? 0),
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
  // Its bodies that landed in a region the target is walled off from, but not a
  // body that left the room the tick after landing on a border tile: those are a
  // drain pair's excursions (soak-step's `entered`).
  siegeWalledEntries: (_, __, dir) => Object.values(rowsOf(dir).find((e) => e.kind === "siege")?.entries ?? {}).filter((s) => s === "walled").length,
  // The least hits a siege's target (a spawn, a core) ever stood at (none seen: Infinity).
  siegeTargetLow: (_, __, dir) => Math.min(Infinity, ...rowsOf(dir).filter((e) => e.kind === "siege" && e.target).map((e) => e.target.low)),
  // Over the first siege's set, the ticks between its first and last body's
  // spawn starting (none: Infinity).
  siegeSpawnSpread: (_, __, dir) => {
    const firsts = siegeSet(dir).map((b) => b.first);
    return firsts.length ? Math.max(...firsts) - Math.min(...firsts) : Infinity;
  },
  // The oldest body of that set's life when the last of it stands: a spawn runs
  // `SPAWN_TICKS_PER_PART` a part (none: -Infinity).
  siegeOldestLife: (_, __, dir) => {
    const set = siegeSet(dir);
    if (!set.length) return -Infinity;
    const stands = Math.max(...set.map((b) => b.first + 3 * b.body.length));
    return 1500 - (stands - Math.min(...set.map((b) => b.first)));
  },
  // Over the fight asks from `from` (a body's name carries its tick and engagement;
  // a name re-asked after its body died is its first), the most ticks from the
  // ask to its last body standing past that ask's own spawn time (none: Infinity).
  askLag: (samples, a, dir) => {
    const start = samples[0].time - samples[0].t;
    const first = [...Map.groupBy(livesOf(dir), (b) => b.name).values()].map((v) => v[0]);
    const asks = Map.groupBy(
      first.filter((b) => b.ours && /^fight-\d+-\d+-/.test(b.name)),
      (b) => Number(b.name.split("-")[1]) - start + "-" + b.name.split("-")[2],
    );
    const lags = [...asks]
      .filter(([key]) => parseInt(key) >= (a.from ?? 0))
      .map(([key, set]) => Math.max(...set.map((b) => b.first + 3 * b.body.length)) - start - parseInt(key) - 3 * set.reduce((n, b) => n + b.body.length, 0));
    return lags.length ? Math.max(...lags) : Infinity;
  },
  // Over the sweeps, the rival's bodies killed and harvest denied (soak-step's `killed`, `denied`), and those less what they cast.
  sweepKilled: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "sweep").reduce((n, e) => n + (e.killed ?? 0), 0),
  sweepDenied: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "sweep").reduce((n, e) => n + (e.denied ?? 0), 0),
  sweepNet: (_, __, dir) => rowsOf(dir).filter((e) => e.kind === "sweep").reduce((n, e) => n + (e.killed ?? 0) + (e.denied ?? 0) - e.cast, 0),
  // Percent of the steps a prey took from inside a sweep body's reach that ended still in it (soak-step's `held`/`kept`).
  sweepKept: (_, __, dir) => {
    const rows = rowsOf(dir).filter((e) => e.kind === "sweep");
    return Math.round((100 * rows.reduce((n, e) => n + (e.kept ?? 0), 0)) / Math.max(1, rows.reduce((n, e) => n + (e.held ?? 0), 0)));
  },
  bodiesLost: (_, __, dir) => rowsOf(dir).reduce((n, e) => n + (e.dead ?? 0), 0),
  // The energy every engagement cast into its bodies.
  cast: (_, __, dir) => rowsOf(dir).reduce((n, e) => n + (e.cast ?? 0), 0),
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
  // The damage struck on our bodies that swing nothing, a harvester or a hauler, each time one
  // came into a rival's reach (soak-step's `reached`): how long a body stays under fire, which
  // the raid's wait on our spawn moves less than it moves the damage.
  unarmedStruckPerReach: (_, __, dir) => {
    const unarmed = livesOf(dir).filter((b) => b.ours && !/[ar]/.test(b.body ?? ""));
    const [struck, reached] = ["struck", "reached"].map((k) => unarmed.reduce((n, b) => n + (b[k] ?? 0), 0));
    return reached ? Math.round(struck / reached) : struck ? Infinity : 0;
  },
  // Ticks an enemy stood in `room`.
  hostileTicks: (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.hostile ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.hostile ?? 0),
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
