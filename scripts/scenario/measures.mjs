// What a soak measured, read off its output directory: `scenario.mjs` bounds
// these per scenario, and `npm run soak` and soak:many hold `invariants`.
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { bugs, outcome, tested } from "../sim/prediction.mjs";

// Ticks are counted from the bot's first, so a window means the same on every start.
const linesOf = (path) =>
  readFileSync(path, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
const rowsOf = (dir) => linesOf(join(dir, "engagements.jsonl"));
export const engagementsOf = (dir) => tested(rowsOf(dir));
/** Every room the run's samples and engagements name: one an assert names outside these reads nothing. */
export const roomsOf = (dir) =>
  new Set(["samples.jsonl", "engagements.jsonl"].filter((f) => existsSync(join(dir, f))).flatMap((f) => [...readFileSync(join(dir, f), "utf8").matchAll(/"([EW]\d+[NS]\d+)"/g)].map((m) => m[1])));
// The bot's own rows (segment 4, sim's `segment4.jsonl`) of `room` (any, unnamed).
const liveOf = (dir, a) => tested(linesOf(join(dir, "segment4.jsonl"))).filter((r) => !a.room || r.room === a.room);
const classed = (r) => r && `${r.p.kind}→${outcome(r.a)}`;
const livesOf = (dir) => linesOf(join(dir, "bodies.jsonl"));
// Each named body's cost once, however many times it left and came back.
const costOf = (lives) => [...new Map(lives.map((b) => [b.name, b.cost])).values()].reduce((n, c) => n + c, 0);
// A body's game tick less this, the run's tick.
const offsetOf = (samples) => samples[0].time - samples[0].t;
const defendsIn = (dir, a) => rowsOf(dir).filter((e) => e.kind === "defend" && openedIn(a)(e));
const drawnBy = (dir, a) => new Set(defendsIn(dir, a).flatMap((e) => e.drew));
// The ticks our bodies that dig or carry were killed in `room` from `from` on, soonest first.
const culls = (samples, a, dir) => {
  const lives = livesOf(dir);
  const end = Math.max(...lives.map((b) => b.last));
  const offset = offsetOf(samples);
  return lives
    .filter((b) => b.ours && b.at.startsWith(`${a.room} `) && b.last < end && b.ttl > AGED && b.last - b.struckAt <= STRUCK_LATELY)
    .filter((b) => /^[wcm]+$/.test(b.body) && /[wc]/.test(b.body))
    .map((b) => b.last - offset)
    .filter((t) => t >= (a.from ?? 0))
    .sort((x, y) => x - y);
};
const scoutsOf = (dir) => livesOf(dir).filter((b) => b.ours && b.name.startsWith("scout-"));
export const samplesOf = (dir) => readFileSync(join(dir, "samples.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
// soak:many's columns beside worth, off a run's metrics and its last sample:
// the worth is not to be read once the ledger is open past a percent of what was dug.
export const exposureOf = (dir, last) => {
  const path = join(dir, "metrics.jsonl");
  const lines = existsSync(path) ? readFileSync(path, "utf8").split("\n").filter(Boolean) : [];
  const sum = (named) =>
    lines.reduce((a, l) => a + Object.entries(JSON.parse(l).values).reduce((b, [n, v]) => b + (named(n) ? v : 0), 0), 0);
  const ledger = last?.ledger;
  return {
    exposure: sum((n) => n === "exposure"),
    lost: sum((n) => n.startsWith("loss.")),
    held: sum((n) => n === "held"),
    open: Boolean(ledger && ledger.dug > 0 && Math.abs(ledger.residual) > ledger.dug / 100),
  };
};
// `ARCHITECTURE.md` §9.3.
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
// A cold tick (the first, or one after a reset) floods every table afresh, so
// it is bounded per colony, apart from the warm ticks.
const warm = (rows) => rows.filter((c) => c.cold === undefined);
const floodsOf = (c) => (c.atlasMisses ?? 0) + (c.atlasLive ?? 0);
const outposts = (key) => (samples, a) => {
  const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
  return Object.keys(last.rooms).slice(1).reduce((n, r) => n + last.rooms[r][key] - (first.rooms[r]?.[key] ?? 0), 0);
};
const at = (samples, t) => samples.find((s) => s.t >= t) ?? samples.at(-1);
const kindIn = (a) => (e) => (!a.kind || e.kind === a.kind) && openedIn(a)(e);
const openedIn = (a) => (e) => (!a.room || e.room === a.room) && e.opened >= (a.from ?? 0) && e.opened < (a.to ?? Infinity);
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
const CREEP_LIFE_TIME = 1500;
// `STRUCK_LATELY` there: struck within these ticks of its death, a body was killed.
const STRUCK_LATELY = 5;
// `MENDER_WORK` in `core/economy/slots.rs`.
const MENDER_WORK = 2;
const firstClaimed = (samples, met) => samples.find((s) => Object.keys(s.claimed ?? {}).length && Object.values(s.claimed).every(met))?.t ?? Infinity;
// A refused "act code" that counts: not an observer's look past a cut world's
// edge (-10), a room the live map always holds.
export const counted = (act) => act !== "observeRoom -10";
// A measure reading `keys` of its assert beside "measure", "min", "max" and "seat": `scenario.mjs` rejects any other.
const on = (keys, f) => Object.assign(f, { reads: keys.split(" ") });
export const measures = {
  metric: on("metric room total from to", (_, a, dir) => {
    const values = windowsOf(dir, a).filter((w) => w.ticks === 100 && w.row === (a.room ?? "*")).map((w) => w.values[a.metric] ?? 0);
    if (!values.length) return a.min !== undefined ? -Infinity : Infinity;
    if (a.total) return values.reduce((n, v) => n + v, 0);
    return a.min !== undefined ? Math.min(...values) : Math.max(...values);
  }),
  // The most percent the bot's spend split (`spend.*`, its metrics) is off the
  // engine's ledger by category, over the rooms the ledger follows, between
  // the first and the last full window read with the ledger beside it.
  spendError: on("from to", (samples, a, dir) => {
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
  }),
  // The bot's deaths (`death.*`) against the engine's, over the bot's full
  // windows: a body is counted dead the tick after it was last seen, and
  // `bodies.jsonl` also closes a body that only walked out of its rooms, so
  // one that comes back, or was never struck, did not die.
  deathsError: on("from to", (_, a, dir) => {
    const full = windowsOf(dir, a).filter((w) => w.ticks === 100);
    if (!full.length) return Infinity;
    const [lo, hi] = [Math.min(...full.map((w) => w.start)), Math.max(...full.map((w) => w.start)) + 100];
    const bot = full.reduce((n, w) => n + Object.entries(w.values).reduce((m, [k, v]) => m + (k.startsWith("death.") ? v : 0), 0), 0);
    const lives = livesOf(dir);
    const back = (b) => lives.some((c) => c.name === b.name && c.first > b.last);
    const died = lives.filter((b) => b.ours && b.struck && b.ttl > AGED && b.last + 1 >= lo && b.last + 1 < hi && !back(b));
    return Math.abs(bot - died.length);
  }),
  // Roads and containers lost between samples, over every room the soak follows.
  decayedAway: on("from to", (samples, a) => {
    const standing = (s) => Object.values(s.rooms).reduce((n, r) => n + r.roads + r.containers, 0);
    const seen = within(samples, a);
    return seen.slice(1).reduce((n, s, i) => n + Math.max(0, standing(seen[i]) - standing(s)), 0);
  }),
  // A road rebuilt within a creep lifetime, or being rebuilt at the end, is not lost (owner).
  roadsLost: on("room from to", (samples, a) => {
    const seen = within(samples, a).map((s) => [s.t, s.rooms[a.room]?.roads ?? 0]);
    const end = seen.at(-1)?.[0] ?? 0;
    let [peak, lost] = [0, 0];
    for (const [t, roads] of seen) {
      peak = Math.max(peak, roads);
      if (t + CREEP_LIFE_TIME > end) break;
      const after = seen.filter(([u]) => u >= t && u <= t + CREEP_LIFE_TIME).map(([, r]) => r);
      lost = Math.max(lost, peak - Math.max(...after));
    }
    return lost;
  }),
  roadsAtEnd: on("room to", (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.roads ?? 0),
  stillLoaded: on("to", (samples, a) => at(samples, a.to ?? Infinity).stillLoaded),
  mineralAside: on("to", (samples, a) => at(samples, a.to ?? Infinity).mineralAside),
  mineralHeld: on("to", (samples, a) => at(samples, a.to ?? Infinity).mineralHeld),
  // The ticks from every claim at RCL2 to every one with a tower, whether a safe mode ran in one, and `room`'s level.
  rcl2ToTower: (samples) => firstClaimed(samples, (c) => c.towers > 0) - firstClaimed(samples, (c) => c.level >= 2),
  claimSafeMode: on("from to", (samples, a) => (within(samples, a).some((s) => Object.values(s.claimed ?? {}).some((c) => c.safe)) ? 1 : 0)),
  claimLevelOf: on("room to", (samples, a) => at(samples, a.to ?? Infinity).claimed?.[a.room]?.level ?? 0),
  // A home's level at the window's end.
  homeLevel: on("room to", (samples, a) => { const s = at(samples, a.to ?? Infinity); return s.homes?.[a.room]?.rcl ?? s.rcl; }),
  claimsOwned: on("to", (samples, a) => Object.values(at(samples, a.to ?? Infinity).claimed ?? {}).filter((c) => c.level > 0).length),
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
  scoutTicksIn: on("room", (_, a, dir) => scoutsOf(dir).reduce((n, b) => n + (b.rooms[a.room] ?? 0), 0)),
  // The most outposts dug at once with no container yet, over the samples.
  rising: on("from to", (samples, a) => Math.max(0, ...within(samples, a).map((s) => Object.entries(s.rooms).slice(1).filter(([, r]) => r.dug > 0 && r.containers === 0).length))),
  reserverOnPost: (_, __, dir) => {
    const lives = livesOf(dir).filter((b) => b.ours);
    const posts = new Set(lives.flatMap((b) => b.tiles?.harvest ?? []));
    return new Set(lives.flatMap((b) => b.tiles?.reserveController ?? []).filter((t) => posts.has(t))).size;
  },
  // Our `name`-prefixed bodies cast in the run from a spawn in `room`.
  castIn: on("room name", (samples, a, dir) =>
    new Set(livesOf(dir).filter((b) => b.ours && b.name.startsWith(a.name) && b.first > offsetOf(samples) && b.entered?.[0]?.[0] === a.room).map((b) => b.name)).size),
  claimersCast: on("from to", (samples, a) => new Set(within(samples, a).flatMap((s) => s.bodies.filter((b) => b.startsWith("claimer-")).map((b) => b.split("@")[0]))).size),
  claimSpawned: (samples) => firstClaimed(samples, (c) => c.spawns > 0),
  produced: on("room resource", (samples, a) => Math.max(0, ...samples.map((s) => Object.entries(s.rooms ?? {}).reduce((n, [r, x]) => n + (!a.room || r === a.room ? (x.produced?.[a.resource] ?? 0) : 0), 0)))),
  // The most of a compound (`resource`) stood at once, in `room` if one is named.
  made: on("room resource", (samples, a) => Math.max(0, ...samples.map((s) => (a.room ? s.rooms?.[a.room]?.made : s.made)?.[a.resource] ?? 0))),
  // Our bodies' boosted parts at once, the most over the run.
  boosted: (samples) => Math.max(0, ...samples.map((s) => s.boosted ?? 0)),
  // The most compounds one of our bodies carried at once.
  boostedWith: (samples) => Math.max(0, ...samples.map((s) => s.boostedWith ?? 0)),
  // Our fight bodies whose life ended boosted short of a compound for each TOUGH, RANGED, HEAL,
  // MOVE and ATTACK type they carry: read on a home holding every T3 combat boost.
  boostedShort: (_, __, dir) =>
    livesOf(dir).filter((b) => b.ours && b.name.startsWith("fight-") && b.boosts > 0 && b.boosts < new Set([...b.body].filter((p) => "trhma".includes(p))).size).length,
  // What `room`'s storage, terminal, labs, factory and power spawn hold at the window's end, and the non-energy of it.
  heldAtEnd: on("room to", (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.held ?? Infinity),
  oresAtEnd: on("room to", (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.ores ?? 0),
  // From the extractor standing to the ore banked first growing, so how long the terminal or the outposts took moves neither end.
  extractorToOre: (samples) => {
    const built = samples.find((s) => s.built.extractor);
    return built ? (samples.find((s) => s.t > built.t && s.ore > built.ore)?.t ?? Infinity) - built.t : Infinity;
  },
  // Signs our bodies sent (sim only), and whether `room` ends the window signed by us since the run's first
  // tick, which the first sample follows.
  signsSent: on("from to", (samples, a) => (at(samples, a.to ?? Infinity).signsSent ?? 0) - (a.from ? (at(samples, a.from).signsSent ?? 0) : 0)),
  // Unsampled (a sim before the count) fails rather than reads as none repeated.
  signsRepeated: on("to", (samples, a) => at(samples, a.to ?? Infinity).signsRepeated ?? Infinity),
  seenAt: on("room to", (samples, a) => (at(samples, a.to ?? Infinity).seen?.[a.room] ?? Infinity) - offsetOf(samples)),
  signedInRun: on("room to", (samples, a) => Number(at(samples, a.to ?? Infinity).rooms[a.room]?.signed >= offsetOf(samples) - 1)),
  bounced: on("from to", (samples, a) => at(samples, a.to ?? Infinity).bounced - at(samples, a.from ?? 0).bounced),
  // Sent to `room`: energy, or `resource` if one is named.
  sentTo: on("room resource from to", (samples, a) => {
    const of = (s) => (a.resource ? s.sentOf?.[a.resource] : s.sent)?.[a.room] ?? 0;
    return of(at(samples, a.to ?? Infinity)) - of(at(samples, a.from ?? 0));
  }),
  // Minerals (or `resource`) that went out and came back between two of our rooms: each pair's lesser way (sim's `reversedOf`).
  sentBack: on("resource", (samples, a) => Object.entries(samples.at(-1)?.reversedOf ?? {}).reduce((n, [r, v]) => n + ((a.resource ? r === a.resource : r !== "energy") ? v : 0), 0)),
  // Energy spent on construction (`room`: in that room alone).
  build: on("room from to", (samples, a) => builtIn(at(samples, a.to ?? Infinity), a) - builtIn(at(samples, a.from ?? 0), a)),
  upgradeWhileQueued: on("room from to", (samples, a) => {
    const seen = within(samples, a);
    return seen.slice(1).reduce((n, s, i) => n + (seen[i].sites > 0 ? upgradedIn(s, a) - upgradedIn(seen[i], a) : 0), 0);
  }),
  // The longest stretch nothing was upgraded once the storage stood; no storage reads as never.
  upgradeStall: on("room from to", (samples, a) => {
    const seen = within(samples, a).filter((s) => s.built.storage);
    let most = seen.length ? 0 : Infinity;
    for (let i = 1, from = seen[0]?.t; i < seen.length; i++) {
      if (upgradedIn(seen[i], a) > upgradedIn(seen[i - 1], a)) from = seen[i].t;
      most = Math.max(most, seen[i].t - from);
    }
    return most;
  }),
  // The fewest ticks `room`'s controller stood from its downgrade over the run, never ours: -Infinity.
  leastDowngrade: on("room to", (samples, a) => at(samples, a.to ?? Infinity).rooms[a.room]?.leastDowngrade ?? -Infinity),
  // Energy our bodies carried out of a home across its border.
  carriedOut: on("room from to", (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.carriedOut ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.carriedOut ?? 0)),
  // Energy dug in every room but the colony's own.
  outpostDug: on("from to", outposts("dug")),
  dug: on("room from to", (samples, a) => (at(samples, a.to ?? Infinity).rooms[a.room]?.dug ?? 0) - (at(samples, a.from ?? 0).rooms[a.room]?.dug ?? 0)),
  upgrade: on("room from to", (samples, a) => upgradedIn(at(samples, a.to ?? Infinity), a) - upgradedIn(at(samples, a.from ?? 0), a)),
  // (none ever: Infinity)
  extensionsBeforeContainer: (samples) => {
    const first = samples.find((s) => s.sourceContainers > 0);
    return first ? (first.built.extension ?? 0) : Infinity;
  },
  sourceContainersAtEnd: on("to", (samples, a) => at(samples, a.to ?? Infinity).sourceContainers),
  // Links standing beside a source's container.
  postedLinksAtEnd: on("to", (samples, a) => at(samples, a.to ?? Infinity).postedLinks),
  builtAtEnd: on("kind to", (samples, a) => at(samples, a.to ?? Infinity).built[a.kind] ?? 0),
  sitesAtEnd: on("room kind to", (samples, a) => { const s = at(samples, a.to ?? Infinity); return (a.room ? s.homes?.[a.room] : s)?.queued?.[a.kind] ?? 0; }),
  buriedAtEnd: on("to", (samples, a) => at(samples, a.to ?? Infinity).buried ?? Infinity),
  unroadedAtEnd: on("to", (samples, a) => at(samples, a.to ?? Infinity).unroaded ?? Infinity),
  oldestPayingSite: on("from to", (samples, a) => Math.max(0, ...within(samples, a).map((s) => s.oldestPaying ?? 0))),
  // Energy lost to link transfers: none means no link ever sent.
  linked: on("from to", (samples, a) => at(samples, a.to ?? Infinity).ledger.linked - at(samples, a.from ?? 0).ledger.linked),
  // Ticks an enemy stood in `room`, else the colony's own.
  hostileTicks: on("room from to", (samples, a) => {
    const [first, last] = [at(samples, a.from ?? 0), at(samples, a.to ?? Infinity)];
    const room = a.room ?? Object.keys(last.rooms)[0];
    return (last.rooms[room]?.hostile ?? 0) - (first.rooms[room]?.hostile ?? 0);
  }),
  // Engagements opened in `room` from `from` to `to` whose predicted Win or Kite was lost (§10.1).
  predictedWinsLost: on("room from to", (_, a, dir) => bugs(engagementsOf(dir).filter(openedIn(a))).length),
  liveEngagements: on("room", (_, a, dir) => liveOf(dir, a).length),
  liveEnergy: on("room", (_, a, dir) => liveOf(dir, a).reduce((n, r) => n + r.energy, 0)),
  // Each engagement the engine saw close, missing from the bot's log or classed
  // apart from it there. An id is reused once closed; the bot's tick is the
  // game's, a tick or so off the run's.
  liveAstray: on("room from to", (samples, a, dir) => {
    const live = liveOf(dir, a);
    const near = (r) => live.find((l) => l.id === r.id && Math.abs(l.closed - offsetOf(samples) - r.closed) <= 2);
    return engagementsOf(dir).filter((r) => r.closed !== null && openedIn(a)(r) && classed(near(r)) !== classed(r)).length;
  }),
  // The tick `room`'s Defends first drew a body, less the first another
  // room's did: a home raided beside its outposts is drafted before every
  // one of them (§7.3). One never drafted: Infinity.
  draftedAfterOthers: on("room", (_, a, dir) => {
    const firsts = new Map();
    for (const e of rowsOf(dir).filter((e) => e.kind === "defend"))
      firsts.set(e.room, Math.min(firsts.get(e.room) ?? Infinity, e.drafted ?? Infinity));
    return (firsts.get(a.room) ?? Infinity) - Math.min(...[...firsts].filter(([r]) => r !== a.room).map(([, t]) => t));
  }),
  // The most ticks a Defend opened in `room` in [from, to) took to reach its foe; one never in contact, or none opened: Infinity.
  contactWait: on("room from to", (_, a, dir) => {
    const waits = rowsOf(dir).filter((e) => e.kind === "defend" && openedIn(a)(e)).map((e) => (e.contact ?? Infinity) - e.opened);
    return waits.length ? Math.max(...waits) : Infinity;
  }),
  // The ticks Defends that disengaged as their raid stepped out stood so with it back in their room (engaged.mjs).
  disengagedBeside: (_, __, dir) => rowsOf(dir).reduce((n, e) => n + (e.beside ?? 0), 0),
  // Sweeps that ever targeted `room`, wherever they opened.
  sweptIn: on("room", (_, a, dir) => rowsOf(dir).filter((e) => e.kind === "sweep" && e.rooms?.includes(a.room)).length),
  // Acts the engine refused (the soak's refusals.json) that `counted` counts.
  refusals: (_, __, dir) =>
    Object.entries(JSON.parse(readFileSync(join(dir, "refusals.json"), "utf8")))
      .filter(([act]) => counted(act))
      .reduce((n, [, c]) => n + c, 0),
  // The most percent of a member's ticks it swung back (`engaged.mjs`'s `swung`),
  // over the engagements of `kind` (none named: every one); none seen: 0.
  oscillation: on("kind", (_, a, dir) =>
    Math.max(0, ...rowsOf(dir).filter((e) => !a.kind || e.kind === a.kind).flatMap((e) => Object.values(e.swung).map((s) => Math.round((100 * s.swings) / Math.max(1, s.ticks)))))),
  // The most re-prices while Fighting, and the most bodies held at once, of one engagement of `kind` (none named: any) opened in [from, to).
  repriced: on("room kind from to", (_, a, dir) => Math.max(0, ...rowsOf(dir).filter((e) => (!a.kind || e.kind === a.kind) && openedIn(a)(e)).map((e) => e.repriced ?? 0))),
  held: on("room kind from to", (_, a, dir) => Math.max(0, ...rowsOf(dir).filter((e) => (!a.kind || e.kind === a.kind) && openedIn(a)(e)).map((e) => e.held ?? 0))),
  bodiesLost: on("room kind from to", (_, a, dir) => rowsOf(dir).filter(kindIn(a)).reduce((n, e) => n + (e.dead ?? 0), 0)),
  defendsOpened: on("room from to", (_, a, dir) => defendsIn(dir, a).length),
  // The cost of the bodies of ours `room`'s Defends drew, each once.
  defendSpent: on("room from to", (_, a, dir) => costOf(livesOf(dir).filter((b) => b.ours && drawnBy(dir, a).has(b.name)))),
  // The cost of an opponent's bodies its `name` role cast in the window.
  rivalSpent: on("name from to", (samples, a, dir) => {
    const t = (b) => b.first - offsetOf(samples);
    return costOf(livesOf(dir).filter((b) => !b.ours && b.name.startsWith(`${a.name}-`) && t(b) >= (a.from ?? 0) && t(b) < (a.to ?? Infinity)));
  }),
  // From a `name` body's first entry into `room` to the first body of a Defend
  // there entering after it: none arrived, 0; never answered, Infinity.
  answeredAfter: on("room name from to", (samples, a, dir) => {
    const lives = livesOf(dir);
    const entries = (b) => (b.entered ?? []).map(([r, at]) => [r, at - offsetOf(samples)]).filter(([r, t]) => r === a.room && t >= (a.from ?? 0) && t < (a.to ?? Infinity)).map(([, t]) => t);
    const arrived = Math.min(...lives.filter((b) => !b.ours && b.name.startsWith(`${a.name}-`)).flatMap(entries));
    if (arrived === Infinity) return 0;
    const drawn = drawnBy(dir, a);
    return Math.min(...lives.filter((b) => b.ours && drawn.has(b.name)).flatMap(entries).filter((t) => t >= arrived)) - arrived;
  }),
  // The most ticks a fighting blob stood split (engaged.mjs).
  strayedMost: on("room from to", (_, a, dir) => Math.max(0, ...rowsOf(dir).filter(openedIn(a)).map((e) => e.strayed ?? 0))),
  // Our bodies gone from the colony's rooms and their neighbours with life left: died, or walked
  // through a room the soak does not follow.
  bodiesOut: (_, __, dir) => {
    const lives = livesOf(dir);
    const end = Math.max(...lives.map((b) => b.last));
    return new Set(lives.filter((b) => b.ours && b.last < end && b.ttl > 1).map((b) => b.name)).size;
  },
  // Our bodies that walked through `room` (in by one side, out by another) on a tick in [from, to).
  crossed: on("room from to", (samples, a, dir) =>
    livesOf(dir).filter((b) => b.ours && (b.entered ?? []).some(([r, t], i, e) => r === a.room && i > 0 && i + 1 < e.length && e[i - 1][0] !== e[i + 1][0] && t - offsetOf(samples) >= (a.from ?? 0) && t - offsetOf(samples) < (a.to ?? Infinity))).length),
  // Our bodies that fell in `room` with life left.
  diedIn: on("room", (_, a, dir) => {
    const lives = livesOf(dir);
    const end = Math.max(...lives.map((b) => b.last));
    return lives.filter((b) => b.ours && b.last < end && b.ttl > 1 && b.at?.startsWith(`${a.room} `)).length;
  }),
  // Our bodies gone below full hits, with life left and no strike lately: recycled
  // wounded, since no other end leaves a body unstruck before its age.
  recycledWounded: (_, __, dir) => {
    const lives = livesOf(dir);
    const end = Math.max(...lives.map((b) => b.last));
    return lives.filter((b) => b.ours && b.last < end && b.ttl > AGED && b.hits < 100 * b.body.length && !(b.last - b.struckAt <= STRUCK_LATELY)).length;
  },
  bodiesStruck: (_, __, dir) => livesOf(dir).reduce((n, b) => n + (b.ours ? (b.struck ?? 0) : 0), 0),
  // The damage struck on the body called `name`.
  struckOn: on("name", (_, a, dir) => livesOf(dir).reduce((n, b) => n + (b.name === a.name ? (b.struck ?? 0) : 0), 0)),
  faults: (_, __, dir) => readFileSync(join(dir, "faults.jsonl"), "utf8").split("\n").filter(Boolean).length,
  // The most levels the room stood below the highest it held before; a room
  // lost stands at level 0.
  levelsLost: on("from to", (samples, a) => {
    let top = 0;
    let worst = 0;
    for (const s of within(samples, a)) {
      top = Math.max(top, s.rcl);
      worst = Math.max(worst, top - s.rcl);
    }
    return worst;
  }),
  spawnsLost: on("from to", (samples, a) => {
    const spawns = within(samples, a).map((s) => s.spawns);
    return spawns[0] - Math.min(...spawns);
  }),
  fewestCreeps: on("from to", (samples, a) => Math.min(...within(samples, a).map((s) => s.creeps))),
  // A death within STRUCK_LATELY ticks of a strike is a kill, as `death.*` reads one: a body
  // struck, then recycled by its spawn, was not killed.
  safeModeAfterCull: on("room from to", (samples, a, dir) => {
    const kills = culls(samples, a, dir);
    if (kills.length < 2) return 0;
    return Math.max(0, (within(samples, a).find((s) => s.safeMode)?.t ?? Infinity) - kills[1]);
  }),
  culled: on("room from", (samples, a, dir) => culls(samples, a, dir).length),
  safeModes: on("from to", (samples, a) => within(samples, a).filter((s, i, w) => s.safeMode && i > 0 && !w[i - 1].safeMode).length),
  // A core engagement that loses sight must not drop its razer's asks.
  closedOnCore: (samples, _, dir) => rowsOf(dir).filter((e) => e.core && e.closed !== null && (at(samples, e.closed).rooms[e.room]?.cores ?? 0) > 0).length,
  // A wave's engagement left open holds its bodies and asks: the most ticks
  // one with no core stayed open after its group was last seen.
  heldPastWave: (samples, _, dir) => Math.max(0, ...rowsOf(dir).filter((e) => !e.core).map((e) => (e.closed ?? samples.at(-1).t) - (e.seen ?? e.opened))),
  idleMost: (_, __, dir) => Math.max(0, ...rowsOf(dir).map((e) => e.idle ?? 0)),
  // Of the engagements of `kind` (any, unnamed) opened in [from, to): the most ticks one formed
  // answered, a sweep's most ticks on end in a room with no rival creep, and the rival bodies that
  // are no fight's whose tombstones stood beside it (`engaged.mjs`).
  formingMost: on("room kind from to", (_, a, dir) => Math.max(0, ...rowsOf(dir).filter(kindIn(a)).map((e) => e.forming ?? 0))),
  bareMost: on("room kind from to", (_, a, dir) => Math.max(0, ...rowsOf(dir).filter(kindIn(a)).map((e) => e.bare ?? 0))),
  killedBeside: on("room kind from to", (_, a, dir) => rowsOf(dir).filter(kindIn(a)).reduce((n, e) => n + (e.killed ?? 0), 0)),
  // The cost of the bodies of ours its engagements drew, each once.
  drewCost: on("room kind from to", (_, a, dir) => costOf(livesOf(dir).filter((b) => b.ours && new Set(rowsOf(dir).filter(kindIn(a)).flatMap((e) => e.drew)).has(b.name)))),
  // Over the Defends that drafted a body in [from, to) and never stood down, the most ticks from
  // that draft to their close.
  answeredMost: on("from to", (samples, a, dir) =>
    Math.max(0, ...rowsOf(dir).filter((e) => e.kind === "defend" && !e.disengaged && e.drafted != null && e.drafted >= (a.from ?? 0) && e.drafted < (a.to ?? Infinity)).map((e) => (e.closed ?? samples.at(-1).t) - e.drafted))),
  // Invader cores standing in the declared outposts at the window's end.
  coresAtEnd: on("to", (samples, a) => Object.values(at(samples, a.to ?? Infinity).rooms).reduce((n, r) => n + (r.cores ?? 0), 0)),
  // Samples in the window where intel keeps a standing stronghold's level, deploy or collapse
  // otherwise than the engine stands it; a collapse read before the deploy is the latest, not known.
  strongholdMisread: on("from to", (samples, a) =>
    within(samples, a).filter((s) =>
      Object.values(s.strongholds ?? {}).some(({ level, deployTime, collapse, intel }) => {
        if (!level) return false;
        if (!intel) return true;
        const [l, deployed, c, seen] = intel;
        return l !== level || (deployTime ? deployed !== deployTime : deployed > seen || c !== collapse);
      }),
    ).length),
  // The first sample in the window where `room`'s stronghold stands fallen (its core killed), else Infinity.
  strongholdFallenAt: on("room from to", (samples, a) => within(samples, a).find((s) => s.strongholds?.[a.room]?.level === 0)?.t ?? Infinity),
  // The percent of the most loot `room` held once its stronghold fell that is gone at the window's end.
  lootTaken: on("room from to", (samples, a) => {
    const fallen = within(samples, a).map((s) => s.strongholds?.[a.room]).filter((h) => h?.level === 0);
    const most = Math.max(0, ...fallen.map((h) => h.loot));
    return most > 0 ? Math.round((100 * (most - fallen.at(-1).loot)) / most) : 0;
  }),
  // Samples in the window where intel read a still-deploying stronghold's deploy tick.
  strongholdDeploying: on("from to", (samples, a) =>
    within(samples, a).filter((s) => Object.values(s.strongholds ?? {}).some((h) => h.deployTime && h.intel?.[1] === h.deployTime)).length),
  // Enemies standing in our room at the window's end.
  hostilesAtEnd: on("to", (samples, a) => at(samples, a.to ?? Infinity).hostiles),
  // The most floods one warm tick ran, and the percent of kept-flood lookups
  // that missed: §1's "counting hits and misses".
  floodsMost: on("from to", (_, a, dir) => withCounts(dir, a, (rows) => Math.max(0, ...warm(rows).map(floodsOf)))),
  coldFloodsPerColony: on("from to", (_, a, dir) =>
    withCounts(dir, a, (rows) => Math.max(0, ...rows.filter((c) => c.cold !== undefined).map((c) => Math.ceil(floodsOf(c) / Math.max(1, c.cold)))))),
  // Ticks whose outposts differ from the tick before's: each re-plans their slots.
  outpostFlips: on("from to", (_, a, dir) =>
    withCounts(dir, a, (rows) => {
      const set = (c) => Object.keys(c).filter((k) => k.startsWith("outpost:")).sort().join();
      return rows.filter((c, i) => i > 0 && set(c) !== set(rows[i - 1])).length;
    })),
  parkedTicks: on("from to", (_, a, dir) => withCounts(dir, a, (rows) => rows.filter((c) => (c.parkedCarriers ?? 0) > 0).length)),
  slotIdsShared: on("from to", (_, a, dir) => withCounts(dir, a, (rows) => Math.max(0, ...rows.map((c) => c.slotIdsShared ?? 0)))),
  threatLookupsMost: on("from to", (_, a, dir) => withCounts(dir, a, (rows) => Math.max(0, ...rows.map((c) => c.threatLookups ?? 0)))),
  atlasMissRate: on("from to", (_, a, dir) =>
    withCounts(dir, a, (rows) => {
      const hits = warm(rows).reduce((n, c) => n + (c.atlasHits ?? 0), 0);
      const misses = warm(rows).reduce((n, c) => n + (c.atlasMisses ?? 0), 0);
      return Number(((100 * misses) / Math.max(1, hits + misses)).toFixed(2));
    })),
};

// `OUTPOST_LOSSES` in `core/fight/kinds/defend.rs`: an outpost's Defend loses this past its budget before it quits.
const OUTPOST_LOSSES = 2;
const homesOf = (samples) => {
  const last = samples.at(-1);
  return new Set(Object.keys(last.homes ?? { [Object.keys(last.rooms)[0]]: last }));
};
// What an `each` assert reads: every occurrence in the run, `t` its run tick and `who` what it names.
export const events = {
  // Each engagement as `engaged.mjs` wrote it: `reach` the ticks from its opening to contact (to its
  // close, or the run's end, without one), `pastBudget` its dead beyond those Defend quits at, `bug`
  // a predicted Win or Kite lost (§10.1).
  engagement: (samples, dir) => {
    const [homes, end] = [homesOf(samples), samples.at(-1).t];
    return rowsOf(dir).map((e) => ({
      ...e,
      t: e.opened,
      who: `${e.kind} ${e.id} in ${e.room}`,
      home: homes.has(e.room),
      lasted: (e.closed ?? end) - e.opened,
      reach: (e.contact ?? e.closed ?? end) - e.opened,
      bug: bugs(tested([e])).length,
      pastBudget: e.dead - (e.budget ?? 0) - (homes.has(e.room) ? 0 : OUTPOST_LOSSES),
    }));
  },
  // Each home's safe mode, at the first sample it runs in: `killed`, our bodies killed there since
  // the last Defend opened there before it; `exposure` and `exposedBefore`, the room's over the
  // metric windows after it and from that Defend to it.
  safeMode: (samples, dir) => {
    const [offset, lives, rows, windows] = [offsetOf(samples), livesOf(dir), rowsOf(dir), windowsOf(dir, {})];
    const of = (s, room) => s.homes?.[room] ?? s;
    return [...homesOf(samples)].flatMap((room) =>
      samples
        .filter((s, i) => i > 0 && of(s, room).safeMode && !of(samples[i - 1], room).safeMode)
        .map((s) => {
          const raided = Math.max(0, ...rows.filter((e) => e.kind === "defend" && e.room === room && e.opened <= s.t).map((e) => e.opened));
          const killed = lives.filter((b) => b.ours && b.at?.startsWith(`${room} `) && b.ttl > AGED && b.last - b.struckAt <= STRUCK_LATELY && b.last - offset >= raided && b.last - offset <= s.t);
          const exposed = (w) => (w.row === room ? (w.values.exposure ?? 0) : 0);
          const exposure = windows.filter((w) => w.start >= s.t).reduce((n, w) => n + exposed(w), 0);
          const exposedBefore = windows.filter((w) => w.start < s.t && w.start + w.ticks > raided).reduce((n, w) => n + exposed(w), 0);
          return { t: s.t, who: `safe mode in ${room}`, room, raided, killed: killed.length, exposure, exposedBefore };
        }),
    );
  },
  // Each worker of ours but an outpost's mender (upkeep, not spend): `worked`, the acts of its life.
  worker: (samples, dir) => {
    const offset = offsetOf(samples);
    return livesOf(dir)
      .filter((b) => b.ours && b.name.startsWith("worker-") && b.body.split("w").length - 1 !== MENDER_WORK)
      .map((b) => ({ t: b.first - offset, who: b.name, worked: Object.values(b.acts).reduce((n, k) => n + k, 0) }));
  },
  // Each room dug that is no home, over the run: `decayedShare`, the percent of its dug left to decay.
  outpost: (samples) => {
    const homes = homesOf(samples);
    return Object.entries(samples.at(-1).rooms)
      .filter(([room, r]) => !homes.has(room) && r.dug > 0)
      .map(([room, r]) => ({ t: samples.find((s) => s.rooms[room]?.dug > 0).t, who: room, room, dug: r.dug, decayed: r.decayed, decayedShare: Math.round((100 * r.decayed) / r.dug) }));
  },
};
// A field's bound: { min?, max? }, or the value it equals.
const holds = (v, b) => (b !== null && typeof b === "object" ? v >= (b.min ?? -Infinity) && v <= (b.max ?? Infinity) : v === b);
/** The occurrences an `each` assert reads: those of its kind matching `where`. */
export const occurrences = (samples, a, dir) => events[a.each](samples, dir).filter((e) => Object.entries(a.where ?? {}).every(([k, b]) => holds(e[k], b)));
/** An `each` assert's breaches, a line each: fewer occurrences matching `where` than `least` (default 1), or one breaking a bound in `assert`. */
export const breaches = (samples, a, dir) => {
  const seen = occurrences(samples, a, dir);
  if (seen.length < (a.least ?? 1)) return [`${seen.length} ${a.each} seen, fewer than ${a.least ?? 1}`];
  return seen.flatMap((e) => Object.entries(a.assert).filter(([k, b]) => !holds(e[k], b)).map(([k]) => `${e.who} t${e.t}: ${k} ${e[k]}`));
};
measures.each = (samples, a, dir) => breaches(samples, a, dir).length;

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
/** Each invariant `samples` (default the run's) break: where, the assert, its check, and a line saying so. */
export const failures = (dir, samples = samplesOf(dir)) => {
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
    return ok ? [] : [{ where, ...a, value, bound, text: `${where}: ${a.measure}${a.kind ? ` ${a.kind}` : ""} ${value} (${bound})` }];
  });
};
export const invariants = (dir) => failures(dir).map((f) => f.text);
