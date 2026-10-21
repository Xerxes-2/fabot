// A tap's window over the recorder's ring: which ticks it keeps, what reading
// them costs of the hour's 360 segment reads, and every tick it could not keep.
// Ring, clock and budget are handed in, so a window's ticks and gaps are checked
// without the server.
import { appendFileSync, existsSync, readFileSync, writeFileSync } from "node:fs";

/// 60 of the hour's 360 segment reads stay unspent: a dozen `observe health` (1
/// read each) or `capture` (5) still answer while a tap runs.
const HELD_BACK = 60;
/// The bot writes a tick's slot during its own tick: a read asks this far in.
const INTO_TICK = 1200;
const BETWEEN = 1000;

/// The hour's segment reads as the server last answered them: `state` is the
/// client's, updated from each response's `x-ratelimit-*`, and `file` carries
/// that answer into the next tap so an hour one tap spent is not spent twice.
export function segmentBudget({ state, file, now = () => Date.now() }) {
  if (state.reset === 0 && existsSync(file)) Object.assign(state, JSON.parse(readFileSync(file, "utf8")));
  const left = () => (state.reset * 1000 > now() ? state.remaining : state.limit);
  let used = 0;
  return {
    limit: state.limit,
    left,
    used: () => used,
    can: (n) => left() - n >= HELD_BACK,
    spend: (n) => {
      used += n;
      writeFileSync(file, JSON.stringify({ remaining: state.remaining, reset: state.reset }));
    },
  };
}

/// Ticks of `range`'s window, each appended to `out` once. `range(now)` is the
/// window's `{from, to}` — `to` null while it is open — or null before it
/// opens; `ring` reads the recorder, `budget` the hour's reads.
export async function tapWindow({
  out, range, ring, budget,
  sleep = (ms) => new Promise((r) => setTimeout(r, ms)), log = console.log,
}) {
  const kept = existsSync(out) ? readFileSync(out, "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l).time) : [];
  const have = new Set(kept);
  let last = kept.length ? Math.max(...kept) : null;
  const saved = [];
  let since = null;
  let end = null;
  let tried = null;
  let stop;
  const keep = (tick) => {
    appendFileSync(out, JSON.stringify(tick) + "\n");
    have.add(tick.time);
    saved.push(tick.time);
    last = last === null ? tick.time : Math.max(last, tick.time);
    log(`t${tick.time}`);
  };
  for (;;) {
    const now = await ring.time();
    const r = await range(now);
    if (r === null) {
      await sleep(BETWEEN);
      continue;
    }
    if (since === null) {
      since = r.from;
      log(`tap: window t${since}–${r.to ?? "open"}`);
    }
    end = r.to;
    if (last !== null && last >= (r.to ?? Infinity)) {
      stop = `the window closes at t${r.to}`;
      break;
    }
    // Between ticks: the clock is free, the ring is not.
    if ((last !== null && now <= last) || tried === now) {
      await sleep(BETWEEN);
      continue;
    }
    const parts = ring.parts();
    // The ring holds this many ticks at most, so a miss further back than that
    // is gone and one read of the tick itself is all that can be had.
    const catchUp = parts === null || last === null || (now - last > 1 && now - last <= ring.depth);
    const cost = catchUp ? ring.depth : parts;
    if (!budget.can(cost)) {
      stop = `the hour's reads — ${budget.left()} of ${budget.limit} left, ${HELD_BACK} held back`;
      break;
    }
    tried = now;
    let got;
    try {
      // The bot writes a tick's slot during its own tick: ask this far in.
      await sleep(INTO_TICK);
      got = catchUp ? await ring.held() : await ring.one(now);
    } catch (e) {
      if (e?.status !== 429) throw e;
      stop = `rate limited — the server's next read in ${e.headers?.["retry-after"] ?? "?"} s`;
      break;
    }
    budget.spend(got.reads);
    const fresh = got.ticks.filter((tick) => tick.time >= since && (r.to === null || tick.time <= r.to) &&
      !have.has(tick.time) && (last === null || tick.time > last));
    for (const tick of fresh) keep(tick);
    if (catchUp && got.ticks.length === 0) {
      stop = "the recorder's ring is empty — `observe switch recorder on`";
      break;
    }
  }
  // Every tick of the window the file does not hold: the ring had dropped it, or
  // a read the budget stopped, or the window opened before the tap did.
  const gaps = [];
  const reach = end ?? last;
  for (let t = since ?? 0; since !== null && reach !== null && t <= reach; t++) {
    const gap = gaps.at(-1);
    if (have.has(t)) continue;
    if (gap && gap[1] === t - 1) gap[1] = t;
    else gaps.push([t, t]);
  }
  log(`tap: ${saved.length} ticks${saved.length ? ` ${saved[0]}–${saved.at(-1)}` : ""} into ${out} · ` +
    `${budget.used()} of the hour's ${budget.limit} segment reads, ${budget.left()} left (${HELD_BACK} held back) · ${stop}`);
  log(gaps.length ? `tap: gaps ${gaps.map(([a, b]) => (a === b ? `${a}` : `${a}-${b}`)).join(",")}` : "tap: gaps none");
  return { saved, gaps };
}
