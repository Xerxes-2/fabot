// The season's room-history replay, read the one way: `observe history`
// prints a tick of it and `capture-tick.mjs` turns a tick of it into an
// arena scenario. No auth: the files are public; only the owner names come
// off the API.
import { gunzipSync } from "node:zlib";

/// The replay file holding `tick`: one file per hundred ticks.
export const historyUrl = (url, shard, room, tick) => {
  const base = Math.floor(tick / 100) * 100;
  return { base, href: `${new URL(url).origin}/room-history/${shard}/${room}/${base}.json` };
};

/// Every object in `room` as it stood at `tick`, by id. The file's first
/// tick holds whole objects and every later tick a deep-merge diff, `null`
/// for removed, so the tick asked for is the first with every diff up to it
/// applied. Throws an Error whose message is the operator's answer.
export async function readHistory({ url, shard, room, tick }) {
  const { base, href } = historyUrl(url, shard, room, tick);
  let res;
  // The server answers a burst with 429: wait it out a few times.
  for (let attempt = 0; ; attempt++) {
    try {
      res = await fetch(href);
    } catch (err) {
      throw new Error(`history fetch failed: ${err.message ?? err}`);
    }
    if (res.status !== 429 || attempt === 3) break;
    await new Promise((resolve) => setTimeout(resolve, 2000 * 2 ** attempt));
  }
  if (res.status === 429) throw new Error(`${href} is rate-limited (HTTP 429); try again in a minute`);
  if (!res.ok) {
    throw new Error(`no history at ${href} (HTTP ${res.status}) — the newest 100-200 ticks are not written yet`);
  }
  const buffer = Buffer.from(await res.arrayBuffer());
  let text;
  try {
    text = gunzipSync(buffer).toString();
  } catch {
    text = buffer.toString();
  }
  const chunk = JSON.parse(text);
  if (!Object.hasOwn(chunk.ticks ?? {}, String(tick))) {
    throw new Error(`t${tick} is not in ${href}; it holds t${Object.keys(chunk.ticks ?? {}).join(", t")}`);
  }
  const deepMerge = (into, diff) => {
    for (const [key, value] of Object.entries(diff)) {
      if (value === null) delete into[key];
      else if (value && typeof value === "object" && !Array.isArray(value) && into[key] && typeof into[key] === "object")
        deepMerge(into[key], value);
      else into[key] = value;
    }
  };
  const objects = {};
  const ticks = Object.keys(chunk.ticks ?? {})
    .map(Number)
    .filter((t) => t <= tick)
    .sort((a, b) => a - b);
  for (const t of ticks) {
    for (const [id, o] of Object.entries(chunk.ticks[t] ?? {})) {
      if (o === null) delete objects[id];
      else if (!objects[id]) objects[id] = structuredClone(o);
      else deepMerge(objects[id], o);
    }
  }
  return { base, href, objects };
}

/// The file names owners by id; the API turns the few ids into names. An id
/// the API cannot name stays an id.
export async function usernamesOf(api, ids) {
  return new Map(
    await Promise.all(
      [...new Set(ids.filter(Boolean))].map(async (id) => {
        const found = await api.req("GET", "/api/user/find", { id }).catch(() => null);
        return [id, found?.user?.username ?? id];
      }),
    ),
  );
}

// A body as counts by part, `16w 16c 16m`, in the order the parts first
// appear; `tough` and `claim` keep enough letters to stay apart. A boosted
// run prints its tier, `10h(T3)`: T3 compounds start with X, T2 carry a 2.
const PART = { work: "w", carry: "c", move: "m", attack: "a", ranged_attack: "r", heal: "h", tough: "t", claim: "cl" };
export const boostTier = (boost) => (!boost ? 0 : boost.startsWith("X") ? 3 : boost.includes("2") ? 2 : 1);
export const bodySummary = (body) => {
  const counts = new Map();
  for (const part of body ?? []) {
    const key = `${part.type}:${boostTier(part.boost)}`;
    counts.set(key, (counts.get(key) ?? 0) + 1);
  }
  return [...counts]
    .map(([key, n]) => {
      const [type, tier] = key.split(":");
      return `${n}${PART[type] ?? type}${tier === "0" ? "" : `(T${tier})`}`;
    })
    .join(" ");
};

// The actions a creep logged this tick, `attack→24,31 move`; the engine
// writes every key and nulls the ones that did not happen. `say` is left
// out: this bot says something every tick and it is never an action.
export const actionSummary = (log) =>
  Object.entries(log ?? {})
    .filter(([name, at]) => at && name !== "say")
    .map(([name, at]) => (typeof at === "object" && "x" in at ? `${name}→${at.x},${at.y}` : name))
    .join(" ");
