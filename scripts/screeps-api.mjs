// The server connection the operator scripts share (`observe.mjs` today), so
// "which shard" is spelt once, in step with `.env.example`.
//
// Config via .env (loaded by `node --env-file-if-exists=.env`):
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard to talk to; when unset and the server has
//                     exactly one shard, that shard is used
import { gunzipSync } from "node:zlib";
import { ScreepsHttpClient } from "screeps-api";

/// Say why and stop. These are operator tools run from a terminal, so a bad
/// token or a missing shard is a message and a non-zero exit, never a stack.
export const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

/// The client, the shard it is pointed at and the base URL it reached — the
/// URL because a script that prints what it captured has to say where the
/// numbers came from.
export async function connect() {
  const token = process.env.SCREEPS_TOKEN;
  if (!token) {
    fail("SCREEPS_TOKEN is not set. Copy .env.example to .env and fill in your token.");
  }
  const url = (process.env.SCREEPS_API_URL ?? "https://screeps.com/season").replace(/\/$/, "") + "/";
  const api = new ScreepsHttpClient({ token, url });

  let shard = process.env.SCREEPS_SHARD;
  if (!shard) {
    const info = await api.req("GET", "/api/game/shards/info", {}).catch((err) => {
      fail(`shard lookup failed: ${err.message ?? err}`);
    });
    const shards = (info.shards ?? []).map((s) => s.name);
    if (shards.length !== 1) {
      fail(`server has shards [${shards.join(", ")}]; set SCREEPS_SHARD to pick one.`);
    }
    shard = shards[0];
  }

  return { api, shard, url, token };
}

/// The hour's segment reads as the server last answered them, through the
/// client's own tracker: each response's `x-ratelimit-remaining` and
/// `x-ratelimit-reset` for the route, so a caller can stop before the 429.
export const segmentReads = (api) => api.rateLimits.GET["/api/user/memory-segment"];

/// The season client's replay files: one gzip JSON per 100 ticks at
/// `<origin>/room-history/<shard>/<room>/<floor(tick/100)*100>.json` (its
/// `HISTORY_URL`, not an API route, so a moved path is a 404 here and not a
/// quiet hole in a report). The JSON's first tick of a chunk is a whole frame,
/// every tick after it only the fields that changed.
export async function roomHistory({ url, token, shard, room, tick }) {
  const base = Math.floor(tick / 100) * 100;
  const res = await fetch(`${new URL(url).origin}/room-history/${shard}/${room}/${base}.json`, { headers: { "X-Token": token } });
  if (!res.ok) return null;
  const body = Buffer.from(await res.arrayBuffer());
  return (body[0] === 0x1f && body[1] === 0x8b ? gunzipSync(body) : body).toString("utf8");
}
