// The server connection the authoring scripts share: `observe.mjs`, which
// reads what the live colony wrote to Memory, and `capture-room.mjs`, which
// captures a room's fixed shape into a fixture (ADR 0036). Both reach the
// same account the same way, and a second spelling of "which shard" would be
// a second thing to keep in step with `.env.example`.
//
// Config via .env (loaded by `node --env-file-if-exists=.env`):
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard to talk to; when unset and the server has
//                     exactly one shard, that shard is used
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

  return { api, shard, url };
}
