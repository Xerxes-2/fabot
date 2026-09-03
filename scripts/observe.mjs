// One-shot CLI over the Transition log (ADR 0009): pull the observe
// subtree from `Memory.fabot.observe` and print it. Config via .env
// (loaded by `node --env-file-if-exists=.env`), same as upload.mjs:
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard for Memory reads; when unset and the server
//                     has exactly one shard, that shard is used
//
// Usage:
//   observe.mjs tasks             every creep's current Task with its Verdict reason
//   observe.mjs timeline <creep>  one creep's Transition log, oldest first
// Every read takes --json to emit the raw stored structure for jq.
import { ScreepsAPI } from "screeps-api";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

const args = process.argv.slice(2).filter((a) => a !== "--json");
const json = process.argv.includes("--json");
const [command, creepArg] = args;

const usage = "usage: observe.mjs tasks [--json] | observe.mjs timeline <creep> [--json]";
if (command !== "tasks" && command !== "timeline") fail(usage);
if (command === "timeline" && !creepArg) fail(usage);

const token = process.env.SCREEPS_TOKEN;
if (!token) {
  fail("SCREEPS_TOKEN is not set. Copy .env.example to .env and fill in your token.");
}
const url = (process.env.SCREEPS_API_URL ?? "https://screeps.com/season").replace(/\/$/, "") + "/";
const api = new ScreepsAPI({ token, url });

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
const res = await api.memory.get("fabot.observe.creeps", shard).catch((err) => {
  fail(`memory read failed: ${err.message ?? err}`);
});
if (res.ok !== 1) fail(`memory read failed: ${JSON.stringify(res)}`);

// The wire shape written by ObserveMemory.fs:
//   { <creep>: { log: [{ t, v }], lastTask?: verdict, lastMove: [verdict] } }
// where a verdict is { kind, ...fields } — see encodeVerdict for the kinds.
// A missing leaf comes back with no data; a missing intermediate (fresh
// respawn, no Memory.fabot at all) as the string "Incorrect memory path".
const creeps = res.data;
if (creeps == null || typeof creeps !== "object") {
  fail(
    "no Transition log at Memory.fabot.observe.creeps — " +
      "an old bundle is still running, or the colony respawned and hasn't written one yet.",
  );
}

// One line of prose per verdict, conclusion level — reasons spelled out,
// creep name left to the caller's layout.
const describeVerdict = (v) => {
  switch (v.kind) {
    case "matched":
      return `matched ${v.task} (${v.factor})`;
    case "kept":
      return `kept ${v.task}`;
    case "released":
      return `released ${v.task} (${v.reason})`;
    case "unassigned":
      return `idle (${v.reason})`;
    case "grounded":
      return "grounded";
    case "yielded":
      return `yielded to ${v.counterpart}`;
    case "rerouted":
      return "rerouted";
    default:
      return JSON.stringify(v);
  }
};

const names = Object.keys(creeps).sort();

if (command === "tasks") {
  if (json) {
    // Raw lastTask per creep: the task-channel cursor is the current assignment.
    const out = Object.fromEntries(names.map((n) => [n, creeps[n].lastTask ?? null]));
    console.log(JSON.stringify(out, null, 2));
  } else if (names.length === 0) {
    console.log("no creeps in the Transition log");
  } else {
    const width = Math.max(...names.map((n) => n.length));
    for (const name of names) {
      const last = creeps[name].lastTask;
      console.log(`${name.padEnd(width)}  ${last ? describeVerdict(last) : "(no task verdict yet)"}`);
    }
  }
} else {
  const record = creeps[creepArg];
  if (!record) {
    fail(`no timeline for creep "${creepArg}"; known creeps: ${names.join(", ") || "(none)"}`);
  }
  if (json) {
    console.log(JSON.stringify(record, null, 2));
  } else if (record.log.length === 0) {
    console.log(`timeline for ${creepArg} is empty`);
  } else {
    const width = Math.max(...record.log.map((e) => String(e.t).length));
    for (const entry of record.log) {
      console.log(`${String(entry.t).padStart(width)}  ${describeVerdict(entry.v)}`);
    }
  }
}
