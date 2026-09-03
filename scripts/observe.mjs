// One-shot CLI over the Transition log (ADR 0009): pull the observe
// subtree from `Memory.fabot.observe`, flip the verbose list remotely, or
// watch the console for a bounded window. Config via .env (loaded by
// `node --env-file-if-exists=.env`), same as upload.mjs:
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard for Memory reads; when unset and the server
//                     has exactly one shard, that shard is used
//
// Usage:
//   observe.mjs tasks              every creep's current Task with its Verdict reason
//   observe.mjs timeline <creep>   one creep's Transition log, oldest first
//   observe.mjs verbose            the verbose list as stored
//   observe.mjs verbose add <creep>     put a creep on the verbose list
//   observe.mjs verbose remove <creep>  take a creep off the verbose list
//   observe.mjs verbose clear           empty the verbose list
//   observe.mjs console --seconds N     subscribe to the live console for N
//                                       seconds, print what arrives, exit —
//                                       the console keeps no history, so a
//                                       bounded window is the only one-shot read
// Every read takes --json to emit the raw stored structure for jq.
import { ScreepsAPI } from "screeps-api";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

const usage =
  "usage: observe.mjs tasks [--json] | timeline <creep> [--json] | " +
  "verbose [add <creep> | remove <creep> | clear] [--json] | console --seconds N";

const rawArgs = process.argv.slice(2);
const json = rawArgs.includes("--json");

// Pull --seconds N out wherever it stands; what remains is positional.
let seconds;
const args = [];
for (let i = 0; i < rawArgs.length; i++) {
  if (rawArgs[i] === "--json") continue;
  if (rawArgs[i] === "--seconds") {
    seconds = Number(rawArgs[++i]);
  } else {
    args.push(rawArgs[i]);
  }
}
const [command, ...rest] = args;
// timeline's one positional is a creep name; verbose's are an action and a name.
const creepArg = rest[0];
const [action, actionName] = rest;

if (!["tasks", "timeline", "verbose", "console"].includes(command)) fail(usage);
if (command === "timeline" && !creepArg) fail(usage);
if (command === "verbose" && action !== undefined) {
  if (!["add", "remove", "clear"].includes(action)) fail(usage);
  if (action !== "clear" && !actionName) fail(usage);
}
if (command === "console" && !(Number.isFinite(seconds) && seconds > 0)) {
  fail("console needs --seconds N (a positive number): the subscription must be bounded");
}
if (command !== "console" && seconds !== undefined) fail(usage);

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

const memoryGet = async (path) => {
  const res = await api.memory.get(path, shard).catch((err) => {
    fail(`memory read failed: ${err.message ?? err}`);
  });
  if (res.ok !== 1) fail(`memory read failed: ${JSON.stringify(res)}`);
  return res.data;
};

// ---- console: a bounded subscription, the one non-Memory command --------

if (command === "console") {
  // Whatever arrives is printed as-is: log lines, command results, and the
  // engine's error channel (where intent failures land), prefixed apart.
  await api.socket.connect().catch((err) => fail(`socket connect failed: ${err.message ?? err}`));
  api.socket.subscribe("console", (event) => {
    const messages = event.data?.messages ?? {};
    for (const line of messages.log ?? []) console.log(line);
    for (const line of messages.results ?? []) console.log(`> ${line}`);
    if (event.data?.error) console.log(`[error] ${event.data.error}`);
  });
  setTimeout(() => {
    api.socket.disconnect();
    process.exit(0);
  }, seconds * 1000);
} else if (command === "verbose") {
  // ---- verbose: the list beside the log, read and written in place ------

  // The stored list, tolerantly: the bot treats anything malformed as off,
  // and so does the CLI — a bad shape reads as empty and is overwritten
  // whole on the next write.
  const stored = action === "clear" ? [] : await memoryGet("fabot.observe.verbose");
  const current = Array.isArray(stored) ? stored.filter((n) => typeof n === "string") : [];

  if (action === undefined) {
    if (json) console.log(JSON.stringify(current, null, 2));
    else if (current.length === 0) console.log("verbose list is empty");
    else for (const n of current) console.log(n);
  } else {
    const next =
      action === "clear" ? [] :
      action === "add" ? [...new Set([...current, actionName])] :
      current.filter((n) => n !== actionName);

    const res = await api.memory.set("fabot.observe.verbose", next, shard).catch((err) => {
      fail(`memory write failed: ${err.message ?? err}`);
    });
    if (res.ok !== 1) fail(`memory write failed: ${JSON.stringify(res)}`);
    console.log(`verbose list is now [${next.join(", ")}]`);
  }
} else {
  // ---- tasks / timeline: reads over the Transition log ------------------

  // The wire shape written by ObserveMemory.fs:
  //   { <creep>: { log: [{ t, v }], lastTask?: verdict, lastScoring?: verdict,
  //                lastMove: [verdict] } }
  // where a verdict is { kind, ...fields } — see encodeVerdict for the kinds.
  // A missing leaf comes back with no data; a missing intermediate (fresh
  // respawn, no Memory.fabot at all) as the string "Incorrect memory path".
  const creeps = await memoryGet("fabot.observe.creeps");
  if (creeps == null || typeof creeps !== "object") {
    fail(
      "no Transition log at Memory.fabot.observe.creeps — " +
        "an old bundle is still running, or the colony respawned and hasn't written one yet.",
    );
  }

  // One line of prose per verdict — reasons spelled out, creep name left to
  // the caller's layout. A scoring verdict renders one clause per Candidate.
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
      case "scoring":
        return (
          "scoring: " +
          v.candidates
            .map((c) =>
              c.reason
                ? `${c.task} rejected (${c.reason})`
                : `${c.task} rank=${c.rank} cost=${c.cost} load=${c.load}`,
            )
            .join("; ")
        );
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
        console.log(
          `${name.padEnd(width)}  ${last ? describeVerdict(last) : "(no task verdict yet)"}`,
        );
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
}
