// One-shot CLI over the observe channels — the Transition log (ADR 0009),
// the Raid log (ADR 0028) and the Layout record (ADR 0035): pull the
// observe subtree from `Memory.fabot.observe`, flip the verbose list
// remotely, or watch the console for a bounded window. Config via .env (loaded by
// `node --env-file-if-exists=.env`), same as upload.mjs:
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard for Memory reads; when unset and the server
//                     has exactly one shard, that shard is used
//
// Usage:
//   observe.mjs tasks              every creep's current Task with its Verdict reason
//   observe.mjs timeline <creep>   one creep's Transition log, oldest first
//   observe.mjs raids              the Raid log's episodes, newest first
//   observe.mjs layout             what the Layout could not deliver this plan
//   observe.mjs verbose            the verbose list as stored
//   observe.mjs verbose add <creep>     put a creep on the verbose list
//   observe.mjs verbose remove <creep>  take a creep off the verbose list
//   observe.mjs verbose clear           empty the verbose list
//   observe.mjs console --seconds N     subscribe to the live console for N
//                                       seconds, print what arrives, exit —
//                                       the console keeps no history, so a
//                                       bounded window is the only one-shot read
// Every read takes --json to emit the raw stored structure for jq.
import { ScreepsHttpClient } from "screeps-api";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

const usage =
  "usage: observe.mjs tasks [--json] | timeline <creep> [--json] | raids [--json] | " +
  "layout [--json] | verbose [add <creep> | remove <creep> | clear] [--json] | " +
  "console --seconds N";

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

if (!["tasks", "timeline", "raids", "layout", "verbose", "console"].includes(command))
  fail(usage);
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

const memoryGet = async (path) => {
  const res = await api.userMemoryGet(path, shard).catch((err) => {
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

    const res = await api.userMemorySet("fabot.observe.verbose", next, shard).catch((err) => {
      fail(`memory write failed: ${err.message ?? err}`);
    });
    if (res.ok !== 1) fail(`memory write failed: ${JSON.stringify(res)}`);

    // The write is queued; Memory reads keep serving the old list until the
    // game applies it on a tick boundary. Poll until the read agrees, so the
    // confirmation below is never ahead of what `verbose` would report.
    const wanted = JSON.stringify([...next].sort());
    const deadline = Date.now() + 20_000;
    for (;;) {
      const readBack = await memoryGet("fabot.observe.verbose");
      const seen = Array.isArray(readBack) ? readBack.filter((n) => typeof n === "string") : [];
      if (JSON.stringify(seen.sort()) === wanted) break;
      if (Date.now() >= deadline) {
        fail(
          `write accepted but not visible after 20s; wanted [${next.join(", ")}], ` +
            `still reading [${seen.join(", ")}] — check again with \`observe.mjs verbose\``,
        );
      }
      await new Promise((r) => setTimeout(r, 1000));
    }
    console.log(`verbose list is now [${next.join(", ")}]`);
  }
} else if (command === "raids") {
  // ---- raids: the Raid log, colony-level and episodic --------------------

  // The wire shape written by ObserveMemory.fs:
  //   { episodes: [{ opened, last, roster: [{ id, owner, body: { part: n } }],
  //                  closest?: { range, x, y, t }, losses: [{ creep, t }],
  //                  damage }],
  //     living: [creep], hits: { <structure id>: hits } }
  // Stored oldest first like the Transition log's ring, printed newest
  // first. `closest` is simply absent when nothing of ours could be placed.
  // The bot writes this leaf every tick, raid or no raid, so an absent leaf
  // is a missing channel and never an empty one — a missing leaf comes back
  // with no data, a missing intermediate (fresh respawn, no Memory.fabot at
  // all) as the string "Incorrect memory path", and both fail loudly rather
  // than reporting no raids. `living` and `hits` are the fold's own
  // baselines, scratch state and not part of the record, so --json prints
  // the episodes alone. `damage` is absent on an episode written before
  // ADR 0034 and reads as zero.
  const stored = await memoryGet("fabot.observe.raids");
  if (stored == null || typeof stored !== "object") {
    fail(
      "no Raid log at Memory.fabot.observe.raids — " +
        "an old bundle is still running, or the colony respawned and hasn't written one yet.",
    );
  }
  const episodes = Array.isArray(stored.episodes) ? [...stored.episodes].reverse() : [];

  if (json) {
    console.log(JSON.stringify(episodes, null, 2));
  } else if (episodes.length === 0) {
    console.log("no raids recorded");
  } else {
    for (const e of episodes) {
      console.log(`t${e.opened}-${e.last}  (${e.last - e.opened + 1} ticks)`);
      for (const r of e.roster ?? []) {
        const body = Object.entries(r.body ?? {})
          .map(([part, n]) => `${n} ${part}`)
          .join(" / ");
        console.log(`  ${r.owner}  ${r.id}  ${body}`);
      }
      console.log(
        e.closest
          ? `  closest approach: range ${e.closest.range} ` +
              `at (${e.closest.x},${e.closest.y}) on t${e.closest.t}`
          : "  closest approach: nothing of ours could be placed",
      );
      const losses = e.losses ?? [];
      console.log(
        losses.length === 0
          ? "  lost nothing"
          : `  lost: ${losses.map((l) => `${l.creep} (t${l.t})`).join(", ")}`,
      );
      console.log(`  damage: ${e.damage ?? 0} hits off the Keep and the ramparts`);
      console.log("");
    }
  }
} else if (command === "layout") {
  // ---- layout: what the Layout could not deliver ------------------------

  // The wire shape written by ObserveMemory.fs:
  //   { unserved: [{ x, y, kind }],
  //     unrouted: [{ source, goal, spawn? }],
  //     deferred: [{ target, source?, pick: { x, y }, serving: { x, y } }] }
  // Three lists in one leaf, all the Layout's own losses: the footing
  // targets the fold found no tile for (#77), the trunks the router found
  // no path for (#107), and the container picks the plan gave up because
  // something already serves their target (ADR 0040). The current plan's
  // record, not a history: no ring, no fold, the same lists every tick
  // under a stable census. What a list can say is three distinct answers
  // and every one of them matters (ADR 0035). A missing leaf is a missing
  // channel — a bundle
  // predating it — and fails loudly, the way `raids` does, rather than
  // reporting a confident "nothing lost" off a stale deploy; an empty list
  // is the guarantee holding, one footing per target, one trunk per
  // (source, goal) and every container target served by the tile the plan
  // picked; a row is something the colony no longer has.
  const stored = await memoryGet("fabot.observe.layout");
  if (stored == null || typeof stored !== "object") {
    fail(
      "no Layout record at Memory.fabot.observe.layout — " +
        "the deployed bundle predates it, or the colony respawned and hasn't written one yet.",
    );
  }
  // A leaf that is there but shapeless is a fourth answer, and it must not
  // collapse into the third: reading a missing list as an empty one would
  // print "every footing target has its footing" off a hand-edit or a moved
  // wire shape, which is the confident false negative this channel is built
  // to avoid (ADR 0035). Each list is guarded in its own right — a bundle
  // that writes one and not the other is exactly a moved wire shape, and
  // the half that is there must not vouch for the half that is not.
  const listOrFail = (name) => {
    const list = stored[name];
    if (!Array.isArray(list)) {
      fail(
        `the Layout record at Memory.fabot.observe.layout carries no \`${name}\` list — ` +
          'the leaf was hand-edited, or its wire shape has moved. Not read as "nothing lost".',
      );
    }
    return list;
  };
  const unserved = listOrFail("unserved");
  const unrouted = listOrFail("unrouted");
  const deferred = listOrFail("deferred");

  // A carrying vocabulary as it reads back: one case spells a name and
  // carries an id beside it, so a row that lost the id says so rather than
  // naming some other case. Flagged and printed rather than dropped —
  // Core's decoder reads such a row as nothing at all, and this is the
  // operator's tool: a row it hid would be one more silence. Both of the
  // Layout channel's carrying vocabularies read this way, a trunk's goal
  // (#107) and a deferral's target (ADR 0040).
  const carrying = (name, carries, carried) =>
    !carries ? name : carried ? `${name} ${carried}` : `${name} (no id)`;

  const goalOf = (t) => carrying(t.goal, t.goal === "spawn", t.spawn);

  const targetOf = (d) => carrying(d.target, d.target === "source", d.source);

  const tileOf = (p) => (p && typeof p === "object" ? `(${p.x},${p.y})` : "(no tile)");

  // `--json` carries every list under its own key. It used to be the bare
  // `unserved` array, back when the leaf held one list; a reader of the
  // old shape wants `.unserved`.
  if (json) {
    console.log(JSON.stringify({ unserved, unrouted, deferred }, null, 2));
  } else {
    if (unserved.length === 0) {
      console.log("every footing target has its footing");
    } else {
      console.log(
        `${unserved.length} footing target${unserved.length === 1 ? "" : "s"} with no footing:`,
      );
      for (const f of unserved) {
        console.log(`  (${f.x},${f.y})  ${f.kind}`);
      }
    }

    if (unrouted.length === 0) {
      console.log("every trunk routes");
    } else {
      console.log(
        `${unrouted.length} trunk${unrouted.length === 1 ? "" : "s"} the Layout could not route:`,
      );
      for (const t of unrouted) {
        console.log(`  ${t.source} -> ${goalOf(t)}`);
      }
    }

    // A row here is an orphan standing in the room: the plan wanted `pick`
    // and the colony keeps what is on `serving` instead (ADR 0040). Nothing
    // demolishes it, so the row stands until #114 does — it is the
    // condition that ticket waits on, not a transient.
    if (deferred.length === 0) {
      console.log("every container target is served by the tile the plan picked");
    } else {
      console.log(
        `${deferred.length} container pick${deferred.length === 1 ? "" : "s"} deferred to a container already standing:`,
      );
      for (const d of deferred) {
        console.log(`  ${targetOf(d)}  wanted ${tileOf(d.pick)}, served by ${tileOf(d.serving)}`);
      }
    }
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

  // A reason with whatever numbers it carries (#88): `too-early` rides the
  // walk and the wait the gate actually compared, so "why hasn't the Anchor
  // left yet" is answered on the line rather than by halving a cost that
  // stopped meaning ticks with ADR 0029. Every other reason is the bare
  // word it has always been. This reads Memory as it stands, not as the
  // bundle would restate it, so it can also meet a `too-early` row written
  // before the numbers existed — one the bot drops on its next load — and
  // prints the bare word for it rather than an invented pair.
  const describeReason = (row) =>
    row.walk == null || row.wait == null
      ? row.reason
      : `${row.reason}: walk ${row.walk}, wait ${row.wait}`;

  // One line of prose per verdict — reasons spelled out, creep name left to
  // the caller's layout. A scoring verdict renders one clause per Candidate.
  const describeVerdict = (v) => {
    switch (v.kind) {
      case "matched":
        return `matched ${v.task} (${v.factor})`;
      case "kept":
        return `kept ${v.task}`;
      case "released":
        return `released ${v.task} (${describeReason(v)})`;
      case "unassigned":
        return `idle (${v.reason})`;
      case "scoring":
        return (
          "scoring: " +
          v.candidates
            .map((c) =>
              c.reason
                ? `${c.task} rejected (${describeReason(c)})`
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
