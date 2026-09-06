// One-shot CLI over the observe channels — the Transition log (ADR 0009),
// the Raid log (ADR 0028), the Layout record (ADR 0035) and the CPU line
// (ADR 0041): pull the observe subtree from `Memory.fabot.observe`, flip
// the verbose list remotely, or watch the console for a bounded window.
// `outposts` is the one read that needs a second endpoint — it also reads
// the server's clock, because shut-or-open has no answer without it, and
// it fails on that read alone where the others cannot.
//
// Config via .env (loaded by `node --env-file-if-exists=.env`), same as
// upload.mjs:
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_SHARD   - shard for Memory reads; when unset and the server
//                     has exactly one shard, that shard is used
//
// Usage:
//   observe.mjs tasks              every creep's current Task with its Verdict reason
//   observe.mjs timeline <creep>   one creep's Transition log, oldest first
//   observe.mjs raids              the Raid log's episodes, newest first
//   observe.mjs outposts           every outpost the Raid log knows: shut or
//                                  open right now, the tick a stand-down runs
//                                  to, the deadline that tick was read off, and
//                                  the rooms another player took, shut by no clock
//   observe.mjs layout             what the Layout could not deliver this plan
//   observe.mjs cpu                the per-tick CPU line — the tick's total,
//                                  where it went phase by phase, how many
//                                  intents the engine took, and ADR 0041's
//                                  revisit trigger read off the totals
//   observe.mjs verbose            the verbose list as stored
//   observe.mjs verbose add <creep>     put a creep on the verbose list
//   observe.mjs verbose remove <creep>  take a creep off the verbose list
//   observe.mjs verbose clear           empty the verbose list
//   observe.mjs console --seconds N     subscribe to the live console for N
//                                       seconds, print what arrives, exit —
//                                       the console keeps no history, so a
//                                       bounded window is the only one-shot read
// Every read takes --json to emit the raw stored structure for jq.
//
// `raids`, `outposts` and `layout` are one colony's record and take
// `--colony <home>` to say whose (ADR 0047). Without it they read the first
// colony under `Memory.fabot.observe.colonies` — this script cannot see
// `Colony.declared`, so "first" is the first home the bot wrote a leaf for,
// which is declaration order because the loop writes in it.
import { ScreepsHttpClient } from "screeps-api";
import { report as cpuReport } from "./cpu-trigger.mjs";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

const usage =
  "usage: observe.mjs tasks [--json] | timeline <creep> [--json] | " +
  "raids [--colony <home>] [--json] | outposts [--colony <home>] [--json] | " +
  "layout [--colony <home>] [--json] | cpu [--json] | " +
  "verbose [add <creep> | remove <creep> | clear] [--json] | " +
  "console --seconds N";

const rawArgs = process.argv.slice(2);
const json = rawArgs.includes("--json");

// Pull --seconds N and --colony <home> out wherever they stand; what
// remains is positional.
let seconds;
let colonyArg;
const args = [];
for (let i = 0; i < rawArgs.length; i++) {
  if (rawArgs[i] === "--json") continue;
  if (rawArgs[i] === "--seconds") {
    seconds = Number(rawArgs[++i]);
  } else if (rawArgs[i] === "--colony") {
    colonyArg = rawArgs[++i];
  } else {
    args.push(rawArgs[i]);
  }
}
const [command, ...rest] = args;
// timeline's one positional is a creep name; verbose's are an action and a name.
const creepArg = rest[0];
const [action, actionName] = rest;

if (
  !["tasks", "timeline", "raids", "outposts", "layout", "cpu", "verbose", "console"].includes(
    command,
  )
)
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
// The colony-keyed commands are exactly the two channels that split by home
// (ADR 0047); anywhere else the flag would name a colony nothing reads.
if (rawArgs.includes("--colony")) {
  if (!["raids", "outposts", "layout"].includes(command)) fail(usage);
  // The flag eats the next argument, so a bare `--colony` at the end, or one
  // in front of `--json`, would silently read the default colony instead of
  // the one the operator asked for.
  if (colonyArg === undefined || colonyArg.startsWith("--")) {
    fail("--colony needs a home room name, e.g. --colony W12S28");
  }
}

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

// One colony's leaf, under `Memory.fabot.observe.colonies.<home>` (ADR
// 0047): the two channels that are a colony's record rather than the
// world's — the Raid log and the Layout record — are keyed by home room,
// because `decide` runs once per colony and each answers for the rooms its
// own colony works.
//
// The whole subtree is read in one call rather than the one path, because
// the key list is itself an answer: it is what `--colony` is checked
// against and what the default is taken from. An absent subtree fails
// loudly the way an absent leaf always has — a bundle predating the split
// writes the old flat leaves and nothing here, and reading that as "no
// raids" would be the confident false negative these commands exist to
// avoid. A missing intermediate (fresh respawn, no Memory.fabot at all)
// comes back as the string "Incorrect memory path" and fails the same way.
const colonyLeaf = async (leaf) => {
  const colonies = await memoryGet("fabot.observe.colonies");
  if (colonies == null || typeof colonies !== "object" || Array.isArray(colonies)) {
    fail(
      "no colony subtree at Memory.fabot.observe.colonies — " +
        "an old bundle is still running (the Raid log and the Layout record moved under " +
        "this key when decide began running once per colony, ADR 0047), or the colony " +
        "respawned and hasn't written one yet.",
    );
  }
  const homes = Object.keys(colonies);
  if (homes.length === 0) {
    fail(
      "Memory.fabot.observe.colonies is empty: no colony wrote a leaf last tick — " +
        "no declared home is both ours and holding a spawn (ADR 0047).",
    );
  }
  const home = colonyArg ?? homes[0];
  if (!Object.prototype.hasOwnProperty.call(colonies, home)) {
    fail(`no colony "${home}" here; the homes with a record are [${homes.join(", ")}].`);
  }
  const stored = colonies[home]?.[leaf];
  if (stored == null || typeof stored !== "object") {
    fail(
      `no \`${leaf}\` record at Memory.fabot.observe.colonies.${home} — ` +
        "the deployed bundle predates it, or that colony has not written one yet.",
    );
  }
  return { home, stored };
};

// The Raid log's leaf, read the same way for both of its families: the
// spawn-room raids and the outpost stand-downs (ADR 0043) share one leaf,
// so they share this read and the one sentence that explains its absence.
// Each family's own guard — `episodes` for the raids, `outposts` for the
// stand-downs — stays in its command, because those differ deliberately.
const raidLeaf = () => colonyLeaf("raids");

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
  //     outposts: [{ room, opened, last, expiry, basis }],
  //     rivalHeld: { <room>: tick },
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
  // ADR 0034 and reads as zero. `outposts` is the Raid log's second family
  // (ADR 0043) — one row per [[stand-down]], the room it shuts, the tick it
  // runs to and which deadline that tick was read off; it is a family of
  // its own and `observe.mjs outposts` reads it whole, so this command
  // prints the spawn-room raids alone rather than filtering a mixed list.
  // `rivalHeld` is that same command's other half — the rooms last seen in
  // another player's hands, against the tick the gate shut on, ADR 0043's
  // withdrawal with no clock — and is no more a raid than a stand-down is.
  const { home, stored } = await raidLeaf();
  const episodes = Array.isArray(stored.episodes) ? [...stored.episodes].reverse() : [];

  if (json) {
    console.log(JSON.stringify(episodes, null, 2));
  } else if (episodes.length === 0) {
    console.log(`no raids recorded for ${home}`);
  } else {
    console.log(`colony ${home}`);
    console.log("");
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
} else if (command === "outposts") {
  // ---- outposts: the Raid log's second family, read as the gate reads it --

  // The wire shape written by ObserveMemory.fs, a key of its own beside
  // `episodes` in the same leaf:
  //   { outposts: [{ room, opened, last, expiry, basis }],
  //     rivalHeld: { <room>: tick } }
  // One `outposts` row per clocked [[stand-down]] (ADR 0043): the room it
  // shuts, the window (opened, and the last tick a core was actually seen
  // there), the absolute tick the stand-down runs to, and which of the
  // three deadlines that tick was read off. Stored oldest first like the
  // raids beside it.
  //
  // Shut or open is `now < expiry` and nothing else — Observe.standingDown,
  // the one place the family's openness is decided — so this command is a
  // read of the same rule the gate applies, never a second one. `last` is
  // deliberately not part of that test: the stand-down withdraws the very
  // creeps whose vision would see the core, so silence there says nobody is
  // looking and never that the room is clear.
  //
  // `rivalHeld` is ADR 0043's other withdrawal, and it has no row shape
  // because it has almost nothing to carry: a room another player owns or
  // reserves is not a threat with a deadline, it is a room that stopped
  // being ours to work, so the record is the room's name against the tick
  // the last look concluded it, and the gate withholds it with no clock to
  // compare against. The tick is not a deadline and nothing is measured
  // from it; it is the date an income drop is lined up against (#117's
  // US-20), the answer the clocked family gets from `opened`. It is a
  // remembered conclusion — the fold writes it on the ticks with vision and
  // holds it through the ticks without, because the gate's own effect is to
  // take that vision away.
  const { home, stored } = await raidLeaf();
  // The list is guarded in its own right, the way each of the Layout
  // record's three is: a leaf carrying `episodes` and no `outposts` is a
  // bundle predating ADR 0043's family or a wire shape that has moved, and
  // the half that is there must not vouch for the half that is not.
  // Reading it as an empty ring would answer "no outpost is shut" off a
  // deploy that cannot shut one — the confident false negative this
  // channel exists to prevent, and the one an operator back from a week
  // away is least able to catch.
  if (!Array.isArray(stored.outposts)) {
    fail(
      `the Raid log at Memory.fabot.observe.colonies.${home}.raids carries no ` +
        "`outposts` list — the deployed bundle predates ADR 0043's outpost family, " +
        "the leaf was hand-edited, " +
        'or its wire shape has moved. Not read as "no outpost is shut".',
    );
  }
  // The clockless half, guarded in its own right for the same reason and
  // never off the presence of the half above: a leaf carrying `outposts`
  // and no `rivalHeld` is a bundle predating the gate, and a room that
  // bundle's colony had already been pushed out of would print as worked.
  if (
    stored.rivalHeld === null ||
    typeof stored.rivalHeld !== "object" ||
    Array.isArray(stored.rivalHeld)
  ) {
    fail(
      `the Raid log at Memory.fabot.observe.colonies.${home}.raids carries no ` +
        "`rivalHeld` map — the deployed bundle predates ADR 0043's clockless withdrawal, " +
        "the leaf was " +
        'hand-edited, or its wire shape has moved. Not read as "no room was taken".',
    );
  }
  const rivalHeld = Object.entries(stored.rivalHeld).map(([room, since]) => {
    if (typeof since !== "number") {
      fail(
        `the tick at Memory.fabot.observe.colonies.${home}.raids.rivalHeld.${room} is off the ` +
          `wire shape: ${JSON.stringify(since)} — the leaf was hand-edited, or its wire ` +
          "shape has moved. " +
          'Not read as "that room is open": the bot is withholding a room this command ' +
          "cannot date.",
      );
    }
    return { room, since };
  });

  // The clock the rows are read against. Off the server rather than off the
  // CPU line's last row: that row is as old as the last tick the bundle
  // finished, and a bundle that stopped writing leaves it behind while the
  // game clock runs on — every stand-down would read as still running.
  // Unreadable is fatal, because "shut or open" has no answer without it
  // and the answer it would default to is "open".
  const clock = await api.gameTime(shard).catch((err) => {
    fail(`game time read failed: ${err.message ?? err}`);
  });
  if (clock.ok !== 1 || typeof clock.time !== "number") {
    fail(`game time read failed: ${JSON.stringify(clock)}`);
  }
  const now = clock.time;

  // The basis vocabulary exactly as `standDownBasisName` spells it on the
  // wire (Core's Types.fs), one clause each: "shut until 172,783" and "shut
  // until 172,783 because nothing could be read" are different answers to
  // an operator, which is why the basis is carried at all.
  const BASIS = {
    "collapse-timer": "the core's own collapse timer",
    reservation: "the end of the reservation the Invader core took",
    fallback: "no deadline was readable — ADR 0043's 2,500-tick expansion period",
  };

  // A row off the wire shape is fatal and quoted, never dropped. The
  // asymmetry is ADR 0043's: a row this reader hid would show its room as
  // open, and Core's decoder drops a row whose `expiry` or `basis` will not
  // decode — so the room a dropped row was holding really is open to the
  // bot, and saying so out loud is the whole point of the command.
  const rows = stored.outposts.map((row) => {
    const readable =
      row !== null &&
      typeof row === "object" &&
      typeof row.room === "string" &&
      typeof row.opened === "number" &&
      typeof row.last === "number" &&
      typeof row.expiry === "number" &&
      // An own-key test and never `BASIS[row.basis] !== undefined`: the
      // key comes off the wire, and every object literal answers a
      // prototype name — `toString`, `constructor`, `valueOf`,
      // `__proto__` — with a function rather than `undefined`. A row
      // spelling one of those would read as a known basis here and print
      // JavaScript internals as its reason, while Core's decoder answers
      // `None` for it (`standDownBasisOf`, Types.fs) and drops the row:
      // the room would stand wide open with this command calling it shut.
      // The vocabulary is exactly the three names `standDownBasisName`
      // spells, and nothing the language put on the table beside them.
      Object.hasOwn(BASIS, row.basis);
    if (!readable) {
      fail(
        `a stand-down row at Memory.fabot.observe.colonies.${home}.raids.outposts is off the ` +
          "wire shape: " +
          `${JSON.stringify(row)} — the leaf was hand-edited, or its wire shape has moved. ` +
          'Not read as "that room is open": the bot drops a row it cannot decode, so a room ' +
          "this one names may be standing wide open right now.",
      );
    }
    return row;
  });

  const tickOf = (t) => `t${t.toLocaleString("en-US")}`;
  const ticks = (n) => `${n.toLocaleString("en-US")} tick${n === 1 ? "" : "s"}`;

  // ADR 0043's dated observation, and the one number here the colony can
  // never read for itself: W15S24 is four rooms out, the bot has no
  // scouting (ADR 0041) and never has vision there, so this cannot arrive
  // on a [[colony view]] the way an outpost's expiry does. It is printed beside
  // the rows because it changes how every one of them reads — when it
  // passes, this sector's invasion switch is off until another stronghold
  // spawns, so a stand-down opened after it is a core that was already
  // standing rather than a fresh expansion, and the 2,500-tick fallback
  // stops being a cadence anything is still running on.
  //
  // `collapse` is the read-only HTTP API's raw `endTime` — an absolute
  // tick, which is the only reason it may be compared against `now` as it
  // stands. Refreshing it the obvious way, off the runtime, would write a
  // *relative* count here: `RoomObject.effects[].ticksRemaining`, the
  // number `InvaderCoreInfo.CollapseTick` is built from, is "how many
  // ticks the effect still lasts" and World.fs adds `Game.time` to it
  // for exactly this reason. Substituted here it would date the sector
  // clock a hundred thousand ticks wrong and print the switch as already
  // off — the one date ADR 0043 says changes every other conclusion.
  const SECTOR = {
    stronghold: "W15S24",
    collapse: 170283,
    read: "t105,945-106,529",
  };

  const roomsOf = (list) => [...new Set(list.map((row) => row.room))].sort();

  if (json) {
    // The stored rows verbatim beside the two facts a reader cannot
    // recover from them — the tick they were judged against, and the
    // sector's date. A row is never hidden from --json; an unreadable one
    // has already failed the whole command above.
    console.log(
      JSON.stringify(
        { now, sector: SECTOR, outposts: stored.outposts, rivalHeld: stored.rivalHeld },
        null,
        2,
      ),
    );
  } else {
    console.log(`colony ${home}, now ${tickOf(now)}`);
    console.log("");

    if (rows.length === 0 && rivalHeld.length === 0) {
      console.log("the Raid log records no stand-down: no outpost is shut");
      console.log("");
    } else {
      // The clockless withdrawals first, and they answer for their room
      // whatever the clocked rows say about it: a room another player holds
      // is shut by a rule with no expiry, so a spent stand-down sitting in
      // the ring beside it must not print that room as open.
      //
      // The clocked rows for the same room are printed under this line and
      // not filtered away with it: a core and a rival's claimer can take one
      // controller on one tick, and an operator told "no clock is running"
      // while the log holds an expiry and a basis has lost exactly the trace
      // ADR 0043 asks this channel to keep (#117's US-20).
      const heldRooms = rivalHeld.map((held) => held.room);

      for (const held of [...rivalHeld].sort((a, b) => a.room.localeCompare(b.room))) {
        console.log(`${held.room}  shut since ${tickOf(held.since)}, and no clock is running`);
        console.log("  because another player owns or reserves it — not a threat that passes,");
        console.log("  a room that stopped being ours to work (ADR 0043)");
        // The truth about getting back in, and it is not a thing the colony
        // can do: the gate subtracts by room name after the declaration is
        // read (`Outpost.worked`), so re-declaring this room changes
        // nothing, and the room is never scanned again, so the tick with
        // vision that would clear it can never arrive.
        console.log("  nothing the colony does re-opens it: the room is not scanned, so the");
        console.log("  look that would clear it never happens, and re-declaring it is a no-op.");
        console.log(
          `  clear "${held.room}" from ` +
            `Memory.fabot.observe.colonies.${home}.raids.rivalHeld once a look confirms it ` +
            "is free,",
        );
        console.log("  or move the declaration in Core to a room somebody else is not working");
        console.log("");
      }

      for (const room of roomsOf(rows)) {
        const mine = rows.filter((row) => row.room === room);
        // At most one of a room's rows can be running — a sighting extends
        // the standing episode and opens a new one only when none holds —
        // but the latest expiry is taken rather than assumed, so a
        // hand-edited leaf reads out the row that is actually holding.
        const running = mine.filter((row) => now < row.expiry).sort((a, b) => b.expiry - a.expiry);
        const spent = mine.filter((row) => now >= row.expiry).sort((a, b) => b.expiry - a.expiry);

        // A room the clockless half already answered for gets its clocked
        // rows as a continuation of that answer rather than a second
        // headline: the gate above it has no expiry to run out, so "open"
        // here would contradict the line four rows up.
        const alsoHeld = heldRooms.includes(room);

        if (running.length > 0) {
          const row = running[0];
          console.log(
            alsoHeld
              ? `${room}  and a stand-down is recorded too, until ${tickOf(row.expiry)} — ` +
                  `${ticks(row.expiry - now)} to go`
              : `${room}  shut until ${tickOf(row.expiry)} — ${ticks(row.expiry - now)} to go`,
          );
          console.log(`  because ${BASIS[row.basis]}`);
          console.log(`  opened ${tickOf(row.opened)}, a core last seen there ${tickOf(row.last)}`);
        } else {
          const row = spent[0];
          console.log(
            alsoHeld
              ? `${room}  and the stand-down recorded beside that has run out`
              : `${room}  open — no stand-down is running`,
          );
          console.log(
            `  last one ran to ${tickOf(row.expiry)}, spent ${ticks(now - row.expiry)} ago ` +
              `(${BASIS[row.basis]})`,
          );
        }
        console.log("");
      }
    }

    // The rooms this command cannot name. The declared outposts are a
    // constant in Core a human moves (ADR 0041) and no Memory leaf carries
    // them, so a room that has never stood down has no row here and cannot
    // be listed as open — said out loud rather than left to be read as
    // "these are all of them".
    console.log(
      "rows are the stand-downs the log holds; a declared outpost that has never been shut " +
        "has no row and is not named above.",
    );
    console.log("");

    console.log(
      now < SECTOR.collapse
        ? `sector clock: ${SECTOR.stronghold}'s collapse timer ends ${tickOf(SECTOR.collapse)} — ` +
            `${ticks(SECTOR.collapse - now)} away; after it this sector's invasion switch is off ` +
            "until another stronghold spawns"
        : `sector clock: ${SECTOR.stronghold}'s collapse timer ended ${tickOf(SECTOR.collapse)}, ` +
            `${ticks(now - SECTOR.collapse)} ago — this sector's invasion switch is off unless ` +
            "another stronghold has spawned since",
    );
    console.log(
      `  read off the read-only API at ${SECTOR.read} and recorded in ADR 0043. A dated ` +
        "observation, never a live read: the colony has no vision there and never will.",
    );
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
  const { home, stored } = await colonyLeaf("layout");
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
        `the Layout record at Memory.fabot.observe.colonies.${home}.layout carries no ` +
          `\`${name}\` list — the leaf was hand-edited, or its wire shape has moved. ` +
          'Not read as "nothing lost".',
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
    console.log(`colony ${home}`);
    console.log("");
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
} else if (command === "cpu") {
  // ---- cpu: the per-tick CPU line ---------------------------------------

  // The wire shape written by ObserveMemory.fs:
  //   { ticks: [{ t, ms, entry?, snapshot?, decide?, save?, execute?, intents? }] }
  // One row per tick the loop finished, oldest first, capped at the ring
  // Core keeps (ADR 0041). The tick number rides each row because the
  // window is only as long as the ticks in it: a tick that threw before
  // the write reaches Memory leaves no row, and the gap in the numbers is
  // the record of it. `ms` is what the bot had spent by the time it
  // stopped looking — after the Executor's intents, before the engine
  // serializes Memory — so it is the tick's cost minus a constant nobody
  // can move.
  //
  // The six optional keys are the phase split (#170) and they are what
  // this command exists to print: the local ruler measures the same
  // scenario at 10.45 ms/tick against a 49.4 ms mean here, and the ruler
  // has no engine — no prelude, no 0.2 CPU per intent — so a single total
  // cannot say where the difference sits. The judgement below still reads
  // the totals alone, unchanged: the split is attribution, not a threshold.
  //
  // A missing leaf is a missing channel and fails loudly the way `raids`
  // and `layout` do: reading it as an empty window would print a
  // confident "not triggered" off a bundle that measures nothing, which
  // is the one answer this channel exists to prevent.
  const stored = await memoryGet("fabot.observe.cpu");
  if (stored == null || typeof stored !== "object") {
    fail(
      "no CPU line at Memory.fabot.observe.cpu — " +
        "the deployed bundle predates it, or the colony respawned and hasn't written one yet.",
    );
  }
  if (!Array.isArray(stored.ticks)) {
    fail(
      "the CPU line at Memory.fabot.observe.cpu carries no `ticks` list — " +
        'the leaf was hand-edited, or its wire shape has moved. Not read as "nothing measured".',
    );
  }
  // A row off the shape is dropped from the judgement rather than fatal —
  // a shortened window still has a mean — but it is never dropped in
  // silence, and it is never dropped from `--json`. A gap in the tick
  // numbers has exactly one meaning here, a tick the loop did not finish,
  // and a row this reader hid would be a second one: the same rule the
  // Layout record's carrying vocabularies are printed under above. So the
  // count of hidden rows is said out loud beside the judgement, and a leaf
  // whose rows are all off the shape says how many are there rather than
  // reporting a bundle that has written nothing.
  const readable = (row) => row && typeof row.t === "number" && typeof row.ms === "number";
  const ticks = stored.ticks.filter(readable);
  const unreadable = stored.ticks.length - ticks.length;

  // The phase columns, in the order the loop reads them and spelt exactly
  // as `saveCpu` writes them. `entry` leads and is not a phase of the
  // bot's at all: it is what the engine had already spent by the time
  // `loop` was entered.
  const PHASES = ["entry", "snapshot", "decide", "save", "execute"];
  const COLUMNS = [...PHASES, "intents"];

  // `--json` is the stored rows, not the judged ones: the raw structure is
  // what a jq reader came for, and a row hidden from it could not be seen
  // anywhere at all — which is why the malformed-split guard below sits
  // inside the printed path and not above this branch. A row carrying half
  // a split is exactly the row an operator opens `--json` to find and
  // repair, and a guard that killed the dump first would leave it visible
  // nowhere.
  if (json) {
    console.log(JSON.stringify(stored.ticks, null, 2));
  } else if (stored.ticks.length === 0) {
    console.log("the CPU line is empty — the bundle has written no finished tick yet");
  } else if (ticks.length === 0) {
    console.log(
      `${stored.ticks.length} row${stored.ticks.length === 1 ? "" : "s"} at ` +
        "Memory.fabot.observe.cpu, none of them decodable — the leaf was hand-edited, or " +
        'its wire shape has moved. Not read as "nothing measured".',
    );
  } else {
    // The split is readable when absent and fatal when malformed, which is
    // #135's asymmetry rather than the one the totals above are dropped
    // under. A row from a bundle older than the split carries no phase key
    // at all, and printing its columns empty says something true about that
    // row. A row carrying half a split, or a phase that is not a number,
    // would print the same empty columns and say the same thing about a
    // bundle that is measuring right now — and Core keeps such a row while
    // dropping its group (`decodeCpuPhases`), so the silence here would be
    // this reader's invention. An own-key test rather than `row[key] !==
    // undefined`, for the reason spelt out in the `outposts` branch above:
    // every object answers `constructor` and `toString` with something.
    for (const row of ticks) {
      const named = COLUMNS.filter((key) => Object.hasOwn(row, key));
      if (named.length === 0) continue;
      if (named.length !== COLUMNS.length || named.some((key) => typeof row[key] !== "number")) {
        fail(
          `the phase split on the row for tick ${row.t} at Memory.fabot.observe.cpu is off the ` +
            `wire shape: ${JSON.stringify(row)} — the leaf was hand-edited, or its wire shape ` +
            'has moved. Not printed as "that tick was never split": the bundle writing this row ' +
            "measured the boundaries and this reader cannot say what they were. `--json` still " +
            "dumps the row.",
        );
      }
    }

    // Every key or none, by the guard above, so the first of them answers
    // for the group.
    const isSplit = (row) => Object.hasOwn(row, "entry");
    const split = ticks.filter(isSplit);

    const width = Math.max(4, ...ticks.map((row) => String(row.t).length));
    const ms = (value) => value.toFixed(3).padStart(8);
    // A split the row does not carry is a dash and never a zero: nobody
    // measured that phase, which is a different statement from measuring
    // it at nothing.
    const absent = "—".padStart(8);
    const cells = (row) =>
      isSplit(row)
        ? [...PHASES.map((key) => ms(row[key])), String(row.intents).padStart(8)]
        : COLUMNS.map(() => absent);

    console.log(
      [
        "tick".padStart(width),
        "total ms".padStart(8),
        ...COLUMNS.map((key) => key.padStart(8)),
      ].join("  "),
    );

    for (const row of ticks) {
      console.log([String(row.t).padStart(width), ms(row.ms), ...cells(row)].join("  "));
    }

    console.log("");

    // The window's phase means, which are the attribution the split was
    // built for: the engine's prelude, the shell's sweep (the Memory parse
    // rides in it), the decision, the observe folds and the Memory writes,
    // and the intents. Averaged over the rows that carry a split rather
    // than over the window, so a deploy's first hundred ticks do not divide
    // five phases by rows that have none.
    //
    // The intent mean is printed on a line of its own, under its own unit:
    // it is a count, and a sixth term on a line headed "mean ms" would read
    // as milliseconds — on the very readout this window is pasted from. The
    // engine's 0.2 CPU an intent is multiplied out here rather than left to
    // the reader, because that product is the candidate the local ruler
    // cannot simulate and the whole reason the count is on the row.
    if (split.length === 0) {
      console.log(
        `no row carries a phase split — the deployed bundle predates it, ` +
          "or it has not written a full window since",
      );
    } else {
      const mean = (key) => split.reduce((total, row) => total + row[key], 0) / split.length;
      console.log(
        `mean ms over ${split.length} split row${split.length === 1 ? "" : "s"}: ` +
          PHASES.map((key) => `${key} ${mean(key).toFixed(2)}`).join("  "),
      );
      console.log(
        `mean intents the engine accepted, per split row: ${mean("intents").toFixed(1)} — ` +
          `a count, not milliseconds; ≈ ${(mean("intents") * 0.2).toFixed(2)} CPU at the ` +
          "engine's 0.2 an intent, which the local ruler does not charge",
      );
    }

    console.log("");

    if (unreadable > 0) {
      console.log(
        `${unreadable} row${unreadable === 1 ? "" : "s"} off the wire shape, not judged — ` +
          "a hand-edit, or the shape has moved",
      );
      console.log("");
    }

    console.log(cpuReport(ticks));
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
      case "stalled":
        return "stalled: nobody this pass can name holds the tile";
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
