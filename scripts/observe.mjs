// One-shot CLI over the observe channels — the Transition log (ADR 0009),
// the Raid log (ADR 0028), the Layout record (ADR 0035), the CPU line
// (ADR 0041) and the breach log (#278): pull the observe subtree from
// `Memory.fabot.observe`, flip the verbose list remotely, or watch the
// console for a bounded window.
// `outposts` is the one read that needs a second endpoint — it also reads
// the server's clock, because shut-or-open has no answer without it, and
// it fails on that read alone where the others cannot.
//
// The server connection and its .env config are `screeps-api.mjs`'s.
//
// Usage:
//   observe.mjs tasks              every creep's current Task with its Verdict reason
//   observe.mjs timeline <creep>   one creep's Transition log, oldest first
//   observe.mjs raids              the Raid log's episodes, newest first
//   observe.mjs outposts           every outpost the Raid log knows: shut or
//                                  open right now, the tick a stand-down runs
//                                  to, the deadline that tick was read off, the
//                                  rooms another player took, shut by no clock,
//                                  and the rooms somebody else's reservation
//                                  stands on — mined, not reserved, withheld
//                                  from nothing
//   observe.mjs layout             what this colony could not deliver this tick:
//                                  the Layout's losses, and the declared outposts
//                                  that do not border this home
//   observe.mjs breaches           what is broken right now: one row per live
//                                  invariant violation, oldest first, each with
//                                  how long it has stood
//   observe.mjs quotas             the cascade's workforce arithmetic, row by row
//   observe.mjs reactor            the season programme and official score
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
// `raids`, `outposts`, `layout`, `quotas` and `breaches` are one colony's
// record and take `--colony <home>` to say whose (ADR 0047). Without it
// they read the first
// colony under `Memory.fabot.observe.colonies` — this script cannot see
// `Colony.declared`, so "first" is the first home the bot wrote a leaf for,
// which is declaration order because the loop writes in it.
import { connect, fail } from "./screeps-api.mjs";
import { report as cpuReport } from "./cpu-trigger.mjs";

const usage =
  "usage: observe.mjs tasks [--json] | timeline <creep> [--json] | " +
  "raids [--colony <home>] [--json] | outposts [--colony <home>] [--json] | " +
  "layout [--colony <home>] [--json] | quotas [--colony <home>] [--json] | " +
  "breaches [--colony <home>] [--json] | " +
  "reactor [--json] | cpu [--json] | " +
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
  ![
    "tasks",
    "timeline",
    "raids",
    "outposts",
    "layout",
    "quotas",
    "breaches",
    "reactor",
    "cpu",
    "verbose",
    "console",
  ].includes(command)
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
// The colony-keyed commands are exactly the channels that split by home
// (ADR 0047); anywhere else the flag would name a colony nothing reads.
if (rawArgs.includes("--colony")) {
  if (!["raids", "outposts", "layout", "quotas", "breaches"].includes(command)) fail(usage);
  // The flag eats the next argument, so a bare `--colony` at the end, or one
  // in front of `--json`, would silently read the default colony instead of
  // the one the operator asked for.
  if (colonyArg === undefined || colonyArg.startsWith("--")) {
    fail("--colony needs a home room name, e.g. --colony W12S28");
  }
}

const { api, shard } = await connect();

const memoryGet = async (path) => {
  const res = await api.userMemoryGet(path, shard).catch((err) => {
    fail(`memory read failed: ${err.message ?? err}`);
  });
  if (res.ok !== 1) fail(`memory read failed: ${JSON.stringify(res)}`);
  return res.data;
};

// One colony's leaf, under `Memory.fabot.observe.colonies.<home>` (ADR
// 0047): the channels that are a colony's record rather than the world's —
// the Raid log, the Layout record and, since #278, the breach log — are
// keyed by home room, because `decide` runs once per colony and each
// answers for the rooms its own colony works.
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
  //                  closest?: { range, room, x, y, t },
  //                  losses: [{ creep, t, room?, x?, y? }],
  //                  damage }],
  //     outposts: [{ room, opened, last, expiry, basis }],
  //     rivalHeld: { <room>: { since, lastLooked } },
  //     living: [creep], placed: { <creep>: { room, x, y } },
  //     hits: { <structure id>: hits } }
  // Stored oldest first like the Transition log's ring, printed newest
  // first. `closest` is simply absent when nothing of ours could be placed
  // — or, since #376, when no *armed* hostile stood where anything of ours
  // was — and since #216 R3 it names the room the approach was measured in
  // (#204): the episode is the colony's and names no room of its own (ADR
  // 0028), so a bare coordinate read as home's could not tell an
  // [[outpost]]'s raid from one at the door. An episode written before that
  // carries no room and prints the coordinate alone.
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
  // `rivalHeld` is that same command's other half — the rooms last seen
  // **owned** by another player, against the tick the gate shut on and the
  // tick of the last look into the room (#275), ADR 0043's withdrawal with no
  // clock (a rival's reservation is a clocked row of `outposts` since #165) —
  // and is no more a raid than a stand-down is.
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
      // Since #376 the approach is measured against **armed** hostiles alone,
      // so an episode of scouts and keepers-at-a-distance has none — but a row
      // written before #376 measured every hostile, and nothing on the row
      // says which bundle wrote it, so the line is not labelled; and a loss
      // carries the tile the body last stood on, printed grouped by room so
      // "8 reservers lost in W15S27" is a line and not an inference. A row
      // written before #376 has no tile and groups under "room unknown".
      console.log(
        e.closest
          ? `  closest approach: range ${e.closest.range} ` +
              `at ${e.closest.room ? `${e.closest.room} ` : ""}` +
              `(${e.closest.x},${e.closest.y}) on t${e.closest.t}`
          : "  closest approach: none — no armed hostile stood where anything of ours was placed",
      );
      const losses = e.losses ?? [];
      if (losses.length === 0) {
        console.log("  lost nothing");
      } else {
        const byRoom = new Map();
        for (const l of losses) {
          const key = l.room ?? "room unknown";
          if (!byRoom.has(key)) byRoom.set(key, []);
          byRoom.get(key).push(l);
        }
        console.log(`  lost ${losses.length}:`);
        for (const [room, rows] of byRoom) {
          const line = rows
            .map((l) => `${l.creep} (t${l.t}${l.room ? ` @${l.x},${l.y}` : ""})`)
            .join(", ");
          console.log(`    ${room}  ${rows.length}: ${line}`);
        }
      }
      console.log(`  damage: ${e.damage ?? 0} hits off the Keep and the ramparts`);
      console.log("");
    }
  }
} else if (command === "outposts") {
  // ---- outposts: the Raid log's second family, read as the gate reads it --

  // The wire shape written by ObserveMemory.fs, a key of its own beside
  // `episodes` in the same leaf:
  //   { outposts: [{ room, opened, last, expiry, basis }],
  //     rivalHeld: { <room>: { since, lastLooked } },
  //     holds: { <room>: { holder, until } } }
  // One `outposts` row per clocked [[stand-down]]: the room it shuts, the
  // window (opened, and the last tick the threat was actually seen there),
  // the absolute tick the stand-down runs to, and which deadline that tick
  // was read off — ADR 0043's three for an invader core, and since #165 a
  // fourth for another player's reservation, which is no threat but ends on
  // a tick the engine is counting down all the same. Stored oldest first
  // like the raids beside it.
  //
  // Shut or open is `now < expiry` and nothing else — Observe.standingDown,
  // the one place the family's openness is decided — so this command is a
  // read of the same rule the gate applies, never a second one. `last` is
  // deliberately not part of that test: the stand-down withdraws the very
  // creeps whose vision would see the core, so silence there says nobody is
  // looking and never that the room is clear.
  //
  // `rivalHeld` is ADR 0043's other withdrawal, and it has no row shape
  // because it has almost nothing to carry: a room another player **owns**
  // is not a threat with a deadline, it is a room that stopped being ours to
  // work, so the record is the room's name against the tick the last look
  // concluded it, and the gate withholds it with no clock to compare
  // against. Ownership alone since #165 — a rival's reservation decays at
  // one a tick and is a row of the clocked list above, where it says so in
  // its basis. `since` is still not a deadline; it is the date an income
  // drop is lined up against (#117's US-20), the answer the clocked family
  // gets from `opened`. The *stride* between looks — one look every
  // `Tuning.RivalRecheck` ticks (#165) — is counted from `lastLooked` beside
  // it, the tick the gate last handed a look out on (#275), so a tick the
  // loop never ran leaves the look owed instead of cancelling it. It is
  // a remembered conclusion: the fold writes it on the ticks with vision and
  // holds it through the ticks without, because the gate's own effect is to
  // take that vision away, and the stride exists because that effect would
  // otherwise make the conclusion permanent.
  //
  // `holds` is the third shape and the third thing a room can be (#333), and
  // it is neither of the two above: a controller under **somebody else's
  // reservation** — the NPC Invader's or another player's — withdraws no room
  // of its own. What the engine refuses is `reserveController` on that
  // controller and `createConstructionSite` in that room, so the reserver row
  // hires nobody for it and the room raises no container. The entry is whose
  // hold it is and the absolute tick it ends on, kept through the blind ticks
  // like `rivalHeld` and retiring itself on that tick like a clocked row,
  // because the engine is counting it down at one a tick.
  //
  // Whether the *room* is still worked depends on the holder, and the lines
  // below say which: the **Invader's** leftover hold withdraws nothing — the
  // room stays in the scan set, its rock stays pooled and the colony goes on
  // mining it — while a **rival's** identical reservation is also #165's
  // clocked stand-down, opened off the same control entry, so that room is
  // withheld as well and carries a row of the ring above saying so.
  //
  // It is printed because W12S27 spent 5,000 ticks being called `open` here
  // while the row bought it a 1,950-energy reserver every 600 of them: an
  // invader core collapsed, ADR 0043's stand-down ended with the core, and
  // the reservation the core had taken outlived it by CONTROLLER_RESERVE_MAX.
  // "Open" was a true statement about the stand-down and a false one about
  // the room, which is the only thing an operator reads it for.
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
  // And the third half, guarded in its own right for the third time (#333).
  // A leaf carrying the two maps above and no `holds` is a bundle predating
  // the read — one whose reserver row is still buying bodies for a controller
  // the engine refuses — and reading it as "nothing is held" would print
  // exactly the `open` this command stopped saying.
  if (stored.holds === null || typeof stored.holds !== "object" || Array.isArray(stored.holds)) {
    fail(
      `the Raid log at Memory.fabot.observe.colonies.${home}.raids carries no ` +
        "`holds` map — the deployed bundle predates #333's read of the reservation, the leaf " +
        'was hand-edited, or its wire shape has moved. Not read as "every outpost is ours to ' +
        'reserve".',
    );
  }
  // Each entry carries two ticks since #275 — `{ since, lastLooked }`: the
  // tick the gate shut on, which dates the withdrawal, and the tick the last
  // look into the room was taken on, which is what the stride to the next look
  // is measured from. A bare number is the shape written before that, and the
  // bot reads it the same way this does: that tick is the shutting and the only
  // look the record can vouch for. Either shape must yield two numbers.
  //
  // A third shape parts the two readers on purpose. The bot drops that entry
  // and keeps the rest of the leaf (ADR 0028's row-by-row degradation), which
  // un-latches the room and lets the next look with vision decide it again —
  // the safe direction for a colony that has to keep running. This command
  // stops instead: a diagnostic that guessed would print a date the bot never
  // read, and the one thing worse than no answer here is a confident wrong one.
  // Both readers refuse to *invent* a tick, which is the rule that matters; what
  // they do afterwards is what each is for.
  const rivalHeld = Object.entries(stored.rivalHeld).map(([room, entry]) => {
    const badShape = () =>
      fail(
        `the entry at Memory.fabot.observe.colonies.${home}.raids.rivalHeld.${room} is off the ` +
          `wire shape: ${JSON.stringify(entry)} — the leaf was hand-edited, or its wire ` +
          "shape has moved. " +
          'Not read as "that room is open": the bot is withholding a room this command ' +
          "cannot date.",
      );

    if (typeof entry === "number") {
      return { room, since: entry, lastLooked: entry };
    }
    // `return badShape()`, though `fail` exits and nothing after it runs:
    // control falling out of a call made for its side effect reads as a bug
    // every time it is re-read, and the reader should not have to go and check
    // that `fail` never returns to know this function does not.
    if (entry === null || typeof entry !== "object" || Array.isArray(entry)) {
      return badShape();
    }
    if (typeof entry.since !== "number") {
      return badShape();
    }
    const lastLooked = entry.lastLooked ?? entry.since;
    if (typeof lastLooked !== "number") {
      return badShape();
    }
    return { room, since: entry.since, lastLooked };
  });

  // Whose CLAIM parts a hold belongs to, exactly as `reservationHolderName`
  // spells it on the wire (Core's Verdicts.fs). `ours` is in the vocabulary
  // and never in this map — the bot records only the holds that are not ours —
  // so a leaf carrying one is a hand edit, and it is named rather than refused:
  // what the line says about such a room is still true of the room, and what
  // it says about the holder is what the leaf says.
  const HOLDER = {
    ours: "this colony's own CLAIM parts (which the bot never records here)",
    invader: "the NPC Invader — the hold a level-0 core takes and leaves behind it",
    rival: "another player's CLAIM parts",
  };

  // The rooms whose controller somebody else is standing on (#333), read the
  // way the clocked rows are: an entry off the wire shape stops the command
  // rather than being dropped. Same asymmetry, same reason — the bot drops
  // what it cannot decode, so the room an unreadable entry named is one the
  // reserver row may be buying bodies for right now, and this is the command
  // that would have said so.
  const holds = Object.entries(stored.holds).map(([room, entry]) => {
    const badShape = () =>
      fail(
        `the entry at Memory.fabot.observe.colonies.${home}.raids.holds.${room} is off the ` +
          `wire shape: ${JSON.stringify(entry)} — the leaf was hand-edited, or its wire ` +
          "shape has moved. " +
          'Not read as "that room is ours to reserve": the bot drops an entry it cannot ' +
          "decode, so this room may be costing a reserver a tick.",
      );

    if (entry === null || typeof entry !== "object" || Array.isArray(entry)) {
      return badShape();
    }
    // An own-key test for the reason the basis table gives: every object
    // literal answers `toString` and `constructor` with a function, so a
    // prototype name off the wire would read as a known holder here while
    // Core's decoder answers `None` and drops the entry.
    if (!Object.hasOwn(HOLDER, entry.holder) || typeof entry.until !== "number") {
      return badShape();
    }
    return { room, holder: entry.holder, until: entry.until };
  });

  // The guard row's memory of a raid in a room it has gone blind in (#366),
  // read **leniently on absence and strictly on shape** — which is the one
  // place this command parts from the three maps above, and for a reason of
  // its own. Absence here is a bundle that predates #366, and what such a
  // bundle does is hire its guards off vision alone: that is the behaviour
  // this command described correctly before the record existed, so failing the
  // whole readout over it would take an operator's only view of three other
  // families away to report a missing feature. A malformed *entry* still stops
  // the command, for `holds`' reason: the bot drops what it cannot decode, so
  // an unreadable entry is a room whose guard is not being hired right now and
  // this is the command that would have said so.
  if (
    stored.threatened !== undefined &&
    stored.threatened !== null &&
    (typeof stored.threatened !== "object" || Array.isArray(stored.threatened))
  ) {
    fail(
      `the Raid log at Memory.fabot.observe.colonies.${home}.raids carries a \`threatened\` ` +
        `entry that is not a map: ${JSON.stringify(stored.threatened)} — the leaf was ` +
        'hand-edited, or its wire shape has moved. Not read as "no outpost is remembered".',
    );
  }
  const threatened =
    stored.threatened === undefined || stored.threatened === null
      ? null
      : Object.entries(stored.threatened).map(([room, entry]) => {
          if (
            entry === null ||
            typeof entry !== "object" ||
            Array.isArray(entry) ||
            typeof entry.until !== "number"
          ) {
            fail(
              `the entry at Memory.fabot.observe.colonies.${home}.raids.threatened.${room} is ` +
                `off the wire shape: ${JSON.stringify(entry)} — the leaf was hand-edited, or ` +
                "its wire shape has moved. " +
                'Not read as "nothing is raiding that room": the bot drops an entry it cannot ' +
                "decode, so the guard bought for this room may be standing at home.",
            );
          }
          return { room, until: entry.until };
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
    "rival-reservation":
      "the end of the reservation another player holds — a room somebody else is " +
      "working, not a threat (#165)",
    // The fifth basis, and this table had four for the whole of its life
    // (#368): a raid with no core in it, withdrawn from only once the colony
    // has stopped fighting for the room — a guard of ours still standing there,
    // or casts left in the episode's ADR 0056 budget, means the room is a fight
    // and not a withdrawal. Its deadline is the raid's own longest remaining
    // life, that being all a coreless raid offers.
    "invader-raid":
      "an invader raid the colony has stopped fighting for — the deadline is the " +
      "raid's own remaining life (ADR 0056, ADR 0043)",
  };

  // What a row's `last` tick is the last sighting *of*, keyed off the same
  // basis and over the same keys, because the two families this ring now
  // holds are seen in different things: three of them by an invader core
  // standing in the room, and #165's fourth by a reservation read off the
  // controller with no core there at all.
  const SIGHTING = {
    "collapse-timer": "a core last seen there",
    reservation: "a core last seen there",
    fallback: "a core last seen there",
    "rival-reservation": "the reservation last read there",
    // A raid is seen as creeps and not as a structure, so what `last` dates
    // here is the last tick one of them was standing in the room (#368).
    "invader-raid": "a raider last seen there",
  };

  // The stride between looks into a latched room, mirroring
  // `Tuning.RivalRecheck` in Core (#165). Printed as the date of the next
  // look, and said to be that mirror rather than a fact off the server: a
  // bundle deployed under a different tuning would look on other ticks, and
  // this command has no way to read which.
  const RIVAL_RECHECK = 5000;

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
      // The vocabulary is exactly the names `standDownBasisName` spells —
      // and `SIGHTING` is keyed over the same ones, so a row this guard
      // passes has both clauses — and nothing the language put on the
      // tables beside them.
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
        {
          now,
          sector: SECTOR,
          outposts: stored.outposts,
          rivalHeld: stored.rivalHeld,
          holds: stored.holds,
          // `null` and not `{}` for a bundle that predates #366: "this bot
          // does not keep the record" and "it keeps it and remembers no raid"
          // are different answers, and one of them means the guard row is
          // still reading vision alone.
          threatened: stored.threatened ?? null,
        },
        null,
        2,
      ),
    );
  } else {
    console.log(`colony ${home}, now ${tickOf(now)}`);
    console.log("");

    // A hold whose tick has passed is a record the bot has not folded away
    // yet — the clock is the engine's and it ran out without anybody looking
    // (ADR 0043's re-entry rule), so the room is ours to reserve again and
    // saying otherwise would be this command's own version of the lie it was
    // written to stop. `now < until` and nothing else, which is
    // `standingDown`'s test over the other clock.
    const standing = holds.filter((hold) => now < hold.until);
    const reservedRooms = standing.map((hold) => hold.room);

    // Which rooms the gate is actually withholding right now — a running
    // clocked row, or the clockless latch. A hold is not one of them and says
    // so out loud, but the three families can name one room at once: a rival's
    // reservation is *both* a hold here and a clocked stand-down (#165), and a
    // hold line telling an operator "the room is mined" over a row telling him
    // it is withheld is the same kind of false line this ticket is about.
    const heldRooms = rivalHeld.map((held) => held.room);

    const shutNow = new Set([
      ...heldRooms,
      ...rows.filter((row) => now < row.expiry).map((row) => row.room),
    ]);

    if (rows.length === 0 && rivalHeld.length === 0 && standing.length === 0) {
      console.log("the Raid log records no stand-down: no outpost is shut");
      console.log("");
    } else {
      // Somebody else's reservation first, above both withdrawals, because it
      // is the one of the three that says nothing about the gate: the room is
      // worked, and an operator reading a clocked row or an `open` line under
      // it has to know that the reservation on that room is not ours before
      // either line means what it looks like.
      for (const hold of [...standing].sort((a, b) => a.room.localeCompare(b.room))) {
        console.log(
          `${hold.room}  mined, not reserved — somebody else's reservation stands on its ` +
            `controller until ${tickOf(hold.until)}, ${ticks(hold.until - now)} to go`,
        );
        console.log(`  held by ${HOLDER[hold.holder]}`);
        console.log(
          "  the engine refuses reserveController on a controller anybody else holds, and " +
            "createConstructionSite",
        );
        console.log(
          "  in the room with it — so the reserver row hires nobody for this room and it " +
            "raises no container (#333).",
        );
        // The row reads this same record, which is why the line above is a
        // statement about the row and not only about the engine: a tick with
        // vision answers for the room itself, and on every blind tick the
        // standing hold is what the Reserve pool and the reserver row are
        // narrowed by (`ColonyView.HeldOutposts`). Without that read the
        // refusal would take its own evidence away — the reserver is the only
        // body such a room ever holds — and the row would buy another the tick
        // after each one died.
        if (shutNow.has(hold.room)) {
          console.log(
            "  this room is withheld as well, by the line printed under this one — the hold " +
              "is not what withholds it",
          );
        } else {
          console.log(
            "  the room itself is not withheld: its rock is pooled and its bodies stand " +
              "in it. Its sources yield",
          );
          console.log("  the neutral five a tick until the hold runs out (ADR 0042).");
          console.log(
            `  Nothing need be done: the engine counts this down at one a tick. On ` +
              `${tickOf(hold.until)} this record retires itself and the row hires one body,`,
          );
          console.log(
            "  whose walk is what re-reads the controller — so a holder that has renewed " +
              "costs that one body and is",
          );
          console.log("  written down again, not a reserver every 600 ticks.");
        }
        console.log("");
      }

      // Then the clockless withdrawals, and they answer for their room
      // whatever the clocked rows say about it: a room another player holds
      // is shut by a rule with no expiry, so a spent stand-down sitting in
      // the ring beside it must not print that room as open.
      //
      // The clocked rows for the same room are printed under this line and
      // not filtered away with it: a core and a rival's claimer can take one
      // controller on one tick, and an operator told "no clock is running"
      // while the log holds an expiry and a basis has lost exactly the trace
      // ADR 0043 asks this channel to keep (#117's US-20).
      for (const held of [...rivalHeld].sort((a, b) => a.room.localeCompare(b.room))) {
        console.log(`${held.room}  shut since ${tickOf(held.since)}, and no clock is running`);
        console.log("  because another player owns it — not a threat that passes,");
        console.log("  a room that stopped being ours to work (ADR 0043)");
        // How this one ends, and since #165 the colony has a part in it: the
        // gate re-admits the room to the **scan** for one tick once a whole
        // `Tuning.RivalRecheck` has passed since the last look, and a look that
        // finds no owner
        // clears the latch. It is a look and not a return — the room is in no
        // pool and no quota on that tick either — and it only tells us
        // anything if something of ours can see the room, which the withdrawal
        // itself makes unlikely. So the hand-edit is still worth naming, and is
        // now a shortcut rather than the only way back.
        //
        // The stride is counted from the last look and not from the shutting
        // (#275), so the next one is that tick plus the stride flat: a look is
        // owed from there onwards and the first tick the gate is evaluated on
        // pays it, where an exact-multiple test lost a whole stride to every
        // tick the loop missed. Already past means the look is owed now.
        const nextLook = held.lastLooked + RIVAL_RECHECK;
        console.log(
          `  the gate looks again at ${tickOf(nextLook)}${nextLook <= now ? " — owed now" : ""}` +
            ` — one look every ${ticks(RIVAL_RECHECK)} at the earliest, counted from the last ` +
            `look at ${tickOf(held.lastLooked)} (#165, #275); the room is scanned`,
        );
        console.log("  on that tick and worked on none, and a look finding no owner clears it");
        console.log(
          "  — but only if something of ours can see the room, which is what the " +
            "withdrawal took away.",
        );
        console.log(
          `  to end it now, clear "${held.room}" from ` +
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

        // A room either of the halves above already answered for gets its
        // clocked rows as a continuation of that answer rather than a second
        // headline. For the clockless withdrawal because the gate above it has
        // no expiry to run out, so "open" here would contradict the line four
        // rows up; for a hold (#333) because "open" is a statement about the
        // stand-down that an operator reads as a statement about the room, and
        // the room is one the colony is mining and not reserving. The clocked
        // rows still print: W12S27's spent stand-down is exactly the row that
        // dates the core whose reservation is still standing.
        const alsoHeld = heldRooms.includes(room) || reservedRooms.includes(room);

        if (running.length > 0) {
          const row = running[0];
          console.log(
            alsoHeld
              ? `${room}  and a stand-down is recorded too, until ${tickOf(row.expiry)} — ` +
                  `${ticks(row.expiry - now)} to go`
              : `${room}  shut until ${tickOf(row.expiry)} — ${ticks(row.expiry - now)} to go`,
          );
          console.log(`  because ${BASIS[row.basis]}`);
          // What `last` is a sighting *of* is the basis's, not the family's:
          // since #165 a row of this ring can be opened by a reservation read
          // off a controller with no core anywhere near it, and naming a core
          // under such a row contradicts the `because` line printed above it.
          console.log(`  opened ${tickOf(row.opened)}, ${SIGHTING[row.basis]} ${tickOf(row.last)}`);
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

    // The guard row's memory, printed under the gate's own families because it
    // is not one of them: it withholds nothing, and what it says about a room
    // is that the colony is still hiring for a fight it can no longer see
    // (#366). Standing entries alone, on `now < until`, which is the test the
    // bot's own gate filters by — an entry whose tick has passed is one the
    // fold has not caught up with and no guard is being hired off it.
    if (threatened === null) {
      console.log(
        "the deployed bundle predates #366: it keeps no memory of a raid in a room it has " +
          "gone blind in, so its guard row reads vision alone.",
      );
      console.log("");
    } else {
      const remembered = threatened
        .filter((latch) => now < latch.until)
        .sort((a, b) => a.room.localeCompare(b.room));

      for (const latch of remembered) {
        console.log(
          `${latch.room}  guarded from memory — an armed threat was standing here at the last ` +
            `look, and nothing of ours can see the room now`,
        );
        console.log(
          `  the guard row hires one body for it until ${tickOf(latch.until)}, ` +
            `${ticks(latch.until - now)} to go (#366)`,
        );
        console.log(
          "  the room is not withheld: its rock is pooled and its Tasks are offered. The " +
            "first tick anything of ours",
        );
        console.log(
          "  sees the room again decides it either way — a threat still standing writes the " +
            "memory forward, a clear room ends it.",
        );
        console.log("");
      }
    }

    // The rooms this command cannot name. The declared outposts are a
    // constant in Core a human moves (ADR 0041) and no Memory leaf carries
    // them, so a room that has never stood down has no row here and cannot
    // be listed as open — said out loud rather than left to be read as
    // "these are all of them".
    console.log(
      "rows are the stand-downs the log holds, and the holds beside them are the rooms last " +
        "seen under somebody else's reservation;",
    );
    console.log(
      "a declared outpost that has never been shut and is ours to reserve has neither, and " +
        "is not named above.",
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
} else if (command === "quotas") {
  // ---- quotas: the cascade's workforce arithmetic ------------------------
  // Wire shape written by ObserveMemory.saveQuotas:
  //   { target, living, casting, rows: [{ row, quota, living, casting }] }
  // One tick's reading, no history. rows: [] with target 0 is the cascade's
  // silence — a spawn's doorstep stood inside a Reach — and is said as such
  // rather than as "nothing to hire".
  const { home, stored } = await colonyLeaf("quotas");
  if (!Array.isArray(stored.rows)) {
    fail(
      `the quotas record at Memory.fabot.observe.colonies.${home}.quotas carries no \`rows\` list — ` +
        "the leaf was hand-edited, or its wire shape has moved.",
    );
  }
  if (json) {
    console.log(JSON.stringify({ home, ...stored }, null, 2));
  } else {
    console.log(`colony ${home}`);
    if (stored.rows.length === 0) {
      console.log("  cascade silent: a spawn's doorstep is inside a Reach (ADR 0033)");
    } else {
      console.log(
        `  target ${stored.target}  living ${stored.living}  casting ${stored.casting}  ` +
          `deficit ${Math.max(0, stored.target - stored.living - stored.casting)}`,
      );
      console.log("  row        quota  living  casting  gap");
      for (const r of stored.rows) {
        const gap = Math.max(0, r.quota - r.living - r.casting);
        console.log(
          `  ${String(r.row).padEnd(9)} ${String(r.quota).padStart(6)} ${String(r.living).padStart(7)} ` +
            `${String(r.casting).padStart(8)} ${String(gap).padStart(4)}`,
        );
      }
      if (Array.isArray(stored.haul)) {
        // The quota is the hauler row's own number, never re-derived here: the
        // mine rounds apart from the energy (#403), and the floor and the ferry
        // sit on top, none of which this sum can see.
        const sum = stored.haul.reduce((a, r) => a + (r.demand ?? 0), 0);
        const hauler = stored.rows.find((r) => r.row === "hauler");
        console.log(
          `  haul: ${sum} demand over a ${stored.load}-energy load; hauler quota ${hauler ? hauler.quota : "?"}`,
        );
        for (const r of stored.haul) {
          const sinks = (r.sinks ?? [])
            .map((k) => `${k.kind} ${k.trip == null ? "unreachable" : k.trip + "t"}`)
            .join(", ");
          console.log(`    ${r.room} (${r.x},${r.y}) output ${r.output}/t → ${sinks}: demand ${r.demand}`);
        }
      }
    }
  }
} else if (command === "layout") {
  // ---- layout: what the Layout could not deliver ------------------------

  // The wire shape written by ObserveMemory.fs:
  //   { unserved: [{ x, y, kind }],
  //     unrouted: [{ source, goal, spawn? }],
  //     deferred: [{ target, source?, pick: { x, y }, serving: { x, y } }],
  //     refused: [{ room, kind }] }
  // Four lists in one leaf, the colony's losses of this tick: the footing
  // targets the fold found no tile for (#77), the trunks the router found
  // no path for (#107), the container picks the plan gave up because
  // something already serves their target (ADR 0040), and the declarations
  // no chain of Seams joins to this home (#243, #259, ADR 0060) — that last
  // one the declaration's loss rather than the Layout's, on this channel
  // because it is the same kind of answer: colony-level, this tick's, and
  // with no creep for a Verdict to name. Each of its rows says the room
  // **and the kind it was declared as**, because there are two kinds now —
  // an outpost a colony mines and an errand it walks a body to for one named
  // object — and the operator's next act is to move one of two lists. The
  // current plan's record, not a history: no ring,
  // no fold, the same lists every tick under a stable census. What a list
  // can say is three distinct answers and every one of them matters (ADR
  // 0035). A missing leaf is a missing channel — a bundle
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
  const refused = listOrFail("refused");
  // A refusal used to be a bare room name and is now `{ room, kind }` (ADR
  // 0060 decision 1). A bundle that predates the kind writes strings here,
  // and printing one as "(no kind)" beside a room would be this channel
  // reporting a confident half-answer off a stale deploy — the very thing
  // `listOrFail` above exists to refuse. So a row of the old shape fails as
  // loudly as a missing list does, and says which deploy it came from.
  //
  // **Both halves, and the kind read through the vocabulary and not merely
  // typechecked.** `declarationKindName` is a closed table (`Verdicts.fs`,
  // round-tripped in `WireTests`), so a row whose kind is a string this
  // channel does not know is a wire shape that has moved under it — and the
  // operator's whole next act is to open one of the two lists this word
  // names. Guarding `room` alone and then printing `kind ?? "(no kind)"`
  // would be the confident half-answer the paragraph above refuses, written
  // three lines under it.
  const declarationKinds = ["outpost", "errand"];
  for (const entry of refused) {
    if (
      typeof entry !== "object" ||
      entry === null ||
      typeof entry.room !== "string" ||
      !declarationKinds.includes(entry.kind)
    ) {
      fail(
        `the Layout record at Memory.fabot.observe.colonies.${home}.layout carries a ` +
          `\`refused\` row that is not { room, kind } with kind one of ` +
          `${declarationKinds.join("/")}: ${JSON.stringify(entry)}. The bundle ` +
          "deployed there predates ADR 0060's declaration kinds, or the leaf was hand-edited. " +
          'Not read as "an outpost".',
      );
    }
  }

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
    console.log(JSON.stringify({ unserved, unrouted, deferred, refused }, null, 2));
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

    // A row here is a room a human declared and this colony cannot reach: a
    // walk is priced over a chain of at most `Tuning.MaxHops` Seams (ADR
    // 0058), so nothing in a room no such chain joins can be priced, walked
    // to or worked, and the colony refuses it rather than hiring bodies that
    // would stand by the spawn for their whole lives (#243, #259). The fix is
    // a human's — move the declaration in `Colony.declared`, in whichever of
    // the two lists its kind names — and it is not the bot's to make. An
    // empty list says every declaration has a chain over the world's own
    // border rings, which is the question `Outpost.routable` and
    // `Errand.routable` both ask.
    if (refused.length === 0) {
      console.log(`every declared outpost and errand is joined to ${home} by a chain of Seams`);
    } else {
      console.log(
        `${refused.length} declaration${refused.length === 1 ? "" : "s"} no chain reaches from ${home}:`,
      );
      for (const entry of refused) {
        console.log(
          `  ${entry.room}  ${entry.kind} — no chain of Seams reaches it, ` +
            "so it is worked by nobody",
        );
      }
    }
  }
} else if (command === "breaches") {
  // ---- breaches: the live invariant violations, oldest first ------------

  // The wire shape written by ObserveMemory.fs:
  //   { rows: [{ kind, room, subject, amount, first, last }] }
  // One row per violation **standing right now**, oldest first, each
  // carrying the tick it opened on and the tick the bot last confirmed it
  // (#278). A breach that clears drops out of the leaf entirely: this
  // channel answers "what is broken now", and the episodic reading of the
  // same rooms is the Raid log's.
  //
  // So the ages are read off the row itself — `last - first` — and this
  // command reads no game clock, unlike `outposts`. It does not need one:
  // every standing row was confirmed on the tick the bot last wrote, and
  // that tick is `last`. What a clock would add is whether the *bundle* is
  // still running, which `cpu` answers and this does not pretend to.
  //
  // The channel exists because the only feedback loop the Thorium
  // programme had was a human polling this API by hand. Four incidents
  // shipped green through the test suite — the suite runs on fixtures this
  // repo authors, so it confirms the code's belief about the projection —
  // and each of them bled score for hours before anybody looked: a draw
  // gate reading a store off a map `World` never writes (915 T on a floor),
  // a delivery interval outrunning the burn, and ore in a room no rule
  // could name.
  const { home, stored } = await colonyLeaf("breaches");
  if (!Array.isArray(stored.rows)) {
    fail(
      `the breach log at Memory.fabot.observe.colonies.${home}.breaches carries no \`rows\` ` +
        "list — the leaf was hand-edited, or its wire shape has moved. " +
        'Not read as "nothing is broken".',
    );
  }

  // One clause per kind, exactly as `breachKindName` spells them on the
  // wire (ObserveMemory.fs), and what the amount on that row counts. A
  // closed table for the reason the `outposts` command's bases are one: the
  // key comes off the wire, the bot drops a row whose kind it cannot read,
  // and a row this reader guessed at would describe a violation nobody
  // wrote.
  const KIND = {
    "ore-on-the-floor": "the T of ore lying on the floor of a room this colony sweeps",
    "ore-unplaceable": "the T aboard a body at the Reactor that the Reactor has no room for",
    "reactor-running-dry":
      "the ticks of burn left in the declared Reactor, with no courier alive to reach it in time " +
      "(#361) — the one row here worth answering the tick it appears, because its whole value is " +
      "arriving before the streak breaks",
    "reactor-starved":
      "0 — the declared Reactor of ours is standing empty, and its continuous-work streak with it",
    "reactor-lost":
      "0 — the declared Reactor's own row says it is not ours, so what is delivered there scores " +
      "for whoever holds it",
  };

  // A row off the wire shape stops the command and is quoted, never
  // dropped — the asymmetry the Layout record's `refused` rows are printed
  // under, and for the same reason. Core drops a row it cannot decode, so
  // an unreadable row is one this command would otherwise hide while the
  // colony is standing in it.
  const rows = stored.rows.map((row) => {
    if (
      row === null ||
      typeof row !== "object" ||
      Array.isArray(row) ||
      // An own-key test and never `KIND[row.kind] !== undefined`: every
      // object literal answers `toString` and `constructor` with a
      // function, so a prototype name off the wire would read as a known
      // kind here while Core's decoder answers `None` and drops the row.
      !Object.hasOwn(KIND, row.kind) ||
      typeof row.room !== "string" ||
      typeof row.subject !== "string" ||
      typeof row.amount !== "number" ||
      typeof row.first !== "number" ||
      typeof row.last !== "number"
    ) {
      fail(
        `a row at Memory.fabot.observe.colonies.${home}.breaches.rows is off the wire shape: ` +
          `${JSON.stringify(row)} — the leaf was hand-edited, or its wire shape has moved. ` +
          'Not read as "that one is fine": the bot drops a row it cannot decode, so this may be ' +
          "a violation nothing else will tell you about.",
      );
    }
    return row;
  });

  const tickOf = (t) => `t${t.toLocaleString("en-US")}`;
  const ticks = (n) => `${n.toLocaleString("en-US")} tick${n === 1 ? "" : "s"}`;

  if (json) {
    console.log(JSON.stringify(stored.rows, null, 2));
  } else {
    console.log(`colony ${home}`);
    console.log("");
    if (rows.length === 0) {
      console.log(
        "no breaches: every check this channel runs held on the tick the bot last wrote",
      );
      console.log(
        "  — which is a statement about what the colony could see, never about what is true " +
          "in a room it is blind in (ADR 0004)",
      );
    } else {
      // Stored oldest first by Core (`breachRows`), and printed in that
      // order rather than re-sorted here: one ordering, in one place, under
      // test. The age is what turns a list into a priority — a pile a
      // courier is three ticks from picking up and a pile that is bleeding
      // read exactly alike without it.
      console.log(`${rows.length} breach${rows.length === 1 ? "" : "es"} standing, oldest first:`);
      const kindWidth = Math.max(...rows.map((row) => row.kind.length));
      const roomWidth = Math.max(...rows.map((row) => row.room.length));
      const subjectWidth = Math.max(...rows.map((row) => row.subject.length));
      for (const row of rows) {
        console.log(
          `  ${row.kind.padEnd(kindWidth)}  ${row.room.padEnd(roomWidth)}  ` +
            `${row.subject.padEnd(subjectWidth)}  amount ${String(row.amount).padStart(4)}  ` +
            `standing ${ticks(row.last - row.first)} ` +
            `(since ${tickOf(row.first)}, last read ${tickOf(row.last)})`,
        );
      }
      console.log("");
      // What each kind that actually appeared means, once rather than per
      // row: the rows are the priority list and stay one line each, and the
      // sentence explaining a kind does not get longer the more piles are
      // on the floor.
      for (const kind of [...new Set(rows.map((row) => row.kind))]) {
        console.log(`  ${kind}: amount is ${KIND[kind]}`);
      }
    }
  }
} else if (command === "reactor") {
  // ---- reactor: the season programme and official score ----------------
  const stored = await memoryGet("fabot.observe.reactor");
  const number = (key, nullable = false) =>
    typeof stored?.[key] === "number" || (nullable && stored?.[key] === null);
  const owner =
    stored?.owner === "ours" || stored?.owner === "none"
      ? stored.owner
      : typeof stored?.owner === "string" &&
          stored.owner.startsWith("rival:") &&
          stored.owner.length > "rival:".length
        ? stored.owner.slice("rival:".length)
        : null;

  if (
    stored == null ||
    typeof stored !== "object" ||
    Array.isArray(stored) ||
    owner === null ||
    !number("storeT") ||
    !number("continuousWork") ||
    !number("seen", true) ||
    !number("bankedT") ||
    !number("lastDelivery", true) ||
    !number("dryTicks")
  ) {
    fail(
      "the Reactor record at Memory.fabot.observe.reactor is absent or off its seven-field " +
        "wire shape — the deployed bundle predates it, or the leaf was hand-edited.",
    );
  }

  const clock = await api.gameTime(shard).catch((err) => {
    fail(`game time read failed: ${err.message ?? err}`);
  });
  if (clock.ok !== 1 || typeof clock.time !== "number") {
    fail(`game time read failed: ${JSON.stringify(clock)}`);
  }

  const me = await api.authMe().catch((err) => {
    fail(`authenticated-user read failed: ${err.message ?? err}`);
  });
  if (me.ok !== 1 || typeof me.username !== "string") {
    fail(`authenticated-user read failed: ${JSON.stringify(me)}`);
  }

  // The seasonal endpoint caps a page at twenty. `search` narrows the normal
  // case to one row; paging remains explicit so a looser server-side match can
  // never hide the exact authenticated username beyond the first page.
  const limit = 20;
  let offset = 0;
  let scoreRow;
  while (scoreRow === undefined) {
    const page = await api
      .req("GET", "/api/scoreboard/list", {
        limit,
        offset,
        search: me.username,
      })
      .catch((err) => fail(`scoreboard read failed: ${err.message ?? err}`));

    if (
      page.ok !== 1 ||
      !Array.isArray(page.users) ||
      page.meta == null ||
      typeof page.meta.length !== "number" ||
      page.users.some(
        (row) =>
          row == null ||
          typeof row !== "object" ||
          typeof row.username !== "string" ||
          typeof row.rank !== "number" ||
          (row.score !== undefined && typeof row.score !== "number"),
      )
    ) {
      fail(`scoreboard read returned an unexpected shape: ${JSON.stringify(page)}`);
    }

    scoreRow = page.users.find((row) => row.username === me.username);
    offset += page.users.length;
    if (scoreRow === undefined && (page.users.length === 0 || offset >= page.meta.length)) {
      fail(`scoreboard carries no exact row for authenticated user ${JSON.stringify(me.username)}.`);
    }
  }

  const score = scoreRow.score ?? 0;
  const age = stored.seen === null ? null : Math.max(0, clock.time - stored.seen);
  const result = {
    ...stored,
    owner,
    freshness: age === null ? "never-seen" : age <= 1 ? "fresh" : "stale",
    age,
    scoreboard: { username: scoreRow.username, rank: scoreRow.rank, score },
  };

  if (json) {
    console.log(JSON.stringify(result, null, 2));
  } else {
    const freshness =
      age === null
        ? "never seen"
        : age <= 1
          ? `fresh — seen at t${stored.seen}`
          : `STALE by ${age} ticks — last seen at t${stored.seen}`;
    const delivered = stored.lastDelivery === null ? "never" : `t${stored.lastDelivery}`;

    console.log(`Reactor programme — ${freshness}`);
    console.log(`  owner ${owner}`);
    console.log(`  store ${stored.storeT} T  continuous work ${stored.continuousWork} ticks`);
    console.log(`  banked ${stored.bankedT} T  last delivery ${delivered}`);
    console.log(`  dry ticks ${stored.dryTicks}`);
    console.log(`season scoreboard — ${scoreRow.username}: rank ${scoreRow.rank}, score ${score}`);
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
  // Two counts ride beside them (#357) and neither is a duration: the bucket
  // the engine had banked for us as the tick ended, and how many colonies threw
  // their plan memo away in it. They are what turn a spike from a number into a
  // diagnosis — a tick may spend `limit + bucket` capped at 500 ms, so the
  // margin says whether a 339 ms tick was survivable, and the replan count says
  // whether `decide` was replanning or pricing.
  const COUNTS = ["intents", "bucket", "replans"];
  const COLUMNS = [...PHASES, ...COUNTS];

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
    // The flood counts (#389): `{ home: [floods, free, pops] }` per row,
    // summed here into one `pops` column because pops are the flood's unit
    // of work and the one number that says whether a `decide` spike was
    // flooding at all. Per colony they are reported below with the other
    // splits. A row without the key is a row an older bundle wrote, and
    // prints the dash for the split's reason.
    const isTriple = (triple) =>
      Array.isArray(triple) && triple.length === 3 && triple.every((n) => typeof n === "number");
    const floodsOf = (row) =>
      row.floods && typeof row.floods === "object"
        ? Object.values(row.floods)
            .filter(isTriple)
            .reduce(
              (total, [floods, free, pops]) => ({
                floods: total.floods + floods,
                free: total.free + free,
                pops: total.pops + pops,
              }),
              { floods: 0, free: 0, pops: 0 },
            )
        : null;
    // Heap and memo rows (#391): a climb a reset cures shows here before it
    // shows in the mean. A legacy row has neither key and prints the dash.
    const numberCell = (row, key, digits) =>
      typeof row[key] === "number" ? row[key].toFixed(digits).padStart(8) : absent;
    const popsCell = (row) => {
      const counted = floodsOf(row);
      return counted ? String(counted.pops).padStart(8) : absent;
    };
    const tail = (row) => [
      popsCell(row),
      numberCell(row, "heap", 1),
      numberCell(row, "ext", 1),
      numberCell(row, "rows", 0),
    ];
    const cells = (row) =>
      isSplit(row)
        ? [...PHASES.map((key) => ms(row[key])), ...COUNTS.map((key) => String(row[key]).padStart(8)), ...tail(row)]
        : [...COLUMNS.map(() => absent), ...tail(row)];

    console.log(
      [
        "tick".padStart(width),
        "total ms".padStart(8),
        ...COLUMNS.map((key) => key.padStart(8)),
        "pops".padStart(8),
        "heap MB".padStart(8),
        "ext MB".padStart(8),
        "rows".padStart(8),
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

      // The margin, and the two ways it is read: where it stands now, and
      // whether the window as a whole is banking or spending. A mean tick under
      // the limit refills what the spikes withdraw, and that arithmetic — not
      // the spike's own size — is what says whether this is survivable.
      const last = split[split.length - 1];
      const drawn = split
        .filter((row) => row.ms > 100)
        .reduce((total, row) => total + row.ms - 100, 0);
      const banked = split
        .filter((row) => row.ms <= 100)
        .reduce((total, row) => total + 100 - row.ms, 0);
      console.log(
        `bucket ${last.bucket} as of t${last.t}; over this window the ticks over the ` +
          `100 ms limit drew ${drawn.toFixed(0)} and the ticks under it banked ` +
          `${banked.toFixed(0)} — ${banked >= drawn ? "net refill" : "NET DRAIN"}`,
      );
      console.log(
        `replans: ${split.reduce((total, row) => total + row.replans, 0)} over the window, ` +
          `${split.filter((row) => row.replans > 0).length} tick(s) with at least one — ` +
          "a colony that threw its plan memo away re-planned its whole layout in that tick (ADR 0033)",
      );

      // Which colony the `decide` phase went into (#370). Printed per colony
      // and never summed into one line, because the whole reason the reading
      // exists is that four colonies are four shapes and a profile has to be
      // taken of one of them: every CPU refusal this bot has recorded was
      // measured on the wrong shape, #332's keeper mask reading 0.00% on the
      // one harness world with no keeper room.
      //
      // The rows that carry no split are counted rather than dropped, for
      // `KIND`'s reason: a reader that silently ignored them would report a
      // window of four rows as a window of a hundred, and the deployed bundle
      // predating this reading is the ordinary case for the first hundred ticks
      // after every upload.
      // One reader for both splits, because they are the same shape under two
      // keys: the colonies' share of `decide` and the rooms' share of
      // `snapshot` (#370). Written once rather than twice for the reason the
      // writer gives — a second copy is where the two drift apart.
      const attributedBy = (key) =>
        split.filter((row) => row[key] && Object.keys(row[key]).length);
      const report = (key, label, phase, footer) => {
        const rows = attributedBy(key);
        if (rows.length === 0) {
          console.log(
            `no row says which ${label} spent \`${phase}\`: the deployed bundle predates the ` +
              `per-${label} reading, or nothing has been ${phase === "decide" ? "decided" : "swept"} since it landed`,
          );
          return;
        }
        const names = [...new Set(rows.flatMap((row) => Object.keys(row[key])))];
        console.log(
          `${phase} by ${label} over ${rows.length} attributed row${rows.length === 1 ? "" : "s"}` +
            `${rows.length < split.length ? ` (of ${split.length} split)` : ""}:`,
        );
        for (const name of names) {
          const ms = rows
            .filter((row) => typeof row[key][name] === "number")
            .map((row) => row[key][name]);
          const mean = ms.reduce((total, one) => total + one, 0) / ms.length;
          console.log(
            `  ${name}  mean ${mean.toFixed(2)} ms  max ${Math.max(...ms).toFixed(2)}  ` +
              `over ${ms.length} tick${ms.length === 1 ? "" : "s"}`,
          );
        }
        console.log("  " + footer);
      };

      // The head beside the rooms, and the tail by subtraction: the row carries
      // `head` because the head is one number the writer knows exactly, while
      // the tail is whatever the phase has left after the last room — so one is
      // read and the other is derived, and the line says which is which.
      const headed = split.filter((row) => typeof row.head === "number");

      if (headed.length) {
        const heads = headed.map((row) => row.head);
        const sweeps = headed.map((row) =>
          Object.values(row.rooms ?? {}).reduce((total, one) => total + one, 0),
        );
        const mean = (xs) => xs.reduce((total, one) => total + one, 0) / xs.length;
        const projected = headed.map((row) =>
          Object.values(row.projects ?? {}).reduce((total, one) => total + one, 0),
        );
        const tails = headed.map(
          (row, i) => (row.snapshot ?? 0) - row.head - sweeps[i] - projected[i],
        );
        console.log(
          `snapshot split over ${headed.length} row${headed.length === 1 ? "" : "s"}: ` +
            `head ${mean(heads).toFixed(2)} ms  rooms ${mean(sweeps).toFixed(2)}  ` +
            `projections ${mean(projected).toFixed(2)}  rest ${mean(tails).toFixed(2)}`,
        );
        console.log(
          "  head is the visible rooms enumerated and every creep grouped by where it stands; " +
            "the rest is the world's tail between the last room and the first projection — the " +
            "sightings, the creep list, the Raid logs. Head, rooms and projections are read off " +
            "the row and the rest is what the column has left over, so a negative rest means the " +
            "phase gained work this reading cannot see",
        );
      }

      // What each colony's decision flooded (#389), printed the way the
      // millisecond splits are and never summed into one line, for the same
      // reason: the reading exists to say *which* colony a spike came out
      // of. A tick's pops several times the window's floor is a flood
      // storm in that colony; pops flat across a spike is a spike that was
      // not flooding, and the phase split says where else to look.
      const counted = split.filter((row) => floodsOf(row));

      if (counted.length === 0) {
        console.log(
          "no row carries flood counts: the deployed bundle predates the reading (#389), or " +
            "nothing has been decided since it landed",
        );
      } else {
        const homes = [...new Set(counted.flatMap((row) => Object.keys(row.floods)))];
        console.log(
          `floods by colony over ${counted.length} counted row${counted.length === 1 ? "" : "s"}` +
            `${counted.length < split.length ? ` (of ${split.length} split)` : ""}:`,
        );
        for (const home of homes) {
          const triples = counted.map((row) => row.floods[home]).filter(isTriple);
          if (triples.length === 0) continue;
          const mean = (i) => triples.reduce((total, t) => total + t[i], 0) / triples.length;
          const maxPops = Math.max(...triples.map((t) => t[2]));
          const worst = counted.find((row) => isTriple(row.floods[home]) && row.floods[home][2] === maxPops);
          console.log(
            `  ${home}  floods ${mean(0).toFixed(1)} (${mean(1).toFixed(1)} free)  ` +
              `pops mean ${mean(2).toFixed(0)}  max ${maxPops} at t${worst.t}  ` +
              `over ${triples.length} tick${triples.length === 1 ? "" : "s"}`,
          );
        }
        console.log(
          "  a flood is one Dijkstra over a room's grid and a pop is its unit of work; free floods " +
            "start at a creep's own tile, the rest are seeded — a far field, a Seam walk, a cast " +
            "leg. Pops several times the mean on a spike tick is the spike; pops flat is a spike " +
            "that was not flooding",
        );
      }

      report(
        "projects",
        "colony",
        "project",
        "the projections stand inside the `snapshot` column, after the last room is swept: this " +
          "is a colony's cut of the world (ADR 0047), and unlike the sweeps beside it, it is our " +
          "own code rather than the engine's",
      );

      report(
        "rooms",
        "room",
        "snapshot",
        "a room appears here once however many colonies project it: the world holds one set of " +
          "facts per room (ADR 0052 decision 1), so this is the price of the sweep and not of the " +
          "projections that read it. The rooms sum to less than the `snapshot` column by the " +
          "sweep's head and tail — enumerating the visible rooms, grouping every creep by where " +
          "it stands, and merging the sightings — which is left readable here rather than " +
          "charged to whichever room happens to be swept first",
      );

      const attributed = attributedBy("colonies");

      if (attributed.length === 0) {
        console.log(
          "no row says which colony decided: the deployed bundle predates the per-colony " +
            "reading, or no colony has decided since it landed",
        );
      } else {
        const homes = [...new Set(attributed.flatMap((row) => Object.keys(row.colonies)))];
        const spent = (home) =>
          attributed
            .filter((row) => typeof row.colonies[home] === "number")
            .map((row) => row.colonies[home]);
        console.log(
          `decide by colony over ${attributed.length} attributed row${attributed.length === 1 ? "" : "s"}` +
            `${attributed.length < split.length ? ` (of ${split.length} split)` : ""}:`,
        );
        for (const home of homes) {
          const ms = spent(home);
          const mean = ms.reduce((total, one) => total + one, 0) / ms.length;
          const worst = Math.max(...ms);
          console.log(
            `  ${home}  mean ${mean.toFixed(2)} ms  max ${worst.toFixed(2)}  ` +
              `over ${ms.length} tick${ms.length === 1 ? "" : "s"}`,
          );
        }
        console.log(
          "  the sum falls short of the `decide` column by the movement arbitration and the " +
            "two Memory reads `decide` is handed, which is why neither is derived from the other",
        );
      }
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

    // The coarse spans (#386): the fine window above is a hundred ticks, about
    // five minutes, and two outages in two days were hours old before anyone
    // read this channel. What these answer is *when* — a span whose `max` went
    // large, or whose bucket floor fell, against one that did not.
    //
    // `max` leads because the engine's per-tick ceiling is a wall: a tick over
    // 500 ms is terminated whatever the bucket holds, so the first question is
    // never the average.
    const spans = Array.isArray(stored.spans) ? stored.spans.filter((s) => s && typeof s.max === "number") : [];

    if (spans.length > 0) {
      const worst = spans.reduce((a, b) => (b.max > a.max ? b : a));
      const floor = spans.reduce((a, b) => (b.b < a.b ? b : a));
      const covered = spans.reduce((total, s) => total + s.n, 0);

      console.log(
        `\nthe long record: ${spans.length} spans over ${covered} ticks, ` +
          `t${spans[0].f.toLocaleString()}-${spans[spans.length - 1].t.toLocaleString()}`,
      );
      console.log(
        `  worst single tick ${worst.max.toFixed(1)} ms in t${worst.f.toLocaleString()}-${worst.t.toLocaleString()}` +
          ` (the engine terminates a tick over 500)`,
      );
      console.log(
        `  lowest bucket ${floor.b.toLocaleString()} in t${floor.f.toLocaleString()}-${floor.t.toLocaleString()}`,
      );

      // The worst tick's pops (#389), from the spans that carry them: a span
      // an older bundle wrote has no `p`, and is left out rather than read as
      // a tick that ran no flood.
      const popped = spans.filter((s) => typeof s.p === "number");

      if (popped.length > 0) {
        const worstPops = popped.reduce((a, b) => (b.p > a.p ? b : a));
        console.log(
          `  most heap pops in one tick ${worstPops.p.toLocaleString()} in ` +
            `t${worstPops.f.toLocaleString()}-${worstPops.t.toLocaleString()}` +
            `${popped.length < spans.length ? ` (${popped.length} of ${spans.length} spans counted)` : ""}`,
        );
      }

      // The heap and the memo tables over the record (#391): where they
      // stood first and last, and the largest either reached. A record whose
      // last span stands well above its first is the climb, whatever the
      // means did; the phase sums under each loud span say which phase.
      const measured = spans.filter((s) => typeof s.h === "number" && s.h > 0 && typeof s.w === "number");

      if (measured.length > 0) {
        const first = measured[0];
        const last = measured[measured.length - 1];
        const peak = measured.reduce((a, b) => (b.h > a.h ? b : a));
        console.log(
          `  heap ${first.h.toFixed(1)} MB at t${first.f.toLocaleString()} → ${last.h.toFixed(1)} MB at ` +
            `t${last.t.toLocaleString()}, peak ${peak.h.toFixed(1)} in t${peak.f.toLocaleString()}-${peak.t.toLocaleString()}; ` +
            `memo rows ${first.w} → ${last.w}, peak ${Math.max(...measured.map((s) => s.w))}` +
            `${measured.length < spans.length ? ` (${measured.length} of ${spans.length} spans measured)` : ""}`,
        );

        // The floor (#393) is what survived a GC — the live set; the peak
        // above is what V8 let pile up before one.
        const floored = measured.filter((s) => typeof s.hmin === "number" && s.hmin > 0);

        if (floored.length > 0) {
          const firstFloor = floored[0];
          const lastFloor = floored[floored.length - 1];
          const ext = (s) => (typeof s.x === "number" ? `${s.x.toFixed(1)} MB` : "—");
          console.log(
            `  heap floor ${firstFloor.hmin.toFixed(1)} MB at t${firstFloor.f.toLocaleString()} → ` +
              `${lastFloor.hmin.toFixed(1)} MB at t${lastFloor.t.toLocaleString()}; ` +
              `off-heap ${ext(firstFloor)} → ${ext(lastFloor)}` +
              `${floored.length < measured.length ? ` (${floored.length} of ${measured.length} measured spans carry a floor)` : ""}`,
          );
        }
      }

      const loud = spans.filter((s) => s.max >= 100).slice(-12);

      if (loud.length > 0) {
        console.log("  spans whose worst tick reached 100 ms:");

        for (const s of loud) {
          console.log(
            `    t${String(s.f).padStart(7)}-${String(s.t).padEnd(7)} ` +
              `max ${s.max.toFixed(0).padStart(4)} ms  mean ${(s.sum / Math.max(1, s.n)).toFixed(0).padStart(3)} ms  ` +
              `bucket floor ${String(s.b).padStart(6)}  replans ${s.r}` +
              (typeof s.p === "number" ? `  max pops ${String(s.p).padStart(6)}` : "") +
              (typeof s.h === "number" && s.h > 0
                ? `  heap ${s.h.toFixed(1)} MB${typeof s.hmin === "number" && s.hmin > 0 ? ` (floor ${s.hmin.toFixed(1)})` : ""}  rows ${s.w}`
                : "") +
              (["ss", "sd", "sv", "sx"].every((key) => typeof s[key] === "number")
                ? `  phases snapshot ${(s.ss / Math.max(1, s.n)).toFixed(1)} decide ${(s.sd / Math.max(1, s.n)).toFixed(1)} ` +
                  `save ${(s.sv / Math.max(1, s.n)).toFixed(1)} execute ${(s.sx / Math.max(1, s.n)).toFixed(1)}`
                : ""),
          );
        }
      } else {
        console.log("  no span's worst tick reached 100 ms");
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
