// The operator scripts' own gate: every subcommand's argument parsing runs
// here with no network, so a crash in it — one shipped as `ReferenceError:
// jsonOut is not defined` — fails `npm test` instead of the next session
// that types the command.
//
// Two paths per `observe.mjs` subcommand. A malformed call must end in a
// usage message, exit 1, no stack. A well-formed one must get all the way
// to `connect()`, which with SCREEPS_TOKEN emptied stops on the token
// before any request — so a parse that throws, or one that refuses a good
// call, is a failure. `cpuprofile.mjs` and `probe.mjs` need no server, so
// they run for real on a synthetic profile and a synthetic bundle.
//
// Run: `npm test` (after the wire gate), or `node scripts/cli-smoke.mjs`.
import { strict as assert } from "node:assert";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import vm from "node:vm";

const script = (name) => new URL(name, import.meta.url).pathname;
const run = (name, args) => {
  const res = spawnSync(process.execPath, [script(name), ...args], {
    env: { ...process.env, SCREEPS_TOKEN: "" },
    encoding: "utf8",
    timeout: 10000,
  });
  return { code: res.status, out: res.stdout, err: res.stderr };
};
// A stack trace or a JS error name on stderr is a crash, whatever the code.
const crashed = (text) => /\n\s+at |(Reference|Type|Syntax|Range)Error/.test(text);

const cases = [];
const smoke = (name, body) => cases.push({ name, body });

const refuses = (args, says) =>
  smoke(`observe ${args.join(" ") || "(no args)"} refuses`, () => {
    const { code, err } = run("observe.mjs", args);
    assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
    assert.ok(!crashed(err), `crashed: ${err}`);
    assert.ok(err.includes(says), `stderr does not say "${says}": ${err}`);
  });

const parses = (args) =>
  smoke(`observe ${args.join(" ")} parses`, () => {
    const { code, err } = run("observe.mjs", args);
    assert.ok(!crashed(err), `crashed: ${err}`);
    assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
    assert.ok(err.includes("SCREEPS_TOKEN is not set"), `stopped before connect(): ${err}`);
  });

refuses([], "usage:");
refuses(["nope"], "usage:");
refuses(["timeline"], "usage:");
refuses(["verbose", "add"], "usage:");
refuses(["draw", "add"], "usage:");
refuses(["draw", "paint", "W13S28"], "usage:");
refuses(["cadence", "5"], "cadence takes 2, 3 or 4");
refuses(["cadence", "2.5"], "cadence takes 2, 3 or 4");
refuses(["cadence", "3", "4"], "usage:");
refuses(["cadence", "--json"], "usage:");
refuses(["fight-only", "yes"], "fight-only takes on, off or clear");
refuses(["fight-only", "on", "off"], "usage:");
refuses(["fight-only", "--json"], "usage:");
refuses(["op"], "op takes list or shadow");
refuses(["op", "add", "sweep"], "op takes list or shadow");
refuses(["op", "list", "x"], "op takes list or shadow");
refuses(["op", "shadow", "x"], "op takes list or shadow");
refuses(["fight-lane"], "usage:");
refuses(["console"], "console needs --seconds");
refuses(["raids", "--colony"], "--colony needs a home room name");
refuses(["cpu", "--colony", "W1N1"], "usage:");
refuses(["eval"], "eval needs an expression");
refuses(["eval", "x".repeat(1001)], "refuses anything over 1000");
refuses(["eval", "Game.time", "--seconds", "0"], "positive number");
refuses(["room"], "room needs a room name");
refuses(["room", "nowhere"], "room needs a room name");
refuses(["wait"], "wait needs --ticks");
refuses(["wait", "--ticks", "-3"], "wait needs --ticks");
refuses(["health", "--samples", "1"], "from 2 to 30");
refuses(["health", "--json"], "usage:");
refuses(["history", "W1N1"], "history needs a tick");
refuses(["history", "W1N1", "12.5"], "history needs a tick");
refuses(["probe", "--all"], "usage:");
refuses(["rivals", "--all"], "usage:");
refuses(["ledger", "--all"], "usage:");

for (const args of [
  ["tasks"],
  ["timeline", "worker-1-Spawn1", "--json"],
  ["raids", "--colony", "W1N1"],
  ["outposts"],
  ["layout", "--json"],
  ["quotas"],
  ["breaches"],
  ["reactor"],
  ["cpu"],
  ["cpu", "--all", "--json"],
  ["verbose", "add", "worker-1-Spawn1"],
  ["draw"],
  ["draw", "add", "W13S28"],
  ["draw", "remove", "ranger-1"],
  ["draw", "clear", "--json"],
  ["cadence"],
  ["cadence", "3"],
  ["cadence", "clear"],
  ["fight-only"],
  ["fight-only", "off"],
  ["fight-only", "clear"],
  ["op", "list"],
  ["op", "list", "--json"],
  ["op", "shadow"],
  ["op", "shadow", "--json"],
  ["console", "--seconds", "3"],
  ["eval", "Game.time"],
  ["eval", "Game.time", "--seconds", "4"],
  ["room", "W15S28", "--json"],
  ["wait", "--ticks", "2"],
  ["health"],
  ["health", "--colony", "W1N1", "--samples", "3"],
  ["history", "W15S28", "877000", "--json"],
  ["probe"],
  ["rivals", "--json"],
  ["ledger"],
  ["ledger", "--json"],
]) {
  parses(args);
}

// ---- cpuprofile.mjs -----------------------------------------------------

const dir = mkdtempSync(join(tmpdir(), "cli-smoke-"));
// root > outer > target > leaf, and outer > target again via a library
// frame: 1 ms of self time on each sample.
const frame = (functionName, lineNumber) => ({ functionName, lineNumber, url: "", scriptId: "0", columnNumber: 0 });
const profile = {
  nodes: [
    { id: 1, callFrame: frame("(root)", 0), children: [2] },
    { id: 2, callFrame: frame("outer", 10), children: [3, 5] },
    { id: 3, callFrame: frame("target", 20), children: [4] },
    { id: 4, callFrame: frame("leaf", 30), children: [] },
    { id: 5, callFrame: frame("fold", 40), children: [6] },
    { id: 6, callFrame: frame("target", 20), children: [] },
  ],
  samples: [3, 4, 4, 6],
  timeDeltas: [1000, 1000, 1000, 1000],
  startTime: 0,
  endTime: 4000,
};
const profilePath = join(dir, "fake.cpuprofile");
writeFileSync(profilePath, JSON.stringify(profile));

smoke("cpuprofile with no args prints usage", () => {
  const { code, err } = run("cpuprofile.mjs", []);
  assert.equal(code, 1);
  assert.ok(!crashed(err) && err.includes("usage:"), err);
});
smoke("cpuprofile under sums the callees", () => {
  const { code, out, err } = run("cpuprofile.mjs", [profilePath, "under", "target"]);
  assert.equal(code, 0, err);
  assert.match(out, /target total ms 4\.0/);
  assert.match(out, /2\.0 target > leaf:30/);
});
smoke("cpuprofile flat sums self time", () => {
  const { code, out, err } = run("cpuprofile.mjs", [profilePath, "flat", "target"]);
  assert.equal(code, 0, err);
  assert.match(out, /target subtree ms 4\.0/);
});
smoke("cpuprofile callers skips library frames", () => {
  const { code, out, err } = run("cpuprofile.mjs", [profilePath, "callers", "target"]);
  assert.equal(code, 0, err);
  assert.match(out, /4\.0 outer:10/);
});
smoke("cpuprofile names a function it cannot find", () => {
  const { code, err } = run("cpuprofile.mjs", [profilePath, "flat", "absent"]);
  assert.equal(code, 1);
  assert.ok(!crashed(err) && err.includes("no node named absent"), err);
});

// ---- observe.mjs cpu, offline -------------------------------------------

// The CPU line as the bundle writes it since #538: only the newest twenty
// rows carry the per-name splits, an older row keeps its `pops`, and a span
// is a positional array. Read off a Memory dump (`OBSERVE_MEMORY`).
const cpuRow = (t) => ({
  t,
  ms: 50 + t,
  entry: 0.1,
  snapshot: 10,
  decide: 30,
  save: 2,
  execute: 8,
  intents: 90,
  bucket: 9000,
  replans: 0,
  heap: 80.5,
  rows: 700,
  ext: 39.2,
});
const cpuMemory = {
  fabot: {
    observe: {
      // The switches in effect (#575), the fight-only switch overridden live.
      switches: {
        cadence: 3,
        cadenceFrom: "bucket",
        fightOnly: false,
        fightOnlyFrom: "override",
      },
      fightOnly: false,
      // The operation ledger (#662): one sweep open, one hold closed.
      ledger: {
        open: [
          {
            k: "sweep", r: "W19S26", s: 1000, l: 1200, p: { rate: 12.5, value: 3, odds: 0.62 },
            cast: [2, 1400, 90], join: 1, lostCast: [1, 700, 350], lostLent: [1, 650, 350], forced: [3, 31.5],
            kills: [2, 1500], camp: [1, 800], down: [40, 400], ans: [1, 1300], resp: 60,
          },
        ],
        closed: [
          {
            k: "hold", r: "W12S27", s: 900, l: 950, e: 950, p: { rate: 0, value: 10 },
            cast: [0, 0, 0], join: 1, lostCast: [0, 0, 0], lostLent: [0, 0, 0], forced: [0, 0], kills: [1, 600],
            down: [0, 0], ans: [0, 0],
          },
        ],
        evicted: 2,
      },
      cpu: {
        ticks: [
          ...Array.from({ length: 5 }, (_, i) => ({ ...cpuRow(i + 1), pops: 9001 + i })),
          // One light tick among the newest twenty decides nothing and
          // carries no split; the breakdowns count it.
          ...Array.from({ length: 20 }, (_, i) =>
            i === 10
              ? { ...cpuRow(i + 6), light: true }
              : {
                  ...cpuRow(i + 6),
                  colonies: { W1N1: 20.5 },
                  rooms: { W1N1: 0.9, W1N2: 0.1 },
                  projects: { W1N1: 1.2 },
                  floods: { W1N1: [5, 5, 4000 + i] },
                  head: 4.5,
                },
          ),
        ],
        spans: [
          { f: 1, t: 100, n: 100, max: 90, sum: 5000, b: 9500, r: 0, p: 8000, h: 80, w: 700, hmin: 60, x: 39, ss: 1000, sd: 3000, sv: 200, sx: 800 },
          [101, 125, 25, 480.5, 1875.25, 9000, 1, 9020, 80.5, 700, 65.7, 39.2, 250, 750, 50, 200],
        ],
      },
    },
  },
};
const cpuMemoryPath = join(dir, "cpu-memory.json");
writeFileSync(cpuMemoryPath, JSON.stringify(cpuMemory));
const runOffline = (args) => {
  const res = spawnSync(process.execPath, [script("observe.mjs"), ...args], {
    env: { ...process.env, SCREEPS_TOKEN: "", OBSERVE_MEMORY: cpuMemoryPath },
    encoding: "utf8",
    timeout: 10000,
  });
  return { code: res.status, out: res.stdout, err: res.stderr };
};

smoke("observe cpu reads the trimmed rows and the positional spans (#538)", () => {
  const { code, out, err } = runOffline(["cpu", "--all"]);
  assert.equal(code, 0, err);
  assert.ok(!crashed(err), err);
  assert.match(out, /^ {3}1 {4}51\.000 .* 9001 /m, "an aged row prints its pops");
  assert.match(out, /mean ms over 25 split rows/);
  assert.match(out, /snapshot split over 19 rows: head 4\.50 ms {2}rooms 1\.00 {2}projections 1\.20/);
  assert.match(out, /decide by colony over 19 attributed rows \(of the newest 20 split rows\):\n {2}W1N1 {2}mean 20\.50 ms/);
  assert.match(out, /snapshot by room over 19 attributed rows \(of the newest 20 split rows\)/);
  assert.match(out, /floods by colony over 19 counted rows \(of the newest 20 split rows\)/);
  assert.match(out, /the long record: 2 spans over 125 ticks, t1-125/, "a keyed span and a positional one both read");
  assert.match(out, /worst single tick 480\.5 ms in t101-125/);
  assert.match(out, /max {2}481 ms {2}mean {2}75 ms {2}bucket floor {3}9000 {2}replans 1 {2}max pops {3}9020/);
});

smoke("observe ledger prints each row's spend, loss, ratio and rate predicted against realised (#662)", () => {
  const { code, out, err } = runOffline(["ledger"]);
  assert.equal(code, 0, err);
  assert.ok(!crashed(err), err);
  assert.match(out, /^ledger {3}1 open, 1 closed, 2 evicted/m);
  assert.match(out, /^open\n {2}sweep {4}W19S26 {2}t1000 \+200$/m);
  // The cast body lost is in its energy already: only the lent one's life adds.
  assert.match(out, /spend 1750: cast 2 = 1400e\/90t, lent 1, lost cast 1 = 700e, lost lent 1 = 650e \(350 life\)/);
  assert.match(out, /loss {2}3200: kills 2 = 1500, down 40t = 400, answered 1 = 1300 in 60t/);
  assert.match(out, /^ {6}camp {2}odds 0\.62, kills 1 = 800$/m);
  assert.match(out, /ratio 1\.83 {2}rate 12\.50 predicted \/ 9\.50 realised {2}value 3\.00 {2}forced 3, tick CPU 31\.5 ms/);
  assert.match(out, /^recent\n {2}hold {5}W12S27 {2}t900 \+50 closed t950$/m);
  assert.match(out, /ratio inf {2}rate 0\.00 predicted \/ 12\.00 realised/);
});

smoke("observe cpu names each switch in effect and its source, a standing override flagged (#575)", () => {
  const { code, out, err } = runOffline(["cpu"]);
  assert.equal(code, 0, err);
  assert.ok(
    out.includes(
      "switches cadence 3 (bucket) · fight-only off (override)" +
        "  ! override standing: fight-only off (observe fight-only clear)",
    ),
    out,
  );
});

const { switchesLine } = await import("./switches.mjs");
smoke("switches: none overridden is no flag; a bundle before them names the override leaves it can read", () => {
  const tuned = { cadence: 2, cadenceFrom: "tuned", fightOnly: false, fightOnlyFrom: "tuned" };
  assert.equal(
    switchesLine({ switches: tuned }),
    "switches cadence 2 (tuned) · fight-only off (tuned)",
  );
  assert.equal(
    switchesLine({ cadence: 4, fightOnly: null, fightLane: "world" }),
    "switches not written (the deployed bundle predates them)  ! override standing: cadence 4 (observe cadence clear)",
    "a cleared leaf is null and an unread one is no override",
  );
  assert.equal(switchesLine({}), "switches not written (the deployed bundle predates them)");
});

// ---- observe.mjs outposts, offline ---------------------------------------

// #162: an outpost the log never shut is named off the declared list the
// bundle writes beside the ring. `OBSERVE_TIME` stands in for the server clock.
const raidsMemory = (raids) => {
  const file = join(dir, `raids-${Math.random().toString(36).slice(2)}.json`);
  writeFileSync(file, JSON.stringify({ fabot: { observe: { colonies: { W1N1: { raids } } } } }));
  return file;
};
const runOutposts = (file, args = []) => {
  const res = spawnSync(process.execPath, [script("observe.mjs"), "outposts", ...args], {
    env: { ...process.env, SCREEPS_TOKEN: "", OBSERVE_MEMORY: file, OBSERVE_TIME: "1000" },
    encoding: "utf8",
    timeout: 10000,
  });
  return { code: res.status, out: res.stdout, err: res.stderr };
};
const quietRaids = { episodes: [], outposts: [], rivalHeld: {}, holds: {} };

smoke("observe outposts names a declared outpost the log never shut (#162)", () => {
  const file = raidsMemory({
    ...quietRaids,
    outposts: [{ room: "W2N1", opened: 10, last: 20, expiry: 5000, basis: "collapse-timer" }],
    declared: ["W1N2", "W2N1"],
  });
  const { code, out, err } = runOutposts(file);
  assert.equal(code, 0, err);
  assert.match(out, /^W1N2 {2}open: declared/m, "the never-shut outpost has a line");
  assert.doesNotMatch(out, /^W2N1 {2}open/m, "a shut outpost is not also called open");
  assert.doesNotMatch(out, /is not named above/);
  const json = JSON.parse(runOutposts(file, ["--json"]).out);
  assert.deepEqual(json.declared, ["W1N2", "W2N1"]);
});

smoke("observe outposts on a leaf with no declared list says what it cannot name", () => {
  const { code, out, err } = runOutposts(raidsMemory(quietRaids));
  assert.equal(code, 0, err);
  assert.match(out, /is not named above/);
});

smoke("observe outposts reads the world danger log (ADR 0086 S2) and the latches beside the owners", () => {
  const file = join(dir, "world-danger.json");
  writeFileSync(
    file,
    JSON.stringify({
      fabot: {
        observe: {
          colonies: { W1N1: { raids: { episodes: [], declared: ["W1N2", "W2N1"] } } },
          world: {
            danger: {
              W2N1: {
                down: { room: "W2N1", opened: 10, last: 20, expiry: 5000, basis: "collapse-timer", stronghold: true },
                hold: { holder: "invader", until: 4000 },
              },
            },
          },
          rooms: { rivals: { W3N1: { o: "Trepidimous", since: 100, lastLooked: 200 }, W4N1: "Trepidimous" } },
        },
      },
    }),
  );
  const { code, out, err } = runOutposts(file);
  assert.equal(code, 0, err);
  assert.match(out, /^W1N2 {2}open: declared/m, "the never-shut outpost has a line");
  assert.doesNotMatch(out, /^W2N1 {2}open/m, "the world's row shuts it");
  const json = JSON.parse(runOutposts(file, ["--json"]).out);
  assert.deepEqual(Object.keys(json.rivalHeld), ["W3N1"], "a latched room, and not a bare owner");
  assert.equal(json.outposts[0].room, "W2N1");
});

smoke("observe rivals prints each rival's dossier (#653) beside the controllers", () => {
  const file = join(dir, "rivals.json");
  const dossier = {
    since: 10,
    latest: 990,
    aim: { shots: [5, 980], alone: 1, near: [4, 1], weak: [4, 3], heal: [0, 0], armed: [3, 3], edge: [0, 0] },
    safe: [1, 100],
    acts: [{ room: "W3N1", at: 100, noticed: 110, rcl: 6, ours: 3, armed: 2, since: 20, struck: { spawn: 5 } }],
    cool: 70100,
    armed: [2, 900],
    shapes: { "2A2M": 2 },
    rooms: { W3N1: { seen: 990, tReads: 2, tLow: 120, tLast: 1900, resp: [1, 12, 12, 12, 112], fireGain: 300, fireTicks: 10 } },
  };
  writeFileSync(file, JSON.stringify({ fabot: { observe: { rivals: { Trepidimous: dossier } } } }));
  const res = spawnSync(process.execPath, [script("observe.mjs"), "rivals"], {
    env: { ...process.env, SCREEPS_TOKEN: "", OBSERVE_MEMORY: file, OBSERVE_TIME: "1000" },
    encoding: "utf8",
    timeout: 10000,
  });
  assert.equal(res.status, 0, res.stderr);
  assert.match(res.stdout, /^Trepidimous {2}seen t10\.\.t990 \(10 ago\)/m);
  assert.match(res.stdout, /tower shots 5 .*nearest 1\/4, weakest 3\/4, attacker 3\/3/);
  assert.match(res.stdout, /W3N1 raised t100 .*our armed there 20 before; struck spawn 5 before/);
  assert.match(res.stdout, /W3N1 seen t990: towers low 120.*answered our armed body 1× avg 12/);
  assert.match(res.stdout, /no rival controllers/);
});

smoke("observe outposts refuses a declared list off its shape", () => {
  const { code, err } = runOutposts(raidsMemory({ ...quietRaids, declared: [7] }));
  assert.equal(code, 1, err);
  assert.ok(!crashed(err), err);
  assert.ok(err.includes("declared"), err);
});

// ---- probe.mjs ----------------------------------------------------------

// The anchors the wrapper needs, as esbuild writes top-level functions,
// under the banner `scripts/bundle.mjs` writes.
const bundle = `var __FABOT_VERSION__ = "abc 123+dirty";
function SpatialInfoModule_homeName(s) { return s; }
function planTasks() { return 1; }
function decideUnarbitrated(view) { return planTasks() + planTasks(); }
function loop() { decideUnarbitrated({ Spatial: "W1N1" }); }
`;
const bundlePath = join(dir, "main.js");
const wrappedPath = join(dir, "wrapped.js");
writeFileSync(bundlePath, bundle);

smoke("probe wraps a bundle that then writes Memory.__probe", () => {
  const { code, err } = run("probe.mjs", [bundlePath, "--out", wrappedPath, "--fns", "planTasks", "--from", "1", "--ticks", "2"]);
  assert.equal(code, 0, err);
  const context = { Game: { time: 0, cpu: { getUsed: () => 0 } }, Memory: {} };
  vm.runInNewContext(`${readFileSync(wrappedPath, "utf8")}\nfor (let i = 0; i < 4; i++) { Game.time++; loop(); }`, context);
  const probe = JSON.parse(JSON.stringify(context.Memory.__probe));
  assert.equal(probe.ticks, 2);
  assert.equal(probe.window, 2);
  assert.deepEqual(probe.fns["W1N1:planTasks"], [0, 4]);
  assert.deepEqual(probe.fns["W1N1:decide"], [0, 2]);
  assert.deepEqual(probe.fns["-:tick"], [0, 2]);
  assert.equal(vm.runInNewContext(`${readFileSync(wrappedPath, "utf8")}\n__FABOT_VERSION__`, context), "abc 123+dirty+probe");
});
smoke("probe refuses a bundle with no version banner", () => {
  const bare = join(dir, "bare.js");
  writeFileSync(bare, bundle.slice(bundle.indexOf("\n")));
  const { code, err } = run("probe.mjs", [bare, "--out", join(dir, "y.js"), "--fns", "planTasks"]);
  assert.equal(code, 1);
  assert.ok(!crashed(err) && err.includes("no version banner"), err);
});
smoke("probe refuses a bundle it already wrapped", () => {
  const { code, err } = run("probe.mjs", [wrappedPath, "--out", join(dir, "again.js"), "--fns", "planTasks"]);
  assert.equal(code, 1);
  assert.ok(!crashed(err) && err.includes("already wrapped"), err);
});
smoke("probe names every missing anchor", () => {
  const { code, err } = run("probe.mjs", [bundlePath, "--out", join(dir, "x.js"), "--fns", "planTasks,gone,lost"]);
  assert.equal(code, 1);
  assert.ok(!crashed(err) && err.includes("gone (0)") && err.includes("lost (0)"), err);
});
smoke("probe refuses an unknown flag", () => {
  const { code, err } = run("probe.mjs", [bundlePath, "--bogus"]);
  assert.equal(code, 1);
  assert.ok(!crashed(err) && err.includes("usage:"), err);
});

// ---- capture-tick.mjs / arena-skeleton.mjs ------------------------------

const captureRefuses = (args, says) =>
  smoke(`capture ${args.join(" ") || "(no args)"} refuses`, () => {
    const { code, err } = run("capture-tick.mjs", args);
    assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
    assert.ok(!crashed(err), `crashed: ${err}`);
    assert.ok(err.includes(says), `stderr does not say "${says}": ${err}`);
  });
captureRefuses([], "usage:");
captureRefuses(["nowhere", "5"], "is not a room name");
captureRefuses(["W1N1"], "capture needs a tick");
captureRefuses(["W1N1", "5", "--rooms", "W1N2,nowhere"], '--rooms: "nowhere"');
captureRefuses(["W1N1", "5", "--name", "9lives"], "is not a slug");
captureRefuses(["W1N1", "5", "--out"], "--out needs a value");
captureRefuses(["W1N1", "5", "--bogus"], "usage:");
captureRefuses(["W1N1", "5", "--live-memory", "--no-live-memory"], "contradict");
smoke("capture W17S25 972544 --rooms W18S25 --name door --out x parses", () => {
  const { code, err } = run("capture-tick.mjs", ["W17S25", "972544", "--rooms", "W18S25", "--name", "door", "--out", join(dir, "x.fs")]);
  assert.ok(!crashed(err), `crashed: ${err}`);
  assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
  assert.ok(err.includes("SCREEPS_TOKEN is not set"), `stopped before connect(): ${err}`);
});

const { skeleton, assignmentsAt } = await import("./arena-skeleton.mjs");
smoke("arena-skeleton writes the arena idiom off a replay", () => {
  const part = (type, hits = 100) => ({ type, hits });
  const objects = {
    c1: { _id: "c1", type: "controller", x: 15, y: 36, level: 4, safeModeAvailable: 2, safeModeCooldown: 2000, downgradeTime: 1100, progress: 7, user: "me" },
    s1: { _id: "s1", type: "spawn", x: 14, y: 28, name: "Spawn11", user: "me", store: { energy: 300 }, hits: 5000, hitsMax: 5000 },
    t1: { _id: "t1", type: "tower", x: 13, y: 29, user: "me", store: { energy: 860 }, hits: 3000, hitsMax: 3000 },
    r1: { _id: "r1", type: "rampart", x: 2, y: 15, user: "me", hits: 900, hitsMax: 3000000, nextDecayTime: 1050 },
    k1: {
      _id: "k1", type: "creep", name: "Crunchy047", x: 0, y: 15, user: "trep", hits: 150, hitsMax: 300, fatigue: 2, ageTime: 1500,
      body: [part("move", 0), part("attack", 50), part("move")], actionLog: { attack: { x: 1, y: 15 }, say: null },
    },
    k2: { _id: "k2", type: "creep", name: "Isolate093", x: 1, y: 16, user: "trep", hits: -5, hitsMax: 200, body: [part("move", 0), part("heal", 0)] },
    k3: {
      _id: "k3", type: "creep", name: "ranger-1-Spawn11", x: 8, y: 23, user: "me", hits: 200, hitsMax: 200, ageTime: 1001,
      body: [part("move"), part("ranged_attack")],
    },
    d1: { _id: "d1", type: "road", x: 3, y: 3, hits: 5000, hitsMax: 5000 },
    d2: { _id: "d2", type: "road", x: 4, y: 3, hits: 1200, hitsMax: 5000 },
    k4: {
      _id: "k4", type: "creep", name: "invader_W17S25_1", x: 20, y: 20, user: "2", hits: 100, hitsMax: 100, ageTime: 2500,
      body: [part("move")], actionLog: { rangedAttack: { x: 8, y: 23 } },
    },
  };
  // The ranger's task at t1000 is the match at t990, not the release after.
  const { known, unknown } = assignmentsAt(
    {
      "ranger-1-Spawn11": {
        log: [
          { t: 990, v: { kind: "matched", task: "refill:t1" } },
          { t: 1001, v: { kind: "released", task: "refill:t1" } },
        ],
      },
    },
    ["ranger-1-Spawn11", "gone-1"],
    1000,
  );
  assert.deepEqual([...known], [["ranger-1-Spawn11", "refill:t1"]]);
  assert.deepEqual(unknown, ["gone-1"]);
  const text = skeleton({
    rooms: [{ name: "W17S25", href: "https://example/room-history/shardSeason/W17S25/1000.json", objects, hasFixture: true }],
    tick: 1000,
    ourId: "me",
    usernames: new Map([["trep", "Trepidimous"]]),
    assignments: { known, unknown, readAt: 1200 },
    slug: "crunchy-door",
    command: "npm run capture -- W17S25 1000",
    liveTick: 1200,
  });
  for (const line of [
    "//   W17S25  https://example/room-history/shardSeason/W17S25/1000.json",
    "let private crunchyDoor () : Arena =",
    '    let trepidimous = Side.Player "Trepidimous"',
    "        |> withController Ownership.Ours None 4 2",
    "        |> withSafeModeCooldown 2_000",
    "        |> withControllerClocks 0 0 99 7",
    '        |> withSpawn "Spawn11" { X = 14; Y = 28 } 300',
    "                towerOf Side.Ours { X = 13; Y = 29 } 860",
    "                    NextDecay = 1_050",
    '            { body "Crunchy047" trepidimous (parts [ Move, 1; Attack, 1; Move, 1 ]) (at "W17S25" 0 15) (Some Hold) with',
    "                Hits = 150",
    "                TicksToLive = 499",
    "                Fatigue = 2",
    "            // Crunchy047: live: 1 ATTACK, 1 MOVE",
    "            // Crunchy047: TODO script; live: attack→1,15",
    "            // Isolate093 died at t1000 (hits -5/200): not placed.",
    '            // invader_W17S25_1: live: rangedAttack→8,23',
    '            { body "invader_W17S25_1" (Side.Npc "Invader") (parts [ Move, 1 ]) (at "W17S25" 20 20) (Some Invader)',
    '            { body "ranger-1-Spawn11" Side.Ours (parts [ Move, 1; RangedAttack, 1 ]) (at "W17S25" 8 23) None',
    '                            "ranger-1-Spawn11", "refill:tower-13-29"',
    "                // TODO gone-1: the log holds no task verdict for it that early (or it has died since).",
    "    let a = arena 1_001 [ w17s25 ] colonies bodies",
    "                // EXPECT: what should happen; if it pins today's behaviour, say which issue flips it.",
  ]) {
    assert.ok(text.includes(line), `missing:\n${line}\n---\n${text}`);
  }
  assert.ok(!text.includes("invader_W17S25_1: TODO"), "the engine's invader AI is the arena's: no script to write");
  assert.ok(text.includes("        |> withRoads [ 3, 3, 5_000; 4, 3, 1_200 ]"), `the roads are laid, by tile:\n${text}`);
  assert.ok(!text.includes("cast by"), "every caster captured");
});
smoke("arena-skeleton seats live Memory up to the tick and names a caster it lacks", () => {
  const objects = {
    c1: { _id: "c1", type: "controller", x: 15, y: 36, level: 4, safeModeAvailable: 2, user: "me" },
    s1: { _id: "s1", type: "spawn", x: 14, y: 28, name: "Spawn11", user: "me", store: { energy: 0 }, storeCapacityResource: { energy: 300 }, hits: 5000, hitsMax: 5000 },
    e1: { _id: "e1", type: "extension", x: 14, y: 26, user: "me", store: { energy: 0 }, storeCapacityResource: { energy: 50 }, hits: 1000, hitsMax: 1000 },
    k1: { _id: "k1", type: "creep", name: "ranger-1-Spawn3", x: 8, y: 23, user: "me", hits: 100, hitsMax: 100, ageTime: 2000, body: [{ type: "move", hits: 100 }] },
  };
  const live = {
    readAt: 1100,
    rooms: { armed: { W17S25: 990, W18S25: 1050, W9N9: 900 } /* W18S25 and W9N9 are not captured */, towered: ["W17S25", "W9N9"] },
    raids: {
      W17S25: {
        outposts: [
          { room: "W18S25", opened: 980, last: 980, expiry: 1080, basis: "player-raid-hold", raiders: ["x"], player: 980 },
          { room: "W17S26", opened: 900, last: 900, expiry: 950, basis: "raid-gone" },
        ],
        threatened: { W18S25: { until: 2490 }, W17S26: { until: 2600 } },
        fought: { W17S25: { seen: 995, squad: "duo", raiders: ["x"] }, W17S24: { seen: 1090 } },
        homeRaided: 1090,
      },
    },
  };
  const text = skeleton({
    rooms: [{ name: "W17S25", href: "h", objects, hasFixture: false }],
    tick: 1000,
    ourId: "me",
    usernames: new Map(),
    assignments: null,
    live,
    slug: "live",
    command: "c",
    liveTick: 1100,
  });
  for (const line of [
    '        |> withSpawn "Spawn11" { X = 14; Y = 28 } 350',
    "cast by Spawn3, whose room is not captured",
    "`npm run capture-room -- W17S25` first",
    "// From live Memory at t1,100, 100 ticks after the capture:",
    'ArmedSeen = Map.ofList [ "W17S25", 990 ]',
    'Towered = Set.ofList [ "W17S25" ]',
    '{ Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"',
    "Basis = StandDownBasis.PlayerRaidHold",
    "PlayerSince = Some 980",
    '"W18S25", ({ Until = 2_490 }: Fabot.Core.Observe.ThreatLatch)',
    '"W17S25", ({ Seen = 995; Raiders = Set.ofList [ "x" ] }: FightRecord)',
    "not seated unless said: Threatened W17S25/W17S26 seen t1,100, Fought W17S25/W17S24 t1,090, HomeRaided W17S25 t1,090.",
  ]) {
    assert.ok(text.includes(line), `missing:\n${line}\n---\n${text}`);
  }
  assert.ok(!text.includes('RoomName = "W17S26"'), "a spent stand-down is not seated");
});

smoke("arena-skeleton banks active extensions, suffixes a clashing site, clamps a later stand-down", () => {
  const site = (id) => ({ _id: id, type: "constructionSite", structureType: "extension", x: 20, y: 20, user: "me", progress: 10, progressTotal: 3000 });
  const extension = (i) => ({ _id: `e${i}`, type: "extension", x: 10 + i, y: 24, user: "me", store: {}, storeCapacityResource: { energy: 50 }, hits: 1000, hitsMax: 1000 });
  const rooms = [
    {
      name: "W17S25", href: "h", hasFixture: true,
      objects: {
        c: { _id: "c", type: "controller", x: 15, y: 36, level: 2, safeModeAvailable: 0, user: "me" },
        s: { _id: "s", type: "spawn", name: "Spawn11", x: 14, y: 28, user: "me", store: {}, storeCapacityResource: { energy: 300 }, hits: 5000, hitsMax: 5000 },
        ...Object.fromEntries([...Array(8).keys()].map((i) => [`e${i}`, extension(i)])),
        a: site("a"),
      },
    },
    { name: "W18S25", href: "h", hasFixture: true, objects: { b: site("b") } },
  ];
  const live = {
    readAt: 1100, rooms: {},
    raids: { W17S25: { outposts: [{ room: "W18S25", opened: 990, last: 1050, expiry: 1200, basis: "player-raid-hold", player: 1020 }] } },
  };
  const text = skeleton({ rooms, tick: 1000, ourId: "me", usernames: new Map(), assignments: null, live, slug: "clash", command: "c", liveTick: 1100 });
  for (const line of [
    // RCL2 keeps five of the eight extensions active.
    '        |> withSpawn "Spawn11" { X = 14; Y = 28 } 550',
    '        |> withSite Side.Ours "extension" None 10 { X = 20; Y = 20 }',
    '@ [ { Id = "site-extension-20-20-W18S25"; Kind = "extension"',
    "stand-down W17S25/W18S25 seen t1,050 (LastSeen clamped to the tick, Raiders as read)",
    "LastSeen = 1_000",
    "PlayerSince = None",
  ]) {
    assert.ok(text.includes(line), `missing:\n${line}\n---\n${text}`);
  }
  assert.ok(!text.includes("let at roomName"), "no unused position helper when no body is placed");
});

// A record the skeleton spells out whole fails to compile the day its F#
// type gains a field (`OutpostEpisode.HolderUnread` did, #627): each such
// literal's top-level fields must be its type's, read off the source.
const repo = new URL("../", import.meta.url).pathname;
const fieldsOfType = (file, name) => {
  const lines = readFileSync(join(repo, file), "utf8").split("\n");
  const start = lines.findIndex((l) => l === `type ${name} =`);
  assert.ok(start >= 0 && lines[start + 1] === "    {", `type ${name} is not a record in ${file}`);
  const fields = [];
  for (const line of lines.slice(start + 2)) {
    if (line === "    }") return fields.sort();
    const field = /^ {8}(\w+):/.exec(line);
    if (field) fields.push(field[1]);
  }
  assert.fail(`type ${name} in ${file} never closes`);
};
// The top-level field names of one `{ ... }` literal, strings and nested
// records dropped.
const fieldsOfLiteral = (literal) => {
  let depth = 0;
  let flat = "";
  for (const ch of literal.replace(/"(?:[^"\\]|\\.)*"/g, '""')) {
    if (ch === "{") depth++;
    if (depth === 1) flat += ch;
    if (ch === "}") depth--;
  }
  return [...flat.matchAll(/(?:\{|;|\n)\s*(?:[\w.]+\.)?(\w+) =/g)].map((m) => m[1]).sort();
};
smoke("arena-skeleton spells every field of each record it writes whole", () => {
  const site = (id) => ({ _id: id, type: "constructionSite", structureType: "extension", x: 20, y: 20, user: "me", progress: 10, progressTotal: 3000 });
  const rooms = [
    { name: "W17S25", href: "h", hasFixture: true, objects: { a: site("a") } },
    { name: "W18S25", href: "h", hasFixture: true, objects: { b: site("b") } },
  ];
  const live = {
    readAt: 1100,
    rooms: {},
    raids: {
      W17S25: {
        outposts: [{ room: "W18S25", opened: 980, last: 980, expiry: 1080, basis: "reservation", read: true }],
        threatened: { W18S25: { until: 2490 } },
        fought: { W17S25: { seen: 995, raiders: ["x"] } },
      },
    },
  };
  const text = skeleton({ rooms, tick: 1000, ourId: "me", usernames: new Map(), assignments: null, live, slug: "shape", command: "c", liveTick: 1100 });
  const literal = (re) => {
    const m = re.exec(text);
    assert.ok(m, `no ${re} in:\n${text}`);
    return m[1];
  };
  for (const [type, file, re] of [
    ["OutpostEpisode", "src/Core/Observe.fs", /(\{ Fabot\.Core\.Observe\.OutpostEpisode\.[^}]*\})/],
    ["ThreatLatch", "src/Core/Observe.fs", /\((\{[^()]*\}): Fabot\.Core\.Observe\.ThreatLatch\)/],
    ["FightRecord", "src/Core/Types/Colonies.fs", /\((\{[^()]*\}): FightRecord\)/],
    ["ArenaSite", "tests/Core.Tests/Arena.fs", /@ \[ (\{.*\}) \]/],
  ]) {
    assert.deepEqual(fieldsOfLiteral(literal(re)), fieldsOfType(file, type), `${type}: the skeleton's fields drifted from ${file}`);
  }
  assert.ok(text.includes("HolderUnread = false"), "a row whose holder a look read is seated read");
});

smoke("arena-skeleton runs a deployed stronghold: core-focus towers, its clocks, a Defend defender", () => {
  const collapse = [{ effect: 1002, power: 1002, endTime: 1045681, duration: 67951 }];
  const objects = {
    core: { _id: "core", type: "invaderCore", level: 2, user: "2", x: 8, y: 36, hits: 100000, hitsMax: 100000, effects: collapse, spawning: { name: "defender0", spawnTime: 1100 } },
    t: { _id: "t", type: "tower", user: "2", x: 7, y: 35, store: { energy: 980 }, hits: 3000, hitsMax: 3000, effects: collapse },
    d: { _id: "d", type: "creep", name: "defender1", user: "2", x: 8, y: 35, hits: 3000, hitsMax: 3000, ageTime: 1045681, strongholdId: "W16S26_1", body: [{ type: "attack", hits: 100 }] },
  };
  const text = skeleton({
    rooms: [{ name: "W16S26", href: "h", objects, hasFixture: true }],
    tick: 1000, ourId: "me", usernames: new Map(), assignments: null, slug: "bunker", command: "c",
  });
  for (const line of [
    "        |> withTowerDuties [ FocusCore ]",
    '        |> withStronghold 1_045_681 (Some("defender0", 1_100))',
    "                    Level = 2",
    '            { body "defender1" (Side.Npc "Invader") (parts [ Attack, 1 ]) (at "W16S26" 8 35) (Some Defend) with',
  ]) {
    assert.ok(text.includes(line), `missing:\n${line}\n---\n${text}`);
  }
  assert.ok(!text.includes("defender1: TODO"), "the core drives its defender: no script to write");
});

// ---- profile.mjs, flags -------------------------------------------------
// The flags are read before the bundle loads, so a refusal needs no build.

const profileRefuses = (args, says) =>
  smoke(`profile ${args.join(" ")} refuses`, () => {
    const { code, err } = run("profile.mjs", args);
    assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
    assert.ok(!crashed(err), `crashed: ${err}`);
    assert.ok(err.includes(says), `stderr does not say "${says}": ${err}`);
  });

profileRefuses(["--short", "-1"], "--short must be");
profileRefuses(["--short", "x"], "--short must be");
// RCL5 allows 30 extensions and holds 3 back as sites: 27 can go unbuilt, 28 cannot.
profileRefuses(["--level", "5", "--short", "28"], "--short must be");
profileRefuses(["--stock", "-5"], "--stock must be");
profileRefuses(["--stock", "1000001"], "--stock must be");
// The `pair` child is the flagged colony and stands no Storage at RCL2.
profileRefuses(["--scenario", "pair", "--stock", "0"], "stands none");

// ---- xxscreeps (#543): refusals only, never the engine, nix or dotnet ------

const xxRefuses = (args, says) =>
  smoke(`xx ${args.join(" ") || "(no args)"} refuses`, () => {
    const { code, err } = run("xxscreeps/xx.mjs", args);
    assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
    assert.ok(!crashed(err), `crashed: ${err}`);
    assert.ok(err.includes(says), `stderr does not say "${says}": ${err}`);
  });

xxRefuses([], "usage:");
xxRefuses(["nope"], "usage:");
xxRefuses(["setup", "extra"], "usage:");
xxRefuses(["run"], "usage:");
xxRefuses(["run", "a", "b"], "usage:");
xxRefuses(["run", "Not_A_Slug"], "neither a scenario .json nor a slug");
xxRefuses(["run", join(dir, "absent.scenario.json")], "no scenario at");
xxRefuses(["run", "w11s28-invasion", "--ticks", "0"], "--ticks needs a positive whole number");
xxRefuses(["run", "w11s28-invasion", "--seed", "1.5"], "--seed needs a whole number");
xxRefuses(["run", "w11s28-invasion", "--bot", join(dir, "nowhere")], "is not standin, replay, dist");
xxRefuses(["run", "w11s28-invasion", "--bot"], "--bot needs a value");
xxRefuses(["run", "w11s28-invasion", "--bogus"], "usage:");
xxRefuses(["diff", "a.jsonl"], "usage:");
xxRefuses(["diff", join(dir, "a.jsonl"), join(dir, "b.jsonl")], "no trace at");

// ---- spar (#567): refusals only, never dotnet or the engine ----------------

const sparRefuses = (args, says) =>
  smoke(`spar ${args.join(" ")} refuses`, () => {
    const { code, err } = run("spar.mjs", args);
    assert.equal(code, 1, `exit ${code}; stderr: ${err}`);
    assert.ok(!crashed(err), `crashed: ${err}`);
    assert.ok(err.includes(says), `stderr does not say "${says}": ${err}`);
  });

sparRefuses(["--bogus"], "usage:");
sparRefuses(["--accept", "extra"], "usage:");
sparRefuses(["xx", "Not_A_Slug"], "usage:");
sparRefuses(["xx", "--seed"], "--seed needs a whole number");
sparRefuses(["xx", "outpost-melee", "--tolerance", "x"], "--tolerance needs a whole number");

// ---- run ----------------------------------------------------------------

let failed = 0;
for (const { name, body } of cases) {
  try {
    body();
    console.log(`  ok   ${name}`);
  } catch (error) {
    failed += 1;
    console.log(`  FAIL ${name}`);
    console.log(`       ${error.message.split("\n").join("\n       ")}`);
  }
}
rmSync(dir, { recursive: true, force: true });
console.log(`\n${cases.length - failed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
