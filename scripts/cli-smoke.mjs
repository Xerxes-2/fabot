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

// ---- probe.mjs ----------------------------------------------------------

// The anchors the wrapper needs, as esbuild writes top-level functions.
const bundle = `"use strict";
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
    '            { body "ranger-1-Spawn11" Side.Ours (parts [ Move, 1; RangedAttack, 1 ]) (at "W17S25" 8 23) None',
    '                            "ranger-1-Spawn11", "refill:tower-13-29"',
    "                // TODO gone-1: the log holds no task verdict for it that early (or it has died since).",
    "    let a = arena 1_001 [ w17s25 ] colonies bodies",
    "                // EXPECT: what should happen; if it pins today's behaviour, say which issue flips it.",
  ]) {
    assert.ok(text.includes(line), `missing:\n${line}\n---\n${text}`);
  }
  assert.ok(!text.includes("road-3-3"), "a road is not placed");
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
    '"W17S25", ({ Seen = 995; Squad = Some "duo"; Raiders = Set.ofList [ "x" ] }: FightLatch)',
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
