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
