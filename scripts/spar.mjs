// npm run spar — the combat bench (#567): run the arena's spar scenarios
// (`tests/Core.Tests/Spar.fs`), print each one's outcome metrics, and diff
// them against the committed baseline `tests/spar-baseline.json`.
// How to read a moved metric, and when to accept one: docs/agents/spar.md.
//
// Usage:
//   spar.mjs [--accept]
//       run every scenario; exit 1 when a metric is worse than the
//       baseline, or a scenario is new or gone, unless --accept writes the
//       run as the new baseline
//   spar.mjs xx [slug ...] [--seed N] [--tolerance T]
//       the periodic cross-check: run scenarios through xxscreeps
//       (`npm run xx`, our bodies replayed) and compare the two runs' deaths;
//       exit 1 when a death differs or lands more than T ticks apart
//       (default 30). Default slugs: outpost-melee, home-melee, home-squad.
import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { deaths, readTrace } from "./xxscreeps/trace.mjs";

const usage = `usage: spar.mjs [--accept]
       spar.mjs xx [slug ...] [--seed N] [--tolerance T]`;

function fail(message) {
  console.error(message);
  process.exit(1);
}

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const BASELINE = join(REPO, "tests/spar-baseline.json");
const OUT = join(REPO, "build/spar");
const FILTER = "FullyQualifiedName~spar bench";

// Each metric, how it reads in the table, and which way is worse. `cleared`
// is the tick the fight was won, null for never: later or never is worse.
const METRICS = [
  { key: "cleared", head: "cleared", worse: (now, was) => (now ?? Infinity) > (was ?? Infinity) },
  { key: "ourDeaths", head: "deaths", worse: (now, was) => now > was },
  { key: "kills", head: "kills", worse: (now, was) => now < was },
  { key: "castEnergy", head: "cast", worse: (now, was) => now > was },
  { key: "hurt", head: "hurt", worse: (now, was) => now > was },
  { key: "struck", head: "struck", worse: (now, was) => now > was },
  { key: "towerSpent", head: "tower", worse: (now, was) => now > was },
  { key: "stoodDown", head: "down", worse: (now, was) => now && !was },
  { key: "safeMode", head: "safe", worse: (now, was) => now && !was },
];

const [first, ...rest] = process.argv.slice(2);
if (first === "xx") crossCheck(rest);
else bench([first, ...rest].filter((a) => a !== undefined));

// ---- the bench ---------------------------------------------------------------
function bench(args) {
  const accept = args.includes("--accept");
  if (args.some((a) => a !== "--accept")) fail(usage);

  rmSync(OUT, { recursive: true, force: true });
  const res = spawnSync("dotnet", ["test", join(REPO, "tests/Core.Tests"), "--filter", FILTER], {
    stdio: ["ignore", "pipe", "inherit"],
    encoding: "utf8",
    env: { ...process.env, FABOT_SPAR_DIR: OUT },
  });
  if (res.error) fail(`dotnet: ${res.error.message}`);
  if (res.status !== 0) fail(`${res.stdout}\ndotnet test failed (exit ${res.status})`);
  if (!existsSync(OUT)) fail(`no scenario wrote a row under ${OUT}: is the filter "${FILTER}" still the test list's name?`);

  const run = Object.fromEntries(
    readdirSync(OUT)
      .filter((f) => f.endsWith(".json"))
      .sort()
      .map((f) => JSON.parse(readFileSync(join(OUT, f), "utf8")))
      .map(({ slug, ...row }) => [slug, row]),
  );
  const baseline = existsSync(BASELINE) ? JSON.parse(readFileSync(BASELINE, "utf8")) : {};

  const worse = [];
  const better = [];
  const slugs = [...new Set([...Object.keys(run), ...Object.keys(baseline)])].sort();
  const width = Math.max(...slugs.map((s) => s.length), 8);
  const cell = (v) => (v === null || v === undefined ? "-" : v === true ? "yes" : v === false ? "no" : String(v));
  console.log(`${"scenario".padEnd(width)}  ${METRICS.map((m) => m.head.padStart(9)).join("")}`);
  for (const slug of slugs) {
    const now = run[slug];
    const was = baseline[slug];
    if (!now) {
      worse.push(`${slug}: in the baseline, not run`);
      continue;
    }
    if (!was) worse.push(`${slug}: new, no baseline`);
    const cells = METRICS.map((m) => {
      if (!was || now[m.key] === was[m.key]) return cell(now[m.key]).padStart(9);
      const moved = `${cell(was[m.key])}>${cell(now[m.key])}`;
      if (m.worse(now[m.key], was[m.key])) {
        worse.push(`${slug}: ${m.key} ${moved}`);
        return `${moved}!`.padStart(9);
      }
      better.push(`${slug}: ${m.key} ${moved}`);
      return `${moved}+`.padStart(9);
    });
    console.log(`${slug.padEnd(width)}  ${cells.join("")}`);
  }
  console.log("\n(a>b: baseline a, now b; ! worse, + better; cleared - is never inside the scenario's horizon)");

  if (accept) {
    const sorted = Object.fromEntries(Object.keys(run).sort().map((s) => [s, run[s]]));
    writeFileSync(BASELINE, JSON.stringify(sorted, null, 2) + "\n");
    console.log(`baseline written: ${BASELINE}`);
    return;
  }
  if (better.length > 0) console.log(`\nbetter than the baseline (accept it with --accept):\n  ${better.join("\n  ")}`);
  if (worse.length > 0) {
    console.log(`\nworse than the baseline — name and justify each in the commit, then --accept:\n  ${worse.join("\n  ")}`);
    process.exit(1);
  }
  console.log(better.length > 0 ? "" : "\nthe baseline holds");
}

// ---- the xxscreeps cross-check ---------------------------------------------
// The defaults cast nothing: `--bot replay` replays the bodies a scenario
// starts with and our towers' shots, never a body our spawns cast.
function crossCheck(args) {
  const slugs = [];
  let seed = "1";
  let tolerance = 30;
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === "--seed" || a === "--tolerance") {
      const value = args[++i];
      if (value === undefined || !/^\d+$/.test(value)) fail(`${a} needs a whole number\n${usage}`);
      if (a === "--seed") seed = value;
      else tolerance = Number(value);
    } else if (/^[a-z0-9][a-z0-9-]*$/.test(a)) slugs.push(a);
    else fail(usage);
  }
  if (slugs.length === 0) slugs.push("outpost-melee", "home-melee", "home-squad");

  let drifted = false;
  for (const slug of slugs) {
    const name = `spar-${slug}`;
    console.log(`\n== ${slug}`);
    const res = spawnSync(process.execPath, [join(REPO, "scripts/xxscreeps/xx.mjs"), "run", name, "--bot", "replay", "--seed", seed, "--filter", "spar bench"], {
      stdio: ["ignore", "ignore", "inherit"],
    });
    if (res.status !== 0) fail(`npm run xx failed on ${name} (exit ${res.status})`);
    const dir = join(REPO, "build/xxscreeps", name);
    const arena = readTrace(join(dir, `${name}.arena.jsonl`));
    const xx = readTrace(join(dir, `${name}.replay.xxscreeps.jsonl`));
    const da = deaths(arena);
    const dx = deaths(xx);
    for (const [id, a] of da) {
      if (!dx.has(id)) {
        console.log(`  ${id}: not in the xxscreeps run (cast in the arena), left out`);
        continue;
      }
      const x = dx.get(id);
      const apart = a === undefined || x === undefined ? (a === x ? 0 : Infinity) : Math.abs(a - x);
      const flag = apart > tolerance ? "  DRIFT" : "";
      if (flag) drifted = true;
      console.log(`  ${id.padEnd(20)} died arena ${String(a ?? "-").padStart(4)}  xxscreeps ${String(x ?? "-").padStart(4)}${flag}`);
    }
    console.log(`  per-tick diff: ${join(dir, `${name}.replay.diff.txt`)}`);
  }
  if (drifted) {
    console.log(`\na death parts the arena from xxscreeps by more than ${tolerance} ticks: read the per-tick diff (docs/agents/arena-capture.md)`);
    process.exit(1);
  }
  console.log(`\nevery death agrees within ${tolerance} ticks`);
}
