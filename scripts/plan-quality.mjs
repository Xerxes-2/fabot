// Every real room's plan and a gen-world sample's, read on what the economy
// pays before tunnels stand (core/layout/tests/quality.rs), written to
// .scratch/plan-quality/<name>.txt so two revisions diff; --base <name> lists
// the rooms that got worse against that earlier report. A revision from before
// the report existed is read by running this on the report's change rebased onto it.
//
// Usage: npm run plan:quality [-- --base <name>] [--rev <name>]   (<name> defaults to @'s commit)
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const DIR = join(REPO, ".scratch/plan-quality");
// Seeds the scenarios and soaks play, one 2×2 for gen111013's E5S18.
const SEEDS = [["1"], ["3"], ["7"], ["597"], ["850"], ["1042"], ["1644"], ["1702"], ["111013", "2x2"]];
const flag = (name) => {
  const i = process.argv.indexOf(name);
  return i < 0 ? undefined : process.argv[i + 1];
};
const run = (cmd, args, env) => {
  const res = spawnSync(cmd, args, { cwd: REPO, env: { ...process.env, ...env }, encoding: "utf8", stdio: ["ignore", "pipe", "inherit"] });
  if (res.status !== 0) {
    console.error(`${cmd} ${args.join(" ")} failed`);
    process.exit(1);
  }
  return res.stdout;
};

mkdirSync(DIR, { recursive: true });
const gen = join(DIR, "gen-rooms.txt");
// The generator lives in the sim binary: a newer binary writes the sample again.
const built = run("cargo", ["build", "--quiet", "--release", "-p", "sim", "--message-format=json"]).split("\n");
const sim = built.map((l) => (l.startsWith("{") ? JSON.parse(l) : {})).find((m) => m.target?.name === "sim" && m.executable)?.executable;
const genRooms = ([seed, sectors = "1x1"]) => run(sim, ["gen-rooms", "--gen", seed, "--sectors", sectors]);
if (!existsSync(gen) || statSync(sim).mtimeMs > statSync(gen).mtimeMs) writeFileSync(gen, SEEDS.map(genRooms).join(""));
const rev = flag("--rev") ?? run("jj", ["log", "--no-graph", "-r", "@", "-T", "commit_id.short(8)"]).trim();
const out = join(DIR, `${rev}.txt`);
run("cargo", ["test", "--quiet", "--release", "-p", "fabot-layout", "--test", "quality", "--", "--ignored"], { PLAN_QUALITY_OUT: out, PLAN_QUALITY_GEN: gen });
const text = readFileSync(out, "utf8");
console.log(text.slice(0, text.indexOf("--\n")) + `report: ${out}`);

const base = flag("--base");
if (base) {
  const rooms = (file) => {
    const body = readFileSync(file, "utf8").split("--\n")[1] ?? "";
    const entries = body.split("\n").filter(Boolean).map((l) => {
      const [room, ...kv] = l.split(" ");
      return [room, Object.fromEntries(kv.map((p) => p.split("=")))];
    });
    return new Map(entries);
  };
  const before = rooms(join(DIR, `${base}.txt`));
  const after = rooms(out);
  // Worse: a CARRY more haul, the farthest extension five ticks farther, or a walk cut.
  const worse = [];
  const saved = [];
  let moved = 0;
  for (const [room, b] of before) {
    const a = after.get(room);
    if (!a) continue;
    moved += Number(a.centre !== b.centre);
    if (b.haul === "unreachable") continue;
    const haul = Number(a.haul) - Number(b.haul);
    const far = Number(a.farthest) - Number(b.farthest);
    const cut = a.haul === "unreachable" && b.haul !== "unreachable";
    if (Number.isFinite(haul)) saved.push(-haul);
    if (cut || haul >= 1 || far >= 5) worse.push({ room, haul: cut ? Infinity : haul, far, from: b.centre, to: a.centre });
  }
  worse.sort((x, y) => y.haul - x.haul || y.far - x.far);
  const sum = saved.reduce((s, v) => s + v, 0);
  console.log(`against ${base}: ${moved} of ${before.size} rooms moved centre, haul CARRY saved ${sum.toFixed(0)} over ${saved.length}, ${worse.length} worse`);
  const signed = (v, d) => (v === Infinity ? "unreachable" : `${v >= 0 ? "+" : ""}${v.toFixed(d)}`);
  for (const w of worse) console.log(`  ${w.room} ${w.from}->${w.to} haul ${signed(w.haul, 1)} farthest ${signed(w.far, 0)}`);
}
