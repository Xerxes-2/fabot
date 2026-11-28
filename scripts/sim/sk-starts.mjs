// Starts for SK farming on generated maps: each gen seed's world (`sim gen-rooms --out`), its
// homes Strategy may claim with two sources and a keeper room within REACH_HOPS crossings,
// and the normal rooms' sources within two crossings, to tell a poor-remote home from a good one.
// --ab plays chosen homes at --node N7 through soak:many, and says how long each keeper room was worked.
//
// Usage: node scripts/sim/sk-starts.mjs [--gens 1..K|a,b,...] [--sectors WxH]
//        node scripts/sim/sk-starts.mjs --ab gen:room,... --bot base=a,sk=b [--seeds 1,2] [--ticks 30000] [--jobs 8]
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { basename, join } from "node:path";
import { near } from "./node.mjs";
import { REPO, jsonLines, seedFlags, simBinary, soakDir } from "./run.mjs";

const usage = "usage: sk-starts.mjs [--gens 1..K] [--sectors WxH] | --ab gen:room,... --bot base=a,sk=b [--seeds 1,2] [--ticks N] [--jobs N]";
const argv = process.argv.slice(2);
const DIR = join(REPO, ".scratch/sk-starts");
mkdirSync(DIR, { recursive: true });
const sim = simBinary() ?? process.exit(1);
const run = (cmd, args) => {
  const res = spawnSync(cmd, args, { cwd: REPO, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"], maxBuffer: 1 << 26 });
  if (res.status !== 0) (console.error(`${cmd} ${args.join(" ")} failed:\n${res.stderr}`), process.exit(1));
  return res.stdout;
};
const REACH_HOPS = 3; // core/base/declarations.rs
const worldOf = (gen, sectors) => {
  const file = join(DIR, `gen${gen}${sectors ? `-${sectors}` : ""}.json`);
  if (!existsSync(file)) run(sim, ["gen-rooms", "--gen", String(gen), ...(sectors ? ["--sectors", sectors] : []), "--out", file]);
  return file;
};

/** Each room's crossings from `home` through open exits, up to `most`. */
function crossings(rooms, home, most) {
  const walled = (r, side) => [...Array(50).keys()].every((i) => Number(r.terrain[[i, i * 50 + 49, 2450 + i, i * 50][side]]) & 1);
  const seen = new Map([[home, 0]]);
  for (let queue = [home]; queue.length; ) {
    const at = queue.shift();
    const [, n, , w, e, , s] = near(at, 1);
    for (const [next, side] of [[n, 0], [e, 1], [s, 2], [w, 3]])
      if (rooms.has(next) && !seen.has(next) && seen.get(at) < most && !walled(rooms.get(at), side)) {
        seen.set(next, seen.get(at) + 1);
        queue.push(next);
      }
  }
  return seen;
}

function starts(gen, sectors) {
  const file = worldOf(gen, sectors);
  const rooms = new Map(JSON.parse(readFileSync(file, "utf8")).rooms.map((r) => [r.name, r]));
  const count = (r, type) => r.objects.filter((o) => o.type === type).length;
  // Strategy's claim rule and Layout's first spawn, as `npm run start-rooms` ranks them.
  const [head, ...rows] = run(sim, ["start-rooms", file]).split("\n").filter(Boolean);
  const col = (row, name) => row.slice(head.search(new RegExp(`\\b${name}\\b`))).split(/\s+/)[0];
  return rows.filter((row) => col(row, "claim") === "yes" && col(row, "src") === "2").flatMap((row) => {
    const home = col(row, "room");
    const hops = crossings(rooms, home, REACH_HOPS);
    const orthogonal = [1, 3, 4, 6].map((i) => near(home, 1)[i]);
    const keepers = [...rooms.values()].filter((r) => count(r, "keeperLair") && (hops.has(r.name) || orthogonal.includes(r.name)));
    if (!keepers.length) return [];
    const remotes = [...hops].filter(([r, d]) => d && d <= 2 && count(rooms.get(r), "controller"));
    return [{ gen, home, sk: keepers.map((r) => `${r.name}:${hops.get(r.name) ?? "-"}`).sort((a, b) => a.split(":")[1] - b.split(":")[1]),
      lairs: keepers.map((r) => count(r, "keeperLair")), remote: remotes.reduce((n, [r]) => n + count(rooms.get(r), "source"), 0), outSrc: col(row, "out-src") }];
  });
}

const flag = (name, or) => (argv.includes(name) ? argv[argv.indexOf(name) + 1] : or);
if (!argv.includes("--ab")) {
  const { seeds: gens } = seedFlags(["--seeds", flag("--gens", "1..8")], usage);
  console.log(["gen", "home", "sk(room:crossings)", "lairs", "remote-src", "out-src"].join("\t"));
  for (const s of gens.flatMap((g) => starts(g, flag("--sectors"))).sort((a, b) => a.remote - b.remote))
    console.log([s.gen, s.home, s.sk.join(" "), s.lairs.join(" "), s.remote, s.outSrc].join("\t"));
  process.exit(0);
}

const homes = flag("--ab").split(",").map((h) => h.split(":"));
const ticks = flag("--ticks", "30000");
const many = spawnSync("node", [join(REPO, "scripts/scenario/soak-many.mjs"), "--tag", "sk", "--bot", flag("--bot", ""), "--seed", flag("--seeds", "1,2"), "--ticks", ticks,
  "--jobs", flag("--jobs", "8"), "--room", homes.map(([g, r]) => `${worldOf(g, flag("--sectors"))}:${r}@N7`).join(",")], { cwd: REPO, stdio: "inherit" });
// Ticks Strategy worked each keeper room in reach (metrics `held`), off the runs soak:many left.
const bots = flag("--bot", "").split(",").map((b) => b.split("=")[0]);
console.log(["gen", "home", "seed", ...bots.map((b) => `${b}: worth dug, sk held ticks`)].join("\t"));
for (const [gen, room] of homes) {
  const sk = starts(Number(gen), flag("--sectors")).find((s) => s.home === room)?.sk.map((s) => s.split(":")[0]) ?? [];
  for (const seed of flag("--seeds", "1,2").split(",")) {
    const cells = bots.map((bot) => {
      const out = soakDir(`sk-${bot}-s${seed}-${basename(worldOf(gen, flag("--sectors")))}-${room}-N7`);
      const held = jsonLines(join(out, "metrics.jsonl")).filter((m) => sk.includes(m.row)).reduce((n, m) => ({ ...n, [m.row]: (n[m.row] ?? 0) + (m.values.held ?? 0) }), {});
      const last = jsonLines(join(out, "samples.jsonl")).at(-1);
      const worth = last && Object.values(last.worth).reduce((a, b) => a + b, 0);
      return `${last ? `${Math.round(worth / 1000)}k ${Math.round(last.ledger.dug / 1000)}k` : "?"}, ${Object.entries(held).map(([r, n]) => `${r} ${n}`).join(" ") || "none"}`;
    });
    console.log([gen, room, seed, ...cells].join("\t"));
  }
}
process.exit(many.status ?? 1);
