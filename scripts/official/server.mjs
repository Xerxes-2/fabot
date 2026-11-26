// The official private server on the flake's Node 24, installed once per
// machine and booted per run on a world of its own; the bot runs under a probe
// wrapper that logs one `#probe` line per tick.
import { execFileSync, spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { cpSync, existsSync, mkdirSync, readdirSync, readFileSync, renameSync, rmSync, statSync, writeFileSync } from "node:fs";
import { createServer, connect } from "node:net";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { modulesOf, upload as post } from "../upload.mjs";

const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "../..");
const shell = (cmd, args, opts) => execFileSync("nix", ["develop", REPO, "-c", cmd, ...args], opts);
const node = String(shell("node", ["-p", "process.execPath"])).trim();
const pins = ["package.json", "package-lock.json"].map((f) => readFileSync(join(HERE, f)));
const key = createHash("sha256").update(node).update(pins[0]).update(pins[1]).digest("hex").slice(0, 12);
const CACHE = join(process.env.XDG_CACHE_HOME || join(homedir(), ".cache"), "fabot-official");
export const INSTALL = join(CACHE, key);
export const fail = (m) => {
  console.error(m);
  process.exit(1);
};
const kill = (pid, signal) => {
  try {
    return process.kill(pid, signal), true;
  } catch {
    return false;
  }
};

// Built aside and renamed into place: a racing install loses the rename and
// is thrown away, so a reader never sees half an install.
export function install() {
  if (existsSync(INSTALL)) return;
  mkdirSync(CACHE, { recursive: true });
  for (const [d, pid] of readdirSync(CACHE).map((d) => [d, d.split(".")[1]])) if (pid && !kill(+pid, 0)) rmSync(join(CACHE, d), { recursive: true, force: true });
  const tmp = `${INSTALL}.${process.pid}`;
  console.error(`official: installing the server into ${INSTALL} (minutes, once per machine)`);
  try {
    mkdirSync(tmp, { recursive: true });
    ["package.json", "package-lock.json"].forEach((f, i) => writeFileSync(join(tmp, f), pins[i]));
    // isolated-vm's pinned commit gets <cstdint> transitively, which the flake's GCC 16 no longer gives.
    const env = { ...process.env, CXXFLAGS: "-include cstdint" };
    shell("npm", ["ci", "--no-audit", "--no-fund"], { cwd: tmp, env, stdio: ["ignore", process.stderr, "inherit"] });
    renameSync(tmp, INSTALL);
  } catch (e) {
    if (!existsSync(INSTALL)) throw e;
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

const freePort = () =>
  new Promise((ok) => {
    const s = createServer().listen(0, "127.0.0.1", () => {
      const { port } = s.address();
      s.close(() => ok(port));
    });
  });
for (const signal of ["SIGINT", "SIGTERM"]) process.on(signal, () => process.exit(130));
const sleep = (ms) => new Promise((ok) => setTimeout(ok, ms));
const until = async (what, test, ms) => {
  for (const end = Date.now() + ms; !(await test()); await sleep(200)) if (Date.now() > end) fail(`official: no ${what} in ${ms / 1000}s`);
};

const PROBE = `const t0 = Game.cpu.getUsed();
const bot = require('fabot');
let load = Game.cpu.getUsed() - t0;
module.exports.loop = function () {
  let err;
  try { bot.loop(); } catch (e) { err = String(e && e.stack || e); }
  const h = Game.cpu.getHeapStatistics();
  console.log('#probe ' + JSON.stringify({ t: Game.time, cpu: Game.cpu.getUsed(), load, err,
    heap: h.used_heap_size + h.externally_allocated_size, limit: h.heap_size_limit }));
  load = undefined;
};`;
const unescape = (s) => s.replace(/&#x([0-9a-f]+);/gi, (_, h) => String.fromCharCode(parseInt(h, 16))).replace(/&quot;/g, '"').replace(/&amp;/g, "&");

// A server on `dir` (wiped), from a saved db.json or a fresh world with the bot
// spawned in W8N3; paused until `run`.
export async function boot(dir, { world } = {}) {
  install();
  const mods = join(INSTALL, "node_modules");
  // A run killed past its exit handler leaves its server up.
  if (existsSync(`${dir}.pgid`)) kill(-readFileSync(`${dir}.pgid`, "utf8"), "SIGKILL");
  rmSync(dir, { recursive: true, force: true });
  cpSync(join(mods, "@screeps/launcher/init_dist"), dir, { recursive: true });
  if (world) cpSync(world, join(dir, "db.json"));
  const [port, cli] = [await freePort(), await freePort()];
  const rc = readFileSync(join(dir, ".screepsrc"), "utf8")
    .replace("{{STEAM_KEY}}", "local-dummy") // the backend refuses an empty key; sign-in is screepsmod-auth's
    .replace(/^port = .*/m, `port = ${port}`)
    .replace(/^host = .*/m, "host = 127.0.0.1")
    .replace(/^cli_port = .*/m, `cli_port = ${cli}`)
    .replace(/^cli_host = .*/m, "cli_host = 127.0.0.1");
  writeFileSync(join(dir, ".screepsrc"), rc);
  mkdirSync(join(dir, "bot"));
  writeFileSync(join(dir, "bot/main.js"), "module.exports.loop = () => {};");
  writeFileSync(join(dir, "mods.json"), JSON.stringify({ mods: [join(mods, "screepsmod-auth/index.js")], bots: { fabot: join(dir, "bot") } }));
  // The backend leaves a rejected storage query unhandled now and then, which
  // Node before 15 only warned about; since, it kills the backend and our socket.
  const env = { ...process.env, PATH: `${dirname(node)}:${join(mods, ".bin")}:${process.env.PATH}`, NODE_OPTIONS: "--unhandled-rejections=warn" };
  const proc = spawn("screeps", ["start"], { cwd: dir, env, detached: true, stdio: "ignore" });
  writeFileSync(`${dir}.pgid`, String(proc.pid));
  // Its own process group, which a Ctrl-C to ours does not reach.
  const stop = () => {
    kill(-proc.pid, "SIGKILL");
    rmSync(`${dir}.pgid`, { force: true });
    process.off("exit", stop);
  };
  process.on("exit", stop);
  const log = (f) => (existsSync(join(dir, "logs", f)) ? readFileSync(join(dir, "logs", f), "utf8") : "");
  await until("game server", () => log("backend.log").includes("Game server listening"), 60000);

  // One CLI expression's reply; an error, or a reply not matching `want`, fails.
  const run = (expr, want = /./) =>
    new Promise((ok) => {
      const s = connect(cli, "127.0.0.1");
      const late = setTimeout(() => fail(`official: no CLI reply to ${expr} in 30s`), 30000);
      let out = "",
        sent = false,
        idle;
      s.on("data", (d) => {
        if (!sent) return (sent = true), s.write(`${expr}\r\n`);
        out += d;
        clearTimeout(idle);
        idle = setTimeout(() => {
          s.end();
          clearTimeout(late);
          if (/Error/.test(out) || !want.test(out)) fail(`official: ${expr} answered ${out.trim()}`);
          ok(out);
        }, 300);
      });
    });
  const api = `http://127.0.0.1:${port}`;
  await run("system.pauseSimulation()");
  if (!world) {
    await run("bots.spawn('fabot','W8N3',{username:'fabot',x:17,y:40})", /spawned in W8N3/);
    await run("auth.setPassword('fabot','fabot-local')");
    // A spawned bot starts with an empty bucket, and its first tick is skipped for it.
    await run("storage.db.users.update({username:'fabot'},{$set:{cpuAvailable:10000}})");
  }
  await run("system.setTickDuration(1)");
  const signin = await fetch(`${api}/api/auth/signin`, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ email: "fabot", password: "fabot-local" }) });
  const { token } = await signin.json();
  if (!token) fail(`official: sign-in as fabot got no token (HTTP ${signin.status})`);
  const auth = { "X-Token": token, "X-Username": "fabot", "Content-Type": "application/json" };

  const upload = (bundle) => post(api, auth, modulesOf(bundle, PROBE)).catch((e) => fail(`official: ${bundle}: ${e.message}`));

  // Runs `ticks` probe ticks; `at(probe)` sees each as it lands. Rows: probe, log, error.
  const play = async (ticks, at = () => {}) => {
    const me = await (await fetch(`${api}/api/auth/me`, { headers: auth })).json();
    const ws = new WebSocket(`ws://127.0.0.1:${port}/socket/websocket`);
    const rows = [];
    let [last, seen] = [Date.now(), 0];
    const done = new Promise((ok) => {
      ws.onopen = () => ws.send(`auth ${token}`);
      ws.onclose = () => fail(`official: the game server closed the socket at ${seen} ticks; see ${dir}/logs`);
      ws.onmessage = async (ev) => {
        const m = String(ev.data);
        if (m.startsWith("auth ok")) return ws.send(`subscribe user:${me._id}/console`), run("system.resumeSimulation()");
        if (!m.startsWith("[")) return;
        const [, data] = JSON.parse(m);
        if (data.error) rows.push({ error: unescape(data.error) });
        for (const l of [...(data.messages?.log ?? []), ...(data.messages?.results ?? [])].map(unescape))
          if (l.startsWith("#probe ")) {
            const p = JSON.parse(l.slice(7));
            rows.push({ probe: p });
            [last, seen] = [Date.now(), seen + 1];
            await at(p);
          } else rows.push({ log: l });
        if (seen >= ticks) ok();
      };
    });
    const stall = setInterval(() => Date.now() - last > 60000 && fail(`official: no tick in 60s:\n${JSON.stringify(rows.slice(-5))}`), 1000);
    await done;
    clearInterval(stall);
    ws.onclose = null;
    ws.close();
    await run("system.pauseSimulation()");
    return rows;
  };
  const segment = async (n) => (await (await fetch(`${api}/api/user/memory-segment?segment=${n}`, { headers: auth })).json()).data;
  // The storage autosaves every 10 s; a paused world's next save is whole.
  const save = async (to) => {
    const at = Date.now();
    await until("db save", () => statSync(join(dir, "db.json")).mtimeMs > at, 30000);
    await sleep(500);
    cpSync(join(dir, "db.json"), to);
  };
  return { upload, play, run, segment, save, log, stop };
}

// A run's world, kept for its logs until the next run: on disk, not the shared RAM /tmp.
export const scratch = (name) => join(REPO, ".scratch", "official", name);

export const pct = (xs, f) => {
  const s = [...xs].sort((a, b) => a - b);
  return +s[Math.min(s.length - 1, Math.floor(f * s.length))].toFixed(2);
};
