// sim as `soak:many` runs it: the `sim` profile, this machine's CPU,
// and PGO trained on one W8N3 30k soak. The wasm bot and the tests never
// build this way. The profile is trained once and reused as the code moves,
// since a stale profile costs speed, never an output; --train trains it
// again. Plain rustc flags, not cargo-pgo: its optimize step merges the
// profile anew on every call, and the new file rebuilds sim (2 min of LTO).
//
// Usage: node scripts/sim/build.mjs [--train]   (prints the binary's path)
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, rmSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const PROFILES = join(REPO, "target/pgo-profiles");
const MERGED = join(PROFILES, "merged.profdata");
const TEMPLATE = join(REPO, "soak-runs/.template.json");
const run = (cmd, args, env = process.env) => {
  const res = spawnSync(cmd, args, { cwd: REPO, env, stdio: ["ignore", 2, 2] });
  if (res.status !== 0) {
    console.error(`${cmd} ${args.join(" ")} failed${res.error ? `: ${res.error.message}` : ""}`);
    process.exit(1);
  }
};
const rustc = (flag) => spawnSync("rustc", [flag], { cwd: REPO, encoding: "utf8" }).stdout.trim();
// --target keeps the flags off build scripts, which would write profiles of their own.
const host = /^host: (.+)$/m.exec(rustc("-vV"))[1];
const bin = join(REPO, "target", host, "sim/sim");
const build = (flags) =>
  run("cargo", ["build", "--profile", "sim", "-p", "sim", "--target", host], { ...process.env, RUSTFLAGS: `${process.env.RUSTFLAGS ?? ""} -C target-cpu=native ${flags}`.trim() });

if (process.argv.includes("--train") || !existsSync(MERGED)) {
  rmSync(PROFILES, { recursive: true, force: true });
  build(`-C profile-generate=${PROFILES}`);
  if (!existsSync(TEMPLATE)) {
    mkdirSync(dirname(TEMPLATE), { recursive: true });
    run("node", [join(HERE, "export.mjs"), TEMPLATE]);
  }
  run(bin, ["--world", TEMPLATE, "--room", "W8N3", "--ticks", "30000", "--seed", "1", "--out", join(REPO, "target/pgo-train")]);
  const profiles = readdirSync(PROFILES).filter((f) => f.endsWith(".profraw")).map((f) => join(PROFILES, f));
  // rustup's llvm-tools component (rust-toolchain.toml).
  run(join(rustc("--print=target-libdir"), "../bin/llvm-profdata"), ["merge", "-o", MERGED, ...profiles]);
}
build(`-C profile-use=${MERGED}`);
console.log(bin);
