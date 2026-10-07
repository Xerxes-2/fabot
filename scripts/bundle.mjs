// Bundle build/fable/Main.js into dist/main.js, naming the change it was
// built from (#634): one banner line, `var __FABOT_VERSION__ = "<change>
// <commit>[+dirty]";`, which `Main` reads once per global reset and writes to
// `Memory.fabot.observe.version` for `observe health` and `observe cpu`.
// Two builds of the same change differ in nothing else.
//
// The named commit is `@`, or `@-` when `@` is an empty undescribed change on
// top of it (`jj new` after a ship). `+dirty` marks a commit that is still
// mutable — not pushed — so its id may not survive a rewrite; it is marked,
// never refused. Outside a jj repo the version reads `unknown`.
// `scripts/probe.mjs` appends `+probe` to the same line.
import { execFileSync } from "node:child_process";
import { build } from "esbuild";

const jj = (rev) =>
  execFileSync(
    "jj",
    [
      "log",
      "-r",
      rev,
      "--limit",
      "1",
      "--no-graph",
      "-T",
      'change_id.short() ++ " " ++ commit_id.short() ++ " " ++ ' +
        'if(empty && !description, "bare", "-") ++ " " ++ if(immutable, "pushed", "-")',
    ],
    { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
  )
    .trim()
    .split(" ");

const version = () => {
  try {
    let [change, commit, bare, pushed] = jj("@");
    if (bare === "bare") [change, commit, bare, pushed] = jj("@-");
    return `${change} ${commit}${pushed === "pushed" ? "" : "+dirty"}`;
  } catch {
    return "unknown";
  }
};

const named = version();
await build({
  entryPoints: ["build/fable/Main.js"],
  bundle: true,
  format: "cjs",
  outfile: "dist/main.js",
  banner: { js: `var __FABOT_VERSION__ = ${JSON.stringify(named)};` },
});
console.log(`bundled dist/main.js as ${named}`);
