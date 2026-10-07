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
import { readFileSync, writeFileSync } from "node:fs";
import { build } from "esbuild";

const LIBRARY_PLACEHOLDER = "var __FABOT_LIBRARY_LINES__ = [];";

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
  banner: { js: `var __FABOT_VERSION__ = ${JSON.stringify(named)};\n${LIBRARY_PLACEHOLDER}` },
});

// The bundle's fable-library line ranges (#628), filled into the second
// banner line once the bundle is laid out, on that same line, so no line
// moves: a fault's stack frames are picked past the library's own
// (`Main.faultRow`), read off the `// build/fable/...` module comments
// esbuild writes. Each range is [first, last] line, 1-based.
const bundled = readFileSync("dist/main.js", "utf8");
const lines = bundled.split("\n");
const ranges = [];
let open = null;
lines.forEach((line, index) => {
  if (!line.startsWith("// build/fable/")) return;
  if (open !== null) ranges.push([open, index]);
  open = line.includes("/fable_modules/") ? index + 1 : null;
});
if (open !== null) ranges.push([open, lines.length]);
writeFileSync(
  "dist/main.js",
  bundled.replace(LIBRARY_PLACEHOLDER, () => `var __FABOT_LIBRARY_LINES__ = ${JSON.stringify(ranges)};`),
);
console.log(`bundled dist/main.js as ${named}`);
