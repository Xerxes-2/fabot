// Build the Rust bot with `wasm-pack --target nodejs`. The engine has no `fs`,
// but `require` of a binary module returns its bytes: the wasm ships beside
// the bundle, as `dist/main.wasm`, and uploads as the module `fabot_bg`.
import { execFileSync } from "node:child_process";
import { copyFileSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..");

// The banner names the change and commit the build came from: `@`, or `@-`
// when `@` is an empty undescribed change on top of it (`jj new` after a
// ship); `+dirty` while it can still be rewritten, `unknown` outside a jj repo.
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
const OUT = join(REPO, "dist", "main.js");
const WASM = join(REPO, "dist", "main.wasm");

execFileSync("wasm-pack", ["build", REPO, "--target", "nodejs", "--release", "--out-dir", "pkg", "--out-name", "fabot"], {
  stdio: "inherit",
  cwd: REPO,
});

const glue = readFileSync(join(REPO, "pkg", "fabot.js"), "utf8");
const loader = `const wasmPath = \`\${__dirname}/fabot_bg.wasm\`;
const wasmBytes = require('fs').readFileSync(wasmPath);
const wasmModule = new WebAssembly.Module(wasmBytes);`;
if (!glue.includes(loader)) throw new Error("rust.mjs: wasm-pack's loader changed; the loader patch is stale");

const prelude = `globalThis.__FABOT_VERSION__ = ${JSON.stringify(version())};
${readFileSync(join(REPO, "src", "app", "read.js"), "utf8")}

${readFileSync(join(REPO, "src", "app", "text.js"), "utf8")}
`;

// A trap or CPU timeout can leave a RefCell borrowed or the heap mid-write; a
// timeout skips catch/finally, so recovery keys on a loop() that never returned.
const loop = "function loop() {\n    wasm.loop();\n}";
const views = [...glue.matchAll(/^let (cached\w*Memory0) = null;$/gm)].map((m) => m[1]);
const instance = "let wasmInstance = new WebAssembly.Instance(wasmModule, __wbg_get_imports());\nlet wasm = wasmInstance.exports;";
if (!glue.includes(loop) || !glue.includes(instance) || views.length === 0)
  throw new Error("rust.mjs: wasm-bindgen's loop or instance changed; the recovery is stale");
const recovering = `let running = false;
function loop() {
    if (running) {
        wasmInstance = new WebAssembly.Instance(wasmModule, __wbg_get_imports());
        wasm = wasmInstance.exports;
        ${views.map((v) => `${v} = null;`).join("\n        ")}
        wasm.__wbindgen_start();
    }
    running = true;
    wasm.loop();
    running = false;
}`;

// The engine caches a required module for the global's life; the compiled
// module is all the recovery needs, so the bytes are let go.
const compile = `const wasmModule = new WebAssembly.Module(require('fabot_bg'));
if (require.cache) delete require.cache.fabot_bg;`;
const body = glue.replace(loader, compile).replace(loop, recovering);
mkdirSync(join(REPO, "dist"), { recursive: true });
writeFileSync(OUT, prelude + body);
copyFileSync(join(REPO, "pkg", "fabot_bg.wasm"), WASM);
const kB = (f) => `${(readFileSync(f).length / 1024).toFixed(0)} kB`;
console.log(`bundled ${OUT} (${kB(OUT)}) and ${WASM} (${kB(WASM)})`);
