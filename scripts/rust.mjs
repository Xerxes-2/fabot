// Build the Rust bot: `wasm-pack --target nodejs`, then the one file Screeps
// runs. The engine gives a code package no `fs` and no `require` of a sibling
// `.wasm`, so the module is inlined as base64 and decoded by a small JS
// decoder. Output is `dist/main.js`.
import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
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

execFileSync("wasm-pack", ["build", REPO, "--target", "nodejs", "--release", "--out-dir", "pkg", "--out-name", "fabot"], {
  stdio: "inherit",
  cwd: REPO,
});

const glue = readFileSync(join(REPO, "pkg", "fabot.js"), "utf8");
const wasm = readFileSync(join(REPO, "pkg", "fabot_bg.wasm"));
const loader = `const wasmPath = \`\${__dirname}/fabot_bg.wasm\`;
const wasmBytes = require('fs').readFileSync(wasmPath);`;
if (!glue.includes(loader)) throw new Error("rust.mjs: wasm-pack's loader changed; the inline patch is stale");

const prelude = `globalThis.__FABOT_VERSION__ = ${JSON.stringify(version())};
${readFileSync(join(REPO, "src", "app", "read.js"), "utf8")}

${readFileSync(join(REPO, "src", "app", "text.js"), "utf8")}
const __fabotBase64 = "${wasm.toString("base64")}";
function __fabotWasm() {
  const digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  const table = new Uint8Array(128);
  for (let i = 0; i < 64; i++) table[digits.charCodeAt(i)] = i;
  const text = __fabotBase64;
  const pad = text.endsWith("==") ? 2 : text.endsWith("=") ? 1 : 0;
  const out = new Uint8Array((text.length * 3) / 4 - pad);
  let at = 0;
  for (let i = 0; i < text.length; i += 4) {
    const word =
      (table[text.charCodeAt(i)] << 18) | (table[text.charCodeAt(i + 1)] << 12) |
      (table[text.charCodeAt(i + 2)] << 6) | table[text.charCodeAt(i + 3)];
    out[at++] = word >> 16;
    if (at < out.length) out[at++] = word >> 8;
    if (at < out.length) out[at++] = word;
  }
  return out;
}
`;

// A trap (every Rust panic is one) can leave a RefCell borrowed or the heap
// mid-write, and the engine calls the same instance next tick. A fresh
// instance of the compiled module starts clean, as a global reset does; the
// glue's views of the old memory are dropped with it.
const loop = "function loop() {\n    wasm.loop();\n}";
const views = [...glue.matchAll(/^let (cached\w*Memory0) = null;$/gm)].map((m) => m[1]);
const instance = "let wasmInstance = new WebAssembly.Instance(wasmModule, __wbg_get_imports());\nlet wasm = wasmInstance.exports;";
if (!glue.includes(loop) || !glue.includes(instance) || views.length === 0)
  throw new Error("rust.mjs: wasm-bindgen's loop or instance changed; the trap recovery is stale");
const recovering = `function loop() {
    try {
        wasm.loop();
    } catch (error) {
        wasmInstance = new WebAssembly.Instance(wasmModule, __wbg_get_imports());
        wasm = wasmInstance.exports;
        ${views.map((v) => `${v} = null;`).join("\n        ")}
        wasm.__wbindgen_start();
        throw error;
    }
}`;

const body = glue.replace(loader, "const wasmBytes = __fabotWasm();").replace(loop, recovering);
mkdirSync(join(REPO, "dist"), { recursive: true });
writeFileSync(OUT, prelude + body);
console.log(`bundled ${OUT} (${(wasm.length / 1024).toFixed(0)} kB wasm, ${(readFileSync(OUT).length / 1024).toFixed(0)} kB)`);
