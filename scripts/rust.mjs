// Build the Rust bot: `wasm-pack --target nodejs`, then the one file Screeps
// runs. The engine gives a code package no `fs` and no `require` of a sibling
// `.wasm`, so the module is inlined as base64 and decoded by a small JS
// decoder; `TextDecoder` is absent in the engine's sandbox, so it is polyfilled
// here. Output is `dist/fabot-rs.js` until the port is the bot the gates run.
import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const CRATE = join(REPO, "rust");
const OUT = join(REPO, "dist", "fabot-rs.js");

execFileSync("wasm-pack", ["build", CRATE, "--target", "nodejs", "--release", "--out-dir", "pkg", "--out-name", "fabot"], {
  stdio: "inherit",
  cwd: CRATE,
});

const glue = readFileSync(join(CRATE, "pkg", "fabot.js"), "utf8");
const wasm = readFileSync(join(CRATE, "pkg", "fabot_bg.wasm"));
const loader = `const wasmPath = \`\${__dirname}/fabot_bg.wasm\`;
const wasmBytes = require('fs').readFileSync(wasmPath);`;
if (!glue.includes(loader)) throw new Error("rust.mjs: wasm-pack's loader changed; the inline patch is stale");

const prelude = `
// The engine's sandbox has no TextDecoder and no filesystem: decode the module
// that is our own code and decode UTF-8 the way the runtime would.
if (typeof TextDecoder === "undefined") {
  globalThis.TextDecoder = function TextDecoder() {};
  globalThis.TextDecoder.prototype.decode = function (bytes) {
    if (!bytes) return "";
    let out = "";
    for (let i = 0; i < bytes.length; ) {
      const c = bytes[i++];
      if (c < 0x80) out += String.fromCharCode(c);
      else if (c < 0xe0) out += String.fromCharCode(((c & 0x1f) << 6) | (bytes[i++] & 0x3f));
      else if (c < 0xf0) out += String.fromCharCode(((c & 0xf) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f));
      else {
        const cp = (((c & 7) << 18) | ((bytes[i++] & 0x3f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f)) - 0x10000;
        out += String.fromCharCode(0xd800 + (cp >> 10), 0xdc00 + (cp & 0x3ff));
      }
    }
    return out;
  };
}
if (typeof TextEncoder === "undefined") {
  globalThis.TextEncoder = function TextEncoder() {};
  globalThis.TextEncoder.prototype.encode = function (text) {
    const out = [];
    for (let i = 0; i < text.length; i++) {
      let c = text.charCodeAt(i);
      if (c >= 0xd800 && c <= 0xdbff && i + 1 < text.length) c = ((c - 0xd800) << 10) + (text.charCodeAt(++i) - 0xdc00) + 0x10000;
      if (c < 0x80) out.push(c);
      else if (c < 0x800) out.push(0xc0 | (c >> 6), 0x80 | (c & 0x3f));
      else if (c < 0x10000) out.push(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
      else out.push(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
    }
    return Uint8Array.from(out);
  };
}
const __fabotBase64 = "${wasm.toString("base64")}";
function __fabotWasm() {
  const digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  const clean = __fabotBase64.replace(/=+$/, "");
  const out = new Uint8Array((clean.length * 3) >> 2);
  let bits = 0, held = 0, at = 0;
  for (let i = 0; i < clean.length; i++) {
    held = (held << 6) | digits.indexOf(clean[i]);
    bits += 6;
    if (bits >= 8) {
      bits -= 8;
      out[at++] = (held >> bits) & 0xff;
    }
  }
  return out;
}
`;

const body = glue.replace(loader, "const wasmBytes = __fabotWasm();");
mkdirSync(join(REPO, "dist"), { recursive: true });
writeFileSync(OUT, prelude + body);
console.log(`bundled ${OUT} (${(wasm.length / 1024).toFixed(0)} kB wasm, ${(readFileSync(OUT).length / 1024).toFixed(0)} kB)`);
