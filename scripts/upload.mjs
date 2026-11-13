// Upload a bundle (default dist/main.js) and its wasm (`scripts/rust.mjs`) to
// the Screeps server.
// Config via .env (loaded by `node --env-file-if-exists=.env`):
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_BRANCH  - code branch; omit to upload to the account's active branch
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

const token = process.env.SCREEPS_TOKEN;
if (!token) {
  console.error("SCREEPS_TOKEN is not set. Copy .env.example to .env and fill in your token.");
  process.exit(1);
}
const apiUrl = (process.env.SCREEPS_API_URL ?? "https://screeps.com/season").replace(/\/$/, "");
const branch = process.env.SCREEPS_BRANCH; // undefined -> server uses the active branch

const bundle = process.argv[2] ?? fileURLToPath(new URL("../dist/main.js", import.meta.url));
if (!bundle.endsWith(".js")) {
  console.error(`${bundle}: a bundle is a .js beside its .wasm`);
  process.exit(1);
}
const main = await readFile(bundle, "utf8");
const modules = { main, fabot_bg: { binary: (await readFile(bundle.replace(/\.js$/, ".wasm"))).toString("base64") } };
const body = JSON.stringify({ ...(branch ? { branch } : {}), modules });
// The change the bundle names (`scripts/rust.mjs`), for the record.
const banner = main.match(/^(?:var |globalThis\.)__FABOT_VERSION__ = ("(?:[^"\\]|\\.)*");/);
const version = banner ? JSON.parse(banner[1]) : "unnamed (no version banner)";

const res = await fetch(`${apiUrl}/api/user/code`, {
  method: "POST",
  headers: { "X-Token": token, "Content-Type": "application/json" },
  body,
});
const answer = await res.json().catch(() => ({}));
if (!res.ok || answer.ok !== 1) {
  console.error(`upload failed: HTTP ${res.status}`, answer);
  process.exit(1);
}
console.log(
  `uploaded ${version}, ${(body.length / 1024).toFixed(1)} KiB of JSON, to ${apiUrl} branch "${branch ?? "(active)"}"`,
);
