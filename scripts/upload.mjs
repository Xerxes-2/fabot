// Upload dist/main.js to the Screeps server as the `main` module.
// Config via .env (loaded by `node --env-file-if-exists=.env`):
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_BRANCH  - code branch; omit to upload to the account's active branch
import { readFile } from "node:fs/promises";

const token = process.env.SCREEPS_TOKEN;
if (!token) {
  console.error("SCREEPS_TOKEN is not set. Copy .env.example to .env and fill in your token.");
  process.exit(1);
}
const apiUrl = (process.env.SCREEPS_API_URL ?? "https://screeps.com/season").replace(/\/$/, "");
const branch = process.env.SCREEPS_BRANCH; // undefined -> server uses the active branch

const main = await readFile(new URL("../dist/main.js", import.meta.url), "utf8");

const res = await fetch(`${apiUrl}/api/user/code`, {
  method: "POST",
  headers: { "X-Token": token, "Content-Type": "application/json" },
  body: JSON.stringify({ ...(branch ? { branch } : {}), modules: { main } }),
});
const body = await res.json().catch(() => ({}));
if (!res.ok || body.ok !== 1) {
  console.error(`upload failed: HTTP ${res.status}`, body);
  process.exit(1);
}
console.log(`uploaded ${(main.length / 1024).toFixed(1)} KiB to ${apiUrl} branch "${branch ?? "(active)"}"`);
