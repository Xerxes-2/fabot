// Upload a bundle (default dist/main.js) and its wasm (`scripts/rust.mjs`) to
// the Screeps server.
// Config via .env (loaded by `node --env-file-if-exists=.env`):
//   SCREEPS_TOKEN   - auth token (required)
//   SCREEPS_API_URL - API base, default https://screeps.com/season (seasonal server)
//   SCREEPS_BRANCH  - code branch; omit to upload to the account's active branch
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

// The bundle as the engine's modules; a `main` given wraps it as module `fabot`.
export const modulesOf = (bundle, main) => {
  if (!bundle.endsWith(".js")) throw new Error(`${bundle}: a bundle is a .js beside its .wasm`);
  const code = readFileSync(bundle, "utf8");
  return { ...(main ? { main, fabot: code } : { main: code }), fabot_bg: { binary: readFileSync(bundle.replace(/\.js$/, ".wasm")).toString("base64") } };
};

// POSTs `modules`; throws unless the backend answers ok.
export async function upload(apiUrl, headers, modules, branch) {
  const body = JSON.stringify({ ...(branch ? { branch } : {}), modules });
  const res = await fetch(`${apiUrl}/api/user/code`, { method: "POST", headers: { ...headers, "Content-Type": "application/json" }, body });
  const answer = await res.json().catch(() => ({}));
  if (!res.ok || answer.ok !== 1) throw new Error(`upload failed: HTTP ${res.status} ${JSON.stringify(answer)}`);
  return body.length;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const token = process.env.SCREEPS_TOKEN;
  if (!token) {
    console.error("SCREEPS_TOKEN is not set. Copy .env.example to .env and fill in your token.");
    process.exit(1);
  }
  const apiUrl = (process.env.SCREEPS_API_URL ?? "https://screeps.com/season").replace(/\/$/, "");
  const branch = process.env.SCREEPS_BRANCH; // undefined -> server uses the active branch
  const modules = modulesOf(process.argv[2] ?? fileURLToPath(new URL("../dist/main.js", import.meta.url)));
  // The change the bundle names (`scripts/rust.mjs`), for the record.
  const banner = modules.main.match(/^(?:var |globalThis\.)__FABOT_VERSION__ = ("(?:[^"\\]|\\.)*");/);
  const version = banner ? JSON.parse(banner[1]) : "unnamed (no version banner)";
  const bytes = await upload(apiUrl, { "X-Token": token }, modules, branch);
  console.log(`uploaded ${version}, ${(bytes / 1024).toFixed(1)} KiB of JSON, to ${apiUrl} branch "${branch ?? "(active)"}"`);
}
