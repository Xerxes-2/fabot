// The line ceilings in scripts/ceilings.json, checked. A directory's count is
// every line of every .fs beneath it, minus a `<dir>/Kinds/*` entry's files,
// which are each held to their own ceiling. Exits non-zero on any breach.
//
// Usage: node scripts/ceiling.mjs
import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";

const { lines: ceilings } = JSON.parse(readFileSync(new URL("./ceilings.json", import.meta.url), "utf8"));

const fsFiles = (dir) =>
  readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return name === "bin" || name === "obj" ? [] : fsFiles(path);
    return name.endsWith(".fs") ? [path] : [];
  });
const lineCount = (path) => readFileSync(path, "utf8").split("\n").length - 1;

const perFile = Object.keys(ceilings)
  .filter((key) => key.endsWith("/*"))
  .map((key) => key.slice(0, -2));

const rows = [];
for (const [key, ceiling] of Object.entries(ceilings)) {
  if (key.endsWith("/*")) {
    const dir = key.slice(0, -2);
    if (!existsSync(dir)) continue;
    for (const file of fsFiles(dir)) rows.push([file, lineCount(file), ceiling]);
  } else {
    if (!existsSync(key)) continue;
    const files = fsFiles(key).filter((f) => !perFile.some((dir) => dir !== key && f.startsWith(dir + "/")));
    rows.push([key, files.reduce((n, f) => n + lineCount(f), 0), ceiling]);
  }
}

const over = rows.filter(([, n, c]) => n > c);
for (const [path, n, c] of rows) console.log(`${n > c ? "OVER" : "ok  "} ${path} ${n}/${c}`);
if (over.length > 0) {
  console.error(`ceiling: ${over.length} over; delete before adding (AGENTS.md)`);
  process.exit(1);
}
if (rows.length === 0) console.log("ceiling: no module built yet");
