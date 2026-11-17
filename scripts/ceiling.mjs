// The line and comment-share ceilings in scripts/ceilings.json, checked, and
// no issue or ADR citation in code. A directory's line count is every line of
// every .rs beneath it, minus a `<dir>/kinds/*` entry's files, which are each
// held to their own ceiling. A comment share is per directory of the bot's
// source (`src`, `core`, `sim`), over its own .rs files. Neither counts a `#[cfg(test)]` item nor
// the shared suites under `core/tests`. Exits non-zero on any breach.
//
// Usage: node scripts/ceiling.mjs
import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";

const { lines: ceilings, comments: commentPct } = JSON.parse(
  readFileSync(new URL("./ceilings.json", import.meta.url), "utf8"),
);

const ROOTS = ["src", "core", "sim"];
const SUITES = "core/tests";

const rsFiles = (dir) =>
  readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return rsFiles(path);
    return name.endsWith(".rs") ? [path] : [];
  });
const shipped = (dir) =>
  rsFiles(dir).filter((f) => !f.startsWith(SUITES + "/") && !f.startsWith("src/bin/"));

// The lines the bot ships: a top-level `#[cfg(test)]` item is dropped, to the
// `;` that ends a declaration or the `}` at column 0 that closes a block.
const shippedLines = (file) => {
  const text = readFileSync(file, "utf8").split("\n").slice(0, -1);
  const out = [];
  for (let i = 0; i < text.length; i++) {
    if (text[i] !== "#[cfg(test)]") {
      out.push(text[i]);
      continue;
    }
    if (text[i + 1]?.trimEnd().endsWith(";")) i += 1;
    else while (i < text.length && text[i] !== "}") i++;
  }
  return out;
};
const count = (files) =>
  files.reduce(
    ([lines, comments], f) => {
      const text = shippedLines(f);
      return [
        lines + text.length,
        comments + text.filter((l) => /^\s*\/\//.test(l)).length,
      ];
    },
    [0, 0],
  );

const perFile = Object.keys(ceilings)
  .filter((key) => key.endsWith("/*"))
  .map((key) => key.slice(0, -2));

const rows = [];
for (const [key, ceiling] of Object.entries(ceilings)) {
  if (key.endsWith("/*")) {
    const dir = key.slice(0, -2);
    if (!existsSync(dir)) continue;
    for (const file of shipped(dir))
      rows.push([file, count([file])[0], ceiling]);
  } else {
    if (!existsSync(key)) continue;
    const files = shipped(key).filter(
      (f) => !perFile.some((dir) => dir !== key && f.startsWith(dir + "/")),
    );
    rows.push([key, count(files)[0], ceiling]);
  }
}

// AGENTS.md: no issue numbers or ADR citations in code; git log has them.
const cited = ROOTS.flatMap(rsFiles).flatMap((file) =>
  readFileSync(file, "utf8")
    .split("\n")
    .map((line, i) => [file, i + 1, line])
    .filter(([, , line]) => /#\d{2,}\b|\bADR[- ]?\d{3,4}\b/.test(line)),
);
for (const [file, n, line] of cited)
  console.error(`CITE ${file}:${n}: ${line.trim()}`);
if (cited.length > 0) {
  console.error(
    `ceiling: ${cited.length} issue/ADR citation(s) in code; git log carries them (AGENTS.md)`,
  );
  process.exit(1);
}

const dirs = [
  ...new Set(ROOTS.flatMap(shipped).map((f) => f.slice(0, f.lastIndexOf("/")))),
];
const shares = dirs.map((dir) => {
  const [n, comments] = count(
    shipped(dir).filter((f) => f.lastIndexOf("/") === dir.length),
  );
  return [dir, (100 * comments) / n, commentPct[dir] ?? commentPct._default];
});

const over =
  rows.filter(([, n, c]) => n > c).length +
  shares.filter(([, pct, most]) => pct > most).length;
for (const [path, n, c] of rows)
  console.log(`${n > c ? "OVER" : "ok  "} ${path} ${n}/${c} lines`);
for (const [dir, pct, most] of shares)
  console.log(
    `${pct > most ? "OVER" : "ok  "} ${dir} ${pct.toFixed(1)}%/${most}% comments`,
  );
if (over > 0) {
  console.error(
    `ceiling: ${over} over; delete before adding, comments first (AGENTS.md)`,
  );
  process.exit(1);
}
if (rows.length === 0) console.log("ceiling: no module built yet");
