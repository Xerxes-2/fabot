// Runs a soak's setup commands in one process, in order: each process otherwise spends
// about 0.3 s importing xxscreeps and opening the database before its work.
// xxscreeps.js file://…/batch.mjs <commands.json>: a list of the argument lists a
// command takes after xxscreeps.js ("manage" or a script's file URL, then its args).
// A module runs once per URL, so each command is imported under a query of its own.
import { readFileSync } from "node:fs";

const commands = JSON.parse(readFileSync(process.argv[2], "utf8"));
const random = Math.random;
for (const [i, [entry, ...args]] of commands.entries()) {
  const url = entry === "manage" ? import.meta.resolve("xxscreeps/scripts/manage.js") : entry;
  process.argv = [process.argv[0], url, ...args];
  await import(`${url}?batch=${i}`);
  Math.random = random;
  if (process.exitCode) {
    console.error(`batch: ${[entry, ...args].join(" ")} failed (exit ${process.exitCode})`);
    process.exit(process.exitCode);
  }
}
