// `soak-many` leaves a run directory under soak-runs/.cache, derived from the
// world and the binary, and a session's batches leave thousands. Each batch
// sweeps them to a cap, oldest first, so the disk holds a fixed amount rather
// than a week of them.
import { existsSync, readdirSync, rmSync, statSync } from "node:fs";
import { join } from "node:path";

const KEEP_CACHE = 200;
// A run younger than this may still be in flight in a parallel batch.
const GRACE = 60 * 60e3;

// A name that vanishes under us (a parallel sweep) is not ours to count.
function prune(dir, keep) {
  if (!existsSync(dir)) return 0;
  const aged = [];
  for (const name of readdirSync(dir)) {
    try {
      aged.push({ name, at: statSync(join(dir, name)).mtimeMs });
    } catch {}
  }
  aged.sort((a, b) => b.at - a.at);
  const cutoff = Date.now() - GRACE;
  const gone = aged.slice(keep).filter(({ at }) => at < cutoff);
  for (const { name } of gone) rmSync(join(dir, name), { recursive: true, force: true });
  return gone.length;
}

// `runs` is this checkout's soak-runs directory.
export function sweep(runs) {
  const cached = prune(join(runs, ".cache"), KEEP_CACHE);
  if (cached) console.error(`swept ${cached} cached runs`);
}
