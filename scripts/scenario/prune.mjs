// Every run leaves a world under the engine cache, and `soak-many` a run
// directory under soak-runs/.cache; both are derived from the template and
// the bundle, and a session's batches leave thousands (20 GB and 6 GB at the
// time this was written). Each batch sweeps them to a cap, oldest first, so
// the disk holds a fixed amount rather than a week of them.
import { existsSync, readdirSync, rmSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const KEEP_WORLDS = 200;
const KEEP_CACHE = 200;
// The engine writes to a world without restamping it, so a world's mtime is
// when its run started -- this bounds a run that is still in flight.
const GRACE = 60 * 60e3;

const cacheDir = () => process.env.FABOT_XXSCREEPS_HOME ?? join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "fabot-xxscreeps");

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
  const worlds = prune(join(cacheDir(), "engine", "worlds"), KEEP_WORLDS);
  const cached = prune(join(runs, ".cache"), KEEP_CACHE);
  if (worlds || cached) console.error(`swept ${worlds} worlds, ${cached} cached runs`);
}
