// The owner's first room, ranked offline by the bot's own rules (`sim start-rooms`):
// each free room of a world seated as a respawn is, Strategy's claim score,
// the outposts it opens at RCL2 and Layout's plan and first spawn.
//
// Usage: npm run start-rooms -- <world.json> [room,…]
//   <world.json>  a region: scripts/capture-world.mjs's capture of the
//                 server, or scripts/scenario/season-world.mjs's of an archive
//   room,…        the candidates (default every free controller in it)
import { spawnSync } from "node:child_process";
import { simBinary } from "./sim/run.mjs";

const args = process.argv.slice(2);
if (args.length < 1 || args.length > 2) {
  console.error("usage: npm run start-rooms -- <world.json> [room,…]");
  process.exit(1);
}
const bin = simBinary();
if (!bin) process.exit(1);
const res = spawnSync(bin, ["start-rooms", ...args], { encoding: "utf8", stdio: ["ignore", "inherit", "pipe"], maxBuffer: 1 << 26 });
// `Map::stand` says where it seats each spawn, which the table shows.
process.stderr.write(res.stderr.split("\n").filter((l) => !l.startsWith("spawn at")).join("\n"));
process.exit(res.status ?? 1);
