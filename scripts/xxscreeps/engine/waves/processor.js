// A soak's timed Invader wave (ARCHITECTURE.md §10.2, nodes E1 and S1): the
// raid the engine sends after a room's harvest goal, sent when the soak asks
// (`soak-step.mjs` pushes the internal intent `soakWave` with the exit sides
// that lead to no owned or reserved room). xxscreeps' own goal doubles after
// each raid, so it gives no steady rate to read a stand-down's cost off.
import { registerIntentProcessor } from "xxscreeps/engine/processor/index.js";
import { Game } from "xxscreeps/game/index.js";
import { FIND_EXIT } from "xxscreeps/game/constants/index.js";
import { Room } from "xxscreeps/game/room/index.js";
import { CREEP_LIFE_TIME } from "xxscreeps/mods/classic/creep/constants.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { create } from "xxscreeps/mods/classic/invader/processor.js";
import { activateNPC } from "xxscreeps/mods/npc/processor.js";

const side = { 1: (p) => p.y === 0, 3: (p) => p.x === 49, 5: (p) => p.y === 49, 7: (p) => p.x === 0 };

registerIntentProcessor(Room, "soakWave", { internal: true }, (room, context, ...open) => {
  const exits = room.find(FIND_EXIT).filter((p) => open.some((d) => side[d](p)));
  if (exits.length === 0) return;
  const origin = exits[Math.floor(exits.length * Math.random())];
  exits.sort((a, b) => origin.getRangeTo(a) - origin.getRangeTo(b));
  activateNPC(room, kInvaderUserId);
  ["melee", "healer", "ranged"].forEach((role, i) => exits[i] && room["#insertObject"](create(exits[i], role, "small", Game.time + CREEP_LIFE_TIME)));
  context.setActive();
});
