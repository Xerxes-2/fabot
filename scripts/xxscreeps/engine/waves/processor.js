// A soak's timed Invader wave (ARCHITECTURE.md §10.2, nodes E1 and S1): the
// raid the engine sends after a room's harvest goal, sent when the soak asks
// (`soak-step.mjs` pushes the internal intent `soakWave` with the exit sides
// that lead to no owned or reserved room, and the wave's `role[:size]`s).
// xxscreeps' own goal doubles after each raid, so it gives no steady rate to
// read a stand-down's cost off.
// `soakRaid` stands a scripted raid (`raid/`) the same way: the owner's bodies
// on the exit tiles of the sides given.
// `soakFull` stands a room's spawns and extensions full, the economy's refill when it is switched off.
import { registerIntentProcessor } from "xxscreeps/engine/processor/index.js";
import { Game } from "xxscreeps/game/index.js";
import { FIND_EXIT } from "xxscreeps/game/constants/index.js";
import { Room } from "xxscreeps/game/room/index.js";
import { CREEP_LIFE_TIME } from "xxscreeps/mods/classic/creep/constants.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { create } from "xxscreeps/mods/classic/invader/processor.js";
import { activateNPC } from "xxscreeps/mods/npc/processor.js";
import { createWithBody } from "xxscreeps/mods/classic/creep/creep.js";

const side = { 1: (p) => p.y === 0, 3: (p) => p.x === 49, 5: (p) => p.y === 49, 7: (p) => p.x === 0 };

function edge(room, open) {
  const exits = room.find(FIND_EXIT).filter((p) => open.some((d) => side[d](p)));
  if (exits.length === 0) return exits;
  const origin = exits[Math.floor(exits.length * Math.random())];
  return exits.sort((a, b) => origin.getRangeTo(a) - origin.getRangeTo(b));
}

registerIntentProcessor(Room, "soakWave", { internal: true }, (room, context, open, roles) => {
  const exits = edge(room, open);
  if (exits.length === 0) return;
  activateNPC(room, kInvaderUserId);
  roles.forEach((spec, i) => {
    const [role, size = "small"] = spec.split(":");
    if (exits[i]) room["#insertObject"](create(exits[i], role, size, Game.time + CREEP_LIFE_TIME));
  });
  context.setActive();
});

registerIntentProcessor(Room, "soakFull", { internal: true }, (room, context) => {
  for (const o of room["#objects"])
    if ((o.structureType === "spawn" || o.structureType === "extension") && o["#user"] === room["#user"] && o.store.getFreeCapacity("energy") > 0) {
      o.store["#add"]("energy", o.store.getFreeCapacity("energy"));
      context.didUpdate();
    }
});

registerIntentProcessor(Room, "soakRaid", { internal: true }, (room, context, owner, bodies, open) => {
  const exits = edge(room, open);
  bodies.forEach((parts, i) => {
    if (!exits[i]) return;
    const creep = createWithBody(exits[i], parts.map(({ type, boost }) => ({ type, hits: 100, boost })), `raid-${Game.time}-${i}`, owner);
    creep["#ageTime"] = Game.time + CREEP_LIFE_TIME;
    room["#insertObject"](creep);
  });
  context.setActive();
});
