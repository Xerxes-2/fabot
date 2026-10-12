// A soak's level-0 Invader core (soak.mjs --core): the expansion core a
// stronghold stands in its sector, which reserves the controller and strikes
// nothing. On the open tile nearest the room's centre, deployed, its collapse
// past any soak.
import { registerIntentProcessor } from "xxscreeps/engine/processor/index.js";
import { Game } from "xxscreeps/game/index.js";
import { RoomPosition } from "xxscreeps/game/position.js";
import { Room } from "xxscreeps/game/room/index.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { activateNPC } from "xxscreeps/mods/npc/processor.js";
import { create } from "xxscreeps/mods/modern/stronghold/invader-core.js";

const collapse = 100000;

registerIntentProcessor(Room, "soakCore", { internal: true }, (room, context) => {
  const terrain = Game.map.getRoomTerrain(room.name);
  const taken = [...room["#objects"]].map((o) => o.pos);
  const open = (x, y) => terrain.get(x, y) === 0 && !taken.some((p) => Math.abs(p.x - x) <= 1 && Math.abs(p.y - y) <= 1);
  const ring = (r) => {
    const tiles = [];
    for (let x = 25 - r; x <= 25 + r; x++) for (let y = 25 - r; y <= 25 + r; y++) if (Math.max(Math.abs(x - 25), Math.abs(y - 25)) === r) tiles.push([x, y]);
    return tiles;
  };
  for (let r = 0; r < 20; r++) {
    const tile = ring(r).find(([x, y]) => open(x, y));
    if (!tile) continue;
    const core = create(new RoomPosition(tile[0], tile[1], room.name), 0, 0);
    core["#collapseTime"] = Game.time + collapse;
    room["#insertObject"](core);
    activateNPC(room, kInvaderUserId);
    context.setActive();
    return;
  }
});
