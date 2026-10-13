// A player's visible rooms are a set the processor fills in the order its
// rooms finish, which timing changes run to run: the bot reads them, as
// Game.rooms, in name order.
import { PlayerInstance } from "xxscreeps/engine/runner/instance.js";

const run = PlayerInstance.prototype.run;
PlayerInstance.prototype.run = function (time, visibleRooms, intentRooms) {
  return run.call(this, time, [...visibleRooms].sort(), [...intentRooms].sort());
};
