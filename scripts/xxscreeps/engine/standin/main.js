// The scripted stand-in for our side (#543), the #535 melee rule: chase the
// nearest hostile body, and beside it swing and step onto its tile (attacks
// resolve before moves); otherwise heal self when hurt. A body with no
// hostile in its room walks to where the scenario's first hostile stood.
// `targets.js` (`module.exports = [{ room, x, y }]`) is written beside this
// file per run.
const targets = require("targets");

module.exports.loop = function () {
  for (const creep of Object.values(Game.creeps)) {
    // A body with no ATTACK (the capture's hauler) holds.
    if (creep.getActiveBodyparts(ATTACK) === 0) continue;
    const target = creep.pos.findClosestByRange(creep.room.find(FIND_HOSTILE_CREEPS));
    if (target && creep.pos.isNearTo(target)) {
      creep.attack(target);
      creep.move(creep.pos.getDirectionTo(target));
      continue;
    }
    if (creep.hits < creep.hitsMax) creep.heal(creep);
    if (target) {
      creep.moveTo(target, { reusePath: 0, maxRooms: 1 });
    } else {
      const goal = targets.find((t) => t.room !== creep.room.name);
      if (goal) creep.moveTo(new RoomPosition(goal.x, goal.y, goal.room), { reusePath: 0 });
    }
  }
};
