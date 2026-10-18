// A scripted rival holding its room, one of §10.2's fixed opponents
// (ARCHITECTURE.md): its towers strike the nearest enemy, and one repairer
// keeps them, the spawn and the extensions full from storage and mends the
// weakest rampart. `opponent.js` { safeMode: "extension" } activates safe
// mode once one of its extensions is struck, as a live rival did.
let config = {};
try {
  config = require("opponent");
} catch {}
const BLOCK = [WORK, CARRY, MOVE];

function repairer(creep, room) {
  if (creep.store.getUsedCapacity(RESOURCE_ENERGY) === 0) {
    if (creep.withdraw(room.storage, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(room.storage, { maxRooms: 1 });
    return;
  }
  const empty = room.find(FIND_MY_STRUCTURES, {
    filter: (s) => [STRUCTURE_TOWER, STRUCTURE_SPAWN, STRUCTURE_EXTENSION].includes(s.structureType) && s.store.getFreeCapacity(RESOURCE_ENERGY) > (s.structureType === STRUCTURE_TOWER ? 500 : 0),
  });
  const fill = creep.pos.findClosestByRange(empty);
  if (fill) {
    if (creep.transfer(fill, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(fill, { maxRooms: 1 });
    return;
  }
  const ramparts = room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_RAMPART });
  const weakest = ramparts.sort((a, b) => a.hits - b.hits || a.pos.y - b.pos.y)[0];
  if (weakest && creep.repair(weakest) === ERR_NOT_IN_RANGE) creep.moveTo(weakest, { range: 3, maxRooms: 1 });
}

module.exports.loop = function () {
  for (const room of Object.values(Game.rooms)) {
    if (!room.controller?.my) continue;
    const foes = room.find(FIND_HOSTILE_CREEPS);
    // One dismantled whole in a tick is seen only as one fewer.
    const extensions = room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_EXTENSION });
    const struck = extensions.some((s) => s.hits < s.hitsMax) || extensions.length < (Memory.extensions?.[room.name] ?? 0);
    Memory.extensions = { ...Memory.extensions, [room.name]: extensions.length };
    if (config.safeMode === "extension" && struck) room.controller.activateSafeMode();
    for (const tower of room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_TOWER })) {
      const foe = tower.pos.findClosestByRange(foes);
      if (foe) tower.attack(foe);
    }
    const mine = room.find(FIND_MY_CREEPS);
    for (const creep of mine) repairer(creep, room);
    const spawn = room.find(FIND_MY_SPAWNS).find((s) => !s.spawning);
    if (spawn && mine.length === 0 && room.energyAvailable >= 3 * 200) spawn.spawnCreep([].concat(...Array(3).fill(BLOCK)), `repairer-${Game.time}`);
  }
};
