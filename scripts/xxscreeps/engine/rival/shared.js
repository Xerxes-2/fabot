// What every role shares: a tower's aim, the walk, a room's energy.
const walk = (creep, goals, options) => {
  const path = PathFinder.search(creep.pos, goals, options).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
};

const step = (creep, goals, flee) => walk(creep, goals, { flee, maxRooms: 1 });
const flee = (creep, from, range) => walk(creep, from.map((f) => ({ pos: f.pos, range })), { flee: true });

// { focus: "soft" } takes the foe the heal its side gives it within 3 is weakest
// on, as a live player's towers focus.
function softest(tower, foes) {
  const heal = (f) => foes.filter((o) => o.pos.inRangeTo(f, 3)).reduce((n, o) => n + o.getActiveBodyparts(HEAL), 0);
  return foes.map((f) => [[heal(f), f.hits, tower.pos.getRangeTo(f)], f]).sort(([a], [b]) => a[0] - b[0] || a[1] - b[1] || a[2] - b[2])[0]?.[1];
}

const aim = (tower, foes, focus) => (focus === "soft" ? softest(tower, foes) : tower.pos.findClosestByRange(foes));

const FILL = [STRUCTURE_TOWER, STRUCTURE_SPAWN, STRUCTURE_EXTENSION];

function topUp(creep, room, margin) {
  if (creep.store.getUsedCapacity(RESOURCE_ENERGY) === 0) {
    if (creep.withdraw(room.storage, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(room.storage, { maxRooms: 1 });
    return true;
  }
  const fill = creep.pos.findClosestByRange(FIND_MY_STRUCTURES, { filter: (s) => FILL.includes(s.structureType) && s.store.getFreeCapacity(RESOURCE_ENERGY) > (s.structureType === STRUCTURE_TOWER ? margin : 0) });
  if (!fill) return false;
  if (creep.transfer(fill, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(fill, { maxRooms: 1 });
  return true;
}

// keep mends the weakest rampart once nothing takes energy; remote pours the
// surplus back into storage.
function repairer(creep, room) {
  if (topUp(creep, room, 500)) return;
  const weakest = room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_RAMPART }).sort((a, b) => a.hits - b.hits || a.pos.y - b.pos.y)[0];
  if (weakest && creep.repair(weakest) === ERR_NOT_IN_RANGE) creep.moveTo(weakest, { range: 3, maxRooms: 1 });
}

function filler(creep, room) {
  if (!topUp(creep, room, 0) && creep.transfer(room.storage, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(room.storage, { maxRooms: 1 });
}

module.exports = { aim, filler, flee, repairer, step };
