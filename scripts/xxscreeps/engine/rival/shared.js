// What every role shares: a tower's aim, the walk, a room's energy.
const walk = (creep, goals, options) => {
  const path = PathFinder.search(creep.pos, goals, options).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
};

const step = (creep, goals, flee) => walk(creep, goals, { flee, maxRooms: 1 });

// `engine::tower_attack_at`: 600 to range 5, 450 gone from range 20 out.
const towerDamage = (range) => (range <= 5 ? 600 : Math.floor(600 - 30 * (Math.min(range, 20) - 5)));

// { fire: "outheal" }: the towers' damage on one foe summed against the heal its
// own side gives it, as a live rival held its volley.
function outhealed(towers, foe, foes) {
  const damage = towers.reduce((n, t) => n + towerDamage(t.pos.getRangeTo(foe)), 0);
  const heal = foes
    .filter((f) => f.id === foe.id || f.pos.inRangeTo(foe, 3))
    .reduce((n, f) => n + f.getActiveBodyparts(HEAL) * (f.pos.isNearTo(foe) ? 12 : 4), 0);
  return heal >= damage;
}

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

module.exports = { aim, filler, outhealed, repairer, softest, step };
