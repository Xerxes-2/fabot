// A rival's behaviour lists, one per `opponent.js` role (§10.2).
const FILL = [STRUCTURE_TOWER, STRUCTURE_SPAWN, STRUCTURE_EXTENSION];
const step = (creep, goals, flee) => {
  const path = PathFinder.search(creep.pos, goals, { flee, maxRooms: 1 }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
};

// The towers are topped up to within one volley's worth before any rampart is mended.
function repairer(creep, room) {
  if (creep.store.getUsedCapacity(RESOURCE_ENERGY) === 0) {
    if (creep.withdraw(room.storage, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(room.storage, { maxRooms: 1 });
    return;
  }
  const fill = creep.pos.findClosestByRange(FIND_MY_STRUCTURES, { filter: (s) => FILL.includes(s.structureType) && s.store.getFreeCapacity(RESOURCE_ENERGY) > (s.structureType === STRUCTURE_TOWER ? 500 : 0) });
  if (fill) {
    if (creep.transfer(fill, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) creep.moveTo(fill, { maxRooms: 1 });
    return;
  }
  const weakest = room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_RAMPART }).sort((a, b) => a.hits - b.hits || a.pos.y - b.pos.y)[0];
  if (weakest && creep.repair(weakest) === ERR_NOT_IN_RANGE) creep.moveTo(weakest, { range: 3, maxRooms: 1 });
}

const active = (c, part) => c.getActiveBodyparts(part) > 0;
const hurt = (c) => c.hits < c.hitsMax;

function keep() {
  for (const room of Object.values(Game.rooms)) {
    if (!room.controller?.my) continue;
    const foes = room.find(FIND_HOSTILE_CREEPS);
    for (const tower of room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_TOWER })) {
      const foe = tower.pos.findClosestByRange(foes);
      if (foe) tower.attack(foe);
    }
    const mine = room.find(FIND_MY_CREEPS);
    for (const creep of mine) repairer(creep, room);
    const spawn = room.find(FIND_MY_SPAWNS).find((s) => !s.spawning);
    if (spawn && mine.length === 0 && room.energyAvailable >= 3 * 200) spawn.spawnCreep([].concat(...Array(3).fill([WORK, CARRY, MOVE])), `repairer-${Game.time}`);
  }
}

// raid: a gun kites a swing inside 2 and holds 3 off, a swing closes and never
// leaves, a healer keeps to the hurt.
const targetOf = (creep, foes) =>
  creep.pos.findClosestByRange(foes) ??
  creep.pos.findClosestByRange(FIND_HOSTILE_SPAWNS) ??
  creep.pos.findClosestByRange(FIND_HOSTILE_STRUCTURES, { filter: (s) => s.structureType !== STRUCTURE_CONTROLLER });

function gun(creep, foes) {
  if (active(creep, HEAL) && hurt(creep)) creep.heal(creep);
  const target = targetOf(creep, foes);
  if (!target) return;
  const range = creep.pos.getRangeTo(target);
  if (foes.filter((f) => creep.pos.getRangeTo(f) <= 1).length >= 2) creep.rangedMassAttack();
  else if (range <= 3) creep.rangedAttack(target);
  const swings = foes.filter((f) => active(f, ATTACK) && creep.pos.getRangeTo(f) <= 2);
  if (swings.length > 0) step(creep, swings.map((f) => ({ pos: f.pos, range: 3 })), true);
  else if (range > 3) step(creep, [{ pos: target.pos, range: 3 }], false);
}

function swing(creep, foes) {
  const target = targetOf(creep, foes);
  if (!target) return;
  if (creep.pos.isNearTo(target)) creep.attack(target);
  else step(creep, [{ pos: target.pos, range: 1 }], false);
}

function healer(creep, allies) {
  const patient = allies.filter(hurt).sort((a, b) => a.hits / a.hitsMax - b.hits / b.hitsMax)[0];
  if (patient) {
    if (creep.pos.isNearTo(patient)) creep.heal(patient);
    else if (creep.pos.inRangeTo(patient, 3)) creep.rangedHeal(patient);
  }
  const lead = patient && patient !== creep ? patient : creep.pos.findClosestByRange(allies.filter((a) => a !== creep && (active(a, ATTACK) || active(a, RANGED_ATTACK))));
  if (lead && !creep.pos.isNearTo(lead)) step(creep, [{ pos: lead.pos, range: 1 }], false);
}

// With nothing to strike where it stands, a raid walks on to { target }, as a
// player's would past a scout it killed.
function raid(config) {
  for (const room of Object.values(Game.rooms)) {
    const foes = room.find(FIND_HOSTILE_CREEPS);
    const allies = room.find(FIND_MY_CREEPS);
    for (const creep of allies) {
      if (config.target && room.name !== config.target && !targetOf(creep, foes)) toward(creep, config.target);
      else if (active(creep, ATTACK)) swing(creep, foes);
      else if (active(creep, RANGED_ATTACK)) gun(creep, foes);
      else if (active(creep, HEAL)) healer(creep, allies);
    }
  }
}

const toward = (creep, room) => creep.moveTo(new RoomPosition(25, 25, room), { reusePath: 0 });

module.exports = { keep, raid };
