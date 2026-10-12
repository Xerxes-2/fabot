// Both sides of an exchange duel (`exchange.mjs`) run this: the simplest
// Close/Kite emitter, not the bot's tactics. A swing closes on the nearest
// foe; a gun holds three off it and flees a swing inside two; a healer keeps
// to its most hurt ally; a WORK body dismantles the nearest tower or rampart; towers
// fire on the nearest foe.
const active = (c, part) => c.getActiveBodyparts(part) > 0;
const hurt = (c) => c.hits < c.hitsMax;

function step(creep, goals, flee) {
  const path = PathFinder.search(creep.pos, goals, { flee, maxRooms: 1 }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
}

function mend(creep, allies) {
  const patient = allies.filter(hurt).sort((a, b) => a.hits / a.hitsMax - b.hits / b.hitsMax)[0];
  if (!patient) return false;
  const range = creep.pos.getRangeTo(patient);
  if (range <= 1) creep.heal(patient);
  else if (range <= 3) creep.rangedHeal(patient);
  return range;
}

function act(creep, allies, foes) {
  const target = creep.pos.findClosestByRange(foes);
  if (active(creep, ATTACK)) {
    if (!target) return;
    if (creep.pos.isNearTo(target)) creep.attack(target);
    else if (active(creep, HEAL) && hurt(creep)) creep.heal(creep);
    step(creep, [{ pos: target.pos, range: target.pos.isNearTo(creep) ? 0 : 1 }], false);
    return;
  }
  if (active(creep, RANGED_ATTACK)) {
    if (active(creep, HEAL)) {
      if (hurt(creep)) creep.heal(creep);
      else mend(creep, allies);
    }
    if (!target) return;
    const near = foes.filter((f) => creep.pos.getRangeTo(f) <= 1).length;
    if (near >= 2) creep.rangedMassAttack();
    else if (creep.pos.getRangeTo(target) <= 3) creep.rangedAttack(target);
    const swings = foes.filter((f) => active(f, ATTACK) && creep.pos.getRangeTo(f) <= 2);
    if (swings.length > 0) step(creep, swings.map((f) => ({ pos: f.pos, range: 3 })), true);
    else if (creep.pos.getRangeTo(target) > 3) step(creep, [{ pos: target.pos, range: 3 }], false);
    return;
  }
  if (active(creep, HEAL)) {
    const others = allies.filter((a) => a !== creep);
    const range = mend(creep, allies);
    const patient = others.filter(hurt).sort((a, b) => a.hits - b.hits)[0] ?? creep.pos.findClosestByRange(others);
    if (patient && range !== 0) step(creep, [{ pos: patient.pos, range: 1 }], false);
    else if (!patient && target) step(creep, foes.map((f) => ({ pos: f.pos, range: 4 })), true);
    return;
  }
  if (active(creep, WORK)) {
    const kinds = [STRUCTURE_TOWER, STRUCTURE_RAMPART];
    const goal = creep.pos.findClosestByRange(FIND_HOSTILE_STRUCTURES, { filter: (s) => kinds.includes(s.structureType) });
    if (!goal) return;
    if (creep.pos.isNearTo(goal)) creep.dismantle(goal);
    else step(creep, [{ pos: goal.pos, range: 1 }], false);
  }
}

module.exports.loop = function () {
  for (const room of Object.values(Game.rooms)) {
    const foes = room.find(FIND_HOSTILE_CREEPS);
    const allies = room.find(FIND_MY_CREEPS);
    for (const tower of room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_TOWER })) {
      const target = tower.pos.findClosestByRange(foes);
      if (target) tower.attack(target);
    }
    for (const creep of allies) act(creep, allies, foes);
  }
};
