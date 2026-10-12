// A scripted raid, one of §10.2's fixed opponents (ARCHITECTURE.md).
const active = (c, part) => c.getActiveBodyparts(part) > 0;
const hurt = (c) => c.hits < c.hitsMax;

function step(creep, goals, flee) {
  const path = PathFinder.search(creep.pos, goals, { flee, maxRooms: 1 }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
}

function targetOf(creep, foes) {
  const foe = creep.pos.findClosestByRange(foes);
  if (foe) return foe;
  const spawn = creep.pos.findClosestByRange(FIND_HOSTILE_SPAWNS);
  if (spawn) return spawn;
  return creep.pos.findClosestByRange(FIND_HOSTILE_STRUCTURES, { filter: (s) => s.structureType !== STRUCTURE_CONTROLLER });
}

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
  const others = allies.filter((a) => a !== creep);
  const patient = allies.filter(hurt).sort((a, b) => a.hits / a.hitsMax - b.hits / b.hitsMax)[0];
  if (patient) {
    if (creep.pos.isNearTo(patient)) creep.heal(patient);
    else if (creep.pos.inRangeTo(patient, 3)) creep.rangedHeal(patient);
  }
  const lead = patient && patient !== creep ? patient : creep.pos.findClosestByRange(others.filter((a) => active(a, ATTACK) || active(a, RANGED_ATTACK)));
  if (lead && !creep.pos.isNearTo(lead)) step(creep, [{ pos: lead.pos, range: 1 }], false);
}

module.exports.loop = function () {
  for (const room of Object.values(Game.rooms)) {
    const foes = room.find(FIND_HOSTILE_CREEPS);
    const allies = room.find(FIND_MY_CREEPS);
    for (const creep of allies) {
      if (active(creep, ATTACK)) swing(creep, foes);
      else if (active(creep, RANGED_ATTACK)) gun(creep, foes);
      else if (active(creep, HEAL)) healer(creep, allies);
    }
  }
};
