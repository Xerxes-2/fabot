// A scripted tower drain, one of §10.2's fixed opponents (ARCHITECTURE.md):
// bodies one tile inside `opponent.js`'s { target } edge, healing each
// other, that step home under half hits and come back once healed.
const { target } = require("opponent");
const onEdge = (p) => p.x === 0 || p.x === 49 || p.y === 0 || p.y === 49;
const inward = (p) => (p.x === 0 ? RIGHT : p.x === 49 ? LEFT : p.y === 0 ? BOTTOM : TOP);
const toward = (creep, room) => creep.moveTo(new RoomPosition(25, 25, room), { reusePath: 0 });

function heal(creep) {
  const hurt = creep.pos
    .findInRange(FIND_MY_CREEPS, 3)
    .filter((c) => c.hits < c.hitsMax)
    .sort((a, b) => a.hits - b.hits)[0];
  if (!hurt) return;
  if (creep.pos.isNearTo(hurt)) creep.heal(hurt);
  else creep.rangedHeal(hurt);
}

module.exports.loop = function () {
  const home = Object.values(Game.spawns)[0].room.name;
  for (const creep of Object.values(Game.creeps)) {
    heal(creep);
    if (creep.room.name === target) {
      if (creep.hits < creep.hitsMax / 2) toward(creep, home);
      else if (onEdge(creep.pos)) creep.move(inward(creep.pos));
    } else if (creep.hits === creep.hitsMax) toward(creep, target);
    else if (onEdge(creep.pos)) creep.move(inward(creep.pos));
  }
};
