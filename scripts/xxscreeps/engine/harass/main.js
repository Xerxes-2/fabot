// A scripted flank harasser, one of §10.2's fixed opponents (ARCHITECTURE.md):
// each body pokes the rooms of `opponent.js`'s { targets } in turn, shooting
// the nearest unarmed body, backing off any armed one, going home to heal
// under 60% hits and moving on to the next room once healed or idle. Bodies
// stand on the edge of the colony's home (`soak-step.mjs`): they walk round
// it, under its towers only when it is the room they poke.
const { targets } = require("opponent");
const armed = (c) => c.getActiveBodyparts(ATTACK) + c.getActiveBodyparts(RANGED_ATTACK) > 0;
const IDLE = 50;

function go(creep, room, around) {
  if (creep.room.name === around && room !== around) {
    const exit = creep.pos.findClosestByRange(creep.room.findExitTo(Object.values(Game.spawns)[0].room.name));
    return exit && creep.moveTo(exit, { reusePath: 0 });
  }
  const roomCallback = (r) => (r === around && room !== around ? false : new PathFinder.CostMatrix());
  const path = PathFinder.search(creep.pos, { pos: new RoomPosition(25, 25, room), range: 20 }, { roomCallback, maxOps: 8000 }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
}

function flee(creep, from) {
  const path = PathFinder.search(creep.pos, from.map((f) => ({ pos: f.pos, range: 5 })), { flee: true }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
}

module.exports.loop = function () {
  const home = Object.values(Game.spawns)[0].room.name;
  const around = targets.at(-1);
  Object.keys(Game.creeps)
    .sort()
    .forEach((name, i) => {
      const creep = Game.creeps[name];
      const mem = creep.memory;
      mem.next ??= i;
      mem.idle ??= 0;
      if (creep.hits < creep.hitsMax) creep.heal(creep);
      if (creep.hits < creep.hitsMax * 0.6) mem.healing = true;
      if (mem.healing) {
        if (creep.hits === creep.hitsMax) [mem.healing, mem.next, mem.idle] = [false, mem.next + 1, 0];
        else return creep.room.name === home ? undefined : go(creep, home, around);
      }
      const target = targets[mem.next % targets.length];
      const foes = creep.room.find(FIND_HOSTILE_CREEPS);
      const prey = creep.pos.findClosestByRange(foes.filter((f) => !armed(f)));
      if (prey && creep.pos.inRangeTo(prey, 3)) creep.rangedAttack(prey);
      if (creep.room.name !== target) return go(creep, target, around);
      const threats = foes.filter((f) => armed(f) && creep.pos.inRangeTo(f, 5));
      if (threats.length > 0) return flee(creep, threats);
      if (!prey && ++mem.idle > IDLE) [mem.next, mem.idle] = [mem.next + 1, 0];
      if (prey && !creep.pos.inRangeTo(prey, 3)) creep.moveTo(prey, { range: 3, reusePath: 0 });
    });
};
