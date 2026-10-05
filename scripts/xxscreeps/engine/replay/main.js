// Our side replaying the arena run (#543), so only the other side's AI can
// make the two traces part: each tick, each of our bodies steps toward
// where the arena had it at the end of that tick, and casts the attacks and
// heals the arena's pipeline cast, on whoever of those stands in reach.
// `replay.js` (`{ start, ticks: [{ <creep>: { at: [room, x, y], acts:
// [[verb, target]] } }] }`, tick 0 the arena's first step) is written beside
// this file per run.
const { start, ticks } = require("replay");

const parse = (name) => {
  const [, ew, x, ns, y] = /^([WE])(\d+)([NS])(\d+)$/.exec(name);
  return [ew === "E" ? Number(x) : -Number(x) - 1, ns === "S" ? Number(y) : -Number(y) - 1];
};
// A tile in world coordinates, so a step across a seam is a step.
const world = (room, x, y) => {
  const [rx, ry] = parse(room);
  return [rx * 50 + x, ry * 50 + y];
};
const DIRECTIONS = { "-1,-1": TOP_LEFT, "0,-1": TOP, "1,-1": TOP_RIGHT, "1,0": RIGHT, "1,1": BOTTOM_RIGHT, "0,1": BOTTOM, "-1,1": BOTTOM_LEFT, "-1,0": LEFT };

module.exports.loop = function () {
  const plan = ticks[Game.time - start] ?? {};
  for (const creep of Object.values(Game.creeps)) {
    const mine = plan[creep.name];
    if (!mine) continue;
    const byName = (name) => (name === creep.name ? creep : creep.room.find(FIND_CREEPS).find((c) => c.name === name));
    for (const [verb, name] of mine.acts) {
      const target = name && byName(name);
      if (verb === "attack" && target && creep.pos.isNearTo(target)) creep.attack(target);
      else if (verb === "rangedAttack" && target && creep.pos.inRangeTo(target, 3)) creep.rangedAttack(target);
      else if (verb === "heal" && target && creep.pos.isNearTo(target)) creep.heal(target);
      else if (verb === "rangedHeal" && target && creep.pos.inRangeTo(target, 3)) creep.rangedHeal(target);
      else if (verb === "rangedMassAttack") creep.rangedMassAttack();
    }
    const [room, x, y] = mine.at;
    let [gx, gy] = world(room, x, y);
    const [cx, cy] = world(creep.room.name, creep.pos.x, creep.pos.y);
    // The arena writes a crossing as its landing tile; the step went to the exit tile.
    if (room !== creep.room.name) {
      gx += x === 0 ? -1 : x === 49 ? 1 : 0;
      gy += y === 0 ? -1 : y === 49 ? 1 : 0;
    }
    const dx = gx - cx;
    const dy = gy - cy;
    if (dx === 0 && dy === 0) continue;
    if (Math.abs(dx) <= 1 && Math.abs(dy) <= 1) creep.move(DIRECTIONS[`${dx},${dy}`]);
    else creep.moveTo(new RoomPosition(x, y, room), { reusePath: 0 });
  }
};
