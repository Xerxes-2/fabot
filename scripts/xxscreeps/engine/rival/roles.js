// A rival's behaviour lists, one per `opponent.js` role (§10.2).
const { aim, filler, flee, repairer, step } = require("shared");

const active = (c, part) => c.getActiveBodyparts(part) > 0;
const armed = (c) => active(c, ATTACK) || active(c, RANGED_ATTACK);
const hurt = (c) => c.hits < c.hitsMax;

// keep: a room-holder; { focus } names its towers' aim, { safeMode: "extension" }
// spends safe mode once an extension is struck.
function keep(config) {
  for (const room of Object.values(Game.rooms)) {
    if (!room.controller?.my) continue;
    const foes = room.find(FIND_HOSTILE_CREEPS);
    // One dismantled whole in a tick is seen only as one fewer.
    const extensions = room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_EXTENSION });
    const struck = extensions.some((s) => s.hits < s.hitsMax) || extensions.length < (Memory.extensions?.[room.name] ?? 0);
    Memory.extensions = { ...Memory.extensions, [room.name]: extensions.length };
    if (config.safeMode === "extension" && struck) room.controller.activateSafeMode();
    for (const tower of room.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_TOWER })) {
      const foe = aim(tower, foes, config.focus);
      if (foe) tower.attack(foe);
    }
    const mine = room.find(FIND_MY_CREEPS);
    for (const creep of mine) repairer(creep, room);
    const spawn = room.find(FIND_MY_SPAWNS).find((s) => !s.spawning);
    if (spawn && mine.length === 0 && room.energyAvailable >= 3 * 200) spawn.spawnCreep([].concat(...Array(3).fill([WORK, CARRY, MOVE])), `repairer-${Game.time}`);
  }
}

// remote: a room working { remotes: [room, …] }, its home among them to mine its
// own source; { roles } the roles it keeps (default all), { shift } the ticks
// each remote is worked alone in turn (default all at once), { flee } the range
// a remote body runs from an armed one (default never).
const BODIES = {
  filler: [CARRY, CARRY, CARRY, CARRY, MOVE, MOVE],
  miner: [WORK, WORK, WORK, WORK, WORK, MOVE, MOVE, MOVE],
  hauler: [...Array(10).fill(CARRY), ...Array(5).fill(MOVE)],
  reserver: [CLAIM, CLAIM, MOVE, MOVE],
};
const to = (c, room) => c.moveTo(new RoomPosition(25, 25, room), { range: 20 });

function workAt(c, find, act) {
  const target = c.room.name === c.memory.remote && find(c);
  if (!target) to(c, c.memory.remote);
  else if (act(c, target) === ERR_NOT_IN_RANGE) c.moveTo(target);
}

const miner = (creep) => workAt(creep, (c) => c.pos.findClosestByRange(FIND_SOURCES), (c, source) => c.harvest(source));
const reserver = (creep) => workAt(creep, (c) => c.room.controller, (c, controller) => c.reserveController(controller));

function hauler(c, home) {
  if (c.store.getFreeCapacity() === 0) c.memory.full = true;
  if (c.store.getUsedCapacity() === 0) c.memory.full = false;
  if (c.memory.full) {
    if (c.room.name !== home.name) return to(c, home.name);
    return filler(c, home);
  }
  if (c.room.name !== c.memory.remote) return to(c, c.memory.remote);
  const pile = c.room.find(FIND_DROPPED_RESOURCES).sort((a, b) => b.amount - a.amount)[0];
  if (pile && c.pickup(pile) === ERR_NOT_IN_RANGE) c.moveTo(pile);
}

// A body the rival did not spawn (a soak's raid) guards the room it stands in.
function guard(c) {
  const foe = c.pos.findClosestByRange(FIND_HOSTILE_CREEPS);
  const patient = c.pos.findInRange(FIND_MY_CREEPS, 1).sort((a, b) => a.hits / a.hitsMax - b.hits / b.hitsMax)[0];
  if (patient.hits < patient.hitsMax) c.heal(patient);
  if (!foe) return;
  c.rangedAttack(foe);
  c.moveTo(foe, { maxRooms: 1, range: 1 });
}

function remote(config) {
  const remotes = config.remotes ?? [];
  const roles = ["filler", ...(config.roles ?? ["miner", "hauler", "reserver"])];
  const home = Object.values(Game.rooms).find((r) => r.controller?.my);
  if (!home) return;
  const foes = home.find(FIND_HOSTILE_CREEPS);
  for (const tower of home.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_TOWER })) {
    const foe = aim(tower, foes);
    if (foe) tower.attack(foe);
  }
  const creeps = Object.values(Game.creeps);
  for (const c of creeps) {
    const near = config.flee && c.memory.remote ? c.pos.findInRange(FIND_HOSTILE_CREEPS, config.flee).filter(armed) : [];
    if (near.length) flee(c, near, config.flee + 1);
    else ({ filler, miner, hauler, reserver }[c.memory.role] ?? guard)(c, home);
  }
  const spawn = home.find(FIND_MY_SPAWNS).find((s) => !s.spawning);
  const worked = config.shift ? [remotes[Math.floor(Game.time / config.shift) % remotes.length]] : remotes;
  for (const role of roles)
    for (const remote of role === "filler" ? [home.name] : worked.filter((r) => role !== "reserver" || r !== home.name))
      if (spawn && !creeps.some((c) => c.memory.remote === remote && c.memory.role === role)) return spawn.spawnCreep(BODIES[role], `${role}-${remote}-${Game.time}`, { memory: { role, remote } });
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

function raid() {
  for (const room of Object.values(Game.rooms)) {
    const foes = room.find(FIND_HOSTILE_CREEPS);
    const allies = room.find(FIND_MY_CREEPS);
    for (const creep of allies) {
      if (active(creep, ATTACK)) swing(creep, foes);
      else if (active(creep, RANGED_ATTACK)) gun(creep, foes);
      else if (active(creep, HEAL)) healer(creep, allies);
    }
  }
}

// drain: { target }, bodies one tile inside its edge healing each other, home
// under half hits and back once healed.
const onEdge = (p) => p.x === 0 || p.x === 49 || p.y === 0 || p.y === 49;
const inward = (p) => (p.x === 0 ? RIGHT : p.x === 49 ? LEFT : p.y === 0 ? BOTTOM : TOP);
const toward = (creep, room) => creep.moveTo(new RoomPosition(25, 25, room), { reusePath: 0 });

function mend(creep) {
  const patient = creep.pos.findInRange(FIND_MY_CREEPS, 3).filter(hurt).sort((a, b) => a.hits - b.hits)[0];
  if (!patient) return;
  if (creep.pos.isNearTo(patient)) creep.heal(patient);
  else creep.rangedHeal(patient);
}

function drain(config) {
  const home = Object.values(Game.spawns)[0].room.name;
  for (const creep of Object.values(Game.creeps)) {
    mend(creep);
    if (creep.room.name === config.target) {
      if (creep.hits < creep.hitsMax / 2) toward(creep, home);
      else if (onEdge(creep.pos)) creep.move(inward(creep.pos));
    } else if (creep.hits === creep.hitsMax) toward(creep, config.target);
    else if (onEdge(creep.pos)) creep.move(inward(creep.pos));
  }
}

// harass: { targets }, each body poking them in turn, shooting the nearest
// unarmed body, backing off any armed one within 5, going home to heal under 60%
// hits and on to the next room once healed or idle. The last target is the room
// they walk round, crossing it only on the way to another. { retreat }: a body
// that meets an armed one in its target room walks home and comes back `retreat`
// ticks later, as a live harasser does when it cannot win; without it the body
// stands and backs off.
const IDLE = 50;
// A body that ends a tick on an exit tile is carried into the room beyond it
// (`processor.js`), so a role that parks one there never stands where it waits.
function park(creep) {
  if (!onEdge(creep.pos)) return;
  const path = PathFinder.search(creep.pos, { pos: new RoomPosition(25, 25, creep.room.name), range: 20 }, { maxOps: 500 }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
}
// `Room.findExitTo` answers a code, not tiles: the room's own side toward `next`.
const EXIT = { 1: FIND_EXIT_TOP, 3: FIND_EXIT_RIGHT, 5: FIND_EXIT_BOTTOM, 7: FIND_EXIT_LEFT };

function wayOut(creep, next) {
  const exits = Game.map.describeExits(creep.room.name) ?? {};
  const dir = Object.keys(exits).find((d) => exits[d] === next);
  return creep.pos.findClosestByRange(creep.room.find(EXIT[dir] ?? FIND_EXIT));
}

function go(creep, room, around) {
  if (creep.room.name === around && room !== around) return creep.moveTo(wayOut(creep, room), { reusePath: 0 });
  const roomCallback = (r) => (r === around && room !== around ? false : new PathFinder.CostMatrix());
  const path = PathFinder.search(creep.pos, { pos: new RoomPosition(25, 25, room), range: 20 }, { roomCallback, maxOps: 8000 }).path;
  if (path.length > 0) creep.move(creep.pos.getDirectionTo(path[0]));
}

function harass(config) {
  const { targets } = config;
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
        else if (creep.room.name !== home) return go(creep, home, around);
        else return config.retreat ? park(creep) : undefined;
      }
      const target = targets[mem.next % targets.length];
      const foes = creep.room.find(FIND_HOSTILE_CREEPS);
      if (config.retreat && mem.away) {
        if (creep.room.name === target) return go(creep, home, around);
        if (Game.time < mem.away) return park(creep);
        mem.away = 0;
      }
      const prey = creep.pos.findClosestByRange(foes.filter((f) => !armed(f)));
      if (prey && creep.pos.inRangeTo(prey, 3)) creep.rangedAttack(prey);
      if (config.retreat && creep.room.name === target && foes.some(armed)) {
        mem.away = Game.time + config.retreat;
        return go(creep, home, around);
      }
      if (creep.room.name !== target) return go(creep, target, around);
      const threats = foes.filter((f) => armed(f) && creep.pos.inRangeTo(f, 5));
      if (threats.length > 0) return flee(creep, threats, 5);
      if (!prey && ++mem.idle > IDLE) [mem.next, mem.idle] = [mem.next + 1, 0];
      if (prey && !creep.pos.inRangeTo(prey, 3)) creep.moveTo(prey, { range: 3, reusePath: 0 });
    });
}

// pass: an ally's bodies stand where they are and strike nothing.
const pass = () => {};

module.exports = { drain, harass, keep, pass, raid, remote };
