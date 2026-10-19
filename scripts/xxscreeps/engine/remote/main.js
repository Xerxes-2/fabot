// A scripted rival working remotes, one of §10.2's fixed opponents
// (ARCHITECTURE.md): `opponent.js` { remotes: [room, …] }, its home among them
// to mine its own source, roles?: the remote roles it keeps (default all),
// shift?: ticks each remote is worked alone, in turn (default all at once).
// A body it did not spawn (a soak's raid) guards the room it stands in.
// Each remote keeps a miner, a hauler carrying its pile home and a reserver,
// respawned as each dies, every remote's miner first. At home its towers
// strike the nearest enemy, a filler, kept whatever roles say, keeps the spawn
// and extensions full from storage, and a hauler fills them before storage.
let config = {};
try {
  config = require("opponent");
} catch {}
const remotes = config.remotes ?? [];
const roles = ["filler", ...(config.roles ?? ["miner", "hauler", "reserver"])];
const BODIES = {
  filler: [CARRY, CARRY, CARRY, CARRY, MOVE, MOVE],
  miner: [WORK, WORK, WORK, WORK, WORK, MOVE, MOVE, MOVE],
  hauler: [...Array(10).fill(CARRY), ...Array(5).fill(MOVE)],
  reserver: [CLAIM, CLAIM, MOVE, MOVE],
};
const to = (c, room) => c.moveTo(new RoomPosition(25, 25, room), { range: 20 });

function miner(c) {
  const source = c.room.name === c.memory.remote && c.pos.findClosestByRange(FIND_SOURCES);
  if (!source) to(c, c.memory.remote);
  else if (c.harvest(source) === ERR_NOT_IN_RANGE) c.moveTo(source);
}

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

function reserver(c) {
  const controller = c.room.name === c.memory.remote && c.room.controller;
  if (!controller) to(c, c.memory.remote);
  else if (c.reserveController(controller) === ERR_NOT_IN_RANGE) c.moveTo(controller);
}

function guard(c) {
  const foe = c.pos.findClosestByRange(FIND_HOSTILE_CREEPS);
  const hurt = c.pos.findInRange(FIND_MY_CREEPS, 1).sort((a, b) => a.hits / a.hitsMax - b.hits / b.hitsMax)[0];
  if (hurt.hits < hurt.hitsMax) c.heal(hurt);
  if (!foe) return;
  c.rangedAttack(foe);
  c.moveTo(foe, { maxRooms: 1, range: 1 });
}

function filler(c, room) {
  if (c.store.getUsedCapacity(RESOURCE_ENERGY) === 0) {
    if (c.withdraw(room.storage, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) c.moveTo(room.storage, { maxRooms: 1 });
    return;
  }
  const fill =
    c.pos.findClosestByRange(FIND_MY_STRUCTURES, {
      filter: (s) => [STRUCTURE_TOWER, STRUCTURE_SPAWN, STRUCTURE_EXTENSION].includes(s.structureType) && s.store.getFreeCapacity(RESOURCE_ENERGY) > 0,
    }) ?? room.storage;
  if (c.transfer(fill, RESOURCE_ENERGY) === ERR_NOT_IN_RANGE) c.moveTo(fill, { maxRooms: 1 });
}

module.exports.loop = function () {
  const home = Object.values(Game.rooms).find((r) => r.controller?.my);
  if (!home) return;
  const foes = home.find(FIND_HOSTILE_CREEPS);
  for (const tower of home.find(FIND_MY_STRUCTURES, { filter: (s) => s.structureType === STRUCTURE_TOWER })) {
    const foe = tower.pos.findClosestByRange(foes);
    if (foe) tower.attack(foe);
  }
  const creeps = Object.values(Game.creeps);
  for (const c of creeps) ({ filler, miner, hauler, reserver }[c.memory.role] ?? guard)(c, home);
  const spawn = home.find(FIND_MY_SPAWNS).find((s) => !s.spawning);
  const worked = config.shift ? [remotes[Math.floor(Game.time / config.shift) % remotes.length]] : remotes;
  for (const role of roles)
    for (const remote of role === "filler" ? [home.name] : worked.filter((r) => role !== "reserver" || r !== home.name))
      if (spawn && !creeps.some((c) => c.memory.remote === remote && c.memory.role === role))
        return spawn.spawnCreep(BODIES[role], `${role}-${remote}-${Game.time}`, { memory: { role, remote } });
};
