// The fields `app/world.rs` reads, for `scripts/bench-game.mjs`
// (ARCHITECTURE.md §9.1): a field bound there and missing here reads absent
// in the replay.
const FINDS = [105, 116, 107, 111, 106, 118, 123, 103];
const KEYS = ["id", "name", "structureType", "my", "hits", "hitsMax", "cooldown", "isPublic", "progress", "progressTotal", "energy", "energyCapacity",
  "ticksToRegeneration", "mineralType", "mineralAmount", "resourceType", "amount", "level", "ticksToDowngrade", "safeModeAvailable", "safeMode", "safeModeCooldown", "upgradeBlocked",
  "ticksToLive", "fatigue", "spawning"];

module.exports = (memory) => {
  const objects = {};
  const ref = (o) => {
    const key = o.id ?? `creep:${o.name}`;
    if (key in objects) return key;
    const s = {};
    for (const k of KEYS) if (o[k] !== undefined) s[k] = o[k];
    if (s.spawning && typeof s.spawning === "object") s.spawning = { name: s.spawning.name, remainingTime: s.spawning.remainingTime };
    if (o.owner) s.owner = { username: o.owner.username };
    if (o.reservation) s.reservation = { username: o.reservation.username, ticksToEnd: o.reservation.ticksToEnd };
    if (o.pos) s.pos = { x: o.pos.x, y: o.pos.y, roomName: o.pos.roomName };
    if (o.body) s.body = o.body.map((p) => (p.boost === undefined ? { type: p.type, hits: p.hits } : { type: p.type, hits: p.hits, boost: p.boost }));
    if (o.store) s.store = { held: Object.fromEntries(Object.entries(o.store)), capacity: o.store.getCapacity() ?? o.store.getCapacity("energy") ?? 0 };
    objects[key] = s;
    return key;
  };
  const ids = (byName) => Object.fromEntries(Object.entries(byName).map(([name, o]) => [name, ref(o)]));
  const rooms = Object.values(Game.rooms).map((room) => ({
    name: room.name,
    energyAvailable: room.energyAvailable,
    energyCapacityAvailable: room.energyCapacityAvailable,
    controller: room.controller ? ref(room.controller) : undefined,
    find: Object.fromEntries(FINDS.map((f) => [f, room.find(f).map(ref)])),
    terrain: Array.from(Game.map.getRoomTerrain(room.name).getRawBuffer()).join(""),
  }));
  return JSON.stringify({
    time: Game.time,
    cpu: { limit: Game.cpu.limit, bucket: Game.cpu.bucket },
    gcl: { level: Game.gcl.level },
    memory,
    intel: RawMemory.segments[2],
    rooms,
    creeps: ids(Game.creeps),
    spawns: ids(Game.spawns),
    constructionSites: Object.keys(Game.constructionSites),
    objects,
  });
};
