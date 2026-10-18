// The world in one pass (`src/app/world.rs` decodes it): every number into
// one Int32Array, every string into one NUL-joined string, so a tick crosses
// the wasm–JS seam a handful of times, not once a field. The write order is
// the decoder's read order; a change to one is a change to both.
globalThis.__fabotRead = (() => {
  let ints = new Int32Array(1 << 16);
  let at = 0;
  let strs = [];
  const n = (x) => {
    if (at === ints.length) {
      const grown = new Int32Array(ints.length * 2);
      grown.set(ints);
      ints = grown;
    }
    ints[at++] = x;
  };
  const s = (x) => strs.push(typeof x === "string" ? x : "");
  const absent = (x) => x === undefined || x === null;
  const sorted = (o) => Object.keys(o).sort();

  // An owner as a tag (0 nobody, 1 ours, 2 theirs) and a name.
  const owner = (o) => {
    if (o.my === true) return n(1), s("");
    if (absent(o.owner)) return n(0), s("");
    n(2), s(o.owner.username);
  };
  const pos = (p) => (s(p.roomName), n(p.x), n(p.y));
  const store = (st) => {
    if (absent(st)) return n(0), n(0);
    const keys = Object.keys(st);
    n(keys.length);
    for (const k of keys) s(k), n(st[k]);
    n(st.getCapacity() || st.getCapacity("energy") || 0);
  };
  const body = (parts) => {
    if (!Array.isArray(parts)) return n(0);
    n(parts.length);
    for (const p of parts) s(p.type), n(p.hits), s(p.boost);
  };
  const creep = (c) => {
    s(c.id), s(c.name), owner(c), pos(c.pos), body(c.body);
    n(c.hits), n(c.hitsMax), n(c.ticksToLive), n(c.fatigue), store(c.store);
  };
  const list = (xs, each) => {
    n(xs.length);
    for (const x of xs) each(x);
  };

  const controller = (c) => {
    s(c.id), pos(c.pos), owner(c);
    n(c.level), n(c.progress), n(c.progressTotal), n(c.ticksToDowngrade);
    if (absent(c.reservation)) n(0);
    else n(1), s(c.reservation.username), n(c.reservation.ticksToEnd);
    n(c.safeMode), n(c.safeModeAvailable), n(c.safeModeCooldown), n(c.upgradeBlocked);
  };
  // Only the kinds the engine gives a field read it (`Kind::has_store`, `has_cooldown`).
  const STORED = new Set(["spawn", "extension", "tower", "container", "storage", "link", "terminal", "lab", "factory", "powerSpawn", "nuker", "invaderCore"]);
  const KNOWN = new Set([...STORED, "road", "rampart", "constructedWall", "extractor", "observer", "keeperLair", "powerBank", "portal"]);
  const COOLED = new Set(["tower", "link", "lab", "extractor", "terminal", "factory", "nuker", "powerSpawn"]);
  // The collapse effect's timer if it runs, else the deploy timer and a flag 0.
  const collapse = (st) => {
    const e = Array.isArray(st.effects) ? st.effects.find((e) => e.effect === 1002) : undefined;
    if (e) n(1), n(e.ticksRemaining);
    else n(0), n(st.ticksToDeploy);
  };
  const structure = (st) => {
    const type = st.structureType;
    s(type), s(st.id), n(st.pos.x), n(st.pos.y), owner(st), n(st.hits), n(st.hitsMax);
    if (STORED.has(type) || !KNOWN.has(type)) store(st.store);
    if (COOLED.has(type)) n(st.cooldown);
    if (type === "invaderCore") collapse(st), n(st.level);
    if (type === "rampart") n(st.isPublic === true);
  };
  const remains = (r) => {
    s(r.id), pos(r.pos), store(r.store);
    if (absent(r.creep)) return n(0);
    n(1), creep(r.creep), n(r.deathTime);
  };
  const room = (r) => {
    s(r.name), n(r.energyAvailable), n(r.energyCapacityAvailable);
    if (absent(r.controller)) n(0);
    else n(1), controller(r.controller);
    list(r.find(105), (x) => (s(x.id), pos(x.pos), n(x.energy), n(x.energyCapacity), n(x.ticksToRegeneration)));
    list(r.find(116), (x) => (s(x.id), pos(x.pos), s(x.mineralType), n(x.mineralAmount)));
    list(r.find(107), structure);
    list(r.find(111), (x) => (s(x.structureType), s(x.id), n(x.pos.x), n(x.pos.y), owner(x), n(x.progress), n(x.progressTotal)));
    list(r.find(106), (x) => (s(x.id), pos(x.pos), s(x.resourceType), n(x.amount)));
    const left = [...r.find(118), ...r.find(123)];
    list(left, remains);
    list(r.find(103), creep);
  };

  return () => {
    at = 0;
    strs = [];
    const game = Game;
    n(game.time), n(game.cpu.bucket);
    const rooms = sorted(game.rooms).map((k) => game.rooms[k]);
    const spawns = sorted(game.spawns).map((k) => game.spawns[k]);
    const creeps = sorted(game.creeps).map((k) => game.creeps[k]);
    const mine = rooms.find((r) => r.controller && r.controller.my === true);
    s(spawns.length ? spawns[0].owner?.username : creeps.length ? creeps[0].owner.username : mine?.controller.owner?.username);
    list(rooms, room);
    list(spawns, (sp) => {
      s(sp.id), s(sp.name), pos(sp.pos);
      if (absent(sp.spawning)) return n(0);
      const oven = game.creeps[sp.spawning.name];
      n(1), s(sp.spawning.name), n(sp.spawning.remainingTime), body(oven && oven.body);
    });
    list(
      creeps.filter((c) => !c.spawning),
      creep,
    );
    n(Object.keys(game.constructionSites).length), n(game.gcl.level);
    return [ints.subarray(0, at), strs.join("\0")];
  };
})();
