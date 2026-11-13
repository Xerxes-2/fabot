// The world in one pass (`src/app/world.rs` decodes it): every number into
// one Int32Array, every string into one NUL-joined string, a word the pass
// repeats (a kind, a part, a room) once at its end and by index, so a tick crosses
// the wasm–JS seam a handful of times, not once a field. The write order is
// read order of both decoders, `world.rs` and `__fabotUnread` below; a change
// to one is a change to all three.
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
  let words = new Map();
  const w = (x) => {
    if (typeof x !== "string") x = "";
    let i = words.get(x);
    if (i === undefined) words.set(x, (i = words.size));
    n(i);
  };
  const absent = (x) => x === undefined || x === null;
  const sorted = (o) => Object.keys(o).sort();

  // An owner as a tag (0 nobody, 1 ours, 2 theirs), theirs with a name.
  const owner = (o) => {
    if (o.my === true) return n(1);
    if (absent(o.owner)) return n(0);
    n(2), w(o.owner.username);
  };
  const pos = (p) => (w(p.roomName), n(p.x), n(p.y));
  const store = (st) => {
    if (absent(st)) return n(0), n(0);
    const keys = Object.keys(st);
    n(keys.length);
    for (const k of keys) w(k), n(st[k]);
    n(st.getCapacity() || st.getCapacity("energy") || 0);
  };
  const body = (parts) => {
    if (!Array.isArray(parts)) return n(0);
    n(parts.length);
    for (const p of parts) w(p.type), n(p.hits), w(p.boost);
  };
  const creep = (c) => {
    s(c.id), s(c.name), owner(c), pos(c.pos), body(c.body);
    n(c.hits), n(c.hitsMax), n(c.ticksToLive), n(c.fatigue), store(c.store);
  };
  const list = (xs, each) => {
    n(xs.length);
    for (const x of xs) each(x);
  };
  // Parsed only when it holds a strike or a destruction: most ticks a room logs
  // harvests and transfers alone, and on a colony's 22 KB log the search costs
  // 2.5 us against the parse's 29 (two `includes` cost 14).
  const struckOrDestroyed = new RegExp(`"event":(${EVENT_ATTACK}|${EVENT_OBJECT_DESTROYED}),`);
  const eventsOf = (r) => {
    const raw = r.getEventLog(true);
    if (typeof raw !== "string") return raw ?? [];
    return struckOrDestroyed.test(raw) ? JSON.parse(raw) : [];
  };

  const controller = (c) => {
    s(c.id), pos(c.pos), owner(c);
    // Undefined until the engine first runs a claimed controller's timer: never due.
    n(c.level), n(c.progress), n(c.progressTotal), n(c.ticksToDowngrade ?? CONTROLLER_DOWNGRADE[c.level]);
    if (absent(c.reservation)) n(0);
    else n(1), w(c.reservation.username), n(c.reservation.ticksToEnd);
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
    w(type), s(st.id), n(st.pos.x), n(st.pos.y), owner(st), n(st.hits), n(st.hitsMax);
    if (STORED.has(type) || !KNOWN.has(type)) store(st.store);
    if (COOLED.has(type)) n(st.cooldown);
    if (type === "keeperLair") n(st.ticksToSpawn ?? -1);
    if (type === "invaderCore") collapse(st), n(st.level);
    if (type === "rampart") n(st.isPublic === true);
  };
  const remains = (r) => {
    s(r.id), pos(r.pos), store(r.store);
    if (absent(r.creep)) return n(0);
    n(1), creep(r.creep), n(r.deathTime);
  };
  const room = (r) => {
    w(r.name), n(r.energyAvailable), n(r.energyCapacityAvailable);
    if (absent(r.controller)) n(0);
    else n(1), controller(r.controller);
    list(r.find(105), (x) => (s(x.id), pos(x.pos), n(x.energy), n(x.energyCapacity), n(x.ticksToRegeneration)));
    list(r.find(116), (x) => (s(x.id), pos(x.pos), w(x.mineralType), n(x.mineralAmount)));
    list(r.find(107), structure);
    list(r.find(111), (x) => (w(x.structureType), s(x.id), n(x.pos.x), n(x.pos.y), owner(x), n(x.progress), n(x.progressTotal)));
    list(r.find(106), (x) => (s(x.id), pos(x.pos), w(x.resourceType), n(x.amount)));
    const left = [...r.find(118), ...r.find(123)];
    list(left, remains);
    list(r.find(103), creep);
  };

  // A recorded pass back into `scripts/bench-game.mjs`'s capture, terrain,
  // Memory and intel aside: the same pass reads from it again.
  globalThis.__fabotUnread = (ints, text) => {
    const strs = text.split("\0");
    let i = 0;
    let k = 0;
    const n = () => ints[i++];
    const s = () => strs[k++];
    const words = strs.slice(strs.length - ints.at(-1));
    const w = () => words[n()];
    const objects = {};
    const me = s();
    const object = (id) => (objects[id] ??= { id });
    const list = (each) => Array.from({ length: n() }, each);
    const owner = (o) => {
      const tag = n();
      if (tag === 1) Object.assign(o, { my: true, owner: { username: me } });
      if (tag === 2) Object.assign(o, { my: false, owner: { username: w() } });
    };
    const pos = (o, roomName = w()) => (o.pos = { x: n(), y: n(), roomName });
    const store = (o) => {
      const held = Object.fromEntries(list(() => [w(), n()]));
      o.store = { held, capacity: n() };
    };
    const body = () =>
      list(() => {
        const [type, hits, boost] = [w(), n(), w()];
        return boost ? { type, hits, boost } : { type, hits };
      });
    const creep = (o) => {
      o.name = s();
      owner(o), pos(o);
      Object.assign(o, { body: body(), hits: n(), hitsMax: n(), ticksToLive: n(), fatigue: n() });
      store(o);
      return o.id;
    };
    const controller = (o) => {
      pos(o), owner(o);
      Object.assign(o, { level: n(), progress: n(), progressTotal: n(), ticksToDowngrade: n() });
      if (n()) o.reservation = { username: w(), ticksToEnd: n() };
      Object.assign(o, { safeMode: n(), safeModeAvailable: n(), safeModeCooldown: n(), upgradeBlocked: n() });
    };
    const structure = (room) => {
      const type = w();
      const o = object(s());
      o.structureType = type;
      pos(o, room), owner(o);
      Object.assign(o, { hits: n(), hitsMax: n() });
      if (STORED.has(type) || !KNOWN.has(type)) store(o);
      if (COOLED.has(type)) o.cooldown = n();
      if (type === "keeperLair" && (o.ticksToSpawn = n()) < 0) delete o.ticksToSpawn;
      if (type === "invaderCore") {
        if (n()) o.effects = [{ effect: 1002, ticksRemaining: n() }];
        else o.ticksToDeploy = n();
        o.level = n();
      }
      if (type === "rampart") o.isPublic = n() === 1;
      return o.id;
    };
    const room = () => {
      const r = { name: w(), energyAvailable: n(), energyCapacityAvailable: n() };
      if (n()) {
        r.controller = object(s()).id;
        controller(objects[r.controller]);
      }
      const find = (each) => list(() => each(object(s())));
      r.find = {
        105: find((o) => (pos(o), Object.assign(o, { energy: n(), energyCapacity: n(), ticksToRegeneration: n() })).id),
        116: find((o) => (pos(o), Object.assign(o, { mineralType: w(), mineralAmount: n() })).id),
        107: list(() => structure(r.name)),
      };
      r.find[111] = list(() => {
        const type = w();
        const o = object(s());
        pos(o, r.name), owner(o);
        return Object.assign(o, { structureType: type, progress: n(), progressTotal: n() }).id;
      });
      r.find[106] = find((o) => (pos(o), Object.assign(o, { resourceType: w(), amount: n() })).id);
      r.find[118] = find((o) => {
        pos(o), store(o);
        if (n()) (o.creep = { id: s() }), creep(o.creep), (o.deathTime = n());
        return o.id;
      });
      r.find[103] = find(creep);
      return r;
    };

    const [time, bucket] = [n(), n()];
    const rooms = list(room);
    // The event log is read live only: its strikes and destructions consumed, not stood.
    const strikes = list(() => [s(), s()]);
    const destroyed = list(() => [w(), s(), n()]);
    const creeps = {};
    const spawns = Object.fromEntries(
      list(() => {
        const o = object(s());
        o.name = s();
        pos(o);
        if (n()) {
          o.spawning = { name: s(), remainingTime: n() };
          const key = `creep:${o.spawning.name}`;
          objects[key] = { name: o.spawning.name, pos: o.pos, my: true, owner: { username: me }, spawning: true, body: body() };
          creeps[o.spawning.name] = key;
        }
        return [o.name, o.id];
      }),
    );
    for (const id of list(() => creep(object(s())))) creeps[objects[id].name] = id;
    const constructionSites = Array.from({ length: n() }, (_, i) => `site:${i}`);
    const gcl = { level: n() };
    const controllerStructures = Object.fromEntries(list(() => [w(), list(n)]));
    return { time, cpu: { bucket }, gcl, rooms, creeps, spawns, constructionSites, objects, controllerStructures };
  };

  return (drain) => {
    at = 0;
    strs = [];
    words = new Map();
    const game = Game;
    n(game.time), n(game.cpu.bucket);
    const rooms = sorted(game.rooms).map((k) => game.rooms[k]);
    const spawns = sorted(game.spawns).map((k) => game.spawns[k]);
    const creeps = sorted(game.creeps).map((k) => game.creeps[k]);
    const mine = rooms.find((r) => r.controller && r.controller.my === true);
    const user = spawns.length ? spawns[0].owner?.username : creeps.length ? creeps[0].owner.username : mine?.controller.owner?.username;
    s(user);
    list(rooms, room);
    const stood = new Set(creeps.map((c) => c.pos.roomName));
    const shots = [];
    const destroyed = [];
    for (const r of rooms) {
      const c = r.controller;
      if (r.name !== drain && !stood.has(r.name) && c?.my !== true && c?.reservation?.username !== user) continue;
      for (const e of eventsOf(r)) {
        const kind = e.event ?? e._$Variant;
        if (kind === EVENT_ATTACK) shots.push(e);
        else if (kind === EVENT_OBJECT_DESTROYED) {
          // xxscreeps logs the type beside the id, the game under `data`.
          const type = (e.data ?? e).type;
          if (type !== "creep") destroyed.push([r.name, e.objectId, CONSTRUCTION_COST[type] ?? 0]);
        }
      }
    }
    list(shots, (e) => (s(e.objectId), s(e.data?.targetId)));
    list(destroyed, ([name, id, cost]) => (w(name), s(id), n(cost)));
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
    // A season's server may override an engine table (`World.rules`). The
    // official server's rows are objects by level, missing the levels at 0.
    const levels = (row) => Array.from({ length: 9 }, (_, l) => row[l] ?? 0);
    list(sorted(CONTROLLER_STRUCTURES), (kind) => (w(kind), list(levels(CONTROLLER_STRUCTURES[kind]), n)));
    n(words.size);
    return [ints.subarray(0, at), [...strs, ...words.keys()].join("\0")];
  };
})();
