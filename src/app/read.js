// The world in one pass (`core/base/world.rs` `read` decodes it): every number into
// one Int32Array, every string into one NUL-joined string, a word the pass
// repeats (a kind, a part, a room) once at its end and by index, so a tick crosses
// the wasm–JS seam a handful of times, not once a field. The write order is
// read order of both decoders, `world.rs` and `__fabotUnread` below; a change
// to one is a change to all three. Rooms are written a kind at a time, so one
// clock a kind times it (`world.rs` `SECTIONS`, in this file's order).
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
  let parts = 0;
  const body = (xs) => {
    if (!Array.isArray(xs)) return n(0);
    n(xs.length), (parts += xs.length);
    for (const p of xs) w(p.type), n(p.hits), w(p.boost);
  };
  const creep = (c) => {
    s(c.id), s(c.name), owner(c), pos(c.pos), body(c.body);
    n(c.hits), n(c.hitsMax), n(c.ticksToLive), n(c.fatigue), store(c.store);
  };
  const list = (xs, each) => {
    n(xs.length);
    for (const x of xs) each(x);
    return xs.length;
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
  // Only the kinds the engine gives a field read it (`Built::has_store`, `has_cooldown`).
  const STORED = new Set(["spawn", "extension", "tower", "container", "storage", "link", "terminal", "lab", "factory", "powerSpawn", "nuker", "invaderCore"]);
  const KNOWN = new Set([...STORED, "road", "rampart", "constructedWall", "extractor", "observer", "keeperLair", "powerBank", "portal"]);
  const COOLED = new Set(["tower", "link", "lab", "extractor", "terminal", "factory", "nuker", "powerSpawn"]);
  // The official engine reads these off the object's data, where `store` builds a Store each tick.
  const ENERGY = new Set(["spawn", "extension", "tower", "link"]);
  const energy = (st) => {
    const e = st.energy;
    if (e > 0) n(1), w("energy"), n(e);
    else n(0);
    n(st.energyCapacity);
  };
  // The collapse effect's timer if it runs, else the deploy timer and a flag 0.
  const collapse = (st) => {
    const e = Array.isArray(st.effects) ? st.effects.find((e) => e.effect === 1002) : undefined;
    if (e) n(1), n(e.ticksRemaining);
    else n(0), n(st.ticksToDeploy);
  };
  let fixed = new Map();
  let pass = 0;
  let checking = false;
  const fixedOf = (st, id) => {
    let f = fixed.get(id);
    if (f !== undefined && checking) {
      const read = (fixed.delete(id), fixedOf(st, id));
      for (const k of ["type", "x", "y", "tag", "user", "hitsMax"])
        if (read[k] !== f[k]) throw new Error(`${id} ${k}: kept ${f[k]}, read ${read[k]}`);
    }
    if (f === undefined) {
      const type = st.structureType;
      // A controller changes hands; a rampart's or wall's hitsMax follows its room's level.
      const o = type === "controller" ? undefined : st.owner;
      const tag = type === "controller" ? undefined : st.my === true ? 1 : absent(o) ? 0 : 2;
      const hitsMax = type === "rampart" || type === "constructedWall" ? undefined : st.hitsMax;
      fixed.set(id, (f = { type, x: st.pos.x, y: st.pos.y, tag, user: tag === 2 ? o.username : "", hitsMax, pass }));
    } else f.pass = pass;
    return f;
  };
  const structure = (st) => {
    const id = st.id;
    const f = fixedOf(st, id);
    const type = f.type;
    w(type), s(id), n(f.x), n(f.y);
    if (f.tag === undefined) owner(st);
    else if ((n(f.tag), f.tag === 2)) w(f.user);
    n(st.hits), n(f.hitsMax ?? st.hitsMax);
    if (ENERGY.has(type) && typeof st.energyCapacity === "number") energy(st);
    else if (STORED.has(type) || !KNOWN.has(type)) store(st.store);
    if (COOLED.has(type)) n(st.cooldown);
    if (type === "keeperLair") n(st.ticksToSpawn ?? -1);
    if (type === "invaderCore") collapse(st), n(st.level);
    if (type === "rampart") n(st.isPublic === true);
  };
  // Only what `world.rs` `Fallen` keeps.
  const remains = (r) => {
    s(r.id), pos(r.pos), store(r.store);
    const c = r.creep;
    if (absent(c) || c.my !== true) return n(0);
    n(1), (parts += list(Array.isArray(c.body) ? c.body : [], (p) => w(p.type))), n(c.ticksToLive), n(r.deathTime);
  };
  const room = (r) => {
    w(r.name), n(r.energyAvailable), n(r.energyCapacityAvailable);
    if (absent(r.controller)) n(0);
    else n(1), controller(r.controller);
  };
  const found = [];
  let next = 0;
  const KINDS = [
    (r) => list(r.find(105), (x) => (s(x.id), pos(x.pos), n(x.energy), n(x.energyCapacity), n(x.ticksToRegeneration))) + list(r.find(116), (x) => (s(x.id), pos(x.pos), w(x.mineralType), n(x.mineralAmount))),
    (r) => {
      const xs = r.find(107);
      found.push(xs);
      return xs.length;
    },
    (r) => list(found[next++], structure),
    (r) => list(r.find(111), (x) => (w(x.structureType), s(x.id), n(x.pos.x), n(x.pos.y), owner(x), n(x.progress), n(x.progressTotal))),
    (r) => list(r.find(106), (x) => (s(x.id), pos(x.pos), w(x.resourceType), n(x.amount))),
    (r) => list([...r.find(118), ...r.find(123)], remains),
    (r) => list(r.find(103), creep),
  ];

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
      const r = { name: w(), energyAvailable: n(), energyCapacityAvailable: n(), find: {} };
      if (n()) {
        r.controller = object(s()).id;
        controller(objects[r.controller]);
      }
      return r;
    };
    const find = (each) => list(() => each(object(s())));
    const kinds = [
      (r) => {
        r.find[105] = find((o) => (pos(o), Object.assign(o, { energy: n(), energyCapacity: n(), ticksToRegeneration: n() })).id);
        r.find[116] = find((o) => (pos(o), Object.assign(o, { mineralType: w(), mineralAmount: n() })).id);
      },
      (r) => (r.find[107] = list(() => structure(r.name))),
      (r) =>
        (r.find[111] = list(() => {
          const type = w();
          const o = object(s());
          pos(o, r.name), owner(o);
          return Object.assign(o, { structureType: type, progress: n(), progressTotal: n() }).id;
        })),
      (r) => (r.find[106] = find((o) => (pos(o), Object.assign(o, { resourceType: w(), amount: n() })).id)),
      (r) =>
        (r.find[118] = find((o) => {
          pos(o), store(o);
          if (n()) (o.creep = { my: true, body: list(() => ({ type: w() })), ticksToLive: n() }), (o.deathTime = n());
          return o.id;
        })),
      (r) => (r.find[103] = find(creep)),
    ];

    const [time, bucket] = [n(), n()];
    const rooms = list(room);
    for (const kind of kinds) rooms.forEach(kind);
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

  // Each section's closing clock, what it walked and the body parts it read:
  // the room heads, the kinds, events, our own, packing.
  const marks = new Float64Array(3 * (KINDS.length + 4));
  let m = 0;
  const mark = (count) => ((marks[m++] = Game.cpu.getUsed()), (marks[m++] = count), (marks[m++] = parts), (parts = 0));
  return (drain) => {
    m = 0;
    at = 0;
    found.length = 0;
    next = 0;
    parts = 0;
    strs = [];
    words = new Map();
    checking = globalThis.__fabotCheck === true;
    if (++pass % 100 === 0) fixed = new Map([...fixed].filter(([, f]) => f.pass === pass - 1));
    const game = Game;
    n(game.time), n(game.cpu.bucket);
    const rooms = sorted(game.rooms).map((k) => game.rooms[k]);
    const spawns = sorted(game.spawns).map((k) => game.spawns[k]);
    const creeps = sorted(game.creeps).map((k) => game.creeps[k]);
    const mine = rooms.find((r) => r.controller && r.controller.my === true);
    const user = spawns.length ? spawns[0].owner?.username : creeps.length ? creeps[0].owner.username : mine?.controller.owner?.username;
    s(user);
    mark(list(rooms, room));
    for (const each of KINDS) {
      let walked = 0;
      for (const r of rooms) walked += each(r);
      mark(walked);
    }
    const stood = new Set(creeps.map((c) => c.pos.roomName));
    const shots = [];
    const destroyed = [];
    let logs = 0;
    for (const r of rooms) {
      const c = r.controller;
      if (r.name !== drain && !stood.has(r.name) && c?.my !== true && c?.reservation?.username !== user) continue;
      logs++;
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
    mark(logs);
    const ovens = list(spawns, (sp) => {
      s(sp.id), s(sp.name), pos(sp.pos);
      if (absent(sp.spawning)) return n(0);
      const oven = game.creeps[sp.spawning.name];
      n(1), s(sp.spawning.name), n(sp.spawning.remainingTime), body(oven && oven.body);
    });
    const own = list(
      creeps.filter((c) => !c.spawning),
      creep,
    );
    n(Object.keys(game.constructionSites).length), n(game.gcl.level);
    // A season's server may override an engine table (`World.rules`). The
    // official server's rows are objects by level, missing the levels at 0.
    const levels = (row) => Array.from({ length: 9 }, (_, l) => row[l] ?? 0);
    list(sorted(CONTROLLER_STRUCTURES), (kind) => (w(kind), list(levels(CONTROLLER_STRUCTURES[kind]), n)));
    mark(ovens + own);
    n(words.size);
    const text = [...strs, ...words.keys()].join("\0");
    mark(at);
    return [ints.subarray(0, at), text, marks];
  };
})();
