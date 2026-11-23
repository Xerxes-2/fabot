import { spawnSync } from "node:child_process";

const LATE_BODIES = "harvester:work6carry1move3*2,harvester:work20carry1move10,hauler:carry32move16*2,upgrader:work15carry1move15,worker:work16carry17move17*2";
// Each node's level and storage stock; N4's links are what it judges, so
// they are left to the bot, and every node's ramparts, which a node would
// stand at full hits the bot never paid for.
export const NODES = {
  N1: { level: 1, stored: 0, without: [] },
  N2: { level: 3, stored: 0, without: ["rampart"] },
  N3: { level: 4, stored: 5000, without: ["rampart"] },
  // N3 as another bot built it: each container on another tile serving its source or controller.
  T3: { level: 4, stored: 5000, without: ["rampart"], moved: true },
  N4: { level: 5, stored: 50000, without: ["link", "rampart"] },
  T4: { level: 5, stored: 50000, without: ["link", "rampart"], moved: true },
  E1: { level: 5, stored: 50000, without: ["link", "rampart"], outposts: true },
  E0: { level: 4, stored: 5000, without: ["rampart"], outposts: true },
  N5: { level: 6, stored: 100000, without: ["rampart"] },
  // N5 as RCL6 is reached: its quiet reserve (a third of two towers' two lives) stocked, its ore unbuilt.
  R6: { level: 6, stored: 20000, without: ["rampart", "extractor", "terminal", "lab"] },
  E2: { level: 6, stored: 100000, without: ["rampart"], claims: 1 },
  C2: { level: 6, stored: 100000, without: ["rampart"], claims: 2 },
  G2: { level: 6, stored: 100000, without: ["rampart"], claims: 1, short: true },
  X2: { level: 6, stored: 100000, without: ["rampart"], spare: 1 },
  F2: { level: 6, stored: 100000, without: ["rampart"], spare: 1, stale: true },
  Y2: { level: 6, stored: 20000, without: ["rampart"], spare: 1 },
  // N5 with its labs and the ore for XKHO2 alone stocked.
  L5: { level: 6, stored: "100000,H:6000,O:6000,K:6000,X:6000", without: ["rampart"], labs: true },
  // L5 without its tower, its first lab holding what an outpost's ranger is
  // boosted with: the raid must be answered by a cast, and the lab can boost one at once.
  B5: { level: 6, stored: "100000,H:6000,O:6000,U:6000", without: ["rampart", "tower"], labs: true, labStock: "energy:2000,XKHO2:900" },
  // B5's lab with no compound: the cast is not held waiting for one.
  S5: { level: 6, stored: "100000,H:6000,O:6000", without: ["rampart", "tower"], labs: true, labStock: "energy:2000" },
  // B5 with the compound in the storage and its labs empty: a hauler fills the
  // lab the waiting body walks to, as the minerals rest in the terminal first.
  W5: { level: 6, stored: "100000,H:6000,O:6000,U:6000,UH2O:900", without: ["rampart", "tower"], labs: true },
  // W5 at RCL7, six labs.
  W7: { level: 7, stored: "100000,H:6000,O:6000,U:6000,XKHO2:900", without: ["rampart", "tower", "link", "factory"], labs: true },
  // W7 holding each T3 combat boost.
  // M7 with a level-1 kit's TOUGH boost alone: two parts' worth.
  K7: { level: 7, stored: "100000,H:6000,O:6000,U:6000,XGHO2:60,XLHO2:900,XKHO2:900,XZHO2:900,XUH2O:900", without: ["rampart", "tower", "link", "factory"], labs: true },
  M7: { level: 7, stored: "100000,H:6000,O:6000,U:6000,XGHO2:900,XLHO2:900,XKHO2:900,XZHO2:900,XUH2O:900", without: ["rampart", "tower", "link", "factory"], labs: true },
  // An RCL7 node as another bot left it: its roads and containers worn to
  // half, its links and ramparts sites to build.
  T7: { level: 7, stored: 100000, without: ["link", "rampart", "factory"], worn: 0.5 },
  // The late game without its climb: every structure (the modern template's
  // too) and about the bodies a live RCL8 home carried (captures/live-w11s25.json,
  // its outposts' bodies aside), so the run starts working, not casting.
  N6: { level: 7, stored: 300000, without: ["rampart"], modern: true, bodies: LATE_BODIES },
  N7: { level: 8, stored: 500000, without: ["rampart"], modern: true, bodies: LATE_BODIES },
  // N6 with its storage full of its own ore, its energy under the reserve.
  F6: { level: 7, stored: "30000,H:970000", without: ["rampart"], modern: true, bodies: LATE_BODIES },
  // RCL8 as a from-zero run reaches it: RCL7's plan standing, the stock at
  // its RCL7 level, and the roads its outposts laid on RCL8's extension tiles;
  // every room in reach avoided, so the home's dig alone pays (outposts lost).
  // RCL5 as a run whose outposts' roads went home across the ring before its walls stood on them.
  U5: { level: 5, stored: 50000, without: ["rampart"], walled: true },
  R8: { level: 8, built: 7, stored: 50000, without: ["rampart"], modern: true, bodies: LATE_BODIES, buried: true, alone: true },
  // N7 with every room in reach avoided.
  A7: { level: 8, stored: 500000, without: ["rampart"], modern: true, bodies: LATE_BODIES, alone: true },
  // A T3 from base ores alone: the cluster walks the whole reaction tree.
  L8: { level: 8, stored: "500000,H:3000,O:3000,U:3000,K:3000,L:3000,Z:3000,X:3000", without: ["rampart"], modern: true, bodies: LATE_BODIES, alone: true },
};

export const near = (room, hops) => {
  const [, we, x, ns, y] = /^([WE])(\d+)([NS])(\d+)$/.exec(room);
  const [cx, cy] = [we === "E" ? +x : -x - 1, ns === "S" ? +y : -y - 1];
  const name = (i, j) => `${i < 0 ? `W${-i - 1}` : `E${i}`}${j < 0 ? `N${-j - 1}` : `S${j}`}`;
  return Array.from({ length: (2 * hops + 1) ** 2 }, (_, k) => name(cx + (k % (2 * hops + 1)) - hops, cy + Math.floor(k / (2 * hops + 1)) - hops)).filter((r) => r !== room);
};

// Mirror `sim/src/world.rs` US, the engine's CONTROLLER_DOWNGRADE and krebs's `creeps/build.rs`.
export const US = "u1";
const DOWNGRADE = [0, 20000, 10000, 20000, 40000, 80000, 120000, 150000, 200000];
const energy = (n) => ({ energy: n });
export const BUILT = {
  spawn: [5000, { storeCapacityResource: energy(300) }],
  extension: [1000, { storeCapacityResource: energy(0) }],
  link: [1000, { storeCapacityResource: energy(800), cooldown: 0 }],
  storage: [10000, { storeCapacity: 1000000 }],
  tower: [3000, { storeCapacityResource: energy(1000) }],
  observer: [500, {}],
  extractor: [500, {}],
  lab: [500, { mineralAmount: 0, cooldown: 0, storeCapacity: 5000, storeCapacityResource: energy(2000) }],
  powerSpawn: [5000, { storeCapacityResource: { energy: 5000, power: 100 } }],
  terminal: [3000, { storeCapacity: 300000 }],
  nuker: [1000, { storeCapacityResource: { energy: 300000, G: 5000 } }],
  factory: [1000, { storeCapacity: 50000, cooldown: 0 }],
  container: [250000, { storeCapacity: 2000 }],
  road: [5000, {}],
  rampart: [1, {}],
  constructedWall: [1, { hitsMax: 300000000 }],
};
const RAMPART_HITS_MAX = [0, 0, 300000, 1000000, 3000000, 10000000, 30000000, 100000000, 300000000];
const DECAY = { road: 1000, container: 100, rampart: 100 };

/**
 * What a finished site of `kind` is at tick `t`, on a tile of `terrain` (its
 * digit) in a room at `level`: its store, hits, capacity and next decay.
 */
export function built(kind, terrain, t, level) {
  const [hits, extra] = BUILT[kind];
  const full = kind === "road" ? hits * ({ 1: 150, 2: 5 }[terrain] ?? 1) : kind === "rampart" ? RAMPART_HITS_MAX[level] : (extra.hitsMax ?? hits);
  // An extension holds its level's capacity from the start, as xxscreeps' Extension.create gives it; krebs's
  // extension tick would set it only after the bot's first read.
  const capacity = kind === "extension" && { storeCapacityResource: energy(level >= 8 ? 200 : level === 7 ? 100 : 50) };
  return { store: energy(0), ...extra, ...capacity, hits: kind === "road" ? full : hits, hitsMax: full, ...(DECAY[kind] && { nextDecayTime: t + DECAY[kind] }) };
}
const STOOD = new Set(["level", "built", "stored", "without", "worn", "bodies", "outposts", "alone", "labs", "modern", "buried", "walled", "moved", "claims", "spare", "short", "stale", "labStock"]);
const terrainOf = (room) => [room.terrain].flat().join("");

/** Why sim cannot stand node `name`, or null. */
export function unstood(name) {
  if (!NODES[name]) return `no node ${name}: one of ${Object.keys(NODES).join(", ")}`;
  const missing = Object.keys(NODES[name]).filter((k) => !STOOD.has(k));
  return missing.length ? `sim does not stand ${name}'s ${missing.join(", ")} yet` : null;
}

/**
 * What `node` stands on `ground` (a room's terrain and objects), in the
 * plan tool's build order: its plan at its level but what it is `without`,
 * `buried` adding roads on the extension tiles its level adds past `built`,
 * `walled` the ring's walls with a road under each,
 * `moved` setting each container on another tile serving its rock.
 */
export function planNode(ground, node, plan) {
  const planned = (level, ring = false) => {
    const res = spawnSync(plan, { input: JSON.stringify({ ...ground, level, ring, labs: Boolean(node.labs) }), encoding: "utf8" });
    if (res.status !== 0) throw new Error(`the plan tool failed: ${res.stderr}`);
    return JSON.parse(res.stdout);
  };
  const built = planned(node.built ?? node.level).filter((s) => !node.without.includes(s.kind));
  if (node.buried) {
    const held = new Set(built.map((s) => s.y * 50 + s.x));
    const next = planned(node.level).filter((s) => s.kind === "extension" && !held.has(s.y * 50 + s.x));
    built.push(...next.map(({ x, y }) => ({ kind: "road", x, y })));
  }
  if (node.walled) {
    const walls = planned(node.level, true).filter((s) => s.kind === "constructedWall");
    built.push(...walls.flatMap(({ x, y }) => [{ kind: "road", x, y }, { kind: "constructedWall", x, y }]));
  }
  if (node.moved) {
    const { terrain, sources, controller } = ground;
    const range = (a, [x, y]) => Math.max(Math.abs(a.x - x), Math.abs(a.y - y));
    const used = new Set(built.map((s) => s.y * 50 + s.x));
    for (const c of built.filter((s) => s.kind === "container")) {
      const rock = sources.find((r) => range(c, r) === 1);
      const serves = (t) => (rock ? range(t, rock) === 1 : range(t, controller) <= 3 && !sources.some((r) => range(t, r) <= 1));
      const tiles = Array.from({ length: 2304 }, (_, i) => ({ x: (i % 48) + 1, y: Math.floor(i / 48) + 1 }));
      const seat = tiles.find((t) => serves(t) && terrain[t.y * 50 + t.x] !== "1" && !used.has(t.y * 50 + t.x) && range(t, rock ?? controller) > 0);
      if (seat) [c.x, c.y] = [seat.x, seat.y], used.add(seat.y * 50 + seat.x);
    }
  }
  return built;
}

/** `world` with `room` stood as node `name`; `plan` is the plan tool's binary. */
export function standNode(world, room, name, plan) {
  const node = NODES[name];
  if (unstood(name)) throw new Error(unstood(name));
  // As soak.mjs: a level-1 node is the respawn.
  if (node.level <= 1) return world;
  const t = world.time ?? 0;
  const home = world.rooms.find((r) => r.name === room);
  const terrain = terrainOf(home);
  const xy = (o) => [o.x, o.y];
  const of = (type) => home.objects.filter((o) => o.type === type);
  const [controller] = of("controller");
  const ground = { room, terrain, sources: of("source").map(xy), minerals: of("mineral").map(xy), controller: xy(controller) };
  const list = planNode(ground, node, plan);
  const [first] = list;

  // Ids in list order, so the engine's tie-breaks by object order repeat.
  let id = 0;
  const nextId = () => (++id).toString(16).padStart(24, "0");
  const [stock, ...ores] = String(node.stored).split(",");
  for (const s of list) {
    if (!BUILT[s.kind]) throw new Error(`no ${s.kind} in a node`);
    const o = { _id: nextId(), type: s.kind, room, x: s.x, y: s.y, notifyWhenAttacked: true, ...built(s.kind, terrain[s.y * 50 + s.x], t, node.level) };
    if (!["road", "container", "constructedWall"].includes(s.kind)) o.user = US;
    if (s.kind === "spawn") Object.assign(o, s === first ? { name: "Spawn1", store: energy(300) } : { name: `Node${id}` }, { spawning: null });
    if (s.kind === "storage") o.store = { energy: Number(stock), ...Object.fromEntries(ores.map((m) => m.split(":")).map(([r, n]) => [r, Number(n)])) };
    if (s.kind === "lab" && s === list.find((l) => l.kind === "lab") && node.labStock) {
      o.store = Object.fromEntries(node.labStock.split(",").map((p) => p.split(":")).map(([r, n]) => [r, Number(n)]));
      o.mineralAmount = Object.entries(o.store).find(([r]) => r !== "energy")?.[1] ?? 0;
    }
    if (s.kind === "road" || s.kind === "container") o.hits = Math.ceil(o.hits * (node.worn ?? 1));
    if (s.kind === "nuker") o.cooldownTime = t + 100000;
    home.objects.push(o);
  }
  Object.assign(controller, { user: US, level: node.level, progress: 0, downgradeTime: t + DOWNGRADE[node.level], safeModeAvailable: node.level - 1 });

  const bodies = (node.bodies ?? "").split(",").filter(Boolean).flatMap((spec) => {
    const [, pattern, parts, count = "1"] = /^(\w+):([a-z_0-9]+)(?:\*(\d+))?$/.exec(spec);
    const body = [...parts.matchAll(/([a-z_]+)(\d+)/g)].flatMap(([, type, n]) => Array(Number(n)).fill(type));
    return Array.from({ length: Number(count) }, () => ({ pattern, body }));
  });
  const blocked = new Set(list.filter((s) => s.kind !== "road" && s.kind !== "container").map((s) => s.y * 50 + s.x));
  const free = [];
  for (let r = 1; free.length < bodies.length && r < 25; r++)
    for (let y = first.y - r; y <= first.y + r; y++)
      for (let x = first.x - r; x <= first.x + r; x++)
        if (Math.max(Math.abs(x - first.x), Math.abs(y - first.y)) === r && x > 0 && y > 0 && x < 49 && y < 49 && !(terrain[y * 50 + x] & 1) && !blocked.has(y * 50 + x)) free.push([x, y]);
  if (free.length < bodies.length) throw new Error(`${room}: no free tile for ${bodies.length} bodies near the first spawn`);
  for (const [i, { pattern, body }] of bodies.entries()) {
    const hits = body.length * 100;
    home.objects.push({
      _id: nextId(), type: "creep", room, x: free[i][0], y: free[i][1], name: `${pattern}-${t - i}-Spawn1`, user: US, body: body.map((type) => ({ type, hits: 100 })),
      store: energy(0), storeCapacity: 50 * body.filter((p) => p === "carry").length, hits, hitsMax: hits, spawning: false, fatigue: 0, notifyWhenAttacked: true,
      ageTime: t + Math.round((1500 * (i + 1)) / bodies.length),
    });
  }

  const fits = outposts(world, room);
  if ((node.outposts || node.claims || node.stale) && !fits.length) throw new Error(`${room}: no neighbour fits an outpost`);
  const declarations = { outposts: node.outposts ? fits : [], claims: fits.slice(0, node.claims ?? 0), ...(node.alone && { avoid: near(room, 3) }) };
  // The engine's GCL for that many rooms past the first.
  const rooms = node.spare ?? declarations.claims.length;
  const gcl = rooms && !node.short ? Math.ceil(1e6 * rooms ** 2.4) : world.gcl;
  const expansion = node.stale && { expansion: { room: fits[0].room, since: -100000 } };
  const v2 = { ...world.memory?.v2, ...expansion, declarations: { ...declarations, ...world.memory?.v2?.declarations, ...world.declarations } };
  return { ...world, gcl, simMemory: { ...world.memory, v2 } };
}

function outposts(world, room) {
  const home = terrainOf(world.rooms.find((r) => r.name === room));
  const [, north, , west, east, , south] = near(room, 1);
  const edges = [[north, (i) => i], [east, (i) => i * 50 + 49], [south, (i) => 2450 + i], [west, (i) => i * 50]];
  const fit = [];
  for (const [next, tile] of edges) {
    const objects = world.rooms.find((r) => r.name === next)?.objects ?? [];
    const controller = objects.find((o) => o.type === "controller");
    const sources = objects.filter((o) => o.type === "source");
    const exit = Array.from({ length: 48 }, (_, i) => tile(i + 1)).some((k) => !(home[k] & 1));
    if (exit && controller && !controller.user && !controller.reservation && sources.length) fit.push({ room: next, controller, sources });
  }
  fit.sort((a, b) => b.sources.length - a.sources.length || (a.room < b.room ? -1 : 1));
  const at = (o) => ({ id: o._id, x: o.x, y: o.y });
  return fit.slice(0, 2).map((o) => ({ room: o.room, controller: at(o.controller), sources: o.sources.map(at) }));
}
