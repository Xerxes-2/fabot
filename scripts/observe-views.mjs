import { readFileSync } from "node:fs";
import { getHeapStatistics } from "node:v8";
import { load, stand } from "./bench-game.mjs";
import { RECORDER, unpack } from "./recorder.mjs";
import { fail } from "./screeps-api.mjs";

// Segment 1 is the observe row (§4): what the operator reads, the tick it was
// written for.
const OBSERVE = 1;

const SIZE = 50;
const xy = (i) => [i % SIZE, Math.floor(i / SIZE)];
const tile = (x, y) => y * SIZE + x;
const inside = (x, y) => x >= 0 && x < SIZE && y >= 0 && y < SIZE;
// The engine's OBSTACLE_OBJECT_TYPES but bodies; a rampart blocks all but its owner.
const OBSTACLES = new Set(["spawn", "source", "mineral", "deposit", "controller", "constructedWall", "extension", "link", "storage", "tower",
  "observer", "powerSpawn", "powerBank", "lab", "terminal", "nuker", "factory", "invaderCore"]);
const DETAILED = new Set(["spawn", "tower", "storage", "terminal", "link", "container"]);
const LETTER = { move: "m", work: "w", carry: "c", attack: "a", ranged_attack: "r", heal: "h", claim: "k", tough: "t" };

// Counts in order of first part: an interleaved m1c1… run-length reads worse.
const counts = (letters) => [...new Set(letters)].map((l) => `${l}${letters.split(l).length - 1}`).join("");
const cast = (c) => `${c.name} ${c.room} ${counts(c.body.toLowerCase())}`;
const body = (parts) => counts(parts.map((p) => LETTER[p.type ?? p]).join(""));
const stored = (store) => Object.entries(store ?? {}).filter(([, n]) => n).map(([r, n]) => `${r}:${n}`).join(",") || "empty";
const short = (n) => (n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : n >= 1e4 ? `${Math.round(n / 1e3)}k` : `${n}`);
const hits = (o) => (o.hits === undefined ? "" : ` ${short(o.hits)}/${short(o.hitsMax)}`);
const at = (o) => `${o.x},${o.y}`;
const spans = (ns) => ns.reduce((out, n) => (out.at(-1)?.[1] === n - 1 ? (out.at(-1)[1] = n) : out.push([n, n]), out), []).map(([a, b]) => (a === b ? `${a}` : `${a}-${b}`)).join(",");

export function views({ api, shard, memory, raw }) {
  const time = async () => (await api.req("GET", "/api/game/time", { shard })).time;
  const me = async () => (me.id ??= (await api.req("GET", "/api/auth/me", {}))._id);
  const rooms = new Map();
  const roomOf = async (name) => {
    if (!rooms.has(name)) {
      const res = await api.req("GET", "/api/game/room-objects", { room: name, shard });
      rooms.set(name, { objects: res.objects ?? [], owner: (id) => res.users?.[id]?.username ?? id });
    }
    return rooms.get(name);
  };
  const described = async (room, id) => {
    const o = (await roomOf(room)).objects.find((x) => x._id === id);
    return o ? `${o.type} ${at(o)}${hits(o)}` : `${id} (unseen)`;
  };
  const ours = async (names, also = []) => {
    const ticks = unpack(await Promise.all(RECORDER.map(raw)));
    const last = ticks.at(-1);
    const where = new Map(names.map((n) => [n, last?.objects[last.creeps[n]]?.pos.roomName]));
    const owned = (await api.req("GET", "/api/user/rooms", { id: await me(), shard })).shards?.[shard] ?? [];
    const found = new Map();
    for (const room of new Set([...[...where.values()].filter(Boolean), ...also, ...owned])) {
      if (names.every((n) => found.has(n))) break;
      for (const o of (await roomOf(room)).objects) if (o.type === "creep" && o.user === (await me()) && names.includes(o.name)) found.set(o.name, o);
    }
    return found;
  };

  return {
    async engagements() {
      const priced = new Map((JSON.parse((await raw(OBSERVE)) ?? "null")?.offers ?? []).map(([id, offers]) => [id, offers]));
      const fight = (await memory("v2.fight")) ?? {};
      const engagements = fight.engagements ?? [];
      if (engagements.length === 0) console.log("no open engagements");
      const casts = fight.casts ?? [];
      const members = await ours(engagements.flatMap((e) => e.ours), engagements.map((e) => e.room));
      const now = await time();
      for (const e of engagements) {
        const p = e.predicted;
        console.log(`#${e.id} ${e.kind ?? "defend"} ${e.room} ${e.phase} ${e.tactic} · lost ${e.lost} · opened ${now - e.opened} ago, phase ${now - e.since} ago · predicted ${p ? `${p.kind} ${p.ticks}t dead ${p.dead} hits ${p.hits}` : "none"}`);
        const offers = (priced.get(e.id) ?? []).map(([t, v, c]) => `${t === e.tactic ? "*" : ""}${t} ${v - c} (${v}-${c})`);
        if (offers.length) console.log(`  offers ${offers.join(" · ")}`);
        if (e.target) console.log(`  target ${await described(e.room, e.target)}`);
        for (const [i, id] of e.order.entries()) console.log(`  order ${i + 1} ${await described(e.room, id)}`);
        for (const n of e.ours) {
          const c = members.get(n);
          console.log(`  ${n} ${c ? `${c.room} ${at(c)}${hits(c)}` : "not found"}`);
        }
        for (const c of casts.filter((c) => c.engagement === e.id)) console.log(`  cast ${cast(c)}`);
      }
      const listed = new Set(engagements.map((e) => e.id));
      for (const c of casts.filter((c) => !listed.has(c.engagement))) console.log(`cast for unlisted #${c.engagement} ${cast(c)}`);
    },

    async room(name) {
      const { objects, owner } = await roomOf(name);
      const now = await time();
      const c = objects.find((o) => o.type === "controller");
      if (c)
        console.log(`tick ${now} · controller ${c.user ? `${owner(c.user)} rcl${c.level} downgrade ${c.downgradeTime - now}` : "unowned"}` +
          `${c.reservation ? ` · reserved ${owner(c.reservation.user)} ${c.reservation.endTime - now}` : ""}${c.safeMode ? ` · safe mode ${c.safeMode - now}` : ` · safe mode off (${c.safeModeAvailable ?? 0} left)`}`);
      else console.log(`tick ${now} · no controller`);
      const structures = Map.groupBy(objects.filter((o) => o.hitsMax !== undefined && o.type !== "creep" && o.type !== "powerCreep"), (o) => o.type);
      for (const [type, list] of structures)
        console.log(`${type} ×${list.length}${DETAILED.has(type) ? `: ${list.map((s) => `${at(s)}${hits(s)} ${stored(s.store)}`).join(" · ")}` : ""}`);
      for (const o of objects.filter((o) => o.type === "creep" || o.type === "powerCreep")) {
        const acts = Object.entries(o.actionLog ?? {}).filter(([, v]) => v).map(([k, v]) => (v.message !== undefined ? `say:${v.message}` : `${k}@${v.x},${v.y}`));
        console.log(`${owner(o.user)} ${o.name} ${at(o)} ${body(o.body ?? [])}${hits(o)} ${stored(o.store)}${acts.length ? ` ${acts.join(" ")}` : ""}`);
      }
      for (const t of objects.filter((o) => o.type === "tombstone")) console.log(`tombstone ${t.creepName} (${owner(t.user)}) ${now - t.deathTime} ago ${at(t)}`);
      const sites = Map.groupBy(objects.filter((o) => o.type === "constructionSite"), (o) => o.structureType);
      if (sites.size) console.log(`sites: ${[...sites].map(([type, list]) => `${type} ×${list.length}`).join(", ")}`);
    },

    async creep(name) {
      const c = (await ours([name])).get(name) ?? fail(`creep ${name}: not found in the recorder's rooms or ours`);
      console.log(`${c.name} ${c.room} ${at(c)} ${body(c.body)}${hits(c)} ttl ${c.ageTime ? c.ageTime - (await time()) : "spawning"} ${stored(c.store)}`);
      console.log(`assignment ${JSON.stringify((await memory("v2.assignments"))?.[name] ?? null)}`);
    },

    async regions(name) {
      const terrain = (await api.req("GET", "/api/game/room-terrain", { room: name, encoded: 1, shard })).terrain?.[0]?.terrain ?? fail(`no terrain for ${name}`);
      const { objects, owner } = await roomOf(name);
      const mine = await me();
      const blocked = Uint8Array.from(terrain, (v) => Number(v) & 1);
      for (const o of objects) if (OBSTACLES.has(o.type) || (o.type === "rampart" && o.user !== mine)) blocked[tile(o.x, o.y)] = 1;
      const region = new Int32Array(SIZE * SIZE).fill(-1);
      const sizes = [];
      for (let start = 0; start < SIZE * SIZE; start++) {
        if (blocked[start] || region[start] >= 0) continue;
        const id = sizes.length;
        const stack = [start];
        region[start] = id;
        let size = 0;
        while (stack.length) {
          const [x, y] = xy(stack.pop());
          size++;
          for (const [dx, dy] of NEIGHBOURS) {
            const j = tile(x + dx, y + dy);
            if (inside(x + dx, y + dy) && !blocked[j] && region[j] < 0) (region[j] = id), stack.push(j);
          }
        }
        sizes.push(size);
      }
      const touches = (o, id) => NEIGHBOURS.some(([dx, dy]) => inside(o.x + dx, o.y + dy) && region[tile(o.x + dx, o.y + dy)] === id);
      for (const id of sizes.map((_, i) => i).sort((a, b) => sizes[b] - sizes[a])) {
        const sides = [["N", (x, y) => y === 0, (x) => x], ["E", (x) => x === SIZE - 1, (x, y) => y],
          ["S", (x, y) => y === SIZE - 1, (x) => x], ["W", (x) => x === 0, (x, y) => y]]
          .map(([side, on, along]) => [side, region.reduce((out, r, i) => (r === id && on(...xy(i)) ? [...out, along(...xy(i))] : out), [])])
          .filter(([, ns]) => ns.length).map(([side, ns]) => `${side} ${spans(ns)}`);
        const beside = Map.groupBy(objects.filter((o) => o.user && o.type !== "rampart" && OBSTACLES.has(o.type) && touches(o, id)), (o) => `${owner(o.user)} ${o.type}`);
        if (sides.length === 0 && beside.size === 0) continue;
        console.log(`region ${id}: ${sizes[id]} tiles · exits ${sides.join(" ") || "none"} · beside ${[...beside].map(([k, l]) => `${k} ${l.length > 4 ? `×${l.length}` : l.map(at).join(" ")}`).join(" · ") || "nothing owned"}`);
      }
    },
  };
}

const NEIGHBOURS = [[-1, -1], [0, -1], [1, -1], [-1, 0], [1, 0], [-1, 1], [0, 1], [1, 1]];

// A gap in the capture's ticks reloads the bundle, as a global reset would.
export function replay(file, traced, bundle = new URL("../dist/main.js", import.meta.url).pathname) {
  let bot, last;
  globalThis.fabotTrace = true;
  for (const line of readFileSync(file, "utf8").split("\n").filter(Boolean)) {
    const tick = JSON.parse(line);
    for (const r of tick.rooms) r.terrain = Uint8Array.from(r.terrain, Number);
    if (last !== tick.time - 1) {
      bot = load(bundle);
      console.log(`-- bundle loaded cold at t${tick.time}`);
    }
    last = tick.time;
    const intents = [];
    const game = stand(tick, intents, getHeapStatistics, () => 0);
    globalThis.__fabotRead = bot.read;
    const traces = [];
    const log = console.log;
    console.log = (text, ...rest) => (String(text).startsWith("#trace ") ? traces.push(JSON.parse(text.slice(7))) : log(text, ...rest));
    try {
      bot.loop();
    } finally {
      console.log = log;
    }
    const segment = JSON.parse(game.segments[1]);
    const named = new Map(Object.entries(tick.creeps).map(([name, id]) => [id, name]));
    const who = (id) => named.get(id) ?? id;
    const kept = (actor) => !traced || who(actor) === traced;
    const mine = intents.filter(([id]) => kept(id));
    const refusals = segment.refusals.filter((r) => kept(r.actor));
    const faults = segment.faults.filter((f) => f.tick === tick.time);
    console.log(`t${tick.time} · ${mine.length} intents · ${refusals.length} refusals · ${faults.length} faults`);
    for (const [id, verb, ...args] of mine) console.log(`  ${who(id)} ${verb} ${args.map((a) => JSON.stringify(a)).join(" ")}`);
    for (const r of refusals) console.log(`  refused ${who(r.actor)} ${r.act} ${r.code}`);
    for (const f of faults) console.log(`  fault ${f.where}: ${f.message}`);
    for (const t of traces.filter((t) => kept(t.creep)))
      console.log(`  trace ${t.creep} ${t.tiles.sort((a, b) => b[3] - a[3]).map(([room, x, y, s]) => `${room} ${x},${y}=${s}`).join(" ")}`);
  }
}
