// Offline, server stopped: stand a live capture (`scripts/capture-world.mjs`)
// as the whole world. Runs under xxscreeps' own loader, cwd the world:
//   xxscreeps.js file://…/live.mjs <capture.json>
// The clock is the capture's tick, so the absolute times it holds (decay,
// downgrade, a creep's age, Memory's stamps) keep their meaning. A border
// toward a room not captured is sealed: the engine has nothing beyond it.
// A room captured for its terrain alone stands empty: a bot reads or
// observes it, and the engine throws on a room it has no blob for. Another
// player's creeps are left out: with no code they would only stand.
import * as fs from "node:fs";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import * as User from "xxscreeps/engine/db/user/index.js";
import * as MapSchema from "xxscreeps/game/map.js";
import { makeWriter } from "xxscreeps/schema/write.js";
import { TerrainWriter, packExits } from "xxscreeps/game/terrain.js";
import { computeRoomMeta } from "xxscreeps/mods/modern/sector/terrain.js";
import { GameState, runAsUser, runWithState } from "xxscreeps/game/index.js";
import { STRONGHOLD_DECAY_TICKS } from "xxscreeps/game/constants/structure.js";
import { createRoomObject } from "xxscreeps/game/object.js";
import { RoomPosition } from "xxscreeps/game/position.js";
import { flushUsers } from "xxscreeps/game/room/room.js";
import { updateUserRoomRelationships } from "xxscreeps/engine/processor/model.js";
import { saveMemoryBlob, saveMemorySegmentBlob } from "xxscreeps/mods/meta/memory/model.js";
import { utf16ToBuffer } from "xxscreeps/utility/string.js";
import { generateRoom } from "xxscreeps/scripts/room-gen.js";
import * as Creep from "xxscreeps/mods/classic/creep/creep.js";
import { StructureController, updateRoomStatus } from "xxscreeps/mods/classic/controller/controller.js";
import * as ControllerProc from "xxscreeps/mods/classic/controller/processor.js";
import { Source } from "xxscreeps/mods/classic/source/source.js";
import { kSourceKeeperUserId } from "xxscreeps/mods/classic/source/game.js";
import * as KeeperLair from "xxscreeps/mods/classic/source/keeper-lair.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { activateNPC } from "xxscreeps/mods/npc/processor.js";
import { Mineral } from "xxscreeps/mods/classic/mineral/mineral.js";
import * as Extractor from "xxscreeps/mods/classic/mineral/extractor.js";
import * as Spawn from "xxscreeps/mods/classic/spawn/spawn.js";
import * as Extension from "xxscreeps/mods/classic/spawn/extension.js";
import * as Tower from "xxscreeps/mods/classic/defense/tower.js";
import * as Rampart from "xxscreeps/mods/classic/defense/rampart.js";
import * as Wall from "xxscreeps/mods/classic/defense/wall.js";
import * as Storage from "xxscreeps/mods/classic/logistics/storage.js";
import * as Link from "xxscreeps/mods/classic/logistics/link.js";
import * as Container from "xxscreeps/mods/classic/resource/container.js";
import * as Resource from "xxscreeps/mods/classic/resource/resource.js";
import { RESOURCES_ALL } from "xxscreeps/mods/classic/resource/constants.js";
import * as Road from "xxscreeps/mods/classic/road/road.js";
import * as Terminal from "xxscreeps/mods/classic/brokerage/terminal.js";
import * as Lab from "xxscreeps/mods/classic/chemistry/lab.js";
import * as Site from "xxscreeps/mods/classic/construction/construction-site.js";
import * as Observer from "xxscreeps/mods/modern/observer/observer.js";
import * as Factory from "xxscreeps/mods/modern/factory/factory.js";
import * as PowerSpawn from "xxscreeps/mods/modern/powerspawn/powerspawn.js";
import * as Nuker from "xxscreeps/mods/modern/nuker/nuker.js";

const [capturePath] = process.argv.slice(2);
const capture = JSON.parse(fs.readFileSync(capturePath, "utf8"));
// The modern stronghold mod, loaded only for a capture with an invader core:
// the core is the one object the classic mods do not know.
const InvaderCore = [...capture.rooms, ...(capture.terrain ?? [])].some((r) => (r.objects ?? []).some((o) => o.type === "invaderCore"))
  ? await import("xxscreeps/mods/modern/stronghold/invader-core.js")
  : null;
const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const warnings = [];
const left = new Map();
const leave = (why) => left.set(why, (left.get(why) ?? 0) + 1);

// ---- users -----------------------------------------------------------------
const ours = await User.findUserByName(db, capture.username);
if (!ours) throw new Error(`no user ${capture.username}: manage bot add first`);
await db.data.hSet(User.infoKey(ours), "gcl", String(capture.gcl ?? 0));
const userOf = new Map([[capture.userId, ours]]);
for (const [id, name] of Object.entries(capture.users)) {
  if (id === capture.userId || User.npcUsers[id]) continue;
  if (!(await User.findUserByName(db, name))) await User.create(db, id, name);
  userOf.set(id, await User.findUserByName(db, name));
}
const owner = (live) => (live == null ? null : (userOf.get(live) ?? live));

// ---- the clock and the map -------------------------------------------------
const time = capture.time;
await shard.data.set("time", time);
shard.time = time;
const names = capture.rooms.map((r) => r.name);
const ring = (capture.terrain ?? []).map((r) => ({ ...r, objects: [] }));
const rings = ring.map((r) => r.name);
await shard.data.sRem("rooms", (await shard.data.sMembers("rooms")).filter((n) => !names.includes(n) && !rings.includes(n)));
for (const name of [...names, ...rings])
  await generateRoom(shard, name).catch((err) => {
    if (!/already exists/.test(err.message)) throw err;
  });

const parse = (name) => {
  const [, ew, x, ns, y] = /^([WE])(\d+)([NS])(\d+)$/.exec(name);
  return { x: ew === "E" ? Number(x) : -Number(x) - 1, y: ns === "S" ? Number(y) : -Number(y) - 1 };
};
const nameOf = ({ x, y }) => `${x >= 0 ? `E${x}` : `W${-x - 1}`}${y >= 0 ? `S${y}` : `N${-y - 1}`}`;
const neighbour = (name, dx, dy) => {
  const p = parse(name);
  return nameOf({ x: p.x + dx, y: p.y + dy });
};
const terrainMap = new Map();
for (const r of [...capture.rooms, ...ring]) {
  const group = names.includes(r.name) ? names : rings;
  const t = new TerrainWriter();
  [...r.terrain].forEach((c, i) => t.set(i % 50, Math.floor(i / 50), Number(c) & 3));
  const sides = [
    [0, -1, (i) => [i, 0]],
    [0, 1, (i) => [i, 49]],
    [-1, 0, (i) => [0, i]],
    [1, 0, (i) => [49, i]],
  ];
  for (const [dx, dy, tile] of sides) if (!group.includes(neighbour(r.name, dx, dy))) for (let i = 0; i < 50; ++i) t.set(...tile(i), 1);
  terrainMap.set(r.name, { exits: packExits(t), terrain: t, sectors: [], sectorControl: undefined });
}
const all = new Set([...names, ...rings]);
for (const name of all) terrainMap.set(name, { ...terrainMap.get(name), ...computeRoomMeta(name, all) });
await shard.data.set("terrain", makeWriter(MapSchema.schema)(terrainMap));
const world = await shard.loadWorld();

// ---- objects ---------------------------------------------------------------
const stocked = (object, store) => {
  for (const [type, amount] of Object.entries({ energy: 0, ...store })) {
    if (!RESOURCES_ALL.includes(type)) {
      leave(`${type} in a store`);
      continue;
    }
    const have = object.store.getUsedCapacity(type) ?? 0;
    // A lab's store adds to the mineral it holds, and an empty one holds none.
    if (object.structureType === "lab" && have === 0 && amount > 0) object.store[type] = 0;
    if (amount > have) object.store["#add"](type, amount - have);
    else if (amount < have) object.store["#subtract"](type, have - amount);
  }
  return object;
};
const at = (o) => new RoomPosition(o.x, o.y, o.room);
// A room nobody sees is not updated: a time it carries may be long past,
// which the engine refuses.
const ahead = (t) => (t > time ? t : 0);
function structure(o, level) {
  const user = owner(o.user);
  switch (o.type) {
    case "spawn":
      return Spawn.create(at(o), user, o.name);
    case "extension":
      return Extension.create(at(o), level, user);
    case "tower":
      return Tower.create(at(o), user);
    case "storage":
      return Storage.create(at(o), user);
    case "link":
      return Link.create(at(o), user);
    case "terminal":
      return Terminal.create(at(o), user);
    case "lab":
      return Lab.create(at(o), user);
    case "extractor":
      return Extractor.create(at(o), user);
    case "observer":
      return Observer.create(at(o), user);
    case "factory":
      return Factory.create(at(o), user);
    case "powerSpawn":
      return PowerSpawn.create(at(o), user);
    case "nuker":
      return Nuker.create(at(o), user);
    case "container":
      return Container.create(at(o));
    case "road":
      return Road.create(at(o));
    case "constructedWall":
      return Wall.create(at(o));
    case "rampart": {
      const rampart = Rampart.create(at(o), user);
      rampart.isPublic = Boolean(o.isPublic);
      return rampart;
    }
    case "keeperLair": {
      const lair = KeeperLair.create(at(o));
      lair["#nextSpawnTime"] = ahead(o.nextSpawnTime);
      return lair;
    }
    case "invaderCore": {
      // Deployed (deploy time 0), as `engine/world.mjs` stands one: a level
      // 1-5 core runs its `bunker<level>` behaviour until its collapse — a
      // capture's `decayTime`, or a fresh stronghold's own decay window.
      const core = InvaderCore.create(at(o), o.level ?? 0, 0);
      if ((o.level ?? 0) > 0) core["#templateName"] = "bunker" + (o.level ?? 0);
      const collapse = o.decayTime ?? 0;
      core["#collapseTime"] = collapse > time ? collapse : time + STRONGHOLD_DECAY_TICKS;
      return core;
    }
    default:
      return null;
  }
}

for (const r of [...capture.rooms, ...ring]) {
  const room = await shard.loadRoom(r.name, time);
  const state = new GameState(world, time, [room]);
  const tasks = [];
  const context = { shard, state, didUpdate() {}, setActive() {}, wakeAt() {}, sendRoomIntent() {}, task: (t) => tasks.push(t) };
  runWithState(state, () =>
    runAsUser(ours, () => {
      // Removed before any insert: a flush fills each hole with the last
      // object, and a generated room's count is random.
      for (const object of [...room["#objects"]]) room["#removeObject"](object);
      room["#flushObjects"](state);
      room["#user"] = null;
      room["#level"] = 0;
      const insert = (object, o) => {
        object.id = o._id;
        room["#insertObject"](object);
        return object;
      };
      const c = r.objects.find((o) => o.type === "controller");
      let level = 0;
      if (c) {
        const controller = insert(createRoomObject(new StructureController(), at(c)), c);
        room["#flushObjects"](state);
        if (c.user) {
          ControllerProc.claim(context, controller, owner(c.user));
          level = c.level;
          updateRoomStatus(room, level, owner(c.user));
          controller["#progress"] = c.progress ?? 0;
          controller["#downgradeTime"] = c.downgradeTime ?? 0;
          controller.safeModeAvailable = c.safeModeAvailable ?? 0;
          controller["#safeModeCooldownTime"] = ahead(c.safeModeCooldown);
          controller["#upgradeBlockedUntil"] = ahead(c.upgradeBlocked);
          room["#safeModeUntil"] = ahead(c.safeMode);
        } else if (c.reservation?.endTime > time) {
          ControllerProc.reserve(context, controller, owner(c.reservation.user), c.reservation.endTime - time - 1);
        }
      }
      for (const o of r.objects) {
        let object;
        if (o.type === "controller") continue;
        else if (o.type === "source") {
          object = createRoomObject(new Source(), at(o));
          object.energy = o.energy;
          object.energyCapacity = o.energyCapacity;
          object["#nextRegenerationTime"] = ahead(o.nextRegenerationTime);
        } else if (o.type === "mineral") {
          if (!RESOURCES_ALL.includes(o.mineralType)) {
            leave(`mineral ${o.mineralType}`);
            continue;
          }
          object = Object.assign(createRoomObject(new Mineral(), at(o)), { mineralType: o.mineralType, mineralAmount: o.mineralAmount, density: o.density });
          object["#nextRegenerationTime"] = ahead(o.nextRegenerationTime);
        } else if (o.type === "constructionSite") {
          object = Site.create(at(o), o.structureType, owner(o.user), o.progressTotal, o.name);
          object.progress = o.progress ?? 0;
        } else if (o.type === "creep") {
          // An NPC's creep the engine drives (an Invader core's defenders, a
          // Source Keeper) stands; another player's, with no code, would only
          // stand, kept where a crafted world wants it to (`stands`).
          if (o.user !== capture.userId && !User.npcUsers[o.user] && !o.stands) {
            leave("another player's creep");
            continue;
          }
          if (o.spawning) {
            leave("a creep in the making");
            continue;
          }
          object = Creep.createWithBody(
            at(o),
            o.body.map((p) => ({ type: p.type, hits: p.hits, boost: p.boost ?? undefined })),
            o.name,
            owner(o.user),
          );
          object.hits = o.hits;
          object.fatigue = o.fatigue ?? 0;
          object["#ageTime"] = o.ageTime ?? time + 1500;
          stocked(object, o.store);
        } else if (RESOURCES_ALL.includes(o.type) || o.resourceType) {
          if (!RESOURCES_ALL.includes(o.resourceType ?? o.type)) {
            leave(`pile of ${o.resourceType ?? o.type}`);
            continue;
          }
          object = Resource.create(at(o), o.resourceType ?? o.type, o[o.resourceType ?? o.type] ?? o.amount);
        } else {
          object = structure(o, level);
          if (!object) {
            leave(o.type);
            continue;
          }
          if (o.hits !== undefined) object.hits = o.hits;
          if (ahead(o.nextDecayTime)) object["#nextDecayTime"] = o.nextDecayTime;
          if (ahead(o.cooldownTime)) object["#cooldownTime"] = o.cooldownTime;
          if (object.store) stocked(object, o.store);
        }
        insert(object, o);
      }
      if (r.objects.some((o) => o.type === "keeperLair")) activateNPC(room, kSourceKeeperUserId);
      if (r.objects.some((o) => o.type === "invaderCore")) activateNPC(room, kInvaderUserId);
      room["#flushObjects"](state);
    }),
  );
  await Promise.all(tasks);
  const previous = flushUsers(room);
  await Promise.all([updateUserRoomRelationships(shard, room, previous), shard.saveRoom(r.name, time, room), shard.saveRoom(r.name, time + 1, room)]);
}

const memory = { ...capture.memory, v2: { ...capture.memory?.v2, declarations: capture.declarations } };
await saveMemoryBlob(shard, ours, utf16ToBuffer(JSON.stringify(memory)));
// The intel rows a capture took, so a world knows the rooms it knew out of
// sight: a declared room's row is what casts the scout that later sees it.
for (const [id, text] of Object.entries(capture.segments ?? {})) await saveMemorySegmentBlob(shard, ours, Number(id), utf16ToBuffer(text));
await Promise.all([db.save(), shard.save()]);
for (const [why, n] of left) warnings.push(`${why} ×${n}: left out`);
fs.writeFileSync("world.json", JSON.stringify({ time, rooms: names, warnings }, null, 2));
for (const w of warnings) console.log(`note: ${w}`);
console.log(`stood ${names.length} rooms at tick ${time}`);
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
