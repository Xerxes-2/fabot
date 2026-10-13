// Offline, server stopped (#543): stand an arena scenario in this world.
// Runs under xxscreeps' own loader, cwd the world directory:
//   xxscreeps.js file://…/world.mjs <scenario.json> <rooms dir> <our username>
// The users already exist (`manage bot add` / `user create`). Terrain is our
// `.room` captures', every border sealed but the ones between scenario rooms;
// sources and the controller stand where the capture has them; structures,
// controllers and bodies are the scenario's. Writes `world.json`: the start
// time and the user ids.
import * as fs from "node:fs";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import * as User from "xxscreeps/engine/db/user/index.js";
import { generateRoom } from "xxscreeps/scripts/room-gen.js";
import * as MapSchema from "xxscreeps/game/map.js";
import { makeWriter } from "xxscreeps/schema/write.js";
import { TerrainWriter, packExits } from "xxscreeps/game/terrain.js";
import { computeRoomMeta } from "xxscreeps/mods/modern/sector/terrain.js";
import { GameState, runAsUser, runWithState } from "xxscreeps/game/index.js";
import { createRoomObject } from "xxscreeps/game/object.js";
import { RoomPosition } from "xxscreeps/game/position.js";
import { flushUsers } from "xxscreeps/game/room/room.js";
import { updateUserRoomRelationships } from "xxscreeps/engine/processor/model.js";
import * as Creep from "xxscreeps/mods/classic/creep/creep.js";
import { StructureController, updateRoomStatus } from "xxscreeps/mods/classic/controller/controller.js";
import * as ControllerProc from "xxscreeps/mods/classic/controller/processor.js";
import { Source } from "xxscreeps/mods/classic/source/source.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { kSourceKeeperUserId } from "xxscreeps/mods/classic/source/game.js";
import { activateNPC } from "xxscreeps/mods/npc/processor.js";
import * as Spawn from "xxscreeps/mods/classic/spawn/spawn.js";
import * as Extension from "xxscreeps/mods/classic/spawn/extension.js";
import * as Tower from "xxscreeps/mods/classic/defense/tower.js";
import * as Rampart from "xxscreeps/mods/classic/defense/rampart.js";
import * as Wall from "xxscreeps/mods/classic/defense/wall.js";
import * as Storage from "xxscreeps/mods/classic/logistics/storage.js";
import * as Link from "xxscreeps/mods/classic/logistics/link.js";
import * as Container from "xxscreeps/mods/classic/resource/container.js";
import * as Terminal from "xxscreeps/mods/classic/brokerage/terminal.js";
import * as Lab from "xxscreeps/mods/classic/chemistry/lab.js";
import * as Extractor from "xxscreeps/mods/classic/mineral/extractor.js";

const [scenarioPath, roomsDir, ourName] = process.argv.slice(2);
const scenario = JSON.parse(fs.readFileSync(scenarioPath, "utf8"));
// The modern stronghold mod, loaded only for a scenario with an invader core
// (`xx.mjs` adds it to the world's mods then).
const hasCore = scenario.rooms.some((r) => r.structures.some((s) => s.kind === "invaderCore"));
const InvaderCore = hasCore ? await import("xxscreeps/mods/modern/stronghold/invader-core.js") : null;
const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const warnings = [];

const userIds = { ours: await User.findUserByName(db, ourName) };
for (const side of new Set([...scenario.bodies.map((b) => b.side), ...scenario.rooms.flatMap((r) => r.structures.map((s) => s.owner))])) {
  if (side?.startsWith("player:")) userIds[side] = await User.findUserByName(db, side.slice(7));
}
userIds["npc:Invader"] = kInvaderUserId;
userIds["npc:Source Keeper"] = kSourceKeeperUserId;
const ownerOf = (side) => {
  const id = userIds[side];
  if (id == null) throw new Error(`no user for side ${side}`);
  return id;
};

// ---- terrain ---------------------------------------------------------------
const names = scenario.rooms.map((r) => r.name);
for (const name of names) await generateRoom(shard, name).catch(() => {}); // an imported room exists already

const parseRoom = (name) => {
  const [, ew, x, ns, y] = /^([WE])(\d+)([NS])(\d+)$/.exec(name);
  return { x: ew === "E" ? Number(x) : -Number(x) - 1, y: ns === "S" ? Number(y) : -Number(y) - 1 };
};
const roomName = ({ x, y }) => `${x >= 0 ? `E${x}` : `W${-x - 1}`}${y >= 0 ? `S${y}` : `N${-y - 1}`}`;
const neighbour = (name, dx, dy) => {
  const p = parseRoom(name);
  return roomName({ x: p.x + dx, y: p.y + dy });
};

const captures = new Map();
for (const name of names) {
  const lines = fs.readFileSync(`${roomsDir}/${name}.room`, "utf8").split("\n");
  const rows = lines.slice(lines.indexOf("[terrain]") + 1).filter((l) => /^[0-3]{50}$/.test(l)).slice(0, 50);
  const objects = lines
    .slice(lines.indexOf("[objects]") + 2)
    .map((l) => l.split("\t"))
    .filter((f) => f.length >= 4 && /^\d+$/.test(f[2]))
    .map(([id, type, x, y]) => ({ id, type, x: Number(x), y: Number(y) }));
  captures.set(name, { rows, objects });
}

const world = await shard.loadWorld();
const terrainMap = new Map(world.terrain);
for (const name of names) {
  const t = new TerrainWriter();
  captures.get(name).rows.forEach((row, y) => [...row].forEach((c, x) => t.set(x, y, Number(c) & 3)));
  const sides = [
    [0, -1, (i) => [i, 0]],
    [0, 1, (i) => [i, 49]],
    [-1, 0, (i) => [0, i]],
    [1, 0, (i) => [49, i]],
  ];
  for (const [dx, dy, tile] of sides) {
    if (names.includes(neighbour(name, dx, dy))) continue;
    for (let i = 0; i < 50; ++i) t.set(...tile(i), 1);
  }
  terrainMap.set(name, { exits: packExits(t), terrain: t, sectors: [], sectorControl: undefined });
}
const all = new Set(terrainMap.keys());
for (const name of names) terrainMap.set(name, { ...terrainMap.get(name), ...computeRoomMeta(name, all) });
await shard.data.set("terrain", makeWriter(MapSchema.schema)(terrainMap));

// ---- objects ---------------------------------------------------------------
const world2 = await shard.loadWorld();
const time = shard.time;

// The engine breaks a tie between two moves by object order, and random ids
// would make each run's order its own: every object takes the next of these.
let nextId = 0;
const placed = (object) => {
  object.id = (++nextId).toString(16).padStart(24, "0");
  return object;
};

const withEnergy = (object, energy) => {
  const have = object.store.getUsedCapacity("energy") ?? 0;
  if (energy > have) object.store["#add"]("energy", energy - have);
  else if (energy < have) object.store["#subtract"]("energy", have - energy);
  return object;
};

function structure(s, pos, level, room) {
  const owner = s.owner ? ownerOf(s.owner) : null;
  switch (s.kind) {
    case "invaderCore": {
      // Already deployed: the scenario's own ramparts, towers, containers
      // and roads stand for the template's (deploy time 0). A level 1-5
      // core runs its `bunker<level>` behaviour until the room's collapse
      // tick; a level-0 one is an expansion core, which carries its parent
      // stronghold's collapse (`strongholds.js` expandStronghold). No deposit
      // type: no loot, as the live core's "normal" earns none in the vanilla tables.
      const core = InvaderCore.create(pos, s.level, 0);
      if (s.level > 0) core["#templateName"] = `bunker${s.level}`;
      const collapse = room.strongholdCollapse || (s.level === 0 ? Math.max(0, ...scenario.rooms.map((r) => r.strongholdCollapse ?? 0)) : 0);
      if (collapse > 0) core["#collapseTime"] = time + (collapse - scenario.time);
      else warnings.push(`${s.id}: no stronghold collapse; it stands for good, as no live core does`);
      return core;
    }
    case "spawn":
      return withEnergy(Spawn.create(pos, owner, s.name), s.energy);
    case "extension":
      return withEnergy(Extension.create(pos, level, owner), s.energy);
    case "tower":
      return withEnergy(Tower.create(pos, owner), s.energy);
    case "storage":
      return withEnergy(Storage.create(pos, owner), s.energy);
    case "link":
      return withEnergy(Link.create(pos, owner), s.energy);
    case "terminal":
      return withEnergy(Terminal.create(pos, owner), s.energy);
    case "container":
      return withEnergy(Container.create(pos), s.energy);
    case "lab":
      return Lab.create(pos, owner);
    case "extractor":
      return Extractor.create(pos, owner);
    case "constructedWall":
      return Wall.create(pos);
    case "rampart": {
      const rampart = Rampart.create(pos, owner);
      rampart.isPublic = s.isPublic;
      // A stronghold's rampart decays only at the collapse
      // (`stronghold.js` deploy: nextDecayTime is the decay time).
      if (room.strongholdCollapse > 0 && s.owner === "npc:Invader") {
        rampart["#nextDecayTime"] = time + (room.strongholdCollapse - scenario.time);
      }
      return rampart;
    }
    default:
      warnings.push(`${s.id}: no ${s.kind} in the classic mods; left out`);
      return null;
  }
}

for (const r of scenario.rooms) {
  const room = await shard.loadRoom(r.name, time);
  const state = new GameState(world2, time, [room]);
  const tasks = [];
  const context = { shard, state, didUpdate() {}, setActive() {}, wakeAt() {}, sendRoomIntent() {}, task: (t) => tasks.push(t) };
  runWithState(state, () =>
    runAsUser(userIds.ours, () => {
      // Removed before any insert: a flush fills each hole with the last
      // object, and a generated room's count is random.
      for (const object of [...room["#objects"]]) room["#removeObject"](object);
      room["#flushObjects"](state);
      room["#user"] = null;
      room["#level"] = 0;
      const captured = captures.get(r.name).objects;
      for (const o of captured.filter((o) => o.type === "source")) {
        const source = createRoomObject(new Source(), new RoomPosition(o.x, o.y, r.name));
        source.energy = source.energyCapacity = 3000;
        room["#insertObject"](placed(source));
      }
      const at = captured.find((o) => o.type === "controller");
      let level = 0;
      if (at) {
        const controller = createRoomObject(new StructureController(), new RoomPosition(at.x, at.y, r.name));
        room["#insertObject"](placed(controller));
        room["#flushObjects"](state);
        const c = r.controller;
        if (c && c.owner !== "Unowned") {
          const owner = c.owner === "Ours" ? userIds.ours : ownerOf(`player:${c.username}`);
          ControllerProc.claim(context, controller, owner);
          level = c.level;
          updateRoomStatus(room, level, owner);
          controller["#downgradeTime"] = time + c.ticksToDowngrade;
          controller["#progress"] = c.progress;
          controller.safeModeAvailable = c.safeModeAvailable;
          room["#safeModeUntil"] = c.safeModeUntil > 0 ? time + (c.safeModeUntil - scenario.time) : 0;
        }
      }
      for (const s of r.structures) {
        const object = structure(s, new RoomPosition(s.x, s.y, r.name), level, r);
        if (!object) continue;
        object.hits = s.hits;
        room["#insertObject"](placed(object));
      }
      if (r.sites > 0) warnings.push(`${r.name}: ${r.sites} construction site(s) left out`);
      for (const b of scenario.bodies.filter((b) => b.room === r.name)) {
        const creep = Creep.create(new RoomPosition(b.x, b.y, r.name), b.parts, b.id, ownerOf(b.side));
        // Damage takes the front parts first (`_damage.js`).
        const damage = 100 * b.parts.length - b.hits;
        creep.body.forEach((part, i) => (part.hits = Math.max(0, Math.min(100, 100 * (i + 1) - damage))));
        creep.hits = b.hits;
        creep.fatigue = b.fatigue;
        creep["#ageTime"] = time + b.ticksToLive;
        if (b.energy > 0) creep.store["#add"]("energy", b.energy);
        if (b.side === "npc:Invader" && b.script !== "Invader") {
          warnings.push(`${b.id}: the arena scripts it ${b.script}; here it runs the engine's invader AI`);
        }
        if (b.side.startsWith("player:")) warnings.push(`${b.id}: ${b.side} has no code here; it holds`);
        room["#insertObject"](placed(creep));
      }
      if (
        scenario.bodies.some((b) => b.room === r.name && b.side === "npc:Invader") ||
        r.structures.some((s) => s.kind === "invaderCore")
      ) {
        activateNPC(room, kInvaderUserId);
      }
      room["#flushObjects"](state);
    }),
  );
  await Promise.all(tasks);
  const previous = flushUsers(room);
  await Promise.all([updateUserRoomRelationships(shard, room, previous), shard.saveRoom(r.name, time, room), shard.saveRoom(r.name, time + 1, room)]);
}

await Promise.all([db.save(), shard.save()]);
fs.writeFileSync("world.json", JSON.stringify({ time, userIds, rooms: names, warnings }, null, 2));
for (const w of warnings) console.log(`note: ${w}`);
console.log(`stood ${scenario.name}: ${scenario.bodies.length} bodies in ${names.join(", ")} at tick ${time}`);
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
