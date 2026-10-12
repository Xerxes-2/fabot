// Offline, server stopped: stand the exchange duels (`exchange.mjs`), one
// sealed room a sample. Runs under xxscreeps' own loader, cwd the world:
//   xxscreeps.js file://…/duel-world.mjs <samples.json> <ours> <theirs>
// Each room: its sample's terrain, its owner's controller at RCL8 (so the
// owner's towers fire), each side's towers, ramparts and bodies. Writes `world.json`.
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
import * as Tower from "xxscreeps/mods/classic/defense/tower.js";
import * as Rampart from "xxscreeps/mods/classic/defense/rampart.js";
import { kInvaderUserId } from "xxscreeps/mods/classic/invader/game.js";
import { activateNPC } from "xxscreeps/mods/npc/processor.js";

const [samplesPath, ourName, theirName] = process.argv.slice(2);
const samples = JSON.parse(fs.readFileSync(samplesPath, "utf8"));
const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const users = { ours: await User.findUserByName(db, ourName), theirs: await User.findUserByName(db, theirName) };

const names = samples.map((s) => s.room);
for (const name of names) await generateRoom(shard, name).catch(() => {});
const world = await shard.loadWorld();
const terrainMap = new Map(world.terrain);
for (const s of samples) {
  const t = new TerrainWriter();
  s.ground.forEach((row, y) => [...row].forEach((c, x) => t.set(x, y, Number(c))));
  terrainMap.set(s.room, { exits: packExits(t), terrain: t, sectors: [], sectorControl: undefined });
}
const all = new Set(terrainMap.keys());
for (const name of names) terrainMap.set(name, { ...terrainMap.get(name), ...computeRoomMeta(name, all) });
await shard.data.set("terrain", makeWriter(MapSchema.schema)(terrainMap));

const world2 = await shard.loadWorld();
const time = shard.time;
let nextId = 0;
const placed = (object) => {
  object.id = (++nextId).toString(16).padStart(24, "0");
  return object;
};

for (const s of samples) {
  const room = await shard.loadRoom(s.room, time);
  const state = new GameState(world2, time, [room]);
  const tasks = [];
  const context = { shard, state, didUpdate() {}, setActive() {}, wakeAt() {}, sendRoomIntent() {}, task: (t) => tasks.push(t) };
  runWithState(state, () =>
    runAsUser(users.ours, () => {
      for (const object of [...room["#objects"]]) room["#removeObject"](object);
      room["#user"] = null;
      room["#level"] = 0;
      const controller = createRoomObject(new StructureController(), new RoomPosition(s.controller.x, s.controller.y, s.room));
      room["#insertObject"](placed(controller));
      room["#flushObjects"](state);
      ControllerProc.claim(context, controller, users[s.owner]);
      updateRoomStatus(room, 8, users[s.owner]);
      controller["#downgradeTime"] = time + 200000;
      controller.safeModeAvailable = 0;
      room["#safeModeUntil"] = 0;
      for (const [side, owner] of [["ours", users.ours], ["theirs", users.theirs]]) {
        for (const t of s[side].towers) {
          const tower = Tower.create(new RoomPosition(t.x, t.y, s.room), owner);
          tower.store["#add"]("energy", t.energy);
          room["#insertObject"](placed(tower));
        }
        for (const r of s[side].ramparts) {
          const rampart = Rampart.create(new RoomPosition(r.x, r.y, s.room), owner);
          rampart.hits = r.hits;
          room["#insertObject"](placed(rampart));
        }
        s[side].bodies.forEach((b, i) => {
          const body = b.parts.map((type, k) => ({ type, hits: 100, boost: b.boosts[k] || undefined }));
          const creep = Creep.createWithBody(new RoomPosition(b.x, b.y, s.room), body, `${s.id}-${side}-${i}`, b.invader ? kInvaderUserId : owner);
          if (b.invader) activateNPC(room, kInvaderUserId);
          creep["#ageTime"] = time + 1500;
          room["#insertObject"](placed(creep));
        });
      }
      room["#flushObjects"](state);
    }),
  );
  await Promise.all(tasks);
  const previous = flushUsers(room);
  await Promise.all([updateUserRoomRelationships(shard, room, previous), shard.saveRoom(s.room, time, room), shard.saveRoom(s.room, time + 1, room)]);
}

await Promise.all([db.save(), shard.save()]);
fs.writeFileSync("world.json", JSON.stringify({ time, userIds: users, rooms: names }, null, 2));
console.log(`stood ${samples.length} duels at tick ${time}`);
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
