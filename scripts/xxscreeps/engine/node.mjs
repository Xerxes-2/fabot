// Offline, server stopped: a soak node's start (ARCHITECTURE.md §10.2). Runs
// under xxscreeps' own loader, cwd the world directory:
//   xxscreeps.js file://…/node.mjs ground <room>
//     prints the room's ground as `scripts/plan` reads it
//   xxscreeps.js file://…/node.mjs stand <room> <structures.json> <level> <stored> <username>
//     after `manage bot add --spawn` stood the first spawn: the controller
//     at <level> (progress 0, its downgrade timer full, the claim's safe
//     mode kept and one more in stock for each level skipped), every other structure of the list ours at full hits on its
//     tile, and <stored> energy in the storage.
//   xxscreeps.js file://…/node.mjs declare <room> <username> <declarations.json>
//     <username>'s Memory as `{ v2: { declarations } }`, what the bot reads
//     its declarations from (src/App/Wire.fs); <room> is not read.
//   xxscreeps.js file://…/node.mjs unsafe <room>
//     <room>'s safe mode ended and none left to activate.
//   xxscreeps.js file://…/node.mjs ended <room>
//     <room>'s safe mode ended, its stock kept.
import * as fs from "node:fs";
import { Database, Shard } from "xxscreeps/engine/db/index.js";
import * as User from "xxscreeps/engine/db/user/index.js";
import { GameState, runAsUser, runWithState } from "xxscreeps/game/index.js";
import { RoomPosition } from "xxscreeps/game/position.js";
import { flushUsers } from "xxscreeps/game/room/room.js";
import { updateUserRoomRelationships } from "xxscreeps/engine/processor/model.js";
import { updateRoomStatus } from "xxscreeps/mods/classic/controller/controller.js";
import { CONTROLLER_DOWNGRADE } from "xxscreeps/mods/classic/controller/constants.js";
import { saveMemoryBlob } from "xxscreeps/mods/meta/memory/model.js";
import { utf16ToBuffer } from "xxscreeps/utility/string.js";
import { Source } from "xxscreeps/mods/classic/source/source.js";
import { Mineral } from "xxscreeps/mods/classic/mineral/mineral.js";
import * as Extension from "xxscreeps/mods/classic/spawn/extension.js";
import * as Tower from "xxscreeps/mods/classic/defense/tower.js";
import * as Storage from "xxscreeps/mods/classic/logistics/storage.js";
import * as Container from "xxscreeps/mods/classic/resource/container.js";
import * as Road from "xxscreeps/mods/classic/road/road.js";
import * as Link from "xxscreeps/mods/classic/logistics/link.js";
import * as Extractor from "xxscreeps/mods/classic/mineral/extractor.js";
import * as Terminal from "xxscreeps/mods/classic/brokerage/terminal.js";
import * as Lab from "xxscreeps/mods/classic/chemistry/lab.js";

const [mode, roomName, ...rest] = process.argv.slice(2);
const db = await Database.connect();
const shard = await Shard.connect(db, "shard0");
const world = await shard.loadWorld();
const time = shard.time;
const room = await shard.loadRoom(roomName, time);
const xy = (o) => [o.pos.x, o.pos.y];

if (mode === "ground") {
  const terrain = world.map.getRoomTerrain(roomName);
  const objects = [...room["#objects"]];
  console.log(
    JSON.stringify({
      room: roomName,
      terrain: Array.from({ length: 2500 }, (_, i) => terrain.get(i % 50, Math.floor(i / 50))).join(""),
      sources: objects.filter((o) => o instanceof Source).map(xy),
      minerals: objects.filter((o) => o instanceof Mineral).map(xy),
      controller: xy(room.controller),
    }),
  );
} else if (mode === "stand") {
  const [listPath, levelArg, storedArg, username] = rest;
  const level = Number(levelArg);
  const ours = await User.findUserByName(db, username);
  const list = JSON.parse(fs.readFileSync(listPath, "utf8"));
  const create = {
    extension: (pos) => Extension.create(pos, level, ours),
    tower: (pos) => Tower.create(pos, ours),
    storage: (pos) => Storage.create(pos, ours),
    container: (pos) => Container.create(pos),
    road: (pos) => Road.create(pos),
    link: (pos) => Link.create(pos, ours),
    extractor: (pos) => Extractor.create(pos, ours),
    terminal: (pos) => Terminal.create(pos, ours),
    lab: (pos) => Lab.create(pos, ours),
  };
  const state = new GameState(world, time, [room]);
  runWithState(state, () =>
    runAsUser(ours, () => {
      const spawns = [...room["#objects"]].filter((o) => o.structureType === "spawn");
      const [first, ...others] = list.filter((s) => s.kind === "spawn");
      if (spawns.length !== 1 || spawns[0].pos.x !== first.x || spawns[0].pos.y !== first.y || others.length > 0)
        throw new Error(`${roomName}: the plan's first spawn is not the one standing`);
      // Ids in list order, so the engine's tie-breaks by object order repeat.
      let nextId = 0;
      for (const s of list.filter((s) => s.kind !== "spawn")) {
        if (!create[s.kind]) throw new Error(`no ${s.kind} in a node`);
        const object = create[s.kind](new RoomPosition(s.x, s.y, roomName));
        object.id = (++nextId).toString(16).padStart(24, "0");
        if (s.kind === "storage") object.store["#add"]("energy", Number(storedArg));
        room["#insertObject"](object);
      }
      room["#flushObjects"](state);
      room.controller.safeModeAvailable += level - room.controller.level;
      updateRoomStatus(room, level, ours);
      room.controller["#progress"] = 0;
      room.controller["#downgradeTime"] = time + CONTROLLER_DOWNGRADE[level];
    }),
  );
  const previous = flushUsers(room);
  await Promise.all([updateUserRoomRelationships(shard, room, previous), shard.saveRoom(roomName, time, room), shard.saveRoom(roomName, time + 1, room)]);
  await Promise.all([db.save(), shard.save()]);
  console.log(`stood ${list.length} structures at level ${level} in ${roomName}`);
} else if (mode === "declare") {
  const [username, path] = rest;
  const declarations = JSON.parse(fs.readFileSync(path, "utf8"));
  await saveMemoryBlob(shard, await User.findUserByName(db, username), utf16ToBuffer(JSON.stringify({ v2: { declarations } })));
  await shard.save();
  console.log(`declared ${JSON.stringify(declarations)}`);
} else if (mode === "unsafe" || mode === "ended") {
  room["#safeModeUntil"] = 0;
  if (mode === "unsafe") [...room["#objects"]].find((o) => o.structureType === "controller").safeModeAvailable = 0;
  await Promise.all([shard.saveRoom(roomName, time, room), shard.saveRoom(roomName, time + 1, room)]);
  await shard.save();
  console.log(`${roomName}'s safe mode ${mode}`);
} else throw new Error(`node.mjs: unknown mode ${mode}`);
await shard[Symbol.asyncDispose]();
await db[Symbol.asyncDispose]();
