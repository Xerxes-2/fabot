// Offline, server stopped: a soak node's start (ARCHITECTURE.md §10.2). Runs
// under xxscreeps' own loader, cwd the world directory:
//   xxscreeps.js file://…/node.mjs ground <room>
//     prints the room's ground as `src/bin/plan.rs` reads it
//   xxscreeps.js file://…/node.mjs stand <room> <structures.json> <level> <stored> <username> [<worn>] [<lab>] [<bodies>]
//     after `manage bot add --spawn` stood the first spawn: the controller
//     at <level> (progress 0, its downgrade timer full, the claim's safe
//     mode kept and one more in stock for each level skipped), every other structure of the list ours at full hits on its
//     tile (its roads and containers at <worn> of them, default 1), and <stored> energy in the storage
//     (`<energy>,<resource>:<n>,…` puts each resource there too); <lab> the
//     first lab's store (`<resource>:<n>,…`); <bodies> creeps of ours stood
//     around the first spawn, `<pattern>:<part><n>…[*<count>],…` (as
//     `harvester:work6carry1move3*2`), named as the bot casts them, their
//     ticks to live spread over a lifetime.
//   xxscreeps.js file://…/node.mjs declare <room> <username> <declarations.json> [short | <n>]
//     <username>'s Memory as `{ v2: { declarations } }`, what the bot reads
//     its declarations from (src/app/wire.rs), and the GCL its claims need
//     unless `short`, or <n> rooms past the first; <room> is not read.
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
import * as Spawn from "xxscreeps/mods/classic/spawn/spawn.js";
import * as Tower from "xxscreeps/mods/classic/defense/tower.js";
import * as Storage from "xxscreeps/mods/classic/logistics/storage.js";
import * as Container from "xxscreeps/mods/classic/resource/container.js";
import * as Road from "xxscreeps/mods/classic/road/road.js";
import * as Link from "xxscreeps/mods/classic/logistics/link.js";
import * as Extractor from "xxscreeps/mods/classic/mineral/extractor.js";
import * as Terminal from "xxscreeps/mods/classic/brokerage/terminal.js";
import * as Lab from "xxscreeps/mods/classic/chemistry/lab.js";
import * as Creep from "xxscreeps/mods/classic/creep/creep.js";

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
  const [listPath, levelArg, storedArg, username, wornArg = "1", labStock = "", bodiesArg = ""] = rest;
  const level = Number(levelArg);
  const ours = await User.findUserByName(db, username);
  const list = JSON.parse(fs.readFileSync(listPath, "utf8"));
  const create = {
    extension: (pos) => Extension.create(pos, level, ours),
    tower: (pos) => Tower.create(pos, ours),
    spawn: (pos, i) => Spawn.create(pos, ours, `Node${i}`),
    storage: (pos) => Storage.create(pos, ours),
    container: (pos) => Container.create(pos),
    road: (pos) => Road.create(pos),
    link: (pos) => Link.create(pos, ours),
    extractor: (pos) => Extractor.create(pos, ours),
    terminal: (pos) => Terminal.create(pos, ours),
    lab: (pos) => Lab.create(pos, ours),
  };
  // Their schema is the modern template's alone.
  for (const [kind, path] of [["factory", "factory/factory"], ["observer", "observer/observer"], ["powerSpawn", "powerspawn/powerspawn"], ["nuker", "nuker/nuker"]])
    if (list.some((s) => s.kind === kind)) {
      const mod = await import(`xxscreeps/mods/modern/${path}.js`);
      create[kind] = (pos) => mod.create(pos, ours);
    }
  const bodies = bodiesArg
    .split(",")
    .filter(Boolean)
    .flatMap((spec) => {
      const [, pattern, parts, count = "1"] = /^(\w+):([a-z_0-9]+)(?:\*(\d+))?$/.exec(spec);
      const body = [...parts.matchAll(/([a-z_]+)(\d+)/g)].flatMap(([, type, n]) => Array(Number(n)).fill(type));
      return Array.from({ length: Number(count) }, () => ({ pattern, body }));
    });
  const state = new GameState(world, time, [room]);
  runWithState(state, () =>
    runAsUser(ours, () => {
      const spawns = [...room["#objects"]].filter((o) => o.structureType === "spawn");
      const [first] = list.filter((s) => s.kind === "spawn");
      if (spawns.length !== 1 || spawns[0].pos.x !== first.x || spawns[0].pos.y !== first.y)
        throw new Error(`${roomName}: the plan's first spawn is not the one standing`);
      // Ids in list order, so the engine's tie-breaks by object order repeat.
      let nextId = 0;
      let stocked = false;
      for (const s of list.filter((s) => s !== first)) {
        if (!create[s.kind]) throw new Error(`no ${s.kind} in a node`);
        const object = create[s.kind](new RoomPosition(s.x, s.y, roomName), nextId);
        object.id = (++nextId).toString(16).padStart(24, "0");
        if (s.kind === "storage")
          for (const [i, part] of storedArg.split(",").entries()) {
            const [resource, amount] = i === 0 ? ["energy", part] : part.split(":");
            object.store["#add"](resource, Number(amount));
          }
        // The first lab holds the compound a body is boosted with.
        else if (s.kind === "lab" && labStock && !stocked) {
          stocked = true;
          const minerals = [];
          for (const part of labStock.split(",")) {
            const [resource, amount] = part.split(":");
            if (resource === "energy") object.store["#energy"] = Number(amount);
            else minerals.push([resource, Number(amount)]);
          }
          const [type, amount] = minerals.at(-1) ?? [];
          if (type !== undefined) {
            object.store["#mineralType"] = type;
            object.store["#mineralAmount"] = amount;
          }
        }
        if (s.kind === "road" || s.kind === "container") object.hits = Math.ceil(object.hits * Number(wornArg));
        room["#insertObject"](object);
      }
      const blocked = new Set(list.filter((s) => !["road", "container", "rampart"].includes(s.kind)).map((s) => s.y * 50 + s.x));
      const terrain = world.map.getRoomTerrain(roomName);
      const free = [];
      for (let r = 1; free.length < bodies.length && r < 25; r++)
        for (let y = first.y - r; y <= first.y + r; y++)
          for (let x = first.x - r; x <= first.x + r; x++)
            if (Math.max(Math.abs(x - first.x), Math.abs(y - first.y)) === r && x > 0 && y > 0 && x < 49 && y < 49 && !(terrain.get(x, y) & 1) && !blocked.has(y * 50 + x)) free.push([x, y]);
      if (free.length < bodies.length) throw new Error(`${roomName}: no free tile for ${bodies.length} bodies near the first spawn`);
      for (const [i, { pattern, body }] of bodies.entries()) {
        const creep = Creep.create(new RoomPosition(...free[i], roomName), body, `${pattern}-${time - i}-${spawns[0].name}`, ours);
        creep.id = (++nextId).toString(16).padStart(24, "0");
        creep["#ageTime"] = time + Math.round((1500 * (i + 1)) / bodies.length);
        room["#insertObject"](creep);
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
  const [username, path, short] = rest;
  const declarations = JSON.parse(fs.readFileSync(path, "utf8"));
  const user = await User.findUserByName(db, username);
  await saveMemoryBlob(shard, user, utf16ToBuffer(JSON.stringify({ v2: { declarations } })));
  // A level of GCL a declared claim, at the engine's 1e6 × (level − 1)^2.4.
  const claims = Number(short) || (declarations.claims?.length ?? 0);
  if (claims && short !== "short") await db.data.hSet(User.infoKey(user), "gcl", String(Math.ceil(1e6 * claims ** 2.4)));
  await Promise.all([db.save(), shard.save()]);
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
