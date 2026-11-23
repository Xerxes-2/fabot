// A capture's ticks (`engine/capture.js`) stood whole: its objects are written
// as changes since the tick before and a room's terrain once, so each tick is
// rebuilt from the ones before it in its file. A `full` tick starts again,
// and may carry every room's terrain (sim's), seen or not.
import { createReadStream } from "node:fs";
import { createInterface } from "node:readline";

export async function* ticksOf(files) {
  for (const file of files) {
    let objects = {};
    let controllerStructures;
    const terrain = new Map();
    for await (const line of createInterface({ input: createReadStream(file), crlfDelay: Infinity })) {
      if (!line) continue;
      const { full, gone, terrain: all, ...tick } = JSON.parse(line);
      // A tick without `gone` is from before changes were written: whole.
      if (full || gone === undefined) objects = {};
      for (const k of gone ?? []) delete objects[k];
      Object.assign(objects, tick.objects);
      for (const [name, t] of Object.entries(all ?? {})) terrain.set(name, Uint8Array.from(t, Number));
      tick.controllerStructures = controllerStructures = tick.controllerStructures ?? controllerStructures;
      // Standing a tick reads its records and never writes them.
      tick.objects = { ...objects };
      for (const r of tick.rooms) {
        if (r.terrain !== undefined) terrain.set(r.name, Uint8Array.from(r.terrain, Number));
        r.terrain = terrain.get(r.name);
      }
      tick.terrain = terrain;
      yield tick;
    }
  }
}
