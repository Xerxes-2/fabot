// Season 11's rooms with a source, for `fabot_base::testing::real_rooms`: each
// room's terrain and its sources, controller and mineral, from a copy of the
// season's room history (its README), so unit tests sweep real ground anywhere.
//
// Per room, then raw deflate over all: i8 x, i8 y (season-world.mjs's), u8 n,
// n × u16 source tile, u16 controller tile, u16 mineral tile (0xffff none),
// u8 mineral type (ASCII, 0 none), 625 bytes of terrain as terrain.bin holds it.
//
// Usage: node scripts/real-rooms.mjs <archive> <out.bin>
import { closeSync, openSync, readFileSync, readSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { deflateRawSync } from "node:zlib";

const [archive, out] = process.argv.slice(2);
if (!archive || !out) {
  console.error("usage: real-rooms.mjs <archive> <out.bin>");
  process.exit(1);
}
process.removeAllListeners("warning");
const { readFooter, unpackBlock, segmentFile, RECORD_HEAD, SEGMENT_TICKS } = await import(
  join(archive, "columns.mjs")
);
const map = readFileSync(join(archive, "terrain.bin"));
const coord = (s) => {
  const m = /^([WE])(\d+)([NS])(\d+)$/.exec(s);
  return [m[1] === "E" ? Number(m[2]) : -Number(m[2]) - 1, m[3] === "S" ? Number(m[4]) : -Number(m[4]) - 1];
};

// An object the history meets first is written whole, and a room's first
// listing names every object in it: the block holding any object holds its rocks.
const found = new Map();
for (let seg = 0; seg <= 1500000; seg += SEGMENT_TICKS) {
  const path = join(archive, "segments", segmentFile(seg));
  const fd = openSync(path, "r");
  for (const r of readFooter(path).rows) {
    const [room, , , , off, len, , fmt] = r;
    if (found.has(room)) continue;
    const frame = Buffer.allocUnsafe(len);
    readSync(fd, frame, 0, len, off + RECORD_HEAD);
    const objects = new Map();
    for (const body of unpackBlock(frame, fmt).bodies)
      for (const tick of Object.values(JSON.parse(body).ticks))
        for (const [id, o] of Object.entries(tick ?? {})) if (o?.type) objects.set(id, o);
    if (objects.size > 0) found.set(room, [...objects.values()]);
  }
  closeSync(fd);
}

const records = [];
const tile = (o) => (o ? o.y * 50 + o.x : 0xffff);
const u16 = (v) => [v & 0xff, v >> 8];
for (const [room, objects] of [...found].sort(([a], [b]) => (a < b ? -1 : 1))) {
  const sources = objects.filter((o) => o.type === "source").sort((a, b) => tile(a) - tile(b));
  if (sources.length === 0) continue;
  const controller = objects.find((o) => o.type === "controller");
  const mineral = objects.find((o) => o.type === "mineral");
  const [x, y] = coord(room);
  const at = ((y + 51) * 102 + x + 51) * 625;
  records.push(
    Buffer.from([x & 0xff, y & 0xff, sources.length, ...sources.flatMap((s) => u16(tile(s)))]),
    Buffer.from([...u16(tile(controller)), ...u16(tile(mineral)), mineral ? mineral.mineralType.charCodeAt(0) : 0]),
    map.subarray(at, at + 625),
  );
}
const packed = deflateRawSync(Buffer.concat(records), { level: 9 });
writeFileSync(out, packed);
console.error(`${records.length / 3} rooms, ${packed.length} bytes`);
