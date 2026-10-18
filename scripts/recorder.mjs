// The recorder's ring (`src/app/record.rs`) back into `scripts/bench.mjs`'s
// capture ticks, oldest first, terrain aside: a tick whose parts the ring no
// longer holds whole is left out.
import { createRequire } from "node:module";
import { inflateRawSync } from "node:zlib";

createRequire(import.meta.url)("../src/app/read.js");

export function unpack(segments) {
  const ticks = {};
  for (const text of segments.filter(Boolean)) {
    const [time, part, parts, chunk] = text.split(" ");
    (ticks[time] ??= { parts: Number(parts), chunks: [] }).chunks[part] = chunk;
  }
  return Object.entries(ticks)
    .filter(([, t]) => t.chunks.filter(Boolean).length === t.parts)
    .sort(([a], [b]) => a - b)
    .map(([, t]) => {
      const fields = inflateRawSync(Buffer.from(t.chunks.join(""), "base64")).toString().split("\0");
      const [head, memory, intel, ints] = fields.splice(0, 4);
      const tick = globalThis.__fabotUnread(Int32Array.from(ints.split(","), Number), fields.join("\0"));
      return { ...tick, cpu: { ...tick.cpu, limit: Number(head.split(" ")[1]) }, memory, intel };
    });
}
