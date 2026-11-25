// Segment 3 (ARCHITECTURE.md §4) in the format `Metrics::text`
// (core/observe/metrics.rs) writes, and that file's names and window, read off its source.
import { readFileSync } from "node:fs";

export const METRICS = 3;
export const SOURCE = readFileSync(new URL("../core/observe/metrics.rs", import.meta.url), "utf8");
/** Each metric name by its Rust const's (`EXPOSURE` → "exposure"); a const metrics.rs no longer holds throws. */
export const METRIC = new Proxy(Object.fromEntries([...SOURCE.matchAll(/^const (\w+): &str = "([^"]+)";/gm)].map((m) => [m[1], m[2]])), {
  get(names, k) {
    if (typeof k === "string" && !(k in names)) throw new Error(`core/observe/metrics.rs holds no metric const ${k}`);
    return names[k];
  },
});
export const WINDOW = Number(/^pub const WINDOW: i32 = (\d+);/m.exec(SOURCE)[1]);

export function windows(text) {
  const [head, ...lines] = (text ?? "").split("\n").filter(Boolean);
  if (!head?.startsWith("names ")) return [];
  const names = head.split(" ").slice(1);
  const out = [];
  let start, ticks;
  for (const line of lines) {
    const [first, cells = ""] = line.split(" ");
    if (first.startsWith("@")) {
      [start, ticks] = [Number(first.slice(1)), Number(cells)];
      continue;
    }
    const values = {};
    cells.split(",").forEach((v, i) => v && (values[names[i]] = Number(v)));
    out.push({ start, ticks, row: first, values });
  }
  return out;
}
