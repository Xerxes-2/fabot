// Segment 3 (ARCHITECTURE.md §4) in the format `Metrics::text`
// (src/core/observe/metrics.rs) writes.
export const METRICS = 3;

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
