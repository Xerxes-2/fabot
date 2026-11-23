// The tile a first spawn is seated on (`scripts/sim/run.mjs`).
// `terrain(x, y)` is the tile's mask, `taken` the "x,y" of the room's objects,
// `anchors` its controller and sources; null when no tile fits.
const range = (a, x, y) => Math.max(Math.abs(a.x - x), Math.abs(a.y - y));
export function seatTile(terrain, taken, anchors) {
  let best = null;
  for (let y = 3; y <= 46; y++) {
    for (let x = 3; x <= 46; x++) {
      if (terrain(x, y) !== 0 || taken.has(`${x},${y}`)) continue;
      if (anchors.some((a) => range(a, x, y) < 3)) continue;
      if ([-1, 0, 1].some((dy) => [-1, 0, 1].some((dx) => terrain(x + dx, y + dy) & 1))) continue;
      const score = anchors.reduce((sum, a) => sum + range(a, x, y), 0);
      if (best === null || score < best.score) best = { x, y, score };
    }
  }
  return best;
}
