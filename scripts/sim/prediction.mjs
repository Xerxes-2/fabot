// The prediction error (ARCHITECTURE.md §10.1), one set of definitions for
// `npm run exchange`'s duels and the scenarios' engagements. A row is
// { p: what Exchange predicted, { kind: win|kite|lose, ticks, dead, hits },
// a: what happened, { won: true, false or null (both stood, or its rival left), left?, unresolved?, ticks, dead, hits } }.
// Each combat part's boosts by tier, and the mineral one boosted part takes (LAB_BOOST_MINERAL).
export const COMPOUNDS = {
  tough: ["GO", "GHO2", "XGHO2"],
  attack: ["UH", "UH2O", "XUH2O"],
  ranged_attack: ["KO", "KHO2", "XKHO2"],
  heal: ["LO", "LHO2", "XLHO2"],
  move: ["ZO", "ZHO2", "XZHO2"],
  work: ["ZH", "ZH2O", "XZH2O"],
};
export const BOOST_MINERAL = 30;

export const outcome = (a) => (a.won === true ? "won" : a.won === false ? "lost" : a.left ? "left" : a.unresolved ? "unresolved" : "stood");

// An engagement's outcome, off the facts `scripts/scenario/engaged.mjs` or the bot's
// `engagements.log` writes. Won: its rival dead at the close. Left: closed
// while its rival stands, or with a rival body not seen to die
// (`left`, the bot's): no evidence for a Win. Lost: after contact it disengaged, or
// gave up (a home's turned unanswered after it was answered), whatever came
// after; one that quit before contact, none of ours dead, tested no
// prediction. Unresolved: still open and answered when the run ended. Ticks
// from first contact to the rival's last sighting, as `sim duel` reads a duel.
export function engagement(e) {
  const quit = e.disengaged || e.gaveUp;
  const left = !quit && e.closed !== null && Boolean(e.left);
  const won = quit ? (e.contact === null && e.dead === 0 ? null : false) : e.closed !== null && !left ? true : null;
  const end = won === true ? e.seen + 1 : e.closed;
  return { won, left, unresolved: won === null && e.closed === null && e.answered, ticks: won === null || e.contact === null || end === null ? null : end - e.contact, dead: e.dead };
}

// The rows a run's engagements or the live `engagements.log` (segment 4) hold,
// as `report` reads them: one never priced predicted nothing.
export const tested = (rows) =>
  rows.filter((e) => e.predicted).map((e) => ({ id: e.id, p: e.predicted, a: engagement(e), room: e.room, opened: e.opened, closed: e.closed, tactic: e.tactic, rivals: e.rivals, energy: e.energy }));

// A predicted Win or Kite that lost is an Exchange bug; one that timed out, a miss.
export const bugs = (rows) => rows.filter(({ p, a }) => p.kind !== "lose" && a.won === false);
export const stood = (rows) => rows.filter(({ p, a }) => p.kind !== "lose" && outcome(a) === "stood");
export const misses = (rows) => rows.filter(({ p, a }) => p.kind === "lose" && a.won === true);

const quantiles = (xs) => {
  if (xs.length === 0) return "none";
  const sorted = [...xs].sort((a, b) => a - b);
  const q = (f) => sorted[Math.min(sorted.length - 1, Math.floor(f * sorted.length))];
  return `median ${q(0.5)} p10 ${q(0.1)} p90 ${q(0.9)} (n ${xs.length})`;
};

export function report(rows) {
  const table = {};
  for (const { p, a } of rows) table[`${p.kind}→${outcome(a)}`] = (table[`${p.kind}→${outcome(a)}`] ?? 0) + 1;
  const timed = rows.filter(({ p, a }) => p.kind !== "lose" && a.won === true && a.ticks !== null);
  const won = timed.filter(({ p }) => p.kind === "win");
  return [
    `predicted→actual: ${Object.entries(table).sort().map(([k, n]) => `${k} ${n}`).join(", ")}`,
    `predicted Win/Kite that lost (bugs): ${bugs(rows).length}`,
    `predicted Win/Kite that timed out (misses): ${stood(rows).length}`,
    `predicted Lose that won: ${misses(rows).length}`,
    `tick error, predicted − actual, on predicted wins won: ${quantiles(timed.map(({ p, a }) => p.ticks - a.ticks))}`,
    `bodies lost error on predicted Win won: ${quantiles(won.map(({ p, a }) => p.dead - a.dead))}`,
    ...(rows.every(({ a }) => a.hits === undefined) ? [] : [`hits lost error on predicted Win won: ${quantiles(won.map(({ p, a }) => p.hits - a.hits))}`]),
  ];
}
