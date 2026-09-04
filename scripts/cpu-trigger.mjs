// ADR 0041's condition to revisit the layered projection, in one place
// for the two readers that state it: `npm run profile`, which measures a
// scenario off the clock, and `npm run observe cpu`, which reads the
// per-tick line the bot writes to Memory.
//
// The rule lives here rather than in Core deliberately. ADR 0041 decided
// CPU is *measured, not budgeted* — a budget exists to size a territory
// and this colony's territory is a constant (ADR 0006's row arriving
// without its quota) — so nothing the bot can act on may carry these
// numbers. Guarding `Game.cpu.bucket` by skipping ticks collides with the
// safe-mode reflex (ADR 0007, ADR 0015), which has to be able to fire on
// the very tick a guard would skip; that trade belongs with the defence
// work and is not decided here.

/// A mean tick above this many milliseconds sends the decision back to
/// ADR 0041.
export const MEAN_MS = 50;

/// A single tick above this many milliseconds does the same on its own:
/// the shard's limit is a flat 100 that does not grow with GCL, and one
/// tick over 80 is one tick of headroom from being cut off mid-loop.
export const TICK_MS = 80;

/// Read the trigger off a window of `{ t, ms }` rows — a tick number and
/// what that tick cost, which is the shape both readers already hold: the
/// profile's own clock and the line the bot writes. Every row carries its
/// tick, so a tripped max always names the tick to go and look at.
/// Module-private: `report` is the seam, and a second shape of judgement
/// would be a second place ADR 0041's rule is stated.
function judge(rows) {
  if (rows.length === 0) return null;

  const mean = rows.reduce((total, r) => total + r.ms, 0) / rows.length;
  const worst = rows.reduce((a, b) => (b.ms > a.ms ? b : a));
  return {
    n: rows.length,
    mean,
    max: worst.ms,
    worstTick: worst.t,
    meanTripped: mean > MEAN_MS,
    maxTripped: worst.ms > TICK_MS,
  };
}

/// The judgement as a person reads it: the rule, the two numbers it is
/// read against, and which half tripped. One line of rule and one of
/// verdict, so a run that is fine says so as plainly as one that is not —
/// a trigger nobody can see in the output is a feeling again. An empty
/// window is answered rather than thrown on: this takes whatever window
/// its caller holds, and a caller that can say something better about
/// having none (`observe.mjs cpu` tells a bundle that has written no
/// finished tick from one whose rows are all off the wire shape) says it
/// before ever reaching here.
export function report(rows) {
  const at = (tick) => ` (t${tick})`;
  const verdict = judge(rows);
  if (!verdict) {
    return (
      `ADR 0041 revisit trigger (mean tick > ${MEAN_MS} ms, or any single tick > ${TICK_MS} ms)\n` +
      "  no ticks measured — nothing to judge"
    );
  }

  const tripped = [
    verdict.meanTripped ? `mean over ${MEAN_MS} ms` : null,
    verdict.maxTripped ? `a tick over ${TICK_MS} ms${at(verdict.worstTick)}` : null,
  ].filter(Boolean);

  return (
    `ADR 0041 revisit trigger (mean tick > ${MEAN_MS} ms, or any single tick > ${TICK_MS} ms)\n` +
    `  mean ${verdict.mean.toFixed(2)} ms  max ${verdict.max.toFixed(2)} ms` +
    `${at(verdict.worstTick)} over ${verdict.n} tick${verdict.n === 1 ? "" : "s"}\n` +
    (tripped.length
      ? `  TRIGGERED: ${tripped.join(", ")} — the layered projection goes back to ADR 0041`
      : "  not triggered")
  );
}
