# Roads enter the pricing; Refill layers its rank by target

> **Amended by #234** on the *surplus tier's own order*, which this ADR opened by putting a tower's Refill into it: the tier is two rungs and not one. A construction site in the colony's own [[home room]] stands a step above the tower Refill, the Repair and the Upgrade it used to sit level with, in every colony's own pool and no longer only in a [[nursery]] or an [[outpost]] (ADR 0042, ADR 0047). Only travel cost had separated them, and a loaded generalist fills at the upgrade [[buffer]] and is left standing inside the controller's Upgrade [[work area]], where the Upgrade costs it a step, applies to any load and never goes task-gone — so live at t195,8xx two colonies held fifty construction sites between them, a storage and five extensions among them, while every worker upgraded. "The colony feeds its own reproduction before its guns" survives one rung further out: a site the colony grows by comes before both. The rung stops at the home room, because past the [[seam]] a rank the whole colony shares is what #157's two-builder budget exists to bound and no budget covers a human's site out there. Repair stays on the old rung, because a repair target leaves the pool the tick its structure is whole and a row lifted onto it churns through task-gone releases (#226); the layering *by target* this ADR decided is untouched, and so is the [[downgrade deadline]]'s place above the whole sequence (ADR 0007).

> **Revised by ADR 0023**: Refill's target layering gains one more tier below the controller container — the Storage, the place surplus goes when even the upgrade buffer is full.

Observation after the Anchor landed (ADR 0006): the current room has zero
Dual Seats — no source Seat lies inside the controller's Work Area — so
every drop of upgrade energy commutes, at half speed loaded, over a Work
Area that is one-third swamp. Roads are the lever, but a road the Atlas
cannot price is scenery: creeps would keep walking the terrain-optimal
line beside it. And a tower (coming at RCL3) eats energy with no Task
that feeds it.

We decided two things, revising ADR 0002's travel-cost semantics (as
already revised by ADRs 0006 and 0008):

1. **Roads enter the terrain weights.** Travel cost prices steps at road
   1 / plain 2 / swamp 10 — the engine's own per-part fatigue costs,
   which the old plain 1 / swamp 5 scale was half of. The occupancy
   surcharge (ADR 0008) is re-expressed in the same units (10, formerly
   5); nothing else about the flood changes. The spatial projection
   carries which tiles hold a road (built structures only — a road site
   is not yet a road).
2. **Refill widens to any energy-hungry structure, with rank layered by
   target.** The Refill Task now covers towers as well as spawn-feeding
   structures; the Planner still filters by free capacity. Rank splits by
   target for the first time: spawn-feeding Refill stays in the feeding
   tier (rank 0), tower Refill sits in the surplus tier (rank 1) — a
   colony feeds its own reproduction before its guns. To a creep both are
   the same transfer; splitting the Task kind would buy model purity with
   runtime complexity.

## Considered Options

- **Build roads without teaching the Atlas.** Rejected: pathing is
  cost-driven (ADR 0002); an unpriced road is never preferred, so the
  energy spent building it is wasted by construction.
- **A separate tower-feeding Task kind.** Rejected: same action, same
  applicability, same emitter output as Refill — a new term with no new
  behaviour behind it.
- **Containers and drop-mining** (the other answer to a room with no
  Dual Seats: static heavy-WORK miners plus haulers). Explicitly
  deferred, not rejected — it reshapes Refill's sourcing semantics and
  deserves its own decision. This ADR must not be read as having chosen
  against it.

## Consequences

- Travel cost's unit doubles (half-ticks, not ticks). All comparisons
  are relative so matching is unaffected, but any absolute reading of a
  cost in a Verdict or test must halve it to get ticks.
- Repair becomes load-bearing: roads decay, and a colony that can price
  them must keep them alive. Repair enters as a surplus-tier Task
  (rank 1, triggered below half hits — the threshold is a tunable, not
  part of this decision).
- Rank is no longer a function of Task kind alone. The matching key
  (ADR 0002) is unchanged in shape, but readers of the rank table must
  now ask "which target", not just "which Task".
