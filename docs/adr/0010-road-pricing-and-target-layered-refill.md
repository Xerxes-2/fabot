# Roads enter the pricing; Refill layers its rank by target

> **Amended by #285** on the *arity* of Repair's threshold, and on nothing else this ADR decided. "Repair enters as a surplus-tier Task (rank 1, triggered below half hits — the threshold is a tunable, not part of this decision)" disclaims its own number, so what ADR 0061 overrides is not the half but the **one**: there are two thresholds now for the decaying kinds, `Tuning.RepairTrigger` (0.5) where a structure nobody holds enters the pool and `Tuning.RepairWholeLine` (0.8) where one a creep is already repairing leaves it, with the assignment table saying which a target is judged by. One repair tick is `Work × 100` hits whatever the structure's max — 22% of a plain road and 0.44% of a container at the live worker's body — so on one number the repair is over the tick it starts: live at t403,2xx, 116 roads in W13S28 at a median of exactly 50.0%, two source containers at a third of max, and zero repair holders fleet-wide. The tier, the rank, the layering by target and the rampart's floor are untouched. **The #234 banner below is overridden in its reason and not in its ruling**: "Repair stays on the old rung, because a repair target leaves the pool the tick its structure is whole and a row lifted onto it churns through task-gone releases (#226)" — after ADR 0061 it does not, so that ground is measured away; the rung itself stays where #234 put it, and re-opening it is a separate decision with its own ticket.

> **Amended by #242** on the *feeding tier's own order*, which this ADR's last Consequence opened by making rank a question of the target rather than of the Task kind: a [[pickup]] whose pile holds half a [[hauler unit]]'s load or more stands a rung above the container [[withdraw]]s it shares that tier with — every one of them that is **not full**, the full source container's two rungs (#216 R5) still standing above the pile, and, a rank being one colony-wide scalar and not a pairing, above the rest of that tier with them: the spawn ring's Refill, the Harvest, the Reserve, the Claim, a tombstone's or a ruin's Withdraw and the income-deciding Builds of ADR 0042 and ADR 0047, at any distance. And where two intakes tie on rank, travel cost and crowding load alike the pool's own order puts the piles first. The question a reader of the rank table must ask widens once more — not "which target" alone but "how much is lying on it", the amount being what decides whether a pile is a whole trip — and the reason is the one this ADR's tier order is built on: a container keeps what it holds and a pile loses `ceil(amount / 1000)` a tick, so of two intakes priced the same the decaying one is the income that is going away. Live, W13S28's haulers and workers drew half-full containers while the ground beside them decayed (user, 2026-09-07). The layering *by target*, the tier boundaries and travel cost's say over every smaller pile are all untouched — with the one regime the line itself has: half the row's cast is the pooling threshold's own hundred until the bank passes 450, so at RCL1 there is no smaller pile and the rung is every pile's.
>
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
