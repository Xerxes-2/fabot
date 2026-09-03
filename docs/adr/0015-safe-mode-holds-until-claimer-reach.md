# Safe mode holds until a claimer can reach the controller

ADR 0007 fired safe mode on *sight* of a CLAIM-part hostile, reasoning that the tap it was about to land would itself block activation for 1,000 ticks. With towers now shooting (ADR 0014) that turns out to be over-proved: `attackController` is a **range-1** act judged from tick-start position, and a creep steps at most one tile a tick, so a claimer seen at range 2 cannot land its tap before an activation issued the same tick takes effect. The precise deadline is range 2; sight was just its most conservative approximation. We decided the reflex **holds while every claimer is farther than range 3 of the controller** — the exact deadline plus one tile of margin so a single skipped tick (CPU bucket exhaustion) cannot slip a claimer past the boundary unseen — and fires the tick one closes to 3 or nearer. The hold is free: activation still always wins the race, and every held tick is a tick the fire reflex may kill the claimer en route, saving the stock outright. No damage arithmetic, no tower coordination, no CLAIM-priority targeting is needed — safety does not depend on the towers hitting anything, so ADR 0014's nearest-first rule stands untouched. A controller the projection cannot place has no deadline to measure and falls back to firing on sight (the Atlas totality discipline, ADR 0004). The measure is the claimer's position against the controller's tile, both facts the Snapshot already carries since ADR 0014 — the projection widens by nothing.

## Considered Options

- **Keep firing on sight** — rejected: provably earlier than necessary; every early activation spends an irreplaceable stock a tower kill might have saved.
- **Hold while expected tower damage along the approach exceeds the claimer's hits** — rejected: requires speed, path, escort-healer and focus arithmetic, plus CLAIM-priority tower targeting to make the guarantee real; the range gate gets the whole saving with none of it, because it never relies on the kill happening.
- **The exact deadline, range 2, no margin** — rejected: correct only if the reflex runs every tick; one skipped tick at exactly range 2 loses the room. One tile of margin is cheap insurance.
- **Path-distance instead of Chebyshev range** — rejected: terrain can only make the claimer slower, so straight-line range is the conservative bound; pricing a flood for a reflex buys nothing.

## Consequences

- A claim wave standing off at range beyond 3 no longer drains the stock by mere presence — feinting claimers cost us nothing now.
- The towers get the whole approach as their window; if they kill the claimer, the activation is never spent at all.
- ADR 0007's downgrade-deadline half is untouched, and its "on sight" clause survives only as the fallback for an unplaced controller.
- The margin constant (`safeModeDeadline` = 3) restates an engine mechanic (1 step/tick + range-1 tap at tick-start position) in Core, like `directionCode` before it; an engine change to movement or attackController must be mirrored by hand.
