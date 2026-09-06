# Matching breaks rank ties by travel cost, then load

> **Amended by #216 R5** on where rank comes from, never on the key: the Matcher's first component is now a `Priority` field the Planner set on the pooled Task (ADR 0052 decision 6), so `(rank, travel cost, load)` is unchanged as a key and there is no longer a table of Task kinds behind its first term. The Consequences' "Seat caps (and future per-target capacities) are the intended counterweight" to a pile-on is likewise a field now — a `Capacity` the Planner sets and the Matcher counts holders against.

A fresh creep was observed assigned to the farther of two sources, through swamp, because the Matcher chooses by task rank and load only and ties fall to Snapshot ordering — geometry is invisible at assignment time. We decided the Matcher picks for an unassigned creep by **(rank, travel cost, load)**: rank still dominates absolutely, travel cost — the cheapest-path cost from the creep to the Task's Work Area over the spatial projection — breaks ties within a rank, and load only settles exact-cost ties. A Work Area the creep cannot reach means the Task is not applicable to that creep at all; there is no range-based fallback. Sticky Assignments are never re-evaluated for a closer target.

## Considered Options

- **(rank, load, cost)** — balance load first, use distance last. Rejected: no surveyed bot orders load above distance; the community prevents far-target starvation with per-target capacity (which we already have in Seat-derived caps), never with an assigned-count comparator (`docs/research/task-matching-travel-cost.md` §6c).
- **Chebyshev range as the cost proxy** — the community norm (Overmind, Winsley, The International). Rejected for us: their constraint is engine-pathing CPU, which evaporates for a pure Dijkstra flood over a ≤48×48 projection with a handful of creeps; swamp-heavy rooms are exactly where the proxy misleads, and ADR 0001 already prices swamp.
- **Range fallback when nothing is reachable** (TooAngel style). Rejected: a creep marching at a wall is strictly worse than an unmatched creep parking, which the Resolver already arbitrates.

## Consequences

- One multi-goal Dijkstra flood per creep per tick at most, memoised only within the Snapshot (the function stays pure). Reachability of remembered assignments is re-checked every tick — an assignment whose Work Area became unreachable is released — but a creep already standing inside its Work Area skips the flood, so it runs mainly for fresh, reassigning, or still-travelling creeps.
- Same-rank creeps may now pile onto the nearest target up to its capacity instead of spreading; Seat caps (and future per-target capacities) are the intended counterweight.
- Deferred, deliberately: cross-tick path caching, same-tick trade-up between assignees, lifetime-feasibility rejection (Winsley's `distance × 2 > ticksToLive` guard).
