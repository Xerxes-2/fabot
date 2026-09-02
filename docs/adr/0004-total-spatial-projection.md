# The spatial projection is total; geometry is asked through the Atlas

Six call sites each invented their own answer to "what if the projection is missing a piece": no projection meant floor-only workforce, uncapped Harvest, zero travel cost, unconditionally emitted actions, and no movement — each policy re-derived locally off one `option`, and every new consumer had to invent a seventh. We decided the projection is always present (possibly empty) and absence is per-entry: an entity the projection does not place is simply unpriceable. All geometry questions go through the Atlas — the per-tick, task-aware query interface (seats, work areas, travel costs, first steps, action permission, standing candidates, placed creeps) — and each query gives one documented answer for unpriceable geometry, under a single policy: **geometry that cannot be priced never counts against a Task and never blocks an action**.

## Considered Options

- **Keep `Spatial: option` with per-caller fallbacks** (status quo). Rejected: an empty projection is behaviour-identical to an absent one on every existing path — the per-entry absences already cover it — so the `option` was ceremony that each caller paid for with its own degradation policy.
- **Position-keyed pure-geometry queries** (`travelCost : Pos -> ...`, callers resolve creep placement first). Rejected: it leaves the unplaced-creep policies (cost 0, act unconditionally, no move) outside the module, so the consolidation this decision exists for only half-happens. Queries are keyed by creep name and Task; placement lookups live inside.

## Consequences

- The Atlas is a second public interface in Core beside `decide`: geometry gets its own test surface, and Matcher and Resolver provably consult the same flood (memoised per start tile within the tick — extending ADR 0002's within-Snapshot memoisation to the Resolver, so a travelling creep floods once, not twice).
- Behaviour under missing geometry is unchanged; this was an intent-preserving deepening. Tests that used `Spatial = None` now build the empty projection.
- Policy stays outside: task ranks, glyphs, workforce and capacity rules, and arbitration read the Atlas but do not live in it.
- Deferred, deliberately: folding the Placement projection into the Atlas, and honest pricing of targets outside the projected room (both tracked as their own deepening candidates).
