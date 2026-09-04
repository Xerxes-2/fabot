# The spawn walks join the census memo

Profiling after the Atlas landed (#50, fifth comment) showed `planSpawns` at ~24% of the tick, ~19% of it in the lead's spawn-origin walk floods: one Dijkstra per (spawn, body factor) per tick, for a flood whose every input is in the census signature already — the weights are terrain (room name), built roads and obstacles (the structure and site census, plus the controller, which the room fixes), and the successor's body is its pattern sized by a Capacity ADR 0017 already showed the census covers. We decided **the spawn walk table rides the census-keyed plan memo (ADR 0017)**: the `Walks` table the Atlas fills per (spawn, factor) as leads are priced is handed to the next tick's Atlas whenever the signature is unchanged, and dropped whole when it moves. This takes up ADR 0017's own invitation — "anything census-derived added later may join" — and narrows the line its Consequences drew, "floods and paths still rebuild per tick": one flood family now outlives the tick, exactly the one whose input set is the census and nothing else. It stays heap state, never Memory, and a global reset empties it like the rest of the memo.

## Considered Options

- **Status quo (one spawn flood per factor per tick)** — rejected by measurement: a fifth of the tick re-deriving a value that changes with the census, on the order of once per thousand ticks.
- **Memoise the per-creep traffic-blind floods too (Walk / Baseline pricing, keyed by position)** — deferred: the key space moves with every creep, so it needs an eviction rule the census memo has no concept of; a separate decision if a profile ever asks for it.
- **Memoise the weight grid as well** — rejected for now: after #96 it is one 2304-entry fill per tick, not worth a further piece of cross-tick state.
- **An immutable table rebuilt and returned at tick end** — rejected: every entry is a pure function of the signature, so a mutable table filled on demand carries no ordering hazard, and it is the shape the Atlas already uses within a tick.
- **Per-entry invalidation on a signature change** — rejected: a moved signature may have moved the weights or the Capacity, and telling which is a dependency tracker the plan memo deliberately does not have; the Layout is recomputed whole and so is this.
- **A second memo beside the plan memo with its own signature check** — rejected: the same comparison twice.

## Consequences

- `PlanMemo` gains the walk table; `Atlas.ofSnapshot` receives it (or a fresh one) rather than always creating one. The `decide` seam is unchanged.
- The guard inverts ADR 0017's: beside the tests that every census input perturbs the signature, a test asserts that two Snapshots with equal signatures yield bitwise-equal weights, perturbing only non-census fields. A signature gap for a weights input would otherwise price leads off a stale grid until a reset.
- The **Lead**, **Census signature** and **Atlas** glossary entries say the spawn walks are recalled while the census holds.
- The profiling harness's frozen stub now measures the spawn walks at zero, as it does the Layout; the census-change tick's cost is a harness concern tracked separately.
- ADR 0001/0002's "cross-tick path caching deferred" still holds for every flood that reads a creep's position or the tick's occupancy.
