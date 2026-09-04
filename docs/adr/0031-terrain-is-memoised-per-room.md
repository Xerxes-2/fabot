# The spatial projection's terrain is memoised per room name

Profiling (#92, found while measuring #86) showed the [[spatial projection]]'s terrain — 2304 tiles inserted into a `Map<Pos, Terrain>` by structural comparison on a `Pos` record — as the single largest producer of `MapTreeModule_add` in the tick, about a quarter of an 8 ms tick, for a fact that never changes: room terrain is fixed for the life of the server, and `Game.map.getRoomTerrain` is a constant-time handle the engine hands back unchanged. We decided **the terrain map is memoised per room name in the App's heap**: the first tick that projects a room reads the engine's terrain once and builds the map, every later tick recalls it, and the projection is still assembled whole each tick — only the terrain field's *source* is a memo rather than a fresh walk. The key is the room name and nothing else, because the room name is the only input the map has; it can never go stale, so there is no signature to perturb, no invalidation, and no timed refresh. The memo is heap state, never Memory: ADR 0011's "computed whole, never persisted" survives exactly as it did for the plan memo (ADR 0017) — a global reset empties the table and the next tick rebuilds it from the engine, at the cost of one tick's walk.

This narrows the per-tick-rebuild axiom (ADRs 0002, 0007) a second time, along the line ADR 0017 drew: what must rebuild per tick is anything read from a Snapshot that changes per tick. ADR 0017 admitted the first exception for a value whose whole input set is the census, which changes rarely; this admits the second for a value whose whole input set never changes at all. Nothing else in the projection qualifies — creeps, structures, sites, roads and stores all move tick to tick and are rebuilt as before.

## Considered Options

- **Status quo (rebuild every tick)** — rejected by measurement: ~25% of the tick spent re-deriving an invariant, and the only `MapTreeModule_add` producer whose input is known never to change.
- **Reshape the projection off `Map<Pos, _>` and `Set<Pos>` to flat arrays** — deferred, not rejected: it would delete this cost rather than cache it, but it touches ADR 0005's one projection and every consumer in the Atlas and the tests, and is the architecture pass #50 left open. The memo keeps `SpatialInfo.Terrain`'s type, so every consumer is untouched, and does not pre-empt that pass.
- **Persist the terrain in Memory** — rejected: serialisation cost every tick for a value the engine already hands back for free, and the first breach of "never persisted" for something the heap keeps at zero cost.
- **Memoise the whole projection, or more of it** — rejected: every other field reads per-tick state; the discipline is that only an input the projection cannot observe changing may outlive the tick.

## Consequences

- The **Spatial projection** glossary entry says its terrain is recalled per room rather than rebuilt, and what that does and does not mean.
- The App gains its second piece of cross-tick heap state beside the plan memo; both live in module-level bindings and neither reaches Memory.
- A multi-room projection (#83) can key the same table by room name without contention; the memo does not otherwise anticipate it.
- `npm run profile` reports the count of engine terrain reads over the run, so the once-per-heap-lifetime claim is checked by the profiler, not by inspection.
