# Movement is an arbitrated intent, not an executor side effect

A creep that finished harvesting would upgrade the controller in place, squatting the mining Seat and blocking the next harvester — because standing tiles were represented nowhere: movement happened only as the Executor's `moveTo` fallback on `ERR_NOT_IN_RANGE`. We decided that no movement is ever issued outside a pure per-room Resolver: every creep's Move Intent (candidate standing tiles derived from its Task's Work Area, plus a priority reusing the task rank) is collected each tick and arbitrated once — priority descending, most-constrained first, swap when contested — following screeps-cartographer's semantics. The essential rule this buys: a creep with slack in its Work Area yields to a creep without.

## Considered Options

- **Seat reservation ledger** (The International style): assignment reserves concrete tiles, other tasks avoid them. Rejected as not root-cause — it models one contention (mining seats) instead of yielding in general; Seat *counting* survives as the Harvest capacity bound.
- **Push-on-block inside movement** (Overmind style): keep per-creep `moveTo`, shove blockers ad hoc. Rejected on TooAngel's evidence that pairwise push rules accrete badly.
- **Tick-end arbitration** (chosen): movement becomes a first-class Intent, same shape as the rest of the decision pipeline.

## Consequences

- Pathfinding moves into Core as a pure Dijkstra over the Snapshot's terrain projection — swamp cost is real on our common paths, so terrain is three-state (wall/swamp/plain, cost 1/5) from day one. We accept owning pathfinding in exchange for a fully testable decision chain; the engine's `PathFinder` is not used.
- `creep.moveTo` is forbidden everywhere; the Executor only replays resolved single-step moves (a creep may act and move in the same tick).
- Deferred, deliberately: container/static mining, construction-site eviction, path caching.
