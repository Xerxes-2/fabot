# Census-derived plans recompute only when their census signature changes

Profiling (#50) falsified ADR 0011's premise that "for a deterministic planner the full computation is free": at 8 creeps the Layout is 32% of the tick — `trunkPath` alone 26% — recomputed every tick to emit, almost always, nothing, and the hauler quota's two traffic-blind floods per source container repeat the same waste in miniature. We decided **a plan derived from the census alone is memoised on its census signature**: the Layout (and the hauler quota, which reads the same inputs) keeps its last result in heap beside the signature of exactly the inputs it read — the (kind, position) census of standing structures and pending sites, the controller level, the room name — and recomputes only when the signature differs. This is determinism cashed in, not weakened: same census, same plan, so the memo never changes observable behaviour, and ADR 0011's "computed whole, no persisted plan" survives as *no plan in Memory* — the heap memo dies with every global reset and the next tick recomputes from scratch. The per-tick-rebuild axiom (ADRs 0002, 0007) narrows rather than falls: what must rebuild per tick is anything read from a Snapshot that changes per tick; a value whose entire input set is the census may outlive the tick exactly as long as the census does. There is deliberately **no timed fallback refresh**: the signature is derived mechanically from the same census queries the plan reads, a test asserts every input perturbs the signature, and a periodic blind recompute would only mask a signature gap as an occasional stall instead of a reproducible one.

## Considered Options

- **Status quo (recompute every tick)** — rejected by measurement: the "free" in ADR 0011 was written before a profiler looked; 32% of the tick for a plan that changes on the order of once per thousand ticks.
- **Every-N-ticks recompute** — rejected: imports an arbitrary constant into a deterministic planner, delays a due site by up to N ticks, and answers "when is the plan stale?" with a clock instead of with the plan's own inputs.
- **Persist the plan in Memory** — rejected: serialisation cost every tick, an invalidation story across deploys, and the first genuine breach of "never persisted" for something the heap already keeps at zero cost.
- **Timed safety refresh beside the signature** — rejected: it converts a signature bug from a loud, bisectable stall into a quiet hiccup, and the house style keeps no dark constants.

## Consequences

- The Layout glossary entry loses "every tick" and gains the census signature; the signature term enters the glossary.
- A signature gap is the failure mode: a census input the signature misses means sites stop dropping until a reset. The spec's test surface must construct one case per census input asserting the signature moves.
- The hauler quota rides the same memo only because its input set is a subset of the Layout's; anything census-derived added later may join, anything reading creep state may not.
- The memo is the codebase's first cross-tick computed state; ADRs 0001/0002's "cross-tick path caching deferred" stands — floods and paths still rebuild per tick.
