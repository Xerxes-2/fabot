# Reroute attribution is computed only for verbose creeps

Profiling (#50) showed the rerouted Verdict costs 13% of the tick: attributing a detour to the occupancy surcharge takes a second, traffic-blind Dijkstra flood per assigned creep per tick (ADR 0008's two-flood comparison), it is the only unmemoisable flood — each creep's tile is a unique key, so ADR 0004's memo cannot help — and the label serves observability alone: no decision reads it. We decided **the reroute comparison runs only for creeps on the verbose list**: everyone else's Transition log simply carries no rerouted entries, exactly as ADR 0009 already prices full candidate scoring — attribution that needs real work is pay-per-use, flipped on from the terminal mid-investigation without a redeploy, while conclusion-level Verdicts that fall out of work already done (matched, kept, released, grounded, yielded) stay always-on. ADR 0009's contract narrows from "every Verdict always-on, scoring verbose-only" to "a Verdict whose evidence must be *manufactured* is verbose-only"; the two-flood attribution mechanism of ADR 0008 is untouched — it just runs on demand.

## Considered Options

- **Keep it always-on** — rejected: 13% of the tick, growing linearly with creep count, for a log label that investigations rarely start from.
- **Drop the rerouted Verdict entirely** — rejected: the attribution answers "why did this creep sidestep?" better than anything else in the log, and the verbose list already exists as the exact instrument for paying that cost only while asking.
- **A cheaper always-on approximation** (flag a reroute when the chosen step lands beside occupied tiles, or when the priced path paid any surcharge) — rejected: both misattribute — a surcharge paid on an unchosen branch or a coincidental adjacency is not a detour — and a Verdict that can lie is worse than one that is absent (ADR 0009: reasons are data).

## Consequences

- A quiet colony's Transition logs stop recording reroutes; an investigation into movement starts by putting the creep on the verbose list, as it already does for scoring.
- `firstStepIgnoringTraffic` becomes verbose-only machinery; the Resolver's arbitration and every other movement Verdict are unchanged.
- The Verdict and verbose-list glossary entries gain the reroute clause.
