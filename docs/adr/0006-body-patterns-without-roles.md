# Body patterns are colony facts; creeps still have no role

> **Revised by ADR 0021**: "every part slot the remaining energy affords on Work" gains a ceiling — six Work, source saturation plus one spare — and a spawn casts a body as soon as its bank holds that body's cost rather than waiting for a full bank. Row shape, quota and the no-role axiom stand unchanged.

> **Revised by ADR 0016**: "gates are body-blind" narrows to "gates read part arithmetic" — Withdraw is inapplicable to a body with more Work than Move, the first comparative applicability gate. The no-role axiom and everything else stand unchanged.

One body shape served every task: `workerBodyFor` replicated the worker unit and nothing else. But a Dual Seat — a Seat that also sits inside the controller's Upgrade Work Area — rewards a body the generalist can never be: many Work, one Carry, barely any Move, harvesting and upgrading in place without a single step. The community survey (docs/research/body-design-task-matching.md) found every mature bot casts per-job bodies, and all but one binds the creep to a role at birth — the road this codebase has explicitly closed (CONTEXT.md, avoided terms). We decided to specialise the **body** and leave the creep free: spawning casts from a **pattern table** — the worker unit (generalist) and the **Anchor** (heavy-WORK) — sized by colony facts (Anchor quota = Dual Seat count, embedded inside the unchanged workforce target, Anchors filled first), while matching stays task-based and merely becomes **body-aware**: part-based applicability (no Work → Harvest/Build/Upgrade inapplicable; no Carry → Refill inapplicable) and travel cost restated as *ticks for the moving body* — terrain weights scaled by the body's fatigue factor, revising ADR 0002's terrain-only semantics. The Anchor is pinned to its Dual Seat by arithmetic, not decree: everywhere else its travel cost is enormous; where it stands its output is maximal. Fatigue parity (ADR 0003) narrows to the worker-unit pattern; the Anchor is exempt — its single Move is the one-time price of walking to the seat, precedented by community static-miner builds down to `5W1M`.

## Considered Options

- **Bind the creep to a post at spawn (role-based)** — rejected: contradicts the no-role axiom, demands persistent post state, and fights the per-tick-rebuild architecture on every front. The mainstream road, and the one we refuse.
- **Stay body-first with generalists only (status quo)** — rejected: the generalist is a permanent compromise; a Dual Seat worked by worker units wastes the one geometry where specialisation is free (zero movement, so the classic objection — clashing movement patterns — does not apply).
- **Capability scoring in the Matcher (Work count in the score)** — rejected: redundant once travel cost is body-aware; the one no-role precedent (Winsley) matches on distance plus active parts alone, and a second scoring axis is a mechanism without a proven need.
- **A compound Harvest-Upgrade Task** — rejected: alternation is already emergent — a full Anchor loses Harvest applicability and rematches to Upgrade, an empty one reverses — at the cost of zero new concepts; a compound Task would be the first combinatorial precedent in the pool.

## Consequences

- The Anchor's harvest↔upgrade alternation is emergent, not scripted: applicability release plus rematch flips the assignment while the creep never moves. The chat bubble alternates ⛏/⚡ — an accepted observability quirk, and the test surface for pinning the behaviour in DecideTests.
- ADR 0002's tie-break now measures real time: slow bodies stop pretending to be near, which is exactly what keeps an Anchor home and generalists on the road.
- The pattern table is the growth seam: a future hauler is one row — but every row must arrive with its own quota rule (a colony fact), never just a shape.
- A dead Anchor is refilled through the ordinary workforce gap; pre-spawning a successor before death is explicitly deferred, not designed.
- Fatigue parity's scope shrinks: a reader meeting a one-Move Anchor must find its exemption here rather than conclude the invariant is broken.
