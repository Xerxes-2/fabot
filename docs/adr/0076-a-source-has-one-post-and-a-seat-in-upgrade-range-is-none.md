# A source has one Post, and a Seat in upgrade range is none

> **Status:** accepted

> **Accepted 2026-09-24** on #405; the user chose to remove the Dual Seat outright ("完全删掉dual seat机制") and to take the farthest container as the Post.

W11S27 was claimed with its source at (13,28) one tile from the controller at (14,29). All five walkable Seats of that rock lie inside the controller's Upgrade [[work area]], so every one was a Dual Seat and every one a [[post]]: the colony hired six [[anchor]]s for two sources, five of them splitting one rock's 10 a tick. They blocked each other's tiles and switched between Harvest, Upgrade and idle. The Layout's controller [[container]] also landed on one of those Seats, at (14,28), so `standingPostsIn` counted it as a second source-container Post and the income base counted the same rock twice. ADR 0021 had written "today's colony has no Dual Seat". The other four colonies read exactly one Post per source.

We decided on two changes:

1. **The Dual Seat is gone.** A Post is a Seat under a source's container or its construction site, in the home room as in any other. The Layout plans a source container from level 0 and a site is a Post already (#205). Before the first site exists, the home room's bare-Seat fallback (ADR 0045) carries the bootstrap. The empty-window reprieve no longer excepts a body standing in upgrade range: a Work-heavy body beside its rock holds Harvest through the window on every Seat (ADR 0048's range test, without its Dual Seat clause).
2. **A source has at most one Post.** Of the source's Seats that carry a container or its site, the Post is the one farthest from the room's controller. At equal range a built container beats a site, and after that the first tile wins. The Layout seats a source's container at its [[trunk]] and the controller's beside the controller, so "farthest" names the source's own. A built container in the Upgrade area that is not a Post is the controller's [[buffer]] (`Atlas.controllerContainers`), even when it stands on a Seat.

## Considered Options

- **Cap the Dual Seats at one per rock and keep them.** Rejected. Every home source already gets a container Post from level 0, so a Dual Seat adds nothing but the in-place Upgrade during the empty window. ADR 0021 measured that at about 3.3 energy a tick for a one-Carry body, and this Upgrade was what split one rock across five bodies.
- **Keep two containers on one rock as two Posts.** Rejected. A rock regenerates 10 a tick whatever stands beside it. A second Post doubles its income in the quotas and hires a second 700-energy body to share the same dig.
- **Move the controller container off the Seats in the Layout.** Not done here. W11S27's container is already built, and the census has to read the room that exists. The Layout may still earn this change later.

## Consequences

- A Work-heavy body beside its rock no longer leaves Harvest for an in-place Upgrade through the source's empty window. Its dig goes into the container under it and reaches the controller through the buffer. That throughput is the cost of this decision.
- The hauler quota and the Refill pool read "a source container" as a container on a Post (`Atlas.sourceOfPost`), not as any container within range 1 of a rock. A buffer standing on a Seat is hauled to, not from.
- The Post cap's union of holders and standing bodies (ADR 0024 as #269 widened it) now meets at most one Post per rock. The two-Post rock that showed the difference no longer exists, and its tests went with it.
- ADR 0012, 0020, 0021, 0024, 0025 and 0048 are amended where they name the Dual Seat. None of their other rules moves.
