# A Post's Seat is the garrison's

Live observation of W13S28 (RCL1–2, 2026-09-06, t~170,1xx, #212): both of the room's Posts were stood on by `1W/2C/2M` workers digging two a tick, and the two `2W/1C/1M` [[anchor]]s cast for those Posts read `idle (none-free)` and stood in the swamp. Harvest's capacity was the source's [[seat]] count, shared by every harvester (ADR 0012), with ADR 0024's Post cap counting Work-heavy bodies alone; nothing reserved a Post's Seat for the body hired for it. A light body fills the Seat cap first — it is nearer, it is idle, and at 300 capacity there were twelve of them (#208) — and a light body's full store ends its dig (ADR 0024), so the tile freed every fifty ticks and the nearest idle light body took it again before the Anchor could. The Anchor row is hired one per Post and never reached one.

ADR 0020 considered and rejected "cap a posted source's Harvest at its Post count, so light workers never dig where an Anchor works", as a separate decision with its own failure mode at the RCL2–3 moment — a light crowd left with no rock. That moment is now live in a child colony, and the failure runs the other way: the light crowd is what keeps the Post empty. The home room never showed it because home workers withdraw from the [[storage]] and never take Harvest.

We decided **a Post's Seat is the garrison's**, in two halves that read one fact:

1. **A light body's Harvest [[work area]] is the source's Seats less its Posts** (`Atlas.narrowedArea`) — the complement of ADR 0020's heavy narrowing, so the two kinds of body stand on disjoint tiles of one source. A source with no Post keeps every Seat for the light body: ADR 0045's bare-Seat bootstrap is unchanged. A source whose every Seat is a Post hands a light body nothing, which is the rule's point: its energy at such a source is the container the Anchor fills (ADR 0012).
2. **Harvest admits a light body only into the Seats a source has beyond its Posts** (`Decide.hasCapacity`'s light cap, `seats − posts`, counting light holders), beside ADR 0024's Post cap over the heavy ones and the Seat cap over both. Read only where a Post cap exists, and read off part arithmetic, never a row name (ADR 0006). Memory is under the cap too, so a light body remembered on a Post's Seat is released rather than grandfathered — the shape ADR 0024's own incident had.

## Consequences

- A source with two Seats and one Post admits one light body and one heavy, never two light and an Anchor `none-free`.
- A light body on a posted source may have to walk further: the Seat it may use is whichever is not the Post, and where the bare Seat is unreachable from its side the dig is `Unreachable` and the body takes other work. That is the intended answer, not a gap.
- The tick a container site is placed a Post exists (#205) and the Anchor quota rises with it; the light crowd loses that Seat the same tick the body that will hold it is hired.
- ADR 0020's rejected option is overturned in this narrower form; ADR 0024's cap is joined by its complement. Both carry a banner.
