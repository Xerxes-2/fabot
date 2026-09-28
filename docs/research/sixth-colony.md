# The sixth colony: W17S29, the first step into the free belt to the west

Date: 2026-09-28 (t807,5xx, `shardSeason`). Follows `fifth-colony.md`, whose case for W11S27 rested on W9S28 being the sixth room. That reason is gone (below).

**Verified against:** room-objects for W5–W29 × S20–S34 (ownership, reservations, sources, Thorium, lairs, cores), `map-stats` for the world's borders (S31+ is out of borders), the terrain endpoint for every seam in that rectangle with a BFS over open seams, `/api/auth/me` for GCL, two controller reads 56 ticks apart for rates, and `observe reactor/raids/outposts`. The Layout facts for W17S29, W12S27 and W14S28 are `third-colony.md`'s sweep over the committed captures and were not re-run.

## Live state

- **No Thorium anywhere.** Every Storage, terminal and the Reactor read 0 T. Last delivery t797,836; score 369,055, rank 11. Burning is held (`w15s25.Held`, 2026-09-28), so from here everything mined is banked.
- Deposits we can still dig: **W11S27's 22,000**, which needs RCL6. W11S29 is mined out (its container tail was drained by #421).
- **W11S27**: RCL5 at 684,473 / 1,215,000. The 56-tick sample read 22.9 e/tick; the average since the claim (t702,714) is 12.1. **RCL6 in ~23k–44k ticks, i.e. 1–2 days.**
- **GCL 5**, 38,555,482 points; GCL 6 is at 47,591,348, so 9.04M to go. The rate is 91 points/tick averaged since t701,692 and 103 in the sample. **GCL 6 in ~88k–99k ticks, 4.4–5 days.**
- **The server has slowed to 4.32 s/tick** (it was 2.87 on 2026-09-13). Every "days" figure here uses 4.32.
- All five Storages read ~0 energy: every colony spends everything it harvests on its own controller.

## W9S28 is gone

**Kalgen owns W9S28 (RCL5)** and reserves W9S27 and W9S26, the two rooms on our only chain to it and to W9S26's 45,000. Kalgen also holds W7S22 (RCL6) and reserves most of W5–W9 × S21–S29. Ague (W8S27, W7S28, W6S29) and nightred (W9S24) sit in the same corner. The west is closed. W11S27 is still worth its own 22,000; it is no longer a door to anything.

## Candidates

Every 22,000 room in the rectangle that nobody else owns or reserves, with hops over open seams from the nearest colony:

| room | T | src | nearest colony (hops) | to Reactor | note |
|---|---|---|---|---|---|
| **W14S28** | 22k @2,29 | 1 | W13S28 1, **W15S28 1** | 4 | our outpost (W15S28's); seams W13S28 21, W15S28 29, W14S29 44; trunk 13, the second shortest in `third-colony.md`'s sweep |
| **W12S27** | 22k @24,16 | 1 | W12S28 1 | 5 | our outpost (W12S28's); seams W12S28 36, W12S26 35; trunk 41; an invader raid there ended t729,787 |
| W12S26 | 22k @33,39 | 1 | W12S28 2 (via W12S27) | 4 | unreserved |
| W13S26 | 22k @45,15 | 1 | W12S28 3 | 3 | unreserved; its north and south seams are 0 |
| W12S25 | 22k @46,23 | 1 | W12S28 3 | 5 | one hostile seen; beside Odiodin's W11S25 |
| W13S25 | 22k @24,30 | **2** | W13S28 5 | **2** | only one open seam (W14S25, 22); the Reactor's doorstep, where SlothBot raided |
| W11S26 | 22k @11,6 | **2** | W12S28 5 | 7 | its W11S27 seam is 0; reached only through Odiodin's W11S25 |
| W11S31 / W11S32 | 22k | 1 / **2** | W11S29 2 / 3 | 10 / 11 | south across the S30 highway |
| W17S29 | 22k | **2** | W15S28 3 | 6 | Trepidimous holds W18S26 (RCL6), W17S24 |

The 45,000 rooms left are W17S22, W17S23 and W17S25: one source each, inside Trepidimous's reach, 6+ hops out. Our own 10,000 outposts (W11S28, W12S29, W13S29, W15S27) are listed for completeness and are not worth a GCL slot.

**First draft: W14S28, then W12S27.** Both are our outposts one hop from an RCL7 mother, so they are the cheapest claims. **Overturned by the user (2026-09-28): "兔子不吃窝边草".** The rooms next door stay outposts: they feed the mothers and cost nothing to hold. The slot goes to a room that opens new ground.

**Decision: W17S29** (22,000 @16,44, 2 sources, controller 30,39), mother W15S28, ore by terminal to W15S28.

The only free ground left is west. The world ends at S30, the north (S21–S24) is Odiodin's, and the east is Kalgen's and Ague's. The free part of the west is a belt, W16–W19 × S27–S29, with rivals around it: Trepidimous (W18S26 RCL6, W17S24 RCL4, W22S28 RCL6, reserving W18S27), Shibdib (W18S23, W21S23, the SlothBot that took the Reactor), giaco (W22S25–W25S27, up to RCL7) and Mirroar (W25S23 RCL7).

| S \ W | W19 | W18 | W17 | W16 |
|---|---|---|---|---|
| S26 | 22k 2s | **Trepidimous/6** | 3k, Trepidimous res | – 3s |
| S27 | 22k 1s | 10k, Trepidimous res | 10k 1s | 10k 1s |
| S28 | 22k 2s | 22k 1s | 10k 1s | 10k 1s |
| S29 | 22k 2s | 3k 2s | **22k 2s** | 3k 2s |

Why W17S29 is the belt's door:
- It is three crossings from W15S28 (W15S29 → W16S29 → W17S29; seams 41, 35). `fourth-colony.md` priced the walk: claim walk 106, haul 353/252.
- Its neighbours W16S29 and W18S29 have two sources each. Their Thorium is too small to matter, which makes them a colony's energy outposts, and so a fast RCL.
- It is the belt room farthest from Trepidimous (three rooms from W18S26).
- From it, W18S28, W19S28, W19S27 and W19S29 are 22,000 each, 2–3 hops out: 88,000 more T for the colonies after it.

Its costs, from `fourth-colony.md`'s sweep: the trunk is 70, the longest we have planned, and 2 of its 34 spawn tiles lose the trunk (48,18 and 48,42). The room seam to W17S28 is 5 tiles.

**W19S28** (the middle of the belt, 6 hops) was the other option. It was rejected because it is too far for a mother's pioneers and two rooms from Trepidimous. **W18S28** (1 source) was rejected because it is one room from Trepidimous.

## The constraint is controller energy

Every deposit needs its room at RCL6: **1,800,200 controller energy** from RCL1 (200 + 45k + 135k + 405k + 1.215M). A one-source room makes ~10 e/tick at most. On its own, a one-source room would take ~180k+ ticks, **9+ days**. W11S27 has two sources and still averaged 12 e/tick. At that rate a sixth colony claimed at GCL 6 (day ~4.5) digs around day 13–14. The season ends 2026-11-01, about 34 days away at today's tick rate.

Meanwhile the empire puts ~100 e/tick into controllers. Most of it goes into W12S28 and W13S28 (RCL7, 7.4M and 7.6M of 10.9M) and W15S28 (RCL7, 2.75M). **RCL8 buys no Thorium.** 1.8M at 100 e/tick is 18k ticks, about 21 hours. The same energy spent on a child's controller would bring a 22,000 room online roughly once a day, where RCL8 buys none.

The repo has part of this already: ADR 0052's **ferry** and borrowed Upgrade. A mother hauls into a child's upgrade buffer, but only while the child is `Bootstrapping`, and `Tuning.FerryLoads` is sized at one load. Pushing a child all the way to RCL6 on its mother's energy is a design change: a new stage or a wider ferry, plus a way for a mother to stop upgrading her own controller. It deserves its own ticket and probably an ADR. It is the largest lever this note found.

## The GCL slot

The sixth claim waits for GCL 6, which is 4.4–5 days away. The alternative is to **unclaim W11S29 now**: it is mined out and has one source plus the W12S29 outpost. That frees the slot today and puts the sixth room about 4.5 days earlier. The cost: W11S29's RCL6 structures (terminal, spawn, 40 extensions) and its ~20 e/tick of controller energy, which slows GCL 6 by roughly a fifth. The same trade repeats every time a colony is mined out: a spent room is worth a slot and nothing else.

**Decided (user, 2026-09-28): W11S29 is given up** for this slot.

## Unverified

- Rates are from one 56-tick sample plus long averages. Re-read W11S27's progress over a few thousand ticks before trusting "1–2 days".
- W17S29's Layout is from `fourth-colony.md`'s sweep (2026-09-16), not re-run against today's code; its capture is now committed (with W16S28, W16S29, W17S28 for the chain).
- Trepidimous's intent is unknown. Its only trace in our raid log is two 1-MOVE scouts.
- Whether a mother's borrowed Upgrade can legally stay on a child's controller past `Bootstrapping` is read off ADR 0052's text, not off the code.
