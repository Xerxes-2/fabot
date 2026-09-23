# The fifth colony: W11S27, because it is the door to W9S28

Date: 2026-09-24 (t701,692–702,125, `shardSeason`). Sister to `fourth-colony.md`, whose §8 ranked W11S27 fifth; this note re-reads the live facts and adds the reason that decides it.

**Verified against:** the room-objects endpoint for W8–W18 × S24–S31 (ownership, reservations, sources, minerals, cores), the terrain endpoint for the border rings below, the console for GCL / RCL / stores, `observe quotas` and `observe cpu`, and the repo's own chain test (`RoomOutpostTests` "a chain of real border rings joins every declared outpost to its home") on fresh captures of W11S27 and W13S27.

## Live state

- GCL 5 at t701,692 (1.09M of 19.73M toward 6). About 77 GCL points a tick since t554k, so GCL 6 lands around **t944k**, with the season ending near t1.87M. A sixth room is reachable this season.
- The four colonies: W12S28, W13S28 and W15S28 at RCL7; W11S29 at RCL6 with a terminal and 17,280 T left in the ground. Every other deposit we own is gone.
- CPU: 45–90 ms a tick with a mean around 58, against a limit of 100. The bucket is at 10,000.

## Why W11S27

W11S27 has 2 sources, 22,000 T at (47,12), 35,000 Z, and is unowned and unreserved. Its own deposit is not the reason. The reason is **W9S28**: 2 sources and **45,000 T**, as rich as W11S29 and with a second source, which `fourth-colony.md` §3 wrote off as unreachable. Counted by hand (both ring tiles non-wall):

| seam | open tiles |
|---|---|
| W11S28 \| W10S28 | 0 |
| W10S28 \| W9S28 | 0 |
| W10S29 \| W9S29 | 0 |
| W9S30 \| W9S29 | 0 |
| W11S27 \| W10S27 | 18 |
| W10S27 \| W9S27 | 29 |
| W9S27 \| W9S28 | 23 |

The W10 column is sealed from S28 to S29 and W9S29 is sealed on its west and south. So every chain from the four homes is five crossings, and the owned W11S29 does not help. From W11S27 the chain is W10S27 → W9S27 → W9S28, **three**. The fifth room is what makes the sixth one worth 45,000.

Rejected:
- **W12S27 and W14S28** (22,000 each, our reservations today, one hop): the cheapest claims, but they unlock nothing, and they take a source of income from W12S28 or W15S28, whose Storages read 0 energy.
- **W13S26**: one source, 22,000, three hops.
- **W9S26** (45,000, one source): reached only through W9S27, with nightred's reservation in W9S25 beside it.

## The mother is W13S28

W13S28 is three crossings away (W12S28, W11S28) where W12S28 is two, and `fourth-colony.md` §5 prices the claim walk at 151 against 100. The mother is still W13S28, on the fourth colony's argument that a mother lends stock and not distance. That argument is weaker today: W13S28 holds **86,599**, down from 625,402 when it raised W11S29 and 849,766 on 2026-09-20. W12S28's Storage reads 0, with every unit going into its own RCL8. This overturns §8's "W12S28 / 2" on the same grounds the fourth colony's choice did. §8 also called the entrance a two-tile slot; the capture says the W11S27|W11S28 seam is 5 tiles, and the 2 is W12S28|W11S28's.

## Costs and follow-ups

- **The Claim window** adds five rooms to W13S28's scan set: W11S27 and the rectangle W13S27 / W12S27 / W12S28 / W11S28. It also adds W11S27's two rocks, three crossings out, to W13S28's haul while the room is an outpost. That cost is not priced here. The W14S28 precedent (2,790 → 4,810) says it can double the demand, so the bound is the window's length and not the arithmetic. W11S27 comes **out of W13S28's `Outposts` the day the claim lands** (#404, as #352 was for W11S29). Nothing does this automatically.
- **CPU:** the claim goes ahead only while the bucket holds; before the declaration the mean was ~58 ms against a limit of 100. The `profile` scenarios are unmoved (`pair` 5.52 ms), since none of them stands the rectangle's rooms as work.
- **Risks:** Ague's W8S27 (RCL3) borders W9S27, on the road to W9S28. Nightred (W9S24, RCL6) reserves W9S25 and W8S24/W8S25. No invader core stands in W11S27 or next to it.
- **Unverified:** the W9S28 chain is counted with ring tiles only, not with ADR 0062's ground-behind-the-landing test. W10S27, W9S27 and W9S28 are not captured, so the repo's chain test has not seen them. Capture them before W9S28 is declared.
