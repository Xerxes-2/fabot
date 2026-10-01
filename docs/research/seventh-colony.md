# The seventh and eighth colonies: W17S25's Ultra, and the W12S26 cluster

Date: 2026-10-01 (t877,866–878,137, `shardSeason`). Follows `sixth-colony.md` (2026-09-28), whose survey this re-runs on live data. The question: W11S27 is mined out and will be given up, and GCL 6 is about a day away, so there are up to two slots. Which unclaimed Thorium rooms are the best claims, and is any **Ultra** deposit claimable besides the **High** ones?

**Verified against:** room-objects and the terrain endpoint for W3–W30 × S18–S30 (364 rooms; Thorium `density`/`mineralAmount`/`mineralType`, sources, controllers and reservations, invader cores, towers, spawns, keeper lairs, creeps by owner), `map-stats` for the world's borders (all of S31+ and all of W31 are `out of borders`), a BFS over seams that are open by terrain (foreign-owned rooms impassable, keeper rooms counted), `/api/auth/me` for GCL, and controller progress for our six rooms sampled at t877,866, t877,993, t878,012 and t878,137. The scan scripts lived in the session scratchpad and are not committed.

## Density, checked off the live objects

The levels are Screeps' `DENSITY_LOW/MODERATE/HIGH/ULTRA` = 1/2/3/4, and Season #11 has them one step below Season #5 (`thorium-reactor.md`). Every full deposit in the scan reads exactly:

| density | name | `mineralAmount` | Thorium rooms in W5–W29 × S20–S30 |
|---|---|---|---|
| 4 | **Ultra** | **45,000** | 6 (4 full) |
| 3 | **High** | **22,000** | 72 (70 full) |
| 2 | Moderate | 10,000 | 63 |
| 1 | Low | 3,000 | 23 |

## Live state

- **GCL 5**, 44,936,452 at t878,012; GCL 6 is at 47,591,348, **2.65M to go**. The rate since the sixth-colony note is 90.5 points/tick (t807,5xx → t877,866); a 144-tick sample (t877,993 → t878,137) read 76.8. **GCL 6 in ~29k–34k ticks, 1.3–1.5 days.** Unclaiming W11S27 first takes its ~12 e/tick out of that rate (about an eighth).
- **The server runs at 3.84 s/tick** (averaged over the last six days; 3.77 in the 144-tick sample; it was 4.32 on 2026-09-28). The season ends 2026-11-01, **~31 days, ~690k ticks** away.
- **W11S27 is RCL6 and mined out** (no Thorium object left; W15S28 holds its 21,885 T). W12S28, W13S28 and W15S28's own deposits are gone too.
- **W17S29**: RCL5, 516,967 / 1,215,000. 697k controller energy since its claim at t808,328 is **10.0 e/tick** on average, but the 144-tick sample read **25.3** (its four sources at RCL5). RCL6 in ~28k–69k ticks, 1.2–3 days.
- Mothers: W12S28 and W13S28 are RCL7 at 8.92M and 9.19M of 10.9M; W15S28 RCL7 at 4.25M. Storages: W12S28 45k, W13S28 105k, W15S28 100k energy.
- **A mother still raises a child only to RCL3** (`Tuning.BootstrapLevel = 3`, `FerryLoads = 1`, `PioneerCount = 3`). The wider ferry `sixth-colony.md` called the largest lever has not been built, so from RCL3 to RCL6 (1.62M of the 1.80M) a child runs on its own sources.

## What changed since 2026-09-28

- **giaco claimed W19S29** (RCL3, one tower), two rooms west of W17S29 and directly south of W19S28. The belt's west half is no longer free.
- **Odiodin claimed W17S22** (RCL2, a spawn, garrison creeps from W14S22 of 20 MOVE / 16 RANGED / 4 HEAL), on its 45,000, and **is remote-mining W17S23** (a container site at 18,5, a remote miner, two more garrison creeps). W11S25 is now reserved, no longer owned.
- **Kalgen's "Thorium research stations"**: W9S28 and W8S23 are back to reserved-only with spawn, towers, storage and terminal left standing, and **18 T left in each 45,000 deposit**. Kalgen claims an Ultra, strips it, unclaims and keeps the room reserved. W7S22 (High) is down to 22 T the same way, and Kalgen's W6S22 to 7,384.
- **W17S25 is unreserved.** Trepidimous was remote-mining it; our harassment (`Colony.harass`, 2026-09-29) drove that off. Our ranger stands on its source.

## Every Ultra deposit, and why most are out

| room | T | src | state | claimable? |
|---|---|---|---|---|
| **W17S25** | **45,000 @46,25** | 1 | free, unreserved; ctrl 15,36 | **Yes.** Trepidimous's W17S24 (RCL4, 1 tower) is north, behind a wall seam; W18S26 (RCL6, 2 towers, 73 ramparts) is diagonal |
| W17S23 | 45,000 @17,30 | 1 | free, but Odiodin's remote (container site, miner, 2 garrisons) | **In name only.** Between Odiodin's W17S22 (north, seam 23) and Trepidimous's W17S24 (south, seam 17); 7 hops through 3 keeper rooms, 16+ around them |
| W17S22 | 45,000 @44,19 | 1 | **Odiodin RCL2** | No: owned |
| W9S26 | 45,000 @47,2 | 1 | **Kalgen RCL5**, 2 towers | No: owned |
| W9S28 | 18 | 2 | Kalgen reserved, Kalgen's spawn/towers/terminal | No: dug out |
| W8S23 | 18 | 2 | Kalgen reserved, same | No: dug out |

**One Ultra is claimable: W17S25.** It is worth two High rooms for one GCL slot.

## Candidates

Every High or Ultra room that nobody owns or reserves, plus our own two outposts, within ~6 hops. Hops are over open seams from the nearest colony (W11S27 excluded); "k" counts keeper rooms on that route. Rivals are owned rooms within two rooms (Chebyshev).

| # | room | T | src | nearest colony | to Reactor | keepers on route | rivals within 2 | claim → RCL6 dig |
|---|---|---|---|---|---|---|---|---|
| 1 | **W17S25** | **45k @46,25** | 1 | W15S28 5 (W16S26 K; seams 20,24,15,8,30) or W17S29 8 (0 K, via Trepidimous-reserved W19S26) | **2** | 1 | **Trepidimous W17S24 RCL4 t1, W18S26 RCL6 t2** | ~11.5–13 days |
| 2 | **W12S26** | 22k @33,39 | 1 | **W12S28 2** (seams 36,35) | 4 | 0 | none | ~10 days |
| 3 | W13S26 | 22k @45,15 | 1 | W12S28 3 (via W12S26); W15S28 4 via W14S26 K | 3 | 0 | none | ~11.5–13 days |
| 4 | W13S25 | 22k @24,30 | **2** | W13S28 / W15S28 5; its only seam is W14S25 (22) | **2** | **2** (W14S26, W14S25) | Odiodin W12S23 RCL7 t3 | ~7.3 days |
| 5 | W12S25 | 22k @46,23 | 1 | W12S28 3 | 5 | 0 | Odiodin W12S23 RCL7 t3 | ~11.5–13 days |
| 6 | W19S28 | 22k @31,11 | **2** | W17S29 3 (RCL5 mother) | 7 | 0 | **giaco W19S29 RCL3 (south, adjacent)**, Trepidimous W18S26 | ~7.3 days |
| 7 | W18S28 | 22k @35,20 | 1 | W17S29 2 | 8 | 0 | Trepidimous W18S26 RCL6, giaco W19S29 | ~10 days |
| 8 | W11S26 | 22k @11,6 | **2** | W12S28 5, through Odiodin-reserved W11S25 (seam 4) | 7 | 0 | nightred W9S24 RCL7 t3, Kalgen W9S26 RCL5 t2 | ~7.3 days |
| 9 | W19S27 | 22k @26,35 | 1 | W17S29 4 | 6 | 0 | Trepidimous W18S26 | ~10 days |
| 10 | W19S25 | 22k @34,38 | **2** | W17S29 6 | 4 | 0 | Trepidimous W18S26, W17S24; Shibdib W21S23 RCL6 | ~7.3 days |
| – | W12S27 | 22k | 1 | W12S28 1 | 5 | 0 | none | our outpost; ruled out 2026-09-28 ("兔子不吃窝边草") |
| – | W14S28 | 22k | 1 | W13S28 / W15S28 1 | 4 | 0 | none | same |

Further out, all free High rooms at 7+ hops: W16S22, W16S23, W18S22, W19S21 (Odiodin/Trepidimous country), W21S26, W22S26, W22S27, W23S28, W24S29 (giaco/Trepidimous W22S28), W5S28, W6S27, W7S25, W8S21, W9S22 (Kalgen/Ague/nightred), and W24S22–W29S28 beyond Mirroar. None is a reasonable reach from a GCL-slot claim with 31 days left. The 10,000 rooms are not worth a slot.

## The time to an RCL6 dig

The arithmetic is `sixth-colony.md`'s: **1,800,200 controller energy** from RCL1, with the mother's help ending at RCL3. Measured children: W11S27 (2 sources) put 2.01M into its controller in 175k ticks, **11.5 e/tick**; W17S29 (2 sources plus W16S29's 2) **10.0 e/tick** over 69.5k. A 1-source room with two 1-source outposts should land at 7–9 e/tick; 1-source alone, 5–6.

- 2 sources, ~11 e/tick: 164k ticks, **~7.3 days** after the claim.
- 1 source + 2 outposts, ~8 e/tick: 225k ticks, **~10 days**.
- 1 source, contested outposts, ~6–7 e/tick: 260k–300k ticks, **~11.5–13 days**.

These are lifetime averages and lean slow: W17S29 read 25 e/tick at RCL5 once its extensions and W16S29 were up. Read the estimates as upper bounds.

The claims land on day 0 (W11S27's slot, as soon as it is unclaimed) and day ~1.4 (GCL 6); add that to the table's "claim → RCL6 dig". Mining is then quick: a `[20 WORK, 4 MOVE]` miner digs 3.33 T/tick, so 22,000 takes 6,600 ticks (7 hours) and 45,000 takes 13,500 (14 hours). Every candidate above finishes well inside the season's ~31 days. Ore leaves by terminal (RCL6), so the land route matters for the claim, the pioneers and the defence, not the haul.

**The ferry is still the lever.** W12S26 is two open seams from W12S28 (45k in Storage, putting 12–40 e/tick into an RCL8 that buys no Thorium). Raising a child past RCL3 on the mother's energy would bring W12S26 in days sooner. W17S25 cannot use it cheaply: five crossings, one through a keeper room, with an 8-tile seam.

## Recommendation

**First: W17S25**, mother W15S28, in W11S27's slot now. It is the only claimable Ultra: 45,000 T, two High rooms' worth for one slot, and two rooms from the Reactor. Free outposts W17S26 and W18S25 give it three sources. The costs:
- **Trepidimous is next door.** W17S24 (RCL4) sits behind a wall seam; W18S26 (RCL6, 2 towers, 4 safe modes available) reaches W17S26 across a 41-tile seam. Trepidimous mined this room until we harassed it off, and will likely contest it. An `attackController` blocks upgrading for 1,000 ticks a time, and the 1-source room has a ~12-day climb in which to be stalled. This fits the user's standing intent to fight Trepidimous (siege target, 2026-09-29), but it commits the empire to holding the ground.
- Its supply route crosses W16S26 (keeper lair at 4,9, four tiles from the west exit at y 11–18). The bot already declares and masks that room for the harasser. The keeper-free route from W17S29 is 8 hops through Trepidimous's reservation.

**Second: W12S26**, mother W12S28, at GCL 6. It is safe: two hops from an RCL7 mother, no keeper room, no rival within two rooms. And it opens new ground: W13S26 and W12S25 (22,000 each, 1 source each) are its free outposts and the next claims, a 66,000 cluster off the Odiodin border. It is the room the wider ferry would help most.

If the user will not take on Trepidimous, the swap for W17S25 is **W13S25** (2 sources, 2 from the Reactor, ~7 days to dig) or **W13S26**. W19S28 lost its case when giaco claimed W19S29 under it.

## Unverified

- The child rates are lifetime averages of two 2-source rooms. The 1-source figures are extrapolated, not measured.
- Trepidimous's response to a claim, and whether a fresh claim carries a safe mode, are not checked.
- No Layout sweep was run for W17S25 or W12S26 (W17S25 and W13S26 have committed captures; W12S26 has none).
- Odiodin's W17S23 container site may become a claim; the W17S23 row will age fastest.

## Follow-up: W17S25 as the delivery hub (user, 2026-10-01)

W17S25 is two rooms from the Reactor (by W16S25) where W15S28 is three (by W15S27 and W15S26). Once W17S25 stands at RCL6 with a terminal:

- its own 45,000 T walks two rooms to the Reactor, no terminal hop;
- the Reactor errand (courier and re-claimer, `Errand.w15s25`) moves from W15S28 to W17S25, and every colony's `Consignee` moves from W15S28 to W17S25, so all banked ore arrives by terminal two rooms from the burn.

Both routes cross one keeper room (W16S25 against W15S26). The move waits for the terminal (~11–13 days after the claim, the same clock as the extractor) and for the room to hold against Trepidimous next door: #445 keeps W15S28 raising it until its first tower stands full.
