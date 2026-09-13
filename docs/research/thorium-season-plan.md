# Season #11 scoring: the clock is not the constraint, the ore is

Date: 2026-09-13, live tick 396,515–396,757 on `shardSeason`. Supersedes the situation half of
`thorium-reactor.md` (2026-09-07, tick 202,041) — its **rules** are still the authority and were
re-verified today against `mod-season5` (`da59118`); its **numbers about us, the board and the
sector** are all stale, and §6 below lists every one.

## Summary

- **Time is not the binding constraint, and it is not close.** The season ends 2026-11-01; the
  measured tick rate is 2.87 s/tick, so ~**1,474,000 ticks** remain. Our whole reachable stock is
  66,000 Thorium = 66,000 reactor-ticks = **2.2 real days of reactor work**. We have 22× more time
  than ore. The rate would have to fall to 64 s/tick for the clock to bind.
- **So ore acquisition dominates streak discipline by an order of magnitude.** A break costs
  11,106. **One more 22,000-deposit room is worth 110,000** — ten breaks. ADR 0057's "a break is a
  5% tax, not a failure" was right and should be leaned on harder, not less.
- **Our sector reactor W15S25 (44,6) is now owned by `Odiodin`** (rank 3, 192,305), store empty,
  `launchTime` null. Any Thorium delivered there today scores for him. **The first act of any plan
  is `claimReactor`** — one `[CLAIM, MOVE]`, adjacent for one tick, no cooldown, no precondition.
- **A steal does not break our streak.** `claimReactor` does `bulk.update(target, {user})` and
  nothing else; only an empty store clears `launchTime`. Theft is a 5/tick income leak, not an
  11,106 penalty. Re-verified in `src/creep.claimReactor.js` today.
- **Nobody on the map is running a streak.** Of 100 sector centres, 20 are owned and **exactly one
  is running** (E5N5, manbantoo, ~34 rooms away). Every other owned reactor reads `T: 0`,
  `launchTime: null`. The whole board burns in bursts and pays the restart toll every time.
- **Three owned rooms, three untouched 22,000 deposits, zero extractors.** W12S28 and W13S28 are
  RCL6 and have had the extractor unlocked for days. W15S28 is RCL5 and holds the deposit nearest
  the reactor. **Stock is 66,000, not the doc's 44,000.**
- **66,000 unbroken scores 318,894** against today's leader at 293,347. 44,000 (the two rooms that
  can mine *today*) scores 208,894 — rank 3 as the board stands.
- **Nothing of ADR 0057 exists in the tree.** `grep -ri "thorium|mineral|extractor|reactor"` over
  `src/` returns zero hits. The gate its tickets waited on — W12S28 at RCL6 — opened around
  2026-09-10 and nobody re-queued them.

## 1. The clock

Season #11 runs 2026-09-01 18:00 UTC → 2026-11-01 ([Steam news for Screeps: World](https://steamcommunity.com/app/464350/allnews/),
2026-08-27: *"This season will last for 2 months, till November 1"*). **Unverified:** the hour of the
Nov 1 shutdown; 00:00 UTC is assumed, and a later hour only adds slack.

Tick rate measured twice today, independently of the server's own figure: 123 ticks / 353 s and 209
ticks / 601 s, i.e. **0.348 ticks/s (2.87 s/tick)**. The season-to-date average is 0.407, so the
server is *slowing*. `/api/game/shards/info` reports a 2,852.6 ms rolling mean, agreeing.

| assumed rate | ticks remaining | season-end tick |
|---|---|---|
| 0.407 (season average) | 1,720,000 | ~2,117,000 |
| **0.348 (measured now)** | **1,474,000** | **~1,871,000** |
| 0.30 | 1,269,000 | ~1,666,000 |
| 0.25 | 1,058,000 | ~1,454,000 |

66,000 reactor-ticks is **2.19 real days** at the measured rate. The deadline for the *first*
delivery, so that 66,000 can still be spent, is around tick 1,805,000 (~2026-10-29). Nothing about
this plan is time-pressed.

## 2. What a run is worth

`score = 1 + floor(log10(1 + gameTime − launchTime))` per Thorium consumed, 1 T/tick, `launchTime`
nulled on any tick the store is empty (`reactor.roomObject.js:86`, re-read today). For
`9,999 < N ≤ 99,999`, the total is `38,889 + 5(N − 9,999)`.

| stock | 0 breaks | 1 | 2 | 3 |
|---|---|---|---|---|
| 10,000 | 38,894 | — | — | — |
| 20,000 | 88,894 | 77,788 | — | — |
| **44,000** (the two RCL6 rooms, today) | **208,894** | 197,788 | 186,682 | 175,576 |
| **66,000** (all three, W15S28 at RCL6) | **318,894** | 307,788 | 296,682 | 285,576 |
| 88,000 (a fourth room) | 428,894 | 417,788 | 406,682 | 395,576 |
| 110,000 | **548,895** — crosses 99,999 into the 6/tick band | | | |

A break costs exactly **11,106** wherever it falls (both segments stay past 9,999).

**Do not run two reactors.** Score is per reactor per tick and nothing limits a player to one
(olek_PL owns four), but splitting 66,000 across two gives 307,788 against 318,894 — parallelism
costs exactly one break per extra reactor, because the log bonus is superlinear. One reactor, one
streak, unless the clock binds. It doesn't.

## 3. The board

Live at tick 396,671: 75 registered, **15 scoring**.

| rank | player | score | | rank | player | score |
|---|---|---|---|---|---|---|
| 1 | CrAzYDubC | **293,347** | | 6 | giaco | 82,305 |
| 2 | MeowKittyWow | 221,069 | | 7 | Nidoran | 82,295 |
| 3 | Odiodin | 192,305 | | 8 | manbantoo | 74,614 |
| 4 | Karmo | 135,162 | | … | | |
| 5 | zkl2333 | 133,432 | | 38 | **Xerxes_2** | **none** |

Ranks 2–5 map cleanly onto "two own deposits, burned in several runs at 4–5/tick": MeowKittyWow's
221,069 ≈ 44,200 T, Odiodin's 192,305 ≈ 38,500 T. Their RCL6 rooms read `T: []` — the deposit object
is *deleted*, which `mineral.roomObject.js` does the tick a Thorium mineral hits zero — so they have
mined out and are done unless they find more.

CrAzYDubC does not fit that model: 293,347 needs ≥58,700 T, and their two rooms are E7N27 (deposit
gone) and E6N27 (3,000, density 1). **~13,000+ Thorium came from somewhere else** — strongholds or a
claim → RCL6 → mine out → abandon → re-claim cycle. *Unverified: I could not distinguish the two
from the API.*

**A structural disadvantage worth naming:** the announced northward density gradient is real and it
is against us. All three of our deposits are density 3 = 22,000; Odiodin's W12S23 holds 45,000
(density 4) and CrAzYDubC sits at N27. Northern players get roughly twice per room.

## 4. Where we actually stand

| | W12S28 | W13S28 | W15S28 |
|---|---|---|---|
| RCL | **6** | **6** | **5** (227k/1,215k, ~987k to go) |
| Storage energy | **0** | 469,537 | 168,885 |
| Terminal | none | none | none |
| Thorium | **22,000** at (26,5) | **22,000** at (42,30) | **22,000** at (29,12) |
| Extractor | **none** | **none** | none (needs RCL6) |

25 creeps across three colonies, all rows at quota. **W12S28's storage reading 0 is worth a look on
its own** — it held 258k on 2026-09-07, and while RCL6's upgrade spend explains it (progress
3,289,567/3,645,000), it has not been confirmed as normal.

Also ours but not RCL-capable: W13S29 (outpost, reserved, 10,000 T), W12S27 (22,000 T, Invader-
reserved until 401,524), **W14S28 — unowned, unreserved, 22,000 T**, sitting between W13S28 and
W15S28.

**The walk, re-measured by Dijkstra over live terrain:** from W15S28's Thorium seat (29,12) to a
tile adjacent to the reactor is **154 steps, three room transitions**, and the 5-tile keeper margin
through the SK room W15S26 costs **zero extra steps** (the route hugs the east edge, 8+ tiles clear
of the lair at 35,11). From W12S28 it is 224–267 steps and six transitions. All nine tiles around
the reactor are plain.

**New hazard the old doc does not mention:** a **level-4 Invader stronghold in W14S26** (core at
(9,33), 4 towers, 25 ramparts), two rooms from W15S28 and bordering both routes without blocking
either. The W12-column route rooms and W15S27 now carry level-0 Invader cores and Invader
reservations; the whole wave expires at tick 409,455.

## 5. The plan, ranked by score per effort

1. **Claim W15S25 and never let the store run dry.** One `[CLAIM, MOVE]`, 650 energy. Then keep it:
   a resident re-claimer calling `claimReactor` whenever `!reactor.my` costs ~2 e/tick, and since
   all nine adjacent tiles are plain and no rampart is possible (the room has no controller), a
   nine-body block is the only *hard* denial. Odiodin's W14S22 is three rooms away, so he is in
   claimer range permanently — but a steal is an income leak, not a streak break.
2. **Get the two RCL6 extractors mining now.** Both rooms have had the kind unlocked for days and
   neither has built one. This is 44,000 of the 66,000 and needs no new routing.
3. **Push W15S28 to RCL6** (~987,000 control points, ~20,000 ticks, ~16 real hours at 50 e/tick).
   It unlocks the third extractor *and* shortens the walk from 224 steps to 154. The only item on
   the critical path, and it is not tight.
4. **Then buy ore, not streak discipline.** **W14S28 is unowned, unreserved and holds 22,000** —
   +110,000 score, ten breaks' worth. A fourth owned room needs GCL 13,966,610 against our
   9,836,911, i.e. +4.13M. Crossing **110,000 T total** pushes the streak past 99,999 ticks into the
   **6/tick band** (548,895) — the one threshold that would make the clock worth thinking about
   again, and even that is 3.65 real days of reactor work.
5. **Stretch only: strongholds.** The only renewable Thorium and the highest-variance source — a
   level-4 core can yield 0–133,000 T (`coreAmounts[4] = 400,000`, T drawn at the lowest density
   divisor, so the largest unit yield). Needs RCL7–8 boosted military. ~19 more stronghold
   generations will spawn before Nov 1, so there is genuinely time — but it must not displace 1–4.
   *Correction to `thorium-reactor.md` §1.2: the two `RESOURCE_THORIUM` entries collapse to one key
   under `_.object`, so T is not double-weighted; it is one of 3–5 keys at density 3.*
6. **Do not** run parallel reactors (−11,106 each), and **do not** plan offensive reactor theft: the
   store caps at 1,000, so one uncontested steal nets ≤5,000, and nothing reachable is running.

## 6. Every number `thorium-reactor.md` now gets wrong

All were true at its tick 202,041.

1. "W12S28 is RCL5 … W13S28 owned, RCL4" — both are **RCL6**.
2. "the stock is 44,000" — **66,000**; we own a third RCL-capable room the doc predates. The doc's
   title ("what 44,000 Thorium is worth") is stale.
3. "W12S28 … 258k energy in storage" — W12S28's storage is **0**; the energy is in W13S28 (469,537)
   and W15S28 (168,885).
4. "we are at GCL 2" — GCL is **9,836,911** and we own three rooms. The terminal argument is no
   longer gated on GCL but on building a terminal, and we have none in any room.
5. "W15S25 … unowned and empty ← ours" — **owned by Odiodin**. W5S25, W15S15, W25S15 and W25S5 have
   also been claimed since.
6. The board — superseded entirely. The doc's leader (giaco, 74,768) is now rank 6; the leader is
   CrAzYDubC at **293,347**. 8 scored of 61 → **15 of 75**. We moved 43 → 38, still zero.
7. "W16S25 additionally holds a level-2 invader stronghold" — **gone**. The new hazard is the
   level-4 stronghold in **W14S26**.
8. The six-transition route from W12S28 is no longer the best walk: **W15S28 → W15S27 → W15S26 →
   W15S25 is three transitions and 154 steps.**
9. Outside the doc: `scripts/observe.mjs`'s `SECTOR` constant still says the sector's invasion
   switch is off unless another stronghold has spawned. **One has** — W14S26.

## 7. What blocks the code, and what a human has to decide

`docs/adr/0057-*.md` was cut into #260–#265 and none is implemented. Against today's tree:

- **#260 (facts + layout), #261 (the miner row), #262 (the Thorium haul leg) all still hold**, and
  #260 grew a third room. #262 is the largest mechanical diff in the queue (220 `Withdraw` + 432
  `Refill` sites) and collides with the open #295 split.
- **#263's premise is dead.** It existed to work around ADR 0041's one-hop Seam limit with a
  hand-written `Tuning.ReactorRoute`; ADR 0058 landed the derived chain (and quotes ADR 0057 saying
  this should delete that constant), and #288 made the chain chosen by price. **Close or rewrite.**
  What survives of it are two things the router does *not* give us:
  - **There is no vocabulary for a controller-less remote goal.** `Outpost.Controller` is a
    mandatory field and W15S25 has no controller, so it can never be declared, never projected, and
    `Atlas.routes` is never asked about it. This is the load-bearing open question of the whole
    programme, and it changes a type three ADRs depend on. **Human.**
  - **ADR 0057 decision 5's "neither flees … there is no Reach out there to be inside of" is false
    against today's code.** #286 deliberately keeps hostiles in a seen transit room, and W15S26 has
    four keeper lairs with live keepers. A courier mid-crossing will see them as Threats and ADR
    0033's Flee applies. **Human, and it wants an ADR line** — it overrides a decision made four
    days ago.
- **#264 (courier + reclaimer)** is buildable as written, but its economics moved: the reactor is
  held by a rank-3 rival, so the courier arrives and waits on day one, and the reclaimer is a
  standing claim war rather than cheap insurance.
- **#265's pre-finding is wrong today**: `GET https://screeps.com/season/api/scoreboard/list?limit=20`
  returns 200 with scores and ranks. The 504 was transient; the fallback should not be built.
- **Which colony runs the programme is a human decision.** ADR 0057 assumes W12S28, six hops out and
  outside `Tuning.MaxHops = 3`. Only W15S28 can price the walk under the chain model, and it was
  sited for exactly that (`third-colony.md` §4).

**The shortest path to a first point:** #260 → #261 → #262 puts mining and banking on autopilot in
all three rooms — about three issue-loops, no new routing, no keeper exposure. Delivery is then
hand-drivable, but note that **a hand-spawned courier is adopted by the bot**: `World.ofGame` reads
all of `Game.creeps` and files an unplaced creep under its caster's home, so it will be re-tasked
every tick (and, per the open #164, out-bid every priceable body because it prices at nothing).
Hand-driving therefore means per-tick console overrides, not a one-off nudge.

A single 999-unit delivery into a reactor we hold, cold from `launchTime: null`, is **2,889 score** —
enough to move us from rank 38 to about rank 15.
