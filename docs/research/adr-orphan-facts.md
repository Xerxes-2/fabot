# Orphan facts from the ADR hygiene pass (2026-09-21)

Facts a comment or CONTEXT.md stated that no ADR contains. Each names the
ADR it belongs in; "no ADR" means a rule that exists only in code.

## src/Core/Decide (Quota, Bodies, Spawns, Entry)
- Quota.fs reservableOutposts: #333, a controller held by somebody else's CLAIM is excluded, read off the room's record not vision; W12S27 bought two reservers over 617 ticks at 1,950 a head against an invader-core reservation that outlives its core by 4,999 ticks -> ADR 0042
- Quota.fs worker backlog term: #364, W13S28 2026-09-17 terminal site 3,836/100,000 with 535,748 banked; sized in labour via Tuning.BuildTicksPerLife (a 16-Work body clears 120,000 nominal, live a tenth to a fifth); floored not ceiled; paid out of stock with sites charged first -> ADR 0046 or 0012
- Quota.fs surplusOverLifetime: #304, the miner row is charged beside reserver/anchor/hauler, scaled by Tuning.MineContactAgeing -> ADR 0057
- Quota.fs guardBlocksBeat worked example: lone smallMelee needs one block; with a smallHealer its 40 dmg kills our 1,000 hits in 25 ticks before our 30 net kills its 1,000 -> ADR 0056
- Quota.fs upgraderRow folded into one pass: the split measured +2.4% of a tick on the idle box -> ADR 0046 (#385)
- Entry.fs replan turn: #357, four colonies re-planning in one tick measured 487 ms of the 500 ms ceiling; a stale memo is served on a deferred turn -> ADR 0017
- Entry.fs: #248, rival construction-site tiles are signed for every projected room, wider than the one reader (over-invalidate is cheap) -> ADR 0044
- Entry.fs outpostFactsOf derived once: #383, claimTargets ran 11.4x and outpostControllers 9.1x a tick -> ADR 0042 or 0047
- Entry.fs: #370, the id-keyed signature walk was 4% of a pair --level 7 tick by inclusive samples -> ADR 0031

## CONTEXT.md
- Sighting: Tuning.VisionGrace keeps a held assignment through its room going dark and the mover walks the holder to the seam; a sighting is narrowed to rooms a colony works, not crosses (#151, #271) -> ADR 0004
- Reserve / Raid log / Stand-down: a controller held by another's CLAIM parts is refused like an owned one, read off the last look when blind; the raid log's third shape records held controllers with holder and expiry; the Invader's hold outlives the core by CONTROLLER_RESERVE_MAX 5,000 (#333) -> ADR 0043
- Guard / Raid log: a blind declared outpost is guarded at one body while Tuning.ThreatMemory (creep lifetime) stands, cleared or renewed by a look; the guard's work area falls back to the ring of declared source tiles; the 300-tick clock cost W15S28 four bodies in W15S29 (#366, #369) -> ADR 0056
- Container / Census signature: a rival's construction site is subtracted from the outpost container pick, carried as tiles only, signed with Rival in the kind slot (#248) -> ADR 0040 and 0044
- Pickup: piles stand before Withdraws in pool order, so a full tie falls to the decaying copy (MatchFactor.PoolOrder, #242) -> ADR 0010
- Pickup: ~630 T on W12S28's mine tile and ~300 on W13S28's decayed at t401,850 because FIND_DROPPED_RESOURCES was filtered to energy before #311 -> ADR 0057
- Harvest: a source's Harvest wants a Carry part (keeps the store-less miner off sources); a deposit's Work Area is the mineral Post with no bare-Seat fallback (#261) -> ADR 0057
- Workforce target / Miner: a miner's replacement is charged at Tuning.MineContactAgeing (3) bodies per worker life, beside the anchor's and hauler's one and the reserver's two and a half (#304) -> ADR 0057
- Lead: priced wherever the creep stands; an outpost creep's lead is the minimum over the seam band (#153) -> ADR 0026 / 0030
- Verdict: the resolver's `stalled` outcome (rested, settled off the tile it asked for, nobody nameable holding it) (#219) -> ADR 0008
- Body pattern: a row whose block holds a part outside Work/Carry/Move with no sizing rule fails at its first cast; patternTableTests sizes every row (#155) -> ADR 0006
- Whole line: live at t403,2xx, 116 W13S28 roads at median 50.0%; a container's holder runs dry near 0.74 and is released inapplicable; the 0.8 is open for re-derivation (#323) -> ADR 0061
- CPU line: ring capped at a hundred rows, written last after the Executor; pre-split rows carry no phases; local profile 10.45 ms/tick vs 49.4 ms live mean (#170) -> ADR 0041
- Breach log: the whole channel (#278): one row per live invariant violation with age, capped at twenty oldest-first-seen, absent leaf vs empty list; motivation: 63 fixture files vs 2 room captures, four Thorium incidents shipped green through 1,389 tests -> ADR 0035 or a NEW ADR
- Signature reflex: the whole rule (#381): Colony.signature written by any creep within range 1 of an unsigned projected controller; home controllers unreachable (upgraders work at range 3) and were signed by hand 2026-09-19; transiting and stood-down rooms excluded -> NO ADR EXISTS
- Reactor programme: a delivery is evidenced by a Thorium transfer intent or visible store growth from a prior real sample; a blind tick retains the sample; a malformed leaf resets whole; the CLI pairs the leaf with the scoreboard row (#320) -> ADR 0057 decision 6 / 0060
- Colony live roster and rooms with no chain (W9S28, W13S25, W11S26, W16S28, W17S28); W11S29 left its mother's outpost list (#352) after the overlap took W13S28's haul demand 2,780 -> 5,760 -> no ADR (docs/research/fourth-colony.md)
- Thorium live stock history (22,000 apiece at first read; two mined out by 2026-09-16; 36,484 T banked; 19,760 left in W15S28; W11S29 d4 45,000) -> no ADR (season research note)

## src/Core/Decide (Emitter, Layout, Planner, Resolver)
- Layout.fs planOutpostContainers is recomputed every tick off the memo; measured 4.84 ms/tick without the rule vs 5.35 with it (one flood per outpost room per tick) -> ADR 0042
- Emitter.fs guardTarget applies the swing gate ahead of the nearest-Post choice, narrowing ADR 0056 decision 2: otherwise a guard on a two-creep raid's ring is handed the Post-nearest invader three tiles off and swings at nothing -> ADR 0056 amendment
- Planner.fs: #383, before OutpostFacts the Controller census was walked ~19 times a tick on --scenario reactor --level 7 -> no ADR (profile fact, kept locally)

## src/App and src/Core/Observe.fs (all kept in code, compressed)
- Main.fs round-robin replan (#357): all-four replan tick 487 of 500 ms; a replanning tick averages 209 ms vs 84 mean; turn is Game.time % count -> ADR 0047 or a new CPU ADR
- Main.fs ReplanTurn is a DU because a bool last arg arrived as JS undefined and no layout was planned (#357) -> ADR 0047 / 0017 amendment
- Main.fs: projections were 7.63 ms of a 14.9 ms snapshot phase (#370) -> ADR 0041 amendment
- World.fs stable census memo (#384): TargetKinds is 6.5% of a profiled tick; checksum is sum+xor of id hashes so a same-tick destroy+build cannot collide -> ADR 0031/0032 amendment
- World.fs roomCosts: snapshot is 21% of the live tick and the harness cannot measure it; a one-rock outpost read 2.35 ms vs home 1.23 (#370) -> ADR 0041 amendment
- Observe.fs OutpostHold (#333): a hold withdraws only the reservation, ends by the engine's countdown, and standDown acts on it so a blind tick does not hire one more reserver -> ADR 0043 amendment
- Observe.fs ThreatLatch / Tuning.ThreatMemory (#366, #369): guard-row memory across blind ticks; the clock is a backstop -> ADR 0072
- Observe.fs CpuPhases: harness 10.45 ms/tick vs live 49.4 mean; harness lacks 0.2 CPU/intent, prelude, Memory parse (#170) -> ADR 0041
- Observe.fs coarse spans (#386): 100 x 200 = 20,000 ticks ~5 h; Max is the field because the 500 ms ceiling is a wall -> ADR 0041 amendment
- Observe.fs breach channel (#355/#361/#367/#377): exists because World.fs has no tests; lead time = cast (30 parts x 3) + 50 ticks/room floor, measured 159 vs floor 150 -> NO ADR; wants one
- Observe.fs raidDeadlines: an Invader in an unowned room never suicides (engine branch wants a controller owner) so the raid's own life is the clock -> ADR 0056 holds it; fine

## src/Core/Decide (Pool, Matcher, Facts, Threat) and Types (Tasks, Intents, Verdicts)
- Pool.fs:39 #235 live case: at t199,88x a worker with nine free walked a Seam for one dig on a rock a six-Work Anchor was draining (the spare-rate gate's founding incident) -> ADR 0025 or 0048
- Pool.fs:94 #258 live case: at 204,966 an Anchor a border away took the walk home onto a Post whose garrison outlived its arrival by hundreds of ticks (why hasUnmannedPost reads now, not at arrival) -> ADR 0048
- Pool.fs:585 #284 rescue budget: at t239,65x base roads sat at 50-58% while the trunk north stood at 2% and the outpost's roads at 8% -> ADR 0061
- Pool.fs:669 #266: spread over the whole pool, W13S29's 45 sites took one builder apiece; the colony-wide two was no cap -> ADR 0042
- Pool.fs:704 #367: the courier scored the delivery draw at rank 8 vs -2/0 for energy hauling, hauled energy 465 ticks, Reactor fell 500 -> 47; delivery draw is Feeding and capped at one body -> ADR 0067 or 0073
- Pool.fs:833 #306/#313: lifted at the cap the miner averages 1+p ~ 3.67 ageing a tick, at the cliff ~3.0 -> ADR 0057
- Pool.fs:979 #374: ceil(free / one hauler load) admitted one body for any ring under 1.5 loads, a fifty-energy worker as readily as the courier with 718 aboard -> ADR 0071
- Facts.fs:236 #354: the Reactor's room has no controller, so "a room we own" let 915 T bleed on its floor -> ADR 0060 or 0073
- Facts.fs:278 #362: at t501,501 a hauler landed 196 T behind a courier's 500 and stood on the Reactor's tile 152 ticks (why reactorTakesALoad counts ore afloat) -> ADR 0073
- Facts.fs:423 #361: the courier programme closed when the mine ran dry and the Reactor burned down with 7,226 T banked and a 7,989-tick streak (why a diggable mine is not a programme condition) -> ADR 0067 or 0073

## src/Core/Types
- Colonies.fs W13S28 declaration: the 2026-09-20 conversion-rate table (W13S28 5 rocks / ~50 e/t in / 15.7 e/t into controller / 849,766 banked vs W12S28 30.7 and W15S28 25.3) that moved W14S28 to W15S28 -> no ADR; the comment says it belongs in a ticket

## tests (small group)
- ObserveRaidTests.fs:64 Tuning.QuietGap = 50 is a judgement about poke-and-heal (an absence is a squad healing off-room, ~220 ticks of one raid in #66), not an engine number -> ADR 0052 decision 5
- ObserveCpuTests.fs:202 live shape when the per-colony split was built: four colonies, decide 15.3-56.3 ms, a 140 ms spike -> the CPU line has NO ADR (0041 was a mis-citation)
- SpatialFixtures.fs:100 standing the real World.ofGame against a fake Game is the profile harness's to do (#294, #308) -> plan pointer, no ADR home

## tests (quota and threat)
- ThreatTests.fs:1827 over W15S26's real terrain 24 of a keeper's walk-to-rock tiles reach ground we walk, and no larger margin crosses the room (#327) -> ADR 0060 (keeper mask)
- QuotaBodyTests.fs:~395 guard block order TOUGH, MOVE, ATTACK, HEAL because damage strips from the head; with ATTACK second a guard arrived disarmed (#282) -> ADR 0056
- QuotaGuardTests.fs:70 #366: W11S28's guard row fell to 0 when the room went dark; the row reads RaidState.Threatened while blind and hires one -> ADR 0056 / 0072
- QuotaUpgraderTests.fs:134 #385: 849,766 energy in W13S28's Storage unmoved 365 ticks, 14.1 e/t vs a stockless neighbour's 30.6; floor = UpgradeStockBodies x 1,800 = 36,000; stock may double the row and no more -> ADR 0046
- QuotaUpgraderTests.fs:~690 #216 R4: buffer at zero, container overflowing with 1,859 on the ground, seven mini workers walking fifty tiles; hauler row priced at the dearest reachable sink, not a mean -> ADR 0049 or 0012

## tests (atlas and seam)
- RoomSeamTests.fs:1140 the pre-#354 courier arithmetic read ReactorLoad - DeliveryInterval as a "buffer" and let 999 T leave home every 636 ticks against a 1 T/tick burn; supply is metered by the draw gate on the Reactor's store, not a cadence -> ADR 0073; ADR 0067 still states the 636-tick slot and nothing records that it was the defect

## tests (errand, matcher, room) — dropped from comments
- ErrandTests.fs ~873: live hauler-558190 drew with about two ticks over a 196-tick loaded leg and died in the Reactor's room with 500 T aboard; the incident behind Tuning.DeliveryLifeMargin -> ADR 0073
- ErrandTests.fs ~1791: the courier scored the Storage Thorium draw at rank 8 against energy hauling at -2/0, Reactor fell 500 -> 47 T with 35,376 T banked ten tiles away; cap hazard 34,876 T / 500 = 69 holders -> ADR 0073 / 0067
- ErrandTests.fs ~1111/~1157: 999 T every 636 ticks is 1.57 T/tick against a 1 T/tick burn, 915 T went to the floor; t501,501 two-carrier incident, 196 T stranded 152 ticks -> ADR 0073
- ErrandTests.fs ~1699: a courier and a hauler both matched the terminal's Thorium Refill while 19,848 T sat in the terminal (#363); consignment premise 36,484 T banked, terminal fee 100,000 emptied W12S28's storage (#349) -> NO ADR covers the consignment
- MatcherApplicabilityTests.fs ~100/~370: the vacancy-disjunct bug cost 2,200 energy of Work dribbling into a source container while the deposit went undug (#261); the miner's dig bleeds 3.33 T/tick against a 2,000 container -> ADR 0057 decision 2
- MatcherVerdictTests.fs ~1060: W12S28 with an empty Storage banked 3,600 T where W13S28 with 485,916 energy banked none (#306) -> ADR 0057 decision 3

## tests (layout)
- LayoutMemoTests.fs ~1000: `reactor --census-every 1` ran 109,258 heap pops a tick against 9,554 quiet (2026-09-20) -> ADR 0032 (#388 amendment)
- LayoutPlanTests.fs ~121: at RCL6 the cluster is 41 tiles against a 65-tile reservation and 24 reserved tiles go unchecked, so the weaker form passes a reservation narrowed under the placement (#344 review) -> ADR 0064
- ViewTests.fs ~2587: keeperHome's measured W15S26 geometry (north ring nearest centre the mineral at 38,7; east ring the lair at 42,39; west ring loses y 11..23 and 27..42; the one border the shipped six closes faces W16S26) -> ADR 0060 decision 2
