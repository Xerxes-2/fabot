# The hungry line and the whole line are two numbers, and the assignment table says which one a target is judged by

**Status: accepted, landed in #285.** The decision is the user's on option 1 of that ticket; what follows is the derivation, the seam it is drawn at, and every clause of ADR 0010, ADR 0013, ADR 0025 and ADR 0034 it overrides.

ADR 0010 put a decaying structure's **hungry line and its whole line on one number**: a road is hungry below half of max and whole *at* half. A Task that exists exactly while a condition holds is ADR 0013's own shape and it is right for a rock that is empty or not; applied to a number that a single repair tick steps across, it means the repair is over the tick it starts. The body tops the road up, the structure crosses the line, the Task leaves the pool, and the holder is released `task-gone` on the next tick's pool. #226 measured that as **17 repair `task-gone` releases in ~300 ticks** (t193,888–194,181, whole fleet, after ADR 0054 landed), Repair being the leading source of them once the [[refill cluster]] had taken the extensions out of the count — 11 of the 17 on one W13S28 worker inside a few dozen ticks. #284 split the half of that story it could fix without a decision; this is the half that needs one.

**Measured live, 2026-09-13, t≈403,2xx, over all three colonies** (`/api/game/room-objects`, `observe.mjs tasks`, `observe.mjs timeline`; every number in this paragraph is read off the server today and none is inferred):

- **W13S28**, the RCL6 home: 116 roads, min 34.0%, **median exactly 50.0%**, 58 of them below the line. Four containers at 75,800, 80,900, 125,400 and 135,000 of 250,000 — **two of them have sat at a third of max** while the colony ran. Six ramparts at 85,001–98,401 against a 100,000 floor.
- **W12S28**: 109 roads, min 42.4%, median 56.0%, 12 below the line. **W13S29**, an outpost: 40 roads, min 29.0%, median 51.0%, 17 below.
- **Eighty-nine decaying structures stand below their line right now — 87 roads and two containers — and not one creep holds a Repair.** The fleet of 23 is six workers on Upgrade, two upgraders on Withdraw and idle, and the anchors, haulers, miners and reserver on their own rows.
- **Nothing anywhere is below 25%**, which is `Tuning.RepairRescueLine` — so #284's rescue budget is dormant, not failing. It ran, it lifted the 2% trunk and the 8% outpost roads its ticket was written against (W12S28's worst road is now 42.4%), and it has nothing left to qualify. The roads that remain are not starved; they are **repaired to the line and never above it**, which is a different failure and the one this ADR is for.
- The churn itself, still live. `worker-402902-Spawn2`, six consecutive ticks:

```
403014  matched  repair:6a9d9e92… (pool-order)
403015  released repair:6a9d9e92… (task-gone)
403015  matched  repair:6a9d9a8a… (pool-order)
403017  released repair:6a9d9a8a… (task-gone)
403017  matched  repair:6a9d9365… (load)
403019  released repair:6a9d9365… (task-gone)
403019  matched  upgrade:6a8caaad… (travel-cost)
```

  Those four roads stand today at **70.0%, 51.6%, 52.8% and 50.4%**, and the spread is the whole mechanism: one repair tick is `Work × 100` hits whatever the structure is, so at the live worker's body — **11 Work, 12 Carry, 12 Move**, 1,100 hits a tick and 600 energy a load — a top-up is **22% of a plain road's 5,000, 4.4% of a swamp road's 25,000 and 0.44% of a container's 250,000**. The band a structure gets today is an accident of the body's size against the structure's max, and the live populations are exactly that arithmetic: W12S28's plain roads sit at a median 60.0% and its swamp roads at 51.2%, and the containers sit at 30%.

**The obvious fix is not available, and the reason is the seam and not the number.** Raising the single line to 0.8 buys no hysteresis, because the pool is stateless and asks one question per tick — *is this below the line* — so a structure at 0.6 is indistinguishable on the way up from on the way down. A single pair of numbers moves the oscillation band and nothing else; a road repaired to just over 0.8 is hungry again on its next decay tick exactly as it is today. Hysteresis needs to know whether a structure is **already being repaired**, and that fact exists in exactly one place: `Assignments`, the creep-name-to-task-id map that is the only state this colony carries between ticks.

## The decision, in five parts

### 1. Two lines for the decaying kinds, and the held fact picks between them

A structure of a `WholeLine.Fraction` kind — a road, a [[container]] — is **hungry below `Tuning.RepairTrigger` when no creep holds its Repair, and below `Tuning.RepairWholeLine` when one does.** Everything else about the pool is unchanged: the Task is still derived from scratch every tick, still exists exactly while its condition holds, and still releases its holders `task-gone` when it stops.

`RepairTrigger` keeps its name and its 0.5 — it is the **hungry line**, the number a reader of ADR 0010, ADR 0034 and `CONTEXT.md`'s tuning line already looks up, and what a trigger names is what starts a thing. `RepairWholeLine` is new and is **0.8**.

**Where 0.8 comes from**, derived at RCL6 against a 1,800-energy bank and the worker row's live body, as every `Tuning` field must say (ADR 0052 decision 5):

> The band has to be one a **single load closes on the dearest decaying structure the colony has**, or the ratchet does not ratchet: a body that empties mid-repair is released `inapplicable`, its target is unheld the next tick, and the structure is judged at the hungry line again and leaves the pool wherever the load ran out. The dearest is the container at 250,000. 0.3 × 250,000 = **50,000 hits = 500 energy**, against the 600 a live worker carries. At 0.9 it is 100,000 hits and 1,000 energy — two loads, and a repair that cannot finish in one trip is a repair the rule does not close. A plain road's band is 1,500 hits and 15 energy; a swamp road's is 7,500 and 75.

**The band is free in energy and expensive only in trips, which is the whole argument.** Decay is a rate and repair is priced per hit, so the energy a structure costs over a thousand ticks is the same whatever line it is topped up to — measured, the passive upkeep of all 265 roads in the three rooms is about **450 energy per 1,000 ticks**. What the band changes is how many walks and how many matches buy those hits. A container repaired to 0.8 falls back below 0.5 in 7,500 ticks of decay where today it is hungry again the tick after; a plain road's 30 points is 15,000 ticks of passive decay, or 1,500 hits of traffic — `ROAD_WEAROUT` is one hit per body part per step and is **not** terrain-scaled, so it is about 75 crossings by a 20-part body, and it is why the base cluster is where the supply regenerates. The one-off catch-up, measured: **3,118 energy** lifts all 87 roads that stand below the line today to 0.8 (396 in W12S28, 2,398 in W13S28, 324 in W13S29), and 2,433 more lifts W13S28's two starved containers with them — call it 5,500 energy once, against a colony that grosses that in a few hundred ticks.

### 2. The rule reaches the fraction-judged kinds and no others

`wholeLine` answers three things and this decision touches one of them.

- **`Fraction`** — road, container: two lines, as above.
- **`Floor`** — the [[rampart]]: unchanged, and ADR 0034 rejected hysteresis here in its own Considered Options, on the arithmetic that a repair *tick* already overshoots an absolute floor by 1,100 hits where it overshoots a fraction of 250,000 by nothing. A floor is not a fraction of anything, so the substitution has no second number to make.
- **`Full`** — the [[keep]]: unchanged, and it must be. There is no line above full to raise the release to, and the entry line is read by a second decision — ADR 0034's safe-mode arm fires on a Keep structure below full hits while a hostile stands in the [[home room]] — so a held Keep structure judged by anything but full hits would either arm safe mode on a different fact or leave the arm reading a number the Repair pool chose.

The restriction is the rescue budget's own (`rescued` lifts the decaying kinds alone, "a rampart is judged against a floor and a Keep structure against full hits, and neither is a thing the colony is letting rot"), and it is written once: the substitution happens inside the `Fraction` arm of `isHungry` and nowhere else.

### 3. The seam: one boolean per candidate target, derived once, spelled forward

The Planner is handed **`held: Set<string>`, the task ids standing in this colony's assignment table**, derived once in `Entry` before `planTasks` and read by `isHungry` as `Set.contains (taskId (Repair id)) held`.

Three properties make this the seam rather than "the Planner gets the assignments":

- **It is spelled forward, never parsed.** The candidate id is in hand and `taskId` writes the key; nothing pulls a target out of a string the way the vision grace's `taskTarget` has to. A `Withdraw` and a `Repair` on the same container are different keys by construction, so "held" can never mean "somebody is drawing from it".
- **The Planner learns one fact about its own decisions and nothing about creeps.** It does not see a body, a position, a load or a name. ADR 0025 rejected a Planner-side lookahead because "whether the wait is covered depends on the walker's body and position, which is the Matcher's knowledge" — that reason is untouched and is why the set is a set of task ids and not the `Assignments` map.
- **The set is filtered to living creeps.** `Assignments` arrives from Memory and may name a creep that died last tick; the Matcher drops those silently, but `planTasks` runs first. A colony must not hold a Task open on the strength of a body that is not there, and the join is one `Set` over `view.Creeps`.

`hungryStructures` takes the set. Its **second reader gets its own predicate**: ADR 0034 gave the safe-mode arm a share of this one walk, and its question — is any Keep structure below full hits — needs neither the tuning nor the held set, so `Layout`'s arm calls a `keepDamaged` of its own rather than being handed an empty set that would be silently wrong the day the rule widens. The cost is a second walk over a hundred-odd structure hits, which is not a flood (#171).

### 4. A holder that goes, goes: there is no memory of a half-finished repair

The held fact is **last tick's assignment table and nothing else**. So:

- The tick a holder dies, is released `inapplicable` (an empty store — `Repair` is `spending && not standing`), is released `threatened`, or is released for a shrunk capacity, its target is **unheld on the next tick's pool** and is judged at the hungry line again. Above it, the Task is gone; below it, the Task stands at the ordinary surplus rung and any body may take it.
- **The ratchet is the assignment, not the structure.** A worker that empties at 0.62 leaves a road at 0.62 and the colony forgets it was half-way. That is accepted and is the right price for a stateless pool: 0.62 is better than 0.51, and remembering otherwise means a per-structure flag in heap state that ADR 0012 and ADR 0017 have refused three times, plus an invariant between it and the table that already holds the answer (ADR 0025: one fact, one field).
- **The rule is monotone**: the pool with it is a superset of the pool without it. The held line only ever keeps a Task pooled that would otherwise be gone, and never removes one — so no Task can disappear *earlier* because of this decision, and that is a property a test can state directly.
- **Capacity is unchanged.** An ordinary Repair stays `Capacity.unbounded` — "a road under the spawn is worked by whoever is standing over it" — so two bodies on one held structure finish it sooner and both keep it pooled. Nothing here needs a cap, because what the cap would ration is a walk, and this rule creates none.
- **`task-gone` stays the release reason and stays the measure.** The rule does not stop a structure crossing its whole line; it stops it crossing in one tick. The last repair tick still overshoots — a plain road ends near 0.94 — and the holder is still released `task-gone` on the next pool. What falls is the count: one release per repair **job** instead of one per repair **tick**.

### 5. #284's rescue budget survives this decision untouched, and is re-measured after it

`RepairRescueLine` (0.25) and `RepairRescues` (2) are not changed, not shrunk and not retired here, and the reason is that **this decision removes the premise the budget was sized against**. #284's finding, in `Pool.fs`'s own words, is that "the cluster a loaded worker stands in regenerates its own supply of two-tile-away Repairs faster than anybody would walk out of it" — that regeneration is precisely what a 30-point band removes, by an order of magnitude on a plain road and by four on a container. Whether a far road then wins on travel cost without a rung at all is a question the live colony can answer and this ADR cannot.

What is **not** done, and deliberately: the rescue set does not read the held fact. The rescue asks "which far structure is worth a walk", judged on hits alone; the held fact asks "does this Task still exist". Mixing them needs a fact neither table has — whether a repair in progress *began* as a rescue — because a held road at 0.4 is indistinguishable from a rescued one climbing past 0.25. The leak this leaves is named rather than hidden: a rescued structure passes 0.25 within a tick or two of repair and frees its slot while its body works on to 0.8, so the colony may have more bodies **finishing** rescues than `RepairRescues` **starts**. Bounded by the worker row, and the honest reading is that "two at a time" was always measuring starts.

## What this overrides, clause by clause

**ADR 0010** — "Repair enters as a surplus-tier Task (rank 1, triggered below half hits — the threshold is a tunable, not part of this decision)."

- The ADR **disclaims its own number**, so what is overridden is not 0.5 but the *arity*: there is one threshold in that sentence and there are two now. The tier, the rank and the layering-by-target are untouched.
- **The #234 banner's Repair clause is overridden in its reason**, not in its ruling: "Repair stays on the old rung, because a repair target leaves the pool the tick its structure is whole and a row lifted onto it churns through task-gone releases (#226)." After this decision a repair target does **not** leave the pool the tick a top-up crosses the entry line, so the stated ground for keeping Repair off the home-site rung is measured away. This ADR does **not** re-open that rung — it is a separate decision about what a colony spends surplus on, and it gets a separate ticket.

**ADR 0013** — "the task exists while the condition holds and is *gone* otherwise, releasing its holders through the existing `TaskGone` path", named there as "the Repair shape".

- The condition is now **two conditions and a selector**, for the decaying kinds. Everything else the ADR decided stands: the Task is still derived from scratch, still released through `TaskGone`, still has no rule following the release.
- **Its "no field nobody reads" clause is upheld, not overridden.** The [[spatial projection]] does not widen by one byte; the fact read is one the colony already persists.
- ADR 0013's **first rejected option is re-affirmed**: "Keep the task pooled, gate in `applicable` — rejected: the pool would misrepresent the colony's actual work". See the Considered Options below; that is still the answer.

**ADR 0025** — the Considered Options clause: "Keep Harvest out of the pool while drained and gate the timer in the Planner with a colony-wide lookahead — rejected: **the Planner is creep-blind**; whether the wait is covered depends on the walker's body and position, which is the Matcher's knowledge."

- **This is where the creep-blindness clause actually lives**, and it is what this decision narrows. The Planner now reads one fact about the **assignment table** — a fact about the colony's own decisions last tick. It still reads nothing about a body, a position or a load, so the clause's own stated reason is intact and what is overridden is the stronger reading it has been given in the tree: that the Planner reads nothing about the fleet at all.
- `Planner.fs`'s Harvest comment ("the Matcher's knowledge, not the creep-blind Planner's", citing ADR 0013 and ADR 0025) stays true of Harvest and stops being true of the Planner in general. The comment is a code comment and gets corrected by the ticket, per `docs/agents/orchestration.md`.

**ADR 0034** — "A rampart's Repair reads a floor, not half of max… hungry below the floor and whole at it", and "`isRepairable` becomes a per-kind whole line rather than a boolean", and "What this revises — ADR 0010: Repair's 'below half hits' becomes the trigger of the decaying kinds only".

- The rampart's floor and the Keep's full hits are **unchanged**, and part 2 above says why each must be. What is overridden is the *shape* of `wholeLine`: it answered one line per kind and now answers one line per kind **per held-ness**, for one of its three arms.
- 0034's own revision of 0010 is narrowed one step further: "below half hits" is the **entry** of the decaying kinds only.
- 0034's rejected option "Rampart repair with hysteresis (hungry below the floor, whole at floor plus a band) — rejected as unnecessary" **stands and is not reopened**, on its own arithmetic.

## A correction to the tree

`Pool.fs`'s `rescued` comment says of a rescued road: *"It needs no second visit — a worker's load repairs a hundred hits an energy, so one trip carries a road from a quarter to over the trigger and out of the pool, and the Matcher's keep holds it there until it is whole."*

**`Matcher.fs` contains no `Repair` arm at all — grep it; the word does not appear in the file.** What runs is the generic anti-thrash keep, which is conditioned on the Task still being pooled and on the holder still passing the gate cascade, and `applicable` for Repair is `spending && not standing`. So the sentence credits the Matcher with a Repair-specific hold it does not have, and over-promises the generic one: the hold lasts while the body has energy and the structure is still below the line, and a body that empties at 0.4 is released `inapplicable` with the road left where the load ran out. What actually holds a rescued road is that it is **still below the whole line and therefore still pooled** — which is exactly the fact this ADR is about, and the reason the comment reads as true today is that the two lines are the same number.

Per `docs/agents/orchestration.md`, an accepted decision is not rewritten when a run proves its reason false; the **comment** is corrected by the ticket that lands this ADR, and a test pins the real invariant (a holder released `inapplicable` mid-repair leaves the structure pooled or not by its hits alone).

## Considered Options

- **Raise the single line to 0.8 (or to full).** Rejected, and it is the tempting misreading: the pool is stateless and asks one question a tick, so a structure at 0.6 is the same to it climbing or falling. One pair of numbers moves the oscillation band and removes none of it, and the structure is hungry again on its next decay tick exactly as it is now — worse than doing nothing, in #285's words, because the colony pays a bigger top-up for the same churn.
- **Two lines applied to every target, held or not.** The same option in different clothes: with no selector the lower number is dead and this is the bullet above.
- **Let the Matcher's keep outlive the pool for a Repair below the target line** (#285 option 2, the vision grace's shape, #151). Rejected as written, and the blocker is in the tree: a Task in no pool reaches the Emitter as nothing — `Entry`'s `crossings` says so in its own comment, "they reach the Emitter as nothing … and the mover as a crossing, which is the whole of what the grace buys" — so the holder would stand on the road doing nothing. The grace buys a walk home; there is no walk here.
- **Keep every decaying structure pooled always and put the line in `applicable`** — ADR 0025's answer for Harvest. Rejected on ADR 0013's own first rejected option, unchanged by anything since: the pool would misrepresent the colony's actual work, N per-creep rejections instead of one absent Task, and verbose Scoring narrating rejections of work that does not exist. Measured, it is also the difference between the **89** Repairs standing in the pool today and the **271** — 265 roads and six containers — that would stand in it, every one scored against every applicable body every tick.
- **Remember the repair in heap state** — a per-structure "being repaired" flag in Memory. Rejected: ADR 0012 and ADR 0017 have declined per-structure heap state three times, and the state already exists in the one table the colony does persist, so a second copy would need an invariant between them (ADR 0025: one fact, one field).
- **Hand `planTasks` the `Assignments` map.** Rejected as the seam, not as the change: it is the same information and a wider door, and it invites the next rule to read a creep's name or a body out of it. One boolean per candidate, derived once, is what ADR 0025's clause can survive.
- **Read the held fact in the rescue set too, so a rescue keeps its rung and its cap to the whole line.** Rejected for now, with the leak written down in part 5: the rule needs a fact neither table has — whether a repair in progress began as a rescue — and the alternative spellings either let an ordinary held road take a rescue slot or need the state this ADR exists to avoid.
- **Rename `RepairTrigger` to `RepairHungryLine` so the pair reads as a pair.** Rejected: the name is quoted in ADR 0034, in `Tuning`'s own doc comment and in `CONTEXT.md`'s tuning line, and it is set by name in a couple of dozen tests. A trigger starts a thing and a whole line ends it; the pair reads.
- **Live with the churn as the price of a stateless pool** (#285 option 3). Rejected on the measurement: 17 releases per 300 ticks, 87 structures pinned at their line, two source containers at a third of max, and a colony whose paving sits permanently at half.

## Consequences

- **`Tuning` gains one field**, `RepairWholeLine = 0.8`, beside `RepairTrigger`, `RepairRescueLine` and `RepairRescues`. It is a number the bot **chose** and not one the server sets, so it goes to `Tuning` and not to `Engine` — ADR 0057's split. It states the [[stage]] and bank it was derived at (RCL6, ~1,800, the 11W/12C/12M worker) and it carries its own pairwise test, "because a number without one is a bug that has not happened yet". The invariant `RepairTrigger < RepairWholeLine ≤ 1.0` is a test and not a comment.
- **No engine constant is added and the projection does not widen.** The derivation quotes `REPAIR_POWER`, `ROAD_WEAROUT`, `ROAD_DECAY_AMOUNT` and the container's decay clock, but no decision *reads* them at runtime, and ADR 0007's growth rule admits a field on the tick a decision reads it. `CONTAINER_HITS = 250,000` and the road's 5,000 / 25,000 are corroborated by today's live hits; the decay constants are the engine's published numbers and were **not** re-verified against `mod-season5` in this run — flagged, because the season mod has surprised this repo before (ADR 0060).
- **The number to move is 17 per 300 ticks**, on #226's own measure and window shape (`observe.mjs timeline` over the whole fleet, counting `repair … (task-gone)`). After this lands the count is bounded by the repair **jobs** the fleet finishes in that window, which at 1,100 hits a tick and the live bands is single digits. The honest failure signal is the count not moving — that would mean the releases were `inapplicable` all along and the line was never the binding constraint.
- **A body may stand a long time on one structure.** A container's band is 50,000 hits, which is ~45 ticks at the live worker — 45 ticks it is not upgrading. It is the same energy either way, spent in one place instead of scattered across 45 matches and 45 walks, and it is the first Repair in this colony that is a job rather than a reflex. Verbose Scoring and the transition log should read as one `matched` and one `task-gone` per job; if they do not, the change did not land.
- **An outpost container's band is worth more than a home one's.** A container in a room the colony does not own decays on the 100-tick clock rather than the 500 (`CONTAINER_DECAY_TIME` against `CONTAINER_DECAY_TIME_OWNED`) — inferred from the engine constants, not measured, W13S29's two containers standing at 72% and 84% today — so out there the band buys ~1,500 ticks against ~7,500 at home, and the trip it saves is the one across the [[seam]].
- **The `Pool.fs` `rescued` comment is corrected and a test pins what actually holds a rescued structure.** See the correction above.
- **#234's ground for keeping Repair off the home-site rung is gone**, and re-opening that rung is a separate decision with its own measurement (does a far road ever get matched once the cluster stops regenerating nearby work?). It is filed, not decided here.
- **#284's rescue budget is re-measured rather than re-derived.** The measurement that decides its fate: the count of hungry decaying structures in the home room per tick before and after, and whether any structure outside the base cluster is ever matched **without** the rescue rung. Retiring a rung is cheaper than keeping a tunable nobody can justify, and that is a ticket after the band has run for a few thousand ticks.
- **Two findings from the live read that this ADR does not fix**, filed rather than widened into: (a) **zero repair holders fleet-wide** with 87 structures below the line — Repair loses the surplus tier to the Upgrade under a loaded worker's feet, which is #284's other half and is a rung question, not a line question; (b) **all six of W13S28's ramparts stand 1.6–15% below their 100,000 floor** and none is held, which is the same symptom on a kind this ADR's rule deliberately does not reach.
- **CPU is one set and one lookup per candidate.** The held set is built once over at most the colony's creep count (23 live today) and asked once per pooled Repair candidate (89 today). No flood, no projection field, no second walk except the safe-mode arm's own. Per the profile harness's standing rule (ADR 0056, quoted in ADR 0057), a path no scenario executes is a path no profile has seen: the harness needs a scenario with a **held** Repair, or the new branch is never run in a profile.
- **The Planner's signature changes and ~17 test files call it.** `planTasks` gains the set; `hungryStructures` gains the set; the safe-mode arm loses its share of that walk. Most call sites pass an empty set and mean it. Carry that list forward per `docs/agents/orchestration.md` — the mechanical churn is expected, and any *other* red test is the alarm.

## Glossary additions

Proposed for `CONTEXT.md`, not written here. The existing **Repair** entry says "created when a repairable structure falls below its **whole line**, gone when it is whole", which is now true of two of the three kinds and needs the split:

- **Hungry line** — where a [[repair]] Task is *created*: `Tuning.RepairTrigger` of max for the decaying kinds, the [[rampart]]'s floor, full hits for the [[keep]]. A structure at or above it that nobody holds carries no Task.
- **Whole line** — where a [[repair]] Task is *gone*. For the rampart and the Keep it is the hungry line, unchanged. For the decaying kinds it is `Tuning.RepairWholeLine` and applies **only while a creep holds that Repair** — the one fact the Planner reads about the assignment table, and the reason a repair is a job rather than a one-tick top-up. The colony remembers nothing about a half-finished repair: the ratchet is the assignment, so a holder that empties or dies leaves the structure judged at the hungry line again, wherever its load ran out.

## The ticket cut

One ticket — the diff is small and the parts do not separate — but the acceptance is in four pieces and each is separately checkable.

1. **`Tuning.RepairWholeLine`, the held set, and the two-line `isHungry`.** Scope: the new field and its doc comment (stage, bank, derivation); `heldTaskIds` in `Facts.fs`, derived in `Entry` from `assignments` filtered to `view.Creeps` and spelled forward through `taskId`; `isHungry` and `hungryStructures` taking the set; `planTasks`' signature; `keepDamaged` for the safe-mode arm. **Acceptance:** a 5,000-max road at 2,400 with no holder is pooled, at 2,600 with no holder is not, at 2,600 **with** a holder is, and at 4,100 with a holder is not; a rampart under its floor and a dented Keep are pooled identically with and without a held set; an assignment naming a dead creep holds nothing; the pool with the rule is a superset of the pool without it, over a generated view. **Tests:** `PoolSeatTests` (seats, refill cluster, unreachable targets, **repair**) is the domain file; `QuotaTuningTests` takes the pairwise for the new field; `Fixtures`/`PoolFixtures` carry the `planTasks` signature change into the ~17 files that call it.
2. **The release path.** **Acceptance:** a holder released `inapplicable` (empty store) at 0.62 leaves the structure out of the next tick's pool; released at 0.4, in it, at the ordinary surplus rung and uncapped; the final release when the structure passes 0.8 is still `task-gone`. **Tests:** `MatcherVerdictTests` for the reasons, `PoolSeatTests` for the pool.
3. **The comment corrections.** `Pool.fs`'s `rescued` sentence about the Matcher's keep, and `Planner.fs`'s creep-blind Harvest comment (still true of Harvest; no longer a claim about the Planner in general). Each correction gets the test that pins the real invariant rather than a re-worded promise.
4. **The profile scenario and the banners.** A harness scenario in which a creep holds a Repair on a structure between the two lines. Banners land on this ticket per `docs/agents/orchestration.md`: **ADR 0010** (the trigger's arity, and #234's stated reason for Repair's rung), **ADR 0013** (the condition is two conditions and a selector), **ADR 0025** (the creep-blindness clause, narrowed to body and position), **ADR 0034** (`wholeLine`'s shape, the `Fraction` arm alone).
