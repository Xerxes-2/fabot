# The world decides the fight once a tick; a colony decides its economy

> **Status:** accepted

> **W0 measured 2026-10-07** (harness bundle of main c872325c, 100 ticks, V8 samples under `decideUnarbitrated`). The fight half is `threatsOfHeld`, the tower, safe-mode and rampart-door plans, the strike/heal reflexes and the melee glue, plus every `Threat`/`Facts` frame inside the pool, rows, spawns, Matcher, mover and Emitter. It is a floor: the Matcher's and the mover's own loops over fighters cannot be told apart from their loops over the economy. Fight share of decide: `pair` 12.9%, `reactor` 28.5% (its Source Keepers are threats), `pair --raided` 24.0%, `siege --raided` 35.7%. `threatsOfHeld` alone is 43–109 ms of 286–612 ms. `observe cpu` does not split decide, so there is no live figure. In peace without keepers the fight half is under 15%, so W7 pays mainly in war and in keeper rooms. Partition: no arena tick and no profile scenario's final assignments put a body across the line; every arena step now asserts this.

> **Accepted 2026-10-07** by the user, on a draft written against main c872325c and grilled the same day. Supersedes ADR 0086's Decision: its S1–S5 as-built record stays as that epic's history. The user decided: W0–W4 are done now, and W5 is decided at W4 on W0's numbers and on whether chase bugs keep coming; a Caster colony keeps a bank floor; towers and the safe-mode reflex move to the fight lane, tower refill stays economy; the shape of W7's light-tick snapshot is decided at W7.

ADR 0086 moved the world's *facts* out of the colony one piece at a time: vision (S1), the danger log (S2), one verdict (S3), the answerer (S4), memory (S5). It kept one rule: **a colony owns its bodies, and `decide` runs once per colony**. Its Considered options rejected "a world layer that also matches and casts centrally". S4 then tried *lending* bodies across decisions and dropped it: the review found five failures, and each one was "one body living in two decisions".

That rule is now the debt. Every fighter is matched inside a colony's decision, so every fight that crosses a colony line needs an adapter, and every adapter has needed a fix. We move the fighters, not just the facts: **a fighter lives in exactly one decision, and that decision is the world's.** A colony keeps the bodies whose work is local, the economy.

## Context

**Bugs from colony-sliced fights**, each a seam between two colony decisions or between a colony's cut and the world:

- #573 (t994,093–994,344): a crippled 6-ATTACK, 1-MOVE raider in shut W18S25 went unchased for 180 ticks. The `Win` was bound to W17S29, whose row the stand-down withheld. The fix (99c112d3) adds one more rule to the verdict: "a row a stand-down withholds wins nothing".
- #571 (open): the same class for a melee-only colony's guard row. Its invariant ("a stand-down shuts the economy, never a kill") is the third restatement after #569 and #573.
- #570: a body standing in a room its colony doesn't project got no Task, no Flee and no step, and died under tower fire. Fixed by projecting the rooms a colony's bodies stand in.
- ADR 0086's own list: #534, #518, the double latches, transit rosters, the S8-B split, `Lendable`, `ChildOutposts`. ADR 0085's S8-B and 6d2a37d4 (a kill counts as held only by this colony's body) exist because two decisions pool one Task.

**Adapters that exist only because fighters are split:** `Brief.Answerer`, `Brief.Counted`, `Threats.pools`, `Threats.answers`, `Facts.Judged.{OutOfSight, Decides}`, `ColonyView.lookingInto`, `Pricer.Sends`, `lendableTo`, `rangersAmong`, `castsFor`, `guardedRooms` as a binding exception, `BorrowedWork.{Defended, Garrisoned, ChildOutposts}`, `ColonyView.Defenders`, `LoadedTowers`, `trespassSplit` and `defendSplit` reading shard-wide holder tables, and `StandDown.Chased`/`Facts.chased`. Three content-keyed share memos (`WorldView.SafeGround`, `ReachGround`, `ArrivalGround`) exist so that six colonies building one room's danger maps get one object back.

**CPU** (live, 2026-10-06): full ticks run about 125–145 ms, of which decide is about 97 ms over six colonies; light ticks about 22 ms. #549 measured forced ticks at a third of all ticks. Each colony indexes the same world ids about four times, and grids are copied per colony. The partial forced tick (decide only the colonies a fact concerns) was abandoned because `Facts.judge` reads every colony's view, so every colony must be projected even when one decides.

**What is already world-shaped:** movement is arbitrated once per room across colonies (0ded3f78), observers are aimed once (#559), the danger log, roster and verdict are world, ADR 0088 places hostile maps in `WorldView`, and `harassCasters` picks a colony among all of them. What is still per colony is the skeleton `ColonyView` → `threatsOfHeld` → Briefs → Pool → Matcher → Emitter, run six times.

**The partition is already clean.** `Guard`, `Defend` and `Assault` admit only `isFighterBody`, and fighters hold no economy Task (`Pool.tierOf`, `Matcher.outranked`). Fighter rows are counted against their own row, never the fleet. Fighters and economy bodies share only Flee, the weapon/heal reflexes and the Resolver, so a split along this line moves no economy decision.

## Decision

**One world is built, judged and fought once a tick. Each colony then decides its economy as a reader of that world.**

```
Game ──► World.ofGame (once) ──► WorldIndex: ids, tiles (packed int), rooms, our fleet,
                                  hostile table + side heal cache, room grids
            │
            ▼
        WorldDanger (once per contested room): arrival maps, Reach, Safe, Shelter,
                    danger log fold, roster, stand-down rows
            │
            ▼
        FightLane (once, every tick it has a contested room)
          Briefs per combat Task ─► Verdict per room over the whole fleet
          ─► FightMatch: every fighter × every combat Brief (arrival time, fitness,
             rung, HomeHold) ─► fighter intents, tower and safe-mode acts,
             cast asks (row, blocks, tier, Caster colony)
            │                                           │
            ▼                                           ▼
        EconomyLane per colony (cadence k, or forced): reads WorldIndex, WorldDanger
          (Reach/Shelter/Shut) and its cast asks ─► plan, pool, quotas, spawn queue
          (fight asks merged by tier), economy Matcher, Emitter
            │                                           │
            └──────────────► Resolver (one per room) ◄──┘ ─► Executor
```

1. **Modules** (Core; names settle as each stage lands): `World/Index` (per-tick id and tile indexes over packed int keys, `room*2500 + y*50 + x`, behind `Fresh.*`); `World/Danger` (today's `Arrival`, the Reach/Safe/Shelter builders and `Observe.Danger`, each run once per room); `Fight/` (Brief, Verdict, FightMatch, Asks: the fight halves of Threat/Facts and a fighter-only Pool/Matcher pass, which never sees a `ColonyView`); `Economy/` (Planner, Quota, Spawns, Layout, Labs, Pool, Matcher and Emitter with the fight arms removed, reading a `ColonyView` plus the read-only world).
2. **A colony** is a home room, its outposts and errands, its spawns, bank, structures, census, Layout/Atlas memo, and its **economy bodies**. It *casts* fighters (it owns the spawn and pays the bank) but does not *command* them: the fight lane commands a fighter from the tick it stands. Its caster colony matters only for renew/recycle and the idle walk home. Stage (ADR 0052) and quotas stay per colony.
3. **The stand-down is an economy gate only.** The fight lane never reads it.
4. **The Caster.** For each cast ask, one colony spawns it: for a home's Defend, that home; otherwise fewest hops, then bank, then name; for a declared assault, its declarer. **A colony whose bank is below a floor casts only its own home's Defend.** The floor is a `Tuning` value sized at W6.
5. **HomeHold.** A garrison body leaves its home only while the home's towers and the bodies left still beat every raider that can arrive first. It is one constraint on `FightMatch`; the verdict prices the fleet directly.
6. **Towers and the safe-mode reflex** are fight-lane acts, since they read the verdict. Tower refill is economy.

### What survives, merges, disappears

| Concept | Fate |
|---|---|
| `Threat`, `Reach`, `Shelter`, arrival maps (0088), `Answer` (Win/Kite/Lose), danger log, stand-down row, fight record, roster | **Survive**, built once per room. The economy reads Reach, Shelter and Shut. |
| `Brief`, `BriefKind`, `Selector`, `Recall`, `Rung`, `Avoid`, `Formation`, `Assault` (0084), Task ids `guard:`/`defend:`/`assault:` | **Survive**, built once per Task world-wide. `Admits`/`Capacity`/`Asks` take the world index and fleet, not a `ColonyView`. The wire is unchanged. |
| Garrison, Defender row, guard/ranger rows, harassment | **Survive as asks** the fight lane emits; the Caster rule picks who casts. |
| `RoomVerdict.Answerer`, `Brief.Counted`, `Threats.pools`/`answers`, `harassCasters`, `castsFor`, the `guardedRooms` binding exception | **Merge into the Caster.** Matching needs no answerer: every fighter is in one pool. |
| `lendableTo`, `rangersAmong`, `Lendable`, `mayLend`, the resident lend rule (#531) | **Merge into HomeHold.** |
| `WorldView.Bodies`, `Defenders`, `LoadedTowers`, the `holdersOf`/`trespassSplit`/`defendSplit` holder tables | **Merge into the `WorldIndex` fleet** (by room, tile and Task). |
| `SafeGround`/`ReachGround`/`ArrivalGround` share memos | **Disappear**: built once per room, nothing to share. |
| `Facts.Judged.{OutOfSight, Decides}`, `ColonyView.lookingInto`, `Pricer` | **Disappear**: the verdict reads the world, not views. |
| `StandDown.Chased`, `ChasedOutposts`, `Facts.chased`, the "row withheld wins nothing" rule (99c112d3) | **Disappear**: a chase is a `BriefKind`, and the stand-down cannot reach it. |
| `BorrowedWork.{Defended, Garrisoned, ChildOutposts}` | **Disappear**: they project a room for a Guard. `Rooms` (raising/lost child: Upgrade, Build, Claim) stays, as economy. |
| 6d2a37d4 (kill held per colony), the S8-B residue, `Unprojected` for fighters | **Disappear**: one pool. #570's invariant stays for economy bodies. |
| `LightTick.deciders` | **Splits** into fight facts (run the fight lane) and economy facts (decide that colony). |

## Stages

Each stage ships alone, gated by `npm test`, `npm run build`, the arena suites, spar and the nine fingerprints. CPU figures are estimates until the stage's interleaved A/B replaces them.

| # | What moves | Behaviour | Gate | Est. CPU |
|---|---|---|---|---|
| W0 | Measure decide's fight half (`threatsOfHeld`, Briefs, fight pricing, fighter matching) against the rest, on the harness and live. A characterisation test of the partition. #571 pinned or described. | none | tests only | 0 |
| W1 | One `WorldIndex` per tick (packed int tile keys, `Map`/typed arrays). `ColonyView` and Atlas reference the world room grid instead of copying it. | exact | fingerprints identical; interleaved A/B | −3 to −8 ms full tick |
| W2 | 0086 S6's remainder (C1) and world danger: Reach/Safe/Shelter/arrival built once per contested room off world vision and our ramparts. The share memos go; the scan set is derived once. | C1 only (a raised child's home in the mother's cut) | named fingerprints; spar | −1 to −3 ms |
| W3 | World fleet verdict: `Facts.judge` prices each raided room over the fleet and the world danger. `Pricer`/`lookingInto`/`OutOfSight` go. A stand-down never withholds a row from the verdict (#571 flips). | the #571 class | #571, #573, S8-B, #534 captures; spar | ~0; unlocks partial ticks |
| W4 | The fight pass is carved inside `decide`, still per colony: Briefs/Pool/Matcher run as two passes, fighters × combat Tasks then the rest, with one Resolver. **W5 is decided here.** | exact | fingerprints identical; any diff is a finding | ±0 |
| W5 | The fight lane goes world: one Brief per combat Task, one `FightMatch` over every fighter; towers and safe mode move in. The merges above land. The `assignments` leaf is unchanged. Behind a `Tuning` flag for one live A/B window. | large: who goes where | spar (every worse metric named); arena suites; #573/#534/#532/S8-B captures re-pinned on reproduced live outcomes | −5 to −15 ms war full tick; ~0 in peace |
| W6 | Cast asks and the Caster, with the bank floor. `harassCasters` folds in. | who casts | spar; quota tests; `observe quotas` shows asks by caster | ~0 |
| W7 | The fight lane every tick with a contested room: on a light tick it runs over a snapshot and the economy replays. Fight facts stop forcing colony decides; economy facts force only their colony. | latency: a fight is never a tick late; economy up to k−1 | light-tick tests; spar; `observe cpu` split | −15 to −30 ms mean in war |
| W8 | *Only if W0/W7's numbers ask*: incremental economy, a colony's economy decision recalled on a census, creep-set and hostile signature. | exact by construction | fingerprints; a signature-miss counter | unknown |

W1–W2 are exact or nearly so and pay now. W3 removes the reason the partial tick died before any large move. W4 makes W5 a lift instead of a rewrite. W7 needs W5: a per-colony fight cannot run alone on a light tick. If W5 is not taken at W4, this ADR is amended there to say the fight stays per colony after W4.

### What this changes in other ADRs, and when

The amendment banner lands with the stage that changes the code.

| ADR | Change | Lands at |
|---|---|---|
| 0086 | Decision superseded (this ADR); S1–S5 as built stand as history; S6 folds into W1/W2. | now |
| 0043 | The stand-down never withholds a row from the verdict (W3); it is an economy gate only, never read by the fight (W5). | W3, W5 |
| 0047, 0052 | `decide` per colony becomes economy decide per colony; the fight is decided once per world. Stage and quotas unchanged. | W5 |
| 0085 | The Brief is built once per Task world-wide; `Answerer`/`Counted` leave it; `Admits`/`Capacity`/`Asks` read the fleet. Recall and Formation unchanged. | W5 |
| 0080 | A mother guarding a child's home becomes the Caster rule plus HomeHold. | W5, W6 |
| 0081 | Harassment casters become the Caster rule. | W6 |
| 0082 | Decision 1's cadence governs the economy; the fight lane runs every tick with a contested room; decision 4's forcing rules split into fight and economy facts. | W7 |
| 0088 | Substance unchanged; "our floods per body in the Atlas memo" become the fight lane's fighter floods. | W5 |

## Consequences

- One fight, one decision. Every invariant of the #569/#571/#573 shape holds by construction instead of by a clause in the verdict.
- Fight tests build a world index and declarations, not N `ColonyView`s. Economy tests are untouched. A capture's input is the whole fleet (#532).
- A colony's census no longer holds fighters. Quota code that counts living fighters for casting reads the fleet by caster.
- One colony's bank pays another's fight, bounded by the floor.
- The light tick gains a fight-only kind, and `observe cpu` must split it.
- Risk is concentrated in W5. Every stage before it is reversible and pays alone, so if the season (ends 2026-11-01) runs out, W1–W4 still leave the code cleaner and faster.

## Considered options

- **Keep patching per colony (0086 as is, finish S6).** Cheapest per step, but every patch has been a new adapter (S3's casters rule, S4's `Counted`, #569, #573, #571), partial ticks stay blocked and the share memos stay. Rejected as the end state; its cheap parts are W1 and W2.
- **A world-first rewrite from scratch.** About 25 days remain. The economy half is per colony by nature and works (ADR 0052's reason holds), and the tests, arena and spar are the asset. Rejected.
- **One world decide for everything, economy included.** Re-derives every quota and loses stage and bank per colony; 0086 rejected it, and that reason stands for the economy.
- **Lending (0086 S4's first cut).** Rejected by its own review: one body in two decisions. This ADR is its opposite.
- **Amend 0086 instead of superseding it.** Keeps one epic in one file, at the cost of a long ADR whose Decision says the opposite of its banner. Rejected by the user.
- **The nearest spawn always casts.** A poor colony's bank would be drained by another's raid. Rejected for the floor.
- **Towers stay with the home colony.** They would read the verdict across the lane line. Rejected.

## Open question

- **W7's light-tick snapshot**: a narrow one (contested rooms and fighters) is cheaper but is a second World builder to keep correct; the full World costs the light tick. Decided at W7.
