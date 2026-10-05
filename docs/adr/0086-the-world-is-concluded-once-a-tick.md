# The world is concluded once a tick; a colony owns bodies and economy

> **Status:** accepted

> **Proposed 2026-10-05** on an inventory of main c9deda59 and live play at t973,265–973,581; S0 at main feaee027. **Accepted by the user 2026-10-06.** It amends ADR 0047 and 0052 (the `ColonyView` as decide's only input), 0043 (the stand-down as a per-colony log) and 0085 (the Brief keyed per colony); each carries an "amended by 0086" banner. S0's characterisation tests are commented `Characterisation (the world layer's S0)`.

The shell already builds one `World` a tick (`World.ofGame`), and six memories already live on it once: `Sightings`, `Towered`, `ExitWatches`, `Healers`, `ArmedSeen` and `RivalControllers`. The split into colonies starts at three points:

1. **The cut.** `ColonyView.ofWorldRecalling` narrows every room fact, the hostiles included, to one colony's scan set (`World.scanRecalling`).
2. **The memory.** The Raid log is one `RaidState` per colony (`observe.colonies.<home>.raids`). `Observe.standDown` turns it into that colony's `StandDown` gate *before* the cut, so the gate decides which rooms the colony even sees.
3. **The pricing.** `decide` runs once per colony. Each run builds its own `Threats`, Briefs and `Facts.answered` verdicts, and the Raid-log fold prices `Facts.answer` again on the same view.

So the world's facts are read from the engine once, and then narrowed and re-concluded once per colony. Most of 2026-10-05's seam bugs come from that.

## Context

### Today's seam bugs (live)

1. **Vision does not reach the conclusions** (#534, t973,265–973,349). W17S25's stand-down shut its outpost W18S25 on a Lose while the healer Shade461 stood beside Idol768. The healer left, and Idol768 stood alone at 818 hits, 8 live ATTACK and 1 live MOVE: a perfect Kite. Nobody answered it. The mother W15S28's `ChildOutposts` had dropped the shut room, so her resident in W17S25 could not be called. W13S28's observer was looking at W18S25, yet W17S25's log still read "last seen t973,265". The look reached `World.Rooms` and no view. Live, the resident entered only around t973,440.
2. **One room, two verdicts** (#518, t964,802 and after). Each view prices `Facts.answered` over its own bodies. The patches are `Lendable`, the S8-B split in `rangersWin`, and the cross-colony `mayLend`. By construction, a raid that only mother and child together win is `Lose`, and the child stands it down.
3. **One room, two latches.** At t973,5xx, W18S26 was in both W15S28's and W17S29's `rivalHeld`, with look strides of their own (`LastLooked` 970,261 and 972,434). `Holds` and `Threatened` depend on whose `RoomControl` saw the room. `Threatened` duplicates the world's `ArmedSeen` under a different clock and predicate.
4. **Recorded but not priced.** The episode roster enrols every hostile in a view, transit rooms included. W17S29's log held 105.8 KB of rosters from W17S25's siege, which it only crossed. The stand-down prices only declared rooms and ranger rooms. 52 of 375 hostile ids on record (14%) are in two or more colonies' logs.
5. **A clock left with the wrong colony.** A harassment sighting's clock stays in the old caster's log when `harassCasters` picks a new one. A garrisoned child home's Fight record is the mother's, so the child's own Defend cannot see it.
6. **A capture cannot reproduce a seam.** An arena capture of the child alone decided differently from live play, because her view needed the mother's bodies (`Lendable`) and the mother's scan (`ChildOutposts`).

The live duplication: 34 distinct rooms across 43 scan-set slots (about 21%). W17S25 sat in three views, so its Reach, ring, verdict and roster were built three times, and its verdict up to six times. The raid logs hold about 182 KB of 493 KB of Memory (37%).

### Inventory

One row per datum held or concluded per colony. "Dup" is how many times one room is concluded per tick. The full rows, with file:line, are in the inventory note this ADR was written from. Its table is the S0 inventory as it stood, and the stages amend the code, not the table.

| # | Datum | Built / stored | Dup | Seam | Stage |
|---|---|---|---|---|---|
| A1 | `ColonyView.Hostiles`/`Allies`/`InvaderCores`/`Fallen` | World once, then cut per view | per view | #534: a shut room's look reaches no view | S1 |
| A2 | `Sightings` (narrow) vs `Seen` (wide) | World, heap | 1 | Two names for one memory | S6 |
| A3 | Scan set (`scanRecalling`) | Per colony, twice a tick | 2 per colony | A shut room is unwatched; `ChildOutposts` and `Evacuating` bolt rooms back on | S1, S6 |
| A4 | Observer aim (`Layout.planObserver`) | Per colony owning an observer, off its own Box tier | 1 | Plans for one colony's box; the look is dropped by every other cut | S6 |
| A5 | `ColonyView.RoomControl` | World, cut to worked rooms | per view | B2–B4 are folded off it | S1 |
| A6 | `ColonyView.Foreign` | Per colony | per view | Exists because bodies are split. It stays for movement | stays |
| B1 | `RaidState.Outposts` → `StandDown.Shut`/`Impassable`/`Raided`/`Chased` | Per colony fold, `raids.outposts` | 1 per declaring colony | #534; `Impassable` blocks one colony's chains only | S2 |
| B2 | `RaidState.RivalHeld` | `raids.rivalHeld` | per seeing colony | W18S26 latched twice; three rival memories in all | S2 |
| B3 | `RaidState.Holds` | `raids.holds` | per seeing colony | A reservation is a room fact | S2 |
| B4 | `RaidState.Threatened` | `raids.threatened` | per declaring colony | Duplicates `ArmedSeen` | S2 |
| B5 | `RaidState.Episodes` (roster vs losses) | `raids.episodes` | 14% of ids in two or more logs | Transit rosters (106 KB) | S5 |
| B6 | `RaidState.Fought` | `raids.fought` | per resident-room owner | The child cannot see the mother's record | S5 |
| B7 | `RaidState.Taken`/`Probes` | `raids.taken`/`probes` | 1 | A rival room's fact owned by one assaulter | S5 |
| B8 | Harassment-sighting clock | Caster's `raids.outposts` | 1 | Stays with the old caster | S5 |
| C1 | `Threats.Reach`/`Safe`/`Shelter` | Per colony, per raided room | per view | A transit room has no ramparts in `transiting` | S4 |
| C2 | `Threats.Briefs` (ADR 0085) | Per colony | per pooling colony | S8-B exists because two Briefs disagreed | S4 |
| C3 | `Facts.answered`/`rangersWin`/`rowWins` | Per view, plus `raidDeadlines` and `strikeGone` again | up to 2 × views | One room, two verdicts | S3 |
| C4 | `Facts.mayLend` (cross-colony) | Per view | per view | The lender's rule rebuilt in the borrower's view | S3 |
| C5 | `Atlas` | Per colony plan memo | 2–3 for shared rooms | Not a seam-bug source; terrain tables may be shared later | stays |
| C6 | `Spatial.RivalRooms`/`AvoidRooms` | World, cut | 1 | Already world | S6 |
| D1–D10 | Patches: `Lendable`, `ChildOutposts`, `Defenders`, `LoadedTowers`, `MinedRooms`, `Seen`, `Stages`/`Declared`, `Defended`/`Garrisoned`, `harassCasters`, light-tick `RoomRoles` | Smuggle one colony's facts into another's view | — | The symptoms | S3, S6 |
| E1 | Who answers room R | Nobody: stitched by D1, D2, C4 | — | New | S3 |
| E2 | The watched set: every room some colony declares, raises, garrisons or answers, shut or not | Nobody | — | New | S1 |

`harassCasters` (ADR 0081) is already "who answers a room, chosen among all colonies, once a tick": E1 copies its pattern.

**Genuinely the colony's.** Spawns, casting, bank, refillables, sources, sites, controller, errands, dismantles, consignee, perimeter and rampart floors. Creep ownership and assignments. The plan memo, Layout and Atlas. The economy latches: `ReserveShort`, `HaulOverflow`, `HomeRaided`, breaches. The halves of the Raid log about our side: losses, closest approach, damage, living, placed, hits. The declarations: which colony *mines* a room. Firing safe mode.

## Decision

**The world layer is concluded once a tick and shared by every colony. A colony owns its spawns, its economy and its bodies.**

- **Computed once, keyed by room:**
  - vision: every room seen by any creep, structure or observer of ours, for every reader;
  - each room's hostiles, control and strength;
  - the room's verdict and the core of its Brief (targets, ground, rung, verdict);
  - the danger log: the stand-down rows, rival latches, holds, threat memory and fight records;
  - the raid roster;
  - sightings;
  - the danger maps (Reach, Safe).
- **The watched set** is every room some colony declares, raises, garrisons or answers. It is world-wide, and a shut room stays in it. Shut withholds economy bodies only. It never hides a room from a conclusion.
- **The answerer.** For each room that needs an answer, the world layer picks the colony among all colonies' bodies, ranked as `harassCasters` ranks colonies: by hops, then fitness. Each combat Task is pooled by exactly one colony, so the flat `assignments` leaf never double-writes.
- **A colony's view** becomes its economy plus a read of the world layer. Its `StandDown` is a per-colony *projection* of the world danger log: whether this colony's economy bodies work room R. Matching, casting and quotas stay per colony.
- `Lendable`, `ChildOutposts`, the cross-colony `mayLend`, `Defenders`, `LoadedTowers`, `MinedRooms` and `Seen` fold into world queries and go.

## Stages

Each stage ships on its own. A refactor stage keeps the fingerprints. A behaviour stage names the fingerprints it moves and why. Quirks are kept and named in a test, never fixed in passing. Every stage's gate also holds `npm test`, `npm run build` and the arena suites.

| # | Stage | Acceptance |
|---|---|---|
| S0 | The inventory above; characterisation tests pinning each seam; the profile baseline (below) | Tests only. `npm test` clean, every fingerprint as below |
| S1 = #534 | **Shared vision, shut rooms watched.** A `WorldView` (hostiles, invader cores, fallen and control per room, built once off `World`) and the world watched set. The stand-down fold reads its rooms' hostiles and control from the world, not the view. `ChildOutposts` keeps shut rooms, gating economy only. A Kite or Win on a shut room pools the chase or guard | #534's invariants as tests. The S0 pins marked "flips at S1" flip: an observer-only look updates the log's last sighting; the shut child outpost stays in the mother's watch; the t973,320 capture has the resident called while the room is shut. The `--raided` fingerprints may move, each one named |
| S2 | **The world danger log.** B1–B4 become one record keyed by room, `observe.world.danger.<room>`, holds and threatened beside it. `rivalHeld` merges into `rooms.rivals`. `StandDown` becomes the per-colony projection. One Memory migration on the reset tick: max expiry, OR of stronghold, union of latches, min of `lastLooked` | `wire-check` cases for each new leaf and each merge. A dedicated test that a stronghold row (W16S26, `Impassable`) survives the merge. The double-latch pin flips: one latch, one stride |
| S3 | **One verdict per room.** `Facts.answered` is priced once per raided room in the world layer, over every colony's bodies. `raidDeadlines`, `strikeGone` and `Brief.Verdict` read it. The answerer (E1) is chosen there. `Lendable`, the S8-B split and the cross-colony `mayLend` go; the lender's quiet is read on the lender's side | The two verdict pins flip: one room, one verdict, and the mother+child-together raid is a Win answered by one colony. The arena W18S25 and Synapse673 cases pass. Fingerprints named |
| S4 | **World Briefs.** ADR 0085's `Threats.Briefs` becomes one world map, `Map<Task, Brief>` with an `Answerer: home`. Its colony-closing halves (`Admits`, `Asks.Wanted`, `Blocks`) split into world data and colony functions. Each colony's Planner pools the Briefs it answers. Reach and Safe are built once per raided room | Refactor: every fingerprint unchanged, every ADR 0085 S-test passes |
| S5 | **The memory left over.** `Fought`, `Taken`, `Probes` and the harassment clock are keyed by room. The episode roster splits: the room × hostile roster is world, losses/closest/damage stay with the colony. Transit-only rooms are no longer enrolled per colony. Dead colony leaves (`colonies.W11S29`, `.W11S27`, the legacy flat `observe.raids`) are pruned by code | `wire-check` cases; `observe raids` reads the new leaves. The transit-enrolment pin flips. Memory size reported against the 182 KB of S0 |
| S6 | **Observer and scan.** The observer is aimed off the world watched set. The scan set is derived once per colony. `Defenders`, `LoadedTowers`, `MinedRooms`, `Seen`, `ChildOutposts` and `Sightings`' narrowing fold into world queries | Refactor: fingerprints unchanged |

**S0 baseline** (main feaee027, 2026-10-05). `npm run profile -- --scenario <s>` Memory fingerprints. `siege` and `siege --raided` differ from ADR 0085's d0efa4f6 table by the moves its S7 and later stages recorded:

| Scenario | assignments | creeps | colonies | positions | reactor | after the reset tick |
|---|---|---|---|---|---|---|
| stub | e4c60c3d3cb7114d | 35f00ffe8c7f906f | a3d423611efef558 | ff8230b244e5ecdf | 1f090c8adb0147df | 8a99144b029db026 |
| outpost | 21f66544a80d5c58 | a18c2743ee0f57ca | 4c3c9a282e6408f4 | 6f1e3aa60259fff5 | 1f090c8adb0147df | 1c6b2942b54e68d6 |
| outpost --raided | 269ab9d0e41974f0 | c75391b59310ab78 | 4958d78f16594ca8 | 92e68517450b8401 | 1f090c8adb0147df | 499b986e8d0ea283 |
| young | 0506fdbf9471580d | a7b5fae116bb8048 | 4b7af12bcb3503db | 7cf6ab4219aa33d4 | 1f090c8adb0147df | 26b17c95fd6a3ee6 |
| pair | 209b1abf9dc3bc1d | 586b6e74f3e64b82 | fc81bdc552ff33d4 | e652ba7beed66555 | 1f090c8adb0147df | 6b4c6ba7f751eb90 |
| pair --raided | 4531438fbd527bc1 | 013616cf989f4fd2 | f3aef928624e0dc2 | ad0e1f95a390fa7d | 1f090c8adb0147df | af286497879246a2 |
| reactor | 015d2398251355cb | 8d3bb61797210080 | aab08b854a0feb8e | 168e6a57b4070c6c | 6a7d97dc543b1cfd | c72d13fc3f81ba78 |
| siege | 307c21bd810677b5 | ae7a9eff3252d9c1 | 5bd735ed8551c412 | a5deaa6e93f457a1 | 1f090c8adb0147df | 43eff62eb5e2ca44 |
| siege --raided | c6aa0a873b6c7647 | 97c157120c87e278 | b786099953c0e2d8 | 9aebc0614deba6bc | 1f090c8adb0147df | ffe09f8cc3c4d43c |

**S0's pins** (each says which stage flips it):

- `ViewTests`, "pre-world-layer seams":
  - an observer-only look into a stood-down outpost reaches the world and no view, and the owner's log records nothing (S1);
  - a child's shut outpost leaves the mother's `ChildOutposts`, and its hostile stands in no view (S1);
  - one world, one room, two verdicts: the child's rangers win it, and the mother, who cannot see them, prices a Lose (S3).
- `ObserveOutpostTests`: two colonies latch one rival room, each on a stride of its own (S2).
- `ObserveRaidTests`: a raid in a room the colony only crosses is enrolled in its episodes, and no stand-down opens (S5).
- `OutpostRaidTests`: the existing S8-B case, a raid only the child's ranger and the mother's resident win together, the child standing it down, is marked (S3).
- `ArenaTests`, `#534 capture t973,320`: Idol768 alone in shut W18S25 is answered by nobody for 85 ticks, and the stand-down stays last seen at t973,265 (S1). It reproduces live: the room-history replay shows no body of ours in W18S25 until about t973,440. With the stand-down expired the arena sends bodies in at once, so the seated row is what holds them out.

## Consequences

- **CPU.** This is not the reason for the epic. The saving is modest: about 1–3 ms of the 6–8 ms cut (the duplicate rooms' share plus the doubled scan derivation), and verdict pricing falls from up to 2 × views to 1 per raided room, which matters under a siege. The likely bigger win is Memory. The raid logs are 37% of it, and W17S29's transit rosters are parsed and stringified every tick. Any claim is an interleaved A/B with identical fingerprints.
- **The Memory migration runs on the reset tick** (S2, S5), already the most expensive tick (#442). A wrong merge reopens a stronghold room, so the merge has a dedicated test. A legacy per-colony leaf reads as absent once migrated.
- **The light tick (ADR 0082)** must read world danger. `glanceOf`'s forcing rule and `RoomRoles` are fed from the world layer. Otherwise a light tick misses a raid only the world layer saw: a shut room watched by vision alone.
- **The answerer is new behaviour.** It changes who walks where. `Recall` and the lend rule are re-derived from the lender's own world-side quiet test. "One colony pools each Task id" is an invariant with a test.
- **Fixtures move to one World plus N colonies.** Decide tests build a `ColonyView` today. That is a large but mechanical edit, and `ParallelSafetyTests` still holds: no fixture captures an Atlas. An arena capture then holds the whole picture, mother included (#532).
- **ADR 0085's Brief survives** with its Task keying. Only where it is built moves, from per colony to per world.
- **Ordering.** S1 touches `Sightings` and `Facts`, which #531 (shipped) touched too. The movement epic builds on the world layer and comes after it.

## Considered options

- **Keep patching each seam** (`ChildOutposts` keeps shut rooms, one more `Lendable`-style field per case). Rejected: every patch so far (#447, #448, #451, #484, #517, #518) smuggled one more world fact into a view, and #534 is a bug in one of them.
- **Share vision only (S1 alone).** It fixes #534, but leaves the two verdicts, the double latches and the transit rosters. Kept as the first stage, not as the end state.
- **One `decide` for all colonies over a merged view.** Rejected: ADR 0052's unit is the colony, because a colony's bank, stage and rows differ. Merging loses that, and every quota would be re-derived.
- **A world layer that also matches and casts centrally.** Rejected: the Matcher, quotas and spawns are per colony by ADR 0052 and work. The world layer stops at *who answers*, and the answering colony pools the Task.
