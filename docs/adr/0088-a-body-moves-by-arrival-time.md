# A body moves by arrival time: one fatigue-and-distance map per side decides where it may step and stand

> **Status:** accepted

> **Proposed 2026-10-06** on main d3903da7 and the parked #524 (jj `zmvlsltl`). **Accepted by the user 2026-10-07**, with the five open questions settled as under "Decided" below. Builds on ADR 0086 S4/S6. It will amend ADR 0033 (Reach, Flee) and 0085 (`Brief.Avoid`, Kite ground) stage by stage, each amended ADR getting its banner in the stage that changes it. Folds in #524 and #527.

## Context

Each rule that picks a step or stand tile against hostiles has its own geometry and its own constants:

| Rule | Locator | Constant |
|---|---|---|
| Half-tick step units (ADR 0010): a road is always cheaper than plain, even for an unloaded 1:1 body that walks both at one tile a tick; a ranger takes the road into a melee walking it (user, 2026-10-07) | `stepUnits` Grid.fs:103 | road 1 unit, plain 2 |
| Reach radius, stationary (#510, #521) | `reachRadius` Threat.fs:1671, `ReachMargin` Rules.fs:811, `Gait.stationary` Sightings.fs:93 | +2; +1 at ≥5 ticks/tile |
| Cross-border reach (#514) | `reachAcross` Threat.fs:1627 | same radius |
| Flee area | `Threats.Safe`, `Emitter.outDamaged` :28 | — |
| Avoid kinds | `Avoid` Threat.fs:153, `Brief.Avoid` :787 | four arms |
| Kite ground (#510, #535) | `kiteGroundIn` Threat.fs:1506 | `stepAndSwing` 2, `swampReach` 4, `escapeDepth` 3, `holdReach` 2 |
| Kiter walk-round, exit camp | `kiters`/`avoidOf` Resolver.fs:566 | step+swing |
| Kite fatigue (#524, parked) | `kiteStrikeRange`, `rangerGaitOf`, `lamedIn` | — |
| Walk-round (#472, #512) | `moveIntentFor.round` Resolver.fs:104, `Atlas.firstStepAvoiding` :2999, `stepTowardRoom` :3048 | Reach as a wall |
| Shelter, exit band (#514) | `shelterOf` Threat.fs:1653, `Atlas.shelterGround` :1046 | depth 5 |
| Defend line (#504, #552) | `defendGroundOf` Threat.fs:1579 | swing/shot |
| Melee glue (#535) | `Emitter.glueStep` :695 | range 1 |
| Moving blockers (#536, #551) | `Foreign` Views.fs:1507, Atlas `occupied` :352, `resolveRooms` :707 | last tile (`Left`) |
| Catchability (#531, #535) | `Gait.outruns` :78, `canStrike` Facts.fs:736, `swingReaches` :783 | faster on all terrain |
| Light-tick forcing (#549) | `LightTick.forcing` :618 | range + 1 step |

Room routing (`routesFor` Bold/Wary, `RivalRooms`, ADR 0074) is a room verdict and stays.

The symptoms of 2026-10-05..07, each the same missing rule:
- **W17S26 (10-07):** rangers walked into a kiting melee's face. The ring ignores our fatigue and the melee's next step.
- **#527:** haulers loop Flee/rematch past a 14A1M.
- **#533:** an idle ranger stood in an 8A reach.
- **#535:** a guard lost reach the tick after.
- **#551:** a hauler was refused by a swaying trespasser.
- **#558:** rangers parked on a core ring inside Trep's reach.
- **#552:** an exit tile no swing reaches.
- **Exit band:** bodies were killed stepping out, or idling at y=1. Doorway jam at t972,062; a worker on (1,15) at t972,159.

## Decision

**A tile is unsafe to one of our bodies when some hostile can bring a weapon to bear on it no later than the body can be on it and gone. Both sides are timed by fatigue and distance alone.**

- **Hostile map.** Per room, per hostile class (strike range × gait): one multi-source flood seeded with each body's fatigue and costed `Gait.ticks` per tile. That cost is exact, because the engine carries no fractional fatigue. Hostiles are assumed to use roads. The result, dilated by the strike range, is `A(t)`. A 0-MOVE body never arrives. Parts are read by #531's window. Against a fighter, a class counts only where it out-damages that body's heal (#533's reading); against a non-fighter, every armed class counts.
- **Our map.** `T_b(t)` by live gait (`rangerGaitOf` generalised) and current fatigue. Built only for bodies where `A` is within the horizon.
- **Borders.** Floods carry through `Atlas.seams` onto landing tiles. An unseen neighbour that had hostiles at its last look (world danger log) seeds its landings as risky.
- **Readers.** Walk avoids `A ≤ T_b + held`. Flee steps to the nearest tile with `T_b < A`. Kite picks a tile within 3 of the target with `A_melee > T_b + fatigue`, one the target cannot leave range 3 from before our next shot. Stand (idle, parked, working, shelter, camp, core ring, line) needs `A > H`, off the exit band and doorways, within the work's range. Glue picks the tile that keeps contact after the target's best step. The light tick forces a full tick when a glanced hostile beats the margin recorded at the planned tile.
- **Pursuit is the mirror.** The evader's region is the tiles it reaches first and keeps range on. The pursuer shrinks it and intercepts the exit or waypoint, not the evader's tile. For melee this manufactures contact. **On evidence:** progress (region, gap), a no-progress budget, a catch rate per owner. This replaces `Gait.outruns`.
- **Horizon** `H` is the ticks to the body's next decision: 1, 2 with a light tick, 3 in Lean (ADR 0082).

**Where it lives.** Hostile maps are world facts. `WorldView` builds them lazily, once per contested room (a room where a hostile can strike, or a `Danger` room), on ADR 0086 S4's per-room Reach. Our floods are per body, kept in the Atlas memo with `Fresh.*` and bounded by live creeps. Quiet rooms pay nothing.

## Stages

| | Scope | Acceptance | CPU |
|---|---|---|---|
| S0 | Characterisation pins for every row; captures: W17S26 ranger-vs-melee, W18S25 Virtual458, doorway t972,062, #533, #551, #558 985660; time one flood | Captures reproduce live | measure |
| S1 | `Arrival`: bucket-queue flood, classes, dilation, borders, unseen risk. No readers | Unit tests (swamp, road, fatigue, 0-MOVE, border); fingerprints unchanged | flood ≤0.3 ms; quiet +0 |
| S2 | Reach, Safe, Flee and walk-round read it; `reachRadius`, `ReachMargin`, `reachAcross` and the stationary margin go; #527 is folded in; a fighter's walk is priced in whole ticks (`Gait.ticks`), ties broken toward the safer tile, not the road | #527 hauler detours and keeps its Refill; #533 pin; moved expectations named | war ≤ +2 ms |
| S3 | Kite and glue: `kiteGroundIn` constants, `avoidOf`, `MeleeStepAndSwing`, `holdReach`, #524, `lamedIn`, `glueStep`'s `struck` | W17S26: melee killed untouched; W18S25 chase and siege suites unchanged | `escapeRoom` floods go: net ≤ 0 |
| S4 | One stand-tile picker; `shelterDepth` and the exit band become map readings | Doorway, worker (1,15), #558 | ≤ +0.5 ms |
| S5 | Light-tick forcing reads the margin; `A ≤ 1` collision surcharge (Decided 4) | #549 tests; light share no worse | light +0 |
| S6 | Pursuit on evidence; `outruns` and `slowerOnPlain` retire | Trep-style evader cornered or dropped within budget | one flood per chasing body, chase rooms only |

**CPU frame.** The live full tick is ~150 ms, with ~1.5 floods per moving body (#553). By #553's attribution, ~10 extra priced floods cost ~20 ms. An integer arrival flood without plan filing must come in near 0.3 ms, or S1 revisits the design. The ceiling is **≤ +3 ms per war tick (~2%), +0 on a quiet or light tick**. It is counted in floods: ~2 classes × ≤3 rooms, plus endangered or chasing bodies. Each stage is checked by an interleaved A/B with identical fingerprints.

**Order.** S0 can start now. S1 waits for world S4, and the unseen seeding for world S6. A stage deletes what it subsumes in the same change (the ADR citation cap holds).

## Consequences

- Most per-feature constants and `Avoid` arms go, and ADR 0085's ground builders shrink.
- Danger becomes time, not a disk. S2–S4 move behaviour broadly, and each stage names its moved expectations.
- Hostile fatigue must be glanced (a new `HostileInfo` field).

## Decided (user, 2026-10-07)

1. **Ties are unsafe.** A hostile that arrives on the tick we leave makes the tile unsafe: the W17S26 ranger walked into melee reach on exactly that edge.
2. **An unseen neighbour stays risky until the next look.** Its landings are risky from the danger log's last sighting until vision (an observer or a body) sees the room again; no fixed N.
3. **The damage gate splits by body.** For a fighter, a class counts only where it out-damages the body's heal. For a non-fighter, every armed class counts, because a hauler or miner loses on any hit.
4. **#551's collision keeps `Left` plus a surcharge.** Tiles with `A ≤ 1` cost extra rather than being blocked outright, so a narrow corridor never walls us in.
5. **The budget is ≤ +3 ms per war tick.** The per-owner catch rate lives in the Memory danger log (`observe.world.danger`), so it survives a global reset; it gets a wire-check case.
