# A body moves by arrival time: one fatigue-and-distance map per side decides where it may step and stand

> **Status:** accepted

> **Proposed 2026-10-06** on main d3903da7 and the parked #524 (jj `zmvlsltl`). **Accepted by the user 2026-10-07**, with the five open questions settled as under "Decided" below. Builds on ADR 0086 S4/S6. It will amend ADR 0033 (Reach, Flee) and 0085 (`Brief.Avoid`, Kite ground) stage by stage, each amended ADR getting its banner in the stage that changes it. Folds in #524 and #527.

## Context

Each rule that picks a step or stand tile against hostiles has its own geometry and its own constants:

| Rule | Locator | Constant |
|---|---|---|
| Exit hold (#528, #561): a threat that vanished beside an exit is held for at that exit for a fixed clock, a guard standing on it (user, 2026-10-07: generalise) | `World.watchExits` Sightings.fs:970, `ExitHoldTicks`, `BriefKind.ExitHold` | `ExitHoldTicks` |
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
| S6 | Pursuit on evidence; `outruns` and `slowerOnPlain` retire; an unseen threat is a region grown from its last sighting by its gait, and the exit hold becomes pursuit's interception of that region (which exit, how long, which body), `ExitHoldTicks` retiring | Trep-style evader cornered or dropped within budget | one flood per chasing body, chase rooms only |

**CPU frame.** The live full tick is ~150 ms, with ~1.5 floods per moving body (#553). By #553's attribution, ~10 extra priced floods cost ~20 ms. An integer arrival flood without plan filing must come in near 0.3 ms, or S1 revisits the design. The ceiling is **≤ +3 ms per war tick (~2%), +0 on a quiet or light tick**. It is counted in floods: ~2 classes × ≤3 rooms, plus endangered or chasing bodies. Each stage is checked by an interleaved A/B with identical fingerprints.

**Order.** S0 can start now. S1 waits for world S4, and the unseen seeding for world S6. A stage deletes what it subsumes in the same change (the ADR citation cap holds).

**S0 as built** (tests only; no profile fingerprint can move). Each pin is commented `Characterisation (movement S0 …)` and names the stage that flips it.

- **Pins, one or more per rule row.** New tests:
  - Reach radius: ThreatTests "a Reach is the whole square about the hostile, off the ground too, and five ticks a tile is the cut to a step past its swing" (S2). It pins the square past the ground's edge, and 4A1M keeping +2 while 5A1M gets +1.
  - Half-tick step units: AtlasWalkTests "an unloaded ranger at a MOVE a part takes the road lane over an equally quick plain one" (S2). The road wins whichever lane is paved.
  - Avoid kinds: OutpostRaidTests "each flavour's walk-round arm" (S2–S3). Trespass gets `Reach`; both chases get `MeleeStepAndSwing`; every other flavour gets `Runner`.
  - Kite fatigue: OutpostRaidTests "the kite ground is the same for a lamed, tired ranger as for a quick one" (S3).

  Existing tests, tagged:
  - Cross-border reach: "a melee within its margin of an exit reaches the first two columns across it, and no further" (S2).
  - Flee area: "the Move Intent walks toward a safe tile, and no action is emitted", and `outDamaged` in "a ranger whose Guard went this tick, beside an 8A melee, takes Flee and steps out" (S2).
  - Kite ground: "a ranger is matched to kill it from range three, and the melee guard is not sent" (S3).
  - Kiter walk-round and exit camp: "it walks onto the ring three from the melee, inside its Reach, and shoots from there" and "the chase crosses the border at an exit three or more from the one the target left by" (S3).
  - Walk-round: "a body Flee applies to walks round a Reach to its work, never into it" and "a trespass's killer waits at the edge of a Reach that covers the kill's ground" (S2).
  - Shelter and exit band: "the ground it walks to is behind the declared line, else five in from the exits, never beside a spawn" (S4).
  - Defend line: "a melee at the rampart beside a kiter: the defender answers the melee, a ranger the kiter, each on its own ground" (S4).
  - Melee glue: LightTickTests "the melee's step onto its target never lands on an exit, …" (S3).
  - Moving blockers: ViewTests "a hostile that stepped since the last full tick also stands on the tile it left" (S5).
  - Catchability: "a kill admits no body its target outruns on every ground; equal pace on one is a chase" (S6).
  - Light-tick forcing: "a stationary hostile is near only within its weapon's range; a mobile one a step past it" (S5).
- **Captures** (ArenaTests, "arena border play"):
  - **W17S26 t986,005** (`rangerIntoMelee`, S3): reproduced. Our code gives ranger-984393-Spawn3 guard:W17S26 on its own, and it steps within two of HalfLife847 (17M17A) and is struck. The melee's first two steps are scripted to its live tiles. The pin is one 426 strike where live there were four: the arena's ranger then outwalks the melee, which live stalled on plain.
  - **#527 W18S25 t971,045** (`slowMeleeHaulers`, S2): partly reproduced. hauler-970335-Spawn11 takes Flee at each step Virtual458 takes and rematches refill:tower-13-29, and only it is pinned. hauler-969730-Spawn11 walks round at once, because the arena lays no roads; live, the road made the path past the melee the cheaper one. The tick is too old for live Memory, so the Raid log is empty and the residents later kill Virtual458, which they did not do live.
  - **Doorway t972,061** (`doorwayJam`, S4): partly reproduced. guard-970892-Spawn11 stands idle on W17S25 (1,15) for all 20 ticks, and only it is pinned. Live, hauler-971381-Spawn11 bounced on (49,15)/(0,15); in the arena it walks in, the road path again being the likely difference.
  - **#533 W18S25 t973,267** (`idleRangerInReach`, S2): cannot reproduce by design, because live it ran before the #533 fix. The pin is main's outcome: the ranger takes guard:W17S25, walks home through the melee's Reach, is struck three times, then returns, kills Idol768 and survives. S2's walk-round should keep it out of the Reach. `shutOutpostCripple` (t973,320) comes after the death and does not cover it.
  - **#551**: the existing `swayingTrespassers` (t979,950) is tagged S5. The mechanism moves there, but the outcome must hold.
  - **#558 t985,660**: #558's own capture, `haulerBesideCore`, landed with `RoomFacts.InvaderCores` in the arena. It reproduced the live core-ring park before #558's fix and pins the fixed outcome: both rangers shoot Obsidian522, then the core from range 3. It is tagged S4, where the stand-tile picker takes over the ring and must keep that outcome.
  - **Findings about the arena:** it lays no roads, which moves path choice and road gait. Its `Chase` script steps onto swamp where a live melee kept to plain, so a faithful capture scripts waypoints. An old tick seats no Raid log.
- **Flood timing.** One multi-source Dial's bucket-queue flood over a real 50×50 room (W17S25 terrain), integer costs per tile, plus a separable Chebyshev dilation by the strike range. It was compiled by Fable 5.18 to Int32Array loops and run on Node 22 (Ryzen 9950X3D), each figure averaging 5,000 runs: ~0.10 ms for 1 or 6 melee seeds (swing 1), ~0.15 ms for 6 shooter seeds (range 3), and ~0.165 ms for 20 mixed seeds. That is inside S1's 0.3 ms locally; the MMO's slower cores leave about 2× headroom.

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
