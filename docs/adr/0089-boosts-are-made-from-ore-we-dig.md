# Boosts are made from ore we dig, in one lab cluster, to a demand-driven stock, and applied at spawn to declared rows

> **Status:** accepted

> **Proposed 2026-10-07** at live tick ≈ 988,877. **Accepted by the user 2026-10-07**, with the open questions settled as under "Decided" below. It supersedes the verdict of `docs/research/boosts.md` §0 (2026-10-03, "no labs this season"); that note's engine numbers stand.

## What changed since that research

1. **W12S28 and W13S28 are RCL8**: 10 labs each, not 3.
2. **U is reachable without W12S26's RCL6.** The reactor room W15S25 holds U (35k, d2) under a neutral extractor and has **no keeper lairs**. It is already our guarded courier errand (ADR 0060, 0077/0078).
3. **K and X sit in the adjacent keeper rooms**: K in W14S26 (35k, lair at 12,37 beside the rock) and catalyst X in W15S26 (70k, d3, lair 35,11). Today the keeper margin masks every rock (ADR 0060), so neither can be dug without killing keepers.
4. **Every home extractor stands on a depleted Thorium tile**: W13S28 at 42,30, W12S28 at 26,5, W15S28 at 29,12, W17S29 at 16,44. None is on the room's O or H. Each needs a rebuild (5k energy).
5. **The projection carries no boosts.** `Facts` and `Sightings` price every hostile unboosted, so a T3 raid like Kalgen's (on Meow, t968k) is underpriced by ×2–4.

## Ore within reach (Chebyshev room distance)

| Room | Ore | Amount, density | Danger | Nearest colonies |
|---|---|---|---|---|
| homes W13S28, W12S28, W15S28 | O | 70k d3 each | none | 0 |
| home W17S29 | H | 70k d3 | none | 0 |
| W12S26 / W17S25 (RCL4) | U / H | 35k d2 | none, but an extractor needs RCL6 | 0 |
| **W15S25** | **U** | 35k d2 | **no lairs**; reactor contest | W17S25 2, W15S28 3 |
| W14S26 | **K** | 35k d2 | 4 lairs | W13S28, W12S26, W15S28: 2 |
| W15S26 | **X** | 70k d3 | 4 lairs | W15S28, W17S25: 2 |
| W16S24 | U | 70k d3 | 4 lairs | W17S25 1 |
| W16S26 | O | 70k d3 | 4 lairs + an L2 invader core (loot T, T, bar: no boosts) | W17S25 1 |
| W15S35 / W15S36, W14S34 | Z / L | 35k / 70k, 15k | other sector, ~7 rooms | out of reach |

**There is no Z, L or G within reach.** The nearest Z and L are seven rooms south.

## Reachable compounds

| Tier | Inputs available | Compound: what it boosts | Value to us |
|---|---|---|---|
| **Now** | O, H, U | OH (intermediate), **UH, UH2O: ATTACK ×2/×3**, UO/UHO2: harvest | Melee: medium. Harvest: none. |
| **With K** (keeper killing) | + K | **KO, KHO2: RANGED ×2/×3**, KH/KH2O: carry | Rangers carry the war (memory, combat stats): high |
| **With X** (keeper killing) | + X | **XKHO2 ×4, XUH2O ×4** | The Kalgen-class RMA shooter, minus its TOUGH and HEAL |
| **Never this season** | needs L, Z, G | LO/LHO2 heal, GO/GHO2 tough, ZO move, ZH dismantle, GH upgrade, LH build | Heal is the one boost that beats Trep's out-heal tower gate. **Unreachable except by Odiodin creep transfer or stronghold loot.** |

**Against Trepidimous.** His towers hold fire while our in-room heal beats their damage. Ranged or attack boosts do not move that gate. They shorten fights we already win: a UH2O `25A25M` kills his 18M17A in 2 ticks; KHO2 rangers kill his raiders in transit. A boosted siege still needs heal we cannot make.

## Decision (proposed)

1. **Dig only what a demand needs, into the terminal.** Ore becomes a resource the colony names: `Resource` grows from `Energy | Thorium` to carry a mineral type. The Thorium miner, its container and its hauler tiers are reused. An extractor is rebuilt onto its room's ore. A room digs while its terminal holds less than its target (default 6k) and stops above it. Thorium keeps priority wherever a room has both.
2. **One lab cluster, in W13S28.** The Layout gains `Lab`: a stamp of 2 input labs plus up to 8 outputs within range 2, placed only while storage energy stays above a floor, because a lab costs 50k. Build 3 labs first (one full T2 chain, serial) and grow to 6 only on measured demand.
3. **One reaction plan, driven by demand.** A pure Core function reads target stocks per compound (declared in `Colony`, e.g. UH2O 3k) and the cluster's terminal. It returns the next reaction and the fills and drains it needs. Terminal sends gather the inputs, generalising the Thorium consignment's `SendFromTerminal`.
4. **Boosts are applied at spawn for declared rows only.** These are the guard sized to an exchange and home defenders (the Fight squad is retired by #564; an Assault's breachers may join later). The body walks to the output lab and is boosted before it takes a Task. A row whose compound is short spawns unboosted rather than waiting.
5. **Read hostile boosts first**, independent of the rest: `Sightings` and the Brief price boosted parts with the engine multipliers.

## Stages

| Stage | Delivers | Acceptance | CPU budget |
|---|---|---|---|
| **S0** | Hostile boosts in the projection and the Brief (no labs) | A Decide test prices a captured T3 body ×4. The arena applies `calcBodyEffectiveness` and the TOUGH reduction. | ≤0.1 ms |
| **S1** | Ore mining: `Resource` generalised, extractors rebuilt on W17S29 H and W13S28 O, dig-to-target | Live: H and O rise in both terminals and stop at target. Thorium flow unchanged. Wire-check case for any new Memory key. | ≤0.6 ms a mining room (harvest intent every 6 ticks, plus one hauler) |
| **S2** | U from W15S25: a miner on the reactor errand, hauled to a W15S28/W13S28 terminal | U reaches the cluster terminal. Reactor deliveries unbroken. | ≤0.8 ms |
| **S3** | `Lab` built kind, 3-lab stamp in W13S28, reaction planner, lab fill and drain Task | Pure planner tests: the stock reaches target and stops. Live: 1,500 UH2O made. Light ticks (ADR 0082) stay light between cooldowns. | ≤0.5 ms the colony, decided only on cooldown ticks |
| **S4** | Boost at spawn for declared rows, Memory plan leaf | A squad and a guard spawn boosted live. A shortfall spawns unboosted. | ≤0.2 ms |
| **S5 (later, own epic)** | Keeper killing for W14S26 K and W15S26 X, unmasking a rock while it is held | KO/KHO2, then XKHO2/XUH2O, in stock | priced in that epic |

**S0 as built** (no labs; no profile scenario holds a boosted body, so no fingerprint can move).

- **On the creep.** `HostileInfo.Boosts` is one `Boost` (`Unboosted | Tier1 | Tier2 | Tier3`) a part, head first, or empty for a body with none. `World.boostsOf` reads `body[i].boost` through `Boost.ofCompound`, which keeps only a compound on its part's own fight act. A harvest, build or upgrade WORK boost and a CARRY boost read `Unboosted`. It is a field and not an id-keyed map like ADR 0088's `HostileFatigue`, because every question (`healing`, `gait`, `effectiveParts`) takes the hostile and its side and nothing else; a map would have to be threaded to every reader. The real construction sites were 19, not ~320: the arena builds its `HostileInfo`s at one site.
- **Power.** `Engine.powerAt` counts the acting parts at `Boost.multiple` (×2/×3/×4 for ATTACK, RANGED_ATTACK, HEAL, MOVE, and WORK's dismantle). `HostileInfo.powerWithin`/`effectivePower` is that over the window's parts. `healing`, the heal that restores the window's parts, `gait`/`gaitWithin` (boosted MOVE: Arrival's paces, the kite, the chase), the raid's damage in `Facts.raidPrice`, `swingReaches`, the Emitter's `outDamaged`, and the tower reflex's strikes and the economy reckoning (#545) read it.
- **TOUGH.** `Engine.damageTaken` is `_applyDamage`; `Engine.damageFor` is its inverse, the damage that takes a given number of hits off. The TOUGH is charged once, as it breaks. The tower kill check settles a boosted body tick by tick (`HostileInfo.killedWithin`); the urgent-shot check and the overkill cap price `damageFor (hits + heal)`. `raidPrice` splits the armed hits into the damage that breaks their TOUGH, healed at `damageFor` on the body it costs most, and the rest one for one, and `wonAgainst` runs the clock in those two phases. The kill order's `EffectiveHits`, `kitedDown` and `homeBeaten` price the heal through it too. A heal restores the hits a strike took, the boosted TOUGH in front, so a tick of a T3 Kalgen's 576 heal costs 1,920 of our damage. Three towers' 1,800 never break him; six towers' 3,600 kill him on the third tick.
- **The arena** carries `Body.Boosts`. It prices the acts and MOVE's fatigue at `Engine.powerAt` and applies damage through `Engine.damageTaken`. The capture skeleton writes a boosted body's compounds (`boostsOn`).
- **Left out:** the Fight squad's simulation (`squadFight`, being retired by #564) still reads parts unboosted; its `kiteHolds` reads a melee raider's MOVE at its boosts.

**Energy.** Extractor rebuilds cost 10–15k. Three labs cost 150k; storages hold about 184k today (W13S28 74k). Boosting costs 20 energy and 30 compound per part: one 25-part UH2O brawler is 750 UH2O plus 500 energy.

**Clock.** About 26 days and about 560k ticks remain (season end 2026-11-01). S0 to S4 is about 7–10 agent-days under the staged loop, so the first boosted body lands around day 10–14 and S5 lands near the season's end at best.

## Consequences

- Boosts never add score (Thorium is ore-bound); the whole return is war.
- A resource beyond Energy and Thorium touches every store reader and hauler tier: S1 is the irreversible step that makes this an ADR.
- Out-healing towers is still bought with unboosted HEAL bodies.

## Decided (user, 2026-10-07)

1. **S0 first, then a minimal S1–S4.** Reading hostile boosts pays for defence on its own and goes first. The lab path is built only for the UH line (UH, UH2O: ATTACK ×2/×3), matching the melee "glue and profit" doctrine.
2. **Dig U at W15S25.** It has no keepers and sits on the guarded reactor errand route.
3. **Keeper killing is parked.** It costs too much to pay back in the ~26 days left.
4. **Asking Odiodin for L or G compounds is the user's diplomatic call.** Nothing in the code depends on it; the cluster takes them if they arrive.
5. **The labs go in W13S28** (RCL8, the largest bank). Labs are built, and reactions run, only while its storage holds at least 100k energy, so the cluster never competes with upgrading or defence.
