# A quiet tick replays the last decision instead of deciding

> **Status:** accepted

> **Accepted 2026-09-30**, the user taking every recommendation: a tick of latency outside combat, the `rangedRange` + 2 radius, and a one-to-one cadence. Out of the CPU round that took the mean tick from 86 to about 68 ms by exact cuts alone. The remaining cost is spread over dozens of sites; this is the one structural cut left that is worth double digits. The user chose it over rewriting the projection's collections (B) and caching the snapshot's static structures (C).

Every tick today builds the World (every visible room swept), projects every colony, and runs the whole decision — plan, pool, spawns, the Matcher, the mover — for a fleet that is doing what it did last tick. Measured live 2026-09-30 (t841,9xx, 20 consecutive ticks, 65 creeps): 63 of 65 creeps **kept** their Task, and 13-16 of 65 **moved** on any tick — the haulers (61% of samples), the rangers (26%), and a worker or an anchor now and then; upgraders, reservers and the garrison essentially never. The Matcher is already incremental (a holder is gated against its own Task alone), so the cost is not re-matching; it is re-deriving, every tick, a colony whose derivation has not changed.

We propose that **ticks alternate between a full tick and a light tick, for the whole shard at once**. A full tick is today's tick unchanged, plus one addition: each creep's **step plan** — the tiles of its path from where it stands toward where its Task has it going, as far as its own room's edge — is laid off the flood the mover already ran, and kept on the heap. A light tick builds no World, projects no colony and runs no decision. It re-issues the last full tick's repeatable intents and advances each walking creep one step along its plan, and it checks the few facts that would make that wrong; if any fails, the light tick hands over to a full one on the spot.

## Decisions

1. **The cadence is global and fixed**: a light tick follows each full tick, never two light ticks in a row. Not per colony: the World sweep and the snapshot (~14 ms) are shard-wide and only a shard-wide light tick skips them. Not adaptive to the bucket: ADR 0041 holds — CPU is measured, not budgeted — and a cadence is a schedule, not a budget.

2. **What a light tick re-issues**, per creep, off the last full tick's intents, and only when the creep stands on the tile it stood on then — a creep that stepped and worked on the full tick (a builder on the move) works again on the next full tick, since one step on it may be out of range:
   - the **repeatable work** intents: `HarvestSource`, `UpgradeController`, `ReserveController`, `BuildSite`, `RepairStructure`, `DismantleStructure`;
   - never an intent that shoots or heals (`AttackCreep`, `RangedAttackCreep`, `HealCreep`, `RangedHealCreep`, `FireTower`, `HealWithTower`): a target still in reach makes the tick full (4), and one that is not leaves nothing to shoot;
   - never the one-shot intents: `TransferEnergyToStructure`, `WithdrawFromStore`, `PickupPile`, `SpawnCreep`, `PlaceConstructionSite`, `ClaimController`, `ClaimReactor`, `SignController`, `SendFromTerminal`, `SayCreep`. A creep whose last intent was one of these does nothing on the light tick: it arrives, it acts on the next full tick — at most one tick late.

3. **What a light tick moves**: a creep with a step plan whose head is the tile it stands on issues `MoveCreep` toward the plan's next tile. A creep standing anywhere else (blocked, pushed, fatigued) issues nothing and waits for the next full tick. No arbitration is run: a light tick's moves are the full tick's moves one step on, and two plans that meet on one tile cost one of the two bodies a tick.

4. **What forces a full tick instead** — checked at the top of the light tick, over `Game` directly, before anything is issued:
   - an **armed** hostile creep (Attack, RangedAttack or Heal parts) other than a Source Keeper, visible in a room where we own a structure (`Game.structures`). Amended 2026-10-02 (#461), from "any room any colony projects": a rival's defenders kept standing in one harassment room forced 62 of ~100 live full ticks; an armed body elsewhere is answered by the next rule when it comes within reach, and one tick late at worst when it does not;
   - **any** hostile creep, a Source Keeper included, within `Engine.rangedRange` + 2 of any creep of ours or structure we own (`Game.structures`; an outpost's container or road is nobody's, and a body dismantling one with none of ours in reach waits a tick). An unarmed enemy miner standing off its source in a room we harass, and a keeper on its rock behind the mask, force nothing — the harassment rooms hold an enemy body nearly every tick and the keeper rooms always do, so "any hostile visible" would leave no tick light; one that comes near is a shot to decide;
   - a structure of ours fought on the last full tick: a tower fired or healed, or safe mode was raised. A creep's own shot or heal is not on this list — amended 2026-09-30, when 40 of 83 live full ticks were forced by a harassing ranger's shot at a target already dead or gone: a target still in reach is within the radius above, which forces the tick on its own;
   - any creep of ours standing on its room's border ring with no ground to step onto: the engine carries a body that ends a tick there into the neighbour, so a creep that crossed on the full tick and stood still on the light one would be carried straight back, and every crossing would bounce for ever. A creep on the ring has always just crossed in, so a light tick steps it straight in, or onto an inward diagonal, where the terrain allows (`LightTick.inward`; amended 2026-09-30, when the ring forced 10 of 71 live full ticks); only one walled on all three is forced;
   - any creep of ours that is new since the last full tick, or gone;
   - any creep of ours that lost hits since the last full tick (the heal reflexes). Not a structure: ramparts, roads and containers lose hits to decay on schedule, and a structure under attack has its attacker within reach, which the rule above already reads;
   - any controller of ours whose safe mode or level changed;
   - a global reset (the reset tick is full, as #442 left it).

   Combat is never light: every intent that shoots or heals is decided.

5. **What a light tick writes**: the CPU line (tagged light, so `observe cpu` can split the two populations) and the positions leaf, so `CreepInfo.Moved` keeps meaning "moved since the last tick" on the full tick after it — and nothing else. The Transition log, the Raid logs, the layout and quota leaves, and the assignment table are the full tick's; a light tick holds no Verdict to fold.

## Consequences

- **Expected**: a light tick costs its intents (the engine's 0.2 CPU per accepted intent, ~11 ms of today's ~105 intents) plus the checks in 4 and the step plans, a few ms; the full tick costs what it costs today plus laying the plans. At ~15 ms light and ~70 ms full, the mean falls to ~42 ms, under ADR 0041's revisit line for the first time since it was written. The **peak does not fall**: a full tick is as expensive as today's.
- **Latency**: every judgement is at most one tick older. A replayed Harvest does not look at the store: a miner that filled on the full tick and stands on no container drops one tick's energy beside it, for a hauler to pick up. A hauler that arrives on a light tick transfers a tick later; a spawn idle on a light tick casts a tick later; a body whose store fills on a light tick starts back a tick later. A threat is never a tick late, by 4.
- **Pinned by tests at the Core seam**: the light-tick planner is a pure function of the last full tick's intents and step plans and this tick's creep positions and the 4 facts, so the rules in 2-4 are unit tests; the full tick is unchanged and every existing test holds.
- **Harness**: `npm run profile` runs both kinds of tick; its `decide by colony` table reads full ticks only, and a new line reads the light ticks' mean.
- **Reversible**: one flag in `Tuning` turns every tick full again.

## Considered options

- **Per colony, alternating by colony index** (#357's shape) — rejected: it smooths the peak but skips no World sweep, so it saves the colonies' decide and not the snapshot; ~47 ms against ~42.
- **Skip only colonies in which nothing moves** — rejected on the measurement above: the haulers move on 61% of ticks, so a colony is almost never still.
- **Re-issue last tick's `MoveCreep` directions** instead of a step plan — rejected: a direction is right for one tile only; the next tile's direction is the plan's.
- **Longer cadences (one full tick in three)** — deferred: the latency in Consequences doubles and the saving on the mean is ~10 ms more; a `Tuning` number once two-tick cadence has run live.
- **Rewrite the projection's collections (B), cache the snapshot's statics (C)** — the other two cuts; B is larger and exact, C is smaller. Neither is ruled out by this one.
