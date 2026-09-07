# Thorium mining and reactor scoring — the exact rules, and what 44,000 Thorium is worth

Date: 2026-09-07. All claims verified against primary sources — the seasonal rules module `screeps/mod-season5` (shallow clone today, commit `da59118`, tag 1.0.3), `screeps/engine` (`8097782`), `screeps/common` (`2fb779b`), the official Season #5 forum announcement, and the Steam news feed — unless marked **unverified**. A second class of numbers is not read but **measured today on the official seasonal server `shardSeason` over the read-only API at tick 202,041**; those are labelled *(live)*. Sister notes: `seasonal-threats-safemode.md` §2 (Season #11 = Season #5 rules, 100 CPU, no market, terminals to own terminals only, no portals), `remote-mining.md` (the Thorium deposits in W12S27/W12S28 were first spotted there).

Season #11 runs the Season #5 module. The module is open source and small — 13 files — so essentially every rule below is a verbatim read of the code the official server is running, not a paraphrase of an announcement.

## Summary

- **One extractor per room, ever.** `CONTROLLER_STRUCTURES.extractor = {1..5: 0, 6: 1, 7: 1, 8: 1}` — RCL6 unlocks it and the cap is 1 at every level. Our rooms each hold a normal mineral *and* a Thorium deposit, so **the extractor is a choice, not a pair**: Thorium or the ore, never both. There is no market this season, so it is Thorium.
- **Harvest rate is `WORK / 6` per tick.** `HARVEST_MINERAL_POWER = 1` per WORK per action, `EXTRACTOR_COOLDOWN = 5`, and the cooldown is checked in the intent pass and decremented in the object-tick pass, so successive harvests land 6 ticks apart. A `[20 WORK, 4 MOVE]` miner (2,200 e, fits RCL6's 2,300) yields **3.33 T/tick**; 22,000 T takes **6,600 harvest ticks**.
- **Thorium never regenerates, because the object is deleted.** `minerals/tick.js` would happily refill any exhausted mineral to `MINERAL_DENSITY[density]`, but the mod's `postProcessObject` hook fires first on `mineralType == 'T' && !mineralAmount` and does `bulk.remove(object._id)` — "Thorium depleted". Normal minerals still regenerate on `MINERAL_REGEN_TIME = 50000`.
- **Season #11 shrank the deposits by exactly one density step** *(live, 30 rooms sampled across latitudes)*: density 1 → 3,000; 2 → 10,000; 3 → 22,000; 4 → 45,000 — against Season #5's 10,000 / 22,000 / 45,000 / 67,000 in `mineral.roomObject.js`. The announced northward density gradient is visible but weak in the sample (N45 rooms came back 4/4/3, the S-band mostly 2/3).
- **The decay penalty is a per-tile property of Thorium sitting in a `store`, and it is a step function of the decade.** Each tick the mod sums `store.T` over every object on a tile, takes `p = Math.log10(total)|0`, and then decrements `ageTime` on every creep on that tile and `decayTime`/`nextDecayTime` on every decayable there, by `p`. A creep therefore burns **1 + p ticks of life per tick**. `p ∈ {0,1,2,3}` for anything a creep can carry (max store 2,500), and the cliffs are at 10 / 100 / 1,000 / 10,000.
- **Dropped Thorium and the untouched deposit are both free.** Dropped resources are `type: 'energy'` objects that keep the amount in `object[resourceType]`, not in a `store`, so the mod's filter (`!!o.store && !!o.store.T`) skips them; the 22,000-unit deposit keeps its amount in `mineralAmount`, likewise not a store. **Tombstones and containers do have stores and do count.** Storage and terminal count too — but they are in `OBSTACLE_OBJECT_TYPES`, so no creep can stand on them, which makes them the free place to hoard Thorium.
- **The reactor is un-killable, walkable, and stealable.** It has no `hits` field, so `attack.js` (`if(!target.hits) return`) and `dismantle.js` (`if(!C.CONSTRUCTION_COST[target.type]) return`) both bail out. It is not in `OBSTACLE_OBJECT_TYPES`. And `claimReactor` does exactly one thing — `bulk.update(target, {user: object.user})` — with no cooldown, no ownership precondition and **no reset of `launchTime`**. One `[CLAIM, MOVE]` creep adjacent for one tick takes the reactor *and inherits the streak*.
- **Score is `1 + floor(log10(1 + continuousWork))` per consumed Thorium**, and it resets only when the store empties: the first branch of the reactor's `postProcessObject` clears `launchTime` on any tick with `!object.store.T`. Break points at 9 / 99 / 999 / 9,999 / 99,999 ticks of continuous work. Consumption is exactly 1 T/tick, so **N Thorium = N reactor-ticks**, and one unbroken run of N ticks (for 9,999 < N ≤ 99,999) scores `38,889 + 5·(N − 9,999)`.
- **Every sector centre looks the same** *(live, 11 centres sampled)*: 3 sources, 1 mineral with a pre-built owner-less extractor, 1 reactor, **no controller and no keeper lairs**. Our sector's is **W15S25, reactor at (44,6), unowned and empty**. No controller means no claiming, no reserving, no safe mode, no ramparts and no towers there — and `CONTROLLER_STRUCTURES[type][0]` permits only **roads (2,500) and containers (5)**.
- **All eight rooms around a centre are Source Keeper rooms** *(live)* — four keeper lairs each. Keepers do **not** chase: `keepers/pretick.js` pins each keeper to within range 1 of its assigned source/mineral and it only attacks at melee range 1 / ranged range 3. Staying ≥5 tiles from every source, mineral and lair crosses an SK room untouched.
- **The walk is 267–283 ticks one way** *(live, Dijkstra over the 16-room block W12–W15 × S25–S28 with real terrain)*: W12S28 (26,5) → adjacent to the reactor is **267 steps** with roads and a 5-tile keeper margin, **283 move-cost / 275 steps** unroaded. Dropping the keeper margin would save ~40 steps and is not worth it.
- **Today's board is low** *(live)*: rank 1 `giaco` 74,768; only 8 of 61 players have any score at all; two reactors are owned, one of them already dry. `Xerxes_2` is rank 43 with no score. A single unbroken 44,000-tick run scores **208,894**.

## 1. The extractor and the harvest

- `CONTROLLER_STRUCTURES.extractor: {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 1, 7: 1, 8: 1}` and `EXTRACTOR_COOLDOWN: 5`, `EXTRACTOR_HITS: 500`, `HARVEST_MINERAL_POWER: 1` (https://github.com/screeps/common/blob/master/lib/constants.js). The cap is **one extractor per room at every RCL** — `checkStructureAgainstController` short-circuits on `C.CONTROLLER_STRUCTURES[type][8] === 1`, and `createConstructionSite` refuses a second one through `checkControllerAvailability`.
- Harvesting a mineral (`engine/src/processor/intents/creeps/harvest.js`, the `target.type == 'mineral'` branch) requires an extractor **on the mineral's own tile**, refuses if `extractor.user && extractor.user != object.user`, refuses if `extractor.cooldown`, then takes `calcBodyEffectiveness(body, WORK, 'harvest', HARVEST_MINERAL_POWER)` — i.e. **1 per WORK per action** — and sets `extractor._cooldown = C.EXTRACTOR_COOLDOWN`.
- **The cycle is 6 ticks, not 5.** `processor.js` runs intents (line ~318) before object ticks (line ~349+), and `extractors/tick.js` writes `cooldown: 5` at the end of the harvest tick and then decrements it once per tick. The harvest at tick *t* is followed by five blocked ticks; the next legal harvest is *t+6*.
- **A merely reserved room cannot host an extractor.** `checkControllerAvailability` derives `rcl = 0` unless the controller has both a `level` and a `user`, and `CONTROLLER_STRUCTURES.extractor[0]` is undefined ⇒ `ERR_RCL_NOT_ENOUGH`. Reservation is not enough; the room must be **owned and RCL6**.
- **Regeneration.** `engine/src/processor/intents/minerals/tick.js` sets `nextRegenerationTime = gameTime + MINERAL_REGEN_TIME (50,000)` when `mineralAmount` hits 0 and then refills to `MINERAL_DENSITY[density]`, rerolling density with probability `MINERAL_DENSITY_CHANGE = 0.05` (always for densities 1 and 4). **Thorium never gets there**: `mod-season5/src/mineral.roomObject.js` removes the object outright in `postProcessObject`. The mod's own generator (`genThorium`, run once) also skips any room without a controller, which is why the reactor rooms hold no Thorium.
- **Density table.** Season #5's `mineralDensity` is `{1: 10000, 2: 22000, 3: 45000, 4: 67000}` with cumulative probabilities `{1: .1, 2: .5, 3: .9, 4: 1}`. Season #11 uses a table one step lower *(live)*: **1 → 3,000, 2 → 10,000, 3 → 22,000, 4 → 45,000**. The mod places the deposit on a wall tile at least 5 tiles from every source/mineral/controller with at least one adjacent non-wall tile — so **the Thorium seat is a single accessible tile at the mouth of a wall**, which is what all three of our deposits look like.

### 1.1 Yield per miner

`WORK / 6` per tick, so the whole mining row is one column of arithmetic. RCL6 caps a spawn at `300 + 40×50 = 2,300` energy.

| body | cost | T/tick | ticks for 22,000 |
|---|---|---|---|
| `[5 WORK, 1 MOVE]` | 550 | 0.83 | 26,400 |
| `[10 WORK, 2 MOVE]` | 1,100 | 1.67 | 13,200 |
| `[15 WORK, 3 MOVE]` | 1,650 | 2.50 | 8,800 |
| `[20 WORK, 4 MOVE]` | 2,200 | 3.33 | 6,600 |

Even the smallest of these outruns the reactor's 1 T/tick appetite, so **mining is never the bottleneck** — the binding constraint is miner TTL under the contact penalty (§2), not throughput.

### 1.2 The only renewable Thorium: NPC strongholds

`mod-season5/src/stronghold-rewards.js` rewrites the stronghold loot tables and puts Thorium in both of them:

- `containerRewards = { T: 10, OH: 2, UL: 2, ZK: 2 }` with `containerAmounts = [0, 100, 500, 2000, 2000, 2000]`; `stronghold.js` calls `calcReward(containerRewards, containerAmounts[rewardLevel], 3)` per container, so T at density 10 yields on the order of **tens to ~200 units per stronghold container**.
- `coreRewards.normal` lists `RESOURCE_THORIUM` as the **first two** of six entries, with `coreDensities = [3, 3, 5, 9, 15, 30]` and `coreAmounts = [0, 1000, 16000, 60000, 400000, 3000000]`; `invader-core/destroy.js` builds the ruin's store from `calcReward(_.object(rewards, rewardDensities), coreAmounts[rewardLevel])`. A destroyed high-level core can therefore drop Thorium **in the thousands to tens of thousands**. **Unverified:** the exact split — `_.object` collapses the duplicate `'T'` keys, and `calcReward` randomises the division, so the per-kill amount has to be measured, not derived.

This is the only Thorium that appears after the map is generated. It is also RCL7-plus military work, so it is a later-season option, not a plan — but it is the reason a deposit-only estimate of 44,000 is a floor rather than a ceiling.

## 2. The contact penalty — what Thorium does to creeps, roads and containers

`mod-season5/src/thorium.js` is 44 lines and is the whole hazard model. Once per room per tick, at the very end of the room's processing (`processor.js` emits `processRoom` after every object tick and after `postProcessObject`):

```js
const objectsWithThorium = _.filter(roomObjects, o => !!o.store && !!o.store.T);
// … sum store.T per tile key 50*x + y …
const ttlPenalty = Math.log10(thoriumByPosition[position])|0;
for(const o of objectsInTile) {
    if(o.ageTime)       { bulk.inc(o._id, 'ageTime',       -ttlPenalty); continue; }
    if(o.decayTime)     { bulk.inc(o._id, 'decayTime',     -ttlPenalty); }
    if(o.nextDecayTime) { bulk.inc(o._id, 'nextDecayTime', -ttlPenalty); }
}
```

- **What it multiplies.** `ageTime`, `decayTime` and `nextDecayTime` are all *absolute game times*. Subtracting `p` from them every tick makes the clock run at `1 + p` ticks per tick. So a creep on a tile with `p = 3` loses **4 TTL per tick**; a road whose `nextDecayTime` is 1,000 ticks out reaches its decay event in `1000 / (1+p)` ticks.
- **The load table** (`p = floor(log10 Q)`; a creep's store maxes at 2,500 so `p ≤ 3`):

  | Thorium on the tile | p | TTL burned per tick | road decay ×  | container decay × |
  |---|---|---|---|---|
  | 1 – 9 | 0 | 1 | 1 | 1 |
  | 10 – 99 | 1 | 2 | 2 | 2 |
  | 100 – 999 | 2 | 3 | 3 | 3 |
  | 1,000 – 9,999 | 3 | 4 | 4 | 4 |
  | 10,000 – 99,999 | 4 | 5 | 5 | 5 |

- **It is a tile property, not a proximity effect.** "Standing near Thorium" in the announcement means *on the same tile*. An empty creep parked on a container holding 1,200 T ages at 4/tick; a creep one tile away ages normally.
- **What counts:** anything with a `store` — creeps, containers, storage, terminal, the reactor itself, tombstones, ruins, labs, factories. **What does not count:** dropped piles (`type: 'energy'`, amount in `object[resourceType]`, no `store` — see `_create-energy.js`) and the undisturbed deposit (`mineralAmount`). Tombstones both contribute (they have `store`) and suffer (they have `decayTime`), so a hauler that dies loaded makes the tile it died on hot until the tombstone decays.
- **Storage and terminal are the free warehouse.** Both are in `OBSTACLE_OBJECT_TYPES`, so nothing can stand on them and nothing decayable normally shares the tile. 22,000 T in storage is `p = 4` on a tile no creep can occupy — cost zero. (A road *under* the storage would decay 5× faster: 0.5 hits/tick, 0.005 e/tick at `REPAIR_COST = 0.01`. Noise.)
- **Container upkeep, concretely.** `CONTAINER_DECAY = 5000` hits per event; the interval is `CONTAINER_DECAY_TIME_OWNED = 500` in an owned room and `CONTAINER_DECAY_TIME = 100` otherwise. With penalty `p` the interval divides by `1+p`:
  - mine container in our own room, held under 1,000 T (`p = 2`): 500/3 ≈ 167 ticks ⇒ 30 hits/tick ⇒ **0.3 e/tick**.
  - buffer container beside the reactor (no controller ⇒ 100-tick base), holding under 1,000 T (`p = 2`): 100/3 ≈ 33 ticks ⇒ 150 hits/tick ⇒ **1.5 e/tick**, and 250,000 hits gone in ~1,700 unrepaired ticks.
  Roads are noise by comparison: `ROAD_DECAY_AMOUNT 100 / ROAD_DECAY_TIME 1000` = 0.1 hits/tick base, 0.4 at `p = 3`.
- **Acceptable hauler loads.** Over a loaded leg of `L` ticks the TTL bill is `L·(1+p)` and the cost per Thorium is `L·(1+p)/Q`. With the measured `L ≈ 275`:

  | load Q | p | TTL for the loaded leg | TTL per Thorium |
  |---|---|---|---|
  | 999 | 2 | 825 | 0.83 |
  | **1,000** | **3** | **1,100** | **1.10** |
  | 1,500 | 3 | 1,100 | 0.73 |
  | 2,500 | 3 | 1,100 | 0.44 |

  **The decade cliff is the whole design.** One extra Thorium at 999 → 1,000 costs 275 ticks of life. Since the reactor's store caps at 1,000 anyway, **999 is the load**: `CREEP_LIFE_TIME = 1500`, minus ~40 ticks to reach the mine, minus 825 for the loaded leg, leaves ~635 — enough to walk most of the way home to be recycled, and **not** enough for a second loaded trip. Plan on **one delivery per hauler**.

## 3. The reactor

- **Placement.** `genReactors` (a once-only cron in `reactor.roomObject.js`) selects `db['rooms'].find({_id: {$regex: '5[NS]\\d?5$'}})` — the sector centres — and inserts a reactor at `utils.findFreePos(room, 0)`, i.e. a random free tile, with `store: {}` and `storeCapacityResource: {T: 1000}`. Position is not derivable; scout it.
- **Live census** (tick 202,041, `/api/game/room-objects`): W15S25 (44,6) unowned/empty ← **ours**; W15S35 (23,13) unowned; W5S25 (8,29) unowned; W15S15 (38,39) owned by `57168c9f…`, empty; W25S25 (17,15) owned by `giaco`, `store.T: 0`, `launchTime: null` (dry — his streak is already broken); W5S15 (36,32) owned by `6a6d8e05…` with 112 T and `launchTime 201242` (≈800 ticks of continuous work, scoring 3/tick); W5S35, W25S15, W25S35, W15S5, W25S5 unowned. **Six of eleven sampled reactors are unclaimed after 200,000 ticks.**
- **Claiming.** `creep.claimReactor(target)` requires: the creep is yours, not spawning, has at least one CLAIM part with `hits > 0`, and the target is a `Reactor` at Chebyshev distance ≤ 1. The processor side re-checks adjacency and the CLAIM part, then does **only** `bulk.update(target, {user: object.user})`. There is **no cooldown, no requirement that the reactor be unowned, and `launchTime` is untouched** — an attacker who steals a 40,000-tick streak keeps scoring 5/tick off *our* Thorium from the tick he lands. Symmetrically, **we can steal it straight back and the streak survives**, because continuity depends only on the store never emptying, not on who owns it. `respawnUser` is the only path that unsets both `user` and `launchTime`.
- **Indestructible.** The reactor object has no `hits`, so `attack.js` returns on `if(!target.hits)`, `dismantle.js` returns on `if(!C.CONSTRUCTION_COST[target.type])`, and a rampart cannot be placed over it (no controller in the room). The only contest is over ownership. A resident `[CLAIM, MOVE]` creep (650 e, `CREEP_CLAIM_LIFE_TIME = 600`, so ~325 useful ticks after a 275-tick walk ⇒ ~2 e/tick amortised) calling `claimReactor` whenever `!reactor.my` is the cheapest possible guard.
- **Transfer in, never out.** `transfer.js` is generic: `capacityForResource(reactor, 'T')` returns `storeCapacityResource.T = 1000`, so a creep adjacent to the reactor transfers Thorium into it up to 1,000, and any other resource type gets capacity 0 and is refused. Withdrawal is blocked by the mod's `preProcessObjectIntents` hook, which nulls a `withdraw` intent whose target is a reactor and whose resource is T. There is no ranged deposit — the creep must be adjacent (or standing on it; the reactor is not an obstacle, though a creep parked on it eats `p = 3` from the reactor's own store).
- **Consumption and score** (`postProcessObject`, verbatim order):
  1. `if(!object.store.T && object.launchTime)` → delete `launchTime`, return. **One dry tick resets the streak.**
  2. `if(object.user)` and `object.store.T` → set `launchTime = gameTime` if unset, `store.T -= 1`, and `bulkUsers.inc(object.user, 'score', 1 + Math.floor(Math.log10(1 + gameTime - object.launchTime)))`.
  An **unowned** reactor consumes nothing and scores nothing, but also keeps whatever Thorium it holds — parking Thorium in an unclaimed reactor is a legitimate way to stage a run.
- **Score ladder** (`cw = gameTime − launchTime`, starting at 0):

  | continuous ticks | score/tick | cumulative at the end of the band |
  |---|---|---|
  | 0 – 8 | 1 | 9 |
  | 9 – 98 | 2 | 189 |
  | 99 – 998 | 3 | 2,889 |
  | 999 – 9,998 | 4 | 38,889 |
  | 9,999 – 99,998 | 5 | 488,889 |
  | 99,999 + | 6 | — |

  For `9,999 < N ≤ 99,999`: **total = 38,889 + 5·(N − 9,999)**. Past 10,000 ticks the marginal Thorium is worth **5 score**, and **each break costs ≈ 11,106 score** (the next 9,999 ticks earn 38,889 instead of 49,995) — about 2,200 Thorium's worth.
- **The reactor room is not a normal room.** No controller *(live: 6 objects — 3 sources, 1 mineral, 1 owner-less extractor, 1 reactor)*, therefore: cannot be claimed or reserved, no safe mode, no towers, no ramparts, no spawn. `createConstructionSite` passes the `controller.level > 0 && !my` gate (there is no controller) and then `checkControllerAvailability` with `rcl = 0` allows exactly **roads (2,500) and containers (5)** — and the count includes other players' structures. The pre-placed extractor is owner-less, so `harvest.js` would let anyone mine that room's U; irrelevant with no market.
- **Getting in.** All eight neighbours of a centre are Source Keeper rooms *(live: 4 keeper lairs and 4 keeper creeps each in W15S24, W15S26, W14S25, W16S25)*, so **every approach crosses one SK room**. `keepers/pretick.js` binds each keeper to `memory_sourceId` and moves it to range 1 of that source/mineral; it attacks only at melee range ≤1 and ranged ≤3 and never pursues. **A route that keeps ≥5 tiles from every source, mineral and lair is safe.** W16S25 additionally holds a level-2 invader stronghold (core at (9,5), 2 towers, 9 ramparts) — route around it.

## 4. Delivery

- **Terminals are useless for this.** `TERMINAL_CAPACITY 300000`, `TERMINAL_MIN_SEND 100`, `TERMINAL_COOLDOWN 10`, and the cost is `Math.ceil(amount * (1 - Math.exp(-range / 30)))` energy (`utils.calcTerminalEnergyCost`; `TERMINAL_SEND_COST: 0.1` is a dead constant), where `range` is the Chebyshev room distance with world wrap. Season #11's `terminal-restriction.js` nulls any `send` whose target terminal has a different `user`. The reactor room has no controller, so it can never hold a terminal of ours — the only use would be shortening the walk by owning a room near the centre: W12S28 → W14S25 is range 3 ⇒ `ceil(0.0952 · amount)` ≈ **96 energy per 1,000 T**, near-free. That needs a third owned room; we are at **GCL 2** (`gcl 1,823,489`, `calcNeededGcl(3) = 1e6·2^2.4 = 5,278,032`) and already own two.
- **So it is creeps, 6 room transitions.** W12S28 → W12S27 → W12S26 → W12S25 → W13S25 → W14S25 (SK) → W15S25. Measured with real terrain: **267 steps roaded with a 5-tile keeper margin, 283 move-cost / 275 steps unroaded**; 226 steps if the margin were ignored.
- **A hauler that fits RCL6** (2,300 e): `[20 CARRY, 10 MOVE]` = 1,500 e, 1,000 capacity, fatigue 20 vs 20 decrement ⇒ 1 tile/tick **on roads only**; off-road use `[20 CARRY, 20 MOVE]` = 2,000 e. Load 999 (§2), one delivery, then walk home to be recycled. ≈ **1.5 energy per Thorium delivered**.
- **The cadence is lazy.** The reactor burns 1 T/tick and holds 1,000, so a 999-T delivery is needed roughly **once per 1,000 ticks** against a 275-tick trip — one hauler in flight at a time with 700 ticks of slack. A buffer container at the reactor costs 1.5 e/tick (§2) and is not needed at that cadence.

## 5. Measuring the score

- **API.** `mod-season5/src/scoreboard.js` registers `GET /scoreboard/list` on the backend router, i.e. **`https://screeps.com/season/api/scoreboard/list?limit=<≤20>&offset=<n>[&search=<name>]`** — `limit > 20` is rejected outright. It returns `{ok, users: [{username, badge, score, rank}], meta: {length}}`, sorted by `rank`. A companion `updateRanks` cron re-sorts every 60 s by `score` then `gcl`; users with zero score get a `rank` but **no `score` field**. Verified live with our own token: `Xerxes_2` is `rank 43`, no score.
- **In game.** The mod exposes `FIND_REACTORS = 10051` and `LOOK_REACTORS = "reactor"`, and the `Reactor` prototype carries `store`, `owner`, `my` and **`continuousWork` (= `time − launchTime`, 0 when idle)**. That is how the bot watches its own streak — but only with vision in W15S25. There is no in-game object exposing the season score itself; use the API. **Unverified:** whether the official server exposes any additional season-score endpoint beyond `/scoreboard/list`.
- **Live board** (tick 202,041): giaco 74,768 · CrAzYDubC 73,699 · Odiodin 63,574 · squisher 62,259 · zkl2333 30,132 · olek_PL 14,328 · Karmo 4,853 · manbantoo 672 — and nothing below. 61 registered users, 8 with a score.
- **Reading the board backwards.** 74,768 score is what ~15,000–19,000 Thorium buys at an average of 4–5/tick — i.e. the leader has already burned most of two or three rooms' deposits, and *(live)* his reactor is currently dry, so he is paying the ~11,106 restart toll each time he refills. Nobody on this server is yet running the one-long-streak strategy.

## Appendix — sources read

Primary code, shallow-cloned and read locally today: `screeps/mod-season5` `da59118` (tag 1.0.3, https://github.com/screeps/mod-season5) — `src/thorium.js`, `src/reactor.roomObject.js`, `src/creep.claimReactor.js`, `src/mineral.roomObject.js`, `src/terminal-restriction.js`, `src/stronghold-rewards.js`, `src/scoreboard.js`; `screeps/engine` `8097782` — `src/processor.js`, `processor/intents/creeps/{harvest,transfer,drop,attack,dismantle,_die}.js`, `processor/intents/{extractors,minerals,roads,containers,tombstones,energy,terminal}/tick.js`, `processor/intents/creeps/keepers/pretick.js`, `src/utils.js`, `src/game/rooms.js`; `screeps/common` `2fb779b` — `lib/constants.js`. First-party announcements: the Season #5 forum post (https://screeps.com/forum/topic/3277/season-5-is-open) and the Steam news feed (https://steamcommunity.com/app/464350/allnews/), the latter quoted at length in `seasonal-threats-safemode.md` §2. Live measurements: the official season API (`https://screeps.com/season/api/...`, endpoints `game/room-objects`, `game/room-terrain`, `game/time`, `auth/me`, `scoreboard/list`) at tick 202,041.

**Standing caveat.** The official MMO backend is closed; `mod-season5` and `screeps/engine` are the published sources the official server is built from, and the module's `index.js` even hints at a private `./official-specific` shim it loads in a `try/catch`. Everything above is "verified in the published source, corroborated live where the API can see it" — the live census agrees with the code everywhere it was checkable (reactor placement regex, store capacity 1,000, density table shifted one step, owner-less centre extractors, keeper-lair geography).

## 6. What it means for fabot

*(live, tick 202,041)* W12S28 is **RCL5**, 258k energy in storage, 22,000 T at (26,5); W13S28 is **owned, RCL4**, 22,000 T at (42,30); W12S27 is **reserved only**, 22,000 T at (24,16). An extractor needs an **owned RCL6** room, so W12S27's share is out of reach at GCL 2 — the stock is **44,000, not 66,000**. As one unbroken run that scores **208,894**, against 175,576 split into four runs and 74,768 for today's leader. Marginal Thorium is worth 5 score; one dry tick costs ~11,106. Timing is not the constraint — 1.85M ticks remain, the run needs 44,000 — so optimise for **never breaking**.

1. **The extractor seat.** One extractor per room, on the Thorium not the ore; the deposit's wall mouth forces the seat. Post-like container seat with a repair row (0.3 e/tick while held under 1,000 T), or a 0-CARRY miner dropping on bare ground (`p = 0`, full life, pile bleeds `ceil(A/1000)`/tick)?
2. **The mining row and its churn.** Miner life is set by what sits under it: `p = 2` ⇒ 500 ticks ⇒ ~14 miners/room (~31k e); `p = 1` ⇒ 750 ⇒ ~9 (~20k e); `p = 0` ⇒ 1,500 ⇒ ~5 (~11k e). Buy miner life with hauling frequency; park the stock in **storage**, which is free.
3. **The delivery row.** Load **999**, never 1,000: one delivery per hauler, 275 ticks, ~825 TTL, ~1.5 e per Thorium, one in flight per ~1,000 ticks. The route holds ≥5 tiles off every source, mineral and lair in W14S25 and avoids the W16S25 stronghold.
4. **Claiming and guarding.** Claim W15S25 only once the store can be fed forever; a resident `[CLAIM, MOVE]` re-claimer (~2 e/tick) is the whole defence.
5. **The gate.** All of it waits on **W12S28 reaching RCL6**; W13S28's deposit waits on its own.
