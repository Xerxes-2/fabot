# Labs and boosts: what we can make, and whether a boosted strike breaks Trepidimous

Date: 2026-10-03 (live tick ~t923,750). Read-only research.

**Sources.**
- Engine and constants: the local Screeps server's `@screeps/engine` 4.3.0-beta and `@screeps/common` 2.16.0-beta, at `~/.local/share/Steam/steamapps/common/Screeps/server/package/node_modules/@screeps/`. They are cited as `engine/…` and `constants.js:line`.
- Season rules: `screeps/mod-season5` at `da59118`, cited as `mod/…`. The official server runs it (`thorium-reactor.md`).
- Docs: docs.screeps.com, the API reference (`StructureLab.boostCreep`, `runReaction`, `StructureController.activateSafeMode`) and `defense.html` (safe mode).
- Live numbers: the API (`room-objects`, `room-terrain`, `game/time`) and `observe eval`, each labelled *(live, t…)*.

**Simulators.** These are in the scratchpad: `siege.py` (breach and walk-in), `safemode.py` (safe-mode and tap arithmetic) and `rewards.js` (a Monte Carlo of the engine's `calcReward`). Their rules are in §4.1. Anything marked **inferred** is reasoned from code and has not been seen live.

## 0. Verdict

- **Do not build labs this season.** From our own minerals, exactly one combat boost line is reachable: **UH / UH2O, ATTACK ×2 / ×3**. It needs U, and our only U sits in W12S26, which is RCL4. An extractor needs RCL6, which is about 1.6M control points away, or roughly 8–14 days at today's rate. Everything else is out of reach:
  - TOUGH (GO), HEAL (LO), RANGED (KO), MOVE (ZO) and DISMANTLE (ZH) need G, L, K or Z. We own none of them.
  - Every T3 compound needs catalyst X. We own none.
  - **H + O alone make OH, and OH boosts nothing.**
- **Boosts are not what stands between us and Trepidimous.** In the simulator:
  - Unboosted squads break W17S24's perimeter in 246 ticks for 18,300 energy, and W18S26's far line in 161–322 ticks.
  - UH2O roughly halves the breach time. Casting two more unboosted dismantlers does the same for about 7,500 energy.
  - Labs cost 50,000 energy *each* (`constants.js:203`). Three labs, 150,000 energy, buy about 40 unboosted dismantlers.
- **Safe mode does not make a decisive strike impossible. The engine gives us two levers.**
  1. **Only one room per player can be in safe mode at a time** (`engine/game/structures.js:223-225`, `ERR_BUSY`; docs `defense.html`). While W18S26 is in safe mode, W17S24 cannot activate one, and the reverse holds too. A strike that provokes one room's safe mode opens the other room for 20,000 ticks. The provoked room is then on a 50,000-tick cooldown, so it is itself unprotectable from tick 20,000 to tick 50,000.
  2. **A one-level controller downgrade sets `safeModeAvailable = 0`** (`engine/processor/intents/controllers/tick.js:63-65`). A tapper that holds W17S24's controller with 5 CLAIM drops it to RCL3 in about 16,000 ticks, roughly 0.7 days. That erases every activation W17S24 has, and Trepidimous cannot rebuy them because it owns no G.
- **Recommended next step.** Skip labs. Build the safe-mode-aware siege of W17S24 out of the squad work already queued (S1–S5, #452–#456), plus three additions:
  - a dismantler body;
  - a controller-rampart breach;
  - a tapper relay.
  
  Separately, ask Odiodin whether he will hand compounds over creep-to-creep. The engine allows it (§6.3). Even then, one boosting lab (50k) would be enough.

## 1. Inventory

### 1.1 Our minerals *(live, t923,747)*

| Room | RCL | Mineral (tile) | Density / amount | Extractor today | Can mine it? |
|---|---|---|---|---|---|
| W12S28 | 7 | **O** (11,7) | d3, 70,000, full | at 26,5, on the old Thorium tile, now depleted | Yes, after rebuilding the extractor on 11,7 (5,000 e). |
| W13S28 | 7 (10.13M/10.94M) | **O** (7,25) | d3, 70,000 | at 42,30, old Thorium tile | Yes, same rebuild. |
| W15S28 | 7 | **O** (6,40) | d3, 70,000 | at 29,12, old Thorium tile | Yes, same rebuild. |
| W17S29 | 6 | **H** (11,3) | d3, 70,000 | none | Yes, but it competes with its own Thorium (16,44), 22,000 unmined. |
| W12S26 | 4 (6,955/405,000) | **U** (41,24) | d2, 35,000 | none | **Not before RCL6.** It also competes with its Thorium (33,39), 22,000 unmined. |
| W17S25 (claim pending) | 0 | H (15,24) | d2, 35,000 | — | Needs a claim and RCL6. Its Thorium is 45,000 at d4. |

**Engine rules behind this table:**
- **One extractor per room, from RCL6:** `"extractor": {…, 6: 1, 7: 1, 8: 1}` (`constants.js:224`). In each room the extractor is either the ore or the Thorium, never both (`thorium-reactor.md` §1).
- **Densities are the standard table**, unlike Thorium's season-shrunk one: `MINERAL_DENSITY {1: 15000, 2: 35000, 3: 70000, 4: 100000}` (`constants.js:309-314`).
- **Regeneration** is 50,000 ticks after depletion (`MINERAL_REGEN_TIME`, `constants.js:297`). It is irrelevant here because none of these will run dry this season.
- **Harvest rate** is 1 per WORK per action, one action every 6 ticks (`HARVEST_MINERAL_POWER`, `EXTRACTOR_COOLDOWN = 5`, `thorium-reactor.md` §1). A `[20 WORK, 5 MOVE]` miner (2,250 e) yields **3.33 a tick**. Mining is never the bottleneck: 1,500 U takes 450 ticks.

### 1.2 Stock *(live, t923,747)*

| Room | Storage | Terminal |
|---|---|---|
| W12S28 | energy 43,437 | energy 8,575 |
| W13S28 | energy 150,252 | energy 4,811 |
| W15S28 | energy 132,220, **T 21,885** | energy 3,892 |
| W17S29 | 0 | energy 4,153 |
| W12S26 | none | none |

**We hold no base mineral and no compound anywhere.**

### 1.3 Labs

**We own no labs.** Our codebase has no lab, reaction or boost code. A grep of `src/` for lab, boost and runReaction finds nothing. Mineral mining exists only for Thorium (`Emitter.fs:626-640`).

### 1.4 What W12S26's U costs in time

- **Rate.** The claim landed at t881,062, RCL3 came at about t898k (memory `session-2026-10-01-w17s25`), and RCL4 came at about t922k. That is roughly 135,000 points in 24,000 ticks, **about 5.6 a tick**. A spot sample gave 300 points in 82 ticks (t923,747→923,829), 3.7 a tick.
- **Distance.** RCL6 needs 405,000 + 1,215,000 − 6,955 = **1,613,045 more points**.

| Upgrade rate (e/tick) | Ticks to RCL6 | Days (~21.6k ticks/day) |
|---|---|---|
| 5.6 (today) | 288,000 | 13.3 |
| 10 | 161,000 | 7.5 |
| 20 | 81,000 | 3.7 |

The 20 e/tick row means feeding W12S26 about 1.6M energy. That is five times what all our storages hold today.

**Tick rate.** 3.96 s/tick measured over a 60-second sample *(live, t923,751→923,767)*. The 28 days left are therefore about **560,000–610,000 ticks**.

## 2. Chemistry

### 2.1 Reactions and boosts (engine numbers)

**Rules:**
- `REACTIONS` is at `constants.js:483-614` and `BOOSTS` at `constants.js:616-729`.
- `REACTION_TIME` (the lab cooldown, in ticks) is at `constants.js:732-765`.
- A boost multiplies the part's power in `calcBodyEffectiveness` (`engine/utils.js:623-636`).
- A TOUGH boost scales the damage absorbed by that part's hits (`engine/processor/intents/creeps/tick.js:7-29`).

| Part | T1 (reagents → product, cooldown) | T2 (+ OH) | T3 (+ X) | Effect T1 / T2 / T3 |
|---|---|---|---|---|
| ATTACK | U + H → **UH**, 10 | UH + OH → **UH2O**, 5 | + X → XUH2O, 60 | attack ×2 / ×3 / ×4 (30 → 60/90/120 per part, structures included, `engine/processor/intents/creeps/attack.js:33-38`) |
| RANGED_ATTACK | K + O → KO, 10 | KHO2, 5 | XKHO2, 60 | rangedAttack and rangedMassAttack ×2 / ×3 / ×4 |
| HEAL | L + O → LO, 10 | LHO2, 5 | XLHO2, 60 | heal and rangedHeal ×2 / ×3 / ×4 |
| TOUGH | G + O → GO, 10 | GHO2, 30 | XGHO2, 150 | damage taken ×0.7 / ×0.5 / ×0.3 |
| WORK (dismantle) | Z + H → ZH, 20 | ZH2O, 40 | XZH2O, 160 | dismantle ×2 / ×3 / ×4 (50 → 100/150/200) |
| MOVE | Z + O → ZO, 10 | ZHO2, 5 | XZHO2, 60 | fatigue removed ×2 / ×3 / ×4 |
| (intermediate) | H + O → **OH**, 20 | — | — | **none: OH boosts nothing** |
| (ghodium) | Z + K → ZK, 5; U + L → UL, 5; ZK + UL → G, 5 | — | — | G is the reagent of GO, GH, safe-mode generation (1,000 G, `engine/processor/intents/creeps/generateSafeMode.js:19-23`) and nukes |
| WORK (harvest) | U + O → UO, 10 | UHO2, 5 | XUHO2, 60 | mineral and energy harvest ×3 / ×5 / ×7 |

### 2.2 What is reachable from O + H + U only

| Compound | Reachable? | When |
|---|---|---|
| **OH** | yes: H (W17S29) + O (any RCL7 room) | as soon as an H extractor runs. **Useless alone.** |
| **UH** (ATTACK ×2) | yes, with U | after W12S26 reaches RCL6 (§1.4) |
| **UH2O** (ATTACK ×3) | yes: UH + OH | same |
| UO / UHO2 (harvest) | yes | same; no siege value |
| KO, LO, GO, ZH, ZO and every T2/T3 above them | **no**: needs K, L, Z or G | never, from our own ore |
| G | **no**: needs Z + K + U + L | never |
| any X compound (T3) | **no**: needs catalyst X | never |

**Plainly:**
- Of the six parts that matter in a siege, only **ATTACK** can be boosted, and only to T2.
- **TOUGH and HEAL, the two that decide fights under towers, are unreachable.**
- **No T3 of any kind is reachable.**
- Trepidimous is in the same position. It mines only H (W18S26 storage holds H 76,000 and its terminal H 6,000). Its 3 labs hold energy only, and none of its 26 creeps seen today carries a boost *(live, t923,747)*.

### 2.3 Where the top 10's T3 comes from: strongholds

`mod/src/stronghold-rewards.js` replaces the core loot table. A destroyed core of level L drops the first L+1 entries of `[T, T, bar, T1, T2, T3]`, each array entry sampled at random:
- T1 is one of UH, LO, ZH, KO, OH, GO.
- T2 is one of UH2O, LHO2, ZH2O, KHO2, GH2O, GHO2.
- T3 is one of XUH2O, XLHO2, XZH2O, XKHO2, GH2O, XGHO2.

The amounts come from `engine/utils.js:677-698` `calcReward` (`engine/processor/intents/invader-core/destroy.js:29-35`). Our Monte Carlo of that code (`rewards.js`, 20,000 draws) gives these expected amounts:

| Core level | T | bars | one T1 | one T2 | one T3 |
|---|---|---|---|---|---|
| 3 | 5,533 | 3,556 | 2,218 | — | — |
| 4 | 30,351 | 18,078 | 10,606 | 6,664 | — |
| 5 | 188,771 | 112,323 | 64,726 | 38,643 | 20,005 |

**A collapse drops nothing.** `invader-core/tick.js:11-26` only resets the controller. Loot comes only from a kill (`destroy.js`).

**The nearest prize** *(live, t923,794)* is a **level-4 stronghold in W14S26 at (29,13)**. It collapses at **t972,726**, about 49,000 ticks or 2.3 days from now. On average it holds 30k T, about 10.6k of one T1 and about 6.7k of one T2. It is defended by:
- 4 towers at range 1 of the core;
- 1,000,000-hit ramparts on every tile (`STRONGHOLD_RAMPART_HITS[4]`, `constants.js:848`; template `bunker4`, `common/lib/strongholds.js:86-146`);
- its own boosted defenders.

Within 600 hits a tick of each tower at range ≤5, no unboosted squad we can cast survives (§4). **It is out of reach**, but it is the reason the top 10 hold T3.

The other cores in W13S25–W17S26 are level 0, with no loot.

## 3. Labs and throughput

### 3.1 Engine numbers

| Rule | Value | Source |
|---|---|---|
| Labs per RCL | RCL6: 3, RCL7: 6, RCL8: 10 | `constants.js:225` |
| Lab cost | **50,000 energy each** | `CONSTRUCTION_COST.lab`, `constants.js:203` |
| Lab capacity | 3,000 mineral + 2,000 energy | `constants.js:274-275` |
| Reaction | 5 units per `runReaction` (`LAB_REACTION_AMOUNT`). Both source labs must be within range 2 of the output lab. The output lab's cooldown is `REACTION_TIME[product]`. | `engine/processor/intents/labs/run-reaction.js:12, 26, 38, 56`; docs `StructureLab.runReaction` |
| Boost | **30 mineral + 20 energy per part, confirmed.** `LAB_BOOST_MINERAL: 30`, `LAB_BOOST_ENERGY: 20`. One `boostCreep` boosts every eligible part the lab can pay for. The creep must be adjacent and not spawning. | `constants.js:276-277`; `engine/processor/intents/labs/boost-creep.js:15, 23, 41-46`; docs "20 energy and 30 minerals per body part" |
| Boost order | Unless the creep's first eligible part is TOUGH, the list is reversed, so a partial boost (`bodyPartsCount`) takes the **tail** parts first. | `boost-creep.js:33-38` |

### 3.2 Rates for the one reachable chain

| Product | Cooldown | Units a tick, per output lab |
|---|---|---|
| OH | 20 | 0.25 |
| UH | 10 | 0.5 |
| UH2O | 5 | 1.0 |

**One boosted brawler `25A25M` with UH2O** takes:
- 750 UH2O = 750 UH + 750 OH = **750 U + 1,500 H + 750 O**;
- 500 energy at the lab;
- 1 lab fill.

**A two-brawler strike (1,500 UH2O)** takes:

| Lab set | Lab time |
|---|---|
| 3 labs, serial | OH 6,000 + UH 3,000 + UH2O 1,500 = 10,500 ticks (~0.5 day) |
| 6 labs | about 6,000 ticks |
| UH only (×2, no OH step) | 3,000 ticks on 3 labs |

### 3.3 A realistic plan, and when the first boosted part exists

| Step | Earliest | Cost |
|---|---|---|
| W12S26 reaches RCL6 | day 8–14 (§1.4) | 1.6M energy into its controller |
| U extractor in W12S26, which pauses its 22,000 T | +1 day | 5,000 e |
| H extractor in W17S29, which pauses its T; or an O rebuild in an RCL7 room | in parallel | 5,000 e each |
| 3 labs in one RCL7 room near storage | in parallel | **150,000 e** (W13S28 holds 150,252) |
| Haul U and H to the lab room by creep (W12S26→W12S28 is 2 hops). No terminal is needed. | +0.5 day | — |
| React 1,500 UH2O | +0.5 day | — |
| **First boosted strike** | **about day 10–16 of 28** | |
| Code (§6.2) | 7–10 agent-days, in parallel | |

**Output by season end:** a few thousand UH2O at most, enough for 5–10 boosted brawlers. All of them are ATTACK. None are TOUGH or HEAL.

## 4. Does a boosted strike break Trepidimous?

### 4.1 Simulator rules and limits (`siege.py`)

**Rules, each from the engine:**
- **Tower damage** is 600 at range ≤5, falling linearly to 150 at range ≥20 (`engine/processor/intents/towers/attack.js:32-39`; `TOWER_*`, `constants.js:245-253`). A tower shot at a creep standing on no rampart hits the creep.
- **Structure damage:**
  - dismantle is 50 per WORK (`creeps/dismantle.js:33`);
  - attack is 30 per ATTACK, times the boost (`attack.js:33-38`);
  - ranged is 10 per part.
- **Heal** is 12 per HEAL, adjacent. Damage is applied, then heal, then the death check (`creeps/tick.js:116-135`).

**Model choices:**
- Towers focus our front striker, and every healer heals it. When it dies, the next body takes the fire. This is the arena's `Shoot Nearest` taken to its worst case.
- **Towers are refilled every tick**, also the worst case. Their 50,657 + 40,417 energy is 9,000 shots, so there is no draining them.
- **Their repair** is a flat rate on the struck rampart: 0, 900 (their 9-WORK `Tension599`), or 1,800 (two of them).

**Not modelled:**
- their defender squads (`squads.md` §3.2 prices them: our 8,650 e duo wins stand-up);
- terrain detours inside the base;
- safe mode (§4.3 handles it).

**Bodies** (RCL7 cap 5,600):

| Body | Parts | Energy | Damage or heal a tick |
|---|---|---|---|
| D | `25W25M` | 3,750 | 1,250 on structures |
| B | `25A25M` | 3,250 | 750; 1,500 with UH; 2,250 with UH2O (+750 mineral, +500 e) |
| H | `18H18M` | 5,400 | 216 heal |

### 4.2 Results

**Layout** *(live, t923,747)*:

- **W17S24** (RCL4, 1 tower at 17,33, spawn 23,33, storage 23,35):
  - 22 ramparts at 603k–646k. 19 of them seal the room: x=2 y18–21, x=6 y2–3, x=24 y3–6, x30–34 y10, x43–46 y33. The other 3 enclose the controller at 9,8: (10,8), (9,9), (10,9).
  - The spawn, tower and storage are **not** under ramparts.
  - The controller is at 354,493/405,000, close to RCL5. That would bring a second tower and one more safe mode.
- **W18S26** (RCL6, towers at 18,8 and 22,8, spawn 20,12):
  - 73 ramparts at 788k–831k: 45 at x=30, 16 along y=47–48, 8 at x=1–2 by the west exits, and 4 around the controller at 11,21.
  - **Spawn, towers and storage are unramparted** there as well.

**A. Breach one perimeter rampart** (bodies lost: 0 in every row that falls)

| Squad | Energy + mineral | W17S24 west x2 (tower 300/t): repair 0 / 900 / 1,800 | W18S26 far line x30 y44 (2×150/t): repair 0 / 900 / 1,800 |
|---|---|---|---|
| 2 D + 2 H, unboosted | 18,300 | t246 / t384 / t876 | t322 / t503 / t1148 |
| 2 B + 2 H, unboosted | 17,300 | t410 / t1024 / holds | t537 / t1341 / holds |
| 2 B(UH) + 2 H | 18,300 + 1,500 UH | t205 / t293 / t511 | t269 / t383 / t670 |
| **2 B(UH2O) + 2 H** | 18,300 + 1,500 UH2O | **t137 / t171 / t228** | **t179 / t224 / t298** |
| **4 D + 3 H, unboosted** | 31,200 | **t123 / t150 / t192** | **t161 / t197 / t251** |
| 4 B(UH2O) + 3 H | 31,200 + 3,000 UH2O | t69 / t76 / t86 | t90 / t100 / t112 |

**Breaching W18S26's near line** (x30 y8, where the towers do 900 a tick) **wipes every squad in the table, boosted or not.** No HEAL boost is reachable.

**B. After the breach: walk to the towers and kill them** (3,000 hits each, unramparted, 600 each at range ≤5, refilled)

| Squad | W17S24 from (2,20): 14 steps | W18S26 from far breach (30,44): 35 steps, 21,300 walk damage | W18S26 from near breach (30,8) |
|---|---|---|---|
| 2 D + 2 H | tower dead t16, 4/4 alive | **wiped**, 1 tower standing | t14, 3/4 |
| 2 B + 2 H | t16, 4/4 | **wiped**, 2 standing | t16, 3/4 |
| 2 B(UH2O) + 2 H | t15, 4/4 | t39, 3/4 | t12, 3/4 |
| 4 D + 3 H | t15, 7/7 | t37, 6/7 | t10, 7/7 |

**Reading the tables:**
- **W17S24 is breakable unboosted today.** With 18,300 energy, the line falls in about 250 ticks, and its one tower dies about 15 ticks after the squad walks in.
- **UH2O's real effect is speed and robustness to repair.** It is worth the same as roughly two more unboosted dismantlers, about 7,500 energy per strike.
- **The one place UH2O changes an outcome** is the 4-body W18S26 far breach plus walk-in: unboosted dies, UH2O lives. Six unboosted bodies (4 D + 3 H) do it too.

### 4.3 Safe mode: the real gate, and how to beat it

**Their counts** *(live, t923,747)*:
- W18S26: `safeModeAvailable 4`. Last used at t422,871, so the cooldown is long over.
- W17S24: 3 available. Last used at t490,731.
- Both can activate now. W17S24 gains one more at RCL5 (`creeps/upgradeController.js:73`).
- Trepidimous also owns W22S28 (RCL6).
- We do not know what triggers their bot. Room history for those old activations returns 404. **Assume the worst: the room is safe-moded the moment it is struck.**

**Engine rules:**

| Rule | Source |
|---|---|
| Duration 20,000 ticks; cooldown 50,000 from activation | `constants.js:241-242`; `controllers/tick.js:25-30` |
| In a safe-moded room, our attack, rangedAttack, dismantle, heal and attackController all fail. Their towers still fire. | `attack.js:30`, `rangedAttack.js:30`, `dismantle.js:24`, `heal.js:22`, `attackController.js:25` |
| **One safe mode per player at a time.** `activateSafeMode` returns `ERR_BUSY` if any of the player's controllers is in safe mode. | `engine/game/structures.js:223-225`; docs `defense.html`: "safe mode can be active only in one room per shard at the same time" |
| Activation is refused while `upgradeBlocked > gameTime`. The CLAIM tap (`attackController`) sets `upgradeBlocked = t + 1000`. | `controllers/activateSafeMode.js:17-19`, `attackController.js:48` |
| A controller can be tapped only once per 1,000 ticks: `attackController` refuses while it is blocked. A CLAIM creep lives 600 ticks. | `attackController.js:25-27`; `CREEP_CLAIM_LIFE_TIME`, `constants.js:111` |
| Each tap removes 300 × CLAIM from `downgradeTime`. While blocked, upgrading does not restore it. | `attackController.js:41-48`; `controllers/tick.js:38` |
| **A one-level downgrade sets `safeModeAvailable = 0`** and restarts the cooldown | `controllers/tick.js:49-68` |
| Safe modes come back only by a level-up or 1,000 G (`generateSafeMode`). Trepidimous has no G route (§2.2). | `upgradeController.js:73`, `generateSafeMode.js:19-23` |
| Safe mode cannot activate when `ticksToDowngrade < CONTROLLER_DOWNGRADE[level]/2 − 5,000` | `activateSafeMode.js:20` |

**What follows (`safemode.py`):**

1. **Provoke and switch.** Breach W18S26's far line, which is cheap and costs no bodies (§4.2 A). If they safe-mode W18S26, then:
   - W17S24 **cannot** safe-mode for 20,000 ticks, about 0.9 day;
   - W18S26 itself cannot from tick 20,000 to tick 50,000, a 30,000-tick window.
   
   Each activation they spend opens the other room. At most one of their rooms is protected at any tick.
2. **Tap to zero.** Inside W17S24, during that window:
   - breach one controller rampart: (10,8) at 604k is about 240 more ticks for 2 D;
   - park a tapper on it.
   
   Each tap blocks activation for 1,000 ticks. A relay of `5CLAIM5M` tappers (3,250 e, one per ~1,000 ticks) drops W17S24 from RCL4 to RCL3 in **about 16,000 ticks**. At that moment `safeModeAvailable` goes to 0 for good. W18S26 is a harder version of the same: RCL6, 120,000 downgrade, 8 CLAIM, about 35,000 ticks (1.6 days), under two towers. It is not worth it until its towers are dead.

| Tapper | W17S24 (ttd 40,000) → RCL3, 0 safe modes | W18S26 (ttd 120,000) → RCL5 |
|---|---|---|
| 1 CLAIM | 30,768 ticks (1.4 d) | 92,311 (4.3 d) |
| 5 CLAIM | 16,000 (0.7 d) | 48,002 (2.2 d) |
| 8 CLAIM | 11,764 (0.5 d) | 35,295 (1.6 d) |

3. **Kill the spawn and tower** during the window. The tower dies about 15 ticks after the squad walks in (§4.2 B). With 600k ramparts but no spawn, no tower and no safe modes, the room is broken. What remains is an attrition of their reinforcements from W18S26 (`squads.md` §3.3: under 100 ticks).

**So: safe mode does not make a decisive strike impossible within 28 days.** Each strike costs about 1 day of window, and 560k+ ticks remain. Safe mode forces three things:
- **sequencing:** a strike lands only on a room that cannot activate;
- **pre-staging:** the squad waits at rally until the window opens;
- **a tapper relay.**

None of that needs labs.

**Counter-risks:**
- **They may hold W22S28's safe mode back.** Activating it would *also* block both of these rooms, which hurts them.
- **They may out-repair the line.** Two repairers at 900 each make the 2-D squad slow (t876), but not the 4-D squad (t192).
- **Odiodin borders W17S24.** Tell him first (memory `neighbours-diplomacy`).

## 5. Defensive value for W17S25

What we can make is **UH/UH2O on ATTACK only**:
- A UH2O `25A25M` resident deals 2,250 a hit, which kills a Trepidimous `18M17A` (3,500 hits) in 2 ticks.
- Strike-back uses the defender's boosted ATTACK (`engine/processor/intents/_damage.js:17-18` via `calcBodyEffectiveness`). Every melee that hits it off a rampart eats 2,250 back.
- Two exceptions:
  - A brawler standing *on* a rampart gives no strike-back, because the hit lands on the rampart (`attack.js:33-36`; `_damage.js:16`).
  - Kiters and healers cannot be caught by it.
- **Without U, there is nothing.**

**What W17S25's defence actually needs is out of reach.** TOUGH (GO, ×0.7 damage) and HEAL (LO, ×2) are what would let residents hold without towers. They need G and L.

`squads.md` §3.2 already shows the unboosted duo, `25M25A` + `18M18H` for 8,650 e, wins the replayed W17S25 raid stand-up with no losses. Boosts would shorten a fight we already win, and the tower-independent hold stays unbuildable. **The defensive value is low.**

## 6. Recommendation and costs

### 6.1 Build labs? No

| | Labs + UH2O | No labs: safe-mode siege, unboosted |
|---|---|---|
| Energy | 150k–300k for 3–6 labs, plus 5k per extractor rebuild, plus about 1.6M to rush W12S26 to RCL6 | about 18–31k per strike, plus 3,250 per tapper per ~1,000 ticks |
| Earliest strike | day 10–16, and only once W12S26 reaches RCL6 | as soon as the squad stages (S1–S5) plus a tapper relay |
| What it buys | about 1.8× structure damage per ATTACK body | the same breach, with 2 more bodies |
| Thorium cost | pauses W12S26's and W17S29's 22k-T extractors | none |
| Agent effort | 7–10 days (§6.2) | 3–5 days on top of the queued squad stages |

### 6.2 If the user builds them anyway, the code needed

| Piece | Rough size |
|---|---|
| Mineral mining beyond Thorium. Rebuild extractors off depleted Thorium tiles, a miner and hauler for O/H/U, and a choice between Thorium and ore per room. That choice may be an ADR-worthy trade. | 1–2 days |
| Lab cluster in the layout planner: 3 or 6 labs, all within range 2 of the 2 inputs, by storage. | 1 day |
| Lab logistics and reaction scheduler: fill inputs, `runReaction`, drain the output, keep a target stock per compound. | 2 days |
| `boostCreep` after spawning: the creep walks to the boost lab and waits. This needs a Memory leaf for the plan (a `scripts/wire-check.mjs` case). | 1–2 days |
| Squad casting with boosted bodies, plus arena support for boosts: the `calcBodyEffectiveness` multiplier, TOUGH reduction (`creeps/tick.js:7-29`) and boosted strike-back. | 1–2 days |
| Cross-room U haul, W12S26 → lab room. | 0.5–1 day |
| **Total** | **about 7–10 agent-days**, under the strict implement → 3 reviews loop (memory `rewrite-stages-strict-process`) |

### 6.3 Alternatives that do reach boosts

- **Odiodin's stock, by creep transfer (inferred, untested live).**
  - The season mod blocks only terminal sends between players (`mod/src/terminal-restriction.js`).
  - `Creep.transfer` checks only that the *sender* is ours (`engine/game/creeps.js:429-460`), and the processor has no owner check (`engine/processor/intents/creeps/transfer.js`).
  - So an ally's hauler can hand us compounds such as XGHO2, XLHO2 or XUH2O.
  - Then **one lab (50k e) at an RCL6+ room** is all we need: it is a boost station, with no reactions, no chain and no U. The T3 TOUGH and HEAL it would unlock (damage ×0.3, heal ×4) are the only boosts that change a tower fight.
  - **This is the one boost path worth a question to the user**, or through the user to Odiodin.
- **Stronghold loot.** A level-3 core is worth about 2.2k of one random T1, and level 4 about 6.7k of one T2. W14S26's level 4 is out of reach before it collapses at t972,726 (§2.3). Watch for level 1–3 cores in range; they are new loot only at level 3 and above.
- **More unboosted bodies.** 4 D + 3 H (31,200 e) matches 2 B(UH2O) + 2 H everywhere in §4.2.

### 6.4 Next step

1. Do not queue lab work.
2. Extend the squad plan (`squads.md` §5) with a **siege variant**:
   - a dismantler role (`25W25M`);
   - a rampart breach target chosen far from towers;
   - a tapper relay that holds `upgradeBlocked` on a breached controller;
   - a "strike only a room that cannot activate safe mode" gate, read off the target controller's `safeMode`, `safeModeCooldown` and `upgradeBlocked`, and off the owner's other controllers.
3. Add an arena scenario for W17S24 (capture with `--structures`, ADR 0036 as amended by #465), modelled on the W18S26 probe.
4. The user's calls:
   - whether to strike at all, and to tell Odiodin first;
   - whether to ask Odiodin for compounds.
