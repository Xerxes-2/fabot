# Defending a remote against NPC invaders — what the engine forces, what five mature bots do, and what a defender costs

Date: 2026-09-07. All claims verified against primary sources (the open-sourced server code of `screeps/engine`, `screeps/common` and `screeps/backend-local`, and the bots' own repositories) unless marked **unverified**. Everything below was read from local shallow clones taken today, default branches: `screeps/engine` 8097782, `screeps/common` 2fb779b, `screeps/backend-local` 9d07928; Overmind 5eca49a (2019-06-14), bonzAI 4a0006d (2017-09-24), The International 7e5106e (2024-08-27), Quorum f6868c5 (2020-03-16), TooAngel 87645a0 (2026-08-17), Jon Winsley `glitchassassin/screeps` a835bbc (2023-07-28, last public snapshot). Every commit matches the ones `remote-mining.md` read, so the two documents quote the same code.

Motivating question: ADR 0033 gives a creep in a Reach one answer — run — and ADR 0043 gives an outpost one answer — stand down until a tick read off the threat, or 2,500 ticks if no tick is readable. That ADR says in as many words that it "decides no fighting" and that a remote guard is its own ticket. This is the reading for that ticket. Sister documents: `seasonal-threats-safemode.md` (invader trigger, boosts, safe mode), `remote-mining.md` §1.4/§6 (the 100,000 goal, the community's abandonment triggers), `remote-candidates.md` (our five candidate outposts and their net energy).

## Summary

- **Nine raids in ten are a single melee invader that walks straight at you.** `createRaid` (`backend-local/lib/cronjobs.js`) leaves `count = 1` unless `Math.random() > 0.9`, and the first creep of any raid in a non-centre room is `subtype = 'Melee'`. `smallMelee` is `[TOUGH,TOUGH,MOVE,MOVE,MOVE,MOVE,RANGED_ATTACK,WORK,ATTACK,MOVE]` — 1,000 hits, 40 damage a tick at range 1, 10 at range 2–3 — and because it has an ATTACK part `findAttack.js` never routes it into `flee.js`. It closes with you. Only groups (10%, of which the 2–5 case is 2%) contain the kiting `smallRanged` and the `smallHealer`.
- **In an unowned room an invader never suicides and never leaves.** `findAttack.js` ends with `if(!unreachableSpawns.length && roomController && roomController.user) { suicide }`. A remote's controller has no `user` — reservation is a different field — so with no hostile creeps to chase and no spawn to head for, the invader emits no intent at all and stands where it is for the rest of its 1,500-tick `ticksToLive`. **Waiting one out is bounded by 1,500 ticks, not by 2,500** — and `ticksToLive` is readable on hostile creeps (`engine/src/game/creeps.js`: `ticksToLive: (o) => o.ageTime ? o.ageTime - runtimeData.time : undefined`, ungated by ownership), which is exactly the "deadline read off the threat" ADR 0043 asks for and did not have for this case.
- **They do eat roads and containers, but only on the tile they are about to step on.** The tail of `findAttack.js` dismantles or attacks any `CONTROLLER_STRUCTURES` object (road and container both qualify) standing at `memory_move.path[0]`, and only for a creep with ATTACK or WORK — i.e. only `smallMelee`, at 50 hits a tick (100 boosted `ZH`). They never `withdraw`, never `pickup`, never `harvest`; there is no container-camping behaviour anywhere in `engine/src/processor/intents/creeps/invaders/`.
- **The exit gate is a controller test on the neighbour, and three of our five candidates can never pass it.** `checkExit` rejects an exit only when the neighbouring room has a controller that is owned or reserved (`if(controller && (controller.user || controller.reservation)) return q.reject()`). A highway has no controller at all, so the `if` is false and the exit stays valid forever. W11S28 borders W10S28, W12S29 borders W12S30 and W13S29 borders W13S30 — all highway (`remote-candidates.md` §1). **Reserving neighbours can close the two home rooms; it can never close those three outposts.**
- **Melee is 4.6× the damage per energy of ranged, and that decides the body at RCL4–5.** ATTACK 30 damage for 130 energy with its MOVE = 0.231 dmg/e; RANGED_ATTACK 10 for 200 = 0.050 dmg/e; HEAL 12 for 300 = 0.040 hp/e (`BODYPART_COST` and the `*_POWER` constants in `common/lib/constants.js`).
- **Overmind and bonzAI independently converged on the same 750-energy unit**: `[TOUGH, ATTACK, ATTACK, ATTACK, MOVE, MOVE, MOVE, MOVE, MOVE, HEAL]` (`Overmind/src/creepSetups/setups.ts` `CombatSetups.broodlings.default`) and `configBody({tough: 1, move: 5, attack: 3, heal: 1})` (`bonzAI/src/ai/missions/BodyguardMission.ts` `getBody`). Ten parts, 1,000 hits, 90 damage and 12 self-heal a tick, 1:1 MOVE. Repeated to the bank: **one unit at W13S28's 1,300, two at W12S28's 1,800.**
- **Winsley and TooAngel independently converged on the other 500-energy unit**: `[RANGED_ATTACK, MOVE, MOVE, HEAL]` (`winsley/src/Minions/Builds/blinky.ts` `buildBlinky`, for energy ≥ 1000) and `layoutString: 'MRH'` with `amount: {1: [2,1,1]}` (`screeps/src/role_defender.js`), which expands to the identical M,M,R,H. Safer, and strictly worse at this budget: three segments (1,500 e) deal 30 damage a tick, which **cannot out-damage even an unboosted `smallHealer`'s 60 hp/tick**.
- **The cheapest answer that keeps the income is a guard, by an order of magnitude.** At W12S27 (net +6.58 e/tick, a raid every ~10,000 ticks) ADR 0043's 2,500-tick fallback forgoes **16,450 energy**; a 1,500-energy guard is **11× cheaper**. At W13S29 (net +14.24 e/tick, a raid every ~5,000 ticks) the same stand-down forgoes **35,600 energy — half of everything the outpost earns between raids** — against a 750-energy guard, **47× cheaper**.
- **Reserving the exit neighbours purely for the gate is the expensive answer.** W12S27's three non-home neighbours cost ~3.5 e/tick in reservers (`remote-candidates.md` §3), or ~35,000 energy per 10,000-tick raid cycle, against 1,500 for a guard — **23× worse**, and it buys nothing for the three outposts that border highways.
- **No bot implements the exit gate deliberately.** A grep across all six repositories finds exactly one mention, and it is a comment: `// maybe room is surrounded by owned/reserved rooms and invasions aren't possible` (`Overmind/src/intel/RoomIntel.ts:311`). Nobody reserves a room to suppress spawns.
- **"Invader bait" has no implementation either** — zero hits for `bait` across the six bots. It is folklore, not code.
- **Ramparts, walls and towers are impossible in a remote, confirmed in the engine, not inferred.** `CONTROLLER_STRUCTURES.rampart`, `.constructedWall` and `.tower` have no key `0`, and `utils.checkControllerAvailability` sets `rcl = 0` unless `roomController.level && (roomController.user || roomController.owner)` — a reservation does not count — so `structuresCnt < undefined` is false and `createConstructionSite` returns `ERR_RCL_NOT_ENOUGH`. Only `road` (2500) and `container` (5) have a level-0 allowance. Towers are additionally per-room by construction: `intents/towers/attack.js` resolves its target as `roomObjects[intent.id]`, the tower's own room table.
- **The one rule fabot already shares with the community is the flee split.** TooAngel sets `roles.carry.flee = true` and `roles.sourcer.flee = false` (with a TODO dated 2016 saying it should be true) — haulers run, the work-heavy miner stands. Overmind's `Room.fleeDefaults` selects hostiles by `getActiveBodyparts(ATTACK) > 0 || getActiveBodyparts(RANGED_ATTACK) > 0`, **the same part test ADR 0033 uses to define a Threat**.

## 1. Invader mechanics that matter for defence

### 1.1 Trigger and timetable

`seasonal-threats-safemode.md` §1 and `remote-mining.md` §1.4 already verified the whole chain; the parts a defender design needs are these. `genInvaders` runs every 5 real minutes (`cronjobs.js:18`), skips any room that still holds a `user: '2'` creep, requires `_.sum(sources, 'invaderHarvested') >= room.invaderGoal || INVADERS_ENERGY_GOAL (100000)`, requires a live `invaderCore` with `level > 0` somewhere in the sector regex, and requires at least one exit passing `checkExit`. After a raid the goal is re-rolled to `floor(100000 × (rand×0.6 + 0.7))` = 70k–130k, with a 5% chance of ×2 and 5% of ×0, and every source's `invaderHarvested` is zeroed.

The counter is fed by *our* mining (`intents/creeps/harvest.js`), so the timetable is ours to compute: a reserved single-source outpost at 10 e/tick trips at **~10,000 ticks**, W13S29's two sources at 20 e/tick at **~5,000**, and thereafter every 7,000–13,000 / 3,500–6,500 ticks. **Both of our declared and candidate outposts sit inside the W16S25 stronghold's sector**, so the core gate is open until tick 249,241 (`remote-candidates.md` §4).

One caveat carried over unchanged: the room set is `db.rooms.find({status: 'normal', _id: {$nin: activeRooms}})`, and `ACTIVE_ROOMS` is only ever `sadd`-ed in the published code — the clearing lives in the closed-source main loop. The practical meaning of that filter on the official server is **unverified**; nothing here depends on it.

### 1.2 Composition, by strength

`createCreep` picks `controllerLevel && controllerLevel >= 4 ? 'big' : 'small'`, and `createRaid`'s first argument is `controller && controller.user && controller.level` — falsy in every unowned room. **A remote is raided by `small` bodies forever, whatever our own RCL is.** All three are 10 parts, 1,000 hits, `ticksToLive: 1500`:

| body | parts | output | note |
|---|---|---|---|
| `smallMelee` | 2 TOUGH, 5 MOVE, 1 RANGED_ATTACK, 1 WORK, 1 ATTACK | 40 dmg at range 1, 10 at 2–3; 50 dismantle | always the first creep of a raid outside sector centres |
| `smallRanged` | 2 TOUGH, 5 MOVE, 3 RANGED_ATTACK | 30 dmg at ≤3 | kites, see §1.3 |
| `smallHealer` | 5 MOVE, 5 HEAL | 60 heal at range 1, 20 rangedHeal at ≤3 | the only thing that makes a raid hard |

Sizing: `max = 1, count = 1, boostChance = 0.5`; `Math.random() > 0.9` raises `max = 2`, a nested `> 0.8` raises `max = 5`, then `count = floor(rand×(max-1)) + 2` capped by the number of free exit tiles. So **~90% one creep, ~8% two, ~2% three to five**. For `i == 1` the subtype is Ranged or Healer on a coin flip, so a healer appears in roughly half of the group raids — call it **5–6% of all raids**.

Each creep is independently boosted with probability 0.5, T1 outside sector centres: `attack→UH` (×2), `ranged_attack→KO` (×2), `heal→LO` (×2), `work→ZH` (×2), `tough→GO` (damage ×0.7). A boosted `smallMelee` hits for 80 a tick; a boosted `smallHealer` heals 120.

### 1.3 Behaviour

`pretick.js` splits on `_.some(creep.body, {type: HEAL})`: healers run `healer.js`, everything else runs `findAttack.js`, and both then run `shootAtWill.js`, which fires `rangedAttack` at the **lowest-hits** hostile within range 3.

`findAttack.js`, in order: a body with RANGED_ATTACK but **no** ATTACK calls `flee(creep, 3)` first and kites; a body with ATTACK attacks anything adjacent and then chases the closest hostile creep by path, re-pathing three times with progressively looser rules (ignore creeps → ignore ramparts → `ignoreDestructibleStructures`). Every `moveTo` carries `maxRooms: 1`: **invaders cannot leave the room they spawned in.** Healers heal the most-damaged invader within 3, flee at range 4 below half hits, and suicide only when no other invader exists at all.

Two consequences that change the design:

1. **The lone-invader case is a melee that comes to you.** No kiting, no cornering problem, no chase. A stationary guard wins by standing still.
2. **The empty-room case costs 1,500 ticks, not forever.** With every creep withdrawn, `findAttack` finds no target, finds no unreachable spawn (a remote has none), and cannot reach its suicide branch because `roomController.user` is null. It idles until TTL. Its `ticksToLive` is readable while we still have vision — which is precisely The International's abandonment clock (§2).

Structures: only the `memory_move.path[0]` tile, only for ATTACK/WORK bodies, only `CONTROLLER_STRUCTURES` types other than `spawn`. Roads (5,000 hits) and containers (250,000) both qualify. A boosted melee needs 50 ticks to break a road tile and 2,500 to break a container — real, but slow.

### 1.4 Cores and strongholds

Unchanged from `remote-mining.md` §1.5 and ADR 0043: `spawnStronghold`/`selectRoom` skip reserved rooms, `expandStronghold` tests only `!controller.user` and so lands in ours; a level-0 core spawns nothing (`create-creep.js` returns early), has `INVADER_CORE_HITS = 100000`, and drains our reservation by `INVADER_CORE_CONTROLLER_POWER × CONTROLLER_RESERVE = 2` a tick against a 2-CLAIM reserver's 2. **A core is a different problem from an invader creep and wants a different answer** — a dismantler, or ADR 0043's clock. Winsley is the only bot with a dedicated one (`KillCoreMission`, a single GUARD built from `[ATTACK, MOVE]` segments, `MissionStatus.DONE` when the guard dies); The International sizes `remoteCoreAttacker = invaderCore.length * 8` with `extraParts = [ATTACK, MOVE]` at `cost = 130`.

### 1.5 The exit gate, exactly

```js
function checkExit(roomName, exit) {
    ... return db['rooms.objects'].findOne({room: newRoomName, type: 'controller'})
        .then(controller => { if(controller && (controller.user || controller.reservation)) return q.reject(); })
}
```

Rejected exits are pulled from the list; if none survive, `continue` — no raid at all. The test is on the **neighbour's** controller, it accepts any reservation including ours, and **a room with no controller (every highway, every sector centre) always passes**. `remote-candidates.md` §4 already showed that declaring all five candidates closes both home rooms' four exits each. It also shows the reverse, which that note flagged in one line and which the code confirms: W11S28 (west: W10S28), W12S29 (south: W12S30) and W13S29 (south: W13S30) each border a highway and are therefore permanently raidable. W12S27, W13S27 and W14S28 have four normal neighbours and are gateable in principle.

## 2. What five bots do when an invader appears in a remote

| bot | trigger | defender body | count | station | fight | do miners/haulers keep working? | abandon? |
|---|---|---|---|---|---|---|---|
| **Overmind** `Overseer.handleOutpostDefense` → `DirectiveGuard` → `DefenseNPCOverlord` | `room.dangerousHostiles.length > 0` in an outpost and no defence flag yet → drop a flag at `dangerousHostiles[0].pos`. Overlord wishlist is 1 while `room.invaders.length > 0 \|\| RoomIntel.isInvasionLikely(room)` | `CombatSetups.broodlings.default`: `[TOUGH, ATTACK×3, MOVE×5, HEAL]`, `sizeLimit: Infinity` → **750 e per repeat**, scaled to `energyCapacityAvailable`, capped at 5 repeats by `MAX_CREEP_SIZE`. Below `DefenseNPCOverlord.requiredRCL = 3` it falls back to `broodlings.early` = `[ATTACK, MOVE]` repeats | 1 | in the outpost; with no target it runs `doMedicActions(roomName)` | `attackAndChase` the closest hostile not on an edge tile, `healSelfIfPossible` | miners flee — `miner.flee(miner.room.fleeDefaults, {dropEnergy: true})` (`overlords/mining/miner.ts:357`), fleeRange 8 or 16 | no. The only removal is `RoomIntel.roomOwnedBy`; the guard flag self-removes after **100 consecutive safe ticks** |
| **bonzAI** `MiningOperation` → `BodyguardMission` (always attached) | **pre-emptive**: `maxDefenders = 1` when `InvaderGuru.invaderProbable`; then `ceil(hostiles.length / 2)` once there is vision. `prespawn: 50` | `configBody({tough:1, move:5, attack:3, heal:1})` — the identical 750-e unit — `potency = min(spawnGroup.maxUnits(unit,1), 3)`. Role name `leeroy` | 1, or ⌈hostiles/2⌉ | lives in the remote; with no hostiles it heals itself and runs `medicActions` | closest by range, `attack` + a `move` into it, self-heal when not attacking | **both** flee: `Agent.fleeHostiles()` = `fleeByPath(room.fleeObjects, 6, 2, false)`, called by miner *and* cart in `MiningMission` | no abandonment logic at all on the mining path |
| **Winsley** `DefendRemoteMission` (+ `KillCoreMission`) | `Memory.rooms[room].invaderCore \|\| lastHostileSeen === scanned` for any room in `franchiseDefenseRooms(office, source)` — **the whole corridor, not just the source room** | `buildBlinky(energy)`: `[RANGED_ATTACK, MOVE]` segments below 1,000 e, `[RANGED_ATTACK, MOVE, MOVE, HEAL]` (**500 e**) above | `1` while `totalCreepStats(hostiles).score > totalCreepStats(current).score`, where `score = rangedAttack + attack×3 + heal/mitigation` (comment: "stolen from Overmind") | walks to `(25,25)` of the target room at range 20, then hunts | `blinkyKill`: kite to range 3 when the target `isAttacker`, else close to 1 and `rangedMassAttack` | **yes** — `recordThreat` returns early for `['Source Keeper', 'Invader']`, so an NPC raid never raises `franchiseIsThreatened` and never disables the harvest mission | never for invaders; only for confirmed *player* attackers over `THREAT_TOLERANCE.remote[rcl]` |
| **The International** `RemotesManager.initRun` | `remote.roomManager.enemyAttackers.length` | **none — the whole `remoteDefender` spawn request is commented out** in `spawnRequests.ts:1397–1461`. The dead design is worth reading: RANGED_ATTACK+MOVE pairs sized to the enemy's `minDamage`, HEAL+MOVE pairs sized to `minHeal`, and **if the required body exceeds 50 parts or `spawnEnergyCapacity`, set `abandonRemote = randomIntRange(1000, 1500)` instead** | — | — | — | no — everything withdraws | **yes, on a clock**: `abandonRemote(remoteName, randomIntRange(score, score + 100))` where `score = findLowestScore(enemyAttackers, c => c.ticksToLive)`; `recurseAbandonment` spreads the same term to every sibling remote pathing through the room |
| **TooAngel** `role_reserver.callDefender` | the reserver sees `room.findEnemies().length > 0 \|\| invaderCores.length > 0`, and pushes **once per reserver lifetime** (`creep.memory.defender_called`) onto the base room's spawn queue. Gated by `config.creep.reserverDefender: true` | `roles.defender`: `layoutString: 'MRH'`, `amount: {1: [2,1,1], 8: [4,1,1]}` → the unit is M,M,R,H = **500 e**, repeated `floor(min(energyAvailable / 500, 46 / 4))` | 1 per call | routed to `targetRoom`; recycles itself on returning to base | `handleDefender` + `selfHeal` each tick | **the split**: `roles.carry.flee = true`, `roles.sourcer.flee = false` (TODO comment dated 2016) | not on danger — TooAngel drops a remote on **its own** economy (`spawnIdle < 0.2`, storage < 100,000) or on losing memory to a global reset |
| **Quorum** `CityMine` | `this.underAttack = this.mine.find(FIND_HOSTILE_CREEPS).length > 0` — **unfiltered, so NPC invaders trigger it** | **none. Quorum has no combat role at all** (`src/roles/` holds no guard or defender) | 0 | — | — | **no and yes, the worst combination**: `minerQuantity = 0`, haulers not resized, `reserveRoom(false)` — but nothing recalls the creeps already there. The miner keeps `travelTo(minerPos)` and harvests until it dies | `defend()` only calls `recordAggression` telemetry |

Three things generalise:

1. **Four of five spawn something; only one of five withdraws.** The International is the sole bot whose answer to an invader is retreat, and its retreat is on a clock read off the enemy's own `ticksToLive` — the same number §1.3 says is readable.
2. **Two independent pairs converged on two bodies.** Overmind and bonzAI on the melee 750-e unit; Winsley and TooAngel on the ranged 500-e unit. Nobody uses pure ATTACK without a HEAL part, and nobody at this scale uses TOUGH beyond a single part.
3. **Two of five pre-spawn from the harvest counter.** `bonzAI/InvaderGuru.trackEnergyTillInvader` and `Overmind/RoomIntel.isInvasionLikely` use near-identical thresholds — 3 sources 65,000, 2 sources 75,000, 1 source 90,000, with a 20,000-tick staleness guard — and both **reconstruct the server-side `invaderHarvested` counter from the client side**, by accumulating `source.energyCapacity - source.energy` on the tick `source.ticksToRegeneration === 1`. That is the only way to see the timetable from inside the game.

## 3. What a defender buys, per energy

Damage and healing per energy, counting the MOVE that carries the part (1:1, which is what an unroaded remote needs):

| part | output | cost with MOVE | per energy |
|---|---|---|---|
| ATTACK | 30 | 130 | **0.231 dmg/e** |
| RANGED_ATTACK | 10 | 200 | 0.050 dmg/e |
| HEAL | 12 | 300 | 0.040 hp/e |

**Melee is 4.6× ranged.** That is the whole argument for the Overmind/bonzAI unit at RCL4–5, and it is only defensible because §1.3 shows the lone invader — nine raids in ten — walks into range 1 by itself.

What each bank buys, and whether it wins (arithmetic derived from the constants above; not measured in game):

| bank | body | damage / heal / hits | vs lone `smallMelee` (1,000 hp, 40 dmg) | vs boosted melee (80 dmg) | vs melee + healer (60 or 120 hp/t) |
|---|---|---|---|---|---|
| 1,300 (W13S28) | 1 × Overmind unit, 750 e | 90 / 12 / 1,000 | kills in 12 t, takes ~336 | kills in 12 t, takes ~816 — survives, barely | **loses**: 90 − 60 = 30 net, 34 t to kill, 1,360 taken |
| 1,300 | 2 × blinky segment, 1,000 e | 20 / 24 / 800 | kites at 3, takes 10 − heals 24 → immune, 50 t | takes 20 − heals 24 → immune, 55 t | **stalemate** — cannot out-damage 60 hp/t |
| 1,800 (W12S28) | 2 × Overmind unit, 1,500 e | 180 / 24 / 2,000 | 6 t | 7 t | **wins**: 120 net (60 vs a boosted healer), 9–17 t |
| 1,800 | 3 × blinky segment, 1,500 e | 30 / 36 / 1,200 | immune, 34 t | immune, 36 t | **stalemate** |
| 1,300, two guards | 2 × 750 e | 180 / 24 / 2,000 | trivial | trivial | wins |

The 2%-case five-group (up to two healers, 120–240 hp/tick) beats a single 1,500-e guard and needs two. Winsley's score comparison and bonzAI's `ceil(hostiles/2)` are two spellings of the same rule.

## 4. The economics

Per-raid, using `remote-candidates.md` §3's net figures (which are already computed in fabot's own `Engine` constants and body rules) and §1.1's cadence:

| | W12S27 (declared, 1 source, +6.58 e/tick) | W13S29 (recommended, 2 sources, +14.24 e/tick) |
|---|---|---|
| raid cadence | ~10,000 t, then 7,000–13,000 | ~5,000 t, then 3,500–6,500 |
| income between raids | 65,800 e | 71,200 e |
| **ADR 0043 stand-down, 2,500-tick fallback** | **16,450 e** (25% of the cycle) | **35,600 e** (50% of the cycle) |
| stand-down clocked on the invader's TTL (≤1,500) | 9,870 e | 21,360 e |
| **one guard, spawned per wave** | **1,500 e** (2 units at bank 1,800) | **750 e** (1 unit at bank 1,300) |
| a guard kept standing permanently | 1.0 e/tick = 10,000 e/cycle | 0.5 e/tick = 2,500 e/cycle (3.5% of income) |
| creeps lost if nothing runs and nothing fights | anchor 700 + hauler 1,800 = 2,500 e | anchor 700 + hauler 1,200 = 1,900 e |
| reserving the exit neighbours instead | ~3.5 e/tick for three rooms = ~35,000 e/cycle | impossible — W13S30 is highway |

**A guard is 11× cheaper than the fallback stand-down at W12S27 and 47× cheaper at W13S29.** Even the crude option of standing one permanently costs 3.5% of W13S29's income. Reserving neighbours for the gate is 23× worse than the guard where it is possible at all.

Two smaller running costs that a stand-down does *not* incur as badly as intuition suggests:

- **Containers survive one stand-down and not two.** Unrepaired in an unowned room they lose `CONTAINER_DECAY / CONTAINER_DECAY_TIME` = 50 hits a tick against `CONTAINER_HITS = 250000` — 5,000 ticks to collapse. A 2,500-tick stand-down spends half the container's life; back-to-back ones cost the 5,000-energy rebuild. Roads lose 0.1 hits/tick/tile against 5,000 — 50,000 ticks — and are not worth counting.
- **The reservation survives, because of how fabot already sizes reservers.** `reserverBodyWithin` sets `claims = ceil((5000 − ticksToEnd)/600)`, which drives `ticksToEnd` to the `CONTROLLER_RESERVE_MAX = 5000` cap; W12S27 measured 4,911 on 2026-09-07. A single 2,500-tick stand-down is absorbed by that buffer. A second consecutive one is not, and a lapse also truncates the source's stock to `SOURCE_ENERGY_NEUTRAL_CAPACITY = 1500` on the spot (`sources/tick.js`).

## 5. Alternatives beyond fleeing and fighting

- **Towers: confirmed impossible, twice over.** `CONTROLLER_STRUCTURES.tower` has no key `0` and `checkControllerAvailability` reads `rcl = 0` for an unowned controller, so the site is refused with `ERR_RCL_NOT_ENOUGH`; and `intents/towers/attack.js` resolves its target out of `roomObjects`, the tower's own room table, so a home tower cannot reach across a Seam even in principle.
- **Ramparts: confirmed impossible.** Same path — `rampart` and `constructedWall` are `{1: 0, 2: 2500, …}` with no level-0 entry. **ADR 0034's answer for a threatened Post — stand it on a rampart — does not extend to an outpost.** An outpost anchor has exactly two options: die, or be defended by a creep.
- **Source-keeper rooms: irrelevant here** and already excluded by the type system — `Outpost.Controller` is a required field and an SK room has no controller (`remote-candidates.md` §Summary).
- **"Invader bait": no implementation exists.** Zero hits for the term across all six bots. Note also that `genInvaders` skips any room already holding a `user: '2'` creep, so a live invader anywhere in the room postpones the next raid on that room — which is an argument for *not* killing one instantly, and a weak one, since the goal was already re-rolled at spawn time.
- **Timing our withdrawals is genuinely available.** The raid clock is our own harvest volume, and the counter is reconstructible client-side by the Overmind/bonzAI sampling trick (§2). This makes "an invasion is due in N ticks" a computable economy signal, exactly as `seasonal-threats-safemode.md` §Implications 5 said for the home room — but it is *pre-spawn* information, not a substitute for a defender.
- **Reserving all exit neighbours** closes both home rooms (`remote-candidates.md` §4) and is worth having as a side effect of declaring the five candidates. It is not worth paying for on its own: §4's 23× ratio, and three of the five outposts border highways and can never be gated.

## 6. What it means for fabot

Under 100 CPU, at RCL4–5, with one outpost each and a raid due every 5,000–10,000 ticks, **the cheapest thing that keeps the income is one melee guard per outpost, spawned on contact**. Nine raids in ten are a single `smallMelee` that walks into range 1 by itself; melee gives 4.6× the damage per energy of ranged; and 750 energy at W13S28's bank or 1,500 at W12S28's beats it outright. Against a 16,450–35,600-energy stand-down that is an eleven- to forty-sevenfold saving, and it is the only option that leaves the container, the reservation and the anchor where they are.

An ADR would have to decide four things about that guard and two about what surrounds it.

**The defender row.** *Body*: the Overmind/bonzAI unit `[TOUGH, ATTACK×3, MOVE×5, HEAL]` repeated to the bank — one unit at 1,300, two at 1,800 — chosen over the Winsley/TooAngel ranged unit because a 1,500-energy blinky cannot out-damage even an unboosted healer. *Trigger*: a Threat in an outpost, which ADR 0033 already derives; pre-spawning off the reconstructed harvest counter is a second ticket, not this one. *Station*: inside the outpost, and closer to the invader than the anchor, or the invader walks past it to the anchor. *Count*: one, rising to two when the raid's healing per tick exceeds our damage per tick — Winsley's score rule, ~6% of raids.

**Around it.** Flee stays for haulers and stays off for anchors: that split is exactly TooAngel's `carry.flee = true` / `sourcer.flee = false`, and Overmind's Threat test is character-for-character ADR 0033's. And ADR 0043's stand-down should not be deleted but re-clocked: a plain invader has a readable deadline after all — its own `ticksToLive`, ≤1,500 and ungated on hostile creeps — so the 2,500-tick fallback belongs to cores, which spawn nothing, never leave, and want a dismantler instead. Reserving the exit neighbours is not the first move: it is 23× the guard's cost, and W11S28, W12S29 and W13S29 border highways that can never be gated.

## Sources

- Invader spawner, bodies, boosts, raid sizing, exit gate: https://github.com/screeps/backend-local/blob/master/lib/cronjobs.js (`genInvaders`, `createRaid`, `createCreep`, `checkExit`); `lib/strongholds.js` (`spawnStronghold`, `selectRoom`, `expandStronghold`); `lib/utils.js` (`activateRoom`, `getActiveRooms`)
- Invader AI: https://github.com/screeps/engine/tree/master/src/processor/intents/creeps/invaders — `pretick.js`, `findAttack.js` (the chase, the structure dismantle, the suicide branch), `flee.js`, `healer.js`, `shootAtWill.js`
- Engine rules quoted: `src/game/rooms.js` `Room.prototype.createConstructionSite`; `src/utils.js` `checkControllerAvailability`; `src/processor/intents/towers/attack.js`; `src/game/creeps.js` (`ticksToLive`); `src/processor/intents/sources/tick.js`; `src/processor/intents/containers/tick.js`
- Constants: https://github.com/screeps/common/blob/master/lib/constants.js — `BODYPART_COST`, `ATTACK_POWER`/`RANGED_ATTACK_POWER`/`HEAL_POWER`/`DISMANTLE_POWER`, `BOOSTS`, `CONTROLLER_STRUCTURES`, `CONTAINER_*`, `ROAD_*`, `INVADERS_ENERGY_GOAL`, `INVADER_CORE_HITS`, `CREEP_LIFE_TIME`
- Overmind 5eca49a: `src/Overseer.ts` (`handleOutpostDefense`), `src/directives/defense/guard.ts`, `src/overlords/defense/npcDefense.ts`, `src/overlords/defense/guardSwarm.ts`, `src/creepSetups/setups.ts` (`CombatSetups.broodlings`), `src/intel/RoomIntel.ts` (`isInvasionLikely`), `src/prototypes/Room.ts` (`fleeDefaults`), `src/movement/Movement.ts` (`flee`), `src/overlords/mining/miner.ts`
- bonzAI 4a0006d: `src/ai/missions/BodyguardMission.ts`, `EnhancedBodyguardMission.ts`, `InvaderGuru.ts`, `Agent.ts` (`fleeHostiles`, `fleeByPath`), `src/ai/operations/MiningOperation.ts`
- The International 7e5106e: `src/room/commune/remotesManager.ts` (`initRun`, `abandonRemote`), `src/room/commune/spawning/spawnRequests.ts` (the commented-out `remoteDefender` block, `remoteCoreAttacker`), `src/room/creeps/roleManagers/remote/remoteDefender.ts`
- Quorum f6868c5: `src/programs/city/mine.js` (`underAttack`, `defend`, `recordAggression`), `src/roles/` (no combat role)
- TooAngel 87645a0: `src/role_defender.js`, `src/role_reserver.js` (`callDefender`), `src/config.js` (`reserverDefender`), `src/prototype_room_creepbuilder.js` (`getPartConfig`, `getMaxRepeat`), `src/role_carry.js` / `src/role_sourcer.js` (the flee flags)
- Jon Winsley a835bbc: `src/Missions/Implementations/DefendRemoteMission.ts`, `KillCoreMission.ts`, `src/Minions/Builds/blinky.ts`, `guard.ts`, `src/Behaviors/blinkyKill.ts`, `src/Selectors/Combat/combatStats.ts`, `src/Strategy/Territories/HarassmentZones.ts` (`recordThreat`, `franchiseIsThreatened`)
- In-repo: `docs/research/seasonal-threats-safemode.md`, `docs/research/remote-mining.md` §1.3–1.5 and §6, `docs/research/remote-candidates.md` §3–§4, `docs/adr/0033-threats-gate-tasks-through-reach-and-flee.md`, `docs/adr/0043-an-outpost-stands-down-until-a-tick-it-reads-off-the-threat.md`
