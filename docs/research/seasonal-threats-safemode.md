# Hostile-threat model for an RCL2 colony on the seasonal server — and is safe mode enough?

Date: 2026-09-02. All claims verified against primary sources (official engine/backend source on GitHub, official docs, first-party announcements) unless marked **unverified**. Engine citations are from the default branches of `screeps/engine`, `screeps/backend-local` and `screeps/common` (the open-sourced server code that the official server is built from; the MMO backend itself is closed, so backend-level behavior is "verified in the published server code, presumed identical on official"). Motivating question: fabot is a single-room, no-tower RCL2 colony on the official seasonal server — what can actually hurt it, and does "detect hostiles → `activateSafeMode()`" suffice as the only defense?

## Summary

- **Current season (verified, first-party):** Season #11, spawning open since Aug 27, ticks started **September 1, 2026 18:00 UTC**, ends **November 1, 2026**. Rules = Season #5 (Thorium → Reactors in sector centers, score for continuous reactor operation) with smaller thorium deposits and resource density increasing toward the north of the map. Flat 100 CPU, GCL1/GPL0 start, **no market, terminals only to your own terminals, no portals**. No announced changes to invaders or safe mode. As of today the world is **one day old** — every opponent is also ~RCL2.
- **NPC invaders (verified in source):** triggered per room when the sum of `invaderHarvested` over its sources reaches `invaderGoal` — normally `floor(100000 × rand(0.7..1.3))`, 5% chance doubled; a **freshly spawned room starts with `invaderGoal = 1,000,000`** (10× grace period). Checked by a cron every 5 real minutes. Invasions additionally require a live invader stronghold in the sector and at least one exit bordering an un-owned, un-reserved room. At RCL < 4 you get **small** bodies only: usually 1 invader (~50% chance T1-boosted), 10% chance a group of 2–5. They hunt creeps and only attack structures that block their path; they never touch the controller and die of TTL after 1,500 ticks.
- **Safe mode (verified in source):** duration **20,000 ticks**, cooldown **50,000 ticks from activation** (⇒ a 30,000-tick window after expiry in which re-activation is impossible), one room per shard, one activation granted per controller level-up, `safeModeAvailable` **zeroed if the controller ever downgrades a level**. It blocks every hostile *harmful* intent in the room (attack, rangedAttack, rangedMassAttack, dismantle, attackController, withdraw, even heal) but does **not** block hostile movement, hostile `harvest` of your sources, or `pickup` of your dropped energy. Placing your spawn starts the room with safe mode already active for 20,000 ticks.
- **Verdict:** at RCL2, against the actual threat population of a one-day-old seasonal world, **safe mode as sole defense is viable and close to optimal** — one activation outlasts any invader raid by 13× and the 1M fresh-room invader goal means fabot likely reaches RCL3 (towers) before the first invasion is even possible. The real weaknesses are: the 30k-tick cooldown gap, the RCL2 stock of exactly one activation, losing the stock on controller downgrade, and a determined human attacker who taps the controller with a CLAIM creep (blocks activation for 1,000 ticks) — none of which are acute in week 1.

## 1. NPC invaders — exact trigger conditions (engine/backend source)

The invader spawner is the `genInvaders` cron job in the open-sourced backend, `lib/cronjobs.js` (https://github.com/screeps/backend-local/blob/master/lib/cronjobs.js), registered as `genInvaders: [5*60, genInvaders]` — **every 5 real-world minutes** (~80–160 game ticks at the current 1000–2000 ticks/hour shard rates). Per room, in order:

1. **Skip if invader creeps are already in the room** (`user: '2'` creeps present).
2. **Energy threshold:** `const invaderHarvested = _.sum(sources, 'invaderHarvested'); const goal = room.invaderGoal || C.INVADERS_ENERGY_GOAL; if(goal != 1 && invaderHarvested < goal) continue;`
   - `INVADERS_ENERGY_GOAL: 100000` (https://github.com/screeps/common/blob/master/lib/constants.js).
   - `invaderHarvested` is incremented on the source by every harvest intent: `let invaderHarvested = (target.invaderHarvested || 0) + amount;` in `engine/src/processor/intents/creeps/harvest.js` — i.e. the counter tracks *your* mining; invaders themselves never harvest.
   - `goal != 1` is a debug hook (setting `invaderGoal = 1` via CLI forces a raid).
3. **Stronghold gate:** the room's sector (regex over the room name, e.g. all of `W1xN2x`) must contain an `invaderCore` with `level > 0`, else `continue` ("Skip room … since there is no invaderCore in sector"). The official docs corroborate for MMO: destroying the sector's NPC Stronghold stops invasions "until the next stronghold spawns" (https://docs.screeps.com/invaders.html). Strongholds themselves spawn/expand via `genStrongholds`/`expandStrongholds` crons (5 and 15 min) and decay after `STRONGHOLD_DECAY_TICKS: 75000`.
4. **Exit gate:** invaders enter only through exit walls whose neighboring room has a controller that is neither owned nor reserved (`checkExit` rejects `controller.user || controller.reservation`). If every exit borders an owned/reserved room, **no invasion happens at all**. Docs agree: invaders spawn only "at exits to neutral rooms". They spawn *on the exit tiles of your room* and cannot leave the room.
5. **After a raid**, the next goal is randomized: `invaderGoal = Math.floor(C.INVADERS_ENERGY_GOAL * (Math.random()*0.6 + 0.7))` → 70k–130k; then `if(Math.random() < 0.1) invaderGoal *= Math.floor(Math.random() > 0.5 ? 2 : 0.5)` — a quirk: 5% chance ×2 (140k–260k), 5% chance ×0, and a zero goal falls back to the default 100k on the next check. All sources' `invaderHarvested` reset to 0.

**Fresh-room grace (verified in backend source, MMO parity presumed):** the place-spawn endpoint (`lib/game/api/game.js`, same repo) resets `invaderHarvested: 0` on all sources and sets `db.rooms.update({_id: room}, {$set: {invaderGoal: 1000000}})`. So the *first* invasion of a newly settled room requires **~1,000,000 harvested energy**, not 100k. At RCL2's theoretical maximum of 20 energy/tick (2 sources × 3000/300), that is ≥50,000 ticks (~2 days at 1000 t/h) of *perfect* harvesting; a realistic early bot takes several times longer. Subsequent invasions then need 70k–130k each (≥3,500–6,500 ticks at perfect efficiency).

### Raid composition (`createRaid`/`createCreep`, same file, verbatim from source)

- Body class: `controllerLevel >= 4 ? 'big' : 'small'` — **an RCL2 room only ever sees small invaders**.
- Count: default `max = 1, count = 1, boostChance = 0.5`. With 10% probability (or always in sector-center rooms): `max = 2`; nested 20% (2% overall): `max = 5`; then `count = floor(rand*(max-1)) + 2`, capped by available exit tiles. Matches docs: "10% chance that you will get … a whole company of them, from 2 to 5".
- First creep of a group is Melee (non-center rooms), the rest alternate Ranged/Healer; group members spawn on adjacent exit tiles.
- **Small bodies** (10 parts, 1,000 hits, `ticksToLive: 1500`):
  - `smallMelee`: 2×TOUGH, 5×MOVE, 1×RANGED_ATTACK, 1×WORK, 1×ATTACK → 40 dmg/tick adjacent (30 attack + 10 ranged), 50/tick dismantle vs blocking structures.
  - `smallRanged`: 2×TOUGH, 5×MOVE, 3×RANGED_ATTACK → 30 dmg/tick at range ≤3.
  - `smallHealer`: 5×MOVE, 5×HEAL → 60 hp/tick heal.
- **Boosts:** each spawned invader is independently boosted with probability `boostChance` (0.5 for small). In ordinary rooms boosts are T1: `attack→UH, ranged_attack→KO, heal→LO, work→ZH, tough→GO` (T3 `XUH2O` etc. only in sector-center rooms). A boosted smallMelee hits for 80/tick; a boosted smallHealer heals 120/tick.
- Big bodies (RCL4+, for later reference): 50 parts — bigMelee 16T/24M/3RA/4W/2A, bigRanged 6T/25M/18RA/1W, bigHealer 25M/25H.

### Invader behavior (engine AI, `engine/src/processor/intents/creeps/invaders/*.js`)

- `pretick.js`/`findAttack.js`: invaders classify every non-invader creep as hostile and pursue the closest by path; melee attacks adjacent creeps; pure-ranged bodies kite at range 3 (`flee.js`); `shootAtWill.js` fires `rangedAttack` at the lowest-hits hostile in range 3 every tick; healers heal the most-damaged invader.
- **Structures:** they path *around* structures first; only when creeps are unreachable do they re-path ignoring destructible structures and attack/dismantle the specific structure on their path (non-spawn structures via rangedAttack/dismantle; they also head for unreachable spawns). Docs phrasing: they "will not touch your structures most of the time, but if a structure gets on its way, it will try to destroy it".
- They never target the controller (`attackController` requires CLAIM parts, which no invader body has) and never harvest.
- If no hostile creep is reachable and every spawn is reachable in an owned room, they `suicide`.

## 2. Seasonal server — Season #11 (current)

Primary source: the official Steam news feed for Screeps: World (https://steamcommunity.com/app/464350/allnews/ — the forum at screeps.com/forum is archived; Steam News is the current announcement channel). Verbatim from the Season #11 announcement (posted Aug 27):

> "Season #11 is open! … The game will start on September 1st at 18:00 UTC. This season will last for 2 months, till November 1 [2026]."
> "Everyone has GCL 1/GPL 0, equal 100 CPU from the start (which does not scale with GCL)."
> "Season #11 brings back the rules of Season #5 with a few changes: The seasonal resource is Thorium, a finite mineral that does not regenerate. … Thorium deposits contain less material than in Season #5. Thorium accelerates creep aging and the decay of roads and containers. Players have to deliver Thorium to Reactors located in the center of each game sector. Reactors consume Thorium and generate season score for their owner, with the score increasing the longer a Reactor operates continuously. Reactors can be claimed by other players."
> "Unlike Season #5, resources are distributed unevenly across the world. Resource density increases toward the upper part of the world."
> "Other game changes: every player has 100 CPU, the market is not available, terminal structures can send resources only to your own terminals. Also, there will be no portals."

Season #5 mechanics that carry over (official forum announcement, https://screeps.com/forum/topic/3277/season-5-is-open): reactors consume 1 thorium/tick and score `1 + floor(log10(ticks of continuous operating))` per tick; thorium contact decay factor is `floor(log10(total thorium on tile))`; reactors are re-claimable "using a single creep with the CLAIM body part". Access costs 5 keys now, dropping to 1 by Oct 15; seasons restart every 3 months (Season 8 announcement, Feb 2026, same feed). The numbering jumped (Season 10 was the free 10th-anniversary season in June 2026), which is why community wikis that stop at Season 6 (e.g. https://wiki.screepspl.us/Seasonal_World/) look stale.

**Threat-relevant implications:**

- **Invader mechanics: same as MMO.** The season announcements enumerate their deltas from the regular game ("All game mechanics are identical to the regular game, with an exception that…" — Season 8/10 wording) and none touch invaders, strongholds, or safe mode. **Unverified residual:** whether the custom seasonal map actually seeds NPC strongholds in every sector — if a sector has no live `invaderCore`, the backend logic above produces *zero* invasions there. Worth confirming by scouting once; do not design around it.
- **PvP is structurally encouraged, not restricted.** Single winner-take-most leaderboard, claimable/stealable reactors at sector centers, no announced spawn protection beyond the standard mechanics, and denser resources northward funneling competition. No novice areas were announced. The only "protection" is the standard one: **placing your spawn sets the controller to `safeMode: gameTime + 20000`** (verified: `lib/game/api/game.js` place-spawn handler, and the initial 20k-tick active safe mode is the same code path on any respawn).
- **Season-specific NPC threats: none.** Thorium adds no hostile NPCs; its hazard is self-inflicted (creep aging / road+container decay when carrying it), and irrelevant until fabot mines thorium.
- **Timing reality check:** today (2026-09-02) every player is ≤1 day old. RCL2 opponents cannot even spawn a CLAIM creep (`[CLAIM, MOVE]` costs 650; RCL2 energy capacity is 300 + 5×50 = 550; RCL3 = 800), so controller attacks and room claims are impossible from RCL2 attackers. Early rushes are limited to small melee/ranged creeps from nearby spawns.

## 3. Safe mode — exact rules (engine source + official docs)

Constants (https://github.com/screeps/common/blob/master/lib/constants.js): `SAFE_MODE_DURATION: 20000`, `SAFE_MODE_COOLDOWN: 50000`, `SAFE_MODE_COST: 1000` (ghodium, for `generateSafeMode`), `CONTROLLER_DOWNGRADE_SAFEMODE_THRESHOLD: 5000`, `CONTROLLER_DOWNGRADE: {1: 20000, 2: 10000, …}`.

**Activation checks** — API side (`engine/src/game/structures.js`) and processor side (`engine/src/processor/intents/controllers/activateSafeMode.js`) agree:

- `ERR_NOT_ENOUGH_RESOURCES` if `safeModeAvailable <= 0`.
- `ERR_TIRED` if `safeModeCooldown` is running, or `upgradeBlocked > 0`, or `ticksToDowngrade < CONTROLLER_DOWNGRADE[level]/2 - 5000`. **At RCL2 the downgrade clause is vacuous** (10000/2 − 5000 = 0 ⇒ any positive downgrade timer passes); at RCL3 it bites when `ticksToDowngrade < 5000`.
- `ERR_BUSY` if any *other* controller you own has active safe mode — "only in one room per shard" (https://docs.screeps.com/defense.html); the backend additionally rejects the raw intent with a DB-wide count (`add-object-intent` handler in `backend-local/lib/game/api/game.js`). The seasonal world is a single shard, and fabot is single-room, so this never fires.
- There is **no energy/terminal requirement** — the "1000 in terminal" idea is a confusion with `SAFE_MODE_COST: 1000` *ghodium*, which is what `Creep.generateSafeMode` consumes to **add** an activation (RCL6+ economy; irrelevant at RCL2).

**Acquiring activations** (all verified in engine): +1 per controller level-up (`target.safeModeAvailable = (target.safeModeAvailable || 0) + 1` in `processor/intents/creeps/upgradeController.js`); the initial spawn placement gives an *active* safe mode but sets no `safeModeAvailable`. So the ledger at RCL2 is: **1 activation** (from the 1→2 level-up), +1 more at each subsequent level.

**On activation** (`processor/intents/controllers/tick.js`): `safeModeAvailable −1`, `safeMode = gameTime + 20000`, `safeModeCooldown = gameTime + 50000` — **the cooldown runs from activation**, so after the 20k duration ends there is a 30k-tick window in which re-activation is impossible regardless of stock.

**Losing the stock** (same file): when a controller downgrades a level, `safeModeAvailable = 0` and a fresh 50k cooldown is set (same on unclaim). A nuke landing cancels active safe mode outright (`safeMode: gameTime, safeModeCooldown: null` in `processor/intents/nukes/tick.js`) — RCL8-attacker territory, not a week-1 concern.

**What safe mode blocks** — every harmful intent in the room checks `roomController.user != object.user && roomController.safeMode > gameTime` and silently no-ops: `attack`, `rangedAttack`, `rangedMassAttack`, `dismantle`, `attackController`, `withdraw` (from your structures), and even hostile `heal`/`rangedHeal`. Hostile creeps stepping on your construction sites don't stomp them during safe mode (`processor/intents/movement.js`), and your own builds treat enemy creeps as non-blocking (`build.js`). Your creeps and towers act normally — docs: you keep "defensive capabilities".

**What safe mode does NOT block (all verified by absence of the check in the intent processor):**

- Hostile **movement** — enemies and invaders still roam your room and body-block tiles.
- Hostile **`harvest`** — `creeps/harvest.js` has no safe-mode check: an enemy creep *can* mine your sources during your safe mode (that is the real "energy drain" vector — invaders never do this, players could).
- Hostile **`pickup`** — no check in `pickup.js`: dropped energy (RCL2 drop-mining!) can be stolen during safe mode.
- **Invader spawning and the `invaderHarvested` counter** — the cron has no safe-mode/novice check; raids can arrive during safe mode (where they harmlessly chase creeps until their 1,500-tick TTL kills them, since 20k ≫ 1.5k).
- Anything **outside the room** — protection is strictly per-room (the check reads the room's own controller).

**The CLAIM-tap counter-play (verified):** `attackController` by a creep with CLAIM parts sets `upgradeBlocked = gameTime + 1000` (`CONTROLLER_ATTACK_BLOCKED_UPGRADE: 1000`) and knocks 300 ticks/part off the downgrade timer — and `upgradeBlocked` blocks `activateSafeMode` (ERR_TIRED). Safe mode blocks `attackController`, so this is a race: whoever lands first wins. Same-tick intent ordering is **unverified**; from the next tick the lockout is certain. A determined attacker who keeps re-tapping your controller defeats safe-mode-only defense — but needs RCL3+ economy for the 650-energy claim creep and must keep it alive at range 1 of your controller.

**Seasonal changes to safe mode: none announced** (see §2).

## 4. Threat list for an RCL2, single-room, no-tower colony (Season #11, week 1)

| # | Threat | Likelihood now | What it does | Covered by safe mode? |
|---|--------|----------------|--------------|----------------------|
| 1 | Lone small invader (50% T1-boosted) | Near-zero until ~1M energy harvested (fresh-room goal); then every 70k–130k harvested | 30–80 dmg/tick vs creeps; dismantles structures in its path; dies after 1,500 ticks | Yes — fully neutralized; it can't attack, and TTL expires 13× before safe mode does |
| 2 | Invader group 2–5 (10% of raids) | Same gating as #1 | As above, with kiting ranged + 60–120 hp/t healers — unbeatable for RCL2 creeps in open field | Yes — same as #1 |
| 3 | Early PvP harass (small attack/ranged creeps from neighbors) | Real but modest; everyone is ≤RCL2–3 and CPU-capped at 100 | Kills workers, camps sources, stomps construction sites (site-stomping blocked only during safe mode) | Yes for attacks; no for body-blocking, source-harvest theft, dropped-energy theft |
| 4 | Controller CLAIM-tap → safe-mode lockout → clean-out | Zero from RCL2 attackers (650 > 550 capacity); rises as neighbors hit RCL3+ | Blocks activation 1,000 ticks per tap, then attacker kills spawn/creeps at leisure | **No** — this is the designed counter to safe mode |
| 5 | Controller downgrade (own neglect) | Under fabot's control | RCL2→1 costs 5 extensions *and zeroes `safeModeAvailable`* | N/A — safe mode doesn't pause the downgrade timer; keep upgrading |
| 6 | Room claim/attack during the 30k-tick cooldown gap or with 0 activations left | Low in week 1 | Full exposure, no towers | **No** — the structural hole in safe-mode-only defense |
| 7 | Thorium hazards, reactor-area PvP, strongholds/cores in neighbor rooms | Not until fabot leaves its room | Blocks remotes (cores reserve neutral controllers), decays thorium carriers | N/A — out-of-room, safe mode never applies |

Not threats at RCL2: invaders vs the controller (impossible — no CLAIM parts), invader energy theft (they never harvest/pickup), nukes (RCL8 attacker, and would cancel safe mode anyway), source keepers (only in SK rooms — **unverified** whether the seasonal map even has them).

## 5. Verdict on "detect hostiles → activate safe mode" as sole defense

**Viable at RCL2, with four caveats to encode rather than ignore.**

It works because the numbers line up: one activation (20k ticks) outlasts any invader raid (1,500-tick TTL) by 13×, the fresh-room `invaderGoal` of 1M means fabot's first possible raid is ~50k+ ticks of perfect harvesting away (RCL3 + a tower should land far earlier), the initial spawn placement itself grants 20k ticks of active protection covering the entire early-rush window, and at RCL2 the downgrade-based ERR_TIRED clause can't fire. Against everything the engine can actually field at a one-day-old colony, popping safe mode on first hostile contact is a complete answer.

It fails at the margins: (a) the 30k-tick post-expiry cooldown window plus a stock of exactly one activation means a *second* attack wave 20k–50k ticks after the first is undefendable — mitigate by treating safe mode as a once-per-level resource and racing to RCL3 towers, not by hoarding; (b) triggering on "hostile in room" wastes the activation on harmless passers-by — trigger only on hostiles with ATTACK/RANGED_ATTACK/WORK/CLAIM parts, or on actual damage/`upgradeBlocked` events, and never for invaders while creeps can simply retreat behind distance (invaders can't leave the room and despawn in 1,500 ticks; fleeing to a corner or an adjacent room and pausing harvest is often the cheaper answer to threat #1); (c) a CLAIM-tap lands the lockout race from tick+1, so the *instant* a hostile creep containing CLAIM parts enters the room is the one case to fire immediately and unconditionally; (d) safe mode protects nothing outside the room and doesn't stop energy theft inside it.

## Implications for fabot at RCL2

1. **Ship the trivial version now:** each tick, if `room.find(FIND_HOSTILE_CREEPS)` contains a creep with CLAIM parts → `activateSafeMode()` immediately; else if a hostile has ATTACK/RANGED_ATTACK parts *and* (any own creep/structure lost hits this tick, or hostiles outnumber what we can evade) → activate. Log and notify either way. Check the return code — ERR_TIRED/ERR_NOT_ENOUGH_RESOURCES means "run the evacuation behavior instead".
2. **Do not spend the activation on invaders reflexively.** Invaders can't leave the room, can't touch the controller, and die in 1,500 ticks. A cheaper standing response: pull workers out of weapons range (melee needs range 1, ranged range 3 — and they only chase creeps), suspend harvesting, wait them out. Reserve `activateSafeMode` for: hostiles adjacent to spawn, a boosted group we can't out-walk, or any CLAIM creep.
3. **Never let the controller coast.** A downgrade to RCL1 wipes `safeModeAvailable` and sets a 50k cooldown — the upgrade task must be starvation-proof (RCL2's full timer is only 10,000 ticks).
4. **The real defense milestone is RCL3.** One tower (RCL3) kills a small invader in ~3–17 shots and closes the cooldown gap; ramparts/walls (available at RCL2, 2,500 each) are optional until then because invaders only attack blocking structures. Prioritize controller ≥ threat response ≥ extensions.
5. **Invasion forecasting is free:** the trigger is *our own* harvest volume. Track cumulative harvested energy since spawn; before ~1M total, invader probability is ~0 (backend-verified grace), after that expect a raid within ~0–5 real minutes of each 70k–130k increment. This makes "invader incoming soon" a computable economy signal, not an emergency.
6. **Later (remotes/RCL3+):** reserving or claiming all neighbor rooms suppresses invasions entirely (exit-gate rule), and a live sector stronghold is a hard prerequisite for invasions — both worth wiring into room-threat state once fabot scouts the sector. On this season specifically: no market and own-terminals-only means no bail-out energy purchases — defense economy is fully self-hosted.

## Sources

- Constants: https://github.com/screeps/common/blob/master/lib/constants.js (`INVADERS_ENERGY_GOAL`, `SAFE_MODE_*`, `CONTROLLER_DOWNGRADE*`, `STRONGHOLD_*`, `BODYPART_COST`, `CONTROLLER_STRUCTURES`)
- Invader spawner: https://github.com/screeps/backend-local/blob/master/lib/cronjobs.js (`genInvaders`, bodies, boosts, raid sizing, goal randomization)
- Fresh-room grace + shard-wide safe-mode check: https://github.com/screeps/backend-local/blob/master/lib/game/api/game.js (place-spawn: `safeMode: gameTime + 20000`, `invaderGoal: 1000000`; `add-object-intent`)
- Engine, safe mode: `src/processor/intents/controllers/activateSafeMode.js`, `controllers/tick.js`, `src/game/structures.js`, and the per-intent checks in `src/processor/intents/creeps/{attack,rangedAttack,rangedMassAttack,dismantle,heal,withdraw,attackController}.js`, `movement.js`, `nukes/tick.js` — and the *absence* of checks in `creeps/{harvest,pickup}.js` (https://github.com/screeps/engine)
- Engine, invader AI: https://github.com/screeps/engine/tree/master/src/processor/intents/creeps/invaders (`pretick.js`, `findAttack.js`, `shootAtWill.js`, `flee.js`, `healer.js`)
- Official docs: https://docs.screeps.com/invaders.html, https://docs.screeps.com/defense.html, https://docs.screeps.com/api/#StructureController (source: https://github.com/screeps/docs/blob/master/api/source/StructureController.md)
- Season #11 announcement + season cadence + numbering skip: official Steam news feed, https://steamcommunity.com/app/464350/allnews/ (posts "Season #11 starts on September 1", "Season 8 starts on March 1st", "10th Anniversary and Free Season")
- Season #5 rules (inherited): https://screeps.com/forum/topic/3277/season-5-is-open (forum now archive-mode)
- Community wiki (stale, for contrast only — lists seasons only through #6): https://wiki.screepspl.us/Seasonal_World/ — **unverified/outdated**, not relied upon
