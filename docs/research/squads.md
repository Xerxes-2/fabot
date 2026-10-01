# Squads: would a squad have held W17S25, and how to build one

Date: 2026-10-01 (live tick ~t884,4xx). Read-only research: room-history replays, the code at `109ab6f`, the engine source of the local Screeps server (`~/.local/share/Steam/steamapps/common/Screeps/server/package/node_modules/@screeps/engine/src`, cited as `engine/…`), and a tick simulator (scratchpad `sim_lock.py`, rules in Appendix A). Claims marked **inferred** are reasoned from code, not seen in a replay.

**The question.** The user believes that with squad roles and formations we would have won the W17S25 fight, where Trepidimous parked `2×18M17A + 2×11M7H + 3-CLAIM tapper` on our controller (15,36) from about t880,300. Our resident ranger `ranger-879853-Spawn8` (21M 14R 7H) went in alone and died.

## 0. Verdict

- **With the bodies we fielded, no formation wins.** One R7 deals 140 a tick against 168 a tick of raid heal (2 × 7 HEAL × 12). Nothing the raid owns can die to it under any formation (§3.3, row 1). On top of that the code sent it to the wrong ground, at the wrong target, alone (§1.3).
- **With the two residents we already pay for (2 × R7, 9,800 e), squad play is a coin flip.** "Squad play" here means assembled, focusing the healer, kiting. This pair wins only if the melee catch a ranger on fewer than about 1 tick in 5. W17S25's controller sits in a walled pocket, which makes that unlikely (§3.4).
- **A real squad wins.** In the simulator, two options win the stand-up fight with no losses when they kill healers first:
  - one melee duo `25M25A` + `18M18H` (8,650 e);
  - the same duo plus 2 × R8 (19,850 e).
  
  A 3 × R8 kiting trio (16,800 e) wins in 28 ticks with no losses while it is caught on no more than 1 tick in 3. It loses a stand-up fight.
- **Even a winning squad needs about 400 ticks to arrive.** That is about 150 ticks to spawn and about 250 to walk the 5 crossings from W15S28. Trepidimous reinforces from W17S24, the room adjacent to W17S25, in under 100. The squad would have won the fight. Whether it holds W17S25 is a war of attrition at a 4:1 travel disadvantage, and that is the user's call.
- **So the user is right in kind but not in cause.** The decisive gaps are not formations. They are:
  1. healers are never targets;
  2. a ranger is sent onto a melee's range-1 ring;
  3. every engage and cast decision is priced for one body.
  
  Fixing those three needs no formation code at all, and is stage S0 below.

## 1. Evidence from the replays

### 1.1 What could be replayed

`room-history/shardSeason/W17S25/{879700…880800}.json` returns **HTTP 404**, and so do the five neighbours (W16S25, W18S25, W17S24, W17S26, W16S26) over 880,200–880,400. The siege itself (t880,300–880,420) is not on the server. What we do have:
- W17S25 at t879,100–879,699: the earlier pass.
- The t880,341 snapshot that this session's own `observe room W17S25` printed at the time:
  - Eternity536 at 16,36; Prism305 (H) at 16,35; Paragon722 (H) at 17,36; Prime803 at 17,37; Rune908 at 16,37;
  - both healers `heal→` their own tile;
  - 1 tombstone;
  - `ranger-879853-Spawn8 DEAD`;
  - `ranger-880193-Spawn8` holding `guard:W17S25`, idle `none-free` by t880,372.
- W17S23 at t878,400–878,699 and W17S22 at t878,800–879,099, both complete.

### 1.2 How Trepidimous fights (every replayed wave)

| Behaviour | Evidence |
|---|---|
| **Snake formation.** The melee leads and the healer steps into the tile the melee just vacated, every tick, at 1 tile/tick. | W17S25 t879,264–879,276: Phobia122 7,48→8,47→9,47…; Crunchy068 is on the previous tile one tick later. W17S22 t878,883–878,960: Torque592 → Vibe199 → Genie669, a three-creep chain. |
| **Pre-heal.** The healer heals the melee every tick, even at full hits, and heals itself only when hurt. | W17S25 t879,300: Crunchy068 `heal>9,20`, the tile Phobia122 just left, with Phobia at 3500/3500. W17S22 t878,965: Genie669 self-heals while it is being shot. |
| **Parking as a 2×2 block when sieging.** | t880,341: two melee and two healers fill 16–17 × 35–37 around the tapper at 16,37, beside the controller 15,36. |
| **Melee targets the nearest thing, structures included.** | W17S22 t878,946–879,058: Torque592 and Nail636 spend tens of ticks on the spawn's ramparts (28,10 and 29,13; 5,000 hits each) while garrisons shoot them from range 2–3. |
| **Body order: MOVE first, ATTACK, then one MOVE last. No boosts.** | t878,470 JSON: Forest967 `m(0)m(0)m(0)m(80)m…m a×17 m`; Failsafe204 `m×11 h×7`. No part carries `boost` in any replayed wave. |
| **The claimer travels separately.** | W17S22: Quake773 (2M2CL) walks its own line, 33,41 → 23,19. |

### 1.3 How our ranger died (inferred from code and the t880,341 snapshot)

1. **It went alone.** The raised-home garrison is `Tuning.RangerResidents` = 2 bodies of `RangerResidentBlocks` = 7 (`src/Core/Types/Rules.fs:524-525`). They were cast about 340 ticks apart (Spawn8 at ~879,853 and ~880,193). Each body is matched to `Guard W17S25` independently, by travel cost (Pool cap `src/Core/Decide/Pool.fs:952-953`). Nothing makes the first body wait for the second.
2. **It was sent onto the melee's range-1 ring.** With an armed Threat in the home, the resident ground is dropped (`Threat.fs:185-187`, `raised` filters rooms in `armed.Ring`). `areaFor (Guard room)` then falls to `Threats.Ring` (`Pool.fs:247-255`), which is "walkable tiles within range 1 of a Threat" (`Threat.fs:141-158`). A ranger's Work Area under a melee raid is therefore the set of tiles where 2 × 510 lands on it.
3. **It shot the tapper first.** `guardTarget` sorts on `not claimsAFlag, not isArmed, distance, id` (`Emitter.fs:416-417`). Rune908 (600 hits, 3M3CL) stood beside both healers. 140 damage against 168 heal is −28 a tick, so it never dies.
4. **It could not shoot the healers at all.** `guardShoots` = `isArmed || claimsAFlag || harassTarget` (`Facts.fs:274-277`), and `isArmed` is ATTACK or RANGED_ATTACK (`Sightings.fs:45-58`). An `11M7H` is never a target outside a harassment room.
5. **It could not run, and was lamed first.**
   - Fighters are exempt from Flee (`Emitter.fs:286`).
   - The ranger pattern puts MOVE first "so damage lames it before it disarms it" (`Bodies.fs:83-90`, ADR 0078 §1). That is right for a ring-holder and fatal for a body that must out-walk melee.
   - 4,200 hits + 84 self-heal against 1,020 a tick is about 5 ticks.
6. **The relief was correctly withheld, for the wrong reason.** `rangersWanted` adds `Engine.guardCap` relief only "while some ranger size wins" (`Quota.fs:482-494`). The largest ranger, R8 at 160 damage, is below 168 heal, so `rangerBlocksBeat … rangerBlocksMost` is false. The exchange can only ask "does one body win". It never asks "do three". So the colony concluded "unwinnable" and fed residents in one at a time.

### 1.4 W17S23, t878,435–878,495: Odiodin's garrisons won (the premise needs correcting)

Four `20M16R4H` garrisons faced Trepidimous's Lee339 (10M10A), Inverse251 (6M4H), Forest967 (18M17A), Failsafe204 (11M7H) and two `2R5M3H` kiters.

- **Focus fire.** All four shoot one target:
  - Inverse251 goes 1000→840→408→−24 (t878,437–440);
  - Lee339 goes 1568→…→−32 (t878,438–442);
  - Failsafe204, **the healer first**, goes 1800→1564→1168→772→376→104→−192 (t878,465–471). That is −396 a tick = 3 × 160 − 84 self-heal, exactly the engine arithmetic;
  - then Forest967 unhealed goes 3340→…→−140 (t878,470–481).
- **Range.** The garrisons stand 2–3 tiles off and step back.
- **The one loss.** `garrison-877906` was caught (t878,467–478). Its body is MOVE-first, so the first 1,900 damage stripped 19 MOVE (`m(0)…`, `f16`–`f28` fatigue). It could not step away and died.
- **No other combat loss.** `876843` and `876877` vanish at full hits on t878,461 and t878,495. That is name + ~120 spawn + 1,500 life, so they died of age.
- **Result.** Trepidimous lost 4 bodies and Odiodin lost 1. W17S23 was not "taken off four garrisons" in this window.

### 1.5 W17S22, t878,903–879,069: three garrisons held

The raid was Torque592 (18M17A), Vibe199 (11M7H), Genie669 (6M4H) and Quake773 (2M2CL), then Nail636 (18M17A) with Acid582 (11M7H). The room is RCL2: 1 spawn, 5 extensions and 17 ramparts, no tower.

- **Healers first, from range 2–3.**
  - Two garrisons at 24,12 and 25,12 shoot Genie669 at 27,13: 888→672→484→296→96→−128 (t878,959–965). That is −188 a tick = 320 − (48 self + 84 Vibe).
  - Then Vibe199: 1640→…→−16 (t878,966–977), −236 a tick = 320 − 84.
  - Then Torque592 dies unhealed by t878,986.
- **Ramparts and chaff absorb the melee.** Torque592 and Nail636 hit spawn ramparts 28,10 and 29,13 for about 20 ticks. 3M3A and 4M4A defenders die in its reach (t878,904, t878,974).
- **Wave 2 repeats it.** Acid582 dies by t879,069 and Nail636 by t879,065.

**What this shows.** Both of Odiodin's wins are the same pattern:
- 2–4 ranged bodies together;
- every gun on the healer first, then the melee;
- standing at range 2–3 and stepping back;
- the melee distracted by structures or chaff.

The healer dies because 2 garrisons (320) beat one healer's self-heal plus one partner (132–168). No single body of ours can reach 168.

## 2. How we fight today, and what is missing

| Concern | Today (file:line) | Missing for squad play |
|---|---|---|
| **Who fights where** | One `Guard room` Task per guarded room. Capacity is `Capacity.fighters n` (`Pool.fs:952-958`). Each body is matched alone by travel cost. | **Assembly.** Nothing holds a body until its partners stand beside it, so bodies trickle in (§1.3). |
| **Ground** | Outposts get `Threats.Ring`, range 1 of every Threat (`Threat.fs:141-158`). Resident rooms get the controller's or Reactor's ring in peace and the threats' ring under a raid (`Threat.fs:166-195`). Harass rooms get an ambush ring (`Threat.fs:197-251`). The source ring is the fallback when blind (`Pool.fs:247-255`). | **Kite ground.** A ranged body has no "range 3 of the target, outside every melee's range 2". |
| **Movement** | Per-creep candidates, arbitrated per room (`Resolver.fs:26-170`, `resolveRooms` ~`:520`). A fatigued body is `tired` and holds its tile. | **Moving in step.** No leader/follower, no "wait for a tired partner", no 2×2. |
| **Targets** | Per creep: claimer first, then armed, then distance to a Post or to self, then id (`Emitter.fs:383-421`). Only `guardShoots` hostiles (`Facts.fs:274`). | **Focus fire** (one squad target, kept until dead), **healers as targets**, and **target lock**. A rule that re-sorts on "still armed" ping-pongs between two half-dead melee in the simulator (Appendix A). |
| **Healing** | `healReflex`: self if hurt, else the most-hurt within 1, else within 3, counted off missing hits (`Emitter.fs:450-505`). | **Pre-heal.** Our reflex heals only a body that is already missing hits. Trepidimous pre-heals every tick (§1.2), and the engine applies damage and heal together before the death check (`engine/processor/intents/creeps/tick.js:120-135`). We also have no healer role at all (ADR 0078: "No medic row"). |
| **Retreat** | Fighters never Flee (`Emitter.fs:286`). An idle fighter walks home (`Entry.fs:342-348`). | **Retreat and regroup thresholds.** A hurt body cannot step behind its partners, and a lost fight cannot fall back to rally. |
| **Engage/size** | `exchangeWon` takes one side's damage, heal and hits (`Quota.fs:316-341`). `blocksBeat` multiplies one block (`:349-362`). `homeHolds` asks for "some one body" (`:381-387`). `guardBlocksReach` is "one body and never the row's two summed" (`:420-431`). Healers are "priced in the healing and never in the hits" (`:312`). | **A squad exchange.** It must price the squad summed (valid only once assembly exists), give the raid's healers hits (they can be killed first), carry a kite term (melee damage ≈ 0 while out-walked), include melee strike-back, and keep kill order. |
| **Casting** | Rows are per body: `guard` and `ranger` patterns, census by cut (`Spawns.fs:317-331`). Relief is "guardCap more while a single size wins" (`Quota.fs:482-494`). | **Casting a composition** (roles in one order), and casting it when no single body wins but a squad does. |
| **Body** | `rangerPattern` puts MOVE first (`Bodies.fs:87-91`). `guardPattern` is `T, M×5, A×3, H` (`Bodies.fs:71-74`). | **Role bodies**: a kiter with MOVE after its guns; a brawler `25M25A`; a medic `18M18H`. |

## 3. Pricing the counter

### 3.1 Engine rules used

All of these come from `engine/` or `Rules.fs`.

**Damage and heal**
- **ATTACK** 30 at range 1 (`Rules.fs:111`).
- **Strike-back.** A melee hit on a creep that has ATTACK parts deals that creep's ATTACK power back to the attacker, unless the attacker stands on a rampart (`engine/processor/intents/_damage.js:16-19, 86-91`, `EVENT_ATTACK_TYPE_HIT_BACK`). Our exchange model ignores this. It makes a bigger melee win melee-on-melee.
- **RANGED_ATTACK** 10 at range 1–3 (`Rules.fs:117`).
- **rangedMassAttack** 10 × {1, 1, 0.4, 0.1} by range 0–3 (`engine/processor/intents/creeps/rangedMassAttack.js:32`). Against a 2×2 block at range 3 that is 1 per part per target, which is useless. Keep single-target `rangedAttack`.
- **HEAL** 12 adjacent, 4 at range 2–3 (`Rules.fs:124-127`).
- **Order within a tick.** Damage is applied, then heal, then the death check (`engine/processor/intents/creeps/tick.js:120-135`). So a pre-heal lands on the same tick as the hit.
- **Which parts work.** Damage strips parts from the head of the body. `calcBodyEffectiveness` counts only parts with hits above 0.

**Movement**
- **Fatigue** = (non-MOVE, non-CARRY parts) × 2 on plain, 10 on swamp, 1 on road (`engine/processor/intents/movement.js:204-239`). Each MOVE removes 2 a tick.
- **Plain.** Both sides are 1:1 (their `18M17A`, `11M7H`; our R7 and R8), so everyone moves 1 tile a tick and kiting holds distance in the open.
- **Swamp** (ticks per tile):

  | Body | Ticks per swamp tile |
  |---|---|
  | Their 18M17A | ~4 |
  | Their 11M7H | ~3 |
  | Our R8 (24M) | ~5 |

  **On swamp their melee out-walks our rangers.**

**Cost, size and time**
- **Part costs.** MOVE 50, ATTACK 80, RANGED_ATTACK 150, HEAL 250, TOUGH 10, CLAIM 600.
- **Body limits.** 50 parts at most, 100 hits per part, 3 spawn ticks per part, 1,500-tick life.

**Boosts: none possible today.** A live read at t884,4xx found 0 labs in every colony. Minerals are O in W12S28, W13S28 and W15S28, H in W17S29 and U in W12S26. No K, L, G or Z. UH (ATTACK ×2) would need U + H and labs. The ranged, heal and tough boosts (KO, LO, GO) need K, L or G, which we lack.

**Our capacity (live).** W15S28, W12S28 and W13S28 are each RCL7 with 5,600 capacity and 2 spawns. W12S28 is **RCL7, not RCL8**: no 12,900 bank exists today. Storage holds 93,659 (W15S28), 138,786 (W13S28) and 8,014 (W12S28).

**The raid costs Trepidimous 11,070 energy:**

| Body | Count | Cost each | Hits each | Damage or heal a tick |
|---|---|---|---|---|
| Melee 18M17A | 2 | 2,260 | 3,500 | 510 damage |
| Healer 11M7H | 2 | 2,300 | 1,800 | 84 heal |
| Tapper 3M3CL | 1 | 1,950 | 600 | — |

Totals: 1,020 damage and 168 heal a tick.

### 3.2 Candidate squads against that raid (simulator, target lock, healer-first unless noted)

Columns:
- **Stand-up**: every tick in contact, which is the worst case for rangers.
- **Kite 1/3**: the melee reach a member 1 tick in 3.
- **Kite 0**: the melee never reach.

`t` is the tick the fight ends. "Lose n" is the number of our bodies lost.

| Squad | Energy | Parts / spawn-ticks (2 spawns) | Stand-up | Kite 1/3 | Kite 0 |
|---|---|---|---|---|---|
| 1 × R7 `21M14R7H` (what we sent) | 4,900 | 42 / 126 | dies t6, kills nothing | dies t19 | stalemate (140 < 168) |
| 2 × R7 (the resident pair) | 9,800 | 84 / 126 | both die t11 | both die t43 | **win t51**, 0 lost |
| 2 × R8 `24M16R8H` | 11,200 | 96 / 144 | both die t13 | win t63, lose 1 | win t43 |
| **3 × R8** | 16,800 | 144 / 288 | all die t20 | **win t28, 0 lost** | win t28 |
| 4 × R8 | 22,400 | 192 / 288 | win t29, lose 2 | win t21 | win t21 |
| Guard row's largest body (5 blocks, 3,750) | 3,750 | 50 / 150 | dies t6 | — | — |
| **1 duo**: brawler `25M25A` + medic `18M18H` | **8,650** | 86 / 150 | **win t7, 0 lost** if they hit our brawler. Win t12, lose the medic, if they hit our medic. **Lose** if we go melee-first and they hit the medic. | — | no contact |
| 2 duos | 17,300 | 172 / 300 | win t6–t11, lose ≤1 medic | win t16 | no contact |
| duo + 2 × R8 | 19,850 | 182 / 300 | win t6–t10, lose ≤1 | win t15 | win t43 |

How to read it:
- **Kill order is the whole game.** Healer-first wins every row that wins. Melee-first leaves both healers alive in every ranger row.
- **Why the duo wins.** Two engine rules do the work: strike-back (each of their melee takes 750 a tick for hitting our brawler) and pre-heal (216 a tick on the brawler).
- **The duo's risk** is a raid that targets our medic, which is why the medic walks *behind* the brawler.
- **The ranger rows depend on kiting.** They live or die by how often the melee are in contact.
- **Every row beats per-body pricing.** No single body we can cast wins (`rangerBlocksBeat … 8` is false; the guard at 5 blocks dies in 6 ticks). Several squads of the same per-body sizes do. That gap is exactly what per-body pricing cannot see.

### 3.3 Terrain and timing at W17S25

**Terrain** (`Game.map.getRoomTerrain`, t884,4xx):
- The controller at 15,36 sits in a pocket. There is wall to the west (x ≤ 15) and east (x ≥ 24), and a corridor 3–4 tiles wide runs south (x 16–19, y 40–44).
- The only kiting room is the plain band to the north (y 28–34, x 0–27).
- That band has swamp patches at x 4–12, y 21–29 and x 28–34, y 31–34, where their melee out-walk us.

**What that means for each squad:**
- A ranged squad approaching from the north can shoot a healer of the parked block at range 3 while its melee are at range 4. One melee step brings them to range 3 and not to 1, so the squad steps back.
- The squad must never kite *south* into the corridor.
- A melee duo has no such constraint.

**Timing:**
- **Spawn.** A duo takes about 150 ticks on W15S28's two spawns. 3 × R8 takes 288 spawn-ticks, plus three 5,600 refills from storage.
- **Travel.** About 250 ticks from W15S28.
- **Life on station.** About 1,500 − 150 − 250 ≈ 1,100 ticks.
- **The enemy's travel.** W17S24 (Trepidimous, RCL4) borders W17S25, and W18S26 is two rooms off. Their replacement lands in under 100 ticks.

## 4. Design that fits this codebase

The constraints are:
- decisions are pure Core functions over `ColonyView`;
- the Matcher assigns creeps to pooled Tasks and remembers only assignments;
- the Resolver arbitrates every move per room;
- ADR 0082 never runs combat light.

### 4.1 Squad: derived, not stored

- **A Squad is the set of creeps holding one `Fight of room` Task.** That is a new Safety-tier Task beside `Guard`, which is kept for single-body outpost guarding.
- **Roles are a fact of the body**, by part cut, as `isGuardParts` and `isRangerParts` are today:
  - **Brawler**: ATTACK, no HEAL;
  - **Medic**: HEAL, no weapon;
  - **Kiter**: RANGED_ATTACK.
- **Composition is the Task's Capacity per role.** The Matcher already counts holders against capacity by class (`CapScope.Fighters`, `Matcher.fs:193-213`). Extend it to count by role.
- **No new Memory leaf.** Membership is the assignment table, which is already persisted. Whether the squad is assembled, engaged or retreating is derived each tick from member positions and hits (§4.2, §4.7). There is no "launched" flag.

### 4.2 Muster: launch only when complete

- **Ground.** `areaFor (Fight room)` returns the **rally ground** until the squad is complete, and the formation ground after that.
  - Rally ground is a declared or derived tile set one room short of the target: the last transit room on the chain, outside every Reach.
  - "Complete" means every role slot is held, and every holder stands in the rally ground or within range 2 of the leader.
- **The trickle stops in the pool, not in the mover.** A lone first arrival simply has rally tiles as its Work Area.
- **Casting.** The cast takes the squad's roles in one order: brawler, then medic, then kiters. The rows are census-cut per role exactly as the guard and ranger rows are, so `fightingRowStands` keeps holding the reserver seat (ADR 0072 §2).
- **Hysteresis.** A launched squad that loses a member below "complete" does not recall the survivors to rally while it is in contact. Retreat (§4.7) owns that case. Engagement is read off contact: any member within range 3 of a target this tick.

### 4.3 Formation and the mover

**The duo (pair) first.** It is the shape both Trepidimous and the simulator favour.
- **Leader.** The brawler, or the front kiter for an all-ranged squad.
- **The leader's candidates are its usual ones**, with one exception: it holds its tile when any member is `tired` (fatigue > 0), or when a member is not adjacent. The Resolver already computes `tired` (`Resolver.fs` `movementOf`).
- **Each follower's candidate head is the leader's current tile.** The engine moves a creep into a tile vacated the same tick. Its tail is the leader's neighbours.
- **Arbitration.** Pass the squad as one weight: the follower's rank equals the leader's, so a chain seating both outweighs a bystander (`Resolver.fs` weight rule, #237).

**The quad (2×2) is deferred.** It needs all four non-fatigued, a 2×2-passable path cost, rotation, and room-edge crossing. Neither fight we studied needed it: Trepidimous moves in a snake and only parks 2×2. Revisit for towers or siege.

### 4.4 Targeting

- **One target per squad per tick, chosen in the Emitter before per-creep intents.**
- **Widen `guardShoots` to a squad context.** Any hostile body with ATTACK, RANGED_ATTACK **or HEAL**, standing within range 3 of an armed hostile, counts as part of the raid.
- **Kill order** is the lowest *effective hits*: hits + (heal that can reach it this tick) − (our squad damage reaching it). Among targets our damage can actually break, a healer comes first. Without a hard lock this is near-self-locking, because the target we hurt stays the lowest. That is the cure for the simulator's ping-pong.
- **Required:** `HostileInfo` gains `Hits` (and, later, per-part hits or boosts) in `World.fs`. Today it carries the body only (`Sightings.fs:23-39`). This is a heap projection, not Memory, so it needs no wire-check case.
- **Each member** issues `RangedAttackCreep` or `AttackCreep` on the squad target if it is in reach. Otherwise it shoots the in-reach hostile with the lowest effective hits. ADR 0078's single-target `rangedAttack` stays.

### 4.5 Healing

- **Medic, in a squad:** heal the member that will be hit this tick: the one adjacent to the most enemy ATTACK parts (`Sightings` positions), at full hits too. Otherwise heal the most hurt. This is a pre-heal, which the engine credits before the death check (§3.1).
- **Kiter:** self-heal when hurt, else pre-heal the leader if adjacent. `heal` and `rangedAttack` do not conflict (`IntentPlan.fs:21-71`).
- **Where it lives.** A squad branch of `healReflex` (`Emitter.fs:450`). The colony-wide reflex is unchanged for everyone else.

### 4.6 Kiting (ranged members against melee)

**Kite ground** = tiles within range 3 of the squad target, minus tiles within range 2 of any hostile with an active ATTACK part, minus swamp when a melee is within range 4.

Why range 2 is enough:
- Engine actions resolve on start-of-tick positions.
- A melee must start the tick adjacent to hit.
- So a ranger that ends each tick at range ≥ 2 is never hit while it has a free step.

If the kite ground is empty, fall back to the room's Safe set (`Threats.Safe`), which is already derived.

The ranger body for squads puts guns before legs: `RangedAttack×16, Move×24, Heal×8`. Damage then disarms before it lames. That is the opposite of ADR 0078's ring-holder and of Odiodin's `877906`, which lost its MOVE parts first and died (§1.4).

### 4.7 Retreat and regroup (derived each tick)

- **Member retreat.** The member's hits are below the raid's damage that can reach it in 2 ticks. It takes the kite ground's far half, and the medics' heal goes to it.
- **Squad retreat.** The squad exchange (§4.8), re-run on *live* hits, turns lost. The whole squad's area becomes the rally ground, and launch resets.
- **Regroup.** At rally until every member is above 80% hits (a `Tuning` number), then launch again.

### 4.8 Squad exchange: one pure function

`squadWins view room (members: BodyPart list list) (kite: bool) : bool` is a bounded tick simulation, at most 60 ticks and at most 10 bodies. It applies the §3.1 rules: parts stripped from the head, strike-back, heal after damage, kill order as §4.4, and the raid's melee striking our front (the brawler, else the lowest).

When `kite` holds, the raid's melee deal 0. `kite` holds for an all-ranged squad when:
- our slowest member is at least as fast as their fastest melee on plain; and
- the target room's Safe set is non-empty.

It reads `view.Hostiles` only, as `exchangeWon` does.

It has three readers:
1. **Engage or hold.** A complete squad launches only if `squadWins` is true on live hits.
2. **Cast.** Take the cheapest composition from a small fixed catalogue that wins within the casting colony's bank and spawn time: duo, duo + R8, 3 × R8. If none wins, there is no cast, and the colony keeps the ADR 0072 / ADR 0080 stand-down and safe-mode answers.
3. **Stand-down.** `rangersWanted` and `homeHolds` (`Quota.fs:381, 482`) ask `squadWins` of what is standing.

It **replaces** `exchangeWon` only where the summed-bodies objection (`Quota.fs:420-428`, "the second may arrive late") is answered by muster. Outpost guards against NPC invaders keep `guardBlocksBeat`, which is right for one body against one invader.

### 4.9 How it fits the existing rows

- **Residents** (errand rooms and raised homes) keep the ring in peace. When an armed raid arrives and `homeHolds` is false, they become the first members of the room's `Fight` squad. Their Guard is not pooled while a Fight is.
- **ADR 0080 / #447 relief** becomes "cast the squad the catalogue prices" in place of "guardCap more bodies of the size that wins alone".
- **Outpost guards are unchanged.**
- **Harassment (ADR 0081) is unchanged at first.** The longbow duel at W17S25, t879,117–879,152 (our R4 against Shibdib's `8R10M2H`), is a one-on-one fight. The global caster choice (largest bank in reach) is reused for squads.

### 4.10 Light ticks and CPU

- **Combat is already never light** (ADR 0082 §4). Muster at rally is quiet and stays light-eligible.
- **In transit**, a pair can split on a light tick when one member is fatigued and the other steps its plan.
  - Add one force rule in `LightTick`: a squad member whose partner is not adjacent, or whose partner's plan head is not its tile, forces a full tick.
  - The cost is one check per squad member.
- **Per full tick with a squad:**
  - kite ground ≤ 49 tiles per kiter;
  - one target choice per squad;
  - `squadWins` at ≤ 60 × 10 body steps per candidate composition, at most 3 candidates per raided room. That is microseconds against today's ~44 ms mean (memory `session-2026-09-30-cpu`).
  - The exchange runs only while hostiles stand in a guarded room, which `Guarded` already gates.

## 5. Stages, each shippable with a test seam

| Stage | What ships | Test seam (domain owner per `docs/agents/orchestration.md`) |
|---|---|---|
| **S0: stop feeding bodies** | (a) A ranger's Guard ground under a melee raid is kite ground (§4.6), not `Threats.Ring`. (b) A healer standing with an armed hostile is a target. Kill order is lowest effective hits, with `HostileInfo.Hits` added. (c) A fighter pre-heals an adjacent fighter that stands in a Reach. (d) A raid that no single body wins casts no resident into it. Residents already standing hold at Safe ground. | `Pool.areaFor` gives a ranger no tile within range 2 of an ATTACK hostile (Pool tests). `Emitter.guardTarget` picks the healer over a full-hits melee (Emitter tests). `healReflex` heals a full-hits fighter in a Reach. |
| **S1: squad exchange** | `Quota.squadWins` (§4.8) and the catalogue. Read-only at first: printed in `observe quotas` next to `rangerBlocksBeat`. | `QuotaTests`: the t880,341 raid. 1 × R7 loses; 2 × R7 wins kite only; 3 × R8 wins kite; duo wins stand-up healer-first. Fixture from §1.2's bodies. |
| **S2: Fight Task and muster** | The `Fight room` Task, role cuts, per-role Capacity, rally ground in `areaFor`, and the role casts in `Spawns`. | Planner/Pool: an incomplete squad's area is rally tiles. Matcher: a 2-role Capacity admits a brawler and a medic and refuses a second brawler. Spawns: the roles cast in order. |
| **S3: pair movement** | Leader/follower candidates and wait-for-tired in the Resolver, plus the LightTick force rule. | Resolver tests: the follower takes the leader's vacated tile; the leader holds while its partner is fatigued. LightTick test: a split pair forces a full tick. |
| **S4: retreat and regroup** | §4.7 thresholds, derived. | Pool or Emitter: a member below the threshold gets far-half kite ground; a lost exchange returns the squad's area to rally. |
| **S5: role bodies** | Brawler `25M25A`, medic `18M18H`, kiter `R16 M24 H8` (guns first). | `Bodies` tests on order and cost. `profile.mjs` harness scenario with a raided nursery (memory `profile-harness-as-regression-gate`). |
| **S6 (deferred): quad, towers, boosts** | Only after labs exist and a siege is chosen. | — |

S0 alone would not have won W17S25 (§3.2 row 1), but it stops paying a ranger per siege. S1 + S2 + S5 with the duo is the smallest set that wins the replayed raid.

## 6. ADRs and user calls

**ADRs**

- **New ADR: "A raid no single body wins is fought by a squad, priced and launched as a whole."** It covers the `Fight` Task, muster, `squadWins`, and the catalogue. It supersedes the "one body, never summed" clause of ADR 0072 (`guardBlocksReach`), and amends ADR 0080 §3 (the relief becomes the squad) and the ADR 0078 #447 amendment (relief only "while some ranger size wins").
- **Amend ADR 0078:**
  - a ranger under a melee raid kites rather than holding the threats' ring;
  - healers become targets;
  - a squad kiter's body is guns before legs, while the resident keeps MOVE first;
  - "No medic row" is reversed for squads.
- **No ADR for tuning:** regroup hits, rally distance and the catalogue sizes get a why-comment or a test name.

**The user's calls**

1. **Fight Trepidimous at all, and where.** W17S25 is given up (`470e786`). A squad at W17S25 fights at a 4:1 reinforcement disadvantage next to Odiodin's rooms (memory `neighbours-diplomacy`: tell Odiodin first).
2. **Spend 8,650–19,850 energy per engagement.** This is the first time a cast would be a composition rather than a body.
3. **Duo-first or ranger-first.**
   - The duo is cheaper and wins stand-up, but cannot catch kiters.
   - The trio needs open ground and a full 288 spawn-ticks.
   - Recommendation: build the duo first. The S0 kite ground protects the rangers we already field.
4. **Who casts.** The W15S28 mother alone, or the global "best bank in reach" of ADR 0081. W13S28 holds 138k energy.
5. **Boost programme.** Labs plus K, L or G from the market or trade. Out of scope until a siege is chosen.

## Appendix A: simulator rules and limits

The simulator (`sim_lock.py`, scratchpad) runs a tick loop.

**Rules**
- Each side focuses one target, locked until it dies.
- Our heal goes to the most-threatened ally, and theirs likewise. A healer with no hurt ally pre-heals the target that is about to be hit.
- Damage strips parts from the head. Damage is applied, then heal, then the death check.
- ATTACK 30 at range 1 with strike-back, RANGED 10, HEAL 12 adjacent.
- "Contact" decides whether melee deal damage that tick: always, every k-th tick, or never.

**Limits**
- No positions, swamp, ranged heal at 4, or fatigue loss from stripped MOVE.
- The raid always strikes our front.
- A fight "ends" when the raid has no active weapon, so a disarmed melee counts as a kill.

**Checked against the replays.** The same arithmetic reproduces every per-tick hits delta quoted in §1.4–§1.5 to the hit. Examples: −396 = 3 × 160 − 84; −188 = 320 − 132; −236 = 320 − 84.

**Replay tool.** `replay.mjs` (scratchpad) is `observe history` run per tick over a range, with part order and boosts shown.
