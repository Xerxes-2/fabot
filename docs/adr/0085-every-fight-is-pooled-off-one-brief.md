# Every fight is pooled off one Brief: a room's targets, the bodies fit for them, and the ground they hit from

> **Status:** accepted, amended by 0086

> **Accepted 2026-10-05**, the user deciding (see *Answered*), on an audit of main 5afc1713. S0's characterisation tests are marked `Characterisation (the Brief's S0)`.

Ten combat behaviours were added one issue at a time, and each answers the same sentence: *a room holds targets worth hitting; send a fitting body to fitting ground to hit them.* Each one wrote its own copy of that sentence's seven parts. Today's sessions (#504–#517) were mostly spent on those copies drifting apart.

## Context

### Inventory

Every behaviour below is a Safety-tier Task. The table is the S0 inventory as it stood; the stages amend the code, not the table (since #528: a raided Outpost Guard, a raided Resident Guard, a Chase in sight and its room's Fight rank TwoRungsUp, a Trespass kill and a dark Chase OneRungUp, and the Trespass admit has no `underRaid` gate). "Sites" counts the Decide functions (`Decide/*.fs`, file:function) that name the behaviour or its discriminator; the second number counts every function in `src/Core` and `src/App`. Appendix A lists them.

| Behaviour | Targets | Who may take it | Ground | Rung / capacity | Stand-down coupling | Casting row | Sites |
|---|---|---|---|---|---|---|---|
| **Outpost Guard** (ADR 0056, 0072, #366, #417, #450) | `guardShoots`: armed; `killRank`, nearest a Post | `isGuardRowBody` (named guard cut) | `Ring`: range 1 of each Threat; the source ring while blind (#366); exit-hold ground while the room is empty (#450) | tier; `guardsWanted` (1, or 2 when one block loses) | ADR 0043: `raidDeadlines` opens a stand-down when the bank's biggest guard loses (`guardBlocksBeat`); a shut room loses its Guard; `ThreatenedOutposts` keeps a blind Guard pooled | guard row: `guardQuota` over `guardedSplit`'s first half, worst room's `guardBlocksFor` | 24 / 26 |
| **Level-0 core** (#487) | the core (`AttackStructure`) | guard row | `cores` ring, unless something listed earlier in the precedence answers first | as Guard | opens no stand-down; a stronghold makes the room impassable | guard row at `guardBlocksReach` | part of Guard |
| **Resident garrison** (ADR 0077, 0078, 0080; #411, #414, #419, #447, #479) | armed, a rival's claimer, a raid healer | `isRangerRowBody`; lent to a kill next door only while home is quiet (`residentQuiet`) | `ResidentRing` (Reactor or controller ring) in peace. Under a raid, `Kite` tries four in turn: ramparts within 3 of the head, then the safe set when outmatched, then kite ground, then the farthest tiles | tier; `rangersWanted + RangerResidents` | errand: stand-down when the ranger loses and does not kite; raised home: never; outmatched means 0 cast | ranger row: `rangerQuota`, `rangerBlocksFor` | 30 / 38 |
| **Harassment ranger** (ADR 0081, #432, #437, #439, #457) | `harassTarget`: the declared enemy's creeps, armed or not | ranger row; one per room plus a relief (`Capacity.relieving`) | `HarassRing`: an ambush around the diggers first, then at the source, then caught bodies, then the Stand's seats, kept off the seats in a mined room. `Kite` under an armed raid | tier; `rangersWanted` = 1 | `HarassSighting` basis lasts 50 ticks and overrides other bases (`harassOverrides`); a shut room stays scanned | `World.harassCasters` picks the colony; ranger row at `HarassFloors` | 14 / 21 (+ World) |
| **Defender** (#504, ADR 0034) | `AtLine`, the body with the fewest hits beside it (`defendIntent`) | `defenderPattern` by name only | `Defend`: free perimeter ramparts beside the raid, else the nearest ones; walks round `DefendAvoid` | tier; `defendersWanted` (≤ `DefenderCap`) | none (home); the safe-mode reflex is the fallback | defender row first; holds the economic rows (`heldForDefender`) | 29 / 31 |
| **Chase** (#510) | a raid every body of which `cannotClose` | ranger row that kites the raid down with its own parts (`kitedDown`), plus `rangerMayKill`, plus the Matcher's `called` | `Kite` with escape room; while blind, every walkable tile off the border; avoids each melee's step and swing and the exit camp | **OneRungUp**; 1 | the episode still opens. `OutpostEpisode.Chased` is carried through `StandDown.ChasedOutposts`, `ScanSet.chasing`, `ColonyView.ChasedOutposts` and `Threats.Chased`, so the room stays projected | none | 15 / 27 |
| **Trespass kill** (#516) | `trespasser`: a declared enemy's unarmed WORK, CARRY or CLAIM body | `not underRaid`, and a guard row body or a ranger with `rangerMayKill`; `called` | beside the digger or claimer; a hauler is waited for at its store; keyed across the exit row; avoids the whole Reach | **OneRungUp**; `max 1 guardsWanted` | none | none | 17 / 17 |
| **Fight** (ADR 0083, #451–#456) | `guardShoots`, plus any healer in the room | squad casts by name role; residents once launched | per role: `Rally`, `Front`, `Behind`, `Ranged` (`withFights`) | tier; `Capacity.roles` | `Fought` latch, `FightHold`, `fightCooling` | squad rows (`squadQuota`, residents subtracted) | 33 / 45 |
| **Assault** (ADR 0084, #490, #499, #500 parked) | breach ramparts, then the declared targets, then towers and spawns | squad casts of slot roles | `Rally`, `Front`, `Behind`, and `wayOut` (`withAssaults`) | tier; `Capacity.roles` | none; a `RivalSafeMode` window and `Taken` decide instead | squad rows | 10 / 21 |
| *Fire / heal / safe-mode reflexes* | `planFire`: urgent targets, then those the `killable` check passes | towers; any HEAL part | none | none (they are reflexes) | `homeHolds` (safe mode) | none | — |
| *Flee / Shelter / Evacuating* (#512, #514) | none: these bodies move away | `runsFromThreats`, plus a defender with no line | the room's safe set; `Shelter`; the walk home round the Reach | the Flee tier | the `Evacuating` scan rooms | none | 10 / 14 |

### Where the copies disagree

1. **One Task id holds five flavours.** Outpost guards, cores, exit holds, residents, harassment, chases and trespass kills all pool as `guard:<room>`. The id cannot say which flavour it is, so each reader works it out again from the room's class:
   - `Emitter.applicable` (4 arms)
   - `Pool.capacityOf` (5 arms)
   - `Pool` rung (`isKill`)
   - `Pool.areaFor` (the chase fallback)
   - `Threats.guardGroundIn` (5-step precedence)
   - `Quota.guardedSplit`, `rangersWanted`, `rangerBlocksFor`
   - `Resolver.movementOf` (`kiters`, trespass avoid)
   - `Matcher.called`
   - `Planner.planTasks` (`holding`, `chasing`, `trespassed`)
   - `Observe.raidDeadlines`

   That is **13 dispatch sites**.
2. **The precedence is written four times over different subsets.** In `threatsHeldBy`:
   - `holds` excludes Threat rooms, residents and harassment rooms.
   - `cores` excludes those three plus holds.
   - `trespass` excludes Threat rooms, ranger rooms, chases, holds and cores.
   - `guardGroundIn` orders Kite, then resident, then harass, then trespass, then ring.

   No one list is authoritative. For example, a chased room with the raid out of sight and an exit hold gets the melee guard's range-1 exit-hold ground, while the mover's kiter rule (#510) keeps the ranger two or more tiles off the exit tile. Two features' grounds disagree, and it works only because the exit run is wider than the avoid set.
3. **"Is a raid here?" is asked about 15 times with 5 predicates.** The predicates are `weaponRange`/`isArmed` (body parts, destroyed ones included), `isRaider`, `hurtsLive`/`activeCount` (live parts), `guardShoots`, and LightTick's `Armed`/`Hurts`. The sites are:
   - `threatsHeldBy.armed`, `.shelter`, `.contested`
   - `guardedOutpostsOf`, `defendedHomesOf`
   - `residentQuiet`, `mayBeCalled`, `underRaid`, `fightSquadIn`
   - the `Emitter` chase arm
   - `raidAtLine`
   - `raidDeadlines`
   - `safeModeIn`
   - `fightReports`
   - LightTick's forcing rule

   Consequence: a raider whose weapons are shot off still has a Reach and still pools an outpost Guard, while the stand-down (#513 counts live parts) has already reopened the room.
4. **"Does our answer win?" has about 13 predicates over three pricings.** The pricings are `blocksBeat`/`exchangeWon`, `kitedDown`, and `squadFight`. The predicates are `guardBlocksBeat`, `rangerBlocksBeat`, `bodyWins`, `homeHolds`, `outmatched`, `residentsLose`, `kitedByRanger`, `chased`, `pricedSquad`, `guardsWanted`, `rangersWanted`, `guardBlocksFor` and `rangerBlocksFor`. The stand-down (`Observe.raidDeadlines`) and the engagement (`Quota`, `Emitter`) each pick their own, which is how #510 happened: the stand-down priced only the guard, so the residents next door were never sent.
5. **13 ground builders, three of them "ring around a thing".** `Ring`, `ringAround` (errand, home, core) and the trespass "beside" are those three. The others are `HarassRing`, `Kite` (4 alternatives), exit holds, the trespass store ground, `defendGroundOf`, `Shelter`, `FightGround` (4 roles), `AssaultGround` (3) and the `areaFor` fallbacks. `HarassRing` also picks **its own target** (digger first, `catchable`) apart from `guardTarget`'s `killRank`. The shot and the ground follow different orders, so the ground flipped back and forth (W19S25, 3b33a821, patched with more ordering in the ground). It also never learned that a room can be mined as well as harassed (84accf7f: the ranger stood on the outpost's only seat).
6. **12 of `Threats`' 15 fields serve one behaviour each.** They are `Ring`, `ResidentRing`, `HarassRing`, `Kite`, `Held`, `Fight`, `Assault`, `AtLine`, `DefendAvoid`, `Defend`, `Chased` and `Trespass`, with parallel room sets beside them (`residentRooms`, `harassRooms`, `rangerRooms`, `OutpostFacts.Guarded`). Each new behaviour adds a field, an exclusion clause in every sibling, and an arm at each of the 13 dispatch sites. #510 plumbed one bit through six layers. #504 touched 29 Decide functions.
7. **The Matcher knows a Task kind.** `Matcher.called` pattern-matches `Guard room` and reads `Threats.Chased` and `Trespass`. This breaks ADR 0052 decision 6 ("the Matcher recognises no Task kind").
8. **The squads launch in two different ways.** `withFights` and `withAssaults` each derive `Launched`, together, strung and rally on their own. #499 fixed the one-tile corridor in the Assault copy only. #500 (parked) needs that logic again.
9. **Eligibility is tied to one colony's view.** #517 (open): a mother's residents cannot see trespassers in a child's outposts, because "who may answer a room" is spelled per projected room, not per Brief.

## Decision

**Each tick, every combat Task is pooled off one `Brief` per Task id**, derived once beside `Threats` (`Decide/Brief.fs`) by an ordered list of *brief sources*. The first source to claim an id wins, and that single list replaces the four exclusion lists. Through S4, Task kinds and ids (`guard:`, `fight:`, `defend:`, `assault:`) stay exactly as they are: they are the Memory wire. After S4, stage W1 splits `guard:` into one id per flavour (Answered 2). A Brief is the per-tick configuration behind one of them.

```
Brief = { Task      : Task                       // wire identity, unchanged
          Kind      : BriefKind                  // Outpost | Core | ExitHold | Resident | Harass | Chase | Trespass | Defend | Fight | Assault
          Targets   : Selector                   // which hostiles or structures, and in what kill order
          Admits    : CreepInfo -> bool          // body cut, plus residents' and lending rules
          Recall    : Recall option              // who may be called off another Brief, within what walk
          Ground    : Ground                     // Set<RoomPos>, or per role for a Formation
          Avoid     : Avoid                      // Reach | MeleeStepAndSwing (+ exit camp) | DefendAvoid
          Rung      : Rung                       // OnTheTier | OneRungUp (a kill) | TwoRungsUp (a raid's, #528)
          Capacity  : Capacity                   // the existing kind-free record
          Asks      : RowAsk                     // Row(pattern, count, blocks) | None | Squad(roles) | Declared
          Answer    : view -> Verdict }          // Win | Kite | Lose: what the stand-down, the row and the chase all read
```

Each behaviour then becomes a configuration of the Brief:

- **Outpost:** guard selector; guard cut; `Ring`; `guardsWanted`; Row(guard); Answer from `guardBlocksBeat` at reach.
- **Core:** structure selector; guard cut; core ring; Row(guard, `guardBlocksReach`); Answer is always Win.
- **ExitHold:** guard cut; exit-run ground; no row.
- **Resident:** resident selector; ranger cut; resident ring, or the Kite chain under a raid; `rangersWanted` plus the residents; Row(ranger); Answer from `outmatched`/`kitedByRanger`.
- **Harass:** `harassTarget` with digger-first order; ranger cut; ambush ground; relief cap; Row(ranger, floor); Answer from `rangerBlocksBeat`. Its sighting clock stays on the stand-down.
- **Chase:** raid selector; ranger cut plus `kitedDown` plus `rangerMayKill`; Recall(quiet, `chaseWalk`); kite ground; MeleeStepAndSwing avoid; OneRungUp; cap 1; no row.
- **Trespass:** `trespasser`; guard or lent ranger; the same Recall; store or beside ground; Reach avoid; OneRungUp; no row. Defence first is rank, not a gate (#528): an Outpost Guard, and a Resident's or Chase's where a Threat stands, rank TwoRungsUp, the room's Fight alike.
- **Defend:** `AtLine` with fewest-hits order; defender cut; perimeter ground; DefendAvoid; Row(defender).
- **Fight:** Ground is a `Formation` (per-role grounds plus `Launched`); Capacity is roles; a Squad ask; Answer from `residentsLose`/`pricedSquad`.
- **Assault:** Formation; structure selector; a Declared ask. Its Answer is the safe-mode window, not a stand-down.

Every reader then reads the Brief and none dispatches on room class:

- The Planner pools `briefs |> Map.keys`.
- `Pool` reads the Brief for area, rung and capacity.
- `Emitter.applicable` reads `Admits`, and its target functions read `Targets`.
- `Matcher.called` reads `Recall` from the `PooledTask`, which makes it kind-blind again.
- The `Resolver` reads `Avoid`.
- The rows sum `Asks` by pattern and take the worst room's blocks.
- The stand-down reads `Answer` from the same brief source, with a Lose verdict opening the episode.

**What stays outside, and why:**

- **Towers and the safe-mode reflex** are not Briefs. They have no body to cast, no ground and no Task. They share the *target valuation* half (`Selector`: `claimsAFlag`, `healReaching`, the kill check) and the safe mode's `homeHolds` share the `Verdict`, nothing more.
- **The heal reflex** is an action-layer rule for every body.
- **Flee, Shelter and Evacuating** are the dual: a *refuge* for bodies no Brief admits. They share the Threat picture (`Reach`, `Safe`) and the single "armed" vocabulary, not the Brief.
- **Assault** fits the Brief's Task-facing half (admission, capacity, Formation ground). Its declaration, its safe-mode window and `Taken` are an offensive lifecycle with no stand-down, so they stay in `AssaultFacts`.
- **A `Formation`** is the one module both squads launch through (S8, in the plan: Answered 4). #500's edge bounce is built on it.

## Migration

Each S stage keeps behaviour. Its gate is `npm test`, `profile.mjs` Memory fingerprints unchanged on all six scenarios and on the three `--raided` variants (`outpost`, `pair`, `siege`), and the arena suites (`ArenaTests`, `ArenaClaimTests`) unchanged. Every stage ships on its own. Quirks are kept and named in a test (`quirk: …`), never fixed in passing. The W, B and R stages are the deliberate changes the user decided (see *Answered*); each moves the baseline on purpose and says which fingerprints it moves.

| # | Stage | Size |
|---|---|---|
| S0 | Characterisation: one Decide test per Kind pinning pooled id, capacity, ground, applicability and rung, in the domain files that own them; the flavour precedence table; record the fingerprint and arena baseline (below) | tests only, ~800 |
| S1 | Name the hostile predicates in `HostileInfo` (`Sightings.fs`), each saying whether it reads the parts **on paper** (every part in `Body`, shot off or not) or **live** (`Engine.liveParts`): `armedOnPaper`, `raiderOnPaper`, `weaponRangeOnPaper`, `worksOnPaper`, `claimsOnPaper`, `healsOnPaper`, `healingOnPaper`; `armedLive`, `meleeLive`, `shootsLive`, `worksLive`, `breaksLive`, `hurtsLive`, `healsLive`, `healingLive`; the owner questions `isKeeper`, `crossesExits`; and, over a part list for the light tick's glance, `weaponRangeOf`, `breaksOf`, `armedOrHealsOf`. About 48 sites read them; `Facts`' aliases and LightTick's own keeper string go. Each site keeps the semantics it had (the disagreements are listed under *S7's input* below); no semantic change | XS, ~60 |
| S2 | `BriefKind` computed once per Guard room (`Threats.Kinds`, read through `Threats.kindOf`/`roomsOf` until S4's `Brief`), replacing the room-class dispatch in `applicable`, `capacityOf`, the rung, `areaFor`, `guardGroundIn`, `guardedSplit`, `kiters`, the trespass avoid, `Matcher.called`'s kill set and the Planner's kill pooling; no fingerprint moved. The inactive-assault item needed no change: `Assault.worked` already drops an `Active = false` declaration from the scan set, the worked set and `ColonyView.Assaults`, and an assault's entry room is only ever crossed, so it never keeps a harassment room from #520's loose set; the ViewTests case pinning this is characterisation. **Open:** why W19S26 forced ~12% full ticks at t964,5xx is unexplained (likely a body of ours there that is not a harass ranger). The one real disagreement left for S6: `Quota.rangersWanted` and `rangerBlocksFor` ask the harassment before the resident, where the kind puts the resident first | S, ~200 |
| S3 | One ordered source list in `threatsHeldBy` replacing the holds, cores and trespass exclusions and the `guardGroundIn` chain, keeping today's exact precedence (the chased-room exit hold included). Done as `BriefKind.order` (Resident, Harass, Chase, Outpost, ExitHold, Core, Trespass: the trespass, which S2's list wrote fourth, never claimed a room a raid, hold or core held, so it is last) and `BriefKind.opens` (a ring kind is still open under a chase, for two reasons. It is load-bearing: a chase in sight is always a raid's room, so the raid still claims it as Outpost and that shuts the hold, the core and the trespass out. And it is the pinned quirk: out of sight, a hold or a core lays its ring there and the chase takes it); `guardGroundIn` walks the order down from the room's kind. `Planner.coredOutpostsOf` stays kind-blind: an unfuelled errand room with a core is pooled only by it (`quirk: an unfuelled errand room with a core …`), and a held exit with a core is hired for by it; no fingerprint moved | S, ~150 |
| S4 | The `Brief` record. Planner, Pool, Emitter and Resolver read it; the `Threats` fields `Ring`, `ResidentRing`, `HarassRing`, `Kite`, `Held`, `Chased`, `Trespass`, `Defend`, `DefendAvoid` and `AtLine` move behind it; the quiet-tick shortcut and the lazy safe set are kept, and nothing enters a memo key. Done as `Threats.Briefs` (one `Brief` per room a source claims or lays ground in, `Brief.unclaimed` for any other, read through `Threats.briefOf`): `Kind`; `Claims` (every claimant down the order, which is where `Held` went); `Targets` (`Shoots` and the kill order's `Head`, one selector for every kind until B1); `Admits` (the gate's arms); `Recall` (walk and `mayBeCalled`, which the Matcher reads for its kill set; S5 moves it onto `PooledTask`); `Laid` (the kite, resident, harassment, ring and trespass grounds, the five per-room maps transposed); `Ground` (what `guardGroundIn` returned) and `WalksIn` (`areaFor`'s chase fallback); `Avoid` (`Runner`, `Reach`, `MeleeStepAndSwing`); `Rung`; `Capacity` (`capacityOf`'s Guard arms). The answers that need the colony are functions of it, built per Brief (per room per tick), each closing over its room alone. `Kinds`, `Chased` (no reader left) and the five ground maps are gone with `kindOf`, `roomsOf`, `isKill` and `fightLaunchedIn`; `ringIn`, `residentRingIn`, `harassRingIn` and `guardGroundIn` stay as reads of the Brief. Moved so the Brief can be built beside `Threats`: the row cuts to `Bodies`, `guardsWanted` and `rangersWanted` to `Threat`, `Rung` from `Pool`, the Matcher's chase walk onto `Recall`. **Left outside:** `Defend`, `DefendAvoid` and `AtLine`, because the map is keyed by room and the home's Defend would share its key with the `Outpost` Brief a raid in the home claims, and the defender's avoid is read on the runner's branch, not the Brief's; they move with S8's Task-keyed sources. `Fight` and `Assault` are S8's. `Quota.guardedSplit` reads `Kind` until S6. Every fingerprint and the arena suites unchanged; the test edits are field renames | M, ~450 |
| S5 | `Recall` on `PooledTask`; `Matcher.called` loses its `Guard` match. `Recall` moves to `Types` and holds data only (`Walk`), so `PooledTask` keeps structural equality; the `mayBeCalled` predicate stays in the Matcher. Done as `PooledTask.Recall: RecallPolicy option`, set by `planPool`: `Kill(room, Recall)` on a Guard whose Brief recalls, `Callable room` on every other Guard (its holder may be called off it, which a kill's holder may be too), and `Struck room` on a struck rampart's Repair, so `outranked`, the other kind-specific clause, reads it too. `called` takes its kills, their rooms and its holders off the policy and asks `Facts.mayBeCalled` with those rooms; its kill set is now the pooled kills, not every recalling Brief, which answers alike because the only question it puts is whether a held, so pooled, room is a kill. The Matcher matches no `Guard` or `Repair` Task; every fingerprint and the arena suites unchanged, no test edited | S, ~80 |
| S6 | A row ask on the Brief; the guard, ranger, defender and squad quotas sum Briefs; `guardedSplit`, `guardQuota`, `rangerQuota` and the room halves of `OutpostFacts.Guarded` go. Briefs hold only claimed rooms, so a blind outpost (#366) has none: either the quotas keep iterating the guarded set, or every guarded room gets a Brief. Done for the guard and ranger rows as `Brief.Asks: RowAsk option` (named off [[Casting]], a body in an oven; `Row: CastRow` — `Guard | Ranger`, whose `CastRow.cut` is the row's census cut —, `Wanted: view -> Threats -> int`, `Blocks: view -> int`): a ranger kind the ranger row's, a chase none, every other kind the guard row's `guardsWanted` at `guardBlocksFor`. `Quota.rowQuota`, `rowBlocksWanted` and the reserver's withheld rooms sum the asks of `OutpostFacts.Guarded` through `Threats.briefOf`, so a blind outpost reads `Brief.unclaimed`'s guard ask and keeps its guard, and a held exit's or a trespass's ask counts only where its room is guarded too; `Guarded` stays whole. `guardedSplit`, `guardQuota`, `rangerQuota`, `guardBlocksWanted`, `rangerBlocksWanted`, `guardStands`, `rangerStands`, `rangersWanted` and `BriefKind.isRangers` are gone; `guardBlocksFor` and `rangerBlocksFor` (now handed its floor) moved beside `guardsWanted`; `Brief.Capacity` reads the ask's count, so the residents' cap and the row are one number. The S2 disagreement is the Resident ask's named quirk, for the B-stage: a resident room that is a harassment room too asks as the harassment (one ranger, at its floor). The defender and squad rows are not Brief-keyed yet (S4's room-keyed map) and move with S8. Every fingerprint and the arena suites unchanged; the test edits are text naming the removed `rangersWanted` | M, ~250 |
| S7 | `Answer` on each source; `raidDeadlines`, `chased`, `outmatched`, `guardsWanted` and `rangersWanted` read it (Observe keeps `HarassCast`'s shut rooms). With one verdict, a raid our nearest eligible bodies out-damage is answered by them, residents beside a quiet home included: the ranged harasser no row answers today (Synapse673, 13 RANGED_ATTACK 7 MOVE, no heal, in W17S26 at t964,802: the guard row loses, the chase answers melee-only raids, W15S28's two residents next door would win it). The first stage that moves behaviour, done to Answered 7's four rules. **One reading of a hostile (rule 1):** every part-question is asked *in effect*, `HostileInfo.effectiveParts side hostile` beside the hostiles it stands among: live, unless `healReaches` (a HEAL of its owner's acting within ranged heal, its own included), and then every part at full hits (`effectiveHits`). S1's paper and live pairs are gone, and each of the eleven disagreements below now reads it, `catchable`'s gait as `HostileInfo.gait` and `squadFight` starting each raider at its hits in effect. A raider whose weapons are shot off has no Reach, no Guard and no row until a healer stands by it; a body left a WORK holds a row only while a raider stands beside it (a dismantler), never alone, where no Guard answers it. A hostile's parts in effect are not tabled per tick: a whole body answers at once and a worn one scans its room's few hostiles, and an interleaved micro-bench (decide on 8 worn hostiles with 2 healers and 6 rangers over 2 rooms, 1,500 runs, 5 rounds) put main at a 2.53 ms median and S7 at 2.54 ms. **One verdict (rule 2):** `Facts.answered view room`, one function Observe calls and `threatsHeldBy` prices once a tick per room a hostile stands in, lazily, into `Brief.Verdict` (with the `rangersWin` count beside it for a chase's capacity). It is `Win` where no raider stands, or where the biggest body the room's own row casts wins (`rowWins`: the ranger the bank buys, at least one block, beside a raised home's loaded towers, in a ranger or harassed room; the melee guard elsewhere; never in a child's outpost, which no row of the mother casts for), or where the rangers standing in the room, its garrison among them, and those lent from beside it win together (`rangersWin`: in-room first, then beside it, by name, the fewest that win; a child counts her mother's residents standing in her home, `ColonyView.Lendable`, so the live Synapse673 case is answered in both views: the mother calls them and the child does not stand the outpost down). It is `Kite` where a ranger kites the raid down (#510, its healers per rule 1). Otherwise `Lose`. The stand-down opens on `Lose` alone, so an outpost's kited raid no longer shuts it, and an errand room's garrison is priced together, which settles #469 (W15S25 scenario 3 now holds the room). `outmatched` is `Lose`. `chased` is a verdict other than `Lose` that the room's own row does not give; while a ranger holds the room's Guard (`kept`), any ranger answer, so a raid worn down to what a fresh guard would win is not handed off to a body still to be cast. Under a called `Win` a chase admits any lent ranger and takes as many as `rangersWin` counts; under `Kite`, the body that kites the raid down by itself. A chase on the record counts only while no raider is seen there, so `[ChaseDark; Raid; Hold]` is the raid's Guard and the guard takes it. **The light tick (rule 3):** the glance is `LightTick.glanceOf`, the full tick's questions; a healer forces no full tick. **Healing (rule 4):** priced in effect everywhere (`healReaching`, `exchangeWon`, `kitedDown`, `homeBeaten`). **The Fight's record (live t970,260, W17S25):** `residentsLose` prices the rangers standing in the room together beside its towers as well as one body of the garrison's size, and opens a record only where both lose; and a record whose room holds no raider, every one of its last-seen raiders lying dead (`FightLatch.Raiders`, a new `raiders` key on the `fought` wire), is dropped at once, the hold being for a raid that left. `HostileInfo` carries no boosts, so an invader's boosted TOUGH is priced as plain hits. **Left:** `guardsWanted` still sizes one or two bodies off one block, and reads the verdict only through the stand-down and the Brief's kind. Trade-off, named: the verdict prices the row's cast off the bank (`rangerBlocksReach`), where `outmatched` priced the largest ranger at any bank, so a small bank outmatches more resident rooms. Profile fingerprints: unchanged on seven runs (no scenario stands a healed, worn or kited raid, or a ranger beside one); `siege` and `siege --raided` move `colonies` (666354f9d618dccf, 889f05c4d40e039a) and the reset tick (bd863714e9540a55, a85c88e2e861847b) by the fight record's new `raiders` key alone (the same with the hold's drop switched off), assignments, creeps and positions unchanged. Arena: every case passes; the smallMelee outpost guard now disarms its invader rather than kills it (rule 1), and #469's pending case passes in place of its "today" pin | M, ~250 |
| S8 | `Formation`: one launch, strung and rally path for Fight and Assault (in the plan, Answered 4); unblocks #500. Done as two halves. **A (behaviour kept):** `FightGround` and `AssaultGround` are one record, `Formation` (`Slots`, `Launched`, `Rally`, `Target`, `Front`, `Behind`, `Ranged`, `Roles`, `Residents`; an Assault's kiter holds the front, a Fight has no target), and the `Formation` module is the path both squads take: `casts`, `holding`, `leader`, `launched` (rally, members, `ready`, and `onTheWalk`, which only the Assault passes: `linked` or out of home), `strung` (the Assault's only, as before; a Fight strings nothing yet), `rallyOn` (the Fight's) and `musterAtHome` (the Assault's), `rangeAcross` and the rest of the squad geometry. `Threats.Briefs` is keyed by **Task**, which settles S4's clash: the home's `Defend` and the Guard a raid in the home claims are two keys. The `Threats` fields `Fight`, `Assault`, `AtLine`, `DefendAvoid` and `Defend` are gone into Briefs: `Brief.defending` (the raid at the line as its `Targets`, fewest hits first, which `defendIntent` reads; the perimeter ground; `Avoid.Line` for the defender's walk; a `CastRow.Defender` ask at `DefenderBlocks`), and `Brief.formed` for a Fight (its room's Guard's rung and verdict) and an Assault (on the tier). `Brief.Kind` is now `Brief.Duty` (`Guard of BriefKind | Defend | Fight | Assault`): `BriefKind` stays the room Guard's seven flavours and `order` is over them alone, and `Claims` is a Guard's; `Brief.unclaimedFor` answers a combat Task no source claims (a Defend still admits the defender row and wants none). `Asks` is a list: a squad asks one `CastRow.Squad role` a role, its slots less its residents while it can be cast (a Fight not cooling with a rally ground; an Assault with a rally ground, outside safe mode and not `Taken`), so `squadQuota` and the defender row sum Brief asks as S6's rows do, and `Pool` (area, rung, capacity) and `Emitter.applicable` read every combat Task off its Brief (`Threats.briefFor`, `squadRoleOf`, `squadGroundIn`, `formationOf`, `wants`). The Planner pools the Fights, the Assaults and the Defend (while it wants a defender) off their Briefs. One reading moved with no fingerprint: a Fight's sapper or probe holder would read the front where it read nothing, unreachable since no catalogue squad casts either (its cap is 0). Every fingerprint and the arena suites unchanged; the test edits are renames (`squadGroundIn`, `squadRoleOf`, `formationOf`, `Formation.rallyHop`, `.Duty`), and the Planner's Fight test plans off its held Threats rather than none. **B (behaviour change, #518 follow-up 1):** a child's outpost could be the child's `Win` (her rangers and her mother's residents, `Lendable`, priced together) and the mother's `Lose` (hers alone), and nobody covered it. Settled in `rangersWin`: the child prices residents lent from next door apart from her own rangers, as the mother prices them, so the outpost is won by the child's bodies, which she sends, or by the residents alone, which the mother's own verdict calls; a raid only the two together win is `Lose`, and the child stands it down. Chosen over pricing the child's rangers in the mother's view, which would need the child's bodies in her `ColonyView` and would move the mother's verdict on every other room as well. And whether a body may be lent to a kill is one rule, its lender's, whichever colony asks: `Facts.mayLend view resident kill standing` over the lender's resident rooms, which `rangerMayKill` (the Chase's and the Trespass's admission), `mayBeCalled` (the Matcher's call) and `rangersWin` (the verdict and the chase's capacity) all read; a child pricing her mother's residents passes her own resident rooms with the home they stand in, so raids beside that home on two sides break its quiet in both views and the child shuts where the mother lends nobody (before, the child read the home quiet, called it a `Win`, and nobody covered either room). Residents standing in the room itself (the child's home) are still its garrison, priced with hers. Fingerprints unchanged on all nine runs (no scenario lends a resident) | M–L, ~400 |
| W1 | After S4: `guard:` splits into one Task id per flavour. A Memory wire change, with a `scripts/wire-check.mjs` case; the `assignments` fingerprints move (Answered 2) | S–M |
| B1 | The harassment ranger's shot follows its ground's digger-first order (`quirk: the ranger stands on the miner's ground and shoots the nearer hauler` changes on purpose). A behaviour change (Answered 3) | S |
| R1 | The melee body pattern `Guard` is renamed `Lancer`; "Guard" stays the Task's name until W1 names the flavours (Answered 6) | S |
| T1 | #531, a behaviour change (live t972,544–972,617: Crunchy047 read a fast 17A/17M beside a dying healer, so W17S25's residents kited off; Crunchy047 and Wraith403 each broke the home's quiet for the other's kill; a melee guard held W17S26 against Wraith403, which it could never close on). Fight math reads time, off one **reaction window**, the ticks our answering body needs, read at a tile a tick (every fighting row's plain pace): `Reaction.strike`, to close and strike, half a room's side (a garrison holds its room's middle and the raid it answers stands at its border or just past it); `Reaction.lend`, to walk to a kill and back, a room's side each way (the walk a kill's call already allows, `Brief.killWalk`). Two readings because one length cannot serve both: a lend's 100 ticks of a 2-HEAL healer restore 24 parts, the paper this rule exists to stop reading. Each reader names its window: every part-question (`effectiveParts`) and a room's exchange (`exchangeWon`) read `Reaction.strike`; a lend's quiet reads arms, pace, reach and its pricing all at `Reaction.lend`. **Strength (amends Answered 7 rule 1):** `HostileInfo.partsWithin window side hostile` is the live parts and those the live HEAL reaching it restores in the window, `Engine.liveParts` at its hits plus that, up to whole. A heal acts on one target a tick, so a side's budget is spent once: each live healer heals the body of its owner's within three, itself included, short of the most hits (`healTargetOf`), at the heal's power beside it and the ranged heal's beyond. `heals` and `healing` read the live HEAL; an exchange stands a raider at the hits it has and prices its healing once, as the rate (`effectiveHits` is gone, and `squadFight` starts each raider at its hits). A dying healer barely restores and a whole one brings back the paper. **Quiet:** `mayLend`'s home is quiet while nothing its Guard shoots stands in it and what it keeps beats every raider but the kill's own that arrives inside `Reaction.lend` (`arrivesWithin`: armed at that window, the world tiles to its middle less its weapon's range, at its road pace; an Invader and a body with no MOVE acting arrive nowhere). What it keeps (`homeHolds`) is its loaded towers at `Engine.towerHits` each and the bodies of ours standing there no lend takes (any but the ranger row's), priced against the arrivals together by the exchange (`exchangeAgainst`), once. **Strike and hold (the user, 2026-10-05):** `Facts.canStrike view ours target` is the kill's reading: a body with a weapon strikes any target that does not outrun it (`Gait.outruns`: strictly quicker on road, plain and swamp alike — equal pace or slower on any ground is worth the chase, a real opponent running into dead ends, swamp and exit bounces; the named stand-in for the movement epic's progress checks and give-up budgets), and one that does but is stationary, an unarmed worker on its dig seat, or an Invader penned in its room. A kill (Chase, Trespass) admits only a body that strikes **every** target it counts (`Facts.strikesEvery`: a chase's raiders, a trespass's trespassers), and `rangersWin` counts only such rangers, so admission and verdict agree. A room's Guard (Outpost, Resident, Harass, Core, ExitHold) admits its row whatever it can catch: where the row's guard cannot strike every raider, its ground is the hold-the-post ground, the tiles beside the room's Posts, which a raider must reach to hurt the work there, and the room is won by the guard's exchange as before (`rowWins`), so an uncatchable raider shuts an outpost only where no guard castable wins it. A whole guard or ranger walks a road tile a tick, the floor, so nothing outruns it there and the hold ground serves a worn or heavy chaser alone until the movement epic. No existing test expectation moved; arena suites unchanged; profile fingerprints unchanged on all nine runs (no scenario stands a worn or healed raid, a lend, or a raider out-running the guard) | M |
| T2 | #535, a behaviour change (live t973,670–973,783, W11S28: an NPC invasion of two 2T5M3R shooters, a healer behind each and a small mixed body, answered by two 30-part melee guards; the first went in alone and was shot rooted; t974,024–974,080, W18S25: one lent resident walked in alone to a raid priced on two, and a fleeing ranger beside Sky320 only healed itself; t965,417–966,157, W17S26: three 4,900-energy rangers died beside Synapse673 never shooting; 2026-10-06, W15S27: a 10-hit invader sat ~250 ticks beside an idle guard). Each rule is one function every reader calls. **The row is the damage a body brings to bear** (`Facts.borneDamage`): a body's shot in full, its swing only where it reaches every raider (`Facts.swingReaches`: one whose swing outdeals its shot, so it closes — the engine's smallMelee —, one rooted, one penned in its room (an Invader, as `canStrike` reads it), one slower on plain; one token ATTACK among a kiter's guns does not bring it in). `exchangeAgainst` takes our bodies; the raid is priced once (`raidPrice`) and `blocksBeat`, partly applied to its room, prices one block once for every count. A room's own row is `Facts.rowAnswer`, priced once a room a tick for the verdict and the Brief alike: a ranger or harassed room the ranger's; else the guard's where its borne damage wins; else the ranger's where the biggest ranger wins (an `Outpost` then asks one ranger at `rangerBlocksFor 1`, admits a ranger by the lend rule and no melee guard, and holds the kite ground); else still the guard's, which holds the post (#531) and wins while its exchange priced in full wins — no new stand-down for quick shooters no ranger wins either; never a win in a child's outpost. The guard row's sizing (`guardBlocksBeat`, `guardsWanted`, `guardBlocksFor`) keeps the full price. **One target choice** (`Emitter.pickStrike`): the first target of the kill order in reach, else the head; amended 2026-10-06 (#537): or `rangedMassAttack`, where it deals more than the best single strike at any target in reach, each target's damage capped by its hits left (ties keep the single strike, whose target the kill order names), and never with anything within three we may not strike — the engine hits every creep and structure not ours: a creep `Facts.mayStrike` refuses or an ally, a structure or power creep whose owner is not a declared enemy nor the Invader (`Colony.strikesAnywhere`; a power bank too, `Emitter.shields` off `RoomLayer.Foreign`; an enemy's structure never blocks one and its damage is not counted), or anything in a room the view does not carry (a transit room); the full tick strikes only one in reach; a fight's holder not mustering aims at its whole order (`Selector.Order` replaced `Head`), and the light tick picks again off the glance, its pick replacing the full tick's strike (ADR-0082 decision 2). **Keep reach** (the user): a Guard's melee holder striking a target beside it steps onto the target's tile (`Emitter.glueStep`), on the full tick in place of the mover (its tile stands, as a tired body's; #536's blocked foreign tiles never see it) and on the light tick in place of the step plan; never onto an exit tile, off our rampart, onto a tile a second melee swings at, nor for a squad's cast, whose Formation moves it. A ranger's kite ground is within two of the head where any tile is clear, else three. **Engage together** (`withMusters`): a raided Guard whose verdict is `Win`, none of whose holders wins or kites the raid down alone, and that the holders and the bodies its rows still cast win together, holds them on the rally ground (`Brief.Stances`, read first by `Pool.areaFor` and the travel cost) until they launch (`Formation.launched`, its walk the holders standing as one file, `Formation.linked`, as #491's squads: strung out between the rally ground and the crossing they are not pulled back). A mustering ranger still fires its reflex; a mustering melee body swings at nothing. Never once a holder stands in the room or at its crossing: the launch latches there without state, and a holder in reach is never pulled back. Never where even the bodies that can be assembled lose, so nobody waits for ever (a timed latch would need a clock this stage does not carry). **A fight begun is fought to its target's death** (the user, 2026-10-06): one hold rule, `Threats.holdsFight`, read by the Planner (it pools a held Guard) and the Matcher (it keeps the holder over every gate): a holder of a room's Guard or Fight stays while a target its Brief shoots stands in the room that it can strike (`canStrike`), whatever the room's kind, capacity or muster; it ends on the target's death, its leaving or outrunning, a `Lose` verdict, a resident's lender's recall, a target under a rampart not ours (`Facts.underForeignRampart`: `canStrike` is false while it stands, stateless; the general no-progress give-up is the movement epic's), or #533's flee — out-damaged and losing its own exchange where it stands (a guard shot rooted beside shooters). `Threats.Engaged` (the rooms whose Guard a body holds) joins `fightRooms`: a healer there is a target, and so is a cripple. `Facts.trespassingBody`'s disarmed clause is any owner's; `Facts.trespasser` reads the rooms we mine or whose fight we hold. **One diplomacy rule** (`Facts.mayStrike`, the user 2026-10-06): never a Source Keeper nor an ally; a declared enemy's or an Invader's creep anywhere; any other armed on paper only in a room we mine or hold; a neutral that struck us first is the named, deferred `struckUsFirst`. Every target reader reads it — the weapon reflex, the trespass, and `guardShoots`' armed and healer clauses (in a harassment room a keeper or a third party's is no target) —; whether a hostile breaks a home's quiet is the separate threat predicate `Facts.disturbs`, where an armed neutral still counts. **The weapon reflex** (`Emitter.strikeReflex`, before `healReflex`): every body with a weapon that holds no fight of its own strikes the target `pickStrike` picks among what `mayStrike` allows in its room, on a free channel, never moving for it. The W17S26 rangers' cause: shot rooted (no MOVE), a ranger fails `canStrike` against a player body that can step away, so the chase did not admit it; unassigned, it struck nothing, the heal reflex alone acting. Tests: the W11S28 shape asks the ranger row; in reach first and the step onto the target on full and light ticks; the step's four refusals; holders strung out on the walk in are not pulled back; a stronghold Invader under its own rampart holds no guard, off it one; two guards and two lent residents wait apart and go in together, one in the room is never pulled; a guard whose invader is disarmed keeps it and kills it, a fresh body begins none where we mine nothing; Virtual458's shape stays the ranger's; an outrunning target is let go; a lost fight is fled; the diplomacy table; arena captures of t973,670 (the invaders, penned, are in a melee's reach: the two guards, stepping onto what they swing at, run the whole invasion down, neither dying, nothing cast; a player's quick shooters are the ranger row's, the Decide test) and t965,400 (the rooted ranger shoots every tick a gun acts; on main it never shoots), and the melee step after a fleeing raider. Arena pins moved on purpose: S7's smallMelee guard kills the invader it disarms; #450's guard already holds the Guard and carries seven HEAL (the kiter, as before, without ATTACK, is the ranger row's; a guard it out-damages flees); RCL3's east neck holds without residents, the second defender swinging by the reflex. Profile fingerprints in the report. | M |

**S0 baseline** (main d0efa4f6, 2026-10-05). `npm run profile -- --scenario <s>` Memory fingerprints:

| Scenario | assignments | creeps | colonies | positions | reactor | after the reset tick |
|---|---|---|---|---|---|---|
| stub | e4c60c3d3cb7114d | 35f00ffe8c7f906f | a3d423611efef558 | ff8230b244e5ecdf | 1f090c8adb0147df | 8a99144b029db026 |
| outpost | 21f66544a80d5c58 | a18c2743ee0f57ca | 4c3c9a282e6408f4 | 6f1e3aa60259fff5 | 1f090c8adb0147df | 1c6b2942b54e68d6 |
| outpost --raided | 269ab9d0e41974f0 | c75391b59310ab78 | 4958d78f16594ca8 | 92e68517450b8401 | 1f090c8adb0147df | 499b986e8d0ea283 |
| young | 0506fdbf9471580d | a7b5fae116bb8048 | 4b7af12bcb3503db | 7cf6ab4219aa33d4 | 1f090c8adb0147df | 26b17c95fd6a3ee6 |
| pair | 209b1abf9dc3bc1d | 586b6e74f3e64b82 | fc81bdc552ff33d4 | e652ba7beed66555 | 1f090c8adb0147df | 6b4c6ba7f751eb90 |
| pair --raided | 4531438fbd527bc1 | 013616cf989f4fd2 | f3aef928624e0dc2 | ad0e1f95a390fa7d | 1f090c8adb0147df | af286497879246a2 |
| reactor | 015d2398251355cb | 8d3bb61797210080 | aab08b854a0feb8e | 168e6a57b4070c6c | 6a7d97dc543b1cfd | c72d13fc3f81ba78 |
| siege | 307c21bd810677b5 | ae7a9eff3252d9c1 | 3721aef302362928 | a5deaa6e93f457a1 | 1f090c8adb0147df | 8326d43593f8f4f1 |
| siege --raided | c6aa0a873b6c7647 | 97c157120c87e278 | bbbdf98060c8598f | 9aebc0614deba6bc | 1f090c8adb0147df | 8a8b027f8f147d8e |

The arena suites: 101 cases, 100 pass and 1 is skipped ("scenario 3 … as it should be").

**S7's input: where callers read a hostile on paper and where live** (found by S1, each kept as it was; S7 read every one in effect, rule 1):
1. "Is a raid here?": `armedOnPaper`/`raiderOnPaper` in `threatsHeldBy` (Reach, shelter, harassment split), `guardedOutpostsOf`, `defendedHomesOf`, `residentQuiet`, `underRaid`, the `Emitter` chase arm, `fightConfirmed`, `fightSquadIn`, `fightReports`, `killRank`, `approachAt`, `foldFights`, Observe's threatened latch, `safeModeIn`, `recallArmed`, `homeBeaten` and the exit watch; `hurtsLive` in `raidDeadlines` and `strikeGone`; `armedLive` in `raidAtLine` and `trespassingBody`.
2. The kite, defend and assault avoid sets and the chase's melee list read `meleeLive`; the Reach beside them reads the paper weapon range.
3. The light tick's `Armed` is `armedOrHealsOf` on paper, a HEAL included, where the full tick's `armedOnPaper` counts none: a lone healer in a home forces a full tick that then calls it unarmed.
4. The light tick's near rule reads the live weapon range and live WORK/CLAIM (`Breaks`); the full tick's Reach reads the paper range.
5. "Does it strike our structure?": `rampartsUnderAttack` and `planFire`'s `hurting` read the paper weapon range and paper WORK; `raidAtLine` reads live ATTACK/RANGED_ATTACK and live WORK.
6. Healing: `healReaching`, `exchangeWon` and `homeBeaten` price `healingOnPaper` (their healer filters `healsOnPaper`); `kitedDown` prices `healingLive`; `raidHealer` and `guardShoots`' fight-room healer ask `healsLive`.
7. `raidHealer` mixes them: unarmed on paper, healing live, beside a body armed on paper.
8. Trespass: WORK, CARRY or CLAIM on paper and live, no live weapon; its store ground's `hauls` reads live WORK/CLAIM, while the harassment ground's `digs` reads paper WORK.
9. The harassment ground's `catchable` builds its own gait off paper MOVE and paper weight, apart from `HostileInfo.gait` (live MOVE).
10. CLAIM is asked on paper (`claimsAFlag`, the safe-mode claimer) and live elsewhere (`breaksLive`, `trespassingBody`).
11. Pricing: `exchangeWon` counts the raid's damage and hits off paper parts; `kitedDown` and `cannotClose` read live parts.

For S2/S3: `catchable`'s own gait rule (item 9) belongs with the harassment ground's target order, which those stages make one source.

**Deleted at the end:**
- the 12 per-behaviour `Threats` fields and their accessors (`isKill`, `isTrespassed`, `guardGroundIn`, `residentRingIn`, `harassRingIn`, `defendGroundIn`, `defendersWanted` as public reads)
- the 13 dispatch arms
- the 4 exclusion lists
- `guardedSplit`
- the Matcher's kind clause
- the Resolver's `kiters` and trespass branches

That is roughly 500–700 lines net.

## Considered options

- **Leave it, add the next Kind as before.** Each Kind costs about 25 Decide functions and an arm in 13 places. #517 and #511 are next, and both reach into the same chains.
- **One Task kind per flavour** (`kill:`, `harass:`, `resident:`). This removes the dispatch at the id, but it changes the Memory wire and the assignment fingerprints, and it does not unify the grounds, the verdicts or the row asks. Rejected as the first step and taken after it: stage W1 splits the id once the Brief carries the flavour (Answered 2).
- **Fold Flee and the towers into the model.** Rejected. A refuge has no target and a tower has no body or ground. Forcing them in leaves half the record empty for them.
- **Name it Engagement.** That is the natural word, but CONTEXT lists it under Fight's *Avoid*. Taking it means moving that avoid, and Fight then becomes one engagement among several. "Brief" collides with nothing.

## Consequences

- A new combat behaviour is one brief source plus its tests. It is not a field, plus an exclusion clause in each sibling, plus 13 arms.
- The precedence between flavours in one room is a list order you can read and test, not an emergent property.
- A Brief is derived each tick and never stored: the wire, the light-tick replay (ADR 0082) and the memo signatures do not see it.
- Unifying the inconsistencies this audit found changes behaviour, so each is its own issue after S4:
  - "armed" by parts versus by live parts (settled by S7: in effect, Answered 7)
  - the harassment ground's target order versus the shot's (stage B1)
  - the exit-hold ground in a chased room
  - a room both resident and harassed: capped by `rangersWanted`'s harassment count under the resident's arm
- The cost is one more indirection when reading a Guard's ground: the Brief, not a named `Threats` field.

## Answered (user, 2026-10-05)

1. The name is **Brief**.
2. Yes: after S4, `guard:` splits into one Task id per flavour (a Memory wire change, its own stage with a wire-check case).
3. Yes: the harassment ranger's shot follows its ground's digger-first order (a deliberate behaviour change, its own stage).
4. Yes: Fight joins as a Formation (S8 is in the plan, not optional).
5. Yes: S2–S4 land before #511. #517 was already built and under review when this was answered; it ships as is and is folded into the Brief's sources at S4.
6. The melee body pattern is renamed **Lancer** (a pure noun for a melee body); "Guard" stays the Task's name until stage 2's split names the flavours.
7. S7's rules. (1) ~~A hostile's parts are read live when no healing reaches it, and on paper (every part, full hits) when a healer of its side stands within three of it or it heals itself: healed parts come back.~~ Amended by T1 (#531, user 2026-10-05): a hostile's parts are its live parts and those the live HEAL reaching it restores inside the reaction window, `Reaction.strike` ticks. (2) One verdict a room, `Win`, `Kite` or `Lose`, off the bodies that could answer it, which the stand-down (shut on `Lose` only), the rows, the chase and the call all read; `Kite` stays #510's. (3) The light tick's glance counts no HEAL-only body armed. (4) Healing is priced by rule 1 everywhere.

## Appendix A: special-case sites (Decide functions, hit count)

- **Outpost Guard / core / hold:**
  - `Emitter`: applicable(2), guardIntent
  - `Facts`: expansionCores
  - `Planner`: coredOutpostsOf(2), guardedOutpostsOf, outpostFactsOf(2), planTasks(5)
  - `Pool`: planPool(2)
  - `Quota`: guardBlocksFor(2), guardBlocksWanted(2), guardedSplit, guardQuota(2), guardStands(2), guardsWanted, isGuardRowBody, isGuardRowCut, quotaRowsOf, reserverClaimsOf, rowSizingOf
  - `Resolver`: movementOf
  - `Spawns`: planSpawns
  - `Threat`: guardGroundIn, threatsHeldBy(2), withFights
- **Resident:**
  - `Emitter`: guardTarget(2)
  - `Facts`: claimsAFlag(2), fightConfirmed, fights, fightSquadIn(2), guardShoots(3), killRank, mayBeCalled(2), outmatched, raisedHomes(2), rangerMayKill(2), rangerRooms, residentQuiet, residentRooms(2), residentsLose, squadFight, underRaid(2)
  - `Planner`: defendedHomesOf(2), isBootstrapRoom, outpostFactsOf(2)
  - `Pool`: planPool(3)
  - `Quota`: fightReports(2), rangerQuota, rangersWanted(7)
  - `Threat`: guardGroundIn, threatsHeldBy(7), residentRingIn(2), withFights(2)
  - `Observe`: foldFights(2), raidDeadlines
- **Harass:**
  - `Entry`: signaturesOf
  - `Facts`: guardShoots, harassRooms, harassTarget, rangerRooms
  - `Planner`: outpostFactsOf
  - `Pool`: planPool
  - `Quota`: rangerBlocksFor(3), rangersWanted
  - `Threat`: guardGroundIn, harassRingIn(2), threatsHeldBy(2)
  - `Observe`: approachAt, foldRaids, harassOverrides, raidDeadlines(3), rivalDeadlines, sight, standDown
  - `World`: harassCasters
  - `Views`: Dismantles
- **Defend:**
  - `Bodies`: 8 functions
  - `Emitter`: applicable(3), defendIntent
  - `Facts`: raidAtLine
  - `Layout`: planFire
  - `Planner`: planTasks
  - `Pool`: areaFor, planPool
  - `Quota`: quotaRowsOf
  - `Resolver`: moveIntentFor, movementOf
  - `Spawns`: castBodyOf, heldForDefender, planSpawns(5)
  - `Threat`: defendGroundOf(2), defendAvoidIn, defendersWanted(3), defendGroundIn, threatsHeldBy(6)
- **Chase:**
  - `Emitter`: applicable(2)
  - `Facts`: chased, kitedByRanger(2), kitedDown, outmatched
  - `Matcher`: matchCreeps(2)
  - `Planner`: planTasks
  - `Pool`: areaFor, planPool
  - `Quota`: guardedSplit
  - `Resolver`: movementOf
  - `Threat`: threatsHeldBy(7), isKill
  - `Observe`: OutpostEpisode, raidDeadlines, sight, standDown(2), strikeGone(3)
  - `Sightings`: scanRecalling
  - `Views`: ofWorldRecalling
  - `ObserveMemory`: encode and decode
- **Trespass:**
  - `Emitter`: applicable(4)
  - `Facts`: guardShoots, mayBeCalled, rangerMayKill, trespasser(2), trespassingBody, underRaid
  - `Matcher`: matchCreeps(3)
  - `Planner`: planTasks
  - `Pool`: planPool(2)
  - `Resolver`: movementOf
  - `Threat`: guardGroundIn, threatsHeldBy(4), isKill(2), isTrespassed(2)
- **Fight:**
  - `Emitter`: actionIntents(2), applicable
  - `Facts`: fightConfirmed, fightCooling, fightHeld, fights, fightSquadIn(2), heldTaskFacts, latchedSquad, pricedSquad, sinceFightSeen
  - `Planner`: planTasks
  - `Pool`: areaFor
  - `Quota`: rangersWanted, squadQuota(2), squadStands, quotaRowsOf
  - `Spawns`: planSpawns
  - `Threat`: fightGroundIn(2), fightLaunchedIn, fightRoleOf, fightRooms, threatsOfHeld, withFights(4)
  - `Layout`: planRampartDoors
  - `Observe`: foldFights(2), foldRaids, standDown
- **Assault:**
  - `Emitter`: actionIntents, applicable
  - `Pool`: areaFor
  - `Quota`: squadQuota
  - `Threat`: assaultPooled, assaultGroundIn(2), assaultRoleOf, threatsOfHeld, withAssaults(3)
  - `Atlas`: ofViewRecalling(2)
  - `Observe`: approachAt, foldProbes, foldTaken
