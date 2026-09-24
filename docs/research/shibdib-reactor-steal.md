# Shibdib's reactor steal: how SlothBot picks, holds and retaliates

Date: 2026-09-25, live tick ~727,800–728,000 on `shardSeason`. Source read:
[`shibdib/SlothBot`](https://github.com/shibdib/SlothBot) at `c9f5e95` (2026-09-24, *"Add thorium
scarcity detection and reactor stealing mode"*), files `default/modules/module.season.js`,
`default/roles/role.reactorClaimer.js`, `default/modules/spawning/spawnGlobal.js`,
`default/modules/module.diplomacy.js`, `default/configs/config.shardSeason.js`. The rules of the
Reactor itself are still `thorium-reactor.md`'s.

## Summary

- **W15S25's Reactor is Shibdib's, and nobody is typing orders for it.** The take is SlothBot's
  automatic *steal mode*, pushed the day before. The Reactor held 726 T of our ore, burning at
  1/tick for their score. It was guarded by a `10M/8RA/2H` longbow, with `2M/1C` claimers
  behind it.
- **Steal mode is permanent for them this season.** It switches on when their empire's Thorium is 0.
  The count covers storage, creeps, and deposits in RCL≥6 rooms. Deposits never regenerate.
- **They are drawn by an *active* enemy Reactor and by nothing else of ours.** A Reactor with
  `store > 0 || continuousWork > 0` outscores everything else by ~6,500 points, from anywhere
  within 18 rooms. An idle Reactor of ours scores +400, which is below an unowned one. So an
  unbroken run, the season plan's differentiator, is the exact shape their scorer hunts for.
- **Shooting their creeps puts us on their war list.** One damage event is enough, within a
  5,000-tick window. Harassment, remote denial and siege ops are enabled in their season config.
- **What we already had, and what was missing.**
  - `reactorRefills` refuses a Reactor whose flag is a rival's.
  - An armed non-Keeper hostile in the errand room opens a stand-down, clocked to its own life
    (`Observe.raidDeadlines`). It fired live at t727,666, when the longbow arrived, and shut
    W15S25 until t728,847.
  - The gap was the **flag war**: a load poured while our re-claimer holds the flag, beside
    their parked claimer, burns for them the tick after. #406 closes it: a rival CLAIM body in
    the errand room shuts the draw and the pour.
  - A hand withdrawal of the errand (2026-09-25) was reverted once this was understood.

## 1. Who

| | Xerxes_2 | Shibdib |
|---|---|---|
| season rank / score | 10 / 335,425 | 29 / 12,078 |
| GCL points | GCL 5 | 10,653,990 (GCL ~3) |
| rooms | 5, W15S28 at RCL7 (5,300-energy bodies) | W18S23, W21S23, W23S22, all RCL6 (2,300 cap) |
| to W15S25 | W15S28, 3 crossings | W18S23, ~5 crossings |

The raid log shows the contest is not new. The first sighting was a claimer alone at t724,971,
before the steal-mode commit. The longbow joined from t726,458, and the Reactor was theirs by
t727,842.

## 2. Target selection (`pickTargetReactor`)

Candidates are every Reactor in their `Memory.season.reactors` (TTL 1,500 ticks), plus every
sector centre within 2 sectors of each of their rooms. A candidate more than 18 rooms from their
nearest room is dropped, and friendlies are skipped. Every candidate starts from the same base
score, then gets a mode bonus:

```
score = 2000 − 50·dist + 20·northValue          // W15S25: northValue = −25 → −500
```

| Reactor is… | steal mode (their T = 0) | normal mode |
|---|---|---|
| another player's, **active** | **+9000 + min(work,50000)/10 + min(store,2000)** | +1200 |
| unowned | +2500 | +2500 |
| theirs | +1500 | +8000 + work/20 (+4000 under 100 T) |
| another player's, idle | +400 | +800 |

The argmax becomes the one target. `setOperations` deletes every other non-manual `reactor`
auxiliary op, so they chase one Reactor at a time.

## 3. What they send (`setOperations`, `spawnGlobal.js` case `reactor`)

- `claim: !mine`: one `reactorClaimer` while the target is not theirs. It walks in, calls
  `claimReactor` every tick it is not theirs, and parks at range 2 once it is.
- `haulers: mine && tAvail > 0 ? … : 0`: **no haulers in steal mode.** They never feed a stolen
  Reactor; they let the victim's store burn for them. `thoriumHauler`s recycle on scarcity.
- `guards: hostile ? 2 : 1`. `hostile` counts armed intel in the last 1,500 ticks, `threatLevel`,
  **or `steal`**. So the approach is a `longbowSquad` of two (`waitFor: 2`). Once the Reactor is
  theirs and no armed hostile is on record, the op drops to one `longbow`.
- The longbow is a `buildLongbowFamily` body under their 2,300 cap: `10M/8RA/2H`, ~80 ranged
  damage and 24 heal a tick.

## 4. What they see

Every Reactor judgement reads `scanVisibleRooms`, which only sees rooms they have vision of. The
record expires 1,500 ticks after their last look. Once they own W15S25 and the one longbow is the
only thing standing there, their view of the room is exactly that creep's. **Inference, not
tested:** a retake while no creep of theirs stands in W15S25 would stay invisible to their scorer
until a scout or a new op passes.

## 5. Retaliation (`module.diplomacy.js`, v2.9)

- `trackThreat`: a creep of theirs that loses hits, with a ranged body of ours within 3 (or
  melee adjacent), costs our standing **−3.5** at most once per 20 ticks (×3 inside their own
  rooms).
  - Standing ≤ **−25** makes us a **THREAT**: their defenders engage on sight.
  - Standing ≤ **−300** makes us an **ENEMY**: sieges.
  - Threat standing does not recover until 20,000 ticks without combat.
- **`evaluateWarSignals`: any aggression within 5,000 ticks makes us a "worthy" war target**, and
  the top three worthy targets become `WAR_TARGETS`.
  - `config.shardSeason.js` has `OFFENSIVE_OPERATIONS = true` and `HARASSMENT_OPERATIONS = true`
    (cheap longbows raiding a threat's remotes).
  - It also has `HOLD_SECTOR = true`, and `OFFENSIVE_NUKES = true`, which is moot at RCL6.
  - Siege picks are ringed by distance to their empire, and weaker targets are preferred.
- Trespass in their owned rooms costs −0.75 per 50 ticks. We never enter them.

**What this means for us:** one engagement in W15S25 buys harassment of W15S28's remotes (W15S27,
W15S29, W14S28), which are ~5 crossings from W18S23. It lasts for as long as combat keeps
recurring, plus 5,000 ticks after.

## 6. The fight, priced

| | their squad (2 longbows) | one of ours, e.g. `18RA/5H/23M` (5,100 e) |
|---|---|---|
| hits | ~2,000 each | 4,600 |
| damage / tick | 160 | 180 |
| heal / tick | 48 | 60 |

We win one-on-one against the squad, killing the first longbow in ~15 ticks. Their single
holding longbow dies faster still. The engine fight is not the problem; the war list in §5 is.

## 7. Options

1. **Hold by force:** a resident guard in the errand room beside the re-claimer. We win every
   local fight. The price is a standing war with a bot that harasses remotes, for the rest of the
   season.
2. **Another sector's Reactor:** no escape. Any active Reactor within 18 rooms of W18S23 draws
   the same scorer.
3. **Wait:**
   - The ore does not decay in Storage, and there is ~22× more season than ore
     (`thorium-season-plan.md`).
   - Once the 726 T is burnt, W15S25 is theirs and idle, worth +1500 to them, below any unowned
     Reactor within range (+2500). The scorer may well move them off it.
   - Restarting deliveries makes it active again, and brings them back.
4. **In every case (done):** an armed rival shuts the errand room (the existing stand-down);
   a rival claimer shuts delivery (#406). With both, the errand stays declared. The cost left is
   one re-claimer probe each time the stand-down's clock runs out while their longbow is still
   there: 650 energy per ~1,500 ticks.

Open question for a human: whether the season's remaining score is worth a war with SlothBot.
Everything else follows from that.
