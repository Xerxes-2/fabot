# Spar: the combat bench

A pinned arena test holds one capture to one outcome. `npm run spar` (#567) asks a wider question: did a fight change make us stronger or weaker across the standard raids? It runs a fixed set of fights in the arena (`tests/Core.Tests/Spar.fs`), reads each into outcome metrics, and diffs them against the committed `tests/spar-baseline.json`, the way `npm run profile` reports fingerprints.

```sh
npm run spar               # table + diff; exit 1 on a worse metric or a new/gone scenario
npm run spar -- --accept   # write this run as the baseline
npm run spar -- xx [slug ...] [--seed N] [--tolerance T]   # the xxscreeps cross-check
```

The bench takes about 10 s after the build. It is not part of `npm test`. There each scenario only stands up and runs one tick, so a scenario that no longer builds still fails the suite.

## When to run it

Run it on any change to Facts, Threat, Emitter, Resolver or Arrival, and on any change to the arena's physics or the `Invader` script. Treat a worse metric like a moved profile fingerprint: name it in the commit with the reason, then `--accept`. A better metric exits 0; accept it in the same change so the next regression is measured from it.

## The scenarios

Every scenario is hermetic. Its colonies are spelt out in `Spar.fs` and never read off `Colony.declared`, so a roster change moves no metric.

| slug | the fight |
|---|---|
| `outpost-<raid>` | Invaders in W12S27's south-west corner, where our anchor digs and a hauler waits. One guard block stands at home in W12S28 (RCL7); the guard and ranger rows cast. 500 ticks. |
| `home-<raid>` | Invaders inside W12S28's north exit, under its three towers, with the home crew at work. The guard, ranger and defender rows cast. 300 ticks. |
| `home-towerless-melee` | The `home-melee` raid with W12S28's towers gone and no safe mode left (#568): the raid no tower and no line answers, met only by the home's own guard row. 300 ticks. |
| `home-exit-bounce` | The #552 shape: Trepidimous' 18M17A melee and 11M7H healer bounce on W17S25's west exit, against the RCL4 home's tower, its perimeter ramparts and defender, and the defender and ranger rows. 300 ticks. |
| `stronghold-l2-rangers` | Three 21M 14R 7H rangers one tile inside W16S26's south exit, under the L2 bunker (#565, #566). Cleared means the core fell. 150 ticks. |
| `stronghold-w16s26` | The live L2 bunker (t1,002,250: towers at 660 and 550, nine 200k ramparts, the defender) against the cheapest squad that clears it whichever body the towers focus: three 25R 25M rangers on the range-3 tiles east of the core and seven 25H 25M healers behind them, scripted from the first tick (no walk-in, no colony). The core falls at t399. 400 ticks. |
| `home-boosted-pass` | A T3-boosted 10T 15R 10M 15H neutral player walks through W12S28, north exit to west exit (ADR 0089 S0). 150 ticks. |
| `outpost-equal-pace-melee` | The #625 shape: Trepidimous' Wolf0965 (18M 17A, MOVE first, no heal), a tile a tick on plain as our rangers are, walks down W17S26's north road at the anchor by the container and chases the nearest of ours. Our two 6M 4R 2H rangers stand south of the container; nothing casts. 150 ticks. |
| `outpost-fleeing-hauler` | Two rangers vs a fleeing hauler (#619): Trepidimous' empty 23C23M hauler, shot to 2,480, walks past W17S26's container and out along the road to the west exit on Wolf6836's live tiles. Our two 6M 4R 2H rangers stand beside it at the start; nothing casts. 34 ticks, ending as it stands a step off the exit, so a later kill does not count. |

`<raid>` grows in size: `melee` (smallMelee), `melee-healer` (smallMelee + smallHealer), `ranged-healer` (2 smallRanged + smallHealer), `squad` (the W11S28 five: a mixed body, two shooters, two healers).

## The metrics

Each scenario runs to its horizon, whenever the fight ends.

- **cleared**: the tick the fight was first won, with no hostile body left in the arena, or the core gone for the stronghold. `-` means never. Later is worse.
- **deaths**: our bodies that died. More is worse.
- **kills**: their bodies that died, an Invader healer's suicide included. Fewer is worse.
- **cast**: the energy of every body our spawns began. More is worse.
- **hurt**: hits our bodies lost, net of each tick's heal. More is worse.
- **struck**: hits our structures lost, not counting rampart decay. More is worse.
- **tower**: energy our towers spent, net of refills. More is worse, which is why a tower that never fires reads `+` here while every other column falls.
- **down**: whether any colony's outpost was ever stood down.
- **safe**: whether safe mode was raised in a room of ours.

Columns trade off against each other. More cast energy may buy a faster clear. Read the row as a whole and justify the trade in the commit.

## Adding a scenario

Add a `Scenario` to the list in `Spar.fs`. Its slug must be lowercase letters, digits and dashes, because `npm run xx` takes it as `spar-<slug>`. Run `npm run spar`: the new slug fails as "new, no baseline" until it is accepted.

## The xxscreeps cross-check

`npm run spar -- xx` stands scenarios on xxscreeps (`npm run xx -- run spar-<slug> --bot replay`, see `arena-capture.md`). Each death tick is compared with the arena's. The command exits 1 when a body dies in one run and not the other, or the two death ticks are more than `--tolerance` ticks apart (default 30). Run it periodically, and whenever the arena's physics or the `Invader` script changes. It is too slow and too far from shardSeason to be a gate.

The replay bot replays the bodies a scenario starts with, their scripts' attacks and heals as well as the pipeline's, and our towers' shots and heals. A body our spawns cast in the arena does not exist in xxscreeps. A player's body has no code there and stands still. So the cross-check covers only scenarios that cast nothing and fight only Invaders. The defaults are `outpost-melee`, `home-melee`, `home-squad` and `stronghold-w16s26`. An invader core counts like a death: the tick it falls is compared too. Since #592, `home-melee` and `home-squad` agree death for death at seed 1. `outpost-melee` does not: its invader leaves the xxscreeps tiles at t5, after which the replayed guard's swings miss, so the arena kills it at t16 and xxscreeps never does. The outpost raids that cast, the bounce and the boosted pass (the dump carries no boosts) are not cross-checked. Neither is `stronghold-l2-rangers`, whose rangers our pipeline drives.

A scenario with an invader core stands on xxscreeps' modern stronghold mod (a port of `stronghold.js`, `defence.js` and `creeps.js`, bunker1–5), on a template of its own. The core is placed already deployed, with the scenario's ramparts, towers, containers and roads, and no deposit type, so it drops no loot. `stronghold-w16s26` agrees tick for tick at seed 1: every rampart's and the core's hits, the core falling at t399, no body hurt. What it checks: `focusClosest`'s target and tie (the healers heal only the body the arena's towers focus, so a different focus would kill that body), tower damage against the healing, the defender held on its ramparts, ranged attacks landing on the rampart over the core. What it does not check: the refill (the towers run dry from about t55 and are refilled, the arena on its fixed period and xxscreeps on its seeded dice, but the healing outmatches them either way, so neither shows in any hits), the defender's respawn, the collapse, the core's loot. xxscreeps parts from vanilla once the core falls: it hands the defenders to the raid AI, where vanilla leaves a creep with a `strongholdId` idle. The scenario ends on the tick the core falls for that reason.
