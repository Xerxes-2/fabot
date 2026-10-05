# Capturing a live tick as an arena test

A raid seen live becomes an arena regression test (#532). The script reads the
season's room-history replay. It writes no game state.

```sh
npm run capture -- <room> <tick> [--rooms R1,R2] [--name slug] [--out path] [--live-memory | --no-live-memory]
```

- `<room>` is where the incident happened. `--rooms` adds the rooms beside it that the fight crosses into.
- `--rooms` must also add the home room of every spawn that cast one of our bodies in the fight. A mother's residents stay the mother's. Without her room, the child colony adopts them and decides differently from live. The header names any such spawn.
- The replay is written 100-200 ticks behind the live tick, so a tick newer than that has no file yet. Find the tick with `npm run observe -- history <room> <tick>`.
- Every room needs a committed capture under `tests/Core.Tests/rooms/` for its terrain. The skeleton names any room without one; add it with `npm run capture-room -- <room>`.
- The output is printed, or written to `--out`. Nothing is wired into the suite.

## What the skeleton holds

The skeleton has a header and a fixture `let private <slug> () : Arena`. The header names the command, the tick, and each room's replay URL. The fixture holds:

- **Controllers:** owner, level, safe modes, cooldown and clocks.
- **Structures, sites and the Reactor:** placed by the arena builders, with hits, stores and rampart decay. Roads, drops, tombstones and ruins are left out, and the header counts them.
- **Bodies:** both sides, built with `body` plus their hits, life, fatigue and stores. A comment beside each body lists its live parts and its live acts.
- **Banks:** each home's bank is its energy capacity, not what it held at the tick. The arena never debits the bank and reads it as the capacity too. A near-empty bank prices the guard row's cast as a loss, and that stands an outpost down that live held.
- **Live Memory** (`--live-memory`, on by default when the tick is at most 300 ticks behind live): the captured homes' Raid log (stand-downs open at the tick, threat and fight latches, home raided), `ArmedSeen` and `Towered`. Memory holds only what it holds at the time of reading. An entry stamped after the tick is named in a comment and not seated.
- **Assignments:** each creep's Task at the tick, taken from the Transition log in Memory. Engine ids are re-pointed to arena ids (`tower-13-29`, `spawn-Spawn11`). The log is a ring: a creep with no verdict that far back, or one that has since died, gets a `TODO`.

The skeleton ends with a pending `ptest` whose body is `// EXPECT:` and a `failtest`. It compiles as printed.

The arena starts at `tick + 1`. The replay holds the state after the tick ran, so arena tick 0 decides what the live code decided next.

## What it leaves to you

- **Scripts:** every body not ours `Hold`s. Give each one a `Behaviour`, using the live acts written beside it.
- **Memory the replay cannot give:** `Healers`, `ExitWatches` and `Sightings` live only in the heap and always start empty. For an old tick, the Raid log and `ArmedSeen` start empty too.
- **Check it reproduces live** before pinning anything: compare the run with `observe history` and `observe timeline` over the ticks after the capture. A capture that decides differently from live is a false regression test. For the t972,617 test, the first capture missed the mother W15S28 and seated a 14-energy bank. It lent the residents at once and stood W17S26 down, while live they held home.
- **Not modelled by the arena:** reservations, boosts and roads.
- **Colonies:** they come from today's `Colony.declared`. Spell the record out if the test must outlive a roster change.

## Landing it

1. Paste the fixture at module level in the arena file that owns the domain: `ArenaTests.fs` (scenarios, borders, defence), `ArenaClaimTests.fs` or `ArenaReactorTests.fs`.
2. Move the test into that file's testList.
3. Name both after what they pin.
4. Write the expectation, and turn `ptest` into `test`.
5. If the test pins today's behaviour that an open issue will change, say so in a comment.
6. Run `npm run format`.

Example: `#532 capture t972,617` in `ArenaTests.fs`, "arena border play".

## Cross-validating on xxscreeps

The arena is our reading of the engine. `npm run xx` stands the same scenario on [xxscreeps](https://github.com/laverdet/xxscreeps) (#543), which runs vanilla engine rules and a line-by-line port of the vanilla invader AI. It then diffs the two traces tick by tick. The arena stays the regression gate; xxscreeps is a second opinion.

```sh
npm run xx -- run <slug> [--bot standin|replay|dist|<dir>] [--seed N] [--heap [tick]] [--ticks N]
npm run xx -- run build/xxscreeps/<slug>/<slug>.scenario.json --bot replay   # reuse a dump
npm run xx -- diff <arena.jsonl> <xxscreeps.jsonl>
```

- **Wiring a test in:** after its run, call `crossValidate "<slug>" start trace`, where `start` is the `Arena` it ran. Outside `npm run xx` it writes nothing. `run <slug>` runs the arena tests (`--filter`, default `arena`) with the dump on, then runs xxscreeps and writes the diff to `build/xxscreeps/<slug>/`. Example: `w11s28-invasion` (#535).
- **Setup:** the first run installs xxscreeps and builds isolated-vm into `~/.cache/fabot-xxscreeps`, then imports a world. It uses Node 24 from `nix shell nixpkgs#nodejs_24` and never touches the system Node or the repo's `node_modules`.
- **Who drives our bodies (`--bot`):**
  - `standin` (default) is the #535 melee rule: chase, swing, step on.
  - `replay` walks our bodies along the arena's path and casts the arena pipeline's attacks and heals. Only the other side's AI can part the traces, so the "first apart" column is where the arena's NPC rules leave vanilla. It is open-loop: once the NPCs stand elsewhere, the replayed swings miss.
  - `dist` is our bundle (`npm run build` first). Memory is not seeded, so it decides from what it sees: no Raid log and no latches. Use it for heap snapshots, not for verdicts.
- **Determinism:** xxscreeps breaks ties between moves at random. A seeded processor makes one `--seed` replay the same run every time. Run a few seeds and read the spread, not one run.
- **Heap snapshot:** `--heap [tick]` writes `<slug>.<bot>.heapsnapshot` of our isolate, which loads in Chrome DevTools' Memory tab. Its limit is isolated-vm's 256 MB, not the MMO's. This replaces the screepsmod-inspector route.

**When to cross-validate:** for any arena verdict that rests on how NPCs behave, and for any new combat rule in the arena's physics (`Arena.fs`) or the `Invader` script.

**What it cannot tell you:**
- CPU numbers: the engine and the sandbox both differ from the MMO.
- Season mechanics: there are no Reactor, Thorium or score.
- Market or inter-shard behaviour.
- shardSeason's exact rules: xxscreeps is vanilla with small deviations. Its `flee` counts only armed hostiles, and its healer picks a target by range.
- Anything outside the scenario's rooms: every border is sealed except the ones between scenario rooms. Roads, sites and the modern structures (observer, factory, nuker, power spawn) are left out, and the run names what it left out. A player's bodies have no code and stand still.
- A pass or fail: it is too slow and too far from shardSeason to be a gate, so nothing in `npm test` runs it.

**Calibration, #535 W11S28 (t973,670), arena with #541's invader AI against xxscreeps 0.2.1.** Death ticks are listed in the order 735 / 768 / 779 / 495 / 657:

| | deaths | guard min hits |
|---|---|---|
| arena (fabot guards) | t8 / t70 / t15 / t40 / t33 | 2286 |
| xxscreeps, stand-in, seeds 1-5 | t8 / t55-79 / t64-73 / t48-68 / t53-72 | 2670 (seed 1) |

- **The verdict agrees:** every invader dies, and neither guard does.
- **Under replay (seed 1), the bodies part from the arena early:**
  - Healers at t2: both healers want tile 11,19 and the tie goes the other way.
  - Shooter 779 at t4 and 768 at t7.
  - The small body at t6: in the arena it stops beside the guard, while in xxscreeps it steps on into the guard's path.
- **The shooters live 20-50 ticks longer:** shooter 779 dies at t15 in the arena, cornered in the west wall pocket, and at t64-73 under the stand-in. Part of that gap is the stand-in, which does not run it into the pocket the way our guards do.
