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
