# Profiling

`npm run profile` drives the compiled `loop()` in Node against a synthetic
world (`scripts/profile.mjs`) under the V8 sampling profiler and prints
ms/tick plus two hotspot tables (by self and by inclusive time). The raw
`build/fabot.cpuprofile` opens in Chrome DevTools or speedscope.

```sh
npm run build
npm run profile                                # 100 ticks, stub scenario, RCL5
npm run profile -- 500 30                      # ticks, top-N rows per table
npm run profile -- 100 30 --census-every 10    # move the census every 10 ticks
npm run profile -- --scenario outpost          # the colony and its neighbours
npm run profile -- --scenario young            # one colony at RCL1, on a 300 bank
npm run profile -- --scenario pair             # an RCL5 mother and her child
npm run profile -- --scenario outpost --raided # one raider in the first outpost
npm run profile -- --level 4                   # build the colony at another RCL
```

Current numbers, the live CPU history and the per-hotspot attribution are
tracked in #50 — read that, not this file, for where the time goes.

## What the number means

The harness implements only the API surface declared in
`src/App/Bindings.fs`; the world is frozen between ticks and engine-side
costs (the prelude, 0.2 CPU per accepted intent, Memory serialization) are
not simulated. **Relative percentages are the signal; absolute ms/tick is
a floor.** Only runs at the same `--scenario`, `--level` and machine
compare with each other; run-to-run spread is about a tenth of a
millisecond.

Every run prints ADR 0041's trigger to revisit the layered projection —
**a mean tick above 50 ms, or any single tick above 80** — judged against
the run's own ticks. The thresholds live in `scripts/cpu-trigger.mjs`,
shared with `npm run observe -- cpu`, which reads the deployed bundle's own
`Game.cpu.getUsed()` line from `Memory.fabot.observe.cpu`. Only the live
reading decides it: a profile that prints "not triggered" has failed to
trip the trigger, not cleared it. The trigger is a reason to re-decide,
never a budget the bot acts on (ADR 0041: CPU is measured, not budgeted).

## Scenarios

| scenario  | world                                                                                                                                                                                                      | default level |
| --------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------- |
| `stub`    | one synthetic room `W1N1` shaped like the live colony; every neighbour is solid rock                                                                                                                       | RCL5          |
| `outpost` | `W12S28` with its neighbours `W12S27` and `W13S28` from the committed captures (ADR 0036), source containers, reservations and vision stood as on the live server                                          | RCL5          |
| `young`   | `W13S28` alone as a colony at RCL1 on a 300 bank: two source containers, nothing else built (no road, rampart or buffer below `Colony.bootstrapLevel`)                                                     | RCL1          |
| `pair`    | the tick the live bot runs (ADR 0047, ADR 0052): the mother `W12S28` at RCL5 with outpost `W12S27`, and the child `W13S28` bootstrapping with its own Spawn2 at `16,12`; prints a `decide by colony` table | child RCL2    |

`--level N` moves the scenario's colony (`pair` moves the child; the mother
stays at RCL5). `--scenario pair --level 3` is a reading, not a mistake:
at `Colony.bootstrapLevel` the mother stops projecting the child's room
and the two run side by side sharing nothing.

## `--raided`: the one flag that puts a hostile in the world

`--raided` stands one armed hostile in the first outpost of the two
scenarios that furnish one, `outpost` and `pair`; every other scenario
refuses the flag rather than ignoring it. The body is the engine's own
`smallMelee` (2 TOUGH, 5 MOVE, RANGED_ATTACK, WORK, ATTACK — 1,000 hits),
which is nine remote raids in ten (ADR 0056), and it stands on the nearest
free ground to that room's rock, the containers held back so it cannot take
the Post an Anchor garrisons.

It exists because **no other run executes `Threats.Safe`, `Flee` or the
subtraction of a Reach out of a Work Area at all** (ADR 0033) — until this
flag the profiler had never entered them, so their cost was unmeasured
rather than small. The report gains a `raid` block, printed straight after
the per-colony decide table whose ms it qualifies and above the terrain and
observe lines, naming the room, the hostile's tile, the creeps that fled
and the Task each creep standing in that room ended the run holding, read
off `Memory.fabot.assignments`. A raided run that names no runner has not
exercised those paths after all.

A raided run compares only with another raided one: the Reach costs real
ms, and reading it against a quiet baseline reads the raid as a regression.

Known fictions, each named in the report: `stub` still casts ADR 0042's
reservers (quota per _declared_ outpost, not per seen one) and they stand
beside the spawn with nowhere to walk; its upgrader seat stands empty at
every level (not hired under an 800 bank, and one Post's surplus never
buys a body above it).

## Furnishing and fleet are derived from the level

Nothing about the room or the fleet is a hand-written count:

- Extensions, towers and Storage come off `CONTROLLER_STRUCTURES`; three
  extensions are held back as construction sites so the Build family is
  measured, which is why an RCL5 run reports a bank of **1650**, not 1800.
- The cluster steps over the working ground (source Seats, the Upgrade
  Work Area), as ADR 0022 makes the Layout do.
- The fleet is whatever the bundle hires: the harness starts with no creep
  and honours every `SpawnCreep` intent until the bot stops asking. The
  fleet therefore **shrinks as the level climbs** (a bigger bank buys
  bigger bodies) and the tick bottoms out around RCL5.
- The `outpost` scenario also stands a crew the bundle does not hire — a
  hauler per outpost container — after the fleet is hired. It is a floor,
  not a quota.

**A row added to `Decide.patternTable` owes every scenario a `stations`
entry** (the tile that row works from). A hire with no station makes the
run throw, and the throw names the missing edit. This bill has come due
twice (the reserver at #163, the upgrader at #199).

## Reading a perturbed run

`--census-every N` is the only thing that lifts the freeze: every Nth tick
it paves or unpaves one tile of the scenario's spare lane, so the census
signature moves and the census-keyed memos — the Layout, the hauler quota
(ADR 0017), the spawn walks (ADR 0032) — recompute. Without it those paths
run once, in warm-up, and measure as zero forever after.

Read such a run **by class, never by a pooled mean**: ms/tick, both hotspot
tables and the `census-keyed frames` table all split into perturbed ticks
(which pay the recompute) and quiet ticks (which recall it).

- The perturbed column is a per-recompute price; divide by N for a colony
  whose census moves that often.
- Its rows are inclusive and nest (`trunkPath` runs inside `planLayout`),
  so read them one at a time; the column is not a sum.
- Census-keyed frames are printed even below the top-N cut, under a
  `below the cut` line.
- Samples the profiler parks at the root (GC, its own start and stop)
  belong to no tick and to neither class.

## History

The baseline moved with the world it measures, and older numbers do not
compare with today's: #144 furnished the room and derived the fleet; #163
stood the outposts' containers and reservations (three worked rooms, not
three visible ones) and seated the reserver; #199 seated the upgrader;
ADR 0052 added `young` and `pair`. Between #144 and #199 the `perf(atlas)`
line (#168 → #177) took the RCL5 `outpost` quiet tick from 10.5 to 2.6 ms,
and ADR 0046/0049 took the fleet from 20 to 17. The per-commit numbers are
on #50.
