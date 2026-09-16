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
npm run profile -- --scenario reactor          # the season's programme: a mine and an errand
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
compare with each other.

**Run-to-run spread is wider than this page used to claim.** It said "about a
tenth of a millisecond", which is the spread *within* one session; across
sessions on the same machine the median of an unchanged arm drifts by about
**±6%** (measured 2026-09-17 while landing #365: base-arm medians of the same
build wandered between 2.55 and 2.70 ms of per-colony `decide` on `pair
--level 7`). So:

- A claim under about **5%** needs 300-tick rounds, at least three pairs, **both
  arm orders**, and a reported spread. A single number in that range is not
  evidence here.
- Interleave the arms with a **full rebuild between each**, and never compare a
  figure from one session against one written down in another — including the
  figures in this repo's own research documents, which name the run they came
  from for exactly this reason.

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
| `pair`    | the shape of the tick the live bot runs (ADR 0047, ADR 0052): the mother `W12S28` at RCL5 with outpost `W12S27`, and the child `W13S28` bootstrapping with its own Spawn2 at `16,12`; prints a `decide by colony` table | child RCL2    |
| `reactor` | the season's programme (ADR 0057, ADR 0060): the third colony `W15S28` at RCL6 with its Thorium deposit dug — a standing extractor, the mine container on its Seat and a pile on the mine post — and the sector Reactor in `W15S25` declared as an **errand** three crossings out, by `W15S27` and the Source Keeper room `W15S26`, with the re-claimer standing on the reactor's own ring; prints an `errand` block and a `mine` block | RCL6          |

The levels are the shape being profiled and not today's live RCL — the live
pair passed RCL6 on ADR 0055's move. What every scenario standing a declared
home as a colony **does** track is that colony's declaration, and it tracks it
without a human's help: the rooms a scenario furnishes are written down here,
but the rooms it must additionally answer terrain for are **derived** from
`Colony.declared` by asking the bundle's own `World.worldRooms` (#287), so a
declaration moved in `src/Core/Types/Colonies.fs` moves the harness on the same
commit. The set is deliberately not written down again here: it is the
colony's whole projection, outposts and the transit rooms a multi-hop chain
crosses alike (ADR 0058), which is why declaring a room two hops out widens it
by two rooms rather than one. Each room in it is read off its committed capture
(ADR 0036) and never invented, so a declared room with no capture fails as
`capture W16S27.room is not committed`, naming the file to commit rather than
blaming the stub world for holding no terrain.

`--level N` moves the scenario's colony (`pair` moves the child; the mother
stays at RCL5). `--scenario pair --level 3` is a reading, not a mistake:
at `Colony.bootstrapLevel` the mother stops projecting the child's room
and the two run side by side sharing nothing. `--scenario reactor --level 5`
is the same kind of reading: below `Tuning.ExtractorLevel` the Layout plans
neither an extractor nor a mineral container, so the scenario stands neither,
the miner row hires nobody and the whole ore half but the Storage's own
Thorium Refill goes unexecuted — which the `mine` block says rather than
leaving a reader to infer it from a fleet one body short. Below **RCL4** even
that last arm is gone, the room standing no Storage for it to be pooled
against, and the block reads the assignment table back and says so rather than
naming a Refill no body held: a printed coverage claim nobody reads back is
#308's own failure, whatever level it is true at.

## `reactor`: the season's programme, and the half of it that exists

`--scenario reactor` is the answer to #308 and to ADR 0060's last
consequence, and it exists for ADR 0056's standing rule: **a path no scenario
executes is a path no profile has ever seen.** Every scenario has furnished
its room's mineral all along — `furnishHome` fills `FIND_MINERALS` — and until
this one none furnished an extractor, a mineral container or a Thorium pile, so
`ourMineralContainers` was `[]` at every level of every run, the mine
Withdraw was pooled by nothing, the hauler unit's mine term was always zero
and `depositIsDiggable` was never asked a question with a non-trivial answer.
Every "green profile" on the ADR 0057 and ADR 0060 queue had proved the
_energy_ half and nothing else, and eight shipped changes had to say so in
their own change descriptions.

What it stands, and what each thing is there to execute:

- **`W15S28` at RCL6** with a spawn at `18,30`, its two energy Posts, its
  cluster and its trunks, exactly as `furnishHome` furnishes every other
  home room.
- **The mine**: a standing extractor on the deposit at `29,12` (cooldown 0,
  so the dig's act is issued), the mineral container on the Seat the
  furnishing left free, holding 1,800 T — checked against the bundle's own
  `Tuning.MineContactCliff` where the mine is stood, so #306's past-the-cliff
  rung is in the ms and a cliff raised in `Rules.fs` fails the run instead of
  quietly moving it — and a 630 T pile on the mine Post, checked the same way
  against `Tuning.PickupThreshold`. That is the miner row's quota, the
  ore's Harvest, the mine Withdraw and its rung, the floor's Pickup and the
  Storage's Thorium Refill, all reachable for the first time.
- **`W15S25` declared as an errand** in `Colony.declared`, with the sector
  Reactor standing in it under a rival's flag. The id and the tile are read
  off the bundle's own declaration and are written down nowhere in the
  harness (#287's rule), so a declaration a human moves moves the scenario on
  the same commit. That is the errand's projection entry, `Errand.routable`,
  the errand narrowing over a room we _can_ see, the `Reclaim` Task and the
  `ClaimReactor` act. What it is **not** is the refusal: the declaration is
  routable, so `Errand.routable`'s refusing arm and the refused entry on the
  layout record are not in these ms, and the `errand` block says so on the run
  rather than leaving the ticket's word for it. That half is pinned in
  `ViewTests`, where a filter costs less to check than to walk.
- **`W15S26` masked at the keeper margin**, and `W15S27` beside it, both dark
  and carrying terrain alone. Nothing here masks anything: the mask is the
  bundle's own `Keepers.maskedTilesIn` at `Tuning.keeperMargin`, and the
  three-hop chain `W15S28 → W15S27 → W15S26 → W15S25` is priced over it every
  tick by `World.linked`. The report prints the margin and the tile count it
  read back, so the number in the report is the bundle's and never this
  file's.
- **A body three hops out**: the re-claimer, cast from the reserver row — a
  `[Claim; Move]` body is one pattern however it was bought (ADR 0006), which
  is why the errand's seat is a third entry in that row's quota — and
  stationed on the reactor's own range-1 ring, which is where the act is made
  from. At the far **end** of the crossing and not part-way along it: the two
  rooms in between carry terrain alone, so a body mid-crossing means standing a
  third room, and `ClaimReactor` is gated on range 1 (ADR 0057 decision 5) and
  would then be issued never. The chain itself is priced across the mask every
  tick by `World.linked` either way.

The report gains an **`errand`** block and a **`mine`** block, printed after
the `held repair` block, and both are written to be read as _claims about
branches_ rather than as decoration. The `errand` block separates three
things a single ms figure would blur: the **chain**, evidenced by the colony's
own layout record naming no refused declaration (an errand no chain reaches
is named there under its kind, ADR 0060 decision 1); the **Task**, read off
`Memory.fabot.assignments`; and the **act**, counted on the stub's own
`claimReactor`, because a `Reclaim` is held on every tick of a re-claimer's
life while `ClaimReactor` is issued only on a tick the reactor is not ours.
The `mine` block does the same for the four ore acts, each counted against
the mine's own target and, where the engine takes one, its own resource — a
bare `harvest` count is the two Anchors' energy digs with the mine's folded
invisibly into it.

**The delivery is here as two ordinary resource Tasks, not a `Deliver`.** The
scenario first lets the resident re-claimer take the Reactor, then loads the
fixed courier standing in W15S27. On the next tick the assignment table must
name `refill:<reactor-id>:Thorium`: a real multi-room price for the remainder
of the outward leg through W15S26's masked Source Keeper terrain. The harness
cannot walk the preceding Storage Withdraw, so loading on the ownership
transition is its one fiction; the row, sink gate, priced match and emitted
Refill are the production path (#319, ADR 0067).

A `reactor` run compares only with another `reactor` run. It is a different
colony at a different level over a different room set, and reading it against
`outpost` reads the whole season's programme as a regression.

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

## Known fictions

Each is named in the report of the run it belongs to, and the list is kept
honestly rather than trimmed: a scenario that implied coverage it does not
have is the failure #308 found, and it is cheaper to write the gap down than
to re-derive it from a stack trace two tickets later.

`stub` still casts ADR 0042's reservers (quota per _declared_ outpost, not
per seen one) and they stand beside the spawn with nowhere to walk; its
upgrader seat stands empty at every level (not hired under an 800 bank, and
one Post's surplus never buys a body above it).

`reactor` is the newest and the one with the most to declare, because it
stands a room at a level the live server has not reached:

- **`W15S28` is RCL5 live and the scenario stands it at RCL6.** Read off the
  server at t411,716 on 2026-09-13: controller level 5, 361,402 of 1,215,000,
  `Spawn3` at `18,30`, 30 extensions, 2 towers, a Storage holding 253,194 and
  **no extractor** — the kind unlocks at RCL6 (ADR 0060 decision 3 puts that
  about 20,000 ticks out, and it is the programme's whole critical path). So
  the level, the extension count and the mine are the shape the ADR aims at and
  not today's room. The spawn's name and tile are **not** a fiction: the sweep
  in `docs/research/third-colony.md` §2 picked `18,30` (57 trunk tiles,
  `ext=40 / unserved=0 / unrouted=0`) before the room was claimed and the live
  colony stands `Spawn3` on that same tile today.
- **`W15S26` and `W15S27` stand no hostile at all.** Live at t411,716 on
  2026-09-13, W15S26 holds its four keeper lairs with four keepers standing on
  them, and W15S27's controller carries an Invader reservation (user `2`,
  ending t414,454) with no core object left in the room. The
  run's ms therefore include the masked _layer_ and not one keeper in a
  hostile list — so ADR 0033's answer for the keeper that has walked off its
  rock (#327), which the mask deliberately does not cover, is executed by
  nothing here.
- **Nothing about the reactor but its id and its owner.** Its store, its
  `continuousWork` and its tile are absent, which is the tree's own state
  rather than the harness's: the tile is the declaration's, and no rule reads
  the other two (ADR 0007). A scenario cannot furnish a fact nothing asks
  for.
- **No ore but Thorium anywhere.** `loadCapture` keeps the `T` mineral and
  drops the room's ordinary ore, so W15S28's `O` and W15S25's `U` are not in
  any `FIND_MINERALS` table — and the filter `World.factsOf` applies to tell
  them apart is a branch no scenario has ever given a second kind to take.
- **The extractor's cooldown is 0 on every tick**, where the engine allows a
  dig one tick in six (`EXTRACTOR_COOLDOWN` is 5). The world is frozen, so
  this is the frozen world's version of "now"; the withheld arm of ADR 0057
  decision 2's cooldown gate is not executed.
- **The act counts are "reachable on every tick" and never a cadence.** The
  world is otherwise frozen, but `claimReactor` deliberately transfers
  ownership on its first call so the scenario executes both sides of that
  gate and opens the courier's sink. Read the remaining counts as "the act was
  reachable on each tick", which
  is what makes a zero meaningful, and never as a rate.
- **The mineral container and the pile share a tile**, which is the live shape
  `Pool.fs` reads the pile as (that container's next dig, landed on the floor
  because the store was full, #311) — but the two are then priced at the
  **same** travel cost, so which of the two ore rungs an empty carrier takes is
  arbitrated over no distance in this world. The engine pours a drop into a
  container standing on the same tile first (`_create-energy.js`), so the pair
  as stood — a container at 1,800 of 2,000 under a 630 T pile — is the tick
  after a draw and not the tick after a dig.
- **The ore crew and the seeded ore draws are a floor and not a quota** (see
  below).

## The seeded held Repair

For the same reason, every run seeds **one creep holding a Repair** on a
road standing between `Tuning.RepairTrigger` and `Tuning.RepairWholeLine`
(ADR 0061), written into `Memory.fabot.assignments` before the first
warm-up tick. Such a structure is pooled exactly while somebody holds it,
so without a seeded holder the held arm of the repair line is a branch no
scenario executes — and the world is frozen, so a colony left to itself
never arrives at one between the lines by repairing. The report's `held
repair` block, printed after the `raid` block, names the creep, the road's
hits and what the assignment table holds after the last tick: a seed that
did not survive means the held line was measured for part of the run at
most. Two runs print the no-seed line instead, for two different reasons, and
both say which: `young` stands at RCL1 and paves nothing, so it furnishes no
such road at all; `reactor` paves plenty at RCL6 but hires no body that can
repair one. Either way the block names the gap rather than implying a branch
it never ran.

The seed costs the run **one body's ordinary work**: the creep it names
spends every profiled tick on that one road and harvests, hauls and
upgrades nothing. So a parent/child A/B over `Memory` is not a like-for-
like comparison of that creep — on a bundle without the two lines the same
assignment evaporates on tick 1 and the body goes back to the pool, so the
seeded creep's whole history differs between the two runs by construction.
Read a diff of `Memory` with the seeded creep **excluded**; what the seed
is for is the ms, and the ms of the branch it executes.

## The ore crew and the seeded ore draws

The `reactor` scenario needs the same technique twice more, and for a reason
worth stating precisely, because it is the difference between a Task **pooled**
and a Task **executed** — which is exactly what #308 found this harness
reporting as coverage.

The ore crew is three bodies the bundle does not hire, stood after the fleet
is (as the `outpost` scenario's far-end haulers are): one **holding ore**,
beside the Storage, because `Refill(storage, Thorium)` is applicable to a body
carrying Thorium and to no other — ore aboard shuts every energy intake it has
(ADR 0057 decision 3) — and two **empty**, at the mine, one per ore draw the
mine offers. It is a floor and not a quota: the hauler row's own quota already
prices the mine's round trips (`Quota.mineRows`), and what this stands is one
body per _arm_.

Standing them is not enough, and the run without the seed is the proof. Both
ore draws are pooled on every tick and both lose every body to the energy arms
beside them, because a source container's Withdraw is at **Feeding** tier where
the ore's is the Storage's — #306 gives the ore its rungs _inside_ that tier
and not above it. In a live colony the feeding arms drain and the ore's turn
comes — which is measured and not assumed: at t411,716 on 2026-09-13 W12S28's
Storage held **19,848 T** and W13S28's **15,716 T**, every gram of it taken
through this same pair of arms under the same ranking. In a frozen world the
source containers stand at 1,500 for ever and the turn never comes at all. So
`withdraw:<mine container>:Thorium` and `pickup:<pile>:Thorium`
are seeded into `Memory.fabot.assignments` before the first warm-up tick, and
the anti-thrash keeps a still-valid assignment rather than re-competing it.
The `mine` block reads the table **back** and says for each whether the seed
survived, exactly as the held Repair's does.

The laden body's tile is load-bearing in the same way and was got wrong first:
standing it at the mine held `Refill(storage, Thorium)` perfectly well and
issued the `transfer` **never**, the body being twenty tiles from its Work Area
in a world that never finishes a walk. A Task held and an act never issued is
the shape of a harness fiction, which is why the block counts the two apart.

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
  not a quota. The `reactor` scenario's ore crew is the second of them.

**A row added to `Decide.patternTable` owes every scenario a `stations`
entry** (the tile that row works from). A hire with no station makes the
run throw, and the throw names the missing edit. This bill has come due
four times (the reserver at #163, the upgrader at #199, the guard at
#254, the miner at #321 — which had never been cast in this harness at all,
its quota being zero wherever no extractor stands). An entry may
legitimately be **empty** — the guard row stands one body per armed hostile
in a declared outpost (ADR 0056), so every scenario but a `--raided` one
stations none, and a cast against such a world throws under its own message
rather than standing the body nowhere.

**And an Intent added to the Executor owes `stubCreep` the verb it calls.**
The same bill on the other axis, and it stands unpaid for longer, because
nothing goes red about it: `dotnet test` never drives the Executor against a
stub creep, so a missing verb is invisible until a scenario first matches the
row that uses it — and then it takes the whole run down with a `TypeError`
from inside `withCreepTarget`, which is how a stub says "this row does not
exist". `claimReactor` (ADR 0057 decision 5, #318) was missing from the day it
was bound; the `reactor` scenario found it on its first run, in the hiring
loop rather than a profiled tick.

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
and ADR 0046/0049 took the fleet from 20 to 17. #321 added `reactor`, which
compares with nothing before it: it is the first run to execute one line of
the Thorium path or of the errand, so its ms are a new baseline and not a
movement in an old one. The per-commit numbers are on #50.
