# Where the next CPU cut comes from

*Research, not implementation. Every number below is either a measurement this
document's author ran in `/home/xerxes2/Dev/fabot-cpu` on top of
`48d19016` (`main`), or a line of source read at that commit. Nothing was
committed, deployed, or asked of the Screeps API.*

ADR 0041's revisit trigger is firing (live 100-tick windows: mean 57.98 ms, max
103.95 ms, `decide` 36.61 ms), and `docs/research/outpost-wave-2.md` now holds a
room back on CPU grounds. The question this answers is **where the next real cut
is**, ranked by measured ms per unit of risk.

The headline, before the detail:

> **Every whole-room Dijkstra flood this bot runs is a `Reserve` far field.** In
> the `pair --level 7` tick there are eighteen of them, they are 76% of all the
> flood work, and they are the same eighteen floods every tick. The flood is not
> too wide, the heap is not too slow, and `Option.get` is not one bad call site —
> the tick simply re-floods four reserve chains twice each, forever.

---

## 1. Method

- Harness: `npm run profile -- 40 1 --scenario pair --level 7`, which is the
  scenario the existing record (`docs/research/fourth-colony.md` §10, #332,
  #353, #278) is written against. `--scenario reactor --level 6` is used as the
  second reading, because `outpost-wave-2.md`'s numbers are on that one.
- Every A/B pair was **interleaved and rebuilt between runs** (`/tmp/cpu/run.sh`:
  `npm run build` then `npm run profile`, one variant at a time, alternating).
  Single runs are quoted only where a deterministic counter, not a clock, is the
  evidence.
- **The harness clock's spread.** Twenty baseline runs of 40 ticks gave `decide`
  (both colonies) 5.88–6.69 ms, median **6.09**, and whole-tick 8.12–9.28 ms,
  median **8.47**; one further run came back at 9.28/13.02 while another agent
  was running `dotnet test` on this machine and is excluded as contention. So a
  candidate worth less than ~0.4 ms of `decide` **cannot be resolved by the
  clock in this harness**, and for those the evidence below is a counter (pops,
  calls) that does not move between runs at all.
- Profile shares are read off `build/fabot.cpuprofile` with a 12-line subtree
  summer (`/tmp/cpu/prof.mjs`, the helper from `/tmp/scout/sub.mjs` plus a
  caller-attribution mode). At 40 ticks the profiler's own `post` frame is 0.00%
  of samples, so these percentages have no profiler overhead in the denominator;
  at 8 ticks it is 30%+ and they do, which is why every share quoted here comes
  from a 40-tick run.
- The self-check the brief warns about is clean on both scenarios used: the
  terrain-fingerprint line reports `all grids distinct`.
- Semantic equality of a variant was checked by diffing the harness's **whole
  report** minus its timing lines: same creeps, same stations, same fleet, same
  held repair, same terrain reads.

Temporary instrumentation (flood/pop counters in `Grid.fs`, key-logging in
`Atlas.farFieldAlong`, call counters on `seams`/`joinedAcross`/`World.linked`,
per-call-site flood tagging) was written, measured, and **reverted**. `jj st`
reports no change; `npm run format:check`, `npm run build` and `dotnet test`
(1389 passing) are green.

---

## 2. Where the tick actually goes

`pair --level 7`, 40 ticks, samples grouped by frame family
(`/tmp/cpu/family.mjs`):

| Share of tick | Family |
|---:|---|
| **26.2%** | the Dijkstra flood — `settleTo` self 14.1%, `pop` 10.2%, `push` 0.8%, `floodFromAllSeeded` 0.9% |
| 10.3% | `value` / `some` — Fable's `Option.get` |
| 9.9% | `MapTree*` / `SetTree*` / `FSharpMap` / `FSharpSet` |
| 9.0% | structural `compare` / `equals` / `GetHashCode` |
| 5.5% | `FSharpList` and list combinators |
| 5.3% | garbage collector |
| 1.9% | `Dictionary` |
| 31.9% | everything else (the bot's own code) |

So the flood is a quarter of the tick and the **F# immutable-collection tax is
36.6%** — bigger than the flood. Two subtrees for orientation
(`decideUnarbitrated` 67.8%, `matchCreeps` 39.1%, `loop` 91.9%):

| Subtree | Share |
|---|---:|
| `joinedAlong` / `pricedAcrossInto` | 32.4% |
| `farFieldAlong` | 22.8% |
| `chainedInto` | 22.3% |
| `drained` (whole-room floods) | 19.3% |
| `foldChain` | 13.0% |
| `RoomName.routesBy` | 10.2% |
| `floodPricedInto` | 8.8% |
| **`World.linked`** | **7.6%** |
| `nearestReached` | 6.8% |
| `joinedAcross` | 6.4% |
| `ofViewRecalling` | 6.2% |
| `Seam.pairsAcross` | 5.9% |
| `seams` / `Seam.bandBy` | 1.8% / 1.7% |

Two things in that table were not in the brief's list and matter:
`World.linked` at 7.6% is **outside `decide` entirely** (§5.2), and `drained`
— whole-room floods — is 19.3% of 26.2% of flood work, i.e. the resumable
per-creep floods are nearly free by comparison.

### 2.1 Answering the `value` question

`value` is Fable's `Option.get` (`dist/main.js:431`: a null test, an
`instanceof Some` test, a field read). Its 9.28% of self time attributed **by
immediate caller**:

| Share of tick | Caller |
|---:|---|
| 2.23% | `FSharpList__get_Tail` |
| 2.23% | `MapTreeModule_tryFind` |
| 1.23% | anonymous lambdas (list/seq combinator bodies) |
| 0.61% | `IEnumerator.MoveNext` |
| 0.48% | `MapTreeModule_add` |
| 0.39% | `Enumerator_generateWhileSome` |
| 0.31% each | `SetTreeModule_add`, `SetTreeModule_mem`, `MapTreeModule_mk` |
| 0.26% each | `SetTreeModule_rebalance`, `MapTreeModule_rebalance` |

There is no hot caller because there is no hot caller to find. Fable stores an
F# list's tail as `Option<List<T>>`:

```js
function FSharpList__get_Tail(xs) {
  const matchValue = xs.tail;
  if (matchValue != null) { return value(matchValue); } else { ... }
}
```

so **every cons cell walked anywhere in the bot** pays a call, two null tests and
an `instanceof`; `MapTree.tryFind` returns an option and pays it per lookup. The
9–12% is the aggregate price of F# lists and `Map`/`Set` in a JS runtime, spread
over every `List.filter`, `Map.tryFind` and `Set.contains` in the codebase. It is
**not a line item that can be cut** — only a consequence of the data structures,
and the only lever on it is "call the collections less", which is what §5.1 and
§5.2 do by removing whole computations rather than making them cheaper.

---

## 3. The floods (brief item 1 and 2)

### 3.1 The counts

Temporary counters in `Grid.fs` (`statFloods`, `statPops`, plus a call-site tag
set around each `drained`), printed per tick from `Main.loop`. Steady state,
`pair --level 7`:

```
floods=32  drains=18  pops=39646  settled=39562  pushes=40118
sites = resumable 9250 | floodPricedInto 12784 | foldChain 17612 | seamWalk 0 | trunk/walk 0
```

- 39,646 pops a tick, of which **30,396 (76.7%) are the far leg** — the
  `floodPricedInto` into the Task's ground plus the per-hop `foldChain` floods.
- `settled` ≈ `pops` (39,562 of 39,646): almost no stale heap entries. The heap
  is doing near-zero wasted work.
- The memoised per-creep `Resuming` floods are 9,250 pops — 23%, and they are
  already stopping early, which is #174 working.
- `seamWalk` and the trunk/spawn-walk floods contribute **zero** pops at steady
  state: they are behind the plan memo and the census (ADR 0032) and never
  re-run in a frozen world.

### 3.2 Every one of those floods is a `Reserve`

Logging each `farFieldAlong` **miss** with its whole key gives, identically on
every steady-state tick:

```
chain=W12S28>W12S29>W11S29  task=Reserve 6a8c…95f4  heavy=false factor=7/7  pricing=Walk        origins=4
chain=W13S29>W12S29>W11S29  task=Reserve 6a8c…95f4  heavy=false factor=7/7  pricing=Walk        origins=4
chain=W12S28>W11S28         task=Reserve 6a8c…95f2  heavy=false factor=2/2  pricing=TravelCost  origins=3
chain=W12S28>W11S28         task=Reserve 6a8c…95f2  heavy=false factor=2/2  pricing=Walk        origins=3
chain=W13S29                task=Reserve 6a8c…9367  heavy=false factor=7/7  pricing=TravelCost  origins=2
chain=W13S29                task=Reserve 6a8c…9367  heavy=false factor=7/7  pricing=Walk        origins=2
chain=W12S28>W12S29>W11S29  task=Reserve 6a8c…95f4  heavy=false factor=7/7  pricing=TravelCost  origins=4
chain=W13S29>W12S29>W11S29  task=Reserve 6a8c…95f4  heavy=false factor=7/7  pricing=TravelCost  origins=4
```

Eight misses a tick, **all of them `Reserve`**, four chains × two pricings. A
chain of *n* rooms is *n* whole-room floods (`chainedInto` = one
`floodPricedInto` plus one `foldChain` flood per hop), so 3 + 3 + 2 + 1 = 9
floods per pricing, 18 per tick — exactly the `drains=18` the counter reports.

`farFieldAlong` is called 12 times a tick and misses 8. `joinedAcross` runs 12
times over 237 crossings total. So the cross-room pricing machinery is asked
only a dozen questions a tick and spends 22.8% of the tick on them, because each
one drags one to three whole-room floods behind it.

This is the same number the brief already had from another direction ("+20% of a
tick when it was only a declaration"): a declaration that is only reserving
**is** these floods.

### 3.3 The extent of a far field is *not* the problem — measured

The brief's hypothesis was that a far field floods a whole room while the caller
reads only the tiles a band lands on, so a bounded flood would be the biggest
line item. It was prototyped and measured, and **the hypothesis is wrong**.

Prototype (`/tmp/cpu/proto-resumable-plus-instr.diff`): `floodPricedInto` gains
a variant that returns the seeded `Flood` instead of the drained array;
`foldChain` threads a `Flood` and reads it with `reachedBy`; `chainedInto` and
`farFieldAlong` return a `Flood`; the `FarFields` memo holds `Flood`; the two
`reachedIn (farFieldAlong …)` / `reachedIn (chainedInto …)` readers become
`reachedBy`. `castAlong`, which does want the whole room, wraps its already
drained near field in a new `Grid.settledFlood` (empty heap, so a read advances
nothing) and drains the result as before.

Result, same counters:

```
before: floods=32 drains=18 pops=39646 pushes=40118
after:  floods=32 drains=0  pops=35408 pushes=36387  dry=0  reads=1541 unreachedReads=0
```

- **Only 10.7% of the pops go away.** Settling to the tiles beside a band's
  crossings costs ~90% of what settling the whole room costs, because the band
  is a whole room edge and the task's ground is inside the room: the farthest
  crossing is nearly the room's diameter away, and Dijkstra settles everything
  cheaper than it on the way.
- `dry=0`: no flood ever exhausts its heap afterwards, so the residual is not
  "an unreachable band tile forces a full drain". `unreachedReads=0`: every band
  tile is reachable. The 90% is honest geometry.
- It moves every far-leg read onto `reachedBy`: 1,541 entries into `settleTo` a
  tick where the baseline read a finished array. The ms difference was inside the
  harness's spread.

**Conclusion: a bounded or early-stopping far field is not the prize.** The
prize is not running the flood at all (§5.1, §5.3).

### 3.4 The relaxation loop is already as fast as it gets

`settleTo` self is 14.1% of the tick. The suspect was the per-neighbour bounds
arithmetic (`nx >= 0 && nx < roomSide && …`, eight times per settled tile).

Variant: an interior fast path — `x`/`y` both in `1 .. 48` takes a `for k in
0 .. 7` over a precomputed `neighbourOffsets` array and skips every bounds test;
border tiles keep the old nested loop. The relaxation body is written out twice
rather than factored into a closure, because factoring it produced
`const relax = (next) => {…}` **allocated per settled tile** and also made Fable
drop the hoisting of `flood.Weights`/`Occupied`/`StepPrices` into locals.

| | flood frames | `decide` (3 interleaved pairs) |
|---|---:|---|
| baseline | `settleTo` 14.14% + `pop` 10.20% + `push` 0.79% = **25.12%** | 6.64 / 6.07 / 5.88, median 6.07 |
| interior fast path | `settleTo` 14.61% + `pop` 9.71% + `push` 1.05% = **25.32%** | 6.30 / 5.96 / 6.00, median 6.00 |

**No effect.** V8 already eliminates those checks, and the loop's cost is the
memory traffic on five arrays plus the heap. Decisions were byte-identical.

### 3.5 The heap cannot be replaced without changing what the bot decides

`pop` self is 10.2% of the tick, so the whole ceiling of any heap change is ~11%.
Step prices are small integers, which invites a bucket (Dial) queue. The
tie-break is the obstacle: keys are `dist * 2500 + index`, and pop order within
one `dist` fixes which predecessor a tile keeps (`candidate < dist[next]` is
strict, so the *first* settler wins the `Parents` entry), which fixes
`firstStepOn`, which is the direction a creep walks on an equal-cost path.

A standalone benchmark (`/tmp/cpu/heapbench.mjs`: nine 50×50 Dijkstras, the
bot's own heap code against a bucket queue, both with the same relaxation):

| variant | time for 9 floods | agrees with the heap? |
|---|---:|---|
| binary heap (today) | 1.032 ms | — |
| bucket queue, each bucket index-sorted before draining | 1.273 ms (**+23%**) | yes — `dist` *and* `Parents` identical |
| bucket queue, unsorted | 0.712 ms (−31%) | **no** — `Parents` diverges (tile 992: parent 941 vs 1041) |

So the order-preserving bucket queue is *slower* than the heap, and the fast one
changes which tile a creep steps onto. Measured, and closed.

### 3.6 The `Parents` grid is not worth removing

Every flood allocates `Dist` and `Parents` as `Int32Array(2500)` and fills them;
only the per-creep `Resuming` floods ever read `Parents` (`firstStepOn`). The 18
far-leg floods allocate and write it for nothing.

Benchmarked directly (`node -e`, 2000 iterations): allocating and filling 2×2500
`Int32Array` 32 times costs 0.076 ms; once, 0.044 ms. **The `Parents` array is
0.032 ms a tick** — 0.4% of an 8.3 ms tick — plus one extra typed-array store per
relaxation. Not worth the plumbing. (Removing the writes outright breaks
movement, as expected: that variant fails the harness's own checks.)

---

## 4. What is paid per creep versus per colony (brief item 3)

Counters on the pricing path, steady state, `pair --level 7`:

```
seams=22 (7 distinct ordered pairs)   joinedOn=12   joinedAcross=12
crossings=237                          routes=9      farFieldAlong=12 (8 miss / 4 hit)
nearestReached=472                     World.linked=93 (26 distinct pairs)
```

The multiplicity the brief expected to find — `joinedAcross`'s band scan and the
near-leg relaxation repeated per creep — **is not there in this scenario**.
Twelve cross-room prices a tick is twelve band scans, and `joinedAcross` is 6.4%
of the tick for all twelve. Memoising the task-independent half of a band scan
(`besideExit farGround landing` per band, `exitPrice` per `(room, exit,
stepPrices)`) would therefore be worth at most a point or two, and the
`besideExitFrom nearGround from exitTile` half is genuinely per-creep and cannot
be shared.

The caveat is scale: twelve is what *this* fleet asks. The live colony has more
colonies and more creeps, and `joinedAcross` is per (creep × chain), so this line
grows with the fleet where §3's far fields grow with the *declarations*. It is
worth re-counting on the live shape before writing it off for good — but on
everything measurable here it is a small item.

---

## 5. The candidates, ranked

Ranked by measured saving per unit of risk. "Semantics" means: does the bot make
a different decision?

### 5.1 Hold the far field across ticks — **the big one**

**What.** `farFieldAlong`'s memo is `atlas.FarFields`, a `Dictionary` rebuilt
with the Atlas every tick (`Atlas.fs:377`). Every tick therefore re-floods the
same four reserve chains under the same two pricings. But the far field is a
pure function of: the chain (in the key), the Task and the work-heavy flag (in
the key), the fatigue factor and pricing (in the key), the **walking grid of each
room in the chain**, the **occupancy of each room in the chain** — and, for
`Walk` and `Baseline`, `Grid.pricingOf` discards the occupancy and substitutes
`noTraffic`, so those two pricings are traffic-blind by construction.

The walking grid is a function of terrain plus structures, which is exactly what
`Decide.censusSignature` signs — "the walk table's far leg floods the *goal*
room's grid, and a site outside home closes a tile there" (`Entry.fs`). The
machinery to carry a table across ticks under that signature already exists and
is already used for a flood table: `PlanMemo.Walks`, ADR 0032.

**Measured, as a ceiling.** Variant C1: `FarFields` points at a process-global
`Dictionary` that is never invalidated — a measurement device, not a proposal.

| | pops/tick | floods/tick | `decide` (ms, 40 ticks) | whole tick |
|---|---:|---:|---|---|
| baseline | 39,646 | 32 | 5.88–6.69, median **6.09** | median 8.47 |
| C1, `pair --level 7` | **9,250** (−77%) | 14 | 3.87–4.28, median **4.26** | median 6.56 |
| baseline, `reactor --level 6` | 25,130 | 18 | 4.23 | median 5.79 |
| C1, `reactor --level 6` | **8,973** (−64%) | 8 | 3.32 / 3.40 | median 5.01 |

Eight interleaved C1 runs against twenty baseline runs, **disjoint intervals**
(max C1 4.28 < min baseline 5.88). `decide` −30% on `pair`, −20% on `reactor`;
whole tick −21% and −13%. The harness report was byte-identical apart from the
timing lines, which also confirms the field really is tick-invariant in a frozen
world.

**Measured, as the safe half.** Variant C1b holds only `Walk` and `Baseline`
across ticks and leaves `TravelCost` per-tick, which needs no occupancy
reasoning at all:

| | pops/tick | `decide` |
|---|---:|---|
| C1b | 24,448 (−38%) | 4.81 / 5.11 / 5.32 / 5.37, median **5.22** (−15%) |

**Semantics.** None, *if the key is complete*. The key must gain:
1. the census signature — free, the memo already rides it (`PlanMemo`);
2. the **origins**. `farFieldAlong` keys on `(chain, task, workHeavy, factor,
   pricing)` but takes `origins` as an argument, and there are two call paths:
   `pricedAcross` passes `narrowedArea atlas creep task`, while `crossingToward`
   (`Atlas.fs:2238`, reached from the travel-cost reader at 2316 and the mover at
   2516) passes the **caller's** tile set — a Work Area with a Reach subtracted,
   or a Flee area (ADR 0033). Two different origin sets under one key is already
   possible *within* a tick today; a cross-tick memo would extend the same
   assumption in time. Instrumented for it: across `pair`, `reactor` and
   `outpost`, **zero keys were ever seen with two different origin lists**, so it
   does not fire — but the key should carry the origins (or their hash) before
   anything is cached across ticks. *(This is worth its own `needs-triage`
   issue: the aliasing is latent on `main` today, independent of any caching.)*
3. for `TravelCost` only, the occupancy of the chain's rooms — or skip
   `TravelCost`, which is C1b.

The memo must be **per colony** (`PlanMemo` already is, keyed by home): a room's
walking grid is the *projecting colony's* view of it, and ADR 0047 keeps two
colonies' views apart.

**Tests that would move.** None expected: `dotnet test` does not build an Atlas
across two ticks, and `ofView` would keep handing a fresh table. The `PlanMemo`
tests in `LayoutMemoTests.fs` own the signature and would gain a case for the new
table's lifetime. `ParallelSafetyTests` is the guard that a static table is never
reachable from a fixture, which is exactly why the table must ride `PlanMemo`
and not a module-level `let`.

**Risk.** Medium, and all of it in the key. A stale far field is a wrong travel
cost — a silent, persistent mis-ranking rather than a crash. Mitigation is
cheap: ship C1b first (traffic-blind pricings only, no occupancy question at
all), and a debug assertion that recomputes one cached field per *n* ticks and
compares would make the invalidation falsifiable rather than argued.

**Rank: 1.** −15% of `decide` for the safe half, −30% for the whole thing, on
measurements with disjoint intervals, and it is the only candidate that scales
*down* as declarations scale up — a fifth colony's reserve chain is cached like
the other four.

### 5.2 Memoise `World.linked` — cheapest safe ms on the list

**What.** `World.linked` (`Types/Sightings.fs:539`) rebuilds a Seam band from
scratch: `Seam.pairsAcross` allocates 48 tile pairs, then each survivor pays two
ring reads and up to eight `neighbours` reads through `Seam.landsOnGround`. It is
the predicate `RoomName.routesBy` explores every edge with, and it is called from
**two places that ask the same question**: `World.scanOf` narrows the declaration
by `Outpost.routable`/`Errand.routable`, and `ColonyView.ofWorld` builds the
`Refused` channel with `Outpost.refused`/`Errand.refused` — which is the
complement of the first over the same list. Each `routable`/`refused` runs
`routesBy` twice (home→room and room→home, `Colonies.fs:114-115`).

Measured: **93 calls a tick over 26 distinct ordered room pairs**, and
`World.linked`'s subtree is **7.6% of the tick** (`RoomName.routesBy` 10.2%, so
the BFS scaffolding around it is another 2.5%).

**Measured saving.** Variant C6 memoises `linked` on `(fromRoom, toRoom)`. Seven
interleaved pairs of 40-tick runs, whole tick:

```
A  8.56  8.29  8.30  7.98  8.12  8.00  8.54      median 8.30
C6 7.46  8.09  8.11  7.70  7.64  7.62  8.16      median 7.67
```

**7 of 7 paired runs lower**, paired deltas −1.10, −0.20, −0.19, −0.28, −0.48,
−0.38, −0.38, median **−0.38 ms of an 8.3 ms tick (−4.6%)**. The report was
byte-identical apart from timings.

One honest wrinkle: `decide` itself comes out 0.2–0.35 ms *higher* under C6 in
all seven pairs. `linked` runs in the **snapshot** phase, not `decide`
(`Main.fs`: `ColonyView.ofWorld` is before `atSnapshot`), so it cannot be making
`decide` slower; the likeliest reading is that the allocation it stops doing was
paying for GC pauses that now land in the next phase. The whole-tick column is
the one that matters and it is consistently lower.

**Semantics.** None. `linked` is a pure function of two rooms' border rings, the
far room's terrain and the keeper margin — all fixed for the life of the server.

**Where the table lives.** *Not* a module-level static: two test lists with
different terrain under the same room name would collide, which is the same
hazard `AGENTS.md` § Code hygiene states for an Atlas, and `ParallelSafetyTests`
exists to catch. It belongs on the `World` record, which is rebuilt once per tick
in `Main.loop` and is therefore per-tick by construction — the Atlas's own
convention. That caps the win at 93 → 26 calls (−72%) rather than the −100% the
static measured; the difference is 26 band builds, ~2% of the tick.

> **Superseded on 2026-09-18 (#370).** The table took the −100% this paragraph
> gave up, by a route it did not consider: the static lives in the **shell**
> (`Main.fs`, one `JoinTable` for the life of the process, handed in through
> `World.linkedRecalling`, `scanRecalling`, `creepColoniesRecalling` and
> `ColonyView.ofWorldRecalling`), while Core's old names stay as wrappers laying
> a fresh table per call — the `Atlas.ofView` / `ofViewRecalling` shape. No
> test list can share one, and no static in the test assembly reaches one, so
> the collision hazard above never reaches Core. What made a lifetime longer
> than the tick honest is that the one input which moves — whether the world
> holds a room at all — is read off `Rooms` ahead of the table on every ask
> (ADR 0031's amendment says so in the ADR's own terms). The per-tick `World`
> field was tried first and refused by `ParallelSafetyTests`, which reads the
> declared type: a `World` static with a `JoinTable option` in it is a fixture
> holding a table, whatever the value.

**Tests that would move.** None. `ViewTests` owns `Refused`, and the answers do
not change.

**Rank: 2.** Small but the best ratio on the list: ~0.3–0.4 ms of whole tick,
no semantic surface, no ADR to revisit, and the change is one table.

*Worth noting beside it and not measured: `scanOf` and `ofWorld` compute the
routability of the same declared rooms twice per colony per tick. Deriving it
once and handing both readers the answer would halve the remaining 10.2%
outright. That is a view-boundary change and a bigger diff than a memo.*

### 5.3 Share the far field between chains with a common suffix

**What.** `routes` hands out every shortest chain, and two chains toward the same
target from different rooms share a tail: `W12S28>W12S29>W11S29` and
`W13S29>W12S29>W11S29` share `W12S29>W11S29`. `chainedInto` folds the whole chain
from scratch each time, so the flood into W11S29 and the carry into W12S29 are
each computed **twice**. Making `farFieldAlong` recursive — a chain's field is
its own suffix's field carried across one hop, memoised in the same table under
the suffix's key — shares them.

**Measured.** Variant C2. The memo key, the fold's direction and the floods'
order are all unchanged, so the numbers are bit-identical:

| | floods/tick | pops/tick |
|---|---:|---:|
| baseline, `pair --level 7` | 32 | 39,646 |
| C2, `pair --level 7` | 28 | **32,204 (−18.8%)** |
| baseline, `reactor --level 6` | 18 | 25,130 |
| C2, `reactor --level 6` | 18 | 25,130 (**−0%**) |

`dotnet test`: **1389 passed, 0 failed**, unchanged. The harness report is
byte-identical apart from timings. The clock could not resolve it: −18.8% of 26%
of the tick is ~0.3 ms against a ±0.4 ms spread, and the three interleaved pairs
(5.56/6.21/6.18 against 5.90/9.28*/6.01) say nothing.

**Semantics.** None — proven by the test suite and by report equality, and
structurally: `foldChain` is a left fold over the reversed hops, so folding the
suffix first and carrying one more hop is the same sequence of floods in the same
order.

**Risk.** Low. ~25 lines in one function; the recursion is bounded by
`Tuning.MaxHops`. The one thing to get right is that the sub-chain's memo entry
is keyed by the sub-chain, which is what makes the sharing happen at all.

**Rank: 3.** It fires only where two chains share a suffix — which is *exactly*
the growth case the colony is in (a new declaration reached from two existing
rooms), and not at all on a single-colony scenario. Worth it as a companion to
5.1, which it composes with; not worth it alone.

### 5.4 Memoise `seams`

**What.** `Atlas.seams` calls `Seam.bandBy` directly (`Atlas.fs:1543`); the memo
comment further down belongs to `routes`. Measured: **22 calls a tick over 7
distinct ordered pairs** on `pair`, 13 over 4 on `reactor`.

**Measured ceiling.** `seams` subtree 1.75%, `Seam.bandBy` 1.71%. A memo removes
~68% of the calls, so ~**1.2% of the tick** — about 0.1 ms, an order of magnitude
under the harness's spread. `Seam.pairsAcross`'s 5.9% is *not* mostly `bandBy`'s:
`bandBy`'s whole subtree is 1.71%, so at most that much of it is reached through
`seams`, and the remaining ≥4.2% sits under `Seam.joinedBy` (subtree 8.23%) —
which is `World.linked`'s route search, §5.2's line item and not this one.

**Semantics.** None — `seams` is already a pure function of the tick's ring and
ground grids, and the Atlas is per-tick.

**Rank: 4.** Cheap, safe, small. Ship it beside something else, not on its own.

### 5.5 Not worth doing, measured

| Candidate | Measurement | Verdict |
|---|---|---|
| Bounded / early-stopping far field | pops 39,646 → 35,408 (−10.7%), `dry=0`, `unreachedReads=0`; every far-leg read becomes a `settleTo` entry (1,541 a tick) | The band is a room edge; settling to it costs 90% of the room. **No.** |
| Flat `int[]` heap with no `option` | The heap is *already* a `ResizeArray<int>` compiled to a JS array, indexed through `[<Emit>]` accessors; `Option.get` never touches a heap slot (§2.1) | Nothing to remove. **Already done.** |
| `indexOf`-free heap | Keys are `dist * 2500 + index` integers, so the heap never calls `indexOf`; `indexOf` itself is a `Pos → index` multiply-add on the flood's boundary reads and measures **0.00% self** in this profile (Fable inlines it, and V8 finishes the job) | Not in the loop. **No.** |
| Bucket queue instead of the binary heap | order-preserving +23% slower; order-changing −31% but `Parents` diverges | **No**, unless the bot's equal-cost step direction is up for grabs. |
| Interior fast path in the relaxation loop | flood frames 25.12% → 25.32%; `decide` medians 6.07 → 6.00 | **No effect.** |
| Reusing one scratch grid across floods | `Parents` alloc+fill benchmarked at 0.032 ms/tick | **0.4% of a tick.** No. |
| Re-keying `farFieldAlong` off the Task | Confirmed on this tree: all 8 misses are `Reserve`, three distinct controller ids, four distinct chains — the Task is not the multiplier, the chain and the pricing are | **No** (agrees with #353 option 2). |
| Pruning the matcher by best Priority group | Not re-measured; #353's reading stands, and this run corroborates the premise: only 12 cross-room prices a tick exist to skip | **No.** |

---

## 6. The shape of the problem, in one paragraph

CPU is now spent in three roughly equal thirds: a quarter of the tick floods
rooms, a third pays F# collections in a JS runtime, and a third is the bot's own
logic. The flood third is **entirely** the cross-room price of reserving rooms
the colony has declared but does not yet live in, it is the same eighteen floods
every tick, and it grows one chain per declaration — which is precisely why
`outpost-wave-2.md` found CPU binding on growth. It cannot be made cheaper per
pop (§3.4, §3.5, §3.6) and it cannot be made narrower (§3.3); it can only be made
**rarer**, which is §5.1 and §5.3. The collections third has no single call site
(§2.1) and can only be reduced by doing less work, not different work. The best
un-taken cut outside `decide` is the world's routability check (§5.2), which
re-derives the same room graph four times a tick from terrain that never changes.

A fair expectation if 5.1 (safe half), 5.2 and 5.3 all land: roughly −15% of
`decide` and −5% of the rest, on the harness's own clock. Against the live mean
of 57.98 ms with `decide` at 36.61 ms that is on the order of 5–7 ms a tick —
real headroom for one more colony, not for three. The full 5.1 (both pricings)
roughly doubles the `decide` half.

## 7. What was not measured

- **Live shapes.** Everything here is the `pair --level 7` and `reactor --level 6`
  harness worlds, which are frozen: creeps do not move, so the `TravelCost`
  occupancy input to §5.1 never changes and the ceiling C1 measures is the
  friendliest case for it. C1b, which caches only traffic-blind pricings, has no
  such dependence and is the number to trust.
- **`--census-every`.** No run perturbed the census, so the cost of *rebuilding*
  a cross-tick far-field cache when the signature moves is unmeasured. The
  existing `Walks` table has the same exposure and ADR 0032 accepted it.
- **The fourth colony.** W11S29 appears here only as a reserve target reached
  through W12S29; the tick where it becomes a fourth `decide` call was not
  modelled. Every finding in §3 says that tick adds another chain's worth of
  floods, not another colony's worth.
- **`joinedAcross` at fleet scale.** §4's "only twelve band scans a tick" is this
  fleet's number; the per-creep half of it grows with creeps and should be
  re-counted on the live shape before it is dismissed.
