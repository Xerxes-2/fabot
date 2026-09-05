# fabot

Screeps seasonal-server bot in F#, compiled to JS via [Fable](https://fable.io).

## Prerequisites

With Nix (recommended — same toolchain as CI, pinned by `flake.lock`):

- `nix develop` drops you into a shell with .NET SDK 10 and Node.js 24;
  prefix any command with `nix develop -c` to run it without entering the
  shell. (A committed `.envrc` enables `direnv` auto-activation if you have
  direnv + nix-direnv installed.)

Without Nix, install manually:

- .NET SDK 10, Node.js >= 24

Then in either case:

- `dotnet tool restore` (installs Fable)
- `npm install` (installs esbuild)
- `cp .env.example .env` and fill in your Screeps auth token
  (create one at https://screeps.com/a/#!/account/auth-tokens)

## Workflow

```sh
dotnet test      # Expecto tests against the pure decision layer (Core)
npm run deploy   # build (Fable → esbuild → dist/main.js) + upload to the server
```

Or separately: `npm run build` / `npm run upload`.

## Profiling

```sh
npm run build
npm run profile            # 100 ticks at RCL5; or: npm run profile -- 500 [top-N]
npm run profile -- 100 30 --census-every 10   # move the census every 10 ticks
npm run profile -- --scenario outpost         # the colony and its neighbours
npm run profile -- --level 4                  # build both scenarios at another RCL
```

`npm run profile` drives the compiled `loop()` in Node against a stub colony
(`scripts/profile.mjs`) under the V8 sampling profiler and prints ms/tick
plus two hotspot tables (by self and by inclusive time). The raw
`build/fabot.cpuprofile` opens in Chrome DevTools or speedscope.

Every run also prints ADR 0041's condition to revisit the layered
projection — **a mean tick above 50 ms, or any single tick above 80** —
judged against the run's own ticks, tripped or not. The thresholds live in
`scripts/cpu-trigger.mjs`, shared with `npm run observe cpu`, which reads
the same judgement off the per-tick line the bot writes to
`Memory.fabot.observe.cpu`. They are a trigger to re-decide, never a budget
the bot acts on: ADR 0041 decided CPU is measured, not budgeted. Only one of
the two readings decides it: `observe cpu` reads the deployed bundle's own
`Game.cpu.getUsed()`, while the profile's verdict is read off this harness's
clock, which is a floor for the same reason its ms/tick is (below) — a run
that prints "not triggered" has failed to trip the trigger, and has not
cleared it.

Two scenarios:

- **`stub`** (the default) is one synthetic room (`W1N1`) shaped like the
  live colony. A room it does not model, such as a declared outpost,
  answers as solid rock: a neighbour with no exits, which is the fiction
  its walled border ring already tells. That fiction has one price, and
  the report names it: the bundle still casts ADR 0042's two reservers —
  the row's quota is one per *declared* outpost and does not wait on
  vision — and this world has nowhere for them to go, so they stand beside
  the spawn with a Reserve target across a border that has no exits. A
  reserver here is a creep that cannot reach its work; the `outpost`
  scenario is where that walk and that hold are measured.
- **`outpost`** builds the colony's own room and its two declared
  neighbours from the committed room captures (`W12S28`, `W12S27`,
  `W13S28`), on real terrain rather than synthetic (ADR 0036), and stands
  in each neighbour what the live server holds there: the source
  containers ADR 0042 makes the switch into the economy (`15,44` in
  W12S27, `18,3` and `15,8` in W13S28), a reservation of the colony's own
  on the controller, and vision. Those containers make the outposts'
  sources Posts, so two of the bundle's own rows now work a room from the
  spawn that cast them and the harness stations them there: an Anchor on
  each outpost container (one per Post, ADR 0042) and a reserver beside
  each outpost's controller, one per declared outpost in
  `Outpost.declared`'s order. On top of them one crew the bundle does not
  hire — a hauler per outpost container, standing the far end of a round
  trip whose near end is the hired hauler row at the home spawn. It is the
  world ADR 0041's layered
  projection is sized against, and with the constant filled (ADR 0042) the
  scan set really is all three rooms: the run prints the terrain read per
  room, and says so out loud if a room of the world ever falls outside the
  scan (a declaration removed, or ADR 0043's stand-down shutting one).

Both are built at a **controller level** — `--level N`, RCL5 by default,
which is where the live colony stands — and the first line of every report
says which one it ran at. Everything the level implies is derived from it
rather than written down: the extension, tower and Storage counts come off
the engine's `CONTROLLER_STRUCTURES` table (RCL5: 30 extensions, of which 3
stay construction sites so the Build family is measured, 2 towers, 1
Storage), the energy bank off the extensions that stand, and the **fleet
off the bundle itself** — the harness runs `loop()` with no creep standing
anywhere and honours every `SpawnCreep` intent until the bot stops asking
(the first is ADR 0006's disaster-fallback worker), so the creep
count is the Workforce target at that bank (ADR 0012) and not a number in
the script. Moving the colony a level is one flag, and no count in
`scripts/profile.mjs` has to be re-checked by hand.

Two things about that furnishing are worth having in front of you when you
read a number against #144's own table. The three construction sites are
held back **out of** the level's allowance, not added on top of it — the
engine counts a site against `CONTROLLER_STRUCTURES` too, so 30 built plus
3 pending is a room RCL5 cannot hold — which means an RCL5 run stands 27
extensions and `energyCapacityAvailable` reports **1650**, not the 1800 the
ticket's table names for a finished level. Every body ADR 0006 casts is
sized against that 1650, a deliberate 8% under the level's ceiling, and the
price of keeping the Build family in the measurement. And the cluster steps
over the room's **working ground** — every source's Seats and the
controller's Upgrade Work Area — because ADR 0022 keeps the Layout off it,
so the harness furnishes the room the colony would actually build rather
than one with an extension on the tile an Anchor stands on.

Both implement only the API surface declared in `src/App/Bindings.fs`, the
world is frozen between ticks, and engine-side costs are not simulated:
relative percentages are the signal, absolute ms/tick is not.

`--census-every N` lifts the freeze on the one axis that hides work: every
Nth tick it paves one tile of the scenario's spare lane — the ground no
trunk paves — (and, once the lane is paved, lifts those tiles again one at
a time), so the census
signature moves and the census-keyed memos — the Layout and the hauler
quota (ADR 0017), the spawn walks (ADR 0032) — are made to recompute.
Without it those paths recompute once, in a warm-up tick, and are measured
at zero forever after.

Read a perturbed run by class, never by a pooled mean: ms/tick, the hotspot
tables, and the `census-keyed frames` table all split into the perturbed
ticks that pay the recompute and the quiet ticks that recall it. Three
things about that split are worth knowing:

- The perturbed column is a per-recompute price, not a per-tick one —
  divide it by N for what the memos cost a colony whose census moves that
  often.
- Its rows are inclusive and they nest (`trunkPath` runs inside
  `planLayout`), so read them one at a time; the column is not a sum.
- The census-keyed frames are printed in each inclusive table even when
  they rank below the top-N cut, under a `below the cut` line — they are
  easily outranked by the F# runtime plumbing they call through.

Samples the profiler parks at the root — the garbage collector, and its own
start and stop — belong to no tick, so they are in neither class and each
table's percentages are on its own class's base.

**The #50 baseline is superseded (#144), and its successor again (#163).**
Runs before #144 measured both scenarios at RCL3 with a hand-written 8- and
13-creep fleet, while the colony had reached RCL5 — so ADR 0041's trigger
was being judged two levels under the room it is about. `--level 3` does
*not* bring that baseline back: the old world under-furnished its own level
(no extensions and no tower where RCL3 allows ten and one) and hard-coded
its fleet, and both are now derived, so an RCL3 run today is an RCL3 colony
rather than the old fixture. #163 then moved both scenarios again, for the
same class of reason: `Outpost.declared` had been filled, so the bundle was
casting a row (ADR 0042's reserver) the harness stationed nowhere and every
run threw, and once the `outpost` scenario stood its neighbours' containers
and reservations it profiles three *worked* rooms rather than three visible
ones. The current anchor, on this hardware, 100 ticks: unflagged `stub`
means **6.6 ms/tick** (was 4.0), `--scenario outpost` **10.7** (was 2.3),
and a perturbed tick under `--census-every 10` **15.5** on `stub` and
**21.7** on `outpost`. Only runs at the same `--scenario` and `--level` are
comparable with each other.

The two scenarios moved for different reasons, and the split is worth
keeping straight. `stub` furnishes and derives exactly as it did before
#163 — the same room, one creep more — so its 4.0 → 6.6 is not this
harness's world changing but the bundle's work growing: the two declared
outposts entered the scan set (#126) and are projected as solid rock every
tick, and the reserver row (#131) hires two bodies that stand beside the
spawn with nowhere to walk. #163 only gave that row a seat, and the ms it
was already costing became measurable rather than fatal.

The `outpost` jump from 2.3 to 10.7 is that scenario finally measuring what
it was built for, not a regression: before #163 its two neighbours held a
controller and their sources and nothing else, so every quota priced them
at nothing — an unposted outpost source counts zero (ADR 0042) — and the
Task pool, the Atlas floods and the Matcher all did one room's work in a
three-room world. With the containers standing, the sources are Posts, the
reservation doubles them, and the fleet the bundle hires against that
income is twenty over three rooms against thirteen in the one-room `stub`.

Worth knowing before reading a level change: **a higher level is a cheaper
tick, not a dearer one**, and both halves of that are worth having straight
before a number surprises you.

- Raising the level does not enlarge the Layout. The plan is computed to
  ADR 0039's horizon whatever the current level is, so `planLayout` on a
  perturbed tick does not grow with the level — it costs 6.9 ms at RCL5
  against 7.6 at RCL3, if anything shrinking, because the further the room
  is built out the fewer placements are left to make.
- A derived fleet gets **smaller** as the level climbs: a bigger bank buys
  bigger bodies, and a Workforce target is an arithmetic of quotas and
  income (ADR 0012), so the count falls. `stub` hires 15 creeps at RCL3, 13
  at RCL5 and 12 at RCL8; the `outpost` world stands 38 creeps at RCL3, 23
  at RCL5 and 18 at RCL8 — a three-room income buying three-room bodies,
  and the swing across the levels is far wider than one room's. Measured
  means follow it: `stub` is 7.0 ms at RCL3 against 6.6 at RCL5, `outpost`
  15.5 against 10.7.

So the 2.4 → 4.0 step on `stub` was **furnishing the room and deriving the
fleet at all**, not the two extra levels — an RCL3 run on today's harness
is dearer than the RCL5 default, not cheaper. One more number to keep
beside the `outpost` run: ADR 0041 sizes the layered projection against
*roughly sixteen creeps over three rooms*, and at the RCL5 default the
scenario now stands 23 — twenty hired by the bundle's own arithmetic
against a three-room income (fifteen of them standing at home, which is
what the live colony held at t140,810, and five stationed in the outposts),
plus a three-hauler crew the harness stands on the outpost containers. The
crew is a floor and not a quota — ADR 0042 sizes two haulers per container
for an unpaved outpost, and the hired hauler row already prices the same
round trips — so it is stood *after* the fleet is hired, and what the
bundle would have hired instead of it is ADR 0012's arithmetic and not this
harness's to guess.

Current numbers, the live CPU history, and the per-hotspot attribution are
tracked in #50 — read that, not this file, for where the time goes.

## Room fixtures

```sh
npm run capture-room -- W12S28   # writes tests/Core.Tests/rooms/W12S28.room
```

Captures one room's fixed shape — terrain plus its sources, controller and
mineral, no structures — as reviewable text for the Layout's whole-room
invariant suite (ADR 0036). The API is an authoring tool, never a test
dependency: the suite loads the committed file and calls nothing. Terrain
never changes, so a re-capture of unchanged terrain diffs on nothing but
the header's `tick`, which is there to say when the furniture was last
read. Pass `--force` to overwrite an existing fixture deliberately.

## Layout

- `src/Core` — pure decision layer (no JS/Fable deps). Single seam:
  `decide : Snapshot -> Assignments -> Intent list * Assignments`.
- `src/App` — Fable entry point: bindings, Snapshot construction, Executor
  (the only code calling the game API).
- `tests/Core.Tests` — Expecto tests driving the `decide` seam; `rooms/`
  holds the committed room captures the Layout's invariant sweep runs on.

The bundle is a single CommonJS `main` module exporting `loop`, uploaded via
`POST <SCREEPS_API_URL>/api/user/code`. Server contract details and sources:
`docs/research/fable-screeps.md`.
