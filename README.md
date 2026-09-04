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
npm run profile            # 100 ticks; or: npm run profile -- 500 [top-N]
npm run profile -- 100 30 --census-every 10   # move the census every 10 ticks
npm run profile -- --scenario outpost         # the colony and its neighbours
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

- **`stub`** (the default) keeps the #50 baseline shape — one synthetic
  room (`W1N1`) shaped like the live colony at RCL3, 8 creeps — so ms/tick
  stays comparable across commits even as the live colony moves on. A room
  it does not model, such as a declared outpost, answers as solid rock: a
  neighbour with no exits, which is the fiction its walled border ring
  already tells.
- **`outpost`** builds the colony's own room and its two declared
  neighbours from the committed room captures (`W12S28`, `W12S27`,
  `W13S28`), on real terrain rather than synthetic (ADR 0036), with 13
  creeps across the three. It is the world ADR 0041's layered projection is
  sized against, and the run says how much of it the bundle actually
  projected: while `Outpost.declared` stands empty the scan set is the
  spawn room alone, so today its ms are one room's and the harness is
  waiting on the constant (ADR 0042).

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
table's percentages are on its own class's base. Only unflagged `stub` runs
are comparable with the #50 baseline.

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
