# fabot

Screeps seasonal-server bot in F#, compiled to JS via [Fable](https://fable.io).

## Prerequisites

With Nix (recommended — same toolchain as CI, pinned by `flake.lock`):

- `nix develop` drops you into a shell with .NET SDK 10 and Node.js 22;
  prefix any command with `nix develop -c` to run it without entering the
  shell. (A committed `.envrc` enables `direnv` auto-activation if you have
  direnv + nix-direnv installed.)

Without Nix, install manually:

- .NET SDK 10, Node.js >= 22

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
```

`npm run profile` drives the compiled `loop()` in Node against a stub colony
under the V8 sampling profiler and prints ms/tick plus two hotspot tables
(by self time and by inclusive time). The raw `build/fabot.cpuprofile` opens
in Chrome DevTools or speedscope.

The stub (in `scripts/profile.mjs`) mirrors the live colony's shape at the
#50 baseline — W12S28 at RCL3: 2 sources, one spawn, a controller, ~25
trunk-road tiles (spawn → source container, spawn → controller container,
a couple below half hits so Repair pools tasks), a source container and a
controller container, 3 extension sites, and 8 creeps (2 Anchors, 3 hauler
units, 3 worker units). It implements only the API surface declared in
`src/App/Bindings.fs` and never touches the Screeps network API.

The instrument recognizes the known hotspots from #50 — a run on the
machine that recorded that baseline shows ~15 ms/tick with `floodFrom` at
~11% self / ~39% inclusive, `planLayout` ~32% inclusive (`trunkPath` ~25%),
`resolve` ~20%, `matchCreeps` ~15%, and the Fable structural-comparison
family (`compare` / `recordCompareTo` / `sameConstructor` / `item`) around
a third of self time.

Limits to keep in mind when reading the numbers:

- The terrain is synthetic (deterministic walls/swamp over a plain room),
  not W12S28's map; flood and path costs are the right order, not exact.
- The world is frozen: every intent is accepted (`OK`) but nothing mutates
  between ticks except `Game.time` and whatever the bot writes to `Memory`.
  Costs that only appear when state changes tick-to-tick are underweighted.
- No hostiles, towers, or dropped energy piles: the fire, safe-mode, and
  pickup paths are measured at their quiet-colony (near-zero) cost.
- Engine-side costs (intent execution, `Memory` JSON serialization) are not
  simulated.
- Absolute ms/tick is machine-dependent (the official server is several
  times slower); the relative percentages are the signal.

## Layout

- `src/Core` — pure decision layer (no JS/Fable deps). Single seam:
  `decide : Snapshot -> Assignments -> Intent list * Assignments`.
- `src/App` — Fable entry point: bindings, Snapshot construction, Executor
  (the only code calling the game API).
- `tests/Core.Tests` — Expecto tests driving the `decide` seam.

The bundle is a single CommonJS `main` module exporting `loop`, uploaded via
`POST <SCREEPS_API_URL>/api/user/code`. Server contract details and sources:
`docs/research/fable-screeps.md`.
