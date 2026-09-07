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
npm run profile                        # 100 ticks, stub colony at RCL5
npm run profile -- --scenario pair     # the live shape: an RCL5 mother and her child
npm run profile -- 100 30 --census-every 10
```

Drives the compiled `loop()` in Node under the V8 sampling profiler and
prints ms/tick, two hotspot tables and ADR 0041's revisit trigger. The
scenarios, the flags, what the number does and does not mean, and how to
read a perturbed run are in [`docs/profiling.md`](docs/profiling.md);
current numbers live on #50.

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
  `decide : ColonyView -> Assignments -> Intent list * Assignments`; the
  view itself is cut from the tick's `World` by `ColonyView.ofWorld`, which
  is pure and tested (ADR 0052).
- `src/App` — Fable entry point: bindings, `World.ofGame` (the one reader
  of the game's objects), Executor (the only code calling the game API).
- `tests/Core.Tests` — Expecto tests driving the `decide` seam; `rooms/`
  holds the committed room captures the Layout's invariant sweep runs on.

The bundle is a single CommonJS `main` module exporting `loop`, uploaded via
`POST <SCREEPS_API_URL>/api/user/code`. Server contract details and sources:
`docs/research/fable-screeps.md`.
