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
(`scripts/profile.mjs`) under the V8 sampling profiler and prints ms/tick
plus two hotspot tables (by self and by inclusive time). The raw
`build/fabot.cpuprofile` opens in Chrome DevTools or speedscope.

The stub keeps the #50 baseline shape — W12S28 at RCL3, 8 creeps — so
ms/tick stays comparable across commits even as the live colony moves on.
It implements only the API surface declared in `src/App/Bindings.fs`, the
terrain is synthetic, the world is frozen between ticks (so census-keyed
memos are measured at zero), and engine-side costs are not simulated:
relative percentages are the signal, absolute ms/tick is not.

Current numbers, the live CPU history, and the per-hotspot attribution are
tracked in #50 — read that, not this file, for where the time goes.

## Layout

- `src/Core` — pure decision layer (no JS/Fable deps). Single seam:
  `decide : Snapshot -> Assignments -> Intent list * Assignments`.
- `src/App` — Fable entry point: bindings, Snapshot construction, Executor
  (the only code calling the game API).
- `tests/Core.Tests` — Expecto tests driving the `decide` seam.

The bundle is a single CommonJS `main` module exporting `loop`, uploaded via
`POST <SCREEPS_API_URL>/api/user/code`. Server contract details and sources:
`docs/research/fable-screeps.md`.
